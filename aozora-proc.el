;; aozora-proc.el --- 青空文庫プロセッサ

;; Author: Taichi Kawabata <kawabata.taichi@gmail.com>
;; Keywords: 青空文庫, LaTeX, HTML5

(defvar aozora-proc-ver "<ver.:2011-02-07>")

;; 青空文庫・文法チェック＆HTML5・LaTeX変換

;; TODO・底本注記の拡充
;;       バグの報告（小さい「わ」等）

;; References:
;; ・青空文庫・組版案内
;;   http://kumihan.aozora.gr.jp/

(require 'peg)

;; Commentary
(defvar aozora-proc-doc (concat "
 aozora-proc.el ... 青空文庫プロセッサ " aozora-proc-ver "
   コマンドラインからの使い方：

   % emacs --script aozora-proc.el -f <format> file1.txt file2.txt ...
 
     format: `html-h', `tex-h' (横書き), `html-v', `tex-v' (縦書き)

   例：ファイルを青空文庫テキストとみなし、文法チェックを行う。
       % emacs --script aozora-proc.el sample.txt
   例：ワイルドカードにマッチする青空文庫テキストを、HTMLへ変換する。
       % emacs --script aozora-proc.el -f html-v *.txt
       （拡張子が `.html' に置き換わったファイルが生成されます。）
   他のオプション：
         -h              : このヘルプファイルの表示
         -g _gaiji_file_ : 青空文庫外字データファイルの位置
         -l _file_name_  : 設定用 elisp ファイルの場所
         -s 'regexp'     : 処理開始位置を指定する正規表現
         -e 'regexp'     : 処理終了位置を指定する正規表現

   ※ Emacs 23.2 以降が必要です。（ucs-normalize.el を使うため）
   ※ 事前に peg.el をインストールする必要があります。
      （emacswiki.org から取得）
   ※ 必要に応じて青空文庫外字データファイルを用意します。
      （kanji-database.sourcefroge.netから取得）
"))

;;; User Settings
;; 外字データ.  漢字データベースのCVSサーバからダウンロードしておく。
(defvar aozora-proc-gaiji-file 
  "~/Dropbox/cvs/kanji-database/data/aozora_gaiji_chuki.txt")
(defvar aozora-proc-start-regexp
  "-------------------------------------------------------\n\n")
(defvar aozora-proc-end-regexp "\n\n\n")
(defvar aozora-proc-method 'validate
  "ファイル出力のデフォルト処理形式")

;;; Internal variables
(defvar aozora-proc-debug t)
(defvar aozora-proc-stack nil)
(make-variable-buffer-local 'aozora-proc-stack)
(defvar aozora-proc-gaiji-table nil)
(defvar aozora-proc-script nil)

;; Initialization
(defun aozora-proc-initialize ()
  (let ((table (make-hash-table :test 'equal)))
    (when (file-exists-p aozora-proc-gaiji-file)
      (with-temp-buffer
        (insert-file-contents aozora-proc-gaiji-file)
        (goto-char (point-min))
        (while (re-search-forward "	\\([^\t][^\t]?\\)	※［＃\\(.+?\\)、ページ数-行数］" nil t)
          (puthash (match-string 2) (match-string 1) table))))
    (setq aozora-proc-gaiji-table table)))


;; 青空文庫の文法

(eval-and-compile
(defvar aozora-proc-parse-chars
  '(
    (文字列     (+ 文字))
    (文字       (or (and (not "\n") (not "［＃") (not "※［＃") 
                         ;; 〔a.. は合成表記の欧文として扱う。
                         (not (and "〔" 欧文字))
                         (not "《") (not "》") (not "｜") (any))
                    漢字外字 非漢字外字 漢文))
    (漢字       (or (and [㐀-鿋 豈-龎 𠀀-𯿽] (opt [󠄀-󠇯]))
                    ["仝〆○々ヶ"] 漢字外字))
    (かな       (or [ぁ-ん ァ-ヶ ゛-ゞ ・-ヾ] "／″＼" "／＼"))
    ;;          
    (漢字外字   (region (or "※［＃二の字点、1-2-22］"
                            (and "※［＃「" 注記文字列 "］")
                            (and "※［＃二の字点、" 注記文字列 "］")
                            (and "※［＃濁点付き二の字点、" 注記文字列 "］")))
                (action (aozora-proc-外字)))
    ;;(注記文字列     (+ (and (not "］")  文字) )) ;; 後で削除すること。
    ;;
    (非漢字外字 (region (and "※［＃" (not "「") 注記文字列 "］"))
                (action (aozora-proc-外字)))
    ;;
    (漢文       (region (or (and (opt 訓点送り) 返り点)
                            訓点送り))
                (action (aozora-proc-漢文)))
    (訓点送り   (and "［＃（" (+ (or 漢字 かな )) "）］"))
    (返り点     (and "［＃" (or (and 返り順序点 (opt 返りレ点))
                                返りレ点) "］"))
    (返り順序点 ["一二三四上中下天地人甲乙丙丁"])
    (返りレ点   "レ")
    ;;(竪点       "—") ;; TODO これはバックトラック処理するのが良い。
    (欧文字     [a-z A-Z α-ρ σ-ω Α-Ρ Σ-Ω А-я " Ёё"]))
  "青空文庫の文字の記法")

(defvar aozora-proc-parse-sidenotes
;;(defvar aozora-proc-parse-char-notes
  `(
    ;; 一般文脈に現れる文字列
    (一般文字列     (+ (or (and (or 文字列 欧文) (* 引用注記) (opt 一般ルビ) (* 引用注記))
                           (and 指定ルビ (* 引用注記)))))
    ;; 注記の［＃…］で現れる文字列
    (注記文字列     (+ (and (not "］")  文字) (* 引用注記)))
    ;; 注記の「…」内で現れる文字列
    (引用文字列     (+ (or (and (or (+ 引用文字) 欧文) (* 引用注記) (opt 一般ルビ) (* 引用注記))
                           (and 指定ルビ (* 引用注記)))))
    (引用文字       (+ (and (not "」は") (not "」の") (not "」に") (not "」］") 文字)))
    ;; ［＃「…」は…　］というタイプの注記の並び
    (引用注記       (or 修飾注記 原文注記 入力者注記))
    (ルビ注記       (or ルビ修飾注記 ルビ原文注記 ルビ入力者注記))
    ;;
    (修飾注記       (region (and "［＃「" (region 引用文字列) "」"  修飾指定 "］"))
                    (action (aozora-proc-修飾注記)))
    (ルビ修飾注記   (region (and "［＃ルビの「" (region 引用文字列) "」"  修飾指定 "］"))
                    (action (aozora-proc-ルビ修飾注記)))
    (原文注記       (region (and "［＃「" (region 引用文字列) "」" 
                                 (region (opt "の左")) "に「" (region 引用文字列) "」の注記］"))
                    (action (aozora-proc-原文注記)))
    (ルビ原文注記   (region (and "［＃ルビの「" (region 引用文字列) "」" 
                                 (region (opt "の左")) "に「" (region 引用文字列) "」の注記］"))
                    (action (aozora-proc-ルビ原文注記)))
    (入力者注記     (region (and "［＃「" (region 引用文字列) "」は" (region 底本注記) "］"))
                    (action (aozora-proc-入力者注記)))
    (ルビ入力者注記 (region (and "［＃ルビの「" (region 引用文字列) "」は" (region 底本注記) "］"))
                    (action (aozora-proc-ルビ入力者注記)))
    (底本注記       (or (and "底本では「" 引用文字列 "」") "ママ" ))
    ;;
    (修飾指定   (or (and "に" (substring 強調))
                    (and "の" (substring 左強調))
                    (and "の" 左ルビ)
                    (and "は" (substring 字体))
                    (and "は" 文字サイズ)))
    (強調       (or (and (opt "二重") (or "傍線" "波線" "破線" "鎖線"))
                    "傍点" "白ゴマ傍点" "丸傍点" "白丸傍点" "×傍点" ;; 非公式
                    "黒三角傍点" "白三角傍点" "二重丸傍点" "蛇の目傍点"
                    "白四角傍点")) ;;非公式
    (左強調     (and "左に" 強調))
    (左ルビ     (and "左に「" (substring 引用文字列) "」のルビ")
                (action (push (cons "左ルビ" (pop peg-stack)) peg-stack)))
    (字体       (or 見出し 罫囲み 
                    "太字" "斜体" "分数" "上付き小文字" "下付き小文字"
                    "篆書体" "小書き" "行右小書き" "行左小書き" "横組み" "縦中横"
                    "合字" "ローマ数字"))
    (見出し     (and (opt (or "窓" "同行")) (or "大" "中" "小") "見出し"))
    (罫囲み     (and (opt "二重") "罫囲み")) ;; 「二重」は注記一覧になし。
    (文字サイズ (and 数 "段階" (substring (and (or "大きな" "小さな") "文字")))
                (action (push (cons (pop peg-stack) (pop aozora-proc-stack)) peg-stack)))
    (数         (region (or (+ [0-9]) (+ [０-９]) ["一二三四五六七八九十"]))
                (action (aozora-proc-数)))
    ;; ルビ
    (一般ルビ   (and 一般ルビ２ (* ルビ注記)))
    (指定ルビ   (and 指定ルビ２ (* ルビ注記)))
    (一般ルビ２ (region (and "《" 文字列 "》"))
                (action (aozora-proc-一般ルビ)))
    (指定ルビ２ (region (and  "｜" (or 文字列 欧文) (* 引用注記) (region "《" 文字列 "》")))
                (action (aozora-proc-指定ルビ)))
    ;; 
    (欧文       (region (and "〔" 欧文字 (+ (or 欧文字 [!-~] 引用注記)) "〕"))
                (action (aozora-proc-欧文)))
    )
  "青空文庫のルビ等に対する注記")

(defvar aozora-proc-parse-line
  '((行         (+ (or 一般注記 一般文字列))) ;; 一般注記→注記文字列の順番にチェックすること。
    (一般注記   (or 囲み注記 割り注 地上げ注記 図 底本入力者注記))
    ;; ［＃ここから○○○］→［＃ここで○○○終わり］は古い記法。
    (囲み注記   (region (and "［＃" (or (substring (or 強調 左強調 字体))
                                        文字サイズ) "］"
                             (region 一般文字列)
                             "［＃" (substring (or 強調 左強調 字体 文字サイズ終)) "終わり］"))
                (action (aozora-proc-囲み注記))) ;; 外部制約：開始と終了の内容は同一であること。
    (割り注     (region (and "［＃割り注］"
                             (region (+ (or 改行 一般文字列)))
                             "［＃割り注終わり］"))
                (action (aozora-proc-割り注)))
    (改行       (region "［＃改行］")
                (action (aozora-proc-改行)))
    (文字サイズ終 (or "大きな文字" "小さな文字"))
    (地上げ注記 (region (or 地寄り 地付き 地上げ))
                (action (aozora-proc-地上げ注記)))
    ;;
    (地寄り     (and "［＃下げて、地より" 数 "字あきで］"))
    (地上げ     (and "［＃地から" 数 "字上げ］")) ;; ［＃地よりn字上げ］は誤記。
    (地付き      "［＃地付き］" 
                 (action (push 0 aozora-proc-stack)))
    ;;
    (図         (region (and "［＃" (region 図注記) (or "(" "（") 
                             (region ファイル名) ".png" (opt 図大きさ)
                             (or ")" "）") "入る］"))
                (action (aozora-proc-図)))
    (図注記     (+ (and (not "(") (not "（") 文字)))
    (ファイル名 (+ (and (not ".png") 文字)))
    (図大きさ   (and "、横" 数 "×縦" 数))
    (底本入力者注記 (region (and "［＃底本では" 注記文字列 "］"))
                    (action (aozora-proc-底本入力者注記))))
  "青空文庫の脇書きも含めた行全体の記法")

(defvar aozora-proc-parse-block
  '(
    (ブロック   (and (or ページ指定 段落字下げ 段落指定 段落)))
    ;; 段落字下げ指定中のブロック
    (ブロック２ (or ページ指定 段落指定 段落))
    ;; 左右中央指定中のブロック
    (ブロック３ (or 段落字下げ 段落指定 段落))
    ;; ページ指定→段落指定→段落の順番にチェックすること。
    (段落       (and (region (opt 字下げ) (opt 行) "\n"))
                (action (aozora-proc-段落)))
    (字下げ     (region (and "［＃" 数 "字下げ］"))
                (action (aozora-proc-字下げ)))
    ;;
    (ページ指定 (or 左右中央 改まり注記))
    (左右中央   (and (region "［＃ページの左右中央］") "\n"
                     (region (* ブロック３))
                     (region "［＃改ページ］") "\n")
                (action (aozora-proc-左右中央)))
    (改まり注記 (and (region (or "［＃改丁］" "［＃改ページ］")) "\n")
                (action (aozora-proc-改まり注記)))
    ;;
    (段落指定   (and (or 段落字詰め 段落地付き 段落字上げ 
                         改段 段落字体 段落文字大 
                         段落文字小 段組み) "\n"))
    ;;
    (改段       (region "［＃改段］") (action (aozora-proc-改まり注記)))
    ;;
    (段落字下げ (and (+ 段落字下げ2) (region 字下げ終) "\n")
                (action (aozora-proc-字下げ注記)))
    (段落字下げ2 (region (and (or 改行天付き 天字下げ 文字下げ 文字下げ２)
                              "\n" (region (* ブロック２))))
                (action (aozora-proc-字下げ注記2)))
    (改行天付き (and "［＃ここから改行天付き、折り返して" 数 "字下げ］")
                (action (push "改行天付き" aozora-proc-stack)))
    (天字下げ   (and "［＃天から" 数 "字下げ］")
                (action (push "天字下げ" aozora-proc-stack)))
    (文字下げ   (and "［＃ここから" 数 "字下げ］")
                (action (push "文字下げ" aozora-proc-stack)))
    (文字下げ２ (and "［＃ここから" 数 "字下げ、折り返して" 数 "字下げ］")
                (action ;; ２引数を１引数に整理する。
                 (let ((num1 (pop aozora-proc-stack))
                       (num2 (pop aozora-proc-stack)))
                   (push (cons num1 num2) aozora-proc-stack)
                   (push "文字下げ折り返し" aozora-proc-stack))))
    (字下げ終   (and "［＃ここで字下げ終わり］"))
    ;;
    (段落字詰め (region (and "［＃ここから" 数 "字詰め］\n" 
                             (region (* ブロック)))
                             "［＃ここで字詰め終わり］")
                (action (aozora-proc-段落字詰め)))
    (段落地付き (region (and "［＃ここから地付き］\n" (region (* ブロック))
                             "［＃ここで地付き終わり］"))
                (action (aozora-proc-段落地付き)))
    (段落字上げ (region (and "［＃ここから地から" 数 "字上げ］\n" 
                             (region (* ブロック)) "［＃ここで字上げ終わり］"))
                (action (aozora-proc-段落字上げ)))
    (段落字体   (region (and "［＃ここから" (substring 字体) "］\n" 
                             (region (* ブロック))
                             "［＃ここで" (substring 字体) "終わり］"))
                (action (aozora-proc-段落字体)))
    (段落文字大 (region (and "［＃ここから" 数 "段階大きな文字］\n" 
                             (region (* ブロック)) "［＃ここで大きな文字終わり］"))
                (action (aozora-proc-段落文字大)))
    (段落文字小 (region (and "［＃ここから" 数 "段階小さな文字］\n" 
                             (region (* ブロック)) "［＃ここで小さな文字終わり］"))
                (action (aozora-proc-段落文字小)))
    (段組み     (region (and "［＃ここから" 数 "段組み" 
                             (substring (opt "、段間に罫")) "］\n"
                             (region (* ブロック)) "［＃ここで段組み終わり］"))
                (action (aozora-proc-段組み)))
    )
  "青空文庫の複数段落に対する注記")
) ;; eval-and-compile

(defvar aozora-proc-parse-line-peg
  (peg-translate-rules 
   (append aozora-proc-parse-line
           aozora-proc-parse-sidenotes
           aozora-proc-parse-chars)))

(defvar aozora-proc-parse-block-peg
  (peg-translate-rules 
   (append aozora-proc-parse-block
           aozora-proc-parse-line
           aozora-proc-parse-sidenotes
           aozora-proc-parse-chars)))

;;; Parser 

(put 'aozora-proc-save-restriction lisp-indent-function 1)
(defmacro aozora-proc-save-restriction (&rest body)
 `(save-restriction
    (save-excursion
      (goto-char (point-min))
      (let* ((start (re-search-forward aozora-proc-start-regexp nil t))
             (end (progn (goto-char (point-max))
                         (re-search-backward aozora-proc-end-regexp nil t))))
        (narrow-to-region (or start (point-min)) (or end (point-max)))))
    ,@body))

(defun aozora-proc-parse-line ()
  "青空文庫のルビ・修飾を含む１行パーザ。ブロック注記は解釈しない。"
  (interactive)
  (eval aozora-proc-parse-line-peg))

(defun aozora-proc-parse-lines ()
  (interactive)
  (aozora-proc-remove-properties (point) (point-max))
  (while (and (re-search-forward "^" nil t) (not (eobp)))
    (eval aozora-proc-parse-line-peg)))

(defun aozora-proc-parse-block ()
  (interactive)
  (eval aozora-proc-parse-block-peg))

(defun aozora-proc-parse ()
  "現在のカーソルからバッファの最後までを青空文庫注記記法でパーズする。"
  (interactive)
  (aozora-proc-remove-properties (point) (point-max))
  (condition-case err
      (while (and (re-search-forward "^" nil t) (not (eobp)))
        (when (= (% (line-number-at-pos) 10) 0)
          (message 
           "validating %03d lines... %s" (line-number-at-pos)
           (if aozora-proc-script "\x1b[1A" "")))
        (eval aozora-proc-parse-block-peg))
    (error (message "%s at line: %d, column: %d
%s
%s^" err (line-number-at-pos) (current-column)
     (buffer-substring (point-at-bol) (point-at-eol))
     (make-string (current-column) ? ) nil))))

;;;
;;; Data Construction
;;; 

(defun aozora-proc-add-text-property (from to symbol val)
  "FROMからTOの間のaozora-proc-SYMBOLのプロパティ値をVALにする。"
  (let ((buffer-modified (buffer-modified-p)))
    (add-text-properties 
     from to
     (list (intern (concat "aozora-proc-" 
                           (if (stringp symbol) 
                               symbol (symbol-name symbol)))) val))
    (set-buffer-modified-p buffer-modified)))

(defun aozora-proc-remove-properties (from to)
  (interactive "r")
  (set-text-properties from to nil))

(put 'aozora-proc-let lisp-indent-function 1)
(defmacro aozora-proc-let (bindings &rest body)
  "BINDINGSをpeg-stackから順番にpop,bindし、BODYを実行する。"
  `(let
       ,(mapcar (lambda (x)
                  (list x '(pop peg-stack)))
                (reverse bindings))
     ,@body))

(defun aozora-proc-外字 ()
  (aozora-proc-let (from to)
    (let* ((text (buffer-substring-no-properties from to))
           (key (progn
                  (string-match "^※［＃\\(.+?\\)、[^、]+］$" text)
                  (match-string 1 text)))
           str)
      (if (string-match "\\([12]\\)-\\([0-9][0-9]?\\)-\\([0-9][0-9]?\\)］$"
                        text)
          (setq str (char-to-string
                     (make-char 
                      (intern (concat "japanese-jisx0213-" (match-string 1 text)))
                      (+ 32 (string-to-number (match-string 2 text)))
                      (+ 32 (string-to-number (match-string 3 text))))))
        (setq str (gethash key aozora-proc-gaiji-table)))
      (if str (aozora-proc-add-text-property from (1+ from) 'text str)
        (message "外字が見付かりません … %s" key)
        (aozora-proc-add-text-property from (1+ from)
                                       '注釈
                                       (buffer-substring (+ from 3) (- to 1)))))
    (aozora-proc-add-text-property (1+ from) to 'ignorable t)))

(defun aozora-proc-数 ()
  "regionで指定された領域の数字をスタックに入れる。"
  (aozora-proc-let (from to)
    (let ((num (buffer-substring-no-properties from to)))
      (push (if (string-match num "一二三四五六七八九十")
                (1+ (match-beginning 0))
              (string-to-number (japanese-hankaku num)))
            aozora-proc-stack))))

(defun aozora-proc-漢文 ()
  (aozora-proc-let (from to)
    (goto-char from)
    (if (looking-at "\\(?:［＃（\\([^）\n]+?\\)）］\\)?\\(?:［＃\\([^（］]+?\\)］\\)?")
      (aozora-proc-add-text-property 
       from to '漢文 (cons (match-string 1) (match-string 2)))
      (error "漢文 not matching!"))
    (aozora-proc-add-text-property from to 'ignorable t)
  (goto-char to)))

(define-translation-table 'aozora-proc-accent-table
  (make-translation-table-from-alist
   '(([?! ?@] . ?¡) ([?? ?@] . ?¿) 
     ([?A ?E ?&] . ?Æ) ([?a ?e ?&] . ?æ)
     ([?O ?E ?&] . ?Œ) ([?o ?e ?&] . ?œ)
     ([?a ?&] . ?å) ([?A ?&] . ?Å) ([?S ?&] . ?ß)
     ([?O ?/] . ?Ø) ([?o ?/] . ?ø)
     (?` . ?\x300) (?' . ?\x301) (?^ . ?\x302) (?~ . ?\x303) 
     (?: . ?\x308) (?& . ?\x30a) (?, . ?\x327) (?_ . ?\x304)
     )))

(defun aozora-proc-欧文 ()
  (aozora-proc-let (from to)
    (let ((text (buffer-substring-no-properties (1+ from) (1- to))))
      (setq text
            (with-temp-buffer
              (insert text)
              (translate-region (point-min) (point-max) 
                                'aozora-proc-accent-table)
              (ucs-normalize-NFC-region (point-min) (point-max))
              (buffer-string)))
      (aozora-proc-add-text-property from to 'text text))))

(defun aozora-proc-search-backward-and-add-prop (pos str prop val)
  "POSから後方向にignorableでない場所でのSTRを検索して、その領域の
PROPをVALに設定する。"
  (save-excursion
    (goto-char pos)
    (if (string-match "[［《]" str) 
        ;; 検索対象は、ignorableも含む。
        (if (looking-back str)
            (aozora-proc-add-text-property (match-beginning 0)
                                           (match-end 0) prop val)
          (error "STR not found! - %s" str))
    (let (end (chars (nreverse (string-to-list str))))
      (while (get-text-property (point) 'aozora-proc-ignorable)
        (backward-char))
      (if (/= (char-after (point)) (car chars))
          (error "STR not found!! - %s" str)
        (setq end (1+ (point))) ;; プロパティの範囲はchar-beforeまでなので1+。
        (while (setq chars (cdr chars))
          (backward-char)
          (while (get-text-property (point) 'aozora-proc-ignorable)
            (backward-char))
          (if (/= (char-after (point)) (car chars))
              (error "STR not consistent! - %s" str)))
        (aozora-proc-add-text-property (point) end prop val))))))

(defun aozora-proc-修飾注記 ()
  (aozora-proc-let (from1 from2 to2 name to1)
    (let* ((target (buffer-substring-no-properties from2 to2))
           (bol-point (point-at-bol))
           val)
      (if (consp name) (setq val (cdr name) name (car name)) (setq val t))
      (aozora-proc-add-text-property from1 to1 'ignorable t)
      (aozora-proc-search-backward-and-add-prop from1 target name val))))

(defun aozora-proc-文字サイズ ()
  (aozora-proc-let (from to)
    (push (concat (buffer-substring-no-properties from2 to2) "文字")
          peg-stack)))

(defun aozora-proc-looking-back-kanji ()
  "漢字（および外字表現）列をlooking-backする。"
  (let ((pos (point)) new)
    (while (or (looking-back "[仝〆○々ヶ㐀-鿋豈-龎𠀀-𯿽][󠄀-󠇯]?" nil t)
               (looking-back "\\(\\(※［＃「\\|※［＃二の字点、\\|※［＃濁点付き二の字点、\\).+?］\\)" nil t))
      (goto-char (match-beginning 0)))
    (setq new (point))
    (goto-char pos)
    (if (and new (/= pos new)) (progn (set-match-data (list new pos)) t) nil)))

(defun aozora-proc-一般ルビ ()
  (aozora-proc-let (from to)
    (let ((ruby (buffer-substring (1+ from) (1- to))))
      (aozora-proc-add-text-property from to 'ignorable t)
      (save-excursion
        (goto-char from)
        ;; 修飾部分を飛ばす処理はあとまわし
        (if (or (aozora-proc-looking-back-kanji)
                (looking-back "[a-zA-Z0-9]+" nil t)
                (looking-back "〔?[a-z'A-Z0-9]+〕" nil t)
                (looking-back "\\s_+" nil t)
                (looking-back "[あ-ん]+" nil t)
                (looking-back "[ア-ン]+" nil t)
                )
            (aozora-proc-add-text-property (match-beginning 0)
                                           (match-end 0) 'ルビ ruby)
          (error "Not proper Ruby!"))))))

(defun aozora-proc-指定ルビ ()
  (aozora-proc-let (from1 from2 to2 to1)
    (let ((ruby (buffer-substring (1+ from2) (1- to2))))
      (aozora-proc-add-text-property from1 (1+ from1) 'ignorable t)
      (aozora-proc-add-text-property (1+ from1) from2 'ルビ ruby)
      (aozora-proc-add-text-property from2 to1 'ignorable t))))

(defun aozora-proc-原文注記 ()
  (aozora-proc-let (from1 from2 to2 from3 to3 from4 to4 to1)
    (let ((text (buffer-substring from1 to1)))
      (aozora-proc-add-text-property from1 to1 '注釈 text))))

(defun aozora-proc-入力者注記 ()
  (aozora-proc-let (from1 from2 to2 from3 to3 to1)
    (let ((text (buffer-substring from1 to1)))
      (aozora-proc-add-text-property from1 to1 '注釈 text))))

(defun aozora-proc-ルビ入力者注記 ()
  (aozora-proc-let (from1 from2 to2 from3 to3 to1)
    (let ((text (buffer-substring from1 to1)))
      (aozora-proc-add-text-property from1 to1 '注釈 text))))

(defun aozora-proc-底本入力者注記 ()
  (aozora-proc-let (from to)
    (aozora-proc-add-text-property from to '注釈 (pop aozora-proc-stack))
    (aozora-proc-add-text-property from to 'ignorable t)))

(defun aozora-proc-段落 ()
  (aozora-proc-let (from to)
    (aozora-proc-add-text-property from (1- to) '段落 t)))

(defun aozora-proc-字下げ ()
  (aozora-proc-let (from to)
    (aozora-proc-add-text-property from (point-at-eol) '文字下げ (pop aozora-proc-stack))
    (aozora-proc-add-text-property from to 'ignorable t)))

(defun aozora-proc-囲み注記 ()
  (aozora-proc-let (from1 str1 from2 to2 str2 to1)
    (let (val)
      (if (consp str1) (setq val (cdr str1) str1 (car str1)) (setq val t))
      (if (not (equal str1 str2)) (error "囲み注記 not matching! %s-%s" str1 str2))
      (aozora-proc-add-text-property from1 from2 'ignorable t)
      (aozora-proc-add-text-property from2 to2 str1 val)
      (aozora-proc-add-text-property to2 to1 'ignorable t))))

(defun aozora-proc-割り注 ()
  (aozora-proc-let (from1 from2 to2 to1)
    (aozora-proc-add-text-property from1 from2 'ignorable t)
    (aozora-proc-add-text-property from2 to2 '割り注 t)
    (aozora-proc-add-text-property to2 to1 'ignorable t)))

(defun aozora-proc-改行 ()
  (aozora-proc-let (from to)
    (aozora-proc-add-text-property from to '改行 t)
    (aozora-proc-add-text-property from to 'ignorable t)))

(defun aozora-proc-地上げ注記 ()
  (aozora-proc-let (from to)
    (aozora-proc-add-text-property from to 'ignorable t)
    (aozora-proc-add-text-property 
     from (point-at-eol) '地上げ (pop aozora-proc-stack))))

(defun aozora-proc-図 ()
  (aozora-proc-let (from1 from2 to2 from3 to3 to1)
    (aozora-proc-add-text-property 
     from1 to1 '図 
     (list (buffer-substring-no-properties from2 to2)
           (concat (buffer-substring-no-properties from3 to3) ".png")
           (pop aozora-proc-stack) (pop aozora-proc-stack)))
    ;; 縦横が省略された場合はnilが入る。
    (aozora-proc-add-text-property from1 to1 'ignorable t)))

(defun aozora-proc-左右中央 () 
  (aozora-proc-let (from1 from2 to2 from3 to3 to1)
    (aozora-proc-add-text-property from1 to1 '左右中央 t)
    (aozora-proc-add-text-property from1 to1 'ignorable t)))

(defun aozora-proc-改まり注記 ()
  (aozora-proc-let (from to)
    (aozora-proc-add-text-property 
     from to (buffer-substring (+ from 2) (1- to)) t)
    (aozora-proc-add-text-property from to 'ignorable t)))

(defun aozora-proc-字下げ注記 ()
  (aozora-proc-let (from to)
    (aozora-proc-add-text-property from to 'ignorable t)))
    
(defun aozora-proc-字下げ注記2 () 
  (aozora-proc-let (from1 from2 to2 to1)
    (let ((name (pop aozora-proc-stack)))
      (aozora-proc-add-text-property from1 from2 'ignorable t)
      (aozora-proc-add-text-property from2 to1 name (pop aozora-proc-stack)))))

(defmacro aozora-proc-surround (symbol)
  `(aozora-proc-let (from1 from2 to2 to1)
     (aozora-proc-add-text-property from1 from2 'ignorable t)
     (aozora-proc-add-text-property from2 to2 ',symbol
                                    (or (pop aozora-proc-stack) t))
     (aozora-proc-add-text-property to2 to1 'ignorable t)
     (setq aozora-proc-stack nil)))

(defun aozora-proc-段落字体   () 
  (aozora-proc-let (from1 str from2 to2 str2 to1)
    (if (not (equal str str2)) (error "Non-Matching Error!"))
    (aozora-proc-add-text-property from1 from2 'ignorable t)
    (aozora-proc-add-text-property from2 to2 (intern (concat "段落" str)) t)
    (aozora-proc-add-text-property to2 to1 'ignorable t)))

(defun aozora-proc-段落字詰め () (aozora-proc-surround 段落字詰め))
(defun aozora-proc-段落地付き () (aozora-proc-surround 段落地付き))
(defun aozora-proc-段落字上げ () (aozora-proc-surround 段落字上げ))
(defun aozora-proc-段落文字大 () (aozora-proc-surround 段落文字大))
(defun aozora-proc-段落文字小 () (aozora-proc-surround 段落文字小))
(defun aozora-proc-段組み     () 
  (let ((kei (pop aozora-proc-stack)))
    (if kei (aozora-proc-surround 段組み罫)　(aozora-proc-surround 段組み))))

;;; Markup

(defun aozora-proc-remove-ignorable ()
  (let (start end)
    (while (setq start (text-property-any (point-min) (point-max) 'aozora-proc-ignorable t))
      (goto-char start)
      (setq end (text-property-not-all (point) (point-max) 'aozora-proc-ignorable t))
      (delete-region start end))))

(defun aozora-proc-replace-text ()
  (let ((start (point-min)) end text props)
    (while (setq start (text-property-not-all start (point-max) 'aozora-proc-text nil))
      (goto-char start)
      (setq text (get-text-property (point) 'aozora-proc-text))
      ;;(setq props (text-properties-at (point)))
      (setq end (text-property-any (point) (point-max) 'aozora-proc-text nil))
      (delete-region start end)
      (insert text)
      ;;(add-text-properties start (+ start (length text)) props)
      )))

(defun aozora-proc-stack-check (stack old-props new-props prios)
  "スタックを確認し、新STACK,REMOVED,ADDEDを返す。"
  (let (removed added (new-stack stack) (curr-stack stack)
        prop val prio)
    (while curr-stack
      (setq prop (pop curr-stack)
            val  (pop curr-stack))
      (when (and prop (not (equal (plist-get new-props prop) val)))
        (while (not (equal curr-stack new-stack))
          (setq prop (pop new-stack))
          (setq val  (pop new-stack))
          (push prop removed)
          (push val removed)
          )))
    (dolist (markup markups)
      (setq prio (car markup))
      (setq val (plist-get new-props prio))
      (when (and val
                 (null (plist-get new-stack prio)))
        (push val new-stack)
        (push prio new-stack)
        (push prio added)
        (push val added)
        ))
    (list new-stack (nreverse removed) (nreverse added))))

(defun aozora-proc-insert (format val)
  (insert (if (stringp format) (format format val)
            (if (functionp format) (apply format (list val))
              (if (null format) "")))))

(defvar aozora-proc-kana-repeat-voiced "〲") ;; "〴〵"
(defvar aozora-proc-kana-repeat "〱") ;; "〳〵"

(defun aozora-proc-markup ()
  "現在のカーソル位置からバッファの最後までを `aozora-proc-method' でマークアップする。
事前に `aozora-proc-parse' しておく必要がある。"
  (interactive)
  (let* ((entry (assoc aozora-proc-method aozora-proc))
         (markups (eval (elt entry 2)))
         old-props new-props stack added removed pos)
    (while (setq pos (if (null pos) (point) (next-property-change (point))))
      (goto-char pos)
      (setq new-props (text-properties-at (point))
            result (aozora-proc-stack-check 
                    stack old-props new-props
                    markups)
            stack   (elt result 0)
            removed (elt result 1)
            added   (elt result 2))
      ;; close tags
      (while removed
        (aozora-proc-insert (elt (assoc (car removed) markups) 2) (cadr removed))
        (setq removed (cddr removed)))
      ;; open tags
      (while added
        (aozora-proc-insert (elt (assoc (car added) markups) 1) (cadr added))
        (setq added (cddr added)))
      (setq old-props new-props))
    (goto-char (point-min))
    (while (re-search-forward "／＼" nil t)
      (replace-match aozora-proc-kana-repeat))
    (goto-char (point-min))
    (while (re-search-forward "／″＼" nil t)
      (replace-match aozora-proc-kana-repeat-voiced))
    (aozora-proc-replace-text)
    (aozora-proc-remove-ignorable)
    ))

(defun aozora-proc-buffer (&optional buf)
  (interactive "PbBuffer: ")
  (let* ((entry (assoc aozora-proc-method aozora-proc))
         (preamble (eval (elt entry 3)))
         (prolog (eval (elt entry 4))))
    (save-excursion
      (if buf (switch-to-buffer buf))
      (aozora-proc-save-restriction
        (goto-char (point-min))
        (aozora-proc-parse)
        (goto-char (point-min))
        (aozora-proc-markup))
      (goto-char (point-min))
      (when preamble
        (goto-char (point-min))
        (if (stringp preamble) (insert preamble)
          (insert (car preamble))
          (re-search-forward aozora-proc-start-regexp nil t)
          (insert (cdr preamble))))
      (goto-char (point-max))
      (when prolog
        (if (stringp prolog) (insert prolog)
          (save-excursion (insert (cdr prolog)))
          (re-search-backward aozora-proc-end-regexp nil t)
          (insert (car prolog)))))))

(defun aozora-proc-file (file)
  "指定されたファイルを `aozora-proc-method' でマークアップして別ファイルに保存する。"
  (interactive "fFile name:")
  (if (not (file-exists-p file)) (error "File not found! %s" file))
  (let* ((entry (assoc aozora-proc-method aozora-proc))
         (suffix (elt entry 1))
         (new-file (concat (file-name-sans-extension file) suffix)))
    (if suffix
        (with-temp-file new-file
          (insert-file-contents file)
          (aozora-proc-buffer)
          (message "%s generated." new-file))
      (with-temp-buffer
        (insert-file-contents file)
        (aozora-proc-buffer)))))

;;;
;;; Markup Data
;;;

(defun aozora-proc-attach-prefix-lists (lists)
  (mapcar 
   (lambda (x)
     (cons 
      (intern (concat "aozora-proc-" (symbol-name (car x))))
      (cdr x))) lists))

;; HTML5 & CSS3

(defun aozora-proc-html (dir)
  (aozora-proc-attach-prefix-lists
   (let ((l-top (if (eq dir 'h) "left" "top"))     ;; 行頭
         (l-end (if (eq dir 'h) "right" "bottom")) ;; 行末
         (p-top (if (eq dir 'h) "top" "right"))    ;; 段落先頭
         (p-end (if (eq dir 'h) "bottm" "left"))   ;; 段落終端
         (l-wid (if (eq dir 'h) "width" "height")) ;; 行幅
         )
    `(;; 段落
      (文字下げ       ,(concat "<div style='margin-" l-top ": %dem'>") "</div>") ;; top -> left
      (地上げ         ,(concat "<div style='text-align:" l-end "; margin-" l-end ": %dem'>") "</div>") ;; bottom -> right
      (改行天付き     (lambda (x) (format ,(concat "<div style='margin-" l-top ": %dem; text-indent: -%dem;'>") x x))
                      "</div>")
      (文字下げ折り返し (lambda (x) (format ,(concat "<div style='margin-" l-top ": %dem; text-indent: %dem;'>")
                                            (car x) (- (cdr x) (car x)))) "</div>")
      (段落字詰め     ,(concat "<div style='max-" l-wid ": %dem;'>") "</div>")
      (段落文字大     (lambda (x)
                        (format "<div style='font-size: +%f%%'>" (* 100 (expt 1.2 x))))
                      "</div>")
      (段落文字小     (lambda (x)
                        (format "<div style='font-size: +%f%%'>" (* 100 (expt 1.2 (- x)))))
                      "</div>")
      (段落大見出し   "<div class='h3'>" "</div>")
      (段落中見出し   "<div class='h4'>" "</div>")
      (段落小見出し   "<div class='h5'>" "</div>")
      (段落太字       "<div style='font-weight: bold'>" "</div>")
      (段落斜体       "<div style='font-style: italic'>" "</div>")
      (段落罫囲み     "<div class='keigakomi'>" "</div>")
      (段落           "<p>" "</p>")
      (改段           "<hr/>" "")
      (改丁           "<hr/>" "")
      ;;              
      (注釈           "<small>" "</small>")
      (大見出し       "<h3>" "</h3>")
      (中見出し       "<h4>" "</h4>")
      (小見出し       "<h5>" "</h5>")
      (改丁           "<br/>")
      (改ページ       "<hr/>")
      ;; sidenote     
      (割り注         "<small>" "</small>")
      (ルビ           "<ruby>" "<rt>%s</rt></ruby>")
      (左ルビ         "" "<small>（%s）</small>")
      (漢文           (lambda (x)
                        (concat "<sup>" (car x) "</sup><sub>" (cdr x) "</sub>")))
      ;; inline       
      (縦中横         "<span class='tcy'>" "</span>")
      (窓大見出し     "<span class='h3'>" "</span>")
      (窓中見出し     "<span class='h4'>" "</span>")
      (窓小見出し     "<span class='h5'>" "</span>")
      (同行大見出し   "<span class='h3'>" "</span>")
      (同行中見出し   "<span class='h4'>" "</span>")
      (同行小見出し   "<span class='h5'>" "</span>")
      (大きな文字     (lambda (x)
                        (format "<span style='font-size: +%f%%'>" (* 100 (expt 1.2 x))))
                      "</span>")
      (小さな文字     (lambda (x)
                        (format "<span style='font-size: +%f%%'>" (* 100 (expt 1.2 (- x)))))
                      "</span>")
      (罫囲み         "<span class='keigakomi'>" "</span>")
      (二重罫囲み     "<span class='keigakomi'>" "</span>")
      (傍線           "<span class='ul'>" "</span>")
      (左に傍線       "<span class='ol'>" "</span>")
      (二重傍線       "<span class='ul_double'>" "</span>")
      (左に二重傍線   "<span class='ol_double'>" "</span>")
      (破線           "<span class='ul_dashed'>" "</span>")
      (左に破線       "<span class='ol_dashed'>" "</span>")
      (鎖線           "<span class='ul_dashed'>" "</span>")
      (左に鎖線       "<span class='ol_dashed'>" "</span>")
      (波線           "<span class='ul_wave'>" "</span>")
      (左に波線       "<span class='ol_wave'>" "</span>")
      (太字           "<b>" "</b>")
      (斜体           "<i>" "</i>")
      (下線           "<u>" "</u>")
      (上付き小文字   "<sup>" "</sup>")
      (下付き小文字   "<sub>" "</sub>")
      (小書き         "<span class='subscript'>" "</span>")
      (行右小書き     "<span class='subscript'>" "</span>")
      (行左小書き     "<span class='subscript'>" "</span>")
      (傍点           "<span class='sesame'>" "</span>")
      (左に傍点       "<span class='sesame_left'>" "</span>")
      (白ゴマ傍点     "<span class='open_sesame'>" "</span>")
      (左に白ゴマ傍点 "<span class='open_sesame_left'>" "</span>")
      (丸傍点         "<span class='circle'>" "</span>")
      (左に丸傍点     "<span class='circle_left'>" "</span>")
      (白丸傍点       "<span class='open_circle'>" "</span>")
      (左に白丸傍点   "<span class='open_circle_left'>" "</span>")
      (×傍点          "<span class='sesame'>" "</span>")
      (左に×傍点      "<span class='sesame_left'>" "</span>")
      (黒三角傍点     "<span class='triangle'>" "</span>")
      (左に黒三角傍点 "<span class='triangle_left'>" "</span>")
      (白三角傍点     "<span class='open_triangle'>" "</span>")
      (左に白三角傍点 "<span class='open_triangle_left'>" "</span>")
      (蛇の目傍点     "<span class='double-circle'>" "</span>")
      (左に蛇の目傍点 "<span class='double-circle_left'>" "</span>")
      (二重丸傍点     "<span class='open_double-circle'>" "</span>")
      (左に二重丸傍点 "<span class='open_double-circle_left'>" "</span>")
      (四角傍点       "<span class='sesame'>" "</span>")
      (左に四角傍点   "<span class='sesame_left'>" "</span>")
      (白四角傍点     "<span class='sesame'>" "</span>")
      (左に白四角傍点 "<span class='sesame_left'>" "</span>")
      ;;
      (改行           "<small>(改行)</small>")
      (図 (lambda (x)
            (concat "<img src='" (elt x 1) "' caption='" (elt x 0) "'"
                    (if (elt x 2) (format " height='%d'" (elt x 2)))
                    (if (elt x 3) (format " width='%d'" (elt x 3))) ">")))
     ))))

(defvar aozora-proc-html-h (aozora-proc-html 'h))
(defvar aozora-proc-html-v (aozora-proc-html 'v))

(defvar aozora-proc-html-h-preamble
'("<?xml version='1.0' encoding='utf-8'?>
<link href='aozora-proc-h.css' rel='stylesheet' type='text/css' />
<body><pre>\n" . "</pre>\n"))
(defvar aozora-proc-html-v-preamble
'("<?xml version='1.0' encoding='utf-8'?>
<link href='aozora-proc-v.css' rel='stylesheet' type='text/css' />
<body><pre>\n" . "</pre>\n"))
(defvar aozora-proc-html-prolog 
'("\n<pre>\n" . "\n</pre>\n</body>\n"))

;;; up-TeX

(defun aozora-proc-tex-bouten (dir side str)
  (list 
   (format
    (if (eq dir 'h)
        "\n\\renewcommand{\\boutenchar}{\\hbox{\\tiny %s}}\n\\bou{"
      (if (eq side 'r)
          "\n\\renewcommand{\\boutenchar}{\\hbox{\\tiny \\tate\\kern1zw %s}}\n\\bou{"
        "\n\\renewcommand{\\boutenchar}{\\hbox{\\tiny \\tate\\kern1zw \\hbox{\\yoko\\kern-5zw %s}}}\n\\bou{"))
    str) "}"))

(defun aozora-proc-uptex (dir)
  (aozora-proc-attach-prefix-lists
   `(
     (文字下げ)
     (地上げ         "{\\raggedright " "}")
     (改行天付き)
     (文字下げ折り返し)
     (段落字詰め)
     (大見出し       "\\section*{" "}")
     (中見出し       "\\subsection*{" "}")
     (小見出し       "\\subsubsection*{" "}")
     (段落           "\n" "\n")
     (罫囲み         "\\fbox{" "}")
     (二重罫囲み     "\\fbox{" "}")
     (割り注         "\\warichu{}{" "}") ;; warichu.sty
     (注釈           "\\footnote{" "}")
     (縦中横         "\\rensuji{" "}")
     (傍線           "\\ul{" "}")
     (二重傍線       "{\\setniju \\uc{" "}}")
     (左に傍線       "\\ol{" "}")
     (左に二重傍線   "{\\setniju \\uc{" "}}")
     (破線           "{\\sethasen \\uc{" "}}")
     (波線           "{\\setnami \\uc{" "}}")
     (太字           "{\\emph " "}")
     (斜体           "{\\it " "}")
     (下線           "\\ul{" "}")
     (ルビ           "\\kana{" "}{%s}")
     (上付き小文字   "$^{" "}$")
     (下付き小文字   "$_{" "}$")
     (漢文           (lambda (x)
                       (concat "\\kokana{" (car x) "}{" (cdr x) "}")))
     (小書き         "{\\small" "}")
     (傍点           ,@(aozora-proc-tex-bouten dir 'r "﹅"))
     (左傍点         ,@(aozora-proc-tex-bouten dir 'l "﹅"))
     (白ゴマ傍点     ,@(aozora-proc-tex-bouten dir 'r "﹆"))
     (左白ゴマ傍点   ,@(aozora-proc-tex-bouten dir 'l "﹆"))
     (丸傍点         ,@(aozora-proc-tex-bouten dir 'r "•"))
     (左丸傍点       ,@(aozora-proc-tex-bouten dir 'l "•"))
     (白丸傍点       ,@(aozora-proc-tex-bouten dir 'r "◦"))
     (左白丸傍点     ,@(aozora-proc-tex-bouten dir 'l "◦"))
     (×傍点          ,@(aozora-proc-tex-bouten dir 'r "×"))
     (左×傍点        ,@(aozora-proc-tex-bouten dir 'l "×"))
     (黒三角傍点     ,@(aozora-proc-tex-bouten dir 'r "▲"))
     (左黒三角傍点   ,@(aozora-proc-tex-bouten dir 'l "▲"))
     (白三角傍点     ,@(aozora-proc-tex-bouten dir 'r "△"))
     (左白三角傍点   ,@(aozora-proc-tex-bouten dir 'l "△"))
     (二重丸傍点     ,@(aozora-proc-tex-bouten dir 'r "◎"))
     (左二重丸傍点   ,@(aozora-proc-tex-bouten dir 'l "◎"))
     (蛇の目傍点     ,@(aozora-proc-tex-bouten dir 'r "◉"))
     (左蛇の目傍点   ,@(aozora-proc-tex-bouten dir 'l "◉"))
     (四角傍点       ,@(aozora-proc-tex-bouten dir 'r "■"))
     (左四角傍点     ,@(aozora-proc-tex-bouten dir 'l "■"))
     (白四角傍点     ,@(aozora-proc-tex-bouten dir 'r "□"))
     (左白四角傍点   ,@(aozora-proc-tex-bouten dir 'l "□"))
     )))

(defvar aozora-proc-uptex-h (aozora-proc-uptex 'h))
(defvar aozora-proc-uptex-v (aozora-proc-uptex 'v))

(defvar aozora-proc-uptex-preamble "\\begin{document}\n")
(defvar aozora-proc-uptex-prolog "\\end{document}\n")

;;; Markups
(defvar aozora-proc
  '((validate)
    (html-h ".html" aozora-proc-html-h
            aozora-proc-html-h-preamble aozora-proc-html-prolog)
    (html-v ".html" aozora-proc-html-v
            aozora-proc-html-v-preamble aozora-proc-html-prolog)
    (uptex-h ".tex"  aozora-proc-uptex-h
             aozora-proc-uptex-preamble aozora-proc-uptex-prolog)
    (uptex-v ".tex"  aozora-proc-uptex-v
             aozora-proc-uptex-preamble aozora-proc-uptex-prolog)))

(defun aozora-proc-script (argv)
  "青空文庫プロセッサをシェルスクリプトから起動した時の処理。"
  (setq aozora-proc-script t)
  (set-language-environment "Japanese")
  (let (argc files)
    (while argv
      (setq argc (pop argv))
      (cond ((equal argc "-h") (progn (princ aozora-proc-doc) (kill-emacs 1)))
            ((equal argc "-g") (setq aozora-proc-gaiji-file (pop argv)))
            ((equal argc "-l") (load (pop argv)))
            ((equal argc "-s") (setq aozora-proc-start-regexp (pop argv)))
            ((equal argc "-e") (setq aozora-proc-end-regexp (pop argv)))
            ((equal argc "-f") (progn (setq aozora-proc-method (intern (pop argv)))
                                      (if (null (assoc aozora-proc-method aozora-proc)) 
                                          (error "Not supported format!"))))
            ((string-match "^-" argc) (progn (message "Not supported argument -- %s" argc) (kill-emacs 1)))
            ((null argc) (progn (message "Argument not sufficient!") (kill-emacs 1)))
            (t (setq files (cons argc argv) argv nil))))
    (message "files=%s" files)
    (dolist (file-wc files)
      (dolist (file (file-expand-wildcards file-wc))
        (message "Processing %s..." file)
        (aozora-proc-file file)))
    (kill-emacs 0)))

;;; startup
(aozora-proc-initialize)
(if argv (aozora-proc-script argv))

(provide 'aozora-proc)
