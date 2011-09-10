#!/lisp/bin/gosh

(use gauche.charconv)
(use gauche.parameter)
(use srfi-1)
(use srfi-13)
(use srfi-19)
(use rfc.822)
(use rfc.http)
(use rfc.uri)
(use sxml.ssax)
(use sxml.sxpath)
(use sxml.tools)
(use text.gettext)
(use util.list)

;;; WiLiKiをロードパスに追加
(add-load-path "/var/www/common-lisp-users.jp/lib")

(use wiliki.page)
(use wiliki)
(use wiliki.rss)

;;;; ユーティリティ

;;; フォーマッタのフッタに対する処理で使っているだけなので、
;;; 不要なら適宜書き換えて削除すること
(define $$ gettext)

;;;; フォーマッタ

;;; <wiliki-formatter>のサブクラスを作って整形処理を特殊化する
;;; それぞれの処理はメソッドとして定義されているので、
;;; オーバーライドすることで必要な処理だけ動作をカスタマイズできる
(define-class <cl-users-jp-formatter> (<wiliki-formatter>) ())

;;; 整形に使われるフォーマッタのインスタンスをサブクラスのものに変更する
(wiliki:formatter (make <cl-users-jp-formatter>))

;;; ヘッダ内でのページのタイトル（titleタグの中身）
(define-method wiliki:format-head-title
    ((fmt <cl-users-jp-formatter>) (page <wiliki-page>) . options)
  (let ((wiliki-title (ref (wiliki) 'title))
        (page-title (ref page 'title)))
    (if (equal? page-title wiliki-title)
        page-title
        ;; 一部ページ名にサイト名を含むものがあるので削る
        (if-let1 after (string-scan page-title #`",|wiliki-title|: " 'after)
                 #`",after - ,wiliki-title"
                 #`",page-title - ,wiliki-title"))))

;;; フッタ
(define-method wiliki:format-page-footer
    ((fmt  <cl-users-jp-formatter>) (page <wiliki-page>) . options)
  (define (last-modified)
    `(div (@ (id "last-modified"))
          ,($$ "Last modified : ")
          ,(wiliki:format-time (ref page 'mtime))))
  (define (banners)
    `(div (@ (id "banners"))
          (a (@ (rel "license")
                (href "http://creativecommons.org/publicdomain/zero/1.0/"))
             (img (@ (src "http://i.creativecommons.org/p/zero/1.0/80x15.png")
                     (alt "CC0 1.0"))))))
  (define (powered-by)
    (define (wiliki)
      `((a (@ (href "http://practical-scheme.net/wiliki/wiliki.cgi"))
           "WiLiKi")
        " "
        ,(wiliki:version)))
    (define (gauche)
      `((a (@ (href "http://practical-scheme.net/gauche/index-j.html"))
           "Gauche")
        " "
        ,(gauche-version)))
    `(div (@ (id "powered-by"))
          "Powerd by " ,@(wiliki) " on " ,@(gauche)))
  (if (ref page 'mtime)
    `((hr)
      (div (@ (style "text-align: right"))
           ,(last-modified)
           ,(banners)
           ,(powered-by)))
    '()))

;;;; リーダーマクロ

;;; $$hs n
;;;
;;; HyperSpecのnの項目へのリンクを表示する。
;;; L1sp.orgのリダイレクトサービスを使っている。

(define-reader-macro (hs n)
  (let ((url #`"http://l1sp.org/cl/,n"))
    `((a (@ (href ,url)) ,n))))

;;; $$wp term [str]
;;;
;;; Wikipediaのtermの項目へのリンクを表示する。
;;; リンクの文字列は、strが指定されている場合はstr、それ以外はtermになる。
;;; termが"ja:"か"en:"で始まる場合、対応する言語の記事を参照する。
;;; 省略された場合は日本語の記事を参照する。
(define-reader-macro (wp term . opts)
  (let-optionals* opts ((str #f))
    (let* ((matched (#/^(ja|en):(.*)$/ term))
           (lang (if matched (matched 1) "ja"))
           (term (if matched (matched 2) term))
           (encoded (uri-encode-string term :encoding 'utf-8))
           (url #`"http://,|lang|.wikipedia.org/wiki/,|encoded|"))
      `((a (@ (href ,url)) ,(if str str term))))))

;;; $$feed url [max [enc]]
;;;
;;; urlにあるフィードのヘッドラインをmax件表示する。
;;; フィードのエンコーディングが(gauche-character-encoding)と違う場合、
;;; encでエンコーディングを指定することができる。参照できるフィードは、
;;; URLがcl-users-jp:feed-whitelistに登録されたパターンの
;;; いずれかとマッチするものに限られる。

(define cl-users-jp:feed-whitelist (make-parameter '()))

(define-reader-macro (feed url . args)
  (define item-max
    (if (null? args) 10 (car args)))
  (define date-format "~Y/~m/~d")
  (define item-format "(~a) ~a")
  (define ns
    '((atom1 . "http://www.w3.org/2005/Atom")
      (atom03 . "http://purl.org/atom/ns#")
      (openSearch . "http://a9.com/-/spec/opensearchrss/1.0/")
      (georss . "http://www.georss.org/georss")
      (thr . "http://purl.org/syndication/thread/1.0")
      (rss1 . "http://purl.org/rss/1.0/")
      (rdf . "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
      (content . "http://purl.org/rss/1.0/modules/content/")
      (dc . "http://purl.org/dc/elements/1.1/")
      (foo . "bar")))
  (define feed-attrs
    `((atom03
       (converter
        ,(sxpath '(// atom03:entry atom03:title *text*))
        ,(sxpath '(// atom03:entry
                   (atom03:link (@ (equal? (rel "alternate"))))
                   @ href *text*))
        ,(sxpath '(// atom03:entry atom03:issued *text*)))
       (date-parser . ,w3cdtf->date))
      (atom1
       (converter
        ,(sxpath '(// atom1:entry atom1:title *text*))
        ,(sxpath '(// atom1:entry
                   (atom1:link (@ (equal? (rel "alternate"))))
                   @ href *text*))
        ,(sxpath '(// atom1:entry atom1:published *text*)))
       (date-parser . ,w3cdtf->date))
      (rss1
       (converter
        ,(sxpath '(// rss1:item rss1:title *text*))
        ,(sxpath '(// rss1:item rss1:link *text*))
        ,(sxpath '(// rss1:item dc:date *text*)))
       (date-parser . ,w3cdtf->date))
      (rss2
       (converter
        ,(sxpath '(// item title *text*))
        ,(sxpath '(// item link *text*))
        ,(sxpath '(// item pubDate *text*)))
       (date-parser . ,rfc822-date->date))))
  (define converter-title car)
  (define converter-link cadr)
  (define converter-date caddr)
  (define (w3cdtf->date str)
    (define (df->nano df)
      (string->number (string-pad-right (number->string df) 9 #\0)))
    (and-let*
        ((match (#/^(\d\d\d\d)(?:-(\d\d)(?:-(\d\d)(?:T(\d\d):(\d\d)(?::(\d\d)(?:\.(\d+))?)?(?:Z|([+-]\d\d):(\d\d)))?)?)?$/ str)))
      (receive (year month day hour minute second df zh zm)
          (apply values (map (lambda (i) (x->integer (match i))) (iota 9 1)))
        (make-date (df->nano df)
                   second minute hour day month year
                   (* (if (negative? zh) -1 1)
                      (+ (* (abs zh) 3600) (* zm 60)))))))
  (define (feed-attr type field)
    (cdr (assoc field (cdr (assoc type feed-attrs)))))
  (define (decompose-uri url)
    (receive (_ specific) (uri-scheme&specific url)
      (uri-decompose-hierarchical specific)))
  (define (authority&path?query url)
    (define (path?query path query)
      (with-output-to-string
        (lambda ()
          (display path)
          (when query (format #t "?~a" query)))))
    (receive (authority path query _) (decompose-uri url)
      (values authority (path?query path query))))
  (define (feed-get url)
    (receive (authority path?query) (authority&path?query url)
      (receive (status header body) (http-get authority path?query)
        (unless (equal? status "200")
          (error "フィードの読み込みに失敗しました。ステータスコードは~aです。"
                 status))
        (if (or (null? args) (null? (cdr args)))
            body
            (ces-convert body (cadr args))))))
  (define (feed->sxml feed)
    (with-input-from-string feed
      (cut ssax:xml->sxml (current-input-port) ns)))
  (define (feed-type-of sxml)
    (define root-node ((car-sxpath '(*)) sxml))
    (define (atom03?)
      (eq? (sxml:name root-node) 'atom03:feed))
    (define (atom1?)
      (eq? (sxml:name root-node) 'atom1:feed))
    (define (rss1?)
      (eq? (sxml:name root-node) 'rdf:RDF))
    (define (rss2?)
      (and (eq? (sxml:name root-node) 'rss)
           (equal? (sxml:attr root-node 'version) "2.0")))
    (cond ((atom03?) 'atom03)
          ((atom1?) 'atom1)
          ((rss1?) 'rss1)
          ((rss2?) 'rss2)
          (else (error "対応していない種類のフィードです。"))))
  (define (take-item-max nodes)
    (let1 max item-max
      (take* nodes max)))
  (define (whitelisted? url)
    (any (lambda (regexp) (regexp url)) (cl-users-jp:feed-whitelist)))
  (unless (whitelisted? url)
    (error "許可されていないフィードです"))
  (let* ((feed (feed->sxml (feed-get url)))
         (type (feed-type-of feed))
         (conv (feed-attr type 'converter))
         (proc (feed-attr type 'date-parser))
         (title (converter-title conv))
         (link (converter-link conv))
         (date (converter-date conv)))
    `((ul ,@(map (lambda (title link date)
                   `(li (a (@ (href ,link))
                           ,(format #f item-format
                                    (date->string (proc date) date-format)
                                    title))))
                 (take-item-max (title feed))
                 (take-item-max (link feed))
                 (take-item-max (date feed)))))))

;;;; パラメータの設定

(rss-item-description 'html)

(cl-users-jp:feed-whitelist
 '(#/^http:\/\/pipes.yahoo.com\/pipes\/pipe.run\?_id=9a3d4f84998e798dea7e9b5838679fb8&_render=rss$/
   #/^http:\/\/api.atnd.org\/events\/\?keyword=Common%20Lisp&format=atom$/))

;;;; エントリーポイント

(define (main args)
  (wiliki-main
   (make <wiliki>
     :db-path "/var/www/common-lisp-users.jp-data/wiliki.dbm"
     :log-file "/var/www/common-lisp-users.jp-data/wikidata.log"
     :top-page "Common LISP users jp"
     :title "Common LISP users jp"
     :description "Common LISP users JP"
     :style-sheet "wiliki.css"
     :gettext-paths "/lisp/share/locale"
     :language 'jp
     :charsets '((jp . euc-jp) (en . euc-jp))
     :debug-level 0)))

;;;; Local variables:
;;;; mode: scheme
;;;; end:
