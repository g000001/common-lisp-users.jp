#!/lisp/bin/gosh

(use text.gettext)

;;; WiLiKiをロードパスに追加
(add-load-path "/var/www/common-lisp-users.jp/lib")

(use wiliki.page)
(use wiliki)
(use wiliki.rss)

;;; ユーティリティ

(define $$ gettext)

;;; フォーマッタ

(define-class <cl-users-jp-formatter> (<wiliki-formatter>) ())

(wiliki:formatter (make <cl-users-jp-formatter>))

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

;;; フィード

(rss-item-description 'html)

;;; エントリーポイント

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
