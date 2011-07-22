#!/lisp/bin/gosh

;;; WiLiKiをロードパスに追加
(add-load-path "/var/www/common-lisp-users.jp/lib")

(use wiliki)
(use wiliki.rss)

(rss-item-description 'html)

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

