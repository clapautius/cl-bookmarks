(defpackage :cl-bookmarks
  (:use :common-lisp :clsql-sqlite3)
  (:export :bookmark
           :bookm-has-tag-p
           :*cl-bookmarks-debug*
           :*cl-bookmarks-trace-sql*
           :v-time
           :c-time
           :m-time
           :url
           :frx-open-file 
           :frx-close-file
           :frx-add-bookm
           :frx-get-all-bookm
           :frx-get-bookm-by-url
           :frx-get-bookm-by-tags))


;;; * emacs display settings *
;;; Local Variables:
;;; default-tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
