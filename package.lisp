(defpackage :cl-bookmarks
  (:use :common-lisp :clsql-sqlite3)
  (:export :bookmark
           :frx-open-file 
           :frx-close-file
           :frx-add-bookm 
           :frx-get-bookm-by-url))


;;; * emacs display settings *
;;; Local Variables:
;;; default-tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
