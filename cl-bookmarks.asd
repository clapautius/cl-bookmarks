;;; run from SLIME or make a shortcut for: 
;;;   (asdf:oos 'asdf:load-op 'cl-bookmarks) or
;;;   (require 'cl-bookmarks)

(defpackage #:cl-bookmarks-system (:use #:cl #:asdf))
(in-package :cl-bookmarks-system)

(require 'clsql-sqlite3)

(asdf:defsystem :cl-bookmarks
    :components ((:file "package")                        
                 (:file "cl-bookmarks"
                        :depends-on ("package"))
                 (:file "firefox"
                        :depends-on ("package"
                                     "cl-bookmarks"))))


;;; * emacs display settings *
;;; Local Variables:
;;; default-tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
