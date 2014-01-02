;;; run from SLIME or make a shortcut for: 
;;;   (asdf:oos 'asdf:load-op 'cl-bookmarks-conv) or
;;;   (require 'cl-bookmarks-conv)

(defpackage #:cl-bookmarks-conv-system (:use #:cl #:asdf))
(in-package :cl-bookmarks-conv-system)

(require 'cl-bookmarks)
(require 'closure-html)
(require 'cl-json)

(asdf:defsystem :cl-bookmarks-conv
    :components ((:file "package-conv")
                 (:file "delicious"
                        :depends-on ("package-conv"))
                 (:file "conversions"
                        :depends-on ("package-conv"
                                     "delicious"))
                 (:file "firefox-json"
                        :depends-on ("package-conv"))))


;;; * emacs display settings *
;;; Local Variables:
;;; default-tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
