;;; browser independent classes and functions

;;; debug flag
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *cl-bookmarks-debug* nil))


;;; bookmark class - browser independent
(defclass bookmark ()
  ((url :accessor url
        :initarg :url
        :initform (error "Must specify an URL"))

   (title :accessor title
          :initarg :title
          :initform nil)

   ;; creation time - in lisp format
   (c-time :accessor c-time
           :initarg :c-time
           :initform (get-universal-time))

   ;; modification time - in lisp format
   (m-time :accessor  m-time
           :initarg :m-time
           :initform (get-universal-time))

   ;; last visit time - in lisp format
   (v-time :accessor v-time
           :initarg :v-time
           :initform (get-universal-time))

   ;; list of tags
   (tags :accessor tags
         :initarg :tags
         :initform nil))
  (:documentation "Class containing basic elements for a bookmark"))


(defmethod print-object ((bookmark bookmark) stream)
  "Print the bookmark object to the specified stream"
  (format stream "<bookm: title=~a, url=~a, c-time=~a tags: ~a>"
          (title bookmark) (url bookmark) (c-time bookmark) (tags bookmark)))


(defun unix-to-lisp-time (unix-time)
  "Convert an integer value representing a unix time to lisp time"
  (+ unix-time (encode-universal-time 0 0 0 1 1 1970)))

  
;;; * emacs display settings *
;;; Local Variables:
;;; default-tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
