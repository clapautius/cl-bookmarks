;;; browser independent classes and functions
(in-package :cl-bookmarks)


;;; debug flag
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *cl-bookmarks-debug* nil)
  (defparameter *cl-bookmarks-trace-sql* nil))


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
           :initform 0)

   ;; modification time - in lisp format
   (m-time :accessor  m-time
           :initarg :m-time
           :initform 0)

   ;; last visit time - in lisp format
   (v-time :accessor v-time
           :initarg :v-time
           :initform 0)

   ;; list of tags
   (tags :accessor tags
         :initarg :tags
         :initform nil))
  (:documentation "Class containing basic elements for a bookmark"))


(defmethod print-object ((bookmark bookmark) stream)
  "Print the bookmark object to the specified stream"
  (format stream "<bookm: ~a, url=~a~%  vt=~a, ct=~a, mt=~a~%  tags: ~a>"
          (shorten (title bookmark)) (shorten (url bookmark) 35)
          (v-time bookmark) (c-time bookmark) (m-time bookmark)
          (tags bookmark)))


(defgeneric bookm-add-tag (bookmark tag))

(defmethod bookm-add-tag ((bookmark bookmark) tag)
  "A null tag is ignored."
  (when tag
    (if (null (tags bookmark))
        (setf (tags bookmark) (list tag))
        (nconc (tags bookmark) (list tag)))))

(defgeneric bookm-has-tag-p (bookmark tag))

(defmethod bookm-has-tag-p ((bookmark bookmark) tag)
  (position tag (tags bookmark) :test #'string-equal))

(defgeneric bookm-export (bookmark output-stream))

(defmethod bookm-export ((bookm bookmark) output-stream)
  (format output-stream "~a~%~a~%tags: ~{~a~^, ~}~%create-time: ~a (~a)~%modification-time: ~a (~a)~%~%"
          (url bookm) (or (title bookm) "No title") (sort (tags bookm) 'string<)
          (lisp-time-str (c-time bookm)) (c-time bookm)
          (lisp-time-str (m-time bookm)) (m-time bookm)))


(defun unix-to-lisp-time (unix-time)
  "Convert an integer value representing a unix time to lisp time"
  (+ unix-time (encode-universal-time 0 0 0 1 1 1970)))


(defun date-str (lisp-time)
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time lisp-time)
    (format nil "~2,'0d-~2,'0d-~2,'0d" year month day)))


(defun lisp-time-str (lisp-time)
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time lisp-time)
    (format nil "~2,'0d-~2,'0d-~2,'0d,~2,'0d:~2,'0d:~2,'0d" year month day hour min sec)))


(defun shorten (str &optional (len 17))
  "Shorten a string to the first len elements"
  (cond
    ((> (length str) len) (concatenate 'string (subseq str 0 len) "..."))
    (t str)))


;;; * emacs display settings *
;;; Local Variables:
;;; default-tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
