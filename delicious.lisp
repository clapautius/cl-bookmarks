;;; functions for delicious import

(defclass dlc-handler (hax:default-handler)
  ((inside-anchor :accessor inside-a
                   :initform nil)
   (current-bookmark :accessor bookm
                     :initform nil)
   (current-text :accessor text
                 :initform nil)))


(defun split-string (string char)
  (remove-if (lambda (a) (zerop (length a)))
             (loop for i = 0 then (1+ j)
                as j = (position char string :start i)
                collect (subseq string i j)
                while j)))


(let (dlc-bookmarks)

  (defmethod hax:start-element ((h dlc-handler) name attributes)
    (flet ((get-tag (tag-name)
             (find-if
              (lambda (a) (string-equal tag-name (hax:attribute-name a)))
              attributes))
           (get-tag-value (tag-elt)
             (when tag-elt
               (hax:attribute-value tag-elt))))
      (when (string-equal name "a")
        (setf (inside-a h) t)
        ;;(format t ":debug: got A element~%")
        (let* ((href (get-tag "href"))
               (tags (get-tag "tags"))
               (c-time (get-tag "add_date")))
          ;;(format t ":debug: href=~a, tags=~a~%"
          ;;(hax:attribute-value href) (hax:attribute-value tag))
          (setf (bookm h)
                (make-instance
                 'bookmark :url (get-tag-value href)
                 :c-time (unix-to-lisp-time
                          (parse-integer (get-tag-value c-time)))
                 :tags (mapcar (lambda (a) (string-trim '(#\Space) a))
                               (split-string (get-tag-value tags) #\,))))
          ;;(format t ":debug: created new bookmark: ~a~%" (bookm h))
          ))))


  (defmethod hax:characters ((h dlc-handler) data)
    (when (inside-a h)
      (setf (text h) data)))


  (defmethod hax:end-element ((h dlc-handler) name)
    (when (string-equal name "a")
      (setf (inside-a h) nil)
      (setf (title (bookm h)) (text h))
      (setf (text h) nil)
      (if (null dlc-bookmarks)
          (setf dlc-bookmarks (list (bookm h)))
          (nconc dlc-bookmarks (list (bookm h))))
      (setf (bookm h) nil)))


  (defun dlc-reset-bookm-list ()
    (setf dlc-bookmarks nil))


  (defun dlc-parse-file (path)
    "Parse html file exported from delicious. Return list of bookmark objects."
    (dlc-reset-bookm-list)
    (chtml:parse (pathname path) (make-instance 'dlc-handler))
    dlc-bookmarks))


;;; * emacs display settings *
;;; Local Variables:
;;; default-tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
