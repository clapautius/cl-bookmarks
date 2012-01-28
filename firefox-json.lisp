;;; functions handling firefox bookmarks from json files
(in-package :cl-bookmarks)

;; all-bookmarks is a hash table for bookmark objects. The key is the uri.
(let ((all-bookmarks (make-hash-table :test 'equal)))

  (defun add-or-update-bookm (uri title &optional tag)
    "Add or update a bookmark in all-bookmarks hash table"
    (let ((existing-bookm (gethash uri all-bookmarks)))
      (if existing-bookm
          (progn
            ;; update title if empty
            (when (zerop (length (title existing-bookm)))
              (setf (title existing-bookm) title))
            ;; add tag
            (when (not (bookm-has-tag-p existing-bookm tag))
              (bookm-add-tag existing-bookm tag)))
          (progn
            (setf (gethash uri all-bookmarks) 
                  (make-instance 'bookmark :url uri :title title
                                 :tags (list tag)))))))

  (defun get-bookm-sorted-by-uri ()
    "Return a list of bookmarks sorted alphabetically by uri"
    (let ((bookm-list (list 0))) ;; use a fake first elt
      (maphash (lambda (key value) (declare (ignore key))
                       (nconc bookm-list (list value)))
               all-bookmarks)
      (setf bookm-list (cdr bookm-list)) ;; skip the fake first elt
      (sort bookm-list (lambda (a b) (string< (url a) (url b))))))

  (defun get-all-bookm ()
    "Return a list with all bookmarks (unsorted)"
    (let ((bookm-list (list 0))) ;; use a fake first elt
      (maphash (lambda (key value) (declare (ignore key))
                       (nconc bookm-list (list value)))
               all-bookmarks)
      (cdr bookm-list))))
      

(defun do-json-obj (obj)
  "Analyze a json object"
  (if (slot-exists-p obj 'type)
      (cond
        ((equal (slot-value obj 'type) "text/x-moz-place-container")
         (do-json-container obj))
        ((equal (slot-value obj 'type) "text/x-moz-place")
         (do-json-uri obj))
        (t
         (format t "Warning: unknown json type: ~a. Ignoring.~%"
                 (slot-value obj 'type))))
      (error "Strange object, does not have 'type' slot")))


(defun do-json-container (obj)
  "Analyze a json container"
  (format t "Found a container with name ~a~%(press)~%" (slot-value obj 'title))
  (read)
  (when (slot-exists-p obj 'children)
    (let ((children (slot-value obj 'children)))
      (dotimes (i (length children))
        (do-json-obj (elt children i))))))


(defun do-json-uri (obj)
  "Analyze a json uri"
  (format t "Found a uri with name ~a~%" (slot-value obj 'title)))


(defun frx-json-parse-file (filename)
  "Load bookmarks from a json file exported by firefox"
  (json:with-decoder-simple-clos-semantics ; switch to clos
    (let ((json:*json-symbols-package* nil))
      (with-open-file (json-strm filename :direction :input)
        (let ((root (json:decode-json json-strm)))
          (do-json-obj root)))))
  (format t "~%DONE~%"))
