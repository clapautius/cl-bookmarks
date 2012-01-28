;;; functions handling firefox bookmarks from json files
(in-package :cl-bookmarks)


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
