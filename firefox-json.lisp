;;;; QRH
;;;; * pretty-print json: python -mjson.tool in.json > out.json
;;;; * convert jsonlz4 to json: lz4jsoncat (https://github.com/andikleen/lz4json)
;;;;
;;;; Initially, the hierarchy of the bookmarks and tags was based on a tree-like
;;;; structure. If a bookmark had 2 tags (e.g. 'news' and 'photo'), there were at least 2
;;;; instances of the bookmark having as parents the tags 'news' and 'photo'.
;;;; In newer versions of firefox, there is only one instance of the URL that has the tags
;;;; as a comma-separated string.


;;; functions handling firefox bookmarks from json files
(in-package :cl-bookmarks)

;; all-bookmarks is a hash table for bookmark objects. The key is the uri.
(let (all-bookmarks)

  (defun init-bookm-hash-table ()
    (setf all-bookmarks (make-hash-table :test 'equal)))

  (defun add-or-update-bookm (uri title c-time m-time tag-list &optional tag)
    "Add or update a bookmark in all-bookmarks hash table"
    (let ((existing-bookm (gethash uri all-bookmarks)))
      (if existing-bookm
          (progn
            ;; update title if empty
            (when (zerop (length (title existing-bookm)))
              (setf (title existing-bookm) title))
            ;; add tags from tag-list
            (when tag-list
              (dolist (tag-elt tag-list)
                (when (not (bookm-has-tag-p existing-bookm tag-elt))
                  (bookm-add-tag existing-bookm tag-elt))))
            ;; add tag
            (when (not (bookm-has-tag-p existing-bookm tag))
              (bookm-add-tag existing-bookm tag))
            ;; update c-time or m-time
            (when (zerop (c-time existing-bookm))
              (setf (c-time existing-bookm) c-time))
            (when (zerop (m-time existing-bookm))
              (setf (m-time existing-bookm) m-time)))
          (progn
            (setf (gethash uri all-bookmarks)
                  (if tag
                      (make-instance 'bookmark :url uri :title title
                                     :c-time c-time :m-time m-time
                                     :tags (list tag))
                      (make-instance 'bookmark :url uri :title title
                                     :c-time c-time :m-time m-time
                                     :tags tag-list)))))))

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


;; :fixme: optimize or use something else
(defun tokenize-comma-separated-string (str)
  "Tokenize a comma separated string. Empty elements are discarded."
  (let (result cur-elt)
    (loop for char across str do
      (if (equal char #\,)
          (progn
            (when cur-elt
              (setf result (append result (list cur-elt))))
            (setf cur-elt nil))
          (setf cur-elt (concatenate 'string cur-elt (string char)))))
    (when cur-elt
      (setf result (append result (list cur-elt))))
    result))


(defun do-json-obj (obj &optional tag)
  "Analyze a json object"
  (if (slot-exists-p obj 'type)
      (let ((type (slot-value obj 'type)))
        (when *cl-bookmarks-debug*
          (format t ":debug: parsing a json obj having type ~a~%" type))
        (cond
          ((equal type  "text/x-moz-place-container")
           (do-json-container obj))
          ((equal type "text/x-moz-place")
           (do-json-uri obj tag))
          ((equal type "text/x-moz-place-separator")) ; nothing to do with it
          (t
           (format t "Warning: unknown json type: ~a. Ignoring.~%" type))))
      (error "Strange object, does not have 'type' slot")))


(defun do-json-container (obj)
  "Analyze a json container"
  (let* ((title (if (slot-exists-p obj 'title) (slot-value obj 'title) ""))
         parent-id tag)
    (when *cl-bookmarks-debug*
      (format t ":debug: found a container with name ~a~%" title))
    (when (slot-boundp obj 'parent)
      (setf parent-id (slot-value obj 'parent))
      ;; an element that has parent with id 4 is a tag
      (when (equal parent-id 4)
        (setf tag title)))
    (when (and *cl-bookmarks-debug* tag)
      (format t ":debug: container is a tag~%"))
    (when (slot-boundp obj 'children)
      (let ((children (slot-value obj 'children)))
        (dotimes (i (length children))
          (do-json-obj (elt children i) tag))))))


(defun do-json-uri (obj tag)
  "Analyze a json uri"
  ;;(format t "Found a uri with name ~a~%" (slot-value obj 'title))
  (handler-case
      (let ((uri (slot-value obj 'uri))
            (title (if (slot-boundp obj 'title) (slot-value obj 'title) ""))
            (c-time (from-frx-time (if (slot-boundp obj 'date-added)
                                       (slot-value obj 'date-added)
                                       0)))
            (m-time (from-frx-time (if (slot-boundp obj 'last-modified)
                                       (slot-value obj 'last-modified)
                                       0)))
            (tag-string (when (slot-boundp obj 'tags) (slot-value obj 'tags)))
            tag-list)
        (when tag-string
          (setf tag-list (tokenize-comma-separated-string tag-string)))
        (add-or-update-bookm uri title c-time m-time tag-list tag))
    (condition (c)
      (format t "Error parsing bookmark ~a with title ~a (error: ~a)~%"
              obj (slot-value obj 'title) c)
      (error c))))


(defun frx-json-parse-file (filename)
  "Load bookmarks from a json file exported by firefox. Return a list with all
bookmarks (sorted by uri)."
  (init-bookm-hash-table)
  (json:with-decoder-simple-clos-semantics ; switch to clos
    (let ((json:*json-symbols-package* nil))
      (with-open-file (json-strm filename :direction :input)
        (let ((root (json:decode-json json-strm)))
          (do-json-obj root)))))
  (get-bookm-sorted-by-uri))


(defun frx-json-to-txt (json-fname txt-fname &optional print-bookm)
  "Parse a json file and save all bookmarks to txt-fname (using print-bookm
  function if provided or in standard format: url, title and tags, each one of
  these on a line"
  (let ((bookm-list (frx-json-parse-file json-fname)))
    (with-open-file (output txt-fname :direction :output :if-exists :supersede)
      (dolist (bookm bookm-list)
        (if print-bookm
            (funcall print-bookm bookm output)
            (format output "~a~%~a~%tags: ~{~a~^, ~}~%create-time: ~a (~a)~%modification-time ~a (~a)~%~%"
                    (url bookm) (title bookm) (sort (tags bookm) 'string<)
                    (lisp-time-str (c-time bookm)) (c-time bookm)
                    (lisp-time-str (m-time bookm)) (m-time bookm)))))))


(defun main-frx-json-to-txt (argv)
  "Function used by buildapp to export firefox bookmarks from json to a txt file.
ARGV : list of command line arguments (ARGV[1] = json file, ARGV[2] = txt file)"
  (let* ((json-file "bookmarks.json")
         (txt-file "bookmarks.txt"))
    (when (and (> (length argv) 1) (string= (second argv) "-d"))
      (setf *cl-bookmarks-debug* t)
      (setf argv (remove "-d" argv :test #'string=)))
    (when (> (length argv) 2)
      (setf json-file (second argv))
      (setf txt-file (third argv)))
    (format t "Converting bookmarks from ~a to ~a~%" json-file txt-file)
    (frx-json-to-txt json-file txt-file)))
