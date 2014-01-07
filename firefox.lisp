;;; functions handling firefox bookmarks from the sqlite database
(in-package :cl-bookmarks)


(defun frx-open-file (&optional (path "places.sqlite"))
  "Open an sqlite connection to the specified file."
  (clsql:connect (list path) :database-type :sqlite3 :if-exists :old)
  (frx-read-tags))


(defun frx-close-file ()
  "Close the sqlite connection"
  (clsql:disconnect)
  (frx-clear-tags))



(let (frx-tags)

  (defun frx-print-tags (&optional stream)
    "Print tags"
    (format stream "Tags in memory: ~a" frx-tags))


  (defun frx-read-tags ()
    "Read tags from the sqlite database"
    (let* ((query "select id, title from moz_bookmarks where parent=4"))
      (dolist (tag (clsql:query query :field-names nil))
        (setf frx-tags (acons (second tag) (first tag) frx-tags)))))


  (defun frx-clear-tags ()
    "Clear tags list"
    (setf frx-tags nil))


  (defun frx-add-tag (tag-name)
    "Add a tag into the tags list and into the sqlite database. Return the id of
the new tag."
    (clsql:insert-records :into "moz_bookmarks"
                          :attributes '(type fk parent title
                                        dateAdded lastModified)
                          :values (list 2 0 4 tag-name (frx-time) (frx-time)))
    (let ((tag-id (car (car (clsql:query
                             "select last_insert_rowid() from moz_bookmarks"
                             :field-names nil)))))
      (setf frx-tags (acons tag-name tag-id frx-tags))
      tag-id))


  (defun frx-get-tag-id (tag-name &key (if-not-exist :skip))
    "Add tag if it does not exist and if-not-exist is :add. Return the tag id or
nil if the tag does not exist and cannot be added (if-not-exist is :skip)."
    (let ((tag (assoc tag-name frx-tags :test #'equal)))
      (if (null tag)
          (when (eql if-not-exist :add)
              (frx-add-tag tag-name))
          (cdr tag))))

  ) ; end frx-tags closure


(defun frx-get-all-bookm (&optional skip-title)
  (when *cl-bookmarks-debug*
    (format t ":debug: searching for all bookmarks"))
  (let* ((query
          (if skip-title
              (format nil "select url, fk, last_visit_date, dateAdded,
lastModified, parent from moz_bookmarks b, moz_places p where fk=p.id")
              (format nil "select b.title, url, fk, last_visit_date, dateAdded,
lastModified, parent from moz_bookmarks b, moz_places p where fk=p.id")))
         (results (clsql:query query :field-names nil))
         bookmark all-bookmarks parent)
    (when *cl-bookmarks-trace-sql*
      (format t ":debug: ~a~%" query))
    (dolist (bookm-sql results)
      (when *cl-bookmarks-trace-sql*
        (format t ":debug: parsing bookm ~a~%" bookm-sql))
      (setf parent (nth (if skip-title 5 6) bookm-sql))
      (when (eq (frx-get-type-of-elt parent) :bookmark)
        (setf bookmark
              (if skip-title
                  (make-instance 'bookmark
                                 :url (first bookm-sql)
                                 :v-time (from-frx-time (nth 2 bookm-sql))
                                 :c-time (from-frx-time (nth 3 bookm-sql))
                                 :m-time (from-frx-time (nth 4 bookm-sql)))
                  (make-instance 'bookmark :title (first bookm-sql)
                                 :url (second bookm-sql)
                                 :v-time (from-frx-time (nth 3 bookm-sql))
                                 :c-time (from-frx-time (nth 4 bookm-sql))
                                 :m-time (from-frx-time (nth 5 bookm-sql)))))
        ;; get tags
        (let* ((fk-pos (if skip-title 1 2))
               (query (format nil "select b.title from
moz_bookmarks a, moz_bookmarks b where a.fk=~a and b.parent=4 and a.parent=b.id"
                              (nth fk-pos bookm-sql)))
               (tags (clsql:query query :field-names nil)))
          (when *cl-bookmarks-trace-sql*
            (format t ":debug: ~a~%" query)
            (format t ":debug: tags: ~a~%" tags))
          (dolist (tag tags)
            (bookm-add-tag bookmark (car tag)))
          (setf all-bookmarks (cons bookmark all-bookmarks)))))
    all-bookmarks))


(defun frx-get-bookm-by-id (id &optional skip-title)
  "Return a bookmark object from the firefox database."
  (when (null id)
    (return-from frx-get-bookm-by-id nil))
  (when *cl-bookmarks-debug*
      (format t ":debug: searching for bookm with id=~a~%" id))
  (let* ((query
          (if skip-title
              (format nil "select url, fk, last_visit_date, dateAdded,
lastModified from moz_bookmarks b, moz_places p where fk=p.id and b.id=~a" id)
              (format nil "select b.title, url, fk, last_visit_date, dateAdded,
lastModified from moz_bookmarks b, moz_places p where fk=p.id and b.id=~a" id)))
         (results (clsql:query query :field-names nil))
         bookmark)
    (when *cl-bookmarks-trace-sql*
      (format t ":debug: ~a~%" query))
    (when *cl-bookmarks-debug*
      (format t ":debug: found bookm with id ~a: ~a~%" id results))
    (setf bookmark
          (if skip-title
          (make-instance 'bookmark
                         :url (first (car results))
                         :v-time (from-frx-time (nth 2 (car results)))
                         :c-time (from-frx-time (nth 3 (car results)))
                         :m-time (from-frx-time (nth 4 (car results))))
          (make-instance 'bookmark :title (first (car results))
                         :url (second (car results))
                         :v-time (from-frx-time (nth 3 (car results)))
                         :c-time (from-frx-time (nth 4 (car results)))
                         :m-time (from-frx-time (nth 5 (car results))))))
    ;; get tags
    (let* ((fk-pos (if skip-title 1 2))
           (query (format nil "select b.title from
moz_bookmarks a, moz_bookmarks b where a.fk=~a and b.parent=4 and a.parent=b.id"
                          (nth fk-pos (car results))))
           (results (clsql:query query :field-names nil)))
      (when *cl-bookmarks-trace-sql*
        (format t ":debug: ~a~%" query))
      (when *cl-bookmarks-debug*
        (format t ":debug: tags if bookm id ~a: ~a~%" id results))
      (dolist (result results)
        (bookm-add-tag bookmark (car result))))
    bookmark))


(defun frx-get-bookm-by-url (url)
  "Return a bookmark object from the firefox database."
  (let* ((query (concatenate 'string "select b.id, p.title, url, b.parent "
                             "from moz_bookmarks b, moz_places p where "
                             "fk=p.id and url='" url "'"))
         (results (clsql:query query :field-names nil))
         tags
         bookmark)
    (when url
      (dolist (result results)
        ;;(format t "result=~a~%" result)
        (let* ((parent-id (nth 3 result))
               (type-of-elt (frx-get-type-of-elt parent-id)))
          (cond
            ((equal type-of-elt :bookmark)
             (setf bookmark (make-instance 'bookmark :title (second result)
                                           :url (third result))))
            ((eql type-of-elt :tag)
             (if (null tags)
                 (setf tags (list (frx-get-title-of-elt parent-id)))
                 (nconc tags (list (frx-get-title-of-elt parent-id)))))))))
    (when (and bookmark tags)
      (setf (tags bookmark) tags))
    bookmark))


(defun frx-get-bookm-with-parent (parent-id &optional skip-title)
  "Return a list of ids of the bookmarks that have the parent parent-id"
  (let* ((query (format nil "select id from moz_bookmarks where parent=~a"
                        parent-id))
         (results (clsql:query query :field-names nil)))
    (when *cl-bookmarks-debug*
      (format t ":debug: children of ~a: ~a~%" parent-id results))
    (mapcar (lambda (a) (frx-get-bookm-by-id (car a) skip-title)) results)))


(defun frx-get-bookm-by-tags (tags &optional skip-title)
  "Return a list of bookmarks that have at least one of the tags in the tags
list."
  (when (null tags)
    (return-from frx-get-bookm-by-tags nil))
  (let* ((query (concatenate 'string "select id from moz_bookmarks where "
                             " parent=4 and "
                             (format nil "( ~{title = '~a' ~^ or ~} )" tags)))
         (results (clsql:query query :field-names nil))
         big-list)
    (loop for tag-id in results do
         (setf big-list (append big-list
                                  (frx-get-bookm-with-parent (car tag-id) skip-title))))
    big-list))


(defun frx-get-type-of-elt (id)
  "Get the type of element specified by id (:bookmark, :tag or nil)"
  (let* ((query (format nil "select parent from moz_bookmarks where id=~a" id))
         (parent-id (car (car (clsql:query query :field-names nil)))))
    (cond
      ((null parent-id) nil)
      ((= parent-id 4) :tag)
      (t :bookmark))))


(defun frx-get-title-of-elt (id)
  "Get the title of the element specified by id (in moz_bookmarks table)."
  (let* ((query (format nil "select title from moz_bookmarks where id=~a" id))
         (title (car (car (clsql:query query :field-names nil)))))
    title))


(defun frx-url-exist-p (url)
  "Return true if the specified url exists in the moz_places table (even if it
  is not a bookmark!)"
  (let* ((query (format nil "select count(*) from moz_places where url='~a'"
                        url))
         (no (car (car (clsql:query query :field-names nil)))))
    (plusp no)))


(defun frx-get-folder-props (name)
  "Return folder id & no. of elements in that folder (as multiple values) or nil
if no such folder exists."
  (let* ((query (format nil "select id from moz_bookmarks where type=2 and
parent!=4 and title='~a'" name))
        (folder-id (car (car (clsql:query query :field-names nil)))))
    (if folder-id
        (let* ((query (format nil "select max(position) from moz_bookmarks
  where parent=~a" folder-id))
               (size (car (car (clsql:query query :field-names nil)))))
          (values-list (list folder-id size)))
        (values-list (list nil nil)))))


(defun frx-add-bookm-tags (bookm place-id)
  "Add tags of the bookm object into the database"
  (dolist (tag (tags bookm))
    (let* ((tag-id (frx-get-tag-id tag :if-not-exist :add)))
      (clsql:insert-records :into "moz_bookmarks"
                            :attributes '(type fk parent position
                                          dateAdded lastModified)
                            :values (list 1 place-id tag-id 0
                                          (frx-time :time (c-time bookm))
                                          (frx-time :time (m-time bookm)))))))


(defun frx-add-bookm (bookm &optional (folder-name "Unsorted Bookmarks"))
  "Add a bookmark into the firefox sqlite file under folder with name
folder-name. If folder-name does not exist the function throws an error."
  (multiple-value-bind (parent-id parent-size)
      (frx-get-folder-props folder-name)
    (when (null parent-id)
      (error (concatenate 'string "No such folder: " folder-name)))
    (when (frx-url-exist-p (url bookm))
      (when *cl-bookmarks-debug*
        (format t ":debug: url '~a' already exists in the db~%" (url bookm)))
      (return-from frx-add-bookm nil))
    (clsql:insert-records :into "moz_places"
                          :attributes '(url title frecency last_visit_date)
                          :values (list (url bookm) (title bookm)
                                        0 (frx-time :time (v-time bookm))))
    (let* ((query "select last_insert_rowid() from moz_places")
           (place-id (car (car (clsql:query query :field-names nil)))))
      (clsql:insert-records :into "moz_bookmarks"
                            :attributes '(type fk parent position title
                                          dateAdded lastModified)
                            :values (list 1 place-id parent-id (1+ parent-size)
                                          (title bookm)
                                          (frx-time :time (c-time bookm)) 
                                          (frx-time :time (m-time bookm))))
      (frx-add-bookm-tags bookm place-id))
    t))


(defun frx-time (&key (time 0 time-p) (year 1900 year-p) (month 1 month-p)
                 (day 1 day-p) (hour 0 hour-p) (min 0 min-p) (sec 1 sec-p))
  "Convert date/time to firefox time. If no parameter is supplied, the current
time is returned. If 'time' is supplied, it is converted from lisp time
(universal time) to firefox time. "
  (cond
    ((or year-p month-p day-p hour-p min-p sec-p)
     (* (- (encode-universal-time sec min hour day month year) 2208981600)
        1000000))
    (time-p
     (* (- time 2208981600) 1000000))
    (t
     (* (- (get-universal-time) 2208981600) 1000000))))


(defun from-frx-time (frx-time)
  "Convert from firefox time to universal (lisp) time. If frx-time is nil,
return 0."
  (if (null frx-time)
      0
      (unix-to-lisp-time (truncate (/ frx-time 1000000)))))


(defun frx-add-bookmarks-to-file (path bookm-list &key
                                  parent-folder (report t))
  "Add a list of bookmark objects to the sqlite file specified by path"
  (flet ((print-report (total tags-alist)
           (format t "~%Total bookmarks added: ~a~%" total)
           (format t "Tags:~%")
           (setf tags-alist (sort tags-alist #'string-lessp :key #'car))
           (dolist (tag-elt tags-alist)
             (when (plusp (cdr tag-elt))
               (if (string-equal (car tag-elt) "")
                   (format t "  without tag - ")
                   (format t "  ~a - " (car tag-elt)))
               (format t "~a bookmarks.~%" (cdr tag-elt))))))

    (frx-open-file path)
    (let ((report-total 0) (report-tags '(("" . 0))))
      (dolist (bookm bookm-list)
        (if (frx-add-bookm bookm (if parent-folder parent-folder
                                     "Unsorted Bookmarks"))
            (progn
              ;; bookmark added succesfully - update report data
              (incf report-total)
              (when report
                (let ((tag-list (tags bookm)))
                  (dolist (tag tag-list)
                    (let ((found (assoc tag report-tags :test #'string-equal)))
                      (if found
                          (rplacd found (1+ (cdr found)))
                          (setf report-tags (acons tag 1 report-tags))))))))
            ;; some error occured
            (format t "~%ERROR: Bookmark with URL '~a' could not be added!~%"
                    (url bookm)))
        ;; progress info
        (when report
          (when (zerop (mod report-total 25))
            (format t "."))
          (when (zerop (mod report-total 500))
            (terpri))))

      (when report
        (print-report report-total report-tags)))
    (frx-close-file)))


(defun frx-sqlite-to-txt (sqlite-fname txt-fname)
  "Read bookmarks from SQLITE-FILE and print them to TXT-FILE."
  (flet ((read-bookm-from-sqlite (sqlite-fname)
           (let (links)
             (cl-bookmarks:frx-open-file sqlite-fname)
             (unwind-protect (setf links (cl-bookmarks:frx-get-all-bookm))
               (cl-bookmarks:frx-close-file))
             (sort links (lambda (a b) (string< (url a) (url b)))))))
           
    (let ((bookm-list (read-bookm-from-sqlite sqlite-fname)))
      (with-open-file (output txt-fname :direction :output :if-exists :supersede)
        (dolist (bookm bookm-list)
          (format output "~a~%~a~%~a~%c-time ~a ~a m-time ~a ~a~%"
                  (url bookm) (title bookm) (sort (tags bookm) 'string<)
                  (c-time bookm)
                  (subseq (multiple-value-list (decode-universal-time (c-time bookm)))
                          0 6)
                  (m-time bookm)
                  (subseq (multiple-value-list (decode-universal-time (m-time bookm)))
                          0 6)))))))


(defun frx-check-invalid-bookmarks (sqlite-fname)
  "Find bookmarks that appear in json output but cannot be found in 'Bookmark manager'
 (see README for details)."
  (cl-bookmarks:frx-open-file sqlite-fname)
  (let* ((query (format nil "select fk, url from moz_bookmarks b, moz_places p
where fk = p.id"))
         (results (clsql:query query :field-names nil)))
    (dolist (result results)
      (when (not (frx-get-bookm-by-url (second result)))
        (format t "Error for url ~a~%" (second result)))))
  (cl-bookmarks:frx-close-file))


;;; * emacs display settings *
;;; Local Variables:
;;; default-tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
