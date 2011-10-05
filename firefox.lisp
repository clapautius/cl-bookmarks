;;; functions handling firefox bookmarks (requires cl-sql, cl-sql-sqlite3 & co.)

;;; :fixme: - add proper package management
(eval-when (:compile-toplevel)
  (asdf:oos 'asdf:load-op :clsql-sqlite3))


;;; bookmark class - browser independent
;;; to be moved in a separate file :todo:
(defclass bookmark ()
  ((url :accessor url
        :initarg :url
        :initform (error "Must specify an URL"))

   (title :accessor title
          :initarg :title
          :initform (error "Must specify a title"))

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
  (format stream "<bookm: title=~a, url=~a, tags: ~a>"
          (title bookmark) (url bookmark) (tags bookmark)))


;;; firefox functions
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
    "Add tag if it does not exist. Return the tag id or nil if the tag does not
exist and cannot be added. if-not-exist may be :add or :skip (default)."
    (let ((tag (assoc tag-name frx-tags :test #'equal)))
      (if (null tag)
          (if (eql if-not-exist :add)
              (frx-add-tag tag-name)
              nil)
          (cdr tag))))

  ) ; end frx-tags closure


(defun frx-get-bookm-by-url (url)
  "Return a bookmark object from the firefox database."
  (let* ((query (concatenate 'string "select b.id, b.title, url, b.parent "
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
      (frx-add-bookm-tags bookm place-id))))


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
