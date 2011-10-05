;;; functions handling firefox bookmarks (requires cl-sql, cl-sql-sqlite3 & co.)

;;; :fixme: - add proper package management
(asdf:oos 'asdf:load-op :clsql-sqlite3)


;;; bookmark class - browser independent
;;; to be moved in a separate file :todo:
(defclass bookmark ()
  ((url :accessor url
		:initarg :url
		:initform (error "Must specify an URL"))

   (title :accessor title
		  :initarg :title
		  :initform (error "Must specify a title"))

   ;; creation time
   (c-time :accessor c-time
		   :initarg :c-time
		   :initform (get-universal-time))

   ;; modification time
   (m-time :accessor  m-time
		   :initarg :m-time
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
(defun frx-open-file (&key (path "places.sqlite"))
  "Open an sqlite connection to the specified file."
  (clsql:connect (list path) :database-type :sqlite3 :if-exists :old))


(defun frx-close-file ()
  "Close the sqlite connection"
  (clsql:disconnect))


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

(defun frx-add-bookm (bookmark)
  "Add a bookmark into the firefox sqlite file"
  (clsql:insert-records :into "moz_places"
						:attributes '(url title frecency)
						:values (list (url bookmark) (title bookmark) 0))
  (let* ((query-str (format nil "select last_insert_rowid() from moz_places"))
		 (place-id (car (car (clsql:query query-str :field-names nil)))))
	(clsql:insert-records :into "moz_bookmarks"
						  :attributes '(type fk parent title)
						  :values (list 1 place-id 5 (title bookmark)))))


(defun time-to-frx-time (&key (year 1900 year-p) (month 1 month-p) (day 1 day-p)
						(hour 0 hour-p) (min 0 min-p) (sec 1 sec-p))
  "Convert date/time to firefox time. If no parameter is supplied, the current
						time is returned."
  (if (or year-p month-p day-p hour-p min-p sec-p)
	  (* (- (encode-universal-time sec min hour day month year) 2208981600)
		 1000000)
	  (* (- (get-universal-time) 2208981600) 1000000)))
