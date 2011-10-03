;;; functions handling firefox bookmarks (requires cl-sql-odbc & co.)

(require 'clsql)


;;; bookmark class - browser independent
;;; to be moved in a separate file :todo:
(defclass bookmark ()
  ((url :accessor url
		:initarg :url
		:initform (error "Must specify an URL"))
   (title :accessor title
		  :initarg :title
		  :initform (error "Must specify a title"))
   (tags :accessor tags
		 :initarg :tags
		 :initform nil))
  (:documentation "Class containing basic elements for a bookmark"))


(defmethod print-object ((bookmark bookmark) stream)
  (format stream "<bookm: title=~a, url=~a, tags: ~a>"
		  (title bookmark) (url bookmark) (tags bookmark)))



(defun bookm-open-file (&key (path "places.sqlite"))
  "Open an sqlite connection to the specified file."
  (clsql:connect (list path) :database-type :sqlite3 :if-exists :old))


(defun bookm-close-file ()
  "Close the sqlite connection"
  (clsql:disconnect))


(defun bookm-get-by-url (url)
  "Return a bookmark object from the firefox database."
  (let* ((query-str (concatenate 'string "select b.id, b.title, url, b.parent "
								 "from moz_bookmarks b, moz_places p where "
								 "fk=p.id and url='" url "'"))
		 (results (clsql:query query-str :field-names nil))
		 tags
		 bookmark)
	(when url
	  (dolist (result results)
		;;(format t "result=~a~%" result)
		(let* ((parent-id (nth 3 result))
			   (type-of-elt (bookm-get-type-of-elt parent-id)))
		  (cond
			((equal type-of-elt :bookmark)
			 (setf bookmark (make-instance 'bookmark :title (second result)
										   :url (third result)))
			 ;;(format t "new obj: ~a~%" bookmark))
			((eq type-of-elt :tag)
			 (if (null tags)
				 (setf tags (list (bookm-get-title-of-elt parent-id)))
				 (nconc tags (list (bookm-get-title-of-elt parent-id)))))))))
	(when (and bookmark tags)
	  (setf (tags bookmark) tags))
	bookmark))
		

(defun bookm-get-type-of-elt (id)
  "Get the type of element specified by id (:bookmark, :tag or nil)"
  (let* ((query-str (format nil "select parent from moz_bookmarks where id=~a"
							id))
		 (parent-id (car (car (clsql:query query-str :field-names nil)))))
	(cond
	  ((null parent-id) nil)
	  ((= parent-id 4) :tag)
	  (t :bookmark))))


(defun bookm-get-title-of-elt (id)
  "Get the title of the element specified by id (in moz_bookmarks table)."
  (let* ((query-str (format nil "select title from moz_bookmarks where id=~a"
							id))
		 (title (car (car (clsql:query query-str :field-names nil)))))
	title))
