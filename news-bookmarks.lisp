;;;; Functions for managing firefox bookmarks tagged with 'news' tag (news, news_weekly,
;;;; etc.)

;;;; Usage (with sbcl): sbcl --noinform --load news-bookmarks.lisp --eval "(cgi)"

;;;; Making an executable using buildapp (tested on debian 7):
;;;; buildapp --require asdf --load news-bookmarks.lisp --entry cgi --output news-bookmarks.cgi

;;;; ASDF should be loaded by the lisp compiler (via system init, user init or
;;;; some other method).

;;;; If run as a different user (not the owner of places.sqlite), both the sqlite file and
;;;; its parent directory must be writeable by the other user (or its group).

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:operate 'asdf:load-op :cl-bookmarks))


(let (news-bk-debug)

  (defun news-bk-debug-p ()
    news-bk-debug)

  (defun news-bk-set-debug-on ()
    (setf news-bk-debug t)))


(let (news-bk-dry-run)

  (defun news-bk-dry-run-p ()
    news-bk-dry-run)

  (defun news-bk-set-dry-run ()
    (when (news-bk-debug-p)
      (format t "Dry run~%"))
    (setf news-bk-dry-run t)))


(defun news-load-visited-times (visited-file-path)
  "Load URLs and visited times from VISITED-FILE-PATH. Return these as an alist."
  ;; create file with empty assoc list if file does not exist
  (when (not (probe-file visited-file-path))
    (with-open-file (visited-stream visited-file-path :direction :output)
      (write '(("placeholder" . 0)) :stream visited-stream)))

  ;; open file and read data
  (with-open-file (visited-stream visited-file-path :direction :input)
    (let ((visited-times (read visited-stream)))
      (when (null visited-times)
        (error "Invalid urls-list (bug?)"))
      visited-times)))


(defun news-save-visited-times (urls-list visited-file-path)
  "Save URLs (and their visited times) to VISITED-FILE-PATH."
  (with-open-file (visited-stream visited-file-path
                                  :direction :output
                                  :if-exists :supersede)
    (write urls-list :stream visited-stream)
    t))


(defun news-visited-time (urls-list url)
  "Return visited time for URL or 0 of the URL does not exist in URLS-LIST."
  (let ((visited-link (assoc url urls-list :test 'equalp)))
    (if (null visited-link)
        0
        (cdr visited-link))))


(defun news-update-visited-time (urls-list url)
  "Update visited time for URL (or add URL to list if it does not exist."
  (when (null urls-list)
    (error "Invalid urls-list (bug?)"))
  (let ((visited-link (assoc url urls-list :test 'equalp)))
    (if (null visited-link)
        ;; add the url at the end
        (nconc urls-list (list (cons url (get-universal-time))))
        ;; modify the time
        (setf (cdr visited-link) (get-universal-time)))
    t))


(define-condition invalid-news-bookmark ()
  ((bookm :accessor bookm :initarg :bookm)
   (error-msg :accessor error-msg :initarg :error-msg)))


(defun frx-get-news-link (firefox-bookmarks visited-file)
  "Return the bookmark tagged with 'news' having the highest priority.
Return nil if there are problems with 'news' bookmarks (in this case the second value will
  contain the warning / error message."
  (let (links)
    (cl-bookmarks:frx-open-file firefox-bookmarks)
    (unwind-protect (setf links (cl-bookmarks:frx-get-bookm-by-tags '("news")))
      (cl-bookmarks:frx-close-file))
    (let* ((links-to-visit
            (remove-if (lambda (a) (cl-bookmarks:bookm-has-tag-p a "subscribed"))
                       links))
           (min-priority (* 24 3600 365))
           (visited (news-load-visited-times visited-file))
           min-priority-link priority error-msg links-with-priority)
      (handler-case
       (dolist (link links-to-visit)
         ;; try last-visit time, then m-time and then c-time
         (let ((last-visit (news-visited-time visited (cl-bookmarks:url link))))
           (when (zerop last-visit)
             ;;(format t "~a: last-visit is 0, using m-time (~a)~%"
                     ;;(cl-bookmarks:url link) (cl-bookmarks:m-time link))
             (setf last-visit (cl-bookmarks:m-time link)))
           (when (zerop last-visit)
             ;;(format t "~a: last-visit is 0, using c-time (~a)~%"
                     ;;(cl-bookmarks:url link) (cl-bookmarks:m-time link))
             (setf last-visit (cl-bookmarks:c-time link)))
           (setf priority (+ last-visit
                             (- (get-universal-time))
                             (* 24 3600 ; days to seconds
                                (cond
                                  ((cl-bookmarks:bookm-has-tag-p link "news-weekly")
                                   7)
                                  ((cl-bookmarks:bookm-has-tag-p link "news-twice-a-month")
                                   15)
                                  ((cl-bookmarks:bookm-has-tag-p link "news-monthly")
                                   30)
                                  ((cl-bookmarks:bookm-has-tag-p link "news-bimonthly")
                                   60)
                                  ((cl-bookmarks:bookm-has-tag-p link "news-sometimes")
                                   90)
                                  (t
                                   (setf error-msg "No duration tag for bookmark")
                                   (error 'invalid-news-bookmark
                                          :bookm link
                                          :error-msg error-msg))))))
           ;;(format t "Link: ~a, last-visit=~a, priority=~a~%"
                   ;;(cl-bookmarks:url link) last-visit priority)
           (when (< priority min-priority)
             (setf min-priority priority)
             (setf min-priority-link link))
           (setf links-with-priority (append (list (cons link priority)) links-with-priority))))
       (invalid-news-bookmark (e)
                              (setf min-priority-link (bookm e))
                              (setf min-priority nil)
                              (setf error-msg (error-msg e))))
      (when (news-bk-debug-p)
        (format t "Sorted list: ~%~{~a~%~}"
                (sort links-with-priority #'< :key #'cdr)))
      ;; update visited time if everything ok
      (when min-priority
        (news-update-visited-time visited (cl-bookmarks:url min-priority-link))
        (when (not (news-bk-dry-run-p))
          (news-save-visited-times visited visited-file)))
      (values min-priority-link error-msg))))


(defun print-news-link-as-txt (bookm error-msg)
  "Print a link (text format).
If ERROR-MSG is nil then there was no problem (BOOKMARK must not be nil).
If ERROR-MSG is non-nil and BOOKMARK is nil, there was a generic problem (IO, network).
If ERROR-MSG is non-nil and BOOKMARK is non-nil, there was a problem with the specified
bookmark."
  (if error-msg
      (if bookm
          (format t "Error: ~a: ~a~%" error-msg (cl-bookmarks:url bookm))
        (format t "Error: ~a~%" error-msg))
    (format t "~a~%" (cl-bookmarks:url bookm))))


(defun print-news-link-as-html (bookm error-msg)
  "Print a link (HTML format).
If ERROR-MSG is nil then there was no problem (BOOKMARK must not be nil).
If ERROR-MSG is non-nil and BOOKMARK is nil, there was a generic problem (IO, network).
If ERROR-MSG is non-nil and BOOKMARK is non-nil, there was a problem with the specified
bookmark."
  (format t "<html><head><title>News link</title></head><body>~%")  
  (if error-msg
      (if bookm
          (format t "  <p style=\"text-color: red; font-weight: bold\">Error: ~a. Link:\
 <a href=\"~a\">~a</a></p>~%" error-msg (cl-bookmarks:url bookm) (cl-bookmarks:url bookm))
        (format t "  <p style=\"text-color: red; font-weight: bold\">Error: ~a.</p>~%"
                error-msg))
    (format t "  <a href=\"~a\">~a</a>"
            (cl-bookmarks:url bookm) (cl-bookmarks:url bookm)))
  (format t "~%</body></html>~%"))


(defun find-frx-places (home)
  "Find 'places.sqlite' in the specified HOME dir. Return its full path or nil on
error."
  (let* ((find-params (list (concatenate 'string home "/.mozilla/firefox/")
                            "-name" "Cache" "-prune" "-o"
                            "-name" "bookmarkbackups" "-prune" "-o"
                            "-name" "OfflineCache" "-prune" "-o"
                            "-name" "mozilla-media-cache" "-prune" "-o"
                            "-name" "healthreport" "-prune" "-o"
                            "-name" "extensions" "-prune" "-o"
                            "-name" "indexedDB" "-prune" "-o"
                            "-name" "places.sqlite" "-print"))
         (proc (sb-ext:run-program "find"
                                   find-params
                                   :search t :output :stream :error nil)))
    (when (news-bk-debug-p)
      (format t "PLACES: home: ~a, exit-code: ~a~%" home (process-exit-code proc))
      (format t "find cmd: find ~a~%" find-params))
    ;; we don't check the exit code because find might fail because of different reasons
    ;; (e.g. no permission)
    ;;(when (zerop (process-exit-code proc))
    (do ((line (read-line (process-output proc) nil) (read-line (process-output proc) nil)))
        ((or (not line) (search ".default" line)) line)
      (when (news-bk-debug-p)
        (format t "checking if ~a is in a default profile~%" line)))))


(defun display-news-link (format &optional home exit)
  "FORMAT can be :text or :html.
If HOME is not specified, it is read from environment."
  (when (null home)
    (setf home (sb-ext:posix-getenv "HOME")))
  (let* (bookm problem
         (exit-code 0)
         (frx-places (find-frx-places home))
         (visited-file (concatenate 'string home "/" ".news-bookmarks.s")))
    (if frx-places
        (multiple-value-setq (bookm problem)
          (frx-get-news-link frx-places visited-file))
      (progn
        (setf exit-code 1)
        (setf problem "Error finding places.sqlite")))
    (cond
     ((eq format :text)
      (print-news-link-as-txt bookm problem))
     ((eq format :html)
      (print-news-link-as-html bookm problem))
     (t ;; fallback -> text
      (print-news-link-as-txt bookm problem)))
    (when exit
      (sb-ext:exit :code exit-code))))


(export 'cgi)

(defun print-usage ()
  (format t "Usage: news-bookmarks [ -h ] [ -d ] [ -n ] [ -t ]~%  -d : debug~%  -n : dry run~%  -t : txt output~%"))


(defun cgi (&optional args)
  ;; ARGS - needed for buildapp
  (let ((output-format :html))
    (dolist (arg (cdr args))
      (when (equal arg "-h")
        (print-usage)
        (return-from cgi))
      (when (equal arg "-n")
        ;; don't update the visited time
        (news-bk-set-dry-run))
      (when (equal arg "-d")
        (news-bk-set-debug-on))
      (when (equal arg "-t")
        ;; txt output
        (when (news-bk-debug-p)
          (format t "Output : txt~%"))
        (setf output-format :txt)))

    (setq sb-impl::*default-external-format* :utf-8)
    ;; HTTP headers
    (if (eq output-format :html)
        (format t "Content-Type: text/html~%~%")
        (format t "Content-Type: text/plain~%~%"))
    (display-news-link output-format "/home/me" t)))

;;; * emacs display settings *
;;; Local Variables:
;;; default-tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
