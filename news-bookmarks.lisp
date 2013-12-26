;;;; Functions for managing firefox bookmarks tagged with 'news' tag (news, news_weekly,
;;;; etc.)

;;;; usage (with sbcl): sbcl --noinform --load news-bookmarks.lisp --eval "(cgi)"

;;;; ASDF should be loaded by the lisp compiler (via system init, user init or
;;;; some other method).

(eval-when (:compile-toplevel :load-toplevel :execute)
  (with-open-file (*standard-output* "/dev/null" :direction :output
                                     :if-exists :supersede)
    (with-open-file (*error-output* "/dev/null" :direction :output
                                    :if-exists :supersede)
      (asdf:operate 'asdf:load-op :cl-bookmarks))))


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
  (cl-bookmarks:frx-open-file firefox-bookmarks)
  (let* ((links-to-visit
          (remove-if (lambda (a) (cl-bookmarks:bookm-has-tag-p a "subscribed"))
                     (cl-bookmarks:frx-get-bookm-by-tags '("news"))))
         (min-priority (* 24 3600 365))
         (visited (news-load-visited-times visited-file))
         min-priority-link priority error-msg)
    (handler-case 
        (dolist (link links-to-visit)
          (setf priority (+ (news-visited-time visited (cl-bookmarks:url link))
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
          (when (< priority min-priority)
            (setf min-priority priority)
            (setf min-priority-link link)))
      (invalid-news-bookmark (e)
        (setf min-priority-link (bookm e))
        (setf min-priority nil)
        (setf error-msg (error-msg e))))
    (cl-bookmarks:frx-close-file)
    ;; update visited time if everything ok
    (when min-priority
      (news-update-visited-time visited (cl-bookmarks:url min-priority-link))
      (news-save-visited-times visited visited-file))
    (values min-priority-link error-msg)))


(defun print-news-link-as-txt (&optional (firefox-bookmarks "places.sqlite")
                                 (visited-file ".news-bookmarks.s"))
  "Print a link."
  (multiple-value-bind (bookm problem) (frx-get-news-link firefox-bookmarks visited-file)
    (if problem
        (format t "~a: ~a~%" problem (cl-bookmarks:url bookm))
        (format t "~a~%" (cl-bookmarks:url bookm)))))


(defun print-news-link-as-html (&optional (firefox-bookmarks "places.sqlite")
                                 (visited-file ".news-bookmarks.s"))
  "Print a link (HTML format)."
  (multiple-value-bind (bookm problem) (frx-get-news-link firefox-bookmarks visited-file)
    (format t "<html><head><title>News link</title></head><body>~%")
    (let ((link (cl-bookmarks:url bookm)))
      (if problem
          (format t "  <p style=\"text-color: red; font-weight: bold\">Error: ~a: \
<a href=\"~a\">~a</a></p>~%" problem link link)
        (format t "  <a href=\"~a\">~a</a>" link link)))
    (format t "~%</body></html>~%")))


(defun cgi-display-news-as-html (&optional home (exit t))
  (when (null home)
    (setf home (sb-ext:posix-getenv "HOME")))
  (let* ((exit-code 0)
         (proc (sb-ext:run-program "find"
                                   (list (concatenate 'string home "/.mozilla/firefox/")
                                         "-name" "Cache" "-prune" "-o" "-name"
                                         "places.sqlite" "-print")
                                   :search t :output :stream)))
    (format t "Content-Type: text/html~%~%")
    (if (zerop (process-exit-code proc))
        (let ((firefox-places (read-line (process-output proc))))
          (print-news-link-as-html firefox-places
                                   (concatenate 'string home "/" ".news-bookmarks.s")))
      (progn
        (setf exit-code 1)
        (format t "<p style=\"text-color: red;\"Error (local error)</p>")))
    (when exit
      (sb-ext:exit :code exit-code))))


(defun cgi ()
  (cgi-display-news-as-html "/home/me"))

;;; * emacs display settings *
;;; Local Variables:
;;; default-tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
