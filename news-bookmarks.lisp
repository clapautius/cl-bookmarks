;;; helper script to get a news link from firefox db
;;; usage (with sbcl): sbcl --noinform --load bookmark-utils.lisp --eval "(print-news-link \"$(find ~/.mozilla/firefox/ -name Cache -prune -o -name places.sqlite -print)\")"
;;; ASDF should be loaded by the lisp compiler (via system init, user init or
;;; some other method).

(eval-when (:compile-toplevel :load-toplevel :execute)
  (with-open-file (*standard-output* "/dev/null" :direction :output
                                     :if-exists :supersede)
    (with-open-file (*error-output* "/dev/null" :direction :output
                                    :if-exists :supersede)
      (asdf:operate 'asdf:load-op :cl-bookmarks))))


(defun frx-get-news-link (&optional (path "places.sqlite"))
  ":fixme: - explain"
  (cl-bookmarks:frx-open-file path)
  (let* ((links-to-visit
          (remove-if (lambda (a) (cl-bookmarks:bookm-has-tag-p a "subscribed"))
                     (cl-bookmarks:frx-get-bookm-by-tags '("news"))))
         (min-priority (* 24 3600 365))
         min-priority-link
         priority)
    (dolist (link links-to-visit)
      (setf priority (+ (cl-bookmarks:v-time link) (- (get-universal-time))
                        (* 24 3600 ; days to seconds
                           (cond
                             ((cl-bookmarks:bookm-has-tag-p link
                                                            "news_weekly")
                              7)
                             ((cl-bookmarks:bookm-has-tag-p link
                                                            "news_monthly")
                              30)
                             ((cl-bookmarks:bookm-has-tag-p link
                                                            "news_bimonthly")
                              60)
                             ((cl-bookmarks:bookm-has-tag-p link
                                                            "news_sometimes")
                              90)
                             (t 365))))) ; :fixme: - print a warning
      (when (< priority min-priority)
        (setf min-priority priority)
        (setf min-priority-link link)))
    (cl-bookmarks:frx-close-file)
    min-priority-link))


(defun print-news-link (path)
  "Print a link and exit (to be used from scripts)."
  ;; :fixme: quit is sbcl-specific?
  (format t "~a~%" (cl-bookmarks:url (frx-get-news-link  path)))
  (quit))


;;; * emacs display settings *
;;; Local Variables:
;;; default-tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
