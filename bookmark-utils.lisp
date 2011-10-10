#!/usr/bin/sbcl --script

(defun command-line-args ()
  (or 
   #+SBCL *posix-argv*  
   #+LISPWORKS system:*line-arguments-list*
   #+CMU extensions:*command-line-words*
   nil))


(when (> (length (command-line-args)) 1)
  (with-open-file (*standard-output* "/dev/null" :direction :output
                                     :if-exists :supersede)
    (require 'cl-bookmarks)))


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


(when (> (length (command-line-args)) 1)
  ;;(format t ":debug: cmd. line: ~a~%" (command-line-args))
  (format t "~a~%" (cl-bookmarks:url (frx-get-news-link 
                                      (second (command-line-args))))))


;;; * emacs display settings *
;;; Local Variables:
;;; default-tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
