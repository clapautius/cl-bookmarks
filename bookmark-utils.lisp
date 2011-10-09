(defun frx-get-news-link (&optional (path "places.sqlite"))
  ":fixme: - explain"
  (flet ((days-to-sec (days)
           (* days 24 3600)))

    (frx-open-file path)
    (let* ((links-to-visit (remove-if (lambda (a)
                                        (bookm-has-tag-p a "subscribed"))
                                      (frx-get-bookm-by-tags '("news"))))
           (min-priority (days-to-sec 365))
           min-priority-link
           priority)
      (dolist (link links-to-visit)
        (setf priority (+ (v-time link) (- (get-universal-time))
                             (days-to-sec
                              (cond
                                ((bookm-has-tag-p link "news_weekly") 7)
                                ((bookm-has-tag-p link "news_monthly") 30)
                                ((bookm-has-tag-p link "news_bimonthly") 60)
                                ((bookm-has-tag-p link "news_sometimes") 90)
                                (t 365))))) ; :fixme: - print a warning
        (when (< priority min-priority)
          (setf min-priority priority)
          (setf min-priority-link link)))
      (frx-close-file)
      (url min-priority-link))))
