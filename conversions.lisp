;;; various helper functions for conversions
(in-package :cl-bookmarks-conv)


(defun delicious-to-firefox (delicious-file firefox-db-path
                             &optional firefox-folder (report t))
  "Convert bookmarks from delicious html file to firefox. firefox-folder
  specifies the parent folder for the added bookmarks."
  (frx-add-bookmarks-to-file firefox-db-path
                             (dlc-parse-file delicious-file)
                             :parent-folder firefox-folder
                             :report report))


;;; * emacs display settings *
;;; Local Variables:
;;; default-tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
