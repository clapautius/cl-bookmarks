all: firefoxb-sqlite-to-txt firefoxb-json-to-txt news-bookmarks.cgi

firefoxb-sqlite-to-txt: firefox.lisp cl-bookmarks.lisp
	buildapp --load ~/.sbclrc --eval '(require :cl-bookmarks)' --load firefox.lisp --eval '(defun main (argv) (cl-bookmarks:main-frx-sqlite-to-txt argv))' --entry main --output firefoxb-sqlite-to-txt

firefoxb-json-to-txt: firefox-json.lisp cl-bookmarks.lisp
	buildapp --load ~/.sbclrc --eval '(require :cl-bookmarks)' --eval '(require :cl-json)' --load firefox-json.lisp --eval '(defun main (argv) (in-package :cl-bookmarks) (cl-bookmarks:main-frx-json-to-txt argv))' --entry main --output firefoxb-json-to-txt

news-bookmarks.cgi: news-bookmarks.lisp cl-bookmarks.lisp
	buildapp --load ~/.sbclrc --eval '(require :cl-bookmarks)' --load news-bookmarks.lisp --entry cgi --output news-bookmarks.cgi
