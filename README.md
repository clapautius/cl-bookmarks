## Installation & usage

### Prerequisites

The application is written in Common Lisp, so a working common lisp environment is needed.

Required CLisp packages:
* cl-sql & cl-sqlite3 for editing firefox bookmarks.
* [closure-html](http://common-lisp.net/project/closure/closure-html/index.html) for delicious import.
 * closure-html needs [closure-common](http://www.cliki.net/closure-common), flexi-streams and cl-babel.

### Compile / generate binary

#### Required packages / tools

    # apt-get install cl-sql cl-sql-sqlite3
    # apt-get install buildapp

#### Export firefox bookmarks from places.sqlite to txt

    $ cd <PRJ-DIR>
    $ ln -s $(pwd)/cl-bookmarks.asd ~/.local/share/common-lisp/source/
    $ buildapp --load ~/.sbclrc --eval '(require :cl-bookmarks)' \
     --load firefox.lisp --eval '(defun main (argv) (cl-bookmarks:main-frx-sqlite-to-txt argv))' \
     --entry main --output firefoxb-sqlite-to-txt

#### Export firefox bookmarks from json to txt

Requires cl-json (no .deb package available for debian/ubuntu).

    $ cd <PRJ-DIR>
    $ ln -s $(pwd)/cl-bookmarks.asd ~/.local/share/common-lisp/source/
    $ buildapp --load ~/.sbclrc --eval '(require :cl-bookmarks)' \
     --eval '(require :cl-json)' \
     --load firefox-json.lisp --eval '(defun main (argv) (in-package :cl-bookmarks) (cl-bookmarks:main-frx-json-to-txt argv))' \
     --entry main --output firefoxb-json-to-txt

#### Compile in slime

In SLIME / lisp compiler: <code>(asdf:oos 'asdf:load-op 'cl-bookmarks-conv)</code> or
<code>(require 'cl-bookmarks-conv)</code>.

### Usage

#### Convert delicious bookmarks to firefox 4+ (including tags)

* Export delicious bookmarks to html (check '' 'include my tags' '' option).
* '''Backup the current firefox bookmarks (''Bookmarks / Show all bookmarks / Import and backup / Backup'') !'''
* Clear firefox history to avoid the risk of collisions with the existing URLs from the history (the app. does not add a bookmark if an identical URL is found in the database, either as a bookmark or as a history element).
* Close firefox.
* After load and compile, run: <code>(cl-bookmarks-conv:delicious-to-firefox <delicious-html-file> <places.sqlite-firefox-db> [ <firefox-bookmark-folder> ])</code>
 * ''firefox-bookmark-folder'' is optional, by default all the bookmark will be added under ''Unsorted Bookmarks'' folder.
 * ''firefox-bookmark-folder'' must exist if specified.
* Read the report and check the results in firefox.
* Restore the backup and repeat if something went wrong.

#### Build news-bookmarks (as a cgi app)

buildapp --load ~/.sbclrc --load news-bookmarks.lisp --entry cgi --output news-bookmarks.cgi

## Development

### Firefox sqlite db. internals

* '''PRTime''' : This type is a 64-bit integer representing the number of microseconds since the NSPR epoch, midnight (00:00:00) 1 January 1970 Coordinated Universal Time (UTC). A time after the epoch has a positive value, and a time before the epoch has a negative value. (https://developer.mozilla.org/en/PRTime)

* A bookmark folder (in moz_bookmarks table) has type = 2 & parent != 4.

* moz_bookmarks.type
 = 2 for bookmark folder (or tag?)
 = 1 for bookmark
