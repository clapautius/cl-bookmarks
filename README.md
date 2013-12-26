## Installation & usage

### Prerequisites

The application is written in Common Lisp, so a working common lisp environment is needed.

Required CLisp packages:
* cl-sql & cl-sqlite3 for editing firefox bookmarks.
* [closure-html](http://common-lisp.net/project/closure/closure-html/index.html) for delicious import.
 * closure-html needs [closure-common](http://www.cliki.net/closure-common), flexi-streams and cl-babel.

### Compile

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

## Development

### Firefox sqlite db. internals

* '''PRTime''' : This type is a 64-bit integer representing the number of microseconds since the NSPR epoch, midnight (00:00:00) 1 January 1970 Coordinated Universal Time (UTC). A time after the epoch has a positive value, and a time before the epoch has a negative value. (https://developer.mozilla.org/en/PRTime)

* A bookmark folder (in moz_bookmarks table) has type = 2 & parent != 4.