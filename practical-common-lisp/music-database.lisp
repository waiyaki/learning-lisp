;; Declare a global database. The asterisks are a convention
;; of declaring global variables in Lisp
(defvar *db* nil)

;; Accept a list of four fields and return a plist of those fields
;; that represent a cd record.
(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

;; Add a new record into the global db
(defun add-record (cd)
  (push cd *db*))

;; Dump the db to stdout in a human readable format.
(defun dump-db-v0 ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))

;; Dump the db to stdout (v1). Lets `format` loop over the list itself,
;; without requiring the call to `dolist` macro
(defun dump-db ()
  (format t "~{~{~a:~10t~a~%~}~%~}" *db*))

;; Prompt user for piece of information and read it.
;; Takes in a string prompt, which is the prompt to display.
(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

;; Combine `make-cd` with `prompt-read` to build a function that makes a cd
;; record from the data it gets by prompting for each value in turn
(defun prompt-for-cd-v0 ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (prompt-read "Rating")
   (prompt-read "Ripped [y/n]")))

;; Reasonably robust `prompt-for-cd`.
;; It'll parse an integer out of the input string and use that as the rating
;; or default to 0 if it can't find a number in the input. Use :junk-allowed
;; to allow for non-numerical input without blowing up.
;; It'll also reprompt the user for the rating if they don't enter one of
;; "y, Y, n or N".
(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped? [y/n]: ")))

;; Loop while prompting for CDs and add them to the db until the user is done.
(defun add-cds()
  (loop (add-record (prompt-for-cd))
     (if (not (y-or-n-p "Another? [y/n]: ")) (return))))

;; Save the current state of the db to a filename. Take the filename as an argument.
(defun save-db (filename)
  (with-open-file (out filename
			:direction :output
			:if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

;; Load a CDs database from a file.
(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

;; Get CDs matching an artist with the given name
(defun select-by-artist (artist)
  (remove-if-not #'(lambda (cd) (equal (getf cd :artist) artist)) *db*))

;; Example
(format t "~s~%" "Using `select-by-artist`")
(format t "~s~%" (select-by-artist "Dixie Chicks"))

;; Select the values that match the passed in predicate (selector function)
;; Use to avoid having too many "select-by" functions
(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

;; Example
(format t "~%~s" "Using `select` and an anonymous function")
(print (select #'(lambda (cd) (equal (getf cd :artist) "Taylor Swift"))))

;; artist-selector - Return a function that will return the cd matching the
;; specified artist.
;; Helps you avoid writing raw artist selectors using lambdas every time.
(defun artist-selector (artist)
  #'(lambda (cd) (equal (getf cd :artist) artist)))

;; Example
(format t "~%~%~s" "Using `artist-selector`")
(print (select (artist-selector "Taylor Swift")))
(print (select (artist-selector "Dixie Chicks")))

;; where-v0 - A selector function generator.
;; Takes four keyword parameters corresponding to fileds in the CD records and
;; generates a function that selects any CDs matching the values given.
;; Saves us from having a bunch or *-selector functions where * is the field.
(defun where-v0 (&key title artist rating (ripped nil ripped-p))
  #'(lambda (cd)
      (and
       (if title    (equal (getf cd :title) title) t)
       (if artist   (equal (getf cd :artist) artist) t)
       (if rating   (equal (getf cd :rating) rating) t)
       (if ripped-p (equal (getf cd :ripped) ripped) t))))

;; Examples
(format t "~%~%~s" "Using `select` and `where`")
(print (select (where-v0 :rating 8)))
(print (select (where-v0 :artist "")))
(print (select (where-v0 :artist "Taylor Swift")))
;; print the whole db since the ifs in the and form of the where function will all return t
(format t "~%~%All CDs using `select` and `where`: ~%~s~%" (select (where-v0)))

;; make-comparison-expr - macro that constructs the comparison expression of the form
;; (EQUAL (GETF CD :RATING) 10)
(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))

;; make-comparisons-list - Given a list of fields, make a list of comparison expressions
;; from it.
(defun make-comparisons-list (fields)
  (loop while fields
     collecting (make-comparison-expr (pop fields) (pop fields))))

;; where - Version 2 of where that eliminates redundant if checks.
(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparisons-list clauses))))

;; update - update records matching a `where` clause.
;; Using a passed-in selector, choose the records to update.
;; Using key word arguments, specify the values to change.
(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
	(mapcar
	 #'(lambda (row)
	     (when (funcall selector-fn row)
	       (if title (setf (getf row :title) title))
	       (if artist (setf (getf row :artist) artist))
	       (if rating (setf (getf row :rating) rating))
	       (if ripped-p (setf (getf row :ripped) ripped)))
	     row) *db*)))

;; Example
(format t "~%Updating db records using `update`:")
(print (update (where :title "You Belong With Me") :rating 10))

;; Deleting rows
(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))
