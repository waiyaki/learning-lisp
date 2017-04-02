(in-package :com.gigamonkeys.pathnames)

;; Test whether a given component of a pathname is present.
(defun component-present-p (value)
  (and value (not (eql value :unspecific))))

;; Test whether a pathname is already in directory form
(defun directory-pathname-p (p)
  (and
   (not (component-present-p (pathname-name p)))
   (not (component-present-p (pathname-type p)))
   p))

;; Convert any pathname to directory form
(defun pathname-as-directory (name)
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (not (directory-pathname-p name))
        (make-pathname
         :directory (append (or (pathname-directory pathname) (list :relative))
                            (list (file-namestring pathname)))
         :name nil
         :type nil
         :defaults pathname)
        pathname)))

;; Return a proper wildcard for the given implementation using read-time
;; conditionalization to make a pathname with a :wild type in all
;; implementations except CLISP and nil in CLISP.
(defun directory-wildcard (dirname)
  (make-pathname
   :name :wild
   :type #-clisp :wild #+clisp nil
   :defaults (pathname-as-directory dirname)))

(defun list-directory (dirname)
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory names."))
  (let ((wildcard (directory-wildcard dirname)))

    #+(or sbcl cmu lispworks)
    (directory wildcard)

    #+ openmcl (directory wildcard :directories t)

    #+allegro
    (directory wildcard :directories-are-files nil)

    #+clisp
    (nconc
     (directory wildcard)
     (directory (clisp-subdirectories-wildcard wildcard)))

    #- (or sbcl cmu lispworks openmcl allegro clisp)
    (error "list-directory not implemented.")))


;; Accept a pathname and return an equivalent pathname if the file exists.
;; Return NIL if it doesn't.
;; Smooth over cross-platform differences.
(defun file-exists-p (pathname)
  #+ (or sbcl lispworks openmcl)
  (probe-file pathname)

  #+ (or allegro cmu)
  (or probe-file (pathname-as-directory pathname))

  #+clisp
  (or (ignore-errors
        (probe-file (pathname-as-file pathname)))
      (ignore-errors
        (let ((directory-form (pathname-as-directory pathname)))
          (when (ext:probe-directory directory-form)
            directory-form))))

  #- (or sbcl cmu lispworks openmcl allegro clisp)
  (error "file-exists-p not implemented."))


;; Return a pathname that's the file form equivalent of the argument.
(defun pathname-as-file (name)
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (directory-pathname-p name)
        (let* ((directory (pathname-directory pathname))
               (name-and-type (pathname (first (last directory)))))
          (make-pathname
           :directory (butlast directory)
           :name (pathname-name name-and-type)
           :type (pathname-type name-and-type)
           :defaults pathname))
        pathname)))

;; Walk a directory, recursively calling a function on the directories (if :directories and :test)
;; and files (if :test) in that directory.
(defun walk-directory (dirname fn &key directories (test (constantly t)))
  (labels
      ((walk (name)
         (cond
           ((directory-pathname-p name)
            (when (and directories (funcall test name))
              (funcall fn name))
            (dolist (x (list-directory name)) (walk x)))
           ((funcall test name) (funcall fn name)))))
    (walk (pathname-as-directory dirname))))
