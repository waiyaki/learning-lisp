(defparameter *nodes* '((living-room (you are in the living room.
                                      a wizard is snoring loudly on the couch.))
                        (garden (you are in a beautiful garden.
                                there is a well in front of you.))
                        (attic (you are in the attic.
                                there is a giant welding torch in the corner.))))

(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

(defparameter *edges* '((living-room (garden west door)
                         (attic upstairs ladder))
                        (garden (living-room east door))
                        (attic (living-room downstairs ladder))))

(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))


(defparameter *objects* '(whiskey bucket frog chain))

(defparameter *object-locations* '((whiskey living-room)
                                   (bucket living-room)
                                   (chain garden)
                                   (frog garden)))

(defun objects-at (loc objs obj-locs)
  "List objects visible from a given location."
  (labels ((at-loc-p (obj)
             (eq (cadr (assoc obj obj-locs)) loc)))
    (remove-if-not #'at-loc-p objs)))

(defun describe-objects (loc objs obj-locs)
  "Describe objects visible at a given location"
  (labels ((describe-obj (obj)
             `(you see a ,obj on the floor.)))
    (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-locs)))))

(defparameter *location* 'living-room
  "Track the players current location")

(defun look ()
  "Describe everything as observed from the current location"
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))

(defun walk (direction)
  "Take a direction and let players walk there."
  (let ((next (find direction
                    (cdr (assoc *location* *edges*))
                    :key #'cadr)))
    (if next
        (progn (setf *location* (car next))
               (look))
        '(you cannot go that way))))


(defun pickup (object)
  (cond ((member object
                 (objects-at *location* *objects* *object-locations*))
         (push (list object 'body) *object-locations*)
         `(you are now carrying the ,object))
        (t `(you cannot get that.))))


(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

(defun game-repl ()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))

(defun game-read ()
  (let ((cmd (read-from-string
              (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (x)
             (list 'quote x)))
      (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

(defparameter *allowed-commands* '(look walk pickup inventory))

(defun game-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
      (eval sexp)
      '(i do not know that command.)))

(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
          (rest (cdr lst)))
      (cond ((eq item #\space) (cons item (tweak-text rest caps lit)))
            ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
            ((eq item #\") (tweak-text rest caps (not lit)))
            (lit (cons item (tweak-text rest nil lit)))
            ((or caps lit) (cons (char-upcase item) (tweak-text rest nil lit)))
            (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

(defun game-print (lst)
  (princ (coerce (tweak-text (coerce (string-trim "() "
                                                  (prin1-to-string lst))
                                     'list)
                             t
                             nil)
                 'string))
  (fresh-line))

;; Allow players to weld chain to bucket if they have it
(defun have (object)
  (member object (inventory)))

(defparameter *chain-welded* nil)

(defun weld-prev (subject object)
  (if (and (eq *location* 'attic)
           (eq subject 'chain)
           (eq object 'bucket)
           (have 'chain)
           (have 'bucket)
           (not *chain-welded*))
      (progn (setf *chain-welded* t)
             '(the chain is now securely welded to the bucket.))
      '(you cannot weld like that.)))

(pushnew 'weld-prev *allowed-commands*)

;; Let the player dunk the bucket into the well to fill it with water.
(defparameter *bucket-filled* nil)

(defun dunk-prev (subject object)
  (if (and (eq *location* 'garden)
           (eq subject 'bucket)
           (eq object 'well)
           (have 'bucket)
           *chain-welded*)
      (progn (setf *bucket-filled* t)
             '(the bucket is now full of water.))
      '(you cannot dunk like that.)))

(pushnew 'dunk-prev *allowed-commands*)

;; Macro to generate game actions (e.g dunk-prev, weld)
(defmacro game-action (command subj obj place &body body)
  `(progn (defun ,command (subject object)
            (if (and (eq *location* ',place)
                     (eq subject ',subj)
                     (eq object ',obj)
                     (have ',subj))
                ,@body
                '(I cannot ,command like that.)))
          (pushnew ',command *allowed-commands*)))

;; Use macro to define game actions
(setf *chain-welded* nil)

(game-action weld chain bucket attic
  (if (and (have 'bucket) (not *chain-welded*))
      (progn (setf *chain-welded* t)
             '(the chain is now securely welded to the bucket.))
      '(you do not have a bucket.)))

(setf *bucket-filled* nil)

(game-action dunk bucket well garden
  (if *chain-welded*
      (progn (setf *bucket-filled* t)
             '(the bucket is now full of water.))
      '(the water level is too low to reach.)))

(game-action splash bucket wizard living-room
  (cond ((not *bucket-filled*) '(the bucket has nothing in it.))
        ((have 'frog) '(the wizard awakens and sees that you stole his frog.
                        he is so upset he banishes you to the
                        netherworlds- you lose! the end.))
        (t '(the wizard awakens from his slumber and greets you warmly.
             he hands you the magic low-carb donut- you win! the end.))))
