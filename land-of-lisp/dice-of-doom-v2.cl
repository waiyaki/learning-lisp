(load "lazy.lisp")

(defparameter *num-players* 2)
(defparameter *max-dice* 3)
(defparameter *board-size* 5)
(defparameter *board-hexnum* (* *board-size* *board-size*))


;; Convert a board represented as a list into an array
(defun board-array (lst)
  (make-array *board-hexnum* :initial-contents lst))

;; Start with a randomized board
(defun gen-board ()
  (board-array (loop for n below *board-hexnum*
                  collect (list (random *num-players*)
                                (1+ (random *max-dice*))))))

;; Convert player number to a letter
(defun player-letter (n)
  (code-char (+ 97 n)))

;; Draw an encoded board on the screen
(defun draw-board (board)
  (loop for y below *board-size*
     do (progn (fresh-line)
               (loop repeat (- *board-size* y)
                  do (princ " "))
               (loop for x below *board-size*
                  for hex = (aref board (+ x (* *board-size* y)))
                  do (format t "~a-~a " (player-letter (first hex))
                             (second hex))))))

;; Game rules engine - generate a game tree.
(defun game-tree (board player spare-dice first-move)
  (list player
        board
        (add-passing-move board
                          player
                          spare-dice
                          first-move
                          (attacking-moves board player spare-dice))))

;; Add passing moves to the game tree
(defun add-passing-move (board player spare-dice first-move moves)
  (if first-move
      moves
      (lazy-cons (list nil
                  (game-tree (add-new-dice board player (1- spare-dice))
                             (mod (1+ player) *num-players*)
                             0
                             t))
            moves)))

;; Add possible attacking moves to the game tree
(defun attacking-moves (board cur-player spare-dice)
  (labels ((player (pos)
             (car (aref board pos)))
           (dice (pos)
             (cadr (aref board pos))))
    (lazy-mapcan
     (lambda (src)
       (if (eq (player src) cur-player)
         (lazy-mapcan
          (lambda (dst)
            (if (and (not (eq (player dst) cur-player))
                       (> (dice src) (dice dst)))
                (make-lazy
                 (list (list (list src dst)
                             (game-tree (board-attack board
                                                      cur-player
                                                      src
                                                      dst
                                                      (dice src))
                              cur-player
                              (+ spare-dice (dice dst))
                              nil))))
              (lazy-nil)))
          (make-lazy (neighbours src)))
         (lazy-nil)))
     (make-lazy (loop for n below *board-hexnum* collect n)))))

;; Calculate the neighbouring hexagons to a given hexagon.
(defun neighbours (pos)
  (let ((up (- pos *board-size*))
        (down (+ pos *board-size*)))
    (loop for p in (append (list up down)
                           (unless (zerop (mod pos *board-size*))
                             (list (1- up) (1- pos)))
                           (unless (zerop (mod (1+ pos) *board-size*))
                             (list (1+ pos) (1+ down))))
       when (and (>= p 0) (< p *board-hexnum*))
       collect p)))

;; Board attack
(defun board-attack (board player src dst dice)
  (board-array (loop for pos from 0
                  for hex across board
                  collect (cond ((eq pos src) (list player 1))
                                ((eq pos dst) (list player (1- dice)))
                                (t hex)))))

;; Add reinforcements to the board
;; Find a slot that can accommodate a dice and add one there, according to
;; how many dice the player captured in last turn.
(defun add-new-dice (board player spare-dice)
  (labels ((f (lst n acc)
             (cond ((zerop n) (append (reverse acc) lst))
                   ((null lst) (reverse acc))
                   (t (let ((cur-player (caar lst))
                            (cur-dice (cadar lst)))
                        (if (and (eq cur-player player)
                                 (< cur-dice *max-dice*))
                            (f (cdr lst)
                               (1- n)
                               (cons (list cur-player (1+ cur-dice)) acc))
                            (f (cdr lst) n (cons (car lst) acc))))))))
    (board-array (f (coerce board 'list) spare-dice nil))))

;; Travel down the game tree, allowing two humans to play the game
(defun play-vs-human (tree)
  (print-info tree)
  (if (not (lazy-null (caddr tree)))
      (play-vs-human (handle-human tree))
      (announce-winner (cadr tree))))

;; Describe status of current node in game tree
(defun print-info (tree)
  (fresh-line)
  (format t "current player = ~a" (player-letter (car tree)))
  (draw-board (cadr tree)))

;; Handle input from humans
(defun handle-human (tree)
  (fresh-line)
  (princ "choose your move: ")
  (let ((moves (caddr tree)))
    (labels ((print-moves (moves n)
               (unless (lazy-null moves)
                 (let* ((move (lazy-car moves))
                        (action (car move)))
                   (fresh-line)
                   (format t "~a. " n)
                   (if action
                       (format t "~a -> ~a" (car action) (cadr action))
                       (princ "end turn")))
                 (print-moves (lazy-cdr moves) (1+ n)))))
      (print-moves moves 1))
    (fresh-line)
    (cadr (lazy-nth (1- (read)) moves))))

;; Handle winners
(defun winners (board)
  (let* ((tally (loop for hex across board
                   collect (car hex)))
         (totals (mapcar (lambda (player)
                           (cons player (count player tally)))
                         (remove-duplicates tally)))
         (best (apply #'max (mapcar #'cdr totals))))
    (mapcar #'car
            (remove-if (lambda (x)
                         (not (eq (cdr x) best)))
                       totals))))

(defun announce-winner (board)
  (fresh-line)
  (let ((w (winners board)))
    (if (> (length w) 1)
        (format t "The game is a tie between ~a" (mapcar #'player-letter w))
        (format t "The winner is ~a" (player-letter (car w))))))

(defparameter *ai-level* 4)
(defun handle-computer (tree)
  (let ((ratings (ab-get-ratings-max (limit-tree-depth tree *ai-level*)
                                     (car tree)
                                     most-positive-fixnum
                                     most-negative-fixnum)))
    (cadr (lazy-nth (position (apply #'max ratings) ratings)
               (caddr tree)))))

(defun play-vs-computer (tree)
  (print-info tree)
  (cond ((lazy-null (caddr tree)) (announce-winner (cadr tree)))
        ((zerop (car tree)) (play-vs-computer (handle-human tree)))
        (t (play-vs-computer (handle-computer tree)))))

;; Trim a tree to a certain depth
(defun limit-tree-depth (tree depth)
  (list (car tree)
        (cadr tree)
        (if (zerop depth)
            (lazy-nil)
            (lazy-mapcar (lambda (move)
                           (list (car move)
                                 (limit-tree-depth (cadr move) (1- depth))))
                         (caddr tree)))))

;; Apply heuristics to score board position at leaf
(defun score-board (board player)
  (loop for hex across board
     for pos from 0
     sum (if (eq (car hex) player)
             (if (threatened pos board)
                 1
                 2)
             -1)))

;; Determine if a hex is threatened, that is, it neighbours an enemy
;; hex with more dice than it.
(defun threatened (pos board)
  (let* ((hex (aref board pos))
         (player (car hex))
         (dice (cadr hex)))
    (loop for n in (neighbours pos)
       do (let* ((nhex (aref board n))
                 (nplayer (car nhex))
                 (ndice (cadr nhex)))
            (when (and (not (eq player nplayer)) (> ndice dice))
              (return t))))))

(defun rate-position (tree player)
  (let ((moves (caddr tree)))
    (if (not (lazy-null moves))
        (apply (if (eq (car tree) player)
                   #'max
                   #'min)
               (get-ratings tree player))
        (score-board (cadr tree) player))))

(defun get-ratings (tree player)
  (take-all (lazy-mapcar (lambda (move)
                           (rate-position (cadr move) player))
                         (caddr tree))))

;; Rate position using alpha-beta pruning.
(defun ab-rate-position (tree player upper-limit lower-limit)
  (let ((moves (caddr tree)))
    (if (not (lazy-null moves))
        (if (eq (car tree) player)
            (apply #'max (ab-get-ratings-max tree
                                             player
                                             upper-limit
                                             lower-limit))
            (apply #'min (ab-get-ratings-min tree
                                             player
                                             upper-limit
                                             lower-limit
                                             )))
        (score-board (cadr tree) player))))


;; Use alpha-beta pruning when getting ratings
(defun ab-get-ratings-max (tree player upper-limit lower-limit)
  (labels ((f (moves lower-limit)
             (unless (lazy-null moves)
               (let ((x (ab-rate-position (cadr (lazy-car moves))
                                          player
                                          upper-limit
                                          lower-limit)))
                 (if (>= x upper-limit)
                     (list x)
                     (cons x (f (lazy-cdr moves) (max x lower-limit))))))))
    (f (caddr tree) lower-limit)))

(defun ab-get-ratings-min (tree player upper-limit lower-limit)
  (labels ((f (moves upper-limit)
             (unless (lazy-null moves)
               (let ((x (ab-rate-position (cadr (lazy-car moves))
                                          player
                                          upper-limit
                                          lower-limit)))
                 (if (<= x lower-limit)
                     (list x)
                     (cons x (f (lazy-cdr moves) (min x upper-limit))))))))
    (f (caddr tree) upper-limit)))

;; Memoize neighbours function
;; symbol-function retrieves the function bound to a symbol.
(let ((original-neighbours (symbol-function 'neighbours))
      (previous (make-hash-table)))
  (defun neighbours (pos)
    (or (gethash pos previous)
        (setf (gethash pos previous) (funcall original-neighbours pos)))))

;; Memoize game-tree function
(let ((original-game-tree (symbol-function 'game-tree))
      (previous (make-hash-table :test #'equalp)))
  (defun game-tree (&rest rest)
    (or (gethash rest previous)
        (setf (gethash rest previous) (apply original-game-tree rest)))))

;; Memoize rate-position function
(let ((original-rate-position (symbol-function 'rate-position))
      (previous (make-hash-table)))
  (defun rate-position (tree player)
    (let ((tab (gethash player previous)))
      (unless tab
        (setf tab (setf (gethash player previous) (make-hash-table))))
      (or (gethash tree tab)
          (setf (gethash tree tab)
                (funcall original-rate-position tree player))))))
