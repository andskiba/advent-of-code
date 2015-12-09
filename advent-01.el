;;;; advent-01 --- Advent calendar code

;;;; Commentary:

;; Solutions to puzzles from http://adventofcode.com

;;;; Code:

;;; Day 1:

(require 'seq)
(require 'cl-lib)

(defun find-floor (directions)
  "Find floor from DIRECTIONS."
  (let ((f 0))
    (dolist (d (string-to-list directions))
      (setq f (if (eq d 40) (+ f 1) (- f 1))))
    f))

(defun find-floor-2 (directions)
  "Functional implementation of find-floor.

Takes a string of DIRECTIONS"
  (let ((ds (string-to-list directions)))
    (apply #'+ (mapcar (lambda (d) (if (eq d 40) 1 -1)) ds))))

(defun entering-basement (directions)
  "At which direction will Santa enter the basement?

Takes a string of DIRECTIONS"
  (let ((ds (string-to-list directions))
        (dir (lambda (c) (if (eq c 40) 1 -1))))
    (+ 1
       (caar
        (seq-reverse
         (seq-take-while
          (lambda (x)
            (>= (cdr x) 0))
          (cl-mapcar #'cons
                     (number-sequence 0 (length ds))
                     (seq-reverse
                      (seq-reduce (lambda (xs x)
                                    (cons (+ (funcall dir x) (car xs)) xs))
                                  ds
                                  (list 0))))))))))

;;; Day 2 - wrapping paper

(defun dim-list (dimensions)
  "Split input DIMENSIONS string into a list of lists."
  (mapcar (lambda (dim-str)
            (cl-destructuring-bind (w h l) (split-string dim-str "x")
              (list (string-to-number w)
                    (string-to-number h)
                    (string-to-number l))))
          (split-string dimensions "\n")))

(defun paper-for-box (l w h)
  "Find paper needed for a box of dimensions L W H."
  (let* ((sides (sort (list (* l w) (* w h) (* h l)) #'<))
         (smallest (car sides)))
    (+ (apply #'+ (mapcar (lambda (x) (* 2 x)) sides)) smallest)))

(defun paper (dimensions)
  "Total amount of wrapping paper for DIMENSIONS."
  (apply #'+
         (mapcar (lambda (dimension)
                   (apply 'paper-for-box dimension))
                 (dim-list dimensions))))

(defun ribbon-for-box (l w h)
  "Calculate ribbon needed for box of dimensions L W H."
  (let ((smallest-sides (seq-take (sort (list l w h) #'<) 2))
        (volume (* l w h)))
    (+ (apply #'+ (mapcar (lambda (x) (* 2 x)) smallest-sides))
       volume)))

(defun ribbon (dims)
  "Total length of ribbon required for presents of DIMS."
  (apply #'+
         (mapcar (lambda (dim)
                   (apply #'ribbon-for-box dim))
                 (dim-list dims))))

;;; day 3 - deliveries

(defun add-pos (pos1 pos2)
  "Add two POS1 and POS2 to each other."
  (cl-destructuring-bind ((x1 . y1) (x2 . y2)) (list pos1 pos2)
    (cons (+ x1 x2) (+ y1 y2))))

(defun dir-to-move (dir)
  "Convert direction string DIR to a move."
  (let ((pos-map '((">" . (1 . 0))
                   ("<" . (-1 . 0))
                   ("^" . (0 . 1))
                   ("v" . (0 . -1)))))
    (cdr (assoc dir pos-map))))

(defun dir-list (directions)
  "Convert DIRECTIONS string to list of directions."
  (cl-remove-if (lambda (x) (eq (length x) 0))
             (split-string directions "")))

(defun house-count (directions)
  "Calculate number of visited houses based on DIRECTIONS."
  (let ((dir-list (dir-list directions)))
    (length (visited-houses dir-list))))

(defun visited-houses (dir-list)
  "Get list of visited houses for list of directions DIR-LIST."
  (delete-dups
   (seq-reduce (lambda (visited dir)
                 (cons (add-pos (car visited) (dir-to-move dir))
                       visited))
               dir-list
               '((0 . 0)))))

(defun remove-indexes (dirs)
  "Remove indexes from DIRS."
  (mapcar #'car dirs))

(defun robo-house-count (directions)
  "Number of visited houses for Santa and Robo-Santa based on DIRECTIONS."
  (let* ((indexed-dirs (cl-mapcar #'cons
                                  (dir-list directions)
                                  (number-sequence 0 (length directions))))
         (santa-dirs (cl-remove-if (lambda (x) (cl-oddp (cdr x))) indexed-dirs))
         (robo-dirs (cl-remove-if (lambda (x) (cl-evenp (cdr x))) indexed-dirs))
         (santa-houses (visited-houses (remove-indexes santa-dirs)))
         (robo-houses (visited-houses (remove-indexes robo-dirs))))
    (length (delete-dups (cl-concatenate 'list santa-houses robo-houses)))))

;;; day 4 - mining AdventCoins

(defun has-leading-zeros (hash zero-count)
  "Test if HASH has at least ZERO-COUNT leading 0s."
  (>= (length (seq-take-while (lambda (x) (= x ?0)) hash)) zero-count))

(defun find-lowest-num (key zero-count)
  "Find lowest number that for KEY gives hash with ZERO-COUNT leading 0s."
  (let ((num 0)
        (hash ""))
    (while (not (has-leading-zeros hash zero-count))
      (setq hash (secure-hash 'md5 (concat key (number-to-string num))))
      (setq num (+ 1 num)))
    (- num 1)))

;;; day 5 - naughty strings

(defun nicep (word)
  "Check if given WORD is nice."
  (and (has-vowels-p word 3)
       (has-double-p word)
       (not (has-naughty-double-p word))))

(defun has-naughty-double-p (word)
  "Check if WORD has any naughty double letter sequences."
  (> (length (cl-remove-if-not #'naughty-double-p (letter-pairs word)))
     0))

(defun naughty-double-p (p)
  "Check if a letter pair P is naughty."
  (let ((naughties '((?a . ?b)
                     (?c . ?d)
                     (?p . ?q)
                     (?x . ?y))))
    (seq-find (lambda (n) (equal n p)) naughties)))

(defun has-double-p (word)
  "Check if WORD has double letters."
  (> (length (cl-remove-if-not (lambda (p) (eq (car p) (cdr p)))
                               (letter-pairs word)))
     0))

(defun letter-pairs (word)
  "Create a list of all letters from WORD and its consequent."
  (seq-mapn #'cons word (seq-drop word 1)))

(defun has-vowels-p (word count)
  "Check if the WORD has at least COUNT vowels."
  (>= (length (cl-remove-if-not #'vowelp word)) count))

(defun vowelp (char)
  "Check if CHAR is a vowel."
  (let ((vowels "aeiou"))
    (seq-find (lambda (c) (char-equal c char)) vowels)))

(defun split-words (input)
  "Split INPUT string into a list of words."
  (split-string input "\n"))

(defun letter-triples (word)
  "Create list of 3-letter sub-strings of WORD."
  (seq-mapn #'string word (seq-drop word 1) (seq-drop word 2)))

(defun has-triples-with-matching-ends-p (word)
  "Check if WORD has triple with first and last character matching."
  (> (length (cl-remove-if-not (lambda (triple) (eq (aref triple 0)
                                                    (aref triple 2)))
                             (letter-triples word)))
     0))

(defun has-double-doubles-p (word)
  "Check if WORD has two doubles that don't overlap."
  (let ((count 0))
    (maphash (lambda (key val)
               (when (and (>= (length val) 2)
                          (> (- (car val) (nth (- (length val) 1) val)) 1))
                 (cl-incf count)))
             (group-doubles (index-seq (letter-doubles word))))
    (> count 0)))



(defun letter-doubles (word)
  "Two letter sub-string of WORD."
  (seq-mapn #'string word (seq-drop word 1)))

(defun index-seq (sequence)
  "Index a SEQUENCE."
  (seq-mapn #'cons sequence (number-sequence 0 (length sequence))))

(defun group-doubles (doubles)
  "Group DOUBLES in ACC."
  (cl-labels ((rec (doubles acc)
                   (if (null doubles)
                       acc
                     (let* ((double (car doubles))
                            (key (car double))
                            (index (cdr double))
                            (value (gethash key acc)))
                       (if (not (null value))
                           (puthash key (cons index value) acc)
                         (puthash key (list index) acc))
                       (rec (cdr doubles) acc)))))
    (rec doubles (make-hash-table :test #'equal))))

(defun nicerp (word)
  "Check if the WORD is even nicer than nice."
  (and (has-double-doubles-p word)
       (has-triples-with-matching-ends-p word)))

(defun nice-count (words predicate)
  "Count the number of words in WORDS that match PREDICATE."
  (length (cl-remove-if-not predicate (split-words words))))

;; part 1 - (nice-count "words" #'nicep)
;; part 2 - (nice-count "words" #'nicerp)

;;;; Day 6 - light grid

(defun light-count (directions)
  "Part 1 - Count lights that are turned on after following DIRECTIONS."
  (let ((lights (make-vector (* 1000 1000) 0))
        (command-list (commands (split-words directions) #'op-to-fun)))
    (dolist (command command-list)
      (apply #'process-rect (cons lights command)))
    (length (cl-remove-if (lambda (x) (= x 0)) lights))))

(defun total-brightness (directions)
  "Part 2 - Calculate total brightness of lights after following DIRECTIONS."
  (let ((lights (make-vector (* 1000 1000) 0))
        (command-list (commands (split-words directions) #'brightness-ops)))
    (dolist (command command-list)
      (apply #'process-rect (cons lights command)))
    (seq-reduce #'+ lights 0)))

(defun commands (lines op2fun)
  "Convert LINES to commands using OP2FUN to find operations."
  (mapcar (lambda (line) (to-command line op2fun)) lines))

(defun to-command (line op2fun)
  "Turn LINE to a command using OP2FUN to turn word into operation."
  (let* ((raw-parts (split-string line " "))
         (parts (if (string= "turn" (car raw-parts))
                    (cdr raw-parts)
                  raw-parts))
         (fun (funcall op2fun (car parts)))
         (coord1 (string-to-coords (nth 1 parts)))
         (coord2 (string-to-coords (nth 3 parts))))
    (list fun (car coord1) (cdr coord1) (car coord2) (cdr coord2))))

(defun string-to-coords (str)
  "Convert STR of form n,n to (n . n)."
  (cl-destructuring-bind (x y) (split-string str ",")
    (cons (string-to-number x) (string-to-number y))))

(defun brightness-ops (op-word)
  "Turn OP-WORD into brightness command."
  (cond ((string= op-word "on") #'turn-up)
        ((string= op-word "off") #'turn-down)
        ((string= op-word "toggle") #'turn-up-more)))

(defun turn-up (lights x y)
  "Turn light in LIGHTS at point X Y +1."
  (let ((idx (light-idx x y)))
    (aset lights idx (+ (aref lights idx) 1))))

(defun turn-down (lights x y)
  "Turn light in LIGHTS at point X Y -1."
  (let ((idx (light-idx x y)))
    (aset lights idx (max (- (aref lights idx) 1) 0))))

(defun turn-up-more (lights x y)
  "Turn light in LIGHTS at point X Y +2."
  (let ((idx (light-idx x y)))
    (aset lights idx (+ (aref lights idx) 2))))

(defun op-to-fun (op-word)
  "Turn OP-WORD string into function."
  (cond ((string= op-word "on") #'turn-on)
        ((string= op-word "off") #'turn-off)
        ((string= op-word "toggle") #'toggle)))

(defun turn-on (lights x y)
  "Turn on light in LIGHTS at point X Y."
  (aset lights (light-idx x y) 1))

(defun turn-off (lights x y)
  "Turn off light in LIGHTS at point X Y."
  (aset lights (light-idx x y) 0))

(defun toggle (lights x y)
  "Toggle light in LIGHTS at point X Y."
  (let ((idx (light-idx x y)))
    (aset lights idx (mod (+ (aref lights idx) 1) 2))))

(defun light-idx (x y)
  "Index of light at point X Y in the array."
  (+ (* y 1000) x))

(defun process-rect (lights op x1 y1 x2 y2)
  "Process array of LIGHTS doing OP on all points in rectangle X1 Y1 X2 Y2."
  (dolist (x (number-sequence x1 x2))
    (dolist (y (number-sequence y1 y2))
      (funcall op lights x y))))

;;; advent-01 ends here
