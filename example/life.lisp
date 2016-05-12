
(ql:quickload :cl-charms)
(ql:quickload :trivia)
(ql:quickload :vanilla-lc)

(defpackage :life
  (:use :cl :vanilla-lc)
  (:import-from :trivia :defpattern :ematch)
  (:import-from :vanilla-lc :^))

(in-package :life)

(defvar glider-gun-points
  ;; generated from glider.rb; 9 rows, 37 cols
  '((0 . 25) (1 . 23) (1 . 25) (2 . 13) (2 . 14) (2 . 21)
    (2 . 22) (2 . 35) (2 . 36) (3 . 12) (3 . 16) (3 . 21)
    (3 . 22) (3 . 35) (3 . 36) (4 .  1) (4 .  2) (4 . 11)
    (4 . 17) (4 . 21) (4 . 22) (5 .  1) (5 .  2) (5 . 11)
    (5 . 15) (5 . 17) (5 . 18) (5 . 23) (5 . 25) (6 . 11)
    (6 . 17) (6 . 25) (7 . 12) (7 . 16) (8 . 13) (8 . 14)))

(defvar church-zero (mklambda1 (mklambda1 0 nil 0) nil 0))
(defvar church-one  (mklambda1 (mklambda1 (mkapp 1 0 nil 1) nil 1) nil 0))

(defun mkcons (value rest)
  "Creates a cons cell of in my array format.  If REST is nil it
returns an infinite list of VALUE."
  (if (null rest)
      (mklambda1 (mklambda1 (mkapp 0 value)))
      (mklambda1 (mklambda1 (mkapp (mkapp 1 value) rest)))))

(defpattern \cons (value rest &optional dirty depth)
  `(or (lambda1 (lambda1 (app (app 1 ,value) ,rest)) ,dirty ,depth)
       (lambda1 (lambda1 (app 0 ,value)) ,dirty ,depth)))

(defun mkarray (terms default)
  (labels ((rec (terms acc)
	     (if terms
		 (rec (cdr terms)
		      (mkcons (car terms) acc))
		 acc)))
    (rec (reverse terms) (mkcons default nil))))

(defun mkgrid (points rows cols)
  (let (matrix)
    (labels ((rec (row col arr)
	       (cond
		 ((>= col cols)
		  (mkarray (nreverse arr) church-zero))
		 ((member (cons row col) points :test #'equal)
		  (rec row (1+ col) (cons church-one arr)))
		 (t
		  (rec row (1+ col) (cons church-zero arr))))))
      (dotimes (row rows (mkarray (reverse matrix) church-zero))
	(push (rec row 0 nil) matrix)))))

(defun convert-grid-row (term)
  (ematch term
    ((\cons _ (^ nil)) nil)
    ((\cons (^ church-zero) rest)
     (cons 0 (convert-grid-row rest)))
    ((\cons (^ church-one)  rest)
     (cons 1 (convert-grid-row rest)))))

(defun convert-grid (term)
  "Convert a grid term into lisp lists."
  (ematch term
    ((\cons _ (^ nil)) nil)
    ((\cons row rest)
     (cons (convert-grid-row row)
	   (convert-grid rest)))))

(defun print-char (char)
  (charms:write-char-at-cursor charms:*standard-window* char))

(defun update-display (term)
  (charms:move-cursor  charms:*standard-window* 0 0)
  (charms:clear-window charms:*standard-window*)
  (dolist (row (convert-grid term))
    (dolist (x row)
      (print-char (if (zerop x) #\  #\#)))
    (print-char #\Newline))
  (charms:refresh-window charms:*standard-window*))

;; takes ~300MB per cycle, increase this to improve gc speed
(setf (sb-ext:bytes-consed-between-gcs) (* 350 1024 1024))

(defvar grid)

(time
 (charms:with-curses ()
   (charms:disable-echoing)
   (charms:enable-raw-input :interpret-control-characters t)
   (charms:enable-non-blocking-mode charms:*standard-window*)
   (charms:clear-window charms:*standard-window*)
   (charms:refresh-window charms:*standard-window*)
   (charms:move-cursor charms:*standard-window* 0 0)

   ;; 12 rows to allow glider to move some
   (setq grid (reduce-star (mkgrid glider-gun-points 12 38)))
   (dotimes (x 30) ;; glider gun period = 30 steps
     ;; NOTE still life forms from glider falling off grid
     (setq grid
	   (vanilla-lc:eval-file
	    (concatenate 'string (sb-posix:getcwd) "/life.lc")
	    :consts `(("Grid" . ,grid))
	    :timeout 10)) ;; takes ~340ms on my machine
     (update-display grid)
     ;; manual gcing to prevent it during evaluation
     (sb-ext:gc))))
