;;; -*- lexical-binding: t -*-
(require 'dash)

(defun var (c) (vector c))
(defun var? (x) (vectorp x))
(defun var=? (x1 x2) (= (elt x1 0) (elt x2 0)))

(defun assp (pred l)
  (-first (lambda (i) (funcall pred (car i))) l))

(defun walk (u s)
  (let ((pr (and (var? u) (assp (lambda (v) (var=? u v)) s))))
    (if pr (walk (cdr pr) s) u)))

;; define pair? function to handle elisp treat empty list '() as
;; nil value, which may cause an infinite recursion in unify
(defun pair? (xs)
  (and (listp xs) (not (null (car xs)))))

(defun ext-s (x v s) `((,x . ,v) . ,s))

(defun == (u v)
  (lambda (s/c)
    (let ((s (unify u v (car s/c))))
      (if s (unit `(,s . ,(cdr s/c))) mzero))))

(defun unit (s/c) (cons s/c mzero))
(defvar mzero nil)

(defun unify (u v s)
  (let ((u (walk u s)) (v (walk v s)))
    (cond
     ((and (var? u) (var? v) (var=? u v)) s)
     ((var? u) (ext-s u v s))
     ((var? v) (ext-s v u s))
     ((and (pair? u) (pair? v))
      (let ((s (unify (car u) (car v) s)))
        (and s (unify (cdr u) (cdr v) s))))
     (t (and (equal u v) s))))) ;; TODO is eq more correct than equal?

(defun call/fresh (f)
  (lambda (s/c)
    (let ((c (cdr s/c)))
      (funcall (funcall f (var c)) `(,(car s/c) . ,(+ c 1))))))

(defun disj (g1 g2)
  (lambda (s/c) (mplus (funcall g1 s/c) (funcall g2 s/c))))

(defun conj (g1 g2)
  (lambda (s/c) (bind (funcall g1 s/c) g2)))

(defun mplus ($1 $2)
  (cond
   ((null $1) $2)
   ((functionp $1) (lambda () (mplus $2 (funcall $1))))
   (t (cons (car $1) (mplus (cdr $1) $2)))))

(defun bind ($ g)
  (cond
   ((null $) mzero)
   ((functionp $) (lambda () (bind (funcall $) g)))
   (t (mplus (funcall g (car $)) (bind (cdr $) g)))))

(setq empty-s (ext-s (var -1) 'dummy '()))

(setq empty-state `(,empty-s . 0))


(unify (list 1 2 3) (list 1 2 (var 0)) empty-s)

;; (unify (list 1 2 3) (list 1 2 (var 0)) empty-s)
;; => (([0] . 3) ([-1] . dummy))

;; (funcall (call/fresh (lambda (q) (== q 5))) empty-state)
;; => (((([0] . 5)) . 1))

;; (funcall
;;  (conj
;;   (call/fresh (lambda (a) (== a 7)))
;;   (call/fresh (lambda (b) (disj (== b 5) (== b 6)))))
;;  empty-state)
;; => (((([1] . 5) ([0] . 7)) . 2) ((([1] . 6) ([0] . 7)) . 2))


