#lang racket

(define (lvar c) (string->keyword (number->string c)))
(define (lvar? x) (and (keyword? x)
                       (string->number (keyword->string x))))

(define empty-state (cons (hash) 0))
(define mzero '())
(define (unit s/c) (cons s/c mzero))

(define (walk key s)
  (cond [(and (lvar? key) (hash-ref s key #f))
         => (lambda (v) (walk v s))]
        [else key]))

(define (unify left right s)
  (match* ((walk left s) (walk right s))
    [((? lvar?) (? lvar?)) s]
    [((? lvar?) _) (hash-set s left right)]
    [(_ (? lvar?)) (hash-set s right left)]
    [((cons fl rl) (cons fr rr))
     (let ([s (unify fl fr s)])
       (and s (unify rl rr s)))]
    [(l l) s]
    [(_ _) #f]))

(define (== left right)
  (match-lambda
    [(cons s c)
     (let ([s (unify left right s)])
       (if s (unit (cons s c)) mzero))]))

(define (call/goal goal)
  (goal empty-state))

(define (call/fresh function)
  (match-lambda
    [(cons s c)
     ((function (lvar c)) (cons s (+ c 1)))]))

(define (mplus stream1 stream2)
  (cond
    [(procedure? stream1)
     (lambda () (mplus stream2 (stream1)))]

    [(and (pair? stream1) (not (empty? stream1)))
     (cons (car stream1) (mplus (cdr stream1) stream2))]

    [else stream2]))

(define (bind stream goal)
  (cond
    [(procedure? stream)
     (lambda () (bind (stream) goal))]

    [(and (pair? stream) (empty? stream))
     (mplus (goal (car stream)) (bind (cdr stream) goal))]

    [else mzero]))

(define ((disj goal1 goal2) s/c)
  (mplus (goal1 s/c) (goal2 s/c)))

(define ((conj goal1 goal2) s/c)
  (bind (goal1 s/c) goal2))

(define (pull stream)
  (if (procedure? stream)
      (pull (stream))
      stream))

(define (take n stream)
  (if (zero? n)
      '()
      (let ([stream (pull stream)])
        (if (empty? stream)
            '()
            (cons (car stream) (take (- n 1) (cdr stream)))))))

(define (take-all stream)
  (let ([stream (pull stream)])
    (if (empty? stream)
        '()
        (cons (car stream) (take-all (cdr stream))))))



;; Infinite fives and sixes goal:

(define (fives x)
  (disj (== x 5) (lambda (s/c) (lambda () ((fives x) s/c)))))
(define (sixes x)
  (disj (== x 6) (lambda (s/c) (lambda () ((sixes x) s/c)))))
(define fives-and-sixes
  (call/fresh (lambda (x) (disj (fives x) (sixes x)))))

;; (take 5 (call/goal fives-and-sixes))
;; '((#hash((#:0 . 5)) . 1)
;;   (#hash((#:0 . 6)) . 1)
;;   (#hash((#:0 . 5)) . 1)
;;   (#hash((#:0 . 6)) . 1)
;;   (#hash((#:0 . 5)) . 1))
