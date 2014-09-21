
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (cond [(> low high) '()]
        [(> (+ low stride) high) (list low)]
        [#t (cons low (sequence (+ low stride) high stride))]))

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

;; streams for testing
(define (stream-maker fn arg)
  (letrec ([f (lambda (x)
                (cons x (lambda () (f (fn x arg)))))])
    (lambda () (f arg))))
;(define ones (stream-maker (lambda (x y) 1) 1))
(define nats  (stream-maker + 1))
(define powers-of-two (stream-maker * 2))

;; returns a list of first n elements produced by stream s 
(define (stream-for-n-steps s n)
  (let ([pr (s)])
    (cond [(= n 0) null]
          [(= n 1) (list (car pr))]
          [#t (cons (car pr) (stream-for-n-steps (cdr pr) (- n 1)))])))

;; stream of natural numbers where numbers divisble by 5 are negated
(define funny-number-stream
  (letrec ([g (lambda (x)
                (if (= (remainder x 5) 0)
                    (cons (- x) (lambda () (g (+ x 1))))
                    (cons x (lambda () (g (+ x 1))))))])
  (lambda () (g 1))))
  
;; stream of strings alternate between "dan.jpg" "dog.jpg" (starting from "dan.jpg")
(define dan-then-dog
  (letrec ([g (lambda (x)
                (if (= (remainder x 2) 0)
                    (cons "dog.jpg" (lambda () (g (+ x 1))))
                    (cons "dan.jpg" (lambda () (g (+ x 1))))))])
  (lambda () (g 1))))

;; takes stream s = v0 v1 ... and return stream (0 . v0) (0 . v1) ...
(define (stream-add-zero s)
  (letrec ([f (lambda (x s) (cons (cons 0 (car (s))) (lambda () (f (+ x 1) (cdr (s))))))])
    (lambda () (f 1 s))))

;; cycle-lists takes two lists xs and ys and returns a stream of pairs
(define (cycle-lists xs ys)
  (letrec ([nth (lambda (as n) (if (= n 0)
                                   (car as)
                                   (nth (cdr as) (- n 1))))])
                
  (letrec ([f (lambda (x) (cons (cons (nth xs (remainder x (length xs))) 
                                      (nth ys (remainder x (length ys)))) 
                                (lambda () (f (+ x 1)))))])
    
    (lambda () (f 0)))))

;; takes vector of pairs and value v; returns the pair if (car pair) == v (first pair); 
;; #f if no such pair
(define (vector-assoc v vec)
  (letrec ([f (lambda (vec n)
                (cond [(= n (vector-length vec)) #f]
                      [(not (pair? (vector-ref vec n))) (f vec (+ n 1))]
                      [(equal? (car (vector-ref vec n)) v) (vector-ref vec n)]
                      [#t (f vec (+ n 1))]))])
    (f vec 0)))
      
  
;; returns function f of v; it uses cache of size n to look up v in xs;
;; xs - list of pairs; f is like assoc (but with cache); cache is vector of size n
(define (cached-assoc xs n)
  (letrec ([memo (make-vector n #f)]
           [f (lambda (i) (lambda (v) 
                (let ([ans (vector-assoc v memo)]) 
                  (if ans
                      ans
                      (let ([new-ans (assoc v xs)])
                        (if new-ans
                            (begin
                              (vector-set! memo i new-ans) (if (< i (- n 1)) (set! i (+ i 1)) (set! i 0))
                              new-ans)
                            #f))))))])
  (f 0)))


























































































