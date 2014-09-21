#lang racket

;; streams for testing
(define (stream-maker fn arg)
  (letrec ([f (lambda (x)
                (cons x (lambda () (f (fn x arg)))))])
    (lambda () (f arg))))
(define ones (stream-maker (lambda (x y) 1) 1))
(define nats  (stream-maker + 1))
(define powers-of-two (stream-maker * 2))

;; returns a list of first n elements produced by stream s 
(define (stream-for-n-steps s n)
  (let ([pr (s)])
    (cond [(= n 0) null]
          [(= n 1) (list (car pr))]
          [#t (cons (car pr) (stream-for-n-steps (cdr pr) (- n 1)))])))


(define longer-strings
  (lambda ()
    (letrec ([f (lambda(s)
                  (cons s (f (string-append "A" s))))])
      (f "A"))))



