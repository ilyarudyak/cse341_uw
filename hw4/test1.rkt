#lang racket

(define vec (vector (cons 2 1) (cons 3 1) (cons 4 1) (cons 5 1)))
(define vec1 (vector (cons 1 1)))


(define xs (list (cons 1 10) (cons 2 20) (cons 3 30) (cons 4 40) (cons 5 50)))
(define f (cached-assoc xs 2))

;;((cached-assoc xs n) 1)