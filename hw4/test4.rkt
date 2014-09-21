#lang racket


(define longer-strings
  (lambda ()
    (letrec ([f (lambda(s)
                  (cons s (f (string-append "A" s))))])
      (f "A"))))
