#lang racket
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
           [f (lambda (i) (lambda (v) (print memo)
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



  
  
  
  


   






  

