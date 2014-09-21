#lang racket

[(ifgreater? e) 
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1)
                      (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")))]


[(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)]
               
               [v3 (eval-under-env (ifgreater-e3 e) env)]
               [v4 (eval-under-env (ifgreater-e4 e) env)])
           
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) 
                       (int-num v2))
                   
                   v3
                   v4)
               (error "MUPL comparison applied to non-number")))]

