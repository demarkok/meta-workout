#lang racket

(require "int.rkt")
(require "auxiliary_functions.rkt")

(define find_name
  '((read name namelist valuelist)
   (search (if (equal? name (car namelist)) found cont))
   (cont (:= valuelist (cdr valuelist))
         (:= namelist (cdr namelist))
         (goto search))
   (found (return (car valuelist)))))

; (int-assn (st-set st-empty 'x '(1 2 3)) '(:= x (car x)))
(int find_name '(x (x y z) (1 2 3)))
(int find_name '(z (x y z) (1 2 3)))
(int int-TM '(((0 if 0 goto 3) (1 right) (2 goto 0) (3 write 1)) 
              (1 1 0 1 1 0 1)))

; binary +1 
(define inc '((0 if 1 goto 4) 
              (100 goto 3) 
              (4 write 0) 
              (1 right) 
              (2 goto 0) 
              (3 write 1)
              (10 if B goto 13) 
              (11 left)
              (12 goto 10)
              (13 right)))

(int int-TM (cons inc `((1 1 1 1 0 0 1))))
