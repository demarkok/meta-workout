#lang racket

(require "int.rkt")
(require "auxiliary_functions.rkt")
(require "mix.rkt")


(define (ok? expected actual) 
  (if (equal? expected actual) `OK `(WA. expected: ,expected found: ,actual)))

(define find-name
  '((read name namelist valuelist)
   (search (if (equal? name (car namelist)) found cont))
   (cont (:= valuelist (cdr valuelist))
         (:= namelist (cdr namelist))
         (goto search))
   (found (return (car valuelist)))))

(define tm-simple '((0 if 0 goto 3) (1 right) (2 goto 0) (3 write 1)))

; binary +1 
(define tm-inc '((0 if 1 goto 4) 
                (100 goto 3) 
                (4 write 0) 
                (1 right) 
                (2 goto 0) 
                (3 write 1)
                (10 if B goto 13) 
                (11 left)
                (12 goto 10)
                (13 right)))

; find-name with specialised name and namelist
(define spec-find-name (int mix `(,find-name (valuelist) ((name namelist)  (foo (bar foo))))))
; test find-name specialization
(ok? 239 (int spec-find-name `((42 239))))

; first futamura projection for int-TM. Compiles tm-simple from Turing Machine into FlowChart
(define futamura1-simple (int mix `(,int-TM
                                   (Right Left)
                                   ((q) (,tm-simple)))))
; test first futamura projection for tm-simple
(ok? `(1 1 1 0 1) (int futamura1-simple `((1 1 0 1 1 0 1))))

; first futamura projection for int-TM. Compiles tm-inc from Turing Machine into FlowChart
(define futamura1-inc (int mix `(,int-TM
                                (Right Left)
                                ((q) (,tm-inc)))))
; test first futamura projection for tm-inc
(ok? `(0 0 1 1 1 0 1) (int futamura1-inc `((1 1 0 1 1 0 1))))


; second futamura projection:
; generate a compiler from TM into FlowChart, compilers gets v0: `((q), (,program)) where program is a TM source code.
(define futamura2 (int mix `(,mix
                            (vs0 pending marked residual-code label-dict point pp vs code then-gen-label else-gen-label priii)
                            ((program division) (,int-TM (Right Left))))))

; Compiles tm-simple using the generated compiler
(define futamura2-simple (int futamura2 `(((q) (,tm-simple)))))
(ok? futamura1-simple futamura2-simple)

; Compiles tm-inc using the generated compiler
(define futamura2-inc (int futamura2 `(((q) (,tm-inc)))))
(ok? futamura1-inc futamura2-inc)

