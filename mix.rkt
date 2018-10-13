#lang racket

(require "auxiliary_functions.rkt")
(provide mix)

; TODO: (subst => reduce)
;       (simplify labels naming)

; Division is a list of dynamic variables
(define mix 
  `((read program division vs0)
    (init (:= pp0 (first-label program))
          (:= pending `((,pp0 ,(initial-st (first vs0) (second vs0)))))
          (:= marked `(,(car pending)))
          (:= residual-code (init-residual program (first vs0)))
          (goto loop1))
    
    (loop1 (if (empty? pending) stop1 cont1))
    (cont1 (:= point (car pending))
           (:= pp (first point))
           (:= vs (second point))
           (:= pending (cdr pending))
           (:= labels (get-labels program division))
           (:= code (init-code point))
           (goto loopX))

      (loopX (if (empty? labels) err2 contX))
      (contX (:= ppp (car labels))
             (:= labels (cdr labels))
             (if (equal? ppp pp) contX2 loopX))
      (contX2 
             (:= bb (bb-lookup program ppp))
             (goto loop2))

        (loop2 (:= instruction `())
               (:= i `())
               (if (empty? bb) stop2 cont2))
        (cont2 (:= instruction (car bb))
               (:= bb (cdr bb))
               (:= i (car instruction))
               (goto switch0))

          (switch0 (if (equal? i `:=) do-assignment switch1))
          (switch1 (if (equal? i `if) do-if switch2))
          (switch2 (if (equal? i `goto) do-goto switch3))
          (switch3 (if (equal? i `return) do-return err))

          (do-assignment (if (static? division (cadr instruction)) static-branch-ass dynamic-branch-ass))
            (static-branch-ass  (:= vs (st-set vs (cadr instruction) (eval-exp vs (caddr instruction))))
                                (goto loop2))
            (dynamic-branch-ass (:= code (cons `(:= ,(cadr instruction) ,(subst vs (caddr instruction))) code))
                                (goto loop2))
          
          (do-if (if (static? division (cadr instruction)) static-branch-if dynamic-branch-if))
            (static-branch-if (if (eval-exp vs (cadr instruction)) sbi-true sbi-false))
              (sbi-true (:= bb (bb-lookup program (caddr instruction)))
                        (goto loop2))
              (sbi-false (:= bb (bb-lookup program (cadddr instruction)))
                         (goto loop2))
            (dynamic-branch-if (:= pending (add-if-isnt-marked `(,(cadddr instruction) ,vs) marked pending))
                               (:= marked (cons `(,(cadddr instruction) ,vs) marked))
                               
                               (:= pending (add-if-isnt-marked `(,(caddr instruction) ,vs) marked pending))
                               (:= marked (cons `(,(caddr instruction) ,vs) marked))

                               (:= code (cons `(if ,(subst vs (cadr instruction)) (,(caddr instruction) ,vs) (,(cadddr instruction) ,vs)) code))
                               (goto loop2))
        
          (do-goto 
                   (:= bb (bb-lookup program (cadr instruction)))
                   (goto loop2))

          (do-return  
                     (:= code (cons `(return ,(subst vs (cadr instruction))) code))
                     (goto loop2))

        (stop2 
               (:= residual-code (cons (reverse code) residual-code))
               ; (:= priii (println (length marked)))
               (goto loop1))

    (stop1 (return (reverse residual-code)))
    (err  (return `(unknown-instruction i)))
    (err2 (return `(wtf?)))))
