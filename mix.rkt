#lang racket

(require "auxiliary_functions.rkt")
(provide mix)


; TODO: (subst => reduce)


; Division should be a pair of two lists: static variables names and dynamic ones
(define mix 
  `((read program division vs0)
    (init (:= pp0 (first-label program))
          (:= pending `((,pp0 ,(initial-st (take (input-vars program) (length vs0)) vs0))))
          (:= marked `())
          (:= residual-code (init-residual program (length vs0)))
          (goto loop1))
    
    (loop1 (if (empty? pending) stop1 cont1))
    (cont1 (:= point (car pending))
           (:= pp (first point))
           (:= vs (second))
           (:= pending (cdr pending))
           (:= marked (cons point marked))
           (:= bb (bb-lookup program pp))
           (:= code (init-code point))
           (goto loop2))

      (loop2 (if (empty? bb) stop2 cont2))
      (cont2 (:= instruction (car bb))
             (:= bb (cdr bb))
             (:= i (car instruction))
             (goto switch0))

        (switch0 (if (equal? i `:=) do-assignment switch1))
        (switch1 (if (equal? i `if) do-if switch2))
        (switch2 (if (equal? i `goto) do-goto switch3))
        (switch3 (if (equal? i `return) do-return err))

        (do-assignment (:= x (cadr instruction))
                       (:= expr (caddr instruction))
                       (if (is-static division x) static-branch-ass dynamic-branch-ass))
          (static-branch-ass  (:= vs (st-set vs x (eval-exp vs expr)))
                              (goto loop2))
          (dynamic-branch-ass (:= code (cons `(:= ,x ,(subst vs expr)) code))
                              (goto loop2))
        
        (do-if (:= expr (cadr instruction))
               (:= then-label (caddr instruction))
               (:= else-label (cadddr instruction))
               (if (is-static division expr) static-branch-if dynamic-branch-if))
          (static-branch-if (:= bb (bb-lookup program (if (eval-exp vs expr) then-label else-label)))
                            (goto loop2))
          (dynamic-branch-if (:= pending (add-if-isnt-marked `(,then-label ,vs) marked pending))
                             (:= pending (add-if-isnt-marked `(,else-label ,vs) marked pending))
                             (:= code (cons `(if ,(subst vs expr) ,then-label ,else-label) code))
                             (goto loop2))
          
        (do-goto (:= next-label (cadr instruction))
                 (:= bb (bb-lookup program next-label))
                 (goto loop2))

        (do-return (:= expr (cadr instruction)) 
                   (:= code (cons `(return ,(subst vs expr)) code))
                   (goto loop2))

      (stop2 (:= residual-code (cons (reverse code) residual-code))
             (goto loop1))

    (stop1 (return (reverse residual-code)))
    (err  (return `(unknown-instruction i)))))
