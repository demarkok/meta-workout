(require "auxiliary_functions.rkt")
(provide mix)


; TODO: first-label init-residual bb-lookup init-code is-static reduce eval... add-if-isnt-marked


(define mix 
  `((read program division vs0)
    (init (:= pp0 (first-label program))
          (:= pending `((pp0 vs0)))
          (:= marked `())
          (:= residual-code (init-residual program))
          (goto loop1))
    
    (loop1 (if (empty? pending) stop1 cont1))
    (cont1 (:= point (car pending))
           (:= pp (first point))
           (:= vs (second point))
           (:= pending (cdr pending))
           (:= marked (cons point marked))
           (:= bb (bb-lookup pp program))
           (:= code (init-code pp vs))
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
          (static-branch-ass  (:= vs (... vs x (eval... expr vs)))
                              (goto loop2))
          (dynamic-branch-ass (:= code (extend code `(:= ,x ,(reduce expr vs))))
                              (goto loop2))
        
        (do-if (:= expr (cadr instruction))
               (:= then-label (caddr instruction))
               (:= else-label (cadddr instruction))
               (if (is-static division expr) static-branch-if dynamic-branch-if))
          (static-branch-if (:= bb (bb-lookup (if (eval... expr vs) then-label else-label) program))
                            (goto loop2))
          (dynamic-branch-if (:= pending (add-if-isnt-marked `(,then-label ,vs) marked pending))
                             (:= pending (add-if-isnt-marked `(,else-label ,vs) marked pending))
                             (:= code (extend code `(if ,(reduce expr vs) ,then-label ,else-label)))
                             (goto loop2))
          
        (do-goto (:= next-label (cadr instruction))
                 (:= bb (bb-lookup next-label program))
                 (goto loop2))

        (do-return (:= expr (cadr instruction)) 
                   (:= code (extend code `(return ,(reduce expr vs))))
                   (goto loop2))

      (stop2 (:= (extend residual-code code))
             (goto loop1))

    (stop1 (return residual-code))
    (err  (return `(unknown-instruction i)))))
