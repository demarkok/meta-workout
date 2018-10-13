#lang racket

(provide (all-defined-out))

; for environment
(define st-lookup dict-ref)
(define st-bound? dict-has-key?)
(define (st-set st x e)
  (dict-set st x (cons 'quote (list e))))
(define st-empty  #hash())
(define (initial-st vars d)
  (if (equal? (length vars) (length d))
      (for/fold ([st st-empty])
                ([i vars]
                 [j d])
        (st-set st i j))
      (error "initial-st error: program arity mismatch")))

; for basic_blocks
(define bb-lookup dict-ref)
(define bb-set    dict-set)
(define bb-empty  #hash())
(define (initial-prog p)
  (for/fold ([bbs bb-empty])
            ([i (cdr p)])
    (bb-set bbs (car i) (cdr i))))

; my-eval
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))
(define (my-eval e)
  (eval e ns))

; eval expression in current environment
(define (subst st e)
  (match e
    [`(,x . ,y) `(,(subst st x) . ,(subst st y))]
    [`,x (if (st-bound? st x) (st-lookup st x) x)]))

(define (eval-exp st e)
  (define ee (subst st e))
  (my-eval ee))

(define (new-q-tail q label)
  (define (starts-with-label label instruction) (equal? label (car instruction)))
  (member label q starts-with-label))

(define (safe-car maybe-empty) (if (empty? maybe-empty) `B (car maybe-empty)))

(define (safe-cdr maybe-empty) (if (empty? maybe-empty) `() (cdr maybe-empty)))

(define (safe-set-head maybe-empty value) (if (empty? maybe-empty) (cons value empty) (list-set maybe-empty 0 value)))


(define first-label caadr)

(define input-vars cdar)

(define (init-residual program binded-vars)
  `(,(cons `read (set-subtract (input-vars program) binded-vars))))

(define get-label dict-ref)
(define (put-point dict point)
  (if (dict-has-key? dict point) dict (dict-set dict point (length (dict-keys dict)))))

(define (init-code point) `(,point))

(define (add-if-isnt-marked elem marked list)
  (if (set-member? marked elem) list (cons elem list)))

(define (static? division e)
  (match e
    [`(,x . ,y) (and (static? division x) (static? division y))]
    [`,x (not (set-member? division x))]))

(define (get-labels program division)
  (cons (first-label program)
    (for*/list ([bb (cdr program)]
               [instr (cdr bb)]
               #:when (and (equal? `if (car instr)) (not (static? division (cadr instr))))
               [label (cddr instr)])
      label)))
