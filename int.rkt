#lang racket

(require "auxiliary_functions.rkt")
(provide int int-TM)

(define fill_in
  (lambda () '()))

#;(FlowChart is a simple imperative language
   <Program>    ::=   read <Var> ... <Var>: <BasicBlock>+
   <BasicBlock> ::=   <Label>: <Assignment>* <Jump>
   <Label>      ::=   any identifier or value
   <Assignment> ::=   := <Var> <Expression>
   <Jump>       ::=   goto <Label> | if <Expr> <Label> <Label> | return <Expression>
   <Expression> ::=   <Const> | <Var> | <Op> <Expr> ... <Expr>
   <Const>      ::=   any quoted data
   <Op>         ::=   any racket function
   Concrete syntax --- see example from examples.rkt

   int      --- FlowChart interpreter
   int-bb   --- basic block interpreter
   int-jump --- jump interpreter
   int-assn --- assignment interpreter
   int-TM   --- Turing Machine Interpreter on FlowChart
   int :: program -> data -> result
   (see example from examples.rkt)
   
   st is a program state (see dict from racket docs)
   all subsidiary functions are defined in auxiliary_functions.rkt

   function eval-exp (from auxiliary_functions) :: st -> expr -> result
     evaluates expression expr in environment st
   )

(define (int p d)
  (define input-variables (cdar p))
  (define first-bb (cdadr p))
  (define initial-state (initial-st input-variables d))
  (define program (initial-prog p))
  (int-bb program initial-state first-bb))

(define (int-bb prog st bb)
  (define (apply-assignments state assignments)
    (for/fold ([result-state state])
              ([assignment assignments])
      (int-assn result-state assignment)))
  (match bb
    [(list assignments ... jump) (int-jump prog (apply-assignments st assignments) jump)]))

(define (int-jump prog st jump)
  (match jump
    [`(goto, label) (int-bb prog st (bb-lookup prog label))]
    [`(if, condition, then_branch, else_branch)
      (int-bb prog st (bb-lookup prog (if (eval-exp st condition) then_branch else_branch)))]
    [`(return, expr) (eval-exp st expr)]))

(define (int-assn st assn)
  (match assn [`(:=, var, expr) (st-set st var (eval-exp st expr))]))

(define int-TM
  `((read q Right)
    (init (:= q-tail q)
           (:= Left `())
           (goto loop))
    (loop (if (null? q-tail) stop cont))

    (cont  (:= instr (cdar q-tail))
           (:= i (car instr))
           (:= q-tail (cdr q-tail))
           (if (equal? i `right) do_right cont1))
    (cont1 (if (equal? i `left)  do_left  cont2))
    (cont2 (if (equal? i `write) do_write cont3))
    (cont3 (if (equal? i `goto)  do_goto  cont4))
    (cont4 (if (equal? i `if)    do_if    err))

    (do_right (:= Left (cons (safe-car Right) Left))
              (:= Right (safe-cdr Right))
              (goto loop))
    (do_left  (:= Right (cons (safe-car Left) Right))
              (:= Left (safe-cdr Left))
              (goto loop))
    (do_write (:= symbol (cadr instr))
              (:= Right (list-set Right 0 symbol))
              (goto loop))
    (do_goto  (:= next-label (cadr instr))
              (goto jump))
    (do_if    (:= symbol (cadr instr))
              (:= next-label (cadddr instr))
              (if (equal? symbol (safe-car Right)) jump loop))

    (jump (:= q-tail (new-q-tail q next-label))
          (goto loop))
    (err  (return `(unknown-instruction i)))
    (stop (return Right))))
