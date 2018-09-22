#lang racket

(require "auxiliary_functions.rkt")
(provide int)

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
