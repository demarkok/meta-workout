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

(define int
  (lambda (p d)
    (fill_in)))

(define int-bb
  (lambda (prog st bb)
    (fill_in)))

(define int-jump
  (lambda (prog st jump)
    (fill_in)))

(define int-assn
  (lambda (st assn)
    (fill_in)))

(define int-TM
  (fill_in))