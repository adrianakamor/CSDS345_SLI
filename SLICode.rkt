#lang racket

; require parser
(require "simpleParser.rkt")

;reads the file and passes it to an evaluator
(define read
  (lambda filename
    (eval-program (parser filename))))

;parses the file into its syntax tree
(define eval-program
  (lambda syntax-tree
    (cond
      ((null? syntax-tree) '())
      (else
       (eval-statement (car syntax-tree))
       (eval-program (cdr syntax-tree))))))

;reads through each statement and determines how it should be treated
;I think I implemented this wrong
(define eval-statement
  (lambda statement
    (cond
      ((null? statement) '())
      ;I'm not sure how to
      ((list? (car statement)) null) 
      ;list of expressions that should call smaller functions
      ((eq? (car statement) 'var) null)
      ((eq? (car statement) '=) null)
      ((eq? (car statement) 'return) null)
      ((eq? (car statement) 'if) null)
      ((eq? (car statement) 'var) null)
      ((eq? (car statement) 'while) null)
      ;may need to have more statements
      (else
       (eval-statement (cdr statement))))))


; interpret function

; binding pairs

; state separation

; returning a value