#lang racket

; require parser
(require "simpleParser.rkt")

;reads the file and passes it to an evaluator
(define read
  (lambda filename
    (eval-program (parser filename))))

;parses the file into its syntax tree
(define eval-program
  (lambda syntax-tree states
    (cond
      ;return-function is a function that returns the 'return' value in the state list
      ((null? syntax-tree) return-function(states))
      ((eq? (car syntax-tree)'return) return_function)
      (else
       ;Essentially how the states will be handled is that each statement (like var and while) will have the value it returns be the list of states (formatted as ((x y...)(5 7...)) ), which is the way
       ;that is "easier to implement later on"). 
       (eval-program (cdr syntax-tree) (eval-statement (car syntax-tree) states))))))

;reads through each statement and determines how it should be treated depending on the keywords
;I think I implemented this wrong
(define eval-statement
  (lambda statement states
    (cond
      ((null? statement) '())
      ;I'm not sure how to
      ((list? (car statement)) null)
      ;list of expressions that should call smaller functions

      ((eq? (car statement) 'var) (declare-var statement states)
       (if (= (length statement) 2)
           (cons (cons (car (cdr statement)) #f) state)
           (cons (cons (car (cdr statement) (eval-expressions (car (cdr (car statement))) state)))))
      
      ((eq? (car statement) '=) (init-assign statement states))
      ((eq? (car statement) 'return) (return statement states))
      ;handling expressions will be tough, since an expression could be just arithmetic or a variable initialization...I can work on that if need be...
      ;((eq? (car statement) 'return) (return statement states))
      ((eq? (car statement) 'if) (if statement states))
      ((eq? (car statement) 'while) (while statement states))
      (else
       (eval-statement (cdr statement)))))))

;accepts the current statement (which will have a var in it) and the current list of states ((...)(...))
;this creates the first entry for a variable
;will detect if there is an equals sign after this, in which case it will call the assign function
;returns the updated state with a new variable
(define declare-var
  (lambda (statement states)
    '()))

;accepts the current statement (which will have a var in it) and the current list of states ((...)(...))
;checks to make sure that the variable has been declared; if not (idk what it'll do lol)
;will take the value on the right of the = and first pass it into an expression function
;will then take the result of the expression function and assign it to the variable on the left of the =
;will return the updated state with an updated value for the var in question
(define init-assign
  (lambda (statement states)
    '()))

;accepts the current statement (which will have a var in it) and the current list of states ((...)(...))
;passes whatever is after the return to the expression function
;returns an updated state for the return
(define return
  (lambda (return states)
    '()))

; accepts the current statement as a while loop, and a current list of states
; checks to ensure that the loop condition is met before entering the loop
; if met, loops until loop condition is no longer met
; if not met, returns the current state
; I know this iteration isn't correct, but I wanted to get down general logic
(define while
  (lambda (condition body states)
    (if (condition)
        (while (condition body states)) ; Need to work on incrementing the condition, changing the state with the body, verifying that variables are withing the bounds of loop condition
        states)))

; interpret function

; binding pairs

; state separation

; other helper method for logic/math expressions
; currently just works with actual integer inputs, need to adjust to use defined variables
  (define eval-expressions
    (lambda (expression state)
      (cond
        ((eq? (car expression) '+)   (+ (eval_expressions (cadr expression) state) (eval_expressions (caddr expression) state)))
        ((eq? (car expression) '-)   (- (eval_expressions (cadr expression) state) (eval_expressions (caddr expression) state)))
        ((eq? (car expression) '*)   (* (eval_expressions (cadr expression) state) (eval_expressions (caddr expression) state)))
        ((eq? (car expression) '/)   (quotient (eval_expressions (cadr expression) state) (eval_expressions (caddr expression) state)))
        ((eq? (car expression) '%)   (remainder (eval_expressions (cadr expression) state) (eval_expressions (caddr expression) state)))
        ((eq? (car expression) '==)  (eq? (eval_expressions (cadr expression) state) (eval_expressions (caddr expression) state)))
        ((eq? (car expression) '!=)  (not (eq? (eval_expressions (cadr expression) state) (eval_expressions (caddr expression) state))))
        ((eq? (car expression) '<)   (< (eval_expressions (cadr expression) state) (eval_expressions (caddr expression) state)))
        ((eq? (car expression) '>)   (> (eval_expressions (cadr expression) state) (eval_expressions (caddr expression) state)))
        ((eq? (car expression) '<=)  (<= (eval_expressions (cadr expression) state) (eval_expressions (caddr expression) state)))
        ((eq? (car expression) '>=)  (>= (eval_expressions (cadr expression) state) (eval_expressions (caddr expression) state)))
        ((eq? (car expression) '&&)  (and (eval_expressions (cadr expression) state) (eval_expressions (caddr expression) state)))
        ((eq? (car expression) '||)  (or (eval_expressions (cadr expression) state) (eval_expressions (caddr expression) state)))
        ((eq? (car expression) '!)   (not (eval_expresssions (cadr expression) state)))
        ((number? expression)        expression)
        ((boolean? expression)       expression)
        ((symbol? expression)        (lookup expression state)))))