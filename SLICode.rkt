#lang racket
; require parser
(require "simpleParser.rkt")


; ------------------------------------------------------------
; CSDS 345 - Simple Language Interpreter
; Group 3: Noah Henriques, Stephen Hogeman, Adriana Kamor
; ------------------------------------------------------------


; Parser Functions
; ------------------------------------------------------------
; Interpret Function
;interprets the file and passes it to an evaluator
(define interpret
  (lambda (filename)
    ;the program is initialized with a return statement
    (eval-program (parser filename) '((return) ()))))

;Parse Function
;parses the file into its syntax tree
(define eval-program
  (lambda (syntax-tree states)
    (cond
      ;how returns will be handled is that during the program, we can assume that the return function is called only once except for if/else cases.
      ;essentially, it will be treated the same as any other variable 'x' or 'y'
      ;at the end of the syntax tree, the return value will be called from the state list and outputted
      ((null? syntax-tree) (lookup 'return states))
      (else
       ;Essentially how the states will be handled is that each statement (like var and while) will have the value it returns be the list of states (formatted as ((x y...)(5 7...)) 
       (eval-program (cdr syntax-tree) (eval-statement (car syntax-tree) states))))))
; ------------------------------------------------------------


; Expression & Statement Evaluations
; ------------------------------------------------------------
; M_state Function
;reads through each statement and determines how it should be treated depending on the keywords
(define eval-statement
  (lambda (statement states)
    (cond
      ((null? statement) '())
      ((list? (car statement)) null)
      ;list of expressions that should call smaller functions
      ((eq? (car statement) 'var) (declare-var statement states))
      ((eq? (car statement) '=) (init-assign (cadr statement) (cddr statement) states))
      ((eq? (car statement) 'return) (init-assign 'return statement states))
      ;give something that returns whether or not this is a statement
      ;((eq? (car statement) eval-expression(car statement)) (return statement states))
      ((eq? (car statement) 'if)
         (if-statement (eval_expressions (cadr statement) states)
             (eval-program (caddr statement) states)
             (eval-program (cadddr statement) states)))
      ((eq? (car statement) 'while) (while (eval_expressions (cadr statement) states) (eval_expressions (caddr statement) states)))
      (else
       (eval-statement (cdr statement) states)))))

; M_value Function
  (define eval_expressions
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
        ((eq? (car expression) '!)   (not (eval_expressions (cadr expression) state)))
        ((eq? (car expression) 'true)(#t (eval_expressions (cadr expression) state) (eval_expressions (caddr expression) state)))
        ((eq? (car expression) 'false)(#f (eval_expressions (cadr expression) state) (eval_expressions (caddr expression) state)))
        ((number? expression)        expression)
        ((boolean? expression)       expression)
        ((symbol? expression)        (lookup expression state))
        (else (error "Type Unknown")))))

; ------------------------------------------------------------


; Helper Functions
; ------------------------------------------------------------
;list of helper functions:
;function that updates the states given a variable and a value
(define update-states
  (lambda (vari val states)
    (replacer (find-index vari (list(car states))) val states)))

;function that appends two lists together; will be used for state assignment
(define append
  (lambda (lis1 lis2)
	(if (null? lis1)
    	lis2
    	(cons (car lis1) (append(cdr lis1) lis2)))))

;function + helper that takes the states and a given variable, and finds the index of that variable in the first part of states
(define find-index
  (lambda (x lst)
  (loop (car lst) x 0)))
(define loop
  (lambda (lst x index)
  (cond
    ((eq? (car lst) x) index)
    (else (loop (cdr lst) x (+ index 1))))))

;function + helper that takes an index and iterates through the second part of the states until it lands on that index,
;replacing the value there with the one given
(define replacer
  (lambda (index value lst)
  (list (car lst) (replacer-helper (cadr lst) index value))))
(define replacer-helper
  (lambda (lst index value)
  (if (= index 0)
      (cons value (cdr lst))
      (cons (car lst) (cons (replacer-helper (cdr lst) (- index 1) value)'())))))

;helper that takes an index and iterates through the second part of the states until it lands on that index,
;returning that value
(define lookup-helper
  (lambda (index lst)
    (if (= index 0)
        (car lst)
        (lookup-helper (- index 1) (cdr lst)))))

;inserts a variable and value into the state given the variable expression and the state
(define create-pair
  (lambda (y x value)
  (cons (cons y (car x))
        (cons (cons value (cadr x)) '()))))
; ------------------------------------------------------------


; Syntax Tree Statements
; ------------------------------------------------------------

;accepts the current statement (which will have a var in it) and the current list of states ((...)(...))
;will detect if there is an equals sign after this, in which case it will call the assign function
;cadr is the variable of the statement
;cdddr is the expression of the statement
;returns the updated state with a new variable
(define declare-var
  (lambda (statement states)
    (cond ((null? (cadr statement)) (error "Error in var statement!"))
        (else
         ;checks if there's an equals sign/anything after the variable
         (if (not (null? (cddr statement)))
             ;all this does is calls the initializer/assigner to initialize this variable
             (init-assign (cadr statement) (caddr statement) (create-pair (cadr statement) states '()))
             ;if there's nothing after the equals sign, it returns the new list as normal
             (create-pair (cadr statement) states '()))))))
            
        
;accepts the variable in question, an expression, and the current list of states ((...)(...))
;checks to make sure that the variable has been declared; if not (idk what it'll do lol)
;will take the value on the right of the = and first pass it into an expression function
;will then take the result of the expression function and assign it to the variable on the left of the =
;will return the updated state with an updated value for the var in question
(define init-assign
  (lambda (vari expression states)
    (if (null? expression)
         (error "Error in var statement!")
          ;assigns the vari a number after however many times it has to iterate through this statement
          ; handle state assignment here
          (update-states vari (eval_expressions expression states) states))))

;looks up a variable's value in the state table
(define lookup
  (lambda (vari states)
    (lookup-helper (find-index vari states) (cadr states))))

; ------------------------------------------------------------


; While Loop
; ------------------------------------------------------------
; accepts the current statement as a while loop, and a current list of states
; checks to ensure that the loop condition is met before entering the loop
; if met, loops until loop condition is no longer met
; if not met, returns the current state
(define while
  (lambda (condition body states)
    (if (eval_expressions condition states)
        (while condition (eval-statement body states) states)
        states)))

; ------------------------------------------------------------

; If Statement
; ------------------------------------------------------------
; accepts the current statement as an if statement and a list of states
; if the condition is met/is true, we perform the desired operation
(define if-statement
  (lambda (condition true-condition false-condition states)
    (if (eval_expressions condition states)
        (eval-program true-condition states)
        (eval-program false-condition states))))
; ------------------------------------------------------------