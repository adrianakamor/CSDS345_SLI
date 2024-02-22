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
; interpret: interprets the file and passes it to an evaluator
(define interpret
  (lambda (filename)
    ;the program is initialized with a return statement
    (eval-program (parser filename) '((return) (())))))

; Parse Function
; eval-program: parses the file into its syntax tree
      ; how returns will be handled is that during the program, we can assume that the return function is called only once except for if/else cases
      ; essentially, it will be treated the same as any other variable 'x' or 'y'
      ; at the end of the syntax tree, the return value will be called from the state list and outputted
      ; essentially how the states will be handled is that each statement (like var and while) will have the value it returns be the list of states
      ; (formatted as ((x y...)(5 7...)) 
(define eval-program
  (lambda (syntax-tree states)
    (cond
      ((null? syntax-tree) (lookup 'return states))
      (else
       (eval-program (cdr syntax-tree) (eval-statement (car syntax-tree) states))))))
; ------------------------------------------------------------



; Expression & Statement Evaluations
; ------------------------------------------------------------
; M_state Function
; eval-statement: reads through each statement and determines how it should be treated depending on the keywords
(define eval-statement
  (lambda (statement states)
    (cond
      ((null? statement) '())
      ((list? (car statement)) null)
      ((eq? (car statement) 'var) (declare-var statement states))
      ((eq? (car statement) '=) (init-assign (cadr statement) (cddr statement) states))
      ((eq? (car statement) 'return) (init-assign 'return (cdr statement) states))
      ((eq? (car statement) 'if)
            (if-statement (eval_expressions (cadr statement) states)
                          (eval-statement (caddr statement) states)
                          (eval-statement (cadddr statement) states) states))
      ((eq? (car statement) 'while) (while (cadr statement) (caddr statement) states))
      (else
       (eval-statement (cdr statement) states)))))

; M_value Function
; eval_expressions: reads through each statement and determines which expressions are used and how those expressions should be treated
  (define eval_expressions
    (lambda (expression state)
      (cond
        ((number? expression)        expression)
        ((boolean? expression)       expression)
        ((symbol? expression)        (lookup expression state))
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
        (else (error "Type Unknown")))))

; ------------------------------------------------------------



; Helper Functions
; ------------------------------------------------------------
; update-states: function that updates the states given a variable and a value
(define update-states
  (lambda (vari val states)
    (replacer (find-index vari (list(car states))) val states)))

; append: function that appends two lists together; will be used for state assignment
(define append
  (lambda (lis1 lis2)
	(if (null? lis1)
    	lis2
    	(cons (car lis1) (append(cdr lis1) lis2)))))

; find-index: function + helper that takes the states and a given variable, and finds the index of that variable in the first part of states
(define find-index
  (lambda (x lst)
  (loop (car lst) x 0)))
(define loop
  (lambda (lst x index)
  (cond
    ((eq? (car lst) x) index)
    (else (loop (cdr lst) x (+ index 1))))))

; replacer: function + helper that takes an index and iterates through the second part of the states until it lands on that index,
      ; replacing the value there with the one given
(define replacer
  (lambda (index value lst)
  (list (car lst) (replacer-helper (cadr lst) index value))))
(define replacer-helper
  (lambda (lst index value)
  (if (= index 0)
      (cons value (cdr lst))
      (cons (car lst) (replacer-helper (cdr lst) (- index 1) value)))))

; lookup-helper: helper that takes an index and iterates through the second part of the states until it lands on that index, returning that value
(define lookup-helper
  (lambda (index lst)
    (if (= index 0)
        (car lst)
        (lookup-helper (- index 1) (cdr lst)))))

; create-pair: inserts a variable and value into the state given the variable expression and the state
(define create-pair
  (lambda (y x value)
  (cons (cons y (car x))
        (cons (cons value (cadr x)) '()))))
; ------------------------------------------------------------



; Syntax Tree Statements
; ------------------------------------------------------------
; declare-var: accepts the current statement (which will have a var in it) and the current list of states ((...)(...))
      ; will detect if there is an equals sign after this, in which case it will call the assign function
      ; cadr is the variable of the statement
      ; cdddr is the expression of the statement
      ; returns the updated state with a new variable
      ; checks if there's an equals sign/anything after the variable
      ; calls the initializer/assigner to initialize this variable, if there's nothing after the equals sign, it returns the new list as normal
(define declare-var
  (lambda (statement states)
    (cond ((null? (cadr statement)) (error "Error in var statement!"))
        (else
         (if (not (null? (cddr statement)))
             (init-assign (cadr statement) (cddr statement) (create-pair (cadr statement) states '()))
             (create-pair (cadr statement) states '()))))))
            
        
; init-assign: accepts the variable in question, an expression, and the current list of states ((...)(...))
      ; checks to make sure that the variable has been declared; if not (idk what it'll do lol)
      ; will take the value on the right of the = and first pass it into an expression function
      ; will then take the result of the expression function and assign it to the variable on the left of the =
      ; will return the updated state with an updated value for the var in question
      ;assigns the vari a number after however many times it has to iterate through this statement
(define init-assign
  (lambda (vari expression states)
    (if (null? expression)
         (error "Error in var statement!")
          (update-states vari (eval_expressions (car expression) states) states))))

; lookup: looks up a variable's value in the state table
(define lookup
  (lambda (vari states)
    (lookup-helper (find-index vari states) (cadr states))))
; ------------------------------------------------------------



; While Loop
; ------------------------------------------------------------
; while: accepts the current statement as a while loop, and a current list of states
      ; checks to ensure that the loop condition is met before entering the loop
      ; if met, loops until loop condition is no longer met
      ; if not met, returns the current state
(define while
  (lambda (condition body states)
    (if (eval_expressions condition states)
        (while condition body (eval-statement body states))
        states)))
; ------------------------------------------------------------



; If Statement
; ------------------------------------------------------------
; if-statement: accepts the current statement as an if statement and a list of states
      ; takes in a condition where the if statement would be true, same for false
      ; if the condition is met/is true, we perform the desired operation
(define if-statement
  (lambda (condition true-condition false-condition states)
    (if (eval_expressions condition states)
        true-condition
        false-condition)))
; ------------------------------------------------------------