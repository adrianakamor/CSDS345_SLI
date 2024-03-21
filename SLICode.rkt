#lang racket
; require parser
(require "simpleParser.rkt")


; ------------------------------------------------------------
; CSDS 345 - Flow Control Interpreter
; Group 3: Noah Henriques, Stephen Hogeman, Adriana Kamor
; ------------------------------------------------------------


; Parser Functions
; ------------------------------------------------------------
; Interpret Function
; interpret: interprets the file and passes it to an evaluator
(define interpret
  (lambda (filename)
    ;the program is initialized with a return statement
    (eval-program (parser filename) '(((return) (()))))))

;Parse Function
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
; Error here, eval-statement now takes three arguments, change?
; ------------------------------------------------------------


; Expression & Statement Evaluations
; ------------------------------------------------------------
; M_state Function
; eval-statement: reads through each statement and determines how it should be treated depending on the keywords
(define eval-statement
  (lambda (statement states)
    (cond
      ((null? statement) (cdr states))
      ((list? (car statement)) null)
      ((eq? (car statement) 'begin) (begin-block (cdr statement) (new-layer states)))
      ((eq? (car statement) 'var) (declare-var statement states))
      ((eq? (car statement) '=) (init-assign (cadr statement) (cddr statement) states))
      ((eq? (car statement) 'return) (init-assign 'return (cdr statement) states))
      ((eq? (car statement) 'if)
        (eval_expressions (cadr statement) states (lambda (v)
            (if-statement v
              (eval-statement (caddr statement) states)
              (eval-statement (cadddr statement) states)))))
      ((eq? (car statement) 'while) (while (cadr statement) (caddr statement) states))
      ; I put a section for helpers below this section, but am not sure if we want to use
      ; helpers or not for these. 
      ; ((eq? (car statement) 'break) (break-helper states k))
      ; ((eq? (car statement) 'continue) (continue-helper states k))
      ; ((eq? (car statement) 'throw) (throw-helper states k))
      (else
       (eval-statement (cdr statement) states)))))

; M_value Function
; eval_expressions: reads through each statement and determines which expressions are used and how those expressions should be treated
  (define eval_expressions
    (lambda (expression state)
      (cond
        ((number? expression)        expression)
        ((boolean? expression)       expression)
        ((eq? expression #t)     #t)
        ((eq? expression 'true)  #t)
        ((eq? expression #f)     #f)
        ((eq? expression 'false) #f)
        ((symbol? expression)        (lookup expression state))
        ((eq? (operator expression) '+)   (+ (eval_expressions (leftoperand expression) state) (eval_expressions (rightoperand expression) state)))
        ((eq? (operator expression) '-)
         (if (null? (cddr expression))
          (- (eval_expressions (leftoperand expression) state))
          (- (eval_expressions (leftoperand expression) state) (eval_expressions (rightoperand expression) state))))
        ((eq? (operator expression) '*)   (* (eval_expressions (leftoperand expression) state) (eval_expressions (rightoperand expression) state)))
        ((eq? (operator expression) '/)   (quotient (eval_expressions (leftoperand expression) state) (eval_expressions (rightoperand expression) state)))
        ((eq? (operator expression) '%)   (remainder (eval_expressions (leftoperand expression) state) (eval_expressions (rightoperand expression) state)))
        ((eq? (operator expression) '==)  (eq? (eval_expressions (leftoperand expression) state) (eval_expressions (rightoperand expression) state)))
        ((eq? (operator expression) '!=)  (not (eq? (eval_expressions (leftoperand expression) state) (eval_expressions (rightoperand expression) state))))
        ((eq? (operator expression) '<)   (< (eval_expressions (leftoperand expression) state) (eval_expressions (rightoperand expression) state)))
        ((eq? (operator expression) '>)   (> (eval_expressions (leftoperand expression) state) (eval_expressions (rightoperand expression) state)))
        ((eq? (operator expression) '<=)  (<= (eval_expressions (leftoperand expression) state) (eval_expressions (rightoperand expression) state)))
        ((eq? (operator expression) '>=)  (>= (eval_expressions (leftoperand expression) state) (eval_expressions (rightoperand expression) state)))
        ((eq? (operator expression) '&&)  (and (eval_expressions (leftoperand expression) state) (eval_expressions (rightoperand expression) state)))
        ((eq? (operator expression) '||)  (or (eval_expressions (leftoperand expression) state) (eval_expressions (rightoperand expression) state)))
        ((eq? (operator expression) '!)   (not (eval_expressions (leftoperand expression) state)))
        (else (error "Type Unknown")))))

; Helper methods for eval-expressions for abstraction
(define operator car)
(define leftoperand cadr)
(define rightoperand caddr)

; ------------------------------------------------------------

; Assignment 2-specific Helper Functions
; ------------------------------------------------------------
;(define break-helper
  ;(lambda (state k)))

;(define continue-helper
  ;(lambda (state k)))

;(define throw-helper
  ;(lambda (state k)))

; new-layer: adds an empty layer of (() ()) to the front of the current state, for the current block of code
(define new-layer (lambda (state) (cons '(() ()) state)))

; The general logic for traversing through the layers is to:
;  A. Check the current layer to see if the desired value is there
;  B. If there is the desired value, return both the layer number and index number of value?
;  C. If not, go through next layer, incrementing counter of layer.
; lookup-state: 
(define lookup-state
  (lambda (var state layer index)
    (if (eq? -1 (lookup-layer (var (caar state) 0)))
        (cons (car state) (lookup-state var (cdr state) (+ 1 layer) index))
        layer)))

; lookup-layer:
(define lookup-layer
  (lambda (var layer-state layer)
    (if (null? layer-state)
        -1
        0)))
        

; replacer-layer:

; replacer-state:


; ------------------------------------------------------------


; Helper Functions
; ------------------------------------------------------------

; update-states: function that updates the states given a variable and a value
(define update-states
  (lambda (vari val states)
    (replacer (find-index vari (list (car states))) val states)))

;find-index: function + helper that takes the states and a given variable, and finds the index of that variable in the first part of states
; Needs to be greatly modified or discarded for part 2, as this won't be helpful since we have multiple layers
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
  (lambda (layer index value lst)
  (list (car lst) (replacer-helper (cadr lst) layer index value))))
(define replacer-helper
  (lambda (state layer index value)
  (if (= index 0)
      (cond
        ((eq? value #t) (cons 'true (cdr state)))
        ((eq? value #f) (cons 'false (cdr state)))
        (else (cons value (cdr state))))
      (cons (car state) (replacer-helper (cdr state) (- index 1) value)))))

; lookup-helper: helper that takes an index and iterates through the second part of the states until it lands on that index,
; returning that value
(define lookup-helper
  (lambda (index lst)
    (if (= index 0)
        (car lst)
        (lookup-helper (- index 1) (cdr lst)))))

; create-pair: inserts a variable and value into the state given the variable expression and the state
(define create-pair
  (lambda (var state value)
  (cons (cons var (caar state))
        (cons (cons value (caadr state)) '()))))

; ------------------------------------------------------------


; Syntax Tree Statements
; ------------------------------------------------------------

; declare-var: accepts the current statement (which will have a var in it) and the current list of states ((...)(...))
;will detect if there is an equals sign after this, in which case it will call the assign function
;cadr is the variable of the statement
;cdddr is the expression of the statement
;returns the updated state with a new variable
;checks if there's an equals sign/anything after the variable
(define declare-var
  (lambda (statement states)
    (cond ((null? (cadr statement)) (error "Error in var statement!"))
        (else
         (if (not (null? (cddr statement)))
             (init-assign (cadr statement) (cddr statement) (create-pair (cadr statement) states '()))
             (create-pair (cadr statement) states '()))))))
; By changing it such that a pair is only created in the first layer of the state, then this should prevent variables being declared outside of their
; scope. 
        
; init-assign: accepts the variable in question, an expression, and the current list of states ((...)(...))
;checks to make sure that the variable has been declared; if not (idk what it'll do lol)
;will take the value on the right of the = and first pass it into an expression function
;will then take the result of the expression function and assign it to the variable on the left of the =
;will return the updated state with an updated value for the var in question
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
; accepts the current statement as a while loop, and a current list of states
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
; accepts the current statement as an if statement and a list of states
; if the condition is met/is true, we perform the desired operation
(define if-statement
  (lambda (condition true-condition false-condition states)
    (if (eval_expressions condition states)
        true-condition
        false-condition)))
; ------------------------------------------------------------


; Assignment 2 - Syntax Tree Statements
; ------------------------------------------------------------
; begin-block: Handles the case of a code block anywhere in the program
(define begin-block
  (lambda (block states)
    (if (null? block)
        (cdr states)
        (begin-block (cdr block) (eval-statement (car block) states)))))