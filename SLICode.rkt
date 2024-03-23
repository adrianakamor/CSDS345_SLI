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
      ((null? syntax-tree) (lookup-var 'return states 0 0))
      (else 
       (eval-program (cdr syntax-tree) (eval-statement (car syntax-tree) states (lambda (v) v)))))))
; Error here, eval-statement now takes three arguments, change?
; ------------------------------------------------------------


; Expression & Statement Evaluations
; ------------------------------------------------------------
; M_state Function
; eval-statement: reads through each statement and determines how it should be treated depending on the keywords
(define eval-statement
  (lambda (statement states k)
    (cond
      ((null? statement) (cdr states))
      ((list? (car statement)) null)
      ((eq? (car statement) 'begin) (begin-block (cdr statement) (new-layer states) k))
      ((eq? (car statement) 'var) (declare-var statement states))
      ((eq? (car statement) '=) (init-assign (cadr statement) (cddr statement) states))
      ((eq? (car statement) 'return) (k (init-assign 'return (cdr statement) states)))
      ((eq? (car statement) 'if)
       (if-statement (eval-expressions (cadr statement) states)
                     (eval-statement (caddr statement) states k) states))
      ((eq? (car statement) 'while) (while (cadr statement) (caddr statement) states k))
      ((eq? (car statement) 'break) (if (in-loop states)
                                        (break-helper states k)
                                        (error "break is not in a loop")))
      ((eq? (car statement) 'continue) (continue-helper states k))
      ((eq? (car statement) 'throw) (throw-helper (cadr statement) states k))
      ((eq? (car statement) 'try) (try-helper (cadr statement) (caddr statement) (cadddr statement) states k))
      (else
       (eval-statement (cdr statement) states k)))))

; M_value Function
; eval-expressions: reads through each statement and determines which expressions are used and how those expressions should be treated
; Bugs here in that only the first value of any expression is returned opposed to the result of the expression
(define eval-expressions
    (lambda (expression state)
      (cond
        ((number? expression)        expression)
        ((boolean? expression)       expression)
        ((eq? expression #t)     #t)
        ((eq? expression 'true)  #t)
        ((eq? expression #f)     #f)
        ((eq? expression 'false) #f)
        ((symbol? expression)        (lookup-var expression state 0 0))
        ((eq? (operator expression) '+)   (+ (eval-expressions (leftoperand expression) state) (eval-expressions (rightoperand expression) state)))
        ((eq? (operator expression) '-)
         (if (null? (cddr expression))
          (- (eval-expressions (leftoperand expression) state))
          (- (eval-expressions (leftoperand expression) state) (eval-expressions (rightoperand expression) state))))
        ((eq? (operator expression) '*)   (* (eval-expressions (leftoperand expression) state) (eval-expressions (rightoperand expression) state)))
        ((eq? (operator expression) '/)   (quotient (eval-expressions (leftoperand expression) state) (eval-expressions (rightoperand expression) state)))
        ((eq? (operator expression) '%)   (remainder (eval-expressions (leftoperand expression) state) (eval-expressions (rightoperand expression) state)))
        ((eq? (operator expression) '==)  (eq? (eval-expressions (leftoperand expression) state) (eval-expressions (rightoperand expression) state)))
        ((eq? (operator expression) '!=)  (not (eq? (eval-expressions (leftoperand expression) state) (eval-expressions (rightoperand expression) state))))
        ((eq? (operator expression) '<)   (< (eval-expressions (leftoperand expression) state) (eval-expressions (rightoperand expression) state)))
        ((eq? (operator expression) '>)   (> (eval-expressions (leftoperand expression) state) (eval-expressions (rightoperand expression) state)))
        ((eq? (operator expression) '<=)  (<= (eval-expressions (leftoperand expression) state) (eval-expressions (rightoperand expression) state)))
        ((eq? (operator expression) '>=)  (>= (eval-expressions (leftoperand expression) state) (eval-expressions (rightoperand expression) state)))
        ((eq? (operator expression) '&&)  (and (eval-expressions (leftoperand expression) state) (eval-expressions (rightoperand expression) state)))
        ((eq? (operator expression) '||)  (or (eval-expressions (leftoperand expression) state) (eval-expressions (rightoperand expression) state)))
        ((eq? (operator expression) '!)   (not (eval-expressions (leftoperand expression) state)))
        (else (error "Type Unknown")))))


; Helper methods for eval-expressions for abstraction
(define operator car)
(define leftoperand cadr)
(define rightoperand caddr)

; ------------------------------------------------------------

; Assignment 2-specific Helper Functions
; ------------------------------------------------------------

; helper for break in eval-statements for CPS and taking in break state
(define break-helper
  (lambda (state k)
    (k state)))


; helper for break for determining if break statement is inside a loop or not
(define in-loop
  (lambda (states)
    (if (null? states)
        #f
        (or (eq? (caar states) 'while) (in-loop (cdr states))))))

; helper for continue in eval-statements for CPS and taking in continue state
(define continue-helper
  (lambda (state k)
    (k state)))

; helper for throw in eval-statements for CPS and taking in error state plus error
(define throw-helper
  (lambda (error state k)
    (k (list 'error error) state)))

; helper for try in eval statements using CPS and referencing catch and finally
(define try-helper
  (lambda (tblock cblock fblock state k)
    (eval-statement tblock state
      (lambda (v1 v2)
        (if (eq? v1 'throw)
          (catch-helper cblock v2 fblock state k)
          (finally-helper fblock v2 state k))))))

; helper to process catch from try-helper using CPS
(define catch-helper
  (lambda (cblock error state fblock k)
    (eval-statement cblock state
      (lambda (v1 v2)
        (finally-helper fblock state k)))))

; helper to process finally from try-helper using CPS
(define finally-helper
  (lambda (fblock state k)
    (if (null? fblock)
        (k '() state)
        (eval-statement fblock state k))))

; new-layer: adds an empty layer of (() ()) to the front of the current state, for the current block of code
(define new-layer (lambda (state) (cons '(() ()) state)))

; lookup-var: Looking up the layer and index of the desired variable
; Returns either a pair detailing the layer and index of the desired variable, or -1 to signify that the variable wasn't found in the state
(define lookup-var
  (lambda (var state layer index)
    (if (eq? -1 (state-find var state 0))
        (error "Variable requested not found in the state!")
        (lookup-var-helper state (state-find var state 0)))))

; lookup-var-helper traverses through the layers of the state, and stops when 0
(define lookup-var-helper
  (lambda (state layer-pair)
    (cond
      ((eq? 0 (car layer-pair)) (lookup-layer (cadar state) (cadr layer-pair)))
      (else (lookup-var-helper (cdr state) (cons (- (car layer-pair) 1) (cdr layer-pair)))))))

(define lookup-layer
  (lambda (lst index)
    (if (zero? index)
        (car lst)
        (lookup-layer (cdr lst) (- index 1)))))

; state-find: Looking up the layer and the index in said layer for the desired variable
(define state-find
  (lambda (var state layer)
    (cond
      ((null? state) -1)
      ((eq? -1 (layer-find var (caar state))) (state-find var (cdr state) (+ 1 layer)))
      (else (cons layer (cons (layer-find var (caar state)) '()))))))
; layer-find: function + helper that takes the states and a given variable, and finds the index of that variable in the first part of states
; Returns -1 if the variable requested is not found in the current layer of state
(define layer-find
  (lambda (x lst)
    (loop lst x 0)))

(define loop
  (lambda (lst x index)
    (cond
      ((null? lst) -1)
      ((eq? (car lst) x) index)
      (else (loop (cdr lst) x (+ index 1))))))

; replacer-state:
(define replacer-state
  (lambda (layer-pair val state)
    (if (zero? (car layer-pair))
        (cons (cons (caar state) (list (replacer-layer state (cadr layer-pair) val (cadar state)))) (cdr state))
        (cons (car state) (replacer-state (cons (- (car layer-pair) 1) (cdr layer-pair)) val (cdr state))))))

; replacer-layer:
(define replacer-layer
  (lambda (state index value lst)
    (if (= index 0)
        (cond
          ((eq? (eval-expressions value state)  #t) (cons 'true (cdr lst)))
          ((eq? (eval-expressions value state)  #f) (cons 'false (cdr lst)))
          (else (cons (eval-expressions value state) (cdr lst))))
      (cons (car lst) (replacer-layer state (- index 1) value (cdr lst)))))) ; should this be (- index 1) value (cdr lst) or am I being silly


; ------------------------------------------------------------


; Assignment 1 Specific Helper Functions
; ------------------------------------------------------------

; append: Implementation of the append function from class
(define append
  (lambda (lis1 lis2)
    (if (null? lis1)
        lis2
        (cons (car lis1) (append(cdr lis1) lis2)))))

; update-layers: function that updates the states given a variable and a value
(define update-states
  (lambda (vari val states)
    (replacer-state (state-find vari states 0) val states)))

; create-pair: inserts a variable and value into the state given the variable expression and the state
(define create-pair
  (lambda (y x value)
    (cons (cons y (car x))
          (cons (cons value (cadr x)) '()))))

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
               (init-assign (cadr statement) (cddr statement) (cons (create-pair (cadr statement) (car states) '()) (cdr states)))
               (create-pair (cadr statement) states '()))))))
        
; init-assign: accepts the variable in question, an expression, and the current list of states ((...)(...))
;checks to make sure that the variable has been declared; if not (idk what it'll do lol)
;will take the value on the right of the = and first pass it into an expression function
;will then take the result of the expression function and assign it to the variable on the left of the =
;will return the updated state with an updated value for the var in question
(define init-assign
  (lambda (vari expression states)
    (if (null? expression)
        (error "Error in var statement!")
        (update-states vari (eval-expressions (car expression) states) states))))
;Maybe an error with this continuation?

; ------------------------------------------------------------


; While Loop
; ------------------------------------------------------------
; accepts the current statement as a while loop, and a current list of states
; checks to ensure that the loop condition is met before entering the loop
; if met, loops until loop condition is no longer met
; if not met, returns the current state
(define while
  (lambda (condition body states k)
    (if (eval-expressions condition states)
        (eval-statement body states (lambda (v) (while condition body v k)))
        (k states))))

; ------------------------------------------------------------

; If Statement
; ------------------------------------------------------------
; accepts the current statement as an if statement and a list of states
; if the condition is met/is true, we perform the desired operation
(define if-statement
  (lambda (condition true-condition states)
    (if (eval-expressions condition states)
        true-condition
        states)))
; ------------------------------------------------------------


; Begin Statement
; ------------------------------------------------------------
; begin-block: Handles the case of a code block anywhere in the program
; With the added state, it manipulates the state and takes the top layer of the state off when it exits the block
(define begin-block
  (lambda (block states k)
    (if (null? block)
        (cdr states)
        (begin-block (cdr block) (eval-statement (car block) states k) k))))