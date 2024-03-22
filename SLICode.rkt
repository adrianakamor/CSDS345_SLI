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
      ((null? syntax-tree) (lookup-var 'return states))
      (else 
       (eval-program (cdr syntax-tree) (eval-statement (car syntax-tree) states))))))
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
      ((eq? (car statement) 'return) (init-assign 'return (cdr statement) states))
      ((eq? (car statement) 'if)
            (if-statement (eval_expressions (cadr statement) states)
              (eval-statement (caddr statement) states k)
              (eval-statement (cadddr statement) states k)))
      ((eq? (car statement) 'while) (while (cadr statement) (caddr statement) states k))
      ((eq? (car statement) 'break) (break-helper states)) ; Temporarily removed k's to get it to run
      ((eq? (car statement) 'continue) (continue-helper states))
      ((eq? (car statement) 'throw) (throw-helper states))
      ((eq? (car statement) 'try) (try-helper states))
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
        ((symbol? expression)        (lookup-var expression state 0 0))
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

; helper for break in eval-statements for CPS and taking in break state
(define break-helper
  (lambda (state k)
    (k 'break state)))

; helper for continue in eval-statements for CPS and taking in continue state
(define continue-helper
  (lambda (state k)
    (k 'continue state)))

; helper for throw in eval-statements for CPS and taking in error state plus error
(define throw-helper
  (lambda (error state k)
    (error "Throw Error")))

; helper for try in eval statements using CPS and referencing catch and finally
(define try-helper
  (lambda (tblock cblock fblock state k)
    (eval-statement tblock state
      (lambda (v1 v2)
        (if (eq? v1 'throw)
          (catch-helper cblock v2 fblock k)
          (finally-helper fblock v2 (lambda (v3 v4) (k v3 v4))))))))

; helper to process catch from try-helper using CPS
(define catch-helper
  (lambda (cblock state fblock k)
    (eval-statement cblock state
      (lambda (v1 v2)
        (finally-helper fblock state (lambda (v3 v4) (k v3 v4)))))))

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
      ((eq? 0 (car layer-pair)) (lookup-layer (cdar state) (cadr layer-pair)))
      (else (lookup-var-helper (cdr state) (cons (- 1 (car layer-pair)) (cdr layer-pair)))))))
(define lookup-layer
  (lambda (lst index)
    (if (zero? index)
        (car lst)
        (lookup-layer (cdr lst) (- 1 index)))))

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
        (cons (cons (caar state) (list (replacer-layer (cadr layer-pair) val (cadar state)))) (cdr state))
        (cons (car state) (replacer-state (cons (- 1 (car layer-pair)) (cdr layer-pair)) val (cdr state))))))

; replacer-layer:
(define replacer-layer
  (lambda (index value lst)
  (if (= index 0)
      (cond
        ((eq? value #t) (cons 'true (cdr lst)))
        ((eq? value #f) (cons 'false (cdr lst)))
        (else (cons value (cdr lst))))
      (cons (car lst) (replacer-layer (cdr lst) (- index 1) value)))))


; ------------------------------------------------------------


; Helper Functions
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
; Will be changed for the second part of the project
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

; Maybe deprecated method
; lookup: looks up a variable's value in the state table
;(define lookup
 ; (lambda (vari states)
  ;  (lookup-helper (find-index vari states) (cadr states))))

; ------------------------------------------------------------


; While Loop
; ------------------------------------------------------------
; accepts the current statement as a while loop, and a current list of states
; checks to ensure that the loop condition is met before entering the loop
; if met, loops until loop condition is no longer met
; if not met, returns the current state
(define while
  (lambda (condition body states k)
    (if (eval_expressions condition states k)
        (while condition body (eval-statement body states k))
        states)))

(define while-cps
  (lambda (condition body states cont)
          (if eval_expressions condition states
              (eval-statement body states (lambda (new-states)
                (while-cps condition body new-states cont))) ; Recursive call in tail position with the continuation
            (cont states))))

; ------------------------------------------------------------

; If Statement
; ------------------------------------------------------------
; accepts the current statement as an if statement and a list of states
; if the condition is met/is true, we perform the desired operation
(define if-statement
  (lambda (condition true-condition false-condition states k)
    (if (eval_expressions condition states k)
        true-condition
        false-condition)))
; ------------------------------------------------------------


; Assignment 2 - Syntax Tree Statements
; ------------------------------------------------------------
; begin-block: Handles the case of a code block anywhere in the program
(define begin-block
  (lambda (block states k)
    (if (null? block)
        (cdr states)
        (begin-block (cdr block) (eval-statement (car block) states k)))))
