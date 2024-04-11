#lang racket
; require parser
(require "functionParser.rkt")


; ------------------------------------------------------------
; ------------------------------------------------------------

; CSDS 345 - Imperative Language Interpreter
; Group 6: Noah Henriques, Stephen Hogeman, Adriana Kamor

; ------------------------------------------------------------
; ------------------------------------------------------------

; Parser Functions
; ------------------------------------------------------------
; ------------------------------------------------------------

; Interpret Function
; interpret: interprets the file and passes it to an evaluator
(define interpret
  (lambda (filename)
    ;the program is initialized with a return statement
    ; we need to change this somehow to take into account the idea of functions instead of the the program
    ; being initialized with return or have it so that we use eval program instead
    (call/cc (lambda (k) (eval-function (parser filename) '((() ())) k null null null null)))))


; Parse Function
;  eval-program: parses the file into its syntax tree
;  how returns will be handled is that during the program, we can assume that the return function is called only once except for if/else cases
;  essentially, it will be treated the same as any other variable 'x' or 'y'
;  at the end of the syntax tree, the return value will be called from the state list and outputted
;  essentially how the states will be handled is that each statement (like var and while) will have the value it returns be the list of states
;  (formatted as ((x y...)(5 7...))

; update to be called/initialized for main function
(define eval-program
  (lambda (syntax-tree states return continue next break throw)
    (cond
      ((null? syntax-tree) return)
      ; main handling?
      ; ((eq? (car syntax-tree) 'main) (eval-statement (car syntax-tree) states return continue
                                     ; (lambda (k) (begin-block (cdr syntax-tree) k return continue next break throw)) break throw))
      (else 
       (eval-program (cdr syntax-tree) (eval-statement (car syntax-tree) states return continue
                                     (lambda (k) (begin-block (cdr syntax-tree) k return continue next break throw)) break throw) return continue next break throw)))))

; ------------------------------------------------------------
; ------------------------------------------------------------



; Expression & Statement Evaluations
; ------------------------------------------------------------
; ------------------------------------------------------------

; Outer_M_state Function:
; This function runs through the program once, defines the closures of the functions, and the global variables in the base layer of the state
(define eval-function
  (lambda (statement states return continue next break throw)
    (cond
      ((null? statement) (main-function states throw))
      ((eq? (car (car statement)) 'function) (eval-function (cdr statement) (declare-var (create-closure (car statement) states) return states throw) return continue next break throw))
      ((eq? (car (car statement)) 'var) (eval-function (cdr statement) (declare-var (car statement) return states throw) return continue next break throw))
      (else (eval-function (cdr statement) states return continue next break throw)))))

;For clarity:
; (create-closure '(function main () ((var min3 (funcall min y z x)) (if (== min1 min3) (if (== min1 min2) (if (== min2 min3) (return 1)))) (return 0))) '((() ()))) will return:
 #| (function main
              (()
               ((var min3 (funcall min y z x)) (if (== min1 min3) (if (== min1 min2) (if (== min2 min3) (return 1)))) (return 0)))
              ((() ()))
           ) |#

;this technically doesn't work because of how we structured var - theoretically, we shouldn't have to hard code inputs to var
(define create-closure
  (lambda (function-def state)
    (cons 'function (cons (cadr function-def) (append (cddr function-def) (cons state '()))))))

(define append
  (lambda (lis1 lis2)
    (if (null? lis1)
        lis2
        (cons (car lis1) (append (cdr lis1) lis2)))))

; M_state Function
; eval-statement: reads through each statement and determines how it should be treated depending on the keywords
(define eval-statement
  (lambda (statement states return continue next break throw)
    (cond
      ((null? statement) (cdr states))
      ((eq? (car statement) 'begin) (begin-block (cdr statement) (new-layer states) return continue next break throw))
      ((eq? (car statement) 'var) (declare-var statement return states throw))
      ((eq? (car statement) '=) (init-assign (cadr statement) (cddr statement) return states throw))
      ((eq? (car statement) 'return) (return (eval-expressions (cadr statement) return states throw)))
      ((eq? (car statement) 'if) (if-statement statement states return continue next break throw))
      ((eq? (car statement) 'while) (while (loop-condition statement) (loop-body statement)
                                           (call/cc (lambda (k) (while (loop-condition statement) (loop-body statement) states return k next break throw)))
                                           return continue next break throw))
      ((eq? (car statement) 'break) (break-helper states break))
      ((eq? (car statement) 'continue) (continue-helper states continue))
      ((eq? (car statement) 'throw) (throw-helper (cadr statement) states throw))
      ((eq? (car statement) 'try) (try-helper (try-body statement) (catch-block statement) (finally-block statement) states return continue next break throw))
      ; function and call function eval statements, #4 on assignment
      ; we need to initialize parameters inside function which is currently not in here
      ; change to have ability to have formal parameters passed to function
      ((eq? (car statement) 'function) (eval-statement (declare-var (create-closure (car statement) states) return states throw) return continue next break throw))
      ; change to eval expression
      ; original
      ;((eq? (car statement) 'funcall) (eval-expressions (cdr statement) states))

      ;alternative to call the function and look up its existence
      ((eq? (car statement) 'funcall) (call-func (lookup-function (cadr statement) states) (cddr statement) '() states return throw))
      (else
       (eval-statement (cdr statement) states return continue next break throw)))))

; Abstraction helpers for the eval-statement function
; loop-condition and loop-body give the cadr and the caddr of the statement for interpretation in the while-loop
(define loop-condition cadr)
(define loop-body caddr)

(define try-body cadr)
(define catch-block caddr)
(define finally-block cadddr)


; M_value Function
; eval-expressions: reads through each statement and determines which expressions are used and how those expressions should be treated
(define eval-expressions
    (lambda (expression return state throw)
      (cond
        ((number? expression)        expression)
        ((boolean? expression)       expression)
        ((eq? expression #t)     #t)
        ((eq? expression 'true)  #t)
        ((eq? expression #f)     #f)
        ((eq? expression 'false) #f)
        ((symbol? expression)        (lookup-var expression state 0 0))
        ((list? (operator expression)) expression)
        ((eq? (operator expression) '+)   (+ (eval-expressions (leftoperand expression) return state throw) (eval-expressions (rightoperand expression) return state throw)))
        ((eq? (operator expression) '-)
         (if (null? (cddr expression))
          (- (eval-expressions (leftoperand expression) return state throw))
          (- (eval-expressions (leftoperand expression) return state throw) (eval-expressions (rightoperand expression) return state throw))))
        ((eq? (operator expression) '*)   (* (eval-expressions (leftoperand expression) return state throw) (eval-expressions (rightoperand expression) return state throw)))
        ((eq? (operator expression) '/)   (quotient (eval-expressions (leftoperand expression) return state throw) (eval-expressions (rightoperand expression) return state throw)))
        ((eq? (operator expression) '%)   (remainder (eval-expressions (leftoperand expression) return state throw) (eval-expressions (rightoperand expression) return state throw)))
        ((eq? (operator expression) '==)  (eq? (eval-expressions (leftoperand expression) return state throw) (eval-expressions (rightoperand expression) return state throw)))
        ((eq? (operator expression) '!=)  (not (eq? (eval-expressions (leftoperand expression) return state throw) (eval-expressions (rightoperand expression) return state throw))))
        ((eq? (operator expression) '<)   (< (eval-expressions (leftoperand expression) return state throw) (eval-expressions (rightoperand expression) return state throw)))
        ((eq? (operator expression) '>)   (> (eval-expressions (leftoperand expression) return state throw) (eval-expressions (rightoperand expression) return state throw)))
        ((eq? (operator expression) '<=)  (<= (eval-expressions (leftoperand expression) return state throw) (eval-expressions (rightoperand expression) return state throw)))
        ((eq? (operator expression) '>=)  (>= (eval-expressions (leftoperand expression) return state throw) (eval-expressions (rightoperand expression) return state throw)))
        ((eq? (operator expression) '&&)  (and (eval-expressions (leftoperand expression) return state throw) (eval-expressions (rightoperand expression) return state throw)))
        ((eq? (operator expression) '||)  (or (eval-expressions (leftoperand expression) return state throw) (eval-expressions (rightoperand expression) return state throw)))
        ((eq? (operator expression) '!)   (not (eval-expressions (leftoperand expression) return state throw)))
        ; evaluate funcall expression, #4 on assignment
        ; eval-expressions as the environment won't work
        ((eq? (car expression) 'funcall) (eval_bindings (lookup-var (cadr expression) state 0 0) (caddr (lookup-var (cadr expression) state 0 0)) (cddr expression) return throw state))
        (else (error "Type Unknown")))))

; Helper methods for eval-expressions for abstraction
(define operator car)
(define leftoperand cadr)
(define rightoperand caddr)

; ------------------------------------------------------------
; ------------------------------------------------------------


; Assignment 3-Specific Helper Functions
; ------------------------------------------------------------
; ------------------------------------------------------------

; call-func holds a new definition here to call functions, given the closure defined
; The closure defines the formal parameters, the body, and the bindings in scope of the function

; NOTE:
; To initialize parameters, should we add formal_params in lambda for newenvironment and eval_bindings passed form call-func?

; Need to add formal_params to the states list along with their 
(define call-func
  (lambda (formal_params body bindings states return throw)
    (call/cc (lambda (k) (eval-program body (newenvironment states bindings) k '() '() '() throw)))))

; newenvironment should create a new environment for the function to act upon based on the bindings in scope
; Does this mean that the environment created for the function is attached to the state we already have, or is it its own separate entity?
(define newenvironment
  (lambda (states bindings)
    (eval_bindings (new-layer states) bindings '() '() '())))

; eval_bindings identifies the formal parameters passed to the function, evaluates the actual parameters given, and binds them
(define eval_bindings
  (lambda (function-closure environment actual-params return throw states)
    (cond
      ((xor (null? (formal-params function-closure)) (null? actual-params)) (error "Mismatching number of parameters!"))
      ((null? (formal-params function-closure)) (eval-program (extract-function function-closure) environment return '() '() '() throw))
      ; cadr bindings: func name, caddr bindings: func parameters, cdddr bindings: func body
      ; should this else lookup the variable as well? Or is this redundant
      ;(else (declare-var (cons (cadr (bindings function-closure)) (cons (cons (cadr (bindings function-closure)) (caddr (bindings function-closure))) states)) states throw)))))

      ;new else, sets the bindings based on the input parameters, and then calls the function
      (else (call/cc (lambda (k) (eval-program (extract-function function-closure) (set-bindings (formal-params function-closure) actual-params return (new-layer states) throw) k '() '() '() throw)))))))

(define extract-function cadr)
(define formal-params car)

; set-bindings: takes an input list of formal parameters for a function, and binds variables in the environment
;               of that function

(define set-bindings
  (lambda (formal-params actual-params return environment throw)
    (cond
      ((and (null? formal-params) (null? actual-params)) environment)
      (else (set-bindings (cdr formal-params) (cdr actual-params)
                                                                   return (declare-var (variable-pair (car formal-params) (car actual-params) return environment throw) return environment throw) throw)))))

; variable-pair: Helper function, gives a simple variable definition to be passed to declare-var in set-bindings
(define variable-pair
  (lambda (var-name value return environment throw)
    (cons 'var (cons var-name (cons (eval-expressions value return environment throw) '())))))

; update-global: a helper variable that takes the changed environment of a funciton, and updates any variables necessary


; main-function to take in main function
;(lookup-var expression state 0 0)
(define main-function
  (lambda (environment throw)
    (cond
      ((null? (lookup-var 'main environment 0 0)) error "No main function")
      (else (call/cc (lambda (k) (eval_bindings (lookup-var 'main environment 0 0) (caddr (lookup-var 'main environment 0 0)) '() k throw environment)))))))

; atom helper function since atom? got used in the outer M_state
(define atom?
  (lambda (x)
    (not (pair? x))))
      

; ------------------------------------------------------------
; ------------------------------------------------------------


; Assignment 2-specific Helper Functions
; ------------------------------------------------------------
; ------------------------------------------------------------

; Go To Statements
; ------------------------------------------------------------

; Break
; ------------------------------------------------------------

; helper for break in eval-statements for CPS and taking in break state
(define break-helper
  (lambda (state break)
    (if (null? break)
        (error "Break is not in a loop!")
        (break state))))


; helper for break for determining if break statement is inside a loop or not
(define in-loop
  (lambda (states)
    (if (null? states)
        #f
        (or (eq? (caar states) 'while) (in-loop (cdr states))))))

(define loop-check
  (lambda (statement)
    (cond
      ((eq? (car statement) 'while) #t)
      ((eq? (car statement) 'begin) (loop-block (cdr statement)))
      (else #f))))

(define loop-block
  (lambda (block)
    (if (null? block)
        #f
        (or (loop-check (car block)) (loop-block (cdr block))))))
; ------------------------------------------------------------

; Continue
; helper for continue in eval-statements for CPS and taking in continue state
(define continue-helper
  (lambda (state k)
    (k state)))

; Throw
; helper for throw in eval-statements for CPS and taking in error state plus error
(define throw-helper
  (lambda (error state throw)
    (throw state error)))

; ------------------------------------------------------------



; Try Catch Statements
; ------------------------------------------------------------

; helper for try in eval statements using CPS and referencing catch and finally
(define try-helper
  (lambda (tblock cblock fblock states return continue next break throw)
    (begin-block tblock (new-layer states)
      ;return
      return
    
      ;continue
      (lambda (s) (finally-helper fblock (remove-top-layer s) return continue next break throw))

      ;next
      (lambda (s) (finally-helper fblock (remove-top-layer s) return continue next break throw))

      ;break
      (lambda (s) (finally-helper fblock (remove-top-layer s) return continue break break throw))

      ;throw
      (lambda (s e) (catch-helper cblock e (remove-top-layer s) return continue
                                  (lambda (n) (finally-helper fblock (remove-top-layer n) return continue next break throw))
                                    break throw)))))

; helper to process catch from try-helper using CPS
(define catch-helper
  (lambda (cblock error states return continue next break throw)
    (if (null? cblock)
        (next states)
        (begin-block (caddr cblock) (declare-var (cons 'var (list (caadr cblock) error)) return (new-layer states) throw) return continue next break throw))))

; helper to process finally from try-helper using CPS
(define finally-helper
  (lambda (fblock states return continue next break throw)
    (if (null? fblock)
        (next states)
        (begin-block (cadr fblock) (new-layer states) return continue next break throw))))

; ------------------------------------------------------------

; Layer methods: Help add and remove the top layer of the state
; ------------------------------------------------------------

; remove-top-layer: Removes the top layer of the state
(define remove-top-layer
  (lambda (state)
    (cdr state)))

; new-layer: adds an empty layer of (() ()) to the front of the current state, for the current block of code
(define new-layer (lambda (states) (cons '(() ()) states)))


; ------------------------------------------------------------

; Lookup Functions
; ------------------------------------------------------------

; lookup-var: Looking up the layer and index of the desired variable
; Returns either a pair detailing the layer and index of the desired variable, or -1 to signify that the variable wasn't found in the state
(define lookup-var
  (lambda (var states layer index)
    (if (eq? -1 (state-find var states 0))
        (error "Variable requested not found in the state!")
        (lookup-var-helper states (state-find var states 0)))))

; lookup-var-helper traverses through all layers of the state, outsourcing to lookup-layer for each individual layer
(define lookup-var-helper
  (lambda (state layer-pair)
    (cond
      ((eq? 0 (car layer-pair)) (lookup-layer (cadar state) (cadr layer-pair)))
      (else (lookup-var-helper (cdr state) (cons (- (car layer-pair) 1) (cdr layer-pair)))))))

; Lookup-layer: Helper to lookup-var-helper, traverses through the current portion of the state
(define lookup-layer
  (lambda (lst index)
    (if (zero? index)
        (car lst)
        (lookup-layer (cdr lst) (- index 1)))))

; don't think we need this if we treat functions as variables
; lookup-function to find a called function
(define lookup-function
  (lambda (name environment)
    (cond
      ((null? environment) #f)
      ((equal? (caar environment) name) (cdar environment))
      (else (lookup-function name (cdr environment))))))

; ------------------------------------------------------------



; Find Functions
; ------------------------------------------------------------

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
    (loop-layer lst x 0)))

; look: Helper to layer-find, loops through current portion of state to return the position of desired variable x
(define loop-layer
  (lambda (lst x index)
    (cond
      ((null? lst) -1)
      ((eq? (car lst) x) index)
      (else (loop-layer (cdr lst) x (+ index 1))))))

; ------------------------------------------------------------



; Replace Functions
; ------------------------------------------------------------



; add to replacer-state return




; replacer-state: Traverses the layers of the state, once it arrives at the layer to be altered, it goes to replacer-helper to alter that layer
(define replacer-state
  (lambda (layer-pair val return state throw)
    (if (zero? (car layer-pair))
        (cons (cons (caar state) (list (replacer-layer state (cadr layer-pair) val (cadar state) return throw))) (cdr state))
        (cons (car state) (replacer-state (cons (- (car layer-pair) 1) (cdr layer-pair)) val return (cdr state) throw)))))

; replacer-layer: Helper to replacer-state, changes the value of the variable at index
(define replacer-layer
  (lambda (state index value lst return throw)
    (if (= index 0)
        (cond
          ((eq? (eval-expressions value return state throw)  #t) (cons 'true (cdr lst)))
          ((eq? (eval-expressions value return state throw)  #f) (cons 'false (cdr lst)))
          (else (cons (eval-expressions value return state throw) (cdr lst))))
      (cons (car lst) (replacer-layer state (- index 1) value (cdr lst) return throw)))))
; ------------------------------------------------------------


; ------------------------------------------------------------
; ------------------------------------------------------------



; Assignment 1 Specific Helper Functions
; ------------------------------------------------------------
; ------------------------------------------------------------

; add to update-states

; update-layers: Updates the state for the given val
(define update-states
  (lambda (vari val return states throw)
    (replacer-state (state-find vari states 0) val return states throw)))

; create-pair: inserts a variable and value into the state given the variable expression and the state
(define create-pair
  (lambda (y x value)
    (cons (cons y (car x))
          (cons (cons value (cadr x)) '()))))

; ------------------------------------------------------------
; ------------------------------------------------------------



; Syntax Tree Statements
; ------------------------------------------------------------
; ------------------------------------------------------------

; declare-var: accepts the current statement (which will have a var in it) and the current list of states ((...)(...))
;  will detect if there is an equals sign after this, in which case it will call the assign function
;  cadr is the variable of the statement
;  cdddr is the expression of the statement
;  returns the updated state with a new variable
;  checks if there's an equals sign/anything after the variable
(define declare-var
  (lambda (statement return states throw)
    (cond ((null? (cadr statement)) (error "Error in var statement!"))
          ((eq? (car statement) 'function) (init-assign (cadr statement) (cddr statement) return (cons (create-pair (cadr statement) (car states) '()) (cdr states)) throw))
          (else
           (if (not (null? (cddr statement)))
               (init-assign (cadr statement) (cddr statement) return (cons (create-pair (cadr statement) (car states) '()) (cdr states)) throw)
               (cons (create-pair (cadr statement) (car states) '()) (cdr states)))))))
        
; init-assign: accepts the variable in question, an expression, and the current list of states ((...)(...))
;  checks to make sure that the variable has been declared; if not (idk what it'll do lol)
;  will take the value on the right of the = and first pass it into an expression function
;  will then take the result of the expression function and assign it to the variable on the left of the =
;  will return the updated state with an updated value for the var in question
;I did think of one problem; do we have to add return and other things to when we store the function?
(define init-assign
  (lambda (vari expression return states throw)
    (cond
      ((null? expression) (error "Error in var statement!"))
      ((is-function? expression) (update-states vari (eval-expressions expression return states throw) return states throw))
      (else (update-states vari (eval-expressions (car expression) return states throw) return states throw)))))

(define is-function?
  (lambda (expression)
    (if (null? (cdr expression))
        #f
        #t)))

; ------------------------------------------------------------
; ------------------------------------------------------------



; While Loop
; ------------------------------------------------------------
; ------------------------------------------------------------

; While
;  accepts the current statement as a while loop, and a current list of states
;  checks to ensure that the loop condition is met before entering the loop
;  if met, loops until loop condition is no longer met
;  if not met, returns the current state
(define while
  (lambda (condition body states return continue next break throw)
    (loop condition body states return
          (lambda (c) (while condition body (remove-top-layer c) return continue next break throw))
          next
          (lambda (s) (next (remove-top-layer s))) throw)))

(define loop
  (lambda (condition body states return continue next break throw)
    (if (eval-expressions condition return states throw)
        (loop condition body (eval-statement body states return continue
                                             (lambda (n) (while condition body (remove-top-layer n) return continue next break throw))
                                             break throw)
              return continue next break throw)
        (next states))))

; ------------------------------------------------------------
; ------------------------------------------------------------



; If Statement
; ------------------------------------------------------------
; ------------------------------------------------------------

; If
;  accepts the current statement as an if statement and a list of states
;  if the condition is met/is true, we perform the desired operation
(define if-statement
  (lambda (statement states return continue next break throw)
    (cond
      ((eval-expressions (condition statement) return states throw) (eval-statement (true-statement statement) states return continue
                                                                       (lambda (n) (next (remove-top-layer n))) break throw))
      ((not (null? (second-condition statement))) (eval-statement (false-statement statement) states return continue
                                                                  (lambda (n) (next (remove-top-layer n))) break throw))
      (else states))))

; definition helpers for if
(define second-condition cdddr)
(define condition cadr)
(define true-statement caddr)
(define false-statement cadddr)

; ------------------------------------------------------------
; ------------------------------------------------------------



; Begin Statement
; ------------------------------------------------------------
; ------------------------------------------------------------

; begin-block: Handles the case of a code block anywhere in the program
; With the added state, it manipulates the state and takes the top layer of the state off when it exits the block
(define begin-block
  (lambda (block states return continue next break throw)
    (if (null? block)
        (next states)
        (begin-block (cdr block)
                     (eval-statement (car block) states return continue
                                     (lambda (k) (begin-block (cdr block) k return continue next break throw)) break throw) return continue next break throw))))

; ------------------------------------------------------------
; ------------------------------------------------------------