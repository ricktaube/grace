;
;; Defining Variables
; 

; To run examples put the cursor at the end of each expression and
; press Command-Return. Look in the console window for any output.

; A variable is a symbol that evalutates to a value. There are two
; kinds of variables: top-level and local. Top-level variables are
; declared using the 'define' command at the top-level of your
; program.  Top-level variables are visible to all code you define
; within your program.

; Internal variables are declared using let and let*.  More about
; these kinds of variables in just a bit...

;
;; Top-level Variables
;

; A top-level variable is a variable declared in the top-level so that
; all your code has access to it. For example, if you are working on a
; twelve-tone composition you might want to create a top-level
; variable to hold the row on which the composition is based. That way
; all the code you develop has access to this imporant piece of data.

(define row '(0 11 1 10 2 9 3 8 4 7 5 6))

(print "Prime row: " row ", retograde row: " (reverse row))

; The define command takes the name of the variable, followed by a
; value initialization. The initialization can be any expression. In
; this next example we define two top-level variables, the first
; variable holds a row type we randomly choose from four possible row
; types and the second hold a transposition offset:

(define row-form (pick :p :r :i :ri))
(define row-transp (between 0 12))
  
;
;; Local Variables
;

; A local variable exists only within the specific block of code in
; which it is declared. To create a local variable use let or
; let*. The difference between these two forms is that in let* binds
; its variables in sequential order so that later variables can depend
; on the values of earlier ones. This example uses let to declare two
; variables, x and y because the y value depends on the value of x.

(let* ((x (random 12))
       (y (+ x 12)))
  (list x y))

; Notice that if you try to evaluate x or y OUTSIDE the block an error
; occurs because the variables exist only within the block in which
; they were declared. Executing this next statement will trigger an
; error because x does not exist at the top-level.

x

; A variable local in an inner block will shadow a variable with the
; same name declared in an outer block. In this example, both blocks
; use a variable 'x' but the print statements clearly show that they
; are different variables!

(let ((x 1))
  (print "outer block, x=" x)
  (let ((x 2))
    (print "inner block, x=" x))
  (print "outer block, x=" x)
  )

;
;; Variable assignment: the 'set!' command
;

; How can you assign a differnt value to an existing variable?  For
; global variables, one solution would be to re-execute the variable
; command but use a different inital value. Another way is to ASSIGN
; the variable a new value using the SET command.

(begin
  (define row '(11 10 9 8 7 6 5 4 3 2 1 0))
  (define row-form :p)
  (define row-transp 0)
  (print "row=" row " row-form=" row-form " row-transp=" row-transp)
  )

(begin
  (set! row '(0 1 2 3 4 5 6 7 8 9 10 11))
  (set! row-form :r)
  (set! row-transp (between 6 12))
  (print "row=" row " row-form=" row-form " row-transp=" row-transp)
  )

; Here is an example of some common set! operation in action. Each
; example uses 'set!' inside a loop that you can see the effect when
; it repeats several times. More about the loop statement in another
; tutorial!

; Increment a variable by a value:

(let ((var 0))
  (loop repeat 5 do (set! var (+ var 1 )))
  var
  )

; Scale a variable by a value

(let ((var 1))
  (loop repeat 5 do (set! var (* var 10)))
  var
  )

; Add a value to the end of a variable

(let ((var '()))
  (loop repeat 5 do (set! var (append var (list (random 128)))))
  var
  )

; Append a list to the end of a variable

(let ((var '()))
  (loop repeat 5 do (set! var (append var (list 1 2 (random 128)))))
  var
  )

; Add a value to the front of a variable

(let ((var '()))
  (loop repeat 5 do (set! var (cons (random 128) var)))
  var
  )

; Minimize a value

(let ((var 128))
  (loop repeat 5 do (set! var (min var (random 128) )))
  var
  )

; Maximizes a value

(let ((var -1))
  (loop repeat 5 do (set! var (max var (random 128))))
  var
  )

