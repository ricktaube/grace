;
;; Function calls
;

; When you work algorithmically almost everything you do will involve
; calling functions that compute results you use to create music.  A
; 'function' is an algorithm (procedure) that accepts input values,
; performs a calculation and returns an output value back to you. To
; call a function you type its name followed by its input
; values inside (). 
; In this example the the value '128' is passed to the function
; 'random' The value returned by the random function is then printed:

(random 128)

; The values you pass to a function are called 'input arguments'.
; Function arguments are specified just after the name of the
; function. Individual arguments
; inside the list are separated by commas. We can demonstrate
; arguments using the 'list' function. In this first example we call
; the list function with zero arguments. note that in the case of zero
; arguments the surrounding () must still be provided!

(list)

; now call the list function with one argument:

(list 1)

; calling the list function with two args:

(list 1 2)

; Note that spaces before or after arguments are ignored
; and arguments can be expressions:

(list  1 2 3 
        (+ 4 5)    6 7)

; Because white spaces delimit individual input arguments,
; each argument can
; involve 'nested' expressions and function calls. Embedded
; expressions are quite common. In this example the list function is
; called with three arguments: a number, a math expression and a
; nested function call. the nested function call has 2 arguments (100
; and 200):

(list 1 (+ 2 4) (between 100 200))

; In this example the outer-most function call has two arguments, each
; is a nested function call. Each of the nested function calls have
; three arguments.

(list (list 1 2 3) (list 100 200 300))

; Note that nesting can occur to any level. Hint: you can double-click
; parenthesis to see exactly the scope of arguments they surround:

(list (list (list (list 1 2 3) 4) 5) 6)

; things can get pretty hairy if you want them too!

(list 1 (+ 2 3) (sin (+ pi (random 2.0))) (list 4 5 6))

;
;; Function parameters
;

; Functions accept their arguments (input values) via special
; variables called 'parameters'. When a function is called the
; arguments inside the argument list are processed in a left to right
; manner, each parameter in the function is assigned its corresponding
; input value. This process of associating input values with
; parameters is called 'binding'. The manner in which each parameter
; is bound to its in the argument list depends on the type of
; parameter involved. There are three basic types of parameters:
; 'required', 'optional' and 'named'.  Note that the order or
; parameters and their types are determined by the composer when they
; define the function.

; Required parameters

; A required parameter MUST be passed an argument value when the
; function is called.  For example, the predicate function 'odd?'  is
; defined to take a single required parameter:

(odd? 1)

; This means that it is an error to pass this function more or less
; than one argument when you call it. both of these examples are
; errors:

(odd?)

(odd? 1 2 3)

; Optional parameters

; An 'optional' parameter means that the argument can be supplied or
; not. If a value is not supplied then the optional parameter receives
; a 'default value' determined by the programmer when the function was
; defined. Some functions take only optional parameters, and you can
; specify as many of them as you want to. The 'list' function is an
; example of this type of function. 

; some functions support both required and optional parameters. In
; this case the required parameters must always specified first
; followed by any optional parameters.

; For example, Common Music's 'odds' function is defined with one
; required parameter and two optional ones. This means that the first
; (required) parameter must be supplied, the remaining two can be
; supplied or not. The required argument for 'odds' is a probability
; factor where 0 odds means always false, .5 odds means the function
; is true half the time, and 1 is always true. Execute each command
; several times to see the boolean value the function produces:

(odds 0)

(odds 0.5)

(odds 1)

; But 'odds' also has two optional parameters that you can pass values
; to. If specified these values will become the actual true and false
; values that the function returns:

(odds .5 "winner!")

(odds .5  "Heads"  "Tails")

;
;; Named parameters
;

; Notice that in the case of the 'odds' function, in order to specify
; the second optional value (the false value) you need to specify the
; first value as well. If there are more than one or two optionals
; this positional requirement becomes significant: to specify the last
; optional value will REQUIRE you specify all the preceding ones.  To
; get around this limitation, you can use the name feature of all
; optional parameters: to specify a specific optional argument you type
; its name with a colon at the beginning followed by the input
; value you
; want to pass it. Each pair ':name value' is then separated by a
; white space. Parameter names ALWAYS start
; with a colon and are highlighted
; in pink coloring. Since named parameters have names they can be
; specified in any order and you only have to specify the ones you
; really care about.  All the (unspecified) optional parameters will
; receive their default values as determined by the programmer.

(odds .3 :false "fooey!")

(odds .3 :false "fooey!" :true 99)