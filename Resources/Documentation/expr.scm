;
;; Expressions and Evaluation
;

; To run code put your cursor after each expression and press
; Command-Return, then check the console window for any output.

; An expression is anything that produces a value. Expressions
; commonly consist of numbers, booleans, strings, symbols (variables
; and function names), lists and function calls.

; An expression becomes a value through a process called evaluation,
; every time you press Command-Return you trigger evaluation.

(+ 1 (* 2 3))

;
;; Integers 
;

; Integers are whole numbers like 1, 2, 3, -6 and so on with digits
; optionally preceded by a + or - sign. Integers are ubiquitous in
; computer composition, they are commonly used as values, counters,
; and indexes. An index describes a position (location) in a list or
; vector that holds a value. Indexes always start from zero, that is,
; the first data element will always be at index 0.

; Musical integers: MIDI key numbers

; MIDI represents note information as the integers 0 to 127. For
; example, 60 is c4 (Middle-C), 59 is B3, 72 is c5, 21 is the lowest A
; on the piano (a1) and 108 is the highest C on the piano (c8), and so
; on.  We will be working with MIDI key number a great deal in this
; class, take the time to become familiar with them!

(keynum "c4")

(note 69)

;
;; Floating point numbers
;

; Floating point numbers, or 'floats',represent real numbers.  Floats
; contain two parts: the significand is the integer portion to the
; left of the dot, the mantissa is the fractional part to the right of
; the decimal point:

123.456

; Note that floats can only approximate some real numbers.  For
; example .333 only an approximation 1/3 to three places. When you
; calculate with an approximation you introduce an error (however
; small) into your work.

;; Musical floats: floating point key numbers

; Common Music extends MIDI's notion of key numbers to include
; floating point key numbers. A floating point keynum is a float
; formatted like 'kkk.cc' where kkk is a MIDI keynum and .cc is a CENT
; value above it. For example, the keynum 50.50 sounds 50 cents above
; middle C (ie one quarter tone up) and the value 59.1 would sound 10
; cents above B3 on. The smallest cent value humans can discriminate
; between is approximately 5 cents so two places is more than enough
; to represent microtonal adjustments.

(note 60)

(note 60.5)

;; Musical floats: amplitude

; We will also use floating point numbers 0.0 to 1.0 to represent
; amplitude values, where 0.0 is silence and 1.0 means "as loud as
; possible". The range on numbers 0.0 to 1.0 is sometimes referred to
; as 'normalized numbers'. Like percents, normalized numbers represent
; proportion. This makes perfect sense for thinking about amplitude
; because the actual loudness of sound will depend on environmental
; factors (computer output levels, stereo amplifier levels etc) more
; than the actual sound source. you can think of amplitude values as
; dynamics, ie .1 = pppp, .5 = mp .7 = f and 1.0 = ffff for example.

;
;; Ratios
;

; A ratio is a quotient of two integers written n/d, where n is the
; numerator and d is the denominator. no spaces are allowed between
; the numbers and slash. Ratios are useful for expressing exact
; proportions, i.e a relationship such as 1/3

1/3

(* 2/7 17/9)

;
;; Mixed Arithmetic
;

; Mixed arithmetic expressions consist of numbers joined together by
; math operators:

;  +   addition
;  -   subtraction
;  *   multiplication
;  /   division
;  modulo   modulus (remainder after division)
;  expt   exponentiation

(+ (expt 10 2) 1)

(+ 1 (* 3  4))

(* (+ 1 3) 4)

(+ (* 2 3) (* 4 5))

(= 2 (+ 1 1))

(modulo 13 12)

; in this next example the first minus is subtraction, the second
; minus is part of the number:

(- 2 -1)

; It is important to understand how differnt kinds of numbers combine
; in mixed expression. The first rule is that if an expression
; contains only integers then entire expression result in an integer
; value:

(+ 10 1)

; If an expression contains any floating point number the entire
; expression will evaluate to a floating point value:

(* 10 .5)

; It is sometimes necessary to convert one type of number into
; another. We will learn how to do this in the next chapter when we
; learn about the common math functions.

;
;; Arithmetic Relations
;

; Scheme's arithmetic relations are:

;  =   equal
;  !=  not equal
;  >   greater than
;  <   less than
;  >=  greater than or equal
;  <=  less than or equal
;  equal?  general equality (true if operands "look" the same)

; Relations are logical operators that evaluate to #t (boolean true)
; or #f (boolean false). See the section on Boolean values for more
; information about this.

(= 2 (+ 1 1))

(!= 2 1)

(< 2 (+ 1 1))

(<= 2 (+ 1 1))

(> 2 1)

(>= 2 1)

(equal? "Hi" "Hi")

(equal? "Hi" "Ho")

(equal? 1 1)

(equal? 1 1.0)

(equal? '(c e g) '(c e g))

;; Relations can be tested order to do something.  for example, the
;; 'if' statement executes one statement if the relation is true and
;; another if it is false.

(if (= (random 3) 0) "winner!" "loser!")
  
; Much more about 'if' and conditional evaluation in a later tutorial!

;
;; Symbols (variables and function names)
; 

; Symbols are words that can name variables and functions.  When a
; symbol appears as a variable in an expression the variable's value
; replaces the symbol when the expression is evaluated.  For example a
; predefined symbol 'pi' hold an approximation of the number pi.  This
; means that you can reference the name 'pi' in a math expression
; rather than typing the actual number

(* 2 pi)

; If we need to use the value of 2 * pi we can create a new variable
; symbol to hold that value as well:

(define 2pi (* 2 pi))

; More about defining variables later...

;; Special variable notations

; S7 provides a special notation that you can use in conjunction with a
; variable that contains a list of data. You can access (get or set)
; any ELEMENT in the list using the notation '(xxx n)', where xxx is the
; name of the variable and n is the element's position (zero based) in
; the list. for example:

(define var '(a b c d e f g))

(var 0)

(var 6)

(print "my random scale degree=" (var (random 7)))

(set! (var 0) -99)

var

;
;; Symbols as command and function names
;

; Functions can have symbol names too. For example, the symbol
; 'random' holds a function that returns a random value each time it
; is evaluated:

(random 100)

;
;; Function calls
; 

; You can tell the difference between symbols used as a functions and
; variables by the fact that function call symbols ALWAYS include
; parentheses () after the function name. The parenthesis hold any
; input values passed to the function:

; A function call with no input values:

(list)

; A function call with 1 input values:

(list 1)

; Use commas to separate inputs if there is more than one:

(list 1 2 3)

; Inputs can be nested to any level:

(list 1 (list 2 (list 3)) 4)

; Much more about this later...

; Notice that the symbol 'list' can be used as a function name as well
; as a variable. Be careful not to define a global variable with the
; same name as a function or that function's definition will be lost!

list

;
;; Boolean Values: #t and #f
;

; A boolean value denotes truth or falsity. The special notation #t
; means true and #f means false:

#t

#f

; A 'boolean expression' is an expression that returns either true or
; false. For example all arithmetic relations are boolean expressions:

(= 1 2)

(< 1 2)

; Functions that return boolean values are called 'predicates'. For
; example, the functions 'even?' and 'odd?' return true or false based
; on a test of their input value. The names of most predicate
; functions end with '?' to mark them as returning #t or #f

(odd? 1)

(odd? 2)

(even? 1)

(even? 2)

(string? "Hi Ho!")

(string? 123)

;
;; Logical Expressions
;

; Scheme defines three logical operators for forming boolean expressions:

;   and   logical AND 
;   or    logical OR
;   not   logical NOT

; Like arithmetic relations, the logical operators return true or
; false based on a test of their arguments.

; Logical AND return true only if BOTH its
; operands are true, otherwise it will return false:

(and #t #t)

(and #t #f)

(and #f #t)

(and #f #f)

; Logical operands can be expressions, of course:

(and (> 1 0) (< 1 2))

; logical OR will return true if EITHER of its operands are true,
; otherwise it will return false:

(or #t #t)

(or #t #f)

(or #f #t)

(or #f #f)

; Logical NOT is the boolean opposite of its operand:

(not #t)

(not #f)

(not (not #t))

; The logical operators can be combined with the arithmetic relations
; to test a series of expressions.To see how this works, consider this
; next example that mimics rolling two dice. The "snake eyes" message
; only appears if both dice land on side 1. Put the cursor at the end
; of the last line and press Enter repeatedly.  You may have to
; execute the statement many times to get a snake eyes!

(if (and (= (random 6) 1) (= (random 6) 1))
    "snake eyes!"
    "loser >:("
    )

;
;; Conditional Expression: (if <test> true false)
;

; A conditional expression is an expression that returns one of two
; values based on a boolean test. If the test expression is true the
; first value is returned, otherwise an optional second value is
; returned. 

(if (odds .5) 100 -100)

;
;; Strings
; 

; Strings are text delimited by "".  

"Hello, world!"

(odds .5 "Winner!" "Loser!")

; When you use the print function with strings it does not include the
; surrounding parentheses in the display.

(print (odds .5) "Winner!" "Loser!")

; Note that the text inside strings is never evaluated:

(random 127)

"random(127)"

;
;; Lists
;

; Lists are structures that group zero or more elements together. A
; list can hold anything, including other lists.  Lists are written by
; delimiting the individual elements that make up the list within
; parens (). To stop evaluation of a list, use the ' function:

'(1 2 3 4 5)

; the empty list (a list of no elements) is a special case: 

()

; elements inside a quoted list are not evaluated, which means symbols
; will not be treated as variables inside the quoted list:

'(bach is a great composer)

; Every open bracket must be balanced by a corresponding right
; bracket.  this is correct:

'(1 2 3 (4 5))

; Lists can hold any type of data, including other lists. This makes
; lists extremely versatile structures for organizing data. What does
; this list represent?

'(("3-9*"  (0 2 7) (0 1 0 0 2 0) "Quartal Trichord")
  ("3-10*" (0 3 6) (0 0 2 0 0 1) "Diminished Chord")
  ("F3-11" (0 3 7) (0 0 1 1 1 0) "Minor Chord")
  ("3-11B" (0 4 7) (0 0 1 1 1 0) "Major Chord"))

; If you want to evaluate an expression inside a list you can use the
; backquote and ,  just before the thing you want to evaluate:

`(1 2 , (pick 3 33 333 3333) 4 5)

; You can 'splice' a list of values into the surrounding list using ,@

`(1 2 ,@ (list (ran) (ran) (ran)) 4 5)

;; Musical lists:  sequences

; Lists are the most natural way to represent musical 'sequences',
; that is, data to be performed in left-to-right order.

;;  Here is a twelve tone row

'(0 8 3 2 11 10 1 7 9 4 6 5)

;; Here is a non-retrogradable rhythm:

'(1 2 3/4 5/4 3/4 2 1)

;; Here are three major chords:

'((60 64 65) (62 66 69) (64 68 71))

;
;; Note lists
;

; Perhaps the most common musical use of lists is to hold a series of
; notes or key numbers. We already know about integer and floating
; point key numbers. A note name is a letter: c d e f g a b plus an
; optional sharp or flat letters: s f n ss ff (Sharp Flat Natural
; double sharp, double flat plut an optional quarter-tone inflection
; sign (< >). plus an optional octave number -1 ... 10. Examples:

; c5    C in the 5th octave
; c>4   C plus 1 quarter tone in the 4th octave
; dn2   D natural in the 2nd octave
; eff4  E double flat in the fourth octave
; bf<0  Bf minus 1 quarter tone in the 0th octave.

; The 'note' and 'key' and 'hertz' functions convert one form into
; another. These functions can be passed lists lists as well as
; individual notes or key numbers. For example here is another way of
; defining the list of chords shown in the last example but in terms
; of keynumbers

(keynum '((c4 e g) (d4 fs a) (e4 gs b)))

(note 69)

(note (keynum 440))

; Note that when you call key and note() with lists of notes, octave
; numbers are "sticky" inside each note lists, i.e. octaves only have
; to be written when they change:

(keynum '(c4 d ef f gs g3 a bf c ds5 e fs g))

(hertz '(c4 d ef f gs g3 a bf c ds5 e fs g))
