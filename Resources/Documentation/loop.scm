;
;; Iteration
;

; To run examples put the cursor at the end of each expression and
; press Command-Return. Look in the console window for any output.

; Here is one way to execute a statement five times

(begin (print "a random keynum: " (random 128)) 
       (print "a random keynum: " (random 128)) 
       (print "a random keynum: " (random 128)) 
       (print "a random keynum: " (random 128)) 
       (print "a random keynum: " (random 128)))

; Loop is a much better way of accomplishing the same thing!

(loop repeat 5
      do
      (print "a random keynum: " (random 128)))

; Note that with a loop you can double the amount of work done without
; adding any additional code

(loop repeat 10
      do
      (print "a random keynum: " (random 128)))

; The 'loop' statement performs iteration: it repeatedly executes an
; action statement some number of times. The simplest way to tell a
; loop how many times to iterate is to use its 'repeat' clause. the
; repeat value can a number or any sexpr that evaluates to a number

(loop repeat (random 10) 
      do
      (print "a random keynum: " (random 128)) )

; Loop's 'do' tag makes it an implicit block so you can execute more
; than one action statement

(loop repeat 4
      do
      (print "a random keynum: " (random 128)) 
      (print "approximately one beat: " (vary 1 0.5)) 
      (print "a random dead composer: " (pick '(bach schoenberg berg mozart))) 
      (print "----------------------------") )


; The loop's action can be any SCM statement

(loop with foo = '()
      repeat 4
      do
      (set! foo (concat foo (random 128)))
      finally (return foo)) 

; This last example is more easily expressed using the 'collect'
; operator

(loop repeat 4
      collect (random 128))

; The 'finally' clause lets you associate an expression with the END
; of the iteration. If given, the finally statement is executed one
; time only, immediately after the iteration has stopped, and the
; value of the expression will be returned as the value of the loop.

(loop repeat 0 do
      (print "a random keynum: " (random 128)) 
      finally (print "All done!"))

; You can use a '(return <value>)' expression in conjunction with 
; 'finally'' to return a value from the loop

(loop with ransum = 0
      repeat 10
      do
      (set! ransum (+ ransum (random 128)) )
      finally (return ransum))

; Use a (begin ...) OR (let ...) block if you want to execute more
; than one finally statement

(loop for x = (random 128)
      with foo = '() and tot = 0
      repeat 10
      do
      (set! foo (append foo (list x)))
      (set! tot (+ tot x))
      finally (let ((avr (/ tot 10.0)))
                (print "keys: " foo)
                (print " average: " avr)
                (print "All done!")))

;
;; Stepping statements
; 

; You can specify any number of 'for' clauses that increment stepping
; variables each time through the loop.  All stepping clauses must
; appear before the action statement. In this example the stepping
; variable i is set to a new random key number each time through the
; loop

; General stepping:  'for <var> = <sexpr>' ...

(loop repeat 10
      for i = (random 128)
      do
      (print "a random major chord: " i " " (+ i 4) " " (+ i 7))
      finally (print "All done!")) 
               
; You can have any number of 'for statements'. This example uses three
; stepping variables: key rhy and amp. Notice that the amp's 'then'
; clause causes it to be randomly chosen only on the FIRST iteration
; and then incremented by .05 each time after that.

(loop repeat 10
      for key = (random 128)
      for rhy = (pick 1/4 1/2 3/4 1)
      for amp = (pick .1 .3 .5) then (+ amp 0.05)
      do
      (print "key=" key)
      (print "amp=" amp) 
      (print "rhy=" rhy) )

; The value of one 'for <var> = ...' variable can depend on
; another. in this example we declare the stepping variable key to
; hold a random keynum and 'maj' to hold a list representing a major
; chord built on whatever keynum was chosen

(loop repeat 10
      for key = (random 121)
      for maj = (list key (+ key 4) (+ key 7))
      do
      (print "a random major chord: " maj) 
      finally (print "All done!"))

; TODO: change the preceding loop to include printing random minor,
; diminished and augmented chords. 

;
;; List stepping: 'for <var> in <list>'
;

; You can step a variable over a list of elements using the 'in'
; clause. the iteration stops once all the elements in the list have
; been accessed

(loop for c in '(a b c d e f g)
      do
      (print c) )

(loop for c in '(bach beethoven mozart berg webern schoenberg)
      for q = (pick '(great fantastic wonderful))
      do
      (print c " is a " q " composer."))

;
;; Numerical stepping: 'for <var> from <num> to <num> by <num> ...'
; 

; Numerical stepping clauses automatically increment stepping
; variables by counting. both the starting and stopping bounds for the
; counting can be provided

; Use 'from' to specify the starting value. if 'from' value is not
; provided the variable starts at 0.  the stopping value is specified
; using one of: 'to', 'below', 'downto', 'above'.  

; The 'to' and 'downto' boundary stops iteration just after the
; variable reaches the value

(loop for x from 1 to 10
      do
      (print "x=" x) )

(loop for x from 10 downto 1
      do
      (print "x=" x) )

; If you omit the 'from' it defaults to 0

(loop for x to 10
      do
      (print "x=" x) )
  
; The 'by' clause lets you specify the increment value for the variable

(loop for x from 0 to 20 by 2
      for y from 100 downto 10 by 10
      do
      (print "x=" x " y=" y))

; You can increment by floating point values too

(loop for x from 2.5 to 8 by .1
      do
      (print "x=" x) )

; The 'below' and 'above' boundaries stop iteration just before the
; variable reaches the value

(loop for x below 10
      do
      (print "x=" x) )

(loop for x from 10 above 0
      do
      (print "x=" x) )

; If more than one stepping clause is provided the loop stops after
; shortest path is reached

(loop for x from 0 to 1000 by 10
      for y to 10 by pi
      do
      (print "x=" x " y=" y))

; You can also specify a numerical iteration without a stepping
; boundary. but some clause better stop the iteration or it will run
; forever!

(loop repeat 10
      for x from 0 by 10
      for y from 0
      for e = (expt y x)
      do
      (print "e=" e) )

;
;; Accumulating results
;

; This example collect all the values of a stepping variable into a
; value that it tne returns

(loop for i from 1 to 10
      collect i)

; Reverse collect all the value of a stepping variable into a list:

(loop with res = '()
      for i from 1 to 10
      do
      (set! res (cons i res))
      finally (return res))

; Sum all the values of a stepping variable 

(loop for i from 1 to 10
      sum i)

; Minimize values

(loop repeat 10
      minimize (random 128))

; Maximize values

(loop repeat 10
      maximize (random 128))

; Multiply all the values of a stepping variable into a local
; variable. 

(loop with res = 1
      for i from 1 to 10
      do
      (set! res (* res i))
      finally (return res))

; This loop both sums and collects random key numbers. It then prints
; the results, along with the average value found:

(loop repeat 10 
      for j = (random 128)
      sum j into totsum
      collect j into totals
      finally (print "res=" totals " avr=" (/ totsum (length totals))))

;
;; Stopping iteration using while and until
;

; You can use 'while' or 'until' clauses to halt iteration based on a
; test of some condition. while stops iteration as soon as the
; condition is false. until stops iteration when the test is true:


; Iterate random numbers until a '9' is found

(loop for x = (random 10)
      until (= x 9)
      do
      (print x) 
      finally (print "all done!"))

; Collect 10 random keynumbers in pentatonic scale

(loop with n = 0
      for x = (random 128)
      for p = (modulo x 12)
      until (= n 10)
      when (member p '(1 3 6 8 10)) 
      collect x
      and do (set! n (+ n 1)))

; Iterate random numbers until the same value is picked consecutively

(loop with old = -1
      for new = (random 20)
      until (= old new)
      do
      (print "new=" new " old=" old) 
      (set! old new)
      finally (print "all done!"))

