;
;; Musical Processes
;

; To run examples put the cursor at the end of each expression and
; press Command-Return. Look in the console window for any output.

; A 'process' is an iterative function (a combination of a function
; and a loop) that runs in the system scheduler and generates a 
; unique timeline of events.  Here is a simple first example.

(define (simple)
  (process repeat 20 
           do
           (mp:midi :key (between 60 96))
           (wait 0.1)
           ))

; Before listening to the process use the Midi Out menu to select the
; MIDI device you want to use and try Test Output to make sure you can
; hear sound!

; Now lets sprout the process to hear it generate output in real
; time. Notice that you can sprout overlapping instance of the process
; and each each time you sprout it the process produces a different
; variation of the algorithm.

(sprout (simple))

; You can start a process running in the future by providing an
; optional 'ahead' argument to sprout. This example tells sprout to
; start running simple 2 seconds in the future.

(sprout (simple) 2)

; To sprout more than one process at a time pass a LIST of processes
; to sprout.

(sprout (list (simple) (simple)))

; The ahead factors can also be a list, in which case each successive
; process will be paired with a successive start value

(sprout (list (simple) (simple)) '(2 2.5))

; The definition of simple looks similar to a function and a loop and
; it has a special 'wait' command. The 'wait' clause tells the process
; how much time to pause before its next iteration occurs.  For
; example 'wait .1', means that the internal time points for the sends
; will be at 0, .1, .2, .3, .4, .5, .6 ... Of course you can specify
; any wait time, even random ones as in this example.

(define (simple2)
  (process repeat 50
           do
           (mp:midi :key (between 60 96))
           (wait (odds .2 .5 .1))
           ))

(sprout (simple2))

;
;; Arguments to a 'process'
;

; As with function definition the 'process' definition command allows
; the process to receive argument values.

(define (playscale low high length)
  (process with rhy = (/ length (+ (- high low) 1))
           for key from low to high
           do
           (mp:midi :key key :dur (* 2 rhy))
           (wait rhy)))

; 'playscale' plays a chromatic scale from 'low' to 'high' within the
; time 'length' (in sec.). the expression: 'with rhy = length / (high
; - low + 1)' calculates the rhythm of each note as a proportion of
; the distance between low and high: the greater the distance the
; faster the scale.

(sprout (playscale 60 72 1))

(sprout (playscale 60 72 2))

; You can run different versions of scale in parallel at the same
; time, each with different argument values by passing a list of
; processes to sprout.

(sprout (list (playscale 72 84 5)
              (playscale 60 84 5)
              (playscale 48 84 5)
              (playscale 36 84 5)
              (playscale 24 84 5)))

; Here is a process that implements Steve Reich's piano phase. it
; creates a cyclic pattern to read the trope over again and stops when
; the current (elapsed) time of the process is the end time specified
; to the process.

(define (piano-phase stop keys rate)
  (process with pat = (make-cycle keys)
           until (>= (elapsed) stop)
           do
           (mp:midi :key (next pat) :dur rate)
           (wait rate)
           ))

(let ((notes '(e4 fs4 b4 cs5 d5 fs4 e4 cs5 b4 fs4 d5 cs5))
      (stop 20))
  (sprout (list (piano-phase stop (key notes) .167)
                (piano-phase stop (key notes) .17))))
              
; You can compute different versions of a piece and choose the ones
; you like. In this example we will shuffle the trope each time to
; compose different versions of the piece each time we run it.  Since
; we are not sure what we will be hearing, we save different versions
; of the file so that we can review and choose the ones we like the
; best.

(let ((notes (shuffle '(e4 fs4 b4 cs5 d5 fs4 e4 cs5 b4 fs4 d5 cs5)))
      (stop 20))
  (print "phasing trope=" notes)
  (sprout (list (piano-phase stop (key notes) .167)
                (piano-phase stop (key notes) .17))))

; The next process demonstrates regulation of a rit/accell in which
; the current rhythm will be continually scaled by 'fac'.

; We first define a helping function called 'chroma' to produce a
; keynum-row from 'low' to 'high'.

(define (chroma low high)
  (loop for v from low to high
        collect v)
  )

(define (acc-rit key-list start-rhy fac)
  (process for k in key-list
           for r = start-rhy then (* r fac)
           do
           (mp:midi :key k :dur r)
           (wait r)
           ))

(sprout (acc-rit (shuffle (chroma 48 85)) .1 1.05))

(sprout (acc-rit (shuffle (chroma 48 85)) .1 .95))

; The combination of multiple 'acc-rits' produces a part-wise
; proportions canon.

(define (acc-rit-list key-list tr-list r-list fac-list)
  (loop for tr in tr-list
        for r in r-list
        for fac in fac-list
        collect (acc-rit (transpose key-list tr) r fac)))

; The sprout command takes an optional second argument, the future
; time(s) to start the algorithm(s) supplied in the first argument.

(let ((lst (shuffle (chroma 60 85))))
  (sprout (acc-rit-list lst
                        '(-24 -12 0 12 24)
                        '(1/4 1/8 3/16 5/16 1/16)
                        '(1.01 1 .9 1 1.1))
          '(0 2 4 6 8)))

; It is possible to sprout processes from inside other processes. We
; first define a process that harmonizes a shuffled and transposed
; melodic line using 4th and 5ths.

(define (ran-series fund row rhy)
  (process for r in (shuffle rhy)
           for k in (shuffle (transpose row fund))
           do
           (mp:midi :key (- k 12) :dur r)
           (mp:midi :key k :dur r)
           (mp:midi :key (+ k (pick 5 7)) :dur r)
           (wait r)
           ))

(sprout (ran-series 72 
                    '(0 2 6 4 1 3 -5 8)
                    '(.375 .125 .5 .375 .125 .25 .25 .25)
                    ))

; Now define the 'meta process' (the process that creates other
; processes) to sprout multiple ran-series processes to create low
; random staccato notes. 5% of the time is sprouts a harmonized line
; otherwise it simply plays the melodic tone.

(define (meta-proc len)
  (process for r = (between .2 .4)
           for k = (between 36 43)
           until (>= (elapsed) len)
           do
           (mp:midi :key k :dur 1)
           (if (odds 0.05)
               (sprout (ran-series (+ k 36)
                                   '(0 2 6 4 1 3 -5 8)
                                   '(.5 .4375 .375 .3125 .25 .1875 .125 .0625))))
           (wait r)
           ))
                     

(sprout (meta-proc 20))
