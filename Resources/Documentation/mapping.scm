;
;; Mapping
;

; To run code put your cursor after each expression and press
; Command-Return, then check the console window for any output.

; Mapping means to transform values from one domain into equivalent
; values in another. For example, to map values in cents into
; equivalent values in dollars divide cent values by 100:

(loop repeat 10
      for cent = (random 100)
      do
      (print cent " cents is " (/ cent 100.0) " Dollars")
      )

; We can use the 'rescale' function to accomplish mapping in a general
; way. rescale maps a value from one range into an equivalent value in
; a new range: rescale(val, min, max, newmin, newmax)

(loop repeat 10
      for cent = (random 100)
      for dollar = (rescale cent 0 100 0.0 1.0)
      do
      (print cent " cents is " dollar " Dollars.")
      )


; Mapping is a central concept in algorithmic composition, it is the
; basis for "sonification" -- of transforming numbers into useful
; musical information. For example lets use rescale to map cents into
; (1) midi key numbers 60 to 84 (2) amplitude values .2 to .8 and (3)
; durations 1.0 to 0.5 (inverted)

(let* ((cent (random 100))
       (keyn (rescale cent 0 100 60.0 84.0))
       (amp  (rescale cent 0 100 .2 .8))
       (dur  (rescale cent 0 100 1.0 .5)))
  (print cent " cents -> keyn=" keyn ", amp=" amp " dur=" dur)
  )

; Listen to it!

(define (cents)
  (process repeat 20
           for cent = (random 100)
           for keyn = (rescale cent 0 100 60.0 84.0)
           for amp = (rescale cent 0 100 .2 .8)
           for dur = (rescale cent 0 100 1.0 .5)
           do
           (mp:midi :key keyn :dur dur :amp amp)
           (wait dur)
           ))

(sprout (cents))

; Here is a slightly generalized version, it takes a several input
; arguments to control the mapping.

(define (cents2 len lokey hikey)
  (process repeat len
           for cent = (random 100)
           for keyn = (rescale cent 0 100 lokey hikey)
           for amp = (rescale cent 0 100 .2 .8)
           for dur = (rescale cent 0 100 1.0 .5)
           do
           (mp:midi :key keyn :dur dur :amp amp)
           (wait dur)
           ))

(sprout (cents2 10 40 67))

(sprout (list (cents2 20 40 67)
              (cents2 20 90 70))
        '(0 2))

;
;; Sin-ful composition
;

; Lots of math functions also perform mapping. For exampe the function
; 'sin' (sine) maps radians values into amplitudes between -1 and 1.
; Radians are simply a way to measure angles: degrees chop the unit
; circle up int 360 equal parts and radians chop the circle up into
; 2pi parts. In other words 360 degrees equals 2pi radians. Let's use
; rescale to show this:

(loop for deg from 0 to 360 by 45
      for rad = (rescale deg 0 360 0 (* 2 pi))
      do
      (print deg " degrees = " rad " radians.")
      )

; Now forget about degrees, lets just move 2pi radians in 8 steps from
; 0 to 1 see what sine gives us:

(loop for x from 0 to 1 by 1/8
      for rad = (rescale x 0 1 0 (* 2 pi))
      for amp = (sin rad)
      do
      (print "x=" x " radians=" rad " sin=" amp)
      )

; So as x goes from 0 to 1, radians goes from 0 to 2pi and sin returns
; the amplitude. if we plot these values of sine left to right we get
; a perfectly symmetrical 'wave figure' (called the sine wave) with a
; shape that looks something like this:

; +1 |   *  *
;    | *      *
;  0 |*--------*--------*----->
;    |          *      *
; -1 |            *  *

;X  : 0  1/4  1/2 3/4   1
;Rad: 0  pi/2 pi  3pi/2 2pi


; Since sin values range from -1 to 1 all we have to do is use the
; 'rescale' function to map sin values to appropriate ranges for
; musical parameters. For example, this loop maps sin values to key
; numbers between 60 and 84:

(loop for x from 0 to 1 by 1/8
  for rad = (rescale x 0 1 0 (* 2 pi))
  for amp = (sin rad)
  for keyn = (rescale amp -1 1 60 84)
  do
  (print "x=" x " radians=" rad " keynum=" keyn)
  )

; So we've mapped x from 0:1 to a sine wave shape of key numbers
; between 60 and 84! What would this sound like?

;
;; Using sin in a musical process
;

; How can we use sin to control the evolution of a musical process?
; The loop example above hints at how we can solve at least one
; approach to the problem. Imagine that we want to control the key
; numbers of a process that runs for 100 iterations of a counter
; variable i. On each iteration we want to calculate the current value
; of sin and output a corresponding keynum. Calculating the keynum
; from the output value of sin is easy (we use rescale), but how do we
; convert the process iteration variable 'i' into a radian value 'r'
; that we can pass into sin? A moments reflection will tell that
; something like this will work:

; [1]    r = 2 * pi * (i / 100))

; formula [1] says that as counter i moves from 0 to 100 the radians r
; moves from 0 to 2pi. For example: 
; when i=0   then i/100=0  so r=2pi*0  and r=0;
; when i=50  then i/100=.5 so r=2pi*.5 and r=pi
; when i=100 then i/100=1  so r=2pi*1  and r=2pi and so on.

; Lets implement the 100 event test process:

(define (sine1)
  (process for i below 100
           for r = (* 2 pi (/ i 100))
           for a = (sin r)
           do
           (mp:midi :key (rescale a -1 1 21 (+ 21 88)))
           (wait .1)
           ))

(sprout (sine1))

; TODO: This example can be generalized! Copy/paste the definition
; to create a new process called sine2 that accept four arguments:
; reps, rate, low and high. The process should run for reps
; iterations, output keynums between low and high, and wait rate
; seconds between iterations. For example here are two calls to the
; function you will create:

;sprout(sine2(100, .1, 0, 127))
;sprout(sine2(100, .1, 30, 90))

; How can we make the process generate more than one cycle of the wave
; as i goes from 0 to num?  Perhaps we can see if we compare formula
; [1] with a second version:

; [1]    r = 2 * pi * (i / 100))
; [2]    r = 2 * pi * 10 * (i / 100)

; As i goes from 1 to 100, formula [1] goes 1 trip around 2pi radians
; and formula [2] goes 10 trips around 2 pi radians. Since [2] make
; ten trips from 0-pi in the same time [1] makes 1 trip, the frequency
; of [2] is ten times that of [1]

; Lets generalize this by adding a new parameter 'cycs' just after the
; 'num' parameter use that parameter as a "frequency" for radian
; calculation. 

(define (sine3 len cycs low hi rhy dur amp)
  (process with 2pi = (* 2 pi)
           for i below len
           for a = (sin (* 2pi cycs (/ i len)))
           do
           (mp:midi :key (rescale a -1 1 low hi)
                    :amp amp :dur dur)
           (wait rhy)
           ))

; We test it out by specify 4 cycles in 100 notes:

(sprout (sine3 100 4 20 100 .1 .1 .6))

; We can easly "dirty up" the deterministic motion of sin by adding a
; random component to its values:

(define (sine4 len cycs low hi rann rhy dur amp)
  (process with 2pi = (* 2 pi)
           for i below len
           for a = (sin (* 2pi cycs (/ i len)))
           for r = (between (- rann) rann)
           do
           (mp:midi :key (rescale (+ a r) (- -1 rann) (+ 1 rann) low hi)
                    :amp amp :dur dur)
           (wait rhy)
           ))

(sprout (sine4 100 4 20 100 .2 .1 .1 .6))

; And lastly, here is a process that uses sin to generate
; "oscillating" rhythmic patterns, ie moving faster and slower around
; a central rhythmic value.

(define (sinrhy num cycs fast slow lb ub)
  (process for x to num
           for s = (sin (* 2 pi cycs (/ x num)))
           for k = (between lb ub)
           do
           (mp:midi :key k :dur .1)
           (wait (rescale s -1 1 slow fast))
           ))

(sprout (sinrhy 40 3 .05 .6 60 80))

; The discrete function is like rescale but it maps floating point
; values onto integers, or lists of values.

(loop repeat 20
      for x = (ran 1.0)
      for d = (discrete x 0.0 1.0 48 90)
      do
      (print "x=" x " d=" d)
      )

; You can also map onto lists of discrete values

(define (sine5 len cycs scal rhy dur amp)
  (process with 2pi = (* 2 pi)
           for i below len
           for a = (sin (* 2pi cycs (/ i len)))
           do
           (mp:midi :key (discrete a -1 1 scal)
                    :amp amp :dur dur)
           (wait rhy)
           ))

(let ((scal (scale 50 20 1 2)))
  (sprout (sine5 100 4 scal .1 .1 .6))
  )


; Exponential scaling

; Linear rescaling is a wonderful tool, it provides a simple and
; general way to control sound parameters as a function of some
; variable whose domain may not be directly related to the parameter
; in question. However, there are times when the "straight line"
; effect produced by linear interpolation may be inappropriate. For
; example, changes in tempo and amplitude often sound more natural if
; they are produced by exponential scaling rather than linear
; interpolation. Values generated by exponential scaling lie on a
; power curve rather than on a straight line. The base argument
; argument to rescale can be used to produce interpolation along a
; power curve. If base is 1 (the default) then linear interpolation
; results. If base is greater than 1 the interpolated values lie on a
; convex curve for positive slopes, or concave curves if it is less
; than 1 The more base deviates from 1 the steeper the power curve
; between points.

(loop for i below 10
      do 
      (print (rescale i 0 9 100 1000 8))
      )

(loop for i from 0 to 10
      for t from 0 by .2
      do
      (mp:midi t :key (rescale i 0 10 40 90))
      (mp:midi t :key (rescale i 0 10 40 90 1/8))
      )


; segs( num, quant, mode, num) 

; segs allows you to "chop up" a quantity into a specified number of
; related segments. If mode is 1 then the values in the list are
; exponentially related: if base is greater than 1 then values are
; returned are in increasing order, if base is less than 1 then values
; are in decreasing order:

(segs 4 20 1)

(segs 4 20 1 8)

(segs 4 20 1 1/2)

; If mode is 3 then segs returns num random values that sum to quant:

(segs 5 10 3)

; The next example defines a process called ballfall whose tempo and
; key numbers are controlled by a list of exponential values produced
; by segs.

(define (ballfall drops sum curve keyn amp)
  (let ((all (segs drops sum 1 curve)))
    (process with big = (first all)
             and low = (hertz keyn)
             for d in all
             for e = (expt 2 (ran (/ d big)))
             for k = (keynum (* low  e))
             for a = (rescale d 0 big .1 amp)
             do
             (mp:midi :dur d :key k :amp a)
             (wait d)
             )))

; Drops are the number of times the ball bounces and end is the time
; it takes to decay. Curve controls the steepness of the power
; curve. Since the longest drop occurs at the beginning the curve
; value should always be less than 1 for this process. Key is the
; lowest key number the ball will play and amp is the maximum
; amplitude. The variable all is set to the list of exponential values
; returned by explsegs. Since curve is less than 1 the first value in
; this list is the maximum drop value max. The process iterates for
; each drop value d in all. On each iteration, the variable e is set
; to a random value between 1 and 2 based on the proportion of the
; current drop d to the longest drop max. This means that as the
; process iterates, the range for successive random values of e
; diminishes from 2.0 to a value approaching 1.0. This
; value is then used to transpose the low frequency to somewhere
; within an octave and k is set to the key number of that Hertz
; frequency.

(sprout (ballfall 40 10 1/256 60 .8))

(sprout (ballfall 100 15 1/150 40 .8))

