;
;; Markov Musical Examples
;

; To run code put your cursor after each expression and press
; Command-Return, then check the console window for any output.

; Markov Rhythmic Patterns

; Imagine generating a melody for a soloist in which the rhythms are
; determined by weighted random selection. Even if only very simple
; rhythms are used, a moments reflection tells us that the patterns
; produced by the process will not reflect any underlying pattern of
; beat. For example, assume that the process is restricted to
; quarters, dotted-eights, eighths, and sixteenths (1 .75 .5 .25)
; Since the random process can place sixteenths and dotted eighths
; anywhere within a beat, the sequence of rhythmic values that result
; will only occasionally line up with the start of a metric pulse.a

; TODO: Define a Markov process that generate random rhythms that
; express an underlying beat and tempo curve. 

(define (play-rhythms len rhys lo hi)
  (let* ((tcurve '(0 1 .7 .75 1 1))
         (pat (make-markov rhys)))
    (process for i below len
             for k = 60 then (discrete (ranpink) -1 1 lo hi)
             for r = (next pat)
             for d = (* (in-tempo r 120) (interp (/ i len) tcurve))
             do
             (mp:midi :key k :dur d)
             (wait d)
             )))

; First listen to zero order rhythms (zero order = weighted random
; selection) -- its hard to hear where the beat is!

(let ((mytable '(( -> 1 .75 .5 .25)))) ; zero order markov
  (sprout (play-rhythms 75 mytable 40 80))
  )

; Now implement this rhythmic transtion table and listen to it:

;      q   e.   e   s
; q    .5  .75  2   0
; e.   0   0    0   1
; e    2   1    2   0
; s    1   0    2   1

(let ((mytable '((1 -> (1 .5) (.75 .75) (.5 2))
                 (.75 -> .25)
                 (.5 -> (1 2) .75 (.5 2))
                 (.25 -> 1 (.5 2) .25)
                 )))
  (sprout (list (play-rhythms 50 mytable 40 60)
                (play-rhythms 50 mytable 70 85)))
  )

;
; Markov melody
;

; Create a Markov process to generate a pseudo Gregorian Chant. (For a
; much larger example of style generation see foster.sal). In the
; crudest of terms, a Gregorian Chant is characterized by mostly step
; wise motion within a range of modal degrees. From any given tone
; there is more likelihood that a chant will move a step up or down
; than leap to a tone further away. The larger the skip, the more
; unlikely it is to occur. In addition, certain tones, such as the
; final and tenor, have more influence over the melody than other
; tones. In the For example, in the Dorian mode the tenor A is
; occasionally decorated by the B-flat directly above it. This B-flat
; almost always returns back to the tenor tone. In an authentic mode,
; the final of the mode also acts as a kind of reflecting boundary
; that redirects the melody in the opposite direction. We can mimic
; these stylistic tendencies using a first order Markov process:

;       d4   e4   f4   g4   a4   bf4
; d4    .1   .35  .25  .1   .15  0
; e4    .35  .1   .35  .1   .1   0
; f4    .2   .2   .1   .2   .12  0
; g4    .2   .1   .3   .1   .3   .2
; a4    .1   .1   .2   .3   .2   .3
; bf4   0    0    0    0    1    0

(define (monk1 len chant rhy)
  (let ((pat (make-markov chant)))
    (process repeat len
             for n = (next pat)
             do
             (mp:midi :key (keynum n) :dur rhy)
             (wait rhy)
             )))

(let ((mytable '( 
                 (d4 -> (d4 .1)  (e4 .35) (f4 .25) (g4 .1) (a4 .15))
                 (e4 -> (d4 .35) (e4 .1) (f4 .35) (g4 .1) (a4 1) )
                 (f4 -> (d4 .2) (e4 .2) (f4 .1) (g4 .2)(a4 .12) )
                 (g4 -> (d4 .2) (e4 .1) (f4 .3) (g4 .1) (a4 .3) (bf4 .2))
                 (a4 -> (d4 .1) (e4 .1) (f4 .2) (g4 .3) (a4 .2) (bf4 .3))
                 (bf4 -> a4)
                 )))
  (sprout (list (monk1 30 mytable .5)))
  )

; In the next monk we look at several strategies for shaping a process
; so that its results are more musically satisfying. We know, for
; example, that in Gregorian Chant the tenor and final are more likely
; to be agogically stressed than the other tones. Moreover, the chant
; should begin and end on the final of the mode.

; Markov with external conditional checks.

(define (chant-dur tone dur)
  ;; adjust dur if tone is D, F or A.
  (if (= (pitch-class tone) 2)
      (odds .7 (* dur 2) dur)
      (if (= (pitch-class tone) 9)
          (odds .5 (* dur 2) dur)
          (if (= (pitch-class tone) 5)
              (odds .25 (* dur 2) dur)
              dur))))

(define (monk2 endtime chant rhy oct)
  (let ((pat (make-markov chant))
        (lk 62))
    (process for k = (keynum (next pat))
             for dur = (chant-dur k rhy)
             until (and (> (elapsed) endtime) (= (pitch-class k) 2))
             do
             (mp:midi :key (transpose k oct) :amp .8 :dur dur)
             (set! lk k)
             (wait dur)
    )))

(let ((mytable '((d4 -> (d4 .1)  (e4 .35) (f4 .25) (g4 .1) (a4 .15))
                 (e4 -> (d4 .35) (e4 .1) (f4 .35) (g4 .1) (a4 1))
                 (f4 -> (d4 .2) (e4 .2) (f4 .1) (g4 .2)(a4 .12))
                 (g4 -> (d4 .2) (e4 .1) (f4 .3) (g4 .1) (a4 .3) (bf4 .2))
                 (a4 -> (d4 .1)(e4 .1)(f4 .2)(g4 .3)(a4 .2)(bf4 .3))
                 (bf4 -> a4)
                 )))
  (sprout (list (monk2 20 mytable .8 12)
                (monk2 20 mytable .8 0)
                (monk2 20 mytable .8 -12)))
  )

; Try monks at the tritone!

;
;; Markov Harmony
;

; In this next example a Markov process generates interval relations
; for generating chords below an underlying melody.

;    1     2     3     4     6
; 1  0     0    .4     .4    .1
; 2  0    .2    .4     .4    .1
; 3  .2   .6    0      .4    0
; 4  0    .2    .4     .4    0
; 6  0    .3    .2     .2    0

(define (markov-chorder len int-mix mel size ud rhy dur)
  (let ((pat (make-markov int-mix)))
    (process repeat len
             for intr = (next pat)
             do
             (set! mel (fit (transpose mel (odds ud intr (- intr)))
                            50
                            90))
             (loop with k = mel
                   repeat size
                   do
                   (mp:midi :key k :dur dur)
                   (set! k  (transpose k (- (next pat)))))
             (wait rhy)
             )))

(let ((mytable '((1 -> (3 .4) (4 .4) (6 .1))
                 (2 -> (2 .2 ) (3 .4) (4 .4) (6 .1))
                 (3 -> (1 .2 )(2 .6 )  (4 .4))
                 (4 -> (2 .2 ) (3 .4) (4 .4))
                 (6 -> (2 .4) (3 .2) (4 .2)))))
  (sprout (markov-chorder 25 mytable 67 6 .7 1.2 1.2))
  )

;
; Markov Texture
;

; In this example a Markov process generates persistent motion on
; black and white keys, where each key is weighted to prefer moving to
; nearer tones of its same color. The basic weighting strategy for
; moving from key to key is:

; Repeat current key:             0.5
; Near-to-far spread same color   2.0 - 0.5
; Near-to-far spread other color  0.2 - 0.1

(define bwmotion '((0 -> (0 .5) (2 2) (4 1.5) (5 1) (7 .5) (9 .5) (11 .5)
                      (1 .2) (3 .1) (6 .1)  (8 .1) (10 .1))
                   (1 ->  (1 .5) (3 2) (6 1.5) (8 1) (10 1) 
                      (0 .2) (2 .2) (4 .1)  (5 .1) (7 .1) (9 .1) (11 .1))
                   (2 ->  (0 2)  (2 .5) (4 2)   (5 1.5) (7 1) (9 .5)
                      (1 .2) (3 .2) (6 .1) (8 .1) (10 .1) (11 .1) )
                   (3 -> (1 2)  (3 .5) (6 1.5) (8 1) (10 .5)
                      (0 .1) (2 .2) (4 .2) (5 .1) (7 .1) (9 .1) (11 .1))
                   (4 -> (0 1.5)(2 2)  (4 .5) (5 2) (7 1.5) (9 1) (11 .5)
                      (1 .1) (3 .2) (6 .2) (8 .1) (10 .1))
                   (5 -> (0 1)  (2 1.5)(4 2) (5 .5) (7 2) (9 1.5) (11 1)
                      (1 .1) (3 .2) (6 .2) (8 .1) (10 .1))
                   (6 ->  (1 1.5)(3 2)  (6 .5) (8 2) (10 1.5) 
                      (0 .1) (2 .1) (4 .1) (5 .2) (7 .2) (9 .1) (11 .1))
                   (7 -> (0 .5) (2 1)  (4 1.5) (5 2) (7 .5) (9 2) (11 1.5) 
                      (1 .1) (3 .1) (6 .2) (8 .2) (10 .1))
                   (8 -> (1 1)  (3 1.5)(6 2) (8 .5) (10 2) 
                      (0 .1) (2 .1) (4 .1) (5 .1) (7 .2) (9 .2) (11 .1))
                   (9 -> (0 .5) (2 .4) (4 1) (5 1.5) (7 2) (9 .5) (11 2) 
                      (1 .1) (3 .1) (6 .1) (8 .2) (10 .2))
                   (10 -> (1 .5) (3 1) (6 1.5) (8 2) (10 .5) 
                       (0 .1) (2 .1) (4 .1) (5 .1) (7 .1) (9 .2) (11 .2))
                   (11 -> (0 .5) (2 .5) (4 .5) (5 1) (7 1.5) (9 2) (11 0.5)
                       (1 0.1) (3 .1) (6 .1) (8 .1) (10 .2)))
  )

(define bwoctaves '((c3 -> (c3 2)   (c4 1)  (c5 .5) (c6 .25))
                    (c4 -> (c3 1)   (c4 2)  (c5 1)  (c6 .5))
                    (c5 -> (c3 .5)  (c4 1)  (c5 2)  (c6 1))
                    (c6 -> (c3 .25) (c4 .5) (c5 1)  (c6 2)))
  )

(define (bw len octlist intlist rate)
  (let* ((ints (make-markov intlist))
         (octs (make-markov octlist))
         (reps 0)
         (intr 0)
         (oct 0))
    (process repeat len
             do
             (if (= reps 0)
                 (begin (set! reps (pick 4 8 12 16))
                        (set! oct (keynum (next octs)))))
             (set! intr (next ints))
             (mp:midi :key (transpose oct intr) :dur (* rate 1.5))
             (wait rate)
             (set! reps (- reps 1))
             )))

(sprout (bw 120 bwoctaves bwmotion .125))

(sprout (list (bw 120 bwoctaves bwmotion .125)
              (bw 120 bwoctaves bwmotion .125))
        '(0 2))
        
