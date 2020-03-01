;
;; Automatic Jazz
;

; To run code put your cursor after each expression and press
; Command-Return, then check the console window for any output.

; This tutorial presents an implementation of an simple automatic jazz
; program. The code is derived from a program originally written by
; Erik Flister at CCRMA, Stanford University, as a project for his
; undergraduate computer music class. His original program has been
; simplified and adapted here to work with General MIDI
; instruments. Flister's improviser generates music for a jazz trio
; (piano, acoustic bass and percussion). 

; Our first step will be to specify the appropriate MIDI program
; changes to establish the Jazz Combo instruments on channels 0,1 and
; 9 (drum track)

; Channel  0: Acoustic Piano
; Channel  1: Acoustic Bass (plucked)
; Channel  9: Percussion

(let* ((piano 0)
       (bass 32))
  (mp:instruments piano bass)
  )

; We dont have to specify a program change for the percussion part
; because MIDI defines channel 9 to be the "drum track", a specialized
; channel that maps key numbers to individual drum sounds. Here are
; key numbers the percussion part will use:

; Closed hi hat:      42
; Electric Snare:     40
; Acoustic Bass Drum: 35
; Ride Cymbal 1:      51
; Ride Cymbal 2:      59

; The automatic jazz program uses a conductor process. The conductor
; runs for a specified number of measures and sprouts piano,
; percussion and bass processes to improvise each measure. Each
; instrument process uses data passed to it from the conductor
; process. This data includes the jazz scale to improvise with, a
; transposition level for the jazz scale, a tempo factor and an
; overall amplitude level. The amplitude level is adjusted on a per
; measure basis by the main conductor process. The other data are
; defined as global variables that can be adjusted and redefined by
; the composer.

; Dorian mode with decorated octave

(begin
  (define jazz-scale '(0 2 3 5 7 9 10 12 14))
  (define jazz-changes (keynum '(bf3 ef4 bf3 bf ef4 ef bf3 bf f4 ef bf3 bf)))
  (define jazz-tempo 120)
  )

;
;; The Percussion Parts
;

; The percussion parts for the Jazz Combo consist of two ride cymbals,
; a high hat, snare and bass drums. We will introduce these parts in
; their order of complexity, from simplest to most difficult.

; The High Hat

; The High Hat percussion process is very simple, it just plays the
; High Hat on the second and fourth quarter Q of every measure and
; rest values ("rest") on the first and third beats. Each sound lasts
; for the duration one triplet eighth note ie 1/3 of a beat

(define (jazz-high-hat tmpo ampl)
  ;; generate a 4/4 measure of high-hat:
  ;;   rest hihat rest hihat
  (let* ((rhy (in-tempo 1 tmpo))
         (dur (in-tempo 1/3 tmpo))
         (amp .5)
         (pat (make-cycle '("rest" 42 "rest" 42))))
    (process repeat 4
             for x = (next pat)
             do
             (if (number? x)
                 (mp:midi :key x :chan 9 :dur dur :amp (* amp ampl)))
             (wait rhy)
             )))

; Listen to eight measures of the High Hat. Since the process
; generates only one measure, we collect eight "versions" of the
; process and offset each by two seconds, exactly the duration of the
; combo's 4/4 measure at tempo 120.

(let ((procs 
       (loop repeat 8
             collect (jazz-high-hat 120 1))))
  (sprout procs '(0 2 4 6 8 10 12 14))
  )

;
;; Jazz Drums 
;

; The jazz-drums process randomly selects between playing the snare,
; the bass drum or resting one quarter of the time. One tenth of the
; time the process produces a very loud tone.

(define (jazz-drums tmpo ampl)
  (let* ((elec-snare 40)
         (bass-drum 35)
         (knums (make-weighting (list '("rest" :weight .25)
                                      elec-snare
                                      bass-drum)))
         (rhys (make-cycle '(2/3 1/3)))
         (amps (make-weighting '(.7 (.95 :weight .1))))
         )
    (process repeat 8
             for k = (next knums)
             for a = (next amps)
             for r = (in-tempo (next rhys) tmpo)
             do
             (if (number? k)
                 (mp:midi :key k :chan 9 :dur r :amp (* a ampl)))
             (wait r)
             )))

; Now listen to eight measures of the drum and hi hat together.

(let ((procs (loop repeat 9
                   append (list (jazz-high-hat 120 .99)
                                (jazz-drums 120 .99)))))
  (sprout procs '(0 2 4 6 8 10 12 14))
  )

;
;; Cymbals
; 

; The cymbals process performs a constant stream of triplet eighths in
; which the ride1 cymbal is played on the beginning or every quarter
; note. The second and third triplets of each beat are either rests or
; a random choice between ride1, ride2 or a rest.  Here is the beat
; map for a measure of the process, where 1 means the ride1 cymbal is
; played, - means a rest and x means a random choice between ride1,
; ride2 or a rest:

; Triplet 8th: 1  2  3    4  5  6    7  8  9   10 11 12
; Cymbals:     1  -  x    1  -  1    1  x  x    1  x  1 

; The random elements marked x are created by this helper function.

(define (or12r wt)
  ;; play ride cymbal 1,2 or rest
  ;; wt is the weight of resting relative to playing.
  ;; return random pattern that slightly prefers playing a ride 1
  ;; pattern over a ride 2 pattern
  (let* ((ridecymb1 51)
         (ridecymb2 59)
         (restitem (list "rest" wt))
         (ride1 (make-weighting (list ridecymb1 restitem) 1))
         (ride2 (make-weighting (list ridecymb2 restitem) 1)))
    (make-weighting (list (list ride1 1.5) ride2) 2)
    ))

(define (jazz-cymbals tmpo ampl)
  (let* ((rhy (in-tempo 1/3 tmpo))
         (ridecymb1 51)
         (ridecymb2 59)
         (amps (make-cycle '(.6 .5 .9 .7 .5 1 .6 .5 .9 .7 .5 1)))
         (knums (make-cycle (list ridecymb1 "rest" (or12r 5)
                                  ridecymb1 "rest" ridecymb1
                                  ridecymb1 (or12r 7) (or12r 7)
                                  ridecymb1 (or12r 3) ridecymb1))))
    (process repeat 12
             for k = (next knums)
             for a = (next amps)
             do
             (if (number? k)
                 (mp:midi :key k :chan 9 :dur rhy :amp (* a ampl)))
             (wait rhy)
             )))

; Now listen to all three of the percussion parts together.

(let ((procs (loop repeat 8 
                   append (list (jazz-high-hat 120 1)
                                (jazz-drums 120 1)
                                (jazz-cymbals 120 1)))))
  (sprout procs '(0  0  0  2  2  2  4  4  4 6 6 6 8 8 8
                 10 10 10 12 12 12 14 14 14))
  )

;
;; Jazz Piano
;

; The jazz piano improvises jazz chords based on a pattern of root
; changes and a scale pattern that is transposed to each root. The
; piano randomly choose between playing triplet eighths or straight
; eights for a given measure.

(define (jazz-piano jscale on tmpo ampl)
  ;; generate a measure of jazz harmony.
  ;; measure contains either 8 or 12 notes.
  (let* ((reps (odds .65 8 12))
         (rhys (if (= reps 8)
                   (make-cycle '(2/3 1/3))
                   (make-cycle '(1/3))))
         (amps (if (= reps 8)
                   (make-weighting (list (make-cycle '(.5 .4 .7))
                                         (make-cycle '(.6 .5 .8))))
                   (make-weighting (list (make-cycle '(.5 .4 .7))
                                         (make-cycle '(.6 .5 .8))))))
         (knms (make-weighting (list (list (make-heap jscale (make-weighting '(1 2 3 4)))
                                           (make-weighting '(1.15 1.65)))
                                     "rest")))
         )
    (process repeat reps
             for r = (in-tempo (next rhys) tmpo)
             for a = (next amps)
             for l = (next knms #t)
             do
             (loop for k in l
                   do
                   (if (number? k)
                       (mp:midi :key (transpose k on) :dur r :amp (* a ampl) :chan 0)))
             (wait r)
             )))

; Each measure of the jazz-piano part will contain either 8 or 12
; notes determined by the odds function, which in this process chooses
; 8 notes per measure approximately 65% of the time, otherwise 12
; notes. The rhys variable is set to a pattern of rhythms that depends
; on the value of reps. If the piano plays 8 notes in the measure then
; the rhythmic pattern for the process will consists of triplet
; quarter (tq) followed by a triplet 8ths (te), otherwise the piano
; will play twelve triplet 8ths. The harmonies are generated by a
; random pattern that selects between a rest and a heap of notes
; created from the scale that was passed into process. Probability of
; choosing a note is either 1.16 or 1.65 relative to the rest and each
; time the heap is selected it will generate one to four notes.

(let* ((procs (loop repeat 4
                    collect (jazz-piano jazz-scale (keynum "bf3") 120 1))))
  (sprout procs '(0 2 4 6))
  )

;
;; Acoustic Bass
;

; The acoustic bass part is the most complex in terms of its
; implementation. The bass part plays a melodic line built out of
; tones from the jazz-scale's tonic seventh chord alternating with
; color tones outside the tonic chord. The process first divides the
; jazz scale's (0 2 3 5 7 9 10 11 12 14) into two sets. The tonic set
; contains the tonic seventh pitches 0 2, 4, 6 and 7 and the color set
; contains the decoration pitches 1, 3, 5, 7 and 9. The bass plays a
; series of 12 triplets per measure, on each triplet only one of the
; two sets is possible. On all but the first triplet a rest is also
; possible.

(define (getset jscale ints)
  ; return the notes in scale at the positions in ints.
  ; used to partition scale into tonic and decoration
  (loop for i in ints collect (list-ref jscale i))
  )

(define (rancyc data prob)
  ; create an element for a random pattern, elements datum
  ; is a cyclic pattern. element has :weight prob
  (list (make-cycle data) prob)
  )

(define (jazz-bass jscale on tmpo ampl)
  (let* ((k #f)
         (rhy (in-tempo 1/3 tmpo))
         (tonics (make-weighting (getset jscale '(0 2 4 6 7))))
         (colors (make-weighting (getset jscale '(1 3 5 6 8))))
         (amps (make-cycle '(.5 .4 1.0 .9 .4 .9 .5 .4 1.0 .9 .5 .9)))
         (durs (make-cycle '(2/3 1/3 1/3)))
         ;; beat map. t is tonic, c is color, r is rest
         (bmap (make-cycle (list
                            ;; 5 possible patterns for triplets 1-4
                            (make-weighting (list (rancyc '(:t r r :c) 1.0)
                                                  (rancyc '(:t r r r) .25)
                                                  (rancyc '(:t r :t :c) .22)
                                                  (rancyc '(:t :c :t :c) .065)
                                                  (rancyc '(:t :c :t r) .014))
                                            1)
                            ;; 5 possible patterns for 5-7
                            (make-weighting (list (rancyc '(r r :t) 1.0)
                                                  (rancyc '(r r r) .25)
                                                  (rancyc '(r :c :t) .22)
                                                  (rancyc '(:t :c :t) .038)
                                                  (rancyc '(:t :c r) .007))
                                            1) 
                            ;; 5 possible patterns for 8-10
                            (make-weighting (list (rancyc '(r r :c) 1.0)
                                                  (rancyc '(r :t :c) .415)
                                                  (rancyc '(r r r) .25)
                                                  (rancyc '(:c :t :c) .11)
                                                  (rancyc '(:c :t r) .018))
                                            1)
                            ;; two possible values for 11
                            (make-weighting '((r 1) (:t .25)) 1 )
                            ;; two possible values for 12
                            (make-weighting '((r 1) (:c .25)) 1))
                           )))
    (process repeat 12
             for x = (next bmap)
             for d = (in-tempo (next durs) tmpo)
             for a = (next amps)
             do
             (if (equal? x :t)
                 (set! k (next tonics))
                 (if (equal? x :c)
                     (set! k (next colors))
                     (set! k x)))
             (if (number? k)
                 (mp:midi :key (transpose k on) :chan 1 :dur d :amp (* a ampl)))
             (wait rhy)
             )))

(let ((procs (loop repeat 8
                   collect (jazz-bass jazz-scale (keynum "bf2") 120 1))))
  (sprout procs '(0 2 4 6 8 10 12 14))
  )

;
;; Conductor process
;

; The jazz-combo function is the conductor process. It does not make
; sound itself but rather sprouts other processes to make sound, each
; sprouted process generates one measure of a particular part.

(define (jazz-combo measures changes tempo jscale)
  (let* ((roots (make-cycle changes))
         (ampl 1))
    (process for meas below measures
             for root = (next roots)
             do
             (if (= 0 (modulo meas 12))
                 (set! ampl (between .5 1)))
             (sprout (jazz-piano jscale root tempo ampl))
             (sprout (jazz-cymbals tempo ampl))
             (sprout (jazz-high-hat tempo ampl))
             (sprout (jazz-drums tempo ampl))
             (sprout (jazz-bass jscale (transpose root -12) tempo ampl))
             (wait (in-tempo 4 tempo))
             )))

(sprout (jazz-combo 48 jazz-changes jazz-tempo jazz-scale))

