;
;; Plotting
;

; To evaluate code put the cursor after each expression and press
; Command-Return, then check the console window for any output.

; Three layers of points in different styles

(plot :title "My Plots"
      :xaxis "unit"
      :yaxis "unit"
      :layer '(0 0 .25 .9 .75 .9 1 0) :title "foo" :style "bar"
      :layer '(0 1 .15 .5 .30 .2 .60 .1 100 0) :title "bar" :style "point"
      :layer '(0 0 .5 1 1 0) :title "baz" :style "envelope"
      )

;
;; Compute layers on the fly
;

(plot (loop for x from 0 to 1 by .2 append (list x (random 1.0)))
      (loop for x from 0 to 1 by .1 append (list x x))
      (loop for x from 0 to 1 by .25 append (list x (expt 10 (- x))))
      )

;
;; Piano roll plot of points with 5 fields of data
;

(loop with amp = .7
  repeat 20
  for beg = 0 then (+ beg  (random 1.0))
  for dur = (pick .1 .2 .3)
  for knum = (between 60 96)
  for chan = (random 16)
  collect (list beg dur knum amp chan) into poi
  finally
  (plot :title "My Notes"
        :fields '((time (seconds 0 10)) ; time's axis is 10 seconds
                  (dur time .5)         ; dur field uses time's axis
                  (key (keynum 60 96))  ; key is keynum between 60 to 90
                  (amp unit)            ; amp is 0 to 1
                  (chan (0 15)))        ; chan is 0 to 15
        :xaxis '(time dur)              ; time and dur on X
        :yaxis "key"                    ; key numbers on Y
        :layer poi                      ; one layer of data
        :style "hbox"              ; display as horizontal boxes
        ))

; Create an empty plot window, add points by Control-clicking
; then get points back from window using plot-data

(plot )

(define mydata (plot-data "Untitled Plot"))

; Adding points by mouse hook (Control-Option mouseclick)

(define (myhook x y)
  (let* ((spec (fm-spectrum y (between 1.5 3.0) 3))
         (keys (spectrum-keys spec :quant 1 :unique #t
                              :min (- y 17) :max (+ y 17))))
    (loop for k in keys collect (list x k))))


; (myhook .1 60)

(plot-hook "my fm chords" myhook)

(plot :title "my fm chords" :xaxis '(0 10) :yaxis '(0 127))

; Plot a distribution and its histogram in the same window

(let ((maxh 0)
      (hist (make-list 100 0))
      (rans (list))
      (bars (list)))

  (loop for x from 0 below 100
        for y = (floor (* (+ (random 100) (random 100)) .5))
        do
        (set! rans (append rans (list x y)))
        (list-set! hist y (+ (list-ref hist y) 1)))

  (set! maxh (apply max hist))

  (loop for y in hist
        for x from 0
        do 
        (if (> y 0) (set! bars (append bars (list x (* (/ y maxh) 100.0))))))

  (plot :title "Distribution and histogram"
        :x-axis :percentage
        :y-axis :percentage
        :layer rans :style :point
        :layer bars :style :impulse))

; Plot a "sampled sine wave"

(let ((wave (loop for x from 0 to 1 by .01
                  for r = (* 2 pi x)
                  append (list r (sin r))))
      (samp (loop for x from 0 to 1 by .05
                  for r = (* 2 pi x)
                  append (list r (sin r)))))
  (plot :x-axis (list 0 (* 2 pi) (/ pi 2) 2)
        :y-axis '(-1 1 .5)
        :layer wave :style :line
        :layer samp :style :vlineandpoint))

; Michael Klingbeil's stretched harmonics example

(begin

  (define (arpeggiate-exprhy keynums offset duration rate midpoint-frac
                             amplow amphi legato bass-legato
                             bass-cutoff last-legato)
    (let* ((segn (length keynums))
           (lastn (- segn 1))
           (midpoint (floor (* segn midpoint-frac)))
           ;; rhythms below midpoint follow one curve above another.
           (rhyts (append (segs midpoint (* midpoint-frac  duration) 1 rate)
                          (segs (- segn  midpoint)
                                (* (- 1 midpoint-frac) duration)
                                1 
                                (/ 1 rate))))
           (data (list)))
      
      (loop with time = 0
            for i from 0
            for k in keynums
            for d in rhyts
            for r = (if (< k  bass-cutoff)
                        bass-legato
                        (if (= i lastn)
                            (* last-legato d)
                            (* legato d)))
            for a = (between 0.45 0.5)
            collect (list (+  offset time)
                          k
                          r
                          a)
            do (set! time (+ time d)))))

  (define (distort-harmonics fund distort)
    (loop for h from 1 below (floor (/ 25.0 distort) )
          if (odds (* 0.9 distort))
          collect (key (* fund (expt h distort)))))
  
  (define (arpa-harmonic fundnote dur gap)
    ;; spawn overlapping arpeggios with mean duration of dur and mean
    ;; gap between arpeggio starts of gap seconds. each arpeggio is
    ;; upward with the general direction of arpeggio starting notes
    ;; moving downward
    (let ((result (list)))

      (define (sorter a b) (< (first a) (first b)))

      (loop with fund = (hertz fundnote) and time = 0
            for distort from 0.7 below 1.05 by 0.05
            for notes = (distort-harmonics fund distort)
            for arpa = (arpeggiate-exprhy notes
                                          time
                                          (* (vary dur 0.1) distort)
                                          (between 4.0 0.25)
                                          (between 0.3 0.6)
                                          0.3  ; amplow
                                          0.8  ; amphi
                                          (* dur distort 0.7) ; bass legato
                                          2.0  ; legato 
                                          59   ; bass cutoff
                                          1.0)
            do (set! result (sort! (append result arpa) sorter))
            (set! time (+ time (vary gap 0.4))))
      result))

; now create two layers of harmonically generated notes

  (plot :title "Arpa Harmonic"
        :style :pianoroll
        :fields '((start (seconds 0 40))
                  (keyn keynum)
                  (dur start)
                  (amp unit))
        :layer (arpa-harmonic "g1" 7.0 5.0)
        :layer (arpa-harmonic "g1" 7.0 5.0)
        )
  )
