;
;; Fomus Examples
;

; To evaluate code put the cursor after each expression and press
; Command-Return, then check the console window for any output.

; Fomus generates Lilypond or MusicXml scores. In order use Fomus you
; must build Grace with the Fomus package enabled.

; To run examples put the cursor at the end of each expression and
; press Command-Return. Look in the console window for any output.

; Simple example

(define (examp1 n r)
  (process repeat n
           do (fms:note :dur r :pitch (between 48 73))
           (wait r)))

(sprout (examp1 16 1/2) "fomus1.ly")

;
;; Staccato and Accent Marks
;

(define (examp2 )
  (process with marks = '(#f (".") (">"))
           repeat 21
           do (fms:note :dur (if (< (elapsed) 10) 1/2 1)
                        :pitch (between 48 73) :marks (nth marks (random 3)))
           (wait 1/2)))
  
(let ((parts '((:id "apart" :name "Piano" :inst "piano"))))
  (sprout (examp2) "fomus2.ly" :parts parts)
  )

;
;; Quartertones
;

(define (examp3 )
  (process repeat 21
           do (fms:note :dur (if (< (elapsed) 10) 1/2 1)
                        :pitch (+ 70 (/ (random 4) 2)))
           (wait 1/2)))

(let ((parts '((:id "apart" :name "Piano" :inst "piano")))
      (sets '(:quartertones #t)))
  (sprout (examp3 ) "fomus3.ly" :parts parts :sets sets)
  )

;
;; Polyphony with Slurs
;

(define (examp4 )
  (process for v from 1 to 2
           do
           (loop for off from 0 to 10 by 1/2
                 do (fms:note :time off
                              :dur (if (< off 10) 1/2  1)
                              :pitch (if (= v 2) (between 35 60) (between 60 85))
                              :voice v :marks (odds .333 '("(.."))))))

(let ((parts '((:id "apart" :name "Piano" :inst "piano"))))
  (sprout (examp4 ) "fomus4.ly" :parts parts)
  )

;
;; Piano Chords
;

(define (examp5 )
  (process repeat 3
           do
           (loop for off from 0 to 10 by 1/2
                 do (fms:note :time off
                              :dur (if (< off 10) 1/2 1)
                              :pitch (between 48 73)
                              :voice '(1 2)))))
       
(let ((parts '((:id "apart" :name "Piano" :inst "piano"))))
  (sprout (examp5) "fomus5.ly" :parts parts)
  )

;
;; Quantizing/Chords
;

(define (examp6 )
  (process repeat 10
           for tim = (random 30.0)
           for dur = (+ 1 (random 3.0))
           do (fms:note :time tim
                        :dur dur :pitch (between 60 85))))
       
(let ((parts '((:id "apart" :name "Piano" :inst "piano"))))
  (sprout (examp6 ) "fomus6.ly" :parts parts)
  )

;
;; Quantizing/Voices
;

(define (examp7 )
  (process repeat 10
           for tim = (random 30.0)
           for dur = (+ 1 (random 3.0))
           do (fms:note :time tim
                        :dur dur
                        :pitch (between 60 85)
                        :voice '(1 2 3))))

(let ((parts '((:id "apart" :name "Piano" :inst "piano"))))
  (sprout (examp7) "fomus7.ly" :parts parts)
  )

;
;; Mark Objects
;

(define (examp8-notes )
  (process for o from 0 to 20 by 1/2
           do
           (fms:note :time o :dur 1/2 :pitch 72 :part "0")
           (fms:note :time o :dur 1/2 :pitch 48 :part "1") ))
  
(define (examp8-marks )
  (process repeat 8
           do (fms:mark :part (odds .5 "0" "1")
                        :time (random 20.0)
                        :marks '("^"))))
       
(let ((parts '((:name "Flute" :inst "flute" :id "0") 
               (:name "Tuba" :inst "tuba" :id "1"))))
  (sprout (list (examp8-notes) (examp8-marks)) "fomus8.ly" 
          :parts parts)
  )

;
;; Percussion 1
;

(define (examp9 )
  (process for o from 0 to 20 by 1/2 
           do (fms:note :time o :dur 1/2
                        :pitch (odds .5 "wb1" "wb2"))))

(let ((parts '((:id "perc" :name "Percussion"
                    :inst (:template "percussion" :percinsts
                                     ((:id "wb1" :template "low-woodblock" :perc-note 57)
                                      (:id "wb2" :template "high-woodblock" :perc-note 64)))))))
  (sprout (examp9 ) "fomus9.ly" :parts parts)
  )

;
;; Percussion 2
;

(define (examp10 )
  (process for o from 0 to 20 by 1/2 
           do (fms:note :time o :dur 1/2
                        :pitch (odds .5 "wb1" "wb2"))))

(let ((parts '((:id "perc" :name "Percussion"
                    :inst (:template "percussion" :percinsts
                                     ((:id "wb1" 
                                           :template "low-woodblock" 
                                           :perc-note 57 
                                           :perc-voice 2)
                                      (:id "wb2" 
                                           :template "high-woodblock"
                                           :perc-note 64
                                           :perc-voice 1)))))))
  (sprout (examp10) "fomus10.ly" :parts parts)
  )

;
;; (Budget) Orchestra Score
;

(define (examp11-notes p)
  (process for n in '(60 62 64)
           for o from 0
           do (fms:note :part p :dur 1 :pitch n)
           (wait 1)))

(define (examp11-parts ps)
  (process for p in ps
           do (sprout (examp11-notes p))))
  
(let ((parts '((:id "fl1" :name "Flute 1" :inst "flute")
               (:id "fl2" :name "Flute 2" :inst "flute")
               (:id "cl2" :name "Clarinet 1" :inst "bflat-clarinet")
               (:id "vln1" :name "Violin 1" :inst "violin")
               (:id "vln2" :name "Violin 2" :inst "violin")
               (:id "vc1" :name "Cello 1" :inst "cello")
               (:id "vc2" :name "Cello 2" :inst "cello")
               (:id "tba" :name "Tuba" :inst "tuba")))
      (sets '(:layout "orchestra")))

  (fms:meas :time 0 :dur 3)
  (sprout (examp11-parts '("fl1" "fl2" "cl2" "vln1" "vln2" "vc1" "vc2" "tba"))
          "fomus11.ly" :parts parts :sets sets)
  )

;
;; Small Ensemble Score
;

(define (examp12-notes p)
  (process for n in '(60 62 64)
           for o from 0
           do (fms:note :part p :dur 1 :pitch n)
           (wait 1)))
 
(define (examp12-parts ps)
  (process for p in ps
           do (sprout (examp12-notes p))))

(let ((parts '((:id "fl1" :name "Flute 1" :inst "flute")
               (:id "fl2" :name "Flute 2" :inst "flute")
               (:id "cl2" :name "Clarinet 1" :inst "bflat-clarinet")
               (:id "vln1" :name "Violin 1" :inst "violin")
               (:id "vln2" :name "Violin 2" :inst "violin")
               (:id "vc1" :name "Cello 1" :inst "cello")
               (:id "vc2" :name "Cello 2" :inst "cello")
               (:id "tba" :name "Tuba" :inst "tuba")))
      (sets '(:layout "small-ensemble")))

  (fms:meas :time 0 :dur 3)

  (sprout (examp12-parts '("fl1" "fl2" "cl2" "vln1" "vln2" "vc1" "vc2" "tba"))
          "fomus12.ly" :parts parts :sets sets)
  )

;
;; 2 Part Polyphony
;

(define (polygen len minp maxp)
  (process repeat len
           do (fms:note :voice '(1 2)
                        :pitch (between minp maxp) :dur 1/4)
           (wait 1/4)))

(sprout (list (polygen 40 50 80)
              (polygen 40 40 70)) "fomus13.ly")

;
;; Generate Prime Harmonics Gesture
;

(define (play-harmonics fund beat harm1 harm2 pname)
  (process for harm from harm1 to harm2
           for knum = (key (* (hertz fund) harm))
           do (fms:note :dur (rhythm beat) :pitch knum :part pname)
           (wait (rhythm beat))))

(loop with sets = '(:quartertones #t :tuplets 11)
      for ph in '(2 3 5 7 11)
      for pn in '("c2" "c3" "c5" "c7" "c11")
      collect (play-harmonics "c1" (/ 1 ph) ph (* ph 4) pn) into procs
      collect (concat :id pn '(:name "Cello" :inst "cello")) into parts
      finally
      (sprout procs "fomus14.ly" :parts parts :sets sets)
      )