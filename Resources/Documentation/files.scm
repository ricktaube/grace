;
;; Output to Files
; 

; To run examples put the cursor at the end of each expression and
; press Command-Return. Look in the console window for any output.

; Use these macros to write musical output to different kinds of files:

; (with-sound (&key ...) ...)           clm/audio file
; (with-midi (file &key ...) ...)       midi file 
; (with-fomus (file &key ...) ...)      lilypond or music xclm file
; (with-csound (file &key ...) ...)     csound score file

; Each macro generates its specific kind file in non-real (faster than
; real) time. Inside the body of the macros you call functions
; (i.e. instruments or 'send' methods) to generate sound output. The
; time stamps you pass to these functions should be the true score
; times of the events. You can specify events out of order and use
; loops to perform iteration. Here are a few examples.

; A CLM audio file

(load "v.scm")

(with-sound (:output "test.wav" :srate 44100 :channels 2
                     :data-format mus-lshort
                     :header-type  mus-riff)
  (loop for t to 4 by .25
        do (fm-violin t .4 (hertz (between 48 84)) .5))
  )

; A midi file

(with-midi ("test.mid" )
  (loop for t to 4 by .25
        do (mp:midi t 1 (between 48 84) .5))
  )

; A lilypond file (must have Fomus installed)

(with-fomus ("test.ly" :parts '((:id "pno" :inst "piano")))
  (loop for t to 4 by .25
        do (fms:note t .25 (between 48 84) "pno"))
  )

; A csound score file

(with-csound ("testsco.sco" )
  (loop for t to 4 by .25
        do (cs:i 1 t .25 (hertz (between 48 84)) .5))
  )

