;; A simple fm instrument with envelopes

(definstrument (fm time duration frequency amplitude
                   (ampenv '(0 0 25 1 75 1 100 0))
                   (cmratio 1) (index 1) (indenv '(0 1 100 1))
                   (degree 0) (distance 0) (reverb 0))
  "fm (time duration frequency amplitude
            (ampenv '(0 0 10 1 90 1 100 0))
            (cmratio 1)
            (index 1)
            (indenv '(0 0 100 0))
            (degree 0)
            (distance 0)
            (reverb 0))
 A simple fm instrument with envelope controls.
 (with-sound () (fm 0 1 440 .1))"
  (let* ((beg (seconds->samples time))
         (end (seconds->samples (+ time duration)))
         (car (make-oscil :frequency frequency))
         (mod (make-oscil :frequency (* frequency cmratio)))
         (ampf (make-env ampenv :scaler amplitude 
                         :duration duration))
         (devf (make-env indenv :duration duration
                         :scaler (hz->radians (* frequency cmratio index))))
         (loc (make-locsig :degree degree :distance distance 
                           :reverb reverb)))
    (do ((i beg (+ i 1)))
	   ((= i end) #t)
	 (locsig loc i (* (env ampf) 
			  (oscil car (* (env devf) 
					(oscil mod))))))))

#|
(with-sound ()
  (fm 0 1 440 .2))

;; adding envelopes to amplitude and FM index value

(with-sound ()
  (fm 0 4 440 .2 :ampenv '(0 0 25 1 98 1 100 0) 
      :index 5 :indenv '(0 1 100 0)))

;; a process that plays a list of modulating ratios

(define (mrats freq mrats rate dur ind amp)
  (process for rat in mrats
    do (fm (elapsed #t) dur freq amp
           :cmratio rat :index ind)
    (wait rate)))

;; plays differnt c/m mratios

(let ((rats (loop for r from 1 to 10 by .5 collect r)))
  (sprout (mrats 220 rats 2 2 5 .2) "test.wav"))

|#

