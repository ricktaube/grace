;;; -------- FM-BELL
(definstrument (fm-bell startime dur frequency amplitude amp-env index-env index)
  "(fm-bell startime dur frequency amplitude amp-env index-env index) mixes in one fm bell note"
  (let ((fmInd2 (hz->radians (* 4.0 (- 8.0 (/ frequency 50.0))))))
    (let ((beg (seconds->samples startime))
					;(len (seconds->samples dur))
	  (end (seconds->samples (+ startime dur)))
	  (fmInd1 (hz->radians (* 32.0 frequency)))
	  
	  (fmInd3 (* fmInd2 0.705 (- 1.4 (/ frequency 250.0))))  
	  (fmInd4 (hz->radians (* 32.0 (- 20 (/ frequency 20)))))
	  (mod1 (make-oscil (* frequency 2)))
	  (mod2 (make-oscil (* frequency 1.41)))
	  (mod3 (make-oscil (* frequency 2.82)))
	  (mod4 (make-oscil (* frequency 2.4)))
	  (car1 (make-oscil frequency))
	  (car2 (make-oscil frequency))
	  (car3 (make-oscil (* frequency 2.4)))
	  (indf (make-env (or index-env 
			      '(0 1 2 1.1 25 .75 75 .5 100 .2))
			  (or index 1.0) dur))
	  (ampf (make-env (or amp-env 
			      '(0 0 .1 1 10 .6 25 .3 50 .15 90 .1 100 0))
			  amplitude dur)))
      (do ((i beg (+ i 1)))
	  ((= i end))
	(let ((fmenv (env indf)))
	  (outa i (* (env ampf)
		     (+ (oscil car1 (* fmenv fmInd1 (oscil mod1)))
			(* .15 (oscil car2 (* fmenv 
					      (+ (* fmInd2 (oscil mod2))
						 (* fmInd3 
						    (oscil mod3))))))
			(* .15 (oscil car3 (* fmenv 
					      fmInd4 
					      (oscil mod4))))))))))))


;(define fbell '(0 1 2 1.1000 25 .7500 75 .5000 100 .2000 ))
;(define abell '(0 0 .1000 1 10 .6000 25 .3000 50 .1500 90 .1000 100 0 ))
;(fm-bell 0.0 1.0 220.0 .5 abell fbell 1.0)

