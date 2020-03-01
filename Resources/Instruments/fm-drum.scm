;;; -------- FM-DRUM
;;; Jan Mattox's fm drum:

(definstrument (fm-drum start-time duration frequency amplitude index 
			high (degree 0.0) (distance 1.0) (reverb-amount 0.01))
  (let (;; many of the following variables were originally passed as arguments
	(indxfun '(0  0     5  .014  10 .033  15 .061  20 .099  
		      25 .153  30 .228  35 .332  40 .477  
		      45 .681  50 .964  55 .681  60 .478  65 .332  
		      70 .228  75 .153  80 .099  85 .061  
		      90 .033  95 .0141 100 0))
	(indxpt (- 100 (* 100 (/ (- duration .1) duration))))
	(atdrpt (* 100 (/ (if high .01 .015) duration))))
    (let ((divindxf (stretch-envelope indxfun 50 atdrpt 65 indxpt))
	  (ampfun '(0 0  3 .05  5 .2  7 .8  8 .95  10 1.0  12 .95  20 .3  30 .1  100 0))
	  (casrat (if high 8.525 3.515))
	  (fmrat (if high 3.414 1.414))
	  (glsfun '(0 0  25 0  75 1  100 1)))
      (let ((beg (seconds->samples start-time))
	    (end (seconds->samples (+ start-time duration)))
	    (glsf (make-env glsfun :scaler (if high (hz->radians 66) 0.0) :duration duration))
	    (ampf (let ((ampe (stretch-envelope ampfun 
						10 atdrpt 
						15 (max (+ atdrpt 1) 
							(- 100 (* 100 (/ (- duration .2) duration)))))))
		    (make-env ampe :scaler amplitude :duration duration)))
	    (indxf (make-env divindxf :scaler (min (hz->radians (* index fmrat frequency)) pi) :duration duration))
	    (mindxf (make-env divindxf :scaler (min (hz->radians (* index casrat frequency)) pi) :duration duration))
	    (devf (let ((deve (stretch-envelope ampfun 
						10 atdrpt 
						90 (max (+ atdrpt 1) 
							(- 100 (* 100 (/ (- duration .05) duration)))))))
		    (make-env deve :scaler (min pi (hz->radians 7000)) :duration duration)))
	    (loc (make-locsig degree distance reverb-amount))
	    (rn (make-rand :frequency 7000 :amplitude 1.0))
	    (carrier (make-oscil frequency))
	    (fmosc (make-oscil (* frequency fmrat)))
	    (cascade (make-oscil (* frequency casrat))))
	(do ((i beg (+ i 1)))
	    ((= i end))
	  (let ((gls (env glsf)))
	    (locsig loc i (* (env ampf) 
			     (oscil carrier 
				    (+ gls 
				       (* (env indxf)
					  (oscil fmosc 
						 (+ (* gls fmrat)
						    (* (env mindxf) 
						       (oscil cascade 
							      (+ (* gls casrat)
								 (* (env devf) (rand rn))))))))))))))))))
#|
(with-sound ()
	    (fm-drum 0 1.5 55 .3 5 #f)
	    (fm-drum 2 1.5 66 .3 4 #t))
|#

