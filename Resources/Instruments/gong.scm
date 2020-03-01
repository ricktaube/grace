;;; -------- FM-GONG
;;; Paul Weineke's gong.

(definstrument (gong start-time duration frequency amplitude
		     (degree 0.0) (distance 1.0) (reverb-amount 0.005))
  (let ((mfq1 (* frequency 1.16))
	(mfq2 (* frequency 3.14))
	(mfq3 (* frequency 1.005)))
    (let ((indx01 (hz->radians (* .01 mfq1)))
	  (indx11 (hz->radians (* .30 mfq1)))
	  (indx02 (hz->radians (* .01 mfq2)))
	  (indx12 (hz->radians (* .38 mfq2)))
	  (indx03 (hz->radians (* .01 mfq3)))
	  (indx13 (hz->radians (* .50 mfq3)))
	  (atpt 5)
	  (atdur (* 100 (/ .002 duration)))
	  (expf '(0 0  3 1  15 .5  27 .25  50 .1  100 0))  
	  (rise '(0 0  15 .3  30 1.0  75 .5  100 0))
	  (fmup '(0 0  75 1.0  98 1.0  100 0))
	  (fmdwn '(0 0  2 1.0  100 0)))
      (let ((ampfun (make-env (stretch-envelope expf atpt atdur)
			      :scaler amplitude :duration duration))
	    (indxfun1 (make-env fmup :duration duration
				:scaler (- indx11 indx01) :offset indx01))
	    (indxfun2 (make-env fmdwn :duration duration
				:scaler (- indx12 indx02) :offset indx02))
	    (indxfun3 (make-env rise :duration duration
				:scaler (- indx13 indx03) :offset indx03))
	    (loc (make-locsig degree distance reverb-amount))
	    (carrier (make-oscil frequency))
	    (mod1 (make-oscil mfq1))
	    (mod2 (make-oscil mfq2))
	    (mod3 (make-oscil mfq3))
	    (beg (seconds->samples start-time))
	    (end (seconds->samples (+ start-time duration))))
	(do ((i beg (+ i 1)))
	    ((= i end))
	  (locsig loc i (* (env ampfun) 
			   (oscil carrier (+ (* (env indxfun1) (oscil mod1))
					     (* (env indxfun2) (oscil mod2))
					     (* (env indxfun3) (oscil mod3)))))))))))

;;; (with-sound () (gong 0 3 261.61 .6))

