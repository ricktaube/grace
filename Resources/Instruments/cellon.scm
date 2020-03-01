
;;; STK's feedback-fm instrument named CelloN in Sambox-land

(definstrument (cellon beg dur pitch0 amp ampfun betafun 
		       beta0 beta1 betaat betadc ampat ampdc dis pcrev deg
		       pitch1 glissfun glissat glissdc
		       pvibfreq pvibpc pvibfun pvibat pvibdc
		       rvibfreq rvibpc rvibfun)
  (let ((st (seconds->samples beg))
	(nd (seconds->samples (+ beg dur)))
	(pit1 (if (zero? pitch1) pitch0 pitch1))
	(loc (make-locsig deg dis pcrev))
	(carrier (make-oscil pitch0))
	(low (make-one-zero .5 -.5))
	(fm 0.0)
	(fmosc (make-oscil pitch0))
	(pvib (make-triangle-wave :frequency pvibfreq :amplitude 1.0))
	(rvib (make-rand-interp :frequency rvibfreq :amplitude 1.0))
	(ampap (if (> ampat 0.0) (* 100 (/ ampat dur)) 25))
	(ampdp (if (> ampdc 0.0) (* 100 (- 1.0 (/ ampdc dur))) 75))
	(glsap (if (> glissat 0.0) (* 100 (/ glissat dur)) 25))
	(glsdp (if (> glissdc 0.0) (* 100 (- 1.0 (/ glissdc dur))) 75))
	(betap (if (> betaat 0.0) (* 100 (/ betaat dur)) 25))
	(betdp (if (> betadc 0.0) (* 100 (- 1.0 (/ betadc dur))) 75))
	(pvbap (if (> pvibat 0.0) (* 100 (/ pvibat dur)) 25))
	(pvbdp (if (> pvibdc 0.0) (* 100 (- 1.0 (/ pvibdc dur))) 75)))
    (let ((pvibenv (make-env (stretch-envelope (or pvibfun '(0 1 100 1)) 25 pvbap 75 pvbdp) :duration dur
			     :scaler (hz->radians (* pvibpc pitch0))))
	  (rvibenv (make-env (or rvibfun '(0 1 100 1)) :duration dur
			     :scaler (hz->radians (* rvibpc pitch0))))
	  (glisenv (make-env (stretch-envelope (or glissfun '(0 0 100 0)) 25 glsap 75 glsdp) :duration dur
			     :scaler (hz->radians (- pit1 pitch0))))
	  (amplenv (make-env (stretch-envelope ampfun 25 ampap 75 ampdp) :scaler amp :duration dur))
	  (betaenv (make-env (stretch-envelope betafun 25 betap 75 betdp) :duration dur
			     :scaler (- beta1 beta0) :offset beta0)))
      (if (and (= pitch0 pitch1)
	       (or (zero? pvibfreq)
		   (zero? pvibpc))
	       (or (zero? rvibfreq)
		   (zero? rvibpc)))
	  (do ((i st (+ i 1)))
	      ((= i nd))
	    (set! fm (one-zero low (* (env betaenv) (oscil fmosc fm))))
	    (locsig loc i (* (env amplenv) (oscil carrier fm))))
	  (do ((i st (+ i 1)))
	      ((= i nd))
	    (let ((vib (+ (* (env pvibenv) (triangle-wave pvib))
			  (* (env rvibenv) (rand-interp rvib))
			  (env glisenv))))
	      (set! fm (one-zero low (* (env betaenv) (oscil fmosc (+ fm vib)))))
	      (locsig loc i (* (env amplenv) 
			       (oscil carrier (+ fm vib))))))))))

