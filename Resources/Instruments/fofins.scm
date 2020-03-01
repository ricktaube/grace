;;; -------- FOF

(definstrument (fofins beg dur frq amp vib f0 a0 f1 a1 f2 a2 (ae '(0 0 25 1 75 1 100 0)) ve)
  "(fofins beg dur frq amp vib f0 a0 f1 a1 f2 a2 (ampenv '(0 0 25 1 75 1 100 0)) vibenv) produces FOF 
synthesis: (fofins 0 1 270 .2 .001 730 .6 1090 .3 2440 .1)"
  (let ((foflen (if (= *clm-srate* 22050) 100 200)))
    (let ((start (seconds->samples beg))
	  (end (seconds->samples (+ beg dur)))
	  (ampf (make-env ae :scaler amp :duration dur))
	  (vibf (make-env (or ve '(0 1 100 1)) :scaler vib :duration dur))
	  (frq0 (hz->radians f0))
	  (frq1 (hz->radians f1))
	  (frq2 (hz->radians f2))
	  (vibr (make-oscil 6))
	  (win-freq (/ (* 2.0 pi) foflen))
	  (wt0 (make-wave-train :size foflen :frequency frq)))
      (let ((foftab (mus-data wt0)))
	(do ((i 0 (+ i 1)))
	    ((= i foflen))
	  (float-vector-set! foftab i (* (+ (* a0 (sin (* i frq0)))
				   (* a1 (sin (* i frq1)))
				   (* a2 (sin (* i frq2))))
				.5 (- 1.0 (cos (* i win-freq)))))))
      (do ((i start (+ i 1)))
	  ((= i end))
	(outa i (* (env ampf) 
		   (wave-train wt0 (* (env vibf) 
				      (oscil vibr)))))))))

