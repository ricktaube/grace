(definstrument (exp-snd file beg dur amp (exp-amt 1.0) (ramp .4) (seglen .15) (sr 1.0) (hop .05) ampenv)
  ;; granulate with envelopes on the expansion amount, segment envelope shape,
  ;; segment length, hop length, and input file resampling rate
  (let ((max-seg-len (if seglen (if (pair? seglen) (max-envelope seglen) seglen) .15))
	(initial-seg-len (if seglen (if (pair? seglen) (cadr seglen) seglen) .15))
	(rampdata (if (pair? ramp) ramp (list 0 ramp 1 ramp)))
	(max-out-hop (if hop (if (pair? hop) (max-envelope hop) hop) .05))
	(initial-out-hop (if hop (if (pair? hop) (cadr hop) hop) .05))
	(min-exp-amt (if exp-amt (if (pair? exp-amt) (min-envelope exp-amt) exp-amt) 1.0))
	(initial-exp-amt (if exp-amt (if (pair? exp-amt) (cadr exp-amt) exp-amt) 1.0)))
    (if (or (<= (min-envelope rampdata) 0.0)
	    (>= (max-envelope rampdata) 0.5))
	(format #t "ramp argument to exp-snd must always be between 0.0 and 0.5: ~A" ramp)
	(let ((st (seconds->samples beg))
	      (nd (seconds->samples (+ beg dur)))
	      (f0 (make-readin file 0))
	      (expenv (make-env (if (pair? exp-amt) 
				    exp-amt
				    (list 0 exp-amt 1 exp-amt))
				:duration dur))
	      (lenenv (make-env (if (pair? seglen) 
				    seglen
				    (list 0 seglen 1 seglen))
				:scaler *clm-srate* :duration dur))
	      (scaler-amp (if (> max-seg-len .15) (/ (* 0.6 .15) max-seg-len) 0.6))
	      (srenv  (make-env (if (pair? sr) 
				    sr 
				    (list 0 sr 1 sr))
				:duration dur))
	      (rampenv (make-env rampdata :duration dur))
	      (initial-ramp-time (if ramp (if (pair? ramp) (cadr ramp) ramp) .4))
	      (max-in-hop (/ max-out-hop min-exp-amt)))
	  (let ((max-len (seconds->samples (+ (max max-out-hop max-in-hop) max-seg-len)))
		(hopenv (make-env (if (pair? hop) 
				      hop
				      (list 0 hop 1 hop))
				  :duration dur))
		(ampe (make-env (or ampenv '(0 0 .5 1 1 0)) :scaler amp :duration dur)))
	    (let ((exA (make-granulate :expansion initial-exp-amt
				      :input f0
				      :max-size max-len
				      :ramp initial-ramp-time 
				      :hop initial-out-hop
				      :length initial-seg-len 
				      :scaler scaler-amp))
		  (vol (env ampe)))
	      (do ((valA0 (* vol (granulate exA)))
		   (valA1 (* vol (granulate exA)))
		   (ex-samp 0.0)
		   (next-samp 0.0)
		   (i st (+ i 1)))
		  ((= i nd))
		(let ((sl (env lenenv))) ;current segment length
		  ;; now we set the granulate generator internal state to reflect all these envelopes
		  (set! vol (env ampe))
		  (set! (mus-length exA) (round sl))
		  (set! (mus-ramp exA) (floor (* sl (env rampenv))))) ;current ramp length (0 to .5)
		(set! (mus-frequency exA) (env hopenv))               ;current hop size
		(set! (mus-increment exA) (env expenv))               ;current expansion amount
		(set! next-samp (+ next-samp (env srenv)))            ;current resampling increment
		(if (> next-samp (+ 1 ex-samp))
		    (let ((samps (floor (- next-samp ex-samp))))
		      (if (> samps 2)
			  (do ((k 0 (+ k 1)))
			      ((= k (- samps 2)))
			    (granulate exA)))
		      (set! valA0 (if (>= samps 2)
				      (* vol (granulate exA))
				      (set! valA0 valA1)))
		      (set! valA1 (* vol (granulate exA)))
		      (set! ex-samp (+ ex-samp samps))))
		
		(outa i (if (= next-samp ex-samp)
			    valA0
			    (+ valA0 (* (- next-samp ex-samp) (- valA1 valA0))))))))))))

;;; (with-sound (:statistics #t) (exp-snd "fyow.snd" 0 3 1 '(0 1 1 3) 0.4 .15 '(0 2 1 .5) 0.05))
;;; (with-sound () (exp-snd "oboe.snd" 0 3 1 '(0 1 1 3) 0.4 .15 '(0 2 1 .5) 0.2))
