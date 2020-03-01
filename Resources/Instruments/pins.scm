;;; spectral modeling (SMS)

(definstrument (pins beg dur file amp
		     (transposition 1.0) ; this can be used to transpose the sound
			  (time-scaler 1.0)    ; this can make things happen faster (< 1.0)/slower (> 1.0) in the output
			  (fftsize 256)        ; should be a power of 2
			  ;; at 22050 srate, this is ok for sounds above 300Hz or so, below that you need 512 or 1024,
			  ;; at 44100, probably best to double these sizes -- it takes some searching sometimes.
			  (highest-bin 128)    ; how high in fft data should we search for peaks
			  (max-peaks 16)       ; how many spectral peaks to track at the maximum
			  attack)	       ; whether to use original attack via time domain splice
  ;; do the sliding fft shuffle, translate to polar coordinates, find spectral peaks,
  ;;   match with current, do some interesting transformation, resynthesize using oscils
  ;;   All the envelopes are created on the fly.  max-peaks is how many of these peaks
  ;;   we are willing to track at any given time.
  (let ((max-peaks-1 max-peaks)
	(fftsize-1 fftsize)
	(highest-bin-1 highest-bin)
	(start (seconds->samples beg))
	(attack-size (or attack 1)))
    
    (let* ((hop (floor (/ fftsize-1 4)))
	   (outhop (floor (* time-scaler hop))))
      (let ((ifreq (/ 1.0 outhop))
	    (max-oscils (* 2 max-peaks-1)))
	(let ((end (+ start (seconds->samples dur)))
	      (fil (make-readin file))
	      (fdr (make-float-vector fftsize-1))
	      (fdi (make-float-vector fftsize-1))
	      (window (make-fft-window blackman2-window fftsize-1))
	      (current-peak-freqs (make-float-vector max-oscils))
	      (last-peak-freqs (make-float-vector max-oscils))
	      (current-peak-amps (make-float-vector max-oscils))
	      (last-peak-amps (make-float-vector max-oscils))
	      (peak-amps (make-float-vector max-peaks-1))
	      (peak-freqs (make-float-vector max-peaks-1))
	      (amps (make-float-vector max-oscils))	;run-time generated amplitude and frequency envelopes
	      (rates (make-float-vector max-oscils))
	      (freqs (make-float-vector max-oscils))
	      (sweeps (make-float-vector max-oscils))
	      ;; (lowest-magnitude .001)
	      
	      (ihifreq (hz->radians ifreq))
	      (fftscale (/ 1.0 fftsize-1 .42323)) ;integrate Blackman-Harris window = .42323*window width and shift by fftsize-1
	      (fft-mag (/ *clm-srate* fftsize-1))
	      (furthest-away-accepted .1)
	      (filptr 0)
	      (filend 0)
	      (cur-oscils max-oscils)
	      (splice-attack (number? attack))
	      (ramped-attack (make-float-vector attack-size)))
	  (let ((obank (make-oscil-bank freqs (make-float-vector max-oscils) amps)))
	    
	    (set! filend (mus-length fil))
	    (float-vector-scale! window fftscale)
	    
	    (when splice-attack
	      (let ((cur-end (+ start attack-size)))
		;; my experience in translating SMS, and rumor via Greg Sandell leads me to believe that
		;; there is in fact no way to model some attacks successfully in this manner, so this block
		;; simply splices the original attack on to the rest of the note.  "attack" is the number
		;; of samples to include bodily.
		(do ((i start (+ i 1)))
		    ((= i cur-end))
		  (outa i (* amp (readin fil))))
		(set! filptr attack_size)
		(let ((mult (make-env '(0 1.0 1.0 0.0) :length attack-size)))
		  (do ((k 0 (+ k 1)))
		      ((= k attack-size))
		    (float-vector-set! ramped-attack k (* (env mult) (readin fil)))))
		(set! start cur-end)))
	    
	    (when (< start end)
	      (do ((i start (+ i outhop)))
		  ((>= i end))
		(when (<= filptr filend)
		  ;; get next block of data and apply window to it
		  (set! (mus-location fil) filptr)
		  (do ((k 0 (+ k 1)))
		      ((= k fftsize-1))
		    (float-vector-set! fdr k (readin fil)))
		  (float-vector-multiply! fdr window)
		  (set! filptr (+ filptr hop))
		  (fill! fdi 0.0)
		  ;; get the fft 
		  (mus-fft fdr fdi fftsize-1 1)
		  ;; change to polar coordinates (ignoring phases)
		  (rectangular->magnitudes fdr fdi)
		  (float-vector-scale! fdr 2.0)
		  
		  (float-vector-subseq current-peak-freqs 0 max-oscils last-peak-freqs)
		  (float-vector-subseq current-peak-amps 0 max-oscils last-peak-amps)
		  (fill! current-peak-amps 0.0)
		  (fill! peak-amps 0.0)
		  
		  (let ((peaks 0))
		    (let ((ra (fdr 0))
			  (la 0.0)
			  (ca 0.0))
		      ;; search for current peaks following Xavier Serra's recommendations in
		      ;; "A System for Sound Analysis/Transformation/Synthesis 
		      ;;      Based on a Deterministic Plus Stochastic Decomposition"
		      (do ((k 0 (+ k 1)))
			  ((= k highest-bin-1))
			(set! la ca)
			(set! ca ra)
			(set! ra (fdr k))
			(unless (or (<= ca 0.001)  ; lowest-magnitude
				    (<= ca ra) 
				    (<= ca la) 
				    (zero? ra) 
				    (zero? la))
			  ;; found a local maximum above the current threshold (its bin number is k-1)
			  (let ((logla (log la 10.0))
				(logca (log ca 10.0))
				(logra (log ra 10.0)))
			    (let ((offset (/ (* .5 (- logla logra)) (+ logla (* -2 logca) logra)))) ; isn't logca always 0?
			      (let ((amp (expt 10.0 (- logca (* .25 (- logla logra) offset))))
				    (freq (* fft-mag (+ k offset -1))))
				;; (if (not (real? amp)) (format *stderr* "~A ~A ~A -> ~A ~A~%" la ca ra offset amp))
				(if (= peaks max-peaks-1)
				    ;; gotta either flush this peak, or find current lowest and flush him
				    (let ((minp 0)
					  (minpeak (peak-amps 0)))
				      (do ((j 1 (+ j 1)))
					  ((= j max-peaks-1))
					(if (< (peak-amps j) minpeak)
					    (begin
					      (set! minp j)
					      (set! minpeak (peak-amps j)))))
				      (if (> amp minpeak)
					  (begin
					    (set! (peak-freqs minp) freq)
					    (set! (peak-amps minp) amp))))
				    (begin
				      (set! (peak-freqs peaks) freq)
				      (set! (peak-amps peaks) amp)
				      (set! peaks (+ peaks 1))))))))))
		    ;; now we have the current peaks -- match them to the previous set and do something interesting with the result
		    ;; the end results are reflected in the updated values in the rates and sweeps arrays.
		    ;; search for fits between last and current, set rates/sweeps for those found
		    ;;   try to go by largest amp first 
		    (do ((k 0 (+ k 1)))
			((= k peaks))
		      (let ((pl (float-vector-peak-and-location peak-amps)))
			(let ((maxpk (car pl))
			      (maxp (cadr pl)))
			  ;; now maxp points to next largest unmatched peak
			  (if (> maxpk 0.0)
			      (let ((closestp -1)
				    (closestamp 10.0)
				    (current-freq (peak-freqs maxp)))
				(let ((icf (/ 1.0 current-freq)))
				  (do ((j 0 (+ j 1)))
				      ((= j max-peaks-1))
				    (if (> (last-peak-amps j) 0.0)
					(let ((closeness (* icf (abs (- (last-peak-freqs j) current-freq)))))
					  (if (< closeness closestamp)
					      (begin
						(set! closestamp closeness)
						(set! closestp j))))))
				  (if (< closestamp furthest-away-accepted)
				      (begin
					;; peak-amp is transferred to appropriate current-amp and zeroed,
					(set! (current-peak-amps closestp) (peak-amps maxp))
					(set! (peak-amps maxp) 0.0)
					(set! (current-peak-freqs closestp) current-freq))))))))))
		  (do ((k 0 (+ k 1)))
		      ((= k max-peaks-1))
		    (if (> (peak-amps k) 0.0)
			;; find a place for a new oscil and start it up
			(let ((new-place -1))
			  (do ((j 0 (+ j 1)))
			      ((or (not (= new-place -1))
				   (= j max-oscils)))
			    (if (= (last-peak-amps j) 0.0 (current-peak-amps j))
				(set! new-place j)))
			  (set! (current-peak-amps new-place) (peak-amps k))
			  (set! (peak-amps k) 0.0)
			  (set! (current-peak-freqs new-place) (peak-freqs k))
			  (set! (last-peak-freqs new-place) (peak-freqs k))
			  (set! (freqs new-place) (hz->radians (* transposition (peak-freqs k)))))))
		  (set! cur-oscils 0)
		  (do ((k 0 (+ k 1)))
		      ((= k max-oscils))
		    (set! (rates k) (* amp ifreq (- (current-peak-amps k) (last-peak-amps k))))
		    (if (not (and (= (current-peak-amps k) 0.0)
				  (= (last-peak-amps k) 0.0)))
			(set! cur-oscils k))
		    (set! (sweeps k) (* ihifreq transposition (- (current-peak-freqs k) (last-peak-freqs k)))))
		  (set! cur-oscils (+ cur-oscils 1))
		  (set! (mus-length obank) cur-oscils)
		  
		  (let ((stop (min end (+ i outhop))))
		    (do ((k i (+ k 1)))
			((= k stop))
		      ;; run oscils, update envelopes
		      (outa k (oscil-bank obank))
		      (float-vector-add! amps rates)
		      (float-vector-add! freqs sweeps))))))))))))

;; (with-sound (:statistics #t) (pins 0 2 "oboe.snd" 1.0 :max-peaks 8))

