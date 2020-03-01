;;; multi-channel sound file expansion with srate and reverb.
;;; michael klingbeil (michael@klingbeil.com)
;;;
;;; $Name:  $
;;; $Revision: 1.1 $
;;; $Date: 2005/10/16 22:15:44 $
;;;
;;; clm-4 and scheme May-08 bil
;;; split out cases to optimize May-09 bil

(provide 'snd-expandn.scm)

(if (provided? 'snd)
    (require snd-ws.scm)
    (require sndlib-ws.scm))
(require snd-env.scm)


(definstrument (expandn time duration filename amplitude
			(expand 1.0)
			matrix
			(ramp 0.4)
			(seglen 0.15)
			(srate 1.0)
			(hop .05)
			(amp-env '(0 0 50 1 100 0))
			(input-start 0.0)
			(grain-amp 0.8)
			reverb)

  (let ((fnam (file-name filename)))
    (if (not (file-exists? fnam))
	(error 'no-such-file (list 'expandn filename))

	(let* ((beg (seconds->samples time))
	       (end (+ beg (seconds->samples duration)))
	       (min-exp-amt (if (pair? expand) (min-envelope expand) expand))
	       (max-out-hop (if (pair? hop) (max-envelope hop) hop)))
	  
	  (let ((in-chans (channels fnam))
		(out-chans (channels *output*))
		(rev-chans (if *reverb* (channels *reverb*) 0)))
	    
	    (let ((update-rate 100)
		  (ochans (max in-chans out-chans))
		  (max-seg-len (if (pair? seglen) (max-envelope seglen) seglen))
		  (rampdata (if (pair? ramp) ramp (list 0 ramp 1 ramp)))
		  (start (floor (* input-start (mus-sound-srate fnam))))
		  (max-in-hop (/ max-out-hop min-exp-amt))
		  (rev-mx (and *reverb* (real? reverb) (> reverb 0.0)
			       (let* ((rchans (max out-chans rev-chans))
				      (rmx (make-float-vector (list rchans rchans))))
				 (do ((i 0 (+ i 1)))
				     ((= i rchans))
				   (set! (rmx i i) reverb))
				 rmx))))
	      
	      (let ((mx (let ((v (make-float-vector (list ochans ochans))))
			  (if (pair? matrix)
			      (let ((mat-in (min ochans (length matrix)))
				    (mat-out (min ochans (length (car matrix)))))
				(do ((inp 0 (+ inp 1)))
				    ((= inp mat-in))
				  (do ((outp 0 (+ outp 1)))
				      ((= outp mat-out))
				    (set! (v inp outp) (matrix inp outp)))))
			      (do ((i 0 (+ i 1)))
				  ((= i ochans))
				(set! (v i i) 1.0)))
			  v))
		    
		    (revvals (and rev-mx (make-float-vector (max out-chans rev-chans))))
		    (update-envs (or (pair? expand)
				     (pair? seglen)
				     (pair? ramp)
				     (pair? hop)))
		    (update-ctr 0)
		    (expenv (make-env (if (pair? expand) expand (list 0 expand 1 expand))
				      :duration (/ duration update-rate)))
		    (lenenv (make-env (if (pair? seglen) seglen (list 0 seglen 1 seglen))
				      :duration (/ duration update-rate)))
		    (segment-scaler (if (> max-seg-len .15)
					(/ (* grain-amp .15) max-seg-len)
					grain-amp))
		    (srenv (make-env (if (pair? srate) srate (list 0 srate)) :duration duration))
		    (rampenv (make-env rampdata :duration (/ duration update-rate)))
		    (minramp-bug (<= (min-envelope rampdata) 0.0))
		    (maxramp-bug (>= (max-envelope rampdata) 0.5))
		    (hopenv (make-env (if (pair? hop) hop (list 0 hop 1 hop))
				      :duration (/ duration update-rate)))
		    (ampenv (make-env amp-env :duration duration :scaler amplitude))
		    (ex-array (make-vector in-chans #f))
		    (ex-samp -1.0)
		    (next-samp 0.0)
		    
		    (max-len (ceiling (* *clm-srate*
					 (+ (max max-out-hop max-in-hop)
					    max-seg-len))))
		    (invals (make-float-vector ochans))
		    (outvals (make-float-vector ochans)))
		
		(if (or minramp-bug maxramp-bug)
		    (error 'out-of-range (list expand 
					       "ramp argument to expandn must always be "
					       (if (and minramp-bug maxramp-bug) "between 0.0 and 0.5"
						   (if minramp-bug "greater than 0.0"
						       "less than 0.5")))))
		
		;; setup granulate generators
		(do ((i 0 (+ i 1)))
		    ((= i in-chans))
		  (vector-set! ex-array i (make-granulate :input (make-readin fnam :start start :channel i)
							  :expansion (if (pair? expand) (cadr expand) expand)
							  :max-size max-len
							  :ramp (if (pair? ramp) (cadr ramp) ramp)
							  :hop (if (pair? hop) (cadr hop) hop)
							  :length (if (pair? seglen) (cadr seglen) seglen)
							  :scaler segment-scaler)))
		;; split out 1 and 2 chan input 
		(case in-chans
		  ((1)
		   (let ((ingen (vector-ref ex-array 0))
			 (sample-0 0.0)
			 (sample-1 0.0))
		     ;; these vars used for resampling
		     
		     (if (not (or (pair? srate) 
				  update-envs 
				  (not (= out-chans 1)) 
				  matrix 
				  rev-mx))
			 
			 (let ((file-end (+ beg (seconds->samples (+ (* 2 seglen) 
								     (/ (* (mus-sound-duration fnam) (mus-sound-srate fnam) expand) 
									*clm-srate* srate))))))
			   (set! end (min end file-end))
			   
			   (do ((i beg (+ i 1)))
			       ((= i end))
			     
			     (let ((vol (env ampenv)))
			       (if (negative? ex-samp)
				   (begin
				     (set! sample-0 (* vol (granulate ingen)))
				     (set! sample-1 (* vol (granulate ingen)))
				     (set! ex-samp (+ ex-samp 1))
				     (set! next-samp ex-samp)
				     (outa i sample-0))
				   (begin
				     (set! next-samp (+ next-samp srate))
				     (if (> next-samp (+ ex-samp 1))
					 (let ((samps (floor (- next-samp ex-samp))))
					   (if (= samps 2)
					       (begin
						 (set! sample-0 (* vol (granulate ingen)))
						 (set! sample-1 (* vol (granulate ingen))))
					       (do ((k 0 (+ k 1)))
						   ((= k samps))
						 (set! sample-0 sample-1)
						 (set! sample-1 (* vol (granulate ingen)))))
					   (set! ex-samp (+ ex-samp samps))))
				     ;(if (= next-samp ex-samp)  ; something is wrong!  which of these if's is correct?
				     (outa i (if (= next-samp ex-samp)
						 sample-0
						 (+ sample-0 (* (- next-samp ex-samp) (- sample-1 sample-0))))))))))
			 
			 (do ((i beg (+ i 1)))
			     ((= i end))
			   
			   (let ((vol (env ampenv))
				 (resa (env srenv)))
			     
			     (if update-envs
				 (begin
				   (set! update-ctr (+ update-ctr 1))
				   (if (>= update-ctr update-rate)
				       (let ((sl (floor (* (env lenenv) *clm-srate*))))
					 (set! update-ctr 0)
					 (set! (mus-length ingen) sl)
					 (set! (mus-ramp ingen) (floor (* sl (env rampenv))))
					 (set! (mus-frequency ingen) (env hopenv))
					 (set! (mus-increment ingen) (env expenv))))))
			     
			     (if (negative? ex-samp)
				 (begin
				   (set! sample-0 (* vol (granulate ingen)))
				   (set! sample-1 (* vol (granulate ingen)))
				   (set! ex-samp (+ ex-samp 1))
				   (set! next-samp ex-samp))
				 (begin
				   (set! next-samp (+ next-samp resa))
				   (if (> next-samp (+ ex-samp 1))
				       (let ((samps (floor (- next-samp ex-samp))))
					 (if (= samps 2)
					     (begin
					       (set! sample-0 (* vol (granulate ingen)))
					       (set! sample-1 (* vol (granulate ingen))))
					     (do ((k 0 (+ k 1)))
						 ((= k samps))
					       (set! sample-0 sample-1)
					       (set! sample-1 (* vol (granulate ingen)))))
					 (set! ex-samp (+ ex-samp samps)))))))
			   (set! (invals 0) (if (= next-samp ex-samp)
						sample-0                      ; output actual samples
						(+ sample-0 (* (- next-samp ex-samp) (- sample-1 sample-0))))) ; output interpolated samples
			   
			   ;; output mixed result
			   (frample->file *output* i (frample->frample mx invals ochans outvals ochans))
			   ;; if reverb is turned on, output to the reverb streams
			   (if rev-mx
			       (frample->file *reverb* i (frample->frample rev-mx outvals ochans revvals rev-chans)))))))
		  
		  ((2) ; 2 chans
		   (let ((sample-0-0 0.0)
			 (sample-1-0 0.0)
			 (sample-0-1 0.0)
			 (sample-1-1 0.0)
			 (ingen0 (vector-ref ex-array 0))
			 (ingen1 (vector-ref ex-array 1)))
		     (do ((i beg (+ i 1)))
			 ((= i end))
		       
		       (let ((vol (env ampenv))
			     (resa (env srenv)))
			 
			 (if update-envs
			     (begin
			       (set! update-ctr (+ update-ctr 1))
			       (if (>= update-ctr update-rate)
				   (let ((expa (env expenv))                ;current expansion amount
					 (segl (env lenenv))                ;current segment length
					 (rmpl (env rampenv))               ;current ramp length (0 to .5)
					 (hp (env hopenv)))                 ;current hop size
				     (let* ((sl (floor (* segl *clm-srate*)))
					    (rl (floor (* rmpl sl))))
				       (set! update-ctr 0)
				       (set! (mus-length ingen0) sl)
				       (set! (mus-ramp ingen0) rl)
				       (set! (mus-frequency ingen0) hp)
				       (set! (mus-increment ingen0) expa)
				       (set! (mus-length ingen1) sl)
				       (set! (mus-ramp ingen1) rl)
				       (set! (mus-frequency ingen1) hp)
				       (set! (mus-increment ingen1) expa))))))
			 
			 (if (negative? ex-samp)
			     (begin
			       (set! sample-0-0 (* vol (granulate ingen0)))
			       (set! sample-1-0 (* vol (granulate ingen0)))
			       (set! sample-0-1 (* vol (granulate ingen1)))
			       (set! sample-1-1 (* vol (granulate ingen1)))
			       (set! ex-samp (+ ex-samp 1))
			       (set! next-samp ex-samp))
			     (begin
			       (set! next-samp (+ next-samp resa))
			       (if (> next-samp (+ ex-samp 1))
				   (let ((samps (floor (- next-samp ex-samp))))
				     (do ((k 0 (+ k 1)))
					 ((= k samps))
				       (set! sample-0-0 sample-1-0)
				       (set! sample-1-0 (* vol (granulate ingen0)))
				       (set! sample-0-1 sample-1-1)
				       (set! sample-1-1 (* vol (granulate ingen1))))
				     (set! ex-samp (+ ex-samp samps)))))))
		       
		       (if (= next-samp ex-samp)
			   ;; output actual samples
			   (begin
			     (set! (invals 0) sample-0-0)
			     (set! (invals 1) sample-0-1))
			   (begin
			     ;; output interpolated samples
			     (set! (invals 0) (+ sample-0-0 (* (- next-samp ex-samp) (- sample-1-0 sample-0-0))))
			     (set! (invals 1) (+ sample-0-1 (* (- next-samp ex-samp) (- sample-1-1 sample-0-1))))))
		       
		       ;; output mixed result
		       (frample->file *output* i (frample->frample mx invals ochans outvals ochans))
		       ;; if reverb is turned on, output to the reverb streams
		       (if rev-mx
			   (frample->file *reverb* i (frample->frample rev-mx outvals ochans revvals rev-chans))))))
		  
		  (else 
		   (let ((samples-0 (make-float-vector in-chans))
			 (samples-1 (make-float-vector in-chans)))
		     ;; more than 2 chans in input file
		     (do ((i beg (+ i 1)))
			 ((= i end))
		       (let ((vol (env ampenv))
			     (resa (env srenv)))
			 
			 (if update-envs
			     (begin
			       (set! update-ctr (+ update-ctr 1))
			       (if (>= update-ctr update-rate)
				   (let ((expa (env expenv))                ;current expansion amount
					 (segl (env lenenv))                ;current segment length
					 (rmpl (env rampenv))               ;current ramp length (0 to .5)
					 (hp (env hopenv)))                 ;current hop size
				     (let* ((sl (floor (* segl *clm-srate*)))
					    (rl (floor (* rmpl sl))))
				       (set! update-ctr 0)
				       (do ((ix 0 (+ ix 1)))
					   ((= ix in-chans))
					 (let ((gen (vector-ref ex-array ix)))
					   (set! (mus-length gen) sl)
					   (set! (mus-ramp gen) rl)
					   (set! (mus-frequency gen) hp)
					   (set! (mus-increment gen) expa))))))))
			 
			 (if (negative? ex-samp)
			     (begin
			       (do ((ix 0 (+ ix 1)))
				   ((= ix in-chans))
				 (let ((gen (vector-ref ex-array ix)))
				   (float-vector-set! samples-0 ix (* vol (granulate gen)))
				   (float-vector-set! samples-1 ix (* vol (granulate gen)))))
			       (set! ex-samp (+ ex-samp 1))
			       (set! next-samp ex-samp))
			     (begin
			       (set! next-samp (+ next-samp resa))
			       (if (> next-samp (+ ex-samp 1))
				   (let ((samps (floor (- next-samp ex-samp))))
				     (do ((k 0 (+ k 1)))
					 ((= k samps))
				       (do ((ix 0 (+ ix 1)))
					   ((= ix in-chans))
					 (let ((gen (vector-ref ex-array ix)))
					   (float-vector-set! samples-0 ix (float-vector-ref samples-1 ix))
					   (float-vector-set! samples-1 ix (* vol (granulate gen))))))
				     (set! ex-samp (+ ex-samp samps)))))))
		       
		       (if (= next-samp ex-samp)
			   ;; output actual samples
			   (copy samples-0 invals 0 in-chans)
			   ;; output interpolated samples
			   (do ((ix 0 (+ ix 1)))
			       ((= ix in-chans))
			     (let ((v0 (float-vector-ref samples-0 ix))
				   (v1 (float-vector-ref samples-1 ix)))
			       (float-vector-set! invals ix (+ v0 (* (- next-samp ex-samp) (- v1 v0)))))))
		       ;; output mixed result
		       (frample->file *output* i (frample->frample mx invals ochans outvals ochans))
		       ;; if reverb is turned on, output to the reverb streams
		       (if rev-mx
			   (frample->file *reverb* i (frample->frample rev-mx outvals ochans revvals rev-chans))))))))))))))

;;; (with-sound () (expandn 0 1 "oboe.snd" 1 :expand 4))
