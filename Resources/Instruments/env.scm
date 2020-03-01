;;; Various envelope functions
;;;
;;; window-envelope (beg end env) -> portion of env lying between x axis values beg and end
;;; map-envelopes (func env1 env2) maps func over the breakpoints in env1 and env2 returning a new envelope
;;;   multiply-envelopes (env1 env2) multiplies break-points of env1 and env2 returning a new envelope
;;;   add-envelopes (env1 env2) adds break-points of env1 and env2 returning a new envelope
;;; max-envelope (env) -> max y value in env, min-envelope
;;; integrate-envelope (env) -> area under env
;;; envelope-last-x (env) -> max x axis break point position
;;; stretch-envelope env old-attack new-attack old-decay new-decay -> divseg-like envelope mangler
;;; scale-envelope (env scaler offset) scales y axis values by 'scaler' and optionally adds 'offset'
;;; reverse-envelope (env) reverses the breakpoints in 'env'
;;; concatenate-envelopes (:rest envs) concatenates its arguments into a new envelope
;;; repeat-envelope env repeats (reflected #f) (normalized #f) repeats an envelope
;;; power-env: generator for extended envelopes (each segment has its own base)
;;; envelope-exp: interpolate segments into envelope to give exponential curves
;;; rms-envelope
;;; normalize-envelope
;;; simplify-envelope

(provide 'snd-env.scm)


;;; -------- window-envelope (a kinda brute-force translation from the CL version in env.lisp)

(define window-envelope 
  (let ((documentation "(window-envelope beg end e) -> portion of e lying between x axis values beg and 
end: (window-envelope 1.0 3.0 '(0.0 0.0 5.0 1.0)) -> '(1.0 0.2 3.0 0.6)"))
    (lambda (beg end e)
      (let ((nenv ())
	    (lasty (if (pair? e) (cadr e) 0.0))
	    (len (length e)))
	(call-with-exit
	 (lambda (return-early)               
	   (do ((i 0 (+ i 2)))
	       ((>= i len))
	     (let ((x (e i))
		   (y (e (+ i 1))))
	       (set! lasty y)
	       (cond ((null? nenv)
		      (when (>= x beg)
			(set! nenv (append nenv (list beg (envelope-interp beg e))))
			(if (not (= x beg))
			    (if (>= x end)
				(return-early (append nenv (list end (envelope-interp end e))))
				(set! nenv (append nenv (list x y)))))))
		     ((<= x end)
		      (set! nenv (append nenv (list x y)))
		      (if (= x end) (return-early nenv)))
		     (else ;(> x end) 
		      (return-early
		       (append nenv (list end (envelope-interp end e))))))))
	   (append nenv (list end lasty))))))))


;;; -------- map-envelopes like map-across-envelopes in env.lisp

(define map-envelopes 
  (let ((documentation "(map-envelopes func env1 env2) maps func over the breakpoints in env1 and env2 returning a new envelope"))
    (lambda (op e1 e2)
      (let* ((xs ())
	     (at0 (lambda (e)
		    (let* ((diff (car e))
			   (len (length e))
			   (lastx (e (- len 2)))
			   (newe (copy e)))
		      (do ((i 0 (+ i 2)))
			  ((>= i len) newe)
			(let ((x (/ (- (newe i) diff) lastx)))
			  (set! xs (cons x xs))
			  (set! (newe i) x)))))))
	(if (null? e1)
	    (at0 e2)
	    (if (null? e2)
		(at0 e1)
		(let ((ee1 (at0 e1))
		      (ee2 (at0 e2))
		      (newe ()))
		  (set! xs (let ((lxs (let rem-dup ((lst xs)
						    (nlst ()))
					(cond ((null? lst) nlst)
					      ((member (car lst) nlst) (rem-dup (cdr lst) nlst))
					      (else (rem-dup (cdr lst) (cons (car lst) nlst)))))))
			     (sort! lxs <)))
		  (do ((len (length xs))
		       (i 0 (+ i 1)))
		      ((= i len) newe)
		    (let ((x (xs i)))
		      (set! newe (append newe (list x (op (envelope-interp x ee1) (envelope-interp x ee2))))))))))))))


;;; -------- multiply-envelopes, add-envelopes

(define multiply-envelopes 
  (let ((documentation "(multiply-envelopes env1 env2) multiplies break-points of env1 and env2 returning a new 
envelope: (multiply-envelopes '(0 0 2 .5) '(0 0 1 2 2 1)) -> '(0 0 0.5 0.5 1.0 0.5)"))
    (lambda (e1 e2)
      (map-envelopes * e1 e2))))

(define add-envelopes 
  (let ((documentation "(add-envelopes env1 env2) adds break-points of env1 and env2 returning a new envelope"))
    (lambda (e1 e2)
      (map-envelopes + e1 e2))))


;;; -------- max-envelope

(define max-envelope 
  (let ((documentation "(max-envelope env) -> max y value in env"))
    (lambda (env1)
      (let max-envelope-1 ((e (cddr env1))
                           (mx (cadr env1)))
        (if (null? e)
            mx
            (max-envelope-1 (cddr e) (max mx (cadr e))))))))

;;; -------- min-envelope

(define min-envelope 
  (let ((documentation "(min-envelope env) -> min y value in env"))
    (lambda (env1)
      (let min-envelope-1 ((e (cddr env1))
                           (mx (cadr env1)))
        (if (null? e)
            mx
            (min-envelope-1 (cddr e) (min mx (cadr e))))))))

;;; -------- integrate-envelope

(define integrate-envelope 
  (let ((documentation "(integrate-envelope env) -> area under env"))
    (lambda (env1)
      (let integrate-envelope-1 ((e env1)
                                 (sum 0.0000))
        (if (or (null? e) (null? (cddr e)))
            sum
            (integrate-envelope-1 (cddr e) (+ sum (* (+ (cadr e) (cadddr e)) 0.5 (- (caddr e) (car e))))))))))

;;; -------- envelope-last-x

(define envelope-last-x 
  (let ((documentation "(envelope-last-x env) -> max x axis break point position"))
    (lambda (e)
      (do ((e e (cddr e))) 
	  ((null? (cddr e))
	   (car e))))))

;;; -------- stretch-envelope

(define stretch-envelope 
  
  (let ((documentation "(stretch-envelope env old-attack new-attack old-decay new-decay) takes 'env' and 
returns a new envelope based on it but with the attack and optionally decay portions stretched 
or squeezed; 'old-attack' is the original x axis attack end point, 'new-attack' is where that 
section should end in the new envelope.  Similarly for 'old-decay' and 'new-decay'.  This mimics 
divseg in early versions of CLM and its antecedents in Sambox and Mus10 (linen).
  (stretch-envelope '(0 0 1 1) .1 .2) -> (0 0 0.2 0.1 1.0 1) 
  (stretch-envelope '(0 0 1 1 2 0) .1 .2 1.5 1.6) -> (0 0 0.2 0.1 1.1 1 1.6 0.5 2.0 0)"))
    (lambda* (fn old-att new-att old-dec new-dec)
      
      (cond ((not new-att)
	     (if old-att
		 (error 'wrong-number-of-args "stretch-envelope: ~A, old-attack but no new-attack?" old-att)
		 fn))
	    ((and old-dec (not new-dec))
	     (error 'wrong-number-of-args "stretch-envelope:~A ~A ~A, old-decay but no new-decay?" old-att new-att old-dec))
	    (else
	     (let ((x0 (car fn))
		   (y0 (cadr fn)))
	       (let ((new-x x0)
		     (last-x (fn (- (length fn) 2)))
		     (new-fn (list y0 x0))
		     (scl (/ (- new-att x0) (max .0001 (- old-att x0)))))
		 
		 (if (and (number? old-dec)
			  (= old-dec old-att))
		     (set! old-dec (* 1e-06 last-x)))
		 (reverse
		  (let stretch-envelope-1 ((new-fn new-fn)
					   (old-fn (cddr fn)))
		    (if (null? old-fn)
			new-fn
			(let ((x1 (car old-fn))
			      (y1 (cadr old-fn)))
			  (when (and (< x0 old-att) (>= x1 old-att))
			    (set! y0 (if (= x1 old-att)
					 y1
					 (+ y0 (* (- y1 y0) (/ (- old-att x0) (- x1 x0))))))
			    (set! x0 old-att)
			    (set! new-x new-att)
			    (set! new-fn (cons y0 (cons new-x new-fn)))
			    (set! scl (if old-dec
					  (/ (- new-dec new-att) (- old-dec old-att))
					  (/ (- last-x new-att) (- last-x old-att)))))
			  (when (and (real? old-dec)
				     (< x0 old-dec)
				     (>= x1 old-dec))
			    (set! y0 (if (= x1 old-dec)
					 y1
					 (+ y0 (* (- y1 y0) (/ (- old-dec x0) (- x1 x0))))))
			    (set! x0 old-dec)
			    (set! new-x new-dec)
			    (set! new-fn (cons y0 (cons new-x new-fn)))
			    (set! scl (/ (- last-x new-dec) (- last-x old-dec))))
			  (unless (= x0 x1)
			    (set! new-x (+ new-x (* scl (- x1 x0))))
			    (set! new-fn (cons y1 (cons new-x new-fn)))
			    (set! x0 x1)
			    (set! y0 y1))
			  (stretch-envelope-1 new-fn (cddr old-fn)))))))))))))
  

;;; -------- scale-envelope

(define scale-envelope 
  (let ((documentation "(scale-envelope env scaler (offset 0)) scales y axis values by 'scaler' and optionally adds 'offset'"))
    (lambda* (e scl (offset 0))
      (if (null? e)
	  ()
	  (cons (car e) (cons (+ offset (* scl (cadr e))) (scale-envelope (cddr e) scl offset)))))))


;;; -------- reverse-envelope

(define reverse-envelope 
  (let ((documentation "(reverse-envelope env) reverses the breakpoints in 'env'"))
    (lambda (e)
      (let ((len (length e)))
	(if (or (not (integer? len))
		(< len 3))
	    e
	    (let ((xd (e (- len 2))))
	      (let reverse-env-1 ((e e) (newe ()))
		(if (null? e)
		    newe
		    (reverse-env-1 (cddr e)
				   (cons (- xd (car e))
					 (cons (cadr e)
					       newe)))))))))))


;;; -------- concatenate-envelopes

(define concatenate-envelopes 
  (letrec ((documentation "(concatenate-envelopes :rest envs) concatenates its arguments into a new envelope")
	   (cat-1 (lambda (e newe xoff x0)
		    (if (null? e)
			newe
			(cat-1 (cddr e)
			       (cons (cadr e)
				     (cons (- (+ (car e) xoff) x0)
					   newe))
			       xoff
			       x0)))))
    (lambda envs
      (let ((ne ())
	    (xoff 0.0))
	(for-each 
	 (lambda (e)
	   (if (and (pair? ne)
		    (= (car ne) (cadr e)))
	       (begin
		 (set! xoff (- xoff .01))
		 (set! ne (cat-1 (cddr e) ne xoff (car e))))
	       (set! ne (cat-1 e ne xoff (car e))))
	   (set! xoff (+ xoff .01 (cadr ne))))
	 envs)
	(reverse ne)))))


(define repeat-envelope 
  (let ((documentation "(repeat-envelope env repeats (reflected #f) (normalized #f)) repeats 'env' 'repeats' 
times.  (repeat-envelope '(0 0 100 1) 2) -> (0 0 100 1 101 0 201 1). 
If the final y value is different from the first y value, a quick ramp is 
inserted between repeats. 'normalized' causes the new envelope's x axis 
to have the same extent as the original's. 'reflected' causes every other 
repetition to be in reverse."))
    (lambda* (ur-env repeats reflected normalized)
      (let ((e (if (not reflected)
		   ur-env
		   (let ((lastx (ur-env (- (length ur-env) 2)))
			 (rev-env (cddr (reverse ur-env)))
			 (new-env (reverse ur-env)))
		     (while (pair? rev-env)
			    (set! new-env (cons (- (+ lastx lastx) (cadr rev-env)) new-env))
			    (set! new-env (cons (car rev-env) new-env))
			    (set! rev-env (cddr rev-env)))
		     (reverse new-env)))))
	(let ((first-y (cadr e))
	      (x (car e)))
	  (let ((x-max (e (- (length e) 2)))
		(new-env (list first-y x)))
	    (let ((len (length e))
		  (times (if reflected (floor (/ repeats 2)) repeats))
		  (first-y-is-last-y (= first-y (e (- (length e) 1)))))
	      (do ((i 0 (+ i 1)))
		  ((= i times))
		(do ((j 2 (+ j 2)))
		    ((>= j len))
		  (set! x (- (+ x (e j)) (e (- j 2))))
		  (set! new-env (cons (e (+ j 1)) (cons x new-env))))
		(if (and (< i (- times 1)) (not first-y-is-last-y))
		    (begin
		      (set! x (+ x (/ x-max 100.0)))
		      (set! new-env (cons first-y (cons x new-env)))))))
	    (set! new-env (reverse new-env))
	    (if normalized
		(do ((scl (/ x-max x))
		     (new-len (length new-env))
		     (i 0 (+ i 2)))
		    ((>= i new-len))
		  (set! (new-env i) (* scl (new-env i)))))
	    new-env))))))


;;; -------- power-env 
;;;
;;; (this could also be done using multi-expt-env (based on env-any) in generators.scm)

(if (provided? 'snd)
    (require snd-ws.scm)
    (require sndlib-ws.scm))

;;; (define pe (make-power-env '(0 0 1 1 2 0) :duration 1.0))
;;; :(power-env pe)
;;; 0.0
;;; :(power-env pe)
;;; 4.5352502324316e-05
;;; :(power-env pe)
;;; 9.0705004648631e-05
;;; :(power-env pe)
;;; 0.00013605750697295


(defgenerator penv (envs #f) (total-envs 0) (current-env 0) (current-pass 0))

(define (power-env pe)
  (with-let pe
    (let ((val (env (vector-ref envs current-env))))
      (set! current-pass (- current-pass 1))
      (when (and (= current-pass 0)
		 (< current-env (- total-envs 1)))
	(set! current-env (+ current-env 1))
	(set! current-pass (- (length (vector-ref envs current-env)) 1)))
      val)))

(define* (make-power-env envelope (scaler 1.0) (offset 0.0) duration)
  (let* ((len (- (floor (/ (length envelope) 3)) 1))
	 (pe (make-penv :envs (make-vector len)
			:total-envs len
			:current-env 0
			:current-pass 0))
	 (xext (- (envelope (- (length envelope) 3)) (car envelope))))
    (do ((i 0 (+ i 1))
	 (j 0 (+ j 3)))
	((= i len))
      (let ((x0 (envelope j))
	    (x1 (envelope (+ j 3)))
	    (y0 (envelope (+ j 1)))
	    (y1 (envelope (+ j 4)))
	    (base (envelope (+ j 2))))
	(vector-set! (pe 'envs) i (make-env (list 0.0 y0 1.0 y1) 
					    :base base :scaler scaler :offset offset 
					    :duration (* duration (/ (- x1 x0) xext))))))
    (set! (pe 'current-pass) (- (length (vector-ref (pe 'envs) 0)) 1))
    pe))

(define* (power-env-channel pe (beg 0) snd chn edpos (edname "power-env-channel"))
  ;; split into successive calls on env-channel
  (let ((curbeg beg)) ; sample number
    (as-one-edit
     (lambda ()
       (do ((i 0 (+ i 1)))
	   ((= i (pe 'total-envs)))
	 (let* ((e (vector-ref (pe 'envs) i))
		(len (length e)))
	   (env-channel e curbeg len snd chn edpos)
	   (set! curbeg (+ curbeg len)))))
     edname)))


;;; here's a simpler version that takes the breakpoint list, rather than the power-env structure:

(define powenv-channel 
  (let ((documentation "(powenv-channel envelope (beg 0) dur snd chn edpos) returns an envelope with a separate base for \
each segment: (powenv-channel '(0 0 .325  1 1 32.0 2 0 32.0))"))
    (lambda* (envelope (beg 0) dur snd chn edpos)
      (let ((len (length envelope))
	    (x1 (car envelope)))
	(let ((curbeg beg)
	      (fulldur (or dur (framples snd chn edpos)))
	      (xrange (- (envelope (- len 3)) x1))
	      (y1 (cadr envelope))
	      (base (caddr envelope))
	      (x0 0.0)
	      (y0 0.0))
	  (if (= len 3)
	      (scale-channel y1 beg dur snd chn edpos)
	      (as-one-edit
	       (lambda ()
		 (do ((i 3 (+ i 3)))
		     ((= i len))
		   (set! x0 x1)
		   (set! y0 y1)
		   (set! x1 (envelope i))
		   (set! y1 (envelope (+ i 1)))
		   (let ((curdur (round (* fulldur (/ (- x1 x0) xrange)))))
		     (xramp-channel y0 y1 base curbeg curdur snd chn edpos)
		     (set! curbeg (+ curbeg curdur)))
		   (set! base (envelope (+ i 2))))))))))))
  

;;; by Anders Vinjar:
;;;
;;; envelope-exp can be used to create exponential segments to include in
;;; envelopes.  Given 2 or more breakpoints, it approximates the
;;; curve between them using 'xgrid linesegments and 'power as the
;;; exponent. 
;;; 
;;; env is a list of x-y-breakpoint-pairs,
;;; power applies to whole envelope,
;;; xgrid is how fine a solution to sample our new envelope with.

(define envelope-exp 
  (let ((documentation "(envelope-exp e (power 1.0) (xgrid 100)) approximates an exponential curve connecting the breakpoints"))
    (lambda* (e (power 1.0) (xgrid 100))
      (let ((mn (min-envelope e)))
	(let ((largest-diff (* 1.0 (- (max-envelope e) mn)))
	      (x-min (car e))
	      (x-max (e (- (length e) 2))))
	  (do ((x-incr (* 1.0 (/ (- x-max x-min) xgrid)))
	       (new-e ())
	       (x x-min (+ x x-incr)))
	      ((>= x x-max)
	       (reverse new-e))     
	    (let ((y (envelope-interp x e)))
	      (set! new-e (cons (if (= largest-diff 0.0)
				    y
				    (+ mn
				       (* largest-diff
					  (expt (/ (- y mn) largest-diff) power))))
				(cons x new-e))))))))))

;;; rms-envelope

(define rms-envelope 
  (let ((documentation "(rms-envelope file (beg 0.0) (dur #f) (rfreq 30.0) (db #f)) returns an envelope of RMS values in 'file'"))
    (lambda* (file (beg 0.0) dur (rfreq 30.0) db)
      ;; based on rmsenv.ins by Bret Battey
      (let* ((fsr (srate file))
	     (start (round (* beg fsr)))
	     (end (if dur (min (* 1.0 (+ start (round (* fsr dur))))
			       (mus-sound-framples file))
		      (mus-sound-framples file))))
	(let ((incrsamps (round (/ fsr rfreq)))
	      (len (- (+ end 1) start)))
	  (let ((reader (make-sampler start file))
		(rms (make-moving-average incrsamps)) ; this could use make-moving-rms from dsp.scm
		(e ())
		(rms-val 0.0)
		(jend 0)
		(data (make-float-vector len)))
	    (do ((i 0 (+ i 1)))
		((= i len))
	      (float-vector-set! data i (next-sample reader)))
	    (float-vector-multiply! data data)
	    (do ((i 0 (+ i incrsamps)))
		((>= i end) 
		 (reverse e))
	      (set! jend (min end (+ i incrsamps)))
	      (do ((j i (+ j 1)))
		  ((= j jend))
		(moving-average rms (float-vector-ref data j)))
	      (set! e (cons (* 1.0 (/ i fsr)) e))
	      (set! rms-val (sqrt (* (mus-scaler rms) (mus-increment rms))))
	      (set! e (cons (if db 
				(if (< rms-val 1e-05) -100.0 (* 20.0 (log rms-val 10.0)))
				rms-val)
			    e)))))))))
  
  
(define* (normalize-envelope env1 (new-max 1.0))
  (scale-envelope env1
		  (/ new-max
		     (let abs-max-envelope-1 ((e (cddr env1))
					      (mx (abs (cadr env1))))
		       (if (null? e)
			   mx
			   (abs-max-envelope-1 (cddr e) (max mx (abs (cadr e)))))))))


;;; simplify-envelope
;;;
;;; this is not very good...

(define simplify-envelope 
  
  ;; grid = how fine a fluctuation we will allow.
  ;; the smaller the grid, the less likely a given bump will get through
  ;; original x and y values are not changed, just sometimes omitted.
  
  (let ((point-on-line 
	 (lambda (px py qx qy tx ty)
	   
	   ;; is point tx ty on line defined by px py and qx qy --
	   ;; #f if no, :before if on ray from p, :after if on ray from q, :within if between p and q
	   ;; (these are looking at the "line" as a fat vector drawn on a grid)
	   ;; taken from "Graphics Gems" by Glassner, code by A Paeth
	   
	   (if (or (= py qy ty) 
		   (= px qx tx))
	       :within
	       (and (< (abs (- (* (- qy py) (- tx px))
			       (* (- ty py) (- qx px))))
		       (max (abs (- qx px))
			    (abs (- qy py))))
		    (if (or (< qx px tx) (< qy py ty) (< tx px qx) (< ty py qy)) 
			:before
			(if (or (< px qx tx) (< py qy ty) (< tx qx px) (< ty qy py)) 
			    :after
			    :within)))))))
	 
    (lambda* (env1 (ygrid 10) (xgrid 100))
      (if (not (and env1
		    (> (length env1) 4)))
	  env1
	  (let ((new-env (list (cadr env1) (car env1)))
		(ymax (max-envelope env1))
		(ymin (min-envelope env1))
		(xmax (env1 (- (length env1) 2)))
		(xmin (car env1)))
	    (if (= ymin ymax)
		(list xmin ymin xmax ymax)
		(do ((y-scl (/ ygrid (- ymax ymin)))
		     (x-scl (/ (or xgrid ygrid) (- xmax xmin)))
		     (px #f) (py #f)
		     (qx #f) (qy #f) 
		     (tx #f) (ty #f) 
		     (qtx #f) (qty #f)
		     (i 0 (+ i 2)))
		    ((>= i (length env1))
		     (set! new-env (cons qty (cons qtx new-env)))
		     (reverse new-env))
		  (let ((ttx (env1 i))
			(tty (env1 (+ i 1))))
		    (set! tx (round (* ttx x-scl)))
		    (set! ty (round (* tty y-scl)))
		    (if px
			(if (not (point-on-line px py qx qy tx ty))
			    (begin
			      (set! new-env (cons qty (cons qtx new-env)))
			      (set! px qx)
			      (set! py qy)))
			(begin
			  (set! px qx)
			  (set! py qy)))
		    (set! qx tx)
		    (set! qy ty)
		    (set! qtx ttx)
		    (set! qty tty)))))))))
    