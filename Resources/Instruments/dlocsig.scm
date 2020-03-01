;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Suggestions, comments and bug reports are welcome. Please 
;;; address email to: nando@ccrma.stanford.edu
;;;
;;; see the file COPYING for license info.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dynamic multichannel three-dimentional signal locator
;;; (wow that sound good! :-)
;;;
;;; by Fernando Lopez Lezcano
;;;    CCRMA, Stanford University
;;;    nando@ccrma.stanford.edu
;;;
;;; Thanks to Juan Pampin for help in the initial coding of the new version
;;; and for prodding me to finish it. To Joseph L. Anderson and Marcelo Perticone
;;; for insights into the Ambisonics coding and decoding process. 
;;; http://www.york.ac.uk/inst/mustech/3d_audio/ambison.htm for more details...

;;; CHANGES:
;;; 01/28/2012: third order ambisonics support (Nando).
;;; 04/18/2011: various small changes from lint.scm.
;;; 04/26/2010: add delay hack to remove artifacts in delay output, fix other bugs (Nando)
;;;             added proper doppler src conversion thanks to Bill's code in dsp.scm
;;;             merged in code for higher order ambisonics (up to 2nd order h/v)
;;; 06/28/2009: remove class/method stuff for s7 (Bill)
;;; 01/08/2007: make a few functions local etc (Bill)
;;; 07/05/2006: translate to scheme, use move-sound generator (Bill)
;;; 04/29/2002: fixed reverb envelopes for no reverb under clisp
;;; 01/14/2001: added multichannel reverb output with local and global control
;;;             in the reverberator (the hrtf code is currently not merged into
;;;             this version). Reverb-amount can now be an envelope. Ambisonics
;;;             now outputs signal to the reverb stream. 
;;; 02/05/2000: . don't compile as part of the clm package, import symbols
;;;             . switched over to clm-2, otherwise convolve HAS to operate
;;;               on a file, we want convolve to process the output of the 
;;;               doppler delay line (for the hrtf code)
;;; 02/03/2000: started working on hrtf's
;;; 01/15/2000: rewrote transform-path code to account for 3d paths
;;; 01/13/2000: . changed order of b-format output file to W:X:Y:Z from X:Y:Z:W
;;;             . plot method would bomb with paths that had constant velocity,
;;;               fixed norm function
;;;             . added make-literal-path and friends to enable to create
;;;               paths with user supplied coordinates
;;;             . decoded-ambisonics was rotating sounds in the wrong direction
;;;             . in change-direction: only check for change if both points are
;;;               different (intersect-inside-radius can create a redundant point)
;;; 11/28/1999: decoded-ambisonics is now working for N channels
;;;             includes reverberation send
;;; 11/27/1999: set minimum segment distance for rendering, otherwise for long
;;;             lines the amplitude envelope does not reflect power curve. 
;;; 11/26/1999: added code to check for intersection with inner radius
;;;             fixed nearest-point to handle trivial cases

;;; 01/21/2001: fix envelope generated for mono reverberation stream.
;;;             change input and output to use frames and mixers
;;;             fix supersonic movement warning code
;;; > add warnings when object goes outside of area covered by speakers
;;; > fix one common vertice case of 3 speaker group transitions
;;; > redo the old code for multiple images (reflections in a rectangular room)
;;;     a bit of a pain, would have to add z (ceiling and floor reflections)
;;;     would be better to find general purpose code for non-rectangular rooms
;;; > we really need a good N-channel reverb [fixed with freeverb]
;;; > change reverb to be multichannel, add local and global reverb
;;;   11/24/1999: should use a waveguide reverb like pph@ccrma project
;;;               would be a good idea to inject the signal through a center
;;;               injection point that moves inside the virtual cube, more like
;;;               a physical model of what actually happens in a room
;;; | add ambisonics back-end
;;;   11/24/1999: code to b-format sort of working
;;;                 how to deal with the inner space and 0:0:0?
;;;               decoded format not working if we incorporate distance att
;;;                 formulas are wrong...
;;; > add hrtf back-end
;;; > extract a supath from an existing path
;;; > recode the transformation functions
;;; > add arcs of circles and other basic geometric paths
;;;     make it so that you can concatenate them...
;;; | 11/25/1999 fix the "diagonal case" (sounds go through the head of the listener)

(provide 'snd-dlocsig.scm)

#|
(define* (envelope-interp x e base)   ;e is list of x y breakpoint pairs, interpolate at x returning y
 ;; "(envelope-interp x e (base 1.0)) -> value of e at x; base controls connecting segment type: (envelope-interp .3 '(0 0 .5 1 1 0) -> .6"
  (cond ((null? e) 0.0)		        ;no data -- return 0.0
	((or (<= x (car e))	        ;we're sitting on x val (or if < we blew it)
	     (null? (cddr e)))	        ;or we're at the end of the list
	 (cadr e))		        ;so return current y value
	((> (caddr e) x)		;x <= next env x axis value
	 (if (or (= (cadr e) (cadddr e))
		 (and base (= base 0.0)))
	     (cadr e)		        ;y1=y0, so just return y0 (avoid endless calculations below)
	     (if (or (not base) (= base 1.0))
		 (+ (cadr e)	        ;y0+(x-x0)*(y1-y0)/(x1-x0)
		    (* (- x (car e))
		       (/ (- (cadddr e) (cadr e))
			  (- (caddr e) (car e)))))
		 (+ (cadr e) ; this does not exactly match xramp-channel
		    (* (/ (- (cadddr e) (cadr e))
			  (- base 1.0))
		       (- (expt base (/ (- x (car e))
					(- (caddr e) (car e))))
			  1.0))))))
	(else (envelope-interp x (cddr e) base)))) ;go on looking for x segment
|#

(define x-norm 
  (let ((documentation "(x-norm e xmax) changes 'e' x axis values so that they run to 'xmax'"))
    (lambda (e xmax)
      (let x-norm-1 ((e e)
                     (scl (/ xmax (e (- (length e) 2))))
                     (new-e ()))
        (if (null? e)
            (reverse! new-e)
            (x-norm-1 (cddr e) scl (cons (cadr e) (cons (* scl (car e)) new-e))))))))


;;;;;;;;;;;;;;;;;;;;;
;;; Global Parameters

;;; Define the base in which all angles are expressed
(define dlocsig-one-turn 360)

(define one-turn-is 
  (let ((documentation "(one-turn-is unit) sets dlocsig's angle unit (degrees=360, the default or radians=2*pi)"))
    (lambda (unit)
      (set! dlocsig-one-turn unit))))

(define angles-in-degree
  (let ((documentation "(angles-in-degree) sets dlocsig's unit to degrees (the default)"))
    (lambda ()
      (one-turn-is 360))))

(define angles-in-radians
  (let ((documentation "(angles-in-radians) sets dlocsig's unit to radians (default is degrees)"))
    (lambda ()
      (one-turn-is (* 2 pi)))))

(define angles-in-turns
  (let ((documentation "(angles-in-turns) sets dlocsig's angle unit to turns"))
    (lambda ()
      (one-turn-is 1))))

;; speed of sound in air, in meters per second under normal conditions
(define dlocsig-speed-of-sound 344)

(define distances-in-meters
  (let ((documentation "(distances-in-meters) sets dlocsig's distances in meters (the default)"))
    (lambda ()
      (set! dlocsig-speed-of-sound 344))))

(define distances-in-feet
  (let ((documentation "(distances-in-feet) sets dlocsig's distances in feet (default is meters)"))
    (lambda ()
      (set! dlocsig-speed-of-sound 1128))))

;; default for whether to use two or three-dimensional speaker configurations
(define dlocsig-3d #f)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Speaker Configuration

(define* (make-group (id 0) (size 0) vertices speakers matrix)
  (list 'group id size vertices speakers matrix))

(define group-id (dilambda (lambda (a) (a 1)) (lambda (a b) (set! (a 1) b))))
(define group-size (dilambda (lambda (a) (a 2)) (lambda (a b) (set! (a 2) b))))
(define group-vertices (dilambda (lambda (a) (a 3)) (lambda (a b) (set! (a 3) b))))
(define group-speakers (dilambda (lambda (a) (a 4)) (lambda (a b) (set! (a 4) b))))
(define group-matrix (dilambda (lambda (a) (a 5)) (lambda (a b) (set! (a 5) b))))


(define* (make-speaker-config number dimension coords groups delays omap)
  (list 'speaker-config number dimension coords groups delays omap))

(define speaker-config-number (dilambda (lambda (a) (a 1)) (lambda (a b) (set! (a 1) b))))
(define speaker-config-dimension (dilambda (lambda (a) (a 2)) (lambda (a b) (set! (a 2) b))))
(define speaker-config-coords (dilambda (lambda (a) (a 3)) (lambda (a b) (set! (a 3) b))))
(define speaker-config-groups (dilambda (lambda (a) (a 4)) (lambda (a b) (set! (a 4) b))))
(define speaker-config-delays (dilambda (lambda (a) (a 5)) (lambda (a b) (set! (a 5) b))))
(define speaker-config-map (dilambda (lambda (a) (a 6)) (lambda (a b) (set! (a 6) b))))


;;; Create a speaker configuration structure based on a list of speakers
;;;
;;; speakers:  list of angles of speakers with respect to 0
;;; delays:    list of delays for each speaker, zero if nil
;;; distances: list relative speaker distances, 
;;;            (instead of delays)
;;; omap:      mapping of speakers to output channels
;;;            content should be output channel number, zero based

(define cis 
  (let ((documentation "(cis a) returns e^(ia)"))
    (lambda (a)
      (exp (* 0.0+1.0i a)))))

(define third 
  (let ((documentation "(third lst) returns the 3rd element of 'lst'"))
    (lambda (a) 
      (and (>= (length a) 3) (a 2)))))

(define fourth 
  (let ((documentation "(fourth lst) returns the 4th element of 'lst'"))
    (lambda (a) 
      (and (>= (length a) 4) (a 3)))))

(define* (last lst (n 1))
  (and (pair? lst) 
       (list-tail lst (- (length lst) n))))

(define arrange-speakers
  ;; sanity checking of configuration
  (letrec ((has-duplicates? 
	    (lambda (lst)
	      (and (not (null? lst))
		   (or (member (car lst) (cdr lst))
		       (has-duplicates? (cdr lst))))))
	   (invert3x3 
	    (lambda (mat) ; invert a 3x3 matrix using cofactors
	      (let ((m (make-float-vector '(3 3))))
		(do ((i 0 (+ i 1)))
		    ((= i 3))
		  (do ((j 0 (+ j 1)))
		      ((= j 3))
		    (set! (m i j) (mat i j))))
		(set! (mat 0 0) (- (* (m 1 1) (m 2 2)) (* (m 1 2) (m 2 1))))
		(set! (mat 0 1) (- (* (m 0 2) (m 2 1)) (* (m 0 1) (m 2 2))))
		(set! (mat 0 2) (- (* (m 0 1) (m 1 2)) (* (m 0 2) (m 1 1))))
		(set! (mat 1 0) (- (* (m 1 2) (m 2 0)) (* (m 1 0) (m 2 2))))
		(set! (mat 1 1) (- (* (m 0 0) (m 2 2)) (* (m 0 2) (m 2 0))))
		(set! (mat 1 2) (- (* (m 0 2) (m 1 0)) (* (m 0 0) (m 1 2))))
		(set! (mat 2 0) (- (* (m 1 0) (m 2 1)) (* (m 1 1) (m 2 0))))
		(set! (mat 2 1) (- (* (m 0 1) (m 2 0)) (* (m 0 0) (m 2 1))))
		(set! (mat 2 2) (- (* (m 0 0) (m 1 1)) (* (m 0 1) (m 1 0))))
		
		(let ((det (+ (* (m 0 0) (mat 0 0))
			      (* (m 0 1) (mat 1 0))
			      (* (m 0 2) (mat 2 0)))))
		  (and (> (abs det) 1e-06)
		       (do ((invdet (/ 1.0 det))
			    (row 0 (+ 1 row)))
			   ((= row 3) mat)
			 (do ((col 0 (+ 1 col)))
			     ((= col 3))
			   (set! (mat row col) (* (mat row col) invdet)))))))))
	   (invert2x2 
	    (lambda (mat) ; invert a 2x2 matrix
	      (let ((m (make-float-vector '(2 2)))
		    (det (- (* (mat 0 0) (mat 1 1))
			    (* (mat 1 0) (mat 0 1)))))
		(and (> (abs det) 1e-06)
		     (begin
		       (set! (m 0 0) (/ (mat 1 1) det))
		       (set! (m 1 1) (/ (mat 0 0) det))
		       (set! (m 0 1) (- (/ (mat 0 1) det)))
		       (set! (m 1 0) (- (/ (mat 1 0) det)))
		       m))))))
    
    ;; arrange-speakers
    (lambda* ((speakers ())
	      (groups ())
	      (delays ())
	      (distances ())
	      (channel-map ()))
      (if (null? speakers)
	  (error 'mus-error "a speaker configuration must have at least one speaker!~%"))
      
      (if (pair? groups)
	  (let ((first-len (length (car groups))))
	    (for-each
	     (lambda (group)
	       (if (not (= (length group) first-len))
		   (error 'mus-error "all groups must be of the same length! (~A)~%" first-len)))
	     groups))
	  
	  ;; if the speakers are defined with only azimuth angles (no elevation)
	  (if (not (pair? (car speakers)))
	      ;; then create the groups ourselves because it is a 2d configuration;
	      ;; we could create the 3d configuration groups but the algorithm for
	      ;; doing that in the generic case is not trivial
	      
	      (let ((len (length speakers)))
		(if (= len 1)
		    (set! groups (list (list 0)))
		    (do ((i 0 (+ i 1))
			 (j 1 (+ j 1)))
			((= i len)
			 (set! groups (reverse groups)))
		      (set! groups (cons (list i (modulo j len)) groups)))))))
      
      (if (null? groups)
	  (error 'mus-error "no groups specified, speakers must be arranged in groups~%"))
      
      (when (pair? delays)
	(if (pair? distances)
	    (error 'mus-error "please specify delays or distances but not both~%"))
	
	(if (> (length speakers) (length delays))
	    (error 'mus-error "all speaker delays have to be specified, only ~A supplied [~A]~%" (length delays) delays)
	    (if (< (length speakers) (length delays))
		(error 'mus-error "more speaker delays than speakers, ~A supplied instead of ~A [~A]~%" (length delays) (length speakers) delays)))
	
	(for-each
	 (lambda (dly)
	   (if (< dly 0.0) (error 'mus-error "delays must be all positive, ~A is negative~%" dly)))
	 delays))
      
      (when (pair? distances)
	(if (> (length speakers) (length distances))
	    (error 'mus-error "all speaker distances have to be specified, only ~A supplied [~A]~%" (length distances) distances)
	    (if (< (length speakers) (length distances))
		(error 'mus-error "more speaker distances than speakers, ~A supplied instead of ~A [~A]~%" (length distances) (length speakers) distances)))
	
	(for-each
	 (lambda (dly)
	   (if (< dly 0.0) (error 'mus-error "distances must be all positive, ~A is negative~%" dly)))
	 distances))
      
      (if (pair? channel-map)
	  (if (> (length speakers) (length channel-map))
	      (error 'mus-error "must map all speakers to output channels, only ~A mapped [~A]~%" (length channel-map) channel-map)
	      (if (< (length speakers) (length channel-map))
		  (error 'mus-error "trying to map more channels than there are speakers, ~A supplied instead of ~A [~A]~%" 
			 (length channel-map) (length speakers) channel-map))))
      
      ;; collect unit vectors describing the speaker positions
      (let* ((coords
	      (let ((val ()))
		(for-each
		 (lambda (s) ; speakers
		   (let* ((evec (let ((e (if (pair? s) (or (cadr s) 0.0) 0.0)))
				  (cis (/ (* e 2 pi) dlocsig-one-turn))))
			  (avec (let ((a (if (pair? s) (car s) s)))
				  (cis (/ (* a 2 pi) dlocsig-one-turn))))
			  (dxy (real-part evec))
			  (z (imag-part evec))
			  (x (* dxy (imag-part avec)))
			  (y (* dxy (real-part avec)))
			  (mag (sqrt (+ (* x x) (* y y) (* z z)))))
		     (set! val (cons (list (/ x mag) (/ y mag) (/ z mag)) val))))
		 speakers)
		(reverse val)))
	     
	     ;; find delay times from specified distances or delays
	     (times (let ((min-dist (if (pair? distances)  ; minimum distance
					(apply min distances)
					0.0))
			  (v (make-float-vector (length speakers))))
		      (do ((i 0 (+ i 1)))
			  ((= i (length speakers)))
			(set! (v i) (let ((distance (and (pair? distances) (distances i)))
					  (dly (and (pair? delays) (delays i))))
				      (or dly
					  (and (number? distance)
					       (/ (- distance min-dist) dlocsig-speed-of-sound))
					  0.0))))
		      v))
	     
	     ;; create the group structures
	     (groups (let ((vals ())
			   (id 0))
		       (for-each 
			(lambda (group)
			  (let* ((size (length group))
				 (vertices (map coords group))
				 (matrix (if (= size 3)
					     (do ((m (make-float-vector '(3 3)))
						  (i 0 (+ i 1)))
						 ((= i 3)
						  (invert3x3 m))   
					       (do ((j 0 (+ j 1)))
						   ((= j 3))
						 (set! (m i j) ((vertices i) j))))
					     (and (= size 2)
						  (do ((m (make-float-vector '(2 2)))
						       (i 0 (+ i 1)))
						      ((= i 2)
						       (invert2x2 m))   
						    (do ((j 0 (+ j 1)))
							((= j 2))
						      (set! (m i j) ((vertices i) j))))))))
			    (set! vals (cons (make-group :id id
							 :size size
							 :speakers group
							 :vertices vertices
							 :matrix matrix)
					     vals))
			    (set! id (+ 1 id))))
			groups)
		       (reverse vals))))
	
	;; check validity of map entries
	(if channel-map
	    (let ((entries (length channel-map)))
	      (for-each
	       (lambda (entry)
		 (if (>= entry entries)
		     (error 'mus-error "channel ~A in map ~A is out of range (max=~A)~%" entry channel-map entries)))
	       channel-map)
	      (if (has-duplicates? channel-map)
		  (error 'mus-error "there are duplicate channels in channel-map ~A~%" channel-map))))
	
	;; create the speaker configuration structure
	
	(make-speaker-config :number (length speakers)
			     :dimension (group-size (car groups))
			     :coords coords
			     :groups groups
			     :delays times
			     :omap (do ((v (make-vector (length speakers)))
					(chan 0 (+ 1 chan)))
				       ((= chan (length speakers)) v)
				     (set! (v chan) (or (and (pair? channel-map) (channel-map chan))
							chan))))))))
    
;;; Default speaker configurations

(define dlocsig-speaker-configs
  ;; by default up to eight channels, 2-d and 3-d configurations
  (list 
   (list
    ;;
    ;; 2-D speaker configurations
    ;; no channels: impossible
    #f
    ;; mono
    (arrange-speakers :speakers '(0))
    ;; stereo
    (arrange-speakers :speakers '(-60 60))
    ;; three channels
    (arrange-speakers :speakers '(-45 45 180))
    ;; four channels
    (arrange-speakers :speakers '(-45 45 135 225))
    ;; five channels (5.1 arrangement)
    (arrange-speakers :speakers '(-45 0 45 135 -135))
    ;; six channels
    (arrange-speakers :speakers '(-60 0 60 120 180 240))
    ;; seven channels
    (arrange-speakers :speakers '(-45 0 45 100 140 -140 -100))
    ;; eight speakers
    (arrange-speakers :speakers '(-22.5 22.5 67.5 112.5 157.5 202.5 247.5 292.5)))
   ;;
   ;; 3-D speaker configurations
   ;;
   (list
    ;; no channels: impossible
    #f
    ;; mono
    #f
    ;; stereo
    #f
    ;; three channels
    #f
    ;; four channels 3d
    (arrange-speakers :speakers '((-60 0) (60 0) (180 0)
				  (0 90))
		      :groups '((0 1 3) (1 2 3) (2 0 3)
				;; floor
				(0 1 2)))
    ;; five channels 3d
    (arrange-speakers :speakers '((-45 0) (45 0) (135 0) (-135 0)
				  (0 90))
		      :groups '((0 1 4) (1 2 4) (2 3 4) (3 0 4)
				;; floor
				(0 1 2) (2 3 0)))
    ;; six channels 3d
    (arrange-speakers :speakers '((-45 0) (45 0) (135 0) (-135 0)
				  (-90 60) (90 60))
		      :groups '((0 1 4) (1 4 5) (1 2 5) (2 3 5) (3 4 5) (3 0 4)
				;; floor
				(0 1 2) (2 3 0)))
    ;; seven channels 3d
    (arrange-speakers :speakers '((-45 0) (45 0) (135 0) (-135 0)
				  (-60 60) (60 60) (180 60))
		      :groups '((0 1 4) (1 4 5) (1 2 5) (2 6 5) (2 3 6) (3 4 6) (3 0 4) (4 5 6)
				;; floor
				(0 1 2) (2 3 0)))
    ;; eight speakers 3d
    (arrange-speakers :speakers '((-45 -10) (45 -10) (135 -10) (225 -10)
				  (-45 45) (45 45) (135 45) (225 45))
		      :groups '((0 4 5) (0 5 1) (5 1 2) (2 6 5) (6 7 2) (2 3 7)
				(3 7 4) (3 0 4)
				;; ceiling
				(4 7 6) (6 5 4)
				;; floor
				(0 1 2) (2 3 0))))))


;;; Set a particular speaker configuration

(define set-speaker-configuration 
  (let ((documentation "(set-speaker-configuration config (configs dlocsig-speaker-configs)) sets a dlocsig speaker configuration"))
    (lambda* (config (configs dlocsig-speaker-configs))
      (let ((lst ((if (< (speaker-config-dimension config) 3) car cadr) configs))
	    (num (speaker-config-number config)))
	(set! (lst num) config)))))


;;; Get the speaker configuration for a given number of output channels

(define get-speaker-configuration 
  (let ((documentation "(get-speaker-configuration channels (3d dlocsig-3d) (configs dlocsig-speaker-configs)) returns a dlocsig speaker configuration"))
    (lambda* (channels (3d dlocsig-3d) (configs dlocsig-speaker-configs))
      (let ((config (((if 3d cadr car) configs) channels)))
	(if (null? config)
	    (error 'mus-error "no speaker configuration exists for ~A ~A output channel~A~%~%" 
		   (if 3d "tridimensional" "bidimensional")
		   channels (if (= channels 1) "s" "")))
	config))))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dlocsig unit generator
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; global dlocsig parameters

(define dlocsig-path ())
(define dlocsig-scaler 1.0)
(define dlocsig-direct-power 1.5)
(define dlocsig-inside-direct-power 1.5)
(define dlocsig-reverb-power 0.5)
(define dlocsig-inside-reverb-power 0.5)
(define dlocsig-initial-delay #f)
(define dlocsig-unity-gain-distance #f)
(define dlocsig-reverb-amount 0.04)
(define dlocsig-inside-radius 1.0)
(define dlocsig-minimum-segment-length 1.0)

;; render using:

(define amplitude-panning 1)
(define ambisonics 2)
(define decoded-ambisonics 3)
					;(define stereo-hrtf 4)

					; for backwards compatibility
(define b-format-ambisonics ambisonics)

					; a reasonable default

(define dlocsig-render-using amplitude-panning)

;; ambisonics horizontal and vertical order for encoding
;; the default is first order b-format WXYZ

(define dlocsig-ambisonics-h-order 1)
(define dlocsig-ambisonics-v-order 1)

;; globals for ambisonics

(define point707 (cos (/ (* pi 2) 8.0)))
(define dlocsig-ambisonics-scaler point707)
(define dlocsig-ambisonics-ho-rev-scaler 0.05)
;; for 3rd order FuMa

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Get number of channels needed by ambisonics

(define* (ambisonics-channels 
	  (h-order dlocsig-ambisonics-h-order)
	  (v-order dlocsig-ambisonics-v-order))
  (if (< h-order 0)
      0              ;; error: we need at least horizontal order 1!
      (let ((count 0))
	(if (>= h-order 1)
	    ;; W X Y
	    (set! count (+ count 3)))
	(if (>= v-order 1)
	    ;; Z
	    (set! count (+ count 1)))
	(if (>= v-order 2)
	    ;; R S T
	    (set! count (+ count 3)))
	(if (>= h-order 2)
	    ;; U V
	    (set! count (+ count 2)))
	(if (>= v-order 3)
	    ;; K L M N O
	    (set! count (+ count 5)))
	(if (>= h-order 3)
	    ;; P Q
	    (set! count (+ count 2)))
	count)))
	

;;;;;;;;;
;;; Paths
;;;;;;;;;

;;; Generic path class

;;; path is a list (type rx ry rz rv rt tx ty tz tt ...)

(define path-rx (dilambda (lambda (p) (p 1)) (lambda (p val) (set! (p 1) val))))
(define path-ry (dilambda (lambda (p) (p 2)) (lambda (p val) (set! (p 2) val))))
(define path-rz (dilambda (lambda (p) (p 3)) (lambda (p val) (set! (p 3) val))))
(define path-rv (dilambda (lambda (p) (p 4)) (lambda (p val) (set! (p 4) val))))
(define path-rt (dilambda (lambda (p) (p 5)) (lambda (p val) (set! (p 5) val))))
(define path-tx (dilambda (lambda (p) (p 6)) (lambda (p val) (set! (p 6) val))))
(define path-ty (dilambda (lambda (p) (p 7)) (lambda (p val) (set! (p 7) val))))
(define path-tz (dilambda (lambda (p) (p 8)) (lambda (p val) (set! (p 8) val))))
(define path-tt (dilambda (lambda (p) (p 9)) (lambda (p val) (set! (p 9) val))))

					;(define (make-path) (list 'path () () () () () () () () ()))
#|
(define (describe path)
  (format #f
	  (if (memq (car path) '(bezier-path open-bezier-path))
	      (values
	       "<bezier-path>:~%  rx: ~A~%  ry: ~A~%  rz: ~A~%  rv: ~A~%  rt: ~A~%  tx: ~A~%  ty: ~A~%  tz: ~A~%  tt: ~A~%  ~
                         x: ~A~%  y: ~A~%  z: ~A~%  v: ~A~%  bx: ~A~%  by: ~A~%  bz: ~A~%  error: ~A~%  curvature: ~A~%"
	       (path-rx path) (path-ry path) (path-rz path) (path-rv path) (path-rt path) (path-tx path)
	       (path-ty path) (path-tz path) (path-tt path) (bezier-x path) (bezier-y path)
	       (bezier-z path) (bezier-v path) (bezier-bx path) (bezier-by path) (bezier-bz path)
	       (bezier-error path) (bezier-curvature path))
	      (values
	       "<path>:~%  rx: ~A~%  ry: ~A~%  rz: ~A~%  rv: ~A~%  rt: ~A~%  tx: ~A~%  ty: ~A~%  tz: ~A~%  tt: ~A~%"
	       (path-rx path) (path-ry path) (path-rz path) (path-rv path) (path-rt path) (path-tx path)
	       (path-ty path) (path-tz path) (path-tt path)))))
|#

;;; Inquiries into the state of the path

(define (not-rendered path)
  (null? (path-rx path)))

(define (not-transformed path)
  (null? (path-tx path)))

;;; Reset any transformations on the originally rendered path

(define (reset-transformation path)
  (set! (path-tt path) ())
  (set! (path-tx path) ())
  (set! (path-ty path) ())
  (set! (path-tz path) ())
  path)

;;; Reset the rendered path (and any transformations)

(define (reset-rendering path)
  (set! (path-rt path) ())
  (set! (path-rv path) ())
  (set! (path-rx path) ())
  (set! (path-ry path) ())
  (set! (path-rz path) ())
  (reset-transformation path))

;;; Return the best possible set of coordinates

(define list?? 
  (let ((documentation "list?? returns a if it is a list"))
    (lambda (a) 
      (and (pair? a) a))))

(define (path-x path)
  (or (list?? (path-tx path))
      (list?? (path-rx path))
      (path-rx (render-path path))))

(define (path-y path)
  (or (list?? (path-ty path))
      (list?? (path-ry path))
      (path-ry (render-path path))))

(define (path-z path)
  (or (list?? (path-tz path))
      (list?? (path-rz path))
      (path-rz (render-path path))))

(define (path-time path)
  (or (list?? (path-tt path))
      (list?? (path-rt path))
      (path-rt (render-path path))))


;;;;;;;;;;;;;;;;
;;; Bezier paths
;;;;;;;;;;;;;;;;

;;; Parse a path as two or three-dimensional paths

(define path-3d #f)

;;; Path class for bezier rendered paths

;;; bezier-path is path + path 3d polar x y z v bx by bz error curvature


(define bezier-path      (dilambda (lambda (p) (p 10)) (lambda (p val) (set! (p 10) val))))
(define bezier-3d        (dilambda (lambda (p) (p 11)) (lambda (p val) (set! (p 11) val))))
(define bezier-polar     (dilambda (lambda (p) (p 12)) (lambda (p val) (set! (p 12) val))))
(define bezier-x         (dilambda (lambda (p) (p 13)) (lambda (p val) (set! (p 13) val))))
(define bezier-y         (dilambda (lambda (p) (p 14)) (lambda (p val) (set! (p 14) val))))
(define bezier-z         (dilambda (lambda (p) (p 15)) (lambda (p val) (set! (p 15) val))))
(define bezier-v         (dilambda (lambda (p) (p 16)) (lambda (p val) (set! (p 16) val))))
(define bezier-bx        (dilambda (lambda (p) (p 17)) (lambda (p val) (set! (p 17) val))))
(define bezier-by        (dilambda (lambda (p) (p 18)) (lambda (p val) (set! (p 18) val))))
(define bezier-bz        (dilambda (lambda (p) (p 19)) (lambda (p val) (set! (p 19) val))))
(define bezier-error     (dilambda (lambda (p) (p 20)) (lambda (p val) (set! (p 20) val))))
(define bezier-curvature (dilambda (lambda (p) (p 21)) (lambda (p val) (set! (p 21) val))))

(define* (make-bezier-path (path ()) (3d #t) polar (error 0.01) curvature)
  (list 'bezier-path () () () () () () () () () path 3d polar () () () () () () () error curvature))


;;; Path class for open bezier paths

(define initial-direction (dilambda (lambda (p) (p 22)) (lambda (p val) (set! (p 22) val))))
(define final-direction   (dilambda (lambda (p) (p 23)) (lambda (p val) (set! (p 23) val))))

(define* (make-open-bezier-path (path ()) (3d #t) polar (error 0.01) curvature 
				(initial-direction '(0.0 0.0 0.0)) (final-direction '(0.0 0.0 0.0)))
  (list 'open-bezier-path () () () () () () () () () path 3d polar () () () () () () () error curvature initial-direction final-direction))



;;;
;;; Generic defining function (for open, closed, polar and cartesian paths)
;;;

(define make-path
  (let ()

    (define (make-path-error-checks path closed initial-direction final-direction)
      ;; some sanity checks
      (if (null? path)
	  (error 'mus-error "Can't define a path with no points in it~%"))
      (when closed 
	(if initial-direction
	    (error 'mus-error "Can't specify initial direction ~A for a closed path ~A~%" initial-direction path))
	(if final-direction
	    (error 'mus-error "Can't specify final direction ~A for a closed path ~A~%" final-direction path))
	(let ((closed? (if (pair? (car path))
			   (let ((start (car path))
				 (end (car (last path))))
			     (and (= (car start) (car end))
				  (= (cadr start) (cadr end))
				  (or (not path-3d)
				      (= (third start) (third end)))))
			   (let ((end (last path (if path-3d 3 2))))
			     (and (= (car path) (car end))
				  (= (cadr path) (cadr end))
				  (or (not path-3d)
				      (= (third path) (third end))))))))
	  (if (not closed?)
	      (error 'mus-error "Closed path ~A is not closed~%" path)))))

    (lambda* (path
	      (3d path-3d)
	      polar
	      closed
	      curvature
	      (error 0.01) ; this name ("error") is a bad idea -- it means we can't call the real error function (this is Scheme, not CL)
	      ;; only for open paths
	      initial-direction
	      final-direction)
  
      (make-path-error-checks path closed initial-direction final-direction) ; the error check uses path-3d -- was 3d intended?
      
      ;; create the path structure
      (if closed
	  (make-bezier-path
	   :path path
	   :3d 3d
	   :polar polar
	   :curvature curvature
	   :error error)
	  (make-open-bezier-path
	   :path path
	   :3d 3d
	   :polar polar
	   :curvature curvature
	   :error error
	   :initial-direction initial-direction
	   :final-direction final-direction)))))


;;; Some convenient abbreviations

(define* (make-polar-path path
			  (3d path-3d)
			  closed
			  curvature
			  (error 0.01)
			  ;; only for open paths
			  initial-direction
			  final-direction)
  (if closed
      (make-path :path path
		 :3d 3d
		 :polar #t
		 :closed closed
		 :curvature curvature
		 :error error)
      (make-path :path path
		 :3d 3d
		 :polar #t
		 :closed closed
		 :curvature curvature
		 :error error
		 :initial-direction initial-direction
		 :final-direction final-direction)))

(define* (make-closed-path path
			   (3d path-3d)
			   polar
			   curvature
			   (error 0.01))
  (make-path :path path
	     :3d 3d
	     :polar polar
	     :closed #t
	     :curvature curvature
	     :error error))



;;;
;;; Parse a path and transform it into cartesian coordinates
;;;

(define (not-parsed path)
  (null? (bezier-x path)))


;;; Parse a set of 2d or 3d points into the separate coordinates

(define parse-cartesian-coordinates 
  (let ((documentation "(parse-cartesian-coordinates points 3d) parses a set of 2d or 3d points into the separate coordinates"))
    (lambda (points 3d)
      (if (pair? (car points))
	  ;; decode a list of lists into x:y:z:v components
	  ;; 3d -> t [default]
	  ;;   '((x0 y0 z0 v0) (x1 y1 z1 v1)...(xn yn zn vn))
	  ;;   '((x0 y0 z0) (x1 y1 z1)...(xn yn zn))
	  ;;   '((x0 y0) (x1 y1)...(xn yn)) 
	  ;;      v: relative velocity
	  ;;      x, y, z: coordinates of source [missing z's assumed 0.0]
	  ;; 3d -> nil
	  ;;   '((x0 y0 v0) (x1 y1 v1)...(xn yn vn))
	  ;;   '((x0 y0) (x1 y1)...(xn yn))
	  ;;      v: relative velocity
	  ;;      x, y, z: coordinates of source [missing z's assumed 0.0]
	  (let ((v ())
		(x ())
		(y ())
		(z ()))
	    (for-each
	     (lambda (p)
	       (set! x (cons (car p) x))
	       (set! y (cons (cadr p) y))
	       (set! z (cons (if 3d (or (third p) 0.0) 0.0) z))
	       (set! v (cons ((if 3d fourth third) p) v)))
	     points)
	    (map reverse (list x y z v)))
	  
	  ;; decode a plain list
	  (let ((px ())
		(py ())
		(len (length points)))
	    (if 3d
		;; it's a three dimensional list
		;; '(x0 y0 z0 x1 y1 z1 ... xn yn zn)
		;;     x, y, z: coordinates of source
		(do ((pz ())
		     (i 0 (+ i 3)))
		    ((>= i len)
		     (list (reverse px) (reverse py) (reverse pz) (make-list (length px) #f)))   
		  (set! px (cons (points i) px))
		  (set! py (cons (points (+ i 1)) py))
		  (set! pz (cons (points (+ i 2)) pz)))
		;; it's a two dimensional list
		;; '(x0 y0 x1 y1 ... xn yn)
		;;     x, y, z: coordinates of source [missing z's assumed 0.0]
		(do ((i 0 (+ i 2)))
		    ((>= i len)
		     (list (reverse px) (reverse py) (make-list (length px) 0.0) (make-list (length px) #f)))   
		  (set! px (cons (points i) px))
		  (set! py (cons (points (+ i 1)) py)))))))))

;;; Parse a set of 2d or 3d polar points into the separate coordinates

(define parse-polar-coordinates 
  (let ((documentation "(parse-polar-coordinates points 3d) parses a polar path"))
    (lambda (points 3d)
      (let ((x ())
	    (y ()))
	(if (pair? (car points))
	    ;; decode a list of lists of d:a:e:v into x:y:z:v components
	    ;; 3d --> t [default]
	    ;;   '((d0 a0 e0 v0) (d1 a1 e1 v1)...(dn an en vn))
	    ;;   '((d0 a0 e0) (d1 a1 e1)...(dn an en))
	    ;;   '((d0 a0) (d1 a1)...(dn an))  
	    ;; 3d --> nil
	    ;;   '((d0 a0 v0) (d1 a1 v1)...(dn an vn))
	    ;;   '((d0 a0) (d1 a1)...(dn an))
	    ;;      v: velocity
	    ;;      d: distance
	    ;;      a: azimut angle
	    ;;      e: elevarion angle [missing elevations assumed 0.0]
	    (let ((z ())
		  (v ()))
	      (for-each
	       (lambda (p)
		 (let ((d (car p))
			(evec (let ((e (if (and 3d (pair? (cddr p))) (caddr p) 0.0)))
				(cis (/ (* e 2 pi) dlocsig-one-turn)))))
		   (let ((dxy (* d (real-part evec)))
			 (avec (let ((a (cadr p)))
				 (cis (/ (* a 2 pi) dlocsig-one-turn)))))
		     (set! x (cons (* dxy (imag-part avec)) x))
		     (set! y (cons (* dxy (real-part avec)) y)))
		   (set! z (cons (* d (imag-part evec)) z))
		   (set! v (cons ((if 3d fourth third) p) v))))
	       points)
	      (map reverse (list x y z v)))
	    
	    ;; decode a list of d:a:e components
	    (let ((len (length points)))
	      (if 3d
		  ;; decode a three dimensional list
		  ;;   '(d0 a0 e0 d1 a1 e1 ... dn an en)
		  ;;      d: distance
		  ;;      a: azimut angle
		  ;;      e: elevarion angle [missing elevations assumed 0.0]
		  (do ((z ())
		       (i 0 (+ i 3)))
		      ((>= i len)
		       (list (reverse x) (reverse y) (reverse z) (make-list (length x) #f)))
		    (let* ((d (points i))
			   (evec (let ((e (points (+ i 2))))
				   (cis (/ (* e 2 pi) dlocsig-one-turn))))
			   (dxy (* d (real-part evec)))
			   (avec (let ((a (points (+ i 1))))
				   (cis (/ (* a 2 pi) dlocsig-one-turn)))))
		      (set! x (cons (* dxy (imag-part avec)) x))
		      (set! y (cons (* dxy (real-part avec)) y))
		      (set! z (cons (* d (imag-part evec)) z))))
		  
		  ;; decode a two dimensional list
		  ;;   '(d0 a0 d1 a1 ... dn an)
		  ;;      d: distance
		  ;;      a: azimut angle
		  ;;      e: elevarion angle [missing elevations assumed 0.0]
		  (do ((i 0 (+ i 2)))
		      ((>= i len)
		       (list (reverse x) (reverse y) (make-list (length x) 0.0) (make-list (length x) #f)))     
		    (let ((d (points i))
			  (avec (let ((a (points (+ i 1))))
				  (cis (/ (* a 2 pi) dlocsig-one-turn)))))
		      (set! x (cons (* d (imag-part avec)) x))
		      (set! y (cons (* d (real-part avec)) y)))))))))))
		

(define (xparse-path xpath)
  (let ((polar (bezier-polar xpath))
	(points (bezier-path xpath))
	(3d (bezier-3d xpath)))
    (let ((vals ((if polar parse-polar-coordinates parse-cartesian-coordinates) points 3d)))
      (set! (bezier-x xpath) (car vals))
      (set! (bezier-y xpath) (cadr vals))
      (set! (bezier-z xpath) (caddr vals))
      (set! (bezier-v xpath) (cadddr vals))))
  (for-each
   (lambda (v)
     (if (and (real? v) 
	      (< v 0))
	 (error 'mus-error "velocities for path ~A must be all positive~%" (bezier-path xpath))))
   (bezier-v xpath))
  (reset-fit xpath))


;;;
;;; Bezier curve fitting auxiliary functions
;;;

;;; Pythagoras

(define distance 
  (let ((documentation "(distance x y z) returns the euclidean distance of (x y z) from the origin"))
    (lambda (x y z)
      (sqrt (+ (* x x) (* y y) (* z z))))))

;;; Nearest point in a line

(define nearest-point 
  (let ((same (lambda (a0 b0 c0 a1 b1 c1)
		(and (= a0 a1) 
		     (= b0 b1)
		     (= c0 c1)))))
    (lambda (x0 y0 z0 x1 y1 z1 px py pz)
      (cond ((same x0 y0 z0 px py pz) (list x0 y0 z0))
	    ((same x1 y1 z1 px py pz) (list x1 y1 z1))
	    ((same x0 y0 z0 x1 y1 z1) (list x0 y0 z0))
	    (else (let ((xm0 (- x1 x0))
			(ym0 (- y1 y0))
			(zm0 (- z1 z0)))
		    (let ((ratio (let ((p (let ((xm1 (- px x0))
						(ym1 (- py y0))
						(zm1 (- pz z0))
						(vcos (lambda (a0 b0 c0 a1 b1 c1)
							(/ (+ (* a0 a1) (* b0 b1) (* c0 c1))
							   (* (distance a0 b0 c0) (distance a1 b1 c1))))))
					    (* (distance xm1 ym1 zm1)
					       (vcos xm0 ym0 zm0 xm1 ym1 zm1)))))
				    (/ p (distance xm0 ym0 zm0)))))
		      (list (+ x0 (* xm0 ratio))
			    (+ y0 (* ym0 ratio))
			    (+ z0 (* zm0 ratio))))))))))

;;; Bezier curve fitting auxilliary functions

(define path-ak-even #f)
(define path-ak-odd #f)
(define path-maxcoeff 8)

(define (make-a-even)
  (let ((g (let ((path-gtab #f))
	     (lambda (m)
	       (if (not path-gtab)
		   (begin
		     (set! path-gtab (make-vector path-maxcoeff))
		     (set! (path-gtab 0) 1)
		     (set! (path-gtab 1) -4)
		     (do ((i 2 (+ i 1)))
			 ((= i path-maxcoeff))
		       (set! (path-gtab i) (- (* -4 (path-gtab (- i 1)))
					      (path-gtab (- i 2)))))))
	       (path-gtab m)))))
  
  (set! path-ak-even (make-vector (- path-maxcoeff 1)))
  (do ((m 1 (+ 1 m)))
      ((= m path-maxcoeff))
    (set! (path-ak-even (- m 1)) (make-vector m))
    (do ((k 1 (+ k 1)))
	((> k m))
      (set! ((path-ak-even (- m 1)) (- k 1)) (* 1.0 (/ (- (g (- m k))) (g m))))))))

(define (make-a-odd)
  (let ((f (let ((path-ftab #f))
	     (lambda (m)
	       (if (not path-ftab)
		   (begin
		     (set! path-ftab (make-vector path-maxcoeff))
		     (set! (path-ftab 0) 1)
		     (set! (path-ftab 1) -3)
		     (do ((i 2 (+ i 1)))
			 ((= i path-maxcoeff))
		       (set! (path-ftab i) (- (* -4 (path-ftab (- i 1)))
					      (path-ftab (- i 2)))))))
	       (path-ftab m)))))
  
  (set! path-ak-odd (make-vector (- path-maxcoeff 1)))
  (do ((m 1 (+ 1 m)))
      ((= m path-maxcoeff))
    (set! (path-ak-odd (- m 1)) (make-vector m))
    (do ((k 1 (+ k 1)))
	((> k m))
      (set! ((path-ak-odd (- m 1)) (- k 1)) (* 1.0 (/ (- (f (- m k))) (f m))))))))

;;; Calculate bezier difference vectors for the given path
;;; (path-x (make-path '((-10 10)(0 5)(10 10))))

(define (fit path)
  (let ((n (- (length (bezier-x path )) 1)))
    (cond ((not (eq? (car path) 'open-bezier-path))
	   (let ((m (/ (- n (if (odd? n) 3 4)) 2))
		 ;; data points P(i)
		 (p (vector (apply vector (bezier-x path))
			    (apply vector (bezier-y path))
			    (apply vector (bezier-z path))))
		 ;; control points D(i)
		 (d (let ((maker (lambda () (make-vector n 0.0))))
		      (vector (maker) (maker) (maker))))
	     
		 (a-1 (lambda (k n)
			(if (odd? (min (+ (* path-maxcoeff 2) 1) n))
			    (begin
			      (if (not path-ak-odd) (make-a-odd))
			      ((path-ak-odd (/ (- n 3) 2)) (- k 1)))
			    (begin
			      (if (not path-ak-even) (make-a-even))
			      ((path-ak-even (/ (- n 4) 2)) (- k 1))))))
		 
		 (xvector-ref (lambda (z j i)
				(z j (if (> i (- n 1))
					 (- i n)
					 (if (< i 0) 
					     (+ i n) i))))))
	     (do ((i 0 (+ i 1)))
		 ((= i n))
	       (do ((k 1 (+ k 1)))
		   ((> k m))
		 (do ((a 0 (+ 1 a)))
		     ((> a 2))
		   (set! (d a i)
			 (+ (d a i)
			    (* (a-1 k n)
			       (- (xvector-ref p a (+ i k))
				  (xvector-ref p a (- i k)))))))))
	     (if (bezier-curvature path)
		 (do ((i 0 (+ i 1)))
		     ((= i n))
		   (set! (d 0 i) (* (d 0 i) (bezier-curvature path)))
		   (set! (d 1 i) (* (d 1 i) (bezier-curvature path)))
		   (set! (d 2 i) (* (d 2 i) (bezier-curvature path)))))
	     (list (- n 1) p d)))
	  (else
	   (let ((m (- n 1))
		 ;; data points P(i)
		 (p (vector (apply vector (bezier-x path))
			    (apply vector (bezier-y path))
			    (apply vector (bezier-z path))))
		 ;; control points D(i)
		 (d (vector (make-vector (+ n 1) 0.0) 
			    (make-vector (+ n 1) 0.0) 
			    (make-vector (+ n 1) 0.0)))
	     
		 (ac (lambda (k n)
		       (let ((un (min n path-maxcoeff)))
			 (if (not path-ak-even) (make-a-even))
			 ((path-ak-even (- un 2)) (- k 1))))))
		 
	     (define (ref z j i) ; refers to 'd
	       (cond ((> i n) (z j (- i n)))
		     ((< i 0) (z j (+ i n)))
		     ((= i n) (- (z j n) (d j n)))
		     ((= i 0) (+ (z j 0) (d j 0)))
		     (else    (z j i))))
	     
	     ;; forced initial direction
	     (if (initial-direction path)
		 (begin
		   (set! (d 0 0) (car (initial-direction path)))
		   (set! (d 1 0) (cadr (initial-direction path)))
		   (set! (d 2 0) (third (initial-direction path))))
		 (begin
		   (set! (d 0 0) 0.0)
		   (set! (d 1 0) 0.0)
		   (set! (d 2 0) 0.0)))
	     
	     ;; forced final direction
	     (if (final-direction path)
		 (begin
		   (set! (d 0 n) (car (final-direction path)))
		   (set! (d 1 n) (cadr (final-direction path)))
		   (set! (d 2 n) (caddr (final-direction path))))
		 (begin
		   (set! (d 0 n) 0.0)
		   (set! (d 1 n) 0.0)
		   (set! (d 2 n) 0.0)))
	     
	     ;; calculate fit
	     (do ((i 1 (+ i 1)))
		 ((= i n))
	       (do ((k 1 (+ k 1)))
		   ((> k (min m (- path-maxcoeff 1))))
		 (let ((d0 (d 0 i))
		       (d1 (d 1 i))
		       (d2 (d 2 i)))
		   (set! (d 0 i) (+ d0 (* (ac k n)
					  (- (ref p 0 (+ i k))
					     (ref p 0 (- i k))))))
		   (set! (d 1 i) (+ d1 (* (ac k n)
					  (- (ref p 1 (+ i k))
					     (ref p 1 (- i k))))))
		   (set! (d 2 i) (+ d2 (* (ac k n)
					  (- (ref p 2 (+ i k))
					     (ref p 2 (- i k)))))))))
	     (list n p d))))))

;;; Calculate bezier control points for the given open path

(define (not-fitted path)
  (null? (bezier-bx path)))

(define (reset-fit path)
  (set! (bezier-bx path) ())
  (set! (bezier-by path) ())
  (set! (bezier-bz path) ())
  (reset-rendering path))

(define (fit-path path)
  (cond ((eq? (car path) 'open-bezier-path)
	 (if (not-parsed path)
	     (xparse-path path))
	 
	 (let ((points (length (bezier-x path))))
	   (cond ((> points 2)
		  (let ((vals (fit path)))
		    (let ((n (car vals))
			  (p (cadr vals))
			  (d (caddr vals)))
		      (let ((c (bezier-curvature path))
			    (cs (make-vector n)))
			;; setup the curvatures array
			(cond ((memq c '(#f ()))                          ; no curvature specified, default is 1.0
			       (do ((i 0 (+ i 1)))
				   ((= i n))
				 (set! (cs i) (list 1.0 1.0))))
			      ((number? c)                    ; same curvature for all segments
			       (do ((i 0 (+ i 1)))
				   ((= i n))
				 (set! (cs i) (list c c))))
			      ((and (pair? c) (= n (length c)))   ; list of curvatures
			       (let ((i 0))
				 (for-each
				  (lambda (ci)
				    (set! (cs i) (if (pair? ci) 
						     (if (= (length ci) 2)
							 ci
							 (error 'mus-error "curvature sublist must have two elements ~A~%" ci))
						     (list ci ci)))
				    (set! i (+ i 1)))
				  c)))
			      (else (error 'mus-error "bad curvature argument ~A to path, need ~A elements~%" c n)))
			
			;; calculate control points
			(do ((xc ())
			     (yc ())
			     (zc ())
			     (i 0 (+ i 1)))
			    ((= i n)
			     (set! (bezier-bx path) (reverse xc))
			     (set! (bezier-by path) (reverse yc))
			     (set! (bezier-bz path) (reverse zc)))
			     
			  (set! xc (cons (list (p 0 i)
					       (+ (p 0 i) (* (d 0 i) (car (cs i))))
					       (- (p 0 (+ i 1)) (* (d 0 (+ i 1)) (cadr (cs i))))
					       (p 0 (+ i 1))) xc))
			  (set! yc (cons (list (p 1 i)
					       (+ (p 1 i) (* (d 1 i) (car (cs i))))
					       (- (p 1 (+ i 1)) (* (d 1 (+ i 1)) (cadr (cs i))))
					       (p 1 (+ i 1))) yc))
			  (set! zc (cons (list (p 2 i)
					       (+ (p 2 i) (* (d 2 i) (car (cs i))))
					       (- (p 2 (+ i 1)) (* (d 2 (+ i 1)) (cadr (cs i))))
					       (p 2 (+ i 1))) zc)))))))
		 
		 ((= points 2)
		  ;; just a line, stays a line
		  (let ((x1 (car (bezier-x path)))
			(x2 (cadr (bezier-x path)))
			(y1 (car (bezier-y path)))
			(y2 (cadr (bezier-y path)))
			(z1 (car (bezier-z path)))
			(z2 (cadr (bezier-z path))))
		    (set! (bezier-bx path) (list (list x1 x1 x2 x2)))
		    (set! (bezier-by path) (list (list y1 y1 y2 y2)))
		    (set! (bezier-bz path) (list (list z1 z1 z2 z2)))))
		 ((= points 1)
		  ;; just one point, bezier won't do much here
		  (set! (bezier-bx path) ())
		  (set! (bezier-by path) ())
		  (set! (bezier-bz path) ())))

	   (reset-rendering path)))
	
	(else
	 (if (not-parsed path)
	     (xparse-path path))
	 
	 (if (> (length (bezier-x path)) 4)
	     (let ((vals (fit path)))
	       (do ((n (car vals))
		    (p (cadr vals))
		    (d (caddr vals))
		    ;; enough points, fit path
		    (xc ())
		    (yc ())
		    (zc ())
		    (i 0 (+ i 1)))
		   ((= i n)
		    (set! (bezier-bx path) (append (reverse xc) (list (list (p 0 n)
									    (+ (p 0 n) (d 0 n))
									    (- (p 0 0) (d 0 0))
									    (p 0 0)))))
		    (set! (bezier-by path) (append (reverse yc) (list (list (p 1 n)
									    (+ (p 1 n) (d 1 n))
									    (- (p 1 0) (d 1 0))
									    (p 1 0)))))
		    (set! (bezier-bz path) (append (reverse zc) (list (list (p 2 n)
									    (+ (p 2 n) (d 2 n))
									    (- (p 2 0) (d 2 0))
									    (p 2 0))))))
		 (set! xc (cons (list (p 0 i)
				      (+ (p 0 i) (d 0 i))
				      (- (p 0 (+ i 1)) (d 0 (+ i 1)))
				      (p 0 (+ i 1))) xc))
		 (set! yc (cons (list (p 1 i)
				      (+ (p 1 i) (d 1 i))
				      (- (p 1 (+ i 1)) (d 1 (+ i 1)))
				      (p 1 (+ i 1))) yc))
		 (set! zc (cons (list (p 2 i)
				      (+ (p 2 i) (d 2 i))
				      (- (p 2 (+ i 1)) (d 2 (+ i 1)))
				      (p 2 (+ i 1))) zc))))
	     
	     ;; not enough points to fit a closed path
	     (do ((xc ())
		  (yc ())
		  (zc ())
		  (len (min (length (bezier-x path)) (length (bezier-y path)) (length (bezier-z path))))
		  (i 0 (+ i 1)))
		 ((>= i len)
		  (format *stderr* "[fit-path:closed-path] not enough points to do bezier fit (~A points)" len)
		  (set! (bezier-bx path) (reverse xc))
		  (set! (bezier-by path) (reverse yc))
		  (set! (bezier-bz path) (reverse zc)))
	       (let ((x1 ((bezier-x path) i))
		     (x2 ((bezier-x path) (+ i 1)))
		     (y1 ((bezier-y path) i))
		     (y2 ((bezier-y path) (+ i 1)))
		     (z1 ((bezier-z path) i))
		     (z2 ((bezier-z path) (+ i 1))))
		 (set! xc (cons (list x1 x1 x2 x2) xc))
		 (set! yc (cons (list y1 y1 y2 y2) yc))
		 (set! zc (cons (list z1 z1 z2 z2) zc)))))
	 (reset-rendering path))))



;;;;;;;;;;;;;;;;;
;;; Literal paths
;;;;;;;;;;;;;;;;;


(define literal-points (dilambda (lambda (p) (p 10)) (lambda (p val) (set! (p 10) val))))
(define literal-3d     (dilambda (lambda (p) (p 11)) (lambda (p val) (set! (p 11) val))))
(define literal-polar  (dilambda (lambda (p) (p 12)) (lambda (p val) (set! (p 12) val))))

;;; Generic literal path creation function
(define* (make-literal-path (points ()) (3d path-3d) polar)
  (list 'literal-path () () () () () () () () () points 3d polar))

;;; Specific polar literal path creation function
(define* (make-literal-polar-path (points ()) (3d path-3d))
  (make-literal-path points 3d #t))


;;;;;;;;;;;
;;; Spirals
;;;;;;;;;;;

(define spiral-start-angle (dilambda (lambda (p) (p 13)) (lambda (p val) (set! (p 13) val))))
(define spiral-total-angle (dilambda (lambda (p) (p 14)) (lambda (p val) (set! (p 14) val))))
(define spiral-step-angle  (dilambda (lambda (p) (p 15)) (lambda (p val) (set! (p 15) val))))
(define spiral-turns       (dilambda (lambda (p) (p 16)) (lambda (p val) (set! (p 16) val))))
(define spiral-distance    (dilambda (lambda (p) (p 17)) (lambda (p val) (set! (p 17) val))))
(define spiral-height      (dilambda (lambda (p) (p 18)) (lambda (p val) (set! (p 18) val))))
(define spiral-velocity    (dilambda (lambda (p) (p 19)) (lambda (p val) (set! (p 19) val))))

(define* (make-spiral-path (start-angle 0.0)
			   total-angle
			   step-angle
			   (turns ())
			   (distance '(0 10 1 10))
			   (height '(0 0 1 0))
			   (velocity '(0 1 1 1)))
  (if (and total-angle (pair? turns))
      (error 'mus-error "can't specify total-angle [~A] and turns [~A] at the same time for the spiral path~%" total-angle turns))
  
  (list 'spiral-path () () () () () () () () () () path-3d #f 
	start-angle total-angle 
	(or step-angle (/ dlocsig-one-turn 100))
	turns distance height velocity))



;;; Transform a Bezier control point fit to a linear segment approximation

(define (bezier-render path)
  (if (not-fitted path)
      (fit-path path))
  (let ((xrx ()) (xry ()) (xrz ()) (xrv ()))
    
    ;; Create linear segment approximations of the bezier segments
    ;; make sure there are initial and final velocity values
    (if (not (pair? (bezier-v path)))
	(set! (bezier-v path) (list 1 1))
	(if (not (car (bezier-v path)))
	    (begin
	      (set! ((bezier-v path) 0) 1)
	      (set! ((bezier-v path) (- (length (bezier-v path)) 1)) 1))))
    
    ;; only one point means no movement, static source
    (if (= (length (bezier-x path)) 1)
	(begin
	  (set! (path-rx path) (bezier-x path))
	  (set! (path-ry path) (bezier-y path))
	  (set! (path-rz path) (bezier-z path))
	  (set! (path-rt path) (list 0.0))
	  (reset-transformation path)) ; after?
	(begin
	  (let ((len (length (bezier-bx path))))
					;(path-x (make-path '((-10 10)(0 5)(10 10))))
	    ;; render the path only if it has at least two points
	    (do ((i 0 (+ i 1)))
		((= i len))
	      (let ((x-bz ((bezier-bx path) i))
		    (y-bz ((bezier-by path) i))
		    (z-bz ((bezier-bz path) i)))
		(let ((vi-bz ((bezier-v path) i))
		      (vf-bz ((bezier-v path) (+ i 1)))
		      (xi-bz (car x-bz))
		      (xf-bz (x-bz (- (length x-bz) 1)))
		      (yi-bz (car y-bz))
		      (yf-bz (y-bz (- (length y-bz) 1)))
		      (zi-bz (car z-bz))
		      (zf-bz (z-bz (- (length z-bz) 1))))

		  (define berny 
		    ;; Create a linear segment rendering of a bezier segment
		    (let ((bezier-point 
			   (lambda (u c)
			     ;; Evaluate a point at parameter u in bezier segment
			     (let ((u1 (- 1 u))
				   (cr (vector (make-vector 3 0.0) (make-vector 3 0.0) (make-vector 3 0.0))))
			       (do ((j 0 (+ j 1)))
				   ((= j 3))
				 (set! (cr 0 j) (+ (* u1 (c 0 j)) (* u (c 0 (+ j 1)))))
				 (set! (cr 1 j) (+ (* u1 (c 1 j)) (* u (c 1 (+ j 1)))))
				 (set! (cr 2 j) (+ (* u1 (c 2 j)) (* u (c 2 (+ j 1))))))
			       (do ((i 1 (- i 1)))
				   ((< i 0))
				 (do ((j 0 (+ j 1)))
				     ((> j i))
				   (set! (cr 0 j) (+ (* u1 (cr 0 j)) (* u (cr 0 (+ j 1)))))
				   (set! (cr 1 j) (+ (* u1 (cr 1 j)) (* u (cr 1 (+ j 1)))))
				   (set! (cr 2 j) (+ (* u1 (cr 2 j)) (* u (cr 2 (+ j 1)))))))
			       (list (cr 0 0)
				     (cr 1 0)
				     (cr 2 0))))))
			   (lambda (xl yl zl xh yh zh ul u uh c err)
			     (let ((vals (bezier-point u c)))
			       (let ((x (car vals))
				     (y (cadr vals))
				     (z (caddr vals)))
				 (let ((val1 (nearest-point xl yl zl xh yh zh x y z)))
				   (let ((xn (car val1))
					 (yn (cadr val1))
					 (zn (caddr val1)))
				     (if (<= (distance (- xn x) (- yn y) (- zn z)) err)
					 (list () () ())
					 (let ((val2 (berny xl yl zl x y z ul (/ (+ ul u) 2) u c err))
					       (val3 (berny x y z xh yh zh u (/ (+ u uh) 2) uh c err)))
					   (let ((xi (car val2))
						 (yi (cadr val2))
						 (zi (caddr val2))
						 (xj (car val3))
						 (yj (cadr val3))
						 (zj (caddr val3)))
					     (list (append xi (cons x xj))
						   (append yi (cons y yj))
						   (append zi (cons z zj)))))))))))))
		  
		  (let ((vals (berny xi-bz yi-bz zi-bz xf-bz yf-bz zf-bz 0.0 0.5 1.0 
				     (vector (apply vector x-bz)
					     (apply vector y-bz)
					     (apply vector z-bz))
				     (bezier-error path))))
		      ;; approximate the bezier curve with linear segments
		    (set! xry (append xry (cons yi-bz (cadr vals))))
		    (set! xrz (append xrz (cons zi-bz (caddr vals))))
		    (let ((xs (car vals)))
		      (set! xrx (append xrx (cons xi-bz xs)))
		      ;; accumulate intermediate unknown velocities as nils
		      (set! xrv (append xrv (cons vi-bz (make-list (length xs) #f))))
		      (if (= i (- len 1))
			  (begin
			    ;; add the last point
			    (set! xrx (append xrx (list xf-bz)))
			    (set! xry (append xry (list yf-bz)))
			    (set! xrz (append xrz (list zf-bz)))
			    (set! xrv (append xrv (list vf-bz)))))))))))
	  
	  ;; calculate times for each velocity segment
	  (let ((ti 0)
		(times (list 0)))
	    (let ((len (- (length xrx) 1))
		  (xseg (list (xrx 0)))
		  (yseg (list (xry 0)))
		  (zseg (list (xrz 0)))
		  (vseg (list (xrv 0)))
		  (vi (xrv 0)))
	      (do ((i 0 (+ i 1)))
		  ((= i len))
		(let ((x (xrx (+ i 1)))
		      (y (xry (+ i 1)))
		      (z (xrz (+ i 1)))
		      (v (xrv (+ i 1))))
		  (set! xseg (append xseg (list x)))
		  (set! yseg (append yseg (list y)))
		  (set! zseg (append zseg (list z)))
		  (set! vseg (append vseg (list v)))
		  
		  (when v
		    (let ((dseg ())
			  (sum 0.0)
			  (len (- (length xseg) 1)))
		      (do ((i 0 (+ i 1)))
			  ((= i len))
			(let ((xsi (xseg i))
			      (ysi (yseg i))
			      (zsi (zseg i))
			      (xsf (xseg (+ i 1)))
			      (ysf (yseg (+ i 1)))
			      (zsf (zseg (+ i 1))))
			  
			  (set! sum (+ sum (distance (- xsf xsi) (- ysf ysi) (- zsf zsi))))
			  (set! dseg (cons sum dseg))))
		      
		      (let ((df (car dseg)))
			(set! dseg (reverse dseg))
			(let ((tseg ()))
			  (let ((a (/ (* (- v vi) (+ v vi)) df 4)))
			    (if (= vi 0.0) (set! vi 1))
			    (for-each
			     (lambda (d)
			       (let ((seg (+ ti (if (= v vi)
						    (/ d vi)
						    (/ (- (sqrt (+ (* vi vi) (* 4 a d))) vi) (* 2 a))))))
				 (set! tseg (cons seg tseg))))
			     dseg))
			  (set! ti (car tseg))
			  (set! times (append times (reverse tseg))))))
		    (set! xseg (list x))
		    (set! yseg (list y))
		    (set! zseg (list z))
		    (set! vseg (list v))
		    (set! vi v)))))
	    
	    (set! (path-rx path) xrx)
	    (set! (path-ry path) xry)
	    (set! (path-rz path) xrz)
	    (set! (path-rt path) 
		  (let ((tf (times (- (length times) 1)))
			(val ()))
		    (for-each
		     (lambda (ti)
		       (set! val (cons (/ ti tf) val)))
		     times)
		    (reverse val)))
	    (reset-transformation path))))))

;; (set! p (make-path '((-10 10 0 0) (0 5 0 1) (10 10 0 0)) :error 0.01))
;; (set! p (make-path '((-10 10 0 1) (-7 7 0 0.9) (0 5 0 0) (7 7 0 0.2) (10 10 0 1)) :error 0.001))
;; (with-sound(:channels 4 :play #f) (sinewave 0 2 880 0.5 :path p))


(define (literal-render path)
  
  ;; Render a user-defined literal path from the data points
  
  ;; decode the points into coordinates
  (let ((points (literal-points path))
	(3d (literal-3d path))
	(polar (literal-polar path)))
    (let ((vals ((if polar parse-polar-coordinates parse-cartesian-coordinates) points 3d)))
      (set! (path-rx path) (car vals))
      (set! (path-ry path) (cadr vals))
      (set! (path-rz path) (caddr vals))
      (set! (path-rv path) (cadddr vals)))
    
    ;; make sure there are initial and final velocity values
    (if (not (car (path-rv path)))
	(begin
	  (set! ((path-rv path) 0) 1)
	  (set! ((path-rv path) (- (length (path-rv path)) 1)) 1)))
    
    ;; only one point means no movement, static source
    (if (= (length (path-rx path)) 1)
	(begin
	  (set! (path-rt path) (list 0.0))
	  (reset-transformation path))
	(let ((rx (path-rx path))
	      (ry (path-ry path))
	      (rz (path-rz path))
	      (rv (path-rv path)))
	  (let ((xseg (list (car rx)))
		(yseg (list (car ry)))
		(zseg (list (car rz)))
		(vseg (list (car rv)))
		(vi (car rv))
		(len (length rx))
		(ti 0))
	    (let ((times (list ti)))
	      (do ((i 1 (+ i 1)))
		  ((= i len))
		(let ((x (rx i))
		      (y (ry i))
		      (z (rz i))
		      (v (rv i)))
		  (set! xseg (append xseg (list x)))
		  (set! yseg (append yseg (list y)))
		  (set! zseg (append zseg (list z)))
		  (set! vseg (append vseg (list v)))
		  
		  (when (number? v)
		    (let ((sofar 0.0)
			  (dseg ())
			  (len (- (length xseg) 1)))
		      (do ((i 0 (+ i 1)))
			  ((= i len))
			(let ((xsi (xseg i))
			      (ysi (yseg i))
			      (zsi (zseg i))
			      (xsf (xseg (+ i 1)))
			      (ysf (yseg (+ i 1)))
			      (zsf (zseg (+ i 1))))
			  (set! sofar (+ sofar (distance (- xsf xsi) (- ysf ysi) (- zsf zsi))))
			  (set! dseg (cons sofar dseg))))
		      (let ((df (car dseg)))
			(set! dseg (reverse dseg))
			(let ((tseg ()))
			  (let ((a (/ (* (- v vi) (+ v vi)) df 4)))
			    (for-each
			     (lambda (d)
			       (let ((seg (+ ti (if (= v vi)
						    (/ d vi)
						    (/ (- (sqrt (+ (* vi vi) (* 4 a d))) vi) (* 2 a))))))
				 (set! tseg (cons seg tseg))))
			     dseg))
			  (set! ti (car tseg))
			  (set! times (append times (reverse tseg))))))
		    (set! xseg (list x))
		    (set! yseg (list y))
		    (set! zseg (list z))
		    (set! vseg (list v))
		    (set! vi v))))
	      
	      (set! (path-rt path) (let ((val ())
					 (tf (times (- (length times) 1))))
				     (for-each
				      (lambda (ti)
					(set! val (cons (/ ti tf) val)))
				      times)
				     (reverse val)))
	      (reset-transformation path)))))))

(define (spiral-render path)
  ;; Render a spiral path from the object data
  
  (let* ((start (/ (* (spiral-start-angle path) 2 pi) dlocsig-one-turn))
	 (total (if (spiral-total-angle path)
		    (/ (* (spiral-total-angle path) 2 pi) dlocsig-one-turn)
		    (if (spiral-turns path)
			(* (spiral-turns path) 2 pi)
			(error 'mus-error "a spiral-path needs either a total-angle or turns, none specified~%"))))
	 (step (let ((steps (abs (/ (* total dlocsig-one-turn) (* (spiral-step-angle path) 2 pi)))))
		 (/ total (ceiling steps)
		  (if (< (spiral-step-angle path) 0) -1 1))))
	 (xdistance (x-norm (spiral-distance path) total))
	 (height (x-norm (spiral-height path) total)))
    (let ((x ())
	  (y ())
	  (z ())
	  (len (+ 1 (round (abs (/ total step))))))
      (do ((i 0 (+ i 1))
	   (angle start (+ angle step)))
	  ((>= i len))
	(let ((xy (cis angle))
	      (d (envelope-interp angle xdistance)))
	  (set! x (cons (* d (imag-part xy)) x))
	  (set! y (cons (* d (real-part xy)) y))
	  (set! z (cons (envelope-interp angle height) z))))
      
      (set! x (reverse x))
      (set! y (reverse y))
      (set! z (reverse z))
      
      (let ((dp ())
	    (len (- (length x) 1))
	    (sofar 0.0))
	(do ((i 0 (+ i 1)))
	    ((>= i len))
	  (let ((xi (x i))
		(xf (x (+ i 1)))
		(yi (y i))
		(yf (y (+ i 1)))
		(zi (z i))
		(zf (z (+ i 1))))
	    (set! sofar (+ sofar (distance (- xf xi) (- yf yi) (- zf zi))))
	    (set! dp (cons sofar dp))))

	(set! dp (reverse dp))
	(let ((tp ())
	      (td 0)
	      (len (- (length dp) 1)))
	  (do ((i 0 (+ i 1)))
	      ((>= i len))
	    (let* ((di (dp i))
		   (df (dp (+ i 1)))
		   (vp (x-norm (spiral-velocity path) df)))
	      (let ((vi (envelope-interp di vp))
		    (vf (envelope-interp df vp)))
		(set! tp (cons td tp))
		(set! td (+ td (/ (- df di) (+ vi vf) 2))))))
	  (let ((tf (car tp)))
	    (set! tp (reverse tp))
	    (set! (path-rx path) x)
	    (set! (path-ry path) y)
	    (set! (path-rz path) z)
	    (let ((val ()))
	      (for-each
	       (lambda (ti)
		 (set! val (cons (/ ti tf) val)))
	       tp)
	      (set! (path-rt path) (reverse val)))))))
    
    (reset-transformation path)))


(define (render-path path)
  ((case (car path) 
    ((bezier-path open-bezier-path) bezier-render)
    ((literal-path)   	            literal-render)
    (else                           spiral-render))
   path))




;;;;;;;;;;;;;;;;;;;
;;; Transformations
;;;;;;;;;;;;;;;;;;;

;;; Transform a rendered path using scaling, translation and rotation 

;;; Transform a path (scaling + translation + rotation)

(define* (transform-path path
			 scaling
			 translation
			 rotation
			 rotation-center
			 (rotation-axis '(0.0 0.0 1.0)))
  
  ;; Derive a rotation matrix from an axis vector and an angle
  
  (define rotation-matrix 
    ;; translated from C routine by David Eberly
    ;; (http://www.magic-software.com/)

    (let ((normalize (lambda (a b c)
		       (let ((mag (distance a b c)))
			 (list (/ a mag) (/ b mag) (/ c mag))))))
    
      (lambda (x y z angle)
	(let ((rotate (vector (vector 0.0 0.0 0.0) (vector 0.0 0.0 0.0) (vector 0.0 0.0 0.0)))
	      (I (vector (vector 1.0 0.0 0.0) (vector 0.0 1.0 0.0) (vector 0.0 0.0 1.0)))
	      (A (let ((vals (normalize x y z)))
		   (let ((dx (car vals))
			 (dy (cadr vals))
			 (dz (caddr vals)))
		     (vector (vector 0.0 dz (- dy)) (vector (- dz) 0.0 dx) (vector dy (- dx) 0.0)))))
	      (AA (vector (vector 0.0 0.0 0.0) (vector 0.0 0.0 0.0) (vector 0.0 0.0 0.0)))
	      (sn (sin (- angle)))
	      (omcs (- 1 (cos angle)))) ; (cos (- angle)) == (cos angle)
	  
	  (do ((row 0 (+ 1 row)))
	      ((= row 3))
	    (do ((col 0 (+ 1 col)))
		((= col 3))
	      (set! (AA row col) 0.0)
	      (do ((mid 0 (+ 1 mid)))
		  ((= mid 3))
		(set! (AA row col)
		      (+ (AA row col)
			 (* (A row mid) 
			    (A mid col)))))))
	  
	  ;; rotation matrix is I+sin(angle)*A+[1-cos(angle)]*A*A 
	  (do ((row 0 (+ 1 row)))
	      ((= row 3))
	    (do ((col 0 (+ 1 col)))
		((= col 3))
	      (set! (rotate row col)
		    (+ (I row col)
		       (* sn (A row col))
		       (* omcs (AA row col))))))
	  rotate))))
  
  (if (not-rendered path)
      (render-path path))
  (if (or scaling translation rotation)
      ;; there's at least one transformation to execute
      (let* ((rotation (and (number? rotation)
			    (/ (* 2 pi rotation) dlocsig-one-turn)))
	     (matrix (and rotation (rotation-matrix (car rotation-axis)
						   (cadr rotation-axis)
						   (third rotation-axis)
						   rotation)))
	     (xc (path-x path))
	     (yc (path-y path))
	     (zc (path-z path)))
	(if (and rotation-center (not (= (length rotation-center) 3)))
	    (error 'mus-error "rotation center has to have all three coordinates~%"))
	(if (and rotation-axis (not (= (length rotation-axis) 3)))
	    (error 'mus-error "rotation axis has to have all three coordinates~%"))
	(do ((xtr ())
	      (ytr ())
	      (ztr ())
	      (len (length xc))
	      (i 0 (+ i 1)))
	    ((= i len)
	     (set! (path-tx path) (reverse xtr))
	     (set! (path-ty path) (reverse ytr))
	     (set! (path-tz path) (reverse ztr)))

	  (let ((x (xc i))
		(y (yc i))
		(z (zc i)))
	    (let ((xw x)
		  (yw y)
		  (zw z))
	      (when rotation
		;; rotating around non-triple zero? translate first
		(when rotation-center
		  (set! xw (- xw (car rotation-center)))
		  (set! yw (- yw (cadr rotation-center)))
		  (set! zw (- zw (third rotation-center))))
		;; rotation
		(let ((xr (+ (* (matrix 0 0) xw)
			     (* (matrix 1 0) yw)
			     (* (matrix 2 0) zw)))
		      (yr (+ (* (matrix 0 1) xw)
			     (* (matrix 1 1) yw)
			     (* (matrix 2 1) zw)))
		      (zr (+ (* (matrix 0 2) xw)
			     (* (matrix 1 2) yw)
			     (* (matrix 2 2) zw))))
		  (set! xw xr)
		  (set! yw yr)
		  (set! zw zr))
		;; rotating around non-triple zero? untranslate
		(when rotation-center
		  (set! xw (+ xw (car rotation-center)))
		  (set! yw (+ yw (cadr rotation-center)))
		  (set! zw (+ zw (third rotation-center)))))
	      
	      ;; scaling
	      (when scaling
		(set! xw (* xw (car scaling)))
		(if (cadr scaling)
		    (set! yw (* yw (cadr scaling))))
		(if (third scaling)
		    (set! zw (* zw (third scaling)))))
	      
	      ;; translating
	      (when translation
		(set! xw (+ xw (car translation)))
		(if (cadr translation)
		    (set! yw (+ yw (cadr translation))))
		(if (third translation)
		    (set! zw (+ zw (third translation)))))
	      
	      ;; collect the points
	      (set! xtr (cons xw xtr))
	      (set! ytr (cons yw ytr))
	      (set! ztr (cons zw ztr))))))
      
      (begin
	;; if there's no transformation just copy the rendered path
	(set! (path-tt path) (copy (path-rt path)))
	(set! (path-tx path) (copy (path-rx path)))
	(set! (path-ty path) (copy (path-ry path)))
	(set! (path-tz path) (copy (path-rz path)))))
  path)

;;; Scale a path

(define (scale-path path scaling)
  (transform-path path :scaling scaling))

;;; Translate a path

(define (translate-path path translation)
  (transform-path path :translation translation))

;;; Rotate a path

(define rotate-path 
  (let ((documentation "rotate-path is a dlocsig function that rotates a dlocsig path"))
    (lambda* (path rotation rotation-center (rotation-axis '(0.0 0.0 1.0)))
      (transform-path path 
		      :rotation rotation 
		      :rotation-center rotation-center
		      :rotation-axis rotation-axis))))

;;; Mirror a path around an axis

(define* (mirror-path path (axis 'y) (around 0))
  (if (not-transformed path)
      (transform-path path))
  (let ((val ()))
    (if (eq? axis 'y)
	(begin
	  (for-each
	   (lambda (x)
	     (set! val (cons (- around x) val)))
	   (path-tx path))
	  (set! (path-tx path) (reverse val)))
	(begin
	  (for-each
	   (lambda (y)
	     (set! val (cons (- around y) val)))
	   (path-ty path))
	  (set! (path-ty path) (reverse val)))))
  path)

;;; Change the times of the rendered envelope so that the velocity is constant

(define constant-velocity 
  (let ((documentation "constant-velocity is a dlocsig function that changes the times of the rendered envelope so that the velocity is constant"))
    (lambda (path)
      (if (not (path-rx path))
	  (render-path path))
      (reset-transformation path)
      (let* ((xcoords (path-x path))
	     (ycoords (path-y path))
	     (zcoords (path-z path))
	     (tcoords (path-time path))
	     (total-distance 
	      (do ((sum 0.0)
		   (len (length xcoords))
		   (i 0 (+ i 1)))
		  ((= i len) sum)
		(let ((x1 (xcoords i))
		      (x2 (xcoords (+ i 1)))
		      (y1 (ycoords i))
		      (y2 (ycoords (+ i 1)))
		      (z1 (zcoords i))
		      (z2 (zcoords (+ i 1))))
		  (set! sum (+ sum (distance (- x2 x1) (- y2 y1) (- z2 z1)))))))
	     (start-time (car tcoords))
	     (velocity (/ total-distance (- (tcoords (- (length tcoords) 1)) start-time)))
	     (now ()))
	(do ((dist 0.0)
	     (len (length xcoords))
	     (i 0 (+ i 1)))
	    ((= i len))
	  (let ((xp (xcoords i))
		(x (xcoords (+ i 1)))
		(yp (ycoords i))
		(y (ycoords (+ i 1)))
		(zp (zcoords i))
		(z (zcoords (+ i 1))))
	    (set! dist (+ dist (distance (- x xp) (- y yp) (- z zp))))
	    (set! now (cons (/ dist velocity) now))))
	(set! now (reverse now))
	(set! (path-rt path) (cons start-time now))
	(set! (path-tx path) (copy (path-rx path)))
	(set! (path-ty path) (copy (path-ry path)))
	(set! (path-tz path) (copy (path-rz path))))
      path)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Create a new dlocsig structure



(define* (make-dlocsig start-time
		       duration
		       (path dlocsig-path)
		       (scaler dlocsig-scaler)
		       (direct-power dlocsig-direct-power)
		       (inside-direct-power dlocsig-inside-direct-power)
		       (reverb-power dlocsig-reverb-power)
		       (inside-reverb-power dlocsig-inside-reverb-power)
		       (reverb-amount dlocsig-reverb-amount)
		       (initial-delay dlocsig-initial-delay)
		       (unity-gain-dist dlocsig-unity-gain-distance)
		       (inside-radius dlocsig-inside-radius)
		       (minimum-segment-length dlocsig-minimum-segment-length)
		       (render-using dlocsig-render-using)
		       (ambisonics-h-order dlocsig-ambisonics-h-order)
		       (ambisonics-v-order dlocsig-ambisonics-v-order)
		       out-channels
		       rev-channels)
  
  (if (null? start-time)
      (error 'mus-error "a start time is required in make-dlocsig~%"))
  (if (null? duration)
      (error 'mus-error "a duration has to be specified in make-dlocsig~%"))
  
  ;; check to see if we have the right number of channels for b-format ambisonics
  (if (= render-using ambisonics)
      (begin
	(if (or (> ambisonics-h-order 3)
		(> ambisonics-v-order 3))
	    (error 'mus-error "ambisonics encoding is currently limited to third order components~%"))
	(let ((channels (ambisonics-channels ambisonics-h-order ambisonics-v-order)))
	  (if (< (or out-channels (mus-channels *output*)) channels)
	      (error 'mus-error "ambisonics number of channels is wrong, dlocsig needs ~A output channels for h:~A, v:~A order (current number is ~A)~%"
		     channels ambisonics-h-order ambisonics-v-order (or out-channels (mus-channels *output*)))))))
  
  (if (not out-channels)
      (if *output*
	  (set! out-channels (channels *output*))
	  (begin
	    (format () "warning: no *output*?  Will set out-channels to 2~%")
	    (set! out-channels 2))))
  (if (not rev-channels)
      (set! rev-channels (if *reverb* (channels *reverb*) 0)))
  
  (let (;; speaker configuration for current number of channels
	(speakers (and (not (= render-using ambisonics))
		       (get-speaker-configuration out-channels)))
	;; coordinates of rendered path
	(xpoints (path-x path))
	(ypoints (path-y path))
	(zpoints (path-z path))
	(tpoints (path-time path))
	
	;; array of gains -- envelopes
	(channel-gains (make-vector out-channels ()))
	(channel-rev-gains (make-vector out-channels ()))
	
	;; speaker output delays
	(max-out-delay 0.0)
	(out-delays (make-vector out-channels))
	
	;; speed of sound expressed in terms of path time coordinates
	(dly ())
	(doppler ())
	(prev-time #f)
	(prev-dist #f)
	(first-dist #f)
	(last-dist #f)
	(min-dist #f)
	(max-dist #f)
	(delay-hack 1)
	;; channel offsets in output stream for ambisonics
	;; (depends on horizontal and vertical order, default is h=1,v=1)
	(z-offset #f)
	(r-offset #f)
	(s-offset #f)
	(t-offset #f)
	(u-offset #f)
	(v-offset #f)
	(k-offset #f)
	(l-offset #f)
	(m-offset #f)
	(n-offset #f)
	(o-offset #f)
	(p-offset #f)
	(q-offset #f))
    
    (when (= render-using ambisonics)
      ;; calculate output channel offsets for ambisonics rendering
      (let ((offset 3))
	;; the default is at least a horizontal order of 1
	(if (>= ambisonics-v-order 1)
	    (begin
	      ;; add Z
	      (set! z-offset offset)
	      (set! offset (+ offset 1))))
	(if (>= ambisonics-v-order 2)
	    (begin
	      ;; add R S T
	      (set! r-offset offset)
	      (set! s-offset (+ offset 1))
	      (set! t-offset (+ offset 2))
	      (set! offset (+ offset 3))))
	(if (>= ambisonics-h-order 2)
	    (begin
	      ;; add U V
	      (set! u-offset offset)
	      (set! v-offset (+ offset 1))
	      (set! offset (+ offset 2))))
	(if (>= ambisonics-v-order 3)
	    (begin
	      ;; add K L M N O 
	      (set! k-offset offset)
	      (set! l-offset (+ offset 1))
	      (set! m-offset (+ offset 2))
	      (set! n-offset (+ offset 3))
	      (set! o-offset (+ offset 4))
	      (set! offset (+ offset 5))))
	(if (>= ambisonics-h-order 3)
	    (begin
	      ;; add P Q
	      (set! p-offset offset)
	      (set! q-offset (+ offset 1)))
	    (set! offset (+ offset 2)))))
    
    (define (dist->samples d) (round (* d (/ *clm-srate* dlocsig-speed-of-sound))))
    ;; (define (dist->seconds d) (/ d dlocsig-speed-of-sound))
    (define (time->samples time) (round (* time *clm-srate*)))
    
    ;; calculate speaker gains for group
    (define (find-gains x y z group)
      (let ((zero-coord 1.0e-10)
	    (zero-gain 1.0e-10)
	    (size (group-size group))
	    (mat (group-matrix group))) ; returns float-vector
	(if (and (< (abs x) zero-coord)
		 (< (abs y) zero-coord)
		 (< (abs z) zero-coord))
	    (list #t (list 1.0 1.0 1.0))
	    (case size
	      ((3)
	       (let* ((gain-a (+ (* (mat 0 0) x)
				 (* (mat 1 0) y)
				 (* (mat 2 0) z)))
		      (gain-b (+ (* (mat 0 1) x)
				 (* (mat 1 1) y)
				 (* (mat 2 1) z)))
		      (gain-c (+ (* (mat 0 2) x)
				 (* (mat 1 2) y)
				 (* (mat 2 2) z)))
		      (mag (distance gain-a gain-b gain-c)))
		 ;; truncate to zero roundoff errors
		 (if (< (abs gain-a) zero-gain)
		     (set! gain-a 0.0))
		 (if (< (abs gain-b) zero-gain)
		     (set! gain-b 0.0))
		 (if (< (abs gain-c) zero-gain)
		     (set! gain-c 0.0))
		 (list (and (>= gain-a 0) (>= gain-b 0) (>= gain-c 0))
		       (list (/ gain-a mag) (/ gain-b mag) (/ gain-c mag)))))
	      
	      ((2)
	       (let* ((gain-a (+ (* (mat 0 0) x)
				 (* (mat 1 0) y)))
		      (gain-b (+ (* (mat 0 1) x)
				 (* (mat 1 1) y)))
		      (mag (sqrt (+ (* gain-a gain-a)
				    (* gain-b gain-b)))))
		 ;; truncate to zero roundoff errors
		 (if (< (abs gain-a) zero-gain)
		     (set! gain-a 0.0))
		 (if (< (abs gain-b) zero-gain)
		     (set! gain-b 0.0))
		 (list (and (>= gain-a 0) (>= gain-b 0))
		       (list (/ gain-a mag) (/ gain-b mag)))))
	      
	      ((1)
	       (list #t (list 1.0)))))))
    

    ;; Render a trajectory breakpoint through amplitude panning
    (define famplitude-panning 
      (let ((speed-limit (/ (* dlocsig-speed-of-sound (- (car (last tpoints)) (car tpoints))) duration))
	    (prev-group #f)
	    (prev-x #f)
	    (prev-y #f)
	    (prev-z #f))
	
	(define (equalp-intersection L1 L2)
	  (if (null? L2) 
	      L2
	      (let loop1 ((L1 L1) 
			  (result ()))
		(cond ((null? L1) 
		       (reverse! result))
		      ((member (car L1) L2) 
		       (loop1 (cdr L1) 
			      (cons (car L1) 
				    result)))
		      (else (loop1 (cdr L1) 
				   result))))))
	
	(define (transition-point-3 vert-a vert-b xa ya za xb yb zb) 
	  (define (cross v1 v2)
	    (list (- (* (cadr v1) (third v2))
		     (* (third v1) (cadr v2)))
		  (- (* (third v1) (car v2))
		     (* (car v1) (third v2)))
		  (- (* (car v1) (cadr v2))
		     (* (cadr v1) (car v2)))))
	  (define (dot v1 v2)
	    (+ (* (car v1) (car v2))
	       (* (cadr v1) (cadr v2))
	       (* (third v1) (third v2))))
	  (define (sub v1 v2)
	    (list (- (car v1) (car v2))
		  (- (cadr v1) (cadr v2))
		  (- (third v1) (third v2))))
	  (define (add v1 v2)
	    (list (+ (car v1) (car v2))
		  (+ (cadr v1) (cadr v2))
		  (+ (third v1) (third v2))))
	  (define (scale v1 c)
	    (list (* (car v1) c)
		  (* (cadr v1) c)
		  (* (third v1) c)))
	  
	  (let* ((tolerance 1.0e-6)
		 (line-b (list xa ya za))
		 (line-m (sub (list xb yb zb) line-b))
		 (normal (cross vert-a vert-b))
		 (denominator (dot normal line-m)))
	    (and (> (abs denominator) tolerance)
		 (add line-b (scale line-m (/ (- (dot normal line-b)) denominator))))))
	
	;; calculate transition point between two adjacent two-speaker groups
	;; original line intersection code from Graphic Gems III
	(define (transition-point-2 vert xa ya xb yb)
	  (let ((Ax (car vert))
		(Bx (- xa xb))
		(Ay (cadr vert))
		(By (- ya yb)))
	    (let ((d (- (* By (- xa)) (* Bx (- ya))))
		  (f (- (* Ay Bx) (* Ax By))))
	      (and (not (= f 0))
		   (list (/ (* d Ax) f)
			 (/ (* d Ay) f))))))
	
	;; find the speaker group that contains a point
	(define (find-group x y z)
	  (call-with-exit
	   (lambda (return)
	     (for-each
	      (lambda (group)
		(let ((vals (find-gains x y z group)))
		  (if (car vals)                           ; inside
		      (return (list group (cadr vals)))))) ; gains
	      (speaker-config-groups speakers))
	     (list #f #f))))
	
	;; push zero gains on all channels
	(define (push-zero-gains time)
	  (do ((len (speaker-config-number speakers))
	       (i 0 (+ i 1)))
	      ((= i len))
	    (set! (channel-gains i) (cons time (channel-gains i)))
	    (set! (channel-gains i) (cons 0.0 (channel-gains i))))
	  (do ((len rev-channels)
	       (i 0 (+ i 1)))
	      ((= i len))
	    (set! (channel-rev-gains i) (cons time (channel-rev-gains i)))
	    (set! (channel-rev-gains i) (cons 0.0 (channel-rev-gains i)))))
	
	;; push gain and time into envelopes
	(define (push-gains group gains dist time)
	  
	  (define* (position val lst (pos 0))
	    (call-with-exit
	     (lambda (return)
	       (and (not (null? lst))
		    (if (= val (car lst))
			(return pos)
			(position val (cdr lst) (+ 1 pos)))))))
	  
	  (let ((outputs (make-vector out-channels 0.0))
		(rev-outputs (make-vector rev-channels 0.0))
		;; attenuation with distance of reverberated signal
		(ratt (if (>= dist inside-radius)
			  (expt dist (- reverb-power))
			  (- 1.0 (expt (/ dist inside-radius) (/ inside-reverb-power))))))
	    (let (;; attenuation with distance of direct signal
		  (att (if (>= dist inside-radius)
			   (expt dist (- direct-power))
			   (- 1.0 (expt (/ dist inside-radius) (/ inside-direct-power))))))
	      (if (>= dist inside-radius)
		  ;; outside the inner sphere, signal is sent to group
		  (do ((len (length gains))
		       (i 0 (+ i 1)))
		      ((= i len))
		    (let ((speaker ((group-speakers group) i))
			  (gain (gains i)))
		      (set! (outputs speaker) (* gain att))
		      (if (> rev-channels (max 1 speaker))
			  (set! (rev-outputs speaker) (* gain ratt)))))
		  
		  (let ((gain 0.0)
			(len (speaker-config-number speakers)))
		    (do ((speaker 0 (+ 1 speaker)))
			((= speaker len))
		      ;; inside the inner sphere, signal is sent to all speakers
		      (let ((found (position speaker (group-speakers group))))
			(if found
			    ;; speaker belongs to group, add to existing gain
			    (begin
			      (set! gain (gains found))
			      (set! (outputs speaker) (+ gain (* (- 1.0 gain) att)))
			      (if (> rev-channels (max 1 speaker))
				  (set! (rev-outputs speaker) (+ gain (* (- 1.0 gain) ratt)))))
			    ;; speaker outside of group
			    (begin
			      (set! (outputs speaker) att)
			      (if (> rev-channels (max 1 speaker))
				  (set! (rev-outputs speaker) ratt)))))))))
	    
	    ;; push all channel gains into envelopes
	    (let ((len (speaker-config-number speakers)))
	      (do ((i 0 (+ i 1)))
		  ((= i len))
		(if (or (null? (channel-gains i))
			(> time (cadr (channel-gains i))))
		    (begin
		      (set! (channel-gains i) (cons time (channel-gains i)))
		      (set! (channel-gains i) (cons (outputs i) (channel-gains i)))))))
	    
	    (if (> rev-channels 1)
		(do ((i 0 (+ i 1)))
		    ((= i rev-channels))
		  (if (or (null? (channel-rev-gains i))
			  (> time (cadr (channel-rev-gains i))))
		      (begin
			(set! (channel-rev-gains i) (cons time (channel-rev-gains i)))
			(set! (channel-rev-gains i) (cons (rev-outputs i) (channel-rev-gains i)))))))
	    
	    ;; push reverb gain into envelope for mono reverb
	    (if (and (= rev-channels 1)
		     (or (null? (channel-rev-gains 0))
			 (> time (cadr (channel-rev-gains 0)))))
		(begin
		  (set! (channel-rev-gains 0) (cons time (channel-rev-gains 0)))
		  (set! (channel-rev-gains 0) (cons ratt (channel-rev-gains 0)))))))
	
	(lambda (x y z dist time)
	  
	  ;; output gains for current point
	  (if prev-group
	      (let ((vals (find-gains x y z prev-group)))
		(let ((inside (car vals))
		      (gains (cadr vals)))
		  ;; check that the source is not moving faster than sound
		  (if (not (= time prev-time))
		      (let ((speed (/ (- dist prev-dist) (- time prev-time))))
			(if (> speed speed-limit)
			    (format () "warning: supersonic radial movement at [~F,~F,~F, ~F], speed=~F~%" x y z time speed))))
		  (if inside
		      ;; still in the same group
		      (begin
			(push-gains prev-group gains dist time)
			(set! prev-x x)
			(set! prev-y y)
			(set! prev-z z))
		      ;; left the group
		      (let ((vals (find-group x y z)))
			(let ((group (car vals))
			      (gains (cadr vals)))
			  (if (not group)
			      (begin
				;; current point is outside all defined groups
				;; we should send a warning at this point...
				(push-zero-gains time)
				(set! prev-group #f))
			      (begin
				;; we have to interpolate a new point that lies on the shared
				;; edge of the adjacent groups so that the speakers opposite
				;; the edge have zero gain when the trajectory switches groups
				(let ((edge (equalp-intersection (group-vertices group)
								 (group-vertices prev-group))))
				  (cond ((= (length edge) 2)
					 ;; the groups have two shared points (ie: share an edge)
					 ;; this must be a three speaker groups transition
					 (let ((pint (transition-point-3 (car edge) (cadr edge) x y z prev-x prev-y prev-z)))
					   (when pint
					     (let* ((xi (car pint))
						    (yi (cadr pint))
						    (zi (third pint))
						    (vals (find-gains xi yi zi prev-group)))
					       (let ((di (distance xi yi zi))
						     (ti (+ prev-time (max .00001 (* (/ (distance (- xi prev-x)
												  (- yi prev-y)
												  (- zi prev-z))
											(distance (- x prev-x)
												  (- y prev-y)
												  (- z prev-z)))
										     (- time prev-time)))))
						     ;; see if we are inside the previous group
						     ;; we can be on either side due to roundoff errors
						     (inside (car vals))
						     (gains (cadr vals)))
						 (if inside
						     (push-gains prev-group gains di ti)
						     (let ((val1 (find-gains xi yi zi group)))
						       (let ((inside (car val1))
							     (gains (cadr val1)))
							 (if inside
							     (push-gains group gains di ti)
							     ;; how did we get here?
							     (error 'mus-error "Outside of both adjacent groups [~A:~A:~A @~A]~%~%" xi yi zi ti))))))))))
					
					((and (pair? edge) 
					      (null? (cdr edge))
					      (= (group-size group) 2))
					 ;; two two-speaker groups share one point
					 ;; z coordinates are silently ignored
					 (let ((pint (transition-point-2 (car edge) x y prev-x prev-y)))
					   (when pint
					     (let* ((xi (car pint))
						    (yi (cadr pint))
						    (vals (find-gains xi yi 0.0 prev-group)))
					       (let ((di (distance xi yi 0.0))
						     (ti (+ prev-time (max .00001 (* (/ (distance (- xi prev-x)
												  (- yi prev-y)
												  0.0)
											(distance (- x prev-x)
												  (- y prev-y)
												  0.0))
										     (- time prev-time)))))
						     ;; see if we are inside the previous group
						     ;; we can be on either side due to roundoff errors
						     (inside (car vals))
						     (gains (cadr vals)))
						 (if inside 
						     (push-gains prev-group gains di ti)
						     (let ((val1 (find-gains xi yi 0.0 group)))
						       (let ((inside (car val1))
							     (gains (cadr val1)))
							 (if inside
							     (push-gains group gains di ti)
							     ;; how did we get here?
							     (format () "Outside of both adjacent groups [~A:~A @~A]~%~%" xi yi ti))))))))))
					
					((and (pair? edge) 
					      (null? (cdr edge)))
					 ;; groups share only one point... for now a warning
					 ;; we should calculate two additional interpolated
					 ;; points as the trajectory must be crossing a third
					 ;; group
					 (for-each
					  (lambda (int-group)
					    (if (and (member (car edge) (group-vertices int-group))
						     (not (equal? int-group group))
						     (not (equal? int-group prev-group)))
						(format () "e1=~A; e2=~A~%~%"
							(equalp-intersection (group-vertices int-group)
									     (group-vertices prev-group))
							(equalp-intersection (group-vertices int-group)
									     (group-vertices group)))))
					  
					  (speaker-config-groups speakers))
					 (format () "warning: crossing between groups with only one point in common~%  prev=~A~%  curr=~A~%" prev-group group))
					
					;; groups don't share points... how did we get here?
					((null? edge)
					 (format () "warning: crossing between groups with no common points, ~A~A to ~A~A~%"
						 (group-id prev-group) (group-speakers prev-group)
						 (group-id group) (group-speakers group)))))
				
				;; finally push gains for current group
				(push-gains group gains dist time)
				(set! prev-group group)
				(set! prev-x x)
				(set! prev-y y)
				(set! prev-z z))))))))
	      ;; first time around
	      (let ((vals (find-group x y z)))
		(let ((group (car vals))
		      (gains (cadr vals)))
		  (if group
		      (begin
			(push-gains group gains dist time)
			(set! prev-group group)
			(set! prev-x x)
			(set! prev-y y)
			(set! prev-z z))
		      (begin
			(push-zero-gains time)
			(set! prev-group #f)))))))))
    
    ;; Render a trajectory breakpoint for ambisonics b-format coding
    ;; http://www.york.ac.uk/inst/mustech/3d_audio/ambis2.htm
    ;;
    ;; Ambisonics b-format has four discrete channels encoded as follows:
    ;; W     0.707107             0.707107
    ;; X     cos(A)cos(E)         x
    ;; Y     sin(A)cos(E)         y
    ;; R     1.5sin(E)sin(E)-0.5  1.5zz-0.5
    ;; S     cos(A)sin(2E)        2zx
    ;; T     sin(A)sin(2E)        2yz
    ;; U     cos(2A)cos(E)cos(E)  xx-yy
    ;; V     sin(2A)cos(E)cos(E)  2xy
    ;; K                          z(2.5zz-1.5)
    ;; L                          K1(x(5zz-1)
    ;;       K1=sqrt(21*45/(224*8))
    ;; M                          K1(y(5zz-1))
    ;; N                          K2(Uz)
    ;;       K2=sqrt(3)*3/2
    ;; O                          K2(Vz)
    ;; P                          x(xx-3yy)
    ;; Q                          y(3xx-yy)
    ;;
    ;; where:
    ;; A: counter-clockwise angle of rotation from the front center
    ;; E: the angle of elevation above the horizontal plane
    ;; 
    ;; in our coordinate system (normalizing the vectors):
    ;; xy: (* dist (cos E))
    ;; (cos A): (/ y xy)
    ;; (sin A): (/ -x xy)
    ;; (cos E): (/ xy dist)
    ;; (sin E): (/ z dist)
    ;; so:
    ;; W: (* signal 0.707)
    ;; X: (* signal (/ y dist))
    ;; Y: (* signal (/ -x dist))
    ;; Z: (* signal (/ z dist))
    ;;
    ;; R: (* signal (- (* 1.5 z z 1/dist 1/dist) 0.5))
    ;; S: (* signal 2 z (- x) 1/dist 1/dist)
    ;; T: (* signal 2 z y 1/dist 1/dist)
    ;; U: (* signal (- (* x x 1/dist 1/dist) (* y y 1/dist 1/dist)))
    ;; V: (* signal 2 (- x) y 1/dist 1/dist)
    ;;
    ;; K: (* signal z (- (* 2.5 z z 1/dist 1/dist) 1.5))
    ;; L: (* signal K1 x 1/dist (- (* 5 z z 1/dist 1/dist) 1))
    ;; M: (* signal K1 y 1/dist (- (* 5 z z 1/dist 1/dist) 1))
    ;; N: (* signal K2 U z 1/dist)
    ;; O: (* signal K2 V z 1/dist)
    ;; P: (* signal x 1/dist (- (* x x 1/dist 1/dist) (* 3 y y 1/dist 1/dist)))
    ;; Q: (* signal y 1/dist (- (* 3 x x 1/dist 1/dist) (* y y 1/dist 1/dist)))
    ;;
    ;; see also: http://wiki.xiph.org/index.php/Ambisonics
    ;; for mixed order systems
    ;;
    (define render-ambisonics
      (let ((w-offset 0)
	    (x-offset 1)
	    (y-offset 2)
	    (ambisonics-k1 (sqrt 135/256)) ;(/ (* 21 45) 224 8)
	    (ambisonics-k2 (* (sqrt 3) 3 0.5)))

	(lambda (x y z dist time)
	  (let ((att (if (> dist inside-radius)
			 (expt (/ inside-radius dist) direct-power)
			 (expt (/ dist inside-radius) (/ inside-direct-power))))
		(ratt (if (> dist inside-radius)
			  (expt (/ inside-radius dist) reverb-power)
			  (expt (/ dist inside-radius) (/ inside-reverb-power))))
		(u 0)
		(v 0)
		(lm 0)
		(no 0))
	    ;; output encoding gains for point
	    ;; W: 0.707
	    (set! (channel-gains w-offset) (cons time (channel-gains w-offset)))
	    (let ((attW (if (> dist inside-radius)
			    (* point707 att)
			    (- 1 (* (- 1 point707) (expt (/ dist inside-radius) direct-power))))))
	      (set! (channel-gains w-offset) (cons attW (channel-gains w-offset))))
	    ;; X: (* (cos A) (cos E))
	    (set! (channel-gains x-offset) (cons time (channel-gains x-offset)))
	    (set! (channel-gains x-offset) (cons (if (zero? dist) 0 (/ (* att y) dist)) (channel-gains x-offset)))
	    ;; Y: (* (sin A) (cos E))
	    (set! (channel-gains y-offset) (cons time (channel-gains y-offset)))
	    (set! (channel-gains y-offset) (cons (if (zero? dist) 0 (/ (* att (- x)) dist)) (channel-gains y-offset)))
	    (when (>= ambisonics-v-order 1)
	      ;; Z: (sin E)
	      (set! (channel-gains z-offset) (cons time (channel-gains z-offset)))
	      (set! (channel-gains z-offset) (cons (if (zero? dist) 0 (/ (* att z) dist)) (channel-gains z-offset))))
	    (when (>= ambisonics-v-order 2)
	      ;; R
	      (set! (channel-gains r-offset) (cons time (channel-gains r-offset)))
	      (set! (channel-gains r-offset) (cons (if (zero? dist) 0 (* (- (/ (* 1.5 z z) dist dist) 0.5) att))
						   (channel-gains r-offset)))
	      ;; S
	      (set! (channel-gains s-offset) (cons time (channel-gains s-offset)))
	      (set! (channel-gains s-offset) (cons (if (zero? dist) 0 (/ (* -2 z x att) dist dist))
						   (channel-gains s-offset)))
	      ;; T
	      (set! (channel-gains t-offset) (cons time (channel-gains t-offset)))
	      (set! (channel-gains t-offset) (cons (if (zero? dist) 0 (/ (* 2 z y att) dist dist))
						   (channel-gains t-offset))))
	    (when (>= ambisonics-h-order 2)
	      (set! u (if (zero? dist) 0 (* (/ (- (* x x) (* y y)) dist dist) att)))
	      (set! v (if (zero? dist) 0 (/ (* -2 x y att) dist dist)))
		    ;; U
	      (set! (channel-gains u-offset) (cons time (channel-gains u-offset)))
	      (set! (channel-gains u-offset) (cons u (channel-gains u-offset)))
	      ;; V
	      (set! (channel-gains v-offset) (cons time (channel-gains v-offset)))
	      (set! (channel-gains v-offset) (cons v (channel-gains v-offset))))
	    
	    (when (>= ambisonics-v-order 3)
	      (set! lm (* ambisonics-k1 att (- (* 5 z z (if (zero? dist) 1 (/ 1.0 dist dist))) 1)))
	      (set! no (* ambisonics-k2 z att (if (zero? dist) 1 (/ dist))))
	      ;; K
	      (set! (channel-gains k-offset) (cons time (channel-gains k-offset)))
	      (set! (channel-gains k-offset) (cons (if (zero? dist) 0
						       (* (- (* 2.5 z z (if (zero? dist) 1 (/ 1.0 dist dist))) 1.5) att))
						   (channel-gains k-offset)))
	      ;; L
	      (set! (channel-gains l-offset) (cons time (channel-gains l-offset)))
	      (set! (channel-gains l-offset) (cons (if (zero? dist) 0 (/ (* lm x) dist)) (channel-gains l-offset)))
	      ;; M
	      (set! (channel-gains m-offset) (cons time (channel-gains m-offset)))
	      (set! (channel-gains m-offset) (cons (if (zero? dist) 0 (/ (* lm y) dist)) (channel-gains m-offset)))
	      ;; N
	      (set! (channel-gains n-offset) (cons time (channel-gains n-offset)))
	      (set! (channel-gains n-offset) (cons (if (zero? dist) 0 (* no u)) (channel-gains n-offset)))
	      ;; O
	      (set! (channel-gains o-offset) (cons time (channel-gains o-offset)))
	      (set! (channel-gains o-offset) (cons (if (zero? dist) 0 (* no v)) (channel-gains o-offset))))
	    
	    (when (>= ambisonics-h-order 3)
	      ;; P
	      (set! (channel-gains p-offset) (cons time (channel-gains p-offset)))
	      (set! (channel-gains p-offset) (let ((dist-p (- (* x x (if (zero? dist) 1 (/ 1.0 dist dist)))
							      (* 3 y y (if (zero? dist) 1 (/ 1.0 dist dist))))))
					       (cons (if (zero? dist) 0 (/ (* att x dist-p) dist))
						     (channel-gains p-offset))))
	      ;; Q
	      (set! (channel-gains q-offset) (cons time (channel-gains q-offset)))
	      (set! (channel-gains q-offset) (let ((dist-q (- (* 3 x x (if (zero? dist) 1 (/ 1.0 dist dist)))
							      (* y y (if (zero? dist) 1 (/ 1.0 dist dist))))))
					       (cons (if (zero? dist) 0 (/ (* att y dist-q) dist))
						     (channel-gains q-offset)))))
	    ;; push reverb gain into envelope
	    (when (= rev-channels 1)
	      ;; mono reverb output
	      (set! (channel-rev-gains 0) (cons time (channel-rev-gains 0)))
	      (set! (channel-rev-gains 0) (cons (if (>= dist inside-radius)
						    (expt dist (- reverb-power))
						    (- 1.0 (expt (/ dist inside-radius) (/ inside-reverb-power))))
						(channel-rev-gains 0))))
	    (when (> rev-channels 1)
	      ;; multichannel reverb, send ambisonics components
	      ;; W: 0.707
	      (set! (channel-rev-gains w-offset) (cons time (channel-rev-gains w-offset)))
	      (let ((rattW (if (> dist inside-radius)
			       (* point707 ratt)
			       (- 1 (* (- 1 point707) (expt (/ dist inside-radius) reverb-power))))))
		(set! (channel-rev-gains w-offset) (cons rattW (channel-rev-gains w-offset))))
	      ;; X: (* (cos A)(cos E))
	      (set! (channel-rev-gains x-offset) (cons time (channel-rev-gains x-offset)))
	      (set! (channel-rev-gains x-offset) (cons (if (zero? dist) 0 (* y (if (zero? dist) 1 (/ dist)) ratt))
						       (channel-rev-gains x-offset)))
	      ;; Y: (* (sin A)(cos E))
	      (set! (channel-rev-gains y-offset) (cons time (channel-rev-gains y-offset)))
	      (set! (channel-rev-gains y-offset) (cons (if (zero? dist) 0 (* (- x) (if (zero? dist) 1 (/ dist)) ratt))
						       (channel-rev-gains y-offset)))
	      (when (>= ambisonics-v-order 1)
		;; Z: (sin E)
		(set! (channel-rev-gains z-offset) (cons time (channel-rev-gains z-offset)))
		(set! (channel-rev-gains z-offset) (cons (if (zero? dist) 0 (* z (if (zero? dist) 1 (/ dist)) ratt))
							 (channel-rev-gains z-offset))))
	      (when (>= ambisonics-v-order 2)
		;; R
		(set! (channel-rev-gains r-offset) (cons time (channel-rev-gains r-offset)))
		(set! (channel-rev-gains r-offset) (cons (if (zero? dist) 0 (* (- (/ (* 1.5 z z) dist dist) 0.5)
									       dlocsig-ambisonics-ho-rev-scaler ratt))
							 (channel-rev-gains r-offset)))
		;; S
		(set! (channel-rev-gains s-offset) (cons time (channel-rev-gains s-offset)))
		(set! (channel-rev-gains s-offset) (cons (if (zero? dist) 0 (/ (* -2 z x dlocsig-ambisonics-ho-rev-scaler ratt)
									       dist dist))
							 (channel-rev-gains s-offset)))
		;; T
		(set! (channel-rev-gains t-offset) (cons time (channel-rev-gains t-offset)))
		(set! (channel-rev-gains t-offset) (cons (if (zero? dist) 0 (/ (* 2 z y dlocsig-ambisonics-ho-rev-scaler ratt)
									       dist dist))
							 (channel-rev-gains t-offset))))
	      (when (>= ambisonics-h-order 2)
		;; U
		(set! (channel-rev-gains u-offset) (cons time (channel-rev-gains u-offset)))
		(set! (channel-rev-gains u-offset) (let ((dist-u (if (zero? dist) 0 (/ (* (- (* x x) (* y y)) dlocsig-ambisonics-ho-rev-scaler ratt)
										       dist dist))))
						     (cons dist-u (channel-rev-gains u-offset))))
		;; V
		(set! (channel-rev-gains v-offset) (cons time (channel-rev-gains v-offset)))
		(set! (channel-rev-gains v-offset) (let ((dist-v (if (zero? dist) 0 (/ (* -2 x y dlocsig-ambisonics-ho-rev-scaler ratt)
										       dist dist))))
						     (cons dist-v (channel-rev-gains v-offset)))))
	      
	      (when (>= ambisonics-v-order 3)
		(set! lm (* ambisonics-k1 (- (* 5 z z (if (zero? dist) 1 (/ 1.0 dist dist))) 1) 
			    dlocsig-ambisonics-ho-rev-scaler ratt))
		(set! no (* ambisonics-k2 z (if (zero? dist) 1 (/ dist)) ratt))
		;; K
		(set! (channel-rev-gains k-offset) (cons time (channel-rev-gains k-offset)))
		(set! (channel-rev-gains k-offset) (let ((dist-k (if (zero? dist) 0 (* (- (/ (* 2.5 z z) dist dist) 1.5) dlocsig-ambisonics-ho-rev-scaler ratt))))
						     (cons dist-k (channel-rev-gains k-offset))))
		;; L
		(set! (channel-rev-gains l-offset) (cons time (channel-rev-gains l-offset)))
		(set! (channel-rev-gains l-offset) (cons (if (zero? dist) 0 (/ (* lm x) dist))
							 (channel-rev-gains l-offset)))
		;; M
		(set! (channel-rev-gains m-offset) (cons time (channel-rev-gains m-offset)))
		(set! (channel-rev-gains m-offset) (cons (if (zero? dist) 0 (/ (* lm y) dist))
							 (channel-rev-gains m-offset)))
		;; N
		(set! (channel-rev-gains n-offset) (cons time (channel-rev-gains n-offset)))
		(set! (channel-rev-gains n-offset) (cons (if (zero? dist) 0 (* no u))
							 (channel-rev-gains n-offset)))
		;; O
		(set! (channel-rev-gains o-offset) (cons time (channel-rev-gains o-offset)))
		(set! (channel-rev-gains o-offset) (cons (if (zero? dist) 0 (* no v))
							 (channel-rev-gains o-offset))))
	      (when (>= ambisonics-h-order 3)
		;; P
		(set! (channel-rev-gains p-offset) (cons time (channel-rev-gains p-offset)))
		(set! (channel-rev-gains p-offset) (let ((dist-p (if (zero? dist) 
								     0 
								     (/ (* ratt dlocsig-ambisonics-ho-rev-scaler x (- (* x x) (* 3 y y)))
									dist dist dist))))
						     (cons dist-p (channel-rev-gains p-offset))))
		;; Q
		(set! (channel-rev-gains q-offset) (cons time (channel-rev-gains q-offset)))
		(set! (channel-rev-gains q-offset) (let ((dist-q (if (zero? dist) 
								     0 
								     (/ (* ratt dlocsig-ambisonics-ho-rev-scaler y (- (* 3 x x) (* y y))) 
									dist dist dist))))
						     (cons dist-q (channel-rev-gains q-offset)))))
	      )))))
    
    ;; Render a trajectory breakpoint to a room for decoded ambisonics
    ;;
    ;; for a given speaker located in 3d space in polar coordinates:
    ;; az: azimut angle, increments clockwise
    ;; el: elevation angle
    ;;
    ;; S: (+ W (* X (cos az) (cos el))
    ;;         (* Y (sin az) (cos el))
    ;;         (* Z (sin el)))
    ;; 
    (define (fdecoded-ambisonics x y z dist time)
      (let ((att (if (> dist inside-radius)
		      (expt (/ inside-radius dist) direct-power)
		      (expt (/ dist inside-radius) (/ inside-direct-power))))
	     (ratt (if (> dist inside-radius)
		       (expt (/ inside-radius dist) reverb-power)
		       (expt (/ dist inside-radius) (/ inside-reverb-power)))))
	(let ((attW (if (> dist inside-radius)
			(* point707 att)
			(- 1 (* (- 1 point707) (expt (/ dist inside-radius) direct-power)))))
	      (rattW (if (> dist inside-radius)
			 (* point707 ratt)
			 (- 1 (* (- 1 point707) (expt (/ dist inside-radius) reverb-power))))))
	  ;; output decoded gains for point
	  (let ((len (speaker-config-number speakers))
		(spkrs (speaker-config-coords speakers)))
	    (do ((i 0 (+ i 1)))
		((= i len))
	      (let ((signal (let ((s (spkrs i)))
			      (* dlocsig-ambisonics-scaler
				 (if (zero? dist)
				     (* attW point707)
				     (+ (* attW point707)
					(/ (* att (+ (* x (car s))
						     (* y (cadr s))
						     (* z (caddr s))))
					   dist)))))))
		(set! (channel-gains i) (cons time (channel-gains i)))
		(set! (channel-gains i) (cons signal (channel-gains i))))))
	  
	  ;; push reverb gain into envelope
	  (if (= rev-channels 1)
	      (begin
		;; mono reverberation
		(set! (channel-rev-gains 0) (cons time (channel-rev-gains 0)))
		(set! (channel-rev-gains 0) (cons (if (>= dist inside-radius)
						      (expt dist (- reverb-power))
						      (- 1.0 (expt (/ dist inside-radius) (/ inside-reverb-power))))
						  (channel-rev-gains 0))))
	      ;; multichannel reverb
	      (do ((i 0 (+ i 1)))
		  ((= i rev-channels))
		(let ((signal (let ((s ((speaker-config-coords speakers) i)))
				(* dlocsig-ambisonics-scaler
				   (if (zero? dist)
				       (* rattW point707)
				       (+ (* rattW point707)
					  (/ (* ratt (+ (* x (car s)) 
							(* y (cadr s))
							(* z (caddr s))))
					     dist)))))))
		  (set! (channel-rev-gains i) (cons time (channel-rev-gains i)))
		  (set! (channel-rev-gains i) (cons signal (channel-rev-gains i)))))))))
    
    ;; Loop through all virtual rooms for one breakpoint in the trajectory
    (define (walk-all-rooms x y z time)
      (let ((dist (distance x y z)))
	;; remember first and last distances
	(if (not first-dist) ; set to #f (far) above
	    (set! first-dist dist))
	(set! last-dist dist)
	;; remember maximum and minimum distances
	(if (or (not (real? min-dist)) (< dist min-dist))
	    (set! min-dist dist))
	(if (or (not (real? max-dist)) (> dist max-dist))
	    (set! max-dist dist))
	;; push delay for current point (for doppler)
	(if (or (null? dly)
		(> time (cadr dly)))
	    (begin
	      (set! dly (cons (dist->samples dist) 
			      (cons time dly)))
	      ;; doppler should be easy, yeah right. We use "relativistic" correction
	      ;; as the sound object can be travelling close to the speed of sound. 
	      ;; http://www.mathpages.com/rr/s2-04/2-04.htm, 
	      ;; va = 0 (stationary listener)
	      ;; ve = moving object
	      ;; va = (* ve (/ 1 (+ 1 (/ ve c))) (sqrt (- 1 (* (/ ve c) (/ ve c)))))
	      (if prev-time
		  (let ((ratio (/ (- dist prev-dist)
				   (* duration (- time prev-time) dlocsig-speed-of-sound))))
		    (set! doppler (cons (* (/ 1.0 (+ 1 ratio)) (sqrt (- 1 (* ratio ratio)))) 
					(cons (/ (+ prev-time time) 2) doppler)))))))

	;; do the rendering of the point
	(cond ((= render-using amplitude-panning)
	       ;; amplitude panning
	       (famplitude-panning x y z dist time))
	      ((= render-using ambisonics)
	       ;; ambisonics b format
	       (render-ambisonics x y z dist time))
	      ((= render-using decoded-ambisonics)
	       ;; ambisonics decoded
	       (fdecoded-ambisonics x y z dist time)))
	
	(let ((room 1))
	  ;; remember current time and distance for next point
	  (set! prev-time time)
	  (set! prev-dist dist)
	  ;; return number of rooms processed (?)
	  room)))
    
    ;; Check to see if a segment changes radial direction:
    ;;   a change in radial direction implies a change in 
    ;;   doppler shift that has to be reflected as a new
    ;;   point in the rendered envelopes
    (define (change-direction xa ya za ta xb yb zb tb)
      (walk-all-rooms xa ya za ta)
      (unless (and (= xa xb)
		   (= ya yb)
		   (= za zb)
		   (= ta tb))
	(let ((vals (nearest-point xa ya za xb yb zb 0 0 0)))
	  (let ((xi (car vals))
		(yi (cadr vals))
		(zi (caddr vals)))
	    (if (and (if (< xa xb) (<= xa xi xb) (<= xb xi xa))
		     (if (< ya yb) (<= ya yi yb) (<= yb yi ya))
		     (if (< za zb) (<= za zi zb) (<= zb zi za)))
		(walk-all-rooms xi yi zi
				(+ tb (* (- ta tb)
					 (/ (distance (- xb xi) (- yb yi) (- zb zi))
					    (distance (- xb xa) (- yb ya) (- zb za)))))))))))
    
    ;; Check to see if a segment intersects the inner sphere:
    ;;   points inside are rendered differently so we need to
    ;;   create additional envelope points in the boundaries
    (define (intersects-inside-radius xa ya za ta xb yb zb tb)
      (let* ((mag (distance (- xb xa) (- yb ya) (- zb za)))
	     (vx (/ (- xb xa) mag))
	     (vy (/ (- yb ya) mag))
	     (vz (/ (- zb za) mag))
	     (bsq (+ (* xa vx) (* ya vy) (* za vz)))
	     (disc (let ((u (- (+ (* xa xa) (* ya ya) (* za za))
			       (* inside-radius inside-radius))))
		     (- (* bsq bsq) u)))
	     (hit (>= disc 0.0)))
	(if (not hit)
	    (change-direction xa ya za ta xb yb zb tb)
	    ;; ray defined by two points hits sphere
	    (let ((root (sqrt disc)))
	      (let ((rin  (- (+ bsq root)))
		    (rout (- root bsq))
		    (xi #f) (yi #f) (zi #f) (ti #f) (xo #f) (yo #f) (zo #f) (to #f))
		(if (> mag rin 0) ;(and (> rin 0) (< rin mag))
		    ;; intersects entering sphere
		    (begin
		      (set! xi (+ xa (* vx rin)))
		      (set! yi (+ ya (* vy rin)))
		      (set! zi (+ za (* vz rin)))
		      (set! ti (+ tb (* (- ta tb)
					(/ (distance (- xb xi) (- yb yi) (- zb zi))
					   (distance (- xb xa) (- yb ya) (- zb za))))))))
		(if (and (> rout 0) (< (abs rout) mag))
		    ;; intersects leaving sphere
		    (begin
		      (set! xo (+ xa (* vx rout)))
		      (set! yo (+ ya (* vy rout)))
		      (set! zo (+ za (* vz rout)))
		      (set! to (+ tb (* (- ta tb)
					(/ (distance (- xb xo) (- yb yo) (- zb zo))
					   (distance (- xb xa) (- yb ya) (- zb za))))))))
		(if xi
		    (begin
		      (change-direction xa ya za ta xi yi zi ti)
		      (if xo
			  (begin
			    (change-direction xi yi zi ti xo yo zo to)
			    (change-direction xo yo zo to xb yb zb tb))
			  (change-direction xi yi zi ti xb yb zb tb)))
		    (if xo
			(begin
			  (change-direction xa ya za ta xo yo zo to)
			  (change-direction xo yo zo to xb yb zb tb))
			(change-direction xa ya za ta xb yb zb tb))))))))
    
    ;; Recursively split segment if longer than minimum rendering distance:
    ;;   otherwise long line segments that have changes in distance render 
    ;;   the amplitude envelope as a linear function that does not reflect
    ;;   the chosen power function (1/d^n)
    (define (fminimum-segment-length xa ya za ta xb yb zb tb)
      (let ((dist (distance (- xb xa) (- yb ya) (- zb za))))
	(if (< dist minimum-segment-length)
	    (intersects-inside-radius xa ya za ta xb yb zb tb)
	    ;; interpolate a new point half way thorugh the segment
	    (let* ((xi (/ (+ xa xb) 2))
		   (yi (/ (+ ya yb) 2))
		   (zi (/ (+ za zb) 2))
		   (ti (+ tb (* (- ta tb)
				(/ (distance (- xb xi) (- yb yi) (- zb zi))
				   (distance (- xb xa) (- yb ya) (- zb za)))))))
	      (fminimum-segment-length xa ya za ta xi yi zi ti)
	      (fminimum-segment-length xi yi zi ti xb yb zb tb)))))
    
    ;; returns the new duration of a sound after using an envelope for time-varying sampling-rate conversion
    ;; (from Bill's dsp.scm)
    (define (src-duration e)
      (let ((len (- (length e) 2)))
	(do ((all-x (- (e len) (e 0))) ; last x - first x
	     (dur 0.0)
	     (i 0 (+ i 2)))
	    ((>= i len) dur)
	  (let ((area (let ((x0 (e i))
			    (x1 (e (+ i 2)))
			    (y0 (e (+ i 1))) ; 1/x x points
			    (y1 (e (+ i 3))))
			(if (< (abs (real-part (- y0 y1))) .0001)
			    (/ (- x1 x0) (* y0 all-x))
			    (/ (* (- (log y1) (log y0)) 
				  (- x1 x0)) 
			       (* (- y1 y0) all-x))))))
	    (set! dur (+ dur (abs (real-part area))))))))
    
    ;; Loop for each pair of points in the position envelope and render them
    (if (and (pair? xpoints) 
	     (null? (cdr xpoints)))
	;; static source (we should check if this is inside the inner radius?)
	(walk-all-rooms (car xpoints) (car ypoints) (car zpoints) (car tpoints))
	
	;; moving source
	(do ((len (- (min (length xpoints) (length ypoints) (length zpoints) (length tpoints)) 1))
	     (i 0 (+ i 1)))
	    ((>= i len))
	  (let ((xa (xpoints i))
		(ya (ypoints i))
		(za (zpoints i))
		(ta (tpoints i))
		(xb (xpoints (+ i 1)))
		(yb (ypoints (+ i 1)))
		(zb (zpoints (+ i 1)))
		(tb (tpoints (+ i 1))))
	    (fminimum-segment-length xa ya za ta xb yb zb tb)
	    (if (= i len)
		(walk-all-rooms xb yb zb tb)))))
    
    ;; create delay lines for output channels that need them
    (if speakers
	(let ((delays (speaker-config-delays speakers)))
	  (do ((len (length delays))
	       (channel 0 (+ 1 channel)))
	      ((= channel len))
	    (let ((delayo (delays channel)))
	      (set! (out-delays channel) (and (not (= delayo 0.0))
					      (make-delay (time->samples delayo))))
	      (set! max-out-delay (max max-out-delay delayo))))))
    
    ;; end of the run according to the duration of the note
    ;; (set! end (time->samples duration))
    ;; start and end of the loop in samples
    (let (;; delay from the minimum distance to the listener
	  (min-delay (dist->samples min-dist))
	  ;; duration of sound at listener's position after doppler src
	  ;; this does not work quite right but the error leads to a longer
	  ;; run with zeroed samples at the end so it should be fine
	  (real-dur (* duration (if (null? doppler) 1.0 (src-duration (reverse doppler)))))
	  ;; minimum distance for unity gain calculation
	  (min-dist-unity (max min-dist inside-radius))
	  (run-beg (time->samples start-time)))
      (let ((run-end (floor (- (+ (time->samples (+ start-time (max duration real-dur)))
				  (dist->samples last-dist)
				  (time->samples max-out-delay))
			       (if initial-delay 0.0 min-delay))))
	    ;; sample at which signal first arrives to the listener
	    (start (+ run-beg (dist->samples (- first-dist (if initial-delay 0.0 min-dist)))))
	    (direct-gain (* scaler
			    (if (number? unity-gain-dist)
				(expt unity-gain-dist direct-power)
				(if unity-gain-dist
				    1.0
				    (expt min-dist-unity direct-power)))))
	    (reverb-gain (* scaler 
			    (if (number? unity-gain-dist)
				(expt unity-gain-dist reverb-power)
				(if unity-gain-dist ; defaults to #f above
				    1.0
				    (expt min-dist-unity reverb-power))))))
      
	;; XXX hack!! this should be intercepted in the calling code, no 0 duration please...
	(if (<= real-dur 0.0)
	    (begin
	      (format () ";;; error: resetting real duration to 0.1 (was ~A)~%" real-dur)
	      (set! real-dur 0.1)))
	
	(let ((gen (make-move-sound
		    (list
		     ;; :start 
		     start
		     ;; :end 
		     (time->samples (+ start-time (max duration real-dur)))
		     ;; :out-channels 
		     (if speakers (speaker-config-number speakers) out-channels)
		     ;; :rev-channels 
		     rev-channels
		     ;; :path 
		     (make-delay delay-hack :max-size (max 1 (+ (ceiling (dist->samples max-dist)) delay-hack)))
		     ;; :delay 
		     (make-env (reverse dly)
			       :offset (if initial-delay 0.0 (- min-delay))
			       :duration real-dur)
		     ;; :rev 
		     (make-env (if (number? reverb-amount) ; as opposed to an envelope I guess
				   (list 0 reverb-amount 1 reverb-amount)
				   reverb-amount)
			       :duration real-dur)
		     ;; :out-delays 
		     out-delays
		     ;; :gains 
		     (do ((v (make-vector out-channels))
			  (i 0 (+ i 1)))
			 ((= i out-channels) v)
		       (set! (v i) (make-env (reverse (channel-gains i))
					     :scaler direct-gain
					     :duration real-dur)))
		     ;; :rev-gains 
		     (and (> rev-channels 0)
			  (do ((v (make-vector rev-channels))
			       (i 0 (+ i 1)))
			      ((= i rev-channels) v)
			    (set! (v i) (make-env (reverse (channel-rev-gains i))
						  :scaler reverb-gain
						  :duration real-dur))))
		     ;; :out-map 
		     (if speakers 
			 (speaker-config-map speakers) 
			 (do ((v (make-vector out-channels))
			      (i 0 (+ i 1)))
			     ((= i out-channels) v)
			   (set! (v i) i))))
		    *output*
		    *reverb*)))
	  (list gen
		;; return start and end samples for the run loop
		run-beg
		run-end))))))

;; (with-sound(:channels 6 :play #f :statistics #t) (sinewave 0 10 440 0.5 :path (make-path '((-10 10) (0.5 0.5) (10 10)) :error 0.001)))
;;
;; (with-sound(:statistics #t :channels 4 :reverb-channels 4 :reverb freeverb :decay-time 3)
;;  (move 0 "/usr/ccrma/snd/nando/sounds/kitchen/bowl/small-medium-large-1.snd"
;;	:paths (list (make-spiral-path :start-angle 0 :turns 2.5)
;;		     (make-spiral-path :start-angle 180 :turns 3.5))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Run macro to localize samples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define dlocsig move-sound)


#|

;; (define hi (make-path '((-10 10) (0.5 0.5) (10 10)) :3d #f :error 0.001))
;; (make-dlocsig 0 1.0 :out-channels 2 :rev-channels 0 :path (make-path '((-10 10) (0.5 0.5) (10 10)) :3d #f))

(if (provided? 'snd)
    (require snd-ws.scm)
    (require sndlib-ws.scm))

(define* (sinewave start-time duration freq amp 
		   (amp-env '(0 1 1 1))
		   (path (make-path :path '(-10 10 0 5 10 10))))
  (let* ((vals (make-dlocsig :start-time start-time
			     :duration duration
			     :path path))
	 (dloc (car vals))
	 (beg (cadr vals))
	 (end (caddr vals)))
    (let ((osc (make-oscil :frequency freq))
	  (aenv (make-env :envelope amp-env :scaler amp :duration duration)))
       (do ((i beg (+ i 1)))
	   ((= i end))
	 (dlocsig dloc i (* (env aenv) (oscil osc)))))))

(with-sound (:channels 2) (sinewave 0 1.0 440 .5 :path (make-path '((-10 10) (0.5 0.5) (10 10)) :3d #f)))

|#


