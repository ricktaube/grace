;;; **********************************************************************
;;; VKEY instrument for sndlib with modular backend rendering. includes
;;; VKEY-DB, a function that creates vkey sample databases from
;;; directories of sample sound files. vkey requires the following
;;; clm instruments also be loaded:
;;;
;;; fullmix.scm : clm-3 sound file mixer
;;; expandn.scm : michael klingbeil's expansion ins with dynamic
;;;               support for multi channel io and reverb
;;;
;;; (vkey time duration keynum samples &KEY base amplitude amp-env
;;;       normalize reverb input-file-start-time degree expand )
;;;
;;; time {number}
;;;     start time of sound in seconds
;;; duration {number | #f}
;;;     duration in seconds. clipped so as not to exceed file length
;;;     taking into account srate change and input start time. defaults
;;;     to the length of the soundfile.
;;; keynum {number}
;;;     MIDI keynumber or SRC ratio (see samples)
;;; samples {list | string}
;;;     a sample database list or the filename of a single sample
;;;     soundfile.  if the latter, then keynum is interpreted as a
;;;     key number if :base is set, or as a SRC ratio if :base is not
;;;     set. for example:
;;;       (vkey 0.0 2.0 72.0 "sounds:guitarC4.aiff" :base 60.0)
;;;       (vkey 0.0 2.0 2.0 "sounds:guitarC4.aiff")
;;;     both play guitarC4.aiff transposed ane octave up 
;;; :base {knum}
;;;     base MIDI key number for a single sound file sample (see 
;;;     explanation under samples)
;;; :amplitude {number}
;;;     an amplitude scaler on the soundfile. defaults to 1.
;;; :amp-env {list}
;;;     envelope shape to apply to the note
;;; :normalize {boolean}
;;;     scales amplitude by normalization factor in sample database. 
;;;     see doc on vkey-db for more information.
;;; :bend {list}
;;;     an envelope on srate 
;;; :reverb {number}
;;;     reverb amount or nil for no reverb
;;; :input-file-start-time {number}
;;;     where to start in the input sample
;;; :degree {number}
;;;     panning, uses locsig to determine the correct panning values.
;;;     should be able to handle n input channels and m output 
;;;     channels correctly.
;;; :expand {number}
;;;     an optional expansion factor passed to the 'expandn' backend.

(define (vkey time duration keynum samples . args)
  (with-optkeys (args (amplitude 1.0)
		      amp-env reverb (input-file-start-time 0.0)
		      expand degree bend base
		      (normalize #t)
		      expand )
    (let ((samps samples)
	  (maxdur #f)
	  (srate #f)
	  (in-chans #f)
	  (out-chans #f)
	  (file #f) )
      (cond ((string? samps)
	     ;; samples is a filename. 
	     (set! file samps)
	     (set! maxdur (mus-sound-duration file))
	     (set! srate
		   (if base
		       (/ (hz keynum) (hz base)) ; base=sample's keynum
		       keynum)))                 ; keynum is a ratio
	    (else
	     ;; value is symbol, list or vkey-db
	     (let ((db (cond ((and (symbol? samps) (not (null? samps)))
			      (error "fix vkey for sample symbols"))
			     ((pair? samps) 
			      samps)
			     (else
			      (error "Illegal vkey samples value"
				     samps))))
		   (vk #f)
		   )
	       ;; find closest key and determine appropriate
	       ;; srate shift of it for achiveing keynum
	       (if (not (number? keynum))
		   (set! keynum (key keynum)))
	       (set! vk (closest-key keynum db))
	       (set! srate (expt 2 (/ (- keynum ( first vk)) 12.0))) ;vk-key
             (set! file ( second vk)) ; vk-fil
             (set! maxdur ( third vk)) ;vk-dur
             (set! in-chans ( sixth vk)) ; vk-chn
             (if normalize
		 (set! amplitude (* amplitude ( fifth vk)))) ;vk-avr
             )))
      ;; fix up duration to not exceed file length (taking
      ;; into account srate change and input start)
      (set! maxdur (/ (- maxdur input-file-start-time) srate))
      (if expand (set! maxdur (* maxdur expand)))
      ;; TODO: warn if out of range of sample database
      (if (not duration)
	  (set! duration maxdur)
	  (set! duration (min duration maxdur)))
      ;; when matrixing need to know num chans in and out
      (when (or degree amp-env)
	(unless in-chans
	  (set! in-chans (mus-sound-chans file)))
	(set! out-chans (mus-channels *output*)))
      (if expand
	(vkey-expandn time file input-file-start-time duration
		      srate bend amplitude amp-env
		      in-chans out-chans
		      expand degree reverb '())
	(vkey-fullmix time file input-file-start-time duration
		      srate bend amplitude amp-env
		      in-chans out-chans
		      expand degree reverb '())))))



(define (vkey-fullmix start file
		      filebeg duration srate bend amp amp-env in-chans
		      out-chans expand degree reverb data)

  #|(format #t "~S~%" (list 'fullmix file start duration (or filebeg 0.0)
		  (if degree
               (degree->matrix degree in-chans out-chans amp amp-env)
               (if amp-env 
                   (ampenv->matrix amp-env in-chans out-chans amp)
                   amp))
           (or srate 1)
           reverb))|#

  (when (pair? bend)
    (let ((env (append bend (list))))
      (do ((tail env (cddr tail)))
	  ((null? tail)
	   (set! srate env))
	;; scale y bends by srate
	(set-car! (cdr tail) (* (cadr tail) srate)))))
  (fullmix file start duration (or filebeg 0.0)
           (if degree
               (degree->matrix degree in-chans out-chans amp amp-env)
               (if amp-env 
                   (ampenv->matrix amp-env in-chans out-chans amp)
                   amp))
           (or srate 1)
           reverb)
  )

(define (vkey-expandn start file
		      filebeg duration srate bend amp amp-env in-chans
		      out-chans expand degree reverb data)
  ;; convert bend to srate env
  (when (pair? bend)
    (let ((env (append bend (list))))
      (do ((tail env (cddr tail)))
	  ((null? tail)
	   (set! srate env))
	;; scale y bends by srate
	(set-car! (cdr tail) (* (cadr tail) srate)))))

  #|(format #t "~S~%" (list 'expandn start duration file amp
           :expand expand
           :ramp (list-prop data :ramp .2)
           :seglen (list-prop data :seglen .15)
           :srate srate
           :hop (list-prop data :hop .04)
           :amp-env (if (pair? amp-env) amp-env '(0 1 100 1))
           :input-start filebeg
           :grain-amp (list-prop data ':grain-amp .9)
           :matrix (if degree
                     (degree->matrix degree in-chans out-chans 1 #f)
                     #f)
           :reverb reverb))|#

  (expandn start duration file amp
           :expand expand
           :ramp (list-prop data :ramp .2)
           :seglen (list-prop data :seglen .15)
           :srate srate
           :hop (list-prop data :hop .04)
           :amp-env (if (pair? amp-env) amp-env '(0 1 100 1))
           :input-start filebeg
           :grain-amp (list-prop data ':grain-amp .9)
           :matrix (if degree
                     (degree->matrix degree in-chans out-chans 1 #f)
                     #f)
           :reverb reverb)
  )

(define (closest-key keynum vkdb)
  ;; return closest vk in vkdb to keynum, which can be a float
  (if (null? vkdb) #f
      (do ((best #f)
	   (bestkey (car vkdb))
	   (tail vkdb (cdr tail))
	   (diff #f))
	  ((or (null? tail)
	       (>= ( first (car tail)) keynum)) ;vk-key
	   bestkey)
	(set! diff (abs (- ( first (car tail)) keynum))) ;vk-key
	(if (or (not best) (<= diff best))
	    (begin 
	      (set! best diff)
	      (set! bestkey (car tail)))))))

(define (ampenv->matrix amp-env in-chans out-chans scaler-amp)
  ;; no panning but ampenv
  (unless (= scaler-amp 1)
    (let ((env (list #f)))
      (do ((ptr env)
	   (tail amp-env (cddr tail)))
	  ((null? tail)
	   (set! amp-env (cdr env)))
	(set-cdr! ptr (list (car tail) (* (cadr tail) scaler-amp)))
	(set! ptr (cddr ptr)))))
  (do ((inp 0 (+ inp 1))
       (mat (make-list in-chans)))
      ((= inp in-chans)
       mat)
    (do ((outp 0 (+ outp 1))
	 (vec (make-list out-chans)))
	((= outp out-chans)
	 (set-car! (list-tail mat inp) vec))
      (set-car! (list-tail vec outp)
		(if (= (modulo inp out-chans)
		       (modulo outp in-chans))
		    amp-env 0.0)))))

; (ampenv->matrix '(0 0 1 1) 2 2 .5)
; (ampenv->matrix '(0 0 1 1) 1 2 .5)
; (ampenv->matrix '(0 0 1 1) 2 1 .5)

(define (degree->matrix degree in-chans out-chans scaler-amp amp-env)
  ;; deal with panning, use locsig to get current values.
  (let ((loc (make-locsig degree :channels out-chans)))
    ;; check for amplitude values that are neary zero
    ;; but not quite due to trig roundoff
    (do ((i 0 (+ i 1)))
	((= i out-chans) #f)
      (if (< (abs (locsig-ref loc i)) 1e-10)
	  (locsig-set! loc i 0.0)))
    (do ((inp 0 (+ inp 1))
	 (mat (make-list in-chans)))
	((= inp in-chans)
	 mat)
      (do ((outp 0 (+ outp 1))
	   (vec (make-list out-chans)))
	  ((= outp out-chans)
	   (set-car! (list-tail mat inp) vec))
	(let ((a (* (locsig-ref loc outp) scaler-amp)))
	  ;; if the input channel is not going to its corresponding
	  ;; original output channel scale back gain slightly to
	  ;; maintain some sense of the original "spread" of the input
	  ;; sound
	  (if (not (= (modulo inp out-chans)
		      (modulo outp in-chans)))
	      (set! a (* a (expt 2 -0.5))))
	  (if (and (pair? amp-env) (not (= 0 a)))
	      (let ((env (append amp-env (list))))
		(do ((tail env (cddr tail)))
		    ((null? tail)
		     (set-car! (list-tail vec outp) env))
		  ;; scale y values by a
		  (set-car! (cdr tail) (* (cadr tail) a)))) 
	      (set-car! (list-tail vec outp) a)
	      ))))))

; (degree->matrix 45 2 2 .5 '(0 0 1 1))
; (degree->matrix 45 1 2 .5 '(0 0 1 1))
; (degree->matrix 45 2 1 .5 '(0 0 1 1))

;;; 
;;;  Vkey Database building
;;;

(define (vkey-db spec . args)
  (define (make-vk key fil dur amp avr chn)
    (list key fil dur amp avr chn))

  (define (setavrs db max neighbors)
    (do ((sum 0.0)
	 (tail db (cdr tail))
	 (i 0 (+ i 1)))
	((null? tail)
	 #f)
      (set! sum 0.0)
      (do ((j (- i neighbors) (+ j 1)))
	  ((> j (+ i neighbors)) #f)
	(let ((k (if (< j 0) 
		     (modulo j (+ 1 (* neighbors 2)))
		     (if (> j max) 
			 (- max (- j max))
			 j))))
	  (set! sum (+ sum ( fourth (list-ref db k)))))) ;vk-amp
      ;; divide avr amp of neighborhood by amp
      ;;(vk-avr (car tail) (/ (/ sum (+ 1 (* neighbors 2)))
      ;;                      (vk-amp (car tail))))
      (set-car! (list-tail (car tail) 4)
		(/ (/ sum (+ 1 (* neighbors 2)))
		   ( fourth (car tail))) ; vk-amp
		)))

  (with-optkeys (args keyfn (neighbors 2))
    (unless keyfn
      (set! keyfn 
	    (lambda (f)
	      (let ((x (read-from-string (pathname-name f))))
		(if (symbol? x) (key x)
		    (if (integer? x) x
			#f))))))
    (do ((db (list))
	 (tail spec (cdr tail))
	 (max 0 (+ max 1))
	 (f #f)
	 (k #f))
	((null? tail)
	 (set! db (sort db (lambda (a b) (< (car a) (car b)))))
	 (setavrs db (- max 1) neighbors)
	 db)
      (set! f (car tail))
      (if (not (file-exists? f))
	  (error "File does not exist" f))
      (set! k (keyfn f))
      (unless (integer? k)
	(error "Can't convert file name to keynum" f ))
      (let ((v (make-vk k
			f
			(mus-sound-duration f) 
			(do ((l (mus-sound-maxamp f) (cddr l))
			     (m 0))
			    ((null? l) m)
			  (if (> (abs (cadr l)) m)
			      (set! m (abs (cadr l)))))
			#f
			(mus-sound-chans f))))
	(set! db (cons v db))
	)
      )))

; (directory "/Users/hkt/Music/Samples/crotales")
; (vkey-db (directory "/Users/hkt/Music/Samples/crotales") )


