(definstrument (wave time dur freq amp
		     (ampenv '(0 0 10 1 90 1 100 0))
		     (degree 0)
		     (dist 0)
		     (reverb 0)
		     (skew '(0 1 100 1)))
  "wave (time dur freq amp
         (ampenv '(0 0 10 1 90 1 100 0))
         (degree 0)
         (dist 0)
         (reverb 0)
         (skew '(0 1 100 1)))
 A simple sine wave instrument with envelope and glissando controls.
 (with-sound () (wave 0 1 440 .1))"
  (let* ((beg (seconds->samples time))
         (end (seconds->samples (+ time dur)))
         (osc (make-oscil freq))
         (aenv (make-env ampenv :scaler amp :duration dur))
         (sig (make-locsig :degree degree
                           :distance dist
                           :reverb reverb))
         ;; skw is frequency change: when our env is 1 we want to add
         ;; 0, when our env is 2 we want to add freq etc
         (skw (make-env skew :scaler (hz->radians freq)
                        :duration dur
                        :offset (hz->radians (- freq)))))
    (do ((i beg (+ i 1)))
        ((= i end) #t)
      (locsig sig i (* (env aenv)
                       (oscil osc (env skw)))))))

;(definstrument (wavea time duration frequency amplitude
;		     :key 
;		     (ampenv '(0 0 10 1 90 1 100 0))
;		     (location 0)
;		     (distance 0)
;		     (reverb 0)
;		     (glissando 1)
;		     (glissenv '(0 0 100 0)))
;  "wave (time duration frequency amplitude
;            :key 
;            (ampenv '(0 0 10 1 90 1 100 0))
;            (location 0)
;            (distance 0)
;            (reverb 0)
;            (glissando 1)
;            (glissenv '(0 0 100 0)))
; A simple sine wave instrument with envelope and glissando controls.
; (with-sound () (wave 0 1 440 .1))"
;  (let* ((beg (seconds->samples time))
;         (end (seconds->samples (+ time duration)))
;         (osc (make-oscil frequency))
;         (env (make-env ampenv :scaler amplitude
;                        :duration duration ))
;         (sig (make-locsig :degree location
;                           :distance distance
;                           :reverb reverb))
;         (gls (make-env glissenv 
;                        :duration duration
;                        :scaler (hz->radians 
;                                 (- (* frequency glissando)
;                                    frequency)))))
;    (run
;     (lambda ()
;       (do ((i beg (+ i 1)))
;	   ((= i end) #t)
;	 (locsig sig i (* (env env)
;			  (oscil osc (env gls))))))
;     )))

;; (with-sound () (wave 0 1 440 .1))
;; (with-sound () (wave 0 4 (hz 'c4) .5 :ampenv '(0 0 2 1 99 1 100 0) :skew '(0 1 25 1 75 1.5 100 1.5)))

