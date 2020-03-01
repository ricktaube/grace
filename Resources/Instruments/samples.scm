;;; ********************************************************************
;;; parsing and cutting audio samples, see end of file for example.
;;;
;;; (parse-samples file &KEY on-amp off-amp channel dur
;;;                min-steady min-duration)
;;;
;;; Finds the start and end times of individual samples in the
;;; specified audio file and returns the results in a list. The start
;;; and end time of samples are found by comparing the specified on
;;; off amplitude thresholds to a windowed average accross the
;;; file. Any bogus (too short) on/off triggerings are returned as a
;;; second value (a list of bad positions in the samples)
;;;
;;;   file           the sound file to parse samples from.
;;;   :on-amp        average amp threshold for beginning a sample.
;;;   :off-amp       average amp threshold for ending a sample.
;;;   :start         start time in file for parsing, default is 0.
;;;   :window        window length (seconds) for amp averaging, default is .2
;;;   :channel       the audio channel to parse, default is 0.
;;;   :dur           duration of file to parse, default is whole file
;;;   :min-steady    minimum time after starting a sample before looking for off
;;;   :min-duration  minimum duration for a sample to be valid.
;;;
;;;
;;; (cut-samples file samps outdit &KEY from to offset sample-steps
;;;              env prefix dbeg dend names)
;;;
;;; Generates sample files from an audio input file, a list of samples
;;; positions returned by parse-samples and an output directory. By
;;; default files are note names starting at the :sample-start keynum.
;;;   file           the sound file to cut samples from
;;;   samps          the sample list returned by parse-samples
;;;   outdir         the directory to hold the samples
;;;   :from          starting index in samps to write (default 0)
;;;   :to            ending index in samps to wite (default end)
;;;   :sample-start  starting keynum for sample files (default 0)
;;;   :sample-steps  chromatic steps between each sample (default 1)
;;;   :prefix        optional prefix for each sample file name
;;;   :dbeg          add/subtract time delta from start of each sample
;;;   :dend          add/subtract time delta from end of each sample
;;;   :names         force sample file names to this list of names.
;;;

(define* (parse-samples file on-amp off-amp
                        (start 0)
                        (window .2)
                        (channel 0)
                        dur
                        (min-steady 0)
                        (min-duration most-negative-fixnum)
                        (trace #t)
                        )
  (if (not (file-exists? file) )
      (error "file does not exist: ~S" file))
  (if (not (number? on-amp))
      (error "missing :on-amp amplitude threshold"))
  (if (not (number? off-amp))
      (error "missing :off-amp amplitude threshold"))
  (let* ((tot (mus-sound-duration file))
         (fil (make-file->sample file))
         (beg (seconds->samples start))
         (len (seconds->samples window))
         (end (- (seconds->samples (min (+ beg (or dur tot)) tot)) len))
         (mov (make-moving-average len))
         (std (seconds->samples min-steady))
         (val 0.0)
         (avr 0.0)
         (k 0)
         (on? #f)
         (middle 0)
         (ontime 0.0)
         (offtime 0.0)
         (times (list))
         (bad (list))
         )
    (run
     (do ()
         ((not (< beg end))
          )
       (set! val (in-any beg channel fil))
       (set! avr (moving-average mov (abs val)))
       (if (not on?) ; not currently in a note
           (if (>= avr on-amp)
               (begin
                 (set! ontime (samples->seconds (+ 1 (- beg len))))
                 (set! middle (+ beg std))
                 (set! k (+ k 1))
                 (when trace
                   (format #t "~3d ~F" (- k 1) ontime))
                 (set! on? #t))
               #f)
           (if (and (>= beg middle) (<= avr off-amp))
               (let ((sdur 0.0))
                 (set! offtime (samples->seconds (- beg len)))
                 (set! sdur (- offtime ontime))
                 (when trace
                   (format #t " : ~F (~F" offtime sdur))
                 (when (< sdur min-duration)
                   (when trace
                     (format #t "    too short!"  ))
                   (set! bad (cons (- k 1) bad))
                   )
                 (when trace
                   (format #t ")~%"))
                 (set! times
                       (cons (list (* 1.0 ontime) (* 1.0 offtime) 
                                   (* 1.0 sdur)) times))
                 
                 (set! on? #f))
               #f)
           )
       (set! beg (+ beg 1))
       ))
  (if (or (pair? times) (pair? bad))
      (if (pair? bad)
          (values (reverse! times) (reverse! bad))
          (values (reverse! times)))
      (list))
  ))

;;;
;;; utility for removing bad samples from the list of samples returned
;;; by parse-samples
;;;

(define (remove-bad-samples samps bad)
  (loop for n in samps
        for i from 0
        unless (member i bad)
        collect n))

;;;
;;; generates sample files from the input file and a list of samples
;;; returned by parse-samples
;;;

(define* (cut-samples file samps outdir 
                      (sample-start 21) (sample-step 1)
                      sample-names
                      (prefix "")
                      (dbeg 0) (dend 0)) ;; add/subtract constant amount
  (unless (file-exists? file)
    (error "Sample file ~S does not exist." file))
  (unless (char=? #\/ (string-ref outdir (- (string-length outdir) 1)))
    (set! outdir (string-append outdir "/")))
  (unless (file-exists? outdir)
    (error "Output directory ~S does not exist." outdir))
  (when sample-names
    (unless (= (length sample-names) (length samps))
      (error "Length of sample list and sample-names is not the same.")))
  (let* ((filetype (pathname-type file))
         (srate (mus-sound-srate file))
         (chans (mus-sound-chans file))
         (temp (default-header-type filetype))
         (headertype #f)
         (dataformat #f)
         )
    (if (null? temp)
        (error "Audio file not .aiff, .wav or .snd: ~S." file)
        (set! headertype (second temp)))
    (set! dataformat (second (default-data-format filetype)))
    (format #t "Cutting ~S samples from ~A:~%" (length samps) file)    
    (do ((tail samps (cdr tail))
         (i 0 (+ i 1)))
        ((null? tail) 
         (values))
      (let* ((cut (car tail))
             (out (string-append outdir prefix
                                 (if sample-names
                                     (list-ref sample-names i)
                                     (note (+ sample-start (* sample-step i))))
                                 "." filetype))
             (beg (+ (first cut) dbeg))
             (end (+ (second cut) dend)))
        (unless (> end beg) 
          (error "End value ~S less than start ~S." end beg))
        (format #t "~3D (~F ~F) -> ~S~%" i beg end out)
        (with-sound (:output out :srate srate :channels chans :play #f
                             #:data-format dataformat
                             #:header-type headertype)
          (let* ((*clm-srate* srate)
                 (frame1 (seconds->samples beg))
                 (frame2 (seconds->samples end)))
            (mus-mix *output* file 0 (- frame2 frame1) frame1)
            ))))
    ))

#|

;; Example: parse flute samples from a file of flute tones. Download the file from:
;; http://theremin.music.uiowa.edu/Sound%20Files/Musical%20Instrument%20Samples/Flute/flute.vib.ff.B3B4.aiff

(load "samples.scm")
(define infile "/Users/hkt/flute.vib.ff.B3B4.aiff")
(define outdir "/tmp/")
(define samps
  (parse-samples infile :on-amp .005 :off-amp .001 :min-steady .5 )
  )
(cut-samples infile samps outdir :sample-start 59)

|#
