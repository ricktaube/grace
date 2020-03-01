(define chords
  (loop repeat 30 
        collect
        (let ((ch (list)))
          (loop for c = (between 60 85)
                while (< (length ch) 3)
                do (if (not (member c ch))
                     (set! ch (append ch (list c)))))
          ch)))

(define (playch chs rhy dur amp)
  (process for ch in chs
           do
           (loop for c in ch
                 do (mp:midi 0 :dur dur :key c :amp amp))
           (wait rhy)))

(note chords)

(sprout (playch chords .7 (* .7 .9) .6))

(acoustic-sort chords #f)
(stacktrace)

(acoustic-dissonance '(69 83 78 67 68))
(merge-spectrums 69 '( 83 78 67 68))

(sprout (playch (acoustic-sort chords #f) .7 (* .7 .9) .6))

chords
(acoustic-sort chords #f)

(map (lambda (x) (cons (acoustic-dissonance x) x)) chords)

(let ((pitches '(60 69 80 61 )))
  (merge-spectrums (first pitches) (rest pitches))
  )

(acoustic-dissonance '(60 69 80 61 66))
(acoustic-dissonance '(62 69))

(sort '(60 540) <)

(acoustic-dissonance '(60 61 62 73))
0.7287

(acoustic-dissonance '(60))
0.0012

(acoustic-dissonance '(47 49 53))
0.3846

(acoustic-dissonance '(62 67 72 77 82))
0.1592


(acoustic-dissonance '(60))
(acoustic-dissonance '(60 72))
(acoustic-dissonance '(60 67))
(acoustic-dissonance '(60 61))




