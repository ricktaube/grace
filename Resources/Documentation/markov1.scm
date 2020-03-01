;
;; Markov Analysis
;

; To run code put your cursor after each expression and press
; Command-Return, then check the console window for any output.

; One of the most interesting uses of Markov is to model real-world
; behavior by analyzing sequental motion to extract a markov table of
; outcomes and weights. Given such an analysis one can then generate
; 'Markov chains' with statistically similar characteristics of the
; original data. In a Markov analysis the target data is analyzed
; using a 'window' that moves over successive positions in the
; sequence elements in the data. The width of this window is
; determined by the order of the analysis. An analysis for a first
; order table uses a window width of 2: the first element in the
; window represents a past outcome and the second represents its
; successor. In a second order analysis the window is 3 elements wide,
; the first two elements are the past outcomes and the third element
; is that past's successor. The analysis proceeds by moving the window
; over successive elements in the data and counting the number of
; times each unique outcome follows each unique past. We can perform
; an example analysis on the following short data sequence:

;    a a b c b a

; Recall that a first order window examines pairs: the first element
; in the pair represents a past value and the second element is its
; outcome. Windowing over the example sequence therefore produces the
; following pairs:

;    aa, ab, bc, cb, ba, aa

; The very last window in the analysis (aa) is produced by wrapping
; the final window around to the front of the sequence again so that
; the last element in the sequence has a successor. An analysis that
; wraps around creates a seamless chain without any terminating
; condition. An alternate method of analysis encodes a unique terminal
; value, call it z, as the consequent of the last a. When z appears
; from the chain the caller will knows that the Markov process cannot
; generate any more events.

; The next step in the analysis is to count each unique outcome for
; each unique past, giving us the raw counts:

;        A       B       C
; A      2       1       0
; B      1       0       1
; C      0       1       0

; That are then normalized to probability proportions.

;        A       B       C
; A  ->  0.667   0.333   0.0
; B  ->  0.5     0.0     0.5
; C  ->  0.0     1.0     0.0


; Common Music provides the function markov-analyze that will compute
; an nth order Markov analysis from a specified sequence of data.
;
; markov-analyze( seq, [order: i] [print?: b] [pattern?: b] ...)

; Results returned by the function vary according to its :print? and
; :pattern? keyword arguments. One or more of the follow results are
; possible:

;   1. A printed table of the statistical analysis.
;   2. A pattern definition (code) that implements the pattern.
;   3. A pattern object ready to use.


; We demonstrate using markov-analysis using an (unnamed) melody.  We
; will analyze and listen to successively increasing Markov orders of
; the tune's pattern.  at some point (order=3 or 4) most people can
; guess the tune

; OK, are you ready to play Name... That... Tune!

; First, define a little process to play the tune pattern. 

(define (playtune reps pat rate)
  (process repeat reps
           for n = (next pat)
           do
           (mp:midi :key (keynum n) :dur (* rate 1.5))
           (wait rate)
           ))

; Next define the tune. You can use notes or keynums, it doesnt matter
; for the analysis. I'm writing it in keynums so you dont cheat!

(define tune (note '(60 60 62 60 65 64 60 60 62 60 67 65 60 60 72
                        69 65 64 62 70 70 69 65 67 65)))

; Now look AND LISTEN to sucessive orders of analysis:

; Display and listed to zero-order analysis results

(markov-analyze tune :order 0 :mode 1)

(sprout (playtune 60
                  (markov-analyze tune :order 0 :mode 2)
                  .2))

; First order results:

(markov-analyze tune :order 1 :mode 1)

(sprout (playtune 60
                  (markov-analyze tune :order 1 :mode 2)
                  .2))

; Second order analysis:

(markov-analyze tune :order 2 :mode 1)

(sprout (playtune 60
                  (markov-analyze tune :order 2 :mode 2)
                  .2))

; Third order results:

(markov-analyze tune :order 3 :mode 1)

(sprout (playtune 60
                  (markov-analyze tune :order 3 :mode 2)
                  .2))

; ...continue increasing the order until YOU can Name...That....Tune!

; Fourth order results:

(markov-analyze tune :order 4 :mode 1)

(sprout (playtune 60
                  (markov-analyze tune :order 4 :mode 2)
                  .2))

; Fifth Order (completely determined...)

(markov-analyze tune :order 5 :mode 1)

(sprout (playtune 60
                  (markov-analyze tune :order 5 :mode 2)
                  .2))

