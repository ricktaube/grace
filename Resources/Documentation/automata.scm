;; Examples of 1D and 2D Cellular Automata

;; To create an automata call '(make-automata states rule ...)'  and
;; pass it a list of initial integer states and a rule function. For
;; 2D automata states is a list of lists where each sublist defines a
;; row of inital states. The rule function implements the automata's
;; state transition. It is called automatically and passed two
;; arguments: the automata and the index of the current cell and it
;; returns the NEXT state of the cell at index. Inside the rule
;; function use the function '(cell-state automata index . neighbor)'
;; to accesses the state of the current cell and any of its neighbors,
;; where neighbor is a positive or negative increment to the
;; index. cell-state automatically computes addresses mod the size of
;; the cell array so you do not have to perform bounds checking when
;; you access neighbors. For 2D automata each index and neighbor is a
;; two dimensional point. You specify 2D points and 2D increments
;; using the '(xy x y)' function, ie '(cell-state index (xy -1 -1))'
;; would access the value of the north-west neighbor of the current
;; cell in a 2D automata.

;; Once an automata has been created, use the '(state automata . all)'
;; function to read the next state from it, or to return a list of all
;; states in the current generation if the optional second argument is
;; true. You can associate a grapical state window with an automata by
;; providing the optional 'window' and 'colormap' arguments (both must
;; be specified if either is)

;; :window
;;    the window title (string) for the new window. an error is
;;    signaled if a window with that name is aready open
;; :colormap
;;    a list of state and color pairs: (s1 c1 s2 c2 ... sN cN) all the
;;    states and their colors must be provided. colors are symbols or
;;    strings, the complet list of colors is available in the *colors*
;;    variable.
;; :backgroundcolor
;;    a string or symbol naming the background color for the window.
;; :cellsize
;;    the size in pixels of each cell in the state window display.
;;    the value of cellsize defaults to 50
;; :cellbordersize
;;    the size in pixes of space between each cell in the state window
;;    display. the default cellbordersize is 1
;; :rows
;;    for 1D automata this specifies the number of successive
;;    generations you want to display in the window. each generation
;;    is shown as a "row" in the display. the default is 1

(define (add-neighbors auto index) 
  (let ((left (cell-state auto index -1))
        (right (cell-state auto index 1)))
    (modulo (+ left right) 3)))

(define foo (make-automata '(0 1 0 1 0) add-neighbors))

(state foo #t)

;; add a state window to display states as they are
;; read. both the 'window' and 'colormap' arguments are required for
;; window displays. the colormap is a list of pairs: (state1 color1
;; state1 color2 ... stateN colorN) where states are integers and
;; colors are color names (symbols or strings). the complete list of
;; colors are in the variable *colors*

(define foo  (make-automata '(1 0 2 2 0 2 1 2)
                            add-neighbors
                            :window "my state window"
                            :colormap '(0 red 1 green 2 blue)
                            :backgroundcolor "azure")) 

(state foo #t)

;; display and sonify an eight-state automata in real time

(define (8states auto index)
  (with-states auto index ((left -1) (here 0) (right 1))
	       (+ (logand left #b100)
       		  (logand here #b010)
       		  (logand right #b001))))

(define (play-8notes len auto keys)
  (process for i below len
       	   for s = (state auto)
       	   for k = (nth keys s)
       	   for r = (rescale s 0 7 .150 .400)
       	   do (send "mp:midi" 0 (* r 1.9) k )
       	   (wait r)))

(let* ((len (* 64 2))
       (keys (key '(c4 fs4 af4 b4 d5 ef5 e5 g5)))
       (colors '(0 pink 1 purple 2 yellow 3 red 
                 4 green 5 brown 6 blue 7 orange))
       (auto (make-automata '(0 1 2 3 4 5 6 7) 8states
                            :window "my 8 states"
                            :colormap colors
                            ))
       )
  (sprout (play-8notes len auto keys)))

;;;
;;;  A "left shift" automata; next state is from i+1
;;;

(define (left-shift automata index)
  (cell-state automata index 1))

(define go-left
  (make-automata '(0 1 2 3) left-shift
                 :window "my left shift"
                 :colormap '(0 pink 1 yellow 2 green 3 blue)
                 :cellsize 100))

; eval this multiple times to rotate through cells

(state go-left)

;;
;; right sum mod 3
;;

(define (sum-right auto index)
   ;; add cell to rightward cell's value mod 3
  (let ((here (cell-state auto index))
        (right (cell-state auto index 1)))
    (modulo (+ here right) 3)))

(define go-right
  (make-automata '(1 0 0 2) sum-right
                 :window "my right sum"
                 :colormap '(0 blue 1 green 2 red) 
                 :cellsize 100
                 ))

(state go-right #t)

(define go-right
  (make-automata (loop repeat 40 collect (random 3)) 
                 sum-right
                 :window "Go Right!"
                 :colormap '(0 pink 1 aqua 2 magenta) 
                 :cellsize 4
                 :rows 8))

(state go-right #t)

;;;
;;; 2D automata using HGLASS rule set. 5 postions are examined,
;;; allowing up to 32 different states.
;;;

;; lookup vector for new state

(define hglass-states 
  (vector 0 1 1 1 0 0 0 0   0 0 0 1 0 0 0 0   
          0 0 0 0 0 1 0 0   0 1 0 0 0 1 1 1))

(define (hglass auto index)
  ;; ior east, west, south and north to compute new state
  (let ((here (cell-state auto index ))
        (east (cell-state auto index (xy 1  0)))
        (west (cell-state auto index (xy -1  0)))
        (south (cell-state auto index (xy 0  1)))
        (north (cell-state auto index (xy 0 -1))))
    (vector-ref hglass-states
                (logior (ash east 4)
                        (ash west 3) 
                        (ash south 2)
                        (ash north 1) 
                        here))))

(define my-hglass
  ;; random first generation
  (let ((init (loop for r below 8
                    collect (loop for c below 8
                                  collect (random 2)))))
    (make-automata init hglass
		   :window "my hglass automata"
		   :colormap '(0 pink 1 lavender)
		   )))

(state my-hglass #t)

;;;
;;; pretty square
;;;

(define hglass-square
  (let ((square
       '(
       	 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
       	 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
       	 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
       	 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
       	 (0 0 0 0 1 1 1 1 1 1 1 1 0 0 0 0)
       	 (0 0 0 0 1 1 1 1 1 1 1 1 0 0 0 0)
       	 (0 0 0 0 1 1 1 1 1 1 1 1 0 0 0 0)
       	 (0 0 0 0 1 1 1 1 1 1 1 1 0 0 0 0)
       	 (0 0 0 0 1 1 1 1 1 1 1 1 0 0 0 0)
       	 (0 0 0 0 1 1 1 1 1 1 1 1 0 0 0 0)
       	 (0 0 0 0 1 1 1 1 1 1 1 1 0 0 0 0)
       	 (0 0 0 0 1 1 1 1 1 1 1 1 0 0 0 0)
       	 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
       	 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
       	 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
       	 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
       	 )))
    (make-automata square hglass
                   :window "my hglass automata"
                   :colormap '(0 pink 1 lavender)
                   :backgroundcolor "yellow")))

(state hglass-square #t)

;;;
;;; Conway's game of Life.
;;;

(define (life auto index)
  (let ((ul (cell-state auto index (xy -1 -1)))
        (um (cell-state auto index (xy 0 -1)))
        (ur (cell-state auto index (xy 1 -1)) )
        (ml (cell-state auto index (xy -1 0)))
        (me (cell-state auto index ))
        (mr (cell-state auto index (xy 1 0)))
        (ll (cell-state auto index (xy -1 1)))
        (lm (cell-state auto index (xy 0 1)))
        (lr (cell-state auto index (xy 1 1)))
        )
    (let ((sum (+ ul um ur ml mr ll lm lr))) 
      (if (= me 0) ; dead cell
        (if (= sum 3) 1 0)
        (if (or (= 2 sum) (= 3 sum)) 1 0)))))

(define game-of-life
  (make-automata '((0 0 0 0 0 0 0 0)
                   (0 0 0 1 1 0 1 0)
                   (0 0 1 0 1 0 1 0)
                   (0 0 1 1 1 0 0 0)
                   (0 1 0 0 1 1 1 0)
                   (0 1 1 1 0 0 0 0)
                   (0 0 0 1 1 0 1 0)
                   (0 0 0 0 0 0 0 0))
                 life
                 :window "my life"
                 :colormap '(0 red 1 green)
                 :cellsize 40 
                 :cellbordersize 1))

(state game-of-life #t)

(define other-life
  (make-automata (loop repeat 8 
                       collect (loop repeat 8 
                                     collect (random 2)))
                 life
                 :window "my other life"
                 :colormap '(0 aquamarine 1 burlywood)
                 :cellbordersize 1
                 :cellsize 16))

(state other-life #t)
