;
;; Receiving MIDI Input
;

; To evaluate code put the cursor after each expression and press
; Command-Return, then check the console window for any output.

; Receiving MIDI input is accomplished by setting a 'receiver' (function)
; to handle incoming MIDI messages. Midi messages arrive as lists of
; (upto) four values: {opcode channel data1 data2}. This assumes you
; know what the MIDI opcodes are and what data values they
; take. Consult the CM dictinary and
; http://www.midi.org/techspecs/midimessages.php for more information.

; Here is the definition of a MIDI receiver that routes midi on and
; off messages to the output port

(define (myplayer data)
  (let* ((opcode (first data))
         (channel (second data))
         (keyn (third data))
         (velocity (fourth data)))
    (if (= opcode mm:on)
        (mp:on :key keyn :vel velocity :chan channel)
        (if (= opcode mm:off)
            (mp:off :key keyn :chan channel)))))

; Set receiver, then play your midi keyboard

(mp:receive myplayer)

; When you are done clear it...

(mp:receive #f)

; An opcode receiver that plays a major or minor chord whenever a
; Note On arrives

(define (mychord data)
  (let* ((chan (second data))
         (keyn (third data))
         (vel (fourth data)))
    (mp:midi :key keyn :amp vel :chan chan)
    (mp:midi :key (+ keyn (pick 3 4)) :amp vel :chan chan)
    (mp:midi :key (+ keyn 7) :amp vel :chan chan)
    ))

; Assign receiver for the mm:on opcode (Note Ons)

(mp:receive mm:on mychord)

; When you are done clear it...

(mp:receive mm:on #f)

; A more comple midi receive example that takes pairs of input midi
; notes and creates an accompanyment figure for them using
; fm-spectrum.

; generator uses two key numbers and velocity to generate fm spectrum
; 1. key2  -> carrier
; 2. key1 / key2 -> c/m ratio
; 3. velocity -> fm index
; 4. time delta -> gestural rhythm

(define* (fmgesture key1 key2 rhy vel (ord 1) (bot 21) (top 108))
  (let* ((fmrat (if (> key2 key1)
                  (/ (hertz key2) (hertz key1))
                  (/ (hertz key1) (hertz key2))))
         (fmind (rescale vel 30 120 2 5))
         (spec (fm-spectrum (hertz key2) fmrat fmind)))
    (spectrum-keys spec :quant 1 :order ord
                   :min bot :max top :unique #t)))

(fmgesture 48 60 .1 60)

; NoteOn receiver takes two notes and generates a boogie-woogie
; accompanyment figure below it.

(define lastdata #f)

(define (myboogie mididata) 
  (if (not lastdata) ; store time and keynum
      (set! lastdata (list (now) (third mididata)))
      (let* ((k1 (second lastdata))
             (k2 (third mididata))
             (vel (fourth mididata))
             (lowest (min k1 k2))
             (delta (max (- (now) (first lastdata)) .2))
             (gest (fmgesture k1 k2 #f vel 1 :top lowest)))
        (set! lastdata #f)
        ;; boogie woogie figure goes up and down
        (set! gest (concat gest (rest (reverse gest))))
        ;; send the gesture
        (loop for n in gest 
              for i from delta by (* delta 2)
              do
              (mp:midi i .5 n vel)
              (mp:midi (+ i delta) .5 (+ n 12) vel))
        )))

(mp:receive mm:on myboogie)

(mp:receive mm:on #f)
