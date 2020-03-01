;
;; Sending MIDI Output
;

; To run examples put the cursor at the end of each expression and
; press Command-Return. Look in the console window for any output.

; These examples demonstrate how to send data to the MIDI output port.
; If no MIDI output device is visible in the Audio menu, use the Midi
; Devices dialog to open one of the available ports. Once a port is
; open test it using the Test Output command (Command-T) in its menu.

; The mp:midi function sends midi data to the open midi port.
; Following the name of the function comes the data you want to
; send. mp:midi allows up to five (optional) values to be specified.

(mp:midi 0 .5 60 .5 0)

; The first value is 'time', the start time of the note in seconds,
; where 0 means to play the note immediately. The second value is
; 'dur', the duration of the note in seconds. The third value is
; 'key', the MIDI key number of the note. The fourth value is 'amp',
; usually a value between 0.0 (no sound) and 1.0 (max sound). The last
; value is 'chan', the MIDI channel of the note, an integer from 0 to
; 15 inclusive (not 1 to 16!)

; The values you send all have defaults, which means that if you don't
; specify a value for a parameter you get the default value (shown in
; first example)

(mp:midi)

; To play c5 starting 1 second in the future and lasting for 3 seconds
; you would do

(mp:midi 1 3 72)

; If you just want to change one or two parameters from their defaults
; consider using the names of the parameters. Named parameters can
; appear in any order:

(mp:midi :amp .9 :key 71)

; Of course, values can be expressions, not just numbers.  The next
; example sends a randomly chosen key between 50 and 80 that lasts
; either .1, .5 or 2 seconds. Try evaluating the line several times to
; listen to different choices.

(mp:midi :key (between 50 80) :dur (pick .1 .5 2))

; You can send messages in the future by providing the appropriate
; future time stamp to each note. For example this loop sends 8
; random notes, all but the first are sent in the future:

(loop repeat 8
      for t from 0 by .125
      do
      (mp:midi t :key (between 40 90)))

; To send a chord, use a loop to output each note in the chord

(loop with chord = (transpose (pick '(0 3 7)
                                    '(0 4 7)
                                    '(0 3 7 10)
                                    '(0 4 7 10)
                                    '(0 4 7 11))
                              (between 48 80))
      for k in chord
      do (mp:midi :key k))

;
;; Midi Instrument Assignment
;

; You can use the "mp:instruments" function to send up to 16
; instrument assignments on channels 0 to 15. This example sets
; channels 0 to 4 with flute, violin, guitar, marimba and harp

(mp:instruments 73 40 24 12 46)

; Send a few middle C's to instruments in the quintet

(mp:midi :key 60 :chan (pick 0 1 2 3 4))

(mp:midi :key (between 48 80) :chan (pick 0 1 2 3 4))

; Now reset the five channels back to Grand Piano
; flute, violin, guitar, marimba, harp and bass

(mp:instruments 0 0 0 0 0)

; For more dynamic note-to-notecontrol over instrumentation use the
; mp:prog (program change) function. This next example sends a random
; instrument assignment on channel 0 (the default channel) each time
; it sends a sound, and picks only black keys to play.

(let* ((myoctave (* 12 (between 3 9)))
       (blackkey (+ myoctave (pick 1 3 6 8 10))))
  (mp:prog :val (random 16))
  (mp:midi :key blackkey)
  )

; Now reset the default sound to piano.

(mp:prog :val 0)

;
;; Microtonal Output
; 
; In order to make microtonal sound using the MIDI port you need to do
; two things: (1) Set your Midi Out port to a micotonal resolution and
; (2) Send floating point key numbers.

; You can use the Ports>Midi Out>Microtones> submenu to set your port
; to a Microtonal resoution your choice (see Help>Ports for more
; information about this) or use the "mp:tuning" message in a send
; expression. Th value you send to tuning is the number of divisions
; per semitones, so 2 puts the port into Quarter tone tuning:

(mp:tuning 2)

; Once you have "tuned" your port to a microtonal resolution you can
; generate microtonal output simply by sending floating point key
; numbers in your data. Recall that Common Music interpets the
; floating point key number kkk.cc as the frequency that is cc cents
; above the key number kkk. So 60.5 means one quarter-tone above middle C:

(mp:midi :key 60)

(mp:midi :key 60.5)

(mp:midi :key 61)

; The floating point key values you send are always quantized to the
; specific microtonal resolution that you set in your port. So even
; the next example moves 10 cents each note (.10) it resolves
; to the nearest quarter tone set by our mp:tuning(2) above

(loop for i from 0.0 to 1.0 by .10
      for j from 0 by .5
      do
      (mp:midi :time j :key (+ 60 i))
      )

; Now tune semitones into 14 parts and try the loop again, since 14
; quantizes to about 7 cent steps, you will hear much better
; resolution of the 10 cent steps in the data:

(mp:tuning 14)

(loop for i from 0.0 to 1.0 by .1
      for j from 0 by .5
      do
      (mp:midi :time j :key (+ 60  i))
      )

; Here's a little loop that generates the harmonics series. it
; converts a fundamenal key into hertz, multiplies it by the harmonic,
; and converts it back into a floating point key number for midi to
; play!

(loop with fund = (hertz 36)
      for harm from 1 to 16
      for time from 0 by .5
      do
      (mp:midi :time time :key (keynum (* fund harm)))
      )

; Now let's set the port back to semitone tuning:

(mp:tuning 1)

