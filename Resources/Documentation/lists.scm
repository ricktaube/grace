;
;; Working with lists
;

; To run examples put the cursor at the end of each expression and
; press Command-Return. Look in the console window for any output.

; A list holds data.  To create a list specify its elements inside '()
; or use the (list ) function. The following two commands create
; equivalent lists

'(60 62 63 65 67)

(list 60 62 63 65 67)

; Use the '() notation when you know the exact contents of the list.
; Use the (list) function when some elements need to be calculated
; (evaluated) before being placed in the list. This example generates
; a variant three-element list each time its evaluated

(list (random 100) (random 100) (random 100))

; Empty lists are also possible

'()

(list)

; Lists can hold numbers, symbols, strings etc., even other lists

(define notes '(c d e f g a b))

notes

(define chords '((c e g) (d f a)))

chords

; 'length' tells you how many elements there are in a list

(length notes)

(length chords)

; An empty list has zero elements

(length '())

; 'append' concatenates input lists into a single list

(append notes notes notes)

; Append expects all its inputs to be proper lists. this next example
; produces an error because the second argument (60) is not a list

(append notes 60 notes)

; This is correct

(append notes '(60) notes)

; 'concat' will concatenate lists and non-lists together
; to form one large list

(concat -99 '(a b c) 60 70 '() '(x y z))

; 'first' ... 'tenth' returns the element at that position

(first notes)

(third notes)

(seventh notes)

; You can nest calls to access sublist elements

(third (second chords))

; 'nth' returns the element at the specified index. Note that the
; index is ZERO based, that is, the first element is at index 0, the
; last element in the list is at length()-1 index

(nth notes 0)

(nth notes 1)

(nth notes (random 7))

(nth notes (- (length notes) 1))

; The '(var n)' notation can also be used to refernce items in a list

(notes 0)

(notes (random (length notes)) )

; The 'reverse' function reverses the elements in a list (duh)

(reverse notes)

(define palin '(a     m a n   a   p l a n   p a n a m a))

(append palin (reverse palin))

; 'butlast' returns all but the last element

(butlast notes)

(append palin (reverse (butlast palin)))

; 'pick' randomly selects an element from a list

(pick notes)

; 'shuffle' randomly scrambles the elements of a list

(shuffle notes)
