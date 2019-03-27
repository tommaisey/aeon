#!chezscheme ;; Needed for symbols like 5th

(library (harmony)
  (export
   5th triad sus2 sus4 6th 7th 9th 11th 13th
   minor major harmMinor pentNeutral pentMajor pentMinor
   blues dorian phrygian lydian mixolydian locrian wholeTone
   chromatic arabicA arabicB japanese ryukyu spanish
   I II III IV V VI VII VIII IX X XI XII
   Ab A A+ Bb B C C+ Db D D+ Eb E F F+ Gb G G+

   :tuning :octave
   :root :midinote
   :scale :scale-degree
   :chord-degree
   :scd :chd :chs

   event-with-freq
   chord-offset)

  (import (scheme) (utilities) (event) (chain-nodes) (logic-nodes))

  (define (midicps midi freqA)
    (* freqA (expt 2 (/ (- midi 69) 12))))

  ;; TODO: get defaults for :octave, :scale, :chord-shape etc from context defaults.
  (define (event-with-freq e)
    (alist-let
     e ([freq ':freq #f]
	[midi :midinote #f])
     (cond
      (freq e)
      (midi (event-set e ':freq (midicps midi 440)))
      (else
       (alist-let
	e ([oct :octave 0]
	   [root :root C]
	   [scale :scale minor]
	   [sc-deg :scale-degree I]
	   [ch-deg :chord-degree I])
	(let* ([s (chord-offset oct sc-deg ch-deg scale)]
	       [f (midicps (+ 60 root s) 440)])
	  (event-remove-multi (event-set e ':freq f)
			      (list :scale :chord-shape :octave :root))))))))

  ;; Gets the semitone offset of a single note in a chord, computed
  ;; from the desired octave offset, scale degree, chord degree and scale.
  ;; The result is just an offset from the root's midinote, not a real midinote itself.
  (define (chord-offset octave root-scale-deg chord-deg scale)
    (let* ([sc-len (shape-len scale)]
	   [scale  (shape-degrees scale)]
	   [sc-deg (+ root-scale-deg chord-deg)]
	   [sc-idx (mod sc-deg sc-len)]
	   [oct-overflow (exact (truncate (/ sc-deg sc-len)))]
	   [semitone-offset (list-nth scale sc-idx)])
      ;; (println (format "sc-idx: ~A, sc-deg: ~A, scale: ~A, semitone: ~A" sc-idx sc-deg scale semitone))
      ;; (println (format "oct-overflow: ~A" oct-overflow))
      (+ semitone-offset
	 (* oct-overflow 12)
	 (* octave 12))))
  
  ;; Event key definitions
  (define :octave ':octave)
  (define :midinote ':midinote)
  (define :root ':root)
  (define :scale ':scale)
  (define :tuning ':tuning)
  (define :chord-shape ':chord-shape)
  (define :scale-degree ':scale-degree)
  (define :chord-degree ':chord-degree)
  (define :scd :scale-degree)
  (define :chd :chord-degree)
  (define :chs :chord-shape)

  ;; Note name and scale numeral definitions
  (define Gb -6)
  (define G  -5)
  (define G+ -4)
  (define Ab -4)
  (define A  -3)
  (define A+ -2)
  (define Bb -2)
  (define B  -1)
  (define C  0)
  (define C+ 1)
  (define Db 1)
  (define D  2)
  (define D+ 3)
  (define Eb 3)
  (define E  4)
  (define F  5)
  (define F+ 6)

  (define I    0)
  (define II   1)
  (define III  2)
  (define IV   3)
  (define V    4)
  (define VI   5)
  (define VII  6)
  (define VIII 7)
  (define IX  8)
  (define X    9)
  (define XI   10)
  (define XII  11)

  ;; Used for chords and scales
  (define-record-type shape
    (fields (immutable name) ;; symbol
	    (immutable degrees) ;; list
	    (immutable len)))  ;; number

  (define-syntax def-shape
    (syntax-rules ()
      ((_ name (a b ...))
       (define name
	 (+-> (to: :chd a)
	      (to: :chd b) ...)))))

  ;; Chord shape definitions (in degrees of current scale)
  (def-shape 5th   (0 4))
  (def-shape triad (0 2 4))
  (def-shape sus2  (0 3 4)) ;; TODO: what should this be? placeholder
  (def-shape sus4  (0 3 4))
  (def-shape 6th   (0 2 4 5))
  (def-shape 7th   (0 2 4 6))
  (def-shape 9th   (0 2 4 8))
  (def-shape 11th  (0 2 4 10))
  (def-shape 13th  (0 2 4 12))

  (define-syntax def-scale
    (syntax-rules ()
      ((_ name lst)
       (define name (make-shape 'name 'lst (length 'lst))))))

  ;; Scale shape definitions
  (def-scale major       (0 2 4 5 7 9 11))
  (def-scale minor       (0 2 4 5 7 8 10))
  (def-scale harmMinor   (0 2 4 5 7 8 11))
  (def-scale pentNeutral (0 2 5 7 10))
  (def-scale pentMajor   (0 2 4 7 9))
  (def-scale pentMinor   (0 3 5 7 10))
  (def-scale blues       (0 3 5 6 7 10))
  (def-scale dorian      (0 2 3 5 7 9 10))
  (def-scale phrygian    (0 1 3 5 7 8 10))
  (def-scale lydian      (0 2 4 6 7 9 11))
  (def-scale mixolydian  (0 2 4 5 7 9 10))
  (def-scale locrian     (0 1 3 5 6 8 10))
  (def-scale wholeTone   (0 2 4 6 8 10))
  (def-scale arabicA     (0 2 3 5 6 8 9 11))
  (def-scale arabicB     (0 2 4 5 6 8 10))
  (def-scale japanese    (0 4 6 7 11))
  (def-scale ryukyu      (0 4 5 7 11))
  (def-scale spanish     (0 1 3 4 5 6 8 10))
  (def-scale chromatic   (0 1 2 3 4 5 6 7 8 9 10 11)))
