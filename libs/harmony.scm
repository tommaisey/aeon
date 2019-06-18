
#!chezscheme ;; Needed for symbols like 5th

(library (harmony)
  (export
    5th triad sus2 sus4 6th 7th 9th 11th 13th
    5th-raw triad-raw sus2-raw sus4-raw 6th-raw 7th-raw 9th-raw 11th-raw
    
    minor major harm-minor pent-neutral pent-major pent-minor
    blues dorian phrygian lydian mixolydian locrian wholeTone
    chromatic arabicA arabicB japanese ryukyu spanish
    I II III IV V VI VII VIII IX X XI XII
    Ab A A+ Bb B C C+ Db D D+ Eb E F F+ Gb G G+

    :tuning :octave
    :root :midinote
    :scale :scale-degree
    :chord-degree
    :scd :chd :chs

    def-chord-shape
    process-event-freq
    chord-offset)

  (import (chezscheme)
          (utilities)
          (event)
          (chain-nodes)
          (basic-nodes))

  (define (midicps midi freqA)
    (* freqA (expt 2 (/ (- midi 69) 12))))

  ;; TODO: get defaults for :octave, :scale, :chord-shape etc from context defaults.
  (define (process-event-freq e)
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
           [oct-overflow (trunc-int (/ sc-deg sc-len))]
           [semitone-offset (list-nth scale sc-idx)])
      ; (println (format "sc-idx: ~A, sc-deg: ~A, scale: ~A, semitone: ~A" sc-idx sc-deg scale semitone-offset))
      ; (println (format "oct-overflow: ~A" oct-overflow))
      (+ semitone-offset
         (* oct-overflow 12)
         (* octave 12))))

  ;; Event key definitions
  (declare-keyword :octave)
  (declare-keyword :midinote)
  (declare-keyword :root)
  (declare-keyword :scale)
  (declare-keyword :tuning)
  (declare-keyword :chord-shape)
  (declare-keyword :scale-degree)
  (declare-keyword :chord-degree)
  (define :scd :scale-degree)
  (define :chd :chord-degree)
  (define :chs :chord-shape)

  ;; Note name and scale numeral definitions
  (define C  0)
  (define C+ 1)
  (define Db 1)
  (define D  2)
  (define D+ 3)
  (define Eb 3)
  (define E  4)
  (define F  5)
  (define F+ 6)
  (define Gb 6)
  (define G  7)
  (define G+ 8)
  (define Ab 8)
  (define A  9)
  (define A+ 10)
  (define Bb 10)
  (define B  11)

  (define I    0)
  (define II   1)
  (define III  2)
  (define IV   3)
  (define V    4)
  (define VI   5)
  (define VII  6)
  (define VIII 7)
  (define IX   8)
  (define X    9)
  (define XI   10)
  (define XII  11)

  ;; Used for chords and scales
  (define-record-type shape
    (fields (immutable name) ;; symbol
            (immutable degrees) ;; list
            (immutable len)))  ;; number

  (define-syntax def-chord-shape
    (lambda (x)
      (syntax-case x ()
        ((_ name (a b ...))
         (with-syntax ([name-raw (gen-id #'name #'name "-raw")])
           #'(begin
               (define name
                 (+-> (to: :chd a)
                      (to: :chd b) ...))
               (define name-raw (list a b ...))))))))

  ;; Chord shape definitions (in degrees of current scale)
  (def-chord-shape 5th   (0 4))
  (def-chord-shape triad (0 2 4))
  (def-chord-shape sus2  (0 3 4))
  (def-chord-shape sus4  (0 3 4))
  (def-chord-shape 6th   (0 2 4 5))
  (def-chord-shape 7th   (0 2 4 6))
  (def-chord-shape 9th   (0 2 4 8))
  (def-chord-shape 11th  (0 2 4 10))
  (def-chord-shape 13th  (0 2 4 12))

  (define-syntax def-scale
    (syntax-rules ()
      ((_ name lst)
       (define name (make-shape 'name 'lst (length 'lst))))))

  ;; Scale shape definitions
  (def-scale major        (0 2 4 5 7 9 11))
  (def-scale minor        (0 2 3 5 7 8 10))
  (def-scale harm-minor   (0 2 3 5 7 8 11))
  (def-scale pent-neutral (0 2 5 7 10))
  (def-scale pent-major   (0 2 4 7 9))
  (def-scale pent-minor   (0 3 5 7 10))
  (def-scale blues        (0 3 5 6 7 10))
  (def-scale dorian       (0 2 3 5 7 9 10))
  (def-scale phrygian     (0 1 3 5 7 8 10))
  (def-scale lydian       (0 2 4 6 7 9 11))
  (def-scale mixolydian   (0 2 4 5 7 9 10))
  (def-scale locrian      (0 1 3 5 6 8 10))
  (def-scale wholeTone    (0 2 4 6 8 10))
  (def-scale arabicA      (0 2 3 5 6 8 9 11))
  (def-scale arabicB      (0 2 4 5 6 8 10))
  (def-scale japanese     (0 4 6 7 11))
  (def-scale ryukyu       (0 4 5 7 11))
  (def-scale spanish      (0 1 3 4 5 6 8 10))
  (def-scale chromatic    (0 1 2 3 4 5 6 7 8 9 10 11)))
