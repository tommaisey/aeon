#!chezscheme ;; Needed for symbols like 5th

(library (harmony)
  (export harmony-docs
          5th triad sus2 sus4 6th 7th 9th 11th 13th

          I II III IV V VI VII VIII IX X XI XII

          minor major harm-minor pent-neutral pent-major pent-minor
          blues dorian phrygian lydian mixolydian locrian wholeTone
          chromatic arabicA arabicB japanese ryukyu spanish

          :tuning :octave
          :root :midinote
          :scale 
          :scale-degree :chord-degree
          :scd :chd :chs

          process-event-freq
          chord-offset

          make-shape
          extend-shape)

  (import (chezscheme)
          (doc)
          (utilities)
          (midinotes)
          (event))

  (define (midicps midi freqA)
    (* freqA (expt 2 (/ (- midi 69) 12))))

  ;; Reads :octave :root :scale :scd and :chd to generate a :freq value.
  (define (process-event-freq e)
    (alist-let e ([freq ':freq #f]
                  [midi :midinote #f])
      (cond
        (freq e)
        (midi (event-set e ':freq (midicps midi 440)))
        (else
          (alist-let e ([oct :octave 0]
                        [root :root C]
                        [scale :scale minor]
                        [scd :scale-degree 0]
                        [chd :chord-degree 0])
            (let* ([s (chord-offset oct scd chd scale)]
                   [f (midicps (+ root s) 440)])
              (event-remove-multi (event-set e ':freq f)
                                  (list :scale :chord-shape :root))))))))

  ;; Gets the semitone offset of a single note in a chord, computed
  ;; from the desired octave offset, scale degree, chord degree and scale.
  ;; The result is an offset from the root's midinote, not a standalone midinote.
  (define (chord-offset octave root-scale-deg chord-deg scale)
    (let* ([sc-len (vector-length scale)]
           [sc-deg (+ root-scale-deg chord-deg)]
           [sc-idx (mod sc-deg sc-len)]
           [oct-pos (if (< sc-deg 0) (- sc-deg (- sc-len 1)) sc-deg)]
           [oct-overflow (trunc-int (/ oct-pos sc-len))]
           [semitone-offset (vector-ref scale sc-idx)])
      ; (println (format "sc-idx: ~A, sc-deg: ~A, scale: ~A, semitone: ~A" 
      ;           sc-idx sc-deg scale semitone-offset))
      ; (println (format "oct-overflow: ~A" oct-overflow))
      (+ semitone-offset
         (* oct-overflow 12)
         (* octave 12))))

  ;; Event key definitions
  (declare-keywords :octave :midinote
                    :root :scale :tuning
                    :chord-shape
                    :scale-degree
                    :chord-degree)
  (alias :scd :scale-degree)
  (alias :chd :chord-degree)
  (alias :chs :chord-shape)

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
  (define XIII 12)

  ;; Give this a name in case we want to change its representation in future.
  (alias make-shape vector)

  (define (extend-shape shape . extensions)
    (list->vector (append (vector->list shape) extensions)))

  ;; Chord shape definitions (in degrees of current scale)
  (define 5th   (make-shape I V))
  (define sus2  (make-shape I IV  V))
  (define sus4  (make-shape I IV  V))
  (define triad (make-shape I III V))
  (define 6th   (extend-shape triad VI))
  (define 7th   (extend-shape triad VII))
  (define 9th   (extend-shape triad IX))
  (define 11th  (extend-shape triad XI))
  (define 13th  (extend-shape triad XIII))

  ;; Scale shape definitions
  (define major        (make-shape 0 2 4 5 7 9 11))
  (define minor        (make-shape 0 2 3 5 7 8 10))
  (define harm-minor   (make-shape 0 2 3 5 7 8 11))
  (define pent-neutral (make-shape 0 2 5 7 10))
  (define pent-major   (make-shape 0 2 4 7 9))
  (define pent-minor   (make-shape 0 3 5 7 10))
  (define blues        (make-shape 0 3 5 6 7 10))
  (define dorian       (make-shape 0 2 3 5 7 9 10))
  (define phrygian     (make-shape 0 1 3 5 7 8 10))
  (define lydian       (make-shape 0 2 4 6 7 9 11))
  (define mixolydian   (make-shape 0 2 4 5 7 9 10))
  (define locrian      (make-shape 0 1 3 5 6 8 10))
  (define wholeTone    (make-shape 0 2 4 6 8 10))
  (define arabicA      (make-shape 0 2 3 5 6 8 9 11))
  (define arabicB      (make-shape 0 2 4 5 6 8 10))
  (define japanese     (make-shape 0 4 6 7 11))
  (define ryukyu       (make-shape 0 4 5 7 11))
  (define spanish      (make-shape 0 1 3 4 5 6 8 10))
  (define chromatic    (make-shape 0 1 2 3 4 5 6 7 8 9 10 11))

  ;;---------------------------------------------------------------
  (define harmony-key-msg 
    "A key whose value will be processed in tandem with several others to produce
a frequency. The keys in this family are:

:root, :scale, :scale-degree, :chord-degree, :octave, :tuning.")
  
  (make-doc harmony-docs
    (:root harmony-key-msg () ())
    (:scale harmony-key-msg () ())
    (:scale-degree harmony-key-msg () ())
    (:chord-degree harmony-key-msg () ())
    (:octave harmony-key-msg () ())
    (:tuning harmony-key-msg () ())

    (:scd (str+ "An alias for :scale-degree.\n" harmony-key-msg) () ())
    (:chd (str+ "An alias for :chord-degree.\n" harmony-key-msg) () ()))
  
  )
