;; High-level tests of the pattern system, what you might call
;; 'integration tests' where we use multiple components together
;; at once, and check the output is as expected.
(module basic-ops-tests ()

  (import (chezscheme)
          (testing)
          (event)
          (context)
          (nodes-chains)
          (nodes-subdivide))

  ;;----------------------------------------------------------
  (testp "[in!] numeric 1" (make-arc 0 1)
    (in! 1)
    ((:beat 0 :sustain 1)))

  (testp "[in!] numeric 2" (make-arc 0 1)
    (in! 2)
    ((:beat 0 :sustain 1/2) (:beat 1/2 :sustain 1/2)))

  (testp "[in!] numeric 3" (make-arc 0 1)
    (in! 3)
    ((:beat 0) (:beat 1/3) (:beat 2/3)))

  (testp "[in!] arc positive" (make-arc 3/2 2)
    (in! 3)
    ((:beat 5/3)))

  (testp "[in!] arc positive end exclusivity" (make-arc 3/2 (+ 2 1/128))
    (in! 3)
    ((:beat 5/3) (:beat 2)))

  (testp "[in!] arc negative" (make-arc -1 0)
    (in! 3)
    ((:beat -1) (:beat -2/3) (:beat -1/3)))

  (testp "[in!] arc negative end exclusivity" (make-arc -4/3 (+ -1 1/128))
    (in! 3)
    ((:beat -4/3) (:beat -1)))

  ;;----------------------------------------------------------
  (testp "[in! over] basic" (make-arc 0 1)
    (in! (over 1 1))
    ((:beat 0)))

  (testp "[in! over] basic" (make-arc 0 1)
    (in! (over 1 [1 1]))
    ((:beat 0) (:beat 1/2)))

  (testp "[in! over] subdivide" (make-arc 0 1)
    (in! (over 1 [1 [1 1]]))
    ((:beat 0) (:beat 1/2 :sustain 1/4)
               (:beat 3/4 :sustain 1/4)))

  (testp "[in! over] subdivide numeric" (make-arc 0 1)
    (in! (over 1 [1 [1 2]]))
    ((:beat 0) (:beat 1/2 :sustain 1/4)
               (:beat 3/4 :sustain 1/8)
               (:beat 7/8 :sustain 1/8)))

  (testp "[in! over] subdivide arc positive" (make-arc 21/2 22/2)
    (in! (over 1 [1 [1 1]]))
    ((:beat 21/2 :sustain 1/4) (:beat 43/4 :sustain 1/4)))

  (testp "[in! over] subdivide arc negative" (make-arc -800 (+ -800 2/3))
    (in! (over 1 [1 [1 1]]))
    ((:beat -800 :sustain 1/2) (:beat (+ -800 1/2) :sustain 1/4)))

  ;;----------------------------------------------------------
  (testp "[in:] basic" (make-arc 0 1)
    (in: :scd I)
    ((:beat 0 :scd I)))

  (testp "[in: over] basic" (make-arc 0 1)
    (in: :scd (over 1 [I V]))
    ((:beat 0 :scd I) (:beat 1/2 :scd V)))

  (testp "[in: over] basic subdivision" (make-arc 0 1)
    (in: :scd (over 1 [[I II] V]))
    ((:beat 0 :scd I) (:beat 1/4 :scd II) (:beat 1/2 :scd V)))

  ;;----------------------------------------------------------
  (testp "[to:] basic" (make-arc 0 1)
    (o->
      (in! 2)
      (to: :scd V))
    ((:beat 0 :scd V) (:beat 1/2 :scd V)))

  (testp "[to:] basic subdivide" (make-arc 0 1)
    (o->
      (in! 2)
      (to: :scd (over 1 [V X])))
    ((:beat 0 :scd V) (:beat 1/2 :scd X)))

  (testp "[to:] multiple keys" (make-arc 0 1)
    (o->
      (in! 2)
      (to: :scd (over 1 [V X])
           :cutoff (over 1 [100 50])))
    ((:beat 0 :scd V :cutoff 100) (:beat 1/2 :scd X :cutoff 50)))

  (testp "[to:] over multiple events" (make-arc 0 1)
    (o->
      (in! 4)
      (to: :scd (over 1 [V X])
           :cutoff (over 1 [100 50])))
    ((:beat 0   :scd V :cutoff 100)
     (:beat 1/4 :scd V :cutoff 100)
     (:beat 2/4 :scd X :cutoff 50)
     (:beat 3/4 :scd X :cutoff 50)))

  ;;----------------------------------------------------------
  (testp "[sq:] modify" (make-arc 0 1)
    (o->
      (in! 4)
      (sq: (over 1 [(to: :scd I) (to: :scd V)])))
    ((:beat 0   :scd I)
     (:beat 1/4 :scd I)
     (:beat 2/4 :scd V)
     (:beat 3/4 :scd V)))

  (testp "[sq:] add" (make-arc 0 1)
    (o->
      (sq: (over 1 [(in! 4 (to: :scd I)) (in! 2 (to: :scd V))])))
    ((:beat 0   :scd I)
     (:beat 1/4 :scd I)
     (:beat 2/4 :scd V)))

  )