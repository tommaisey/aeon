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
  (ptest "[in!] numeric 1" 0 1
         (in! 1)
         ((:beat 0 :sustain 1)))

  (ptest "[in!] numeric 2" 0 1
         (in! 2)
         ((:beat 0 :sustain 1/2) (:beat 1/2 :sustain 1/2)))

  (ptest "[in!] numeric 3" 0 1
         (in! 3)
         ((:beat 0) (:beat 1/3) (:beat 2/3)))

  (ptest "[in!] arc positive" 3/2 2
         (in! 3)
         ((:beat 5/3)))

  (ptest "[in!] arc positive end exclusivity" 3/2 (+ 2 1/128)
         (in! 3)
         ((:beat 5/3) (:beat 2)))

  (ptest "[in!] arc negative" -1 0
         (in! 3)
         ((:beat -1) (:beat -2/3) (:beat -1/3)))

  (ptest "[in!] arc negative end exclusivity" -4/3 (+ -1 1/128)
         (in! 3)
         ((:beat -4/3) (:beat -1)))

  ;;----------------------------------------------------------
  (ptest "[in! over] basic" 0 1
         (in! (over 1 1))
         ((:beat 0)))

  (ptest "[in! over] basic" 0 1
         (in! (over 1 [1 1]))
         ((:beat 0) (:beat 1/2)))

  (ptest "[in! over] subdivide" 0 1
         (in! (over 1 [1 [1 1]]))
         ((:beat 0) (:beat 1/2 :sustain 1/4)
                    (:beat 3/4 :sustain 1/4)))

  (ptest "[in! over] subdivide numeric" 0 1
         (in! (over 1 [1 [1 2]]))
         ((:beat 0) (:beat 1/2 :sustain 1/4)
                    (:beat 3/4 :sustain 1/8)
                    (:beat 7/8 :sustain 1/8)))

  (ptest "[in! over] subdivide arc positive" 21/2 22/2
         (in! (over 1 [1 [1 1]]))
         ((:beat 21/2 :sustain 1/4) (:beat 43/4 :sustain 1/4)))

  (ptest "[in! over] subdivide arc negative" -800 (+ -800 2/3)
         (in! (over 1 [1 [1 1]]))
         ((:beat -800 :sustain 1/2) (:beat (+ -800 1/2) :sustain 1/4)))

  ;;----------------------------------------------------------
  (ptest "[in:] basic" 0 1
         (in: :scd I)
         ((:beat 0 :scd I)))

  (ptest "[in: over] basic" 0 1
         (in: :scd (over 1 [I V]))
         ((:beat 0 :scd I) (:beat 1/2 :scd V)))

  (ptest "[in: over] basic subdivision" 0 1
         (in: :scd (over 1 [[I II] V]))
         ((:beat 0 :scd I) (:beat 1/4 :scd II) (:beat 1/2 :scd V)))

  ;;----------------------------------------------------------
  (ptest "[to:] basic" 0 1
         (o->
           (in! 2)
           (to: :scd V))
         ((:beat 0 :scd V) (:beat 1/2 :scd V)))

  (ptest "[to:] basic subdivide" 0 1
         (o->
           (in! 2)
           (to: :scd (over 1 [V X])))
         ((:beat 0 :scd V) (:beat 1/2 :scd X)))

  (ptest "[to:] multiple keys" 0 1
         (o->
           (in! 2)
           (to: :scd (over 1 [V X])
                :cutoff (over 1 [100 50])))
         ((:beat 0 :scd V :cutoff 100) (:beat 1/2 :scd X :cutoff 50)))

  (ptest "[to:] over multiple events" 0 1
         (o->
           (in! 4)
           (to: :scd (over 1 [V X])
                :cutoff (over 1 [100 50])))
         ((:beat 0   :scd V :cutoff 100)
          (:beat 1/4 :scd V :cutoff 100)
          (:beat 2/4 :scd X :cutoff 50)
          (:beat 3/4 :scd X :cutoff 50)))

  ;;----------------------------------------------------------
  (ptest "[sq:] modify" 0 1
         (o->
           (in! 4)
           (sq: (over 1 [(to: :scd I) (to: :scd V)])))
         ((:beat 0   :scd I)
          (:beat 1/4 :scd I)
          (:beat 2/4 :scd V)
          (:beat 3/4 :scd V)))

  (ptest "[sq:] add" 0 1
         (o->
           (sq: (over 1 [(in! 4 (to: :scd I)) (in! 2 (to: :scd V))])))
         ((:beat 0   :scd I)
          (:beat 1/4 :scd I)
          (:beat 2/4 :scd V)))

  )