(module time-ops-tests ()

  (import (chezscheme)
          (testing)
          (event)
          (context)
          (nodes-chains)
          (nodes-subdivide))

  ;;----------------------------------------------------------
  ;; [tt+] and [tt-] simple tests
  (ptest "[tt+] 1/4" 0 1
         (o->
           (in! 1)
           (tt+ 1/4))
         ((:beat 1/4 :sustain 1)))

  (ptest "[tt+] -1/4" -1 0
         (o->
           (in! 1)
           (tt+ 1/4))
         ((:beat -3/4 :sustain 1)))

  (ptest "[tt-] 1/4" -1 0
         (o->
           (in! 1)
           (tt- 1/4))
         ((:beat -1/4 :sustain 1)))

  (ptest "[tt-] -1/4" 0 1
         (o->
           (in! 1)
           (tt- -1/4))
         ((:beat 1/4 :sustain 1)))

  (ptest "[tt+] followed by [to:]" 0 1
         (o->
           (in! 1)
           (tt+ 1/4)
           (to: :scd (over 1/2 [I V])))
         ((:beat 1/4 :sustain 1 :scd V)))

  ;;----------------------------------------------------------
  ;; [tt*] simple tests
  (ptest "[tt*] simple" 0 1
         (o->
           (in! (over [1 ~]))
           (tt* 1))
         ((:beat 0)))

  (ptest "[tt*] simple" 0 1
         (o->
           (in! (over [1 ~]))
           (tt* 1/2))
         ((:beat 0)
          (:beat 1/2)))

  (ptest "[tt*] simple" 0 1
         (o->
           (in! (over [1 ~]))
           (tt* 1/3))
         ((:beat 0)
          (:beat 1/3)
          (:beat 2/3)))

  (ptest "[tt*] simple negative" 0 1
         (o->
           (in! (over [1 [1 1]]))
           (tt* -1))
         ((:beat 0)
          (:beat 1/4)
          (:beat 1/2)))

  (ptest "[tt*] simple negative" 0 1
         (o->
           (in! (over [1 [1 1]]))
           (tt* -3/4))
         ((:beat 0)
          (:beat 3/16)
          (:beat 3/8)
          (:beat 3/4)
          (:beat 15/16)))

  ;;----------------------------------------------------------
  ;; [tt*] followed by [to:]
  (ptest "[tt*] positive followed by [to:]" 0 1
         (o->
           (in! (over [1 ~ [1 1]]))
           (tt* 1/2)
           (to: :scd (over 1 [V X])))
         ((:beat 0     :scd V)
          (:beat 1/3   :scd V)
          (:beat 5/12  :scd V)
          (:beat 1/2   :scd X)
          (:beat 5/6   :scd X)
          (:beat 11/12 :scd X)))

  (ptest "[tt*] negative followed by [to:]" 0 1
         (o->
           (in! (over [1 ~ [1 1]]))
           (tt* -1/2)
           (to: :scd (over 1 [V X])))
         ((:beat 0     :scd V)
          (:beat 1/12  :scd V)
          (:beat 1/6   :scd V)
          (:beat 1/2   :scd X)
          (:beat 7/12  :scd X)
          (:beat 2/3   :scd X)))

  (for-each (lambda (n)
     (ptest "[tt*] negative followed by [to:]" n (+ n 1)
            (o->
              (in! (over [1 ~ [1 1]]))
              (tt* -1/2)
              (to: :scd (over 1 [V X])))
            ((:beat (+ n 0)     :scd V)
             (:beat (+ n 1/12)  :scd V)
             (:beat (+ n 1/6)   :scd V)
             (:beat (+ n 1/2)   :scd X)
             (:beat (+ n 7/12)  :scd X)
             (:beat (+ n 2/3)   :scd X))))
       (iota 20))

  )