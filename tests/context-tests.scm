(module context-tests ()

  (import (chezscheme)
          (testing)
          (context)
          (context-render))

  (testp  "[context-map]" (make-arc 3.3 4)
          (part
            (in! 4)
            (lambda (c)
              (context-map
               (lambda (c) (event-set (context-event c) :freq 99))
               (context-resolve c))))
         ((:beat 14/4 :freq 99)
          (:beat 15/4 :freq 99)))

  (testp  "[context-filter]" (make-arc 3 4)
          (part
            (in! 4)
            (to: :freq (over 1/2 [100 500]))
            (lambda (c)
              (context-filter
               (lambda (c) (> (event-get (context-event c) :freq -80) 200))
               (context-resolve c))))
         ((:beat 13/4 :freq 500)
          (:beat 15/4 :freq 500)))

  (test-eq "[context-to-event-after]" 2/3
    (event-beat
     (context-event (context-to-event-after
                     (render-arc (in! 3) 0 1) 1/2))))

  (test-eq "[context-to-event-after]" 4
    (event-beat
     (context-event (context-to-event-after
                     (render-arc (in! 2) 3 5) 3.51))))

  )
