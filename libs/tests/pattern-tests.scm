(module pattern-tests ()

  (import (chezscheme)
          (testing)
          (event)
          (context)
          (nodes-chains)
          (nodes-subdivide))

  
  (define (test-event e expected-values-alist)
    (for-all (lambda (kv) (event-check e (car kv) eqv? (cdr kv)))
             expected-values-alist))

  (define (test-events context expected-values-alists)
    (let ([next-events (context-events-next context)]
          [expected expected-values-alists])
      (if (or (null? next-events) (null? expected))
          (and (null? next-events) (null? expected))
          (and (test-event (car next-events) (car expected))
               (test-events (context-move context 1) (cdr expected))))))

  ;; Test that a pattern has events with the expected values in the arc.
  ;; The events-props form is a list of flattened key/value pairs.
  ;; Note that the result can contain extra key/values to the ones tested,
  ;; but if it's missing any of the specified events-props values it will fail.
  (define-syntax test
    (syntax-rules ()
      ((_ name start end pattern-expr ((events-props ...) ...))
       (let ([expected (list (pairwise (list events-props ...)) ...)])
         (test-impl name result (render-arc pattern-expr start end)
                    (test-events result expected)
                    (let* ([actual-events (context-events-next result)]
                           [len-expected (length expected)]
                           [len-actual (length actual-events)]
                           [extra-msg (cond
                                        ((not (eqv? len-expected len-actual))
                                         (format "Expected ~A events, but got ~A."
                                                 len-expected len-actual))
                                        (else ""))])
                      (string-append "Failed: " name ". " extra-msg "\n"
                                     (format "Expected: ~A\nGot:      ~A\nCode:"
                                             expected actual-events))))))))
  (test "[in!] basic" 0 1
        (in! 1)
        ((:beat 0 :sustain 1)))

  (test "[in!] basic" 0 1
        (in! 2)
        ((:beat 0 :sustain 1/2) (:beat 1/2 :sustain 1/2)))

  (test "[in!] basic" 0 1
        (in! 3)
        ((:beat 0) (:beat 1/3) (:beat 2/3)))

  (test "[in!] arc positive" 3/2 2
        (in! 3)
        ((:beat 5/3)))

  (test "[in!] arc positive end is exclusive" 3/2 (+ 2 1/128)
        (in! 3)
        ((:beat 5/3) (:beat 2)))

  (test "[in!] arc negative" -1 0
        (in! 3)
        ((:beat -1) (:beat -2/3) (:beat -1/3)))

  (test "[in!] arc negative end is exclusive" -4/3 (+ -1 1/128)
        (in! 3)
        ((:beat -4/3) (:beat -1)))

  ;;--------------------------------
  (test "[in! over] basic" 0 1
        (in! (over 1 1))
        ((:beat 0)))

  (test "[in! over] basic" 0 1
        (in! (over 1 [1 1]))
        ((:beat 0) (:beat 1/2)))

  (test "[in! over] subdivide" 0 1
        (in! (over 1 [1 [1 1]]))
        ((:beat 0) (:beat 1/2 :sustain 1/4) 
                   (:beat 3/4 :sustain 1/4)))

  (test "[in! over] subdivide numeric" 0 1
        (in! (over 1 [1 [1 2]]))
        ((:beat 0) (:beat 1/2 :sustain 1/4) 
                   (:beat 3/4 :sustain 1/8) 
                   (:beat 7/8 :sustain 1/8)))

  (test "[in! over] subdivide arc positive" 21/2 22/2
        (in! (over 1 [1 [1 1]]))
        ((:beat 21/2 :sustain 1/4) (:beat 43/4 :sustain 1/4)))

  (test "[in! over] subdivide arc negative" -800 (+ -800 2/3)
        (in! (over 1 [1 [1 1]]))
        ((:beat -800 :sustain 1/2) (:beat (+ -800 1/2) :sustain 1/4)))

  ;;--------------------------------

  (test "[in:] basic" 0 1
        (in: :scd I)
        ((:beat 0 :scd I)))

  (test "[in: over] basic" 0 1
        (in: :scd (over 1 [I V]))
        ((:beat 0 :scd I) (:beat 1/2 :scd V)))

  )