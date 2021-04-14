;;----------------------------------------------------------------------
;; Functions which might return different value based on the context they
;; are passed, i.e. 'contextual' values. This lets us maintain the
;; referential transparency that is key for the system to work.
;;
;; They form the 'leaves' of a tree of functions defining musical patterns.
;;
;; Many of these functions base their value on the time/beat of the current
;; event in the context, unless passed one or more extra keys to look at.
;;
;; Others of these functions don't really need a context - but may want to
;; treat _their_ arguments as callable leaves (which *may* need a context).
;;
;; Many of these actually return a special seq-meta object, a function wrapped
;; with some metadata that may be needed to evaluate the graph accurately.
;;----------------------------------------------------------------------
(library (nodes-continuous)
  (export
   ? rnd wpick pick sine)

  (import
    (chezscheme)
    (utilities)
    (rename (matchable) (? ??))
    (context) (event)
    (node-eval)
    (for (pdef) expand))

  (define default-seed 99)

  ;;-------------------------------------------------------------------
  ;; Pseudo-random values and choices.
  (define rnd
    (case-lambda
      [() (rnd '())]
      [(key) (rnd 0.0 1.0 key)]
      [(min max) (rnd min max '())]
      [(min max seed)
       (seq-meta-ranged (list min max)
                        (lambda (context)
                          (let ([min (eval-seq min context)]
                                [max (eval-seq max context)]
                                [seed (trunc-int (* seed (context-now context)))])
                            (pseudo-rand min max seed))))]))

  ;; Choose from a list randomly
  (define-syntax pick
    (syntax-rules ()
      ((_ vals) (pick vals default-seed))

      ((_ vals seed)
       (let* ([lst (pdef vals)]
              [len (length lst)]
              [rgen (rnd 0 len seed)])
         (seq-meta-ranged
          lst
          (lambda (context)
            (let ([choice (eval-seq rgen context)])
              (eval-seq (list-ref lst choice) context))))))))

  ;; Choose from a list randomly, with weightings
  (define-syntax wpick
    (syntax-rules ()
      ((_ vals weights) (wpick vals weights default-seed))

      ((_ vals weights seed)

       (let ([lst (pdef vals)] [wts (pdef weights)])
         (define (to-cumulative lst)
           (let loop ([count 0] [o '()] [lst lst])
             (match lst
               [() (reverse o)]
               [(a . b) (loop (+ count a) (cons (+ count a) o) b)])))

         (define (index-picker cumulative-weights)
           (let ([len (length cumulative-weights)])
             (lambda (v)
               (or (list-index (lambda (x) (>= x v)) cumulative-weights)
                   (- len 1)))))

         (when (or (not (unsafe-list? lst)) (not (unsafe-list? wts))
                   (null? lst) (null? wts))
           (error 'wpick "requires 2 lists" lst wts))

         (let* ([len (length lst)]
                [wts (extend-repeating-last wts len)]
                [cumulative-weights (to-cumulative wts)]
                [top (list-last cumulative-weights)]
                [rgen (rnd 0 top seed)]
                [picker (index-picker cumulative-weights)])
           (seq-meta-ranged
            lst
            (lambda (context)
              (let* ([v (eval-seq rgen context)])
                (eval-seq (list-ref lst (picker v)) context)))))))))

  ;; This short operator picks either rnd, pick or wpick depending
  ;; on the arguments. Two lists gets wpick, one list gets pick, and
  ;; two non-lists gets rnd. Any other combination of args is illegal.
  (define-syntax ?
    (syntax-rules ()
      ((_ a b seed)
       (let ([ad (pdef a)] [bd (pdef b)])
         (unless (integer? seed)
           (error '? "3rd arg must be an integer seed" ad bd seed))
         (if (unsafe-list? ad)
             (if (unsafe-list? bd)
                 (wpick ad bd seed)
                 (error '? "2nd arg must be a list if 1st is" ad bd))
             (rnd ad bd seed))))

      ((_ [a b ...] seed)
       (if (integer? seed)
           (pick [a b ...] seed)
           (? [a b ...] seed)))

      ((_ [a b ...]) (pick [a b ...]))

      ((_ a b) (? a b default-seed))

      ((_ args ...)
       (error '? "invalid number of args" 'args ...))))

  ;; Tag so pdef recognises as a macro
  (tag-pdef-callable pick)
  (tag-pdef-callable wpick)
  (tag-pdef-callable ?)

  ;;--------------------------------------------------------------------
  ;; Rhythmic & sequencing operations.
  (define (sine freq lo hi)
    (seq-meta-ranged
     (list lo hi)
     (lambda (context)
       (let ([f (eval-seq freq context)]
             [l (eval-seq lo context)]
             [h (eval-seq hi context)])
         (range-sine f l h (context-now context))))))

  ;; Helper for `this`, `next` and `nearest`.
  (define (get c key default)
    (event-get (context-event c) key default))

  )
