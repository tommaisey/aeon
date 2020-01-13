(library (arc)
  (export arc arc?
          make-arc
          arc-with-start
          arc-with-end
          arc-start
          arc-end
          arc-length
          arc-negate
          arc-add
          arc-math
          arc-widen
          arc-correct
          arc-valid?
          arc-eq?
          arc-contains?
          within-arc?
          arcs-overlap?)

  (import (chezscheme) (utilities))

  ;;--------------------------------------------------
  ;; A range of time (in beats)
  (define-record-type arc
    (fields (immutable start)
            (immutable end)))

  (define (arc-with-start a new-start)
    (arc-correct
     (make-arc new-start (arc-end a))))
  
  (define (arc-with-end a new-end)
    (arc-correct
     (make-arc (arc-start a) new-end)))
  
  (define (arc-valid? a)
    (< (arc-start a) (arc-end a)))
  
  (define (arc-eq? a b)
    (and (eqv? (arc-start a) (arc-start b))
         (eqv? (arc-end a) (arc-end b))))
  
  (define (within-arc? a t)
    (between-inclusive t (arc-start a) (arc-end a)))
  
  (define (arc-contains? a b) ;; two arcs
    (and (within-arc? a (arc-start b))
         (within-arc? a (arc-end b))))
  
  (define (arcs-overlap? a b) ;; two arcs
    (or (within-arc? b (arc-start a))
        (within-arc? b (arc-end a))
        (within-arc? a (arc-start b))
        (within-arc? a (arc-end b))))
  
  (define (arc-length a)
    (- (arc-end a) (arc-start a)))
  
  (define (arc-negate a)
    (make-arc (- (arc-start a)) (- (arc-end a))))
  
  (define (arc-add a1 a2)
    (arc-correct
     (make-arc (+ (arc-start a1) (arc-start a2))
               (+ (arc-end a1) (arc-end a2)))))
  
  (define (arc-math a proc val)
    (arc-correct
     (make-arc (proc (arc-start a) val) (proc (arc-end a) val))))
  
  (define (arc-widen a val)
    (arc-correct
     (arc-add a (make-arc (- val) val))))
  
  (define (arc-correct a)
    (if (arc-valid? a) a (make-arc (arc-end a) (arc-start a))))

  )