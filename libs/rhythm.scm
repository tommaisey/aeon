;; -*- geiser-scheme-implementation: chez-*-
;;----------------------------------------------------------
;; Rhythm helper functions
;; ---------------------------------------------------------
(library (rhythm)
  (export
   snap-next
   snap-prev
   euc)

  (import (chezscheme) (utilities))

  ;; Snap a value to the next number divisible by divisor,
  ;; if `beat` isn't already cleanly divisible by divisor.
  (define (snap-next beat divisor)
    (let ([overlap (mod beat divisor)])
      (if (zero? overlap) beat (+ (- divisor overlap) beat))))

  ;; Snap a value to the previous number divisible by divisor,
  ;; if 'beat' isn't already cleanly divisible by divisor
  (define (snap-prev beat divisor)
    (let ([overlap (mod beat divisor)])
      (- beat overlap)))


  ;;-----------------------------------------------------------------
  ;; Euclidean patterns
  ;;-----------------------------------------------------------------

  ;; non-iterative derivation of this algorithm:
  ;; http://computermusicdesign.com/simplest-euclidean-rhythm-algorithm-explained/
  (define (euc-hit? step num-steps num-hits offset)
    ;; put bucket in state as if it had done N = offset iterations
    ;; compute state of bucket just before the requested step
    (let* ([offset (mod (add1 offset) num-steps)]
           [bucket (mod (* num-hits (- num-steps offset)) num-steps)]
           [bucket (+ bucket (* num-hits (mod step num-steps)))])
      (not (eqv? (quotient bucket num-steps)
                 (quotient (+ bucket num-hits) num-steps)))))

  (define euc
    (case-lambda
      ((num-steps num-hits offset value)
       (map (lambda (i) (if (euc-hit? i num-steps num-hits offset) value '~))
            (iota num-steps)))

      ((num-steps num-hits offset)
       (euc num-steps num-hits offset 1))))
  
  
  ); end module 'rhythm'
