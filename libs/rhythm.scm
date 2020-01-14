;; -*- geiser-scheme-implementation: chez-*-
;;----------------------------------------------------------
;; Rhythm helper functions
;; ---------------------------------------------------------
(library (rhythm)
  (export rhythm-docs euc)

  (import (chezscheme) (utilities) (doc))

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
  
  (define* (euc num-steps num-hits [/opt (offset 0) (on-val 1) (off-val '~)])
    (map (lambda (i) (if (euc-hit? i num-steps num-hits offset) on-val off-val))
         (iota num-steps)))

  (make-doc rhythm-docs
    (euc 
     "Returns a list representing a euclidean pattern."
     ((steps Number "The number of steps in the rhythm")
      (hits  Number "The number of active steps in the rhythm")
      (offset [/opt Number 0] "The number of steps to rotate the pattern")
      (on-val  [/opt Any 1] "The value for steps that are hits in the returned list")
      (off-val [/opt Any ~] "The value for steps that are not hits in the returned list"))

     (((euc 3 1) => (1 ~ ~))
      ((euc 8 3) => (1 ~ ~ 1 ~ ~ 1 ~))
      ((euc 3 1 1) => (~ 1 ~))
      ((euc 3 1 1 "hi" 0) => (0 "hi" 0)))))
  
  
  ); end module 'rhythm'