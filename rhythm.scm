;; -*- geiser-scheme-implementation: chez-*-
;;----------------------------------------------------------
;; Rhythm helper functions
;; ---------------------------------------------------------
(library (rhythm)
  (export
   snap-next
   snap-next-if
   snap-prev
   snap-prev-if
   snap-nearest
     
   make-euclid
   euclid-num-steps
   euclid-num-hits
   euclid-offset
   euclidean-rhythm)

  (import (chezscheme) (utilities) (event))

  ;; Snap a value to the next number divisible by divisor.
  (define (snap-next beat divisor)
    (let ([overlap (fmod beat divisor)])
      (+ (- beat overlap) divisor)))

  ;; Snap a value to the next number divisible by divisor,
  ;; if `beat` isn't already cleanly divisible by divisor.
  (define (snap-next-if beat divisor)
    (let ([next (snap-next beat divisor)])
      (if (= beat (- next divisor)) beat next)))

  ;; Snap a value to the next number divisible by divisor.
  (define (snap-prev beat divisor)
    (let ([overlap (fmod beat divisor)])
      (- beat overlap)))

  ;; Snap a value to the next number divisible by divisor,
  ;; if `beat` isn't already cleanly divisible by divisor
  (define (snap-prev-if beat divisor)
    (let ([prev (snap-prev beat divisor)])
      (if (= beat (+ prev divisor)) beat prev)))

  ;; Snap to the next or previous divisor, whichever's closer.
  (define (snap-nearest beat divisor)
    (let* ([overlap (fmod beat divisor)]
	   [prev (- beat overlap)])
      (if (>= overlap (* 0.5 divisor))
	  (+ prev divisor) prev)))

  ;;-----------------------------------------------------------------
  ;; Euclidean patterns
  ;;-----------------------------------------------------------------
  ;; Parameters for a euclidean rhythm
  (define-record euclid (num-steps num-hits offset))

  ;; This version would always result in a hit on the last step,
  ;; but the usual expectation is that it's on the first step
  ;; TODO: is this necessary? Wouldn't just (add1 offset) work?
  (define (euclid-normalise-offset e)
    (let ([new-offset (% (add1 (euclid-offset e)) (euclid-num-steps e))])
      (make-euclid (euclid-num-steps e) (euclid-num-hits e) new-offset)))

  ;; non-iterative derivation of this algorithm:
  ;; http://computermusicdesign.com/simplest-euclidean-rhythm-algorithm-explained/
  (define (euclidean-hit? step e)
    (check-type euclid? e "Parameter 'e' must be a euclidean record")
    
    (let*
	([e      (euclid-normalise-offset e)]
	 [hits   (euclid-num-hits e)]
	 [steps  (euclid-num-steps e)]
	 [offset (euclid-offset e)]
	 ;; put bucket in state as if it had done N = offset iterations 
	 [bucket (% (* hits (- steps offset)) steps)]
	 ;; compute state of bucket just before the requested step
	 [bucket (+ bucket (* hits (% step steps)))])
      
      (not (eqv? (quotient bucket steps)
		 (quotient (+ bucket hits) steps)))))

  ;; Make the events inside a range w given a euclidean pattern e
  (define (euclidean-rhythm w e stretch)
    (check-type euclid? e "Parameter 'e' must be a euclid record")
    (check-type range? w "Parameter 'w' must be a range record")

    (let*
	([num-steps  (euclid-num-steps e)]
	 [step-size  (/ stretch num-steps)]
	 [start-beat (snap-next-if (range-start w) step-size)])
      (define (next-step events e w)
	(if (range-valid? w)
	    (let*
		([beat  (range-start w)]
		 [next  (snap-next beat step-size)]
		 [step  (% (round (/ beat step-size)) num-steps)]
		 [events (if (euclidean-hit? step e)
			    (cons (make-event beat) events)
			    events)])
	      (next-step events e (range-with-start w next)))
	    events))
      (next-step '() e (range-with-start w start-beat))))

  ); end module 'rhythm'
