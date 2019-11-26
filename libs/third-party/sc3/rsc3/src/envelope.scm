;; ENVELOPES

;; symbol|number -> number
(define curve-to-shape
  (lambda (c)
    (cond
     ((symbol? c)
      (cond ((equal? c 'step) 0.0)
	    ((equal? c 'linear) 1.0)
	    ((equal? c 'exponential) 2.0)
	    ((equal? c 'sine) 3.0)
	    ((equal? c 'welch) 4.0)
	    ((equal? c 'squared) 6.0)
	    ((equal? c 'cubed) 7.0)
	    (else (error "curve-to-shape" "symbol" c))))
     ((number? c)
      5.0)
     (else
      (error "curve-to-shape" "illegal curve" c)))))

;; any -> number
(define curve-to-value
  (lambda (c)
    (if (number? c) c 0.0)))

;; Make a <list> for use with the EnvGen UGen. `levels' is a <list>
;; containing the left to right gain values for the envelope, it has
;; one more element than the <list> `times', having the delta times
;; for each envelope segment. `curve' is either a string or a number
;; or a <list> of such, in either case it is expanded to a list of the
;; same length as `times'. `release-node' is the index of the
;; 'release' stage of the envelope, `loop-node' is the index of the
;; 'loop' stage of the envelope. These indices are set as invalid, by
;; convention -1, to indicate there is no such node.
(define env
  (lambda (levels times curves release-node loop-node)
    (make-mce
     (append
      (list (head levels) (length times) release-node loop-node)
      (concat
       (zip-with3
	(lambda (l t c)
	  (list l
		t
		(curve-to-shape c)
		(curve-to-value c)))
	(tail levels)
	times
	curves))))))

;; Co-ordinate based static envelope generator.
;; [(ugen . ugen)] -> ugen -> ugen -> [ugen] -> ugen
(define env-coord
  (lambda (d dur amp curves)
    (env (map (lambda (e) (mul (cdr e) amp)) d)
         (map (lambda (e) (mul e dur)) (d->dx (map car d)))
         curves
         -1
         -1)))

(define env-coord-linear
  (lambda (d dur amp)
    (env-coord d dur amp (replicate (- (length d) 1) 1))))

;; (equal? (mk-coord (list 1 2 3 4)) (list (cons 1 2) (cons 3 4)))
(define mk-coord
  (lambda (l)
    (if (null? l)
        '()
        (let ((x (car l))
              (y (cadr l))
              (r (cddr l)))
          (cons (cons x y) (mk-coord r))))))

(define env-bp
  (lambda (bp d a c) (env-coord (mk-coord bp) d a c)))

(define env-bp-linear
  (lambda (bp d a)
    (env-coord-linear (mk-coord bp) d a)))

;; Design a standard trapezoidal envelope. `shape' determines the
;; sustain time as a proportion of `dur', zero is a triangular
;; envelope, one a rectangular envelope. `skew' determines the
;; attack/decay ratio, zero is an immediate attack and a slow decay,
;; one a slow attack and an immediate decay. This implementation
;; builds a zero one breakpoint data set and calls env-coord.
(define env-trapezoid
  (lambda (shape skew dur amp)
    (let* ((x1 (mul skew (sub 1.0 shape)))
	   (bp (list (cons 0 (le skew 0.0))
		     (cons x1 1.0)
		     (cons (add shape x1) 1.0)
		     (cons 1.0 (ge skew 1.0)))))
      (env-coord bp dur amp (replicate 3 'linear)))))

(define env-triangle
  (lambda (dur level)
    (let ((half-dur (mul dur 0.5)))
      (env (list 0.0 level 0.0)
	   (list half-dur half-dur)
	   (list 'linear 'linear)
	   -1
	   -1))))

(define env-sine
  (lambda (dur level)
    (let ((half-dur (mul dur 0.5)))
      (env (list 0.0 level 0.0)
	   (list half-dur half-dur)
	   (list 'sine 'sine)
	   -1
	   -1))))

(define env-perc
  (lambda (attackTime releaseTime level curves)
    (env (list 0.0 level 0.0)
	 (list attackTime releaseTime)
	 curves
	 -1
	 -1)))

(define env-adsr
  (lambda (attackTime
	   decayTime
	   sustainLevel
	   releaseTime
	   peakLevel
	   curves
	   bias)
    (env (map (lambda (e) (mul e bias))
               (list 0.0 peakLevel (mul peakLevel sustainLevel) 0.0))
	 (list attackTime decayTime releaseTime)
	 curves
	 2
	 -1)))

(define env-asr
  (lambda (attackTime sustainLevel releaseTime curves)
    (env (list 0.0 sustainLevel 0.0)
	 (list attackTime releaseTime)
	 curves
	 1
	 -1)))

(define env-linen
  (lambda (attackTime sustainTime releaseTime level curves)
    (env (list 0.0 level level 0.0)
	 (list attackTime sustainTime releaseTime)
	 (if (null? curves) (list 'linear 'linear 'linear) curves)
	 -1
	 -1)))
