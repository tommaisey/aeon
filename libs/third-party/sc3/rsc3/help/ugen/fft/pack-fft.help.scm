;; (Packfft chain bufsize frombin tobin zeroothers magsphases)

;; Pack separate demand-rate fft bin streams into an fft chain buffer

;; Takes a length-prefixed array of magnitudes and phases, and packs
;; them into an fft buffer ready for transforming back into
;; time-domain audio using Ifft.

;; Most people won't need to use this directly - instead, use
;; pvcollect, pvcalc, or pvcalc2.

;; The input data is magsphases, which should be a flat array
;; containing magnitude and phase of all bins in ascending order.
;; e.g. [mag0, phase0, mag1, phase1, mag2, phase2, ... magN, phaseN]
;; This input is typically demand-rate.

;; This is technically similar to demand or duty in that it calls
;; demand-rate UGens further up the graph to process the values,
;; eventually calling Unpackfft. These two ends of the process must in
;; most cases see the same chain...! Otherwise behaviour is undefined
;; and, who knows, possibly unpleasant.

;; frombin and tobin allow you to fill the supplied data only into a
;; subset of the fft bins (i.e. a single delimited frequency band),
;; set zeroothers to 1 to zero all the magnitudes outside this band
;; (otherwise they stay intact).

;; For usage examples, see Unpackfft, but also pvcollect, pvcalc,
;; pvcalc2.

;; Here's an unusual example which uses Packfft without using
;; Unpackfft first - essentially creating our fft data from scratch.

(with-sc3
 (lambda (fd)
   (send fd (b-alloc 10 512 1))))

(let* ((n 100)
       (n* (enum-from-to 1 n))
       (m1 (map (lambda (_) (range (f-sin-osc kr (exp-rand 0.1 1) 0) 0 1)) n*))
       (square (lambda (a) (* a a)))
       (m2 (map mul m1 (map square (iota n 1.0 (- (/ 1.0 n))))))
       (i (map (lambda (_) (lf-pulse kr (pow 2 (i-rand -3 5)) 0 0.3)) n*))
       (m3 (map mul m2 i))
       (p (replicate n 0.0))
       (c1 (fft* 10 (f-sin-osc ar 440 0)))
       (c2 (pack-fft c1 512 0 (- n 1) 1 (packfft-data m3 p)))
       (s (ifft* c2)))
  (audition (out 0 (mce2 s s))))
