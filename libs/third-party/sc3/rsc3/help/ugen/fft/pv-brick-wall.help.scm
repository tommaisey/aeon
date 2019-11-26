;; (pv-brick-wall buffer wipe)

;; Clears bins above or below a cutoff point.  `wipe' = a unit signal,
;; from -1 to 0 the UGen acts as a low-pass filter, from 0 to 1 it
;; acts as a high pass filter.

(with-sc3
 (lambda (fd)
   (async fd (b-alloc 10 2048 1))))

(let ((x (mouse-x kr -1 1 0 0.1))
      (c (fft* 10 (mul (white-noise ar) 0.2))))
  (audition (out 0 (ifft* (pv-brick-wall c x)))))
