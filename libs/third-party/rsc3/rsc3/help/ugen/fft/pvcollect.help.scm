;; (pvcollect chain numframes func frombin tobin zeroothers)

;; Process each bin of an fft chain separately.

;; pvcollect applies function func to each bin of an fft chain. func
;; should be a function that takes magnitude, phase, index as inputs
;; and returns a resulting [magnitude, phase].

;; The "index" is the integer bin number, starting at 0 for DC. You
;; can optionally ignore the phase and only return a single
;; (magnitude) value, in which case the phase is assumed to be left
;; unchanged.

;; frombin, tobin, and zeroothers are optional arguments which limit
;; the processing to a specified integer range of fft bins. If
;; zeroothers is set to 1 then bins outside of the range being
;; processed are silenced.

;; Note that this procedure can be relatively CPU-heavy, depending on
;; how you use it.

(define no-op
  (lambda (m p _)
    (list m p)))

(define rand-phase
  (lambda (m p _)
    (list m (rand 0 3.14))))

(define noise-phase
  (lambda (m p _)
    (list m (lin-lin (lf-noise0 kr 3) -1 1 0 3.14))))

(define combf
  (lambda (m p i)
    (list (if (= (modulo i 7) 0) m 0) p)))

(define noise-mag
  (lambda (m p _)
    (list (mul (gt (lf-noise0 kr 10) 0) m) p)))

(define spectral-delay
  (lambda (m p _)
    (let ((v (lin-lin (lf-par kr 0.5 0) -1 1 0.1 1)))
      (list (add m (delay-n m 1 v)) p))))

(define (bpf-sweep nf)
  (lambda (m p i)
    (let ((e (u:abs (sub i (lin-lin (lf-par kr 0.1 0) -1 1 2 (/ nf 20))))))
      (list (mul (lt e 10) m) p))))

(with-sc3
 (lambda (fd)
   (async fd (b-alloc 10 1024 1))
   (async fd (b-alloc-read 11 "/home/rohan/audio/metal.wav" 0 0))))

(let* ((nf 1024)
       (i (play-buf 1 11 (buf-rate-scale kr 11) 1 0 1))
       (c1 (fft* 10 i))
       (c2 (pvcollect c1 nf spectral-delay 0 250 0)))
  (audition (out 0 (mul 0.1 (ifft* c2)))))
