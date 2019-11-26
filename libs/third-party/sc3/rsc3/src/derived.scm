(define add3 sum3)
(define add4 sum4)

(define as-local-buf
  (lambda (l)
    (let* ((b (local-buf (length l) 1))
           (s (set-buf* b 0 l)))
      (mrg2 b s))))

(define buf-rd-c (lambda (nc r b p l) (buf-rd nc r b p l 4)))
(define buf-rd-l (lambda (nc r b p l) (buf-rd nc r b p l 2)))
(define buf-rd-n (lambda (nc r b p l) (buf-rd nc r b p l 1)))

(define cps-midi cpsmidi)

;; ugen -> ugen -> ugen
(define dcons
  (lambda (x xs)
    (let ((i (dseq 1 (mce2 0 1)))
	  (a (dseq 1 (mce2 x xs))))
      (dswitch i a))))

;; ugen -> ugen -> ugen -> ugen -> ugen -> ugen
(define dyn-klank*
  (lambda (i fs fo ds l)
    (if (null? l)
        0
        (let ((f (list-ref l 0))
              (a (list-ref l 1))
              (d (list-ref l 2)))
          (add (mul (ringz i (mul-add f fs fo) (mul d ds)) a)
               (dyn-klank* i fs fo ds (drop 3 l)))))))

(define dyn-klank
  (lambda (i fs fo ds s)
    (dyn-klank* i fs fo ds (mce-channels s))))

(define fdiv f-div)

(define fft* (lambda (buf in) (fft buf in 0.5 0 1 0)))

;; ugen -> ugen -> ugen -> ugen
(define freq-shift*
  (lambda (i f p)
    (let ((o (sin-osc ar f (mce2 (add p (* 0.5 pi)) p)))
	  (h (hilbert i)))
      (mix (mul h o)))))

(define hear (lambda (u) (audition (out 0 u))))

(define ifft* (lambda (buf) (ifft buf 0 0)))

;; [ugen] -> [ugen] -> [ugen] -> ugen
(define klang-data
  (lambda (freqs amps phases)
    (make-mce
     (concat
      (zip-with3
       list3
       freqs amps phases)))))

;; [ugen] -> [ugen] -> [ugen] -> ugen
(define klank-data klang-data)

;; ugen -> ugen -> ugen -> ugen
(define klank-data-mce
  (lambda (f a p)
    (klank-data (mce-channels f) (mce-channels a) (mce-channels p))))

(define l-choose
  (lambda (l)
    (select (i-rand 0 (length l)) (make-mce l))))

(define lin-lin
  (lambda (in srclo srchi dstlo dsthi)
    (let* ((scale (fdiv (sub dsthi dstlo) (sub srchi srclo)))
           (offset (sub dstlo (mul scale srclo))))
      (mul-add in scale offset))))

(define mce2 (lambda (a b) (make-mce (list a b))))
(define mce3 (lambda (a b c) (make-mce (list a b c))))
(define mce4 (lambda (a b c d) (make-mce (list a b c d))))
(define mce5 (lambda (a b c d e) (make-mce (list5 a b c d e))))

;; mce -> int -> ugen
(define mce-channel
  (lambda (u n)
    (list-ref (mce-channels u) n)))

;; ([ugen] -> [ugen]) -> (mce -> mce)
(define mce-edit
  (lambda (f)
    (lambda (u)
      (make-mce (f (mce-channels u))))))

;; int -> (int -> ugen) -> mce
(define mce-fill
  (lambda (n f)
    (make-mce (map f (enum-from-to 0 (- n 1))))))

;; (ugen -> ugen) -> mce -> mce
(define mce-map (lambda (f u) (make-mce (map f (mce-channels u)))))

;; mce -> mce
(define mce-reverse (mce-edit reverse))

;; mce -> mce
(define mce-transpose
  (lambda (u)
    (make-mce
     (map make-mce (transpose (map mce-channels (mce-channels u)))))))

(define midi-cps midicps)

;; ugen|mce -> ugen
(define mix (lambda (u) (foldl add 0 (mce-channels u))))

;; int -> (int -> ugen) -> ugen
(define mix-fill (lambda (n f) (mix (mce-fill n f))))

;; Rate -> UGen -> UGen -> Warp -> UGen -> UGen
(define mouse-r
  (lambda (rt l r ty tm)
    (let ((f (if (= ty 0) lin-lin lin-exp)))
      (lag (f (lf-noise1 rt 1) -1 1 l r) tm))))

(define mouse-x* mouse-r)
(define mouse-y* mouse-r)

(define mouse-button*
  (lambda (rt l r tm)
    (let ((o (lf-clip-noise rt 1)))
      (lag (lin-lin o -1 1 l r) tm))))

(define mrg2 make-mrg)
(define mrg3 (lambda (a b c) (mrg2 a (mrg2 b c))))
(define mrg4 (lambda (a b c d) (mrg2 a (mrg3 b c d))))
(define mrg5 (lambda (a b c d e) (mrg2 a (mrg4 b c d e))))

;; [ugen] -> mrg
(define mrg-n
  (lambda (xs)
    (if (null? xs)
	(error "mrg-n" "nil input list" xs)
	(if (null? (tail xs))
	    (head xs)
	    (mrg2 (head xs) (mrg-n (tail xs)))))))

(define mul3 (lambda (a b c) (mul (mul a b) c)))
(define mul4 (lambda (a b c d) (mul (mul (mul a b) c) d)))

;; [m] -> [p] -> [#, m, p...]
(define packfft-data
  (lambda (m p)
    (make-mce
     (cons (* 2 (length m))
	   (concat (zip-with list m p))))))

;; [[m, p]] -> [#, m, p...]
(define packfft-data*
  (lambda (mp)
    (make-mce
     (cons (* 2 (length mp))
	   (concat mp)))))

;; double -> void
(define pause-thread thread-sleep)

;; double -> void
(define pause-thread-until
  (lambda (t)
    (let ((c (utcr)))
      (when (> t c) (pause-thread (- t c))))))

;; port -> ugen -> ()
(define play
  (lambda (fd u)
    (play-at fd u -1 add-to-tail 1)))

;; rate -> ugen -> ugen -> ugen -> ugen -> ugen
(define pm-osc
  (lambda (r cf mf pm mp)
    (sin-osc r cf (mul (sin-osc r mf mp) pm))))

(define pvcollect
  (lambda (c nf f from to z?)
    (let* ((m (unpack-fft c nf from to 0))
	   (p (unpack-fft c nf from to 1))
	   (i (enum-from-to from to))
	   (e (zip-with3 f m p i)))
      (pack-fft c nf from to z? (packfft-data* e)))))

(define rand* rand-)

; the cardinality input is derived from the values input...
(define set-buf*
  (lambda (buf offset values)
    (set-buf buf offset (length values) (make-mce values))))

;; ugen -> ugen
(define sound-in
  (lambda (n)
    (if (mce? n)
	(let ((l (mce-channels n)))
	  (if (consecutive? l)
	      (in (length l) ar (add num-output-buses (head l)))
	      (in 1 ar (add num-output-buses n))))
	(in 1 ar (add num-output-buses n)))))

(define t-choose
  (lambda (trig array)
    (select (ti-rand 0 (length (mce-channels array)) trig) array)))

(define tw-choose
  (lambda (trig array weights normalize)
    (select (tw-index trig normalize weights) array)))

(define tw-index t-windex)

(define unpack-fft
  (lambda (c nf from to mp?)
    (map (lambda (i)
            (unpack1-fft c nf i mp?))
	 (enum-from-to from to))))

(define with-sc3*
  (lambda (l)
    (with-sc3 (lambda (fd) (map (lambda (f) (f fd)) l)))))

; RANDOM

;; float -> float -> float
(define exp-random
  (lambda (a b)
    (let ((r (/ b a)))
      (* (expt r (random 0 1)) a))))

;; [a] -> a
(define choose
  (lambda (xs)
    (list-ref xs (i-random 0 (length xs)))))
