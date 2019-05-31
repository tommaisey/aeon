;; (pv-rand-wipe bufferA bufferB wipe trig)

;; Cross fades between two sounds by copying bins in a random order.

;; bufferA = fft buffer A.  bufferB = fft buffer B.  wipe = copies
;; bins from bufferB in a random order (0, 1).  trig = select new
;; random ordering.

(with-sc3
 (lambda (fd)
   (async fd (b-alloc 10 2048 1))
   (async fd (b-alloc 11 2048 1))))

(define-syntax n-of 
  (syntax-rules () 
    ((_ n f) (mix-fill n (lambda (_) f)))))

(let* ((n 6)
       (a (n-of n (mul (lf-saw ar (exp-rand 400.0 1000.0) 0.0) 0.1)))
       (b (n-of n (mul (lf-pulse ar (exp-rand 80.0 400.0) 0.0 0.2)
		       (u:max (mul (sin-osc kr (rand 0.0 8.0) 0.0) 0.2) 0.0))))
       (f (fft* 10 a))
       (g (fft* 11 b))
       (y (mouse-y kr 0 1 0 0.1))
       (x (mouse-x kr 0 1 0 0.1))
       (h (pv-rand-wipe f g x (gt y 0.5)))
       (i (ifft* h)))
  (audition (out 0 (mul 0.5 (mce2 i i)))))
