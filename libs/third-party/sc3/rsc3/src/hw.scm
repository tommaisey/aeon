(define pack-fft
  (lambda (b sz fr to z mp)
    (mk-ugen (list "PackFFT" kr (list b sz fr to z) mp 1 nil nil))))

(define unpack1-fft
  (lambda (c b bi wm)
    (mk-ugen (list "Unpack1FFT" dr (list c b bi wm) nil 1 nil nil))))
