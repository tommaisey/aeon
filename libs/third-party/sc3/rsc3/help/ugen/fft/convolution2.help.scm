;; (convolution2 in bufnum trigger framesize)

#|

Strict convolution with fixed kernel which can be updated using a
trigger signal.

in        - processing target
bufnum    - buffer index for the fixed kernel, may be modulated in
            combination with the trigger
trigger   - update the kernel on a change from <=0 to >0
framesize - size of fft frame, must be a power of two. convolution
            uses twice this number internally, maximum value you
            can give this argument is 2^16=65536. Note that it gets
            progressively more expensive to run for higher powers!
            512, 1024, 2048, 4096 standard.

|#

(with-sc3
 (lambda (fd)
   (for-each
    (lambda (b) 
      (async fd (b-alloc b 2048 1)))
    (list 10 11 12))
   (for-each 
    (lambda (n) 
      (send fd (b-set1 10 (+ (* 400 n) 100) 1))) 
    (enum-from-to 0 2))
   (for-each 
    (lambda (n) 
      (send fd (b-set1 11 (+ (* 20 n) 10) (random 0 1)))) 
    (enum-from-to 0 49))
   (for-each 
    (lambda (n) 
      (send fd (b-set1 12 (+ (* 40 n) 20) 1))) 
    (enum-from-to 0 19))
   (send-synth
    fd "c"
    (letc ((k 0) (t 0))
      (let ((i (impulse ar 1 0)))
	(out 0 (mul (convolution2 i k t 2048) 0.5)))))))

(define send-to 
  (lambda (m)
    (with-sc3 
     (lambda (fd) 
       (send fd m)))))

(define async-to
  (lambda (m) 
    (with-sc3 
     (lambda (fd) 
       (async fd m)))))

(send-to (s-new1 "c" 1001 1 1 "k" 10))

(send-to (n-set1 1001 "k" 11))
(send-to (n-set1 1001 "t" 0))
(send-to (n-set1 1001 "t" 1))

(send-to (n-set1 1001 "k" 12))
(send-to (n-set1 1001 "t" 0))
(send-to (n-set1 1001 "t" 1))

(async-to (b-zero 12))

(for-each 
 (lambda (n) 
   (send-to (b-set1 12 (+ (* 20  n) 10)  1))) 
 (enum-from-to 0 39))

(send-to (n-set1 1001 "t" 0))
(send-to (n-set1 1001 "t" 1))

;; With soundfile.

(async-to (b-alloc-read 10 "/home/rohan/audio/metal.wav" 0 0))

(let ((i (sound-in 0)))
  (audition (out 0 (mul (convolution2 i 10 0 512) 0.5))))
