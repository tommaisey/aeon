;; e-lamell (rd)

(define e-lamell
  (letc ((f 440) (n 12) (d 0.1) (p 0) (a 1.0))
    (let* ((h (line ar n (t-choose 1 (mce2 1 32)) d do-nothing))
           (s (blip ar f h))
           (e-d (env-perc 0.005 d 1 (list -4 -4)))
           (e (env-gen ar 1 a 0 1 remove-synth e-d)))
      (out 0 (pan2 s p e)))))

(define r-note
  (lambda (o p)
    (+ (* (choose o) 12)
       (choose p))))

(define l-sel
  (lambda ()
    (r-note (list 2 3)
            (list 0))))

(define h-sel
  (lambda ()
    (r-note (list 2 3 4)
            (list 0))))

(define pattern
  (lambda (fd)
    (send
     fd
     (bundle
      -1
      (list (s-new "blip" -1 add-to-tail 1
                   (list "f" (s:midi-cps (l-sel))
                         "n" (i-random 2 36)
                         "d" (exp-random 0.01 0.4)
                         "a" (random 0 0.75)
                         "p" (random -1 1)))
            (s-new "blip" -1 add-to-tail 1
                   (list "f" (s:midi-cps (h-sel))
                         "n" (i-random 2 36)
                         "d" (exp-random 0.01 0.4)
                         "a" (choose (list 0 0.25 0.5 1.0))
                         "p" (random -1 1))))))
    (thread-sleep 0.1)
    (pattern fd)))

(with-sc3
 (lambda (fd)
   (send-synth fd "blip" e-lamell)
   (pattern fd)))
