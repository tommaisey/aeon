;; fm-kltr (rd)

(import (rnrs) (sosc) (rsc3) (rhs))

(define fm-instr
  (letc ((bus 0)
         (trig 0)
         (amp 0.1)
         (dur 0.1)
         (freq 400)
         (index 40)
         (pan 0)
         (freq2 600))
    (let* ((p (env-perc 0.01 dur 1 (replicate 2 -4)))
           (e (env-gen ar 1 amp 0 1 remove-synth p))
           (x (x-line kr freq (mul freq (random 0.975 1.025)) dur 0))
           (o (sin-osc ar x 0))
           (l (line kr index (mul freq (random 0.5 1.5)) dur 0))
           (x2 (x-line kr freq2 (mul freq2 (random 0.975 1.025)) dur 0))
           (m (mul-add o l x2)))
      (out bus (pan2 (sin-osc ar m 0)
                     (line kr pan (mul pan (random 0.75 1.25)) dur 0)
                     e)))))

(define fm
  (lambda (fd f ff a d i)
    (send
     fd
     (s-new "fm" -1 1 1
            (list "freq" (s:midi-cps f)
                  "freq2" (+ (s:midi-cps ff) (random -1 1))
                  "amp" a
                  "dur" d
                  "index" i)))))

(define r-note
  (lambda (o p)
    (+ (* (choose o) 12)
       (choose p))))

(define low-f
  (lambda (fd)
    (fm
     fd
     (r-note (list 3 5)
             (list 0 3 7 8))
     (random 36 72)
     (random 0.0 0.2)
     (random 1.2 7.2)
     (random 240 1480))))

(define high-f
  (lambda (fd)
    (fm
     fd
     (r-note (list 7 10)
             (list 0 2 5 10))
     (random 84 120)
     (random 0.1 0.6)
     (random 0.2 1.2)
     (random 240 1480))))

(define cmp-f
  (lambda (a b)
    (compare (car a) (car b))))

(define low-t
  (integrate
   (cons 0
         (replicate-m
          36
          (choose (list 0.25 0.5 0.75 1.0 1.5))))))

(define high-t
  (integrate
   (cons 0
         (replicate-m
          36
          (choose (list 0.05 0.15 0.25 0.5 0.75))))))

(define fm-kltr
  (lambda (fd)
    (send-synth fd "fm" fm-instr)
    (map
     (lambda (x)
       ((cadr x) fd)
       (thread-sleep (car x)))
     (sort-by
      cmp-f
      (append (map (lambda (x) (list x low-f)) low-t)
	      (map (lambda (x) (list x high-f)) high-t))))))

(with-sc3 fm-kltr)
