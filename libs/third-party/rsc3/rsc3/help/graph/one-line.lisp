;; one-line (lance putnam)

(define one-line
  (let* ((lfs (mul-add (lf-saw ar (mce2 1 0.99) (mce2 0 0.6)) 2000 2000))
         (lfs-t (mul (trunc lfs (mce2 400 600)) (mce2 1 -1)))
         (f (one-pole (mix lfs-t) 0.98)))
    (pan2 (sin-osc ar f 0) 0 0.1)))

(hear one-line)
