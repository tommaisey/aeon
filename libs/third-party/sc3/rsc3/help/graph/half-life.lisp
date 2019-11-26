;; half-life (jrhb)

(define half-life
  (let* ((t-half 3.92)
         (n-atoms 1e5)
         (n (u:max 0 (sub n-atoms (pulse-count (local-in 2 ar (mce2 0 0)) 0))))
         (activity (dust ar (mul n (fdiv (log 2) t-half)))))
    (mrg2 activity (local-out activity))))

(hear half-life)
