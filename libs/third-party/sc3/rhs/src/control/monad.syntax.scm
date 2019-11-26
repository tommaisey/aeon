;; replicateM :: (Monad m) => Int -> m a -> m [a]
(define-syntax replicate-m
  (syntax-rules ()
    ((_ i x)
     (replicate-m* i (lambda () x)))))
