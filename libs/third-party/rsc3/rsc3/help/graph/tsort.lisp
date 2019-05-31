;; tsort (jmcc)

;; This simple graph tests the topological sort of the unit generator
;; graph, it ought only to use a minimal number of interconnect buffers.

;; The below 369 node graph works with 'scsynth -u 57110 -w 2'.

;; (Note that graphs loaded from disk during startup will grow the number
;; of interconnect buffers, so to test this we must delete all graphs that
;; would otherwise be loaded.)

(define tsort
  (let* ((n 122)
         (c (env-bp '(0 0 0.15 1 6 0) 1 1 '(1 1 1)))
         (e (env-gen kr 1 1 0 1 remove-synth c))
         (f (lambda (o) (mul (sin-osc ar (add 440 o) 0) 0.001)))
         (s (mix-fill n f)))
  (mul s e)))

(hear tsort)
