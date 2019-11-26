;; (t-delay trigger delayTime)

;; Delays a trigger by a given time. Any triggers which arrive in the
;; time between an input trigger and its delayed output, are ignored.

;; trigger   - input trigger signal.
;; delayTime - delay time in seconds.

(let* ((s (mul (sin-osc ar 440 0) 0.1))
       (z (impulse ar 2 0))
       (l (mul z 0.1))
       (r (mul (toggle-ff (t-delay z 0.5)) s)))
  (audition (out 0 (mce2 l r))))
