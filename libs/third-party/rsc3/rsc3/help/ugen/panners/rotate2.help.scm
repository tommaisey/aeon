(let* ((p (mul (white-noise ar) 0.05))
       (q (mul (mix (lf-saw ar (make-mce (list 200 200.37 201)) 0)) 0.03))
       (encoded (add (pan-b2 p -0.5 1) (pan-b2 q -0.5 1)))
       (rotated (rotate2 (mce-channel encoded 1)
			 (mce-channel encoded 2)
			 (mouse-x kr -1 1 0 0.1)))
       (decoded (decode-b2 4
			  (mce-channel encoded 0)
			  (mce-channel rotated 0)
			  (mce-channel rotated 1)
			  0.5)))
  (audition (out 0 decoded)))

;; Rotation of stereo sound, via LFO.

(let ((x (mul (pink-noise ar) 0.4))
      (y (mul (lf-tri ar 800 0) (mul (lf-pulse kr 3 0 0.3) 0.2))))
  (audition (out 0 (rotate2 x y (lf-saw kr 0.1 0)))))

;; Rotation of stereo sound, via mouse.

(let ((x (mix-fill 4 (lambda (_) (mul (lf-saw ar (rand 198 202) 0) 0.1))))
      (y (mul (sin-osc ar 900 0) (mul (lf-pulse kr 3 0 0.3) 0.2))))
  (audition (out 0 (rotate2 x y (mouse-x kr 0 2 0 0.1)))))
