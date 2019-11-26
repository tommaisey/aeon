;; (pan-b2 in azimuth gain)

;; 2D Ambisonic B-format panner.  Encode a mono signal to two
;; dimensional ambisonic B-format.  The azimuth parameter is the
;; position around the circle from -1 to +1.  -1 is behind, -0.5 is
;; left, 0 is forward, +0.5 is right, +1 is behind.

(let* ((p (pink-noise ar))
       (encoded (pan-b2 p (mouse-x kr -1 1 0 0.1) 0.1))
       (decoded (decode-b2 4 
			  (mce-channel encoded 0)
			  (mce-channel encoded 1)
			  (mce-channel encoded 2)
			  0.5)))
  (audition (out 0 decoded)))
