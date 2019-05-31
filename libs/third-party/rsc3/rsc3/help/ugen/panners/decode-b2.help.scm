;; (decode-b2 numChannels w x y orientation)			

;; 2D Ambisonic B-format decoder.

;; Decode a two dimensional ambisonic B-format signal to a set of
;; speakers in a regular polygon.  The outputs will be in clockwise
;; order. The position of the first speaker is either center or left
;; of center.

;; The number of output speakers is typically 4 to 8.

;; The parameters w, x and y are the B-format signals.

;; The parameter orientation should be zero if the front is a vertex
;; of the polygon. The first speaker will be directly in
;; front. Should be 0.5 if the front bisects a side of the
;; polygon. Then the first speaker will be the one left of
;; center. Default is 0.5.

(let* ((p (pink-noise ar))
       (encoded (pan-b2 p (mouse-x kr -1 1 0 0.1) 0.1))
       (decoded (decode-b2 4
			  (mce-channel encoded 0)
			  (mce-channel encoded 1)
			  (mce-channel encoded 2)
			  0)))
  (audition (out 0 decoded)))
