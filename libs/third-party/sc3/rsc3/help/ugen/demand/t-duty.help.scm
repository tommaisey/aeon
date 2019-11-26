;; Play a little rhythm

(hear
 (let ((s (dseq dinf (make-mce (list 0.1 0.2 0.4 0.3)))))
   (t-duty ar s 0 do-nothing 1 0)))

;; Amplitude changes

(hear
 (let* ((dur (dseq dinf (make-mce (list 0.1 0.2 0.4 0.3))))
        (amp (dseq dinf (make-mce (list 0.1 0.4 0.01 0.5 1.0))))
        (t (t-duty ar dur 0 do-nothing amp 0)))
   (ringz t 1000 0.1)))

(hear
 (let* ((dur (mouse-x kr 0.001 2 1 0.1))
        (amp (dseq dinf (make-mce (list 0.1 0.4 0.01 0.5 1.0))))
        (t (t-duty ar dur 0 0 amp 0)))
   (ringz t 1000 0.1)))
