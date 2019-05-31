;; (pv-copy bufferA bufferB)

;; Copies the spectral frame in bufferA to bufferB at that point in
;; the chain of PV UGens. This allows for parallel processing of
;; spectral data without the need for multiple fft UGens, and to copy
;; out data at that point in the chain for other purposes. bufferA and
;; bufferB must be the same size.

;; bufferA - source buffer.
;; bufferB - destination buffer.

(with-sc3
 (lambda (fd)
   (async fd (b-alloc 0 2048 1))
   (async fd (b-alloc 1 2048 1))))

;; Proof of concept, silence

(let* ((in (lfclip-noise ar 100))
       (c0 (fft* 0 in))
       (c1 (pv-copy c0 1)))
  (audition (out 0 (sub (ifft* c0) (ifft* c1)))))
