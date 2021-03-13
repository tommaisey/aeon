;;------------------------------------------------------------------
;; Keeping track of buffers for reading samples & recording.
;; See also "recorder" synth below.
(define max-num-buffers 1024) ;; sc3 default
(define bufnum-table (make-vector max-num-buffers #f))
(define (find-buffer-slot test-fn) (find-first-slot bufnum-table test-fn))
(define (find-free-buffer) (find-buffer-slot (lambda (x) (not x))))
(define (find-buffer hash) (find-buffer-slot (lambda (x) (eqv? hash x))))

(define (query-buffer bufnum)
  (send-bundle (sc/utc) (list (sc/b-query1 bufnum)) "/b_info"))

(define (get-bufnum bufnum/path)
  (let ([b bufnum/path])
    (cond
      ((number? b) (if (vector-ref bufnum-table b) b #f))
      ((string? b) (find-buffer (string-hash b)))
      (else (error 'get-bufnum "invalid bufnum/path" b)))))

;; Find a buffer loaded from the file path. If not found,
;; load it into a fresh sc3 buffer. Returns the buffer id.
(define (open-read-buffer path)
  (let ([hash (string-hash path)])
    (or (find-buffer hash)
        (lest [bufnum (find-free-buffer)]
              (begin
                (vector-set! bufnum-table bufnum hash)
                (sc/async sc3 (sc/b-alloc-read bufnum path 0 0))
                bufnum)
              (error 'load-buffer "No free buffers remain" path)))))

;; Release an sc3 buffer and its corresponding slot on our side.
(define (close-read-buffer path)
  (lest [bufnum (get-bufnum bufnum-or-path)]
        (begin
          (sc/async sc3 (sc/b-free bufnum))
          (vector-set! bufnum-table bufnum #f))
        (error 'release-buffer "buffer not found for path" path)))

;; Open a new buffer for streamed recording via a diskin ugen.
(define (open-write-buffer path)
  (lest [bufnum (find-free-buffer)]
        (let ([num-frames (expt 2 18)])
          (vector-set! bufnum-table bufnum (string-hash path))
          (sc/async sc3 (sc/b-alloc bufnum num-frames 2)) ; 2 channels
          (sc/async sc3 (sc/b-write bufnum path "wav" "int16" 0 0 1))
          bufnum)
        #f))

;; Close a buffer used for streamed recording via a diskin ugen.
;; TODO: find a way to do this at a specified time. Bundling the 2
;; messages doesn't seem to work.
(define* (close-write-buffer bufnum/path [/opt (t (sc/utc))])
  (lest [bufnum (get-bufnum bufnum/path)]
        (begin
          (send-bundle t (list (sc/b-close bufnum)
                               (sc/b-free bufnum)))
          (vector-set! bufnum-table bufnum #f))
        (error 'close-write-buffer "buffer not found for bufnum/path" bufnum/path)))
