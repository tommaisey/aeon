(library (samples)

  (export
    :sample :sample-idx 
    :smpl :sidx
    samples
    samples-dir
    valid-sample?
    get-sample-safe
    path-append
    name-contains?
    number-strings)

  (import (scheme) (utilities) (node-eval) 
          (file-tools) (context))

  (declare-keywords :sample :sample-idx)
  (alias :smpl :sample)
  (alias :sidx :sample-idx)

  (define-syntax samples
    (syntax-rules ()
      ((_ name files ...)
       (samples-impl name (list files ...) ""))))

  (define-syntax samples-dir
    (syntax-rules ()
      ((_ name dir-path pred)
       (samples-impl
        name
        (let* ([path (expand-path dir-path)]
               [flt (if (procedure? pred) pred (lambda (s) (equal? s pred)))]
               [flt (lambda (s) (and (flt s) (valid-sample? s)))]
               [sorted (list-sort string<? (directory-list path))])
          (map (lambda (f) (path+ path f)) (filter flt sorted)))
        (str+ " from ..." (string-last dir-path 42))))

      ((_ name dir-path)
       (samples-dir name dir-path (lambda (x) #t)))))

  ;; Declares 3 new identifiers based off the `name`.
  ;; - `name` is a vector of samples returned by list-impl.
  ;; - `name/` is a fn that take indeces and returns samples
  ;; - `name-num` is the number of samples
  (define-syntax samples-impl
    (lambda (x)
      (syntax-case x ()
        ((_ name list-impl source-string)
         (with-syntax ([id (gen-id #'name #'name)]
                       [id/  (gen-id #'name #'name "/")]
                       [id-num  (gen-id #'name #'name "-num")])
           #'(begin
               (define last-samples-num (if (top-level-bound? 'id-num) id-num 0))
               (define id (list->vector list-impl))
               (define id-num (vector-length id))

               (define (id/ val)
                 (lambda (context)
                   (get-sample-safe id (eval-seq val context))))

               (println (str+ (symbol->string 'id) ": "
                              (number->string id-num)
                              " samples added"
                              source-string))))))))

  (define (valid-sample? f)
    (and (string? f)
         (for-any (lambda (ext) (string-ci=? ext (path-extension f)))
                  (list "wav" "aif" "aiff" "ogg"))))

  (define (get-sample-safe sample-vec idx)
    (let ([len (vector-length sample-vec)])
      (cond
        [(not (number? idx))
         (error 'get-sample-safe "Can't index sample" idx)]
        [(zero? len)
         (error 'get-sample-safe "No samples in vector")]
        [else (vector-ref sample-vec (mod (trunc-int idx) len))])))

  ;; Returns a predicate for matching strings. May take a string or a
  ;; list of strings. Supply a bool for the first arg to invert the results.
  (define name-contains?
    (case-lambda
      ((string-or-list)
       (name-contains? #t string-or-list))

      ((accept? string-or-list)
       (if (string? string-or-list)
           (name-contains? accept? (list string-or-list))
           (lambda (s) ((if accept? for-any for-none)
                        (lambda (s2) (string-contains s s2)) string-or-list))))))

  ;; Returns a list of the numbers between `start` and `end` as strings.
  ;; They are padded with zeros to `num-chars` characters.
  (define (number-strings start end num-chars)
    (let ([raw-nums (map (lambda (x) (+ start x)) (iota (+ 1 (- end start))))]
          [f-str (string-append "~" (number->string num-chars) ",'0d")])
      (map (lambda (x) (format f-str x)) raw-nums)))
  
  )
