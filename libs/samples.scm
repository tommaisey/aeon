(library (samples)

  (export
    samples
    samples-dir
    valid-sample?
    path-append
    name-contains?
    number-strings)

  (import (scheme) (node-eval) (utilities) (context))

  (define (valid-sample? f)
    (for-any (lambda (ext) (string-ci=? ext (path-extension f)))
             (list "wav" "aif" "aiff" "ogg")))

  (define-syntax samples
    (syntax-rules ()
      ((_ name files ...)
       (samples-impl name (list files ...)))))

  (define-syntax samples-dir
    (syntax-rules ()
      ((_ name dir-path pred)
       (samples-impl name
         (let* ([p (if (procedure? pred) pred (lambda (s) (string=? pred s)))]
                [p (lambda (s) (and (p s) (valid-sample? s)))])
           (map (lambda (x) (path-append dir-path x))
                (filter p (list-sort string<? (directory-list dir-path)))))))

      ((_ name dir-path)
       (samples-dir name dir-path (lambda (x) #t)))))

  ;; Declares 4 new identifiers based off the `name`.
  ;; - `name` is a value containing the first sample returned by list-impl.
  ;; - `name/` is a fn that take indeces and returns samples
  ;; - `name-list` is the raw list of samples
  ;; - `name-num` is the number of samples
  (define-syntax samples-impl
    (lambda (x)
      (syntax-case x ()
        ((_ name list-impl)
         (with-syntax ([id (gen-id #'name #'name)]
                       [id/  (gen-id #'name #'name "/")]
                       [id-list (gen-id #'name #'name "-list")]
                       [id-num  (gen-id #'name #'name "-num")])
           #'(begin
               (define id-list list-impl)
               (define id-num (length id-list))
               (define id (if (zero? id-num) "" (car id-list)))

               (define (id/ val)
                 (define (getter idx)
                   (if (and (number? idx) (not (zero? id-num)))
                       (list-ref id-list (mod (trunc-int idx) id-num))
                       (raise (string-append "No samples in '" 
                                             (symbol->string 'id) "'"))))
                 (if (context? val)
                     (getter 0) ;; raw id was placed in a pattern
                     (lambda (context)
                       (getter (get-leaf val context)))))

               (for-each (lambda (x) (println (path-last x))) id-list)

               (println (string-append (symbol->string 'id) ": "
                                       (number->string id-num)
                                       " samples defined."))))))))

  ;; A safer way to add a file name to a directory
  (define (path-append dir file)
    (string-append (path-root dir) (string (directory-separator)) file))

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
      (map (lambda (x) (format f-str x)) raw-nums))))