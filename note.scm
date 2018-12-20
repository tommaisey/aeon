;; -*- geiser-scheme-implementation: chez-*-
;;-------------------------------------------------------------
;; A rudimentary note event, which is really just a hashtable.
;; ------------------------------------------------------------
(library (note)
  (export
   make-note
   note-has
   note-get
   note-set!
   note-update!
   note-copy
   note-check
   print-note
   print-notes
   make-notes-with-times
   make-notes-regular)

  (import (chezscheme))

  (define note-table-start-size 16)
  (define note-start-beat-key 'beat)

  (define (make-note start-beat)
    (let ([table (make-eq-hashtable note-table-start-size)])
      (hashtable-set! table note-start-beat-key start-beat)
      table))

  (define (note-has note key)
    (hashtable-contains? note key))
  (define (note-get note key default)
    (hashtable-ref note key default))
  (define (note-set! note key value)
    (hashtable-set! note key value))
  (define (note-update! note key change-fn default)
    (hashtable-update! note key change-fn default))
  (define (note-copy note)
    (hashtable-copy note #t)) ; mutable
  (define (note-check note key pred . args)
    (let ([v (note-get note key #f)])
      (and v (apply pred (cons v args)))))

  ;; Takes a lambda that takes a key and a value,
  ;; applies to each hashtable entry.
  (define (hashtable-for-each hashtable fn)
    (call-with-values (lambda () (hashtable-entries hashtable))
      (lambda (keys values) (vector-map fn keys values))))

  (define (print-note note)
    (begin
      (display "{")
      (hashtable-for-each note (lambda (key value)
				 (display " ")
				 (display key)
				 (display ": ")
				 (display value)
				 (display ", ")))
      (display "}")
      (newline)))

  (define (print-notes note-list)
    (for-each print-note note-list))

  (define (make-notes-with-times times-list)
    (map make-note times-list))

  (define (make-notes-regular num interval start)
    (define (impl lst num t)
      (if (= num 0) lst
	  (impl (cons t lst) (sub1 num) (+ t interval))))
    (make-notes-with-times (impl '() num start)))
  
  ) ; end module 'note'
