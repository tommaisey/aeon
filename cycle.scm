;; Exposes the 'cycle' form. The actual subdivision algorithm is
;; implemented in subdivide.scm. Note that this must be imported
;; in a way that makes it available at macroexpand time.
(library (cycle)
  (export cycle event ~)
  (import (scheme) (context)
	  (for (auto-quasi) expand)
	  (for (subdivide) expand))

  ;;------------------------------------------------------------------
  ;; This identifier denotes a new additive subdivide pattern.
  (define event)
  
  ;;------------------------------------------------------------------
  ;; The main subdividing pattern macro. The special identifier 'event
  ;;  marks a statement that will add new events.
  (define-syntax cycle
    (syntax-rules (event)
      
      ((_) (lambda (context) context)) ;; Base case
      
      ((_ [event pdur pdef] rest ...)
       (lambda (context)
	 (let ([c (pcreate context pdur (pdef-quasi pdef))])
	   (contexts-merge context ((cycle rest ...) c)))))

      ((_ [event pdef] rest ...)
       (cycle [event 1 pdef] rest ...))

      ((_ [key pdur pdef] rest ...)
       (lambda (context)
	 (let ([c (pmorph context pdur (pdef-quasi pdef) key)])
	   ((cycle rest ...) c))))

      ((_ [key pdef] rest ...)
       (cycle [key 1 pdef] rest ...))))
  )
