;; Exposes the 'subdivide' form. The actual subdivision algorithm is
;; implemented in 'impl-subdivide'. Note that this must be imported
;; (for (..) expand) to make it available at macroexpand time.
(library (subdivide)
  (export subdivide event ~)
  (import (scheme) (context)
	  (for (auto-quasi) expand)
	  (for (impl-subdivide) expand))

  ;;------------------------------------------------------------------
  ;; This identifier denotes a new additive subdivide pattern.
  (define event)
  
  ;;------------------------------------------------------------------
  ;; The main subdivide macro. The special identifier 'event' marks a
  ;; statement that will add new events.
  (define-syntax subdivide
    (syntax-rules (event)
      
      ((_) (lambda (context) context)) ;; Base case
      
      ((_ [event pdur pdef] rest ...)
       (lambda (context)
	 (let ([c (pcreate context pdur (pdef-quasi pdef))])
	   (contexts-merge context ((subdivide rest ...) c)))))

      ((_ [event pdef] rest ...)
       (subdivide [event 1 pdef] rest ...))

      ((_ [key pdur pdef] rest ...)
       (lambda (context)
	 (let ([c (pmorph context pdur (pdef-quasi pdef) key)])
	   ((subdivide rest ...) c))))

      ((_ [key pdef] rest ...)
       (subdivide [key 1 pdef] rest ...))))
  )
