;; Exposes the cy(x) forms. The actual subdivision algorithm is
;; implemented in subdivide.scm. Note that this must be imported
;; in a way that makes it available at macroexpand time.
(library (cyx)
  (export cy+ cy! ~)
  (import (scheme) (context)
	  (for (auto-quasi) expand)
	  (for (subdivide) expand))

  ;; Creates a context transformer that sets a property of the
  ;; events in the context according to a subdividing pattern.
  ;; e.g. This gives events a freq of 440 if they land
  ;; in the first half of the measure, and 880 if they land in
  ;; the second half:
  ;;
  ;; (cy! :freq [440 880])
  (define-syntax cy!
    (syntax-rules ()

      ((_) (lambda (context) context))

      ((_ key pdef)
       (cy! key 1 pdef))
      
      ((_ key pdur pdef)
       (lambda (context)
	 (cy!impl context pdur (pdef-quasi pdef) key)))))

  ;; Creates a context transformer that adds blank events according
  ;; to a subdividing pattern. Additional statements can be supplied
  ;; that apply cy!-style property patterns to these blank events.
  (define-syntax cy+
    (syntax-rules ()

      ((_ pdef)
       (cy+ 1 pdef))
      
      ((_ pdur pdef (key r ...) ...)
       (lambda (context)
	 (let* ([c (cy+impl context pdur (pdef-quasi pdef))]
		[c ((cy! key r ...) c)] ...)
	   (contexts-merge context c))))))
  )
