;; UID

;; int -> uid
(define-record-type uid
  (fields n))

;; (srfi:define-record-type uid
;;   (make-uid n) uid? (n uid-n))

;; () -> uid
(define incr-uid
  (let ((n 0))
    (lambda (i)
      (set! n (+ n i))
      (make-uid n))))

(define unique-uid
  (lambda ()
    (incr-uid 1)))

;; CONTROL

;; string -> int -> control
(define-record-type control
  (fields name index))

;; string -> float -> rate -> float -> control*
(define-record-type control*
  (fields name default rate lag))

;; syntax for binding control values
(define-syntax letc
  (syntax-rules ()
    ((_ () expr)
     expr)
    ((_ ((name default) ...) expr)
     (let ((name (make-control* (symbol->string (quote name)) default kr 0))
	   ...)
       expr))))

;; GRAPHDEF

;; string -> [float] -> [float] -> [controls] -> [ugens] -> graphdef
(define-record-type graphdef
  (fields name constants defaults controls ugens))

;; graphdef -> int -> ugen
(define graphdef-ugen
  (lambda (g n)
    (list-ref (graphdef-ugens g) n)))

;; graphdef -> int -> control
(define graphdef-control
  (lambda (g n)
    (list-ref (graphdef-controls g) n)))

;; graphdef -> int -> float
(define graphdef-constant
  (lambda (g n)
    (list-ref (graphdef-constants g) n)))

;; UGEN

;; int -> int -> input
(define-record-type input
  (fields ugen port))

;; [ugen] -> mce
(define-record-type mce
  (fields proxies))

;; ugen -> ugen -> mrg
(define-record-type mrg
  (fields left right))

;; rate -> output
(define-record-type output
  (fields rate))

;; ugen -> int -> proxy
(define-record-type proxy
  (fields ugen port))

;; int -> rate
(define-record-type rate
  (fields value))

;; rate
(define ir (make-rate 0))
(define kr (make-rate 1))
(define ar (make-rate 2))
(define dr (make-rate 3))

;; rate -> symbol
;;
;; (map rate->symbol (list ir kr ar dr))
(define rate->symbol
  (lambda (r)
    (case (rate-value r)
      ((0) 'ir)
      ((1) 'kr)
      ((2) 'ar)
      ((3) 'dr)
      (else (error "rate->symbol" "unknown rate")))))

;; any -> rate
(define rate-of
  (lambda (o)
    (cond ((number? o) ir)
	  ((control*? o) (control*-rate o))
	  ((ugen? o) (ugen-rate o))
	  ((proxy? o) (rate-of (proxy-ugen o)))
	  ((mce? o) (rate-select (map rate-of (mce-proxies o))))
	  ((mrg? o) (rate-of (mrg-left o)))
	  (else (error "rate-of" "illegal value" o)))))

;; rate -> int
(define rate-to-ordinal
  (lambda (r)
    (cond ((eq? r ir) 0)
	  ((eq? r kr) 1)
	  ((eq? r ar) 2)
	  ((eq? r dr) 3)
	  (else (error "rate-to-ordinal" "illegal rate")))))

;; rate -> rate -> rate
(define rate-select*
  (lambda (a b)
    (let ((a* (rate-to-ordinal a))
	  (b* (rate-to-ordinal b)))
      (if (> a* b*) a b))))

;; [rate] -> rate
(define rate-select
  (lambda (l)
    (foldl1 rate-select* l)))

;; string -> rate -> [ugen] -> [output] -> int -> uid -> ugen
(define-record-type ugen
  (fields name rate inputs outputs special id))

;; ugen -> int -> output
(define ugen-output
  (lambda (u n)
    (list-ref (ugen-outputs u) n)))

;; ugen -> (ugen -> any) -> any
(define ugen-transform
  (lambda (u f)
    (let ((n (ugen-name u))
	  (r (ugen-rate u))
	  (i (ugen-inputs u))
	  (o (ugen-outputs u))
	  (s (ugen-special u))
	  (d (ugen-id u)))
      (f n r i o s d))))

;; any -> bool
(define input*?
  (lambda (i)
    (or (number? i)
	(control*? i)
	(ugen? i)
	(proxy? i)
	(mce? i)
        (mrg? i))))

;; ugen -> bool
(define ugen-valid?
  (lambda (u)
    (ugen-transform
     u
     (lambda (n r i o s d)
       (and (string? n)
	    (rate? r)
	    (and (list? i) (all input*? i))
	    (and (list? o) (all output? o))
	    (integer? s)
	    (uid? d))))))

;; int -> (() -> ugen) -> mce
(define clone*
  (lambda (n f)
    (make-mce (replicate-m* n f))))

(define-syntax clone
  (syntax-rules ()
    ((_ n u) (make-mce (replicate-m n u)))))

;; control -> [bytevector]
(define encode-control
  (lambda (c)
    (list2 (encode-pstr (control-name c))
           (encode-i16 (control-index c)))))

;; input -> [bytevector]
(define encode-input
  (lambda (i)
    (list2 (encode-i16 (input-ugen i))
           (encode-i16 (input-port i)))))

;; output -> [bytevector]
(define encode-output
  (lambda (o)
    (encode-u8 (rate-value (output-rate o)))))

;; [bytevector]
(define scgf
  (map encode-u8 (map char->integer (string->list "SCgf"))))

;; ugen -> [bytevector]
(define encode-ugen
  (lambda (u)
    (ugen-transform
     u
     (lambda (n r i o s d)
       (list
	(encode-pstr n)
	(encode-u8 (rate-value r))
	(encode-i16 (length i))
	(encode-i16 (length o))
	(encode-i16 s)
	(map encode-input i)
	(map encode-output o))))))

;; graphdef -> bytevector
(define encode-graphdef
  (lambda (g)
    (flatten-bytevectors
     (let ((n (graphdef-name g))
	   (c (graphdef-constants g))
	   (d (graphdef-defaults g))
	   (k (graphdef-controls g))
	   (u (graphdef-ugens g)))
       (list
	scgf
	(encode-i32 0)
	(encode-i16 1)
	(encode-pstr n)
	(encode-i16 (length c))
	(map encode-f32 c)
	(encode-i16 (length d))
	(map encode-f32 d)
	(encode-i16 (length k))
	(map encode-control k)
	(encode-i16 (length u))
	(map encode-ugen u))))))

;; node = ugen | proxy | control* | float

;; node -> mce
(define proxify
  (lambda (u)
    (cond
     ((mce? u) (make-mce (map proxify (mce-proxies u))))
     ((mrg? u) (make-mrg (proxify (mrg-left u)) (mrg-right u)))
     ((ugen? u) (let* ((o (ugen-outputs u))
		       (n (length o)))
		  (if (< n 2)
		      u
		      (make-mce (map (lambda (i) (make-proxy u i))
				      (enum-from-to 0 (- n 1)))))))
     (else (error "proxify" "illegal ugen" u)))))

;; string -> rate|list -> [node] -> node|nil -> int -> int -> uid -> ugen
(define construct-ugen
  (lambda (name rate inputs mce outputs special id)
    (let* ((inputs* (if (null? mce)
                        inputs
			(append inputs (mce-channels mce))))
	   (rate* (if (rate? rate)
                      rate
                      (rate-select (map (lambda (ix) (rate-of (list-ref inputs* ix))) rate))))
           (special* (if (null? special) 0 special))
           (id* (if (null? id) (make-uid 0) id))
	   (u (make-ugen
	       name
	       rate*
	       inputs*
	       (replicate outputs (make-output rate*))
	       special*
	       id*)))
      (proxify (mce-expand u)))))

(define mk-ugen
  (lambda (param)
    (apply construct-ugen param)))

;; ugen -> [node]
(define graph-nodes
  (lambda (u)
    (cond
     ((ugen? u) (cons u (concat-map graph-nodes (ugen-inputs u))))
     ((proxy? u) (cons u (graph-nodes (proxy-ugen u))))
     ((control*? u) (list1 u))
     ((number? u) (list1 u))
     ((mce? u) (concat (map graph-nodes (mce-proxies u))))
     ((mrg? u) (append (graph-nodes (mrg-left u)) (graph-nodes (mrg-right u))))
     (else (error "graph-nodes" "illegal value" u)))))

;; ugen -> [float]
(define graph-constants
  (lambda (u)
    (nub (filter number? (graph-nodes u)))))

;; ugen -> [control*]
(define graph-controls*
  (lambda (u)
    (nub (filter control*? (graph-nodes u)))))

;; ugen -> [ugen]
(define graph-ugens
  (lambda (u)
    (nub (reverse (filter ugen? (graph-nodes u))))))

;; ugen -> [node] -> [control] -> [ugen] -> ugen
(define ugen-close
  (lambda (u nn cc uu)
    (if (not (ugen-valid? u))
	(error "ugen-close" "invalid ugen" u)
	(make-ugen (ugen-name u)
		   (ugen-rate u)
		   (map (lambda (i)
			   (input*-to-input i nn cc uu))
			 (ugen-inputs u))
		   (ugen-outputs u)
		   (ugen-special u)
		   (ugen-id u)))))

;; ugen -> ugen
(define prepare-root
  (lambda (u)
    (cond
     ((mce? u) (mrg-n (mce-proxies u)))
     ((mrg? u) (make-mrg (prepare-root (mrg-left u))
                         (prepare-root (mrg-right u))))
     (else u))))

;; SYNTHDEF

;; [ugen] -> int
(define count-local-buf
  (lambda (uu)
    (length (filter (lambda (u) (equal? (ugen-name u) "LocalBuf")) uu))))

;; ugen|#f
(define implicit-max-local-bufs
  (lambda (uu)
    (let ((n (count-local-buf uu)))
      (if (> n 0) (cons n (max-local-bufs n)) #f))))

;; string -> ugen -> graphdef
(define synthdef
  (lambda (name pre-u)
    (let* ((u (prepare-root pre-u))
           (nn (graph-constants u))
	   (cc (graph-controls* u))
	   (uu (graph-ugens u))
	   (uu* (if (null? cc) uu (cons (implicit-ugen cc) uu)))
           (mx (implicit-max-local-bufs uu))
           (nn* (if mx (cons (car mx) nn) nn))
           (uu** (if mx (cons (cdr mx) uu*) uu*)))
      (make-graphdef
       name
       nn*
       (map control*-default cc)
       (map (lambda (c) (control*-to-control c cc)) cc)
       (map (lambda (u) (ugen-close u nn* cc uu**)) uu**)))))

(define synthdef-write
  (lambda (sy fn)
    (let ((fd (open-file-output-port fn)))
      (put-bytevector fd (encode-graphdef sy))
      (close-port fd))))

;; [control] -> ugen
(define implicit-ugen
  (lambda (cc)
    (make-ugen "Control"
	       kr
	       nil
	       (map make-output (replicate (length cc) kr))
	       0
	       (make-uid 0))))

;; node -> [node] -> int
(define calculate-index
  (lambda (n nn)
    (let ((i (find-index (lambda (e) (equal? e n)) nn)))
      (if (not i)
	  (error "calculate-index" "not located" n nn)
	  i))))

;; float -> [node] -> input
(define number-to-input
  (lambda (n nn)
    (make-input -1 (calculate-index n nn))))

;; control* -> [control*] -> control
(define control*-to-control
  (lambda (c cc)
    (make-control (control*-name c) (calculate-index c cc))))

;; control* -> [control*] -> input
(define control*-to-input
  (lambda (c cc)
    (make-input 0 (calculate-index c cc))))

;; ugen -> [ugen] -> input
(define ugen-to-input
  (lambda (u uu)
    (make-input (calculate-index u uu) 0)))

;; proxy -> [ugen] -> input
(define proxy-to-input
  (lambda (p uu)
    (make-input (calculate-index (proxy-ugen p) uu)
		(proxy-port p))))

;; node -> [node] -> [control] -> [ugen] -> input
(define input*-to-input
  (lambda (i nn cc uu)
    (cond
     ((number? i) (number-to-input i nn))
     ((control*? i) (control*-to-input i cc))
     ((ugen? i) (ugen-to-input i uu))
     ((proxy? i) (proxy-to-input i uu))
     ((mrg? i) (input*-to-input (mrg-left i) nn cc uu))
     ((mce? i) (error "input*-to-input" "mce?" i))
     (else (error "input*-to-input" "illegal input" i)))))

;; MCE

(define mce*
  (lambda l
    (make-mce l)))

;; node -> [ugen]
(define mce-channels
  (lambda (u)
    (cond
     ((mce? u) (mce-proxies u))
     ((mrg? u) (let ((rs (mce-channels (mrg-left u))))
                 (cons (make-mrg (head rs) (mrg-right u)) (tail rs))))
     (else (list1 u)))))

;; mce|mrg -> int
(define mce-degree
  (lambda (m)
    (cond
     ((mce? m) (length (mce-proxies m)))
     ((mrg? m) (mce-degree (mrg-left m)))
     (else (error "mce-degree" "illegal input" m)))))

;; ugen -> bool
(define mce-required?
  (lambda (u)
    (not (null? (filter mce? (ugen-inputs u))))))

;; int -> node -> [node]
(define mce-extend
  (lambda (n i)
    (cond ((mce? i) (extend (mce-proxies i) n))
          ((mrg? i) (let ((rs (mce-extend n (mrg-left i))))
                      (cons (make-mrg (head rs) (mrg-right i)) (tail rs))))
          (else (replicate n i)))))

;; ugen -> mce
(define mce-transform
  (lambda (u)
    (ugen-transform
     u
     (lambda (n r i o s d)
       (let* ((f (lambda (i*) (make-ugen n r i* o s d)))
	      (m (maximum (map mce-degree (filter mce? i))))
	      (e (lambda (i) (mce-extend m i)))
	      (i* (transpose (map e i))))
	 (make-mce (map f i*)))))))

;; node -> node|mce
(define mce-expand
  (lambda (u)
    (cond ((mce? u) (make-mce (map mce-expand (mce-proxies u))))
          ((mrg? u) (make-mrg (mce-expand (mrg-left u)) (mrg-right u)))
          (else (if (mce-required? u)
                    (mce-transform u)
                    u)))))

;; TRANSPORT

;; port -> osc -> ()
(define async
  (lambda (fd m)
    (send fd m)
    (wait fd "/done")))

;; port -> ()
(define reset
  (lambda (fd)
    (send fd (bundle -1 (list (g-free-all1 0)
			      clear-sched
			      (g-new1 1 0 0))))))

;; (socket -> a) -> a
(define with-udp-sc3
  (lambda (f)
    (let* ((fd (udp:open "127.0.0.1" 57110))
	   (r (f fd)))
      (udp:close fd)
      r)))

;; (socket -> a) -> a
(define with-tcp-sc3
  (lambda (f)
    (let* ((fd (tcp:open "127.0.0.1" 57110))
	   (r (f fd)))
      (tcp:close fd)
      r)))

;; (socket -> a) -> a
(define with-sc3 with-udp-sc3)

;; STATUS

;; [string]
(define status-fields
  (list "# UGens                     "
	"# Synths                    "
	"# Groups                    "
	"# Instruments               "
	"% CPU (Average)             "
	"% CPU (Peak)                "
	"Sample Rate (Nominal)       "
	"Sample Rate (Actual)        "))

;; osc -> [string]
(define status-format
  (lambda (r)
    (cons "***** SuperCollider Server Status *****"
	  (zip-with string-append
		    status-fields
		    (map number->string (tail (tail r)))))))

;; port -> [string]
(define server-status
  (lambda (fd)
    (send fd status)
    (let ((r (wait fd "/status.reply")))
      (status-format r))))

;; port -> ()
(define display-server-status
  (lambda (fd)
    (newline)
    (for-each display (intersperse "\n" (server-status fd)))
    (newline)))

;; port -> int -> number
(define server-status-field
  (lambda (fd n)
    (send fd status)
    (let ((r (wait fd "/status.reply")))
      (list-ref r n))))

;; port -> float
(define server-sample-rate-nominal
  (lambda (s)
    (server-status-field s 8)))

;; port -> float
(define server-sample-rate-actual
  (lambda (s)
    (server-status-field s 9)))

;; port -> string -> ugen -> ()
(define send-synth
  (lambda (fd name u)
    (async fd (d-recv (encode-graphdef (synthdef name u))))))

(define play-at
  (lambda (fd u nid act grp)
    (send-synth fd "anonymous" u)
    (send fd (s-new0 "anonymous" nid act grp))))

;; ((socket -> a) -> a) -> (ugen -> ())
(define audition-using
  (lambda (f)
    (lambda (u)
      (f
       (lambda (fd)
	 (play fd u))))))

;; ugen -> ()
(define audition (audition-using with-sc3))

;; RANDOM

;; float -> float -> float
(define random
  (lambda (a b)
    (+ (* (srfi:random-real) (- b a)) a)))

;; int -> int -> int
(define i-random
  (lambda (l r)
    (+ l (srfi:random-integer (- r l)))))

;; TIME

;; () -> float
(define utc
  (lambda ()
    (let ((t (srfi:current-time srfi:time-utc)))
      (+ (srfi:time-second t)
         (/ (srfi:time-nanosecond t) 1e9)))))

;; Local Variables:
;; truncate-lines:t
;; End:
