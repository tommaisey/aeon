;; bytevector -> int
(define decode-u8
  (lambda (v)
    (bytevector-u8-ref v 0)))

;; bytevector -> int
(define decode-u16
  (lambda (v)
    (bytevector-u16-ref v 0 (endianness big))))

;; bytevector -> int
(define decode-u32
  (lambda (v)
    (bytevector-u32-ref v 0 (endianness big))))

;; bytevector -> int
(define decode-u64
  (lambda (v)
    (bytevector-u64-ref v 0 (endianness big))))

;; bytevector -> int
(define decode-i8
  (lambda (v)
    (bytevector-s8-ref v 0)))

;; bytevector -> int
(define decode-i16
  (lambda (v)
    (bytevector-s16-ref v 0 (endianness big))))

;; bytevector -> int
(define decode-i32
  (lambda (v)
    (bytevector-s32-ref v 0 (endianness big))))

;; bytevector -> int
(define decode-i64
  (lambda (v)
    (bytevector-s64-ref v 0 (endianness big))))

;; bytevector -> double
(define decode-f32
  (lambda (v)
    (bytevector-ieee-single-ref v 0 (endianness big))))

;; bytevector -> double
(define decode-f64
  (lambda (v)
    (bytevector-ieee-double-ref v 0 (endianness big))))

;; bytevector -> string
(define decode-str
  (lambda (b)
    (utf8->string b)))

;; bytevector -> string
;;
;; (decode-pstr (flatten-bytevectors (encode-pstr "string")))
(define decode-pstr
  (lambda (v)
    (let* ((n (decode-u8 v))
	   (w (bytevector-section v 1 (+ n 1))))
      (decode-str w))))

;; bytevector -> string
(define decode-cstr
  (lambda (v)
    (let* ((n (bytevector-find-index v 0))
	   (w (bytevector-section v 0 n)))
      (decode-str w))))

;; int -> bytevector
(define encode-u8
  (lambda (n)
    (bytevector-make-and-set1
     bytevector-u8-set!
     1
     (exact n))))

;; int -> bytevector
(define encode-u16
  (lambda (n)
    (bytevector-make-and-set
     bytevector-u16-set!
     2
     (exact n))))

;; int -> bytevector
(define encode-u32
  (lambda (n)
    (bytevector-make-and-set
     bytevector-u32-set!
     4
     (exact n))))

;; int -> bytevector
(define encode-u64
  (lambda (n)
    (bytevector-make-and-set
     bytevector-u64-set!
     8
     (exact n))))

;; int -> bytevector
(define encode-i8
  (lambda (n)
    (bytevector-make-and-set1
     bytevector-s8-set!
     1
     (exact n))))

;; int -> bytevector
(define encode-i16
  (lambda (n)
    (bytevector-make-and-set
     bytevector-s16-set!
     2
     (exact n))))

;; int -> bytevector
(define encode-i32
  (lambda (n)
    (bytevector-make-and-set
     bytevector-s32-set!
     4
     (exact n))))

;; int -> bytevector
(define encode-i64
  (lambda (n)
    (bytevector-make-and-set
     bytevector-s64-set!
     8
     (exact n))))

;; double -> bytevector
(define encode-f32
  (lambda (n)
    (bytevector-make-and-set
     bytevector-ieee-single-set!
     4
     (inexact n))))

;; double -> bytevector
(define encode-f64
  (lambda (n)
    (bytevector-make-and-set
     bytevector-ieee-double-set!
     8
     (inexact n))))

;; string -> bytevector
(define encode-str
  (lambda (s)
    (string->utf8 s)))

;; string -> bytevector
(define encode-pstr
  (lambda (s)
    (let* ((b (encode-str s))
	   (n (encode-u8 (bytevector-length b))))
      (list n b))))

;; string -> [bytevector]
(define encode-cstr
  (lambda (s)
    (let* ((b (encode-str s))
	   (z (encode-u8 0)))
      (list b z))))

;; port -> int -> bytevector
(define read-bstr
  (lambda (p n)
    (get-bytevector-n p n)))

;; port -> string
(define read-pstr
  (lambda (p)
    (let* ((n (lookahead-u8 p))
	   (v (read-bstr p (+ n 1))))
      (decode-pstr v))))

;; port -> string
(define read-cstr
  (lambda (p)
    (let loop ((l nil)
	       (b (get-u8 p)))
      (if (= b 0)
	  (list->string (map integer->char (reverse l)))
	  (loop (cons b l) (get-u8 p))))))

;; port -> int
(define read-i8
  (lambda (p)
    (decode-i8 (read-bstr p 1))))

;; port -> int
(define read-u8
  (lambda (p)
    (decode-u8 (read-bstr p 1))))

;; port -> int
(define read-i16
  (lambda (p)
    (decode-i16 (read-bstr p 2))))

;; port -> int
(define read-u16
  (lambda (p)
    (decode-u16 (read-bstr p 2))))

;; port -> int
(define read-i32
  (lambda (p)
    (decode-i32 (read-bstr p 4))))

;; port -> int
(define read-u32
  (lambda (p)
    (decode-u32 (read-bstr p 4))))

;; port -> int
(define read-i64
  (lambda (p)
    (decode-i64 (read-bstr p 8))))

;; port -> int
(define read-u64
  (lambda (p)
    (decode-u64 (read-bstr p 8))))

;; port -> double
(define read-f32
  (lambda (p)
    (decode-f32 (read-bstr p 4))))

;; port -> double
(define read-f64
  (lambda (p)
    (decode-f64 (read-bstr p 8))))

;; int
(define seconds-from-1900-to-1970
  (+ (* 70 365 24 60 60) (* 17 24 60 60)))

;; double -> int
(define ntpr->ntp
  (lambda (n)
    (exact (round (* n (expt 2 32))))))

;; double -> double
(define utc->ntpr
  (lambda (n)
    (+ n seconds-from-1900-to-1970)))

;; int -> double
(define ntp->utc
  (lambda (n)
    (- (/ n (expt 2 32)) seconds-from-1900-to-1970)))

;; port -> string
(define read-ostr
  (lambda (p)
    (let* ((s (read-cstr p))
	   (n (mod (cstring-length s) 4))
	   (i (- 4 (mod n 4))))
      (if (not (= n 0))
	  (read-bstr p i)
	  #f)
      s)))

;; port -> bytevector
(define read-obyt
  (lambda (p)
    (let* ((n (read-i32 p))
	   (b (read-bstr p n))
	   (i (- 4 (mod n 4))))
      (if (not (= n 0))
	  (read-bstr p i)
	  #f)
      b)))

;; datum = int | double | string | bytevector

;; port -> char -> datum
(define read-value
  (lambda (p t)
    (cond
     ((equal? t oI32) (read-i32 p))
     ((equal? t oI64) (read-i64 p))
     ((equal? t oU64) (read-u64 p))
     ((equal? t oF32) (read-f32 p))
     ((equal? t oF64) (read-f64 p))
     ((equal? t oSTR) (read-ostr p))
     ((equal? t oBYT) (read-obyt p))
     ((equal? t oMID) (read-u32 p))
     (else (error "read-value" "bad type" t)))))

;; port -> [char] -> [datum]
(define read-arguments
  (lambda (p types)
    (if (null? types)
	'()
	(cons (read-value p (car types))
	      (read-arguments p (cdr types))))))

;; port -> (string:[datum])
(define read-message
  (lambda (p)
    (let* ((address (read-ostr p))
	   (types (read-ostr p)))
      (cons address
	    (read-arguments p (cdr (string->list types)))))))

;; port -> (utc:[message])
(define read-bundle
  (lambda (p)
    (let ((bundletag (read-ostr p))
	  (timetag (ntp->utc (read-u64 p)))
	  (parts (list)))
      (if (not (equal? bundletag "#bundle"))
	  (error "read-bundle"
		 "illegal bundle tag"
		 bundletag)
	  (cons timetag
		(let loop ((parts (list)))
		  (if (eof-object? (lookahead-u8 p))
		      (reverse parts)
		      (begin
			;; We have no use for the message size...
			(read-i32 p)
			(loop (cons (read-packet p) parts))))))))))

;; byte
(define hash-u8
  (char->integer #\#))

;; port -> osc
(define read-packet
  (lambda (p)
    (if (equal? (lookahead-u8 p) hash-u8)
	(read-bundle p)
	(read-message p))))

;; bytevector -> osc
(define decode-osc
  (lambda (b)
    (with-input-from-bytevector b read-packet)))

;; [byte] -> ()
(define osc-display
  (lambda (l)
    (zip-with
     (lambda (b n)
       (display (list (number->string b 16) (integer->char b)))
       (if (= 3 (mod n 4))
	   (newline)
	   (display #\space)))
     l
     (enum-from-to 0 (- (length l) 1)))))

;; string -> int
(define cstring-length
  (lambda (s)
    (+ 1 (string-length s))))

;; int -> int
;; (equal? (map osc-align (enum-from-to 0 7)) (list 0 3 2 1 0 3 2 1))
(define osc-align
  (lambda (n)
    (- (fxand (+ n 3) (fxnot 3)) n)))

;; int -> [bytevector]
(define padding-of
  (lambda (n) (replicate (osc-align n) (encode-u8 0))))

;; string -> [bytevector]
(define encode-string
  (lambda (s)
    (list (encode-cstr s) (padding-of (cstring-length s)))))

;; bytevector -> [bytevector]
(define encode-bytes
  (lambda (b)
    (let ((n (bytevector-length b)))
      (list (encode-i32 n)
	    b
            (padding-of n)))))

;; datum -> bytevector
(define encode-value
  (lambda (e)
    (cond ((number? e) (if (integer? e)
			   (encode-i32 e)
			   (encode-f32 e)))
	  ((string? e) (encode-string e))
	  ((bytevector? e) (encode-bytes e))
	  (else (error "encode-value" "illegal value" e)))))

;; [datum] -> bytevector
(define encode-types
  (lambda (l)
    (encode-string
     (list->string
      (cons #\,
	    (map (lambda (e)
		    (cond ((number? e) (if (integer? e) oI32 oF32))
			  ((string? e) oSTR)
			  ((bytevector? e) oBYT)
			  (else (error "encode-types" "type?" e))))
		 l))))))

;; osc -> [bytevector]
(define encode-message
  (lambda (m)
    (list (encode-string (car m))
	  (encode-types (cdr m))
	  (map encode-value (cdr m)))))

;; osc -> [bytevector]
(define encode-bundle-ntp
  (lambda (b)
    (list (encode-string "#bundle")
	  (encode-u64 (ntpr->ntp (car b)))
	  (map (lambda (e)
		  (if (message? e)
		      (encode-bytes (encode-osc e))
		      (error "encode-bundle" "illegal value" e)))
		(cdr b)))))

;; osc -> [bytevector]
(define encode-bundle
  (lambda (b)
    (encode-bundle-ntp (cons (utc->ntpr (car b)) (cdr b)))))

;; osc -> bytevector
(define encode-osc
  (lambda (p)
    (flatten-bytevectors
     (if (bundle? p)
	 (encode-bundle p)
	 (encode-message p)))))

;; any | [any] -> datum | [datum]
(define purify
  (lambda (e)
    (cond ((or3 (number? e) (string? e) (bytevector? e)) e)
	  ((list? e) (map purify e))
	  ((symbol? e) (symbol->string e))
	  ((boolean? e) (if e 1 0))
	  (else (error "purify" "illegal input" e)))))

;; char
(define oI32 #\i)
(define oI64 #\h)
(define oU64 #\t)
(define oF32 #\f)
(define oF64 #\d)
(define oSTR #\s)
(define oBYT #\b)
(define oMID #\m)

;; string -> [any] -> osc
(define message
  (lambda (c l)
    (if (string? c)
	(cons c l)
	(error "message" "illegal address"))))

;; float -> [any] -> osc
(define bundle
  (lambda (t l)
    (if (number? t)
	(cons t l)
	(error "bundle" "illegal timestamp" t))))

;; osc -> bool
(define message?
  (lambda (p)
    (string? (car p))))

;; osc -> bool
(define bundle?
  (lambda (p)
    (number? (car p))))

;; osc -> bool
(define verify-message
  (lambda (m)
    (and2 (string? (car m))
	  (all (lambda (e) (or (number? e)
			       (string? e)))
	       (cdr m)))))

;; osc -> bool
(define verify-bundle
  (lambda (b)
    (and2 (integer? (car b))
	  (all (lambda (e) (or2 (verify-message e)
				(and2 (verify-bundle e)
				      (>= (car e) (car b)))))
	       (cdr b)))))

;; osc -> bool
(define verify-packet
  (lambda (p)
    (or2 (verify-message p)
	 (verify-bundle p))))
