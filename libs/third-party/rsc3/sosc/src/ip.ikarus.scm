(define-record-type udp* (fields i o h p))

;; any -> bool
(define udp:socket?
  udp*?)

;; string -> int -> socket
(define udp:open
  (lambda (h p)
    (let-values
     (((i o) (ikarus:udp-connect h (number->string p))))
     ;;(output-port-buffer-mode (buffer-mode none))
     (make-udp* i o h p))))

;; socket -> bytevector -> ()
(define udp:send
  (lambda (t b)
    (let ((o (udp*-o t)))
      (put-bytevector o b)
      (flush-output-port o))))

;; socket -> int -> maybe bytevector
(define udp:recv
  (lambda (u)
    (get-bytevector-some (udp*-i u))))

;; socket -> ()
(define udp:close
  (lambda (t)
    (close-port (udp*-i t))
    (close-port (udp*-o t))))

(define-record-type tcp* (fields i o h p))

;; any -> bool
(define tcp:socket?
  tcp*?)

;; string -> int -> socket
(define tcp:open
  (lambda (h p)
    (let-values
     (((i o) (ikarus:tcp-connect h (number->string p))))
     ;;(output-port-buffer-mode (buffer-mode none))
     (make-tcp* i o h p))))

;; socket -> bytevector -> ()
(define tcp:send
  (lambda (t b)
    (let ((o (tcp*-o t)))
      (put-bytevector o b)
      (flush-output-port o))))

;; socket -> int -> maybe bytevector
(define tcp:read
  (lambda (t n)
    (get-bytevector-n (tcp*-i t) n)))

;; socket -> ()
(define tcp:close
  (lambda (t)
    (close-port (tcp*-i t))
    (close-port (tcp*-o t))))
