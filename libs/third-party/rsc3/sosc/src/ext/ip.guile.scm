(define-record-type udp* (fields s h p))

;; any -> bool
(define udp:socket? udp*?)

;; string -> int -> socket
(define udp:open
  (lambda (h p)
    (let ((s (socket PF_INET SOCK_DGRAM 0)))
      (connect s AF_INET (inet-pton AF_INET h) p)
      (make-udp* s h p))))

;; socket -> bytevector -> ()
(define udp:send
  (lambda (t b)
    (let ((s (udp*-s t)))
      (send s b))))

;; socket -> int -> maybe bytevector
(define udp:recv
  (lambda (u)
    (let ((b (make-bytevector (* 8192 4))))
      (recv! (udp*-s u) b)
      b)))

;; socket -> ()
(define udp:close
  (lambda (t)
    (close-port (udp*-s t))))

;; TCP NOT IMPLEMENTED (UNLESS BY CHANCE...)

(define-record-type tcp* (fields s h p))

;; any -> bool
(define tcp:socket? tcp*?)

;; string -> int -> socket
(define tcp:open
  (lambda (h p)
    (let ((s (socket PF_INET SOCK_STREAM 0)))
      (connect s AF_INET (inet-pton AF_INET h) p)
      (make-tcp* s h p))))

;; socket -> bytevector -> ()
(define tcp:send
  (lambda (t b)
    (let ((s (tcp*-s t)))
      (send s b))))

;; socket -> int -> maybe bytevector
(define tcp:read
  (lambda (u n)
    (let ((b (make-bytevector n)))
      (recv! (tcp*-s u) b)
      b)))

;; socket -> ()
(define tcp:close
  (lambda (t)
    (close-port (tcp*-s t))))

