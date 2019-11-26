;; any -> bool
(define udp:socket?
  ypsilon:socket?)

;; string -> int -> socket
(define udp:open
  (lambda (h p)
    (ypsilon:make-socket
     h
     (number->string p)
     ypsilon:AF_INET
     ypsilon:SOCK_DGRAM
     (+ ypsilon:AI_V4MAPPED ypsilon:AI_ADDRCONFIG)
     0)))

;; socket -> bytevector -> ()
(define udp:send
  (lambda (s b)
    (ypsilon:socket-send s b 0)))

;; socket -> int -> maybe bytevector
(define udp:recv
  (lambda (s)
    (ypsilon:socket-recv s)))

;; socket -> ()
(define udp:close
  (lambda (s)
    (ypsilon:socket-close s)))
