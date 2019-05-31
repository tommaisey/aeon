;; -----------------------------------------------------------
;; Socket functions from ip.ikarus.scm, reimplemented using 
;; Chez library socket (see above)

(define-record-type udp* (fields fd h p))

;; any -> bool
(define udp:socket? udp*?)

;; string -> int -> socket
(define udp:open
  (lambda (ip port)
    (let* ([fd (socket:socket AF_INET SOCK_DGRAM IPPROTO_IP)])
      (socket:bind fd AF_INET ip 0)
      (make-udp* fd ip port))))

;; socket -> bytevector -> ()
(define udp:send
  (lambda (t b)
    (let ([fd (udp*-fd t)] [ip (udp*-h t)] [port (udp*-p t)])
      (socket:sendto fd b AF_INET ip port))))

;; socket -> int -> maybe bytevector
(define udp:recv
  (lambda (u)
    (socket:read (udp*-fd u))))

;; socket -> ()
(define udp:close
  (lambda (u)
    (socket:close (udp*-fd u))
    (socket:cleanup)))

(define-record-type tcp* (fields fd h p))

;; any -> bool
(define tcp:socket? tcp*?)

;; string -> int -> socket
(define tcp:open 
  (lambda (ip port)
    (let* ([fd (socket:socket AF_INET SOCK_STREAM IPPROTO_IP)])
      (socket:connect fd AF_INET ip port)
      (make-tcp* fd ip port))))

;; socket -> bytevector -> ()
(define tcp:send 
  (lambda (t bv)
    (socket:write (tcp*-fd t) bv)))

;; socket -> int -> maybe bytevector
(define tcp:read 
  (lambda (t n)
    (socket:read (tcp*-fd t) n)))

;; socket -> ()
(define tcp:close 
  (lambda (t)
    (socket:close (tcp*-fd t))
    (socket:cleanup)))
