(import 
  (socket ffi)
  (libc libc))

(define socket-fd (socket AF_INET SOCK_DGRAM IPPROTO_IP))
(define port 8885)
(define ip "127.0.0.1")

(define serv-addr 
  (make-ftype-pointer sockaddr-in
    (foreign-alloc
      (ftype-sizeof sockaddr-in))))
        
(ftype-set! sockaddr-in (sin-family) serv-addr AF_INET)
(ftype-set! sockaddr-in (sin-addr s-addr) serv-addr (c-inet-addr ip))
(ftype-set! sockaddr-in (sin-port) serv-addr (c-htons port))

(define result (connect socket-fd serv-addr (ftype-sizeof sockaddr-in)))

(define buff (make-bytevector 1024))
(let* ([n (c-read socket-fd buff (bytevector-length buff))]
       [bv (make-bytevector n)])
  (bytevector-copy! buff 0 bv 0 n)
  (printf "client: ~a\n" (utf8->string bv)))

(display "connection result: ")
(display result)

(close socket-fd)