(import 
  (socket ffi)
  (libc libc))

(define socket-fd (socket AF_INET SOCK_DGRAM IPPROTO_IP))

(define ip "127.0.0.1")
(define port 8885)

(define serv-addr 
  (make-ftype-pointer sockaddr-in
    (foreign-alloc
      (ftype-sizeof sockaddr-in))))

(ftype-set! sockaddr-in (sin-family) serv-addr AF_INET)
(ftype-set! sockaddr-in (sin-addr s-addr) serv-addr (c-inet-addr ip))
(ftype-set! sockaddr-in (sin-port) serv-addr (c-htons port))

(bind socket-fd serv-addr (ftype-sizeof sockaddr-in))

(listen socket-fd 10)

(printf "listening on ~a\n" port)

(define clnt-addr 
  (make-ftype-pointer sockaddr-in
    (foreign-alloc
      (ftype-sizeof sockaddr-in))))

(define clnt-addr-size 
  (make-ftype-pointer socklen-t
    (foreign-alloc
      (ftype-sizeof socklen-t))))

(ftype-set! socklen-t () clnt-addr-size (ftype-sizeof sockaddr-in))

(define client-fd (accept socket-fd clnt-addr clnt-addr-size))

; (display client-fd)

(let* ([s "Hello World!"]
       [bv (string->utf8 s)])
  (c-write client-fd bv (bytevector-length bv)))

(close client-fd)
(close socket-fd)
