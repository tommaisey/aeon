(library (socket ffi) 
  (export
    AF_UNSPEC
    AF_UNIX
    AF_INET
    SOCK_STREAM     
    SOCK_DGRAM      
    SOCK_RAW      
    SOCK_RDM        
    SOCK_SEQPACKET
    SOCK_PACKET
    INADDR_ANY
    SOL_SOCKET
    SO_REUSEADDR
    SO_REUSEPORT
    SO_SNDBUF
    SO_RCVBUF
    SO_SNDLOWAT
    SO_RCVLOWAT
    SO_SNDTIMEO
    SO_RCVTIMEO
    SO_ERROR
    SO_TYPE
    IPPROTO_IP
    IPPROTO_TCP
    IPPROTO_UDP
    SOCKET_ERROR

    wsadata
    
    c-read
    c-write
    c-recv
    c-send
    c-sendto
    socket
    bind
    connect
    listen
    accept
    shutdown
    close
    closesocket
    wsastartup
    wsacleanup
    makeword
    check)
  (import
    (scheme)
    (libc libc))

  (define lib-name
    (case (machine-type)
      ((i3nt ti3nt a6nt ta6nt) "ws2_32.dll")
      ((a6osx i3osx ta6osx ti3osx)  "libc.dylib")
      ((a6le i3le ta6le ti3le) "libc.so.6")
      (else "libc.so")))

  (define lib (load-shared-object lib-name))

  (define-syntax def-function
    (syntax-rules ()
      ((_ name sym args ret)
       (define name
          (if (foreign-entry? sym)
            (foreign-procedure sym args ret)
            (lambda x (printf "error: ~a not found in ~a\n" sym lib-name)))))))


  (define AF_UNSPEC       0)  
  (define AF_UNIX         1)
  (define AF_INET         2)

  (define SOCK_STREAM      1)
  (define SOCK_DGRAM       2)
  (define SOCK_RAW         3)
  (define SOCK_RDM         4)
  (define SOCK_SEQPACKET   5)
  (define SOCK_PACKET      10)
  (define INADDR_ANY 0)

  (define SOL_SOCKET 0 )
  (define SO_REUSEADDR   #x0004)
  (define SO_REUSEPORT   #x0200)
  (define SO_SNDBUF	#x1001)
  (define SO_RCVBUF	#x1002)
  (define SO_SNDLOWAT	#x1003)
  (define SO_RCVLOWAT	#x1004)
  (define SO_SNDTIMEO	#x1005)
  (define SO_RCVTIMEO	#x1006)
  (define SO_ERROR	#x1007)
  (define SO_TYPE	#x1008)
  
  (define IPPROTO_IP 0)
  (define IPPROTO_TCP 6)
  (define IPPROTO_UDP 22)

  (define SOCKET_ERROR -1)


  (define-ftype wsadata
    (struct
      (wVersion unsigned-short)
      (wHighVersion unsigned-short)
      (szDescription (array 257 char))
      (szSystemStatus (array 129 char))
      (iMaxSockets unsigned-short)
      (iMaxUdpDg unsigned-short)
      (lpVendorInfo (* char))))


  (def-function socket
    "socket" (int int int) int)
  
  (def-function bind
    "bind" (int (* sockaddr-in) int) int)

  (def-function listen
    "listen" (int int) int)

  (def-function accept
    "accept" (int (* sockaddr-in) (* socklen-t)) int)

  (def-function connect
    "connect" (int (* sockaddr-in) int) int)

  (def-function c-read
    "read" (int u8* int) int)

  (def-function c-write
    "write" (int u8* int) int)

  (def-function c-recv
    "recv" (int u8* int int) int)

  (def-function c-send
    "send" (int u8* int int) int)

  (def-function c-sendto
    "sendto" (int u8* int int (* sockaddr-in) int) int)
    
  (def-function shutdown
    "shutdown" (int int) int)

  (def-function close
    "close" (int) int)

  (def-function closesocket
    "closesocket" (int) int)
  
  (def-function wsastartup
    "WSAStartup" (unsigned-short (* wsadata)) int)

  (def-function wsacleanup
    "WSACleanup" () int)

  (define makeword
    (lambda (low high)
      (bitwise-ior low (bitwise-arithmetic-shift-left high 8))))
  
  (define check
    (lambda (who x)
      (if (< x 0)
        (error who (format "return ~a" x))
          x))) 
)
