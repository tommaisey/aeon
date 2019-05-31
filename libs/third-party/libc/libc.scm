(library (libc libc)
  (export 
    c-a64l
    c-abort
    c-abs
    c-atexit
    c-atof
    c-atoi
    c-atol
    c-bsearch
    c-calloc
    c-div
    c-drand48
    c-ecvt
    c-erand48
    c-exit
    c-fcvt
    c-free
    c-gcvt
    c-getenv
    c-getsubopt
    c-grantpt
    c-initstate
    c-l64a
    c-labs
    c-lcong48
    c-ldiv
    c-lrand48
    c-malloc
    c-mblen
    c-mbstowcs
    c-mbtowc
    c-mktemp
    c-mkstemp
    c-mrand48
    c-nrand48
    c-ptsname
    c-putenv
    c-qsort
    c-rand
    c-rand-r
    c-random
    c-realloc
    c-realpath
    c-setstate
    c-srand
    c-srand48
    c-srandom
    c-strtod
    c-strtol
    c-strtoul
    c-system
    c-unlockpt
    c-wcstombs
    c-wctomb
    c-acos
    c-asin
    c-atan
    c-atan2
    c-ceil
    c-cos
    c-cosh
    c-exp
    c-fabs
    c-floor
    c-fmod
    c-frexp
    c-ldexp
    c-log
    c-log10
    c-modf
    c-pow
    c-sin
    c-sinh
    c-sqrt
    c-tan
    c-tanh
    c-isnan
    c-clearerr
    c-ctermid
    c-cuserid
    c-fclose
    c-fdopen
    c-feof
    c-ferror
    c-fflush
    c-fgetc
    c-fgetpos
    c-fgets
    c-fileno
    c-flockfile
    c-fopen
    c-fprintf
    c-fputc
    c-fputs
    c-fread
    c-freopen
    c-fscanf
    c-fseek
    c-fsetpos
    c-ftell
    c-ftrylockfile
    c-funlockfile
    c-fwrite
    c-getc
    c-getchar
    c-getc-unlocked
    c-getchar-unlocked
    c-getopt
    c-gets
    c-getw
    c-pclose
    c-perror
    c-popen
    c-printf
    c-putc
    c-putchar
    c-putc-unlocked
    c-putchar-unlocked
    c-puts
    c-putw
    c-remove
    c-rename
    c-rewind
    c-scanf
    c-setbuf
    c-setvbuf
    c-snprintf
    c-sprintf
    c-sscanf
    c-tempnam
    c-tmpfile
    c-tmpnam
    c-ungetc
    c-memccpy
    c-memchr
    c-memcmp
    c-memcpy
    c-memmove
    c-memset
    c-strcat
    c-strchr
    c-strcmp
    c-strcoll
    c-strcpy
    c-strcspn
    c-strdup
    c-strerror
    c-strlen
    c-strncat
    c-strncmp
    c-strncpy
    c-strpbrk
    c-strrchr
    c-strspn
    c-strstr
    c-strtok
    c-strtok-r
    c-strxfrm
    c-strndup
    c-bcmp
    c-bcopy
    c-bzero
    c-ffs
    c-index
    c-rindex
    c-strcasecmp
    c-strncasecmp
    c-inet-addr
    c-htons
    ;; ftype
    in-addr
    sockaddr-in
    socklen-t)
  (import (scheme))
      
  (define lib-name
    (case (machine-type)
      ((arm32le) "libc.so")
      ((i3nt ti3nt) "crtdll.dll")
      ((a6nt ta6nt) "msvcrt.dll")
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

  (define-ftype div_t
    (struct
      [quot int]
      [rem int]))
  
  (define-ftype ldiv_t
    (struct
      [quot long]
      [rem long]))

  ;;long c_a64l(char*)
  (def-function c-a64l
              "a64l" (string) long)

  ;;void c_abort(void)
  (def-function c-abort
              "abort" () void)

  ;;int c_abs(int)
  (def-function c-abs
              "abs" (int) int)

  ;;int c_atexit()
  (def-function c-atexit
              "atexit" () int)

  ;;double c_atof(char*)
  (def-function c-atof
              "atof" (string) double)

  ;;int c_atoi(char*)
  (def-function c-atoi
              "atoi" (string) int)

  ;;long c_atol(char*)
  (def-function c-atol
              "atol" (string) long)

  ;;void* c_bsearch(void*, void*, size_t, size_t)
  (def-function c-bsearch
              "bsearch" (void* void* int int) void*)

  ;;void* c_calloc(size_t, size_t)
  (def-function c-calloc
              "calloc" (int int) void*)

  ;;div_t c_div(int, int)
  (def-function c-div
              "div" (int int) (* div_t))

  ;;double c_drand48(void)
  (def-function c-drand48
              "drand48" () double)

  ;;char* c_ecvt(double, int, int*, int*)
  (def-function c-ecvt
              "ecvt" (double int void* void*) string)

  ;;double c_erand48(unsigned)
  (def-function c-erand48
              "erand48" (int) double)

  ;;void c_exit(int)
  (def-function c-exit
              "exit" (int) void)

  ;;char* c_fcvt(double, int, int*, int*)
  (def-function c-fcvt
              "fcvt" (double int void* void*) string)

  ;;void c_free(void*)
  (def-function c-free
              "free" (void*) void)

  ;;char* c_gcvt(double, int, char*)
  (def-function c-gcvt
              "gcvt" (double int string) string)

  ;;char* c_getenv(char*)
  (def-function c-getenv
              "getenv" (string) string)

  ;;int c_getsubopt(char, char, char)
  (def-function c-getsubopt
              "getsubopt" (char char char) int)

  ;;int c_grantpt(int)
  (def-function c-grantpt
              "grantpt" (int) int)

  ;;char* c_initstate(unsigned int, char*, size_t)
  (def-function c-initstate
              "initstate" (int string int) string)

  ;;char* c_l64a(long)
  (def-function c-l64a
              "l64a" (long) string)

  ;;long c_labs(long int)
  (def-function c-labs
              "labs" (long int) long)

  ;;void c_lcong48(unsigned)
  (def-function c-lcong48
              "lcong48" (int) void)

  ;;ldiv_t c_ldiv(long int, long int)
  (def-function c-ldiv
              "ldiv" (long int long int) (* ldiv_t))

  ;;long c_lrand48(void)
  (def-function c-lrand48
              "lrand48" () long)

  ;;void* c_malloc(size_t)
  (def-function c-malloc
              "malloc" (int) void*)

  ;;int c_mblen(char*, size_t)
  (def-function c-mblen
              "mblen" (string int) int)

  ;;size_t c_mbstowcs(wchar_t*, char*, size_t)
  (def-function c-mbstowcs
              "mbstowcs" (string string int) int)

  ;;int c_mbtowc(wchar_t*, char*, size_t)
  (def-function c-mbtowc
              "mbtowc" (string string int) int)

  ;;char* c_mktemp(char*)
  (def-function c-mktemp
              "mktemp" (string) string)

  ;;int c_mkstemp(char*)
  (def-function c-mkstemp
              "mkstemp" (string) int)

  ;;long c_mrand48(void)
  (def-function c-mrand48
              "mrand48" () long)

  ;;long c_nrand48(unsigned)
  (def-function c-nrand48
              "nrand48" (int) long)

  ;;char* c_ptsname(int)
  (def-function c-ptsname
              "ptsname" (int) string)

  ;;int c_putenv(char*)
  (def-function c-putenv
              "putenv" (string) int)

  ;;void c_qsort(void*, size_t, size_t)
  (def-function c-qsort
              "qsort" (void* int int) void)

  ;;int c_rand(void)
  (def-function c-rand
              "rand" () int)

  ;;int c_rand_r(unsigned*)
  (def-function c-rand-r
              "rand_r" (void*) int)

  ;;long c_random(void)
  (def-function c-random
              "random" () long)

  ;;void* c_realloc(void*, size_t)
  (def-function c-realloc
              "realloc" (void* int) void*)

  ;;char* c_realpath(char*, char*)
  (def-function c-realpath
              "realpath" (string string) string)

  ;;char* c_setstate(char*)
  (def-function c-setstate
              "setstate" (string) string)

  ;;void c_srand(unsigned int)
  (def-function c-srand
              "srand" (int) void)

  ;;void c_srand48(long int)
  (def-function c-srand48
              "srand48" (long int) void)

  ;;void c_srandom(unsigned)
  (def-function c-srandom
              "srandom" (int) void)

  ;;double c_strtod(char*, char)
  (def-function c-strtod
              "strtod" (string char) double)

  ;;long c_strtol(char*, char, int)
  (def-function c-strtol
              "strtol" (string char int) long)

  ;;unsigned c_strtoul(char*, char, int)
  (def-function c-strtoul
              "strtoul" (string char int) int)

  ;;int c_system(char*)
  (def-function c-system
              "system" (string) int)

  ;;int c_unlockpt(int)
  (def-function c-unlockpt
              "unlockpt" (int) int)

  ;;size_t c_wcstombs(char*, wchar_t*, size_t)
  (def-function c-wcstombs
              "wcstombs" (string string int) int)

  ;;int c_wctomb(char*, wchar_t)
  (def-function c-wctomb
              "wctomb" (string wchar_t) int)

  ;;double c_acos(double)
  (def-function c-acos
              "acos" (double) double)

  ;;double c_asin(double)
  (def-function c-asin
              "asin" (double) double)

  ;;double c_atan(double)
  (def-function c-atan
              "atan" (double) double)

  ;;double c_atan2(double, double)
  (def-function c-atan2
              "atan2" (double double) double)

  ;;double c_ceil(double)
  (def-function c-ceil
              "ceil" (double) double)

  ;;double c_cos(double)
  (def-function c-cos
              "cos" (double) double)

  ;;double c_cosh(double)
  (def-function c-cosh
              "cosh" (double) double)

  ;;double c_exp(double)
  (def-function c-exp
              "exp" (double) double)

  ;;double c_fabs(double)
  (def-function c-fabs
              "fabs" (double) double)

  ;;double c_floor(double)
  (def-function c-floor
              "floor" (double) double)

  ;;double c_fmod(double, double)
  (def-function c-fmod
              "fmod" (double double) double)

  ;;double c_frexp(double, int*)
  (def-function c-frexp
              "frexp" (double void*) double)

  ;;double c_ldexp(double, int)
  (def-function c-ldexp
              "ldexp" (double int) double)

  ;;double c_log(double)
  (def-function c-log
              "log" (double) double)

  ;;double c_log10(double)
  (def-function c-log10
              "log10" (double) double)

  ;;double c_modf(double, double*)
  (def-function c-modf
              "modf" (double void*) double)

  ;;double c_pow(double, double)
  (def-function c-pow
              "pow" (double double) double)

  ;;double c_sin(double)
  (def-function c-sin
              "sin" (double) double)

  ;;double c_sinh(double)
  (def-function c-sinh
              "sinh" (double) double)

  ;;double c_sqrt(double)
  (def-function c-sqrt
              "sqrt" (double) double)

  ;;double c_tan(double)
  (def-function c-tan
              "tan" (double) double)

  ;;double c_tanh(double)
  (def-function c-tanh
              "tanh" (double) double)

  ;;int c_isnan(double)
  (def-function c-isnan
              "isnan" (double) int)

  ;;void c_clearerr(FILE*)
  (def-function c-clearerr
              "clearerr" (void*) void)

  ;;char* c_ctermid(char*)
  (def-function c-ctermid
              "ctermid" (string) string)

  ;;char* c_cuserid(char*)
  (def-function c-cuserid
              "cuserid" (string) string)

  ;;int c_fclose(FILE*)
  (def-function c-fclose
              "fclose" (void*) int)

  ;;FILE* c_fdopen(int, char*)
  (def-function c-fdopen
              "fdopen" (int string) void*)

  ;;int c_feof(FILE*)
  (def-function c-feof
              "feof" (void*) int)

  ;;int c_ferror(FILE*)
  (def-function c-ferror
              "ferror" (void*) int)

  ;;int c_fflush(FILE*)
  (def-function c-fflush
              "fflush" (void*) int)

  ;;int c_fgetc(FILE*)
  (def-function c-fgetc
              "fgetc" (void*) int)

  ;;int c_fgetpos(FILE*, fpos_t*)
  (def-function c-fgetpos
              "fgetpos" (void* void*) int)

  ;;char* c_fgets(char*, int, FILE*)
  (def-function c-fgets
              "fgets" (string int void*) string)

  ;;int c_fileno(FILE*)
  (def-function c-fileno
              "fileno" (void*) int)

  ;;void c_flockfile(FILE*)
  (def-function c-flockfile
              "flockfile" (void*) void)

  ;;FILE* c_fopen(char*, char*)
  (def-function c-fopen
              "fopen" (string string) void*)

  ;;int c_fprintf(FILE*, char*)
  (def-function c-fprintf
              "fprintf" (void* string) int)

  ;;int c_fputc(int, FILE*)
  (def-function c-fputc
              "fputc" (int void*) int)

  ;;int c_fputs(char*, FILE*)
  (def-function c-fputs
              "fputs" (string void*) int)

  ;;size_t fread(void*, size_t, size_t, FILE*)
  (def-function c-fread
              "fread" (u8* int int void*) size_t)

  ;;FILE* c_freopen(char*, char*, FILE*)
  (def-function c-freopen
              "freopen" (string string void*) void*)

  ;;int c_fscanf(FILE*, char*)
  (def-function c-fscanf
              "fscanf" (void* string) int)

  ;;int c_fseek(FILE*, long int, int)
  (def-function c-fseek
              "fseek" (void* int int) int)

  ;;int c_fsetpos(FILE*, fpos_t*)
  (def-function c-fsetpos
              "fsetpos" (void* void*) int)

  ;;long c_ftell(FILE*)
  (def-function c-ftell
              "ftell" (void*) int)

  ;;int c_ftrylockfile(FILE*)
  (def-function c-ftrylockfile
              "ftrylockfile" (void*) int)

  ;;void c_funlockfile(FILE*)
  (def-function c-funlockfile
              "funlockfile" (void*) void)

  ;;size_t c_fwrite(void*, size_t, size_t, FILE*)
  (def-function c-fwrite
              "fwrite" (u8* int int void*) size_t)

  ;;int c_getc(FILE*)
  (def-function c-getc
              "getc" (void*) int)

  ;;int c_getchar(void)
  (def-function c-getchar
              "getchar" () int)

  ;;int c_getc_unlocked(FILE*)
  (def-function c-getc-unlocked
              "getc_unlocked" (void*) int)

  ;;int c_getchar_unlocked(void)
  (def-function c-getchar-unlocked
              "getchar_unlocked" () int)

  ;;int c_getopt(int, char, char*)
  (def-function c-getopt
              "getopt" (int char string) int)

  ;;char* c_gets(char*)
  (def-function c-gets
              "gets" (string) string)

  ;;int c_getw(FILE*)
  (def-function c-getw
              "getw" (void*) int)

  ;;int c_pclose(FILE*)
  (def-function c-pclose
              "pclose" (void*) int)

  ;;void c_perror(char*)
  (def-function c-perror
              "perror" (string) void)

  ;;FILE* c_popen(char*, char*)
  (def-function c-popen
              "popen" (string string) void*)

  ;;int c_printf(char*)
  (def-function c-printf
              "printf" (string) int)

  ;;int c_putc(int, FILE*)
  (def-function c-putc
              "putc" (int void*) int)

  ;;int c_putchar(int)
  (def-function c-putchar
              "putchar" (int) int)

  ;;int c_putc_unlocked(int, FILE*)
  (def-function c-putc-unlocked
              "putc_unlocked" (int void*) int)

  ;;int c_putchar_unlocked(int)
  (def-function c-putchar-unlocked
              "putchar_unlocked" (int) int)

  ;;int c_puts(char*)
  (def-function c-puts
              "puts" (string) int)

  ;;int c_putw(int, FILE*)
  (def-function c-putw
              "putw" (int void*) int)

  ;;int c_remove(char*)
  (def-function c-remove
              "remove" (string) int)

  ;;int c_rename(char*, char*)
  (def-function c-rename
              "rename" (string string) int)

  ;;void c_rewind(FILE*)
  (def-function c-rewind
              "rewind" (void*) void)

  ;;int c_scanf(char*)
  (def-function c-scanf
              "scanf" (string) int)

  ;;void c_setbuf(FILE*, char*)
  (def-function c-setbuf
              "setbuf" (void* string) void)

  ;;int c_setvbuf(FILE*, char*, int, size_t)
  (def-function c-setvbuf
              "setvbuf" (void* string int int) int)

  ;;int c_snprintf(char*, size_t, char*)
  (def-function c-snprintf
              "snprintf" (string int string) int)

  ;;int c_sprintf(char*, char*)
  (def-function c-sprintf
              "sprintf" (string string) int)

  ;;int c_sscanf(char*, char*)
  (def-function c-sscanf
              "sscanf" (string string) int)

  ;;char* c_tempnam(char*, char*)
  (def-function c-tempnam
              "tempnam" (string string) string)

  ;;FILE* c_tmpfile(void)
  (def-function c-tmpfile
              "tmpfile" () void*)

  ;;char* c_tmpnam(char*)
  (def-function c-tmpnam
              "tmpnam" (string) string)

  ;;int c_ungetc(int, FILE*)
  (def-function c-ungetc
              "ungetc" (int void*) int)

  ;;void* c_memccpy(void*, void*, int, size_t)
  (def-function c-memccpy
              "memccpy" (void* void* int int) void*)

  ;;void* c_memchr(void*, int, size_t)
  (def-function c-memchr
              "memchr" (void* int int) void*)

  ;;int c_memcmp(void*, void*, size_t)
  (def-function c-memcmp
              "memcmp" (void* void* int) int)

  ;;void* c_memcpy(void*, void*, size_t)
  (def-function c-memcpy
              "memcpy" (void* void* int) void*)

  ;;void* c_memmove(void*, void*, size_t)
  (def-function c-memmove
              "memmove" (void* void* int) void*)

  ;;void* c_memset(void*, int, size_t)
  (def-function c-memset
              "memset" (void* int int) void*)

  ;;char* c_strcat(char*, char*)
  (def-function c-strcat
              "strcat" (string string) string)

  ;;char* c_strchr(char*, int)
  (def-function c-strchr
              "strchr" (string int) string)

  ;;int c_strcmp(char*, char*)
  (def-function c-strcmp
              "strcmp" (string string) int)

  ;;int c_strcoll(char*, char*)
  (def-function c-strcoll
              "strcoll" (string string) int)

  ;;char* c_strcpy(char*, char*)
  (def-function c-strcpy
              "strcpy" (string string) string)

  ;;size_t c_strcspn(char*, char*)
  (def-function c-strcspn
              "strcspn" (string string) int)

  ;;char* c_strdup(char*)
  (def-function c-strdup
              "strdup" (string) string)

  ;;char* c_strerror(int)
  (def-function c-strerror
              "strerror" (int) string)

  ;;size_t c_strlen(char*)
  (def-function c-strlen
              "strlen" (string) int)

  ;;char* c_strncat(char*, char*, size_t)
  (def-function c-strncat
              "strncat" (string string int) string)

  ;;int c_strncmp(char*, char*, size_t)
  (def-function c-strncmp
              "strncmp" (string string int) int)

  ;;char* c_strncpy(char*, char*, size_t)
  (def-function c-strncpy
              "strncpy" (string string int) string)

  ;;char* c_strpbrk(char*, char*)
  (def-function c-strpbrk
              "strpbrk" (string string) string)

  ;;char* c_strrchr(char*, int)
  (def-function c-strrchr
              "strrchr" (string int) string)

  ;;size_t c_strspn(char*, char*)
  (def-function c-strspn
              "strspn" (string string) int)

  ;;char* c_strstr(char*, char*)
  (def-function c-strstr
              "strstr" (string string) string)

  ;;char* c_strtok(char*, char*)
  (def-function c-strtok
              "strtok" (string string) string)

  ;;char* c_strtok_r(char*, char*, char)
  (def-function c-strtok-r
              "strtok_r" (string string char) string)

  ;;size_t c_strxfrm(char*, char*, size_t)
  (def-function c-strxfrm
              "strxfrm" (string string int) int)

  ;;char* c_strndup(char* s ,size_t n)
  (def-function c-strndup
              "strndup" (string int) string)

  ;;int c_bcmp(void*, void*, size_t)
  (def-function c-bcmp
              "bcmp" (void* void* int) int)

  ;;void c_bcopy(void*, void*, size_t)
  (def-function c-bcopy
              "bcopy" (void* void* int) void)

  ;;void c_bzero(void*, size_t)
  (def-function c-bzero
              "bzero" (void* int) void)

  ;;int c_ffs(int)
  (def-function c-ffs
              "ffs" (int) int)

  ;;char* c_index(char*, int)
  (def-function c-index
              "index" (string int) string)

  ;;char* c_rindex(char*, int)
  (def-function c-rindex
              "rindex" (string int) string)

  ;;int c_strcasecmp(char*, char*)
  (def-function c-strcasecmp
              "strcasecmp" (string string) int)

  ;;int c_strncasecmp(char*, char*, size_t)
  (def-function c-strncasecmp
              "strncasecmp" (string string int) int)

  (def-function c-inet-addr
    "inet_addr" (string) unsigned-long)

  (def-function c-htons
    "htons" (unsigned-short) unsigned-short) 
  
  (define-ftype in-addr
    (struct
      (s-addr unsigned-int)))
  
  (define-ftype sockaddr-in
    (struct 
      (sin-family short)
      (sin-port unsigned-short)
      (sin-addr in-addr)
      (sin-zero (array 8 unsigned-8))))
  
  (define-ftype socklen-t unsigned-int)
)
