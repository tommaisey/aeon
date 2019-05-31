(import (prefix (srfi :1 lists) srfi:)
        (prefix (srfi :13 strings) srfi:))

;; A <list> that maps symbolic binary operator names to integer
;; indexes.  The names and ordering are (and *must* remain) those used
;; as enumeration constants in the scsynth source.
(define binaryop-names '(Add Sub Mul IDiv FDiv Mod EQ NE LT GT LE GE Min Max BitAnd BitOr BitXor LCM GCD Round RoundUp Trunc Atan2 Hypot Hypotx Pow ShiftLeft ShiftRight UnsignedShift Fill Ring1 Ring2 Ring3 Ring4 DifSqr SumSqr SqrSum SqrDif AbsDif Thresh AMClip ScaleNeg Clip2 Excess Fold2 Wrap2 FirstArg RandRange ExpRandRange))

;; Evaluates to the 'SpecialIndex' for the symbolic binary operator
;; `name', or to '#f' if `name' is not a binary operator.
;;
;; (= (binaryop-index 'Mul) 2)
(define binaryop-index
  (lambda (name)
    (srfi:list-index (lambda (e) (eq? e name)) binaryop-names)))

;; Evaluate to the name of the binary operation defined by `index'.
;;
;; (eq? (binaryop-name 2) 'Mul)
(define binaryop-name
  (lambda (index)
    (srfi:list-ref binaryop-names index)))

;; A <list> that maps symbolic unary operation names to integer indexes.
(define unaryop-names '(Neg Not IsNil NotNil BitNot Abs AsFloat AsInt Ceil Floor Frac Sign Squared Cubed Sqrt Exp Recip MIDICPS CPSMIDI MIDIRatio RatioMIDI DbAmp AmpDb OctCPS CPSOct Log Log2 Log10 Sin Cos Tan ArcSin ArcCos ArcTan SinH CosH TanH _Rand Rand2 _LinRand BiLinRand Sum3Rand Distort SoftClip Coin DigitValue Silence Thru RectWindow HanWindow WelchWindow TriWindow _Ramp SCurve))

;; Evaluates to the 'SpecialIndex' for the symbolic unary operator
;; `name', or to '#f' if `name' is not a binary operator.  The names
;; are those used as enueration constants in the SC3 source.
;;
;; (= (unaryop-index 'Exp) 15)
(define unaryop-index
  (lambda (name)
    (srfi:list-index (lambda (e) (eq? e name)) unaryop-names)))

;; Evaluate to the symbolic name of the binary operation defined by
;; `index'.
;;
;; (eq? (unaryop-name 15) 'Exp)
(define unaryop-name
  (lambda (index)
    (srfi:list-ref unaryop-names index)))

;; A UGen user-name is usually just the UGen name with a rate suffix,
;; but for Binary and Unary OpUGens it is the name of the operation
;; without any rate suffix, which is determined by the special index.
;;
;; (eq? (user-name 'BinaryOpUGen 2) 'Mul)
;; (eq? (user-name 'UnaryOpUGen 15) 'Exp)
(define user-name
  (lambda (name special)
    (cond
     ((eq? name 'BinaryOpUGen) (binaryop-name special))
     ((eq? name 'UnaryOpUGen)  (unaryop-name special))
     (else                     name))))

;; USpecs describe UGens.
(define-record-type uspec
  (fields name inputs outputs special rates))

;; name = either a symbol or a duple (Name Implementation).  The
;; naming situation is complicated by two issues: all unary and binary
;; math operators are implemented by the UGens 'UnaryMathOp' and
;; 'BinaryMathOp'.  There are name conflicts between the operator
;; names as given in the sc3 code base and provided UGens (Rand,
;; LinRand and Ramp).

;; input = a duple (Name Default).  If the default value of the last
;; input is a list that input is flattened and sent as multiple inputs
;; to the UGen, which must accept a variable number of inputs.

;; outputs = normally a list that describes the rate of each output
;; port, values are either rate specifiers or the symbol 'u' to
;; indicate the output rate should be the same as the UGen rate.  If
;; the output descriptor is instead an integer, the UGen has a
;; variable number of outputs, and the value indicates the input to
;; read the number of outputs from.  All outputs will be at the rate
;; of the UGen, and the input describing the number of outputs will be
;; removed from the input list.

;; special = an integer index defined only for unary and binary math
;; operators.

;; rates = a list of integer rate specifiers the UGen can operate at.
(define-syntax define-uspec
  (syntax-rules ()
    ((_ u n i o s r) (define u (make-uspec 'n 'i 'o 's 'r)))
    ((_ u n i o s)   (define u (make-uspec 'n 'i 'o 's '(0 1 2))))))

;; Name accessors.
;;
;; (eq? (uspec-name/ugen Mul.uspec) 'BinaryOpUGen)
;; (eq? (uspec-name/ugen SinOsc.uspec) 'SinOsc)
(define uspec-name/ugen
  (lambda (s)
    (let ((n (uspec-name s)))
      (if (list? n) (cadr n) n))))

;; (eq? (uspec-name/constructor Mul.uspec) 'Mul)
;; (eq? (uspec-name/constructor SinOsc.uspec) 'SinOsc)
(define uspec-name/constructor
  (lambda (s)
    (let ((n (uspec-name s)))
      (if (list? n) (car n) n))))

;; Input accessors.
(define uspec-input-name (lambda (i) (car i)))
(define uspec-input-default (lambda (i) (cadr i)))

;; Variadic predicates.
;;
;; (eq? (uspec-has-variable-inputs? SinOsc.uspec) #f)
;; (eq? (uspec-has-variable-inputs? Out.uspec) #t)
(define uspec-has-variable-inputs?
  (lambda (s)
    (list? (uspec-input-default (srfi:last (uspec-inputs s))))))

;; (eq? (uspec-has-variable-outputs? SinOsc.uspec) #f)
;; (eq? (uspec-has-variable-outputs? In.uspec) #t)
(define uspec-has-variable-outputs?
  (lambda (s)
    (not (list? (uspec-outputs s)))))

;; Construct a usage list for the USpec `s'.
;;
;; (equal? (uspec-usage SinOsc.uspec #f) '(SinOsc freq phase))
;; (equal? (uspec-usage SinOsc.uspec #t) '(SinOsc (freq 440.0) (phase 0.0)))
(define uspec-usage
  (lambda (s with-defaults)
    (cons (uspec-name/constructor s)
          (if with-defaults
              (uspec-inputs s)
              (map uspec-input-name (uspec-inputs s))))))

(define uspec-sclang-plain
  (lambda (s c r)
    (list
     " *" c "r { arg "
     (map
      (lambda (i)
        (list " " (uspec-input-name i) " = " (uspec-input-default i) ","))
      (uspec-inputs s))
     " mul = 1.0, add = 0.0;\n"
     "  ^this.multiNew('" r "'"
     (map
      (lambda (i)
        ", " (uspec-input-name i))
      (uspec-inputs s))
     ").madd(mul, add);\n}\n")))

(define uspec-sclang-variadic
  (lambda (s c r)
    (list
     " *" c "r { arg "
     (map
      (lambda (i)
        (list " " (uspec-input-name i) " = " (uspec-input-default i) ","))
      (srfi:drop-right (uspec-inputs s) 1))
     " " (uspec-input-name (last (uspec-inputs s))) ";\n"
     "  ^this.multiNewList(['" r "'"
     (map
      (lambda (i) (list ", " (uspec-input-name i)))
      (srfi:drop-right (uspec-inputs s) 1))
     "] ++ " (uspec-input-name (last (uspec-inputs s))) ");\n}\n")))

(define expr->string
  (lambda (e)
    (call-with-string-output-port (lambda (p) (display e p)))))

(define tree->string
  (lambda (e)
    (cond ((list? e) (srfi:string-concatenate (map tree->string e)))
          (else (expr->string e)))))

;; Construct a tree describing the SC3 class implementation of the
;; USpec 's'.  Does not support variable output USpecs, however it
;; does raise an error...
;;
;; (display (tree->string (uspec-sclang SinOsc.uspec)))
;; (display (tree->string (uspec-sclang Out.uspec)))
(define uspec-sclang
  (lambda (s)
    (if (uspec-has-variable-outputs? s)
        (error "uspec-sclang: variable outputs"))
    (let ((constructor (if (uspec-has-variable-inputs? s)
                           uspec-sclang-variadic
                           uspec-sclang-plain)))
      (list (uspec-name/constructor s) " : UGen {\n"
            (constructor s 'a 'audio)
            (constructor s 'k 'control)
            "}\n"))))

;; (eq? (symbol-append 'p 'q) 'pq)
(define symbol-append
  (lambda (p q)
    (string->symbol (string-append (symbol->string p) (symbol->string q)))))

;; (eq? (symbol-append-string 'p "q") 'pq)
(define symbol-append-string
  (lambda (p q)
    (string->symbol (string-append (symbol->string p) q))))

;; Unary USpecs are generated.
(define gen-unaryop-uspec
  (lambda ()
    (map
     (lambda (name index)
       `(define-uspec ,(symbol-append-string name ".uspec")
          (,name UnaryOpUGen) ((input 0.0)) (u) ,index (0 1 2 3)))
     unaryop-names
     (srfi:iota (length unaryop-names)))))

(define-uspec Neg.uspec (Neg UnaryOpUGen) ((input 0.0)) (u) 0 (0 1 2 3))
(define-uspec Not.uspec (Not UnaryOpUGen) ((input 0.0)) (u) 1 (0 1 2 3))
(define-uspec IsNil.uspec (IsNil UnaryOpUGen) ((input 0.0)) (u) 2 (0 1 2 3))
(define-uspec NotNil.uspec (NotNil UnaryOpUGen) ((input 0.0)) (u) 3 (0 1 2 3))
(define-uspec BitNot.uspec (BitNot UnaryOpUGen) ((input 0.0)) (u) 4 (0 1 2 3))
(define-uspec Abs.uspec (Abs UnaryOpUGen) ((input 0.0)) (u) 5 (0 1 2 3))
(define-uspec AsFloat.uspec (AsFloat UnaryOpUGen) ((input 0.0)) (u) 6 (0 1 2 3))
(define-uspec AsInt.uspec (AsInt UnaryOpUGen) ((input 0.0)) (u) 7 (0 1 2 3))
(define-uspec Ceil.uspec (Ceil UnaryOpUGen) ((input 0.0)) (u) 8 (0 1 2 3))
(define-uspec Floor.uspec (Floor UnaryOpUGen) ((input 0.0)) (u) 9 (0 1 2 3))
(define-uspec Frac.uspec (Frac UnaryOpUGen) ((input 0.0)) (u) 10 (0 1 2 3))
(define-uspec Sign.uspec (Sign UnaryOpUGen) ((input 0.0)) (u) 11 (0 1 2 3))
(define-uspec Squared.uspec (Squared UnaryOpUGen) ((input 0.0)) (u) 12 (0 1 2 3))
(define-uspec Cubed.uspec (Cubed UnaryOpUGen) ((input 0.0)) (u) 13 (0 1 2 3))
(define-uspec Sqrt.uspec (Sqrt UnaryOpUGen) ((input 0.0)) (u) 14 (0 1 2 3))
(define-uspec Exp.uspec (Exp UnaryOpUGen) ((input 0.0)) (u) 15 (0 1 2 3))
(define-uspec Recip.uspec (Recip UnaryOpUGen) ((input 0.0)) (u) 16 (0 1 2 3))
(define-uspec MIDICPS.uspec (MIDICPS UnaryOpUGen) ((input 0.0)) (u) 17 (0 1 2 3))
(define-uspec CPSMIDI.uspec (CPSMIDI UnaryOpUGen) ((input 0.0)) (u) 18 (0 1 2 3))
(define-uspec MIDIRatio.uspec (MIDIRatio UnaryOpUGen) ((input 0.0)) (u) 19 (0 1 2 3))
(define-uspec RatioMIDI.uspec (RatioMIDI UnaryOpUGen) ((input 0.0)) (u) 20 (0 1 2 3))
(define-uspec DbAmp.uspec (DbAmp UnaryOpUGen) ((input 0.0)) (u) 21 (0 1 2 3))
(define-uspec AmpDb.uspec (AmpDb UnaryOpUGen) ((input 0.0)) (u) 22 (0 1 2 3))
(define-uspec OctCPS.uspec (OctCPS UnaryOpUGen) ((input 0.0)) (u) 23 (0 1 2 3))
(define-uspec CPSOct.uspec (CPSOct UnaryOpUGen) ((input 0.0)) (u) 24 (0 1 2 3))
(define-uspec Log.uspec (Log UnaryOpUGen) ((input 0.0)) (u) 25 (0 1 2 3))
(define-uspec Log2.uspec (Log2 UnaryOpUGen) ((input 0.0)) (u) 26 (0 1 2 3))
(define-uspec Log10.uspec (Log10 UnaryOpUGen) ((input 0.0)) (u) 27 (0 1 2 3))
(define-uspec Sin.uspec (Sin UnaryOpUGen) ((input 0.0)) (u) 28 (0 1 2 3))
(define-uspec Cos.uspec (Cos UnaryOpUGen) ((input 0.0)) (u) 29 (0 1 2 3))
(define-uspec Tan.uspec (Tan UnaryOpUGen) ((input 0.0)) (u) 30 (0 1 2 3))
(define-uspec ArcSin.uspec (ArcSin UnaryOpUGen) ((input 0.0)) (u) 31 (0 1 2 3))
(define-uspec ArcCos.uspec (ArcCos UnaryOpUGen) ((input 0.0)) (u) 32 (0 1 2 3))
(define-uspec ArcTan.uspec (ArcTan UnaryOpUGen) ((input 0.0)) (u) 33 (0 1 2 3))
(define-uspec SinH.uspec (SinH UnaryOpUGen) ((input 0.0)) (u) 34 (0 1 2 3))
(define-uspec CosH.uspec (CosH UnaryOpUGen) ((input 0.0)) (u) 35 (0 1 2 3))
(define-uspec TanH.uspec (TanH UnaryOpUGen) ((input 0.0)) (u) 36 (0 1 2 3))
(define-uspec _Rand.uspec (_Rand UnaryOpUGen) ((input 0.0)) (u) 37 (0 1 2 3))
(define-uspec Rand2.uspec (Rand2 UnaryOpUGen) ((input 0.0)) (u) 38 (0 1 2 3))
(define-uspec _LinRand.uspec (_LinRand UnaryOpUGen) ((input 0.0)) (u) 39 (0 1 2 3))
(define-uspec BiLinRand.uspec (BiLinRand UnaryOpUGen) ((input 0.0)) (u) 40 (0 1 2 3))
(define-uspec Sum3Rand.uspec (Sum3Rand UnaryOpUGen) ((input 0.0)) (u) 41 (0 1 2 3))
(define-uspec Distort.uspec (Distort UnaryOpUGen) ((input 0.0)) (u) 42 (0 1 2 3))
(define-uspec SoftClip.uspec (SoftClip UnaryOpUGen) ((input 0.0)) (u) 43 (0 1 2 3))
(define-uspec Coin.uspec (Coin UnaryOpUGen) ((input 0.0)) (u) 44 (0 1 2 3))
(define-uspec DigitValue.uspec (DigitValue UnaryOpUGen) ((input 0.0)) (u) 45 (0 1 2 3))
(define-uspec Silence.uspec (Silence UnaryOpUGen) ((input 0.0)) (u) 46 (0 1 2 3))
(define-uspec Thru.uspec (Thru UnaryOpUGen) ((input 0.0)) (u) 47 (0 1 2 3))
(define-uspec RectWindow.uspec (RectWindow UnaryOpUGen) ((input 0.0)) (u) 48 (0 1 2 3))
(define-uspec HanWindow.uspec (HanWindow UnaryOpUGen) ((input 0.0)) (u) 49 (0 1 2 3))
(define-uspec WelchWindow.uspec (WelchWindow UnaryOpUGen) ((input 0.0)) (u) 50 (0 1 2 3))
(define-uspec TriWindow.uspec (TriWindow UnaryOpUGen) ((input 0.0)) (u) 51 (0 1 2 3))
(define-uspec _Ramp.uspec (_Ramp UnaryOpUGen) ((input 0.0)) (u) 52 (0 1 2 3))
(define-uspec SCurve.uspec (SCurve UnaryOpUGen) ((input 0.0)) (u) 53 (0 1 2 3))

;; Binary USpecs are generated.
(define gen-binop-uspec
  (lambda ()
    (map
     (lambda (name index)
       `(define-uspec ,(symbol-append-string name ".uspec")
          (,name BinaryOpUGen) ((left 0.0) (right 0.0)) (u) ,index (0 1 2 3)))
     binaryop-names
     (srfi:iota (length binaryop-names)))))

(define-uspec Add.uspec (Add BinaryOpUGen) ((left 0.0) (right 0.0)) (u) 0 (0 1 2 3))
(define-uspec Sub.uspec (Sub BinaryOpUGen) ((left 0.0) (right 0.0)) (u) 1 (0 1 2 3))
(define-uspec Mul.uspec (Mul BinaryOpUGen) ((left 0.0) (right 0.0)) (u) 2 (0 1 2 3))
(define-uspec IDiv.uspec (IDiv BinaryOpUGen) ((left 0.0) (right 0.0)) (u) 3 (0 1 2 3))
(define-uspec FDiv.uspec (FDiv BinaryOpUGen) ((left 0.0) (right 0.0)) (u) 4 (0 1 2 3))
(define-uspec Mod.uspec (Mod BinaryOpUGen) ((left 0.0) (right 0.0)) (u) 5 (0 1 2 3))
(define-uspec EQ.uspec (EQ BinaryOpUGen) ((left 0.0) (right 0.0)) (u) 6 (0 1 2 3))
(define-uspec NE.uspec (NE BinaryOpUGen) ((left 0.0) (right 0.0)) (u) 7 (0 1 2 3))
(define-uspec LT.uspec (LT BinaryOpUGen) ((left 0.0) (right 0.0)) (u) 8 (0 1 2 3))
(define-uspec GT.uspec (GT BinaryOpUGen) ((left 0.0) (right 0.0)) (u) 9 (0 1 2 3))
(define-uspec LE.uspec (LE BinaryOpUGen) ((left 0.0) (right 0.0)) (u) 10 (0 1 2 3))
(define-uspec GE.uspec (GE BinaryOpUGen) ((left 0.0) (right 0.0)) (u) 11 (0 1 2 3))
(define-uspec Min.uspec (Min BinaryOpUGen) ((left 0.0) (right 0.0)) (u) 12 (0 1 2 3))
(define-uspec Max.uspec (Max BinaryOpUGen) ((left 0.0) (right 0.0)) (u) 13 (0 1 2 3))
(define-uspec BitAnd.uspec (BitAnd BinaryOpUGen) ((left 0.0) (right 0.0)) (u) 14 (0 1 2 3))
(define-uspec BitOr.uspec (BitOr BinaryOpUGen) ((left 0.0) (right 0.0)) (u) 15 (0 1 2 3))
(define-uspec BitXor.uspec (BitXor BinaryOpUGen) ((left 0.0) (right 0.0)) (u) 16 (0 1 2 3))
(define-uspec LCM.uspec (LCM BinaryOpUGen) ((left 0.0) (right 0.0)) (u) 17 (0 1 2 3))
(define-uspec GCD.uspec (GCD BinaryOpUGen) ((left 0.0) (right 0.0)) (u) 18 (0 1 2 3))
(define-uspec Round.uspec (Round BinaryOpUGen) ((left 0.0) (right 0.0)) (u) 19 (0 1 2 3))
(define-uspec RoundUp.uspec (RoundUp BinaryOpUGen) ((left 0.0) (right 0.0)) (u) 20 (0 1 2 3))
(define-uspec Trunc.uspec (Trunc BinaryOpUGen) ((left 0.0) (right 0.0)) (u) 21 (0 1 2 3))
(define-uspec Atan2.uspec (Atan2 BinaryOpUGen) ((left 0.0) (right 0.0)) (u) 22 (0 1 2 3))
(define-uspec Hypot.uspec (Hypot BinaryOpUGen) ((left 0.0) (right 0.0)) (u) 23 (0 1 2 3))
(define-uspec Hypotx.uspec (Hypotx BinaryOpUGen) ((left 0.0) (right 0.0)) (u) 24 (0 1 2 3))
(define-uspec Pow.uspec (Pow BinaryOpUGen) ((left 0.0) (right 0.0)) (u) 25 (0 1 2 3))
(define-uspec ShiftLeft.uspec (ShiftLeft BinaryOpUGen) ((left 0.0) (right 0.0)) (u) 26 (0 1 2 3))
(define-uspec ShiftRight.uspec (ShiftRight BinaryOpUGen) ((left 0.0) (right 0.0)) (u) 27 (0 1 2 3))
(define-uspec UnsignedShift.uspec (UnsignedShift BinaryOpUGen) ((left 0.0) (right 0.0)) (u) 28 (0 1 2 3))
(define-uspec Fill.uspec (Fill BinaryOpUGen) ((left 0.0) (right 0.0)) (u) 29 (0 1 2 3))
(define-uspec Ring1.uspec (Ring1 BinaryOpUGen) ((left 0.0) (right 0.0)) (u) 30 (0 1 2 3))
(define-uspec Ring2.uspec (Ring2 BinaryOpUGen) ((left 0.0) (right 0.0)) (u) 31 (0 1 2 3))
(define-uspec Ring3.uspec (Ring3 BinaryOpUGen) ((left 0.0) (right 0.0)) (u) 32 (0 1 2 3))
(define-uspec Ring4.uspec (Ring4 BinaryOpUGen) ((left 0.0) (right 0.0)) (u) 33 (0 1 2 3))
(define-uspec DifSqr.uspec (DifSqr BinaryOpUGen) ((left 0.0) (right 0.0)) (u) 34 (0 1 2 3))
(define-uspec SumSqr.uspec (SumSqr BinaryOpUGen) ((left 0.0) (right 0.0)) (u) 35 (0 1 2 3))
(define-uspec SqrSum.uspec (SqrSum BinaryOpUGen) ((left 0.0) (right 0.0)) (u) 36 (0 1 2 3))
(define-uspec SqrDif.uspec (SqrDif BinaryOpUGen) ((left 0.0) (right 0.0)) (u) 37 (0 1 2 3))
(define-uspec AbsDif.uspec (AbsDif BinaryOpUGen) ((left 0.0) (right 0.0)) (u) 38 (0 1 2 3))
(define-uspec Thresh.uspec (Thresh BinaryOpUGen) ((left 0.0) (right 0.0)) (u) 39 (0 1 2 3))
(define-uspec AMClip.uspec (AMClip BinaryOpUGen) ((left 0.0) (right 0.0)) (u) 40 (0 1 2 3))
(define-uspec ScaleNeg.uspec (ScaleNeg BinaryOpUGen) ((left 0.0) (right 0.0)) (u) 41 (0 1 2 3))
(define-uspec Clip2.uspec (Clip2 BinaryOpUGen) ((left 0.0) (right 0.0)) (u) 42 (0 1 2 3))
(define-uspec Excess.uspec (Excess BinaryOpUGen) ((left 0.0) (right 0.0)) (u) 43 (0 1 2 3))
(define-uspec Fold2.uspec (Fold2 BinaryOpUGen) ((left 0.0) (right 0.0)) (u) 44 (0 1 2 3))
(define-uspec Wrap2.uspec (Wrap2 BinaryOpUGen) ((left 0.0) (right 0.0)) (u) 45 (0 1 2 3))
(define-uspec FirstArg.uspec (FirstArg BinaryOpUGen) ((left 0.0) (right 0.0)) (u) 46 (0 1 2 3))
(define-uspec RandRange.uspec (RandRange BinaryOpUGen) ((left 0.0) (right 0.0)) (u) 47 (0 1 2 3))
(define-uspec ExpRandRange.uspec (ExpRandRange BinaryOpUGen) ((left 0.0) (right 0.0)) (u) 48 (0 1 2 3))

;; The remaining SC3 uspecs are hand written.
(define-uspec APF.uspec APF ((in 0.0) (freq 440.0) (radius 0.8)) (u) 0)
(define-uspec AllpassC.uspec AllpassC ((in 0.0) (maxDelayTime 0.2) (delayTime 0.2) (decayTime 1.0)) (u) 0)
(define-uspec AllpassL.uspec AllpassL ((in 0.0) (maxDelayTime 0.2) (delayTime 0.2) (decayTime 1.0)) (u) 0)
(define-uspec AllpassN.uspec AllpassN ((in 0.0) (maxDelayTime 0.2) (delayTime 0.2) (decayTime 1.0)) (u) 0)
(define-uspec Amplitude.uspec Amplitude ((in 0.0) (attackTime 0.01) (releaseTime 0.01)) (u) 0)
(define-uspec BPF.uspec BPF ((in 0.0) (freq 440.0) (rq 1.0)) (u) 0)
(define-uspec BPZ2.uspec BPZ2 ((in 0.0)) (u) 0)
(define-uspec BRF.uspec BRF ((in 0.0) (freq 440.0) (rq 1.0)) (u) 0)
(define-uspec BRZ2.uspec BRZ2 ((in 0.0)) (u) 0)
(define-uspec Balance2.uspec Balance2 ((left 0.0) (right 0.0) (pos 0.0) (level 0.0)) (u u) 0)
(define-uspec Ball.uspec Ball ((in 0.0) (g 10.0) (damp 0.0) (friction 0.01)) (u) 0)
(define-uspec BiPanB2.uspec BiPanB2 ((inA 0.0) (inB 0.0) (azimuth 0.0) (gain 1.0)) (u u u) 0)
(define-uspec Blip.uspec Blip ((freq 440.0) (numharm 200.0)) (u) 0)
(define-uspec BrownNoise.uspec BrownNoise () (u) 0)
(define-uspec BufAllpassC.uspec BufAllpassC ((buf 0.0) (in 0.0) (delayTime 0.2) (decayTime 1.0)) (u) 0)
(define-uspec BufAllpassL.uspec BufAllpassL ((buf 0.0) (in 0.0) (delayTime 0.2) (decayTime 1.0)) (u) 0)
(define-uspec BufAllpassN.uspec BufAllpassN ((buf 0.0) (in 0.0) (delayTime 0.2) (decayTime 1.0)) (u) 0)
(define-uspec BufChannels.uspec BufChannels ((buf 0.0)) (u) 0)
(define-uspec BufCombC.uspec BufCombC ((buf 0.0) (in 0.0) (delayTime 0.2) (decayTime 1.0)) (u) 0)
(define-uspec BufCombL.uspec BufCombL ((buf 0.0) (in 0.0) (delayTime 0.2) (decayTime 1.0)) (u) 0)
(define-uspec BufCombN.uspec BufCombN ((buf 0.0) (in 0.0) (delayTime 0.2) (decayTime 1.0)) (u) 0)
(define-uspec BufDelayC.uspec BufDelayC ((buf 0.0) (in 0.0) (delayTime 0.2)) (u) 0)
(define-uspec BufDelayL.uspec BufDelayL ((buf 0.0) (in 0.0) (delayTime 0.2)) (u) 0)
(define-uspec BufDelayN.uspec BufDelayN ((buf 0.0) (in 0.0) (delayTime 0.2)) (u) 0)
(define-uspec BufDur.uspec BufDur ((buf 0.0)) (u) 0)
(define-uspec BufFrames.uspec BufFrames ((buf 0.0)) (u) 0)
(define-uspec BufRateScale.uspec BufRateScale ((buf 0.0)) (u) 0)
(define-uspec BufRd.uspec BufRd ((numChannels 1.0) (buf 0.0) (phase 1.0) (loop 1.0) (interpolation 2.0)) 0 0)
(define-uspec BufSampleRate.uspec BufSampleRate ((buf 0.0)) (u) 0)
(define-uspec BufSamples.uspec BufSamples ((buf 0.0)) (u) 0)
(define-uspec BufWr.uspec BufWr ((buf 0.0) (phase 1.0) (loop 1.0) (inputs ())) (u) 0)
(define-uspec COsc.uspec COsc ((buf 0.0) (freq 440.0) (beats 0.0)) (u) 0)
(define-uspec Clip.uspec Clip ((in 0.0) (lo 0.0) (hi 1.0)) (u) 0)
(define-uspec ClipNoise.uspec ClipNoise () (u) 0)
(define-uspec CoinGate.uspec CoinGate ((prob 0.0) (in 0.0)) (u) 0)
(define-uspec CombC.uspec CombC ((in 0.0) (maxDelayTime 0.2) (delayTime 0.2) (decayTime 1.0)) (u) 0)
(define-uspec CombL.uspec CombL ((in 0.0) (maxDelayTime 0.2) (delayTime 0.2) (decayTime 1.0)) (u) 0)
(define-uspec CombN.uspec CombN ((in 0.0) (maxDelayTime 0.2) (delayTime 0.2) (decayTime 1.0)) (u) 0)
(define-uspec Compander.uspec Compander ((in 0.0) (control 0.0) (thresh 0.5) (slopeBelow 1.0) (slopeAbove 1.0) (clampTime 0.01) (relaxtime 0.01)) (u) 0)
(define-uspec CompanderD.uspec CompanderD ((in 0.0) (hresh 0.5) (slopeBelow 1.0) (slopeAbove 1.0) (clampTime 0.01) (relaxtime 0.01)) (u) 0)
(define-uspec Control.uspec Control ((numChannels 1.0)) 0 0)
(define-uspec ControlRate.uspec ControlRate () (u) 0)
(define-uspec Convolution.uspec Convolution ((in 0.0) (kernel 0.0) (framesize 512.0)) (u) 0)
(define-uspec Convolution2.uspec Convolution2 ((in 0.0) (kernel 0.0) (framesize 512.0)) (u) 0)
(define-uspec Crackle.uspec Crackle ((chaosParam 1.5)) (u) 0)
(define-uspec CuspC.uspec CuspC ((freq 22050.0) (a 1.0) (b 1.9) (xi 0.0)) (u) 0)
(define-uspec CuspL.uspec CuspL ((freq 22050.0) (a 1.0) (b 1.9) (xi 0.0)) (u) 0)
(define-uspec CuspN.uspec CuspN ((freq 22050.0) (a 1.0) (b 1.9) (xi 0.0)) (u) 0)
(define-uspec Dbrown.uspec Dbrown ((lo 0.0) (hi 1.0) (step 1.0) (length -1.0)) (u) 0 (3))
(define-uspec Decay.uspec Decay ((in 0.0) (decayTime 1.0)) (u) 0)
(define-uspec Decay2.uspec Decay2 ((in 0.0) (attackTime 1.0) (decayTime 1.0)) (u) 0)
(define-uspec DecodeB2.uspec DecodeB2 ((numChannels 0.0) (w 0.0) (x 0.0) (y 0.0) (orientation 0.5)) 0 0)
(define-uspec DegreeToKey.uspec DegreeToKey ((buf 0.0) (in 0.0) (octave 12.0)) (u) 0)
(define-uspec Delay1.uspec Delay1 ((in 0.0)) (u) 0)
(define-uspec Delay2.uspec Delay2 ((in 0.0)) (u) 0)
(define-uspec DelayC.uspec DelayC ((in 0.0) (maxDelayTime 0.2) (delayTime 0.2)) (u) 0)
(define-uspec DelayL.uspec DelayL ((in 0.0) (maxDelayTime 0.2) (delayTime 0.2)) (u) 0)
(define-uspec DelayN.uspec DelayN ((in 0.0) (maxDelayTime 0.2) (delayTime 0.2)) (u) 0)
(define-uspec Demand.uspec Demand ((trig 0.0) (reset 0.0) (ugens ())) (u) 0)
(define-uspec DemandEnvGen.uspec DemandEnvGen ((level 0.0) (dur 0.0) (shape 1.0) (curve 0.0) (gate 1.0) (reset 1.0) (levelScale 1.0) (levelBias 0.0) (timeScale 1.0) (doneAction 0.0)) (u) 0)
(define-uspec DetectSilence.uspec DetectSilence ((in 0.0) (amp 1e-05) (time 0.2) (doneAction 0.0)) (u) 0)
(define-uspec Dgeom.uspec Dgeom ((start 0.0) (grow 1.0) (length 100)) (u) 0 (3))
(define-uspec Dibrown.uspec Dibrown ((lo 0.0) (hi 1.0) (step 1.0) (length -1.0)) (u) 0 (3))
(define-uspec DiskIn.uspec DiskIn ((numChannels 0.0) (buf 0.0)) 0 0)
(define-uspec DiskOut.uspec DiskOut ((buf 0.0) (channels ())) () 0)
(define-uspec Diwhite.uspec Diwhite ((lo 0.0) (hi 1.0) (length -1.0)) (u) 0 (3))
(define-uspec Done.uspec Done ((src 0.0)) (u) 0)
(define-uspec Drand.uspec Drand ((repeats 1.0) (spec ())) (u) 0 (3))
(define-uspec Dseq.uspec Dseq ((repeats 1.0) (spec ())) (u) 0 (3))
(define-uspec Dser.uspec Dser ((repeats 1.0) (spec ())) (u) 0 (3))
(define-uspec Dseries.uspec Dseries ((start 0.0) (step 1.0) (length 100)) (u) 0 (3))
(define-uspec Dswitch1.uspec Dswitch1 ((index 1.0) (spec ())) (u) 0 (3))
(define-uspec Dust.uspec Dust ((density 0.0)) (u) 0)
(define-uspec Dust2.uspec Dust2 ((density 0.0)) (u) 0)
(define-uspec Duty.uspec Duty ((dur 1.0) (reset 0.0) (level 1.0) (doneAction 0.0)) (u) 0)
(define-uspec Dwhite.uspec Dwhite ((lo 0.0) (hi 1.0) (length -1.0)) (u) 0 (3))
(define-uspec Dxrand.uspec Dxrand ((repeats 1.0) (spec ())) (u) 0 (3))
(define-uspec EnvGen.uspec EnvGen ((gate 1.0) (levelScale 1.0) (levelBias 0.0) (timeScale 1.0) (doneAction 0.0) (envelope ())) (u) 0)
(define-uspec ExpRand.uspec ExpRand ((lo 0.1) (hi 1.0)) (u) 0)
(define-uspec FBSineC.uspec FBSineC ((freq 22050.0) (im 1.0) (fb 0.1) (a 1.1) (c 0.5) (xi 0.1) (yi 0.1)) (u) 0)
(define-uspec FBSineL.uspec FBSineL ((freq 22050.0) (im 1.0) (fb 0.1) (a 1.1) (c 0.5) (xi 0.1) (yi 0.1)) (u) 0)
(define-uspec FBSineN.uspec FBSineN ((freq 22050.0) (im 1.0) (fb 0.1) (a 1.1) (c 0.5) (xi 0.1) (yi 0.1)) (u) 0)
(define-uspec FFT.uspec FFT ((buffer 0.0) (in 0.0)) (u) 0)
(define-uspec FOS.uspec FOS ((in 0.0) (a0 0.0) (a1 0.0) (b1 0.0)) (u) 0)
(define-uspec FSinOsc.uspec FSinOsc ((freq 440.0) (iphase 0.0)) (u) 0)
(define-uspec Fold.uspec Fold ((in 0.0) (lo 0.0) (hi 1.0)) (u) 0)
(define-uspec Formant.uspec Formant ((fundfreq 440.0) (formfreq 1760.0) (bwfreq 880.0)) (u) 0)
(define-uspec Formlet.uspec Formlet ((in 0.0) (freq 440.0) (attackTime 1.0) (decayTime 1.0)) (u) 0)
(define-uspec Free.uspec Free ((trig 0.0) (id 0.0)) (u) 0)
(define-uspec FreeSelf.uspec FreeSelf ((src 0.0)) (u) 0)
(define-uspec FreeSelfWhenDone.uspec FreeSelfWhenDone ((src 0.0)) (u) 0)
(define-uspec Gate.uspec Gate ((in 0.0) (trig 0.0)) (u) 0)
(define-uspec GbmanC.uspec GbmanC ((freq 22050.0) (xi 1.2) (yi 2.1)) (u) 0)
(define-uspec GbmanL.uspec GbmanL ((freq 22050.0) (xi 1.2) (yi 2.1)) (u) 0)
(define-uspec GbmanN.uspec GbmanN ((freq 22050.0) (xi 1.2) (yi 2.1)) (u) 0)
(define-uspec Gendy1.uspec Gendy1 ((ampdist 1) (durdist 1) (adparam 1.0) (ddparam 1.0) (minfreq 440) (maxfreq 660) (ampscale 0.5) (durscale 0.5) (initCPs 12) (knum 12)) (u) 0)
(define-uspec Gendy2.uspec Gendy2 ((ampdist 1) (durdist 1) (adparam 1.0) (ddparam 1.0) (minfreq 440) (maxfreq 660) (ampscale 0.5) (durscale 0.5) (initCPs 12) (knum 12)) (u) 0)
(define-uspec Gendy3.uspec Gendy3 ((ampdist 1) (durdist 1) (adparam 1.0) (ddparam 1.0) (minfreq 440) (maxfreq 660) (ampscale 0.5) (durscale 0.5) (initCPs 12) (knum 12)) (u) 0)
(define-uspec GrayNoise.uspec GrayNoise () (u) 0)
(define-uspec HPF.uspec HPF ((in 0.0) (freq 440.0)) (u) 0)
(define-uspec HPZ1.uspec HPZ1 ((in 0.0)) (u) 0)
(define-uspec HPZ2.uspec HPZ2 ((in 0.0)) (u) 0)
(define-uspec Hasher.uspec Hasher ((in 0.0)) (u) 0)
(define-uspec HenonC.uspec HenonC ((freq 22050.0) (a 1.4) (b 0.3) (x0 0.0) (x1 0.0)) (u) 0)
(define-uspec HenonL.uspec HenonL ((freq 22050.0) (a 1.4) (b 0.3) (x0 0.0) (x1 0.0)) (u) 0)
(define-uspec HenonN.uspec HenonN ((freq 22050.0) (a 1.4) (b 0.3) (x0 0.0) (x1 0.0)) (u) 0)
(define-uspec IFFT.uspec IFFT ((buffer 0.0)) (u) 0)
(define-uspec IRand.uspec IRand ((lo 0.0) (hi 127)) (u) 0)
(define-uspec ImageWarp.uspec ImageWarp ((pic 0.0) (x 0.0) (y 0.0) (interpolationType 1.0)) (u) 0)
(define-uspec Impulse.uspec Impulse ((freq 440.0) (iphase 0.0)) (u) 0)
(define-uspec In.uspec In ((bus 0.0) (numChannels 1.0)) 1 0)
(define-uspec InFeedback.uspec InFeedback ((bus 0.0) (numChannels 1.0)) 1 0)
(define-uspec InRange.uspec InRange ((in 0.0) (lo 0.0) (hi 1.0)) (u) 0)
(define-uspec InRect.uspec InRect ((x 0.0) (y 0.0) (left 0.0) (top 0.0) (right 0.0) (bottom 0.0)) (u) 0)
(define-uspec InTrig.uspec InTrig ((bus 0.0) (numChannels 1.0)) 1 0)
(define-uspec Index.uspec Index ((buf 0.0) (in 0.0)) (u) 0)
(define-uspec Integrator.uspec Integrator ((in 0.0) (coef 1.0)) (u) 0)
(define-uspec K2A.uspec K2A ((input 0.0)) (u) 0)
(define-uspec KeyState.uspec KeyState ((keynum 0.0) (minval 0.0) (maxval 1.0) (lag 0.0)) (u) 0)
(define-uspec Klang.uspec Klang ((freqScale 1.0) (freqOffset 0.0) (spec ())) (u) 0)
(define-uspec Klank.uspec Klank ((in 0.0) (freqScale 1.0) (freqOffset 0.0) (decayScale 1.0) (spec ())) (u) 0)
(define-uspec LFClipNoise.uspec LFClipNoise ((freq 500.0)) (u) 0)
(define-uspec LFCub.uspec LFCub ((freq 440.0) (iphase 0.0)) (u) 0)
(define-uspec LFDClipNoise.uspec LFDClipNoise ((freq 500.0)) (u) 0)
(define-uspec LFDNoise0.uspec LFDNoise0 ((freq 500.0)) (u) 0)
(define-uspec LFDNoise1.uspec LFDNoise1 ((freq 500.0)) (u) 0)
(define-uspec LFDNoise3.uspec LFDNoise3 ((freq 500.0)) (u) 0)
(define-uspec LFNoise0.uspec LFNoise0 ((freq 500.0)) (u) 0)
(define-uspec LFNoise1.uspec LFNoise1 ((freq 500.0)) (u) 0)
(define-uspec LFNoise2.uspec LFNoise2 ((freq 500.0)) (u) 0)
(define-uspec LFPar.uspec LFPar ((freq 440.0) (iphase 0.0)) (u) 0)
(define-uspec LFPulse.uspec LFPulse ((freq 440.0) (iphase 0.0) (width 0.5)) (u) 0)
(define-uspec LFSaw.uspec LFSaw ((freq 440.0) (iphase 0.0)) (u) 0)
(define-uspec LFTri.uspec LFTri ((freq 440.0) (iphase 0.0)) (u) 0)
(define-uspec LPF.uspec LPF ((in 0.0) (freq 440.0)) (u) 0)
(define-uspec LPZ1.uspec LPZ1 ((in 0.0)) (u) 0)
(define-uspec LPZ2.uspec LPZ2 ((in 0.0)) (u) 0)
(define-uspec Lag.uspec Lag ((in 0.0) (lagTime 0.1)) (u) 0)
(define-uspec Lag2.uspec Lag2 ((in 0.0) (lagTime 0.1)) (u) 0)
(define-uspec Lag3.uspec Lag3 ((in 0.0) (lagTime 0.1)) (u) 0)
(define-uspec LagControl.uspec LagControl ((numChannels 1.0) (values ())) 0 0)
(define-uspec LagIn.uspec LagIn ((bus 0.0) (numChannels 1.0) (lag 0.1)) 1 0)
(define-uspec LastValue.uspec LastValue ((in 0.0) (diff 0.01)) (u) 0)
(define-uspec Latch.uspec Latch ((in 0.0) (trig 0.0)) (u) 0)
(define-uspec Latoocarfian.uspec Latoocarfian ((a 0.0) (b 0.0) (c 0.0) (d 0.0)) (u) 0)
(define-uspec LatoocarfianC.uspec LatoocarfianC ((freq 22050.0) (a 1.0) (b 3.0) (c 0.5) (d 0.5) (xi 0.5) (yi 0.5)) (u) 0)
(define-uspec LatoocarfianL.uspec LatoocarfianL ((freq 22050.0) (a 1.0) (b 3.0) (c 0.5) (d 0.5) (xi 0.5) (yi 0.5)) (u) 0)
(define-uspec LatoocarfianN.uspec LatoocarfianN ((freq 22050.0) (a 1.0) (b 3.0) (c 0.5) (d 0.5) (xi 0.5) (yi 0.5)) (u) 0)
(define-uspec LeakDC.uspec LeakDC ((in 0.0) (coef 0.995)) (u) 0)
(define-uspec LeastChange.uspec LeastChange ((a 0.0) (b 0.0)) (u) 0)
(define-uspec Limiter.uspec Limiter ((in 0.0) (level 1.0) (dur 0.01)) (u) 0)
(define-uspec LinCongC.uspec LinCongC ((freq 22050.0) (a 1.1) (c 0.13) (m 1.0) (xi 0.0)) (u) 0)
(define-uspec LinCongL.uspec LinCongL ((freq 22050.0) (a 1.1) (c 0.13) (m 1.0) (xi 0.0)) (u) 0)
(define-uspec LinCongN.uspec LinCongN ((freq 22050.0) (a 1.1) (c 0.13) (m 1.0) (xi 0.0)) (u) 0)
(define-uspec LinExp.uspec LinExp ((in 0.0) (srclo 0.0) (srchi 1.0) (dstlo 1.0) (dsthi 2.0)) (u) 0)
(define-uspec LinLin.uspec LinLin ((in 0.0) (srclo 0.0) (srchi 1.0) (dstlo 1.0) (dsthi 2.0)) (u) 0)
(define-uspec LinPan2.uspec LinPan2 ((in 0.0) (pos 0.0) (level 1.0)) (u u) 0)
(define-uspec LinRand.uspec LinRand ((lo 0.0) (hi 1.0) (minmax 0.0)) (u) 0)
(define-uspec LinXFade2.uspec LinXFade2 ((ina 0.0) (inb 0.0) (pan 0.0) (level 0.0)) (u) 0)
(define-uspec Line.uspec Line ((start 0.0) (end 1.0) (dur 1.0) (doneAction 0.0)) (u) 0)
(define-uspec Linen.uspec Linen ((gate 1.0) (attackTime 0.01) (susLevel 1.0) (releaseTime 1.0) (doneAction 0.0)) (u) 0)
(define-uspec LocalIn.uspec LocalIn ( (numChannels 1.0)) 0 0)
(define-uspec LocalOut.uspec LocalOut ( (inputs ())) () 0)
(define-uspec Logistic.uspec Logistic ((chaosParam 3.0) (freq 1000.0)) (u) 0)
(define-uspec LorenzL.uspec LorenzL ((freq 22050.0) (s 10.0) (r 28.0) (b 2.667) (h 0.05) (xi 0.1) (yi 0.0) (zi 0.0)) (u) 0)
(define-uspec MantissaMask.uspec MantissaMask ((in 0.0) (bits 3.0)) (u) 0)
(define-uspec Median.uspec Median ((length 3.0) (in 0.0)) (u) 0)
(define-uspec MidEQ.uspec MidEQ ((in 0.0) (freq 440.0) (rq 1.0) (db 0.0)) (u) 0)
(define-uspec MostChange.uspec MostChange ((a 0.0) (b 0.0)) (u) 0)
(define-uspec MouseButton.uspec MouseButton ((minval 0.0) (maxval 1.0) (lag 0.0)) (u) 0)
(define-uspec MouseX.uspec MouseX ((minval 0.0) (maxval 1.0) (warp 0.0) (lag 0.2)) (u) 0)
(define-uspec MouseY.uspec MouseY ((minval 0.0) (maxval 1.0) (warp 0.0) (lag 0.2)) (u) 0)
(define-uspec MulAdd.uspec MulAdd ((input 0.0) (mul 1.0) (add 0.0)) (u) 0)
(define-uspec NRand.uspec NRand ((lo 0.0) (hi 1.0) (n 0.0)) (u) 0)
(define-uspec NoahNoise.uspec NoahNoise () (u) 0)
(define-uspec Normalizer.uspec Normalizer ((in 0.0) (level 1.0) (dur 0.01)) (u) 0)
(define-uspec NumAudioBuses.uspec NumAudioBuses () (u) 0)
(define-uspec NumBuffers.uspec NumBuffers () (u) 0)
(define-uspec NumControlBuses.uspec NumControlBuses () (u) 0)
(define-uspec NumInputBuses.uspec NumInputBuses () (u) 0)
(define-uspec NumOutputBuses.uspec NumOutputBuses () (u) 0)
(define-uspec NumRunningSynths.uspec NumRunningSynths () (u) 0)
(define-uspec OffsetOut.uspec OffsetOut ((bus 0.0) (inputs ())) () 0)
(define-uspec OnePole.uspec OnePole ((in 0.0) (coef 0.5)) (u) 0)
(define-uspec OneZero.uspec OneZero ((in 0.0) (coef 0.5)) (u) 0)
(define-uspec Osc.uspec Osc ((buf 0.0) (freq 440.0) (phase 0.0)) (u) 0)
(define-uspec OscN.uspec OscN ((buf 0.0) (freq 440.0) (phase 0.0)) (u) 0)
(define-uspec Out.uspec Out ((bus 0.0) (inputs ())) () 0)
(define-uspec PSinGrain.uspec PSinGrain ((freq 440.0) (dur 0.2) (amp 1.0)) (u) 0)
(define-uspec PV_Add.uspec PV_Add ((bufferA 0.0) (bufferB 0.0)) (u) 0)
(define-uspec PV_BinScramble.uspec PV_BinScramble ((buffer 0.0) (wipe 0.0) (width 0.2) (trig 0.0)) (u) 0)
(define-uspec PV_BinShift.uspec PV_BinShift ((buffer 0.0) (stretch 0.0) (shift 0.0)) (u) 0)
(define-uspec PV_BinWipe.uspec PV_BinWipe ((bufferA 0.0) (bufferB 0.0) (wipe 0.0)) (u) 0)
(define-uspec PV_BrickWall.uspec PV_BrickWall ((buffer 0.0) (wipe 0.0)) (u) 0)
(define-uspec PV_ConformalMap.uspec PV_ConformalMap ((buffer 0.0) (real 0.0) (imag 0.0)) (u) 0)
(define-uspec PV_CopyPhase.uspec PV_CopyPhase ((bufferA 0.0) (bufferB 0.0)) (u) 0)
(define-uspec PV_Diffuser.uspec PV_Diffuser ((buffer 0.0) (trig 0.0)) (u) 0)
(define-uspec PV_HainsworthFoote.uspec PV_HainsworthFoote ((buffer 0.0) (proph 0.0) (propf 0.0) (threshold 1.05) (waittime 0.04)) (u) 0)
(define-uspec PV_JensenAndersen.uspec PV_JensenAndersen ((buffer 0.0) (propsc 0.25) (prophfe 0.25) (prophfc 0.25) (propsf 0.25) (threshold 1.05) (waittime 0.04)) (u) 0)
(define-uspec PV_LocalMax.uspec PV_LocalMax ((buffer 0.0) (threshold 0.0)) (u) 0)
(define-uspec PV_MagAbove.uspec PV_MagAbove ((buffer 0.0) (threshold 0.0)) (u) 0)
(define-uspec PV_MagBelow.uspec PV_MagBelow ((buffer 0.0) (threshold 0.0)) (u) 0)
(define-uspec PV_MagClip.uspec PV_MagClip ((buffer 0.0) (threshold 0.0)) (u) 0)
(define-uspec PV_MagFreeze.uspec PV_MagFreeze ((buffer 0.0) (freeze 0.0)) (u) 0)
(define-uspec PV_MagMul.uspec PV_MagMul ((bufferA 0.0) (bufferB 0.0)) (u) 0)
(define-uspec PV_MagNoise.uspec PV_MagNoise ((buffer 0.0)) (u) 0)
(define-uspec PV_MagShift.uspec PV_MagShift ((buffer 0.0) (stretch 0.0) (shift 0.0)) (u) 0)
(define-uspec PV_MagSmear.uspec PV_MagSmear ((buffer 0.0) (bins 0.0)) (u) 0)
(define-uspec PV_MagSquared.uspec PV_MagSquared ((buffer 0.0)) (u) 0)
(define-uspec PV_Max.uspec PV_Max ((bufferA 0.0) (bufferB 0.0)) (u) 0)
(define-uspec PV_Min.uspec PV_Min ((bufferA 0.0) (bufferB 0.0)) (u) 0)
(define-uspec PV_Mul.uspec PV_Mul ((bufferA 0.0) (bufferB 0.0)) (u) 0)
(define-uspec PV_PhaseShift.uspec PV_PhaseShift ((buffer 0.0) (shift 0.0)) (u) 0)
(define-uspec PV_PhaseShift270.uspec PV_PhaseShift270 ((buffer 0.0)) (u) 0)
(define-uspec PV_PhaseShift90.uspec PV_PhaseShift90 ((buffer 0.0)) (u) 0)
(define-uspec PV_RandComb.uspec PV_RandComb ((buffer 0.0) (wipe 0.0) (trig 0.0)) (u) 0)
(define-uspec PV_RandWipe.uspec PV_RandWipe ((bufferA 0.0) (bufferB 0.0) (wipe 0.0) (trig 0.0)) (u) 0)
(define-uspec PV_RectComb.uspec PV_RectComb ((buffer 0.0) (numTeeth 0.0) (phase 0.0) (width 0.5)) (u) 0)
(define-uspec PV_RectComb2.uspec PV_RectComb2 ((bufferA 0.0) (bufferB 0.0) (numTeeth 0.0) (phase 0.0) (width 0.5)) (u) 0)
(define-uspec Pan2.uspec Pan2 ((in 0.0) (pos 0.0) (level 1.0)) (u u) 0)
(define-uspec Pan4.uspec Pan4 ((in 0.0) (xpos 0.0) (ypos 0.0) (level 1.0)) (u u u u) 0)
(define-uspec PanAz.uspec PanAz ((numChannels 2.0) (in 0.0) (pos 0.0) (level 0.0) (width 2.0)) 0 0)
(define-uspec PanB.uspec PanB ((in 0.0) (azimuth 0.0) (elevation 0.0) (gain 1.0)) (u u u u) 0)
(define-uspec PanB2.uspec PanB2 ((in 0.0) (azimuth 0.0) (gain 1.0)) (u u u) 0)
(define-uspec Pause.uspec Pause ((gate 0.0) (id 0.0)) (u) 0)
(define-uspec PauseSelf.uspec PauseSelf ((src 0.0)) (u) 0)
(define-uspec PauseSelfWhenDone.uspec PauseSelfWhenDone ((src 0.0)) (u) 0)
(define-uspec Peak.uspec Peak ((trig 0.0) (reset 0.0)) (u) 0)
(define-uspec PeakFollower.uspec PeakFollower ((in 0.0) (decay 0.999)) (u) 0)
(define-uspec Phasor.uspec Phasor ((trig 0.0) (rate 1.0) (start 0.0) (end 1.0) (resetpos 0.0)) (u) 0)
(define-uspec PinkNoise.uspec PinkNoise () (u) 0)
(define-uspec Pitch.uspec Pitch ((in 0.0) (initFreq 440.0) (minFreq 60.0) (maxFreq 4000.0) (execFreq 100.0) (maxBinsPerOctave 16) (median 1) (ampThreshold 0.01) (peakThreshold 0.5) (downSample 1)) (u u) 0)
(define-uspec PitchShift.uspec PitchShift ((in 0.0) (winSize 0.2) (pchRatio 1.0) (pchDispersion 0.0) (timeDispersion 0.0)) (u) 0)
(define-uspec PlayBuf.uspec PlayBuf ((numChannels 1.0) (buf 0.0) (rate 1.0) (trigger 1.0) (startPos 0.0) (loop 0.0)) 0 0)
(define-uspec Pulse.uspec Pulse ((freq 440.0) (width 0.5)) (u) 0)
(define-uspec PulseCount.uspec PulseCount ((trig 0.0) (reset 0.0)) (u) 0)
(define-uspec PulseDivider.uspec PulseDivider ((trig 0.0) (div 2.0) (start 0.0)) (u) 0)
(define-uspec QuadC.uspec QuadC ((freq 22050.0) (a 1.0) (b -1.0) (c -0.75) (xi 0.0)) (u) 0)
(define-uspec QuadL.uspec QuadL ((freq 22050.0) (a 1.0) (b -1.0) (c -0.75) (xi 0.0)) (u) 0)
(define-uspec QuadN.uspec QuadN ((freq 22050.0) (a 1.0) (b -1.0) (c -0.75) (xi 0.0)) (u) 0)
(define-uspec RHPF.uspec RHPF ((in 0.0) (freq 440.0) (rq 1.0)) (u) 0)
(define-uspec RLPF.uspec RLPF ((in 0.0) (freq 440.0) (rq 1.0)) (u) 0)
(define-uspec RadiansPerSample.uspec RadiansPerSample () (u) 0)
(define-uspec Ramp.uspec Ramp ((in 0.0) (lagTime 0.1)) (u) 0)
(define-uspec Rand.uspec Rand ((lo 0.0) (hi 1.0)) (u) 0)
(define-uspec RandID.uspec RandID ((id 0.0)) () 0)
(define-uspec RandSeed.uspec RandSeed ((trig 0.0) (seed 56789)) () 0)
(define-uspec RecordBuf.uspec RecordBuf ((buf 0.0) (offset 0.0) (reclevel 1.0) (prelevel 0.0) (run 1.0) (loop 1.0) (trigger 1.0) (inputs ())) () 0)
(define-uspec ReplaceOut.uspec ReplaceOut ((bus 0.0) (inputs ())) () 0)
(define-uspec Resonz.uspec Resonz ((in 0.0) (freq 440.0) (bwr 1.0)) (u) 0)
(define-uspec Ringz.uspec Ringz ((in 0.0) (freq 440.0) (decayTime 1.0)) (u) 0)
(define-uspec Rotate2.uspec Rotate2 ((x 0.0) (y 0.0) (pos 0.0)) (u u) 0)
(define-uspec RunningSum.uspec RunningSum ((in 0.0) (numsamp 40.0)) (u) 0)
(define-uspec SOS.uspec SOS ((in 0.0) (a0 0.0) (a1 0.0) (a2 0.0) (b1 0.0) (b2 0.0)) (u) 0)
(define-uspec SampleDur.uspec SampleDur () (u) 0)
(define-uspec SampleRate.uspec SampleRate () (u) 0)
(define-uspec Saw.uspec Saw ((freq 440.0)) (u) 0)
(define-uspec Schmidt.uspec Schmidt ((in 0.0) (lo 0.0) (hi 1.0)) (u) 0)
(define-uspec ScopeOut.uspec ScopeOut ((buf 0.0) (inputs ())) (u) 0)
(define-uspec Select.uspec Select ((which 0.0) (array ())) (u) 0)
(define-uspec SendTrig.uspec SendTrig ((in 0.0) (id 0.0) (value 0.0)) () 0)
(define-uspec SetResetFF.uspec SetResetFF ((trig 0.0) (reset 0.0)) (u) 0)
(define-uspec Shaper.uspec Shaper ((buf 0.0) (in 0.0)) (u) 0)
(define-uspec SharedIn.uspec SharedIn ((bus 0.0) (numChannels 1.0)) 0 0)
(define-uspec SharedOut.uspec SharedOut ((bus 0.0) (inputs ())) () 0)
(define-uspec Silent.uspec Silent ((numChannels 0.0)) 0 0)
(define-uspec SinOsc.uspec SinOsc ((freq 440.0) (phase 0.0)) (u) 0)
(define-uspec SinOscFB.uspec SinOscFB ((freq 440.0) (feedback 0.0)) (u) 0)
(define-uspec Slew.uspec Slew ((in 0.0) (up 1.0) (dn 1.0)) (u) 0)
(define-uspec Slope.uspec Slope ((in 0.0)) (u) 0)
(define-uspec Spring.uspec Spring ((in 0.0) (spring 1.0) (damp 0.0)) (u) 0)
(define-uspec StandardC.uspec StandardC ((freq 22050.0) (k 1.0) (xi 0.5) (yi 0.0)) (u) 0)
(define-uspec StandardL.uspec StandardL ((freq 22050.0) (k 1.0) (xi 0.5) (yi 0.0)) (u) 0)
(define-uspec StandardN.uspec StandardN ((freq 22050.0) (k 1.0) (xi 0.5) (yi 0.0)) (u) 0)
(define-uspec Stepper.uspec Stepper ((trig 0.0) (reset 0.0) (min 0.0) (max 7.0) (step 1.0) (resetval 0.0)) (u) 0)
(define-uspec Sweep.uspec Sweep ((in 0.0) (rate 1.0)) (u) 0)
(define-uspec SyncSaw.uspec SyncSaw ((syncFreq 440.0) (sawFreq 440.0)) (u) 0)
(define-uspec TBall.uspec TBall ((in 0.0) (g 10.0) (damp 0.0) (friction 0.01)) (u) 0)
(define-uspec TDelay.uspec TDelay ((in 0.0) (dur 0.1)) (u) 0)
(define-uspec TDuty.uspec TDuty ((dur 1.0) (reset 0.0) (level 1.0) (doneAction 0.0)) (u) 0)
(define-uspec TExpRand.uspec TExpRand ((lo 0.1) (hi 1.0) (trig 0.0)) (u) 0)
(define-uspec TGrains.uspec TGrains ((numChannels 2.0) (trigger 0.0) (buf 0.0) (rate 1.0) (centerPos 0.0) (dur 0.1) (pan 0.0) (amp 0.1) (interp 4.0)) 0 0)
(define-uspec TIRand.uspec TIRand ((lo 0.0) (hi 127) (trig 0.0)) (u) 0)
(define-uspec TPulse.uspec TPulse ((trig 0.0) (freq 440.0) (width 0.5)) (u) 0)
(define-uspec TRand.uspec TRand ((lo 0.0) (hi 1.0) (trig 0.0)) (u) 0)
(define-uspec TWindex.uspec TWindex ((in 0.0) (normalize 0.0) (array ())) (u) 0)
(define-uspec Timer.uspec Timer ((in 0.0)) (u) 0)
(define-uspec ToggleFF.uspec ToggleFF ((trig 0.0)) (u) 0)
(define-uspec Trapezoid.uspec Trapezoid ((in 0.0) (a 0.2) (b 0.4) (c 0.6) (d 0.8)) (u) 0)
(define-uspec Trig.uspec Trig ((in 0.0) (dur 0.1)) (u) 0)
(define-uspec Trig1.uspec Trig1 ((in 0.0) (dur 0.1)) (u) 0)
(define-uspec TrigControl.uspec TrigControl ((numChannels 1.0)) 0 0)
(define-uspec TwoPole.uspec TwoPole ((in 0.0) (freq 440.0) (radius 0.8)) (u) 0)
(define-uspec TwoZero.uspec TwoZero ((in 0.0) (freq 440.0) (radius 0.8)) (u) 0)
(define-uspec VOsc.uspec VOsc ((bufpos 0.0) (freq 440.0) (phase 0.0)) (u) 0)
(define-uspec VOsc3.uspec VOsc3 ((bufpos 0.0) (freq1 110.0) (freq2 220.0) (freq3 440.0)) (u) 0)
(define-uspec VarSaw.uspec VarSaw ((freq 440.0) (iphase 0.0) (width 0.5)) (u) 0)
(define-uspec Vibrato.uspec Vibrato ((freq 440.0) (rate 6) (depth 0.02) (delay 0.0) (onset 0.0) (rateVariation 0.04) (depthVariation 0.1) (iphase 0.0)) (u) 0)
(define-uspec WhiteNoise.uspec WhiteNoise () (u) 0)
(define-uspec Wrap.uspec Wrap ((in 0.0) (lo 0.0) (hi 1.0)) (u) 0)
(define-uspec WrapIndex.uspec WrapIndex ((buf 0.0) (in 0.0)) (u) 0)
(define-uspec XFade2.uspec XFade2 ((ina 0.0) (inb 0.0) (pan 0.0) (level 0.0)) (u) 0)
(define-uspec XLine.uspec XLine ((start 1.0) (end 2.0) (dur 1.0) (doneAction 0.0)) (u) 0)
(define-uspec XOut.uspec XOut ((bus 0.0) (xfade 0.0) (inputs ())) () 0)
(define-uspec XY.uspec XY ((xscale 1.0) (yscale 1.0) (xoff 0.0) (yoff 0.0) (rot 0.0) (rate 1.0)) (u) 0)
(define-uspec ZeroCrossing.uspec ZeroCrossing ((in 0.0)) (u) 0)

;; Local Variables:
;; truncate-lines:t
;; End:
