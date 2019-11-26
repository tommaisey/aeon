(define a2k
  (lambda (input)
    (mk-ugen (list "A2K" kr (list input) nil 1 nil nil))))

(define apf
  (lambda (input freq radius)
    (mk-ugen (list "APF" (list 0) (list input freq radius) nil 1 nil nil))))

(define allpass-c
  (lambda (input maxdelaytime delaytime decaytime)
    (mk-ugen (list "AllpassC" (list 0) (list input maxdelaytime delaytime decaytime) nil 1 nil nil))))

(define allpass-l
  (lambda (input maxdelaytime delaytime decaytime)
    (mk-ugen (list "AllpassL" (list 0) (list input maxdelaytime delaytime decaytime) nil 1 nil nil))))

(define allpass-n
  (lambda (input maxdelaytime delaytime decaytime)
    (mk-ugen (list "AllpassN" (list 0) (list input maxdelaytime delaytime decaytime) nil 1 nil nil))))

(define amp-comp
  (lambda (rt freq root exp_)
    (mk-ugen (list "AmpComp" rt (list freq root exp_) nil 1 nil nil))))

(define amp-comp-a
  (lambda (rt freq root minAmp rootAmp)
    (mk-ugen (list "AmpCompA" rt (list freq root minAmp rootAmp) nil 1 nil nil))))

(define amplitude
  (lambda (rt input attackTime releaseTime)
    (mk-ugen (list "Amplitude" rt (list input attackTime releaseTime) nil 1 nil nil))))

(define audio-control
  (lambda (rt values)
    (mk-ugen (list "AudioControl" rt (list values) nil 1 nil nil))))

(define b-all-pass
  (lambda (input freq rq)
    (mk-ugen (list "BAllPass" (list 0) (list input freq rq) nil 1 nil nil))))

(define b-band-pass
  (lambda (input freq bw)
    (mk-ugen (list "BBandPass" (list 0) (list input freq bw) nil 1 nil nil))))

(define b-band-stop
  (lambda (input freq bw)
    (mk-ugen (list "BBandStop" (list 0) (list input freq bw) nil 1 nil nil))))

(define b-hi-pass
  (lambda (input freq rq)
    (mk-ugen (list "BHiPass" (list 0) (list input freq rq) nil 1 nil nil))))

(define b-hi-shelf
  (lambda (input freq rs db)
    (mk-ugen (list "BHiShelf" (list 0) (list input freq rs db) nil 1 nil nil))))

(define b-low-pass
  (lambda (input freq rq)
    (mk-ugen (list "BLowPass" (list 0) (list input freq rq) nil 1 nil nil))))

(define b-low-shelf
  (lambda (input freq rs db)
    (mk-ugen (list "BLowShelf" (list 0) (list input freq rs db) nil 1 nil nil))))

(define bpf
  (lambda (input freq rq)
    (mk-ugen (list "BPF" (list 0) (list input freq rq) nil 1 nil nil))))

(define bpz2
  (lambda (input)
    (mk-ugen (list "BPZ2" (list 0) (list input) nil 1 nil nil))))

(define b-peak-eq
  (lambda (input freq rq db)
    (mk-ugen (list "BPeakEQ" (list 0) (list input freq rq db) nil 1 nil nil))))

(define brf
  (lambda (input freq rq)
    (mk-ugen (list "BRF" (list 0) (list input freq rq) nil 1 nil nil))))

(define brz2
  (lambda (input)
    (mk-ugen (list "BRZ2" (list 0) (list input) nil 1 nil nil))))

(define balance2
  (lambda (rt left right pos level)
    (mk-ugen (list "Balance2" rt (list left right pos level) nil 2 nil nil))))

(define ball
  (lambda (rt input g damp friction)
    (mk-ugen (list "Ball" rt (list input g damp friction) nil 1 nil nil))))

(define beat-track
  (lambda (rt chain lock)
    (mk-ugen (list "BeatTrack" rt (list chain lock) nil 1 nil nil))))

(define beat-track2
  (lambda (rt busindex numfeatures windowsize phaseaccuracy lock weightingscheme)
    (mk-ugen (list "BeatTrack2" rt (list busindex numfeatures windowsize phaseaccuracy lock weightingscheme) nil 6 nil nil))))

(define bi-pan-b2
  (lambda (rt inA inB azimuth gain)
    (mk-ugen (list "BiPanB2" rt (list inA inB azimuth gain) nil 3 nil nil))))

(define binary-op-ugen
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 nil nil))))

(define blip
  (lambda (rt freq numharm)
    (mk-ugen (list "Blip" rt (list freq numharm) nil 1 nil nil))))

(define block-size
  (lambda (rt)
    (mk-ugen (list "BlockSize" rt nil nil 1 nil nil))))

(define brown-noise
  (lambda (rt)
    (mk-ugen (list "BrownNoise" rt nil nil 1 nil (incr-uid 1)))))

(define buf-allpass-c
  (lambda (rt buf input delaytime decaytime)
    (mk-ugen (list "BufAllpassC" rt (list buf input delaytime decaytime) nil 1 nil nil))))

(define buf-allpass-l
  (lambda (rt buf input delaytime decaytime)
    (mk-ugen (list "BufAllpassL" rt (list buf input delaytime decaytime) nil 1 nil nil))))

(define buf-allpass-n
  (lambda (rt buf input delaytime decaytime)
    (mk-ugen (list "BufAllpassN" rt (list buf input delaytime decaytime) nil 1 nil nil))))

(define buf-channels
  (lambda (rt bufnum)
    (mk-ugen (list "BufChannels" rt (list bufnum) nil 1 nil nil))))

(define buf-comb-c
  (lambda (rt buf input delaytime decaytime)
    (mk-ugen (list "BufCombC" rt (list buf input delaytime decaytime) nil 1 nil nil))))

(define buf-comb-l
  (lambda (rt buf input delaytime decaytime)
    (mk-ugen (list "BufCombL" rt (list buf input delaytime decaytime) nil 1 nil nil))))

(define buf-comb-n
  (lambda (rt buf input delaytime decaytime)
    (mk-ugen (list "BufCombN" rt (list buf input delaytime decaytime) nil 1 nil nil))))

(define buf-delay-c
  (lambda (rt buf input delaytime)
    (mk-ugen (list "BufDelayC" rt (list buf input delaytime) nil 1 nil nil))))

(define buf-delay-l
  (lambda (rt buf input delaytime)
    (mk-ugen (list "BufDelayL" rt (list buf input delaytime) nil 1 nil nil))))

(define buf-delay-n
  (lambda (rt buf input delaytime)
    (mk-ugen (list "BufDelayN" rt (list buf input delaytime) nil 1 nil nil))))

(define buf-dur
  (lambda (rt bufnum)
    (mk-ugen (list "BufDur" rt (list bufnum) nil 1 nil nil))))

(define buf-frames
  (lambda (rt bufnum)
    (mk-ugen (list "BufFrames" rt (list bufnum) nil 1 nil nil))))

(define buf-rate-scale
  (lambda (rt bufnum)
    (mk-ugen (list "BufRateScale" rt (list bufnum) nil 1 nil nil))))

(define buf-rd
  (lambda (nc rt bufnum phase loop interpolation)
    (mk-ugen (list "BufRd" rt (list bufnum phase loop interpolation) nil nc nil nil))))

(define buf-sample-rate
  (lambda (rt bufnum)
    (mk-ugen (list "BufSampleRate" rt (list bufnum) nil 1 nil nil))))

(define buf-samples
  (lambda (rt bufnum)
    (mk-ugen (list "BufSamples" rt (list bufnum) nil 1 nil nil))))

(define buf-wr
  (lambda (bufnum phase loop inputArray)
    (mk-ugen (list "BufWr" (list 3) (list bufnum phase loop) inputArray 1 nil nil))))

(define c-osc
  (lambda (rt bufnum freq beats)
    (mk-ugen (list "COsc" rt (list bufnum freq beats) nil 1 nil nil))))

(define check-bad-values
  (lambda (input id_ post)
    (mk-ugen (list "CheckBadValues" (list 0) (list input id_ post) nil 1 nil nil))))

(define clip
  (lambda (input lo hi)
    (mk-ugen (list "Clip" (list 0) (list input lo hi) nil 1 nil nil))))

(define clip-noise
  (lambda (rt)
    (mk-ugen (list "ClipNoise" rt nil nil 1 nil (incr-uid 1)))))

(define coin-gate
  (lambda (prob input)
    (mk-ugen (list "CoinGate" (list 1) (list prob input) nil 1 nil (incr-uid 1)))))

(define comb-c
  (lambda (input maxdelaytime delaytime decaytime)
    (mk-ugen (list "CombC" (list 0) (list input maxdelaytime delaytime decaytime) nil 1 nil nil))))

(define comb-l
  (lambda (input maxdelaytime delaytime decaytime)
    (mk-ugen (list "CombL" (list 0) (list input maxdelaytime delaytime decaytime) nil 1 nil nil))))

(define comb-n
  (lambda (input maxdelaytime delaytime decaytime)
    (mk-ugen (list "CombN" (list 0) (list input maxdelaytime delaytime decaytime) nil 1 nil nil))))

(define compander
  (lambda (input control_ thresh slopeBelow slopeAbove clampTime relaxTime)
    (mk-ugen (list "Compander" (list 0) (list input control_ thresh slopeBelow slopeAbove clampTime relaxTime) nil 1 nil nil))))

(define compander-d
  (lambda (rt input thresh slopeBelow slopeAbove clampTime relaxTime)
    (mk-ugen (list "CompanderD" rt (list input thresh slopeBelow slopeAbove clampTime relaxTime) nil 1 nil nil))))

(define control-dur (mk-ugen (list "ControlDur" ir nil nil 1 nil nil)))

(define control-rate (mk-ugen (list "ControlRate" ir nil nil 1 nil nil)))

(define convolution
  (lambda (rt input kernel framesize)
    (mk-ugen (list "Convolution" rt (list input kernel framesize) nil 1 nil nil))))

(define convolution2
  (lambda (rt input kernel trigger framesize)
    (mk-ugen (list "Convolution2" rt (list input kernel trigger framesize) nil 1 nil nil))))

(define convolution2l
  (lambda (rt input kernel trigger framesize crossfade)
    (mk-ugen (list "Convolution2L" rt (list input kernel trigger framesize crossfade) nil 1 nil nil))))

(define convolution3
  (lambda (rt input kernel trigger framesize)
    (mk-ugen (list "Convolution3" rt (list input kernel trigger framesize) nil 1 nil nil))))

(define crackle
  (lambda (rt chaosParam)
    (mk-ugen (list "Crackle" rt (list chaosParam) nil 1 nil nil))))

(define cusp-l
  (lambda (rt freq a b xi)
    (mk-ugen (list "CuspL" rt (list freq a b xi) nil 1 nil nil))))

(define cusp-n
  (lambda (rt freq a b xi)
    (mk-ugen (list "CuspN" rt (list freq a b xi) nil 1 nil nil))))

(define dc
  (lambda (rt input)
    (mk-ugen (list "DC" rt (list input) nil 1 nil nil))))

(define dbrown
  (lambda (length_ lo hi step)
    (mk-ugen (list "Dbrown" dr (list length_ lo hi step) nil 1 nil (incr-uid 1)))))

(define dbufrd
  (lambda (bufnum phase loop)
    (mk-ugen (list "Dbufrd" dr (list bufnum phase loop) nil 1 nil (incr-uid 1)))))

(define dbufwr
  (lambda (bufnum phase loop input)
    (mk-ugen (list "Dbufwr" dr (list bufnum phase loop input) nil 1 nil (incr-uid 1)))))

(define decay
  (lambda (input decayTime)
    (mk-ugen (list "Decay" (list 0) (list input decayTime) nil 1 nil nil))))

(define decay2
  (lambda (input attackTime decayTime)
    (mk-ugen (list "Decay2" (list 0) (list input attackTime decayTime) nil 1 nil nil))))

(define decode-b2
  (lambda (nc rt w x y orientation)
    (mk-ugen (list "DecodeB2" rt (list w x y orientation) nil nc nil nil))))

(define degree-to-key
  (lambda (bufnum input octave)
    (mk-ugen (list "DegreeToKey" (list 1) (list bufnum input octave) nil 1 nil nil))))

(define del-tap-rd
  (lambda (buffer phase delTime interp)
    (mk-ugen (list "DelTapRd" (list 1) (list buffer phase delTime interp) nil 1 nil nil))))

(define del-tap-wr
  (lambda (buffer input)
    (mk-ugen (list "DelTapWr" (list 1) (list buffer input) nil 1 nil nil))))

(define delay1
  (lambda (input)
    (mk-ugen (list "Delay1" (list 0) (list input) nil 1 nil nil))))

(define delay2
  (lambda (input)
    (mk-ugen (list "Delay2" (list 0) (list input) nil 1 nil nil))))

(define delay-c
  (lambda (input maxdelaytime delaytime)
    (mk-ugen (list "DelayC" (list 0) (list input maxdelaytime delaytime) nil 1 nil nil))))

(define delay-l
  (lambda (input maxdelaytime delaytime)
    (mk-ugen (list "DelayL" (list 0) (list input maxdelaytime delaytime) nil 1 nil nil))))

(define delay-n
  (lambda (input maxdelaytime delaytime)
    (mk-ugen (list "DelayN" (list 0) (list input maxdelaytime delaytime) nil 1 nil nil))))

(define demand
  (lambda (trig reset demandUGens)
    (mk-ugen (list "Demand" (list 0) (list trig reset) demandUGens (length (mce-channels demandUGens)) nil nil))))

(define demand-env-gen
  (lambda (rt level dur shape curve gate reset levelScale levelBias timeScale doneAction)
    (mk-ugen (list "DemandEnvGen" rt (list level dur shape curve gate reset levelScale levelBias timeScale doneAction) nil 1 nil nil))))

(define detect-index
  (lambda (bufnum input)
    (mk-ugen (list "DetectIndex" (list 1) (list bufnum input) nil 1 nil nil))))

(define detect-silence
  (lambda (input amp time doneAction)
    (mk-ugen (list "DetectSilence" (list 0) (list input amp time doneAction) nil 1 nil nil))))

(define dgeom
  (lambda (length_ start grow)
    (mk-ugen (list "Dgeom" dr (list length_ start grow) nil 1 nil (incr-uid 1)))))

(define dibrown
  (lambda (length_ lo hi step)
    (mk-ugen (list "Dibrown" dr (list length_ lo hi step) nil 1 nil (incr-uid 1)))))

(define disk-in
  (lambda (nc bufnum loop)
    (mk-ugen (list "DiskIn" ar (list bufnum loop) nil nc nil nil))))

(define disk-out
  (lambda (bufnum input)
    (mk-ugen (list "DiskOut" ar (list bufnum) input 1 nil nil))))

(define diwhite
  (lambda (length_ lo hi)
    (mk-ugen (list "Diwhite" dr (list length_ lo hi) nil 1 nil (incr-uid 1)))))

(define donce
  (lambda (input)
    (mk-ugen (list "Donce" dr (list input) nil 1 nil (incr-uid 1)))))

(define done
  (lambda (rt src)
    (mk-ugen (list "Done" rt (list src) nil 1 nil nil))))

(define dpoll
  (lambda (input label_ run trigid)
    (mk-ugen (list "Dpoll" dr (list input label_ run trigid) nil 1 nil (incr-uid 1)))))

(define drand
  (lambda (repeats list_)
    (mk-ugen (list "Drand" dr (list repeats) list_ 1 nil (incr-uid 1)))))

(define dreset
  (lambda (input reset)
    (mk-ugen (list "Dreset" dr (list input reset) nil 1 nil (incr-uid 1)))))

(define dseq
  (lambda (repeats list_)
    (mk-ugen (list "Dseq" dr (list repeats) list_ 1 nil (incr-uid 1)))))

(define dser
  (lambda (repeats list_)
    (mk-ugen (list "Dser" dr (list repeats) list_ 1 nil (incr-uid 1)))))

(define dseries
  (lambda (length_ start step)
    (mk-ugen (list "Dseries" dr (list length_ start step) nil 1 nil (incr-uid 1)))))

(define dshuf
  (lambda (repeats list_)
    (mk-ugen (list "Dshuf" dr (list repeats) list_ 1 nil (incr-uid 1)))))

(define dstutter
  (lambda (n input)
    (mk-ugen (list "Dstutter" dr (list n input) nil 1 nil (incr-uid 1)))))

(define dswitch
  (lambda (index list_)
    (mk-ugen (list "Dswitch" dr (list index) list_ 1 nil (incr-uid 1)))))

(define dswitch1
  (lambda (index list_)
    (mk-ugen (list "Dswitch1" dr (list index) list_ 1 nil (incr-uid 1)))))

(define dunique
  (lambda (source maxBufferSize protected)
    (mk-ugen (list "Dunique" dr (list source maxBufferSize protected) nil 1 nil (incr-uid 1)))))

(define dust
  (lambda (rt density)
    (mk-ugen (list "Dust" rt (list density) nil 1 nil (incr-uid 1)))))

(define dust2
  (lambda (rt density)
    (mk-ugen (list "Dust2" rt (list density) nil 1 nil (incr-uid 1)))))

(define dust-r
  (lambda (rt iot_min iot_max)
    (mk-ugen (list "DustR" rt (list iot_min iot_max) nil 1 nil nil))))

(define duty
  (lambda (rt dur reset doneAction level)
    (mk-ugen (list "Duty" rt (list dur reset doneAction level) nil 1 nil nil))))

(define dwhite
  (lambda (length_ lo hi)
    (mk-ugen (list "Dwhite" dr (list length_ lo hi) nil 1 nil (incr-uid 1)))))

(define dwrand
  (lambda (repeats weights list_)
    (mk-ugen (list "Dwrand" dr (list repeats weights) list_ 1 nil (incr-uid 1)))))

(define dxrand
  (lambda (repeats list_)
    (mk-ugen (list "Dxrand" dr (list repeats) list_ 1 nil (incr-uid 1)))))

(define env-gen
  (lambda (rt gate levelScale levelBias timeScale doneAction envelope_)
    (mk-ugen (list "EnvGen" rt (list gate levelScale levelBias timeScale doneAction) envelope_ 1 nil nil))))

(define exp-rand
  (lambda (lo hi)
    (mk-ugen (list "ExpRand" (list 0 1) (list lo hi) nil 1 nil (incr-uid 1)))))

(define fb-sine-c
  (lambda (rt freq im fb a c xi yi)
    (mk-ugen (list "FBSineC" rt (list freq im fb a c xi yi) nil 1 nil nil))))

(define fb-sine-l
  (lambda (rt freq im fb a c xi yi)
    (mk-ugen (list "FBSineL" rt (list freq im fb a c xi yi) nil 1 nil nil))))

(define fb-sine-n
  (lambda (rt freq im fb a c xi yi)
    (mk-ugen (list "FBSineN" rt (list freq im fb a c xi yi) nil 1 nil nil))))

(define fft
  (lambda (buffer input hop wintype active winsize)
    (mk-ugen (list "FFT" kr (list buffer input hop wintype active winsize) nil 1 nil nil))))

(define fos
  (lambda (input a0 a1 b1)
    (mk-ugen (list "FOS" (list 0) (list input a0 a1 b1) nil 1 nil nil))))

(define f-sin-osc
  (lambda (rt freq iphase)
    (mk-ugen (list "FSinOsc" rt (list freq iphase) nil 1 nil nil))))

(define fold
  (lambda (input lo hi)
    (mk-ugen (list "Fold" (list 0) (list input lo hi) nil 1 nil nil))))

(define formant
  (lambda (rt fundfreq formfreq bwfreq)
    (mk-ugen (list "Formant" rt (list fundfreq formfreq bwfreq) nil 1 nil nil))))

(define formlet
  (lambda (input freq attacktime decaytime)
    (mk-ugen (list "Formlet" (list 0) (list input freq attacktime decaytime) nil 1 nil nil))))

(define free
  (lambda (trig id_)
    (mk-ugen (list "Free" (list 0) (list trig id_) nil 1 nil nil))))

(define free-self
  (lambda (input)
    (mk-ugen (list "FreeSelf" kr (list input) nil 1 nil nil))))

(define free-self-when-done
  (lambda (rt src)
    (mk-ugen (list "FreeSelfWhenDone" rt (list src) nil 1 nil nil))))

(define free-verb
  (lambda (input mix room damp)
    (mk-ugen (list "FreeVerb" (list 0) (list input mix room damp) nil 1 nil nil))))

(define free-verb2
  (lambda (input in2 mix room damp)
    (mk-ugen (list "FreeVerb2" (list 0) (list input in2 mix room damp) nil 2 nil nil))))

(define freq-shift
  (lambda (rt input freq phase)
    (mk-ugen (list "FreqShift" rt (list input freq phase) nil 1 nil nil))))

(define g-verb
  (lambda (input roomsize revtime damping inputbw spread drylevel earlyreflevel taillevel maxroomsize)
    (mk-ugen (list "GVerb" (list 0) (list input roomsize revtime damping inputbw spread drylevel earlyreflevel taillevel maxroomsize) nil 2 nil nil))))

(define gate
  (lambda (input trig)
    (mk-ugen (list "Gate" (list 0) (list input trig) nil 1 nil nil))))

(define gbman-l
  (lambda (rt freq xi yi)
    (mk-ugen (list "GbmanL" rt (list freq xi yi) nil 1 nil nil))))

(define gbman-n
  (lambda (rt freq xi yi)
    (mk-ugen (list "GbmanN" rt (list freq xi yi) nil 1 nil nil))))

(define gendy1
  (lambda (rt ampdist durdist adparam ddparam minfreq maxfreq ampscale durscale initCPs knum)
    (mk-ugen (list "Gendy1" rt (list ampdist durdist adparam ddparam minfreq maxfreq ampscale durscale initCPs knum) nil 1 nil (incr-uid 1)))))

(define gendy2
  (lambda (rt ampdist durdist adparam ddparam minfreq maxfreq ampscale durscale initCPs knum a c)
    (mk-ugen (list "Gendy2" rt (list ampdist durdist adparam ddparam minfreq maxfreq ampscale durscale initCPs knum a c) nil 1 nil (incr-uid 1)))))

(define gendy3
  (lambda (rt ampdist durdist adparam ddparam freq ampscale durscale initCPs knum)
    (mk-ugen (list "Gendy3" rt (list ampdist durdist adparam ddparam freq ampscale durscale initCPs knum) nil 1 nil (incr-uid 1)))))

(define grain-buf
  (lambda (nc trigger dur sndbuf rate_ pos interp pan envbufnum maxGrains)
    (mk-ugen (list "GrainBuf" ar (list trigger dur sndbuf rate_ pos interp pan envbufnum maxGrains) nil nc nil nil))))

(define grain-fm
  (lambda (nc trigger dur carfreq modfreq index pan envbufnum maxGrains)
    (mk-ugen (list "GrainFM" ar (list trigger dur carfreq modfreq index pan envbufnum maxGrains) nil nc nil nil))))

(define grain-in
  (lambda (nc trigger dur input pan envbufnum maxGrains)
    (mk-ugen (list "GrainIn" ar (list trigger dur input pan envbufnum maxGrains) nil nc nil nil))))

(define grain-sin
  (lambda (nc trigger dur freq pan envbufnum maxGrains)
    (mk-ugen (list "GrainSin" ar (list trigger dur freq pan envbufnum maxGrains) nil nc nil nil))))

(define gray-noise
  (lambda (rt)
    (mk-ugen (list "GrayNoise" rt nil nil 1 nil (incr-uid 1)))))

(define hpf
  (lambda (input freq)
    (mk-ugen (list "HPF" (list 0) (list input freq) nil 1 nil nil))))

(define hpz1
  (lambda (input)
    (mk-ugen (list "HPZ1" (list 0) (list input) nil 1 nil nil))))

(define hpz2
  (lambda (input)
    (mk-ugen (list "HPZ2" (list 0) (list input) nil 1 nil nil))))

(define hasher
  (lambda (input)
    (mk-ugen (list "Hasher" (list 0) (list input) nil 1 nil nil))))

(define henon-c
  (lambda (rt freq a b x0 x1)
    (mk-ugen (list "HenonC" rt (list freq a b x0 x1) nil 1 nil nil))))

(define henon-l
  (lambda (rt freq a b x0 x1)
    (mk-ugen (list "HenonL" rt (list freq a b x0 x1) nil 1 nil nil))))

(define henon-n
  (lambda (rt freq a b x0 x1)
    (mk-ugen (list "HenonN" rt (list freq a b x0 x1) nil 1 nil nil))))

(define hilbert
  (lambda (input)
    (mk-ugen (list "Hilbert" (list 0) (list input) nil 2 nil nil))))

(define hilbert-fir
  (lambda (rt input buffer)
    (mk-ugen (list "HilbertFIR" rt (list input buffer) nil 2 nil nil))))

(define i-env-gen
  (lambda (rt index envelope_)
    (mk-ugen (list "IEnvGen" rt (list index) envelope_ 1 nil nil))))

(define ifft
  (lambda (buffer wintype winsize)
    (mk-ugen (list "IFFT" ar (list buffer wintype winsize) nil 1 nil nil))))

(define i-rand
  (lambda (lo hi)
    (mk-ugen (list "IRand" ir (list lo hi) nil 1 nil (incr-uid 1)))))

(define impulse
  (lambda (rt freq phase)
    (mk-ugen (list "Impulse" rt (list freq phase) nil 1 nil nil))))

(define in
  (lambda (nc rt bus)
    (mk-ugen (list "In" rt (list bus) nil nc nil nil))))

(define in-feedback
  (lambda (nc bus)
    (mk-ugen (list "InFeedback" ar (list bus) nil nc nil nil))))

(define in-range
  (lambda (input lo hi)
    (mk-ugen (list "InRange" (list 0) (list input lo hi) nil 1 nil nil))))

(define in-rect
  (lambda (rt x y rect)
    (mk-ugen (list "InRect" rt (list x y rect) nil 1 nil nil))))

(define in-trig
  (lambda (nc rt bus)
    (mk-ugen (list "InTrig" rt (list bus) nil nc nil nil))))

(define index
  (lambda (bufnum input)
    (mk-ugen (list "Index" (list 1) (list bufnum input) nil 1 nil nil))))

(define index-in-between
  (lambda (rt bufnum input)
    (mk-ugen (list "IndexInBetween" rt (list bufnum input) nil 1 nil nil))))

(define index-l
  (lambda (rt bufnum input)
    (mk-ugen (list "IndexL" rt (list bufnum input) nil 1 nil nil))))

(define info-ugen-base
  (lambda (rt)
    (mk-ugen (list "InfoUGenBase" rt nil nil 1 nil nil))))

(define integrator
  (lambda (input coef)
    (mk-ugen (list "Integrator" (list 0) (list input coef) nil 1 nil nil))))

(define k2a
  (lambda (input)
    (mk-ugen (list "K2A" ar (list input) nil 1 nil nil))))

(define key-state
  (lambda (rt keycode minval maxval lag)
    (mk-ugen (list "KeyState" rt (list keycode minval maxval lag) nil 1 nil nil))))

(define key-track
  (lambda (rt chain keydecay chromaleak)
    (mk-ugen (list "KeyTrack" rt (list chain keydecay chromaleak) nil 1 nil nil))))

(define klang
  (lambda (rt freqscale freqoffset specificationsArrayRef)
    (mk-ugen (list "Klang" rt (list freqscale freqoffset) specificationsArrayRef 1 nil nil))))

(define klank
  (lambda (input freqscale freqoffset decayscale specificationsArrayRef)
    (mk-ugen (list "Klank" (list 0) (list input freqscale freqoffset decayscale) specificationsArrayRef 1 nil nil))))

(define lf-clip-noise
  (lambda (rt freq)
    (mk-ugen (list "LFClipNoise" rt (list freq) nil 1 nil (incr-uid 1)))))

(define lf-cub
  (lambda (rt freq iphase)
    (mk-ugen (list "LFCub" rt (list freq iphase) nil 1 nil nil))))

(define lfd-clip-noise
  (lambda (rt freq)
    (mk-ugen (list "LFDClipNoise" rt (list freq) nil 1 nil (incr-uid 1)))))

(define lfd-noise0
  (lambda (rt freq)
    (mk-ugen (list "LFDNoise0" rt (list freq) nil 1 nil (incr-uid 1)))))

(define lfd-noise1
  (lambda (rt freq)
    (mk-ugen (list "LFDNoise1" rt (list freq) nil 1 nil (incr-uid 1)))))

(define lfd-noise3
  (lambda (rt freq)
    (mk-ugen (list "LFDNoise3" rt (list freq) nil 1 nil (incr-uid 1)))))

(define lf-gauss
  (lambda (rt duration width iphase loop doneAction)
    (mk-ugen (list "LFGauss" rt (list duration width iphase loop doneAction) nil 1 nil nil))))

(define lf-noise0
  (lambda (rt freq)
    (mk-ugen (list "LFNoise0" rt (list freq) nil 1 nil (incr-uid 1)))))

(define lf-noise1
  (lambda (rt freq)
    (mk-ugen (list "LFNoise1" rt (list freq) nil 1 nil (incr-uid 1)))))

(define lf-noise2
  (lambda (rt freq)
    (mk-ugen (list "LFNoise2" rt (list freq) nil 1 nil (incr-uid 1)))))

(define lf-par
  (lambda (rt freq iphase)
    (mk-ugen (list "LFPar" rt (list freq iphase) nil 1 nil nil))))

(define lf-pulse
  (lambda (rt freq iphase width)
    (mk-ugen (list "LFPulse" rt (list freq iphase width) nil 1 nil nil))))

(define lf-saw
  (lambda (rt freq iphase)
    (mk-ugen (list "LFSaw" rt (list freq iphase) nil 1 nil nil))))

(define lf-tri
  (lambda (rt freq iphase)
    (mk-ugen (list "LFTri" rt (list freq iphase) nil 1 nil nil))))

(define lpf
  (lambda (input freq)
    (mk-ugen (list "LPF" (list 0) (list input freq) nil 1 nil nil))))

(define lpz1
  (lambda (input)
    (mk-ugen (list "LPZ1" (list 0) (list input) nil 1 nil nil))))

(define lpz2
  (lambda (input)
    (mk-ugen (list "LPZ2" (list 0) (list input) nil 1 nil nil))))

(define lag
  (lambda (input lagTime)
    (mk-ugen (list "Lag" (list 0) (list input lagTime) nil 1 nil nil))))

(define lag2
  (lambda (input lagTime)
    (mk-ugen (list "Lag2" (list 0) (list input lagTime) nil 1 nil nil))))

(define lag2ud
  (lambda (input lagTimeU lagTimeD)
    (mk-ugen (list "Lag2UD" (list 0) (list input lagTimeU lagTimeD) nil 1 nil nil))))

(define lag3
  (lambda (input lagTime)
    (mk-ugen (list "Lag3" (list 0) (list input lagTime) nil 1 nil nil))))

(define lag3ud
  (lambda (input lagTimeU lagTimeD)
    (mk-ugen (list "Lag3UD" (list 0) (list input lagTimeU lagTimeD) nil 1 nil nil))))

(define lag-control
  (lambda (rt values lags)
    (mk-ugen (list "LagControl" rt (list values lags) nil 1 nil nil))))

(define lag-in
  (lambda (nc rt bus lag)
    (mk-ugen (list "LagIn" rt (list bus lag) nil nc nil nil))))

(define lag-ud
  (lambda (input lagTimeU lagTimeD)
    (mk-ugen (list "LagUD" (list 0) (list input lagTimeU lagTimeD) nil 1 nil nil))))

(define last-value
  (lambda (input diff)
    (mk-ugen (list "LastValue" (list 0) (list input diff) nil 1 nil nil))))

(define latch
  (lambda (input trig)
    (mk-ugen (list "Latch" (list 0) (list input trig) nil 1 nil nil))))

(define latoocarfian-c
  (lambda (rt freq a b c d xi yi)
    (mk-ugen (list "LatoocarfianC" rt (list freq a b c d xi yi) nil 1 nil nil))))

(define latoocarfian-l
  (lambda (rt freq a b c d xi yi)
    (mk-ugen (list "LatoocarfianL" rt (list freq a b c d xi yi) nil 1 nil nil))))

(define latoocarfian-n
  (lambda (rt freq a b c d xi yi)
    (mk-ugen (list "LatoocarfianN" rt (list freq a b c d xi yi) nil 1 nil nil))))

(define leak-dc
  (lambda (input coef)
    (mk-ugen (list "LeakDC" (list 0) (list input coef) nil 1 nil nil))))

(define least-change
  (lambda (rt a b)
    (mk-ugen (list "LeastChange" rt (list a b) nil 1 nil nil))))

(define limiter
  (lambda (input level dur)
    (mk-ugen (list "Limiter" (list 0) (list input level dur) nil 1 nil nil))))

(define lin-cong-c
  (lambda (rt freq a c m xi)
    (mk-ugen (list "LinCongC" rt (list freq a c m xi) nil 1 nil nil))))

(define lin-cong-l
  (lambda (rt freq a c m xi)
    (mk-ugen (list "LinCongL" rt (list freq a c m xi) nil 1 nil nil))))

(define lin-cong-n
  (lambda (rt freq a c m xi)
    (mk-ugen (list "LinCongN" rt (list freq a c m xi) nil 1 nil nil))))

(define lin-exp
  (lambda (input srclo srchi dstlo dsthi)
    (mk-ugen (list "LinExp" (list 0) (list input srclo srchi dstlo dsthi) nil 1 nil nil))))

(define lin-pan2
  (lambda (input pos level)
    (mk-ugen (list "LinPan2" (list 0) (list input pos level) nil 2 nil nil))))

(define lin-rand
  (lambda (lo hi minmax)
    (mk-ugen (list "LinRand" ir (list lo hi minmax) nil 1 nil (incr-uid 1)))))

(define lin-x-fade2
  (lambda (inA inB pan level)
    (mk-ugen (list "LinXFade2" (list 0 1) (list inA inB pan level) nil 1 nil nil))))

(define line
  (lambda (rt start end dur doneAction)
    (mk-ugen (list "Line" rt (list start end dur doneAction) nil 1 nil nil))))

(define linen
  (lambda (gate attackTime susLevel releaseTime doneAction)
    (mk-ugen (list "Linen" kr (list gate attackTime susLevel releaseTime doneAction) nil 1 nil nil))))

(define local-buf
  (lambda (numChannels numFrames)
    (mk-ugen (list "LocalBuf" ir (list numChannels numFrames) nil 1 nil (incr-uid 1)))))

(define local-in
  (lambda (nc rt default_)
    (mk-ugen (list "LocalIn" rt nil default_ nc nil nil))))

(define local-out
  (lambda (input)
    (mk-ugen (list "LocalOut" (list 0) nil input 1 nil nil))))

(define logistic
  (lambda (rt chaosParam freq init_)
    (mk-ugen (list "Logistic" rt (list chaosParam freq init_) nil 1 nil nil))))

(define lorenz-l
  (lambda (rt freq s r b h xi yi zi)
    (mk-ugen (list "LorenzL" rt (list freq s r b h xi yi zi) nil 1 nil nil))))

(define loudness
  (lambda (rt chain smask tmask)
    (mk-ugen (list "Loudness" rt (list chain smask tmask) nil 1 nil nil))))

(define mfcc
  (lambda (rt chain numcoeff)
    (mk-ugen (list "MFCC" rt (list chain numcoeff) nil 13 nil nil))))

(define mantissa-mask
  (lambda (input bits)
    (mk-ugen (list "MantissaMask" (list 0) (list input bits) nil 1 nil nil))))

(define max-local-bufs
  (lambda (count)
    (mk-ugen (list "MaxLocalBufs" ir (list count) nil 1 nil nil))))

(define median
  (lambda (length_ input)
    (mk-ugen (list "Median" (list 1) (list length_ input) nil 1 nil nil))))

(define mid-eq
  (lambda (input freq rq db)
    (mk-ugen (list "MidEQ" (list 0) (list input freq rq db) nil 1 nil nil))))

(define mod-dif
  (lambda (rt x y mod_)
    (mk-ugen (list "ModDif" rt (list x y mod_) nil 1 nil nil))))

(define moog-ff
  (lambda (input freq gain reset)
    (mk-ugen (list "MoogFF" (list 0) (list input freq gain reset) nil 1 nil nil))))

(define most-change
  (lambda (a b)
    (mk-ugen (list "MostChange" (list 0 1) (list a b) nil 1 nil nil))))

(define mouse-button
  (lambda (rt minval maxval lag)
    (mk-ugen (list "MouseButton" rt (list minval maxval lag) nil 1 nil nil))))

(define mouse-x
  (lambda (rt minval maxval warp lag)
    (mk-ugen (list "MouseX" rt (list minval maxval warp lag) nil 1 nil nil))))

(define mouse-y
  (lambda (rt minval maxval warp lag)
    (mk-ugen (list "MouseY" rt (list minval maxval warp lag) nil 1 nil nil))))

(define n-rand
  (lambda (lo hi n)
    (mk-ugen (list "NRand" ir (list lo hi n) nil 1 nil (incr-uid 1)))))

(define normalizer
  (lambda (input level dur)
    (mk-ugen (list "Normalizer" (list 0) (list input level dur) nil 1 nil nil))))

(define num-audio-buses (mk-ugen (list "NumAudioBuses" ir nil nil 1 nil nil)))

(define num-buffers (mk-ugen (list "NumBuffers" ir nil nil 1 nil nil)))

(define num-control-buses (mk-ugen (list "NumControlBuses" ir nil nil 1 nil nil)))

(define num-input-buses (mk-ugen (list "NumInputBuses" ir nil nil 1 nil nil)))

(define num-output-buses (mk-ugen (list "NumOutputBuses" ir nil nil 1 nil nil)))

(define num-running-synths (mk-ugen (list "NumRunningSynths" ir nil nil 1 nil nil)))

(define offset-out
  (lambda (bus input)
    (mk-ugen (list "OffsetOut" (list 1) (list bus) input 1 nil nil))))

(define one-pole
  (lambda (input coef)
    (mk-ugen (list "OnePole" (list 0) (list input coef) nil 1 nil nil))))

(define one-zero
  (lambda (input coef)
    (mk-ugen (list "OneZero" (list 0) (list input coef) nil 1 nil nil))))

(define onsets
  (lambda (chain threshold odftype relaxtime floor_ mingap medianspan whtype rawodf)
    (mk-ugen (list "Onsets" kr (list chain threshold odftype relaxtime floor_ mingap medianspan whtype rawodf) nil 1 nil nil))))

(define osc
  (lambda (rt bufnum freq phase)
    (mk-ugen (list "Osc" rt (list bufnum freq phase) nil 1 nil nil))))

(define osc-n
  (lambda (rt bufnum freq phase)
    (mk-ugen (list "OscN" rt (list bufnum freq phase) nil 1 nil nil))))

(define out
  (lambda (bus input)
    (mk-ugen (list "Out" (list 1) (list bus) input 1 nil nil))))

(define p-sin-grain
  (lambda (rt freq dur amp)
    (mk-ugen (list "PSinGrain" rt (list freq dur amp) nil 1 nil nil))))

(define pv-add
  (lambda (bufferA bufferB)
    (mk-ugen (list "PV_Add" kr (list bufferA bufferB) nil 1 nil nil))))

(define pv-bin-scramble
  (lambda (buffer wipe width trig)
    (mk-ugen (list "PV_BinScramble" kr (list buffer wipe width trig) nil 1 nil (incr-uid 1)))))

(define pv-bin-shift
  (lambda (buffer stretch shift interp)
    (mk-ugen (list "PV_BinShift" kr (list buffer stretch shift interp) nil 1 nil nil))))

(define pv-bin-wipe
  (lambda (bufferA bufferB wipe)
    (mk-ugen (list "PV_BinWipe" kr (list bufferA bufferB wipe) nil 1 nil nil))))

(define pv-brick-wall
  (lambda (buffer wipe)
    (mk-ugen (list "PV_BrickWall" kr (list buffer wipe) nil 1 nil nil))))

(define pv-chain-ugen
  (lambda (maxSize)
    (mk-ugen (list "PV_ChainUGen" kr (list maxSize) nil 1 nil nil))))

(define pv-conformal-map
  (lambda (buffer areal aimag)
    (mk-ugen (list "PV_ConformalMap" kr (list buffer areal aimag) nil 1 nil nil))))

(define pv-conj
  (lambda (buffer)
    (mk-ugen (list "PV_Conj" kr (list buffer) nil 1 nil nil))))

(define pv-copy
  (lambda (bufferA bufferB)
    (mk-ugen (list "PV_Copy" kr (list bufferA bufferB) nil 1 nil nil))))

(define pv-copy-phase
  (lambda (bufferA bufferB)
    (mk-ugen (list "PV_CopyPhase" kr (list bufferA bufferB) nil 1 nil nil))))

(define pv-diffuser
  (lambda (buffer trig)
    (mk-ugen (list "PV_Diffuser" kr (list buffer trig) nil 1 nil nil))))

(define pv-div
  (lambda (bufferA bufferB)
    (mk-ugen (list "PV_Div" kr (list bufferA bufferB) nil 1 nil nil))))

(define pv-hainsworth-foote
  (lambda (maxSize)
    (mk-ugen (list "PV_HainsworthFoote" kr (list maxSize) nil 1 nil nil))))

(define pv-jensen-andersen
  (lambda (maxSize)
    (mk-ugen (list "PV_JensenAndersen" kr (list maxSize) nil 1 nil nil))))

(define pv-local-max
  (lambda (buffer threshold)
    (mk-ugen (list "PV_LocalMax" kr (list buffer threshold) nil 1 nil nil))))

(define pv-mag-above
  (lambda (buffer threshold)
    (mk-ugen (list "PV_MagAbove" kr (list buffer threshold) nil 1 nil nil))))

(define pv-mag-below
  (lambda (buffer threshold)
    (mk-ugen (list "PV_MagBelow" kr (list buffer threshold) nil 1 nil nil))))

(define pv-mag-clip
  (lambda (buffer threshold)
    (mk-ugen (list "PV_MagClip" kr (list buffer threshold) nil 1 nil nil))))

(define pv-mag-div
  (lambda (bufferA bufferB zeroed)
    (mk-ugen (list "PV_MagDiv" kr (list bufferA bufferB zeroed) nil 1 nil nil))))

(define pv-mag-freeze
  (lambda (buffer freeze)
    (mk-ugen (list "PV_MagFreeze" kr (list buffer freeze) nil 1 nil nil))))

(define pv-mag-mul
  (lambda (bufferA bufferB)
    (mk-ugen (list "PV_MagMul" kr (list bufferA bufferB) nil 1 nil nil))))

(define pv-mag-noise
  (lambda (buffer)
    (mk-ugen (list "PV_MagNoise" kr (list buffer) nil 1 nil nil))))

(define pv-mag-shift
  (lambda (buffer stretch shift)
    (mk-ugen (list "PV_MagShift" kr (list buffer stretch shift) nil 1 nil nil))))

(define pv-mag-smear
  (lambda (buffer bins)
    (mk-ugen (list "PV_MagSmear" kr (list buffer bins) nil 1 nil nil))))

(define pv-mag-squared
  (lambda (buffer)
    (mk-ugen (list "PV_MagSquared" kr (list buffer) nil 1 nil nil))))

(define pv-max
  (lambda (bufferA bufferB)
    (mk-ugen (list "PV_Max" kr (list bufferA bufferB) nil 1 nil nil))))

(define pv-min
  (lambda (bufferA bufferB)
    (mk-ugen (list "PV_Min" kr (list bufferA bufferB) nil 1 nil nil))))

(define pv-mul
  (lambda (bufferA bufferB)
    (mk-ugen (list "PV_Mul" kr (list bufferA bufferB) nil 1 nil nil))))

(define pv-phase-shift
  (lambda (buffer shift integrate)
    (mk-ugen (list "PV_PhaseShift" kr (list buffer shift integrate) nil 1 nil nil))))

(define pv-phase-shift270
  (lambda (buffer)
    (mk-ugen (list "PV_PhaseShift270" kr (list buffer) nil 1 nil nil))))

(define pv-phase-shift90
  (lambda (buffer)
    (mk-ugen (list "PV_PhaseShift90" kr (list buffer) nil 1 nil nil))))

(define pv-rand-comb
  (lambda (buffer wipe trig)
    (mk-ugen (list "PV_RandComb" kr (list buffer wipe trig) nil 1 nil (incr-uid 1)))))

(define pv-rand-wipe
  (lambda (bufferA bufferB wipe trig)
    (mk-ugen (list "PV_RandWipe" kr (list bufferA bufferB wipe trig) nil 1 nil (incr-uid 1)))))

(define pv-rect-comb
  (lambda (buffer numTeeth phase width)
    (mk-ugen (list "PV_RectComb" kr (list buffer numTeeth phase width) nil 1 nil nil))))

(define pv-rect-comb2
  (lambda (bufferA bufferB numTeeth phase width)
    (mk-ugen (list "PV_RectComb2" kr (list bufferA bufferB numTeeth phase width) nil 1 nil nil))))

(define pv-split
  (lambda (bufferA bufferB)
    (mk-ugen (list "PV_Split" kr (list bufferA bufferB) nil 2 nil nil))))

(define pan2
  (lambda (input pos level)
    (mk-ugen (list "Pan2" (list 0) (list input pos level) nil 2 nil nil))))

(define pan4
  (lambda (rt input xpos ypos level)
    (mk-ugen (list "Pan4" rt (list input xpos ypos level) nil 4 nil nil))))

(define pan-az
  (lambda (nc input pos level width orientation)
    (mk-ugen (list "PanAz" (list 0) (list input pos level width orientation) nil nc nil nil))))

(define pan-b
  (lambda (rt input azimuth elevation gain)
    (mk-ugen (list "PanB" rt (list input azimuth elevation gain) nil 4 nil nil))))

(define pan-b2
  (lambda (rt input azimuth gain)
    (mk-ugen (list "PanB2" rt (list input azimuth gain) nil 3 nil nil))))

(define part-conv
  (lambda (input fftsize irbufnum)
    (mk-ugen (list "PartConv" ar (list input fftsize irbufnum) nil 1 nil nil))))

(define pause
  (lambda (rt gate id_)
    (mk-ugen (list "Pause" rt (list gate id_) nil 1 nil nil))))

(define pause-self
  (lambda (rt input)
    (mk-ugen (list "PauseSelf" rt (list input) nil 1 nil nil))))

(define pause-self-when-done
  (lambda (rt src)
    (mk-ugen (list "PauseSelfWhenDone" rt (list src) nil 1 nil nil))))

(define peak
  (lambda (input trig)
    (mk-ugen (list "Peak" (list 0) (list input trig) nil 1 nil nil))))

(define peak-follower
  (lambda (input decay)
    (mk-ugen (list "PeakFollower" (list 0) (list input decay) nil 1 nil nil))))

(define phasor
  (lambda (rt trig rate_ start end resetPos)
    (mk-ugen (list "Phasor" rt (list trig rate_ start end resetPos) nil 1 nil nil))))

(define pink-noise
  (lambda (rt)
    (mk-ugen (list "PinkNoise" rt nil nil 1 nil (incr-uid 1)))))

(define pitch
  (lambda (input initFreq minFreq maxFreq execFreq maxBinsPerOctave median ampThreshold peakThreshold downSample clar)
    (mk-ugen (list "Pitch" kr (list input initFreq minFreq maxFreq execFreq maxBinsPerOctave median ampThreshold peakThreshold downSample clar) nil 2 nil nil))))

(define pitch-shift
  (lambda (input windowSize pitchRatio pitchDispersion timeDispersion)
    (mk-ugen (list "PitchShift" (list 0) (list input windowSize pitchRatio pitchDispersion timeDispersion) nil 1 nil nil))))

(define play-buf
  (lambda (nc rt bufnum rate_ trigger startPos loop doneAction)
    (mk-ugen (list "PlayBuf" rt (list bufnum rate_ trigger startPos loop doneAction) nil nc nil nil))))

(define pluck
  (lambda (input trig maxdelaytime delaytime decaytime coef)
    (mk-ugen (list "Pluck" (list 0) (list input trig maxdelaytime delaytime decaytime coef) nil 1 nil nil))))

(define poll
  (lambda (trig input label_ trigid)
    (mk-ugen (list "Poll" (list 1) (list trig input label_ trigid) nil 1 nil nil))))

(define pulse
  (lambda (rt freq width)
    (mk-ugen (list "Pulse" rt (list freq width) nil 1 nil nil))))

(define pulse-count
  (lambda (trig reset)
    (mk-ugen (list "PulseCount" (list 0) (list trig reset) nil 1 nil nil))))

(define pulse-divider
  (lambda (trig div_ start)
    (mk-ugen (list "PulseDivider" (list 0) (list trig div_ start) nil 1 nil nil))))

(define quad-c
  (lambda (rt freq a b c xi)
    (mk-ugen (list "QuadC" rt (list freq a b c xi) nil 1 nil nil))))

(define quad-l
  (lambda (rt freq a b c xi)
    (mk-ugen (list "QuadL" rt (list freq a b c xi) nil 1 nil nil))))

(define quad-n
  (lambda (rt freq a b c xi)
    (mk-ugen (list "QuadN" rt (list freq a b c xi) nil 1 nil nil))))

(define r-delay-map
  (lambda (bufnum input dynamic spec)
    (mk-ugen (list "RDelayMap" (list 1) (list bufnum input dynamic) spec 1 nil nil))))

(define r-delay-set
  (lambda (rt input spec)
    (mk-ugen (list "RDelaySet" rt (list input spec) nil 1 nil nil))))

(define r-delay-set-b
  (lambda (rt bufnum input spec)
    (mk-ugen (list "RDelaySetB" rt (list bufnum input spec) nil 1 nil nil))))

(define r-freezer
  (lambda (rt bufnum left right gain increment incrementOffset incrementRandom rightRandom syncPhaseTrigger randomizePhaseTrigger numberOfLoops)
    (mk-ugen (list "RFreezer" rt (list bufnum left right gain increment incrementOffset incrementRandom rightRandom syncPhaseTrigger randomizePhaseTrigger numberOfLoops) nil 1 nil nil))))

(define rhpf
  (lambda (input freq rq)
    (mk-ugen (list "RHPF" (list 0) (list input freq rq) nil 1 nil nil))))

(define rlpf
  (lambda (input freq rq)
    (mk-ugen (list "RLPF" (list 0) (list input freq rq) nil 1 nil nil))))

(define r-loop-set
  (lambda (rt bufnum left right gain increment spec)
    (mk-ugen (list "RLoopSet" rt (list bufnum left right gain increment spec) nil 1 nil nil))))

(define r-play-trace
  (lambda (rt bufnum degree rate_ axis)
    (mk-ugen (list "RPlayTrace" rt (list bufnum degree rate_ axis) nil 1 nil nil))))

(define r-shuffler-b
  (lambda (bufnum readLocationMinima readLocationMaxima readIncrementMinima readIncrementMaxima durationMinima durationMaxima envelopeAmplitudeMinima envelopeAmplitudeMaxima envelopeShapeMinima envelopeShapeMaxima envelopeSkewMinima envelopeSkewMaxima stereoLocationMinima stereoLocationMaxima interOffsetTimeMinima interOffsetTimeMaxima ftableReadLocationIncrement readIncrementQuanta interOffsetTimeQuanta)
    (mk-ugen (list "RShufflerB" ar (list bufnum readLocationMinima readLocationMaxima readIncrementMinima readIncrementMaxima durationMinima durationMaxima envelopeAmplitudeMinima envelopeAmplitudeMaxima envelopeShapeMinima envelopeShapeMaxima envelopeSkewMinima envelopeSkewMaxima stereoLocationMinima stereoLocationMaxima interOffsetTimeMinima interOffsetTimeMaxima ftableReadLocationIncrement readIncrementQuanta interOffsetTimeQuanta) nil 2 nil nil))))

(define r-shuffler-l
  (lambda (rt input fragmentSize maxDelay)
    (mk-ugen (list "RShufflerL" rt (list input fragmentSize maxDelay) nil 1 nil nil))))

(define r-trace-rd
  (lambda (rt bufnum degree index axis)
    (mk-ugen (list "RTraceRd" rt (list bufnum degree index axis) nil 1 nil nil))))

(define r-trace-rd-x
  (lambda (rt bufnum degree index)
    (mk-ugen (list "RTraceRdX" rt (list bufnum degree index) nil 1 nil nil))))

(define r-trace-rd-y
  (lambda (rt bufnum degree index)
    (mk-ugen (list "RTraceRdY" rt (list bufnum degree index) nil 1 nil nil))))

(define r-trace-rd-z
  (lambda (rt bufnum degree index)
    (mk-ugen (list "RTraceRdZ" rt (list bufnum degree index) nil 1 nil nil))))

(define radians-per-sample (mk-ugen (list "RadiansPerSample" ir nil nil 1 nil nil)))

(define ramp
  (lambda (input lagTime)
    (mk-ugen (list "Ramp" (list 0) (list input lagTime) nil 1 nil nil))))

(define rand
  (lambda (lo hi)
    (mk-ugen (list "Rand" ir (list lo hi) nil 1 nil (incr-uid 1)))))

(define rand-id
  (lambda (rt id_)
    (mk-ugen (list "RandID" rt (list id_) nil 1 nil nil))))

(define rand-seed
  (lambda (rt trig seed)
    (mk-ugen (list "RandSeed" rt (list trig seed) nil 1 nil nil))))

(define record-buf
  (lambda (rt bufnum offset recLevel preLevel run loop trigger doneAction inputArray)
    (mk-ugen (list "RecordBuf" rt (list bufnum offset recLevel preLevel run loop trigger doneAction) inputArray 1 nil nil))))

(define replace-out
  (lambda (bus input)
    (mk-ugen (list "ReplaceOut" (list 1) (list bus) input 1 nil nil))))

(define resonz
  (lambda (input freq bwr)
    (mk-ugen (list "Resonz" (list 0) (list input freq bwr) nil 1 nil nil))))

(define ringz
  (lambda (input freq decaytime)
    (mk-ugen (list "Ringz" (list 0) (list input freq decaytime) nil 1 nil nil))))

(define rotate2
  (lambda (x y pos)
    (mk-ugen (list "Rotate2" (list 0 1) (list x y pos) nil 2 nil nil))))

(define running-max
  (lambda (input trig)
    (mk-ugen (list "RunningMax" (list 0) (list input trig) nil 1 nil nil))))

(define running-min
  (lambda (input trig)
    (mk-ugen (list "RunningMin" (list 0) (list input trig) nil 1 nil nil))))

(define running-sum
  (lambda (input numsamp)
    (mk-ugen (list "RunningSum" (list 0) (list input numsamp) nil 1 nil nil))))

(define sos
  (lambda (input a0 a1 a2 b1 b2)
    (mk-ugen (list "SOS" (list 0) (list input a0 a1 a2 b1 b2) nil 1 nil nil))))

(define sample-dur (mk-ugen (list "SampleDur" ir nil nil 1 nil nil)))

(define sample-rate (mk-ugen (list "SampleRate" ir nil nil 1 nil nil)))

(define saw
  (lambda (rt freq)
    (mk-ugen (list "Saw" rt (list freq) nil 1 nil nil))))

(define schmidt
  (lambda (rt input lo hi)
    (mk-ugen (list "Schmidt" rt (list input lo hi) nil 1 nil nil))))

(define scope-out
  (lambda (rt inputArray bufnum)
    (mk-ugen (list "ScopeOut" rt (list inputArray bufnum) nil 1 nil nil))))

(define scope-out2
  (lambda (rt inputArray scopeNum maxFrames scopeFrames)
    (mk-ugen (list "ScopeOut2" rt (list inputArray scopeNum maxFrames scopeFrames) nil 1 nil nil))))

(define select
  (lambda (which array)
    (mk-ugen (list "Select" (list 0 1) (list which) array 1 nil nil))))

(define send-trig
  (lambda (input id_ value)
    (mk-ugen (list "SendTrig" (list 0) (list input id_ value) nil 1 nil nil))))

(define set-reset-ff
  (lambda (trig reset)
    (mk-ugen (list "SetResetFF" (list 0) (list trig reset) nil 1 nil nil))))

(define shaper
  (lambda (bufnum input)
    (mk-ugen (list "Shaper" (list 1) (list bufnum input) nil 1 nil nil))))

(define sin-osc
  (lambda (rt freq phase)
    (mk-ugen (list "SinOsc" rt (list freq phase) nil 1 nil nil))))

(define sin-osc-fb
  (lambda (rt freq feedback)
    (mk-ugen (list "SinOscFB" rt (list freq feedback) nil 1 nil nil))))

(define slew
  (lambda (input up dn)
    (mk-ugen (list "Slew" (list 0) (list input up dn) nil 1 nil nil))))

(define slope
  (lambda (input)
    (mk-ugen (list "Slope" (list 0) (list input) nil 1 nil nil))))

(define spec-centroid
  (lambda (rt buffer)
    (mk-ugen (list "SpecCentroid" rt (list buffer) nil 1 nil nil))))

(define spec-flatness
  (lambda (rt buffer)
    (mk-ugen (list "SpecFlatness" rt (list buffer) nil 1 nil nil))))

(define spec-pcile
  (lambda (rt buffer fraction interpolate)
    (mk-ugen (list "SpecPcile" rt (list buffer fraction interpolate) nil 1 nil nil))))

(define spring
  (lambda (rt input spring damp)
    (mk-ugen (list "Spring" rt (list input spring damp) nil 1 nil nil))))

(define standard-l
  (lambda (rt freq k xi yi)
    (mk-ugen (list "StandardL" rt (list freq k xi yi) nil 1 nil nil))))

(define standard-n
  (lambda (rt freq k xi yi)
    (mk-ugen (list "StandardN" rt (list freq k xi yi) nil 1 nil nil))))

(define stepper
  (lambda (trig reset min_ max_ step resetval)
    (mk-ugen (list "Stepper" (list 0) (list trig reset min_ max_ step resetval) nil 1 nil nil))))

(define stereo-convolution2l
  (lambda (rt input kernelL kernelR trigger framesize crossfade)
    (mk-ugen (list "StereoConvolution2L" rt (list input kernelL kernelR trigger framesize crossfade) nil 2 nil nil))))

(define subsample-offset (mk-ugen (list "SubsampleOffset" ir nil nil 1 nil nil)))

(define sum3
  (lambda (in0 in1 in2)
    (mk-ugen (list "Sum3" (list 0 1 2) (list in0 in1 in2) nil 1 nil nil))))

(define sum4
  (lambda (in0 in1 in2 in3)
    (mk-ugen (list "Sum4" (list 0 1 2 3) (list in0 in1 in2 in3) nil 1 nil nil))))

(define sweep
  (lambda (trig rate_)
    (mk-ugen (list "Sweep" (list 0) (list trig rate_) nil 1 nil nil))))

(define sync-saw
  (lambda (rt syncFreq sawFreq)
    (mk-ugen (list "SyncSaw" rt (list syncFreq sawFreq) nil 1 nil nil))))

(define t2a
  (lambda (input offset)
    (mk-ugen (list "T2A" ar (list input offset) nil 1 nil nil))))

(define t2k
  (lambda (rt input)
    (mk-ugen (list "T2K" rt (list input) nil 1 nil nil))))

(define t-ball
  (lambda (rt input g damp friction)
    (mk-ugen (list "TBall" rt (list input g damp friction) nil 1 nil nil))))

(define t-delay
  (lambda (input dur)
    (mk-ugen (list "TDelay" (list 0) (list input dur) nil 1 nil nil))))

(define t-duty
  (lambda (rt dur reset doneAction level gapFirst)
    (mk-ugen (list "TDuty" rt (list dur reset doneAction level gapFirst) nil 1 nil nil))))

(define t-exp-rand
  (lambda (lo hi trig)
    (mk-ugen (list "TExpRand" (list 2) (list lo hi trig) nil 1 nil (incr-uid 1)))))

(define t-grains
  (lambda (nc trigger bufnum rate_ centerPos dur pan amp interp)
    (mk-ugen (list "TGrains" ar (list trigger bufnum rate_ centerPos dur pan amp interp) nil nc nil nil))))

(define ti-rand
  (lambda (lo hi trig)
    (mk-ugen (list "TIRand" (list 2) (list lo hi trig) nil 1 nil (incr-uid 1)))))

(define t-rand
  (lambda (lo hi trig)
    (mk-ugen (list "TRand" (list 2) (list lo hi trig) nil 1 nil (incr-uid 1)))))

(define t-windex
  (lambda (input normalize array)
    (mk-ugen (list "TWindex" (list 0) (list input normalize) array 1 nil (incr-uid 1)))))

(define tap
  (lambda (nc rt bufnum delaytime)
    (mk-ugen (list "Tap" rt (list bufnum delaytime) nil nc nil nil))))

(define timer
  (lambda (trig)
    (mk-ugen (list "Timer" (list 0) (list trig) nil 1 nil nil))))

(define toggle-ff
  (lambda (trig)
    (mk-ugen (list "ToggleFF" (list 0) (list trig) nil 1 nil nil))))

(define trig
  (lambda (input dur)
    (mk-ugen (list "Trig" (list 0) (list input dur) nil 1 nil nil))))

(define trig1
  (lambda (input dur)
    (mk-ugen (list "Trig1" (list 0) (list input dur) nil 1 nil nil))))

(define trig-control
  (lambda (rt values)
    (mk-ugen (list "TrigControl" rt (list values) nil 1 nil nil))))

(define two-pole
  (lambda (input freq radius)
    (mk-ugen (list "TwoPole" (list 0) (list input freq radius) nil 1 nil nil))))

(define two-zero
  (lambda (input freq radius)
    (mk-ugen (list "TwoZero" (list 0) (list input freq radius) nil 1 nil nil))))

(define unary-op-ugen
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 nil nil))))

(define v-disk-in
  (lambda (nc bufnum rate_ loop sendID)
    (mk-ugen (list "VDiskIn" ar (list bufnum rate_ loop sendID) nil nc nil nil))))

(define v-osc
  (lambda (rt bufpos freq phase)
    (mk-ugen (list "VOsc" rt (list bufpos freq phase) nil 1 nil nil))))

(define v-osc3
  (lambda (rt bufpos freq1 freq2 freq3)
    (mk-ugen (list "VOsc3" rt (list bufpos freq1 freq2 freq3) nil 1 nil nil))))

(define var-lag
  (lambda (rt input time curvature warp start)
    (mk-ugen (list "VarLag" rt (list input time curvature warp start) nil 1 nil nil))))

(define var-saw
  (lambda (rt freq iphase width)
    (mk-ugen (list "VarSaw" rt (list freq iphase width) nil 1 nil nil))))

(define vibrato
  (lambda (rt freq rate_ depth delay onset rateVariation depthVariation iphase)
    (mk-ugen (list "Vibrato" rt (list freq rate_ depth delay onset rateVariation depthVariation iphase) nil 1 nil (incr-uid 1)))))

(define warp1
  (lambda (nc bufnum pointer freqScale windowSize envbufnum overlaps windowRandRatio interp)
    (mk-ugen (list "Warp1" ar (list bufnum pointer freqScale windowSize envbufnum overlaps windowRandRatio interp) nil nc nil nil))))

(define white-noise
  (lambda (rt)
    (mk-ugen (list "WhiteNoise" rt nil nil 1 nil (incr-uid 1)))))

(define width-first-ugen
  (lambda (rt maxSize)
    (mk-ugen (list "WidthFirstUGen" rt (list maxSize) nil 1 nil nil))))

(define wrap
  (lambda (input lo hi)
    (mk-ugen (list "Wrap" (list 0) (list input lo hi) nil 1 nil nil))))

(define wrap-index
  (lambda (bufnum input)
    (mk-ugen (list "WrapIndex" (list 1) (list bufnum input) nil 1 nil nil))))

(define x-fade2
  (lambda (inA inB pan level)
    (mk-ugen (list "XFade2" (list 0 1) (list inA inB pan level) nil 1 nil nil))))

(define x-line
  (lambda (rt start end dur doneAction)
    (mk-ugen (list "XLine" rt (list start end dur doneAction) nil 1 nil nil))))

(define x-out
  (lambda (bus xfade input)
    (mk-ugen (list "XOut" (list 2) (list bus xfade) input 1 nil nil))))

(define zero-crossing
  (lambda (input)
    (mk-ugen (list "ZeroCrossing" (list 0) (list input) nil 1 nil nil))))

(define mul-add
  (lambda (input mul add)
    (mk-ugen (list "MulAdd" (list 0) (list input mul add) nil 1 nil nil))))

(define set-buf
  (lambda (buf offset length_ array)
    (mk-ugen (list "SetBuf" ir (list buf offset length_) array 1 nil nil))))

(define neg
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 0 nil))))

(define u:not
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 1 nil))))

(define is-nil
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 2 nil))))

(define not-nil
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 3 nil))))

(define bit-not
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 4 nil))))

(define u:abs
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 5 nil))))

(define as-float
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 6 nil))))

(define as-int
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 7 nil))))

(define ceil
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 8 nil))))

(define u:floor
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 9 nil))))

(define frac
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 10 nil))))

(define sign
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 11 nil))))

(define squared
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 12 nil))))

(define cubed
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 13 nil))))

(define u:sqrt
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 14 nil))))

(define u:exp
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 15 nil))))

(define recip
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 16 nil))))

(define midicps
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 17 nil))))

(define cpsmidi
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 18 nil))))

(define midi-ratio
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 19 nil))))

(define ratio-midi
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 20 nil))))

(define db-amp
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 21 nil))))

(define amp-db
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 22 nil))))

(define oct-cps
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 23 nil))))

(define cps-oct
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 24 nil))))

(define u:log
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 25 nil))))

(define log2
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 26 nil))))

(define log10
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 27 nil))))

(define u:sin
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 28 nil))))

(define u:cos
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 29 nil))))

(define u:tan
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 30 nil))))

(define arc-sin
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 31 nil))))

(define arc-cos
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 32 nil))))

(define arc-tan
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 33 nil))))

(define sin-h
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 34 nil))))

(define cos-h
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 35 nil))))

(define tan-h
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 36 nil))))

(define rand-
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 37 nil))))

(define rand2
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 38 nil))))

(define lin-rand-
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 39 nil))))

(define bi-lin-rand
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 40 nil))))

(define sum3rand
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 41 nil))))

(define distort
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 42 nil))))

(define soft-clip
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 43 nil))))

(define coin
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 44 nil))))

(define digit-value
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 45 nil))))

(define silence
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 46 nil))))

(define thru
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 47 nil))))

(define rect-window
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 48 nil))))

(define han-window
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 49 nil))))

(define welch-window
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 50 nil))))

(define tri-window
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 51 nil))))

(define ramp-
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 52 nil))))

(define s-curve
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 53 nil))))

(define add
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 0 nil))))

(define sub
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 1 nil))))

(define mul
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 2 nil))))

(define i-div
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 3 nil))))

(define f-div
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 4 nil))))

(define u:mod
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 5 nil))))

(define u:eq
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 6 nil))))

(define ne
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 7 nil))))

(define u:lt
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 8 nil))))

(define u:gt
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 9 nil))))

(define le
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 10 nil))))

(define ge
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 11 nil))))

(define u:min
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 12 nil))))

(define u:max
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 13 nil))))

(define bit-and
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 14 nil))))

(define bit-or
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 15 nil))))

(define bit-xor
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 16 nil))))

(define u:lcm
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 17 nil))))

(define u:gcd
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 18 nil))))

(define u:round
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 19 nil))))

(define round-up
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 20 nil))))

(define trunc
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 21 nil))))

(define atan2
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 22 nil))))

(define hypot
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 23 nil))))

(define hypotx
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 24 nil))))

(define pow
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 25 nil))))

(define shift-left
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 26 nil))))

(define shift-right
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 27 nil))))

(define unsigned-shift
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 28 nil))))

(define fill
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 29 nil))))

(define ring1
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 30 nil))))

(define ring2
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 31 nil))))

(define ring3
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 32 nil))))

(define ring4
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 33 nil))))

(define dif-sqr
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 34 nil))))

(define sum-sqr
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 35 nil))))

(define sqr-sum
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 36 nil))))

(define sqr-dif
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 37 nil))))

(define abs-dif
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 38 nil))))

(define thresh
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 39 nil))))

(define am-clip
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 40 nil))))

(define scale-neg
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 41 nil))))

(define clip2
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 42 nil))))

(define excess
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 43 nil))))

(define fold2
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 44 nil))))

(define wrap2
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 45 nil))))

(define first-arg
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 46 nil))))

(define rand-range
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 47 nil))))

(define exp-rand-range
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 48 nil))))
