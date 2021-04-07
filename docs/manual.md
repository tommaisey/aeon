
[//]: # (When editing this please use a max column length of 70!)

# aeon manual
This is embryonic documentation for a system that's under heavy
development.  Things may change!!

A large part of the motivation for writing these docs is to force
myself to explain the main concepts to a beginner - I think this will
help me to figure out where things make sense and where they need more
work.

## contents
1. [overview](#overview)
   1. [note on examples](#note-on-examples)
2. [general concepts](#general-concepts)
3. [patterns](#patterns)
   1. [pattern definitions](#pattern-definitions)
      1. [quantise changes](#quantise-changes)
      2. [play length](#play-length)
   2. [pattern operators](#pattern-operators)
      1. ['in' family](#in-family)
      2. ['to' family](#to-family)
      3. ['tt' family](#tt-family)
   3. [subdividing sequences](#subdividing-sequences)
      1. [over](#over)
      2. [step](#step)
      3. [subdivision](#subdivision)
      4. [rests, ties and repeats](#rests-ties-and-repeats)
   4. [continuous sequences](#continouous-sequences)
      1. [random](#random)
      2. [sine](#sine)
      3. [every](#every)
   5. [algorithms](#algorithms)
      1. [euclidean](#euclidean)
      2. [bits](#bits)
   6. [temporal operators](#temporal-operators)
      1. [swing](#swing)
      2. [legato](#legato)
      3. [taps](#taps)
   7. [grouping with chains](#grouping-with-chains)
      1. [part o->](#part)
      2. [with x->](#with)
      3. [copy +->](#copy)
4. [drones and effects](#drones-and-effects)
   1. [drones](#drones)
   2. [effect groups](#effect-groups)
   3. [control patterns](#control-patterns)
5. [instruments and sound design](#instruments-and-sound-design)
   1. [sources](#sources)
   2. [effects](#effects)
   3. [send effects](#send-effects)
6. [projects and saves](#projects-and-saves)
   1. [creating a project](#creating-a-project)
   2. [version-control](#version-control)
7. [recording loops](#recording-loops)
8. [appendices](#appendices)
   1. [note on square brackets](#note-on-square-brackets)

## overview
`aeon` is a music live-coding and composition tool that runs on the
[Scheme](https://scheme.com/tspl4/) programming language (specifically
[Chez Scheme](https://scheme.com)). Its focus is on writing and
transforming musical patterns in a way that's intuitive, discoverable
and fast.

This document lists all of the built-in forms that `aeon` provides. It
is structured as a general reference, not a step-by-step guide to
learning `aeon`.

If you are just starting out, you may prefer the
[tutorial](tutorial.md), or perhaps the [recipes](recipes/), which
present a more hands-on and musically inspiring introduction.

## general concepts
### units of time
Time values in `aeon` are always relative to a 'measure'. This
basically a musical bar. We usually write time values as fractions, so
to express a sixteenth note you write `1/16`, or `1/3` for a triplet.
You can use any fraction, such as `5/7`, if you wish.

The relationship of measures to seconds is determined by the
playhead's bpm (beats per minute), as this is familiar to most people.
The bpm determines how many `1/4` measures elapse per minute.

Changing the bpm is done like this: `(set-bpm! 100)`.

## patterns
At the heart of `aeon` is a collection of tools for generating events
in time. Often events represent notes to be played on a synthesizer or
sampler instrument (either one you built yourself or a built-in
template). But events can also represent control data for changing
existing synth voices or effects.

Events have 'properties', which are interpreted by the playback system
when it generates sound. Each note destined for a synth or sampler,
for example, may have any properties supported by that particular
instrument. These could include frequency, envelope information,
oscillator shape, effects or output sends, etc. The sky is the limit,
and is customised per instrument (though many properties are shared
between instruments). Thus you can sequence many more properties than
are available in a traditional MIDI-based sequencer.

The techniques for specifying when events happen and what properties
they have are unified, flexible and powerful.

### pattern definitions
A pattern definition tells the `aeon` system about a pattern you want
to play. It has a name, so that it can be updated in the future, and a
series of operator expressions that describe the pattern:

``` scheme
(pattern name
  (operator) ...)
```

Evaluating this form will begin playing the pattern instantly.
Re-evaluating it will apply any changes to the pattern instantly.
The pattern will continue playing until you stop it.


``` scheme
;; stop one or more patterns
(stop pattern-names ...)

;; stop all patterns
(stop)
```

#### quantise changes
[**note**: unimplemented]

If you don't want changes to happen instantly, you can quantise
changes to a pattern to a certain number of measures like this:

``` scheme
(pattern (name quantised-measures)
  (operator) ...)
  
;; play quarter notes
;; start/change on a 2 measure boundary
(pattern (x 2) (in! 4))
```

#### play length
[**note**: unimplemented]

Adding a third value into the first form of a pattern directs the
system to play it for only a limited of measures. This can be useful
for 'one-shot' effects or transitional sounds during a performance.

``` scheme
(pattern (name quantised-measures length-limit-measures)
  (operator) ...)
  
;; play sixteenth notes
;; start/change on a 2 measure boundary
;; only play for 1 measure
(pattern (x 2 1) (in! 16))
```

### pattern operators
Operators are constructs that dictate how a sequence (or set of
sequences) will be used to generate or modify events. There are
different families of operators for generating and modifying events,
but all of these can use the same pattern constructs to do their work.

#### `in` family
This family of operators is used for adding new events to a stream.
The events can may be created 'blank' to add properties later, or with
some properties already baked-in.

##### `in!`
Creates blank events that will be given the instrument
`"sine-grain"` and a `:freq` of 440 by default. 

Here is the general form of `in!`. It takes a pattern sequence,
followed by zero or more further operations:

``` scheme
(in! sequence ops ...)
```

If further operations are supplied, it's as if the `in!` expression
were wrapped in a [part chain](#part), so that those operations _only_
get applied to the new events generated by `in!`, and not to existing
events in the stream.

``` scheme
;; above is equivalent to:
(part (in! sequence) ops ...)
```

Here are some further illustrative examples:


``` scheme
;; four events per measure:
(in! 4)

;; equivalent:
(in! (over 1 [4]))

;; a more complex subdivided rhythm:
(in! (over 1 [2 [~ 1] 1 $]))
  
;; two event streams, with separate further operations:
(part
  (in! 2
       (to: :scd I)
       (tt* 1/3))
  (in! 8
       (to: :scd V)))
```

If the sequence provided to `in!` is not a subdivider, it is wrapped
in the [over](#over) subdivider, with a length of 1 measure. Thus `8`
is equivalent to `(over [8])`.

Values other than `1` in a subdivider further subdivide the step that
they reside in. Examples:

``` scheme
;; These are equivalent:
(in! 4)
(in! (over [4]))
(in! (over [1 1 1 1]))

;; As are these:
(in! (over [2 1]))
(in! (over [[1 1] 1]))
```

##### `in:`
Behaves very much like `in!` except the values returned from its
pattern sequence are used to set a property. Here is its general form:

``` scheme
(in: :property sequence
     ops ...)

;; equivalent to:
(part (in: :property sequence)
      ops ...)
```

Some examples. These must be wrapped in `(pattern name ...)` if you
want to hear them.

``` scheme
;; 4 events per measure, with different scale degrees
(in: :scd (over [I IV VI V]))

;; 2 events per half measure, on different instruments
(in: :inst (over 1/2 ["pulse-pluck" "fm-grain"]))

;; 2 events per half measure, on different instruments
(in: :inst (over 1/2 ["pulse-pluck" "fm-grain"])
     (to: :scd (over 4 [I VI VIII IV])))
```

### subdividing sequences
Sequences that produce values at different time steps, in a looping
fashion. Each of the time steps can be further subdivided into steps
that produce values, recursively.

#### `over`
Produces a number of values over a given length of time. Each value
occupies a time length of `total-time / number-of-values`. The
sequence loops forever.

``` scheme
;; general form:
(over total-time [values-list ...])

                     ; values endure for (measures):
(over 1 [1])         ;=> [1]
(over 1 [2 3])       ;=> [1/2 1/2]
(over 1/2 [4 5])     ;=> [1/4 1/4]
(over 1/2 [1 2 3])   ;=> [1/6 1/6 1/6]
(over 1/2 [1 [2 3]]) ;=> [1/4 [1/8 1/8]]
```

#### `step`
Produces a number of values over time. Each value occupies the same
length of time. The total length of the sequence is therefore
`step-time * number-of-steps`. The sequence loops forever.

``` scheme
;; general form:
(step step-time [values-list ...])

                     ;  values endure for (measures):
(step 1 [1])         ;=> [1]
(step 1 [2 3])       ;=> [1 1]
(step 1/2 [4 5])     ;=> [1/2 1/2]
(step 1/4 [1 2 3])   ;=> [1/4 1/4 1/4]
(step 1/2 [1 [2 3]]) ;=> [1/2 [1/4 1/4]]
```

#### subdivision
Within the subdividing sequences mentioned above (i.e. `over` and
`step`) you can create more intricate patterns by subdividing steps
into smaller pieces. This is done by nesting the values list like
this:

``` scheme
[1 2 [3 4]]
```

In this sequence we have three equally sized steps, the last of which
is subdivided into two equally sized steps. How this translates into
values spread across time depends on the context this values list is
found in. For example:

``` scheme
(over 1   [1 2 [3 4]]) ; => [1/3 1/3 [1/6 1/6]]
(step 1/4 [1 2 [3 4]]) ; => [1/4 1/4 [1/8 1/8]]
```

Of course, we can further sibdivide steps by nesting further:

``` scheme
(over 1   [1 2 [3 [4 5]]]) ; => [1/3 1/3 [1/6 [1/12 1/12]]]
(step 1/4 [1 2 [3 [4 5]]]) ; => [1/4 1/4 [1/8 [1/16 1/16]]]
```

#### rests, ties and repeats

#### embedding

### continuous sequences
Sequences that can provide continuously varying values for any point
in time, as opposed to the 'stepped' sequences of a subdivider.

A note: continuous sequences can be used as the sequence for an
`in:` expression, but without further specification, the system won't
know how often to generate events. By default it will generate them
once per measure. To choose other frequencies, wrap it in an `over`
expression:

``` scheme
;; 1 event per measure, frequency chosen by sine:
(in: :freq (sine 4 100 1000))

;; 8 events per measure:
(in: :freq (over 1/8 (sine 4 100 1000)))
```

## appendices
The following notes relate mostly to the underlying syntax of Scheme,
or other deeper topics of implementation. If you're only interested
in making music, feel free to skip them!

### note on square brackets `[]`
In `aeon` we traditionally put the 'values list' of sequences inside
square brackets. Examples:

``` scheme
(over [2 4 6])
(? [2 4 6])
```

But what do these square brackets really mean?

In Chez Scheme, `aeon`'s host language, square brackets are exactly
the same as round brackets `()`. They are interchangable. However, we
use them to indicate that 'something special is going on here'. Let's
try typing into a Chez Scheme REPL:

``` scheme
(2 4 6) ;=> Exception: attempt to apply non-procedure 2
[2 4 6] ;=> Exception: attempt to apply non-procedure 2
```

This happens because these are both lists, and when you input a list
into Scheme it is treated as code by default. Scheme expects the first
element to be a function name, and further elements to be the
function's arguments. If you prepend a quote character, Scheme will
stop trying to interpret the list as code, and will happily return
a list:

``` scheme
'(2 4 6) ;=> (2 4 6)
'[2 4 6] ;=> (2 4 6)
```

Or, you could use the `list` function, which constructs a list from
its arguments:

``` scheme
(list 2 4 (list 6 8)) ;=> (2 4 (6 8))
[list 2 4 [list 6 8]] ;=> (2 4 (6 8))
```

Note that Scheme can output a raw list like `(1 2 3)` - it's only on
the input where this is disallowed, because that's where the
evaluation of code happens. Note also that the square brackets, once
quoted, produce exactly the same result as the round.

So if this is the case, how come we're allowed to write things like
this in `aeon` _without_ quoting the inner lists?

``` scheme
(over [2 4 [6 8]])
(? [2 4 [6 8]])
```

The reason is that `over` and `?` are macros. These are constructs
that let us temporarily bend or break the rules of Scheme within the
scope of their parentheses. To make writing `aeon` patterns easier we
use the macro [pdef](#../libs/pdef.scm) to acheive a similar effect to
quoting (or quasiquoting) the list. 

`pdef` contains some special magic to discern function calls withing
lists so that the following works, even though to Scheme the `[4 6]`
form isn't much different to the `(+ 2 2)`.

``` scheme
(over [2 [4 6] (+ 2 2)])
```

So in short, we use square brackets to say: 'normal rules of
evaluation don't apply here!'.

