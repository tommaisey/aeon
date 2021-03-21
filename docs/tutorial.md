# aeon docs
This is embryonic documentation for a system that's under heavy
development.  Things may change!!

A large part of the motivation for writing these docs is to force
myself to explain the main concepts to a beginner - I think this will
help me to figure out where things make sense and where they need more
work.

## contents
1. [overview](#overview)
    1. [mental model](#mental-model)
2. [basic patterns](#basic-patterns)
    1. [blank events](#blank-events)
    2. [chains and to:](#chains-and-to)
    3. [subdividers](#subdividers)
    4. [rests, ties and repeats](#rests-ties-and-repeats)
    4. [grouping with chains](#grouping-with-chains)
3. [further with patterns](#further-with-patterns)
    1. [dynamic patterns](#dynamic-patterns)
    2. [time travel patterns](#time-travel-patterns)
    3. [control patterns](#control-patterns)
    4. [effects patterns](#effects-chains)
4. [sound design](#sound-design)
    1. [building synths](#building-synths)
    2. [buiding effects](#building-effects)
    3. [send effects](#send-effects)
5. [projects](#projects)
6. [version control](#version-control)
7. [recording loops](#recording-loops)

## overview
`aeon` is a music live-coding and composition tool that runs on the
[Scheme](https://scheme.com/tspl4/) programming language (specifically
[Chez Scheme](https://scheme.com)). Its focus is on writing and
transforming musical patterns in a way that's intuitive, discoverable
and fast.

### mental model
`aeon` is all about generating events. Most commonly those events
represent notes to be played on a synthesizer or sampler instrument -
either one you built yourself or a built-in template. But events can
also represent control data for changing existing synth voices or
other patterns. Events have 'properties', which are interpreted by the
playback system to generate sound. Each note destined for a synth or
sampler, for example, may have any properties supported by the
instrument. These might include frequency, envelope information,
oscillator shape, effects or output sends, etc. The sky is the limit,
and is customised per instrument. Thus you can sequence many more
properties than are available in a traditional MIDI-based
sequencer. The techniques for specifying patterns are unified
(i.e. the same for all events and their properties), flexible and
powerful.

The following tutorial takes you through using `aeon`'s pattern
system and other features. It's presented in rather a 'slow' way,
so feel free to experiment at any point, for example by taking
the examples and modifying them to see what happens. I feel it's
important to have a slow step-by-step guide, however, that you
can refer to if you need to understand the concepts more deeply.

## basic patterns
### blank events
Let's write a simple pattern, giving it the name blip:

```scheme
(pattern blip
  (in! 1))
```

After evaluating this, you should hear a repeated sine blip. Change it
to the following and evaluate again:

```scheme
(pattern blip
  (in! 4))
```

You hear a faster blip. Try different numbers, and notice how the
speed changes, but the pattern is never out-of-time when it
changes. It stays consistent with the playhead that's running in the
background. You can evaluate the following to stop the pattern, after
which re-evaluating the `pattern` expression will start it again.

```scheme
(stop blip)
```

A pattern consists of a single expression. Here the expression is
`(in! 4)`. Expressions nearly always follow the format of an operator
name (i.e. `in!`), followed by one or more operands (i.e. `4`), with
the whole lot enclosed in parentheses `()`. It's also normal to refer
to the operator and operand as sub-expressions, because in many cases
we will nest expressions inside each other e.g. `(in! (? 2 64))`.

`in!` is an operator that generates blank events. It's short for
'inject' or 'input', and the `!` part of its name is just there to
remind you that the events it creates will be blank<sup
id="a1">[1](#f1)</sup>.

An event is simply a bundle of named properties. All events (even
'blank' ones) have a `:beat` property describing their location in
time, and a `:sustain` property describing their length. Later we will
give our events more properties, and these instruct the playback
system how to turn them into sound. For example the `:freq` property
can describe a note's pitch, the `:amp` property its loudness, and the
`:inst` property the instrument it's played on. Actually, events don't
_always_ represent notes - sometimes they specify control data for
existing notes, for example, but we'll get to that later.

For now, what's important is that if we create events with only
`:beat` and `:sustain` properties, the system interprets them as note
events playing a simple sine-wave instrument at 440Hz. It does this
just to help you start making sounds with less effort.

Turning back to our `(in! 4)` expression, we see that the first
operand (which is the second sub-expression) is the number `4`. This
operand tells `in!` how often to inject events. The simplest thing
recognised here is a number: `1` yeilds one event per musical 'bar'
(or 'measure'), `4` yeilds four events per measure, etc. Later we'll
look at how to specify more interesting rhythmic patterns, but for now
a steady pulse will do.

<b id="f1">[1](#a1)</b>. Most of the major operators in `aeon` take
the form of two letters followed by a punctuation mark. We've tried to
make these as 'mnemonic' as possible to help you remember them.

### chains and to:
Our events are boring. To make them more interesting, we want to add
more properties to them. But first we need to learn how to chain
operations together. Remember that a pattern consists of one
expression only. To 'wrap' multiple operations into one expression we
need to chain them.

```scheme
(pattern blip
  (o->
    (in! 4) 
    (to: :inst "pulse-pluck")))
```

`o->` is a chain operator. There are a couple of these, but this is
the most commonly used. In essence, it says 'do these operations
one-by-one, feeding the result each time into the next
operation'. Here, we inject events with `in!` as before, and then we
set a property on them with `to:`.

`to:` doesn't create any new events, it merely affects the events it
receives from 'above' (i.e. from previous operations in the
chain). Here we set the instrument to `"pulse-pluck"`, an instrument
that's defined by default in `aeon`. Now let's try this:

```scheme
(pattern blip
  (o->
    (in! 4) 
    (to: :inst "pulse-pluck" :freq 330)))
```

The pitch of the events has changed. We can set multiple properties in
a single `to:` expression by writing alternating keys and values. A
key is one of those words starting with `:`, like `:inst`, and a value
might be a number (e.g. `330`), some text (in quotes `"like this"`) or
a pattern (we'll get to those soon).

When we set multiple properties in a `to:` expression, it's often
easier to read if we use line-breaks like this:

```scheme
(pattern blip
  (o->
    (in! 4) 
    (to: :inst "pulse-pluck"
         :freq 330 :cutoff 0.1)))
```

Chains have other (perhaps more interesting) uses too, which we'll
explore later.

### subdividers
So far we can only make very uninteresting sequences, but things are
about to get much more exciting. Two things that could make our music
more interesting are to generate events in more complex rhythmic
patterns, and also to vary the properties of some events to create
more interesting musical progressions. In `aeon`, the mechanism for
both of these is the same.

We can replace most of the numeric values in a pattern with an
expression which describes different values spread out over
time. These are called subdividers. Let's use a subdivider called
`over` as the operand for our `in!` expression:

```scheme
(pattern blip
  (in! (over 1/2 [1 1])))
```

This tells `in!` to distribute two events (that's the `1`s in square
brackets) over half a measure. Now try this:

```scheme
(pattern blip
  (in! (over 1 [1 1])))
```

It tells `in!` to distribute the two events over one measure, so the
pattern appears half the speed. Now try adding an extra 1 to the list
in square brackets:

```scheme
(pattern blip
  (in! (over 1 [1 1 1])))
```

Experiment for a while with different values for the measure such as
`3/4`, `2` or `0.45`. Also experiment with different numbers of the
`1`s in the items list. The items in the list can themselves be
subdivided too:

```scheme
(pattern blip
  (in! (over 1/2 [1 [1 1]])))
```

Now the second part of the pattern is subdivided into two chunks, with
an event placed at each. We can add more subdivided items, and nest
the subdivisions too:

```scheme
(pattern blip
  (in! (over 1/2 [1 [1 1] [1 [1 1]] 1])))
```

Inside most subdividers, the `~` character represents a rest:

```scheme
(pattern blip
  (in! (over 1/2 [1 [~ 1] [~ [1 1]] 1])))
```

A special rule that is unique to `in!` says that numbers greater than
1 are shortcuts to subdivide by that number:

```scheme
(pattern blip
  (in! (over 1/2 [2 3])))
```

In fact, if you supply a normal value to `in!` or `to:` is it
implicitly wrapped in an `over` subdivider, so when you wrote this
earlier you were taking advantage of this rule:

```scheme
(in! 4) ;; what we wrote earlier
(in! (over 1 [4])) ;; exactly equivalent
```

So we've seen what happens when we use a subdivision with `in!`. What
about `to:`?

```scheme
(pattern blip
  (o->
    (in! (over 1/2 [1 [~ 1] [~ [1 1]] 1])) 
    (to: :scale-degree (over 2 [I VI III VIII]))))
```

Here we're using `:scale-degree` rather than `:freq` to select the
pitch of our notes. The playback system at the end will convert these
degrees into frequency values.

Notice that using a subdivider inside `to:` has the effect of setting
different values on the incoming events according to which chunk of
time they fall into. As before with `in!`, try changing the measure
operand from `2` to some other numbers or fractions. And try different
numbers of items in the list, to get a feel for how this works.

We can of course give different subdividers to multiple properties in
a `to:` expression:

```scheme
(pattern blip
  (o->
    (in! (over 1/2 [1 [~ 1] [~ [1 1]] 1])) 
    (to: :scale-degree (over 2 [I VI III VIII])
         :octave (over 1/2 [-1 0]))))
```

### rests, ties and repeats
*TODO*

### grouping with chains
We're nearly done with the basics of `aeon`, so that we can start
exploring a richer variety of ways to make patterns with random
values, time-travel, control data, sample triggering, effects and
more. But first we need to solve one more problem. 

Let's take a pattern similar to the one we just built. From now on,
instead of `scale-degree` I'm going to write `:scd`, which is just
a shorter alias for the same thing.

```scheme
(pattern blip
  (o->
    (in! (over 1/2 [1 [~ 1] [~ [1 1]] 1])) 
    (to: :scale-degree (over 2 [I VI III VIII]))))
```

Now let's add another stream of events and try to set the `:scd`
property on these, the `:scale-degree`. We'd probably start with
something like this:

```scheme
(pattern blip
  (o->
    (in! (over 1/2 [1 [~ 1] [~ [1 1]] 1])) 
    (to: :scd (over 2 [I VI III VIII]))
    
    (in! (over 1/4 1))
    (to: :scd (over 1 [IV VII]))))
```

But if you evaluate that, you'll hear a problem. Our original notes
have aquired the same pitch as our new notes! This is because the
`to:` operator changes _all_ the events that pass through it. We need
a way to separate out the events we want to change. Luckily, we have
seen the solution before, with `o->`:

```scheme
(pattern blip
  (o->
    (in! (over 1/2 [1 [~ 1] [~ [1 1]] 1])) 
    (to: :scd (over 2 [I VI III VIII]))
    
    (o->
      (in! (over 1/4 1))
      (to: :scd (over 1 [IV IX])))))
```

We already know that `o->` executes a series of operations, feeding
the result from each into the next. The new thing we just learned is
that is does this _without_ feeding in any events from its surrounding
context - it is 'isolated'. The result of the `o->` block is then
inserted into its parent chain.

This allows you to build up independent blocks of patterns that are
then merged with other patterns. In fact, this use of `o->` is so
common that there's a shortcut:

```scheme
(pattern blip
  (o->
    (in! (over 1/2 [1 [~ 1] [~ [1 1]] 1])
         (to: :scd (over 2 [I VI III VIII])))
    
    (in! (over 1/8 1)
         (to: :scd (over 1 [IV IX])))))
```

`in!` can take additional blocks following its first 'subdivision'
statement. The blocks act as if they were wrapped in `o->` along with
the `in!` expression. The above code is exactly equivalent to the code
that precedes it.

For future reference, you'll find that `in!`'s relative `in:` has the
same ability.

There are two other chain operators, `x->` and `+->`, which I'll 
describe briefly, but which won't become really useful until later
in your journey.

`x->` groups operations together but _does_ feed the events from its
surrounding context to its child operations. If you replace `o->` with
`x->` in the 'previous plus one' example, you will find that we're
back to our original 'problem'. What use is that? It lets us define
transformations that are shared between different patterns:

```scheme
(define (quiet)
  (x-> (to: :amp 0.1)))
  
(pattern one
  (in! 4 (quiet)))
(pattern two
  (in! 12 (quiet)))
```

If I had many patterns that shared some transformation, this could be
useful, because I could change the transformation in one place, and
all the patterns would be updated.

`+->` creates multiple copies of the events it receives, transforming
each set of copies separately. This is useful for making chords,
rounds, canons, and many other musical structures. We'll look at this
chain operator later, in the [time-travel](#time-travel-patterns)
section.

## further with patterns

OK, now the basic structure of patterns that create events and set
properties has been explained. But our patterns are still pretty
static things that loop in the same way forever. It's time to learn
how to add additional variation with random choices, the manipulation
of time, and the addition of effects to spice up our rather 'dry'
patterns.

### dynamic patterns

In order to give our patterns more subtle variations we might want to
change some properties over time in different ways. To begin, let's
change the instrument our pattern is played on - a sine wave doesn't
offer much opportunity for hearing dynamic changes in sound.

```scheme
(pattern blip
  (in! (over 1 [1 [~ 1] [~ [1 1]] 1])
       (to: :scd (over 2 [I VI III VIII])
            :inst "pulse-pluck"
            :octave -1
            :cutoff (sine 4 0.2 0.8))))
```

We set a property `:inst` to `"pulse-pluck"`, which is the name of a
simple instrument that's built into `aeon`. Note that like any other
property, this could be a subdividing pattern, so you can send
different events in the stream to different instruments. We also
set `:octave` to `-1`, which lowers the pitch by an octave.

But the change we're mostly interested in here is the value of
`:cutoff`. 

### time-travel patterns
### control patterns
### effects patterns

## sound design
*TODO*

### building synths
### building effects
### send effects

## projects
*TODO*

## version control
*TODO*

## recording loops
*TODO*
