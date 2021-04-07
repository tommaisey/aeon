
[//]: # (When editing this please use a max column length of 70!)

# quick start tutorial
This is a guide to get up-and-running with making music in `aeon`. The
idea here is to get you going so you can experiment yourself,
furnishing just a little of the most important theory on the way.

If you want to learn how things work in a more comprehensive way, you
can check out [the manual](manual.md). Throughout this tutorial, I
will link to sections of the manual so that you can explore more
deeply if you want to.

If you haven't already done so, you should follow the [installation
procedure](../README.md) and get a suitable text-editor environment
set up so that you can evaluate code in a live context.

## contents
1. [starting and stopping patterns](#starting-and-stopping-patterns)
2. [properties and 'to:'](#properties-and-to)
3. [patterns in time](#patterns-in-time)
4. [embedding patterns](#embedding-patterns)
5. [working with samples](#working-with-samples)

## starting and stopping patterns
Let's write a simple pattern, giving it the name `blip`:

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

To start the pattern again, re-evaluate the `pattern` expression.

You can start as many patterns as you want, but they must all have
unique names. You can stop multiple pattern by supplying more names:

```scheme
(stop blip drums hum)
```

Or you can stop all patterns like this:

```scheme
(stop)
```

For the time being let's stick with one pattern, `blip`, to keep
things understandable.

## properties and `to:`
Let's learn how to give our events more properties, so we can make
them sound a little more interesting. Edit your file to look like
this, and re-evaluate:

```scheme
(pattern blip
  (in! 4)
  (to: :inst "pulse-pluck"))
```

Property names start with a colon `:like-this`. As events are
processed, their properties are inspected and used in various ways to
change the sound.  The operator `to:` is used to set these named
properties on events.

Setting the property `:inst` on our events directs the system to play
them on a different instrument. You can set additional properties in a
single `to:` statement:

```scheme
(pattern blip
  (in! 4)
  (to: :inst "pulse-pluck"
       :cutoff 0.5
       :scale-degree V))
```

Event properties can serve different purposes. As we've seen, `:inst`
selects _which_ instrument to play, but the vast majority of
properties control aspects of a particular instrument's sound -
`:cutoff` is an example of this. It's a property of the
`"pulse-pluck"` instrument. Other instruments support `:cutoff` too,
but not all do. It's up to the creator of the instrument. When
instruments have similar features we try to keep the property names
consistent between them.

Some properties, like `:scale-degree`, are a bit
special. `"pulse-pluck"` doesn't recognise this property. Instead,
`aeon` converts `:scale-degree` into another property, `:freq`, which
`"pulse-pluck"` _does_ recognise. During this process it also consults
other properties such as `:scale` and `:tuning`. If they are not
present, defaults representing a minor scale and classic 12-tone
tuning are used.

There is a further category of properties that control the way voices
are grouped together to have effects applied, but we'll look at those
later.

## patterns in time
So far our pattern is _very_ monotonous. Let's speed it up, and add
acsome variety:

```scheme
(pattern blip
  (in! 16)
  (to: :inst "pulse-pluck"
       :cutoff (sine 4 0.4 0.9)
       :scale-degree (over 2 [I VI IV V])))
```

You'll notice that the brightness of the notes changes in a slowly
oscillating pattern. The `"pulse-pluck"` instrument recognises a
property called `:cutoff`, which describes a filter position for each
event.

The `:cutoff` is oscillating between `0.4` and `0.9` over the course
of `4` musical measures or bars. Time is almost always specified in
measures in `aeon`. Similarly, many common parameters like `:cutoff`
are specified as a number between 0 and 1. This isn't universal (some
parameters are more naturally specified in other ranges) but it is
quite common.

For the `:scale-degree` property we've used a repeating pattern.
`over` spreads a looping pattern of values over a certain length
of time, in this case `2` measures. The pattern in this case specifies
degrees in a scale (the minor scale by default, I like moody music).

When a pattern like `over` is used in a `to:` statement, events are
given values according to what time range they fall into. Several
events may get the same value if the events are firing faster than the
`to:` pattern is advancing. In other words, the rhythms of the events
and the properties that are set on them are totally decoupled.

You can also use a pattern to get more interesting rhythms from `in!`:

```scheme
(pattern blip
  (in! (over 1/4 [1 [~ 1]]))
  (to: :inst "pulse-pluck"
       :cutoff (sine 4 0.4 0.9)
       :scale-degree (over 2 [I VI IV V])))
```

Here one of the 'steps' of the `over` pattern is subdivided: `[1 [~
1]]`.  The inner `[]` means that step is subdivided. The `~` symbol
represents a 'rest'. No event will be added at this point in the
pattern. If you use `~` in a `to:` statement, no property will be
applied to that region of time.

[More about subdivision in the manual.](manual.md#subdivision)

## embedding patterns
Patterns can even be embedded inside other patterns.

```scheme
(pattern blip
  (in! (over 1/4 [1 [~ 1]]))
  (to: :inst "pulse-pluck"
       :cutoff (over 2 [(sine 4 0.6 1) 0.5])))
```

For the first of two measures, the cutoff oscillates using a sine
wave, while for the second measure it stays at `0.5`.

Another useful pattern is the random pattern, accessed through `?`.

```scheme
(pattern blip
  (in! (over 1/4 [1 [~ 1]]))
  (to: :inst "pulse-pluck"
       :cutoff (over 2 [(sine 4 0.6 1) 0.5])
       :attack (? 0 0.1)))
```

Here the attack envelope is set to a random value between `0` and
`0.1`. And the random pattern can be embedded in other patterns just
like the `sine` pattern. You can use the same operator to choose from
a list of options too - see [the relevant section](manual.md#random)
of the manual.

## working with samples
Of course, we don't always want to work with synthetic sounds,
sometimes we want to use recorded samples. 

The basics of doing this are simple, but a little more verbose than
we'd like, so we'll look at a shortcut syntax afterwards.

First, that basic idea:

```scheme
(pattern kick
  (in! (over 
```
