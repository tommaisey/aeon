# aeon docs
This is an embryonic documentation for an unfinished and as-yet unnamed system that's under heavy development. Useful huh?

A large part of the motivation for writing these docs is to force myself to explain the main concepts to a beginner - I think this will help me to figure out where things make sense and where they need more work.

## overview
`aeon` (that's its codename) is a music live-coding and composition tool that runs on the [Scheme](https://scheme.com/tspl4/) programming language (specifically [Chez Scheme](https://scheme.com)). Its focus is on writing and transforming musical patterns in a way that's intuitive, discoverable and fast.

I'm developing an accompanying editor environment too, because I think that setting up and using progammer-focused tools is overwhelming for musicians who are starting with live-coding. This is developed in a separate (private) repo and may be closed-source to enable some revenue stream.

## main model
`aeon` is all about generating events. Most commonly those events represent notes to be played on a synthesizer or sampler instrument - either one you built yourself or a built-in template. But events can also represent control data for changing existing synth voices or other patterns. Events have properties, which are interpreted by the playback system to generate sound. Each note destined for a synth or sampler, for example, may have any properties supported by the instrument. These might include frequency, envelope information, oscillator shape, effects or output sends, etc. The sky is the limit, and is custom per instrument. Thus you can sequence many more properties than are available in a traditional MIDI-based sequencer. The techniques for specifying patterns are unified (i.e. the same for all events and their properties), flexible and powerful.

`aeon` has an interesting relationship with time. When you specify your patterns you are actually describing an infinite timeline of music. We can ask the system about any part of that timeline, past or future, whenever we want. When you edit your pattern, you have in some sense 'swapped' the old timeline for a new one. This approach has several concrete outcomes:

##### stable
 You can change a pattern at any time, with no quantisation required (though it can be specified if you want), and everything will stay in time. It will be as if the new pattern had always been playing. You never get into inconsistent or out-of-phase states. Of course, that doesn't mean that you can't create chaotic and brain-bending patterns...

 ##### time-travel
 We can manipulate time in interesting ways inside our patterns. We can take a simple sequence and easily play back segments at different speeds or reversed. Polyrhythms and polymeters are easy, as are custom groove templates. Of course we can layer other transformations on top of these temporal ones, and copies of these 'alternate' timelines can be played simultaneously to generate complicated polyphonic patterns.

 ##### anticipation
 We can write patterns that take an input pattern, look into its 'future' and make decisions in the present. For example, we might say 'if the input pattern will have a crash cymbal in 4 bars time, play a crescendo now'.

##### composition
The independence of different patterns means they can be reused in different contexts. This leads to a more 'musical' mode of interaction than some systems. Instead of 'code stuff' you'll be writing 'music stuff', using words like `swing`, `legato`, `chorus-phrase`, `each`, `every` and so-on.

## stop waffling!
Ok, time to learn how to use this thing. The central concept is the `pattern`. When you evaluate a `pattern` it begins playing immediately - `aeon`'s playhead is always running by default. Every `pattern` has a name, meaning that when you re-evaluate it, the playing instance will be swapped for a new instance.

#### blank events
Let's write a simple pattern, giving it the name 'blip':

    (pattern blip
      (in! 1))

After evaluating this, you should hear a repeated sine blip. Change it to the following and evaluate again:

    (pattern blip
      (in! 4))

You hear a faster blip. Try different numbers, and notice how the speed changes, but the pattern is never out-of-time when it changes. It stays consistent with the playhead that's running in the background. You can evaluate the following to stop the pattern, after which re-evaluating the `pattern` expression will start it again.

    (stop blip)

A pattern consists of a single expression. Here the expression is `(in! 4)`. Expressions nearly always follow the format of an operator name (i.e. `in!`), followed by one or more operands (i.e. `4`), with the whole lot enclosed in parentheses `()`. It's also normal to refer to the operator and operand as sub-expressions, because in many cases we will nest expressions inside each other e.g. `(in! (? 2 64))`.

`in!` is an operator that generates blank events. It's short for 'inject' or 'input', and the `!` part of its name is just there to remind you that the events it creates will be blank<sup id="a1">[1](#f1)</sup>.

An event is simply a bundle of named properties. All events (even 'blank' ones) have a `:beat` property describing their location in time, and a `:sustain` property describing their length. Later we will give our events more properties, and these instruct the playback system how to turn them into sound. For example the `:freq` property can describe a note's pitch, the `:amp` property its loudness, and the `:inst` property the instrument it's played on. Actually, events don't _always_ represent notes - sometimes they specify control data for existing notes, for example, but we'll get to that later.

For now, what's important is that if we create events with only `:beat` and `:sustain` properties, the system interprets them as note events playing a simple sine-wave instrument at 440Hz. It does this just to help you start making sounds with less effort.

Turning back to our `(in! 4)` expression, we see that the first operand (which is the second sub-expression) is the number `4`. This operand tells `in!` how often to inject events. The simplest thing recognised here is a number: `1` yeilds one event per musical 'bar' (or 'measure'), `4` yeilds four events per measure, etc. Later we'll look at how to specify more interesting rhythmic patterns, but for now a steady pulse will do.

<b id="f1">1</b> Most of the major operators in `aeon` take the form of two letters followed by a punctuation mark. We've tried to make these as 'mnemonic' as possible to help you remember them. [back](#a1)

#### chains and to:
Our events are boring. To make them more interesting, we want to add more properties to them. But first we need to learn how to chain operations together. Remember that a pattern consists of one expression only. To 'wrap' multiple operations into one expression we need to chain them.

    (pattern blip
      (o->
        (in! 4)
        (to: :inst "pulse-pluck")))

`o->` is a chain operator. There are a couple of these, but this is the most commonly used. In essence, it says 'do these operations one-by-one, feeding the result each time into the next operation'. Here, we inject events with `in!` as before, and then we set a property on them with `to:`.

`to:` doesn't create any new events, it merely affects the events it receives from 'above' (i.e. from previous operations in the chain). Here we set the instrument to `"pulse-pluck"`, an instrument that's defined by default in `aeon`. Now let's try this:

    (pattern blip
      (o->
        (in! 4)
        (to: :inst "pulse-pluck" :freq 330)))

The pitch of the events has changed. We can set multiple properties in a single `to:` expression by writing alternating keys and values. A key is one of those words starting with `:`, like `:inst`, and a value might be a number (e.g. `330`), some text (in quotes `"like this"`) or a pattern (we'll get to those soon). 

When we set multiple properties in a `to:` expression, it's often easier to read if we use line-breaks like this:

    (pattern blip
      (o->
        (in! 4)
        (to: :inst "pulse-pluck" 
             :freq 330
             :cutoff 0.1)))

Chains have other (perhaps more interesting) uses too, which we'll explore later.

#### subdividers
So far we can only make very uninteresting sequences, but things are about to get much more exciting. Two things that could make our music more interesting are to generate events in more complex rhythmic patterns, and also to vary the properties of some events to create more interesting musical progressions. In `aeon`, the mechanism for both of these is the same. 

We can replace most of the numeric values in a pattern with an expression which describes different values spread out over time. These are called subdividers. Let's use a subdivider called `over` as the operand for our `in!` expression:

    (pattern blip
      (in! (over 1/2 [1 1])))

This tells `in!` to distribute two events (that's the `1`s in square brackets) over half a measure. Now try this:

    (pattern blip
      (in! (over 1 [1 1])))

It tells `in!` to distribute the two events over one measure, so the pattern appears half the speed. Now try adding an extra 1 to the list in square brackets:

    (pattern blip
      (in! (over 1 [1 1 1])))

Experiment for a while with different values for the measure such as `3/4`, `2` or `0.45`. Also experiment with different numbers of the `1`s in the items list. The items in the list can themselves be subdivided too:

    (pattern blip
      (in! (over 1/2 [1 [1 1]])))

Now the second part of the pattern is subdivided into two chunks, with an event placed at each. We can add more subdivided items, and nest the subdivisions too:

    (pattern blip
      (in! (over 1/2 [1 [1 1] [1 [1 1]] 1])))

Inside most subdividers, the `~` character represents a rest:

    (pattern blip
      (in! (over 1/2 [1 [~ 1] [~ [1 1]] 1])))

A special rule that is unique to `in!` says that numbers greater than 1 are shortcuts to subdivide by that number:

    (pattern blip
      (in! (over 1/2 [2 3])))

In fact, if you supply a normal value to `in!` or `to:` is it implicitly wrapped in an `over` subdivider, so when you wrote this earlier you were taking advantage of this rule:

    (in! 4) ;; this
    (in! (over 1 [4])) ;; is equivalent to this

So we've seen what happens when we use a subdivision with `in!`. What about `to:`?

    (pattern blip
      (o->
        (in! (over 1/2 [1 [~ 1] [~ [1 1]] 1]))
        (to: :scale-degree (over 2 [I VI III VIII]))))

Here we're using `:scale-degree` rather than `:freq` to select the pitch of our notes. The playback system at the end will convert these degrees into scale values.

Notice that using a subdivider inside `to:` has the effect of setting different values on the incoming events according to which chunk of time they fall into. As before with `in!`, try changing the measure operand from `2` to some other numbers or fractions. And try different numbers of items in the list, to get a feel for how this works.

We can of course give different subdividers to multiple properties in a `to:` expression:

    (pattern blip
      (o->
        (in! (over 1/2 [1 [~ 1] [~ [1 1]] 1]))
        (to: :scale-degree (over 2 [I VI III VIII])
             :octave (over 1/2 [-1 0]))))

#### grouping with chains
We're nearly done with the basics of `aeon`, so that we can start exploring a richer variety of ways to make patterns with random values, time-travel, control data, sample triggering, effects and more. But first we need to solve one more problem. Let's take a pattern similar to the one we just built:

    (pattern blip
      (o->
        (in! (over 1/2 [1 [~ 1] [~ [1 1]] 1]))
        (to: :scale-degree (over 2 [I VI III VIII]))))

Now let's add another stream of events:

    (pattern blip
      (o->
        (in! (over 1/2 [1 [~ 1] [~ [1 1]] 1]))
        (to: :scale-degree (over 2 [I VI III VIII])
        
        (in! (over 1/4 1)))))

## prior art

Here are some similar projects if you want to try out something different or more fleshed-out. 

- [Tidal cycles](https://tidalcycles.org/) is probably the most similar for its focus on pure-functions. Hence it shares some of `aeon`'s properties like stability, time-travel. Like aeon, it uses SuperCollider to generate sound, and has a focus on subdividing patterns. Tidal's syntax looks _very_ different, however.
- [Extempore](https://extemporelang.github.io) is based on Scheme, but focuses on 'temporal recursion' for generating patterns. It also has a low-level sister language for defining DSP, where `aeon` uses a SuperCollider backend.
- [Overtone](http://overtone.github.io) is a Clojure-based interface to SuperCollider. I'm not too sure what its story is on pattern-generation, but it looks fun to use.

