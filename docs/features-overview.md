# Features overview
In which we enumerate some nifty features (or soon-to-be features) of `aeon`.

This document is basically a sales pitch. There are quite a few live-coding music
systems: why choose this one? Well, you probably shouldn't yet, because it's still
being developed! But good progress is being made and many of these features can be
used today:

## Easy install

A simple installation process is a priority for this project. I'm hoping to target
musicians as well as computer-people, so an error-prone installation process full
of magical incantations is out.

However, you will be expected to type some simple commands into a terminal, so
a little experience/practice with that is recommended.

## Time-travel

Music is a temporal art, so control over time naturally yields many useful and
powerful capabilities.

The engine that powers `aeon` is perfectly time-stable, meaning that you can change a
pattern at any moment, and the music keeps playing in time. You never get into
inconsistent or out-of-phase states. Of course, that doesn't mean that you can't create
chaotic and brain-bending patterns...

Patterns can reference each other, and when they do, they can 'see' the past and
future. That is, they can find out what another pattern will produce (or has produced)
and copy, warp or react to what they see. This can be used for many purposes, from the
subtle to the extreme. Examples include groove adjustments, canons, reverse delays,
complex polyrhythms etc. These temporal transformations can be applied or removed
instantly, as with any other transformation.

This capability is a bit mind-bending when you first encounter it, because so few
music systems have it ([Tidal Cycles](https://tidalcycles.org/) is an exception). 
However, give it time, and it may change the way you write music.

##### Composable

The independence of different patterns means they can be reused in different
contexts. This leads to a more 'musical' mode of interaction than some systems. Instead of
'code stuff' you'll be writing 'music stuff', using words like `swing`, `legato`,
`chorus-phrase`, `each`, `every` and so-on.

## Seamless sound design

`aeon` uses [SuperCollider](https://supercollider.github.io/) as its sound engine, 
telling it when to trigger sounds. Handily, you can specify new sound generators 
and effects from within `aeon` itself - or more properly, within `scheme`, the 
language  `aeon` is written in. 

You have access to the full range of SuperCollider's [unit
generators](http://doc.sccode.org/Browse.html#UGens%3EGenerators%3EDeterministic), which
can be arranged to produce synthesizers and effects
([SynthDefs](http://doc.sccode.org/Tutorials/Getting-Started/10-SynthDefs-and-Synths.html),
in SuperCollider terminology).

`aeon` also comes with quite a few pre-written SynthDefs, so there's no need to 
write your own if you don't want to.

## Loop export

If you're primarily a DAW user you might still find `aeon` useful for making loops
that would be difficult to create in your DAW environment. With this in mind,
`aeon` includes a simple recording facility that can record your current output for
a given number of musical measures. The recording has WAV format, and should have 
precisely the right length, meaning it can be dragged directly into your DAW and 
used instantly.

## Version control

`[not yet complete]`

When you're working on a set of patterns, you can take a snapshot of your entire project
state, and later roll back to this snaphot at any time. You can take as many snapshots
as you want, and roll backwards or forwards during a performance without losing musical 
time. You can do this without leaving the `aeon` environment.

You can even create alternative branches of snapshot history, and jump between them. 
This should allow you to evolve your music in different directions over the course 
of a performance, and also to ensure that you never lose any serendipitous musical magic.

Rollbacks are implemented by providing a highly simplified set of commands that
call out to `git`. You can therefore share the project's history by posting it to
github or similar.

