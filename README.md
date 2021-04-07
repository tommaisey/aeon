# aeon [alpha]

[//]: # (When editing this please use a max column length of 80!)

A music live-coding suite. Still under development, and not ready for prime-time
yet!

## Overview

`aeon` is a live-coding music system. That means that you can improvise live
electronic music by typing into a text editor or terminal. Its primary purpose
is to create sequences of musical events that can be modified on the fly.

This is far from a new idea, as the [list of alternatives](#alternatives)
suggests.  But `aeon` tries to bring some new ideas to the table, or at least to
remix existing ones in novel ways. Some of these are listed in the ['features'
document](docs/features-overview.md). If you want more comprehensive
documentation, try the [tutorial](docs/tutorial.md) or
[manual](#docs/manual.md).

The most important thing: you don't need to know programming to use `aeon`. At
least, that is my goal. Instead you'll be using a very simplified
'domain-specific language' to define patterns. And later you may accidentally
learn some programming if you aren't careful `;)`

## Installation

`aeon` currently supports macOS and Linux only. If you're on Linux, you already
have (and probably use) a package manager. If you're on macOS, I recommend you
install [Homebrew](https://brew.sh/).

```bash
# Install dependencies:
$ brew install chezscheme supercollider # macOS
$ sudo apt install chezscheme supercollider # Ubuntu/Debian linux

# Install
$ git clone git@github.com:tommaisey/aeon.git
$ cd aeon
$ chez aeon.scm
```

Note that some package managers name the Chez Scheme binary `scheme`, so the
final command might be `scheme aeon.scm`.

You should see a Chez Scheme welcome message followed by some messages about
files being compiled. This will take a short while, but should only happen the
first time you launch `aeon`.

If everything went well, you should see the following message:

```
-----------------------------------------------
[aeon] musical patterns
-----------------------------------------------
```

Now you can check that your sound is working by typing this and pressing enter:

```scheme
(pattern hello (in! (over [4 2 1 2])))
```

To stop the sound, type: `(stop hello)`. To exit `aeon` entirely you can hit
`Ctrl-D` or type `(exit)`.

If you want to learn how to get started constructing patterns, sounds and
projects, head to the ['tutorial' document](docs/tutorial.md).

### Using a text editor

The REPL (the prompt you are now in) is a fun way to explore and test ideas
while learning. But as you build more complex patterns you will want to store
and edit them in text files.

The simplest approach is to write your code in any editor you like, then launch
`scheme aeon.scm` and run `(load "/path/to/my/file.scm")` to evaluate it. You
can resave your text file and rerun that command to change the pattern as it
plays.

However, for a more seamless experience you will want two things:

1. Send code to the REPL directly from your editor.
2. Structural editing and navigation.

'Structural editing' is a fancy way of saying that your editor can navigate and
edit code quickly using the ubiquitous parentheses `()` of scheme. This turns
those annoying and fiddly parentheses into an advantage, streamlining your
experience enormously. But it does take a little practice.

My plan is to create a simple plugin for [VS
Code](https://code.visualstudio.com/) that offers these features in the simplest
way possible.

Until then, your choices are:

- [VS Code](https://code.visualstudio.com/) with
  [paredit](https://marketplace.visualstudio.com/items?itemName=clptn.code-paredit)
  and a simple
  [repl](https://marketplace.visualstudio.com/items?itemName=nvbn.sendtorepl)
  plugin
- [Emacs](https://www.gnu.org/software/emacs/) with
  [paredit](https://github.com/emacsmirror/paredit) (built in) and the
  [geiser](https://nongnu.org/geiser/) package.

If you're not experienced with Emacs, note that it can take a long time to
become proficient and to customize it to your tastes. However, the end result is
powerful and satisfying. If you just want to make music quickly, you should use
VS Code, which is more friendly to newcomers.

### Troubleshooting

If you don't hear any sound it's likely that SuperCollider isn't connecting to
your audio outputs correctly. The log output above may give some clues, or if
not, it's worth hitting `Ctrl-D` to get out of scheme, and running `scsynth`
directly, to see if it can tell you more:

```bash
# macOS:
$ /Applications/SuperCollider.app/Contents/Resources/scsynth -u 57100
```

## Alternatives

There are many other live-coding systems that you can try, each with different
approaches and design trade-offs. Here are some of my favourites:

- [SuperCollider](https://supercollider.github.io/) [procedural]
  [object-oriented]
- [Tidal Cycles](https://tidalcycles.org/) [functional] [haskell]
- [Orca](https://hundredrabbits.itch.io/orca) [esoteric] [stateful]
- [Extempore](https://github.com/digego/extempore) [scheme]
- [Overtone](https://overtone.github.io/) [clojure]
- [FoxDot](https://foxdot.org/) [python] [simple]
