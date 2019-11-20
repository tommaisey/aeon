# convex

This is a functional music live-coding suite. It's heavily under development, and currently undocumented. 
In essence, this is a placeholder! 

I was asked to share something after a talk at ADC 2019 for those interested in the project. 
I didn't expect the level of interest, so this is a quick way to give the project a home.

If you want to attempt to run it, good luck! Because it's currently set up to work on my own machine only (it contains paths to samples on my local machine, for example).
If you want to proceed anyway, you'll need to clone this repo (resolving any git submodules) and download Chez Scheme 9.4.

I recommend interacting with your scheme interpreter through Emacs and Geiser.
Navigate to the project directory, launch Chez and call `(load "init/init.scm")`. 
This will fail to load! Once you've fixed the errors you find, you _may_ be up and running.
Try looking at the examples in `music/`, and reading implementation code in `libs/` to guess what things might do!

### License

At present, the code is copyright 2019 Tom Maisey.
You may download, run and modify it, but you may not redistribute it or modified copies of it.

When development is further progressed, the license will likely change to something more permissive.
