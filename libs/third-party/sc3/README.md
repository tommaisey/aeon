README: rsc3
-----------------------
This folder contains the rsc3 library for interacting with SuperCollider, and some related dependencies.

Running `make` should build the libraries. You should add this folder to your `(library-directories)`
and after that a simple `(import (rsc3))` should work.

If you need to change the build, here's some more info:

The `rsc3`, `rhs` and `sosc` projects are wrapped into consumable libraries by the `mk-r6rs` script. This
is invoked in each project by `make`. The script essentially concatenates all the files and wraps them into
an r6rs library, adding exports for all the `define` forms it finds in the project.

I have made some minimal changes to the `mk.scm` file for each project to get the libraries working in 
Chez Scheme.
