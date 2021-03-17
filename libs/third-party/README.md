README: Aeon Dependencies
-----------------------
This directory contains third party scheme code used in `aeon`'s pattern engine. 
The code is copied into this repo (rather than in submodules) for a few reasons:

- Reduces the `git`-foo required to install `aeon`, which is targeted at non-developers.
- Some of the libraries (e.g. `rsc3`) use different version-control systems from `git`.
- To reduce the aeon's code size by copying only some parts (e.g. `thunderchez`).

Notes:

- `rsc3` was retreived on 21st January 2019. Its build system & scripts were modified to work inside chez scheme and aeon's startup procedure.
- `thunder` contains a few libraries copied from `thunderchez`
