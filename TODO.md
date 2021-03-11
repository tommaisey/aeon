
- Add init routine to make a new project
   - This will create a new directory, with an init file that contains a 
     path to the known aeon directory, which is probably the current directory


## Installation
- Figure out installation procedure, add to top-level docs:
   - `brew install chezscheme supercollider`
   - `git clone aeon.git`
   - `cd aeon; scheme aeon.scm`
   - `(pattern test (in! 4))`
- Implies that the init file will be able to launch a SuperCollider
  process automatically. (Actually, why didn't I think of that before?)
