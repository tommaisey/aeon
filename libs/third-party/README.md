README: Aeon Dependencies
-----------------------
This repo contains third party scheme code used in Aeon's pattern engine. Because
these libraries can be found in repositories spanning many different version control
systems, and because many have had to be massaged/ported to Chez, I'm going to maintain
my own monorepo of these libraries. Some will stay very close to their original versions,
others will have to be heavily refactored (e.g. rsc3).

RSC3 was retreived on 21st January 2019, and refactored into R6RS libraries, partly through
a quick 'n' dirty automated tool I wrote. Its file structure has been changed as a result,
but as yet its functionality has not been. If I need to start making changes to the library
itself I'll have to come up with a strategy to get those changes upstream. Or just fork it
as it doesn't appear to be maintained.

Other libraries, such as 'thunderchez' and 'socket' are mostly unchanged from their original,
so could be updated quite painlessly.
