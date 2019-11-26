README: timeout
-----------------------
A tiny library to expose Unix `select` so we don't have to block waiting to `read` a pipe indefinitely.

This helps us report when SuperCollider hasn't started, instead of just hanging.
