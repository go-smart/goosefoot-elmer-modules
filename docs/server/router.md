# Go-Smart Simulation Architecture - WAMP Router

The simulation server and client relate to each other through a WAMP server. For
this purpose, we recommend [Crossbar](http://crossbar.io/) and have provided
sample configuration within the GSSA source tree (in the `web/` subdirectory).

This allows direct interaction between clients of various languages (JS, Python and
C# have been used) with the Python server.
