#Go-Smart Simulation Architecture - Simulation Server

One or more simulation servers attach to the WAMP router and handle new
simulation requests. More than one may run on a machine and, as the simulations
are launched as one or more separate processes, there is no necessarily
correlation between servers and processes.

The server is controlled via WAMP calls and is subscribed to certain pubsub
notifications. Any client permitted on the WAMP router may manipulate it, so
protection should be put in accordingly, especially until access control is
established here.
