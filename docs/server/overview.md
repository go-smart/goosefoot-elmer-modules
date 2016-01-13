# Go-Smart Simulation Architecture - Simulation Server

One or more simulation servers attach to the WAMP router and handle new
simulation requests. More than one may run on a machine and, as the simulations
are launched as one or more separate processes, there is no necessarily
correlation between servers and processes.

Each server should have a unique ID (by default, a UUID). This may persist
through restarts by passing it as an argument to
[go-smart-simulation-server](executables.md#go-smart-simulation-server), the
entry-point for the simulation server. This also refers to the server-side
simulation [database](database.md) and re-using the ID will allow the simulation
to maintain awareness of pre-existing simulations after restart.

The server is controlled via WAMP calls and, to enable discovery, is subscribed to
certain Pub-Sub
notifications. Any client permitted on the WAMP router may manipulate it, so
protection should be put in accordingly, especially until access control is
established here.

As with [GSSF](../gssf/logging.md#statistics-monitoring), the GSSA server will
register with [vigilant](https://github.com/redbrain/vigilant) if it finds it.
However, this has not been fully tested, and is not part of our current use case;
it will be more extensively documented when proved reliable.

## Controlling the server

The simulation server is controlled via WAMP. This requires a [WAMP
router](router.md), to which the server and client both connect.
[WAMP](http://wamp-proto.org/) is a
protocol providing RPC (remote procedure call) and Pub-Sub
(publisher-subscriber) support. Both communication patterns are used by the
GSSA server. To provide the server-side WAMP session, the
[Autobahn](http://autobahn.ws/) libraries are used.

On start-up, the server registers a series of RPC methods:

Method | Description
-------|------------
`com.gosmartsimulation.[ID].init(GUID)` | Notify server that a simulation will be set up (dummy method at present)
`com.gosmartsimulation.[ID].start(GUID)` | Begin simulating simulation with given GUID
`com.gosmartsimulation.[ID].update_settings_xml(GUID, XML)` | Load the passed XML string as [GSSA-XML](../gssa-xml.md) for this simulation
`com.gosmartsimulation.[ID].update_files(GUID, FILES)` | Add the passed files map (basenames to remote locations relative to the transferrer) to the simulation's input file map
`com.gosmartsimulation.[ID].request_files(GUID, FILES)` | Send the requested output files (basenames to remote locations relative to the transferrer)
`com.gosmartsimulation.[ID].finalize(GUID)` | Do any remaining XML processing or file transferring, to be ready for the simulation to begin
`com.gosmartsimulation.[ID].compare(XML1, XML2)` | Compare two GSSA-XML files and report the differences in a user readable list
`com.gosmartsimulation.[ID].clean(GUID)` | Remove any server-side files related to this simulation, including the working directory
`com.gosmartsimulation.[ID].properties(GUID)` | Return a map of basic server-defined properties for this simulation (server-side working directory)
`com.gosmartsimulation.[ID].retrieve_status(GUID)` | Return the last known status of the simulation - this comprises most information held about a simulation, in a map

The `ID` should be the ID of the simulation server. In addition, the first
server registered will add each call without the `[ID].` component, providing a
global default on the WAMP router.

The server will also publish/subscribe the following notifications:

On Event | Description
------|----------------------
`com.gosmartsimulation.request_announce` | *Response*: `com.gosmartsimulation.announce`<br/> Does not expect arguments. For each simulation in the database, publishes with arguments: *(server_id, simulation, (percentage, status), working_directory, last_status_timestamp, validation_xml)*. Additionally, triggers an `identify` response afterwards.
`com.gosmartsimulation.request_identify` | *Response*: `com.gosmartsimulation.identify`<br/> Does not expect arguments. Responds by publishing *(server_id, server_hostname, score)*. The *score* is the difference between the available processors and the number of active simulations in the database.

Each server also subscribes to the same event with `[ID].` inserted before
`request`, allowing targeted triggering of the same server response.

In addition to the responses above, a client may listen for the following
events:

Event | Description
------|------------
`com.gosmartsimulation.complete` | Indicates a simulation completion. Arguments: *(simulation_id, SUCCESS_STATUS, working_directory, timestamp, validation_xml)*
`com.gosmartsimulation.fail` | Indicates a simulation failure. Arguments: *(simulation_id, ERROR_STATUS, working_directory, timestamp, validation_xml)*
`com.gosmartsimulation.update_status` | Indicates a simulation failure. Arguments: *(simulation_id, (percentage, PROGRESS_STATUS), working_directory, timestamp, validation_xml)*

Note that, with RPC calls, the server is configured to forward exceptions back
to the client. In general, these should be [errors](../errors.md) defined for GSSA,
but your client should recognise the possibility that they are unhandled
server-side errors and catch accordingly. If not a `GoSmartError`, this would indicate a bug-report
should be filed against GSSA.

It should be noted by authors of client-side applications that RPC, in
particular, needs care to implement - while the method calls reach specially
prepared routines on the server-side, as the protocol is over a network, no
guarantees are available about the completion of the call, the time for response or the integrity of the
transmission or reply, as might exist in the equivalent library API call. As
such, appropriate error handling for exceptions from your WAMP library should be
incorporated, and threading/timeouts set.

## Concurrency

As much as possible, any asynchronous behaviour has been mediated by
[asyncio](https://docs.python.org/3/library/asyncio.html)
using *coroutines* from the main thread. This includes simulation, meshing, WAMP
interactions, socket handling, database interaction and file transfers.
