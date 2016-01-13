# Go-Smart Simulation Server Database

By default, the GSSA server will create an [SQLite](https://www.sqlite.org/)
database to record past and present simulations, and their statuses. It is
specific to a server ID, so you can effectively resume a server by passing the
same ID to
[`go-smart-simulation-server`](executables.md#go-smart-simulation-server).

The database records the following information:

Column | Type | Description
-------|------|------------
id | integer | Row in the database, not the GUID of the simulation
guid | guid | GUID of the simulation, as used by the client
directory | text | Location of the simulation's (last known) working directory
exit_code | text(null) | [Error](../errors.md) code on simulation exit
status | text | Last status update from the simulation
percentage | real | Completion percentage
timestamp | real | Time of last status update
validation | text | Validation XML
created_at | timestamp | Entry created
deleted | tinyint | Soft-deletion flag
