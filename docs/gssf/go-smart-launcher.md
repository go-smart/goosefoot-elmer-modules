# Go-Smart Simulation Framework - Launcher script (go-smart-launcher)

## Command line usage

Wrapper for Go-Smart simulation environment. This is the usual entry point to
GSSF and is invoked by the GSSA server with when used inside the architecture.
It may, however, be used standalone from the command-line.

```shell
go-smart-launcher [-h] [--elmer ELMER_BINARY]
                  [--elmer-logfile OUTFILENAME] [--logfile-addpid]
                  [--silent] [--debug] [--nprocs NPROCS] [--only ONLY]
                  [--black-and-white] [--leavetree]
                  [--cwd GLOBAL_WORKING_DIRECTORY]
                  [--status-socket UPDATE_STATUS]
                  configfilenames
                  ...
```

### Positional arguments

Argument | Description
---------|------------
  configfilenames    |  Locations of configuration file (latter override former)

### Optional arguments

Argument | Description
---------|------------
  -h, --help                     |  show this help message and exit
  --elmer ELMER_BINARY           |  name of the ElmerSolver binary
  --elmer-logfile OUTFILENAME    |  name of the ElmerSolver binary
  --logfile-addpid               |  whether the PID should be appended to the given
                                 |  logfile name
  --silent                       |  prevent wrapper output
  --debug                        |  additional, debug output (overridden by --silent)
  --nprocs NPROCS                |  number of processes to start
  --only ONLY                    |  only execute a single component
  --black-and-white              |  force colorama off
  --leavetree                    |  do not touch the mesh filetree in Elmer
  --cwd GLOBAL_WORKING_DIRECTORY |  override working directory as root of simulation
  --status-socket UPDATE_STATUS  |  location of socket to which to write status
