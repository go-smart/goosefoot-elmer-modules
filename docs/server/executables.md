# Executables

## go-smart-simulation-server

This is the simulation server script itself. You are expected to have a full
working installation of GSSA for this to behave as described, including GSSF and
Docker if you wish to run such workflows.

    go-smart-simulation-server [-h] [--host HOST]
                                  [--websocket-port WEBSOCKET_PORT]
                                  [--sftp-host SFTP_HOST]
                                  [--sftp-port SFTP_PORT]
                                  [--key-file KEY_FILE] [--ignore-development]
                                  [sid]

### Positional arguments

Argument            | Description
------------------- | ------------------
sid                 | Server UUID (should be subdirectory of current directory)

### Optional arguments

Argument                         | Description
-------------------------------- | ------------------
-h, --help                       | show this help message and exit
--host HOST                      | host to connect to
--websocket-port WEBSOCKET_PORT  | port hosting websocket
--sftp-host SFTP_HOST            | server hosting SFTP
--sftp-port SFTP_PORT            | port hosting SFTP
--key-file KEY_FILE              | file for authenticating to SFTP
--ignore-development             | do not process cases with parameter DEVELOPMENT truthy

## go-smart-simulation-client

This is primarily a testing script for the server, but is capable of delivering
a GSSA-XML to any registered GSSA server via the named WAMP router.

    go-smart-simulation-client [-h] [--gssa-file GSSA_FILE]
                                  [--subdir SUBDIR] [--host HOST]
                                  [--websocket-port WEBSOCKET_PORT]
                                  [--skip-clean]
                                  [--output OUTPUT [OUTPUT ...]]
                                  [--definition DEFINITION [DEFINITION ...]]
                                  [--input INPUT [INPUT ...]]
                                  [--server SERVER]

### Optional arguments

Argument                                    | Description
------------------------------------------- | ------------------
  -h, --help                                | show this help message and exit
  --gssa-file GSSA_FILE                     | GSSA-XML simulation description
  --subdir SUBDIR                           | subdirectory of /tmp containing input files
  --host HOST                               | host to connect to
  --websocket-port PORT                     | port hosting websocket
  --skip-clean                              | do not clean up after run
  --output OUT [OUT ...]                    | file(s) to request as output
  --definition DEF [DEF ...]                | file(s) containing module / text of the definition node<br/>(which should exist but be empty in the GSSA file)
  --input IN [IN ...]                       | input files for surfaces, etc.
  --server SERVER                           | specific server to contact (UUID)
