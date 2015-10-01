class GoSmartSimulationFrameworkArguments:
    def __init__(self, elmer_binary=None, outfilename=None, addpid=False, silent=True,
                 debug=False, nprocs=None, baw=True, only=None, leavetree=False, configfilenames=[],
                 status_socket='update.sock'):
        self.elmer_binary = elmer_binary
        self.outfilename = outfilename
        self.addpid = addpid
        self.silent = silent
        self.debug = debug
        self.nprocs = nprocs
        self.baw = baw
        self.only = only
        self.leavetree = leavetree
        self.configfilenames = configfilenames
        self.status_socket = status_socket

    def to_list(self):
        args = {
            '--elmer': self.elmer_binary,
            '--elmer-logfile': self.outfilename,
            '--logfile-addpid': self.addpid,
            '--silent': self.silent,
            '--debug': self.debug,
            '--nprocs': self.nprocs,
            '--only': self.only,
            '--black-and-white': self.baw,
            '--leavetree': self.leavetree,
            '--status-socket': self.status_socket
        }
        command_line = []
        for k, v in args.items():
            if v is not None:
                if isinstance(v, bool):
                    if v:
                        command_line += [k]
                else:
                    command_line += [k, str(v)]
        return command_line + self.configfilenames
