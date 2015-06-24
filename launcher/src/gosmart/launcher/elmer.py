# This file is part of the Go-Smart Simulation Architecture (GSSA).
# Go-Smart is an EU-FP7 project, funded by the European Commission.
#
# Copyright (C) 2013-  NUMA Engineering Ltd. (see AUTHORS file)
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
import os
import select
import threading
import socket
import string
import shutil
import re
import uuid
import json

import gosmart.config
from gosmart.launcher.component import GoSmartComponent
from gosmart.launcher.globals import \
    defaults, \
    _sif_calculations, \
    _elmer_modules_required, \
    launcher_command, \
    slugify
from gosmart.launcher.elmer_probelocation import GoSmartElmerProbeLocationFactory, probe_location_factories
from gosmart.launcher.elmer_powerovertime import power_over_time_factories


def _type_to_sif_type(typ, var):
    if (typ == 'float' or typ == 'array(float)'):
        return 'Real %s' % _type_to_sif_string(typ, var)
    elif (typ == 'int' or typ == 'array(int)'):
        return 'Integer %s' % _type_to_sif_string(typ, var)
    elif (typ == 'array(Time,float)'):
        ret = "Variable Time\nReal \n%s\nEnd" % "\n".join(map(lambda row: " ".join(map(str, row)), sorted(json.loads(var).items())))
        return ret
    var = _type_to_sif_string(typ, var)
    return var


def _type_to_sif_string(typ, var):
    if (typ in ('array(float)', 'array(int)')):
        return " ".join(map(str, json.loads(var)))
    return str(var)


# Class to hold settings specific to the Go-Smart/Elmer solver
class GoSmartElmer(GoSmartComponent):
    suffix = 'elmer'
    _startinfo = 'ELMERSOLVER_STARTINFO'
    restart_binary = 'ResultToResult'
    _sif_suffices = {
        "rfa point sources": "_ps",
        "rfa point sources (modified bioheat)": "_ps-modbioheat",
        "rfa point sources (new elmer)": "_ps-newelmer",
        "rfa joule heating": "_jh",
        "mwa": "_mwa",
        "mwa (nonlinear)": "_mwa-nl",
        "cryoablation": "_cryo",
        "ire": "_ire"
    }

    logpick_pairs = (("NumaCellStateSolve: Starting Assembly", "NumaCellStateSolve: Assembly done", "Assembly [cell]"),
                     ("HeatSolve: Starting Assembly", "HeatSolve:: Dirichlet boundary conditions set", "Assembly [heat]"),
                     ("NumaStatCurrentSolve: Starting Assembly", "NumaStatCurrentSolve: Assembly done", "Assembly [jh]"),
                     ("CRS_IncompleteLU: ILU(0) (Real), Starting Factorization", "ComputeChange:", "Solver"))

    todos = ("PTW: Conversion from ml to g in perfusion rate at 1:1!!",
             "PTW: Basic alphabetization to rotate indices means that this will break with more than ~26 zones - switch letters to (slugified?) suitably escaped zone names",
             "PTW: Double check that the missing boundary conditions don't default to somewhere else - use non-existent target boundaries if absolutely ness.",
             "PTW: Check correct inclusion of perfusion coefficient in JH",
             "PTW: Ignoring segfaults and other signals (negative codes indicate terminated by a given Unix signal",
             "PTW: MWA not using variable power",
             "PTW: Not currently using intersection and (in-)trocar points")
    needle_type = "excised"
    probe_location_factory = None
    _restarting = None

    def __init__(self, logger, elmer_binary=None, elmer_mpi_binary=None, probe_location_factory=None):
        super().__init__(logger)

        if elmer_binary is None:
            elmer_binary = defaults["elmer binary"]
        if elmer_mpi_binary is None:
            elmer_mpi_binary = defaults["elmer mpi binary"]
        if probe_location_factory is None:
            probe_location_factory = GoSmartElmerProbeLocationFactory()
        self.fortran_binary = defaults["fortran binary"]

        self.elmer_binary = elmer_binary
        self.elmer_mpi_binary = elmer_mpi_binary
        self._sif_mapping = {
            "RUNNAME": self.logger.runname,
        }
        self._probe_location_factory = probe_location_factory
        self._sif_variant = "default"
        self._sif = None
        self._elmer_modules_required = []
        self._algorithms = {}

        self.configfiles = []

    def _maybefloat(self, c):
        if isinstance(c, str):
            try:
                c = float(c)
            except:
                pass
        return c

    def _get_requirements(self, requirements):
        inp = [self.get_constant(x) for x in requirements]
        return list(map(self._maybefloat, inp))

    def _check_sif_mapping_set(self, sif_template):
        for name, definition in _sif_calculations.items():
            if all(v in self.get_constants() for v in definition['requirements']):
                value = definition['formula'](*self._get_requirements(definition['requirements']))
                self.add_or_update_constant(name, str(value), False, "CALCULATED")
            else:
                self.logger.print_debug("SIF Generation - Skipping %s as missing parameters (requires %s)" % (name, ", ".join(definition['requirements'])))

        for name, info in self.get_mapping_warn().items():
            if not sif_template.template.find(name):
                self.logger.print_error("SIF Generation - Missing %s" % info)

        if self.logger.debug:
            for name, value in self.get_constants().items():
                self.logger.print_debug("%s set to %s" % (name, value))

    def _prepare_algorithms(self):
        for result, algorithm in self._algorithms.items():
            filename = str(uuid.uuid4()) + '.matc'
            with open(os.path.join(self.logger.make_cwd(self.cwd), filename), 'w') as f:
                f.write(algorithm["definition"])
            algorithm["sources"] = "$ source(\"%s\")\n" % filename
            algorithm["call"] = "Variable %s\n\t\tReal MATC \"%s(%s)\"" % (
                ", ".join(algorithm["arguments"]),
                result,
                ", ".join("tx(%d)" % i for i in range(len(algorithm["arguments"])))
            )

    def _generate_sif(self):
        if self._sif_variant is not None:
            sif_suffix = self._sif_suffices[self._sif_variant]
            sif_filename = os.path.join(gosmart.config.template_directory, "templates", "go-smart-template%s.sif" % sif_suffix)
            self.logger.print_debug("SIF template filename %s" % sif_filename)
            sif_template_stream = open(sif_filename, "r")
            sif_definition = sif_template_stream.read()
            sif_template_stream.close()
        else:
            sif_definition = self._sif

        def type_substitution(match):
            typ = self.logger.get_constant_type(match.group(2))

            if typ is not None:
                altered = match.group(1) + _type_to_sif_type(typ, self.logger.get_constant(match.group(2)))
                self.logger.print_debug(altered)
                return altered

            return match.group(0)

        sif_definition = re.sub(r'(=\s*)\$((CONSTANT|SETTING)_[A-Za-z_0-9]+)', type_substitution, sif_definition)
        sif_template = string.Template(sif_definition)

        if self._restarting:
            self._sif_mapping["RESTART_SECTION"] = """
                Restart File = File \"Result.dat\"
                Restart Position = Integer %d
                Restart Time = Real %lf
            """ % (self._restarting['position'], self._restarting['time'])
            self._sif_mapping["RESTART_FILEINDEX_OFFSET"] = "Fileindex Offset = %d" % (self._restarting['position'] + 1)
        else:
            self._sif_mapping["RESTART_FILEINDEX_OFFSET"] = ""
            self._sif_mapping["RESTART_SECTION"] = """
                Output File = 'Restart.dat'
            """
        self._prepare_algorithms()
        self._check_sif_mapping_set(sif_template)

        constants = self.get_constants()
        for constant, value in constants.items():
            constants[constant] = _type_to_sif_string(self.logger.get_constant_type(constant), value)

        self._sif_mapping.update(constants)
        self._sif_mapping.update(self.logger.get_region_ids())
        self._sif_mapping.update(dict((r, a["call"]) for r, a in self._algorithms.items()))
        sources = "\n".join(a["sources"] for a in self._algorithms.values())
        self._sif_mapping["SOURCES"] = sources

        for regions in re.finditer(r'\$((BOUNDARIES_|BODIES_|)REGIONS_[A-Z0-9_]*)', sif_template.template):
            regions = regions.group(1)
            if regions not in self._sif_mapping:
                self._sif_mapping[regions] = "!Boundary missing: %s" % regions
                self.logger.print_debug("%s empty" % regions)

        sif_string = sif_template.substitute(self._sif_mapping)
        sif_stream = open(os.path.join(self.logger.make_cwd(self.cwd), self._sif_file), "w")
        sif_stream.write(sif_string)
        sif_stream.close()

    def _generate_startinfo(self, nprocs, sif=None):
        if sif is None:
            sif = self._sif_file

        with open(os.path.join(self.logger.make_cwd(self.cwd), self._startinfo), "w") as startinfo_file:
            startinfo_file.write("%s\n" % sif)
            startinfo_file.write("%d\n" % nprocs)

    def get_endtime(self):
        try:
            endtime = float(self.get_constant("final timestep", group="SETTING")) * \
                float(self.get_constant("timestep size", group="SETTING"))
        except KeyError:
            self.logger.print_fatal("Final timestep and timestep size settings not yet available")

        return endtime

    def _generate_power_over_time(self):
        if not hasattr(self, "power_over_time_factory"):
            return

        power_over_time = self.power_over_time_factory.generate_powers(self.get_endtime())
        if power_over_time is None:
            self.logger.print_fatal("Failed to generate power over time")

        power_over_time_template_file = open(os.path.join(gosmart.config.template_directory, "go-smart-power-over-time.txt"), "r")
        power_over_time_template = string.Template(power_over_time_template_file.read())
        power_over_time_template_file.close()

        rows = sorted(power_over_time.items())
        power_over_time_table = "\n".join(("%lf\t%lf" % row) for row in rows)

        power_over_time_filename = os.path.join(self.logger.make_cwd(self.cwd), self.logger.runname.lower() + "-power-over-time.txt")
        with open(power_over_time_filename, "w") as power_over_time_file:
            power_over_time_file.write(power_over_time_template.substitute({"POWERTABLE": power_over_time_table}))

        self.logger.print_debug("Generated power over time")

    def _generate_probe_locations(self):
        probe_locations = self.probe_location_factory.generate_locations()
        if probe_locations is None:
            self.logger.print_fatal("Failed to generate probe locations")

        # Note that time can also be phase
        self._sif_mapping.update({
            "PROBELOCATIONS_COUNT": len(probe_locations),
            "PROBELOCATIONS": " ".join(str(l) for l in sorted(l.time for l in probe_locations.values())),
        })

        probe_location_template_file = open(os.path.join(gosmart.config.template_directory, "go-smart-probe-location-template.txt"), "r")
        probe_location_template = string.Template(probe_location_template_file.read())
        probe_location_template_file.close()

        for idx, probe_location in probe_locations.items():
            for segment in ('ends', 'middles', 'thermocouples'):
                probe_location_filename = os.path.join(self.logger.make_cwd(self.cwd), self.logger.runname.lower() + "-probe-locations_" + str(idx + 1) + "-" + segment + ".txt")
                with open(probe_location_filename, "w") as probe_location_file:
                    points = probe_location[segment]
                    point_template_map = {"POINTCOUNT": len(points), "POINTTABLE": "\n".join(" ".join(map(str, list(row[0]) + [row[1]])) for row in points)}
                    probe_location_file.write(probe_location_template.substitute(point_template_map))

    def _copy_power_profiles(self):
        for profile in os.listdir(gosmart.config.power_profiles_directory):
            shutil.copy(os.path.join(gosmart.config.power_profiles_directory, profile), self.logger.make_cwd(self.cwd))

    def _build_elmer_modules(self):
        elmer_modules = self._elmer_modules_required
        if self._sif_variant is not None:
            elmer_modules += _elmer_modules_required[self._sif_variant]
        for mod in elmer_modules:
            shutil.copy(os.path.join(gosmart.config.fortran_template_directory, "%s.f90" % mod), self.logger.make_cwd(self.cwd))
            self._launch_subprocess(self.fortran_binary, [
                "%s.f90" % mod,
                "-o", "%s.so" % mod,
                "-I", gosmart.config.fortran_modules_directory,
                "-fPIC", "-shared",
            ])

    def _interpolate_restart(self, nprocs):
        restart = self._restarting
        self._generate_startinfo(nprocs, "%s.sif" % restart["old"])
        cwd = self.logger.make_cwd(self.suffix)
        shutil.copy(os.path.join(cwd, "%s.sif" % self.logger.runname), os.path.join(cwd, "Interpolation.sif"))
        self._launch_subprocess(self.restart_binary, [])
        self._generate_startinfo(nprocs)

    def set_update_status(self, update_status):
        self._update_status = update_status

    def _percentage_server(self):
        cwd = self.logger.make_cwd(self.suffix)
        address = os.path.join(cwd, "percentage.sock")

        try:
            os.remove(address)
        except OSError:
            if os.path.exists(address):
                return

        s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        s.bind(address)
        s.listen(1)
        self._monitoring_status = True

        while self._monitoring_status:
            inp, out, exc = select.select([s], [], [])

            if len(inp):
                conn, client = s.accept()
                client_file = conn.makefile("r", buffering=0)

                while True:
                    data = client_file.readline()
                    if data:
                        self.logger.print_line("[status]: %s" % data)

                        if self._update_status is not None:
                            self._update_status(data)
                    else:
                        # Stop watching; we should only look after one process in a run
                        self.stop_monitoring_status()
                        break

        conn.close()

    def stop_monitoring_status(self):
        self._monitoring_status = False

    def _setup_percentage_socket(self):
        self._percentage_thread = threading.Thread(target=self._percentage_server)
        self._percentage_thread.start()

    def launch(self, nprocs=1, mesh_locations=None):
        super().launch()

        if not mesh_locations:
            mesh_locations = {"-mesher": "elmergrid/%s-mesher" % self.logger.runname}

        self.cwd = self.suffix

        for appendix, mesh_location in mesh_locations.items():
            mesh_target = os.path.join(self.logger.make_cwd(self.cwd), self.logger.runname + appendix)

            if not self.logger.leavetree:
                if os.path.exists(mesh_target):
                    shutil.rmtree(mesh_target)
                shutil.copytree(os.path.join(self.logger.get_cwd(), mesh_location), mesh_target)

            mesh_location_slug = slugify(appendix)
            self._sif_mapping["MESHLOCATION" + mesh_location_slug] = self.logger.runname + appendix

        self._sif_file = self.logger.runname + ".sif"

        if nprocs < 2:
            self.logger.print_line(" [in serial]")
            if not self.logger.leavetree:
                self._generate_power_over_time()
                if self.probe_location_factory:
                    self._generate_probe_locations()

                self._copy_power_profiles()
                self._build_elmer_modules()

                self._generate_sif()
                if self._restarting:
                    self._interpolate_restart(nprocs)
                self._generate_startinfo(nprocs)
                try:
                    self._setup_percentage_socket()
                    self._launch_subprocess(self.elmer_binary, [])
                finally:
                    print("STOPPING MONITORING")
                    self.stop_monitoring_status()
            else:
                self._launch_subprocess(self.elmer_binary, [])
        else:

            self._generate_power_over_time()
            if self.probe_location_factory:
                self._generate_probe_locations()

            self._copy_power_profiles()
            self._build_elmer_modules()

            if self._restarting:
                self._interpolate_restart(nprocs)
            self._generate_sif()
            self._generate_startinfo(nprocs)
            self.cwd = '.'
            self.logger.print_line(" [using MPI with %d processes]" % nprocs)
            args = ["-np", str(nprocs),
                    launcher_command,
                    "--silent",
                    "--only", "elmer",
                    "--leavetree",
                    "--elmer", self.elmer_mpi_binary,
                    "--elmer-logfile", self.outfilename,
                    "--logfile-addpid"] + self.configfiles
            self._launch_subprocess("mpirun", args, mute=True)

        self.stop_monitoring_status()
        self._percentage_thread.join()

    def parse_config(self, config_node):
        super().parse_config(config_node)

        for element in config_node:
            if element.tag == 'probelocations':
                if element.get('type') not in probe_location_factories.keys():
                    self.logger.print_fatal("Could not find probe location type")

                self.probe_location_factory = probe_location_factories[element.get('type')]()
                self.probe_location_factory.parse_config(element)
            elif element.tag == 'restart':
                self._restarting = {'time': float(element.get('time')), 'position': int(element.get('position')), 'old': element.get('old')}
            elif element.tag == 'pointsources':
                self.needle_type = 'point sources'
                generation_system = element.get('system')
                if generation_system is None:
                    generation_system = 'straight tines'
                self.probe_location_factory = probe_location_factories[generation_system]()
                self.probe_location_factory.parse_config(element, logger=self.logger)
            elif element.tag == 'powerovertime':
                if element.get('type') not in power_over_time_factories.keys():
                    self.logger.print_fatal("Could not find power-over-time type")

                self.power_over_time_factory = power_over_time_factories[element.get('type')]()
                self.power_over_time_factory.parse_config(element)
            elif element.tag == 'variant':
                modules_required = element.get('modules')
                if modules_required is not None:
                    self._elmer_modules_required += modules_required.split('; ')
                name = element.get('name')
                if name is None:
                    self._sif = element.text
                    self._sif_variant = None
                elif name.lower() in ("rfa point sources", "mwa", "mwa (nonlinear)", "cryoablation", "ire", "rfa joule heating", "rfa point sources (new elmer)"):
                    self._sif_variant = name.lower()
                    self._load_constant_set("variant-%s" % slugify(self._sif_variant))

                    for jh_element in config_node:
                        pass
                else:
                    self.logger.print_fatal("Unknown variant: %s" % name)
            elif element.tag == 'settings':
                for constant in element:
                    value = constant.get("value")
                    if value is None:
                        value = constant
                    self.add_or_update_constant(constant.get("name"), value, True, "SETTING", typ=constant.get("type"))
            elif element.tag == 'algorithms':
                for algorithm in element:
                    arguments = map(lambda a: a.get("name"), algorithm.find("arguments"))
                    definition = algorithm.find("content").text
                    if definition is not None:
                        self._algorithms[algorithm.get("result")] = {
                            "definition": definition,
                            "arguments": list(arguments)
                        }
            else:
                self.logger.print_error("Unknown element %s in %s" %
                                        (element.tag, config_node.tag))
