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
from lxml import etree as ET
import time
import errno
import subprocess
import math
import threading
import re

from gosmart.launcher.globals import slugify, colorama_imported
import gosmart.config
from gosmart.server.error import Error as ErrorCode

if colorama_imported:
    import colorama


# Note that derived classes should account for the possibility
# that parse_config is not called due to missing information in
# the settings file (intentionally or otherwise)
class GoSmartComponent:
    suffix = 'unknown'
    todos = ()
    logpick_pairs = ()
    error_regex = None
    last_error = None
    manual_return_code_handling = False

    # Tuplet - number of consecutive triggered seconds before suppressing; number of lines per second to trigger
    suppress_logging_over_per_second = None

    def __init__(self, logger):
        self.logger = logger
        self.mute = None  # This _overrides_ value passed to _launch_subprocess
        self.cwd = "."

        self._constant_mapping = {}
        self._constant_mapping_types = {}
        self._constant_mapping_warn = {}

        self._timings = {}
        self._timings_tmp = {}

        self.print_timing = lambda t: self.logger.print_line(t, color="BLUE", color_bright=True)
        self.error_regex = re.compile(r'(error|fatal)', re.IGNORECASE)

        for todo in self.todos:
            self.logger.print_line("TODO (%s): %s" % (self.suffix, todo), color_bright=True)

    def logpick(self, offset, logline):
        sym = " "

        if not self.logpick_pairs:
            return sym

        for pair in self.logpick_pairs:
            if pair[0] in self._timings_tmp and logline.startswith(pair[1]):
                if pair[0] not in self._timings:
                    self._timings[pair[0]] = 0
                self._timings[pair[0]] += offset - self._timings_tmp[pair[0]]
                del self._timings_tmp[pair[0]]
                sym = ">"
            if pair[0] not in self._timings_tmp and logline.startswith(pair[0]):
                self._timings_tmp[pair[0]] = offset
                sym = "|" if sym in (">", "|") else "<"

        return sym

    def print_logpick(self, total):
        self.print_timing("Timings (sec resolution):")
        total_counted = 0
        for timing in self.logpick_pairs:
            if timing[0] in self._timings:
                self.print_timing(" -- %4d %s <'%s' - '%s'>" % (self._timings[timing[0]], timing[2], timing[0], timing[1]))
                total_counted += self._timings[timing[0]]

        self.print_timing(" -- %4d [other]" % (total - total_counted))
        self.print_timing("    ====")
        self.print_timing(" -- %d" % total)

    def add_or_update_constant(self, name, value, warn=False, group="CONSTANT", typ=None):
        mangled_name = "%s_%s" % (slugify(group), slugify(name))
        self._constant_mapping[mangled_name] = value
        self._constant_mapping_types[mangled_name] = typ
        if warn:
            self._constant_mapping_warn[mangled_name] = "constant : %s" % name

        self.logger.add_or_update_constant(name, value, False, group, typ=typ)

    def get_constant_type(self, name, group="CONSTANT"):
        mangled_name = "%s_%s" % (slugify(group), slugify(name))

        if name in self._constant_mapping_types:
            return self._constant_mapping_types[name]
        elif mangled_name in self._constant_mapping_types:
            return self._constant_mapping_types[mangled_name]

        return self.logger.get_constant(name, group)

    def get_constant(self, name, group="CONSTANT"):
        mangled_name = "%s_%s" % (slugify(group), slugify(name))

        if name in self._constant_mapping:
            return self._constant_mapping[name]
        elif mangled_name in self._constant_mapping:
            return self._constant_mapping[mangled_name]

        return self.logger.get_constant(name, group)

    def get_constants(self):
        constants = self._constant_mapping.copy()
        constants.update(self.logger.get_constants())

        return constants

    def get_mapping_warn(self):
        warn = self._constant_mapping_warn.copy()
        warn.update(self.logger.get_mapping_warn())

        return warn

    def parse_config(self, config_node):
        if config_node.get('mute') == "true":
            self.mute = True
        if config_node.get('mute') == "false":
            self.mute = False

        element = config_node.find('constants')
        if element is not None:
            default_set = element.get("defaults")

            if default_set is not None:
                self._load_constant_set(default_set)

            for constant in element:
                value = constant.get("value")
                if value is None:
                    value = constant
                self.add_or_update_constant(constant.get("name"), value, True, constant.tag, typ=constant.get("type"))

    def _load_constant_set(self, setname):
        constant_filename = "constants/constants-%s.xml" % setname.lower()
        with open(os.path.join(gosmart.config.template_directory, constant_filename), "r") as constant_file:
            configtree = ET.parse(constant_file)
            root = configtree.getroot()

            for constant in root:
                value = constant.get("value")
                if value is None:
                    value = constant
                self.add_or_update_constant(constant.get('name'), value, (constant.get('warn') == 'yes'), typ=constant.get("type"))

    def set_outfile_prefix(self, outfile):
        self.outfilename = os.path.join(self.logger.get_cwd(), outfile)

    def get_outfile(self):
        return self.outfilename + '-' + self.suffix + '.log'

    def _log_thread(self, process_stdout, outstream, mute):
        current_second = -1
        current_per_second = 0
        consecutive_overrun = 0
        for line in iter(process_stdout.readline, ''):
            outstream.write(line)

            offset = time.time() - self._start_time
            sym = self.logpick(offset, line)

            if self.error_regex and self.error_regex.search(line):
                self.last_error = line

            suppress = mute
            if self.suppress_logging_over_per_second is not None:
                if math.floor(offset) == current_second:
                    current_per_second += 1
                else:
                    if current_per_second == -1:
                        consecutive_overrun = 0
                    else:
                        current_per_second = -1
                        consecutive_overrun += 1

                    current_second = math.floor(offset)

                if consecutive_overrun >= self.suppress_logging_over_per_second[0]:
                    if current_per_second == self.suppress_logging_over_per_second[1]:
                        line = "......"
                    elif current_per_second > self.suppress_logging_over_per_second[1]:
                        suppress = True

            if not suppress:
                if colorama_imported:
                    line = "%s  + %8d %s[%s %s" % (colorama.Fore.YELLOW, offset, sym, colorama.Fore.RESET, line.strip())
                else:
                    line = "  + %8d %s[ %s" % (offset, sym, line.strip())
                self.logger.print_line(line, color_text=False)

    def _launch_subprocess(self, executable, args, mute=False, environment={}):
        if self.mute is not None:
            mute = self.mute

        outfile = self.get_outfile()
        outstream = open(outfile, "w")

        self.logger.print_line("  (output to %s)" % outfile)

        if not mute and not self.logger.silent:
            self.logger.print_error(outfile)

        args = [str(a) for a in args]
        args.insert(0, executable)
        self.logger.print_line("  Command: " + " ".join(args) + " [" + self.cwd + "]")

        outstream_line = "GOSMART: Output for " + " ".join(args) + " [" + self.cwd + "]"
        outstream.write(outstream_line + "\n")
        outstream.write("=" * len(outstream_line))
        outstream.write("\n")

        self._start_time = time.time()
        self.logger.print_line("  Starting %d s after launcher" % (self._start_time - self.logger.init_time))

        self.logger.flush_logfile()
        outstream.flush()

        self._process = subprocess.Popen(args, cwd=self.logger.make_cwd(self.cwd), stderr=subprocess.STDOUT,
                                         stdout=subprocess.PIPE, bufsize=1, universal_newlines=True)

        thread = threading.Thread(target=self._log_thread, args=[self._process.stdout, outstream, mute])
        thread.daemon = True
        thread.start()

        # Let it go about its business
        return_code = self._process.wait()

        self._end_time = time.time()
        self.logger.print_line("  Starting %d s after launcher" % (self._end_time - self.logger.init_time))

        # If the return code is non-zero we crash out
        # FIXME: Ignoring segfaults and other signals (negative codes indicate terminated by a given Unix signal)
        if not self.manual_return_code_handling and return_code != 0:
            # If there is no manual return code handling, we assume this is our indicator for the Go-Smart code
            try:
                code = ErrorCode(return_code).name
            except ValueError:
                if return_code < 0:
                    code = "E_SERVER"
                else:
                    code = "E_UNKNOWN"

            message = "%s did not exit cleanly - returned %d (%s)" % (executable, return_code, self.suffix)
            if self.last_error:
                message += ": %s" % self.last_error
            self.logger.print_fatal(message, code)

        # Just in case the thread gets stuck for some reason
        thread.join(timeout=1)

        self._end_time = time.time()
        self.print_logpick(self._end_time - self._start_time)

        return return_code

    def launch(self):
        self.logger.print_line("Launching %s..." % self.suffix)

        self.logger.flush_logfile()

        absolute_path = os.path.join(self.logger.get_cwd(), self.suffix)
        try:
            os.makedirs(absolute_path)
        except OSError as e:
            if e.errno != errno.EEXIST:
                self.print_fatal("Could not create %s directory: %s" %
                                 (absolute_path, str(e)))
