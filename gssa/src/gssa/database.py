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
import sqlite3
import traceback
import sys
import os

from .definition import GoSmartSimulationDefinition


# SQLite database for storing information about active databases
class SQLiteSimulationDatabase:
    def __init__(self, database):
        # If we do not currently have a database file, we should make one
        should_create = not os.path.exists(database)

        self._db = sqlite3.connect(database)
        self._db.row_factory = sqlite3.Row

        if should_create:
            self.create()

    # Add the validation XML string to the simulation row
    def updateValidation(self, guid, validation_xml):
        cursor = self._db.cursor()
        cursor.execute('''
            UPDATE simulations
            SET validation=:validation
            WHERE guid=:guid
        ''', {'guid': guid, 'validation': validation_xml})
        self._db.commit()

    # Return just the validation XML string for a simulation
    def getValidation(self, guid):
        cursor = self._db.cursor()
        cursor.execute('''
            SELECT validation
            FROM simulations
            WHERE guid=? AND deleted=0
        ''', guid)
        try:
            simulation_row = cursor.fetchone()
        except Exception:
            return None

        validation = simulation_row[0]
        return validation

    # Update the status of a simulation in the database
    def setStatus(self, guid, exit_code, status, percentage, timestamp):
        cursor = self._db.cursor()
        cursor.execute('''
            UPDATE simulations
            SET exit_code=:exit_code, status=:status, percentage=:percentage, timestamp=:timestamp
            WHERE guid=:guid
            ''', {"guid": guid, "status": status, "percentage": percentage, "exit_code": exit_code, "timestamp": timestamp})
        self._db.commit()

    # Return both status and validation
    def getStatusAndValidation(self, guid):
        cursor = self._db.cursor()
        cursor.execute('''
            SELECT status, percentage, exit_code, timestamp, validation
            FROM simulations
            WHERE guid=? AND deleted=0
            ''', guid)
        try:
            simulation_row = cursor.fetchone()
        except Exception:
            return None

        status, percentage, exit_code, timestamp, validation = simulation_row
        return percentage, status, exit_code, timestamp, validation

    # Set up the database
    def create(self):
        cursor = self._db.cursor()
        cursor.execute('''
            CREATE TABLE simulations(
                id INTEGER PRIMARY KEY,
                guid TEXT UNIQUE,
                directory TEXT,
                exit_code TEXT NULLABLE DEFAULT NULL,
                status TEXT,
                percentage REAL,
                timestamp REAL,
                validation TEXT,
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                deleted TINYINT DEFAULT 0
                )
        ''')
        self._db.commit()

    # Update the simulation row or add a new one if not already here
    def addOrUpdate(self, simulation):
        try:
            cursor = self._db.cursor()
            cursor.execute('''
                INSERT OR REPLACE INTO simulations(guid, directory)
                VALUES(:guid,:directory)
            ''', {"guid": simulation.get_guid(), "directory": simulation.get_dir()})
            self._db.commit()
        except Exception:
            traceback.print_exc(file=sys.stderr)

    # Mark simulations as exited if still appearing to run - usually on server start-up
    def markAllOld(self):
        cursor = self._db.cursor()
        cursor.execute('''
            UPDATE simulations
            SET status=('Unfinished (' || percentage || '%)'), percentage=0, exit_code='E_UNKNOWN'
            WHERE percentage IS NOT NULL AND percentage < 100
        ''')
        self._db.commit()

    # Check how many simulations are still marked IN_PROGRESS
    def active_count(self):
        cursor = self._db.cursor()
        cursor.execute('''
            SELECT COUNT(id) as active
            FROM simulations
            WHERE status="IN_PROGRESS"
        ''')
        return cursor.fetchone()['active']

    def all(self):
        cursor = self._db.cursor()
        cursor.execute('''
            SELECT *
            FROM simulations
        ''')

        simulations = cursor.fetchall()
        return simulations

    # Get a simulation by the client's GUID
    def retrieve(self, guid):
        cursor = self._db.cursor()
        cursor.execute('''
            SELECT *
            FROM simulations
            WHERE guid=? AND deleted=0
        ''', guid)
        try:
            simulation_row = cursor.fetchone()
        except Exception:
            return None

        directory = simulation_row['directory']

        if not os.path.exists(directory):
            return None

        # Simulations should not be added to the database until they are finalized
        return GoSmartSimulationDefinition(guid, directory, finalized=True)

    def delete(self, simulation, soft=True):
        cursor = self._db.cursor()
        if soft:
            cursor.execute('''
                UPDATE simulations
                SET deleted=1
                WHERE guid=?
            ''', simulation.get_guid())
        else:
            cursor.execute('''
                DELETE FROM simulations
                WHERE guid=?
            ''', simulation.get_guid())

    def __del__(self):
        # Tidy up before we leave
        self._db.close()
