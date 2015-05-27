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
import sys
import os
import unittest
import commands
import filecmp
from datetime import datetime
import re
import numpy as np
import xlwt
import matplotlib as mpl
from mpl_toolkits.mplot3d import Axes3D
import numpy as np
import matplotlib.pyplot as plt


volnear=[]
volfar=[]
volzone=[]
volcellsize=[]
vollesvol=[]
volumedev=[]
HOME=commands.getoutput('echo ${HOME}')
WORK="/Code/go-smart-data/cases/RFA/Validation-Case/"
direc="| grep ""/"" "
xlsfilename='volumedeviationperipheral.xls'
if os.path.isdir("volume-deviationperi")==0:
  commands.getoutput('mkdir volume-deviationperi')
x=commands.getoutput('ls -p '+HOME+WORK + direc)
x1=x.splitlines()[4:len(x)]
ut=commands.getoutput('more '+HOME+WORK + x1[0]+'logger/'+'*lesion.log')
print ut
for z in xrange(1,len(x1)):
  y=commands.getoutput('ls -p '+HOME+WORK + x1[z]+direc)
  for k in range(len(y.splitlines())):
    if re.findall("logger",y.splitlines()[k]):
      logger=y.splitlines()[k]
      logoutput=commands.getoutput('more '+HOME+WORK + x1[z]+logger+'*lesion.log')
      logout=logoutput.splitlines()
      if len(logout) > 2:
        for k1 in range(len(logoutput.splitlines())):
          if re.findall('cells before',logoutput.splitlines()[k1]):
            cellsize=''.join(k2 for k2 in logoutput.splitlines()[k1] if k2.isdigit())
            volcellsize.append(int(cellsize))            
          if re.findall('LVOL',logoutput.splitlines()[k1]):
            volumefind=re.findall("\d+.\d", logoutput.splitlines()[k1])
            vollesvol.append(float(volumefind[0]))
            near=x1[z][15:18]
            zone=x1[z][19:22]
            far=x1[z][23:26]
            volnear.append(float(near))
            volzone.append(float(zone))
            volfar.append(float(far))
for x in range(len(vollesvol)):
  volumedev.append((vollesvol[x]/vollesvol[0]-1)*100)
#            print cellsize,near,zone,far
#print volnear,volzone,volfar,volcellsize,vollesvol,volumedev
#print len(volnear),len(volcellsize)

def from_tst_dir(filename):
    return os.path.join(os.path.dirname(os.path.abspath(__file__)), filename)

class TestSimple(unittest.TestCase):
    def create_simple_xls(self):
        font0 = xlwt.Font()
        font0.name = 'Times New Roman'
        font0.colour_index = 4
        font0.bold = True
        font0.height = 240

        style0 = xlwt.XFStyle()
        style0.font = font0
        alignment = xlwt.Alignment() # Create Alignment 
        alignment.horz = xlwt.Alignment.HORZ_CENTER
        style0.alignment = alignment
        style1 = xlwt.XFStyle()
        style1.num_format_str = 'D-MMM-YY'


        font1 = xlwt.Font()
        font1.name = 'Times New Roman'
        font1.colour_index = 0
        font1.bold = False
        font1.height = 240
        alignment2 = xlwt.Alignment() # Create Alignment 
        alignment2.horz = xlwt.Alignment.HORZ_LEFT
        style2 = xlwt.XFStyle()
        style2.font = font1
        style2.alignment=alignment2

        wb = xlwt.Workbook()
        ws = wb.add_sheet('Lesion Simulation Details')
        ws.row(0).height = 16*20
        ws.col(4).width = 200*20
        ws.col(5).width = 200*20
        ws.col(6).width = 200*20
        ws.col(7).width = 300*20
        ws.row(1).height = 16*20
        ws.write(0, 0, 'Cases', style0)
        ws.write(0, 1, 'Near Field', style0)
        ws.write(0, 2, 'Zone Field', style0)
        ws.write(0, 3, 'Far Field', style0)
        ws.write(0, 4, 'Cell Size', style0)
        ws.write(0, 5, 'Volume Deviation', style0)
        for z in range(len(volnear)):      
          ws.row(z+2).height = 16*20
          ws.write(z+2, 0, 'Cases'+str(z), style2)
          ws.write(z+2, 1, volnear[z], style2)
          ws.write(z+2, 2, volzone[z], style2)
          ws.write(z+2, 3, volfar[z], style2)
          ws.write(z+2, 4, volcellsize[z], style2)
          ws.write(z+2, 5, volumedev[z], style2)
        wb.save(xlsfilename)



    def test_create_simple_xls(self):
        self.create_simple_xls()
        self.assertTrue(filecmp.cmp(from_tst_dir(xlsfilename),
                                    from_tst_dir(os.path.join(xlsfilename)),
                                    shallow=False))

if __name__=='__main__':
    unittest.main()
