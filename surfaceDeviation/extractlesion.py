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
# Importing Sections
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
# end of importing sections


# This program works (with minor modifications on WORK) only if the following conditions are satisfied
#   Your directory where the lesion file resides should have the following format
#     MUL-001-model${number}-${near}-${zone}-${far}
#     The lesion file should be MUL-001-lesion_surface.vtk
#     
#     or simply if you used numavalidation.sh and a change in the working directory @WORK
surfdeviate=[]
surfx=[]
surfy=[]
xlsfilename='surfacedeviationcentral.xls'
surfdirec="surface-deviationcent" #change the directory name as you wish say DIREC
HOME=commands.getoutput('echo ${HOME}')
WORK="/Code/go-smart-data/cases/RFA/Validation-CaseRelease/"  # Please change this as per the location of your files
direc="| grep ""/"" "
v1="MUL-001-lesion_surface.vtk" #name of the lesion file        
def from_tst_dir(filename):
    return os.path.join(os.path.dirname(os.path.abspath(__file__)), filename)

class TestSimple(unittest.TestCase):
    def create_simple_xls(self):
#fonts for the numbers
        font0 = xlwt.Font()
        font0.name = 'Times New Roman'
        font0.colour_index = 4
        font0.bold = True
        font0.height = 240
#style for the xls file
        style0 = xlwt.XFStyle()
        style0.font = font0
        alignment = xlwt.Alignment() # Create Alignment 
        alignment.horz = xlwt.Alignment.HORZ_CENTER
        style0.alignment = alignment
#font style for the text

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
#creating excel workbook and their column title
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
        ws.write_merge(0,0,5,7,'Surface Deviation',style0)
        ws.write(1,5,'Max Distance',style0)
        ws.write(1,6,'Min Distance',style0)
        ws.write(1,7,'Average Deviation',style0)
# Home directory        

          
        if os.path.isdir(surfdirec)==0: 
          commands.getoutput('mkdir ' + surfdirec)
        x=commands.getoutput('ls -p '+HOME+WORK + direc)
        x2=x.splitlines()[4:len(x)]
        x1=x2[0:len(x2)-8]
        for z in range(len(x1)):
          y=commands.getoutput('ls -p '+HOME+WORK + x1[z]+direc)
          for k in range(len(y.splitlines())):
            if re.findall("logger",y.splitlines()[k]):
              logger=y.splitlines()[k]
              logout=commands.getoutput('more '+HOME+WORK + x1[z]+logger+'*lesion.log')
              for k1 in range(len(logout.splitlines())):
                if re.findall('cells before',logout.splitlines()[k1]):
                  cellsize=''.join(k2 for k2 in logout.splitlines()[k1] if k2.isdigit())
          if len(y.splitlines())>=9:
            if len(y.splitlines())==9:
              t=y.splitlines()[8]
            else:
              t=y.splitlines()[9]
          v=commands.getoutput('ls '+HOME+WORK+x1[z]+t)

          if v==v1:
            s=commands.getoutput('pwd')
            source=HOME+WORK+x1[z]+t+v
            dest=s+"/" + surfdirec+"/" 
            copy='cp '+source+' '+dest
            s1=commands.getoutput(copy)
            move='mv '+dest+v+' '+dest+x1[z][0:len(x1[z])-1]+'-cell'+cellsize+'cell-'+v
            s2=commands.getoutput(move)

        s=commands.getoutput('pwd')
        dest=s+"/" + surfdirec+"/"
        s3=commands.getoutput('ls '+dest)
        s31=commands.getoutput('pwd')
        s4=s3.splitlines()
        segmented=s4[0][0:len(s4[0])-4] #it takes the first lesion file as segmented lesion. If you want to give different lesion, rename it
        print "Segmented Lesion =",segmented

        for z in range((len(s4)-1)):
          predicted=s4[z+1][0:len(s4[z+1])-4]
          surf='surface-deviation '+dest+segmented+' '+dest+predicted⇒  cd ..
buildtools/      elmerparam/  mathlibs/          utils/
cmake/           fem/         mesher-cgal/       volumedeviation/
eio/             front/       meshgen2d/         CMakeLists.txt
elmergrid/       hutiter/     misc/              CMakeLists.txt.orig
ElmerGUI/        launcher/    post/              cscope.files
ElmerGUIlogger/  lesion/      surfaceDeviation/  LICENSES
ElmerGUItester/  logger/      third-party/       trilinos_include.patch
elmerice/        matc/        umfpack/

          surface=commands.getoutput('surface-deviation '+dest+segmented+' '+dest+predicted)
          surfsplit=surface.splitlines()[1]
          surfmax=surface.splitlines()[2]
          surfmaxw=surfmax[31:len(surfmax)]
          surfmin=surface.splitlines()[5]
          surfminw=surfmin[31:len(surfmin)]
          avedev=surface.splitlines()[8]
          avedevw=avedev[20:len(avedev)]
          surfdeviate.append(float(avedevw))
          near=surfsplit[107+4:110+4] 
          zone=surfsplit[111+4:114+4]
          far=surfsplit[115+4:118+4]
          surfx.append(float(zone))
          surfy.append(float(far))
          cell= re.search('cell(.*)cell', surfsplit).group(1)
          ws.row(z+2).height = 16*20
          ws.write(z+2, 0, 'Cases'+str(z), style2)
          ws.write(z+2, 1, float(near), style2)
          ws.write(z+2, 2, float(zone), style2)
          ws.write(z+2, 3, float(far), style2)
          ws.write(z+2, 4, float(cell), style2)
          ws.write(z+2, 5, float(surfmaxw), style2)
          ws.write(z+2, 6, float(surfminw), style2)
          ws.write(z+2, 7, float(avedevw), style2)



        wb.save(xlsfilename)
        ss=np.array(surfdeviate)
        print "The best deviation lesion is ",s4[np.argmin(ss)]
        
# Please uncomment these sections and modify them if you want to plot the file 
#        mpl.rcParams['legend.fontsize'] = 10

#        fig = plt.figure()
#        ax = fig.gca(projection='3d')
#        ax.plot(surfx, surfy, surfdeviate, label='Average Deviation for Near Field=0.1')
#        ax.set_xlabel('Zone Field')
#        ax.set_ylabel('Far Field')
#        ax.set_zlabel('Average Deviation')
#        ax.set_title('Average Deviation for Near Field=0.1')
#        plt.savefig('surfdevcentral.png')
#        plt.show()

    def test_create_simple_xls(self):
        self.create_simple_xls()
        self.assertTrue(filecmp.cmp(from_tst_dir(xlsfilename),
                                    from_tst_dir(os.path.join(xlsfilename)),
                                    shallow=False))

if __name__=='__main__':
    unittest.main()
