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

surfdeviate=[]
surfx=[]
surfy=[]
surfnear=[]
#HOME=commands.getoutput('echo ${HOME}')
#WORK="/Code/go-smart-data/cases/RFA/Validation-Case/"
#direc="| grep ""/"" "
#if os.path.isdir("surface-deviation")==0:
#  commands.getoutput('mkdir surface-deviation')
#x=commands.getoutput('ls -p '+HOME+WORK + direc)
##print x.splitlines()[4]
#x1=x.splitlines()[4:len(x)]
#for z in range(len(x1)):
#  y=commands.getoutput('ls -p '+HOME+WORK + x1[z]+direc)
#  if len(y.splitlines())>=9:
#    if len(y.splitlines())==9:
#      t=y.splitlines()[8]
##    t1=commands.getoutput('ls '+HOME+WORK+x1[z]+t[8])
#    else:
#      t=y.splitlines()[9]
#  v=commands.getoutput('ls '+HOME+WORK+x1[z]+t)
#  v1="MUL-001-lesion_surface.vtk"
#  if v=="MUL-001-lesion_surface.vtk":
#    s=commands.getoutput('pwd')
#    source=HOME+WORK+x1[z]+t+v
#    dest=s+"/surface-deviation/"
#    copy='cp '+source+' '+dest
##    print copy
#    s1=commands.getoutput(copy)
#    move='mv '+dest+v+' '+dest+x1[z][0:len(x1[z])-1]+'-'+v
##    print move
#    s2=commands.getoutput(move)

#s=commands.getoutput('pwd')
#dest=s+"/surface-deviation/"
#s3=commands.getoutput('ls '+dest)
#s31=commands.getoutput('pwd')
#s4=s3.splitlines()
#segmented=s4[0][0:len(s4[0])-4]
##print segmented

#for z in range(3):#(len(s4)-1):
#  predicted=s4[z+1][0:len(s4[z+1])-4]
#  surf='surface-deviation '+dest+segmented+' '+dest+predicted
##  print surf
#  surface=commands.getoutput('surface-deviation '+dest+segmented+' '+dest+predicted)
#  near=surface.splitlines()[1][107:110]
#  print surface.splitlines()[1][107:110]

#    s4=commands.getoutput('cd..')
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
        ws.write_merge(0,0,5,7,'Surface Deviation',style0)
        ws.write(1,5,'Max Distance',style0)
        ws.write(1,6,'Min Distance',style0)
        ws.write(1,7,'Average Deviation',style0)
#        HOME=commands.getoutput('echo ${HOME}')
#        WORK="/Code/go-smart-data/cases/RFA/Validation-Case/"
#        count=int(commands.getoutput('ls -1 -p ${HOME}/Code/go-smart-data/cases/RFA/Validation-Case/ | grep "/" | wc -l'))-4
        HOME=commands.getoutput('echo ${HOME}')
        WORK="/Code/go-smart-data/cases/RFA/Validation-Case/"
        direc="| grep ""/"" "
        if os.path.isdir("surface-deviationperi")==0:
          commands.getoutput('mkdir surface-deviationperi')
        x=commands.getoutput('ls -p '+HOME+WORK + direc)
#        print x.splitlines()
        x1=x.splitlines()[4:len(x)]
#        x1=x2[0:len(x2)-8]
#        print x1
        for z in range(len(x1)):
          y=commands.getoutput('ls -p '+HOME+WORK + x1[z]+direc)
          for k in range(len(y.splitlines())):
            if re.findall("logger",y.splitlines()[k]):
              logger=y.splitlines()[k]
              logout=commands.getoutput('more '+HOME+WORK + x1[z]+logger+'*lesion.log')
              for k1 in range(len(logout.splitlines())):
                if re.findall('cells before',logout.splitlines()[k1]):
                  cellsize=''.join(k2 for k2 in logout.splitlines()[k1] if k2.isdigit())
#                  print cellsize
          if len(y.splitlines())>=9:
            if len(y.splitlines())==9:
              t=y.splitlines()[8]
        #    t1=commands.getoutput('ls '+HOME+WORK+x1[z]+t[8])
            else:
              t=y.splitlines()[9]
          v=commands.getoutput('ls '+HOME+WORK+x1[z]+t)
          v1="MUL-001-lesion_surface.vtk"
          if v=="MUL-001-lesion_surface.vtk":
            s=commands.getoutput('pwd')
            source=HOME+WORK+x1[z]+t+v
            dest=s+"/surface-deviationperi/"
            copy='cp '+source+' '+dest
#            print copy
            s1=commands.getoutput(copy)
            move='mv '+dest+v+' '+dest+x1[z][0:len(x1[z])-1]+'-cell'+cellsize+'cell-'+v
        #    print move
            s2=commands.getoutput(move)

        s=commands.getoutput('pwd')
        dest=s+"/surface-deviationperi/"
        s3=commands.getoutput('ls '+dest)
        s31=commands.getoutput('pwd')
        s4=s3.splitlines()
#        print s3
        segmented=s4[1][0:len(s4[1])-4]
        print "Segmented Lesion =",segmented

        for z in range((len(s4)-1)):
          predicted=s4[z+1][0:len(s4[z+1])-4]
          surf='surface-deviation '+dest+segmented+' '+dest+predicted
        #  print surf
          surface=commands.getoutput('surface-deviation '+dest+segmented+' '+dest+predicted)
          surfsplit=surface.splitlines()[1]
          surfmax=surface.splitlines()[2]
          surfmaxw=surfmax[31:len(surfmax)]

##          surfmaxw=re.findall("\d+.\d+",surfmax)
#          if re.findall("\d+.\d+e-",surfmax):
#            surfmaxw=re.findall("\d+.\d+e-\d+",surfmax)
#          elif re.findall("\d+.\d+e-",surfmax):
#            surfmaxw=re.findall("\d+.\d+e+\d+",surfmax)
#          else:
#            surfmaxw=re.findall("\d+.\d+",surfmax)

          surfmin=surface.splitlines()[5]
          surfminw=surfmin[31:len(surfmin)]
#          if re.findall("\d+.\d+e-",surfmin): 
#            surfminw=re.findall("\d+.\d+e-\d+",surfmin)
#          elif re.findall("\d+.\d+e+",surfmin):
#            surfminw=re.findall("\d+.\d+e+\d+",surfmin)
#          else:
#            surfminw=re.findall("\d+.\d+",surfmin)
          avedev=surface.splitlines()[8]
          avedevw=avedev[20:len(avedev)]
#          if re.findall("\d+.\d+e-",avedev) :
#            avedevw=re.findall("\d+.\d+e-\d+",avedev)
#          elif re.findall("\d+.\d+e-",avedev) :
#            avedevw=re.findall("\d+.\d+e+\d+",avedev)
#          else:
#            avedevw=re.findall("\d+.\d+",avedev)
#          print avedev
#          print avedevw
          surfdeviate.append(float(avedevw))
          near=surfsplit[107+4:110+4]
          surfnear.append(float(near))
#          print surfsplit
          zone=surfsplit[111+4:114+4]
          surfx.append(float(zone))
          far=surfsplit[115+4:118+4]
          surfy.append(float(far))
          cell= re.search('cell(.*)cell', surfsplit).group(1)
#          print surface.splitlines()[1][107:110]
          ws.row(z+2).height = 16*20
          ws.write(z+2, 0, 'Cases'+str(z), style2)
          ws.write(z+2, 1, float(near), style2)
          ws.write(z+2, 2, float(zone), style2)
          ws.write(z+2, 3, float(far), style2)
          ws.write(z+2, 4, float(cell), style2)
          ws.write(z+2, 5, float(surfmaxw), style2)
          ws.write(z+2, 6, float(surfminw), style2)
          ws.write(z+2, 7, float(avedevw), style2)

#          logger=commands.getoutput('more '+HOME+WORK+ )
#        print count
#        print "root prints out directories only from what you specified"
#        print "dirs prints out sub-directories from root"
#        print "files prints out all files from root and directories"
#        print "*" * 20
#        print HOME+WORK
#        for root, dirs, files in os.walk(HOME+WORK):
#            if len(dirs)>10:
#              totaldirec=dirs
##        print totaldirec
#        sorteddir=[]
#        for x in sorted(totaldirec):
#          sorteddir.append(x)
#        finaldir=sorteddir[3:len(sorteddir)-1]
#        print len(finaldir)
#        i=0
#        for x in range(len(finaldir)):
##          print finaldir[x]
##          print "case"+str(x)
#          near=finaldir[x][15:18]
#          zone=finaldir[x][19:22]
#          far=finaldir[x][23:27]
##          print near,zone,far
#          currentdir=HOME+WORK+finaldir[x]
##          print currentdir
#          commands.getoutput('cd '+currentdir)
#          currentout=currentdir+'/output'
#          if os.path.isdir(currentout):
#            currentlog=currentdir+'/logger/'
#            for root, dirs, files in os.walk(currentlog):
#              if len(files)==7:
#                t=sorted(files)[2]
#                f=open(currentlog+t)
#                lines=f.readlines()
#                if len(lines)!=19:
#                  print "reporting error in file"+currentlog
#                else:
#                  i=i+1
#                  vbef=lines[15]
#                  vaft=lines[16]
#                  vvol=lines[18]
#                  before=''.join(s for s in vbef if s.isdigit())
#                  after=''.join(k for k in vaft if k.isdigit())
#                  volume=re.findall("\d+.\d+",vvol)
##                  print volume[0]
##                  print before,after
#                  tempmin=lines[7].split()[5]
#                  tempmax=lines[7].split()[7]
#                  tempmean=lines[7].split()[9]
##                  print tempmin
#                  deadmin=lines[6].split()[5]
#                  deadmax=lines[6].split()[7]
#                  deadmean=lines[6].split()[9]
#                  tempstd=lines[13].split()[1]
#                  deadstd=lines[12].split()[1]
#                  f.close()
#                  t1=sorted(files)[3]
#                  lookup = 'exuding time'
#                  with open(currentlog+t1) as myFile:
#                      for num, line in enumerate(myFile, 1):
#                          if lookup in line:
#                              meshgentime=re.findall("\d+.\d+", line)

##                  print meshgentime
##                  print sorted(files)[0]
#                  t2=sorted(files)[0]
#                  lookup='SOLVER TOTAL TIME'
#                  with open(currentlog+t2) as myFile:
#                      for num, line in enumerate(myFile, 1):
#                          if lookup in line:
#                              comptime=re.findall("\d+.\d+", line)
#                  ws.row(i+1).height = 16*20
#                  ws.write(i+1, 0, 'Cases'+str(i), style2)
#                  ws.write(i+1, 1, float(near), style2)
#                  ws.write(i+1, 2, float(zone), style2)
#                  ws.write(i+1, 3, float(far), style2)
#                  ws.write(i+1, 4, float(volume[0]), style2)
#                  ws.write(i+1, 5, int(before), style2)
#                  ws.write(i+1, 6, int(after), style2)
#                  ws.write(i+1, 7, float(meshgentime[0]), style2)
#                  ws.write(i+1, 8, float(comptime[0]), style2)
#                  ws.write(i+1, 9, float(meshgentime[0])+float(comptime[0]), style2)
#                  ws.write(i+1, 10, float(tempmin), style2)
#                  ws.write(i+1, 11, float(tempmax), style2)
#                  ws.write(i+1, 12, float(tempmean), style2)
#                  ws.write(i+1, 13, float(tempstd), style2)
#                  ws.write(i+1, 14, float(deadmin), style2)
#                  ws.write(i+1, 15, float(deadmax), style2)
#                  ws.write(i+1, 16, float(deadmean), style2)
#                  ws.write(i+1, 17, float(deadstd), style2)



#                  print comptime[0]

#                  f=open(currentlog+t1)
#                  lines=f.readlines()
#                  strings=("exuding time")
#                  if any(s in l for l in lines for s in strings):
#                    print len(lines)
#            if os.path.exists(currentdir+'/logger/*.log'):
#             print "hey",commands.getoutput('ls '+currentdir+'/logger/')
#        ws.write(1, 0, datetime(2010, 12, 5), style1)
#        ws.write(2, 0, 1)
#        ws.write(2, 1, 1)
#        ws.write(2, 2, xlwt.Formula("A3+B3"))
        ws.write(3,8,'segmented lesion',style0)
        ws.col(8).width = 300*20
        wb.save('surfacedeviationperipheral.xls')
        ss=np.array(surfdeviate)
        print "The best deviation lesion is ",s4[np.argmin(ss)]
        
#        mpl.rcParams['legend.fontsize'] = 10

        fig = plt.figure()
        ax = fig.gca(projection='3d')
        ax.plot(surfx[1:15], surfy[1:15], surfdeviate[1:15], label='Average Deviation with Near Field=0.6')
        ax.set_xlabel('Zone Field')
        ax.set_ylabel('Far Field')
        ax.set_zlabel('Average Deviation')
        ax.set_title('Average Deviation for Near Field = 0.6')
        plt.savefig('surfdevperipheral1.png')
        plt.show()

        fig = plt.figure()
        ax = fig.gca(projection='3d')
        ax.plot(surfx[16:51], surfy[16:51], surfdeviate[16:51], label='Average Deviation with Near Field=1.1')
        ax.set_xlabel('Zone Field')
        ax.set_ylabel('Far Field')
        ax.set_zlabel('Average Deviation')
        ax.set_title('Average Deviation for Near Field = 1.1')
        plt.savefig('surfdevperipheral2.png')
        plt.show()


        fig = plt.figure()
        ax = fig.gca(projection='3d')
        ax.plot(surfx[52:77], surfy[52:77], surfdeviate[52:77], label='Average Deviation with Near Field=1.6')
        ax.set_xlabel('Zone Field')
        ax.set_ylabel('Far Field')
        ax.set_zlabel('Average Deviation')
        ax.set_title('Average Deviation for Near Field = 1.6')
        plt.savefig('surfdevperipheral3.png')
        plt.show()


        fig = plt.figure()
        ax = fig.gca(projection='3d')
        ax.plot(surfx[78:92], surfy[78:92], surfdeviate[78:92], label='Average Deviation with Near Field=1.7')
        ax.set_xlabel('Zone Field')
        ax.set_ylabel('Far Field')
        ax.set_zlabel('Average Deviation')
        ax.set_title('Average Deviation for Near Field = 1.7')
        plt.savefig('surfdevperipheral4.png')
        plt.show()


    def test_create_simple_xls(self):
        self.create_simple_xls()
        self.assertTrue(filecmp.cmp(from_tst_dir('surfacedeviationperipheral.xls'),
                                    from_tst_dir(os.path.join('surfacedeviationperipheral.xls')),
                                    shallow=False))

if __name__=='__main__':
    unittest.main()
