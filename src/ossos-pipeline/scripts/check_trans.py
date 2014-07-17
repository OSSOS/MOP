
import sys, math
from ossos import storage
fobj = storage.open_vos_or_local('L.txt')
lines = fobj.read().split('\n')
fobj.close()
thres = 30
for line in lines:
   field = line.split()[-1]
   for ccd in range(35,36):
      rfailed = False
      ffailed = False
      for exp in line.split(' ')[0:3]:
         try:
            trans = storage.get_trans(exp, ccd) 
            if math.fabs(trans['dx']) > thres or math.fabs(trans['dy']> thres):
               rfailed = True
               sys.stderr.write("{:10} {:10} {:4} {}\n".format(field, exp, ccd, str(trans)))
         except Exception as e:
            sys.stderr.write(str(e)+"\n")
            rfailed = True
            pass
         try:
            trans = storage.get_trans(exp, ccd, version='s', prefix='fk')
            if math.fabs(trans['dx']) > thres or math.fabs(trans['dy']> thres ):
               ffailed = True
               sys.stderr.write("{:10} {:10} {:4} {}\n".format(field, exp, ccd, str(trans)))
         except Exception as e:
            sys.stderr.write(str(e)+"\n")
            ffailed = True
            pass
      sys.stdout.write("{} {:4} {:10} {:10}\n".format(line, ccd, rfailed, ffailed))
      sys.stdout.flush()
