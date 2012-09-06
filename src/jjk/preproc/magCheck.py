
import os

header="""## X  Y   PMAG  RATE ANGLE"""

cwd=os.getcwd()
for found in file('found.list'):
    dirname=os.path.dirname(found)
    filename=os.path.basename(found).strip()
    os.chdir(dirname)
    outfile=file(filename+".obj",'w')
    outfile.write(header+"\n")
    outfile.write(file(filename).read())
    outfile.close()
    print os.getcwd()
    v=file("aper.corr").readline().split()
    print v
    image = filename.split('.')[0]
    cmd="""daophot.py --input=%s.obj --output=%s.phot --aperture=%s --sky=%d --swidth=5 --apcor=%s %s.fits""" % (filename,filename,v[0],float(v[1])+2,v[2],image)
    print cmd
    os.system(cmd)
    os.chdir(cwd)
        

