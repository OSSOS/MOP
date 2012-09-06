#!/usr/bin/env python
import sys,re,string
import pyfits

for file_id in sys.argv:
    group=re.match(r'(.*).fits',file_id)
    if group:
        file_id=group.group(0)

    f=pyfits.open(file_id,'r')
    if f[0].header.get('SIMPLE','F')='T':
        num_ext=0
    else:
        numext=f[0].header.get('NEXTEND','1')
    extfile=str(hdu.header.get('EXPNUM'))+flag[imtype]
    print 'preproc.py --split --flip  %s' % (ext, file_id)
    extfile=file_id+"_"+
    print 'stepZjmp -f %s' % ( extfile, ) 
    print 'step0jmp -f %s -w 4.5 -t 4.0 -m 65000' % (extfile, ) 
    print 'MOPpsf.py --xbox=23 --ybox=23 --outfile=%s.psf.fits --stars=%s.bright.psf ' % (extfile, extfile )
    print 'kbo_gen.py --file=%s' % ( extfile)
    print 'MOPplant.py --psf=%s --art=objects --suffix=add %s' % (extfile+".psf.fits", extfile
            
    
        
    


