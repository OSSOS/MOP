#!/usr/bin/env python
#*
#*   RCS data:
#*  $RCSfile: search.py,v $
#*  $Revision: 1.9 $
#*  $Date: 2007/05/15 19:40:19 $
#*
#*   Programmer     : JJ Kavelaars
#*
#*   Modification History:
#*
#*   Overhauled from 2013/03/13 by Michele Bannister
#*
#****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
#*
# Run J-M.'s and Matt's object finding systems... then intersect the 
# result.  


def scrambleTriples(expnums,ccd):
    """Pull the three images and then scramble the MJD-OBS keywords"""
    import pyfits, MOPfits
    mjd=[]
    fid=[]
    fs=[]
    filenames=[]
    for expnum in expnums:
        if int(ccd)<18:
            cutout="[-*,-*]"
        else:
            cutout="[*,*]"
        filenames.append(MOPfits.adGet(str(expnum)+opt.raw,extno=int(ccd),cutout=cutout))
	fs.append(pyfits.open(filenames[-1]))
	mjd.append(fs[-1][0].header.get("MJD-OBS"))
	fid.append(fs[-1][0].header.get("EXPNUM"))

    if not os.access('weight.fits',os.F_OK):
        os.symlink(MOPfits.adGet("weight",extno=int(ccd),cutout=cutout),'weight.fits')

    for filename in filenames:
        if not os.access(filename,os.R_OK):
            sys.stderr.write("Ad Get Failed\n")
            raise TaskError, 'adGet Failed'
	    
    order=[2,0,1]
    basenames=[]
    for i in range(len(fs)):
       fs[i][0].header.update("MJD-OBS",mjd[order[i]]);
       fs[i][0].header.update("EXPNUM",fid[order[i]]);
       filename=filenames[order[i]].replace(opt.raw,'s')
       basenames.append(os.path.splitext(filename)[0])
       if os.access(filename,os.F_OK):
          os.unlink(filename)
       fs[i][0].writeto(filename,output_verify='ignore')
       os.unlink(filenames[order[i]])

    return basenames


def getTriples(expnums,ccd):
    """Pull the three images and then scramble the MJD-OBS keywords"""
    import pyfits, MOPfits
    filenames=[]
    for expnum in expnums:
        if int(ccd)<18:
            cutout="[-*,-*]"
        else:
            cutout="[*,*]"
        filenames.append(MOPfits.adGet(str(expnum)+opt.raw,extno=int(ccd),cutout=cutout))

    for filename in filenames:
        if not os.access(filename,os.R_OK):
            sys.stderr.write("Ad Get Failed\n")
            raise TaskError, 'adGet Failed'
        
    if not os.access('weight.fits',os.F_OK):
        os.symlink(MOPfits.adGet("weight",extno=int(ccd),cutout=cutout),'weight.fits')
    
    basenames=[]
    for filename in filenames:
       filename=os.path.splitext(filename)
       basenames.append(filename[0])
       
    print filenames, basenames
    return basenames
