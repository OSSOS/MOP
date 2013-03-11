#!/usr/bin/env python
#/*+
#************************************************************************
#****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
#*
#* (c) 2004.				(c) 2004.
#* National Research Council		Conseil national de recherches
#* Ottawa, Canada, K1A 0R6 		Ottawa, Canada, K1A 0R6
#* All rights reserved			Tous droits reserves
#* 					
#* NRC disclaims any warranties,	Le CNRC denie toute garantie
#* expressed, implied, or statu-	enoncee, implicite ou legale,
#* tory, of any kind with respect	de quelque nature que se soit,
#* to the software, including		concernant le logiciel, y com-
#* without limitation any war-		pris sans restriction toute
#* ranty of merchantability or		garantie de valeur marchande
#* fitness for a particular pur-	ou de pertinence pour un usage
#* pose.  NRC shall not be liable	particulier.  Le CNRC ne
#* in any event for any damages,	pourra en aucun cas etre tenu
#* whether direct or indirect,		responsable de tout dommage,
#* special or general, consequen-	direct ou indirect, particul-
#* tial or incidental, arising		ier ou general, accessoire ou
#* from the use of the software.	fortuit, resultant de l'utili-
#* 					sation du logiciel.
#*
#************************************************************************
#*
#*   Script Name:	preproc.py
#*
#*   Purpose:
#*	Do image preprocessing for CFHT MEGAPRIME images
#*
#*   Functions:
#+	overscan        : overscan subtract the 2 amps ofMEGAPRIME CCDs
#+	trim            : Trim off the overscan region
#*	
#*
#*
#*   CVS data:
#*	$Header: /home/observe/cvsroot/MOP/src/jjk/preproc/preproc.py,v 1.7 2007/01/26 20:20:24 observe Exp $
#*
#*   Initiated by       : JJ Kavelaars
#*   Date		: <Nov 30 2004>
#*
#*   Modification History:
#*   $Log $
#*
#****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
#************************************************************************
#-*/


"""Create a MEGAPRIME bias frame given a list of input bias exposure numbers"""

__Version__ = "$Revision: 1.7 $"
import re, os, string, sys
import numpy as np
import numpy.ma as MaskArray
from scipy import stats
import MOPfits
import pyfits

version=re.match(r'\$Rev.*: (\d*.\d*) \$',__Version__).group(1)

def trim(hdu):
    """TRIM a CFHT MEGAPRIME frame  using the DATASEC keyword"""
    datasec = re.findall(r'(\d+)',
                         hdu.header.get('DATASEC'))
    l=int(datasec[0])-1
    r=int(datasec[1])
    b=int(datasec[2])-1
    t=int(datasec[3])
    if opt.verbose:
        print "Trimming [%d:%d,%d:%d]" % ( l,r,b,t)
    hdu.data = hdu.data[b:t,l:r]    
    hdu.header['DATASEC']="[%d:%d,%d:%d]" % (1,r-l+1,1,t-b+1)
    return

def overscan(hdu):
    """Overscan subtract a CFHT MEGAPRIME frame.
    oversan(hdu) --> status

    The BIAS section keywords are expected to be BSEC[A|B] (for amps
    A and B) and the AMP sectoin is ASEC[A|B].
    """

    for amp in (['A','B']):
        AMPKW= 'ASEC'+amp
        BIASKW= 'BSEC'+amp
        dsec=re.findall(r'(\d+)',
                       hdu.header.get(AMPKW))
        bias=re.findall(r'(\d+)',
                        hdu.header.get(BIASKW))
        
        ## the X-boundaries set the amp section off from the bias section
        ## (ie. this is a column orriented device)
        al=int(dsec[0])-1
        ah=int(dsec[1])
        bl=int(bias[0])-1
        bh=int(bias[1])
        ### the Y directions must match or the array math fails
        ## b == Bottom
        ## t == Top
        b=max(int(bias[2]),int(dsec[2]))-1
        t=min(int(bias[3]),int(dsec[3]))

        bias = np.add.reduce(hdu.data[b:t,bl:bh],axis=1)/float(len(hdu.data[b:t,bl:bh][0]))
        mean = bias.mean()
        hdu.data[b:t,al:ah] -= bias[:,np.newaxis]
        hdu.header.update("BIAS",mean,comment="Mean bias level")

    ### send back the mean bias level subtracted
    return mean






if __name__=='__main__':
    ### Must be running as a script
    import optparse
    from optparse import OptionParser
    
    parser=OptionParser()
    parser.add_option("--verbose","-v",
                      action="store_true",
                      dest="verbose",
                      help="Provide feedback on what I'm doing")
    parser.add_option("--outfile",
                      action="store",
                      type="string",
                      dest="outfile",
                      help="name for output Master BIAS file")
    parser.add_option("--overscan",
                      action="store_true",
                      help="Overscan subtract?")
    parser.add_option("--trim",
                      action="store_true",
                      help="Trim to data section")
    parser.add_option("--short",
                      action="store_true",
                      help="write files as ushort (Int16,BSCALE=1,BZERO=32768")
    parser.add_option("--bias",
                      action="store",
                      type="string",
                      dest="bias",
                      default=None,
                      help="Bias frame for subtraction [leave out if you don't want to bias subtract your inputs")
    parser.add_option("--flat",
                      default=None,
                      action="store",
                      help="Flat field [leave out if you don't want to flatten your inputs]")

    parser.add_option("--normal",
                      action="store_true",
                      help="Normallize before averaging?")
    parser.add_option("--combine",
                      action="store_true",
                      help="Combine multiple images into single OUTFILE")
    parser.add_option("--split",
                      action="store_true",
                      help="Split the input MEF into single chips")
    parser.add_option("--flip",
                      action="store_true",
                      help="Flip CCDs 0 to 18?")
    parser.add_option("--dist",
                      action="store",
		      default=None,
                      help="Distribution the output by header/chip?")
    parser.add_option("--ccd",
                      action="store",
		      default=36,
                      type="int",
                      help="CCD to process [do all be default]")
                      
    ### get the bias frames from the archive.
    (opt, file_ids)=parser.parse_args()






    if opt.combine and opt.normal and 0==1:
        ## only take one image from each pointing
        t={}
        for file_id in file_ids:
            f=pyfits.open(file_id)
            t[f[0].header['OBJECT']]=file_id
            f.close()
        file_ids=[]
        for object in t:
            file_ids.append(t[object])


    images={}
    file_names=[]
    for file_id in file_ids:
	if not re.match(r'.*.fits',file_id):
        	file_name=file_id+".fits"
	else :
		file_name=file_id
        if opt.verbose:
            print "Attempting to get and open "+file_name
	if not os.access(file_name,os.F_OK):
            file_names.append(MOPfits.adGet(file_id))
        if not os.access(file_name,os.F_OK):
            sys.exit("Failed to get access to "+file_name)
        images[file_id]=pyfits.open(file_name,"readonly")

    ### zero is a zero field array of the required output size.
    ### get the required output size by looking a the first input
    ### array?
    ##

    if opt.bias and os.access(opt.bias,os.F_OK):
        bias=pyfits.open(opt.bias,"readonly")
    else:
        opt.bias=None
    if opt.flat and os.access(opt.flat,os.F_OK):
        flat=pyfits.open(opt.flat,"readonly")
    else:
        opt.flat=None

    flag={'FLAT': 'f',
          'BIAS': 'b',
          'OBJECT': 'p',
          'ZERO': 'b'
          }
        
    if opt.ccd!=36:
        if opt.ccd > 36:
            sys.exit("Bad CCD number")
        ccds=[opt.ccd]
    else:
        ccds=range(36)

    for ccd in ccds:
        if opt.verbose:
            print "Working on ccd "+str(ccd)
	stack=[]
        nim=0
        for image in images:
            nim+=1
            hdu=images[image][int(ccd)+1]

            ### reopen the output file for each extension.
            ### Create an output MEF file based on extension name if
            ### opt.split is set.
            if not opt.outfile and not opt.combine:
                imtype=hdu.header.get('OBSTYPE')
                outfile=str(hdu.header.get('EXPNUM'))+flag[imtype]
            elif ( opt.combine or len(images)<2 ) and opt.outfile:
                re.match(r'(^.*)\.fits',opt.outfile)
                outfile=opt.outfile
            else:
                print "\nMulitple input images with only one output"
                print "but --combine option not set? [Logic Error]"
                sys.exit(-1)
            subs="."
            if opt.dist:
                subs=opt.dist
                object=hdu.header.get('OBJECT')
                nccd=hdu.header.get('EXTNAME')
                for dirs in [nccd, object]:
                    subs = subs+"/"+dirs
                    if not os.access(subs,os.F_OK):
                        os.makedirs(subs)
            subs=subs+"/"
            if opt.split:
                nccd=hdu.header.get('EXTVER')
                outfile=outfile+string.zfill(str(nccd),2)
            outfile=subs+outfile+".fits"
            ### exit if the file exist and this is the ccd or
            ### were splitting so every file should only have one
            ### extension
            if os.access(outfile,os.W_OK) and (ccd==0 or opt.split) and not opt.combine:
                sys.exit("Output file "+outfile+" already exists")
            elif not os.access(outfile,os.W_OK) and not opt.split:
                if opt.verbose:
                    print "Creating output image "+outfile
                fitsobj = pyfits.HDUList()
                fitsobj.append(images[image][0])
                fitsobj.writeto(outfile)
                fitsobj.close
                
            ### do the overscan for each file
            if opt.verbose:
                print "Processing "+image

            if opt.overscan:
                if opt.verbose:
                    print "Overscan subtracting"
                overscan(hdu)
            if opt.bias:
                if opt.verbose:
                    print "Subtracting bias frame "+opt.bias
                hdu.data -= bias[ccd+1].data
            if opt.trim:
                if opt.verbose:
                    print "Triming image"
                trim(hdu)
            if opt.flat:
                if opt.verbose:
                    print "Dividing by flat field "+opt.flat
                hdu.data /= flat[ccd+1].data
                hdu.header.update("Flat",opt.flat,comment="Flat Image")
            if opt.normal:
                if opt.verbose:
                    print "Normalizing the frame"
                (h, b) = np.histogram(hdu.data,bins=1000)
                idx = h.argmax()
                mode = (b[idx]+b[idx+1])/2.0
                hdu.data = hdu.data/mode
            if opt.flip:
	        if ccd < 18 :
                    if opt.verbose:
                        print "Flipping the x and y axis"
                    hdu.data = hdu.data[::-1,::-1]
                    hdu.header['CRPIX2']= hdu.data.shape[0] - hdu.header['CRPIX2'] 
                    hdu.header['CRPIX1'] = hdu.data.shape[1] - hdu.header['CRPIX1']
                    hdu.header['CD1_1']=-1.0*hdu.header['CD1_1']
                    hdu.header['CD2_2']=-1.0*hdu.header['CD2_2']
                                                                 
            hdu.header.update('CADCPROC',float(version),
                              comment='Version of cadcproc')
            ### write out this image if not combining
            if not opt.combine or len(images)==1:
                if opt.short:
                    if opt.verbose:
                        print "Scaling data to ushort"
                    hdu.scale(type='int16',bscale=1,bzero=32768)

                if opt.verbose:
                    print "writing data to "+outfile
                ### write out the image now (don't overwrite
                ### files that exist at the start of this process
                if opt.split:
		    fitsobj=pyfits.HDUList()
                    phdu=pyfits.PrimaryHDU()
                    phdu.header=hdu.header
                    phdu.data=hdu.data
                    del phdu.header['XTENSION']
                    del phdu.header['PCOUNT']
                    del phdu.header['GCOUNT']
                    phdu.verify(option='fix')
		    fitsobj.append(phdu)
                    fitsobj.writeto(outfile)
                    #phdu.close()
                else:
                    fitsobj=pyfits.open(outfile,'append')
                    fitsobj.append(hdu)
                    fitsobj.close()
                nim=0
            else:
                ### stack em up
                if opt.verbose:
                    print "Saving the data for later"
                stack.append(hdu)

        ### free up the memory being used by the bias and flat
        if opt.bias:
            bias[ccd+1].data=None
        if opt.flat:
            flat[ccd+1].data=None
        
        ### last image has been processed so combine the stack
        ### if this is a combine and we have more than on hdu
            
        if opt.combine and len(images)>1:
            import bottleneck as bn
            if opt.verbose:
                print "Median combining "+str(nim)+" images"
            mstack = []
            for im in stack:
            #    print "computing clipping for %s" % ( im.header['EXPNUM'])
            #    gain = im.header.get("GAINA",1.5)
            #    im_med = np.median(im.data)
            #    std = np.sqrt(im_med)/gain
            #    im.data[(np.fabs(im.data - im_med)/std) > 2.0] = np.nan
                mstack = np.append(mstack, im.data)
            print mstack.shape
            mstack.shape = [len(stack),stack[0].data.shape[0],stack[0].data.shape[1]]
            print "Computing 40th Percentile"
            #hdu.data = bn.nanmean(mstack, axis=0)
            hdu.data = np.percentile(mstack, 34, axis=0)            
            if opt.short:
                if opt.verbose:
                    print "Scaling data to ushort"
                hdu.scale(type='Int16',bscale=1,bzero=32768)
            if opt.verbose:
                print "writing median combined stack to file "+outfile
            if opt.split:
                fitsobj=pyfits.open(outfile,'update')
                fitsobj[0]=hdu
            else:
                fitsobj=pyfits.open(outfile,'append')
                fitsobj.append(hdu)
            fitsobj.close()
            mstack = None
            stack = None
            im = None
            hdu.data = None
            for i in stack:
                i=None
    
#    for file_name in file_names:
#        os.unlink(file_name)

