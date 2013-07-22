#!/usr/bin/env python2.7
#/*+
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

__Version__ = "2.0"
import re, os, string, sys
import vos
import numpy as np
from scipy import stats
from cStringIO import StringIO
from astropy.io import fits 

version=__Version__

elixir_header={ 'PHOT_C' : ( 30.0000, "Fake Elixir zero point" ),
                'PHOT_CS': ( 1.0000, "Fake Elixir zero point - scatter" ),
                'PHOT_NS': ( 0, 'Elixir zero point - N stars' ),
                'PHOT_NM': ( 0, 'Elixir zero point - N images' ),
                'PHOT_C0': ( 25.978, 'Elixir zero point - nominal' ),
                'PHOT_X' : ( 0.0000, 'Elixir zero point - color term' ),
                'PHOT_K' : ( -0.1000, 'Elixir zero point - airmass term' ),
                'PHOT_C1': ( 'g_SDSS', 'Elixir zero point - color 1' ),
                'PHOT_C2': ( 'r_SDSS', 'Elixir zero point - color 2')
                }



def trim(hdu, datasec='DATASEC'):
    """TRIM a CFHT MEGAPRIME frame  using the DATASEC keyword"""
    datasec = re.findall(r'(\d+)',
                         hdu.header.get(datasec))
    l=int(datasec[0])-1
    r=int(datasec[1])
    b=int(datasec[2])-1
    t=int(datasec[3])
    if opt.verbose:
        print "Trimming [%d:%d,%d:%d]" % ( l,r,b,t)
    hdu.data = hdu.data[b:t,l:r]    
    hdu.header.update('DATASEC',
                      "[%d:%d,%d:%d]" % (1,r-l+1,1,t-b+1),
                      comment="Image was trimmed")
    hdu.header.update('ODATASEC',
                      "[%d:%d,%d:%d]" % (l+1,r,b+1,t),
                      comment="previous DATASEC")
    return

def overscan(hdu,
             biassecs=['BSECA', 'BSECB'],
             ampsecs=['ASECA','ASECB']):
    """Overscan subtract the image.

    subtract the average row of biassecs from ampsecs data.


    Default for megapipe. The BIAS section keywords are expected to be
    BSEC[A|B] (for amps A and B) and the AMP sectoin is ASEC[A|B].

    """

    for idx in range(len(biassecs)):
        AMPKW= ampsecs[idx]
        BIASKW= biassecs[idx]
        dsec=re.findall(r'(\d+)',
                       hdu.header.get(AMPKW))
        if len(dsec) != 4 :
            raise ValueError(
                "Failed to parse AMPSEC: %s %s" % (AMPKW,
                                                   hdu.header.get(AMPKW)))
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

        bias = np.add.reduce(hdu.data[b:t,bl:bh],axis=1)
        bias /= float(len(hdu.data[b:t,bl:bh][0]))
        mean = bias.mean()
        hdu.data[b:t,al:ah] -= bias[:,np.newaxis]
        hdu.header.update("BIAS%d" % (idx ),mean,comment="Mean bias level")
	del(bias)

    ### send back the mean bias level subtracted
    return mean






if __name__=='__main__':
    ### Must be running as a script
    import argparse
    
    parser=argparse.ArgumentParser()
    parser.add_argument("--verbose","-v",
                      action="store_true",
                      dest="verbose",
                      help="Provide feedback on what I'm doing")
    parser.add_argument("--outfile",
                      action="store",
                      type=str,
                      dest="outfile",
                      help="name for output Master BIAS file")
    parser.add_argument("--overscan",
                      action="store_true",
                      help="Overscan subtract?")
    parser.add_argument("--trim",
                      action="store_true",
                      help="Trim to data section")
    parser.add_argument("--short",
                      action="store_true",
                      help="write files as ushort (Int16,BSCALE=1,BZERO=32768")
    parser.add_argument("--bias",
                      action="store",
                      type=str,
                      dest="bias",
                      default=None,
                      help="Bias frame for subtraction")
    parser.add_argument("--flat",
                      default=None,
                      action="store",
                      help="Flat field")
    parser.add_argument("--normal",
                      action="store_true",
                      help="Normallize before averaging?")
    parser.add_argument("--combine",
                      action="store_true",
                      help="Combine multiple images into single OUTFILE")
    parser.add_argument("--extname_kw",
                        action="store",
                        default="EXTNAME",
                        help="FITS keyword that is the extention name")
    parser.add_argument("--split",
                        action="store_true",
                        default=False,
                        help="Split MEF files into SIF on write")
    parser.add_argument("--flip",
                        action="store_true",
                        help="Flip CCDs 0 to 18?")
    parser.add_argument("--dist",
                        action="store",
                        default=None,
                        help="Distribution the output by header/chip?")
    parser.add_argument("--ccds",
                        action="store",
                        default=range(36),
                        type=int,
                        nargs='+',
                        help="CCDs to process [do all be default]")
    parser.add_argument("--dbimages",
                        action="store",
                        default="vos:OSSOS/dbimages",
                        help="VOSpace location of the dbimages directory")
    parser.add_argument("--obstype_kw",
                        action="store",
                        default="OBSTYPE",
                        help="observation type keyword")
    parser.add_argument("--file_id_kw",
                        action="store",
                        default="EXPNUM",
                        help="Keyword that uniquely identifies this exposure")
    parser.add_argument("images",
                        nargs='+',
                        help=("Images to process with give flat/bias/trim "
                              "or stack into output flat/bias"))
    parser.add_argument("--megapipe",
                        action="store_true",
                        default=False,
                        help=("Add 'dummy' ELIXIR keywords for megapipe?")
                        )
                      
    ### get the bias frames from the archive.
    (args)=parser.parse_args()
    opt=args
    file_ids = args.images

    vos_client = vos.Client()
    if opt.combine and opt.normal and False:
        ## only take one image from each pointing
        t={}
        for file_id in file_ids:
            filename = os.path.join(
                opt.dbimages,"%s/%so.head" % ( file_id, file_id))
            f = fits.open(
                StringIO(vos_client.open(filename,view='data').read()))
            t[f[0].header['OBJECT']]=file_id
            f.close()
            f = None
        file_ids=[]
        for object in t:
            file_ids.append(t[object])

    images={}
    file_names=[]
    for file_id in file_ids:
        if opt.verbose:
            print "Attempting to get and open file assocaiated with  "+str(file_id)
	if not re.match(r'.*.fits.*',file_id):
            filename=file_id+"o.fits.fz"
	else :
            filename=file_id
	if not os.access(filename,os.F_OK):
            vo_filename = os.path.join(opt.dbimages,
                                       "%s/%s" % ( file_id, filename))
            vos_client.copy(vo_filename, filename)
        if not os.access(filename,os.F_OK):
            sys.exit("Failed to get access to "+filename)
        file_names.append(filename)

    ### zero is a zero field array of the required output size.
    ### get the required output size by looking a the first input
    ### array?
    ##

    if opt.bias:
        if not os.access(opt.bias,os.F_OK):
            uri = "/".join([opt.dbimages,
                            "calibrators",
                            opt.bias])
            uri = os.path.normpath(uri)
            vos_client.copy(uri, opt.bias)
        bias=fits.open(opt.bias,"readonly")
    else:
        opt.bias=None
    if opt.flat:
        if not os.access(opt.flat,os.F_OK):
            uri = "/".join([opt.dbimages,
                            "calibrators",
                            opt.flat])
            uri = os.path.normpath(uri)
            vos_client.copy(uri, opt.flat)
        flat=fits.open(opt.flat,"readonly")
    else:
        opt.flat=None

    flag={'FLAT': 'f',
          'BIAS': 'b',
          'OBJECT': 'p',
          'ZERO': 'b'
          }
        
    ccds=args.ccds

    for ccd in ccds:
        print "Working on ccd "+str(ccd)
	stack=[]
	mstack=[]
        nim=0
        for filename in file_names:
            nim+=1
            hdu = fits.open(filename,mode='readonly',memmap=True)[int(ccd)+1]

            ### reopen the output file for each extension.
            ### Create an output MEF file based on extension name if
            ### opt.split is set.
            if not opt.outfile and not opt.combine:
                imtype=hdu.header.get('OBSTYPE')
                outfile=str(hdu.header.get('EXPNUM'))+flag[imtype]
            elif ( opt.combine or len(file_names)<2 ) and opt.outfile:
                re.match(r'(^.*)\.fits.fz',opt.outfile)
                outfile=opt.outfile
            else:
                print "\nMulitple input images with only one output"
                print "but --output option not set? [Logic Error]"
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
                
            ### do the overscan for each file
            print "Processing "+filename

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
                idx = h.argsort()
                mode = float((b[idx[-1]]+b[idx[-2]])/2.0)
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
            if args.megapipe:
                for keyword in elixir_header:
                    hdu.header.update(keyword,hdu.header.get(keyword,default=elixir_header[keyword][0]), elixir_header[keyword][1])

            if not opt.combine or len(file_names)==1:
                if opt.verbose:
                    print "writing data to "+outfile
                ### write out the image now (don't overwrite
                ### files that exist at the start of this process
                if opt.split:
		    hdu_list=fits.HDUList()
                    phdu=fits.ImageHDU(header=hdu.header,
                                       data=hdu.data)
                    
                    del phdu.header['XTENSION']
                    del phdu.header['PCOUNT']
                    del phdu.header['GCOUNT']
                    phdu.verify(option='fix')
		    fitsobj.append(phdu)
                    fitsobj.writeto(outfile)
                else:
		    if not os.access(outfile,os.R_OK):
                       pdu = fits.open(filename,
                                         mode='readonly',
                                         memmap=True)[0]
                       
                       hdul = fits.HDUList(fits.PrimaryHDU(header=pdu.header))
                       hdul.writeto(outfile)
                       hdul.close()
                    hdul = fits.open(outfile, 'append')
		    hdul.append(fits.ImageHDU(data=hdu.data,
                                              header=hdu.header))
                    if opt.short:
                        print "Scaling to interger"
                        hdul[-1].scale('int16', bzero=32768)
                    hdul.close()
                hdu = None
                hdul = None
                nim=0
            else:
                ### stack em up
                if opt.verbose:
                    print "Saving the data for later"
		data = hdu.data
	        naxis1 = hdu.data.shape[0]
	        naxis2 = hdu.data.shape[1]
                mstack.append(data)

        ### free up the memory being used by the bias and flat
        if opt.bias:
            bias[ccd+1].data=None
        if opt.flat:
            flat[ccd+1].data=None
        
        ### last image has been processed so combine the stack
        ### if this is a combine and we have more than on hdu
            
        if opt.combine and len(file_names)>1:
            if opt.verbose:
                print "Median combining "+str(nim)+" images"
            mstack = np.vstack(mstack)
            mstack.shape = [len(file_names),naxis1,naxis2]
            data = np.percentile(mstack, 40, axis=0)
            del(mstack)
            stack = fits.ImageHDU(data)
            stack.header[args.extname_kw] = (hdu.header.get(args.extname_kw,args.extname_kw), 'Extension Name')
            stack.header['QRUNID'] = (hdu.header.get('QRUNID',''), 'CFHT QSO Run flat built for')
            stack.header['FILTER'] = (hdu.header.get('FILTER',''), 'Filter flat works for')
            stack.header['DETSIZE'] = hdu.header.get('DETSIZE','')
            stack.header['DETSEC'] = hdu.header.get('DETSEC','')
            
            for im in file_names:
                hdu.header['comment'] = str(im)+" used to make this flat"
            if opt.verbose:
                print "writing median combined stack to file "+outfile
            if opt.split:
                fitsobj=fits.open(outfile,'update')
                fitsobj[0]=hdu
                if opt.short:
                    if opt.verbose:
                        print "Scaling data to ushort"
                    fitsobj[0].scale(type='int16', bzero=32768)
		fitsobj.close()
            else:
                if not os.access(outfile,os.W_OK) :
                    if opt.verbose:
                        print "Creating output image "+outfile
                    fitsobj = fits.HDUList()
                    pdu = fits.PrimaryHDU()
                    fitsobj.append(fits.PrimaryHDU())
                    fitsobj.writeto(outfile)
                    fitsobj.close()
                fitsobj=fits.open(outfile,'append')
                fitsobj.append(stack)
                if opt.short:
                    if opt.verbose:
                        print "Scaling data to ushort"
                    fitsobj[-1].scale(type='int16', bzero=32768)
                fitsobj.close()
            del(stack)
            del(hdu)
    

