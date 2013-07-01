#!/usr/bin/env python2.7
"""do the standard pre-processing steps associated with CCD imaging"""
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



__Version__ = "2.0"
import re, os, string, sys
import numpy as np
from astropy.io import fits
import logging
import errno

version=__Version__

file_type_flags={'FLAT': 'f',
                 'BIAS': 'b',
                 'OBJECT': 'p',
                 'ZERO': 'b'
                 }


elixir_header={ 'PHOT_C' : (30.0000 , "Fake Elixir zero point"),
                'PHOT_CS': (1.0000 , "Fake Elixir zero point - scatter" ) ,
                'PHOT_NS': (0, 'Elixir zero point - N stars' ),
                'PHOT_NM' : (0, 'Elixir zero point - N images'),
                'PHOT_C0' : ( 25.978, 'Elixir zero point - nominal'),
                'PHOT_X'  :             (  0.0000 ,'Elixir zero point - color term'),
                'PHOT_K'  :             ( -0.1000 , 'Elixir zero point - airmass term'),
                'PHOT_C1' : ('g_SDSS            ' , 'Elixir zero point - color 1'),
                'PHOT_C2' : ('r_SDSS            ' , 'Elixir zero point - color 2')
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
    hdu.header.update('DATASEC',"[%d:%d,%d:%d]" % (1,r-l+1,1,t-b+1), comment="Image was trimmed")
    hdu.header.update('ODATASEC',"[%d:%d,%d:%d]" % (l+1,r,b+1,t), comment="previous DATASEC")
    return

def overscan(hdu,
             biassecs=['BSECA', 'BSECB'],
             ampsecs=['ASECA','ASECB']):
    """Overscan subtract the image.

    subtract the average row of biassecs from ampsecs data.


    Default for megapipe. The BIAS section keywords are expected to be BSEC[A|B] (for amps
    A and B) and the AMP sectoin is ASEC[A|B].
    """
    if len(biassecs) != len(ampssec):
        raise ValueError("different number of bias and amp sections provided")

    for indx in range(len(biassecs)):
        AMPKW= ampsecs[idx]
        BIASKW= biassecs[idx]
        dsec=re.findall(r'(\d+)',
                       hdu.header.get(AMPKW))
        if len(dsec) != 4 :
            raise ValueError("Failed to parse AMPSEC: %s %s" % (AMPKW,
                                                                hdu.header.get(AMPKW)))
        bias=re.findall(r'(\d+)',
                        hdu.header.get(BIASKW))
        if len(bias) != 4 :
            raise ValueError("Failed to parse BIASSEC: %s %s" % (BIASKW,
                                                                hdu.header.get(BIASKW)))
        
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

        ## we set the bias level to the mean value in each row
        bias = np.add.reduce(hdu.data[b:t,bl:bh],axis=1)/float(len(hdu.data[b:t,bl:bh][0]))
        mean = bias.mean()
        hdu.data[b:t,al:ah] -= bias[:,np.newaxis]

        hdu.header.update("BIAS%d" % ( idx),
                          mean,comment="amp %d biase level" % ( idx))
	del(bias)

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
                      help="Flat field [leave out if you don't want to flatten your inputs]")

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
                      help="Split MEF files into FITS on write after processing")
    parser.add_argument("--flip",
                      action="store_true",
                      help="Flip CCDs 0 to 18?")
    parser.add_argument("--dist",
                      action="store",
		      default=None,
                      help="Distribution the output by header/chip?")
    parser.add_argument("--ccd",
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
                      help="Images to process with give flat/bias/trim or to stack into flat/bias")
    parser.add_argument("--megapipe",
                      action="store_true",
                      default=False,
                      help="Add 'dummy' ELIXIR keywords to image so megapipe will work?")
                      
    ### get the bias frames from the archive.
    (args)=parser.parse_args()
    opt=args

    if args.verbose:
        logging.basicConfig(level=logging.INFO)

    file_names = args.images
    if len(args.images) == 0:
        parser.error("You must provide a list of images to process")

    ccds = args.ccd
    images={}

    ### zero is a zero field array of the required output size.
    ### get the required output size by looking a the first input
    ### array?
    ##


    bias=(args.bias is not None and fits.open(opt.bias,"readonly")) or False
    flat=(opt.flat is not None and fits.open(opt.flat,"readonly")) or False

    if bias and flat and len(bias) != len(flat):
        raise ValueError(
            "BIAS (%s) and FLAT (%s) have different number of extensions" % ( args.bias, args.flat))

    num_ext = None
    for filename in file_names:
        images[filename] = fits.open(filename, mode='readonly')
        if num_ext is None:
            num_ext = len(images[filename])
        logging.info("Expecting %d extensions" % ( num_ext))
        if num_ext != len(images[filename]):
            raise ValueError("Lengths of input MEFs are not all the same.")

    if bias and num_ext != len(bias):
        raise ValueError("Length of bias MEF does not match input images.")

    if flat and num_ext != len(flat):
        raise ValueError("Length of flat MEF does not match input images.")


    for extno in range(num_ext):
        print "Working on extxension "+str(extno)
	stack=[]
	mstack=[]
        for filename in images:
            hdu = images[filename][extno]
            
            ### reopen the output file for each extension.
            ### Create an output MEF file based on extension name if
            ### opt.split is set.
            if not opt.outfile and not opt.combine:
                ## No output file name provide and we're not combining into a single output so
                ## try and guess an output filename
                imtype=hdu.header.get(args.obstype_kw,args.obstype_kw)
                outfile=str(hdu.header.get(args.obs_id_kw,args.obs_id_kw))+file_type_flags(imtype,imtype)
            elif ( opt.combine or len(file_names)==1 ) and opt.outfile:
                ## we can push to a single output
                outfile=opt.outfile
            else:
                parser.error("Multiple inputs with combine but no output fileanem")

            if args.split:
                outfile += str(hdu.header.get(args.extname_kw, args.extname_kw))
            
            outfile = outfile+".fits"
            ### exit if the file exist and this is the ccd or
            ### were splitting so every file should only have one
            ### extension
            if os.access(outfile,os.W_OK) and ( opt.split or extno == 0):
                raise IOError(errno.EEXIST,"Output file "+outfile+" already exists")
                
            ### do the overscan for each file
            print "Processing "+filename

            if opt.overscan:
                logging.info("Applying overscan correction.")
                overscan(hdu)

            if opt.trim:
                logging.info("Trimming images, may not work as megapipe input now")
                trim(hdu)
                
            if bias:
                logging.info("Applying the bias correction.")
                hdu.data -= bias[extno].data
                hdu.header.update("BIAS",args.bias,comment="BIAS Image")

            if flat:
                logging.info("Applying flat field.")
                hdu.data /= flat[extno].data
                hdu.header.update("FLAT",args.flat,comment="Flat Image")

            if opt.normal:
                if hdu.data is None:
                    continue
                logging.info("Normallizing input frame.")
                (h, b) = np.histogram(hdu.data,bins=10000)
                idx = h.argsort()
                mode = float((b[idx[-1]]+b[idx[-2]])/2.0)
                hdu.data = hdu.data/mode
		hdu.data = hdu.data.astype('float16')

            hdu.header.update('CADCPROC',float(version),
                              comment='Version of cadcproc')

            ### add 'ELIXIR' keywords, so megapipe works on these images
            if args.megapipe:
                for keyword in elixir_header:
                    hdu.header.update(keyword,
                                      hdu.header.get(keyword,default=elixir_header[keyword][0]),
                                      elixir_header[keyword][1])

            if not opt.combine or len(file_names)==1:

                ### write out the image now (don't overwrite
                ### files that exist at the start of this process
                if opt.split:
		    hdu_list=fits.HDUList()
                    phdu=fits.ImageHDU()
                    phdu.header=hdu.header
                    phdu.data=hdu.data
                    del phdu.header['XTENSION']
                    del phdu.header['PCOUNT']
                    del phdu.header['GCOUNT']
                    phdu.verify(option='fix')
                    if opt.short:
                        logging.info("ushort'n the pixel values")
                        phdu.scale(type='int16',bscale=1,bzero=32768)
		    fitsobj.append(phdu)
                    logging.info("Writing extension %s to %s" % ( str(extno), outfile))
                    fitsobj.writeto(outfile)
                else:
                    if opt.short:
                        logging.info("short'n the data")
                        hdu.scale(type='int16',
                                  bscale=1,
                                  bzero=32768)
                    try:
                        proc_hdu_list = fits.open(outfile,'append')
                        proc_hdu_list.append(hdu)
                        proc_hdu_list.flush()
                        proc_hdu_list.close()
                    except IOError as e:
                        if e.errno == 2:
                            fits.PrimaryHDU(header=hdu.header,
                                              data=hdu.data).writeto(outfile)
            else:
                mstack.append(hdu.data.astype('float16'))
                naxis1 = mstack[-1].shape[0]
                naxis2 = mstack[-1].shape[1]
                
        ### last image has been processed so combine the stack
        ### if this is a combine and we have more than on hdu
        if bias:
            bias[extno]=None
        if flat:
            flat[extno]=None
            
        if opt.combine and len(file_names)>1:
            if len(mstack) > 0:
                mstack = np.vstack(mstack)
                mstack.shape = [len(file_names),naxis1,naxis2]
                data = np.percentile(mstack, 40, axis=0)
            else:
                data = hdu.data
            del(mstack)
            stack = fits.ImageHDU(data)
            if stack.data is not None:
                stack.data = stack.data.astype('float32')
            stack.header[args.extname_kw] = (hdu.header.get(args.extname_kw,args.extname_kw), 'Extension Name')
            stack.header['QRUNID'] = (hdu.header.get('QRUNID',''), 'CFHT QSO Run flat built for')
            stack.header['FILTER'] = (hdu.header.get('FILTER',''), 'Filter flat works for')
            stack.header['DETSIZE'] = hdu.header.get('DETSIZE','')
            stack.header['DETSEC'] = hdu.header.get('DETSEC','')
            
            for filename in images:
                stack.header['comment'] = str(filename)+" used to make this flat"
            if opt.short:
                logging.info("ushort'n data")
                stack.scale(type='int16',bscale=1,bzero=32768)
            logging.info("Writing 40th percentile image to %s" % ( outfile))
            if opt.split:
                logging.info("writing to %s" % ( outfile))
                stack.writeto(outfile)
            else:
                try:
                    fitsobj = fits.open(outfile, 'append')
                    fitsobj.append(stack)
                    fitsobj.close()
                except IOError as e:
                    if e.errno != 2 :
                        raise e
                    logging.info("Creating output image %s" % ( outfile))
                    fits.PrimaryHDU(stack).writeto(outfile)
            del(stack)
            del(hdu)
    

