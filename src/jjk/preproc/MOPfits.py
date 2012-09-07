#!/usr/cadc/misc/bin/python
#/*+
#************************************************************************
#****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
#*
#* (c) 2003.				(c) 2003.
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
#*   Script Name:	MOPFits.py
#*
#*   Purpose:
#*      control access to FITS files in the MOP environment.
#*      extensions are all part of the same base mosiac.
#*	
#*
#*   Functions:
#+      def build(outfile,verbose,infiles): Build the MEF file -> status
#+      def adPut(file, file_id, archive, stream): Put a file into AD using adPut -> status
#+      def _open(file): Open a fits file -> hdu
#+      def _open_fix(file): Open a non-compliant FITS file -> hdu
#*
#*
#*   Date		: Jun 19, 2004
#*
#*   RCS data:
#*	$RCSfile: MOPfits.py,v $
#*	$Revision: 1.8 $
#*	$Date: 2007/01/10 22:26:01 $
#*
#*   Programmer		: JJ Kavelaars
#*
#*   Modification History:
#*   $Log: MOPfits.py,v $
#*   Revision 1.8  2007/01/10 22:26:01  observe
#*   small changes to improve speed
#*
#*   Revision 1.7  2005/12/08 19:17:44  observe
#*   Bringing gimli upto date... [that's jj's linux machine]
#*
#*   Revision 1.6  2005/10/03 22:02:18  observe
#*   simplyfied the search and verify .. slightly
#*
#*   Revision 1.5  2005/09/20 00:01:52  observe
#*   added 'local' data area check to the adGet sequence, made searchTriples work on 'blocks' too
#*
#*   Revision 1.4  2005/09/19 21:59:20  observe
#*   verifyDetection, a script to see if object is real... not quite working yet
#*
#*   Revision 1.3  2005/07/15 19:06:28  observe
#*   wcsupdate improved to 'magnitude' range limited catalog. [changes needed in center.py
#*   preproc stuff still under development and should be considered ALPHA stuff
#*
#*   Revision 1.2  2005/05/11 00:19:02  observe
#*   search a set of images for moving objects... currently just a stub
#*
#*   Revision 1.1  2005/05/10 21:03:12  observe
#*   New automated processing scripts for use inside the CADC
#*
#*   Revision 1.3  2004/07/23 20:54:31  kavelaar
#*   added functions to search HISOTRY/COMMENT keywords for master detrended names
#*   this was needed for some keywords that didn't get updated during elixir processing.
#*
#*   Revision 1.2  2004/06/23 06:33:21  kavelaar
#*   Removed debuging message.   Added call to _open_fix when header returned
#*   by pyfits has zero length.  _open_fix now works on .hdx files
#*
#*   Revision 1.1  2004/06/22 21:42:54  kavelaar
#*   Initial revision
#*
#*   Revision 1.1  2004/06/22 21:39:30  kavelaar
#*   Initial revision
#*
#*   Revision 1.4  2004/06/22 21:08:24  kavelaar
#*   removed  some debuging print statements ....
#*
#*   Revision 1.3  2004/06/22 19:20:14  kavelaar
#*   fixxed the ID line to have shorter format so FITS header card will not overflow.
#*
#*   Revision 1.2  2004/06/22 19:16:39  kavelaar
#*   Working version of cfh12kFits module
#*
#*   Revision 1.1  2004/06/22 07:08:15  kavelaar
#*   Initial revision
#*
#*
#****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
#************************************************************************
#-*/
"""Combine a list of .fits images into an MEF format .fits image.

Designed to stick single fits files together into the MEF format.
This work is done based on the standard published in
http://adsabs.harvard.edu/cgi-bin/nph-data_query?bibcode=1994A%26AS..105...53P&db_key=AST&link_type=ABSTRACT&high=3f68ae7df628038
"""

### set the version flag for this program
__Version__ = "$Revision: 1.8 $"
__Author__ = "$Author: observe $"
__Id__ = "$Id: MOPfits.py,v 1.8 2007/01/10 22:26:01 observe Exp $"
__LastUpdate__ = "$Date: 2007/01/10 22:26:01 $"

#
#+
########################################################################
#
#   Function:	inputs
#
#   Purpose:
#	read the imcombine HISTORY lines in the image header to determine which
#       files were used to build a master detrend image.
#
#   Arguments:
#       header     An HDU with the HISTORY lines to check.
#   Return values:
#       inputs     List of file_ids representing the files used to build the detrend frame
#
########################################################################
#-

from myTaskError import TaskError

def inputs(header):
    """Read through the HISTORY cards in an image header looking for detrend
    input lines.

    Detrend inputs are given on lines like:
    HISTORY imcombred: file_id

    We require that the value in file_id be store in the CADC archive before
    adding to the inputs list.
    """
    import string, re

    inputs=[]
    for h in header.ascardlist():
        if h.key=="HISTORY":
            g=h.value
            result=re.search('imcombred: (\d{6}[bfopd])\d{2} .*',g)
            if not result:
                continue
            file_id=result.group(1)
            import os
            status=os.system("adInfo -a CFHT -s "+file_id)
            if status==0:
                result=re.search('(\d{6}).*',file_id)
                if not result:
                    continue
                expnum=result.group(1)
                inputs.append(expnum)

    if len(inputs)==0:
        ### try using the new FLIPS 2.0 keywords
        nit = header.get('IMCMB_NI',0)
        if nit==0:
            return(inputs)
        for nin in range(nit):        
            kwd='IMCMB_'+str(nin).zfill(2)
            file=(header.get(kwd,''))
            result=re.search('.*(\d{6}[bfopd]).*',g)
            if not result:
                continue
            file_id=result.group(1)
            import os
            status=os.system("adInfo -a CFHT -s "+file_id)
            if status==0:
                result=re.search('(\d{6}).*',file_id)
                if not result:
                    continue
                expnum=result.group(1)
                inputs.append(expnum)
                
    return inputs



#
#+
########################################################################
#
#   Function:	elixir_decode
#
#   Purpose:
#	Decode an elixir filename into RUNID, OBS-TYPE, FILTER, EXPTIME...
#
#   Arguments:
#	elixr_filename	: Name of the filename to decode.
#
#   Return values:
#       keywords        : array of fits header cards with the key/value
#  
#
########################################################################
#-

def elixir_decode(elixir_filename):
    """
    Takes an elixir style file name and decodes it's content.

    Values returned as a dictionary.  Elixir filenames have the format
    RUNID.TYPE.FILTER/EXPTIME.CHIPID.VERSION.fits
    """
    import re, pyfits
    
    parts_RE=re.compile(r'([^\.\s]+)')
    dataset_name = parts_RE.findall(elixir_filename)

    ### check that this was a valid elixir_filename
    if not dataset_name or len(dataset_name)<5 :
        raise ValueError('String %s does not parse as elixir filename'
                         % elixir_filename )
        
    comments={'exptime': 'Integration time (seconds)',
              'filter': 'Name of filter in position  ',
              'crunid': 'CFHT Q RunID',
              'obstype': 'Observation or Exposure type',
              'imageid': 'CCD chip number',
              'filename': 'file name at creation of this MEF file'
              }
        
    keywords={}
    keywords['filename']=elixir_filename
    keywords['runid']=dataset_name[0]
    keywords['obstype']=dataset_name[1]
    keywords['exptime']=None
    keywords['filter']=None

    ### if the third part of the name is all numbers we assume exposure time
    if re.match(r'\d+',dataset_name[2]):
        keyword['exptime']=int(dataset_name[2])
    else:
        keyword['filter']=dataset_name[2]
    
    keywords['imageid']=dataset_name[3]
    keywords['version']=dataset_name[4]

    header=pyfits.Header()
    for keyword in keywords.keys():
        if keywords[keyword]:
            header.update(keyword,keywords[keyword],comment=comment[keyword])

    return header

#
#+
########################################################################
#
#   Function:	create_mef
#
#   Purpose:
#	Create a blank MEF FITS file.
#
#   Arguments:
#	filename	: Name for the MEF (if blank then generate unique)
#
#   Return values:
#       filename
#
########################################################################
#-

def create_mef(filename=None):
    """
    Create a file an MEF fits file called filename.  Generate a random
    filename if None given
    """
    import pyfits, time

    if not filename:
        ### here I know what filename is to start with.
        import tempfile
        filename=tempfile.mktemp(suffix='.fits')
    else:
        import string, re
        ### filenames gotta be a string and no lead/trailing space
        filename=string.strip(str(filename))
        ### require that the filename ends in .fits
        suffix=re.match(r'^.*.fits$',filename)
        if not suffix:
            filename = filename+'.fits'

    ### create an HDU list
    temp = pyfits.HDUList()

    ### create a primary HDU
    prihdu = pyfits.PrimaryHDU()

    ### build the header
    h=prihdu.header
    h.update('EXTEND',pyfits.TRUE,after='NAXIS')
    h.update('NEXTEND',0,after='EXTEND')
    h.add_comment('MEF created at CADC')
    h.add_comment('Created using '+__name__+' '+__Version__)
    h.add_comment('Extensions may not be in CCD order')
    #h.update('cfh12k',__Version__,comment='split2mef software at CADC')
    h.add_comment('Use the EXTNAME keyword')
    h.add_history('Primary HDU created on '+time.asctime())

    ### stick the HDU onto the HDU list and write to file
    temp.append(prihdu)
    temp.writeto(filename)
    temp.close()

    return(filename)

#
#+
########################################################################
#
#   Function:	strip_pd
#
#   Purpose:
#	strip the comment pads from CFHT FITS header
#
#   Arguments:
#	hdu		: fits HDU
#	
#
#   Return values:
#       status
#
########################################################################
#-

__cfht_padding='COMMENT  Reserved space.  This line can be used to add a new FITS card.         '
__comment_keys=['COMMENT', 'HISTORY', '      '];

def strip_pad(hdu):
    """Remove the padding lines that CFHT adds to headers"""
    
    l = hdu.header.ascardlist()
    d = []
    for index in range(len(l)):
        if l[index].key in __comment_keys and str(l[index])==__cfht_padding:
            d.append(index)
    d.reverse()
    for index in d:
        del l[index]
    return(0)

#
#+
########################################################################
#
#   Function:	stack
#
#   Purpose:
#	Stack a list of input FITS files (Can be MEF) into an MEF output
#       file.
#
#   Arguments:
#	outfile		: path of the output file (created if doesn't exist)
#	infiles		: path of the files to stack into the output file
#       vebose=0        : print information about what's happening.
#
#   Return values:
#       0               : Success
#       1               : Failure
#
########################################################################
#-



def stack(outfile,infiles,verbose=0):
    """
    Stick infiles into outfiles as FITS extensions.

    outfile willl contain an MEF format file of the single extension FITS
    files named in the infiles array
    """
    
    import os, sys, string, tempfile, shutil
    import pyfits, re, time

### if there is a pre-existing MEF file for output then append to it
### otherwise we need to create a PrimaryHDU
    if os.access(outfile,os.R_OK)!=1:
        if verbose:
            print "Creating new MEF file: ",outfile
        outfile=create_mef(outfile)
        
### get a handle for the output image, _open is the local variant of
### pyfits.open and just does some error recovery if pyfits.open raises an
### exception.
    out = pyfits.open(outfile,'append')
    hdr = out[0].header
    count=0

### append the fits files given on the command line to the
### output file.
    det_xmin=None
    det_xmax=None
    det_ymin=None
    det_ymax=None
    for infile in infiles:
        if verbose:
            print "Adding ",infile," to ",outfile

        ### _open  tries to handle bad fits format exceptions.
        file=_open(infile)
        if not file:
            raise IOError("Cann't get the HDU for "+infile)

        for hdu in file:
            extname=None
            if hdu.header.has_key('EXTNAME') :
                extname=hdu.header['EXTNAME']
            elif hdu.header.has_key('EXTVER') :
                extname="ccd"+string.zfill(hdu.header.has_key('EXTVER'),2)
                    
            if hdu.header.has_key('EPOCH'):
                if hdu.header.has_key('EQUINOX'):
                    del hdu.header['EPOCH']
                else:
                    hdu.header.update('EQUINOX',hdu.header['EQUINOX'].value,
                                      comment=hdu.header['EQUINOX'].comment)

            ahdu=pyfits.ImageHDU(data=hdu.data, header=hdu.header, name=extname)
            out.append(ahdu)

        ### build the size of the overall detector
            if hdu.header.has_key('DETSEC'):
                values=re.findall(r'(\d+)',
                                  hdu.header['DETSEC'])
                if len(values)==4:
                    xmin=int(values[0])
                    xmax=int(values[1])
                    ymin=int(values[2])
                    ymax=int(values[3])

                if xmin>xmax:
                    t=xmin
                    xmin=xmax
                    xmax=t
                if ymin>ymax:
                    t=ymin
                    ymin=ymax
                    ymax=t

                if xmin<det_xmin or not det_xmin:
                    det_xmin=xmin
                if xmax>det_xmax or not det_xmax:
                    det_xmax=xmax
                if ymin<det_ymin or not det_ymin:
                    det_ymin=ymin
                if ymax>det_ymax or not det_ymax:
                    det_ymax=ymax
    

        file.close()


    detsize='['+str(det_xmin)+':'+str(det_xmax)+','+str(det_ymin)+':'+str(det_ymax)+']'
    out[0].header.update('DETSIZE',detsize,comment='Size of Mosaic')
    out.close()    
    if verbose:
        print "Done building MEF: ",outfile
    return 0

#
#+
########################################################################
#
#   Function:	adGet
#
#   Purpose:
#	get a file from the AD system.
#
#   Arguments:
#	file_id         : file_id to retrieve
#	filename        : output filename
#
#   Return values:
#       filename
#
########################################################################
#-

def	adGet(file_id, archive="CFHT", extno=None, cutout=None ):
    """Use get a fits image from the CADC."""
    
    import os, string, re,urllib


    proxy="http://www.cadc-ccda.hia-iha.nrc-cnrc.gc.ca/authProxy/getData"
    #proxy="http://www.cadc-ccda.hia-iha.nrc-cnrc.gc.ca/data/auth"
    #proxy="http://test.cadc-ccda.hia-iha.nrc-cnrc.gc.ca/authProxy/getData"
    #proxy="http://192.168.11.48/authProxy/getData"

    if file_id is None:
        return(-1)

    if  extno is None:
        filename=file_id+".fits"
    else:
        filename="%s%s.fits" % (file_id, string.zfill(extno,2))
    #print filename

    if os.access(filename,os.R_OK):
    	return filename

    args={
        "file_id": file_id,
        "archive": archive
        }

    if extno is not None:
        args['cutout']="["+str(extno+1)+"]"
    else:
        args['cutout']=''

    if cutout is not None:
        args['cutout']=args['cutout']+cutout
        
    argline=""
    sep=""
    import sys

    ### get the directory that may contain the data
    mop_data_path=os.curdir
    if os.environ.has_key('MOP_DATA_PATH'):
        mop_data_path=os.environ['MOP_DATA_PATH']

    suffix="fits"
    ### Stuff here was to try and use gimli as a storage node.
    ### this cause pain for CADC and should not be done again.
    ### try re-writing the way jobs are run instead.
    basefile=mop_data_path+"/"+file_id+".fits"
    ### try and get via gimli data area
    if os.access(basefile,os.R_OK):
    ##    basefile=file_id+".fits"
#	mop_data_path=os.curdir
#	command="(ssh gimli imcopy '/staging/gimli/1/kavelaar/data/%s%s' - ) > %s " % ( basefile, args['cutout'], filename)
##	command=command.replace('[','\[')
#	command=command.replace(']','\]')
#	command=command.replace('*','\*')
#	status=os.system(command)
#	if status!=0:
#	    os.unlink(filename)
#    else:
        command="imcopy %s%s %s" % ( basefile,args['cutout'],filename)
	status=os.system(command)
	if status!=0:
	    os.unlink(filename)
        
    if not os.access(filename,os.R_OK):
        ### see if this in my person AD area
    	argdict={}
	argline=''
	sep=''
        for arg in args:
            if not args[arg]:
                continue
	    argline+=sep+"%s=%s" % ( arg, args[arg])
	    sep='&'


        url=proxy+"?"+argline
        command="curl --location-trusted --silent -g  --fail --max-time 1800 '"+url+"' > "+filename
        try:
            print command
            status=os.system(command)
        except:
            sys.stderr.write("Failed to execute command: %s\n" % ( command))
	    raise TaskError, "getData failed"
    if not os.access(filename,os.R_OK):
       raise TaskError, "MOPfits.adGet failed"
    if status!=0:
        sys.stderr.write("Failed while executing command: %s\n" % ( command))
    	raise TaskError, "status failure"

    return filename

#
#+
########################################################################
#
#   Function:	_open
#
#   Purpose:
#	Open a fits file using pyfits.  If that failes try the internal
#       version _open_fix 
#
#   Arguments:
#	file            : File to read in
#	mode            : mode to open
#
#   Return values:
#       hdu             : a pyfits HDU instance list
#
########################################################################
#-

def _open(file,mode='copyonwrite'):
    """Opens a FITS format file and calls _open_fix if header doesn't
    verify correctly.
    """
    import pyfits

    try:
        infits=pyfits.open(file,mode)   
        hdu=infits
    except (ValueError,pyfits.VerifyError,pyfits.FITS_SevereError):
        import sys
        #### I really only know how to deal with one error right now.
        #if str(sys.exc_info()[1])=='mandatory keywords are not fixed format':
        hdu=_open_fix(file)
        #else:
        #    print sys.exc_info()[1]
        #    print " Failed trying to repair  ", file
        #    raise
    
    for f in hdu:
        strip_pad(f)
    return hdu

#
#+
########################################################################
#
#   Function:	find_proc_date
#
#   Purpose:
#	find the elixir processing date inside the comments of 12K image
#
#   Arguments:
#	header		: fitsHDU
#
#   Return values:
#       proc_date       : processing date
#
########################################################################
#-

def find_proc_date(header):
    """Search the HISTORY fields of a header looking for the FLIPS
    processing date.
    """
    import string, re
    for h in header.ascardlist():
        if h.key=="HISTORY":
            g=h.value
            if ( string.find(g,'FLIPS 1.0 -:') ):
                result=re.search('imred: FLIPS 1.0 - \S{3} (.*) - ([\s\d]\d:\d\d:\d\d)\s*$',g)
                if result:
                    date=result.group(1)
                    time=result.group(2)
                    datetime=date+" "+time
                    return datetime
    return None

#
#+
########################################################################
#
#   Function:	find_detrend_keyword
#
#   Purpose:
#	grep through a FITS HDU looking for DETREND keywords in COMMENT lines
#
#   Arguments:
#	header 	: fitsHDU
#       type    : FLAT/BIAS/MASK etc.
#
#   Return values:
#
########################################################################
#-

def find_detrend_keyword(header, type):
    """Search through header and find
    the elixir formated string(s) that match the the
    input 'type'.

    header is a FITS HDU.
    Elixir formated strings are crunid.type.filter/exptime.chipid.version.
    """
    import re, string
    value='NULL'
    #print type
    for h in header:
        g = str(h)
        if ( string.find(g,'.'+type+'.')!= -1 ):
            result=re.search('[^\s]*\.'+type+'\.[^\s]*\.\d\d\.\d\d',g)
            if result:
                return result.group(0)



#
#+
########################################################################
#
#   Function:	_open_fix
#
#   Purpose:
#	Open a FITS file but try and correct some standard file format errors
#
#   Arguments:
#       file            : File to open. 
#
#   Return values:
#       hdu             : list of pyfits HDU instances
#
########################################################################
#-

def _open_fix(file):
    """Takes in a fits file name, open the file in binary mode and creates an HDU.

    Will attempt to fix some of the header keywords to match the standard FITS format.
    """
    import pyfits, re, string
    temp = pyfits.HDUList()
    hdu = pyfits.PrimaryHDU()

    hdu._file=open(file,'rb')

    _number_RE = re.compile(
                r'(?P<sign>[+-])?0*(?P<digt>(\.\d+|\d+(\.\d*)?)([deDE][+-]?\d+)?)')

    ### here's the real difference between pyFits and cfh12kFits.
    ### I'm more flexible on the format of the header file so that allows me
    ### read more files.
    card_RE=re.compile(r"""
    (?P<KEY>[-A-Z0-9_a-za ]{8})   ### keyword is the first 8 bytes... i'll allow small letters
    (
     (
      (?P<VALUE>=\s)             ### =\s indicats a value coming.
      (\s*
       (   
        (?P<STRING>\'[^\']*[\'/])   ### a string 
        |
        (?P<FLOAT>([+-]?(\.\d+|\d+\.\d*)([dDEe][+-]?\d+)?))  ### a floating point number
        |
        (?P<INT>[+-]?\d+)      ### an integer
        |
        (?P<BOOL>[TFtf])       ### perhaps value is boolian
        )
       \s*
       (( / )?(?P<COMMENT>.*))?    ### value related comment.
      )
     )
     |
     (?P<C2>.*)     ### strickly a comment field
    )
    """,re.VERBOSE)

    
    done=0
    while ( not done):

        ### read a line of 80 characters up to a new line from the file.
        block=hdu._file.readline(80)

        string_end=79
        if len(block)== 0:
            done=1
            continue
        if block[-1]=='\n':
            string_end=len(block)-2

        line = re.match(r'[ -~]{0,'+str(string_end)+'}',block)

        line = string.ljust(line.group(0),80)[0:79]

        if line[0:8] == 'END     ':
            done=1
            break

        card=card_RE.match(line)
        if not card or not card.group('KEY'):
            print card.groups()
            raise SyntaxError("Failed to get keyword from FITS Card %s" % line)
            
        key=card.group('KEY')
        value=None
        if card.group('INT'):
            try:
                value=int(card.group('INT'))
            except:
                value=card.group('INT')
        elif card.group('FLOAT'):
            try:
                value=float(card.group('FLOAT'))
            except:
                value=float(card.group('FLOAT'))
        elif card.group('BOOL'):
            value=pyfits.Boolean(card.group('BOOL'))
        elif card.group('STRING'):
            value=card.group('STRING')[1:-1]
            
        if card.group('COMMENT'):
            _comment=card.group('COMMENT')
        elif card.group('C2'):
            _comment=card.group('C2')
        else:
            _comment=None

        try:
            if key =='COMMENT ':
                hdu.header.add_comment(_comment)
            elif key =='HISTORY ':
                hdu.header.add_history(_comment)
            elif key =='        ':
                hdu.header.add_blank(_comment)
            elif key:
                if key =='DATE-OBS' and value:
                    value=string.replace(value,'/','-')
                hdu.header.update(key,value,comment=_comment)
        except:
            raise SyntaxError("Failed to convert line to FITS Card %s" % line)

    ### set some internal variables to decided on data flow.
    hdu._bzero=hdu.header.get('BZERO',0)
    hdu._bscale=hdu.header.get('BSCALE',1)
    hdu._bitpix=hdu.header.get('BITPIX',-16)

    if hdu.header.get('NAXIS',0)>0:
        naxis1=hdu.header.get('NAXIS1',1)
        naxis2=hdu.header.get('NAXIS2',1)
    ### now read the data... this is a HACK from pyfits.py
        import numarray as num
    
        code = pyfits._ImageBaseHDU.NumCode[hdu._bitpix]
        dims = tuple([naxis2,naxis1])
        raw_data = num.fromfile(hdu._file,type=code,shape=dims)
        raw_data._byteorder='big'

        if ( hdu._bzero != 0
             or hdu._bscale!=1 ):
            if  hdu._bitpix > 0 :
                hdu.data=num.array(raw_data,type=num.Float32)
            else:
                hdu.data=raw_data
            if hdu._bscale != 1:
                num.multiply(hdu.data,hdu._bscale,hdu.data)
            if hdu._bzero!=0:
                hdu.data=hdu.data + hdu._bzero

            del hdu.header['BSCALE']
            del hdu.header['BZERO']
            hdu.header['BITPIX']=pyfits._ImageBaseHDU.ImgCode[hdu.data.type()]
            
    temp.append(hdu)
    return temp

if __name__ == '__main__':
    ### Import some standard python.
    import optik,sys
    from optik import OptionParser
    
### parse the commandline arguements
    parser = OptionParser()
    parser.add_option("-o","--outfile",
                      action="store", type="string", dest="outfile",
                      help="Name of output MEF file", metavar="MEF")
    parser.add_option("-q","--quiet",
                      action="store_false", dest="verbose", default=1,
                      help="run makepsf/iraf quietly?")

    
### the left over arguments are assumed to be fits images.
    (opt,infiles)=parser.parse_args()
    
    sys.exit(stack(opt.outfile,infiles,opt.verbose))


