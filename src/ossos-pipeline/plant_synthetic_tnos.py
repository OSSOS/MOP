
def searchTriples(filenames,plant=False):
    """Given a list of exposure numbers, find all the KBOs in that set of exposures"""

    print filenames
    if opt.none :
        return


    import MOPfits,os 
    import MOPdbaccess
    import string
    import os.path
    import pyfits

    
    if len(filenames)!=3:
        raise TaskError, "got %d exposures"%(len(expnums))


    ### Some program Constants
    proc_these_files=[]
    if not plant:
        proc_these_files.append("# Files to be planted and searched\n")
        proc_these_files.append("#            image fwhm plant\n")

        
    for filename in filenames:
    	try: 
            mysql=MOPdbaccess.connect('bucket','cfhls','MYSQL')
            bucket=mysql.cursor()
	except:
            raise TaskError, "mysql failed"
        #bucket.execute("SELECT obs_iq_refccd FROM exposure WHERE expnum=%s" , (expnum, ) )
        #row=bucket.fetchone()
	#mysql.close()
        #fwhm=row[0]
        #if not fwhm > 0:
        fwhm=1.0

        if not plant:
            #proc_these_files.append("%s %f %s \n" % ( filename[0], fwhm/0.183, 'no'))
	    pstr='NO'
        else:
	    pstr='YES'
            ### since we're planting we need a psf.  JMPMAKEPSF will
            ### update the proc-these-files listing

        ### run the make psf script .. always.  This creates proc-these-files
        ### which is needed by the find.pl script.
        command='jmpmakepsf.csh ./ %s %s' % ( filename, pstr )
        if opt.verbose:
            sys.stderr.write( command )
        try:
            os.system(command)
        except:
            raise TaskError, "jmpmakepsf noexec"
        if os.access(filename+'.jmpmakepsf.FAILED',os.R_OK) or not os.access(filename+".psf.fits", os.R_OK) :
	    if plant:
                raise TaskError, "jmpmakepsf failed"
#	do without plant
            else:
	        plant=False
		pstr='NO'
	        ### we're not planting so, lets keep going
		### but check that there is a line in proc_these_files
	        add_line=True
		if not os.access('proc-these-files',os.R_OK):
		    f=open('proc-these-files','w')
		    for l in proc_these_files:
		        f.write(l)
		    f.close()
	    	f=open('proc-these-files','r')
		ptf_lines=f.readlines()
		f.close()
		for ptf_line in ptf_lines:
		    if ptf_line[0]=='#':
		        continue
	            ptf_a=ptf_line.split()
		    import re
		    if re.search('%s' % (filename),ptf_a[0]):
		        ### there's already a line for this one
			add_line=False
		        break
                if add_line:
		    f=open('proc-these-files','a')
		    f.write("%s %f %s \n" % ( filename, fwhm/0.183, 'no'))
		    f.close()

    if opt.none:
        return(-1)
    prefix=''
    if plant:
        command="plant.csh ./ " 
        #command="plant.csh ./ -rmin %s -rmax %s -ang %s -width %s " % ( opt.rmin, opt.rmax, opt.angle, opt.width)
        try: 
            os.system(command)
        except:
            raise TaskError, 'plant exec. failed'
        if not os.access('plant.OK',os.R_OK):
            raise TaskError, 'plant failed'
        prefix='fk'
    #else:
    #    f=open('proc-these-files','w')
    #    for line in proc_these_files:
    #        f.write(line)
    #    f.flush()
    #    f.close()
    	
    if opt.rerun and os.access('find.OK',os.R_OK):
        os.unlink("find.OK")

    #command="find.pl -p "+prefix+" -rn %s -rx %s -a %s -aw %s -d ./ " % ( opt.rmin, opt.rmax, opt.angle, opt.width) 
    command="find.pl -p "+prefix+" -d ./ " 
    if opt.union :
        command+=" -u"

    if opt.verbose:
        sys.stderr.write( command )

    try:
        os.system(command)
    except:
        raise TaskErorr, "execute find"
    

    if not os.access("find.OK",os.R_OK):
        raise TaskError, "find failed"

    ### check the transformation file
    command = "checktrans -p "+prefix
    
    try:
        os.system(command)
    except:
        raise TaskError, "execute checktrans"
    
    if not os.access("checktrans.OK",os.R_OK):
        raise TaskError, "checktrans failed"
    elif os.access("checktrans.FAILED",os.R_OK):
        os.unlink("checktrans.FAILED")

    if os.access("BAD_TRANS"+prefix,os.R_OK):
        raise TaskError,"BAD TRANS"

    ## check that the transformation in .trans.jmp files look reasonable
    import math
    for filename in filenames:
      try:
          for line in open(filename+".trans.jmp"):
              for v in line.split():
                  if math.fabs(float(v)) > 200:
                      raise TaskError,"BAD TRANS"
      except:
          raise TaskError, "TRAN_CHECK FAILED"
        
    astrom=prefix+filenames[0]+".cands.comb"
    if opt.plant:
        for filename in filenames:
	    try:
	       ushort(prefix+filename+".fits")
	    except:
	       raise TaskError("ushort failed %s" % (prefix+filename+".fits"))

    if opt.plant:
        astrom=prefix+filenames[0]+".comb.found"
        try:
            #make sure we have +5 lines in this file
	    lines=file(astrom).readlines()
	    if len(lines)<5:
	       raise TaskError,"Too few Found"
	except:
	    raise TaskError, "Error reading %s" %(astrom)
  
    
    if os.access(astrom,os.R_OK):
        return(1)
    else:
        return(0)

def ushort(filename):
    """Ushort a the pixels"""
    import pyfits
    f=pyfits.open(filename,mode='update')
    f[0].scale('int16','',bzero=32768)
    f.flush()
    f.close()



def get_nailing(expnum,ccd):
    """Get the 'nailing' images associated with expnum"""
    sql="""
    SELECT e.expnum, (e.mjdate - f.mjdate) dt
    FROM bucket.exposure e
    JOIN bucket.exposure f
    JOIN bucket.association b ON b.expnum=f.expnum
    JOIN bucket.association a ON a.pointing=b.pointing AND a.expnum=e.expnum
    WHERE f.expnum=%d
    #AND abs(e.mjdate - f.mjdate) > 0.5
    #AND abs(e.mjdate - f.mjdate) < 15.0
    #ORDER BY abs(e.mjdate-f.mjdate)
    """ % ( expnum )
    try:
        import MOPdbaccess
        mysql=MOPdbaccess.connect('bucket','cfhls',dbSystem='MYSQL')
        bucket=mysql.cursor()
        bucket.execute(sql)
        nailings = bucket.fetchall()
        mysql.close()
        if int(ccd) < 18:
            cutout="[-*,-*]"
        else:
            cutout=None
        import MOPfits
        for nailing in nailings:
            filename=MOPfits.adGet(str(nailing[0])+opt.raw,extno=int(ccd),cutout=cutout)
    except:
        raise TaskError, "get nailing failed"
        
