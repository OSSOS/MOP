#!/usr/bin/env python
#*
#*   RCS data:
#*	$RCSfile: search.py,v $
#*	$Revision: 1.9 $
#*	$Date: 2007/05/15 19:40:19 $
#*
#*   Programmer		: JJ Kavelaars
#*
#*   Modification History:
#*
#****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
# Run J-M.'s and Matt's object finding systems... then intersect the 
# result.  

import sys
import time
sys.path.append('/home/cadc/kavelaar/lib/python')

from myTaskError import TaskError



def get_image(expnum,ccd):
    try:
        if int(ccd) < 18:
            cutout="[-*,-*]"
        else:
            cutout=None
        import MOPfits
        filename=MOPfits.adGet(str(expnum)+opt.raw,extno=int(ccd),cutout=cutout)
    except:
        raise TaskError, "get image failed"
        

if __name__=='__main__':
        ### Must be running as a script
        import optparse, sys,string
        from optparse import OptionParser

        parser=OptionParser()
        parser.add_option("--verbose","-v",
                          action="store_true",
                          dest="verbose",
                          help="Provide feedback on what I'm doing")
        parser.add_option("--field","-f",
                          action="store",
                          dest="field",
                          help="CFEPS field to search")
        parser.add_option("--block","-b",
                          action="store",
                          dest="block",
			  default=None,
                          help="CFEPS block to search") 
        parser.add_option("--ccd","-c",
                          action="store",
                          default=None,
                          type="int",
                          dest="ccd")
        parser.add_option("--raw",
                          action="store_true",
                          default=False,
                          help="Use the raw exposures?")

        (opt, file_ids)=parser.parse_args()
	if opt.raw:
	    opt.raw="o"
	else:
	    opt.raw="p"
	fext = opt.raw

        import os, shutil, sys
        import MOPdbaccess
        mysql=MOPdbaccess.connect('bucket','cfhls',dbSystem='MYSQL')
        bucket=mysql.cursor()
        if opt.verbose:
            print "Starting to gather info about chips to search\n"

        field=None
	if opt.field:
	  field=" e.object like '%s' " % ( opt.field)
        qname=None
	if opt.block:
	  qname=" b.block LIKE '%s' " % ( opt.block ) 

        sql="""SELECT e.expnum,e.object FROM exposure e JOIN blocks b ON
               e.expnum = b.expnum """
        sep=" WHERE "
        if field :
            sql += sep + field
            sep = " AND "
        if qname :
            sql += sep + qname
            sep = " AND "

        if (opt.verbose): 
            print sql
        bucket.execute(sql)
        expnums=bucket.fetchall()
	mysql.close()

        high=36
        low=0
        if opt.ccd is not None:
            low=int(opt.ccd)
            high=int(opt.ccd)+1

        rows=[]
        for ccd in range(low,high):
            for expnum in expnums:
                rows.append((expnum[0],ccd,expnum[1]))

        cwd=os.getcwd()
            
        for row in rows:
            expnum=row[0]
	    ccd=row[1]
            object=row[2]
	    ccdPath=os.path.join("chip"+string.zfill(str(ccd),2),str(object))
	    ndir=os.path.join(cwd,os.path.join("nailing",ccdPath))
            if opt.verbose:
                sys.stderr.write("Working on "+str(expnum)+":"+str(ccd)+" into " + ndir + "\n")

	    ### Grab this row from the list of stuff TBD
            if not os.path.exists(ndir):
	        os.makedirs(ndir)
            os.chdir(ndir)
            opt.raw=fext
            try:
                if opt.verbose :
                    sys.stderr.write("Doing CCD: %s, of file: %s, EXT: %s\n" %( str(ccd),str(expnum),str(opt.raw)))
                get_image(expnum,ccd)
            except TaskError, info:
                sys.stderr.write("ERROR:\n")
                sys.stderr.write(str(info)+"\n")
                comment=str(info)
                pass
            os.chdir(cwd)

