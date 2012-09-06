#!/usr/bin/env python
#*
#*   RCS data:
#*	$RCSfile: searchTriples.py,v $
#*	$Revision: 1.5 $
#*	$Date: 2005/10/03 22:02:18 $
#*
#*   Programmer		: <your name>
#*
#*   Modification History:
#*
#****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
# Run J-M.'s and Matt's object finding systems... then intersect the 
# results.  


def searchTriples(expnums,ccd):
    """Given a list of exposure numbers, find all the KBOs in that set of exposures"""
    import MOPfits,os 
    import MOPdbaccess
    
    if len(expnums)!=3:
        return(-1)


    


    mysql=MOPdbaccess.connect('bucket','cfhls','MYSQL')
    bucket=mysql.cursor()
    ### Some program Constants

    proc_file = open("proc-these-files","w")
    proc_file.write("# Files to be planted and searched\n")
    proc_file.write("#            image fwhm plant\n")
    
    import string
    import os.path
    filenames=[]
    import pyfits
    for expnum in expnums:
        bucket.execute("SELECT obs_iq_refccd FROM exposure WHERE expnum=%s" , (expnum, ) )
        row=bucket.fetchone()
        fwhm=row[0]
        if not fwhm > 0:
            fwhm=1.0

        if int(ccd)<18:
            cutout="\[-*,-*\]"
        else:
            cutout=None
        filename=MOPfits.adGet(str(expnum)+"p",extno=int(ccd),cutout=cutout)
        print filename
        if not os.access(filename,os.R_OK):
            return(-3)
        filename=os.path.splitext(filename)
        filenames.append(filename[0])
        proc_file.write("%s %f %s \n" % ( filename[0], fwhm/0.183, "no"))

    proc_file.flush()
    proc_file.close()
    
    command="find.pl -p '' -d ./ "
    sys.stderr.write(command)
    try:
        os.system(command)
    except:
        sys.stderr.write("Failed while running find")
    

    file_extens=[
        "cands.comb",
        "measure3.cands.astrom",
        "measure3.WARNING",
        "measure3.astrom.scatter"]
    
    if os.access("find.OK",os.R_OK):
        os.system("touch /home/cadc/kavelaar/results/05AQ06B/"+filenames[0]+".OK")
    else:
        os.system("touch /home/cadc/kavelaar/results/05AQ06B/"+filenames[0]+".FAILED")
        
        ### look for the cand.comb file and store in the DB
    import shutil
    for ext in file_extens:
        if os.access(filenames[0]+"."+ext,os.R_OK):
            shutil.copy(filenames[0]+"."+ext,"/home/cadc/kavelaar/results/05AQ06B")
    astrom=filenames[0]+".measure3.cands.astrom"
    print astrom
    cmd = "mpc_gen.pl -c "+astrom
    print os.access(astrom,os.R_OK)
    if os.access(astrom,os.R_OK):
        print cmd
        os.system(cmd)
        os.system("mpcIngest.pl *.MPC")
	os.system("cp *.MPC /home/cadc/kavelaar/results/05AQ06B")
    return(0)

if __name__=='__main__':
        ### Must be running as a script
        import optik, sys
        from optik import OptionParser

        parser=OptionParser()
        parser.add_option("--verbose","-v",
                          action="store_true",
                          dest="verbose",
                          help="Provide feedback on what I'm doing")
        parser.add_option("--triple","-t",
                          action="store",
                          type="int",
			  default=0,
                          dest="triple",
                          help="Triple to search")
        parser.add_option("--block","-b",
                          action="store",
                          dest="block",
                          help="CFEPS block to search")
        parser.add_option("--ccd","-c",
                          action="store",
                          default=-1,
                          type="int",
                          dest="ccd",
                          help="Provide feedback on what I'm doing")
        (opt, file_ids)=parser.parse_args()


        import os, shutil
        if os.getenv('_CONDOR_SCRATCH_DIR') != None: os.chdir(os.getenv('_CONDOR_SCRATCH_DIR'))

        import MOPdbaccess
        mysql=MOPdbaccess.connect('cfeps','cfhls',dbSystem='MYSQL')
        cfeps=mysql.cursor()

        if not opt.triple:
            sql="""SELECT DISTINCT(t.id)
            FROM triples t JOIN discovery d ON t.id=d.triple
            JOIN bucket.association a ON t.pointing=a.pointing
            JOIN bucket.blocks b ON a.expnum=b.expnum
            WHERE b.qname LIKE '%s'""" % ( opt.block, )

            cfeps.execute(sql)
            results=cfeps.fetchall()
            rows=[]
            for result in results:
                for ccd in range(36):
                    rows.append([result[0],ccd])
        else:
            rows=[]
	    if opt.ccd>0:
                rows.append([opt.triple,opt.ccd])
	    else:
                for ccd in range(36):
                    rows.append([opt.triple,ccd])
        print rows;
        sys.exit()
        for row in rows:
            triple=row[0]
            ccd=row[1]
            sql="SELECT expnum FROM triple_members WHERE triple=%d ORDER BY expnum " % ( triple,)
            cfeps.execute(sql)
            exps=cfeps.fetchall()
            file_ids=[]
            for exp in exps:
                file_ids.append(exp[0])
            if opt.verbose:
                sys.stderr.write("Running find on the files "+str(file_ids))
            wdir=str(triple)+"."+str(ccd)
            os.mkdir(wdir)
            os.chdir(wdir)
            result=searchTriples(file_ids,ccd)
	    if not opt.verbose:
                os.chdir("../")
                shutil.rmtree(wdir)
            sql="INSERT INTO processing (triple, status, comment, ccd) VALUES ( %d, %d, '%s', %d ) " % ( triple, result, "TESTING",ccd)
            cfeps.execute(sql)
            mysql.commit()
            sys.stderr.write(str(result)+"\n")
