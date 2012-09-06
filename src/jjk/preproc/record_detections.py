#!/usr/bin/env python
#*
#*   RCS data:
#*	$RCSfile: search.py,v $
#*	$Revision: 1.5 $
#*	$Date: 2006/01/09 13:36:44 $
#*
#*   Programmer		: JJ Kavelaars
#*
#*   Modification History:
#*
#****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
# Run J-M.'s and Matt's object finding systems... then intersect the 
# results.  
"""Given a list of directories that have the format triple.ccd look for *.real.astrom and *.NONE.astrom file and update the 'status' line in the process table of the cfeps database"""

from myTaskError import TaskError

if __name__=='__main__':
        ### Must be running as a script
        import optik, sys
        from optik import OptionParser

        parser=OptionParser()
        parser.add_option("--verbose","-v",
                          action="store_true",
                          dest="verbose",
                          help="Provide feedback on what I'm doing")


        (opt, dirs)=parser.parse_args()


        import os, shutil, sys, string

        import MOPdbaccess
        mysql=MOPdbaccess.connect('cfeps','cfhls',dbSystem='MYSQL')
        cfeps=mysql.cursor()

        foreach dir in dirs:
            [triple.ccd] = dirs.split('.')
            
            sql="""SELECT distinct(t.id),m.ccd 
            FROM triples t
            JOIN %s d ON t.id=d.triple
            JOIN block_pointing b ON b.pointing=t.pointing
	    JOIN mosaic m
	    LEFT JOIN processing p ON ( p.triple=d.triple AND p.ccd=m.ccd)
            WHERE p.comment IS NULL 
	    AND m.instrument LIKE 'MEGAPRIME' 
	    AND b.block LIKE '%s' order by t.id,m.ccd""" % ( opt.epoch, opt.block, )

            cfeps.execute(sql)
            rows=cfeps.fetchall()
        else:
	    rows=[]
            low=0
            high=36
            if opt.ccd>-1:
                low=opt.ccd
                high=opt.ccd
            for ccd in range(low,high):
                rows.append([opt.triple,ccd])
            
	mysql.close()

        if opt.verbose:
            sys.stderr.write("Searching %d triples \n" % ( len(rows), ) )

        for row in rows:
            triple=row[0]
	    ccd=row[1]
            if opt.verbose:
                sys.stderr.write("Working on "+str(triple)+":"+str(ccd)+"\n")
            comment="searched"
            mysql=MOPdbaccess.connect('cfeps','cfhls',dbSystem='MYSQL')
            cfeps=mysql.cursor()
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

	    result=-1
            try:
                result=searchTriples(file_ids,ccd)
            except TaskError, info:
                comment=str(info)
                sql="INSERT INTO processing (triple, status, comment, ccd) VALUES ( %d, %d, '%s', %d ) " % ( triple, result, comment,ccd)
                cfeps.execute(sql)
                mysql.commit()
                
            os.chdir("../")

            if not result>0:
                shutil.rmtree(wdir)
            else:
                result=result+1
            try:
                sql="INSERT INTO processing (triple, status, comment, ccd) VALUES ( %d, %d, '%s', %d ) " % ( triple, result, comment,ccd)
                cfeps.execute(sql)
                mysql.commit()
            except:
                sys.stderr.write("Update failed\n")
                sys.exit(-1)
            
	    mysql.close()
            if opt.verbose:
                sys.stderr.write("Found %d candidates in triple %d on ccd %d\n" % (result, triple, ccd))
