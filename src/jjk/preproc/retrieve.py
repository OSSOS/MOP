#!/usr/bin/env python
#*
#*   RCS data:
#*	$RCSfile: retrieve.py,v $
#*	$Revision: 1.3 $
#*	$Date: 2006/01/09 13:36:44 $
#*
#*   Programmer		: <your name>
#*
#*   Modification History:
#*
#****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
# Run J-M.'s and Matt's object finding systems... then intersect the 
# results.  


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
                          dest="triple",
                          help="Triple to search")
        parser.add_option("--block","-b",
                          action="store",
                          default=None,
                          dest="block",
                          help="CFEPS Block to retrieve, (eg: 03BQ02)")
        parser.add_option("--pointing","-p",
                          action="store",
                          default=None,
                          dest="pointing",
                          help="CFEPS POINTING to retrieve, (eg: 03BQ02)")
        
        
        (opt, file_ids)=parser.parse_args()


        import os, shutil

        import MOPdbaccess,MOPfits
        mysql=MOPdbaccess.connect('cfeps','cfhls',dbSystem='MYSQL')

        if not opt.triple and not opt.block:
            parser.print_help()
            sys.exit("\n\n You must specify either a triple or block\n")

        cfeps=mysql.cursor()

            
	if not opt.triple:
            sql="""SELECT DISTINCT(t.id) 
            FROM triples t JOIN checkup d ON t.id=d.triple 
            JOIN bucket.association a ON t.pointing=a.pointing
            JOIN bucket.pointings p ON t.pointing=p.id
            JOIN bucket.blocks b ON a.expnum=b.expnum
            WHERE b.qname LIKE '%s'""" % ( opt.block, ) 
	    if opt.pointing:
	        sql+=""" AND p.name LIKE '%s'""" % (opt.pointing, ) 
            print sql
            cfeps.execute(sql)
            rows=cfeps.fetchall()
        else:
            rows=[opt.triple,]
	print rows
        
        for row in rows:
            triple=row[0]
            sql="SELECT expnum FROM triple_members WHERE triple=%d ORDER BY expnum " % ( triple,)
            cfeps.execute(sql)
            exps=cfeps.fetchall()
            file_ids=[]
            for exp in exps:
                if opt.verbose:
                    sys.stderr.write("Getting exposure %s \n" % (exp[0],))
                MOPfits.adGet(str(exp[0])+'p')
