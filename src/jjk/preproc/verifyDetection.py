#!/usr/bin/env python
#*
#*   RCS data:
#*	$RCSfile: verifyDetection.py,v $
#*	$Revision: 1.4 $
#*	$Date: 2006/05/11 23:53:38 $
#*
#*   Programmer		: J Kavelaars
#*
#*   Modification History:
#*
#****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
# Run J-M.'s and Matt's object finding systems... then intersect the 
# results.  


def get_flipped_ext(file_id,ccd):
    """Given a list of exposure numbers and CCD, get them from the DB"""
    
    import MOPfits
    import os, shutil
    
    filename=MOPfits.adGet(file_id,extno=int(ccd))
    if int(ccd)<18:
        tfname=filename+"F"
	shutil.move(filename, tfname)
        os.system("imcopy %s[-*,-*] %s" % (tfname, filename))
	os.unlink(tfname)
    if not os.access(filename,os.R_OK):
        return(None)
    return(filename)


def get_file_ids(object):
    """Get the exposure for a particular line in the meausre table"""
    import MOPdbaccess
    mysql = MOPdbaccess.connect('cfeps','cfhls',dbSystem='MYSQL')
    cfeps=mysql.cursor()
    sql="SELECT file_id FROM measure WHERE provisional LIKE %s"
    cfeps.execute(sql,(object, ))
    file_ids=cfeps.fetchall()

    return (file_ids)


if __name__=='__main__':
        ### Must be running as a script
        import optparse, sys
        from optparse import OptionParser

        parser=OptionParser()
        parser.add_option("--verbose","-v",
                          action="store_true",
                          dest="verbose",
                          help="Provide feedback on what I'm doing")

        (opt, objects)=parser.parse_args()

        import re,os
        for obj in objects:
            file_ids=get_file_ids(obj)
            for file_id in file_ids:
	        filename=file_id[0]+".fits"
                file_id=file_id[0]
                print file_id
                _sre_FILE_ID = re.compile('(?P<file_id>\d*\D)(?P<ccd>\d*)')
                ID = _sre_FILE_ID.match(file_id)
                if not ID:
                    print "Bad file_id"
                    continue
                file_id=ID.group("file_id")
                ccd=ID.group("ccd")
                print file_id,ccd
		if ( not os.access(filename,os.R_OK)):
                    filename=get_flipped_ext(file_id,ccd)	
                    os.system("wcsUpdate.pl -f %s" %(filename))
                ### Now run fitsIngest on this file
                os.system("fitsIngest.pl %s" %(filename))

            #sys.exit(0)

        ### Now create an ABG file for UNI to use.
            sql ="SELECT mpc,measure,file_id FROM measure WHERE provisional LIKE %s"
            import MOPdbaccess
            mysql = MOPdbaccess.connect('cfeps','cfhls',dbSystem='MYSQL')
            cfeps=mysql.cursor()
            cfeps.execute(sql,(obj, ))
            measures=cfeps.fetchall()
            ast_file="%s.ast" % ( obj,)
            abg_file="%s.abg" % ( obj,)
            if os.access(abg_file,os.R_OK):
                os.unlink(abg_file)
            ast =open(ast_file,"w")
            these_measures=[]
            for measure in measures:
                ast.write("#L %s \n" % ( measure[2],))
                ast.write("%s \n" % ( measure[0], ))
                these_measures.append(measure[1])
            ast.close()
            command="uni.pl -o %s " % ( obj, ) 
            status=os.system(command)
            if (status==0) :
                sql="INSERT source (measure, provisional) VALUES ( %s , %s)"
                for measure in these_measures :
                    cfeps.execute(sql,(measure,obj))
                
