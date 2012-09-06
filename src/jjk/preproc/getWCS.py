#!/usr/cadc/misc/bin/python
#/*+
#************************************************************************
#****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
#*
#* (c) <2007>.				(c) <2007>.
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
#*   Script Name:	ephemSearch.py
#*
#*   Purpose:
#*	Proxy delivering JPEG cutouts of CFHT data. Datasets are CFH12K,
#*	public MegaPrime, CFHTLS elixir and CFHTLS Terapix. Users enter
#*	RA, DEC, cutout radius and filter. 
#*
#*   Date		: Nov 1, 2005
#*
#*   Field RCS data	: %data%
#*	Module Name	: %M%
#*	Version Number	: %I%
#*	Release Number	: %R%
#*	Last Updated	: %G%
#*
#*   Programmer		: JJ Kavelaars
#*
#*   Modification History:
#*
#****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
#************************************************************************
#-*/
#################################
# Import required Python modules
###########################sys.
import sys

import sys, os, string, time, RO, cgi
import cgitb
cgitb.enable()


#####################################
# look up SYBASE account information
#####################################
import Sybase
db_name = 'cfht'
user_name='cadcuser51'
user_passwd='MhU7nuvP5/67A:31:30'
#dbinfo = os.popen('/usr/cadc/local/scripts/dbrc_get SYBASE cfht').read()
#print dbinfo
#user_name = string.split(dbinfo, ' ')[0]
#user_passwd = string.split(dbinfo, ' ')[1][:-1]
orbfit= "/usr/cadc/misc/orbfit"



#####################
#####################
# start main routine
#####################
#####################

if __name__ == '__main__':
 
    import sys, RO.StringUtil 
    so=sys.stdout
    form=cgi.FieldStorage()
    if not form.has_key('expnum') or not form.has_key('extno'):
        so.write("Content-type: text/html\n\n")
        so.flush()

    	so.write("<html>Usage:wcsGet?expnum=######&extno=#")
	sys.exit(0)


    expnum=form.getvalue('expnum')
    extno=form.getvalue('extno')
    db = Sybase.connect('SYBASE', user_name, user_passwd, database=db_name)
    c = db.cursor()
   
    SQL  = "SELECT naxis1, naxis2, crpix1, crpix2, crval1, crval2, cd1_1, cd1_2, cd2_1,  cd2_2, ctype1, ctype2 "
    SQL += "FROM wcsInfoShort WHERE dataset_name = '%s' AND ext_no = %d " % ( expnum, int(extno))
    
    db = Sybase.connect('SYBASE', user_name, user_passwd, database=db_name)
    c = db.cursor()
    c.execute(SQL)
    row=c.fetchone()
    description=c.description
    for col in range(len(row)):
        so.write("%18s " % ( description[col][0]))
    so.write("\n")
    for col in range(len(row)):
        so.write("%18s " % str(row[col]))
    so.write("\n")
