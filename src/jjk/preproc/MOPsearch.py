#!/usr/cadc/misc/bin/python
#/*+
#************************************************************************
#****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
#*
#* (c) <year>.				(c) <year>.
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
#*   Field SCCS data	: %Z%
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


def circTOcutout(wcs,ra,dec,rad):
    """Convert an RA/DEC/RADIUS to an imcopy cutout"""
    (x1,y1)=wcs.rd2xy((ra+rad/2.0,dec-rad/2.0))
    (x2,y2)=wcs.rd2xy((ra-rad/2.0,dec+rad/2.0))
    xl=min(x1,x2)
    xr=max(x1,x2)
    yl=min(y1,y2)
    yu=max(y1,y2)
    
    ### constrain the cutout to be inside the image
    x1=max(xl,1)
    x1=int(min(x1,wcs.naxis1))
    
    x2=max(xr,x1,1)
    x2=int(min(x2,wcs.naxis1))
    
    y1=max(yl,1)
    y1=int(min(y1,wcs.naxis2))
    y2=max(yu,y1,1)
    y2=int(min(y2,wcs.naxis2))

    area=(x2-x1)*(y2-y1)
    cutout="[%d:%d,%d:%d]" % ( x1,x2,y1,y2)

    if not y1<y2 or not x1<x2: cutout=None
    return (cutout, area)

def find_images(file_id,ra,dec):
    import os
    import MOPdbaccess
    intersect_info = {}

    #################################################
    # get datasets that contain cutout circle center
    #################################################
    db = MOPdbaccess.connect('cfeps','cfhls',dbSystem='MYSQL')

    dbcmd = "SELECT * FROM wcs w WHERE file_id='%s'" % ( file_id)
    c = db.cursor()
    c.execute(dbcmd)
    wcsInfo = c.fetchall()
    db.close()
    import wcsutil,string

    for dataset in wcsInfo:
        ### build a WCSObject
        wcsDict={}
        for i,elt in enumerate(dataset):
            wcsDict[string.upper(c.description[i][0])]=elt
        wcs=wcsutil.WCSObject(wcsDict)
        print wcs
        (x1,y1)=wcs.rd2xy((ra,dec))

    return (x1,y1)


if __name__ == '__main__':

    import sys
    print find_images(sys.argv[1],sys.argv[2],sys.argv[3])


