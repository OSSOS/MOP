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
#*   Script Name:	cfhtCutout
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
#################################
# Import required Python modules
###########################sys.
import sys
sys.stdout.write("Content-type: text/html\n\n")
sys.stdout.flush()


import sys, os, string, time, RO, cgi
import cgitb
cgitb.enable()


#####################################
# look up SYBASE account information
#####################################
import Sybase
db_name = 'cfht'
dbinfo = os.popen('/usr/cadc/local/scripts/dbrc_get SYBASE cfht').read()
user_name = string.split(dbinfo, ' ')[0]
user_passwd = string.split(dbinfo, ' ')[1][:-1]
orbfit= "/usr/cadc/misc/orbfit"


def resolve(object):
    """Look up the name of a source using a resolver"""


    import re
    sesame_cmd = 'curl -s http://cdsweb.u-strasbg.fr/viz-bin/nph-sesame/-oI?'+string.replace(object,' ','')
    f = os.popen(sesame_cmd)
    lines = f.readlines()
    f.close()
    for line in lines:        
        if re.search('%J ', line):
            result2 = line.split()
            ra_deg = float(result2[1])
            dec_deg = float(result2[2])
            return (ra_deg, dec_deg)
        
    return (0,0)

def htmIndex(ra,dec,htm_level=3):
    """Compute htm index of htm_level at position ra,dec"""
    import re
    if os.uname()[0] == "Linux": javabin = '/opt/java2/bin/java '
    htm_level = htm_level
    verc_htm_cmd = javabin+'-classpath /usr/cadc/misc/htm/htmIndex.jar edu.jhu.htm.app.lookup %s %s %s' % (htm_level, ra, dec)

    for result in os.popen( verc_htm_cmd ).readlines():
        result = result[:-1]
        if re.search("ID/Name cc", result):
            (void, coord ) = result.split("=")
            (void, junk, htm_index) = coord.split(" ")
            return htm_index


################
# set MIME type
################OB

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

def gather_data(dform):

    import re, string, RO.StringUtil
    cdata = {}


    ########################
    # convert RA and DEC to degrees
    ########################
    dkey={'ra': 'ra_sexg', 'dec': 'dec_sexg', 'cutout': 'cutout_radius'}
    ckey={'ra': 'ra_deg', 'dec': 'dec_deg', 'cutout': 'radius_deg'}

    for param in ['ra', 'dec', 'cutout']:
        sexg = str(dform[dkey[param]].value.replace(' ',':'))
        if RO.StringUtil.checkDMSStr(sexg):
            cdata[ckey[param]]=RO.StringUtil.degFromDMSStr(sexg)
        else:
            try:
                cdata[ckey[param]]=float(sexg)
            except:
                cdata[ckey[param]]=0


    return cdata

def find_images(cdata,boxsize):
    import os

    intersect_info = {}

    #################################################
    # get datasets that contain cutout circle center
    #################################################
    db = Sybase.connect('SYBASE', user_name, user_passwd, database=db_name)
    dbcmd = """
    SELECT w.file_id,w.ext_no,creation_date, 
    CASE WHEN release_date<getdate() THEN 1 ELSE 0 END release, w.*
    FROM wcsInfo w
      JOIN cfht_received r ON w.dataset_name=r.dataset_name
      JOIN exposure e ON e.expnum=r.expnum
    WHERE w.htm_index LIKE '%s%s' ORDER BY creation_date
    """ % (cdata['htm_index'],'%')

    c = db.cursor()
    c.execute(dbcmd)
    wcsInfo = c.fetchall()
    import wcsutil

    intersect_dataset = []
    intersect_extnum = []
    intersect_cutout=[]
    intersect_date=[]
    intersect_public=[]

    import string
    largest_area_value=-1
    largest_area_dataset=''
    
    for dataset in wcsInfo:
        ### build a WCSObject
        wcsDict={}
        for i,elt in enumerate(dataset):
            wcsDict[string.upper(c.description[i][0])]=elt
        wcs=wcsutil.WCSObject(wcsDict)
        (cutout, area)=circTOcutout(wcs,cdata['RA'],cdata['DEC'],max(boxsize,cdata['RADIUS']))
        key=str(dataset[0])+string.zfill(int(dataset[1]),2)
        if cutout==None:
            continue
        if area>largest_area_value:
            largest_area_value = area
            largest_area_dataset = key
            
        intersect_cutout.append( cutout)
        intersect_dataset.append(str(dataset[0]))
        intersect_extnum.append( str(dataset[1]))
	intersect_date.append(str(dataset[2]))
	intersect_public.append(str(dataset[3]))
        
    c.close()
    db.close()

    ##############################################################
    # construct dictionary of nested dictionaries to be returned
    ##############################################################
    intersect_info['dataset'] = intersect_dataset
    intersect_info['extnum'] = intersect_extnum
    intersect_info['cutout'] = intersect_cutout
    intersect_info['date'] = intersect_date
    intersect_info['public'] = intersect_public

    data_ok = -1
    if len(intersect_dataset)>0:
        data_ok = 1

    return largest_area_dataset,intersect_info, data_ok

def generate_url(file_id,extno,cutout,proxy_url= 'http://www.cadc.hia.nrc.gc.ca/anonProxy/getData'):
    return "%s?file_id=%s&extno=%d&cutout=%s&archive=CFHT" % ( proxy_url,
                                                                             file_id,
                                                                             int(extno),
                                                                             cutout )
#####################
#####################
# start main routine
#####################
#####################

if __name__ == '__main__':
 
    import sys, RO.StringUtil 
    so=sys.stdout
    form=cgi.FieldStorage()
    if not form.has_key('submit') or ( not form.has_key('object') and (not form.has_key('ra') or not form.has_key('dec'))):
        print """
        <HTML>
        <HEAD>
        <TITLE> CFHT Image Cutout Service </TITLE>
        </HEAD>
        <BODY BGCOLOR="#FFFFFF">
        <H1><A HREF="/cfht/cfhtls_priv/cfhtCutout_help.html"><CENTER>CFHT Image Cutout Service</CENTER></A></H1>
        <TABLE BORDER=0 WIDTH=100%>
        <TR>
        <TD ALIGN=LEFT WIDTH=150><A HREF=http://www.gc.ca><IMG SRC=http://cadcwww.hia.nrc.ca/images/canada.gif BORDER=0></A></TD>
        <TD ALIGN=CENTER><A HREF=http://cadcwww.hia.nrc.ca><IMG SRC=http://cadcwww.hia.nrc.ca/icons/cadc.gif BORDER=0></A></TD>
        <TD ALIGN=RIGHT WIDTH=150><A HREF=http://www.nrc.ca><IMG SRC=http://cadcwww.hia.nrc.ca/images/nrc.5.gif BORDER=0></A></TD>
        </TR>
        </TABLE>
        <CENTER>
        <H3>Enter your desired qualifiers in the fields below and click the submit button.</H3>
        </CENTER>
        """    
    	so.write("<form method=post enctype='multipart/form-data' action='cfhtCutout'>")
        so.write("Object: <input name=object /><br />")
	so.write("RA: <input name=ra /> DEC: <input name=dec /> <br />")
        so.write("Radius: <input name=radius /> (in arc minutes)<br />")
        so.write("<input name=submit value=Search type=submit>")
        print """<HR>    <CENTER>    <TABLE BORDER=0 width=100%>    <TR>    <TD ALIGN=LEFT BORDER=1 width=150><A HREF=http://www.gc.ca><IMG SRC=/images/canada.gif BORDER=0></A></TD>    <TD ALIGN=CENTER>Please report any problems to the <A HREF=mailto:cadc@hia.nrc.ca>CADC</A>    <TABLE ALIGN=CENTER BORDER=0 cellpadding=0 cellspacing=0>    <TR>    <TD><A HREF=/ TARGET=_top><IMG BORDER=0 NAME=home SRC=/icon2000/home.gif ALT=Go to the CADC home page></A></TD>
        <TD><A HREF=mailto:cadc@nrc.ca><IMG BORDER=0 NAME=mailto SRC=/icon2000/mailto.gif ALT=Send e-mail to the CADC></A></TD>
        <TD><A HREF=/staff/staff.html TARGET=_top><IMG BORDER=0 NAME=reach SRC=/icon2000/reach.gif ALT=CADC staff and contact information></A></TD>
        </TR>
        </TABLE>
        </TD>
        <TD ALIGN=RIGHT width=150><A HREF=http://www.nrc.ca><IMG SRC=/images/nrc.5.gif BORDER=0></A></TD>
        </TR>
        </TABLE>
        </CENTER>
        </BODY>
        </HTML>
        """

        sys.exit(0)
    import string

    obj={}
    obj['name']=''

    try:
        if form.has_key('object'):
            obj['name']=form.getvalue('object')
        if len(obj['name'])>0:
            (obj['RA'],obj['DEC'])=resolve(obj['name'])
        else:
            dec=string.replace(form.getvalue('dec'),' ',':')
            dec=RO.StringUtil.degFromDMSStr(dec)
            obj['RA']=15.0*RO.StringUtil.degFromDMSStr(string.replace(form.getvalue('ra'),' ',':'))
            obj['DEC']=dec
    
        obj['htm_index']=htmIndex(obj['RA'],obj['DEC'],htm_level=5)

	radius=0
        if form.has_key('radius'):
	    try:
                radius=float(form.getvalue('radius'))
  	    except:
	        radius=0
        if radius>0:
            obj['RADIUS']=radius/60.0
        else:
            obj['RADIUS']=2.0/60.0
        so.write("Resolved object %s \n<br />" % ( obj['name']))
        so.write("Searching Near: %s %s %s \n<br />" % ( obj['RA'], obj['DEC'], obj['htm_index']))
    except:
        so.write("<b />Failed while converting RA/DEC to htm Index<br />")
        if len(obj['name'])>0:
            so.write("Resolved object %s \n<br />" % ( obj['name']))
        so.write("Values: %s %s %s \n<br />" % ( obj['RA'], obj['DEC'], obj['htm_index']))
        sys.exit(0)
        

    (largest,intersect_info,data_ok)=find_images(obj,obj['RADIUS'])
    print "<TABLE><TR><TH>Creation Date</th><TH>PREVIEW</TH></TR>\n"
    if data_ok>0:
        for key in range(len(intersect_info['dataset'])):
            #for key in (largest, ):
            file_id=intersect_info['dataset'][key]
            extno=intersect_info['extnum'][key]
            cutout=intersect_info['cutout'][key]
	    date=intersect_info['date'][key]
            print "<TR><TD>%s</tD>" % (date,)
	    if intersect_info['public'][key]==1:
                url=generate_url(file_id,extno,cutout)
                print "<TD><a href='cfhtCutout_aladin?aladin_url="+url.encode('hex_codec')+"'>%s</a></TD></TR>" % (url)
	    else:
                url=generate_url(file_id,extno,cutout,proxy_url='http://www.cadc.hia.nrc.gc.ca/authProxy/getData')
		print "<TD><a href="+url+">"+url+"</a></TD></TR>"
            #print "<p />\n"
            sys.stdout.flush()
    else:
        print 'Failed to find any matching frames \n'
    print "</TABLE>"


