#!/usr/bin/env python
import sys
sys.stdout.write("Content-Type: text/html\n\n")
sys.stdout.flush()
import cgi

import MySQLdb
dbh = MySQLdb.connect(user='cfhls',
                      passwd='***REMOVED***',
                      db='cfeps',
                      host='cadchd.hia.nrc.ca',
                      port=33306)

cfeps=dbh.cursor()

def mk_dict(results,description):
    """Given a result list and descrition sequence, return a list
    of dictionaries"""
    
    rows=[]
    for row in results:
        row_dict={}
        for idx in range(len(row)):
            col=description[idx][0]
            row_dict[col]=row[idx]
        rows.append(row_dict)
    return rows
    

def get_orbits(official='%'):
    """Query the orbit table for the object whose official desingation
    matches parameter official.  By default all entries are returned
    """
   
    sql= "SELECT * FROM orbits WHERE official LIKE '%s' " %  (official, ) 
    cfeps.execute(sql)
    return mk_dict(cfeps.fetchall(),cfeps.description)


def get_astrom(official='%',provisional='%'):
    """Query the measure table for all measurements of a particular object.
    Default is to return all the astrometry in the measure table,
    sorted by mjdate"""

    sql= "SELECT m.* FROM measure m "
    sql+="LEFT JOIN object o ON m.provisional LIKE o.provisional "
    if not official:
        sql+="WHERE  o.official IS NULL"
    else:
        sql+="WHERE o.official LIKE '%s' " % ( official, ) 
    sql+=" AND m.provisional LIKE '%s' " % ( provisional, )

    cfeps.execute(sql)
    return mk_dict(cfeps.fetchall(), cfeps.description)


def getData(file_id,ra,dec):
    """Create a link that connects to a getData URL"""
    DATA="www.cadc-ccda.hia-iha.nrc-cnrc.gc.ca"
    BASE="http://"+DATA+"/authProxy/getData"
    archive="CFHT"
    wcs="corrected"
    import re
    groups=re.match('^(?P<file_id>\d{6}).*',file_id)

    if not groups:
        return None
    
    file_id=groups.group('file_id')
    file_id+="p"
    #### THIS IS NOT WORKING YET....
    URL=BASE+"?dataset_name="+file_id+"&cutout=circle("+str(ra*57.3)+","
    URL+=str(dec*57.3)+","+str(5.0/60.0)+")"

    return URL

        

form=cgi.FieldStorage()
if form.has_key('official'):
    print "<TABLE>"
    ### provide a list of 'data links'
    measures=get_astrom(form.getfirst('official'))
    for row in measures:
        print "<TR>"
        url=getData(row['file_id'],row['ra_rad'],row['dec_rad'])
        print "<TD><A HREF=%s>%s</A></TD>" % (url, row['file_id'])
        print "<TD>%s</TD>" % ( row['mpc'], )
        print "</TR>"
    sys.exit();

orbits=get_orbits()

print "<TABLE>"
for row in orbits:
    print "<TR>"
    print "<TD><A HREF='cfeps_object.py?official=%s'>%s</A></TD>" % ( row['official'], row['official'])
    for col in ['a','e','i']:
        print "<TD>%s</TD>" % ( row[col])
    print "</TR>"

print "</TABLE>"
    
