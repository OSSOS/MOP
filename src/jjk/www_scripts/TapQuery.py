
import atpy
import urllib,urllib2
import sys
import time
import MOPdbaccess


"""Query the CADC TAP service to determine the list of images for the
TVS Search
   
Logic:  Return all exposures associated with a particular runid

"""

inputListQuery="""SELECT collectionID AS expnum, target_name AS object, 
COORD1(CENTROID(Plane.position_bounds)) AS "ra", 
COORD2(CENTROID(Plane.position_bounds)) AS "dec", 
Plane.time_exposure AS exptime, 
Plane.time_bounds_cval1 as mjdate, 
Plane.energy_bandpassName as filter, 
proposal_id as runid
FROM caom.Observation AS Observation JOIN
caom.Plane AS Plane ON Observation.obsID = Plane.obsID WHERE 
proposal_id in ('11BC22','11BF25') and Plane.time_bounds_cval1 > 55889.2 and Plane.observable_ctype='RAW' ORDER BY collectionID"""


def TAPQuery(query):
    """The __main__ part of the script"""

    tapURL = "http://cadc-ccda.hia-iha.nrc-cnrc.gc.ca/tap/sync"


    ## Some default parameters for that TAP service queries.
    tapParams={'REQUEST': 'doQuery',
               'LANG':    'ADQL',
               'FORMAT':  'votable',
               'QUERY':   query}
    cnt=0
    while True:	
       try:
          print "running query"
          r=urllib2.urlopen(tapURL,urllib.urlencode(tapParams))
          return r
       except urllib2.HTTPError, e:
          cnt+=1
          if e.code!=503:
              sys.stderr.write("# TAP Query got Code: %s Attempt: %d (exiting)\n" % (str(e.code),cnt))
              sys.exit(-1)
          sys.stderr.write("# TAP Query got Code: %s Attempt: %d (sleeping for 10)\n" % (str(e.code),cnt))
          time.sleep(10)


queryResult=TAPQuery(inputListQuery)
tf = open('obs.vot','w')
for line in queryResult:
    tf.write(line)
tf.flush()
tf.close()
print "opening local votable"
t=atpy.Table('obs.vot',type='vo')
print t.columns
mysql=MOPdbaccess.connect('bucket','lsadmin','MYSQL')
bucket=mysql.cursor()
cols=("expnum","object","ra","dec","exptime","mjdate","filter","runid")
print cols
print t
#,"qrunid","date","uttime")

for irow in range(len(t)):
    
    sql="SELECT count(*) from bucket.exposure where expnum=%s" 
    bucket.execute(sql,(t[irow]['expnum']))
    count=bucket.fetchall()
    if count[0][0] > 0:
        continue
    
    ### now add to the exposure table too.
    sql = "INSERT INTO `exposure` (";
    sep = '';
    for col in cols:
	sql += sep+"`"+col+"`"
	sep=","
    sep ="";
    sql += " ) VALUES ( " 
    values=[]
    for col in  cols :
        values.append(t[irow][col])
	sql += sep + "%s"
	sep=","
    
    
    sql += " ) "
    bucket.execute(sql,values)



