#!/usr/bin/env python

import MOPdbaccess,sys

cfhlsvw = MOPdbaccess.connect('bucket','lsadmin','MYSQL')
bucket=cfhlsvw.cursor()


sql="SELECT ra_rad,dec_rad, mjdate, official,measure,mpc  FROM cfeps.measure c join cfeps.object o on c.provisional like o.official where official like 'l3h09' ORDER BY official,mjdate ";
bucket.execute(sql)
measures =bucket.fetchall()

proxy_url="http://jkavelaars:***REMOVED***@data.cadc-ccda.hia-iha.nrc-cnrc.gc.ca/cfhtProxies/getData"
import math,os

r2d=180.0/math.pi 
for measure in measures:
    sql="SELECT expnum FROM exposure where ABS(ra - %s)<0.5*cos(`dec`/57.3) and ABS(`dec` - %s)<0.5 and ABS(mjdate - %s)*24*3600<90 "
    bucket.execute(sql,(measure[0]*r2d,measure[1]*r2d,measure[2]))
    exps = bucket.fetchall()
    for exp in exps:
        sql="UPDATE cfeps.measure  SET file_id=%s WHERE measure=%s"
        bucket.execute( sql % ( exp[0],measure[4]))
	
        #print ("%s\n" % (measure[5],));
        os.system("wget '%s?dataset_name=%s&cutout=circle(%12.5f,%12.5f,%5.3f)' -O l3h09_%d.fits " % (proxy_url,exp[0],measure[0]*r2d,measure[1]*r2d,1.0/60.0,exp[0]) )

cfhlsvw.commit()
cfhlsvw.close()
