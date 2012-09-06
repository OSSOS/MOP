#!/usr/bin/env python

import MOPdbaccess,sys

cfhlsvw = MOPdbaccess.connect('bucket','lsadmin','MYSQL')
bucket=cfhlsvw.cursor()


#sql="SELECT ra_rad,dec_rad, mjdate, official,measure  FROM cfeps.measure c join cfeps.object o on c.provisional like o.official where official like 'l%' ORDER BY official,mjdate ";
sql="SELECT ra_rad,dec_rad, mjdate, measure  FROM cfeps.measure WHERE file_id LIKE 'unknown' and observatory=568 and mjdate >  52722.44745  "
bucket.execute(sql)
measures =bucket.fetchall()


for measure in measures:
    sql="SELECT expnum FROM exposure where ABS(ra - %s)<1.0*cos(`dec`/57.3) and ABS(`dec` - %s)<1.0 and ABS(mjdate - %s)*24*3600<90 "
    bucket.execute(sql,(measure[0]*57.3,measure[1]*57.2,measure[2]))
    exps = bucket.fetchall()
    for exp in exps:
        sql="UPDATE cfeps.measure SET file_id=%s WHERE measure=%s"
        #print sql % ( exp[0], measure[3])
        bucket.execute(sql,(exp[0],measure[3]) )
        #print ("http://services.cadc-ccha.hia-iha.nrc-cnrc.gc.ca/cfhtProxies/getData?dataset_name%s&cutout=(%08.3f,%08.3f,%5.3f)" % (exp[0],measure[0]*57.3,measure[1]*57.3,1.0/60.0) )

