#!/usr/bin/env python

import MOPdbaccess,sys

mysql = MOPdbaccess.connect('cfeps','lsadmin','MYSQL')
sybase=MOPdbaccess.connect('cfht','kavelaar')
bucket=mysql.cursor()
cfht=sybase.cursor()


sql="SELECT ra_rad,dec_rad, mjdate, measure,file_id  FROM measure m join object o ON m.provisional=o.provisional where official LIKE 'l%' ORDER BY mjdate";
bucket.execute(sql)
measures =bucket.fetchall()


import string
for measure in measures:
    #sql="SELECT w.dataset_name,w.ext_no FROM wcsInfo w JOIN cfht_files c ON w.dataset_name=c.dataset_name JOIN exposure e on e.expnum=c.expnum where ra_1 < %s and ra_4 > %s and dec_1 > %s and dec_2 < %s and ABS(mjdate - %s)*24*3600<90.0 "
    sql="SELECT w.dataset_name,w.ext_no FROM wcsInfo w WHERE ra_1 < %s and ra_4 > %s and dec_1 > %s and dec_2 < %s AND dataset_name='%s' "
    thisRA=measure[0]*57.3
    thisDEC=measure[1]*57.3
    thisMJDATE=measure[2]
    thisMEASURE=measure[3]
    thisDATASET=measure[4]
    print measure[4]
    SQL= sql % (thisRA,thisRA, thisDEC, thisDEC, thisDATASET)
    print SQL 
    cfht.execute(SQL)
    wcsInfo = cfht.fetchone()
    print wcsInfo 
    thisFILE_ID="%6sp%2s" % ( str(wcsInfo[0]) ,string.zfill(str(wcsInfo[1]),2))
    sql="UPDATE  measure SET file_id=%s WHERE measure=%s"
    #bucket.execute( sql % ( thisFILE_ID,thisMJDATE))
    print  sql % ( thisFILE_ID, str(thisMJDATE))
    #print ("http://services.cadc-ccha.hia-iha.nrc-cnrc.gc.ca/cfhtProxies/getData?dataset_name%s&cutout=(%08.3f,%08.3f,%5.3f)" % (exp[0],measure[0]*57.3,measure[1]*57.3,1.0/60.0) )
    sys.exit(0)

