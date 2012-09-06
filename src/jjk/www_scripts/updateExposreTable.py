#!/usr/cadc/misc/bin/python

import MOPdbaccess,sys

cadc = MOPdbaccess.connect('cfht','kavelaar')
cfhlsvw = MOPdbaccess.connect('bucket','lsadmin','MYSQL')

bucket=cfhlsvw.cursor()
cfht=cadc.cursor()

sql="SELECT expnum FROM exposure " 
bucket.execute(sql)
rows=bucket.fetchall()
sql_insert="UPDATE exposure SET qso_status=%s, obs_iq_refccd=%s WHERE expnum=%s"
sql_select="SELECT qso_status,obs_iq_refccd FROM detrended WHERE expnum=@expnum"
for row in rows:
	expnum = int(row[0])
	cfht.execute(sql_select,{'@expnum': expnum} )
	exposures=cfht.fetchone()
	if not exposures:
		sys.stdout.write( "No IQ for "+str(expnum)+"     \r")
		continue

	sys.stdout.write(sql_insert % (exposures[0],exposures[1], str(expnum) ) )
	sys.stdout.write('\r')
	bucket.execute(sql_insert ,(exposures[0],exposures[1], expnum ) )
	if bucket.rowcount>0:
		sys.stdout.write("inserted qso_status and obs_iq_refccd for "+str(expnum)+" \r")
	else:
		sys.stdout.write("Nothing happend? \r")
	

cfhlsvw.commit()
	
