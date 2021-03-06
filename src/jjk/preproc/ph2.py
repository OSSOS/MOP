#!/usr/bin/env python


import RO.StringUtil

deg_2_dms=RO.StringUtil.dmsStrFromDeg


sql="select p.* from opposition o join pointings p on o.pointing=p.id join association a on a.pointing=p.id JOIN blocks b on b.expnum=a.expnum where runid LIKE '06AQ02' and qname LIKE '03BQ06A' group by p.id"

import MOPdbaccess

mysql=MOPdbaccess.connect('bucket','cfhtlsvw','MYSQL')
bucket=mysql.cursor()
bucket.execute(sql)
rows=bucket.fetchall()
print """
<?xml version = "1.0"?>
<!DOCTYPE ASTRO SYSTEM "http://vizier.u-strasbg.fr/xml/astrores.dtd">
<ASTRO ID="v0.8" xmlns:ASTRO="http://vizier.u-strasbg.fr/doc/astrores.htx">
<TABLE ID="Table">
   <NAME>Fixed Targets</NAME>
   <TITLE>Fixed Targets for CFHT QSO</TITLE>
   <!-- Definition of each field -->
   <FIELD name="NAME" datatype="A" width="20">
       <DESCRIPTION>Name of target</DESCRIPTION>
   </FIELD>
   <FIELD name="RA" datatype="A" width="11" unit="h" format="RAh:RAm:RAs">
       <DESCRIPTION>Right ascension of target</DESCRIPTION>
   </FIELD>
   <FIELD name="DEC" datatype="A" width="11" unit="deg" format="DEd:DEm:DEs">
       <DESCRIPTION>Declination of target</DESCRIPTION>
   </FIELD>
   <FIELD name="EPOCH" datatype="F" width="6">
   <DESCRIPTION>Epoch of coordinates</DESCRIPTION>
   </FIELD>
   <FIELD name="POINT" datatype="A" width="5">
   <DESCRIPTION>Pointing name</DESCRIPTION>
   </FIELD>
   <!-- Data table -->
<DATA><CSV headlines="4" colsep="|">
<![CDATA[
NAME                |RA         |DEC        |EPOCH |POINT|
                    |hh:mm:ss.ss|+dd:mm:ss.s|      |     |
12345678901234567890|12345678901|12345678901|123456|12345|
--------------------|-----------|-----------|------|-----|"""

sign='+'
for row in rows:
    if row[2]>0:
	    sign='+'
    if row[2]<0:
	    sign='-'
    print "%-20s|%11s|%s%10s|%5.1f|%-5d|" % ( row[1],deg_2_dms(row[2]/15.0),sign,deg_2_dms(row[3]),2000.0,1)

print"""
]]></CSV></DATA>
</TABLE>
</ASTRO>"""
