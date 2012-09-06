#!/usr/bin/env python

import MOPdbaccess


db = MOPdbaccess.connect('cfeps','cfhls',dbSystem='MYSQL',password='shift+add')

cfhls=db.cursor()

cfhls.execute("SELECT official, a, e, i  FROM orbits WHERE official LIKE 'L3%'")
rows = cfhls.fetchall()


print "<TABLE>"
print "<TR><TH>OBJECT</TH><TH>a</TH><TH>e</TH><TH>i</TH></TR>"
for row in rows:
   print "<TR>"
   for col in row:
      print "<TD>%s</TD>" %(col)
   print "</TR>"
print "</table>"
