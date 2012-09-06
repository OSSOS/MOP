#
#+
########################################################################
#
#   Class:	table
#
#   Purpose:
#	A convenience class for accessing database tables, for use in www
#       displayed in a www page (a'la wdbi)
#
#   Arguments:
#	table          : name of the table in the database
#	string         : header line for table listing
#
#   Return values:
#       column
#
########################################################################
#-
class Table:

    def __init__(self, table, header=None):
        self.table=table
        if not header:
            header=table
        self.header=header

    def discover(self):
        """
        Discovers column structure using a query to the
        db for a description of the table.
        """
        
#
#+
########################################################################
#
#   Class:	column
#
#   Purpose:
#	A convenience class for accessing database columns, for use in www
#       displayed in a www page (a'la wdbi)
#
#   Arguments:
#	column          : name of the column in the database
#	header          : header line for table listing of columns
#       format          : format of the column when displayed
#
#   Return values:
#       column
#
########################################################################
#-
class	Column:

    def __init__(self,column,header=None,format='%s'):
        self.column(column)
        self.header(header)
        self.format(format)

    def column(self,column=None):
        if column:
           self._column=column
        return(self._column)

    def header(self,header=None):
        if header:
            self._header=header
        if not self._header:
            self._header=self.column
        return(self._header)

    def format(self,format=None):
        if format:
            self._format=format
        if not self.format:
            self._format='%s'
        return(self._format)

    def value(self,value=None):
        if not value==None:
            self._value=value
        return(self._value)

    def __str__(self):
        return  self.format() % (self.value())




import MySQLdb
dbh = MySQLdb.connect(user='cfhls',
                      passwd='shift+add',
                      db='cfeps',
                      host='cadchd.hia.nrc.ca',
                      port=33306)

cfeps=dbh.cursor()


mags=[
    #Column("file_id","Dataset","%7s"),
    #Column("mjdate","Modified<BR>Julian Date","%12.5f"),
    #Column("ra_rad","RA","%12s"),
    #Column("dec_rad","DEC","%12s"),
    Column("avg(mag)","MAG","%12.1f"),
    Column("filter","Filter","%12s")]


orbs=[
    Column("official","Designation","%10s"),
    Column("a","a","%8.2f"),
    Column("e","e","%8.2f"),
    Column("i","i","%8.2f")
      ]



sql = "SELECT "
sep = ""
for col in orbs:
    sql=sql+sep+col.column()
    sep=","

sql+= " FROM orbits where official LIKE 'l%' order by official "

cfeps.execute(sql)
orbits=cfeps.fetchall()


sql = "SELECT "
sep = ""
for col in mags:
    sql=sql+sep+col.column()
    sep=","

sql = sql+" FROM measure m JOIN object o on m.provisional=o.provisional "
sql = sql+" WHERE o.official LIKE %s GROUP BY filter ORDER BY official,mjdate "

import sys
sys.stdout.write("Content-Type: text/html\n\n")
sys.stdout.flush()

## write header
sys.stdout.write("<TABLE><TR>\n")

sys.stdout.write("</TR>\n")

filters=["g","R","i"]

for orbit in orbits:
    sys.stdout.write('<TR>\n')
    for i in range(len(orbit)):
        orbs[i].value(orbit[i])
    for col in orbs:
        sys.stdout.write("<TD>"+str(col)+"</TD>")


    cfeps.execute(sql,(orbit[0]))
    infos=cfeps.fetchall()

    magnitudes={}
    for info in infos:
        mags[0].value(info[0])
        magnitudes[info[1]]=str(mags[0])

    
    for filter in filters:
        if magnitudes.has_key(filter):
            sys.stdout.write("<TD>"+magnitudes[filter]+"</TD>")
        else:
            sys.stdout.write("<TD>&nbsp;</TD>")

    sys.stdout.write('</TR>\n')
    
    
