#!/usr/cadc/misc/bin/python
#*
#*
#*   RCS data:
#*	$RCSfile: MOPdbaccess.py,v $
#*	$Revision: 1.2 $
#*	$Date: 2006/05/11 23:53:38 $
#*
#*   Programmer		: JJ Kavelaars
#*
#*   Modification History:
#*      $RCSlog$
#*
#****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
#************************************************************************
#-*/
"""Create a connection to the MOP db system using values stored in the
a users .dbrc file.  Values stored in the file can be over-riden using command
line options.

Each user should  have a readonly resource file $HOME/.dbrc
This file has the format

DBTYPE  database  user password.
"""

### search the .dbrc file and create a dictionary of user/password pairs

__system='MYSQL'
__database='cfeps'


def _get_db_options(args):
    """Parse through a command line of arguments to over-ride the values
    in the users .dbrc file.

    If no user name is given then the environment variable $USERNAME is
    used. If $USERNAME is not defined then prompt for input.
    """
    import optik, getpass,sys
    from optik import OptionParser
    parser=OptionParser()
    parser.add_option("-d","--database",
                  action="store", type="string", dest="database",
                  default="cfht",
                  help="Name of the SYBASE database containing TABLE",
                  metavar="FILE")
    parser.add_option("-u","--user",
                  action="store", type="string", dest="user",
                  default=getpass.getuser(),
                  help="User name to access db with",
                  metavar="USER")
    (opt, unused_args) = parser.parse_args(args)
    return opt.database,opt.user,unused_args

def _get_db_password(dbSystem,db,user):
    """Read through the users .dbrc file to get password for the db/user
    combination suplied.  If no password is found then prompt for one
    """
    import string, getpass, os
    dbrc = os.environ['HOME']+"/.dbrc"
    password={}
    if os.access(dbrc,os.R_OK):
        fd=open(dbrc)
        lines=fd.readlines()
        for line in lines:
            entry=line.split()
            if entry[0]==dbSystem and entry[1]==db and entry[2]==user:
                return entry[3]

    return getpass.getpass()

def _get_db_connect(dbSystem,db,user,password):
    """Create a connection to the database specified on the command line
    """
    if dbSystem=='SYBASE':
        import Sybase    
        try:
            dbh = Sybase.connect(dbSystem,
                                 user,
                                 password,
                                 database=db )
        except:
            dbh=None
    elif dbSystem=='MYSQL':
        import MySQLdb
        try:
            dbh = MySQLdb.connect(user=user,
                                  passwd=password,
                                  db=db ,
                                  host='gimli')
        except:
            dbh=None
            
    return dbh

def connect(db,user,dbSystem='SYBASE',password=None):
    #(db,user)=_get_db_options(db,user)
    if password is None:
        password=_get_db_password(dbSystem,db,user)
    dbh= _get_db_connect(dbSystem,db,user,password)
    import time
    i=0
    while(dbh is None and i < 200 ):
        time.sleep(15)
	i+=1
        dbh= _get_db_connect(dbSystem,db,user,password)
    return dbh 

if __name__ == '__main__':
    import sys
    print connect(sys.argv)
