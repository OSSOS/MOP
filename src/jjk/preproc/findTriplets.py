#!/usr/bin/env python
#/*+
#************************************************************************
#****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
#*
#* (c) <year>.				(c) <year>.
#* National Research Council		Conseil national de recherches
#* Ottawa, Canada, K1A 0R6 		Ottawa, Canada, K1A 0R6
#* All rights reserved			Tous droits reserves
#* 					
#* NRC disclaims any warranties,	Le CNRC denie toute garantie
#* expressed, implied, or statu-	enoncee, implicite ou legale,
#* tory, of any kind with respect	de quelque nature que se soit,
#* to the software, including		concernant le logiciel, y com-
#* without limitation any war-		pris sans restriction toute
#* ranty of merchantability or		garantie de valeur marchande
#* fitness for a particular pur-	ou de pertinence pour un usage
#* pose.  NRC shall not be liable	particulier.  Le CNRC ne
#* in any event for any damages,	pourra en aucun cas etre tenu
#* whether direct or indirect,		responsable de tout dommage,
#* special or general, consequen-	direct ou indirect, particul-
#* tial or incidental, arising		ier ou general, accessoire ou
#* from the use of the software.	fortuit, resultant de l'utili-
#* 					sation du logiciel.
#*
#************************************************************************
#*
#*   Script Name:	getData.py
#*
#*   Purpose:
#*	Get a batch of lsvw data observations from the cadcArchive
#*
#*   Functions:
#+	
#+	function_name	: Brief description
#*	...
#*
#*   Date		: <mmm dd, yyyy>
#*
#*   RCS data:
#*	$RCSfile: findTriplets.py,v $
#*	$Revision: 1.6 $
#*	$Date: 2006/05/11 23:53:38 $
#*
#*   Programmer		: <your name>
#*
#*   Modification History:
#*
#****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
#************************************************************************
#-*/
"""Search through the sql.db and return a list of exposures that comprise
a discovery tripple.

"""


def getNewTriples():

    ### sql statement to select a list of triples with discovery observations.


    night=" floor(e.mjdate-0.0833) "
    
    sql="SELECT distinct(pointing) ,"+night+" night "
    sql=sql+"FROM bucket.exposure e "
    sql=sql+"JOIN bucket.association a on e.expnum=a.expnum "
    sql=sql+"LEFT JOIN triple_members t ON e.expnum=t.expnum "
    sql=sql+"LEFT JOIN bucket.blocks b ON e.expnum=b.expnum "
    sql=sql+"WHERE t.expnum IS NULL and b.block LIKE 'll%' " 
    sql=sql+"GROUP BY a.pointing, "+night
    sql=sql+"HAVING count(*)>2 "

    cfeps.execute(sql)
    fields=cfeps.fetchall()
    return(fields)


def getExpInfo(expnum):
    """Return a dictionary of information about a particular exposure"""

    col_names=['object',
               'e.expnum',
               'mjdate',
               'uttime',
               'filter',
               'elongation',
               'obs_iq_refccd',
               'triple', 'qso_status']

    sql="SELECT "
    sep=" "
    for col_name in col_names:
        sql=sql+sep+col_name
        sep=","
    sql=sql+" FROM bucket.exposure e "
    sql=sql+" JOIN bucket.circumstance c ON e.expnum=c.expnum "
    sql=sql+" LEFT JOIN triple_members t ON e.expnum=t.expnum "
    sql=sql+" WHERE e.expnum=%d " % ( expnum ) 
    #sys.stderr.write(sql);
    cfeps.execute(sql)
    rows=cfeps.fetchall()
    #print rows
    result={}
    #import datetime
    for idx in range(len(rows[0])):
        result[col_names[idx]]=rows[0][idx]
    
    return(result)

def getTripInfo(triple):
    """Return a dictionary of information about a particular triple"""

    col_names=['mjdate', 'filter', 'elongation', 'discovery','checkup', 'recovery', 'iq','block' ]
    sql="SELECT mjdate md,"
    sql=sql+" filter, avg(elongation), d.id, checkup.checkup, recovery.recovery , avg(obs_iq_refccd), b.qname "
    sql=sql+"FROM triple_members t JOIN bucket.exposure e ON t.expnum=e.expnum "
    sql=sql+"JOIN bucket.blocks b ON b.expnum=e.expnum "
    sql=sql+"JOIN bucket.circumstance c on e.expnum=c.expnum "
    sql=sql+"LEFT JOIN discovery d ON t.triple=d.triple "
    sql=sql+"LEFT JOIN checkup  ON t.triple=checkup.triple "
    sql=sql+"LEFT JOIN recovery ON t.triple=recovery.triple "
    sql=sql+"WHERE t.triple=%s "
    sql=sql+"GROUP BY t.triple ORDER BY t.triple "
    cfeps.execute(sql,(triple, ) )
    rows=cfeps.fetchall()
    result={}
    #import datetime
    for idx in range(len(rows[0])):
        result[col_names[idx]]=rows[0][idx]

    return result

def getExpnums(pointing,night=None):
    """Get all exposures of specified pointing ID.

    Default is to return a list of all exposure numbers"""

    if night:
        night=" floor(e.mjdate-0.0833)=%d " % ( night ) 
    else:	
        night=''
    
    sql="SELECT e.expnum "
    sql=sql+"FROM bucket.exposure e "
    sql=sql+"JOIN bucket.association a on e.expnum=a.expnum "
    sql=sql+"WHERE a.pointing="+str(pointing)+" AND "+night
    sql=sql+" ORDER BY mjdate, uttime DESC "
    cfeps.execute(sql)

    return(cfeps.fetchall())

def getTriples(pointing):
    """Get all triples of a specified pointing ID.

    Defaults is to return a complete list triples."""

    sql="SELECT id FROM triples t join triple_members m ON t.id=m.triple"
    sql+=" join bucket.exposure e on e.expnum=m.expnum "
    sql+=" WHERE pointing=%s  group by id  order by e.expnum  "
    cfeps.execute(sql, ( pointing, ) )
    return(cfeps.fetchall())

def getPointingsWithTriples():
    """Get a list of Pointings that have Triples"""

    cfeps.execute(" SELECT distinct(pointing), name FROM triples t join bucket.pointings p on t.pointing=p.id where name LIKE 'll%' order by p.ra")
    return(cfeps.fetchall())

def createNewTriples(Win):
    """Add entries to the triples tables based on new images in the db"""

    win.help("Building list of exposures to look for triples")

    cols=('e.expnum', 'object',
      'mjdate',
      'uttime',
      'elongation',
      'filter',
      'obs_iq_refccd','qso_status' )
    header='%6s %-10s%-12s%10s%10s%10s%8s%10s' %  cols  


    pointings=getNewTriples()
    num_p=len(pointings)

    for pointing in pointings:
        pid=pointing[0]
	mjd=pointing[1]
        expnums=getExpnums(pointing=pid,night=mjd)
        num_p=num_p-1
        while (1):
            ### Loop over this pointing until keystroke gets us out

            win.help("Select (space)  members of triplets - %d remaining" % num_p )

            ## start with an empty list
            explist=[]
            choices=[]

            current_date=''
            for expnum in expnums:
                info=getExpInfo(expnum[0])
                row=()
                if not str(info['triple'])=='None' :
                       continue
                if str(info['obs_iq_refccd'])=='None':
                    info['obs_iq_refccd']=-1.0

                choices.append('%6d %10s %15s %10s %8.2f %10s %8.2f %10s' % (
                    int(info['e.expnum']),
                    str(info['object']),
                    str(info['mjdate']),
                    str(info['uttime']),
                    float(str(info['elongation'])),
                    str(info['filter']),
                    float(str(info['obs_iq_refccd'])),
                    str(info['qso_status'])
                    ))
                explist.append(expnum[0])

            if len(choices)<3:
                ### we need to provide at least 3 choices,
                ### otherwise this isn't a triple (is it)
                break

            ### win.list returns the user's choices as a list.
            choice_list=win.list(header,choices)

            ### zero length list implies were done.
            if choice_list==None:
                break

            ### is this actually a triple?
            if len(choice_list)!=3:
                win.help("Must have 3 members to make a tripple")
                continue

            ### Create a new line in the triple table
            sql = "INSERT INTO triples (id, pointing ) VALUES ( NULL, %s ) "
            cfeps.execute(sql, ( pid, ) )
	    sql = "SELECT id FROM triples WHERE pointing=%s order by id desc" 
	    cfeps.execute(sql, ( pid, ) )
	    ttt=cfeps.fetchall()
            triple= ttt[0][0]
	    win.help(str(triple))

            ### record the members of this new triple.
            sql = "INSERT INTO triple_members (triple, expnum) VALUES ( %s, %s)";
	    win.help(sql)

            for exp in choice_list:
                cfeps.execute(sql,(triple,explist[exp]))

    return(0)

def setDiscoveryTriples(win,table="discovery"):
    """Provide user with a list of triples that could be discovery triples"""

    win.help("Getting a list of pointings with triples from the CFEPS db")

    pointings=getPointingsWithTriples()
    win.help("Select the "+table+" triple form the list...")
    import time
    for pointing in pointings:
        header="%10s %10s %8s %10s %8s" % (pointing[1],'mjdate','Elongation','Filter', 'IQ')
        triples=getTriples(pointing=pointing[0])
        choices=[]
        triplist=[]
	no_type=0
	previous_list=[]
        for triple in triples:
	    #win.help(str(triple))
            tripinfo=getTripInfo(triple[0])
            if not tripinfo[table]==None:
	        previous_list.append(triple[0])
	    #if not abs(180-tripinfo['elongation'])< 20:
	    #	continue
            triplist.append(triple)
            if str(tripinfo['iq'])=='None':
                    tripinfo['iq']=-1.0 
            obs_type=' '
	    if tripinfo['discovery']:
	    	obs_type='D'
	    elif tripinfo['checkup']:
	        obs_type='C'
            elif tripinfo['recovery']:
	        obs_type='R'
	    if obs_type==' ':
	        no_type+=1
            line=(obs_type,tripinfo['mjdate'], tripinfo['elongation'],
                  tripinfo['filter'], tripinfo['iq'], tripinfo['block'] ) 
            choices.append('%10s %10s %8.2f %10s %8.2f %8s' % line)
        if len(choices)==0 or no_type==0:
            continue
	#if len(previous_list)==1:
	#    continue
        win.help("Choose a "+table+" triple (space) [no choice means skip] then press enter\n (q) to exit")
        choice=win.list(header,choices)
        if choice==None:
            win.help("Loading next triple")
            break
    ### Record which triplet is a discovery triplet
        if len(choice)!=1:
            win.help("Loading next triple\n")
            continue
        discovery_triple=triplist[choice[0]]
	for triple in previous_list:
	    sql="DELETE FROM "+table+" WHERE triple=%s "
	    cfeps.execute(sql,triple)
        sql="INSERT INTO "+table+" ( triple ) VALUES ( %s ) "
        cfeps.execute(sql,discovery_triple)


if __name__ == '__main__':
    import MOPdbaccess
    MySQL=MOPdbaccess.connect('cfeps','cfhls',dbSystem='MYSQL')
    cfeps=MySQL.cursor()
    import sys,re,string,os
    import MOPwindow
    win=MOPwindow.MOPwindow()
    
    
    tasks=["Search for New Triples",
           "Define discovery pointing",
	   "Define checkup pointing",
	   "Define recovery pointing"
	   ]
    
    while(1):
        #win.help("arrow to the task of your desire\n Press Space to select task")
        task = win.list("Choose a Task from the list",tasks)
	if not task:
	    break
        if task[0]==0:
            createNewTriples(win)
        elif task[0]==1:
            setDiscoveryTriples(win)
        elif task[0]==2:
            setDiscoveryTriples(win,table='checkup')
        elif task[0]==3:
            setDiscoveryTriples(win,table='recovery')
        
    win.end()

    sys.exit(0)

