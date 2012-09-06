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
#*   Script Name:	read_mop_file.py
#*
#*   Purpose:
#*	readin a MOP format file
#*      MOP FORMAT IS
#* ## KEYWORD
#* #  VALUE
#* ...
#* ## COL_HEADINGS
#*    ROWS
#*
#*
#*   Date		: <DEC, 17, 2004>
#*
#*   RCS data:
#*	$RCSfile: mop_files.py,v $
#*	$Revision: 1.4 $
#*	$Date: 2006/05/11 23:53:38 $
#*
#*   Programmer		: <your name>
#*
#*   Modification History:
#*
#****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
#************************************************************************
#-*/

import sys,warnings

def write(file,hdu,order=None,format=None):
    """Write a file in the crazy MOP format given an mop data hdu."""


    if order or format:
        warnings.warn('Use of <order> and <format> depricated',DeprecationWarning)

    ## if the order of the columns isn't constrained just use the keys in what ever order
    data='data'
    if not order:
        if not hdu.has_key('order'):
            hdu['order']=hdu[data].keys()
    else:
        hdu['order']=order
	
    if not format:
        if not hdu.has_key('format'):
            hdu['format']={}
        for o in hdu['order']:
            if not hdu['format'].has_key(o):
	       hdu['format'][o]='%10s'
    else:
        hdu['format']=format
	
    f=open(file,'w')
    kline='## '
    vline='#  '
    header='header'
    num=0
    for keyword in hdu[header]:
        kline+='%10s ' % (keyword, )
        vline+='%10s ' % str(hdu[header][keyword])
        num+=1
        if not ( num % 6 ) :
            num=0
            f.write(kline+'\n')
            f.write(vline+'\n')
            kline='## '
            vline='#  '
    if num > 0:
        f.write(kline+'\n')
        f.write(vline+'\n')

    f.write('## ')
    for column in hdu['order']:
        f.write(' %10s' % (column))
    f.write('\n')
    dlen=len(hdu[data][hdu['order'][0]])
    for i in range(dlen):
        f.write('   ')
        for column in hdu['order']:
            f.write(hdu['format'][column] % (hdu[data][column][i]))
            f.write(' ')
        f.write('\n')
    f.close()
    return

def store(hdu,dbase='cfeps',duser='cfhls',dtype='MYSQL',table='source'):
    """Write the contents of a MOP data structure to a SQL table"""

    import MOPdbaccess
    db=MOPdbaccess.connect(dbase,duser,dbSystem=dtype)
    dbc=db.cursor()

    ### INSERT THE 'HEADER' in the meta-data table
    file_id=hdu['header']['image']
    for key in hdu['header'].keys():
        if key == 'image':
            continue
        value=hdu['header'][key]
        ### require that the file_id+keyword is unique
        sql="DELETE FROM metadata WHERE file_id='%s' and keyword='%s' " % ( file_id, key)
        dbc.execute(sql)
        sql="INSERT INTO metadata (file_id, keyword, value) values ('%s','%s','%s' ) "% ( file_id, key, value)
        dbc.execute(sql)
    db.commit()

    sql="DELETE FROM source WHERE file_id LIKE '%s'  " % ( file_id )
    dbc.execute(sql)

    sql="INSERT INTO %s ( " % ( table)
    sep=" "
    values="("
    cols=hdu['hdu2sql'].keys()
    for col in cols:
        sql+=sep+hdu['hdu2sql'][col]
        values+=sep+" %s " 
        sep=', '
    values+=", '%s' )" % ( file_id ) 
    sql+=", file_id ) VALUES "+values
    #print sql
    #sys.exit()
    #values=[]
    for row in range(len(hdu['data'][cols[0]])):
        value=[]
        for col in cols:
            value.append(hdu['data'][col][row])
        dbc.execute(sql,value)
        
    
    db.commit()

    return 
                 
                 
        
    
def read(file):
    """Read in a file and create a data strucuture that is a hash with members 'header'
    and 'data'.  The 'header' is a hash of header keywords, the data is a hash of columns.

    To get to the nth element of column NAME use hdu[data][NAME][n].  To get the header
    information use hdu[header][KEYWORD]. """
    
    f = open(file,'r')
    lines=f.readlines()
    f.close()

    import re, string
    keywords=[]
    values=[]
    formats={}
    header={}
    cdata={}

    for line in lines:
        if ( re.match(r'^##',line)):
            ## header line
            m=string.split(string.lstrip(line[2:]))
            if not m:
                sys.stderr.write( "Ill formed header line in %s \ n " % ( file, ))
                sys.stderr.write( line)
                continue
            keywords=m
            add_id=False
            continue
        if ( re.match(r'^# ',line)):
            ## this is a keyword value line, must match the previous keyword line
            m=string.split(string.lstrip(line[1:]))
            values=m
            if len(values)!= len(keywords):
                sys.stderr.write( "Ill formed header, keyword/value missmatach\n")
            for index in range(len(values)):
                header[keywords[index]]=values[index]
	    ## blank the arrays, we've already stuck this into the header
            keywords=[]
            values=[]
            continue
        if ( re.match(r'^#F',line)):
            ## this is a format line for the columns needs to be a keyword line first
            if not keywords:
                sys.stderr.write("Cann't have formats without column names...\n")
                continue
            f=string.split(string.lstrip(line))
            if add_id:
                f.append('%8d')
            for col in keywords:
                formats[col]=f.pop(0)
        ### must be the actual data
        ### expect a previous header line to define column headers
        if not keywords:
            sys.stderr.write( "No keywords for data columns, assuming x,y,mag,msky\n")
            keywords=('X','Y','MAG','MSKY')
        values = string.split(string.lstrip(line))
        ### add the id value to the values array if needed
        if not 'ID' in keywords:
            keywords.append('ID')
            add_id=True
        if not cdata:
            for keyword in keywords:
                cdata[keyword]=[]
        if add_id:
            values.append(len(cdata[keywords[0]])+1) 
        if len(values)!=len(keywords):
            sys.stderr.write("Keyword and value index have different length?\n")
        for index in range(len(values)):
            cdata[keywords[index]].append(values[index])

    hdu={'data': cdata, 'header': header, 'format': formats, 'order': keywords}
    ### the hdu is a dictionary with the header and data
    return hdu


if __name__=='__main__':
    import optik

    parser=optik.OptionParser()
    parser.add_option("--stars",help="MOP file to read")
    (opt,args)=parser.parse_args()
    
    hdu=read(opt.stars)
    for key in hdu['header']:
        print key,hdu['header'][key]
    for key in hdu['data']:
        print key,hdu['data'][key]

