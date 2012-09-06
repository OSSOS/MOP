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

import sys

def write(file,hdu,order=None,format=None):
    """Write a file in the crazy MOP format given an mop data hdu."""

    ## if the order of the columns isn't constrained just use the keys in what ever order
    data='data'
    if not order:
        order=hdu[data].keys()
    if not format:
        format={}
        for o in order:
            format[o]='%10s'
            
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
    for column in order:
        f.write(' %10s' % (column))
    f.write('\n')
    dlen=len(hdu[data][order[0]])
    for i in range(dlen):
        f.write('   ')
        for column in order:
            f.write(format[column] % (hdu[data][column][i]))
            f.write(' ')
        f.write('\n')
    f.close()
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
            continue
        if ( re.match(r'^# ',line)):
            ## this is a keyword value line.
            m=string.split(string.lstrip(line[1:]))
            values=m
            if len(values)!= len(keywords):
                sys.stderr.write( "Ill formed header, keyword/value missmatach\n")
            for index in range(len(values)):
                header[keywords[index]]=values[index]
            keywords=[]
            values=[]
            continue
        ### must be the actual data
        ### expect a previous header line to define column headers
        if not keywords:
            sys.stderr.write( "No keywords for data columns, assuming x,y,mag,msky\n")
            keywords=('X','Y','MAG','MSKY')
        values = string.split(string.lstrip(line))
        if len(values)!=len(keywords):
            sys.stderr.write("Keyword and value index have different length?\n")
        if not cdata:
            for keyword in keywords:
                cdata[keyword]=[]
        for index in range(len(values)):
            cdata[keywords[index]].append(values[index])


    hdu={'data': cdata, 'header': header}
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

