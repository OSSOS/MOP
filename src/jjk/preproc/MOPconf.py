#!/usr/bin/env python

# A script to help with confirming detections of KBOs.

""" Look through all subdirectories for all the 'no_candidate' or 
    'file_id.cands.comb' files.  Building a list of things to check.
    Check if there are gaps in the listing. """


import os, sys,string, sre, pyraf


## read in the .cands.comb file
def read_cands(filename):
    """Read in the contents of a cands comb file"""
    import sre
    
    lines=file(filename).readlines()
    exps=[]
    cands=[]
    comments=[]
    coo=[]
    for line in lines:
        if ( line[0:2]=="##" ) :
	    comments.append(line) 
            break
        exps.append(line[2:].strip())

    for line in lines:
        if ( line[0]=="#" ) :
	    comments.append(line) 
            continue
        if len(line.strip())==0:
            if len(coo)!=0:
                cands.append(coo)
            coo=[]
            continue
        vals=line.split()
        cols=['x','y','x_0','y_0','flux','size','max_int','elon']
        values={}
        for j in range(len(cols)):
            col=cols.pop().strip()
            val=vals.pop().strip()
            values[col]=float(val)
        coo.append(values)
    cands.append(coo)
    return {'fileId': exps, 'cands': cands, 'comments': comments}

def discands(record):
    """Display the candidates contained in a candidate record list"""
    import pyfits
    pyraf.iraf.images()
    pyraf.iraf.images.tv()
    display = pyraf.iraf.images.tv.display
    
    width=128
    
    cands = record['cands']
    exps=   record['fileId']
    comments= record['comments']

    ### load some header info from the mophead file
    headers={}
    for exp in exps:
        f = pyfits.open(exp+".fits")
        headers[exp]={}
        for key in ['MJDATE', 'NAXIS1', 'NAXIS2', 'EXPTIME', 'FILTER']:
            headers[exp][key]=f[0].header[key]
        headers[exp]['MJD-OBSC']=headers[exp]['MJDATE']+headers[exp]['EXPTIME']/2.0/3600.0/24.0
        f.close()

    import math,os
    real=0
    cand_total=len(cands)
    if cand_total > 100 :
        sys.stderr.write("Too many candidates (%d) skipping this chip...\n" % ( cand_total))
	return -1
    for cand in cands:
        cand_total=cand_total-1
        x2=[]
        y2=[]
        y1=[]
        x1=[]
        for i in range(len(exps)):
	    #print exps[i]
            fileId=exps[i]
            x2.append(int(min(math.floor(cand[i]['x'])+width,headers[fileId]['NAXIS1'])))
            y2.append(int(min(math.floor(cand[i]['y'])+width,headers[fileId]['NAXIS2'])))
            x1.append(int(max(math.floor(cand[i]['x'])-width,1)))
            y1.append(int(max(math.floor(cand[i]['y'])-width,1)))
	#print exps
	#print x1,x2
	#print y1,y2
        x_1 = min(x1)
        y_1 = min(y1)
        x_2 = max(x2)
        y_2 = max(y2)
	ans='f'
	fake=True
	xshifts=[]
	yshifts=[]
	first_pass=True
	show_coords=True
	ans='j'
	while ( first_pass or ans == 'f' or ans == 'c') : 
	  first_pass=False
	  if ans=='c':
	    if show_coords: 
	       show_coords=False
            else:
	       show_coords=True
	  if ans=='f':
	    if fake:
	       fake=False
	    else:
	       fake=True
          for i in range(len(exps)):
            xshift=cand[i]['x']-cand[i]['x_0']
            yshift=cand[i]['y']-cand[i]['y_0']
	    allmark=open('all.coo','w')
	    for j in range(len(exps)):
	      if j==i :
	         continue
              allmark.write('%f %f\n' % ( cand[j]['x_0']+xshift, cand[j]['y_0']+yshift))
	    allmark.close()
            tvmark=open('tv.coo','w')
            tvmark.write('%f %f %d\n' % ( cand[i]['x'], cand[i]['y'], cand_total))
            x1=max(x_1 + xshift,1)
            y1=max(y_1 + yshift,1)
            x2=min(x_2 + xshift,headers[exps[i]]['NAXIS1'])
            y2=min(y_2 + yshift,headers[exps[i]]['NAXIS2'])
            cutout = "[%d:%d,%d:%d]" % (x1,x2,y1,y2)
	    if not fake:
	       if exps[i][0:2]=='fk' :
	         fileId=exps[i][2:]
	       else:
                 fileId=exps[i]
            else:
               fileId=exps[i]
#	    sys.stdout.write("---> ",fileId+cutout,xshift,yshift
            try:
                junk=display(fileId+cutout,i+1,Stdout=1)
            except:
                sys.stderr.write("ERROR\n")
            tvmark.close()
	    if show_coords:
              pyraf.iraf.images.tv.tvmark(i+1,'all.coo',mark='circle',radii=10,color=205)
              pyraf.iraf.images.tv.tvmark(i+1,'tv.coo',mark='circle',radii=8,color=204,label='yes',nxoffset=10)
            os.unlink('tv.coo')
	    os.unlink('all.coo')
        ### ask if this is a real candidate, take action.
          ans='j'
          while ans not in ('y', 'n', 'q', 's', 'f', 'c'):
            ans=raw_input("[%d] (y,n,s,q,c,f) : " % ( cand_total))
        if ans=='y':
	    if os.access("cands.REAL",os.W_OK):
                f=open("cands.REAL","a")
 	    else:
                f=open("cands.REAL","w")
	        for comment in comments:
		    f.write(comment)
            cols=['x','y','x_0','y_0','flux','size','max_int','elon']
            #for col in cols:
            #    f.write("%s\t" % col)
            f.write("\n")
            for ii in range(len(exps)):
                for col in cols:
                    f.write("%8.2f\t" % cand[ii][col])
                f.write("\n")
            f.close()
            real=real+1
        if ans=='q':
            return -2
	if ans=='s':
	    return -1
    return real

cand_list={}
field_list=[]
chip_list=[]
import os
if (os.access("MOPconf.lock",os.F_OK)) :
   sys.stderr.write("This directory is locked... deelte MOPconf.lock to steal\n");
   sys.exit(-1)
else:
   l=file("MOPconf.lock",'w')
   l.write('PID: %d\n' % ( os.getpid()))
   l.close()

for i in range(36):
    chip = "chip%s" % string.zfill(str(i),2)
    chip_list.append(chip)
    cand_list[chip]={}

import glob

total_dirs = 0
for chip in chip_list:
    if not os.access(chip,os.R_OK):
        sys.stderr.write("Skipping %s " % ( chip))
        continue
    os.chdir(chip)
    for field in glob.glob("*"):
        os.chdir(field)
        if field not in field_list:
            field_list.append(field)
        filenames=['*.cands.comb.checked*','no_candidates', '*.cands.comb']
        for filename in filenames:
            list = glob.glob(filename)
            if len(list) > 0 :
                cand_list[chip][field]=list[0]
                break
        total_dirs=total_dirs+1
        os.chdir("..")
    os.chdir("..")

sys.stderr.write("Must check %d directories\n"  % ( total_dirs))
for chip in chip_list:
    #pyraf.iraf.cd(chip)
    if chip not in cand_list:
        sys.stderr.write("No candidate on %s\n" % ( chip))
        conintue
    for field in field_list:
        if field not in cand_list[chip]:
            sys.stderr.write("%s/%s failed to complete.\n"  % ( chip,field) )
            continue
        if cand_list[chip][field]=="no_candidates":
            continue
        if sre.search('checked',cand_list[chip][field]): 
            continue
        else:
	    #print cand_list[chip][field]
            sys.stderr.write("Checking candidates in %s %s\n" % ( field , chip))
            pyraf.iraf.cd(chip+"/"+field)
            result=discands(read_cands(cand_list[chip][field]))
            if result > -1: 
                sys.stderr.write("%d objects marked as real\n" % ( result))
                os.rename(cand_list[chip][field],cand_list[chip][field]+".checked")
            pyraf.iraf.cd("../..")
            if result==-2:
                sys.stderr.write("Removing lock file and exiting.\n")
                os.unlink('MOPconf.lock')
                sys.exit()
