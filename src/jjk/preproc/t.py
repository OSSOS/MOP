

## read in the .cands.comb file
def read_cands(filename):
    """Read in the contents of a cands comb file"""
    import sre
    
    lines=file(filename).readlines()
    exps=[]
    cands=[]
    coo=[]
    for line in lines:
        if ( line[0:2]=="##" ) :
            break
        exps.append(line[2:].strip())

    for line in lines:
        if ( line[0]=="#" ) :
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
    return {'fileId': exps, 'cands': cands}

def discands(record):
    """Display the candidates contained in a candidate record list"""
    import pyraf, pyfits

    pyraf.iraf.tv()
    display = pyraf.iraf.tv.display
    
    width=128
    
    cands = record['cands']
    exps=   record['fileId']

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
    for cand in cands:
        for i in range(len(exps)):
            x2=[]
            y2=[]
            y1=[]
            x1=[]
            fileId=exps[i]
            x2.append(int(min(math.floor(cand[i]['x'])+width,headers[fileId]['NAXIS1'])))
            y2.append(int(min(math.floor(cand[i]['y'])+width,headers[fileId]['NAXIS2'])))
            x1.append(int(max(math.floor(cand[i]['x'])-width,1)))
            y1.append(int(max(math.floor(cand[i]['y'])-width,1)))
        x_1 = min(x1)
        y_1 = min(y1)
        x_2 = max(x2)
        y_2 = max(y2)
        for i in range(len(exps)):
            tvmark=open('tv.coo','w')
            xshift=cand[i]['x']-cand[i]['x_0']
            yshift=cand[i]['y']-cand[i]['y_0']
            tvmark.write('%f %f\n' % ( cand[i]['x'], cand[i]['y']))
            x1=x_1 + xshift
            y1=y_1 + yshift
            x2=x_2 + xshift
            y2=y_2 + yshift
            cutout = "[%d:%d,%d:%d]" % (x1,x2,y1,y2)
            fileId=exps[i]
            display(fileId+cutout,i+1)
            tvmark.close()
            pyraf.iraf.tv.tvmark(i+1,'tv.coo',mark='circle',radii=15)
            os.unlink('tv.coo')


if __name__ == '__main__':
    import sys

    discands(read_cands(sys.argv[1]))

