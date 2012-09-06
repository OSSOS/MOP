#!/usr/bin/env python
"""Generate a list of artificial KBOs to plant in an image for searching"""

import mop_files


def get_rates(file,au_min=25,au_max=150):
    """ Use the rates program to determine the minimum and maximum bounds for planting"""
    import os, string

    rate_command='rate.pl --file %s %d ' % ( file, au_min)
    rate=os.popen(rate_command)
    line=rate.readline()
    print line
    rate.close()
    (min_rate, min_ang, min_aw, min_rmin, min_rmax)=string.split(line)
    rate_command='rate.pl --file %s %d ' % ( file, au_min)
    rate=os.popen(rate_command)
    line=rate.readline()
    rate.close()
    (max_rate, max_ang, max_aw, max_rmin, max_rmax)=string.split(line)
    rmin=float(min(max_rmin, min_rmin))
    rmax=float(max(max_rmax, min_rmax))
    aw=float(max(max_aw,min_aw))
    angle=(float(max_ang)+float(min_ang))/2.0
    rates={'rmin': rmin, 'rmax': rmax, 'angle': angle, 'aw': aw}
    return rates

def kbo_gen(file,outfile='objects.list',mmin=22.5,mmax=24.5):
    """Generate a file with object moving at a range of rates and angles"""

    header=get_rates(file)
    print header
    import pyfits
    hdulist=pyfits.open(file)
    header['xmin']=1
    header['xmax']=hdulist[0].header.get('NAXIS1',2048)
    header['ymin']=1
    header['aw']=round(header['aw'],2)
    header['angle']=round(header['angle'],2)
    header['ymax']=hdulist[0].header.get('NAXIS2',4096)
    header['pixscale']=hdulist[0].header.get('PIXSCALE',0.185)
    header['rmax']=round(float(header['rmax'])/float(header['pixscale']),2)
    header['rmin']=round(float(header['rmin'])/float(header['pixscale']),2)
    header['mmin']=mmin
    header['mmax']=mmax
    header['expnum']=hdulist[0].header.get('EXPNUM',1000000)
    header['chipnum']=hdulist[0].header.get('CHIPNUM')
    
    import random
    number = 250
    cdata={'x': [], 'y': [], 'mag': [], 'pix_rate': [], 'angle': [], 'id':[]}
    order=['x','y','mag','pix_rate','angle','arc_rate','id']
    for i in range(number):
        cdata['x'].append( random.uniform(header['xmin'],header['xmax']))
        cdata['y'].append( random.uniform(header['ymin'],header['ymax']))
        cdata['pix_rate'].append( random.uniform(header['rmin'],header['rmax']))
        cdata['angle'].append(random.uniform(header['angle']-header['aw'],
                                             header['angle']+header['aw']))
        cdata['mag'].append(random.uniform(header['mmin'],header['mmax']))
        cdata['id'].append(i)

    hdu={'data': cdata, 'header': header}
    return hdu

    
if __name__=='__main__':
    import sys
    data= kbo_gen(sys.argv[1])
    mop_files.write('objects',data,['x', 'y', 'mag', 'pix_rate', 'angle', 'id'],
                    {'x': '%10.2f',
                     'y': '%10.2f',
                     'mag': '%10.2f',
                     'pix_rate': '%10.2f',
                     'angle': '%10.2f',
                     'id': '%10d'})
