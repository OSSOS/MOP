from ossos import mpc
import glob
from ossos import orbfit
from matplotlib import pyplot
import math
import os
import cPickle
from planning.plotting import parsers

parsers.parameters.RELEASE_DETECTIONS="/Users/jjk/OSSOSv4.detections"

tnos = parsers.ossos_release_parser(table=True)

for filename in glob.glob("o3[eo]*.mpc"):
    lcolour = 'o3e' in filename  and 'r' or 'b'
    obs = list(mpc.MPCReader().read(filename))
    cl = tnos['cl'][tnos['object']==obs[0].provisional_name][0]
    print obs[0].provisional_name, cl
    colours = {'cla': 'r',
               'cen': 'y',
               'sca': 'g',
               'res': 'b',
               'det': 'm',
               'default': 'k'}
    symbols = {'cla': 'o',
               'cen': 'd',
               'sca': 's',
               'res': '*',
               'det': 'o',
               'default': 'o'}
    msymbol = symbols.get(cl, symbols['default'])
    mcolour = colours.get(cl, colours['default'])
    for ob in obs:
       if ob.discovery :
          t0 = ob.date.jd
          break
    obs.sort(key=lambda x: math.fabs(x.date.jd-t0))

    t = []
    da = []
    pfilename = '{}_da'.format(filename)
    if os.access(pfilename,os.R_OK):
        with open(pfilename,'r') as pf:
            (t,da) = cPickle.load(pf)
    else:
        for idx in range(3,len(obs)+1):
            orb = orbfit.Orbfit(obs[:idx])
            t.append((math.fabs(obs[idx-1].date.jd - t0))/365.25)
            da.append(orb.da/orb.a) 
        with open(pfilename,'w') as pf:
            cPickle.dump((t,da),pf)
        
    pyplot.plot(t,da,'-'+lcolour,alpha=0.3)
    pyplot.plot(t,da,'.'+lcolour,alpha=0.3)
    pyplot.plot(t[-1], da[-1], msymbol+lcolour, alpha=0.5, ms=11)

pyplot.xlabel('arc length')
pyplot.ylabel('da/a')
pyplot.yscale('log')
pyplot.xscale('log')
pyplot.grid(axis='y',alpha=0.6)
pyplot.vlines([30/365.0,60/365.0,1-60/3650],1e-5,1, alpha=0.4)
pyplot.show()
