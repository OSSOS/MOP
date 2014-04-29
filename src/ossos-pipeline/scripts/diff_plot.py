#!python
from matplotlib import pyplot
from numpy import loadtxt
from numpy import histogram
import sys
import numpy
all = loadtxt('junk.dat', unpack=True)
fig = pyplot.figure()
ax = fig.add_subplot(111)

rate_low = 0
for rate in [10, 60, 100]:
    rate_high = rate
    constraintp = numpy.all((all[9] > 0 , all[3] < rate_high, all[3] > rate_low), axis=0)
    constraintf = numpy.all((all[3] < rate_high, all[3] > rate_low), axis=0)
    (h1,x) = histogram(all[2][constraintp], range=(21,26),bins=min(31,int(len(all[2])/5)))
    (h2,x) = histogram(all[2][constraintf], range=(21,26),bins=min(31,int(len(all[2])/5)))
    frac = (1.0*h1[h2>0])/h2[h2>0]
    x = x[1:]
    x = x[h2>0]
    ax.plot(x[frac>0],frac[frac>0])
    rate_low = rate_high 

ax.legend(('rate < 10', '10 < rate < 60', '60 < rate' ))
ax.set_ylim(0,1.1)
ax.set_xlim(21,26)
pyplot.minorticks_on()
ax.tick_params(axis='x',which='minor',bottom='on')
ax.set_xlabel('mag (r)')
ax.set_ylabel('frac')

pyplot.savefig('mag.png')
