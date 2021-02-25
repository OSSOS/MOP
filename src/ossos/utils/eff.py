#!python

import sys
import numpy
from matplotlib import pyplot

dmag = []
fastmag_found = []
fastmag_planted = []
slowmag_found = []
slowmag_planted = []
for filename in sys.argv[1:]:
  for line in open(filename):
      line = line.strip()
      if ( not len(line) > 0 ) or line[0] == '#':
          continue
      v = line.split()
      dmag.append(float(v[2]) - float(v[9]))
      if float(v[3]) > 33 : 
         fastmag_planted.append(float(v[2]))
         if float(v[9]) > 1 :
             fastmag_found.append(float(v[2]))
      else:
         slowmag_planted.append(float(v[2]))
         if float(v[9]) > 1 :
             slowmag_found.append(float(v[2]))

(fp_hist, bin_edges) = numpy.histogram(fastmag_planted, bins=40, range=(21,25))
(ff_hist, bin_edges) = numpy.histogram(fastmag_found, bins=40, range=(21,25))
print(ff_hist, fp_hist)
pyplot.plot(bin_edges[1:], (1.0*ff_hist)/(1.0*fp_hist))
pyplot.ylim(0,1.1)
pyplot.xlim(21,26)
pyplot.xlabel('mag (r)')
pyplot.ylabel('frac')


(sp_hist, bin_edges) = numpy.histogram(slowmag_planted, bins=50, range=(21,25))
(sf_hist, bin_edges) = numpy.histogram(slowmag_found, bins=50, range=(21,25))
print(sp_hist, sf_hist)
pyplot.plot(bin_edges[1:], (1.0*sf_hist)/(1.0*sp_hist))
pyplot.savefig('eff.pdf')
