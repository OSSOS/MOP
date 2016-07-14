import numpy as np
from matplotlib import pyplot
import sys

slow_cut = int(3.0/0.185)
fast_cut = int(1.2*0.5/0.185)

for filename in sys.argv[1:]:
  cands = np.genfromtxt(filename, unpack=True, usecols=[2,7,4, 3])
  cands[2] += 23
  cands[2] = np.sqrt(cands[2]**2)
  pcond = np.all((31 > cands[2], cands[3] < slow_cut), axis=0)
  fcond = np.all((cands[1]!=0, pcond), axis=0)

  (planted, edges) = np.histogram(cands[0][pcond],range=(20,25), bins=25)
  (found, edges) = np.histogram(cands[0][fcond],range=(20,25), bins=25)


  p = planted[planted!=0]
  f = 1.0*found[planted!=0]
  x = edges[1:][planted!=0]
  pyplot.plot(x, f/p, alpha=0.3, label="rate < {} pix/hr ; width < 31".format(slow_cut))


  pcond = np.all((20 > cands[2], cands[3] > fast_cut), axis=0)
  fcond = np.all((cands[1]!=0, pcond), axis=0)
  (planted, edges) = np.histogram(cands[0][pcond],range=(20,25), bins=25)
  (found, edges) = np.histogram(cands[0][fcond],range=(20,25), bins=25)


  p = planted[planted!=0]
  f = 1.0*found[planted!=0]
  x = edges[1:][planted!=0]
  pyplot.plot(x, f/p, alpha=0.3, label="rate > {} pix/hr; width < 20".format(fast_cut))


print cands[2].min()-1, cands[2].max()+1
angles = [31, 25, 20, 0]
for idx in range(len(angles)-1):
  pcond = np.all((angles[idx] > cands[2], cands[2] > angles[idx+1], cands[3] < slow_cut, cands[3] > fast_cut), axis=0)
  fcond = np.all((cands[1]!=0, pcond), axis=0)

  (planted, edges) = np.histogram(cands[0][pcond],range=(20,25), bins=25)
  (found, edges) = np.histogram(cands[0][fcond],range=(20,25), bins=25)

  p = planted[planted!=0]
  f = 1.0*found[planted!=0]
  x = edges[1:][planted!=0]
  pyplot.plot(x, f/p, alpha=0.3, label="{} > width > {}; rate < {}".format(angles[idx], angles[idx+1], slow_cut))

pyplot.legend(loc='lower left')
pyplot.xlabel("""mag(r')""")
pyplot.ylabel("""frac""")
pyplot.xlim(21,25)
pyplot.ylim(-0.01, 1.01)
pyplot.savefig("eff.png")
