__author__ = 'Michele Bannister   git:@mtbannister'

import sys

import numpy
import matplotlib.pyplot as plt
from astropy.table import Table

import plot_fanciness
import parameters

path = '/Users/michele/Dropbox/Papers in progress/OSSOS/First_quarter/data/SSim_fit/'

fig, ax = plt.subplots(1, 1)
ls = ['d', 'k--', 'r-.']

for i, fn in enumerate(['EO-a-i.detections',
                        'L7-EO-a-i.sim',
                        'NoKernel-EO-a-i.sim']):
    with open(path + fn, 'r') as infile:
        raw = Table.read(infile, format='ascii', delimiter=' ', data_start=1, guess=False,
                         header_start=0)['a']
        # create a cumulative fraction
        raw.sort()
        bins = range(400, 470, 1)
        bins = [n / 10. for n in bins]
        hist, bin_edges = numpy.histogram(raw, bins=bins)
        cumulative = []
        # plotting differs for the actual detections as they are far less numerous: sample sparsely
        if i == 0:
            for k, r in enumerate(raw):
                cumulative.append(float(k + 1) / float(len(raw)))
            ax.scatter(raw, cumulative,
                       marker=ls[i], s=15, alpha=0.7, label='OSSOS, this work')
        else:
            for j, n in enumerate(hist):
                cumulative.append(float(sum(hist[:j])) / float(len(raw)))
            if fn == 'L7-EO-a-i.sim':
                label = 'L7 model'
            if fn == 'NoKernel-EO-a-i.sim':
                label = 'L7 model, no kernel'
            ax.plot(bins[:len(bins) - 1], cumulative, ls[i], alpha=0.8, label=label)

plt.ylabel('fraction with semimajor axis $<$ a')
plt.xlabel('semimajor axis (AU)')
plt.xlim([42.4, 47])
plt.ylim([0.0, 1.05])
plt.grid(True, which='both', alpha=0.4)
plt.legend(loc='lower right', scatterpoints=1)
plot_fanciness.remove_border(ax)
plt.draw()
# plt.show()
outfile = 'ssim_v{}.pdf'.format(parameters.RELEASE_VERSION)
plt.savefig(outfile, transparent=True, bbox_inches='tight')
sys.stdout.write('{}\n'.format(outfile))
