from matplotlib import rcParams
from matplotlib import pyplot as plt
from astropy.io import ascii
import numpy
import sys

# # EmulateApJ columnwidth=245.26 pts
fig_width_pt = 246.0 * 3.0
inches_per_pt = 1.0 / 72.27
golden_mean = (numpy.sqrt(5.) - 1.0) / 2.0
fig_width = fig_width_pt * inches_per_pt
fig_height = fig_width * golden_mean * 1.5
fig_size = [fig_width, fig_height]

params = {'backend': 'pdf',
          'axes.labelsize': 12,
          'text.fontsize': 12,
          'legend.fontsize': 10,
          'xtick.labelsize': 10,
          'ytick.labelsize': 10,
          'text.usetex': False,
          'font.serif': 'Times',
          'font.family': 'serif',
          'image.aspect': 'equal',
          'figure.subplot.left': 0.2,
          'figure.subplot.bottom': 0.15,
          'figure.figsize': fig_size}

rcParams.update(params)

match_file=sys.argv[1]

T = ascii.read(match_file, header_start=-1, fill_values=['--', 0])
(nadd, bins) = numpy.histogram(T['mag'], bins=numpy.arange(21,26,0.25))
(nfnd, bins) = numpy.histogram(T['mag'][T['measure_mag1'].mask == False], bins=numpy.arange(21,26,0.25))

f = nfnd[nadd>0]/(1.0*nadd[nadd>0])
m = bins[nadd>0]

plt.subplot(2,2,1)
plt.plot(m, f, 'o-')
plt.xlabel('mag')
plt.ylabel('f')

plt.subplot(2,2,2)
plt.plot(T['x'],T['y'],'ob', alpha=0.1)
plt.plot(T['x'][T['measure_mag1'].mask==False],T['y'][T['measure_mag1'].mask==False],'or', alpha=0.1)
plt.xlabel('X-pixel')
plt.ylabel('Y-pixel')
plt.xlim(min(0,T['x'].min()),max(2048,T['x'].max()))
plt.ylim(min(0,T['y'].min()),max(4600,T['y'].max()))

plt.subplot(2,2,3)
dx = T['sky_rate']*numpy.cos(numpy.radians(T['angle']))
dy = T['sky_rate']*numpy.sin(numpy.radians(T['angle']))
plt.plot(dx, dy, 'o', alpha=0.1)
plt.xlabel('sky_rate*cos(angle)')
plt.ylabel('sky_rate*sin(angle)')

plt.subplot(2,2,4)


for measure in ['measure_mag1', 'measure_mag2', 'measure_mag3']:
   dmag = T['mag'] - T[measure]
   plt.plot(T['mag'], dmag, ',')

plt.xlabel('mag')
plt.ylabel('plant_mag - measure_mag')
plt.ylim(-0.4,0.4)
plt.savefig('{}.pdf'.format(match_file))


