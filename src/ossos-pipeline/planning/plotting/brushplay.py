__author__ = 'Michele Bannister   git:@mtbannister'

import matplotlib.pyplot as plt
import matplotlib
import mpld3
from mpld3 import plugins

import parsers
import plot_fanciness


matplotlib.rcParams['font.size'] = 20  # good for posters/slides
matplotlib.rcParams['font.family'] = 'sans-serif'
matplotlib.rcParams['font.sans-serif'] = ['Sofia Pro', 'Tahoma']


# Define some CSS to control our custom labels
css = """
table
{
  border-collapse: collapse;
}
th
{
  color: #ffffff;
  background-color: #000000;
}
td
{
  background-color: #cccccc;
}
table, th, td
{
  font-family:Sofia Pro, sans-serif;
  border: 1px solid black;
  text-align: right;
}
"""

data = parsers.ossos_release_with_metadata()
# Just plot the sensible ones for now
dset = [d for d in data if ((d.a is not None) and (d.a_e / d.a < 0.05))]

# # suitably bright ones
# bright = [d for d in data if d.mag < 23.5]
# print len(bright)
#
# # just pulling out the cold classicals...
# temp = [d for d in data if (42. < d.a < 46.) and d.e < 0.15 and d.i < 5. and d.mag < 23.5]
# print len(temp)
# data = temp

fig, ax = plt.subplots(2, 1, sharex=True, figsize=(10, 10))
fig.subplots_adjust(hspace=0.05)

# why is this one object not fitting at all correctly?
# and not d.observations[-1].provisional_name.strip(' ').__contains__('o3e17'))]
# for n in dset:
# if n.observations[-1].provisional_name.strip(' ').__contains__('o3e38'):
#         print n.a, n.inc, n.e

# For calling with parsers.ossos_release_with_metadata()
points = ax[0].errorbar([d.a for d in dset], [d.i for d in dset],
                        xerr=[d.a_e for d in dset], yerr=[d.i_e for d in dset],
                        fmt='.', alpha=0.4, ecolor='0.1', ms=10)

# For using Orbfit objects
# points = ax[0].errorbar([d.a for d in dset], [d.inc for d in dset],
#                         xerr=[d.da for d in dset], yerr=[d.dinc for d in dset],
#                         fmt='.', alpha=0.4, ecolor='0.1', ms=15)

ax[0].set_ylabel('inclination (degrees)')
ax[0].set_ylim([-1, 55])
ax[0].grid(True, alpha=0.3)

# For calling with parsers.ossos_release_with_metadata()
ax[1].errorbar([d.a for d in dset], [d.e for d in dset],
               xerr=[d.a_e for d in dset], yerr=[d.e_e for d in dset],
               fmt='.', alpha=0.4, ecolor='0.1', ms=10)

# For using Orbfit objects
# ax[1].errorbar([d.a for d in dset], [d.e for d in dset],
#                xerr=[d.da for d in dset], yerr=[d.de for d in dset],
#                fmt='.', alpha=0.4, ecolor='0.1', ms=15)

ax[1].set_ylabel('eccentricity')
ax[1].set_ylim([-0.01, .8])
ax[1].grid(True, alpha=0.3)
# ax[1].set_xlabel('semimajor axis (AU)')
plt.xlim([20, 155])

plot_fanciness.remove_border(ax[0])
plot_fanciness.remove_border(ax[1])

plt.xlabel('semimajor axis (AU)')

# plt.legend()
plt.draw()
outfile = 'Sep24_OSSOS_discoveries_orbits'
plt.savefig(outfile + '.pdf', transparent=True)

# plugins.connect(fig, LinkedBrush(points))

labels = []
for obj in data:
    peri = None
    try:
        peri = obj.peri
    except:
        peri = obj.a * (1. - obj.e)
        obj.peri = peri
    if peri is not None:
        # label = {'dist': obj.dist, 'peri':obj.peri, 'obj':obj.name}
        html = '<table><tr><th>Name</th><td>{:}</td></tr>' \
               '<tr><th>Dist</th><td>{:.2f}</td></tr>' \
               '<tr><th>Peri</th><td>{:.2f}</td></tr></table>'.format(obj.name, obj.dist, peri)
        labels.append(html)

# Activate the interactivity
tooltip = plugins.PointHTMLTooltip(points[0], labels, voffset=10, hoffset=10, css=css)
plugins.connect(fig, tooltip)
mpld3.show()


# Generate a new plot of the objects designed to match the parameters in Petit et al. 2011
# layout is to be top left, a-q, top right, i-q, lower left, a-i.
# For calling with parsers.ossos_release_with_metadata()

plt.close()
fig, ax = plt.subplots(2, 2, figsize=(10, 10))
fig.subplots_adjust(hspace=0.0)  # want to work on the v-space as well

points = ax[0][0].errorbar([d.a for d in dset], [d.peri for d in dset],
                           xerr=[d.a_e for d in dset],
                           fmt='.', alpha=0.4, ecolor='0.1', ms=10)
ax[0][0].grid(True, alpha=0.4)
ax[0][0].set_xlim([39., 48.])
ax[1][0].set_xticks(range(40, 49, 2))
ax[0][0].set_ylim([34., 48.])
ax[0][0].set_ylabel('pericenter (AU)')
ax[0][0].set_yticks(range(36, 50, 4))

ax[0][1].errorbar([d.i for d in dset], [d.peri for d in dset],
                  xerr=[d.i_e for d in dset],
                  fmt='.', alpha=0.4, ecolor='0.1', ms=10)
ax[0][1].grid(True, alpha=0.4)
ax[0][1].set_xlim([0., 35.])
ax[0][1].set_xlabel('inclination (deg)')
ax[0][1].set_xticks(range(5, 40, 10))
ax[0][1].set_ylim([34., 48.])
ax[0][1].set_yticks(range(36, 50, 4))

ax[1][0].errorbar([d.a for d in dset], [d.i for d in dset],
                  xerr=[d.a_e for d in dset], yerr=[d.i_e for d in dset],
                  fmt='.', alpha=0.4, ecolor='0.1', ms=10)
ax[1][0].set_xlim([39., 48.])
ax[1][0].set_xticks(range(40, 49, 2))
ax[1][0].set_xlabel('semimajor axis (AU)')
ax[1][0].set_ylim([0., 35.])
ax[1][0].set_ylabel('inclination (deg)')
ax[1][0].set_yticks(range(0, 40, 10))
ax[1][0].grid(True, which='both', alpha=0.4)

plot_fanciness.remove_border(ax[0][0])
ax[0][0].tick_params(labelbottom='off')
plot_fanciness.remove_border(ax[0][1])
plot_fanciness.remove_border(ax[1][0])
plot_fanciness.remove_border(ax[1][1], remove=['left', 'right', 'top', 'bottom'], keep=[])
ax[1][1].tick_params(labelbottom='off', labelleft='off')

plt.draw()
outfile = 'Sep24_OSSOS_discoveries_Petit_style'
plt.savefig(outfile + '.pdf', transparent=True)




# # now plot again with the tno sizes indicated (approximating albedo to 4%)
#
# fig, ax = plt.subplots(3, 1, sharex=True, figsize=(10,10))
# ax[0].set_title('OSSOS discoveries in v2 release with points scaled by object size')
# fig.subplots_adjust(hspace=0.12)
#
# for p in data:
#     #diameter = ( (p.dist**2)*(10**((18.8 - p.mag)/5.)) )/math.sqrt(0.04)  # Fraser and Kavelaars, 2008
#     au = 149597871.
#     K = 5.61 # Vilenius et al., 2012
#     albedo = 0.1 # reasonable interval between cold and hot, could subdivide later as desired by incl.
#     diameter = 10**(math.sqrt((5.*math.log10((p.dist*au)**2) + K - p.mag)/2.5*albedo))
#
#     print p.dist, diameter, diameter*100., p.mag
#     p.diameter = diameter
#
# # first all the objects. Some of these don't have all their orbital parameters yet, but dist and i are ok
# for p in data:  # do this point-by-point to calculate individual object sizes
#     ax[0].errorbar(p.dist, p.i,
#                    xerr=p.dist_e, yerr=p.i_e,
#                    fmt='.', alpha=0.3, ecolor='0.1', ms=p.diameter/100., mfc='b', mec='b')
# ax[0].set_ylim([-1, 90])
# ax[0].grid(True, alpha=0.3)
# ax[0].set_ylabel('inclination (degrees)')
# ax[0].set_xlabel('heliocentric distance (AU)')
# # ax[0].annotate('')
#
# # now the plots for objects where we have much better arcs
# dset = [d for d in data if d.a is not None]
# for p in dset:
#     ax[1].errorbar(p.a, p.i,
#                     xerr=p.a_e, yerr=p.i_e,
#                     fmt='.', alpha=0.4, ecolor='0.1', ms=p.diameter/100., mfc='b', mec='b')
# ax[1].set_ylabel('inclination (degrees)')
# ax[1].set_ylim([-1, 35])
# ax[1].grid(True, alpha=0.3)
# ax[1].set_xlabel('semimajor axis (AU)')
#
# for p in dset:
#     ax[2].errorbar(p.a, p.e,
#                    xerr=p.a_e, yerr=p.e_e,
#                    fmt='.', alpha=0.4, ecolor='0.1', ms=p.diameter/100., mfc='b', mec='b')
# ax[2].set_ylabel('eccentricity')
# ax[2].set_ylim([-0.01, .8])
# ax[2].grid(True, alpha=0.3)
# ax[2].set_xlabel('semimajor axis (AU)')
#
# # plt.show()
#
# plt.draw()
# outfile = 'plots/2013_OSSOS_discoveries_sizes'
# plt.savefig(outfile+'.pdf', transparent=True)
