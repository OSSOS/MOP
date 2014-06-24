__author__ = 'Michele Bannister   git:@mtbannister'

import matplotlib.pyplot as plt
import mpld3
from mpld3 import plugins

from . import parsers


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

dat = parsers.ossos_release_parser()
data = [d for d in dat if not d.name.__contains__('nt')]  # leave off the uncharacterised for now, they fill up the plot

# suitably bright ones
bright = [d for d in data if d.mag < 23.5]
print len(bright)

# just pulling out the cold classicals...
temp = [d for d in data if (42. < d.a < 46.) and d.e < 0.15 and d.i < 5. and d.mag < 23.5]
print len(temp)
data = temp

fig, ax = plt.subplots(3, 1, sharex=True, figsize=(10, 10))
ax[0].set_title('OSSOS cold classical discoveries after 2013 observations in v2 release')
fig.subplots_adjust(hspace=0.12)

# first all the objects. Some of these don't have all their orbital parameters yet, but dist and i are ok
points = ax[0].errorbar([d.dist for d in data], [d.i for d in data],
                        xerr=[d.dist_e for d in data], yerr=[d.i_e for d in data],
                        fmt='.', alpha=0.3, ecolor='0.1', ms=10)
# ax[0].set_ylim([-1, 90])
ax[0].grid(True, alpha=0.3)
ax[0].set_ylabel('inclination (degrees)')
ax[0].set_xlabel('heliocentric distance (AU)')
# ax[0].annotate('')

# now the plots for objects where we have much better arcs
dset = [d for d in data if d.a is not None]
ax[1].errorbar([d.a for d in dset], [d.i for d in dset],
               xerr=[d.a_e for d in dset], yerr=[d.i_e for d in dset],
               fmt='.', alpha=0.4, ecolor='0.1', ms=10)
ax[1].set_ylabel('inclination (degrees)')
# ax[1].set_ylim([-1, 35])
ax[1].grid(True, alpha=0.3)
ax[1].set_xlabel('semimajor axis (AU)')

ax[2].errorbar([d.a for d in dset], [d.e for d in dset],
               xerr=[d.a_e for d in dset], yerr=[d.e_e for d in dset],
               fmt='.', alpha=0.4, ecolor='0.1', ms=10)
ax[2].set_ylabel('eccentricity')
# ax[2].set_ylim([-0.01, .8])
ax[2].grid(True, alpha=0.3)
ax[2].set_xlabel('semimajor axis (AU)')
# plt.xlim([30, 80])
plt.xlim([42, 46])

# plt.xlabel('semimajor axis (AU)')

# plt.legend()
plt.draw()
outfile = 'plots/2013_OSSOS_discoveries_orbits'
plt.savefig(outfile + '.pdf', transparent=True)

# plugins.connect(fig, LinkedBrush(points))

labels = []
for obj in data:
    if obj.peri is not None:
        # label = {'dist': obj.dist, 'peri':obj.peri, 'obj':obj.name}
        html = '<table><tr><th>Name</th><td>{:}</td></tr>' \
               '<tr><th>Dist</th><td>{:.2f}</td></tr>' \
               '<tr><th>Peri</th><td>{:.2f}</td></tr></table>'.format(obj.name, obj.dist, obj.peri)
        labels.append(html)

tooltip = plugins.PointHTMLTooltip(points[0], labels, voffset=10, hoffset=10, css=css)
plugins.connect(fig, tooltip)
mpld3.show()





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
