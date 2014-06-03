__author__ = 'Michele Bannister'

import matplotlib.mlab as mlab
from scipy.optimize import curve_fit
import matplotlib.pyplot as plt
import brewer2mpl
from matplotlib import rcParams
from matplotlib.ticker import MultipleLocator, FormatStrFormatter
import numpy as np
import math
import sys
import ephem
from scipy.stats import norm
from track_done import parse, get_names
from ossos import storage

# nicer defaults
# every colourbrewer scale: http://bl.ocks.org/mbostock/5577023
# hues create primary visual differences between classes; does not imply magnitude differences between classes
set2 = brewer2mpl.get_map('Set2', 'qualitative', 8).mpl_colors
# sequential for ordered data where light is low values, dark is high values
ylorrd = brewer2mpl.get_map('YlOrRd', 'sequential', 9).mpl_colors
# diverging for equal emphasis on mid-range critical values and extremes at both ends of data range
puor = brewer2mpl.get_map('PuOr', 'diverging', 11).mpl_colors
almost_black = '#262626'

# rcParams['figure.figsize'] = (10, 10)
# rcParams['figure.dpi'] = 150
# rcParams['axes.color_cycle'] = puor
rcParams['font.size'] = 12   # good for posters/slides
rcParams['patch.facecolor'] = set2[0]

def remove_border(axes=None, keep=('left', 'bottom'), remove=('right', 'top'), labelcol=almost_black):
    """
    Minimize chart junk by stripping out unnecessary plot borders and axis ticks.
    The top/right/left/bottom keywords toggle whether the corresponding plot border is drawn
    """
    ax = axes or plt.gca()
    for spine in remove:
        ax.spines[spine].set_visible(False)
    for spine in keep:
        ax.spines[spine].set_linewidth(0.5)
        # ax.spines[spine].set_color('white')
    # match the label colour to that of the axes
    ax.xaxis.label.set_color(labelcol)
    ax.yaxis.label.set_color(labelcol)

    # remove all ticks, then add back the ones in keep
    # Does this also need to specify the ticks' colour, given the axes/labels are changed?
    ax.yaxis.set_ticks_position('none')
    ax.xaxis.set_ticks_position('none')
    for spine in keep:
        if spine == 'top':
            ax.xaxis.tick_top()
        if spine == 'bottom':
            ax.xaxis.tick_bottom()
        if spine == 'left':
            ax.yaxis.tick_left()
        if spine == 'right':
            ax.yaxis.tick_right()

    return

def clean_legend():
    # as per prettyplotlib.
    # Remove the line around the legend box, and instead fill it with a light grey
    # Also only use one point for the scatterplot legend because the user will
    # get the idea after just one, they don't need three.
    light_grey = np.array([float(248)/float(255)]*3)
    legend = ax.legend(frameon=True, scatterpoints=1)
    rect = legend.get_frame()
    rect.set_facecolor(light_grey)
    rect.set_linewidth(0.0)

    # Change the legend label colors to almost black, too
    texts = legend.texts
    for t in texts:
        t.set_color(almost_black)




"""
Plots to make:
- tweaked version of 13A field_locations plot with appropriate styling for poster
- tweaked version of 13B field_locations plot with appropriate styling for poster
- 13AE discoveries:
    - sky location (shrunk version of field_locations, 13AE only)
    - efficiencies plot per JJ's calculations
    - top-down Solar System with clumping vs. Neptune and RA pie overlay - done, yay!
    - orbital parameters per Brett's plots:
        - i vs a
        - q vs a
"""

def top_down_SolarSystem(discoveries, date="2013/04/09 08:50:00"):
    """
    Plot the OSSOS discoveries on a top-down Solar System showing the position of Neptune and model TNOs.
    Coordinates should be polar to account for RA hours, radial axis is in AU.
    :return: a saved plot
    """
    fig = plt.figure(figsize=(6, 6))
    rect = [0.1, 0.1, 0.8, .8]  # the plot occupies not all the figspace

    ax1 = fig.add_axes(rect, polar=True, frameon=False) # theta (RA) is zero at E, increases anticlockwise
    ax1.set_aspect('equal')

    # plot exclusion zones due to Galactic plane: RAs indicate where bar starts, rather than its centre angle
    width = math.radians(3*15)
    plt.bar(math.radians(16.5*15), 65, width=width, color=almost_black, linewidth=0, alpha=0.2)
    plt.bar(math.radians(4.5*15), 65, width=width, color=almost_black, linewidth=0, alpha=0.2)

    # plot OSSOS blocks
    # FIXME: should probably convert hours to ecliptic coords with a bit more finesse than just overplotting it
    for block in ["14:15:28.89", "15:58:01.35", "00:54:00.00", "01:30:00.00"]:
        # truncate these at 8 AU to show that we don't have sensitivity in close; detect Ijiraq at 9.80 AU
        # again RAs indicate where bar starts, so subtract half the block width from the block centrepoints
        plt.bar(ephem.hours(block)-math.radians(3.5), 65, width=math.radians(7), bottom=8, color='b', linewidth=0, alpha=0.2)

    ax1.set_rlim(0,65)
    ax1.set_rgrids([20,40,60], labels=['','20 AU','40 AU','60 AU'], angle=308, alpha=0.45)
    ax1.yaxis.set_major_locator(MultipleLocator(20))
    ax1.xaxis.set_major_locator(MultipleLocator(math.radians(30)))  # every 2 hours
    ax1.grid(axis='x', color='k', linestyle='--', alpha=0.2)
    ax1.set_xticklabels(['0h','','','','','','',"","","","20h","22h"],
                        color='b', alpha=0.6)  # otherwise they get in the way

    plot_planets_plus_Pluto(ax1, date)
    plot_ossos_discoveries(ax1, discoveries, date=date)
     # special detection in 13AE: Ijiraq at 2013-04-09 shows inner limit of sensitivity. Position from Horizons
    ax1.scatter(ephem.hours('14 29 46.57'), 9.805,
                marker='o', s=5, facecolor='b', edgecolor=almost_black, linewidth=0.15, alpha=0.8)
    ax1.annotate('Ijiraq', (ephem.hours('14 29 46.57') + math.radians(7), 9.8+2), size=5)
    ra, dist, hlat = synthetic_model_kbos(date, kbotype='resonant')
    ax1.scatter(ra, dist, marker='o', s=2, facecolor=almost_black, edgecolor=almost_black, linewidth=0.1, alpha=0.1)

    plt.draw()
    outfile = 'ra_vs_au_20130409'
    plt.savefig(outfile+'.pdf', transparent=True)

    return

def plot_planets_plus_Pluto(ax, date):
    for planet in [ephem.Neptune(), ephem.Pluto()]:
        planet.compute(ephem.date(date))
        ax.scatter(planet.ra, planet.sun_distance,
                     marker='o',
                     s=30,
                     facecolor='#E47833',
                     edgecolor='#E47833')
        ax.annotate(planet.name, (planet.ra+(math.radians(1)), planet.sun_distance+2), size=10)
        # plot Neptune's orbit: e is 0.01 so can get away with a circle
        if planet.name == 'Neptune':
            orb = np.arange(0,2*np.pi,(2*np.pi)/360)
            ax.plot(orb, np.repeat(planet.sun_distance, len(orb)), color='b', linestyle=':', linewidth=0.4, alpha=0.7)

    return

def plot_ossos_discoveries(ax, discoveries, date="2013/04/09 08:50:00"):
    for orbit in discoveries:
        orbit.predict(date.replace('/', '-'))
        ra = math.radians(orbit.coordinate.ra.degrees)
        fc = 'b'
        alph = 0.8
        mags = [n.mag for n in orbit.observations if (n.mag != -1 and n.mag != None)]  # temporary workaround until zeropoints fixed
        mean_mag = sum(mags)/len(mags)
        if mean_mag > 24.15:  # we can't track these ones
            fc = 'w'
            alph = 1.
            print orbit.observations[0].provisional_name, mean_mag
        # if orbit.distance > 50:   # identifying the one that's far out
        #     print kbo, orbit.distance
        #     sys.exit()
        ax.scatter(ra, orbit.distance, marker='o', s=4.5, facecolor=fc, edgecolor=almost_black, linewidth=0.15, alpha=alph)

    return

def synthetic_model_kbos(date, maglimit=24.5, kbotype='resonant'):
    ra = []
    dist = []
    hlat = []
    lines = storage.open_vos_or_local('vos:OSSOS/CFEPS/L7SyntheticModel-v09.txt').read().split('\n')
    for line in lines:
        if line[0]=='#':
            continue
        kbo = ephem.EllipticalBody()
        values = line.split()
        kbo._a = float(values[0])
        kbo._e = float(values[1])
        kbo._inc = float(values[2])
        kbo._Om = float(values[3])
        kbo._om = float(values[4])
        kbo._M = float(values[5])
        kbo._H = float(values[6])
        kbo._epoch_M = ephem.date(2453157.50000 - ephem.julian_date(0))
        kbo._epoch = kbo._epoch_M
        kbo.name = values[8]  # values[9] and [10] encode the type of resonance eg. 2:1 - add that if wanted

        kbo.compute(ephem.date(date))
        if (kbo.mag < maglimit):# and (kbo.name == kbotype):
            ra.append(kbo.ra)
            dist.append(kbo.sun_distance)
            hlat.append(kbo.hlat)

    return ra, dist, hlat

def side_elevation_SolarSystem(date="2013/04/09 08:50:00"):
    fig = plt.figure(figsize=(6, 2))
    ax = fig.add_subplot(111)#, aspect="equal")

    # viewpoint is from RA 18 hrs (have to ensure that)
    # want to plot the L7 with their inclinations
    # and the OSSOS blocks with the ecliptic latitudes = inclinations, again as wedges

    # need to think about converting RA properly to ecliptic lat/lon

    ax.set_xlim(-60,60)
    ax.set_ylim(-40,40)
    plt.ylabel('inclination (degrees)')  # proxy for Ecliptic latitude (degrees)
    plt.xlabel('AU')

    plt.draw()
    outfile = 'side_elevation_20130409'
    plt.savefig(outfile+'.pdf', transparent=True)


def sky_map_global():
    subplot(111,projection='aitoff')
    grid(True)
    # coords in radians
    box = Rectangle((radians(30),radians(30)),radians(15),radians(30))
    gca().add_patch(box)
    draw()

    return


def orbit_fit_residuals(discoveries, blockID='O13AE'):
    """Brett: What are relevant and stunning are the actual orbit fit residuals
    for orbits with d(a)/a<1%. I would be happy with a simple histogram of all those numbers,
    although the 'tail' (probably blended and not yet completely flagged) should
    perhaps be excluded.  There will be a pretty clear gaussian I would think, peaking near 0.07".
    """
    ra_residuals = []
    dec_residuals = []
    ressum = []
    for i, orbit in enumerate(discoveries):
        if (orbit.da/orbit.a) < 0.01:
            print i, len(discoveries), orbit.observations[0].provisional_name, orbit.da/orbit.a
            res = orbit.residuals.split('\n')
            for r in res:
                rr = r.split(' ')
                if (len(rr) > 1) and not (rr[0].startswith('!')):
                    ra_residuals.append(float(r.split(' ')[4]))
                    dec_residuals.append(float(r.split(' ')[5]))
                    ressum.append(1000*(float(r.split(' ')[4])**2 + float(r.split(' ')[5])**2))

    plt.figure(figsize=(6, 6))
    bins = [r for r in range(-25,150,25)]
    n, bins, patches = plt.hist(ressum, histtype='step', color='b', bins=bins, label='$dra^{2}+ddec^{2}$')
    # midpoints = [r/1000. for r in range(-350,350,50)]
    # popt, pcov = curve_fit(gaussian, midpoints, n)
    # print 'a', popt[0], 'mu', popt[1], 'sigma', popt[2]
    # xm = np.linspace(-.375, .375, 100)  # 100 evenly spaced points
    # plt.plot(xm, gaussian(xm, popt[0], popt[1], popt[2]), ls='--', color='#E47833', linewidth=2)
    # sigma = r'$\sigma = %.2f \pm %.2f$' % (popt[2], np.sqrt(pcov[2, 2]))
    # plt.annotate(sigma, (0.12, 50), color='#E47833')
    plt.xlim((-25,150))  # sufficient for 13AE data
    plt.ylabel('observations of orbits with d(a)/a < 1% (i.e 3-5 month arcs)')
    plt.xlabel('orbit fit residuals (milliarcsec)')
    plt.legend()
    # clean_legend()
    plt.draw()
    outfile = 'orbit_fit_residuals_13AE_corrected'
    plt.savefig(outfile+'.pdf', transparent=True)

    return

def gaussian(x, a, b, c):
    val = a * np.exp(-(x - b)**2 / c**2)
    return val


def delta_a_over_a(discoveries):
    arclen = []
    da_over_a = []
    for orbit in discoveries:
        if (orbit.da/orbit.a) < 0.01:
            da_over_a.append((orbit.da/orbit.a)*100)
            arclen.append(orbit.arc_length)

    plt.figure(figsize=(6, 6))
    plt.scatter(arclen, da_over_a, marker='o', facecolor='b', edgecolor=almost_black, linewidth=0.15, alpha=0.5)
    plt.xlabel('arc length of orbit (days)')
    plt.ylabel('d(a)/a (percent)')
    remove_border()
    plt.draw()
    outfile = 'delta_a_over_a_13AE_corrected'
    plt.savefig(outfile+'.pdf', transparent=True)
    print 'objects plotted:', len(arclen)

def get_discoveries(blockID='O13AE'):
    retval = []
    path = 'vos:OSSOS/measure3/2013A-E/track/submitted/'  # rather than discoveries, for longer arcs
#    path = 'vos:OSSOS/measure3/2013A-E/track/radec_corrected/'
    discoveries = storage.listdir(path)
    names = get_names(path, blockID)
    print 'Discoveries:', len(names)
    for i, kbo in enumerate(names):
        try:
            arclen, orbit = parse(kbo, discoveries, path=path)
            retval.append(orbit)
        except:
            continue

    return retval


def main():
    discoveries = get_discoveries()  # just the Orbit objects
    top_down_SolarSystem(discoveries)
#    orbit_fit_residuals(discoveries)
#    delta_a_over_a(discoveries)


if __name__ == "__main__":
    main()


