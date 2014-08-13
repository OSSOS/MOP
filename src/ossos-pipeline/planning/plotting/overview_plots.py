__author__ = 'Michele Bannister'

import math

import matplotlib.pyplot as plt
import brewer2mpl
from matplotlib import rcParams
from matplotlib.ticker import MultipleLocator
import numpy as np
import ephem

import parsers
import parameters
import plot_fanciness

set2 = brewer2mpl.get_map('Set2', 'qualitative', 8).mpl_colors
rcParams['font.size'] = 12   # good for posters/slides
rcParams['patch.facecolor'] = set2[0]


def top_down_SolarSystem(discoveries, extent=65):
    """
    Plot the OSSOS discoveries on a top-down Solar System showing the position of Neptune and model TNOs.
    Discoveries are plotted each at their time of discovery according to the value in the Version Release.
    Coordinates should be polar to account for RA hours, radial axis is in AU.
    :return: a saved plot

    This is not a 'projection' of the particles into any
    common plane.  Each wedge is a different latitude above
    the plane, but inside each wedge it is heliocentric distance
    vs RA.
     That is fine.  It's just important to know that if someone
    asks we know this is NOT (for example) a projection of each
    object down into the ecliptic.
     A very minor future long-term improvement is that the galactic
    plane wedge is not (I don't think) symmetric around 18h and 6h.
    I think that lines of constant (say 15 degree galactic lat) are
    centered a little off (I think the wedge would 'twist' a little
    bit counter-clockwise).  I am not absolutely sure along the
    ecliptic where the b= +/- 15 degree lines are but I don't
    think they are symmetric?
    """
    fig = plt.figure(figsize=(6, 6))
    rect = [0.1, 0.1, 0.8, .8]  # the plot occupies not all the figspace

    ax1 = fig.add_axes(rect, polar=True, frameon=False) # theta (RA) is zero at E, increases anticlockwise
    ax1.set_aspect('equal')

    # plot exclusion zones due to Galactic plane: RAs indicate where bar starts, rather than its centre angle
    width = math.radians(3*15)
    plt.bar(math.radians(16.5 * 15), extent, width=width, color=plot_fanciness.ALMOST_BLACK, linewidth=0, alpha=0.2)
    plt.bar(math.radians(4.5 * 15), extent, width=width, color=plot_fanciness.ALMOST_BLACK, linewidth=0, alpha=0.2)

    # plot OSSOS blocks
    # FIXME: should probably convert hours to ecliptic coords with a bit more finesse than just overplotting it
    for blockname, block in parameters.BLOCKS.items():  # ["14:15:28.89", "15:58:01.35", "00:54:00.00", "01:30:00.00"]:
        # truncate these at 8 AU to show that we don't have sensitivity in close; detect Ijiraq at 9.80 AU
        # extend to 75 AU as indicative only, sensitivity is to ~300 AU
        # again RAs indicate where bar starts, so subtract half the block width from the block centrepoints
        # and approximate blocks as math.radians(7) degrees wide.
        plt.bar(ephem.hours(block["RA"]) - math.radians(3.5), extent,
                width=math.radians(7), bottom=8, color='b', linewidth=0.1, alpha=0.2)

    ax1.set_rlim(0, extent)
    ax1.set_rgrids([20, 40, 60], labels=["", "", '20 AU', '40 AU', '60 AU'], angle=308, alpha=0.45)
    ax1.yaxis.set_major_locator(MultipleLocator(20))
    ax1.xaxis.set_major_locator(MultipleLocator(math.radians(30)))  # every 2 hours
    ax1.grid(axis='x', color='k', linestyle='--', alpha=0.2)
    ax1.set_xticklabels(['0h','','','','','','',"","","","20h","22h"],
                        color='b', alpha=0.6)  # otherwise they get in the way

    plot_planets_plus_Pluto(ax1)
    plot_ossos_discoveries(ax1, discoveries)

    # special detection in 13AE: Ijiraq at 2013-04-09 shows inner limit of sensitivity.
    # Position from Horizons as it's excluded from the list of detections
    ax1.scatter(ephem.hours('14 29 46.57'), 9.805,
                marker='o', s=5, facecolor='b', edgecolor=plot_fanciness.ALMOST_BLACK, linewidth=0.15, alpha=0.8)
    ax1.annotate('Ijiraq', (ephem.hours('14 29 46.57') + math.radians(7), 9.8+2), size=5)

    ra, dist, hlat = parsers.synthetic_model_kbos(kbotype='resonant', arrays=True)
    ax1.scatter(ra, dist, marker='o', s=2, facecolor=plot_fanciness.ALMOST_BLACK,
                edgecolor=plot_fanciness.ALMOST_BLACK, linewidth=0.1, alpha=0.1)

    plt.draw()
    outfile = 'topdown_RA_d_OSSOSv3'
    plt.savefig(outfile+'.pdf', transparent=True)

    return


def plot_planets_plus_Pluto(ax, date=parameters.NEWMOONS[parameters.DISCOVERY_NEW_MOON]):
    for planet in [ephem.Uranus(), ephem.Neptune(), ephem.Pluto()]:
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


def plot_ossos_discoveries(ax, discoveries, lpmag=False):
    """
    these are all being plotted at their discovery locations now,
    which are provided by the Version Releases in decimal degrees.
    """
    for obj in discoveries:
        fc = 'b'
        alph = 0.8
        if lpmag:
            if obj.mag <= lpmag:  # show up the bright ones for Gemini LP
                fc = 'r'
        ra = ephem.hours(str(obj.ra_discov))
        ax.scatter(ra, obj.dist,
                   marker='o', s=4.5, facecolor=fc, edgecolor=plot_fanciness.ALMOST_BLACK, linewidth=0.15, alpha=alph)

    return

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
    plt.scatter(arclen, da_over_a, marker='o', facecolor='b', edgecolor=plot_fanciness.ALMOST_BLACK,
                linewidth=0.15, alpha=0.5)
    plt.xlabel('arc length of orbit (days)')
    plt.ylabel('d(a)/a (percent)')
    plot_fanciness.remove_border()
    plt.draw()
    outfile = 'delta_a_over_a_13AE_corrected'
    plt.savefig(outfile+'.pdf', transparent=True)
    print 'objects plotted:', len(arclen)



def main():
    # parsers.output_discoveries_for_animation()
    discoveries = parsers.ossos_release_with_metadata()
    top_down_SolarSystem(discoveries)

#    orbit_fit_residuals(discoveries)
#    delta_a_over_a(discoveries)


if __name__ == "__main__":
    main()


