
__author__ = 'Michele Bannister'

import math
import matplotlib.pyplot as plt
from matplotlib import rcParams
from matplotlib.ticker import MultipleLocator
import numpy as np
import ephem
import brewer2mpl

from ossos import (parameters)
from src.ossos.utils import parsers
from src.ossos.planning.plotting import plot_fanciness

set2 = brewer2mpl.get_map('Set2', 'qualitative', 8).mpl_colors
rcParams['font.size'] = 12  # good for posters/slides
rcParams['patch.facecolor'] = set2[0]

block_labels = ['E', 'O', 'L', 'H', 'P', 'M', 'S', 'D']

def top_down_SolarSystem(discoveries,
                         inner_limit=6,      # truncate at 8 AU to show that we don't have sensitivity in close
                         extent=83,          # extend to 83 AU as indicative only, sensitivity is to ~300 AU
                         plot_discoveries=None,
                         plot_colossos=False,
                         plot_blocks=None,
                         plot_galaxy=False,
                         feature_blocks=None,
                         plot_Ijiraq=False,  # detected Ijiraq at 9.80 AU in the 13AE block
                         label_blocks=True,
                         savefilename=None):
    """
    Plot the OSSOS discoveries on a top-down Solar System showing the position of Neptune and model TNOs.
    Discoveries are plotted each at their time of discovery according to the value in the Version Release.
    Coordinates should be polar to account for RA hours, radial axis is in AU.
    :return: a saved plot

    This is not a 'projection' of the particles into any common plane.  Each wedge is a different latitude above
    the plane, but inside each wedge it is heliocentric distance vs RA.
     That is fine.  It's just important to know that if someone asks we know this is NOT (for example) a projection of each
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
    rect = [0.01, 0.01, 0.95, .95]  # the plot occupies not all the figspace

    ax1 = fig.add_axes(rect, polar=True, frameon=False)  # theta (RA) is zero at E, increases anticlockwise
    ax1.set_aspect('equal')

    ax1.set_rlim(0, extent)
    ax1.set_rgrids([20, 40, 60, 80], labels=["", "", '20 au', '40 au', '60 au', '80 au'], angle=190, alpha=0.45)  # angle = 308
    ax1.yaxis.set_major_locator(MultipleLocator(20))
    ax1.xaxis.set_major_locator(MultipleLocator(math.radians(15)))  # every 2 hours
    ax1.grid(axis='x', color='k', linestyle='--', alpha=0.2)
    ax1.set_xticklabels(['', '0h', "", '2h', "", '4h', "", '6h', "", '8h', "", '10h', "", '12h', "", '14h', "", '16h',
                         "", '18h', "", '20h', "", '22h', "", ],
                        # ""])  # str(r)+'h' for r in range(-1,24)],
                        #        ['', '0h', '2h', '4h', '6h', '8h', '10h', '12h', '14h', '16h', '18h', '20h', '22h'],
                        color='b', alpha=0.6)  # otherwise they get in the way


    if plot_galaxy:
        # plot exclusion zones due to Galactic plane: RAs indicate where bar starts, rather than its centre angle
        width = math.radians(3 * 15)
        plt.bar(math.radians(4.5 * 15), extent, width=width, color=plot_fanciness.ALMOST_BLACK, linewidth=0, alpha=0.2)
        plt.bar(math.radians(16.5 * 15), extent, width=width, color=plot_fanciness.ALMOST_BLACK, linewidth=0, alpha=0.2)
        ax1.annotate('galactic plane', (math.radians(6.9 * 15), extent - 15), size=10, color='k', alpha=0.45)
        ax1.annotate('  galactic plane\navoidance zone', (math.radians(16.9 * 15), extent - 12), size=10, color='k', alpha=0.45)

    # again RAs indicate where bar starts, so subtract half the block width from the block centrepoints
    # and approximate blocks as math.radians(7) degrees wide.

    for blockname, block in list(parameters.BLOCKS.items()):
        if plot_blocks:
            if blockname in plot_blocks:
                if feature_blocks is not None and blockname in feature_blocks:
                    colour = 'm' #'#E47833'
                    alpha = 0.25
                else:
                    colour = 'b'
                    alpha = 0.1
                # if blockname.startswith('13') or blockname.startswith('14'):
                    width = math.radians(7) # the 3 x 7 field layout
                # else:
                #     width = math.radians(5) # the 4 x 5 field layout

                # cmap = plt.get_cmap('Blues')
                # if blockname.endswith('AO'):
                #     colour = '#E47833'
                #     alpha = 0.17
                #     cmap = plt.get_cmap('Oranges')  # colorbrewer.sequential.Oranges_3.mpl_colors

                ax1.bar(ephem.hours(block["RA"]) - math.radians(3.5), extent, linewidth=0.1,
                        width=width, bottom=inner_limit, zorder=0, color=colour, alpha=alpha)
                if label_blocks:
                    ax1.annotate(blockname[3], (ephem.hours(block["RA"]) + math.radians(0.36), extent - 3.), size=15,
                                 color='b')

    # No optional on these just yet
    plot_planets_plus_Pluto(ax1)
    ra, dist, hlat, Hmag = parsers.synthetic_model_kbos(kbotype='resonant', arrays=True, maglimit=24.7)
    # can't plot Hmag as marker size in current setup.
    ax1.scatter(ra, dist, marker='o', s=2, facecolor=plot_fanciness.ALMOST_BLACK,
                edgecolor=plot_fanciness.ALMOST_BLACK, linewidth=0.1, alpha=0.12, zorder=1)

    if plot_discoveries is not None:
        plot_ossos_discoveries(ax1, discoveries, plot_discoveries, plot_colossos=plot_colossos)

    # special detection in 13AE: Saturn's moon Ijiraq at 2013-04-09 shows inner limit of sensitivity.
    if plot_Ijiraq:
        # Position from Horizons as it's excluded from the list of detections
        ax1.scatter(ephem.hours('14 29 46.57'), 9.805,
                    marker='o', s=4, facecolor='b', edgecolor=plot_fanciness.ALMOST_BLACK, linewidth=0.15, alpha=0.8)

    plt.draw()
    if savefilename is not None:
        outfile = '{}.pdf'.format(savefilename)
    else:
        outfile = 'topdown_RA_d_OSSOS_v{}.pdf'.format(parameters.RELEASE_VERSION)
    plt.savefig(outfile, transparent=True, bbox_inches='tight')

    return


def plot_planets_plus_Pluto(ax, date=parameters.NEWMOONS[parameters.DISCOVERY_NEW_MOON]):
    for planet in [ephem.Saturn(), ephem.Uranus(), ephem.Neptune(), ephem.Pluto()]:
        planet.compute(ephem.date(date))
        fc = plot_fanciness.ALMOST_BLACK
        if planet.name == 'Pluto':
            alpha = 0.35
            size = 10
            fs = 5
        else:
            alpha = 0.7
            size = 20
            fs = 10
        ax.scatter(planet.ra, planet.sun_distance,
                   marker='o',
                   s=size,
                   facecolor=fc,
                   edgecolor=fc,
                   alpha=alpha)
        if planet.name == 'Saturn':
            ax.annotate(planet.name, (planet.ra + (math.radians(3)), planet.sun_distance - 2), size=fs)
        elif planet.name == 'Uranus':
            ax.annotate(planet.name, (planet.ra - (math.radians(12)), planet.sun_distance + 1), size=fs)
        else:
            ax.annotate(planet.name, (planet.ra - (math.radians(0.5)), planet.sun_distance + 2), size=fs)

        # Neptune's orbit has e = 0.01, so can get away with a circle
        if planet.name == 'Neptune':
            orb = np.arange(0, 2 * np.pi, (2 * np.pi) / 360)
            ax.plot(orb, np.repeat(planet.sun_distance, len(orb)), color='b', linestyle=':', linewidth=0.4, alpha=0.7)

    return


def plot_ossos_discoveries(ax, discoveries, plot_discoveries,
                           plot_colossos=False, split_plutinos=False):
    """
    plotted at their discovery locations, provided by the Version Releases in decimal degrees.
    """
    fc = ['b', '#E47833', 'k']
    alpha = [0.85, 0.6, 1.]
    marker = ['o', 'd']
    size = [7, 25]

    plottable = []   # Which blocks' discoveries to include?
    for d in discoveries:
        for n in plot_discoveries:
            if d['object'].startswith(n):  # can for sure be better, but this hack works. Need to get where going
                plottable.append(d)

    # Hack to get in the O15BD objects
    # directory_name = '/Users/bannisterm/Dropbox/OSSOS/measure3/ossin/D_tmp/'
    # kbos = parsers.ossos_discoveries(directory_name, all_objects=False, data_release=None)
    # for kbo in kbos:
    #     plottable_kbo = {'RAdeg': kbo.discovery.coordinate.ra.to_string(unit=units.degree, sep=':'),
    #                      'dist': kbo.orbit.distance.value}
    #     plottable.append(plottable_kbo)

    if plot_colossos:
        fainter = []
        colossos = []
        for n in plottable:
            if n['object'] in parameters.COLOSSOS:
                colossos.append(n)
            else:
                fainter.append(n)
        plot_ossos_points(fainter, ax, marker[0], size[0], fc[0], alpha[1], 1)
        plot_ossos_points(colossos, ax, marker[1], size[1], fc[2], alpha[2], 2)
    elif split_plutinos:
        # plutino_index = np.where((plottable['cl'] == 'res') & (plottable['j'] == 3) & (plottable['k'] == 2))
        raise NotImplementedError
    else:
        plot_ossos_points(plottable, ax, marker[0], size[0], fc[0], alpha[0], 2)

    return


def plot_ossos_points(data, ax, marker, size, fc, alpha, zorder):
    ra = [ephem.degrees(str(n['RAdeg'])) for n in data]
    dist = [n['dist'] for n in data]
    ax.scatter(ra, dist,
               marker=marker, s=size, facecolor=fc, edgecolor='w', linewidth=0.25,
               alpha=alpha, zorder=zorder)  # original size=4.5


def side_elevation_SolarSystem(date="2013/04/09 08:50:00"):
    fig = plt.figure(figsize=(6, 2))
    ax = fig.add_subplot(111)  # , aspect="equal")

    # viewpoint is from RA 18 hrs (have to ensure that)
    # want to plot the L7 with their inclinations
    # and the OSSOS blocks with the ecliptic latitudes = inclinations, again as wedges

    # need to think about converting RA properly to ecliptic lat/lon

    ax.set_xlim(-60, 60)
    ax.set_ylim(-40, 40)
    plt.ylabel('inclination (degrees)')  # proxy for Ecliptic latitude (degrees)
    plt.xlabel('AU')

    plt.draw()
    outfile = 'side_elevation_20130409'
    plt.savefig(outfile + '.pdf', transparent=True)


def sky_map_global():
    subplot(111, projection='aitoff')
    grid(True)
    # coords in radians
    box = Rectangle((radians(30), radians(30)), radians(15), radians(30))
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
        if (orbit.da / orbit.a) < 0.01:
            print(i, len(discoveries), orbit.observations[0].provisional_name, orbit.da / orbit.a)
            res = orbit.residuals.split('\n')
            for r in res:
                rr = r.split(' ')
                if (len(rr) > 1) and not (rr[0].startswith('!')):
                    ra_residuals.append(float(r.split(' ')[4]))
                    dec_residuals.append(float(r.split(' ')[5]))
                    ressum.append(1000 * (float(r.split(' ')[4]) ** 2 + float(r.split(' ')[5]) ** 2))

    plt.figure(figsize=(6, 6))
    bins = [r for r in range(-25, 150, 25)]
    n, bins, patches = plt.hist(ressum, histtype='step', color='b', bins=bins, label='$dra^{2}+ddec^{2}$')
    # midpoints = [r/1000. for r in range(-350,350,50)]
    # popt, pcov = curve_fit(gaussian, midpoints, n)
    # print 'a', popt[0], 'mu', popt[1], 'sigma', popt[2]
    # xm = np.linspace(-.375, .375, 100)  # 100 evenly spaced points
    # plt.plot(xm, gaussian(xm, popt[0], popt[1], popt[2]), ls='--', color='#E47833', linewidth=2)
    # sigma = r'$\sigma = %.2f \pm %.2f$' % (popt[2], np.sqrt(pcov[2, 2]))
    # plt.annotate(sigma, (0.12, 50), color='#E47833')
    plt.xlim((-25, 150))  # sufficient for 13AE data
    plt.ylabel('observations of orbits with d(a)/a < 1% (i.e 3-5 month arcs)')
    plt.xlabel('orbit fit residuals (milliarcsec)')
    plt.legend()
    # clean_legend()
    plt.draw()
    outfile = 'orbit_fit_residuals_13AE_corrected'
    plt.savefig('/ossos_sequence/'+ outfile + '.pdf', transparent=True)

    return


def gaussian(x, a, b, c):
    val = a * np.exp(-(x - b) ** 2 / c ** 2)
    return val


def delta_a_over_a(discoveries):
    arclen = []
    da_over_a = []
    for orbit in discoveries:
        if (orbit.da / orbit.a) < 0.01:
            da_over_a.append((orbit.da / orbit.a) * 100)
            arclen.append(orbit.arc_length)

    plt.figure(figsize=(6, 6))
    plt.scatter(arclen, da_over_a, marker='o', facecolor='b', edgecolor=src.ossos.core.ossos.planning.plotting.plot_fanciness.ALMOST_BLACK,
                linewidth=0.15, alpha=0.5)
    plt.xlabel('arc length of orbit (days)')
    plt.ylabel('d(a)/a (percent)')
    plot_fanciness.remove_border()
    plt.draw()
    outfile = 'delta_a_over_a_13AE_corrected'
    plt.savefig(outfile + '.pdf', transparent=True)
    print('objects plotted:', len(arclen))


def ossos_sequence(discoveries):
    # Sky with reference rings and plutino model
    # top_down_SolarSystem(discoveries, savefilename='1_reference')
    # # Add a wedge for E and O
    # top_down_SolarSystem(discoveries, plot_blocks=['13AE', '13AO'],
    #                      label_blocks=block_labels[0:2], savefilename='2_EO')
    # # 3. Add H and L wedges
    # top_down_SolarSystem(discoveries, plot_blocks=['13AE', '13AO', '13BL', '14BH'],
    #                  label_blocks=block_labels[0:4], savefilename='3_EOHL')
    # # 4. Add E and O detections
    # top_down_SolarSystem(discoveries, plot_discoveries=['o3e', 'o3o'], plot_Ijiraq=True,
    #                      plot_blocks=['13AE', '13AO', '13BL', '14BH'],
    #                      label_blocks=block_labels[0:4], savefilename='4_EOHL_discoveries')
    # # 5. Add H and L detections
    # top_down_SolarSystem(discoveries, plot_discoveries=['o3e', 'o3o', 'o3l', 'o4h'], plot_Ijiraq=True,
    #                      plot_blocks=['13AE', '13AO', '13BL', '14BH'],
    #                      label_blocks=block_labels[0:4], savefilename='5_EOHL_discoveries')
    # # 6. Add P and M wedges
    # top_down_SolarSystem(discoveries, plot_discoveries=['o3e', 'o3o', 'o3l', 'o4h'], plot_Ijiraq=True,
    #                  plot_blocks=['13AE', '13AO', '13BL', '14BH', '15AM', '15AP'],
    #                  label_blocks=block_labels[0:6], savefilename='6_EOHLPM')
    # # 7. Add S and D wedges
    # top_down_SolarSystem(discoveries, plot_discoveries=['o3e', 'o3o', 'o3l', 'o4h'], plot_Ijiraq=True,
    #                  plot_blocks=['13AE', '13AO', '13BL', '14BH', '15AM', '15AP', '15BS', '15BD'],
    #                  label_blocks=block_labels, savefilename='7_EOHLPMSD')
    # # 8. Add P and M detections
    # top_down_SolarSystem(discoveries, plot_discoveries=['o3e', 'o3o', 'o3l', 'o4h', 'o5p', 'o5m'], plot_Ijiraq=True,
    #                  plot_blocks=['13AE', '13AO', '13BL', '14BH', '15AM', '15AP', '15BS', '15BD'],
    #                  label_blocks=block_labels, savefilename='8_EOHLPM_discoveries')
    # # 9. Add S/T detections
    # top_down_SolarSystem(discoveries, plot_discoveries=['o3e', 'o3o', 'o3l', 'o4h', 'o5p', 'o5m', 'o5s', 'o5t'], plot_Ijiraq=True,
    #                      plot_blocks=['13AE', '13AO', '13BL', '14BH', '15AM', '15AP', '15BS', '15BD'],
    #                      label_blocks=block_labels, savefilename='9_EOHLPMST_discoveries')
    # 10. Add D detections
    top_down_SolarSystem(discoveries,
                         plot_discoveries=['o3e', 'o3o', 'o3l', 'o4h', 'o5p', 'o5m', 'o5s', 'o5t', 'o5c', 'o5d'],
                         plot_Ijiraq=True,
                         plot_blocks=['13AE', '13AO', '13BL', '14BH', '15AM', '15AP', '15BS', '15BD'],
                         label_blocks=block_labels,
                         savefilename='10_EOHLPMSTCD_discoveries')

def colossos(discoveries):
    top_down_SolarSystem(discoveries,
                         plot_discoveries=['o3e', 'o3o', 'o3l', 'o4h'],
                         plot_Ijiraq=True,
                         plot_blocks=['13AE', '13AO', '13BL', '14BH', '15AM', '15AP', '15BS', '15BD'],
                         feature_blocks=['13AE', '13AO', '13BL', '14BH'],
                         label_blocks=block_labels,
                         plot_colossos=True,
                         savefilename='colossos_highlighted')


def main():
    # parsers.output_discoveries_for_animation()
    discoveries = parsers.ossos_release_parser(table=True)
    ossos_sequence(discoveries)
    # colossos(discoveries)

# orbit_fit_residuals(discoveries)
# delta_a_over_a(discoveries)


if __name__ == "__main__":
    main()


