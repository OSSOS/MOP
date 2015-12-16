__author__ = 'Michele Bannister'

import math

import matplotlib.pyplot as plt
from matplotlib import rcParams
from matplotlib.ticker import MultipleLocator
import numpy as np
import ephem

import brewer2mpl
import parsers
import parameters
import plot_fanciness

set2 = brewer2mpl.get_map('Set2', 'qualitative', 8).mpl_colors
rcParams['font.size'] = 12  # good for posters/slides
rcParams['patch.facecolor'] = set2[0]


def top_down_SolarSystem(discoveries,
                         extent=68,
                         plot_blocks=True,
                         future_blocks=True,
                         plot_Ijiraq=True,
                         label_blocks=True):
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

    ax1 = fig.add_axes(rect, polar=True, frameon=False)  # theta (RA) is zero at E, increases anticlockwise
    ax1.set_aspect('equal')

    ax1.set_rlim(0, extent)
    ax1.set_rgrids([20, 40, 60], labels=["", "", '20 AU', '40 AU', '60 AU'], angle=197, alpha=0.45)  # angle = 308
    ax1.yaxis.set_major_locator(MultipleLocator(20))
    ax1.xaxis.set_major_locator(MultipleLocator(math.radians(15)))  # every 2 hours
    ax1.grid(axis='x', color='k', linestyle='--', alpha=0.2)
    ax1.set_xticklabels(['', '0h', "", '2h', "", '4h', "", '6h', "", '8h', "", '10h', "", '12h', "", '14h', "", '16h',
                         "", '18h', "", '20h', "", '22h', "", ],
                        # ""])  # str(r)+'h' for r in range(-1,24)],
                        #        ['', '0h', '2h', '4h', '6h', '8h', '10h', '12h', '14h', '16h', '18h', '20h', '22h'],
                        color='b', alpha=0.6)  # otherwise they get in the way


    # plot exclusion zones due to Galactic plane: RAs indicate where bar starts, rather than its centre angle
    # can I do this with a warp, or will it make more sense to just plot stellar density? maybe that?
    width = math.radians(3 * 15)
    # plt.bar(math.radians(4.5 * 15), extent, width=width, color=plot_fanciness.ALMOST_BLACK, linewidth=0, alpha=0.2)
    # plt.bar(math.radians(16.5 * 15), extent, width=width, color=plot_fanciness.ALMOST_BLACK, linewidth=0, alpha=0.2)
    # ax1.annotate('galactic plane', (math.radians(6.9 * 15), extent - 15), size=10, color='k', alpha=0.45)
    # ax1.annotate('  galactic plane\navoidance zone', (math.radians(16.9 * 15), extent - 12), size=10, color='k', alpha=0.45)


    # FIXME: should probably convert hours to ecliptic coords with a bit more finesse than just overplotting it
    # truncate these at 8 AU to show that we don't have sensitivity in close; detect Ijiraq at 9.80 AU
    # extend to 75 AU as indicative only, sensitivity is to ~300 AU
    # again RAs indicate where bar starts, so subtract half the block width from the block centrepoints
    # and approximate blocks as math.radians(7) degrees wide.

    for blockname, block in parameters.BLOCKS.items():  # ["14:15:28.89", "15:58:01.35", "00:54:00.00", "01:30:00.00"]:
        if plot_blocks:
            if blockname.startswith('13') or blockname.startswith('14'):
                colour = 'b'
                alpha = 0.1
                cmap = plt.get_cmap('Blues')  # colorbrewer.sequential.Blues_4.mpl_colors
                # if blockname.endswith('AO'):
                #     colour = '#E47833'
                #     alpha = 0.17
                #     cmap = plt.get_cmap('Oranges')  # colorbrewer.sequential.Oranges_3.mpl_colors

                # want to apply a colour gradient to the block
                # luminance = [[.6, .6],[.7,.7]]
                # gradient = np.linspace(0, 1, 256)
                # gradient = np.vstack((gradient, gradient))
                #
                # r_i = np.linspace(8, extent, 256)
                # r_i = np.vstack((r_i, r_i))
                # theta_i = np.linspace(ephem.hours(block["RA"]) - math.radians(3.5),
                #                       ephem.hours(block["RA"]) + math.radians(3.5), 7)
                # grid = np.meshgrid(theta_i, r_i)
                #
                # ax1.imshow(r_i,
                #            extent=(ephem.hours(block["RA"]) - math.radians(3.5),
                #                    ephem.hours(block["RA"]) + math.radians(3.5),
                #                    8, extent),
                #            cmap=cmap)

                bar = ax1.bar(ephem.hours(block["RA"]) - math.radians(3.5), extent, linewidth=0.1,
                              width=math.radians(7), bottom=8, zorder=0, color=colour, alpha=alpha)
                # bar[0].set_facecolor(cmap)
                # bar[0].set_alpha(0.8)
                if label_blocks:
                    ax1.annotate(blockname[3], (ephem.hours(block["RA"]) + math.radians(0.3), extent + 0.15), size=15,
                                 color='b')

        if future_blocks:
            if blockname.startswith('15'):
                plt.bar(ephem.hours(block["RA"]) - math.radians(3.5), extent,
                        width=math.radians(7), bottom=8, color='#E47833', linewidth=0.1, alpha=0.17)
                if label_blocks:
                    ax1.annotate(blockname[3], (ephem.hours(block["RA"]) - math.radians(1.5), extent + 0.15), size=15,
                                 color='r')

    # what if try adding a ring around the whole thing with alpha gradient to get the wedge-edge fadeout?
    inner = plt.Circle(58)
    outer = plt.Circle(65)
    # plt.PatchCollection?

    # plot_ossos_discoveries(ax1, discoveries)

    plot_planets_plus_Pluto(ax1)

    if plot_Ijiraq:
        # special detection in 13AE: Ijiraq at 2013-04-09 shows inner limit of sensitivity.
        # Position from Horizons as it's excluded from the list of detections
        ax1.scatter(ephem.hours('14 29 46.57'), 9.805,
                    marker='o', s=4, facecolor='b', edgecolor=plot_fanciness.ALMOST_BLACK, linewidth=0.15, alpha=0.8)
        # ax1.annotate('Ijiraq', (ephem.hours('14 29 46.57') + math.radians(7), 9.8 + 2), size=5)

    ra, dist, hlat, Hmag = parsers.synthetic_model_kbos(kbotype='resonant', arrays=True, maglimit=24.7)
    # can't plot Hmag as marker size in current setup.
    ax1.scatter(ra, dist, marker='o', s=2, facecolor=plot_fanciness.ALMOST_BLACK,
                edgecolor=plot_fanciness.ALMOST_BLACK, linewidth=0.1, alpha=0.12, zorder=1)

    plt.draw()
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
        if planet.name != 'Saturn':
            ax.annotate(planet.name, (planet.ra - (math.radians(0.5)), planet.sun_distance + 2), size=fs)
        else:
            ax.annotate(planet.name, (planet.ra + (math.radians(10)), planet.sun_distance - 2), size=fs)
        # plot Neptune's orbit: e is 0.01 so can get away with a circle
        if planet.name == 'Neptune':
            orb = np.arange(0, 2 * np.pi, (2 * np.pi) / 360)
            ax.plot(orb, np.repeat(planet.sun_distance, len(orb)), color='b', linestyle=':', linewidth=0.4, alpha=0.7)

    return


def plot_ossos_discoveries(ax, discoveries, lpmag=False):
    """
    these are all being plotted at their discovery locations now,
    which are provided by the Version Releases in decimal hours.
    """
    # need to make this row-wise
    fc = ['b', '#E47833']
    alph = 0.85
    marker = ['o', 'd']
    size = [7, 11]
    pl_index = np.where((discoveries['cl'] == 'res') & (discoveries['j'] == 3) & (discoveries['k'] == 2))
    l = []
    for k, n in enumerate(discoveries):
        if k not in pl_index[0]:
            l.append(k)
    not_plutinos = discoveries[l]
    plutinos = discoveries[pl_index]
    for j, d in enumerate([not_plutinos, plutinos]):
        ra = [ephem.hours(str(n)) for n in d['ra_dis']]
        ax.scatter(ra, d['dist'],
                   marker=marker[j], s=size[j], facecolor=fc[j], edgecolor='w', linewidth=0.25,
                   alpha=alph, zorder=2)  # original size=4.5

    # for obj in discoveries:
    # if lpmag:
    #         if obj.mag <= lpmag:  # show up the bright ones for Gemini LP
    #             fc = 'r'
    #     ra = ephem.hours(str(obj.ra_discov))
    #     if (obj.classification == 'res' and obj.n == 3 and obj.m == 2):
    #         print obj.name
    #         ax.scatter(ra, obj.dist,
    #                    marker='o', s=15 - obj.H, facecolor='#E47833', edgecolor='w', linewidth=0.4,
    #                    alpha=alph)  # original size=4.5
    #     else:
    #         # scaling the size of marker by the absolute magnitude of each object (so size++ with --H, as needed).
    #         ax.scatter(ra, obj.dist,
    #                    marker='o', s=15 - obj.H, facecolor=fc, edgecolor='w', linewidth=0.4,
    #                    alpha=alph)  # original size=4.5

    return


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
            print i, len(discoveries), orbit.observations[0].provisional_name, orbit.da / orbit.a
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
    plt.savefig(outfile + '.pdf', transparent=True)

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
    plt.scatter(arclen, da_over_a, marker='o', facecolor='b', edgecolor=plot_fanciness.ALMOST_BLACK,
                linewidth=0.15, alpha=0.5)
    plt.xlabel('arc length of orbit (days)')
    plt.ylabel('d(a)/a (percent)')
    plot_fanciness.remove_border()
    plt.draw()
    outfile = 'delta_a_over_a_13AE_corrected'
    plt.savefig(outfile + '.pdf', transparent=True)
    print 'objects plotted:', len(arclen)


def main():
    # parsers.output_discoveries_for_animation()
    discoveries = parsers.ossos_release_parser()
    top_down_SolarSystem(discoveries, plot_blocks=True, future_blocks=False, plot_Ijiraq=True,
                         label_blocks=True)

# orbit_fit_residuals(discoveries)
# delta_a_over_a(discoveries)


if __name__ == "__main__":
    main()


