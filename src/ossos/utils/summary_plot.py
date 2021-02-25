import sys
import math

from matplotlib import pyplot
import numpy
import ephem

from ossos import orbfit
from ossos import mpc
from ossos import figures

figures.setFigForm()


def opposition(coordinate):
    ec = ephem.Ecliptic(0, 0)
    ra = ephem.hours(coordinate.ra.radians)
    dec = ephem.degrees(coordinate.dec.radians)
    ec.from_radec(ra, dec)

    opp_angle = math.pi
    opp_date = -1

    start_date = ephem.date('2013/01/01')
    sun = ephem.Sun()
    for day in range(365):
        d = ephem.date(start_date + day)
        sun.compute(d)
        opp = math.fabs(ephem.degrees(ec.lon - sun.hlon))
        if opp < opp_angle:
            opp_angle = opp
            opp_date = ephem.date(start_date + day)
    sun.compute(opp_date)
    print(sun.ra, coordinate.ra, opp_date)   
    return ephem.julian_date(opp_date)


def build_summary_plot(mpc_obs):
    orbit = orbfit.Orbfit(mpc_obs)
    residuals = orbit.residuals
    mags = []
    errs = []
    ras = []
    decs = []
    perrs = []
    times = []
    for ob in mpc_obs:
        if ob.comment.mag is None:
            continue
        if ob.null_observation:
            continue
        try:
            mags.append(float(ob.comment.mag))
            errs.append(float(ob.comment.mag_uncertainty))
            times.append(float(ob.date.jd))
            ras.append(float(ob.ra_residual))
            decs.append(float(ob.dec_residual))
            perrs.append(float(ob.comment.plate_uncertainty))
        except Exception as err:
            sys.write.stderr(str(err))
    ras = numpy.array(ras)
    decs = numpy.array(decs)
    mags = numpy.array(mags)
    mags = mags - mags.mean()
    times = numpy.array(times)
    ax1.plot(times, mags, '.', alpha=0.05)
    ax3.plot(times, ras, '.', alpha=0.1)
    ax4.plot(times, decs, '.', alpha=0.1)
    ax2.plot(ras, decs, '+', alpha=0.1)


if __name__ == '__main__':

    fig = pyplot.Figure()
    ax1 = pyplot.subplot(221)
    ax2 = pyplot.subplot(222)
    ax3 = pyplot.subplot(223)
    ax4 = pyplot.subplot(224)
    ax1.set_ylabel(r'$\Delta_{mag}$')
    ax1.set_xlabel("jd (days)")
    ax2.set_aspect('equal', adjustable='box')
    ax3.set_ylabel(r'$\Delta_{RA}$')
    ax4.set_ylabel(r'$\Delta_{DEC}$')
    ax3.set_xlabel("JD (days)")
    ax4.set_xlabel("JD (days)")
    ax2.set_xlabel(r'$\Delta_{RA}$')
    ax2.set_ylabel(r'$\Delta_{DEC}$')
    t = numpy.arange(0, numpy.pi*2.5, 0.1)
    x = 0.05 * numpy.cos(t)
    y = 0.05 * numpy.sin(t)
    ax2.plot(x, y, '-k')

    for mpc_file in sys.argv[1:]:
        mpc_obs = mpc.MPCReader(mpc_file).mpc_observations
        build_summary_plot(mpc_obs)

    ax2.set_xlim(-0.5, 0.5)
    ax2.set_ylim(-0.5, 0.5)
    ax3.set_ylim(-0.5, 0.5)
    ax4.set_ylim(-0.5, 0.5)
    ax1.set_ylim(-0.5, 0.5)

    pyplot.savefig("orbits.pdf")
