
import sys
from ossos import orbfit
from ossos import mpc
from matplotlib import pyplot
import numpy
import ephem
import math

import figures

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
    print sun.ra, coordinate.ra, opp_date   
    return ephem.julian_date(opp_date)


def build_summary_plot(original_mpc_obs, modified_mpc_obs, result_pdf_file):
    origin = original_mpc_obs
    modif = modified_mpc_obs
    op_jd = opposition(origin[0].coordinate)
    if False:
        for idx in range(len(origin)):
            c = origin[idx]
            d = True
            for obs2 in modif:
                if c.date == obs2.date:
                    d = False
                    break
            if d:
                del origin[idx]

    data = [origin, modif]
    start_time = origin[0].date
    end_time = origin[-1].date

    ax1 = pyplot.subplot(221)
    ax2 = pyplot.subplot(222)
    ax3 = pyplot.subplot(223)
    ax4 = pyplot.subplot(224)
    colors = ['g', 'b']
    offset = [0, 5]
    data_set_names = ['before', 'after']
    for idx in range(len(data)):
        obs = data[idx]
        orbit = orbfit.Orbfit(obs)
        residuals = orbit.residuals
        op_jd = op_jd - start_time.jd
        mags = []
        errs = []
        ras = []
        decs = []
        perrs = []
        times = []
        for ob in obs:
            if ob.comment.mag is None:
                continue
            if ob.null_observation:
                continue
            try:
                mags.append(float(ob.comment.mag))
                errs.append(float(ob.comment.mag_uncertainty))
                times.append(float(ob.date.jd) - start_time.jd + offset[idx])
                ras.append(float(ob.ra_residual))
                decs.append(float(ob.dec_residual))
                perrs.append(float(ob.comment.plate_uncertainty))
            except Exception as e:
                raise e
        ras = numpy.array(ras)
        decs = numpy.array(decs)
        mags = numpy.array(mags)
        mags = mags - mags.mean()
        times = numpy.array(times)
        errs = numpy.array(errs)
        ax1.errorbar(times, mags, yerr=errs, ecolor=colors[idx], fmt=None)
        ax1.plot([op_jd, op_jd], [-1, 1], '--k')
        ax1.plot([0, end_time.jd - start_time.jd], [0,0], '--k')
        ax3.errorbar(times, ras, yerr=perrs, alpha=0.3, ecolor=colors[idx], fmt=None)
        ax4.errorbar(times, decs, yerr=perrs, alpha=0.3, ecolor=colors[idx], fmt=None)
        ax3.plot(times, ras, 's{}'.format(colors[idx]), alpha=0.3)
        ax4.plot(times, decs, 's{}'.format(colors[idx]), alpha=0.3)
        ax2.plot(ras, decs, '+{}'.format(colors[idx]),
                 label=r'${:>8s} : RA_\sigma:{:5.2f} DEC_\sigma:{:5.2f}$'.format(data_set_names[idx], ras.std(), decs.std()))

    ax1.yaxis.set_ticks(numpy.arange(-1, 1.1, 0.25))
    ax3.set_ylim(-0.5, 0.5)
    ax1.set_ylim(-1, 1)
    ax1.set_ylabel(r'$\Delta_{mag}$')
    ax1.set_xlabel("jd - [{}]".format(start_time))
    ax3.plot([0, end_time.jd - start_time.jd], [0, 0])
    ax4.plot([0, end_time.jd - start_time.jd], [0, 0])
    ax3.set_xlim(0, end_time.jd - start_time.jd)
    ax4.set_xlim(0, end_time.jd - start_time.jd)
    ax1.set_xlim(0, end_time.jd - start_time.jd)
    t = numpy.arange(0, numpy.pi*2.5, 0.1)
    x = ras.std() * numpy.cos(t)
    y = decs.std() * numpy.sin(t)
    ax2.plot(x, y, '-k')
    ax2.set_aspect('equal', adjustable='box')
    ax3.set_ylabel(r'$\sigma_{RA}$')
    ax4.set_ylabel(r'$\sigma_{DEC}$')
    ax3.set_xlabel("time (days)")
    ax4.set_xlabel("time (days)")
    ax4.set_ylim(-0.5, 0.5)
    ax3.set_ylim(-0.5, 0.5)
    ax2.legend(loc='center left', bbox_to_anchor=(1, 0.5))
    ax2.plot(0, 0, 'ok')
    ax2.set_xlabel(r'$\sigma_{RA}$')
    ax2.set_ylabel(r'$\sigma_{DEC}$')
    ax2.set_ylim(-0.3, 0.3)
    ax2.set_xlim(-0.3, 0.3)
    ax2.yaxis.set_ticks(numpy.arange(-0.3, 0.4, 0.1))
    ax2.xaxis.set_ticks(numpy.arange(-0.3, 0.4, 0.1))

    if False:
        ax4 = pyplot.subplot(224)
        ras = []
        decs = []
        for jd in numpy.arange(start_time.jd, end_time.jd, 1):
            orbit.predict(jd)
            ras.append(orbit.coordinate.ra.degrees)
            decs.append(orbit.coordinate.dec.degrees)
        ax4.plot(ras, decs)
        for idx in range(len(data)):
            xerr = []
            ras = []
            decs = []
            obs = data[idx]
            for ob in obs:
                ras.append(ob.coordinate.ra.degrees)
                decs.append(ob.coordinate.dec.degrees)
                xerr.append(ob.comment.plate_uncertainty / 3600.0)
            ax4.errorbar(ras, decs, yerr=xerr, xerr=xerr, fmt='.{}'.format(colors[idx]), alpha=0.2)

    pyplot.savefig(result_pdf_file)

if __name__ == '__main__':
    this_origin = mpc.MPCReader(sys.argv[1]).mpc_observations
    this_modfified = mpc.MPCReader(sys.argv[2]).mpc_observations
    build_summary_plot(this_origin, this_modfified, 'orbit.pdf')
