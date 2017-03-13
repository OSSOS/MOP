"""
Build the .ast file entries given an input .astrom file
"""
from ossos import astrom
from ossos.downloads.cutouts import downloader
from ossos import orbfit
from ossos import mpc
import sys
from ossos.gui import config
from ossos.util import  Time
from astropy.time import TimeDelta
from astropy import units
import re

alpha = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'

parser = astrom.AstromParser()
dlm = downloader.ImageCutoutDownloader()

filename = sys.argv[1]
sources = parser.parse(filename)

basename = filename[3:8].replace('+', 'p').replace('-', 'm')

idx = int(re.findall('_p(\d+)', filename)[0])

basename = "{}{:02d}".format(basename, idx)
if True:
    count = -1
    run = 0
    for source in sources.get_sources():
        count += 1
        if count > len(alpha):
            run += 1
        source_obs = []
        for reading in source.get_readings():
            try:
                cutout = dlm.download_cutout(reading, needs_apcor=True)
                name = "{}{}{}".format(basename, alpha[count % len(alpha)], run)
                phot = cutout.get_observed_magnitude()
                cen_x = phot['XCENTER'][0]
                cen_y = phot['YCENTER'][0]
                obs_mag = phot['MAG'][0]
                obs_mag_err = phot['MERR'][0]
            except Exception as ex:
                sys.stderr.write("ERROR processing reading {}".format(reading))
                sys.stderr.write(str(ex))
                break
            header = cutout.fits_header


            #(x, y, extno) = source_cutout.world2pix(ra, dec, usepv=False)
            #cutout.update_pixel_location((cen_x, cen_y))

            mjd_obs = float(header.get('MJD-OBS'))
            exptime = float(header.get('EXPTIME'))

            mpc_date = Time(mjd_obs,
                            format='mjd',
                            scale='utc',
                            precision=5)
            mpc_date += TimeDelta(exptime * units.second) / 2.0
            date = mpc_date.mpc

            obs = mpc.Observation(
                null_observation=False,
                minor_planet_number=None,
                provisional_name=name,
                discovery=True,
                note1=None,
                note2=config.read('MPC.NOTE2DEFAULT')[0],
                date=date,
                ra=cutout.ra,
                dec=cutout.dec,
                mag=obs_mag,
                mag_err=obs_mag_err,
                frame=reading.obs.rawname,
                band=header['FILTER'],
                xpos=cutout.observed_x,
                ypos=cutout.observed_y,
                comment='AUTO',
                astrometric_level=cutout.astrom_header.get('ASTLEVEL', None)
                )


            source_obs.append(obs)
            with open(name+".mpc", 'a') as fout:
                fout.write(obs.to_tnodb()+"\n")

        if len(source_obs) == 3:
            mags = 0
            for source in source_obs:
                mags += source.mag
            mags = mags/len(source_obs)
            orbit = orbfit.Orbfit(source_obs)
            residuals = orbit.residuals(overall=True)
            print orbit.summarize()
            if orbit.a > 5*units.AU and mags < 24.7:
                file('keepers.txt', 'a').write("{}\n".format(source_obs[0].provisional_name))

