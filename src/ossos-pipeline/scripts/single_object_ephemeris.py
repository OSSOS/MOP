__author__ = 'Michele Bannister   git:@mtbannister'

from ossos import ephem_target as et

if __name__ == '__main__':
    -    logger = logging.getLogger()
-    parser = argparse.ArgumentParser()
-    parser.add_argument('mpc_files', nargs='+', help='mpc_file to base ephemeris from.')
-    parser.add_argument('--verbose', '-v', action='store_true', default=None, help='verbose feedback')
-    parser.add_argument('--start', '-s', default=None, help='Date as YYYY/MM/DD, default is current date')
-    parser.add_argument('--range', '-r', default=30, help='Length of ephemeris is days', type=int)
-    parser.add_argument('--ccd', '-c', default=22, help='Offset so target is on this CCD (0 is the first CCD)')
-    parser.add_argument('--geometry', '-g', default="MEGACAM_36",
                         -                        help = 'camera geometry (see ossos.cameras for options); default is ' \
                                                         'MEGACAM_36')
-    parser.add_argument('--dra', default=0, help='Additional RA offset (arcmin)')
-    parser.add_argument('--ddec', default=0, help='Additional DECA offset (arcmin)')
-
-    opt = parser.parse_args()
-
if opt.verbose:
    -        logger.setLevel(logging.INFO)
-
-    start_date = (opt.start is not None and time.Time(opt.start, scale='utc')) or \
                  -                 time.Time(datetime.datetime.utcnow().isoformat(), scale='utc')
-
-    offset = Camera.geometry[opt.geometry]
-
-  ## build orbit instance for object
-
for mpc_file in opt.mpc_files:
    -        obs = []
-
for line in open(mpc_file, 'r'):
    -
if not line.startswith('#'):
    -                ob = mpc.Observation.from_string(line)
-                ob.null_observation = False
-                obs.append(ob)
-        orbit = orbfit.Orbfit(obs)
-
-        et = EphemTarget(orbit.name)
-
for day in range(opt.range):
    -            orbit.predict(time.Time(start_date.jd + day, scale='utc', format='jd'))
-
if opt.ccd:
    -                orbit.coordinate.ra = orbit.coordinate.ra + coordinates.RA(offset[int(opt.ccd)]["ra"],
                                                                                units.degree)
-                orbit.coordinate.dec = orbit.coordinate.dec + coordinates.Dec(offset[int(opt.ccd)]["dec"],
                                                                               units.degree)
-
if opt.dra:
    -                orbit.coordinate.ra = orbit.coordinate.ra + coordinates.RA(float(opt.dra), units.degree)
-
if opt.ddec:
    -                orbit.coordinate.dec = orbit.coordinate.dec + coordinates.Dec(float(opt.ddec), units.degree)
-            et.coordinates.append(orbit.coordinate)
-        et.save()
