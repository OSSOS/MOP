from astropy import units
from astropy.coordinates import SkyCoord
from astropy.table import Table
from astropy.time import TimeDelta, Time
import numpy as np
from ossos.cameras import Camera
from ossos import mpc
import mp_ephem
from ossos.ephem_target import EphemTarget
import sys
import os
import argparse
import logging
import math
from copy import deepcopy
import ephem
from ephemeris import Ephemeris

cfht = ephem.Observer()
cfht.lat = 0.344
cfht.lon = -2.707
cfht.elevation = 4100
cfht.date = '2021/09/01 10:00:00'
sun = ephem.Sun()
fb = ephem.FixedBody()


def is_up(coordinate, current_time):
    """
    Given the position and time determin if the given target is up.

    @param coordinate: the J2000 location of the source
    @param current_time: The time of the observations
    @return: True/False
    """
    cfht.date = current_time.iso.replace('-', '/')
    cfht.horizon = math.radians(-7)
    sun.compute(cfht)
    sun_rise = Time(str(sun.rise_time).replace('/', '-'))
    sun_set = Time(str(sun.set_time).replace('/', '-'))

    if current_time < sun_set or current_time > sun_rise:
        return False

    fb._ra = coordinate.ra.radian
    fb._dec = coordinate.dec.radian
    cfht.horizon = math.radians(40)
    fb.compute(cfht)
    fb_rise_time = Time(str(fb.rise_time).replace('/', '-'))
    fb_set_time = Time(str(fb.set_time).replace('/', '-'))

    if (current_time > fb_set_time > fb_set_time or
       fb_rise_time > current_time > fb_set_time):
        return False
    return True


def create_ephemeris_file(name, camera, kbos, orbits, pointing_date, runid, ephem_format="CFHT API"):
    """

    @param name: name of the pointing
    @param camera: camera object that is this pointing
    @param kbos: list of kbos covered by this pointing
    @param orbits: orbits of all known kbos
    @param pointing_date: data of pointing associated with camera object
    @param runid:  The CFHT RunID that will be used to observer the target.
    @param ephem_format: What format to write the pointing list out in.
    @return: None
    """

    et = EphemTarget(name, ephem_format=ephem_format, runid=runid)
    # determine the mean motion of target KBOs in this field.
    field_kbos = []
    center_ra = 0
    center_dec = 0

    pointing_date = mpc.Time(pointing_date)
    start_date = pointing_date - TimeDelta(0.1 * units.hour)
    end_date = start_date + TimeDelta(2 * units.hour)
    time_step = TimeDelta(1.5 * units.hour)

    # Compute the mean position of KBOs in the field on current date.
    for kbo_name in kbos:
        kbo = orbits[kbo_name]
        kbo.predict(pointing_date)
        ra = kbo.coordinate.ra
        dec = kbo.coordinate.dec
        field_kbos.append(kbo)
        center_ra += ra.radian
        center_dec += dec.radian

    today = start_date
    while today < end_date:
        today += time_step
        mean_motion = (0, 0)
        max_mag = 0.0
        if len(field_kbos) > 0:
            current_ra = 0
            current_dec = 0
            for kbo in field_kbos:
                kbo.predict(today)
                max_mag = max(max_mag, kbo.r_mag)
                current_ra += kbo.coordinate.ra.radian
                current_dec += kbo.coordinate.dec.radian
            mean_motion = ((current_ra - center_ra) / len(field_kbos),
                           (current_dec - center_dec) / len(field_kbos))
        ra = camera.coordinate.ra.radian + mean_motion[0]
        dec = camera.coordinate.dec.radian + mean_motion[1]
        cc = SkyCoord(ra=ra,
                      dec=dec,
                      unit=(units.radian, units.radian),
                      obstime=today)
        if not is_up(cc, today):
            continue
        dt = pointing_date - today
        cc.dra = (mean_motion[0] * units.radian / dt.to(units.hour)).to(units.arcsec / units.hour).value * math.cos(dec)
        cc.ddec = (mean_motion[1] * units.radian / dt.to(units.hour)).to(units.arcsec / units.hour).value
        cc.mag = max_mag
        et.append(cc)

    et.save()
    return


def optimize(orbits, required, locations, tokens, camera_name="DEIMOS"):
    """

    @param orbits: a dictionary of orbits to optimize the point of.
    @param required: list of objects that MUST be observed.
    @param locations: locations of the objects at the start of observing period.
    @param tokens: List of tokens (object names) that we will try and cover.
    @param camera_name: name of camera to look up in geometry file.
    @return:
    """

    # Get the tokens that will be paged through in random order.
    token_order = np.random.permutation(required)
    optimal_pointings = {}
    covered = []  # the objects that have already been covered by a planned pointing.
    search_order = [0, ]
    # search_order = [22 + 4]
    # search_order.extend(range(len(Camera.names)))
    # For each required target find the pointing that will include the largest number of other required targets
    # and then tweak that specific pointing to include the maximum number of secondary targets.
    for token in token_order:
        if token in covered:
            continue
        logging.debug("Optimizing pointing for required target: {}".format(token))
        if token not in orbits:
            logging.error("No orbit available for: {}".format(token))
            continue
        obj = orbits[token]
        q = SkyCoord(obj.coordinate.ra,
                     obj.coordinate.dec)
        pointing = Camera(q, camera=camera_name, name=token)
        bbox = pointing.polygon.boundingBox()
        radius = (max((bbox[1]-bbox[0])**2, (bbox[3]-bbox[2])**2)*2)**0.5
        separations = obj.coordinate.separation(locations)
        logging.info(f"Seaching within {radius} degrees of {token}")
        possible_tokens = tokens[separations < radius * units.degree]
        best_coverage = [token]
        optimal_pointing = pointing
        if len(possible_tokens) == 1:
            continue
        for dx in np.arange((q.ra.degree-bbox[0]-1/60.0), (bbox[1] - q.ra.degree), 1/60.0):
            for dy in np.arange((q.dec.degree-bbox[2]-1/60.0), (bbox[3] - q.dec.degree), 1/60.0):
                p = SkyCoord(obj.coordinate.ra + dx*units.degree,
                             obj.coordinate.dec + dy*units.degree)
                pointing = Camera(p, camera=camera_name, name=token)
                this_required = []
                for this_token in required:
                    if this_token in possible_tokens:
                        this_required.append(this_token)
                logging.debug(f"Field {token} near required targets {this_required}")
                # This object is not inside the existing coverage.
                if len(this_required) == 1:
                    continue
                else:
                    logging.debug("Finding required targets within pointing")
                    this_coverage = []
                    for this_token in this_required:
                        if this_token in covered:
                            continue
                        if pointing.polygon.isInside(orbits[this_token].coordinate.ra.degree, orbits[this_token].coordinate.dec.degree):
                            logging.debug(f"Adding {this_token} to pointing {token}")
                            this_coverage.append(this_token)
                    if len(this_coverage) > len(best_coverage):
                        logging.info(f"Best {token} pointing is {pointing.coordinate} with {len(this_coverage)} targets.")
                        best_coverage = this_coverage
                        optimal_pointing = pointing

        optimal_coverage = []

        for dx in np.arange((q.ra.degree - bbox[0] - 2 / 60.0), (bbox[1] - q.ra.degree - 1/60.0), 1 / 60.0):
            for dy in np.arange((q.dec.degree - bbox[2] - 2 / 60.0), (bbox[3] - q.dec.degree - 1/60.0), 1 / 60.0):
                p = SkyCoord(obj.coordinate.ra + dx * units.degree,
                             obj.coordinate.dec + dy * units.degree)
                pointing = Camera(p, camera=camera_name, name=token)
                # Skip offset if we loose required object.
                exclude = False
                for this_token in best_coverage:
                    if not pointing.polygon.isInside(orbits[this_token].coordinate.ra.degree, orbits[this_token].coordinate.dec.degree):
                        exclude = True
                        break
                if exclude:
                    continue

                this_coverage = []

                for this_token in possible_tokens:
                    if this_token in covered:
                        continue
                    if pointing.polygon.isInside(orbits[this_token].coordinate.ra.degree, orbits[this_token].coordinate.dec.degree):
                        this_coverage.append(this_token)

                if len(this_coverage) > len(optimal_coverage):
                    print(pointing.polygon.isInside(orbits[token].coordinate.ra.degree, orbits[token].coordinate.dec.degree))
                    optimal_coverage = this_coverage
                    optimal_pointing = pointing

        # remove all sources covered by optimal_pointing from further consideration.
        unique_coverage_list = []
        for this_token in best_coverage:
            if this_token not in covered:
                unique_coverage_list.append(this_token)
                covered.append(this_token)

        optimal_pointings[token] = optimal_pointing, unique_coverage_list

    return optimal_pointings


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('date',
                        type=str,
                        help="ISO date string to optimize the pointings too")
    parser.add_argument('runid',
                        help="CFHT RUN ID")
    parser.add_argument('pointing_objects',
                        type=str,
                        help="Name of file that contains list of objects to try and observe.")
    parser.add_argument('required_objects',
                        type=str,
                        help="name of file that contains a list of required objects")
    parser.add_argument('--nattempts', type=int,
                        help="Number of random variations of pointings to try",
                        default=2)
    parser.add_argument('--camera', default="MEGACAM_40",
                        choices=Camera.known_cameras,
                        help="Name of camera")
    parser.add_argument('--ephem-format', default='CFHT_API',
                        choices=['CFHT_API', 'CFHT_ET', 'GEMINI_ET', 'KECK'])

    args = parser.parse_args()

    logging.basicConfig(level=logging.INFO)

    pointing_date = args.date

    logging.info("Date for orbit predictions: {}".format(args.date))
    logging.info("Building pointings that include all targets in {}".format(args.required_objects))
    logging.info("Optimized to include as many objects in {} as possible".format(args.pointing_objects))

    required_objects = []
    required_filename = args.required_objects
    for required_object in open(required_filename):
        if '#' in required_object:
            continue
        required_objects.append(required_object.strip())

    pointings_filename = os.path.splitext(required_filename)[0] + "_pointings.txt"

    filenames = open(args.pointing_objects).readlines()

    orbits = {}
    tokens = []

    # load the orbits of all objects of interest.
    for filename in filenames:
        filename = filename.strip()
        token = os.path.splitext(os.path.basename(filename))[0]
        try:
            abg_filename = os.path.splitext(filename)[0] + ".abg"
            orbits[token] = mp_ephem.BKOrbit(None, ast_filename=filename, abg_file=abg_filename)
        except Exception as ex:
            # Try loading an 'autoclouds10' file.
            token = os.path.splitext(os.path.basename(filename))[0].removesuffix('_autoclouds10')
            ephem_filename = os.path.join(filename, f"{token}_autoclouds10_sep21_ephem.txt")
            orbits[token] = Ephemeris(token, ephem_filename)

        tokens.append(token)

    # Turn the object locations at the time of interest into a SkyCoord numpy array.
    tokens = np.array(tokens)
    minimum_number_of_pointings = len(required_objects)

    best_pointing_list = []
    for attempt in range(args.nattempts):
        locations = []
        for token in tokens:
            orbits[token].predict(pointing_date)
            locations.append([orbits[token].coordinate.ra.degree, orbits[token].coordinate.dec.degree])
        locations = SkyCoord(locations, unit='degree')
        logging.info("Attempt : {} \n".format(attempt))
        pointings = optimize(orbits, required_objects, locations, tokens)
        if minimum_number_of_pointings >= len(pointings):
            minimum_number_of_pointings = len(pointings)
            best_pointing_list = deepcopy(pointings)

    with open(pointings_filename, 'w') as pobj:
        pobj.write("index {}\n".format(pointing_date))
        pointing_number = 0
        for token in best_pointing_list:
            pobj.write("{} {} {} {} # {}\n".format(pointing_number + 1,
                                                   "{}".format(token),
                                                   best_pointing_list[token][0].coord.to_string("hmsdms", sep=" "),
                                                   2000,
                                                   len(best_pointing_list[token][1])))
            pointing_number += 1
            create_ephemeris_file(token, best_pointing_list[token][0], best_pointing_list[token][1], orbits,
                                  pointing_date, args.runid, ephem_format=args.ephem_format.replace("_", " "))


if __name__ == '__main__':
    sys.exit(main())
