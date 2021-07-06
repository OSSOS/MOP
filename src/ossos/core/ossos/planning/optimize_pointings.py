#!python
from astropy import units
from astropy.coordinates import SkyCoord
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

cfht = ephem.Observer()
cfht.lat = 0.344
cfht.lon = -2.707
cfht.elevation = 4100
cfht.date = '2018/03/28 20:00:00'
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
    @return: None
    """

    et = EphemTarget(name, ephem_format=ephem_format, runid=runid)
    # determine the mean motion of target KBOs in this field.
    field_kbos = []
    center_ra = 0
    center_dec = 0

    pointing_date = mpc.Time(pointing_date)
    start_date = pointing_date - TimeDelta(8.1 * units.day)
    end_date = start_date + TimeDelta(17 * units.day)
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


def optimize(orbits, required, locations, tokens, camera_name="MEGACAM_40"):
    """

    @param orbits: a dictionary of orbits to optimize the point of.
    @param required: list of objects that MUST be observed.
    @param locations: locations of the objects at the start of observing period.
    @param tokens: List of tokens (object names) that we will try and cover.
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
        separations = obj.coordinate.separation(locations)
        possible_tokens = tokens[separations < 1.3 * units.degree]
        this_required = []
        for this_token in required:
            if this_token in possible_tokens:
                this_required.append(this_token)
        # This object is not inside the existing coverage.
        max_sources_in_pointing = 0
        best_coverage = []
        p = SkyCoord(obj.coordinate.ra,
                     obj.coordinate.dec)
        pointing = Camera(p, camera=camera_name)
        if len(possible_tokens) == 1:
            optimal_pointings[token] = pointing, possible_tokens
            logging.info(" {} is all alone!".format(token))
            continue

        logging.debug("examining possible optimizations")

        if len(this_required) == 1:
            best_coverage = [token]
        else:
            logging.debug("Maximizing inclusion of required targets")
            for idx in search_order:
                pointing.offset(index=idx)
                this_coverage = []
                for this_token in this_required:
                    if this_token in covered:
                        continue
                    this_obj = orbits[this_token]
                    if pointing.polygon.isInside(this_obj.coordinate.ra.degree, this_obj.coordinate.dec.degree):
                        this_coverage.append(this_token)

                if len(this_coverage) > max_sources_in_pointing:
                    max_sources_in_pointing = len(this_coverage)
                    best_coverage = this_coverage

        # best_pointing now contains the pointing that covers the largest number of required sources.
        # best_coverage is the list of sources (tokens) that this pointing covered.
        # now move around the best_pointing, keeping all the objects listed in best_coverage but maximizing the
        # number of other sources that get coverage (optimal_coverage)
        logging.info("{} pointing these {} required targets: {}".format(token, len(best_coverage), best_coverage))
        max_sources_in_pointing = 0
        optimal_coverage = []
        optimal_pointing = None
        for idx in search_order:
            pointing.offset(index=idx)
            exclude = False  # exclude this offset because it doesn't overlap one or more of the required sources.
            for this_token in best_coverage:
                this_obj = orbits[this_token]
                if not pointing.polygon.isInside(this_obj.coordinate.ra.degree, this_obj.coordinate.dec.degree):
                    exclude = True
                    break

            if exclude:
                continue

            # OK, this offset has all the required sources possible, how many extra sources do we get?
            this_coverage = []
            for this_token in possible_tokens:
                if this_token in covered:
                    continue
                this_obj = orbits[this_token]
                if pointing.polygon.isInside(this_obj.coordinate.ra.degree, this_obj.coordinate.dec.degree):
                    this_coverage.append(this_token)

            if len(this_coverage) > max_sources_in_pointing:
                max_sources_in_pointing = len(this_coverage)
                optimal_coverage = this_coverage
                optimal_pointing = idx

        # remove all sources covered by optimal_pointing from further consideration.
        sys.stdout.write("{} pointing covers: ".format(token))
        unique_coverage_list = []
        for this_token in optimal_coverage:
            if this_token not in covered:
                unique_coverage_list.append(this_token)
                sys.stdout.write(" {} ".format(this_token))
                covered.append(this_token)
        sys.stdout.write("\n")
        pointing.offset(index=optimal_pointing)
        optimal_pointings[token] = pointing, unique_coverage_list
        n = 0
        new_required = []
        for this_token in token_order:
            if this_token not in covered:
                new_required.append(this_token)
                n += 1
        logging.info("Remaining required targets: {}, targets covered: {}".format(n, len(covered)))
        required = new_required
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
                        choices=list(Camera._geometry.keys()),
                        help="Name of camera")
    parser.add_argument('--ephem-format', default='CFHT_API',
                        choices=['CFHT_API', 'CFHT_ET', 'GEMINI_ET'])

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
        abg_filename = os.path.splitext(filename)[0] + ".abg"
        orbits[token] = mp_ephem.BKOrbit(None, ast_filename=filename, abg_file=abg_filename)
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
