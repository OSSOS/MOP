#!python

from astropy import units
from astropy.coordinates import SkyCoord
from astropy.time import TimeDelta
import numpy as np
from ossos.cameras import Camera
from ossos import mpc
import mp_ephem
from ossos.ephem_target import EphemTarget
import sys, os
import argparse
import logging
import math

dec_offset = 4600*0.185/3600.0 * units.degree
ra_offset = 2048*0.185/3600.0 * units.degree


def create_ephemeris_file(name, camera, kbos, orbits, pointing_date):
    """

    @param name: name of the pointing
    @param camera: camera object that is this pointing
    @param kbos: list of kbos covered by this pointing
    @param orbits: orbits of all known kbos
    @param pointing_date: data of pointing associated with camera object
    @return: None
    """

    et = EphemTarget(name, format="CFHT API")
    # determine the mean motion of target KBOs in this field.
    field_kbos = []
    center_ra = 0
    center_dec = 0

    pointing_date = mpc.Time(pointing_date)
    start_date = pointing_date - TimeDelta(8.1 * units.day)
    end_date = start_date + TimeDelta(17 * units.day)
    time_step = TimeDelta(3.0 * units.hour)

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
        dt = pointing_date - today
        cc.dra = (mean_motion[0] * units.radian / dt.to(units.hour)).to(units.arcsec / units.hour).value * math.cos(dec)
        cc.ddec = (mean_motion[1] * units.radian / dt.to(units.hour)).to(units.arcsec / units.hour).value
        cc.mag = max_mag
        et.append(cc)

    et.save()
    return


def optimize(orbits, required, locations, tokens):
    """

    @param orbits: a dictionary of orbits to optimize the point of.
    @param required: list of objects that MUST be observed.
    @return:
    """

    ra_offsets =  np.array([0, 1, -1, 2, -2, 3, -3, 4, -4])*ra_offset
    dec_offsets = np.array([0.5, -0.5, 1.5, -1.5])*dec_offset

    # Get the tokens that will be paged through in random order.
    token_order = np.random.permutation(required)
    optimal_pointings = {}
    covered = []  # the objects that have already been covered by a planned pointing.

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
        if len(possible_tokens) == 1:
            p = SkyCoord(obj.coordinate.ra,
                         obj.coordinate.dec + dec_offset * 0.5)
            pointing = Camera(p)
            optimal_pointings[token] = pointing, possible_tokens
            logging.info(" {} is all alone!".format(token))
            continue

        logging.debug("examining possible optimizations")
        # create two arrays that provide RA/DEC offsets from a central pointing
        # that includes the target.  All these offset pointings will be explored to
        # find the one that includes the largest number of targets.
        ras = obj.coordinate.ra + ra_offsets / np.cos(obj.coordinate.dec.radian)
        decs = obj.coordinate.dec + dec_offsets
        p=[]
        for ra in ras:
            for dec in decs:
                p.append([ra, dec])
        coords = SkyCoord(p)

        if len(this_required) == 1:
            best_coverage = [token]
        else:
            logging.debug("Maximizing inclusion of required targets")
            for coord in coords:
                pointing = Camera(coord)

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
        for coord in coords:
            pointing = Camera(coord)
            exclude = True  # exclude this offset because it doesn't overlap one or more of the required sources.
            for this_token in best_coverage:
                this_obj = orbits[this_token]
                exclude = not pointing.polygon.isInside(this_obj.coordinate.ra.degree, this_obj.coordinate.dec.degree)

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
                optimal_pointing = pointing

        #  remove all sources covered by optimal_pointing from further consideration.
        optimal_unique_count = 0
        sys.stdout.write("{} pointing covers: ".format(token))
        unique_coverage_list = []
        for this_token in optimal_coverage:
            if this_token not in covered:
                unique_coverage_list.append(this_token)
                sys.stdout.write(" {} ".format(this_token))
                covered.append(this_token)
        sys.stdout.write("\n")
        optimal_pointings[token] = optimal_pointing, unique_coverage_list
        n = 0
        new_required = []
        for this_token in token_order:
            if this_token not in covered:
                new_required.append(this_token)
                n += 1
        logging.info("Remaining required targets: {}, targets covered: {}".format(n, len(covered)))
        required = new_required
    return optimal_pointings


if __name__ == '__main__':

    parser = argparse.ArgumentParser()
    parser.add_argument('date',
                        type=str,
                        help="ISO date string to optimize the pointings too")
    parser.add_argument('pointing_objects',
                        type=str,
                        help="Name of file that contains list of objects to try and observe.")
    parser.add_argument('required_objects',
                        type=str,
                        help="name of file that contains a list of required objects")
    parser.add_argument('--nattempts',
                        help="Number of random variations of pointings to try",
                        default=2)

    args = parser.parse_args()

    reader = mp_ephem.EphemerisReader()

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

    pointings_filename = os.path.splitext(required_filename)[0]+"_pointings.txt"

    filenames = open(args.pointing_objects).readlines()

    orbits = {}
    locations = []
    tokens = []

    # load the orbits of all objects of interest.
    for filename in filenames:
        filename = filename.strip()
        token = os.path.splitext(os.path.basename(filename))[0]
        abg_filename = os.path.splitext(filename)[0]+".abg"
        orbits[token] = mp_ephem.BKOrbit(None, ast_filename=filename, abg_file=abg_filename)
        obj = orbits[token]
        obj.predict(pointing_date)
        tokens.append(token)
        locations.append([obj.coordinate.ra.degree, obj.coordinate.dec.degree])
        print token, obj.r_mag

    # Turn the object locations at the time of interest into a SkyCoord numpy array.
    locations = SkyCoord(locations, unit='degree')
    tokens = np.array(tokens)
    minimum_number_of_pointings = len(required_objects)
    best_order = None

    for attempt in range(args.nattempts):
        logging.info("Attempt : {} \n".format(attempt))
        pointings = optimize(orbits, required_objects, locations, tokens)
        if minimum_number_of_pointings >= len(pointings):
            minimum_number_of_pointings = len(pointings)
            with open(pointings_filename, 'w') as pobj:
                pobj.write("index {}\n".format(pointing_date))
                pointing_number = 0
                for token in pointings:
                    pobj.write("{} {} {} {} # {}\n".format(pointing_number+1,
                                                      "{}".format(token),
                                                      pointings[token][0].coord.to_string("hmsdms", sep=" "),
                                                      2000,
                                                      len(pointings[token][1])))
                    pointing_number += 1
                    create_ephemeris_file(token, pointings[token][0], pointings[token][1], orbits, pointing_date)



