#!python
from astropy import units
from astropy.coordinates import SkyCoord
import numpy as np
from ossos.cameras import Camera
import mp_ephem
import sys, os
import glob

dec_offset = 4600*0.185/3600.0 * units.degree
ra_offset = 2048*0.185/3600.0 * units.degree


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
        print("Optimizing pointing to cover {}".format(token))
        if token not in orbits:
            print token
            print orbits.keys()
        obj = orbits[token]
        separations = obj.coordinate.separation(locations)
        possible_tokens = tokens[separations < 1.3 * units.degree]
        this_required = []
        for this_token in required:
            if this_token in possible_tokens:
                this_required.append(this_token)
        # This object is not insize the existing coverage.
        max_sources_in_pointing = 0
        best_coverage = []
        if len(possible_tokens) == 1:
            p = SkyCoord(obj.coordinate.ra,
                         obj.coordinate.dec + dec_offset * 0.5)
            pointing = Camera(p)
            optimal_pointings[token] = pointing
            continue

        print("examining possible optimizations")

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
            print("Maximizing inclusion of required targets")
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
        print("Maximized required targets include: {}".format(best_coverage))
        max_sources_in_pointing = 0
        optimal_coverage = []
        optimal_pointing = None
        for coord in coords:
            pointing = Camera(coord)
            exclude = True  # exclude this offset because it doesn't overlap one or more of the required sources.
            for this_token in best_coverage:
                this_obj = orbits[this_token]
                exclude = True
                if not pointing.polygon.isInside(this_obj.coordinate.ra.degree, this_obj.coordinate.dec.degree):
                    break
                exclude = False

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
        optimal_pointings[token] = optimal_pointing
        sys.stdout.write("optimized coverage for {} added the following targets: ".format(token))
        for this_token in optimal_coverage:
            if this_token not in covered:
                sys.stdout.write(" {} ".format(this_token))
                covered.append(this_token)
        sys.stdout.write("\n")
        n = 0
        new_required = []
        for this_token in token_order:
            if this_token not in covered:
                new_required.append(this_token)
                n += 1
        print("Remaining required targets: {}, targets covered: {}".format(n, len(covered)))
        required = new_required
    return(optimal_pointings)


reader = mp_ephem.EphemerisReader()

pointing_date = "2016-12-27 10:00:00"

required_objects = []
required_filename = sys.argv[1]
for required_object in open(required_filename):
    required_objects.append(required_object.strip())

pointings_filename = os.path.splitext(required_filename)[0]+"_pointings.txt"

filenames = open(sys.argv[2]).readlines()

orbits = {}
locations = []
tokens = []
for filename in filenames:
    filename = filename.strip()
    token = os.path.splitext(os.path.basename(filename))[0]
    abg_filename = os.path.splitext(filename)[0]+".abg"
    orbits[token] = mp_ephem.BKOrbit(None, ast_filename=filename, abg_file=abg_filename)
    obj = orbits[token]
    obj.predict(pointing_date)
    tokens.append(token)
    locations.append([obj.coordinate.ra.degree, obj.coordinate.dec.degree])

locations = SkyCoord(locations, unit='degree')
tokens = np.array(tokens)
minimum_number_of_pointings = len(required_objects)
best_order = None
for attempt in range(10):
    sys.stdout.write("Attempt : {} \n".format(attempt))
    pointings = optimize(orbits, required_objects, locations, tokens)
    if minimum_number_of_pointings >= len(pointings):
        minimum_number_of_pointings = len(pointings)
        with open(pointings_filename, 'w') as pobj:
            pobj.write("index {}\n".format(pointing_date))
            pointing_number = 0
            for token in pointings:
                pobj.write("{} {} {} {}\n".format(pointing_number+1,
                                                  "et_{}".format(token),
                                                  pointings[token].coord.to_string("hmsdms", sep=" "),
                                                  2000))
                pointing_number += 1
