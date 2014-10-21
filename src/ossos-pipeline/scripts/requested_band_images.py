__author__ = 'Michele Bannister   git:@mtbannister'

import sys
import argparse

import datetime
from astropy.time import Time
from astropy.io import ascii

from ossos import storage
from ossos import mpc
from ossos import orbfit
from ossos.ssos import Query


def parse_kbo(kbo_filename,
              camera_filter,
              search_start_date=Time('2013-02-08', scale='utc'),
              search_end_date=Time(datetime.datetime.now().strftime('%Y-%m-%d'), scale='utc')):
    mpc_observations = mpc.MPCReader(kbo_filename).mpc_observations
    orbit = orbfit.Orbfit(mpc_observations)
    sys.stderr.write("Sending query on %s to SSOS\n" % orbit.name)

    query = Query(mpc_observations,
                  search_start_date=search_start_date,
                  search_end_date=search_end_date)  # searches full survey period
    obs_in_filter = parse_ssois_return(query.get(), camera_filter=camera_filter)

    return orbit, obs_in_filter


def parse_ssois_return(ssois_return, camera_filter='r.MP9601', telescope_instrument='CFHT/MegaCam'):
    assert camera_filter in ['r.MP9601', 'u.MP9301']
    ret_table = []

    table_reader = ascii.get_reader(Reader=ascii.Basic)
    table_reader.inconsistent_handler = _skip_missing_data
    table_reader.header.splitter.delimiter = '\t'
    table_reader.data.splitter.delimiter = '\t'
    table = table_reader.read(ssois_return)

    for row in table:
        # check if a dbimages object exists
        ccd = int(row['Ext']) - 1
        expnum = row['Image'].rstrip('p')
        X = row['X']
        Y = row['Y']
        mjd = row['MJD']

        # Excludes the OSSOS wallpaper.
        # note: 'Telescope_Insturment' is a typo in SSOIS's return format
        if (row['Telescope_Insturment'] == telescope_instrument) and (row['Filter'] == camera_filter) \
                and not row['Image_target'].startswith('WP'):
            ret_table.append(row)

    return ret_table


def _skip_missing_data(str_vals, ncols):
    """
    add a extra column if one is missing, else return None.
    """
    if len(str_vals) == ncols - 1:
        str_vals.append('None')
        return str_vals
    else:
        raise ValueError("not enough columns in table")


def main():
    parser = argparse.ArgumentParser(
        description='Run SSOIS on a given set of minor planets and return the available images in a particular filter.')

    parser.add_argument("--filter", "-f",
                        action="store",
                        default='r',
                        dest="filter",
                        choices=['r', 'u'],
                        help="passband: default is r'")
    parser.add_argument("--ossin",
                        action="store",
                        default="vos:OSSOS/dbaseclone/ast/",
                        help='vospace dbaseclone containerNode')
    parser.add_argument("--dbimages",
                        action="store",
                        default="vos:OSSOS/dbimages",
                        help='vospace dbimages containerNode')
    parser.add_argument('--type',
                        default='p',
                        choices=['o', 'p', 's'],
                        help="restrict type of image (unprocessed, reduced, calibrated)")
    parser.add_argument("--output", "-o",
                        action="store",
                        default="/Users/michele//Desktop/band.txt",
                        help='Location and name of output file containing image IDs.')

    args = parser.parse_args()

    if args.filter.lower().__contains__('r'):
        args.filter = 'r.MP9601'  # this is the old (standard) r filter for MegaCam
    if args.filter.lower().__contains__('u'):
        args.filter = 'u.MP9301'

    with open(args.output, 'w') as outfile:
        outfile.write("{:>10s} {:>10s} {:>2s} {:>5s} {:>5s} {:>16s} {:>16s} {:>16s} {:>12s}\n".format(
            "Object", "Image", "Ext", "X", "Y", "RA", "DEC", "time", "filter"))

    for kbo_filename in storage.listdir(args.ossin):
        orbit, obs_in_filter = parse_kbo(args.ossin + kbo_filename, args.filter)
        if len(obs_in_filter) > 0:
            with open(args.output, 'a') as outfile:
                for line in obs_in_filter:
                    outfile.write(
                        "{:>10s} {:>10s} {:>2s} {:6.1f} {:6.1f} {:8.16f} {:8.16f} {:>10s} {:>10s}\n".format(orbit.name,
                                                                                                            line[
                                                                                                                'Image'],
                                                                                                            str(line[
                                                                                                                'Ext']),
                                                                                                            line['X'],
                                                                                                            line['Y'],
                                                                                                            line[
                                                                                                                'Object_RA'],
                                                                                                            line[
                                                                                                                'Object_Dec'],
                                                                                                            Time(line[
                                                                                                                     'MJD'],
                                                                                                                 format='mjd',
                                                                                                                 scale='utc'),
                                                                                                            line[
                                                                                                                'Filter']))

                outfile.write('\n')


if __name__ == '__main__':
    main()
