import argparse
import os
import time
from astropy import units
from astropy.io import ascii
from astropy.time import Time
from mp_ephem import horizons
from ossos import storage
from ossos.ssos import Query


_DIR_PATH_BASE = config._DIR_PATH_BASE
_FAMILY_LISTS = config._FAMILY_LISTS
_OUTPUT_DIR = config._OUTPUT_DIR


def main():
    """
    Input asteroid family, filter type, and image type to query SSOIS
    """

    parser = argparse.ArgumentParser(description='Run SSOIS and return the available images in a particular filter.')

    parser.add_argument("--filter",
                        action="store",
                        default='r',
                        dest="filter",
                        choices=['r', 'u'],
                        help="Passband: default is r.")
    parser.add_argument("--family", '-f',
                        action="store",
                        default=None,
                        help='List of objects to query.')
    parser.add_argument("--member", '-m',
                        action="store",
                        default=None,
                        help='Member object of family to query.')

    args = parser.parse_args()

    if args.family != None and args.member == None:
        get_family_info(str(args.family), args.filter)
    elif args.family == None and args.member != None:
        get_member_info(str(args.member), args.filter)
    else:
        print "Please input either a family or single member name"


def get_family_info(familyname, filtertype='r', imagetype='p'):
    """
    Query the ssois table for images of objects in a given family. Then parse through for desired image type,
    filter, exposure time, and telescope instrument
    """

    # establish input
    family_list = '{}/{}_family.txt'.format(_FAMILY_LISTS, familyname)

    if os.path.exists(family_list):
        with open(family_list) as infile:
            filestr = infile.read()
        object_list = filestr.split('\n')  # array of objects to query
    elif familyname == 'all':
        object_list = find_family.get_all_families_list()
    else:
        object_list = find_family.find_family_members(familyname)

    for obj in object_list[0:len(object_list) - 1]:  # skip header lines
        get_member_info(obj, filtertype)


def get_member_info(object_name, filtertype='r', imagetype='p'):
    """
    Query the ssois table for images of a given object. Then parse through for desired image type,
    filter, exposure time, and telescope instrument
    """

    # From the given input, identify the desired filter and rename appropriately                    Replace this?
    if filtertype.lower().__contains__('r'):
        filtertype = 'r.MP9601'  # this is the old (standard) r filter for MegaCam
    if filtertype.lower().__contains__('u'):
        filtertype = 'u.MP9301'

    # Define time period of image search, basically while MegaCam in operation
    search_start_date = Time('2013-01-01', scale='utc')  # epoch1=2013+01+01
    search_end_date = Time('2017-01-01', scale='utc')  # epoch2=2017+1+1

    print("----- Searching for images of object {}".format(object_name))

    image_list = []
    expnum_list = []
    ra_list = []
    dec_list = []

    query = Query(object_name, search_start_date=search_start_date, search_end_date=search_end_date)
    result = query.get()
    print(result)
    try:
        objects = parse_ssois_return(query.get(), object_name, imagetype, camera_filter=filtertype)
    except IOError:
        print("Sleeping 30 seconds")
        time.sleep(30)
        objects = parse_ssois_return(query.get(), object_name, imagetype, camera_filter=filtertype)

    print(objects)
    # Setup output, label columns
    if len(objects)>0:
        output = '{}/{}_object_images.txt'.format(_OUTPUT_DIR, object_name)
        with open(output, 'w') as outfile:
            outfile.write("{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\n".format(
                "Object", "Image", "Exp_time", "RA (deg)", "Dec (deg)", "time", "filter", "RA rate (\"/hr)", "Dec rate (\"/hr)"))

        for line in objects:
            with open(output, 'a') as outfile:
                image_list.append(object_name)
                expnum_list.append(line['Exptime'])
                t_start = Time(line['MJD'], format='mjd', scale='utc') - 1.0 * units.minute
                t_stop = t_start + 2 * units.minute
                hq = horizons.Query(object_name)
                hq.start_time = t_start
                hq.stop_time = t_stop
                hq.step_size = 1 * units.minute
                p_ra = hq.table[1]['R.A._(ICRF/J2000.0']
                p_dec = hq.table[1]['DEC_(ICRF/J2000.0']
                ra_dot = hq.table[1]['dRA*cosD']
                dec_dot = hq.table[1]['dDEC/dt']
                try:
                    outfile.write("{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\n".format(
                        object_name, line['Image'], line['Exptime'], p_ra, p_dec,
                        Time(line['MJD'], format='mjd', scale='utc'), line['Filter'], ra_dot, dec_dot))
                except Exception as e:
                    print("Error writing to outfile: {}".format(e))

    return image_list, expnum_list, ra_list, dec_list


def parse_ssois_return(ssois_return, object_name, imagetype, camera_filter='r.MP9601',
                       telescope_instrument='CFHT/MegaCam'):
    """
    Parse through objects in ssois query and filter out images of desired filter, type, exposure time, and instrument
    """

    assert camera_filter in ['r.MP9601', 'u.MP9301']

    ret_table = []
    good_table = 0

    table_reader = ascii.get_reader(Reader=ascii.Basic)
    table_reader.inconsistent_handler = _skip_missing_data
    table_reader.header.splitter.delimiter = '\t'
    table_reader.data.splitter.delimiter = '\t'
    table = table_reader.read(ssois_return)

    for row in table:
        # Excludes the OSSOS wallpaper.
        # note: 'Telescope_Insturment' is a typo in SSOIS's return format
        if not 'MegaCam' in row['Telescope_Insturment']:
            continue
        # Check if image of object exists in OSSOS observations
        if not storage.exists(storage.get_uri(row['Image'][:-1])):
            continue
        if not str(row['Image_target']).startswith('WP'):
           good_table += 1
           ret_table.append(row)

    if good_table > 0:
        print(" %d images found" % good_table)

    return ret_table


def _skip_missing_data(str_vals, ncols):
    """
    add a extra column if one is missing, else return None.
    """
    if len(str_vals) == ncols - 1:
        str_vals.append('None')
        return str_vals
    # else:
        # raise ValueError("not enough columns in table")
        # print '  Not enough columns in data table'


if __name__ == '__main__':
    main()
