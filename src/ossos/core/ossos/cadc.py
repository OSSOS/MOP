#!python
import argparse
from astropy.io import votable
import warnings
from astropy.table import Table
import requests
from io import StringIO
from astropy.time import Time


def cfht_megacam_tap_query(ra_deg=180.0, dec_deg=0.0, width=1, height=1, date=None):
    """Do a query of the CADC Megacam table.

    Get all observations inside the box (right now it turns width/height into a radius, should not do this).

    @rtype : Table
    @param ra_deg: center of search region, in degrees
    @param dec_deg: center of search region in degrees
    @param width: width of search region in degrees
    @param height: height of search region in degrees
    @param date: ISO format date string.  Query will be +/- 0.5 days from date given.
    """

    radius = min(90, max(width, height) / 2.0)

    query = ("SELECT "
             "COORD1(CENTROID(Plane.position_bounds)) AS RAJ2000,"
             "COORD2(CENTROID(Plane.position_bounds)) AS DEJ2000,"
             "target_name "
             "FROM "
             "caom2.Observation as o "
             "JOIN caom2.Plane as Plane on o.obsID=Plane.obsID "
             "WHERE o.collection = 'CFHT' "
             "AND o.instrument_name = 'MegaPrime' "
             "AND INTERSECTS( CIRCLE('ICRS', %f, %f, %f), Plane.position_bounds ) = 1")

    query = query % (ra_deg, dec_deg, radius)

    if date is not None:
        mjd = Time(date, scale='utc').mjd
        query += " AND Plane.time_bounds_lower <= {} AND {} <= Plane.time_bounds_upper ".format(mjd+0.5, mjd-0.5)

    data = {"QUERY": query,
            "REQUEST": "doQuery",
            "LANG": "ADQL",
            "FORMAT": "votable"}

    url = "http://www.cadc.hia.nrc.gc.ca/tap/sync"

    warnings.simplefilter('ignore')
    ff = StringIO(requests.get(url, params=data).content)
    ff.seek(0)
    table = votable.parse(ff).get_first_table().to_table()
    assert isinstance(table, Table)
    return table


def main(ra_deg, dec_deg, width, height):
    print(cfht_megacam_tap_query(ra_deg, dec_deg, width, height))

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('ra', type=float,
                        help="RA (in degrees) of the centre of the search area.")
    parser.add_argument('dec', type=float,
                        help="DEC (in degrees) of the centre of the search area.")
    parser.add_argument('width', nargs='?', type=float,
                        help="width (in degrees) of the search area.", default=10.0/60.0)
    parser.add_argument('height', nargs='?', type=float,
                        help="height (in degrees) of the search area. (default==width)",
                        default=None)

    args = parser.parse_args()
    if args.height is None:
        args.height = args.width
    main(args.ra, args.dec, args.width, args.height)
