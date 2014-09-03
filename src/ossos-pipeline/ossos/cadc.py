#!python
from astropy.io import votable
import warnings
from astropy.table import Table
import requests
from cStringIO import StringIO
from astropy.time import Time


def cfht_megacam_tap_query(RAdeg=180.0, DECdeg=0.0, width=1, height=1, date=None):
    """Do a query of the CADC Megacam table.

    Get all observations inside the box (right now it turns width/height into a radius, should not do this).

    @rtype : Table
    @param RAdeg: center of search region, in degrees
    @param DECdeg: center of search region in degrees
    @param width: width of search region in degrees
    @param height: height of search region in degrees
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

    query = query % (RAdeg, DECdeg, radius)

    if date is not None:
        mjd = Time(date, scale='utc').mjd
        query += " AND Plane.time_bounds_cval1 <= {} AND {} <= Plane.time_bounds_cval2 ".format(mjd+0.5, mjd-0.5)

    data = {"QUERY": query,
            "REQUEST": "doQuery",
            "LANG": "ADQL",
            "FORMAT": "votable"}

    url = "http://www.cadc.hia.nrc.gc.ca/tap/sync"

    warnings.simplefilter('ignore')
    ff = StringIO(requests.get(url, params=data).content)
    ff.seek(0)
    print ff.read()
    table = votable.parse(ff).get_first_table().to_table()
    assert isinstance(table, Table)
    return table

if __name__ == '__main__':

    ra_cen = (37 + 7) / 2.0
    dec_cen = (20 + 0) / 2.0
    width = 20 - 0
    height = 37 - 7
    print cfht_megacam_tap_query(ra_cen, dec_cen, width, height)