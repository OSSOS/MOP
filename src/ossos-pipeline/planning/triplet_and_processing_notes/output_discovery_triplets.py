__author__ = 'Michele Bannister   git:@mtbannister'

import sqlalchemy as sa
# import web.field_obs.queries
import sys


class OssuaryTable(object):
    def __init__(self, tablename):
        # reflect_table_from_ossuary
        # Development testing database on local machine provided by Postgres.App
        engine = sa.create_engine('postgresql://localhost/ossuary', echo=False)
        metadata = sa.MetaData(bind=engine)
        table = sa.Table(tablename, metadata, autoload=True, autoload_with=engine)  # reflect existing table
        conn = engine.connect()

        self.tablename = tablename
        self.table = table
        self.conn = conn


class ImagesQuery(object):
    def __init__(self):
        """
        An ImagesQuery allows queries to ossuary's images table, and marshalls vtags associated with each image.
        """
        ot = OssuaryTable('images')
        self.images = ot.table
        self.conn = ot.conn


ims = ImagesQuery()

field = sys.argv[1]  # format as e.g. AE, AO, BL

outfile = 'planning/14{}_triplets_details.txt'.format(field)

with open('planning/triplet_and_processing_notes/14{}_discovery_expnums.txt'.format(field), 'r') as infile:
    it = ims.images

    with open(outfile, 'w') as ofile:
        ofile.write(
            'Expnum RA DEC MJD_middle Exptime_sec plant_xmin_px plant_xmax_px plant_ymin_px plant_ymax_px '
            'search_xmin_px search_xmax_px search_ymin_px search_ymax_px\n'.format())

    for triplet in infile.readlines():
        # this should have two versions, a very precise one for use with the updated headers
        # and a straightforward version for use with the initial stuff that's in the database,
        # which allows use without VOSpace.
        with open(outfile, 'a') as ofile:  # blank line between triplets
            ofile.write('{}'.format(triplet.split(' ')[3]))

        for expnum in triplet.split(' ')[0:3]:
            ss = sa.select([it.c.image_id, it.c.crval_ra, it.c.crval_dec, it.c.mjd_start, it.c.mjd_end, it.c.exptime],
                           order_by=it.c.image_id)
            ss.append_whereclause(ims.images.c.image_id == expnum)
            query = ims.conn.execute(ss)
            init_retval = [s for s in query][0]
            retval = list(init_retval[0:3])
            # JM wants MJD_middle only: calculate it from the midpoint between the start and end
            retval.append((init_retval[3] + init_retval[4]) / 2.)
            # add the exptime back
            retval.append(init_retval[5].total_seconds())
            # now the area in which we planted TNOs for characterisation (hardwired in plant.csh's calls to kbo_gen)
            # chosen to match that in header keyword DATASEC = [33:2080,1:4612] / Imaging area of the entire CCD in
            # raw frame
            if field == 'E' or 'O':
                xmin = 66  # ya, we don't know why it got hardwired this way initially. 1.6% underplanting.
            else:  # spotted this by the time H and L etc were done. This was fixed for H/L to use DATASEC directly.
                xmin = 33
            retval.append(xmin)
            # xmax, ymin and ymax were all correctly taken from DATASEC
            retval.append(2080)
            retval.append(1)
            retval.append(4612)
            # the search area for E and O is/was [33:2080;1:4612], correctly, and same for H/L.
            retval.append(33)
            retval.append(2080)
            retval.append(1)
            retval.append(4612)

            with open(outfile, 'a') as ofile:
                # expnum, ra, dec, obs_end, mjd_middle, exptime (seconds)
                ofile.write('{} {} {} {} {} {} {} {} {} {} {} {} {}\n'.format(*retval))

        with open(outfile, 'a') as ofile:  # blank line between triplets
            ofile.write('\n')

            # copy up the file to VOSpace
