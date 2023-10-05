__author__ = 'Michele Bannister   git:@mtbannister'

import sys
from ossos import storage

field = sys.argv[1]  # format as e.g. AE, AO, BL

outfile = 'planning/15B{}_triplets_details.txt'.format(field)

with open('planning/triplet_and_processing_notes/{}_15B_discovery_expnums.txt'.format(field), 'r') as infile:
    # it = ims.images

    with open(outfile, 'w') as ofile:
        ofile.write(
            'Expnum RA DEC MJD_middle Exptime_sec plant_xmin_px plant_xmax_px plant_ymin_px plant_ymax_px '
            'search_xmin_px search_xmax_px search_ymin_px search_ymax_px\n'.format())

    for triplet in infile.readlines():
        with open(outfile, 'a') as ofile:  # blank line between triplets
            ofile.write('{}'.format(triplet.split()[3]))

        for expnum in triplet.split()[0:3]:
            # expnum, ra, dec, obs_end, mjd_middle, exptime (seconds)
            header = storage.get_header(storage.get_uri(expnum))
            # print header
            # JM wants MJD_middle only: calculate it from the midpoint between the start and end
            mjd_mid = header['MJDATE'] + (header['MJDEND'] - header['MJDATE']) / 2.
            retval = [expnum, header['CRVAL1'], header['CRVAL2'], mjd_mid, header['EXPTIME']]

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
