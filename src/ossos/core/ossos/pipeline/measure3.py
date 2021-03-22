#!/usr/bin/env python
# use the xy2skypv code to generate the astrometric values
# that measure3 would normally produce.
import os
import argparse
import logging
from pathlib import Path
from ossos import storage


SUCCESS_FILE = "measure3.OK"
FAILED_EXT = "measure3.FAILED"
CANDS_COMB_EXT = 'cands.comb'
CANDS_ASTROM_EXT = 'measure3.cands.astrom'


def main():
    parser = argparse.ArgumentParser(
        description="Run xy2skypv on cands.comb files to produce cands.astrom")
    parser.add_argument('base_image',
                        help="The base image referencing the .cands.comb file")
    parser.add_argument('--dbimages',
                        help="DBImage VOSpace URI",
                        default=storage.DBIMAGES)

    args = parser.parse_args()
    base_image = args.base_image
    storage.DBIMAGES = args.dbimages
    run(base_image, dbimages=storage.DBIMAGES)


def run(base_image, dbimages=None):
    """
    execute sky2xypv on sources found in cands.comb file.
    """

    if dbimages is not None:
        storage.DBIMAGES = dbimages

    Path(f'{base_image}.{FAILED_EXT}').touch()

    cands_filename = "%s.%s" % (base_image, CANDS_COMB_EXT)
    if not os.access(cands_filename, os.R_OK):
        FileNotFoundError("Failed to open input candidate file %s\n" % cands_filename)

    astrom_header = """##   X        Y        X_0     Y_0          R.A.          DEC                   \n"""

    astrom_filename = "%s.%s" % (base_image, CANDS_ASTROM_EXT)
    coords = []
    base_names = []
    line_counter = 0
    xy_files = {}
    xy_lines = {}
    get_image_names = True
    started = False
    with open(astrom_filename, 'w') as astrom_file:
        cands_lines = open(cands_filename).readlines()
        for cands_line in cands_lines:
            # read the names of the exposures to work from...
            if len(cands_line.strip()) == 0:
                # Skip EMPTY lines...
                continue
            if cands_line.lstrip()[0] == '#':
                # write out all the header lines except the one with column
                # names as its different for the astrom version 'X_0' is a
                # column name in the cands.comb files.
                if "X_0" not in cands_line:
                    astrom_file.write(cands_line)
                else:
                    astrom_file.write(astrom_header)
                if cands_line.lstrip()[1] == '#':
                    get_image_names = not started
                    continue
                logging.info("Read this line: {}".format(cands_line[2:].strip()))
                if get_image_names and cands_line.lstrip()[1] == ' ':
                    base_name = cands_line[2:].strip()
                    base_names.append(base_name)
                    xy_files[base_name] = open("%s.xy" % base_name, 'w')
                    started = True
                    continue
                continue
            v = cands_line.strip().split()
            coords.append(v)
            base_name = base_names[line_counter % len(base_names)]
            if base_name not in xy_lines:
                xy_lines[base_name] = []
            xy_lines[base_name].append(cands_line)
            xy_files[base_name].write("%s %s\n" % (v[0], v[1]))
            xy_files[base_name].flush()
            line_counter += 1

        # Now run the astrometry for each object on the frame that
        # object was detected on

        astrometry_lines = {}
        for base_name in xy_files:
            storage.get_frame(base_name)
            xy_files[base_name].close()
            cmd = 'xy2skypv %s.fits %s.xy %s.rd' % (base_name,
                                                    base_name, base_name)
            os.system(cmd)
            astrometry_lines[base_name] = open("%s.rd" % base_name).readlines()

        for idx in range(len(astrometry_lines[base_names[0]])):
            astrom_file.write("\n")
            for base_name in base_names:
                rd = astrometry_lines[base_name][idx].split()
                xy = xy_lines[base_name][idx].split()
                if rd[2] != xy[0] or rd[3] != xy[1]:
                    # the files are misalligned?
                    raise ValueError("MISMATCH in astrometric code... bailing out.\n")
                astrom_file.write(" %8.2f %8.2f %8.2f %8.2f %12.7f %12.7f\n" % (float(xy[0]),
                                                                                float(xy[1]),
                                                                                float(xy[2]),
                                                                                float(xy[3]),
                                                                                float(rd[0]),
                                                                                float(rd[1])))

    Path(f'{SUCCESS_FILE}').touch()
    os.unlink(f'{base_image}.{FAILED_EXT}')


if __name__ == '__main__':
    main()
