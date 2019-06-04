import argparse

from ossos import storage
from ossos.gui import context
from ossos.gui.progress import DONE_PROPERTY, LOCK_PROPERTY


def scan_for_miscreant_files(directory, quarantine_directory, file_of_miscreants):
    reals_fks = []
    con = context.get_context(directory)
    listing = con.listdir()

    with open(file_of_miscreants, 'r') as infile:
        for line in infile.readlines():
            if line.split()[5] != '0':
                reals_fks.append(line.split()[3] + '_p' + line.split()[4])
            if line.split()[6] != '0':
                reals_fks.append('fk_' + line.split()[3] + '_s' + line.split()[4])

    for fileID in reals_fks:
        # remove any .measure3.reals.astrom if present
        reals_file = fileID + '.measure3.reals.astrom'
        try:
            if con.exists(reals_file):  # this isn't catching the error message, no obvious reason why
                # anything interesting about this file?
                print 'Removing:', reals_file, con.get_file_size(reals_file), \
                    'done: %s' % storage.get_property(con.get_full_path(reals_file), DONE_PROPERTY), \
                    'locked: %s' % storage.get_property(con.get_full_path(reals_file), LOCK_PROPERTY)
                # con.remove(reals_file)
        except:
            continue

        # obtain all files remaining in the directory listing that contain this chip, just not the removed reals.astrom
        fileID_files = [n for n in listing if
                        n.split('.')[0] == fileID and not n.__contains__('.measure3.reals.astrom')]
        print fileID
        print fileID_files
        # move all remaining matching files to quarantine
        for fn in fileID_files:
            print directory + fn, '>', quarantine_directory + fn
            con.rename(directory + fn, quarantine_directory + fn)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("-d",
                        default="vos:OSSOS/measure3/2013B-L/",
                        help="The directory to be examined for miscreant files.")
    parser.add_argument("-o",
                        default="vos:OSSOS/measure3/2013B-L/quarantine/",
                        help="The directory that miscreant files are to be moved to.")
    parser.add_argument("-m",
                        default="/Users/michele/Desktop/trans_failed.txt",
                        help="File containing listing of miscreants (trans fails, etc).")

    args = parser.parse_args()

    scan_for_miscreant_files(args.d, args.o, args.m)


if __name__ == "__main__":
    main()
