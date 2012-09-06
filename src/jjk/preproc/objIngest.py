#!/usr/cadc/misc/bin/python
"""Script to take an .obj file [MOP object file] and ingest into the MOP object database"""

from myTaskError import TaskError


def objIngest(obj_file):
    import sys,os,math,re
    
    import pyfits
    
    import MOPfiles

    obj=MOPfiles.read(obj_file)

    """
    The SQL description of the source table
    +-------------+---------+------+-----+---------+----------------+
    | Field       | Type    | Null | Key | Default | Extra          |
    +-------------+---------+------+-----+---------+----------------+
    | sourceID    | int(11) |      | PRI | NULL    | auto_increment |
    | x_pix       | float   | YES  | MUL | NULL    |                |
    | y_pix       | float   | YES  |     | NULL    |                |
    | iso_flux    | float   | YES  |     | NULL    |                |
    | iso_err     | float   | YES  |     | NULL    |                |
    | aper_flux   | float   | YES  |     | NULL    |                |
    | aper_err    | float   | YES  |     | NULL    |                |
    | iso_area    | float   | YES  |     | NULL    |                |
    | kron_radius | float   | YES  | MUL | NULL    |                |
    | elongation  | float   | YES  |     | NULL    |                |
    | cxx         | float   | YES  |     | NULL    |                |
    | cyy         | float   | YES  |     | NULL    |                |
    | cxy         | float   | YES  |     | NULL    |                |
    | max_flux    | float   | YES  |     | NULL    |                |
    | max_int     | float   | YES  |     | NULL    |                |
    | mag_dao     | float   | YES  | MUL | NULL    |                |
    | merr_dao    | float   | YES  |     | NULL    |                |
    | sky_cts     | float   | YES  |     | NULL    |                |
    | chi2        | float   | YES  |     | NULL    |                |
    | npix        | float   | YES  |     | NULL    |                |
    | sharp       | float   | YES  |     | NULL    |                |
    | ra_deg      | float   | YES  | MUL | NULL    |                |
    | dec_deg     | float   | YES  |     | NULL    |                |
    +-------------+---------+------+-----+---------+----------------+
    """


    """
    Columns in the SOURCE table...
    ##           X          Y   FLUX_ISO FLUXERR_ISO  FLUX_APER FLUXERR_APER ISOAREA_IMAGE KRON_RADIUS ELONGATION  CXX_IMAGE  CYY_IMAGE  CXY_IMAGE   FLUX_MAX         ID    MAX_INT       FLUX       MERR        SKY       ELON        X^2      N_PIX        MAG      SHARP       SIZE

    """

    ### The mapping 
    obj['hdu2sql']={'MAX_INT': 'peak',
                    'FLUX': 'flux',
		    'MAG': 'mag',
		    'MERR': 'merr',
                    'SKY': 'sky',
                    'ELON': 'elongation',
                    'X^2': 'chi2',
                    'N_PIX': 'npix',
                    'SHARP': 'sharpness',
                    'Y': 'yPix',
                    'X': 'xPix',
                    'SIZE': 'size',
                    'RA': 'raDeg',
                    'DEC': 'decDeg',
                    }


    MOPfiles.store(obj)

    return


if __name__ == '__main__':
    
    from optparse import OptionParser
    
    parser=OptionParser()
    parser.add_option("--verbose","-v",action="store_true",help="Tell you what I'm doing.")
    (opt,obj_files)=parser.parse_args()

    for obj_file in obj_files:
        if opt.verbose:
            sys.stdout.write("Ingesting %s\n" % ( obj_file))
        objIngest(obj_file)
        
    

