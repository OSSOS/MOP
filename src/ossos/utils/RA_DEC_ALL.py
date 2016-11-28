
from ossos import mop_file, util, storage
import os

from astropy.table import Column, Table
from astropy.io import fits
import numpy
from vos import Client
import sys

client=Client()

RA = 'RA_J2000'
DEC = 'DE_J2000'
del_keyword_list = []
pathname = os.path.dirname(os.path.realpath(__file__))
for line in file(pathname+'/junk_key.txt').readlines():
    del_keyword_list.append(line.strip())

lines = file(sys.argv[1]).readlines()

for line in lines:
    expnum = int(line.strip())
    if expnum < 1785619:
        # Last exposures with 36 CCD Megaprime
        ccdlist = range(0,36)
    else:
        # First exposrues with 40 CCD Megaprime
        ccdlist = range(0, 40)
    for ccd in ccdlist:
        try:
            header = storage.get_astheader(expnum, ccd)
            datasec = storage.datasec_to_list(header.get('DATASEC', '[80:2080,30,4160]'))
        except Exception as ex:
            print ex
            continue
        try:
            fwhm = "{:5.2f}".format(storage.get_fwhm(expnum, ccd))
        except:
            fwhm = 'unknown'
        for keyword in del_keyword_list:
            try:
                del(header[keyword])
            except:
                pass
        header['FWHM'] = (fwhm, 'FWHM in pixels')
        header['EXTNAME'] = 'header'
        primary_hdu = fits.PrimaryHDU(header=header)
        hdu_list = fits.HDUList([primary_hdu,])
        for ext in ['jmp', 'matt']:
            extension = 'obj.'+ext
            name = "{}p{:02d}.{}".format(expnum, ccd, extension)
            try:
                os.unlink(name)
                os.unlink(name+".fits")
            except:
                pass
            print "Doing {}".format(name)
            try:
                obj_file = mop_file.Parser(expnum, ccd, extension)
                obj_file.parse()
            except Exception as ex:
                print ex
                continue
            t = numpy.all([datasec[0] < obj_file.data['X'], obj_file.data['X'] < datasec[1], 
                           datasec[2] < obj_file.data['Y'], obj_file.data['Y'] < datasec[3]], axis=0)
            print "Source remaining after datasec cut: {} of {}".format(len(obj_file.data[t]['X']), len(t))
            table_hdu = fits.table_to_hdu(obj_file.data[t])
            table_hdu.header['CATALOG'] = name
            table_hdu.header['EXTNAME'] = ext
            hdu_list.append(table_hdu)
            del(table_hdu)
            del(obj_file)
            os.unlink(name)

        name = "{}p{:02d}.{}".format(expnum, ccd, 'obj.fits')
        if os.access(name, os.F_OK):
            os.unlink(name)
        hdu_list.writeto(name)
        uri = storage.dbimages_uri(expnum, ccd, 'p', ext=".obj.fits")
        print name+" -> "+uri
        count = 0
        while count < 10:
            try:
                storage.copy(name, uri)
                os.unlink(name)
                break
            except Exception as ex:
                print ex
                count += 1
                continue
          
sys.exit(0)

