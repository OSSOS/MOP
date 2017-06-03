#!python
from multiprocessing import Pool
from ossos import mop_file, storage
import os
from astropy.io import fits
import numpy
from vos import Client
import sys

client = Client()

RA = 'RA_J2000'
DEC = 'DE_J2000'
del_keyword_list = []
pathname = os.path.dirname(os.path.realpath(__file__))
for line in open(pathname+'/junk_key.txt').readlines():
    del_keyword_list.append(line.strip())

def build_catalog(line):
    values = line.strip().split()
    expnum = int(values[0])
    ccd = int(values[1])
    if storage.get_status("slow", "", expnum, "p", ccd):
        print "Already did {} {}".format(expnum, ccd)
        return 
    if 1 == 1:
        try:
            try:
                header = storage.get_astheader(expnum, ccd)
                datasec = storage.datasec_to_list(header.get('DATASEC', '[80:2080,30,4160]'))
            except Exception as ex:
                print(ex)
                storage.set_status('slow', '', expnum, 'p', ccd, str(ex))
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
            hdu_list = fits.HDUList([primary_hdu, ])
            for ext in ['jmp', 'matt']:
                extension = 'obj.'+ext
                name = "{}p{:02d}.{}".format(expnum, ccd, extension)
                try:
                    os.unlink(name)
                    os.unlink(name+".fits")
                except:
                    pass
                print("Doing {}".format(name))
                try:
                    obj_file = mop_file.Parser(expnum, ccd, extension)
                    obj_file.parse()
                except Exception as ex:
                    print(ex)
                    continue
                t = numpy.all([datasec[0] < obj_file.data['X'], obj_file.data['X'] < datasec[1],
                               datasec[2] < obj_file.data['Y'], obj_file.data['Y'] < datasec[3]], axis=0)
                print("Source remaining after datasec cut: {} of {}".format(len(obj_file.data[t]['X']), len(t)))
                table_hdu = fits.table_to_hdu(obj_file.data[t])
                table_hdu.header['CATALOG'] = name
                table_hdu.header['EXTNAME'] = ext
                hdu_list.append(table_hdu)
                del table_hdu
                del obj_file
                os.unlink(name)

            name = "{}p{:02d}.{}".format(expnum, ccd, 'obj.fits')
            if os.access(name, os.F_OK):
                os.unlink(name)
            hdu_list.writeto(name)
            uri = storage.dbimages_uri(expnum, ccd, 'p', ext=".obj.fits")
            print(name+" -> "+uri)
            count = 0
            while count < 10:
                print("Copy attempt {}".format(count))
                try:
                    storage.copy(name, uri)
                    storage.set_status('slow', '', expnum, 'p', ccd, storage.SUCCESS)
                    os.unlink(name)
                    break
                except Exception as ex:
                    storage.set_status('slow', '', expnum, 'p', ccd, str(ex))
                    print(ex)
                    count += 1
                    continue
        except Exception as ex:
            print(ex)


if __name__ == '__main__':
    p = Pool(25)
    lines = open(sys.argv[1],'r').readlines()
    p.map(build_catalog, lines)
