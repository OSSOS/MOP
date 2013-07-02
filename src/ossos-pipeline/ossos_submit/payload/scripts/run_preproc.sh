#!/bin/sh

source /home/jkavelaars/.bash_profile
source /home/jkavelaars/.moprc

ls

cp cadcproxy.pem ~/.ssl/

expnum=$1

vls vos:OSSOS/dbimages/${expnum}/${expnum}p.fits >& /dev/null && exit 0

${HOME}/preproc.py --overscan --flat 13AQ05_r_flat.fits --short --verbose $expnum 
vcp -v ${expnum}p.fits vos:OSSOS/dbimages/${expnum}/
