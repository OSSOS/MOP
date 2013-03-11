#!/bin/bash


source /home/jkavelaars/.bash_profile
source /home/jkavelaars/.moprc

exitTag() {
  tag=run_mkpsf.sh
  vosURI=${destDir}
  value=$1
  echo "Tagging $vosURI with ${tag} => ${value}"
  vtag --certfile=cadcproxy.pem ${vosURI} ${tag} "${value}"
  exit -1
}



fileId=$1
ccd=$2
ccd=${ccd#0}
extno=$(($ccd+1))

URL=https://www.cadc.hia.nrc.gc.ca/data/pub/CFHT/${fileId}[${extno}]

destDir=`printf "vos:OSSOS/dbimages/%s/ccd%02d" ${fileId} ${ccd}`


echo "Will store results at ${destDir}"

vmkdir -v --certfile=cadcproxy.pem -p ${destDir}  || exitTag "mkdir Fails"

[ `vtag ${destDir} run_mkpsf.sh`0 ==  "success0" ] && exit 0 


filename=`printf "%s%02d" ${fileId} ${ccd}`
tmp_filename=TMP.fits
echo "Pulling ${URL} to file ${tmp_filename} "

curl -f -L -s -k -E cadcproxy.pem -g $URL > ${tmp_filename} || exitTag "get image FAILED"

[ `vls --long --certfile=cadcproxy.pem ${destDir}/${filename}.psf.fits | wc -l ` -ne 0  ] && exitTag "success"

header=${fileId}.head
URL="https://www.cadc.hia.nrc.gc.ca/data/pub/CFHTSG/${header}"
echo "Pulling ${URL} to file ${header} " 
curl -f -L -s -k -E cadcproxy.pem -g $URL > ${header}  || exitTag "get header FAILED"

echo "running swapHead"
swapHead.py ${tmp_filename} ${header}  || exitTag "swapHead FAILED"

if [ ${extno} -lt 19 ] ; then 
   echo "flipping the image"
   imcopy ${tmp_filename}[-*,-*]   ${filename}.fits || exitTag "imcopy FAILED"
else
   echo "mv ${tmp_filename} to ${filename}.fits"
   mv ${tmp_filename} ${filename}.fits || exitTag "mv FAILED"
fi

echo "Running: jmpmakepsf.csh ${filename} no"
jmpmakepsf.csh . ${filename} no || exitTag "jmpmakepsf FAILED"

for ext in "fits" "mopheader" "psf.fits" "zeropoint.used" "apcor" "fwhm" ; do
  vcp --certfile=cadcproxy.pem -v ${filename}.${ext} ${destDir}/${filename}.${ext} || exitTag "vcp Failed"
done

exitTag "success"
