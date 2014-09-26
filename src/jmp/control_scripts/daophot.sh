#!/bin/sh
usage="Usage: daophot.sh -i image -a aperture -c coordinates -z zeropoint"
### Given an image and coordinate file, compute the magnitudes
while [ $# -gt 0 ]; do
  case "$1" in 
  -i)
    shift
    im1=$1
    echo "Working on image $im1"
    shift
    ;;
  -a)
    shift
    aper=$1
    shift
    ;;
  -c)
    shift
    coo=$1
    shift
    ;;
  -z)
    shift 
    zeropoint=$1
    shift
    ;;
  -o)
    shift
    outfile=$1
    shift
    ;;
  *)
    echo  $usage
    exit
  esac
done

if [ "X$coo" == "X" -o "X$aper" == "X" -o "X$im1" == "X" -o "X$zeropoint" == "X" ]; then
    echo $usage
    exit
fi


touch daophot.FAILED

if [ -f $im1.fits ] ; then 
   img=$im1
elif [ -f $im1 ] ; then 
   img=`echo $im1 | sed -e 's/.fits//'`
else
   echo "Can't resolve image name from: $im1"
   exit
fi


maxlin=`gethead $img.fits MAXLIN`
if [ "X$maxlin" == "X" ]; then
   maxlin=35000
fi

echo "Staring IRAF"
curdir=`pwd`
cd ~/iraf
cl <<EOF
  real annulus
  real dannulus
  annulus=5*$aper+1 
  dannulus=$aper
  digiphot
  daophot
  cd $curdir
  s1=mktemp("$img")
  phot("$img","$coo",s1,apertures=$aper,zmag=$zeropoint,datapars.exposur="EXPTIME",datapars.datamin=0,datapars.datamax=$maxlin,fitskypars.annulus=annulus,fitskypars.dannulus=dannulus,datapars.gain="GAIN",datapars.obstime="UTC-OBS",centerpars.calgori="centroid",verify-,verbose-)
  txdump(s1,"XCEN,YCEN,MAG,MERR,ID","(SIER==0)&&(CIER==0)&&(PIER==0)&&(MAG!=INDEF)&&(MERR<0.1)",>" $outfile") 
  delete(s1,verify-,go_ahead+)

  logout
EOF
cd $curdir
touch daophot.OK
rm daophot.FAILED

exit
