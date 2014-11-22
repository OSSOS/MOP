#!/bin/bash

fwhm="4.0"
thresh="4.0"
maxlin="30000"
aper="10.0"
annulus="30.0"
dannulus="12.0"
zp="26.0"

# Go into the "raw" file directory. For the first image of the triplet:

im1="831019o_26"

AjustMag() {
  im1=$1
  im2=`echo $im1 | sed "s/o_/p_/"`
  im=`echo $im1 | sed "s/o_/_/"`

  zerop1=`cat zeropoint.used`
  zerop2=`gethead $im2.fits PHOT_C`
  if [ "X$zerop2" == "X" ]; then
    set zerop2="26.0"
  fi

if [ $im1 != $im2 ]; then

  stepZjmp -f $im1
  step0jmp -f $im1 -w $fwhm -t $thresh -m $maxlin

  \rm $im1.phot $im2.phot
  \rm $im1.mag $im2.mag
  curdir=`pwd`
  cd ~/iraf
  cl <<EOF

  digiphot
  daophot
  cd $curdir
  phot $im1 $im1.bright.psf $im1.phot apertures=$aper zmag=$zp datapars.datamin=0 datapars.datamax=$maxlin datapars.fwhmpsf=$fwhm fitskypars.annulus=$annulus fitskypars.dannulus=$dannulus centerpars.calgori="centroid"

  txdump $im1.phot "XCEN,YCEN,MAG,MERR" "(SIER==0)&&(CIER==0)&&(PIER==0)" > $im1.mag

  phot $im2 $im1.mag $im2.phot apertures=$aper zmag=$zp datapars.datamin=0 datapars.datamax=$maxlin datapars.fwhmpsf=$fwhm fitskypars.annulus=$annulus fitskypars.dannulus=$dannulus centerpars.calgori="centroid"

  txdump $im2.phot "XCEN,YCEN,MAG,MERR" "(SIER==0)&&(CIER==0)&&(PIER==0)" > $im2.mag

  logout
EOF
  cd $curdir


# M'_1 = $zerop2 - $zerop1 + <m_2 - m_1> + M_1
# so the new zeropoint would be
# Z_p' = $zerop2 + <m_2 - m_1>

  dmag-rp.pl --zp1 $zerop1 --zp2 $zerop2 --file1 $im1.mag --file2 $im2.mag --im $im1 > $im.dmag

else

  dzp=`echo "$zerop1 $zerop2" | awk '{ print $2-$1 }' -`
  echo "0.0 $zerop1 $zerop2 $dzp $zerop2" > $im.dmag

fi

  awk '{ print $5 }' $im.dmag > zeropoint.corrected
  dm=`awk '{ if (($1 > 0.5) || ($1 < -0.5)) {print 1} else {print 0} }' $im.dmag`
  echo $dm
  if [ $dm == "1" ]; then
    awk '{ print $1 }' $im.dmag > WARNING
  fi
}

initdir=`pwd`
basedir=`pwd`
tailbin=`which tail`
ID=`date +"%H%M%S"`
ln -s ${tailbin} ajust-mag.tail.${ID}

#pwd

ajust-mag.tail.${ID} -n +1 -f ${basedir}/dir-list | (
  while read wd; do
    cd ${basedir}
#    pwd
    if [ X"$wd" = XDONE ]; then
      echo "Don't forget to run CorrectMag.sh !"
      pid=`/sbin/pidof -o $$ -o $PPID -o %PPID -x ajust-mag.tail.${ID}`
      if [ "$pid" != "" ] ; then
        \rm -f ajust-mag.tail.${ID}
	kill $pid
	exit
      fi
    fi
    cd $wd
#    pwd
    for i in ??????o_??.fits; do
      if [ -e $i ]; then
        filename=`echo $i | sed "s/.fits//"`
        echo "AjustMag $filename"
        AjustMag $filename
      else
	for j in ??????p_??.fits; do
          if [ -e $j ]; then
            filename=`echo $j | sed "s/.fits//"`
            echo "AjustMag $filename"
            AjustMag $filename
	  fi
	done
      fi
    done
    cd $basedir
  done
)

cd ${initdir}

exit
