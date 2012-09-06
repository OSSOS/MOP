#! /bin/sh

#for b in 05[AB]Q*L5?; do
#for b in L7a; do
for b in L5[rs]; do
    cd $b/chip00
    for f in *; do
	cd $f
	mid=`ls *_00.fits | head -1 | sed "s/_00.fits//" | sed "s/o/p/"`
	~/Petit/wget-cadc-file $mid
	mv $mid.fits.fz ../../
	cd ../
    done
    cd ../
    for i in *.fits.fz; do
	j=`echo $i | sed "s/.fz//"`
	imcopy $i $j
	\rm -f $i
    done
    ls *.fits | cat > file-list
    echo "DONE" >> file-list
    ProcessFiles.sh > Processfiles.log 2>&1 &
    cd ../
done

exit
