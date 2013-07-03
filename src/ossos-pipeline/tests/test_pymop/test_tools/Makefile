FC = gfortran
PI = perl

testxy:
	$(FC) -o testxy wcs.f test_xy2sky.f
	./testxy
	rm ./testxy

testsky:
	$(FC) -o testsky wcs.f test_sky2xy.f
	./testsky
	rm ./testsky

testphot:
	$(PI) dophot.pl --file ../../data/1616681p22.fits --xcen 560.06 \
	--ycen 406.51 --maxcount 30000 --exptime 1.0 --ap 4 --insky 11 \
	--outsky 15 --zeropoint 32.026 | grep DOPHOT
	rm ./dophot.pl.FAILED
	rm ./dophot.pl.OK