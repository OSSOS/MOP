FC = gfortran

testxy:
	$(FC) -o testxy wcs.f test_xy2sky.f
	./testxy
	rm ./testxy

testsky:
	$(FC) -o testsky wcs.f test_sky2xy.f
	./testsky
	rm ./testsky
