      PROGRAM test_xy2sky
* Run subroutines to generate test data for Python code
* Test data acquired from:
* http://www.cadc-ccda.hia-iha.nrc-cnrc.gc.ca/data/pub/CFHTSG/
* 821543p.head
* Compile with "gfortran -o test_xy2sky wcs.f test_xy2sky.f"
* or use the Makefile's testxy target
      DOUBLE PRECISION x, y, crpix1, crpix2, crval1, crval2, cd(2,2),
     & pv(2,0:10), ra, dec
      INTEGER nord

      x = 15000D0
      y = 20000D0
      crpix1 = -7535.57493517D0
      crpix2 = 9808.40914361D0
      crval1 = 176.486157083D0
      crval2 = 8.03697351091D0

* Note Fortran uses column-major ordering
      cd = reshape((/ 5.115244026718D-05, -1.280229655229D-07,
     & 7.064503033578D-07, -5.123112374523D-05 /), shape(cd))

      pv = reshape((/ -7.030338745606D-03, -6.146513090656D-03,
     & 1.01755337222D0,  1.01552885426D0, 8.262429361142D-03,
     & 8.259666421752D-03, 0.00000000000D0, 0.00000000000D0,
     & -5.910145454849D-04, -4.567030382243D-04, -7.494178330178D-04,
     & -6.978676921999D-04, -3.470178516657D-04, -3.732572951216D-04,
     & -2.331150605755D-02, -2.332572754467D-02, -8.187062772669D-06,
     & -2.354317291723D-05, -2.325429510806D-02, -2.329623852891D-02,
     & 1.135299506292D-04, 1.196394469003D-04/), shape(pv))

      nord = 3

      PRINT *, 'Running xy2sky with the following parameters:'
      PRINT *, 'x = ', x
      PRINT *, 'y = ', y
      PRINT *, 'crpix1 = ', crpix1
      PRINT *, 'crpix2 = ', crpix2
      PRINT *, 'crval1 = ', crval1
      PRINT *, 'crval2 = ', crval2
      PRINT *, 'cd11 = ', cd(1, 1)
      PRINT *, 'cd12 = ', cd(1, 2)
      PRINT *, 'cd21 = ', cd(2, 1)
      PRINT *, 'cd22 = ', cd(2, 2)
      PRINT *, 'pv1_0 = ', pv(1, 0)
      PRINT *, 'pv1_1 = ', pv(1, 1)
      PRINT *, 'pv1_2 = ', pv(1, 2)
      PRINT *, 'pv1_3 = ', pv(1, 3)
      PRINT *, 'pv1_4 = ', pv(1, 4)
      PRINT *, 'pv1_5 = ', pv(1, 5)
      PRINT *, 'pv1_6 = ', pv(1, 6)
      PRINT *, 'pv1_7 = ', pv(1, 7)
      PRINT *, 'pv1_8 = ', pv(1, 8)
      PRINT *, 'pv1_9 = ', pv(1, 9)
      PRINT *, 'pv1_10 = ', pv(1, 10)
      PRINT *, 'pv2_0 = ', pv(2, 0)
      PRINT *, 'pv2_1 = ', pv(2, 1)
      PRINT *, 'pv2_2 = ', pv(2, 2)
      print *, 'pv2_3 = ', pv(2, 3)
      print *, 'pv2_4 = ', pv(2, 4)
      print *, 'pv2_5 = ', pv(2, 5)
      print *, 'pv2_6 = ', pv(2, 6)
      print *, 'pv2_7 = ', pv(2, 7)
      print *, 'pv2_8 = ', pv(2, 8)
      print *, 'pv2_9 = ', pv(2, 9)
      print *, 'pv2_10 = ', pv(2, 10)

      PRINT *, 'nord = ', nord

      CALL xy2sky(crval1, crval2, crpix1, crpix2, cd, pv, nord, x, y,
     & ra, dec)

      PRINT *, 'ra = ', ra
      PRINT *, 'dec = ', dec

      END PROGRAM test_xy2sky
