C -*-compile-command: "make_libpsf"; -*-
c
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
C This subroutine derives the concentric aperture photometry. Derived
c from the official DAO version to communicate with the external world
c through arguments instead of files.
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

      SUBROUTINE  PHOT (F, NCOL, NROW, PHPADU, LOBAD, HIBAD,
     $  aper, n_ap, is, os, id, xcen, ycen, apmageout, magerrout,
     $  s_sky, vs, sk, ntot)
C
C=======================================================================
C
C This subroutine derives the concentric aperture photometry.  At 
C present, this is the only place in all of DAOPHOT where sky values 
C are derived for the individual stars.
C
C               OFFICIAL DAO VERSION:  1991 April 18
C
C Argument
C
C=======================================================================
C
      IMPLICIT NONE
      INTEGER MINSKY, MAXSKY, MAXAP, NCOL, NROW
      PARAMETER  (MINSKY=20, MAXAP=12, MAXSKY=10000)
C
C Parameters:
C
C MINSKY is the smallest number of pixels from which the sky may be
C        determined.  If for some star the number of sky pixels
C        is less than MINSKY, an error code will result and 
C        control will return to the main program.
C
C MAXSKY the maximum number of pixels allowed in the sky annulus.
C        This and the user's requested inner sky radius will later
C        determine the maximum permitted outer sky radius.
C
C MAXAP  the maximum number of star apertures allowed.
C
      DOUBLE PRECISION DLOG10, DBLE
      DOUBLE PRECISION APMAG(MAXAP), AREA(MAXAP)

      REAL
     $  SKY(MAXSKY), MAGERR(MAXAP), F(NCOL,NROW), ERROR(3), aper(*),
     $  magerrout(MAXAP,*), PAR(MAXAP+2), PMIN(MAXAP+2),
     $  is, os, xcen(*), ycen(*), apmageout(MAXAP,*), s_sky(*), vs(*),
     $  sk(*), LOBAD, SKYMOD, SKYSIG, SKYSKW, SIGSQ, SKYVAR, DATUM, R,
     $  RSQ, FRACTN, EDGE, HIBAD, DUM, PHPADU, XC, YC,
     $  APMXSQ, RINSQ, ROUT, ROUTSQ, DYSQ

      REAL AMIN1, AMAX1, SQRT

      INTEGER
     $  id(*), i_s, ntot, n_ap, i_ap, I, J, K, L, NAPER, LX,
     $  LY, MX, MY, NSKY

      INTEGER MIN0, MAX0
C
      DATA PAR /MAXAP*0., 2*0./, PMIN /1.E-30, MAXAP*0., 1./
C
C-----------------------------------------------------------------------
C
C SECTION 1
C
C Ascertain the name of the aperture photometry parameter table, and
C read it in.  Then set up all necessary variables for the forthcoming
C reductions. Finally, identify and open the input and output files.
C
      L = MAXAP+1
      PMIN(L) = 1.

      i_ap = n_ap
      if (i_ap .gt. maxap) j = maxap
      do i = 1, i_ap
         par(i) = aper(i)
      end do
      par(13) = is
      par(14) = os
 1000 continue
C
C Count up the number of apertures that will be used.  The first zero or
C negative number encountered terminates the list.
C
      NAPER=MAXAP
      APMXSQ=-1.
      DO 1010 I=1,MAXAP
      IF (PAR(I) .LE. 0.0) GO TO 1020
 1010 APMXSQ=AMAX1(APMXSQ, (PAR(I)+0.5)**2)
      GO TO 1030
C
 1020 NAPER=I-1
 1030 CONTINUE
C
C NAPER   is the number of apertures, whose radii are stored in 
C         elements 1 through NAPER of the array PAR.  
C
C APMXSQ  is the outermost edge of the largest aperture-- if the 
C         distance squared of the center of a pixel from the centroid of
C         the star is greater than APMXSQ, then we know that no part 
C         of the pixel is to be included in any aperture.
C
C Now define the other variables whose values are in the table.
C
      RINSQ=AMAX1(PAR(MAXAP+1), 0.)**2      ! Inner sky radius squared 
      ROUTSQ = REAL(MAXSKY)/3.142 + RINSQ
      DUM = PAR(MAXAP+2)**2
      IF (DUM .GT. ROUTSQ) THEN
         write (6, '(a)')
     $     '   *** You have specified too big a sky annulus. ***'
         WRITE (6,6) SQRT(ROUTSQ)
    6    FORMAT (F10.2, ' pixels is the largest outer sky radius ',
     .        'currently permitted.')
         stop
      ELSE IF (DUM .LE. RINSQ) THEN
         write (6, *)
     $     'Your outer sky radius is no bigger than the inner radius.'
         WRITE (6,8)
    8    FORMAT ('Please try again.')
         stop
      ELSE
         ROUT = PAR(MAXAP+2)
         ROUTSQ = DUM
      END IF
C
C-----------------------------------------------------------------------
C
C SECTION 2
C
C Derive aperture photometry object by object.
C
      i_s = 0
 2000 continue
      i_s = i_s + 1
      if (i_s .gt. ntot) goto 9000
C
C Get the coordinates of next object to be measured.
C
      xc = xcen(i_s)
      yc = ycen(i_s)
C
C Compute the limits of the submatrix.
C
      LX = MAX0(1, INT(XC-ROUT)+1)
      MX = MIN0(NCOL, INT(XC+ROUT))
      LY = MAX0(1, INT(YC-ROUT)+1)
      MY = MIN0(NROW, INT(YC+ROUT))
      EDGE=AMIN1(XC-0.5, (NCOL+0.5)-XC, YC-0.5, (NROW+0.5)-YC)
C
C EDGE is the distance of the star's centroid from the outermost
C extremum of the array.
C
C Initialize star counts and aperture area.
C
      DO 2010 I=1,NAPER
      APMAG(I) = 0.D0
C 
C If this star aperture extends outside the array, the magnitude
C in this aperture will be no good.
C
      IF (EDGE .LT. PAR(I)) APMAG(I)=-1.1D38            ! Null magnitude
 2010 AREA(I)=0.0D0
C
C Now read through the submatrix, picking out the data we want.
C
      NSKY=0
C
      DO 2130 J=LY,MY
      DYSQ=(J-YC)**2
C
         DO 2125 I=LX,MX
            RSQ=DYSQ+(I-XC)**2
            DATUM=F(I,J)
C
C Is this pixel within the sky annulus?
C
            IF ((RSQ .LT. RINSQ) .OR. (RSQ .GT. ROUTSQ) .OR. 
     .           (NSKY .GT. MAXSKY) .OR. (DATUM .LT. LOBAD) .OR. 
     .           (DATUM .GT. HIBAD)) then
               GO TO 2110
            end if
            NSKY=NSKY+1
            SKY(NSKY)=DATUM
C
C The inclusion of partial pixels inside the aperture is done as 
C follows:  if the distance of the center of the current pixel from the
C centroid of the star [radius vector r(i,j)] is exactly equal to the 
C radius of the aperture [R(k)], then one-half of the counts in the
C pixel are included.  If r(i,j) < R(k)-0.5, then the entire pixel is
C included, while if r(i,j) > R(k)+0.5, the pixel is wholly excluded.
C In between, viz. for  R(k)-0.5 < r(i,j) < R(k)+0.5, the fraction of
C the counts included varies linearly.  Therefore a circular aperture
C is approximated by an irregular (not even convex) polygon.
C
C If this pixel falls completely outside the LARGEST aperture, go on
C to the next pixel.  Notice that APMXSQ has actually been defined
C as (R(k)+0.5)**2 for the largest value of R(k), in accordance with
C the formula used for the partial pixels.
C
 2110       CONTINUE
            IF (RSQ .GT. APMXSQ) GO TO 2125
            R=SQRT(RSQ)-0.5
C
            DO 2120 K=1,NAPER
C
C If this pixel falls completely outside THIS aperture, go on to the
C next aperture.
C
               IF (R .GT. PAR(K)) GO TO 2120
               FRACTN=AMAX1(0.0, AMIN1(1.0,PAR(K)-R))
C
C FRACTN is the fraction of the pixel that falls inside the 
C (irregular) aperture.
C
C If the pixel is bad, set the total counts in this aperture to a number
C so negative that it will never be positive again.
C
               IF ((DATUM .GE. LOBAD) .AND. (DATUM .LE. HIBAD) .AND.
     .              (APMAG(K) .GT. -1.0D38)) THEN
                  APMAG(K) = APMAG(K)+DBLE(FRACTN*DATUM)
                  AREA(K) = AREA(K)+DBLE(FRACTN)
               ELSE
                  APMAG(K) = -1.1D38
               END IF
 2120       CONTINUE
 2125    CONTINUE
C
 2130 CONTINUE
C
C We have accumulated the brightnesses of individual sky pixels in the
C one-dimensional array SKY.  Pixels falling above or below the BAD 
C limits have already been eliminated.  Now sort SKY to place the 
C pixels in order of increasing brightness.
C
      IF (NSKY .LT. MINSKY)  THEN
         write (6, *) 'There aren''t enough pixels in the sky annulus.'
         WRITE (6,*)
     .        ' Are you sure your bad pixel thresholds are all right?'
         WRITE (6,*)
     .        ' If so, then you need a larger outer sky radius.'
         stop
      END IF

c Sort the pixel values of the background for use in MMM

      call sortlr (NSKY, SKY)
C
C Obtain the mode, standard deviation, and skewness of the peak in the
C sky histogram.
C
      CALL MMM (SKY, NSKY, HIBAD, DUM, DATUM, SKYMOD, SKYSIG, SKYSKW)
      SKYVAR=SKYSIG**2
      SIGSQ=SKYVAR/FLOAT(NSKY)
C
C SKYMOD has units of (ADU/pixel), and SKYSIG is the pixel-to-pixel
C scatter of SKYMOD, in units of (ADU/pixel).  SKYVAR is the
C variance (square of the standard deviation) of the sky brightness,
C (ADU/pixel)**2, and SIGSQ is the square of the standard error of the 
C mean sky brightness.
C
C Subtract the sky from the integrated brightnesses in the apertures,
C convert the results to magnitudes, and compute standard errors.
C
      DO 2220 I=1,NAPER
C
C If the modal sky value could not be determined, set the magnitude 
C to 99.999.  
C
      IF (SKYSIG .LT. -0.5) GO TO 2210
      APMAG(I)=APMAG(I)-DBLE(SKYMOD)*AREA(I)
C
C If the star + sky is fainter than the sky, or if the star aperture 
C extends beyond the limits of the picture, or if there is a bad pixel 
C in the star aperture, set the magnitude to 99.999.
C
      IF (APMAG(I) .LE. 0.0D0) GO TO 2210
      ERROR(1)=SNGL(AREA(I))*SKYVAR
      ERROR(2)=SNGL(APMAG(I))/PHPADU
      ERROR(3)=SIGSQ*SNGL(AREA(I))**2
C
C These variables ERRORn are the respective variances (squares of the
C mean errors) for: (1) random noise inside the star aperture, including
C readout noise and the degree of contamination by other stars in the
C neighborhood, as estimated by the scatter in the sky values (this
C standard error increases as the square root of the area of the 
C aperture); (2) the Poisson statistics of the observed star brightness;
C (3) the uncertainty of the mean sky brightness (this standard error 
C increases directly with the area of the aperture).
C
      MAGERR(I)=AMIN1(9.999, 
     .     1.0857*SQRT(ERROR(1)+ERROR(2)+ERROR(3))/SNGL(APMAG(I)))
      APMAG(I)=25.D0-2.5D0*DLOG10(APMAG(I))
      IF (APMAG(I) .GT. 99.999D0) GO TO 2210
      GO TO 2220
 2210 APMAG(I)=99.999D0
      MAGERR(I)=9.999
 2220 CONTINUE
C
C Write out the answers.
C      
      do i = 1, NAPER
         apmageout(i,i_s) = apmag(i)
         magerrout(i,i_s) = magerr(i)
      end do
      s_sky(i_s) = skymod
      vs(i_s) = AMIN1(999.99,SKYSIG)
      sk(i_s) = AMIN1(999.99, AMAX1(-99.99,SKYSKW))
C
      GO TO 2000
C
C-----------------------------------------------------------------------
C
C Normal return.  
C
 9000 CONTINUE

      RETURN
C
C-----------------------------------------------------------------------
C
      END!
