C -*-compile-command: "make_libpsf"; -*-
c
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
c This subroutine determines the PSF on a set of stars.
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

      SUBROUTINE  GETPSF (PIC, NCOL, NROW, psfrad, fitrad, lobad, hibad,
     $  phpadu, readns, fwhm, anal_mod, iexpand, ifrac, npass, id, xcen,
     $  ycen, apmag, sky, NTOT, idp, nsp, nei, PAR, NPAR, psf, npsf,
     $  nexp, psf_head)
C
C=======================================================================
C
C This subroutine generates a point-spread function from one or the
C average of several stars.
C
C                OFFICIAL DAO VERSION:  1991 May 10
C
C The subroutine reads in a subarray around the first desired 
C point-spread function star, fits a gaussian profile to the core of 
C the image, and generates a look-up table of the residuals of the 
C actual image data from the gaussian fit.  If desired, it will then
C fit this PSF to another star to determine its precise centroid, 
C scale the same Gaussian to the new star's core, and add the 
C differences between the actual data and the scaled Gaussian to the
C look-up table.  This can go on star after star after star.
C The parameters of the Gaussian approximation and the table of 
C corrections from the approximation to the true PSF are stored in 
C a disk file.
C
C Arguments
C
C PSFRAD (INPUT) is the radius, in pixels, of the circular area within
C        which we ultimately wish to define the PSF.  Because of the
C        need to perform numerical interpolation, we will actually
C        generate and store a look-up table occupying a larger area.
C        The look-up table will be square, even though the corners will
C        be unused.
C
C FITRAD (INPUT) is the fitting radius, which will be used in defining 
C        what is meant by a "neighbor" of the PSF star.
C
C Both of these arguments are user-definable parameters, whose values
C may be altered by a DEFAULT.OPT file, or by the OPTIONS command.
C
C=======================================================================
C
C Modifications as of 1991 May 10.
C ===================================
C
C
C I'm going to try to rig this thing so it can reduce the weights of
C (that is, "ignore") discordant pixels in the calculation of the
C look-up tables of corrections.  To accomplish this, I'm going to
C change the way PSF operates.  Instead of reading in a subsection
C for each PSF star one at a time, it will read in the whole image
C and work in memory.
C
      IMPLICIT NONE

      INTEGER MAXBOX, MAXPSF, MAXN, MAXPAR, MAXEXP, NCOL, NROW
      INTEGER anal_mod

      PARAMETER  (MAXPAR=6, MAXPSF=500, MAXEXP=6, 
     .     MAXBOX=69, MAXN=500) 
C
C Parameters
C
C MAXBOX is the square subarray that will hold the largest final PSF.
C        If the maximum PSF radius permitted is R, then MAXBOX is the
C        odd integer 2*INT(R)+1.  However, because we will be dealing
C        with two levels of interpolation:  (1) interpolating the raw
C        picture data to arrive at a PSF whose centroid coincides with
C        the central pixel of the look-up table; and (2) interpolating
C        within the PSF itself to evaluate it for comparison with the
C        raw picture data for the program stars, PSF will have to
C        operate on a square array which is larger by 7 pixels in X
C        and Y than MAXBOX.  Hence, the dimensions below are all
C        MAXBOX + 7.
C        
C MAXPSF is the dimension of the largest lookup table that will ever 
C        need to be generated.  Recall that the corrections from the
C        Gaussian approximation of the PSF to the true PSF will be
C        stored in a table with a half-pixel grid size.
C        MAXPSF must then equal
C
C                       2*[ 2*INT(R) + 1 ] + 7.
C
C MAXN is the maximum permitted number of PSF stars.
C
C MAXPAR is the maximum number of parameters which may be used to 
C        describe the analytic part of the PSF, *** IN ADDITION TO
C        the central intensity, and the x and y centroids.
C
C MAXEXP is the maximum number of terms in the expansion of the
C        look-up table (1 for constant, 3 for linear, 6 for quadratic).
C
      CHARACTER LINE*80, LABEL*8, psf_head(2)*80
      REAL CON(MAXPSF,MAXPSF), CORNER(MAXPSF*MAXPSF/4)
      REAL PIC(NCOL,NROW)
      REAL PAR(MAXPAR)
      REAL C(MAXEXP,MAXEXP), V(MAXEXP), TERM(MAXEXP)
      REAL PSF(MAXPSF,MAXPSF,MAXEXP)
      REAL XCEN(*), YCEN(*), APMAG(*), SKY(*)
      REAL HPSF(MAXN), WEIGHT(MAXN)
      REAL HJNK(MAXN), XJNK(MAXN), YJNK(MAXN)
      REAL PROFIL, BICUBC, PCTILE, SQRT
      INTEGER ID(*), NTAB(-1:2), NPARAM, idp(*), nsp, nei, lun_o
      LOGICAL IN(MAXPSF,MAXPSF), EDGE(MAXPSF,MAXPSF), SATR8D(MAXN)
C
      DOUBLE PRECISION SUM, VOL
      REAL LOBAD, HIBAD, PHPADU, READNS,
     .     FWHM, FITRAD, RADSQ, PSFRAD, PSFRSQ, PSFMAG, FMAX, 
     .     DFDX, DFDY, SIG, DX, DY, RDX, RDY, XMID, YMID, DP, OLD, W, 
     .     WT, RADIUS, SCALE, SUMSQ, SUMN, DATUM, RSQ
      INTEGER I, J, K, L, N, NIN, LX, LY, MX, MY, NTOT, NSTAR, ISTAR, NL
      INTEGER IEXPAND, IFRAC, IPSTYP, NPSF, NPAR, ISTAT, EDGESQ, is,
     .     NEXP, MIDDLE, MIDSQ, IDX, JDY, JDYSQ, IX, JY, ITER, NPASS
      LOGICAL STAROK, SATUR8, REDO
C
      DATA NTAB / 0, 1, 3, 6 /
C
C-----------------------------------------------------------------------
C
C SECTION 1
C
C Set up the necessary variables, open the necessary files, read in the
C relevant data for all stars.
C
      RADSQ = FITRAD**2
      PSFRSQ = (PSFRAD+2.)**2
C
C If in the final analysis we want to be able to determine a value for
C the point-spread function anywhere within a radius of R pixels, and
C we want the lookup table to have a one-half pixel grid spacing in 
C each coordinate.  To go from 0 to R with half-pixel steps, we
C need 2*R points, not including the central ("0") one.  Outside this
C we will need an additional pixel, just to make sure we will be able to
C do the interpolations at the outermost radii.  Then, to get a
C square box centered on (0,0) we will need 2*(2*R+1)+1 pixels.
C
      NPSF = 2*(NINT(2.*PSFRAD)+1)+1
      PSFRAD = (REAL(NPSF-1)/2. - 1.)/2.
C
C-----------------------------------------------------------------------
C
C SECTIONS 2
C
C Learn the name of each PSF star and find it in the star list.  Display
C a subarray around it if desired, and check it for invalid pixels.
C
      NSTAR = 0
      N = NPARAM(1, FWHM, LABEL, PAR, MAXPAR)
C
C NSTAR will point at the last PSF star in the stack after the PSF
C stars have been brought to the top.
C
      is = 0
 2000 CONTINUE
      is = is + 1
      IF (is .gt. nsp) goto 3000

      do istar = 1, ntot
         if (idp(is) .eq. id(istar)) goto 2020
      end do
      write (6, '(i5,a)') idp(is), ': Star not found.'
      goto 2000
C
C If a given star appears in the .LST file more than once, employ it
C only once.
C
 2020 IF (ISTAR .LE. NSTAR) GO TO 2000
      IF ( ( INT(XCEN(ISTAR)-FITRAD) .LT. 0 ) .OR.
     .     ( INT(XCEN(ISTAR)+FITRAD) .GT. NCOL ) .OR.
     .     ( INT(YCEN(ISTAR)-FITRAD) .LT. 0 ) .OR.
     .     ( INT(YCEN(ISTAR)+FITRAD) .GT. NROW ) ) THEN
         WRITE (6,'(i5,a)') ID(ISTAR), ': Too near edge of frame.'
         GO TO 2000
      END IF
      IF (NSTAR .EQ. 0) THEN
C
C This assigns the aperture magnitude of the first PSF star to the
C constant PSFMAG.  If the star-list file contains an aperture
C magnitude, use that one rather than the one in the input
C photometry file, which may have accumulated some drift over the
C course of several iterations.
C
         PSFMAG = APMAG(ISTAR)
      END IF
C
C Define the subarray containing the star and search it for "bad" 
C pixels.  This will include a two-pixel border around the PSF box 
C itself, to guarantee valid interpolations to within the PSF box.
C
      LX = MAX0( 1, INT(XCEN(ISTAR)-PSFRAD)-1 )
      LY = MAX0( 1, INT(YCEN(ISTAR)-PSFRAD)-1 )
      MX = MIN0(NCOL, INT(XCEN(ISTAR)+PSFRAD)+2 )
      MY = MIN0(NROW, INT(YCEN(ISTAR)+PSFRAD)+2 )
C
      FMAX=-32768.
      STAROK=.TRUE.
      SATUR8=.FALSE.
C
 2040 REDO = .FALSE.
      DO 2070 J=LY,MY
         DP = (REAL(J)-YCEN(ISTAR))**2
         DO 2050 I=LX,MX
            DX = REAL(I)-XCEN(ISTAR)
            RSQ = DX**2 + DP
            IF (RSQ .GT. PSFRSQ) THEN
               IF (DX .GE. 0.) GO TO 2070
               GO TO 2050
            END IF
            DATUM = PIC(I,J)
            IF ((DATUM .LT. LOBAD) .OR. (DATUM .GT. HIBAD)) THEN
C
C There is a defective pixel.
C
               IF (RSQ .LE. RADSQ) THEN
C
C It is inside the fitting radius.
C
                 IF (DATUM .LT. LOBAD) THEN
C
C The pixel is bad low, so reject the star.
C
                   WRITE (6,627) ID(ISTAR)
  627              FORMAT (1X, I5, ' is not a good star.')
                   GO TO 2000
                 ELSE IF (.NOT. SATUR8) THEN
C
C The pixel is bad high, so presume the star is saturated.
C
                    WRITE (6,626) ID(ISTAR)
 626                FORMAT (I5, ' is saturated. Don''t use it.')
                    GO TO 2000
                 END IF
                 GO TO 2050
               END IF
C
C The bad pixel isn't inside the fitting radius.
C
               IF (STAROK) THEN
                 STAROK = .FALSE.
               END IF
C
C Replace the defective pixel with an average of surrounding
C pixels.
C
               DATUM = 0.
               SUMN = 0.
               DO L=MAX0(1,J-1),MIN0(NROW,J+1)
                  DO K=MAX0(1,I-1),MIN0(NCOL,I+1)
                     IF ((PIC(K,L) .GE. LOBAD) .AND.
     .                   (PIC(K,L) .LE. HIBAD)) THEN
                        DATUM = DATUM + PIC(K,L)
                        SUMN = SUMN+1.
                     END IF
                  END DO
               END DO
C
C If there are fewer than three valid neighboring pixels, do not
C fudge the pixel yet.  Instead, we will go through again (and again
C if necessary) and allow the fudge to eat into the defect from the
C outside.
C
               IF ((SUMN .LT. 2.5) .AND. (.NOT. SATUR8)) THEN
                  REDO = .TRUE.
               ELSE
                  PIC(I,J) = DATUM/SUMN
               END IF
            ELSE
C
C The pixel wasn't bad.
C
               IF (DATUM .GT. FMAX) FMAX=DATUM
            END IF
 2050    CONTINUE
 2070 CONTINUE
      IF (REDO) GO TO 2040
C
C
C Switch this star (at position ISTAR) with the one at position NSTAR+1
C (which is the highest star in the stack not already a PSF star).
C Estimate the height of the best-fitting profile of type 1 (probably
C a Gaussian) on the basis of the central pixel.
C
      NSTAR = NSTAR+1
      CALL SWAPP (ID, XCEN, YCEN, APMAG, SKY, ISTAR, NSTAR)
      HPSF(NSTAR) = (FMAX-SKY(NSTAR))/
     .     PROFIL(1, 0., 0., PAR, DFDX, DFDY, TERM, 0)
      SATR8D(NSTAR) = SATUR8
      IF (NSTAR .LE. MAXN) GO TO 2000
C
 3000 CONTINUE
      NEXP = NTAB(IEXPAND) + 2*IFRAC
      IF (NSTAR .LT. NEXP) THEN
         write (6, '(a)')
     .     ' There aren''t enough PSF stars for a PSF this variable.'
         WRITE (6,*) ' Please change something.'
         RETURN
      END IF
C
C If the first star is saturated, exchange it for the first 
C unsaturated one.
C
      IF (SATR8D(1)) THEN
         DO ISTAR=2,NSTAR
            IF (.NOT. SATR8D(ISTAR)) GO TO 3005
         END DO
         write (6, '(a)') 'Every single PSF star is saturated.'
         RETURN
C
 3005    CONTINUE
         CALL SWAPP (ID, XCEN, YCEN, APMAG, SKY, 1, ISTAR)
         SATR8D(ISTAR) = .TRUE.
         SATR8D(1) = .FALSE.
      END IF
C
C If OPT(16) (Analytic model PSF) is negative, then try all PSF types
C from 1 to | OPT(16) |, inclusive.
C
      K = anal_mod
      J = MAX0(1,K)
      K = IABS(K)
      OLD = 1.E38
      IPSTYP = 0
      DO 3050 I=J,K
C
         DO L=1,NSTAR
            HJNK(L) = HPSF(L)
            XJNK(L) = XCEN(L)
            YJNK(L) = YCEN(L)
         END DO
C
         N = NPARAM(I, FWHM, LABEL, PAR, MAXPAR)
         CALL  FITANA  (PIC, NCOL, NROW, HJNK, XJNK, YJNK, SKY, 
     .       SATR8D, NSTAR, FITRAD, I, PAR, N, SIG)
C
C SIG is the root-mean-square scatter about the best-fitting analytic
C function averaged over the central disk of radius FITRAD, expressed
C as a fraction of the peak amplitude of the analytic model.
C
         IF (PAR(1) .LT. 0.) GO TO 3050
C
C If this latest fit is better than any previous one, store the
C parameters of the stars and the model for later reference.
C
         IF (SIG .LT. OLD) THEN
            IPSTYP = I
            WRITE (LINE,201) (PAR(L), L=1,N)
  201       FORMAT (1X, 1P, 6E13.5)
            DO L=1,NSTAR
               HPSF(L) = HJNK(L)
               XCEN(L) = XJNK(L)
               YCEN(L) = YJNK(L)
            END DO
            OLD = SIG
         END IF
 3050 CONTINUE
      IF (IPSTYP .LE. 0) RETURN
      NPAR = NPARAM(IPSTYP, FWHM, LABEL, PAR, MAXPAR)
      READ (LINE,*) (PAR(L), L=1,NPAR)
      anal_mod = IPSTYP
C
C This last bit ensures that the subsequent computations will be 
C performed with the parameters rounded off exactly the same as they
C appear in the output .PSF file.
C
C Write PSF file header.
C
      XMID = REAL(NCOL-1)/2.
      YMID = REAL(NROW-1)/2.
      WRITE (psf_head(1),202) LABEL, NPSF, NPAR, NTAB(IEXPAND), 2*IFRAC,
     .     PSFMAG, HPSF(1), XMID, YMID
  202 FORMAT (1X, A8, 4I5, F9.3, F15.3, 2F9.1)
      psf_head(2) = LINE
C
C=======================================================================
C
C At this point, we have values for the parameters of the best-fitting
C analytic function.  Now we may want to generate the look-up table of
C corrections from the best-fitting analytic function to the actual
C data.  This will be generated within a square box of size NPSF x NPSF
C with half-pixel spacing, centered on the centroid of the star.
C
C First, subtract the analytic function from all the PSF stars in the
C original image.
C
 3300 DP = 0.
      SATUR8 = .FALSE.
      DO 3400 ISTAR=1,NSTAR
         IF (HPSF(ISTAR)*PROFIL(IPSTYP,0.,0.,PAR,DFDX,DFDY,TERM,0)
     .        + SKY(ISTAR) .GT. HIBAD) SATR8D(ISTAR) = .TRUE.
C
C The centroids and peak heights are not yet known for saturated
C stars.
C
      IF (SATR8D(ISTAR)) THEN
         SATUR8 = .TRUE.
         GO TO 3400
      END IF
C
      LX = MAX0( 1, INT(XCEN(ISTAR)-PSFRAD)-1 )
      LY = MAX0( 1, INT(YCEN(ISTAR)-PSFRAD)-1 )
      MX = MIN0(NCOL, INT(XCEN(ISTAR)+PSFRAD)+2 )
      MY = MIN0(NROW, INT(YCEN(ISTAR)+PSFRAD)+2 )
      RDX = 0.0
      RDY = 0.0
      DO 3350 J=LY,MY
         DY = REAL(J) - YCEN(ISTAR)
         W = DY**2
         DO I=LX,MX
            DX = REAL(I) - XCEN(ISTAR)
            PIC(I,J) = PIC(I,J) - HPSF(ISTAR) *
     .           PROFIL(IPSTYP, DX, DY, PAR, DFDX, DFDY, TERM, 0)
            IF (DX**2 + W .LT. RADSQ) THEN
               RDX = RDX + (PIC(I,J)-SKY(ISTAR))**2
               RDY = RDY + 1.
            END IF
         END DO
 3350 CONTINUE
      DP = DP + RDY                      ! Total number of pixels
      RDX = SQRT(RDX/RDY)                ! Scatter inside fit radius
      PSF(ISTAR,1,1) = RDX/(HPSF(ISTAR)*
     .     PROFIL(IPSTYP, 0., 0., PAR, DFDX, DFDY, TERM, 0))
 3400 CONTINUE
C
      DP = SQRT(DP/(DP-REAL(NEXP+3*NSTAR))) ! Degrees of freedom
      DO ISTAR=1,NSTAR
         IF (.NOT. SATR8D(ISTAR)) THEN
            PSF(ISTAR,1,1) = DP * PSF(ISTAR,1,1)
            IF (SIG .GT. 0.) THEN
               RDX = PSF(ISTAR,1,1)/SIG
               IF (HPSF(ISTAR) .LE. 0) THEN
                  WEIGHT(ISTAR) = 0.
               ELSE
                  WEIGHT(ISTAR) = 1./(1. + (RDX/2.)**2)
               END IF
            END IF
         END IF
      END DO
      K = (NSTAR-1)/5 + 1                   ! Number of lines
      IF (IEXPAND+IFRAC .LT. 0) GO TO 2900
      SCALE = HPSF(1)
      DO ISTAR=NSTAR,1,-1
         HPSF(ISTAR) = HPSF(ISTAR)/HPSF(1)
      END DO
C
C Tabulate the constant part of the PSF.
C
      MIDDLE = (NPSF+1)/2
      MIDSQ = MIDDLE**2
      EDGESQ = (MIDDLE-2)**2
      DO J=1,NPSF
         JDY = J-MIDDLE
         JDYSQ = JDY**2
         DY = REAL(JDY)/2.
         DO I=1,NPSF
            IDX = I-MIDDLE
            K = IDX**2 + JDYSQ
            IF (K .LE. MIDSQ) THEN
               IN(I,J) = .TRUE.
               NIN = NIN+1
               DX = REAL(IDX)/2.
               CON(I,J) = SCALE *
     .              PROFIL(IPSTYP, DX, DY, PAR, DFDX, DFDY, V, 0)
               IF (K .GE. EDGESQ) THEN
                  EDGE(I,J) = .TRUE.
               ELSE
                  EDGE(I,J) = .FALSE.
               END IF
            ELSE
               IN(I,J) = .FALSE.
               EDGE(I,J) = .FALSE.
            END IF
         END DO
      END DO
C
C=======================================================================
C
C Now compute look-up tables.
C
 3500 CONTINUE
C
C MIDDLE is the center of the look-up table, which will correspond to
C the centroid of the analytic PSF.  MIDSQ is the square of the
C radius of the PSF table, plus a pixel's worth of slack just to
C make sure you'll always have a 4x4 array suitable for interpolation.
C
      DO K=1,NEXP
         DO J=1,NPSF
            DO I=1,NPSF
               PSF(I,J,K) = 0.0
            END DO
         END DO
      END DO
C
      DO 4950 JY=1,NPSF
         RDY = REAL(JY-MIDDLE)/2.
C
         DO 4940 IX=1,NPSF
C
C Don't waste time with pixels outside a radius = (MIDDLE-1).
C
            IF (.NOT. IN(IX,JY)) GO TO 4940
            RDX = REAL(IX-MIDDLE)/2.
            DO 4935 ITER=0,NPASS
C
C Initialize accumulators.
C
            TERM(1) = 1.
            SUMSQ = 0.
            SUMN = 0.
            DO L=1,NEXP
               V(L) = 0.
               DO K=1,NEXP
                  C(K,L) = 0.
               END DO
            END DO
C
C Now, for this point in the set of lookup table(s) [(IX,JY), where
C (MIDDLE,MIDDLE) corresponds to the centroid of the PSF], consult
C each of the PSF stars to determine the value(s) to put into the
C table(s).
C
            DO 4900 ISTAR = 1,NSTAR
               IF (SATR8D(ISTAR)) GO TO 4900
C
C What pixels in the original image constitute a 4x4 box surrounding
C this (IX,JY) in the PSF, as referred to this star's centroid?
C
               DX = XCEN(ISTAR) + RDX
               DY = YCEN(ISTAR) + RDY
               I = INT(DX)
               J = INT(DY)
C
C The 4x4 box is given by  I-1 <= x <= I+2, J-1 <= y <= J+2.
C
               IF ((I .LT. 2) .OR. (J .LT. 2) .OR. 
     .              (I+2 .GT. NCOL) .OR.
     .              (J+2 .GT. NROW)) GO TO 4900           ! Next star
C
               DO L=J-1,J+2
                  DO K=I-1,I+2
                     IF (PIC(K,L) .GT. HIBAD) GO TO 4900
                  END DO
               END DO
C
               DX = DX - I
               DY = DY - J
C
C The point which corresponds PRECISELY to the offset (RDX,RDY)
C from the star's centroid, lies a distance (DX,DY) from pixel
C (I,J).
C
C Use bicubic interpolation to evaluate the residual PSF amplitude
C at this point.  Scale the residual up to match the first PSF star.
C
               DP = BICUBC(PIC(I-1,J-1), NCOL, DX, DY, DFDX, DFDY)
     .              - SKY(ISTAR)
               DP = DP/HPSF(ISTAR)
               SUMSQ = SUMSQ + ABS(DP)
               SUMN = SUMN + 1.
               IF (IEXPAND .GE. 1) THEN
                  TERM(2) = (XCEN(ISTAR)-1.)/XMID-1.
                  TERM(3) = (YCEN(ISTAR)-1.)/YMID-1.
                  IF (IEXPAND .GE. 2) THEN
                     TERM(4) = 1.5*TERM(2)**2-0.5
                     TERM(5) = TERM(2)*TERM(3)
                     TERM(6) = 1.5*TERM(3)**2-0.5
                  END IF
               END IF
C
C               IF (IFRAC .GE. 1) THEN
C
C INSERT CODE HERE
C
               IF (ITER .GT. 0) THEN
                  OLD = 0.
                  DO K=1,NEXP
                     OLD = OLD + PSF(IX,JY,K) * TERM(K)
                  END DO
C
                  IF (ITER .LE. MAX0(3,NPASS/2)) THEN
                     W = HPSF(ISTAR)/(1. + (ABS(DP - OLD)/SIG))
                  ELSE 
                     W = HPSF(ISTAR)/(1. + ((DP - OLD)/SIG)**2)
                  END IF
               ELSE
                  W = HPSF(ISTAR)
               END IF
C
               W = W*WEIGHT(ISTAR)
               DO K=1,NEXP
                  WT = W*TERM(K)
                  V(K) = V(K) + WT*DP
                  DO L=1,NEXP
                     C(K,L) = C(K,L) + WT*TERM(L)
                  END DO
               END DO
 4900       CONTINUE
C
            IF (SUMN .LT. NEXP) THEN
               write (6, '(a)')
     .           ('Not enough PSF stars.  Please start over.')
               RETURN
            END IF
            CALL INVERS (C, MAXEXP, NEXP, ISTAT)
            CALL VMUL (C, MAXEXP, NEXP, V, TERM)
            DO K=1,NEXP
               PSF(IX,JY,K) = TERM(K)
            END DO
            IF (SUMN .LE. NEXP) GO TO 4940
            SIG = 1.2533*SUMSQ/SQRT(SUMN*(SUMN - NEXP))
 4935       CONTINUE
 4940    CONTINUE
 4950 CONTINUE
      IF (NEXP .LT. 2) GO TO 2800
C
C At this point, we must be sure that any higher order terms
C in the PSF [i.e. those that go as powers of (XCEN-XMID) and 
C (YCEN-YMID)] contain zero volume, so that the total volume
C of the PSF is independent of position.  This will be done
C by looking at the total flux contained in the look-up
C tables of index 2 and higher.  The analytic function
C will be scaled to the same total flux and subtracted from
C each of the higher lookup tables and added into the
C lookup table of index 1.  I scale the analytic function
C instead of simply transferring the net flux from the 
C higher tables to table 1, because it is poor fits of
C the analytic profile to the image data which has caused
C the net flux in the lookup tables to depend upon position.
C
C Compute the brightness weighted average value of each of 
C the terms in the polynomial expansion.
C
      DO K=1,NEXP
         TERM(K) = 0.
      END DO
C
      DO ISTAR=NSTAR,1,-1
         DX = (XCEN(ISTAR)-1.)/XMID-1.
         DY = (YCEN(ISTAR)-1.)/YMID-1.
         W = WEIGHT(ISTAR)*HPSF(ISTAR)
         TERM(1) = TERM(1) + W
         TERM(2) = TERM(2) + W*DX
         TERM(3) = TERM(3) + W*DY
         IF (IEXPAND .GE. 2) THEN
            TERM(4) = TERM(4) + W*(1.5*DX**2-0.5)
            TERM(5) = TERM(5) + W*(DX*DY)
            TERM(6) = TERM(6) + W*(1.5*DY**2-0.5)
         END IF
C
C            IF (IFRAC .GE. 1) THEN
C
C INSERT CODE HERE
C
      END DO
C
      DO K=NEXP,1,-1
         TERM(K) = TERM(K)/TERM(1)
      END DO
C
C Okey doke.  Now if there is any net volume contained in any of the
C higher order (variable) terms of the PSF, we will remove it in two
C steps.  First of all, there is the possibility that there were some
C errors in the sky estimates which was correlated with position.
C We will remove this by determining the median value of PSF(I,J,K)
C in the outermost ring of the PSF, at the PSF RADIUS.  This
C value will be subtracted from PSF(I,J,K); it will also be added into
C the constant part of the PSF --- evaluated at a location corresponding
C to the mean value of the polynomial term --- to keep the PSF for the
C mean polynomial unchanged.  That way, if there is on average some
C systematic error in sky estimate, at least it will be constant over
C the area of the frame.  (This, of course, is potentially dangerous for
C a globular cluster or similar object centered within the frame.  I 
C have to think about this some more.)
C
      DO K=2,NEXP
         L = 0
         DO J=1,NPSF
            DO I=1,NPSF
               IF (EDGE(I,J)) THEN
                  L = L+1
                  CORNER(L) = PSF(I,J,K)
               END IF
            END DO
         END DO
         DX = PCTILE(CORNER, L, (L+1)/2)
         DY = TERM(K) * DX
C
         DO J=1,NPSF
            DO I=1,NPSF
               IF (IN(I,J)) THEN
                  PSF(I,J,K) = PSF(I,J,K) - DX
                  PSF(I,J,1) = PSF(I,J,1) + DY
               END IF
            END DO
         END DO
      END DO
C
C Now for each of the higher PSF's, determine the mean volume inside
C the PSF RADIUS.  Scale the constant part of the PSF as needed and
C subtract from the variable part and add back in to the constant part.
C
C
C Ensure that that profile which will be added to or subtracted from
C the various subtables has a median value of zero around its rim.
C
      L = 0
      DO J=1,NPSF
         DO I=1,NPSF
            IF (EDGE(I,J)) THEN
               L = L+1
               CORNER(L) = CON(I,J)
            END IF
         END DO
      END DO
      DX = PCTILE(CORNER, L, (L+1)/2)
C
      VOL = 0.0D0
      DO J=1,NPSF
         DO I=1,NPSF
            IF (IN(I,J)) THEN
               CON(I,J) = CON(I,J) - DX
               VOL = VOL + DBLE(CON(I,J))
            END IF
         END DO
      END DO
C
C Now determine the net volume of each of the higher-order PSF 
C tables, and force it to zero by subtracting a scaled copy of
C the constant PSF.  Scale the part that has been subtracted off
C by the mean polynomial term and add it in to the constant
C part of the PSF, so that at the centroid of the PSF stars'
C positions, the PSF remains unchanged.
C
      DO K=2,NEXP
         SUM = 0.0D0
         DO J=1,NPSF
            DO I=1,NPSF
               IF (IN(I,J)) SUM = SUM + DBLE(PSF(I,J,K))
            END DO
         END DO
C
         DX = SNGL(SUM/VOL)
         DY = TERM(K)*DX
         DO J=1,NPSF
            DO I=1,NPSF
               IF (IN(I,J)) THEN
                  PSF(I,J,K) = PSF(I,J,K) - DX * CON(I,J)
                  PSF(I,J,1) = PSF(I,J,1) + DY * CON(I,J)
               END IF
            END DO
         END DO
      END DO
C
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
 2800 CONTINUE
C
C---------------------------------------------------------------------
C
c      DO K=1,NEXP
c         WRITE (lun_o,204) ((PSF(I,J,K), I=1,NPSF), J=1,NPSF)
c  204    FORMAT (1X, 1P, 6E13.6)
c      END DO
C
 2900 continue

      nsp = NSTAR
      nei = NSTAR
      IF (NSTAR .GE. NTOT) GO TO 9000
C
C Now look for all stars within a radius equal to 
C 1.5*PSFRAD+2.*FITRAD+1 of any of the PSF stars.
C
      RSQ = (1.5*PSFRAD+2.*FITRAD+1.)**2
      L = NSTAR+1
C
C L will point at the first irrelevant star.
C
      DO I=1,NSTAR
         J = L
 5020    IF ((XCEN(J)-XCEN(I))**2 + (YCEN(J)-YCEN(I))**2 .LE. RSQ) THEN
            CALL SWAPP (ID, XCEN, YCEN, APMAG, SKY, J, L)
            nei = L
            L = L + 1
         END IF
         IF (J .LT. NTOT) THEN
            J = J+1
            GO TO 5020
         END IF
      END DO
C
C Now all stars in the stack between positions NSTAR+1 and L-1,
C inclusive, lie within the specified radius of the PSF stars and have
C been written out.  Finally, look for any stars within a radius 
C equal to 2*FITRAD+1 of any of THESE stars, and of each other.
C
      IF (L .GT. NTOT) GO TO 9000
      RSQ = (2.*FITRAD+1)**2
      I = NSTAR+1
 5110 J = L
 5120 IF ((XCEN(J)-XCEN(I))**2 + (YCEN(J)-YCEN(I))**2 .LE. RSQ) THEN
         CALL SWAPP (ID, XCEN, YCEN, APMAG, SKY, J, L)
         nei = L
         L = L + 1
      END IF
      IF (J .LT. NTOT) THEN
         J = J+1
         GO TO 5120
      END IF
C
      I = I+1
      IF (I .LT. L) GO TO 5110
 9000 continue

      RETURN
      END!
C
C#######################################################################
C
      SUBROUTINE  FITANA  (PIC, NCOL, NROW, H, XCEN, YCEN, SKY, 
     .     SATR8D, NSTAR, 
     .    FITRAD, IPSTYP, PAR, NPAR, CHI)
C
C This subroutine fits the ANALYTIC profile to the selected stars.
C
C  OFFICIAL DAO VERSION:    1991 May 10
C
      IMPLICIT NONE
      INTEGER MAXPAR, MAXBOX, MAXN, NCOL, NROW
      PARAMETER  (MAXPAR=6, MAXBOX=69, MAXN=200)
      CHARACTER*80 LINE
      REAL PIC(NCOL,NROW)
      REAL H(*), XCEN(*), YCEN(*), SKY(*), CLAMP(MAXPAR), OLD(MAXPAR)
      REAL C(MAXPAR,MAXPAR), V(MAXPAR), PAR(MAXPAR), T(MAXPAR), 
     .     Z(MAXPAR)
      REAL XCLAMP(MAXN), YCLAMP(MAXN), XOLD(MAXN), YOLD(MAXN)
      REAL ABS, PROFIL, SQRT
      LOGICAL SATR8D(*)
C
      REAL DHN, DHD, DXN, DXD, DYN, DYD, DX, DY, DYSQ
      REAL PEAK, DP, PROD, DHDXC, DHDYC, P, WT
      REAL RSQ, SUMWT, OLDCHI, CHI, FITRAD
      INTEGER I, J, K, L, LX, LY, MX, MY, ISTAT
      INTEGER ISTAR, MPAR, NITER, NPAR, IPSTYP, NSTAR
      LOGICAL FULL
C
C-----------------------------------------------------------------------
C
      FULL = .FALSE.
      DO I=1,NPAR
         Z(I) = 0.
         OLD(I) = 0.
         CLAMP(I) = 0.5
      END DO
      CLAMP(1) = 2.
      CLAMP(2) = 2.
      NITER = 0
      OLDCHI = 0.
      DO I=1,NSTAR
         XOLD(I) = 0.
         YOLD(I) = 0.
         XCLAMP(I) = 1.
         YCLAMP(I) = 1.
      END DO
C
      MPAR = 2
C
C-----------------------------------------------------------------------
C
C SECTION 1
C
C Now we will fit an integrated analytic function to the central part 
C of the stellar profile.  For each star we will solve for three 
C parameters: (1) H, the central height of the model profile (above 
C sky); (2) XCEN, C the centroid of the star in x; and (3) YCEN, 
C likewise for y.  In addition, from ALL STARS CONSIDERED TOGETHER
C we will determine any other parameters, PAR(i), required to describe 
C the profile.  We will use a circle of radius FITRAD centered on the 
C position of each PSF star. NOTE THAT we do not fit the data to an
C actual analytic profile, but rather the function is numerically
C integrated over the area of each pixel, and the observed data are fit
C to these integrals.
C
 1000 NITER = NITER+1
      IF (NITER .GT. 300) THEN
         write (6, '(a)') 'Failed to converge.'
         PAR(1) = -1.
         RETURN
      END IF
C
C Initialize the big accumulators.
C
      DO I=1,NPAR
         V(I)=0.0                        ! Zero the vector of residuals
         DO J=1,NPAR
            C(J,I)=0.0                   ! Zero the normal matrix
         END DO
      END DO
      CHI = 0.0
      SUMWT = 0.0
C
C Using the analytic model PSF defined by the current set of parameters,
C compute corrections to the brightnesses and centroids of all the PSF
C stars.  MEANWHILE, accumulate the corrections to the model parameters.
C
      RSQ = FITRAD**2
      DO 1400 ISTAR=1,NSTAR
      IF (SATR8D(ISTAR)) GO TO 1400
      LX = INT(XCEN(ISTAR)-FITRAD) + 1
      LY = INT(YCEN(ISTAR)-FITRAD) + 1
      MX = INT(XCEN(ISTAR)+FITRAD)
      MY = INT(YCEN(ISTAR)+FITRAD)
C
      DHN = 0.0D0
      DHD = 0.0D0
      DXN = 0.0D0
      DXD = 0.0D0
      DYN = 0.0D0
      DYD = 0.0D0
      DO J=LY,MY
         DY = REAL(J) - YCEN(ISTAR)
         DYSQ = DY**2
         DO I=LX,MX
            DX = REAL(I) - XCEN(ISTAR)
            WT = (DX**2+DYSQ)/RSQ
            IF (WT .LT. 1.) THEN
               P = PROFIL(IPSTYP, DX, DY, PAR, DHDXC, DHDYC, T, 0)
               DP = PIC(I,J) - H(ISTAR)*P - SKY(ISTAR)
               DHDXC = H(ISTAR)*DHDXC
               DHDYC = H(ISTAR)*DHDYC
               WT = 5./(5.+WT/(1.-WT))
               PROD = WT*P
               DHN = DHN + PROD*DP
               DHD = DHD + PROD*P
               PROD = WT*DHDXC
               DXN = DXN + PROD*DP
               DXD = DXD + PROD*DHDXC
               PROD = WT*DHDYC
               DYN = DYN + PROD*DP
               DYD = DYD + PROD*DHDYC
            END IF
         END DO
      END DO
C
      H(ISTAR) = H(ISTAR) + DHN/DHD
C
      DXN = DXN/DXD
      IF (XOLD(ISTAR)*DXN .LT. 0.) XCLAMP(ISTAR) = 0.5*XCLAMP(ISTAR)
      XOLD(ISTAR) = DXN
      XCEN(ISTAR) = XCEN(ISTAR)+DXN/(1.+ABS(DXN)/XCLAMP(ISTAR))
C
      DYN = DYN/DYD
      IF (YOLD(ISTAR)*DYN .LT. 0.) YCLAMP(ISTAR) = 0.5*YCLAMP(ISTAR)
      YOLD(ISTAR) = DYN
      YCEN(ISTAR) = YCEN(ISTAR)+DYN/(1.+ABS(DYN)/YCLAMP(ISTAR))
C
      PEAK = H(ISTAR) * PROFIL(IPSTYP,0.,0., PAR, DHDXC, DHDYC, T, 0)
      DO J=LY,MY
         DY = REAL(J)-YCEN(ISTAR)
         DYSQ = DY**2
         DO I=LX,MX
            DX = REAL(I)-XCEN(ISTAR)
            WT = (DX**2+DYSQ)/RSQ
            IF (WT .LT. 1.) THEN
               P = PROFIL(IPSTYP, DX, DY, PAR, DHDXC, DHDYC, T, 1)
               DP = PIC(I,J) - H(ISTAR)*P - SKY(ISTAR)
               DO K=1,MPAR
                  T(K) = H(ISTAR)*T(K)
               END DO
               CHI = CHI + (DP/PEAK)**2
               SUMWT = SUMWT + 1.
               WT = 5./(5.+WT/(1.-WT))
               IF (NITER .GE. 4) WT = WT/(1.+ABS(20.*DP/PEAK))
               DO K=1,MPAR
                  V(K) = V(K) + WT*DP*T(K)
                  DO L=1,MPAR
                     C(L,K) = C(L,K) + WT*T(L)*T(K)
                  END DO
               END DO
            END IF
         END DO
      END DO
 1400 CONTINUE
C
C Correct the fitting parameters.
C
      CALL INVERS (C, MAXPAR, MPAR, ISTAT)
      CALL VMUL (C, MAXPAR, MPAR, V, Z)
      DO I=1,MPAR
         IF (Z(I)*OLD(I) .LT. 0.) THEN
            CLAMP(I) = 0.5*CLAMP(I)
         ELSE
            CLAMP(I) = 1.1*CLAMP(I)
         END IF
         OLD(I) = Z(I)
         Z(I) = CLAMP(I)*Z(I)
      END DO
      Z(1) = AMAX1(-0.1*PAR(1), AMIN1(0.1*PAR(1), Z(1)))
      Z(2) = AMAX1(-0.1*PAR(2), AMIN1(0.1*PAR(2), Z(2)))
C     Z(3) = Z(3)/(1.+ABS(Z(3))/(AMIN1(0.1,1.-ABS(PAR(3)))))
      DO I=1,MPAR
         PAR(I) = PAR(I)+Z(I)
      END DO
C
      SUMWT = SUMWT - REAL(MPAR + 3*NSTAR)
      IF (SUMWT .GT. 0) THEN
         CHI = SQRT(CHI/SUMWT)
      ELSE
         CHI = 9.9999
      END IF
C
      WRITE (LINE,661) CHI, (PAR(I), I=1,MPAR)
  661 FORMAT (1X, F7.4, 6F10.5)
      I=10*MPAR+13
      IF (MPAR .EQ. NPAR) THEN
         IF (ABS(OLDCHI/CHI-1.) .LT. 1.E-5) THEN
            RETURN
         END IF
      ELSE
         IF (ABS(OLDCHI/CHI-1.) .LT. 1.E-3) THEN
            MPAR = MPAR+1
            OLDCHI = 0.
            GO TO 1000
         END IF
      END IF
C
      OLDCHI = CHI
      GO TO 1000
      END!
C
C#######################################################################
C
C Exchange two stars in the star list.
C
C   OFFICIAL DAO VERSION:       1991 May 10
C
      SUBROUTINE SWAPP (ID, X, Y, A, S, I, J)
      IMPLICIT NONE
      REAL X(*), Y(*), A(*), S(*)
      INTEGER ID(*)
C
      REAL HOLD
      INTEGER I, J, IHOLD
C
      IHOLD = ID(I)
      ID(I) = ID(J)
      ID(J) = IHOLD
      HOLD = X(I)
      X(I) = X(J)
      X(J) = HOLD
      HOLD = Y(I)
      Y(I) = Y(J)
      Y(J) = HOLD
      HOLD = A(I)
      A(I) = A(J)
      A(J) = HOLD
      HOLD = S(I)
      S(I) = S(J)
      S(J) = HOLD
      RETURN
      END!
C
C=======================================================================
C
      FUNCTION NPARAM  (IPSTYP, FWHM, LABEL, PAR, MAXPAR)
      CHARACTER*8 LABEL
      INTEGER MAXPAR
      REAL PAR(MAXPAR)
      PAR(1) = FWHM/2.
      PAR(2) = PAR(1)
      IF (IPSTYP .EQ. 1) THEN
         NPARAM = 2
         LABEL = 'GAUSSIAN'
      ELSE IF (IPSTYP .EQ. 3) THEN
         NPARAM = 3
         PAR(3) = 0.
         PAR(4) = 2.5
         LABEL = 'MOFFAT25'
      ELSE IF (IPSTYP .EQ. 5) THEN
         NPARAM = 4
         PAR(3) = 0.75
         PAR(4) = 0.0
         LABEL = 'PENNY1  '
      ELSE IF (IPSTYP .EQ. 6) THEN
         NPARAM = 5
         PAR(3) = 0.75
         PAR(4) = 0.0
         PAR(5) = 0.0
         LABEL = 'PENNY2  '
      ELSE IF (IPSTYP .EQ. 2) THEN
         NPARAM = 3
         PAR(3) = 0.
         PAR(4) = 1.5
         LABEL = 'MOFFAT15'
      ELSE IF (IPSTYP .EQ. 4) THEN
         NPARAM = 3
         PAR(3) = 0.
         LABEL = 'LORENTZ '
      ELSE
         WRITE (6, '(a)') ('Invalid PSF type: '//CHAR(IPSTYP+48))
      END IF
      RETURN
      END!
C
C***********************************************************************
C
      SUBROUTINE  INVERS (A, MAX, N, IFLAG)
C
C Although it seems counter-intuitive, the tests that I have run
C so far suggest that the 180 x 180 matrices that NSTAR needs can
C be inverted with sufficient accuracy if the elements are REAL
C rather than DOUBLE PRECISION
C
C Arguments
C 
C     A (INPUT/OUTPUT) is a square matrix of dimension N.  The inverse 
C       of the input matrix A is returned in A.
C
C   MAX (INPUT) is the size assigned to the matrix A in the calling 
C       routine.  It's needed for the dimension statement below.
C
C IFLAG (OUTPUT) is an error flag.  IFLAG = 1 if the matrix could not
C       be inverted; IFLAG = 0 if it could.
C
      IMPLICIT NONE
      INTEGER MAX
      REAL A(MAX,MAX)
C
      INTEGER N, IFLAG, I, J, K
C
C-----------------------------------------------------------------------
C
      IFLAG=0
      I=1
  300 IF(A(I,I).EQ.0.0E0)GO TO 9100
      A(I,I)=1.0E0/A(I,I)
      J=1
  301 IF(J.EQ.I)GO TO 304
      A(J,I)=-A(J,I)*A(I,I)
      K=1
  302 IF(K.EQ.I)GO TO 303
      A(J,K)=A(J,K)+A(J,I)*A(I,K)
  303 IF(K.EQ.N)GO TO 304
      K=K+1
      GO TO 302
  304 IF(J.EQ.N)GO TO 305
      J=J+1
      GO TO 301
  305 K=1
  306 IF(K.EQ.I)GO TO 307
      A(I,K)=A(I,K)*A(I,I)
  307 IF(K.EQ.N)GO TO 308
      K=K+1
      GO TO 306
  308 IF(I.EQ.N)RETURN                                   ! Normal return
      I=I+1
      GO TO 300
C
C-----------------------------------------------------------------------
C
C Error:  zero on the diagonal.
C
 9100 IFLAG=1
      RETURN
C
      END!
C
C#######################################################################
C
      SUBROUTINE  VMUL (A, MAX, N, V, X)
C
C Multiply a matrix by a vector:
C
C                    A * V = X
C
C Arguments
C
C    A(column,row)  (INPUT) is a square matrix of dimension N.
C
C              MAX  (INPUT) is the size assigned to the array in the 
C                           calling routine.
C
C           V(row)  (INPUT) is a column vector of dimension N.
C
C           X(row) (OUTPUT) is a column vector of dimension N.
C
      IMPLICIT NONE
      INTEGER MAX
      REAL A(MAX,MAX), V(MAX)
      REAL X(MAX)
C
      DOUBLE PRECISION DBLE
      REAL SNGL
C
      DOUBLE PRECISION SUM
      INTEGER N, I, J
C
C-----------------------------------------------------------------------
C
      I=1
  200 SUM=0.0D0
      J=1
  201 SUM=SUM+DBLE(A(J,I))*DBLE(V(J))
      IF (J .EQ. N) GO TO 203
      J=J+1
      GO TO 201
  203 X(I)=SNGL(SUM)
      IF (I .EQ. N) RETURN                               ! Normal return
      I=I+1
      GO TO 200
      END!
C
C#######################################################################
C
      REAL  FUNCTION  PROFIL  (IPSTYP, DX, DY, PAR, DHDXC, DHDYC, 
     .     TERM, IDERIV)
C
C Compute the value of an ANALYTIC prfile for a point DX,DY distant
C from the centroid.  Return both the computed value and its
C first derivatives with respect to x and y.  If IDERIV .NE. 0,
C return also the first derivatives with respect to all the parameters
C defining the profile.
C
      IMPLICIT NONE
      INTEGER MAXPAR, MAXPT
      PARAMETER (MAXPAR=6, MAXPT=4)
C
      REAL PAR(MAXPAR), TERM(MAXPAR)
      REAL D(MAXPT,MAXPT), W(MAXPT,MAXPT)
      REAL X(MAXPT), XSQ(MAXPT), P1XSQ(MAXPT)
C
      REAL EXP, DAOERF
C
      REAL DX, DY, DHDXC, DHDYC, WFSQ, Y, WT, WF, ONEMP3
      REAL RSQ, E, TALPHA, P1SQ, P2SQ, XY, DENOM
      REAL FUNC, YSQ, WP4FOD, P4FOD, F, P1P2, ERFX, DHDSX, ERFY
      REAL DEBY, DFBY, DBYX0, DBYY0
      REAL DHDSY, ALPHA, P2YSQ
      INTEGER I, IPSTYP, IDERIV, IX, IY, NPT
C
      DATA D / 0.00000000,  0.0,        0.0       , 0.0       ,
     .        -0.28867513,  0.28867513, 0.0       , 0.0       ,
     .        -0.38729833,  0.00000000, 0.38729833, 0.0       ,
     .        -0.43056816, -0.16999052, 0.16999052, 0.43056816/
      DATA W / 1.00000000,  0.0       , 0.0       , 0.0       ,
     .         0.50000000,  0.50000000, 0.0       , 0.0       ,
     .         0.27777778,  0.44444444, 0.27777778, 0.0       ,
     .         0.17392742,  0.32607258, 0.32607258, 0.17392742/
C
      PROFIL = 0.
      DHDXC = 0.
      DHDYC = 0.
C
      IF (IDERIV .GT. 0) THEN
         DO I=1,MAXPAR
            TERM(I) = 0.
         END DO
      END IF
C
      IF (IPSTYP .EQ. 1) THEN
C
C GAUSSIAN
C
C     F = ERFX * ERFY / (PAR(1) * PAR(2))
C
C PAR(1) is the HWHM in X; sigma(x) = 0.8493218 * HWHM
C PAR(2) is the HWHM in Y; ditto
C
         P1P2 = PAR(1)*PAR(2)
         ERFX = DAOERF(DX, 0., PAR(1), DHDXC, DHDSX)
         ERFY = DAOERF(DY, 0., PAR(2), DHDYC, DHDSY)
         PROFIL = ERFX*ERFY/P1P2
         DHDXC = DHDXC*ERFY/P1P2
         DHDYC = DHDYC*ERFX/P1P2
         IF (IDERIV .GT. 0) THEN
            TERM(1) = (DHDSX-ERFX/PAR(1))*ERFY/P1P2
            TERM(2) = (DHDSY-ERFY/PAR(2))*ERFX/P1P2
         END IF
      ELSE IF (IPSTYP .EQ. 3) THEN
C
C MOFFAT FUNCTION  BETA = 2.5
C                            BETA-1
C F = --------------------------------------------------------
C      Ax * Ay * [1 + (X/Ax)**2 + (Y/Ay)**2 + (XY*Axy)]**BETA
C
C PAR(1) is the HWHM in x at y = 0: 
C
C             1/2 = 1/[1 + (PAR(1)/Ax)**2]**BETA
C so
C             2**(1/BETA) - 1 = (PAR(1)/Ax)**2
C
C             Ax**2 = PAR(1)**2/[2**(1/BETA) - 1]
C
C When BETA = 2.5, Ax**2 = 3.129813 * PAR(1)**2
C
C Hence, let us use
C
C                                  1
C F = ---------------------------------------------------------------
C     P(1)*P(2)*{1+0.3195079*[(X/P(1))**2+(Y/P(2))**2+(XY*P(3))]**2.5
C 
C neglecting a constant of proportionality.
C
         ALPHA = 0.3195079
         TALPHA = 0.6390158                 ! 2.*ALPHA
         P1SQ = PAR(1)**2
         P2SQ = PAR(2)**2
         P1P2 = PAR(1)*PAR(2)
         XY = DX*DY
C
         DENOM = 1. + ALPHA*(DX**2/P1SQ + DY**2/P2SQ + XY*PAR(3))
         IF (DENOM .GT. 1.E4) RETURN
         FUNC = 1. / (P1P2 * DENOM**PAR(4))
         IF (FUNC .GE. 0.046) THEN
            NPT = 4
         ELSE IF (FUNC .GE. 0.0022) THEN
            NPT = 3
         ELSE IF (FUNC .GE. 0.0001) THEN
            NPT = 2
         ELSE IF (FUNC .GE. 1.E-10) THEN
            PROFIL = (PAR(4) - 1.) * FUNC
            P4FOD = PAR(4)*ALPHA*PROFIL/DENOM
            DHDXC = P4FOD*(2.*DX/P1SQ + DY*PAR(3))
            DHDYC = P4FOD*(2.*DY/P2SQ + DX*PAR(3))
            IF (IDERIV .GT. 0) THEN
               TERM(1) = (2.*P4FOD*DX**2/P1SQ-PROFIL)/PAR(1)
               TERM(2) = (2.*P4FOD*DY**2/P2SQ-PROFIL)/PAR(2)
               TERM(3) = - P4FOD*XY
C              TERM(4) = PROFIL*(1./(PAR(4)-1.)-ALOG(DENOM))
            END IF
            RETURN
         ELSE
            RETURN
         END IF
C
         DO IX=1,NPT
            X(IX) = DX+D(IX,NPT)
            XSQ(IX) = X(IX)**2
            P1XSQ(IX) = XSQ(IX)/P1SQ
         END DO
C
         DO IY=1,NPT
            Y = DY+D(IY,NPT)
            YSQ = Y**2
            P2YSQ = YSQ/P2SQ
            DO IX=1,NPT
               WT = W(IY,NPT)*W(IX,NPT)
               XY=X(IX)*Y
               DENOM = 1. + ALPHA*(P1XSQ(IX) + P2YSQ + XY*PAR(3))
               FUNC = (PAR(4) - 1.) / (P1P2 * DENOM**PAR(4))
               P4FOD = PAR(4)*ALPHA*FUNC/DENOM
               WP4FOD = WT*P4FOD
               WF = WT*FUNC
               PROFIL = PROFIL + WF
               DHDXC = DHDXC + WP4FOD*(2.*X(IX)/P1SQ + Y*PAR(3))
               DHDYC = DHDYC + WP4FOD*(2.*Y/P2SQ + X(IX)*PAR(3))
               IF (IDERIV .GT. 0) THEN
                  TERM(1) = TERM(1) + 
     .                     (2.*WP4FOD*P1XSQ(IX)-WF)/PAR(1)
                  TERM(2) = TERM(2) + 
     .                     (2.*WP4FOD*P2YSQ-WF)/PAR(2)
                  TERM(3) = TERM(3) - WP4FOD*XY
C                 TERM(4) = TERM(4) + WF*(1./(PAR(4)-1.)-ALOG(DENOM))
               END IF
            END DO
         END DO
      ELSE IF (IPSTYP .EQ. 5) THEN
C
C Penny function --- Gaussian core plus Lorentzian wings.  The Lorentzian 
C is elongated along the x or y axis, the Gaussian may be tilted.
C
         P1SQ = PAR(1)**2
         P2SQ = PAR(2)**2
         ONEMP3 = 1.-PAR(3)
         XY = DX*DY
C
         RSQ = DX**2/P1SQ + DY**2/P2SQ
         IF (RSQ .GT. 1.E10) RETURN
C
         F = 1./(1.+RSQ)
         RSQ = RSQ + XY*PAR(4)
         IF (RSQ .LT. 34.) THEN
            E = EXP(-0.6931472*RSQ)
            FUNC = PAR(3)*E + ONEMP3*F
         ELSE
            E = 0.
            FUNC = ONEMP3*F
         END IF
C
         IF (FUNC .GE. 0.046) THEN
            NPT = 4
         ELSE IF (FUNC .GE. 0.0022) THEN
            NPT = 3
         ELSE IF (FUNC .GE. 0.0001) THEN
            NPT = 2
         ELSE IF (FUNC .GE. 1.E-10) THEN
            PROFIL = FUNC
            DFBY = ONEMP3*F**2
            DEBY = 0.6931472*PAR(3)*E
            DBYX0 = 2.*DX/P1SQ
            DBYY0 = 2.*DY/P2SQ
            DHDXC = DEBY*(DBYX0 + DY*PAR(4)) + DFBY*DBYX0
            DHDYC = DEBY*(DBYY0 + DX*PAR(4)) + DFBY*DBYY0
            IF (IDERIV .GT. 0) THEN
               DBYX0 = DBYX0*DX/PAR(1)
               DBYY0 = DBYY0*DY/PAR(2)
               DFBY = DFBY + DEBY
               TERM(1) = DFBY * DBYX0
               TERM(2) = DFBY * DBYY0
               TERM(3) = E - F
               TERM(4) = - DEBY * XY
     .              / (0.5 - ABS(PAR(4)))
            END IF
            RETURN
         ELSE
            RETURN
         END IF
C
         DO IX=1,NPT
            X(IX) = DX+D(IX,NPT)
            P1XSQ(IX) = X(IX)/P1SQ
         END DO
C
         DO IY=1,NPT
            Y = DY+D(IY,NPT)
            P2YSQ = Y/P2SQ
            DO IX=1,NPT
               WT = W(IY,NPT)*W(IX,NPT)
               XY = X(IX)*Y
               RSQ = P1XSQ(IX)*X(IX) + P2YSQ*Y
               F = 1./(1.+RSQ)
               RSQ = RSQ + XY*PAR(4)
               IF (RSQ .LT. 34.) THEN
                  E = EXP(-0.6931472*RSQ)
                  FUNC = PAR(3)*E + ONEMP3*F
                  DEBY = 0.6931472*WT*PAR(3)*E
               ELSE
                  E = 0.
                  FUNC = ONEMP3*F
                  DEBY = 0.
               END IF
               PROFIL = PROFIL + WT*FUNC
               DFBY = WT*ONEMP3*F**2
               DBYX0 = 2.*P1XSQ(IX)
               DBYY0 = 2.*P2YSQ
               DHDXC = DHDXC + 
     .              DEBY*(DBYX0 + DY*PAR(4)) + DFBY*DBYX0
               DHDYC = DHDYC +
     .              DEBY*(DBYY0 + DX*PAR(4)) + DFBY*DBYY0
               IF (IDERIV .GT. 0) THEN
                  DBYX0 = DBYX0*DX/PAR(1)
                  DBYY0 = DBYY0*DY/PAR(2)
                  TERM(1) = TERM(1) + (DFBY+DEBY)*DBYX0
                  TERM(2) = TERM(2) + (DFBY+DEBY)*DBYY0
                  TERM(3) = TERM(3) + WT*(E-F)
                  TERM(4) = TERM(4) - DEBY * XY
               END IF
            END DO
         END DO
      ELSE IF (IPSTYP .EQ. 6) THEN
C
C Penny function --- Gaussian core plus Lorentzian wings.
C The Lorentzian and Gaussian may be tilted in different
C directions.
C
         P1SQ = PAR(1)**2
         P2SQ = PAR(2)**2
         ONEMP3 = 1.-PAR(3)
         XY = DX*DY
C
         RSQ = DX**2/P1SQ + DY**2/P2SQ
         DFBY = RSQ + PAR(5)*XY
         IF (DFBY .GT. 1.E10) RETURN
         F = 1./(1.+DFBY)
C
         DEBY = RSQ + PAR(4)*XY
         IF (DEBY .LT. 34.) THEN
            E = EXP(-0.6931472*DEBY)
         ELSE
            E = 0.
         END IF
C
         FUNC = PAR(3)*E + ONEMP3*F
         IF (FUNC .GE. 0.046) THEN
            NPT = 4
         ELSE IF (FUNC .GE. 0.0022) THEN
            NPT = 3
         ELSE IF (FUNC .GE. 0.0001) THEN
            NPT = 2
         ELSE IF (FUNC .GE. 1.E-10) THEN
            PROFIL = FUNC
            DFBY = ONEMP3*F**2
            DEBY = 0.6931472*PAR(3)*E
            DBYX0 = 2.*DX/P1SQ
            DBYY0 = 2.*DY/P2SQ
            DHDXC = DEBY*(DBYX0 + DY*PAR(4)) + 
     .              DFBY*(DBYX0 + DY*PAR(5))
            DHDYC = DEBY*(DBYY0 + DX*PAR(4)) + 
     .              DFBY*(DBYY0 + DX*PAR(5))
            IF (IDERIV .GT. 0) THEN
               DBYX0 = DBYX0*DX/PAR(1)
               DBYY0 = DBYY0*DY/PAR(2)
               TERM(5) = -DFBY * XY
               DFBY = DFBY + DEBY
               TERM(1) = DFBY * DBYX0
               TERM(2) = DFBY * DBYY0
               TERM(3) = E - F
               TERM(4) = - DEBY * XY
            END IF
            RETURN
         ELSE
            RETURN
         END IF
C
         DO IX=1,NPT
            X(IX) = DX+D(IX,NPT)
            P1XSQ(IX) = X(IX)/P1SQ
         END DO
C
         DO IY=1,NPT
            Y = DY+D(IY,NPT)
            P2YSQ = Y/P2SQ
            DO IX=1,NPT
               WT = W(IY,NPT)*W(IX,NPT)
               XY = X(IX)*Y
               RSQ = P1XSQ(IX)*X(IX) + P2YSQ*Y
               F = 1./(1.+RSQ+PAR(5)*XY)
               DEBY = RSQ + PAR(4)*XY
               IF (DEBY .LT. 34.) THEN
                  E = EXP(-0.6931472*DEBY)
                  FUNC = PAR(3)*E + ONEMP3*F
                  DEBY = 0.6931472*WT*PAR(3)*E
               ELSE
                  E = 0.
                  FUNC = ONEMP3*F
                  DEBY = 0.
               END IF
               PROFIL = PROFIL + WT*FUNC
               DFBY = WT*ONEMP3*F**2
               DBYX0 = 2.*P1XSQ(IX)
               DBYY0 = 2.*P2YSQ
               DHDXC = DHDXC + 
     .              DEBY*(DBYX0 + DY*PAR(4)) + 
     .              DFBY*(DBYX0 + DY*PAR(5))
               DHDYC = DHDYC +
     .              DEBY*(DBYY0 + DX*PAR(4)) + 
     .              DFBY*(DBYY0 + DX*PAR(5))
               IF (IDERIV .GT. 0) THEN
                  DBYX0 = DBYX0*DX/PAR(1)
                  DBYY0 = DBYY0*DY/PAR(2)
                  TERM(1) = TERM(1) + (DFBY+DEBY)*DBYX0
                  TERM(2) = TERM(2) + (DFBY+DEBY)*DBYY0
                  TERM(3) = TERM(3) + WT*(E-F)
                  TERM(4) = TERM(4) - DEBY * XY
                  TERM(5) = TERM(5) - DFBY * XY
               END IF
            END DO
         END DO
      ELSE IF (IPSTYP .EQ. 2) THEN
C
C MOFFAT FUNCTION   BETA = 1.5
C
C                            BETA-1
C F = --------------------------------------------------------
C      Ax * Ay * [1 + (X/Ax)**2 + (Y/Ay)**2 + (XY*Axy)]**BETA
C
C PAR(1) is the HWHM in x at y = 0: 
C
C             1/2 = 1/[1 + (PAR(1)/Ax)**2]**BETA
C so
C             2**(1/BETA) - 1 = (PAR(1)/Ax)**2
C
C             Ax**2 = PAR(1)**2/[2**(1/BETA) - 1]
C
C When BETA = 1.5, Ax**2 = 1.7024144 * PAR(1)**2
C
C Hence, let us use
C
C                                  1
C F = ---------------------------------------------------------------
C     P(1)*P(2)*{1+0.5874011*[(X/P(1))**2+(Y/P(2))**2+(XY*P(3))]**1.5
C 
C neglecting a constant of proportionality.
C
         ALPHA = 0.5874011
         TALPHA = 1.1748021  ! 2.*ALPHA
         P1SQ = PAR(1)**2
         P2SQ = PAR(2)**2
         P1P2 = PAR(1)*PAR(2)
         XY = DX*DY
C
         DENOM = 1. + ALPHA*(DX**2/P1SQ + DY**2/P2SQ + XY*PAR(3))
         IF (DENOM .GT. 5.E6) RETURN
         FUNC = 1. / (P1P2 * DENOM**PAR(4))
         IF (FUNC .GE. 0.046) THEN
            NPT = 4
         ELSE IF (FUNC .GE. 0.0022) THEN
            NPT = 3
         ELSE IF (FUNC .GE. 0.0001) THEN
            NPT = 2
         ELSE IF (FUNC .GE. 1.E-10) THEN
            PROFIL = (PAR(4) - 1.) * FUNC
            P4FOD = PAR(4)*ALPHA*PROFIL/DENOM
            DHDXC = P4FOD*(2.*DX/P1SQ + DY*PAR(3))
            DHDYC = P4FOD*(2.*DY/P2SQ + DX*PAR(3))
            IF (IDERIV .GT. 0) THEN
               TERM(1) = (2.*P4FOD*DX**2/P1SQ-PROFIL)/PAR(1)
               TERM(2) = (2.*P4FOD*DY**2/P2SQ-PROFIL)/PAR(2)
               TERM(3) = - P4FOD*XY
C              TERM(4) = PROFIL*(1./(PAR(4)-1.)-ALOG(DENOM))
            END IF
            RETURN
         ELSE
            RETURN
         END IF
C
         DO IX=1,NPT
            X(IX) = DX+D(IX,NPT)
            XSQ(IX) = X(IX)**2
            P1XSQ(IX) = XSQ(IX)/P1SQ
         END DO
C
         DO IY=1,NPT
            Y = DY+D(IY,NPT)
            YSQ = Y**2
            P2YSQ = YSQ/P2SQ
            DO IX=1,NPT
               WT = W(IY,NPT)*W(IX,NPT)
               XY=X(IX)*Y
               DENOM = 1. + ALPHA*(P1XSQ(IX) + P2YSQ + XY*PAR(3))
               FUNC = (PAR(4) - 1.)/ (P1P2 * DENOM**PAR(4))
               P4FOD = PAR(4)*ALPHA*FUNC/DENOM
               WP4FOD = WT*P4FOD
               WF = WT*FUNC
               PROFIL = PROFIL + WF
               DHDXC = DHDXC + WP4FOD*(2.*X(IX)/P1SQ + Y*PAR(3))
               DHDYC = DHDYC + WP4FOD*(2.*Y/P2SQ + X(IX)*PAR(3))
               IF (IDERIV .GT. 0) THEN
                  TERM(1) = TERM(1) + 
     .                     (2.*WP4FOD*P1XSQ(IX)-WF)/PAR(1)
                  TERM(2) = TERM(2) + 
     .                     (2.*WP4FOD*P2YSQ-WF)/PAR(2)
                  TERM(3) = TERM(3) - WP4FOD*XY
C                 TERM(4) = TERM(4) + WF*(1./(PAR(4)-1.)-ALOG(DENOM))
               END IF
            END DO
         END DO
      ELSE IF (IPSTYP .EQ. 4) THEN
C
C LORENTZ FUNCTION
C                      1
C F = --------------------------------------
C     [1 + (X/Ax)**2 + (Y/Ay)**2 + (XY*Axy)]
C
C PAR(1) is the HWHM in x at y = 0.
C
         P1SQ = PAR(1)**2
         P2SQ = PAR(2)**2
         P1P2 = PAR(1)*PAR(2)
         XY = DX*DY
C
         DENOM = 1. + DX**2/P1SQ + DY**2/P2SQ + XY*PAR(3)
         IF (DENOM .GT. 1.E10) RETURN
         FUNC = 1. / DENOM
         IF (FUNC .GE. 0.046) THEN
            NPT = 4
         ELSE IF (FUNC .GE. 0.0022) THEN
            NPT = 3
         ELSE IF (FUNC .GE. 0.0001) THEN
            NPT = 2
         ELSE IF (FUNC .GE. 1.E-10) THEN
            PROFIL = FUNC
            WFSQ = FUNC**2
            DHDXC = WFSQ*(2.*DX/P1SQ + DY*PAR(3))
            DHDYC = WFSQ*(2.*DY/P2SQ + DX*PAR(3))
            IF (IDERIV .GT. 0) THEN
               TERM(1) = WFSQ*(2.*DX**2/P1SQ)/PAR(1)
               TERM(2) = WFSQ*(2.*DY**2/P2SQ)/PAR(2)
               TERM(3) = - WFSQ*XY
            END IF
            RETURN
         ELSE
            RETURN
         END IF
C
         DO IX=1,NPT
            X(IX) = DX+D(IX,NPT)
            XSQ(IX) = X(IX)**2
            P1XSQ(IX) = XSQ(IX)/P1SQ
         END DO
C
         DO IY=1,NPT
            Y = DY+D(IY,NPT)
            YSQ = Y**2
            P2YSQ = YSQ/P2SQ
            DO IX=1,NPT
               WT = W(IY,NPT)*W(IX,NPT)
               XY=X(IX)*Y
               DENOM = 1. + P1XSQ(IX) + P2YSQ + XY*PAR(3)
               FUNC = 1. / DENOM
               WF = WT*FUNC
               WFSQ = WF*FUNC
               PROFIL = PROFIL + WF
               DHDXC = DHDXC + WFSQ*(2.*X(IX)/P1SQ + Y*PAR(3))
               DHDYC = DHDYC + WFSQ*(2.*Y/P2SQ + X(IX)*PAR(3))
               IF (IDERIV .GT. 0) THEN
                  TERM(1) = TERM(1) + WFSQ*(2.*P1XSQ(IX))/PAR(1)
                  TERM(2) = TERM(2) + WFSQ*(2.*P2YSQ)/PAR(2)
                  TERM(3) = TERM(3) - WFSQ*XY
               END IF
            END DO
         END DO
      ELSE
         write (6, '(a)') ('Invalid PSF type.')
         stop
      END IF
      RETURN
      END!
C
C#######################################################################
C
      REAL  FUNCTION  DAOERF (XIN, XO, BETA, DFDXO, DFDBET)
C
C Numerically integrate a Gaussian function 
C
C          F = EXP {-0.5*[(x-XO)/SIGMA]**2 },
C
C from XIN-0.5 to XIN+0.5 using Gauss-Legendre integration.  BETA
C is the half-width at half-maximum, which is equal to 1.17741 * SIGMA.
C Thus,
C
C          F = EXP {-0.6931472*[(x-XO)/BETA]**2 }.
C
C Also: provide the first derivative of the integral with respect to 
C Xo and BETA.  Use Gauss-Legendre integration.
C
C-----------------------------------------------------------------------
C
      IMPLICIT NONE
      INTEGER MAXPT
      PARAMETER (MAXPT=4)
C
      REAL DX(MAXPT,MAXPT), WT(MAXPT,MAXPT)
C
      REAL EXP
C
      REAL X, XSQ
      REAL XIN, XO, BETA, DFDXO, DFDBET, BETASQ, DELTAX, F, WF
      INTEGER NPT, I
C
      DATA DX / 0.00000000,  0.0,        0.0       , 0.0       ,
     .         -0.28867513,  0.28867513, 0.0       , 0.0       ,
     .         -0.38729833,  0.00000000, 0.38729833, 0.0       ,
     .         -0.43056816, -0.16999052, 0.16999052, 0.43056816/
      DATA WT / 1.00000000,  0.0       , 0.0       , 0.0       ,
     .          0.50000000,  0.50000000, 0.0       , 0.0       ,
     .          0.27777778,  0.44444444, 0.27777778, 0.0       ,
     .          0.17392742,  0.32607258, 0.32607258, 0.17392742/
      DAOERF = 0.
      DFDXO = 0.
      DFDBET = 0.
      BETASQ=BETA**2
      DELTAX = XIN-XO
C
      XSQ = DELTAX**2
      F = XSQ/BETASQ
      IF (F .GT. 34.) RETURN
      F = EXP(-0.6931472*F)
      IF (F .GE. 0.046) THEN
         NPT = 4
      ELSE IF (F .GE. 0.0022) THEN
         NPT = 3
      ELSE IF (F .GE. 0.0001) THEN
         NPT = 2
      ELSE IF (F .GE. 1.E-10) THEN
         DAOERF = F
         DFDXO = 1.3862944 * DELTAX * F / BETASQ
         DFDBET = 1.3862944 * XSQ * F / (BETASQ*BETA)
         RETURN
      ELSE
         RETURN
      END IF
C
      DO I=1,NPT
         X = DELTAX + DX(I,NPT)
         XSQ = X**2
         F = EXP(-0.6931472*XSQ/BETASQ)
         WF = WT(I,NPT)*F
         DAOERF = DAOERF+WF
         DFDXO = DFDXO + X*WF
         DFDBET = DFDBET + XSQ*WF
      END DO
      DFDXO = 1.3862944*DFDXO/BETASQ
      DFDBET = 1.3862944*DFDBET/(BETASQ*BETA)
C
      RETURN
      END!
C
C#######################################################################
C
      REAL  FUNCTION  BICUBC  (F, NBOX, DX, DY, DFDX, DFDY)
C
C Perform a type of bicubic interpolation in a grid of values.
C For a point located DX, DY distant from the corner of the grid
C (defined to be 1,1), return both the interpolated value and
C its first derivatives with respect to x and y.
C
      IMPLICIT NONE
      INTEGER NBOX
      REAL F(NBOX,NBOX), TEMP(4), DFDXT(4)
C
      REAL DX, DY, DFDX, DFDY, C1, C2, C3, C4
      INTEGER JY
C
C By construction, the point at which we want to estimate the function
C will lie between the second and third columns, and between the second
C and third rows of F, at a distance of (DX,DY) from the (2,2) element
C of F.
C
      DO JY=1,4
         C1 = 0.5*(F(3,JY)-F(1,JY))
         C4 = F(3,JY) - F(2,JY) - C1
         C2 = 3.*C4 - 0.5*(F(4,JY)-F(2,JY)) + C1
         C3 = C4 - C2
         C4 = DX*C3
         TEMP(JY) = DX*(DX*(C4+C2)+C1)+F(2,JY)
         DFDXT(JY)= DX*(C4*3.+2.*C2)+C1
      END DO
      C1 = 0.5*(TEMP(3)-TEMP(1))
      C4 = TEMP(3) - TEMP(2) - C1
      C2 = 3.*C4 - 0.5*(TEMP(4)-TEMP(2)) + C1
      C3 = C4 - C2
      C4 = DY*C3
      BICUBC = DY*(DY*(C4+C2)+C1)+TEMP(2)
      DFDY = DY*(C4*3.+2.*C2)+C1
      C1 = 0.5*(DFDXT(3)-DFDXT(1))
      C4 = DFDXT(3) - DFDXT(2) - C1
      C2 = 3.*C4 - 0.5*(DFDXT(4)-DFDXT(2)) + C1
      C3 = C4 - C2
      DFDX = DY*(DY*(DY*C3+C2)+C1)+DFDXT(2)
      RETURN
      END!
C
C#######################################################################
C
      REAL FUNCTION PCTILE(DATUM,N,NPCT)
C
C=======================================================================
C
C This is a modification of a quick-sorting algorithm, which is intended
C to take in a vector of numbers, and return the value of the PCT-th
C percentile in that vector:
C
C    DATUM (input real vector)     containing real data.
C    N     (input integer)         number of elements in DATUM.
C    NPCT  (input integer)         element of sorted vector whose value 
C                                  is desired.
C    PCTILE (output real)          the value of the NPCT-th element
C                                  in the sorted vector DATUM.
C
C-----------------------------------------------------------------------
C
C The quick-sorting algorithm was suggested by the discussion on pages 
C 114-119 of THE ART OF COMPUTER PROGRAMMING, Vol. 3, SORTING AND 
C SEARCHING, by D.E. Knuth, which was referenced in Don Wells' 
C subroutine QUIK.  This is my own attempt at encoding a quicksort-- 
C                                                             PBS.
C
C The array DATUM contains randomly ordered data. 
C
      IMPLICIT NONE
      REAL DATUM(*)
C
      INTEGER MIN0, MAX0
C
      REAL DKEY
      INTEGER LO, HI, N, NPCT, LIMLO, LIMHI
C
C Which element of the sorted array will we be interested in?
C
      NPCT=MAX0(1,MIN0(N,NPCT))
C
C Initialize the pointers.
C
      LIMLO=1
      LIMHI=N
C
  100 DKEY=DATUM(LIMLO)
CD     TYPE *,'LOW=',LIMLO,' HIGH=',LIMHI,' KEY=',DKEY
C
C Compare all elements in the sub-vector between LIMLO and LIMHI with
C the current key datum.
C
      LO=LIMLO
      HI=LIMHI
  101 CONTINUE
C
C If LO equals HI, we have tested all the elements in the current search
C interval.
C
      IF(LO.EQ.HI)GO TO 200
      IF(DATUM(HI).LE.DKEY)GO TO 109
      HI=HI-1
C
C The pointer HI is to be left pointing at a datum SMALLER than the
C key, which is intended to be overwritten.
C
      GO TO 101
C
  109 DATUM(LO)=DATUM(HI)
      LO=LO+1
  110 CONTINUE
      IF(LO.EQ.HI)GO TO 200
      IF(DATUM(LO).GE.DKEY)GO TO 119
      LO=LO+1
      GO TO 110
C
  119 DATUM(HI)=DATUM(LO)
      HI=HI-1
C
C The pointer LO is to be left pointing at a datum LARGER than the
C key, which is intended to be overwritten.
C
      GO TO 101
C
  200 CONTINUE
C
C LO and HI are equal, and point at a value which is intended to
C be overwritten.  Since all values below this point are less than
C the key and all values above this point are greater than the key,
C this is where we stick the key back into the vector.
C
      DATUM(LO)=DKEY
CD     DO 1666 I=LIMLO,LO-1
CD1666 TYPE *,DATUM(I)
CD     TYPE *,DATUM(LO),' KEY'
CD     DO 2666 I=LO+1,LIMHI
CD2666 TYPE *,DATUM(I)
C
C At this point in the subroutine, all data between LIMLO and LO-1, 
C inclusive, are less than DATUM(LO), and all data between LO+1 and 
C LIMHI are larger than DATUM(LO).  If LO = NPCT, then DATUM(LO) is
C the value we are looking for.  If NPCT < LO, then we want to sort the
C values of DATUM from LIMLO to LO-1, inclusive, whereas if NPCT > LO,
C then we want to sort the values of DATUM from LO+1 to LIMHI, 
C inclusive.
C
CD     TYPE *,'NPCT=',NPCT,' LO=',LO
      IF(NPCT-LO)300,900,400
  300 LIMHI=LO-1
      GO TO 100
  400 LIMLO=LO+1
      GO TO 100
  900 PCTILE=DATUM(LO)
      RETURN
      END!
