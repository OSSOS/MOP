C -*-compile-command: "make_libpsf"; -*-
c
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
c This routine estimates the sky background from a histogram of pixel
c values. It uses an estimate of the result to setup the histogram.
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

      subroutine mean_sky (image, nx, ny, m_sky, e_sky)

      implicit none

      integer*4
     $  n_hist_max

      parameter
     $  (n_hist_max = 10000)

      integer*4
     $  nx, ny, i, im_size, im_step, j, hist(n_hist_max), n_hist, n_d,
     $  n_c, i_max, max_hist, thresh

      real*4
     $  image(*), m_sky, e_sky, p_low, p_high, p_step, mean_hist,
     $  height

      real*8
     $  m_sky8, xp(n_hist_max), yp(n_hist_max), sig(n_hist_max),
     $  coeff(10)

      external
     $  monomes

c Define steps, ...

      im_size = nx*ny
      p_low = e_sky*0.25
      p_high = e_sky*2.0
      p_step = float(nint(sqrt(e_sky)/10.))
      n_hist = int((p_high - p_low)/p_step) + 1
      if (n_hist .gt. n_hist_max) then
         n_hist = n_hist_max
         p_high = float(n_hist - 1)*p_step + p_low
      end if
      n_d = 21
      if (n_hist .lt. n_d) then
         n_hist = n_d
         p_step = float(int((p_high - p_low)/float(n_hist - 1) + 1))
      end if
      im_step = im_size/1000000 + 1

c Compute histogram

      do i = 1, n_hist
         hist(i) = 0
      end do

      do i = 1, im_size, im_step
         if ((image(i) .ge. p_low) .and. (image(i) .le. p_high)) then
            j = int((image(i) - p_low)/p_step) + 1
            hist(j) = hist(j) + 1
         end if
      end do

c Find maximum of histogram, and mean value of hist(i)

      i_max = 0
      max_hist = 0
      j = 0
      do i = 1, n_hist
         j = j + hist(i)
         if (hist(i) .gt. max_hist) then
            i_max = i
            max_hist = hist(i)
         end if
      end do
      mean_hist = float(j)/float(n_hist)

c Try to fit a parabola through the maximum of the histogram.
c First, extract the vicinity of the maximum

      height = float(max_hist)
      thresh = nint(mean_hist + (height - mean_hist)/2.)

      j = 0
      do i = i_max-n_d/2, i_max+n_d/2
         if (hist(i) .gt. thresh) then
            j = j + 1
            xp(j) = dble(i)
            yp(j) = dble(hist(i))
            sig(j) = 1.d0
         end if
      end do
      n_d = j

c Now fit the parabola

      n_c = 3
      call fit_1d (xp, yp, sig, n_d, coeff, n_c, monomes)

c Finally estimate the location of the maximum

      m_sky8 = -coeff(2)/(2.d0*coeff(3))
      m_sky = (sngl(m_sky8) - 1.)*p_step + p_low

      return
      end

c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
c This subroutine computes the mode, standard deviation, and skewness
c of the peak in the sky histogram.
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

      SUBROUTINE  MMM (SKY, NSKY, HIBAD, SKYMN, SKYMED, 
     .     SKYMOD, SIGMA, SKEW)
C
C=======================================================================
C
C               Official DAO version:  1988 July 1
C
C This version of MMM (modified by PBS 1984.IV.10ff) assumes that
C the sky brightnesses in the one-dimensional array SKY are already
C sorted on entering this routine, and that pixels outside the "bad"
C limits have already been eliminated.
C
C This particular version of MMM also takes cognizance of the fact that,
C pixels falling below the LOBAD threshold already having been 
C eliminated, the contaminated sky pixels values overwhelmingly display
C POSITIVE departures from the true value.
C
C If for some reason it is impossible to obtain the mode of the sky
C distribution, this will be flagged by setting SIGMA = -1.0.
C
C Arguments
C
C     SKY (INPUT) is a real vector containing actual sorted sky values.
C    NSKY (INPUT) is the number of defined elements in SKY.
C  SKYMOD (OUTPUT) is the estimated mode of the sky values.
C   SIGMA (OUTPUT) is the computed standard deviation of the peak in
C         the sky histogram.
C    SKEW (OUTPUT) is the computed skewness of the peak in the sky
C         histogram.
C
C=======================================================================
C
      IMPLICIT NONE
      INTEGER NSKY
      REAL SKY(NSKY)
C
      DOUBLE PRECISION DSQRT, DBLE
      REAL ALOG10, AMIN1, AMAX1
C
      DOUBLE PRECISION SUM,SUMSQ
      REAL CUT, CUT1, CUT2, DELTA, SKYMID, SKYMED, SKYMN, SKYMOD
      REAL SIGMA, SKEW, R, SIGN, X, HIBAD, CENTER, SIDE
      REAL DMOD, OLD, CLAMP
      INTEGER I, MINIMM, MAXIMM, NITER, ISTEP, MAXIT, MINSKY, JSTEP
      LOGICAL REDO
      DATA MAXIT / 30 /, MINSKY / 20 /
C
C-----------------------------------------------------------------------
C
C SECTION 1
C
      IF (NSKY .LE. 0) THEN
         GO TO 9900
      END IF
      SKYMID=0.5*(SKY((NSKY+1)/2)+SKY(NSKY/2+1))
C
C SKYMID is the median value for the whole ensemble of sky pixels.
C Notice that if NSKY is odd, then (NSKY+1)/2 and (NSKY/2)+1 will be the
C same number, whereas if NSKY is even, they will be different numbers.
C This same trick will be used again later.
C
C Initialize the variables for accumulating the mean and standard
C deviation, and initialize the rejection limits.
C
      SUM=0.D0
      SUMSQ=0.D0
      CUT1=AMIN1(SKYMID-SKY(1), SKY(NSKY)-SKYMID, HIBAD-SKYMID)
C
C For the first pass we will consider only pixels in a symmetric 
C interval of brightness values about the median value.  This exploits
C the assumption that all the bad pixels are already rejected from the
C lower end of the brightness range.
C
      CUT2=SKYMID + CUT1
      CUT1=SKYMID - CUT1
C
      MINIMM=0
      DO 1010 I=1,NSKY
         IF (SKY(I) .LT. CUT1) THEN
            MINIMM=I
            GO TO 1010
         END IF
         IF (SKY(I) .GT. CUT2) GO TO 1020
         DELTA=SKY(I)-SKYMID
         SUM=SUM+DELTA
         SUMSQ=SUMSQ+DELTA**2
         MAXIMM=I
 1010 CONTINUE
C
C Henceforth in this subroutine, MINIMM will point to the highest value
C rejected at the lower end of the vector, and MAXIMM will point to the
C highest value accepted at the upper end of the vector.
C MAXIMM-MINIMM is the number of pixels within the acceptance range.
C
C Compute mean and sigma (from the first pass).
C
 1020 CONTINUE
      SKYMED=0.5*(SKY((MINIMM+MAXIMM+1)/2)+SKY((MINIMM+MAXIMM)/2+1))
      SKYMN=SUM/DBLE(MAXIMM-MINIMM)
      SIGMA=DSQRT(SUMSQ/DBLE(MAXIMM-MINIMM)-SKYMN**2)
      SKYMN=SKYMN+SKYMID
C
C The middle sky value, SKYMID, was subtracted off up above and added 
C back in down here to reduce the truncation error in the computation 
C of SIGMA.
C Note that this definition of SIGMA is incorrect by a factor of
C SQRT [NSKY/(NSKY-1.)], but for all but pathological cases (where none
C of this can be trusted anyway), it's close enough.
C
      SKYMOD=SKYMN
      IF (SKYMED .LT. SKYMN) SKYMOD=3.*SKYMED-2.*SKYMN
C
C If the mean is less than the mode, that means the contamination is
C slight, and the mean value is what we really want.  Note that this
C introduces a slight bias toward underestimating the sky when
C the scatter in the sky is caused by random fluctuations rather than
C by contamination, but I think this bias is negligible compared to the
C problem of contamination.
C
C-----------------------------------------------------------------------
C
C SECTION 2
C
C Rejection and recomputation loop:
C
      NITER=0
      OLD = 0.
      CLAMP = 1.
 2000 NITER=NITER+1
      IF ((NITER .GT. MAXIT) .OR. (MAXIMM-MINIMM .LT. MINSKY)) THEN
         GO TO 9900
      END IF
C
C Compute Chauvenet rejection criterion.
C         
      R=ALOG10(FLOAT(MAXIMM-MINIMM))
      R=AMAX1(2., (-.1042*R+1.1695)*R+.8895)
C
C Compute rejection limits (symmetric about the current mode).
C
      CUT=R*SIGMA+0.5*ABS(SKYMN-SKYMOD)
      CUT=AMAX1(1.5,CUT)
      CUT1=SKYMOD-CUT
      CUT2=SKYMOD+CUT
C
C Recompute mean and sigma by adding and/or subtracting sky values
C at both ends of the interval of acceptable values.
C
C At each end of the interval, ISTEP will show the direction we have to 
C step through the vector to go from the old partition to the new one.
C Pixels are added or subtracted depending upon whether the limit is 
C moving toward or away from the mode.
C
      REDO=.FALSE.
C
C Is CUT1 above or below the minimum currently-accepted value?
C
      ISTEP=INT(SIGN(1.0001, CUT1-SKY(MINIMM+1)))
      JSTEP=(ISTEP+1)/2
C
C If ISTEP = +1, JSTEP = 1.  If ISTEP = -1, JSTEP=0.  If ISTEP = +1, 
C then we know that at least one pixel must be deleted at the low end.
C
      IF (ISTEP .GT. 0) GO TO 2120
 2100 IF ((ISTEP .LT. 0) .AND. (MINIMM .LE. 0)) GO TO 2150
C
C Quit when SKY(MINIMM) < CUT1 <= SKY(MINIMM+1)
C
      IF ((SKY(MINIMM) .LE. CUT1) .AND. (SKY(MINIMM+1) .GE. CUT1))
     .     GO TO 2150
C
C If ISTEP is positive, subtract out the sky value at MINIMM+1; if 
C ISTEP is negative, add in the sky value at MINIMM.
C
 2120 CONTINUE
      DELTA=SKY(MINIMM+JSTEP)-SKYMID
      SUM=SUM-REAL(ISTEP)*DELTA
      SUMSQ=SUMSQ-REAL(ISTEP)*DELTA**2
      MINIMM=MINIMM+ISTEP
      REDO=.TRUE.                                 ! A change has occured
      GO TO 2100
C
 2150 CONTINUE
C
C Is CUT2 above or below the current maximum?
C
      ISTEP=INT(SIGN(1.0001, CUT2-SKY(MAXIMM)))
      JSTEP=(ISTEP+1)/2
C
C If ISTEP = +1, JSTEP = 1.  If ISTEP = -1, JSTEP=0.  If ISTEP = -1, 
C then we know that we must subtract at least one pixel from the high 
C end.
C
      IF (ISTEP .LT. 0) GO TO 2220
 2200 IF ((ISTEP .GT. 0) .AND. (MAXIMM .GE. NSKY)) GO TO 2250
C
C Quit when SKY(MAXIMM) <= CUT2 < SKY(MAXIMM+1)
C
      IF ((SKY(MAXIMM) .LE. CUT2) .AND. (SKY(MAXIMM+1) .GE. CUT2))
     .     GO TO 2250
C
C If ISTEP is positive, add in the sky value at MAXIMM+1; if ISTEP is 
C negative, subtract off the sky value at MAXIMM.
C
 2220 DELTA=SKY(MAXIMM+JSTEP)-SKYMID
      SUM=SUM+REAL(ISTEP)*DELTA
      SUMSQ=SUMSQ+REAL(ISTEP)*DELTA**2
      MAXIMM=MAXIMM+ISTEP
      REDO=.TRUE.                                 ! A change has occured
      GO TO 2200
C
 2250 CONTINUE
C
C Compute mean and sigma (from this pass).
C
      SKYMN=SUM/DBLE(MAXIMM-MINIMM)
      SIGMA=DSQRT(SUMSQ/DBLE(MAXIMM-MINIMM)-SKYMN**2)
      SKYMN=SKYMN+SKYMID
C
C Obtain the median.  To first approximation, the median would be the
C value of the sky in the middle pixel in the sorted data (if the
C total number is odd) or the mean of the two pixels straddling
C the middle (if the total number of pixels is even).
C
C     SKYMED=0.5*(SKY((MINIMM+MAXIMM+1)/2)+SKY((MINIMM+MAXIMM)/2+1))
C
C However, this is not good enough.  If you look at the estimator for
C the mode, you will note that a tiny change in the list of sky pixels,
C just sufficient to alter the median value of the sky brightness by
C one unit, will change the estimator of the mode by three units.  We
C really want something more robust than this.  As a first attempt
C at a more robust median estimator, I propose to estimate the median
C of the distribution by the mean of the central five percent of sky
C values.  This involves considerable care to make sure you get
C a perfectly symmetric sample of pixels about the median, whether
C there is an even or an odd number of pixels within the acceptance
C interval.
C
      SKYMED=0.0
      X=0.0
      CENTER = REAL(MINIMM+1 + MAXIMM)/2.
      SIDE = REAL(NINT(0.05*REAL(MAXIMM-MINIMM)))/2. + 0.25
C
      DO 2310 I=NINT(CENTER-SIDE),NINT(CENTER+SIDE)
      SKYMED=SKYMED+SKY(I)
 2310 X=X+1.
C
      SKYMED=SKYMED/X
      IF (SKYMED .LT. SKYMN) THEN
         DMOD=3.*SKYMED-2.*SKYMN - SKYMOD
      ELSE
         DMOD=SKYMN - SKYMOD
      END IF
C
C If the mean is less than the mode, that means the contamination is
C slight, and the mean value is what we really want.  Note that this
C introduces a slight bias toward underestimating the sky when
C the scatter in the sky is caused by random fluctuations rather than
C by contamination, but I think this bias is negligible compared to the
C problem of contamination.
C
C If the limits have not yet stopped moving, try again.
C
      IF (DMOD*OLD .LT. 0.) CLAMP = 0.5*CLAMP
      SKYMOD = SKYMOD + CLAMP*DMOD
      OLD = DMOD
      IF (REDO) GO TO 2000
C
C-----------------------------------------------------------------------
C
C Normal return.
C
      SKEW=(SKYMN-SKYMOD)/AMAX1(1., SIGMA)
      NSKY=MAXIMM-MINIMM
      RETURN
C
C-----------------------------------------------------------------------
C
C An error condition has been detected.
C
 9900 SIGMA=-1.0
      SKEW=0.0
      RETURN
C
      END!

c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
c This subroutine computes the the mean and rms of sky background,
c variance and skewness.
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

      subroutine mnsky (mean_sky, sig_sky, msig_sky, ssig_sky, m_skew,
     $  s_skew, s_sky, vs, sk, n_star, work)

      implicit none

      integer*4
     $  i, n_star

      real*4
     $  mean_sky, sig_sky, msig_sky, ssig_sky, m_skew, s_skew,
     $  s_sky(*), vs(*), sk(*), work(*)

c Check that there are actually some stars.

      if (n_star .le. 0) then
         write (6, *) 'No stars left at this point.'
         stop
      end if

c Evaluate mean and rms of sky.

      mean_sky = 0.
      sig_sky = 0.
      do i = 1, n_star
         mean_sky = mean_sky + s_sky(i)
         sig_sky = sig_sky + s_sky(i)**2
         work(i) = vs(i)
      end do
      mean_sky = mean_sky/float(n_star)
      sig_sky = sqrt(sig_sky/float(n_star) - mean_sky**2)

c Evaluate mean and rms of sky variance.

      call sortlr (n_star, work)
      msig_sky = 0.5*(work((n_star+1)/2) + work((n_star/2)+1))
      ssig_sky = amin1(msig_sky-work(1), work(n_star)-msig_sky)

c Evaluate mean and rms of sky skewness.

      do i = 1, n_star
         work(i) = sk(i)
      end do
      call sortlr (n_star, work)
      m_skew = 0.5*(work((n_star+1)/2) + work((n_star/2)+1))
      s_skew = amin1(m_skew-work(1), work(n_star)-m_skew)

      return
      end

c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
c This subroutine discards stars whose magnitude with aperture #na is
c not properly defined.
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

      subroutine clean_mag (idstar, x_br, y_br, s_sky, vs, sk, ap_mag,
     $  mag_err, n_star, n_ap, maxap, na)

      integer*4
     $  idstar(*), n_star, n_ap, na, i, n, MAXAP

      real*4
     $  x_br(*), y_br(*), s_sky(*), vs(*), sk(*), ap_mag(maxap,*),
     $  mag_err(maxap,*)

      n = 0
      do i = 1, n_star
         if (ap_mag(na,i) .le. 95.) then
            n = n + 1
            idstar(n) = idstar(i)
            x_br(n) = x_br(i)
            y_br(n) = y_br(i)
            s_sky(n) = s_sky(i)
            vs(n) = vs(i)
            sk(n) = sk(i)
            do j = 1, n_ap
               ap_mag(j,n) = ap_mag(j,i)
               mag_err(j,n) = mag_err(j,i)
            end do
         end if
      end do
      n_star = n

      return
      end

c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
c This subroutine selects stars that are more than a critical distance
c from the edge of the frame and having no brighter star within a
c critical distance.
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

      subroutine pckpsf (x, y, m, nstar, fitrad, psfrad, ncol, nrow,
     $  nbpsf, n_ap, maxap, work, indx)

      implicit none

      integer*4
     $  INDX(*), I, J, K, N, NREQ, NCOL, NROW, nbpsf,
     $  NSTAR, n_ap, MAXAP

      REAL*4
     $  X(*), Y(*), M(MAXAP,*), PSFRAD, FITRAD, RADSQ, DY, RADIUS,
     $  XYMIN, XMAX, YMAX, work(*)
C
      REAL ABS
C
      XYMIN = FITRAD + 1.
      XMAX = REAL(NCOL) - FITRAD
      YMAX = REAL(NROW) - FITRAD
      RADIUS = PSFRAD+FITRAD+2.
      RADSQ = RADIUS**2
C
C-----------------------------------------------------------------------
C
C SECTION 2
C
C Sort stars by magnitude.
C
      do i = 1, nstar
         work(i) = m(n_ap,i)
      end do
      call sortreal (nstar, work, indx)

c Do the selection

      nreq = 0
      do i = 1, nstar
         n = indx(i)
         if ((m(n_ap,n) .lt. 95.) .and. (x(n) .ge. xymin) .and.
     $     (x(n) .le. xmax) .and. (y(n) .ge. xymin) .and.
     $     (y(n) .le. ymax)) then
            do j = 1, i-1
               k = indx(j)
               dy = abs(y(k) - y(n))
               if (dy .lt. radius) then
                  dy = dy**2 + (x(k) - x(n))**2
                  if (dy .lt. radsq) then
                     m(n_ap,n) = 99.99
                     goto 2000
                  end if
               end if
            end do
            nreq = nreq + 1
 2000       continue
         else
            m(n_ap,n) = 99.99
         end if
         if (nreq .ge. nbpsf) goto 2100
      end do
 2100 continue

      RETURN
      END

c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
c This subroutine selects stars that have a well behaved sky annulus.
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

      subroutine pselect (ap_mag, s_sky, vs, sk, n_star, mn_sky,
     $  sig_sky, msig_sky, ssig_sky, m_skew, s_skew, thresh, n_ap,
     $  maxap, id)

      implicit none

      integer*4
     $  n_star, n_ap, maxap, i, id(*)

      real*4
     $  ap_mag(maxap,*), s_sky(*), vs(*), sk(*), mn_sky, sig_sky,
     $  msig_sky, ssig_sky, m_skew, s_skew, thresh, l_thresh

      l_thresh = thresh*sig_sky
      do i = 1, n_star
         if ((abs(s_sky(i) - mn_sky) .gt. l_thresh) .or.
     $     (abs(vs(i) - msig_sky) .gt. ssig_sky) .or.
     $     (abs(sk(i) - m_skew) .gt. s_skew)) then
            ap_mag(n_ap,i) = 99.99
         end if
      end do

      return
      end
