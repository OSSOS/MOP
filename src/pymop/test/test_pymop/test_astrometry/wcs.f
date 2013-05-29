      subroutine xy2sky(crval1,crval2,crpix1,crpix2,cd,pv,nord,
     &  xn,yn,ra,dec)
* a complicated transformation
      implicit double precision (a-h,o-z)
      double precision cd(2,2),pv(2,0:10)
      integer nord
      pi180=   57.2957795130823208767981548141052d0
      xp=xn-CRPIX1
      yp=yn-CRPIX2

      x = cd(1,1) * xp + cd(1,2) * yp
      y = cd(2,1) * xp + cd(2,2) * yp

      if (nord.lt.0) then
         xi = x
         eta= y
      endif

      if (nord.ge.0) then
         xi  = PV(1,0)
         eta = PV(2,0)
      endif
      if (nord.ge.1) then
         r=sqrt(x**2 + y**2)
         xi  = xi  + PV(1,1)*x + PV(1,2)*y + PV(1,3)*r
         eta = eta + PV(2,1)*y + PV(2,2)*x + PV(2,3)*r
      endif
      if (nord.ge.2) then
         x2 = x**2
         xy = x*y
         y2 = y**2
         xi  = xi  + PV(1,4)*x2 + PV(1,5)*xy + PV(1,6)*y2
         eta = eta + PV(2,4)*y2 + PV(2,5)*xy + PV(2,6)*x2
      endif
      if (nord.ge.3) then
         x3  = x**3
         x2y = x2*y
         xy2 = x*y2
         y3  = y**3
         xi  = xi  + PV(1,7)*x3 + PV(1,8)*x2y + PV(1,9)*xy2
     &        + PV(1,10)*y3
         eta = eta + PV(2,7)*y3 + PV(2,8)*xy2 + PV(2,9)*x2y
     &        + PV(2,10)*x3
      endif

      xir=xi/pi180
      etar=eta/pi180

      ra0=CRVAL1/pi180
      dec0=CRVAL2/pi180

      ctan=tan(dec0)
      ccos=cos(dec0)
      raoff=atan2 (xir / ccos, 1d0 - etar * ctan)
      ra=raoff+ra0
      dec = atan(cos (raoff) / ((1d0 - (etar * ctan)) / (etar + ctan)))

      ra=ra*pi180
      if (ra.lt.0d0) ra=ra+360d0
      if (ra.ge.360d0) ra=ra-360d0 ! probably not necessary
      dec=dec*pi180

      return
      end

*****************

      subroutine sky2xy(crval1,crval2,crpix1,crpix2,dc,pv,nord,
     &  ran,decn,xout,yout)
      implicit double precision (a-h,o-z)
      double precision cd(2,2),pv(2,0:10)
      double precision dc(2,2)
      integer nord
      pi180=   57.2957795130823208767981548141052d0

*      write(*,'(2f15.8,2f10.3)') ran,decn,crval1,crval2
      if (abs(ra-crval1).gt.100) then
         !write (*,'(2f12.6,$)') ra,crval1
         if (crval1.lt.180) then
            ra=ra-360d0
         else
            ra=ra+360d0
         endif
         !write (*,'(2f12.6)') ra
      endif

      ra=ran/pi180
      dec=decn/pi180
      tdec=tan(dec)
      ra0  = CRVAL1/pi180
      dec0 = CRVAL2/pi180
      ctan = tan(dec0)
      ccos = cos (dec0)

      traoff = tan (ra - ra0)
      craoff = cos (ra - ra0)
      etar = (1d0 - ctan * craoff / tdec) / (ctan + (craoff / tdec))
      xir = traoff * ccos * (1d0 - (etar * ctan))
      xi = xir * pi180
      eta = etar * pi180

      if (nord.lt.0) then            ! the simple solution
         x=xi
         y=eta
      else                            ! reverse by Newton's method
         tolerance=0.001/3600.        ! 0.001 arcsec
         x = xi
         y = eta  ! intial guess
         iter=0
 100  iter=iter+1
      if (nord.ge.0) then
         f = PV(1,0)                     ! estimates
         g = PV(2,0)
         fx=0d0                          ! derivatives
         fy=0d0
         gx=0d0
         gy=0d0
      endif
      if (nord.ge.1) then
         r=sqrt(x**2 + y**2)
         f = f + PV(1,1)*x + PV(1,2)*y + PV(1,3)*r
         g = g + PV(2,1)*y + PV(2,2)*x + PV(2,3)*r
         fx=fx + PV(1,1)               + PV(1,3)*x/r
         fy=fy             + PV(1,2)   + PV(1,3)*y/r
         gx=gx             + PV(2,2)   + PV(2,3)*x/r
         gy=gy + PV(2,1)               + PV(2,3)*y/r
      endif
      if (nord.ge.2) then
         x2 = x**2
         xy = x*y
         y2 = y**2
         f = f + PV(1,4)*x2    + PV(1,5)*xy + PV(1,6)*y2
         g = g + PV(2,4)*y2    + PV(2,5)*xy + PV(2,6)*x2
         fx=fx + PV(1,4)*2d0*x + PV(1,5)*y
         fy=fy                 + PV(1,5)*x  + PV(1,6)*2d0*y
         gx=gx                 + PV(2,5)*y  + PV(2,6)*2d0*x
         gy=gy + PV(2,4)*2d0*y + PV(2,5)*x
      endif
      if (nord.ge.3) then
         x3  = x**3
         x2y = x2*y
         xy2 = x*y2
         y3  = y**3
         f = f + PV(1,7)*x3     + PV(1,8)*x2y    + PV(1,9)*xy2
     &        + PV(1,10)*y3
         g = g + PV(2,7)*y3     + PV(2,8)*xy2    + PV(2,9)*x2y
     &        + PV(2,10)*x3
         fx=fx + PV(1,7)*3d0*x2 + PV(1,8)*2d0*xy + PV(1,9)*y2
         fy=fy                  + PV(1,8)*x2     + PV(1,9)*2d0*xy
     &        + PV(1,10)*3d0*y2
         gx=gx                  + PV(1,8)*y2     + PV(2,9)*2d0*xy
     &        + PV(2,10)*3d0*x2
         gy=gy + PV(2,7)*3d0*y2 + PV(1,8)*2d0*xy + PV(2,9)*x2
      endif
      !write(*,'(i6,6f15.10)') iter,f,xi,g,eta,dx,dy

      f = f - xi
      g = g - eta
	dx = ((-f * gy) + (g * fy)) / ((fx * gy) - (fy * gx))
	dy = ((-g * fx) + (f * gx)) / ((fx * gy) - (fy * gx))
	x = x + dx
	y = y + dy


        if (abs(dx).gt.tolerance) goto 100
        if (abs(dy).gt.tolerance) goto 100
        if (iter.gt.300) then
           print*,"ERROR: Too many iterations in sky2xy"
           stop
        endif
      endif


      xp = dc(1,1)*x + dc(1,2) * y
      yp = dc(2,1)*x + dc(2,2) * y

      xout=xp+CRPIX1
      yout=yp+CRPIX2

      return
      end