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