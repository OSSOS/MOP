      subroutine getwcs(filename,
     &     crval1,crval2,crpix1,crpix2,cd,dc,pv,nord)
      implicit none
      double precision pi180
      parameter(pi180=57.2957795130823208767981548141052d0)
      character*(*) filename
      double precision crval1,crval2,crpix1,crpix2
      double precision cd(2,2),dc(2,2),pv(2,0:10)
      integer nord
      integer ldot,lend,length
      integer reclen,bitpix,naxis,naxis1,naxis2
      logical verbose
      integer nsexmax
      double precision cfactor
      common /parblk/ verbose,nsexmax,cfactor
      integer nordgwyn,nordgwynp
      common /gwynblk/ nordgwynp
      integer i,j
      integer indexrev

      do 10 i=1,2
      do 10 j=0,10
 10      pv(i,j)=0d0

      nordgwyn=-10
      ldot=indexrev(filename,'.')
      lend=min(length(filename),ldot+4)
      if (ldot.lt.1) then
         write(*,*) 'WARNING: wierd looking filename: ',filename
      endif

      if (filename(ldot:lend).eq.'.fits') then
         call getbits(filename,reclen,bitpix,naxis,naxis1,naxis2)
         call getwcsfits(filename,reclen,
     &        crval1,crval2,crpix1,crpix2,cd,dc,pv,nord,nordgwyn)
      else
         call getwcshead(filename,
     &        crval1,crval2,crpix1,crpix2,cd,dc,pv,nord,nordgwyn)       
      endif
      if ((nordgwyn.gt.-10).and.(nordgwyn.ne.nord)) then
         write(*,*) 'ERROR: Problems reading header'
         write(*,*) 'nord     ',nord
         write(*,*) 'nordgwyn ',nordgwyn
         stop
      else
         nordgwynp=nordgwyn
      endif

      if (verbose) then
         call dumpwcs(crval1,crval2,crpix1,crpix2,cd,pv,nord)
      endif
      return
      end
********************


      subroutine getwcshead
     & (filename,crval1,crval2,crpix1,crpix2,cd,dc,pv,nord,nordgwyn)
      implicit double precision (a-h,o-z)
      parameter(pi180=57.2957795130823208767981548141052d0)
      character*(*) filename
      character*8 keyword
      character*8 keycheck
      character ci,cj
      character*70 value
      double precision cd(2,2),dc(2,2),pv(2,0:10)
      logical cdfound(2,2) /4*.false./
      character*8 ctype1,ctype2
      double precision cdelt1,cdelt2,crota1/0d0/,crota2/0d0/
      integer nord
      integer nordlev(0:10) /0,1,1,1,2,2,2,3,3,3,3/
      logical verbose
      integer nsexmax
      common /parblk/ verbose,nsexmax,cfactor
      integer nordgwyn
      nordgwyn=-10


        
     
      nord=-1
 5    open(1,file=filename,err=1,status='old')

 10   read(1,9999,end=100) keyword,value
      n=index(value,'/')-1
      if (n.lt.0) n=70
      value=value(1:n)
      if (keyword.eq.'GWYNFIT ') read(value,*) nordgwyn
      if (keyword.eq.'NORDFIT ') read(value,*) nordgwyn
      if (keyword.eq.'CRVAL1  ') read(value,*) crval1
      if (keyword.eq.'CRVAL2  ') read(value,*) crval2
      if (keyword.eq.'CRPIX1  ') read(value,*) crpix1
      if (keyword.eq.'CRPIX2  ') read(value,*) crpix2
      if (keyword.eq.'CDELT1  ') read(value,*) cdelt1
      if (keyword.eq.'CDELT2  ') read(value,*) cdelt2
      if (keyword.eq.'CROTA1  ') read(value,*) crota1
      if (keyword.eq.'CROTA2  ') read(value,*) crota2
      if (keyword.eq.'CTYPE1  ') read(value,*) ctype1
      if (keyword.eq.'CTYPE2  ') read(value,*) ctype2
      
      do 20 i=1,2
         do 20 j=1,2
            write(ci,'(i1)') i
            write(cj,'(i1)') j
            keycheck="CD"//ci//"_"//cj//"   "
            if (keyword.eq.keycheck) then
               read(value,*) cd(i,j)
               cdfound(i,j)=.true.
            endif
 20   continue
      do 30 i=1,2
         do 30 j=0,10
            write(ci,'(i1)') i
            write(cj,'(i1)') j
            keycheck="PV"//ci//"_"//cj//"   "
            if (j.eq.10) keycheck="PV"//ci//"_10  " !can you say kludge?
            if (keyword.eq.keycheck) then
               read(value,*) pv(i,j)
               nord=max(nord,nordlev(j))
            endif
 30   continue

      goto 10
 100  close(1)

      if ((ctype1.ne.'RA---TAN').or.(ctype2.ne.'DEC--TAN')) then
         write(*,*) 'WARNING: WCS type not TAN: ignoring'
      endif

      if (.not.(cdfound(1,1).and.cdfound(2,2))) then
         if (cdelt1*cdelt2.eq.0) then
             print*,'WARNING in getwcshead: 
     & not enough of the WCS is present in the header',
     &           filename
         else
            if (CROTA1.ne.CROTA2) write(*,'(a)') 
     &           "WARNING in getwcshead: ignoring skewed axes"
            
            cd(1,1)= CDELT1*cos(CROTA2/pi180)
            cd(1,2)=-CDELT2*sin(CROTA2/pi180)
            cd(2,2)= CDELT2*cos(CROTA2/pi180)
            cd(2,1)= CDELT1*sin(CROTA2/pi180)
         endif
      endif

* invert the matrix
      det = cd(1,1)*cd(2,2)-cd(1,2)*cd(2,1)
      
      dc(1,1)=cd(2,2)/det
      dc(2,2)=cd(1,1)/det
      dc(1,2)=-cd(1,2)/det
      dc(2,1)=-cd(2,1)/det

      crota1=pi180*atan2(-cd(1,2),cd(1,1))
      crota2=pi180*atan2( cd(2,1),cd(2,2))
*      print*,crota1,crota2

 9999 format(a8,2x,a70)
      return

 1    print*,'ERROR in getwcshead: ',filename,' does not exist'
      stop
      end
*****************

      subroutine getwcsfits(filename,reclen,
     &     crval1,crval2,crpix1,crpix2,cd,dc,pv,nord,nordgwyn)
      implicit double precision (a-h,o-z)
      parameter(pi180=57.2957795130823208767981548141052d0)
      character*(*) filename
      character*8 keyword
      character*80 line
      character*8 keycheck
      character ci,cj
      character*70 value
      double precision cd(2,2),dc(2,2),pv(2,0:10)
      logical cdfound(2,2) /4*.false./
      character*8 ctype1,ctype2
      double precision cdelt1,cdelt2,crota1,crota2
      integer nord
      integer nordlev(0:10) /0,1,1,1,2,2,2,3,3,3,3/
      integer reclen

      integer nordgwyn
* got some time? add some checks to see if actual
* values are being read...

      crota1=0d0
      crota2=0d0

      nordgwyn=-10
      nord=-1
      open(1,file=filename,status='old',
     &     access='direct',recl=reclen) !<---|
*** this number changes with system _____| 

      irec=1
 1000 read(1,rec=irec) line
*      print9998,line
      if (line(1:8).eq.'END     ') goto 2000

      read(line,9999) keyword,value
      n=index(value,'/')-1
      if (n.lt.0) n=70
      value=value(1:n)

      if (keyword.eq.'GWYNFIT ') read(value,*) nordgwyn
      if (keyword.eq.'NORDFIT ') read(value,*) nordgwyn
      if (keyword.eq.'CRVAL1  ') read(value,*) crval1
      if (keyword.eq.'CRVAL2  ') read(value,*) crval2
      if (keyword.eq.'CRPIX1  ') read(value,*) crpix1
      if (keyword.eq.'CRPIX2  ') read(value,*) crpix2
      if (keyword.eq.'CDELT1  ') read(value,*) cdelt1
      if (keyword.eq.'CDELT2  ') read(value,*) cdelt2
      if (keyword.eq.'CROTA1  ') read(value,*) crota1
      if (keyword.eq.'CROTA2  ') read(value,*) crota2
      if (keyword.eq.'CTYPE1  ') read(value,*) ctype1
      if (keyword.eq.'CTYPE2  ') read(value,*) ctype2

      do 20 i=1,2
         do 20 j=1,2
            write(ci,'(i1)') i
            write(cj,'(i1)') j
            keycheck="CD"//ci//"_"//cj//"   "
            if (keyword.eq.keycheck) then
               read(value,*) cd(i,j) 
               cdfound(i,j)=.true.
            endif
 20   continue
      do 30 i=1,2
         do 30 j=0,10
            write(ci,'(i1)') i
            write(cj,'(i1)') j
            keycheck="PV"//ci//"_"//cj//"   "
            if (j.eq.10) keycheck="PV"//ci//"_10  " !can you say kludge?
            if (keyword.eq.keycheck) then
               read(value,*) pv(i,j)
               nord=max(nord,nordlev(j))
            endif
 30   continue

      irec=irec+1
      goto 1000
 2000 close(1)

      if ((ctype1.ne.'RA---TAN').or.(ctype2.ne.'DEC--TAN')) then
         write(*,*) 'WARNING: WCS type not TAN: ignoring'
      endif

      if (.not.(cdfound(1,1).and.cdfound(2,2))) then
         if (cdelt1*cdelt2.eq.0d0) then
            print*,'not enough of the WCS is present in the header'
     &           ,filename
         else
            if (CROTA1.ne.CROTA2) write(*,'(a,2f8.3)')
     &           "warning: ignoring skewed axes",CROTA1,CROTA2
            cd(1,1)= CDELT1*cos(CROTA2/pi180)
            cd(1,2)=-CDELT2*sin(CROTA2/pi180)
            cd(2,2)= CDELT2*cos(CROTA2/pi180)
            cd(2,1)= CDELT1*sin(CROTA2/pi180)
         endif
      endif
      
* invert the matrix
      det = cd(1,1)*cd(2,2)-cd(1,2)*cd(2,1)
      
      dc(1,1)=cd(2,2)/det
      dc(2,2)=cd(1,1)/det
      dc(1,2)=-cd(1,2)/det
      dc(2,1)=-cd(2,1)/det

      crota1=pi180*atan2(-cd(1,2),cd(1,1))
      crota2=pi180*atan2( cd(2,1),cd(2,2))
*     print*,crota1,crota2
 9998 format(a80)
 9999 format(a8,2x,a70)

      return

 1    print*,'file ',filename,' does not exist'
      stop
      end

*****************

      subroutine dumpwcs(crval1,crval2,crpix1,crpix2,cd,pv,nord)
      implicit none
      double precision crval1,crval2,crpix1,crpix2
      double precision cd(2,2),pv(2,0:10)
      integer nord
      integer no(0:3) /0,3,6,10/
      integer i,j
      write(*,'("CRVALs",2f14.7)')crval1,crval2
      write(*,'("CRPIXs",2f14.2)')crpix1,crpix2
      write(*,'("CD:   ",1p,4(1x,e10.3))')
     &     cd(1,1),cd(1,2),cd(2,1),cd(2,2)
      if (nord.ge.0) write(*,'("order:"i2)') nord
      if (nord.gt.0) then
         write(*,'("PV1_: ",1p,11e10.2)') (pv(1,j),j=0,no(nord))
         write(*,'("PV2_: ",1p,11e10.2)') (pv(2,j),j=0,no(nord))
      endif
      write(*,*)
      return
      end
*****************************


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
*****************


      subroutine getbits(filename,reclen,
     &     bitpix,naxis,naxis1,naxis2)
      character*(*) filename
      character*80 line1,line2
      integer irectry(3) /80,40,20/
      integer reclen,bitpix
      character*8 keyword
      character*70 value
      naxis1=0
      naxis2=0
      do 10 i=1,3
         open(1,file=filename,status='old',
     &        access='direct',recl=irectry(i))
         
         read(1,rec=1) line1
         read(1,rec=2) line2
         if ( (line1(1:8).ne.'SIMPLE  ')
     $   .and.(line1(1:8).ne.'XTENSION')) then
            print*,'ERROR: Dubious FITS file: ',filename
            stop
         endif
         if (line2(1:8).eq.'BITPIX  ') then
            reclen=irectry(i)
            irec=0
 100        irec=irec+1
            call rec2kv(1,irec,keyword,value)
            if (keyword.eq.'BITPIX  ') read(value,*) bitpix
            if (keyword.eq.'NAXIS   ') read(value,*) naxis
            if (keyword.eq.'NAXIS1  ') read(value,*) naxis1
            if (keyword.eq.'NAXIS2  ') read(value,*) naxis2
            if (keyword.eq.'END     ') goto 20         
            goto 100
         endif
 10   close(1)
 20   close(1)
 9999 format(a8,2x,a70)

      return
      end
**************

      subroutine rec2kv(nunit,irec,key,value)
      integer nunit,irec
      character*80 line
      character*8 ikey
      character*70 ivalue
      character*(*) key,value
      read(nunit,rec=irec,err=1) line
      read(line,9999) ikey,ivalue
      n=index(ivalue,'/')-1
      if (n.lt.0) n=70
      key=ikey
      value=ivalue(1:n)
      return
 9999 format(a8,2x,a70)
 1    print*,'ERROR in rec2kv reading unit',nunit,' record',irec
      stop
      end
***************
      subroutine getword(nrec,nword)
      integer nrec,nword
      character*80 line
      character*8 keyword
      character*70 value
      read(1,rec=nrec) line
      read(line,9999) keyword,value
      read(value,*) nword
 9999 format(a8,2x,a70)
      return
      end
**************      
         
      subroutine getdimen(filename,xmax,ymax)
* returns naxis1 and naxis2 from a plain file or a fits image
      implicit none
      double precision xmax,ymax
      character*(*) filename
      integer reclen,bitpix
      integer ldot,lend,length,naxis1,naxis2,naxis
      integer indexrev

      ldot=indexrev(filename,'.')
      lend=min(length(filename),ldot+4)

      if (filename(ldot:lend).eq.'.fits') then
         call getbits(filename,reclen,bitpix,naxis,naxis1,naxis2)
      else
         naxis1=2048
         naxis2=4096
         call getdimhead(filename,naxis1,naxis2)
      endif

      xmax=dble(naxis1)
      ymax=dble(naxis2)

      return
      end
**************      
     
      subroutine getdimenint(filename,naxis1,naxis2)
* returns naxis1 and naxis2 from a plain file or a fits image
      implicit none
      character*(*) filename
      integer reclen,bitpix
      integer ldot,lend,length,naxis1,naxis2,naxis
      integer indexrev

      ldot=indexrev(filename,'.')
      lend=min(length(filename),ldot+4)

      if (filename(ldot:lend).eq.'.fits') then
         call getbits(filename,reclen,bitpix,naxis,naxis1,naxis2)
      else
         naxis1=2048
         naxis2=4096
         call getdimhead(filename,naxis1,naxis2)
      endif

      return
      end
**************      

      subroutine getdimhead(filename,naxis1,naxis2)
* returns naxis1 and naxis2 from a plain file
      character*(*) filename
      character*8 keyword
      character*70 value
      logical verbose
      integer nsexmax
      double precision cfactor
      common /parblk/ verbose,nsexmax,cfactor
             
 5    open(1,file=filename,err=1,status='old')

 10   read(1,9999,end=100) keyword,value
      n=index(value,'/')-1
      if (n.lt.0) n=70
      value=value(1:n)
      if (keyword.eq.'NAXIS1  ') read(value,*) naxis1
      if (keyword.eq.'NAXIS2  ') read(value,*) naxis2
      goto 10
 100  close(1)

 9999 format(a8,2x,a70)
      return

 1    print*,'file ',filename,' does not exist'
      stop
      end
*****************

      subroutine filewcs(headname,outname,
     & crval1,crval2,crpix1,crpix2,cd,pv,nord,nm,sixper)
      implicit double precision (a-h,o-z)
      character*(*) headname
      character*(*) outname
      character*30 fileq
      integer nord
      integer no(0:3) /0,3,6,10/
      character*80 line
      double precision  crval1,crval2,crpix1,crpix2
      double precision cd(2,2),pv(2,0:10)    
      
      character*35 pvlabel(2,0:10)/
     &'xi =   PV1_0',
     &'eta =  PV2_0',
     &'     + PV1_1 * x',
     &'     + PV2_1 * y',
     &'     + PV1_2 * y',
     &'     + PV2_2 * x',
     &'     + PV1_3 * sqrt(x**2 + y**2)',
     &'     + PV2_3 * sqrt(x**2 + y**2)',
     &'     + PV1_4 * x**2',
     &'     + PV2_4 * y**2',
     &'     + PV1_5 * x*y',
     &'     + PV2_5 * y*x',
     &'     + PV1_6 * y**2',
     &'     + PV2_6 * x**2',
     &'     + PV1_7 * x**3',
     &'     + PV2_7 * y**3',
     &'     + PV1_8 * x**2 * y',
     &'     + PV2_8 * y**2 * x',
     &'     + PV1_9 * x * y**2',
     &'     + PV2_9 * y * x**2',
     &'     + PV1_10* y**3',
     &'     + PV2_10* x**3'/


      character*25 now,when

      ldot=indexrev(headname,'.')
      ihead=0
      if ((ldot.eq.0).or.(length(headname).lt.ldot+4)) then
         ihead=1
      else
         if (headname(ldot:ldot+4).ne.'.fits') then
            ihead=1
         else
            ihead=0
         endif
      endif

      if (ihead.eq.1) then
         fileq=headname
         call system("cp "//fileq//" biteme")
      else
         call imhead(headname,'biteme')
      endif

      open(11,file='biteme')
      open(12,file=outname,err=2)

 10   read(11,9998,end=100) line
      iw=1
      if (line(1:5).eq.'CTYPE')   iw=0
      if (line(1:7).eq.'GWYNFIT') iw=0
      if (line(1:7).eq.'NORDFIT') iw=0
      if (line(1:8).eq.'NSTARS  ')iw=0
      if (line(1:8).eq.'ASTERR  ') iw=0
      if (line(1:8).eq.'FSTARS  ')iw=0
      if (line(1:3).eq.'END')     iw=0
      if (line(1:5).eq.'CRVAL')   iw=0
      if (line(1:5).eq.'CRPIX')   iw=0
      if (line(1:5).eq.'CDELT')   iw=0
      if (line(1:5).eq.'CROTA')   iw=0
      if (line(1:8).eq.'        ') iw=0
      if ((line(1:2).eq.'PV').and.(line(4:4).eq.'_')) iw=0
      if ((line(1:2).eq.'CD').and.(line(4:4).eq.'_')) iw=0
      if (index(line,'AstroGwyn').gt.0) iw=0
      if (iw.gt.0) then
         write(12,9998) line
      endif
     
      goto 10
 100  close(11)
      now=when()
      write(12,9993) 'Run on '//now
      write(12,9994) 'NORDFIT ',nord,'Order of fit'
      write(12,9994) 'NSTARS  ',nm,' Number of stars in final match'
      write(12,9989) 'ASTERR',sixper,'68%-tile of Residuals'  
      write(12,'(a)') "CTYPE1  = 'RA---TAN'           / Coordinate type"
      write(12,'(a)') "CTYPE2  = 'DEC--TAN'           / Coordinate type"
      write(12,9995) 'CRVAL1',crval1,'Tangent point RA, degrees'
      write(12,9995) 'CRVAL2',crval2,'Tangent point Dec, degrees'
      write(12,9995) 'CRPIX1',crpix1,'Tangent point x, pixels'
      write(12,9995) 'CRPIX2',crpix2,'Tangent point y, pixels'     

      do 30 i=1,2
         do 30 j=1,2
 30         write(12,9997) 'CD',i,j,cd(i,j)


      if (nord.lt.0) goto 1000
      do 20 i=1,2
         do 20 j=0,no(nord)
            if(j.lt.10) then
               write(12,9991) 'PV',i,j,pv(i,j),pvlabel(i,j)
            else
               write(12,9990) 'PV',i,j,pv(i,j),pvlabel(i,j)
            endif
 20   continue

 1000 write(12,'(a3)') 'END'
      close(12)

      
 9998 format(a80)
 9997 format(a2,i1,'_',i1,'   = ',1pg20.12)
 9996 format(a2,i1,'_',i2, '  = ',1pg20.12)
 9995 format(a6,'  = ',1pg20.12,' / ',a)
 9994 format(a8,'= ',i20,' / ',a)
 9993 format('HISTORY = AstroGwyn: ',a)
 9992 format('COMMENT   AstroGwyn: ',a)
 9991 format(a2,i1,'_',i1,'   = ',1pg20.12,' / ',a35)
 9990 format(a2,i1,'_',i2, '  = ',1pg20.12,' / ',a35)
 9989 format(a6,'  = ',f20.4,' / ',a)
      if(ihead.eq.0) call system('rm biteme')

      return
 1    print*,'trouble opening header file ',headname
 2    print*,'trouble opening output file ',outname
      stop
      end
*****************************
      
      subroutine imhead(filename,outname)
      character*(*) filename,outname
      character*80 line
      integer reclen,bitpix
      call getbits(filename,reclen,bitpix,naxis,naxis1,naxis2)
      
      open(1,file=filename,status='old',
     &     access='direct',recl=reclen) !<---|
***     this number changes with system _____| 

      open(11,file=outname)

      irec=1
 1000 read(1,rec=irec) line
      write(11,'(a80)') line
      if (line(1:8).eq.'END     ') goto 2000
      irec=irec+1 
      goto 1000
 2000 close(1)
      close(11)
      return
      end
************
