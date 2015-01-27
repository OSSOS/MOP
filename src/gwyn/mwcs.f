      subroutine mgetbits(filename,reclen,bitpix)
      character*80 filename
      character*80 line1,line2
      integer irectry(3) /80,40,20/
      integer reclen,bitpix
      character*8 keyword
      character*70 value
      
      do 10 i=1,3
         open(1,file=filename,status='old',err=2,
     &        access='direct',recl=irectry(i))
         
         read(1,rec=1,err=1) line1
         read(1,rec=2,err=1) line2
         if (line1(1:8).ne.'SIMPLE  ') then
            print*,'ERROR: Dubious FITS file: ',filename
            stop
         endif
         if (line2(1:8).eq.'BITPIX  ') then
            reclen=irectry(i)
            n=index(line2,'/')-1
            if (n.lt.0) n=70
            read(line2(10:n),*) bitpix
            goto 20
         endif
 10   close(1)
 20   close(1)
      return

 2    print*,'ERROR in mgetbits: ',filename(1:length(filename))
     $     ,' does not exist'
 1    print*,'ERROR in mgetbits reading: ',filename(1:length(filename))
      stop
 9999 format(a8,2x,a70)
      end
**************

      subroutine mgetwcs(filename,next,
     &     crval1,crval2,crpix1,crpix2,cd,dc,pv,nord)
      implicit none
      double precision pi180
      parameter(pi180=57.2957795130823208767981548141052d0)
      character*80 filename
      double precision crval1,crval2,crpix1,crpix2
      double precision cd(2,2),dc(2,2),pv(2,0:10)
      integer ldot,lend,length
      integer reclen,bitpix,naxis,naxis1,naxis2
      logical verbose
      integer nsexmax
      double precision cfactor
      common /parblk/ verbose,nsexmax,cfactor
      integer nord,nordgwyn,nordgwynp
      common /gwynblk/ nordgwynp
      integer i,j
      integer indexrev
      integer next
      
      do 10 i=1,2
      do 10 j=0,10
 10      pv(i,j)=0d0
      nordgwyn=-10
      ldot=indexrev(filename,'.')
      lend=min(length(filename),ldot+4)

      if (filename(ldot:lend).eq.'.fits') then
         call mgetbits(filename,reclen,bitpix)
         call mgetwcsfits(filename,next,reclen,
     &        crval1,crval2,crpix1,crpix2,cd,dc,pv,nord,nordgwyn)    
       elseif ((filename(ldot:lend).eq.'.head')
     &        .or.(filename(ldot:lend).eq.'.fead')) then
         call mgetwcshead(filename,next,
     &        crval1,crval2,crpix1,crpix2,cd,dc,pv,nord,nordgwyn)   
    
      elseif (filename(lend-7:lend).eq.'.fits.fz') then
         call mgetbits(filename,reclen,bitpix)
         call mgetwcsfits(filename,next,reclen,
     &        crval1,crval2,crpix1,crpix2,cd,dc,pv,nord,nordgwyn)    
         

      else
          write(*,*) 'ERROR in mgetwcs: unrecognized file type '
     $         ,filename(1:lend)
          stop
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
         write(*,'(a)') 'read the following WCS:'
         call dumpwcs(crval1,crval2,crpix1,crpix2,cd,pv,nord)
      endif
     
      return
      end
********************


      subroutine mgetwcshead(filename,next,
     &     crval1,crval2,crpix1,crpix2,cd,dc,pv,nord,nordgwyn)
      implicit double precision (a-h,o-z)
      parameter(pi180=57.2957795130823208767981548141052d0)
      character*(*) filename
      character*8 keyword
      character*8 keycheck
      character ci,cj
      character*80 line
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
      
      do nq=1,next
         call mfindhead(1,filename,line)
      enddo

 10   read(1,9999,end=100,err=2) keyword,value
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
      if (keyword.eq.'END     ') goto 100
      
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
 2    print*,'ERROR in getwcshead: reading ',filename
      stop
      end
*****************

      subroutine mdumpwcs(crval1,crval2,crpix1,crpix2,cd,pv,nord)
      implicit none
      double precision crval1,crval2,crpix1,crpix2
      double precision cd(2,2),pv(2,0:10)
      integer nord
      integer no(0:3) /0,3,6,10/
      integer i,j
      write(*,'(a4)') "WCS:"
      write(*,'("CRVALs",2f12.7)')crval1,crval2
      write(*,'("CRPIXs",2f12.2)')crpix1,crpix2
      write(*,'("CD:   ",1p,4(1x,e10.3))') cd
      if (nord.ge.0) then
         write(*,'("order:"i2)') nord
         write(*,'("PV1_: ",1p,11e10.2)') (pv(1,j),j=0,no(nord))
         write(*,'("PV2_: ",1p,11e10.2)') (pv(2,j),j=0,no(nord))
      endif
      write(*,*)
      return
      end
*****************************


      subroutine mxy2sky(crval1,crval2,crpix1,crpix2,cd,pv,nord,
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

c$$$      subroutine msky2xy(crval1,crval2,crpix1,crpix2,dc,pv,nord,
c$$$     &  ran,decn,xout,yout)
c$$$      implicit double precision (a-h,o-z)
c$$$      double precision cd(2,2),pv(2,0:10)
c$$$      double precision dc(2,2)
c$$$      integer nord
c$$$      pi180=   57.2957795130823208767981548141052d0    
c$$$
c$$$*      write(*,'(2f15.8,2f10.3)') ran,decn,crval1,crval2
c$$$      ra=ran/pi180
c$$$      dec=decn/pi180
c$$$      tdec=tan(dec)
c$$$      ra0  = CRVAL1/pi180
c$$$      dec0 = CRVAL2/pi180
c$$$      ctan = tan(dec0)
c$$$      ccos = cos (dec0)
c$$$  
c$$$      traoff = tan (ra - ra0)
c$$$      craoff = cos (ra - ra0)
c$$$      etar = (1d0 - ctan * craoff / tdec) / (ctan + (craoff / tdec))
c$$$      xir = traoff * ccos * (1d0 - (etar * ctan))
c$$$      xi = xir * pi180
c$$$      eta = etar * pi180
c$$$
c$$$      if (nord.lt.0) then            ! the simple solution
c$$$         x=xi
c$$$         y=eta 
c$$$      else                            ! reverse by Newton's method
c$$$         tolerance=0.001/3600.        ! 0.001 arcsec
c$$$         x = xi  
c$$$         y = eta  ! intial guess
c$$$         iter=0
c$$$ 100  iter=iter+1
c$$$      if (nord.ge.0) then
c$$$         f = PV(1,0)                     ! estimates
c$$$         g = PV(2,0)
c$$$         fx=0d0                          ! derivatives 
c$$$         fy=0d0
c$$$         gx=0d0
c$$$         gy=0d0
c$$$      endif
c$$$      if (nord.ge.1) then
c$$$         r=sqrt(x**2 + y**2)
c$$$         f = f + PV(1,1)*x + PV(1,2)*y + PV(1,3)*r
c$$$         g = g + PV(2,1)*y + PV(2,2)*x + PV(2,3)*r
c$$$         fx=fx + PV(1,1)               + PV(1,3)*x/r
c$$$         fy=fy             + PV(1,2)   + PV(1,3)*y/r
c$$$         gx=gx             + PV(2,2)   + PV(2,3)*x/r
c$$$         gy=gy + PV(2,1)               + PV(2,3)*y/r      
c$$$      endif
c$$$      if (nord.ge.2) then
c$$$         x2 = x**2
c$$$         xy = x*y
c$$$         y2 = y**2
c$$$         f = f + PV(1,4)*x2    + PV(1,5)*xy + PV(1,6)*y2 
c$$$         g = g + PV(2,4)*y2    + PV(2,5)*xy + PV(2,6)*x2        
c$$$         fx=fx + PV(1,4)*2d0*x + PV(1,5)*y
c$$$         fy=fy                 + PV(1,5)*x  + PV(1,6)*2d0*y
c$$$         gx=gx                 + PV(2,5)*y  + PV(2,6)*2d0*x
c$$$         gy=gy + PV(2,4)*2d0*y + PV(2,5)*x 
c$$$      endif
c$$$      if (nord.ge.3) then
c$$$         x3  = x**3
c$$$         x2y = x2*y
c$$$         xy2 = x*y2
c$$$         y3  = y**3
c$$$         f = f + PV(1,7)*x3     + PV(1,8)*x2y    + PV(1,9)*xy2    
c$$$     &        + PV(1,10)*y3 
c$$$         g = g + PV(2,7)*y3     + PV(2,8)*xy2    + PV(2,9)*x2y    
c$$$     &        + PV(2,10)*x3
c$$$         fx=fx + PV(1,7)*3d0*x2 + PV(1,8)*2d0*xy + PV(1,9)*y2     
c$$$         fy=fy                  + PV(1,8)*x2     + PV(1,9)*2d0*xy 
c$$$     &        + PV(1,10)*3d0*y2
c$$$         gx=gx                  + PV(1,8)*y2     + PV(2,9)*2d0*xy 
c$$$     &        + PV(2,10)*3d0*x2
c$$$         gy=gy + PV(2,7)*3d0*y2 + PV(1,8)*2d0*xy + PV(2,9)*x2
c$$$      endif 
c$$$*      write(*,'(4f15.10)') f,xi,g,eta
c$$$
c$$$      f = f - xi
c$$$      g = g - eta
c$$$	dx = ((-f * gy) + (g * fy)) / ((fx * gy) - (fy * gx)) 
c$$$	dy = ((-g * fx) + (f * gx)) / ((fx * gy) - (fy * gx)) 
c$$$	x = x + dx
c$$$	y = y + dy
c$$$
c$$$        
c$$$        if (abs(dx).gt.tolerance) goto 100
c$$$        if (abs(dy).gt.tolerance) goto 100     
c$$$        if (iter.gt.200) then
c$$$           print*,"ERROR: Too many iterations in sky2xy"
c$$$           stop
c$$$        endif
c$$$      endif
c$$$      
c$$$
c$$$      xp = dc(1,1)*x + dc(1,2) * y
c$$$      yp = dc(2,1)*x + dc(2,2) * y
c$$$      
c$$$      xout=xp+CRPIX1
c$$$      yout=yp+CRPIX2
c$$$
c$$$      return
c$$$      end
c$$$*****************

      subroutine mgetdimen(filename,next,xmax,ymax)
* returns naxis1 and naxis2 from a plain file or a fits image
      implicit none
      double precision xmax,ymax
      character*80 filename
      integer reclen,bitpix
      integer ldot,lend,length,naxis1,naxis2,naxis
      integer indexrev
      integer next

      if (next.le.0) then
         call getdimen(filename,xmax,ymax)
         return
      endif

      ldot=indexrev(filename,'.')
      lend=min(length(filename),ldot+4)
      

      if (filename(ldot:lend).eq.'.fits') then
         call mgetdimfits(filename,next,naxis1,naxis2,
     $        'NAXIS1  ','NAXIS2  ')

      elseif ((filename(ldot:lend).eq.'.head')
     $        .or.(filename(ldot:lend).eq.'.fead')) then
         naxis1=2048
         naxis2=4096
         call mgetdimhead(filename,next,naxis1,naxis2)
      elseif (filename(lend-7:lend).eq.'.fits.fz') then
         call mgetdimfits(filename,next,naxis1,naxis2,
     $        'ZNAXIS1 ','ZNAXIS2 ')
      else
         write(*,*) 'ERROR in getdimen: unrecognized file type '
     $        ,filename(1:lend)
         stop
      endif

      xmax=dble(naxis1)
      ymax=dble(naxis2)

      return
      end
**************   
      

      subroutine mgetdimhead(filename,next,naxis1,naxis2)
* returns naxis1 and naxis2 from a plain file
      implicit none
      integer next,naxis1,naxis2,n,i
      character*80 filename,line
      character*8 keyword
      character*70 value
      logical verbose
      integer nsexmax
      double precision cfactor
      common /parblk/ verbose,nsexmax,cfactor
      logical terapix 
      integer ierr
      terapix=.false.
      naxis1=0
      naxis2=0
 5    open(1,file=filename,err=1,status='old')
      do i=1,next
          call mfindhead(1,filename,line)
      enddo
      i=0
 10   read(1,9999,end=100,err=3) keyword,value
      n=index(value,'/')-1
      if (n.le.0) n=70
      if (index(value(n:70),'SCAMP').gt.0) terapix=.true.
      value=value(1:n)
      if (keyword.eq.'NAXIS1  ') read(value,*) naxis1
      if (keyword.eq.'NAXIS2  ') read(value,*) naxis2
      if (keyword.eq.'END     ') goto 100
      goto 10
 100  close(1)
      if (naxis1*naxis2.eq.0) then
         print*,'could not find NAXIS1 and/or NAXIS2'
         naxis1=2048
         naxis2=4096
         if (terapix) then
            print*,'this a terapix image: no NAXIS keywords'
            naxis1=2048
            naxis2=4612
         endif
         print*,'assuming naxis1=',naxis1,' naxis2=',naxis2
      endif
 9999 format(a8,2x,a70)
      return

 1    print*,'file ',filename,' does not exist'
 2    print*,'extension',next,' does not exit in file',filename
 3    print*,'problems reading',filename
      stop
      end
*****************
  
      subroutine mfilewcs(headname,next,outname,catname,
     & crval1,crval2,crpix1,crpix2,cd,pv,nord,nm,sixper)
      implicit double precision (a-h,o-z)
      character*(*) headname
      character*(*) outname
      character*(*) catname
      character*30 fileq
      integer nord
      integer no(0:3) /0,3,6,10/
      character*80 line
      double precision  crval1,crval2,crpix1,crpix2
      double precision cd(2,2),pv(2,0:10)    
      character*9 access
      character*30 kludge /'                              '/
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
      lcat=min(length(catname),20)

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
         open(1,file=headname,status='old')
         open(11,file='biteme')
         do i=1,next
            call mfindhead(1,headname,line)
         enddo
         write(11,'(a)') line

 11      read(1,'(a)') line
         write(11,'(a)') line
         if (line(1:8).ne.'END     ') goto 11
         close(1)
         close(11)
      else
         call mimhead(headname,next,'biteme')
      endif

      open(11,file='biteme')
      open(12,file=outname,err=2,access='append')
      
      !write(12,'(a)') 'SIMPLE  =                    T / Standard FITS'

 10   read(11,9998,end=100) line
      iw=1
      if (line(1:5).eq.'CTYPE')    iw=0
      if (line(1:5).eq.'CRVAL')    iw=0
      if (line(1:5).eq.'CRPIX')    iw=0
      if (line(1:5).eq.'CDELT')    iw=0
      if (line(1:5).eq.'CROTA')    iw=0
      if (line(1:3).eq.'END')      iw=0
      if (line(1:8).eq.'ASTCAT  ') iw=0
      if (line(1:8).eq.'GWYNFIT ') iw=0
      if (line(1:8).eq.'NORDFIT ') iw=0
      if (line(1:8).eq.'NSTARS  ') iw=0
      if (line(1:8).eq.'FSTARS  ') iw=0
      if (line(1:8).eq.'ASTERR  ') iw=0
      if (line(1:8).eq.'        ') iw=0
      if ((line(1:2).eq.'PV').and.(line(4:4).eq.'_')) iw=0
      if ((line(1:2).eq.'CD').and.(line(4:4).eq.'_')) iw=0
      if (index(line,'AstroGwyn').gt.0) iw=0
      if (index(line,'Reserved space.').gt.0) iw=0
      if (iw.gt.0) then
         write(12,9998) line
      endif
     
      goto 10
 100  close(11)
      call system("rm biteme")
      now=when()
      kludge=''''//catname(1:lcat)//''''
      write(12,9993) 'Run on '//now
      write(12,9994) 'NORDFIT ',nord,' Order of fit'
      write(12,9994) 'NSTARS  ',nm,' Number of stars in final match'
      write(12,9989) 'ASTERR',sixper,'68%-tile of Residuals'
      write(12,9988) 'ASTCAT',kludge(1:max(length(kludge),20))
     $     ,'Reference catalogue'
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
 9994 format(a8,'= ',i20,' /',a)
 9993 format('HISTORY = AstroGwyn: ',a)
 9992 format('COMMENT   AstroGwyn: ',a)
 9991 format(a2,i1,'_',i1,'   = ',1pg20.12,' / ',a35)
 9990 format(a2,i1,'_',i2, '  = ',1pg20.12,' / ',a35)
 9989 format(a6,'  = ',f20.4,' / ',a)
 9988 format(a6,'  = ',a,' / ',a)
      !if(ihead.eq.0) call system('rm biteme')

      return
 1    print*,'trouble opening header file ',headname
 2    print*,'trouble opening output file ',outname
      stop
      end
*****************************
      
      subroutine mimhead(filename,next,outname)
      character*(*) filename,outname
      character*80 line
      integer reclen,bitpix,reclast
      call getbits(filename,reclen,bitpix,naxis,naxis1,naxis2)
      
      open(1,file=filename,status='old',
     &     access='direct',recl=reclen) !<---|
***     this number changes with system _____| 

      open(11,file=outname)

      open(1,file=filename,status='old',
     &     access='direct',recl=reclen) 
      
      reclast=0
      do i=1,next
         mq=mfindfits(1,reclast)
      enddo
      irec=reclast

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

      subroutine mfindhead(nunit,file,line)
      character*80 line
      character*(*) file
 10   read(nunit,'(a)',end=20) line
      if (line(1:8).eq.'XTENSION') then
         return
      endif
      goto 10
 20   print*,'ERROR in mfindhead reading file: ',file
      stop
      end
***************

      function mfindfits(nunit,reclast)
      character*8 keyword
      integer reclast,imsize
      character*8 key
      character*80 value
      integer pcount,naxis1,naxis2,bitpix
      save imsize
      logical debug /.true./
      if (reclast.eq.0) imsize=0  
      if (imsize.gt.0) then
         !print*,reclast,imsize
         ibytes=reclast*80+imsize
         iblocks=int(real(ibytes)/2880.)
         irec=iblocks*36+1    ! might be not so good
      else
         irec=1
      endif
      !if( debug) print*,'      starting at:',irec
 10   irec=irec+36
      !if (debug) print*,'block',(irec-1)/36
      call rec2kv(nunit,irec,key,value)
      if (key.ne.'XTENSION') goto 10
      !if (debug) print 9999,irec,key,value
      reclast=irec
      pcount=0
      naxis1=0
      naxis2=0
      bitpix=0
 20   irec=irec+1
      call rec2kv(nunit,irec,key,value)
      if (key.eq.'PCOUNT  ') read(value,*) pcount
      if (key.eq.'NAXIS1  ') read(value,*) naxis1
      if (key.eq.'NAXIS2  ') read(value,*) naxis2
      if (key.eq.'BITPIX  ') read(value,*) bitpix
      if (key.ne.'END     ') goto 20
      if (pcount.gt.0) then
         imsize=pcount
      else
         imsize=naxis1*naxis2*abs(bitpix/8)
      endif
      if (imsize.eq.0) goto 3
      !print*,'pcount',pcount
      !print*,'naxis1',naxis1
      !print*,'naxis2',naxis2
      !print*,'bitpix',bitpix
      !print*,'imsize',imsize
      mfindfits=0
      return

 3    print*,'ERROR in mindfits'
      print*,'pcount',pcount
      print*,'naxis1',naxis1
      print*,'naxis2',naxis2
      print*,'bitpix',bitpix
 9999 format(i20,2x,a8,2x,a70)
      stop
      end
***************

      character*80 function mgetkey(filename,next,keyword)
* delete???
      character*80 filename
      integer next
      character*8 keyword
      mgetkey=''
      return
      end
***************

      subroutine mgetdimfits(filename,next,naxis1,naxis2,caxis1,caxis2)
* returns naxis1 and naxis2 from a FITS file
      character*(*) filename
      character*8 key,caxis1,caxis2
      character*70 value
      logical verbose
      integer nsexmax
      double precision cfactor
      common /parblk/ verbose,nsexmax,cfactor
      integer reclen,bitpix,reclast
      naxis1=0
      naxis2=0
      reclast=0
      call  mgetbits(filename,reclen,bitpix)
      open(1,file=filename,status='old',err=1,
     &        access='direct',recl=reclen)
      mq=0
       do i=1,next
          mq=mfindfits(1,reclast)
          if (mq.gt.0) then
            print*,'problems with extension ',i
            goto 3 
         endif
      enddo

      irec=reclast
 10   irec=irec+1
      call rec2kv(1,irec,key,value)
      if (key.eq.caxis1) read(value,*) naxis1
      if (key.eq.caxis2) read(value,*) naxis2
      if (key.ne.'END     ') goto 10
 100  close(1)
      if (naxis1*naxis2.eq.0) then
         print*,'could not find NAXIS1 and/or NAXIS2'
         naxis1=2048
         naxis2=4096    
         print*,'assuming naxis1=',naxis1,' naxis2=',naxis2
      endif
 9999 format(a8,2x,a70)
      return

 1    print*,'file ',filename,' does not exist'
 2    print*,'extension',next,' does not exit in file',filename
 3    print*,'problems reading',filename
      stop
      end
*****************


      subroutine mgetwcsfits(filename,next,reclen,
     &     crval1,crval2,crpix1,crpix2,cd,dc,pv,nord,nordgwyn)
      implicit double precision (a-h,o-z)
      parameter(pi180=57.2957795130823208767981548141052d0)
      character*80 filename
      character*8 keyword
      character*80 line
      character*8 keycheck
      character ci,cj
      character*70 value
      double precision crval1,crval2,crpix1,crpix2
      double precision cd(2,2),dc(2,2),pv(2,0:10)
      logical cdfound(2,2) /4*.false./
      character*8 ctype1,ctype2
      double precision cdelt1,cdelt2,crota1/0d0/,crota2/0d0/
      integer nord,nordgwyn
      integer nordlev(0:10) /0,1,1,1,2,2,2,3,3,3,3/
      integer reclen,reclast

      nordgwyn=-10
      nord=-1
      open(1,file=filename,status='old',
     &     access='direct',recl=reclen) 
      
      reclast=0
      do i=1,next
         mq=mfindfits(1,reclast)
      enddo
      irec=reclast
 1000 irec=irec+1
      call rec2kv(1,irec,keyword,value)
      if (keyword.eq.'END     ') goto 2000
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
            if (CROTA1.ne.CROTA2) write(*,'(a)')
     &           "warning: ignoring skewed axes"
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

      integer function getnextmax(filename)
      implicit none
      character*(*) filename
      integer getnextmaxfits,getnextmaxhead,indexrev,length
      if (filename(indexrev(filename,'.'):length(filename))
     $     .eq.'.fits') then
         getnextmax=getnextmaxfits(filename)
      elseif (filename(indexrev(filename,'.'):length(filename))
     $        .eq.'.head') then
         getnextmax=getnextmaxhead(filename)
      else
         write(*,*) 'ERROR in getnextmax: unrecognised extensions for ',
     $        filename(1:length(filename))
      write(*,'(a)') filename(indexrev(filename,'.'):length(filename))
         stop
      endif
      return
      end
*****************

    
      integer function getnextmaxfits(filename)
      implicit none
      character*(*) filename
      character*80 line
      integer reclen,bitpix,reclast
      integer naxis,naxis1,naxis2,irec,length
      character*8 key
      character*70 value
      call getbits(filename,reclen,bitpix,naxis,naxis1,naxis2)
      
      open(1,file=filename,status='old',
     &     access='direct',recl=reclen) !<---|
***     this number changes with system _____| 

      irec=1
 10   call rec2kv(1,irec,key,value)
      if (key.eq.'END     ') then
         getnextmaxfits=0
         close(1)
         return
      endif
      if (key.eq.'NEXTEND ') then 
         read(value,*) getnextmaxfits
         close(1)
         return
      endif
      irec=irec+1
      goto 10
      
      return
      end
************

      integer function getnextmaxhead(filename)
      implicit none
      character*(*) filename
      character*80 line
      integer n,length,next
      character*8 key
      character*70 value
      next=0
      open(1,file=filename,status='old')
 10   read(1,'(a)',end=20) line
      key=line(1:8)
      n=index(line,'/')
      if (n.le.1) n=30
      value=line(10:n)
      if (key.eq.'XTENSION') next=next+1
      if ((key.eq.'END     ').and.(next.eq.0)) goto 20
      if (key.eq.'NEXTEND ') then 
         read(value,*) next
         goto 20
      endif
      goto 10
 20   close(1)
      getnextmaxhead=next
      return
      return
      end
************

      subroutine movemainhead(headname,outname)
      character*80 headname,outname,line
      integer reclen,bitpix,naxis,naxis1,naxis2
      if (headname(indexrev(headname,'.'):length(headname))
     $     .eq.'.fits') then
         call getbits(headname,reclen,bitpix,naxis,naxis1,naxis2)
      
         open(1,file=headname,status='old',
     $        access='direct',recl=reclen)
         open(11,file=outname)
         irec=1
 10      read(1,rec=irec,err=1) line
         write(11,'(a)') line
         if (line(1:8).ne.'END     ') then
            irec=irec+1
            goto 10
         endif
         close(1)
         close(11)
      elseif (headname(indexrev(headname,'.'):length(headname))
     $        .eq.'.head') then
         open(1,file=headname,status='old')
         open(11,file=outname)
 20      read(1,'(a)') line
         write(11,'(a)') line
         if (line(1:8).ne.'END     ') goto 20
         close(1)
         close(11)
      else
        write(*,*) 'ERROR: unrecognised extension for ',
     $        headname(1:length(headname))
        write(*,'(a)') headname(indexrev(headname,'.'):length(headname))
         stop
      endif
      return
 1    print*,'ERROR in movemainhead reading ',
     $     headname(1:length(headname))
      end
************
      
