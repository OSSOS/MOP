      program sky2xypv
* compile with f77 -o sky2xypv sky2xypv.f utils.f wcs.f
      implicit double precision (a-h,o-z)
      parameter(nmax=5000)
      double precision cd(2,2),dc(2,2),pv(2,0:10)
      double precision xsex(nmax),ysex(nmax)
      double precision rasex(nmax),desex(nmax)
      double precision racat(nmax),decat(nmax)
      double precision rhold(nmax)
      integer nmatch(nmax)
      double precision rmatch(nmax)
      character*1 vs /'n'/
      logical diag/.FALSE./
      integer nord
      character*256 filein  
      character*256 fileout 
      character*256 image  
      character*256 line
      logical verbose
      integer nsexmax
      common /parblk/ verbose,nsexmax,cfactor
      double precision xmax,ymax

      call procarg(image,filein,fileout)

      call getwcs(image,
     & crval1,crval2,crpix1,crpix2,cd,dc,pv,nord)

      call getdimen(image,xmax,ymax)
      if (verbose) then
         write(*,'("size of image",2f8.0)') xmax,ymax
      endif

      call xy2sky(crval1,crval2,crpix1,crpix2,cd,pv,nord,1d0,1d0,
     &     ra,dec)
      ramax=ra
      decmax=dec
      ramin=ra
      decmin=dec

      call xy2sky(crval1,crval2,crpix1,crpix2,cd,pv,nord,1d0,ymax,
     &     ra,dec)
      ramax=max(ra,ramax)
      decmax=max(dec,decmax)
      ramin=min(ra,ramin)
      decmin=min(dec,decmin)

      call xy2sky(crval1,crval2,crpix1,crpix2,cd,pv,nord,xmax,1d0,
     &     ra,dec)
      ramax=max(ra,ramax)
      decmax=max(dec,decmax)
      ramin=min(ra,ramin)
      decmin=min(dec,decmin)

      call xy2sky(crval1,crval2,crpix1,crpix2,cd,pv,nord,xmax,ymax,
     &     ra,dec)
      ramax=max(ra,ramax)
      decmax=max(dec,decmax)
      ramin=min(ra,ramin)
      decmin=min(dec,decmin)

      if (verbose) then
         write(*,'("limits",4f15.8)')  ramin,ramax,decmin,decmax
      endif

      open(1,file=filein)

      if ((fileout.eq.'stdout').or.(fileout.eq.'STDOUT')) then
         filenum=6
      else
         filenum=11
         open(11,file=fileout)
      endif

 10   read(1,'(a)',end=20,err=10) line
      read(line,*,err=9000) ra,dec
      itran=1
      if (ra.lt.ramin) itran=0
      if (ra.gt.ramax) itran=0
      if (dec.lt.decmin) itran=0
      if (dec.gt.decmax) itran=0
      if (itran.gt.0) then
         call sky2xy(crval1,crval2,crpix1,crpix2,dc,pv,nord,
     &        ra,dec,x,y)
         if (x.lt.0d0) itran=0 
         if (x.gt.xmax) itran=0 
         if (y.lt.0d0) itran=0 
         if (y.gt.ymax) itran=0 
      endif
      if (itran.eq.0) then
         x=-9999.999d0
         y=-9999.999d0
      endif
      lq=length(line)
      write(filenum,'(2f12.3,2x,a)') x,y,line(1:lq)
      nsex=nsex+1
      goto 10

 20   close(1)
      close(11)
      if (verbose) write(*,9999) nsex,filein


 9998 format(2f15.9,a)
 9999 format ('read ',i5,' lines from ',a)
      stop
 9000 write(*,*) 'Error reading file: ',filein
      stop
      end
*****************
*****************

      subroutine procarg(image,filein,fileout)
      implicit none
      character vs
      integer li,ldot
      logical verbose
      integer nsexmax
      double precision cfactor
      common /parblk/ verbose,nsexmax,cfactor
      integer iargc,length
      character*6 sextail,outtail
      character*256 image,filein,fileout
      integer debug,logfile
      common /debugblk/ debug,logfile
      double precision xmax,ymax

      if (iargc().lt.1) then
         write(*,*) 'Usage:'
         write(*,*) 'sky2xypv <image> <filein> <fileout> <xmax> <ymax>'
         write(*,*) '<image> file containing WCS'
         write(*,*) '        if <image> has a .fits, extension,' 
         write(*,*) '        the WCS will be read from the header'
         write(*,*) '<filein> file containing x,y positions'
         write(*,*) '         default is USNO'
         write(*,*) '<fileout> file to which output will be sent'
         write(*,*) '          default is STDOUT'
	 STOP
      endif
      call condargc(1,image)
      ldot=index(image,'.')
      if (ldot.lt.1) then
         li=length(image)
         image=image(1:li)//'.fits'
         ldot=index(image,'.')
      endif
      filein='USNO'
      call condargc(2,filein)
      if (filein(1:1).eq.'@') filein=filein(2:length(filein))

      fileout='STDOUT'
      call condargc(3,fileout)
      vs='n'
      call condargc(4,vs)
      verbose=((vs.eq.'y').or.(vs.eq.'Y').or.(vs.eq.'v').or.(vs.eq.'V'))

      return
      end
*****************************
