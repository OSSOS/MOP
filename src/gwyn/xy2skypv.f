      program xy2skypv
* compile with f77 -o xy2skypv xy2skypv.f utils.f wcs.f
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
      character*40 filein  
      character*40 fileout 
      character*40 image  
      character*80 line
      logical verbose
      integer nsexmax
      common /parblk/ verbose,nsexmax,cfactor
      integer filenum

      call procarg(image,filein,fileout)

      call getwcs(image,
     & crval1,crval2,crpix1,crpix2,cd,dc,pv,nord)

      open(1,file=filein)

      if ((fileout.eq.'stdout').or.(fileout.eq.'STDOUT')) then
         filenum=6
      else
         filenum=11
         open(11,file=fileout)
      endif

 10   read(1,'(a)',end=20,err=10) line
      read(line,*,err=9000) x,y
      lq=length(line)
      call xy2sky(crval1,crval2,crpix1,crpix2,cd,pv,nord,
     &        x,y,ra,dec)      
      write(filenum,'(2f15.9,2x,a)') ra,dec,line(1:lq)
      nsex=nsex+1
      goto 10
 20   close(1)
      close(filenum)
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
      character*40 image,filein,fileout
      integer debug,logfile
      common /debugblk/ debug,logfile
      
      if (iargc().lt.1) then
         write(*,*) 'Usage:'
         write(*,*) 'xy2skypv <image> <filein> <fileout>'
         write(*,*) '<image> file containing WCS'
         write(*,*) '        if <image> has a .fits, extension,' 
         write(*,*) '        the WCS will be read from the header'
         write(*,*) '<filein> file containing x,y positions'
         write(*,*) '   default is the image file with a .sex extension'
         write(*,*) '<fileout> file to which output will be sent'
         write(*,*) '          default is STDOUT'
         stop
      endif
      call condargc(1,image)
      ldot=index(image,'.')
      if (ldot.lt.1) then
         li=length(image)
         image=image(1:li)//'.fits'
         ldot=index(image,'.')

      endif
      filein=image(1:ldot)//'sex'
      call condargc(2,filein)
      if (filein(1:1).eq.'@') filein=filein(2:length(filein))

      fileout='STDOUT'
      call condargc(3,fileout)
      vs='n'
      call condargc(4,vs)
      verbose=((vs.eq.'y').or.(vs.eq.'Y').or.(vs.eq.'v').or.(vs.eq.'V'))
      if (verbose) then
         write(*,*) "Verbose mode"
      endif
      return
      end
*****************************
