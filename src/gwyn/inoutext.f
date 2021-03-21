
      subroutine fitsextopen(filename,next,readwrite,unit)
      character*(*) filename
      integer status,unit,readwrite,hdutype
      status=0
*** open the file
      call ftgiou(unit,status)
      call ftnopn(unit,filename,readwrite,status)
      call printerror(status)
*** move the right HDU
      call ftmahd(unit,next+1,hdutype,status)
      call printerror(status)

      return 
      end
***************************
     
      subroutine fitsclose(unit)
      integer status,unit
      status=0
      call ftclos(unit, status)
      call ftfiou(unit, status)
      call printerror(status)
      return
      end
***************************

      subroutine printerror(status)
*     Print out the FITSIO error messages to the user
      integer status
      character errtext*30,errmessage*80
*     check if status is OK (no error); if so, simply return
      if (status .le. 0)return

*     get the text string which describes the error
 1    call ftgerr(status,errtext)
      print *,'FITSIO Error Status =',status,': ',errtext

*     read and print out all the error messages on the FITSIO stack
 2    call ftgmsg(errmessage)
      do while (errmessage .ne. ' ')
          print *,errmessage
          call ftgmsg(errmessage)
      end do
* reset the status
      status=0
      return
      end
***************************



      function imodesec2d(a,i1,i2,j1,j2)
* finds the mode of an image section with integer value
      integer dim1,dim2
      parameter(dim1=2112,dim2=4644)
      integer hist(65536),histmax
      integer a(dim1,dim2)
      real ha,hn
*** initialize the histogram
      do n=1,65536
         hist(n)=0
      enddo

*** load the histogram from the image section
      do j=j1,j2
      do i=i1,i2
         hist(a(i,j))=hist(a(i,j))+1
      enddo
      enddo
 
*** find the peak
      histmax=hist(1)
      do n=1,65536
         if (hist(n).gt.histmax) then
            nmax=n
            histmax=hist(n)
         endif
      enddo

*** return it
      imodesec2d=nmax
      return
      end
****************


      function amodesec2d(a,i1,i2,j1,j2)
* finds the mean of image section, but make the histogram
* and only includes the peak 
      integer dim1,dim2
      parameter(dim1=2112,dim2=4644)
      integer hist(65536),histmax,nh
      real a(dim1,dim2)
      real ha,hn
*** initialize the histogram
      do n=1,65536
         hist(n)=0
      enddo

*** load the histogram from the image section
      do j=j1,j2
      do i=i1,i2
         nh=int(a(i,j))
         if ( (nh.ge.1).and.(nh.le.65536)) then
            hist(nh)=hist(nh)+1
         endif
      enddo
      enddo
 
*** find the peak
      histmax=hist(1)
      do n=1,65536
         if (hist(n).gt.histmax) then
            nmax=n
            histmax=hist(n)
         endif
      enddo

      ha=0
      hn=0
*** work out from the peak down until we hit an empty bin
      do n=nmax,1,-1
         if (hist(n).eq.0) goto 10
         ha=ha+n*hist(n)
         hn=hn+hist(n)
      enddo
 10   continue
      
         
*** work up from the peak down until we hit an empty bin
      do n=nmax+1,65536
         if (hist(n).eq.0) goto 20
         ha=ha+n*hist(n)
         hn=hn+hist(n)
       enddo
 20   continue
      
      amodesec2d=ha/hn
      return
      end
****************


**** below here, possibly not used.




      subroutine moveinextint(filename,next,a,dim1,dim2)
* Read an integer image from an extension
* dim1 and dim2 have to be the same size as NAXIS1 and NAXIS2 and the size of the fortran array
      character*(*) filename
      integer next
      integer dim1,dim2
      integer a(dim1,dim2)
      integer status,unit,readwrite,hdutype,group
      logical anynull
      
      group=0
      status=0
      readwrite=0

*** open the file
      call ftgiou(unit,status)
      call ftnopn(unit,filename,readwrite,status)
      call printerror(status)

*** move the right HDU
      call ftmahd(unit,next+1,hdutype,status)
      call printerror(status)

*** actually read the data
      call ftg2dj(unit,group,nullval,dim1,dim1,dim2,
     &     a,anynull,status)
      call printerror(status)

*** close the file and free the unit number
      call ftclos(unit, status)
      call ftfiou(unit, status)
      call printerror(status)

      return 
      end
***************************

      subroutine moveinext(filename,next,a,dim1,dim2)
* Read a real image from an extension
* dim1 and dim2 have to be the same size as NAXIS1 and NAXIS2 and the size of the fortran array
      character*(*) filename
      integer next
      integer dim1,dim2
      real a(dim1,dim2)
      integer status,unit,readwrite,hdutype,group
      logical anynull

      group=0
      status=0
      readwrite=0

*** open the file
      call ftgiou(unit,status)
      call ftnopn(unit,filename,readwrite,status)
      call printerror(status)

*** move the right HDU
      call ftmahd(unit,next+1,hdutype,status)
      call printerror(status)

*** actually read the data
      call ftg2de(unit,0,nullval,dim1,dim1,dim2,
     &     a,anynull,status)
      call printerror(status)

*** close the file and free the unit number
      call ftclos(unit, status)
      call ftfiou(unit, status)
      call printerror(status)

      return 
      end
***************************

      subroutine moveoutextint(filename,next,a,dim1,dim2,iou)
* Read an integer image from an extension
* dim1 and dim2 have to be the same size as NAXIS1 and NAXIS2 and the size of the fortran array
* possibly obselete
      character*(*) filename
      integer next
      integer dim1,dim2
      integer a(dim1,dim2)
      integer status,unit,readwrite,hdutype,group

      group=0
      status=0
      readwrite=1

*** open the file
      call ftgiou(unit,status)
      call ftnopn(unit,filename,readwrite,status)
      call printerror(status)

*** move the right HDU
      call ftmahd(unit,next+1,hdutype,status)
      call printerror(status)

*** actually write the data
      call ftp2dj(unit,group,dim1,dim1,dim2,a,status)
      call printerror(status)

      if (iou.eq.0) then
*** close the file and free the unit number
         call fitsclose(unit)
      else
         iou=unit
      endif
      return 
      end
***************************
