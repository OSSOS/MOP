      program pitcairn
* compile with f77 -O5 -C -o pitcairn pitcairn.f mwcs.f utils.f wcs.f inoutext.f -L$HOME/cfitsio/lib/ -lcfitsio 
      integer dim1,dim2
      parameter(dim1=2112,dim2=4644)
      integer iim(dim1,dim2)
      real im(dim1,dim2)
      real flat(dim1,dim2)
      real overa,overb
      character*80 name,flatname,outname
      character*4 ext
      integer rdim1,rdim2,c
      integer status,readwrite,blocksize,group,nullval,reclen,bitpix
      integer hdutype,uo,ui
      integer getnextmax
      integer ibiasa1/1/   ,ibiasa2/32/  ,jbiasa1/1/,jbiasa2/4644/
      integer ibiasb1/2081/,ibiasb2/2112/,jbiasb1/1/,jbiasb2/4644/
      integer iampa1 /1/   ,iampa2/1056/ ,jampa1/1/ ,jampa2/4612/
      integer iampb1 /1057/,iampb2/2112/ ,jampb1/1/ ,jampb2/4612/
      integer idata1 /33/  ,idata2/2080/ ,jdata1/1/ ,jdata2/4612/
      logical anynull
      call getarg(1,flatname)
      call getarg(2,name)
      
      status=0
      group=0
      nullval=0

      norm=0
      next=getnextmax(name)

      outname=name
      i=index(outname,'o')
      outname(i:i)='p'


      do c=1,next 
         print*,'working on'//name(1:length(name))//' ccd',c
*** read the flat
         ui=0
         call fitsextopen(flatname,c,0,ui)
         call ftg2de(ui,0,nullval,dim1,dim1,dim2,flat,anynull,status)
         call printerror(status)
         call fitsclose(ui)

*** read the image
         ui=0
         call fitsextopen(name,c,0,ui)
         call ftg2de(ui,0,nullval,dim1,dim1,dim2,im,anynull,status)
         call printerror(status)
         call fitsclose(ui)

*** overscan correction
         overa=amodesec2d(im,ibiasa1,ibiasa2,jbiasa1,jbiasa2)
         overb=amodesec2d(im,ibiasb1,ibiasb2,jbiasb1,jbiasb2)
         print*,'removing overscan from amp A',overa
         do j=jampa1,jampa2
         do i=iampa1,iampa2
            im(i,j)=im(i,j)-overa
         enddo
         enddo
         print*,'done'
         
         print*,'removing overscan from amp B',overb
         do j=jampb1,jampb2
         do i=iampb1,iampb2
            im(i,j)=im(i,j)-overb
         enddo
         enddo
         print*,'done'
        
*** flat fielding
         print*,'flatfielding'
         do j=jdata1,jdata2
         do i=idata1,idata2
            im(i,j)=im(i,j)/flat(i,j)
         enddo
         enddo
         print*,'done'

*** convert back to int
         print*,'converting to intiger'
         do j=1,dim2
         do i=1,dim1
            iim(i,j)=nint(im(i,j))
            iim(i,j)=max(0    ,iim(i,j))
            iim(i,j)=min(65535,iim(i,j))
         enddo
         enddo
         print*,'done'

*** open the outputfile
*** write the data
         call fitsextopen(outname,c,1,uo)
         call ftp2dj(uo,group,dim1,dim1,dim2,iim,status)
         call printerror(status)
*** update the keywords
         call delreserved(uo,4)
         call ftukyf(uo,'BIAS0',overa,3,'Mean bias level',status)
         call ftukyf(uo,'BIAS1',overb,3,'Mean bias level',status)
         call ftukys(uo,'FLAT',flatname,'Flat Image',status)
         call ftukye(uo,'PITCAIRN',0.9,-1,'Version of Pitcairn',status)
         call fitsclose(uo)
*** close the file
      enddo


      stop
      end
****************
****************

      subroutine delreserved(unit,ndel)
*** delete some of the COMMENT  Reserved space.  This line can be used to add a new FITS card 
      integer unit,ndel
      integer keyse,keysa,status
      character*80 card
      status=0
      call ftghsp(unit,keyse,keysa,status)
      ndelled=0
      do n=1,keyse
         call ftgrec(unit,n,card,status)
         if (index(card,
     $        'This line can be used to add a new FITS card').gt.0) then
            call ftdrec(unit,n,status)
            call printerror(status)
            ndelled=ndelled+1
            if (ndelled.eq.ndel) goto 10
         endif
      enddo
 10   continue
      call ftghsp(unit,keyse,keysa,status)
      call printerror(status)
      return
      end
****************

      subroutine uphead(filename,next,overa,overb,flat,iou)
* update some relevant keywords
      character*(*) filename
      integer next
      integer status,unit,readwrite,hdutype,group
      character*40 value,comment
      character*80 card
      integer overa,overb
      character*40 flat
      group=0
      status=0
      readwrite=1
      print*,filename

*** if the file is open just use it
      if (iou.gt.0) then
         unit=iou
      else
*** maybe open the file
         call ftgiou(unit,status)
         call ftnopn(unit,filename,readwrite,status)
         call printerror(status)
      endif

*** move the right HDU
      call ftmahd(unit,next+1,hdutype,status)
      call printerror(status)

*** other fake elixir keywords

      call ftukyj(unit,'BIAS0',overa,'Mean bias level',status)
      call ftukyj(unit,'BIAS1',overb,'Mean bias level',status)
      call ftukys(unit,'FLAT',flat,'Flat Image',status)
      call ftukye(unit,'PITCAIRN',0.9,-1,'Version of Pitcairn',status)

      call printerror(status)

*** close the file and free the unit number


      return 
      end
***************************


