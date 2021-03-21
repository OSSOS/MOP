      program gcombx
* compile with f77 -O5 -C -mcmodel=medium -o gcombx gcombx.f mwcs.f utils.f wcs.f inoutext.f -L$HOME/cfitsio/lib/ -lcfitsio 
      integer dim1,dim2
      parameter(dim1=2112,dim2=4644,nimmax=100)
      real ims(dim1,dim2,nimmax)
      real imq(dim1,dim2)
      integer out(dim1,dim2)
      real overa,overb,norm
      real val(nimmax),sig(nimmax),gaina(nimmax),gainb(nimmax)
      real snorm(nimmax)
      character*80 name(nimmax),outname
      character*4 ext
      character*80 comment
      integer rdim1,rdim2,c
      integer status,readwrite,blocksize,group,nullval,reclen,bitpix
      integer getnextmax
      integer ibiasa1/1/   ,ibiasa2/32/  ,jbiasa1/1/,jbiasa2/4644/
      integer ibiasb1/2081/,ibiasb2/2112/,jbiasb1/1/,jbiasb2/4644/
      integer iampa1 /1/   ,iampa2/1056/ ,jampa1/1/ ,jampa2/4612/
      integer iampb1 /1057/,iampb2/2112/ ,jampb1/1/ ,jampb2/4612/
      integer idata1 /33/  ,idata2/2080/ ,jdata1/1/ ,jdata2/4612/
      logical anynull
      character*72 hist
      character*20 filter,crunid
      call getarg(1,filter)
      call getarg(2,crunid)
      outname='flat.'//filter(1:length(filter))//'.'
     $               //crunid(1:length(crunid))//'.fits';

      print*,outname
      nim=iargc()-2
      do n=3,nim+2
         call getarg(n,name(n-2))
         print*,n-2,name(n-2)
      enddo
      
      status=0
      group=0
      nullval=0
      norm=0
      !next=getnextmax(name(1))
      next=36

*** loop over extension 
      do c=1,next
*** loop over image
         do n=1,nim
            write(*,'(a15,"ccd",i3)') name(n),c
*** read the image extension
            call fitsextopen(name(n),c,0,ui)
            call ftgkye(ui,'GAINA',gaina(n),comment,status)
            call ftgkye(ui,'GAINB',gainb(n),comment,status)
            print*,gaina(n),gainb(n)
            call printerror(status)

            call ftg2de(ui,0,nullval,dim1,dim1,dim2,imq,anynull,status)


            call printerror(status)
            call fitsclose(ui)

*** overscan correction
            overa=amodesec2d(imq,ibiasa1,ibiasa2,jbiasa1,jbiasa2)
            overb=amodesec2d(imq,ibiasb1,ibiasb2,jbiasb1,jbiasb2)
            print*,'removing overscan from amp A',overa
            do j=jampa1,jampa2
            do i=iampa1,iampa2
                 imq(i,j)=imq(i,j)-overa
            enddo
            enddo
            
            print*,'removing overscan from amp B',overb
            do j=jampb1,jampb2
            do i=iampb1,iampb2
               imq(i,j)=imq(i,j)-overb
            enddo
            enddo

*** figure out the normaliztion
            print*,'finding normalization'
            !call bs2d(imq,idata1,idata2,jdata1,jdata2,norm,snorm(n))
            call bs2d(imq,iampa1,iampa2,jampa1,jampa2,anorm,snorm(n))
            call bs2d(imq,iampb1,iampb2,jampb1,jampb2,bnorm,snorm(n))
            snorm(n)=sqrt(norm*gaina(n))/gaina(n)
            !print*,norm,snorm(n)
            !norm=(anorm+bnorm)/2.
            norm=(anorm+bnorm)/2.
            !write(21,*) anorm,bnorm,anorm/bnorm,norm,name(n)


*** apply normalization and save that image in the stack
            do j=1,dim2
            do i=1,dim1
               ims(i,j,n)=imq(i,j)/norm
            enddo
            enddo
         enddo


*** zero the output image
         do j=1,dim2
         do i=1,dim1
            imq(i,j)=0
         enddo
         enddo
        

*** combine all the images using artifical skeptism
         print*,'combining images for CCD',c
         nimq=nint(0.4*nim)
         !do j=1,dim2
         !do i=1,dim1
         do j=jdata1,jdata2
         do i=idata1,idata2

            do n=1,nim
               val(n)=ims(i,j,n)
            enddo
            imq(i,j)=aselip(nimq,nim,val)

            !call askep(nim,val,snorm,cen,sig)
            !imq(i,j)=cen
         enddo
         enddo


         print*,'writing CCD',c,' to ',outname(1:length(outname))
         call fitsextopen(outname,c,1,uo)

         do n=1,nim 
            write(hist,9999) name(n),overa,overb,anorm,bnorm
 9999       format(a13,' overA',f8.2,' overB',f8.2,
     $                 ' normA',f8.2,' normB',f8.2)
           call ftphis(uo,hist,status)
           call printerror(status)

           call ftdkey(uo,'?RUNID',status)
           call ftdkey(uo,'FILTER',status)
           call printerror(status)

           call ftpkys(uo,'CRUNID',crunid(1:length(crunid))
     $          ,'Camera run is valid for',status)
           call ftpkys(uo,'FILTER',filter(1:length(filter))
     $          ,'Filter is valid for',status)

           call printerror(status)

         enddo
         call ftp2de(uo,group,dim1,dim1,dim2,imq,status)
         call printerror(status)
         call fitsclose(uo)
      enddo
       
      stop
      end
****************
****************

      subroutine bs2d(a,i1,i2,j1,j2,avg,rms)
      integer dim1,dim2
      parameter(dim1=2112,dim2=4644,nimmax=30)
      real work(dim1*dim2)
      real a(dim1,dim2)
      nwork=0
      do j=j1,j2
      do i=i1,i2
         nwork=nwork+1
         work(nwork)=a(i,j)
      enddo
      enddo
      call mad(work,nwork,avg,rms)
      avg=bsmode(work,nwork)

      return
      end
****************

	subroutine mad(a,n,avg,rms)
* returns the average and standard deviation of an array 
	dimension a(n)
	sum=0.
	sqr=0.
	do 100 i=1,n
100	sum=sum+a(i)
	avg=sum/real(n)
	do 110 i=1,n
110	sqr=sqr+(avg-a(i))**2
	if (n.le.1) then
	   rms = sqrt(sqr)
	else
	   rms=sqrt(sqr/real(n-1))
	endif
	return
	end
******************************

      real function bsmode(a,nmax)
* bisection search for mode
      implicit none 
      integer nmax,iter,i1,i2,i,imin,intsize
      real a(nmax),mean,med,medold,rangemin,range
      call bssort(nmax,a)


      iter=0
      i1=1
      i2=nmax
      !write(*,'(5a10)') 'first','last','a(first)','a(last)','range'
      !write(*,9999) i1,i2,a(i1),a(i2),a(i2)-a(i1)

 10   continue
      intsize=(i2-i1)/2
      do i=i1,i2-intsize
         range=a(i+intsize)-a(i)
         if (i.eq.i1) then
            imin=i1
            rangemin=range
         elseif (range.lt.rangemin) then
            imin=i
            rangemin=range
         endif
      enddo
      !write(*,9999) imin,imin+intsize,a(imin),a(imin+intsize),rangemin
      i1=imin
      i2=imin+intsize
      iter=iter+1
      if ((iter.lt.50).and.(i2.ne.i1)) goto 10
      bsmode = a(imin)
 9999 format(2i10,3f10.1)
      return
      end
****************

      SUBROUTINE bssort(n,arr)
      INTEGER n,M,NSTACK
      REAL arr(n)
      PARAMETER (M=7,NSTACK=5000)
      INTEGER i,ir,j,jstack,k,l,istack(NSTACK)
      REAL a,temp
      jstack=0
      l=1
      ir=n
1     if(ir-l.lt.M)then
        do 12 j=l+1,ir
          a=arr(j)
          do 11 i=j-1,1,-1
            if(arr(i).le.a)goto 2
            arr(i+1)=arr(i)
11        continue
          i=0
2         arr(i+1)=a
12      continue
        if(jstack.eq.0)return
        ir=istack(jstack)
        l=istack(jstack-1)
        jstack=jstack-2
      else
        k=(l+ir)/2
        temp=arr(k)
        arr(k)=arr(l+1)
        arr(l+1)=temp
        if(arr(l+1).gt.arr(ir))then
          temp=arr(l+1)
          arr(l+1)=arr(ir)
          arr(ir)=temp
        endif
        if(arr(l).gt.arr(ir))then
          temp=arr(l)
          arr(l)=arr(ir)
          arr(ir)=temp
        endif
        if(arr(l+1).gt.arr(l))then
          temp=arr(l+1)
          arr(l+1)=arr(l)
          arr(l)=temp
        endif
        i=l+1
        j=ir
        a=arr(l)
3       continue
          i=i+1
        if(arr(i).lt.a)goto 3
4       continue
          j=j-1
        if(arr(j).gt.a)goto 4
        if(j.lt.i)goto 5
        temp=arr(i)
        arr(i)=arr(j)
        arr(j)=temp
        goto 3
5       arr(l)=arr(j)
        arr(j)=a
        jstack=jstack+2
        if(jstack.gt.NSTACK) then
           print*,'NSTACK too small in sort'
           stop
        endif
        if(ir-i+1.ge.j-l)then
          istack(jstack)=ir
          istack(jstack-1)=i
          ir=j-1
        else
          istack(jstack)=j-1
          istack(jstack-1)=l
          l=i
        endif
      endif
      goto 1
      END
****************

      subroutine askep(nmax,a,s,aa,ss)
      real a(nmax),s(nmax)
      if (nmax.eq.0) then
         aa=0
         ss=0
         return
      endif
      n40=nint(real(nmax)*.5)
      ave=aselip(n40,nmax,a)
      !write(*,*) nmax,n40,ave,(a(i),i=1,nmax)
      ss=1
      aa=ave
      !return

      do iter=1,5
         asum=0.
         ssum=0.
         do n=1,nmax
            res=abs(a(n)-ave)
            weight=s(n)/(1.+(res**2.)*s(n)) ! assume s(n) is inverse variance
            asum=asum+weight*a(n)
            ssum=ssum+weight
         enddo
         ave=asum/ssum
      enddo
      aa=ave
      ss=0
      do n=1,nmax
         ss=ss+1/s(n)
      enddo
      ss=sqrt(ss)/real(nmax)
      return
      end
*****************      


      real function amed(n,a)
      real a(n)
* returns the median of an array a which is n long
      if ((n.gt.100).or.(real(n)/2).ne.int(real(n)/2)) then
* if the array is long or odd, just take the middle-most
         amed=amedselect(n/2+1,n,a)
      else
* if the array is even and short, average the two middle-most
         amed=(amedselect(n/2,n,a)+amedselect(n/2+1,n,a))/2.
      endif
      return
      end
***************

      FUNCTION amedselect(k,n,arr)
      INTEGER k,n
      REAL amedselect,arr(n)
      INTEGER i,ir,j,l,mid
      REAL a,temp
      l=1
      ir=n
1     if(ir-l.le.1)then
        if(ir-l.eq.1)then
          if(arr(ir).lt.arr(l))then
            temp=arr(l)
            arr(l)=arr(ir)
            arr(ir)=temp
          endif
        endif
        amedselect=arr(k)
        return
      else
        mid=(l+ir)/2
        temp=arr(mid)
        arr(mid)=arr(l+1)
        arr(l+1)=temp
        if(arr(l+1).gt.arr(ir))then
          temp=arr(l+1)
          arr(l+1)=arr(ir)
          arr(ir)=temp
        endif
        if(arr(l).gt.arr(ir))then
          temp=arr(l)
          arr(l)=arr(ir)
          arr(ir)=temp
        endif
        if(arr(l+1).gt.arr(l))then
          temp=arr(l+1)
          arr(l+1)=arr(l)
          arr(l)=temp
        endif
        i=l+1
        j=ir
        a=arr(l)
3       continue
          i=i+1
        if(arr(i).lt.a)goto 3
4       continue
          j=j-1
        if(arr(j).gt.a)goto 4
        if(j.lt.i)goto 5
        temp=arr(i)
        arr(i)=arr(j)
        arr(j)=temp
        goto 3
5       arr(l)=arr(j)
        arr(j)=a
        if(j.ge.k)ir=j-1
        if(j.le.k)l=i
      endif
      goto 1
      END
*******************

      FUNCTION aselip(k,n,arr)
      INTEGER k,n,M
      REAL aselip,arr(n),BIG
      PARAMETER (M=64,BIG=1.E30)
CU    USES ashell
      INTEGER i,j,jl,jm,ju,kk,mm,nlo,nxtmm,isel(M+2)
      REAL ahi,alo,sum,sel(M+2)
      if(k.lt.1.or.k.gt.n.or.n.le.0) then
         print*,'bad input to aselip'
         stop
      endif
      kk=k
      ahi=BIG
      alo=-BIG
1     continue
        mm=0
        nlo=0
        sum=0.
        nxtmm=M+1
        do 11 i=1,n
          if(arr(i).ge.alo.and.arr(i).le.ahi)then
            mm=mm+1
            if(arr(i).eq.alo) nlo=nlo+1
            if(mm.le.M)then
              sel(mm)=arr(i)
            else if(mm.eq.nxtmm)then
              nxtmm=mm+mm/M
              sel(1+mod(i+mm+kk,M))=arr(i)
            endif
            sum=sum+arr(i)
          endif
11      continue
        if(kk.le.nlo)then
          aselip=alo
          return
        else if(mm.le.M)then
          call ashell(mm,sel)
          aselip=sel(kk)
          return
        endif
        sel(M+1)=sum/mm
        call ashell(M+1,sel)
        sel(M+2)=ahi
        do 12 j=1,M+2
          isel(j)=0
12      continue
        do 13 i=1,n
          if(arr(i).ge.alo.and.arr(i).le.ahi)then
            jl=0
            ju=M+2
2           if(ju-jl.gt.1)then
              jm=(ju+jl)/2
              if(arr(i).ge.sel(jm))then
                jl=jm
              else
                ju=jm
              endif
            goto 2
            endif
            isel(ju)=isel(ju)+1
          endif
13      continue
        j=1
3       if(kk.gt.isel(j))then
          alo=sel(j)
          kk=kk-isel(j)
          j=j+1
        goto 3
        endif
        ahi=sel(j)
      goto 1
      END
************************

      SUBROUTINE ashell(n,a)
      INTEGER n
      REAL a(n)
      INTEGER i,j,inc
      REAL v
      inc=1
1     inc=3*inc+1
      if(inc.le.n)goto 1
2     continue
        inc=inc/3
        do 11 i=inc+1,n
          v=a(i)
          j=i
3         if(a(j-inc).gt.v)then
            a(j)=a(j-inc)
            j=j-inc
            if(j.le.inc)goto 4
          goto 3
          endif
4         a(j)=v
11      continue
      if(inc.gt.1)goto 2
      return
      END
************************


