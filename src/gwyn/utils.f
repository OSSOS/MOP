        subroutine condargd(n,dog)
* this subroutine gets  the n-th (double) parameter from the
* command line. If there isn't a n-th parameter, it doesn't
* change the value of rog (so you can set a default before calling)
        character*40 cog
        double precision dog
        if (iargc().ge.n) then
                call getarg(n,cog)
                read(cog,*) dog
        endif
        return
        end
*****************************

	subroutine condargc(n,cog)
* this subroutine gets  the n-th (string) parameter from the 
* command line. If there isn't a n-th parameter, it doesn't
* change the value of cog (so you can set a default before calling)
	character*(*) cog
	if (iargc().ge.n) then
		call getarg(n,cog) 
	endif
	return
	end
*****************************

        subroutine condargi(n,iog) 
* this subroutine gets  the n-th (integer) parameter from the  
* command line. If there isn't a n-th parameter, it doesn't 
* change the value of rog (so you can set a default before calling) 
        character*40 cog 
        if (iargc().ge.n) then 
                call getarg(n,cog)  
                read(cog,*) rog 
                iog=nint(rog) 
        endif 
        return 
        end 
***************************** 


	subroutine condargr(n,rog)
* this subroutine gets  the n-th (real) parameter from the 
* command line. If there isn't a n-th parameter, it doesn't
* change the value of rog (so you can set a default before calling)
	character*40 cog
	if (iargc().ge.n) then
		call getarg(n,cog) 
		read(cog,*) rog
	endif
	return
	end
*****************************


	subroutine condargb(n,bog)
* this subroutine gets  the n-th (boolean/logical) parameter from the 
* command line. If there isn't a n-th parameter, it doesn't
* change the value of bog (so you can set a default before calling)
        logical bog
	character*40 cog
	if (iargc().ge.n) then
		call getarg(n,cog) 
                bog=.false.
                if (cog(1:1).eq.'y') bog=.true.
                if (cog(1:1).eq.'Y') bog=.true.
	endif
	return
	end
*****************************	
 
      SUBROUTINE sort2di(n,arr,brr)
      implicit none
      INTEGER n,M,NSTACK
      double precision arr(n)
      integer brr(n),tempb,b
      PARAMETER (M=7,NSTACK=50)
      INTEGER i,ir,j,jstack,k,l,istack(NSTACK)
      double precision a,tempa
      jstack=0
      l=1
      ir=n
1     if(ir-l.lt.M)then
        do 12 j=l+1,ir
          a=arr(j)
          b=brr(j)
          do 11 i=j-1,1,-1
            if(arr(i).le.a)goto 2
            arr(i+1)=arr(i)
            brr(i+1)=brr(i)
11        continue
          i=0
2         arr(i+1)=a
          brr(i+1)=b
12      continue
        if(jstack.eq.0)return
        ir=istack(jstack)
        l=istack(jstack-1)
        jstack=jstack-2
      else
        k=(l+ir)/2
        tempa=arr(k)
        arr(k)=arr(l+1)
        arr(l+1)=tempa
        tempb=brr(k)
        brr(k)=brr(l+1)
        brr(l+1)=tempb
        if(arr(l+1).gt.arr(ir))then
          tempa=arr(l+1)
          arr(l+1)=arr(ir)
          arr(ir)=tempa
          tempb=brr(l+1)
          brr(l+1)=brr(ir)
          brr(ir)=tempb
        endif
        if(arr(l).gt.arr(ir))then
          tempa=arr(l)
          arr(l)=arr(ir)
          arr(ir)=tempa
          tempb=brr(l)
          brr(l)=brr(ir)
          brr(ir)=tempb
        endif
        if(arr(l+1).gt.arr(l))then
          tempa=arr(l+1)
          arr(l+1)=arr(l)
          arr(l)=tempa
          tempb=brr(l+1)
          brr(l+1)=brr(l)
          brr(l)=tempb
        endif
        i=l+1
        j=ir
        a=arr(l)
        b=brr(l)
3       continue
          i=i+1
        if(arr(i).lt.a)goto 3
4       continue
          j=j-1
        if(arr(j).gt.a)goto 4
        if(j.lt.i)goto 5
        tempa=arr(i)
        arr(i)=arr(j)
        arr(j)=tempa
        tempb=brr(i)
        brr(i)=brr(j)
        brr(j)=tempb
        goto 3
5       arr(l)=arr(j)
        arr(j)=a
        brr(l)=brr(j)
        brr(j)=b
        jstack=jstack+2
        if(jstack.gt.NSTACK) then
           print*,'NSTACK too small in sort2'
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
***************

      FUNCTION selip(k,n,arr)
      implicit none
      INTEGER k,n,M
      double precision selip,arr(n),BIG
      PARAMETER (M=64,BIG=1d30)
CU    USES shell
      INTEGER i,j,jl,jm,ju,kk,mm,nlo,nxtmm,isel(M+2)
      double precision ahi,alo,sum,sel(M+2)
      if(k.lt.1.or.k.gt.n.or.n.le.0) then
         print*,'bad input to selip'
         stop
      endif
      kk=k
      ahi=BIG
      alo=-BIG
1     continue
        mm=0
        nlo=0
        sum=0d0
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
          selip=alo
          return
        else if(mm.le.M)then
          call shell(mm,sel)
          selip=sel(kk)
          return
        endif
        sel(M+1)=sum/mm
        call shell(M+1,sel)
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
****************

      SUBROUTINE shell(n,a)
      INTEGER n
      double precision a(n)
      INTEGER i,j,inc
      double precision v
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

****************

      SUBROUTINE svdfit(x,y,sig,ndata,a,ma,u,v,w,mp,np,chisq,funcs)
      implicit none
      INTEGER ma,mp,ndata,np,NMAX,MMAX
      double precision chisq,a(ma),sig(ndata),u(mp,np),v(np,np),w(np),
     &     x(ndata),y(ndata),TOL
      EXTERNAL funcs
      PARAMETER (NMAX=1000000,MMAX=300,TOL=1d-5)
CU    USES svbksb,svdcmp
      INTEGER i,j
      double precision sum,thresh,tmp,wmax,afunc(MMAX),b(NMAX)
      logical debug
      common /bugblk/ debug

      if (debug) write(*,'(a)') 'entering svdfit'

      do 12 i=1,ndata
        call funcs(x(i),afunc,ma)
        tmp=1d0/sig(i)
        do 11 j=1,ma
          u(i,j)=afunc(j)*tmp
11      continue
        b(i)=y(i)*tmp
12    continue
      call svdcmp(u,ndata,ma,mp,np,w,v)
      wmax=0d0
      do 13 j=1,ma
        if(w(j).gt.wmax)wmax=w(j)
13    continue
      thresh=TOL*wmax
      do 14 j=1,ma
        if(w(j).lt.thresh)w(j)=0d0
14    continue
      call svbksb(u,w,v,ndata,ma,mp,np,b,a)
      chisq=0d0
      do 16 i=1,ndata
        call funcs(x(i),afunc,ma)
        sum=0d0
        do 15 j=1,ma
          sum=sum+a(j)*afunc(j)
15      continue
        chisq=chisq+((y(i)-sum)/sig(i))**2
16    continue

      if (debug) write(*,'(a)') 'exiting  svdfit'
      return
      END
****************

      SUBROUTINE svbksb(u,w,v,m,n,mp,np,b,x)
      implicit none
      INTEGER m,mp,n,np,NMAX
      double precision b(mp),u(mp,np),v(np,np),w(np),x(np)
      PARAMETER (NMAX=500)
      INTEGER i,j,jj
      double precision s,tmp(NMAX)
      do 12 j=1,n
        s=0d0
        if(w(j).ne.0d0)then
          do 11 i=1,m
            s=s+u(i,j)*b(i)
11        continue
          s=s/w(j)
        endif
        tmp(j)=s
12    continue
      do 14 j=1,n
        s=0d0
        do 13 jj=1,n
          s=s+v(j,jj)*tmp(jj)
13      continue
        x(j)=s
14    continue
      return
      END
***********************

      SUBROUTINE svdcmp(a,m,n,mp,np,w,v)
      implicit none
      INTEGER m,mp,n,np,NMAX
      double precision a(mp,np),v(np,np),w(np)
      PARAMETER (NMAX=500)
CU    USES pythag
      INTEGER i,its,j,jj,k,l,nm
      double precision anorm,c,f,g,h,s,scale,x,y,z,rv1(NMAX),pythag
      g=0d0
      scale=0d0
      anorm=0d0
      do 25 i=1,n
        l=i+1
        rv1(i)=scale*g
        g=0d0
        s=0d0
        scale=0d0
        if(i.le.m)then
          do 11 k=i,m
            scale=scale+abs(a(k,i))
11        continue
          if(scale.ne.0d0)then
            do 12 k=i,m
              a(k,i)=a(k,i)/scale
              s=s+a(k,i)*a(k,i)
12          continue
            f=a(i,i)
            g=-sign(sqrt(s),f)
            h=f*g-s
            a(i,i)=f-g
            do 15 j=l,n
              s=0d0
              do 13 k=i,m
                s=s+a(k,i)*a(k,j)
13            continue
              f=s/h
              do 14 k=i,m
                a(k,j)=a(k,j)+f*a(k,i)
14            continue
15          continue
            do 16 k=i,m
              a(k,i)=scale*a(k,i)
16          continue
          endif
        endif
        w(i)=scale *g
        g=0d0
        s=0d0
        scale=0d0
        if((i.le.m).and.(i.ne.n))then
          do 17 k=l,n
            scale=scale+abs(a(i,k))
17        continue
          if(scale.ne.0d0)then
            do 18 k=l,n
              a(i,k)=a(i,k)/scale
              s=s+a(i,k)*a(i,k)
18          continue
            f=a(i,l)
            g=-sign(sqrt(s),f)
            h=f*g-s
            a(i,l)=f-g
            do 19 k=l,n
              rv1(k)=a(i,k)/h
19          continue
            do 23 j=l,m
              s=0d0
              do 21 k=l,n
                s=s+a(j,k)*a(i,k)
21            continue
              do 22 k=l,n
                a(j,k)=a(j,k)+s*rv1(k)
22            continue
23          continue
            do 24 k=l,n
              a(i,k)=scale*a(i,k)
24          continue
          endif
        endif
        anorm=max(anorm,(abs(w(i))+abs(rv1(i))))
25    continue
      do 32 i=n,1,-1
        if(i.lt.n)then
          if(g.ne.0d0)then
            do 26 j=l,n
              v(j,i)=(a(i,j)/a(i,l))/g
26          continue
            do 29 j=l,n
              s=0d0
              do 27 k=l,n
                s=s+a(i,k)*v(k,j)
27            continue
              do 28 k=l,n
                v(k,j)=v(k,j)+s*v(k,i)
28            continue
29          continue
          endif
          do 31 j=l,n
            v(i,j)=0d0
            v(j,i)=0d0
31        continue
        endif
        v(i,i)=1d0
        g=rv1(i)
        l=i
32    continue
      do 39 i=min(m,n),1,-1
        l=i+1
        g=w(i)
        do 33 j=l,n
          a(i,j)=0d0
33      continue
        if(g.ne.0d0)then
          g=1d0/g
          do 36 j=l,n
            s=0d0
            do 34 k=l,m
              s=s+a(k,i)*a(k,j)
34          continue
            f=(s/a(i,i))*g
            do 35 k=i,m
              a(k,j)=a(k,j)+f*a(k,i)
35          continue
36        continue
          do 37 j=i,m
            a(j,i)=a(j,i)*g
37        continue
        else
          do 38 j= i,m
            a(j,i)=0d0
38        continue
        endif
        a(i,i)=a(i,i)+1d0
39    continue
      do 49 k=n,1,-1
        do 48 its=1,30
          do 41 l=k,1,-1
            nm=l-1
            if((abs(rv1(l))+anorm).eq.anorm)  goto 2
            if((abs(w(nm))+anorm).eq.anorm)  goto 1
41        continue
1         c=0d0
          s=1d0
          do 43 i=l,k
            f=s*rv1(i)
            rv1(i)=c*rv1(i)
            if((abs(f)+anorm).eq.anorm) goto 2
            g=w(i)
            h=pythag(f,g)
            w(i)=h
            h=1d0/h
            c= (g*h)
            s=-(f*h)
            do 42 j=1,m
              y=a(j,nm)
              z=a(j,i)
              a(j,nm)=(y*c)+(z*s)
              a(j,i)=-(y*s)+(z*c)
42          continue
43        continue
2         z=w(k)
          if(l.eq.k)then
            if(z.lt.0d0)then
              w(k)=-z
              do 44 j=1,n
                v(j,k)=-v(j,k)
44            continue
            endif
            goto 3
          endif
          if(its.eq.40) then 
             print*, 'no convergence in svdcmp'
             stop
          endif
          x=w(l)
          nm=k-1
          y=w(nm)
          g=rv1(nm)
          h=rv1(k)
          f=((y-z)*(y+z)+(g-h)*(g+h))/(2d0*h*y)
          g=pythag(f,1d0)
          f=((x-z)*(x+z)+h*((y/(f+sign(g,f)))-h))/x
          c=1d0
          s=1d0
          do 47 j=l,nm
            i=j+1
            g=rv1(i)
            y=w(i)
            h=s*g
            g=c*g
            z=pythag(f,h)
            rv1(j)=z
            c=f/z
            s=h/z
            f= (x*c)+(g*s)
            g=-(x*s)+(g*c)
            h=y*s
            y=y*c
            do 45 jj=1,n
              x=v(jj,j)
              z=v(jj,i)
              v(jj,j)= (x*c)+(z*s)
              v(jj,i)=-(x*s)+(z*c)
45          continue
            z=pythag(f,h)
            w(j)=z
            if(z.ne.0d0)then
              z=1.0/z
              c=f*z
              s=h*z
            endif
            f= (c*g)+(s*y)
            x=-(s*g)+(c*y)
            do 46 jj=1,m
              y=a(jj,j)
              z=a(jj,i)
              a(jj,j)= (y*c)+(z*s)
              a(jj,i)=-(y*s)+(z*c)
46          continue
47        continue
          rv1(l)=0d0
          rv1(k)=f
          w(k)=x
48      continue
3       continue
49    continue
      return
      END
******************

      FUNCTION pythag(a,b)
      implicit none
      double precision a,b,pythag
      double precision absa,absb
      absa=abs(a)
      absb=abs(b)
      if(absa.gt.absb)then
        pythag=absa*sqrt(1.+(absb/absa)**2)
      else
        if(absb.eq.0d0)then
          pythag=0d0
        else
          pythag=absb*sqrt(1.+(absa/absb)**2)
        endif
      endif
      return
      END
*****************************

      function length(c)
      integer l
      character*(*) c
      l=len(c)
 10   continue
      if (c(l:l).eq.' ') then
         l=l-1
         goto 10
      endif
      length=l
      return
      end
*****************      

      character*2 function full(iin)
      character hex(0:9) /'0','1','2','3','4','5','6','7','8','9'/
      iq=iin-1
      if ((iq.lt.0).or.(iq.gt.99)) then
         print*,"ERROR: out of range in full"
         stop
      endif
      
      iq2=mod(iq,10)
      iq1=(iin-iq2)/10
      full=hex(iq1)//hex(iq2)

      return
      end
*******************

      subroutine  extend(ext,root)
      character*(*) ext,root
      logical isextend
      if (ext(1:1).eq.'.') then
         isextend=.true.
      else
         if ((index(ext,'.').eq.0).and.(length(ext).le.4)) then
            isextend=.true.
         else
            isextend=.false.   
         endif
      endif

      if (isextend) then
         if (ext(1:1).eq.'.') then
            ext=ext(2:len(ext))
         endif

         ldot=indexrev(root,'.')
         lend=length(root)
         latt=ldot
         if (ldot.eq.0) latt=lend
         if (root(lend-7:lend).eq.'.fits.fz') latt=lend-7
         ext=root(1:latt)//ext
      endif
      return
      end
*******************

      SUBROUTINE sort2(n,arr,brr)
      INTEGER n,M,NSTACK
      REAL arr(n),brr(n)
      PARAMETER (M=7,NSTACK=50)
      INTEGER i,ir,j,jstack,k,l,istack(NSTACK)
      REAL a,b,temp
      jstack=0
      l=1
      ir=n
1     if(ir-l.lt.M)then
        do 12 j=l+1,ir
          a=arr(j)
          b=brr(j)
          do 11 i=j-1,1,-1
            if(arr(i).le.a)goto 2
            arr(i+1)=arr(i)
            brr(i+1)=brr(i)
11        continue
          i=0
2         arr(i+1)=a
          brr(i+1)=b
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
        temp=brr(k)
        brr(k)=brr(l+1)
        brr(l+1)=temp
        if(arr(l+1).gt.arr(ir))then
          temp=arr(l+1)
          arr(l+1)=arr(ir)
          arr(ir)=temp
          temp=brr(l+1)
          brr(l+1)=brr(ir)
          brr(ir)=temp
        endif
        if(arr(l).gt.arr(ir))then
          temp=arr(l)
          arr(l)=arr(ir)
          arr(ir)=temp
          temp=brr(l)
          brr(l)=brr(ir)
          brr(ir)=temp
        endif
        if(arr(l+1).gt.arr(l))then
          temp=arr(l+1)
          arr(l+1)=arr(l)
          arr(l)=temp
          temp=brr(l+1)
          brr(l+1)=brr(l)
          brr(l)=temp
        endif
        i=l+1
        j=ir
        a=arr(l)
        b=brr(l)
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
        temp=brr(i)
        brr(i)=brr(j)
        brr(j)=temp
        goto 3
5       arr(l)=arr(j)
        arr(j)=a
        brr(l)=brr(j)
        brr(j)=b
        jstack=jstack+2
        if(jstack.gt.NSTACK) then
           print*, 'NSTACK too small in sort2'
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
*****************

      character*25 function when()
      integer n(8)
      character*25,a,b,c
      character*4 year
      character*2 month,day,hour,minute,seconds
      character*2 full
      call DATE_AND_TIME (a,b,c,n)
      write(year,'(i4)') n(1)
      month  =full(n(2)+1)
      day    =full(n(3)+1)
      hour   =full(n(5)+1)
      minute =full(n(6)+1)
      seconds=full(n(7)+1)
      write (a, 9999) year,month,day,hour,minute,seconds
 9999 format (a4,'-',a2,'-',a2,' ',a2,':',a2,':',a2,' (UTC)')
      when=a
      return
      end
****************

      integer function indexrev(c1,c2)
* returns index of the last instance of c2 in c1
* if c2 does not occur in c1, the value 0 is returned
* probably not programmed for optimal speed...
      implicit none
      integer l,l1,l2
      character*(*) c1,c2
      l1=len(c1)
      l2=len(c2)
      l=l1-l2+1
      if (l.lt.0) then  ! if c2 is shorter than c1
         indexrev=0     ! return
         return
      endif
 10   continue          !otherwise, start searching.
      if (c1(l:l+l2-1).ne.c2) then
         l=l-1
         if (l.eq.0) goto 20
         goto 10
      endif
 20   indexrev=l
      return
      end
*****************      

      subroutine escline(n,nmod)
      character*2 esc
      esc=char(27)//'['
      if (mod(n,nmod).eq.0) then
         print*,esc,'1M line:',n,esc//'1A'
      endif
      return
      end
****************
