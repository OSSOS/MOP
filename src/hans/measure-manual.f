      implicit NONE

      integer i

      integer iah,iam,idh,idm
      integer icode
      integer narg
      integer len_image_name
      integer rah, ram, decd, decm, sidec
      integer iargc
      external iargc

      real*8 cand_x,cand_y

      real*8 alpha_cand,delta_cand,rms
      real*8 xias,hds

      character*100 filename_plate_solution
      character*100 filename_stars_coord
      character*100 filename_cand_alpha_delta
      character*100 filename_usno_coord_mag
      character*80 line
      character*80 filename_moving_objects

      character*1 sign,c1
      character*80 filename_image,arg

      rms = 0.d0

      narg = iargc()
      i = 1
      call getarg (i, arg)
      filename_image = arg

      i = 1
      do while(filename_image(i:i).ne.' ')
        i = i+1
      end do
      i = i-1
      len_image_name = i

      i = 2
      call getarg (i, arg)
      filename_moving_objects = arg

      filename_cand_alpha_delta = 'MPC-'//filename_moving_objects

      open(11,file=filename_moving_objects,status='old',err=100)
      open(12,file=filename_cand_alpha_delta,status='unknown')

      line = filename_image
      filename_stars_coord = line(1:len_image_name)
     *                           //'.bright.jmp'
      filename_plate_solution = line(1:len_image_name)
     *                           //'.platesol'
      filename_usno_coord_mag = line(1:len_image_name)
     *                           //'.usno'

   90 continue
        read (11, *, end=100, err=100) cand_x, cand_y

c     astrometry
c     ----------
      
        call find_alpha_delta_linear(cand_x,cand_y,
     *              alpha_cand,delta_cand,
     *              filename_usno_coord_mag,
     *              filename_plate_solution,
     *                  icode)
        if(icode.ne.0) then
          write(6,*) 'icode = ',icode
          stop
        end if

c Change to hours and degrees, and format output.
        alpha_cand = alpha_cand/15.d0
        sidec = 1
        if (delta_cand .lt. 0.d0) then
           sidec = -1
           delta_cand = -delta_cand
        end if
        rah = int(alpha_cand)
        alpha_cand = (alpha_cand - dble(rah))*60.d0
        ram = int(alpha_cand)
        alpha_cand = (alpha_cand - dble(ram))*60.d0
        decd = int(delta_cand)
        delta_cand = (delta_cand - dble(decd))*60.d0
        decm = int(delta_cand)
        delta_cand = (delta_cand - dble(decm))*60.d0

        if (sidec .lt. 0) then
           write(12,2012) rah, ram, alpha_cand, decd, decm, delta_cand
        else
           write(12,2013) rah, ram, alpha_cand, decd, decm, delta_cand
        end if
 2012   format(1x, i2.2, 1x, i2.2, f6.2, ' -', i2.2, 1x, i2.2, f5.1)
 2013   format(1x, i2.2, 1x, i2.2, f6.2, ' +', i2.2, 1x, i2.2, f5.1)

      goto 90

  100 continue
      stop
      end

c     ------------------------
      real*8 function sind(x)
      real*8 x,pie,pierez
      pie = 3.141592653589793D0
      pierez = pie/180.d0
      sind = dsin(x*pierez)
      return
      end
c     ------------------------
      real*8 function cosd(x)
      real*8 x,pie,pierez

      pie = 3.141592653589793D0
      pierez = pie/180.d0
      cosd = dcos(x*pierez)
      return
      end
c     ------------------------
      real*8 function tand(x)
      real*8 x,pie,pierez

      pie = 3.141592653589793D0
      pierez = pie/180.d0
      tand = dtan(x*pierez)
      return
      end


c     ------------------------------------------
c1111
      subroutine find_alpha_delta_linear(ccand_x,ccand_y,
     *              alpha_cand,delta_cand,
     *              filename_usno_coord_mag,
     *              filename_plate_solution,
     *                  icode)

      implicit NONE

      integer max_usno,n_refstars
      parameter(max_usno=5000)
      parameter(n_refstars=20)

      integer i,j,nz,l,icode,k,ierr
      integer naxis1,naxis2
      integer n_axis1_meaning,n_axis2_meaning
      integer n_axis1_direction,n_axis2_direction
      integer indx_sort(max_usno)

      real*8 star_coord(max_usno,2),star_coord_sort(max_usno,2)
      real*8 xmag
      real*8 usno_alpha(max_usno),usno_alpha_sort(max_usno)
      real*8 usno_delta(max_usno),usno_delta_sort(max_usno)
      real*8 dist_pix_2(max_usno),dist_pix_2_sort(max_usno)
      real*8 dist_2_arcsec(max_usno),dist_2_arcsec_sort(max_usno)
      real*8 focal_length,x_off,y_off
      real*8 alpha_proj_center_deg,delta_proj_center
      real*8 pie,pierez
      real*8 cand_x,cand_y
      real*8 ccand_x,ccand_y
      real*8 aa,bb,da,dd,rms
      real*8 ttan
      real*8 eta,zeta,alpha_cand,delta_cand
      real*8 alphaz_deg,deltaz_deg
      real*8 x_ast,y_ast,x_orig,y_orig
      real*8 x_gnom,y_gnom,alpha_deg,delta_deg
      real*8 usno_gnom(n_refstars,2),coord_rel_gnom(n_refstars,2)
      real*8 xx(2*n_refstars),xx1(2*n_refstars),
     *       t(2*n_refstars,6),t1(2*n_refstars,6)
      real*8 w(6),plc(6),rv(6),pplc(6,1)
      real*8 kn
      real*8 tand,sind,cosd
      external tand,sind,cosd


      character*1 minus,axis1_meaning,axis2_meaning,sign,
     *             axis1_direction,axis2_direction

      character*100 filename_usno_coord_mag
      character*100 filename_plate_solution


      icode = 0
      pie = 3.141592653589793D0
      pierez = pie/180.d0
      minus = '-'


c     determine alpha,delta of candidates
c     -----------------------------------
      open(4,err=10,file=filename_plate_solution,status='old')

      read(4,4000) n_axis1_meaning,n_axis1_direction,
     *              n_axis2_direction
 4000 format(3i3)
      read(4,4001) focal_length,x_off,y_off
 4001 format(3d25.15)
      read(4,4002) alpha_proj_center_deg,delta_proj_center
 4002 format(2d25.15)
      close(4)

c     find closest reference stars
c     ----------------------------
      open(4,file=filename_usno_coord_mag,status='old')
      i = 0
   20 i = i+1
      if(i.gt.max_usno) then
        write(6,*) 
     * 'increase max_usno in subroutine find_alpha_delta_linear'
        icode = 99
        return
      end if
      read(4,*,end=30) star_coord(i,1),star_coord(i,2),
     *                 xmag,j,usno_alpha(i),usno_delta(i),
     *                 dist_2_arcsec(i)
      dist_pix_2(i) = (star_coord(i,1)-ccand_x)**2+
     *                (star_coord(i,2)-ccand_y)**2
      dist_pix_2(i) = dist_pix_2(i)
      goto 20
   30 nz = i-1
      call sortreal8(nz,dist_pix_2,indx_sort)
      do k = 1,nz
        i = indx_sort(k)
        star_coord_sort(k,1) = star_coord(i,1)
        star_coord_sort(k,2) = star_coord(i,2)
        dist_2_arcsec(k) = dist_2_arcsec(i)
        usno_alpha_sort(k) = usno_alpha(i)
        usno_delta_sort(k) = usno_delta(i)
        dist_pix_2_sort(k) = dsqrt(dist_pix_2(i))
      end do

      x_orig = x_off
      y_orig = y_off

      axis1_direction = ' '
      if(n_axis1_direction.lt.0) axis1_direction = minus
      axis2_direction = ' '
      if(n_axis2_direction.lt.0) axis2_direction = minus

      cand_x = ccand_x-x_orig
      cand_y = ccand_y-y_orig

      if(n_axis1_meaning.eq.2) then
        aa = cand_y
        bb = cand_x
        cand_x = aa
        cand_y = bb
      end if
      if(axis1_direction.eq.minus) cand_x = -cand_x
      if(axis2_direction.eq.minus) cand_y = -cand_y

      cand_x = cand_x/focal_length
      cand_y = cand_y/focal_length

      do i=1,2*n_refstars
        do j=1,6
          t(i,j) = 0.d0
        end do
      end do

      do i = 1,n_refstars
        coord_rel_gnom(i,1) = 
     *       (star_coord_sort(i,1)-x_orig)/focal_length
        coord_rel_gnom(i,2) = 
     *       (star_coord_sort(i,2)-y_orig)/focal_length
      end do
      do i = 1,n_refstars
      if(n_axis1_meaning.eq.2) then
        aa = coord_rel_gnom(i,2)
        bb = coord_rel_gnom(i,1)
        coord_rel_gnom(i,1) = aa
        coord_rel_gnom(i,2) = bb
      end if
      if(axis1_direction.eq.minus) 
     * coord_rel_gnom(i,1) = -coord_rel_gnom(i,1)
      if(axis2_direction.eq.minus) 
     *  coord_rel_gnom(i,2) = -coord_rel_gnom(i,2) 
      end do
      do i = 1,n_refstars
        alpha_deg = usno_alpha_sort(i)
        delta_deg = usno_delta_sort(i)
        call equa_deg_to_xygnom(alpha_deg,delta_deg,
     *      alpha_proj_center_deg,delta_proj_center,
     *      x_gnom,y_gnom)
        usno_gnom(i,1) = x_gnom
        usno_gnom(i,2) = y_gnom
      end do
      do i = 1,n_refstars
        t(i,1) =1
        t(i,2) = coord_rel_gnom(i,1)
        t(i,3) = coord_rel_gnom(i,2)
        xx(i) = usno_gnom(i,1)
        xx(i+n_refstars) = usno_gnom(i,2)
        do l=1,3
          t(i+n_refstars,l+3) = t(i,l)
        end do
      end do
      do i=1,2*n_refstars
        xx1(i) = xx(i)
        do j=1,6
          t1(i,j) = t(i,j)
        end do
      end do

      call minfitpack (2*n_refstars,2*n_refstars,6,t1,w,1,xx1,ierr,rv,
     *  pplc,6,kn)
      if(ierr.ne.0) then
      write(6,*) 'ierr = ',ierr
      write(6,*) 'NO ASTROMETRY POSSIBLE'
      icode = 42
      return
      end if
      do i = 1,6
        plc(i) = pplc(i,1)
      end do

      x_ast = cand_x
      y_ast = cand_y
      zeta =  plc(1)+plc(2)*x_ast+plc(3)*y_ast

      eta  =  plc(4)+plc(5)*x_ast+plc(6)*y_ast


       alphaz_deg = alpha_proj_center_deg
       deltaz_deg = delta_proj_center


       da = atan(zeta/(cosd(deltaz_deg)-eta*sind(deltaz_deg)))
       da = da/pierez
       alpha_cand  = alphaz_deg + da

       ttan   = tand((alpha_cand - alphaz_deg)/2.)
       dd =       asin((eta-(zeta*sind(deltaz_deg)*ttan))/
     *            dsqrt(zeta**2+eta**2+1 ))
       dd = dd/pierez
       delta_cand = deltaz_deg + dd
c      write(6,*) alpha_proj_center_deg,delta_proj_center
c      write(6,*) 'da,dd',da*3600.,dd*3600.
c     read(5,*)

      return
   10 icode = 1
      return
      end 

      subroutine sortreal8(n,arrin,indx)
      implicit real*8(a-h,o-z)
      dimension arrin(n),indx(n)
      do j = 1,n
        indx(j) = j
      end do
      l = n/2+1
      ir = n
   10 continue
      if(l.gt.1) then
        l = l-1
        indxt = indx(l)
        q = arrin(indxt)
      else
        indxt = indx(ir)
        q = arrin(indxt)
        indx(ir) = indx(1)
        ir = ir-1
        if(ir.eq.1) then
          indx(1) = indxt
          return
        endif
      endif
      i = l
      j = l+l
   20 if(j.le.ir) then
        if(j.lt.ir) then
          if(arrin(indx(j)).lt.arrin(indx(j+1))) j = j+1
        end if
        if(q.lt.arrin(indx(j))) then
          indx(i) = indx(j)
          i = j
          j = j+j
        else
          j = ir+1
        end if
      goto 20
      end if
      indx(i) = indxt
      goto 10
      end


c     *************************************************************
        subroutine equa_deg_to_xygnom(alpha_deg,delta_deg,
     *                 alphaz_deg,deltaz_deg,x_gnom,y_gnom)

c
c***********************************************************
c  this subroutine determines the normalized rectangular coordinates x_g
c  y_gnom, from alpha (alpha_deg), delta (delta_deg),
c   alpha0 (alphaz_deg) and delta0 (deltaz_deg)
c  using gnomonic (tangential plane) projection
c****************************<*******************************

        implicit none
c
        real*8  x_gnom,y_gnom
        real*8  delta_deg,alpha_deg
        real*8  deltaz_deg,alphaz_deg,sdelz,cdelz
        real*8  sind,cosd
        external sind,cosd


c       projection gnomonique
c       --------------------
        sdelz =sind(deltaz_deg)
        cdelz =cosd(deltaz_deg)
c
        x_gnom = (cosd(delta_deg)*sind(alpha_deg-alphaz_deg))/
     *           (sind(delta_deg)*  sdelz+cosd(delta_deg)*
     *           cdelz*cosd(alpha_deg-alphaz_deg))

c
        y_gnom = ((sind(delta_deg)*cdelz-cosd(delta_deg)*
     *           sdelz*cosd(alpha_deg-alphaz_deg)))/(sind(delta_deg)*
     *           sdelz+cosd(delta_deg)*cdelz*cosd(alpha_deg-alphaz_deg))

c
        return
        end

                                                                     

      subroutine minfitpack(nm,m,n,a,w,ip,b,ierr,rv,aplusb,lab,kn)   
                                                                        
c         normalisation de la matrice a.                                
c         seule modif par rapport a minfit  : il faut donner  nouveau   
c         parametre : lab qui est le nombre de ligne de aplusb dans les 
c         declarations du programme principal : aplusb (lab,...)        
c         kn = condition-number                                         
                                                                        
*declarations                                                           
*~~~~~~~~~~~~                                                           
                                                                        
        implicit        none                                            
                                                                        
        integer*4       i,j,k,ip,ierr,nm,n,m,lab                        
        real*8          a(nm,*),b(nm,*),w(*),rv(*),aplusb(lab,*)
        real*8          coef_normalisation(200),toto,titi,kn            
                                                                        
*mise a zero du tableau qui contiendra aplus * b                        
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~                        
                                                                        
        do i=1,n                                                        
          do j=1,ip                                                     
            aplusb(i,j) = 0.d0                                          
          end do                                                        
        end do                                                          
                                                                        
*normalisation de a                                                     
*~~~~~~~~~~~~~~~~~~                                                     
                                                                        
          do j=1,n                                                      
            coef_normalisation(j) = 0.d0                                
            do i=1,m                                                    
              if (abs(a(i,j)).gt.coef_normalisation(j)) then            
                coef_normalisation(j) = abs(a(i,j))                     
              end if                                                    
            end do                                                      
          end do                                                        
                                                                        
          do i=1,m                                                      
            do j=1,n                                                    
              if (coef_normalisation(j).ne.0) a(i,j) = a(i,j)/          
     1                              coef_normalisation(j)               
            end do                                                      
          end do                                                        
                                                                        
*calcul par minfit                                                      
*~~~~~~~~~~~~~~~~~                                                      
                                                                        
        call minfit (nm,m,n,a,w,ip,b,ierr,rv)                           
                                                                        
        do k =1,n                                                       
          if(w(k).lt.1.d-15) go to 100                                  
          do i=1,n                                                      
            do j=1,ip                                                   
              aplusb(i,j) = aplusb(i,j) + a(i,k)*b(k,j)/w(k)            
            end do                                                      
          end do                                                        
 100    continue                                                        
        end do                                                          
                                                                        
*denormalisation de aplusb                                              
*~~~~~~~~~~~~~~~~~~~~~~~~~                                              
                                                                        
          do i=1,n                                                      
            do j=1,ip                                                   
              if (coef_normalisation(i).ne.0) aplusb(i,j) = aplusb(i,j)/
     1       coef_normalisation(i)                                      
            end do                                                      
          end do                                                        
                                                                        
*nombre evaluant le conditionnement de la matrice                       
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~                       
                                                                        
        toto = 0.d0                                                     
        titi = 100.d0                                                   
        do i=1,n                                                        
          if (toto.lt.abs(w(i)))toto = abs(w(i))                        
          if (titi.gt.abs(w(i)))titi = abs(w(i))                        
        end do                                                          
        kn      =  toto/titi                                            
*fin                                                                    
*~~~                                                                    
                                                                        
        return                                                          
        end                                                             
                                                                        
        subroutine minfit(nm,m,n,a,w,ip,b,ierr,rv1)                     
        real*8 a(nm,n),w(n),b(nm,*),rv1(n)                              
        real*8 c,f,g,h,s,x,y,z,eps,scale,machep                         
        machep=10.d-16                                                  
        ierr=0                                                          
        g=0.d0                                                            
        scale=0.d0                                                        
        x=0.d0                                                            
        do 300 i=1,n                                                    
        l=i+1                                                           
        rv1(i)=scale*g                                                  
        g=0.d0                                                            
        s=0.d0                                                            
        scale=0.d0                                                        
        if(i.gt.m) go to 210                                            
        do 120 k=i,m                                                    
 120    scale=scale+dabs(a(k,i))                                        
        if(scale.eq.0.d0) go to 210                                       
        do 130 k=i,m                                                    
        a(k,i)=a(k,i)/scale                                             
        s=s+a(k,i)**2                                                   
 130    continue                                                        
        f=a(i,i)                                                        
        g=-sign(dsqrt(s),f)                                             
        h=f*g-s                                                         
        a(i,i)=f-g                                                      
        if(i.eq.n) go to 160                                            
        do 150 j=l,n                                                    
        s=0.d0                                                            
        do 140 k=i,m                                                    
 140    s=s+a(k,i)*a(k,j)                                               
        f=s/h                                                           
        do 150 k=i,m                                                    
        a(k,j)=a(k,j)+f*a(k,i)                                          
 150    continue                                                        
 160    if(ip.eq.0) go to 190                                           
        do 180 j=1,ip                                                   
        s=0.d0                                                            
        do 170 k=i,m                                                    
 170    s=s+a(k,i)*b(k,j)                                               
        f=s/h                                                           
        do 180 k=i,m                                                    
        b(k,j)=b(k,j)+f*a(k,i)                                          
 180    continue                                                        
 190    do 200 k=i,m                                                    
 200    a(k,i)=scale*a(k,i)                                             
 210    w(i)=scale*g                                                    
        g=0.d0                                                            
        s=0.d0                                                            
        scale=0.d0                                                        
        if(i.gt.m.or.i.eq.n) go to 290                                  
        do 220 k=l,n                                                    
 220    scale=scale+dabs(a(i,k))                                        
        if(scale.eq.0.d0) go to 290                                       
        do 230 k=l,n                                                    
        a(i,k)=a(i,k)/scale                                             
        s=s+a(i,k)**2                                                   
 230    continue                                                        
        f=a(i,l)                                                        
        g=-sign(dsqrt(s),f)                                             
        h=f*g-s                                                         
        a(i,l)=f-g                                                      
        do 240 k=l,n                                                    
 240    rv1(k)=a(i,k)/h                                                 
        if(i.eq.m) go to 270                                            
        do 260 j=l,m                                                    
        s=0.d0                                                            
        do 250 k=l,n                                                    
 250    s=s+a(j,k)*a(i,k)                                               
        do 260 k=l,n                                                    
        a(j,k)=a(j,k)+s*rv1(k)                                          
 260    continue                                                        
 270    do 280 k=l,n                                                    
 280    a(i,k)=scale*a(i,k)                                             
 290    x=dmax1(x,dabs(w(i))+dabs(rv1(i)))                              
 300    continue                                                        
        do 400 ii=1,n                                                   
        i=n+1-ii                                                        
        if(i.eq.n) go to 390                                            
        if(g.eq.0) go to 360                                            
        do 320 j=l,n                                                    
 320    a(j,i)=(a(i,j)/a(i,l))/g                                        
        do 350 j=l,n                                                    
        s=0.d0                                                            
        do 340 k=l,n                                                    
 340    s=s+a(i,k)*a(k,j)                                               
        do 350 k=l,n                                                    
        a(k,j)=a(k,j)+s*a(k,i)                                          
 350     continue                                                       
 360     do 380 j=l,n                                                   
        a(i,j)=0.d0                                                       
        a(j,i)=0.d0                                                       
 380    continue                                                        
 390    a(i,i)=1.d0                                                       
        g=rv1(i)                                                        
        l=i                                                             
 400    continue                                                        
        if(m.ge.n.or.ip.eq.0) go to 510                                 
        m1=m+1                                                          
        do 500 i=m1,n                                                   
        do 500 j=1,ip                                                   
        b(i,j)=0.d0                                                       
 500    continue                                                        
 510    eps=machep*x                                                    
        do 700 kk=1,n                                                   
        k1=n-kk                                                         
        k=k1+1                                                          
        its=0                                                           
 520    do 530 ll=1,k                                                   
        l1=k-ll                                                         
        l=l1+1                                                          
        if(dabs(rv1(l)).le.eps) go to 565                               
        if(dabs(w(l1)).le.eps) go to 540                                
 530    continue                                                        
 540    c=0.d0                                                            
        s=1.d0                                                            
        do 560 i=l,k                                                    
        f=s*rv1(i)                                                      
        rv1(i)=c*rv1(i)                                                 
        if(dabs(f).le.eps) go to 565                                    
        g=w(i)                                                          
        h=dsqrt(f*f+g*g)                                                
        w(i)=h                                                          
        c=g/h                                                           
        s=-f/h                                                          
        if(ip.eq.0) go to 560                                           
        do 550 j=1,ip                                                   
        y=b(l1,j)                                                       
        z=b(i,j)                                                        
        b(l1,j)=y*c+z*s                                                 
        b(i,j)=-y*s+z*c                                                 
 550    continue                                                        
 560    continue                                                        
 565    z=w(k)                                                          
        if(l.eq.k) go to 650                                            
        if(its.eq.30) go to 1000                                        
        its=its+1                                                       
        x=w(l)                                                          
        y=w(k1)                                                         
        g=rv1(k1)                                                       
        h=rv1(k)                                                        
        f=((y-z)*(y+z)+(g-h)*(g+h))/2.*h*y                              
        g=dsqrt(f*f+1.)                                                 
        f=((x-z)*(x+z)+h*(y/(f+dsign(g,f))-h))/x                        
        c=1.d0                                                            
        s=1.d0                                                            
        do 600 i1=l,k1                                                  
        i=i1+1                                                          
        g=rv1(i)                                                        
        y=w(i)                                                          
        h=s*g                                                           
        g=c*g                                                           
        z=dsqrt(f*f+h*h)                                                
        rv1(i1)=z                                                       
        c=f/z                                                           
        s=h/z                                                           
        f=x*c+g*s                                                       
        g=-x*s+g*c                                                      
        h=y*s                                                           
        y=y*c                                                           
        do 570 j=1,n                                                    
        x=a(j,i1)                                                       
        z=a(j,i)                                                        
        a(j,i1)=x*c+z*s                                                 
        a(j,i)=-x*s+z*c                                                 
 570    continue                                                        
        z=dsqrt(f*f+h*h)                                                
        w(i1)=z                                                         
        if(z.eq.0.d0) go to 580                                           
        c=f/z                                                           
        s=h/z                                                           
 580    f=c*g+s*y                                                       
        x=-s*g+c*y                                                      
        if(ip.eq.0) go to 600                                           
        do 590 j=1,ip                                                   
        y=b(i1,j)                                                       
        z=b(i,j)                                                        
        b(i1,j)=y*c+z*s                                                 
        b(i,j)=-y*s+z*c                                                 
 590    continue                                                        
 600    continue                                                        
        rv1(l)=0.d0                                                       
        rv1(k)=f                                                        
        w(k)=x                                                          
        go to 520                                                       
 650    if(z.ge.0.d0) go to 700                                           
        w(k)=-z                                                         
        do 690 j=1,n                                                    
 690    a(j,k)=-a(j,k)                                                  
 700    continue                                                        
        go to 1001                                                      
 1000   ierr=k                                                          
 1001   return                                                          
        end                                                             
                                                                        
        subroutine dproduit (a,b,c,u,v,w,ub,vb,wb)                      
                                                                        
************************************************************************
*                                                                       
*                                                                       
*       sous_programme dproduit.for                                     
*       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~                                    
*                                                                       
*         ce programme multiplie les matrices a et b et sort le         
*       resultat c en real*8                                            
*         effectue   :          a * b = c                               
*                                                                       
*                                                                       
*       u,v,w sont les dimensions declarees dans le programme principal.
*       a(u,v)   b(v,w)    c(u,w)                                       
*                                                                       
*       ub,vb,wb   sont les dimensions utilisables des matrices         
*                                                                       
************************************************************************
                                                                        
                                                                        
*declarations                                                           
*~~~~~~~~~~~~                                                           
                                                                        
        implicit        real*8 (a-h,o-z)                                
        integer         u,v,w,ub,vb,wb                                  
        real*8          a(u,v),b(v,w),c(u,w)                            
                                                                        
*test d'erreur sur les dimensions d'appel                               
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~                               
                                                                        
        if (ub.gt.u.or.vb.gt.v.or.wb.gt.w) then                         
         print *,' dimensions d`appel de dproduit incorrectes'          
          stop                                                          
        end if                                                          
                                                                        
*calcul                                                                 
*~~~~~~                                                                 
                                                                        
        do i=1,u                                                        
          do j=1,w                                                      
            c(i,j) = 0.d0                                                 
          end do                                                        
        end do                                                          
                                                                        
        do i=1,ub                                                       
          do j=1,wb                                                     
            do k=1,vb                                                   
               c(i,j)=c(i,j)+a(i,k)*b(k,j)                              
            end do                                                      
          end do                                                        
        end do                                                          
                                                                        
*fin                                                                    
*~~~                                                                    
                                                                        
        return                                                          
        end 

