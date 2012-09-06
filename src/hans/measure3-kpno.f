c     measure3    local linear solution     21/12/2001
c     ------------------------------------------------
      implicit NONE

      integer i
      integer number_frames

      integer iah,iam,idh,idm
      integer icode
      integer narg
      integer len_image_name
      integer iargc
      external iargc


      real*8 cand_coord(3,2),cand_coord_trans(3,2)
      real*8 cand_x,cand_y
      real*8 cand_x_trans,cand_y_trans

      real*8 alpha_cand,delta_cand,rms
      real*8 xias,hds

      character*100 filename_plate_solution
      character*100 filename_stars_coord
      character*100 filename_cand_alpha_delta
      character*100 ffilename_stars_coord(3)
      character*100 ffilename_plate_solution(3)
      character*100 filename_scatter
      character*100 filename_error
      character*100 ffilename_usno_coord_mag(3)
      character*100 filename_usno_coord_mag
      character*80 line
      character*80 filename_moving_objects_old
      character*80 filename_moving_objects
      character*100 ffilename_info(3),filename_info

      character*1 sign,c1
      character*80 filename_image,arg
      character*20 image_name(3)


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

      filename_moving_objects = 
     *   filename_image(1:len_image_name)//'.cands.comb'
      filename_cand_alpha_delta = 
     *   filename_image(1:len_image_name)//'.cands.astrom'

      open(11,file=filename_moving_objects,status='old',err=100)
      open(12,file=filename_cand_alpha_delta,status='unknown')
      do i = 1,10
        read(11,'(a)') line
        write(12,'(a)')line
      end do
      rewind 11
      do i = 1,3
        read(11,2001) image_name(i)
      end do
 2001 format(2x,a20)
      do i = 1,7
        read(11,'(a)') line
      end do

      do i = 1,3
        line = image_name(i)
        ffilename_stars_coord(i) = line(1:len_image_name)
     *                             //'.bright.jmp'
        ffilename_plate_solution(i) = line(1:len_image_name)
     *                             //'.platesol'
        ffilename_usno_coord_mag(i) = line(1:len_image_name)
     *                             //'.usno'
        ffilename_info(i) =
     *             line(1:len_image_name)//'.info.astrom'
      end do

      line = image_name(1)
      filename_scatter =    line(1:len_image_name)
     *                             //'.starastrom.scatter'
      filename_error = line(1:len_image_name)
     *                             //'.cands.ERROR'


      call check_plate_solution(ffilename_plate_solution,
     *                  ffilename_stars_coord,
     *                  filename_scatter,
     *                  filename_error,
     *                   icode)
c       if(icode.ne.0) then
c         write(6,*) 'icode = ',icode
c         stop
c       end if

   90 continue
      icode = 0
      read(11,'(a)',end=100)c1
      do i = 1,3
        read(11,*) cand_coord(i,1),cand_coord(i,2),
     *             cand_coord_trans(i,1),cand_coord_trans(i,2)

      end do
      do i = 1,3
        cand_x = cand_coord(i,1)
        cand_y = cand_coord(i,2)
        cand_x_trans= cand_coord_trans(i,1)
        cand_y_trans= cand_coord_trans(i,2)

        filename_stars_coord = ffilename_stars_coord(i)
        filename_plate_solution = ffilename_plate_solution(i)
        filename_usno_coord_mag = ffilename_usno_coord_mag(i)
        filename_info = ffilename_info(i)

c     astrometry
c     ----------
        call find_alpha_delta_linear(cand_x,cand_y,
     *              alpha_cand,delta_cand,
     *              filename_usno_coord_mag,
     *              filename_plate_solution,
     *              filename_info,filename_error,
     *                  icode)
        if(i.eq.1) then
          line = ' '
          write(12,'(a)')line
        end if
        if(icode.ne.0) then
c         write(6,*) 'icode = ',icode
          alpha_cand = -1.d0
          delta_cand = 91.d0
          write(12,2012) cand_x,cand_y,cand_x_trans,cand_y_trans,
     *               alpha_cand,delta_cand
        else
          write(12,2012) cand_x,cand_y,cand_x_trans,cand_y_trans,
     *               alpha_cand,delta_cand
 2012     format(4f10.2,2f15.7)
c         write(6,*) 'cand_x,cand_y',cand_x,cand_y
c         write(6,2005) alpha_cand,delta_cand
c2005     format(2f15.7)

c         call conv_alpha_delta(alpha_cand,delta_cand,
c    *          iah,iam,xias,sign,idh,idm,hds)
c         write(6,2000) iah,iam,xias,sign,idh,idm,hds,cand_x,cand_y
c2000     format(2i3,f7.2,3x,a1,i2,i3,f5.1,2f10.1)

        end if
      end do
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
     *              filename_info,filename_error,
     *                  icode)

      implicit NONE

      integer max_usno,n_refstars
      parameter(max_usno=5000)
      parameter(n_refstars=15)

      integer i,j,nz,l,icode,k,ierr
      integer naxis1,naxis2
      integer n_axis1_meaning,n_axis2_meaning
      integer n_axis1_direction,n_axis2_direction
      integer indx_sort(max_usno)
      integer n_elim,n_stars,nn_max
      integer nna,nnn

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
      real*8 test(2*n_refstars)
      real*8 w(6),plc(6),rv(6),pplc(6,1)
      real*8 epsilon,epsilon_2
      real*8 kn
      real*8 tand,sind,cosd
      external tand,sind,cosd


      character*1 minus,axis1_meaning,axis2_meaning,sign,
     *             axis1_direction,axis2_direction

      character*100 filename_usno_coord_mag
      character*100 filename_plate_solution
      character*100 filename_info,filename_error



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
c     write(6,*) ccand_x,ccand_y
c     write(6,*) dist_pix_2_sort(1)
c    *       ,star_coord_sort(1,1),star_coord_sort(1,2)
c     write(6,*) dist_pix_2_sort(2)
c    *       ,star_coord_sort(2,1),star_coord_sort(2,2)
c     write(6,*) dist_pix_2_sort(3)
c    *       ,star_coord_sort(3,1),star_coord_sort(3,2)
c     x_orig = star_coord_sort(1,1)
c     y_orig = star_coord_sort(1,2)
c     alpha_proj_center_deg = usno_alpha_sort(1)
c     delta_proj_center = usno_delta_sort(1)

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

      n_stars = n_refstars


 5000 continue
      do i = 1,n_stars
        coord_rel_gnom(i,1) = 
     *       (star_coord_sort(i,1)-x_orig)/focal_length
        coord_rel_gnom(i,2) = 
     *       (star_coord_sort(i,2)-y_orig)/focal_length
      end do
      do i = 1,n_stars
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

      do i = 1,n_stars
        alpha_deg = usno_alpha_sort(i)
        delta_deg = usno_delta_sort(i)
        call equa_deg_to_xygnom(alpha_deg,delta_deg,
     *      alpha_proj_center_deg,delta_proj_center,
     *      x_gnom,y_gnom)
        usno_gnom(i,1) = x_gnom
        usno_gnom(i,2) = y_gnom
      end do

      do i=1,2*n_stars
        do j=1,6
          t(i,j) = 0.d0
        end do
      end do

      do i = 1,n_stars
        t(i,1) =1
        t(i,2) = coord_rel_gnom(i,1)
        t(i,3) = coord_rel_gnom(i,2)
        xx(i) = usno_gnom(i,1)
        xx(i+n_stars) = usno_gnom(i,2)
        do l=1,3
          t(i+n_stars,l+3) = t(i,l)
        end do
      end do
      do i=1,2*n_stars
        xx1(i) = xx(i)
        do j=1,6
          t1(i,j) = t(i,j)
        end do
      end do

      nn_max = n_refstars
      call minfitpack (2*nn_max,2*n_stars,6,t1,w,1,xx1,ierr,rv,
     *  pplc,6,kn)
      if(ierr.ne.0) then
c     write(nu_fail,*) 'ierr = ',ierr
c     write(nu_fail,*) 'NO ASTROMETRY POSSIBLE'
      icode = 42
      return
      end if

c     3-sigma test on individual reference stars
c     ------------------------------------------
      n_elim = 0
      epsilon_2 = 0.d0
      
      call dproduit (t,pplc,test,2*nn_max,6,1,2*n_stars,6,1)
      do i=1,2*n_stars
        epsilon_2 = epsilon_2 + ((xx(i)-test(i))**2)
      end do
      epsilon_2 = epsilon_2/dfloat(2*n_stars-6)
      epsilon = dsqrt(epsilon_2)
      if (epsilon.gt.(3.d0/180.d0/3600.d0*pie)) then
        nnn = 0
        open(14,file=filename_error,status='unknown')
   60   read(14,*,end=61)
        nnn = nnn+1
        goto 60
   61   rewind 14
        if(nnn.gt.0) then
          do nna = 1,nnn
            read(14,*)
          end do
        end if
        write(14,*) ' '
        write(14,*) filename_usno_coord_mag(1:20)
        write(14,*) 'cand_x,cand_y',ccand_x,ccand_y
        write(14,*) 'rms too large'
        write(14,*) 'NO ASTROMETRY POSSIBLE'
        write(14,*) ' '
        close(14)
        icode = 43
        return
      end if

      open(13,file='dummy_ref_stars',form='unformatted',
     *      status='unknown')
      do i = 1,n_stars
        if (dabs(xx(i)-test(i)).gt.(3.*epsilon).or.
     1  dabs(xx(i+n_stars)-test(i+n_stars)).gt.(3.*epsilon) ) then
          n_elim = n_elim+1
        else
          write(13) usno_alpha_sort(i),usno_delta_sort(i),
     *              star_coord_sort(i,1),star_coord_sort(i,2)
        end if
      end do
      if(n_elim.gt.0) then
        n_stars = n_stars-n_elim
        nnn = 0
        open(15,file=filename_info,status='unknown')
   70   read(15,*,end=71)
        nnn = nnn+1
        goto 70
   71   rewind 15
        if(nnn.gt.0) then
          do nna = 1,nnn
            read(15,*)
          end do
        end if
        write(15,*) ' '
        write(15,*) filename_usno_coord_mag(1:20)
        write(15,*) 'cand_x,cand_y',ccand_x,ccand_y
        write(15,*) 'number of eliminated stars',n_elim
        write(15,*) 'number of remaining stars',n_stars
        close(15)
        if(n_stars.lt.6) then
          icode = 44
          nnn = 0
          open(14,file=filename_error,status='unknown')
   62     read(14,*,end=63)
          nnn = nnn+1
          goto 62
   63     rewind 14
          if(nnn.gt.0) then
            do nna = 1,nnn
              read(14,*)
            end do
          end if
          write(14,*) ' '
          write(14,*) filename_usno_coord_mag(1:20)
          write(14,*) 'cand_x,cand_y',ccand_x,ccand_y
          write(14,*) 'not enough reference stars'
          write(14,*) ' '
          close(14)
          return
        end if
        rewind 13
        do i = 1,n_stars
          read(13) usno_alpha_sort(i),usno_delta_sort(i),
     *              star_coord_sort(i,1),star_coord_sort(i,2)
        end do
      end if
      close(13)
      if(n_elim.gt.0) goto 5000
c       write(6,*) n_elim,n_stars

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

c     ------------------------------------------
c1111
      subroutine check_plate_solution(ffilename_plate_solution,
     *                  ffilename_stars_coord,
     *                  filename_scatter,
     *                  filename_error,
     *                   icode)

      implicit NONE
      integer max_number_stars
      parameter(max_number_stars=2000)

      integer i,j,nz,l,icode,iii
      integer naxis1,naxis2
      integer n_axis1_meaning,n_axis2_meaning
      integer n_axis1_direction,n_axis2_direction

      real*8 focal_length,x_off,y_off
      real*8 alpha_proj_center_deg,delta_proj_center
      real*8 alpha_dev_1_2,delta_dev_1_2
      real*8 alpha_dev_1_3,delta_dev_1_3
      real*8 offset_a_1_2,offset_a_1_3
      real*8 offset_d_1_2,offset_d_1_3
      real*8 alpha(max_number_stars,3)
      real*8 delta(max_number_stars,3)
      real*8 pie,pierez
      real*8 cand_x,cand_y
      real*8 ccand_x,ccand_y
      real*8 aa,bb,da,dd,rms
      real*8 ttan
      real*8 tol_offset_arc_sec
      real*8 eta,zeta,alpha_cand,delta_cand
      real*8 alphaz_deg,deltaz_deg
      real*8 x_ast,y_ast
      real*8 plc(30)
      real*8 tand,sind,cosd
      external tand,sind,cosd


      character*1 minus,axis1_meaning,axis2_meaning,sign,
     *             axis1_direction,axis2_direction

      character*100 filename_plate_solution,filename_stars_coord
      character*100 ffilename_plate_solution(3)
      character*100 ffilename_stars_coord(3)
      character*100 filename_scatter
      character*100 filename_error


      icode = 0
      pie = 3.141592653589793D0
      pierez = pie/180.d0
      minus = '-'

      tol_offset_arc_sec = 1.0d0


c     determine alpha,delta of stars
c     -----------------------------------
      do iii = 1,3
        filename_plate_solution = ffilename_plate_solution(iii)
        filename_stars_coord = ffilename_stars_coord(iii)

        open(4,err=10,file=filename_plate_solution,status='old')

        read(4,4000) n_axis1_meaning,n_axis1_direction,
     *              n_axis2_direction
 4000   format(3i3)
        read(4,4001) focal_length,x_off,y_off
 4001   format(3d25.15)
        read(4,4002) alpha_proj_center_deg,delta_proj_center
 4002   format(2d25.15)
        read(4,4003) (plc(i),i=1,5)
        read(4,4003) (plc(i),i=6,10)
        read(4,4003) (plc(i),i=11,15)
        read(4,4003) (plc(i),i=16,20)
      read(4,4003) (plc(i),i=21,25)
      read(4,4003) (plc(i),i=26,30)
 4003   format(5d25.15)
        close(4)
        axis1_direction = ' '
        if(n_axis1_direction.lt.0) axis1_direction = minus

        axis2_direction = ' '
        if(n_axis2_direction.lt.0) axis2_direction = minus


        nz = 0      
        open(1,err=11,file=filename_stars_coord,status='old')
        read(1,*)
        read(1,*)
    5   read(1,*,end=20)ccand_x,ccand_y
        nz = nz+1 
        cand_x = ccand_x-x_off
        cand_y = ccand_y-y_off
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


        x_ast = cand_x
        y_ast = cand_y
      zeta =  plc(1)+plc(2)*x_ast+plc(3)*y_ast
     *       +plc(4)*(x_ast**2)+plc(5)*(y_ast**2)+plc(6)*x_ast*y_ast
     *       +plc(7)*(x_ast**3)+plc(8)*(y_ast**3)
     *       +plc(9)*(x_ast**2)*y_ast+plc(10)*x_ast*(y_ast**2)
     *       +plc(11)*(x_ast**4)+plc(12)*(y_ast**4)
     *       +plc(13)*x_ast*(y_ast**3)+plc(14)*(x_ast**2)*(y_ast**2)
     *       +plc(15)*(x_ast**3)*y_ast

      eta  =  plc(16)+plc(17)*x_ast+plc(18)*y_ast
     *       +plc(19)*(x_ast**2)+plc(20)*(y_ast**2)+plc(21)*x_ast*y_ast
     *       +plc(22)*(x_ast**3)+plc(23)*(y_ast**3)
     *       +plc(24)*(x_ast**2)*y_ast+plc(25)*x_ast*(y_ast**2)
     *       +plc(26)*(x_ast**4)+plc(27)*(y_ast**4)
     *       +plc(28)*x_ast*(y_ast**3)+plc(29)*(x_ast**2)*(y_ast**2)
     *       +plc(30)*(x_ast**3)*y_ast


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
        alpha(nz,iii) = alpha_cand
        delta(nz,iii) = delta_cand
        if(nz.eq.max_number_stars) then
          write(6,*) 'WARNING: increase max_number_stars'
          goto 20
        end if
        goto 5
   20   close(1)
        if(nz.eq.0) then
          write(6,*) 'ERROR: number of reference stars is zero'
          icode = 5
          return
        end if
      end do

      alpha_dev_1_2 = 0.d0
      delta_dev_1_2 = 0.d0
      offset_a_1_2 = 0.d0
      offset_d_1_2 = 0.d0
      do i = 1,nz
        offset_a_1_2 = offset_a_1_2+
     *           (alpha(i,2)-alpha(i,1))*cosd(delta(i,1))
        offset_d_1_2 = offset_d_1_2+delta(i,2)-delta(i,1)
        alpha_dev_1_2 = alpha_dev_1_2 + 
     *           (alpha(i,1)-alpha(i,2))**2*cosd(delta(i,1))**2
        delta_dev_1_2 = delta_dev_1_2 + (delta(i,1)-delta(i,2))**2
      end do
      alpha_dev_1_2 = dsqrt(alpha_dev_1_2/dble(nz-1))*3600.
      delta_dev_1_2 = dsqrt(delta_dev_1_2/dble(nz-1))*3600.

      alpha_dev_1_3 = 0.d0
      delta_dev_1_3 = 0.d0
      offset_a_1_3 = 0.d0
      offset_d_1_3 = 0.d0
      do i = 1,nz
        offset_a_1_3 = offset_a_1_3+
     *                 (alpha(i,3)-alpha(i,1))*cosd(delta(i,1))
        offset_d_1_3 = offset_d_1_3+delta(i,3)-delta(i,1)
        alpha_dev_1_3 = alpha_dev_1_3 + 
     *          (alpha(i,1)-alpha(i,3))**2*cosd(delta(i,1))**2
        delta_dev_1_3 = delta_dev_1_3 + (delta(i,1)-delta(i,3))**2
c       write(6,*) i,
c    *    dsqrt((alpha(i,1)-alpha(i,3))**2*cosd(delta(i,1))**2)*3600.d0,
c    *               dsqrt((delta(i,1)-delta(i,3))**2)*3600.d0
      end do
      offset_a_1_2 = offset_a_1_2/dble(nz)*3600.
      offset_a_1_3 = offset_a_1_3/dble(nz)*3600.
      offset_d_1_2 = offset_d_1_2/dble(nz)*3600.
      offset_d_1_3 = offset_d_1_3/dble(nz)*3600.

      alpha_dev_1_3 = dsqrt(alpha_dev_1_3/dble(nz-1))*3600.
      delta_dev_1_3 = dsqrt(delta_dev_1_3/dble(nz-1))*3600.
c     write(6,*) alpha_dev_1_2,delta_dev_1_2
c     write(6,*) delta_dev_1_3,delta_dev_1_3

      if(alpha_dev_1_3 .gt. tol_offset_arc_sec    .or.
     *   delta_dev_1_3 .gt. tol_offset_arc_sec    .or.
     *   alpha_dev_1_2 .gt. tol_offset_arc_sec    .or.
     *   delta_dev_1_2 .gt. tol_offset_arc_sec) then
      open(9,file=filename_error,status='unknown')
      write(9,*) 'PLATE SOLUTION CHECK DID NOT WORK'
      write(9,*) 'number of stars used for plate solution check',nz
      write(9,*) 'offset between image 1 and 2',
     *      offset_a_1_2,offset_d_1_2
      write(9,*) 'offset between image 1 and 3',
     *       offset_a_1_3,offset_d_1_3
      write(9,*) 'alpha_dev_1_2,delta_dev_1_2',
     *            alpha_dev_1_2,delta_dev_1_2
      write(9,*) 'alpha_dev_1_3,delta_dev_1_3',
     *            alpha_dev_1_3,delta_dev_1_3
      close(9)
      icode = 4
      return
      end if
      open(8,file=filename_scatter,status='unknown')
      write(8,*) alpha_dev_1_2,delta_dev_1_2
      write(8,*) alpha_dev_1_3,delta_dev_1_3
      close(8)

      return
   10 icode = 2
      return
   11 icode = 3
      return
      end 


      subroutine conv_alpha_delta(alpha_deg,delta,
     *      iah,iam,xias,sign,idh,idm,hds)

      implicit real*8(a-h,o-z)

      character*1 sign

      alpha = alpha_deg/360.*24.
      iah = alpha
      xiam = (alpha-iah)*60.
      iam = xiam
      xias = (xiam-iam)*60.
      h = delta
      sign = ' '
      if(h.lt.0.d0) then
        h = -h
        sign = '-'
      end if
      idh = h
      hdm = (h-idh)*60.
      idm = hdm
      hds = (hdm-idm)*60.

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

