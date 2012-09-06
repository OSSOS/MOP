c     measure1
c     release 12.05.2003
c
      implicit NONE
      integer maxframes,mmaxframes
      parameter(maxframes=5)

      integer icode,i
      integer n_skip_rec_file_usno_identif
      integer iargc,narg
      external iargc


      character*80
     *           ffilename_usno_identified(maxframes),
     *           filename_warn,
     *           filename_failed,
     *           filename_moving_objects,
     *           filename_cand_alpha_delta,
     *           filename_dummy_ref_stars,
     *           read_arg,
     *           filename_ok
      character*80 arg

      logical pipeline

      mmaxframes = maxframes

      narg = iargc()
      i = 1
      call getarg (i, arg)

      read_arg = arg

      call provide_filenames_pipeline_m1(
     *           ffilename_usno_identified,
     *           filename_warn,
     *           filename_ok,
     *           filename_failed,
     *           filename_moving_objects,
     *           filename_cand_alpha_delta,
     *           filename_dummy_ref_stars,
     *           read_arg,
     *           n_skip_rec_file_usno_identif,
     *           mmaxframes,
     *           pipeline,
     *           icode)

      n_skip_rec_file_usno_identif = 5


      if(icode.eq.0) call do_the_astrometry_m1(
     *           ffilename_usno_identified,
     *           filename_warn,
     *           filename_ok,
     *           filename_failed,
     *           filename_moving_objects,
     *           filename_cand_alpha_delta,
     *           filename_dummy_ref_stars,
     *           n_skip_rec_file_usno_identif,
     *           mmaxframes,
     *           pipeline,
     *           icode)

      if(icode.ne.0 ) then
        write(6,*) 'failure, sorry '
      end if

      stop
      end
c**********************************************************
      subroutine provide_filenames_pipeline_m1(
     *           ffilename_usno_identified,
     *           filename_warn,
     *           filename_ok,
     *           filename_failed,
     *           filename_moving_objects,
     *           filename_cand_alpha_delta,
     *           filename_dummy_ref_stars,
     *           read_arg,
     *           n_skip_rec_file_usno_identif,
     *           maxframes,
     *           pipeline,
     *           icode)

      implicit NONE
     
      integer i,narg,len_image_name
      integer 
     *           n_skip_rec_file_usno_identif,
     *           maxframes,
     *           icode

      character*80 
     *           ffilename_usno_identified(maxframes),
     *           filename_warn,
     *           filename_ok,
     *           filename_moving_objects,
     *           filename_cand_alpha_delta,
     *           line,
     *           filename_dummy_ref_stars,
     *           filename_failed

      character*80 filename_image,read_arg
      character*20 image_name(3)
      character*80 filename_test

      logical pipeline

      icode = 0

      filename_image = read_arg

      i = 1
      do while(filename_image(i:i).ne.' ')
        i = i+1
      end do
      i = i-1
      len_image_name = i

      n_skip_rec_file_usno_identif = 5

      filename_moving_objects =
     *   filename_image(1:len_image_name)//'.coo'
      filename_cand_alpha_delta =
     *   filename_image(1:len_image_name)//'.measure1.mpc'
      filename_failed =
     *   filename_image(1:len_image_name)//'.measure1.FAILED'
      filename_ok =
     *   filename_image(1:len_image_name)//'.measure1.OK'
      filename_warn =
     *   filename_image(1:len_image_name)//'.measure1.WARNING'
      filename_dummy_ref_stars=
     *   filename_image(1:len_image_name)//'.measure1.dummy_ref'
      ffilename_usno_identified(1) = filename_image(1:len_image_name)
     *                             //'.mkpltsol.usno'
      pipeline = .true.
      return
      end
c******************************************************************
      subroutine do_the_astrometry_m1(
     *           ffilename_usno_identified,
     *           filename_warn,
     *           filename_ok,
     *           filename_failed,
     *           filename_moving_objects,
     *           filename_cand_alpha_delta,
     *           filename_dummy_ref_stars,
     *           n_skip_rec_file_usno_identif,
     *           maxframes,
     *           pipeline,
     *           icode)

      implicit NONE

      integer i,j,nz
      integer imode
      integer unit12
      integer n_refstars
      integer n_skip_rec_file_usno_identif

      integer icode,icode_lin,icode_lin_pair,icode_lin_impair
      integer maxframes
      integer unitx
      integer jahr,monat,itag,ist,imin,monat1,monat2,iday1
      integer ih1,ih2,iam1,iam2,ias1
      integer id1,id2,idm1,idm2,ids1

      real*8 cand_coord(maxframes,2)
      real*8 cand_coord_trans(maxframes,2)
      real*8 cand_x,cand_y
      real*8 cand_x_trans,cand_y_trans
      real*8 alpha_s,delta_s

      real*8 alpha_cand,delta_cand,rms_arcsec
      real*8 alpha_cand_lin,delta_cand_lin
      real*8 alpha_cand_lin_pair,delta_cand_lin_pair
      real*8 alpha_cand_lin_impair,delta_cand_lin_impair
      real*8 xias,hds
      real*8 diff_alpha_lin_lin_pair,diff_alpha_lin_lin_impair
      real*8 diff_delta_lin_lin_pair,diff_delta_lin_lin_impair
      real*8 jul_date,sec,hour,day
      real*8 cosd,sind
      external cosd,sind

      character*80 filename_stars_coord
      character*80 filename_cand_alpha_delta
      character*80 filename_failed
      character*80 filename_ok
      character*80 filename_warn
      character*80 filename_dummy_ref_stars
      character*80 filename_usno_identified
      character*80 line
      character*80 filename_moving_objects
      character*80 ffilename_usno_identified(maxframes)

      character*1 sign,c1,sign_delta
      character*2 aa

      logical pipeline

      icode = 0

      rms_arcsec = 0.5d0

      open(11,file=filename_moving_objects,status='old',err=110)

      unit12 =12
      call open_for_appending_file(unit12,filename_cand_alpha_delta)
      

      icode = 0
      n_refstars = 10

      icode_lin = 0
      icode_lin_pair = 0
      icode_lin_impair = 0

   90 continue
      read(11,*,end=100) cand_x,cand_y

      filename_usno_identified = ffilename_usno_identified(1)

c     astrometry
c     ----------

c     use linear solution with n_refstars closest stars
c     imode = 1
c     ------------------------------------------
        imode = 1
        alpha_cand_lin = 100.d0
        delta_cand_lin = 100.d0

        call find_alpha_delta_linear
     *              (cand_x,cand_y,
     *              alpha_cand_lin,delta_cand_lin,
     *              jul_date,
     *              filename_usno_identified,
     *              filename_warn,
     *              filename_dummy_ref_stars,
     *              n_refstars,
     *              imode,
     *              n_skip_rec_file_usno_identif,
     *                  pipeline,
     *              icode_lin)
        if(icode_lin.ne.0) then
          open(9,file=filename_failed,status='unknown')
c         if(.not.pipeline) then
              write(6,*) 
     *       'linear solution using closest n_refstars failed, ',
     *       'icode_lin = ',icode_lin
          write(6,9191) cand_x,cand_y
c         else
              write(9,*)
     *       'linear solution using closest n_refstars failed, ',
     *       'icode_lin = ',icode_lin
          write(9,9191) cand_x,cand_y
c         end if
 9191 format(2f10.1)
          icode = 12
          close(9)
          goto 50
        end if


c     use linear solution with 2*n_refstars closest stars using
c     imode = 2
c     first, third,...... closest star
c     ------------------------------------------
        imode = 2

        alpha_cand_lin_impair = 200.d0
        delta_cand_lin_impair = 200.d0

        call find_alpha_delta_linear
     *              (cand_x,cand_y,
     *              alpha_cand_lin_impair,delta_cand_lin_impair,
     *              jul_date,
     *              filename_usno_identified,
     *              filename_warn,
     *              filename_dummy_ref_stars,
     *              n_refstars,
     *              imode,
     *              n_skip_rec_file_usno_identif,
     *                  pipeline,
     *              icode_lin_impair)
        if(icode_lin_impair.ne.0) then
          unitx = 17
          call open_for_appending_file(unitx,filename_warn)
c         if(.not.pipeline) then
          write(6,*) 'see the WARNING file'
          write(6,'(a)') filename_warn
c         end if
          write(unitx,*) 
     *    'linear solution using closest impair n_refstars failed, ',
     *       'icode_lin_impair = ',icode_lin_impair
          write(unitx,9191) cand_x,cand_y

          close(unitx)
        end if


c     use linear solution with 2*n_refstars closest stars using
c     imode = 3
c     second,fourth,...... closest star
c     ------------------------------------------
        imode = 3
        alpha_cand_lin_pair = 300.d0
        delta_cand_lin_pair = 300.d0

        call find_alpha_delta_linear
     *              (cand_x,cand_y,
     *              alpha_cand_lin_pair,delta_cand_lin_pair,
     *              jul_date,
     *              filename_usno_identified,
     *              filename_warn,
     *              filename_dummy_ref_stars,
     *              n_refstars,
     *              imode,
     *              n_skip_rec_file_usno_identif,
     *                  pipeline,
     *              icode_lin_pair)

        if(icode_lin_pair.ne.0) then
          unitx = 17
          call open_for_appending_file(unitx,filename_warn)
c         if(.not.pipeline) then
          write(6,*) 'see the WARNING file'
          write(6,'(a)') filename_warn
c         end if
          write(unitx,*)
     *    'linear solution using closest pair n_refstars failed',
     *       ' icode_lin_pair = ',icode_lin_pair
          write(unitx,9191) cand_x,cand_y
          close(unitx)
        end if

c     compare the solutions for alpha,delta
c     -------------------------------------
        diff_alpha_lin_lin_pair = 
     *      (alpha_cand_lin-alpha_cand_lin_pair) 
     *       *cosd(delta_cand_lin)*3600.d0
        diff_alpha_lin_lin_impair = 
     *      (alpha_cand_lin-alpha_cand_lin_impair) 
     *       *cosd(delta_cand_lin)*3600.d0
        diff_delta_lin_lin_pair = 
     *      (delta_cand_lin-delta_cand_lin_pair)*3600.d0
        diff_delta_lin_lin_impair = 
     *      (delta_cand_lin-delta_cand_lin_impair)*3600.d0

        if(dabs(diff_alpha_lin_lin_pair).gt.999.d0)
     *          diff_alpha_lin_lin_pair = 999.d0
        if(dabs(diff_alpha_lin_lin_impair).gt.999.d0)
     *          diff_alpha_lin_lin_impair = 999.d0
        if(dabs(diff_delta_lin_lin_pair).gt.999.d0)
     *          diff_delta_lin_lin_pair = 999.d0
        if(dabs(diff_delta_lin_lin_impair).gt.999.d0)
     *          diff_delta_lin_lin_impair = 999.d0
        if(dabs(diff_alpha_lin_lin_pair).gt.rms_arcsec .or.
     *    dabs(diff_alpha_lin_lin_impair).gt.rms_arcsec .or.
     *    dabs(diff_delta_lin_lin_pair).gt.rms_arcsec .or.
     *    dabs(diff_delta_lin_lin_impair).gt.rms_arcsec) then
          unitx = 17
          call open_for_appending_file(unitx,filename_warn)

c         if(.not.pipeline) then
          write(6,*) 'see the WARNING file'
          write(6,'(a)') filename_warn
c         end if
          aa = '##'
          write(unitx,'(a)') '## '
          write(unitx,'(a)')
     * '## WARNING: RA or DEC discrepancy among linear solutions' 
          write(unitx,'(a)')'##      X        Y' 
          write(unitx,2013) aa,cand_x,cand_y
 2013 format(a2,2f10.1)
          write(unitx,'(a)') 
     *    '## RA1 - RA2(")  RA1 - RA3(")  DEC1-DEC2(")  DEC1-DEC3(")'
          write(unitx,2015)aa,
     *               diff_alpha_lin_lin_pair,
     *               diff_alpha_lin_lin_impair,
     *               diff_delta_lin_lin_pair,
     *               diff_delta_lin_lin_impair
 2015 format(a2,f6.1,10x,f6.1,8x,f6.1,10x,f6.1)
          close(unitx)

        end if
        
   50   continue
        if(icode.ne.0) goto 90

      call julciv(jul_date,jahr,monat,itag,ist,imin,sec)
      hour = SEC/3600.d0+dble(imin)/60.d0+dble(IST)
      day = dble(ITAG)+hour/24.d0
      
      monat1 = monat/10
      monat2 = monat-10*monat1
      iday1 = day/10
      day = day-10*iday1
      call tr_alpha_delta_mpc(alpha_cand_lin,delta_cand_lin,
     *    ih1,ih2,iam1,iam2,ias1,alpha_s,
     *    id1,id2,idm1,idm2,ids1,delta_s,sign_delta)

      write(6,4044) jahr,monat1,monat2,iday1,day,
     *      ih1,ih2,iam1,iam2,ias1,alpha_s,
     *       sign_delta,id1,id2,idm1,idm2,ids1,delta_s,
     *        cand_x,cand_y
      write(12,4044) jahr,monat1,monat2,iday1,day,
     *      ih1,ih2,iam1,iam2,ias1,alpha_s,
     *       sign_delta,id1,id2,idm1,idm2,ids1,delta_s,
     *        cand_x,cand_y
 4044 format(T16,i4,1x,2i1,1x,i1,f7.5,1x,2i1,1x,2i1,1x,i1,f4.2,1x,
     *        a1,2i1,1x,2i1,1x,i1,f3.1,4x,2f8.1)

      goto 90

  100 continue
      close(12)
      if(icode+icode_lin.eq.0) then
        open(14,file=filename_ok,status='unknown')
        write(14,*) 'OK'
        close(14)
      end if
      return
  110 close(11)
      open(9,file=filename_failed,status='unknown')
        write(9,*) 'did not find the file for moving objects ',
     *             filename_moving_objects
         write(6,*) 'did not find the file for moving objects ',
     *             filename_moving_objects
      close(9)
      return

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
      subroutine find_alpha_delta_linear(ccand_x,ccand_y,
     *              alpha_cand,delta_cand,
     *              jul_date,
     *              filename_usno_identified,
     *              filename_warn,
     *              filename_dummy_ref_stars,
     *              n_refstars,
     *              imode,
     *              n_skip_rec_file_usno_identif,
     *                  pipeline,
     *              icode)

      implicit NONE

      integer max_usno,n_refstars,n_stars
      parameter(max_usno=5000)

      integer i,j,nz,l,icode,k,ierr
      integer imode
      integer naxis1,naxis2
      integer n_axis1_meaning,n_axis2_meaning
      integer n_axis1_direction,n_axis2_direction
      integer indx_sort(max_usno)
      integer n_elim
      integer nn_max
      integer axis1_orientation,axis2_orientation
      integer n_skip_rec_file_usno_identif
      integer unitx

      real*8 star_coord(max_usno,2),star_coord_sort(max_usno,2)
      real*8 usno_alpha(max_usno),usno_alpha_sort(max_usno)
      real*8 usno_delta(max_usno),usno_delta_sort(max_usno)
      real*8 dist_pix_2(max_usno),dist_pix_2_sort(max_usno)
      real*8 star_coord_trans(max_usno,2)
      real*8 star_coord_trans_sort(max_usno,2)
      real*8 alpha_proj_center_deg,delta_proj_center
      real*8 pie,pierez
      real*8 cand_x,cand_y
      real*8 ccand_x,ccand_y
      real*8 x,y
      real*8 aa,bb,da,dd
      real*8 ttan
      real*8 eta,zeta,alpha_cand,delta_cand
      real*8 alphaz_deg,deltaz_deg
      real*8 x_ast,y_ast,x_orig,y_orig
      real*8 x_gnom,y_gnom,alpha_deg,delta_deg
      real*8 usno_gnom(n_refstars,2)
      real*8 coord_rel_gnom(n_refstars,2)
      real*8 test(2*n_refstars)
      real*8 xx(2*n_refstars),xx1(2*n_refstars),
     *       t(2*n_refstars,6),t1(2*n_refstars,6)
      real*8 w(6),plc(6),rv(6),pplc(6,1)
      real*8 kn
      real*8 epsilon,epsilon_2
      real*8 add_to_fr_stars_trans_x,add_to_fr_stars_trans_y
      real*8 add_to_fr_center_al_deg
      real*8 add_to_fr_center_del_deg
      real*8 fr_center_al_deg,fr_center_del_deg
      real*8 x_center,y_center,pix_scale,focal_length
      real*8 ua,ud,cx,cy,tx,ty
      real*8 angle,fr_minus_opt_ax_x,fr_minus_opt_ax_y
      real*8 fr_x,fr_y
      real*8 jul_date_modified,jul_date

      real*8 tand,sind,cosd
      external tand,sind,cosd

      logical take_it
      logical pipeline

      character*1 minus,axis1_meaning,axis2_meaning,sign,
     *             axis1_direction,axis2_direction

      character*80 filename_usno_identified
      character*80 filename_warn
      character*80 filename_dummy_ref_stars


      icode = 0
      pie = 3.141592653589793D0
      pierez = pie/180.d0
      minus = '-'


c     determine alpha,delta of candidates
c     -----------------------------------
      open(4,err=10,file=filename_usno_identified,status='old')

      read(4,4000) naxis1,naxis2,
     *       axis1_orientation,axis2_orientation
      read(4,4001) x_center,y_center,pix_scale
      read(4,4001) focal_length,
     *      add_to_fr_stars_trans_x,add_to_fr_stars_trans_y
 4000 format(4i6)
 4001 format(3d25.15)
      read(4,4006) fr_center_al_deg,fr_center_del_deg,
     *             jul_date_modified
      read(4,4004) add_to_fr_center_al_deg,
     *              add_to_fr_center_del_deg
      read(4,4001) angle,fr_minus_opt_ax_x,fr_minus_opt_ax_y
 4004 format(2d25.15)
 4006 format(3d25.15)
      jul_date = jul_date_modified+2400000.5d0

      close(4)

c     find closest reference stars
c     ----------------------------
      open(4,file=filename_usno_identified,status='old')
      do i = 1,n_skip_rec_file_usno_identif
        read(4,*)
      end do
      i = 0
   20 i = i+1
      if(i.gt.max_usno) then
          unitx = 17
          call open_for_appending_file(unitx,filename_warn)
c         if(.not.pipeline) then
          write(6,*) 'see the WARNING file'
          write(6,'(a)') filename_warn
c         end if
        write(unitx,*) 'WARNING: ',
     * 'increase max_usno in subroutine find_alpha_delta_linear'
        close(unitx)
        goto 30
      end if
      read(4,*,end=30) star_coord(i,1),star_coord(i,2),
     *                 star_coord_trans(i,1),star_coord_trans(i,2),
     *                 usno_alpha(i),usno_delta(i)
      dist_pix_2(i) = (star_coord(i,1)-ccand_x)**2+
     *                (star_coord(i,2)-ccand_y)**2
      dist_pix_2(i) = dist_pix_2(i)
      goto 20
   30 nz = i-1
      close(4)
      call sortreal8(nz,dist_pix_2,indx_sort)
      open(13,file=filename_dummy_ref_stars,form='unformatted',
     *      status='unknown')
      do k = 1,nz
        i = indx_sort(k)
        star_coord_sort(k,1) = star_coord(i,1)
        star_coord_sort(k,2) = star_coord(i,2)
        star_coord_trans_sort(k,1) = star_coord_trans(i,1)
        star_coord_trans_sort(k,2) = star_coord_trans(i,2)
        usno_alpha_sort(k) = usno_alpha(i)
        usno_delta_sort(k) = usno_delta(i)
        dist_pix_2_sort(k) = dsqrt(dist_pix_2(i))
      end do
      do k = 1,nz
        write(13) usno_alpha_sort(k),usno_delta_sort(k),
     *     star_coord_sort(k,1),star_coord_sort(k,2),
     *     star_coord_trans_sort(k,1),star_coord_trans_sort(k,2)

      end do

      rewind 13

      i = 0
      do k = 1,nz
        take_it = .false.
        read(13) ua,ud,cx,cy,tx,ty
        if(imode.eq.1) take_it = .true.
        if(imode.eq.2 .and. mod(k+1,2).eq.0) take_it = .true.
        if(imode.eq.3 .and. mod(k,2).eq.0) take_it = .true.
        if(take_it) then
          i = i+1
          usno_alpha_sort(i) = ua
          usno_delta_sort(i) = ud
          star_coord_sort(i,1) = cx
          star_coord_sort(i,2) = cy
          star_coord_trans_sort(i,1) = tx
          star_coord_trans_sort(i,2) = ty
          if(i.eq.n_refstars) goto 40
        end if
      end do

   40 continue
      close(13)
        

c     transform coordinates in  centered system
c     -----------------------------------------
      cand_x = ccand_x
      cand_y = ccand_y
      call transform_to_center_system(cand_x,cand_y,
     *       pix_scale,naxis1,naxis2,
     *       axis1_orientation,axis2_orientation,
     *       x_center,y_center,
     *      icode)

      fr_x = cand_x+fr_minus_opt_ax_x
      fr_y = cand_y+fr_minus_opt_ax_y
      cand_x = fr_x*cosd(angle) + fr_y*sind(angle)
      cand_y = -fr_x*sind(angle) + fr_y*cosd(angle)

      cand_x = cand_x-fr_minus_opt_ax_x
      cand_y = cand_y-fr_minus_opt_ax_y

      cand_x = cand_x+add_to_fr_stars_trans_x
      cand_y = cand_y+add_to_fr_stars_trans_y

      cand_x = cand_x/focal_length
      cand_y = cand_y/focal_length

      n_stars = n_refstars

 5000 continue
      do i = 1,n_stars
        x = star_coord_trans_sort(i,1)
        y = star_coord_trans_sort(i,2)
c       x = star_coord_sort(i,1)
c       y = star_coord_sort(i,2)
c       call transform_to_center_system(x,y,
c    *       pix_scale,naxis1,naxis2,
c    *       axis1_orientation,axis2_orientation,
c    *       x_center,y_center,
c    *      icode)
c      x = x+add_to_fr_stars_trans_x
c      y = y+add_to_fr_stars_trans_y
        coord_rel_gnom(i,1) = x/focal_length
        coord_rel_gnom(i,2) = y/focal_length
      end do

      alpha_proj_center_deg = fr_center_al_deg
      delta_proj_center = fr_center_del_deg

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

      if (epsilon.gt.(3.d0/180.d0/3600.d0*pie))
     *    epsilon = 1.d0/180.d0/3600.d0*pie
      open(13,file=filename_dummy_ref_stars,form='unformatted',
     *      status='unknown')
      do i = 1,n_stars
        if (dabs(xx(i)-test(i)).gt.(3.*epsilon).or.
     1  dabs(xx(i+n_stars)-test(i+n_stars)).gt.(3.*epsilon) ) then
          n_elim = n_elim+1
        else
          write(13) usno_alpha_sort(i),usno_delta_sort(i),
     *              star_coord_sort(i,1),star_coord_sort(i,2),
     *      star_coord_trans_sort(i,1),star_coord_trans_sort(i,2)
        end if
      end do
      if(n_elim.gt.0) then
        n_stars = n_stars-n_elim
        if(n_stars.lt.6) then
          icode = 44
          return
        end if
        rewind 13
        do i = 1,n_stars
          read(13) usno_alpha_sort(i),usno_delta_sort(i),
     *      star_coord_sort(i,1),star_coord_sort(i,2),
     *      star_coord_trans_sort(i,1),star_coord_trans_sort(i,2)
        end do
      end if
      close(13)
      if(n_elim.gt.0) goto 5000

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
      return
   10 icode = 1
      return
      end


c********************************************************
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

******************************************************
      subroutine transform_to_center_system(x,y,
     *       pix_scale,naxis1,naxis2,
     *       axis1_orientation,axis2_orientation,
     *       x_center,y_center,
     *      icode)
      implicit NONE

      integer axis1_orientation,axis2_orientation
      integer icode,naxis1,naxis2

      real*8 x,y,x_center,y_center,pix_scale,x_frame,y_frame

      x_frame = x
      y_frame = y

      x_center = x_frame - (dble(naxis1)/2.d0)
      y_center = y_frame - (dble(naxis2)/2.d0)
      icode = 0

      if(axis1_orientation.eq. 1 .and.
     *   axis2_orientation.eq. 2) then
        x = x_center
        y = y_center
        return
      end if

      if(axis1_orientation.eq. 1 .and.
     *   axis2_orientation.eq.-2) then
        x =  x_center
        y = -y_center
        return
      end if

      if(axis1_orientation.eq.-1 .and.
     *   axis2_orientation.eq. 2) then
        x = -x_center
        y =  y_center
        return
      end if

      if(axis1_orientation.eq.-1 .and.
     *   axis2_orientation.eq.-2) then
        x = -x_center
        y = -y_center
        return
      end if
      if(axis1_orientation.eq. 2 .and.
     *   axis2_orientation.eq. 1) then
        x = y_center
        y = x_center
        return
      end if

      if(axis1_orientation.eq. 2 .and.
     *   axis2_orientation.eq.-1) then
        x = y_center
        y =-x_center
        return
      end if


      if(axis1_orientation.eq.-2 .and.
     *   axis2_orientation.eq. 1) then
        x = -y_center
        y =  x_center
        return
      end if

      if(axis1_orientation.eq.-2 .and.
     *   axis2_orientation.eq.-1) then
        x = -y_center
        y = -x_center
        return
      end if

      icode = 1
      return
      end
c**************************************************8
      subroutine open_for_appending_file(unit,filename)
      implicit NONE

      integer unit,nz,i

      character*80 filename

      open(unit,file=filename,status='unknown')

      nz = 0
    5 read(unit,*,end=6)
      nz = nz+1
      goto 5
    6 close(unit)
      open(unit,file=filename,status='unknown')
      if(nz.eq.0) return
      do i = 1,nz
        read(unit,*)
      end do
      return
      end
c*************************************************************
      subroutine chip_char(maximum_chip_number,chip_number_char)
      implicit NONE
      integer maximum_chip_number
      character*2 chip_number_char(0:maximum_chip_number)

      chip_number_char(0) = '00'
      chip_number_char(1) = '01'
      chip_number_char(2) = '02'
      chip_number_char(3) = '03'
      chip_number_char(4) = '04'
      chip_number_char(5) = '05'
      chip_number_char(6) = '06'
      chip_number_char(7) = '07'
      chip_number_char(8) = '08'
      chip_number_char(9) = '09'
      chip_number_char(10) = '10'
      chip_number_char(11) = '11'
      chip_number_char(12) = '12'
      chip_number_char(13) = '13'
      chip_number_char(14) = '14'
      chip_number_char(15) = '15'
      chip_number_char(16) = '16'
      chip_number_char(17) = '17'
      chip_number_char(18) = '18'
      chip_number_char(19) = '19'
      chip_number_char(20) = '20'
      chip_number_char(21) = '21'
      chip_number_char(22) = '22'
      chip_number_char(23) = '23'
      chip_number_char(24) = '24'
      chip_number_char(25) = '25'
      chip_number_char(26) = '26'
      chip_number_char(27) = '27'
      chip_number_char(28) = '28'
      chip_number_char(29) = '29'
      chip_number_char(30) = '30'
      chip_number_char(31) = '31'
      chip_number_char(32) = '32'
      chip_number_char(33) = '33'
      chip_number_char(34) = '34'
      chip_number_char(35) = '35'
      chip_number_char(36) = '36'
      chip_number_char(37) = '37'
      chip_number_char(38) = '38'
      chip_number_char(39) = '39'
      chip_number_char(40) = '40'

      return
      end
c***************************************************
      subroutine tr_alpha_delta_mpc(alpha,delta,
     *    ih1,ih2,iam1,iam2,ias1,alpha_s,
     *    id1,id2,idm1,idm2,ids1,delta_s,sign_delta)

      implicit NONE
      real*8 alpha,alpha_h,delta,alpha_s,delta_s
      real*8 alpha_m,delta_m

      integer ih1,ih2,iam1,iam2,ias1,ih,iam
      integer id1,id2,idm1,idm2,ids1,id,idm

      character*1 sign_delta

      alpha_h = alpha/15.d0
      ih = alpha_h
      ih1 = ih/10
      ih2 = ih-10*ih1

      alpha_m = (alpha_h-ih)*60.d0
      iam  = alpha_m
      iam1 = iam/10
      iam2 = iam-10*iam1

      alpha_s = (alpha_m-iam)*60.d0
      ias1 = alpha_s/10.
      alpha_s = alpha_s-ias1*10.d0

      if(delta.lt.0.d0) then
        sign_delta = '-'
      else
        sign_delta = '+'
      end if

      delta = dabs(delta)
      id = delta
      id1 = id/10
      id2 = id-10*id1

      delta_m = (delta-id)*60.d0
      idm  = delta_m
      idm1 = idm/10
      idm2 = idm-10*idm1

      delta_s = (delta_m-idm)*60.d0
      ids1 = delta_s/10.
      delta_s = delta_s-ids1*10.d0

      return
      end
c***********************************************
      SUBROUTINE julciv(JD,JJAHR,MMONAT,ITAG,IST,IMIN,SEC)
C
C     COMM. ACM, VOL. 11, P. 657, (1968)
C     ICH HABE DAS PROGRAMM ETWAS GEAENDERT UND ERWEITERT
C
C
C
      IMPLICIT REAL*8  (A-Z)
      INTEGER I,J,K,L,M,N,JJAHR,MMONAT,ITAG,IST,IMIN
      LOGICAL SCHALT
      external schalt
      DIMENSION  TAGPM(12)
C
      DATA  TAGPM /31.D0,28.D0,31.D0,30.D0,31.D0,30.D0,31.D0,31.D0,
     F              30.D0,31.D0,30.D0,31.D0/
C
C
C
C
      L = JD + 68569
      N = 4*L/146097
      L = L - (146097*N+3)/4
      I = 4000*(L+1)/1461001
      L = L - 1461*I/4 + 31
      J = 80*L/2447
      K = L - 2447*J/80
      L = J/11
      J = J + 2 - 12*L
      I = 100*(N-49) + I + L
      JAHR = I
      MONAT = J
      TAG = K
      L = JD
C
C
C     DER WERT 1.D-9 (ENTSPRECHEND 0.0000864 SEC) IST ZU ADDIEREN,
C     DA BEI DER SUBTRAKTION  JD-L  NUR 9 KORREKTE STELLEN HINTER
C     DEM KOMMA STEHEN, DA JD UND L 7-STELLIGE ZAHLEN SIND. DIE NACH-
C     FOLGENDE ABFRAGE WUERDE OHNE DIESE ADDITION ALSO EVTL. SCHIEF
C     GEHEN.
C
      X = JD - L + 1.D-9
      IF ( X.GE.0.5D0)  TAG = TAG + 1.D0
C
      IF (SCHALT(JAHR))  TAGPM(2) = 29.D0
      IF (TAG.LE.TAGPM(J))  GO TO 100
      TAG = 1.D0
      MONAT = MONAT + 1.D0
      IF (MONAT.LE.12.D0)  GO TO 100
      MONAT = 1.D0
      JAHR = JAHR + 1.D0
 100   CONTINUE
C
C
C
C
C     STUNDEN, MINUTEN, SEKUNDEN BESTIMMEN
C
      M = JD
      X = M + 0.5D0
      IF (X.GT.JD)  X = X - 1.D0
      X = (JD-X)*24.D0
      CALL STHMS (X,ST,MIN,SEC)
      JJAHR = JAHR
      MMONAT = MONAT
      ITAG = TAG
      IST = ST
      IMIN = MIN
      RETURN
      END
      SUBROUTINE  STHMS (X,H,M,S)
C
C     D.H.: REST KLEINER ALS EPS
C
C
C     AUS DEN GEGEBENEN STUNDEN (MIT DEZIMALEN)
C
C     DIE SEKUNDEN WERDEN AUF 3 DEZIMALEN GERUNDET (S0=59999.5) UND
C     EVTL. ZU DEN MINUTEN ADDIERT
C
C
C     ALLES DOPPELT GENAU
C
C
      IMPLICIT REAL*8  (A-Z)
      INTEGER I
C
      ST = X
C
C     REDUKTION AUF DAS INTERVALL (0,24)
C
      ST = DMOD(ST,24.D0)
      IF (ST.LT.0.D0)  ST = ST + 24.D0
C
C
      I = ST
      H = I
      ST = (ST-H)*60.D0
      I = ST
      M = I
      S = (ST-M)*60.D0
C
C     SEKUNDEN EVTL. ZU DEN MINUTEN ADDIEREN
C
      S0 = 59999.5D0
      IF (S*1000.D0-S0)  30,10,10
 10   S = 0.D0
      M = M+1.D0
      IF (M-59.5D0)  30,20,20
 20   M = 0.D0
      H = H + 1.D0
 30   CONTINUE
      H = DMOD(H,24.D0)
      RETURN
      END
      LOGICAL FUNCTION SCHALT (JAHR)
C
C     PRUEFEN, OB JAHR EIN SCHALTJAHR IST (ERGEBNIS .TRUE.)
C     DAS UNTERPROGRAMM DRTEIL WIRD BENOETIGT
C
C
      REAL*8  X,JAHR,EPS,R2
      INTEGER I
      LOGICAL L1,L2,L3,DRTEIL
      external DRTEIL
C
      SCHALT = .FALSE.
      EPS = 1.D-13
      R2 = 4.D0
      L1 = DRTEIL(JAHR,R2,EPS)
      R2 = 100.D0
      L1 = DRTEIL(JAHR,R2,EPS)
      R2 = 400.D0
      L1 = DRTEIL(JAHR,R2,EPS)
      IF (L1)  SCHALT = .TRUE.
      IF (L2)  SCHALT = .FALSE.
      IF (L3)  SCHALT = .TRUE.
      RETURN
      END
      LOGICAL FUNCTION DRTEIL (R1,R2,EPS)
C
      REAL*8  R1,R2,EPS,X
      INTEGER I
C
      R1 = DABS(R1)
      R2 = DABS(R2)
      DRTEIL = .FALSE.
      I = R1/R2 + 1.D-15
      X = R1/R2
      IF (DABS(X-I).LT.EPS)  DRTEIL = .TRUE.
      RETURN
      END


      
