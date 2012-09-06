c mkpltsol.f
c   Release 2002.01.24
c
c     concerns release 2002.01.05
c     two different triangle algorithms can be used
c     if the first fast algorithm fails, the second is applied
c
c     concerns release 2002.01.24
c     test if a USNO star is on the frame
c     the tolerance for comparing USNO stars with object list stars
c     is now 1.5*seeing
c     the edge of the frame is 3.*seeing
c     -----------------------------------------------------------------------

      implicit NONE

      integer i
      integer number_frames

      integer chip_number
      integer naxis1,naxis2
      integer n_axis1_meaning,n_axis2_meaning
      integer icode
      integer idum1
      integer len_image_name
      integer narg,n_plate
      integer n_axis1_direction,n_axis2_direction
      integer nu_fail,nu_ok,nu_warn
      integer n_find_max_stars

      real*8 frame_size_alpha_arcminutes,frame_size_delta_minutes  
      real*8 chip_center_shift_alpha_arcmin,chip_center_shift_delta_m
      real*8 focal_length,x_off,y_off
      real*8 alpha_camera_center_deg,
     *             delta_camera_center
      real*8 pix_scale,tol_dis_usno_objstars_pix
      real*8 tol_dis_usno_objstars_arcsec,seeing_pix_units
      real*8 x
      real*8 obs_date,dum(7)
      real*8 alpha_proj_center_deg,delta_proj_center
      real*8 dum1,dum2,dum3
      real*8 plc(30)
      real*8 pie,pierez

      character*1 single_char
      character*80 filename_image,arg

      character*100 filename_stars_coord,filename_matching_list
      character*100 filename_plate_solution
      character*100 ffilename_stars_coord(3)
      character*100 ffilename_plate_solution(3)
      character*100 filename_usno_coord_mag
      character*100 usno_star_file
      character*100 filename_usno_not_identified
      character*100 filename_object_list
      character*100 filename_failed
      character*100 filename_ok,filename_warn
      character*100 usno_star_file_dummy

      character*1 axis1_meaning,axis2_meaning,
     *            axis1_direction,axis2_direction
      character*19 name_camera

      pie = 3.141592653589793D0
      pierez = pie/180.d0


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

      filename_stars_coord =
     *   filename_image(1:len_image_name)//'.bright.jmp'
      filename_plate_solution = 
     *    filename_image(1:len_image_name)//'.platesol'
      filename_usno_coord_mag =
     *    filename_image(1:len_image_name)//'.usno'
      filename_usno_not_identified =
     *    filename_image(1:len_image_name)//'.usno_no_ident'
      filename_object_list =
     *    filename_image(1:len_image_name)//'.obj.jmp'
      filename_failed =
     *    filename_image(1:len_image_name)//'.mkpltsol.FAILED'
      filename_warn =
     *    filename_image(1:len_image_name)//'.LOWUSNO.WARNING'
      filename_ok =
     *    filename_image(1:len_image_name)//'.mkpltsol.OK'
      usno_star_file = 
     *    filename_image(1:len_image_name)//'.usno_stars_read_from_cd'
      filename_matching_list = 
     *    filename_image(1:len_image_name)//'.usno_br_match'
      usno_star_file_dummy = 
     *    filename_image(1:len_image_name)//'.usno_dummy'

      nu_fail = 19
      nu_ok = 18
      nu_warn = 17
      open(nu_fail,file=filename_failed,status='unknown')


      open(2,file=filename_stars_coord,status='old')
      read(2,1000) obs_date,dum(1),dum(2),dum(3),
     *             dum(4),alpha_camera_center_deg,
     *             delta_camera_center,idum1
 1000 format(2x, f16.7, f8.2, 2f6.2, f9.1, 2f11.5, i9)
 1001 format(2x, f6.3, i4, 2f10.2, 2i6, 1x, a19)
      read(2,1001) dum(5),chip_number,dum(6),dum(7),naxis1,naxis2,
     *             name_camera
      pix_scale = dum(5)
      close(2)

      icode = 2
      if(name_camera.eq.'CFH12K') then
        call cfh_12k(chip_number,
     *                   nu_fail,
     *                   frame_size_alpha_arcminutes,
     *                   frame_size_delta_minutes,
     *                   axis1_meaning,axis1_direction,
     *                   axis2_meaning,axis2_direction,
     *                   chip_center_shift_alpha_arcmin,
     *                   chip_center_shift_delta_m,icode)
      end if
      if(name_camera.eq.'CCDMosaThin1') then
        call kittpeak_camera(chip_number,
     *                   nu_fail,
     *                   frame_size_alpha_arcminutes,
     *                   frame_size_delta_minutes,
     *                   axis1_meaning,axis1_direction,
     *                   axis2_meaning,axis2_direction,
     *                   chip_center_shift_alpha_arcmin,
     *                   chip_center_shift_delta_m,icode)
      end if
      if(icode.ne.0) then
        write(nu_fail,*) 'CODE DID FAIL icode = ',icode
        close(nu_fail)
        stop
      end if

      chip_center_shift_alpha_arcmin = chip_center_shift_alpha_arcmin
     *               /dcos(delta_camera_center*pierez)

c     match frame stars with USNO stars 
c     ---------------------------------
      call match_usno_1(filename_stars_coord,filename_matching_list,
     *                 filename_usno_coord_mag,
     *                 usno_star_file,
     *                 usno_star_file_dummy,
     *       frame_size_alpha_arcminutes,frame_size_delta_minutes,
     *                   axis1_meaning,axis1_direction,
     *                   axis2_meaning,axis2_direction,
     *                   n_axis1_meaning,n_axis2_meaning,
     *                   chip_center_shift_alpha_arcmin,
     *                   chip_center_shift_delta_m,
     *             nu_fail,
     *             focal_length,
     *             x_off,y_off,pix_scale,
     *             alpha_proj_center_deg,delta_proj_center,
     *             alpha_camera_center_deg,
     *             delta_camera_center,naxis1,naxis2,icode)
      if(icode.gt.600) then
        write(nu_fail,*) 'Fast triangle algorithm failed; try slow one'
        call match_usno_2(filename_stars_coord,filename_matching_list,
     *                 filename_usno_coord_mag,
     *                 usno_star_file,
     *                 usno_star_file_dummy,
     *       frame_size_alpha_arcminutes,frame_size_delta_minutes,
     *                   axis1_meaning,axis1_direction,
     *                   axis2_meaning,axis2_direction,
     *                   n_axis1_meaning,n_axis2_meaning,
     *                   chip_center_shift_alpha_arcmin,
     *                   chip_center_shift_delta_m,
     *             nu_fail,
     *             focal_length,
     *             x_off,y_off,pix_scale,
     *             alpha_proj_center_deg,delta_proj_center,
     *             alpha_camera_center_deg,
     *             delta_camera_center,naxis1,naxis2,icode)
      end if
      if(icode.ne.0) then
        write(nu_fail,*) 'CODE DID FAIL icode = ',icode
        write(nu_fail,*) 'icode = ',icode
        close(nu_fail)
        stop
      end if

c     astrometry
c     ----------
      n_plate = 1
      call plate_solution(n_plate,
     *             filename_plate_solution,
     *             filename_matching_list,
     *                   axis1_meaning,axis1_direction,
     *                   axis2_meaning,axis2_direction,
     *              n_axis1_meaning,n_axis1_direction,
     *              n_axis2_direction,
     *             nu_fail,
     *              focal_length,x_off,y_off,
     *              plc,
     *             alpha_proj_center_deg,delta_proj_center,
     *             naxis1,naxis2,icode)
      if(icode.ne.0) then
        icode = 40
        write(nu_fail,*) 'CODE DID FAIL icode = ',icode
        write(nu_fail,*) 'icode = ',icode
        close(nu_fail)
        stop
      end if

      open(9,file=filename_object_list,status='old')
      read(9,*) single_char,dum1,dum2,dum3,seeing_pix_units
C BG      tol_dis_usno_objstars_pix = 1.5*seeing_pix_units
      tol_dis_usno_objstars_pix = 2.0*seeing_pix_units
      tol_dis_usno_objstars_arcsec = tol_dis_usno_objstars_pix*pix_scale
      close(9)

      n_find_max_stars = 1
      call find_maximum_usno_stars(
     *              filename_object_list,
     *              filename_usno_coord_mag,
     *              filename_usno_not_identified,
     *              filename_matching_list,
     *              usno_star_file,
     *              n_find_max_stars,
     *              n_axis1_meaning,n_axis1_direction,
     *              n_axis2_direction,
     *             nu_fail,
     *             nu_warn,
     *             filename_warn,
     *              tol_dis_usno_objstars_arcsec,
     *              focal_length,x_off,y_off,
     *              plc,
     *             alpha_proj_center_deg,delta_proj_center,
     *             seeing_pix_units,
     *             naxis1,naxis2,icode)

      n_plate = 2
      call plate_solution(n_plate,
     *             filename_plate_solution,
     *             filename_matching_list,
     *                   axis1_meaning,axis1_direction,
     *                   axis2_meaning,axis2_direction,
     *              n_axis1_meaning,n_axis1_direction,
     *              n_axis2_direction,
     *             nu_fail,
     *              focal_length,x_off,y_off,
     *              plc,
     *             alpha_proj_center_deg,delta_proj_center,
     *             naxis1,naxis2,icode)
      if(icode.ne.0) then
        icode = 50
        write(nu_fail,*) 'CODE DID FAIL icode = ',icode
        write(nu_fail,*) 'icode = ',icode
        close(nu_fail)
        stop
      end if

      n_find_max_stars = 2
      call find_maximum_usno_stars(
     *              filename_object_list,
     *              filename_usno_coord_mag,
     *              filename_usno_not_identified,
     *              filename_matching_list,
     *              usno_star_file,
     *              n_find_max_stars,
     *              n_axis1_meaning,n_axis1_direction,
     *              n_axis2_direction,
     *             nu_fail,
     *             nu_warn,
     *             filename_warn,
     *              tol_dis_usno_objstars_arcsec,
     *              focal_length,x_off,y_off,
     *              plc,
     *             alpha_proj_center_deg,delta_proj_center,
     *             seeing_pix_units,
     *             naxis1,naxis2,icode)

      n_plate = 2
      call plate_solution(n_plate,
     *             filename_plate_solution,
     *             filename_matching_list,
     *                   axis1_meaning,axis1_direction,
     *                   axis2_meaning,axis2_direction,
     *              n_axis1_meaning,n_axis1_direction,
     *              n_axis2_direction,
     *             nu_fail,
     *              focal_length,x_off,y_off,
     *              plc,
     *             alpha_proj_center_deg,delta_proj_center,
     *             naxis1,naxis2,icode)
      if(icode.ne.0) then
        icode = 51
        write(nu_fail,*) 'CODE DID FAIL icode = ',icode
        write(nu_fail,*) 'icode = ',icode
        close(nu_fail)
        stop
      end if

      n_find_max_stars = 3
      call find_maximum_usno_stars(
     *              filename_object_list,
     *              filename_usno_coord_mag,
     *              filename_usno_not_identified,
     *              filename_matching_list,
     *              usno_star_file,
     *              n_find_max_stars,
     *              n_axis1_meaning,n_axis1_direction,
     *              n_axis2_direction,
     *             nu_fail,
     *             nu_warn,
     *             filename_warn,
     *              tol_dis_usno_objstars_arcsec,
     *              focal_length,x_off,y_off,
     *              plc,
     *             alpha_proj_center_deg,delta_proj_center,
     *             seeing_pix_units,
     *             naxis1,naxis2,icode)

      n_plate = 2
      call plate_solution(n_plate,
     *             filename_plate_solution,
     *             filename_matching_list,
     *                   axis1_meaning,axis1_direction,
     *                   axis2_meaning,axis2_direction,
     *              n_axis1_meaning,n_axis1_direction,
     *              n_axis2_direction,
     *             nu_fail,
     *              focal_length,x_off,y_off,
     *              plc,
     *             alpha_proj_center_deg,delta_proj_center,
     *             naxis1,naxis2,icode)
      if(icode.ne.0) then
        icode = 51
        write(nu_fail,*) 'CODE DID FAIL icode = ',icode
        write(nu_fail,*) 'icode = ',icode
        close(nu_fail)
        stop
      end if

      
c     call final_match_usno_obj(
c    *              filename_object_list,
c    *              filename_usno_coord_mag,
c    *              filename_usno_not_identified,
c    *              usno_star_file,
c    *              n_axis1_meaning,n_axis1_direction,
c    *              n_axis2_direction,
c    *             nu_warn,
c    *             filename_warn,
c    *             nu_fail,
c    *             seeing_pix_units,
c    *              tol_dis_usno_objstars_pix,
c    *              focal_length,x_off,y_off,
c    *             alpha_proj_center_deg,delta_proj_center,
c    *             naxis1,naxis2,icode)

      close(nu_fail)

      open(nu_ok,file=filename_ok,status='unknown')
      write(nu_ok,*) 'OK '
      close(nu_ok)

      stop
      end
c-------------------------------------------------------------------
      subroutine match_usno_1
     *            (filename_stars_coord,filename_matching_list,
     *                 filename_usno_coord_mag,
     *                  usno_star_file,
     *                 usno_star_file_dummy,
     *       frame_size_alpha_arcminutes,frame_size_delta_minutes,
     *                   axis1_meaning,axis1_direction,
     *                   axis2_meaning,axis2_direction,
     *                   n_axis1_meaning,n_axis2_meaning,
     *                   chip_center_shift_alpha_arcmin,
     *                   chip_center_shift_delta_m,
     *             nu_fail,
     *             focal_length,
     *             x_off,y_off,pix_scale,
     *             alpha_proj_center_deg,delta_proj_center,
     *             alpha_camera_center_deg,
     *             delta_camera_center,naxis1,naxis2,icode)

c     Triangle method developed January 2002
c     ---------------------------------------

      implicit NONE
      integer max_bright_stars,max_bright_stars_center
      integer max_usno,max_usno_center

      parameter(
     *          max_usno=5000,
     *          max_usno_center=500,
     *          max_bright_stars=500,
     *          max_bright_stars_center=500)

      real*8 alpha_center_frame_deg
      real*8 delta_center_frame_deg
      real*8 alpha_camera_center_deg
      real*8 delta_camera_center
      real*8 chip_center_shift_delta_m
      real*8 chip_center_shift_alpha_arcmin
      real*8 chip_center_shift_alpha_pix
      real*8 chip_center_shift_delta_pix
      real*8 ualpha,udelta,umag 
      real*8 usno_alpha_deg(max_usno),usno_alpha_deg_sort(max_usno)
      real*8 usno_alpha_deg_orig(max_usno)
      real*8 usno_delta_orig(max_usno)
      real*8 usno_mag_orig(max_usno)
      real*8 usno_delta(max_usno),usno_delta_sort(max_usno)
      real*8 usno_mag(max_usno),usno_mag_sort(max_usno)
      real*8 frame_size_alpha_arcminutes,frame_size_delta_minutes
      real*8 frame_size_alpha_deg,
     *        frame_size_delta,mag_limit
      real*8 size_alpha,size_delta
      real*8 alpha_deg,delta_deg,x_gnom,y_gnom
      real*8 usno_gnom(max_usno,2),usno_sort(max_usno,2)
      real*8 ratio_x,ratio_y
      real*8 alpha_max,delta_min
      real*8 xbr,ybr,xbrmin,ybrmin,xbrmax,ybrmax,xnax
      real*8 dist_usno(max_usno_center,max_usno_center) 
      real*8 pie,pierez
      real*8 focal_length,tol_offset,tol_offset_2
      real*8 focal_length_orig,pix_scale
      real*8 x_off,y_off,d,d_min
      real*8 x_off_max,y_off_max,xx_off,yy_off
      real*8 dist_br
     *  (max_bright_stars_center,max_bright_stars_center)
      real*8 xbrcat(max_bright_stars),ybrcat(max_bright_stars)
      real*8 xbrcat_center(max_bright_stars)
      real*8 ybrcat_center(max_bright_stars)
      real*8 xbrcat_sort(max_bright_stars)
      real*8 ybrcat_sort(max_bright_stars)
      real*8 usnox(max_usno),usnoy(max_usno) 
      real*8 center_field_size_minutes
      real*8 xcenter,ycenter
      real*8 min_dist_br_stars
      real*8 min_dist_usno_stars,min_dist_usno_stars_2
      real*8 x_closest,y_closest,x,y,xx,yy
      real*8 field_size_deg
      real*8 usno_alpha_min,usno_alpha_max,usno_delta_min,
     *       usno_delta_max
      real*8 usno_br_x(max_usno),usno_br_y(max_usno)
      real*8 alpha_proj_center_deg,delta_proj_center
      real*8 d12_u,d13_u,d23_u
      real*8 d12_b,d13_b,d23_b
      real*8 r12,r13,r23
      real*8 d12,d13,d23
      real*8 b12,b13,b23
      real*8 offset_max_error_pix
      real*8 tol_triangle_match_ratio,tol_triangle_match_near1
      real*8 tol_triangle_match_ratio_0,delta_match_ratio
      real*8 tol_triangle_match_near1_0,delta_match_near1
      real*8 focal_length_fac
      real*8 diff1,diff2,diff3,tol_offset_step1,tol_offset_step2
      real*8 tol_offset_step2_0
      real*8 x_dev,y_dev,dist
      real*8 offset_stand_dev

      

      integer i,j,k,n,n_usno,nz,iu,ju,ku,ii,jj,kk,nzmax
      integer i2,i3,i4,i5,i6
      integer n_tol_offset_step2
      integer n_triangles
      integer iu1,iu2,iu3,ib1,ib2,ib3
      integer nu_fail
      integer n1,n2,icode
      integer nm_br
      integer n_br,n_br_center
      integer n_axis1_meaning,n_axis2_meaning
      integer naxis1,naxis2,nax
      integer iah,iam,idg,idm
      real*8 as,ds
      integer indx_sort_usno(max_usno)
      integer usno_n(max_usno)
      integer n_proposed_triangle_br(max_bright_stars_center)
      integer match_numbers(max_usno_center,max_bright_stars_center)
      integer match_proposed(max_usno_center,2)
      integer br_intensity(max_bright_stars)
      integer br_intensity_center(max_bright_stars_center)
      integer inten,inten_closest
      integer n_usno_center
      integer n_usno_orig
      integer indx_sort_br(max_bright_stars_center)
      integer running_number_br(max_bright_stars)
      integer running_number_usno(max_usno)
      integer running_number_usno_sort(max_usno)
      integer ident_br(max_bright_stars),br_usno_ident(max_bright_stars)
      integer number_frames
      integer number_tr_match
      integer iter_triangle_match
      integer iter_increase_field_size
      integer nstart
      integer min_number_n_usno_center
      integer max_number_n_usno_center
      integer ident_b
      integer identification_u(max_usno_center)
      integer max_iter_increase_field_size
      integer min_number_identif_usno_stars

      character*100 usno_star_file
      character*100 usno_star_file_dummy
      character*100 filename_stars_coord
      character*100 filename_matching_list
      character*100 filename_usno_coord_mag
      character*1 sign_delta,minus,blank,axis1_meaning,axis2_meaning,
     *            meaning(4),axis1_direction,axis2_direction

      logical participate(max_usno),participate_br(max_bright_stars)
      logical participate_u(max_usno_center)
      logical cont,triangle_found

      icode = 0

      min_dist_br_stars = 40.d0
      min_dist_usno_stars = min_dist_br_stars
      min_dist_usno_stars_2 = min_dist_usno_stars**2

      mag_limit = 15.d0
      center_field_size_minutes = 4.0d0
      max_number_n_usno_center = 25
      min_number_n_usno_center = 10
      max_iter_increase_field_size = 10
      min_number_identif_usno_stars = 35

      offset_max_error_pix = 200.d0

      tol_triangle_match_ratio_0 = 0.000d0
      delta_match_ratio = 0.0005d0

      tol_triangle_match_near1_0 = 0.0d0
      delta_match_near1 = 0.002d0

      iter_triangle_match = 5

      tol_offset_step1 = min_dist_usno_stars/2.d0
      tol_offset_step2_0 = 3.d0
      n_tol_offset_step2 = 4
      do i =1,n_tol_offset_step2
        tol_offset_step2_0 = tol_offset_step2_0*2.d0
      end do

      tol_offset = min_dist_usno_stars/2.d0

      offset_stand_dev = 3.d0


      pie = 3.141592653589793D0
      pierez = pie/180.d0
      minus = '-'
      blank = ' '
      meaning(1) = 'a'
      meaning(2) = 'A'
      meaning(3) = 'd'
      meaning(4) = 'D'
      nz = 0
      do i = 1,4
        if(meaning(i).eq.axis1_meaning) nz = i
      end do
      if(nz.eq.0) then
        write(nu_fail,*) 'letter indicating axis1_correspondence ',
     *             'in match_parm_file wrong'
      icode = 1
      return
      end if
      n_axis1_meaning = (nz+1)/2
      nz = 0
      do i = 1,4
        if(meaning(i).eq.axis2_meaning) nz = i
      end do
      if(nz.eq.0) then
        write(nu_fail,*) 'letter indicating axis2_correspondence ',
     *             'in match_parm_file wrong'
      icode = 2
      return
      end if
      n_axis2_meaning = (nz+1)/2

      frame_size_alpha_deg = frame_size_alpha_arcminutes/60.d0
      frame_size_alpha_deg = frame_size_alpha_deg
     *               /cos(delta_camera_center*pierez)
      frame_size_delta = frame_size_delta_minutes/60.d0
      size_alpha = frame_size_alpha_deg/2.
      size_delta = frame_size_delta/2.



      iter_increase_field_size = 0
      nstart = 0

      chip_center_shift_alpha_pix =
     *     chip_center_shift_alpha_arcmin*60./pix_scale
       chip_center_shift_delta_pix =
     *     chip_center_shift_delta_m*60./pix_scale

      alpha_center_frame_deg = alpha_camera_center_deg
     *                         +chip_center_shift_alpha_arcmin/60.d0
      delta_center_frame_deg = delta_camera_center
     *                          + chip_center_shift_delta_m/60.d0

      alpha_proj_center_deg = alpha_camera_center_deg
      delta_proj_center = delta_camera_center



      do i = 1,max_usno
        participate(i) = .true.
      end do

c     provide bright frame stars and select stars near frame center
c     ----------------------------------------------------------
      call provide_bright_stars
     *  (filename_stars_coord,min_dist_br_stars,
     *   br_intensity,br_intensity_center,
     *   xbrcat,ybrcat,xbrcat_center,ybrcat_center,
     *   dist_br,
     *   n_br,n_br_center,running_number_br,
     *   naxis1,naxis2)
       

c     provide USNO stars and select stars near center
c     -----------------------------------------------
      if(nstart.eq.0) then
        call square(alpha_center_frame_deg,delta_center_frame_deg,
     *                size_alpha,size_delta,n_usno,
     *                 usno_star_file)
        nstart = 1
      else
        open(2,file=usno_star_file,status='old')
        n_usno = 0
  293   read(2,*,end=294) 
        n_usno = n_usno+1
        goto 293
  294   close(2)
      end if
      n_usno_orig = n_usno

      if(n_usno.gt.max_usno) then
        write(nu_fail,*) 'WARNING: increase max_usno '
        n_usno = max_usno
      end if

c     eliminate too brilliant and too close USNO stars
c     ------------------------------------------------
      open(2,file=usno_star_file,status='old')
      do i = 1,n_usno
        read(2,*) usno_alpha_deg(i),usno_delta(i),
     *            usno_mag(i),usno_n(i)
        usno_alpha_deg_orig(i) = usno_alpha_deg(i)
        usno_delta_orig(i)     = usno_delta(i)
        usno_mag_orig(i)       = usno_mag(i)
          usno_gnom(i,1) = (usno_alpha_deg(i)-alpha_center_frame_deg)
     *                      *3600.
     *                      /pix_scale
     *                      /cos(delta_center_frame_deg*pierez)
          usno_gnom(i,2) = (usno_delta(i)-delta_center_frame_deg) 
     *                      *3600.
     *                     /pix_scale
      end do
      do i = 1,n_usno
        participate(i) = .true.
      end do
      do i = 1,n_usno
        if(usno_mag(i).lt.mag_limit) participate(i) = .false.
      end do
      do i = 1,n_usno-1
        do j = i+1,n_usno
          if((usno_gnom(j,1)-usno_gnom(i,1)) 
     *      .gt. 2.*min_dist_usno_stars) goto 8
          d = (usno_gnom(i,1)-usno_gnom(j,1))**2+
     *        (usno_gnom(i,2)-usno_gnom(j,2))**2
          if(d.lt.min_dist_usno_stars_2) then
            participate(i) = .false.
            participate(j) = .false.
          end if
        end do
    8 continue
      end do
      close(2)

      open(2,file=usno_star_file_dummy,status='unknown')
      j = 0
      usno_alpha_min = 10000.d0
      usno_delta_min = 10000.d0
      usno_alpha_max = -1000.d0
      usno_delta_max = -10000.d0
      
      do i = 1,n_usno
        if(participate(i)) then
          j = j+1
          write(2,*) j,usno_alpha_deg(i),usno_delta(i),usno_mag(i)
          usno_alpha_min = dmin1(usno_alpha_min,usno_alpha_deg(i))
          usno_delta_min = dmin1(usno_delta_min,usno_delta(i))
          usno_alpha_max = dmax1(usno_alpha_max,usno_alpha_deg(i))
          usno_delta_max = dmax1(usno_delta_max,usno_delta(i))
        end if
      end do
      n_usno = j

c     find USNO stars in a small field near frame center
c     --------------------------------------------------
      field_size_deg =  center_field_size_minutes/60. 
 1110 continue
      usno_alpha_min = 10000.d0
      usno_delta_min = 10000.d0
      usno_alpha_max = -1000.d0
      usno_delta_max = -10000.d0
      rewind 2
      i = 0
      do j = 1,n_usno
        read(2,*) k,ualpha,udelta,umag

        if(dabs(ualpha-alpha_center_frame_deg).gt.field_size_deg) 
     *   goto 10
        if(dabs(udelta-delta_center_frame_deg).gt.field_size_deg) 
     *     goto 10
        i = i+1
        if(i.gt.max_usno_center) then
          write(nu_fail,*) 'WARNING: increase max_usno_center'
        i = max_usno_center
        end if        
        usno_alpha_deg(i) = ualpha
        usno_delta(i) = udelta 
        usno_mag(i) = umag
        running_number_usno(i) = k
          usno_alpha_min = dmin1(usno_alpha_min,usno_alpha_deg(i))
          usno_delta_min = dmin1(usno_delta_min,usno_delta(i))
          usno_alpha_max = dmax1(usno_alpha_max,usno_alpha_deg(i))
          usno_delta_max = dmax1(usno_delta_max,usno_delta(i))
   10 continue
      end do
      n_usno_center = i
      if(n_usno_center.gt.max_number_n_usno_center) then 
        field_size_deg = field_size_deg-field_size_deg/10.
        goto 1110
      end if
      close(2)
      if(n_usno_center.lt.min_number_n_usno_center) then
        iter_increase_field_size = iter_increase_field_size+1
        if(iter_increase_field_size.gt.
     *          max_iter_increase_field_size) then
          write(nu_fail,*) 'number of USNO stars on chip too small',
     *              n_usno_center
          icode = 3
          return
        end if
        field_size_deg = field_size_deg+field_size_deg/10.
        goto 1110
      end if
        
c     gnomonic projection of USNO stars
c     ---------------------------------
      alpha_deg = alpha_proj_center_deg
      delta_deg = delta_proj_center+pix_scale/3600.d0
        call equa_deg_to_xygnom(alpha_deg,delta_deg,
     *      alpha_proj_center_deg,delta_proj_center,
     *      x_gnom,y_gnom)
      focal_length_orig = 1./y_gnom
 6020 format(2f15.8)
      alpha_max = -1.d0
      delta_min = 100.d0
      focal_length = focal_length_orig
      do i = 1,n_usno_center
        alpha_deg = usno_alpha_deg(i)
        delta_deg = usno_delta(i)
        call equa_deg_to_xygnom(alpha_deg,delta_deg,
     *      alpha_proj_center_deg,delta_proj_center,
     *      x_gnom,y_gnom)
        usno_gnom(i,1) = x_gnom
        usno_gnom(i,2) = y_gnom
      end do

      do i5 = 1,2
c       match astronomical coordinate system with chip coordinate system
c       ascending R.A. and ascending declination corresponds to which axes ?
c       -------------------------------------    
        do i = 1,n_usno_center
          usnox(i) = usno_gnom(i,1)*focal_length
          usnoy(i) = usno_gnom(i,2)*focal_length
        end do
        do i = 1,n_usno_center
          if(axis1_direction.eq.minus) usnox(i) = -usnox(i)
          if(axis2_direction.eq.minus) usnoy(i) = -usnoy(i)
        end do
        do i = 1,n_usno_center
          if(n_axis1_meaning.eq.2) then
            y_gnom = usnox(i)
            x_gnom = usnoy(i)
            usnox(i) = x_gnom
            usnoy(i) = y_gnom
          end if
        end do

        do i = 1,n_usno_center
          usnox(i) = usnox(i)+chip_center_shift_alpha_pix+naxis1/2      
          usnoy(i) = usnoy(i)-chip_center_shift_delta_pix+naxis2/2
        end do

        call sortreal8(n_usno_center,usnoy,indx_sort_usno)
        do k = 1,n_usno_center
          i = indx_sort_usno(k)
          usno_sort(k,1) = usnox(i)
          usno_sort(k,2) = usnoy(i)
          usno_delta_sort(k) = usno_delta(i)
          usno_alpha_deg_sort(k) = usno_alpha_deg(i)
          usno_mag_sort(k) = usno_mag(i)
          running_number_usno_sort(k) = running_number_usno(i)
        end do
        do i = 1,n_usno_center
          usnox(i) = usno_sort(i,1)
          usnoy(i) = usno_sort(i,2)
        end do

        do i = 1,n_usno_center-1
          do j = i+1,n_usno_center
            dist_usno(i,j) = dsqrt((usnox(i)-usnox(j))**2
     *           +(usnoy(i)-usnoy(j))**2)
            dist_usno(j,i) = dist_usno(i,j)
          end do
        end do

c       match USNO stars with bright frame stars near frame center
c       mapped triangles matching; compare distance ratios
c       ---------------------------------------------------             

   50   continue
        tol_triangle_match_ratio = tol_triangle_match_ratio_0
        do i3 = 1,iter_triangle_match
          tol_triangle_match_ratio = tol_triangle_match_ratio+
     *           delta_match_ratio
          tol_triangle_match_near1 = tol_triangle_match_near1_0
          do i6 = 1,iter_triangle_match
            tol_triangle_match_near1 = tol_triangle_match_near1+
     *             delta_match_near1
            do i = 1,n_br_center
              participate_br(i) = .true.
            end do
            do i = 1,max_usno_center
              participate_u(i) = .true.
            end do 

            do i2 = 1,2
              do i = 1,max_usno_center
                match_proposed(i,1) = 0
                match_proposed(i,2) = 0
                do j = 1,max_bright_stars_center
                  match_numbers(i,j) = 0
                end do
              end do
              do i = 1,n_br_center
                n_proposed_triangle_br(i) = 0
              end do

              do iu = 1,n_usno_center-2
                if(participate_u(iu)) then
                  do ju = iu+1,n_usno_center-1
                    if(participate_u(ju)) then
                      do ku = ju+1,n_usno_center
                        if(participate_u(ku)) then
                          d12_u = dist_usno(ju,iu)
                          d13_u = dist_usno(ku,iu)
                          d23_u = dist_usno(ku,ju)

                          do i = 1,n_br_center-2

              if(participate_br(i)) then
                 if(dabs(usnox(iu)-xbrcat_center(i))
     *              .lt.offset_max_error_pix .and.
     *                  dabs(usnoy(iu)-ybrcat_center(i))
     *                .lt.offset_max_error_pix) then

                                do j = i+1,n_br_center-1

                     if(participate_br(j)) then
                     if(dabs(usnox(ju)-xbrcat_center(j))
     *                   .lt.offset_max_error_pix .and.
     *                     dabs(usnoy(ju)-ybrcat_center(j))
     *                    .lt.offset_max_error_pix) then

                                      do k = j+1,n_br_center

                     if(participate_br(k)) then
                    if(dabs(usnox(ku)-xbrcat_center(k))
     *                      .lt.offset_max_error_pix .and.
     *                      dabs(usnoy(ku)-ybrcat_center(k))
     *                     .lt.offset_max_error_pix) then
                                              d12_b = dist_br(j,i)
                                              d13_b = dist_br(k,i)
                                              d23_b = dist_br(k,j)
                                              r12 = d12_u/d12_b
                                              r13 = d13_u/d13_b
                                              r23 = d23_u/d23_b
                if(dabs(r13-r12).lt.tol_triangle_match_ratio .and.
     *            dabs(r23-r13).lt.tol_triangle_match_ratio .and.
     *            dabs(r12-r23).lt.tol_triangle_match_ratio) then
                if(dabs(1.-r12).lt.tol_triangle_match_near1 .and.
     *               dabs(1.-r13).lt.tol_triangle_match_near1 .and.
     *               dabs(1.-r23).lt.tol_triangle_match_near1) then
                       match_numbers(iu,i) = match_numbers(iu,i)+1
                       match_numbers(ju,j) = match_numbers(ju,j)+1
                       match_numbers(ku,k) = match_numbers(ku,k)+1
                                                end if
                                              end if
                                            end if
                                          end if
                                        end do
                                      end if
                                    end if
                                  end do  
                                end if
                              end if
                            end do
                          end if
                        end do
                      end if
                    end do
                  end if
                end do

c     test on identified USNO stars in triangles
c     ------------------------------------------

c     select USNO stars with a unique identification
c     test if the identified frame star has a single identification with 
c     a single USNO star
c     ------------------------------------------------------------------
              do i = 1,n_usno_center
                identification_u(i) = 0
              end do
              do i = 1,n_usno_center
                n = 0
                do j = 1,n_br_center
                  if(match_numbers(i,j).ne.0) then
                    n = n+1
                    ident_b = j
                  end if
                end do
                if(n.eq.1) then
                  k = 0
                  do ku = 1,n_usno_center
                    if(match_numbers(ku,ident_b).gt.0) k = k+1
                  end do
                  if(k.eq.1) identification_u(i) = ident_b
                end if
              end do

              do i = 1,n_br_center
                participate_br(i) = .false.
              end do
              do i = 1,max_usno_center
                participate_u(i) = .false.
              end do

              n = 0
              do i = 1,n_usno_center
                if(identification_u(i).ne.0) then
                  n = n+1
                  participate_u(i) = .true.
                  j = identification_u(i)
                  participate_br(j) = .true.
                end if
              end do

            end do
            if(n.ge.3) goto 60

          end do
        end do

   60   if(n.ge.3) then
          triangle_found = .true.
          n_triangles = n
        else
          triangle_found = .false.
        end if

c     use first triangle to improve platescale and determine offset
c     ------------------------------------------------------
        if(.not.triangle_found) then
          icode = 601
          return
        end if

        n = 0
        do i = 1,n_usno_center
          if(identification_u(i).ne.0) then
            n = n+1
            if(n.gt.3) goto 65
            if(n.eq.1) then
              iu1 = i
              ib1 = identification_u(i)
            end if
            if(n.eq.2) then
              iu2 = i
              ib2 = identification_u(i)
            end if
            if(n.eq.3) then
              iu3 = i
              ib3 = identification_u(i)
            end if
          end if
        end do
   65   continue
        d12_u = dist_usno(iu1,iu2)
        d13_u = dist_usno(iu1,iu3)
        d23_u = dist_usno(iu2,iu3)
        d12_b = dist_br(ib1,ib2)
        d13_b = dist_br(ib1,ib3)
        d23_b = dist_br(ib2,ib3)
        r12 = d12_b/d12_u
        r13 = d13_b/d13_u
        r23 = d23_b/d23_u
        focal_length_fac = (r12+r13+r23)/3.
        focal_length = focal_length_orig*focal_length_fac
        diff1 = xbrcat_center(ib1)-usnox(iu1)
        diff2 = xbrcat_center(ib2)-usnox(iu2)
        diff3 = xbrcat_center(ib3)-usnox(iu3)
        x_off = (diff1+diff2+diff3)/3.
        if(dabs(x_off-diff1).gt.tol_offset_step1) then
          icode = 602
          return
        end if
        if(dabs(x_off-diff2).gt.tol_offset_step1) then
          icode = 603
          return
        end if
        if(dabs(x_off-diff3).gt.tol_offset_step1) then    
          icode = 604
          return
        end if
        diff1 = ybrcat_center(ib1)-usnoy(iu1)
        diff2 = ybrcat_center(ib2)-usnoy(iu2)
        diff3 = ybrcat_center(ib3)-usnoy(iu3)
        y_off = (diff1+diff2+diff3)/3.
        if(dabs(y_off-diff1).gt.tol_offset_step1) then    
          icode = 605
          return
        end if
        if(dabs(y_off-diff2).gt.tol_offset_step1) then    
          icode = 606
          return
        end if
        if(dabs(y_off-diff3).gt.tol_offset_step1) then    
          icode = 607
          return
        end if

        chip_center_shift_alpha_pix = chip_center_shift_alpha_pix+x_off
        chip_center_shift_delta_pix = chip_center_shift_delta_pix-y_off

c     recompute pix coordinates of USNO stars in center using improved
c     focal length and offset
c     --------
        tol_offset_step2 = tol_offset_step2_0
        do i4 = 1,n_tol_offset_step2
          tol_offset_step2 = tol_offset_step2/2.
          do i = 1,n_usno_center
            usnox(i) = usno_gnom(i,1)*focal_length
            usnoy(i) = usno_gnom(i,2)*focal_length
          end do
        do i = 1,n_usno_center
          if(axis1_direction.eq.minus) usnox(i) = -usnox(i)
          if(axis2_direction.eq.minus) usnoy(i) = -usnoy(i)
        end do
        do i = 1,n_usno_center
          if(n_axis1_meaning.eq.2) then
            y_gnom = usnox(i)
            x_gnom = usnoy(i)
            usnox(i) = x_gnom
            usnoy(i) = y_gnom
          end if
        end do

        do i = 1,n_usno_center
          usnox(i) = usnox(i)+chip_center_shift_alpha_pix+naxis1/2
          usnoy(i) = usnoy(i)-chip_center_shift_delta_pix+naxis2/2
        end do

        call sortreal8(n_usno_center,usnoy,indx_sort_usno)
        do k = 1,n_usno_center
          i = indx_sort_usno(k)
          usno_sort(k,1) = usnox(i)
          usno_sort(k,2) = usnoy(i)
          usno_delta_sort(k) = usno_delta(i)
          usno_alpha_deg_sort(k) = usno_alpha_deg(i)
          usno_mag_sort(k) = usno_mag(i)
          running_number_usno_sort(k) = running_number_usno(i)
        end do
        do i = 1,n_usno_center
          usnox(i) = usno_sort(i,1)
          usnoy(i) = usno_sort(i,2)
        end do

c     use a maximum of USNO and bright stars to determine offset
c     in central region
c     -------------------------------------------------------
        n = 0
        x_off = 0.d0
        y_off = 0.d0
        do i = 1,n_usno_center
          identification_u(i) = 0
        end do
        do i = 1,n_usno_center
          do j = 1,n_br_center
            dist = dsqrt((usnox(i)-xbrcat_center(j))**2+
     *                 (usnoy(i)-ybrcat_center(j))**2)
            if(dist.lt.tol_offset_step2) then
              n = n+1
              x_off = x_off+(xbrcat_center(j)-usnox(i))
              y_off = y_off+(ybrcat_center(j)-usnoy(i))
              identification_u(i) = j
            end if
          end do
        end do
        if(n.lt.3) then
          icode = 610
          return
        end if
        x_off = x_off/dble(n)
        y_off = y_off/dble(n)
        chip_center_shift_alpha_pix = chip_center_shift_alpha_pix+x_off
        chip_center_shift_delta_pix = chip_center_shift_delta_pix-y_off
        x_dev = 0.d0
        y_dev = 0.d0
        n = 0
        do i = 1,n_usno_center
          if(identification_u(i).ne.0) then
            n = n+1
            j = identification_u(i)
            x_dev = x_dev+(xbrcat_center(j)-usnox(i)-x_off)**2
            y_dev = y_dev+(ybrcat_center(j)-usnoy(i)-y_off)**2
          end if
        end do
        x_dev = x_dev/dble(n-1)
        x_dev = dsqrt(x_dev)
        y_dev = y_dev/dble(n-1)
        y_dev = dsqrt(y_dev)
c       if(dabs(x_off).lt.1.d0 .and. dabs(y_off).lt.1.d0) goto 68
        end do
   68   continue
        if(x_dev.gt.offset_stand_dev) then
          icode = 611
          return  
        end if
        if(y_dev.gt.offset_stand_dev) then
          icode = 612
        end if

      end do

      x_off = chip_center_shift_alpha_pix+naxis1/2 
      y_off = -chip_center_shift_delta_pix+naxis2/2

c     find USNO stars allover the frame
c     --------------------------------------------------
      open(2,file=usno_star_file_dummy,status='unknown')          
      i = 0
   85 read(2,*,end=86) k,ualpha,udelta,umag

      if(dabs(ualpha-alpha_center_frame_deg).gt.
     *                   frame_size_alpha_deg) goto 90
      if(dabs(udelta-delta_center_frame_deg).gt.
     *                    frame_size_delta) goto 90
      i = i+1        
      usno_alpha_deg(i) = ualpha
      usno_delta(i) = udelta 
      usno_mag(i) = umag
      running_number_usno(i) = k
   90 continue
      goto 85
   86 continue
      close(2)
      n_usno = i-1
      if(n_usno.lt.6) then
        write(nu_fail,*) 'number of USNO stars on chip too small',n_usno
        icode = 9
        return
      end if  
c     gnomonic projection of all USNO stars
c     -------------------------------------
      nz = 0
      alpha_max = -1.d0
      delta_min = 100.d0
      do i = 1,n_usno
        alpha_deg = usno_alpha_deg(i)
        delta_deg = usno_delta(i)
        call equa_deg_to_xygnom(alpha_deg,delta_deg,
     *      alpha_proj_center_deg,delta_proj_center,
     *      x_gnom,y_gnom)
        usno_gnom(i,1) = x_gnom
        usno_gnom(i,2) = y_gnom
      end do

c     match astronomical coordinate system with chip coordinate system
c     ascending R.A. and ascending declination corresponds to which axes ?
c     -------------------------------------    
      do i = 1,n_usno
        usnox(i) = usno_gnom(i,1)*focal_length
        usnoy(i) = usno_gnom(i,2)*focal_length
      end do
      do i = 1,n_usno
        if(axis1_direction.eq.minus) usnox(i) = -usnox(i)
        if(axis2_direction.eq.minus) usnoy(i) = -usnoy(i)
      end do
      do i = 1,n_usno
        if(n_axis1_meaning.eq.2) then
          y_gnom = usnox(i)
          x_gnom = usnoy(i)
          usnox(i) = x_gnom
          usnoy(i) = y_gnom
        end if
      end do
      do i = 1,n_usno
        usnox(i) = usnox(i)+x_off
        usnoy(i) = usnoy(i)+y_off
      end do

      call sortreal8(n_usno,usnoy,indx_sort_usno)
      do k = 1,n_usno
        i = indx_sort_usno(k)
        usno_sort(k,1) = usnox(i)
        usno_sort(k,2) = usnoy(i)
        usno_delta_sort(k) = usno_delta(i)
        usno_alpha_deg_sort(k) = usno_alpha_deg(i)
        usno_mag_sort(k) = usno_mag(i)
        running_number_usno_sort(k) = running_number_usno(i)
      end do

      do i = 1,n_usno
        usno_gnom(i,1) = usno_sort(i,1)
        usno_gnom(i,2) = usno_sort(i,2)
      end do

      nz = 0
      open(8,file=filename_matching_list,status='unknown')
      do i = 1,max_usno
        participate(i) = .false.
      end do
      do i = 1,max_bright_stars
        participate_br(i) = .false.
      end do

      do i = 1,n_br
        ident_br(i) = 0
        br_usno_ident(i) = 0
        running_number_br(i) = i
      end do
      tol_offset_2 = tol_offset*tol_offset
      do i = 1,n_br
        do iu = 1,n_usno
          if(dabs(usno_gnom(iu,2)-ybrcat(i)).lt.tol_offset) then
            d = (usno_gnom(iu,1)-xbrcat(i))**2 +
     *          (usno_gnom(iu,2)-ybrcat(i))**2
            if(d.lt.tol_offset_2) then
              ident_br(i) = ident_br(i)+1
              br_usno_ident(i) = iu
            end if
          end if
        end do
      end do

      nz = 0
      do i = 1,n_br
        if(ident_br(i).eq.1) then
              iu = br_usno_ident(i)
              ualpha = usno_alpha_deg_sort(iu)/15.d0
              iah = ualpha
              iam = int((ualpha-iah)*60.)
              as = (ualpha-iah-dble(iam)/60.)*3600.
              udelta = usno_delta_sort(iu)
              sign_delta = blank
              if(udelta.lt.0.d0) sign_delta = minus
              udelta = dabs(udelta)
              idg = udelta
              idm = (udelta-idg)*60.
              ds = (udelta-idg-dble(idm)/60.)*3600.
              nz = nz+1
              participate(iu) = .true.
              participate_br(i) = .true.
              write(8,3000) xbrcat(i),ybrcat(i),
     *         usno_alpha_deg_sort(iu),usno_delta_sort(iu)
 3000 format(2f10.2,2f15.10)
c             write(8,2000) running_number_br(i),
c    *         xbrcat(i),ybrcat(i),br_intensity(i),
c    *         running_number_usno_sort(iu),
c    *         usno_mag_sort(iu),iah,iam,as,sign_delta,idg,idm,ds,
c    *         usno_alpha_deg_sort(iu),usno_delta_sort(iu)
c2000 format (i5,2f8.2,i6,i5,f7.1,2x,2i3,f6.2,
c    *        3x,a1,i2,i3,f5.1,2x,2f15.10)
        end if
      end do 
      close(8)
      write(nu_fail,*) 'number of identified bright stars',nz

c     compute pixel coordinates for all USNO stars on frame
c     -----------------------------------------------------
      open(8,file=filename_usno_coord_mag)
      do i = 1,n_usno_orig
          alpha_deg = usno_alpha_deg_orig(i)
          delta_deg = usno_delta_orig(i)
          call equa_deg_to_xygnom(alpha_deg,delta_deg,
     *      alpha_proj_center_deg,delta_proj_center,
     *      x_gnom,y_gnom)
          usno_gnom(i,1) = x_gnom
          usno_gnom(i,2) = y_gnom
          usnox(i) = usno_gnom(i,1)*focal_length
          usnoy(i) = usno_gnom(i,2)*focal_length
          if(axis1_direction.eq.minus) usnox(i) = -usnox(i)
          if(axis2_direction.eq.minus) usnoy(i) = -usnoy(i)
          if(n_axis1_meaning.eq.2) then
            y_gnom = usnox(i)
            x_gnom = usnoy(i)
            usnox(i) = x_gnom
            usnoy(i) = y_gnom
          end if
          usnox(i) = usnox(i)+x_off
          usnoy(i) = usnoy(i)+y_off
          if(usnox(i).gt.10. .and.usnox(i).lt.dble(naxis1-10.).and.
     *       usnoy(i).gt.10. .and.usnox(i).lt.dble(naxis2)-10.) then
            write(8,8080) usnox(i),usnoy(i),usno_mag_orig(i),usno_n(i),
     *                  usno_alpha_deg_orig(i),usno_delta_orig(i)
 8080 format(2f10.1,f6.1,i11,2f15.7)
          end if
      end do
      close(8)
      return
      end

c-------------------------------------------------------------------
      subroutine
     *   match_usno_2(filename_stars_coord,filename_matching_list,
     *                 filename_usno_coord_mag,
     *                 usno_star_file,
     *                 usno_star_file_dummy,
     *       frame_size_alpha_arcminutes,frame_size_delta_minutes,
     *                   axis1_meaning,axis1_direction,
     *                   axis2_meaning,axis2_direction,
     *                   n_axis1_meaning,n_axis2_meaning,
     *                   chip_center_shift_alpha_arcmin,
     *                   chip_center_shift_delta_m,
     *             nu_fail,
     *             focal_length,
     *             x_off,y_off,pix_scale,
     *             alpha_proj_center_deg,delta_proj_center,
     *             alpha_camera_center_deg,
     *             delta_camera_center,naxis1,naxis2,icode)

c     Triangle method developed in 2001
c     ---------------------------------
      implicit NONE
      integer max_bright_stars,max_bright_stars_center
      integer max_usno,max_usno_center

      parameter(
     *          max_usno=5000,
     *          max_usno_center=500,
     *          max_bright_stars=500,
     *          max_bright_stars_center=500)

      real*8 alpha_center_frame_deg
      real*8 delta_center_frame_deg
      real*8 alpha_camera_center_deg
      real*8 delta_camera_center
      real*8 chip_center_shift_delta_m
      real*8 chip_center_shift_alpha_arcmin
      real*8 chip_center_shift_alpha_pix
      real*8 chip_center_shift_delta_pix
      real*8 ualpha,udelta,umag 
      real*8 usno_alpha_deg(max_usno),usno_alpha_deg_sort(max_usno)
      real*8 usno_alpha_deg_orig(max_usno)
      real*8 usno_delta_orig(max_usno)
      real*8 usno_mag_orig(max_usno)
      real*8 usno_delta(max_usno),usno_delta_sort(max_usno)
      real*8 usno_mag(max_usno),usno_mag_sort(max_usno)
      real*8 frame_size_alpha_arcminutes,frame_size_delta_minutes
      real*8 frame_size_alpha_deg,
     *        frame_size_delta,mag_limit
      real*8 size_alpha,size_delta
      real*8 alpha_deg,delta_deg,x_gnom,y_gnom
      real*8 usno_gnom(max_usno,2),usno_sort(max_usno,2)
      real*8 ratio_x,ratio_y
      real*8 alpha_max,delta_min
      real*8 xbr,ybr,xbrmin,ybrmin,xbrmax,ybrmax,xnax
      real*8 dist_usno(max_usno_center,max_usno_center) 
      real*8 pie,pierez
      real*8 focal_length,tol_focal_length,tol_offset,tol_offset_2
      real*8 focal_length_orig,pix_scale
      real*8 tol_triangle_match
      real*8 r12_13,r12_23,r13_23,r12,r13,r23
      real*8 b12_13,b12_23,b13_23
      real*8 d12,d13,d23,b12,b13,b23,x_off,y_off,d,d_min
      real*8 x_off_max,y_off_max,xx_off,yy_off
      real*8 dist_br_center
     *  (max_bright_stars_center,max_bright_stars_center)
      real*8 xbrcat(max_bright_stars),ybrcat(max_bright_stars)
      real*8 xbrcat_center(max_bright_stars)
      real*8 ybrcat_center(max_bright_stars)
      real*8 xbrcat_sort(max_bright_stars)
      real*8 ybrcat_sort(max_bright_stars)
      real*8 usnox(max_usno),usnoy(max_usno) 
      real*8 center_field_size_minutes
      real*8 xcenter,ycenter
      real*8 min_dist_br_stars
      real*8 min_dist_usno_stars,min_dist_usno_stars_2
      real*8 x_closest,y_closest,x,y,xx,yy
      real*8 field_size_deg
      real*8 usno_alpha_min,usno_alpha_max,usno_delta_min,
     *       usno_delta_max
      real*8 usno_br_x(max_usno),usno_br_y(max_usno)
      real*8 x1,x2,x3,xm,y1,y2,y3,ym
      real*8 alpha_proj_center_deg,delta_proj_center
      real*8 tol_chip_center_shift
      

      integer i,j,k,n_usno,nz,iu,ju,ku,ii,jj,kk,nzmax
      integer nu_fail
      integer n1,n2,icode
      integer nm_br
      integer n_br,n_br_center
      integer n_axis1_meaning,n_axis2_meaning
      integer naxis1,naxis2,nax
      integer iah,iam,idg,idm
      real*8 as,ds
      integer indx_sort_usno(max_usno)
      integer usno_n(max_usno)
      integer n_proposed_triangle_br(max_bright_stars_center)
      integer match_numbers(max_usno_center,max_bright_stars_center)
      integer match_proposed(max_usno_center,2)
      integer br_intensity(max_bright_stars)
      integer br_intensity_center(max_bright_stars_center)
      integer inten,inten_closest
      integer n_usno_center
      integer n_usno_orig
      integer indx_sort_br(max_bright_stars_center)
      integer running_number_br(max_bright_stars)
      integer running_number_usno(max_usno)
      integer running_number_usno_sort(max_usno)
      integer ident_br(max_bright_stars),br_usno_ident(max_bright_stars)
      integer number_frames
      integer number_tr_match
      integer iter_offset,iter_triangle_match
      integer iter_chip_center_shift,iter_focal_length
      integer iter_increase_field_size
      integer nstart
      integer min_number_n_usno_center
      integer max_number_n_usno_center

      character*100 usno_star_file
      character*100 usno_star_file_dummy
      character*100 filename_stars_coord
      character*100 filename_matching_list
      character*100 filename_usno_coord_mag
      character*1 sign_delta,minus,blank,axis1_meaning,axis2_meaning,
     *            meaning(4),axis1_direction,axis2_direction

      logical participate(max_usno),participate_br(max_bright_stars)
      logical cont

      mag_limit = 15.d0
      center_field_size_minutes = 4.0d0
      max_number_n_usno_center = 25
      min_number_n_usno_center = 10

      icode = 0

      pie = 3.141592653589793D0
      pierez = pie/180.d0
      minus = '-'
      blank = ' '
      meaning(1) = 'a'
      meaning(2) = 'A'
      meaning(3) = 'd'
      meaning(4) = 'D'
      nz = 0
      do i = 1,4
        if(meaning(i).eq.axis1_meaning) nz = i
      end do
      if(nz.eq.0) then
        write(nu_fail,*) 'letter indicating axis1_correspondence ',
     *             'in match_parm_file wrong'
      icode = 1
      return
      end if
      n_axis1_meaning = (nz+1)/2
      nz = 0
      do i = 1,4
        if(meaning(i).eq.axis2_meaning) nz = i
      end do
      if(nz.eq.0) then
        write(nu_fail,*) 'letter indicating axis2_correspondence ',
     *             'in match_parm_file wrong'
      icode = 2
      return
      end if
      n_axis2_meaning = (nz+1)/2

      frame_size_alpha_deg = frame_size_alpha_arcminutes/60.d0
      frame_size_alpha_deg = frame_size_alpha_deg
     *               /cos(delta_camera_center*pierez)
      frame_size_delta = frame_size_delta_minutes/60.d0
      size_alpha = frame_size_alpha_deg/2.
      size_delta = frame_size_delta/2.


      iter_increase_field_size = 0
      iter_chip_center_shift = 0
      tol_chip_center_shift = 3.d0
      nstart = 0

  400 continue
      chip_center_shift_alpha_pix =
     *     chip_center_shift_alpha_arcmin*60./pix_scale
       chip_center_shift_delta_pix =
     *     chip_center_shift_delta_m*60./pix_scale

      alpha_center_frame_deg = alpha_camera_center_deg
     *                         +chip_center_shift_alpha_arcmin/60.d0
      delta_center_frame_deg = delta_camera_center
     *                          + chip_center_shift_delta_m/60.d0

c     alpha_proj_center_deg = alpha_center_frame_deg
c     delta_proj_center = delta_center_frame_deg
      alpha_proj_center_deg = alpha_camera_center_deg
      delta_proj_center = delta_camera_center
c     chip_center_shift_alpha_pix = 0.d0
c     chip_center_shift_delta_pix = 0.d0

      iter_offset = 0
      tol_offset = 20.d0

  290 continue

      tol_triangle_match = 0.01d0

      min_dist_br_stars = 2.*tol_offset
      min_dist_usno_stars = min_dist_br_stars
      min_dist_usno_stars_2 = min_dist_usno_stars**2

      icode = 0

      do i = 1,max_usno
        participate(i) = .true.
      end do

c     provide bright frame stars and select stars near frame center
c     ----------------------------------------------------------
      call provide_bright_stars
     *  (filename_stars_coord,min_dist_br_stars,
     *   br_intensity,br_intensity_center,
     *   xbrcat,ybrcat,xbrcat_center,ybrcat_center,
     *   dist_br_center,
     *   n_br,n_br_center,running_number_br,
     *   naxis1,naxis2)
       

c     provide USNO stars and select stars near center
c     -----------------------------------------------
      if(nstart.eq.0) then
        call square(alpha_center_frame_deg,delta_center_frame_deg,
     *                size_alpha,size_delta,n_usno,
     *                 usno_star_file)
        nstart = 1
      else
        open(2,file=usno_star_file,status='old')
        n_usno = 0
  293   read(2,*,end=294) 
        n_usno = n_usno+1
        goto 293
  294   close(2)
      end if
      n_usno_orig = n_usno

      if(n_usno.gt.max_usno) then
        write(nu_fail,*) 'WARNING: increase max_usno '
        n_usno = max_usno
      end if

c     eliminate too brilliant and too close USNO stars
c     ------------------------------------------------
      open(2,file=usno_star_file,status='old')
      do i = 1,n_usno
        read(2,*) usno_alpha_deg(i),usno_delta(i),
     *            usno_mag(i),usno_n(i)
        usno_alpha_deg_orig(i) = usno_alpha_deg(i)
        usno_delta_orig(i)     = usno_delta(i)
        usno_mag_orig(i)       = usno_mag(i)
          usno_gnom(i,1) = (usno_alpha_deg(i)-alpha_center_frame_deg)
     *                      *3600.
     *                      /pix_scale
     *                      /cos(delta_center_frame_deg*pierez)
          usno_gnom(i,2) = (usno_delta(i)-delta_center_frame_deg) 
     *                      *3600.
     *                     /pix_scale
      end do
      do i = 1,n_usno
        participate(i) = .true.
      end do
      do i = 1,n_usno
        if(usno_mag(i).lt.mag_limit) participate(i) = .false.
      end do
      do i = 1,n_usno-1
        do j = i+1,n_usno
          if((usno_gnom(j,1)-usno_gnom(i,1)) 
     *      .gt. 2.*min_dist_usno_stars) goto 8
          d = (usno_gnom(i,1)-usno_gnom(j,1))**2+
     *        (usno_gnom(i,2)-usno_gnom(j,2))**2
          if(d.lt.min_dist_usno_stars_2) then
            participate(i) = .false.
            participate(j) = .false.
          end if
        end do
    8 continue
      end do
      close(2)

      open(2,file=usno_star_file_dummy,status='unknown')
      j = 0
      usno_alpha_min = 10000.d0
      usno_delta_min = 10000.d0
      usno_alpha_max = -1000.d0
      usno_delta_max = -10000.d0
      
      do i = 1,n_usno
        if(participate(i)) then
          j = j+1
          write(2,*) j,usno_alpha_deg(i),usno_delta(i),usno_mag(i)
          usno_alpha_min = dmin1(usno_alpha_min,usno_alpha_deg(i))
          usno_delta_min = dmin1(usno_delta_min,usno_delta(i))
          usno_alpha_max = dmax1(usno_alpha_max,usno_alpha_deg(i))
          usno_delta_max = dmax1(usno_delta_max,usno_delta(i))
        end if
      end do
      n_usno = j

c     find USNO stars in a small field near frame center
c     --------------------------------------------------
      field_size_deg =  center_field_size_minutes/60. 
 1110 continue
      usno_alpha_min = 10000.d0
      usno_delta_min = 10000.d0
      usno_alpha_max = -1000.d0
      usno_delta_max = -10000.d0
      rewind 2
      i = 0
      do j = 1,n_usno
        read(2,*) k,ualpha,udelta,umag

        if(dabs(ualpha-alpha_center_frame_deg).gt.field_size_deg) 
     *   goto 10
        if(dabs(udelta-delta_center_frame_deg).gt.field_size_deg) 
     *     goto 10
        i = i+1
        if(i.gt.max_usno_center) then
          write(nu_fail,*) 'WARNING: increase max_usno_center'
        i = max_usno_center
        end if        
        usno_alpha_deg(i) = ualpha
        usno_delta(i) = udelta 
        usno_mag(i) = umag
        running_number_usno(i) = k
          usno_alpha_min = dmin1(usno_alpha_min,usno_alpha_deg(i))
          usno_delta_min = dmin1(usno_delta_min,usno_delta(i))
          usno_alpha_max = dmax1(usno_alpha_max,usno_alpha_deg(i))
          usno_delta_max = dmax1(usno_delta_max,usno_delta(i))
   10 continue
      end do
      n_usno_center = i
      if(n_usno_center.gt.max_number_n_usno_center) then 
        field_size_deg = field_size_deg-field_size_deg/10.
        goto 1110
      end if
      close(2)
      if(n_usno_center.lt.min_number_n_usno_center) then
        iter_increase_field_size = iter_increase_field_size+1
        if(iter_increase_field_size.gt.10) then
          write(nu_fail,*) 'number of USNO stars on chip too small',
     *              n_usno_center
          icode = 3
          return
        end if
        field_size_deg = field_size_deg+field_size_deg/10.
        goto 1110
      end if
        
c     gnomonic projection of USNO stars
c     ---------------------------------
      alpha_deg = alpha_proj_center_deg
      delta_deg = delta_proj_center+pix_scale/3600.d0
        call equa_deg_to_xygnom(alpha_deg,delta_deg,
     *      alpha_proj_center_deg,delta_proj_center,
     *      x_gnom,y_gnom)
      focal_length_orig = 1./y_gnom
 6020 format(2f15.8)
      alpha_max = -1.d0
      delta_min = 100.d0
      focal_length = focal_length_orig
      do i = 1,n_usno_center
        alpha_deg = usno_alpha_deg(i)
        delta_deg = usno_delta(i)
        call equa_deg_to_xygnom(alpha_deg,delta_deg,
     *      alpha_proj_center_deg,delta_proj_center,
     *      x_gnom,y_gnom)
        usno_gnom(i,1) = x_gnom
        usno_gnom(i,2) = y_gnom
      end do

c     match astronomical coordinate system with chip coordinate system
c     ascending R.A. and ascending declination corresponds to which axes ?
c     -------------------------------------    
      do i = 1,n_usno_center
        usnox(i) = usno_gnom(i,1)*focal_length
        usnoy(i) = usno_gnom(i,2)*focal_length
      end do
      do i = 1,n_usno_center
        if(axis1_direction.eq.minus) usnox(i) = -usnox(i)
        if(axis2_direction.eq.minus) usnoy(i) = -usnoy(i)
      end do
      do i = 1,n_usno_center
        if(n_axis1_meaning.eq.2) then
          y_gnom = usnox(i)
          x_gnom = usnoy(i)
          usnox(i) = x_gnom
          usnoy(i) = y_gnom
        end if
      end do

      do i = 1,n_usno_center
        usnox(i) = usnox(i)+chip_center_shift_alpha_pix+naxis1/2      
        usnoy(i) = usnoy(i)-chip_center_shift_delta_pix+naxis2/2
      end do

      call sortreal8(n_usno_center,usnoy,indx_sort_usno)
      do k = 1,n_usno_center
        i = indx_sort_usno(k)
        usno_sort(k,1) = usnox(i)
        usno_sort(k,2) = usnoy(i)
        usno_delta_sort(k) = usno_delta(i)
        usno_alpha_deg_sort(k) = usno_alpha_deg(i)
        usno_mag_sort(k) = usno_mag(i)
        running_number_usno_sort(k) = running_number_usno(i)
      end do
      do i = 1,n_usno_center
        usnox(i) = usno_sort(i,1)
        usnoy(i) = usno_sort(i,2)
      end do

      do i = 1,n_usno_center-1
        do j = i+1,n_usno_center
          dist_usno(i,j) = dsqrt((usnox(i)-usnox(j))**2
     *           +(usnoy(i)-usnoy(j))**2)
          dist_usno(j,i) = dist_usno(i,j)
        end do
      end do

c     match USNO stars with bright frame stars near frame center
c     mapped triangles matching; compare distance ratios
c     ---------------------------------------------------             
      iter_triangle_match = 0
      do i = 1,n_br_center
        participate_br(i) = .true.
      end do
   50 continue
      do i = 1,max_usno_center
        match_proposed(i,1) = 0
        match_proposed(i,2) = 0
        do j = 1,max_bright_stars_center
          match_numbers(i,j) = 0
        end do
      end do
      do i = 1,n_br_center
        n_proposed_triangle_br(i) = 0
      end do

      do iu = 1,n_usno_center-2
        do ju = iu+1,n_usno_center-1
          do ku = ju+1,n_usno_center
            d12 = dist_usno(ju,iu)
            d13 = dist_usno(ku,iu)
            d23 = dist_usno(ku,ju)
            r12_13 = d12/d13
            r12_23 = d12/d23
            r13_23 = d13/d23

            do i = 1,n_br_center-2
              if(participate_br(i)) then
              do j = i+1,n_br_center-1
                if(participate_br(j)) then
                do k = j+1,n_br_center
                  if(participate_br(k)) then
                  b12_13 = dist_br_center(j,i)/dist_br_center(k,i)
                  if(dabs(b12_13-r12_13).lt.tol_triangle_match) then
                    b12_23 = dist_br_center(j,i)/dist_br_center(k,j)
                    if(dabs(b12_23-r12_23).lt.tol_triangle_match) then
                      b13_23 = dist_br_center(k,i)/dist_br_center(k,j)
                      if(dabs(b13_23-r13_23).lt.tol_triangle_match) then
                      x1 = usno_sort(iu,1)-xbrcat_center(i)
                      x2 = usno_sort(ju,1)-xbrcat_center(j)
                      x3 = usno_sort(ku,1)-xbrcat_center(k)
                      y1 = usno_sort(iu,2)-ybrcat_center(i)
                      y2 = usno_sort(ju,2)-ybrcat_center(j)
                      y3 = usno_sort(ku,2)-ybrcat_center(k)
                      xm = (x1+x2+x3)/3.
                      ym = (y1+y2+y3)/3.
                      cont = .true.
                      if(dabs(x1-xm).gt.3.*tol_offset) cont = .false.
                      if(dabs(x2-xm).gt.3.*tol_offset) cont = .false.
                      if(dabs(x3-xm).gt.3.*tol_offset) cont = .false.
                      if(dabs(y1-ym).gt.3.*tol_offset) cont = .false.
                      if(dabs(y2-ym).gt.3.*tol_offset) cont = .false.
                      if(dabs(y3-ym).gt.3.*tol_offset) cont = .false.
                      if(cont) then
                        match_numbers(iu,i) = match_numbers(iu,i)+1
                        match_numbers(ju,j) = match_numbers(ju,j)+1
                        match_numbers(ku,k) = match_numbers(ku,k)+1
                      end if
                      end if
                    end if
                  end if
                end if
                end do
              end if
              end do
            end if
            end do
          end do
        end do
      end do

      do i = 1,n_usno_center
        nzmax = 0
        jj = 0
        do j = 1,n_br_center
          if(match_numbers(i,j).ne.0) then
            if(max(nzmax,match_numbers(i,j)).gt.nzmax) then
              jj = j
              nzmax = match_numbers(i,j)
            end if
          end if
        end do
        if(jj.ne.0)then
          match_proposed(i,1) = jj
          match_proposed(i,2) = match_numbers(i,jj)
        end if
      end do
      nz = 0
      ii = 0
      do i = 1,n_usno_center
        if(match_proposed(i,1).gt.0) then
          nz = nz+1
          ii = ii+match_proposed(i,2)
        end if
      end do
      if(nz.eq.0) then
        icode = 4
        goto 300
      end if
      ii = 3
      number_tr_match = 0
      do i = 1,n_usno_center
        if(match_proposed(i,2).gt.ii) then
          j = match_proposed(i,1)
          n_proposed_triangle_br(j) = n_proposed_triangle_br(j)+1
          number_tr_match = number_tr_match+1
        else
          match_proposed(i,1) = 0
        end if
      end do
      j = 0
      do i = 1,n_br_center
        if(n_proposed_triangle_br(i).gt.1) then
          participate_br(i) = .false.
          j = 1
        end if
      end do
      if(j.eq.1 .or.number_tr_match.lt.5) then
        tol_triangle_match = tol_triangle_match+tol_triangle_match*0.2
        iter_triangle_match = iter_triangle_match+1
        write(nu_fail,*) 'iter_triangle_match ',iter_triangle_match
        write(nu_fail,*) 'new tol_triangle_match',tol_triangle_match
        if(iter_triangle_match.le.3) goto 50
c        write(nu_fail,*) 'no matching possible'
       icode = 6
       goto 300
      end if

c     test for bad triangle stars
c     ---------------------------
      do i = 1,n_usno_center
        usno_br_x(i) = 0.d0
        usno_br_y(i) = 0.d0
      end do
      nz = 0
      do i = 1,n_usno_center 
        if(match_proposed(i,1).gt.0) then
          nz = nz+1
          j = match_proposed(i,1)
          usno_br_x(nz) = usno_sort(i,1)-xbrcat_center(j)
          usno_br_y(nz) = usno_sort(i,2)-ybrcat_center(j)
        end if
      end do
      call sortreal8(nz,usno_br_x,indx_sort_usno)
      j = nz/2
      i = indx_sort_usno(j)
      x_off = usno_br_x(i)
      call sortreal8(nz,usno_br_y,indx_sort_usno)
      i = indx_sort_usno(j)
      y_off = usno_br_y(i)
      do i = 1,n_usno_center
        if(match_proposed(i,1).gt.0) then
          j = match_proposed(i,1)
          if(dabs(usno_sort(i,1)-xbrcat_center(j)-x_off).gt.tol_offset)
     *       match_proposed(i,1) = 0
          if(dabs(usno_sort(i,2)-ybrcat_center(j)-y_off).gt.tol_offset)
     *       match_proposed(i,1) = 0
        end if
      end do

c     find focal length; compare triangles of identified stars
c     ----------------------------------------------------------
      iter_focal_length = 0
   60 continue
      focal_length = 0.d0
      nz = 0
      do iu = 1,n_usno_center-2
        if(match_proposed(iu,1).gt.0) then
          do ju = iu+1,n_usno_center-1
            if(match_proposed(ju,1).gt.0) then
              do ku = ju+1,n_usno_center
                if(match_proposed(ku,1).gt.0) then
                  d12 = dist_usno(ju,iu)
                  d13 = dist_usno(ku,iu)
                  d23 = dist_usno(ku,ju)
                  i = match_proposed(iu,1)
                  j = match_proposed(ju,1)
                  k = match_proposed(ku,1)
                  b12 = dsqrt((xbrcat_center(i)-xbrcat_center(j))**2+
     *                (ybrcat_center(i)-ybrcat_center(j))**2)
                  b13 = dsqrt((xbrcat_center(i)-xbrcat_center(k))**2+
     *                (ybrcat_center(i)-ybrcat_center(k))**2)
                  b23 = dsqrt((xbrcat_center(k)-xbrcat_center(j))**2+
     *                (ybrcat_center(k)-ybrcat_center(j))**2)
                  r12 = b12/d12
                  r13 = b13/d13
                  r23 = b23/d23
                  focal_length = focal_length+r12+r13+r23
                  nz = nz+3
                end if
              end do
            end if
          end do
        end if
      end do
      if(nz.eq.0) then
c       write(nu_fail,*) 'no stars found for focal length determination'
        icode = 7
        goto 300
      end if
      focal_length = focal_length/dble(nz)
      tol_focal_length = focal_length/10.d0


c     test triangles for deviation from mean value
c      -------------------------------------------
      do iu = 1,n_usno_center-2
        if(match_proposed(iu,1).gt.0) then
          do ju = iu+1,n_usno_center-1
            if(match_proposed(ju,1).gt.0) then
              do ku = ju+1,n_usno_center
                if(match_proposed(ku,1).gt.0) then
                  d12 = dist_usno(ju,iu)
                  d13 = dist_usno(ku,iu)
                  d23 = dist_usno(ku,ju)
                  i = match_proposed(iu,1)
                  j = match_proposed(ju,1)
                  k = match_proposed(ku,1)
                  b12 = dsqrt((xbrcat_center(i)-xbrcat_center(j))**2+
     *                        (ybrcat_center(i)-ybrcat_center(j))**2)
                  b13 = dsqrt((xbrcat_center(i)-xbrcat_center(k))**2+
     *                        (ybrcat_center(i)-ybrcat_center(k))**2)
                  b23 = dsqrt((xbrcat_center(k)-xbrcat_center(j))**2+
     *                        (ybrcat_center(k)-ybrcat_center(j))**2)
                  r12 = b12/d12
                  r13 = b13/d13
                  r23 = b23/d23
                  if(dabs(focal_length-r12).gt.tol_focal_length
     *           .or.dabs(focal_length-r13).gt.tol_focal_length
     *           .or.dabs(focal_length-r23).gt.tol_focal_length) 
     *                                                then
                   match_proposed(iu,1) = 0
                   match_proposed(ju,1) = 0 
                   match_proposed(ku,1) = 0 
                   iter_focal_length = iter_focal_length+1
                   write(nu_fail,*) 'iter_focal_length',
     *                iter_focal_length
                   if(iter_focal_length.le.3) then 
                     goto 60
                   else
c                    write(nu_fail,*) 'problem to find focal length'
                     icode = 8
                     goto 300
                   end if
                  end if                                    
                end if
              end do
            end if
          end do
        end if
      end do
      focal_length = focal_length_orig*focal_length
      goto 305

  300 continue
      tol_offset = tol_offset-5.
      iter_offset = iter_offset+1
      write(nu_fail,*) 'iter_offset',iter_offset
      if(icode.eq.7 .or. icode.eq.8) write(nu_fail,*)
     *            'problem with focal length determination'
      if(icode.lt.7) write(nu_fail,*)
     *           'triangle matching problem, not enough USNO stars'
      write(nu_fail,*) 'try new tol_offset for triangle match',
     *                  tol_offset
      if(iter_offset.lt.3) goto 290
      icode = 11
      return

  305 continue
c     find offset between USNO and bright stars
c     -----------------------------------------
      do i = 1,n_usno_center
        usnox(i) = usno_gnom(i,1)*focal_length
        usnoy(i) = usno_gnom(i,2)*focal_length
      end do
      do i = 1,n_usno_center
        if(axis1_direction.eq.minus) usnox(i) = -usnox(i)
        if(axis2_direction.eq.minus) usnoy(i) = -usnoy(i)
      end do
      do i = 1,n_usno_center
        if(n_axis1_meaning.eq.2) then
          y_gnom = usnox(i)
          x_gnom = usnoy(i)
          usnox(i) = x_gnom
          usnoy(i) = y_gnom
        end if
      end do
      do i = 1,n_usno_center
        usnox(i) = usnox(i)+chip_center_shift_alpha_pix+naxis1/2
        usnoy(i) = usnoy(i)-chip_center_shift_delta_pix+naxis2/2
      end do
      call sortreal8(n_usno_center,usnoy,indx_sort_usno)
      do k = 1,n_usno_center
        i = indx_sort_usno(k)
        usno_sort(k,1) = usnox(i)
        usno_sort(k,2) = usnoy(i)
        usno_delta_sort(k) = usno_delta(i)
        usno_alpha_deg_sort(k) = usno_alpha_deg(i)
        usno_mag_sort(k) = usno_mag(i)
        running_number_usno_sort(k) = running_number_usno(i)
      end do

   70 continue
      x_off = 0.d0
      y_off = 0.d0
      x_off_max = 0.d0
      y_off_max = 0.d0
      nz = 0
      do iu = 1,n_usno_center
        if(match_proposed(iu,1).gt.0) then
          i = match_proposed(iu,1)
          x_off = x_off+(xbrcat_center(i)-usno_sort(iu,1))
          y_off = y_off+(ybrcat_center(i)-usno_sort(iu,2))
          x_off_max = 
     *        dmax1(x_off_max,dabs(xbrcat_center(i)-usno_sort(iu,1)))
          y_off_max = 
     *        dmax1(y_off_max,dabs(ybrcat_center(i)-usno_sort(iu,2)))
          nz = nz+1
        end if
      end do
      x_off = x_off/dble(nz)
      y_off = y_off/dble(nz)
      x_off_max = dabs(x_off_max-dabs(x_off))
      y_off_max = dabs(y_off_max-dabs(y_off))
c     write(6,*) nz,x_off,x_off_max,chip_center_shift_alpha_pix
c     write(6,*) nz,y_off,y_off_max,chip_center_shift_delta_pix
c     write(6,*) alpha_proj_center_deg,delta_proj_center
c     write(6,*) alpha_center_frame_deg,delta_center_frame_deg

      if(dabs(x_off).lt.tol_chip_center_shift .and.
     *   dabs(y_off).lt.tol_chip_center_shift) goto 84
      iter_chip_center_shift = iter_chip_center_shift+1
      write(nu_fail,*) 'iter_chip_center_shift',iter_chip_center_shift
      if(iter_chip_center_shift.gt.5) then
        write(nu_fail,*) 'chip center iteration failed'
        icode = 55
        return
      end if
      write(nu_fail,*) 'try new chip_center_shift_alpha_arcmin'
      chip_center_shift_alpha_arcmin = chip_center_shift_alpha_arcmin
     *         +x_off*pix_scale/60.d0
      chip_center_shift_delta_m = chip_center_shift_delta_m
     *                            -y_off*pix_scale/60.d0
      write(nu_fail,*) 'chip_center_shift_alpha_arcmin',
     *                  chip_center_shift_alpha_arcmin
      write(nu_fail,*)'chip_center_shift_delta_m',
     *                chip_center_shift_delta_m
      goto 400

   84 continue
      x_off = chip_center_shift_alpha_pix+naxis1/2 
      y_off = -chip_center_shift_delta_pix+naxis2/2

c     find USNO stars allover the frame
c     --------------------------------------------------
      open(2,file=usno_star_file_dummy,status='unknown')          
      i = 0
   85 read(2,*,end=86) k,ualpha,udelta,umag

      if(dabs(ualpha-alpha_center_frame_deg).gt.
     *                   frame_size_alpha_deg) goto 90
      if(dabs(udelta-delta_center_frame_deg).gt.
     *                    frame_size_delta) goto 90
      i = i+1        
      usno_alpha_deg(i) = ualpha
      usno_delta(i) = udelta 
      usno_mag(i) = umag
      running_number_usno(i) = k
   90 continue
      goto 85
   86 continue
      close(2)
      n_usno = i-1
      if(n_usno.lt.6) then
        write(nu_fail,*) 'number of USNO stars on chip too small',n_usno
        icode = 9
        return
      end if  
c     gnomonic projection of all USNO stars
c     -------------------------------------
      nz = 0
      alpha_max = -1.d0
      delta_min = 100.d0
      do i = 1,n_usno
        alpha_deg = usno_alpha_deg(i)
        delta_deg = usno_delta(i)
        call equa_deg_to_xygnom(alpha_deg,delta_deg,
     *      alpha_proj_center_deg,delta_proj_center,
     *      x_gnom,y_gnom)
        usno_gnom(i,1) = x_gnom
        usno_gnom(i,2) = y_gnom
      end do

c     match astronomical coordinate system with chip coordinate system
c     ascending R.A. and ascending declination corresponds to which axes ?
c     -------------------------------------    
      do i = 1,n_usno
        usnox(i) = usno_gnom(i,1)*focal_length
        usnoy(i) = usno_gnom(i,2)*focal_length
      end do
      do i = 1,n_usno
        if(axis1_direction.eq.minus) usnox(i) = -usnox(i)
        if(axis2_direction.eq.minus) usnoy(i) = -usnoy(i)
      end do
      do i = 1,n_usno
        if(n_axis1_meaning.eq.2) then
          y_gnom = usnox(i)
          x_gnom = usnoy(i)
          usnox(i) = x_gnom
          usnoy(i) = y_gnom
        end if
      end do
      do i = 1,n_usno
        usnox(i) = usnox(i)+x_off
        usnoy(i) = usnoy(i)+y_off
      end do

      call sortreal8(n_usno,usnoy,indx_sort_usno)
      do k = 1,n_usno
        i = indx_sort_usno(k)
        usno_sort(k,1) = usnox(i)
        usno_sort(k,2) = usnoy(i)
        usno_delta_sort(k) = usno_delta(i)
        usno_alpha_deg_sort(k) = usno_alpha_deg(i)
        usno_mag_sort(k) = usno_mag(i)
        running_number_usno_sort(k) = running_number_usno(i)
      end do

      do i = 1,n_usno
        usno_gnom(i,1) = usno_sort(i,1)
        usno_gnom(i,2) = usno_sort(i,2)
      end do

      nz = 0
      open(8,file=filename_matching_list,status='unknown')
      do i = 1,max_usno
        participate(i) = .false.
      end do
      do i = 1,max_bright_stars
        participate_br(i) = .false.
      end do

      do i = 1,n_br
        ident_br(i) = 0
        br_usno_ident(i) = 0
        running_number_br(i) = i
      end do
      tol_offset_2 = tol_offset*tol_offset
      do i = 1,n_br
        do iu = 1,n_usno
          if(dabs(usno_gnom(iu,2)-ybrcat(i)).lt.tol_offset) then
            d = (usno_gnom(iu,1)-xbrcat(i))**2 +
     *          (usno_gnom(iu,2)-ybrcat(i))**2
            if(d.lt.tol_offset_2) then
              ident_br(i) = ident_br(i)+1
              br_usno_ident(i) = iu
            end if
          end if
        end do
      end do

      nz = 0
      do i = 1,n_br
        if(ident_br(i).eq.1) then
              iu = br_usno_ident(i)
              ualpha = usno_alpha_deg_sort(iu)/15.d0
              iah = ualpha
              iam = int((ualpha-iah)*60.)
              as = (ualpha-iah-dble(iam)/60.)*3600.
              udelta = usno_delta_sort(iu)
              sign_delta = blank
              if(udelta.lt.0.d0) sign_delta = minus
              udelta = dabs(udelta)
              idg = udelta
              idm = (udelta-idg)*60.
              ds = (udelta-idg-dble(idm)/60.)*3600.
              nz = nz+1
              participate(iu) = .true.
              participate_br(i) = .true.
              write(8,3000) xbrcat(i),ybrcat(i),
     *         usno_alpha_deg_sort(iu),usno_delta_sort(iu)
 3000 format(2f10.2,2f15.10)
c             write(8,2000) running_number_br(i),
c    *         xbrcat(i),ybrcat(i),br_intensity(i),
c    *         running_number_usno_sort(iu),
c    *         usno_mag_sort(iu),iah,iam,as,sign_delta,idg,idm,ds,
c    *         usno_alpha_deg_sort(iu),usno_delta_sort(iu)
c2000 format (i5,2f8.2,i6,i5,f7.1,2x,2i3,f6.2,
c    *        3x,a1,i2,i3,f5.1,2x,2f15.10)
        end if
      end do 
      close(8)

c     compute pixel coordinates for all USNO stars on frame
c     -----------------------------------------------------
      open(8,file=filename_usno_coord_mag)
      do i = 1,n_usno_orig
        if(usno_mag_orig(i).ge.10. .and. 
     *       usno_mag_orig(i).le.22) then
          alpha_deg = usno_alpha_deg_orig(i)
          delta_deg = usno_delta_orig(i)
          call equa_deg_to_xygnom(alpha_deg,delta_deg,
     *      alpha_proj_center_deg,delta_proj_center,
     *      x_gnom,y_gnom)
          usno_gnom(i,1) = x_gnom
          usno_gnom(i,2) = y_gnom
          usnox(i) = usno_gnom(i,1)*focal_length
          usnoy(i) = usno_gnom(i,2)*focal_length
          if(axis1_direction.eq.minus) usnox(i) = -usnox(i)
          if(axis2_direction.eq.minus) usnoy(i) = -usnoy(i)
          if(n_axis1_meaning.eq.2) then
            y_gnom = usnox(i)
            x_gnom = usnoy(i)
            usnox(i) = x_gnom
            usnoy(i) = y_gnom
          end if
          usnox(i) = usnox(i)+x_off
          usnoy(i) = usnoy(i)+y_off
          if(usnox(i).gt.10. .and.usnox(i).lt.dble(naxis1-10.).and.
     *       usnoy(i).gt.10. .and.usnox(i).lt.dble(naxis2)-10.)
     *    write(8,8080) usnox(i),usnoy(i),usno_mag_orig(i),usno_n(i),
     *                  usno_alpha_deg_orig(i),usno_delta_orig(i)
 8080 format(2f10.1,f6.1,i11,2f15.7)
        end if
      end do
      close(8)

  210 close(1)
      return
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
c
c
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

      subroutine sortinteger(n,arrin,indx)
      implicit integer(a-z)
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

      subroutine provide_bright_stars
     *        (filename,min_dist_br_stars,
     *         br_intensity,br_intensity_center,
     *   xbrcat,ybrcat,xbrcat_center,ybrcat_center,
     *   dist_br_center,
     *         n_br,n_br_center,running_number_br,
     *         naxis1,naxis2)

      implicit NONE
      integer max_bright_stars,max_bright_stars_center
      integer max_usno,max_usno_center

      parameter(
     *          max_usno=5000,
     *          max_usno_center=500,
     *          max_bright_stars=500,
     *          max_bright_stars_center=500)

      real*8 xbrcat(max_bright_stars)
      real*8 ybrcat(max_bright_stars)
      real*8 xbrcat_prelim(max_bright_stars)
      real*8 ybrcat_prelim(max_bright_stars)
      real*8 x,y,min_dist_br_stars
      real*8 min_dist_br_stars_2,dist,xx,yy
      real*8 xbrcat_center(max_bright_stars)
      real*8 ybrcat_center(max_bright_stars)
      real*8 xbrcat_center_prelim(max_bright_stars)
      real*8 ybrcat_center_prelim(max_bright_stars)
      real*8 xcenter,ycenter,xnax
      real*8 xbrmin,xbrmax,ybrmin,ybrmax
      real*8 dist_br_center
     *  (max_bright_stars_center,max_bright_stars_center)

      integer i,j,k,limit,nz
      integer n_br,n_br_center,n_br_center_most
      integer hist(6000)
      integer br_intensity(max_bright_stars)
      integer br_intensity_prelim(max_bright_stars)
      integer br_intensity_center(max_bright_stars_center)
      integer br_intensity_center_prelim(max_bright_stars_center)
      integer naxis1,naxis2,nax
      integer indx_sort_br(max_bright_stars)
      integer running_number_br(max_bright_stars)

      character*100 filename

      logical participate(max_bright_stars)


      n_br_center_most = 100

      open(1,file=filename,status='old')      

      do i = 1,max_bright_stars
        participate(i) = .true.
      end do

      read(1,*)
      read(1,*) 

      nz = 0
   25 read(1,*,end=30) x,y
        nz = nz+1
        if(nz.gt.max_bright_stars) then
          nz = max_bright_stars
          goto 30
      end if
        xbrcat_prelim(nz) = x
        ybrcat_prelim(nz) = y
        br_intensity_prelim(nz) = 10000
      goto 25
   30 continue
      close(1)

c     reject close stars
c     ------------------
      min_dist_br_stars_2 = min_dist_br_stars**2
      do i = 1,nz-1
        do j = i+1,nz
          dist = (xbrcat_prelim(i)-xbrcat_prelim(j))**2 +
     *           (ybrcat_prelim(i)-ybrcat_prelim(j))**2
          if(dist.lt.min_dist_br_stars_2) then
            participate(i) = .false.
            participate(j) = .false.
          end if
        end do
      end do
      j = 0
      do i = 1,nz
        if(participate(i))then
          j = j+1
          xbrcat(j) = xbrcat_prelim(i)
          ybrcat(j) = ybrcat_prelim(i)
          br_intensity(j) = br_intensity_prelim(i)
        end if
      end do
      n_br = j

c     find bright stars near image center
c     -----------------------------------
      do i = 1,max_bright_stars
        participate(i) = .false.
      end do
      xcenter = dble(naxis1/2)
      ycenter = dble(naxis2/2)
      nax = min(naxis1,naxis2)
      xnax = dble(nax)

      xbrmin = xcenter - 0.4*xnax
      xbrmax = xcenter + 0.4*xnax
      ybrmin = ycenter - 0.4*xnax
      ybrmax = ycenter + 0.4*xnax

      nz = 0     
      do i = 1,n_br
        if(xbrcat(i).gt.xbrmin .and.
     *     xbrcat(i).lt.xbrmax .and.
     *     ybrcat(i).gt.ybrmin .and.
     *     ybrcat(i).lt.ybrmax) then
          nz = nz+1
          xbrcat_center(nz) = xbrcat(i)
          ybrcat_center(nz) = ybrcat(i)
          br_intensity_center(nz) = br_intensity(i)
          running_number_br(nz) = nz
        end if
      end do
      n_br_center = nz
c     take the n_br_center_most brightest stars in case there are more
c     ---------------------------------------------------
      if(n_br_center.gt.n_br_center_most) then
        do i = 1,n_br_center
          br_intensity_center_prelim(i) = -br_intensity_center(i)
          xbrcat_center_prelim(i) = xbrcat(i)
          ybrcat_center_prelim(i) = ybrcat(i)
        end do
        call sortinteger(n_br_center,br_intensity_center_prelim,
     *                   indx_sort_br)
        do k = 1,n_br_center
          i = indx_sort_br(k)
          if(i.le.n_br_center_most) then
            participate(k) = .true.
          else
            participate(k) = .false.
          end if
        end do
        nz = 0
        do i = 1,n_br_center
          if(participate(i)) then
            xbrcat_center(i) = xbrcat_center_prelim(i)
            ybrcat_center(i) = ybrcat_center_prelim(i)
            br_intensity_center(i) = - br_intensity_center_prelim(i)
            running_number_br(i) = i
            nz = nz+1
          end if
        end do
        n_br_center = nz
      end if
 
c     compute distances among bright stars near center
c     ------------------------------------------------
      do i = 1,n_br_center-1
        do j = i+1,n_br_center
          dist_br_center(i,j) = 
     *            dsqrt((xbrcat_center(i)-xbrcat_center(j))**2+
     *           (ybrcat_center(i)-ybrcat_center(j))**2)
         dist_br_center(j,i) = dist_br_center(i,j)
        end do
      end do 

 1000 format(i5,2f11.2,i8)
      return
      end

c     ------------------------------------------
c1111
      subroutine plate_solution(n_plate,
     *             filename_plate_solution,
     *             filename_matching_list,
     *                   axis1_meaning,axis1_direction,
     *                   axis2_meaning,axis2_direction,
     *              n_axis1_meaning,n_axis1_direction,
     *              n_axis2_direction,
     *             nu_fail,
     *              focal_length,x_off,y_off,
     *              plc,
     *             alpha_proj_center_deg,delta_proj_center,
     *             naxis1,naxis2,icode)

      implicit NONE
      integer max_bright_stars,max_bright_stars_center
      integer max_usno,max_usno_center


      parameter(
     *          max_usno=5000,
     *          max_usno_center=500,
     *          max_bright_stars=500,
     *          max_bright_stars_center=500)


      integer i,j,nz,l,icode
      integer naxis1,naxis2,number_frames
      integer year,month
      integer n_axis1_meaning,n_axis2_meaning
      integer br_intensity(max_bright_stars)
      integer iah,iam,idd,idm
      integer number_stars,nn_max,ierr
      integer idh
      integer n_axis1_direction,n_axis2_direction
      integer n_plate
      integer nu_fail

      real*8 frame_size_alpha_arcminutes,
     *             frame_size_delta_minutes,
     *             focal_length,
     *             x_off,y_off,
     *             alpha_proj_center_deg,delta_proj_center
      real*8 day
      real*8 pie,pierez,epsilon,epsilon_2
      real*8 asec,dsec
      real*8 aa,bb,al,del,h,hdm,hds,rms,da,dd
      real*8 x_gnom,y_gnom,cand_x,cand_y
      real*8 ttan,alpha_cand_h,xiam,xias
      real*8 eta,zeta,alpha_cand,delta_cand,alphaz_deg,deltaz_deg
      real*8 chip_coord(max_bright_stars,2)
      real*8 magnitude(max_bright_stars)
      real*8 alpha_deg(max_bright_stars),delta(max_bright_stars)
      real*8 star_coord_gnom_only(max_bright_stars,2)
      real*8 trans_coord(max_bright_stars,2)
      real*8 ccand_x,ccand_y
      real*8 x_ast,y_ast
      real*8 xx(2*max_bright_stars),xx1(2*max_bright_stars),
     *       t(2*max_bright_stars,30),t1(2*max_bright_stars,30)
      real*8 x(max_bright_stars),y(max_bright_stars)
      real*8 w(30),plc(30),rv(30),test(2*max_bright_stars),pplc(30,1)
      real*8 kn
      real*8 tand,sind,cosd
      external tand,sind,cosd


      character*1 minus,axis1_meaning,axis2_meaning,sign,
     *             axis1_direction,axis2_direction


      character*100 filename_matching_list
      character*100 filename_plate_solution


      icode = 0
      pie = 3.141592653589793D0
      pierez = pie/180.d0
      minus = '-'

c     read coordinates of matched stars
c     --------------------------------------
      open(1,file=filename_matching_list,status='old')
      nz = 1
    5 read(1,*,end=10) chip_coord(nz,1),chip_coord(nz,2),
     *                 alpha_deg(nz),delta(nz)
c   5 read(1,*,end=10) i,chip_coord(nz,1),chip_coord(nz,2),
c    *                 br_intensity(nz),j,magnitude(nz),
c    *                 iah,iam,asec,idd,idm,dsec,
c    *                 alpha_deg(nz),delta(nz)
      nz = nz+1
      if(nz.gt.max_bright_stars) then
        write(nu_fail,*) 
     *   'WARNING:increase max_bright_stars in subr plate_solution'
        goto 10
      end if
      goto 5
   10 number_stars = nz-1
      close(1)

c     gnomonic projection of USNO coordinates with respect to
c     direction of camera center --  telescope direction given in FITS header
c     ----------------------------------------------------------------------
      do i = 1,number_stars
        al = alpha_deg(i)
        del = delta(i)
        call equa_deg_to_xygnom(al,del,
     *      alpha_proj_center_deg,delta_proj_center,
     *      x_gnom,y_gnom)
        star_coord_gnom_only(i,1) = x_gnom
        star_coord_gnom_only(i,2) = y_gnom
      end do


c     transformation of matched star x,y coordinates:
c           chip_coordinate_system ----> gnomonic system
c     --------------------------------------------------
      do i = 1,number_stars
        trans_coord(i,1) = chip_coord(i,1)-x_off
        trans_coord(i,2) = chip_coord(i,2)-y_off
      end do
      if(n_axis1_meaning.eq.2) then
        do i = 1,number_stars
          y_gnom = trans_coord(i,1)
          x_gnom = trans_coord(i,2)
          trans_coord(i,1) = x_gnom
          trans_coord(i,2) = y_gnom
        end do
      end if
      do i = 1,number_stars
        if(axis1_direction.eq.minus) 
     *            trans_coord(i,1) = -trans_coord(i,1)
        if(axis2_direction.eq.minus) 
     *            trans_coord(i,2) = -trans_coord(i,2)
      end do
      do i = 1,number_stars
        trans_coord(i,1) = trans_coord(i,1)/focal_length
        trans_coord(i,2) = trans_coord(i,2)/focal_length
      end do

c     fourth order reduction
c     ---------------------

c     initialisation of plate constant matrix
c     ---------------------------------------
      do i=1,2*number_stars
        do j=1,30
          t(i,j) = 0.d0
        end do
      end do

      epsilon_2 = 0.d0                                                
      epsilon =0.d0                                                   
                                                                       
c     construct plate constant matrix                               
c     -------------------------------
      do i = 1,number_stars
        t(i,1) =1     
        t(i,2) = trans_coord(i,1)
        t(i,3) = trans_coord(i,2)
        xx(i) = star_coord_gnom_only(i,1)
        xx(i+number_stars) = star_coord_gnom_only(i,2)
        t(i,4) =(t(i,2)**2)
        t(i,5) =(t(i,3)**2)
        t(i,6) =(t(i,2)*t(i,3))
        t(i,7) =(t(i,2)**3)
        t(i,8) =(t(i,3)**3)
        t(i,9) =(t(i,4)*t(i,3))
        t(i,10)=(t(i,2)*t(i,5))
        t(i,11) =(t(i,2)**4)
        t(i,12) = (t(i,3)**4)
        t(i,13) = (t(i,2)*t(i,8))
        t(i,14) = (t(i,4)*t(i,5))
        t(i,15) = (t(i,7)*t(i,3))
        do l=1,15
          t(i+number_stars,l+15) = t(i,l)
        end do
      end do

      do i=1,2*number_stars
        xx1(i) = xx(i)                                                
        do j=1,30                                                     
          t1(i,j) = t(i,j)                                            
        end do                                                        
      end do                                                          
      nn_max = max_bright_stars

c     solve for plate constants
c     -------------------------                      
      call minfitpack (2*nn_max,2*number_stars,30,t1,w,1,xx1,ierr,rv,     
     *  pplc,30,kn)                                  
      if(ierr.ne.0) then
      write(nu_fail,*) 'ierr = ',ierr
      write(nu_fail,*) 'NO ASTROMETRY POSSIBLE'
      icode = 41
      return
      end if

      do i = 1,30
	plc(i) = pplc(i,1)
      end do
                                                                        
c     residuals
c     ----------                               
      call dproduit (t,pplc,test,2*nn_max,30,1,2*number_stars,30,1)        
                                                                        
      do i=1,2*number_stars                                              
        epsilon_2 = epsilon_2 + ((xx(i)-test(i))**2)              
      end do                                                          
      epsilon_2 = epsilon_2/dfloat(2*number_stars-30)                
      epsilon = dsqrt(epsilon_2)                                    
      if (epsilon.gt.(3.d0/180.d0/3600.d0*pie)) then
        write(nu_fail,*) ' '                   
        write(nu_fail,*) 'rms too large'
        write(nu_fail,*) 'NO ASTROMETRY POSSIBLE'
        write(nu_fail,*) ' '   
        icode = 42
        return
      end if           

c     write(6,*) 'n_plate,number_stars',n_plate,number_stars

      n_axis1_direction = 1
      if(axis1_direction.eq.minus) n_axis1_direction = -1
      n_axis2_direction = 1
      if(axis2_direction.eq.minus) n_axis2_direction = -1

c    store plate solution
c    --------------------
      if(n_plate.gt.0) then
      open(4,file=filename_plate_solution,status='unknown')
      write(4,4000) n_axis1_meaning,n_axis1_direction,
     *              n_axis2_direction
 4000 format(3i3)
      write(4,4001) focal_length,x_off,y_off
 4001 format(3d25.15)
      write(4,4002) alpha_proj_center_deg,delta_proj_center
 4002 format(2d25.15)
      write(4,4003) (plc(i),i=1,5)
      write(4,4003) (plc(i),i=6,10)
      write(4,4003) (plc(i),i=11,15)
      write(4,4003) (plc(i),i=16,20)
      write(4,4003) (plc(i),i=21,25)
      write(4,4003) (plc(i),i=26,30)
 4003 format(5d25.15)
      close(4)
      end if
                                                         
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

      subroutine cfh_12k(chip_number,
     *                   nu_fail,
     *                   frame_size_alpha_arcminutes,
     *                   frame_size_delta_minutes,
     *                   axis1_meaning,axis1_direction,
     *                   axis2_meaning,axis2_direction,
     *                   chip_center_shift_alpha_arcmin,
     *                   chip_center_shift_delta_m,icode)
      IMPLICIT NONE
      integer chip_number,icode,nu_fail

      real*8 chip_center_shift_alpha_arcmin
      real*8 chip_center_shift_delta_m
      real*8 frame_size_alpha_arcminutes
      real*8 frame_size_delta_minutes
      real*8 cp

      character*1 axis1_meaning,axis1_direction
      character*1 axis2_meaning,axis2_direction

      icode = 0

c     CFH 12 chips camera
c     -------------------
      if(chip_number.lt.0 .or. chip_number.gt.11) then
        write(nu_fail,*) 'FATAL ERROR: Chip number is wrong',chip_number
        icode = 51
        return
      end if

      frame_size_alpha_arcminutes = 7.d0
      frame_size_delta_minutes = 14.d0
      axis1_meaning = 'A'
      axis1_direction = '-'
      axis2_meaning = 'D'
      axis2_direction = '+'

c     CHIP offset
c     meaning: chip_center - telescope direction 

      if(chip_number.gt.-1 .and. chip_number.lt.6) then
        chip_center_shift_delta_m  = 0.117*60.d0
      else
        chip_center_shift_delta_m  = -0.117*60.d0
      end if

      if(chip_number.eq.0)  cp = -0.3138d0
      if(chip_number.eq.1)  cp = -0.1883d0
      if(chip_number.eq.2)  cp = -0.0628d0
      if(chip_number.eq.3)  cp = +0.0628d0
      if(chip_number.eq.4)  cp = +0.1883d0
      if(chip_number.eq.5)  cp = +0.3138d0

      if(chip_number.eq.6)  cp = -0.3138d0
      if(chip_number.eq.7)  cp = -0.1883d0
      if(chip_number.eq.8)  cp = -0.0628d0
      if(chip_number.eq.9)  cp = +0.0628d0
      if(chip_number.eq.10) cp = +0.1883d0
      if(chip_number.eq.11) cp = +0.3138d0

      chip_center_shift_alpha_arcmin = cp*60.d0
      return
      end

      subroutine kittpeak_camera(chip_number,
     *                   nu_fail,
     *                   frame_size_alpha_arcminutes,
     *                   frame_size_delta_minutes,
     *                   axis1_meaning,axis1_direction,
     *                   axis2_meaning,axis2_direction,
     *                   chip_center_shift_alpha_arcmin,
     *                   chip_center_shift_delta_m,icode)
      IMPLICIT NONE
      integer chip_number,icode,nu_fail

      real*8 chip_center_shift_alpha_arcmin
      real*8 chip_center_shift_delta_m
      real*8 frame_size_alpha_arcminutes
      real*8 frame_size_delta_minutes
      real*8 cp

      character*1 axis1_meaning,axis1_direction
      character*1 axis2_meaning,axis2_direction

      icode = 0

c     Kitt Peak camera 
c     ----------------
      if(chip_number.lt.1 .or. chip_number.gt.8) then
        write(nu_fail,*) 'FATAL ERROR: Chip number is wrong',chip_number
        icode = 51
        return
      end if

c     frame_size_alpha_arcminutes = 20.d0
      frame_size_alpha_arcminutes = 25.d0
c     frame_size_delta_minutes = 11.d0
      frame_size_delta_minutes = 15.d0
      axis1_meaning = 'A'
      axis1_direction = '-'
      axis2_meaning = 'D'
      axis2_direction = '+'

c     CHIP offset
c     meaning: chip_center - telescope direction

      if(chip_number.eq.1)  then
        chip_center_shift_alpha_arcmin = 9.d0
        chip_center_shift_delta_m =      13.5d0
      end if
      if(chip_number.eq.2)  then
        chip_center_shift_alpha_arcmin = 9.d0
        chip_center_shift_delta_m =      4.5d0
      end if
      if(chip_number.eq.3)  then
        chip_center_shift_alpha_arcmin = 9.d0
        chip_center_shift_delta_m =     -4.5d0
      end if
      if(chip_number.eq.4)  then
        chip_center_shift_alpha_arcmin = 9.d0
        chip_center_shift_delta_m =     -13.5
      end if
      if(chip_number.eq.5)  then
        chip_center_shift_alpha_arcmin = -9.d0
        chip_center_shift_delta_m =       13.5d0
      end if
      if(chip_number.eq.6)  then
        chip_center_shift_alpha_arcmin = -9.d0
        chip_center_shift_delta_m =       4.5d0
      end if
      if(chip_number.eq.7)  then
        chip_center_shift_alpha_arcmin = -9.d0
        chip_center_shift_delta_m =      -4.5d0
      end if
      if(chip_number.eq.8)  then
        chip_center_shift_alpha_arcmin = -9.d0
        chip_center_shift_delta_m =      -13.5d0
      end if

      return
      end
 
c
c
      subroutine find_maximum_usno_stars(
     *              filename_object_list,
     *              filename_usno_coord_mag,
     *              filename_usno_not_identified,
     *              filename_matching_list,
     *              usno_star_file,
     *              n_find_max_stars,
     *              n_axis1_meaning,n_axis1_direction,
     *              n_axis2_direction,
     *             nu_fail,
     *             nu_warn,
     *             filename_warn,
     *              tol_dis_usno_objstars_arcsec,
     *              focal_length,x_off,y_off,
     *              plc,
     *             alpha_proj_center_deg,delta_proj_center,
     *             seeing_pix_units,
     *             naxis1,naxis2,icode)

      implicit NONE

      integer number_obj_max,max_usno
      parameter(number_obj_max=10000)
      parameter(max_usno=5000)

      integer n_axis1_meaning,n_axis1_direction
      integer n_axis2_direction
      integer naxis1,naxis2,icode
      integer i,j,k,l
      integer number_obj,nz
      integer indx_sort(number_obj_max)
      integer n_usno_orig
      integer number_usno(max_usno)
      integer problem_identif(max_usno)
      integer n_usno_identif
      integer n_usno_not_identif
      integer nu_fail,nu_warn
      integer n_find_max_stars

      real*8 focal_length,x_off,y_off
      real*8 alpha_proj_center_deg,delta_proj_center
      real*8 plc(30)
      real*8 tol_dis_usno_objstars_arcsec
      real*8 usnox(max_usno),usnoy(max_usno)
      real*8 usno_alpha_not_identif(max_usno)
      real*8 usno_delta_not_identif(max_usno)
      real*8 alpha_deg,delta_deg,x_gnom,y_gnom
      real*8 obj_coord(number_obj_max,2)
      real*8 obj_coord_1(number_obj_max,2)
      real*8 obj_alpha(number_obj_max),obj_delta(number_obj_max)
      real*8 cand_x,cand_y
      real*8 alpha_cand,delta_cand
      real*8 usno_alpha(max_usno),usno_delta(max_usno)
      real*8 mag(max_usno),mag_not_identif(max_usno)
      real*8 cdcd,dist_2,tol_dis_arcsec_2,dist_2_f
      real*8 ddd
      real*8 x,y,xx,yy,eff,seeing_pix_units,x_buffer,y_buffer
      real*8 alpha_low,delta_low
      real*8 alpha_high,delta_high
      real*8 cosd
      external cosd

      character*100 filename_object_list
      character*100 filename_usno_coord_mag
      character*100 filename_usno_not_identified
      character*100 filename_matching_list
      character*100 usno_star_file
      character*100 filename_warn


      icode = 0

      x_buffer = 3.*seeing_pix_units
      y_buffer = x_buffer


      tol_dis_arcsec_2 = tol_dis_usno_objstars_arcsec**2

c     identify a maximum of USNO stars on frame
c     -----------------------------------------------------
      open(9,file=filename_object_list,status='old')
      read(9,*)
      read(9,*)
      i = 0
  500 i = i+1
      if(i.gt.number_obj_max) then
        write(nu_fail,*) 'WARNING: increase number_obj_max'
        goto 501
      end if
      read(9,*,end=501) obj_coord_1(i,1),obj_coord_1(i,2)
      goto 500
  501 close(9)
      number_obj = i-1
      nz = 0
      do i = 1,number_obj
        cand_x = obj_coord_1(i,1)
        cand_y = obj_coord_1(i,2)
        if(cand_x.gt.x_buffer .and. 
     *       cand_x.lt.dble(naxis1)-x_buffer .and.
     *     cand_y.gt.y_buffer .and. 
     *       cand_y.lt.dble(naxis2)-y_buffer) then
          nz = nz+1
          call compute_alpha_delta(cand_x,cand_y,
     *              n_axis1_meaning,n_axis1_direction,
     *              n_axis2_direction,
     *              focal_length,x_off,y_off,
     *              plc,
     *              alpha_proj_center_deg,delta_proj_center,
     *              alpha_cand,delta_cand)
          obj_alpha(nz) = alpha_cand
          obj_delta(nz) = delta_cand
          obj_coord(nz,1) = obj_coord_1(i,1)
          obj_coord(nz,2) = obj_coord_1(i,2)
        end if
      end do
      number_obj = nz

c     determine boundaries for USNO star selection
c     --------------------------------------------
      cand_x = 0.d0
      cand_y = 0.d0
          call compute_alpha_delta(cand_x,cand_y,
     *              n_axis1_meaning,n_axis1_direction,
     *              n_axis2_direction,
     *              focal_length,x_off,y_off,
     *              plc,
     *              alpha_proj_center_deg,delta_proj_center,
     *              alpha_cand,delta_cand)
      alpha_high= alpha_cand
      delta_low = delta_cand

      cand_x = naxis1
      cand_y = naxis2
          call compute_alpha_delta(cand_x,cand_y,
     *              n_axis1_meaning,n_axis1_direction,
     *              n_axis2_direction,
     *              focal_length,x_off,y_off,
     *              plc,
     *              alpha_proj_center_deg,delta_proj_center,
     *              alpha_cand,delta_cand)

      alpha_low = alpha_cand
      delta_high = delta_cand  
c     write(6,*) ' alpha_low,delta_low',alpha_low,delta_low
c     write(6,*) ' alpha_high,delta_high',alpha_high,delta_high


c     read USNO star file
c     -------------------
      open(1,file=usno_star_file,status='old')
      i = 0
      nz = 0
  510 i = i+1
      if(i.gt.max_usno) then
        write(nu_fail,*) 'WARNING: increase max_usno'
        goto 515
      end if
  512 read(1,*,end=515) usno_alpha(i),usno_delta(i),
     *                  mag(i),number_usno(i)
      nz = nz+1
      call equa_deg_to_xygnom(usno_alpha(i),usno_delta(i),
     *      alpha_proj_center_deg,delta_proj_center,
     *      x_gnom,y_gnom)
      x = x_gnom*focal_length
      y = y_gnom*focal_length
      if(n_axis1_direction.eq.-1) x = -x
      if(n_axis2_direction.eq.-1) y = -y
      if(n_axis1_meaning.eq.2) then
        xx = x
        yy = y
        x = yy
        y = xx
      end if
      x = x+x_off
      y = y+y_off

      if(x.lt.0.d0) goto 512
      if(y.lt.0.d0) goto 512
      if(x.gt.dble(naxis1)) goto 512
      if(y.gt.dble(naxis2)) goto 512
      goto 510
  515 n_usno_orig = i-1
      close(1)
c     write(6,*) 'number of USNO stars read from CD', nz
c     write(6,*) 'number of USNO stars supposed to be on frame',
c    *            n_usno_orig
      
c     do the matching
c     ---------------
      open(7,file=filename_matching_list,status='unknown')
      open(8,file=filename_usno_coord_mag,status='unknown')
      n_usno_identif = 0
      n_usno_not_identif = 0
      do j = 1,n_usno_orig
        k = 0
        cdcd = cosd(usno_delta(j))
        do i = 1,number_obj
          dist_2 = ((usno_alpha(j)-obj_alpha(i))*cdcd)**2 
     *             +(usno_delta(j)-obj_delta(i))**2
          dist_2 = dist_2*3600.*3600.d0
          if(dist_2.le.tol_dis_arcsec_2) then
            k = k+1
            l = i
            dist_2_f = dist_2
          end if
        end do
c       if(k.gt.1) write(nu_fail,*) 'matching',j,l,k
        if(k.eq.1) then
            write(8,8080) obj_coord(l,1),obj_coord(l,2),
     *       mag(j),number_usno(j),
     *       usno_alpha(j),usno_delta(j),dist_2_f
 8080 format(2f10.1,f6.1,i11,2f15.7,1pd12.3)
            write(7,3000) obj_coord(l,1),obj_coord(l,2),
     *       usno_alpha(j),usno_delta(j)
 3000 format(2f10.2,2f15.10)
          n_usno_identif = n_usno_identif+1
        else
          n_usno_not_identif = n_usno_not_identif+1
          usno_alpha_not_identif(n_usno_not_identif) = usno_alpha(j)
          usno_delta_not_identif(n_usno_not_identif) = usno_delta(j)
          mag_not_identif(n_usno_not_identif) = mag(j)
          problem_identif(n_usno_not_identif) = k
        end if
      end do
      close(7)
      close(8)

c      compute x,y coordinates of unidentified USNO stars
c     ---------------------------------------------------
      if(n_usno_not_identif.gt.0) then
      open(9,file=filename_usno_not_identified,status='unknown')
      do i = 1,n_usno_not_identif
        alpha_deg = usno_alpha_not_identif(i)
        delta_deg = usno_delta_not_identif(i)
        call equa_deg_to_xygnom(alpha_deg,delta_deg,
     *      alpha_proj_center_deg,delta_proj_center,
     *      x_gnom,y_gnom)
        usnox(i) = x_gnom
        usnoy(i) = y_gnom
      end do
      do i = 1,n_usno_not_identif
        usnox(i) = usnox(i)*focal_length
        usnoy(i) = usnoy(i)*focal_length
      end do
      do i = 1,n_usno_not_identif
        if(n_axis1_direction.eq.-1) usnox(i) = -usnox(i)
        if(n_axis2_direction.eq.-1) usnoy(i) = -usnoy(i)
      end do
      do i = 1,n_usno_not_identif
        if(n_axis1_meaning.eq.2) then
          y_gnom = usnox(i)
          x_gnom = usnoy(i)
          usnox(i) = x_gnom
          usnoy(i) = y_gnom
        end if
      end do

      do i = 1,n_usno_not_identif
        usnox(i) = usnox(i)+x_off
        usnoy(i) = usnoy(i)+y_off
        write(9,9001) usnox(i),usnoy(i),problem_identif(i),
     *                usno_alpha_not_identif(i),
     *                usno_delta_not_identif(i),mag_not_identif(i)
 9001 format(2f10.2,i4,5x,2f15.10,f5.1)
      end do

      close(9)
      end if

      write(nu_fail,*) 'number of identified stars in object list',
     *                  n_usno_identif,n_usno_orig

      if(n_find_max_stars.eq.3) then
        eff = dble(n_usno_identif)/dble(n_usno_orig)
        if(eff.lt.0.66666d0) then
          open(nu_warn,file=filename_warn,status='unknown')
          write(nu_warn,*)n_usno_identif,' found out of',n_usno_orig,
     *                  ' only'
          close(nu_warn)
        end if
      end if
c     write(6,*) 'n_usno_orig,n_usno_identif',
c    *            n_usno_orig,n_usno_identif

      return
      end
c
      subroutine compute_alpha_delta(ccand_x,ccand_y,
     *              n_axis1_meaning,n_axis1_direction,
     *              n_axis2_direction,
     *              focal_length,x_off,y_off,
     *              plc,
     *              alpha_proj_center_deg,delta_proj_center,
     *              alpha_cand,delta_cand)
      implicit NONE

      integer i,j,nz,l,icode
      integer naxis1,naxis2
      integer n_axis1_meaning,n_axis2_meaning
      integer n_axis1_direction,n_axis2_direction

      real*8 focal_length,x_off,y_off
      real*8 alpha_proj_center_deg,delta_proj_center
      real*8 pie,pierez,epsilon
      real*8 cand_x,cand_y
      real*8 ccand_x,ccand_y
      real*8 aa,bb,da,dd,rms
      real*8 ttan
      real*8 eta,zeta,alpha_cand,delta_cand
      real*8 alphaz_deg,deltaz_deg
      real*8 x_ast,y_ast
      real*8 plc(30)
      real*8 tand,sind,cosd
      external tand,sind,cosd


      character*1 minus,axis1_meaning,axis2_meaning,sign,
     *             axis1_direction,axis2_direction

      character*100 filename_plate_solution,filename_stars_coord


      icode = 0
      pie = 3.141592653589793D0
      pierez = pie/180.d0
      minus = '-'


c     determine alpha,delta of candidates
c     -----------------------------------
      axis1_direction = ' '
      if(n_axis1_direction.lt.0) axis1_direction = minus

      axis2_direction = ' '
      if(n_axis2_direction.lt.0) axis2_direction = minus

      rms = epsilon*180.d0*3600.d0/pie


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


      return
   10 icode = 61
      return
      end
c
c    -----------------------------------------------------------
      subroutine final_match_usno_obj(
     *              filename_object_list,
     *              filename_usno_coord_mag,
     *              filename_usno_not_identified,
     *              usno_star_file,
     *              n_axis1_meaning,n_axis1_direction,
     *              n_axis2_direction,
     *             nu_warn,
     *             filename_warn,
     *             nu_fail,
     *             seeing_pix_units,
     *              tol_dis_usno_objstars_pix,
     *              focal_length,x_off,y_off,
     *             alpha_proj_center_deg,delta_proj_center,
     *             naxis1,naxis2,icode)

      implicit NONE

      integer number_obj_max,max_usno
      parameter(number_obj_max=10000)
      parameter(max_usno=5000)

      integer i,j,k,l,n
      integer n_axis1_meaning,n_axis1_direction,n_axis2_direction
      integer nu_warn,nu_fail
      integer naxis1,naxis2,icode
      integer number_obj,n_usno_match,n_usno_on_frame
      integer usno_n(max_usno)

      real*8 tol_dis_usno_objstars_pix,tol_dis_usno_objstars_pix_2
      real*8 focal_length,x_off,y_off
      real*8 alpha_proj_center_deg,delta_proj_center
      real*8 usnox(max_usno),usnoy(max_usno)
      real*8 usno_alpha_deg(max_usno),usno_delta(max_usno)
      real*8 usno_mag(max_usno)
      real*8 usno_x(max_usno),usno_y(max_usno)
      real*8 u_alpha_deg,u_delta,u_mag
      real*8 x,y,xx,yy
      real*8 x_gnom,y_gnom
      real*8 x_buffer,y_buffer
      real*8 dist_2,dist_f,effiency
      real*8 obj_coord(number_obj_max,2)
      real*8 seeing_pix_units
      real*8 objx,objy,dx,dy

      character*100 filename_object_list
      character*100 filename_usno_coord_mag
      character*100 filename_usno_not_identified
      character*100 usno_star_file
      character*100 filename_warn

      icode = 0

      open(8,file=filename_usno_coord_mag,status='unknown')
      n = 0
      dx = 0.d0
      dy = 0.d0
    5 read(8,*,end=6)objx,objy,x,n,u_alpha_deg,u_delta
      call equa_deg_to_xygnom(u_alpha_deg,u_delta,
     *      alpha_proj_center_deg,delta_proj_center,
     *      x_gnom,y_gnom)
      x = x_gnom*focal_length
      y = y_gnom*focal_length
      if(n_axis1_direction.eq.-1) x = -x
      if(n_axis2_direction.eq.-1) y = -y
      if(n_axis1_meaning.eq.2) then
        xx = x
        yy = y
        x = yy
        y = xx
      end if
      x = x+x_off
      y = y+y_off
c     write(6,*) objx-x,objy-y
      dx = dx+objx-x
      dy = dy+objy-y
      n = n+1
      goto 5
    6 close(8)
      dx = dx/dble(n)
      dy = dy/dble(n)
c     write(6,*) 'dx,dy',dx,dy
      x_off = x_off+dx
      y_off = y_off+dy

      tol_dis_usno_objstars_pix_2 = 
     *       tol_dis_usno_objstars_pix**2

      x_buffer = 3.*seeing_pix_units
      y_buffer = x_buffer

c     take all original USNO stars; compute x-y coordinates
c     -----------------------------------------------------
      open(2,file=usno_star_file,status='old')
      n_usno_on_frame = 0
   10 read(2,*,end=20) u_alpha_deg,u_delta,u_mag,n
      call equa_deg_to_xygnom(u_alpha_deg,u_delta,
     *      alpha_proj_center_deg,delta_proj_center,
     *      x_gnom,y_gnom)
      x = x_gnom*focal_length
      y = y_gnom*focal_length
      if(n_axis1_direction.eq.-1) x = -x
      if(n_axis2_direction.eq.-1) y = -y
      if(n_axis1_meaning.eq.2) then
        xx = x
        yy = y
        x = yy
        y = xx
      end if
      x = x+x_off
      y = y+y_off
      if(x.lt.x_buffer) goto 10
      if(y.lt.y_buffer) goto 10
      if(x.gt.dble(naxis1)-x_buffer) goto 10
      if(y.gt.dble(naxis2)-y_buffer) goto 10
      n_usno_on_frame = n_usno_on_frame+1
      if(n_usno_on_frame.gt.max_usno) then
        write(nu_fail,*) 'WARNING: increase parameter max_usno'
        n_usno_on_frame = max_usno
        goto 20
      end if
      usno_alpha_deg(n_usno_on_frame) = u_alpha_deg
      usno_delta(n_usno_on_frame) = u_delta
      usno_mag(n_usno_on_frame) = u_mag
      usno_n(n_usno_on_frame) = n
      usno_x(n_usno_on_frame) = x
      usno_y(n_usno_on_frame) = y
      goto 10

   20 close(2)

c     match USNO stars with stars in object-list
c     ------------------------------------------
      open(9,file=filename_object_list,status='old')
      read(9,*)
      read(9,*)
      i = 0
   30 i = i+1
      if(i.gt.number_obj_max) then
        write(nu_fail,*) 'WARNING: increase number_obj_max'
        goto 31
      end if
      read(9,*,end=31) obj_coord(i,1),obj_coord(i,2)
      goto 30
   31 close(9)
      number_obj = i-1

      open(7,file=filename_usno_not_identified,status='unknown')
      open(8,file=filename_usno_coord_mag,status='unknown')
      n_usno_match = 0
      do j = 1,n_usno_on_frame
        k = 0
        do i = 1,number_obj
          dist_2 = (usno_x(j)-obj_coord(i,1))**2
     *            +(usno_y(j)-obj_coord(i,2))**2
          if(dist_2.le.tol_dis_usno_objstars_pix_2) then
            k = k+1
            l = i
            dist_f = dsqrt(dist_2)
          end if
        end do
        if(k.eq.1) then
          write(8,8080) obj_coord(l,1),obj_coord(l,2),
     *       usno_mag(j),usno_n(j),
     *       usno_alpha_deg(j),usno_delta(j),dist_f
 8080 format(2f10.1,f6.1,i11,2f15.7,1pd12.3)
          n_usno_match = n_usno_match+1
        else
          write(7,9001) usno_x(j),usno_y(j),k,
     *                usno_alpha_deg(j),usno_delta(j),
     *                usno_mag(j),usno_n(j)
 9001 format(2f10.2,i4,5x,2f15.10,f5.1,i11)
        end if
      end do
      effiency = dble(n_usno_match)/dble(n_usno_on_frame)
      if(effiency.lt.0.66666d0) then
        open(nu_warn,file=filename_warn,status='unknown')
        write(nu_warn,*)n_usno_match,' found out of',n_usno_on_frame,
     *                  ' only'
        close(nu_warn)
      end if

c     write(6,*) 'n_usno_match,n_usno_on_frame',
c    *            n_usno_match,n_usno_on_frame

      write(nu_fail,*) 'number of identified stars in object list',
     *                  n_usno_match

      close(7)
      close(8)

      return
      end
