c     mkpltsol.f
c     USNO star matching
c     find camera offset in RA and DEC
c     find camera rotation angle
c     Monte-Carlo shooting is the underlying method

c     needs to be linked with library to read USNO star catalogue
c
c     ---------------------------------------------

      implicit NONE 

      integer nn_max_stars 
      parameter(nn_max_stars=1000) 

      integer icode 
      integer i,j,len_image_name 
      integer n_skip_rec_id_file,n_skip_rec_br_file 
      integer n_skip_read_parms,n_skip_rec_obj_file
      integer naxis1,naxis2,camera,chip_number 
      integer n_max_stars 
      integer n_usno_org 
      integer n_br_stars_org 
      integer n_id_stars_org,n_fr_stars 
      integer n_obj_stars_half
      integer usno_ref_number(nn_max_stars) 
      integer intensity(nn_max_stars) 
      integer min_number_usno_stars_on_frame 
      integer n_match,n_match_first 
      integer iseed 
      integer number_monte_carlo_shoots
      integer number_monte_carlo_shoots_1
      integer iter 
      integer unit
      integer iter_search_camera_pointing

      real*8 fr_id_stars_all_x(nn_max_stars) 
      real*8 fr_id_stars_all_y(nn_max_stars) 
      real*8 fr_obj_stars_half_x(nn_max_stars) 
      real*8 fr_obj_stars_half_y(nn_max_stars) 
      real*8 flux(nn_max_stars) 
      real*8 seeing 
      real*8 opt_ax_al_deg_org,opt_ax_del_deg_org 
      real*8 opt_ax_al_deg_1,opt_ax_del_deg_1 
      real*8 opt_ax_al_deg,opt_ax_del_deg 
      real*8 crpix1,crpix2
      real*8 pix_scale 
      real*8 fr_center_minus_opt_ax_al_deg_or 
      real*8 fr_center_minus_opt_ax_del_deg_or 
      real*8 fr_size_al_deg,fr_size_del_deg 
      real*8 size_al_deg,size_del_deg 
      real*8 focal_length_org 
      real*8 focal_length 
      real*8 fr_center_al_deg_org,fr_center_del_deg_org 
      real*8 fr_center_al_deg,fr_center_del_deg 
      real*8 usno_al_deg(nn_max_stars) 
      real*8 usno_del_deg(nn_max_stars) 
      real*8 usno_mag_r(nn_max_stars),usno_mag_b(nn_max_stars) 
      real*8 fr_br_stars_x(nn_max_stars),fr_br_stars_y(nn_max_stars) 
      real*8 fr_stars_x(nn_max_stars),fr_stars_y(nn_max_stars) 
      real*8 matching_coords(nn_max_stars,6) 
      real*8 pointing_error_arcsec,min_percentage_match 
      real*8 min_percentage_match_first
      real*8 pointing_error_max
      real*8 pointing_error_1,pointing_error_2
      real*8 pointing_error_arcsec_3,pointing_error_arcsec_4
      real*8 f1,f2
      real*8 percentage 
      real*8 ymin,ymax,xmin,xmax 
      real*8 off_x,off_y,dev_off_x,dev_off_y 
      real*8 off_x_max,off_y_max
      real*8 angle 
      real*8 max_camera_rot_deg,min_dist_among_stars 
      real*8 radius_0,delta_radius 
      real*8 max_diff_fr_unso_arcsec,plc(30) 
      real*8 max_diff_fr_unso
      real*8 foc_length_err_promille
      real*8 min_dist_among_stars_arsec
      real*8 usno_x_pix_max,usno_x_pix_min
      real*8 usno_y_pix_max,usno_y_pix_min
      real*8 cosd 
      external cosd 

      character*80 file_image 
      character*80 file_usno_pix_coords
      character*80 file_usno_nomatch_pix_coords
      character*80 file_usno_read_from_cd 
      character*80 file_bright_stars_coord 
      character*80 file_all_objects
      character*80 file_identified_objects 
      character*80 file_matching_list 
      character*80 file_warning,file_ok,file_failed
      character*40 release

      logical pipeline 
      logical test_run 
      logical warning

      release = 'RELEASE 2004.2.13'

c      test_run = .true. 
      test_run = .false. 

      n_max_stars = nn_max_stars 

c     ---------------
c     read image name
c     ---------------
      icode = 0 
      call read_image_name(icode, 
     *                     pipeline,
     *                     release,
     *                     file_image) 
      if(.not. pipeline) test_run = .true.

    5 continue

      warning = .false.

      if(test_run) then
      write(6,*) ' ' 
      write(6,*) '--------------------------------------'
      write(6,*) file_image 
      end if

c     ---------------
c     build file names
c     ---------------
      call build_file_names(icode,
     *                     pipeline,
     *                     file_image,
     *                     file_bright_stars_coord,
     *                     file_identified_objects,
     *                     file_all_objects,
     *                     file_matching_list,
     *                     file_usno_read_from_cd,
     *                     file_usno_pix_coords,
     *                     file_usno_nomatch_pix_coords,
     *                     file_warning,
     *                     file_ok,
     *                     file_failed,
     *                     n_skip_read_parms,
     *                     n_skip_rec_obj_file,
     *                     n_skip_rec_id_file,
     *                     n_skip_rec_br_file)

      open(1,file=file_failed,status='unknown')
      write(1,*)
     *  'if there is also a ...mkpltsol.OK file, mkpltsol worked fine'
      write(1,*) ' '
      close(1)

c     ---------------------------------------
c     get frame and observational parameters
c     ---------------------------------------
      call get_frame_parameters(icode,
     *          file_failed,
     *          file_all_objects,
     *          n_skip_rec_obj_file,
     *          n_skip_read_parms,
     *          pipeline,
     *          opt_ax_al_deg_org,
     *          opt_ax_del_deg_org,
     *          naxis1,naxis2,
     *          camera,
     *          chip_number,
     *          pix_scale,
     *          crpix1,crpix2,
     *          fr_size_al_deg,fr_size_del_deg)
c     opt_ax_al_deg_org = opt_ax_al_deg_org+1./60
c     opt_ax_del_deg_org = opt_ax_del_deg_org-1./60.

c     write(6,*) 'type crpix1,crpix2'
c     read(5,*) crpix1,crpix2
      if(icode.gt.0) goto 100

c     opt_ax_al_deg_org = opt_ax_al_deg_org+270./3600.
c     opt_ax_del_deg_org = opt_ax_del_deg_org-100./3600.

      opt_ax_al_deg_1 = opt_ax_al_deg_org
      opt_ax_del_deg_1 = opt_ax_del_deg_org

c     ----------------------------------
c     get hardwired tolerance parameters
c     ----------------------------------
      call get_run_parameters(icode,
     *             camera,
     *             number_monte_carlo_shoots,
     *             min_number_usno_stars_on_frame,
     *             min_percentage_match,
     *             min_percentage_match_first,
     *             n_match_first,
     *             max_camera_rot_deg,
     *             foc_length_err_promille,
     *             pointing_error_arcsec,pointing_error_max,
     *             max_diff_fr_unso_arcsec,
     *             min_dist_among_stars_arsec,
     *             radius_0,pix_scale,
     *             delta_radius)


      number_monte_carlo_shoots_1 = number_monte_carlo_shoots
      pointing_error_arcsec_3 = pointing_error_arcsec/3.d0
      pointing_error_arcsec_4 = pointing_error_arcsec/5.d0

c     ----------------------
c     estimate focal length
c     ----------------------
      icode = 0 
      call first_estimation_focal_length(icode, 
     *          opt_ax_al_deg_org, 
     *          opt_ax_del_deg_org, 
     *          pix_scale, 
     *          focal_length_org) 

c     -------------------------------------
c     estimate R.A. and DEC of frame center
c     -------------------------------------
      fr_center_al_deg_org = opt_ax_al_deg_org+ 
     *                      (crpix1*pix_scale/3600.d0- 
     *                       dble(naxis1)/2.d0*pix_scale/3600.d0) 
     *                       /cosd(opt_ax_del_deg_org) 
      if (fr_center_al_deg_org > 360.0 )then
      	fr_center_al_deg_org = fr_center_al_deg_org -360.0
      end if
      if (fr_center_al_deg_org < 0.0 ) then
      	fr_center_al_deg_org = fr_center_al_deg_org +360.0
      end if
      fr_center_del_deg_org =    opt_ax_del_deg_org- 
     *                       crpix2*pix_scale/3600.d0+ 
     *                       dble(naxis2)/2.d0*pix_scale/3600.d0 
c     write(6,*) 'fr_center_al_deg_org',fr_center_al_deg_org
c     write(6,*) 'fr_center_del_deg_org',fr_center_del_deg_org



c     ---------------
c     get USNO stars
c     ---------------
      icode = 0 
      call get_usno_stars(icode, 
     *          file_failed,file_warning,warning,
     *          n_max_stars, 
     *          file_usno_read_from_cd, 
     *          usno_al_deg,usno_del_deg,usno_mag_r,usno_mag_b, 
     *          usno_ref_number, 
     *          n_usno_org, 
     *          min_number_usno_stars_on_frame, 
     *          fr_size_al_deg,fr_size_del_deg, 
     *          fr_center_al_deg_org,fr_center_del_deg_org) 
      if(icode.gt.0) goto 100



c     ----------------
c     get bright stars
c     ----------------
      icode = 0 
      call get_bright_stars(icode, 
     *          file_failed,file_warning,warning,
     *          iseed, 
     *          n_max_stars, 
     *          file_bright_stars_coord, 
     *          fr_br_stars_x,fr_br_stars_y, 
     *          n_br_stars_org, 
     *          n_skip_rec_br_file) 

c     ---------------------------------------------------------
c     in case there is no bright star file, use obj file to
c       build virtual bright and id star files
c     ---------------------------------------------------------


      if(icode.eq.300) then 
        icode = 0
        call build_virtual_br_id_files(icode,
     *          file_failed,file_warning,warning,
     *          test_run,
     *          n_usno_org,
     *          file_bright_stars_coord,       
     *          file_identified_objects, 
     *          file_all_objects,
     *          pipeline,
     *          n_skip_rec_obj_file,
     *          n_skip_rec_id_file,
     *          n_skip_rec_br_file)


        if(icode.gt.0) goto 100
        icode = 0
        call get_bright_stars(icode,
     *          file_failed,file_warning,warning,
     *          iseed,
     *          n_max_stars,
     *          file_bright_stars_coord,
     *          fr_br_stars_x,fr_br_stars_y,
     *          n_br_stars_org,
     *          n_skip_rec_br_file)
      end if



c     --------------------
c     get identified stars
c     --------------------
      icode = 0 
      call get_identif_stars(icode, 
     *          file_failed,file_warning,warning,
     *          n_max_stars, 
     *          file_identified_objects, 
     *          fr_id_stars_all_x,fr_id_stars_all_y, 
     *          n_id_stars_org, 
     *          intensity,flux, 
     *          pipeline, 
     *          n_skip_rec_id_file) 
      if(icode.gt.0) goto 100




c     ------------------------------
c     write pix coords of USNO stars
c     ------------------------------
      icode = 0 
      angle = 0.d0
      call write_usno_pix_coords(icode, 
     *          file_usno_pix_coords, 
     *          file_all_objects,
     *          n_skip_rec_obj_file,
     *          n_max_stars,n_usno_org, 
     *          usno_x_pix_max,usno_x_pix_min,
     *          usno_y_pix_max,usno_y_pix_min,
     *          usno_al_deg,usno_del_deg,usno_mag_r,usno_mag_b,
     *          usno_ref_number,
     *          opt_ax_al_deg_org, 
     *          opt_ax_del_deg_org, 
     *          crpix1,crpix2,
     *          angle,
     *          focal_length_org) 
c     write(6,*) 'type return'
c     read(5,*)


c     ------------------------------------------
c     first try to match USNO with bright stars
c     ------------------------------------------
      icode = 0 
      n_match = 0 
      number_monte_carlo_shoots = number_monte_carlo_shoots_1
      pointing_error_1 = 0.d0
      pointing_error_2 = pointing_error_arcsec

      call match_using_monte_carlo(icode, 
     *          matching_coords,n_match, 
     *          pointing_error_1,pointing_error_2,
     *          number_monte_carlo_shoots,
     *          max_camera_rot_deg,angle, 
     *          foc_length_err_promille,
     *          min_percentage_match, 
     *          xmin,xmax,ymin,ymax, 
     *          off_x,off_y,dev_off_x,dev_off_y, 
     *          radius_0,delta_radius, 
     *          iseed, 
     *          n_max_stars, 
     *          usno_al_deg,usno_del_deg, 
     *          n_usno_org, 
     *          fr_br_stars_x,fr_br_stars_y, 
     *          n_br_stars_org, 
     *          opt_ax_al_deg_org, 
     *          opt_ax_del_deg_org, 
     *          focal_length_org, 
     *          naxis1,naxis2,
     *          opt_ax_al_deg,opt_ax_del_deg, 
     *          focal_length, 
     *          crpix1,crpix2) 



      percentage = dble(n_match)/dble(n_usno_org)*100.d0 

      if(test_run) then
      write(6,*) '               monte carlo 1'
        write(6,*) 'number of monte-carlo shoots',
     *              number_monte_carlo_shoots
        write(6,*) 'n_usno_org,n_br_stars_org',
     *              n_usno_org,n_br_stars_org
        write(6,*) 'percentage,n_match',
     *              percentage,n_match
      end if
      if(percentage.gt.min_percentage_match_first .or.
     *   n_match.gt.n_match_first) goto 10

c     --------------------------------------------------------------------
c     if first try failed repeat with larger error for telescope pointing
c     --------------------------------------------------------------------
      iter_search_camera_pointing = 0
    6 continue
      iter_search_camera_pointing = 
     *    iter_search_camera_pointing+1

      f1 = pointing_error_2**2-pointing_error_1**2

      pointing_error_1 = pointing_error_1+pointing_error_arcsec
     *                     -pointing_error_arcsec/5.d0
      pointing_error_2 = pointing_error_1+pointing_error_arcsec
      f2 = pointing_error_2**2-pointing_error_1**2

      if(pointing_error_2.gt. pointing_error_max) then
        open(1,file=file_failed,status='unknown')
        write(1,*) 'not enough stars to make 4th order plate solution'
        write(1,*) 'n_match = ',n_match
        close(1)
        icode = 800
        goto 100
      end if

c     ---------------
c     get USNO stars
c     ---------------
      icode = 0
      size_al_deg = fr_size_al_deg+pointing_error_2/3600.d0
      size_del_deg = fr_size_del_deg+pointing_error_2/3600.d0
      call get_usno_stars(icode,
     *          file_failed,file_warning,warning,
     *          n_max_stars,
     *          file_usno_read_from_cd,
     *          usno_al_deg,usno_del_deg,usno_mag_r,usno_mag_b,
     *          usno_ref_number,
     *          n_usno_org,
     *          min_number_usno_stars_on_frame,
     *          size_al_deg,size_del_deg,
     *          fr_center_al_deg_org,fr_center_del_deg_org)
      if(icode.gt.0) goto 100


      icode = 0 
      n_match = 0 
      number_monte_carlo_shoots = 
     *        dble(number_monte_carlo_shoots)*dsqrt(f2/f1)
      call match_using_monte_carlo(icode, 
     *          matching_coords,n_match, 
     *          pointing_error_1,pointing_error_2,
     *          number_monte_carlo_shoots,
     *          max_camera_rot_deg,angle, 
     *          foc_length_err_promille,
     *          min_percentage_match, 
     *          xmin,xmax,ymin,ymax, 
     *          off_x,off_y,dev_off_x,dev_off_y, 
     *          radius_0,delta_radius, 
     *          iseed, 
     *          n_max_stars, 
     *          usno_al_deg,usno_del_deg, 
     *          n_usno_org, 
     *          fr_br_stars_x,fr_br_stars_y, 
     *          n_br_stars_org, 
     *          opt_ax_al_deg_org, 
     *          opt_ax_del_deg_org, 
     *          focal_length_org, 
     *          naxis1,naxis2,
     *          opt_ax_al_deg,opt_ax_del_deg, 
     *          focal_length, 
     *          crpix1,crpix2) 

      percentage = dble(n_match)/dble(n_usno_org)*100.d0

      if(test_run) then
      write(6,*) '               monte carlo 1f'
        write(6,*) 'pointing_error_1,pointing_error_2',
     *              pointing_error_1,pointing_error_2
        write(6,*) 'number of monte-carlo shoots',
     *              number_monte_carlo_shoots
        write(6,*) 'n_usno_org,n_br_stars_org',
     *              n_usno_org,n_br_stars_org
        write(6,*) 'percentage,n_match',
     *              percentage,n_match
      end if

      if(n_match.lt.n_match_first .and. 
     *       percentage.lt.min_percentage_match) 
     *     goto 6

c     find USNO stars in the corrected field
c     ---------------------------------------

      opt_ax_al_deg_org = opt_ax_al_deg- 
     *    off_x*pix_scale/3600.d0/cosd(opt_ax_del_deg) 
      opt_ax_del_deg_org = 
     *    opt_ax_del_deg+off_y*pix_scale/3600.d0 

      fr_center_al_deg_org = opt_ax_al_deg_org+ 
     *                      (crpix1*pix_scale/3600.d0- 
     *                       dble(naxis1)/2.d0*pix_scale/3600.d0) 
     *                       /cosd(opt_ax_del_deg_org) 
      if (fr_center_al_deg_org > 360.0 )then
      	fr_center_al_deg_org = fr_center_al_deg_org -360.0
      end if
      if (fr_center_al_deg_org < 0.0 ) then
      	fr_center_al_deg_org = fr_center_al_deg_org +360.0
      end if
      fr_center_del_deg_org =    opt_ax_del_deg_org- 
     *                       crpix2*pix_scale/3600.d0+ 
     *                       dble(naxis2)/2.d0*pix_scale/3600.d0 
      icode = 0 
      call get_usno_stars(icode, 
     *          file_failed,file_warning,warning,
     *          n_max_stars, 
     *          file_usno_read_from_cd, 
     *          usno_al_deg,usno_del_deg,usno_mag_r,usno_mag_b, 
     *          usno_ref_number, 
     *          n_usno_org, 
     *          min_number_usno_stars_on_frame, 
     *          fr_size_al_deg,fr_size_del_deg, 
     *          fr_center_al_deg_org,fr_center_del_deg_org) 

      icode = 0 
      angle = 0.d0
      call write_usno_pix_coords(icode,
     *          file_usno_pix_coords,
     *          file_all_objects,
     *          n_skip_rec_obj_file,
     *          n_max_stars,n_usno_org,
     *          usno_x_pix_max,usno_x_pix_min,
     *          usno_y_pix_max,usno_y_pix_min,
     *          usno_al_deg,usno_del_deg,usno_mag_r,usno_mag_b,
     *          usno_ref_number,
     *          opt_ax_al_deg_org,
     *          opt_ax_del_deg_org,
     *          crpix1,crpix2,
     *          angle,
     *          focal_length_org)

      icode = 0 
      n_match = 0 
      pointing_error_1 = 0.d0
      pointing_error_2 = pointing_error_arcsec
      number_monte_carlo_shoots = number_monte_carlo_shoots_1
      call match_using_monte_carlo(icode, 
     *          matching_coords,n_match, 
     *          pointing_error_1,pointing_error_2,
     *          number_monte_carlo_shoots,
     *          max_camera_rot_deg,angle, 
     *          foc_length_err_promille,
     *          min_percentage_match, 
     *          xmin,xmax,ymin,ymax, 
     *          off_x,off_y,dev_off_x,dev_off_y, 
     *          radius_0,delta_radius, 
     *          iseed, 
     *          n_max_stars, 
     *          usno_al_deg,usno_del_deg, 
     *          n_usno_org, 
     *          fr_br_stars_x,fr_br_stars_y, 
     *          n_br_stars_org, 
     *          opt_ax_al_deg_org, 
     *          opt_ax_del_deg_org, 
     *          focal_length_org, 
     *          naxis1,naxis2,
     *          opt_ax_al_deg,opt_ax_del_deg, 
     *          focal_length, 
     *          crpix1,crpix2) 

      percentage = dble(n_match)/dble(n_usno_org)*100.d0 

   10 continue 

c     ---------------------------------------------------
c     improve values found for
c             telescope pointing
c             camera rotation
c             focal length
c     ---------------------------------------------------
      pointing_error_arcsec = pointing_error_arcsec_3
      number_monte_carlo_shoots = number_monte_carlo_shoots_1
      min_dist_among_stars = 
     *      min_dist_among_stars_arsec/pix_scale*cosd(opt_ax_del_deg) 
      call match_more_stars(icode, 
     *          n_max_stars, 
     *          iseed, 
     *          matching_coords,n_match, 
     *          min_percentage_match, 
     *          pointing_error_arcsec,
     *          number_monte_carlo_shoots,
     *          min_dist_among_stars, 
     *          max_camera_rot_deg,angle, 
     *          xmin,xmax,ymin,ymax, 
     *          off_x,off_y, 
     *          dev_off_x,dev_off_y, 
     *          usno_al_deg,usno_del_deg, 
     *          n_usno_org, 
     *          fr_br_stars_x,fr_br_stars_y, 
     *          n_br_stars_org, 
     *          opt_ax_al_deg,opt_ax_del_deg, 
     *          focal_length, 
     *          pix_scale, 
     *          crpix1,crpix2) 

      if(test_run) then 
      write(6,*) ' ' 
      write(6,*) '               match_more_stars' 
      write(6,*) 'n_match',n_match 
      write(6,*) 'ymin,ymax',ymin,ymax 
      end if 

      pointing_error_arcsec = pointing_error_arcsec_4
      number_monte_carlo_shoots = number_monte_carlo_shoots_1
      call improve_offset(icode, 
     *          n_max_stars, 
     *          iseed, 
     *          matching_coords,n_match, 
     *          pointing_error_arcsec,
     *          number_monte_carlo_shoots, 
     *          max_camera_rot_deg, 
     *          foc_length_err_promille,
     *          xmin,xmax,ymin,ymax, 
     *          off_x,off_y, 
     *          dev_off_x,dev_off_y, 
     *          opt_ax_al_deg,opt_ax_del_deg, 
     *          focal_length,angle, 
     *          off_x_max,off_y_max,
     *          crpix1,crpix2) 

      if(test_run) then 
      write(6,*) ' ' 
      write(6,*) '               improve_offset 1' 
      write(6,*) 'n_match',n_match 
      write(6,*) 'ymin,ymax',ymin,ymax 
      write(6,*) 'angle',angle 
      end if 

      pointing_error_arcsec = pointing_error_arcsec_4
      number_monte_carlo_shoots = number_monte_carlo_shoots_1
      call improve_offset(icode, 
     *          n_max_stars, 
     *          iseed, 
     *          matching_coords,n_match, 
     *          pointing_error_arcsec,
     *          number_monte_carlo_shoots, 
     *          max_camera_rot_deg, 
     *          foc_length_err_promille,
     *          xmin,xmax,ymin,ymax, 
     *          off_x,off_y, 
     *          dev_off_x,dev_off_y, 
     *          opt_ax_al_deg,opt_ax_del_deg, 
     *          focal_length,angle, 
     *          off_x_max,off_y_max,
     *          crpix1,crpix2) 

      if(test_run) then 
      write(6,*) ' ' 
      write(6,*) '               improve_offset 2' 
      write(6,*) 'n_match',n_match 
      write(6,*) 'ymin,ymax',ymin,ymax 
      write(6,*) 'angle',angle 
      end if 

c     -----------------------------------------------
c     build 4-th order plate solution 
c       matched stars may be removed
c     -----------------------------------------------
      if(n_match.le.n_match_first) then
        open(1,file=file_failed,status='unknown')
        write(1,*) 'not enough stars to make 4th order plate solution'
        write(1,*) 'n_match = ',n_match
        close(1)
        icode = 900
        goto 100
      end if
      max_diff_fr_unso = 
     *               max_diff_fr_unso_arcsec/cosd(opt_ax_del_deg) 
      call make_fourth_order_plate_solution(icode, 
     *          file_failed,
     *          n_max_stars, 
     *          matching_coords,n_match, 
     *          opt_ax_al_deg,opt_ax_del_deg, 
     *          focal_length,angle, 
     *          max_diff_fr_unso, 
     *          plc, 
     *          crpix1,crpix2) 
      if(icode.gt.0) goto 100

      pointing_error_arcsec = pointing_error_arcsec_4
      number_monte_carlo_shoots = number_monte_carlo_shoots_1
      call improve_offset(icode, 
     *          n_max_stars, 
     *          iseed, 
     *          matching_coords,n_match, 
     *          pointing_error_arcsec,
     *          number_monte_carlo_shoots, 
     *          max_camera_rot_deg, 
     *          foc_length_err_promille,
     *          xmin,xmax,ymin,ymax, 
     *          off_x,off_y, 
     *          dev_off_x,dev_off_y, 
     *          opt_ax_al_deg,opt_ax_del_deg, 
     *          focal_length,angle, 
     *          off_x_max,off_y_max,
     *          crpix1,crpix2) 

      if(test_run) then 
      write(6,*) ' ' 
      write(6,*) '               improve_offset 3' 
      write(6,*) 'n_match',n_match 
      write(6,*) 'ymin,ymax',ymin,ymax 
      write(6,*) 'angle',angle 
      end if 

      if(n_match.le.n_match_first) then
        open(1,file=file_failed,status='unknown')
        write(1,*) 'not enough stars to make 4th order plate solution'
        write(1,*) 'n_match = ',n_match
        close(1)
        icode = 901
        goto 100
      end if

      call make_fourth_order_plate_solution(icode, 
     *          file_failed,
     *          n_max_stars, 
     *          matching_coords,n_match, 
     *          opt_ax_al_deg,opt_ax_del_deg, 
     *          focal_length,angle, 
     *          max_diff_fr_unso, 
     *          plc, 
     *          crpix1,crpix2) 
      if(icode.gt.0) goto 100

c     ---------------------------------------------------- 
c     use 4-th order plate solution to identify more stars 
c     ---------------------------------------------------- 

c     get brightest object_list stars
c     ------------------------------
      icode = 0
      call get_obj_stars(icode,
     *          n_max_stars,
     *          pipeline,
     *          file_all_objects,
     *          file_warning,warning,
     *          n_skip_rec_obj_file,
     *          fr_obj_stars_half_x,
     *          fr_obj_stars_half_y,
     *          n_obj_stars_half)

      icode = 0
      call use_fourth_order_plate_solution(icode, 
     *          file_failed,
     *          n_max_stars, 
     *          matching_coords,n_match, 
     *          opt_ax_al_deg,opt_ax_del_deg, 
     *          focal_length,angle, 
     *          max_diff_fr_unso, 
     *          usno_al_deg,usno_del_deg, 
     *          n_usno_org, 
     *          fr_id_stars_all_x,fr_id_stars_all_y, 
     *          n_id_stars_org, 
     *          fr_obj_stars_half_x,
     *          fr_obj_stars_half_y,
     *          n_obj_stars_half,
     *          plc, 
     *          crpix1,crpix2) 
      if(test_run) then
      write(6,*) ' '
      write(6,*) '   use_fourth_order_plate_solution'
      write(6,*) 'n_match',n_match
      end if
      if(icode.gt.0) goto 100


      pointing_error_arcsec = pointing_error_arcsec_4
      number_monte_carlo_shoots = number_monte_carlo_shoots_1
      call improve_offset(icode, 
     *          n_max_stars, 
     *          iseed, 
     *          matching_coords,n_match, 
     *          pointing_error_arcsec,
     *          number_monte_carlo_shoots, 
     *          max_camera_rot_deg, 
     *          foc_length_err_promille,
     *          xmin,xmax,ymin,ymax, 
     *          off_x,off_y, 
     *          dev_off_x,dev_off_y, 
     *          opt_ax_al_deg,opt_ax_del_deg, 
     *          focal_length,angle, 
     *          off_x_max,off_y_max,
     *          crpix1,crpix2) 
      if(test_run) then
      write(6,*) ' '
      write(6,*) '               improve_offset 4'
      write(6,*) 'n_match',n_match
      write(6,*) 'ymin,ymax',ymin,ymax
      write(6,*) 'angle',angle
      end if


c     ----------------------------------- 
c     build 4-th order plate solution 
c     ----------------------------------- 
      if(n_match.le.n_match_first) then
        open(1,file=file_failed,status='unknown')
        write(1,*) 'not enough stars to make 4th order plate solution'
        write(1,*) 'n_match = ',n_match
        close(1)
        icode = 902
        goto 100
      end if



      call make_fourth_order_plate_solution(icode, 
     *          file_failed,
     *          n_max_stars, 
     *          matching_coords,n_match, 
     *          opt_ax_al_deg,opt_ax_del_deg, 
     *          focal_length,angle, 
     *          max_diff_fr_unso, 
     *          plc, 
     *          crpix1,crpix2) 
      if(icode.gt.0) goto 100

      unit=8
      call open_for_appending_file(unit,file_warning)
      write(unit,*)
     * 'Got to the end of 4th order solution'
      close(unit)


c     ---------------------------------------------------------------
c     write pix coordinates of all USNO stars supposed to be on frame
c     ---------------------------------------------------------------
      call write_usno_pix_coords(icode,
     *          file_usno_pix_coords,
     *          file_all_objects,
     *          n_skip_rec_obj_file,
     *          n_max_stars,n_usno_org,
     *          usno_x_pix_max,usno_x_pix_min,
     *          usno_y_pix_max,usno_y_pix_min,
     *          usno_al_deg,usno_del_deg,usno_mag_r,usno_mag_b,
     *          usno_ref_number,
     *          opt_ax_al_deg,
     *          opt_ax_del_deg,
     *          crpix1,crpix2,
     *          angle,
     *          focal_length)

c     ---------------------------------------------------------------------
c     write pix coordinates of all matched stars and of unmatched USNO stars
c     writes plate solution coefficients
c     ---------------------------------------------------------------------
      call write_matching_file(icode, 
     *          file_matching_list, 
     *          plc,
     *          file_all_objects,
     *          file_usno_nomatch_pix_coords,
     *          n_skip_rec_obj_file,
     *          n_max_stars,n_match,n_usno_org, 
     *          usno_al_deg,usno_del_deg,usno_mag_r,usno_mag_b,
     *          usno_ref_number,
     *          opt_ax_al_deg,
     *          opt_ax_del_deg,
     *          crpix1,crpix2,
     *          angle,
     *          focal_length,
     *          matching_coords) 

      percentage = dble(n_match)/dble(n_usno_org)*100.d0 

      if(test_run) then
      write(6,*) 'n_usno_org,n_br_stars_org,n_id_stars_org', 
     *            n_usno_org,n_br_stars_org,n_id_stars_org 
      write(6,*) 'file_image,n_match,percentage   ', 
     *            file_image(1:30),n_match,percentage
      write(6,*) 'xmin,xmax',xmin,xmax 
      write(6,*) 'ymin,ymax',ymin,ymax 
      write(6,*) 'angle',angle 
      write(6,*) 'opt_ax_al_deg_org-opt_ax_al_deg',
     *     (opt_ax_al_deg_1-opt_ax_al_deg)*3600.
      write(6,*) 'opt_ax_del_deg_org-opt_ax_del_deg',
     *     (opt_ax_del_deg_1-opt_ax_del_deg)*3600.
      end if

c     ------------------------------
c     write warning file
c     ------------------------------
      call write_warning_file(icode,
     *       pointing_error_arcsec,
     *       opt_ax_al_deg_1,opt_ax_al_deg,
     *       opt_ax_del_deg_1,opt_ax_del_deg,
     *       file_image,file_warning,
     *       percentage,min_percentage_match,
     *       n_usno_org,n_match,
     *       naxis1,naxis2,
     *       warning,test_run,
     *       xmin,xmax,usno_x_pix_min,usno_x_pix_max,
     *       ymin,ymax,usno_y_pix_min,usno_y_pix_max)


      open(1,file=file_ok,status='unknown')
      write(1,*) 'OK'
      close(1)

  100 continue

      if(icode.gt.0 .and. test_run) then
        write(6,*) ' '
        write(6,*) '********** failure image ',file_image(1:30)
      end if
      if(warning .and. test_run) then
        write(6,*) ' '
        write(6,*) '********** warning image ',file_image(1:30)
      end if

      stop 
      end 

c************************************************************
      subroutine explanation(icode,release)

      implicit NONE

      integer icode
      character*40 release

      write(6,'(a)') ' '
      write(6,'(a)') ' '
      write(6,'(a)') 'mkpltsol(1)'
      write(6,'(a)') ' '
      write(6,'(a)') release
      write(6,'(a)') ' '
      write(6,'(a)') 'NAME'
      write(6,'(a)') ' '
      write(6,'(a)') ' mkpltsol - matches frame stars with USNO stars'
      write(6,'(a)') '          - computes mosaic center pointing'
      write(6,'(a)') '          - computes camera rotation'
      write(6,'(a)') '          - computes fourth order plate solution'
      write(6,'(a)') '          - writes fourth order plate solution'
      write(6,'(a)') '            together with all matched USNO stars'
      write(6,'(a)') '            on a file'
      write(6,'(a)') '          - writes all unmatched USNO stars'
      write(6,'(a)') '            on a file'
      write(6,'(a)') ' '
      write(6,'(a)') 'SYNOPSIS'
      write(6,'(a)') ' '
      write(6,'(a)') '  mkpltsol imagename'
      write(6,'(a)') ' '
      write(6,'(a)') 'DESCRIPTION'
      write(6,'(a)') ' '
      write(6,'(a)') '  mkpltsol '
      write(6,'(a)') '    reads a list of pix coords of frame stars'
      write(6,'(a)') '            and values for naxis1,naxis2,....'
      write(6,'(a)') '     either from'
      write(6,'(a)') '        imagename.bright.jmp'
      write(6,'(a)') '        imagename.id.jmp'
      write(6,'(a)') '        imagename.obj.jmp'
      write(6,'(a)') ' '
      write(6,'(a)') '    or'
      write(6,'(a)') '       if there is no imagename.bright.jmp file,'
      write(6,'(a)') '         imagename.obj.jmp file is needed.'
      write(6,'(a)') '           See EXAMPLE for an '
      write(6,'(a)') '           imagename.bright.jmp file'
      write(6,'(a)') '           which contains the minimum necessary'
      write(6,'(a)') '           information to run mkpltsol.'
      write(6,'(a)') ' '
      write(6,'(a)') 
     *      '    In the latter case, less stars may be matched.'
      write(6,'(a)') ' '
      write(6,'(a)') '    Values for the following parms MUST be given:'
      write(6,'(a)') '    CRVAL1   R.A. pointing of mosaic center'
      write(6,'(a)') '    CRVAL2   DEC  pointing of mosaic center'
      write(6,'(a)') '    SCALE    pixel_scale'
      write(6,'(a)') '    CRPIX1   chip offset from mosaic center in x'
      write(6,'(a)') '    CRPIX2   chip offset from mosaic center in y'
      write(6,'(a)') '    NAX1     naxis1'
      write(6,'(a)') '    NAX2     naxis2'
      write(6,'(a)') ' '
      write(6,'(a)') 'OUPUT FILES'
      write(6,'(a)') '  imagename.mkpltsol.usno'
      write(6,'(a)') '       fourth-order plate solution'
      write(6,'(a)') '       list of matched USNO stars'
      write(6,'(a)') '  imagename.mkpltsol.usno_nomatch.pix'
      write(6,'(a)') '       list of unmatched USNO stars'
      write(6,'(a)') '  imagename.all_usno.pix '
      write(6,'(a)') '       list of matched and unmatched USNO stars'
      write(6,'(a)') '  imagename.mkpltsol.usno_org'
      write(6,'(a)') '       working file'
      write(6,'(a)') '  imagename.mkpltsol.FAILED'
      write(6,'(a)') '       is  created at start'
      write(6,'(a)') '  imagename.mkpltsol.OK'
      write(6,'(a)') '       is created when matching was completed'
      write(6,'(a)') '  imagename.mkpltsol.WARNING'
      write(6,'(a)') '       contains eventually warnings'

      write(6,'(a)') ' '
      write(6,'(a)') ' '
      write(6,'(a)') 'EXAMPLE FOR A SINGLE INPUTFILE imagename.obj.jmp'
      write(6,'(a)') '   Values not used in mkpltsol'
      write(6,'(a)') '     are set equal to zero.'
      write(6,'(a)') '   Note that X,Y and FLUX are necessary.'
      write(6,'(a)') '   FLUX can be on an arbitrary scale.'
      write(6,'(a)') '    Larger values for FLUX mean brighter objects.'
      write(6,'(a)') ' '  
      write(6,'(a)') '## XXXXXXXXXX' 
      write(6,'(a)') '#  XXXX'
      write(6,'(a)') '## MJD-OBS-CENTER  EXPTIME THRES FWHM'
     *           //'  MAXCOUNT    CRVAL1     CRVAL2     EXPNUM'
      write(6,'(a)') 
     *      '#    00000.0000000    0.0   0.0   0.0       0.0'
     *               //'  329.20142  -13.03178   000000'
      write(6,'(a)') 
     *         '## SCALE CHIP CRPIX1    CRPIX2    NAX1  NAX2'
     *               //'   DETECTOR           PHADU RDNOIS'
      write(6,'(a)') 
     *       '#   .185  00   7372.50     00.00  2047  4612'
     *               //' xxxxxxxxx            0.00  0.00'
      write(6,'(a)') '##   X       Y        FLUX    '
      write(6,'(a)') '   78.23   52.38       108.75 '  
      write(6,'(a)') ' 1578.60   56.07      4100.73 '
      write(6,'(a)') ' 1748.44   62.58     50183.65 '
      write(6,'(a)') '  678.68   67.29       272.74 '
      write(6,'(a)') ' 1969.17   78.79      1150.00 '
      write(6,'(a)') '             .'
      write(6,'(a)') '             .'
      write(6,'(a)') '             .'
      write(6,'(a)') '             .'
      write(6,'(a)') '             .'

      if(icode.eq.0) stop
      return
      end
c************************************************************
      subroutine build_file_names(icode,
     *                     pipeline,
     *                     image_name_file,
     *                     file_bright_stars_coord,
     *                     file_identified_objects,
     *                     file_all_objects,
     *                     file_matching_list,
     *                     file_usno_read_from_cd,
     *                     file_usno_pix_coords,
     *                     file_usno_nomatch_pix_coords,
     *                     file_warning,
     *                     file_ok,
     *                     file_failed,
     *                     n_skip_read_parms,
     *                     n_skip_rec_obj_file,
     *                     n_skip_rec_id_file,
     *                     n_skip_rec_br_file)

      implicit NONE

      integer icode
      integer n_skip_rec_id_file,n_skip_rec_br_file
      integer n_skip_read_parms,n_skip_rec_obj_file

      character*80 image_name_file
      character*80 file_bright_stars_coord
      character*80 file_identified_objects
      character*80 file_all_objects
      character*80 file_matching_list
      character*80 file_usno_read_from_cd
      character*80 file_usno_pix_coords
      character*80 file_usno_nomatch_pix_coords
      character*80 file_warning,file_ok,file_failed
      logical pipeline

      icode = 0

      if(.not.pipeline) then
        call hans_files(icode,
     *                     image_name_file,
     *                     file_bright_stars_coord,
     *                     file_identified_objects,
     *                     file_all_objects,
     *                     file_matching_list,
     *                     file_usno_read_from_cd,
     *                     file_usno_pix_coords,
     *                     file_usno_nomatch_pix_coords,
     *                     file_warning,
     *                     file_ok,
     *                     file_failed,
     *                     n_skip_read_parms,
     *                     n_skip_rec_obj_file,
     *                     n_skip_rec_id_file,
     *                     n_skip_rec_br_file)

      else
        call pipeline_files(icode,
     *                     image_name_file,
     *                     file_bright_stars_coord,
     *                     file_identified_objects,
     *                     file_all_objects,
     *                     file_matching_list,
     *                     file_usno_read_from_cd,
     *                     file_usno_pix_coords,
     *                     file_usno_nomatch_pix_coords,
     *                     file_warning,
     *                     file_ok,
     *                     file_failed,
     *                     n_skip_read_parms,
     *                     n_skip_rec_obj_file,
     *                     n_skip_rec_id_file,
     *                     n_skip_rec_br_file)
      end if

      return
      end
c**********************************************************8
      subroutine hans_files(icode,
     *                     frame,
     *                     file_bright_stars_coord,
     *                     file_identified_objects,
     *                     file_all_objects,
     *                     file_matching_list,
     *                     file_usno_read_from_cd,
     *                     file_usno_pix_coords,
     *                     file_usno_nomatch_pix_coords,
     *                     file_warning,
     *                     file_ok,
     *                     file_failed,
     *                     n_skip_read_parms,
     *                     n_skip_rec_obj_file,
     *                     n_skip_rec_id_file,
     *                     n_skip_rec_br_file)
      implicit NONE

      integer icode,i
      integer n_skip_rec_id_file,n_skip_rec_br_file
      integer len_frame_name
      integer n_skip_read_parms,n_skip_rec_obj_file

      character*80 file_bright_stars_coord
      character*80 file_identified_objects
      character*80 file_all_objects
      character*80 file_matching_list
      character*80 file_usno_read_from_cd
      character*80 file_usno_pix_coords
      character*80 file_usno_nomatch_pix_coords
      character*80 file_warning,file_ok,file_failed
      character*80 frame

      icode = 0

      n_skip_rec_br_file = 7
      n_skip_rec_id_file = 7
      n_skip_read_parms = 3
      n_skip_rec_obj_file = 7

      i = 1
      do while(frame(i:i).ne.' ')
        i = i+1
      end do
      i = i-1
      len_frame_name = i

      file_bright_stars_coord =
     *     frame(1:len_frame_name)//'_br'

      file_identified_objects =
     *     frame(1:len_frame_name)//'_id'

      file_all_objects =
     *     frame(1:len_frame_name)//'_obj'

      file_matching_list =
     *     frame(1:len_frame_name)//
     *                '.mkpltsol.usno'

      file_usno_read_from_cd =
     *     frame(1:len_frame_name)//
     *                '.mkpltsol.usno_org'

      file_usno_pix_coords =
     *     frame(1:len_frame_name)//
     *        '.mkpltsol.all_usno.pix'

      file_usno_nomatch_pix_coords = 
     *     frame(1:len_frame_name)//
     *        '.mkpltsol.usno_nomatch.pix'

      file_warning =
     *     frame(1:len_frame_name)//
     *        '.mkpltsol.WARNING'

      file_ok =
     *     frame(1:len_frame_name)//
     *        '.mkpltsol.OK'

      file_failed =
     *     frame(1:len_frame_name)//
     *        '.mkpltsol.FAILED'

      return
      end
c**********************************************************8
      subroutine pipeline_files(icode,
     *                     file_image,
     *                     file_bright_stars_coord,
     *                     file_identified_objects,
     *                     file_all_objects,
     *                     file_matching_list,
     *                     file_usno_read_from_cd,
     *                     file_usno_pix_coords,
     *                     file_usno_nomatch_pix_coords,
     *                     file_warning,
     *                     file_ok,
     *                     file_failed,
     *                     n_skip_read_parms,
     *                     n_skip_rec_obj_file,
     *                     n_skip_rec_id_file,
     *                     n_skip_rec_br_file)

      implicit NONE

      integer i,len_image_name
      integer icode
      integer n_skip_rec_id_file,n_skip_rec_br_file
      integer n_skip_read_parms,n_skip_rec_obj_file

      character*80 file_image
      character*80 file_bright_stars_coord
      character*80 file_all_objects
      character*80 file_identified_objects
      character*80 file_matching_list
      character*80 file_usno_read_from_cd
      character*80 file_usno_pix_coords
      character*80 file_usno_nomatch_pix_coords
      character*80 file_warning,file_ok,file_failed



      n_skip_rec_id_file = 7
      n_skip_rec_br_file = 7
      n_skip_read_parms = 3
      n_skip_rec_obj_file = 7
      i = 1
      do while(file_image(i:i).ne.' ')
        i = i+1
      end do
      i = i-1
      len_image_name = i

      file_bright_stars_coord =
     *   file_image(1:len_image_name)//'.bright.jmp'
      file_identified_objects =
     *   file_image(1:len_image_name)//'.id.jmp'
      file_all_objects =
     *   file_image(1:len_image_name)//'.obj.jmp'
      file_matching_list =
     *    file_image(1:len_image_name)//'.mkpltsol.usno'
      file_ok =
     *    file_image(1:len_image_name)//'.mkpltsol.OK'
      file_failed =
     *    file_image(1:len_image_name)//'.mkpltsol.FAILED'
      file_warning =
     *    file_image(1:len_image_name)//'.mkpltsol.WARNING'
      file_usno_read_from_cd=
     *    file_image(1:len_image_name)//'.mkpltsol.usno_org'
      file_usno_pix_coords =
     *    file_image(1:len_image_name)//
     *        '.mkpltsol.all_usno.pix'
      file_usno_nomatch_pix_coords =
     *    file_image(1:len_image_name)//
     *        '.mkpltsol.usno_nomatch.pix'

      return
      end

      subroutine build_virtual_br_id_files(icode,
     *          file_failed,file_warning,warning,
     *          test_run,
     *          n_usno_org,
     *          file_bright_stars_coord,
     *          file_identified_objects,
     *          file_all_objects,
     *          pipeline,
     *          n_skip_rec_obj_file,
     *          n_skip_rec_id_file,
     *          n_skip_rec_br_file)

      implicit NONE

      integer n_max_stars
      parameter(n_max_stars=1000)

      integer icode,n_usno_org
      integer n_skip_rec_obj_file,n_skip_rec_id_file
      integer n_skip_rec_br_file
      integer intensity(n_max_stars),intensity_sort(n_max_stars)
      integer nz,index_sort(n_max_stars),unit
      integer n,n_br,n_id,i,j

      real*8 fr_x(n_max_stars),fr_y(n_max_stars)
      real*8 fr_sort_x(n_max_stars),fr_sort_y(n_max_stars)
      real*8 flux(n_max_stars),flux_sort(n_max_stars)
       
      character*80 file_bright_stars_coord
      character*80 file_identified_objects
      character*80 file_all_objects
      character*80 file_failed
      character*80 file_warning
      character*80 line

      logical warning,pipeline,test_run

      warning = .true.
      unit = 8
      call open_for_appending_file(unit,file_warning)
      write(unit,*)
     *          'there is no bright star file'
      write(unit,*)
     *          'build bright star and id files'
      close(unit)
      if(test_run) then
      unit = 6
      write(unit,*) ' '
      write(unit,*)
     *          'there is no bright star file'
      write(unit,*)
     *          'build bright star and id files'
      end if

      file_bright_stars_coord = 'dummy_'//file_bright_stars_coord
      open(2,file=file_bright_stars_coord,status='unknown')
      line = '# virtual bright_star_file'
      do i = 1,n_skip_rec_br_file
        write(2,'(a)') line
      end do

      file_identified_objects = 'dummy_'//file_identified_objects
      open(3,file=file_identified_objects,status='unknown')
      line = '# virtual id_star_file'
      do i = 1,n_skip_rec_id_file
        write(3,'(a)') line
      end do

      open(1,file=file_all_objects,status='old',err=100)
      do i = 1,n_skip_rec_obj_file
        read(1,*)
      end do

      i = 0
    5 i = i+1

      if(i.gt.n_max_stars) then
          warning = .true.
          unit = 8
          call open_for_appending_file(unit,file_warning)
          write(unit,*)
     * 'increase parameter n_max_stars in subroutine'
         write(unit,*)
     * '   build_virtual_br_id_files'
          close(unit)
        goto 10
      end if

      if(pipeline) then
        read(1,*,end=10,err=110)
     *           fr_x(i),fr_y(i),flux(i)
        intensity(i) = flux(i)
      else
        read(1,*,end=10,err=110)
     *       fr_x(i),fr_y(i),intensity(i)
      end if
      goto 5

   10 close(1)
      nz = i-1

      do i = 1,nz
        intensity(i) = -intensity(i)
      end do

      call sortinteger(nz,intensity,index_sort)

      do i = 1,nz
        intensity(i) = -intensity(i)
      end do

      do i = 1,nz
        j = index_sort(i)
        fr_sort_x(i) = fr_x(j)
        fr_sort_y(i) = fr_y(j)
        intensity_sort(i) = intensity(j)
        if(pipeline) flux_sort(i) = flux(j)
      end do

      n_br = n_usno_org+n_usno_org/2
c     n_br = n_usno_org
      if(n_br.gt.nz) n_br = nz
      n_id = 6*n_usno_org
      if(n_id.gt.nz) n_id = nz

      i = 0
   20 i = i+1
      if(i.lt.n_br) then
        if(pipeline) then
          write(2,2000) fr_sort_x(i),fr_sort_y(i),
     *                  fr_sort_x(i),fr_sort_y(i),flux_sort(i)
 2000 format(4f10.2,f20.2)
        else
          write(2,2001) fr_sort_x(i),fr_sort_y(i),
     *                  fr_sort_x(i),fr_sort_y(i),intensity_sort(i)
 2001 format(4f10.2,i10)
        end if
      end if

      if(pipeline) then
        write(3,2000) fr_sort_x(i),fr_sort_y(i),
     *                  fr_sort_x(i),fr_sort_y(i),flux_sort(i)
      else
        write(3,2001) fr_sort_x(i),fr_sort_y(i),
     *                  fr_sort_x(i),fr_sort_y(i),intensity_sort(i)
      end if

      if(i.lt.nz) goto 20

      close(2)
      close(3)


      return

  100 icode = 600
      open(4,file=file_failed,status='unknown')
      write(4,6666) 'can not open file ',file_all_objects
 6666 format(a,a)
      close(4)
      return


  110 icode = 601
      open(4,file=file_failed,status='unknown')
      write(4,6666) 'problem to read star coordinates on ',
     *     file_all_objects        
      write(4,*)
     *      ' probably wrong value for n_skip_rec_obj_file'
      write(4,*) 'n_skip_rec_obj_file = ',n_skip_rec_obj_file
      close(4)
      return

      end
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
c************************************************************ 
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

c**************************************************** 
      subroutine first_estimation_focal_length(icode, 
     *          opt_ax_al_deg, 
     *          opt_ax_del_deg, 
     *          pix_scale, 
     *          focal_length_org) 

      implicit NONE 

      integer icode 

      real*8 opt_ax_al_deg,opt_ax_del_deg 
      real*8 pix_scale 
      real*8 focal_length_org 
      real*8 x_gnom,y_gnom,x,y 
      real*8 al_deg,del_deg 

      icode = 0 

      al_deg = opt_ax_al_deg 
      del_deg = opt_ax_del_deg+pix_scale/3600.d0 
      call equa_deg_to_xygnom(al_deg,del_deg, 
     *      opt_ax_al_deg,opt_ax_del_deg, 
     *      x_gnom,y_gnom) 
      focal_length_org = 1./y_gnom 

      return 
      end 
c**************************************************** 
      subroutine get_bright_stars(icode, 
     *          file_failed,file_warning,warning,
     *          iseed, 
     *          n_max_stars, 
     *          file_bright_stars_coord, 
     *          fr_br_stars_x,fr_br_stars_y, 
     *          n_br_stars_org, 
     *          n_skip_rec_br_file) 

      implicit NONE 

      integer i,icode,n_max_stars,n_br_stars_org 
      integer n_skip_rec_br_file 
      integer iseed,unit 

      real*8 fr_br_stars_x(n_max_stars),fr_br_stars_y(n_max_stars) 

      character*80 file_bright_stars_coord 
      character*80 file_failed,file_warning

      logical warning

      icode = 0 

c     provide frame stars 
c     -------------------- 
      open(2,file=file_bright_stars_coord,status='old',err=100) 
      do i = 1,n_skip_rec_br_file 
        read(2,*) 
      end do 
      i = 1 
   10 read(2,*,end=15,err=110) fr_br_stars_x(i),fr_br_stars_y(i) 
      i = i+1 
      if(i.gt.n_max_stars) then 
          warning = .true.
          unit = 8
          call open_for_appending_file(unit,file_warning)
          write(unit,*)
     *          'there are more bright stars than n_max_stars'
          write(unit,*)
     *          'increase parameter nn_max_stars in main program'
          close(unit)
        goto 15 
      end if 
      goto 10 
   15 n_br_stars_org = i-1 
      i = n_br_stars_org/2 
      iseed = fr_br_stars_y(i)*100.d0 
      close(2) 
      
      return 

  100 icode = 300
      open(4,file=file_failed,status='unknown')
      write(4,6666) 'can not open file ',file_bright_stars_coord
 6666 format(a,a)
      close(4)
      return


  110 icode = 301
      open(4,file=file_failed,status='unknown')
      write(4,6666) 'problem to read star coordinates on ',
     *     file_bright_stars_coord
      write(4,*)    
     *      ' probably wrong value for n_skip_rec_br_file'
      write(4,*) 'n_skip_rec_br_file = ',n_skip_rec_br_file
      close(4)
      return

      end 
c************************************************************ 
      subroutine get_frame_parameters(icode,
     *          file_failed,
     *          file_stars_coord,
     *          n_skip_rec_br_file,
     *          n_skip_read_parms,
     *          pipeline,
     *          opt_ax_al_deg,
     *          opt_ax_del_deg,
     *          naxis1,naxis2,
     *          camera,
     *          chip_number,
     *          pix_scale,
     *          crpix1,crpix2,
     *          fr_size_al_deg,fr_size_del_deg)

      implicit NONE

      integer icode,i,nskip
      integer naxis1,naxis2,camera,chip_number
      integer n_skip_rec_br_file,n_skip_read_parms

      real*8 opt_ax_al_deg,opt_ax_del_deg
      real*8 pix_scale
      real*8 fr_size_al_deg,fr_size_del_deg
      real*8 crpix1,crpix2

      character*80 file_stars_coord
      character*19 camera_name
      character*80 file_failed

      logical pipeline

      icode = 0

      nskip = n_skip_read_parms
      camera = -1

      open(1,file=file_stars_coord,status='old',err=100)
      do i = 1,nskip
        read(1,*)
      end do
      read(1,2000,err=110) opt_ax_al_deg,opt_ax_del_deg
 2000 format(47x,2f11.5)
      read(1,*)
      if(pipeline) then
        read(1,2002,err=120) 
     *       pix_scale,chip_number,crpix1,crpix2,naxis1,naxis2,
     *               camera_name
        if(camera_name.eq.'MegaPrime') camera = 11
 2002 format(1x,f7.3,i4,2f10.2,2i6,1x,a)
      else
        read(1,2001,err=120) 
     *      pix_scale,chip_number,crpix1,crpix2,naxis1,naxis2,
     *               camera
 2001 format(1x,f7.3,i4,2f10.2,2i6,20x,i5,f7.1)
      end if

      if(pix_scale.le.0.d0) goto 150
      if(naxis1.le.0 .or. naxis2 .le.0) goto 160
      fr_size_al_deg = dble(naxis1)*pix_scale/3600.d0
      fr_size_del_deg = dble(naxis2)*pix_scale/3600.d0

      if(opt_ax_al_deg.lt.0.d0 .or. opt_ax_al_deg.gt. 360.d0)
     *       goto 180
      if(opt_ax_del_deg.gt.90.d0 .or. opt_ax_del_deg.lt.-90.d0)
     *       goto 190
      return

  100 icode = 100
      open(4,file=file_failed,status='unknown')
      write(4,6666)
     *   'ABORT: cannot open file '//
     *                  file_stars_coord
 6666 format(a,a)
      close(4)
      return

  110 icode = 101
      open(4,file=file_failed,status='unknown')
      write(4,6666)
     * 'ABORT: cannot read opt_ax_al_deg,opt_ax_del_deg on '//
     *   file_stars_coord
      close(4)
      return

  120 icode = 102
      open(4,file=file_failed,status='unknown')
      write(4,6666)
     * 'ABORT: cannot read pix_scale,chip_number etc. on '//
     *   file_stars_coord
      close(4)
      return

  150 icode = 103
      open(4,file=file_failed,status='unknown')
      write(4,6666)
     * 'ABORT:  read meaningless pix_scale on '//
     *   file_stars_coord 
      write(4,*) 'pix_scale = ',pix_scale
      close(4)
      return

  160 icode = 104
      open(4,file=file_failed,status='unknown')
      write(4,6666)
     * 'ABORT:  read meaningless naxis1,naxis2 on '//
     *   file_stars_coord
      write(4,*) 'naxis1 = ',naxis1
      write(4,*) 'naxis2 = ',naxis2
      close(4)
      return

  180 icode = 105
      open(4,file=file_failed,status='unknown')
      write(4,6666)
     * 'ABORT:  read meaningless opt_ax_al_deg on '//
     *   file_stars_coord
      write(4,*) 'opt_ax_al_deg = ',opt_ax_al_deg
      close(4)
      return


  190 icode = 106
      open(4,file=file_failed,status='unknown')
      write(4,6666)
     * 'ABORT:  read meaningless opt_ax_del_deg on '//
     *   file_stars_coord
      write(4,*) 'opt_ax_al_deg = ',opt_ax_del_deg
      close(4)
      return

      end
      subroutine get_identif_stars(icode, 
     *          file_failed,file_warning,warning,
     *          n_max_stars, 
     *          file_identified_objects, 
     *          fr_id_stars_all_x,fr_id_stars_all_y, 
     *          n_id_stars_org, 
     *          intensity,flux, 
     *          pipeline, 
     *          n_skip_rec_id_file) 


      implicit NONE 

      integer i,icode,n_id_stars_org,n_max_stars 
      integer n_skip_rec_id_file 
      integer unit
      integer intensity(n_max_stars) 

      real*8 fr_id_stars_all_x(n_max_stars) 
      real*8 fr_id_stars_all_y(n_max_stars) 
      real*8 flux(n_max_stars),x,y 

      character*80 file_identified_objects 
      character*80 file_failed,file_warning

      logical pipeline 
      logical warning

      icode = 0 


      open(1,file=file_identified_objects,status='old',err=100) 

      do i = 1,n_skip_rec_id_file 
        read(1,*) 
      end do 

      i = 1 
    5 continue 
      if(pipeline) then 
        read(1,*,end=10,err=110) 
     *           fr_id_stars_all_x(i),fr_id_stars_all_y(i), 
     *                   x,y,flux(i) 
      else 
        read(1,*,end=10,err=110) 
     *       fr_id_stars_all_x(i),fr_id_stars_all_y(i),
     *                   x,y,intensity(i) 
      end if 
      i = i+1 
      if(i.gt.n_max_stars) then
          warning = .true.
          unit = 3
          call open_for_appending_file(unit,file_warning)
          write(unit,*)
     *          'there are more bright stars than n_max_stars'
          write(unit,*)
     *          'increase parameter nn_max_stars in main program'
          close(unit)
        goto 10
      end if
      goto 5 
   10 close(1) 
      n_id_stars_org = i-1 
      return 

  100 icode = 400
      open(4,file=file_failed,status='unknown')
      write(4,6666) 'can not open file ',file_identified_objects
 6666 format(a,a)
      close(4)
      return


  110 icode = 401
      open(4,file=file_failed,status='unknown')
      write(4,6666) 'problem to read star coordinates on ',
     *     file_identified_objects
      write(4,*) 
     *      ' probably wrong value for n_skip_rec_id_file'
      write(4,*) 'n_skip_rec_id_file = ',n_skip_rec_id_file
      close(4)
      return

      end 
c************************************************************ 
      subroutine get_obj_stars(icode,
     *          n_max_stars,
     *          pipeline,
     *          file_all_objects,
     *          file_warning,warning,
     *          n_skip_rec_obj_file,
     *          fr_obj_stars_half_x,
     *          fr_obj_stars_half_y,
     *          n_obj_stars_half)

      implicit NONE

      integer n_obj_stars_max
      parameter(n_obj_stars_max=1000)

      integer icode,n_max_stars,n_skip_rec_obj_file
      integer n_obj_stars_half,n_obj_stars
      integer intensity(n_obj_stars_max)
      integer index_sort(n_obj_stars_max)
      integer i,j,nz,unit
      
      real*8 fr_obj_stars_half_x(n_max_stars)
      real*8 fr_obj_stars_half_y(n_max_stars)
      real*8 fr_x(n_obj_stars_max)
      real*8 fr_y(n_obj_stars_max)
      real*8 flux(n_obj_stars_max)

      character*80 file_all_objects
      character*80 file_warning

      logical pipeline,warning

      open(1,file=file_all_objects,status='old',err=100)
      do i = 1,n_skip_rec_obj_file
        read(1,*)
      end do

      i = 0
    5 i = i+1
      if(i.gt.n_obj_stars_max) then
          warning = .true.
          unit = 8
          call open_for_appending_file(unit,file_warning)
          write(unit,*)
     * 'increase parameter n_obj_stars_max in subroutine'
         write(unit,*)
     * '  get_obj_stars'
          close(unit)
        goto 10
      end if

      if(pipeline) then
        read(1,*,end=10,err=110)
     *           fr_x(i),fr_y(i),flux(i)
        intensity(i) = flux(i)
      else
        read(1,*,end=10,err=110)
     *       fr_x(i),fr_y(i),intensity(i)
      end if
      goto 5

   10 close(1)
      nz = i
      do i = 1,nz
        intensity(i) = -intensity(i)
      end do

      call sortinteger(nz,intensity,index_sort)

      n_obj_stars_half = nz/2
      if(n_obj_stars_half.gt.n_max_stars)
     *     n_obj_stars_half = n_max_stars

      do i = 1,n_obj_stars_half
        j = index_sort(i)
        fr_obj_stars_half_x(i) = fr_x(j)
        fr_obj_stars_half_y(i) = fr_y(j)
      end do

      return

  100 continue
      warning = .true.
      unit = 8
      call open_for_appending_file(unit,file_warning)
      write(unit,6666)
     * 'can not open file ',file_all_objects
 6666 format(a,a)
      close(unit)
      n_obj_stars_half = 0
      return

  110 continue
      close(1)
      warning = .true.
      unit = 8
      call open_for_appending_file(unit,file_warning)
      write(unit,6666)
     * 'can not read x,y coordinates on file',file_all_objects
      close(unit)
      n_obj_stars_half = 0
      return

      end

      subroutine get_run_parameters(icode,
     *             camera,
     *             number_monte_carlo_shoots,
     *             min_number_usno_stars_on_frame,
     *             min_percentage_match,
     *             min_percentage_match_first,
     *             n_match_first,
     *             max_camera_rot_deg,
     *             foc_length_err_promille,
     *             pointing_error_arcsec_nominal,
     *             pointing_error_arcsec_max,
     *             max_diff_fr_unso_arcsec,
     *             min_dist_among_stars_arsec,
     *             radius_0,pix_scale,
     *             delta_radius)

      implicit NONE

      integer icode,number_monte_carlo_shoots,camera
      integer min_number_usno_stars_on_frame
      integer n_match_first

      real*8       min_percentage_match,
     *             min_percentage_match_first,
     *             max_camera_rot_deg,
     *             foc_length_err_promille,
     *             pointing_error_arcsec_nominal,
     *             pointing_error_arcsec_max,
     *             max_diff_fr_unso_arcsec,
     *             min_dist_among_stars_arsec,
     *             radius_0,pix_scale,
     *             delta_radius

      icode = 0

c     main parameters
c     ---------------
      number_monte_carlo_shoots = 500
      min_number_usno_stars_on_frame = 30
      n_match_first = 15
      min_percentage_match = 30.d0
      min_percentage_match_first = 20.d0
      max_camera_rot_deg = 1.5d0
      foc_length_err_promille = 2.d0
      pointing_error_arcsec_max = 180.d0
c     pointing_error_arcsec_max = 600.d0

c     parameters for Monte-Carlo matching
c     ------------------------------------
      pointing_error_arcsec_nominal = 15.d0
      radius_0 = 4.0/pix_scale
      delta_radius = 3.0/pix_scale

c     parameter for plate solution
c     ----------------------------
      max_diff_fr_unso_arcsec = 3.d0

c     matching parameter
c     ------------------
      min_dist_among_stars_arsec = 10.d0

      return
      end
      subroutine get_usno_stars(icode, 
     *          file_failed,file_warning,warning,
     *          n_max_stars, 
     *          file_usno_read_from_cd, 
     *          usno_al_deg,usno_del_deg,usno_mag_r,usno_mag_b, 
     *          usno_ref_number, 
     *          n_usno_org, 
     *          number_min_usno_stars_on_frame, 
     *          fr_size_al_deg,fr_size_del_deg, 
     *          fr_center_al_deg,fr_center_del_deg) 

      implicit NONE 

      integer i,unit 
      integer icode,n_max_stars,n_usno_org 
      integer usno_ref_number(n_max_stars) 
      integer number_min_usno_stars_on_frame 

      real*8 usno_al_deg(n_max_stars),usno_del_deg(n_max_stars) 
      real*8 usno_mag_r(n_max_stars),usno_mag_b(n_max_stars) 
      real*8 fr_center_al_deg,fr_center_del_deg 
      real*8 fr_size_al_deg,fr_size_del_deg 
      real*8 size_al,size_del 

      logical warning

      character*80 file_usno_read_from_cd 
      character*80 file_failed,file_warning

      size_al = fr_size_al_deg/2.d0 
      size_del = fr_size_del_deg/2.d0 
      write(*,*) fr_center_al_deg, fr_center_del_deg
      call square(fr_center_al_deg,fr_center_del_deg, 
     *            size_al,size_del, 
     *            n_usno_org, 
     *            file_usno_read_from_cd) 
      if(n_usno_org.gt.n_max_stars) then 
          warning = .true.
          unit = 8
          call open_for_appending_file(unit,file_warning)
          write(unit,*) 
     *      'increase parameter nn_max_stars in main program' 
          write(unit,*) 'n_usno_org',n_usno_org 
          close(unit)
          n_usno_org = n_max_stars
      end if 

      if(n_usno_org.lt.number_min_usno_stars_on_frame) then 
        icode = 200
        open(4,file=file_failed,status='unknown')
        write(4,*) 'do not find enough USNO stars',n_usno_org 
        close(4)
        return 
      end if 

      open(1,file=file_usno_read_from_cd,status='old') 
      do i = 1,n_usno_org 
        read(1,*) usno_al_deg(i),usno_del_deg(i), 
     *            usno_mag_r(i),usno_mag_b(i),usno_ref_number(i) 
      end do 

      return 
      end 
c************************************************************ 
      subroutine improve_offset(icode, 
     *          n_max_stars, 
     *          iseed, 
     *          matching_coords,n_match, 
     *          pointing_error,iter_max, 
     *          camera_rotation_error, 
     *          foc_length_err_promille,
     *          xmin,xmax,ymin,ymax, 
     *          off_x,off_y, 
     *          dev_off_x,dev_off_y, 
     *          opt_ax_al_deg,opt_ax_del_deg, 
     *          focal_length,angle, 
     *          off_x_max,off_y_max,
     *          crpix1,crpix2) 

      implicit NONE 

      integer n_max_stars,icode,n_match 
      integer i,j,nz 
      integer iseed 
      integer iter,iter_max 

      real*8 usno_al_deg(n_max_stars) 
      real*8 usno_del_deg(n_max_stars) 
      real*8 fr_stars_x(n_max_stars) 
      real*8 fr_stars_y(n_max_stars) 
      real*8 opt_ax_al_deg_org,opt_ax_del_deg_org 
      real*8 focal_length_org 
      real*8 xmin,xmax,ymin,ymax
      real*8 off_x,off_y,dev_off_x,dev_off_y 
      real*8 opt_ax_al_deg,opt_ax_del_deg 
      real*8 focal_length,crpix1,crpix2 
      real*8 matching_coords_org(n_max_stars,6) 
      real*8 matching_coords(n_max_stars,6) 
      real*8 usno_pix_x(n_max_stars),usno_pix_y(n_max_stars) 
      real*8 x,y,al_deg,del_deg 
      real*8 crpix1_org,crpix2_org 
      real*8 pointing_error 
      real*8 dd_foc,dd_alpha,dd_delta 
      real*8 opt_ax_al_deg_min 
      real*8 opt_ax_al_deg_max,opt_ax_al_deg_best 
      real*8 opt_ax_del_deg_min 
      real*8 opt_ax_del_deg_max,opt_ax_del_deg_best 
      real*8 focal_length_best 
      real*8 focal_length_min,focal_length_max 
      real*8 off_x_best,off_y_best 
      real*8 off_x_max,off_y_max
      real*8 dev_off_x_best,dev_off_y_best 
      real*8 angle,angle_min,angle_max,dd_angle,angle_best 
      real*8 x_gnom,y_gnom 
      real*8 camera_rotation_error 
      real*8 foc_length_err_promille
      real*8 ran3,cosd,sind 
      external ran3,cosd,sind 

      icode = 0 

      opt_ax_al_deg_org = opt_ax_al_deg 
      opt_ax_del_deg_org = opt_ax_del_deg 
      focal_length_org = focal_length 
      crpix1_org = crpix1 
      crpix2_org = crpix2 

      do i = 1,n_match 
        do j = 1,6 
          matching_coords_org(i,j) = matching_coords(i,j) 
        end do 
      end do 

      angle_max = camera_rotation_error 
      angle_min = -camera_rotation_error 
      dd_angle = angle_max-angle_min 

      focal_length_min = focal_length_org-
     *           focal_length_org*foc_length_err_promille/1000.d0
      focal_length_max = focal_length_org+
     *           focal_length_org*foc_length_err_promille/1000.d0
      dd_foc = focal_length_max-focal_length_min 

      opt_ax_al_deg_min = 
     *       opt_ax_al_deg_org-pointing_error/3600.d0 
      opt_ax_al_deg_max = 
     *       opt_ax_al_deg_org+pointing_error/3600.d0 
      dd_alpha = 
     *       opt_ax_al_deg_max-opt_ax_al_deg_min 

      opt_ax_del_deg_min = 
     *       opt_ax_del_deg_org-pointing_error/3600.d0 
      opt_ax_del_deg_max = 
     *       opt_ax_del_deg_org+pointing_error/3600.d0 
      dd_delta = opt_ax_del_deg_max-opt_ax_del_deg_min 

      off_x_best = 1.d6 
      off_y_best = 1.d6 
      dev_off_x_best = 1.d6 
      dev_off_y_best = 1.d6 

      iter = 0 
    5 iter = iter+1 

      focal_length = focal_length_min+dd_foc*ran3(iseed) 
      opt_ax_al_deg = opt_ax_al_deg_min+dd_alpha*ran3(iseed) 
      opt_ax_del_deg = opt_ax_del_deg_min+dd_delta*ran3(iseed) 
      angle = angle_min+dd_angle*ran3(iseed) 

      do i = 1,n_match 
        usno_al_deg(i) = matching_coords_org(i,5) 
        usno_del_deg(i) = matching_coords_org(i,6) 
        fr_stars_x(i) = matching_coords_org(i,3) 
        fr_stars_y(i) = matching_coords_org(i,4) 
      end do 

      do i = 1,n_match 
        al_deg = usno_al_deg(i) 
        del_deg = usno_del_deg(i) 
        call equa_deg_to_xygnom(al_deg,del_deg, 
     *      opt_ax_al_deg,opt_ax_del_deg, 
     *      x_gnom,y_gnom) 
        x = -x_gnom*focal_length 
        y =  y_gnom*focal_length 
        usno_pix_x(i) =  x*cosd(angle)+y*sind(angle) 
     *                      + crpix1 
        usno_pix_y(i) = -x*sind(angle)+y*cosd(angle) 
     *                      +crpix2 
      end do 

      off_x = 0.d0 
      off_y = 0.d0 

      do i = 1,n_match 
        off_x = off_x+(usno_pix_x(i)-fr_stars_x(i)) 
        off_y = off_y+(usno_pix_y(i)-fr_stars_y(i)) 
      end do 
      off_x = off_x/dble(n_match) 
      off_y = off_y/dble(n_match) 

      dev_off_x = 0.d0 
      dev_off_y = 0.d0 

      do i = 1,n_match 
        dev_off_x = dev_off_x+ 
     *              (usno_pix_x(i)-fr_stars_x(i)-off_x)**2 
        dev_off_y = dev_off_y+ 
     *              (usno_pix_y(i)-fr_stars_y(i)-off_y)**2 
      end do 
      dev_off_x = dsqrt(dev_off_x)/dble(n_match-1) 
      dev_off_y = dsqrt(dev_off_y)/dble(n_match-1) 

      if(dev_off_x**2+dev_off_y**2 .lt. 
     *   dev_off_x_best**2+dev_off_y_best**2) then 
        off_x_best = off_x 
        off_y_best = off_y 
        dev_off_x_best = dev_off_x 
        dev_off_y_best = dev_off_y 
        opt_ax_al_deg_best = opt_ax_al_deg 
        opt_ax_del_deg_best = opt_ax_del_deg 
        focal_length_best = focal_length 
        angle_best = angle 
      end if 

      if(iter.le.iter_max) goto 5 

      off_x = 0.d0 
      off_y = 0.d0 
      dev_off_x = dev_off_x_best 
      dev_off_y = dev_off_y_best 
      opt_ax_al_deg = opt_ax_al_deg_best 
      opt_ax_del_deg = opt_ax_del_deg_best 
      focal_length = focal_length_best 
      crpix1 = crpix1_org-off_x_best 
      crpix2 = crpix2_org-off_y_best 
      angle = angle_best 

      xmin = 1.d6 
      xmax = -1.d6 
      ymin = 1.d6 
      ymax = -1.d6 
      off_x_max = -1.d6
      off_y_max = -1.d6
      do i = 1,n_match 
        al_deg = usno_al_deg(i) 
        del_deg = usno_del_deg(i) 
        call equa_deg_to_xygnom(al_deg,del_deg, 
     *      opt_ax_al_deg,opt_ax_del_deg, 
     *      x_gnom,y_gnom) 
        x = -x_gnom*focal_length 
        y =  y_gnom*focal_length 
        usno_pix_x(i) =  x*cosd(angle)+y*sind(angle) 
     *                     + crpix1 
        usno_pix_y(i) = -x*sind(angle)+y*cosd(angle) 
     *                     + crpix2 
        matching_coords(i,1) = usno_pix_x(i) 
        matching_coords(i,2) = usno_pix_y(i) 
        do j = 3,6 
          matching_coords(i,j) = matching_coords_org(i,j) 
        end do 
        xmin = dmin1(xmin,matching_coords(i,3)) 
        xmax = dmax1(xmax,matching_coords(i,3)) 
        ymin = dmin1(ymin,matching_coords(i,4)) 
        ymax = dmax1(ymax,matching_coords(i,4)) 
        off_x_max = dmax1(off_x_max,
     *              dabs(matching_coords(i,1)-matching_coords(i,3)))
        off_y_max = dmax1(off_y_max,
     *              dabs(matching_coords(i,2)-matching_coords(i,4)))
      end do 

      return 
      end 

c******************************************************** 
      subroutine make_fourth_order_plate_solution(icode, 
     *          file_failed,
     *          n_max_stars, 
     *          matching_coords,n_match, 
     *          opt_ax_al_deg,opt_ax_del_deg, 
     *          focal_length,angle, 
     *          max_diff_fr_unso, 
     *          plc, 
     *          crpix1,crpix2) 

      implicit NONE 

      integer icode,n_max_stars,n_match,number_stars 
      integer i,j,l,n_elim,nz,k 
      integer nn_max,ierr 
      integer iter_eliminate_stars_max 
      integer iter_eliminate_stars 

      real*8 opt_ax_al_deg,opt_ax_del_deg 
      real*8 focal_length,angle,crpix1,crpix2 
      real*8 matching_coords(n_max_stars,6) 
      real*8 matching_coords_copy(n_max_stars,6) 
      real*8 usno_gnom_x(n_max_stars) 
      real*8 usno_gnom_y(n_max_stars) 
      real*8 fr_gnom_x(n_max_stars) 
      real*8 fr_gnom_y(n_max_stars) 
      real*8 x,y,al_deg,del_deg,x_gnom,y_gnom 
      real*8 xx(2*n_match),xx1(2*n_match), 
     *       t(2*n_match,30),t1(2*n_match,30) 
      real*8 w(30),plc(30),rv(30),test(2*n_match),pplc(30,1) 
      real*8 kn,sigma,sigma_2 
      real*8 max_diff_fr_unso 
      real*8 pie,pierez 
      real*8 diff_al_arcsec,diff_del_arcsec 
      real*8 dd,delta_cand,alpha_cand,ttan,da 
      real*8 x_ast,y_ast,zeta,eta,alphaz_deg,deltaz_deg 

      real*8 cosd,sind,tand 
      external cosd,sind,tand 

      logical participate(n_max_stars) 
      logical star_elimination 

      character*80 file_failed

      pie = dasin(1.d0)*2. 
      pierez = pie/180.d0 

      icode = 0 

      do i = 1,n_match 
        do j = 1,6 
           matching_coords_copy(i,j) = matching_coords(i,j) 
        end do 
      end do 

      iter_eliminate_stars = -1 
      iter_eliminate_stars_max = 10

      do i = 1,n_match 
        participate(i) = .true. 
      end do 

   10 continue 
      iter_eliminate_stars = iter_eliminate_stars+1 
      number_stars = 0 
      do i = 1,n_match 
      if(participate(i)) then 
        number_stars = number_stars+1 
        al_deg = matching_coords(i,5) 
        del_deg = matching_coords(i,6) 
        call equa_deg_to_xygnom(al_deg,del_deg, 
     *      opt_ax_al_deg,opt_ax_del_deg, 
     *      x_gnom,y_gnom) 
        usno_gnom_x(i) = x_gnom 
        usno_gnom_y(i) = y_gnom 

        x = matching_coords(i,3)-crpix1 
        y = matching_coords(i,4)-crpix2 
        fr_gnom_x(i) =-(x*cosd(-angle)+y*sind(-angle))/focal_length 
        fr_gnom_y(i) = (-x*sind(-angle)+y*cosd(-angle))/focal_length 

      end if 
      end do 

      if(number_stars.lt.15) then 
        icode = 400
        open(4,file=file_failed,status='unknown')
        write(4,*) 'not enough stars to make 4th order plate solution'
        write(4,*) 'number_stars = ',number_stars
        return 
      end if 

c     initialisation of plate constant matrix 
c     --------------------------------------- 
      do i=1,2*number_stars 
        do j=1,30 
          t(i,j) = 0.d0 
        end do 
      end do 
      sigma_2 = 0.d0 
      sigma =0.d0 

c     construct plate constant matrix 
c     ------------------------------- 
      do i = 1,number_stars 
        t(i,1) =1 
        t(i,2) = fr_gnom_x(i) 
        t(i,3) = fr_gnom_y(i) 
        xx(i) = usno_gnom_x(i) 
        xx(i+number_stars) = usno_gnom_y(i) 
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

      nn_max = n_match 

c     solve for plate constants 
c     ------------------------- 
      call minfitpack (2*nn_max,2*number_stars,30,t1,w,1,xx1,ierr,rv, 
     *  pplc,30,kn) 
      if(ierr.ne.0) then 
        icode = 404
        open(4,file=file_failed,status='unknown')
        write(4,*) 'minfitpack failed'
        write(4,*) 'n_match = ',n_match
        return
      end if 

      do i = 1,30 
        plc(i) = pplc(i,1) 
      end do 

c     residuals 
c     ---------- 
      call dproduit (t,pplc,test,2*nn_max,30,1,2*number_stars,30,1) 

      do i=1,2*number_stars 
        sigma_2 = sigma_2 + ((xx(i)-test(i))**2) 
      end do 
      sigma_2 = sigma_2/dfloat(2*number_stars-30) 
      sigma = dsqrt(sigma_2) 

      star_elimination = .false. 
      do i = 1,number_stars 
        participate(i) = .true. 
      end do 

      do i = 1,number_stars 
        if (dabs(xx(i)-test(i)).gt.(3.*sigma).or. 
     *     dabs(xx(i+number_stars)-test(i+number_stars)).gt. 
     *                (3.*sigma) ) then 
          star_elimination = .true. 
          participate(i) = .false. 
        end if 
      end do 

      if(.not.star_elimination) goto 100 
      if(iter_eliminate_stars.gt.iter_eliminate_stars_max) 
     *         go to 100 
      goto 10 

  100 continue 


      n_elim = 0 
      do i = 1,n_match 
        if(.not.participate(i)) then 
          n_elim = n_elim+1 
        end if 
      end do 

      nz = 0 
      do i = 1,n_match 
        if(participate(i)) then 
          nz = nz+1 
          do j = 1,6 
            matching_coords(nz,j) = matching_coords_copy(i,j) 
          end do 
        end if 
      end do 
      n_match = nz 

c     consistency check: apply 4-th order solution to matched frame stars 
c     ------------------------------------------------------------------- 
      icode = 0 
      do i = 1,n_match 
        do j = 1,6 
          matching_coords_copy(i,j) = matching_coords(i,j) 
        end do 
      end do 

      do i = 1,n_match 
        participate(i) = .true. 
      end do 

      do i = 1,n_match 
        x = matching_coords(i,3)-crpix1 
        y = matching_coords(i,4)-crpix2 
        x_ast =-(x*cosd(-angle)+y*sind(-angle))/focal_length 
        y_ast = (-x*sind(-angle)+y*cosd(-angle))/focal_length 

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
         alphaz_deg = opt_ax_al_deg 
         deltaz_deg = opt_ax_del_deg 


         da = atan(zeta/(cosd(deltaz_deg)-eta*sind(deltaz_deg))) 
         da = da/pierez 
         alpha_cand  = alphaz_deg + da 

         ttan   = tand((alpha_cand - alphaz_deg)/2.) 
         dd =       asin((eta-(zeta*sind(deltaz_deg)*ttan))/ 
     *            dsqrt(zeta**2+eta**2+1 )) 
         dd = dd/pierez 
         delta_cand = deltaz_deg + dd 
         diff_al_arcsec = (alpha_cand-matching_coords(i,5))*3600.d0 
         diff_del_arcsec = (delta_cand-matching_coords(i,6))*3600.d0 
c        write(6,*) i,diff_al_arcsec,diff_del_arcsec 
         if(dabs(diff_al_arcsec).gt.max_diff_fr_unso .or. 
     *      dabs(diff_del_arcsec).gt.max_diff_fr_unso) then 
           participate(i) = .false. 
        end if 
      end do 

      nz = 0 
      do i = 1,n_match 
        if(participate(i)) then 
          nz = nz+1 
          do j = 1,6 
            matching_coords(nz,j) = matching_coords_copy(i,j) 
          end do 
        end if 
      end do 
      n_match = nz 

      if(n_match.lt.15) then
        icode = 402
        open(4,file=file_failed,status='unknown')
        write(4,*) 'not enough stars to make 4th order plate solution'
        write(4,*) 'n_match = ',n_match
        return
      end if


      return 
      end 
c******************************************************** 
      subroutine match_more_stars(icode, 
     *          n_max_stars, 
     *          iseed, 
     *          matching_coords_best,n_match, 
     *          percentage_min_match, 
     *          pointing_error,iter_max,min_dist_among_stars, 
     *          camera_rotation_error,angle, 
     *          xmin,xmax,ymin,ymax, 
     *          off_x,off_y, 
     *          dev_off_x,dev_off_y, 
     *          usno_al_deg,usno_del_deg, 
     *          n_usno_org, 
     *          fr_stars_x,fr_stars_y, 
     *          n_fr_stars_org, 
     *          opt_ax_al_deg,opt_ax_del_deg, 
     *          focal_length, 
     *          pix_scale, 
     *          crpix1,crpix2) 

      implicit NONE 

      integer n_max_stars,n_usno_org,n_fr_stars_org 
      integer i,j,n,n_match,nz,match,im,jm,n_match_org 
      integer icode 
      integer match_table(n_max_stars,2) 
      integer n_match_best 
      integer iseed,iter,iter_max 

      real*8 fr_stars_x(n_max_stars) 
      real*8 fr_stars_y(n_max_stars) 
      real*8 usno_al_deg(n_max_stars) 
      real*8 usno_del_deg(n_max_stars) 
      real*8 usno_pix_x(n_max_stars) 
      real*8 usno_pix_y(n_max_stars) 
      real*8 matching_coords_best(n_max_stars,6) 
      real*8 opt_ax_al_deg_org,opt_ax_del_deg_org 
      real*8 focal_length_org,crpix1,crpix2 
      real*8 al_deg,del_deg,x_gnom,y_gnom,x,y 
      real*8 opt_ax_al_deg,opt_ax_del_deg 
      real*8 focal_length 
      real*8 opt_ax_al_deg_1,opt_ax_al_deg_min 
      real*8 opt_ax_al_deg_max,opt_ax_al_deg_best 
      real*8 opt_ax_del_deg_1,opt_ax_del_deg_min 
      real*8 opt_ax_del_deg_max,opt_ax_del_deg_best 
      real*8 focal_length_1,focal_length_best 
      real*8 focal_length_min,focal_length_max 
      real*8 dd_foc,dd_alpha,dd_delta 
      real*8 percentage_min_match 
      real*8 off_x_org,off_y_org 
      real*8 off_x_best,off_y_best 
      real*8 dev_off_x_best,dev_off_y_best 
      real*8 off_x,off_y,dev_off_x,dev_off_y 
      real*8 xmin,xmax,xmin_best,xmax_best
      real*8 ymin,ymax,ymin_best,ymax_best 
      real*8 pix_scale 
      real*8 pointing_error 
      real*8 min_dist_among_stars,dist 
      real*8 percentage_match 
      real*8 camera_rotation_error 
      real*8 angle,angle_min,angle_max,dd_angle,angle_best 
      real*8 cosd,sind 
      external cosd,sind 
      real*8 ran3 
      external ran3 

      logical participate_fr(n_max_stars) 
      logical participate_usno(n_max_stars) 
      logical double_match_usno(n_max_stars) 
      logical double_match_fr(n_max_stars) 


      icode = 0 

      n_match_org = n_match 
      focal_length_org = focal_length 
      opt_ax_al_deg_org = opt_ax_al_deg 
      opt_ax_del_deg_org = opt_ax_del_deg 
      off_x_org = off_x 
      off_y_org = off_y 


      do i = 1,n_fr_stars_org 
        participate_fr(i) = .true. 
      end do 

      do i = 1,n_fr_stars_org-1 
        do j = i+1,n_fr_stars_org 
          dist = dsqrt( 
     *                 (fr_stars_x(i)-fr_stars_x(j))**2 
     *                +(fr_stars_y(i)-fr_stars_y(j))**2) 
          if(dist.lt.1.1*min_dist_among_stars) then 
            participate_fr(i) = .false. 
            participate_fr(j) = .false. 
          end if 
        end do 
      end do 

      do i = 1,n_usno_org 
        participate_usno(i) = .true. 
      end do 

      do i = 1,n_usno_org 
        al_deg = usno_al_deg(i) 
        del_deg = usno_del_deg(i) 
        call equa_deg_to_xygnom(al_deg,del_deg, 
     *      opt_ax_al_deg_org,opt_ax_del_deg_org, 
     *      x_gnom,y_gnom) 
        x = -x_gnom*focal_length 
        y =  y_gnom*focal_length 
        usno_pix_x(i) =  x*cosd(angle)+y*sind(angle) 
     *                      + crpix1 
        usno_pix_y(i) = -x*sind(angle)+y*cosd(angle) 
     *                      +crpix2 
      end do 

      do i = 1,n_usno_org-1 
        do j = i+1,n_usno_org 
          dist = dsqrt( 
     *                 (usno_pix_x(i)-usno_pix_x(j))**2 
     *                +(usno_pix_y(i)-usno_pix_y(j))**2) 
          if(dist.lt.1.1*min_dist_among_stars) then 
            participate_usno(i) = .false. 
            participate_usno(j) = .false. 
          end if 
        end do 
      end do 

      focal_length_1 = focal_length_org 

      opt_ax_al_deg_1 = opt_ax_al_deg_org- 
     *    off_x_org*pix_scale/3600.d0/cosd(opt_ax_del_deg_org) 
      opt_ax_del_deg_1 = 
     *    opt_ax_del_deg_org+off_y_org*pix_scale/3600.d0 

c     use matched stars to improve offset 
c     ----------------------------------- 


      angle_max = camera_rotation_error 
      angle_min = -camera_rotation_error 
      dd_angle = angle_max-angle_min 

      focal_length_min = focal_length_1-focal_length/400.d0 
      focal_length_max = focal_length_1+focal_length/400.d0 
      dd_foc = focal_length_max-focal_length_min 

      opt_ax_al_deg_min = 
     *       opt_ax_al_deg_1-pointing_error/3600.d0 
      opt_ax_al_deg_max = 
     *       opt_ax_al_deg_1+pointing_error/3600.d0 
      dd_alpha = 
     *       opt_ax_al_deg_max-opt_ax_al_deg_min 

      opt_ax_del_deg_min = 
     *       opt_ax_del_deg_1-pointing_error/3600.d0 
      opt_ax_del_deg_max = 
     *       opt_ax_del_deg_1+pointing_error/3600.d0 
        dd_delta = opt_ax_del_deg_max-opt_ax_del_deg_min 

      iter = 0 

      n_match_best = 0 

    5 iter = iter+1 

      focal_length = focal_length_min+dd_foc*ran3(iseed) 
      opt_ax_al_deg = opt_ax_al_deg_min+dd_alpha*ran3(iseed) 
      opt_ax_del_deg = opt_ax_del_deg_min+dd_delta*ran3(iseed) 
      angle = angle_min+dd_angle*ran3(iseed) 

c     match ref stars 
c     --------------- 
      do i = 1,n_usno_org 
        al_deg = usno_al_deg(i) 
        del_deg = usno_del_deg(i) 
        call equa_deg_to_xygnom(al_deg,del_deg, 
     *      opt_ax_al_deg,opt_ax_del_deg, 
     *      x_gnom,y_gnom) 
        x = -x_gnom*focal_length 
        y =  y_gnom*focal_length 
        usno_pix_x(i) =  x*cosd(angle)+y*sind(angle) 
     *                      + crpix1 
        usno_pix_y(i) = -x*sind(angle)+y*cosd(angle) 
     *                      +crpix2 
      end do 

      xmin = 1.d10 
      xmax = -1.d10 
      ymin = 1.d10 
      ymax = -1.d10 
      nz = 0 
      do i = 1,n_usno_org 
      if(participate_usno(i)) then 
        match = 0 
        do j = 1,n_fr_stars_org 
        if(participate_fr(j)) then 
          dist = dsqrt( 
     *            (usno_pix_x(i)-fr_stars_x(j))**2 
     *           +(usno_pix_y(i)-fr_stars_y(j))**2) 
          if(dist.lt.min_dist_among_stars) then 
            match = match+1 
            im = i 
            jm = j 
          end if 
        end if 
        end do 
        if(match.eq.1) then 
          nz = nz+1 
          match_table(nz,1) = im 
          match_table(nz,2) = jm 
          xmin = dmin1(xmin,fr_stars_x(jm)) 
          xmax = dmax1(xmax,fr_stars_x(jm)) 
          ymin = dmin1(ymin,fr_stars_y(jm)) 
          ymax = dmax1(ymax,fr_stars_y(jm)) 
        end if 
      end if 
      end do 
c     check for double matching 
c     ------------------------- 
      n = nz 
      if(nz.eq.0) goto 3 
      do i = 1,n_usno_org 
        double_match_usno(i) = .false. 
      end do 
      do i = 1,n-1 
        im = match_table(i,1) 
        do j = i+1,n 
          if(im.eq.match_table(j,1)) then 
            double_match_usno(im) = .true. 
          end if 
        end do 
      end do 
      do i = 1,n_fr_stars_org 
        double_match_fr(i) = .false. 
      end do 
      do i = 1,n-1 
        jm = match_table(i,2) 
        do j = i+1,n 
          if(jm.eq.match_table(j,2)) then 
            double_match_fr(jm) = .true. 
          end if 
        end do 
      end do 
      nz = 0 
      do i = 1,n 
        im = match_table(i,1) 
        jm = match_table(i,2) 
        if(double_match_usno(im) .or. 
     *     double_match_fr(jm)) goto 2 
        nz = nz+1 
    2   continue 
      end do 

    3 continue 

      if(nz.gt.n_match_best) then 
        j = 0 
        do i = 1,n 
          im = match_table(i,1) 
          jm = match_table(i,2) 
          if(double_match_usno(im) .or. 
     *     double_match_fr(jm)) goto 4 
          j = j+1 
          matching_coords_best(j,1) = usno_pix_x(im) 
          matching_coords_best(j,2) = usno_pix_y(im) 
          matching_coords_best(j,3) = fr_stars_x(jm) 
          matching_coords_best(j,4) = fr_stars_y(jm) 
          matching_coords_best(j,5) = usno_al_deg(im) 
          matching_coords_best(j,6) = usno_del_deg(im) 
    4   continue 
        end do 
        off_x = 0.d0 
        off_y = 0.d0 
        do i = 1,nz 
          off_x = off_x+(matching_coords_best(i,1)- 
     *                   matching_coords_best(i,3)) 
          off_y = off_y+(matching_coords_best(i,2)- 
     *                   matching_coords_best(i,4)) 
        end do 
        off_x = off_x/dble(nz) 
        off_y = off_y/dble(nz) 

        dev_off_x = 0.d0 
        dev_off_y = 0.d0 
        do i = 1,nz 
          dev_off_x = dev_off_x+ 
     *      (matching_coords_best(i,1)- 
     *        matching_coords_best(i,3)-off_x)**2 
          dev_off_y = dev_off_y+ 
     *      (matching_coords_best(i,2)- 
     *        matching_coords_best(i,4)-off_x)**2 
        end do 
        dev_off_x = dsqrt(dev_off_x)/dble(nz-1) 
        dev_off_y = dsqrt(dev_off_y)/dble(nz-1) 

        n_match_best = nz 
        xmin_best = xmin 
        xmax_best = xmax 
        ymin_best = ymin 
        ymax_best = ymax 
        opt_ax_al_deg_best = opt_ax_al_deg 
        opt_ax_del_deg_best = opt_ax_del_deg 
        focal_length_best = focal_length 
        angle_best = angle 
        dev_off_x_best = dev_off_x 
        dev_off_y_best = dev_off_y 
        off_x_best = off_x 
        off_y_best = off_y 

      end if 

      if(iter.lt.iter_max) goto 5 

      off_x = off_x_best 
      off_y = off_y_best 
      dev_off_x = dev_off_x_best 
      dev_off_y = dev_off_y_best 
      xmin = xmin_best 
      xmax = xmax_best 
      ymin = ymin_best 
      ymax = ymax_best 

      opt_ax_al_deg = opt_ax_al_deg_best- 
     *    off_x_best*pix_scale/3600.d0/cosd(opt_ax_del_deg_best) 
      opt_ax_del_deg = 
     *    opt_ax_del_deg_best+off_y_best*pix_scale/3600.d0 
      focal_length = focal_length_best 
      angle = angle_best 
      n_match = n_match_best 
      percentage_match = 
     *      dble(n_match)/dble(n_usno_org)*100.d0 

      return 
      end 
c****************************************************************** 
      subroutine match_using_monte_carlo(icode, 
     *          matching_coords,n_match, 
     *          pointing_error_1,pointing_error_2,iter_max, 
     *          camera_rotation_error,angle, 
     *          foc_length_err_promille,
     *          percentage_min_match, 
     *          xmin,xmax,ymin,ymax, 
     *          off_x,off_y,dev_off_x,dev_off_y, 
     *          radius_0,delta_radius, 
     *          iseed, 
     *          n_max_stars, 
     *          usno_al_deg,usno_del_deg, 
     *          n_usno_org, 
     *          fr_stars_x,fr_stars_y, 
     *          n_fr_stars_org, 
     *          opt_ax_al_deg_org, 
     *          opt_ax_del_deg_org, 
     *          focal_length_org, 
     *          naxis1,naxis2,
     *          opt_ax_al_deg,opt_ax_del_deg, 
     *          focal_length, 
     *          crpix1,crpix2) 

      implicit NONE 

      integer iter_tol_max,iiter_tol_max 
      parameter(iter_tol_max=6) 

      integer n_max_stars,n_usno_org,n_fr_stars_org 
      integer i,j,ic,nnz,nnz_max 
      integer icode 
      integer iseed 
      integer naxis1,naxis2
      integer nz_fr
      integer iter,iter_max,n_match 
      integer n_match_best(iter_tol_max) 
      integer n_match_indiv 
      integer iter_tol 

      real*8 fr_stars_x(n_max_stars) 
      real*8 fr_stars_y(n_max_stars) 
      real*8 usno_al_deg(n_max_stars) 
      real*8 usno_al_deg_match 
      real*8 usno_del_deg(n_max_stars) 
      real*8 usno_del_deg_match 
      real*8 usno_pix_x(n_max_stars) 
      real*8 usno_pix_y(n_max_stars) 
      real*8 usno_pix_x_match,usno_pix_y_match 
      real*8 fr_stars_x_match,fr_stars_y_match 
      real*8 matching_coords(n_max_stars,6) 
      real*8 best_matching_coords(n_max_stars,6,iter_tol_max) 
      real*8 opt_ax_al_deg_org,opt_ax_del_deg_org 
      real*8 focal_length_org,crpix1,crpix2 
      real*8 al_deg,del_deg,x_gnom,y_gnom,x,y 
      real*8 opt_ax_al_deg,opt_ax_del_deg 
      real*8 focal_length 
      real*8 focal_length_best(iter_tol_max) 
      real*8 opt_ax_al_deg_best(iter_tol_max) 
      real*8 opt_ax_del_deg_best(iter_tol_max) 
      real*8 focal_length_min,focal_length_max 
      real*8 opt_ax_al_deg_min,opt_ax_al_deg_max 
      real*8 opt_ax_del_deg_min,opt_ax_del_deg_max 
      real*8 dd_foc,dd_alpha,dd_delta 
      real*8 dist 
      real*8 dist_usno(n_usno_org,n_usno_org) 
      real*8 dist_fr(n_fr_stars_org,n_fr_stars_org) 
      real*8 min_dist_among_stars_pix 
      real*8 radius,delta_radius,radius_0 
      real*8 percentage_match,percentage_min_match 
      real*8 dx,dy,dev_x,dev_y 
      real*8 offset_x_pix(iter_tol_max) 
      real*8 offset_y_pix(iter_tol_max) 
      real*8 dev_offset_x_pix(iter_tol_max) 
      real*8 dev_offset_y_pix(iter_tol_max) 
      real*8 pointing_error,foc_length_err_promille 
      real*8 pointing_error_1,pointing_error_2
      real*8 theta,dd_pointing_error
      real*8 xmin,xmax,ymin,ymax 
      real*8 off_x,off_y,dev_off_x,dev_off_y 
      real*8 camera_rotation_error,angle,angle_best(iter_tol_max) 
      real*8 angle_max,angle_min,dd_angle 
      real*8 sind,cosd 
      external sind,cosd 

      real*8 ran3 
      external ran3 

      logical participate_fr(n_max_stars) 
      logical participate_usno(n_max_stars) 
      logical participate_usno_2(n_max_stars) 



      icode = 0 

      iiter_tol_max = iter_tol_max 

      do i = 1,iiter_tol_max 
        opt_ax_al_deg_best(i) = 0.d0 
        opt_ax_del_deg_best(i) = 0.d0 
        n_match_best(i) = 0 
        offset_x_pix(i) = 0.d0 
        offset_y_pix(i) = 0.d0 
        dev_offset_x_pix(i) = 0.d0 
        dev_offset_y_pix(i) = 0.d0 
        focal_length_best(i) = 0.d0 
        angle_best(i) = 0.d0 
      end do 

      do i = 1,n_fr_stars_org-1 
        do j = i+1,n_fr_stars_org 
          dist_fr(i,j) = dsqrt( 
     *                 (fr_stars_x(i)-fr_stars_x(j))**2 
     *                +(fr_stars_y(i)-fr_stars_y(j))**2) 
          dist_fr(j,i) = dist_fr(i,j) 
        end do 
      end do 

        do i = 1,n_usno_org 
          al_deg = usno_al_deg(i) 
          del_deg = usno_del_deg(i) 
          call equa_deg_to_xygnom(al_deg,del_deg, 
     *      opt_ax_al_deg_org,opt_ax_del_deg_org, 
     *      x_gnom,y_gnom) 
          x = -x_gnom*focal_length_org+crpix1 
          y =  y_gnom*focal_length_org+crpix2 
          usno_pix_x(i) = x 
          usno_pix_y(i) = y 
 2000 format(2f10.1,2f15.7) 
        end do 

      do i = 1,n_usno_org-1 
        do j = i+1,n_usno_org 
          dist_usno(i,j) = dsqrt( 
     *                 (usno_pix_x(i)-usno_pix_x(j))**2 
     *                +(usno_pix_y(i)-usno_pix_y(j))**2) 
          dist_usno(j,i) = dist_usno(i,j) 
        end do 
      end do 

      radius = radius_0-delta_radius 

      do iter_tol = 1,iiter_tol_max 
c     write(6,*) 'iter_tol',iter_tol

        radius = radius+delta_radius 
        min_dist_among_stars_pix = 2.d0*radius 

        do i = 1,n_fr_stars_org 
          participate_fr(i) = .true. 
        end do 

        do i = 1,n_fr_stars_org-1 
          do j = i+1,n_fr_stars_org 
            if(dist_fr(i,j).lt. 
     *             min_dist_among_stars_pix) then 
              participate_fr(i) = .false. 
              participate_fr(j) = .false. 
            end if 
          end do 
        end do 

        nz_fr = 0 
        do i = 1,n_fr_stars_org 
          if(participate_fr(i)) nz_fr = nz_fr+1 
        end do 

        do i = 1,n_usno_org 
          participate_usno_2(i) = .true. 
        end do 

        do i = 1,n_usno_org-1 
          do j = i+1,n_usno_org 
            if(dist_usno(i,j).lt. 
     *          min_dist_among_stars_pix) then 
              participate_usno_2(i) = .false. 
              participate_usno_2(j) = .false. 
            end if 
          end do 
        end do 

        angle_max = camera_rotation_error 
        angle_min = -camera_rotation_error 
        dd_angle = angle_max-angle_min 

        focal_length_min = focal_length_org-
     *          focal_length*foc_length_err_promille/1000.d0
        focal_length_max = focal_length_org+
     *          focal_length*foc_length_err_promille/1000.d0
        dd_foc = focal_length_max-focal_length_min 

        dd_pointing_error = pointing_error_2-pointing_error_1

        n_match_best(iter_tol) = 0 
        iter = 0
c       nnz_max = 0

    5   iter = iter+1 

        focal_length = focal_length_min+dd_foc*ran3(iseed) 
        pointing_error = 
     *      pointing_error_1+dd_pointing_error*ran3(iseed)
        theta = 360.d0*ran3(iseed)
        dd_alpha = pointing_error*cosd(theta)
        dd_delta = pointing_error*sind(theta)
        opt_ax_al_deg = opt_ax_al_deg_org+dd_alpha/3600.d0
        opt_ax_del_deg = opt_ax_del_deg_org+dd_delta/3600.d0
        angle = angle_min+dd_angle*ran3(iseed) 

        do i = 1,n_usno_org
          participate_usno(i) = participate_usno_2(i)
        end do
        do i = 1,n_usno_org 
          al_deg = usno_al_deg(i) 
          del_deg = usno_del_deg(i) 
          call equa_deg_to_xygnom(al_deg,del_deg, 
     *      opt_ax_al_deg,opt_ax_del_deg, 
     *      x_gnom,y_gnom) 
          x = -x_gnom*focal_length 
          y =  y_gnom*focal_length 
          usno_pix_x(i) =  x*cosd(angle)+y*sind(angle) 
     *                      + crpix1 
          usno_pix_y(i) = -x*sind(angle)+y*cosd(angle) 
     *                      +crpix2 
          if(usno_pix_x(i).lt.0. .or.
     *       usno_pix_y(i).lt.0. .or.
     *       usno_pix_x(i).gt.dble(naxis1) .or.
     *       usno_pix_y(i).gt.dble(naxis2)) 
     *         participate_usno(i) = .false.
        end do 
c       nnz = 0
c       do i = 1,n_usno_org
c         if(participate_usno(i)) nnz = nnz+1
c       end do
c       nnz_max= max(nnz_max,nnz)

        n_match = 0 
        do i = 1,n_usno_org 
          if(participate_usno(i)) then 
            n_match_indiv = 0 
            do j = 1,n_fr_stars_org 
              if(participate_fr(j)) then 
                dist = dsqrt( 
     *                (usno_pix_x(i)-fr_stars_x(j))**2 
     *               +(usno_pix_y(i)-fr_stars_y(j))**2) 
                if(usno_pix_x(i).gt.fr_stars_x(j)  .and. 
     *             usno_pix_y(i).gt.fr_stars_y(j)  .and. 
     *             dist.lt.radius) then 
                  n_match_indiv = n_match_indiv+1 
                  usno_pix_x_match = usno_pix_x(i) 
                  usno_pix_y_match = usno_pix_y(i) 
                  fr_stars_x_match = fr_stars_x(j) 
                  fr_stars_y_match = fr_stars_y(j) 
                  usno_al_deg_match = usno_al_deg(i) 
                  usno_del_deg_match = usno_del_deg(i) 
                end if 
              end if 
            end do 
            if(n_match_indiv.eq.1)then 
              n_match = n_match+1 
              matching_coords(n_match,1) = usno_pix_x_match 
              matching_coords(n_match,2) = usno_pix_y_match 
              matching_coords(n_match,3) = fr_stars_x_match 
              matching_coords(n_match,4) = fr_stars_y_match 
            end if 
          end if 
   10     continue 
        end do 

        if(n_match.gt.n_match_best(iter_tol)) then 
          n_match_best(iter_tol) = n_match 
          opt_ax_al_deg_best(iter_tol) = opt_ax_al_deg 
          opt_ax_del_deg_best(iter_tol) = opt_ax_del_deg 
          focal_length_best(iter_tol) = focal_length 
          angle_best(iter_tol) = angle 
          do i = 1,n_match 
            do j = 1,6 
            best_matching_coords(i,j,iter_tol) = 
     *       matching_coords(i,j) 
            end do 
          end do 
          percentage_match = 
     *      dble(n_match)/dble(n_usno_org)*100.d0 
          if(percentage_match.gt.percentage_min_match) goto 15 
        end if 

        if(iter.lt.iter_max) goto 5 

   15   continue 

        dx = 0.d0 
        dy = 0.d0 

        j = n_match_best(iter_tol) 
        if(j.eq.0) goto 20 
        do i = 1,j 
          dx = dx+ 
     *       (best_matching_coords(i,1,iter_tol)- 
     *        best_matching_coords(i,3,iter_tol)) 
          dy = dy+ 
     *       (best_matching_coords(i,2,iter_tol)- 
     *        best_matching_coords(i,4,iter_tol)) 
        end do 
        dx = dx/dble(n_match_best(iter_tol)) 
        dy = dy/dble(n_match_best(iter_tol)) 

        dev_x = 0.d0 
        dev_y = 0.d0 
        j = n_match_best(iter_tol) 
        if(j.gt.10) then 
          do i = 1,j 
            dev_x = dev_x+ 
     *       (best_matching_coords(i,1,iter_tol)- 
     *        best_matching_coords(i,3,iter_tol)-dx)**2 
            dev_y = dev_y+ 
     *       (best_matching_coords(i,2,iter_tol)- 
     *        best_matching_coords(i,4,iter_tol)-dy)**2 
        end do 
        dev_x = dsqrt(dev_x)/dble(n_match_best(iter_tol)) 
        dev_y = dsqrt(dev_y)/dble(n_match_best(iter_tol)) 

        offset_x_pix(iter_tol) = dx 
        offset_y_pix(iter_tol) = dy 
        dev_offset_x_pix(iter_tol) = dev_x 
        dev_offset_y_pix(iter_tol) = dev_y 
        end if 

   20   continue 

        if(iter_tol.gt.1) then 
          if(n_match_best(iter_tol).lt. 
     *         n_match_best(iter_tol-1)) goto 30 
        end if 
c       write(6,*) 'nnz_max',nnz_max
      end do 

   30 continue 

      n_match = 0 
      do i = 1,iiter_tol_max 
        n_match = max(n_match,n_match_best(i)) 
      end do 
      do i = 1,iiter_tol_max 
        ic = i 
        if(n_match.eq.n_match_best(i)) goto 70 
      end do 
      icode = 10 
      return 

   70 continue 

      do i = 1,n_match 
          matching_coords(i,1) = 
     *        best_matching_coords(i,1,ic)-offset_x_pix(ic) 
          matching_coords(i,2) = 
     *        best_matching_coords(i,2,ic)-offset_y_pix(ic) 
        do j = 3,4 
          matching_coords(i,j) = best_matching_coords(i,j,ic) 
        end do 
      end do 
      xmin = 10000.d0 
      xmax = -10000.d0 
      ymin = 10000.d0 
      ymax = -10000.d0 
      do i = 1,n_match 
        xmin = dmin1(matching_coords(i,1),xmin) 
        xmax = dmax1(matching_coords(i,1),xmax) 
        ymin = dmin1(matching_coords(i,2),ymin) 
        ymax = dmax1(matching_coords(i,2),ymax) 
      end do 
      opt_ax_al_deg = opt_ax_al_deg_best(ic) 
      opt_ax_del_deg = opt_ax_del_deg_best(ic) 
      focal_length = focal_length_best(ic) 
      angle = angle_best(ic) 
      off_x = offset_x_pix(ic) 
      off_y = offset_y_pix(ic) 
      dev_off_x = dev_offset_x_pix(ic) 
      dev_off_y = dev_offset_y_pix(ic) 
      percentage_match = 
     *      dble(n_match)/dble(n_usno_org)*100.d0 


      return 
      end 
c******************************************************* 
      DOUBLE PRECISION FUNCTION RAN3(IDUM) 
      IMPLICIT REAL*8 (A-H,O-Z) 
      PARAMETER (MBIG=1000000000,MSEED=161803398,MZ=0,FAC=1.E-9) 
      INTEGER INEXT, INEXTP, MA(55) 
      SAVE INEXT,INEXTP,MA 
      DATA IFF /0/ 
      IF(IDUM.LT.0.OR.IFF.EQ.0)THEN 
        IFF=1 
        MJ=MSEED-IABS(IDUM) 
        MJ=MOD(MJ,MBIG) 
        MA(55)=MJ 
        MK=1 
        DO 11 I=1,54 
          II=MOD(21*I,55) 
          MA(II)=MK 
          MK=MJ-MK 
          IF(MK.LT.MZ)MK=MK+MBIG 
          MJ=MA(II) 
11      CONTINUE 
        DO 13 K=1,4 
          DO 12 I=1,55 
            MA(I)=MA(I)-MA(1+MOD(I+30,55)) 
            IF(MA(I).LT.MZ)MA(I)=MA(I)+MBIG 
12        CONTINUE 
13      CONTINUE 
        INEXT=0 
        INEXTP=31 
        IDUM=1 
      ENDIF 
      INEXT=INEXT+1 
      IF(INEXT.EQ.56)INEXT=1 
      INEXTP=INEXTP+1 
      IF(INEXTP.EQ.56)INEXTP=1 
      MJ=MA(INEXT)-MA(INEXTP) 
      IF(MJ.LT.MZ)MJ=MJ+MBIG 
      MA(INEXT)=MJ 
      RAN3=MJ*FAC 
      RETURN 
      END 
c********************************************************8 
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
c************************************************************** 
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
c**************************************************8 
      subroutine read_image_name(icode, 
     *                     pipeline,
     *                     release,
     *                     image_name_file) 

      implicit NONE 

      integer icode,i 
      integer len_image_name
      integer iargc,narg 
      external iargc 

      character*80 image_name_file,arg 
      character*80 file_bright_stars_coord
      character*80 file_all_objects
      character*40 release

      logical pipeline

      icode = 0 

      narg = iargc() 
      i = 1 
      call getarg (i, arg) 
      image_name_file = arg 

      if(image_name_file.eq.'-h') call explanation(icode,release)

      i = 1
      do while(image_name_file(i:i).ne.' ')
        i = i+1
      end do
      i = i-1
      len_image_name = i

      file_bright_stars_coord =
     *   image_name_file(1:len_image_name)//'.bright.jmp'
      file_all_objects =
     *   image_name_file(1:len_image_name)//'.obj.jmp'
      open(1,file=file_bright_stars_coord,status='old',err=10)
      pipeline = .true.
      close(1)
      return

   10 open(2,file=file_all_objects,status='old',err=20)
      pipeline = .true.
      close(1)
      return
   20 pipeline = .false.

      return
      end
**************************************************** 
      subroutine sortinteger(n,arrin,indx) 
      implicit integer(a-h,o-z) 
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
c**************************************************8 
      subroutine use_fourth_order_plate_solution(icode, 
     *          file_failed,
     *          n_max_stars, 
     *          matching_coords,n_match, 
     *          opt_ax_al_deg,opt_ax_del_deg, 
     *          focal_length,angle, 
     *          max_diff_fr_unso, 
     *          usno_al_deg,usno_del_deg, 
     *          n_usno_org, 
     *          fr_id_stars_all_x,fr_id_stars_all_y, 
     *          n_id_stars_org, 
     *          fr_obj_stars_half_x,
     *          fr_obj_stars_half_y,
     *          n_obj_stars_half,
     *          plc, 
     *          crpix1,crpix2) 

      implicit NONE 

      integer icode,n_max_stars,n_match_org,n_match 
      integer n_id_stars_org,n_usno_org,n_obj_stars_half 
      integer i,j,nz,match,im,jm 

      real*8 matching_coords(n_max_stars,6) 
      real*8 matching_coords_copy(n_max_stars,6) 
      real*8 usno_al_deg(n_max_stars) 
      real*8 usno_del_deg(n_max_stars) 
      real*8 fr_id_stars_all_x(n_max_stars) 
      real*8 fr_id_stars_all_y(n_max_stars) 
      real*8 fr_obj_stars_half_x(n_max_stars)
      real*8 fr_obj_stars_half_y(n_max_stars)
      real*8 fr_alpha(n_max_stars) 
      real*8 fr_delta(n_max_stars) 
      real*8 opt_ax_al_deg,opt_ax_del_deg 
      real*8 al_deg,del_deg 
      real*8 crpix1,crpix2 
      real*8 max_diff_fr_unso 
      real*8 focal_length,angle 
      real*8 plc(30) 
      real*8 x,y,x_ast,y_ast,x_gnom,y_gnom 
      real*8 zeta,eta,da,alpha_cand,ttan,dd,delta_cand 
      real*8 alphaz_deg,deltaz_deg 
      real*8 pie,pierez 

      real*8 sind,cosd,tand 
      external sind,cosd,tand 

      character*80 file_failed

      icode = 0 

      n_match_org = n_match 

      pie = dasin(1.d0)*2. 
      pierez = pie/180.d0 

      alphaz_deg = opt_ax_al_deg 
      deltaz_deg = opt_ax_del_deg 

c     match USNO with id stars
c     ------------------------
      do i = 1,n_id_stars_org 
        x = fr_id_stars_all_x(i)-crpix1 
        y = fr_id_stars_all_y(i)-crpix2 
        x_ast =-(x*cosd(-angle)+y*sind(-angle))/focal_length 
        y_ast = (-x*sind(-angle)+y*cosd(-angle))/focal_length 
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

         da = atan(zeta/(cosd(deltaz_deg)-eta*sind(deltaz_deg))) 
         da = da/pierez 
         alpha_cand  = alphaz_deg + da 

         ttan   = tand((alpha_cand - alphaz_deg)/2.) 
         dd =       asin((eta-(zeta*sind(deltaz_deg)*ttan))/ 
     *            dsqrt(zeta**2+eta**2+1 )) 
         dd = dd/pierez 
         delta_cand = deltaz_deg + dd 
        fr_alpha(i) = alpha_cand 
        fr_delta(i) = delta_cand 
      end do 

      do i = 1,n_match_org 
        do j = 1,6 
           matching_coords_copy(i,j) = matching_coords(i,j) 
        end do 
      end do 

      nz = 0 
      do i = 1,n_usno_org 
        match = 0 
        do j = 1,n_id_stars_org 
          if(dabs(fr_alpha(j)-usno_al_deg(i))*3600.d0 
     *                    .lt. max_diff_fr_unso  .and. 
     *           dabs(fr_delta(j)-usno_del_deg(i))*3600.d0 
     *                    .lt. max_diff_fr_unso) then 
            match = match+1 
            jm = j 
          end if 
        end do 
        if(match.eq.1) then 
          nz = nz+1 
          matching_coords(nz,3) = fr_id_stars_all_x(jm) 
          matching_coords(nz,4) = fr_id_stars_all_y(jm) 
          matching_coords(nz,5) = usno_al_deg(i) 
          matching_coords(nz,6) = usno_del_deg(i) 
        end if 
      end do 

      n_match = nz 
      do i = 1,n_match 
        al_deg = matching_coords(i,5) 
        del_deg = matching_coords(i,6) 
        call equa_deg_to_xygnom(al_deg,del_deg, 
     *      opt_ax_al_deg,opt_ax_del_deg, 
     *      x_gnom,y_gnom) 
        x = -x_gnom*focal_length 
        y =  y_gnom*focal_length 
        matching_coords(i,1) =  x*cosd(angle)+y*sind(angle) 
     *                    + crpix1 
        matching_coords(i,2) = -x*sind(angle)+y*cosd(angle) 
     *                    +crpix2 
      end do 

c     match USNO with obj stars
c     ---------------------------
      if(n_obj_stars_half.eq.0) goto 50
      do i = 1,n_obj_stars_half
        do j = 1,n_match
          if(fr_obj_stars_half_x(i).eq.matching_coords(j,3)
     *           .and.
     *       fr_obj_stars_half_y(i).eq.matching_coords(j,4))
     *        goto 20
        end do

        x = fr_obj_stars_half_x(i)-crpix1
        y = fr_obj_stars_half_y(i)-crpix2
        x_ast =-(x*cosd(-angle)+y*sind(-angle))/focal_length
        y_ast = (-x*sind(-angle)+y*cosd(-angle))/focal_length
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

        da = atan(zeta/(cosd(deltaz_deg)-eta*sind(deltaz_deg)))
        da = da/pierez
        alpha_cand  = alphaz_deg + da

        ttan   = tand((alpha_cand - alphaz_deg)/2.)
        dd =       asin((eta-(zeta*sind(deltaz_deg)*ttan))/
     *            dsqrt(zeta**2+eta**2+1 ))
        dd = dd/pierez
        delta_cand = deltaz_deg + dd

        match = 0
        do j = 1,n_usno_org
          if(dabs(alpha_cand-usno_al_deg(j))*3600.d0
     *                    .lt. max_diff_fr_unso  .and.
     *           dabs(delta_cand-usno_del_deg(j))*3600.d0
     *                    .lt. max_diff_fr_unso) then
            match = match+1
            jm = j
          end if
        end do
        if(match.ne.1) goto 20
        do j = 1,n_match
          if(usno_al_deg(jm).eq.matching_coords(j,5) .and.
     *       usno_del_deg(jm).eq.matching_coords(j,6))
     *       goto 20
        end do

        n_match = n_match+1
        nz = n_match
        matching_coords(nz,3) = fr_obj_stars_half_x(i) 
        matching_coords(nz,4) = fr_obj_stars_half_y(i) 
        matching_coords(nz,5) = usno_al_deg(jm)
        matching_coords(nz,6) = usno_del_deg(jm)
        al_deg = matching_coords(nz,5)
        del_deg = matching_coords(nz,6)
        call equa_deg_to_xygnom(al_deg,del_deg,
     *      opt_ax_al_deg,opt_ax_del_deg,
     *      x_gnom,y_gnom)
        x = -x_gnom*focal_length
        y =  y_gnom*focal_length
        matching_coords(nz,1) =  x*cosd(angle)+y*sind(angle)
     *                    + crpix1
        matching_coords(nz,2) = -x*sind(angle)+y*cosd(angle)
     *                    +crpix2

   20 continue
      end do


   50 continue

      if(n_match.lt.15) then
        icode = 500
        open(4,file=file_failed,status='unknown')
        write(4,*) 'not enough stars to make 4th order plate solution'
        write(4,*) 'n_match = ',n_match
        return
      end if


      return 
      end 
c******************************************************** 
      subroutine write_matching_file(icode, 
     *          file_matching_list, 
     *          plc,
     *          file_all_objects,
     *          file_usno_nomatch_pix_coords,
     *          n_skip_rec_obj_file,
     *          n_max_stars,n_match,n_usno_org,
     *          usno_al_deg,usno_del_deg,usno_mag_r,usno_mag_b,
     *          usno_ref_number,
     *          opt_ax_al_deg,
     *          opt_ax_del_deg,
     *          crpix1,crpix2,
     *          angle,
     *          focal_length,
     *          matching_coords) 

      implicit NONE 

      integer i,j,icode,n_max_stars,n_match,n_usno_org 
      integer usno_ref_number(n_max_stars)
      integer n_skip_rec_obj_file

      real*8 matching_coords(n_max_stars,6) 
      real*8 usno_al_deg(n_max_stars)
      real*8 usno_del_deg(n_max_stars)
      real*8 usno_mag_r(n_max_stars),usno_mag_b(n_max_stars)
      real*8 opt_ax_al_deg,opt_ax_del_deg
      real*8 crpix1,crpix2,angle,focal_length
      real*8 x,y,xx,yy,al_deg,del_deg,x_gnom,y_gnom
      real*8 plc(30)
      real*8 sind,cosd
      external sind,cosd

      character*80 file_matching_list 
      character*80 file_all_objects
      character*80 file_usno_nomatch_pix_coords
      character*80 line
      character*1 col1

      logical participate(n_max_stars)

      do i = 1,n_max_stars
        participate(i) = .false.
      end do

      col1 = '#'
      open(1,file=file_matching_list,status='unknown') 
      open(2,file=file_all_objects,status='old')
      open(3,file=file_usno_nomatch_pix_coords,status='unknown')
      do i = 1,n_skip_rec_obj_file-1
        read(2,'(a)') line
        write(1,'(a)') line
        write(3,'(a)') line
      end do
      close(2)

      line =
     * '##  CRVAL1_N  CRVAL2_N    FOCAL     CRPIX1_N  CRPIX2_N    ROT'
      write(1,'(a)') line
      write(1,2001) col1,opt_ax_al_deg,opt_ax_del_deg,
     *                focal_length,crpix1,crpix2,angle
      write(3,'(a)') line
      write(3,2001) col1,opt_ax_al_deg,opt_ax_del_deg,
     *                focal_length,crpix1,crpix2,angle
 2001 format(a1,1x,2f10.5,f11.1,2f10.2,f10.4)

      line = 
     *   '## 10 lines for 30 4th-order plate solution coefficients'
      write(1,'(a)') line
      write(1,4003) col1,(plc(i),i=1,3)
      write(1,4003) col1,(plc(i),i=4,6)
      write(1,4003) col1,(plc(i),i=7,9)
      write(1,4003) col1,(plc(i),i=10,12)
      write(1,4003) col1,(plc(i),i=13,15)
      write(1,4003) col1,(plc(i),i=16,18)
      write(1,4003) col1,(plc(i),i=19,21)
      write(1,4003) col1,(plc(i),i=22,24)
      write(1,4003) col1,(plc(i),i=25,27)
      write(1,4003) col1,(plc(i),i=28,30)
 4003 format(a1,3d25.15)

      line = 
     * '# X_USNO   Y_USNO    X_OBJ    Y_OBJ      R.A.'//
     * '          DEC       R    B   REF_ID'
      write(1,'(a)') line
      line = 
     * '#   X_USNO    Y_USNO      R.A.          DEC       '
     *  //'R     B     REF_ID'
      write(3,'(a)') line

      do i = 1,n_match 
        do j = 1,n_usno_org
          if(usno_al_deg(j) .eq. matching_coords(i,5) .and.
     *       usno_del_deg(j) .eq. matching_coords(i,6)) then
            participate(j) = .true.
            write(1,1000) matching_coords(i,1),
     *                    matching_coords(i,2),
     *                    matching_coords(i,3),
     *                    matching_coords(i,4),
     *                    usno_al_deg(j),
     *                    usno_del_deg(j),
     *                    usno_mag_r(j),usno_mag_b(j),
     *                    usno_ref_number(j)
 1000 format(4f9.2,1x,2f13.8,2f5.1,i11)

            goto 10
          end if
        end do
   10 continue
      end do 

      close(1) 

      do i = 1,n_usno_org
        if(.not.participate(i)) then
          al_deg = usno_al_deg(i)
          del_deg = usno_del_deg(i)
          call equa_deg_to_xygnom(al_deg,del_deg,
     *      opt_ax_al_deg,opt_ax_del_deg,
     *      x_gnom,y_gnom)
          x = -x_gnom*focal_length+crpix1
          y =  y_gnom*focal_length+crpix2
          x = -x_gnom*focal_length
          y =  y_gnom*focal_length
          xx =  x*cosd(angle)+y*sind(angle)
     *                      + crpix1
          yy = -x*sind(angle)+y*cosd(angle)
     *                      +crpix2
          write(3,2200) xx,yy,usno_al_deg(i),usno_del_deg(i),
     *          usno_mag_r(i),usno_mag_b(i),usno_ref_number(i)
 2200   format(2f10.2,1x,2f13.8,2f6.1,i11)

        end if
      end do
      close(3)

      return 
      end 
c***************************************************** 
      subroutine write_usno_pix_coords(icode, 
     *          file_usno_pix_coords, 
     *          file_all_objects,
     *          n_skip_rec_obj_file,
     *          n_max_stars,n_usno_org, 
     *          usno_x_pix_max,usno_x_pix_min,
     *          usno_y_pix_max,usno_y_pix_min,
     *          usno_al_deg,usno_del_deg,usno_mag_r,usno_mag_b,
     *          usno_ref_number,
     *          opt_ax_al_deg, 
     *          opt_ax_del_deg, 
     *          crpix1,crpix2,
     *          angle,
     *          focal_length) 

      implicit NONE 

      integer icode,n_max_stars,n_usno_org 
      integer i 
      integer usno_ref_number(n_max_stars)
      integer n_skip_rec_obj_file

      real*8 usno_al_deg(n_max_stars),usno_del_deg(n_max_stars) 
      real*8 usno_mag_r(n_max_stars),usno_mag_b(n_max_stars)
      real*8 focal_length 
      real*8 al_deg,del_deg,x,y,x_gnom,y_gnom,xx,yy 
      real*8 opt_ax_al_deg,opt_ax_del_deg 
      real*8 crpix1,crpix2,angle
      real*8 usno_x_pix_max,usno_x_pix_min
      real*8 usno_y_pix_max,usno_y_pix_min
      real*8 cosd,sind
      external cosd,sind

      character*80 file_usno_pix_coords 
      character*80 file_all_objects
      character*80 line
      character*1 col1

      col1 = '#'

      open(1,file=file_all_objects,status='old')
      open(3,file=file_usno_pix_coords,status='unknown') 
      do i = 1,n_skip_rec_obj_file-1
        read(1,'(a)') line
        write(3,'(a)') line
      end do
      close(1)

      line =
     * '##  CRVAL1_N  CRVAL2_N    FOCAL     CRPIX1_N  CRPIX2_N    ROT'
      write(3,'(a)') line
      write(3,2001) col1,opt_ax_al_deg,opt_ax_del_deg,
     *                focal_length,crpix1,crpix2,angle
 2001 format(a1,1x,2f10.5,f11.1,2f10.2,f10.4)
      line =
     *  '#     X         Y         R.A.         DEC        '
     *     //'R     B     REF_ID'
      write(3,'(a)') line

      usno_x_pix_max = -1.d5
      usno_x_pix_min = 1.d5
      usno_y_pix_max = -1.d5
      usno_y_pix_min = 1.d5

      do i = 1,n_usno_org 
        al_deg = usno_al_deg(i) 
        del_deg = usno_del_deg(i) 
        call equa_deg_to_xygnom(al_deg,del_deg, 
     *      opt_ax_al_deg,opt_ax_del_deg, 
     *      x_gnom,y_gnom) 
        x = -x_gnom*focal_length+crpix1 
        y =  y_gnom*focal_length+crpix2 
        x = -x_gnom*focal_length
        y =  y_gnom*focal_length
        xx =  x*cosd(angle)+y*sind(angle)
     *                      + crpix1
        yy = -x*sind(angle)+y*cosd(angle)
     *                      +crpix2
        usno_x_pix_max = dmax1(usno_x_pix_max,xx)
        usno_x_pix_min = dmin1(usno_x_pix_min,xx)
        usno_y_pix_max = dmax1(usno_y_pix_max,yy)
        usno_y_pix_min = dmin1(usno_y_pix_min,yy)

        write(3,2200) xx,yy,usno_al_deg(i),usno_del_deg(i),
     *                 usno_mag_r(i),usno_mag_b(i),usno_ref_number(i)
 2200   format(2f10.2,1x,2f13.8,2f6.1,i11)
      end do 

      close(3) 

      return 
      end

c************************************************************ 
      subroutine write_warning_file(icode,
     *       pointing_error_arcsec,
     *       opt_ax_al_deg_1,opt_ax_al_deg,
     *       opt_ax_del_deg_1,opt_ax_del_deg,
     *       file_image,file_warning,
     *       percentage,min_percentage_match,
     *       n_usno_org,n_match,
     *       naxis1,naxis2,
     *       warning,test_run,
     *       xmin,xmax,usno_x_pix_min,usno_x_pix_max,
     *       ymin,ymax,usno_y_pix_min,usno_y_pix_max)

      implicit NONE

      integer icode,n_usno_org,n_match,naxis1,naxis2
      integer unit

      logical warning,test_run

      real*8 pointing_error_arcsec,
     *       opt_ax_al_deg_1,opt_ax_al_deg,
     *       opt_ax_del_deg_1,opt_ax_del_deg,
     *       percentage,min_percentage_match,
     *       xmin,xmax,usno_x_pix_min,usno_x_pix_max,
     *       ymin,ymax,usno_y_pix_min,usno_y_pix_max

      character*80 file_image,file_warning

      if(dabs(opt_ax_al_deg_1-opt_ax_al_deg)*3600. .gt.
     *         60.d0 .or.
     *   dabs(opt_ax_del_deg_1-opt_ax_del_deg)*3600. .gt.
     *         60.d0) then
        unit = 8
        warning = .true.
        call open_for_appending_file(unit,file_warning)
        write(unit,'(a)')
     *      'bad telescope pointing '
        write(unit,*)
     *     '  opt_ax_al_deg_org-opt_ax_al_deg_found',
     *     (opt_ax_al_deg_1-opt_ax_al_deg)*3600.
        write(unit,*)
     *      '  opt_ax_del_deg_org-opt_ax_del_deg_found',
     *     (opt_ax_del_deg_1-opt_ax_del_deg)*3600.
        close(unit)
        if(test_run) then
          write(6,6666) 'warning for image ',file_image(1:30)
          unit = 6
        write(unit,*)
     *      'bad telescope pointing '
        write(unit,*)
     *     '  opt_ax_al_deg_org-opt_ax_al_deg_found',
     *     (opt_ax_al_deg_1-opt_ax_al_deg)*3600.
        write(unit,*)
     *      '  opt_ax_del_deg_org-opt_ax_del_deg_found',
     *     (opt_ax_del_deg_1-opt_ax_del_deg)*3600.
        end if
      end if

      if(percentage.lt.min_percentage_match) then
        unit = 8
        warning = .true.
        call open_for_appending_file(unit,file_warning)
        write(unit,'(a)')
     *      'percentage of matched stars is low'
        write(unit,*) '  n_usno_total,n_match,percentage',
     *        n_usno_org,n_match,percentage
        close(unit)
        if(test_run) then
          write(6,*) ' '
 6666 format(a,a)
          write(6,*)
     *      'warning: percentage of matched stars is low'
          write(6,*) 'n_usno_org,n_match,percentage',
     *        n_usno_org,n_match,percentage
        end if
      end if

      if(dble(naxis1)-xmax .gt. 0.10*dble(naxis1) .or.
     *   xmin .gt. 0.10*dble(naxis1) .or.
     *   dble(naxis2)-ymax .gt. 0.10*dble(naxis2) .or.
     *   ymin .gt. 0.10*dble(naxis2)) then
        warning = .true.
        unit = 8
        call open_for_appending_file(unit,file_warning)
        write(unit,'(a)')
     *      'matched USNO stars cover badly image'
        write(unit,6) '  smallest x-coord of a matched USNO star is ',
     *              xmin
        write(unit,6) '  smallest x-coord of all USNO stars is ',
     *                 usno_x_pix_min
        write(unit,*) '  '
        write(unit,6) '  largest x-coord of a matched USNO star is ',
     *              xmax
        write(unit,6) '  largest x-coord of all USNO stars is ',
     *                 usno_x_pix_max
        write(unit,*) '  '
        write(unit,6) '  smallest y-coord of a matched USNO star is ',
     *              ymin
        write(unit,6) '  smallest y-coord of all USNO stars is ',
     *                 usno_y_pix_min
        write(unit,*) '  '
        write(unit,6) '  largest y-coord of a matched USNO star is ',
     *              ymax
        write(unit,6) '  largest y-coord of all USNO stars is ',
     *                 usno_y_pix_max
        write(unit,*) '  '
        write(unit,*) ' naxis1,naxis2',naxis1,naxis2
    6 format(a,T50,f7.1)
        close(unit)
        if(test_run) then
        unit = 6
        write(unit,*) ' '
        write(unit,'(a)')
     *      'matched USNO stars cover badly image'
        write(unit,6) '  smallest x-coord of a matched USNO star is ',
     *              xmin
        write(unit,6) '  smallest x-coord of all USNO stars is ',
     *                 usno_x_pix_min
        write(unit,*) '  '
        write(unit,6) '  largest x-coord of a matched USNO star is ',
     *              xmax
        write(unit,6) '  largest x-coord of all USNO stars is ',
     *                 usno_x_pix_max
        write(unit,*) '  '
        write(unit,6) '  smallest y-coord of a matched USNO star is ',
     *              ymin
        write(unit,6) '  smallest y-coord of all USNO stars is ',
     *                 usno_y_pix_min
        write(unit,*) '  '
        write(unit,6) '  largest y-coord of a matched USNO star is ',
     *              ymax
        write(unit,6) '  largest y-coord of all USNO stars is ',
     *                 usno_y_pix_max
        write(unit,*) '  '
        write(unit,*) ' naxis1,naxis2',naxis1,naxis2
        end if
      end if

      return
      end
