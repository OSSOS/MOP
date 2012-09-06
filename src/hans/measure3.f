c     measure3.f
c     transforms x,y pixel-coordinates of an object on several plates
c           ---> RA, DEC
c     linear method using closest USNO stars
c    
c     Release 2004.1.9
c     ---------------------------------------------

      implicit NONE

      integer nn_max_stars,n_max_stars
      integer mmax_frames,max_frames
      parameter(nn_max_stars=5000)
      parameter(mmax_frames=5)

      integer icode,n_frames
      integer i
      integer n_skip_rec_id_file
      integer n_skip_rec_br_file
      integer n_copy_cands_file
      integer n_refstars,n_stars,n_refstars_org
      integer imode
      integer unit,unitx
      integer nz,j
      integer ifirst,n_usno

      real*8 tol_diff_arc_sec
      real*8 cand_x,cand_y
      real*8 cand_x_trans,cand_y_trans
      real*8 alpha_cand_lin,delta_cand_lin
      real*8 alpha_cand,delta_cand
      real*8 alpha_cand_lin_impair,delta_cand_lin_impair
      real*8 alpha_cand_lin_pair,delta_cand_lin_pair
      real*8 diff_max_arcsec_lin_test,diff
      real*8 error_alpha,error_delta
      real*8 diff_alpha_lin_pair,
     *               diff_alpha_lin_impair,
     *               diff_delta_lin_pair,
     *               diff_delta_lin_impair
      real*8 distmax
      real*8 cosd
      external cosd

      character*80 ffile_bright_stars_coord(mmax_frames)
      character*80 ffile_identified_objects(mmax_frames)
      character*80 ffile_matching_list(mmax_frames)
      character*80 file_matching_list
      character*80 file_image
      character*80 file_cands_ra_dec
      character*80 file_cands_moving
      character*80 file_scatter
      character*80 file_mpc
      character*80 file_warning
      character*80 file_ok
      character*80 file_failed
      character*80 line
      character*2 aa

      logical pipeline,warning,test_run,last_try

c     test_run = .true.
      test_run = .false.

      n_max_stars = nn_max_stars
      max_frames = mmax_frames

      aa = '##'

c     ---------------
c     read image name
c     ---------------
      icode = 0
      call read_image_name_measure3(icode,
     *                     pipeline,
     *                     n_frames,
     *                     file_image)

      if(test_run) then
        write(6,*) ' '
        write(6,*) '--------------------------------------'
        write(6,*) file_image
      end if

c     ---------------
c     build file names
c     ---------------
      icode = 0
      call build_file_names_measure3(icode,
     *                     pipeline,
     *                     file_image,
     *                     ffile_bright_stars_coord,
     *                     ffile_identified_objects,
     *                     ffile_matching_list,
     *                     file_cands_ra_dec,
     *                     file_cands_moving,
     *                     file_scatter,
     *                     file_mpc,
     *                     file_warning,
     *                     file_ok,
     *                     file_failed,
     *                     n_frames,max_frames,
     *                     n_copy_cands_file,
     *                     n_skip_rec_id_file,
     *                     n_skip_rec_br_file)

      open(1,file=file_failed,status='unknown')
      write(1,*) 
     *  'if there is also a ...measure3.OK file, measure3 worked fine'
      close(1)

c     get hardwired parameters
c     ------------------------
      icode = 0
      call get_run_parameters_measure3(icode,
     *                     n_refstars_org,
     *                     diff_max_arcsec_lin_test,
     *                     tol_diff_arc_sec)

c     check plate solutions
c     compare RA and DEC of bright stars on all frames
c     --------------------------------------------
      icode = 0
      call check_plate_solution(icode,
     *                  ffile_matching_list,
     *                  ffile_bright_stars_coord,
     *                  file_scatter,
     *                  file_warning,
     *                  file_failed,
     *                  pipeline,
     *                  tol_diff_arc_sec,
     *                  n_frames,max_frames,
     *                  n_max_stars,
     *                  n_skip_rec_br_file)

c     compute RA and DEC for objects
c     ------------------------------
      open(11,file=file_cands_moving,status='old',err=110)
      open(12,file=file_cands_ra_dec,status='unknown')
      do i = 1,n_copy_cands_file
        read(11,'(a)') line
        write(12,'(a)')line
      end do
      read(11,*)
      line = '##   X        Y        X_0     Y_0       '//
     *       '   R.A.          DEC'
      write(12,'(a)')line

   10 continue

      read(11,*,end=100)
      do i = 1,n_frames
        read(11,*) cand_x,cand_y,cand_x_trans,cand_y_trans
        icode = 0
        ifirst = 1

        error_alpha = 0.d0
        error_delta = 0.d0

c     use linear solution with n_refstars closest stars
c     imode = 1
c     ------------------------------------------
        file_matching_list = ffile_matching_list(i)
        n_refstars = n_refstars_org
        last_try = .false.

   20   imode = 1

        call find_alpha_delta_linear(icode,
     *              ifirst,
     *              imode,
     *              n_refstars,n_stars,
     *              n_max_stars,
     *              cand_x,cand_y,
     *              alpha_cand_lin,delta_cand_lin,
     *              n_usno,
     *              distmax,
     *              n_skip_rec_br_file,
     *              diff_max_arcsec_lin_test,
     *              file_matching_list,
     *              file_failed,
     *              file_warning)
        if(icode.ne.0.and. .not. last_try) goto 95
        if(icode.ne.0.and.last_try) then
          unit = 8
          call open_for_appending_file(unit,file_warning)
          write(8,'(a)') 
     *     'no linear solution for object using closest stars' 
          write(8,8080) '   object coords',cand_x,cand_y
 8080 format(a,2f10.1)
          close(8)
          if(test_run) then 
            write(6,'(a)')
     *     'no linear solution for object using closest stars' 
            write(6,8080) '   object coords',cand_x,cand_y
          end if
          goto 96
        end if


c     use linear solution with 2*n_refstars closest stars using
c     imode = 2
c     first, third,...... closest star
c     ------------------------------------------
        imode = 2
        call find_alpha_delta_linear(icode,
     *              ifirst,
     *              imode,
     *              n_refstars,n_stars,
     *              n_max_stars,
     *              cand_x,cand_y,
     *              alpha_cand_lin_impair,delta_cand_lin_impair,
     *              n_usno,
     *              distmax,
     *              n_skip_rec_br_file,
     *              diff_max_arcsec_lin_test,
     *              file_matching_list,
     *              file_failed,
     *              file_warning)
        if(icode.ne.0.and. .not. last_try) goto 95
        if(icode.ne.0.and.last_try) then
          unit = 8
          call open_for_appending_file(unit,file_warning)
          write(8,'(a)')
     *     'no linear solution for object using closest impair stars' 
          write(8,8080) '   object coords',cand_x,cand_y
          close(8)
         if(test_run) then
          write(6,'(a)')
     *     'no linear solution for object using closest impair stars' 
          write(6,8080) '   object coords',cand_x,cand_y
         end if
          goto 96
        end if


c     use linear solution with 2*n_refstars closest stars using
c     imode = 3
c     second,fourth,...... closest star
c     ------------------------------------------
        imode = 3
        call find_alpha_delta_linear(icode,
     *              ifirst,
     *              imode,
     *              n_refstars,n_stars,
     *              n_max_stars,
     *              cand_x,cand_y,
     *              alpha_cand_lin_pair,delta_cand_lin_pair,
     *              n_usno,
     *              distmax,
     *              n_skip_rec_br_file,
     *              diff_max_arcsec_lin_test,
     *              file_matching_list,
     *              file_failed,
     *              file_warning)
        if(icode.ne.0.and. .not. last_try) goto 95
        if(icode.ne.0.and.last_try) then
          unit = 8
          call open_for_appending_file(unit,file_warning)
          write(8,'(a)')
     *     'no linear solution for object using closest pair stars'
          write(8,8080) '   object coords',cand_x,cand_y
          close(8)
         if(test_run) then
          write(6,'(a)')
     *     'no linear solution for object using closest pair stars'
          write(6,8080) '   object coords',cand_x,cand_y
         end if
          goto 96
        end if

c     compare the three solutions
c     ---------------------------
        diff_alpha_lin_pair =
     *      (alpha_cand_lin-alpha_cand_lin_pair)
     *       *cosd(delta_cand_lin)*3600.d0
        diff_alpha_lin_impair =
     *      (alpha_cand_lin-alpha_cand_lin_impair)
     *       *cosd(delta_cand_lin)*3600.d0
        diff_delta_lin_pair =
     *      (delta_cand_lin-delta_cand_lin_pair)*3600.d0
        diff_delta_lin_impair =
     *      (delta_cand_lin-delta_cand_lin_impair)*3600.d0

        if(.not.last_try) goto 40
c================================================================
        if(dabs(diff_alpha_lin_pair).gt.999.d0)
     *          diff_alpha_lin_pair = 999.d0
        if(dabs(diff_alpha_lin_impair).gt.999.d0)
     *          diff_alpha_lin_impair = 999.d0
        if(dabs(diff_delta_lin_pair).gt.999.d0)
     *          diff_delta_lin_pair = 999.d0
        if(dabs(diff_delta_lin_impair).gt.999.d0)
     *          diff_delta_lin_impair = 999.d0

        diff = diff_max_arcsec_lin_test
        if(dabs(diff_alpha_lin_pair).gt.diff .or.
     *    dabs(diff_alpha_lin_impair).gt.diff .or.
     *    dabs(diff_delta_lin_pair).gt.diff .or.
     *    dabs(diff_delta_lin_impair).gt.diff) then
          unitx = 17
          call open_for_appending_file(unitx,file_warning)
          write(unitx,'(a)') '## '
          write(unitx,'(a)')
     * '## WARNING: RA or DEC discrepancy among linear solutions'
          write(unitx,'(a)')'##      X        Y'
          write(unitx,2013) aa,cand_x,cand_y
 2013 format(a2,2f10.1)
          write(unitx,'(a)')
     *    '## RA1 - RA2(")  RA1 - RA3(")  DEC1-DEC2(")  DEC1-DEC3(")'
          write(unitx,2015)aa,
     *               diff_alpha_lin_pair,
     *               diff_alpha_lin_impair,
     *               diff_delta_lin_pair,
     *               diff_delta_lin_impair
 2015 format(a2,f6.1,10x,f6.1,8x,f6.1,10x,f6.1)

        end if

        error_alpha = dmax1(dabs(diff_alpha_lin_pair),
     *                        dabs(diff_alpha_lin_impair))
        error_delta = dmax1(dabs(diff_delta_lin_pair),
     *                        dabs(diff_delta_lin_impair))

         if(test_run) then
        write(6,*) ' '
        write(6,*) 'alpha_cand_lin,delta_cand_lin',
     *              alpha_cand_lin,delta_cand_lin
        write(6,*) 'cand_x,cand_y',cand_x,cand_y
        write(6,*) 'error_alpha,error_delta',
     *              error_alpha,error_delta
         end if
        goto 96
c================================================================

   40   continue
        diff = diff_max_arcsec_lin_test
        if(dabs(diff_alpha_lin_pair).lt.diff .and.
     *    dabs(diff_alpha_lin_impair).lt.diff .and.
     *    dabs(diff_delta_lin_pair).lt.diff .and.
     *    dabs(diff_delta_lin_impair).lt.diff) goto 96
          if(test_run) write(6,2015)aa,
     *               diff_alpha_lin_pair,
     *               diff_alpha_lin_impair,
     *               diff_delta_lin_pair,
     *               diff_delta_lin_impair



   95 continue
        n_refstars = n_refstars+2
        if(n_refstars.le.n_usno/2) goto 20

c the three solutions with different sets of close stars differ strongly
c take the solution with the 20 closest stars
c ------------------------------------------------------------------------
        n_refstars = 20
        last_try = .true.

        goto 20

   96 continue

      if(test_run) write(6,*) 'n_refstars',n_refstars

c     output
c     ------
      if(icode.ne.0) then
          alpha_cand_lin = -1.d0
          delta_cand_lin = 91.d0
        end if
        if(i.eq.1) then
          line = ' '
          write(12,'(a)')line
        end if
        write(12,2012) cand_x,cand_y,cand_x_trans,cand_y_trans,
     *               alpha_cand_lin,delta_cand_lin
 2012 format(4f9.2,2f15.7)
        if(.not.pipeline .and. icode.ne.0) then
          write(6,3015) 'problem to measure object with coords',
     *      cand_x,cand_y
 3015 format(a,2f10.1)
        end if

        if(.not.pipeline) 
     *     call write_in_mpc_format(icode,
     *                         file_mpc,
     *                         file_matching_list,
     *                         cand_x,cand_y,
     *                         alpha_cand_lin,
     *                         delta_cand_lin)

      end do

      goto 10

  100 close(11)

      open(1,file=file_ok,status='unknown')
      write(1,'(a)') 'measure3 worked fine'
      close(1)

      stop

  110 open(8,file=file_failed,status='unknown')
      write(8,'(a)') 'cannot open file',file_cands_moving
      close(8)
      write(6,'(a)') 'FAILURE cannot open file',file_cands_moving
      stop
      end

c********************************************************
      subroutine write_in_mpc_format(icode,
     *                         file_mpc,
     *                         file_matching_list,
     *                         cand_x,cand_y,
     *                         alpha_cand_lin,
     *                         delta_cand_lin)

      implicit NONE

      integer icode
      integer obs_code
      integer unit18
      integer jahr,monat,itag,ist,imin,monat1,monat2,iday1
      integer ih1,ih2,iam1,iam2,ias1
      integer id1,id2,idm1,idm2,ids1

      real*8 alpha_cand_lin,delta_cand_lin
      real*8 mjd
      real*8 jul_date,sec,hour,day
      real*8 alpha_s,delta_s
      real*8 cand_x,cand_y

      character*80 file_mpc
      character*80 file_matching_list
      character*17 camera_name
      character*1 sign_delta

      open(1,file=file_matching_list,status='old')

      read(1,*)
      read(1,*)
      read(1,*)
      read(1,1000) mjd
 1000 format(1x,f17.7)
      read(1,*)
      read(1,1001) camera_name
 1001 format(45x,a)

      close(1)

      obs_code = 0

      jul_date = mjd+2400000.5d0
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

      unit18 = 18
      call open_for_appending_file(unit18,file_mpc)
      write(unit18,4044) jahr,monat1,monat2,iday1,day,
     *      ih1,ih2,iam1,iam2,ias1,alpha_s,
     *       sign_delta,id1,id2,idm1,idm2,ids1,delta_s,
     *        cand_x,cand_y
 4044 format(T13,i4,1x,2i1,1x,i1,f8.6,1x,2i1,1x,2i1,1x,i1,f5.3,1x,
     *        a1,2i1,1x,2i1,1x,i1,f4.2,4x,2f8.1)
      close(unit18)

      return
      end

c**********************************************
      subroutine read_image_name_measure3(icode,
     *                     pipeline,
     *                     n_frames,
     *                     file_image)

      implicit NONE

      integer icode,i
      integer max_frames,n_frames
      integer iargc,narg
      external iargc

      logical pipeline

      character*80 file_image
      character*80 arg

      icode = 0

      narg = iargc()
      i = 1
      call getarg (i, arg)
      file_image = arg

      open(1,file=file_image,status='old',err=10)
      pipeline = .false.
      goto 20
   10 pipeline = .true.
   20 close(1)

      if(pipeline) then
        n_frames = 3
      else
        n_frames = 3
      end if

      return
      end

c*************************************************************8
      subroutine get_run_parameters_measure3(icode,
     *                     n_refstars,
     *                     diff_max_arcsec_lin_test,
     *                     tol_diff_arc_sec)

      implicit NONE

      integer icode
      integer n_refstars

      real*8 diff_max_arcsec_lin_test
      real*8 tol_diff_arc_sec

      tol_diff_arc_sec = 2.0d0
      diff_max_arcsec_lin_test = 0.5d0
      n_refstars = 12

      return
      end

c*************************************************************8
      subroutine build_file_names_measure3(icode,
     *                     pipeline,
     *                     file_image,
     *                     ffile_bright_stars_coord,
     *                     ffile_identified_objects,
     *                     ffile_matching_list,
     *                     file_cands_ra_dec,
     *                     file_cands_moving,
     *                     file_scatter,
     *                     file_mpc,
     *                     file_warning,
     *                     file_ok,
     *                     file_failed,
     *                     n_frames,max_frames,
     *                     n_copy_cands_file,
     *                     n_skip_rec_id_file,
     *                     n_skip_rec_br_file)

      implicit NONE

      integer icode,n_frames,max_frames
      integer n_skip_rec_id_file
      integer n_skip_rec_br_file
      integer n_copy_cands_file

      character*80 file_image
      character*80 ffile_bright_stars_coord(max_frames)
      character*80 ffile_identified_objects(max_frames)
      character*80 ffile_matching_list(max_frames)
      character*80 file_cands_ra_dec
      character*80 file_cands_moving
      character*80 file_scatter
      character*80 file_mpc
      character*80 file_warning
      character*80 file_ok
      character*80 file_failed

      logical pipeline

      icode = 0

      if(.not.pipeline) then
        call hans_files_measure3(icode,
     *                     file_image,
     *                     ffile_bright_stars_coord,
     *                     ffile_identified_objects,
     *                     ffile_matching_list,
     *                     file_cands_ra_dec,
     *                     file_cands_moving,
     *                     file_scatter,
     *                     file_mpc,
     *                     file_warning,
     *                     file_ok,
     *                     file_failed,
     *                     n_frames,max_frames,
     *                     n_copy_cands_file,
     *                     n_skip_rec_id_file,
     *                     n_skip_rec_br_file)

      else
        call pipeline_files_measure3(icode,
     *                     file_image,
     *                     ffile_bright_stars_coord,
     *                     ffile_identified_objects,
     *                     ffile_matching_list,
     *                     file_cands_ra_dec,
     *                     file_cands_moving,
     *                     file_scatter,
     *                     file_mpc,
     *                     file_warning,
     *                     file_ok,
     *                     file_failed,
     *                     n_frames,max_frames,
     *                     n_copy_cands_file,
     *                     n_skip_rec_id_file,
     *                     n_skip_rec_br_file)
      end if

      return
      end
c*****************************************************
      subroutine hans_files_measure3(icode,
     *                     detect_input_file,
     *                     ffile_bright_stars_coord,
     *                     ffile_identified_objects,
     *                     ffile_matching_list,
     *                     file_cands_ra_dec,
     *                     file_cands_moving,
     *                     file_scatter,
     *                     file_mpc,
     *                     file_warning,
     *                     file_ok,
     *                     file_failed,
     *                     n_frames,max_frames,
     *                     n_copy_cands_file,
     *                     n_skip_rec_id_file,
     *                     n_skip_rec_br_file)
      implicit NONE

      integer icode,n_frames,max_frames
      integer n_skip_rec_id_file
      integer n_skip_rec_br_file
      integer i,j,len_cat_path_name,len_frame_name
      integer n_copy_cands_file

      character*80 detect_input_file
      character*80 ffile_bright_stars_coord(max_frames)
      character*80 ffile_identified_objects(max_frames)
      character*80 ffile_matching_list(max_frames)
      character*80 file_cands_ra_dec
      character*80 file_cands_moving
      character*80 file_scatter
      character*80 file_mpc
      character*80 file_warning
      character*80 file_ok
      character*80 file_failed
      character*80 path_cat,frame

      icode = 0

      n_skip_rec_br_file = 2+7*n_frames+1
      n_skip_rec_id_file = 2+7*n_frames+1
      n_copy_cands_file = 23

      open(2,file=detect_input_file,status='old')
      read(2,*)
      read(2,'(a)') path_cat
      read(2,*)
      read(2,*)
      read(2,*)

      i = 1
      do while(path_cat(i:i).ne.' ')
        i = i+1
      end do
      i = i-1
      len_cat_path_name = i

      do j = 1,n_frames
        read(2,'(a)') frame

        i = 1
        do while(frame(i:i).ne.' ')
          i = i+1
        end do
        i = i-1
        len_frame_name = i

        if(j.eq.1) then
          file_cands_ra_dec = path_cat(1:len_cat_path_name)
     *        //frame(1:len_frame_name)//
     *        '.measure3.cands.astrom'
          file_cands_moving = path_cat(1:len_cat_path_name)
     *        //frame(1:len_frame_name)//
     *        '_cands_comb'
          file_scatter = path_cat(1:len_cat_path_name)
     *        //frame(1:len_frame_name)//
     *            '.measure3.astrom.scatter'
          file_mpc = path_cat(1:len_cat_path_name)
     *        //frame(1:len_frame_name)//
     *            '.measure3.mpc'
          file_warning = path_cat(1:len_cat_path_name)
     *        //frame(1:len_frame_name)//
     *            '.measure3.WARNING'
          file_ok = path_cat(1:len_cat_path_name)
     *        //frame(1:len_frame_name)//
     *            '.measure3.OK'
          file_failed = path_cat(1:len_cat_path_name)
     *        //frame(1:len_frame_name)//
     *           '.measure3.FAILED'
        end if
        ffile_bright_stars_coord(j) = 
     *              path_cat(1:len_cat_path_name)
     *        //frame(1:len_frame_name)//
     *        '_br'
        ffile_identified_objects(j) =
     *              path_cat(1:len_cat_path_name)
     *        //frame(1:len_frame_name)//
     *        '_id'
        ffile_matching_list(j) =
     *              path_cat(1:len_cat_path_name)
     *        //frame(1:len_frame_name)//
     *                '.mkpltsol.usno'

      end do
      close(2)

      return
      end
c*****************************************************
      subroutine pipeline_files_measure3(icode,
     *                     file_image,
     *                     ffile_bright_stars_coord,
     *                     ffile_identified_objects,
     *                     ffile_matching_list,
     *                     file_cands_ra_dec,
     *                     file_cands_moving,
     *                     file_scatter,
     *                     file_mpc,
     *                     file_warning,
     *                     file_ok,
     *                     file_failed,
     *                     n_frames,max_frames,
     *                     n_copy_cands_file,
     *                     n_skip_rec_id_file,
     *                     n_skip_rec_br_file)
      implicit NONE

      integer icode,n_frames,max_frames
      integer n_skip_rec_id_file
      integer n_skip_rec_br_file
      integer i,j,len_image_name
      integer n_copy_cands_file

      character*80 file_image
      character*80 ffile_bright_stars_coord(max_frames)
      character*80 ffile_identified_objects(max_frames)
      character*80 ffile_matching_list(max_frames)
      character*80 file_cands_ra_dec
      character*80 file_cands_moving
      character*80 file_scatter
      character*80 file_mpc
      character*80 file_warning
      character*80 file_ok
      character*80 file_failed
      character*20 image_name(max_frames),line

      icode = 0

      i = 1
      do while(file_image(i:i).ne.' ')
        i = i+1
      end do
      i = i-1
      len_image_name = i

      n_skip_rec_id_file = 9
      n_skip_rec_br_file = 7
      n_copy_cands_file = 23

      file_cands_moving = 
     *      file_image(1:len_image_name)//
     *        '.cands.comb'
      file_cands_ra_dec = 
     *      file_image(1:len_image_name)//
     *        '.measure3.cands.astrom'
      file_scatter = 
     *      file_image(1:len_image_name)//
     *        '.measure3.astrom.scatter'
      file_mpc = 
     *      file_image(1:len_image_name)//
     *        '.measure3.mpc'
      file_warning=
     *      file_image(1:len_image_name)//
     *        '.measure3.WARNING'
      file_ok =
     *      file_image(1:len_image_name)//
     *        '.measure3.OK'
      file_failed =
     *      file_image(1:len_image_name)//
     *        '.measure3.FAILED'

      open(11,file=file_cands_moving,status='old',err=110)
      do i = 1,3
        read(11,2001) image_name(i)
      end do
      close(11)

 2001 format(2x,a20)
      do i = 1,3
        line = image_name(i)
        ffile_bright_stars_coord(i) = line(1:len_image_name)
     *                             //'.bright.jmp'
        ffile_matching_list(i) = line(1:len_image_name)
     *                             //'.mkpltsol.usno'
        ffile_identified_objects(i) = line(1:len_image_name)
     *                             //'.id.jmp'
      end do

      return

  110 open(1,file=file_failed,status='unknown')
      write(1,*) 'can not open file',file_cands_moving
      close(1)
      stop
      
      end

c     ------------------------------------------
      subroutine find_alpha_delta_linear(icode,
     *              ifirst,
     *              imode,
     *              n_refstars,n_stars,
     *              n_max_stars,
     *              cand_x,cand_y,
     *              alpha_cand,delta_cand,
     *              n_usno,
     *              distmax,
     *              n_skip_rec_file,
     *              diff_max_arcsec_lin_test,
     *              file_matching_list,
     *              file_failed,
     *              file_warning)


      implicit NONE

      integer icode,imode,n_max_stars,n_refstars,n_stars

      integer i,j,nz,l,k,ierr
      integer indx_sort(n_max_stars)
      integer n_elim
      integer nn_max
      integer unitx
      integer n_skip_rec_file
      integer n_usno
      integer unit,ifirst

      real*8 usno_alpha(n_max_stars),usno_alpha_sort(n_max_stars)
      real*8 usno_delta(n_max_stars),usno_delta_sort(n_max_stars)
      real*8 frame_star_coord(n_max_stars,2)
      real*8 frame_star_coord_sort(n_max_stars,2)
      real*8 dist_pix_2(n_max_stars)
      real*8 dist_pix_2_sort(n_max_stars)
      real*8 distmax
      real*8 pie,pierez
      real*8 al_deg,del_deg
      real*8 cand_x,cand_y
      real*8 x,y
      real*8 aa,bb,da,dd
      real*8 ttan
      real*8 eta,zeta,alpha_cand,delta_cand
      real*8 alphaz_deg,deltaz_deg
      real*8 x_ast,y_ast,x_orig,y_orig
      real*8 x_gnom,y_gnom
      real*8 test(2*n_refstars)
      real*8 xx(2*n_refstars),xx1(2*n_refstars),
     *       t(2*n_refstars,6),t1(2*n_refstars,6)
      real*8 w(6),plc(6),rv(6),pplc(6,1)
      real*8 kn
      real*8 epsilon,epsilon_2
      real*8 opt_ax_al_deg,opt_ax_del_deg
      real*8 pix_scale,focal_length,angle,crpix1,crpix2
      real*8 ua,ud,cx,cy,tx,ty
      real*8 jul_date_modified,jul_date
      real*8 cand_x_pix,cand_y_pix
      real*8 diff_max_arcsec_lin_test,diff

      real*8 tand,sind,cosd
      external tand,sind,cosd

      logical take_it


      character*80 file_matching_list
      character*80 file_warning
      character*80 file_failed
      character*1 col1

      save 

      icode = 0
      pie = 3.141592653589793D0
      pierez = pie/180.d0

      alpha_cand = -1.d0
      delta_cand = 99.d0

      if(ifirst.ne.1) goto 20

c     read ref star coords
c     -----------------------------------
      open(4,err=110,file=file_matching_list,status='old')
      do i = 1,n_skip_rec_file-1
        read(4,*)
      end do
      read(4,*)
      read(4,2001) col1,opt_ax_al_deg,opt_ax_del_deg,
     *                focal_length,crpix1,crpix2,angle
 2001 format(a1,1x,2f10.5,f11.1,2f10.2,f10.4)
      do i = 1,12
        read(4,*)
      end do
      i = 1
   10 read(4,1000,end=15) x,y,
     *             frame_star_coord(i,1),
     *             frame_star_coord(i,2),
     *             usno_alpha(i),
     *             usno_delta(i)
 1000 format(4f9.2,1x,2f13.8)
      i = i+1
      goto 10
   15 n_usno = i-1
      close(4)

      do i = 1,n_usno
        dist_pix_2(i) = (frame_star_coord(i,1)-cand_x)**2+
     *                  (frame_star_coord(i,2)-cand_y)**2 
      end do
      call sortreal8(n_usno,dist_pix_2,indx_sort)
      do k = 1,n_usno
        i = indx_sort(k)
        frame_star_coord_sort(k,1) = frame_star_coord(i,1)
        frame_star_coord_sort(k,2) = frame_star_coord(i,2)
        usno_alpha_sort(k) = usno_alpha(i)
        usno_delta_sort(k) = usno_delta(i)
        dist_pix_2_sort(k) = dist_pix_2(i)
      end do

   20 continue
      ifirst = 2

      i = 0
      if(imode.eq.1) nz = n_refstars
      if(imode.ne.1) nz = n_refstars/2
      do k = 1,n_usno
        take_it = .false.
        if(imode.eq.1) take_it = .true.
        if(imode.eq.2 .and. mod(k+1,2).eq.0) take_it = .true.
        if(imode.eq.3 .and. mod(k,2).eq.0) take_it = .true.
        if(take_it) then
          i = i+1
          usno_alpha(i) = usno_alpha_sort(k)
          usno_delta(i) = usno_delta_sort(k)
          frame_star_coord(i,1) = frame_star_coord_sort(k,1)
          frame_star_coord(i,2) = frame_star_coord_sort(k,2)
          dist_pix_2(i) = dist_pix_2_sort(k)
          if(i.eq.nz) goto 40
        end if
      end do

   40 continue
      n_stars = n_refstars
      if(n_stars.gt.n_usno) n_stars = n_usno
      if(n_stars.gt.i) n_stars = i
      distmax = dsqrt(dist_pix_2(n_stars))
        
   45 continue
      do i=1,2*n_stars
        do j=1,6
          t(i,j) = 0.d0
        end do
      end do

      do i = 1,n_stars
        x = frame_star_coord(i,1)-crpix1
        y = frame_star_coord(i,2)-crpix2
        x_ast =-(x*cosd(-angle)+y*sind(-angle))/focal_length
        y_ast = (-x*sind(-angle)+y*cosd(-angle))/focal_length

        t(i,1) =1
        t(i,2) = x_ast
        t(i,3) = y_ast

        al_deg = usno_alpha(i)
        del_deg = usno_delta(i)
        call equa_deg_to_xygnom(al_deg,del_deg,
     *      opt_ax_al_deg,opt_ax_del_deg,
     *      x_gnom,y_gnom)

        xx(i) = x_gnom
        xx(i+n_stars) = y_gnom
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
        unit = 8
        call open_for_appending_file(unit,file_warning)
        write(8,'(a)') 'least square solution fails'
        write(8,8181) '  object coordinates',cand_x,cand_y
        close(8)
        write(6,'(a)') 'least square solution fails'
        write(6,8181) '  object coordinates',cand_x,cand_y
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

      diff = diff_max_arcsec_lin_test
      if (epsilon.gt.(3.*diff/180.d0/3600.d0*pie))
     *    epsilon = diff/180.d0/3600.d0*pie
c     if (epsilon.gt.(3./180.d0/3600.d0*pie))
c    *    epsilon = 1./180.d0/3600.d0*pie
      open(13,file='dummy_ref_stars',form='unformatted',
     *      status='unknown')
      do i = 1,n_stars
        if (dabs(xx(i)-test(i)).gt.(3.*epsilon).or.
     1  dabs(xx(i+n_stars)-test(i+n_stars)).gt.(3.*epsilon) ) then
          n_elim = n_elim+1
c         write(6,*) n_elim,i,n_stars
        else
          write(13) frame_star_coord(i,1),
     *              frame_star_coord(i,2),
     *              usno_alpha(i),usno_delta(i),
     *              dist_pix_2(i)
        end if
      end do
      close(13)
      if(n_elim.eq.0) goto 50

      n_stars = n_stars-n_elim
      if(n_stars.lt.6) then
        unit = 8
        call open_for_appending_file(unit,file_warning)
        write(8,'(a)') 'number of reference stars less than 6'
        write(8,8181) '  object coordinates',cand_x,cand_y
        close(8)
 8181 format(a,2f10.1)
c       write(6,'(a)') 'number of reference stars less than 6'
c       write(6,8181) '  object coordinates',cand_x,cand_y
        icode = 44
        return
      end if
      open(13,file='dummy_ref_stars',form='unformatted',
     *      status='old')
      do i = 1,n_stars
        read(13) frame_star_coord(i,1),
     *              frame_star_coord(i,2),
     *              usno_alpha(i),usno_delta(i),
     *              dist_pix_2(i)
      end do
      distmax = dsqrt(dist_pix_2(n_stars))
      close(13)
      goto 45

   50 continue
      do i = 1,6
        plc(i) = pplc(i,1)
      end do

      x = cand_x-crpix1
      y = cand_y-crpix2
      x_ast =-(x*cosd(-angle)+y*sind(-angle))/focal_length
      y_ast = (-x*sind(-angle)+y*cosd(-angle))/focal_length

      zeta =  plc(1)+plc(2)*x_ast+plc(3)*y_ast
      eta  =  plc(4)+plc(5)*x_ast+plc(6)*y_ast

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
      return

  110 open(8,file=file_failed,status='unknown')
      write(8,'(a)') 'cannot open file',file_matching_list
      close(8)
      write(6,'(a)') 'cannot open file',file_matching_list
      stop

      end

c-------------------------------------------
      subroutine check_plate_solution(icode,
     *                  ffile_matching_list,
     *                  ffile_stars_coord,
     *                  file_scatter,
     *                  file_warn,
     *                  file_failed,
     *                  pipeline,
     *                  tol_diff_arc_sec_0,
     *                  n_frames,max_frames,
     *                  n_max_stars,
     *                  n_skip_rec_file)

      implicit NONE
      integer n_max_stars

      integer i,j,nz,l,icode,iii
      integer naxis1,naxis2
      integer n_skip_rec_file
      integer unitx
      integer n_frames,max_frames
      integer n_fail_fourth_order
      integer n_stars(max_frames)
      integer n_warn_rms

      real*8 alpha(max_frames,n_max_stars)
      real*8 delta(max_frames,n_max_stars)
      real*8 coord_x(max_frames,n_max_stars)
      real*8 coord_y(max_frames,n_max_stars)
      real*8 coord_0_x_read(max_frames,n_max_stars)
      real*8 coord_0_y_read(max_frames,n_max_stars)
      real*8 coord_x_read(max_frames,n_max_stars)
      real*8 coord_y_read(max_frames,n_max_stars)
      real*8 pie,pierez
      real*8 ttan
      real*8 opt_ax_al_deg,opt_ax_del_deg
      real*8 focal_length,crpix1,crpix2,angle
      real*8 tol_diff_arc_sec_0
      real*8 tol_diff_arc_sec
      real*8 eta,zeta,alpha_cand,delta_cand
      real*8 alphaz_deg,deltaz_deg
      real*8 x_ast,y_ast,x,y
      real*8 plc(30)
      real*8 da,dd
      real*8 alpha_rms(max_frames),delta_rms(max_frames)
      real*8 alpha_diff,delta_diff
      real*8 tand,sind,cosd
      external tand,sind,cosd

      character*80 ffile_matching_list(max_frames)
      character*80 file_matching_list
      character*80 ffile_stars_coord(max_frames)
      character*80 file_stars_coord
      character*80 file_scatter
      character*80 file_warn
      character*80 file_failed
      character*80 line
      character*1  col1

      logical pipeline

      icode = 0
      pie = 3.141592653589793D0
      pierez = pie/180.d0

c     read star coordinates
c     ---------------------
      do iii = 1,n_frames
        do i = 1,n_max_stars
          coord_x(iii,i) = -1.d3
          coord_y(iii,i) = -1.d3
          alpha(iii,i) = -1.d3
        end do
      end do

      do iii = 1,n_frames
        file_stars_coord = ffile_stars_coord(iii)
        open(1,err=100,file=file_stars_coord,status='old')
        do i = 1,n_skip_rec_file
          read(1,*)
        end do
        nz = 1
    5   read(1,*,end=20)coord_x_read(iii,nz),coord_y_read(iii,nz),
     *                  coord_0_x_read(iii,nz),coord_0_y_read(iii,nz)

        if(nz.eq.n_max_stars) then
          unitx = 17
          call open_for_appending_file(unitx,file_warn)
          write(unitx,*) ' '
          write(unitx,*) 'WARNING: increase parameter n_max_stars'
          if(.not.pipeline) 
     *        write(6,*) 'WARNING: increase parameter n_max_stars'
          close(unitx)
          goto 20
        end if
        nz = nz+1
        goto 5

   20   continue
        close(1)
        n_stars(iii) = nz-1

c       warning if no stars
c       -------------------
        if(nz.eq.1) then
        unitx = 17
        call open_for_appending_file(unitx,file_warn)
        write(unitx,*) ' '
        write(unitx,*) 'WARNING in subroutine check_plate_solution:'
        write(unitx,*) '  There are no frame stars on file',
     *                 file_stars_coord
        close(unitx)
        if(.not.pipeline) then
        write(6,*) ' '
        write(6,*) 'WARNING in subroutine check_plate_solution:'
        write(6,*) '  There are no frame stars on file',
     *                 file_stars_coord
        end if
        return
        end if

      end do

c     order coordinates
c     -----------------
      do i = 1,n_stars(1)
        coord_x(1,i) = coord_x_read(1,i)
        coord_y(1,i) = coord_y_read(1,i)
        do iii = 2,n_frames
          do j = 1,n_stars(iii)
            if(dabs(coord_0_x_read(1,i)-coord_0_x_read(iii,j))
     *               .lt.0.1d0   .and.
     *         dabs(coord_0_y_read(1,i)-coord_0_y_read(iii,j))
     *               .lt.0.1d0) then
              coord_x(iii,i) = coord_x_read(iii,j)
              coord_y(iii,i) = coord_y_read(iii,j)
              goto 25
            end if
          end do
   25     continue
        end do
      end do

c     compute alpha,delta of frame stars using 4-th order solution
c     ------------------------------------------------------------
      do iii = 1,n_frames
        file_matching_list = ffile_matching_list(iii)
        open(4,err=110,file=file_matching_list,status='old')
        do i = 1,n_skip_rec_file-1
          read(4,*)
        end do
        read(4,*)
        read(4,2001) col1,opt_ax_al_deg,opt_ax_del_deg,
     *                focal_length,crpix1,crpix2,angle
 2001 format(a1,1x,2f10.5,f11.1,2f10.2,f10.4)
        read(4,*)
        read(4,4003) col1,(plc(i),i=1,3)
        read(4,4003) col1,(plc(i),i=4,6)
        read(4,4003) col1,(plc(i),i=7,9)
        read(4,4003) col1,(plc(i),i=10,12)
        read(4,4003) col1,(plc(i),i=13,15)
        read(4,4003) col1,(plc(i),i=16,18)
        read(4,4003) col1,(plc(i),i=19,21)
        read(4,4003) col1,(plc(i),i=22,24)
        read(4,4003) col1,(plc(i),i=25,27)
        read(4,4003) col1,(plc(i),i=28,30)
 4003 format(a1,3d25.15)
        close(4)

        alphaz_deg = opt_ax_al_deg
        deltaz_deg = opt_ax_del_deg

        tol_diff_arc_sec = 
     *          tol_diff_arc_sec_0/cosd(opt_ax_del_deg)

        do i = 1,n_stars(iii)
          x = coord_x(iii,i)
          y = coord_y(iii,i)
          if(x.lt.-1.d2) goto 30
          x = x-crpix1
          y = y-crpix2
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

          alpha(iii,i) = alpha_cand
          delta(iii,i) = delta_cand

   30   continue
        end do
      end do

c     test on differences in alpha,delta
c     determine rms for these differences
c     ----------------------------------
      do iii = 1,n_frames
        alpha_rms(iii) = 0.d0
        delta_rms(iii) = 0.d0
      end do

      n_warn_rms = 0
      do iii = 2,n_frames
        do i = 1,n_stars(1)
          do j = 1,n_frames
            if(alpha(j,i).lt.0.d0) goto 35
          end do
          nz = nz+1
          alpha_diff = 
     *           (alpha(iii,i)-alpha(1,i))*cosd(delta(i,1))
          alpha_rms(iii) = alpha_rms(iii)+alpha_diff**2
          delta_diff = delta(iii,i)-delta(1,i)
          delta_rms(iii) = delta_rms(iii)+delta_diff**2
          if(dabs(alpha_diff)*3600.d0.gt.tol_diff_arc_sec .or.
     *       dabs(delta_diff)*3600.d0.gt.tol_diff_arc_sec)
     *       n_warn_rms = n_warn_rms+1
   35  continue
       end do
       alpha_rms(iii) = dsqrt(alpha_rms(iii)/dble(nz-1))*3600.
       delta_rms(iii) = dsqrt(delta_rms(iii)/dble(nz-1))*3600.
      end do

      do iii = 2,n_frames
        if(alpha_rms(iii).gt.tol_diff_arc_sec .or.
     *     delta_rms(iii).gt.tol_diff_arc_sec) then
          unitx = 8
          open(unitx,file=file_failed,status='unknown')
          write(unitx,*) 
     *      'FAILURE in subroutine check_plate_solution:'
        write(unitx,*)
     *  '  Applying plate solutions of all frames on pix coordinates'
        write(unitx,*)
     *  '  of bright stars and comparing their resulting RA, DEC'
        write(unitx,*)
     *  '  bright stars have too large differences in RA, DEC.'
        write(unitx,*) 'difference (arcsecs) tolerated',
     *       tol_diff_arc_sec

          write(unitx,*) 'diff_alpha_rms,diff_delta_rms',
     *       alpha_rms(iii),delta_rms(iii)
          close(unitx)
          if(.not.pipeline) then
          write(6,*)
     *      'FAILURE in subroutine check_plate_solution:'
          write(6,*) 'diff_alpha_rms,diff_delta_rms',
     *       alpha_rms(iii),delta_rms(iii)
          end if
          stop
        end if
      end do

c     line = '# subroutine check_plate_solution'
c     write(6,'(a)') line
c     line =
c    * '# RMS(arcsec) between frame 1 and frame N'
c     write(6,'(a)') line
c     line = '#  FRAME    RA       DEC'
c     write(6,'(a)') line
c     do iii = 2,n_frames
c       write(6,8080) iii,alpha_rms(iii),delta_rms(iii)
c     end do

      open(8,file=file_scatter,status='unknown')
      line = '# subroutine check_plate_solution'
      write(8,'(a)') line 
      line = 
     * '# RMS(arcsec) between frame 1 and frame N' 
      write(8,'(a)') line 
      line = '#  FRAME    RA       DEC'
      write(8,'(a)') line 
      do iii = 2,n_frames
        write(8,8080) iii,alpha_rms(iii),delta_rms(iii)
 8080 format(i6,2f9.2)
      end do
      close(8)

c     if(n_warn_rms.gt.0) then
c       unitx = 17
c       call open_for_appending_file(unitx,file_warn)
c       write(unitx,*) ' '
c       write(unitx,*) 'WARNING in subroutine check_plate_solution:'
c       write(unitx,*) 
c    *  '  Applying plate solutions of all frames on pix coordinates'
c       write(unitx,*) 
c    *  '  of bright stars and comparing their resulting RA, DEC'
c       write(unitx,*) 
c    *  '  some bright stars have too large differences in RA, DEC.'
c       write(unitx,*) 'difference (arcsecs) tolerated', 
c    *       tol_diff_arc_sec
c       write(unitx,*) 'number of stars found to exceed this difference', 
c    *       n_warn_rms
c       close(unitx)

c       if(.not.pipeline) then
c         write(6,*) 
c    *     ' WARNING in subroutine check_plate_solution:'
c         write(6,*)     
c    *     '   number of too large differences',n_warn_rms
c       end if
c     end if

      return

  100   unitx = 8
        open(unitx,file=file_failed,status='unknown')
        write(unitx,*)
     *      'FAILURE in subroutine check_plate_solution:'
        write(unitx,*) ' can not open file',file_stars_coord
        close(unitx)
        if(.not.pipeline) then
        write(6,*)
     *      'FAILURE in subroutine check_plate_solution:'
        write(6,*) ' can not open file',file_stars_coord
        end if
        stop
  110   unitx = 8
        open(unitx,file=file_failed,status='unknown')
        write(unitx,*)
     *      'FAILURE in subroutine check_plate_solution:'
        write(unitx,*) ' can not open file',file_matching_list
        close(unitx)
        if(.not.pipeline) then
        write(6,*)
     *      'FAILURE in subroutine check_plate_solution:'
        write(6,*) ' can not open file',file_matching_list
        end if
        stop

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
