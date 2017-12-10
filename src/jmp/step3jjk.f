C -*-compile-command: "cf77 -O6 -o ../bin/LINUX/step3jmp step3jmp.f -L ../lib/LINUX -ljmp; /bin/rm step3jmp.o"; -*-

      implicit none

      include 'match-mov.h'

      real*4
     $  flux(maxframes,n_o_max_cat), intensity(maxframes,n_o_max_cat),
     $  size_obj(maxframes,n_o_max_cat), elong(maxframes,n_o_max_cat),
     $  coo(maxframes,n_o_max_cat,2), coo_sys1(maxframes,n_o_max_cat,2),
     $  tmpr(n_o_max_cat), fwhm(maxframes)

      integer*4
     $  mov_obj(maxframes,n_o_max_cat), indx(n_o_max_cat),
     $  n_objects(maxframes), n_obj, narg, iargc, i, j, k, n, l,
     $  iyyy, mm, m1, m2, m3, k1, ff, i1, i2, npr_3, nf, n1,
     $  proposed_n(maxframes,n_o_max_cat), nz, min_num_frames,
     $  indx2(n_o_max_cat), nframes, frame_number, bright_enough

      real*4
     $  rmax, rmin, tmp1, c1(2), c2(2), cr(4), min_day_m, tmp,
     $  max_day_m, min_mo_sec_hour, max_mo_sec_hour, tol_dis_lin,
     $  min_motion_pix, pixscale, ang_mean, ang_width, mopvers

      real*8
     $  obs_time(maxframes), mjd, day

      logical
     $  decide, finished, no_a, no_f1, no_f2, no_f3, no_rn,
     $  no_rx, no_w, help, jclndr, size_ok

      character
     $  images(maxframes)*20, line*200, arg*80, frame*100,
     $  result_ident_file*100, result_ident_files(maxframes)*100,
     $  unidentif_file_cats(maxframes)*100, header(7)*80

      common /c1/min_day_m, max_day_m, tol_dis_lin, min_motion_pix
      common /angles/ang_mean, ang_width


c Create a file for later error handling

      open (unit=1, file='step3jmp.FAILED', status='unknown')
      write (1, *) ' '
      close (1)

      do i = 1, maxframes
         do j = 1, n_o_max_cat
            proposed_n(i,j) = 0
         end do
      end do

      narg = iargc()
      i = 1
      no_f1 = .true.
      no_f2 = .true.
      no_f3 = .true.
      no_rn = .true.
      no_rx = .true.
      no_a = .true.
      no_w = .true.
      help = (narg .ne. 14)

 90   continue
      if (i .le. narg) then
         call getarg (i, arg)
         if (arg(1:3) .eq. '-f1') then
            i = i + 1
            call getarg (i, images(1))
            no_f1 = .false.
         else if (arg(1:3) .eq. '-f2') then
            i = i + 1
            call getarg (i, images(2))
            no_f2 = .false.
         else if (arg(1:3) .eq. '-f3') then
            i = i + 1
            call getarg (i, images(3))
            no_f3 = .false.
         else if (arg(1:3) .eq. '-rn') then
            i = i + 1
            call getarg (i, arg)
            read (arg, *) min_mo_sec_hour
            no_rn = .false.
         else if (arg(1:3) .eq. '-rx') then
            i = i + 1
            call getarg (i, arg)
            read (arg, *) max_mo_sec_hour
            no_rx = .false.
         else if (arg(1:2) .eq. '-a') then
            i = i + 1
            call getarg (i, arg)
            read (arg, *) ang_mean
            no_a = .false.
          else if (arg(1:2) .eq. '-w') then
            i = i + 1
            call getarg (i, arg)
            read (arg, *) ang_width
            no_w = .false.
         else if ((arg(1:2) .eq. '-h') .or. (arg(1:2) .eq. '-?')) then
            i = i + 1
            help = .true.
        end if
         i = i + 1
         goto 90
      end if

      j = 1
      do i = 0, narg
         call getarg (i, arg)
         call read_file_name (arg, i1, i2, finished, 80)
         line(j:j+i2-i1) = arg(i1:i2)
         j = j + i2 - i1 + 1
         line(j:j) = ' '
         j = j + 1
      end do
      write (6, *) line(1:j-1)

      open (unit=1, file='step3jmp.FAILED', status='unknown')
      write (1, *) line(1:j-1)
      close (1)

      help = help .or. no_a .or. no_f1 .or. no_f2 .or. no_f3 .or. no_rn
     $  .or. no_rx .or. no_w

      if (help) then
         write (6, *) 'Usage: step3jmp -f1 <im1> -f2 <im2> -f3 <im3>'
     $     //' -rn <rate> -rx <rate> -a <angle> -w <width>'
         write (6, *) 'where:'
         write (6, *) '-fn <im>: image name'
         write (6, *) '-rn <rate>: minimum motion per hour (")'
         write (6, *) '-rx <rate>: maximum motion per hour (")'
         write (6, *) '-a <angle>: mean direction of motion (deg)'
         write (6, *)
     $     '-w <width>: maximum angle deviation for motion (deg)'
         stop
      end if

      min_motion_pix = 0.
      min_num_frames = 3
      ang_width = abs(ang_width)
 1234 continue
      if (ang_mean .gt. 180.) then
         ang_mean = ang_mean - 360.
         goto 1234
      end if
 1235 continue
      if (ang_mean .lt. -180.) then
         ang_mean = ang_mean + 360.
         goto 1235
      end if

      nframes = 3

      do j = 1, nframes
c     construct filenames related to the frames
c     -----------------------------------------
         do i = 1, 80
            frame(i:i) = char(0)
         end do
         frame = images(j)
         call read_file_name (frame, i1, i2, finished, 80)
         if (finished) stop 'We''ve got a problem here!'
         unidentif_file_cats(j) = frame(i1:i2)//
     $     '.unid.jmp'
         result_ident_files(j) = frame(i1:i2)//
     *     '.moving.jmp'
      end do

      result_ident_file = result_ident_files(1)
      open(2,file=result_ident_file,status='unknown')

      do j = 1, nframes
         do i = 1, 80
            frame(i:i) = char(0)
         end do
         frame = images(j)
         call read_file_name (frame, i1, i2, finished, 80)
         write (2, '(''# '', a)') frame(i1:i2)
      end do

      jclndr = .false.
      tol_dis_lin = 0.
      do frame_number = 1, nframes

         frame = images(frame_number)
         call read_file_name (frame, i1, i2, finished, 80)

c     read obs_time
c     -------------
         open (unit=1, file=unidentif_file_cats(frame_number),
     $     status='old')
         do i = 1, 7
            read (1, '(a)') header(i)
         end do
         read (header(2)(3:), *) mopvers
         i = i2 - i1 + 10
         call check_version (mopvers, unidentif_file_cats(frame_number),
     $     i, j)
         read (header(4)(3:), *) obs_time(frame_number), tmp, tmp, 
     $        fwhm(frame_number)
         tol_dis_lin = tol_dis_lin + fwhm(frame_number)
         mjd = obs_time(frame_number) + 2400000.5d0
         call caldat (mjd, jclndr, iyyy, mm, day)
         if (day .lt. 10.) then
            write (header(4)(3:18), '(i4, 1x, i2.2, '' 0'', f7.5)')
     $        iyyy, mm, day
         else
            write (header(4)(3:18), '(i4, 1x, i2.2, 1x, f8.5)') iyyy,
     $        mm, day
         end if
         read (header(6)(3:), *) pixscale
         do i = 1, 6
            write (2, '(a)') header(i)
         end do

c     read cat of unidentified objects
c     --------------------------------
         n = 1
 40      continue
         read (1, *, end=50)
     $     coo(frame_number,n,1), coo(frame_number,n,2),
     $     coo_sys1(frame_number,n,1), coo_sys1(frame_number,n,2),
     $     flux(frame_number,n), size_obj(frame_number,n),
     $     intensity(frame_number,n), elong(frame_number,n)
         n = n+1
         goto 40
 50      continue
         close(1)
         n_objects(frame_number) = n-1

C skip sorting.
c         goto 8888

         if (frame_number .eq. 1) then
            call sortreal (n_objects(frame_number), intensity, indx)
            do i = 1, n_objects(frame_number)
               j = n_objects(frame_number) + 1 - i
               tmpr(i) = size_obj(frame_number,indx(j))
            end do
            do i = 1, n_objects(frame_number)
               size_obj(frame_number,i) = tmpr(i)
            end do
            do i = 1, n_objects(frame_number)
               j = n_objects(frame_number) + 1 - i
               tmpr(i) = coo(frame_number,indx(j),1)
            end do
            do i = 1, n_objects(frame_number)
               coo(frame_number,i,1) = tmpr(i)
            end do
            do i = 1, n_objects(frame_number)
               j = n_objects(frame_number) + 1 - i
               tmpr(i) = coo(frame_number,indx(j),2)
            end do
            do i = 1, n_objects(frame_number)
               coo(frame_number,i,2) = tmpr(i)
            end do
            do i = 1, n_objects(frame_number)
               j = n_objects(frame_number) + 1 - i
               tmpr(i) = coo_sys1(frame_number,indx(j),1)
            end do
            do i = 1, n_objects(frame_number)
               coo_sys1(frame_number,i,1) = tmpr(i)
            end do
            do i = 1, n_objects(frame_number)
               j = n_objects(frame_number) + 1 - i
               tmpr(i) = coo_sys1(frame_number,indx(j),2)
            end do
            do i = 1, n_objects(frame_number)
               coo_sys1(frame_number,i,2) = tmpr(i)
            end do
            do i = 1, n_objects(frame_number)
               j = n_objects(frame_number) + 1 - i
               tmpr(i) = flux(frame_number,indx(j))
            end do
            do i = 1, n_objects(frame_number)
               flux(frame_number,i) = tmpr(i)
            end do
            do i = 1, n_objects(frame_number)
               j = n_objects(frame_number) + 1 - i
               tmpr(i) = intensity(frame_number,indx(j))
            end do
            do i = 1, n_objects(frame_number)
               intensity(frame_number,i) = tmpr(i)
            end do
            do i = 1, n_objects(frame_number)
               j = n_objects(frame_number) + 1 - i
               tmpr(i) = elong(frame_number,indx(j))
            end do
            do i = 1, n_objects(frame_number)
               elong(frame_number,i) = tmpr(i)
            end do
         end if

c 8888    continue

      end do
      tol_dis_lin = tol_dis_lin/float(nframes)

c Save velocity parameters.
      write (2, '(a)') '##     RMIN    RMAX   ANGLE   AWIDTH'
      write (2, 2010) min_mo_sec_hour, max_mo_sec_hour, ang_mean,
     $  ang_width
      write (2, '(a)') header(7)
      if (pixscale .le. 0.) stop
      min_day_m=min_mo_sec_hour*24.e0/pixscale
      max_day_m=max_mo_sec_hour*24.e0/pixscale

 2010 format ('# ', 4f8.1)

      k1 = nframes-min_num_frames+1
      npr_3 = 0
      do m1 = 1, k1
         do m2 = m1+1, k1+1
            do m3 = m2+1, k1+2
               n1 = npr_3 + 1
               call find_on_three_frames (coo_sys1, n_objects, obs_time,
     $           maxframes, n_o_max_cat, m1, m2, m3, npr_3, proposed_n,
     $           indx, indx2, tmpr)
               do nf = m3+1, nframes
                  call find_on_further_frame
     $              (m1, m2, m3, nf, n1, npr_3, proposed_n)
               end do
            end do
         end do
      end do

c     output
c     ------
      do l = 1, npr_3
         nz = 0
         do ff = 1,nframes
            if (proposed_n(ff,l) .ne. 0) nz = nz + 1
         end do
         indx(l) = nz
      end do

      nz = 0
      do k1 = nframes, min_num_frames, -1
         do l = 1, npr_3
            if (indx(l) .eq. k1) then
               nz = nz+1
               do i = 1, nframes
                  mov_obj(i,nz) = proposed_n(i,l)
               end do
            end if
         end do
      end do

      n_obj = 0
      do l = 1, nz
         rmax = -1000.
         rmin = 1000000.
         do ff = 1, nframes
            i = mov_obj(ff,l)
            if (i .ne. 0) then
               size_ok = (size_obj(ff,i) .gt. 3.141/4.0*fwhm(ff)**2)
               rmax = amax1(rmax,size_obj(ff,i))
               rmin = amin1(rmin,size_obj(ff,i))
            end if
         end do
         decide = (rmax/rmin .le. 4.)
         if (decide) then
            rmax = -1000.
            rmin = 1000000.
            bright_enough = 0
            do ff = 1,nframes
               i = mov_obj(ff,l)
               if (i .ne. 0) then
                  rmax = amax1(rmax,flux(ff,i))
                  rmin = amin1(rmin,flux(ff,i))
                  if ( flux(ff,i) > 1000 ) then
                     bright_enough = bright_enough + 1
                  end if
               end if
            end do
c JJK Set maximum flux ratio to 3 instead of 4
c AND at least two of the measures have flux > 1000
            decide = ((rmax/rmin .le. 3.) .and. ( bright_enough .gt. 1 ))
         end if

c JMP: Reject objects that are at the same location on
c each frame.

         if (decide) then
            i = mov_obj(1,l)
            decide = .false.
            do ff = 2, nframes
               j = mov_obj(ff,l)
               decide = decide .or.
     $           ((abs(coo(1,l,1)-coo(ff,l,1)) .gt. .5)
     $           .or. (abs(coo(1,l,2)-coo(ff,l,2)) .gt. .5))
            end do
c JJK require that there be more 2 pixels offset between x value of first and third observation
            decide = decide .and. (abs(coo(1,l,1) - coo(3,l,1)) .gt. 2)
         end if

c JMP: Reject objects whose elongation change too much.

         if (decide) then
            rmax = -1000.
            rmin = 1000000.
            smin = .true.
            do ff = 1,nframes
               i = mov_obj(ff,l)
               if (i .ne. 0) then
                  rmax = amax1(rmax,elong(ff,i))
                  rmin = amin1(rmin,elong(ff,i))
               end if
            end do
            decide = (rmax/rmin .le. 3.)
         end if

c Done. Output the object if selected.

         if(decide) then       
            write (2, *)
            do ff = 1, nframes
               i = mov_obj(ff,l)
               if (i .eq. 0) then
                  c1(1) = 0.e0
                  c1(2) = 0.e0
                  c2(1) = 0.e0
                  c2(2) = 0.e0
                  do k = 1, 4
                     cr(k) = 0.
                  end do
               else
                  cr(1) = flux(ff,i)
                  cr(2) = size_obj(ff,i)
                  cr(3) = intensity(ff,i)
                  cr(4) = elong(ff,i)
                  c1(1) = coo_sys1(ff,i,1)
                  c1(2) = coo_sys1(ff,i,2)
                  c2(1) = coo(ff,i,1)
                  c2(2) = coo(ff,i,2)
               end if
               write (2,2001) c2, c1, cr
            end do
            n_obj = n_obj + 1
         end if
      end do
 2001 format (4f8.2, f13.2, f9.1, f10.2, f6.2)
      write (6, *) 'Found ', n_obj, ' moving objects.'

c Apparently things went right

      open (unit=1, file='step3jmp.OK', status='unknown')
      write (1, *) ' '
      close (1)

      stop
      end

      subroutine find_on_three_frames (coo_sys1, n_objects, obs_time,
     $  maxframes, n_o_max_cat, m1, m2, m3, npr_3, proposed_n, indx2,
     $  indx3, tmpr)

      implicit none

      integer*4
     $  indsize

      real*4
     $  step

      parameter
     $  (indsize = 1000, step = 10.)

      integer*4
     $  maxframes, n_o_max_cat, n_objects(maxframes), i, j, k, n1, n2,
     $  n3, f1, f2,f3, proposed_n(maxframes,*), npr_3, indx2(*),
     $  indx3(*), sy2(0:indsize), sy3(0:indsize), k1, k2, j1, j2, ks,
     $  js, m1, m2, m3

      real*4
     $  coo_sys1(maxframes,n_o_max_cat,2), tmpr(*),
     $  tol_d, dx_13, dy_13, dist_12, dist_23, x_expec, y_expec, ratio,
     $  sina2, cosa2, sina3, cosa3, max_mean_mo, max_mean_mo_2,
     $  min_mean_mo, min_mean_mo_2, min_day_m,
     $  max_day_m, min_motion_pix, ang_mean, ang_width, ang2, ang_d2,
     $  ang3, ang_d3, tol_dis_lin, yl, yh

      real*8
     $  obs_time(maxframes), t1, t2, t3

      logical
     $  decide

      common /c1/min_day_m, max_day_m, tol_dis_lin, min_motion_pix
CCC JMP
      common /angles/ang_mean, ang_width

      f1 = m1
      f2 = m2
      f3 = m3

      n1 = n_objects(f1)
      n2 = n_objects(f2)
      n3 = n_objects(f3)
      t1 = obs_time(f1)
      t2 = obs_time(f2)
      t3 = obs_time(f3)
      max_mean_mo = max_day_m*(t3-t1)
      max_mean_mo_2 = max_mean_mo**2
      min_mean_mo =  min_day_m*(t3-t1)
      min_mean_mo_2 = min_mean_mo**2

      ratio = (t2-t1)/(t3-t1)

      if ((t2.lt.t1) .or. (t3.lt.t2)  .or. (t3.lt.t1)) then
	 write(6,*) 'attention, time is not growing'
	 write(6,*) t1
	 write(6,*) t2
	 write(6,*) t3
         stop
      end if
     
      tol_d = tol_dis_lin**2

      do i = 0, indsize
         sy2(i) = 1
      end do
      do i = 1, n2
         tmpr(i) = coo_sys1(f2,i,2)
      end do
      call sortreal (n2, tmpr, indx2)
      do i = 1, n2
         j = min(max(int(coo_sys1(f2,indx2(i),2)/step),0) + 1, indsize)
         sy2(j) = i
      end do
      do i = 2, indsize
         if (sy2(i) .eq. 1) sy2(i) = sy2(i-1)
      end do

      do i = 0, indsize
         sy3(i) = 1
      end do
      do i = 1, n3
         tmpr(i) = coo_sys1(f3,i,2)
      end do
      call sortreal (n3, tmpr, indx3)
      do i = 1, n3
         j = min(max(int(coo_sys1(f3,indx3(i),2)/step),0) + 1, indsize)
         sy3(j) = i
      end do
      do i = 2, indsize
         if (sy3(i) .eq. 1) sy3(i) = sy3(i-1)
      end do

      do i = 1, n1
         yl = coo_sys1(f1,i,2) - max_mean_mo
         yh = coo_sys1(f1,i,2) + max_mean_mo
         k1 = sy3(max(int(yl/step), 0))
         k2 = sy3(min(int(yh/step)+1, indsize))
         do ks = k1, k2
            k = indx3(ks)
            dx_13 = coo_sys1(f3,k,1)-coo_sys1(f1,i,1)
            dy_13 = coo_sys1(f3,k,2)-coo_sys1(f1,i,2)

            if ((dx_13**2+dy_13**2 .lt. max_mean_mo_2) .and.
     *        (dx_13**2+dy_13**2 .gt. min_mean_mo_2)) then
               x_expec = ratio*dx_13+coo_sys1(f1,i,1)
               y_expec = ratio*dy_13+coo_sys1(f1,i,2)
               yl = y_expec - tol_dis_lin
               yh = y_expec + tol_dis_lin
               j1 = sy2(max(int(yl/step), 0))
               j2 = sy2(min(int(yh/step)+1, indsize))
               do js = j1, j2
                  j = indx2(js)
            if(abs(y_expec-coo_sys1(f2,j,2)).le.tol_dis_lin) then 
            if(abs(x_expec-coo_sys1(f2,j,1)).le.tol_dis_lin) then

c          linear motion test
c          ---------------------------------------------------
            if((y_expec-coo_sys1(f2,j,2))**2+
     *         (x_expec-coo_sys1(f2,j,1))**2.lt.tol_d) then
c          ----------------------------------------------------
              dist_12 = sqrt((coo_sys1(f2,j,1)-coo_sys1(f1,i,1))**2
     *                      + (coo_sys1(f2,j,2)-coo_sys1(f1,i,2))**2)
              dist_23 = sqrt((coo_sys1(f3,k,1)-coo_sys1(f2,j,1))**2
     *                      +(coo_sys1(f3,k,2)-coo_sys1(f2,j,2))**2)
              decide = (dist_12 .ge. min_motion_pix)
     *          .and. (dist_23 .ge. min_motion_pix)
              if (decide) then
                 sina2 = (coo_sys1(f2,j,2)-coo_sys1(f1,i,2))/dist_12
                 cosa2 = (coo_sys1(f2,j,1)-coo_sys1(f1,i,1))/dist_12
                 sina3 = (coo_sys1(f3,k,2)-
     *             coo_sys1(f2,j,2))/dist_23
                 cosa3 = (coo_sys1(f3,k,1)-
     *             coo_sys1(f2,j,1))/dist_23
                 ang2 = atan2(sina2, cosa2)*180./3.141592653
                 ang3 = atan2(sina3, cosa3)*180./3.141592653
                 ang_d2 = ang_mean - ang2
                 if (ang_d2 .lt. -180.) ang_d2 = ang_d2 + 360.
                 if (ang_d2 .gt. 180.) ang_d2 = ang_d2 - 360.
                 ang_d3 = ang_mean - ang3
                 if (ang_d3 .lt. -180.) ang_d3 = ang_d3 + 360.
                 if (ang_d3 .gt. 180.) ang_d3 = ang_d3 - 360.
                 decide = (abs(ang_d2) .le. ang_width)
     $             .and. (abs(ang_d3) .le. ang_width)
	      end if

c          tests are finished
c          ------------------
              if (decide) then
                 npr_3 = npr_3 + 1
                 if (npr_3 .gt. n_o_max_cat) then
                    write(6,*) 'increase n_o_max_cat in match-mov.h'
                    write(6,*) 'n_o_max_cat = ',n_o_max_cat
                    stop
                 end if
                 proposed_n(f1,npr_3) = i
                 proposed_n(f2,npr_3) = j
                 proposed_n(f3,npr_3) = k
              end if
            end if
            end if
            end if
               end do
            end if
         end do
      end do

      return
      end

      subroutine find_on_further_frame
     $  (m1, m2, m3, f4, ns, npr_3, proposed_n)

      include 'match-mov.h'

      integer
     $  n_objects(maxframes), i, j, k, l, m1, m2, m3, n4,
     $  f1, f2, f3, f4, proposed_n(maxframes,*), npr_3, ns

      real*4
     $  coo_sys1(maxframes,n_o_max_cat,2), tol_d, dx_13, dy_13, dist_14,
     $  sina4, cosa4, x_expec, y_expec, ratio, max_mean_mo,
     $  max_mean_mo_2, min_mean_mo, min_mean_mo_2, min_day_m,
     $  max_day_m, min_motion_pix, tol_dis_lin

      real*8
     $  obs_time(maxframes), t1, t2, t3, t4

      logical
     $  decide

      character*20 images(maxframes)

      common /c1/min_day_m, max_day_m, tol_dis_lin, min_motion_pix
      common /c2/obs_time, n_objects, images
      common /c3/coo_sys1

      f1 = m1
      f2 = m2
      f3 = m3

      t1 = obs_time(f1)
      t2 = obs_time(f2)
      t3 = obs_time(f3)

      t4 = obs_time(f4)
      n4 = n_objects(f4)

      max_mean_mo = max_day_m*(t4-t1)
      max_mean_mo_2 = max_mean_mo**2
      min_mean_mo =  min_day_m*(t4-t1)
      min_mean_mo_2 = min_mean_mo**2

      ratio = (t4-t1)/(t3-t1)
      tol_d = tol_dis_lin**2

      do l = ns, npr_3
        i = proposed_n(f1,l)
	k = proposed_n(f3,l)
        if(i .ne.0) then
          dx_13 = coo_sys1(f3,k,1)-coo_sys1(f1,i,1)
	  dy_13 = coo_sys1(f3,k,2)-coo_sys1(f1,i,2)
          x_expec = ratio*dx_13+coo_sys1(f1,i,1)
          y_expec = ratio*dy_13+coo_sys1(f1,i,2)
          do j = 1,n4
            if(abs(x_expec-coo_sys1(f4,j,1)).lt.tol_dis_lin) then
            if(abs(y_expec-coo_sys1(f4,j,2)).lt.tol_dis_lin) then 
c           linear motion test
c            ---------------------------------------------------
            if((x_expec-coo_sys1(f4,j,1))**2+
     *         (y_expec-coo_sys1(f4,j,2))**2.lt.tol_d) then
c          ----------------------------------------------------
              dist_14 = sqrt((coo_sys1(f4,j,1)-coo_sys1(f1,i,1))**2
     *                      + (coo_sys1(f4,j,2)-coo_sys1(f1,i,2))**2)
              sina4 = (coo_sys1(f4,j,2)-coo_sys1(f1,i,2))/dist_14
              cosa4 = (coo_sys1(f4,j,1)-coo_sys1(f1,i,1))/dist_14 

c             test on minimum motion between two frames 
c             -----------------------------------------
	      decide = dist_14 .ge. min_motion_pix

              if(decide) then
                proposed_n(f4,l) = j
c                goto 5000
              end if
            end if
            end if
            end if
          end do
 5000   continue
        end if
      end do
      return
      end
