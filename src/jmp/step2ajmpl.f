C -*-compile-command: "cf77 -O6 -o ../bin/LINUX/step2ajmpl step2ajmpl.f -L ../lib/LINUX -ljmp; /bin/rm step2ajmpl.o"; -*-

      include 'match-mov.h'

      integer*4
     $  narg, iargc, n_cats(maxframes), i, j, n, i1, i2, n_cat_1,
     $  n_cat_2, nb_1, nb_2, nframes

      real*4
     $  inten_1(n_o_max_cat), inten_2(n_o_max_cat), flux_1(n_o_max_cat),
     $  flux_2(n_o_max_cat), size_o1(n_o_max_cat), size_o2(n_o_max_cat),
     $  inten_max_br_1, inten_max_br_2, inten_thresh_1,
     $  inten_thresh_2, coo_1(2,n_o_max_cat),
     $  coo_2(2,n_o_max_cat), offset_x, offset_y,
     $  stan_dev_x, stan_dev_y, inten_max, tmp, tmpf,
     $  tol_br_dist, min_dist_br, tol_dist_ident_cat,
     $  coo_1_br(2,n_br_max), coo_2_br(2,n_br_max),
     $  dist_1(n_br_max,n_br_max), dist_2(n_br_max,n_br_max),
     $  diff_cat_x(3*n_br_max), diff_cat_y(3*n_br_max), mopvers

      logical
     $  participate_1(3*n_br_max), participate_2(n_br_max),
     $  finished, help

      character
     $  arg*100, line*80, frame_1*100, frame_2*100, header(7)*80,
     $  infile_cats(maxframes)*100, transf(maxframes)*100

      external iargc

c Create a file for later error handling

      open (unit=1, file='step2ajmpl.FAILED', status='unknown')
      write (1, *) ' '
      close (1)

c     transform coordinates of cat2 (c00_2) in the system of cat1 :
c     ----------------------------------------------------------------  
c     c_2_in_sys1(1,*) = coo_2(1,*)-offset_x_with_others 
c     c_2_in_sys1(2,*) = coo_2(2,*)-offset_y_with_others   

      narg = iargc()
      help = (narg .ne. 3) .and. (narg .ne. 2)
      i = 1
 90   continue
      if (i .le. narg) then
         call getarg (i, arg)
         if ((arg(1:2) .eq. '-h') .or. (arg(1:2) .eq. '-?')) then
            i = narg + 1
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

      open (unit=1, file='step2ajmpl.FAILED', status='unknown')
      write (1, *) line(1:j-1)
      close (1)

      if (help) then
         write (6, *) 'Usage: step2jmp image1 image2 [image3]'
         stop
      end if

      tol_br_dist = 0.3
      min_dist_br = 30.

c     read coordinates of cat 1
c     -------------------------
      do j = 1, 80
         frame_1(j:j) = char(0)
      end do
      call getarg (1, frame_1)
      call read_file_name (frame_1, i1, i2, finished, 80)
      if (finished) stop
      infile_cats(1) = frame_1(i1:i2)//'.obj.jmp'
      transf(1) = frame_1(i1:i2)//'.trans.jmp'

c First frame is reference frame. So transform is identity.

      open (1, file=transf(1), status='unknown')
      write (1, '(''0. 1. 0. 0. 0. 1.'')')
      close (1)

      nframes = 1

      open(1,file=infile_cats(1),status='old')
      do i = 1, 7
         read (1, '(a)') header(i)
      end do
      read (header(2)(3:), *) mopvers
      i = i2 - i1 + 9
      call check_version (mopvers, infile_cats(1), i, j)
      read (header(4)(33:), *) tol_dist_ident_cat, inten_max
      tol_dist_ident_cat = tol_dist_ident_cat/4.
      n = 1
    1 continue
         read(1,*,end=2,err=1)
     *     coo_1(1,n),coo_1(2,n),
     *     flux_1(n), size_o1(n), inten_1(n), tmp, tmp, tmp, tmp, tmpf
         if (inten_1(n) .gt. inten_max) inten_max = inten_1(n)
         n = n+1

         if(n.gt.n_o_max_cat) then
            write(6,*) 'increase n_o_max_cat in '
            write(6,*) 'match-mov.h'
            stop
         end if
      goto 1
    2 continue
      n_cat_1 = n-1
      close(1)
c     BG/LA change to account for high pix values with CFH12k
      if (inten_max.gt.65536.0) then
         inten_max = 65536.0
      endif
      inten_max = 0.8*inten_max

      n_cats(1) = n_cat_1

c     determine intensity range for bright objects in cat1
c     ----------------------------------------------------
      call select_bright_old (n_cat_1, inten_max_br_1,
     $  inten_thresh_1, coo_1, coo_1_br, inten_1, dist_1,
     $  participate_1, nb_1, min_dist_br, n_br_max, inten_max)

c*************************************************************
c     read cats to be compared with cat1
c     ----------------------------------
   10 continue
      if (nframes .ge. narg) goto 100
      nframes = nframes + 1
      if (nframes .gt. maxframes) then
         write(6,2001) 'nframes .gt. maxframes',nframes,maxframes
         write(6,*) 'increase maxframes in match-mov.h'
         stop
      end if

      do j = 1, 80
         frame_2(j:j) = char(0)
      end do
      call getarg (nframes, frame_2)
      call read_file_name (frame_2, i1, i2, finished, 80)
      if (finished) stop

c     construct filenames related to cat
c     ----------------------------------
 2001 format(a,T40,2i10)
      infile_cats(nframes) = frame_2(i1:i2)//'.obj.jmp'
      transf(nframes) = frame_2(i1:i2)//'.trans.jmp'

c     read coordinates of cat_2
c     -------------------------
      open(1,file=infile_cats(nframes),status='old')
      do i = 1, 7
         read (1, '(a)') header(i)
      end do
      read (header(2)(3:), *) mopvers
      i = i2 - i1 + 9
      call check_version (mopvers, infile_cats(nframes), i, j)
      n = 1
   21 continue
         read(1,*,end=22,err=21)
     *     coo_2(1,n),coo_2(2,n),
     *     flux_2(n), size_o2(n), inten_2(n), tmp, tmp, tmp, tmp, tmpf
         n = n+1

         if(n.gt.n_o_max_cat) then
            write(6,*) 'increase n_o_max_cat in '
            write(6,*) 'match-mov.h'
            stop
         end if
      goto 21
   22 n_cat_2 = n-1
      close(1)

      n_cats(nframes) = n_cat_2

c     determine intensity range for bright objects in cat2
c     ----------------------------------------------------
      call select_bright_old (n_cat_2, inten_max_br_2,
     $  inten_thresh_2, coo_2, coo_2_br, inten_2, dist_2,
     $  participate_2, nb_2, min_dist_br, n_br_max, inten_max)

      call register_two_cats (nb_1, nb_2, coo_1_br,
     $  coo_2_br, offset_x, offset_y, stan_dev_x, stan_dev_y,
     $  dist_1, dist_2, participate_1, participate_2,
     $  tol_br_dist, diff_cat_x, diff_cat_y, n_br_max)

      if ((stan_dev_x .gt. 1.0e0) .or. (stan_dev_y .gt. 1.0e0)) then
         write (6, *) 'No transformation. 4'
c         write(6,*) ' '
c         write(6,*) 'problem in comparison of ',frame_1,'  ',frame_2 
c         write(6,*) 'press return to continue'
         stop
c         read(5,*)
      end if

c Stores the transform

      open (unit=1, file=transf(nframes), status='unknown')
      write (1, '(f8.2,1x,f10.8,1x,e14.8,f8.2,1x,e14.8,1x,f10.8)')
     $  -offset_x, 1., 0., -offset_y, 0., 1.
      close (1)

c End of catalog loop
c -------------------
      goto 10

  100 continue

c Apparently things went right

      open (unit=1, file='step2ajmpl.OK', status='unknown')
      write (1, *) ' '
      close (1)

      stop
      end
