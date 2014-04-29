C -*-compile-command: "cf77 -O6 -o ../bin/step2jmp step2jmp.f; /bin/rm step2jmp.o"; -*-

      include 'match-mov.h'

      integer*4
     $  narg, iargc, indx(n_o_max_cat), ident(n_o_max_cat),
     $  ident2(n_o_max_cat), ident_with_others(maxframes,n_o_max_cat),
     $  n_cats(maxframes), i, j, n, i1, i2, nz, n_cat_1, n_cat_2,
     $  n_cat, nb_1, nb_2, nframes

      real*4
     $  inten_1(n_o_max_cat), inten_2(n_o_max_cat), flux_1(n_o_max_cat),
     $  flux_2(n_o_max_cat), size_o1(n_o_max_cat), size_o2(n_o_max_cat),
     $  inten_max_br_1, inten_max_br_2, inten_thresh_1,
     $  inten_thresh_2, inten_thresh, coo_1(2,n_o_max_cat),
     $  coo_2(2,n_o_max_cat), c_1_2(2,n_o_max_cat), dist12(n_o_max_cat),
     $  offset_x_with_others(maxframes),
     $  offset_y_with_others(maxframes), offset_x, offset_y,
     $  stan_dev_x, stan_dev_y, inten_max, tmp,
     $  tol_br_dist, min_dist_br, tol_dist_ident_cat,
     $  coo_1_br(2,n_br_max), coo_2_br(2,n_br_max),
     $  dist_1(n_br_max,n_br_max), dist_2(n_br_max,n_br_max),
     $  diff_cat_x(3*n_br_max), diff_cat_y(3*n_br_max)

      logical
     $  participate_1(3*n_br_max), participate_2(n_br_max), flag,
     $  finished, help, full_ident_cats_logic(maxframes,n_o_max_cat)

      character
     $  arg*100, line*80, frame_1*100, frame_2*100,
     $  bright_stars_cats(maxframes)*100, infile_cats(maxframes)*100,
     $  unidentif_file_cats(maxframes)*100


c Create a file for later error handling

      open (unit=1, file='step2jmp.FAILED', status='unknown')
      write (1, *) ' '
      close (1)

c     transform coordinates of cat2 (c00_2) in the system of cat1 :
c     ----------------------------------------------------------------  
c     c_2_in_sys1(1,*) = coo_2(1,*)-offset_x_with_others 
c     c_2_in_sys1(2,*) = coo_2(2,*)-offset_y_with_others   

      do i = 1,maxframes
        offset_x_with_others(i) = 0.e0
        offset_y_with_others(i) = 0.e0
        do j = 1,n_o_max_cat
          ident_with_others(i,j) = -1
          full_ident_cats_logic(i,j) = .false.
        end do
      end do

      narg = iargc()
      help = (narg .ne. 3)
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

      if (help) then
         write (6, *) 'Usage: step2jmp image1 image2 image3'
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
      unidentif_file_cats(1) = frame_1(i1:i2)//'.unid.jmp'
      bright_stars_cats(1) = frame_1(i1:i2)//'.bright.jmp'

      nframes = 1

      open(1,file=infile_cats(1),status='old')
      read (1, 104) tmp, tmp, tmp, tol_dist_ident_cat, inten_max
      read (1, *)
      tol_dist_ident_cat = tol_dist_ident_cat/4.
 104  format (2x, f16.7, f8.2, 2f6.2, f9.1)
      n = 1
    1 continue
         read(1,*,end=2)
     *     coo_1(1,n),coo_1(2,n),
     *     flux_1(n), size_o1(n), inten_1(n)
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
      inten_max = 0.8*inten_max

      n_cats(1) = n_cat_1
 2000 format(a,T40,i10)

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
      unidentif_file_cats(nframes) = frame_2(i1:i2)//'.unid.jmp'
      bright_stars_cats(nframes) = frame_2(i1:i2)//'.bright.jmp'

c     read coordinates of cat_2
c     -------------------------
      open(1,file=infile_cats(nframes),status='old')
      read (1, *)
      read (1, *)
      n = 1
   21 read(1,*,end=22)
     *       coo_2(1,n),coo_2(2,n),
     *       flux_2(n), size_o2(n), inten_2(n)
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

      call match_two_cats (coo_1, coo_2, offset_x, offset_y, ident,
     $  ident2, dist12, n_cat_1, n_cat_2, tol_dist_ident_cat, c_1_2)

      do i = 1,n_cat_1
        if(ident(i).ne.0) then
          j = ident(i)
          tmp = flux_1(i)/flux_2(j)
          if ((tmp .ge. 0.2) .and. (tmp .le. 5.)) then
             tmp = size_o1(i)/size_o2(j)
             if ((tmp .ge. .25) .and. (tmp .le. 4.))
     $         ident_with_others(nframes,i) = j
          end if
        end if
      end do

      offset_x_with_others(nframes) = offset_x
      offset_y_with_others(nframes) = offset_y

c End of catalog loop
c -------------------
      goto 10

  100 continue

c     construct star identification table
c     -----------------------------------
      nz = 0
      do i = 1,n_cat_1
        flag = .true.
        do n = 2,nframes
           flag = flag .and. (ident_with_others(n,i) .gt. 0)
        end do
        full_ident_cats_logic(1,i) = flag
        if (flag) then
           nz = nz + 1
           do n = 2, nframes
              i2 = ident_with_others(n,i)
              full_ident_cats_logic(n,i2) = .true.
           end do
        end if
      end do
      write(6,2001) 'number of fully identified stars', nz

c     produce outputfiles
c     -------------------
      do n = 1,nframes
	open(7,file=infile_cats(n),status='old')
	open(9,file=unidentif_file_cats(n),status='unknown')
	open(4,file=bright_stars_cats(n),status='unknown')
        read (7, '(a)') line
        write (9, '(a)') line
        write (4, '(a)') line
        read (7, '(a)') line
        write (9, '(a)') line
        write (4, '(a)') line
        n_cat = n_cats(n)

c       find bright fully identified stars
c       ----------------------------------
        nz = 0
        do i = 1,n_cat
           read (7, *) coo_1(1,i), coo_1(2,i), flux_1(i), size_o1(i),
     $       inten_1(i), flux_2(i)
           coo_2(1,i) = coo_1(1,i)-offset_x_with_others(n)
           coo_2(2,i) = coo_1(2,i)-offset_y_with_others(n)
           if (full_ident_cats_logic(n,i)) then
              if (inten_1(i) .le. inten_max_br_1) then
                 nz = nz+1
                 inten_2(nz) = inten_1(i)
              end if
           else
              write (9, 2016) coo_1(1,i), coo_1(2,i), coo_2(1,i),
     $          coo_2(2,i), flux_1(i), size_o1(i), inten_1(i),
     $          flux_2(i)
           end if
        end do
        if (n .eq. 1) then
           call sortreal (nz,inten_2,indx)
           i = nz-n_br_max
           if (i .le. 0) i = 1
           j = indx(i)
           inten_thresh = inten_2(j)
           do i = 1, n_cats(1)
              if (full_ident_cats_logic(1,i)) then
                 if ((inten_1(i) .ge. inten_thresh) .and.
     $             (inten_1(i) .le. inten_max_br_1)) then
                    write (4, 2016) coo_1(1,i), coo_1(2,i), coo_2(1,i),
     $                coo_2(2,i), flux_1(i), size_o1(i), inten_1(i),
     $                flux_2(i)
                    ident_with_others(1,i) = 1
                 end if
              end if
           end do
        else
           do i = 1,n_cats(1)
              if (ident_with_others(1,i) .eq. 1) then
                 i2 = ident_with_others(n,i)
                 write (4, 2016) coo_1(1,i2), coo_1(2,i2), coo_2(1,i2),
     $             coo_2(2,i2), flux_1(i2), size_o1(i2), inten_1(i2),
     $             flux_2(i2)
              end if
           end do
        end if
 2016 format(4f8.2, f13.2, f9.1, f10.2, f6.2)

        close(4)
        close(7)
        close(9)       
      end do

c Apparently things went right

      open (unit=1, file='step2jmp.OK',
     $  status='unknown')
      write (1, *) ' '
      close (1)

      stop
      end

      include 'match_two_cats.f'
      include 'read_file_name.f'
      include 'sort.f'
