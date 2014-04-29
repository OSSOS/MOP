C -*-compile-command: "cf77 -O6 -o ../bin/LINUX/step2bjmp step2bjmp.f -L ../lib/LINUX -ljmp; /bin/rm step2bjmp.o"; -*-

      implicit none

      include 'match-mov.h'

      real*8
     $  pi

      parameter
     $  (pi = 3.141592653589793)

      integer*4
     $  i, i1, i2, j, n, n1(maxframes), n1_br(maxframes), nf, naxis1,
     $  naxis2, narg, iargc, indx1(n_br_max,maxframes), n_c,
     $  ident1(n_o_max_cat,maxframes), k, n_minb, n_max,
     $  indxt1(n_o_max_cat), indxt2(n_o_max_cat)

      real*4
     $  x1(n_o_max_cat,maxframes), y1(n_o_max_cat,maxframes),
     $  x1_sys1(n_o_max_cat,maxframes), y1_sys1(n_o_max_cat,maxframes),
     $  flux1(n_o_max_cat,maxframes), inten1(n_o_max_cat,maxframes),
     $  size1(n_o_max_cat,maxframes), el1(n_o_max_cat,maxframes), isol,
     $  separ, inten_max, fwhm(maxframes), tol_dist,
     $  tmpr(n_o_max_cat), max_dist, tmp, tmpf,
     $  x1_br(n_br_max,maxframes), y1_br(n_br_max,maxframes), mopvers

      real*8
     $  coeff(20)

      character
     $  arg*100, line*80, frame*100, bright_stars(maxframes)*100,
     $  infile(maxframes)*100, unident_obj(maxframes)*100,
     $  header(7,maxframes)*80, trans_cat(maxframes)*100,
     $  ident_obj(maxframes)*100

      logical
     $  help, finished, ok(n_o_max_cat), alone(n_o_max_cat),
     $  identified(n_o_max_cat,maxframes)

      external trans

c Create a file for later error handling

      open (unit=1, file='step2bjmp.FAILED', status='unknown')
      write (1, *) ' '
      close (1)

c Get input parameters

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

      open (unit=1, file='step2bjmp.FAILED', status='unknown')
      write (1, *) line(1:j-1)
      close (1)

      if (help) then
         write (6, *) 'Usage: step2bjmp image1 image2 image3'
         stop
      end if

      separ = 40.
      n_max = nint(float(n_br_max)*2./3.)
      n_c = 3

c Gets name of first catalog.

      nf = 1

      do j = 1, 80
         frame(j:j) = char(0)
      end do
      call getarg (1, frame)
      call read_file_name (frame, i1, i2, finished, 80)
      if (finished) stop
      infile(1) = frame(i1:i2)//'.obj.jmp'
      unident_obj(1) = frame(i1:i2)//'.unid.jmp'
      ident_obj(1) = frame(i1:i2)//'.id.jmp'
      bright_stars(1) = frame(i1:i2)//'.bright.jmp'
      trans_cat(1) = frame(i1:i2)//'.trans.jmp'

c Reads in transform.

      open (1, file=trans_cat(1), status='unknown')
      read (1, *) (coeff(i), i=1,2*n_c)
      close (1)

c Reads in first catalog.

      open (1, file=infile(1), status='old')
      do i = 1, 7
         read (1, '(a)') header(i,nf)
      end do
      read (header(2,nf)(3:), *) mopvers
      i = i2 - i1 + 9
      call check_version (mopvers, infile(1), i, j)
      read (header(4,nf)(33:), *) fwhm(nf)
      read (header(6,nf)(33:), *) naxis1, naxis2
      tol_dist = fwhm(nf)/4.0

      n = 1
    1 continue
         read (1,*,end=2,err=1) x1(n,nf), y1(n,nf), flux1(n,nf),
     $     size1(n,nf), inten1(n,nf), el1(n,nf), tmp, tmp, tmp, tmpf
         flux1(n,nf) = flux1(n,nf)*tmpf
         x1_sys1(n,nf) = x1(n,nf)
         y1_sys1(n,nf) = y1(n,nf)
         if (inten1(n,nf) .gt. inten_max) inten_max = inten1(n,nf)
         n = n+1

         if (n .gt. n_o_max_cat) then
            write (6,*) 'increase n_o_max_cat in match-mov.h'
            stop
         end if
      goto 1
    2 continue
      n1(nf) = n-1
      close (1)
      inten_max = 0.8*inten_max
      max_dist = amin0(naxis1, naxis2)/4.

c Apply transform to every body.

      call trans_xy (x1(1,nf), y1(1,nf), n1(nf), x1_sys1(1,nf),
     $  y1_sys1(1,nf), coeff, n_c, trans)

c Gets name of other catalogs.

   10 continue
      if (nf .ge. narg) goto 100
      nf = nf + 1
      if (nf .gt. maxframes) then
         write(6,2001) 'nf .gt. maxframes',nf,maxframes
         write(6,*) 'increase maxframes in match-mov.h'
         stop
      end if

      do j = 1, 80
         frame(j:j) = char(0)
      end do
      call getarg (nf, frame)
      call read_file_name (frame, i1, i2, finished, 80)
      if (finished) stop
      infile(nf) = frame(i1:i2)//'.obj.jmp'
      unident_obj(nf) = frame(i1:i2)//'.unid.jmp'
      ident_obj(nf) = frame(i1:i2)//'.id.jmp'
      bright_stars(nf) = frame(i1:i2)//'.bright.jmp'
      trans_cat(nf) = frame(i1:i2)//'.trans.jmp'

c Reads in transform.

      open (unit=1, file=trans_cat(nf), status='unknown')
      read (1, *) (coeff(i), i=1,2*n_c)
      close (1)

c Reads in other catalog.

      open (1, file=infile(nf), status='old')
      do i = 1, 7
         read (1, '(a)') header(i,nf)
      end do
      read (header(2,nf)(3:), *) mopvers
      i = i2 - i1 + 9
      call check_version (mopvers, infile(nf), i, j)
      read (header(4,nf)(33:), *) fwhm(nf)

      n = 1
 21   continue
         read(1,*,end=22,err=21) x1(n,nf), y1(n,nf), flux1(n,nf),
     $     size1(n,nf), inten1(n,nf), el1(n,nf), tmp, tmp, tmp, tmpf
         flux1(n,nf) = flux1(n,nf)*tmpf
         n = n+1

         if(n.gt.n_o_max_cat) then
            write(6,*) 'increase n_o_max_cat in match-mov.h'
            stop
         end if
      goto 21
 22   continue
      n1(nf) = n-1
      close (1)

c Apply transform to every body.

      call trans_xy (x1(1,nf), y1(1,nf), n1(nf), x1_sys1(1,nf),
     $  y1_sys1(1,nf), coeff, n_c, trans)

c Now find the matching stars.

      call match_lists_new (x1_sys1(1,1), y1_sys1(1,1), ident1(1,nf),
     $  n1(1), x1_sys1(1,nf), y1_sys1(1,nf), ident1(1,1), n1(nf),
     $  tmpr, tol_dist, indxt1)

c Check for fortuituous matching of unlike objects.

      k = 0
      do i = 1, n1(1)
         if(ident1(i,nf).ne.0) then
            j = ident1(i,nf)
            tmp = flux1(i,1)/flux1(j,nf)
            if ((tmp .ge. 0.2) .and. (tmp .le. 5.)) then
               tmp = size1(i,1)/size1(j,nf)
               if ((tmp .ge. .25) .and. (tmp .le. 4.)) then
                  k = k + 1
               else
                  ident1(i,nf) = 0
               end if
            else
               ident1(i,nf) = 0
            end if
         end if
      end do

c End of catalog loop.

      goto 10

 100  continue

c We have read all the catalogs.
c Find stars identified on all frames.

      do j = 1, nf
         do i = 1, n1(j)
            identified(i,j) = .false.
         end do
      end do
      k = 0
      do i = 1, n1(1)
         help = .true.
         do n = 2, nf
            help = help .and. (ident1(i,n) .gt. 0)
         end do
         identified(i,1) = help
         if (help) then
            k = k + 1
            do n = 2, nf
               i2 = ident1(i,n)
               identified(i2,n) = .true.
            end do
         end if
      end do

      write (6, *) k, ' fully identified stars.'

c Writes output files

      do n = 1, nf
         open (unit=9, file=unident_obj(n), status='unknown')
         open (unit=8, file=ident_obj(n), status='unknown')
         open (unit=4, file=bright_stars(n), status='unknown')
         do i = 1, 6
            write (9, '(a)') header(i,n)
            write (8, '(a)') header(i,n)
            write (4, '(a)') header(i,n)
         end do
         write (9, '(a)') '##   X       Y     X_0     Y_0'
     $     //'        FLUX       SIZE   MAX_INT  ELON'
         write (8, '(a)') '##   X       Y     X_0     Y_0'
     $     //'        FLUX       SIZE   MAX_INT  ELON'
         write (4, '(a)') '##   X       Y     X_0     Y_0'
     $     //'        FLUX       SIZE   MAX_INT  ELON'

c Writes file of unidentified objects.

         do i = 1, n1(n)
            if (.not. identified(i,n)) then
               write (9, 2016) x1(i,n), y1(i,n), x1_sys1(i,n),
     $           y1_sys1(i,n), flux1(i,n), size1(i,n), inten1(i,n),
     $           el1(i,n)
            else
               write (8, 2016) x1(i,n), y1(i,n), x1_sys1(i,n),
     $           y1_sys1(i,n), flux1(i,n), size1(i,n), inten1(i,n),
     $           el1(i,n)
            end if
         end do

c Writes file of bright stars.

         if (n .eq. 1) then

c Get more bright stars.

            n_minb = int(float(n_br_max)*2.0/3.0)
            n_max = int(float(n_br_max)*3.0/4.0)
            isol = fwhm(1)
            call select_bright (x1(1,1), y1(1,1), inten1(1,1), n1(1),
     $        x1_br(1,1), y1_br(1,1), indx1(1,1), n1_br(1), inten_max,
     $        isol, separ, n_minb, n_max, ok, alone, tmpr, indxt1,
     $        indxt2)

            do i = 1, n1(1)
               ident1(i,1) = 0
            end do
            do j = 1, n1_br(1)
               i = indx1(j,1)
c               write (50, *) j, x1_br(j,1), y1_br(j,1), i, x1(i,n),
c     $           y1(i,n), identified(i,1)
               if (identified(i,1)) then
                  write (4, 2016) x1(i,n), y1(i,n), x1_sys1(i,n),
     $              y1_sys1(i,n), flux1(i,n), size1(i,n), inten1(i,n),
     $              el1(i,n)
                  ident1(i,1) = 1
              end if
            end do
         else
            do k = 1, n1_br(1)
               j = indx1(k,1)
               if (ident1(j,1) .eq. 1) then
                  i = ident1(j,n)
                  write (4, 2016) x1(i,n), y1(i,n), x1_sys1(i,n),
     $              y1_sys1(i,n), flux1(i,n), size1(i,n), inten1(i,n),
     $              el1(i,n)
               end if
            end do
         end if

         close (9)
         close (8)
         close (4)
      end do

c Apparently things went right

      open (unit=1, file='step2bjmp.OK', status='unknown')
      write (1, *) ' '
      close (1)

 2001 format (a, T40, 2i10)
 2016 format (4f8.2, f13.2, f9.1, f10.2, f6.2)
      stop

      end
