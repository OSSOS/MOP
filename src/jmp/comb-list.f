C -*-compile-command: "cf77 -O6 -o ../bin/LINUX/comb-list comb-list.f -L../lib/LINUX -ljmp; /bin/rm comb-list.o"; -*-

      include 'match-mov.h'

      integer*4
     $  i, j, k, narg, iargc, i1, i2, n1, n2, nc, corresp(n_o_max_cat),
     $  nf, yy, mm, id, jul, corresp1(n_o_max_cat)

      real*4
     $  x1(3,n_o_max_cat), y1(3,n_o_max_cat), x01(3,n_o_max_cat),
     $  y01(3,n_o_max_cat), flux1(3,n_o_max_cat), size1(3,n_o_max_cat),
     $  intens1(3,n_o_max_cat), elong1(3,n_o_max_cat),
     $  x2(3,n_o_max_cat), y2(3,n_o_max_cat), x02(3,n_o_max_cat),
     $  y02(3,n_o_max_cat), flux2(3,n_o_max_cat), size2(3,n_o_max_cat),
     $  intens2(3,n_o_max_cat), elong2(3,n_o_max_cat), dist(3), fwhm(3),
     $  fx(3), fy(3), mag, fra, fangle, d(3), ca, sa, dd

      real*8
     $  t(3), dt(3), pix_scale(3)

      character
     $  arg*80, basename*80, filename*80, header(24)*80, outname*80,
     $  line*80

      logical
     $  help, finished, keep, found1, found2,
     $  fake2(n_o_max_cat), ready, keep_all


c Create a file for later error handling

      open (unit=1, file='comb-list.FAILED', status='unknown')
      write (1, *) ' '
      close (1)

      keep_all = .false.
      narg = iargc()
      help = (narg .lt. 1)
      i = 1
 90   continue
      if (i .le. narg) then
         call getarg (i, arg)
         if ((arg(1:2) .eq. '-h') .or. (arg(1:2) .eq. '-?')) then
            i = narg + 1
            help = .true.
         end if
         if (arg(1:2) .eq. '-a') then
            i = narg +1
            keep_all = .true.
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

      open (unit=1, file='comb-list.FAILED', status='unknown')
      write (1, *) line(1:j-1)
      close (1)

      if (help) then
         write (6, *) 'Usage: comb-list image1'
         stop
      end if

      do j = 1, 80
         basename(j:j) = char(0)
      end do
      call getarg (1, basename)
      call read_file_name (basename, i1, i2, finished, 80)
      if (finished) stop
      filename = basename(i1:i2)//'.moving.jmp'
      open (unit=1, file=filename, status='old')
      do i = 1, 24
         read (1, '(a)') header(i)
      end do
      filename = basename(i1:i2)//'.moving.matt'
      open (unit=2, file=filename, status='old')
      do i = 1, 24
         read (2, *)
      end do

      do i = 1, 3
         read (header(i*6+1)(3:), *) yy, mm, dd, tmp, tmp, fwhm(i)
         read(header(i*6+3)(3:), *)  pix_scale(i)
         fwhm(i) = fwhm(i)**2
         id = int(dd)
         dd = dd - id
         jul = julday(mm, id, yy)
         t(i) = jul + dble(dd)
         dt(i) = (t(i) - t(1))*24.0
      end do


      n1 = 0
 100  continue
         read (1, *, err=110, end=110)
         n1 = n1 + 1
         do i = 1, 3
            read (1, *) x1(i,n1), y1(i,n1), x01(i,n1), y01(i,n1),
     $        flux1(i,n1), size1(i,n1), intens1(i,n1), elong1(i,n1)
         end do
         corresp1(n1)=0
      goto 100
 110  continue

      nc = 0
      n2 = 0
 200  continue
         read (2, *, err=210, end=210)
         n2 = n2 + 1
         do i = 1, 3
            read (2, *) x2(i,n2), y2(i,n2), x02(i,n2), y02(i,n2),
     $        flux2(i,n2), size2(i,n2), intens2(i,n2), elong2(i,n2)
         end do
         corresp(n2) = 0
         fake2(n2) = .false.
         j = 1
 202     continue
         if (j .le. n1) then
            keep = .true.
            do i = 1, 3
               dist(i) = (x01(i,j) - x02(i,n2))**2
     $           + (y01(i,j) - y02(i,n2))**2
               keep = keep .and. (dist(i) .lt. fwhm(i))
            end do
            if (keep) then
               corresp(n2) = j
               nc = nc + 1
               goto 203
            end if
            j = j + 1
            goto 202
         end if
 203     continue
      goto 200
 210  continue

      write (6, *) 'Found ', nc, ' common moving objects.'

      outname = basename(i1:i2)//'.cands.comb'
      filename = 'Object.planted'
      open (unit=14, file=filename, status='old', err=400)
      filename = basename(i1:i2)//'.jmp.found'
      open (unit=15, file=filename, status='unknown')
      filename = basename(i1:i2)//'.matt.found'
      open (unit=16, file=filename, status='unknown')
      filename = basename(i1:i2)//'.comb.found'
      open (unit=17, file=filename, status='unknown')
      filename = basename(i1:i2)//'.comb.missed'
      open (unit=18, file=filename, status='unknown')

      nf = 0
 300  continue
         read (14, *, err=300, end=350) fx(1), fy(1), mag, fra, fangle
         nf = nf + 1
         sa = sin(fangle/180.0*3.1415926539)
         ca = cos(fangle/180.0*3.1415926539)
         do i = 2, 3
            fx(i) = fx(1) + dt(i)*fra*ca/pix_scale(i)
            fy(i) = fy(1) + dt(i)*fra*sa/pix_scale(i)
         end do
         found1 = .false.
         found2 = .false.
         keep = .false.
         do i1 = 1, n1
            ready = .true.
            do i = 1, 3
               d(i) = (x01(i,i1) - fx(i))**2 + (y01(i,i1) - fy(i))**2
               ready = ready .and. (d(i) .lt. 4*fwhm(i))
            end do
            if (ready) then
               found1 = .true.
            end if
         end do
         if (found1) then
            write (15, 1003) fx(1), fy(1), mag, fra, fangle
         end if
         do i2 = 1, n2
            ready = .true.
            do i = 1, 3
               d(i) = (x02(i,i2) - fx(i))**2 + (y02(i,i2) - fy(i))**2
               ready = ready .and. (d(i) .lt. 4*fwhm(i))
            end do
            if (ready) then
               fake2(i2) = .true.
               found2 = .true.
               if (corresp(i2) .gt. 0) keep = .true.
            end if
         end do
         if (found2) then
            write (16, 1003) fx(1), fy(1), mag, fra, fangle
         end if
         if ( ((found1 .or. found2) .and. keep_all ) .or.
     $        (found1 .and. found2  .and. keep     )) then
            write (17, 1003) fx(1), fy(1), mag, fra, fangle
         else
            write (18, 1003) fx(1), fy(1), mag, fra, fangle
         end if
         goto 300
 350  continue
      close (14)
      close (15)
      close (16)
      close (17)
      close (18)

 400  continue
      ready = .false.
      do j = 1, n2
c        don't skip putting the fakes into the candidate file
c        if ((.not. fake2(j)) .and. (corresp(j) .ne. 0)) then
         k = j
         if ( corresp(j) .eq. 0 .and. .not. keep_all ) cycle
         if (corresp(j) .ne. 0) then
            corresp1(k)=j
         end if
         if (.not. ready) then
            open (unit=3, file=outname, status='unknown')
            do i = 1, 24
               write (3, '(a)') header(i)
            end do
            ready = .true.
         end if
         write (3, *)
         do i = 1, 3
            write (3, 1002) x2(i,k), y2(i,k), x02(i,k),
     $           y02(i,k), flux2(i,j), size2(i,j),
     $           intens2(i,j), elong2(i,j)
         end do
 410     continue
      end do
      if ( keep_all ) then 
         do j = 1, n1
c     Now print stuff that was only found in 1
            if (corresp1(j) .eq. 0) then
               if (.not. ready) then
                  open (unit=3, file=outname, status='unknown')
                  do i = 1, 24
                     write (3, '(a)') header(i)
                  end do
                  ready = .true.
               end if
               write (3, *)
               k = j
               do i = 1, 3
                  write (3, 1002) x1(i,k), y1(i,k), x01(i,k),
     $                 y01(i,k), flux1(i,j), size1(i,j),
     $                 intens1(i,j), elong1(i,j)
               end do
            end if
         end do
      end if

      close (1)
      close (2)
      if (ready) close (3)

 1002 format (4f8.2, f13.2, f9.1, f10.2, f6.2)
 1003 format (5f10.2)

c Apparently things went right

      open (unit=1, file='comb-list.OK', status='unknown')
      write (1, *) ' '
      close (1)

      stop
      end
