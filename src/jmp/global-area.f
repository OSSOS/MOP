C -*-compile-command: "g77 -O3 -o ../../bin/LINUX/global-area global-area.f -L../../lib/LINUX -ljmp"; -*-

      implicit none

      integer*4
     $  i, j, nm_max, lp, narg, iargc, i2, i1, ld, lf, nr_max,
     $  naxis1, naxis2

      parameter
     $  (nm_max = 3000, nr_max = 100)

      real*4
     $  area, pixscale

      character
     $  arg*80, dirname*200, filename*100, listname*80, pref*80,
     $  line*200

      logical help, finished, no_f


c Create a file for later error handling

      open (unit=1, file='global-area.FAILED',
     $  status='unknown')
      write (1, *) ' '
      close (1)

      do j = 1, 80
         pref(j:j) = char(0)
      end do
      narg = iargc()
      no_f = .true.
      help = (narg .ne. 2)
      i = 1
 90   continue
      if (i .le. narg) then
         call getarg (i, arg)
         if (arg(1:2) .eq. '-f') then
            i = i + 1
            call getarg (i, listname)
            no_f = .false.
         else if ((arg(1:2) .eq. '-h') .or. (arg(1:2) .eq. '-?')) then
            i = narg + 1
            help = .true.
         end if
         i = i + 1
         goto 90
      end if

      help = help .or. no_f

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

      open (unit=1, file='global-area.FAILED',
     $  status='unknown')
      write (1, *) line(1:j-1)
      close (1)

      if (help) then
         write (6, *) 'Usage: global-area -f <file list>'
         stop
      end if

      area = 0.
      open (unit=42, file=listname, status='old')

 100  continue
      read (42, '(a)', end = 400) line

      call read_file_name (line, i1, i2, finished, 200)
c      if (finished) stop
      dirname = line(i1:i2)
      ld = i2 - i1 + 1

      line = line(i2+1:)
      lp = 200 - i2
      call read_file_name (line, i1, i2, finished, lp)
c      if (finished) stop
      pref = line(i1:i2)
      lf = i2 - i1 + 1

      pixscale = 0.185
      naxis1 = 2047
      naxis2 = 4612
      filename = dirname(1:ld)//pref(1:lf)//'.mopheader'
c      write (6, *) dirname(1:ld), pref(1:lf), filename
      open (unit=16, file=filename, status='old', err=500)
 150  continue
         read (16, '(a)', err=150, end=160) line
         if (line(1:8) .eq. 'PIXSCALE') then
            read (line(10:), *) pixscale
         else if (line(1:8) .eq. 'NAXIS1  ') then
            read (line(10:), *) naxis1
         else if (line(1:8) .eq. 'NAXIS2  ') then
            read (line(10:), *) naxis2
         end if
         goto 150
 160  continue
      close (16)

      area = area + pixscale**2*float(naxis1*naxis2)/3600.**2

      goto 100

 400  continue
      close (42)

      call read_file_name (listname, i1, i2, finished, 80)
      filename = listname(i1:i2)//'.area'
      open (unit=3, file=filename, status='unknown')
      write (3, '(a18, f5.2)') '# area (sq.deg.): ', area
      close (3)

c Apparently things went right

      open (unit=1, file='global-area.OK', status='unknown')
      write (1, *) ' '
      close (1)
      stop

 500  continue
      write (6, *) 'Could not find '//filename
      write (6, *) 'Skipping to next file.'
      goto 100

      end
