C -*-compile-command: "cf77 -O6 -o ../bin/LINUX/recover recover.f -L../lib/LINUX -ljmp; /bin/rm recover.o"; -*-

      implicit none

      integer*4
     $  narg, iargc, nframes, i, i1, i2, j

      parameter
     $  (nframes = 3)

      real*4
     $  x_cut, y_cut, x_off, y_off,
     $  x(nframes), y(nframes), x_sys1(nframes), y_sys1(nframes)

      logical
     $  no_f, no_b, help, first, finished

      character*80
     $  ast_name, bright, line(nframes), arg

      external iargc

c Create a file for later error handling

      open (unit=1, file='recover.FAILED', status='unknown')
      write (1, *) ' '
      close (1)

      narg = iargc()
      i = 1
      no_f = .true.
      no_b = .true.
      help = (narg .le. 3)

 90   continue
      if (i .le. narg) then
         call getarg (i, arg)
         if (arg(1:2) .eq. '-f') then
            i = i + 1
            call getarg (i, ast_name)
            no_f = .false.
         else if (arg(1:2) .eq. '-b') then
            i = i + 1
            call getarg (i, bright)
            no_b = .false.
         else if ((arg(1:2) .eq. '-h') .or. (arg(1:2) .eq. '-?')) then
            help = .true.
            i = i + 1
        end if
         i = i + 1
         goto 90
      end if

      j = 1
      do i = 0, narg
         call getarg (i, arg)
         call read_file_name (arg, i1, i2, finished, 80)
         line(1)(j:j+i2-i1) = arg(i1:i2)
         j = j + i2 - i1 + 1
         line(1)(j:j) = ' '
         j = j + 1
      end do
      write (6, *) line(1)(1:j-1)

      open (unit=1, file='recover.FAILED', status='unknown')
      write (1, *) line(1)(1:j-1)
      close (1)

      help = (no_f .or. no_b) .or. help

      if (help) then
         write (6, *) 'Usage: recover -f <ast_file> -b <bright>'
         write (6, *) 'where:'
         write (6, *) '-f <ast_file>: file name of moving objects'
         write (6, *) '-b <bright>: file name of bright objects'
         stop
      end if

      call read_file_name (ast_name, i1, i2, finished, 80)
      if (finished) stop 'No candidate file given.'
      if (ast_name(i2-6:i2) .eq. '.astrom') then
         write (6, *) 'You are trying to run this program on an'
     $     //' astrometry file.'
         write (6, *) 'Re-run it with a candidate file.'
         stop
      end if

      call read_file_name (bright, i1, i2, finished, 80)
      if (finished) stop 'No bright star file given.'
      if (index(bright, '.bright') .eq. 0) then
         write (6, *) 'It seems that ', bright(i1:i2), ' does''nt'
     $     //' contain bright stars.'
         write (6, *) 'Re-run it with a bright star file.'
         stop
      end if

      open (unit=21, file=ast_name, status='old', err=9000)
      open (unit=11, file='hans.ast', status='unknown')
      open (unit=12, file='hans.list', status='unknown')
      open (unit=13, file='hans.ref', status='unknown')
      open (unit=15, file='hans.shift', status='unknown')

      do i = 1, nframes
         read (21, '(a)') line(i)
         write (12, '(a)') line(i)(3:)
      end do
      write (13, '(a)') line(1)(3:)

      do i = 1, 6*nframes+3
         read (21, *)
      end do

      first = .true.
 1000 continue
         read (21, *, end=1100)
         do i = 1, nframes
            read (21, *) x(i), y(i), x_sys1(i), y_sys1(i)
         end do
         if (first) then
            x_cut = 0.
            y_cut = 0.
            do i = 1, nframes
               x_off = x_sys1(i) - x(i)
               y_off = y_sys1(i) - y(i)
               if (x_off .gt. x_cut) x_cut = x_off
               if (y_off .gt. y_cut) y_cut = y_off
               write (15, *) x_off, y_off
            end do
            first = .false.
         end if
         do i = 1, 3
            write (11, '(f7.2, 1x, f7.2)')
     $        x_sys1(i) - x_cut, y_sys1(i) - y_cut
         end do
      goto 1000
 1100 continue
      close (21)
      close (11)
      close (12)
      close (13)
      close (15)

      open (unit=22, file=bright, status='old', err=9010)
      open (unit=14, file='hans.coo', status='unknown')

      do i = 1, 7
         read (22, *)
      end do

 1200 continue
         read (22, *, end=1300) x(1), y(1)
         write (14, *) x(1), y(1)
      goto 1200
 1300 continue

      close (22)
      close (14)

c Apparently things went right

      open (unit=1, file='recover.OK', status='unknown')
      write (1, *) ' '
      close (1)

      stop

 9000 continue
      write (6, *) 'Error opening '//ast_name
      stop

 9010 continue
      write (6, *) 'Error opening '//bright
      stop
      end
