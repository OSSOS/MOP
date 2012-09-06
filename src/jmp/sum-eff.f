C -*-compile-command: "cf77 -O6 -o ../bin/LINUX/sum-eff sum-eff.f; /bin/rm sum-eff.o"; -*-

C     takes the found and missed files and creates an efficiency of detection for both/jmp/matt in the
C     same output file.  The actual numbers of add/detected objects are returned instead of a detection 
C     fraction with errors.

      implicit none

      integer*4
     $  i, i_mag, j, n_max, lp, narg, iargc, i2, i1

      parameter
     $  (n_max = 3000)

      real*4
     $  mag, fx, fy, fra, mag_min, mag_step, hfa(n_max), hfo(n_max),
     $  frac, fangle, sigma, jmpfo(n_max), mattfo(n_max)

      character
     $  arg*80, filename*80, pref*80, line*80

      logical
     $  help, finished, no_s, no_m, no_f

      external iargc

c Create a file for later error handling

      open (unit=1, file='sum-eff.FAILED', status='unknown')
      write (1, *) ' '
      close (1)

      do j = 1, 80
         pref(j:j) = char(0)
      end do
      narg = iargc()
      no_f = .true.
      no_m = .true.
      no_s = .true.
      help = (narg .ne. 6)
      i = 1
 90   continue
      if (i .le. narg) then
         call getarg (i, arg)
         if (arg(1:2) .eq. '-f') then
            i = i + 1
            call getarg (i, pref)
            no_f = .false.
         else if (arg(1:2) .eq. '-s') then
            i = i + 1
            call getarg (i, arg)
            read (arg, *) mag_step
            no_s = .false.
         else if (arg(1:2) .eq. '-m') then
            i = i + 1
            call getarg (i, arg)
            read (arg, *) mag_min
            no_m = .false.
         else if ((arg(1:2) .eq. '-h') .or. (arg(1:2) .eq. '-?')) then
            i = narg + 1
            help = .true.
         end if
         i = i + 1
         goto 90
      end if

      help = help .or. no_f .or. no_m .or. no_s

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

      open (unit=1, file='sum-eff.FAILED', status='unknown')
      write (1, *) line(1:j-1)
      close (1)

      if (help) then
         write (6, *) 'Usage: sum-eff -f image1 -m <min mag>'
     $        //' -s <mag step>'
         stop
      end if

      do i = 1, n_max
         hfa(i) = 0.
         hfo(i) = 0.
         mattfo(i)=0.
         jmpfo(i)=0.
      end do

      call read_file_name (pref, i, lp, finished, 80)
      if (finished) stop

      filename = pref(1:lp)//'.jmp.found'
      open (unit=17, file=filename, status='old', err=500)
 200  continue
         read (17, *, err=200, end=210) fx, fy, mag, fra, fangle
         i_mag = int((mag-mag_min)/mag_step) + 1
         if ((i_mag .gt. 0) .and. (i_mag .le. n_max)) then
            jmpfo(i_mag) = jmpfo(i_mag) + 1.
         end if
         goto 200
 210  continue
      close (17)

      filename = pref(1:lp)//'.matt.found'
      open (unit=17, file=filename, status='old', err=500)
 201  continue
         read (17, *, err=201, end=211) fx, fy, mag, fra, fangle
         i_mag = int((mag-mag_min)/mag_step) + 1
         if ((i_mag .gt. 0) .and. (i_mag .le. n_max)) then
            mattfo(i_mag) = mattfo(i_mag) + 1.
         end if
         goto 201
 211  continue
      close (17)

      filename = pref(1:lp)//'.comb.found'
      open (unit=18, file=filename, status='old', err=500)
 202  continue
         read (18, *, err=202, end=212) fx, fy, mag, fra, fangle
         i_mag = int((mag-mag_min)/mag_step) + 1
         if ((i_mag .gt. 0) .and. (i_mag .le. n_max)) then
            hfo(i_mag) = hfo(i_mag) + 1.
            hfa(i_mag) = hfa(i_mag) + 1.
         end if
         goto 202
 212  continue
      close(17)


      filename = pref(1:lp)//'.comb.missed'
      open (unit=18, file=filename, status='old', err=500)
 300  continue
         read (18, *, err=300, end=310) fx, fy, mag, fra, fangle
         i_mag = int((mag-mag_min)/mag_step) + 1
         if ((i_mag .gt. 0) .and. (i_mag .le. n_max)) then
            hfa(i_mag) = hfa(i_mag) + 1.
         end if
         goto 300
 310  continue
      close (18)

      filename = pref(1:lp)//'.eff'
      open (unit=3, file=filename, status='unknown')
      do i = 1, n_max
         if (hfa(i) .gt. 0.) then
            frac = hfo(i)/hfa(i)
            sigma = sqrt(frac*(1.-frac)/nint(hfa(i)))
            write (3, '(1x,f5.2,4(1x,i4))')
     $        mag_min + (i-0.5)*mag_step,
     $        nint(hfa(i)), nint(hfo(i)),
     $        nint(jmpfo(i)), nint(mattfo(i))
         end if
      end do
      close (3)

 500  continue

c Apparently things went right

      open (unit=1, file='sum-eff.OK', status='unknown')
      write (1, *) ' '
      close (1)

      end
