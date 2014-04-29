C -*-compile-command: "g77 -O3 -o ../../bin/LINUX/global-mag-rate-eff global-mag-rate-eff.f -L../../lib/LINUX -ljmp"; -*-

      implicit none

      integer*4
     $  i, i_rate, j, nm_max, lp, narg, iargc, i2, i1, ld, lf, nr_max,
     $  i_mag

      parameter
     $  (nm_max = 3000,nr_max = 100)

      real*4
     $  mag, fx, fy, fra, rate_min, rate_step, hfa(nr_max,nm_max),
     $  hfo(nr_max,nm_max),
     $  frac, fangle, sigma, jmpfo(nm_max), mattfo(nm_max), pixscale,
     $  rate, mag_step, mag_min

      character
     $  arg*80, dirname*200, filename*100, listname*80, pref*80,
     $  line*200

      logical help, finished, no_rs, no_rn, no_f, no_mn, no_ms


c Create a file for later error handling

      open (unit=1, file='global-mag-rate-eff.FAILED',
     $  status='unknown')
      write (1, *) ' '
      close (1)

      do j = 1, 80
         pref(j:j) = char(0)
      end do
      narg = iargc()
      no_f = .true.
      no_rn = .true.
      no_rs = .true.
      no_mn = .true.
      no_ms = .true.
      help = (narg .ne. 10)
      i = 1
 90   continue
      if (i .le. narg) then
         call getarg (i, arg)
         if (arg(1:2) .eq. '-f') then
            i = i + 1
            call getarg (i, listname)
            no_f = .false.
         else if (arg(1:3) .eq. '-rs') then
            i = i + 1
            call getarg (i, arg)
            read (arg, *) rate_step
            no_rs = .false.
         else if (arg(1:3) .eq. '-rn') then
            i = i + 1
            call getarg (i, arg)
            read (arg, *) rate_min
            no_rn = .false.
         else if (arg(1:3) .eq. '-ms') then
            i = i + 1
            call getarg (i, arg)
            read (arg, *) mag_step
            no_ms = .false.
         else if (arg(1:3) .eq. '-mn') then
            i = i + 1
            call getarg (i, arg)
            read (arg, *) mag_min
            no_mn = .false.
         else if ((arg(1:2) .eq. '-h') .or. (arg(1:2) .eq. '-?')) then
            i = narg + 1
            help = .true.
         end if
         i = i + 1
         goto 90
      end if

      help = help .or. no_f .or. no_rn .or. no_rs .or. no_mn .or. no_ms

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

      open (unit=1, file='global-mag-rate-eff.FAILED',
     $  status='unknown')
      write (1, *) line(1:j-1)
      close (1)

      if (help) then
         write (6, *) 'Usage: global-mag-rate-eff -f <file list>'
     $        //' -rn <min rate> -rs <rate step> -mn <min mag>'
     $        //' -ms <mag step>'
         stop
      end if

      do i_mag = 1, nm_max
         do i_rate = 1, nr_max
            hfa(i_rate,i_mag) = 0.
            hfo(i_rate,i_mag) = 0.
         end do
      end do

      open (unit=42, file=listname, status='old')

 100  continue
      read (42, '(a)', end = 400) line

      call read_file_name (line, i1, i2, finished, 200)
      if (finished) stop
      dirname = line(i1:i2)
      ld = i2 - i1 + 1

      line = line(i2+1:)
      lp = 200 - i2
      call read_file_name (line, i1, i2, finished, lp)
      if (finished) stop
      pref = line(i1:i2)
      lf = i2 - i1 + 1

      pixscale = 0.185
      filename = dirname(1:ld)//pref(1:lf)//'.mopheader'
      open (unit=16, file=filename, status='old', err=500)
 150  continue
         read (16, '(a)', err=150, end=160) line
         if (line(1:8) .eq. 'PIXSCALE') then
            read (line(10:), *) pixscale
            goto 160
         end if
         goto 150
 160  continue
      close (16)

      filename = dirname(1:ld)//pref(1:lf)//'.comb.found'
      open (unit=17, file=filename, status='old', err=500)
 200  continue
         read (17, *, err=200, end=210) fx, fy, mag, fra, fangle
         i_mag = int((mag-mag_min)/mag_step) + 1
         rate = fra*pixscale
         i_rate = int((rate-rate_min)/rate_step) + 1
         if ((i_rate .gt. 0) .and. (i_rate .le. nr_max)) then
            hfa(i_rate,i_mag) = hfa(i_rate,i_mag) + 1.
            hfo(i_rate,i_mag) = hfo(i_rate,i_mag) + 1.
         end if
         goto 200
 210  continue
      close (17)

      filename = dirname(1:ld)//pref(1:lf)//'.comb.missed'
      open (unit=18, file=filename, status='old', err=500)
 300  continue
         read (18, *, err=300, end=310) fx, fy, mag, fra, fangle
         i_mag = int((mag-mag_min)/mag_step) + 1
         rate = fra*pixscale
         i_rate = int((rate-rate_min)/rate_step) + 1
         if ((i_rate .gt. 0) .and. (i_rate .le. nm_max)) then
            hfa(i_rate,i_mag) = hfa(i_rate,i_mag) + 1.
         end if
         goto 300
 310  continue
      close (18)

      goto 100

 400  continue
      close (42)

      call read_file_name (listname, i1, i2, finished, 80)
      filename = listname(i1:i2)//'.mag-rate.eff'
      open (unit=3, file=filename, status='unknown')
      do i_rate = 1, nr_max
         if (rate_min + i_rate*rate_step .gt. 50.) goto 410
         write (3, '(a9, f5.2, 1x, f5.2)')
     $     '# rates: ', rate_min + (i_rate-1)*rate_step,
     $     rate_min + i_rate*rate_step
         do i_mag = 1, nm_max
            if (hfa(i_rate,i_mag) .gt. 0.) then
               frac = hfo(i_rate,i_mag)/hfa(i_rate,i_mag)
               sigma = sqrt(frac*(1.-frac)/nint(hfa(i_rate,i_mag)))
               write (3, '(2(1x,f5.2),1x,i5,2(1x,f5.2))')
     $           mag_min + (i_mag-0.5)*mag_step, frac,
     $           nint(hfa(i_rate,i_mag)),
     $           min(sigma, 1.-frac), min(sigma, frac)
            end if
         end do
      end do
 410  continue
      write (3, '(''#'')')
      close (3)

      filename = listname(i1:i2)//'.mag-rate.eff-2D'
      open (unit=3, file=filename, status='unknown')
      write (3, '(''#     '', 12f6.1)')
     $  (rate_min+(i_rate-0.5)*rate_step, i_rate=1,12)
      do i_mag = 1, nm_max
         frac = mag_min + (i_mag-0.5)*mag_step
         if (frac .lt. 25.) then
            write (3, '(''# '', f5.2)') frac
         end if
      end do
      do i_mag = 1, nm_max
         frac = mag_min + (i_mag-0.5)*mag_step
         if (frac .lt. 25.) then
            write (3, '(''      '', $)')
            do i_rate = 1, 12
               if (hfa(i_rate,i_mag) .gt. 0.) then
                  frac = hfo(i_rate,i_mag)/hfa(i_rate,i_mag)
                  write (3, '(f6.3, $)') frac
               else
                  write (3, '(''      '', $)')
               end if
            end do
            write (3, *)
         end if
      end do
      close (3)

c Apparently things went right

      open (unit=1, file='global-mag-rate-eff.OK', status='unknown')
      write (1, *) ' '
      close (1)
      stop

 500  continue
      write (6, *) 'Could not find '//filename
      write (6, *) 'Skipping to next file.'
      goto 100

      end
