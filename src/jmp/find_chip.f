C -*-compile-command: "cf77 -O6 -o ../bin/LINUX/find_chip find_chip.f -L../lib/LINUX -ljmp -lcfitsio; /bin/rm find_chip.o"; -*-
C When actually using g77, add the option -fno-automatic

      implicit none

      include 'match-mov.h'

      integer*4
     $  i, i1, i2, naxis1, naxis2, lun_i, lun_o, narg, iargc,
     $  j, lun_h, istat, x_pos, y_pos,
     $  MC_chip(1:4,1:9), C12k_chip(1:2,1:6), WFI_chip(1:2,1:4),
     $  MC_r_off(1:9), MC_d_off(1:4), C12k_r_off(1:6), C12k_d_off(1:2),
     $  WFI_r_off(1:4), WFI_d_off(1:2)

      real*8
     $  ra_asked, dec_asked, ra_p, dec_p, pixscale, crpix1, crpix2

      character
     $  in_name*80, arg*80, detect*19,
     $  line*80, head_name*80, out_name*80

      logical
     $  finished, help, no_f, no_r, no_d, no_o

      external iargc

      data
     $  MC_chip    / 0, 1, 2, 3, 4, 5, 6, 7, 8,
     $               9,10,11,12,13,14,15,16,17,
     $              18,19,20,21,22,23,24,25,26,
     $              27,28,29,30,31,32,33,34,35/,
     $  C12k_chip  / 5, 4, 3, 2, 1, 0,
     $              11,10, 9, 8, 7, 6/,
     $  WFI_chip   / 1, 2, 3, 4,
     $               5, 6, 7, 8/,
     $  MC_r_off   /10576,8196,5816,3436,1056,-1324,-3704,-6084,-8464/,
     $  MC_d_off   /-5065,-151,4763,9677/,
     $  C12k_r_off /6283,4148,2047,-39,-2135,-4210/,
     $  C12k_d_off /20,4173/,
     $  WFI_r_off  /4148,2047,-39,-2135/,
     $  WFI_d_off  /20,4173/
c     $  MC_r_s     /0.5/,
c     $  MC_d_s     /0.51/,
c     $  C12k_r_s   /0.36/,
c     $  C12k_d_s   /0.24/,
c     $  WFI_r_s    /0.27/,
c     $  WFI_d_s    /0.27/,
c     $  MC_r_c     /0.122/,
c     $  MC_d_c     /0.253/,
c     $  C12k_r_c   /0.12/,
c     $  C12k_d_c   /0.24/,
c     $  WFI_r_c    /0.136/,
c     $  WFI_d_c    /0.271/

c Create a file for later error handling

      open (unit=1, file='find_chip.FAILED', status='unknown')
      write (1, *) ' '
      close (1)

c Get input parameters

      do i = 1, 80
         in_name(i:i) = char(0)
         out_name(i:i) = char(0)
      end do

      narg = iargc()
      i = 1
      no_f = .true.
      no_o = .true.
      no_r = .true.
      no_d = .true.
      help = (narg .ne. 8)

 90   continue
      if (i .le. narg) then
         call getarg (i, arg)
         if (arg(1:2) .eq. '-f') then
            i = i + 1
            call getarg (i, in_name)
            no_f = .false.
         else if (arg(1:2) .eq. '-o') then
            i = i + 1
            call getarg (i, out_name)
            no_o = .false.
         else if (arg(1:2) .eq. '-r') then
            i = i + 1
            call getarg (i, arg)
            call hms (arg, ra_asked)
            no_r = .false.
         else if (arg(1:2) .eq. '-d') then
            i = i + 1
            call getarg (i, arg)
            call hms (arg, dec_asked)
            no_d = .false.
         else if ((arg(1:2) .eq. '-h') .or. (arg(1:2) .eq. '-?')) then
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

      open (unit=1, file='find_chip.FAILED', status='unknown')
      write (1, *) line(1:j-1)
      close (1)

      help =  help .or. no_f .or. no_o .or. no_r .or. no_d

      if (help) then
         write (6, *)
     $     'Usage: find_chip -f <header list> -o <output file> '//
     $     '-r <RA> -d <DEC>'
         write (6, *) 'where:'
         write (6, *) '-f <header list>: header file name list'
         write (6, *) '-o <output file>: output file name'
         write (6, *) '-r <RA>: Right Ascension asked for'
         write (6, *) '-d <DEC>: Declination asked for'
         stop
      end if

 80   continue

      call read_file_name (in_name, i1, i2, finished, 80)
      if (finished) stop

      call read_file_name (out_name, i1, i2, finished, 80)
      if (finished) stop

      lun_i = 20
      lun_o = 21
      lun_h = 22

      open (unit=lun_i, file=in_name, status='old', err=1010)
      goto 1020

 1010 continue
         open (unit=1, file='find_chip.FAILED', status='unknown')
         write (1, *) 'find_chip failed openning '//in_name
         close (1)
         stop
 1020 continue

      open (unit=lun_o, file=out_name, status='unknown', err=1030)
      goto 1040

 1030 continue
         open (unit=1, file='find_chip.FAILED', status='unknown')
         write (1, *) 'find_chip failed openning '//out_name
         close (1)
         stop
 1040 continue

 1100 continue
      do i = 1, 80
         head_name(i:i) = char(0)
      end do
      read (lun_i, '(a)', end=1200) head_name
      call read_file_name (head_name, i1, i2, finished, 80)
      if (finished) goto 1100

      call get_nth_key_s (head_name, lun_h, 16, detect, istat)
      if (istat .ne. 0) then
         write (6, *) 'find_chip failed reading DETECTOR in '//
     $     head_name
         write (6, *) 'Error code: ', istat
         goto 1100
      end if

      call get_nth_key_d (head_name, lun_h, 9, ra_p, istat)
      if (istat .ne. 0) then
         write (6, *) 'find_chip failed reading CRVAL1 in '//
     $     head_name
         write (6, *) 'Error code: ', istat
         goto 1100
      end if

      call get_nth_key_d (head_name, lun_h, 10, dec_p, istat)
      if (istat .ne. 0) then
         write (6, *) 'find_chip failed reading CRVAL2 in '//
     $     head_name
         write (6, *) 'Error code: ', istat
         goto 1100
      end if

      call get_nth_key_d (head_name, lun_h, 11, crpix1, istat)
      if (istat .ne. 0) then
         write (6, *) 'find_chip failed reading CRPIX1 in '//
     $     head_name
         write (6, *) 'Error code: ', istat
         goto 1100
      end if

      call get_nth_key_d (head_name, lun_h, 12, crpix2, istat)
      if (istat .ne. 0) then
         write (6, *) 'find_chip failed reading CRPIX2 in '//
     $     head_name
         write (6, *) 'Error code: ', istat
         goto 1100
      end if

      call get_nth_key_d (head_name, lun_h, 13, pixscale, istat)
      if (istat .ne. 0) then
         write (6, *) 'find_chip failed reading PIXSCALE in '//
     $     head_name
         write (6, *) 'Error code: ', istat
         goto 1100
      end if

      call get_nth_key_j (head_name, lun_h, 14, naxis1, istat)
      if (istat .ne. 0) then
         write (6, *) 'find_chip failed reading NAXIS1 in '//
     $     head_name
         write (6, *) 'Error code: ', istat
         goto 1100
      end if

      call get_nth_key_j (head_name, lun_h, 15, naxis2, istat)
      if (istat .ne. 0) then
         write (6, *) 'find_chip failed reading NAXIS2 in '//
     $     head_name
         write (6, *) 'Error code: ', istat
         goto 1100
      end if

      if (detect(1:9) .eq. 'MegaPrime') then
c         if ((abs(ra_asked-ra_p) .lt. MC_r_s)
c     $     .and. (abs(dec_asked-dec_p) .lt. MC_d_s)) then
c            i = int((dec_p - dec_asked)/MC_d_c + 3)
c            j = int((ra_p - ra_asked)/MC_r_c + 5.5)
c            x_pos = int(3600.*(ra_p - ra_asked)/pixscale) + MC_r_off
c            y_pos = int(3600.*(dec_asked - dec_p)/pixscale) + MC_d_off
c            write (lun_o, *) 'Position asked for in chip ',
c     $        MC_chip(i,j), ' of field'
c            write (lun_o, '(a)') head_name(i1:i2)
c            write (lun_o, *) 'Approximate position: (', x_pos, ', ',
c     $        y_pos, ')'
c         end if
         x_pos = int(3600.*(ra_p - ra_asked)/pixscale) + crpix1
         y_pos = int(3600.*(dec_asked - dec_p)/pixscale) + crpix2
         if ((abs(x_pos - naxis1/2.) .lt. naxis1*(1./2. + 1./10.))
     $     .and. (abs(y_pos - naxis2/2.) .lt. naxis2*(1./2. + 1./10.)))
     $     then
            write (lun_o, *) 'Position asked found in field '
     $        //head_name(i1:i2)
            write (lun_o, *) 'Approximate position: (', x_pos, ', ',
     $        y_pos, ')'
         end if
      else if (detect(1:6) .eq. 'CFH12K') then
      else if (detect(1:3) .eq. 'WFI') then
      else
         write (6, *) 'Unknown detector name: '//detect
      end if
      goto 1100

 1200 continue
      close (lun_i)
      close (lun_o)

c Apparently things went right

      open (unit=1, file='find_chip.OK', status='unknown')
      write (1, *) ' '
      close (1)

      stop

      end

      SUBROUTINE  hms(str,val)
c
c...Crack String And Create Value
c
      IMPLICIT
     *            NONE
      CHARACTER*(*)
     *            str
      DOUBLE PRECISION
     *            val, piece(3), dp, sgn, z, scale
      INTEGER
     *            nstr, i, j, dpfind
      CHARACTER*1
     *            c
c
c...Initialization
c
  100 val = 0.0D00
      DO i=1,3
        piece(i) = 0.0D00
      ENDDO
      scale = 1.d0
      j = 1
      dpfind = 0
      sgn = 1.0D00
      nstr = LEN(str)
      IF (nstr.le.0) RETURN
c
c...Loop Over The String
c
      DO i=1,nstr
        c = str(i:i)
c
c...Parse
c
        IF ((c.eq.'-').or.(c.eq.'e').or.(c.eq.'E')
     *  .or.(c.eq.'s').or.(c.eq.'S')) THEN
          sgn = -1.0D00
        ELSEIF ((c.eq.'+').or.(c.eq.'w').or.(c.eq.'W')
     *      .or.(c.eq.'n').or.(c.eq.'N')) THEN
          sgn = 1.0D00
        ELSEIF ((c.eq.':').or.(c.eq.',').or.(c.eq.' ')) THEN
          j = j+1
          dpfind = 0
          IF (j.gt.3) GO TO 110
        ELSEIF (c.eq.'.') THEN
          dpfind = 1
          dp = 1.0D00
        ELSEIF ((c.eq.'h').or.(c.eq.'H')) THEN
          scale = 15.d0
        ELSEIF ((c.ge.'0').and.(c.le.'9')) THEN
          z = ICHAR(c)-ICHAR('0')
          IF (dpfind.eq.0) THEN
            piece(j) = 10.0D00*piece(j) + z
          ELSE
            dp = 0.1D00*dp
            piece(j) = piece(j) + dp*z
          ENDIF
        ENDIF
      ENDDO
c
c...Return
c
  110 val = piece(1) + piece(2)/60.0D00 + piece(3)/3600.0D00
      val = val*sgn*scale
      RETURN
      END
