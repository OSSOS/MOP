C -*-compile-command: "make_lib"; -*-

      subroutine create_header (lun_i, naxis1, naxis2, head_name, lun_h)
c-------------------------------------------------------------------

      implicit none

      include 'MOP_version.inc'

      integer*4
     $  status, lun_i, naxis1, naxis2, lun_h, expnum, chipnum,
     $  i1, i2, i

      real*8
     $  crpix1, crval1, crpix2, crval2, exptime, x1, x2, y1, y2,
     $  mjd_obs, pixscale, cdelt1, cdelt2, phpadu, rdnoise, hh, mm, ss

      character
     $  comment*80, ras*19, decs*19, string*19, detect*20, line*80,
     $  ut_start*20, telesc*20, ccdsum*8, detsec*40, head_name*80

      if (lun_h .le. 0) stop

      open (unit=lun_h, file=head_name, status='unknown',
     $  form='unformatted', access='direct', recl=80, err=1000)

c Reads header keywords.

      call ftgrec (lun_i, 0, comment, status)
      call ftgkys (lun_i, 'INSTRUME', detect, comment, status)
      if (status .gt. 0) then
         status = 0
         call ftgrec (lun_i, 0, comment, status)
         call ftgkys (lun_i, 'DETECTOR', detect, comment, status)
      end if
      write (6,'(a)') detect
      if (detect(1:7) .eq. 'CFH12K ') then

c This is CFH12K on CFHT

         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'MJD-OBS', mjd_obs, comment, status)
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'EXPTIME', exptime, comment, status)
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyj (lun_i, 'EXPNUM', expnum, comment, status)
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'CRVAL1', crval1, comment, status)
         if (status .gt. 0) then
            status = 0
            ras = '                   '
            call ftgrec (lun_i, 0, comment, status)
            call ftgkys (lun_i, 'RA', ras, comment, status)
            call hms (ras, crval1)
            crval1 = crval1*15.
         end if
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'CRVAL2', crval2, comment, status)
         if (status .gt. 0) then
            status = 0
            decs = '                   '
            call ftgrec (lun_i, 0, comment, status)
            call ftgkys (lun_i, 'DEC', decs, comment, status)
            call hms (decs, crval2)
         end if
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'PIXSCAL1', pixscale, comment, status)
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'CRPIX1', crpix1, comment, status)
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'CRPIX2', crpix2, comment, status)
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyj (lun_i, 'IMAGEID', chipnum, comment, status)
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'GAIN', phpadu, comment, status)
         if (chipnum .eq. 0) then
            rdnoise = 4.2
         else if (chipnum .eq. 1) then
            rdnoise = 3.0
         else if (chipnum .eq. 2) then
            rdnoise = 5.4
         else if (chipnum .eq. 3) then
            rdnoise = 4.6
         else if (chipnum .eq. 4) then
            rdnoise = 10.5
         else if (chipnum .eq. 5) then
            rdnoise = 3.4
         else if (chipnum .eq. 6) then
            rdnoise = 5.0
         else if (chipnum .eq. 7) then
            rdnoise = 5.1
         else if (chipnum .eq. 8) then
            rdnoise = 5.9
         else if (chipnum .eq. 9) then
            rdnoise = 3.9
         else if (chipnum .eq. 10) then
            rdnoise = 3.4
         else if (chipnum .eq. 11) then
            rdnoise = 5.2
         end if
         detect(8:19) = '            '

      else if (detect(1:4) .eq. 'Mega') then

c This is MegaPrime on CFHT

         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'MJD-OBS', mjd_obs, comment, status)
         write (6, *) mjd_obs

         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'EXPTIME', exptime, comment, status)
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyj (lun_i, 'EXPNUM', expnum, comment, status)
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'CRVAL1', crval1, comment, status)
         if (status .gt. 0) then
            status = 0
            ras = '                   '
            call ftgrec (lun_i, 0, comment, status)
            call ftgkys (lun_i, 'RA', ras, comment, status)
            call hms (ras, crval1)
            crval1 = crval1*15.
         end if
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'CRVAL2', crval2, comment, status)
         if (status .gt. 0) then
            status = 0
            decs = '                   '
            call ftgrec (lun_i, 0, comment, status)
            call ftgkys (lun_i, 'DEC', decs, comment, status)
            call hms (decs, crval2)
         end if
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'PIXSCAL1', pixscale, comment, status)
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyj (lun_i, 'CHIPID', chipnum, comment, status)
         if (status .gt. 0) then
            status = 0
            call ftgrec (lun_i, 0, comment, status)
            call ftgkys (lun_i, 'AMPNAME', detsec, comment, status)
            read (detsec(1:2), *) chipnum
            chipnum = chipnum + 1
         end if
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'GAIN', phpadu, comment, status)
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'RDNOISE', rdnoise, comment, status)
         if (rdnoise .gt. 20.) then
            rdnoise = 4.6
         end if
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'CRPIX1', crpix1, comment, status)
         if (status .gt. 0) then
            status = 0
            call ftgrec (lun_i, 0, comment, status)
            call ftgkys (lun_i, 'DETSEC', detsec, comment, status)
            i1 = index(detsec, '[') + 1
            i2 = index(detsec, ':') - 1
            read (detsec(i1:i2), *) x1
            i1 = i2 + 2
            i2 = index(detsec(i1:), ',') + i1 - 2
            read (detsec(i1:i2), *) x2
            i1 = i2 + 2
            i2 = index(detsec(i1:), ':') + i1 - 2
            read (detsec(i1:i2), *) y1
            i1 = i2 + 2
            i2 = index(detsec(i1:), ']') + i1 - 2
            read (detsec(i1:i2), *) y2
            crpix1 = 11604.5 - (x1 + x2)/2. + float(naxis1)/2.
            crpix2 = 9681 - (y1 + y2)/2. + float(naxis2)/2.
         else
            call ftgrec (lun_i, 0, comment, status)
            call ftgkyd (lun_i, 'CRPIX2', crpix2, comment, status)
         end if
         detect(10:19) = '          '

      else if (detect(1:3) .eq. 'WFI') then

c This is WFI on ESO 2.2m

         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'MJD-OBS', mjd_obs, comment, status)
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'EXPTIME', exptime, comment, status)
         expnum = nint((mjd_obs - 51000.)*2000.)
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'CRVAL1', crval1, comment, status)
         if (status .gt. 0) then
            status = 0
            ras = '                   '
            call ftgrec (lun_i, 0, comment, status)
            call ftgkys (lun_i, 'RA', ras, comment, status)
            call hms (ras, crval1)
            crval1 = crval1*15.
         end if
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'CRVAL2', crval2, comment, status)
         if (status .gt. 0) then
            status = 0
            decs = '                   '
            call ftgrec (lun_i, 0, comment, status)
            call ftgkys (lun_i, 'DEC', decs, comment, status)
            call hms (decs, crval2)
         end if
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'CD1_1', pixscale, comment, status)
         pixscale = abs(pixscale*3600.)
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'CRPIX1', crpix1, comment, status)
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'CRPIX2', crpix2, comment, status)
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyj (lun_i, 'IMAGEID', chipnum, comment, status)
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'OUTGAIN', phpadu, comment, status)
         if (phpadu .eq. 0.) phpadu = 1.
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'OUTRON', rdnoise, comment, status)
         if (rdnoise .eq. 0.) rdnoise = 4.
         detect(4:19) = '                '

      else if (detect(1:12) .eq. 'CCDMosaThin1') then

c This is the Mosaic camera on KPNO

         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'MJD-OBS', mjd_obs, comment, status)
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'EXPTIME', exptime, comment, status)
         expnum = nint((mjd_obs - 51000.)*2000.)
         ras = '                   '
         call ftgrec (lun_i, 0, comment, status)
         call ftgkys (lun_i, 'RA', ras, comment, status)
         call hms (ras, crval1)
         crval1 = crval1*15.
         decs = '                   '
         call ftgrec (lun_i, 0, comment, status)
         call ftgkys (lun_i, 'DEC', decs, comment, status)
         call hms (decs, crval2)
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'PIXSCAL1', pixscale, comment, status)
         call ftgrec (lun_i, 0, comment, status)
         call ftgkys (lun_i, 'CCDSUM', ccdsum, comment, status)
         if ( ccdsum(1:3) .eq. '2 2') pixscale = 2.0*pixscale
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'CRPIX1', crpix1, comment, status)
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'CRPIX2', crpix2, comment, status)
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyj (lun_i, 'IMAGEID', chipnum, comment, status)
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'GAIN', phpadu, comment, status)
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'RDNOISE', rdnoise, comment, status)
         detect(13:19) = '       '

      elseif (detect(1:8) .eq. 'SUSI/3.2') then

c This is SUSI on NTT !

         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'MJD-OBS', mjd_obs, comment, status)
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'EXPTIME', exptime, comment, status)
         ras = '                   '
         call ftgrec (lun_i, 0, comment, status)
         call ftgkys (lun_i, 'RA', ras, comment, status)
         call hms (ras, crval1)
         crval1 = crval1*15.
         decs = '                   '
         call ftgrec (lun_i, 0, comment, status)
         call ftgkys (lun_i, 'DEC', decs, comment, status)
         call hms (decs, crval2)
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'CRPIX1', crpix1, comment, status)
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'CRPIX2', crpix2, comment, status)
         if (crpix1 .gt. 2*naxis1) then
            chipnum = 1
         else if (crpix1 .gt. naxis1) then
            chipnum = 0
         end if
         if (naxis1 .gt. 2000) then
            pixscale = 0.0805
         else if (naxis1 .gt. 1000) then
            pixscale = 0.161
         end if
         expnum = nint((mjd_obs - 51000.)*2000.)
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'GAIN', phpadu, comment, status)
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'RDNOISE', rdnoise, comment, status)
         detect(9:19) = '           '

      elseif (detect(1:8) .eq. '4SHOOTER') then

c This is the Mount Hopkins 1.2m !

         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'GJDN', mjd_obs, comment, status)
c  BG : MH header has JD
         mjd_obs = mjd_obs - 2400000.5
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'EXPTIME', exptime, comment, status)
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyj (lun_i, 'DISKFILE', expnum, comment, status)
         ras = '                   '
         call ftgrec (lun_i, 0, comment, status)
         call ftgkys (lun_i, 'TRA', ras, comment, status)
         call hms (ras, crval1)
         crval1 = crval1*15.
         decs = '                   '
         call ftgrec (lun_i, 0, comment, status)
         call ftgkys (lun_i, 'TDEC', decs, comment, status)
         call hms (decs, crval2)
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyj (lun_i, 'IMAGEID', chipnum, comment, status)

         crpix1 = naxis1/2
         crpix2 = naxis2/2
         call hms (ras, crval1)
         crval1 = crval1*15.
         call hms (decs, crval2)
         if (naxis1 .lt. 1500) then
            cdelt1 = .66/3600.
         else
            cdelt1 = .33/3600.
         end if
         if (naxis1 .gt. 2000) then
            pixscale = 0.33
         else if (naxis1 .gt. 1000) then
            pixscale = 0.66
         end if
         cdelt2 = cdelt1
         detect(9:19) = '            '

      elseif (detect(1:4) .eq. 'TEK3') then

c This is Palomar !

         call ftgrec (lun_i, 0, comment, status)
         call ftgkys (lun_i, 'UT-START', ut_start, comment, status)
         call hms (ut_start, mjd_obs)
         mjd_obs = mjd_obs/24.
         crpix1 = naxis1/2
         crpix2 = naxis2/2
         call hms (ras, crval1)
         crval1 = crval1*15.
         call hms (decs, crval2)
         if (naxis1 .lt. 2000) then
            cdelt1 = .56/3600.
         else
            cdelt1 = .28/3600.
         end if
         cdelt2 = cdelt1
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'GAIN', phpadu, comment, status)
         rdnoise = 5.0

      else if (detect(1:3) .eq. 'LFC') then

c This is Palomar Mosaic !

         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'JD', mjd_obs, comment, status)
         mjd_obs = mjd_obs - 2400000.5
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'EXPTIME', exptime, comment, status)
         expnum = nint((mjd_obs - 51000.)*2000.)
         call ftgrec (lun_i, 0, comment, status)
         ras = '                   '
         call ftgrec (lun_i, 0, comment, status)
         call ftgkys (lun_i, 'RA', ras, comment, status)
         call hms (ras, crval1)
         crval1 = crval1*15.
         decs = '                   '
         call ftgrec (lun_i, 0, comment, status)
         call ftgkys (lun_i, 'DEC', decs, comment, status)
         call hms (decs, crval2)
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'SECPIX1', pixscale, comment, status)
         crpix1 = naxis1/2
         crpix2 = naxis2/2
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyj (lun_i, 'CHIPID', chipnum, comment, status)
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'GAIN', phpadu, comment, status)
         rdnoise = 5.0
         detect(4:19) = '                '

      else if (detect(1:5) .eq. 'ccd24') then

c This is Bigelow !

         call ftgrec (lun_i, 0, comment, status)
         call ftgkys (lun_i, 'UT', ut_start, comment, status)
         call hms (ut_start, mjd_obs)
         mjd_obs = mjd_obs/24.
         crpix1 = naxis1/2
         crpix2 = naxis2/2
         call hms (ras, crval1)
         crval1 = crval1*15.
         call hms (decs, crval2)
         if (naxis1 .lt. 2000) then
            cdelt1 = .56/3600.
         else
            cdelt1 = .28/3600.
         end if
         cdelt2 = cdelt1

      elseif (detect(1:9) .eq. 'CAFOS 2.2') then

c This is the CAFOS on the Calar Alto 2.2m

         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'MJD-OBS', mjd_obs, comment, status)
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'EXPTIME', exptime, comment, status)
         expnum = nint((mjd_obs - 51000.)*2000.)
         ras = '                   '
         call ftgrec (lun_i, 0, comment, status)
         call ftgkys (lun_i, 'RA', ras, comment, status)
         call hms (ras, crval1)
         crval1 = crval1*15.
         decs = '                   '
         call ftgrec (lun_i, 0, comment, status)
         call ftgkys (lun_i, 'DEC', decs, comment, status)
         call hms (decs, crval2)
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'INSTRSCL', pixscale, comment, status)
         crpix1 = naxis1/2
         crpix2 = naxis2/2
         chipnum = 0
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'CCDSENS', phpadu, comment, status)
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'CCDRON', rdnoise, comment, status)
         detect(10:19) = '          '

      else if (detect(1:5) .eq. 'FORS1') then

c This VLT FORS1

         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'MJD-OBS', mjd_obs, comment, status)
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'EXPTIME', exptime, comment, status)
         ras = '                   '
         call ftgrec (lun_i, 0, comment, status)
         call ftgkys (lun_i, 'RA', ras, comment, status)
         call hms (ras, crval1)
         crval1 = crval1*15.
         decs = '                   '
         call ftgrec (lun_i, 0, comment, status)
         call ftgkys (lun_i, 'DEC', decs, comment, status)
         call hms (decs, crval2)
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'CRPIX1', crpix1, comment, status)
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'CRPIX2', crpix2, comment, status)
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'PIXSCALE', pixscale, comment, status)
         chipnum = 0
         expnum = nint((mjd_obs - 51000.)*2000.)
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'GAIN', phpadu, comment, status)
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'RDNOISE', rdnoise, comment, status)
         detect(6:19) = '              '

      else if (detect(1:5) .eq. 'FORS2') then

c This VLT FORS2

         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'MJD-OBS', mjd_obs, comment, status)
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'EXPTIME', exptime, comment, status)
         ras = '                   '
         call ftgrec (lun_i, 0, comment, status)
         call ftgkys (lun_i, 'RA', ras, comment, status)
         call hms (ras, crval1)
         crval1 = crval1*15.
         decs = '                   '
         call ftgrec (lun_i, 0, comment, status)
         call ftgkys (lun_i, 'DEC', decs, comment, status)
         call hms (decs, crval2)
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'CRPIX1', crpix1, comment, status)
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'CRPIX2', crpix2, comment, status)
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'PIXSCALE', pixscale, comment, status)
         if (naxis1 .lt. 2200) then
            pixscale = pixscale*2.d0
         end if
         if (crpix2 .gt. naxis2/2) then
            chipnum = 2
         else
            chipnum = 1
         end if
         expnum = nint((mjd_obs - 51000.)*2000.)
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'GAIN', phpadu, comment, status)
         call ftgrec (lun_i, 0, comment, status)
         call ftgkyd (lun_i, 'RDNOISE', rdnoise, comment, status)
         detect(6:19) = '              '

      else
         call ftgrec (lun_i, 0, comment, status)
         call ftgkys (lun_i, 'TELESCOP', telesc, comment, status)
         if (telesc(1:7) .eq. 'ESO-3P6') then
         else if (telesc(1:5) .eq. 'LX200') then

c This LX200

            call ftgrec (lun_i, 0, comment, status)
            call ftgkyd (lun_i, 'JDAY', mjd_obs, comment, status)
            mjd_obs = nint(mjd_obs - 2400000.5)
            call ftgrec (lun_i, 0, comment, status)
            call ftgkys (lun_i, 'UT-START', telesc, comment, status)
            read (telesc(1:2), *) hh
            read (telesc(4:5), *) mm
            read (telesc(7:), *) ss
            mjd_obs = mjd_obs + ((ss/60.d0 + mm)/60.d0)/24.d0
            call ftgrec (lun_i, 0, comment, status)
            call ftgkyd (lun_i, 'EXPOSURE', exptime, comment, status)
            exptime = exptime*60.d0
            call ftgrec (lun_i, 0, comment, status)
            call ftgkyd (lun_i, 'RA', crval1, comment, status)
            call ftgrec (lun_i, 0, comment, status)
            call ftgkyd (lun_i, 'DEC', crval2, comment, status)
            crpix1 = naxis1/2.d0
            crpix2 = naxis2/2.d0
            call ftgrec (lun_i, 0, comment, status)
            call ftgkyd (lun_i, 'CDELTM1', pixscale, comment, status)
            call ftgrec (lun_i, 0, comment, status)
            call ftgkyd (lun_i, 'FOCAL', hh, comment, status)
            pixscale = pixscale/hh/3.141592*180.*3600.
            chipnum = 0
            expnum = nint((mjd_obs - 51000.)*2000.)
            phpadu = 2.5
            rdnoise = 10.
            detect(6:19) = '              '
         else
            call ftgrec (lun_i, 0, comment, status)
            call ftgkys (lun_i, 'CTYPE1', string, comment, status)
            if (status .le. 0) then
               call ftgrec (lun_i, 0, comment, status)
               call ftgkys (lun_i, 'CTYPE2', string, comment, status)
               call ftgrec (lun_i, 0, comment, status)
               call ftgkyd (lun_i, 'CRPIX1', crpix1, comment, status)
               call ftgrec (lun_i, 0, comment, status)
               call ftgkyd (lun_i, 'CRPIX2', crpix2, comment, status)
               call ftgrec (lun_i, 0, comment, status)
               call ftgkyd (lun_i, 'CRVAL1', crval1, comment, status)
               call ftgrec (lun_i, 0, comment, status)
               call ftgkyd (lun_i, 'CRVAL2', crval2, comment, status)
               call ftgrec (lun_i, 0, comment, status)
               call ftgkyd (lun_i, 'CDELT1', cdelt1, comment, status)
               if (status .le. 0) then
                  call ftgrec (lun_i, 0, comment, status)
                  call ftgkyd (lun_i, 'CDELT2', cdelt2, comment, status)
               else
               end if
            end if
         end if
      end if

c Fills in with blanks.

      line(31:80) = '                                                  '
      line(1:31) = '                               '
      do i = 1, 35
         write (lun_h, rec=i) line
      end do
      line(1:3) = 'END'
      write (lun_h, rec=36) line

c Now writes keywords.

      line(1:30) = 'SIMPLE  =                    T'
      write (lun_h, rec=1) line
      line(1:30) = 'BITPIX  =                   16'
      write (lun_h, rec=2) line
      line(1:30) = 'NAXIS   =                    0'
      write (lun_h, rec=3) line
      line(1:30) = 'EXTEND  =                    F'
      write (lun_h, rec=4) line
      line(1:10) = 'EXPNUM  = '
      write (line(11:30), '(i20)') expnum
      write (lun_h, rec=5) line
      line(1:10) = 'CHIPNUM = '
      write (line(11:30), '(i20)') chipnum
      write (lun_h, rec=6) line
      line(1:10) = 'MJD-OBSC= '
      write (line(11:30), '(f20.7)') mjd_obs + exptime
     $  /(2.d0*24.d0*3600.d0)
      write (lun_h, rec=7) line
      line(1:10) = 'EXPTIME = '
      write (line(11:30), '(f20.2)') exptime
      write (lun_h, rec=8) line
      line(1:10) = 'CRVAL1  = '
      write (line(11:30), '(f20.5)') crval1
      write (lun_h, rec=9) line
      line(1:10) = 'CRVAL2  = '
      write (line(11:30), '(f20.5)') crval2
      write (lun_h, rec=10) line
      line(1:10) = 'CRPIX1  = '
      write (line(11:30), '(f20.2)') crpix1
      write (lun_h, rec=11) line
      line(1:10) = 'CRPIX2  = '
      write (line(11:30), '(f20.2)') crpix2
      write (lun_h, rec=12) line
      line(1:10) = 'PIXSCALE= '
      write (line(11:30), '(f20.3)') pixscale
      write (lun_h, rec=13) line
      line(1:10) = 'NAXIS1  = '
      write (line(11:30), '(i20)') naxis1
      write (lun_h, rec=14) line
      line(1:10) = 'NAXIS2  = '
      write (line(11:30), '(i20)') naxis2
      write (lun_h, rec=15) line
      line(1:10) = 'DETECTOR= '
      write (line(11:31), '(a21)') ''''//detect(1:19)//''''
      write (lun_h, rec=16) line
      line(31:31) = ' '
      line(1:10) = 'PHPADU  = '
      write (line(11:30), '(f20.2)') phpadu
      write (lun_h, rec=17) line
      line(1:10) = 'RDNOISE = '
      write (line(11:30), '(f20.2)') rdnoise
      write (lun_h, rec=18) line
      line(1:10) = 'MOP_VER = '
      write (line(11:30), '(f20.2)') MOP_version
      write (lun_h, rec=19) line

      close (lun_h)

      return

 1000 continue
      write (6, '(a)') 'Couldn''t create file '//head_name
      stop 'Exiting.'

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
     *            val, piece(3), dp, sgn, z
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
      val = val*sgn
      RETURN
      END
