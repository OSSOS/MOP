C -*-compile-command: "cf77 -O6 -o ../bin/LINUX/step0jmp step0jmp.f -L ../lib/LINUX -ljmp -lcfitsio; /bin/rm step0jmp.o"; -*-
C When actually using g77, add the option -fno-automatic

      implicit none

      include 'match-mov.h'

      integer*4
     $  image_size_max

      parameter
     $  (image_size_max = 2048*1100)

      integer*4
     $  i, i1, i2, n_obj_tot, nx, ny, y_trans, y_offset, n_min, n_max,
     $  y_seg, n_seg, naxis1, naxis2, lun_i, lun_o, i_seg, n, lun_d,
     $  n_obj, size_obj(n_o_max_cat), n_obj_max, narg, iargc,
     $  n_br, indx(n_o_max_cat), j, indx2(n_o_max_cat), lun_h,
     $  indxt1(n_o_max_cat), indxt2(n_o_max_cat)

      real*4
     $  image(image_size_max), coeff(image_size_max),
     $  tmp1(image_size_max), mean, sig, thres1, thres2,
     $  moments(7,n_o_max_cat), max_el, max_int(n_o_max_cat),
     $  im_sky(image_size_max), maxcount, echelle, echelle_c, isol,
     $  separ, x_o(n_o_max_cat), y_o(n_o_max_cat), x_br(n_br_max),
     $  y_br(n_br_max), inten(n_o_max_cat), el(n_o_max_cat),
     $  flux(n_o_max_cat), area(n_o_max_cat), sky, fwhm(n_o_max_cat),
     $  sharp(n_o_max_cat), sfl(n_o_max_cat), lsky(n_o_max_cat),
     $  lsfl(n_o_max_cat), mfw, sfw, msh, ssh, msk, ssk, tmp,
     $  tmpel(n_o_max_cat), tmp_1, tmp_2, tmp_3

      character
     $  image_name*80, objects_name*80, base_name*80, arg*80,
     $  line*80, head_name*80, detect_name*80, fwhm_name*80

      logical
     $  finished, help, no_f, no_m, no_t, no_w, two_thresh,
     $  ok(n_o_max_cat), alone(n_o_max_cat)


c Create a file for later error handling

      open (unit=1, file='step0jmp.FAILED', status='unknown')
      write (1, *) ' '
      close (1)

c Get input parameters

      do i = 1, 80
         base_name(i:i) = char(0)
      end do

      n_obj_max = n_o_max_cat

      narg = iargc()
      i = 1
      no_f = .true.
      no_m = .true.
      no_t = .true.
      no_w = .true.
      help = (narg .ne. 8)

 90   continue
      if (i .le. narg) then
         call getarg (i, arg)
         if (arg(1:2) .eq. '-f') then
            i = i + 1
            call getarg (i, base_name)
            no_f = .false.
         else if (arg(1:2) .eq. '-m') then
            i = i + 1
            call getarg (i, arg)
            read (arg, *) maxcount
            no_m = .false.
         else if (arg(1:2) .eq. '-t') then
            i = i + 1
            call getarg (i, arg)
            read (arg, *) thres1
            no_t = .false.
         else if (arg(1:2) .eq. '-w') then
            i = i + 1
            call getarg (i, arg)
            read (arg, *) echelle
            no_w = .false.
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

      open (unit=1, file='step0jmp.FAILED', status='unknown')
      write (1, *) line(1:j-1)
      close (1)

      help =  help .or. no_f .or. no_m .or. no_t .or. no_w

      if (help) then
         write (6, *)
     $     'Usage: step0jmp -f <image file> -w <scale> -t <threshold>'//
     $     ' -m <maxcount>'
         write (6, *) 'where:'
         write (6, *) '-f <image file>: image to process (no extension)'
         write (6, *) '-w <scale>: FWHM in pixels (int)'
         write (6, *) '-t <threshold>: detection threshold'
         write (6, *) '-m <maxcount>: maximum pixel value allowed'
         stop
      end if

 80   continue

      call read_file_name (base_name, i1, i2, finished, 80)
      if (finished) stop

      image_name = base_name(i1:i2)//'.fits'
      objects_name = base_name(i1:i2)//'.bright.psf'
      detect_name = base_name(i1:i2)//'.obj.psf'
      head_name = base_name(i1:i2)//'.mopheader'
      fwhm_name = base_name(i1:i2)//'.fwhm'
      lun_i = 20
      lun_o = 21
      lun_h = 22
      lun_d = 23
      echelle_c = amax1(echelle, 3.05)

      max_el = 10.
      isol = 5.*echelle
      separ = 10.*echelle
      n_max = n_br_max
      n_min = 40

c Open object list file

      open (unit=lun_o, file=objects_name, status='unknown')
      open (unit=lun_d, file=detect_name, status='unknown')

c Open image file and find geometry informations

      call open_image (image_name, lun_i, naxis1, naxis2)

c Reads in the header keywords, and stores them at beginning of file.

      call get_header (head_name, lun_h, lun_o, thres1, echelle,
     $  maxcount)
      call get_header (head_name, lun_h, lun_d, thres1, echelle,
     $  maxcount)

c Determine image segment size and offset

      y_seg = max(2*nint(echelle_c), 20)
      nx = naxis1
      ny = image_size_max/nx
      n_seg = (naxis2 - y_seg - 1)/(ny - y_seg) + 1
      y_trans = (naxis2 - y_seg)/n_seg
      ny = y_trans + y_seg
      y_offset = 0
      n_obj_tot = 0

      two_thresh = .true.
      thres1 = 1.1*thres1
      thres2 = thres1/2.

c Loop on the segments

      do i_seg = 1, n_seg

c Read image segment

         call read_seg (image, y_offset, lun_i, nx, ny)

c Compute wavelet coefficients at desired scale

         call convol (image, echelle_c, coeff, im_sky, tmp1, nx, ny)

c Compute thresholding value

         call momenta (coeff, mean, sig, thres1, nx, ny)

c Create detection mask

         call thresh (coeff, tmp1, mean, sig, thres1, thres2,
     $     two_thresh, image, sky, nx, ny)

c Detect objects

         call segment (image, coeff, im_sky, tmp1, y_offset,
     $     size_obj, moments, sfl, max_int, n_obj, nx, ny, sky,
     $     n_obj_max, max_el, echelle, echelle_c, .false.)

c Rejects objects on bad columns

         call bad_column (moments, size_obj, n_obj, image, nx, ny,
     $     y_offset, echelle)

c Write objects in file

         do n = 1, n_obj
            if ((size_obj(n) .gt. 0) .and. (max_int(n) .gt. 0.)
     $        .and. (moments(4,n) .lt. 50.)
     $        .and. (moments(1,n) .gt. separ)
     $        .and. (moments(1,n) .lt. naxis1-separ)
     $        .and. (moments(2,n) .gt. separ)
     $        .and. (moments(2,n) .lt. naxis2-separ)) then
               n_obj_tot = n_obj_tot + 1
               x_o(n_obj_tot) = moments(1,n)
               y_o(n_obj_tot) = moments(2,n)
               inten(n_obj_tot) = max_int(n)
               el(n_obj_tot) = moments(5,n)
               flux(n_obj_tot) = moments(6,n)
               area(n_obj_tot) = float(size_obj(n))
               sharp(n_obj_tot) = moments(4,n)
               lsky(n_obj_tot) = moments(7,n)
               fwhm(n_obj_tot) = moments(3,n)
               lsfl(n_obj_tot) = sfl(n)
c               write (30, 9000) moments(1,n), moments(2,n),
c     $           moments(6,n), float(size_obj(n)), max_int(n),
c     $           moments(5,n), moments(3,n), moments(4,n), moments(7,n)
c     $           ,sfl(n)
            end if
         end do

         y_offset = y_offset + y_trans
      end do

      write (6, *) 'Found ', n_obj_tot, ' objects.'

      mfw = 0.
      sfw = 0.
      do i = 1, n_obj_tot
         mfw = mfw + fwhm(i)
         sfw = sfw + fwhm(i)**2
      end do
      mfw = mfw/float(n_obj_tot)
      sfw = sqrt(sfw/float(n_obj_tot) - mfw**2)
c      write (6, *) mfw, sfw

      n_br = 0
      msh = 0.
      ssh = 0.
      msk = 0.
      ssk = 0.
      do n = 1, n_obj_tot
         if ((fwhm(n) .gt. mfw - 1.*sfw) .and. (sharp(n) .lt. 10.)) then
            msh = msh + sharp(n)
            ssh = ssh + sharp(n)**2
            msk = msk + lsky(n)
            ssk = ssk + lsky(n)**2
            n_br = n_br + 1
            x_o(n_br) = x_o(n)
            y_o(n_br) = y_o(n)
            inten(n_br) = inten(n)
            el(n_br) = el(n)
            flux(n_br) = flux(n)
            area(n_br) = area(n)
            sharp(n_br) = sharp(n)
            lsky(n_br) = lsky(n)
            fwhm(n_br) = fwhm(n)
            lsfl(n_br) = lsfl(n)
            write (lun_d, 9000) x_o(n_br), y_o(n_br), flux(n_br),
     $        area(n_br), inten(n_br), el(n_br), fwhm(n_br),
     $        sharp(n_br), lsky(n_br), lsfl(n_br)
         end if
      end do
      n_obj_tot = n_br
      msh = msh/float(n_obj_tot)
      ssh = sqrt(ssh/float(n_obj_tot) - msh**2)
      msk = msk/float(n_obj_tot)
      ssk = sqrt(ssk/float(n_obj_tot) - msk**2)
c      write (6, *) msh, ssh
c      write (6, *) msk, ssk

      call select_bright (x_o, y_o, inten, n_obj_tot, x_br, y_br,
     $  indx, n_br, maxcount, isol, separ, n_min, n_max, ok, alone,
     $  tmpel, indxt1, indxt2)

c      do i = 1, n_br
c         n = indx(i)
c         write (31, 9000) x_o(n), y_o(n), flux(n), area(n), inten(n),
c     $     el(n), fwhm(n), sharp(n), lsky(n), lsfl(n)
c      end do

      write (6, *) 'Found ', n_br, ' bright objects.'

      do i = 1, n_br
c         tmpel(i) = el(indx(i))
         tmpel(i) = inten(indx(i))
      end do

      call sortreal (n_br, tmpel, indx2)

      mean = 0.
      n = 0
c      do i = 1, n_br
      do i = n_br, 1, -1
         tmp_1 = abs(sharp(indx(indx2(i)))-msh)/ssh
         tmp_2 = abs(fwhm(indx(indx2(i)))-mfw)/sfw
         tmp_3 = abs(lsky(indx(indx2(i)))-msk)/ssk
         tmp = tmp_1 + tmp_2 + tmp_3
         if ((tmp .le. 6.) .and. (tmp_1 .le. 3.)
     $     .and. (tmp_2 .le. 3.) .and. (tmp_3 .le. 3.)) then
            mean = mean + fwhm(indx(indx2(i)))
            write (lun_o, 9000) x_br(indx2(i)), y_br(indx2(i)),
     $        flux(indx(indx2(i))), area(indx(indx2(i))),
     $        inten(indx(indx2(i))), el(indx(indx2(i))),
     $        fwhm(indx(indx2(i))), sharp(indx(indx2(i))),
     $        lsky(indx(indx2(i))), lsfl(indx(indx2(i)))
            n = n + 1
         end if
c         if ((n .gt. n_min) .or. (el(indx(indx2(i))) .gt. 1.5))
         if (n .gt. n_min)
     $     goto 8000
      end do
 8000 continue
      write (6, *) 'Recorded ', n, ' bright objects.'
      mean = mean/float(n)

      call close_image (lun_i)
      close (lun_o)
      close (lun_d)

      open (unit=lun_d, file=fwhm_name, status='unknown')
      write (lun_d, '(f7.2)') mean*1.2
      close (lun_d)

c Apparently things went right

      if (n_obj_tot .gt. 0) then
         open (unit=1, file='step0jmp.OK', status='unknown')
         write (1, *) ' '
         close (1)
      end if

c 9000 format (2f8.2, f13.2, f9.1, f10.2, f6.2)
 9000 format (2(1x,f7.2), 1x, f12.2, 1x, f5.0, 1x, f8.1, 1x, f5.2,
     $  2(1x,f5.2), 1x, f8.2, 1x, f5.0)

      stop

      end
