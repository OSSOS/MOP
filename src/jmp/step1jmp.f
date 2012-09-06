C -*-compile-command: "cf77 -O6 -o ../bin/LINUX/step1jmp step1jmp.f -L../lib/LINUX -ljmp -lcfitsio; /bin/rm step1jmp.o"; -*-
C When actually using g77, add the option -fno-automatic

      implicit none

      include 'match-mov.h'

      integer*4
     $  image_size_max

      parameter
     $  (image_size_max = 2048*1100)

      integer*4
     $  i, i1, i2, n_obj_tot, nx, ny, y_trans, y_offset, j,
     $  y_seg, n_seg, naxis1, naxis2, lun_i, lun_o, i_seg, n,
     $  n_obj, size_obj(n_o_max_cat), n_obj_max, narg, iargc, lun_h

      real*4
     $  image(image_size_max), coeff(image_size_max),
     $  tmp1(image_size_max), mean, sig, thres1, thres2,
     $  moments(7,n_o_max_cat), max_el, max_int(n_o_max_cat),
     $  im_sky(image_size_max), maxcount, echelle, echelle_c, sky,
     $  sfl(n_o_max_cat), dummy

      character
     $  image_name*80, objects_name*80, base_name*80, arg*80,
     $  line*80, head_name*80

      logical
     $  finished, help, no_f, no_m, no_t, no_w, two_thresh

      external iargc

c Create a file for later error handling

      open (unit=1, file='step1jmp.FAILED', status='unknown')
      write (1, *) ' '
      close (1)

c Get input parameters

      do i = 1, 80
         base_name(i:i) = char(0)
      end do

      max_el = 5.
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

      open (unit=1, file='step1jmp.FAILED', status='unknown')
      write (1, *) line(1:j-1)
      close (1)

      help =  help .or. no_f .or. no_m .or. no_t .or. no_w

      if (help) then
         write (6, *)
     $     'Usage: step1jmp -f <image file> -w <scale> -t <threshold>'//
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
      objects_name = base_name(i1:i2)//'.obj.jmp'
      head_name = base_name(i1:i2)//'.mopheader'
      lun_i = 20
      lun_o = 21
      lun_h = 22
      echelle_c = amax1(echelle, 3.05)

c Open object list file

      open (unit=lun_o, file=objects_name, status='unknown')

c Open image file and find geometry informations

      call open_image (image_name, lun_i, naxis1, naxis2)

c Reads in the header keywords, and stores them at beginning of file.

      call get_header (head_name, lun_h, lun_o, thres1, echelle,
     $  maxcount)

c Determine image segment size and ofsset

      y_seg = max(2*nint(echelle_c), 20)
      nx = naxis1
      ny = image_size_max/nx
      n_seg = (naxis2 - y_seg - 1)/(ny - y_seg) + 1
      y_trans = (naxis2 - y_seg)/n_seg
      ny = y_trans + y_seg
      y_offset = 0
      n_obj_tot = 0

      two_thresh = .true.
      thres2 = thres1
      thres1 = 1.1*thres1

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
     $     n_obj_max, max_el, echelle, echelle_c, .true.)

c Get rid of objects in extrem half of overlap strip

         if (i_seg .lt. n_seg) then
            dummy = float(y_offset + y_trans + y_seg/2)
            do n = 1, n_obj
               if (moments(2,n) .gt. dummy) size_obj(n) = 0
            end do
         end if

         if (i_seg .gt. 1) then
            dummy = float(y_offset + y_seg/2)
            do n = 1, n_obj
               if (moments(2,n) .lt. dummy) size_obj(n) = 0
            end do
         end if

c Write objects in file

         i = 0
         do n = 1, n_obj
            if ((size_obj(n) .gt. 0) .and. (max_int(n) .gt. 0.)
     $        .and. (moments(6,n) .gt. 0.95*sqrt(moments(7,n)))) then
               i = i + 1
               write (lun_o, 9000) moments(1,n), moments(2,n),
     $           moments(6,n), float(size_obj(n)), max_int(n),
     $           moments(5,n), moments(3,n), moments(4,n), moments(7,n),
     $           sfl(n)
            end if
         end do

         n_obj_tot = n_obj_tot + i
         y_offset = y_offset + y_trans
      end do

      write (6, *) 'Found ', n_obj_tot, ' objects total.'
      write (6, *)

      call close_image (lun_i)
      close (lun_o)

c Apparently things went right

      if (n_obj_tot .gt. 0) then
         open (unit=1, file='step1jmp.OK', status='unknown')
         write (1, *) ' '
         close (1)
      end if

c 9000 format (2f8.2, f13.2, f9.1, f10.2, f6.2, 2f6.2, f9.2)
c 9000 format (2f8.2, f13.2, f6, f9.1, f6.2, 2f6.2, f9.2, f6)
 9000 format (2(1x,f7.2), 1x, f12.2, 1x, f5.0, 1x, f8.1, 1x, f5.2,
     $  2(1x,f5.2), 1x, f8.2, 1x, f5.0)

      stop

      end
