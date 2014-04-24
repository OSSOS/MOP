C -*-compile-command: "g77 -O2 -fno-automatic -funroll-loops -malign-double -o ../../bin/LINUX/stepZjmp stepZjmp.f -L../../lib/LINUX -ljmp -lcfitsio"; -*-
C When actually using g77, add the option -fno-automatic

      implicit none

      include 'match-mov.h'

      integer*4
     $  image_size_max

      parameter
     $  (image_size_max = 2048*1100)

      integer*4
     $  i, i1, i2, naxis1, naxis2, lun_i, lun_o, narg, iargc,
     $  j, lun_h

      character
     $  image_name*80, base_name*80, arg*80,
     $  line*80, head_name*80

      logical
     $  finished, help, no_f


c Create a file for later error handling

      open (unit=1, file='stepZjmp.FAILED', status='unknown')
      write (1, *) ' '
      close (1)

c Get input parameters

      do i = 1, 80
         base_name(i:i) = char(0)
      end do

      narg = iargc()
      i = 1
      no_f = .true.
      help = (narg .ne. 2)

 90   continue
      if (i .le. narg) then
         call getarg (i, arg)
         if (arg(1:2) .eq. '-f') then
            i = i + 1
            call getarg (i, base_name)
            no_f = .false.
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

      open (unit=1, file='stepZjmp.FAILED', status='unknown')
      write (1, *) line(1:j-1)
      close (1)

      help =  help .or. no_f

      if (help) then
         write (6, *)
     $     'Usage: stepZjmp -f <image file>'
         write (6, *) 'where:'
         write (6, *) '-f <image file>: image to process (no extension)'
         stop
      end if

 80   continue

      call read_file_name (base_name, i1, i2, finished, 80)
      if (finished) stop

      image_name = base_name(i1:i2)//'.fits'
      head_name = base_name(i1:i2)//'.mopheader'
      lun_i = 20
      lun_h = 22

c Open image file and find geometry informations

      call open_image (image_name, lun_i, naxis1, naxis2)

c Reads in the header keywords, and stores them in header file.

      call create_header (lun_i, naxis1, naxis2, head_name, lun_h)

c Close files.

      call close_image (lun_i)

c Apparently things went right

      open (unit=1, file='stepZjmp.OK', status='unknown')
      write (1, *) ' '
      close (1)

      stop

      end
