C -*-compile-command: "make_lib"; -*-

      subroutine open_image (image_name, lun_i, naxis1, naxis2)
c-------------------------------------------------------------------

      implicit none

      integer*4
     $  mode, naxis, naxes(2), status, blocksize, i,
     $  lun_i, naxis1, naxis2

      character
     $  image_name*80, title*80, comment*80

c Reads FITS file.

      mode = 0
      status=0
      call ftopen (lun_i, image_name, mode, blocksize, status)
      if (status .gt. 0) then
         call ftgerr (status, title)
         write (6, *) 'FITSIO Open Error Status = ', status, ': ', title
         call ftgmsg (title)
         do while (title .ne. ' ')
            write (6, *) title
            call ftgmsg (title)
         end do
         stop
      end if

      call ftgrec (lun_i, 0, comment, status)
      call ftgknj (lun_i, 'NAXIS', 1, 2, naxes, naxis, status)
      if (naxis .ne. 2) then
         write (6, *) 'FTGKNJ failed to read the NAXIS keyword.'
         write (6, *) naxis, (naxes(i),i=1,naxis)
         stop
      end if
      naxis1 = naxes(1)
      naxis2 = naxes(2)

      return
      end
