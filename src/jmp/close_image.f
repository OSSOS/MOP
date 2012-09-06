C -*-compile-command: "make_lib"; -*-

      subroutine close_image (lun_i)
c--------------------------------------------------------------

      integer*4
     $  lun_i, status

      character
     $  title*80

      call ftclos (lun_i, status)

      status=0
      if (status .gt. 0) then
         call ftgerr (status, title)
         write (6, *) 'FITSIO Close Error Status = ', status, ': ', title
         call ftgmsg (title)
         do while (title .ne. ' ')
            write (6, *) title
            call ftgmsg (title)
         end do
         stop
      end if

      return
      end
