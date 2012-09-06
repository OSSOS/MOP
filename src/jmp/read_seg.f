C -*-compile-command: "make_lib"; -*-

      subroutine read_seg (image, y_offset, lun_i, nx, ny)
c---------------------------------------------------------

      integer*4
     $  nx, ny, y_offset, nelements, lun_i, status, group, fpixel

      real*4
     $  image(nx,ny), nullval

      logical
     $  anynull

      character
     $  title*80

      status=0
      nelements = nx*ny
      group = 1
      fpixel = 1 + y_offset*nx
      nullval = 0.
      call ftgpve (lun_i, group, fpixel, nelements, nullval,
     $  image, anynull, status)

      if (status .gt. 0) then
         call ftgerr (status, title)
         write (6, *) 'FITSIO Read Error Status = ', status, ': ', title
         call ftgmsg (title)
         do while (title .ne. ' ')
            write (6, *) title
            call ftgmsg (title)
         end do
         stop
      end if

      return
      end
