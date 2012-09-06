C -*-compile-command: "make_lib"; -*-

      subroutine read_center (image, lun_i, nx, ny, naxis1, naxis2)
c---------------------------------------------------------------

      integer*4
     $  nx, ny, i, nelements, lun_i, status, group, fpixel,
     $  naxis1, naxis2

      real*4
     $  image(nx,ny), nullval

      logical
     $  anynull

      nelements = nx
      group = 1
      fpixel = 1 + (naxis2 - ny)/2*naxis1 + (naxis1 - nx)/2
      nullval = 0.
      do i = 1, ny
         call ftgpve (lun_i, group, fpixel, nelements, nullval,
     $     image(1,i), anynull, status)
         fpixel = fpixel + naxis1
      end do

      return
      end
