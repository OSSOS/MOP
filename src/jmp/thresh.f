C -*-compile-command: "make_lib"; -*-

      subroutine thresh (coeff, mask, mean, sig, thres1, thres2,
     $  two_thresh, image, sky, nx, ny)
c-----------------------------------------------------------------

      integer*4
     $  nx, ny, i, j, ns

      real*4
     $  coeff(nx,ny), mask(nx,ny), image(nx,ny), mean, sig, sky, thres1,
     $  thres2, t1, t2, lsky

      logical
     $  two_thresh

      t1 = mean + sig*thres1
      if (two_thresh) then
         t2 = mean + sig*thres2
      else
         t2 = t1
      end if

      ns = 0
      sky = 0.
      do i = 1, ny
         lsky = 0.
         do j = 1, nx
            if (coeff(j,i) .ge. t1) then
               mask(j,i) = -2.
            else if (coeff(j,i) .ge. t2) then
               mask(j,i) = -1.
            else
               mask(j,i) = 0.
               ns = ns + 1
               lsky = lsky + image(j,i)
            end if
         end do
         sky = sky + lsky
      end do
      if (ns .gt. 0) sky = sky/float(ns)

      return
      end
