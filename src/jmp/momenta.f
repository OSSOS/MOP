C -*-compile-command: "make_lib"; -*-

      subroutine momenta (coeff, mean, sig, thresh, nx, ny)

      integer*4
     $  k, i, j, n, nx, ny

      real*4
     $  coeff(nx,ny), mean, sig, c_max, c_min, thresh, lmean, lsig

      c_max = -1.e31
      c_min = 1.e31
      do i = 1, ny
         do j = 1, nx
            c_max = amax1(c_max, coeff(j,i))
            c_min = amin1(c_min, coeff(j,i))
	end do
      end do

      do k = 1, 10
         mean = 0.e0
         sig = 0.e0
         n = 0
         do i = 1, ny
            lmean = 0.
            lsig = 0.
            do j = 1, nx
               if ((coeff(j,i) .gt. c_min) .and.
     $           (coeff(j,i) .lt. c_max)) then 
                  lmean = lmean + coeff(j,i)
                  lsig = lsig + coeff(j,i)**2
                  n = n+1
               end if
            end do
            mean = mean + lmean
            sig = sig + lsig
         end do
         mean = mean/float(n)
         sig = sqrt(sig/float(n) - mean**2)
         c_max =  mean + sig*thresh
         c_min =  mean - sig*thresh
      end do

      return
      end
