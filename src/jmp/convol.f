C -*-compile-command: "make_lib"; -*-

      subroutine convol (image, echelle, coeff, im_sky, tmp1, nx, ny)
c     ----------------------------------------------------------

      integer*4
     $  i, j, k, i1, i2, nx, ny

      real*4
     $  image(nx,ny), coeff(nx,ny), tmp1(nx,ny), tmp,
     $  im_sky(nx,ny), echelle

      k = 1
      do i = 1 + k, ny - k
         i1 = i - k
         i2 = i + k
         do j = 1, nx
            tmp1(j,i) = image(j,i)*0.5
     $           +(image(j,i2) + image(j,i1))*0.25
         end do
      end do
      do i = 1, k
         i2 = i + k
         do j = 1, nx
            tmp1(j,i) = image(j,i)*0.5
     $           +(image(j,i2) + image(j,1))*0.25
         end do
      end do
      do i = ny - k + 1, ny
         i1 = i - k
         do j = 1, nx
            tmp1(j,i) = image(j,i)*0.5
     $           +(image(j,ny) + image(j,i1))*0.25
         end do
      end do

      do i = 1, ny
         do j = 1 + k, nx - k
            coeff(j,i) = tmp1(j,i)*0.5
     $           +(tmp1(j+k,i) + tmp1(j-k,i))*0.25
         end do
      end do
      do i = 1, ny
         do j = 1, k
            coeff(j,i) = tmp1(j,i)*0.5
     $           +(tmp1(j+k,i) + tmp1(1,i))*0.25
         end do
      end do
      do i = 1, ny
         do j = nx - k + 1, nx
            coeff(j,i) = tmp1(j,i)*0.5
     $           +(tmp1(nx,i) + tmp1(j-k,i))*0.25
         end do
      end do

 100  continue
      k = k*2
      if (float(2*k-1) .lt. echelle) then
         do i = 1 + k, ny - k
            i1 = i - k
            i2 = i + k
            do j = 1, nx
               tmp1(j,i) = coeff(j,i)*0.5
     $           +(coeff(j,i2) + coeff(j,i1))*0.25
            end do
         end do
         do i = 1, k
            i2 = i + k
            do j = 1, nx
               tmp1(j,i) = coeff(j,i)*0.5
     $           +(coeff(j,i2) + coeff(j,1))*0.25
            end do
         end do
         do i = ny - k + 1, ny
            i1 = i - k
            do j = 1, nx
               tmp1(j,i) = coeff(j,i)*0.5
     $           +(coeff(j,ny) + coeff(j,i1))*0.25
            end do
         end do

         do i = 1, ny
            do j = 1 + k, nx - k
               coeff(j,i) = tmp1(j,i)*0.5
     $           +(tmp1(j+k,i) + tmp1(j-k,i))*0.25
            end do
         end do
         do i = 1, ny
            do j = 1, k
               coeff(j,i) = tmp1(j,i)*0.5
     $           +(tmp1(j+k,i) + tmp1(1,i))*0.25
            end do
         end do
         do i = 1, ny
            do j = nx - k + 1, nx
               coeff(j,i) = tmp1(j,i)*0.5
     $           +(tmp1(nx,i) + tmp1(j-k,i))*0.25
            end do
         end do
         goto 100
      end if

      do i = 1 + k, ny - k
         i1 = i - k
         i2 = i + k
         do j = 1, nx
            tmp1(j,i) = coeff(j,i)*0.5
     $           +(coeff(j,i2) + coeff(j,i1))*0.25
         end do
      end do
      do i = 1, k
         i2 = i + k
         do j = 1, nx
            tmp1(j,i) = coeff(j,i)*0.5
     $           +(coeff(j,i2) + coeff(j,1))*0.25
         end do
      end do
      do i = ny - k + 1, ny
         i1 = i - k
         do j = 1, nx
            tmp1(j,i) = coeff(j,i)*0.5
     $           +(coeff(j,ny) + coeff(j,i1))*0.25
         end do
      end do

      do i = 1, ny
         do j = 1 + k, nx - k
            tmp = tmp1(j,i)*0.5
     $        +(tmp1(j+k,i) + tmp1(j-k,i))*0.25
            coeff(j,i) = coeff(j,i) - tmp
            im_sky(j,i) = image(j,i) - tmp
         end do
      end do
      do i = 1,ny
         do j = 1,k
            tmp = tmp1(j,i)*0.5
     $           +(tmp1(j+k,i) + tmp1(1,i))*0.25
            coeff(j,i) = coeff(j,i) - tmp
            im_sky(j,i) = image(j,i) - tmp
         end do
      end do
      do i = 1,ny
         do j = nx-k+1,nx
            tmp = tmp1(j,i)*0.5
     $           +(tmp1(nx,i) + tmp1(j-k,i))*0.25
            coeff(j,i) = coeff(j,i) - tmp
            im_sky(j,i) = image(j,i) - tmp
         end do
      end do

      return
      end
