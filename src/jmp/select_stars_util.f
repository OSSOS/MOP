C -*-compile-command: "make_lib"; -*-
c
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
c This routine selects bright stars that are "isol" pixels away from
c anything else and at least "separ" pixels apart.
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
      subroutine select_bright (x_o, y_o, int_o, n_o, x_br, y_br,
     $  indx_br, n_br, int_max_br, isol, separ, n_min, n_max,
     $  ok, alone, work, indx_w1, indx_w2)

      implicit none

c      include 'match-mov.h'

      integer*4
     $  n_o, n_br, indx_br(*), i, j, max_hist, iter, n_min, n_max,
     $  indx_w1(*), indx_w2(*), n_a, k, l, m, n

      parameter
     $  (max_hist = 400)

      real*4
     $  x_o(*), y_o(*), x_br(*), y_br(*), int_o(*), int_max_br,
     $  br_hist(0:max_hist+1), int_max, int_thresh, d_int,
     $  dist, separ2, isol2, isol, separ, work(*)

      logical
     $  ok(*), alone(*)

c      write (50, *) int_max_br, isol, separ
c      write (50, *)
      isol2 = isol**2
      separ2 = separ**2
      int_max = 0.
      n_a = 0
      do i = 1, n_o
         if (int_o(i) .gt. int_max) int_max = int_o(i)
         alone(i) = .true.
         do j = 1, i-1
            dist = (x_o(i) - x_o(j))**2
     $        + (y_o(i) - y_o(j))**2
            alone(i) = alone(i) .and. (dist .gt. isol2)
     $        .and. ((int_o(j) .lt. int_o(i)) .or. (dist .gt. separ2))
         end do
         do j = i+1, n_o
            dist = (x_o(i) - x_o(j))**2
     $        + (y_o(i) - y_o(j))**2
            alone(i) = alone(i) .and. (dist .gt. isol2)
     $        .and. ((int_o(j) .lt. int_o(i)) .or. (dist .gt. separ2))
         end do
         if (alone(i)) then
            n_a = n_a + 1
            work(n_a) = int_o(i)
            indx_w1(n_a) = i
         end if
         ok(i) = .false.
      end do

c      d_int = float(max_hist)/int_max
c      do i = 0, max_hist + 1
c         br_hist(i) = 0
c      end do
c      do i = 1, n_o
c         if (alone(i)) then
c            j = int(int_o(i)*d_int)
c            br_hist(j) = br_hist(j) + 1
c         end if
c      end do
c      write (50, *) int_max, d_int
c      write (50, *)

      int_max = amin1(int_max_br, int_max)
c      write (50, *) int_max, n_a
c      write (50, *)
c      j = int(int_max*d_int)
c      iter = 0
c      do i = j-1, 0, -1
c         iter = iter + br_hist(i)
c         if (iter .lt. int(n_max)) then
c            int_thresh = i/d_int
c         end if
c      end do
c      write (50, *) int_max, int_thresh, j
c      write (50, *)

      call sortreal (n_a, work, indx_w2)

      do i = n_a, 1, -1
         j = indx_w2(i)
         if (work(j) .le. int_max_br) then
            k = indx_w1(j)
            ok(k) = .true.
            do l = n_a, i+1, -1
               m = indx_w2(l)
               n = indx_w1(m)
               if (ok(n)) then
                  dist = (x_o(k) - x_o(n))**2
     $                 + (y_o(k) - y_o(n))**2
                  if (dist .lt. separ2) then
                     ok(k) = .false.
                     ok(n) = .false.
                  end if
               end if
            end do
         end if
      end do

c      iter = 0
c 100  continue
c      write (50, *) iter, int_thresh
c      write (50, *)
c      do i = 1, n_o
c         ok(i) =  alone(i) .and. (int_o(i) .le. int_max_br)
c     $        .and. (int_o(i) .ge. int_thresh)
c      end do

c      n_br = 0
c      do i = 1, n_o-1
c         if (ok(i)) then
c            do j = i+1, n_o
c               if (ok(j)) then
c                  dist = (x_o(i) - x_o(j))**2
c     $                 + (y_o(i) - y_o(j))**2
c                  if (dist .lt. separ2) then
c                     ok(i) = .false.
c                     ok(j) = .false.
c                  end if
c               end if
c            end do
c            if (ok(i)) n_br = n_br + 1
c            if (ok(i)) then
c               write (50, *) n_br, x_o(i), y_o(i)
c            end if
c         end if
c      end do
c      if (ok(n_o)) n_br = n_br + 1
c      if (ok(n_o)) then
c         write (50, *) n_br, x_o(n_o), y_o(n_o)
c      end if
c      write (50, *)
c      iter = iter + 1

c      if ((n_br .lt. n_min) .and. (iter .lt. 3)) then
c         int_thresh = 0.8*int_thresh
c         goto 100
c      end if

c      n_br = 0
c      do i = 1, n_o
c         if (ok(i)) then
c            n_br = n_br + 1
c            if (n_br .gt. n_max) n_br = n_max
c            x_br(n_br) = x_o(i)
c            y_br(n_br) = y_o(i)
c            indx_br(n_br) = i
c         end if
c      end do

      n_br = 0
      do i = n_a, 1, -1
         j = indx_w2(i)
         k = indx_w1(j)
         if (ok(k)) then
            n_br = n_br + 1
            x_br(n_br) = x_o(k)
            y_br(n_br) = y_o(k)
            indx_br(n_br) = k
c            write (50, *) n_br, x_o(k), y_o(k)
            if (n_br .ge. n_max) return
         end if
      end do

      return
      end

c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
c This routine selects stars at the center of the chip.
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
      subroutine select_center (x, y, indx, n, xc, yc, indxc, nc,
     $  n_min_d, naxis1, naxis2)

c      include 'match-mov.h'

      integer*4
     $  n, nc, n_min, n_min_d, i, naxis1, naxis2, indx(*), indxc(*)

      real*4
     $  x(*), y(*), xc(*), yc(*), max_dx, max_dy, xcent, ycent

      xcent = float(naxis1)/2.
      ycent = float(naxis2)/2.
      max_dx = xcent/2.
      max_dy = ycent/2.
      n_min = min(n, n_min_d)
 100  continue
      nc = 0
      do i = 1, n
         if ((abs(x(i)-xcent) .lt. max_dx)
     $     .and. (abs(y(i)-ycent) .lt. max_dy)) then
            nc = nc + 1
            xc(nc) = x(i)
            yc(nc) = y(i)
            indxc(nc) = indx(i)
         end if
      end do
      if ((nc .lt. n_min)
     $  .and. (max_dx .lt. xcent) .and. (max_dy .lt. ycent)) then
         max_dx = max_dx*1.2
         max_dy = max_dy*1.2
         goto 100
      end if

      return
      end

c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
c This routine rejects stars that are on bad columns.
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
      subroutine bad_column (moments, size_obj, n_obj, image, nx, ny,
     $  y_offset, echelle)

      integer*4
     $  jv, iv, k, n, n_obj, size_obj(*), y_offset, nx, ny

      real*4
     $  image(nx,ny), moments(7, *), maxh, minh, maxv, minv, tmp2,
     $  echelle

      do n = 1, n_obj
         if (size_obj(n) .gt. 0) then
            jv = nint(moments(1,n))
            iv = nint(moments(2,n)) - y_offset
            maxh = 0.
            minh = image(jv,iv)
            do k = max(1, jv-nint(echelle)),
     $        min(nx, jv+nint(echelle))
               tmp2 = image(k,iv)
               if (tmp2 .lt. minh) minh = tmp2
               if (tmp2 .gt. maxh) maxh = tmp2
            end do
            maxh = maxh - minh
            maxv = 0.
            minv = image(jv,iv)
            do k = max(1, iv-nint(echelle)),
     $        min(ny, iv+nint(echelle))
               tmp2 = image(jv, k)
               if (tmp2 .lt. minv) minv = tmp2
               if (tmp2 .gt. maxv) maxv = tmp2
            end do
            maxv = maxv - minv
            if ((maxv .gt. 5.*maxh)
     $        .or. (maxh .gt. 5.*maxv)) then
               size_obj(n) = 0
            end if
         end if
      end do

      return
      end
