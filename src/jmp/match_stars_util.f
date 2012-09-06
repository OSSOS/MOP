C -*-compile-command: "make_lib"; -*-
c
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
c This routine create a list of "well behaved" triangles.
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
      subroutine build_triangles (x, y, n, tr_r, tr_i,
     $  n_tr, max_dist)

      include 'match-mov.h'

      integer*4
     $  n, tr_i(4,*), n_tr, i(3), i1, i2, i3, j1, j2, j3, k

      real*4
     $  x(*), y(*), tr_r(3,*), max_dist, a, b, c, d(3),
     $  max_dist2

      max_dist2 = max_dist**2

      n_tr = 0
      do i1 = 1, n-2
         i(1) = i1
         do i2 = i1 + 1, n-1
            i(2) = i2
            d(3) = (x(i2) - x(i1))**2 + (y(i2) - y(i1))**2
            if (d(3) .lt. max_dist2) then
               do i3 = i2 + 1, n
                  i(3) = i3
                  d(2) = (x(i3) - x(i1))**2 + (y(i3) - y(i1))**2
                  if (d(2) .lt. max_dist2) then
                     d(1) = (x(i3) - x(i2))**2 + (y(i3) - y(i2))**2
                     if (d(1) .lt. max_dist2) then
                        a = 0.
                        do k = 1, 3
                           if (d(k) .gt. a) then
                              a = d(k)
                              j1 = i(k)
                           end if
                        end do
                        c = a
                        do k = 1, 3
                           if (d(k) .le. c) then
                              c = d(k)
                              j3 = i(k)
                           end if
                        end do
                        do k = 1, 3
                           if ((i(k) .ne. j1) .and. (i(k) .ne. j3)) then
                              b = d(k)
                              j2 = i(k)
                           end if
                        end do
                        b = b/a
                        c = c/a
                        if ((b .lt. 0.9) .and. (c .lt. 0.9*b)
     $                       .and. (c .gt. 0.1)) then
                           n_tr = n_tr + 1
                           if (n_tr .gt. n_o_max_cat)
     $                       n_tr = n_o_max_cat
                           tr_i(1,n_tr) = j1
                           tr_i(2,n_tr) = j2
                           tr_i(3,n_tr) = j3
                           tr_i(4,n_tr) = 0
                           tr_r(1,n_tr) = a
                           tr_r(2,n_tr) = b
                           tr_r(3,n_tr) = c
                        end if
                     end if
                  end if
               end do
            end if
         end do
      end do

      return
      end
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
c This routine computes the number of matching between each pair of star.
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
      subroutine match_triangles (tr1_i, tr1_r, n1, tr2_i, tr2_r, n2,
     $  n_match, radius, scale, nm)

      include 'match-mov.h'

      integer*4
     $  n1, n2, i1, i2, nm, tr1_i(4,*), tr2_i(4,*),
     $  n_match(n_br_max,n_br_max), i

      real*4
     $  tr1_r(3,*), tr2_r(3,*), radius, r2, d2, scale, r_min, r_max

      logical
     $  l_scale

      l_scale = .false.
      if (scale .gt. 0.) then
         r_max = scale*1.1
         r_min = scale*0.9
         l_scale = .true.
      end if
      r2 = radius**2
      nm = 0
      do i2 = 1, n_br_max
         do i1 = 1, n_br_max
            n_match(i1,i2) = 0
         end do
      end do

      do i1 = 1, n1
         do i2 = 1, n2
            d2 = (tr1_r(2,i1) - tr2_r(2,i2))**2
     $         + (tr1_r(3,i1) - tr2_r(3,i2))**2
            if (d2 .lt. r2) then
               if (l_scale) then
                  d2 = tr2_r(1,i2)/tr1_r(1,i1)
                  if ((d2 .gt. r_min) .and. (d2 .lt. r_max)) then
                     do i = 1, 3
                        n_match(tr1_i(i,i1),tr2_i(i,i2)) = 1 +
     $                       n_match(tr1_i(i,i1),tr2_i(i,i2))
                     end do
                     tr1_i(4,i1) = i2
                     tr2_i(4,i2) = i1
                     nm = nm + 1
                  end if
               else
                  do i = 1, 3
                     n_match(tr1_i(i,i1),tr2_i(i,i2)) = 1 +
     $                    n_match(tr1_i(i,i1),tr2_i(i,i2))
                  end do
                  tr1_i(4,i1) = i2
                  tr2_i(4,i2) = i1
                  nm = nm + 1
               end if
            end if
         end do
      end do

      return
      end
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
c This routine finds out the pairs that got the most matchings.
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
      subroutine best_match (n_match, n1, n2, best_m, n_best)

      include 'match-mov.h'

      integer*4
     $  n_match(n_br_max,n_br_max), best_m(3,*), n_best, n1, n2, n,
     $  i, i1, i2, buf(n_br_max*n_br_max), indx(n_br_max*n_br_max),
     $  j, k, id1(n_br_max), id2(n_br_max)

c best_m(1,*) : index of star of first list.
c best_m(2,*) : index of star of second list.
c best_m(3,*) : number of matchings.

      n = n1*n2
      i = 0
      do i1 = 1, n1
         do i2 = 1, n2
            i = i + 1
            buf(i) = n_match(i1,i2)
         end do
      end do
      do i1 = 1, n1
         id1(i1) = 0
      end do
      do i2 = 1, n2
         id2(i2) = 0
      end do

      call sortint (n, buf, indx)

      n_best = min(n1, n2)
      k = 0
      do j = 1, n_best
         i = indx(n + 1 - j)
         i1 = (i-1)/n2 + 1
         i2 = i - (i1-1)*n2
         if ((id1(i1) .eq. 0) .and. (id2(i2) .eq. 0)) then
            k = k + 1
            best_m(1,k) = i1
            best_m(2,k) = i2
            best_m(3,k) = buf(i)
            id1(i1) = i2
            id2(i2) = i1
         end if
      end do
      n_best = k

 100  continue
      if (n_best .gt. 1) then
         if (best_m(3,n_best) .lt. 3) then
            n_best = n_best - 1
            goto 100
         end if
      end if

      return
      end
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
c This routine compares 2 lists and give the corresponding stars.
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
      subroutine match_lists (x1, y1, ident1, n1, x2, y2, ident2, n2,
     $  dist12, tol_dist)

      implicit none

      integer*4
     $  i, j, n1, n2, ident1(*), ident2(*)

      real*4
     $  x1(*), y1(*), x2(*), y2(*), tol_dist, dist12(*),
     $  d_min, dist, dx, dy

      do i = 1, n1
         ident1(i) = 0
      end do
      do i = 1, n2
         ident2(i) = 0
      end do

      do i = 1, n1
         d_min = 2.*tol_dist
         do j = 1, n2
            dy = abs(y1(i) - y2(j))
            if (dy .le. tol_dist) then
               dx = abs(x1(i) - x2(j))
               if (dx .le. tol_dist) then
                  dist = sqrt(dx**2 + dy**2)
                  if (dist .lt. d_min) then
                     ident1(i) = j
                     d_min = dist
                  end if
               end if
            end if
         end do
         j = ident1(i)
         if (j .gt. 0) then
            if (ident2(j) .eq. 0) then
               ident2(j) = i
               dist12(j) = d_min
            else
               if (d_min .lt. dist12(j)) then
                  ident1(ident2(j)) = 0
                  ident2(j) = i
                  dist12(j) = d_min
               else
                  ident1(i) = 0
               end if
            end if
         end if
      end do

      return
      end
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
c This routine compares 2 lists and give the corresponding stars.
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
      subroutine match_lists_new (x1, y1, ident1, n1, x2, y2, ident2,
     $  n2, dist12, tol_dist, indx)

      implicit none

      integer*4
     $  indsize

      real*4
     $  step

      parameter
     $  (indsize = 1000, step = 10.)

      integer*4
     $  i, j, n1, n2, ident1(*), ident2(*), indx(*), sy(0:indsize),
     $  j1, j2, js

      real*4
     $  x1(*), y1(*), x2(*), y2(*), tol_dist, dist12(*),
     $  d_min, dist, dx, dy, yl, yh

      do i = 1, n1
         ident1(i) = 0
      end do
      do i = 1, n2
         ident2(i) = 0
      end do

      do i = 0, indsize
         sy(i) = 1
      end do
      call sortreal (n2, y2, indx)
      do i = 1, n2
         j = int(y2(indx(i))/step) + 1
         sy(j) = i
      end do
      do i = 2, indsize
         if (sy(i) .eq. 1) sy(i) = sy(i-1)
      end do

      do i = 1, n1
         d_min = 2.*tol_dist
         yl = y1(i) - tol_dist
         yh = y1(i) + tol_dist
         j1 = sy(max(int(yl/step), 0))
         j2 = sy(min(int(yh/step)+1, indsize))
         do js = j1, j2
            j = indx(js)
            dy = abs(y1(i) - y2(j))
            if (dy .le. tol_dist) then
               dx = abs(x1(i) - x2(j))
               if (dx .le. tol_dist) then
                  dist = sqrt(dx**2 + dy**2)
                  if (dist .lt. d_min) then
                     ident1(i) = j
                     d_min = dist
                  end if
               end if
            end if
         end do
         j = ident1(i)
         if (j .gt. 0) then
            if (ident2(j) .eq. 0) then
               ident2(j) = i
               dist12(j) = d_min
            else
               if (d_min .lt. dist12(j)) then
                  ident1(ident2(j)) = 0
                  ident2(j) = i
                  dist12(j) = d_min
               else
                  ident1(i) = 0
               end if
            end if
         end if
      end do

      return
      end
