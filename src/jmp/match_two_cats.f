C -*-compile-command: "make_lib"; -*-

      subroutine select_bright_old (n_o_1, inten_max_br_1,
     $  inten_thresh_1, coo_1, coo_1_br, inten_1, dist_1,
     $  participate_1, nb_1, min_dist_br, n_br_max, inten_max)
c----------------------------------------------------------------

      implicit none

      integer*4
     $  i, n, nb_1, n_o_1, n_br_max, max_hist, iter, j

      parameter
     $  (max_hist = 200)

      real*4
     $  inten_max_br_1, inten_thresh_1, coo_1(2,*), coo_1_br(2,*),
     $  inten_1(*), dist_1(n_br_max,n_br_max), min_dist_br,
     $  br_hist(0:max_hist+1), dinten, inten_max

      logical
     $  participate_1(*)

      do i = 1,n_br_max
        participate_1(i) = .false.
      end do

      inten_max_br_1 = 0.
      do i = 1, n_o_1
         if (inten_1(i) .gt. inten_max_br_1) inten_max_br_1 = inten_1(i)
      end do

      dinten = float(max_hist)/inten_max_br_1
      do i = 0, max_hist + 1
         br_hist(i) = 0
      end do
      do i = 1, n_o_1
         j = int(inten_1(i)*dinten)
         br_hist(j) = br_hist(j) + 1
      end do

      inten_max_br_1 = amin1(inten_max, inten_max_br_1)
      j = int(inten_max_br_1*dinten)
      iter = 0
      do i = j-1, 0, -1
         iter = iter + br_hist(i)
         if (iter .lt. n_br_max) then
            inten_thresh_1 = i/dinten
         end if
      end do

  502 continue
 2001 format(a,T40,2i10)
c     select br stars in cat 1
c     ----------------------------------------
      nb_1 = 0
      do n = 1,n_o_1
        if(inten_1(n).gt.inten_thresh_1 
     *     .and. inten_1(n).lt.inten_max_br_1
     *                                      ) then
            nb_1 = nb_1+1
            participate_1(nb_1) = .true.
            coo_1_br(1,nb_1) = coo_1(1,n)
            coo_1_br(2,nb_1) = coo_1(2,n)
          if(nb_1.eq.n_br_max) goto 100
        end if
      end do

  100 continue
      if(nb_1.lt.3) then
        write(6,2000) 'number of br stars in cat1 too small',nb_1
        stop
 2000 format(a,T40,i10)
      end if

      do i = 1, nb_1-1
         dist_1(i,i) = 0.e0  
         do j = i+1, nb_1
            dist_1(i,j) = 
     *        sqrt((coo_1_br(1,i)-coo_1_br(1,j))**2
     *        + (coo_1_br(2,i)-coo_1_br(2,j))**2)
            dist_1(j,i) = dist_1(i,j)
            if (dist_1(j,i) .lt. min_dist_br) then
               participate_1(i) = .false.
               participate_1(j) = .false.
            end if
         end do
      end do 
      dist_1(nb_1,nb_1) = 0.e0

      return
      end
c
      subroutine register_two_cats (nb_1, nb_2, coo_1_br,
     $  coo_2_br, offset_x, offset_y, stan_dev_x, stan_dev_y,
     $  dist_1, dist_2, participate_1, participate_2,
     $  tol_br_dist, diff_cat_x, diff_cat_y, n_br_max)
c------------------------------------------------------

      implicit none

      integer*4
     $  i1, i2, j1, j2, k1, k2, nb_1, nb_2, n, n_triangles, i,
     $  nz, nmax_x, nmax_y, nmin_x, nmin_y, n_br_max

      real*4
     $  coo_1_br(2,*), coo_2_br(2,*), dist_1(n_br_max,n_br_max),
     $  dist_2(n_br_max,n_br_max), offset_x, offset_y, stan_dev_x,
     $  stan_dev_y, tol_br_dist, diff_cat_x(*), diff_cat_y(*)

      real*4
     $  diff_cat1_cat2_x_1, diff_cat1_cat2_x_2, diff_cat1_cat2_x_3,
     $  diff_cat1_cat2_y_1, diff_cat1_cat2_y_2, diff_cat1_cat2_y_3,
     $  offset_min_x, offset_min_y, offset_max_x, offset_max_y      
   
      logical
     $  participate_1(*), participate_2(*), successful

c     find translation between both cats
c      coo_2_br(1,*) =  coo_1_br(1,*)+offset_x
c      coo_2_br(2,*) =  coo_1_br(2,*)+offset_y
c     ----------------------------------------

      n_triangles = 0
      n = 0

c     select triangles
c     ----------------
      do i1 = 1,nb_1-2
      if(participate_1(i1)) then

        do j1 = i1+1,nb_1-1
        if(participate_1(j1) .and. j1.gt.i1) then

          do i2 = 1,nb_2-2
          if(participate_2(i2)) then
            do j2 = i2+1,nb_2-1
            if(participate_2(j2)) then
              if(abs(dist_1(i1,j1)-dist_2(i2,j2)).lt.
     *             tol_br_dist) then
                do k1 = j1+1,nb_1
                if(participate_1(k1) .and. k1.gt.j1) then 
                  do k2 = j2+1,nb_2
                  if(participate_2(k2)) then
                    if(abs(dist_1(i1,k1)-dist_2(i2,k2)).lt.
     *                   tol_br_dist .and.
     *                  abs(dist_1(j1,k1)-dist_2(j2,k2)).lt.
     *                   tol_br_dist) then

                      diff_cat1_cat2_x_1 = coo_2_br(1,i2) -
     *                                   coo_1_br(1,i1)
                      diff_cat1_cat2_y_1 = coo_2_br(2,i2) -
     *                                   coo_1_br(2,i1)

                      diff_cat1_cat2_x_2 = coo_2_br(1,j2) -
     *                                   coo_1_br(1,j1)
                      diff_cat1_cat2_y_2 = coo_2_br(2,j2) -
     *                                   coo_1_br(2,j1)
                      diff_cat1_cat2_x_3 = coo_2_br(1,k2) -
     *                                   coo_1_br(1,k1)
                      diff_cat1_cat2_y_3 = coo_2_br(2,k2) -
     *                                   coo_1_br(2,k1)
                      successful = abs(diff_cat1_cat2_x_1 -
     *                                  diff_cat1_cat2_x_2 ).lt.
     *                                  tol_br_dist
                      successful = successful .and.
     *                             abs(diff_cat1_cat2_x_1 -
     *                                  diff_cat1_cat2_x_3 ).lt.
     *                                  tol_br_dist
                      successful = successful .and.
     *                             abs(diff_cat1_cat2_x_2 -
     *                                  diff_cat1_cat2_x_3 ).lt.
     *                                  tol_br_dist

                      successful = successful .and.
     *                             abs(diff_cat1_cat2_y_1 -
     *                                  diff_cat1_cat2_y_2 ).lt.
     *                                  tol_br_dist

                      successful = successful .and.
     *                             abs(diff_cat1_cat2_y_1 -
     *                                  diff_cat1_cat2_y_3 ).lt.
     *                                  tol_br_dist

                      successful = successful .and.
     *                             abs(diff_cat1_cat2_y_2 -
     *                                  diff_cat1_cat2_y_3 ).lt.
     *                                  tol_br_dist

                      if(successful) then
                        n = n+1
                        diff_cat_x(n) = diff_cat1_cat2_x_1
                        diff_cat_y(n) = diff_cat1_cat2_y_1
                        n = n+1
                        diff_cat_x(n) = diff_cat1_cat2_x_2
                        diff_cat_y(n) = diff_cat1_cat2_y_2
                        n = n+1
                        diff_cat_x(n) = diff_cat1_cat2_x_3
                        diff_cat_y(n) = diff_cat1_cat2_y_3
                        n_triangles = n_triangles+1
                        goto 50
                      end if
                    end if
		  end if
                  end do
                end if
		end do

              end if
            end if
            end do
          end if
          end do
        end if
        end do

   50 continue
      end if
      end do

      if(n_triangles.lt.2) then
         write (6, *) 'No transformation. 1'
c        write(6,2000) 'n_triangles too small',n_triangles
c        write(6,2002) 'number of br stars in cat1 and cat2',nb_1,nb_2
 2000 format(a,T40,i10)
 2002 format(a,T40,2i10)
         stop
c	 write(6,*) 'type offset_x,offset_y'
c	 read(5,*) offset_x,offset_y
c	 stan_dev_x = 0.e0
c	 stan_dev_y = 0.0e0
c	 goto 55
      end if

      offset_x = 0.e0  
      offset_y = 0.e0  
      stan_dev_x = 0.e0  
      stan_dev_y = 0.e0  

      do i = 1,n
          offset_x = offset_x+diff_cat_x(i)
          offset_y = offset_y+diff_cat_y(i)
          stan_dev_x = stan_dev_x + diff_cat_x(i)**2
          stan_dev_y = stan_dev_y + diff_cat_y(i)**2
          participate_1(i) = .true.
      end do

      offset_x = offset_x/float(n)
      offset_y = offset_y/float(n)
      stan_dev_x = sqrt(amax1(stan_dev_x/float(n) - offset_x**2, 0.))
      stan_dev_y = sqrt(amax1(stan_dev_y/float(n) - offset_y**2, 0.))
      if(stan_dev_x.lt.1.0e0 .and. stan_dev_y.lt.1.0e0) goto 55

      offset_max_x = -1.e31
      offset_min_x = 1.e31
      offset_max_y = -1.e31
      offset_min_y = 1.e31

      do i = 1,n
        offset_max_x = amax1(offset_max_x,diff_cat_x(i))
        offset_max_y = amax1(offset_max_y,diff_cat_y(i))
        offset_min_x = amin1(offset_min_x,diff_cat_x(i))
        offset_min_y = amin1(offset_min_y,diff_cat_y(i))
      end do
      if(abs(offset_max_x-offset_min_x) .lt.1.e0 .and.
     *   abs(offset_max_y-offset_min_y).lt.1.e0) then
         write (6, *) 'No transformation. 2'
c         write(6,*) 'problem to determine offset'
c         write(6,*) 'offset_x,offset_y',offset_x,offset_y
c         write(6,*) 'stan_dev_x,stan_dev_y',stan_dev_x,stan_dev_y
c         write(6,*) 'offset_max_x,offset_max_y',
c     *               offset_max_x,offset_max_y
c         write(6,*) 'offset_min_x,offset_min_y',
c     *               offset_min_x,offset_min_y
c	 write(6,*) 'type offset_x,offset_y'
         stop
c	 read(5,*) offset_x,offset_y
c	 stan_dev_x = 0.e0
c	 stan_dev_y = 0.0e0
c	 goto 55
      end if

      nmax_x = 0
      nmax_y = 0
      nmin_x = 0
      nmin_y = 0
      do i = 1,n
	if(abs(diff_cat_x(i)-offset_max_x).lt.1.)
     *     nmax_x = nmax_x+1
	if(abs(diff_cat_y(i)-offset_max_y).lt.1.)
     *     nmax_y = nmax_y+1
	if(abs(diff_cat_x(i)-offset_min_x).lt.1.)
     *     nmin_x = nmin_x+1
	if(abs(diff_cat_y(i)-offset_min_y).lt.1.)
     *     nmin_y = nmin_y+1
      end do

      if(nmax_x.gt.nmin_x) then
	 offset_x = offset_max_x
      else
	offset_x = offset_min_x
      end if
      if(abs(offset_x).lt.1.e-1) then
	if(abs(offset_max_x).gt.abs(offset_min_x)) then
	  offset_x = offset_max_x
        else
	  offset_x = offset_min_x
        end if
      end if
      if(nmax_y.gt.nmin_y) then
	 offset_y = offset_max_y
      else
	offset_y = offset_min_y
      end if
      if(abs(offset_y).lt.1.e-1) then
	if(abs(offset_max_y).gt.abs(offset_min_y)) then
	  offset_y = offset_max_y
        else
	  offset_y = offset_min_y
        end if
      end if

      do i = 1,n
          if(abs(diff_cat_x(i)-offset_x).gt.1.) 
     *        participate_1(i) = .false.
          if(abs(diff_cat_y(i)-offset_y).gt.1.) 
     *        participate_1(i) = .false.
      end do

      offset_x = 0.e0
      offset_y = 0.e0
      stan_dev_x = 0.e0  
      stan_dev_y = 0.e0  
      nz = 0
      do i = 1,n
        if(participate_1(i)) then
          offset_x = offset_x + diff_cat_x(i)
          offset_y = offset_y + diff_cat_y(i)
          stan_dev_x = stan_dev_x + diff_cat_x(i)**2
          stan_dev_y = stan_dev_y + diff_cat_y(i)**2
          nz = nz+1
        end if
      end do
      if(nz.lt.3) then
         write (6, *) 'No transformation. 3'
c                 write(6,*) 'problem to determine offset'
c         write(6,*) 'offset_x,offset_y',offset_x,offset_y
c         write(6,*) 'stan_dev_x,stan_dev_y',stan_dev_x,stan_dev_y
c         write(6,*) 'offset_max_x,offset_max_y',
c     *               offset_max_x,offset_max_y
c	 write(6,*) 'nmax_x,nmax_y',
c     *               nmax_x,nmax_y
c         write(6,*) 'offset_min_x,offset_min_y',
c     *               offset_min_x,offset_min_y
c	 write(6,*) 'nmin_x,nmin_y',
c     *               nmin_x,nmin_y
c	 write(6,*) 'type offset_x,offset_y'
         stop
c	 read(5,*) offset_x,offset_y
c	 stan_dev_x = 0.e0
c	 stan_dev_y = 0.0e0
c	 goto 55
      end if
      offset_x = offset_x/float(nz)
      offset_y = offset_y/float(nz)
      stan_dev_x = sqrt(amax1(stan_dev_x/float(nz) - offset_x**2, 0.))
      stan_dev_y = sqrt(amax1(stan_dev_y/float(nz) - offset_y**2, 0.))

   55 continue

      return
      end
c
      subroutine match_two_cats (coo_1, coo_2, offset_x, offset_y,
     $  ident1, ident2, dist12, n_o_1, n_o_2, tol_dist, c_1_2)
c-----------------------------------------------------------------

      implicit none

      integer*4
     $  i, j, n_o_1, n_o_2, ident1(*), ident2(*)

      real*4
     $  coo_1(2,*), coo_2(2,*), offset_x, offset_y, c_1_2(2,*),
     $  tol_dist, dist12(*)

      real*4
     $  d_min, dist, dx, dy

c     identify objects in cat 1 and 2
c     -------------------------------------
      do i = 1, n_o_1
         ident1(i) = 0
      end do
      do i = 1, n_o_2
         ident2(i) = 0
      end do

      do i = 1,n_o_1
        c_1_2(1,i) = coo_1(1,i)+offset_x
        c_1_2(2,i) = coo_1(2,i)+offset_y
      end do

      do i = 1, n_o_1
         d_min = 2.*tol_dist
         do j = 1, n_o_2
            dy = abs(c_1_2(2,i)-coo_2(2,j))
            if (dy .le. tol_dist) then
               dx = abs(c_1_2(1,i)-coo_2(1,j))
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
