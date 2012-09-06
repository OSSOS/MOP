C -*-compile-command: "make_lib"; -*-

      subroutine segment (image, coeff, im_sky, mask, y_offset,
     $  size_obj, moments, sfl, max_int, n_obj, nx, ny, g_sky,
     $  n_obj_max, max_el, echelle, echelle_c, cr_reject)
c     ----------------------------------------------------------

      integer*4
     $  nx, ny, n_obj, size_obj(*), size_max,
     $  y_offset, n_obj_max

      parameter
     $  (size_max = 40000)

      real*4
     $  image(nx,ny), coeff(nx,ny), mask(nx,ny), moments(7,*), sky,
     $  max_int(*), rn, kk, tmp, angle, ca, sa, pi, pi2, rn1, rn2,
     $  max_el, im_sky(nx,ny), tmp11, tmp21, tmp2, cm, cur_val, cmm,
     $  l_sky, g_sky, echelle, echelle_c, s1, s2, sfl(*)

      integer*4
     $  first_emp, current, i, j, n, im, jm, n1, n2, liste(size_max),
     $  ic, jc, iv, jv, i_incr(4), j_incr(4), nt, ii, jmm, imm, nb,
     $  back_size, jvm, ivm, jvm1, ivm1

      logical
     $  cr_reject

      parameter
     $  (pi = 3.141592653, pi2 = pi**2)

      data
     $  (i_incr(i), i=1,4) / 0, 0, +1, -1/,
     $  (j_incr(i), i=1,4) /+1, -1, 0, 0/

      back_size = nint(4.0*echelle_c)

c Detect objects

      n = 0
      rn = 0.
      do i = 1, ny
         do j = 1, nx
            if (mask(j,i) .eq. -2.) then

c Pass 1.
               n = n + 1
               rn = rn + 1.
               if (n .gt. n_obj_max) then
                  write (6,*) 'n .gt. n_obj_max', n
                  stop
               end if
               mask(j,i) = rn
               first_emp = 1
               liste(first_emp) = i*nx + j - 1
               first_emp = first_emp + 1
               cm = coeff(j,i)
               jm = j
               im = i
               nt = 1

               current = 1
 1000          continue
               ic = liste(current)/nx
               jc = liste(current) - ic*nx + 1
c
c Look in all directions.
c
               do k = 1, 4
                  iv = ic + i_incr(k)
                  jv = jc + j_incr(k)
c
c Neighbour is in the array.
c
                  if ((iv .ge. 1) .and. (iv .le. ny)
     $              .and. (jv .ge. 1) .and. (jv .le. nx)) then
c
c Neighbour is good and not already marked.
c
                     if (mask(jv,iv) .le. -1.) then
                        if (coeff(jv,iv) .gt. cm) then
                           cm = coeff(jv,iv)
                           jm = jv
                           im = iv
                        end if
                        mask(jv,iv) = rn
                        liste(first_emp) = iv*nx + jv - 1
                        if (first_emp .ge. size_max) then
                           do ii = current + 1, first_emp
                              liste(ii-current) = liste(ii)
                           end do
                           first_emp = first_emp - current
                           current = 0
                        end if
                        first_emp = first_emp + 1
                        nt = nt + 1
                     end if
                  end if
               end do
               current = current + 1
c
c If not finished.
c
               if (current .lt. first_emp) goto 1000
c
c If too small, jump to next object.
c
               if (nt .lt. 4) then
                  size_obj(n) = 0
                  goto 5000
               end if
c
c Estimate local background. Fall back to global background if no
c point retained.
c 
               nb = 0
               l_sky = 0.
               tmp2 = image(jm,im)
               do ic = max(1, im-back_size), min(ny, im+back_size)
                  do jc = max(1, jm-back_size), min(nx, jm+back_size)
                     if ((mask(jc,ic) .eq. 0.)
     $                 .and. (image(jc,ic) .lt. tmp2))then
                        l_sky = l_sky + image(jc,ic)
                        nb = nb + 1
                     end if
                  end do
               end do
               if (nb .gt. 0) then
                  sky = l_sky/float(nb)
               else
                  sky = g_sky
               end if
c Pass 2.
               rn2 = rn + 0.1
               mask(jm,im) = rn2
               first_emp = 1
               liste(first_emp) = im*nx + jm - 1
               first_emp = first_emp + 1
               size_obj(n) = 1
c
c This is 0 because we change the origin to here. Hence x = y = 0.
c
               moments(1,n) = 0.
               moments(2,n) = 0.
               moments(3,n) = 0.
               moments(4,n) = 0.
               moments(5,n) = 0.
               moments(6,n) = image(jm,im)-sky
               if (moments(6,n) .gt. 0.) then
                  s1 = 1.
               else
                  moments(6,n) = 0.
                  s1 = 0.
               end if
               max_int(n) = image(jm,im)
               jvm = jm
               ivm = im
               jmm = 0
               imm = 0
               cmm = 0.

               current = 1
 2000          continue
               ic = liste(current)/nx
               jc = liste(current) - ic*nx + 1
               cur_val = coeff(jc,ic)
c
c Look in all the directions.
c
               do k = 1, 4
                  iv = ic + i_incr(k)
                  jv = jc + j_incr(k)
c
c Neighbour is in the array.
c
                  if ((iv .ge. 1) .and. (iv .le. ny)
     $              .and. (jv .ge. 1) .and. (jv .le. nx)) then
c
c Neighbour is good and not already marked.
c
                     if (mask(jv,iv) .eq. rn) then
                        if (coeff(jv,iv) .lt. cur_val) then
                           mask(jv,iv) = rn2
                           liste(first_emp) = iv*nx + jv - 1
                           if (first_emp .ge. size_max) then
                              do ii = current + 1, first_emp
                                 liste(ii-current) = liste(ii)
                              end do
                              first_emp = first_emp - current
                              current = 0
                           end if
                           first_emp = first_emp + 1
                           size_obj(n) = size_obj(n) + 1
                           kk = image(jv,iv)-sky
                           if (kk .gt. 0.) then
                              s1 = s1 + 1.
                           else
                              kk = 0.
                           end if
                           moments(1,n) = moments(1,n)
     $                       + float(jv-jm)*kk
                           moments(2,n) = moments(2,n)
     $                       + float(iv-im)*kk
                           moments(3,n) = moments(3,n)
     $                       + float((jv-jm)*(jv-jm))*kk
                           moments(4,n) = moments(4,n)
     $                       + float((iv-im)*(iv-im))*kk
                           moments(5,n) = moments(5,n)
     $                       + float((iv-im)*(jv-jm))*kk
                           moments(6,n) = moments(6,n) + kk
                           if (image(jv,iv) .gt. max_int(n)) then
                              max_int(n) = image(jv,iv)
                              jvm = jv
                              ivm = iv
                           end if
                        else if (coeff(jv,iv) .gt. cmm) then
                           jmm = jv
                           imm = iv
                           cmm = coeff(jv,iv)
                        end if
                     end if
                  end if
               end do
               current = current + 1
c
c If not finished (pass 2).
c
               if (current .lt. first_emp) goto 2000

c Pass 3.
               n1 = 0
               if (size_obj(n) .lt. nt) then
                  n1 = n
                  rn1 = rn
                  n = n + 1
                  rn = rn + 1.
                  jvm1 = jvm
                  ivm1 = ivm
                  s2 = s1
                  if (n1 .gt. n_obj_max) then
                     write (6,*) 'n1 .gt. n_obj_max', n1
                     stop
                  end if
                  liste(1) = imm*nx + jmm - 1
                  current = 1
                  first_emp = 2
                  n2 = 0
                  mask(jmm,imm) = rn
                  size_obj(n) = 1
                  kk = image(jmm,imm)-sky
                  if (kk .gt. 0.) then
                     s1 = 1.
                  else
                     kk = 0.
                     s1 = 0.
                  end if
                  moments(1,n) = float(jmm-jm)*kk
                  moments(2,n) = float(imm-im)*kk
                  moments(3,n) = float((jmm-jm)*(jmm-jm))*kk
                  moments(4,n) = float((imm-im)*(imm-im))*kk
                  moments(5,n) = float((imm-im)*(jmm-jm))*kk
                  moments(6,n) = kk
                  max_int(n) = 0.
                  jvm = jmm
                  ivm = imm

 3000             continue
                  ic = liste(current)/nx
                  jc = liste(current) - ic*nx + 1
                  cur_val = coeff(jc,ic)
c
c Look in all the directions.
c
                  do k = 1, 4
                     iv = ic + i_incr(k)
                     jv = jc + j_incr(k)
c
c Neighbour is in the array.
c
                     if ((iv .ge. 1) .and. (iv .le. ny)
     $                 .and. (jv .ge. 1) .and. (jv .le. nx)) then
c
c Neighbour is good and not already marked.
c
                        if (mask(jv,iv) .eq. rn1) then
                           mask(jv,iv) = rn
                           liste(first_emp) = iv*nx + jv - 1
                           if (first_emp .ge. size_max) then
                              do ii = current + 1, first_emp
                                 liste(ii-current) = liste(ii)
                              end do
                              first_emp = first_emp - current
                              current = 0
                           end if
                           first_emp = first_emp + 1
                           size_obj(n) = size_obj(n) + 1
                           kk = image(jv,iv)-sky
                           if (kk .gt. 0.) then
                              s1 = s1 + 1.
                           else
                              kk = 0.
                           end if
                           moments(1,n) = moments(1,n)
     $                       + float(jv-jm)*kk
                           moments(2,n) = moments(2,n)
     $                       + float(iv-im)*kk
                           moments(3,n) = moments(3,n)
     $                       + float((jv-jm)*(jv-jm))*kk
                           moments(4,n) = moments(4,n)
     $                       + float((iv-im)*(iv-im))*kk
                           moments(5,n) = moments(5,n)
     $                       + float((iv-im)*(jv-jm))*kk
                           moments(6,n) = moments(6,n) + kk
                           if (image(jv,iv) .gt. max_int(n)) then
                              max_int(n) = image(jv,iv)
                              jvm = jv
                              ivm = iv
                           end if

                        else if ((mask(jv,iv) .eq. rn2)
     $                    .and. (coeff(jv,iv) .lt. cur_val)) then

c Neighbour is good and part of the other piece.

                           n2 = n2 + 1
                           mask(jv,iv) = rn
                           liste(first_emp) = iv*nx + jv - 1
                           if (first_emp .ge. size_max) then
                              do ii = current + 1, first_emp
                                 liste(ii-current) = liste(ii)
                              end do
                              first_emp = first_emp - current
                              current = 0
                           end if
                           first_emp = first_emp + 1
                           size_obj(n) = size_obj(n) + 1
                           kk = 0.5*(image(jv,iv)-sky)
                           if (kk .gt. 0.) then
                              s1 = s1 + .5
                              s2 = s2 - .5
                           else
                              kk = 0.
                           end if
                           moments(1,n) = moments(1,n)
     $                       + float(jv-jm)*kk
                           moments(2,n) = moments(2,n)
     $                       + float(iv-im)*kk
                           moments(3,n) = moments(3,n)
     $                       + float((jv-jm)*(jv-jm))*kk
                           moments(4,n) = moments(4,n)
     $                       + float((iv-im)*(iv-im))*kk
                           moments(5,n) = moments(5,n)
     $                       + float((iv-im)*(jv-jm))*kk
                           moments(6,n) = moments(6,n) + kk
                           if (image(jv,iv) .gt. max_int(n)) then
                              max_int(n) = image(jv,iv)
                              jvm = jv
                              ivm = iv
                           end if
                           moments(1,n1) = moments(1,n1)
     $                       - float(jv-jm)*kk
                           moments(2,n1) = moments(2,n1)
     $                       - float(iv-im)*kk
                           moments(3,n1) = moments(3,n1)
     $                       - float((jv-jm)*(jv-jm))*kk
                           moments(4,n1) = moments(4,n1)
     $                       - float((iv-im)*(iv-im))*kk
                           moments(5,n1) = moments(5,n1)
     $                       - float((iv-im)*(jv-jm))*kk
                           moments(6,n1) = moments(6,n1) - kk
                        end if
                     end if
                  end do
                  current = current + 1
c
c If not finished (pass 3).
c
                  if (current .lt. first_emp) goto 3000
                  size_obj(n) = size_obj(n) - (n2+1)/2
                  size_obj(n1) = size_obj(n1) - n2/2
c
c Check if really 2 different objects.
c
                  if ((moments(6,n) .gt. 0.)
     $              .and. (moments(6,n1) .gt. 0.)) then
                     tmp = (moments(1,n)/moments(6,n)
     $                 - moments(1,n1)/moments(6,n1))**2
     $                 + (moments(2,n)/moments(6,n)
     $                 - moments(2,n1)/moments(6,n1))**2
                     if (4.*tmp .lt. echelle**2) then
c                        write (60, *) n, moments(1,n)/moments(6,n)
c     $                    + float(jm), moments(2,n)/moments(6,n)
c     $                    + float(im) + y_offset, moments(6,n),
c     $                    size_obj(n), max_int(n), s1
c                        write (60, *) n1, moments(1,n1)/moments(6,n1)
c     $                    + float(jm), moments(2,n1)/moments(6,n1)
c     $                    + float(im) + y_offset, moments(6,n1),
c     $                    size_obj(n1), max_int(n1), s2
                        do k = 1, 6
                           moments(k,n) = moments(k,n) + moments(k,n1)
                        end do
                        size_obj(n) = size_obj(n) + size_obj(n1)
                        s1 = s1 + s2
                        if (max_int(n1) .gt. max_int(n)) then
                           max_int(n) = max_int(n1)
                           jvm = jvm1
                           ivm = ivm1
                        end if
c                        write (60, *) n, moments(1,n)/moments(6,n)
c     $                    + float(jm), moments(2,n)/moments(6,n)
c     $                    + float(im) + y_offset, moments(6,n),
c     $                    size_obj(n), max_int(n), s1
                        size_obj(n1) = 0
                        n1 = 0
                     end if
                  end if
               end if
c
c Finished. First piece.
c
               if ((moments(6,n) .le. 0.)
     $           .or. (size_obj(n) .lt. 4)) then
c                  write (50, *) 'Obj. 1, small.', jm, im,
c     $              moments(6,n), size_obj(n), sky
                  size_obj(n) = 0
               else
                  moments(1,n) = moments(1,n)/moments(6,n)
                  moments(2,n) = moments(2,n)/moments(6,n)
                  moments(3,n) = moments(3,n)/moments(6,n)
     $              - moments(1,n)**2
                  moments(4,n) = moments(4,n)/moments(6,n)
     $              - moments(2,n)**2
                  moments(5,n) = moments(5,n)/moments(6,n)
     $              - moments(1,n)*moments(2,n)
                  moments(1,n) = moments(1,n) + float(jm)
                  moments(2,n) = moments(2,n) + float(im) + y_offset
                  if (moments(3,n)*moments(4,n)*moments(5,n) .eq. 0.)
     $              then
c                     write (50, *) 'Obj. 1, zero.', moments(1,n),
c     $                 moments(2,n), moments(3,n), moments(4,n),
c     $                 moments(5,n)
                     size_obj(n) = 0
                  else
                     angle = 0.5*atan2(2.*moments(5,n),
     $                 moments(3,n)-moments(4,n))
                     ca = cos(angle)
                     sa = sin(angle)
                     tmp = ca**2*moments(3,n) + 2.*ca*sa*moments(5,n)
     $                 + sa**2*moments(4,n)
                     moments(3,n) = sa**2*moments(3,n)
     $                 - 2.*ca*sa*moments(5,n) + ca**2*moments(4,n)
                     moments(4,n) = tmp
                     if (moments(3,n) .eq. 0.) then
                        moments(5,n) = 1e31
                     else
                        moments(5,n) = sqrt(moments(4,n)/moments(3,n))
                     end if

c Remember that moments(5,n) is always >= 1.

                     if (moments(5,n) .gt. max_el) then
c                        write (50, *) 'Obj. 1, elongated.',
c     $                    moments(1,n), moments(2,n), moments(5,n)
                        size_obj(n) = 0
                     else

c These are no longer the 2nd order momenta.

c                        aire = sqrt(16.*pi2*moments(3,n)*moments(4,n))
c                        moments(3,n) = sqrt(aire/(moments(5,n)*pi))
                        moments(3,n) = 2.*sqrt(moments(3,n))
                        moments(4,n) = moments(3,n)*moments(5,n)

c Check for cosmic rays. First see if computation of the axis was ok.

                        if ((moments(3,n) .ge. moments(4,n))
     $                    .or. (moments(3,n) .le. moments(4,n)))
     $                    then

c If yes, delete small objects.

c                           if (moments(3,n) .lt. echelle/2.) then
                           if (moments(3,n) .lt. 1.) then
c                              write (50, *) 'Obj. 1, cr small.',
c     $                          moments(1,n), moments(2,n), moments(3,n)
                              size_obj(n) = 0
                           else

c Get rid of objects with very pronounced central peak.

                              tmp2 = 0.
                              nt = 0
                              do k = 1, 4
                                 iv = ivm + i_incr(k)*nint(echelle/2.)
                                 jv = jvm + j_incr(k)*nint(echelle/2.)
                                 if ((iv .ge. 1) .and. (iv .le. ny)
     $                             .and. (jv .ge. 1) .and. (jv .le. nx))
     $                             then
                                    if (image(jv,iv)-sky .gt. 0.) then
                                       tmp2 = tmp2 + image(jv,iv)-sky
                                       nt = nt + 1
                                    end if
                                 end if
                              end do
                              tmp11 = max_int(n) - sky
                              if (nt .gt. 0) then
                                 tmp2 = tmp2/float(nt)
                              else
                                 tmp2 = tmp11*1.e-30
                              end if
                              if (cr_reject
     $                          .and. (((tmp2 .gt. sqrt(sky))
     $                          .and. (tmp11 .gt. 4.*tmp2))
     $                          .or. (tmp11 .gt. 10.*tmp2))) then
c                                 write (50, *) 'Obj. 1, sharp.',
c     $                             moments(1,n), moments(2,n), tmp11,
c     $                             tmp2, sky
                                 size_obj(n) = 0
                              end if
                              moments(4,n) = tmp11/tmp2
                              moments(7,n) = sky
                              moments(6,n) = moments(6,n)/s1
                              sfl(n) = s1
                           end if

c Finally delete the objects for which the computation of the axis went
c wrong.

                        else
c                           write (50, *) 'Obj. 1, undef.', moments(1,n),
c     $                       moments(2,n), moments(3,n), moments(4,n)
                           size_obj(n) = 0
                        end if
                     end if
                  end if
               end if

c Second piece.

               if (n1 .gt. 0) then
                  if ((moments(6,n1) .le. 0.)
     $              .or. (size_obj(n1) .lt. 4)) then
c                     write (50, *) 'Obj. 2, small.', jm, im,
c     $                 moments(6,n1), size_obj(n1), sky
                     size_obj(n1) = 0
                  else
                     moments(1,n1) = moments(1,n1)/moments(6,n1)
                     moments(2,n1) = moments(2,n1)/moments(6,n1)
                     moments(3,n1) = moments(3,n1)/moments(6,n1)
     $                 - moments(1,n1)**2
                     moments(4,n1) = moments(4,n1)/moments(6,n1)
     $                 - moments(2,n1)**2
                     moments(5,n1) = moments(5,n1)/moments(6,n1)
     $                 - moments(1,n1)*moments(2,n1)
                     moments(1,n1) = moments(1,n1) + float(jm)
                     moments(2,n1) = moments(2,n1) + float(im)
     $                 + y_offset
                     if (moments(3,n1)*moments(4,n1)*moments(5,n1)
     $                 .eq. 0.) then
c                        write (50, *) 'Obj. 2, zero.', moments(1,n1),
c     $                    moments(2,n1), moments(5,n1), moments(4,n1),
c     $                    moments(5,n1)
                        size_obj(n1) = 0
                     else
                        angle = 0.5*atan2(2.*moments(5,n1),
     $                    moments(3,n1)-moments(4,n1))
                        ca = cos(angle)
                        sa = sin(angle)
                        tmp = ca**2*moments(3,n1)
     $                    + 2.*ca*sa*moments(5,n1)
     $                    + sa**2*moments(4,n1)
                        moments(3,n1) = sa**2*moments(3,n1)
     $                    - 2.*ca*sa*moments(5,n1)
     $                    + ca**2*moments(4,n1)
                        moments(4,n1) = tmp
                        if (moments(3,n1) .eq. 0.) then
                           moments(5,n1) = 1e31
                        else
                           moments(5,n1) = sqrt(moments(4,n1)
     $                       /moments(3,n1))
                        end if
                        if (moments(5,n1) .gt. max_el) then
c                           write (50, *) 'Obj. 2, elongated.',
c     $                       moments(1,n1), moments(2,n1), moments(5,n1)
                           size_obj(n1) = 0
                        else

c These are no longer the 2nd order momenta.

                           moments(3,n1) = 2.*sqrt(moments(3,n1))
                           moments(4,n1) = moments(3,n1)*moments(5,n1)

c Check for cosmic rays. First see if computation of the axis was ok.

                           if ((moments(3,n1) .ge. moments(4,n1))
     $                       .or. (moments(3,n1) .le. moments(4,n1)))
     $                       then

c If yes, delete small objects.

c                              if (moments(3,n1) .lt. echelle/2.) then
                              if (moments(3,n1) .lt. 1.) then
c                                 write (50, *) 'Obj. 2, cr small.',
c     $                             moments(1,n1), moments(2,n1),
c     $                             moments(3,n1)
                                 size_obj(n1) = 0
                              else

c Get rid of objects with very pronounced central peak.

                                 tmp2 = 0.
                                 nt = 0
                                 do k = 1, 4
                                    iv = ivm1 + i_incr(k)
     $                                *nint(echelle/2.)
                                    jv = jvm1 + j_incr(k)
     $                                *nint(echelle/2.)
                                    if ((iv .ge. 1) .and. (iv .le. ny)
     $                                .and. (jv .ge. 1)
     $                                .and. (jv .le. nx)) then
                                       if (image(jv,iv)-sky .gt. 0.)
     $                                   then
                                          tmp2 = tmp2 + image(jv,iv)
     $                                      -sky
                                          nt = nt + 1
                                       end if
                                    end if
                                 end do
                                 tmp21 = max_int(n1) - sky
                                 if (nt .gt. 0) then
                                    tmp2 = tmp2/float(nt)
                                 else
                                    tmp2 = tmp21*1.e-30
                                 end if
                                 if (cr_reject
     $                             .and. (((tmp2 .gt. sqrt(sky))
     $                             .and. (tmp21 .gt. 4.*tmp2))
     $                             .or. (tmp21 .gt. 10.*tmp2))) then
c                                    write (50, *) 'Obj. 2, sharp.',
c     $                                moments(1,n1), moments(2,n1),
c     $                                tmp21, tmp2, sky
                                    size_obj(n1) = 0
                                 end if
                                 moments(4,n1) = tmp21/tmp2
                                 moments(7,n1) = sky
                                 moments(6,n1) = moments(6,n1)/s2
                                 sfl(n1) = s2
                              end if

c Finally delete the object for which the computation of the axis went
c wrong.

                           else
c                              write (50, *) 'Obj. 2, undef.',
c     $                          moments(1,n1), moments(2,n1),
c     $                          moments(3,n1), moments(4,n1)
                              size_obj(n1) = 0
                           end if
                        end if
                     end if
                  end if
               end if
            end if
 5000       continue
         end do
      end do
      n_obj = n

c     edge problem
c     ------------
      i = 1
      do j = 1, nx
         if(mask(j,i) .gt. 0.) then
            n = nint(mask(j,i))
            size_obj(n) = 0
         end if
      end do
      i = ny
      do j = 1, nx
         if(mask(j,i) .gt. 0.) then
            n = nint(mask(j,i))
            size_obj(n) = 0
         end if
      end do

      return
      end
