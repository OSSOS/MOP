C -*-compile-command: "cf77 -O6 -o ../bin/LINUX/step2ajmps step2ajmps.f -L ../lib/LINUX -ljmp; /bin/rm step2ajmps.o"; -*-

      implicit none

      include 'match-mov.h'

      real*8
     $  pi

      parameter
     $  (pi = 3.141592653589793)

      integer*4
     $  i, i1, i2, j, n, n1(maxframes), n1_br(maxframes), nf, naxis1,
     $  naxis2, narg, iargc, indx1(n_br_max,maxframes), n_c,
     $  tr1_i(4,n_o_max_cat), k, tr2_i(4,n_o_max_cat), n1_tr, n2_tr,
     $  indx(n_br_max), n_best, n_match(n_br_max,n_br_max),
     $  best_m(3,n_br_max), n_minb, n_minc, n_max,
     $  indxt1(n_o_max_cat), indxt2(n_o_max_cat)

c tr?_i contains indices for the triangles:
c tr?_i(1,*) : index of star opposite of side a
c tr?_i(2,*) : index of star opposite of side b
c tr?_i(3,*) : index of star opposite of side c
c tr?_i(4,*) : index of matching triangle

      real*4
     $  x1(n_o_max_cat,maxframes), y1(n_o_max_cat,maxframes),
     $  x1_sys1(n_o_max_cat,maxframes), y1_sys1(n_o_max_cat,maxframes),
     $  flux1(n_o_max_cat,maxframes), inten1(n_o_max_cat,maxframes),
     $  size1(n_o_max_cat,maxframes), el1(n_o_max_cat,maxframes), isol,
     $  separ, inten_max, fwhm(maxframes), tol_br_dist, tol_dist,
     $  max_dist, radius, scale, tmpr(n_o_max_cat),
     $  tr1_r(3,n_o_max_cat), tr2_r(3,n_o_max_cat),
     $  x1_br(n_br_max,maxframes), y1_br(n_br_max,maxframes),
     $  x(n_br_max), y(n_br_max), dx_m, dy_m, dx_s, dy_s, mopvers

      real*8
     $  x1_f(n_br_max), y1_f(n_br_max), x2_f(n_br_max), y2_f(n_br_max),
     $  coeff(20), sig(n_br_max)

c tr?_r contains the length and length ratios defining the triangles:
c tr?_r(1,*) : length of longest side a
c tr?_r(2,*) : b/a (0 < b < 1)
c tr?_r(3,*) : c/a (0 < c < b)

      character
     $  arg*100, line*80, frame*100, infile(maxframes)*100,
     $  header(7)*80, trans_cat(maxframes)*100

      logical
     $  help, finished, ok(n_o_max_cat), alone(n_o_max_cat)

      external iargc, trans

c Create a file for later error handling

      open (unit=1, file='step2ajmps.FAILED', status='unknown')
      write (1, *) ' '
      close (1)

c Get input parameters

      narg = iargc()
      help = (narg .ne. 3) .and. (narg .ne. 2)
      i = 1
 90   continue
      if (i .le. narg) then
         call getarg (i, arg)
         if ((arg(1:2) .eq. '-h') .or. (arg(1:2) .eq. '-?')) then
            i = narg + 1
            help = .true.
         end if
         i = i + 1
         goto 90
      end if

      j = 1
      do i = 0, narg
         call getarg (i, arg)
         call read_file_name (arg, i1, i2, finished, 80)
         line(j:j+i2-i1) = arg(i1:i2)
         j = j + i2 - i1 + 1
         line(j:j) = ' '
         j = j + 1
      end do
      write (6, *) line(1:j-1)

      open (unit=1, file='step2ajmps.FAILED', status='unknown')
      write (1, *) line(1:j-1)
      close (1)

      if (help) then
         write (6, *) 'Usage: step2jmp image1 image2 [image3]'
         stop
      end if

      radius = 0.002
      tol_br_dist = 0.3
      separ = 40.
      n_minb = n_br_max/2
      n_minc = n_br_max/4
      n_max = nint(float(n_br_max)*2./3.)

c Gets name of first catalog.

      nf = 1

      do j = 1, 80
         frame(j:j) = char(0)
      end do
      call getarg (1, frame)
      call read_file_name (frame, i1, i2, finished, 80)
      if (finished) stop
      infile(1) = frame(i1:i2)//'.obj.jmp'
      trans_cat(1) = frame(i1:i2)//'.trans.jmp'

c First frame is reference frame. So transform is identity.

      open (1, file=trans_cat(1), status='unknown')
      write (1, '(''0. 1. 0. 0. 0. 1.'')')
      close (1)

c Reads in first catalog.

      open (1, file=infile(1), status='old')
      do i = 1, 7
         read (1, '(a)') header(i)
      end do
      read (header(2)(3:), *) mopvers
      i = i2 - i1 + 9
      call check_version (mopvers, infile(1), i, j)
      read (header(4)(33:), *) fwhm(nf)
      read (header(6)(33:), *) naxis1, naxis2
      isol = 2.0*fwhm(nf)
c      isol = 20.
      tol_dist = fwhm(nf)/4.0

      n = 1
    1 continue
         read (1,*,end=2,err=1) x1(n,nf), y1(n,nf), flux1(n,nf),
     $     size1(n,nf), inten1(n,nf), el1(n,nf)
         x1_sys1(n,nf) = x1(n,nf)
         y1_sys1(n,nf) = y1(n,nf)
         if (inten1(n,nf) .gt. inten_max) inten_max = inten1(n,nf)
         n = n+1

         if (n .gt. n_o_max_cat) then
            write (6,*) 'increase n_o_max_cat in match-mov.h'
            stop
         end if
      goto 1
    2 continue
      n1(nf) = n-1
      close (1)
      inten_max = 0.8*inten_max
      max_dist = amin0(naxis1, naxis2)/4.

c Select bright, isolated stars.

      call select_bright (x1(1,nf), y1(1,nf), inten1(1,nf), n1(nf),
     $  x, y, indx, j, inten_max, isol, separ, n_minb, n_max, ok,
     $  alone, tmpr, indxt1, indxt2)
c      write (6, *) j, ' bright stars.'
c      do i = 1, j
c         write (22+10*nf, *) x(i), y(i)
c      end do

c Select stars at center

      call select_center (x, y, indx, j, x1_br(1,nf), y1_br(1,nf),
     $  indx1(1,nf), n1_br(nf), n_minc, naxis1, naxis2)
c      write (6, *) n1_br(nf), ' bright stars at center.'
c      do i = 1, n1_br(nf)
c         write (23+10*nf, *) x1_br(i,nf), y1_br(i,nf)
c      end do

c Built triangles of bright stars.

      call build_triangles (x1_br(1,nf), y1_br(1,nf), n1_br(nf),
     $  tr1_r, tr1_i, n1_tr, max_dist)
c      write (6, *) n1_tr, ' triangles.'

c Gets name of other catalogs.

   10 continue
      if (nf .ge. narg) goto 100
      nf = nf + 1
      if (nf .gt. maxframes) then
         write(6,2001) 'nf .gt. maxframes',nf,maxframes
         write(6,*) 'increase maxframes in match-mov.h'
         stop
      end if

      do j = 1, 80
         frame(j:j) = char(0)
      end do
      call getarg (nf, frame)
      call read_file_name (frame, i1, i2, finished, 80)
      if (finished) stop
      infile(nf) = frame(i1:i2)//'.obj.jmp'
      trans_cat(nf) = frame(i1:i2)//'.trans.jmp'

c Reads in other catalog.

      open (1, file=infile(nf), status='old')
      do i = 1, 7
         read (1, '(a)') header(i)
      end do
      read (header(2)(3:), *) mopvers
      i = i2 - i1 + 9
      call check_version (mopvers, infile(nf), i, j)
      read (header(4)(33:), *) fwhm(nf)

      n = 1
 21   continue
         read(1,*,end=22,err=21) x1(n,nf), y1(n,nf), flux1(n,nf),
     $     size1(n,nf), inten1(n,nf), el1(n,nf)
         n = n+1

         if(n.gt.n_o_max_cat) then
            write(6,*) 'increase n_o_max_cat in match-mov.h'
            stop
         end if
      goto 21
 22   continue
      n1(nf) = n-1
      close (1)

c Select bright, isolated stars.

      call select_bright (x1(1,nf), y1(1,nf), inten1(1,nf), n1(nf),
     $  x, y, indx, j, inten_max, isol, separ, n_minb, n_max, ok,
     $  alone, tmpr, indxt1, indxt2)
c      write (6, *) j, ' bright stars.'
c      do i = 1, j
c         write (22+10*nf, *) x(i), y(i)
c      end do

c Select stars at center

      call select_center (x, y, indx, j, x1_br(1,nf), y1_br(1,nf),
     $  indx1(1,nf), n1_br(nf), n_minc, naxis1, naxis2)
c      write (6, *) n1_br(nf), ' bright stars at center.'
c      do i = 1, n1_br(nf)
c         write (23+10*nf, *) x1_br(i,nf), y1_br(i,nf)
c      end do

c Built triangles of bright stars.

      call build_triangles (x1_br(1,nf), y1_br(1,nf), n1_br(nf),
     $  tr2_r, tr2_i, n2_tr, max_dist)
c      write (6, *) n2_tr, ' triangles.'

c Evaluates the matching count for each pair of bright stars.

      scale = 1.
      call match_triangles (tr1_i, tr1_r, n1_tr, tr2_i, tr2_r, n2_tr,
     $  n_match, radius, scale, j)
c      write (6, *) j, ' matched triangles.'

c Figure out who got more matching.

      call best_match (n_match, n1_br(1), n1_br(nf), best_m, n_best)
c      write (6, *) n_best, ' "well" matched stars.'
c      do i = 1, n_best
c         write (21+10*nf, *) x1_br(best_m(1,i),1), y1_br(best_m(2,i),1),
c     $     x1_br(best_m(1,i),nf), y1_br(best_m(2,i),nf), best_m(3,i)
c      end do

c Fit the two sets of stars.

      do i = 1, n_best
         x1_f(i) = x1_br(best_m(1,i),1)
         y1_f(i) = y1_br(best_m(1,i),1)
         x(i) = x1_br(best_m(2,i),nf)
         y(i) = y1_br(best_m(2,i),nf)
         x2_f(i) = x(i)
         y2_f(i) = y(i)
         sig(i) = 1.
      end do

      j = 0
 200  continue
      if (n_best .lt. 2) then
         write (6, *) 'ERROR: not enough matched stars.'
         stop
      else if (n_best .lt. 6) then
         n_c = 1
      else
         n_c = 3
      end if

      call fit_xy (x1_f, y1_f, x2_f, y2_f, sig, n_best, coeff, n_c,
     $  trans)
c      write (6, *) (coeff(i), i=1,2*n_c)

c Apply the transform to bright stars.

      call trans_xy (x, y, n_best, x1_sys1(1,nf), y1_sys1(1,nf), coeff,
     $  n_c, trans)

c Check for correctness of correction.

      dx_m = 0.
      dy_m = 0.
      dx_s = 0.
      dy_s = 0.
      do i = 1, n_best
         x1_sys1(i,nf) = x1_f(i) - x1_sys1(i,nf)
         y1_sys1(i,nf) = y1_f(i) - y1_sys1(i,nf)
         dx_m = dx_m + x1_sys1(i,nf)
         dy_m = dy_m + y1_sys1(i,nf)
         dx_s = dx_s + x1_sys1(i,nf)**2
         dy_s = dy_s + y1_sys1(i,nf)**2
      end do
      dx_s = sqrt(amax1(0., (dx_s - dx_m**2)/float(n_best)))
      dy_s = sqrt(amax1(0., (dy_s - dy_m**2)/float(n_best)))
      dx_m = dx_m/float(n_best)
      dy_m = dy_m/float(n_best)
c      write (6, *) dx_m, dx_s, dy_m, dy_s

      n = n_best
      do i = 1, n_best
         if ((abs(x1_sys1(i,nf)) .gt. 3.0*dx_s)
     $     .or. (abs(y1_sys1(i,nf)) .gt. 3.0*dy_s)) then
            do k = i+1, n
               x1_f(k-1) = x1_f(k)
               y1_f(k-1) = y1_f(k)
               x(k-1) = x(k)
               y(k-1) = y(k)
               x2_f(k-1) = x2_f(k)
               y2_f(k-1) = y2_f(k)
            end do
            n = n - 1
         end if
      end do
      if ((n .lt. n_best) .and. (j .lt. 5)) then
         n_best = n
         j = j + 1
         goto 200
      end if

c Stores the transform

      open (unit=1, file=trans_cat(nf), status='unknown')
      if (n_c .eq. 3) then
         write (1, '(f8.2,1x,f10.8,1x,e14.8,f8.2,1x,e14.8,1x,f10.8)')
     $     (coeff(i), i=1,2*n_c)
      else
         write (1, '(f8.2,1x,f10.8,1x,e14.8,f8.2,1x,e14.8,1x,f10.8)')
     $     coeff(1), 1., 0., coeff(2), 0., 1.
      end if
      close (1)

c End of catalog loop.

      goto 10

 100  continue

c Apparently things went right

      open (unit=1, file='step2ajmps.OK', status='unknown')
      write (1, *) ' '
      close (1)

 2001 format (a, T40, 2i10)
      stop

      end
