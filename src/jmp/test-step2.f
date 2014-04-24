C -*-compile-command: "cf77 -O6 -o ../bin/LINUX/test-step2 test-step2.f -L ../lib/LINUX -ljmp; /bin/rm test-step2.o"; -*-

      implicit none

      include 'match-mov.h'

      real*8
     $  pi

      parameter
     $  (pi = 3.141592653589793)

      integer*4
     $  i, i1, i2, j, n, n1(maxframes), nf, narg, iargc, n_c,
     $  n_max

      real*4
     $  x1(n_o_max_cat,maxframes), y1(n_o_max_cat,maxframes),
     $  x1_sys1(n_o_max_cat,maxframes), y1_sys1(n_o_max_cat,maxframes),
     $  x2_sys1(n_o_max_cat,maxframes), y2_sys1(n_o_max_cat,maxframes),
     $  separ

      real*8
     $  coeff(20)

      character
     $  arg*100, line*80, frame*100,
     $  infile(maxframes)*100, trans_cat(maxframes)*100

      logical
     $  help, finished

      external trans

c Create a file for later error handling

      open (unit=1, file='test-step2.FAILED', status='unknown')
      write (1, *) ' '
      close (1)

c Get input parameters

      narg = iargc()
      help = (narg .ne. 3)
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

      open (unit=1, file='test-step2.FAILED', status='unknown')
      write (1, *) line(1:j-1)
      close (1)

      if (help) then
         write (6, *) 'Usage: test-step2 image1 image2 image3'
         stop
      end if

      separ = 40.
      n_max = nint(float(n_br_max)*2./3.)
      n_c = 3

c Gets name of first catalog.

      infile(1) = 'Object.planted'
c Reads in fake object catalog.

      open (1, file=infile(1), status='old')
      nf = 1
      n = 1
    1 continue
         read (1,*,end=2,err=1) x1(n,nf), y1(n,nf)
         n = n+1
         if (n .gt. n_o_max_cat) then
            write (6,*) 'increase n_o_max_cat in match-mov.h'
            stop
         end if
      goto 1
    2 continue
      n1(nf) = n-1
      close (1)

      do nf = 1, 3
         do j = 1, 80
            frame(j:j) = char(0)
         end do
         call getarg (nf, frame)
         call read_file_name (frame, i1, i2, finished, 80)
         if (finished) stop
         trans_cat(nf) = frame(i1:i2)//'.trans.jmp'

c Reads in transform.

         open (1, file=trans_cat(nf), status='unknown')
         read (1, *) (coeff(i), i=1,2*n_c)
         close (1)

c Apply transform to every body.

         call trans_xy (x1(1,1), y1(1,1), n1(1), x1_sys1(1,nf),
     $     y1_sys1(1,nf), coeff, n_c, trans)

c Reads in other transform.

         open (1, file='fk'//trans_cat(nf), status='unknown')
         read (1, *) (coeff(i), i=1,2*n_c)
         close (1)

c Apply transform to every body.

         call trans_xy (x1(1,1), y1(1,1), n1(1), x2_sys1(1,nf),
     $     y2_sys1(1,nf), coeff, n_c, trans)

      end do

c Writes output files

      open (1, file='check-'//trans_cat(1), status='unknown')
      do i = 1, n1(1)
         write (1, '(9(f8.2))') (x1_sys1(i,nf), x2_sys1(i,nf),
     $     x1_sys1(i,nf)-x2_sys1(i,nf), nf=1,3)
         write (1, '(9(f8.2))') (y1_sys1(i,nf), y2_sys1(i,nf),
     $     y1_sys1(i,nf)-y2_sys1(i,nf), nf=1,3)
      end do
      close (1)

c Apparently things went right

      open (unit=1, file='test-step2.OK', status='unknown')
      write (1, *) ' '
      close (1)

 2001 format (a, T40, 2i10)
 2016 format (4f8.2, f13.2, f9.1, f10.2, f6.2)
      stop

      end
