C -*-compile-command: "ifort -assume 2underscores -save -assume byterecl -o ../../bin/LINUX/checktrans checktrans.f -L ../../lib/LINUX -ljmp"; -*-

c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
c This program checks that transforms between frames are good.
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
c
c J-M. Petit  Observatoire de Besancon
c Version 1 : May 2006
c
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

      implicit none

      include 'match-mov.h'

      integer*4
     $  if, nf, n, n1(maxframes), i, n_c, j, n_nf, i1, i2, narg, iargc,
     $  i3

      real*4
     $  x1(n_o_max_cat,maxframes), y1(n_o_max_cat,maxframes),
     $  x1_sys1(n_o_max_cat,maxframes), y1_sys1(n_o_max_cat,maxframes)

      real*8
     $  coeff(20)

      character
     $  filelist*100, infile(maxframes)*100, trans_cat(maxframes)*100,
     $  line*100, prefix*20, arg*80, suf*3

      logical
     $  found, finished, help, has_p

      external trans

c Create a file for later error handling

      open (unit=1, file='checktrans.FAILED', status='unknown')
      write (1, *) ' '
      close (1)

c Get input parameters

      do i = 1, 20
         prefix(i:i) = char(0)
      end do

      narg = iargc()
      i = 1
      help = (narg .gt. 2)

 90   continue
      if (i .le. narg) then
         call getarg (i, arg)
         if (arg(1:2) .eq. '-p') then
            i = i + 1
            call getarg (i, prefix)
         else if ((arg(1:2) .eq. '-h') .or. (arg(1:2) .eq. '-?')) then
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

      open (unit=1, file='checktrans.FAILED', status='unknown')
      write (1, *) line(1:j-1)
      close (1)

      if (help) then
         write (6, *)
     $     'Usage: checktrans [-p <prefix>] [-h|-?]'
         write (6, *) 'where:'
         write (6, *) '-p <prefix>: optional prefix to file name'
         stop
      end if

      call read_file_name (prefix, i1, i2, finished, 20)
      has_p = .not. finished
      if (has_p) then
         suf = 'jmp'
      else
         suf = 'psf'
      end if

c reads in file names

      open (unit=1, file='proc-these-files', status='old')
      read (1, *)
      read (1, *)
      nf = 1
 1000 continue
         read (1, '(a19)', end=1050) infile(nf)
         i3 = 20
 1005    continue
         if (infile(nf)(i3-1:i3-1) .eq. ' ') then
            i3 = i3 - 1
            if (i3 .ge. 2) goto 1005
         end if
         if (infile(nf)(i3-1:i3-1) .eq. char(9)) then
            i3 = i3 - 1
            if (i3 .ge. 2) goto 1005
         end if
         i = 1
 1010    continue
         if (infile(nf)(i:i) .eq. ' ') then
            i = i + 1
            if (i .lt. i3) goto 1010
         end if
         infile(nf) = infile(nf)(i:)
         if (nf .eq. 1) then
            infile(nf)(i3-i+1:) = '.bright.'//suf
         else
            infile(nf)(i3-i+1:) = '.obj.'//suf
         end if
         trans_cat(nf) = infile(nf)(1:i3-i)//'.trans.jmp'
         if (has_p) then
            infile(nf) = prefix(i1:i2)//infile(nf)
            trans_cat(nf) = prefix(i1:i2)//trans_cat(nf)
         end if
         nf = nf + 1
         goto 1000
 1050 continue
      close (1)
      nf = nf - 1

      n_c = 3

c reads in bright stars from first catalog

      if = 1
      open (unit=1, file=infile(if), status='old')
      do i = 1, 7
         read (1, *)
      end do

      n = 1
 2000 continue
         read (1, *, end=2050, err=2000) x1(n,if), y1(n,if)
         x1_sys1(n,if) = x1(n,if)
         y1_sys1(n,if) = y1(n,if)
         n = n + 1
         if (n .gt. n_o_max_cat) then
            write (6,*) 'increase n_o_max_cat in match-mov.h'
            stop
         end if
         goto 2000
 2050 continue
      close (1)
      n1(if) = n - 1

c Now loop on other frames

      do if = 2, nf
         open (unit=1, file=infile(if), status='old')
         do i = 1, 7
            read (1, *)
         end do
         n = 1
 2100    continue
            read (1, *, end=2150, err=2100) x1(n,if), y1(n,if)
            n = n + 1
            if (n .gt. n_o_max_cat) then
               write (6,*) 'increase n_o_max_cat in match-mov.h'
               stop
            end if
            goto 2100
 2150    continue
         close (1)
         n1(if) = n - 1

         open (unit=1, file=trans_cat(if), status='unknown')
         read (1, *) (coeff(i), i=1,2*n_c)
         close (1)

         call trans_xy (x1(1,if), y1(1,if), n1(if), x1_sys1(1,if),
     $     y1_sys1(1,if), coeff, n_c, trans)

c Now, see if we can find a corresponding object in the new catalog for
c each bright star from the first frame

         n_nf = 0
         do i = 1, n1(1)
            do j = 1, n1(if)
               found = ((x1_sys1(i,1)-x1_sys1(j,if))**2
     $           + (y1_sys1(i,1)-y1_sys1(j,if))**2) .lt. 16.
               if (found) goto 2200
            end do
 2200       continue
            if (.not. found) then
               n_nf = n_nf + 1
c               write (20, *) n_nf, ' Point: ', i, x1(i,1), y1(i,1),
c     $           ' not found in ',infile(if)
            end if
         end do
         if ((n_nf .gt. n1(1)/4) .and. 
     $        (n1(1) - n_nf .lt. 20)) goto 3000

      end do
      goto 4000

 3000 continue
      line = 'BAD_TRANS'//prefix
      open (unit=1, file=line, status='unknown')
      write (1, *) n_nf, ' points not found in ', infile(if)
      close (1)

 4000 continue

c Apparently things went right

      open (unit=1, file='checktrans.OK', status='unknown')
      write (1, *) ' '
      close (1)

      end
