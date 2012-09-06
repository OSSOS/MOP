
      SUBROUTINE  whereis
c
c...Load Paths To Catalog Location
c
c	WARNING.  Very Machine Specific!
c
      INCLUDE
     *            'square.inc'
      INTEGER
     *            i
      CHARACTER*80
     *         filename
c
c
 100  continue
      open (unit=1, 
c    *      file='/home/observe/bin/catalog.toc', 
     *      file='/home/scholl/USNO_SOFTWARE/catalog.toc', 
c    *      file='catalog.toc', 
     *      status='old', err=102)
      goto 110
 102  continue
      write (6, '(a,$)') 'Catalog file name: '
      read (5, '(a)') filename
      open (unit=1, file=filename, status='old', err=102)
      goto 110
 110  continue
      DO i=1,NSPDZONE
         call readq (1, nfn(i),fn(i))
         nfn(i) = nfn(i) - 3
      ENDDO
      CLOSE (1)
      RETURN
      END
