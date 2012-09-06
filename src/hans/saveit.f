      SUBROUTINE  saveit(j,usno_star_file)
c
c...Save Results
c
      INCLUDE
     *            'square.inc'
      INTEGER
     *            billion, million, thousand
      PARAMETER
     *           (billion = 1000*1000*1000,
     *            million = 1000*1000,
     *            thousand = 1000)
      INTEGER
     *            j, k, fld, mb, mr

      character*80 usno_star_file
c
 9003 FORMAT (i5, 3i15)
 9004 FORMAT (' NSAV=', i10)
c
  100 IF (nsav.eq.1) THEN
        OPEN (name=usno_star_file,
     *        status='unknown',
     *        unit=2
     *       )
      ENDIF
      k = MOD(ABS(buf(3,j)),BILLION)
      fld = k/MILLION
      k = k - fld*MILLION
      mb = k/THOUSAND
      mr = k - mb*THOUSAND
c      WRITE (2,9003) oldzone,buf(1,j),buf(2,j),buf(3,j)
      write (2, 9001) dble(buf(1,j))/360000.d0,
     $  dble(buf(2,j))/360000.d0-90.d0,
     $  dble(mr)/10., dble(mb)/10., oldzone*million+j
 9001 format(2f15.7,2f10.1,i11)
c     IF (MOD(nsav,1000).eq.0) THEN
c       WRITE (6,9004) nsav
c     ENDIF
      RETURN
c
c...User Exit
c
  200 stop
      END

