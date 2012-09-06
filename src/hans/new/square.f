      subroutine  square(r,d,s_alpha,s_delta,n_usno,usno_star_file)
c
c...Extract A Square From USNO-A
c
      INCLUDE
     *         'square.inc'
      DOUBLE PRECISION
     *         r, d, s_alpha,s_delta
      INTEGER
     *         i, j,n_usno
      character*80 usno_star_file
c
 9001 FORMAT (' Found', i10, ' Entries')
c
  100 CALL whereis
      oldzone = -1
      scale = 67.14D00
      CALL corner(r,d,s_alpha,s_delta)
      nsav = 0
      DO j=1,ndec
        DO i=1,nra
          CALL eatit(rfrst(i),rlast(i),dzone(j),dfrst(j),dlast(j),
     *               usno_star_file)
        ENDDO
      ENDDO
      CLOSE (2)
      n_usno = nsav
c     WRITE (*,9001) nsav

      return
      end
