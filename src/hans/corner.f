      SUBROUTINE  corner(r,d,s_alpha,s_delta)
c
c...Take Nominal And Compute Corners And Other Search Parameters
c
      INCLUDE
     *            'square.inc'
      DOUBLE PRECISION
     *            r, d, rd, r1, r2, d1, d2, cd, zmin, zmax,
     *            s_alpha,s_delta
      INTEGER
     *            i, j, z1, z2
c
 9001 FORMAT (' Illegal Input: R=', f10.6, '  D=', f10.6)
 9002 FORMAT (' Illegal Size=', f10.6)
c
c...Sanity Checks
c
CCC BG  100 IF ((r.lt.0.0D00).or.(r.ge.24.0D00)) THEN
  100 IF ((r.lt.0.0D00).or.(r.ge.360.0D00)) THEN
        WRITE (*,9001) r,d
        stop
      ENDIF
      IF ((d.le.-90.0D00).or.(d.ge.90.0D00)) THEN
        WRITE (*,9001) r,d
        stop
      ENDIF
      IF (s_alpha.le.0.0D00 .or.s_delta.le.0.0D0) THEN
        WRITE (*,9002) s_alpha,s_delta
        stop
      ENDIF
c
c...Compute The Corners And Number Of RA Chunks
c
CCC BG      rd = r*15.0D00
      rd = r
      rcent = rd
      dcent = d
      cd = COS(d/180.d0*3.141592653d0)
      r1 = rd - s_alpha/cd
      r2 = rd + s_alpha/cd
      IF (r1.lt.0.0D00) THEN
        nra = 2
        rfrst(1) = 0.0D00
        rlast(1) = r2
        rfrst(2) = 360.0D00+r1
        rlast(2) = 360.0D00
      ELSEIF (r2.ge.360.0D00) THEN
        nra = 2
        rfrst(1) = 0.0D00
        rlast(1) = r2-360.0D00
        rfrst(2) = r1
        rlast(2) = 360.0D00
      ELSE
        nra = 1
        rfrst(1) = r1
        rlast(1) = r2
      ENDIF
c
c...Compute Dec Corners And Zones
c
      d1 = d - s_delta
      d2 = d + s_delta
      z1 = (d1+90.0D00)/7.5D00
      z2 = (d2+90.0D00)/7.5D00
      ndec = z2+1-z1
      j = 0
      DO i=z1,z2
        zmin =  i*7.5D00 - 90.0D00
        zmax = zmin+7.5D00
        j = j+1
        dzone(j) = i*75
        dfrst(j) = MAX(d1,zmin)
        dlast(j) = MIN(d2,zmax)
      ENDDO
      RETURN
      END
