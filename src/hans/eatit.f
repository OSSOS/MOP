      SUBROUTINE  eatit(r1,r2,dz,d1,d2,usno_star_file)
c
c...Ingest ACC And Loop Over DAT
c
      INCLUDE
     *            'square.inc'
      INTEGER
     *            dz, i, i1, i2, fr, nr, C_ROOPEN, fd, nlb, err,
     *            C_POSITION, n, nmost, nlast, ir1, ir2, id1, id2,
     *            C_CLOSER, C_READER, m, j
      DOUBLE PRECISION
     *            r1, r2, d1, d2
      CHARACTER*64
     *            lb
      BYTE
     *            bb(65)

      character*80 usno_star_file
c
 9002 FORMAT (5x, 2i12)
 9003 FORMAT (' Cannot Open ', a)
 9004 FORMAT (' Fatal Error Accessing ', a)
 9005 FORMAT (' Too Many Stars - Quitting Early')
 9006 FORMAT (' Z=', i4, ' RA(', i9, ':', i9, ')  SPD(', i9, ':',
     *        i9, ')')
c
c...Eat The ACC File
c
  100 i = (dz/75) + 1
      lb = fn(i)
      nlb = nfn(i)
      IF (dz.ne.oldzone) THEN
        lb(nlb-2:nlb) = 'acc'
 120    continue
        write (6,'(a)') 'Attempting to open'//lb(1:nlb)

        OPEN (name=lb(1:nlb),
     *        status='old',
     *        unit=1,
     *        err=200
     *       )
        goto 121
  200 continue
      write (6, '(a)') 'Plase insert USNO-A2.0 CDROM number'
     $  //fn(i)(nfn(i)+1:nfn(i)+3)//', then press ENTER.'
      read (5, *)
      goto 120
  121 continue
        DO i=1,NACC
          READ (1,9002) frec(i),nrec(i)
        ENDDO
        CLOSE (1)
        oldzone = dz
      ENDIF
c
c...Compute Offset And Length
c
      i1 = r1/3.75D00
      i1 = MAX(1,MIN(NACC,i1+1))
      i2 = r2/3.75D00
      i2 = MAX(1,MIN(NACC,i2+1))
      fr = frec(i1)-1
      nr = 0
      DO i=i1,i2
        nr = nr+nrec(i)
      ENDDO
c
c...Open And Position File
c
      lb(nlb-2:nlb) = 'cat'
      write (*,*) lb
      DO i=1,nlb
        bb(i) = ICHAR(lb(i:i))
      ENDDO
      bb(nlb+1) = 0
      fd = C_ROOPEN(bb)
      IF (fd.lt.3) THEN
        WRITE (*,9003) lb(1:nlb)
        stop
      ENDIF
      IF (fr.gt.0) THEN
        err = C_POSITION(fd,12*fr)
        IF (err.le.0) THEN
          WRITE (*,9004) lb(1:nlb)
          stop
        ENDIF
      ENDIF
c
c...Set Up Search Parameters
c
      n = ((nr-1)/NCHUNK) + 1
      IF (n.gt.1) THEN
        nmost = NCHUNK
        nlast = nr - (n-1)*NCHUNK
      ELSE
        nmost = 0
        nlast = nr
      ENDIF
      ir1 = CONVERT*r1
      ir2 = CONVERT*r2
      id1 = CONVERT*(d1+90.0D00)
      id2 = CONVERT*(d2+90.0D00)
c     WRITE (6,9006) oldzone,ir1,ir2,id1,id2
c
c...Do The Search
c
      DO i=1,n
        IF (i.eq.n) THEN
          m = nlast
        ELSE
          m = nmost
        ENDIF
        err = C_READER(fd,buf,12*m)
        IF (err.ne.0) THEN
          WRITE (*,9004) lb(1:nlb)
          stop
        ENDIF
        DO j=1,m
          IF (buf(1,j).ge.ir1) THEN
            IF (buf(1,j).le.ir2) THEN
              IF ((buf(2,j).ge.id1).and.(buf(2,j).le.id2)) THEN
                nsav = nsav+1
                CALL saveit(j,usno_star_file)
              ENDIF
            ELSE
              GO TO 110
            ENDIF
          ENDIF
        ENDDO
        fr = fr+m
      ENDDO
c
c...All Done
c
  110 err = C_CLOSER(fd)
      RETURN
      END
