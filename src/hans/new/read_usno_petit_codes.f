Date: Mon, 12 Mar 2001 16:28:29 +0100 (CET)
From: Jean-Marc PETIT <petit@obs-nice.fr>
To: scholl@obs-nice.fr



Content-Description: 

      PROGRAM  square
c
c...Extract A Square From USNO-A
c
      INCLUDE
     *         'square.inc'
      DOUBLE PRECISION
     *         r, d, s
      INTEGER
     *         i, j
c
 9001 FORMAT (' Found', i10, ' Entries')
c
  100 CALL whereis
      oldzone = -1
      scale = 67.14D00
      CALL gimme(r,d,s)
      CALL corner(r,d,s)
      nsav = 0
      DO j=1,ndec
        DO i=1,nra
          CALL eatit(rfrst(i),rlast(i),dzone(j),dfrst(j),dlast(j))
        ENDDO
      ENDDO
      CLOSE (2)
      WRITE (*,9001) nsav

      END
c
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
      open (unit=1, file='catalog.toc', status='old', err=101)
      goto 110
 101  continue
      open (unit=1, file='../../../usno/catalog.toc',
     $  status='old', err=102)
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
c
      SUBROUTINE  gimme(v1,v2,v3)
c
c...Get RA,DEC,Width From Command Line Or User
c
      IMPLICIT
     *            NONE
      DOUBLE PRECISION
     *            v1, v2, v3
      INTEGER
     *            nlb, n, IARGC, i
      CHARACTER*64
     *            lb
c
 9001 FORMAT (' Enter Alpha (deg): ',$)
 9002 FORMAT (' Enter Delta (deg): ',$)
 9003 FORMAT (' Enter Width (''): ',$)
c
  100 n = IARGC()
      IF (n.ne.3) THEN
  110   WRITE (*,9001)
        call readq (5, nlb, lb)
        IF (nlb.le.0) GO TO 110
        CALL hms(lb(1:nlb),v1)
  120   WRITE (*,9002)
        call readq (5, nlb, lb)
        IF (nlb.le.0) GO TO 120
        CALL hms(lb(1:nlb),v2)
  130   WRITE (*,9003)
        call readq (5, nlb, lb)
        IF (nlb.le.0) GO TO 130
        CALL ms(lb(1:nlb),v3)
      ELSE
        DO i=1,3
          CALL getarg(i,lb)
          nlb = 0
          DO nlb=LEN(lb),2,-1
            IF (lb(nlb:nlb).gt.' ') GO TO 140
          ENDDO
  140     IF (i.eq.1) THEN
            CALL hms(lb(1:nlb),v1)
          ELSEIF (i.eq.2) THEN
            CALL hms(lb(1:nlb),v2)
          ELSE
            CALL ms(lb(1:nlb),v3)
          ENDIF
        ENDDO
      ENDIF
      v3 = 0.5D00*v3
      RETURN
c
c...Error
c
  200 stop
      END
c
	subroutine readq (canal, long, chaine)

	integer*4
     1	  canal,
     1	  i,
     1	  long

	character
     1	  chaine*(*)

	long = len(chaine)
	do 1001 i = 1, long
	    chaine(i:i) = char(0)
 1001	continue

   10	format (a)
	read (canal, 10) chaine

 1002	continue
	if (chaine(long:long).eq.char(0)
     1		.or. chaine(long:long).eq.' ') then
	    long = long - 1
	    if (long.eq.0) goto 1003
	    goto 1002
	end if
 1003	continue

	return
	end
c
      SUBROUTINE  hms(str,val)
c
c...Crack String And Create Value
c
      IMPLICIT
     *            NONE
      CHARACTER*(*)
     *            str
      DOUBLE PRECISION
     *            val, piece(3), dp, sgn, z
      INTEGER
     *            nstr, i, j, dpfind
      CHARACTER*1
     *            c
c
c...Initialization
c
  100 val = 0.0D00
      DO i=1,3
        piece(i) = 0.0D00
      ENDDO
      j = 1
      dpfind = 0
      sgn = 1.0D00
      nstr = LEN(str)
      IF (nstr.le.0) RETURN
c
c...Loop Over The String
c
      DO i=1,nstr
        c = str(i:i)
c
c...Parse
c
        IF ((c.eq.'-').or.(c.eq.'e').or.(c.eq.'E')
     *  .or.(c.eq.'s').or.(c.eq.'S')) THEN
          sgn = -1.0D00
        ELSEIF ((c.eq.'+').or.(c.eq.'w').or.(c.eq.'W')
     *      .or.(c.eq.'n').or.(c.eq.'N')) THEN
          sgn = 1.0D00
        ELSEIF ((c.eq.':').or.(c.eq.',').or.(c.eq.' ')) THEN
          j = j+1
          dpfind = 0
          IF (j.gt.3) GO TO 110
        ELSEIF (c.eq.'.') THEN
          dpfind = 1
          dp = 1.0D00
        ELSEIF ((c.ge.'0').and.(c.le.'9')) THEN
          z = ICHAR(c)-ICHAR('0')
          IF (dpfind.eq.0) THEN
            piece(j) = 10.0D00*piece(j) + z
          ELSE
            dp = 0.1D00*dp
            piece(j) = piece(j) + dp*z
          ENDIF
        ENDIF
      ENDDO
c
c...Return
c
  110 val = piece(1) + piece(2)/60.0D00 + piece(3)/3600.0D00
      val = val*sgn
      RETURN
      END
c
      SUBROUTINE  ms(str,val)
c
c...Crack String And Create Value
c
      IMPLICIT
     *            NONE
      CHARACTER*(*)
     *            str
      DOUBLE PRECISION
     *            val, piece(2), dp, sgn, z
      INTEGER
     *            nstr, i, j, dpfind
      CHARACTER*1
     *            c
c
c...Initialization
c
  100 val = 0.0D00
      DO i=1,2
        piece(i) = 0.0D00
      ENDDO
      j = 1
      dpfind = 0
      sgn = 1.0D00
      nstr = LEN(str)
      IF (nstr.le.0) RETURN
c
c...Loop Over The String
c
      DO i=1,nstr
        c = str(i:i)
c
c...Parse
c
        IF ((c.eq.'-').or.(c.eq.'e').or.(c.eq.'E')
     *  .or.(c.eq.'s').or.(c.eq.'S')) THEN
          sgn = -1.0D00
        ELSEIF ((c.eq.'+').or.(c.eq.'w').or.(c.eq.'W')
     *      .or.(c.eq.'n').or.(c.eq.'N')) THEN
          sgn = 1.0D00
        ELSEIF ((c.eq.':').or.(c.eq.',').or.(c.eq.' ')) THEN
          j = j+1
          dpfind = 0
          IF (j.gt.2) GO TO 110
        ELSEIF (c.eq.'.') THEN
          dpfind = 1
          dp = 1.0D00
        ELSEIF ((c.ge.'0').and.(c.le.'9')) THEN
          z = ICHAR(c)-ICHAR('0')
          IF (dpfind.eq.0) THEN
            piece(j) = 10.0D00*piece(j) + z
          ELSE
            dp = 0.1D00*dp
            piece(j) = piece(j) + dp*z
          ENDIF
        ENDIF
      ENDDO
c
c...Return
c
  110 val = piece(1)/60.0D00 + piece(2)/3600.0D00
      val = val*sgn
      RETURN
      END
c
      SUBROUTINE  corner(r,d,s)
c
c...Take Nominal And Compute Corners And Other Search Parameters
c
      INCLUDE
     *            'square.inc'
      DOUBLE PRECISION
     *            r, d, s, rd, r1, r2, d1, d2, cd, zmin, zmax
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
      IF (s.le.0.0D00) THEN
        WRITE (*,9002) s
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
      r1 = rd - s/cd
      r2 = rd + s/cd
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
      d1 = d - s
      d2 = d + s
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
c
      SUBROUTINE  eatit(r1,r2,dz,d1,d2)
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
        OPEN (name=lb(1:nlb),
     *        status='old',
     *        unit=1,
     *        err=200
     *       )
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
      WRITE (*,9006) oldzone,ir1,ir2,id1,id2
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
                CALL saveit(j)
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
c
c...You Better Not Get Here
c
  200 continue
      write (6, '(a)') 'Plase insert USNO-A2.0 CDROM number'
     $  //fn(i)(nfn(i)+1:nfn(i)+3)//', then press ENTER.'
      read (5, *)
      goto 120

      END
c
      SUBROUTINE  saveit(j)
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
c
 9003 FORMAT (i5, 3i15)
 9004 FORMAT (' NSAV=', i10)
c
  100 IF (nsav.eq.1) THEN
        OPEN (name='usno.ccmap',
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
      write (2, *) float(buf(1,j))/360000.,
     $  float(buf(2,j))/360000.-90.,
     $  float(mr)/10., oldzone*million+j
      IF (MOD(nsav,1000).eq.0) THEN
        WRITE (*,9004) nsav
      ENDIF
      RETURN
c
c...User Exit
c
  200 stop
      END
Content-Description: 

#include	<stdio.h>
#include	<sys/types.h>
#include	<unistd.h>
#include	<netinet/in.h>
/*
 **************************************************************************
 *
 *   C_OPENER  --  Open A NEW File
 */
c_opener__(bb)
char bb[];
{
	return(creat(bb,0644));
}
/*
 ***************************************************************************
 *
 *   C_CLOSER  --  Close A File
 */
c_closer__(fd)
int *fd;
{
	return(close(*fd));
}
/*
 ***************************************************************************
 *
 *  C_WRITER  --  Use HTONL() To Select ENDIAN
 */
c_writer__(fd,buf,n)
int *fd, *n;
char buf[];
{
	u_long x;
	int i, nn;
	char b;

	nn = (*n);
	x = 102030405;
	if (htonl(x) != x) {
		for (i=0; i<nn; i+=4) {
			b = buf[i  ];
			buf[i  ] = buf[i+3];
			buf[i+3] = b;
			b = buf[i+1];
			buf[i+1] = buf[i+2];
			buf[i+2] = b;
		}
	}
	i = write(*fd,buf,nn);
	if (nn == i)
		return(0);
	else
		return(i);
}
/*
 *****************************************************************************
 *
 *  C_READER  --  Use HTONL() To Select ENDIAN
 */
c_reader__(fd,buf,n)
int *fd, *n;
char buf[];
{
	u_long x;
	int i, nn;
	char b;

	nn = (*n);
	i = read(*fd,buf,nn);
	if (i == nn) {
		x = 102030405;
		if (htonl(x) != x) {
			for (i=0; i<nn; i+=4) {
				b = buf[i  ];
				buf[i  ] = buf[i+3];
				buf[i+3] = b;
				b = buf[i+1];
				buf[i+1] = buf[i+2];
				buf[i+2] = b;
			}
		}
		return(0);
	} else
		return(i);
}
/*
 ***************************************************************************
 *
 *   C_ROOPEN -- Open A ReadOnly File
 */
c_roopen__(bb)
char bb[];
{
	return(open(bb,0));
}
/*
 ****************************************************************************
 *
 *  C_GENLEN  --  Return The Length Of A File
 */
c_genlen__(bb)
char bb[];
{
	int fd, n;

	if ((fd=open(bb,0)) < 3)
		return(-1);
	n = (int)lseek(fd, 0, SEEK_END);
	close(fd);
	return(n);
}
/*
 ****************************************************************************
 *
 *   position  --  Move file pointer
 */
c_position__(fd,nb)
int *fd,*nb;
{
	return(lseek(*fd, *nb, SEEK_SET));
}
/*
 **************************************************************************
 *
 *   rawwriter  --  Just scribble
 */
c_rawwriter__(fd,bb,n)
int *fd,*n;
char bb[];
{
	int i, nn;

	nn = (*n);
	i = write(*fd,bb,nn);
	if (i == nn)
		return(0);
	else
		return(i);
}
/*
 ****************************************************************************
 *
 *   rawreader -- Just read
 */
c_rawreader__(fd,bb,n)
int *fd,*n;
char bb[];
{
	int i, nn;

	nn = (*n);
	i = read(*fd,bb,nn);
	if (i == nn)
		return(0);
	else
		return(i);
}
Content-Description: 

/cdrom/zone0000.cat 01
/cdrom/zone0075.cat 01
/cdrom/zone0150.cat 09
/cdrom/zone0225.cat 07
/cdrom/zone0300.cat 05
/cdrom/zone0375.cat 04
/cdrom/zone0450.cat 03
/cdrom/zone0525.cat 02
/cdrom/zone0600.cat 01
/cdrom/zone0675.cat 06
/cdrom/zone0750.cat 07
/cdrom/zone0825.cat 10
/cdrom/zone0900.cat 09
/cdrom/zone0975.cat 08
/cdrom/zone1050.cat 08
/cdrom/zone1125.cat 11
/cdrom/zone1200.cat 10
/cdrom/zone1275.cat 11
/cdrom/zone1350.cat 06
/cdrom/zone1425.cat 04
/cdrom/zone1500.cat 02
/cdrom/zone1575.cat 03
/cdrom/zone1650.cat 03
/cdrom/zone1725.cat 02
