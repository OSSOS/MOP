C     @ JJ Kavelaars DEC 2002
C
C     CREATE A COMMAND LINE FOR THE perl scripts search_usno.pl
C
C     search_usno.pl PRODUCES A LIST OF AVAILABLE USNO STARS
C

      subroutine square(r,d,s_alpha,s_delta,n_usno,usno_star_file)

      INCLUDE
     *         'square.inc'
      DOUBLE PRECISION
     *         r, d, s_alpha,s_delta,ra,dec
      INTEGER
     *         n_usno, i, j, status
      CHARACTER*80 usno_star_file
      CHARACTER*256 cmd

c First try local catalog.

 100  CALL whereis (status)

      if (status .eq. 0) then
         oldzone = -1
         scale = 67.14D00
         CALL corner(r,d,s_alpha,s_delta)
         nsav = 0
         DO j=1,ndec
            DO i=1,nra
               CALL eatit(rfrst(i),rlast(i),dzone(j),dfrst(j),
     *              dlast(j),usno_star_file)
            ENDDO
         ENDDO
         CLOSE (2)
         n_usno = nsav

      else

c Local solution didn't work. Try on-line.

         WRITE (cmd,*) 'search_usno.pl --ra  ',r,
     *        ' --dec ',d,
     *        ' --xsize',s_alpha,
     *        ' --ysize ',s_delta,
     *        ' --num 5000',
     *        ' --file ',
     *        usno_star_file
         CALL SYSTEM(cmd,status)

C     COUNT THE NUMBER OF USNO STARS

         n_usno = 0
         open(10,file=usno_star_file,status='old') 
 200     continue
            read (10, *, err=200, end=210) ra,dec
            n_usno = n_usno+1
            goto 200
 210     continue
         close (10)
      end if

      if ( n_usno .eq. 0 ) then
         write(*,*) 'USNO CATALOG SEARCH FAILED'
      end if

      RETURN

      END
