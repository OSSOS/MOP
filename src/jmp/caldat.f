C -*-compile-command: "make_lib"; -*-

C FILE CALDAT.FTN
C     SUBROUTINE CALDAT                                      1986 Feb 18
C     Computes calendar date from a Julian Date

C     Copyright (C) 1986 by David J. Tholen

      SUBROUTINE CALDAT(JD,JCLNDR,Y,M,D)
      IMPLICIT DOUBLE PRECISION (A-Z)
      INTEGER Y,M,IJDP,LPDAYS,LPD100,LPD400,TMP
      LOGICAL JCLNDR
      DATA ZERO,JULYR,DYMNTH/0.0d0,365.25d0,30.6001d0/

C     Separate fractional part of day from the Julian Date

      JDP = JD + 0.5d0
      IJDP = IDINT(JDP)
      IF (JDP .LT. ZERO .AND. JDP .NE. DBLE(IJDP)) IJDP = IJDP - 1
      DIJDP = DBLE(IJDP)
      JDF = JDP - DIJDP

C     If Gregorian calendar, adjust for missing leap days

      IF (JCLNDR) THEN
         LPDAYS = 0
      ELSE
         Y100 = (DIJDP - 1867216.25d0)/36524.25d0
         LPD100 = IDINT(Y100)
         IF (Y100 .LT. ZERO .AND. Y100 .NE. DBLE(LPD100))
     &      LPD100 = LPD100 - 1
         Y400 = DBLE(LPD100)/4.0d0
         LPD400 = IDINT(Y400)
         IF (Y400 .LT. ZERO .AND. Y400 .NE. DBLE(LPD400))
     &      LPD400 = LPD400 - 1
         LPDAYS = 1 + LPD100 - LPD400
      END IF

C     Determine year (relative to year -4716)

      TMP = IJDP + 1524 + LPDAYS
      DY = (DBLE(TMP) - 122.1d0)/JULYR
      IF (DY .LT. ZERO) DY = DY - 1.0d0
      Y = IDINT(DY)

C     Determine month

      DM = JULYR*DBLE(Y)
      IF (DM .LT. ZERO) DM = DM - 0.9d0
      TMP = TMP - IDINT(DM)
      M = IDINT(DBLE(TMP)/DYMNTH)

C     Determine day

      D = DBLE(TMP - IDINT(DYMNTH*DBLE(M))) + JDF

C     'Internal' calendar starts in March so that leap days occur at end
C     of year; adjust calendar to start in January

      IF (M .LE. 13) THEN
         M = M - 1
      ELSE
         M = M - 13
      END IF
      IF (M .GE. 3) THEN
         Y = Y - 4716
      ELSE
         Y = Y - 4715
      END IF
      RETURN
      END


        FUNCTION JULDAY(MM,ID,IYYY)
        PARAMETER (IGREG=15+31*(10+12*1582))
        IF (IYYY.EQ.0) PAUSE 'There is no Year Zero.'
        IF (IYYY.LT.0) IYYY=IYYY+1
        IF (MM.GT.2) THEN
                JY=IYYY
                JM=MM+1
        ELSE
                JY=IYYY-1
                JM=MM+13
        ENDIF
        JULDAY=INT(365.25*JY)+INT(30.6001*JM)+ID+1720995
        IF (ID+31*(MM+12*IYYY).GE.IGREG) THEN
                JA=INT(0.01*JY)
                JULDAY=JULDAY+2-JA+INT(0.25*JA)
        ENDIF
        RETURN
        END
