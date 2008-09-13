Ccc   * $Author: Capote $
Ccc   * $Date: 2008-09-13 16:17:05 $
Ccc   * $Id: thora.f,v 1.7 2008-09-13 16:17:05 Capote Exp $
      SUBROUTINE THORA(IOUT)
C
C     R. Capote, March 2005
C
C     GIVES THE TIME ELAPSED SINCE THE FIRST CALL
C     Note: Elapsed time must be less than one month
C
C     (g77, g95, LAHEY, and MS FORTRAN compatible)
C
      REAL BEGTIM,ENDTIM,BEGDAY,ENDDAY,DIFTIM,DIFTI1
      INTEGER IOUT
      CHARACTER*8 DATE
      CHARACTER*10 TIME
      CHARACTER*5 ZONE
      INTEGER DT
      DIMENSION DT(8)
      LOGICAL NEVER_CALLED
      DATA NEVER_CALLED/.TRUE./
      SAVE BEGTIM,BEGDAY,NEVER_CALLED

      IF (NEVER_CALLED) then
        NEVER_CALLED = .FALSE.
        CALL DATE_AND_TIME (DATE,TIME,ZONE,DT)
        BEGDAY=DT(3)
        BEGTIM=DT(5)*3600+DT(6)*60+DT(7)+DT(8)/1000.
        WRITE(IOUT,1001) time(1:2),time(3:4),time(5:6),
     >                   DATE(7:8),DATE(5:6),DATE(1:4)
      ELSE
        CALL DATE_AND_TIME (DATE,TIME,ZONE,DT)
        ENDTIM=DT(5)*3600+DT(6)*60+DT(7)+DT(8)/1000.
        ENDDAY=DT(3)
        ENDTIM = ENDTIM +  (ENDDAY-BEGDAY)*86400.
        DIFTIM=(ENDTIM-BEGTIM)/60.
        DIFTI1=(DIFTIM-INT(DIFTIM))*60.
        WRITE(IOUT,1003) time(1:2),time(3:4),time(5:6),
     >                   DATE(7:8),DATE(5:6),DATE(1:4)
        WRITE(IOUT,1002) INT(DIFTIM),NINT(DIFTI1)
      ENDIF

      RETURN
 1001 FORMAT
     >(/22X,'Start time: ',A2,':',A2,'.',A2,' (',A2,'-',A2,'-',A4,')'/)
 1002 FORMAT(1X,' Calculation time: ',I3,' min ',I2,' s')
 1003 FORMAT
     >(/22X,'End   time: ',A2,':',A2,'.',A2,' (',A2,'-',A2,'-',A4,')'/)


C====================================================================
      END