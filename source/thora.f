Ccc   * $Rev: 2526 $
Ccc   * $Author: shoblit $
Ccc   * $Date: 2012-02-09 21:34:11 +0100 (Do, 09 Feb 2012) $
 
      SUBROUTINE THORA(Iout)
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      INTEGER :: Iout
C
C Local variables
C
      REAL, SAVE :: begday, begtim, difti1, diftim, endday, endtim
      CHARACTER(8) :: date
      INTEGER, DIMENSION(8) :: dt
      INTEGER :: INT, NINT
      LOGICAL :: never_called
      CHARACTER(10) :: time
      CHARACTER(5) :: zone
C
C*** End of declarations rewritten by SPAG
C
C
C     R. Capote, March 2005
C
C     GIVES THE TIME ELAPSED SINCE THE FIRST CALL
C     Note: Elapsed time must be less than one month
C
C     (g77, g95, LAHEY, and MS FORTRAN compatible)
C
      DATA never_called/.TRUE./
 
      IF(never_called)THEN
        never_called = .FALSE.
        CALL DATE_AND_TIME(date,time,zone,dt)
        begday = dt(3)
        begtim = dt(5)*3600 + dt(6)*60 + dt(7) + dt(8)/1000.
        WRITE(Iout,1010)time(1:2), time(3:4), time(5:6), date(7:8), 
     &                  date(5:6), date(1:4)
      ELSE
        CALL DATE_AND_TIME(date,time,zone,dt)
        endtim = dt(5)*3600 + dt(6)*60 + dt(7) + dt(8)/1000.
        endday = dt(3)
        endtim = endtim + (endday - begday)*86400.
        diftim = (endtim - begtim)/60.
        difti1 = (diftim - INT(diftim))*60.
        WRITE(Iout,1030)time(1:2), time(3:4), time(5:6), date(7:8), 
     &                  date(5:6), date(1:4)
        WRITE(Iout,1020)INT(diftim), NINT(difti1)
      ENDIF
 
      RETURN
 1010 FORMAT(/22X,'Start time: ',A2,':',A2,'.',A2,' (',A2,'-',A2,'-',A4,
     &       ')'/)
 1020 FORMAT(2X,'Calculation time: ',I3,' min ',I2,' s')
 1030 FORMAT(/2X,'End time: ',A2,':',A2,'.',A2,' (',A2,'-',A2,'-',A4,
     &       ')'/)
 
 
C====================================================================
      END SUBROUTINE THORA
 
 
