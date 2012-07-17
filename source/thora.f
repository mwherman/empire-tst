Ccc   * $Rev: 2942 $
Ccc   * $Author: rcapote $
Ccc   * $Date: 2012-07-17 10:01:11 +0200 (Di, 17 Jul 2012) $

      SUBROUTINE THORA(IOUT)
C
C     R. Capote, March 2005
C
C     GIVES THE TIME ELAPSED SINCE THE FIRST CALL
C     Note: Elapsed time must be less than one month
C
C     (g77, g95, LAHEY, and MS FORTRAN compatible)
C
      REAL*8 BEGTIM,ENDTIM,BEGDAY,ENDDAY,DIFTIM,DIFTI1,DIFTI2,DIFHOUR
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
        ENDTIM=DT(5)*3600.d0+DT(6)*60.d0+DT(7)+DT(8)/1000.d0
        ENDDAY=DT(3)
        ENDTIM = ENDTIM +  (ENDDAY-BEGDAY)*86400.
        DIFTIM=(ENDTIM-BEGTIM)/60.
	DIFHOUR=0.d0
	IF(DIFTIM.GT.60.d0) DIFHOUR=DIFTIM/60.D0
        DIFTI1=(DIFTIM-INT(DIFTIM))*60.d0
	DIFTI2=(DIFHOUR-INT(DIFHOUR))*60.
        WRITE(IOUT,1003) time(1:2),time(3:4),time(5:6),
     >                   DATE(7:8),DATE(5:6),DATE(1:4)
	IF(DIFTIM.LT.60.d0) THEN      
          WRITE(IOUT,1002) INT(DIFTIM),NINT(DIFTI1)
	ELSE
          WRITE(IOUT,1004) INT(DIFHOUR),NINT(DIFTI2),NINT(DIFTI1)	
	ENDIF  
      ENDIF

      RETURN
 1001 FORMAT
     >(/22X,'Start time: ',A2,':',A2,'.',A2,' (',A2,'-',A2,'-',A4,')'/)
 1002 FORMAT(2X,'Calculation time: ',I5,' min ',I2,' s')
 1003 FORMAT
     >(/2X,'End time: ',A2,':',A2,'.',A2,' (',A2,'-',A2,'-',A4,')'/)
 1004 FORMAT(2X,'Calculation time: ',I4,' H ', I2,' min ',I2,' s')     


C====================================================================
      END


