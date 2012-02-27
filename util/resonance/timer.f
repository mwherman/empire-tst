      SUBROUTINE TIMER
C=======================================================================
C
C     TOTAL EXECUTION TIME
C
C=======================================================================
      SAVE
      INTEGER OUTP,OTAPE
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      DATA TSTART/0.0/
      DATA IPASS/0/
C-----DEFINE CURRENT TIME
      CALL TIMEIT(TNOW)
C-----ON FIRST PASS DEFINE STARTING TIME
      IF(IPASS.EQ.0) TSTART=TNOW
      IPASS=IPASS+1
C-----PRINT EVERY PASS EXCEPT FIRST ONE
      IF(IPASS.LE.1) RETURN
      WRITE(OUTP,10) TNOW-TSTART
      WRITE(OUTP,20)
C-----OUTPUT TO SCREEN ONLY IF ENDF/B OUTPUT IS PRODUCED
      IF(OTAPE.GT.0) WRITE(*   ,10) TNOW-TSTART
      IF(OTAPE.GT.0) WRITE(*   ,20)
   10 FORMAT(1X,78('=')/' Total Execution Time',F20.2,' Seconds')
   20 FORMAT(1X,78('='))
      RETURN
      END
