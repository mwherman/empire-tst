      SUBROUTINE TIMEIT(SECONDS)
C=======================================================================
C
C     LINUX - TIME SINCE START OF PROBLEM (BASE TIME) IN SECONDS
C
C=======================================================================
      SAVE
      DIMENSION TARRAY(2)
      SECONDS=ETIME(TARRAY)
      SECONDS=(TARRAY(1)+TARRAY(2))
      RETURN
      END
