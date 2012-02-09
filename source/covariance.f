Ccc   * $Rev: 2526 $
Ccc   * $Author: shoblit $
Ccc   * $Date: 2012-02-09 21:34:11 +0100 (Do, 09 Feb 2012) $
 
      PROGRAM COVARIANCE
C
C*** Start of declarations rewritten by SPAG
C
C Local variables
C
      CHARACTER(4) :: crun
      INTEGER :: i, nruns, nstart
      INTEGER*4 :: i1, i2
C
C*** End of declarations rewritten by SPAG
C
C     write(*,*) 'Number of sampling runs?'
      OPEN(10,FILE = 'RUNS.INP')
      READ(10,*)nstart, nruns
      CLOSE(10)
      DO i = nstart, nruns + nruns
        CALL SYSTEM('../source/empire')
        WRITE(crun,'(I4.4)')i
        CALL SYSTEM('mv OUTPUT.DAT OUT'//crun)
        CALL SYSTEM('mv LIST.DAT LST'//crun)
        OPEN(10,FILE = 'R250SEED.DAT',STATUS = 'OLD')
        READ(10,*)i1, i2
        WRITE(*,'(1x,A6,i12,1x,i12/)')'SEEDS:', i1, i2
        CLOSE(10)
      ENDDO
      STOP
      END PROGRAM COVARIANCE
