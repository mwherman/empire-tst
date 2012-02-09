 
      SUBROUTINE READNUBAR(Infile,Nin,Ierr)
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      INTEGER :: Ierr
      CHARACTER(200) :: Infile
      INTEGER*4 :: Nin
C
C*** End of declarations rewritten by SPAG
C
C     to avoid compiler warnings
      Infile = ' '
      Nin = 10
      Ierr = 0
      RETURN
      END SUBROUTINE READNUBAR
 
!---------------------------------------------------------------------------
 
      FUNCTION FNIU_NUBAR_EVAL(En)
      IMPLICIT REAL*8(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      REAL*8 :: En
      REAL*8 :: FNIU_NUBAR_EVAL
C
C Local variables
C
      REAL*8, DIMENSION(20) :: eniu, vniu
      INTEGER :: i
C
C*** End of declarations rewritten by SPAG
C
      DATA eniu/1.D-11, 1.D0, 3.D0, 4.D0, 5.7D0, 7.D0, 10.D0, 14.7D0, 
     &     20.D0, 22.D0, 24.D0, 26.D0, 28.D0, 30.D0, 35.D0, 40.D0, 
     &     45.D0, 50.D0, 55.D0, 60.D0/
      DATA vniu/2.05D0, 2.127D0, 2.263D0, 2.4023D0, 2.64D0, 2.996D0, 
     &     3.37D0, 3.97D0, 4.79D0, 5.052D0, 5.2731D0, 5.5143D0, 
     &     5.7053D0, 5.9263D0, 6.4284D0, 6.8801D0, 7.3217D0, 7.7434D0, 
     &     8.1242D0, 8.5053D0/
 
      FNIU_NUBAR_EVAL = vniu(1)
      IF(En.LT.1.D-11)RETURN
 
C     if(en.gt.60) STOP 'En & 60 MeV, NO PFNM data'
      IF(En.GT.60.D0)THEN
        WRITE(8,*)' ERROR: Einc > 60 MeV in NUBAR calculation'
        STOP ' ERROR: Einc > 60 MeV in NUBAR calculation'
      ENDIF
 
      DO i = 1, 20
        IF(eniu(i).GT.En)EXIT
      ENDDO
      FNIU_NUBAR_EVAL = vniu(i - 1) + (vniu(i) - vniu(i - 1))
     &                  *(En - eniu(i - 1))/(eniu(i) - eniu(i - 1))
 
      RETURN
      END FUNCTION FNIU_NUBAR_EVAL
