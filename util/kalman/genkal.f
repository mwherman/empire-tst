      PROGRAM GENKAL

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      IMPLICIT INTEGER (I-N)

      PARAMETER (NE=1300,NR=60,NP=180)
      CHARACTER*4 IA,IB,IC
      CHARACTER*6 PN,TMP
      CHARACTER*8 INPSEN
      CHARACTER*15 MATSEN,XSC
      CHARACTER*12 REACTION(NR),PNAME(NP)
      CHARACTER*25 FILE
      DIMENSION E(NE),CSEC(NE,NR),PERTB(NP),SENS(NP,NE,NR)
      DIMENSION SW(NP),W(NP,NP)

      integer*4, external :: rctn

      INPSEN='-inp.sen'

      READ(5,*) FILE,MT1,MAT,NEX,NPFNS
      IF (NPFNS.EQ.0) THEN
        MATSEN='-mat.sen'
        XSC='.xsc'
      ELSE
        MATSEN='-pfns-mat.sen'
        XSC='-pfns.kal'
      ENDIF

      CALL STRLEN(FILE,L1,L2)

C  GENERATE CROSS SECTION FILE - UNIT 50

      OPEN(10,FILE=FILE(L1:L2)//trim(XSC),STATUS='OLD',ERR=100)
      READ(10,'(1X,I3)') NNUCD
      IF (NNUCD .GT. NR) STOP 'TOO MANY REACTIONS'
      READ(10,'(12X,(94A12))') (REACTION(J),J=1,NNUCD)
      I=1
      do
         READ(10,*,END=10) E(I),(CSEC(I,J),J=1,NNUCD)
         I = I+1
         IF (I .GE. NE) STOP 'TOO MANY ENERGIES'
      end do
10    CLOSE(10)
      IE=I-1

C CHECKING IF THE REQUIRED MT1 IS PRESENT IN THE FILE.XSC FILE
      IF(NPFNS.EQ.0) THEN ! This check is only necessary when fitting cross sections, not pfns
        J=0
        DO I = 1,NNUCD
           mt = RCTN(REACTION(I))
           IF (MT .EQ. MT1) THEN
              J=1
              exit
           ENDIF
        end do
        IF (J .EQ. 0) THEN
           WRITE(0,*) 'REACTION( MT=',MT1,') NOT FOUND IN '
     &   ,FILE(L1:L2)//trim(XSC)
           WRITE(6,*)  'REACTION( MT=',MT1,') NOT FOUND IN '
     &   ,FILE(L1:L2)//trim(XSC)
           STOP
        ENDIF
      ENDIF

      DO I=1,NNUCD
         WRITE(50,1000) REACTION(I),IE
         WRITE(50,1001) (E(J),CSEC(J,I),J=1,IE)
      end do
      CLOSE(50)


C  GENERATE SENSITIVITY FILE - UNIT 52

      OPEN(10,FILE=FILE(L1:L2)//INPSEN,STATUS='OLD',ERR=200)
      I=1
      do
        READ(10,*,END=20) PN,PERTB(I),IA,IB,IC
        IF (PN(1:1).EQ.'!') cycle
        CALL STRLEN(PN,LN1,LN2)

        IF (IA .NE. '!') THEN
          CALL STRLEN(IA,LA1,LA2)
          CALL STRLEN(IB,LB1,LB2)
          PNAME(I)=PN(LN1:LN2)//IA(LA1:LA2)//IB(LB1:LB2)
          IF (IC .NE. '!') THEN
            CALL STRLEN(IC,LC1,LC2)
            PNAME(I)=PN(LN1:LN2)//IA(LA1:LA2)//IB(LB1:LB2)//IC(LC1:LC2)
          ENDIF
        ELSE
          PNAME(I)=PN
        ENDIF

        I = I+1
        IF (I .GE. NP) STOP 'TOO MANY PARAMETERS'
      end do
20    CLOSE(10)
      NPARM=I-1
      write(0,*) nparm

      DO I=1,NPARM
         DO J=1,NPARM
            W(I,J)=0.0
         end do
         W(I,I) = 1.0
         SW(I)  = 1.0
         ! UN(I) = 0.1
      end do

      WRITE(52,1003) NPARM
      WRITE(52,1004) (PNAME(I),I=1,NPARM)
      WRITE(52,1005) (SW(I),I=1,NPARM)
      ! WRITE(52,1005) (UN(I),I=1,NPARM)
      WRITE(52,1005) (PERTB(I),I=1,NPARM)
      DO I=1,NPARM
         WRITE(52,1006) (W(I,J),J=1,NPARM)
      end do

      OPEN(10,FILE=FILE(L1:L2)//trim(MATSEN),STATUS='OLD',ERR=300)
      DO I=1,NPARM
         READ(10,*)!FIRST LINE
         READ(10,'(13X,A6)') PN
         CALL STRLEN(PN,LT1,LT2)
         TMP=PNAME(I)
         ! WRITE(0,*) TMP(LT1:LT2),PN(LT1:LT2)
         IF (TMP(LT1:LT2) .NE. PN(LT1:LT2)) STOP 'WRONG PARAMETER'
         READ(10,*)!THIRD LINE
         DO J=1,IE
            READ(10,*) E(J),(SENS(I,J,K),K=1,NNUCD)
         end do
         READ(10,*)!EMPTY LINE
      end do
      CLOSE(10)

      DO K=1,NNUCD
         WRITE(52,'(A12,13X,I5)') REACTION(K),IE
         DO J=1,IE
            WRITE(52,1005)(SENS(I,J,K)*CSEC(J,K)/PERTB(I)/2.,I=1,NPARM)
         end do
      end do

      STOP

 100  WRITE(0,*) 'CROSS SECTION DATA FILE NOT FOUND: ',FILE(L1:L2)//
     &trim(XSC)
      STOP
 200  WRITE(0,*) 'PARAMETER FILE NOT FOUND: ',FILE(L1:L2)//INPSEN
      STOP
 300  WRITE(0,*) 'SENSITIVITY FILE NOT FOUND: ',
     &FILE(L1:L2)//trim(MATSEN)
      STOP
 1000 FORMAT(A12,31X,I5)
 1001 FORMAT(6(1PE11.4))
C 1002 FORMAT(A6,1PE11.2,3(A4),I2)
 1003 FORMAT(25X,I5)
 1004 FORMAT(11A12)
 1005 FORMAT(11(1PE12.5))
 1006 FORMAT(20(F6.3))

      END
