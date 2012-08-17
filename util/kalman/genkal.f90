      PROGRAM GENKAL

      implicit none

      ! this routine converts the empire cross sections for each reaction
      ! and the sensitivities those reactions have for each varied empire
      ! model parameter. The cross sections are written out to unit 50.
      ! the sensitivies are written out to unit 52. This routine assumes
      ! that the model parameters are completely independent and their
      ! correlation matrix is the identity matrix.

      integer*4, parameter :: nr = 60                ! maximum # reactions allowed
      integer*4, parameter :: ne = 1500              ! maximum # energies allowed for each reaction
      integer*4, parameter :: np = 200               ! maximum # of varied empire parameters

      integer*4 i,j,k,mt,mt1,ios,nreac,ie,mat,nex,npfns,l1,l2
      integer*4 ln1,ln2,la1,la2,lb1,lb2,lc1,lc2,nparm,lt1,lt2
      CHARACTER*4 IA,IB,IC
      CHARACTER*6 PN,TMP
      CHARACTER*8 INPSEN
      CHARACTER*15 MATSEN,XSC
      CHARACTER*12 REACTION(NR),PNAME(NP)
      CHARACTER*25 FILE
      real*8 E(NE),CSEC(NE,NR),PERTB(NP),SENS(NP,NE,NR),SW(NP),W(NP,NP)

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

      ! GENERATE CROSS SECTION FILE - UNIT 50

      OPEN(10,FILE=FILE(L1:L2)//trim(XSC),STATUS='OLD',iostat=ios)
      if(ios /= 0) then
          WRITE(0,*) 'CROSS SECTION DATA FILE NOT FOUND: ',FILE(L1:L2)//trim(XSC)
          stop 1
      endif

      READ(10,'(1X,I3)') nreac
      IF (nreac .GT. NR) then
          write(0,*) 'Too many reactions specified in file ',FILE(L1:L2)//trim(XSC)
          stop 1
      endif

      READ(10,'(12X,(94A12))') (REACTION(J),J=1,nreac)
      i = 1
      do
         READ(10,*,END=10) E(I),(CSEC(I,J),J=1,nreac)
         I = I+1
         IF (I > NE) then
             write(0,*) 'Too many energies in file ',FILE(L1:L2)//trim(XSC)
             stop 1
         endif
      end do
10    CLOSE(10)
      ie = i - 1

      ! CHECKING IF THE REQUIRED MT1 IS PRESENT IN THE FILE.XSC FILE
      ! This check is only necessary when fitting cross sections, not pfns

      IF(NPFNS.EQ.0) THEN
        J=0
        DO I = 1,nreac
           mt = RCTN(REACTION(I))
           IF (MT .EQ. MT1) THEN
              J=1
              exit
           ENDIF
        end do
        IF (J .EQ. 0) THEN
           WRITE(0,*) 'REACTION( MT=',MT1,') NOT FOUND IN ',FILE(L1:L2)//trim(XSC)
           STOP 1
        ENDIF
      ENDIF

      ! write the cross section file for kalman

      DO I=1,nreac
         WRITE(50,'(A12,31X,I5)') REACTION(I),IE
         WRITE(50,'(6(1PE11.4))') (E(J),CSEC(J,I),J=1,IE)
      end do
      CLOSE(50)


      ! GENERATE SENSITIVITY FILE - UNIT 52

      OPEN(10,FILE=FILE(L1:L2)//INPSEN,STATUS='OLD',iostat=ios)
      if(ios /= 0) then
          WRITE(0,*) 'PARAMETER FILE NOT FOUND: ',FILE(L1:L2)//INPSEN
          STOP 1
      endif
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
        IF (I > NP) then
            write(0,*) 'TOO MANY PARAMETERS in ',FILE(L1:L2)//INPSEN
            stop 1
        endif
      end do
20    CLOSE(10)
      NPARM=I-1

      DO I=1,NPARM
         DO J=1,NPARM
            W(I,J)=0.0
         end do
         W(I,I) = 1.0
         SW(I)  = 1.0
      end do

      WRITE(52,'(25X,I5)') NPARM
      WRITE(52,'(11A12)') (PNAME(I),I=1,NPARM)
      WRITE(52,100) (SW(I),I=1,NPARM)
      WRITE(52,100) (PERTB(I),I=1,NPARM)
      DO I=1,NPARM
         WRITE(52,'(20(F6.3))') (W(I,J),J=1,NPARM)
      end do

      OPEN(10,FILE=FILE(L1:L2)//trim(MATSEN),STATUS='OLD',iostat=ios)
      if(ios /= 0) then
          WRITE(0,*) 'SENSITIVITY FILE NOT FOUND: ',FILE(L1:L2)//trim(MATSEN)
          STOP 1
      endif
      DO I=1,NPARM
         READ(10,*) ! FIRST LINE
         READ(10,'(13X,A6)') PN
         CALL STRLEN(PN,LT1,LT2)
         TMP=PNAME(I)
         ! WRITE(0,*) TMP(LT1:LT2),PN(LT1:LT2)
         IF (TMP(LT1:LT2) .NE. PN(LT1:LT2)) then
            write(0,*) 'WRONG PARAMETER found in ',FILE(L1:L2)//trim(MATSEN)
            stop 1
         endif
         READ(10,*) ! THIRD LINE
         DO J=1,IE
            READ(10,*) E(J),(SENS(I,J,K),K=1,nreac)
         end do
         READ(10,*)!EMPTY LINE
      end do
      CLOSE(10)

      DO K=1,nreac
         WRITE(52,'(A12,13X,I5)') REACTION(K),IE
         DO J=1,IE
            WRITE(52,100)(SENS(I,J,K)*CSEC(J,K)/PERTB(I)/2.,I=1,NPARM)
         end do
      end do

100   FORMAT(11(1PE12.5))

      END
