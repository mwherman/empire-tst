      PROGRAM GENKAL
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
      PARAMETER (NE=500,NR=60,NP=180)
      CHARACTER*4 XSC,IA,IB,IC
      CHARACTER*6 PN,TMP
      CHARACTER*8 INPSEN,MATSEN
      CHARACTER*12 REACTION(NR),PNAME(NP)
      CHARACTER*25 FILE
      DIMENSION E(NE),CSEC(NE,NR),PERTB(NP),SENS(NP,NE,NR)
      DIMENSION SW(NP),W(NP,NP)

      XSC='.xsc'
      INPSEN='-inp.sen'
      MATSEN='-mat.sen'

      READ(5,*) FILE,MT1

      CALL STRLEN(FILE,L)


C  GENERATE CROSS SECTION FILE - UNIT 50

      OPEN(10,FILE=FILE(1:L)//XSC,STATUS='OLD',ERR=100)
      READ(10,'(1X,I3)') NNUCD
      IF (NNUCD .GT. NR) STOP 'TOO MANY REACTIONS'
      READ(10,'(12X,(3A12),(A10),(90A12))') (REACTION(J),J=1,NNUCD)
      I=1
 98   READ(10,*,END=99) E(I),(CSEC(I,J),J=1,NNUCD)
      I=I+1
      IF (I .GE. NE) STOP 'TOO MANY ENERGIES'
      GOTO 98
 99   CONTINUE
      CLOSE(10)
      IE=I-1

C CHECKING IF THE REQUIRED MT1 IS PRESENT IN THE FILE.XSC FILE
      J=0
      DO 15 I=1,NNUCD
         CALL RCTN(REACTION(I),MT)
         IF (MT .EQ. MT1) THEN
            J=1
            GOTO 16
         ENDIF
 15      CONTINUE
 16   CONTINUE
      IF (J .EQ. 0) THEN
C         WRITE(0,*) 'REACTION( MT=',MT1,') NOT FOUND IN '
C        1,FILE(1:L)//XSC
C         WRITE(6,*) 'REACTION( MT=',MT1,') NOT FOUND IN '
C        1,FILE(1:L)//XSC
         STOP         
      ENDIF


      DO 20 I=1,NNUCD
         WRITE(50,1000) REACTION(I),IE
            WRITE(50,1001) (E(J),CSEC(J,I),J=1,IE)
 20      CONTINUE
      CLOSE(50)

C  GENERATE SENSITIVITY FILE - UNIT 52

      OPEN(10,FILE=FILE(1:L)//INPSEN,STATUS='OLD',ERR=200)
      I=1
 97   READ(10,*,END=96) PN,PERTB(I),IA,IB,IC
c      write(0,*) PN,PERTB(I),IA,IB,IC
      CALL STRLEN(PN,LN)

      IF (IA .NE. '!') THEN
         CALL STRLEN(IA,LA)
         CALL STRLEN(IB,LB)
         PNAME(I)=PN(1:LN)//IA(1:LA)//IB(1:LB)
         IF (IC .NE. '!') THEN
            CALL STRLEN(IC,LC)
            PNAME(I)=PN(1:LN)//IA(1:LA)//IB(1:LB)//IC(1:LC)
         ENDIF
      ELSE
          PNAME(I)=PN
      ENDIF

      I=I+1
      IF (I .GE. NP) STOP 'TOO MANY PARAMETERS'
      GOTO 97
 96   CONTINUE
      CLOSE(10)
      NPARM=I-1


      DO 30 I=1,NPARM
         SW(I)=1.0
C         UN(I)=0.1
 30      CONTINUE

      DO 31 I=1,NPARM
         DO 32 J=1,NPARM
            W(I,J)=0.0
 32      CONTINUE
         W(I,I)=1.0
 31      CONTINUE

      WRITE(52,1003) NPARM
      WRITE(52,1004) (PNAME(I),I=1,NPARM)
      WRITE(52,1005) (SW(I),I=1,NPARM)
C      WRITE(52,1005) (UN(I),I=1,NPARM)
      WRITE(52,1005) (PERTB(I),I=1,NPARM)
      DO 40 I=1,NPARM
         WRITE(52,1006) (W(I,J),J=1,NPARM)
 40      CONTINUE

      OPEN(10,FILE=FILE(1:L)//MATSEN,STATUS='OLD',ERR=300)
      DO 50 I=1,NPARM
         READ(10,*)!FIRST LINE
         READ(10,'(13X,A6)') PN
         CALL STRLEN(PN,LT)
         TMP=PNAME(I)
c         write(0,*) TMP(1:LT),' ',PN(1:LT)
         IF (TMP(1:LT).NE.PN(1:LT)) STOP 'WRONG PARAMETER'
         READ(10,*)!THIRD LINE
         DO 51 J=1,IE
            READ(10,*) E(J),(SENS(I,J,K),K=1,NNUCD)
 51         CONTINUE
         READ(10,*)!EMPTY LINE
 50      CONTINUE
      CLOSE(10)

      DO 60 K=1,NNUCD
         WRITE(52,'(A12,13X,I5)') REACTION(K),IE
         DO 61 J=1,IE
            WRITE(52,1005) (SENS(I,J,K)*CSEC(J,K)/PERTB(I)/2.,I=1,NPARM)
 61         CONTINUE
 60      CONTINUE

      STOP
 100  WRITE(0,*) 'CROSS SECTION DATA FILE NOT FOUND: ',FILE(1:L)//XSC
      STOP
 200  WRITE(0,*) 'PARAMETER FILE NOT FOUND: ',FILE(1:L)//INPSEN
      STOP
 300  WRITE(0,*) 'SENSITIVITY FILE NOT FOUND: ',FILE(1:L)//MATSEN
      STOP
 1000 FORMAT(A12,31X,I5)
 1001 FORMAT(6(1PE11.4))
 1002 FORMAT(A6,1PE11.2,3(A4),I2)
 1003 FORMAT(25X,I5)
 1004 FORMAT(11A12)
 1005 FORMAT(11(1PE12.5))
 1006 FORMAT(20(F6.3))
      END

c$$$      SUBROUTINE RCTN(NAME,IO)
c$$$      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
c$$$      IMPLICIT INTEGER (I-N)
c$$$      PARAMETER (NRMAX=44)
c$$$      CHARACTER*12 LIST(NRMAX),NAME
c$$$      DIMENSION MT(NRMAX)
c$$$      DATA LIST/
c$$$     & '  Total     ', '  Elastic   ', '  (z,n)     ', '  (z,2nd)   ',
c$$$     & '  (z,2n)    ', '  (z,3n)    ', '  Fission   ', '  (z,f)     ',
c$$$     & '  (z,nf)    ', '  (z,2nf)   ', '  (z,na)    ', '  (z,n3a)   ',
c$$$     & '  (z,2na)   ', '  (z,3na)   ', '  (z,np)    ', '  (z,n2a)   ',
c$$$     & '  (z,2n2a)  ', '  (z,nd)    ', '  (z,nt)    ', '  (z,nHe3)  ',
c$$$     & '  (z,nd2a)  ', '  (z,nt2a)  ', '  (z,4n)    ', '  (z,3nf)   ',
c$$$     & '  (z,2np)   ', '  (z,3np)   ', '  (z,n2p)   ', '  (z,npa)   ',
c$$$     & '  (z,gamma) ', '  (z,p)     ', '  (z,d)     ', '  (z,t)     ',
c$$$     & '  (z,He3)   ', '  (z,a)     ', '  (z,2a)    ', '  (z,3a)    ',
c$$$     & '  (z,2p)    ', '  (z,pa)    ', '  (z,t2a)   ', '  (z,d2a)   ',
c$$$     & '  (z,pd)    ', '  (z,pt)    ', '  (z,da)    ', '  Reaction  '/
c$$$      DATA MT/
c$$$     11, 2, 4, 11, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 28, 29, 30,
c$$$     232, 33, 34, 35, 36, 37, 38, 41, 42, 44, 45, 102, 103, 104, 105,
c$$$     3106, 107, 108, 109, 111, 112, 113, 114, 115, 116, 117, 0/
c$$$
c$$$      IO=0
c$$$
c$$$      I=1
c$$$ 99   IF (NAME.EQ.LIST(I)) THEN
c$$$         IO=MT(I)
c$$$         GOTO 100
c$$$      ELSEIF (NAME.NE.LIST(I)) THEN
c$$$         I=I+1
c$$$         IF (I .GT. NRMAX) THEN
c$$$          IO=-1
c$$$          WRITE(0,*) 'MT NOT FOUND FOR',NAME
c$$$          GOTO 100
c$$$         ENDIF
c$$$         GOTO 99
c$$$      ENDIF
c$$$ 100  RETURN
c$$$      END

c$$$      SUBROUTINE STRLEN(ST,LS)
c$$$      INTEGER I,LS
c$$$      CHARACTER	ST*(*)
c$$$      I = LEN(ST)
c$$$      DO WHILE (ST(I:I) .EQ. ' ')
c$$$         I = I - 1
c$$$         IF (I.EQ.0) THEN
c$$$            WRITE(0,*) 'EMPTY STRING'
c$$$            GOTO 100
c$$$         ENDIF
c$$$      ENDDO
c$$$ 100  LS = I
c$$$      RETURN
c$$$      END
