C*********************************************************************
C*    PROGRAM   KALEND2 :                                            *
C*                             ORIGINAL VERSION    T. KAWANO  (LANL) *
C*                              REVISED VERSION    M.T. PIGNI (BNL)  *
C*                                                                   *
C*    X,Y       :  ENERGIES AND CROSS SECTIONS                       *
C*    W         :  DIAGONAL (REACTION) COVARIANCE MATRICES           *
C*    KALEND.INP:  CONTAINS MT AND MAT NUMBER                        *
C*    READENDF  :  READ ENDF.DAT TO FIND THE THRESHOLD FOR MT        *
C*    UNIT(16)  :  READ X,Y,W                                        *
C*                                                                   *
C*    RCTN      :  FIND MATCH BETWEEN NAME REACTION IN UNIT(16)      *
C*                 AND MT NUMBER                                     *
C*    UNIT(17)  :  WRITE CORRELATION MATRIX                          *
C*    UNIT(18)  :  WRITE PERCENTUAL ERROR                            *
C*    UNIT(20)  :  WRITE FILE SUCCESIVELY PROCESSED FOR MF=33        *
C*    UNIT(25)  :  WRITE FILE FOR PLOTTING A CORRELATION MATRIX      *
C*                 WITH A MESH DEFINED BY NN (DEFAULT NN=8)          *
C*                                                                   *
C*                                                                   *
C*    NOTE(1)   : THIS PROGRAM IS ASSOCIATED WITH THE BASH SCRIPT    *
C*                (KALEND2.SH) WICH GENERATES THE FILE 33 FOR        *
C*                DIAGONAL REACTION COVARIANCE MATRICES              *
C*                                                                   *
C*    NOTE(2)   : FILE 33 FOR OFF-DIAGONAL REACTIONS IS STILL IN     *
C*                DEVELOPMENT                                        *
C*********************************************************************
      PROGRAM KALEND2

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
      PARAMETER (NRMAX=20,NS=200,EV=1.D-6)
      DIMENSION X(NS),Y(NS),W(NS,NS)
      DIMENSION S(NS),E(NS),D(NS),V(NS,NS),C(NS,NS)
      CHARACTER*12 REACTION
      LOGICAL RES

      RES=.FALSE.

      LS=1
      LB=5
      NL=1
      NI=1

      OPEN(5, FILE='KALEND.INP', STATUS='OLD')
      READ(5,*) MT1,MAT1
      CLOSE(5)

      CALL READENDF(MT1, MAT1, ZA, AWR, XMF1, XLFS1, ETH, RES)
      ETH=ETH*EV

      
 99   READ(16,1900,END=101) NENRG,REACTION
      READ(16,2010)(X(I),I=1,NENRG)
      READ(16,2010)(Y(I),I=1,NENRG)
      DO I=1,NENRG
          READ(16,2010)(W(I,J),J=1,NENRG) 
      ENDDO
      MT=0
      IF (MT1.EQ.-111) THEN
         WRITE(0,*) 'NO MT NUMBER IN ENDF FILE!'
         STOP
      ENDIF
      CALL RCTN(REACTION,MT)
      IF (MT.EQ.MT1) GOTO 100
      GOTO 99
 101     PAUSE 'NO COVARIANCE MATRIX FOUND IN FORT.16'
 100  CONTINUE
      
      DO 20 I=1,NS
         D(I)=0.0
         E(I)=0.0
         S(I)=0.0
         DO 30 J=1,NS
            V(I,J)=0.0
            C(I,J)=0.0
 30      CONTINUE
 20   CONTINUE
C STORE THE ZERO ENERGY (MEV) IN THE FIRST ELEMENT
      K = 1
      E(K)  = 1E-11
      S(K)  = 0.0
      V(K,K)= 0.0

C IF THRESHOLD REACTION, LIKE (N,2N), ADD THE THRESHOLD ENERGY,
C ZERO CROSS SECTION, AND A DEFAULT ERROR OF 50%. 

      IF (ETH.GT.0.0 .AND. .NOT.RES) THEN
         K = K+1
         E(K)  = ETH
         S(K)  = 0.0
         V(K,K)= 0.25
      ENDIF

C FIND THE FIRST ENERGY THAT IS HIGHER THAN ETH

      ISKIP = 0
      DO I=1,NENRG
         IF (X(I) .GT. ETH) THEN
            ISKIP = I-1
            GOTO 10
         ENDIF
      ENDDO
 10   CONTINUE

C CHECK THE ELEMENT OF S, WHICH IS JUST ABOVE ETH,
C AND ELIMINATE IT IF IT IS TOO SMALL (LESS THAN 1 MICROBARNS)

      IF (S(ISKIP+1).LE.1E-03) THEN
         ISKIP = ISKIP+1
      ENDIF

C NOW THE NUMBER OF ENERGY POINTS, NE, IS NENRG+K-ISKIP
C K=1 FOR MT=1,102 ETC.
C K=2 FOR MT=16,17 ETC.

      NE = NENRG+K-ISKIP
      
      DO 50 I=K+1,NE
         E(I)=X(I-K+ISKIP)
         S(I)=Y(I-K+ISKIP)
         DO 40 J=K+1,NE
            V(I,J)=W(I-K+ISKIP,J-K+ISKIP)
 40      CONTINUE
 50   CONTINUE

C REPLACE THE SECOND ELEMENTS OF E AND S, IF MT=1,102 ETC

      IF ( RES ) THEN
         E(K+1) = ETH
         S(K+1) = Y(ISKIP+1)
      ENDIF
C CORRELATION MATRIX LIMITED TO UNCERTAINTIES OF 99%

      DO I=K+1,NE
         D(I)=SQRT(V(I,I))
      ENDDO

      DO I=K+1,NE
         DO J=K+1,NE
            C(I,J)=V(I,J)/D(I)/D(J)
         ENDDO
         IF (D(I)/S(I) .GT. 1.00) THEN
            D(I)=S(I)*.99
         ENDIF
      ENDDO
C NEW COVARIANCE MATRIX
      DO I=K+1,NE
         DO J=K+1,NE
            V(I,J)=C(I,J)*D(I)*D(J)
         ENDDO
      ENDDO
      DO I=1,NE
       DO J=I,NE
         W(I,J)=V(I,J)
         W(J,I)=W(I,J)
       ENDDO
      ENDDO
C COVARIANCE MATRIX NORMALIZED TO CROSS SECTIONS
      DO I=K+1,NE
         DO J=K+1,NE
            IF(S(I).EQ.0.0 .OR. S(J).EQ.0.0) THEN
               V(I,J)= 0.0
C            ELSE IF(S(I).LT.0.D0 .OR. S(J).LT.0.D0) THEN
C               WRITE(0,*) 'CROSS SECTIONS < 0 !!!!'
            ELSE
               V(I,J) = V(I,J)/S(I)/S(J)
            ENDIF
         ENDDO
      ENDDO

      NT = NE*(NE+1)/2
      WRITE(20,2020) ZA,AWR,0,MTL,0,NL
C     WRITE(20,2020) XMF1,XLFS1,MAT1,MT1,NC,NI
      WRITE(20,2020) XMF1,XLFS1,0,MT1,NC,NI
      WRITE(20,2020) 0.0,0.0,LS,LB,NT,NE
      WRITE(20,2030)(E(I)/EV,I=1,NE),((V(I,J),J=I,NE-1),I=1,NE-1)
      DO I=1,NE
         WRITE(18,2040) E(I),SQRT(V(I,I))*100.0
         WRITE(0,2040) E(I),SQRT(V(I,I))*100.0
         DO J=1,NE
            WRITE(17,2040) E(I),E(J),V(I,J)
         ENDDO
         WRITE(17,*)
      ENDDO
      WRITE(18,*)

C NN DEFINES THE MESH IN PLOTTING - HIGHER NN MAKES BIGGER (IN SIZE) PLOTS.
      NN=8

      OPEN(UNIT=25,FILE='corrplot.dat',STATUS='UNKNOWN')

      DO 70 I=K,NE-1
         EI=E(I)
         DEI=(E(I+1)-E(I))/NN
         DO 71 IP=0,NN-1
            DO 72 J=K,NE-1
               EJ=E(J)
               DEJ=(E(J+1)-E(J))/NN
               DO 73 JP=0,NN-1
                  IF(W(I,I).GT.0.D0.AND.W(J,J).GT.0.D0) THEN
                     WRITE(25,2040)
     1  EI+IP*DEI,EJ+JP*DEJ,W(I,J)/DSQRT(W(I,I)*W(J,J))
                  ELSE
                    WRITE(25,2040)
     1  EI+IP*DEI,EJ+JP*DEJ,0.D0
                  ENDIF
 73               CONTINUE
 72            CONTINUE
               WRITE(25,*)
 71         CONTINUE
 70      CONTINUE
         CLOSE(25)

 1900 FORMAT(I5,43X,A12)
 2000 FORMAT(I5)
 2010 FORMAT(6E12.5)
 2020 FORMAT(2(1PE13.6),4I11)
 2030 FORMAT(6(1PE13.6))
 2040 FORMAT(3(1X,1PE13.6))
 3000 STOP
      END
C
      SUBROUTINE RCTN(NAME,IO) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
      PARAMETER (NRMAX=20)
      CHARACTER*12 LIST(NRMAX),NAME
      DIMENSION MT(NRMAX)
      DATA LIST/
     & '  Total     ', '  Elastic   ', '  Reaction  ', '  Fission   ',
     & '  (z,gamma) ', '  (z,n)     ', '  (z,2n)    ', '  (z,3n)    ',
     & '  (z,p)     ', '  (z,np)    ', '  (z,2np)   ', '  (z,3np)   ',
     & '  (z,a)     ', '  (z,na)    ', '  (z,2na)   ', '  (z,3na)   ',
     & '  (z,pa)    ', '  (z,npa)   ', '  (z,2npa)  ', '  (z,3npa)  '/
      DATA MT/
     11, 2, 0, 18, 102, 4, 16, 17, 103, 28, 41, 42, 107, 22, 24, 25, 
     2112, 45, 46, 47/    

      IO=0

      I=1
 99   IF (NAME.EQ.LIST(I)) THEN
         IO=MT(I)
         GOTO 100
      ELSEIF (NAME.NE.LIST(I)) THEN
         I=I+1
         GOTO 99
      ENDIF
 100  RETURN
      END
C
      SUBROUTINE READENDF(MT1, MAT1, ZA, AWR, XMF1, XLFS1, ETH, RES)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL RES
      CHARACTER*66 C66
      DATA LIN /12/
C THE TWO QUANTITIES BELOW ARE SET TO 0 SINCE WE ONLY TREAT COVARIANCES
C WITHIN ONE REACTION OF A SINGLE MATERIAL
      XMF1  = 0.0
      XLFS1 = 0.0

      IF (MT1.EQ.1 .OR. MT1.EQ.2 .OR. MT1.EQ.102 .OR. MT1.EQ.18) THEN
         RES = .TRUE.
      ELSE
         RES = .FALSE.
      ENDIF

      OPEN(LIN, FILE='ENDF.DAT', STATUS='UNKNOWN')
      READ (LIN,940) C66,MAT,MF,MT,NC !TOP LINE
  10  READ (LIN,940) C66,MAT,MF,MT,NC !LINE 1
      IF (MAT.LT.0) STOP 'MAT MISSING IN THE ENDF FILE'
C IN CASE MAT1=0 USE THE FIRST MATERIAL IN THE FILE      
      IF (MAT1.EQ.0) MAT1 = MAT
      IF (MAT.NE.MAT1) GOTO 10
      READ (C66,924) ZA, AWR
  20  READ (LIN,940,END=300) C66,MAT,MF,MT,NC
C READ UPPER BOUNDARY OF  THE  RESOLVED  RESONACE  REGION TO BE USED  AS
C A THRESHOLD FOR THE FAST NEUTRON COVARIANCES IN CASE OF TOTAL, ELASTIC 
C CAPTURE, AND FISSION TO BE COMMENTED FOR  RESONANCE CALCULATIONS  (DR)
      IF(RES .AND. MT.EQ.151) THEN
         READ (LIN,940) C66,MAT,MF,MT,NC 
         READ (LIN,924) E1,ETH
         RETURN
      ENDIF
      
      IF (MF.NE.3 .OR. MT.NE.MT1) GOTO 20
      READ (LIN,940) C66,MAT,MF,MT,NC 
      READ (LIN,940) C66,MAT,MF,MT,NC 
  30  READ (LIN,940) C66,MAT,MF,MT,NC 
      IF(MF.NE.3.AND.MT.NE.MT1) STOP 'MT MISSING OR LACKS NON-ZERO XS'
      READ (C66,925) E1, XSEC1, E2, XSEC2, E3, XSEC3 
      IF(XSEC1.EQ.0 .AND. XSEC2.GT.0) THEN
         ETH = E1
         RETURN
      ELSEIF(XSEC2.EQ.0 .AND. XSEC3.GT.0) THEN
         ETH = E2
         RETURN
      ELSE
         ETH = E3
         RETURN
      ENDIF
 
 940  FORMAT(A66,I4,I2,I3,I5)
 924  FORMAT(2F11.0,4I11)
 925  FORMAT(6F11.0)
 930  FORMAT(44x,I11)
 300  WRITE(0,*) 'MT NUMBER NOT FOUND IN ENDF FILE!'
      MT1=-111
      RETURN
      END
