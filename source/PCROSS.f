      SUBROUTINE PCRoss(SIGr)

C     FORMAL PARAMETERS:
C     SIGR = REACTION CROSS SECTION
C                ---- OUTPUT -------
C     CROss(NDEjc+1) = CHANNEL EMISSION CROSS SECTION
C     FR = DEPLETION FACTOR FOR PREEQUILIBRIUM
C     SPEc(NDEjc+1,NDEx) = Channel emission spectra, +1 means gamma channel
C
C     Some variables:
C     AC,ZC = MASS AND ATOMIC NUMBER OF THE COMPOUND NUCLEUS
C     EC CN EXCITATION ENERGY, LEVEL DENSITY & PAIRING FOR COMP.NUC.
C     GC,PC LEVEL DENSITY & PAIRING FOR COMP.NUC.
C     G(NEJc),PAIR(NEJc) THE SAME FOR RESIDUAL NUCLEI AFTER EMISSION
C     KME = MEAN FREE PATH PARAMETER
C
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'

      REAL*8 KME,C1,PI26,EPS,CNHEQ,SIGR
      INTEGER*4 PMAX
      PARAMETER(C1=3.6824121D17,EPS=1.D-4,KME=1.3d0,
     1          PI26=1.6449340107D0,CNHEQ=1.4D0,PMAX=50)
C
C     REAL VARIABLES
C     SCALARS
C
      REAL*8 EC,GC,PC,FR
      REAL*8 SUMt,CME,HLP1,EEE,FF1,FF2,FF3,WB,WDA,SG,ER,FF,EMIs
C
C     REAL MATRIX
C
C     FORMAL PARAMETERS
      REAL*8 CROss(NDEjc+1),G(NDEjc+1),PAIr(NDEjc+1)
      REAL*8 EM(PMAX),WE(NDEjc+1,PMAx,NDEx),SPEc(NDEjc+1,NDEx),
     >       L(NDEjc+1,PMAX),FLM(4,4),R(4,PMAX,NDEjc),
     >       EINt(NDEx),FINt(NDEx),EMAxi,EMIni
C
C     REAL FUNCTIONS
C
      REAL*8 DENSW
C
C     INTEGER VARIABLES
C
      INTEGER*4 AC,AO,AR,P,HH,H1,I,IENERG,NHEQ,ICON,ICON1,J,
     1 ICON3,ZC,ZR,INIC,AT,ZT,AP,ZP,IHMAX,Ien
      INTEGER*2 IEMin(NDEjc+1),IEMax(NDEjc+1),NUres(NDEjc+1)

C======================================================================
      COMMON/CALC5/L,NHEq
C
C     NPRoject - projectile nejc number
C     NTArget - target nnuc number

C     INITIALIZATION
      CME=KME/1.4D21

      FR=0.d0
      SUMt=0.d0
      DO I=1,NDEjc+1
       CROss(I)=0.
      ENDDO
      DO HH=1,PMAx
      EM(HH)=0.d0
      ENDDO
      EM(1)=1.D0

C     Excitation energy (MeV)
      EC  = EXCn              
C     Compound nucleus quantities
      AC=A(1)
      ZC=Z(1)
      GC=ROPar(1,1)/PI26
      IF(GC.eq.0.d0) THEN
      GC=AC/8./PI26
      PC=0.
      ELSE
      PC=ROPar(3,1)
      ENDIF
C     NEJcm + 1 correspond to the compund gamma emitting nucleus
      G(NEJcm+1)=GC
      PAIR(NEJcm+1)=PC
C
C    EXCITON EQUILIBRIUM NUMBER NHEq
C
      NHEq=MIN(PMAx, NINT(SQRT(1.4*GC*EC)) ) + 1

C     ZERO ARRAY INITIALIZATION
      DO NEJc=1,NDEJc+1
      IEMin(NEJc)=2
      IEMax(NEJc)=NDEx
      NURes(NEJc)=1
        DO IENERG=1,NDEx
          SPEC(NEJc,IENERG)=0.d0
          DO HH=1,PMAX
            WE(NEJc,HH,IENERG)=0.d0
       ENDDO
        ENDDO
        DO HH=1,PMAX
          L(NEJc,HH)=0.d0
      ENDDO
      ENDDO

      AP=AEJc(NPRoject)
      AT=AC-AP
      ZP=ZEJc(NPRoject)
      ZT=ZC-ZP

      WRITE(6,3)
    3 FORMAT(/' ',91('+')/)
      WRITE(6,1)
    1 FORMAT(5X,' PREEQUILIBRIUM DECAY AND DIRECT REACTIONS'/)
      WRITE(6,2)EC,KME
    2 FORMAT(/
     &1X,'EXCITATION ENERGY OF THE COMPOUND SYSTEM=',F7.3,'(MEV)'/
     &1X,'MEAN FREE PATH PARAMETER=',F4.2)

C     NEJcm is the maximum number of particles emitted
      DO NEJc = 1, NEJcm
        AR = AC - AEJc(NEJc)
        ZR = ZC - ZEJc(NEJc)
        izares = INT(1000*ZR + AR)
        CALL WHERE(izares, NNUr, iloc)
        IF(iloc.EQ.1)THEN
          WRITE(6, '('' NO LOCATION ASCRIBED TO NUCLEUS '')')izares
          STOP 'EXCITON'
        ENDIF
      NURes(NEJc)=NNUr
        G(NEJc)=ROPar(1,NNUr)/PI26
      IF(G(NEJc).eq.0.) THEN
      G(NEJc)=AR/8./PI26
      PAIr(NEJc)=0.
      ELSE
      PAIr(NEJc)=ROPar(3,NNUr)
      ENDIF
C       Maximum and minimum energy bin
      DO IENerg=2,NEX(NNUr)
          EEE=ETL(IENerg,NEJc,NNUr)
      IF(EMAx(NNUr).GT.EEE) IEMax(NEJc)=IENerg
      IF(EEE.GT.0.) THEN 
      SG=SIGabs(IENerg,NEJc,NNUr)
      IF (SG.LT.1.D-6) IEMin(NEJc)=IENerg 
      ENDIF
      ENDDO
100   ENDDO

      DO IENerg=2,NDEx
      EEE=DE*(IENerg-1)
      IF(EC.GT.EEE) IEMax(NEJcm+1)=IENerg
      ENDDO

      izares = INT(1000*ZC + AC)
      CALL WHERE(izares, nnur, iloc)
      NURes(NEJcm+1)=nnur
      BETA2=0.

      CALL RQFACT(NHEq,R)

C***************************************************************
C     EMISSION RATES CALCULATIONS FOLLOWS
C
C     PRIMARY PARTICLE LOOP
C
C
      DO NEJc = 1, NEJcm+1

      NNUr=NURes(NEJc) 

      IF(NEJc.ne.NEJcm+1) then
       AO = AEJc(NEJc)
       ZO = ZEJc(NEJc)
       AR = AC - AO
       ZR = ZC - ZO

            IF (AO.EQ.1 .AND. ZO.EQ.0)  FF1=2.0173*AR/(AR+1.0087)  ! n
            IF (AO.EQ.1 .AND. ZO.EQ.1)  FF1=2.0145*AR/(AR+1.0073)  !	p
            IF (AO.EQ.4 .AND. ZO.EQ.2)  FF1=4.0015*AR/(AR+4.0015)  ! alpha
            IF (AO.EQ.3 .AND. ZO.EQ.1)  FF1=6.0298*AR/(AR+3.0149)  ! t
            IF (AO.EQ.2 .AND. ZO.EQ.1)  FF1=6.0408*AR/(AR+2.0136)  ! d

      ELSE

         AR=AC 
         ZR=ZC
         AO=0
         ZO=0
          FF1=1.d0/AMUmev
         
         ENDIF

          ICON1=0
          INIC=0
C
C         PARTICLE-HOLE LOOP
C
        DO 107 H1=1,NHEq
            
        ICON=0
            HH=H1-1
            IF(HH.EQ.0.AND.NEJc.NE.NEJcm+1) GOTO 107

            P=HH+AP
            FF2=DENSW(GC,PC,P,HH,EC)
            IF(FF2.EQ.0.) GOTO 107

C
C           PRIMARY ENERGY CICLE
C
        DO IENERG=IEMin(NEJc),IEMax(NEJc)
C
              IF(nejc.NE.NEJcm+1) THEN         
               EEE=ETL(IENERG,NEJc,NNUr)
        ELSE
               EEE=DE*(IENERG-1)  
        ENDIF

       IF(EEE.EQ.0.) GOTO 355
C             Inverse cross section
        IF(NEJc.NE.NEJcm+1) THEN
         SG=SIGabs(IENERG,NEJc,NNUr)
      ELSE
            AAT=AC
           AZT=ZC
           SG=SGAM(AAT,AZT,EEE,BETA2)
      ENDIF

      IF(SG.EQ.0.) GOTO 355

              ER=EMAX(NNUr) - EEE

              IF(ER.LE.EPS) GOTO 355
       HLP1=0.d0
C
C             PREEQUILIBRIUM neutron and proton EMISSION CALCULATION
C
              IF(NEJc.LE.2) THEN              
            WDA=DENSW(G(NEJc),PAIR(NEJc),P-AO,HH,ER)
           IF(WDA.GT.0.) HLP1=WDA*R(1,H1,NEJc) 
C
C             PREEQUILIBRIUM CLUSTER EMISSION CALCULATION
C
              ELSEIF(NEJc.GT.2 .and. NEJc.LT.NEJcm+1) THEN
                CALL Preformation(FLM,EEE)
                DO J=1,AO
                  WDA=DENSW(G(NEJc),PAIR(NEJc),P-J,HH,ER)
                  IF(WDA.GT.0.) 
     &            HLP1=HLP1+FLM(AO,J)*WDA*R(J,H1,NEJc) 
                ENDDO
C----------------------------------------------------------
C             PREEQUILIBRIUM GAMMA EMISSION CALCULATION
              ELSEIF(NEJc.eq.NEJcm+1) THEN
                HLP1=DENSW(GC,PC,P,HH,ER)*DBLE(P+HH)/(DBLE(P+HH)+EEE*GC)
                IF(HH.GE.1) HLP1=HLP1+
     &            GC*EEE*DENSW(GC,PC,P-1,HH-1,ER)/(DBLE(P+HH-2)+EEE*GC)
              ENDIF
C----------------------------------------------------------
              IF(HLP1.EQ.0.) GOTO 355
              FF3=HLP1*EEE*SG
              IF(NEJc.eq.NEJcm+1) FF3=FF3*EEE
              FF=FF1*FF3/FF2
              WE(NEJc,H1,IENerg)=FF*C1
              ICON=ICON+1
              ICON3=IENerg
              EINt(ICON)=EEE
              FINt(ICON)=FF

 355        ENDDO
C
C           INTEGRATION PROCEDURE #1 (EMISSION RATES)
C
            IF(ICON.GT.0) THEN
           IF(NEJc.ne.NEJcm+1) then
           EMAxi=EMAx(NNUr)
          IF(ETL(ICON3+1,NEJc,NNUr).LT.EMAxi) 
     &    EMAxi=ETL(ICON3+1,NEJc,NNUr)
           EMIni=0.d0
           IF(IEMin(NEJc).GT.2) EMIni=ETL(IEMin(NEJc)-1,NEJc,NNUr)
             HLP1=  FINt(1)*(EINt(1)-EMIni) +
     &             FINt(ICON)*(EMAxi-EINt(ICON))
           ELSE
           EMAxi=EMAx(NNUr)
           EMIni=0.d0
             HLP1=  FINt(1)*(EINt(1)-EMIni) +
     &            FINt(ICON)*(EMAxi-EINt(ICON))
           ENDIF    
           DO IEN=2,ICON
                HLP1 = HLP1 + (EINt(IEN)-EINt(IEN-1))*
     &                  (FINt(IEN-1)+FINt(IEN))
           ENDDO 
              L(NEJc,H1)=0.5d0*HLP1*C1
            ENDIF

 107      CONTINUE
C     END OF PARTICLE-HOLE LOOP

      ENDDO
C     END OF EMIITED PARTICLE LOOP

C****************************************************************
C     TRANSITION RATES CALCULATIONS FOLLOWS
C
      CALL TRANSIT(EC,GC,AP,NHEq)
C====================================================================
C
C     MASTER EQUATION SOLUTION
C
      CALL RESOL(EM,IHMax,AP,CME)
C=====================================================================
C
C     PARTICLE LOOP FOR EMISSION SPECTRA CALCULATIONS
C

      SUMt=0.D0
      DO NEJc = 1, NEJcm+1
      HLP1 = 0.d0
          DO IENerg=IEMin(NEJc),IEMax(NEJc)
            EMIs=0.D0
            DO H1=1,IHMax
              WB=WE(NEJc,H1,IENerg) 
              IF(WB.GT.0.) EMIs=EMIs+WB*EM(H1)
            ENDDO  
            SPEc(NEJc,IENerg)=SIGr*EMIs
        HLP1=HLP1+SPEc(NEJc,IENerg)*DE
       ENDDO
          SUMt=SUMt+HLP1
          CROss(NEJc)=HLP1
      ENDDO
C     Only for printing
      IF(IOUt.GE.3) THEN 
      WRITE(6,*) '==========================='

        DO NEJc = 1, NEJcm+1
       NNUr=NUres(NEJc) 
      IF(NEJc.LT.NEJcm+1) THEN
        WRITE(6,'(2X,A4,'' emission cross section'',G12.5,'' mb'')')
     &    SYMbe(NEJc),CROss(NEJc)
          ELSE
        WRITE(6,'(2X,A5,'' emission cross section'',G12.5,'' mb'')')
     &    'gamma',CROss(NEJc)
       ENDIF
          DO IENerg=IEMin(NEJc),IEMax(NEJc)
        IF(NEJc.NE.NEJcm+1) THEN          
          EEE=ETL(IENerg,NEJc,NNUr)
        ELSE
          EEE=DE*(IENerg-1)
        ENDIF
            WRITE(6,7) EEE,SPEc(NEJc,IENerg)
      ENDDO
      WRITE(6,*) '==========================='
      ENDDO
      ENDIF
      FR=SUMt/SIGr

      WRITE(6,4) SUMt, FR, (CROSS(NEJc),NEJc=1,NEJcm+1)

  4   FORMAT(/
     &1X,'PREEQUILIBRIUM TOTAL CROSS SECTION   =',F8.2,'(mBARN)'/
     &1X,'PREEQUILIBRIUM FRACTION              =',F8.2/
     &1X,'PREEQUILIBRIUM CROSS SECTIONS =',7(F8.2,1x),'(mBARN)'/
     &/)
  5   FORMAT(/
     &1X,'COMPOUND AC=',I3,' ZC=',I3,/
     &1X,'RESIDUAL AR=',I3,' ZR=',I3,//)
  6   FORMAT(1x,F7.2,2x,D12.6)
  7   FORMAT(1x,F7.2,2x,7(D12.6,1x))
C
      RETURN
      END
C
      SUBROUTINE RQFACT(HEQ,R)
C
C     INDC(CPR)-014 and references there in
C
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
      INTEGER*4 PMAX
      PARAMETER (PMAX=50)
      REAL*8 R(4,PMAX,NDEjc)
      REAL*8 FA,LFA
      INTEGER*4 AT,ZT,AP,ZP,Ab,Zb,Nb,AC,ZC
      REAL*8 TA,TZ,TN,ZZZ,UUU,S1,S2,F1,F2,F21,F22,F23,F24
      INTEGER*4 HEQ,I,H1,HHH,P,L,M,MINJ,MAXJ,J,NP,NT,J1

      COMMON/FACT/FA(2*PMAX+3),LFA(2*PMAX+3)
C
C     FACTORIAL CALCULATION
C
      LFA(1)=0.
      LFA(2)=0.
      LFA(3)=0.
      FA(1)=1.
      FA(2)=1.
      FA(3)=1.
      DO I=4,2*PMAX+3
       LFA(I)=ALOG(FLOAT(I-3))+LFA(I-1)
       FA(I)=(I-3)*FA(I-1)
      ENDDO
C
      DO NEJc=1,NEJcm
        DO H1=1,PMAX
      DO L=1,4 
            R(L,H1,NEJc)=1.D0
       ENDDO
      ENDDO
      ENDDO

      AC=A(1)
      ZC=Z(1)
      AP=AEJc(NPRoject)
      AT=AC-AP
      ZP=ZEJc(NPRoject)
      ZT=ZC-ZP

      NT=AT-ZT
      NP=AP-ZP
      TN=NT
      TA=AT
      TZ=ZT
      ZZZ=TZ/TA
      UUU=1.d0 - ZZZ
C
C     Q-FACTOR CALCULATION FOR PARTICLE EMISSION
C

      DO NEJc=1,NEJcm

      Ab = AEJc(NEJc)
      Zb = ZEJc(NEJc)
      Nb = Ab - Zb

      DO L=1,Ab

      DO H1=1,HEQ

         HHH=H1-1
         P=HHH+AP
         IF(P.LT.L) GOTO 500
         M=Ab-L
         MINJ=MAX(0,L-Nb)
         MAXJ=MIN(L,Zb)
         S1=0.D0

         DO I=0,HHH

           F1=ZZZ**I*UUU**(HHH-I)*
     &    EXP(LFA(HHH+3)-LFA(HHH-I+3)-LFA(I+3))

           S2=0.D0
           DO J=MINJ,MAXJ
            F21=1.D0
            IF(J-1.GE.0) THEN
               DO J1=0,J-1
                 F21=F21*FLOAT(ZP+I-J1)
               ENDDO
               F21=F21/FA(J+3)
            ENDIF
            F22=1.D0
            IF(L-J-1.GE.0) THEN
               DO J1=0,L-J-1
                 F22=F22*FLOAT(NP+HHH-I-J1)
               ENDDO
               F22=F22/FA(L-J+3)
            ENDIF
            F23=1.D0
            IF(Zb-J-1.GE.0) THEN
               DO J1=0,Zb-J-1
                 F23=F23*FLOAT(ZT-I-J1)
               ENDDO
               F23=F23/FA(Zb-J+3)
            ENDIF
            F24=1.D0
            IF(Nb-L+J-1.GE.0) THEN
               DO J1=0,2-L+J-1
                 F24=F24*FLOAT(NT-HHH+I-J1)
               ENDDO
               F24=F24/FA(Nb-L+J+3)
            ENDIF
            S2=F21*F22*F23*F24+S2

           ENDDO

           S1=S1+F1*S2

         ENDDO

         F2=1.D0
         DO J=0,M-1
           F2=F2*(AT-HHH-J)
         ENDDO

         F2=EXP( LFA(L+3)+LFA(M+3)+LFA(P-L+3)-LFA(P+3) +
     &         LFA(Ab+3)-LFA(Zb+3)-LFA(Nb+3) )/F2

         R(L,H1,NEJc)=S1*F2*(TA/TZ)**Zb*(TA/TN)**Nb

500    ENDDO

      ENDDO

      ENDDO

      DO NEJc=1,NEJcm
      Ab = AEJc(NEJc)
      DO H1=1,HEQ
       DO L=1,Ab
        R(L,H1,NEJc)=R(L,H1,NEJc)/R(L,HEQ,NEJc)
      ENDDO
        ENDDO
      ENDDO
     
      RETURN
      END
C
      SUBROUTINE TRANSIT(E,GC,AP,NHEq)
      IMPLICIT LOGICAL*1 (A-Z)
      INTEGER*4 PMAX,NHEq
      PARAMETER (PMAX=50)
      REAL*8 E,GC
      REAL*8 LP(PMAX),LM(PMAX),A0P,UC0
      INTEGER*4 I,NMAX,H1,P,H,N,AP
      COMMON/CALC3/LP,LM,NMAX
C****************************************************************
C     TRANSITION RATES CALCULATIONS FOLLOWS
C
      DO 364 I=1,PMAX
      LP(I)=0.
 364  LM(I)=0.
      NMAx=NHEq
C     DO 365 H1=1,PMAX
      DO 365 H1=1,NHEq
      H=H1-1
      P=H+AP
      N=P+H
      A0P=FLOAT(P*(P-1)+H*(H-1))*.25
      UC0=E-A0P/GC
      IF(UC0.LE.0.D0) THEN
      NMAX=H1-1
      RETURN
      ENDIF
C
C     3/8 is coming from the Gupta paper
C
      LM(H1)=FLOAT(P*H)*FLOAT((N-2)*(N-1))/(GC*GC*UC0)*(3./8.)
      LP(H1)=UC0*(3./8.)
 365  CONTINUE
 102  CONTINUE
C
C END OF TRANSITION RATES CALCULATIONS [ NORMALIZED TO CME ]
C*****************************************************************
      RETURN
      END
C
      SUBROUTINE RESOL(EM,IH2,AP,CME)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'

      INTEGER*4 PMAX
      PARAMETER (PMAX=50)
C
C     H1=1+(NEXCIT-1)/2
C
C     REAL VARIABLES
C     SCALARS
      REAL*8 CME,HLP1,HLP2,HLP4
C     MATRIX
      REAL*8 L(NDEjc+1,PMAX),LM(PMAX),LN(PMAX),B(PMAX),LP(PMAX)
      REAL*8 C(PMAX),E(PMAX),EM(PMAX),LS(PMAX)
C
C     INTEGER VARIABLES
C
      INTEGER*4 HHH,H1,I,IH2,NMAX,AP,N,IJ,NHEQ
C======================================================================
      COMMON/CALC3/LP,LM,NMAX
      COMMON/CALC5/L,NHEq
C======================================================================
      DO 364 I=1,PMAX
364   LN(I)=0.
C     N=NMAX-1
      N=NMAX
      IF(N.GT.PMAX) N=PMAX
      DO 381 H1=1,N
      DO 382 I=1,NEJcm+1
 382  LN(H1)=LN(H1)+L(I,H1)
C-----------------------------------------
C NEVER COME BACK ASUMPTION IS ASSUMED
C
      LM(H1)=LM(H1)*1.D-7
C-----------------------------------------
 381  LS(H1)=CME*LN(H1)+LP(H1)+LM(H1)
      WRITE (6,524)
      DO H1=1,N
        HHH=H1-1
        IJ=2*HHH+AP
        HLP1=LP(H1)/CME
        HLP2=LM(H1)/CME
        HLP4=HLP1+HLP2
        WRITE (6,526) IJ,(L(I,H1),I=1,4),LN(H1),HLP1,HLP2,HLP4
      ENDDO
C
C     INITIAL CONDITIONS
C
      DO 1111 I=1,PMAX
      B(I)=EM(I)
1111  EM(I)=0.
C
      C(1)=0.D0
      DO 601 H1=1,N-1
      C(H1+1)=-LP(H1)
601   E(H1)=-LM(H1+1)

      CALL MASTER(C,LS,E,B,N,PMAX)
C--------------------------------------------------------------------
C     NEVER COME BACK ASSUMPTION(ONLY PE-CONTRIBUTION)
C
      DO 1211 HHH=NHEQ,N
 1211 B(HHH)=0.
C--------------------------------------------------------------------
      DO 1113 H1=1,N
      EM(H1)=B(H1)*CME
 1113 IF (EM(H1).NE.0.) IH2=H1
      WRITE (6,528)
      IH2=MIN(IH2,NHEq)
      DO 628 H1=1,IH2
        HHH=2*(H1-1)+AP
 628  WRITE (6,529) HHH,EM(H1)
      RETURN
  524 FORMAT(/2X,'N', 8X,'E  M  I  S  S  I  O  N     R  A  T  E  S',
     2 '       T R A N S I T I O N   R A T E S',//,
     3 5X,'NEUTRONS   PROTONS    ALPHAS    GAMMAS    TOTAL',
     4 '        PLUS     MINUS     TOTAL'/)

  526 FORMAT(I3,5E10.3,2X,3E10.3)
  528 FORMAT (/3X,'TIME INTEGRALS OF TAU(N)'/
     &         3X,'     UP TO Nequil       '/)
  529 FORMAT(I5,2E14.3)
      END
C
      SUBROUTINE MASTER(SUBD,DIAG,SUPERD,B,N,NDIM)
      IMPLICIT LOGICAL (A-Z)
      INTEGER*4 N,NDIM
      REAL*8 SUBD(NDIM),DIAG(NDIM),SUPERD(NDIM),B(NDIM)
      INTEGER*4 K,KB,KP1,NM1,NM2
      REAL*8 T
C
        SUBD(1) = DIAG(1)
        NM1 = N - 1
        IF (NM1 .LT. 1) GO TO 40
           DIAG(1) = SUPERD(1)
           SUPERD(1) = 0.0D0
           SUPERD(N) = 0.0D0
C
           DO 30 K = 1, NM1
              KP1 = K + 1
              IF (DABS(SUBD(KP1)) .GE. DABS(SUBD(K))) THEN
                T = SUBD(KP1)
                SUBD(KP1) = SUBD(K)
                SUBD(K) = T
                T = DIAG(KP1)
                DIAG(KP1) = DIAG(K)
                DIAG(K) = T
                T = SUPERD(KP1)
                SUPERD(KP1) = SUPERD(K)
                SUPERD(K) = T
                T = B(KP1)
                B(KP1) = B(K)
                B(K) = T
              ENDIF
          IF (SUBD(K) .EQ. 0.0D0)
     1     STOP ' DIAGONAL ELEMENT = 0 in SOLUTION of MASTER equation'
              T = -SUBD(KP1)/SUBD(K)
              SUBD(KP1) = DIAG(KP1) + T*DIAG(K)
              DIAG(KP1) = SUPERD(KP1) + T*SUPERD(K)
              SUPERD(KP1) = 0.0D0
   30         B(KP1) = B(KP1) + T*B(K)
   40   CONTINUE
        IF (SUBD(N) .EQ. 0.0D0) GO TO 100
C
C          BACK ITERATION
C
           NM2 = N - 2
           B(N) = B(N)/SUBD(N)
           IF (N .GT. 1) THEN
              B(NM1) = (B(NM1) - DIAG(NM1)*B(N))/SUBD(NM1)
              IF (NM2 .GE. 1) THEN
              DO 60 KB = 1, NM2
              K = NM2 - KB + 1
   60         B(K) = (B(K) - DIAG(K)*B(K+1) - SUPERD(K)*B(K+2))/SUBD(K)
              ENDIF
           ENDIF
  100 RETURN
      END
C
      SUBROUTINE Preformation(FLM,E)
C
C IWAMOTO,HARADA COALESCENCE PICK-UP MODEL, PHYS.REV.1982,V.26,P.1821
C
C Fitted and coded by R. Capote Noy, November 2002
C    
C     Input : Energy E in cms 
C     Output: FLM(Ejectile Mass,L)
C             L=1,Aejectile
C
C     i.e. FLM(4,L) means alpha particle preformation factor (L=1,4)
C
      REAL*8 FLM(4,4),X(5),E

C     FOR FAST POLYNOMIAL EVALUATION
      X(1)=E
      DO L=2,5
      X(L)=E*X(L-1)
      ENDDO

C     DEUTERON (MASS = 2)
      IF(E.LT.80.d0) THEN
      FLM(2,2)=
     &       max(0.00821+0.02038*X(1)+5.95941E-4*X(2)
     &       -2.24726E-5*X(3)+2.38917E-7*X(4)-8.34053E-10*X(5),0.)
      ELSE
       FLM(2,2)=1.
      ENDIF
      FLM(2,1)=max(1.-FLM(2,2),0.)

C     TRITIUM or HELIUM-3 (MASS = 3)
      IF(E.LT.70.d0) THEN
      FLM(3,1)= max(0.57315-0.02083*X(1)+3.19204E-4*X(2)
     & -2.85876E-6*X(3)+1.26332E-8*X(4),0.d0)
      ELSE
        FLM(3,1)=0.d0
      ENDIF
      
      IF(E.LT.170.) THEN
      FLM(3,3)=max(0.00705-0.00164*X(1)+2.16549E-4*X(2)
     &        -1.73867E-6*X(3)+5.24069E-9*X(4)-5.79848E-12*X(5),0.d0)  
      ELSE
        FLM(3,3)=1.d0
      ENDIF

      FLM(3,2)=max(1.d0-FLM(3,1)-FLM(3,3),0.d0)

C     ALPHA PARTICLES (M=4)
      IF(E.LT.70.d0) THEN
      FLM(4,1)=max(0.29119-0.01434*X(1)+3.34045E-4*X(2)
     &             -4.10957E-6*X(3)+2.02375E-8*X(4),0.d0)
      ELSE
        FLM(4,1)=0.d0
      ENDIF

      IF(E.LT.130.d0) THEN
        FLM(4,2)=0.60522+1.30071E-4*X(1)-1.77653E-4*X(2)
     &          +1.64048E-6*X(3)-4.55562E-9*X(4)+2.09521E-12*X(5)
      ELSE
      FLM(4,2)=0.d0
      ENDIF

      IF(E.LT.30.d0) THEN
      FLM(4,4)=0.d0
      ELSEIF(E.LT.300.d0) THEN
        FLM(4,4)=0.01917-0.00307*X(1)+9.57966E-5*X(2)-4.07085E-7*X(3)
     &      +5.28037E-10*X(4)+7.92029E-16*X(5)
      ELSE
      FLM(4,4)=1.d0
      ENDIF

      IF(E.LT.300.d0) THEN
        FLM(4,3)=max(1.d0-FLM(4,1)-FLM(4,2)-FLM(4,4),0.d0)
      ELSE
        FLM(4,3)=0.d0
      ENDIF
     
      RETURN
      END
C
C
      FUNCTION densw(G,D,P,H,E)
C
C  WILLIAMS FORMULATION, PAULI CORRECTION BY KALBACH
C
C  G - LEVEL DENSITY PARAMETER
C  D - PAIRING CORRECTION
C  P(H) - PARTICLE(HOLE) NUMBER
C  E - ENERGIA DE EXCITACION
C
      IMPLICIT LOGICAL (A-Z)
      REAL*8 G,D,E
      REAL*8 A,U,FAC,DENSW
      INTEGER*4 P,H,N,PMAX
      PARAMETER (PMAX=50)
      REAL*8 FA,LFA
      COMMON/FACT/FA(2*PMAX+3),LFA(2*PMAX+3)

      DENSW=0.d0
      N=P+H
      IF(N.LE.0) RETURN
      FAC=LFA(P+3)+LFA(H+3)+LFA(N+2)
      A=.25D0*(P*(P-1)+H*(H-1))
      U=G*(E-D)-A
      IF(U.LE.0.) RETURN

      DENSW=G*(DEXP((N-1)*DLOG(U)-FAC))

      RETURN
      END

      FUNCTION SGAM(A,Z,EG,BETA2)
C [1] S.A.FAYANS
C     "Lectures,DUBNA,feb.1982"
C SGAM = GAMMA ABSORPTION CROSS SECTION in mb
      IMPLICIT REAL*8(A-H,O-Z)
       A3=BETA2
       A3=A**(.3333333333333333D0)
       EGR=29.*DSQRT((1.D0+2.D0/A3)/A3)
       GAM=5.D0
       SGM=53.2*(A-Z)*Z/A
       SGAM=SGM*GAM*EG*EG/((EG*EG-EGR*EGR)**2+(GAM*EG)**2)
      RETURN
      END

C      FUNCTION SGAM(A,Z,EG,BETA2)
C
C Parametrization of the strenght function for 40 < A <160
C
C [1] D.G.GARDNER,F.S.DIETRICH
C     "Proc.Int.Conf.Nucl.Cross Sect. for Technol.",Knoxville,Tenn.1979,
C     NBS Special Publication 594(1980)p770
C [2] D.G.Gardner ANL-83-4(1983)p67
C
C [3] Rev.Roum.Phys.32,N8,(1987)p837-848
C
C SGAM = GAMMA ABSORPTION CROSS SECTION in mb
C
C      IMPLICIT REAL*8(A-H,O-Z)
C      PARAMETER(EX=5.D0)
C        IF(A.GT.90) THEN
C        FSR=1.25
C        ELSE
C        FSR=1.
C        ENDIF
C      AA=A**(-(1.D0/3.D0))
CC
C      ER=31.2*AA+20.6*A**(-(1.D0/6.D0))
CC
C            IF(A.LT.160) THEN
C            GR2=29.7*AA
C            ELSE
C            GR2=66.1/SQRT(A)
C            ENDIF
C            GR1=.5*GR2
CC
C      FBB=1.
C      IF(BETA2.NE.0.) FBB=(2.126+.822*BETA2)/(2.126-BETA2)*.95
C      ER1=3.*ER/(2.*FBB+1.)
C      ER2=.5*(3.*ER-ER1)
CC
C      IF(EG.LE.(ER+EX)*.5D0) THEN
C      GDE1=GR1*(2.*EG/(EX+ER1))**2
C      GDE2=GR2*(2.*EG/(EX+ER2))**2
C      FGR1=1./(  1.+4.*(EG-ER1)*(EG-ER1)/ (GDE1*GR1)  )
C      FGR2=1./(  1.+4.*(EG-ER2)*(EG-ER2)/ (GDE2*GR2)  )
C      ELSE
C      FGR1=1./(1.+4.*(EG-ER1)*(EG-ER1)/(GR1*GR1))
C      FGR2=1./(1.+4.*(EG-ER2)*(EG-ER2)/(GR2*GR2))
C      ENDIF
C      FE1=3.32*(A-Z)*Z/A*FSR*(FGR1+FGR2)/(GR1+GR2)
C      SGAM=FE1/8.67*100.
C      RETURN
C      END

