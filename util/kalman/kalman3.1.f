C*********************************************************************
C*     PROGRAM KALMAN    :    KYUSHU UNIVERSITY 1992                 *
C*                                                                   *
C*                             ORIGINAL VERSION 1983 Y.UENOHARA      *
C*                              REVISED VERSION 1989 T.KAWANO        *
C*                                 OPEN VERSION 1992 T.KAWANO        *
C*                                                                   *
C*    UNIT(5)  : READ NREAC,NPARM,IEXP,IPARM,EW                      *
C*    UNIT(6)  : OUTPUT                                              *
C*    UNIT(10) : READ X,Y                                            *
C*    UNIT(11) : READ X,Z                                            *
C*    UNIT(12) : READ V                                              *
C*    UNIT(13) : WRITE SPLINE FUNCTION                               *
C*    UNIT(14) : WRITE COVARIANCE OF SPLINE FUNCTION                 *
C*    UNIT(15) : WRITE VARIATION OF PARAMETERS                       *
C*    UNIT(50) : READ E0,S0                                          *
C*    UNIT(52) : READ P0,X0,A                                        *
C* NREAC,NPARM : NUMBER OF REACTIONS, PARAMETERS TO BE ESTIMATED     *
C* NRMAX,NPMAX : MAXIMAL NUMBER OF REACTIONS, PARAMETERS             *
C*  IEXP,IPARM : INDEX OF EXPERIMENTAL DATA, PARAMETERS              *
C*       NMSUR : NUMBER OF EXPERIMENTS IN EACH REACTIONS             *
C*          EW : WEIGHT OF EACH MEASUREMENT                          *
C*       P0,P1 : PRIOR AND POSTERIOR PARAMETERS                      *
C*          E0 : KNOTS OF SPLINE (EVALUATION POINT)                  *
C*          S0 : CALCULATED VALUE BY PRIOR PARAMETERS ( F(P0) )      *
C*           A : SENSITIVITY (D F(P0)/D P0)                          *
C*       X,Y,Z : EXPERIMENTAL DATA AND ERROR                         *
C*        X0,V : COVARIANCE OF PARAMETERS AND EXPERIMENTAL DATA      *
C*               SYMMETRICAL MATRIX IS STORED IN ONE-DIM. MODE       *
C*               V(1)                                                *
C*               V(2) V(3)                                           *
C*               V(4) V(5) V(6)                                      *
C*                                                                   *
C*    HISTORY                                                        *
C*    1994, 8  : COVARIANCE OF EXP.DATA READ FROM UNIT(12)           *
C*               SEE SUBROUTINE EXDATA                               *
C*                                                                   *
C*********************************************************************
c     PARAMETER(NP=100,MP=5050,NX=300,MX=45150,NS=100,NR=30)
      PARAMETER(NP=100,MP=NP*(NP+1)/2,NX=10000,MX=NX*(NX+1)/2,NS=100,
     &          NR=30)
C               NP : MAXIMAL NUMBER OF PARAMETERS
C               NX : MAXIMAL NUMBER OF EXPERIMENTAL DATA
C               NS : MAXIMAL NUMBER OF SPLINE KNOTS
C               NR : MAXIMAL NUMBER OF REACTION TYPES
C               MP : SIZE OF COVARIANCE OF PARAMETERS = NP*(NP+1)/2
C               MX : SIZE OF COVARIANCE OF DATA       = NX*(NX+1)/2
C
      PARAMETER(NW=5000)
C               NW : MAXIMAL NUMBER OF MEASUREMENTS IN A REACTION
C
      REAL*8 X(NX),Y(NX),V(MX)
      REAL*8 S0(NS),E0(NS),A(NS,NP),SG(NR,NS),EG(NR,NS),AG(NR,NS,NP)
      REAL*8 P0(NP),P1(NP),PE(NP),PD(NP),X0(MP)
      REAL*8 PRIP(NP),PRIE(NP),PRIC(MP)
      REAL*8 EW(NW),CHISQ,CHITOT,RATIO,XX,YY,EMIN0,EMAX0,EMINIM,EMAXIM
      DIMENSION IEXP(NR),IPARM(NP),NENRG(NR)
      CHARACTER*12 PNAME(NP)
      CHARACTER*43 REACT(NR)
      CHARACTER*80 TITLE
C
      DATA S0,P0,P1,EW/ NS*0.0,NP*0.0,NP*0.0,NW*1.0/
      DATA EMIN0,EMAX0,EMINIM,EMAXIM/ 4*0.0/
      DATA NTOTAL,ISEQNO,KCOVEX,CHITOT/ 0, 0, 0, 0.0/
      DATA PNAME / NP*'            '/
C
      READ(5,2000) TITLE
      READ(5,2015) NREAC,NPARM,KCTL1,KCTL2,KCOVEX,EMIN0,EMAX0
      READ(5,2010)(IPARM(I),I=1,NPARM)
C
      CALL GETCRX(NENRG,NRMAX,SG,EG,REACT)
      CALL GETPRM(NPARM,NPMAX,P0,P1,X0,PRIP,PRIE,PRIC,IPARM,PNAME)
      CALL GETSTV(NENRG,NRMAX,NPARM,NPMAX,AG,IPARM)
      IF(KCTL1.NE.0) THEN 
         CALL PREAD(KCTL1,NPARM,NPMAX,P1,X0,PRIP,PRIE,PRIC,IPARM)
      END IF
C
      WRITE(6,2000) TITLE
      WRITE(6,2040) NPMAX
      WRITE(6,2050) NPARM
      WRITE(15,2060) 0,NPARM
      WRITE(15,2030)(P1(I),I=1,NPARM)
C
      EMINIM=EMIN0
      EMAXIM=EMAX0
C
C***************************************
C     REACTION LOOP                    *
C***************************************
      DO 1000 IREAC=1,NREAC
         READ(5,2010) IEXP(IREAC),NMSUR
         READ(5,2020) (EW(I),I=1,NMSUR)
CD       WRITE(6,2020)(EW(I),I=1,NMSUR)
         CALL SETSTV(NENRG,NPARM,S0,E0,A,SG,EG,AG,
     &               IPARM,IEXP(IREAC),IZERO)
         DO 10 I=1,NPARM
   10       PD(I)=P1(I)-P0(I)
         DO 20 I=1,NENRG(IREAC)
            XX=0.0
            DO 30 J=1,NPARM
   30           XX=XX+A(I,J)*PD(J)
            S0(I)=S0(I)+XX
   20    CONTINUE
C
C***************************************
C     MEASUREMENT LOOP                 *
C***************************************
         DO 1100 IMSUR=1,NMSUR
            IF(EMIN0 .EQ. 0.0) EMINIM=E0(IZERO)
            IF(EMAX0 .EQ. 0.0) EMAXIM=E0(1)
            CALL EXDATA(X,Y,V,EMAXIM,EMINIM,EW(IMSUR),NDATA,1,KCOVEX)
            IF(EW(IMSUR).EQ.0.0 .OR. NDATA.EQ.0) THEN
               CHISQ=0.0
               GO TO 1200
            END IF
C
C***************************************
C     MAIN CALCULATION                 *
C***************************************
            CALL PRCALC(NDATA,NENRG(IEXP(IREAC)),NPARM,
     &                  X,Y,V,P1,PD,X0,S0,E0,A,CHISQ)
C
            CHITOT=CHITOT+CHISQ
            NTOTAL=NTOTAL+NDATA
C
 1200       WRITE(6,2080) REACT(IEXP(IREAC))
            WRITE(6,2090) ISEQNO+IMSUR,IMSUR,NDATA,NTOTAL
            WRITE(6,2100) CHISQ,CHITOT
C
            WRITE(15,2060) IREAC,NPARM
            WRITE(15,2030)(P1(I),I=1,NPARM)
 1100    CONTINUE
C
         ISEQNO=ISEQNO+IMSUR-1
 1000 CONTINUE
C
C***************************************
C     OUTPUT POSTERIOR PARAMETERS      *
C***************************************
      IF(NTOTAL.EQ.0) THEN
          RATIO=0.0
      ELSE
          RATIO=CHITOT/DBLE(NTOTAL)
      END IF
      WRITE(6,2110) CHITOT,NTOTAL,RATIO
      IF(RATIO.LE.1.0) RATIO=1.0
      RATIO=DSQRT(RATIO)
C
      IF(KCTL1 .EQ. 0) THEN
         WRITE(6,2120)
         DO 100 I=1,NPARM
            PE(I)=DSQRT(X0(I*(I+1)/2))
            XX=P1(I)
            IF(XX.EQ.0.) THEN
               WRITE(6,2130) I,PNAME(I),P0(I),P1(I),PE(I)
            ELSE
               YY=PE(I)/XX*100.
               WRITE(6,2140) I,PNAME(I),P0(I),P1(I),YY
            END IF
  100    CONTINUE
      ELSE
         WRITE(6,2150)
         DO 110 I=1,NPARM
            PE(I)=DSQRT(X0(I*(I+1)/2))
            XX=P1(I)
            K=IPARM(I)
            IF(XX.EQ.0.) THEN
               WRITE(6,2160) I,PNAME(I),P0(I),PRIP(K),P1(I),PE(I)
            ELSE
               YY=PE(I)/XX*100.
               WRITE(6,2170) I,PNAME(I),P0(I),PRIP(K),P1(I),YY
            END IF
  110    CONTINUE
      END IF
C
      DO 200 I=1,NPARM
         KI=IPARM(I)
         PRIP(KI)=P1(I)
         PRIE(KI)=PE(I)
         DO 200 J=1,I
            KJ =IPARM(J)
            IJ=I*(I-1)/2+J
            IF(KJ.LE.KI) THEN
               KIJ=KI*(KI-1)/2+KJ
            ELSE 
               KIJ=KJ*(KJ-1)/2+KI
            END IF
            PRIC(KIJ)=X0(IJ)
  200 CONTINUE
C
C***************************************
C     OUTPUT SPLINE FUNCTIONS          *
C***************************************
      CALL OUTSPL(NPARM,NRMAX,NENRG,IPARM,SG,EG,AG,P0,P1,REACT)
C
C***************************************
C     OUTPUT COVARIANCE OF SPLINES     *
C***************************************
      CALL CVRSPL(NPMAX,NRMAX,NENRG,PRIC,AG,SG,EG,REACT)
C
C***************************************
C     OUTPUT COVARIANCE OF PARAMETERS  *
C***************************************
      CALL CVRPRM(0,NPARM,P1,PE,PNAME,X0)
      IF(KCTL2 .NE. 0) THEN 
         CALL PWRITE(KCTL2,NPARM,NPMAX,P1,PE,PRIP,PRIE,PRIC,IPARM)
      END IF
C
C***************************************
C     OUTPUT PARAMETER VARIATIONS      *
C***************************************
      CALL PRSORT(PNAME)
C
 2000 FORMAT(A80)
 2010 FORMAT(14I5)
 2015 FORMAT(5I5,5X,2E10.3)
 2020 FORMAT(7E10.3)
 2030 FORMAT(6(1PE11.4))
 2040 FORMAT('NUMBER OF TOTAL PARAMETERS ',I5 )
 2050 FORMAT('      ESTIMATED PARAMETERS ',I5/)
 2060 FORMAT(15X,I5,23X,I5)
 2080 FORMAT('CROSS SECTION :',A43)
 2090 FORMAT('SQ.NO.:',I5,3X,
     1       'REACT.:',I5,3X,
     2       'POINTS:',I5,3X,
     3       'SUM UP:',I5)
 2100 FORMAT('PARTIAL CHI SQ:',1PE12.5,3X,'CUMULATIVE :',1PE12.5/)
 2110 FORMAT('CHI-SQUARE TEST !   ',   15X,'CHI - S = ',E12.5,
     &      /'DEGREE OF FREEDOM = ',I5,10X,'RATIO   = ',E12.5)
 2120 FORMAT(/5X,' PARAMETER  ',
     &'  INITIAL  ','  FINAL    ','  ERROR    '/)
 2130 FORMAT(I5,A12,3(1PE11.4),1X,'(ABS)')
 2140 FORMAT(I5,A12,3(1PE11.4),1X,'( % )')
 2150 FORMAT(/5X,' PARAMETER  ',
     &'  INITIAL  ','  FINAL 1  ','  FINAL 2  ','  ERROR    '/)
 2160 FORMAT(I5,A12,4(1PE11.4),1X,'(ABS)')
 2170 FORMAT(I5,A12,4(1PE11.4),1X,'( % )')
      STOP
      END
C
C*************************************************************
C     PRCALC : MAIN CALCULATION                              *
C*************************************************************
C
      SUBROUTINE PRCALC(NDATA,NENRG,NPARM,
     &                  X,Y,V,P1,PD,X0,S0,E0,A,CHISQ)
      PARAMETER(NP=100,MP=NP*(NP+1)/2,NX=10000,MX=NX*(NX+1)/2,NS=100,
     &          NR=30)
      REAL*8 X(NX),Y(NX),V(MX),S0(NS),E0(NS),A(NS,NP)
      REAL*8 P1(NP),PD(NP),X0(MP)
      REAL*8 D(NX,NS),W(NP,NX),F(NX,NP)
      REAL*8 CHISQ,X1,X2,X3,X4
C
      CALL DESIGN(D,X,E0,NDATA,NENRG)
C***************************************
C     Y <= Y - F S0                    *
C***************************************
      DO 10 I=1,NDATA
         X1=0.0
         DO 15 K=1,NENRG
            X1=X1+D(I,K)*S0(K)
   15    CONTINUE
         Y(I)=Y(I)-X1
   10 CONTINUE
C***************************************
C     F <= FI * A                      *
C***************************************
      DO 20 I=1,NDATA
         DO 20 J=1,NPARM
            F(I,J)=0.0
            DO 20 K=1,NENRG
               F(I,J)=D(I,K)*A(K,J)+F(I,J)
   20 CONTINUE
C***************************************
C     W <= F * X0                      *
C***************************************
      DO 30 J=1,NDATA
         ISTART=0
         DO 30 I=1,NPARM
            X1=0.0
            DO 31 K=1,I
               IK=ISTART+K
   31          X1=X1+X0(IK)*F(J,K)
            ISTART=ISTART+I
            IK=ISTART
            DO 32 K=I+1,NPARM
               IK=IK+K-1
   32          X1=X1+X0(IK)*F(J,K)
            W(I,J)=X1
   30 CONTINUE
C***************************************
C     V <= ( F * X0 * F**T + V )**(-1) *
C***************************************
      ISTART=0
      DO 40 I=1,NDATA
         DO 41 J=1,I
            X1=0.0
            IJ=ISTART+J
            DO 42 K=1,NPARM
   42          X1=X1+W(K,J)*F(I,K)
            V(IJ)=V(IJ)+X1
   41    CONTINUE
         ISTART=ISTART+I
   40 CONTINUE
      CALL INVMTX(NDATA,V)
C***************************************
C     CALC P1,X1,DP                    *
C***************************************
      ISTART=0
      DO 50 I=1,NPARM
         DO 51 J=1,I-1
            X1=0.0
            IJ=ISTART+J
            KSTART=0
            DO 52 K=1,NDATA
               X3=0.0
               DO 53 L=1,K
                  KL=KSTART+L
                  X3=X3+V(KL)*W(J,L)
   53          CONTINUE
               KSTART=KSTART+K
               KL=KSTART
               DO 54 L=K+1,NDATA
                  KL=KL+L-1
                  X3=X3+V(KL)*W(J,L)
   54          CONTINUE
               X1=X1+W(I,K)*X3
   52       CONTINUE
            X0(IJ)=X0(IJ)-X1
   51    CONTINUE
         ISTART=ISTART+I
         X1=0.0
         X2=0.0
         IJ=ISTART
         KSTART=0
         DO 55 K=1,NDATA
            X3=0.0
            X4=0.0
            DO 56 L=1,K
               KL=KSTART+L
               X3=X3+V(KL)*W(I,L)
               X4=X4+V(KL)*Y(L)
   56       CONTINUE
            KSTART=KSTART+K
            KL    =KSTART
            DO 57 L=K+1,NDATA
               KL=KL+L-1
               X3=X3+V(KL)*W(I,L)
               X4=X4+V(KL)*Y(L)
   57       CONTINUE
            X1=X1+W(I,K)*X3
            X2=X2+W(I,K)*X4
   55    CONTINUE
         X0(IJ)=X0(IJ)-X1
         PD(I) =X2
         P1(I) =P1(I)+X2
   50 CONTINUE
      DO 58 I=1,NENRG
         X1=0.0
         DO 59 J=1,NPARM
   59       X1=X1+A(I,J)*PD(J)
         S0(I)=S0(I)+X1
   58 CONTINUE
C***************************************
C     CHI SQUARE TEST                  *
C***************************************
      ISTART=0
      X2=0.0
      DO 60 I=1,NDATA
         X1=0.0
         DO 61 J=1,I-1
            IJ=ISTART+J
   61       X1=X1+V(IJ)*Y(J)
         ISTART=ISTART+I
         X2    =X2+Y(I)*X1
   60 CONTINUE
      X1=0.0
      DO 62 I=1,NDATA
         II=I*(I+1)/2
         X1=X1+V(II)*Y(I)**2
   62 CONTINUE
      CHISQ =2.0*X2+X1
      RETURN
      END
C
C*************************************************************
C     GETCRX : READ CALCULATED VALUES WITH PRIOR PARAMETERS  *
C*************************************************************
C
      SUBROUTINE GETCRX(NENRG,NRMAX,SG,EG,REACT)
      PARAMETER(NP=100,MP=NP*(NP+1)/2,NX=10000,MX=NX*(NX+1)/2,NS=100,
     &          NR=30)
      REAL*8 SG(NR,NS),EG(NR,NS)
      DIMENSION NENRG(NR)
      CHARACTER*43 REACT(NR)
C
      I=1
  100 READ(50,2000,END=200) REACT(I),NENRG(I)
      READ(50,2010        )(EG(I,K),SG(I,K),K=1,NENRG(I))
CD    WRITE(6,2000)         REACT(I),NENRG(I)
CD    WRITE(6,2010)        (EG(I,K),SG(I,K),K=1,NENRG(I))
      I=I+1
      GO TO 100
  200 CONTINUE
      NRMAX=I-1
 2000 FORMAT(A43,I5)
 2010 FORMAT(6E11.4)
      RETURN
      END
C
C*************************************************************
C     GETPRM : READ PRIOR PARAMETERS AND ITS COVARIANCE      *
C*************************************************************
C
      SUBROUTINE GETPRM(NPARM,NPMAX,P0,P1,X0,PRIP,PRIE,PRIC,IPARM,
     &                  PNAME)
      PARAMETER(NP=100,MP=NP*(NP+1)/2,NX=10000,MX=NX*(NX+1)/2,NS=100,
     &          NR=30)
      REAL*8 P1(NP),P0(NP),X0(MP)
      REAL*8 PRIP(NP),PRIE(NP),PRIC(MP)
      DIMENSION IPARM(NP)
      CHARACTER*12 PNAME(NP),PDUMMY(NP)
C
      READ(52,2000) NPMAX
      READ(52,2010)(PDUMMY(I),I=1,NPMAX)
C
      CALL PREAD(52,NPARM,NPMAX,P0,X0,PRIP,PRIE,PRIC,IPARM)
      DO 10 I=1,NPARM
         PNAME(I)=PDUMMY(IPARM(I))
         P1(I)   =P0(I)
   10 CONTINUE
C
CD    DO 20 I=1,NPARM
CD 20    WRITE(6,2020) PNAME(I),P0(I),P1(I)
CD    DO 30 I=1,NPARM
CD 30    WRITE(6,2030)(X0(I*(I-1)/2+J),J=1,I)
 2000 FORMAT(25X,I5)
 2010 FORMAT(11A12)
 2020 FORMAT(A11,5X,3(1PE12.5))
 2030 FORMAT(11(1PE12.5))
      RETURN
      END
C
C*************************************************************
C     GETSTV : READ SENSITIVITY MATRIX                       *
C*************************************************************
C
      SUBROUTINE GETSTV(NENRG,NRMAX,NPARM,NPMAX,AG,IPARM)
      PARAMETER(NP=100,MP=NP*(NP+1)/2,NX=10000,MX=NX*(NX+1)/2,NS=100,
     &          NR=30)
      REAL*8 AG(NR,NS,NP)
      DIMENSION IPARM(NP),NENRG(NR)
      CHARACTER*25 TARG
C
      DO 10 I=1,NRMAX
         READ(52,2000) TARG,N
         IF(N.NE.NENRG(I)) THEN
            WRITE(6,2000) TARG,N
            WRITE(6,*) 'SENSITIVITY, SPLINE, POINTS DIFFERENT'
            STOP
         END IF
CD       WRITE(6,2000) TARG,N
         DO 20 K=1,N
            READ(52,2010)(AG(I,K,L),L=1,NPMAX)
CD          WRITE(6,2010)(AG(I,K,L),L=1,NPMAX)
   20    CONTINUE
   10 CONTINUE
 2000 FORMAT(A25,I5)
 2010 FORMAT(11E12.5)
      RETURN
      END
C
C*************************************************************
C     SETSTV : PREPARE SENSITIVITY AND SPLINE                *
C*************************************************************
C
      SUBROUTINE SETSTV(NENRG,NPARM,S0,E0,A,SG,EG,AG,IPARM,KREAC,IZERO)
      PARAMETER(NP=100,MP=NP*(NP+1)/2,NX=10000,MX=NX*(NX+1)/2,NS=100,
     &          NR=30)
      REAL*8 SG(NR,NS),EG(NR,NS),AG(NR,NS,NP),S0(NS),E0(NS),A(NS,NP)
      DIMENSION NENRG(NR),IPARM(NP)
C
      DO 10 J=1,NENRG(KREAC)
         E0(J)=EG(KREAC,J)
         S0(J)=SG(KREAC,J)
         DO 10 K=1,NPARM
            A(J,K)=AG(KREAC,J,IPARM(K))
   10 CONTINUE
C***************************************
C     QUICK SORT                       *
C***************************************
      DO 20 J=1,NENRG(KREAC)
         L=J
         DO 30 I=J,NENRG(KREAC)
            IF(E0(I).GT.E0(L)) L=I
   30    CONTINUE
         CALL SWAP(E0(J),E0(L))
         CALL SWAP(S0(J),S0(L))
         DO 20 K=1,NPARM
            CALL SWAP(A(J,K),A(L,K))
   20 CONTINUE
C***************************************
C     FIND ZERO                        *
C***************************************
      IZERO=NENRG(KREAC)
      DO 40 I=1,NENRG(KREAC)
         IF(S0(I).EQ.0.0) THEN
            IZERO=I-1
            GO TO 50
         END IF
   40 CONTINUE
   50 CONTINUE
CD    WRITE(6,*) 'IZERO =',IZERO
      RETURN
      END
C
C*************************************************************
C     EXDATA : READ EXPERIMENTAL DATA                        *
C*************************************************************
C
      SUBROUTINE EXDATA(X,Y,V,EMAX,EMIN,EW,NDATA,NGROUP,KCOVEX)
      PARAMETER(NP=100,MP=NP*(NP+1)/2,NX=10000,MX=NX*(NX+1)/2,NS=100,
     &          NR=30)
C
C           KCOVEX = 1 : READ COVARIANCE FILE FROM UNIT(12)
C           KCOVEX = 0 : DONT READ COVARIANCE FILE
C
      REAL*8 X(NX),Y(NX),V(MX),EMAX,EMIN,EW,XX
      REAL*8 Z(NX),XMIN,XMAX
      CHARACTER*43 TITLE
C
      DO 110 I=1,NX
         ISTART=I*(I-1)/2
         DO 110 J=1,I
            IJ=ISTART+J
            IF(I.EQ.J) THEN
               V(IJ)=1.0
            ELSE
               V(IJ)=0.0
            END IF
  110 CONTINUE
C***************************************
C     READ UNIT 10,11,12               *
C***************************************
      NDATA=0
      DO 210 KG=1,NGROUP
         READ(10,100) TITLE,ND
         READ(10,300)(X(I+NDATA),Y(I+NDATA),I=1,ND)
         WRITE(6,100) TITLE,ND
         READ(11,100) TITLE,ND
         READ(11,300)(XX        ,Z(I+NDATA),I=1,ND)
CD       WRITE(6,400)(X(I+NDATA),Y(I+NDATA),Z(I+NDATA),I=1,ND)
C
         IF(KCOVEX.EQ.1) THEN
            READ(12,100) TITLE,NC
CD          WRITE(6,100) TITLE,NC
            IF(NC.NE.0) THEN
               IF(ABS(NC).NE.ND) THEN
                  WRITE(6,2050) NC, ND
                  STOP
               END IF
               IF(NC.GE.1) THEN
                  DO 220 I=1,NC
                     ISTART=NDATA+(I+NDATA)*(I+NDATA-1)/2
                     READ(12,500)(V(ISTART+J),J=1,I)
CD                   WRITE(6,500)(V(ISTART+J),J=1,I)
  220             CONTINUE
               ELSE IF(NC.LE.-1) THEN
                  READ(12,500) XX
                  DO 230 I=1,IABS(NC)
                     ISTART=NDATA+(I+NDATA)*(I+NDATA-1)/2
                     DO 240 J=1,I-1
  240                   V(ISTART+J)=XX
                     V(ISTART+I)=1.0
CD                   WRITE(6,500)(V(ISTART+J),J=1,I)
  230             CONTINUE
               END IF
            END IF
         END IF
C
         NDATA=NDATA+ND
  210 CONTINUE
C
      IF(EW .EQ. 0.0) THEN
         WRITE(6,2010)
         NDATA=0
         GOTO 1000
      END IF
C
C***************************************
C     RANGE CHECK                      *
C***************************************
      XMIN=X(1)
      XMAX=X(1)
      DO 250 I=2,NDATA
         IF(XMIN .GT. X(I)) XMIN=X(I)
         IF(XMAX .LT. X(I)) XMAX=X(I)
  250 CONTINUE
      IF(XMIN.GT.EMAX .OR. XMAX.LT.EMIN) THEN
         WRITE(6,2020)
         NDATA=0
         GOTO 1000
      END IF
C
C***************************************
C     MAKE COVARIANCE OF EXP. DATA     *
C***************************************
      DO 310 I=1,NDATA
         IF(Z(I) .LT. 0.0) THEN
            Z(I)=EW*DABS(Z(I))
         ELSE IF(Z(I).EQ.0.0) THEN
            WRITE(6,2030) I
            NDATA=0
            GOTO 1000
         ELSE
            Z(I)=EW*Y(I)*Z(I)
         END IF
C        IF(Z(I)/Y(I).LE.0.03) WRITE(6,*) 'ERR < 3%, I=',I
         ISTART=I*(I-1)/2
         DO 310 J=1,I
            IJ=ISTART+J
            V(IJ)=V(IJ)*Z(I)*Z(J)
  310 CONTINUE
C***************************************
C     QUICK SORT                       *
C***************************************
      DO 410 J=1,NDATA
         L=J
         DO 420 I=J,NDATA
            IF(X(I).GT.X(L)) L=I
  420    CONTINUE
         CALL SWAP(X(J),X(L))
         CALL SWAP(Y(J),Y(L))
         CALL SWCL(V,NDATA,J,L)
  410 CONTINUE
C***************************************
C     ELIMINATE OUT OF RANGE           *
C***************************************
      ND=NDATA
      DO 510 I=NDATA,1,-1
         IF(X(I) .LT. EMIN) THEN
            ND=ND-1
         ELSE IF(X(I) .GT. EMAX) THEN
            DO 520 L=I+1,NDATA
               X(L-1)=X(L)
               Y(L-1)=Y(L)
               ISTART=L*(L-1)/2
                  DO 520 J=I+1,L
                     V(ISTART+J-1)=V(ISTART+J)
  520       CONTINUE
            DO 530 L=I+1,NDATA
               IS1=(L-2)*(L-1)/2
               IS2= L   *(L-1)/2
               DO 530 J=1,L-1
                  V(IS1+J)=V(IS2+J)
  530       CONTINUE
            ND=ND-1
         END IF
  510 CONTINUE
      NDATA=ND
C
CD    DO 610 I=1,NDATA
CD       ISTART=I*(I-1)/2
CD610    WRITE(6,600) X(I),Y(I),(V(ISTART+J),J=1,I)
C
  100 FORMAT(A43,I5)
  200 FORMAT(6E12.5)
  300 FORMAT(6E11.4)
  400 FORMAT(3(1PE11.4))
  500 FORMAT(12F6.3)
  600 FORMAT(6(1PE11.4))
 2010 FORMAT('*** EXP. DATA IGNORED ********')
 2020 FORMAT('*** EXP. DATA OUT OF RANGE ***')
 2030 FORMAT('*** ERR = 0 FOUND, I=',I5)
 2050 FORMAT('*** NO. OF POINTS DIFFERENT **,',I5,' AND ',I5)
 1000 RETURN
      END
C
C***************************************
C     SWAP DATA A <-> B                *
C***************************************
      SUBROUTINE SWAP(A,B)
      REAL*8 A,B,C
      C=A
      A=B
      B=C
      RETURN
      END
C
C***************************************
C     SWAP COLUMNS OF COVARIANCE       *
C***************************************
      SUBROUTINE SWCL(V,N,I1,I2)
      PARAMETER(NP=100,MP=NP*(NP+1)/2,NX=10000,MX=NX*(NX+1)/2,NS=100,
     &          NR=30)
      REAL*8 V(MX)
      REAL*8 A(NX),B(NX),C(NX)
C
      DO 10 J=1,N
         IF(J .LE. I1) THEN
             A(J)=V(I1*(I1-1)/2+J )
         ELSE
             A(J)=V(J *(J -1)/2+I1)
         END IF
         IF(J .LE. I2) THEN
             B(J)=V(I2*(I2-1)/2+J )
         ELSE
             B(J)=V(J *(J -1)/2+I2)
         END IF
   10 CONTINUE
      CALL SWAP(A(I1),A(I2))
      CALL SWAP(B(I1),B(I2))
C
      DO 20 I=1,N
         IF(I.NE.I1 .AND. I.NE.I2) THEN
            DO 30 J=1,N
               IF(J .LE. I) THEN
                  C(J)=V(I*(I-1)/2+J)
               ELSE
                  C(J)=V(J*(J-1)/2+I)
               END IF
   30       CONTINUE
            CALL SWAP(C(I1),C(I2))
         END IF
         ISTART=I*(I-1)/2
         DO 20 J=1,I
            IJ=ISTART+J
            IF(I .EQ. I1) THEN
               V(IJ)=B(J)
            ELSE IF(I .EQ. I2) THEN
               V(IJ)=A(J)
            ELSE
               V(IJ)=C(J)
            END IF
   20 CONTINUE
      RETURN
      END
C
C*************************************************************
C     DESIGN : PREPARE DESIGN MATRIX                         *
C*************************************************************
C
      SUBROUTINE DESIGN(D,X,E0,NDATA,NENRG)
      PARAMETER(NP=100,MP=NP*(NP+1)/2,NX=10000,MX=NX*(NX+1)/2,NS=100,
     &          NR=30)
      PARAMETER(NO=1)
C               NO : ORDER OF POLYNOMIAL = 1
C
      REAL*8 D(NX,NS),X(NX),E0(NS),POLYNM
C
      DO 10 I=1,NDATA
         DO 10 J=1,NENRG
            D(I,J)=POLYNM(X(I),E0,J,NO,NENRG,NS)
   10 CONTINUE
      RETURN
      END
C
      REAL*8 FUNCTION POLYNM(E,X,J,N,NENRG,NS)
      REAL*8 X(NS),Y,Y1,Y2,E,DE1,DE2
      Y=0.0
      DO 10 L=1,NENRG-1
         DE1=E-X(L)
         DE2=E-X(L+1)
         IF(DE1.LE.0.0 .AND. DE2.GT.0.0) GO TO 20
   10 CONTINUE
   20 ID1=L+N
      ID2=L
      IO=ID2-1
      IP=NENRG-ID1
      IF(IO.LT.0) ID2=1
      IF(IP.LT.0) ID1=NENRG
      IF(ID1.LT.J .OR. ID2.GT.J) GO TO 50
      Y1=1.0
      Y2=1.0
      DO 30 K=ID2,ID1
         IF(K.EQ.J) GO TO 30
         Y1=Y1*(E-X(K))
         Y2=Y2*(X(J)-X(K))
   30 CONTINUE
      Y=Y1/Y2
   50 POLYNM=Y
CD    WRITE(6,100) L,J,E,X(L),X(L+1),Y
  100 FORMAT(5X,'L=',I3,' I=',I3,' E=',F7.3,' X(L)=',F7.3,' X(L+1)=',
     /F7.3,' Y=',F7.3)
      RETURN
      END
C
C*************************************************************
C     OUTSPL : WRITE SPLINES ON UNIT 13                      *
C*************************************************************
      SUBROUTINE OUTSPL(NPARM,NRMAX,NENRG,IPARM,SG,EG,AG,P0,P1,REACT)
      PARAMETER(NP=100,MP=NP*(NP+1)/2,NX=10000,MX=NX*(NX+1)/2,NS=100,
     &          NR=30)
      REAL*8 SG(NR,NS),EG(NR,NS),AG(NR,NS,NP),P0(NP),P1(NP)
      REAL*8 XX
      DIMENSION NENRG(NR),IPARM(NP)
      CHARACTER*43 REACT(NR)
C
C***************************************
C     SG <= AG * (P1 - P0)             *
C***************************************
      DO 10 IREAC=1,NRMAX
         DO 20 I=1,NENRG(IREAC)
            XX=0.0
            DO 30 J=1,NPARM
   30          XX=XX+AG(IREAC,I,IPARM(J))*(P1(J)-P0(J))
            SG(IREAC,I)=SG(IREAC,I)+XX
   20    CONTINUE
         WRITE(13,100) REACT(IREAC),NENRG(IREAC)
         WRITE(13,200)(EG(IREAC,I),SG(IREAC,I),I=1,NENRG(IREAC))
   10 CONTINUE
  100 FORMAT(A43,I5)
  200 FORMAT(6(1PE11.4))
      RETURN
      END
C
C*************************************************************
C     CVRSPL : WRITE COVARIANCE MATRIX OF SPLINES ON UNIT 14 *
C*************************************************************
      SUBROUTINE CVRSPL(NPMAX,NRMAX,NENRG,PRIC,AG,SG,EG,REACT)
      PARAMETER(NP=100,MP=NP*(NP+1)/2,NX=10000,MX=NX*(NX+1)/2,NS=100,
     &          NR=30)
      REAL*8 PRIC(MP),SG(NR,NS),EG(NR,NS),AG(NR,NS,NP)
      REAL*8 VG(NS,NS),ER(NR,NS)
      DIMENSION NENRG(NR),IV(NS)
      CHARACTER*43 REACT(NR)
C
C***************************************
C     SAME REACTION                    *
C***************************************
      INDEX=0
      DO 10 IR=1,NRMAX
         CALL MULMTX(IR,IR,NENRG(IR),NENRG(IR),NPMAX,PRIC,AG,VG)
         DO 20 I=1,NENRG(IR)
   20       ER(IR,I)=DSQRT(VG(I,I))
         WRITE(14,300) REACT(IR)
         WRITE(14,200)(INDEX+J,J=1,NENRG(IR))
c
c**************************************
c     Generate ENDF-like numbers      *
c**************************************
         write(16,100) nenrg(ir)
         write(16,500) (eg(ir,i),i=1,nenrg(ir))
         write(16,500) (sg(ir,i),i=1,nenrg(ir))
         do i=1,nenrg(ir)
            write(16,500) (vg(i,j),j=1,nenrg(ir))
         end do
c
         DO 30 I=1,NENRG(IR)
            DO 40 J=1,NENRG(IR)
               IF(ER(IR,I).EQ.0.0 .OR. ER(IR,J).EQ.0.0) THEN
                  IV(J)=0
               ELSE
                  IV(J)=VG(I,J)/(ER(IR,I)*ER(IR,J))*1000.0
               END IF
               IF(I.EQ.J) IV(J)=1000
   40       CONTINUE
            JMAX=MIN(I,50)
            IF(SG(IR,I).EQ.0.0) THEN
               WRITE(14,100) INDEX+I,EG(IR,I),ER(IR,I),(IV(J),J=1,JMAX)
            ELSE
               WRITE(14,100) INDEX+I,EG(IR,I),ER(IR,I)/SG(IR,I)*100.,
     &                      (IV(J),J=1,JMAX)
            END IF
            IF(JMAX .LT. I) WRITE(14,200)(IV(J),J=JMAX+1,I)
   30    CONTINUE
         INDEX=INDEX+NENRG(IR)
   10 CONTINUE
C
C***************************************
C     DIFFERENT REACTION               *
C***************************************
      INDEX=0
      DO 50 IR=1,NRMAX
         JNDEX=0
         DO 60 JR=1,IR-1
            CALL MULMTX(IR,JR,NENRG(IR),NENRG(JR),NPMAX,PRIC,AG,VG)
            WRITE(14,400) REACT(IR),REACT(JR)
            WRITE(14,200)(JNDEX+J,J=1,NENRG(JR))
            DO 70 I=1,NENRG(IR)
               DO 80 J=1,NENRG(JR)
                  IF(ER(IR,I).EQ.0.0 .OR. ER(JR,J).EQ.0.0) THEN
                     IV(J)=0
                  ELSE
                     IV(J)=VG(I,J)/(ER(IR,I)*ER(JR,J))*1000.0
                  END IF
   80          CONTINUE
               JMAX=MIN(NENRG(JR),50)
               IF(SG(IR,I).EQ.0.0) THEN
                  WRITE(14,100) INDEX+I,EG(IR,I),ER(IR,I),
     1                         (IV(J),J=1,JMAX)
               ELSE
                  WRITE(14,100) INDEX+I,EG(IR,I),
     1                          ER(IR,I)/SG(IR,I)*100.0,
     2                         (IV(J),J=1,JMAX)
               END IF
               IF(JMAX .LT. NENRG(JR))
     1           WRITE(14,200) (IV(J),J=JMAX+1,NENRG(JR))
   70       CONTINUE
            JNDEX=JNDEX+NENRG(JR)
   60    CONTINUE
         INDEX=INDEX+NENRG(IR)
   50 CONTINUE
  100 FORMAT(I5,2(1PE10.2),50I5)
  150 FORMAT(I5,20F6.2)
  200 FORMAT(25X,50I5)
  300 FORMAT('DIAGONAL    ',A43)
  400 FORMAT('OFF-DIAGONAL',A43,/,
     &       '            ',A43)
  500 FORMAT(6(1PE12.5))
      RETURN
      END
C
C***************************************
C     VG <= A * PRIC * A**T            *
C***************************************
      SUBROUTINE MULMTX(IR,JR,NIR,NJR,NPMAX,PRIC,AG,VG)
      PARAMETER(NP=100,MP=NP*(NP+1)/2,NX=10000,MX=NX*(NX+1)/2,NS=100,
     &          NR=30)
      REAL*8 PRIC(MP),AG(NR,NS,NP),VG(NS,NS)
      REAL*8 W(NP),XX
C
      DO 10 I=1,NIR
         ISTART=0
         DO 20 IP=1,NPMAX
            XX=0.0
            DO 30 K=1,IP
               IK=ISTART+K
   30          XX=XX+PRIC(IK)*AG(IR,I,K)
            ISTART=ISTART+IP
            IK=ISTART
            DO 40 K=IP+1,NPMAX
               IK=IK+K-1
   40          XX=XX+PRIC(IK)*AG(IR,I,K)
            W(IP)=XX
   20    CONTINUE
         DO 50 J=1,NJR
            XX=0.0
            DO 60 K=1,NPMAX
   60          XX=XX+ W( K)*AG(JR,J,K)
            VG(I,J)=XX
   50    CONTINUE
   10 CONTINUE
      RETURN
      END
C
C*************************************************************
C     PRSORT : WRITE VARIATION OF PARAMETERS ON UNIT 15      *
C*************************************************************
C
      SUBROUTINE PRSORT(PNAME)
      PARAMETER(NP=100,MP=NP*(NP+1)/2,NX=10000,MX=NX*(NX+1)/2,NS=100,
     &          NR=30)
      PARAMETER(NT=300)
C               NT : MAXIMAL NUMBER OF ESTIMATION STEPS
C
      CHARACTER*12 PNAME(NP)
      REAL*8 SPARM(NP,NT)
C
      REWIND 15
      NSTEP=1
   10 READ(15,100,END=20) NPARM
      READ(15,200,END=20)(SPARM(J,NSTEP),J=1,NPARM)
      NSTEP=NSTEP+1
      GO TO 10
   20 CONTINUE
      REWIND 15
      NSTEP=NSTEP-1
      DO 30 I=1,NPARM
         WRITE(15,300) I,PNAME(I),NSTEP
   30    WRITE(15,200)(DBLE(J-1),SPARM(I,J),J=1,NSTEP)
  100 FORMAT(43X,I5)
  200 FORMAT(6(1PE11.4))
  300 FORMAT(I4,A12,27X,I5)
      RETURN
      END
C
C*************************************************************
C     CVRPRM : WRITE COVARIANCE MATRIX                       *
C*************************************************************
C
      SUBROUTINE CVRPRM(K,NPARM,P1,PE,PNAME,X0)
      PARAMETER(NP=100,MP=NP*(NP+1)/2,NX=10000,MX=NX*(NX+1)/2,NS=100,
     &          NR=30)
      REAL*8 X0(MP),P1(NP),PE(NP)
      CHARACTER*12 PNAME(NP)
      DIMENSION IX(40)
C
      IF(K.EQ.0) THEN
         NBLOCK=NPARM/10+1
         LO=MOD(NPARM,10)
         IF(LO.EQ.0) NBLOCK=NBLOCK-1
         DO 10 IBLOCK=1,NBLOCK
            J1=10*IBLOCK-9
            J2=10*IBLOCK
            IF(IBLOCK.EQ.NBLOCK .AND. LO.NE.0) J2=J1+LO-1
            JM=J2-J1+1
            DO 20 J=1,JM
   20          IX(J)=J1+J-1
            WRITE(6,300) (IX(J),J=1,JM)
            DO 30 I=J1,NPARM
               ISTART=(I*(I-1))/2
               J12   =MIN(I,J2)
               DO 40 J=J1,J12
                  IX(J-J1+1)=X0(ISTART+J)/(PE(I)*PE(J))*1000.
                  IF(I.EQ.J) IX(J-J1+1)=1000
   40          CONTINUE
   30          WRITE(6,100) I,PNAME(I),P1(I),(IX(J),J=1,J12-J1+1)
   10    CONTINUE
      ELSE
         DO 50 I=1,NPARM
            ISTART=(I*(I-1))/2
   50       WRITE(6,200) (X0(ISTART+J),J=1,I)
      END IF
  100 FORMAT(I5,A12,1PE9.2,10I5)
  200 FORMAT(12E10.3)
  300 FORMAT(/,26X,20I5)
      RETURN
      END
C
C*************************************************************
C     PREAD  : PRIOR PARAMETER READ                          *
C*************************************************************
C
      SUBROUTINE PREAD(KUNIT,NPARM,NPMAX,P1,X0,PRIP,PRIE,PRIC,IPARM)
      PARAMETER(NP=100,MP=NP*(NP+1)/2,NX=10000,MX=NX*(NX+1)/2,NS=100,
     &          NR=30)
      REAL*8 P1(NP),X0(MP),PRIP(NP),PRIE(NP),PRIC(MP)
      REAL*8 PW(NP)
      DIMENSION IPARM(NP)
C
      READ(KUNIT,100)(PRIP(I),I=1,NPMAX)
      READ(KUNIT,100)(PRIE(I),I=1,NPMAX)
      DO 10 I=1,NPMAX
         PRIE(I)=PRIP(I)*PRIE(I)
         ISTART=I*(I-1)/2
         READ(KUNIT,200)(PW(J),J=1,NPMAX)
         DO 20 J=1,I
   20       PRIC(ISTART+J)=PW(J)
   10 CONTINUE
C
      DO 30 I=1,NPMAX
         ISTART=I*(I-1)/2
         DO 40 J=1,I
            IJ=ISTART+J
   40       PRIC(IJ)=PRIC(IJ)*PRIE(I)*PRIE(J)
   30 CONTINUE
C
      DO 50 I=1,NPARM
         P1(I)=PRIP(IPARM(I))
         KI=IPARM(I)
         ISTART=I*(I-1)/2
         DO 60 J=1,I
            KJ=IPARM(J)
            IF(KJ.LE.KI) THEN
               KIJ=KI*(KI-1)/2+KJ
            ELSE 
               KIJ=KJ*(KJ-1)/2+KI
            END IF
   60       X0(ISTART+J)=PRIC(KIJ)
   50 CONTINUE
  100 FORMAT(11E12.5)
  200 FORMAT(20F6.3)
      RETURN
      END
C
C*************************************************************
C     PWRITE : POSTERIOR PARAMETER OUTPUT                    *
C*************************************************************
C
      SUBROUTINE PWRITE(KUNIT,NPARM,NPMAX,P1,PE,PRIP,PRIE,PRIC,IPARM)
      PARAMETER(NP=100,MP=NP*(NP+1)/2,NX=10000,MX=NX*(NX+1)/2,NS=100,
     &          NR=30)
      REAL*8 P1(NP),PE(NP),PRIP(NP),PRIE(NP),PRIC(MP)
      REAL*8 PW(NP)
      DIMENSION IPARM(NP)
C
      WRITE(KUNIT,100)(PRIP(I)        ,I=1,NPMAX)
      WRITE(KUNIT,100)(PRIE(I)/PRIP(I),I=1,NPMAX)
      DO 10 I=1,NPMAX
         DO 20 J=1,NPMAX
            IF(J.LE.I) THEN
               IJ=I*(I-1)/2+J
            ELSE
               IJ=J*(J-1)/2+I
            END IF
            PW(J)=PRIC(IJ)/(PRIE(I)*PRIE(J))
   20    CONTINUE
         WRITE(KUNIT,200)(PW(J),J=1,NPMAX)
   10 CONTINUE
  100 FORMAT(11(1PE12.5))
  200 FORMAT(20F6.3)
      RETURN
      END
C
C*************************************************************
C     INVMTX : CALCULATION IF INVERSE MATRIX                 *
C*************************************************************
C
      SUBROUTINE INVMTX(NDATA,V)
      PARAMETER(NP=100,MP=NP*(NP+1)/2,NX=10000,MX=NX*(NX+1)/2,NS=100,
     &          NR=30)
      REAL*8 V(MX)
      ICOND=0
      IF(NDATA.LT.1) THEN
         WRITE(6,*) 'MATRIX DIMENSION < 1'
         STOP 9999
      END IF
      IF(NDATA.EQ.1) THEN 
         IF(V(1).NE.0.0) THEN
            V(1)=1.0/V(1)
         END IF
      ELSE
         CALL CHOLSK(NDATA,V,ICOND)
         IF(ICOND.EQ.-1) THEN
            WRITE(6,*) 'MATRIX NOT POSITIV'
            STOP 9999
         ELSE IF(ICOND.EQ.-2) THEN 
            WRITE(6,*) 'PIVOT ZERO'
            STOP 9999
         END IF
         CALL INVERS(NDATA,V,ICOND)
         IF(ICOND.EQ.-1) THEN
            WRITE(6,*) 'MATRIX SINGULAR'
            STOP 9999
         END IF
      END IF
      RETURN
      END
C
C*************************************************************
C     CHOLSK : LDU DECOMPOSITION BY MODIFIED CHOLESKI METHOD *
C              L TRIANGULAR MATRIX                           *
C              D INVERSE OF DIAGONAL MATRIX                  *
C              U TRANSPOSE OF L                              *
C*************************************************************
C
      SUBROUTINE CHOLSK(N,V,ICOND)
      PARAMETER(NP=100,MP=NP*(NP+1)/2,NX=10000,MX=NX*(NX+1)/2,NS=100,
     &          NR=30)
      REAL*8 V(MX)
      REAL*8 X1,XX,EPS
C
      EPS=1.0E-72
      ICOND=0
      If(V(1).EQ.0.0) THEN
         ICOND=-2
         GOTO 1000
      ELSE IF(V(1).LT.0.0) THEN
         ICOND=-1
         GOTO 1000
      END IF
C
      V(1)=1.0/V(1)
      X1  =V(3)-V(1)*V(2)*V(2)
      V(2)=V(1)*V(2)
      IF( DABS(X1).LT.DABS(V(3)*EPS) ) THEN
         ICOND=-2
         GO TO 1000
      END IF
      IF(X1.LT.0.0) THEN
         ICOND=-1
         GO TO 1000
      END IF
      V(3)=1.0/X1
C***************************************
C  FOR N=2                             *
C                                      *
C  V1  V2       1 /V1  V2/ V1          *
C  V2  V3  -->  V2/V1  V1/(V1*V3-V2*V2)*
C                                      *
C***************************************
      IF (N.EQ.2) GO TO 1000
C
      IV=4
      DO 10 I=1,N-2
         JK=2
         IK=IV
         DO 20,J=1,I
            XX=0.0
            DO 30 IJ=IV,IV+J-1
               XX=XX+V(IJ)*V(JK)
               JK=JK+1
   30       CONTINUE
            JK=JK+1
            IK=IK+1
            V(IK)=V(IK)-XX
   20    CONTINUE
         JJ=1
         IJ=IV
         XX=0.0
         DO 50 J=2,I+2
            X1=V(IJ)*V(JJ)
            XX=XX+V(IJ)*X1
            V(IJ)=X1
            JJ=JJ+J
            IJ=IJ+1
   50    CONTINUE
         X1=V(JJ)-XX
         IF( DABS(X1) .LT. DABS(V(JJ)*EPS) ) THEN
           ICOND=-2
           GOTO 1000
         END IF
         IF(X1.LT.0.0) THEN
           ICOND=-1
           GOTO 1000
         END IF
         V(JJ)=1.0/X1
         IV=JJ+1
   10 CONTINUE
 1000 RETURN
      END
C
C*************************************************************
C     INVERS : INVERSION OF LDU DECOMPOSED MATRIX            *
C              V=LDU                                         *
C              V**(-1)= U**(-1) D L**(-1)                    *
C*************************************************************
C
      SUBROUTINE INVERS(N,V,ICOND)
      PARAMETER(NP=100,MP=NP*(NP+1)/2,NX=10000,MX=NX*(NX+1)/2,NS=100,
     &          NR=30)
      REAL*8 V(MX)
      REAL*8 X1,XX
C
      ICOND=0
      V(2)=-V(2)
      IF(N.NE.2) THEN
         IJ2= 2
         DO 10,I=2,N-1
            IJ1=IJ2+2
            IJ2=IJ2+I+1
            KJ1=0
            DO 20 J=2,I
               XX =V(IJ1)
               IJ1 =IJ1+1
               KJ2 =J
               KJ1 =KJ1+KJ2
               KJ  =KJ1
               DO 30 IK=IJ1,IJ2
                  XX =XX+V(IK)*V(KJ)
                  KJ  =KJ+KJ2
                  KJ2 =KJ2+1
   30          CONTINUE
               V(KJ)=-XX
   20       CONTINUE
            V(IJ2)=-V(IJ2)
   10    CONTINUE
      END IF
C
      II= 1
      KI= 2
      XX=V(1)
      DO 40 K=2,N
         II=II+K
         IF(V(II).LT.0.0) THEN
            ICOND=-1
            GO TO 1000
         END IF
         X1=V(KI)*V(II)
         XX=XX+V(KI)*X1
         V(KI)=X1
         KI=KI+K
   40 CONTINUE
      V(1)=XX
      IF(N.EQ.2) GO TO 1000
      IJ=1
      DO 50 I=2,N-1
         DO 60 J=2,I
            IJ=IJ+1
            KJ=IJ
            XX=V(KJ)
            IJSP=I+1-J
            DO 70 K=I,N-1
               KJ=KJ+K
               KI=KJ+IJSP
               XX=XX+V(KJ)*V(KI)
   70       CONTINUE
            V(IJ)=XX
   60    CONTINUE
         IJ=IJ+1
         KI=IJ+I
         II=IJ
         XX=V(II)
         DO 80 K=I+1,N
            II=II+K
            X1=V(KI)*V(II)
            XX=XX+V(KI)*X1
            V(KI)=X1
            KI=KI+K
   80    CONTINUE
         V(IJ)=XX
   50 CONTINUE
 1000 RETURN
      END
