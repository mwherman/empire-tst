Ccc   * $Author: mike $
Ccc   * $Date: 2001-07-09 17:33:38 $
Ccc   * $Id: MSD-orion3.f,v 1.1.1.1 2001-07-09 17:33:38 mike Exp $
C
      SUBROUTINE ORION(Q2, Q3, Ktrl3, Extcom1, Ldw, Ist, Ltrmax, Atar, 
     &                 Ztar, Apro, Zpro, Iout, Angle, Ndang, Icompff)
Ccc
Ccc ********************************************************************
Ccc *                                                         class:fpu*
Ccc *                         O R I O N   3                            *
Ccc *                                                                  *
Ccc * Calculates two step Multi-step Direct amplitudes in the frame of *
Ccc * the TUL theory. The  results are later used by the TRISTAN       *
Ccc * to produce MSDR cross sections.                                  *
Ccc *                                                                  *
Ccc *                                                                  *
Ccc * input:Q2     - first step energy loss                            *
Ccc *       Q3     - total energy loss                                 *
Ccc *       KTRL3  - optical model parameter set                       *
Ccc *       EXTCOM1- incident energy (c.m.)                            *
Ccc *       LDW    - maximum l to be considered                        *
Ccc *       IST    - spin of the particle in the continuum (times 2)   *
Ccc *       LTRMAX - maximum l transfer                                *
Ccc *       ATAR   - target A                                          *
Ccc *       ZTAR   - target Z                                          *
Ccc *       APRO   - projectile A                                      *
Ccc *       ZPRO   - projectile Z                                      *
Ccc *       IOUT   - controls output amount (0-6)                      *
Ccc *       ANGLE  - matrix of angles                                  *
Ccc *       NDANG  - ANGLE matrix dimension                            *
Ccc *                                                                  *
Ccc *                                                                  *
Ccc * output:TAPE15                                                    *
Ccc *                                                                  *
Ccc *                                                                  *
Ccc * calls:  CCCTRL                                                   *
Ccc *             FLGLCH                                               *
Ccc *             OMPOTEN                                              *
Ccc *         FFCAL                                                    *
Ccc *         HIBORN                                                   *
Ccc *             MSTEP                                                *
Ccc *         OPMPARN                                                  *
Ccc *             OMPAR                                                *
Ccc *             WHERE                                                *
Ccc *         XSEC                                                     *
Ccc *             LEGNDR                                               *
Ccc *             XSC12                                                *
Ccc *                 CLEBSCH                                          *
Ccc *                 XSCABC                                           *
Ccc *                     CLEBHF                                       *
Ccc *                     CLEBRD                                       *
Ccc *                     CLEBZ                                        *
Ccc *                     RACHLF                                       *
Ccc *                     RACSIM                                       *
Ccc *                                                                  *
Ccc *                                                                  *
Ccc *                                                                  *
Ccc * author: H.Lenske                                                 *
Ccc *                                                                  *
Ccc * adapted by: M.Herman                                             *
Ccc * date:   25.Sep.1996                                              *
Ccc *                                                                  *
Ccc * revision:1    by:M.Herman                 on: 2.Nov.1997         *
Ccc * Compressional form factor in l=0 transfer implemented (see       *
Ccc * modification to FFCAL by H. Lenske).                             *
Ccc *                                                                  *
Ccc ********************************************************************
Ccc
CMAD
CMAD      **** WARNING **** WARNING **** WARNING **** WARNING *******
CMAD      *                                                         *
CMAD      * THIS VERSION IS FOR INELASTIC SCATTERING ONLY           *
CMAD      * THE SUBROUTINES BECTRL,UNCPLB AND POTFF ARE MISSING AND *
CMAD      * FFCAL IS MODIFIED SO THAT CALCULATION OF TRANSFER       *
CMAD      * CROSS SECTIONS IS NO LONGER POSSIBLE                    *
CMAD      *                                                         *
CMAD      ***********************************************************
CMAD
CCCCCC      IF KTRL(3).GE.0 CARD-9 IS NOT NEEDED.
CCCCCC      IF KTRL(3).LT.0 CARD-9 IS NEEDED
CCCCCC      IF KTRL(4)=1 VSP=0, EVEN IF NON-ZERO VSP IS INPUT.
CCCCCC      KEXCOM(2)=NXMAX=NXCPLE IF NON-ZERO (CCCTRL,SN-181)
CCCCCC      KEXCOM(40,41,42) ARE USED IN BECTRL,UNCPLB ETC
CCCCCC      KEXCOM(45)=JJ
CCCCCC      EXTCOM(1)=ELAB
CCCCCC      EXTCOM(2)=XMES
CCCCCC      KTLOUT(1)   TO OUTPUT FORM FACTOR AND RELATED QUANTITIES
CCCCCC      KTLOUT(2)=1 TO OUTPUT COULOMB FUNCTIONS
CCCCCC      KTLOUT(2)=2 TO OUTPUT COULOMB FUNCTIONS AND OPTICAL POTENTIALS
CCCCCC      KTLOUT(3)   TO OUTPUT C-MATRIX FROM HIBORN
CCCCCC      KTLOUT(4)   TO OUTPUT QUANTITITES IN XSEC
CCCCCC      KTLOUT(6)   TO OUTPUT SGMA AND SGMAT IN XSC12
CCCCCC
CCCCCC      USE OF LBTRF(N),NODF(N) -- (SEE SN.9007)
CCCCCC
CCCCCC
CCCCCC          FOR TRANSFER MODE LBTRF AND NODF ARE TRUE VALUES
CCCCCC          OF LB AND NODE OF THE BOUND STATE WAVE FUNCTION.
CCCCCC          POTENTIAL PARAMETERS ARE THOSE GIVEN BY CARD 8-N.
CCCCCC
CCCCCC          FOR INELASTIC MODE LBTRF(N) MUST BE NEGATIVE.
CCCCCC          KDER=-LBTRF(N) SELECTTS A TYPE OF THE DERIVATIVE FOR
CCCCCC          THE FORM FACTOR (SEE FFCAL). IF NODF(N)=0, PARAMETERS
CCCCCC          ON CARD 8-N ARE USED. IF NC=NODF(N) IS NON-ZERO,
CCCCCC          OPTICAL PARAMETERS FOR NC-TH CHANNEL ARE USED.
CCCCCC
CCCCCC      DZERO(N)=D-ZERO (ABOUT 100.) FOR TRANSFER REACTION
CCCCCC      DZERO(N)=BETA-VAL (ABOUT 0.02) FOR INELASTIC SCATTERING.
CCCCCC
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C
C PARAMETER definitions
C
      INTEGER NGLXX
      PARAMETER(NGLXX = 100)
C
C COMMON variables
C
      DOUBLE PRECISION ANGler(NGLXX), ARAtio(4), C1Mem(2), CE(4), 
     &                 CFUnir(4), CHArgr(4), CONst1, CONst2, DFNf(4), 
     &                 DFNr(4), DFNsf(4), DFNsir(4), DFNspf(4), 
     &                 DFNspr(4), DFNsr(4), DFNwf(4), DFNwr(4), DZEro(4)
     &                 , ECM(4), EGS(4), ELAb, ETA, ETUnit, EXTcom(50), 
     &                 EXTcom2(10), FAClm(NGLXX), FAClog(500), 
     &                 P(975, NGLXX), pl(30), plm10, PLM10m(NGLXX), 
     &                 plm20, PLM20m(NGLXX), PMAsr(4), QVAlue(4), RAC, 
     &                 RACie(50), radian, RD, RHOmx, RMAsr(4), RZEcf(4), 
     &                 RZEcr(4), RZEf(4), RZEr(4), RZEsf(4), RZEsir(4), 
     &                 RZEspf(4), RZEspr(4), RZEsr(4), RZEwf(4), 
     &                 RZEwr(4), SGMa(25, NGLXX, 2), SGMat(NGLXX, 2), 
     &                 SGMaz, SGMazz(4), SQRt10, THEta(NGLXX), TMAsr(4), 
     &                 U9, VSOf(4), VSOr(4), VSXf(4), VSXr(4), WN(4), 
     &                 WNIni(4), WNUnit, WR1(1000, 2), WR2(5000, 2), 
     &                 WSFf(4), WSFr(4), WSOr(4), WSXf(4), WSXr(4), 
     &                 XBAr, XMAx, XMEs, ZPR(4), ZTR(4)
      DOUBLE COMPLEX CSUm2(NGLXX), TTI, TTR, XAMp(8300, 4), ZERo
      INTEGER ia, ib, ic, id, ie, ig, ISTw(3), JJ, JLSmax, KCFf(4), 
     &        KEXcom(50), KEXcom1(8), KEXcom2(28), KTLout(50), 
     &        KTLout1(8), KTLout2(28), KTRl(30), KTRl1(8), KTRl2(28), 
     &        l1outp, l9(9), LBTrf(4), lcaltr, LDWmxr(4), LDWmxr1(3), 
     &        LLRow(120), LMAx, LTRamx(4), mmxtr, MXRow, NANglr, NCHanl, 
     &        NNDim(4), NODf(4), NXCple, NXMax
      COMMON /BRMH  / WR1, WR2, SGMa, SGMat, P, CSUm2, XAMp, PLM10m, 
     &                PLM20m, FAClm, RACie, C1Mem, SQRt10, CONst1, 
     &                CONst2
      COMMON /CHANEL/ TMAsr, PMAsr, RMAsr, CHArgr, ARAtio, CFUnir, VSXr, 
     &                WSXr, WSFr, VSOr, DFNr, DFNwr, DFNsr, DFNspr, 
     &                RZEr, RZEwr, RZEsr, RZEspr, RZEcr, LLRow, NNDim, 
     &                QVAlue, ECM, CE, WN, WNIni, SGMazz
      COMMON /CNTROL/ KTRl, KEXcom, EXTcom, KTLout
      COMMON /FOFINT/ EGS, DZEro, VSXf, WSXf, WSFf, VSOf, DFNf, DFNwf, 
     &                DFNsf, DFNspf, RZEf, RZEwf, RZEsf, RZEspf, RZEcf, 
     &                ZTR, ZPR
      COMMON /FOFINTI/ JLSmax, NODf, LBTrf, KCFf
      COMMON /PARAMT/ NCHanl, MXRow, NXMax, NXCple, NANglr, LMAx, 
     &                LTRamx, WNUnit, ETUnit, XMEs, ANGler, THEta, TTR, 
     &                TTI, ZERo, ELAb, ETA, XBAr, XMAx, SGMaz, RHOmx, 
     &                RD, LDWmxr
      COMMON /RACFAC/ FAClog, RAC, U9
      COMMON /SOIMAG/ WSOr, DFNsir, RZEsir
      COMMON /SPIN  / ISTw, JJ
C
C Dummy arguments
C
      DOUBLE PRECISION Apro, Atar, Extcom1, Q2, Q3, Zpro, Ztar
      INTEGER Iout, Ist, Ktrl3, Ldw, Ltrmax, Ndang
      DOUBLE PRECISION Angle(Ndang)
C
C Local variables
C
      DOUBLE PRECISION ad, ai, amupmu, as, av, aw, e, ec, fn, rc, ri, 
     &                 rs, rv, rw, v, vi, vs, w, wd, xwr1(1000, 2), 
     &                 xwr2(5000, 2)
      CHARACTER*3 ampmwr, holamu, holpmu
      INTEGER i, ind, j, kase, kder, maxi, mini, n, n1mx, n1wx, n2mx, 
     &        na, nc, nejc, nlr, no, nw1, nw2, nw3, nz
C
C
C
C----ten common dodalem 27.10.98 bo wydawal sie brakowac
C     COMMON /BIMH/ISTW1, ISTW2, ISTW3, JAS, JBS, JCS, L1MAXM, L2MAXM,
C     1   L12MXM, J12MXT, LDWMXA, LDWMXB, LDWMXC, KASE, NBSAMP(40), L1TR
C     2   , L2TR, L1TW, L2TW, J1TW, J2TW, JLMITW, JLMXTW, JLRANG
C
CWQAN
      EQUIVALENCE(EXTcom(1), EXTcom2)
      EQUIVALENCE(KTLout(1), KTLout2)
      EQUIVALENCE(KEXcom(1), KEXcom2)
      EQUIVALENCE(KTRl(1), KTRl2)
      EQUIVALENCE(LDWmxr(1), LDWmxr1)
      EQUIVALENCE(KTLout(1), KTLout1)
      EQUIVALENCE(KEXcom(1), KEXcom1)
      EQUIVALENCE(KTRl(1), KTRl1)
      DATA holamu, holpmu/'AMU', 'PMU'/
      OPEN(8, FORM = 'unformatted', STATUS = 'scratch')
C
CWQAN
      QVAlue(2) = Q2
      QVAlue(3) = Q3
      KTRl(3) = Ktrl3
      EXTcom(1) = Extcom1*(Atar + Apro)/Atar
C
Cmh   READ(5,*) QVALUE(2),QVALUE(3)
      QVAlue(1) = 0.
C
      DO no = 9, 28
         KTRl(no) = 0
         KEXcom(no) = 0
         KTLout(no) = 0
      ENDDO
C
      TTR = (1., 0.)
      TTI = (0., 1.)
      ZERo = (0., 0.)
C
      FAClog(1) = 0.
      FAClog(2) = 0.
      fn = 1.
      DO n = 3, 500
         fn = fn + 1.
         FAClog(n) = FAClog(n - 1) + LOG(fn)
      ENDDO
C
      amupmu = 0.
      ETUnit = 0.15745400
      WNUnit = 0.21870660
C
Cmh   READ(5,*)(KTRL(N),N=1,8)
Cmh   set control parameters, KTRL(3) defines set of o.m. parameters
Cmh   and is passed through formal parameters of the routine
      KTRl(1) = 0
      KTRl(2) = 0
      KTRl(4) = 0
      KTRl(5) = 0
      KTRl(6) = 0
      KTRl(7) = 0
      KTRl(8) = 0
Cmh   READ(5,*)(KEXCOM(N),N=1,8)
      KEXcom(1) = 0
      KEXcom(2) = 140
      KEXcom(3) = 0
      KEXcom(4) = 0
      KEXcom(5) = 0
      KEXcom(6) = 0
      KEXcom(7) = 0
      KEXcom(8) = 0
Cmh   READ(5,*)(KTLOUT(N),N=1,8)
Cmh   KTLOUT(1)=1 outputs form factor and related quantities
      KTLout(1) = 0
      IF(Iout.GT.5)KTLout(1) = 1
Cmh   KTLOUT(2)=1 outputs Coulomb functions and optical potentials
      KTLout(2) = 0
Cmh   KTLOUT(3)=1 outputs C-matrix from HIBRON
      KTLout(3) = 0
Cmh   KTLOUT(4)=1 outputs cross sections
      KTLout(4) = 0
      IF(Iout.GT.4)KTLout(4) = 1
      KTLout(5) = 0
      KTLout(6) = 0
      KTLout(7) = 0
      KTLout(8) = 0
Cmh   READ(5,*)(EXTCOM(N),N=1,10)
Cmh   EXTCOM(1) defines incident energy (passed trough formal parameters)
Cmh   EXTCOM(2) defines mesh
      EXTcom(2) = 0.1
      DO n = 3, 10
         EXTcom(n) = 0.0
      ENDDO
Cmh   READ(5,*) NCHANL,NANGLR,(LDWMXR(N),N=1,3),(ISTW(N),N=1,3)
Cmh   NCHANL defines number of steps (NCHANL=3 means 2 steps)
      NCHanl = 3
Cmh   number of angles
      NANglr = -Ndang
Cmh   LDWMXR defines maximum number of partial waves l
Cmh   ISTW   defines spin of the particle in the continuum times 2
      DO n = 1, 3
         LDWmxr(n) = Ldw
         ISTw(n) = Ist
      ENDDO
      DO i = 1, NCHanl
Cmh      READ(5,*)TMASR(I),PMASR(I),ZTR(I),ZPR(I)
Cmh      TMASR defines target mass
         TMAsr(i) = Atar
Cmh      PMASR defines projectile mass
         PMAsr(i) = Apro
Cmh      ZTR defines target Z
         ZTR(i) = Ztar
Cmh      ZPR defines projectile Z
         ZPR(i) = Zpro
      ENDDO
C
      ELAb = EXTcom(1)
      XMEs = EXTcom(2)
      JLSmax = NCHanl - 1
C
Cmh   READ(5,*)(LTRAMX(J),J=1,JLSMAX)
Cmh   set maximum l-transfer
      DO j = 1, JLSmax
         LTRamx(j) = Ltrmax
      ENDDO
      IF(NANglr.GT.0)THEN
         READ(5, *)(ANGler(n), n = 1, NANglr)
      ELSE
         NANglr = -NANglr
         DO na = 1, NANglr
            ANGler(na) = Angle(na)
         ENDDO
      ENDIF
C
      DO j = 1, JLSmax
Cmh      READ(5,*)LBTRF(J),NODF(J),KCFF(J),EGS(J),DZERO(J)
Cmh      LBTRF must be negative for inelastic
         LBTrf(j) = -1
         NODf(j) = 1
         KCFf(j) = 1
         EGS(j) = 0.0
Cmh      DZERO must be 1.0 for inelastic
         DZEro(j) = 1.0
      ENDDO
C
      DO n = 1, NANglr
         THEta(n) = ANGler(n)*0.0174532925
      ENDDO
C
C
      IF(KTRl(3).LT.0)THEN
C
         DO n = 1, NCHanl
            CHArgr(n) = ZTR(n)*ZPR(n)
Cmh         optical potential parameters (not read since only global
Cmh         potentials are allowed for the time being)
Cmh         READ(5,*)VSXR(N),WSXR(N) ,WSFR(N) ,VSOR(N)  ,
Cmh         *          DFNR(N),DFNWR(N),DFNSR(N),DFNSPR(N),
Cmh         *          RZER(N),RZEWR(N),RZESR(N),RZESPR(N),RZECR(N)
         ENDDO
C
      ELSE
C
         DO n = 1, NCHanl
            CHArgr(n) = ZTR(n)*ZPR(n)
Cmh         TFAC=(TMASR(N)-2.*ZTR(N))/TMASR(N)
Cmh         ZFAC=ZTR(N)/(TMASR(N)**.3333333333)
            e = ELAb
            IF(n.NE.1)THEN
               ec = ELAb*TMAsr(1)/(TMAsr(1) + PMAsr(1)) - QVAlue(n)
               e = ec*(TMAsr(n) + PMAsr(n))/TMAsr(n)
            ENDIF
            nz = ZPR(n) + .1
            na = PMAsr(n) + .1
C
C------definition of NEJC (1,2,3,4 for n,p,alpha, light ion)
C
            nejc = 4
            IF(na.EQ.1 .AND. nz.EQ.0)nejc = 1
            IF(na.EQ.1 .AND. nz.EQ.1)nejc = 2
            IF(na.EQ.2 .AND. nz.EQ.2)nejc = 3
C
C
            CALL OPMPARN(Atar, Ztar, nejc, e, v, w, wd, vs, vi, av, aw, 
     &                   ad, as, ai, rv, rw, RD, rs, ri, rc)
C
            IF(KTRl(4).EQ.1)vs = 0.0
            VSXr(n) = v
            WSXr(n) = w
            WSFr(n) = wd
            VSOr(n) = vs
            WSOr(n) = vi
            DFNr(n) = av
            DFNwr(n) = aw
            DFNsr(n) = ad
            DFNspr(n) = as
            DFNsir(n) = ai
            RZEr(n) = rv
            RZEwr(n) = rw
            RZEsr(n) = RD
            RZEspr(n) = rs
            RZEsir(n) = ri
            RZEcr(n) = rc
C
            IF(EXTcom(5).GT.0.0 .AND. n.EQ.2)RZEr(n) = EXTcom(5)*RZEr(n)
            IF(EXTcom(6).GT.0.0 .AND. n.EQ.3)RZEr(n) = EXTcom(6)*RZEr(n)
            IF(EXTcom(7).GT.0.0 .AND. n.EQ.1)VSOr(n) = EXTcom(7)*VSOr(n)
C
         ENDDO
C
      ENDIF
C
C
      DO n = 1, JLSmax
         IF(LBTrf(n).LT.0 .AND. NODf(n).NE.0)THEN
            nc = NODf(n)
            VSXf(n) = VSXr(nc)
            WSXf(n) = WSXr(nc)
            WSFf(n) = WSFr(nc)
            VSOf(n) = VSOr(nc)
            DFNf(n) = DFNr(nc)
            DFNwf(n) = DFNwr(nc)
            DFNsf(n) = DFNsr(nc)
            DFNspf(n) = DFNspr(nc)
            RZEf(n) = RZEr(nc)
            RZEwf(n) = RZEwr(nc)
            RZEsf(n) = RZEsr(nc)
            RZEspf(n) = RZEspr(nc)
            RZEcf(n) = RZEcr(nc)
         ENDIF
      ENDDO
C
      IF(Iout.GT.3)THEN
         WRITE(6, 99001)
99001    FORMAT('1'//)
         WRITE(6, 99002)ELAb
99002    FORMAT(24X, 10('*'), 6X, 
     &          'MULTI-STEP DIRECT-REACTION CALCULATION', 6X, 10('*')
     &          /39X, '(ON PROGRAM ORION3, Munich/Giessen 1997)'/47X, 
     &          '(ELAB=', F8.3, ' [MeV])')
      ENDIF
C
      IF(Iout.GT.3)THEN
         DO n = 1, NCHanl
            WRITE(6, 99003)n, TMAsr(n), PMAsr(n), ZTR(n), ZPR(n), 
     &                     LDWmxr(n), ISTw(n)
C
99003       FORMAT(/21X, 'CHANNEL NO.', I1, 6X, 'TMAS=', F5.1, 2X, 
     &             'PMAS=', F5.1, 2X, 'ZT=', F5.1, '  ZP=', F5.1, 3X, 
     &             'LDWMX=', I2, 3X, 'ISTW=', I1/)
            WRITE(6, 99014)VSXr(n), WSXr(n), WSFr(n), VSOr(n), DFNr(n), 
     &                     DFNwr(n), DFNsr(n), DFNspr(n), RZEr(n), 
     &                     RZEwr(n), RZEsr(n), RZEspr(n), RZEcr(n)
         ENDDO
      ENDIF
C
      IF(Iout.GT.3)THEN
         WRITE(6, 99004)(LTRamx(j), QVAlue(j + 1), j = 1, JLSmax)
C
99004    FORMAT(/51X, 'TRANSITIONS ARE'/42X, 'LTRAMX=', I2, 5X, 
     &          'QVALUE.(-1)=', F7.3)
         WRITE(6, 99005)
99005    FORMAT(/, 48X, 'FORM FACTOR PARAMETERS')
      ENDIF
C
C
      DO n = 1, JLSmax
         IF(LBTrf(n).GE.0)THEN
            IF(Iout.GT.3)WRITE(6, 99006)n, LBTrf(n), NODf(n), EGS(n), 
     &                                  DZEro(n)
C
C
99006       FORMAT(/21X, 'JLS=', I1/21X, 'LBTR=', I2, 2X, 'NOD=', I2, 
     &             2X, 'B.E.=', F7.4, 3X, 'DZERO=', F7.1)
         ELSE
            nc = NODf(n)
            kder = -LBTrf(n)
            IF(Iout.GT.3)WRITE(6, 99007)n, nc, kder, KCFf(n), DZEro(n)
99007       FORMAT(/21X, 'JLS=', I1/21X, 'NC=', I1, 2X, 'KDER=', I1, 2X, 
     &             'KCFF=', I1, 5X, 'BETA=', F7.4)
         ENDIF
         IF(Iout.GT.3)WRITE(6, 99014)VSXf(n), WSXf(n), WSFf(n), VSOf(n), 
     &                               DFNf(n), DFNwf(n), DFNsf(n), 
     &                               DFNspf(n), RZEf(n), RZEwf(n), 
     &                               RZEsf(n), RZEspf(n), RZEcf(n)
      ENDDO
C
C
      IF(Iout.GT.3)WRITE(6, 99008)(i, i = 1, 28)
C
99008 FORMAT(////7X, 28I4)
      IF(Iout.GT.3)THEN
C
         WRITE(6, 99009)KTRl2
C
99009    FORMAT(' KTRL  ', 28I4)
         WRITE(6, 99010)KEXcom2
99010    FORMAT(' KEXCOM', 28I4)
         WRITE(6, 99011)KTLout2
99011    FORMAT(' KTLOUT', 28I4)
         WRITE(6, 99012)EXTcom2
99012    FORMAT(' EXTCOM', 10F10.5)
      ENDIF
C
      ampmwr = holamu
      IF(amupmu.NE.0.D0)ampmwr = holpmu
      IF(Iout.GT.3)WRITE(6, 99013)XMEs, ampmwr
99013 FORMAT(' XMES =', F7.5, 3X, ' UNIT =', A3)
C
C
      n1mx = (LTRamx(1) + 1)*NANglr
      n2mx = (LTRamx(2) + 1)*n1mx
      n1wx = n1mx + KTRl(8)*n1mx
C
C-----clear 1-step and 2-step cross section matrices
      DO nlr = 1, 2
         DO nw1 = 1, n1wx
            xwr1(nw1, nlr) = 0.
         ENDDO
         DO nw2 = 1, n2mx
            xwr2(nw2, nlr) = 0.
         ENDDO
      ENDDO
C
      CALL CCCTRL(Iout)
C-----all l-transfers for both steps (calculated using surface form factor)
C     WRITE(6,*)'L transfer maxs: ',(LTRAMX(J),J=1,JLSMAX)
C     WRITE(6,*)'Form factor    : ',(LBTRF(j),J=1,JLSMAX)
      CALL FFCAL
      CALL HIBORN
      CALL XSEC
C-----transfer cross sections onto XWR* matrices
      DO nlr = 1, 2
         DO nw1 = 1, n1wx
            xwr1(nw1, nlr) = WR1(nw1, nlr)
         ENDDO
         DO nw2 = 1, n2mx
            xwr2(nw2, nlr) = WR2(nw2, nlr)
         ENDDO
      ENDDO
C-----next line if controls use of the compressional l=0 form factor
      IF(Icompff.GT.0)THEN
C--------all l-transfers in the first step (calculated using surface form factor)
C--------only l=0 transfer in the second step (calculated with the copmressional
C--------form factor)
         LTRamx(1) = Ltrmax
         LBTrf(1) = -1
         LTRamx(2) = 0
         LBTrf(2) = -6
C        WRITE(6,*)'L transfer maxs: ',(LTRAMX(J),J=1,JLSMAX)
C        WRITE(6,*)'Form factor    : ',(LBTRF(j),J=1,JLSMAX)
         CALL FFCAL
         CALL HIBORN
         CALL XSEC
C--------transfer cross sections onto XWR2 matrices
         DO nlr = 1, 2
            DO nw3 = 1, Ltrmax + 1
               mini = (nw3 - 1)*(Ltrmax + 1)*NANglr + 1
               maxi = mini + NANglr - 1
               DO nw2 = mini, maxi
                  ind = (nw3 - 1)*NANglr + nw2 - mini + 1
                  xwr2(nw2, nlr) = WR2(ind, nlr)
               ENDDO
            ENDDO
         ENDDO
C--------only l=0 transfer in the first step (calculated with the copmressional
C--------form factor)
C--------all l-transfers in the second step (calculated using surface form
         LTRamx(1) = 0
         LBTrf(1) = -6
         LTRamx(2) = Ltrmax
         LBTrf(2) = -1
C        WRITE(6,*)'L transfer maxs: ',(LTRAMX(J),J=1,JLSMAX)
C        WRITE(6,*)'Form factor    : ',(LBTRF(j),J=1,JLSMAX)
         CALL FFCAL
         CALL HIBORN
         CALL XSEC
C--------transfer cross sections onto XWR2 matrices
         DO nlr = 1, 2
            DO nw3 = 1, Ltrmax
               mini = nw3*NANglr + 1
               maxi = mini + NANglr - 1
               DO nw2 = mini, maxi
                  xwr2(nw2, nlr) = WR2(nw2, nlr)
               ENDDO
            ENDDO
         ENDDO
C--------only l=0 transfer in the first step (calculated with the copmressional
C--------form factor)
C--------only l=0 transfer in the second step (calculated with the copmressional
C--------form factor)
         LTRamx(1) = 0
         LBTrf(1) = -6
         LTRamx(2) = 0
         LBTrf(2) = -6
C        WRITE(6,*)'L transfer maxs: ',(LTRAMX(J),J=1,JLSMAX)
C        WRITE(6,*)'Form factor    : ',(LBTRF(j),J=1,JLSMAX)
         CALL FFCAL
         CALL HIBORN
         CALL XSEC
C--------transfer cross sections onto XWR* matrices
         DO nlr = 1, 2
            DO nw1 = 1, NANglr
               xwr1(nw1, nlr) = WR1(nw1, nlr)
            ENDDO
            DO nw2 = 1, NANglr
               xwr2(nw2, nlr) = WR2(nw2, nlr)
            ENDDO
         ENDDO
      ENDIF   !on removing compressional form factor for dl=0
C
C-----write results to TAPE15
      DO nlr = 1, 2
         IF(kase.NE.2)THEN
            WRITE(15, *)(xwr1(n, nlr), n = 1, n1wx)
            IF(NCHanl.EQ.2)GOTO 100
         ENDIF
         WRITE(15, *)(xwr2(n, nlr), n = 1, n2mx)
 100  ENDDO
99014 FORMAT(21X, 'VSX,WSX,WSF,VSO', 18X, '=', 4F8.3/21X, 
     &       'DFN,DFNW,DFNS,DFNSP', 14X, '=', 4F8.3/21X, 
     &       'RZERO,RZEROW,RZEROS,RZROSP,RZEROC=', 5F8.3)
      END
C
      SUBROUTINE OPMPARN(Atar, Ztar, Nejc, E, V, W, Wd, Vs, Vi, Av, Aw, 
     &                   Ad, As, Ai, Rv, Rw, Rd, Rs, Ri, Rc)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C
C Dummy arguments
C
      DOUBLE PRECISION Ad, Ai, As, Atar, Av, Aw, E, Rc, Rd, Ri, Rs, Rv, 
     &                 Rw, V, Vi, Vs, W, Wd, Ztar
      INTEGER Nejc
C
C Local variables
C
      INTEGER iloc, izaf, nnuc
      INTEGER INT
C
C
      izaf = Ztar*1000 + Atar
      CALL WHERE(izaf, nnuc, iloc)
      IF(iloc.EQ.1)THEN
         WRITE(6, *)' ORION has been called for the nucleus Z=', 
     &              INT(Ztar), ' A=', INT(Atar)
         WRITE(6, *)' which is not defined in the table of nuclei'
         WRITE(6, *)' EXECUTION STOPPED !!!!'
         STOP
      ENDIF
      CALL OMPAR(Nejc, nnuc, E)
      Rv = RVOm(1, Nejc, nnuc)
      Av = AVOm(Nejc, nnuc)
      Rw = RWOm(1, Nejc, nnuc)
      Aw = AWOm(Nejc, nnuc)
      Rd = RWOmv(1, Nejc, nnuc)
      Ad = AWOmv(Nejc, nnuc)
      Rs = RVSo(1, Nejc, nnuc)
      As = AVSo(Nejc, nnuc)
      Rc = RCOul(Nejc, nnuc)
      V = VOM(1, Nejc, nnuc)
      W = WOMv(1, Nejc, nnuc)
      Wd = WOMs(1, Nejc, nnuc)
      Vs = VSO(1, Nejc, nnuc)
      Vi = 0.0
      Ai = 1.0
      Ri = 1.0
      END
C
C***********************************************************************
      SUBROUTINE CCCTRL(Iout)
C***********************************************************************
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C
C PARAMETER definitions
C
      INTEGER NGLXX
      PARAMETER(NGLXX = 100)
C
C COMMON variables
C
      DOUBLE PRECISION ANGler(NGLXX), ARAtio(4), CE(4), CFUnir(4), 
     &                 CHArgr(4), DFNr(4), DFNspr(4), DFNsr(4), DFNwr(4)
     &                 , ECM(4), ELAb, ETA, ETUnit, EXTcom(50), F(70), 
     &                 FC(50, 3), FD(70), FDC(50, 3), G(70), GC(50, 3), 
     &                 GD(70), GDC(50, 3), PMAsr(4), QVAlue(4), RD, 
     &                 RHOmx, RMAsr(4), RZEcr(4), RZEr(4), RZEspr(4), 
     &                 RZEsr(4), RZEwr(4), SGMaz, SGMazz(4), 
     &                 THEta(NGLXX), TMAsr(4), VSOr(4), VSXr(4), WN(4), 
     &                 WNIni(4), WNUnit, WSFr(4), WSXr(4), XBAr, XMAx, 
     &                 XMEs
      DOUBLE COMPLEX EXSgri(50, 3), TTI, TTR, ZERo
      INTEGER KEXcom(50), KTLout(50), KTRl(30), LDWmxr(4), LLRow(120), 
     &        LMAx, LTRamx(4), MXRow, NANglr, NCHanl, NNDim(4), NXCple, 
     &        NXMax
      COMMON F, FD, G, GD
      COMMON /CHANEL/ TMAsr, PMAsr, RMAsr, CHArgr, ARAtio, CFUnir, VSXr, 
     &                WSXr, WSFr, VSOr, DFNr, DFNwr, DFNsr, DFNspr, 
     &                RZEr, RZEwr, RZEsr, RZEspr, RZEcr, LLRow, NNDim, 
     &                QVAlue, ECM, CE, WN, WNIni, SGMazz
      COMMON /CNTROL/ KTRl, KEXcom, EXTcom, KTLout
      COMMON /COUWF / EXSgri, FC, FDC, GC, GDC
      COMMON /PARAMT/ NCHanl, MXRow, NXMax, NXCple, NANglr, LMAx, 
     &                LTRamx, WNUnit, ETUnit, XMEs, ANGler, THEta, TTR, 
     &                TTI, ZERo, ELAb, ETA, XBAr, XMAx, SGMaz, RHOmx, 
     &                RD, LDWmxr
C
C Dummy arguments
C
      INTEGER Iout
C
C Local variables
C
      DOUBLE PRECISION a1, aovb, charge, denom, dx, e1, einv1, einv2, 
     &                 einv3, einv5, einv7, einv9, eta2, eta6, etasq, 
     &                 fl, fmodtp, fnxmax, pmas, rmas, sg, sigma0, tmas, 
     &                 vsx, x
      INTEGER i1, l, lmaxm1, modtpi, n
C
C
C
      dx = XMEs
C
      DO n = 1, NCHanl
         pmas = PMAsr(n)
         tmas = TMAsr(n)
         RMAsr(n) = pmas*tmas/(pmas + tmas)
         ARAtio(n) = TMAsr(1)/tmas
         CFUnir(n) = 1./(RMAsr(n)*WNUnit*WNUnit)
      ENDDO
C
      rmas = RMAsr(1)
      pmas = PMAsr(1)
      ECM(1) = ELAb*rmas/pmas
C
      DO i1 = 1, NCHanl
         rmas = RMAsr(i1)
         charge = CHArgr(i1)
         vsx = VSXr(i1)
         ECM(i1) = ECM(1) - QVAlue(i1)
         e1 = ABS(ECM(i1))
         CE(i1) = ETUnit*charge*SQRT(rmas/e1)
         WN(i1) = WNUnit*SQRT(rmas*e1)
         WNIni(i1) = WNUnit*SQRT(rmas*(ECM(i1) + vsx))
         ETA = CE(i1)
C
         IF(ETA.LT.10.0D0)THEN
            eta2 = ETA*ETA
C<<<<<<     variant A
Cmh         eta2a=2.0*eta
            eta6 = eta2 + 16.0
            sigma0 = ( - (ETA/(12.*eta6))
     &               *(1. + (eta2-48.)/(30.*eta6**2) + 
     &               ((eta2-160.)*eta2+1280.)/(105.*eta6**4)))
     &               - ETA + (ETA/2.)*LOG(eta6) + 3.5*ATAN(0.25*ETA)
     &               - (ATAN(ETA) + ATAN(0.5*ETA) + ATAN(ETA/3.))
         ELSE
            einv1 = 1.0/ETA
            einv2 = einv1*einv1
            einv3 = einv1*einv2
            einv5 = einv3*einv2
            einv7 = einv5*einv2
            einv9 = einv7*einv2
            sigma0 = 0.7853981634 + ETA*LOG(ETA)
     &               - ETA - (0.08333333333*einv1 + 
     &               0.00277777777*einv3 + 0.00079365079*einv5 + 
     &               0.00059523810*einv7 + 0.00084175084*einv9)
         ENDIF
C
         modtpi = sigma0/6.2831853072
         fmodtp = modtpi
         SGMazz(i1) = sigma0 - 6.2831853072*fmodtp
      ENDDO
C
      NXMax = KEXcom(2)
      NXCple = KEXcom(2)
      fnxmax = NXMax
      XMAx = dx*fnxmax
      a1 = TMAsr(1)**0.333333333333
      XBAr = RZEr(1)*a1
      IF(Iout.GT.3)THEN
         WRITE(6, 99001)(ECM(i1), i1 = 1, NCHanl)
99001    FORMAT(/' ECM   ', 6E15.5)
         WRITE(6, 99002)(WN(i1), i1 = 1, NCHanl)
99002    FORMAT(' WN    ', 6E15.5)
         WRITE(6, 99003)(WNIni(i1), i1 = 1, NCHanl)
99003    FORMAT(' WNINI ', 6E15.5)
         WRITE(6, 99004)(CE(i1), i1 = 1, NCHanl)
99004    FORMAT(' ETA   ', 6E15.5)
         WRITE(6, 99005)(SGMazz(i1), i1 = 1, NCHanl)
99005    FORMAT(' SIGM0 ', 6E15.5)
         WRITE(6, 99006)XMAx, XBAr, NXCple, NXMax, 
     &                  (LDWmxr(n), n = 1, NCHanl)
99006    FORMAT(' XMAX,XBAR=', 2E13.5/' NXCPLE,NXMAX,LDWMXR(N)=', 8I5)
      ENDIF
C
      DO i1 = 1, NCHanl
         LMAx = LDWmxr(i1) + 1
         lmaxm1 = LMAx - 1
         aovb = ARAtio(i1)
         dx = XMEs*aovb
         x = XMAx*aovb
C
CBF      MATCHING POINT IN MSTEP IS AT NXMAX-2
         x = x - 2.*dx
C
         ETA = CE(i1)
         SGMaz = SGMazz(i1)
         RHOmx = x*WN(i1)
         RD = dx*WN(i1)
C
         CALL FLGLCH
C
         DO l = 1, LMAx
            FC(l, i1) = F(l)
            GC(l, i1) = G(l)
            FDC(l, i1) = FD(l)
            GDC(l, i1) = GD(l)
         ENDDO
      ENDDO
C
      DO i1 = 1, NCHanl
         ETA = CE(i1)
         etasq = ETA*ETA
         sg = SGMazz(i1)
         EXSgri(1, i1) = COS(sg) + TTI*SIN(sg)
         fl = 1.0
         lmaxm1 = LDWmxr(i1)
         DO l = 1, lmaxm1
            denom = SQRT(1.0/(etasq + fl*fl))
            EXSgri(l + 1, i1) = (fl + TTI*ETA)*EXSgri(l, i1)*denom
            fl = fl + 1.0
         ENDDO
      ENDDO
C
      IF(KTLout(2).NE.0)THEN
C
         WRITE(6, 99007)
C
99007    FORMAT(/)
         DO n = 1, NCHanl
            WRITE(6, 99008)n
99008       FORMAT(' COULOMB FUNCTIONS FOR NO.', I1, ' CHANNEL')
            LMAx = LDWmxr(n) + 1
            WRITE(6, 99009)(FC(l, n), l = 1, LMAx, 2)
99009       FORMAT(3X, 'F ', 10E12.4)
            WRITE(6, 99010)(GC(l, n), l = 1, LMAx, 2)
99010       FORMAT(3X, 'G ', 10E12.4)
            WRITE(6, 99011)(FDC(l, n), l = 1, LMAx, 2)
99011       FORMAT(3X, 'FD', 10E12.4)
            WRITE(6, 99012)(GDC(l, n), l = 1, LMAx, 2)
99012       FORMAT(3X, 'GD', 10E12.4)
            WRITE(6, 99013)(EXSgri(l, n), l = 1, LMAx, 4)
99013       FORMAT(3X, 'EX ', 5('(', E10.3, E12.4, '),'))
         ENDDO
C
      ENDIF
      CALL OMPOTEN
C
      REWIND 7
C
      END
C
C
C***********************************************************************
      SUBROUTINE FLGLCH
C***********************************************************************
      IMPLICIT DOUBLE PRECISION(A - h), DOUBLE PRECISION(O - Z)
C
C
C PARAMETER definitions
C
      INTEGER NGLXX
      PARAMETER(NGLXX = 100)
C
C COMMON variables
C
      DOUBLE PRECISION ANGler(NGLXX), ELAb, ETA, ETUnit, F(70), FP(70), 
     &                 G(70), GP(70), RD, RHOmx, SGMaz, THEta(NGLXX), 
     &                 W(70), WNUnit, XBAr, XMAx, XMEs
      DOUBLE COMPLEX EXSg(70), TTI, TTR, ZERo
      INTEGER LDWmxr(4), LMAx, LTRamx(4), MXRow, NANglr, NCHanl, NXCple, 
     &        NXMax
      COMMON F, FP, G, GP, W, EXSg
      COMMON /PARAMT/ NCHanl, MXRow, NXMax, NXCple, NANglr, LMAx, 
     &                LTRamx, WNUnit, ETUnit, XMEs, ANGler, THEta, TTR, 
     &                TTI, ZERo, ELAb, ETA, XBAr, XMAx, SGMaz, RHOmx, 
     &                RD, LDWmxr
C
C Local variables
C
      DOUBLE PRECISION accy, an, bn, denom, dvcf(7), eta2, etac, ff, fn, 
     &                 fo, fpn, fpo, gn, go, gpn, gpo, gs, gs1, gt, gt1, 
     &                 h, hinc, hsq, ps, ps1, pt, pt1, r2, r4, ro, ro2, 
     &                 roincx, s, s12, s3, s4, sf, sg, sigmao, sp, spf, 
     &                 spg, sum, sump, t3, t4, test, tetao, tra, trb, 
     &                 wr, z1, z2, z3, zl
      DOUBLE COMPLEX exsgo
      INTEGER i, inc, inci, irol, j, j2, l, l1, l2, lf, ll, n, n1, ninc, 
     &        nstep
      INTEGER MIN0
C
C
C
CB    INPUT PARAMETERS ARE LMAX,SGMAZ,RHOMX,RD
C
      DATA dvcf/ - .01666666666D0, .15D0, -.75D0, 0.0D0, .75D0, -.15D0, 
     &     .01666666666D0/
C
      h = RD
      sigmao = SGMaz
      etac = ETA*ETA
      eta2 = ETA + ETA
      hsq = h*h
      ninc = 1./h + 0.1
      hinc = h*ninc
      s12 = .08333333333
      accy = 1.E-10
      roincx = MAX(100.D0, 10.D0*ETA)
      ro = RHOmx
      inc = 0
      inci = 0
C<<<<<<<<<<<<< variant A
C     L = 0
      i = 0
C<<<<<<<<<<<<< variant A
C     NSTEP = 0
 100  ro2 = ro + ro
      n = 0
      ps = 1.
      sf = 1.
      sg = 0.
      gs = 0.
      spf = 0.
      pt = 0.
      spg = 1. - ETA/ro
      gt = 1. - ETA/ro
      trb = 0.
C
 200  n1 = n + 1
      denom = n1*ro2
      an = (n1 + n)*ETA/denom
      bn = (etac - n*n1)/denom
      ps1 = an*ps - bn*pt
      gs1 = an*gs - bn*gt - ps1/ro
      pt1 = an*pt + bn*ps
      gt1 = an*gt + bn*gs - pt1/ro
      sf = sf + ps1
      sg = sg + gs1
      spf = spf + pt1
      spg = spg + gt1
      n = n + 1
C
      IF(n.GE.17)THEN
C
         IF(n.LE.17)tra = ps*ps + pt*pt
         trb = ps1*ps1 + pt1*pt1
         test = tra - trb
C
         IF(test.LE.0.D0)THEN
C
            IF(i.NE.1)THEN
C
               wr = sf*spg - sg*spf - 1.
               IF(ABS(wr).GE.accy)THEN
                  IF(ro.GT.roincx)THEN
C
C<<<<<<<<<<<<<       variant A
C                    IERR = 1
                     WRITE(6, 99001)inc, ro, wr
99001                FORMAT('0ASYMPT. EXPANS. DOES NOT CONVERGE', I5, 
     &                      2E12.4/)
                  ELSE
                     inci = inci + 1
                     inc = inc + inci
                     ro = ro + inci*hinc
                     GOTO 100
                  ENDIF
C
               ENDIF
            ENDIF
            tetao = ro - ETA*LOG(ro2) + sigmao
            tra = SIN(tetao)
            trb = COS(tetao)
            go = sf*trb - spf*tra
            gpo = sg*trb - spg*tra
            fo = spf*trb + sf*tra
            fpo = spg*trb + sg*tra
C
            IF(inc.NE.0)THEN
C
               ff = hsq*(eta2/ro - 1.)
               an = 1. - s12*ff
C
               IF(i.EQ.1)THEN
C
                  t4 = go*an
                  ro = ro - h
                  nstep = inc*ninc + 3
                  r4 = 0.
C
                  DO i = 1, nstep
C
                     j = nstep - i + 1
                     ro = ro - h
                     ff = hsq*(eta2/ro - 1.)
                     an = 1./(1. - s12*ff)
                     r2 = t3 + t3 - t4 + s4
                     t4 = t3
                     t3 = r2
                     s3 = t3*an
                     s4 = s3*ff
C
                     IF(j.LE.7)THEN
C
                        IF(j.EQ.4)go = s3
                        r4 = r4 + dvcf(j)*s3
C
                     ENDIF
                  ENDDO
                  gpo = r4/h
               ELSE
C
                  s3 = go
                  s4 = go*ff
                  t3 = go*an
                  ro = ro + h
                  i = 1
C
                  GOTO 100
               ENDIF
C
            ENDIF
            F(1) = fo
            FP(1) = fpo
            G(1) = go
            GP(1) = gpo
            W(1) = fpo*go - fo*gpo - 1.
            EXSg(1) = DCMPLX(COS(sigmao), SIN(sigmao))
            exsgo = DCMPLX(COS(sigmao), SIN(sigmao))
C
            IF(LMAx.NE.1 .OR. inc.NE.0)THEN
C
               ll = LMAx - 1
               ro = RHOmx
               irol = 0
               IF(ETA + SQRT(etac + ll*(ll+1)).GT.ro .OR. inc.GT.0)
     &            irol = 1
               sum = 0.
               sump = 0.
               l = 0
               lf = 0
C<<<<<<<<<<<<< variant A
C              LMX=200
C
 210           IF(lf.LT.ll)THEN
C
                  l = l + 1
                  l1 = l + 1
                  zl = l
                  z3 = SQRT(etac + zl*zl)
                  z1 = z3/zl
                  z2 = ETA/zl + zl/ro
                  gn = (z2*go - gpo)/z1
                  gpn = z1*go - z2*gn
C
                  IF(l.GT.ll)THEN
C
                     s = 1./(z1*gn*go)
                     sp = (z1*z1 - z2*z2)/(z1*gpn*gpo)
                     sum = sum + s
                     sump = sump + sp
                     go = gn
                     gpo = gpn
C
                     IF(ABS(s/sum).GE.accy .OR. ABS(sp/sump).GE.accy)
     &                  THEN
C<<<<<<<<<<<<<          variant A
C                       IF (L .LT. LMX) GO TO 10
                        IF(l.LT.200)GOTO 210
C
                        WRITE(6, 99002)l, go, gpo, sum, sump
99002                   FORMAT('0NO CONV IN F-RECURRENCE ', I5, 4E15.6)
C
                     ENDIF
                     go = fo
                     gpo = fpo
                     l = ll
                     l1 = l + 1
                     F(l1) = G(l1)*sum
                     fo = G(l1)*sum
                     FP(l1) = GP(l1)*sump
                     fpo = GP(l1)*sump
                     W(l1) = fpo*G(l1) - fo*GP(l1) - 1.
C
                     l2 = l
                     j2 = MIN0(lf + 1, l2)
                     DO l = l2, j2, -1
                        l1 = l
                        zl = l1
                        z1 = SQRT(etac + zl*zl)/zl
                        z2 = ETA/zl + zl/ro
                        fn = (z2*fo + fpo)/z1
                        fpn = z2*fn - z1*fo
                        F(l1) = fn
                        fo = fn
                        FP(l1) = fpn
                        fpo = fpn
                        W(l1) = fpo*G(l1) - fo*GP(l1) - 1.
C
                     ENDDO
                  ELSE
C
                     G(l1) = gn
                     go = gn
                     GP(l1) = gpn
                     gpo = gpn
C
                     IF(irol.LE.0)THEN
C
                        fn = (z2*fo - fpo)/z1
                        fpn = z1*fo - z2*fn
                        F(l1) = fn
                        fo = fn
                        FP(l1) = fpn
                        fpo = fpn
                        lf = l
                        W(l1) = fpo*go - fo*gpo - 1.
C
C                       W A R N I N G !!!! IT WAS MULTIPLE INSTRUCTION LIKE
C                       THAT: 18 EXSG(L1)=EXSGO=EXSGO*DCMPLX(ZL/Z3,ETA/Z3)
C
                     ENDIF
                     EXSg(l1) = exsgo*DCMPLX(zl/z3, ETA/z3)
                     exsgo = exsgo*DCMPLX(zl/z3, ETA/z3)
C
                     GOTO 210
                  ENDIF
C
               ENDIF
            ENDIF
            GOTO 99999
         ENDIF
      ENDIF
      ps = ps1
      gs = gs1
      pt = pt1
      gt = gt1
      tra = trb
      GOTO 200
99999 END
C
C
C***********************************************************************
      SUBROUTINE OMPOTEN
C***********************************************************************
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C
C PARAMETER definitions
C
      INTEGER MAXX, NGLXX
      PARAMETER(MAXX = 4000, NGLXX = 100)
C
C COMMON variables
C
      DOUBLE PRECISION ANGler(NGLXX), ARAtio(4), CE(4), CFUnir(4), 
     &                 CHArgr(4), DFNr(4), DFNsir(4), DFNspr(4), 
     &                 DFNsr(4), DFNwr(4), ECM(4), ELAb, ETA, ETUnit, 
     &                 EXTcom(50), PMAsr(4), QVAlue(4), RD, RHOmx, 
     &                 RMAsr(4), RZEcr(4), RZEr(4), RZEsir(4), RZEspr(4)
     &                 , RZEsr(4), RZEwr(4), SGMaz, SGMazz(4), 
     &                 THEta(NGLXX), TMAsr(4), VCEnti(MAXX, 3), 
     &                 VCEntr(MAXX, 3), VCOulm(MAXX, 3), VSOr(4), 
     &                 VSPin(MAXX, 3), VSPini(MAXX, 3), VSXr(4), WN(4), 
     &                 WNIni(4), WNUnit, WSFr(4), WSOr(4), WSXr(4), 
     &                 XBAr, XMAx, XMEs
      INTEGER KEXcom(50), KTLout(50), KTRl(30), LDWmxr(4), LLRow(120), 
     &        LMAx, LTRamx(4), MXRow, NANglr, NCHanl, NNDim(4), NXCple, 
     &        NXMax
      DOUBLE COMPLEX TTI, TTR, ZERo
      COMMON /CHANEL/ TMAsr, PMAsr, RMAsr, CHArgr, ARAtio, CFUnir, VSXr, 
     &                WSXr, WSFr, VSOr, DFNr, DFNwr, DFNsr, DFNspr, 
     &                RZEr, RZEwr, RZEsr, RZEspr, RZEcr, LLRow, NNDim, 
     &                QVAlue, ECM, CE, WN, WNIni, SGMazz
      COMMON /CNTROL/ KTRl, KEXcom, EXTcom, KTLout
      COMMON /OMPOT / VCEntr, VCEnti, VSPin, VCOulm, VSPini
      COMMON /PARAMT/ NCHanl, MXRow, NXMax, NXCple, NANglr, LMAx, 
     &                LTRamx, WNUnit, ETUnit, XMEs, ANGler, THEta, TTR, 
     &                TTI, ZERo, ELAb, ETA, XBAr, XMAx, SGMaz, RHOmx, 
     &                RD, LDWmxr
      COMMON /SOIMAG/ WSOr, DFNsir, RZEsir
C
C Local variables
C
      DOUBLE PRECISION charge, dfn, dfns, dfnsp, dfnspi, dfnw, dx, 
     &                 pform(3, 5), pmas, rzero, rzeroc, rzeros, rzerow, 
     &                 rzrosi, rzrosp, tmas, vclfc1, vclfc2, vso, vspfc, 
     &                 vsx, wsf, wso, wspfc, wsx, x, xbarc, xbars, 
     &                 xbarsi, xbarsp, xbarw, xbfac, xmem(4000)
      INTEGER k, n, nx
C
C
C
C<<<<<<<<<<<<< variant A
C     DOUBLE PRECISION XMEM(MAXX), PFORM(3,5)
C
      DO n = 1, NCHanl
C
         vsx = VSXr(n)
         wsx = WSXr(n)
         wsf = WSFr(n)
         vso = VSOr(n)
         wso = WSOr(n)
C
         dfn = DFNr(n)
         dfnw = DFNwr(n)
         dfns = DFNsr(n)
         dfnsp = DFNspr(n)
         dfnspi = DFNsir(n)
C
         rzero = RZEr(n)
         rzerow = RZEwr(n)
         rzeros = RZEsr(n)
         rzrosp = RZEspr(n)
         rzrosi = RZEsir(n)
         rzeroc = RZEcr(n)
C
         tmas = TMAsr(n)
         pmas = PMAsr(n)
         charge = CHArgr(n)
         vspfc = 2.0*vso/dfnsp
         wspfc = 2.0*wso/dfnspi
C
         xbfac = tmas**0.33333333
         xbarw = rzerow*xbfac
         xbars = rzeros*xbfac
         xbarc = rzeroc*xbfac
         xbarsp = rzrosp*xbfac
         xbarsi = rzrosi*xbfac
         XBAr = rzero*xbfac
C
         vclfc2 = 1.4398650*charge
         vclfc1 = vclfc2*0.5/xbarc
C
         NXMax = KEXcom(2)
         x = 0.0
         dx = XMEs
C
         DO nx = 1, NXMax
C
            x = x + dx
            xmem(nx) = x
C
            pform(1, 1) = EXP((x - XBAr)/dfn)
            pform(1, 2) = EXP((x - xbarw)/dfnw)
            pform(1, 3) = EXP((x - xbars)/dfns)
            pform(1, 4) = EXP((x - xbarsp)/dfnsp)
            pform(1, 5) = EXP((x - xbarsi)/dfnspi)
C
            DO k = 1, 5
               pform(2, k) = 1.0/(1.0 + pform(1, k))
               pform(3, k) = pform(1, k)*pform(2, k)*pform(2, k)
            ENDDO
C
            VCEntr(nx, n) = -vsx*pform(2, 1)
            VCEnti(nx, n) = ( - wsx*pform(2, 2)) - 4.0*wsf*pform(3, 3)
            VSPin(nx, n) = (vspfc*pform(3, 4))/x
            VSPini(nx, n) = (wspfc*pform(3, 5))/x
            IF(x.LE.xbarc)THEN
               VCOulm(nx, n) = vclfc1*(3.0 - (x/xbarc)**2)
            ELSE
               VCOulm(nx, n) = vclfc2/x
            ENDIF
         ENDDO
C
C
         IF(KTLout(2).EQ.2)THEN
C
            WRITE(6, 99001)n
C
99001       FORMAT(/' OPTICAL POTENTIAL FOR CHANNEL NO. ', I1/6X, 'R', 
     &             8X, 'VCENTR', 6X, 'VCENTI', 6X, 'VCOULM', 6X, 
     &             'VSPIN', 7X, 'VSPINI')
            DO nx = 10, NXMax, 10
               WRITE(6, 99002)xmem(nx), VCEntr(nx, n), VCEnti(nx, n), 
     &                        VCOulm(nx, n), VSPin(nx, n), VSPini(nx, n)
99002          FORMAT(1X, 6E12.4)
            ENDDO
C
         ENDIF
      ENDDO
C
      END
C
C
C***********************************************************************
      SUBROUTINE FFCAL
C***********************************************************************
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C
C PARAMETER definitions
C
      INTEGER MAXX, NGLXX
      PARAMETER(MAXX = 4000, NGLXX = 100)
C
C COMMON variables
C
      DOUBLE PRECISION ANGler(NGLXX), ARAtio(4), CE(4), CFUnir(4), 
     &                 CHArgr(4), DFNf(4), DFNr(4), DFNsf(4), DFNspf(4), 
     &                 DFNspr(4), DFNsr(4), DFNwf(4), DFNwr(4), DZEro(4)
     &                 , ECM(4), EGS(4), ELAb, ETA, ETUnit, EXTcom(50), 
     &                 PMAsr(4), QVAlue(4), RD, RHOmx, RMAsr(4), 
     &                 RZEcf(4), RZEcr(4), RZEf(4), RZEr(4), RZEsf(4), 
     &                 RZEspf(4), RZEspr(4), RZEsr(4), RZEwf(4), 
     &                 RZEwr(4), SGMaz, SGMazz(4), THEta(NGLXX), 
     &                 TMAsr(4), VINti(MAXX, 2), VINtr(MAXX, 2), VSOf(4)
     &                 , VSOr(4), VSXf(4), VSXr(4), WN(4), WNIni(4), 
     &                 WNUnit, WSFf(4), WSFr(4), WSXf(4), WSXr(4), XBAr, 
     &                 XMAx, XMEs, ZPR(4), ZTR(4)
      INTEGER JLSmax, KCFf(4), KEXcom(50), KTLout(50), KTRl(30), 
     &        LBTrf(4), LDWmxr(4), LLRow(120), LMAx, LTRamx(4), MXRow, 
     &        NANglr, NCHanl, NNDim(4), NODf(4), NXCple, NXMax
      DOUBLE COMPLEX TTI, TTR, ZERo
      COMMON /CHANEL/ TMAsr, PMAsr, RMAsr, CHArgr, ARAtio, CFUnir, VSXr, 
     &                WSXr, WSFr, VSOr, DFNr, DFNwr, DFNsr, DFNspr, 
     &                RZEr, RZEwr, RZEsr, RZEspr, RZEcr, LLRow, NNDim, 
     &                QVAlue, ECM, CE, WN, WNIni, SGMazz
      COMMON /CNTROL/ KTRl, KEXcom, EXTcom, KTLout
      COMMON /FOFINT/ EGS, DZEro, VSXf, WSXf, WSFf, VSOf, DFNf, DFNwf, 
     &                DFNsf, DFNspf, RZEf, RZEwf, RZEsf, RZEspf, RZEcf, 
     &                ZTR, ZPR
      COMMON /FOFINTI/ JLSmax, NODf, LBTrf, KCFf
      COMMON /MSFF  / VINtr, VINti
      COMMON /PARAMT/ NCHanl, MXRow, NXMax, NXCple, NANglr, LMAx, 
     &                LTRamx, WNUnit, ETUnit, XMEs, ANGler, THEta, TTR, 
     &                TTI, ZERo, ELAb, ETA, XBAr, XMAx, SGMaz, RHOmx, 
     &                RD, LDWmxr
C
C Local variables
C
      DOUBLE PRECISION dfn, dfns, dfnw, dx, dzr, exr, exs, exw, fehler, 
     &                 pmas, t1, t2, t3, t4, tmas, vfac1, vfac2, vsx, 
     &                 wdfc1, wfac1, wsf, wsx, x, xbars, xbarw, xbfac, 
     &                 xmem(MAXX), xmest, z1, z2, zzp, zzt
      INTEGER i, iffpr, kder, n, n1, n2, nt, nx
C
C
C
C<<<<<<<<<<<<< variant A
C>>>>>>>>>>>>> variant B
C     DOUBLE PRECISION XMEM(4000)
C
C     INTEGER NAT1(3)
C     INTEGER NAT(100)
C     INTEGER NAMOB(10)
C     DOUBLE PRECISION POL(10)
C     DOUBLE PRECISION TEXFF(5)
C     INTEGER IFMT(2)
C     DATA NAT0, NAT1, NAT/ 4H  N , 4H  P , 4H  D , 4H  T , 2H H, 2HHE,
C     1   2HLI, 2HBE, 2H B, 2H C, 2H N, 2H O, 2H F, 2HNE, 2HNA, 2HMG,
C     2   2HAL, 2HSI, 2H P, 2H S, 2HCL, 2HAR, 2H K, 2HCA, 2HSC, 2HTI,
C     3   2H V, 2HCR, 2HMN, 2HFE, 2HCO, 2HNI, 2HCU, 2HZN, 2HGA, 2HGE,
C     4   2HAS, 2HSE, 2HBR, 2HKR, 2HRB, 2HSR, 2H Y, 2HZR, 2HNB, 2HMO,
C     5   2HTC, 2HRU, 2HRH, 2HPD, 2HAG, 2HCD, 2HIN, 2HSN, 2HSB, 2HTE,
C     6   2H I, 2HXE, 2HCS, 2HBA, 2HLA, 2HCE, 2HPR, 2HND, 2HPR, 2HSM,
C     7   2HEU, 2HGD, 2HTB, 2HDY, 2HHO, 2HER, 2HTM, 2HYB, 2HLU, 2HHF,
C     8   2HTA, 2H W, 2HRE, 2HOS, 2HIR, 2HPT, 2HAU, 2HHG, 2HTL, 2HPB,
C     9   2HBI, 2HPO, 2HAT, 2HRN, 2HFR, 2HRA, 2HAC, 2HTH, 2HPA, 2H U,
C     .   2HNP, 2HPU, 2HAM, 2HCM, 2HBK, 2HCF, 2HES, 2HFM/
C     DATA NAMOB/ 1HS, 1HP, 1HD, 1HF, 1HG, 1HH, 1HI, 1HJ, 1HK, 1HL/
C
      NXMax = KEXcom(2)
C
      DO nx = 1, NXMax
         DO n1 = 1, JLSmax
            VINtr(nx, n1) = 0.0
            VINti(nx, n1) = 0.0
         ENDDO
      ENDDO
C
C
      DO n1 = 1, JLSmax
C
         n2 = n1 + 1
         t1 = TMAsr(n1)
         t2 = TMAsr(n2)
         z1 = ZTR(n1)
         z2 = ZTR(n2)
C
         IF(t1.GT.t2)THEN
            t4 = t2
            nt = n1
         ELSE
            t4 = t1
            nt = n2
         ENDIF
C
         t3 = TMAsr(1)/t4
         xmest = XMEs*t3
CB
C
         fehler = ABS(xmest - XMEs)/XMEs
         WRITE(6, *)
         IF(fehler.GT.0.01)THEN
            WRITE(6, *)'WARNING **** WARNING **** WARNING'
            WRITE(6, *)'==> IN FFCAL ABS(XMEST-XMES)/XMES=', fehler
         ENDIF
C
         pmas = ABS(t1 - t2)
         tmas = TMAsr(nt)
         zzp = ABS(z1 - z2)
         zzt = ZTR(nt)
C
CB       IF(LBTRF(N1).GE.0) GO TO 301
         IF(LBTrf(n1).GE.0)THEN
            WRITE(6, *)'THIS VERSION OF ORION IS FOR INELASTIC ', 
     &                 'SCATTERING ONLY'
            STOP
         ENDIF
C
         xbfac = tmas**0.33333333
         vsx = VSXf(n1)
         wsx = WSXf(n1)
         wsf = WSFf(n1)
         dfn = DFNf(n1)
         dfnw = DFNwf(n1)
         dfns = DFNsf(n1)
         XBAr = RZEf(n1)*xbfac
         xbarw = RZEwf(n1)*xbfac
         xbars = RZEsf(n1)*xbfac
         vfac1 = -vsx*XBAr/dfn
         vfac2 = vfac1*XBAr*0.5/dfn
         wfac1 = -wsx*xbarw/dfnw
         wdfc1 = -4.0*wsf*xbars/dfns
C
         x = 0.0
         dx = xmest
C
         DO nx = 1, NXMax
            x = x + dx
            xmem(nx) = x
            exr = EXP((x - XBAr)/dfn)
            exw = EXP((x - xbarw)/dfnw)
            exs = EXP((x - xbars)/dfns)
            kder = -LBTrf(n1)
C
C           Modification for Compressional Form Factor (L=O Transitions)
C           H. Lenske, Oct. 29, 1997
C
C
C           Definition of FormFactors :
C           FF for KDER=1,2,3,4,5,6:   (selected by LBTRF in input!)
C           1st Derivative WS/Volume WS/2nd Derivative WS/modifed 2nd
C           deriv./Volume and 1st deriv superimposed
C
            IF(kder.EQ.1)VINtr(nx, n1) = vfac1*exr/(1.0 + exr)**2
            IF(kder.EQ.2)VINtr(nx, n1) = vfac1/(1.0 + exr)
            IF(kder.EQ.3)VINtr(nx, n1) = vfac2*(exr - 1.0)
     &         *exr/(1.0 + exr)**3
            IF(kder.EQ.4)VINtr(nx, n1)
     &         = vfac1*exr*exw/((1.0 + exr)**2*(1. + exw))
            IF(kder.EQ.5)VINtr(nx, n1) = wfac1/(1. + exw)
     &         + wdfc1*exs/(1. + exs)**2
C
C           Monopole case (H. Lenske, Oct. 29, 1997):
C
            IF(kder.EQ.6)THEN
               VINtr(nx, n1) = -(3.0*vsx - x*(vsx/dfn)*exr/(1. + exr))
     &                         /(1. + exr)
               VINti(nx, n1) = ( - (3.0*wsx - x*(wsx/dfnw)*exw/(1.+exw))
     &                         /(1. + exw))
     &                         - (3.0*wsf - x*(wsf/dfns)*(exs - 1.)
     &                         /(1. + exs))
     &                         *4.*exs*xbars/(dfns*(1. + exs)**2)
               GOTO 50
            ENDIF
            IF(KCFf(n1).NE.0)VINti(nx, n1) = wfac1*exw/(1.0 + exw)
     &         **2 + wdfc1*(exs - 1.0)*exs/(1.0 + exs)**3
C
 50      ENDDO
CB
CB       FOR TRANSFER REACTIONS THE MISSING PART MUST BE INSERTED HERE
CB
         dzr = DZEro(n1)
         DO nx = 1, NXMax
            VINtr(nx, n1) = VINtr(nx, n1)*dzr
            VINti(nx, n1) = VINti(nx, n1)*dzr
         ENDDO
C
Cmh      IF(LBTRF(N1).GE.0) WRITE(6,416)EGEST,VSX
Cmh      416 FORMAT(3X,'B.E.=',F7.3,' MEV, VSX=',F7.3,' MEV, AFTER SEARCH'/)
C
      ENDDO
C
      iffpr = 1
      IF(iffpr.NE.1)THEN
C
C        open(30,file='ff.d',status='unknown')
C
C        DO NN = 1, NXMAX
C        WRITE (30, 3000) XMEM(NN), (VINTR(NN,I),VINTI(NN,I),I=1,
C        1         JLSMAX)
C        END DO
C3000    FORMAT(F10.5,8E15.7)
C        CLOSE(30)
         IF(KTRl(6).EQ.0)THEN
            DO i = 16, NXMax, 5
               PRINT 99001, xmem(i), 
     &               (VINtr(i, n), VINti(i, n), n = 1, JLSmax)
99001          FORMAT(' ', F6.2, 8E13.5)
            ENDDO
         ENDIF
      ENDIF
C
      END
C
C
C***********************************************************************
      SUBROUTINE HIBORN
C***********************************************************************
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C
C PARAMETER definitions
C
      INTEGER MAXX, NGLXX
      PARAMETER(MAXX = 4000, NGLXX = 100)
C
C COMMON variables
C
      DOUBLE PRECISION ANGler(NGLXX), ARAtio(4), CE(4), CFUnir(4), 
     &                 CHArgr(4), DFNr(4), DFNspr(4), DFNsr(4), DFNwr(4)
     &                 , ECM(4), ELAb, ETA, ETUnit, EXTcom(50), H, HSQ, 
     &                 HTWelv, PMAsr(4), QVAlue(4), RD, RHOmx, RMAsr(4), 
     &                 RZEcr(4), RZEr(4), RZEspr(4), RZEsr(4), RZEwr(4), 
     &                 SGMaz, SGMazz(4), THEta(NGLXX), TMAsr(4), 
     &                 VINti(MAXX, 2), VINtr(MAXX, 2), VSOr(4), VSXr(4), 
     &                 WN(4), WNIni(4), WNUnit, WSFr(4), WSXr(4), XBAr, 
     &                 XMAx, XMEs, XSQiv(MAXX)
      DOUBLE COMPLEX CFOrm(MAXX, 2), CMAt1(42, 61), CMAt2(42, 42, 61), 
     &               TTI, TTR, ZERo
      INTEGER ISTw(3), JJ, KEXcom(50), KTLout(50), KTRl(30), LDWmxr(4), 
     &        LLRow(120), LMAx, LTRamx(4), MXRow, NANglr, NCHanl, 
     &        NNDim(4), NRX, NXCple, NXMax
      COMMON CFOrm, XSQiv
      COMMON /CHANEL/ TMAsr, PMAsr, RMAsr, CHArgr, ARAtio, CFUnir, VSXr, 
     &                WSXr, WSFr, VSOr, DFNr, DFNwr, DFNsr, DFNspr, 
     &                RZEr, RZEwr, RZEsr, RZEspr, RZEcr, LLRow, NNDim, 
     &                QVAlue, ECM, CE, WN, WNIni, SGMazz
      COMMON /CMATR / CMAt1, CMAt2
      COMMON /CNTROL/ KTRl, KEXcom, EXTcom, KTLout
      COMMON /MSFF  / VINtr, VINti
      COMMON /NCONST/ H, HTWelv, HSQ
      COMMON /NCONSTI/ NRX
      COMMON /PARAMT/ NCHanl, MXRow, NXMax, NXCple, NANglr, LMAx, 
     &                LTRamx, WNUnit, ETUnit, XMEs, ANGler, THEta, TTR, 
     &                TTI, ZERo, ELAb, ETA, XBAr, XMAx, SGMaz, RHOmx, 
     &                RD, LDWmxr
      COMMON /SPIN  / ISTw, JJ
C
C Local variables
C
      DOUBLE PRECISION bova2, drsq, drsqd(3), dx, fpi, pi, wn1, x
      DOUBLE COMPLEX cmat0
      INTEGER ii1, ii2, ii3, istp(3), jatw, jbtw, jbtwmn, jbtwmx, jctw, 
     &        jctwmn, jctwmx, kheq, kieq, l1trx, l1twx, l2trx, l2twx, 
     &        la, latw, lb, lbmax, lbmin, lbtw, lc, lcmax, lcmin, lctw, 
     &        ldwmxa, ldwmxb, ldwmxc, m1, m2, m3, mb, mc, n, n1, n2, n3, 
     &        na, namx, nb, nbmin, nbmx, nc, ncmin, ncmx, nct, nx
C
C
CBF   DOUBLE COMPLEX CMAT1,CMAT2,SMAT(0:100,3,3)
C
C
CB    DIMENSION OF C-MATRIX ELEMENTS CMAT1,CMAT2
CB    LAST INDEX COUNTS POSSIBLE (L,J)-COMBINATIONS IN ENTRANCE CHANNEL
CB    NA= 2la + ja-la+1/2  <=  2*LDWMXA+1
CB    FIRST INDICES COUNT ANGULAR MOMENTUM TRANSFER
CB    FIRST INDEX IS FOR EXIT CHANNEL
CB    SECOND INDEX IS FOR INTERMEDIATE CHANNEL IN CMAT2
CB    FOR TRANSITION FROM X TO Y:
CB    NY= 2(ly-(lx-lmax)) + (jy-ly+1/2) + 1 <= 4*LTRMX+2
C
      ZERo = (0.0D0, 0.0D0)
      TTR = (1.0D0, 0.0D0)
      TTI = (0.0D0, 1.0D0)
      REWIND 7
C
      NXMax = KEXcom(2)
      dx = XMEs
C
      NRX = NXMax
      H = dx
      HSQ = H*H
      HTWelv = HSQ/12.
C
      IF(NCHanl.EQ.2)THEN
         LTRamx(2) = 0
         LDWmxr(3) = LDWmxr(2)
      ENDIF
C
      ldwmxa = LDWmxr(1)
      ldwmxc = LDWmxr(2)
      ldwmxb = LDWmxr(3)
      l1trx = LTRamx(1)
      l2trx = LTRamx(2)
      l1twx = 2*l1trx
      l2twx = 2*l2trx
C
      namx = 2*ldwmxa + 1
      ncmx = 4*l1trx + 2
      nbmx = 4*l2trx + 2
      DO na = 1, namx
         DO nc = 1, ncmx
            CMAt1(nc, na) = (0.0, 0.0)
            DO nb = 1, nbmx
               CMAt2(nb, nc, na) = (0.0, 0.0)
            ENDDO
         ENDDO
      ENDDO
C
C
C     PREPARE FORM FACTORS
C
      wn1 = WN(1)
      DO nct = 1, NCHanl
         bova2 = 1.0/ARAtio(nct)**2
         drsqd(nct) = bova2*RMAsr(1)/RMAsr(nct)
      ENDDO
      IF(NCHanl.EQ.2)drsqd(3) = drsqd(2)
      drsq = (dx*wn1)**2/ECM(1)
C
CB    FORMFACTOR MULTIPLIED BY 2*RMAS/HBAR**2 (=1/CFUNIR)
      x = 0.0
      DO nx = 1, NXMax
         x = x + dx
         XSQiv(nx) = 1./(x*x)
         CFOrm(nx, 1) = DCMPLX(VINtr(nx, 1), VINti(nx, 1))/CFUnir(2)
         IF(NCHanl.EQ.3)CFOrm(nx, 2) = DCMPLX(VINtr(nx, 2), VINti(nx, 2)
     &                                 )/CFUnir(3)
      ENDDO
C
      pi = 4.*ATAN(1.D0)
      fpi = 4.*pi
C
      DO n = 1, NCHanl
         istp(n) = 1
         IF(ISTw(n).EQ.1 .OR. ISTw(n).EQ.2)istp(n) = 2
      ENDDO
C
      kheq = 0
      kieq = 1
      na = 0
C
C     LOOP OVER INCIDENT PARTIAL WAVES
C
      DO la = 0, ldwmxa
         ii1 = 1
         latw = la + la
         n1 = 0
C
         DO m1 = -ISTw(1), ISTw(1), istp(1)
            n1 = n1 + 1
            jatw = latw + m1
            IF(jatw.GE.0)THEN
               na = na + 1
C
               CALL MSTEP(latw, jatw, cmat0, kheq, ii1)
C
CBF            SMAT(LA,N1,II1)=1.D0+2.D0*TTI*CMAT0
CBF            RMAT           =(JATW+1.)*(1.-ABS(SMAT(LA,N1,II1))**2)
CBF            REACXS(II1)    =REACXS(II1)+RMAT
C
C              ONE-STEP
C
               lcmin = MAX(la - l1trx, 0)
               lcmax = MIN(la + l1trx, ldwmxc)
               jctwmn = MAX(1, jatw - l1twx)
               jctwmx = jatw + l1twx
C
               ncmin = l1twx - latw + lcmin + (jctwmn + 1)/2 + 1
               nc = ncmin - 1
C
CBF            PC=PHASEF(LCMIN+LCMAX-L1TRX)
CBF            IF(PC.LT.0.D0)LCMAX=LCMAX-1
C
               DO lc = lcmin, lcmax
                  n2 = 0
                  ii2 = 2
                  lctw = lc + lc
C
                  DO m2 = -ISTw(2), ISTw(2), istp(2)
                     n2 = n2 + 1
                     jctw = lctw + m2
                     IF(jctw.GE.jctwmn .AND. jctw.LE.jctwmx)THEN
                        nc = nc + 1
C
                        CALL MSTEP(lctw, jctw, cmat0, kheq, ii2)
CBF                     SMAT(LC,N2,II2)=1.+2.*TTI*CMAT0
                        CALL MSTEP(lctw, jctw, CMAt1(nc, na), kieq, ii2)
C
C                       PREPARE FOR TWO-STEP
C
                        IF(NCHanl.NE.2)THEN
C
                           lbmin = MAX(lc - l2trx, 0)
                           lbmax = MIN(lc + l2trx, ldwmxb)
                           jbtwmn = MAX(1, jctw - l2twx)
                           jbtwmx = jctw + l2twx
C
                           nbmin = l2twx - lctw + lbmin + (jbtwmn + 1)
     &                             /2 + 1
                           nb = nbmin - 1
C
CBF                        PB=PHASEF(LBMIN+LBMAX-L2TRX)
CBF                        IF(PB.LT.0.D0)LBMAX=LBMAX-1
C
C
C                          TWO-STEP
C
                           DO lb = lbmin, lbmax
                              lbtw = lb + lb
                              n3 = 0
                              ii3 = 3
C
                              DO m3 = -ISTw(3), ISTw(3), istp(3)
                                 n3 = n3 + 1
                                 jbtw = lbtw + m3
                                 IF(jbtw.GE.jbtwmn .AND. jbtw.LE.jbtwmx)
     &                              THEN
                                    nb = nb + 1
C
                                    CALL MSTEP(lbtw, jbtw, cmat0, kheq, 
     &                                 ii3)
CBF                                 SMAT(LB,N3,II3)=1.+2.*TTI*CMAT0
                                    CALL MSTEP(lbtw, jbtw, 
     &                                 CMAt2(nb, nc, na), kieq, ii3)
C
                                 ENDIF
                              ENDDO
                           ENDDO
                           IF(KTLout(3).NE.0)WRITE(6, 99001)na, nc, nb, 
     &                        (CMAt2(mb, nc, na), mb = nbmin, nb)
C6020                      FORMAT(' REACXS:   ',E13.5,'  (mb)')
99001                      FORMAT(/'NA=', I3, ' NC=', I3, ' NB=', I3, 
     &                            ' YIELD CMAT2(1..NB,NC,NA)'/
     &                            (4(:'(',E11.4,',',E11.4,' )',3X)))
C
C                          END OF TWO-STEP
C
                        ENDIF
                     ENDIF
                  ENDDO
               ENDDO
               IF(KTLout(3).NE.0)THEN
                  WRITE(6, 99002)na, nc, (CMAt1(mc, na), mc = ncmin, nc)
99002             FORMAT(/'NA=', I3, ' NC=', I3, 
     &                   ' YIELD CMAT1(1..NC,NA)'/
     &                   (4(:'(',E11.4,',',E11.4,' )',3X)))
C
C                 END OF ONE-STEP
C
C
Cmh               IF(KTLOUT(3).NE.0) WRITE(6,6010)LA,JATW,SMAT(LA,N1,II1),RMAT
               ENDIF
            ENDIF
         ENDDO
      ENDDO
C
C
CBF   REACXS(1)=REACXS(1)*RCONST(1)
CBF   WRITE(6,6020)REACXS(1)
C
99003 FORMAT(/'LA=', I2, '  JA=', I3, '/2  SMAT=(', 2E13.5, ')  RMAT=', 
     &       E13.5)
C
      END
C
C
C***********************************************************************
      SUBROUTINE MSTEP(Lptw, Jptw, Cmat, Ktype, Nch)
C***********************************************************************
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C
C PARAMETER definitions
C
      INTEGER MAXX, NGLXX
      PARAMETER(MAXX = 4000, NGLXX = 100)
C
C COMMON variables
C
      DOUBLE PRECISION ANGler(NGLXX), ARAtio(4), CE(4), CFUnir(4), 
     &                 CHArgr(4), DFNr(4), DFNspr(4), DFNsr(4), DFNwr(4)
     &                 , ECM(4), ELAb, ETA, ETUnit, FC(50, 3), 
     &                 FDC(50, 3), GC(50, 3), GDC(50, 3), H, HSQ, 
     &                 HTWelv, PMAsr(4), QVAlue(4), RD, RHOmx, RMAsr(4), 
     &                 RZEcr(4), RZEr(4), RZEspr(4), RZEsr(4), RZEwr(4), 
     &                 SGMaz, SGMazz(4), THEta(NGLXX), TMAsr(4), 
     &                 VCEnti(MAXX, 3), VCEntr(MAXX, 3), VCOulm(MAXX, 3)
     &                 , VSOr(4), VSPin(MAXX, 3), VSPini(MAXX, 3), 
     &                 VSXr(4), WN(4), WNIni(4), WNUnit, WSFr(4), 
     &                 WSXr(4), XBAr, XMAx, XMEs, XSQiv(MAXX)
      DOUBLE COMPLEX CFH(0:MAXX, 3), CFI(0:MAXX, 3), CFOrm(MAXX, 2), 
     &               CSRc(0:MAXX, 2), CST(2, 3), EXSgri(50, 3), TTI, 
     &               TTR, ZERo
      INTEGER LDWmxr(4), LLRow(120), LMAx, LTRamx(4), MXRow, NANglr, 
     &        NCHanl, NNDim(4), NRX, NXCple, NXMax
      COMMON CFOrm, XSQiv
      COMMON /CHANEL/ TMAsr, PMAsr, RMAsr, CHArgr, ARAtio, CFUnir, VSXr, 
     &                WSXr, WSFr, VSOr, DFNr, DFNwr, DFNsr, DFNspr, 
     &                RZEr, RZEwr, RZEsr, RZEspr, RZEcr, LLRow, NNDim, 
     &                QVAlue, ECM, CE, WN, WNIni, SGMazz
      COMMON /COUWF / EXSgri, FC, FDC, GC, GDC
      COMMON /NCONST/ H, HTWelv, HSQ
      COMMON /NCONSTI/ NRX
      COMMON /OMPOT / VCEntr, VCEnti, VSPin, VCOulm, VSPini
      COMMON /PARAMT/ NCHanl, MXRow, NXMax, NXCple, NANglr, LMAx, 
     &                LTRamx, WNUnit, ETUnit, XMEs, ANGler, THEta, TTR, 
     &                TTI, ZERo, ELAb, ETA, XBAr, XMAx, SGMaz, RHOmx, 
     &                RD, LDWmxr
      COMMON /WFST  / CFH, CFI, CSRc, CST
C
C Dummy arguments
C
      DOUBLE COMPLEX Cmat
      INTEGER Jptw, Ktype, Lptw, Nch
C
C Local variables
C
      DOUBLE COMPLEX c, c1, c2, cdet, cfd, cff, chd, chp, cnrm, 
     &               cu(0:4000)
      DOUBLE PRECISION eps, fll, fso, ui, ur
      INTEGER istw, l, lc, n, nrm
C
C
C
C<<<<<<<<<<<<< variant A
C     DIMENSION CU(0:MAXX)
      DATA eps/1.D-02/
C
      istw = ABS(Lptw - Jptw)
      fll = Lptw*(Lptw + 2)
      fso = 0.25*(Jptw*(Jptw + 2) - fll - istw*(istw + 2))
      fll = 0.25*fll
      l = Lptw/2
      lc = l + 1
C
CB    MATCHINGPOINT IS AT NRM=NRX-2
      nrm = NRX - 2
C
      DO n = 1, NRX
         ur = VCEntr(n, Nch) + fso*VSPin(n, Nch) + VCOulm(n, Nch)
     &        - ECM(Nch)
         ur = ur/CFUnir(Nch) + fll*XSQiv(n)
         ui = (VCEnti(n, Nch) + fso*VSPini(n, Nch))/CFUnir(Nch)
         cu(n) = DCMPLX(ur, ui)
      ENDDO
      cu(0) = cu(1)
C
      chp = DCMPLX(GC(lc, Nch), FC(lc, Nch))
      chd = DCMPLX(GDC(lc, Nch), FDC(lc, Nch))
      chd = WN(Nch)*chd
C
C     HOMOGENUOUS EQUATION
C
      IF(Ktype.EQ.0)THEN
C
C        INITIATE
C
         c2 = DCMPLX(0., 0.)
         IF(l.EQ.1)c2 = eps*eps
         c1 = eps
         CFH(0, Nch) = c2/2.**((l + 3)/3)
         CFH(1, Nch) = c1*(1. + HTWelv*cu(1))/2.**((l + 3)/3)
C
C        INTEGRATE RADIAL WAVE EQUATION
C
         DO n = 2, NRX
            c = 2.*c1 - c2 + HSQ*cu(n - 1)*CFH(n - 1, Nch)
            CFH(n, Nch) = c*(1. + HTWelv*cu(n))
            c2 = c1
            c1 = c
         ENDDO
C
C        MATCHING TO ASYMPTOTIC FORM AT NRM=NRX-2
C
         cff = CFH(nrm, Nch)
         cfd = (CFH(nrm - 2, Nch) - 8.*CFH(nrm - 1, Nch)
     &         + 8.*CFH(nrm + 1, Nch) - CFH(nrm + 2, Nch))/(12.*H)
C
         cdet = chp*cfd - chd*cff
         IF(ABS(cdet).NE.0.D0)THEN
            cdet = DCONJG(cdet)/ABS(cdet)**2
         ELSE
            cdet = DCMPLX(0., 0.)
         ENDIF
         cnrm = cdet*(chp*WN(Nch)*FDC(lc, Nch) - chd*FC(lc, Nch))
         Cmat = cdet*(cff*WN(Nch)*FDC(lc, Nch) - cfd*FC(lc, Nch))
C
C
CB       COULOMB PHASE FOR CNRM IN ENTRANCE CHANNEL
         IF(Nch.EQ.1)cnrm = cnrm*EXSgri(lc, Nch)
C
         DO n = 1, NRX
            CFH(n, Nch) = cnrm*CFH(n, Nch)
         ENDDO
C
         CST(1, Nch) = CFH(nrm, Nch)
         CST(2, Nch) = cnrm*cfd
C
C        SOURCE TERM FOR 1-STEP CALCULATION
C
         IF(Nch.EQ.1)THEN
            DO n = 1, NRX
               CSRc(n, Nch) = CFOrm(n, Nch)*CFH(n, Nch)
            ENDDO
            CSRc(0, Nch) = (0.0, 0.0)
         ENDIF
C
      ENDIF
C
C     INHOMOGENUOUS EQUATION
C
      IF(Ktype.EQ.1)THEN
C
C        INITIATE
C
         c2 = DCMPLX(0., 0.)
         c1 = eps*CSRc(1, Nch - 1)
         CFI(0, Nch) = c2
         CFI(1, Nch) = c1*(1. + HTWelv*cu(1))
C
C        INTEGRATE INHOMOG. WAVE EQUATION
C
         DO n = 2, NRX
            c = 2.*c1 - c2 + HSQ*cu(n - 1)*CFI(n - 1, Nch)
     &          + HTWelv*(CSRc(n, Nch - 1) + 10.*CSRc(n - 1, Nch - 1)
     &          + CSRc(n - 2, Nch - 1))
            CFI(n, Nch) = c*(1. + HTWelv*cu(n))
            c2 = c1
            c1 = c
         ENDDO
C
C        MATCHING
C
         cff = CFI(nrm, Nch)
         cfd = (CFI(nrm - 2, Nch) - 8.*CFI(nrm - 1, Nch)
     &         + 8.*CFI(nrm + 1, Nch) - CFI(nrm + 2, Nch))/(12.*H)
C
         cdet = CST(1, Nch)*chd - CST(2, Nch)*chp
         cdet = DCONJG(cdet)/ABS(cdet)**2
         cnrm = cdet*(cff*chd - cfd*chp)
         Cmat = cdet*(CST(1, Nch)*cfd - CST(2, Nch)*cff)
C
CB       COULOMB PHASE FOR CMAT
         Cmat = Cmat*EXSgri(lc, Nch)
C
C
C        SOURCE TERM FOR 2-STEP
C
         IF(NCHanl.EQ.3 .AND. Nch.EQ.2)THEN
            DO n = 1, NRX
               CSRc(n, Nch) = CFOrm(n, Nch)
     &                        *(CFI(n, Nch) - cnrm*CFH(n, Nch))
            ENDDO
            CSRc(0, Nch) = (0.0, 0.0)
         ENDIF
C
      ENDIF
      END
C
C
C***********************************************************************
      SUBROUTINE XSEC
C***********************************************************************
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C
C PARAMETER definitions
C
      INTEGER NGLXX
      PARAMETER(NGLXX = 100)
C
C COMMON variables
C
      DOUBLE PRECISION ANGler(NGLXX), ARAtio(4), C1Mem(2), CE(4), 
     &                 CFUnir(4), CHArgr(4), CONst1, CONst2, DFNr(4), 
     &                 DFNspr(4), DFNsr(4), DFNwr(4), ECM(4), ELAb, ETA, 
     &                 ETUnit, EXTcom(50), FAClm(NGLXX), FAClog(500), 
     &                 P(975, NGLXX), PL(30), PLM10, PLM10m(NGLXX), 
     &                 PLM20, PLM20m(NGLXX), PMAsr(4), QVAlue(4), RAC, 
     &                 RACie(50), RADian, RD, RHOmx, RMAsr(4), RZEcr(4), 
     &                 RZEr(4), RZEspr(4), RZEsr(4), RZEwr(4), 
     &                 SGMa(25, NGLXX, 2), SGMat(NGLXX, 2), SGMaz, 
     &                 SGMazz(4), SQRt10, THEta(NGLXX), TMAsr(4), U9, 
     &                 VSOr(4), VSXr(4), WN(4), WNIni(4), WNUnit, 
     &                 WR1(1000, 2), WR2(5000, 2), WSFr(4), WSXr(4), 
     &                 XBAr, XMAx, XMEs
      DOUBLE COMPLEX CSUm2(NGLXX), TTI, TTR, XAMp(8300, 4), ZERo
      INTEGER ISTw(3), ISTw1, ISTw2, ISTw3, J12mxt, J1Tw, J2Tw, JAS, 
     &        JBS, JCS, JJ, JLMitw, JLMxtw, JLRang, KASe, KEXcom(50), 
     &        KTLout(50), KTRl(30), L12mxm, L1Maxm, L1Tr, L1Tw, L2Maxm, 
     &        L2Tr, L2Tw, LCAltr, LDWmxa, LDWmxb, LDWmxc, LDWmxr(4), 
     &        LLRow(120), LMAx, LTRamx(4), MMXtr, MXRow, NANglr, 
     &        NBSamp(40), NCHanl, NNDim(4), NXCple, NXMax
      COMMON /BIMH  / ISTw1, ISTw2, ISTw3, JAS, JBS, JCS, L1Maxm, 
     &                L2Maxm, L12mxm, J12mxt, LDWmxa, LDWmxb, LDWmxc, 
     &                KASe, NBSamp, L1Tr, L2Tr, L1Tw, L2Tw, J1Tw, J2Tw, 
     &                JLMitw, JLMxtw, JLRang
      COMMON /BRMH  / WR1, WR2, SGMa, SGMat, P, CSUm2, XAMp, PLM10m, 
     &                PLM20m, FAClm, RACie, C1Mem, SQRt10, CONst1, 
     &                CONst2
      COMMON /CHANEL/ TMAsr, PMAsr, RMAsr, CHArgr, ARAtio, CFUnir, VSXr, 
     &                WSXr, WSFr, VSOr, DFNr, DFNwr, DFNsr, DFNspr, 
     &                RZEr, RZEwr, RZEsr, RZEspr, RZEcr, LLRow, NNDim, 
     &                QVAlue, ECM, CE, WN, WNIni, SGMazz
      COMMON /CNTROL/ KTRl, KEXcom, EXTcom, KTLout
      COMMON /LEGENC/ LCAltr, MMXtr, RADian, PLM20, PLM10, PL
      COMMON /PARAMT/ NCHanl, MXRow, NXMax, NXCple, NANglr, LMAx, 
     &                LTRamx, WNUnit, ETUnit, XMEs, ANGler, THEta, TTR, 
     &                TTI, ZERo, ELAb, ETA, XBAr, XMAx, SGMaz, RHOmx, 
     &                RD, LDWmxr
      COMMON /RACFAC/ FAClog, RAC, U9
      COMMON /SPIN  / ISTw, JJ
C
C Local variables
C
      DOUBLE PRECISION an(21), s1, sqrt2i, th, wl, wq(21), wr
      CHARACTER*1 hola, holl, holr, holx, nlrw
      INTEGER istw23, l0posi, l1, l1p1, l1p1mx, l2p1, l2p1mx, lbcp1x, 
     &        ll, llmax, lm10ps, lm20ps, lmm, lmp, lmpos, mm, mm1, n, 
     &        n1, n1mx, n1wx, n2mx, na, nanglx, nb, njl1, njl1mx, nlr, 
     &        nr, nr1, nw1, nw2
      INTEGER MAX0, MIN0
C
C
C
C
C
      DATA holr, holl, hola, holx/'R', 'L', 'A', 'X'/
      SQRt10 = SQRT(10.)
      sqrt2i = 1./SQRT(2.)
      CONst1 = (RMAsr(1)*WN(2))/(RMAsr(2)*WN(1)**3)
      CONst2 = (RMAsr(1)*WN(NCHanl))/(RMAsr(NCHanl)*WN(1)**3)
C
      ISTw1 = ISTw(1)
      ISTw2 = ISTw(2)
      ISTw3 = ISTw(3)
      istw23 = ISTw2 + ISTw3
      IF(istw23.EQ.0)KASe = 3*ISTw1 + 1
      IF(istw23.GE.1)KASe = istw23 + 1
      IF(NCHanl.EQ.2)ISTw3 = ISTw2
C
      JAS = 1
      JCS = 1
      JBS = 1
      L1Maxm = LTRamx(1)
      L2Maxm = LTRamx(2)
      L12mxm = L1Maxm + L2Maxm
      J12mxt = 2*L12mxm + (1 - MOD(KASe, 2))
      LDWmxa = LDWmxr(1)
      LDWmxb = LDWmxr(NCHanl)
      LDWmxc = LDWmxr(2)
C
      lbcp1x = MAX0(LDWmxc, LDWmxb) + 1
      REWIND 7
      l1p1mx = L1Maxm + 1
      l2p1mx = L2Maxm + 1
C<<<<<<<<<<<<< variant A
C     IF(NCHANL .EQ. 2) L2P1MX = 1
      IF(KTLout(4).NE.0)WRITE(6, 99001)
99001 FORMAT(/' ENTER XSEC')
C
C
C
CB    CALCULATION OF LEGENDRE-POLYNOMIALS
C<<<<<<<<<<<<< variant A
C     LLMIN=3
      llmax = lbcp1x
C
      DO na = 1, NANglr
         RADian = THEta(na)
         P(1, na) = 1.
         P(2, na) = COS(RADian)
         s1 = SIN(RADian)
         P(3, na) = s1*sqrt2i
      ENDDO
C
      lm20ps = 1
      lm10ps = 2
      l0posi = 4
C
      DO ll = 3, llmax
         LCAltr = ll - 1
         MMXtr = MIN0(L12mxm + 1, LCAltr)
         mm1 = MMXtr + 1
C
         DO mm = 1, mm1
            lmp = ll + mm - 1
            lmm = ll - mm + 1
            FAClm(mm) = EXP(.5*(FAClog(lmm) - FAClog(lmp)))
         ENDDO
C
         DO na = 1, NANglr
            RADian = THEta(na)
            PLM10 = P(lm10ps, na)
            PLM20 = P(lm20ps, na)
            CALL LEGNDR
C
            DO mm = 1, mm1
               lmpos = l0posi + mm - 1
               P(lmpos, na) = PL(mm)*FAClm(mm)
            ENDDO
         ENDDO
C
         lm20ps = lm10ps
         lm10ps = l0posi
         l0posi = l0posi + MMXtr + 1
      ENDDO
C
C
CB    CALCULATION OF CROSS SECTIONS
C
      n1mx = (L1Maxm + 1)*NANglr
      n2mx = (L2Maxm + 1)*n1mx
      n1wx = n1mx + KTRl(8)*n1mx
C
      DO nlr = 1, 2
         DO nw1 = 1, n1wx
            WR1(nw1, nlr) = 0.
         ENDDO
         DO nw2 = 1, n2mx
            WR2(nw2, nlr) = 0.
         ENDDO
      ENDDO
C
      CALL XSC12
C
C
CB    OUTPUT OF CROSS SECTIONS
C
      IF(KTLout(4).NE.0)WRITE(6, 99002)L1Maxm, L2Maxm
99002 FORMAT('1', 47X, ' CROSS SECTIONS FOR L1MAX=', I2, ' L2MAX=', I2/)
C
      nanglx = MIN(NANglr, 7)
C
      l1p1mx = L1Maxm + 1
      l2p1mx = L2Maxm + 1
      njl1mx = 1 + (ISTw1 - ISTw2)
C
      IF(KTRl(8).EQ.0)njl1mx = 1
      IF(KASe.NE.2)THEN
C
C
CB       OUTOUT OF ONE-STEP CROSS SECTIONS
C
         IF(KTLout(4).NE.0)THEN
            WRITE(6, 99003)
C
99003       FORMAT(//44X, 10('*'), ' ONE-STEP CROSS SECTIONS ', 10('*')
     &             /)
            IF(KTRl(8).EQ.1)WRITE(6, 99004)
99004       FORMAT(44X, 9('*'), '  J1LOW AND J1UP SEPERATED ', 9('*')/)
            WRITE(6, 99005)(ANGler(n), n = 1, nanglx)
99005       FORMAT(7X, 'L/R L1  J1 ', 5X, 40(4X, F5.1, 4X))
         ENDIF
C
C
         DO l1p1 = 1, l1p1mx
C
            L1Tr = l1p1 - 1
            L1Tw = L1Tr + L1Tr
C
            DO njl1 = 1, njl1mx
               IF(KTRl(8).EQ.0)THEN
C
                  J1Tw = L1Tw
               ELSE
                  J1Tw = L1Tw + (2*njl1 - 3)
               ENDIF
               IF(J1Tw.GE.0)THEN
                  IF(KTRl(8).EQ.0)THEN
C
                     nr1 = L1Tr*NANglr + 1
                  ELSE
                     nr1 = nr1 + (njl1 - 1)*n1mx
                  ENDIF
C
                  DO na = 1, NANglr
                     nb = nr1 - 1 + na
                     SGMat(na, 1) = 0.5*(WR1(nb, 1) + WR1(nb, 2))
                     SGMat(na, 2) = 0.5*(WR1(nb, 1) - WR1(nb, 2))
     &                              /SGMat(na, 1)
                  ENDDO
C
                  IF(KTLout(4).NE.0)THEN
                     WRITE(6, 99006)
C
99006                FORMAT(' ')
                     nlrw = holl
                     WRITE(6, 99012)nlrw, L1Tr, J1Tw, 
     &                              (WR1(nr, 1), nr = nr1, 
     &                              nr1 + nanglx - 1)
                     nlrw = holr
                     WRITE(6, 99012)nlrw, L1Tr, J1Tw, 
     &                              (WR1(nr, 2), nr = nr1, 
     &                              nr1 + nanglx - 1)
                     nlrw = holx
                     WRITE(6, 99012)nlrw, L1Tr, J1Tw, 
     &                              (SGMat(na, 1), na = 1, nanglx)
                     nlrw = hola
                     WRITE(6, 99007)nlrw, L1Tr, J1Tw, 
     &                              (SGMat(na, 2), na = 1, nanglx)
99007                FORMAT(8X, A1, 2I4, '/2', 5X, 40F13.5)
                  ENDIF
C
               ENDIF
            ENDDO
         ENDDO
C
C
         IF(NCHanl.EQ.2)GOTO 100
      ENDIF
C
C
CB    OUTPUT OF TWO-STEP CROSS SECTIONS
C
      IF(KTLout(4).NE.0)THEN
         WRITE(6, 99008)
C
99008    FORMAT(//44X, 10('*'), ' TWO-STEP CROSS SECTIONS ', 10('*')/)
         WRITE(6, 99009)(ANGler(na), na = 1, nanglx)
99009    FORMAT(6X, 'L/R L1   L2 ', 40(4X, F5.1, 4X))
      ENDIF
C
C
      DO l1p1 = 1, l1p1mx
         L1Tr = l1p1 - 1
C
         DO l2p1 = 1, l2p1mx
            L2Tr = l2p1 - 1
C
            nr1 = NANglr*(L1Tr*(L2Maxm + 1) + L2Tr) + 1
C
            DO na = 1, NANglr
               nb = nr1 - 1 + na
               SGMat(na, 1) = 0.5*(WR2(nb, 1) + WR2(nb, 2))
               SGMat(na, 2) = 0.5*(WR2(nb, 1) - WR2(nb, 2))/SGMat(na, 1)
            ENDDO
C
            IF(KTLout(4).NE.0)THEN
               WRITE(6, 99010)
C
99010          FORMAT(' ')
               nlrw = holl
               WRITE(6, 99013)nlrw, L1Tr, L2Tr, 
     &                        (WR2(nr, 1), nr = nr1, nr1 + nanglx - 1)
               nlrw = holr
               WRITE(6, 99013)nlrw, L1Tr, L2Tr, 
     &                        (WR2(nr, 2), nr = nr1, nr1 + nanglx - 1)
               nlrw = holx
               WRITE(6, 99013)nlrw, L1Tr, L2Tr, 
     &                        (SGMat(na, 1), na = 1, nanglx)
               nlrw = hola
               WRITE(6, 99011)nlrw, L1Tr, L2Tr, 
     &                        (SGMat(na, 2), na = 1, nanglx)
99011          FORMAT(7X, A1, I4, I5, 1X, 40F13.5)
            ENDIF
C
         ENDDO
      ENDDO
C
 100  REWIND 7
C
      DO nlr = 1, 2
         IF(KASe.NE.2)THEN
            WRITE(12, *)(WR1(n, nlr), n = 1, n1wx)
            IF(NCHanl.EQ.2)GOTO 200
         ENDIF
         WRITE(12, *)(WR2(n, nlr), n = 1, n2mx)
 200  ENDDO
C
C
C
CWQAN AUSGABE NACH OUTWQAN
C
      IF(1.EQ.1)THEN
         OPEN(19, FILE = 'wq1.p', STATUS = 'unknown')
         OPEN(20, FILE = 'ay1.p', STATUS = 'unknown')
C
C        L1=L1OUTP
C
         DO na = 1, NANglr
            DO l1 = 1, l1p1mx
C
               th = ANGler(na)
               n1 = l1*NANglr + na
C
               wl = WR1(n1, 1)
               wr = WR1(n1, 2)
C
               wq(l1) = 0.5*(wl + wr)
               an(l1) = (wl - wr)/(wl + wr)
C
            ENDDO
C
            WRITE(19, 99014)th, (wq(l1), l1 = 1, l1p1mx)
            WRITE(20, 99014)th, (an(l1), l1 = 1, l1p1mx)
C
         ENDDO
      ENDIF
99012 FORMAT(8X, A1, 2I4, '/2', 5X, 40E13.5)
99013 FORMAT(7X, A1, I4, I5, 1X, 40E13.5)
99014 FORMAT(F9.5, 8E12.4)
      END
C
C
C***********************************************************************
      SUBROUTINE XSC12
C***********************************************************************
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C
C PARAMETER definitions
C
      INTEGER NGLXX
      PARAMETER(NGLXX = 100)
C
C COMMON variables
C
      DOUBLE PRECISION ANGler(NGLXX), C1Mem(2), CLEbmm(250), CONst1, 
     &                 CONst2, ELAb, ETA, ETUnit, EXTcom(50), 
     &                 FAClm(NGLXX), FAClog(500), P(975, NGLXX), 
     &                 PLM10m(NGLXX), PLM20m(NGLXX), RAC, RACie(50), RD, 
     &                 RHOmx, SGMa(25, NGLXX, 2), SGMat(NGLXX, 2), 
     &                 SGMaz, SQRt10, THEta(NGLXX), U9, WNUnit, 
     &                 WR1(1000, 2), WR2(5000, 2), XBAr, XMAx, XMEs
      DOUBLE COMPLEX CSUm2(NGLXX), TTI, TTR, XAMp(8300, 4), ZERo
      INTEGER IA, IB, IC, ID, IE, IG, ISTw1, ISTw2, ISTw3, J12mxt, J1Tw, 
     &        J2Tw, JAS, JBS, JCS, JLMitw, JLMxtw, JLRang, KASe, 
     &        KEXcom(50), KTLout(50), KTRl(30), L12mxm, L1Maxm, L1Tr, 
     &        L1Tw, L2Maxm, L2Tr, L2Tw, L9(9), LDWmxa, LDWmxb, LDWmxc, 
     &        LDWmxr(4), LMAx, LTRamx(4), MXRow, NANglr, NBSamp(40), 
     &        NCHanl, NXCple, NXMax
      COMMON /BIMH  / ISTw1, ISTw2, ISTw3, JAS, JBS, JCS, L1Maxm, 
     &                L2Maxm, L12mxm, J12mxt, LDWmxa, LDWmxb, LDWmxc, 
     &                KASe, NBSamp, L1Tr, L2Tr, L1Tw, L2Tw, J1Tw, J2Tw, 
     &                JLMitw, JLMxtw, JLRang
      COMMON /BRMH  / WR1, WR2, SGMa, SGMat, P, CSUm2, XAMp, PLM10m, 
     &                PLM20m, FAClm, RACie, C1Mem, SQRt10, CONst1, 
     &                CONst2
      COMMON /CLEBAB/ CLEbmm
      COMMON /CNTROL/ KTRl, KEXcom, EXTcom, KTLout
      COMMON /PARAMT/ NCHanl, MXRow, NXMax, NXCple, NANglr, LMAx, 
     &                LTRamx, WNUnit, ETUnit, XMEs, ANGler, THEta, TTR, 
     &                TTI, ZERo, ELAb, ETA, XBAr, XMAx, SGMaz, RHOmx, 
     &                RD, LDWmxr
      COMMON /RACFAC/ FAClog, RAC, U9
      COMMON /RACFACI/ IA, IB, IC, ID, IE, IG, L9
C
C Local variables
C
      DOUBLE PRECISION fms, ph, t1, xconst, xsig1(0:10, 100), 
     &                 xsig2(0:10, 0:10, 100)
      INTEGER IABS, MIN0
      INTEGER ii, iimax, iimin, is3ke3, ji, jl, jla, jlamx, jlb, jlbmi, 
     &        jlbmx, jlmod, jlmttl, jlr, jlrg, jltohf, jx, kaspar, ke3, 
     &        l1, l1p1, l1p1mx, l2, l2p1, l2p1mx, lap1mx, lbp1mx, m, mm, 
     &        mmmax, mrg, ms, n, n1, n1mx, n2, na, namp, nampmi, nampmx, 
     &        njl1, njl1mx, njl2, njl2mx, nlr, nrun, nrun0
      DOUBLE COMPLEX phlr, xss1, xss2
C
C
C
      lap1mx = LDWmxa + 1
      lbp1mx = LDWmxb + 1
      l1p1mx = L1Maxm + 1
      l2p1mx = L2Maxm + 1
C
C
CB    CALCULATION OF CLEBSCH-GORDAN-COEFF. AND WRITING TO TAPE8
C
      REWIND 8
      kaspar = 2 - MOD(KASe, 2)
      njl1mx = 1 + (ISTw1 - ISTw2)
      njl2mx = 1 + (ISTw2 - ISTw3)
      n1mx = L1Maxm + 1
C
      DO ke3 = 1, kaspar
         is3ke3 = (1 - ISTw3)*(ke3 - 1)
         jlbmi = 1 + is3ke3
         jlbmx = lbp1mx - is3ke3
C
         DO jlb = jlbmi, jlbmx, JBS
            IB = 2*jlb - (2 - ISTw3)
            jlamx = lap1mx + (1 - ke3)
C
            DO jla = 1, jlamx, JAS
               IA = 2*jla - (2 - ISTw1)
               ji = IABS(IA - IB)
               jx = MIN0(IA + IB, J12mxt)
               IF(ji.LE.jx)THEN
                  jlmttl = ((jx + ji + 2*(ISTw3+1))*(jx - ji + 2))/4
                  jltohf = jlmttl/2
                  jlrg = (jx - ji)/2 + 1
                  IC = ji - 2
                  n1 = 0
C
                  DO jl = 1, jlrg
                     IC = IC + 2
                     mrg = IC/2 + 1
C
                     DO m = 1, mrg
                        IG = -(2*m + kaspar - 3)
                        n1 = n1 + 1
C
                        DO ms = 1, 2
                           ID = 3 - 2*ms
                           IE = IG - ID
                           n2 = n1 + (ms - 1)*jltohf
                           IF(IABS(IE).GT.IB)THEN
                              CLEbmm(n2) = 0.0
                           ELSE
C
                              CALL CLEBSCH
                              CLEbmm(n2) = RAC
                           ENDIF
                        ENDDO
                     ENDDO
                  ENDDO
C
                  WRITE(8)(CLEbmm(n), n = 1, n2)
                  IF(n2.NE.jlmttl)WRITE(6, 99001)jla, jlb, n2, jlmttl
99001             FORMAT(//////'   XSEC-63.  JA,JB,N2,JLMTTL=', 4I5)
               ENDIF
            ENDDO
         ENDDO
      ENDDO
C
C
CB    CALCULATION OF CROSS SECTIONS
C
CB    LOOP OVER L-TRANSFER IN FIRST AND SECOND STEP
C
      DO l1p1 = 1, l1p1mx
C
         L1Tr = l1p1 - 1
         L1Tw = 2*L1Tr
C
         DO njl1 = 1, njl1mx
C
            IF(njl1mx.EQ.1)J1Tw = L1Tw
            IF(njl1mx.EQ.2)J1Tw = L1Tw + (2*njl1 - 3)
            IF(J1Tw.GE.0)THEN
C
               DO l2p1 = 1, l2p1mx
C
                  L2Tr = l2p1 - 1
                  L2Tw = 2*L2Tr
C
                  DO njl2 = 1, njl2mx
C
                     IF(njl2mx.EQ.1)J2Tw = L2Tw
                     IF(njl2mx.EQ.2)J2Tw = L2Tw + (2*njl2 - 3)
                     IF(J2Tw.GE.0)THEN
C
                        JLMitw = IABS(J1Tw - J2Tw)
                        JLMxtw = J1Tw + J2Tw
                        JLRang = (JLMxtw - JLMitw)/2 + 1
                        NBSamp(1) = 0
                        namp = (J1Tw + 1)/2 + (1 - MOD(J1Tw, 2))
                        nampmi = (JLMitw + 1)/2 + (1 - MOD(JLMitw, 2))
C
                        DO jlr = 1, JLRang
                           NBSamp(jlr + 1) = namp
                           namp = namp + nampmi + (jlr - 1)
                        ENDDO
C
                        nampmx = namp*NANglr
                        IF(nampmx.GT.100*100)THEN
                           WRITE(6, *)' sub XSEC - increase XAMP to:', 
     &                                nampmx
                           STOP
                        ENDIF
C
                        DO namp = 1, nampmx
                           DO n = 1, 4
                              XAMp(namp, n) = (0.0, 0.0)
                           ENDDO
                        ENDDO
C
                        CALL XSCABC
C
CB                      CALCULATION OF CROSS SECTIONS FROM X-MATRIX FOR
CB                      L1TR,L2TR
CB                      FIRST  STEP:  L=L1TR
CB                      II=1
CB                      SECOND STEP:  ABS(L2TR-L1TR) <= L <= L1TR+L2TR
CB                      II=2..JLRANG+1
C
                        iimin = 2
                        IF(L2Tr.EQ.0)iimin = 1
                        iimax = JLRang + 1
                        IF(NCHanl.EQ.2)iimax = 1
C
                        DO nlr = 1, 2
                           DO na = 1, NANglr
                              SGMat(na, nlr) = 0.0
                              DO ii = iimin, iimax
                                 SGMa(ii, na, nlr) = 0.0
                              ENDDO
                           ENDDO
                        ENDDO
C
                        DO ii = iimin, iimax
C
                           IF(ii.EQ.1)THEN
                              xconst = CONst1
                              jlmod = 1 - MOD(J1Tw, 2)
                              mmmax = (J1Tw + 1)/2 + jlmod
                           ELSE
                              xconst = CONst2
                              jlmod = 1 - MOD(JLMitw, 2)
                              mmmax = (JLMitw + 1)/2 + jlmod + (ii - 2)
                           ENDIF
C
                           DO mm = 1, mmmax
C
                              fms = xconst
                              IF(mm.EQ.1 .AND. jlmod.EQ.1)
     &                           fms = 0.5*xconst
                              namp = (NBSamp(ii) + mm - 1)*NANglr
C
                              DO na = 1, NANglr
                                 namp = namp + 1
C
                                 DO nlr = 1, 2
                                    ph = 3 - 2*nlr
                                    phlr = ph*(0.0, 1.0)
C
                                    IF(ISTw3.EQ.0)THEN
                                       xss1 = XAMp(namp, 1)
     &                                    + phlr*XAMp(namp, 2)
                                       t1 = fms*(xss1*DCONJG(xss1))
                                    ELSEIF(mm.EQ.1)THEN
                                       xss1 = XAMp(namp, 1)
     &                                    + phlr*XAMp(namp, 2)
                                       xss2 = XAMp(namp, 3)
     &                                    - phlr*XAMp(namp, 4)
                                       t1 = fms*(xss1*DCONJG(xss1)
     &                                    + xss2*DCONJG(xss2))
                                    ELSE
                                       xss1 = XAMp(namp, 1)
     &                                    - phlr*XAMp(namp, 2)
                                       xss2 = XAMp(namp, 3)
     &                                    - phlr*XAMp(namp, 4)
                                       t1 = fms*(xss1*DCONJG(xss1)
     &                                    + xss2*DCONJG(xss2))
                                    ENDIF
C
                                    SGMa(ii, na, nlr)
     &                                 = SGMa(ii, na, nlr) + t1
C
                                 ENDDO
C
                              ENDDO
                           ENDDO
C
                        ENDDO
C
C
CB                      SAVE ONE-STEP CROSS SECTION FOR L1TR IN WR1
C
                        IF(L2Tr.EQ.0)THEN
C
                           nrun0 = L1Tr + (njl1 - 1)*n1mx
                           nrun = nrun0*NANglr
                           IF(KTRl(8).EQ.0)nrun = L1Tr*NANglr
C
                           DO na = 1, NANglr
                              nrun = nrun + 1
                              DO nlr = 1, 2
                                 WR1(nrun, nlr) = SGMa(1, na, nlr)
                              ENDDO
                              xsig1(L1Tr, na)
     &                           = 0.5*(SGMa(1, na, 1) + SGMa(1, na, 2))
                           ENDDO
C
                        ENDIF
C
CB                      SAVE TWO-STEP CROSS SECTION FOR L1TR,L2TR IN WR2
C
                        IF(NCHanl.EQ.3)THEN
C
                           DO ii = 2, iimax
                              DO na = 1, NANglr
                                 DO nlr = 1, 2
                                    SGMat(na, nlr) = SGMat(na, nlr)
     &                                 + SGMa(ii, na, nlr)
                                 ENDDO
                              ENDDO
                           ENDDO
C
                           nrun = (L1Tr*(L2Maxm + 1) + L2Tr)*NANglr
                           DO na = 1, NANglr
                              nrun = nrun + 1
                              DO nlr = 1, 2
                                 WR2(nrun, nlr) = SGMat(na, nlr)
                              ENDDO
                              xsig2(L1Tr, L2Tr, na)
     &                           = 0.5*(SGMat(na, 1) + SGMat(na, 2))
                           ENDDO
C
                        ENDIF
C
99002                   FORMAT(5X, 8E11.4)
C
                     ENDIF
                  ENDDO
C<<<<<<<<<<<<<    variant A
C1005             CONTINUE
               ENDDO
            ENDIF
         ENDDO
C
      ENDDO
C
C     Output of Cross Sections
C
      OPEN(3, FILE = 'xs.d', STATUS = 'unknown')
      DO l1 = 0, L1Maxm
         WRITE(6, 99003)l1, l1, (l2, l2 = 0, MIN(9, L2Maxm))
99003    FORMAT(/' 1- and 2-step Cross Sections for L1:', I3/'   Theta', 
     &          5X, 'L1:', I2, 9(7X, 'L2:', I2))
         WRITE(3, 99004)l1, l1, (l2, l2 = 0, MIN(9, L2Maxm))
99004    FORMAT('# 1- and 2-step Cross Sections for L1:', I3/'#  Theta', 
     &          8X, 'L1:', I2, 9(10X, 'L2:', I2))
         DO na = 1, NANglr
            WRITE(6, 99005)ANGler(na), xsig1(l1, na), 
     &                     (xsig2(l1, l2, na), l2 = 0, MIN(9, L2Maxm))
99005       FORMAT(F8.3, 10E12.4)
            WRITE(3, 99006)ANGler(na), xsig1(l1, na), 
     &                     (xsig2(l1, l2, na), l2 = 0, MIN(9, L2Maxm))
99006       FORMAT(F9.4, 10E15.7)
         ENDDO
         WRITE(3, 99007)
99007    FORMAT()
      ENDDO
      CLOSE(3)
      END
C
C
C***********************************************************************
      SUBROUTINE XSCABC
C***********************************************************************
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C
C PARAMETER definitions
C
      INTEGER NGLXX
      PARAMETER(NGLXX = 100)
C
C COMMON variables
C
      DOUBLE PRECISION ANGler(NGLXX), C1Mem(2), CLEbmm(250), CONst1, 
     &                 CONst2, ELAb, ETA, ETUnit, FAClm(NGLXX), 
     &                 FAClog(500), P(975, NGLXX), PLM10m(NGLXX), 
     &                 PLM20m(NGLXX), RAC, RACie(50), RD, RHOmx, 
     &                 SGMa(25, NGLXX, 2), SGMat(NGLXX, 2), SGMaz, 
     &                 SQRt10, THEta(NGLXX), U9, WNUnit, WR1(1000, 2), 
     &                 WR2(5000, 2), XBAr, XMAx, XMEs
      DOUBLE COMPLEX CMAt1(42, 61), CMAt2(42, 42, 61), CSUm2(NGLXX), 
     &               TTI, TTR, XAMp(8300, 4), ZERo
      INTEGER IA, IB, IC, ID, IE, IG, ISTw1, ISTw2, ISTw3, J12mxt, J1Tw, 
     &        J2Tw, JAS, JBS, JCS, JLMitw, JLMxtw, JLRang, KASe, L12mxm, 
     &        L1Maxm, L1Tr, L1Tw, L2Maxm, L2Tr, L2Tw, L9(9), LDWmxa, 
     &        LDWmxb, LDWmxc, LDWmxr(4), LMAx, LTRamx(4), MXRow, NANglr, 
     &        NBSamp(40), NCHanl, NXCple, NXMax
      COMMON /BIMH  / ISTw1, ISTw2, ISTw3, JAS, JBS, JCS, L1Maxm, 
     &                L2Maxm, L12mxm, J12mxt, LDWmxa, LDWmxb, LDWmxc, 
     &                KASe, NBSamp, L1Tr, L2Tr, L1Tw, L2Tw, J1Tw, J2Tw, 
     &                JLMitw, JLMxtw, JLRang
      COMMON /BRMH  / WR1, WR2, SGMa, SGMat, P, CSUm2, XAMp, PLM10m, 
     &                PLM20m, FAClm, RACie, C1Mem, SQRt10, CONst1, 
     &                CONst2
      COMMON /CLEBAB/ CLEbmm
      COMMON /CMATR / CMAt1, CMAt2
      COMMON /PARAMT/ NCHanl, MXRow, NXMax, NXCple, NANglr, LMAx, 
     &                LTRamx, WNUnit, ETUnit, XMEs, ANGler, THEta, TTR, 
     &                TTI, ZERo, ELAb, ETA, XBAr, XMAx, SGMaz, RHOmx, 
     &                RD, LDWmxr
      COMMON /RACFAC/ FAClog, RAC, U9
      COMMON /RACFACI/ IA, IB, IC, ID, IE, IG, L9
C
C Local variables
C
      DOUBLE PRECISION a1, a2, ab, b1, b12sjc, b1b2, b2, c1, c2, c3, c5, 
     &                 h12, h1p, h2p, hatac1, hatbc2, hatj1, hatj2, 
     &                 hatja, hatjb, hatjc, hatl1, hatl2, hatla, hatlb, 
     &                 hatlc, s1, s1p, s2, s2p, s5, sga, sjc
      DOUBLE COMPLEX ari, cmri, csum1, eri
      REAL FLOAT
      INTEGER IABS, MAX0, MIN0
      INTEGER ie1, ie1max, ie1min, ii, iimax, iimin, ja, jadpmx, jami, 
     &        jamit8, jamitw, jamx, jamx8, jamxp1, jamxt8, jamxtw, jatw, 
     &        jb, jbmx, jbtw, jc, jcmi, jcmitw, jcmx, jcmxtw, jctw, jj1, 
     &        jj2, jl, jlmmmm, jltohf, jtl, jtwmi, jtwmx, kaspar, ktr, 
     &        la, latw, lb, lbtw, lc, lctw, legbas, ll, ll1, ll2, lmpos, 
     &        loccbc, mbast, mjlbtw, mjtw, mlb, mm, mmmax, ms, msmsp, 
     &        msp, msprg, msptw, msrg, mstw, n1, n2, na, namp, njla, 
     &        njlb, njlb0, njlc, njlc0, nla, nlb, nlc
C
C
C
      hatl1 = SQRT(FLOAT(L1Tw + 1))
      hatl2 = SQRT(FLOAT(L2Tw + 1))
      hatj1 = SQRT(FLOAT(J1Tw + 1))
      hatj2 = SQRT(FLOAT(J2Tw + 1))
C
      kaspar = 2 - MOD(KASe, 2)
      REWIND 8
      jbmx = LDWmxb + 1
      jamx8 = LDWmxa + 1
      jamit8 = ISTw1
      jamxt8 = 2*jamx8 - 2 + ISTw1
C
C
      DO jb = 1, jbmx, JBS
C
         jbtw = 2*jb - 2 + ISTw3
C
         jamitw = MAX0(jbtw - JLMxtw, jamit8)
         jamxtw = MIN0(jbtw + JLMxtw, jamxt8)
         jami = (jamitw + ISTw1)/2 + (1 - ISTw1)
         jamx = (jamxtw + ISTw1)/2 + (1 - ISTw1)
C
         IF(jami.GT.jamx)THEN
            DO ja = 1, jamx8, JAS
               jatw = 2*ja - 2 + ISTw1
               CALL CLEBRD(jatw, jbtw, J12mxt, kaspar, jtl)
            ENDDO
            GOTO 100
         ENDIF
C
C
         DO nlb = 0, ISTw3
C
            lb = jb - 1 + nlb
            lbtw = 2*lb
C
            IF(lb.LE.LDWmxb)THEN
C
               njlb0 = 2*L2Maxm + lb + (jbtw + 3)/2
C
               hatlb = SQRT(FLOAT(lbtw + 1))
               hatjb = SQRT(FLOAT(jbtw + 1))
C
               legbas = 0
               DO ll = 1, lb
                  legbas = legbas + MIN0(L12mxm + 1, ll - 1) + 1
               ENDDO
               IF(lb.EQ.0)legbas = 0
C
               IF(nlb.NE.0)THEN
                  DO ja = jamx, jami, -JAS
                     BACKSPACE 8
                  ENDDO
               ELSEIF(jami.NE.1)THEN
                  jadpmx = jami - JAS
                  DO ja = 1, jadpmx, JAS
                     jatw = 2*ja - 2 + ISTw1
                     CALL CLEBRD(jatw, jbtw, J12mxt, kaspar, jtl)
                  ENDDO
               ENDIF
C
               DO ja = jami, jamx, JAS
C
                  jatw = 2*ja - 2 + ISTw1
C
                  CALL CLEBRD(jatw, jbtw, J12mxt, kaspar, jtl)
                  jltohf = jtl/2
C
                  jcmitw = MAX0(IABS(jatw - J1Tw), IABS(jbtw - J2Tw))
                  jcmxtw = MIN0(jatw + J1Tw, jbtw + J2Tw, 
     &                     2*LDWmxc + ISTw2)
                  jcmi = (jcmitw + ISTw2)/2 + (1 - ISTw2)
                  jcmx = (jcmxtw + ISTw2)/2 + (1 - ISTw2)
C
                  IF(KASe.EQ.4)THEN
                     jcmi = jcmi + MOD(jcmi + 1, 2)
                     jcmx = jcmx - MOD(jcmi + 1, 2)
                  ENDIF
C
                  IF(jcmi.LE.jcmx)THEN
C
C
                     DO nla = 0, ISTw1
C
                        la = ja - 1 + nla
                        latw = 2*la
C
                        IF(la.LE.LDWmxa)THEN
C
                           njla = la + (jatw + 1)/2
                           njlc0 = 2*L1Maxm - latw
C
                           hatla = SQRT(FLOAT(latw + 1))
                           hatja = SQRT(FLOAT(jatw + 1))
C
                           csum1 = (0.0, 0.0)
                           jlmmmm = L12mxm + 2
                           DO jl = 1, jlmmmm
                              CSUm2(jl) = (0.0, 0.0)
                           ENDDO
C
C
                           DO jc = jcmi, jcmx, JCS
                              DO nlc = 0, ISTw2
C
                                 lc = jc - 1 + nlc
                                 lctw = 2*lc
                                 jctw = lctw*(1 - ISTw2) + (2*jc - 1)
     &                                  *ISTw2
C
                                 IF(KASe.EQ.2 .OR. KASe.EQ.3)
     &                              ktr = MIN0(jatw + jctw - L1Tw, 
     &                              L1Tw - IABS(jatw - jctw))
                                 IF(KASe.EQ.4)
     &                              ktr = MIN0(jatw + lctw - J1Tw, 
     &                              J1Tw - IABS(jatw - lctw))
                                 IF(ktr.LT.0)GOTO 2
C
                                 IF(L1Tr.GE.ABS(la - lc) .AND. 
     &                              L1Tr.LE.la + lc)THEN
                                    IF(MOD(la + lc - L1Tr, 2).EQ.0)THEN
                                       IF(lc.LE.LDWmxc)THEN
CB
CB                                        CALCULATION OF FIRST STEP PART
CB
                                         hatlc = SQRT(FLOAT(lctw + 1))
                                         hatjc = SQRT(FLOAT(jctw + 1))
C
                                         hatac1 = (hatla*hatlc)/hatl1
                                         IA = latw
                                         IB = lctw
                                         IC = L1Tw
                                         CALL CLEBZ
                                         c1 = RAC
                                         s1 = 1 - 
     &                                      2*MOD((la + lc - L1Tr)/2, 2)
                                         a1 = hatac1*s1*c1*0.2820948
                                         IA = latw
                                         IB = jatw
                                         b1 = a1
C
                                         IF(KASe.EQ.1)THEN
                                         ELSEIF(KASe.EQ.4)THEN
C
                                         IC = L1Tw
                                         ID = J1Tw
                                         IG = lctw
                                         CALL RACHLF
                                         h1p = hatja*hatl1
                                         b1 = a1*RAC*h1p
                                         ELSE
C
                                         IC = lctw
                                         ID = jctw
                                         IG = L1Tw
                                         CALL RACHLF
                                         s1p = 1 - 
     &                                      2*MOD((jatw + jctw + L1Tw)
     &                                      /2, 2)
                                         h1p = hatja*hatjc/hatl1
                                         b1 = a1*RAC*s1p*h1p
                                         ENDIF
C
                                         njlc = njlc0 + lc + (jctw + 3)
     &                                      /2
C
                                         IF(KASe.NE.2 .AND. 
     &                                      L2Tr.EQ.0 .AND. lb.EQ.lc)
     &                                      THEN
                                         csum1 = b1*CMAt1(njlc, njla)
                                         IF(KASe.EQ.1)
     &                                      csum1 = csum1/hatl1
                                         ENDIF
C
C
                                         IF(NCHanl.NE.2 .AND. 
     &                                      lb.LE.LDWmxb)THEN
CB
CB                                           CALCULATION OF SECOND STEP PART
CB
                                         jtwmi = MAX0(IABS(jatw - jbtw), 
     &                                      JLMitw)
                                         jtwmx = MIN0(jatw + jbtw, 
     &                                      JLMxtw)
                                         IF(jtwmi.LE.jtwmx)THEN
C
                                         ktr = MIN0(jctw + jbtw - L2Tw, 
     &                                      L2Tw - IABS(jctw - jbtw))
                                         IF(kaspar.NE.1 .OR. ktr.GE.0)
     &                                      THEN
                                         IF(L2Tr.GE.ABS(lb - lc) .AND. 
     &                                      L2Tr.LE.lb + lc)THEN
C
                                         IA = lctw
                                         IB = lbtw
                                         IC = L2Tw
                                         CALL CLEBZ
                                         c2 = RAC
                                         s2 = 1 - 
     &                                      2*MOD((lc + lb - L2Tr)/2, 2)
                                         hatbc2 = (hatlb*hatlc)/hatl2
                                         a2 = hatbc2*c2*s2*0.2820948
                                         b2 = a2
C
                                         IF(KASe.EQ.1 .OR. KASe.EQ.4)
     &                                      THEN
                                         ELSEIF(KASe.EQ.3)THEN
C
                                         IA = lbtw
                                         IB = jbtw
                                         IC = lctw
                                         ID = jctw
                                         IG = L2Tw
                                         CALL RACHLF
                                         s2p = 1 - 
     &                                      2*MOD((jctw + jbtw + L2Tw)
     &                                      /2, 2)
                                         h2p = hatjc*hatjb/hatl2
                                         b2 = a2*RAC*h2p*s2p
                                         ELSE
C
                                         IA = L2Tw
                                         IB = J2Tw
                                         IC = lctw
                                         ID = jctw
                                         IG = lbtw
                                         CALL RACHLF
                                         h2p = hatjc*hatl2
                                         b2 = a2*RAC*h2p
                                         ENDIF
C
                                         b1b2 = b1*b2
C
                                         IF(KASe.EQ.1 .OR. KASe.EQ.4)
     &                                      sjc = 1.
                                         IF(KASe.EQ.2 .OR. KASe.EQ.3)
     &                                      sjc = 1 - 
     &                                      2*MOD((jctw + 1 - lctw)/2, 
     &                                      2)
C
                                         IF(KASe.EQ.1)h12 = 1.
                                         IF(KASe.EQ.2)h12 = hatl1*hatj2
                                         IF(KASe.EQ.3)h12 = hatl1*hatl2
                                         IF(KASe.EQ.4)h12 = hatj1*hatl2
C
                                         b12sjc = b1b2*sjc*h12
C
                                         njlb = njlb0 - lctw
                                         cmri = CMAt2(njlb, njlc, njla)
C
                                         IF(KASe.EQ.2)THEN
C
                                         IA = L1Tw
                                         IB = J2Tw
                                         IC = jatw
                                         ID = lbtw
                                         IG = jctw
                                         ELSEIF(KASe.EQ.3)THEN
C
                                         IA = L1Tw
                                         IB = L2Tw
                                         IC = jatw
                                         ID = jbtw
                                         IG = jctw
                                         ELSEIF(KASe.EQ.4)THEN
C
                                         IA = J1Tw
                                         IB = L2Tw
                                         IC = jatw
                                         ID = lbtw
                                         IG = lctw
                                         ELSE
C
                                         IA = L1Tw
                                         IB = L2Tw
                                         IC = latw
                                         ID = lbtw
                                         IG = lctw
                                         ENDIF
C
                                         CALL RACSIM(RACie)
                                         ie1min = MAX0(IABS(IA - IB), 
     &                                      IABS(IC - ID)) + 1
                                         ie1max = MIN0(IA + IB, IC + ID)
     &                                      + 1
                                         n1 = 0
C
                                         DO ie1 = ie1min, ie1max, 2
                                         n1 = n1 + 1
                                         n2 = (ie1 + 1)/2
                                         eri = b12sjc*RACie(n1)*cmri
                                         CSUm2(n2) = CSUm2(n2) + eri
                                         ENDDO
C
                                         ENDIF
                                         ENDIF
                                         ENDIF
                                         ENDIF
                                       ENDIF
                                    ENDIF
                                 ENDIF
                              ENDDO
 2                         ENDDO
C
C
                           iimax = JLRang + 1
                           IF(L2Tr.EQ.0 .AND. lb.LE.LDWmxc)iimin = 1
                           IF(L2Tr.GT.0 .OR. lb.GT.LDWmxc)iimin = 2
                           IF(NCHanl.EQ.2 .OR. lb.GT.LDWmxb)iimax = 1
                           IF(KASe.EQ.2)iimin = 2
C
                           sga = 1 - 2*MOD(la, 2)
                           ab = SQRt10*hatla*hatlb*sga
                           C1Mem(1) = 1.
C
                           IF(KASe.NE.1)THEN
                              DO ms = 1, 2
                                 IA = latw
                                 IC = jatw
                                 ID = 0
                                 IE = 3 - 2*ms
                                 IG = 3 - 2*ms
                                 CALL CLEBHF
                                 C1Mem(ms) = RAC
                              ENDDO
                           ENDIF
C
C
                           DO ii = iimin, iimax
C
                              IF(kaspar.EQ.2)THEN
C
                                 IF(ii.EQ.1)mmmax = (J1Tw + 1)/2
                                 IF(ii.GT.1)mmmax = (JLMitw + 1)
     &                              /2 + (ii - 2)
                                 jj1 = IABS(jatw - lbtw)
                                 IF(ii.EQ.1)jj2 = J1Tw
                                 IF(ii.GT.1)jj2 = IABS(J1Tw - J2Tw)
     &                              + 2*(ii - 2)
                                 mbast = ((jj2 + jj1)*(jj2 - jj1))/8
                              ELSE
C
                                 IF(ii.EQ.1)mmmax = L1Tr + 1
                                 IF(ii.GT.1)mmmax = JLMitw/2 + (ii - 1)
                                 ll1 = IABS(jatw - jbtw)/2
                                 IF(ii.EQ.1)ll2 = L1Tr
                                 IF(ii.GT.1)ll2 = IABS(L1Tr - L2Tr)
     &                              + (ii - 2)
                                 mbast = (ll2*(ll2 + 1) - ll1*(ll1 + 1))
     &                              /2
                              ENDIF
C
                              IF(mbast.GE.0)THEN
                                 IF(ii.EQ.1)eri = csum1
                                 ie1 = (JLMitw + 2*(ii - 2)) + 1
                                 n2 = (ie1 + 1)/2
                                 IF(ii.GT.1)eri = CSUm2(n2)
                                 IA = lbtw
                                 IC = jbtw
C
                                 DO mm = 1, mmmax
C
                                    mjtw = 2*(mm - 1) + (kaspar - 1)
                                    msrg = 1 + ISTw1
C
                                    DO ms = 1, msrg
C
                                       mstw = (3 - 2*ms)*ISTw1
                                       c1 = C1Mem(ms)
                                       mjlbtw = mjtw + mstw
                                       IF(IABS(mjlbtw).LE.IC)THEN
C
                                         loccbc = mbast + mm + (ms - 1)
     &                                      *jltohf
                                         c3 = CLEbmm(loccbc)
C
                                         IF(KASe.EQ.1)THEN
                                         s5 = 1.
                                         ELSE
                                         s5 = -mstw
                                         ENDIF
C
                                         msprg = 1 + ISTw3
C
                                         DO msp = 1, msprg
C
                                         msmsp = 2*(msp - 1) + ms
                                         c5 = ab*c1*c3
                                         mlb = mjlbtw/2
C
                                         IF(KASe.EQ.3)THEN
                                         msptw = 3 - 2*msp
                                         IE = msptw
                                         IG = mjlbtw
                                         ID = IG - IE
                                         IF(IABS(ID).GT.IA)GOTO 4
                                         CALL CLEBHF
                                         c2 = RAC
                                         c5 = ab*s5*c1*c2*c3
                                         mlb = ID/2
                                         ENDIF
C
                                         lmpos = legbas + IABS(mlb) + 1
                                         namp = (NBSamp(ii) + mm - 1)
     &                                      *NANglr
                                         ari = c5*eri
C
                                         DO na = 1, NANglr
                                         namp = namp + 1
                                         XAMp(namp, msmsp)
     &                                      = XAMp(namp, msmsp)
     &                                      + ari*P(lmpos, na)
                                         ENDDO
C
 4                                       ENDDO
                                       ENDIF
                                    ENDDO
                                 ENDDO
                              ENDIF
                           ENDDO
C
                        ENDIF
                     ENDDO
                  ENDIF
               ENDDO
C
            ENDIF
         ENDDO
C
         IF(jamx.NE.jamx8)THEN
            jamxp1 = jamx + JAS
            DO ja = jamxp1, jamx8, JAS
               jatw = 2*ja - 2 + ISTw1
               CALL CLEBRD(jatw, jbtw, J12mxt, kaspar, jtl)
            ENDDO
         ENDIF
C
 100  ENDDO
C
      END
C
C
C***********************************************************************
      SUBROUTINE LEGNDR
C***********************************************************************
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C
C COMMON variables
C
      INTEGER LCAltr, MMXtr
      DOUBLE PRECISION PL(30), PLM10, PLM20, RADian
      COMMON /LEGENC/ LCAltr, MMXtr, RADian, PLM20, PLM10, PL
C
C Local variables
C
      DOUBLE PRECISION co, cosab, ct, fl, flm1, flp1, fm, fmp1, pl0, 
     &                 plp10, si, twlm1, twlp1
      INTEGER k1, m, mmax
      INTEGER MIN0
C
C
      fl = LCAltr
      flm1 = fl - 1
      flp1 = fl + 1
      twlm1 = fl + flm1
      twlp1 = fl + flp1
      co = COS(RADian)
      cosab = ABS(co)
      IF(ABS(cosab - 1.).GE.1.D-7)THEN
         si = 1./SIN(RADian)
         ct = 2.*co*si
         pl0 = (twlm1*co*PLM10 - flm1*PLM20)/fl
         plp10 = (twlp1*co*pl0 - fl*PLM10)/flp1
         PL(1) = pl0
         PL(2) = flp1*si*(co*pl0 - plp10)
         mmax = MIN0(LCAltr, MMXtr) + 1
         IF(mmax.GE.3)THEN
            fm = 0.
            DO m = 3, mmax
               fmp1 = fm + 1.
               PL(m) = ct*fmp1*PL(m - 1) - (fl - fm)*(fl + fmp1)
     &                 *PL(m - 2)
               fm = fmp1
            ENDDO
         ENDIF
         GOTO 99999
      ENDIF
      k1 = MOD(LCAltr, 2)
      mmax = MMXtr + 1
      PL(1) = co**k1
      DO m = 2, mmax
         PL(m) = 0.
      ENDDO
99999 END
C
C
C***********************************************************************
      SUBROUTINE CLEBSCH
C***********************************************************************
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C
C COMMON variables
C
      DOUBLE PRECISION FAClog(500), RAC, U9
      INTEGER IA, IB, IC, ID, IE, IG, L9(9)
      COMMON /RACFAC/ FAClog, RAC, U9
      COMMON /RACFACI/ IA, IB, IC, ID, IE, IG, L9
C
C Local variables
C
      DOUBLE PRECISION fb, fc2, s1, sqfclg, ssterm, termlg
      INTEGER iabc, iabcp, iamd, iapd, ibca, ibme, ibpe, icab, icmf, 
     &        icpf, k1, k2, k3, nz, nzm1, nzmi, nzmic2, nzmic3, nzmx, 
     &        nzt1, nzt2, nzt3, nzt4, nzt5
      INTEGER IABS, MAX0, MIN0
C
C
      RAC = 0.0
      IF(ID + IE.EQ.IG)THEN
         k1 = IA + IB + IC
         IF(k1.EQ.2*(k1/2))THEN
            k1 = IA + IB - IC
            k2 = IC - IABS(IB - IA)
            k3 = MIN0(k1, k2)
            IF(k3.GE.0)THEN
               IF(( - 1)**(IB + IE).GT.0)THEN
                  IF(( - 1)**(IC + IG).GT.0)THEN
                     IF(IA.GE.IABS(ID))THEN
                        IF(IB.GE.IABS(IE))THEN
                           IF(IC.GE.IABS(IG))THEN
                              IF(IA.GE.0)THEN
                                 IF(IA.EQ.0)THEN
                                    RAC = 1.0
                                 ELSEIF(IB.GE.0)THEN
                                    IF(IB.EQ.0)THEN
                                       RAC = 1.0
                                    ELSEIF(IC.GE.0)THEN
                                       IF(IC.EQ.0)THEN
                                         fb = IB + 1
                                         RAC = ( - 1.0)**((IA - ID)/2)
     &                                      /SQRT(fb)
                                       ELSE
                                         fc2 = IC + 1
                                         iabcp = (IA + IB + IC)/2 + 1
                                         iabc = iabcp - IC
                                         icab = iabcp - IB
                                         ibca = iabcp - IA
                                         iapd = (IA + ID)/2 + 1
                                         iamd = iapd - ID
                                         ibpe = (IB + IE)/2 + 1
                                         ibme = ibpe - IE
                                         icpf = (IC + IG)/2 + 1
                                         icmf = icpf - IG
                                         sqfclg = 0.5*(LOG(fc2)
     &                                      - FAClog(iabcp + 1)
     &                                      + FAClog(iabc)
     &                                      + FAClog(icab)
     &                                      + FAClog(ibca)
     &                                      + FAClog(iapd)
     &                                      + FAClog(iamd)
     &                                      + FAClog(ibpe)
     &                                      + FAClog(ibme)
     &                                      + FAClog(icpf)
     &                                      + FAClog(icmf))
                                         nzmic2 = (IB - IC - ID)/2
                                         nzmic3 = (IA - IC + IE)/2
                                         nzmi = MAX0(0, nzmic2, nzmic3)
     &                                      + 1
                                         nzmx = MIN0(iabc, iamd, ibpe)
                                         s1 = ( - 1.0)**(nzmi - 1)
                                         DO nz = nzmi, nzmx
                                         nzm1 = nz - 1
                                         nzt1 = iabc - nzm1
                                         nzt2 = iamd - nzm1
                                         nzt3 = ibpe - nzm1
                                         nzt4 = nz - nzmic2
                                         nzt5 = nz - nzmic3
                                         termlg = sqfclg - FAClog(nz)
     &                                      - FAClog(nzt1)
     &                                      - FAClog(nzt2)
     &                                      - FAClog(nzt3)
     &                                      - FAClog(nzt4)
     &                                      - FAClog(nzt5)
                                         ssterm = s1*EXP(termlg)
                                         RAC = RAC + ssterm
                                         s1 = -s1
                                         ENDDO
                                       ENDIF
                                    ENDIF
                                 ENDIF
                              ENDIF
                           ENDIF
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      END
C
C
C***********************************************************************
      SUBROUTINE CLEBRD(Jatw, Jbtw, J12mxt, Kasem2, Jtl)
C***********************************************************************
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C
C COMMON variables
C
      DOUBLE PRECISION CLEbmm(250)
      COMMON /CLEBAB/ CLEbmm
C
C Dummy arguments
C
      INTEGER J12mxt, Jatw, Jbtw, Jtl, Kasem2
C
C Local variables
C
      INTEGER IABS, MIN0
      INTEGER ji, jx, n
C
C
      ji = IABS(Jatw - Jbtw)
      jx = MIN0(Jatw + Jbtw, J12mxt)
      IF(ji.LE.jx)THEN
         Jtl = ((jx + ji + 6 - 2*Kasem2)*(jx - ji + 2))/4
         READ(8)(CLEbmm(n), n = 1, Jtl)
      ENDIF
      END
C
C
C***********************************************************************
      SUBROUTINE CLEBZ
C***********************************************************************
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C
C COMMON variables
C
      DOUBLE PRECISION FAClog(500), RAC, U9
      INTEGER IA, IB, IC, ID, IE, IG, L9(9)
      COMMON /RACFAC/ FAClog, RAC, U9
      COMMON /RACFACI/ IA, IB, IC, ID, IE, IG, L9
C
C Local variables
C
      REAL FLOAT
      INTEGER iabc, iabmc, iahf, ibcma, ibhf, icamb, ichf, igma, igmb, 
     &        igmc, igtw, k1, k2, k3
      INTEGER IABS, MIN0
      DOUBLE PRECISION r1, s1
C
C
      RAC = 0.0
      igtw = (IA + IB + IC)/2
      IF(MOD(igtw, 2).EQ.0)THEN
         k1 = IA + IB - IC
         k2 = IC - IABS(IB - IA)
         k3 = MIN0(k1, k2)
         IF(k3.GE.0)THEN
            IG = igtw/2
            iahf = IA/2
            ibhf = IB/2
            ichf = IC/2
            s1 = 1 - 2*MOD(IG + ichf, 2)
            iabc = igtw + 2
            iabmc = iahf + ibhf - ichf + 1
            icamb = ichf + iahf - ibhf + 1
            ibcma = ibhf + ichf - iahf + 1
            igma = IG - iahf + 1
            igmb = IG - ibhf + 1
            igmc = IG - ichf + 1
            r1 = 0.5*(FAClog(iabmc) + FAClog(icamb) + FAClog(ibcma)
     &           - FAClog(iabc)) + FAClog(IG + 1) - FAClog(igma)
     &           - FAClog(igmb) - FAClog(igmc)
            RAC = s1*SQRT(FLOAT(IC + 1))*EXP(r1)
         ENDIF
      ENDIF
      END
C
C
C***********************************************************************
      SUBROUTINE RACHLF
C***********************************************************************
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C
C COMMON variables
C
      DOUBLE PRECISION FAClog(500), RAC, U9
      INTEGER IA, IB, IC, ID, IE, IG, L9(9)
      COMMON /RACFAC/ FAClog, RAC, U9
      COMMON /RACFACI/ IA, IB, IC, ID, IE, IG, L9
C
C Local variables
C
      DOUBLE PRECISION fd, fn, s1
C
C
      s1 = 1 - 2*MOD((IB + ID - IG)/2, 2)
      IF(IA.GE.IB)THEN
         IF(IC.GE.ID)THEN
            fd = (IB + 1)*(IB + 2)*(ID + 1)*(ID + 2)
            fn = (IB + ID + IG + 4)*(IB + ID - IG + 2)/4
            GOTO 100
         ENDIF
         fd = (IB + 1)*(IB + 2)*ID*(ID + 1)
         fn = (IG + IB - ID + 2)*(IG - IB + ID)/4
         GOTO 100
      ENDIF
      IF(IC.GE.ID)THEN
         fd = IB*(IB + 1)*(ID + 1)*(ID + 2)
         fn = (IG - IB + ID + 2)*(IG + IB - ID)/4
      ELSE
         fd = IB*(IB + 1)*ID*(ID + 1)
         fn = (IB + ID + IG + 2)*(IB + ID - IG)/4
         s1 = -s1
      ENDIF
 100  RAC = s1*SQRT(fn/fd)
      END
C
C
C***********************************************************************
      SUBROUTINE RACSIM(Racie)
C***********************************************************************
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C
C COMMON variables
C
      DOUBLE PRECISION FAClog(500), RAC, U9
      INTEGER IA, IB, IC, ID, IE, IG, L9(9)
      COMMON /RACFAC/ FAClog, RAC, U9
      COMMON /RACFACI/ IA, IB, IC, ID, IE, IG, L9
C
C Dummy arguments
C
      DOUBLE PRECISION Racie(50)
C
C Local variables
C
      DOUBLE PRECISION dfccor(50), f1, f2, f3, f4, f5, f6, f7, f8, 
     &                 flabcd, fsq, sum, t1fc(50), t1log, t2fc(50), 
     &                 t2log
      INTEGER i1, i2, i3, i4, iabcd1, iabe, iabep, iabtw, iacf, iacfp, 
     &        iadftw, ibcftw, ibdf, ibdfp, ibea, icde, icdep, icdtw, 
     &        icfa, idec, idfb, ieab, iecd, ieloc, iemax, iemin, ier, 
     &        ierang, ifac, ifbd, nez, nezitw, nezr, nezrag, nezxtw, nz, 
     &        nzmax, nzmin, nzp1x2
      INTEGER IABS, MAX0, MIN0
C
C
      iemin = MAX0(IABS(IA - IB), IABS(IC - ID))
      iemax = MIN0(IA + IB, IC + ID)
      ierang = (iemax - iemin)/2 + 1
      dfccor(1) = 1.
      IE = iemin
      IF(ierang.NE.1)THEN
         f1 = (IE + IA - IB + 2)/2
         f2 = (IE - IA + IB + 2)/2
         f3 = (IA + IB + IE + 4)/2
         f4 = (IA + IB - IE)/2
         f5 = (IE + IC - ID + 2)/2
         f6 = (IE - IC + ID + 2)/2
         f7 = (IC + ID + IE + 4)/2
         f8 = (IC + ID - IE)/2
         DO ier = 2, ierang
            fsq = f1*f2*f5*f6/(f3*f4*f7*f8)
            dfccor(ier) = SQRT(fsq)*dfccor(ier - 1)
            f1 = f1 + 1.
            f2 = f2 + 1.
            f3 = f3 + 1.
            f4 = f4 - 1.
            f5 = f5 + 1.
            f6 = f6 + 1.
            f7 = f7 + 1.
            f8 = f8 - 1.
         ENDDO
      ENDIF
      iabep = (IA + IB + IE)/2 + 1
      icdep = (IC + ID + IE)/2 + 1
      iabe = iabep - IE
      ieab = iabep - IB
      ibea = iabep - IA
      icde = icdep - IE
      iecd = icdep - ID
      idec = icdep - IC
      iabcd1 = (IA + IB + IC + ID + 4)/2
      flabcd = 0.5*FAClog(iabcd1)
      iadftw = IA + ID - IG
      ibcftw = IB + IC - IG
      iabtw = IA + IB
      icdtw = IC + ID
      nezitw = MAX0(iadftw, ibcftw)
      nezxtw = MIN0(iabtw, icdtw)
      i1 = (iabtw - nezitw)/2 + 1
      i2 = (icdtw - nezitw)/2 + 1
      i3 = (nezitw - iadftw)/2 + 1
      i4 = (nezitw - ibcftw)/2 + 1
      t1log = 0.5*(FAClog(iabe) + FAClog(ieab) + FAClog(ibea)
     &        + FAClog(icde) + FAClog(iecd) + FAClog(idec)
     &        - FAClog(iabep + 1) - FAClog(icdep + 1)) + flabcd - 
     &        (FAClog(i1) + FAClog(i2) + FAClog(i3) + FAClog(i4))
      t1fc(1) = EXP(t1log)
      nezrag = (nezxtw - nezitw)/2 + 1
      f1 = i1
      f2 = i2
      f3 = i3 - 1
      f4 = i4 - 1
      DO nezr = 2, nezrag
         f1 = f1 - 1.
         f2 = f2 - 1.
         f3 = f3 + 1.
         f4 = f4 + 1.
         t1fc(nezr) = (f1*f2/(f3*f4))*t1fc(nezr - 1)
      ENDDO
      iacfp = (IA + IC + IG)/2 + 1
      ibdfp = (IB + ID + IG)/2 + 1
      iacf = iacfp - IG
      ifac = iacfp - IC
      icfa = iacfp - IA
      ibdf = ibdfp - IG
      ifbd = ibdfp - ID
      idfb = ibdfp - IB
      t2log = 0.5*(FAClog(ifac) + FAClog(icfa) + FAClog(ifbd)
     &        + FAClog(idfb) - FAClog(iacf) - FAClog(ibdf)
     &        - FAClog(iacfp + 1) - FAClog(ibdfp + 1)) + flabcd
      t2fc(1) = EXP(t2log)
      nzp1x2 = MIN0(iacf, ibdf)
      IF(nzp1x2.NE.1)THEN
         f1 = iacf
         f2 = ibdf
         f3 = 0.
         f4 = iabcd1
         DO nz = 2, nzp1x2
            f1 = f1 - 1.
            f2 = f2 - 1.
            f3 = f3 + 1.
            f4 = f4 - 1.
            t2fc(nz) = -(f1*f2/(f3*f4))*t2fc(nz - 1)
         ENDDO
      ENDIF
      IE = iemin - 2
      DO ier = 1, ierang
         IE = IE + 2
         nzmin = MAX0(0, iadftw - IE, ibcftw - IE)/2 + 1
         nzmax = MIN0((iabtw - IE)/2, (icdtw - IE)/2, nzp1x2 - 1) + 1
         ieloc = (IE - nezitw)/2
         sum = 0.
         DO nz = nzmin, nzmax
            nez = nz + ieloc
            sum = sum + t1fc(nez)*t2fc(nz)
         ENDDO
         Racie(ier) = dfccor(ier)*sum
      ENDDO
      END
C
C
C***********************************************************************
      SUBROUTINE CLEBHF
C***********************************************************************
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C
C COMMON variables
C
      DOUBLE PRECISION FAClog(500), RAC, U9
      INTEGER IA, IB, IC, ID, IE, IG, L9(9)
      COMMON /RACFAC/ FAClog, RAC, U9
      COMMON /RACFACI/ IA, IB, IC, ID, IE, IG, L9
C
C Local variables
C
      REAL FLOAT
      DOUBLE PRECISION fn
      INTEGER kase
C
C
      kase = (1 - IC + IA) + (3 - IE)/2
      IF(kase.EQ.1 .OR. kase.EQ.4)fn = (IA + IG + 1)/2
      IF(kase.EQ.2 .OR. kase.EQ.3)fn = (IA - IG + 1)/2
      RAC = SQRT(fn/FLOAT(IA + 1))
      IF(kase.EQ.3)RAC = -RAC
      END
