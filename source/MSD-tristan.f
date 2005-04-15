Ccc
Ccc   * $Date: 2005-04-15 18:21:02 $
Ccc   * $Id: MSD-tristan.f,v 1.30 2005-04-15 18:21:02 Capote Exp $
C
      SUBROUTINE TRISTAN(Nejc,Nnuc,L1maxm,Qm,Qs)
CCC
CCC   ******************************************************************
CCC   *                                                       class:fpu*
CCC   *                       T R I S T A N                            *
CCC   *                                                                *
CCC   * Calculates two step multi-step direct cross sections using     *
Ccc   * the results of  ORION  stored  on TAPE15 and QRPA response     *
Ccc   * functions which take into account vibrational collectivity.    *
Ccc   *                                                                *
Ccc   *                                                                *
Ccc   * input:NEJC   - particle index (actually 0 i.e. projectile)     *
CCC   *       NNUC   - target nucleus index (actually 0)               *
CCC   *       L1MAXM - maximum l transfer                              *
CCC   *       QM     - maximum energy loss                             *
CCC   *       QS     - step in energy loss triangle                    *
CCC   *                                                                *
CCC   *       TAPE15 - ORION results                                   *
CCC   *                                                                *
CCC   *                                                                *
CCC   * output:none                                                    *
CCC   *                                                                *
CCC   *                                                                *
CCC   *                                                                *
CCC   * author: H.Lenske                                               *
CCC   *                                                                *
CCC   ******************************************************************
CCC
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C COMMON variables
C
      DOUBLE PRECISION AI, ALSin, ANGle(NDANG), AR, CLRn(11), CNOrin(22)
     &                 , CROs1(30,49,2*NDANG), CROs2(30,49,2*NDANG), 
     &                 DTHeta, ECEntr(5), EFItin(22), EOUtmi, EOUtmx, 
     &                 ESTep, ETMax, ETMin, EXTcom(10), FACb, 
     &                 FAClog(500), FFAc1d(2), FFAc2d(2), FFAc3d(2), 
     &                 FFTot(10), GAPin(2), HOMin, Q0, QGRand, QMAx, 
     &                 QMIna, QMInb, QS1, QS2, QSTep, RAC, 
     &                 RHOb(301,11,2), RI, ROPt, RR, THEta1, THEta2, U0,
     &                 U9, W0, WIDex, WIDexin, WR1(12*NDANG), 
     &                 WR2(144*NDANG)
      INTEGER IA, IB, IC12x, IC1max, IC1mxr, IC1x, IC2max, IC2mxr, IC2x,
     &        ICC, ICMax, ID, IE, IG, KDEnvi, KEX3, KEXcom(10), KRTmax, 
     &        KRType, KTRl(10), L9(10), NAVerg, NCHanl, NEBinx, NFAc12, 
     &        NN, NQ1x, NQ2x, NRMax(10), NTHeta, NZ
      COMMON  CROs1, CROs2, WR1, WR2
      COMMON /CC    / THEta1, THEta2, NCHanl, NN, NZ, IC1mxr, IC2mxr, 
     &                KRType, KRTmax, KDEnvi, NTHeta, NEBinx, NAVerg, 
     &                NFAc12, KEX3
      COMMON /CCLQ  / IC1max, IC2max, IC1x, IC2x, IC12x, ICMax, NQ1x, 
     &                NQ2x
      COMMON /CONTRL/ EXTcom, KTRl, KEXcom
      COMMON /CRAC  / FAClog, RAC, U9, IA, IB, ICC, ID, IE, IG, L9
      COMMON /INELA / WIDex, ROPt, CLRn, ETMin, ETMax, RHOb, QS1, QS2, 
     &                Q0, FACb, FFTot, NRMax
      COMMON /TRINP / WIDexin, GAPin, HOMin, ALSin, EFItin, CNOrin
      COMMON /TRINTP/ DTHeta, ANGle, ESTep, EOUtmi, EOUtmx, ECEntr, 
     &                QSTep, QMIna, QMInb, QMAx, QGRand, FFAc1d, FFAc2d,
     &                FFAc3d
      COMMON /U_OPT / U0, W0, RR, RI, AR, AI
C
C Dummy arguments
C
      INTEGER L1maxm, Nejc, Nnuc
      DOUBLE PRECISION Qm, Qs
C
C Local variables
C
      DOUBLE PRECISION a3, ap, at, crose(NDEX,NDANG,NDANG), eccm, elab, 
     &                 fn, q1, q2, ri0, rr0
      REAL FLOAT
      INTEGER i1, i2, ic, ic1, ic12, ic1xr, ic2, ic2xr, icp, icpx, 
     &        iout2, ka, kb, kread, l1, l12x, l1maxr, l2, l2maxm, 
     &        l2maxr, mtb, n, n1, n12, n1x, n2, na, nangle, nb, ne, nlr,
     &        nnb, np, npb, nq, nq12x, nwr1, nwr2, nzb
      INTEGER INT
C
C-----L1MAXM - maximum l transfer
C-----QM - maximum energy loss
C-----QS - step in energy loss triangle
C
C
C
      QMAx = Qm
      QSTep = Qs
      OPEN (16,FILE = 'TAPE16',STATUS = 'UNKNOWN',FORM = 'UNFORMATTED')
C     OPEN(15, FILE='TAPE15', STATUS='OLD')
      FAClog(1) = 0.0D0
      FAClog(2) = 0.0D0
      fn = 1.D0
      DO n = 3, 500
         fn = fn + 1.D0
         FAClog(n) = FAClog(n - 1) + LOG(fn)
      ENDDO
      NCHanl = 3
      KRType = 1
      NFAc12 = 1
      NAVerg = 1
      nangle = NDANG
      NZ = INT(Z(0))
      NN = INT(XN(0))
      np = INT(AEJc(0))
      nzb = INT(Z(0))
      nnb = INT(XN(0))
      npb = INT(AEJc(0))
      l2maxm = L1maxm
      l1maxr = L1maxm
      l2maxr = L1maxm
      KTRl(1) = 2
      KTRl(2) = 0
      KTRl(3) = 0
      KTRl(4) = 1
      KTRl(5) = 0
      KTRl(6) = 0
      KTRl(7) = 0
      KTRl(8) = -1
      KEXcom(1) = 4
      KEXcom(2) = 4
      KEXcom(3) = 10
      KEXcom(4) = 0
      icpx = KTRl(1)
      iout2 = KTRl(2)
      NQ1x = KEXcom(1)
      NQ2x = KEXcom(2)
      nq12x = KEXcom(3)
      KEX3 = KEXcom(4) + 1
      IC1max = L1maxm + 1
      IC2max = l2maxm + 1
      IC1mxr = l1maxr + 1
      IC2mxr = l2maxr + 1
      ICMax = MAX(IC1max,IC2max)
      DO na = 1, nangle
         ANGle(na) = ANGles(na)
      ENDDO
      elab = EIN*(A(Nnuc) + AEJc(Nejc))/A(Nnuc)
C-----energy step in spectra
      ESTep = DE
C-----experimental energy resolution
      WIDex = WIDexin
      QMIna = 0.
      QMInb = 0.
      QMAx = -QMAx
      QGRand = 0.
      IF (THEta1.LE.0.D0) THEta1 = ANGle(1)
      IF (THEta2.LE.0.D0) THEta2 = ANGle(nangle - 1)
      KRTmax = MIN(KRType,2)
      DO n = 1, NFAc12
         FFAc1d(n) = 1.
         FFAc2d(n) = 1.
         FFAc3d(n) = 1.
      ENDDO
C
C     parameters of optical potential (entrance channel)
C
      rr0 = 1.25
      ri0 = 1.37
      at = NN + NZ
      a3 = at**(1./3.)
      ap = np
      eccm = elab/(1.0 + ap/at)
      Q0 = SQRT(eccm*(at + ap)*2.*939./(at*ap))/197.33
      ETMin = 0.
      ETMax = ( - QMAx) + 25.*ESTep
      IF (ETMax.GT.eccm) ETMax = eccm - ESTep
      NEBinx = (ETMax - ETMin)/ESTep + 1.2
      EOUtmx = ESTep*FLOAT(NEBinx)
      EOUtmi = EOUtmx - ESTep*FLOAT(NEBinx)
      WRITE (6,99020)
99020 FORMAT ('1'//11X,10('*'),6X,
     &        'MSDR CALCULATION OF CONTINUOUS SPECTRA',6X,10('*')//34X,
     &        '(ON PROGRAM TRISTAN )'/34X,'   (V2.0, OCT.94)'//)
      mtb = nnb + nzb
      FACb = (1878./197.33**2)*FLOAT(mtb + npb)/FLOAT(npb*mtb)
      RR = a3*rr0
      RI = a3*ri0
      IF (KTRl(3).NE.1) THEN
         nwr1 = (l1maxr/KEX3 + 1)*nangle
         nwr2 = (l2maxr/KEX3 + 1)*nwr1
         IF (nwr1.GT.12*NDANG) THEN
            WRITE (6,*) ' INSUFFICIENT DIMENSION OF THE WR1 ARRAY IN'
            WRITE (6,*) ' TRISTAN. MUST BE ', nwr1, ' AT LEAST'
            IF (nwr2.GT.144*NDANG) THEN
               WRITE (6,*) ' '
               WRITE (6,*) ' INSUFFICIENT DIMENSION OF THE WR2 ARRAY IN'
               WRITE (6,*) ' TRISTAN. MUST BE ', nwr2, ' AT LEAST'
            ENDIF
            STOP 'ERROR IN WR1 WR2 DIMENSIONS IN TRISTAN'
         ENDIF
CCCCCC
         REWIND 15
         REWIND 16
CCCCCC
         IC1x = L1maxm/KEX3 + 1
         IC2x = l2maxm/KEX3 + 1
         ic1xr = l1maxr/KEX3 + 1
         ic2xr = l2maxr/KEX3 + 1
         IC12x = IC1x*IC2x
         l12x = IC1max*IC2max
         kread = 0
CCCCCC
C        readin cross sections from TAPE15
CCCCCC
   50    DO ne = 1, nq12x
CCCCCC
CCCCC       NLR=1,2 refers to cross sections, analyzing powers resp.
CCCCCC
            DO nlr = 1, icpx
               IF (kread.NE.1) THEN
                  READ (15,*) (WR1(n),n = 1,nwr1)
C                 modiffication to run formated orion output
C                 READ(15,3434)(WR1(N),N=1,NWR1)
                  IF (ne.LE.NQ1x) THEN
                     n1 = 0
                     DO ic = 1, IC1x
                        DO na = 1, nangle
                           nb = na + (nlr - 1)*nangle
                           n1 = n1 + 1
                           CROs1(ne,ic,nb) = WR1(n1)
                        ENDDO
                     ENDDO
                  ENDIF
               ENDIF
               IF (NCHanl.NE.2) THEN
                  IF (kread.EQ.0) READ (15,*) (WR2(n),n = 1,nwr2)
C                 modiffication to run formated orion output
C                 IF(KREAD.EQ.0)READ(15,3434)(WR2(N),N=1,NWR2)
C                 IF(kread.EQ.1)READ(16)(WR2(n), n = 1, nwr2)
                  n2 = 0
                  DO ic1 = 1, ic1xr
                     IF (ic1.GT.IC1x) GOTO 60
                     DO ic2 = 1, ic2xr
                        ic12 = (ic1 - 1)*IC2x + ic2
                        DO na = 1, nangle
                           nb = na + (nlr - 1)*nangle
                           n2 = n2 + 1
                           IF (ic2.LE.IC2x) CROs2(ne,ic12,nb) = WR2(n2)
                        ENDDO
                     ENDDO
                  ENDDO
               ENDIF
   60       ENDDO
         ENDDO
C
C------print input cross sections and polarizations
C
         IF (iout2.NE.0) THEN
            IF (kread.EQ.0) THEN
               DO icp = 1, icpx
                  ka = nangle*(icp - 1) + 1
                  kb = ka - 1 + nangle
                  IF (icp.EQ.1) WRITE (6,99030) (ANGle(n),n = 1,nangle)
99030             FORMAT (//20X,'1-STEP INPUT LEFT CROSS SECTIONS'//' ',
     &                    ' L1',2X,'Q1',2X,9F12.2)
                  IF (icp.EQ.2) WRITE (6,99035) (ANGle(n),n = 1,nangle)
99035             FORMAT (//20X,'1-STEP INPUT RIGHT CROSS SECTIONS'//
     &                    ' ',' L1',2X,'Q1',2X,9F12.2)
                  DO i1 = 1, IC1x
                     l1 = KEX3*(i1 - 1)
                     q1 = QMAx - QSTep
                     DO nq = 1, NQ1x
                        q1 = q1 + QSTep
                        WRITE (6,99040) l1, q1, 
     &                                  (CROs1(nq,i1,na),na = ka,kb)
99040                   FORMAT (' ',I3,F6.2,9E12.5)
                     ENDDO
                  ENDDO
               ENDDO
               IF (NCHanl.EQ.2) GOTO 100
            ENDIF
            DO icp = 1, icpx
               WRITE (6,99065)
               ka = nangle*(icp - 1) + 1
               kb = ka - 1 + nangle
               IF (icp.EQ.1) WRITE (6,99045) (ANGle(n),n = 1,nangle)
99045          FORMAT (//20X,'2-STEP INPUT LEFT CROSS SECTIONS'//' ',
     &                 ' L1 L2  Q1',4X,'Q2  ',9F12.2)
               IF (icp.EQ.2) WRITE (6,99050) (ANGle(n),n = 1,nangle)
99050          FORMAT (//20X,'2-STEP INPUT RIGHT CROSS SECTIONS'//' ',
     &                 ' L1 L2  Q1',4X,'Q2  ',9F12.2)
               ic = 0
               DO i1 = 1, IC1x
                  l1 = KEX3*(i1 - 1)
                  DO i2 = 1, IC2x
                     l2 = KEX3*(i2 - 1)
                     ic = ic + 1
                     q2 = QMAx - QSTep
                     nq = 0
                     WRITE (6,99055)
99055                FORMAT (/)
                     DO n2 = 1, NQ2x
                        q2 = q2 + QSTep
                        n1x = NQ1x - n2 + 1
                        q1 = q2 - QSTep
                        DO n1 = 1, n1x
                           q1 = q1 + QSTep
                           nq = nq + 1
                           WRITE (6,99060) l1, l2, q1, q2, 
     &                            (CROs2(nq,ic,n),n = ka,kb)
99060                      FORMAT (' ',2I3,2F6.1,9E12.5)
                        ENDDO
                     ENDDO
                  ENDDO
               ENDDO
            ENDDO
            WRITE (6,99065)
         ENDIF
  100    DO na = 1, nangle
            IF (kread.NE.1) THEN
               DO n1 = 1, NQ1x
                  DO icp = 1, icpx
                     nb = na + (icp - 1)*nangle
                     WRITE (16) (CROs1(n1,ic1,nb),ic1 = 1,IC1x)
                  ENDDO
               ENDDO
            ENDIF
            DO n12 = 1, nq12x
               DO icp = 1, icpx
                  nb = na + (icp - 1)*nangle
                  WRITE (16) (CROs2(n12,ic12,nb),ic12 = 1,IC12x)
               ENDDO
            ENDDO
         ENDDO
         IF (KTRl(7).NE.0) THEN
            IF (kread.NE.1) THEN
               kread = 1
               GOTO 50
            ENDIF
         ENDIF
      ENDIF
      CALL RESPNS
      IF (KTRl(3).EQ.0) CALL SPECTR(nangle,2,nangle,nq12x,NEBinx,l12x,
     &                              CROs2,crose,Nejc)
      CLOSE (16)
99065 FORMAT ('1')
      END
C
C
      SUBROUTINE INELAS(Est,Nn,Nz,Nebins)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C COMMON variables
C
      DOUBLE PRECISION ALSin, ANGle(NDANG), AU, AW, BETa(301,11), BST(3)
     &                 , BST1(2), CLRn(11), CNOrin(22), DTHeta, 
     &                 EBCs(500,2), ECEntr(5), EFItin(22), EOUtmi, 
     &                 EOUtmx, ESP(500,2), ESTep, ETMax, ETMin, FACb, 
     &                 FAClog(500), FFAc1d(2), FFAc2d(2), FFAc3d(2), 
     &                 FFTot(10), GAP(2), GAPin(2), GSDm(50), HOMega, 
     &                 HOMin, Q0, QGRand, QMAx, QMIna, QMInb, QS1, QS2, 
     &                 QSTep, RAC, RHO(301,11), RHOb(301,11,2), RI, 
     &                 RMS(3), ROPt, RR, SREw(21), SREwl(21), SRNew(21),
     &                 U0, U9, UAMp(500,2), VAMp(500,2), VLS(2), W0, 
     &                 WIDex, WIDexin
      INTEGER IA, IB, IC, IC12x, IC1max, IC1x, IC2max, IC2x, ICMax, ID, 
     &        IE, IG, JSP(500,2), L9(10), LSP(500,2), NHOle(2), 
     &        NLEv(301,11), NQ1x, NQ2x, NRMax(10), NSP(500,2), NTOtal(2)
      COMMON  RHO, SRNew, SREw, SREwl, GSDm, BETa, NLEv
      COMMON /CCLQ  / IC1max, IC2max, IC1x, IC2x, IC12x, ICMax, NQ1x, 
     &                NQ2x
      COMMON /CRAC  / FAClog, RAC, U9, IA, IB, IC, ID, IE, IG, L9
      COMMON /EVBCS / EBCs, VAMp, UAMp
      COMMON /INELA / WIDex, ROPt, CLRn, ETMin, ETMax, RHOb, QS1, QS2, 
     &                Q0, FACb, FFTot, NRMax
      COMMON /SPPA  / HOMega, VLS, RMS, BST, GAP, NHOle, NTOtal
      COMMON /SPQN  / ESP, NSP, LSP, JSP
      COMMON /TRINP / WIDexin, GAPin, HOMin, ALSin, EFItin, CNOrin
      COMMON /TRINTP/ DTHeta, ANGle, ESTep, EOUtmi, EOUtmx, ECEntr, 
     &                QSTep, QMIna, QMInb, QMAx, QGRand, FFAc1d, FFAc2d,
     &                FFAc3d
      COMMON /U_OPT / U0, W0, RR, RI, AU, AW
C
C Dummy arguments
C
      INTEGER Nebins, Nn, Nz
      DOUBLE PRECISION Est(0:3*NDEX)
C
C Local variables
C
      DOUBLE PRECISION a1, a3, ad, aew, anp(2,2), anz, api, aqq, bosc, 
     &                 bqq, cci(11), ccm(11), ccp(11), ccpm(2), ccr(11),
     &                 ceff, clex(11), clsc(11), cneg, cnorm, cpos, 
     &                 cr1(11), cr2, cr3(11), dci(11), dcr(11), ddr(2), 
     &                 de3, deqq, dnz, dqqst(5000), dr, dwex, dwsx, e, 
     &                 e0, efit(22), efitx, egr, em, emi, emisq, ep, 
     &                 epl, eplsq, eqq, eqqst(5000), eqqx, ess(0:10000),
     &                 est3, ext, f1, fe, ff1, fltwp1, fourpi, fpi, 
     &                 greenr, greenx, greeny, hat, hcorr, homeb, phtrm,
     &                 pxmd, pymd, qqi, qqr, qqx, qqy, r, r1, rd, rdopt,
     &                 rdsq, re1, re2, re3, reduqq, resid
      REAL d1, hhh
      DOUBLE PRECISION DWIDTH
      REAL FLOAT
      INTEGER i, j, jtw1, jtw2, k, kc, kcx, kh, kmax, kp, kqq, krt, 
     &        krtx, l, l1, l2, lm, lmax, lst, lt, lth, ltmax, ltmaxr, 
     &        ltmin, ltp, ltp1, ltr, lttw, n, nconf(21), ne, nebinx, 
     &        nesx, nh, nlhm, nlpm, nos1, nos2, np, np1, nr, nxmax
      INTEGER IABS, INT
      DOUBLE PRECISION rfqqr(0:501,11), rfqqx(0:501,11), rfqqy(0:501,11)
     &                 , rh0, rho1, rl, rmax, rmosc, rmsgs, rnorm, 
     &                 rnp(3,2), rp, rqr, rrr(2), rws, rwsq, t1, t2, 
     &                 umatqq, veff, vnorm, w, wbcs, we, wgr, widas, 
     &                 wide(0:10000), widea, widgr, wqa, wqq, 
     &                 wqqst(5000), wqrex, x, xea(11), xir, xneg, xp, 
     &                 xpos, xqq, yea(11), yqq
      EQUIVALENCE (BST(1),BST1)
      DATA rnp/1.2490D0, -0.5401D0, -0.9582D0, 1.2131D0, -0.4415D0, 
     &     0.8931D0/
      DATA anp/0.4899D0, -0.1236D0, 0.4686D0, 0.0741D0/
      DATA eqqx/80.0D0/
C     IF(NEBINX.GT.301)THEN
C     WRITE(6,*)' ------>> INCREASE DIMENSIONS FOR RESPONSE FUNCTIONS
C     & ETC. TO: ',NEBINX
C     STOP
C     ENDIF
C     WRITE(17,*)NEBINX,ICMAX
      fpi = 4.*PI
      fourpi = 1.0/fpi
C-----selfconsistent strength taken for the l=0 transfer field
      efit(1) = 0.0
      IF (EFItin(1).GT.0.0D0) efit(1) = EFItin(1)
      IF (EFItin(1).LT.0.0D0) efit(1) = 0.0
C-----energy of the first collective -1 state (GDR)
C-----GDR mean position calculated as a weighted average of the two hump GDR
      efit(2) = (GDRpar(1,1)*GDRpar(2,1)*GDRpar(3,1) + GDRpar(4,1)
     &          *GDRpar(5,1)*GDRpar(6,1))
     &          /(GDRpar(2,1)*GDRpar(3,1) + GDRpar(5,1)*GDRpar(6,1))
      IF (EFItin(2).GT.0.0D0) efit(2) = EFItin(2)
      IF (EFItin(2).LT.0.0D0) efit(2) = 0.0
C-----energy of the first collective +2 state
      efit(3) = -QCC(1)
      IF (EFItin(3).GT.0.0D0) efit(3) = EFItin(3)
      IF (EFItin(3).LT.0.0D0) efit(3) = 0.0
C-----energy of the first collective -3 state
      efit(4) = -QCC(2)
      IF (EFItin(4).GT.0.0D0) efit(4) = EFItin(4)
      IF (EFItin(4).LT.0.0D0) efit(4) = 0.0
C-----energy of the first collective +4 state
      efit(5) = 0.0
      IF (EFItin(5).GT.0.0D0) efit(5) = EFItin(5)
      IF (EFItin(5).LT.0.0D0) efit(5) = 0.0
C-----energies of the remaining levels (if any)
      DO l = 6, 22
         IF (EFItin(l).GT.0.0D0) efit(l) = EFItin(l)
         IF (EFItin(l).LE.0.0D0) efit(l) = 0.0
      ENDDO
C
C-----check that energy interval is compatible with range of
C-----experimental ex used to determine coupl. const.
C
      efitx = -10.
      DO l = 1, ICMax
         efitx = MAX(efit(l),efitx)
      ENDDO
      nebinx = MAX(INT((efitx+5.*WIDex)/ESTep + 1.),Nebins)
      IF (nebinx.NE.Nebins .AND. IOUt.GT.3) WRITE (6,99005) nebinx*ESTep
99005 FORMAT (/' >> LIMIT OF EX IS INCREASED TO:',F9.3,' (MEV) <<'/
     &        '    (USED TO DETERMINE THE COUPLING CONSTANTS)')
      ltmaxr = ICMax
      lmax = ICMax*2 + 2
      est3 = ESTep/3.
      a1 = Nn + Nz
      rd = 0.
      ad = 0.
      anz = Nz
      dnz = FLOAT(Nn - Nz)/a1
      a3 = a1**(1./3.)
C-----set xea and yea to 0 to avoid undefined if efit's are 0
      DO lt = 1, ltmaxr
         xea(lt) = 0.D0
         yea(lt) = 0.D0
      ENDDO
      DO i = 1, 2
         rd = rd + anz*(a3*rnp(1,i) + rnp(2,i) + rnp(3,i)*dnz)
         ad = ad + anz*(anp(1,i) + anp(2,i)*dnz)
         anz = Nn
      ENDDO
      rd = rd/a1
      ad = ad/a1
      rdsq = rd*rd
      rh0 = 0.75*a1/(rd**3*PI*(1. + (PI*ad/rd)**2))
      rws = rd
      rwsq = rws*rws
      ROPt = RR
      IF (ROPt.LE.0.0D0) ROPt = rd
      rdopt = rd/ROPt
      IF (IOUt.GT.3) WRITE (6,99010) rd, ad, rh0, ROPt, rdopt
99010 FORMAT (//' ','RDENS=',F7.4,3X,'ADENS=',F7.4,3X,'RH0=',F7.4/' ',
     &        'ROPT =',F7.4,3X,'RATIO=',F7.4)
C
C     >>>>> energy dependent width of RPA/QRPA states <<<<<
C     WIDEX = experimental energy resolutioN
C     (FWHM; a Lorentzian shape is assumed)
C     WGR   = 90/A**(2/3) MeV is taken as width for GR'S
C     (from systematics of experimental GR widths)
C     the energy dependence is paramaterized as
C     WID(E)=WIDEX + WIDAS*(F(E) + F(-E))
C     F(E)  =1./(1+EXP((E-EO)/A))
C     WIDAS = 1.1*WGR
C     EO is determined such that WID(EGR) = 0.9 *WIDAS=WGR
C     EGR is the average GDR-energy  71/A**(1/3)
C     WGR is the average GDR-width   90/A**(2/3)
C     A = AEW with AEW= 1.5 MeV  fixes the rate of increase
C     of WID(E) to the saturation value of WIDEX + WIDAS
C     at E>>EGR
C
      egr = 71.0/a3
      wgr = 90.0/a3**2
      widgr = wgr
      aew = 1.5
      e0 = egr - aew*LOG(9.)
      widas = widgr*1.11
      IF (IOUt.GT.2) WRITE (6,99015) egr, wgr, widas, WIDex, e0, aew
99015 FORMAT (/'  ENERGY DEPENDENT WIDTH OF RPA/QRPA STATES:'/
     &        '  E(GDR) :',F8.3,'   W(GDR)   :',F8.3,
     &        ' [MEV]'/'  WID(AS):',F8.3,'   WID(EXP.):',F8.3,
     &        ' [MEV]'/'  E0     :',F8.3,'   AEW      :',F8.3,' [MEV]')
      e = ETMin - 2.*ESTep
      DO ne = 0, nebinx + 1
         e = e + ESTep
         Est(ne) = e
      ENDDO
C-----6 lines below make a substantial difference for the response function
C-----with L=0. these were absent in the previous version of the code.
      e = -ESTep
      nesx = MIN(10000.D0,2.D0*eqqx/ESTep)
      DO ne = 0, nesx
         e = e + ESTep
         wide(ne) = DWIDTH(e,widas,e0,aew)*0.5D0
         ess(ne) = e
      ENDDO
C
C-----calculate the moments of the g.s. density distribution
C-----(to be used for self-consistent RPA coupling constants)
C
      api = fpi/a1
      dr = 0.2
C
C-----fix rmax such that radial integrals are accurate to 1.e-04
C
      rp = rd + ad*4.*LOG(10.)
      rmax = rp + ad*(lmax - 1.)*LOG(rp/rd)
      nxmax = rmax/dr
      nxmax = 2*(nxmax/2)
      DO l = 2, lmax
         GSDm(l) = 0.0
      ENDDO
C
C------->> the factor 4*pi/A is excluded - see definition of
C------->> selfconsistent RPA-coupling constants
C
C     (output contains the factor - see below)
C
      r = 0.0
      t1 = dr/3.0
      ff1 = 2.0
      DO nr = 1, nxmax
         r = r + dr
         ff1 = 6.0 - ff1
         f1 = t1*ff1
         IF (nr.EQ.nxmax) f1 = 0.5*t1
         d1 = rh0/(1.0 + EXP((r-rd)/ad))
         rl = r*r
         DO l = 1, lmax
            GSDm(l) = GSDm(l) + rl*f1*d1
            rl = rl*r
         ENDDO
      ENDDO
C
      IF (IOUt.GT.2) WRITE (6,99020)
99020 FORMAT (//8X,' RADIAL MOMENTS <R**L>/A OF THE G.S. DENSITY'/5X,
     &        '           ( UNITS: [FM**L/A] )             '/4X,'L',11X,
     &        'L  ',11X,'L+1',11X,'L+2',11X,'L+3',11X,'L+4')
      lst = 5
      DO l = 1, lmax, lst
         l1 = l
         l2 = MIN(lmax,l1 + lst - 1)
         IF (IOUt.GT.2) WRITE (6,99025) l - 1, (GSDm(n)*api,n = l1,l2)
99025    FORMAT (I5,5E14.7)
      ENDDO
C
C-----adjust oscillator constant to r.m.s. - radius
C
      rmsgs = api*GSDm(3)
      rmosc = RMS(3)/BST(3)**2
      bosc = SQRT(rmsgs/rmosc)
      homeb = (197.33/bosc)**2/938.0
      hcorr = homeb/HOMega
      vnorm = homeb
      IF (IOUt.GT.3) THEN
         WRITE (6,99030) bosc, BST(3), bosc/BST(3), BST1
99030    FORMAT (//'  OSCILLATOR LENGTH:'/'  B(DENSITY):',F7.3,
     &           '  B(NILSSON):',F7.3,'  RATIO:',F7.4/'  B(PROTON) :',
     &           F7.3,'  B(NEUTRON):',F7.3)
         WRITE (6,99035) homeb, HOMega, hcorr
99035    FORMAT (/'  OSCILLATOR ENERGY:'/'  E(DENSITY):',F7.3,
     &           '  E(NILSSON):',F7.3,'  RATIO:',F7.4)
         WRITE (6,99040) (SQRT(RMS(i)),i = 1,3)
99040    FORMAT (/'  <R**2> (IN [FM]) :'/'  PROTON    :',F7.3,
     &           '  NEUTRON   :',F7.3,'  TOTAL:',F7.4)
         WRITE (6,99045) vnorm
99045    FORMAT (/'   STRENGTH OF THE MULTIPOLE-MULTIPOLE INTERACTION:'/
     &           '  VNORM     :',F7.3,' [MEV]')
      ENDIF
      HOMega = homeb
C
C-----compute inelastic       excitations for KRT=1
C-----compute charge exchange excitations for KRT=2
C-----(KRT=2 for (n,p)-reaction step)
C
      krtx = 1
      DO krt = 1, krtx
         DO ne = 0, nebinx + 1
            DO lt = 1, ICMax
               IF (ne.GE.1) RHO(ne,lt) = 0.0
               rfqqr(ne,lt) = 0.0
               rfqqx(ne,lt) = 0.0
               rfqqy(ne,lt) = 0.0
               IF (krt.EQ.1 .AND. ne.GE.1) THEN
                  RHOb(ne,lt,1) = 0.0
                  RHOb(ne,lt,2) = 0.0
               ENDIF
            ENDDO
         ENDDO
C
         DO lt = 1, ICMax
            clsc(lt) = 0.
            clex(lt) = 0.
            SREw(lt) = 0.0
            SRNew(lt) = 0.0
            SREwl(lt) = 0.0
            nconf(lt) = 0.0
            ccr(lt) = 0.D0                                 
            cci(lt) = 0.D0                                 
            dcr(lt) = 0.D0                                 
            dci(lt) = 0.D0                                 
            cr1(lt) = 0.D0                                 
            cr3(lt) = 0.D0                                 
            ccm(lt) = 0.D0                                 
            ccp(lt) = 0.D0                                 
         ENDDO
         IF (krt.EQ.1) kmax = 2
         IF (krt.EQ.2) kmax = 1
C
C--------KRT=1 inelastic excitation
C--------KRT=2 charge exchange excitation for (n,p) reaction
C
         DO k = 1, kmax
            IF (krt.EQ.1) THEN
               nlhm = NHOle(k)
               nlpm = NTOtal(k)
               kh = k
               kp = k
            ELSE
               nlhm = NHOle(1)
               nlpm = NTOtal(2)
               kh = 1
               kp = 2
            ENDIF
            kc = 0
            DO nh = 1, nlhm
               lth = LSP(nh,kh)
               jtw1 = JSP(nh,kh)
               nos1 = NSP(nh,kh) - 1
               IF (VAMp(nh,kh).GE.0.01D0) THEN
                  DO np = 1, nlpm
                     IF (UAMp(np,kp).GE.0.01D0) THEN
                        wbcs = VAMp(nh,kh)*UAMp(np,kp) + UAMp(nh,kh)
     &                         *VAMp(np,kp)
                        IF (wbcs.GE.0.01D0) THEN
                           ltp = LSP(np,kp)
                           jtw2 = JSP(np,kp)
                           nos2 = NSP(np,kp) - 1
                           eqq = EBCs(nh,kh) + EBCs(np,kp)
                           IF (eqq.LE.eqqx) THEN
                              kc = kc + 1
C
C-----------------------------total width at the 2-qp energy (multiplied by
C-----------------------------factor 1/2)
                              wqq = DWIDTH(eqq,widas,e0,aew)*0.5
C
C-----------------------------energy shift at the 2-qp energy (dispersion
C-----------------------------integral)
                              deqq = 0.D0
                              w = 4.
                              de3 = ESTep/3.
                              DO ne = 0, nesx
                                 w = 6.D0 - w
                                 we = w*de3
                                 IF (ne.EQ.0 .OR. ne.EQ.nebinx) we = de3
                                 em = eqq - ess(ne)
                                 ep = eqq + ess(ne)
                                 fe = (wide(ne) - wqq)
     &                                *(1.D0/em + 1.D0/ep)
                                 deqq = deqq + we*fe
                              ENDDO
                              deqq = deqq/PI
                              wqqst(kc) = wqq*2.D0
                              dqqst(kc) = deqq
                              eqq = eqq + deqq
                              eqqst(kc) = eqq
C
C-----------------------------end of energy shift
C
                              l1 = IABS(ltp - lth)
                              l2 = IABS(jtw1 - jtw2)/2
                              ltmin = MAX(l1,l2)
                              IF (MOD(ltmin - l1,2).EQ.1)
     &                            ltmin = ltmin + 1
                              ltmin = ltmin + 1
                              ltmax = MIN(ltp + lth,(jtw1 + jtw2)/2) + 1
                              ltmax = MIN(ltmax,ltmaxr)
                              IF (ltmin.LE.ltmax) THEN
                                 DO ltp1 = ltmin, ltmax, 2
                                    lttw = 2*(ltp1 - 1)
C
C-----------------------------------calculate the 2-qp matrix elements
C-----------------------------------REDUQQ = reduced matrix element (squared)
C-----------------------------------PHTRM = radial  matrix element
C-----------------------------------UMATQQ = full    matrix element (squared)
C
                                    IA = jtw1
                                    IB = jtw2
                                    IC = lttw
                                    ID = 1
                                    IE = -1
                                    IG = 0
                                    hat = (jtw1 + 1)*(jtw2 + 1)
                                    CALL CLEBTRI
                                    r1 = RAC
                                    reduqq = r1*r1*hat*fourpi/FLOAT
     &                                 (lttw + 1)
                                    ltr = lttw/2
C
                                    CALL RADIAL(bosc,phtrm,nos1,lth,
     &                                 nos2,ltp,ltr)
C
                                    IF (ltr.GT.0)
     &                                  phtrm = phtrm*vnorm/rws**ltr
                                    IF (ltr.EQ.0)
     &                                  phtrm = phtrm*vnorm/rws**2
                                    umatqq = reduqq*(phtrm*wbcs)**2
                                    IF (umatqq.GE.1.D-08) THEN
C
C--------------------------------------count number of 2-qp configurations
C
                                       nconf(ltp1) = nconf(ltp1) + 1
C
C--------------------------------------calculate the 2-quasiparticle Green
C--------------------------------------function calculate the 2-quasiparticle
C--------------------------------------response function prepare for the
C--------------------------------------self-consistent width of QRPA-states
                                       DO ne = 0, nebinx + 1
                                         ext = Est(ne)
                                         dwex = wqq + 0.5D0*WIDex
                                         dwsx = dwex*dwex
                                         emi = eqq - ext
                                         epl = eqq + ext
                                         emisq = emi*emi
                                         eplsq = epl*epl
                                         pxmd = 1./(emisq + dwsx)
                                         pymd = 1./(eplsq + dwsx)
C
C----------------------------------------real and imaginary parts of the 2-qp
C----------------------------------------Green functions
                                         greenr = (emi*pxmd + epl*pymd)
     &                                      *umatqq
                                         greenx = wqq*(pxmd - pymd)
     &                                      *umatqq
                                         greeny = (pxmd - pymd)*umatqq
C
C                                         real and imaginary parts of the 2-qp
C                                         response functions
                                         rfqqr(ne,ltp1) = rfqqr(ne,ltp1)
     &                                      + greenr
                                         rfqqx(ne,ltp1) = rfqqx(ne,ltp1)
     &                                      + greenx
                                         rfqqy(ne,ltp1) = rfqqy(ne,ltp1)
     &                                      + greeny
                                       ENDDO
C
C calculate the 2qp response function at the fitting energy
C
                                       IF (efit(ltp1).NE.0.0D0) THEN
                                         i = 1
                                         ext = efit(ltp1) - ESTep/10.D0
                                         DO WHILE (i.LE.3)
                                         emi = eqq - ext
                                         epl = eqq + ext
                                         emisq = emi*emi
                                         eplsq = epl*epl
                                         pxmd = 1./(emisq + dwsx)
                                         pymd = 1./(eplsq + dwsx)
C
C----------------------------------------real and imaginary parts of the 2-qp
C----------------------------------------Green function
C
                                         greenr = (emi*pxmd + epl*pymd)
     &                                      *umatqq
                                         greenx = wqq*(pxmd - pymd)
     &                                      *umatqq
                                         greeny = (pxmd - pymd)*umatqq
C
C----------------------------------------real and imaginary parts of the 2-qp
C----------------------------------------response function
                                         IF (i.EQ.1) THEN
                                         cr1(ltp1) = cr1(ltp1) + greenr
                                         ccm(ltp1) = ccm(ltp1)
     &                                      + greenx + 
     &                                      0.5D0*WIDex*greeny
                                         ELSEIF (i.EQ.2) THEN
                                         ccr(ltp1) = ccr(ltp1) + greenr
                                         xea(ltp1) = xea(ltp1) + greenx
                                         yea(ltp1) = yea(ltp1) + greeny
                                         cci(ltp1) = cci(ltp1)
     &                                      + greenx + 
     &                                      0.5D0*WIDex*greeny
C
C calculate the 1'st derivative of the 2qp response function at the fitting energy
C
                                         dcr(ltp1) = dcr(ltp1)
     &                                      + ((emisq - dwsx)*pxmd**2 - 
     &                                      (eplsq - dwsx)*pymd**2)
     &                                      *umatqq
                                         dci(ltp1) = dci(ltp1)
     &                                      + ((2.D0*wqq + WIDex)
     &                                      *(emi*pxmd**2 + epl*pymd**2)
     &                                      )*umatqq
                                         ELSEIF (i.EQ.3) THEN
                                         cr3(ltp1) = cr3(ltp1) + greenr
                                         ccp(ltp1) = ccp(ltp1)
     &                                      + greenx + 
     &                                      0.5D0*WIDex*greeny
                                         ENDIF
                                         ext = ext + ESTep/10.0D0
                                         i = i + 1
                                         ENDDO
                                       ENDIF
                                    ENDIF
                                 ENDDO
                              ENDIF
                           ENDIF
                        ENDIF
                     ENDIF
                  ENDDO
               ENDIF
            ENDDO
         ENDDO
         kcx = kc
         kqq = 1
         IF (kqq.EQ.1) THEN
C
C-----------sorting
C
            DO n = 1, kcx - 1
               np1 = n + 1
               x = eqqst(n)
               aqq = wqqst(n)
               bqq = dqqst(n)
               DO np = np1, kcx
                  xp = eqqst(np)
                  xqq = wqqst(np)
                  yqq = dqqst(np)
                  IF (x.GT.xp) THEN
                     eqqst(np) = x
                     wqqst(np) = aqq
                     dqqst(np) = bqq
                     eqqst(n) = xp
                     wqqst(n) = xqq
                     dqqst(n) = yqq
                     x = xp
                     aqq = xqq
                     bqq = yqq
                  ENDIF
               ENDDO
            ENDDO
         ENDIF
C
C
C--------the multipole fields are normalized as:
C--------U(l;r) = (r/rd)**l * homega
C--------rd     = half-density radius of the g.s. density rhogs(r)
C--------homega = oscillator energy
C--------0
C--------the Tassie model is taken for the transition densities:              0
C--------rhotr(l;r) = (r/rd)**(l-1)*rd*d rhogs(r) /dr
C--------rd         = half-density radius of rhogs(r)
C--------self-consistent    coupling constants CLSC(L)
C--------phenomenological   coupling constants CLEX(L)
C--------renomalization of  coupling constants CLRN(L) = CLEX(L)/CLSC(L)
C--------( from fit to experimental energies )
C
C-------->>>> the inverse coupling constants are calculated! <<<<
C
C--------units for CLSC and CLEX :   [MeV]
C
         DO lt = 1, ltmaxr
C
C           INVERSE COUPLING CONSTANTS (FROM FIT TO EXP. ENERGIES)
C
            IF (efit(lt).NE.0.0D0) THEN
               IF (dcr(lt).NE.0.0D0) THEN
                  xir = dci(lt)/dcr(lt)
                  xpos = cci(lt)*xir/(1.D0 + SQRT(1.D0 + xir**2))
                  xneg = -cci(lt)**2/xpos
                  cpos = ccr(lt) + xpos
                  cneg = ccr(lt) + xneg
                  ccpm(1) = cpos
                  ccpm(2) = cneg
C
C-----------------check   for maximum
C
C                 cr1 = rfqqr(nea - 1, lt)
                  cr2 = ccr(lt)
C                 cr3 = rfqqr(nea + 1, lt)
                  clex(lt) = 0.D0
                  DO j = 1, 2
                     re1 = ccm(lt)/((ccpm(j) - cr1(lt))**2 + ccm(lt)**2)
                     re2 = cci(lt)/((ccpm(j) - cr2)**2 + cci(lt)**2)
                     re3 = ccp(lt)/((ccpm(j) - cr3(lt))**2 + ccp(lt)**2)
                     ddr(j) = (re3 + re1 - 2.D0*re2)/ESTep**2
                     rrr(j) = re2
                  ENDDO
                  IF (ddr(1).LT.0.D0 .AND. ddr(2).GT.0.D0) clex(lt)
     &                = ccpm(1)
                  IF (ddr(2).LT.0.D0 .AND. ddr(1).GT.0.D0) clex(lt)
     &                = ccpm(2)
                  IF (ddr(1).LT.0.D0 .AND. ddr(2).LT.0.D0) THEN
                     IF (rrr(1).GT.rrr(2)) clex(lt) = ccpm(1)
                     IF (rrr(2).GT.rrr(1)) clex(lt) = ccpm(2)
                  ENDIF
C                 cr1 = rfqqr(nea - 1, lt)
                  cr2 = ccr(lt)
C                 cr3 = rfqqr(nea + 1, lt)
                  re1 = ccm(lt)/((clex(lt) - cr1(lt))**2 + ccm(lt)**2)
                  re2 = cci(lt)/((clex(lt) - cr2)**2 + cci(lt)**2)
                  re3 = ccp(lt)/((clex(lt) - cr3(lt))**2 + ccp(lt)**2)
               ELSE
                  clex(lt) = 0.0
               ENDIF
               IF (clex(lt).EQ.0.D0) THEN
                  WRITE (6,99065) lt - 1, efit(lt), ddr
99065             FORMAT (/'WARNING: From TRISTAN:'/
     &                   'WARNING: - No fit of response function for J='
     &                   ,I3/'WARNING: E(EXP.):',F8.2,'  2nd deriv.:',
     &                   2E13.5/
     &             'WARNING: Energy is inconsistent with 2-qp spectrum!'
     &             /'WARNING: Self-consistent response is used!')
                  efit(lt) = 0.D0
                  clex(lt) = 0.D0
                  xea(lt) = 0.D0
                  yea(lt) = 0.D0
C                 xea(lt) = rfqqx(nea, lt)
C                 yea(lt) = rfqqy(nea, lt)
               ENDIF
            ENDIF
C
            l = lt - 1
            fltwp1 = 2*l + 1
            lm = MAX(2*l - 1,1)
            IF (l.EQ.1 .OR. efit(lt).GT.homeb) THEN
               IF (l.GT.1) clsc(lt) = vnorm*GSDm(lm)*SQRT(fltwp1)
     &                                *rdsq/(rd*rws)**l
C--------------isovector strength is chosen as 1/2 of the isoscalar strength
               IF (l.EQ.1) clsc(lt) = -0.5D0*vnorm*GSDm(2)
     &                                *4.D0/(rws*SQRT(fltwp1))
            ELSE
C-----------vibrational model  (good for low-energy states)
               clsc(lt) = vnorm*GSDm(l + 1)*(l + 3)
     &                    /(SQRT(fltwp1)*rws**l)
            ENDIF
C
C-----------monopole case (0+) - compressional mode
C-----------factor pi**2/6 comes from series expansion of
C-----------the   0+ transition potential
C
            IF (l.EQ.0) clsc(lt) = 2.*(vnorm/rwsq)*GSDm(3)*36.D0/PI**4
            IF (efit(lt).GT.0.0D0) THEN
               CLRn(lt) = clsc(lt)/clex(lt)
            ELSE
               CLRn(lt) = 1.0
               clex(lt) = clsc(lt)
            ENDIF
C
C-----------effective coupling constant is used in the response functions
C           ( if EFIT is .NE. 0 )
C
            ceff = 1./clex(lt)
            cnorm = ceff*ceff
            rnorm = cnorm/CLRn(lt)**2
C-----------normalization of the response function with the factor given in input
            rnorm = rnorm*CNOrin(lt)
C
C-----------calculate the QRPA response function                    (rho )
C-----------calculate the QRPA deformation parameters               (beta)
C
            DO ne = 1, nebinx
               qqr = rfqqr(ne,lt)*cnorm
               qqx = rfqqx(ne,lt)*cnorm
               qqy = rfqqy(ne,lt)*cnorm
               qqy = MAX(1.D-06,qqy)
               wqa = qqx/qqy
               wqrex = 0.5*WIDex + wqa
               qqi = wqrex*qqy
               rqr = ceff - qqr
               rho1 = rnorm*qqi/(rqr**2 + qqi**2)
               RHO(ne,lt) = rho1/PI
            ENDDO
C
C-----------calculate RPA-deformation parameters beta
C-----------(integrated over an energy interval of 2*ESTEP )
C-----------(except for the first and last mesh points   )
C
            BETa(1,lt) = 0.5*ESTep*(RHO(1,lt) + RHO(2,lt))
            BETa(nebinx,lt) = 0.5*ESTep*(RHO(nebinx,lt) + RHO(nebinx - 1
     &                        ,lt))
            DO ne = 2, nebinx - 1
               BETa(ne,lt) = est3*(RHO(ne - 1,lt) + 4.*RHO(ne,lt) + RHO(
     &                       ne + 1,lt))
            ENDDO
            DO ne = 1, nebinx
               BETa(ne,lt) = SQRT(BETa(ne,lt))
            ENDDO
         ENDDO
C
C-----write response function on TAPE17
C
C        DO NE=1,NEBINX
C        WRITE(17,1400)EST(NE),(RHO(NE,I),I=1,MIN(10,ICMAX))
C        ENDDO
C
         IF (krt.EQ.1) WRITE (6,99080)
99080    FORMAT (//6X,'COUPLING CONSTANTS AND RENORMALIZATION'/6X,
     &           '       ( INELASTIC EXCITATION )      '/3X,'L',4X,
     &           'EA  ',2X,'SELF-CON',3X,'EMPIRICAL',6X,'RATIO',7X,
     &           'VEFF',4X,'RESIDUE',6X,'WIDTH',' CONFIG.')
         IF (krt.EQ.2) WRITE (6,99085)
99085    FORMAT (//6X,'COUPLING CONSTANTS AND RENORMALIZATION'/6X,
     &           '   ( CHARGE EXCHANGE EXCITATION )    '/3X,'L',4X,
     &           'EA  ',3X,'SELF-CON',3X,'EMPIRICAL',6X,'RATIO',4X,
     &           'RESIDUE',6X,'WIDTH',' CONFIG.')
         DO l = 1, ICMax
            clsc(l) = 1./clsc(l)
            clex(l) = 1./clex(l)
            IF (yea(l).NE.0.0D0) THEN
               resid = 1./yea(l)
               widea = 2.0*xea(l)/yea(l)
            ELSE
               resid = 0.0
               widea = 0.0
            ENDIF
            veff = vnorm*CLRn(l)
            WRITE (6,99090) l - 1, efit(l), clsc(l), clex(l), CLRn(l), 
     &                      veff, resid, widea, nconf(l)
99090       FORMAT (I4,F8.3,2E11.4,2F11.4,E11.4,F11.4,I7)
         ENDDO
         WRITE (6,99095) eqqx
99095    FORMAT (8X,'CONFIGURATION SPACE IS EQQX:',F9.3,' (MEV)')
C
C--------calculate non-energy weighted (SRNEW) and energy weighted (SREW)
C--------sum rules
C
         DO ltp1 = 1, ltmaxr
            DO ne = 1, nebinx
               hhh = ESTep
               IF (ne.EQ.1 .OR. ne.EQ.nebinx) hhh = 0.5*hhh
               eqq = Est(ne)
               t2 = hhh*RHO(ne,ltp1)
               SRNew(ltp1) = SRNew(ltp1) + t2
               SREw(ltp1) = SREw(ltp1) + t2*eqq
               IF (eqq.LE.ETMax) SREwl(ltp1) = SREwl(ltp1) + t2*eqq
            ENDDO
         ENDDO
C        WRITE(6,607)RD,(LT-1,LT=1,LTMAXR)
C        607 FORMAT(1H1/9X,'BETA-VALUES FOR TARGET EXCITATIONS  RD=',
C        1 F7.4//1H ,4X,' EX',11(5X,I3,3X))
C        DO 110 NE=1,NEBINX
C        110 WRITE(6,608)EST(NE),(BETA(NE,LT),LT=1,LTMAXR)
C        608 FORMAT(1H ,F7.2,11E11.4)
      ENDDO
      END
C
C
      SUBROUTINE RADIAL(Bdtw,Phtrm,N1,L1,N2,L2,Ltr)
C
C COMMON variables
C
      DOUBLE PRECISION FAClog(500), RAC, U9
      INTEGER IA, IB, IC, ID, IE, IG, L9(10)
      COMMON /CRAC  / FAClog, RAC, U9, IA, IB, IC, ID, IE, IG, L9
C
C Dummy arguments
C
      DOUBLE PRECISION Bdtw, Phtrm
      INTEGER L1, L2, Ltr, N1, N2
C
C Local variables
C
      DOUBLE PRECISION fnorm, s1, sum, t1
      INTEGER IABS
      INTEGER k1, k2, k3, k4, k5, k6, k7, k8, l, ln1, ln2, nc1, nc2, 
     &        nup1, nup1mi, nup1mx
      Phtrm = 0.0
      l = Ltr
      IF (MOD(l + L1 + L2,2).NE.0) RETURN
      IF (l.GT.L1 + L2 .OR. l.LT.IABS(L1 - L2)) RETURN
      nc1 = N1*2 + L1
      nc2 = N2*2 + L2
      IF (l.EQ.0) l = 2
      IF (l.GE.IABS(nc1 - nc2)) THEN
         k1 = (l + L2 - L1)/2
         nup1mi = MAX(N1 - k1 + 1,1)
         nup1mx = N2 + 1
         IF (nup1mx.GE.nup1mi) THEN
            ln1 = L1 + N1 + 1
            ln2 = L2 + N2 + 1
            s1 = 1 - MOD(N1,2)*2
            t1 = FAClog(N2 + 1) + FAClog(ln2*2) + FAClog(ln1)
     &           - FAClog(N1 + 1) - FAClog(ln1*2) - FAClog(ln2)
            fnorm = EXP(t1*0.5)*2.0**(N1 - N2 - l)*s1
            sum = 0.0
            k1 = l + L1 + L2 + nup1mi*2 - 2
            k2 = (l + L2 - L1)/2 + nup1mi - 1
            k3 = L2 + nup1mi - 1
            k4 = (l + L2 + L1)/2 + nup1mi - 1
            k5 = k2 - N1
            k6 = nup1mi - 1
            k7 = N2 - nup1mi + 3
            k8 = L2*2 + nup1mi*2 - 2
            DO nup1 = nup1mi, nup1mx
               k1 = k1 + 2
               k8 = k8 + 2
               k2 = k2 + 1
               k3 = k3 + 1
               k4 = k4 + 1
               k5 = k5 + 1
               k6 = k6 + 1
               k7 = k7 - 1
               s1 = 1 - MOD(nup1 - 1,2)*2
               t1 = FAClog(k1) + FAClog(k2) + FAClog(k3) - FAClog(k4)
     &              - FAClog(k5) - FAClog(k6) - FAClog(k7) - FAClog(k8)
               sum = sum + EXP(t1)*s1
            ENDDO
            Phtrm = sum*fnorm*Bdtw**l
         ENDIF
      ENDIF
      END
C
C
      SUBROUTINE CLEBTRI
C
C COMMON variables
C
      DOUBLE PRECISION FAClog(500), RAC, U9
      INTEGER IA, IB, IC, ID, IE, IG, L9(10)
      COMMON /CRAC  / FAClog, RAC, U9, IA, IB, IC, ID, IE, IG, L9
C
C Local variables
C
      DOUBLE PRECISION fb, fc2, s1, sqfclg, ssterm, termlg
      INTEGER iabc, iabcp, iamd, iapd, ibca, ibme, ibpe, icab, icmf, 
     &        icpf, k1, k2, k3, nz, nzm1, nzmi, nzmic2, nzmic3, nzmx, 
     &        nzt1, nzt2, nzt3, nzt4, nzt5
      INTEGER IABS
      RAC = 0.0
      IF (ID + IE.EQ.IG) THEN
         k1 = IA + IB + IC
         IF (k1.EQ.2*(k1/2)) THEN
            k1 = IA + IB - IC
            k2 = IC - IABS(IB - IA)
            k3 = MIN(k1,k2)
            IF (k3.GE.0) THEN
               IF (( - 1)**(IB + IE).GT.0) THEN
                  IF (( - 1)**(IC + IG).GT.0) THEN
                     IF (IA.GE.IABS(ID)) THEN
                        IF (IB.GE.IABS(IE)) THEN
                           IF (IC.GE.IABS(IG)) THEN
                              IF (IA.GE.0) THEN
                                 IF (IA.EQ.0) THEN
                                    RAC = 1.0
                                 ELSEIF (IB.GE.0) THEN
                                    IF (IB.EQ.0) THEN
                                       RAC = 1.0
                                    ELSEIF (IC.GE.0) THEN
                                       IF (IC.EQ.0) THEN
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
                                         nzmi = MAX(MAX(0,nzmic2),
     &                                      nzmic3) + 1
                                         nzmx = MIN(MIN(iabc,iamd),ibpe)
                                         s1 = 1 - 2*MOD(nzmi - 1,2)
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
                                         IF (ABS(RAC).LT.1.0D-10)
     &                                      RAC = 0.0
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
      SUBROUTINE TRIDIG(N,A,B,C,D,Ierr)
C
C Dummy arguments
C
      INTEGER Ierr, N
      DOUBLE PRECISION A(N), B(N), C(N), D(N)
C
C Local variables
C
      DOUBLE PRECISION alpha
      INTEGER i, j, k
      Ierr = 0
      IF (N.LE.1) THEN
         IF (D(1).EQ.0.D0) THEN
            i = 1
            GOTO 100
         ELSE
            A(1) = A(1)/D(1)
            IF (N.LE.0) Ierr = -1
            GOTO 99999
         ENDIF
      ENDIF
      alpha = D(1)
      IF (alpha.NE.0.D0) THEN
         C(1) = C(1)/alpha
         C(N) = 0.
         D(1) = A(1)/alpha
         DO i = 2, N
            alpha = D(i) - B(i)*C(i - 1)
            IF (ABS(alpha).LT.1.D-20) GOTO 100
            C(i) = C(i)/alpha
            D(i) = (A(i) - B(i)*D(i - 1))/alpha
         ENDDO
         A(N) = D(N)
         DO k = 2, N
            j = N + 1 - k
            A(j) = D(j) - C(j)*A(j + 1)
         ENDDO
         GOTO 99999
      ELSE
         i = 1
      ENDIF
  100 Ierr = i
99999 END
C
      SUBROUTINE BCS(A,Gap,I3,Ndim,Chemic)
C
C COMMON variables
C
      DOUBLE PRECISION EBCs(500,2), ESP(500,2), GSD(201,3), GSP(201,2), 
     &                 RST(201), UAMp(500,2), VAMp(500,2), WFR(201,50)
      INTEGER IBLk(2), JBL(2), JSP(500,2), LBL(2), LSP(500,2), NBL(2), 
     &        NRX, NSP(500,2)
      COMMON /BLOCK / IBLk, NBL, LBL, JBL
      COMMON /DENS  / GSD, GSP
      COMMON /EVBCS / EBCs, VAMp, UAMp
      COMMON /RADI  / RST, NRX
      COMMON /SPQN  / ESP, NSP, LSP, JSP
      COMMON /WAVF  / WFR
C
C Dummy arguments
C
      DOUBLE PRECISION A, Chemic, Gap
      INTEGER I3, Ndim
C
C Local variables
C
      DOUBLE PRECISION chem(3), d(3), dc, dvl, e, eh, ep, eta, fpi, fsq, 
     &                 gjj, gsq, pi, pjj, usq, vsq, xnum
      INTEGER i, ii, imax, k, n, nrxx, nst
      CHARACTER*40 text(2)
      DATA imax/100/
      pi = 4.*ATAN(1.)
      fpi = 4.*pi
      IF (Gap.LT.0.0D0) Gap = 2.
      text(1) = 'PROTONS '
      text(2) = 'NEUTRONS'
      IF (IBLk(I3).NE.0) THEN
         WRITE (6,99005) text(I3), NBL(I3), LBL(I3), JBL(I3)
99005    FORMAT (/5X,'PAIRING WITH BLOCKING FOR ',A10/5X,'N:',I3,'  L:',
     &           I3,'  J:',I3,'/2')
      ELSE
         WRITE (6,99010) text(I3)
99010    FORMAT (/5X,'PAIRING WITHOUT BLOCKING FOR ',A10/5X)
      ENDIF
      DO n = 1, NRX
         GSD(n,I3) = 0.0
         GSP(n,I3) = 0.0
      ENDDO
C
C-----sort energies
C
      CALL ESORT(Ndim,I3)
C
C-----BCS CALCULATION IN THE CONSTANT GAP APPROXIMATION
C
      gsq = Gap*Gap
      dc = 1.0
      chem(1) = ESP(1,I3)
C
      CALL NUMBER(Ndim,I3,gsq,A,xnum,chem(1),d(1))
C
C-----iteration for chemical potential
C
      ii = 0
  100 ii = ii + 1
      IF (ii.GT.imax) WRITE (6,*) 
     &                   ' INITIALISATION FOR CHEMICAL POTENTIAL FAILED'
      chem(2) = chem(1) + dc
      CALL NUMBER(Ndim,I3,gsq,A,xnum,chem(2),d(2))
      IF (d(1)*d(2).LE.0.0D0) THEN
         ii = 0
  150    ii = ii + 1
         dvl = (d(2) - d(1))/(chem(2) - chem(1))
         dc = -d(1)/dvl
         chem(2) = chem(1)
         d(2) = d(1)
         chem(1) = chem(1) + dc
         CALL NUMBER(Ndim,I3,gsq,A,xnum,chem(1),d(1))
         IF (ABS(d(1)).LT.1.D-04) THEN
            Chemic = chem(1)
            text(1) = ' BCS FOR PROTONS  '
            text(2) = ' BCS FOR NEUTRONS '
            WRITE (6,99015) text(I3), Gap, Chemic, xnum
99015       FORMAT (//A20,'  GAP:',F8.3,'  CHEM.POT.:',F10.4,' NUMBER:',
     &              F8.1/'  NO.   N   L     J',5X,'EBCS',7X,'V',5X,
     &              'VSQ',3X,'EHOLE',3X,'EPART')
            DO i = 1, Ndim
C              n = indeks(i)
C--------------original line above was replaced by the one below to avoid
C--------------'indeks undefined' message. (n is not used anyway)
               n = i
               eta = ESP(i,I3) - Chemic
               e = SQRT(eta*eta + gsq)
               vsq = 0.5*(1.0 - eta/e)
               usq = 1.0 - vsq
               EBCs(i,I3) = e
               VAMp(i,I3) = SQRT(vsq)
               UAMp(i,I3) = SQRT(usq)
               eh = ( - e) + Chemic
               ep = e + Chemic
               IF (vsq.GE.usq) THEN
                  ESP(i,I3) = eh
               ELSE
                  ESP(i,I3) = ep
               ENDIF
               WRITE (6,99020) i, NSP(i,I3), LSP(i,I3), JSP(i,I3), e, 
     &                         VAMp(i,I3), vsq, eh, ep
99020          FORMAT (I5,3I4,'/2',F9.4,2F8.5,F8.3,2X,2F8.3)
C
C--------------TEMPORARY
C--------------(SKIP CALCULATION OF MICROSC. G.S. DENSITIES)
C
               IF (vsq.LT.0.0D0) THEN
                  IF (IBLk(I3).EQ.0) THEN
                     gjj = (JSP(i,I3) + 1.)*vsq/fpi
                  ELSE
                     IF (NSP(i,I3).EQ.NBL(I3) .AND. LSP(i,I3).EQ.LBL(I3)
     &                   .AND. JSP(i,I3).EQ.JBL(I3))
     &                   gjj = ((JSP(i,I3) + 1.0)*vsq + (usq - vsq))/fpi
                  ENDIF
                  pjj = (JSP(i,I3) + 1.0)*SQRT(usq*vsq)/fpi
                  DO k = 1, NRX
                     fsq = WFR(k,n)**2
                     GSD(k,I3) = GSD(k,I3) + gjj*fsq
                     GSP(k,I3) = GSP(k,I3) + pjj*fsq
                  ENDDO
               ENDIF
            ENDDO
            IF (NRX.GE.0) RETURN
            WRITE (6,99025)
99025       FORMAT (//5X,'DENSITY AND PAIRING DENSITY'/8X,'R',6X,
     &              'DENSITY',6X,'PAIRING')
            nrxx = MIN(12.D0/(RST(2) - RST(1)),1.D0*NRX)
            nst = MAX(1,nrxx/25)
            DO n = 1, nrxx, nst
               k = MAX(1,n - 1)
               WRITE (6,99030) RST(k), GSD(k,I3), GSP(k,I3)
99030          FORMAT (F9.2,5E13.5)
            ENDDO
         ELSE
            IF (ii.GT.imax) THEN
               WRITE (6,*) ' NO CONVERGENCE FOR CHEMICAL POTENTIAL'
               STOP
            ENDIF
            GOTO 150
         ENDIF
      ELSE
         chem(1) = chem(2)
         d(1) = d(2)
         GOTO 100
      ENDIF
      END
C
      SUBROUTINE NUMBER(Ndim,I3,Gsq,A,Xnum,Chem,D)
C
C COMMON variables
C
      DOUBLE PRECISION ESP(500,2)
      INTEGER IBLk(2), JBL(2), JSP(500,2), LBL(2), LSP(500,2), NBL(2), 
     &        NSP(500,2)
      COMMON /BLOCK / IBLk, NBL, LBL, JBL
      COMMON /SPQN  / ESP, NSP, LSP, JSP
C
C Dummy arguments
C
      DOUBLE PRECISION A, Chem, D, Gsq, Xnum
      INTEGER I3, Ndim
C
C Local variables
C
      DOUBLE PRECISION e, eta, fjj, s1, s2, vsq
      INTEGER n
      Xnum = 0.0
      s1 = 0.0
      s2 = 0.0
      DO n = 1, Ndim
         eta = ESP(n,I3) - Chem
         e = SQRT(eta*eta + Gsq)
         vsq = 0.5*(1.0 - eta/e)
         fjj = JSP(n,I3) + 1.0
         IF (IBLk(I3).NE.0) THEN
            IF (NBL(I3).EQ.NSP(n,I3) .AND. JBL(I3).EQ.JSP(n,I3) .AND. 
     &          LBL(I3).EQ.LSP(n,I3)) fjj = fjj + (1.0 - 2.0*vsq)/vsq
         ENDIF
         Xnum = Xnum + fjj*vsq
         s1 = s1 + fjj/e
         s2 = s2 + fjj*(1. - ESP(n,I3)/e)
      ENDDO
      D = Chem + (s2 - 2.*A)/s1
      END
C
      SUBROUTINE ESORT(Ndim,I3)
C
C COMMON variables
C
      DOUBLE PRECISION EBCs(500,2), ESP(500,2), UAMp(500,2), VAMp(500,2)
      INTEGER JJ(500,2), LL(500,2), NN(500,2)
      COMMON /EVBCS / EBCs, VAMp, UAMp
      COMMON /SPQN  / ESP, LL, NN, JJ
C
C Dummy arguments
C
      INTEGER I3, Ndim
C
C Local variables
C
      INTEGER i, ip, j, jp, k, kp, n, np, np1
      DOUBLE PRECISION q, qp, u, up, v, vp, x, xp
C
C     ORDER ENERGIES
C
C     DO 3 N=1,NDIM
C     3  INDEKS(N)=N
C
      DO n = 1, Ndim - 1
         np1 = n + 1
         x = ESP(n,I3)
         q = EBCs(n,I3)
         i = NN(n,I3)
         j = LL(n,I3)
         k = JJ(n,I3)
         u = UAMp(n,I3)
         v = VAMp(n,I3)
C--------L=INDEKS(N)
         DO np = np1, Ndim
            xp = ESP(np,I3)
            qp = EBCs(np,I3)
            ip = NN(np,I3)
            jp = LL(np,I3)
            kp = JJ(np,I3)
            up = UAMp(n,I3)
            vp = VAMp(n,I3)
C-----------LP=INDEKS(NP)
            IF (x.GT.xp) THEN
               ESP(np,I3) = x
               EBCs(np,I3) = q
               UAMp(np,I3) = u
               VAMp(np,I3) = v
               NN(np,I3) = i
               LL(np,I3) = j
               JJ(np,I3) = k
C--------------INDEKS(NP)=L
               ESP(n,I3) = xp
               EBCs(n,I3) = qp
               UAMp(n,I3) = up
               VAMp(n,I3) = vp
               NN(n,I3) = ip
               LL(n,I3) = jp
               JJ(n,I3) = kp
C--------------INDEKS(N)=LP
               x = xp
               q = qp
               u = up
               v = vp
               i = ip
               j = jp
               k = kp
C--------------L=LP
            ENDIF
         ENDDO
      ENDDO
      END
C
C
      INTEGER FUNCTION INDF(N,L,Jtw)
C
C Dummy arguments
C
      INTEGER Jtw, L, N
C
C Local variables
C
      INTEGER n0, np
      np = 2*(N - 1) + L
      n0 = (np*(np + 1))/2
      INDF = n0 + (Jtw + 1)/2
      END
      DOUBLE PRECISION FUNCTION DWIDTH(E,W0,E0,A0)
C
C-----damping width of 1-phonon states
C-----anti-symmetric E-dependence is taken
C
C Dummy arguments
C
      DOUBLE PRECISION A0, E, E0, W0
C
C Local variables
C
      DOUBLE PRECISION f0, fe, g0, ge
      fe = EXP((E - E0)/A0)
      ge = EXP(( - (E+E0)/A0))
      f0 = 1./(1. + fe)
      g0 = 1./(1. + ge)
      DWIDTH = W0*(g0 - f0)
      END
C
C
      SUBROUTINE SPLVL(I3,Amass,Nz,Iout)
C
C COMMON variables
C
      DOUBLE PRECISION ALSin, BST(3), CNOrin(22), EBCs(500,2), 
     &                 EFItin(22), ESP(500,2), GAP(2), GAPin(2), HOMega,
     &                 HOMin, RMS(3), RST(201), UAMp(500,2), VAMp(500,2)
     &                 , VLS(2), WIDexin
      INTEGER IBLk(2), JBL(2), JSP(500,2), LBL(2), LSP(500,2), NBL(2), 
     &        NHOle(2), NRX, NSP(500,2), NTOtal(2)
      COMMON /BLOCK / IBLk, NBL, LBL, JBL
      COMMON /EVBCS / EBCs, VAMp, UAMp
      COMMON /RADI  / RST, NRX
      COMMON /SPPA  / HOMega, VLS, RMS, BST, GAP, NHOle, NTOtal
      COMMON /SPQN  / ESP, NSP, LSP, JSP
      COMMON /TRINP / WIDexin, GAPin, HOMin, ALSin, EFItin, CNOrin
C
C Dummy arguments
C
      DOUBLE PRECISION Amass
      INTEGER I3, Iout, Nz
C
C Local variables
C
      DOUBLE PRECISION a3, ad, all, als, anp(2,2), bcsrerun, chemic, 
     &                 dnz, e0, espx, hhh, hom, pi, pqn, r, rd, rde, 
     &                 rmsd, rnp(3,2), rw0, rws, uscal, uvec, vll(16,2),
     &                 w, xnp
      DOUBLE PRECISION EHO
      REAL FLOAT
      INTEGER i, ifermi, iocc(2000), ipr, is, jtw, k, l, lmax, lmin, 
     &        ltw, n, n1, n2, ndim, nhx, nl, nnucl, np, npar, nq, nqx, 
     &        nr, nspl, numnuc
      INTEGER INDF
      INTEGER INT
      CHARACTER*10 text(2)

      DATA rnp/1.2490D0, -0.5401D0, -0.9582D0, 1.2131D0, -0.4415D0, 
     &     0.8931D0/
      DATA anp/0.4899D0, -0.1236D0, 0.4686D0, 0.0741D0/
      DATA vll/0.0000D0, 0.0000D0, 0.0000D0, 0.0175D0, 0.0313D0, 
     &     0.0315D0, 0.0315D0, 0.0315D0, 0.0315D0, 0.0315D0, 0.0315D0, 
     &     0.0315D0, 0.0315D0, 0.0315D0, 0.0315D0, 0.0315D0, 0.0000D0, 
     &     0.0000D0, 0.0000D0, 0.0250D0, 0.0225D0, 0.0225D0, 0.0225D0, 
     &     0.0225D0, 0.0225D0, 0.0225D0, 0.0225D0, 0.0225D0, 0.0225D0, 
     &     0.0225D0, 0.0225D0, 0.0225D0/
C-----M. Herman increased first dimension in vll to 16 and set vll(16,1)
C-----=0.0315 and vll(16,2)=0.0225 to avoid going out of dimension
C-----also set vll(5,2) to 0.0225 instead of 0.0313 as in the original version.
      DATA uscal, uvec, rw0/51.5D0, 35.4D0, 1.27D0/
      pi = 4.*ATAN(1.D0)
      dnz = (Amass - 2.*Nz)/Amass
      a3 = Amass**(1./3.)
      rws = rw0*a3
      HOMega = 41.47/a3
      VLS(I3) = (5.0 + 8.0/a3**2)/HOMega
      rd = rnp(1,I3)*a3 + rnp(2,I3) + rnp(3,I3)*dnz
      ad = anp(1,I3) + anp(2,I3)*dnz
      hhh = 0.2
      rmsd = 0.
      NRX = 100
      r = 0.0
      w = 2.0
      DO n = 1, NRX
         r = r + hhh
         w = 6. - w
         IF (n.EQ.NRX) w = 1.
         rmsd = rmsd + r**4*w/(1. + EXP((r-rd)/ad))
      ENDDO
      rmsd = rmsd*hhh/3.
      rde = 0.75/(rd**3*pi*(1. + (pi*ad/rd)**2))
C
C-----calculate E0 = U(R=0) for protons and neutrons
C-----(s.p. energies are taken relative to E0)
C
      IF (I3.EQ.1) THEN
         e0 = (( - (uscal+dnz*uvec))) - 0.4*FLOAT(Nz)
     &        /a3 + 2.16*Nz/(1.2*a3)
         rde = Nz*rde
         xnp = Nz
      ELSE
         e0 = -(uscal - dnz*uvec)
         rde = (Amass - 2.*Nz)*rde
         xnp = Amass - Nz
      ENDIF
      rmsd = 4.*pi*rde*rmsd/Amass
C
      DO n = 1, 2000
         iocc(n) = 0
      ENDDO
      DO n = 1, 500
         EBCs(n,I3) = 0.
         ESP(n,I3) = 0.0
         VAMp(n,I3) = 0.0
         UAMp(n,I3) = 0.0
      ENDDO
C
C-----input Nilsson parameters for protons (I3=1) and neutrons (I3=2)
C-----first  card:
C-----HOM= hbar*omega (oscillator energy in MeV)
C-----ALS= l*s strength (in MeV)
C-----note: als is converted into units of homega intrinsically
C-----second card:
C-----pairing with blocking if IBLK .gt. 0
C-----NBL,LBL,JBL : nrad,l,2*j of blocked state
C-----third  card:
C-----NHX= number of (hole) states to be included in the BCS-calculation
C-----GAP= pairing gap
C-----NHX cards with:
C-----NSP,LSP,JSP,ESP  - nrad,l,2*j,esp  for BCS-configurations
C
      hom = HOMin
      als = ALSin
      IF (hom.GT.0.0D0) HOMega = hom
C
C-----BST contains the oscillator length
C
      BST(I3) = 197.33/SQRT(938.0*HOMega)
      e0 = 0.5*(e0 - HOMega*(rws/BST(I3))**2)
      IF (als.NE.0.0D0) VLS(I3) = als/HOMega
      als = VLS(I3)
      GAP(I3) = GAPin(I3)
      IF (GAP(I3).LE.0.0D0) GAP(I3) = 12.0/SQRT(Amass)
C     ESPX=100.+ABS(E0)
      espx = 100.
      nqx = MIN(espx/HOMega,15.D0)
      IF (nqx*HOMega.LT.espx) nqx = nqx + 1
      text(1) = 'PROTONS '
      text(2) = 'NEUTRONS'
      WRITE (6,99005) text(I3), HOMega, VLS(I3), VLS(I3)*HOMega, e0
99005 FORMAT (//,5X,'NILSSON-HAMILTONIAN FOR ',A10/'  HOMEGA:',F8.3,
     &        '  A(LS):',F8.3,' V(LS):',F8.3,
     &        ' (MEV)'/'  E0 (POTENTIAL DEPTH U(R=0)):',F8.3,' [MEV]')
      ipr = 5
      IF (Iout.GT.3) THEN
         WRITE (6,99010) (i - 1,i = 1,ipr)
99010    FORMAT (/7X,'A(LL) (IN UNITS OF HOMEGA)'/3X,'N',5(5X,'N+',I1))
         DO n = 1, nqx + 1, ipr
            n1 = n
            n2 = MIN(nqx + 1,n1 + ipr - 1)
            WRITE (6,99015) n - 1, (vll(k,I3),k = n1,n2)
99015       FORMAT (I4,5F8.4)
         ENDDO
      ENDIF
C     WRITE (6, 6003) TEXT(I3)
C6003 FORMAT(//5X,'SINGLE PARTICLE STATES - INPUT FOR ',A8/
C     1   '  NO.   N   L     J',5X,'ESP')
C
C
C-----single particle levels quantum numbers (NRAD,L,2*J)
      IF (I3.EQ.1) THEN
C-----protons
         NSP(1,1) = 1
         NSP(2,1) = 1
         NSP(3,1) = 1
         NSP(4,1) = 2
         NSP(5,1) = 1
         NSP(6,1) = 1
         NSP(7,1) = 2
         NSP(8,1) = 2
         NSP(9,1) = 1
         NSP(10,1) = 1
         NSP(11,1) = 1
         NSP(12,1) = 1
         NSP(13,1) = 2
         NSP(14,1) = 1
         NSP(15,1) = 2
         NSP(16,1) = 3
         NSP(17,1) = 1
         NSP(18,1) = 1
         NSP(19,1) = 2
         NSP(20,1) = 2
         NSP(21,1) = 3
         NSP(22,1) = 3
         NSP(23,1) = 1
C
         LSP(1,1) = 0
         LSP(2,1) = 1
         LSP(3,1) = 1
         LSP(4,1) = 0
         LSP(5,1) = 2
         LSP(6,1) = 2
         LSP(7,1) = 1
         LSP(8,1) = 1
         LSP(9,1) = 3
         LSP(10,1) = 3
         LSP(11,1) = 4
         LSP(12,1) = 4
         LSP(13,1) = 2
         LSP(14,1) = 5
         LSP(15,1) = 2
         LSP(16,1) = 0
         LSP(17,1) = 5
         LSP(18,1) = 6
         LSP(19,1) = 3
         LSP(20,1) = 3
         LSP(21,1) = 1
         LSP(22,1) = 1
         LSP(23,1) = 7
C
         JSP(1,1) = 1
         JSP(2,1) = 3
         JSP(3,1) = 1
         JSP(4,1) = 1
         JSP(5,1) = 5
         JSP(6,1) = 3
         JSP(7,1) = 3
         JSP(8,1) = 1
         JSP(9,1) = 7
         JSP(10,1) = 5
         JSP(11,1) = 9
         JSP(12,1) = 7
         JSP(13,1) = 5
         JSP(14,1) = 11
         JSP(15,1) = 3
         JSP(16,1) = 1
         JSP(17,1) = 9
         JSP(18,1) = 13
         JSP(19,1) = 7
         JSP(20,1) = 5
         JSP(21,1) = 3
         JSP(22,1) = 1
         JSP(23,1) = 15
      ELSE
C-----neutrons
         NSP(1,2) = 1
         NSP(2,2) = 1
         NSP(3,2) = 1
         NSP(4,2) = 2
         NSP(5,2) = 1
         NSP(6,2) = 1
         NSP(7,2) = 2
         NSP(8,2) = 2
         NSP(9,2) = 1
         NSP(10,2) = 1
         NSP(11,2) = 1
         NSP(12,2) = 2
         NSP(13,2) = 1
         NSP(14,2) = 3
         NSP(15,2) = 2
         NSP(16,2) = 1
         NSP(17,2) = 2
         NSP(18,2) = 1
         NSP(19,2) = 1
         NSP(20,2) = 3
         NSP(21,2) = 2
         NSP(22,2) = 3
         NSP(23,2) = 2
         NSP(24,2) = 1
         NSP(25,2) = 1
         NSP(26,2) = 3
         NSP(27,2) = 2
         NSP(28,2) = 4
         NSP(29,2) = 3
         NSP(30,2) = 2
C
         LSP(1,2) = 0
         LSP(2,2) = 1
         LSP(3,2) = 1
         LSP(4,2) = 0
         LSP(5,2) = 2
         LSP(6,2) = 2
         LSP(7,2) = 1
         LSP(8,2) = 1
         LSP(9,2) = 3
         LSP(10,2) = 3
         LSP(11,2) = 4
         LSP(12,2) = 2
         LSP(13,2) = 4
         LSP(14,2) = 0
         LSP(15,2) = 2
         LSP(16,2) = 5
         LSP(17,2) = 3
         LSP(18,2) = 5
         LSP(19,2) = 6
         LSP(20,2) = 1
         LSP(21,2) = 3
         LSP(22,2) = 1
         LSP(23,2) = 4
         LSP(24,2) = 7
         LSP(25,2) = 6
         LSP(26,2) = 2
         LSP(27,2) = 4
         LSP(28,2) = 0
         LSP(29,2) = 2
         LSP(30,2) = 5
C
         JSP(1,2) = 1
         JSP(2,2) = 3
         JSP(3,2) = 1
         JSP(4,2) = 1
         JSP(5,2) = 5
         JSP(6,2) = 3
         JSP(7,2) = 3
         JSP(8,2) = 1
         JSP(9,2) = 7
         JSP(10,2) = 5
         JSP(11,2) = 9
         JSP(12,2) = 5
         JSP(13,2) = 7
         JSP(14,2) = 1
         JSP(15,2) = 3
         JSP(16,2) = 11
         JSP(17,2) = 7
         JSP(18,2) = 9
         JSP(19,2) = 13
         JSP(20,2) = 3
         JSP(21,2) = 5
         JSP(22,2) = 1
         JSP(23,2) = 9
         JSP(24,2) = 15
         JSP(25,2) = 11
         JSP(26,2) = 5
         JSP(27,2) = 7
         JSP(28,2) = 1
         JSP(29,2) = 3
         JSP(30,2) = 11
      ENDIF
C
C-----no BCS blocking in the first run
C
      IBLk(I3) = 0
      NBL(I3) = 0
      LBL(I3) = 0
      JBL(I3) = 0
      bcsrerun = 0
C
C-----set number of hole states to be included in the BCS calculations
C
      IF (I3.EQ.1) THEN
         nnucl = Nz
         nspl = 23
      ENDIF
      IF (I3.EQ.2) THEN
         nnucl = INT(Amass) - Nz
         nspl = 30
      ENDIF
      numnuc = 0
      DO n = 1, nspl
         numnuc = numnuc + JSP(n,I3) + 1
         IF (numnuc.GT.nnucl) GOTO 100
      ENDDO
  100 nhx = n + 1
  200 NHOle(I3) = nhx
C
      DO n = 1, nhx
C--------external energy is taken if ESP .ne. 0.0 (with rerunning - only
C--------internal) internal energy is taken if ESP .eq. 0.0 (spherical
C--------Nilsson-HO)
         np = INDF(NSP(n,I3),LSP(n,I3),JSP(n,I3))
         iocc(np) = 1
         IF (ESP(n,I3).EQ.0.0D0) THEN
            nl = 2*(NSP(n,I3) - 1) + LSP(n,I3)
            all = vll(nl + 1,I3)
            ESP(n,I3) = EHO(NSP(n,I3),LSP(n,I3),JSP(n,I3),HOMega,all,
     &                  als)
            ESP(n,I3) = ESP(n,I3) + e0
         ENDIF
C--------next 2 lines can be uncommented to print s.p.l. without pairing
C        WRITE (6, 601) N, NSP(N,I3), LSP(N,I3), JSP(N,I3), ESP(N,I3)
C        601    FORMAT(I5,3I4,'/2',F8.3)
      ENDDO
C
      CALL BCS(xnp,GAP(I3),I3,nhx,chemic)
C
C-----find out the Fermi level
      numnuc = 0
      DO n = 1, nspl
         numnuc = numnuc + JSP(n,I3) + 1
         IF (numnuc.GE.nnucl) GOTO 300
      ENDDO
  300 ifermi = n
      IF (nhx.LT.ifermi + 1) nhx = ifermi + 1
C-----set BCS blocking
      IF (MOD(nnucl,2).NE.0 .AND. bcsrerun.EQ.0.0D0) THEN
         IBLk(I3) = ifermi
         NBL(I3) = NSP(IBLk(I3),I3)
         LBL(I3) = LSP(IBLk(I3),I3)
         JBL(I3) = JSP(IBLk(I3),I3)
C--------reset to 0 s.p.l. energies before reruning BCS
         DO n = 1, nhx
            ESP(n,I3) = 0.0
         ENDDO
         bcsrerun = 1.0
C--------rerun BCS
         GOTO 200
      ENDIF
      k = nhx
      npar = -1
      DO nq = 0, nqx
         npar = -npar
         IF (npar.LT.0) lmin = 1
         IF (npar.GT.0) lmin = 0
         lmax = nq
         DO l = lmin, lmax, 2
            ltw = l + l
            jtw = ltw - 3
            DO is = 1, 2
               jtw = jtw + 2
               IF (jtw.GE.0) THEN
                  nr = (nq - l)/2 + 1
                  np = INDF(nr,l,jtw)
                  IF (iocc(np).EQ.0) THEN
                     k = k + 1
                     NSP(k,I3) = nr
                     LSP(k,I3) = l
                     JSP(k,I3) = jtw
C--------------------here the first index of vll could be out of dimension
C--------------------thus vll dimension has been increased to vll(16,2)
                     all = vll(nq + 1,I3)
                     ESP(k,I3) = EHO(nr,l,jtw,HOMega,all,VLS(I3))
                     ESP(k,I3) = ESP(k,I3) + e0
                     EBCs(k,I3) = ESP(k,I3) - chemic
                     UAMp(k,I3) = 1.0
                     VAMp(k,I3) = 0.0
                  ENDIF
               ENDIF
            ENDDO
         ENDDO
      ENDDO
      NTOtal(I3) = k
      ndim = k
C
C-----<R**2> is calculated
C
      RMS(I3) = 0.0
      DO nl = 1, nhx
         pqn = 2*(NSP(nl,I3) - 1) + LSP(nl,I3) + 1.5
         RMS(I3) = RMS(I3) + pqn*FLOAT(JSP(nl,I3) + 1)*VAMp(nl,I3)**2
      ENDDO
      RMS(I3) = RMS(I3)*BST(I3)**2
      CALL ESORT(ndim,I3)
      END
C
C
      SUBROUTINE RESPNS
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C COMMON variables
C
      DOUBLE PRECISION ALPha(11,6), AN(6), ANGle(NDANG), BST(3), 
     &                 CLRn(11), DTHeta, DUMmy(1041), ECEntr(5), EOUtmi,
     &                 EOUtmx, ESP(500,2), ESTep, ETMax, ETMin, 
     &                 EXTcom(10), FACb, FAClog(500), FFAc1d(2), 
     &                 FFAc2d(2), FFAc3d(2), FFTot(10), FNOrm(6), 
     &                 FNQ(6,6), FQ(6), GAP(2), HOMega, Q0, QGRand, 
     &                 QMAx, QMIna, QMInb, QS1, QS2, QSTep, RAC, 
     &                 RHO(301,11), RHOb(301,11,2), RMS(3), ROPt, 
     &                 SREw(21), SREwl(21), SRNew(21), THEta1, THEta2, 
     &                 U9, VLS(2), WIDex
      INTEGER IA, IB, IC12x, IC1max, IC1mxr, IC1x, IC2max, IC2mxr, IC2x,
     &        ICC, ICMax, ID, IE, IG, JSP(500,2), KDEnvi, KEX3, 
     &        KEXcom(10), KRTmax, KRType, KTRl(10), L9(10), LSP(500,2), 
     &        NAVerg, NCHanl, NEBinx, NFAc12, NHOle(2), NLEv(301,11), 
     &        NN, NQ1x, NQ2x, NRMax(10), NSP(500,2), NTHeta, NTOtal(2), 
     &        NZ
      COMMON  RHO, SRNew, SREw, SREwl, AN, FNQ, FQ, FNOrm, ALPha, DUMmy,
     &        NLEv
      COMMON /CC    / THEta1, THEta2, NCHanl, NN, NZ, IC1mxr, IC2mxr, 
     &                KRType, KRTmax, KDEnvi, NTHeta, NEBinx, NAVerg, 
     &                NFAc12, KEX3
      COMMON /CCLQ  / IC1max, IC2max, IC1x, IC2x, IC12x, ICMax, NQ1x, 
     &                NQ2x
      COMMON /CONTRL/ EXTcom, KTRl, KEXcom
      COMMON /CRAC  / FAClog, RAC, U9, IA, IB, ICC, ID, IE, IG, L9
      COMMON /INELA / WIDex, ROPt, CLRn, ETMin, ETMax, RHOb, QS1, QS2, 
     &                Q0, FACb, FFTot, NRMax
      COMMON /SPPA  / HOMega, VLS, RMS, BST, GAP, NHOle, NTOtal
      COMMON /SPQN  / ESP, NSP, LSP, JSP
      COMMON /TRINTP/ DTHeta, ANGle, ESTep, EOUtmi, EOUtmx, ECEntr, 
     &                QSTep, QMIna, QMInb, QMAx, QGRand, FFAc1d, FFAc2d,
     &                FFAc3d
C
C Local variables
C
      DOUBLE PRECISION a1, basq, e0, e1, e2, eex, est(0:501), hhh
      REAL FLOAT
      INTEGER i, ic, iout4, ipr, k, krt, krtx, lp1, lt, ltmaxr, ne, neb,
     &        nlmax
C
C-----in RHO,NLEV,RHOB etc the increased argument NEB corresponds to
C-----increased excitation energy.
C
      iout4 = KTRl(4)
      a1 = NZ + NN
      ltmaxr = ICMax
      DO k = 1, 2
         RMS(k) = 0.0
         CALL SPLVL(k,a1,NZ,IOUt)
         ipr = 1
         IF (ipr.NE.0) THEN
            IF (k.EQ.1) THEN
               WRITE (6,99005)
99005          FORMAT (//10X,' PROTON SINGLE PARTICLE STATES:')
            ELSE
               WRITE (6,99010)
99010          FORMAT (//10X,'NEUTRON SINGLE PARTICLE STATES:')
            ENDIF
            nlmax = NTOtal(k)
            WRITE (6,99015) (ESP(i,k),NSP(i,k),LSP(i,k),JSP(i,k),i = 1,
     &                      nlmax)
99015       FORMAT (6(F9.2,3I3,'/2'))
         ENDIF
      ENDDO
      basq = (NZ*BST(1)**2 + NN*BST(2)**2)/a1
      BST(3) = SQRT(basq)
      RMS(3) = (RMS(1) + RMS(2))/a1
      RMS(1) = RMS(1)/NZ
      RMS(2) = RMS(2)/NN
      DO ne = 1, NEBinx
         DO lt = 1, ICMax
            RHO(ne,lt) = 0.0
            DO i = 1, 2
               RHOb(ne,lt,i) = 0.0
            ENDDO
         ENDDO
      ENDDO
      CALL INELAS(est,NN,NZ,NEBinx)
      DO lp1 = 1, ltmaxr
         DO ne = 1, NEBinx
            RHOb(ne,lp1,1) = RHO(ne,lp1)
         ENDDO
      ENDDO
      IF (KRType.GE.4) THEN
         RAC = a1/(a1 - 3.)
         IF (KRType.GE.4) THEN
            WRITE (6,*) '  ******  NOT USED  ******'
            STOP
         ENDIF
         DO lp1 = 1, ICMax
            DO ne = 1, NEBinx
               RHOb(ne,lp1,2) = RHO(ne,lp1)
            ENDDO
         ENDDO
      ENDIF
  100 IF (iout4.NE.0) THEN
         krtx = KTRl(7) + 1
         DO krt = 1, krtx
            IF (krt.EQ.1) THEN
               WRITE (6,99020) ETMin, ETMax, ESTep
99020          FORMAT (///10X,
     &                 'RPA RESPONSE FUNCTIONS (INELASTIC EXCITATION)'/
     &                 10X,'EXMIN=',F7.2,' EXMAX=',F7.2,' ESTEP=',F7.2/)
            ELSE
               WRITE (6,99025) ETMin, ETMax, ESTep
99025          FORMAT (///10X,'RPA RESPONSE FUNCTIONS (CHARGE EXCHANGE)'
     &                 /10X,'EXMIN=',F7.2,' EXMAX=',F7.2,' ESTEP=',
     &                 F7.2/)
            ENDIF
            WRITE (6,99030) (lt - 1,lt = 1,ltmaxr)
99030       FORMAT (5X,'EX',10(4X,I4,3X))
            DO neb = 1, NEBinx
               WRITE (6,99035) est(neb), 
     &                         (RHOb(neb,lt,krt),lt = 1,ltmaxr)
99035          FORMAT (' ',F6.2,10E11.4)
            ENDDO
         ENDDO
         WRITE (6,99040) (SRNew(lt),lt = 1,ltmaxr)
99040    FORMAT (///' ','SRNEW:',11E11.4)
         WRITE (6,99045) (SREw(lt),lt = 1,ltmaxr)
99045    FORMAT (' ','SREW :',11E11.4)
         IF (KTRl(8).EQ.( - 1)) GOTO 99999
      ENDIF
      e1 = ETMin + ESTep
      DO ic = 1, ICMax
         SREw(ic) = ESTep*0.5*(ETMin*RHOb(1,ic,1) + e1*RHOb(2,ic,1))
      ENDDO
      eex = ETMin - ESTep
      hhh = ESTep/3.
      WRITE (6,99055) (ic - 1,ic = 1,ICMax)
99055 FORMAT ('1'/5X,'EWSR AS A FUNCTION OF EXCITATION ENERGY'/' ',3X,
     &        'EX ',11(5X,I3,3X))
      DO ne = 2, NEBinx, 2
         eex = eex + 2.*ESTep
         IF (ne.NE.2) THEN
            e0 = ETMin + FLOAT(ne - 1)*ESTep
            e1 = e0 - ESTep
            e2 = e1 - ESTep
            DO ic = 1, ICMax
               SREw(ic) = SREw(ic)
     &                    + hhh*(e2*RHOb(ne - 2,ic,1) + 4.*e1*RHOb
     &                    (ne - 1,ic,1) + e0*RHOb(ne,ic,1))
            ENDDO
            WRITE (6,99060) eex, (SREw(ic),ic = 1,ICMax)
99060       FORMAT (' ',F6.2,11E11.4)
         ENDIF
      ENDDO
      KTRl(8) = -1
      GOTO 100
99999 END
C
C
      DOUBLE PRECISION FUNCTION EHO(N,L,Jtw,Hbo,All,Als)
C
C Dummy arguments
C
      DOUBLE PRECISION All, Als, Hbo
      INTEGER Jtw, L, N
C
C Local variables
C
      DOUBLE PRECISION ell, els, fjj, fso, rll
      INTEGER nq
      nq = 2*(N - 1) + L
      rll = L*(L + 1.)
      fjj = 0.25*(Jtw + 2.)*Jtw
      IF (L.EQ.0) THEN
         fso = 0.0
      ELSE
         fso = 0.5*(fjj - rll - 0.75)
      ENDIF
      ell = -All*(rll - 0.5*nq*(nq + 3.))
      els = -Als*fso
      EHO = Hbo*(nq + 1.5 + ell + els)
      END
C
C
      SUBROUTINE POLYNM(N,X,F)
C
C Dummy arguments
C
      INTEGER N
      DOUBLE PRECISION X
      DOUBLE PRECISION F(N)
C
C Local variables
C
      REAL FLOAT
      INTEGER i, k
      DOUBLE PRECISION y
      y = FLOAT(N - 1) - 2.*X
      F(1) = 1.
      F(2) = y/FLOAT(N - 1)
      DO i = 3, N
         k = i - 1
         F(i) = (y*F(i - 1)*FLOAT(2*k - 1) - F(i - 2)*FLOAT((k-1)*(N+k-1
     &          )))/FLOAT(k*(N - k))
      ENDDO
      END
C
C
      SUBROUTINE PNORM(Ndim,N,Fnorm,Fxn)
C
C Dummy arguments
C
      INTEGER N, Ndim
      DOUBLE PRECISION Fnorm(N), Fxn(Ndim,N)
C
C Local variables
C
      REAL FLOAT
      DOUBLE PRECISION flog(30), x
      INTEGER j, k, ntw

      flog(1) = 0.
      flog(2) = 0.
      ntw = 2*N
      DO k = 3, ntw
         flog(k) = flog(k - 1) + LOG(FLOAT(k - 1))
      ENDDO
      DO k = 1, N
         Fnorm(k) = EXP(flog(N + k) + flog(N - k + 1) - 2.*flog(N))
     &              /FLOAT(2*k - 1)
      ENDDO
      DO k = 1, N
         x = k - 1
         CALL POLYNM(N,x,flog)
         DO j = 1, N
            Fxn(k,j) = flog(j)
         ENDDO
      ENDDO
      END
C
C
      SUBROUTINE INVERT(Ndim,N,A,B)
C
C Dummy arguments
C
      INTEGER N, Ndim
      DOUBLE PRECISION A(Ndim,N), B(Ndim,N)
C
C Local variables
C
      DOUBLE PRECISION cik, hilf, pivot, s1, s2
      REAL FLOAT
      INTEGER i, indx, indy, ix, iy, j, k, l, m, merkx(20), merky(20)

      DO i = 1, N
         merkx(i) = 0
         merky(i) = 0
         DO l = 1, N
            B(i,l) = A(i,l)
         ENDDO
      ENDDO
      DO i = 1, N
         pivot = 0.
         DO ix = 1, N
            IF (merkx(ix).EQ.0) THEN
               DO iy = 1, N
                  IF (merky(iy).EQ.0) THEN
                     IF (ABS(B(ix,iy)).GT.ABS(pivot)) THEN
                        pivot = B(ix,iy)
                        indx = ix
                        indy = iy
                     ENDIF
                  ENDIF
               ENDDO
            ENDIF
         ENDDO
         IF (ABS(pivot).LE.0.0D0) GOTO 99999
         merkx(indx) = indy
         merky(indy) = indx
         B(indx,indy) = 1./pivot
         DO l = 1, N
            IF (l.NE.indx) THEN
               DO m = 1, N
                  IF (m.NE.indy) B(l,m) = B(l,m) - B(l,indy)*B(indx,m)
     &                /pivot
               ENDDO
            ENDIF
         ENDDO
         DO ix = 1, N
            IF (ix.NE.indx) B(ix,indx) = B(ix,indy)/pivot
         ENDDO
         DO iy = 1, N
            IF (iy.NE.indy) B(indx,iy) = -B(indx,iy)/pivot
         ENDDO
      ENDDO
      DO i = 2, N
         ix = i - 1
         IF (merkx(ix).NE.ix) THEN
            DO j = i, N
               iy = j
               IF (merky(iy).EQ.ix) GOTO 20
            ENDDO
   20       DO k = 1, N
               hilf = B(ix,k)
               B(ix,k) = B(iy,k)
               B(iy,k) = hilf
            ENDDO
            merkx(iy) = merkx(ix)
         ENDIF
         merkx(ix) = ix
      ENDDO
      DO i = 2, N
         ix = i - 1
         IF (merky(ix).NE.ix) THEN
            DO j = i, N
               iy = j
               IF (merky(iy).EQ.ix) GOTO 40
            ENDDO
   40       DO k = 1, N
               hilf = B(k,ix)
               B(k,ix) = B(k,iy)
               B(k,iy) = hilf
            ENDDO
            merky(iy) = merky(ix)
         ENDIF
         merkx(ix) = ix
      ENDDO
      s1 = 0.
      s2 = 0.
      DO i = 1, N
         DO k = 1, N
            cik = 0.
            DO l = 1, N
               cik = cik + A(i,l)*B(l,k)
            ENDDO
            IF (i.EQ.k) s1 = s1 + cik
            s2 = s2 + cik
         ENDDO
      ENDDO
      s2 = s2 - s1
      s1 = s1/FLOAT(N) - 1.
      IF (ABS(s1).GE.1.D-07 .OR. ABS(s2).GE.1.D-07) THEN
         WRITE (6,99005) s1, s2
99005    FORMAT (//' ','ERROR IN INVERT - S1,S2:',2E14.7)
      ENDIF
      DO i = 1, N
         DO j = 1, N
            A(j,i) = B(j,i)
         ENDDO
      ENDDO
99999 END
C
      SUBROUTINE SPECTR(Idimm,Kdim,Ngle,Nq12x,Nbinx,Lc12x,Cros,Crose,
     &                  Nejc)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C
C COMMON variables
C
      DOUBLE PRECISION ANGle(NDANG), CLRn(11), DTHeta, ECEntr(5), 
     &                 EOUtmi, EOUtmx, ESTep, ETMax, ETMin, EXTcom(10), 
     &                 FAC1d(2), FAC2d(2), FAC3d(2), FACb, FFTot(10), 
     &                 Q0, QGRand, QMAx, QMIna, QMInb, QS1, QS2, QSTep, 
     &                 RHOb(301,11,2), ROPt, THEta1, THEta2, WIDex
      INTEGER IC12x, IC1mx, IC1mxr, IC2mx, IC2mxr, ICMax, KDEnvi, KEX3, 
     &        KEXcom(10), KRTmax, KRType, KTRl(10), LC1mx, LC2mx, 
     &        NAVerg, NCHanl, NEBinx, NFAc12, NN, NQ1x, NQ2x, NRMax(10),
     &        NTHeta, NZ
      COMMON /CC    / THEta1, THEta2, NCHanl, NN, NZ, IC1mxr, IC2mxr, 
     &                KRType, KRTmax, KDEnvi, NTHeta, NEBinx, NAVerg, 
     &                NFAc12, KEX3
      COMMON /CCLQ  / LC1mx, LC2mx, IC1mx, IC2mx, IC12x, ICMax, NQ1x, 
     &                NQ2x
      COMMON /CONTRL/ EXTcom, KTRl, KEXcom
      COMMON /INELA / WIDex, ROPt, CLRn, ETMin, ETMax, RHOb, QS1, QS2, 
     &                Q0, FACb, FFTot, NRMax
      COMMON /TRINTP/ DTHeta, ANGle, ESTep, EOUtmi, EOUtmx, ECEntr, 
     &                QSTep, QMIna, QMInb, QMAx, QGRand, FAC1d, FAC2d, 
     &                FAC3d
C
C Dummy arguments
C
      INTEGER Idimm, Kdim, Lc12x, Nbinx, Nejc, Ngle, Nq12x
      DOUBLE PRECISION Cros(Nq12x,Lc12x,Kdim), Crose(Nbinx,Ngle,Idimm)
C
C Local variables
C
      DOUBLE PRECISION a1, a2, a3, adum(5,7), al1l2(7,7), amat(15,15), 
     &                 an, ay, aynorm, csfit(NDANG), delta, eout, f1(15)
     &                 , f11(2), f2(15), f21(2), fh, fl1(7,7), fl2(7,7),
     &                 fnl1(7), fnl2(7), fnq1(7), fnq2(7), fq1(6,6), 
     &                 fq2(6,6), gmat(15,15), piece, pxsec, q1, q2, 
     &                 qq(5), rb12, s1, s2, s3, sg(301), sigm, sn, sum, 
     &                 sumpx(301,2), x, x2, xl, xq, yl, zz(15)
      REAL FLOAT
      REAL hhh
      INTEGER i, ic, icp, icpmx, icpx, ier, iloc, j, jx, k, k1, k2, k2n,
     &        kc, kcp, kcpmx, kkp, kq, kr, krtx, kx, ky, l, l1p1, l2p1, 
     &        lc, lc1, lcp1, ln, m2, m2n, mx, my, n, n0, n1, n2, n2n, 
     &        na, nad, nangle, nc, ncm1, ndim, ne, neb, necs, nej, nemn,
     &        nemx, nep, nepp, nmax, nnur, np, npx, nq, nqx, nx, ny
      INTEGER MAX0, MIN0
      EQUIVALENCE (f2(1),f21)
      EQUIVALENCE (f1(1),f11)
      nej = 1
      IF (ZEJc(Nejc).EQ.1.0D0) nej = 2
      IF (ZEJc(Nejc).GT.1.0D0) THEN
         WRITE (6,*) 
     &' THIS IMPLEMENTATION OF TRISTAN IS NOT ABLE TO TREAT ANYTHING ELS
     &E AS NEUTRON OR PROTON IN THE INCOMMING CHANNEL'
         STOP
      ENDIF
C-----locate residue
      CALL WHERE(IZA(1) - IZAejc(Nejc),nnur,iloc)
      icpx = KTRl(1)
      icpmx = icpx + KTRl(7)
      kcpmx = icpmx + 1
      krtx = KTRl(7) + 1
      kkp = 1
      IF (KRType.GE.4) kkp = 2
      nangle = Ngle
      IF (IOUt.GT.3) WRITE (6,99005)
99005 FORMAT ('1'/20X,10('+'),5X,'MSDR - CROSS SECTIONS AND',
     &        ' ANALYZING POWERS',5X,10('+')///40X,
     &        'UNITS ARE MB AND %'/)
      delta = 2.*FLOAT(NAVerg)*ESTep
      CALL PNORM(6,NQ1x,fnq1,fq1)
      CALL PNORM(6,NQ2x,fnq2,fq2)
      ndim = 0
      DO i = 1, NQ1x
         jx = MIN0(i,NQ2x)
         DO j = 1, jx
            ndim = ndim + 1
         ENDDO
      ENDDO
      Nq12x = ndim
      nqx = (NQ1x*(NQ1x + 1))/2
      i = 0
      n0 = NQ1x - NQ2x + 1
      DO nx = 1, NQ1x
         n2n = MAX0(n0,nx)
         ny = 0
         DO n2 = n2n, NQ1x
            ny = ny + 1
            i = i + 1
            j = 0
            DO mx = 1, NQ1x
               m2n = MAX0(n0,mx)
               my = 0
               DO m2 = m2n, NQ1x
                  my = my + 1
                  j = j + 1
                  gmat(i,j) = 0.
                  DO kx = 1, NQ1x
                     k2n = MAX0(n0,kx)
                     ky = MAX0(0,k2n - n0)
                     sum = 0.
                     DO k2 = k2n, NQ1x
                        ky = ky + 1
                        sum = sum + fq2(ky,ny)*fq2(ky,my)
                     ENDDO
                     gmat(i,j) = gmat(i,j) + sum*fq1(kx,nx)*fq1(kx,mx)
                  ENDDO
               ENDDO
            ENDDO
         ENDDO
      ENDDO
C
C
      CALL INVERT(15,ndim,gmat,amat)
      IF (KEX3.NE.1) THEN
         CALL PNORM(7,IC1mx,fnl1,fl1)
         CALL PNORM(7,IC2mx,fnl2,fl2)
      ENDIF
      DO k = 1, Idimm
         DO j = 1, nangle
            DO i = 1, Nbinx
               Crose(i,j,k) = 0.
            ENDDO
         ENDDO
      ENDDO
      REWIND 16
      DO kr = 1, krtx
         DO na = 1, nangle
            IF (kr.EQ.1) THEN
C
C--------------one-step
C
               DO n1 = 1, NQ1x
                  DO icp = 1, icpx
C
C--------------------ICP = 1   left  x-sections
C--------------------ICP = 2   right x-section
C
                     READ (16) (Cros(n1,lc1,icp),lc1 = 1,LC1mx,KEX3)
                     IF (KEX3.NE.1) THEN
C
C-----------------------interpolation for L1
C
                        DO ic = 1, IC1mx
                           sum = 0.
                           j = 0
                           DO lc = 1, LC1mx, KEX3
                              j = j + 1
                              pxsec = LOG(Cros(n1,lc,icp))
                              sum = sum + pxsec*fl1(j,ic)
                           ENDDO
                           al1l2(ic,1) = sum/fnl1(ic)
                        ENDDO
                        DO lcp1 = 2, LC1mx, KEX3
                           x = FLOAT(lcp1 - 1)/2.
                           CALL POLYNM(IC1mx,x,f1)
                           sum = 0.
                           DO i = 1, IC1mx
                              sum = sum + f1(i)*al1l2(i,1)
                           ENDDO
                           Cros(n1,lcp1,icp) = EXP(sum)
                        ENDDO
                     ENDIF
                  ENDDO
               ENDDO
C
C--------------interpolation for Q1-dependence
C
               DO l1p1 = 1, LC1mx
                  DO icp = 1, icpx
                     DO n = 1, NQ1x
                        sum = 0.
                        DO k = 1, NQ1x
                           kq = NQ1x - k + 1
                           pxsec = LOG(Cros(kq,l1p1,icp))
                           sum = sum + pxsec*fq1(k,n)
                        ENDDO
                        al1l2(n,icp) = sum/fnq1(n)
                     ENDDO
                  ENDDO
                  q1 = ETMin - ESTep
                  DO ne = 1, Nbinx
                     q1 = q1 + ESTep
                     x = FLOAT(NQ1x - 1)*(q1 - ((-QMIna)))
     &                   /(((-QMAx)) - ((-QMIna)))
                     CALL POLYNM(NQ1x,x,f1)
                     DO icp = 1, icpx
                        kcp = kcpmx*icp - icpmx
                        sum = 0.
                        DO n1 = 1, NQ1x
                           sum = sum + al1l2(n1,icp)*f1(n1)
                        ENDDO
                        sg(ne) = EXP(sum)
                        Crose(ne,na,kcp) = Crose(ne,na,kcp) + sg(ne)
     &                     *RHOb(ne,l1p1,kkp)
                     ENDDO
                  ENDDO
               ENDDO
C
C
C--------------two - step
C
C
               IF (NCHanl.EQ.2) GOTO 20
            ENDIF
            DO nq = 1, Nq12x
               DO icp = 1, icpx
                  READ (16) (Cros(nq,KEX3*(ic-1) + 1,icp),ic = 1,IC12x)
                  IF (KEX3.NE.1) THEN
C
C--------------------interpolation for (L1,L2)-dependence
C
                     DO i = 1, IC1mx
                        DO j = 1, IC2mx
                           sum = 0.
                           DO k = 1, LC1mx, KEX3
                              kc = (k - 1)/KEX3 + 1
                              DO l = 1, LC2mx, KEX3
                                 lc = (l - 1)/KEX3 + 1
                                 nc = (k - 1)*IC2mx + l
                                 pxsec = LOG(Cros(nq,nc,icp))
                                 sum = sum + pxsec*fl1(kc,i)*fl2(lc,j)
                              ENDDO
                           ENDDO
                           al1l2(i,j) = sum/(fnl1(i)*fnl2(j))
                        ENDDO
                     ENDDO
                     lc = 0
                     DO l1p1 = 1, LC1mx
                        xl = FLOAT(l1p1 - 1)/2.
                        CALL POLYNM(IC1mx,xl,f1)
                        DO l2p1 = 1, LC2mx
                           yl = FLOAT(l2p1 - 1)/2.
                           CALL POLYNM(IC2mx,yl,f2)
                           lc = lc + 1
                           sum = 0.
                           DO k = 1, IC1mx
                              DO l = 1, IC2mx
                                 sum = sum + al1l2(k,l)*f1(k)*f2(l)
                              ENDDO
                           ENDDO
                           Cros(nq,lc,icp) = EXP(sum)
                        ENDDO
                     ENDDO
                  ENDIF
               ENDDO
            ENDDO
            lc = 0
            DO l1p1 = 1, LC1mx
               DO l2p1 = 1, LC2mx
                  lc = lc + 1
                  DO icp = 1, icpx
                     nq = 0
C
C--------------------interpolation for (Q1,Q2)-dependence
C
                     DO nx = 1, NQ1x
                        n2n = MAX0(n0,nx)
                        ny = 0
                        DO n2 = n2n, NQ1x
                           ny = ny + 1
                           nq = nq + 1
                           zz(nq) = 0.
                           DO kx = 1, NQ1x
                              ln = MAX0(n0,kx)
                              ky = MAX0(0,ln - n0)
                              DO l = ln, NQ1x
                                 ky = ky + 1
                                 kc = nqx - (l*(l - 1))/2 - kx + 1
                                 pxsec = LOG(Cros(kc,lc,icp))
                                 zz(nq) = zz(nq) + pxsec*fq1(kx,nx)
     &                              *fq2(ky,ny)
                              ENDDO
                           ENDDO
                        ENDDO
                     ENDDO
                     DO n1 = 1, Nq12x
                        amat(n1,icp) = 0.
                        DO n2 = 1, Nq12x
                           amat(n1,icp) = amat(n1,icp) + gmat(n1,n2)
     &                        *zz(n2)
                        ENDDO
                     ENDDO
                  ENDDO
                  q2 = ETMin
                  DO ne = 2, Nbinx
                     q2 = q2 + ESTep
                     x2 = FLOAT(NQ2x - 1)*(q2 - ((-QMInb)))
     &                    /(((-QMAx)) - ((-QMInb)))
                     CALL POLYNM(NQ2x,x2,f2)
                     q1 = ETMin - ESTep
                     DO i = 1, 2
                        DO nep = 1, ne
                           sumpx(nep,i) = 0.
                        ENDDO
                     ENDDO
                     i = 0
                     DO nx = 1, NQ1x
                        n2n = MAX0(n0,nx)
                        ny = 0
                        DO icp = 1, icpx
                           amat(nx,3 + icp) = 0.
                        ENDDO
                        DO n2 = n2n, NQ1x
                           i = i + 1
                           ny = ny + 1
                           DO icp = 1, icpx
                              amat(nx,3 + icp) = amat(nx,3 + icp)
     &                           + amat(i,icp)*f2(ny)
                           ENDDO
                        ENDDO
                     ENDDO
                     DO nep = 1, ne
                        q1 = q1 + ESTep
                        xq = FLOAT(NQ1x - 1)*(q1 - ((-QMIna)))
     &                       /(((-QMAx)) - ((-QMIna)))
                        CALL POLYNM(NQ1x,xq,f1)
                        nepp = ne - nep + 1
C-----------------------next two lines are for (p,alpha) calculations (orig. as
C-----------------------came from mu)
C-----------------------IF(KR.EQ.1)RB12=RHOB(NEP,L1P1,2)*RHOB(NEPP,L2P1,1)
C-----------------------IF(KR.EQ.2)RB12=RHOB(NEP,L1P1,1)*RHOB(NEPP,L2P1,2) next
C-----------------------line substitutes previous 2 according to Horst's
C-----------------------suggestion
                        rb12 = RHOb(nep,l1p1,krtx - kr + 1)
     &                         *RHOb(nepp,l2p1,kr)
                        DO icp = 1, icpx
                           sum = 0.
                           DO nx = 1, NQ1x
                              sum = sum + amat(nx,icp + 3)*f1(nx)
                           ENDDO
                           IF (sum.GT.100.D0) sum = 100.
                           sumpx(nep,icp) = sumpx(nep,icp)
     &                        + rb12*EXP(sum)
                        ENDDO
                     ENDDO
                     DO icp = 1, icpx
                        fh = 2.
                        npx = ne - 1
                        sum = sumpx(1,icp) + sumpx(ne,icp)
                        IF (ne.GE.3) THEN
                           IF (1.GE.2*MOD(ne,2)) THEN
                              sum = sumpx(1,icp) + sumpx(npx,icp)
     &                              + 1.5*(sumpx(npx,icp)
     &                              + sumpx(ne,icp))
                              npx = npx - 1
                           ENDIF
                           DO np = 2, npx
                              fh = 6. - fh
                              sum = sum + fh*sumpx(np,icp)
                           ENDDO
                        ENDIF
                        sumpx(icp,1) = sum*ESTep/3.
                     ENDDO
                     DO icp = 1, icpx
                        kcp = kcpmx*icp - icpmx + kr
                        Crose(ne,na,kcp) = Crose(ne,na,kcp)
     &                     + sumpx(icp,1)
                     ENDDO
                  ENDDO
               ENDDO
            ENDDO
            IF (NAVerg.GT.0) THEN
               IF (KTRl(7).EQ.1 .AND. kr.EQ.1) GOTO 50
C
C--------------averaging of the X - section and analyzing powers
C
               ncm1 = NCHanl - 1
               DO nc = 1, ncm1
                  kcp = kcpmx*(nc - 1)
                  DO icp = 1, icpmx
                     kcp = kcp + 1
                     DO ne = 1, Nbinx
                        nemn = MAX0(1,ne - NAVerg)
                        nemx = MIN0(Nbinx,nemn + NAVerg)
                        sum = 0.
                        DO neb = nemn, nemx
                           hhh = ESTep
                           IF (neb.EQ.nemn .OR. neb.EQ.nemx)
     &                         hhh = 0.5*hhh
                           sum = sum + hhh*Crose(neb,na,kcp)
                        ENDDO
                        sg(ne) = sum/delta
                     ENDDO
                     DO ne = 1, Nbinx
                        Crose(ne,na,kcp) = sg(ne)
                     ENDDO
                  ENDDO
               ENDDO
            ENDIF
            DO ne = 1, Nbinx
               DO k = 1, 2
                  kx = k*kcpmx
                  kcp = (k - 1)*kcpmx
                  DO icp = 1, icpmx
                     kcp = kcp + 1
                     Crose(ne,na,kx) = Crose(ne,na,kx)
     &                                 + Crose(ne,na,kcp)
                  ENDDO
               ENDDO
            ENDDO
   20       IF (IOUt.GT.3) THEN
               WRITE (6,99010) ANGle(na), delta, 
     &                         (FAC1d(n),FAC2d(n),FAC3d(n),n = 1,NFAc12)
99010          FORMAT (/'0',6X,
     &                 'X-SECTION AND ANALYZING POWER FOR THETA=',
     &                 F6.2/' ',5X,'AVERAGING:',F6.2,5X,'FACND:',
     &                 2(3F7.3,3X)//' ',2X,'EOUT',3X,'1-STEP',10X,
     &                 '2-STEP',11X,'TOTAL',32X,'1-STEP',10X,'2-STEP',
     &                 11X,'TOTAL')
               WRITE (66,99015) ANGle(na), Nbinx
99015          FORMAT (' THETA= ',F5.1,I5)
            ENDIF
            eout = EOUtmx + ESTep
            DO n = 1, 2
               f1(n) = 0.
               f2(n) = 0.
            ENDDO
            s3 = 0.
            a3 = 0.
            k1 = kcpmx
            k2 = 2*k1
            DO ne = 1, Nbinx
               eout = eout - ESTep
               IF (ne.NE.1) THEN
                  sigm = 0.5*(Crose(ne,na,k1) + Crose(ne,na,k2))
                  ay = 50.*(Crose(ne,na,k1) - Crose(ne,na,k2))/sigm
                  s1 = 0.5*(Crose(ne,na,1) + Crose(ne,na,k1 + 1))
                  s2 = 0.5*(Crose(ne,na,2) + Crose(ne,na,k1 + 2))
                  a1 = 50.*(Crose(ne,na,1) - Crose(ne,na,k1 + 1))/sigm
                  a2 = 50.*(Crose(ne,na,2) - Crose(ne,na,k1 + 2))/sigm
                  IF (KTRl(7).NE.0) THEN
                     s3 = 0.5*(Crose(ne,na,3) + Crose(ne,na,7))
                     a3 = 50.*(Crose(ne,na,3) - Crose(ne,na,7))/sigm
                  ENDIF
                  IF (NFAc12.NE.0) THEN
                     DO nad = 1, NFAc12
                        sn = FAC1d(nad)*s1 + FAC2d(nad)*s2
                        IF (KTRl(7).EQ.1) sn = sn + FAC3d(nad)*s3
                        aynorm = sigm/sn
                        f1(nad) = sn
                        an = FAC1d(nad)*a1 + FAC2d(nad)*a2
                        IF (KTRl(7).EQ.1) an = an + FAC3d(nad)*a3
                        f2(nad) = an*aynorm
                     ENDDO
                  ENDIF
                  IF (IOUt.GT.3) THEN
                     WRITE (6,99020) eout, s1, s2, s3, sigm, f11, a1, 
     &                               a2, a3, ay, f21
                     WRITE (66,99020) eout, s1, s2, sigm
                  ENDIF
                  necs = Nbinx - ne + 2
C-----------------store ddx to continuum
                  IF (IDNa(2*nej,2).NE.0 .AND. necs.LE.NEX(nnur) - 1)
     &                THEN
                     CSEa(necs,na,nej,1) = CSEa(necs,na,nej,1) + sigm
C-----------------discrete level region is not needed since spectra are
C-----------------constructed out of discrte levels
                  ELSEIF (IDNa(2*nej - 1,2).NE.0 .AND. necs.GE.NEX(nnur)
     &                    ) THEN
                     CSEa(necs,na,nej,1) = CSEa(necs,na,nej,1) + sigm
                  ENDIF
               ENDIF
            ENDDO
   50    ENDDO
      ENDDO
	CLOSE(66)
      REWIND 14
      k1 = kcpmx
C-----integrate angular distributions over angle (and energy)
C-----if ECIS active use only continuum part of the MSD spectrum
      nmax = Nbinx
      IF (DIRect.GT.0) nmax = MIN(Nbinx,NEX(nnur))
      DO ne = 1, nmax
         DO na = 1, nangle
            csfit(na) = CSEa(ne,nangle - na + 1,nej,1)
         ENDDO
         CALL LSQLEG(CANgler,csfit,nangle,qq,5,adum,ier)
         piece = 4.0*3.14159*qq(1)
         CSEmsd(ne,nej) = CSEmsd(ne,nej) + piece
         CSMsd(nej) = CSMsd(nej) + piece*DE
      ENDDO
C-----angular distributions integration *** done ***
99020 FORMAT (' ',F6.2,6E11.4,4X,6F9.4)
      END
C
C
C
C
C
      SUBROUTINE ACCUMSD(Nnuc,Nnur,Nejc)
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:mpu*
Ccc   *                         A C C U M S D                            *
Ccc   *                                                                  *
Ccc   * Distributes MSD spectrum over the residual nucleus continuum     *
Ccc   * assuming that the spin distribution is proportional to the       *
Ccc   * spin distribution of the 2-exciton states.                       *
Ccc   *                                                                  *
Ccc   * It distributes also MSD contribution over the discrete levels    *
Ccc   * in a very crude and arbitrary way, feeding essentially 2+ and    *
Ccc   * 3- (also 4+) states possibly close to the collective states      *
Ccc   * of these multipolarities.                                        *
Ccc   *                                                                  *
Ccc   * input:NNUC - index of the decaying nucleus                       *
Ccc   *       NNUR - index of the residual nucleus                       *
Ccc   *       NEJC - index of the ejectile                               *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * output:none                                                      *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C Dummy arguments
C
      INTEGER Nejc, Nnuc, Nnur
C
C Local variables
C
      DOUBLE PRECISION coef, csmsdl, dang, echannel, ecm, eemi, erecoil,
     &                 excnq, phdj(NDLW), pops, somj, swght, w, weight, 
     &                 wght(NDLV), xj, xnor
      REAL FLOAT
      INTEGER icsp, ie, il, irec, j, na, nangle, nexrt, next
      INTEGER INT
      IF (NEX(Nnuc).LT.1) THEN
         WRITE (6,*) ' HM !! THERE MUST BE SOMETHING WRONG !!!'
         WRITE (6,*) ' ACCUMSD COMPLAINS NEGATIVE ENERGY FOR'
         WRITE (6,*) ' NUCLEUS NNUC=', Nnuc, ' NEX(NNUC)=', NEX(Nnuc)
         WRITE (6,*) ' I BETTER  S T O P'
         STOP
      ENDIF
C-----
C----- CONTINUUM
C-----
      excnq = EX(NEX(Nnuc),Nnuc) - Q(Nejc,Nnuc)
C-----number of spectrum bins to continuum WARNING! might be negative!
      nexrt = INT((excnq - ECUt(Nnur))/DE + 1.0001)
C-----total number of bins
      next = INT(excnq/DE + 1.0001)
C-----calculate spin distribution for 1p-1h states
      SIG = 2*0.26*A(Nnur)**0.66666667
      somj = 0.0
      DO j = 1, NLW, LTUrbo
         xj = SQRT(FLOAT(j)**2 + XJLv(LEVtarg,0)**2)
         phdj(j) = 0.0
         w = (xj + 1.0)*xj/2./SIG
         IF (w.LE.50.D0) THEN
            phdj(j) = (2*xj + 1.)*EXP(( - w))
            somj = somj + phdj(j)
         ENDIF
      ENDDO
C-----distribution of the continuum MSD contribution -
C-----proportional to the p-h spin distribution shifted by the target
C-----ground state target spin XJLV(1,0)
      IF (nexrt.GT.0) THEN
         DO j = 1, NLW, LTUrbo
            xnor = 0.5*phdj(j)/somj
            DO ie = 1, nexrt
               pops = xnor*CSEmsd(nexrt - ie + 1,Nejc)
               POP(ie,j,1,Nnur) = POP(ie,j,1,Nnur) + pops
               POP(ie,j,2,Nnur) = POP(ie,j,2,Nnur) + pops
            ENDDO
         ENDDO
C--------add MSD contribution to the population spectra
C--------used for ENDF exclusive spectra
         IF (ENDf(1).EQ.1) THEN
            DO ie = 1, nexrt
               icsp = nexrt - ie + 1
C--------------DE
               POPcse(ie,Nejc,icsp,Nnur) = POPcse(ie,Nejc,icsp,Nnur)
     &            + CSEmsd(icsp,Nejc)
C--------------Correct last bin (not needed for POP as for this it is done at the end)
               IF (ie.EQ.1) POPcse(ie,Nejc,icsp,Nnur)
     &             = POPcse(ie,Nejc,icsp,Nnur) - 0.5*CSEmsd(icsp,Nejc)
C--------------DDX using portions
               POPcseaf(ie,Nejc,icsp,Nnur) = 1.0
C--------------DDX
C--------------Bin population by MSD (spin/parity integrated)
               POPbin(ie,Nnur) = CSEmsd(icsp,Nejc)
            ENDDO
         ENDIF
C--------storing continuum recoils
         IF (ENDf(1).GT.0) THEN
            nangle = NDANG
            dang = 3.14159/FLOAT(nangle - 1)
            coef = 2*3.14159*dang/DERec
            ecm = EINl - EIN
            DO ie = 1, nexrt
               echannel = (ie - 1)*DE*AEJc(Nejc)/A(1)
               DO na = 1, nangle
                  erecoil = ecm + echannel + 2*SQRT(ecm*echannel)
     &                      *CANgler(na)
                  irec = erecoil/DERec + 1.001
                  weight = (erecoil - (irec - 1)*DERec)/DERec
                  IF (irec + 1.GT.NDEREC) GOTO 20
                  csmsdl = CSEa(nexrt - ie + 1,na,Nejc,1)*SANgler(na)
     &                     *coef
                  RECcse(irec,ie,Nnur) = RECcse(irec,ie,Nnur)
     &               + csmsdl*(1.0 - weight)
                  RECcse(irec + 1,ie,Nnur) = RECcse(irec + 1,ie,Nnur)
     &               + csmsdl*weight
C
               ENDDO
   20       ENDDO
         ENDIF
      ENDIF
C-----
C----- DISCRETE LEVELS
C-----
C-----return if MSD to discrte levels not used (matrix IDNa)
      IF (IDNa(2*Nejc - 1,2).EQ.0) RETURN
C-----discrete level contribution to recoil spectra
C-----in case only discrete levels can be populated we set nexrt to 1
C-----(NOTE: it is usually negative in such a case)
      IF (nexrt.LE.0) nexrt = 1
      IF (ENDf(1).GT.0) THEN
         nangle = NDANG
         DO ie = nexrt, next
            echannel = (ie - 1)*DE*AEJc(Nejc)/A(1)
            DO na = 1, nangle
               erecoil = ecm + echannel + 2*SQRT(ecm*echannel)
     &                   *CANgler(na)
               irec = erecoil/DERec + 1.001
               weight = (erecoil - (irec - 1)*DERec)/DERec
               IF (irec + 1.GT.NDEREC) GOTO 50
               csmsdl = CSEa(ie,na,Nejc,1)*SANgler(na)*coef*DE
               IF (ie.EQ.nexrt) csmsdl = 0.5*csmsdl
               RECcse(irec,0,Nnur) = RECcse(irec,0,Nnur)
     &                               + csmsdl*(1.0 - weight)
               RECcse(irec + 1,0,Nnur) = RECcse(irec + 1,0,Nnur)
     &            + csmsdl*weight
            ENDDO
   50    ENDDO
      ENDIF
C-----distribution of the MSD contribution to discrete levels
C-----
C-----MSD contribution is integrated over the discrete level region and
C-----distributed among 2+, 3- and 4+ levels (or those close to such for
C-----noninteger spin nuclei) using arbitrary weights (most to 2+ and very
C-----little to 4+). Angular distributions for these levels are those
C-----provided by TRISTAN at the closest bin.
      csmsdl = 0.0
      DO ie = nexrt, next
         csmsdl = csmsdl + CSEmsd(ie,Nejc)*DE
      ENDDO
      csmsdl = csmsdl - 0.5*CSEmsd(nexrt,Nejc)*DE
      csmsdl = csmsdl - 0.5*CSEmsd(next,Nejc)*DE
      swght = 0.0
      DO il = 2, NLV(Nnur)
         wght(il) = 0.0
         eemi = excnq - ELV(il,Nnur)
         IF (eemi.LT.0.0D0) GOTO 100
         IF (ABS(XJLv(il,Nnur) - 2.D0).LT.0.6D0 .AND. LVP(il,Nnur).EQ.1)
     &       THEN
            wght(il) = 4.0/(ABS(ELV(il,Nnur) + QCC(1)) + 0.2)
            wght(il) = wght(il)**2
            swght = swght + wght(il)
         ENDIF
         IF (ABS(XJLv(il,Nnur) - 3.D0).LT.0.6D0 .AND. LVP(il,Nnur)
     &       .EQ.( - 1)) THEN
            wght(il) = 2.0/(ABS(ELV(il,Nnur) + QCC(2)) + 0.2)
            wght(il) = wght(il)**2
            swght = swght + wght(il)
         ENDIF
         IF (ABS(XJLv(il,Nnur) - 4.D0).LT.0.6D0 .AND. LVP(il,Nnur).EQ.1)
     &       THEN
            wght(il) = 1.0/(ABS(ELV(il,Nnur) + QCC(2) - 1.) + 0.2)
            wght(il) = wght(il)**2
            swght = swght + wght(il)
         ENDIF
      ENDDO
  100 IF (swght.EQ.0.0D0) THEN
         WRITE (6,*) ' WARNING:'
         WRITE (6,*) ' WARNING: No level to put msd level contribution '
     &               , csmsdl, ' mb'
         WRITE (6,*) ' WARNING: Load everything to the ground state '
         WRITE (6,*) ' WARNING: Ang. dist. of discrete levels ignored'
         WRITE (6,*) ' WARNING:'
         POPlv(1,Nnur) = POPlv(1,Nnur) + csmsdl
         RETURN
      ENDIF
      csmsdl = csmsdl/swght
      DO il = 2, NLV(Nnur)
         eemi = excnq - ELV(il,Nnur)
         IF (eemi.LT.0.0D0) RETURN
         POPlv(il,Nnur) = POPlv(il,Nnur) + csmsdl*wght(il)
         CSDirlev(il,Nejc) = CSDirlev(il,Nejc) + csmsdl*wght(il)
C--------Store ang. distr. for discrete levels. For each level shape of the closest CSEa
C--------bin is taken and normalized with level population  csmsdl*wght(il)
C--------Find the closest bin
         ie = eemi/DE + 1.5
         ie = MIN(ie,next)
C--------Normalization factor
         IF (CSEmsd(ie,Nejc).NE.0) THEN
            xnor = csmsdl*wght(il)/CSEmsd(ie,Nejc)
         ELSE
            xnor = 0.0
         ENDIF
C--------Add MSD transitions to discrete levels to the population spectrum
C--------used for the ENDF exclusive spectra
C--------This is actually not needed since MSD contribution to discrete levels
C--------is done through the CSDirlev and CSAlev
C        POPcse(0,nejc,ie,Nnur) = POPcse(0,nejc,ie,Nnur) +
C    &                            csmsdl*wght(il)/DE
C--------Store ang. dist.
         DO na = 1, NDANG
            CSAlev(na,il,Nejc) = CSAlev(na,il,Nejc)
     &                           + xnor*CSEa(ie,na,Nejc,1)
         ENDDO
      ENDDO
      END
