Ccc   * $Rev: 2526 $
Ccc   * $Author: shoblit $
Ccc   * $Date: 2012-02-09 21:34:11 +0100 (Do, 09 Feb 2012) $
C
      SUBROUTINE TRISTAN(Nejc,Nnuc,L1maxm,Qm,Qs,Xsinl)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: AI, ALSin, AR, BET2in, DTHeta, EOUtmi, EOUtmx, ESTep, 
     &          ETMax, ETMin, FACb, HOMin, Q0, QGRand, QMAx, QMIna, 
     &          QMInb, QS1, QS2, QSTep, RAC, RI, ROPt, RR, THEta1, 
     &          THEta2, U0, U9, W0, WIDex, WIDexin
      REAL*8, DIMENSION(NDAngecis) :: ANGle
      REAL*8, DIMENSION(8,8) :: CLRn, CNOrin, EFItin
      REAL*8, DIMENSION(30,49,2*NDAngecis) :: CROs1, CROs2
      REAL*8, DIMENSION(5) :: ECEntr
      REAL*8, DIMENSION(10) :: EXTcom, FFTot
      REAL*8, DIMENSION(500) :: FAClog
      REAL*8, DIMENSION(2) :: FFAc1d, FFAc2d, FFAc3d, GAPin, GRIn
      INTEGER :: IA, IB, IC12x, IC1max, IC1mxr, IC1x, IC2max, IC2mxr, 
     &           IC2x, ICC, ICMax, ID, IE, IG, KDEnvi, KEX3, KRTmax, 
     &           KRType, NAVerg, NCHanl, NEBinx, NFAc12, NN, NQ1x, NQ2x, 
     &           NTHeta, NZ
      INTEGER, DIMENSION(10) :: KEXcom, KTRl, L9, NRMax
      REAL*8, DIMENSION(3*(NDEx + 25),11,2) :: RHOb
      REAL*8, DIMENSION(12*NDAngecis) :: WR1
      REAL*8, DIMENSION(144*NDAngecis) :: WR2
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
      COMMON /TRINP / WIDexin, GAPin, HOMin, ALSin, EFItin, CNOrin, 
     &                BET2in, GRIn
      COMMON /TRINTP/ DTHeta, ANGle, ESTep, EOUtmi, EOUtmx, ECEntr, 
     &                QSTep, QMIna, QMInb, QMAx, QGRand, FFAc1d, FFAc2d, 
     &                FFAc3d
      COMMON /U_OPT / U0, W0, RR, RI, AR, AI
C
C Dummy arguments
C
      INTEGER :: L1maxm, Nejc, Nnuc
      REAL*8 :: Qm, Qs, Xsinl
C
C Local variables
C
      REAL*8 :: a3, ap, at, eccm, elab, fn, q1, q2, ri0, rr0
      REAL*8, DIMENSION(2*NDEx,NDAngecis,NDAngecis) :: crose
      REAL :: FLOAT
      INTEGER :: i1, i2, ic, ic1, ic12, ic1xr, ic2, ic2xr, icp, icpx, 
     &           iout2, ka, kb, kread, l1, l12x, l1maxr, l2, l2maxm, 
     &           l2maxr, mtb, n, n1, n12, n1x, n2, na, nangle, nb, ne, 
     &           nlr, nnb, np, npb, nq, nq12x, nwr1, nwr2, nzb
      INTEGER :: INT
C
C*** End of declarations rewritten by SPAG
C
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
CCC   *       XSinl  - Calculated inelastic cross section              *
CCC   *       TAPE15 - ORION results                                   *
CCC   *                                                                *
CCC   *                                                                *
CCC   * output:none                                                    *
CCC   *                                                                *
CCC   * author: H.Lenske, H. Wienke                                    *
CCC   *                                                                *
CCC   ******************************************************************
CCC
 
      Xsinl = 0.D0
C
C-----L1MAXM - maximum l transfer
C-----QM - maximum energy loss
C-----QS - step in energy loss triangle
C
C
C
      QMAx = Qm
      QSTep = Qs
      OPEN(16,FILE = 'TAPE16',STATUS = 'UNKNOWN',FORM = 'UNFORMATTED')
      IF(IOUt.GT.3)OPEN(66,FILE = 'TAPE66',STATUS = 'UNKNOWN')
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
      nangle = NDAng
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
 
      IF(BET2in.EQ.0.0D+0)BET2in = DEF(1,Nnuc)
      WRITE(8,*)
      WRITE(8,
     &    '('' beta_2 deformation in Nilsson Hamiltonian (MSD)'', F6.3)'
     &    )BET2in
 
      DO na = 1, nangle
        ANGle(na) = ANGles(na)
      ENDDO
      elab = EIN*(A(Nnuc) + AEJc(Nejc))/A(Nnuc)
C-----energy step in spectra
C     ESTep = DE
C-----Use half of the step in MSD calculations to reduce fluctuations
      ESTep = DE/2.0
C-----experimental energy resolution
      WIDex = WIDexin
      QMIna = 0.
      QMInb = 0.
      QMAx = -QMAx
      QGRand = 0.
      IF(THEta1.LE.0.D0)THEta1 = ANGle(1)
      IF(THEta2.LE.0.D0)THEta2 = ANGle(nangle - 1)
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
      IF(ETMax.GT.eccm)ETMax = eccm - ESTep
      NEBinx = (ETMax - ETMin)/ESTep + 1.2
      EOUtmx = ESTep*FLOAT(NEBinx)
      EOUtmi = EOUtmx - ESTep*FLOAT(NEBinx)
      WRITE(8,1010)
 1010 FORMAT('1'//11X,10('*'),6X,
     &       'MSDR CALCULATION OF CONTINUOUS SPECTRA',6X,10('*')//34X,
     &       '(ON PROGRAM TRISTAN )'/34X,'   (V2.0, OCT.94)'//)
      mtb = nnb + nzb
      FACb = (1878./197.33**2)*FLOAT(mtb + npb)/FLOAT(npb*mtb)
      RR = a3*rr0
      RI = a3*ri0
      IF(KTRl(3).NE.1)THEN
        nwr1 = (l1maxr/KEX3 + 1)*nangle
        nwr2 = (l2maxr/KEX3 + 1)*nwr1
        IF(nwr1.GT.12*NDAng)THEN
          WRITE(8,*)' INSUFFICIENT DIMENSION OF THE WR1 ARRAY IN'
          WRITE(8,*)' TRISTAN. MUST BE ', nwr1, ' AT LEAST'
          IF(nwr2.GT.144*NDAng)THEN
            WRITE(8,*)' '
            WRITE(8,*)' INSUFFICIENT DIMENSION OF THE WR2 ARRAY IN'
            WRITE(8,*)' TRISTAN. MUST BE ', nwr2, ' AT LEAST'
          ENDIF
          STOP 'ERROR IN WR1 WR2 DIMENSIONS IN TRISTAN'
        ENDIF
CCCCCC
        REWIND(15)
        REWIND(16)
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
    5   DO ne = 1, nq12x
CCCCCC
CCCCC       NLR=1,2 refers to cross sections, analyzing powers resp.
CCCCCC
          DO nlr = 1, icpx
            IF(kread.NE.1)THEN
              READ(15,*)(WR1(n),n = 1,nwr1)
C                 modiffication to run formated orion output
C                 READ(15,3434)(WR1(N),N=1,NWR1)
              IF(ne.LE.NQ1x)THEN
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
            IF(NCHanl.NE.2)THEN
              IF(kread.EQ.0)READ(15,*)(WR2(n),n = 1,nwr2)
C                 modiffication to run formated orion output
C                 IF(KREAD.EQ.0)READ(15,3434)(WR2(N),N=1,NWR2)
C                 IF(kread.EQ.1)READ(16)(WR2(n), n = 1, nwr2)
              n2 = 0
              DO ic1 = 1, ic1xr
                IF(ic1.GT.IC1x)EXIT
                DO ic2 = 1, ic2xr
                  ic12 = (ic1 - 1)*IC2x + ic2
                  DO na = 1, nangle
                    nb = na + (nlr - 1)*nangle
                    n2 = n2 + 1
                    IF(ic2.LE.IC2x)CROs2(ne,ic12,nb) = WR2(n2)
                  ENDDO
                ENDDO
              ENDDO
            ENDIF
          ENDDO
        ENDDO
C
C------print input cross sections and polarizations
C
        IF(iout2.NE.0)THEN
          IF(kread.EQ.0)THEN
            DO icp = 1, icpx
              ka = nangle*(icp - 1) + 1
              kb = ka - 1 + nangle
              IF(icp.EQ.1)WRITE(8,1020)(ANGle(n),n = 1,nangle)
 1020         FORMAT(//20X,'1-STEP INPUT LEFT CROSS SECTIONS'//' ',
     &               ' L1',2X,'Q1',2X,9F12.2)
              IF(icp.EQ.2)WRITE(8,1030)(ANGle(n),n = 1,nangle)
 1030         FORMAT(//20X,'1-STEP INPUT RIGHT CROSS SECTIONS'//' ',
     &               ' L1',2X,'Q1',2X,9F12.2)
              DO i1 = 1, IC1x
                l1 = KEX3*(i1 - 1)
                q1 = QMAx - QSTep
                DO nq = 1, NQ1x
                  q1 = q1 + QSTep
                  WRITE(8,1040)l1, q1, (CROs1(nq,i1,na),na = ka,kb)
 1040             FORMAT(' ',I3,F6.2,9E12.5)
                ENDDO
              ENDDO
            ENDDO
            IF(NCHanl.EQ.2)GOTO 10
          ENDIF
          DO icp = 1, icpx
            WRITE(8,1090)
            ka = nangle*(icp - 1) + 1
            kb = ka - 1 + nangle
            IF(icp.EQ.1)WRITE(8,1050)(ANGle(n),n = 1,nangle)
 1050       FORMAT(//20X,'2-STEP INPUT LEFT CROSS SECTIONS'//' ',
     &             ' L1 L2  Q1',4X,'Q2  ',9F12.2)
            IF(icp.EQ.2)WRITE(8,1060)(ANGle(n),n = 1,nangle)
 1060       FORMAT(//20X,'2-STEP INPUT RIGHT CROSS SECTIONS'//' ',
     &             ' L1 L2  Q1',4X,'Q2  ',9F12.2)
            ic = 0
            DO i1 = 1, IC1x
              l1 = KEX3*(i1 - 1)
              DO i2 = 1, IC2x
                l2 = KEX3*(i2 - 1)
                ic = ic + 1
                q2 = QMAx - QSTep
                nq = 0
                WRITE(8,1070)
 1070           FORMAT(/)
                DO n2 = 1, NQ2x
                  q2 = q2 + QSTep
                  n1x = NQ1x - n2 + 1
                  q1 = q2 - QSTep
                  DO n1 = 1, n1x
                    q1 = q1 + QSTep
                    nq = nq + 1
                    WRITE(8,1080)l1, l2, q1, q2, 
     &                           (CROs2(nq,ic,n),n = ka,kb)
 1080               FORMAT(' ',2I3,2F6.1,9E12.5)
                  ENDDO
                ENDDO
              ENDDO
            ENDDO
          ENDDO
          WRITE(8,1090)
        ENDIF
   10   DO na = 1, nangle
          IF(kread.NE.1)THEN
            DO n1 = 1, NQ1x
              DO icp = 1, icpx
                nb = na + (icp - 1)*nangle
                WRITE(16)(CROs1(n1,ic1,nb),ic1 = 1,IC1x)
              ENDDO
            ENDDO
          ENDIF
          DO n12 = 1, nq12x
            DO icp = 1, icpx
              nb = na + (icp - 1)*nangle
              WRITE(16)(CROs2(n12,ic12,nb),ic12 = 1,IC12x)
            ENDDO
          ENDDO
        ENDDO
        IF(KTRl(7).NE.0)THEN
          IF(kread.NE.1)THEN
            kread = 1
            GOTO 5
          ENDIF
        ENDIF
      ENDIF
      CALL RESPNS
      IF(KTRl(3).EQ.0)CALL SPECTR(nangle,2,nangle,nq12x,NEBinx,l12x,
     &                            CROs2,crose,Nejc,Xsinl)
      CLOSE(16)
 1090 FORMAT('1')
      END SUBROUTINE TRISTAN
 
!---------------------------------------------------------------------------
C
C
      SUBROUTINE INELAS(Est,Nn,Nz,Nebins)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: ALSin, AU, AW, BET2in, DTHeta, EOUtmi, EOUtmx, ESTep, 
     &          ETMax, ETMin, FACb, HOMega, HOMin, Q0, QGRand, QMAx, 
     &          QMIna, QMInb, QS1, QS2, QSTep, RAC, RI, ROPt, RR, U0, 
     &          U9, W0, WIDex, WIDexin
      REAL*8, DIMENSION(NDAngecis) :: ANGle
      REAL*8, DIMENSION(500,14,2) :: ASP
      REAL*8, DIMENSION(3*(NDEx + 25),8,8) :: BETa, RHO
      REAL*8, DIMENSION(3) :: BST, RMS
      REAL*8, DIMENSION(2) :: BST1, FFAc1d, FFAc2d, FFAc3d, GAP, GAPin, 
     &                        GRIn, VLS
      REAL*8, DIMENSION(8,8) :: CLRn, CNOrin, EFItin
      REAL*8, DIMENSION(500,2) :: EBCs, ESP, UAMp, VAMp
      REAL*8, DIMENSION(5) :: ECEntr
      REAL*8, DIMENSION(500) :: FAClog
      REAL*8, DIMENSION(10) :: FFTot
      REAL*8, DIMENSION(50) :: GSDm
      INTEGER :: IA, IB, IC, IC12x, IC1max, IC1x, IC2max, IC2x, ICMax, 
     &           ID, IE, IG, NQ1x, NQ2x
      INTEGER, DIMENSION(500,2) :: KSP, NSP
      INTEGER, DIMENSION(10) :: L9, NRMax
      INTEGER, DIMENSION(2) :: NHOle, NTOtal
      REAL*8, DIMENSION(3*(NDEx + 25),11,2) :: RHOb
      REAL*8, DIMENSION(21) :: SREw, SREwl, SRNew
      COMMON  RHO, SRNew, SREw, SREwl, GSDm, BETa
      COMMON /CCLQ  / IC1max, IC2max, IC1x, IC2x, IC12x, ICMax, NQ1x, 
     &                NQ2x
      COMMON /CRAC  / FAClog, RAC, U9, IA, IB, IC, ID, IE, IG, L9
      COMMON /EVBCS / EBCs, VAMp, UAMp
      COMMON /INELA / WIDex, ROPt, CLRn, ETMin, ETMax, RHOb, QS1, QS2, 
     &                Q0, FACb, FFTot, NRMax
      COMMON /SPPA  / HOMega, VLS, RMS, BST, GAP, NHOle, NTOtal
      COMMON /SPQN  / ESP, KSP, NSP
      COMMON /SPQN2 / ASP
      COMMON /TRINP / WIDexin, GAPin, HOMin, ALSin, EFItin, CNOrin, 
     &                BET2in, GRIn
      COMMON /TRINTP/ DTHeta, ANGle, ESTep, EOUtmi, EOUtmx, ECEntr, 
     &                QSTep, QMIna, QMInb, QMAx, QGRand, FFAc1d, FFAc2d, 
     &                FFAc3d
      COMMON /U_OPT / U0, W0, RR, RI, AU, AW
C
C Dummy arguments
C
      INTEGER :: Nebins, Nn, Nz
      REAL*8, DIMENSION(0:3*(NDEx + 25)) :: Est
C
C Local variables
C
      REAL*8 :: a1, a3, ad, aew, anz, api, bosc, ceff, cneg, cnorm, 
     &          cpos, cr2, de3, deqq, dnz, dr, dwex, dwsx, e, e0, efitx, 
     &          egr, em, emi, emisq, ep, epl, eplsq, eqq, eqqx, est3, 
     &          ext, f1, fe, ff1, fltwp1, fourpi, fpi, greenr, greenx, 
     &          greeny, hat, hcorr, homeb, phtrm, pxmd, pymd, qqi, qqr, 
     &          qqx, qqy, r, r1, rd, rdopt, rdsq, re1, re2, re3, resid, 
     &          rh0, rho1, rl, rmax, rmosc, rmsgs, rnorm, rp, rqr, rws, 
     &          rwsq, sumx, t1, t2, umatqq, veff, vnorm, w, wbcs, we
      REAL*8, DIMENSION(14) :: ah, ap
      REAL*8, DIMENSION(2,2) :: anp
      REAL*8, DIMENSION(8,8) :: cci, ccm, ccp, ccr, clex, clsc, cr1, 
     &                          cr3, dci, dcr, efit, xea, yea
      REAL*8, DIMENSION(2) :: ccpm, ddr, rrr
      REAL :: d1, hhh
      REAL*8 :: DWIDTH, VCC
      REAL*8, DIMENSION(0:10000) :: ess, wide
      REAL :: FLOAT
      INTEGER :: i, iminh, iminp, j, k, kc, kh, kmax, kp, krt, 
     &           krtx, kt, kth, ktp, ktp1, l, l1, l2, lm, lmax, lst, lt, 
     &           ltmax, ltmaxr, ltmin, ltp1, ltr, lttw, n, n1, n2, ne, 
     &           nebinx, nesx, nexnew, nh, nlhm, nlpm, nos1, nos2, np, 
     &           nr, nrad1, nrad2, nxmax
      INTEGER :: IABS, INT
      INTEGER, DIMENSION(14) :: jtw1, jtw2, lth, ltp
      INTEGER, DIMENSION(21) :: nconf
      REAL*8, DIMENSION(0:3*(NDEx + 25),8,8) :: rfqqr, rfqqx, rfqqy
      REAL*8, DIMENSION(3,2) :: rnp
      REAL*8 :: wgr, widas, widea, widgr, wqa, wqq, wqrex, xir, xneg, 
     &          xpos
C
C*** End of declarations rewritten by SPAG
C
C     All lines labeled with "! nilsson" fall under copyright of H.Wienke,
C     Geel 09/2005
C    &                 de3, deqq, dnz, dqqst(40000), dr, dwex, dwsx, e,  ! nilsson
C    &                 epl, eplsq, eqq, eqqst(40000), eqqx,ess(0:10000), ! nilsson
 
C     &                wqqst(40000), wqrex, x, xea(11), xir, xneg, xp,   ! nilsson
      EQUIVALENCE(BST(1),BST1)
      DATA rnp/1.2490D0, -0.5401D0, -0.9582D0, 1.2131D0, -0.4415D0, 
     &     0.8931D0/
      DATA anp/0.4899D0, -0.1236D0, 0.4686D0, 0.0741D0/
      DATA eqqx/80.0D0/
      IF(Nebins.GT.3*(NDEx + 25))THEN
        WRITE(8,*)
     &  ' ERROR:   INCREASE DIMENSIONS FOR RESPONSE FUNCTIONS ETC. TO: '
     &  , nebinx
        STOP
      ENDIF
C     WRITE(17,*)NEBINX,ICMAX
      fpi = 4.*PI
      fourpi = 1.0/fpi
C-----selfconsistent strength taken for the l=0 transfer field
      efit(1,1) = 0.0
      IF(EFItin(1,1).GT.0.0D0)efit(1,1) = EFItin(1,1)
      IF(EFItin(1,1).LT.0.0D0)efit(1,1) = 0.0
      efit(2,1) = (GDRpar(1,1)*GDRpar(2,1)*GDRpar(3,1) + GDRpar(4,1)
     &            *GDRpar(5,1)*GDRpar(6,1))
     &            /(GDRpar(2,1)*GDRpar(3,1) + GDRpar(5,1)*GDRpar(6,1))
      IF(EFItin(2,1).GT.0.0D0)efit(2,1) = EFItin(2,1)
      IF(EFItin(2,1).LT.0.0D0)efit(2,1) = 0.0
      efit(2,2) = (GDRpar(1,1)*GDRpar(2,1)*GDRpar(3,1) + GDRpar(4,1)
     &            *GDRpar(5,1)*GDRpar(6,1))
     &            /(GDRpar(2,1)*GDRpar(3,1) + GDRpar(5,1)*GDRpar(6,1))
      IF(EFItin(2,2).GT.0.0D0)efit(2,2) = EFItin(2,2)
      IF(EFItin(2,2).LT.0.0D0)efit(2,2) = 0.0
      efit(3,1) = -QCC(1)
      IF(EFItin(3,1).GT.0.0D0)efit(3,1) = EFItin(3,1)
      IF(EFItin(3,1).LT.0.0D0)efit(3,1) = 0.0
      efit(3,2) = -QCC(1)
      IF(EFItin(3,2).GT.0.0D0)efit(3,2) = EFItin(3,2)
      IF(EFItin(3,2).LT.0.0D0)efit(3,2) = 0.0
      IF((EFItin(3,2).LT.0.001).AND.(EFItin(3,2).GT. - .001))efit(3,2)
     &   = efit(3,1)
      efit(3,3) = -QCC(1)
      IF(EFItin(3,3).GT.0.0D0)efit(3,3) = EFItin(3,3)
      IF(EFItin(3,3).LT.0.0D0)efit(3,3) = 0.0
      IF((EFItin(3,3).LT.0.001).AND.(EFItin(3,3).GT. - .001))efit(3,3)
     &   = efit(3,1)
      efit(4,1) = -QCC(2)
      IF(EFItin(4,1).GT.0.0D0)efit(4,1) = EFItin(4,1)
      IF(EFItin(4,1).LT.0.0D0)efit(4,1) = 0.0
      efit(4,2) = -QCC(2)
      IF(EFItin(4,2).GT.0.0D0)efit(4,2) = EFItin(4,2)
      IF(EFItin(4,2).LT.0.0D0)efit(4,2) = 0.0
      IF((EFItin(4,2).LT.0.001).AND.(EFItin(4,2).GT. - .001))efit(4,2)
     &   = efit(4,1)
      efit(4,3) = -QCC(2)
      IF(EFItin(4,3).GT.0.0D0)efit(4,3) = EFItin(4,3)
      IF(EFItin(4,3).LT.0.0D0)efit(4,3) = 0.0
      IF((EFItin(4,3).LT.0.001).AND.(EFItin(4,3).GT. - .001))efit(4,3)
     &   = efit(4,1)
      efit(4,4) = -QCC(2)
      IF(EFItin(4,4).GT.0.0D0)efit(4,4) = EFItin(4,4)
      IF(EFItin(4,4).LT.0.0D0)efit(4,4) = 0.0
      IF((EFItin(4,4).LT.0.001).AND.(EFItin(4,4).GT. - .001))efit(4,4)
     &   = efit(4,1)
      efit(5,1) = 0.0
      IF(EFItin(5,1).GT.0.0D0)efit(5,1) = EFItin(5,1)
      IF(EFItin(5,1).LT.0.0D0)efit(5,1) = 0.0
      efit(5,2) = 0.0
      IF(EFItin(5,2).GT.0.0D0)efit(5,2) = EFItin(5,2)
      IF(EFItin(5,2).LT.0.0D0)efit(5,2) = 0.0
      IF((EFItin(5,2).LT.0.001).AND.(EFItin(5,2).GT. - .001))efit(5,2)
     &   = efit(5,1)
      efit(5,3) = 0.0
      IF(EFItin(5,3).GT.0.0D0)efit(5,3) = EFItin(5,3)
      IF(EFItin(5,3).LT.0.0D0)efit(5,3) = 0.0
      IF((EFItin(5,3).LT.0.001).AND.(EFItin(5,3).GT. - .001))efit(5,3)
     &   = efit(5,1)
      efit(5,4) = 0.0
      IF(EFItin(5,4).GT.0.0D0)efit(5,4) = EFItin(5,4)
      IF(EFItin(5,4).LT.0.0D0)efit(5,4) = 0.0
      IF((EFItin(5,4).LT.0.001).AND.(EFItin(5,4).GT. - .001))efit(5,4)
     &   = efit(5,1)
      efit(5,5) = 0.0
      IF(EFItin(5,5).GT.0.0D0)efit(5,5) = EFItin(5,5)
      IF(EFItin(5,5).LT.0.0D0)efit(5,5) = 0.0
      IF((EFItin(5,5).LT.0.001).AND.(EFItin(5,5).GT. - .001))efit(5,5)
     &   = efit(5,1)
      efit(6,1) = 0.0
      IF(EFItin(6,1).GT.0.0D0)efit(6,1) = EFItin(6,1)
      IF(EFItin(6,1).LT.0.0D0)efit(6,1) = 0.0
      efit(6,2) = 0.0
      IF(EFItin(6,2).GT.0.0D0)efit(6,2) = EFItin(6,2)
      IF(EFItin(6,2).LT.0.0D0)efit(6,2) = 0.0
      IF((EFItin(6,2).LT.0.001).AND.(EFItin(6,2).GT. - .001))efit(6,2)
     &   = efit(6,1)
      efit(6,3) = 0.0
      IF(EFItin(6,3).GT.0.0D0)efit(6,3) = EFItin(6,3)
      IF(EFItin(6,3).LT.0.0D0)efit(6,3) = 0.0
      IF((EFItin(6,3).LT.0.001).AND.(EFItin(6,3).GT. - .001))efit(6,3)
     &   = efit(6,1)
      efit(6,4) = 0.0
      IF(EFItin(6,4).GT.0.0D0)efit(6,4) = EFItin(6,4)
      IF(EFItin(6,4).LT.0.0D0)efit(6,4) = 0.0
      IF((EFItin(6,4).LT.0.001).AND.(EFItin(6,4).GT. - .001))efit(6,4)
     &   = efit(6,1)
      efit(6,5) = 0.0
      IF(EFItin(6,5).GT.0.0D0)efit(6,5) = EFItin(6,5)
      IF(EFItin(6,5).LT.0.0D0)efit(6,5) = 0.0
      IF((EFItin(6,5).LT.0.001).AND.(EFItin(6,5).GT. - .001))efit(6,5)
     &   = efit(6,1)
      efit(6,6) = 0.0
      IF(EFItin(6,6).GT.0.0D0)efit(6,6) = EFItin(6,6)
      IF(EFItin(6,6).LT.0.0D0)efit(6,6) = 0.0
      IF((EFItin(6,6).LT.0.001).AND.(EFItin(6,6).GT. - .001))efit(6,6)
     &   = efit(6,1)
      efit(7,1) = 0.0
      IF(EFItin(7,1).GT.0.0D0)efit(7,1) = EFItin(7,1)
      IF(EFItin(7,1).LT.0.0D0)efit(7,1) = 0.0
      efit(7,2) = 0.0
      IF(EFItin(7,2).GT.0.0D0)efit(7,2) = EFItin(7,2)
      IF(EFItin(7,2).LT.0.0D0)efit(7,2) = 0.0
      IF((EFItin(7,2).LT.0.001).AND.(EFItin(7,2).GT. - .001))efit(7,2)
     &   = efit(7,1)
      efit(7,3) = 0.0
      IF(EFItin(7,3).GT.0.0D0)efit(7,3) = EFItin(7,3)
      IF(EFItin(7,3).LT.0.0D0)efit(7,3) = 0.0
      IF((EFItin(7,3).LT.0.001).AND.(EFItin(7,3).GT. - .001))efit(7,3)
     &   = efit(7,1)
      efit(7,4) = 0.0
      IF(EFItin(7,4).GT.0.0D0)efit(7,4) = EFItin(7,4)
      IF(EFItin(7,4).LT.0.0D0)efit(7,4) = 0.0
      IF((EFItin(7,4).LT.0.001).AND.(EFItin(7,4).GT. - .001))efit(7,4)
     &   = efit(7,1)
      efit(7,5) = 0.0
      IF(EFItin(7,5).GT.0.0D0)efit(7,5) = EFItin(7,5)
      IF(EFItin(7,5).LT.0.0D0)efit(7,5) = 0.0
      IF((EFItin(7,5).LT.0.001).AND.(EFItin(7,5).GT. - .001))efit(7,5)
     &   = efit(7,1)
      efit(7,6) = 0.0
      IF(EFItin(7,6).GT.0.0D0)efit(7,6) = EFItin(7,6)
      IF(EFItin(7,6).LT.0.0D0)efit(7,6) = 0.0
      IF((EFItin(7,6).LT.0.001).AND.(EFItin(7,6).GT. - .001))efit(7,6)
     &   = efit(7,1)
      efit(7,7) = 0.0
      IF(EFItin(7,7).GT.0.0D0)efit(7,7) = EFItin(7,7)
      IF(EFItin(7,7).LT.0.0D0)efit(7,7) = 0.0
      IF((EFItin(7,7).LT.0.001).AND.(EFItin(7,7).GT. - .001))efit(7,7)
     &   = efit(7,1)
C
C-----check that energy interval is compatible with range of
C-----experimental ex used to determine coupl. const.
C
      efitx = -10.
      DO l = 1, ICMax
        DO k = 1, l
          efitx = MAX(efit(l,k),efitx)
        ENDDO
      ENDDO
      nebinx = MAX(INT((efitx+5.*WIDex)/ESTep + 1.),Nebins)
      IF(nebinx.GE.3*(NDEx + 25))THEN
        nexnew = INT(3*FLOAT(NDEx + 25)/FLOAT(nebinx)*NEX(1) - 1)
        WRITE(8,*)' '
        WRITE(8,*)'Insufficent dimensions for response function in'
        WRITE(8,*)'MSD-tristan.f  Possible solutions:'
        WRITE(8,*)'- decrease NEX in input to ', nexnew
        WRITE(8,*)'- increase NDEX in dimension.h to', nebinx/3 - 24, 
     &            ' and recompile the source'
        WRITE(8,*)'- start MSD at higher incident energy '
        STOP 'Insuficient dimensions in TRISTAN (see output)'
      ENDIF
      IF(nebinx.NE.Nebins.AND.IOUt.GT.3)WRITE(8,1010)nebinx*ESTep
 1010 FORMAT(/' >> LIMIT OF EX IS INCREASED TO:',F9.3,' (MEV) <<'/
     &       '    (USED TO DETERMINE THE COUPLING CONSTANTS)')
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
        DO kt = 1, ltmaxr
          xea(lt,kt) = 0.D0
          yea(lt,kt) = 0.D0
        ENDDO
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
      IF(ROPt.LE.0.0D0)ROPt = rd
      rdopt = rd/ROPt
      IF(IOUt.GT.3)WRITE(8,1020)rd, ad, rh0, ROPt, rdopt
 1020 FORMAT(//' ','RDENS=',F7.4,3X,'ADENS=',F7.4,3X,'RH0=',F7.4/' ',
     &       'ROPT =',F7.4,3X,'RATIO=',F7.4)
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
      IF(IOUt.GT.2)WRITE(8,1030)egr, wgr, widas, WIDex, e0, aew
 1030 FORMAT(/'  ENERGY DEPENDENT WIDTH OF RPA/QRPA STATES:'/
     &       '  E(GDR) :',F8.3,'   W(GDR)   :',F8.3,
     &       ' [MEV]'/'  WID(AS):',F8.3,'   WID(EXP.):',F8.3,
     &       ' [MEV]'/'  E0     :',F8.3,'   AEW      :',F8.3,' [MEV]')
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
        IF(nr.EQ.nxmax)f1 = 0.5*t1
        d1 = rh0/(1.0 + EXP((r-rd)/ad))
        rl = r*r
        DO l = 1, lmax
          GSDm(l) = GSDm(l) + rl*f1*d1
          rl = rl*r
        ENDDO
      ENDDO
C
      IF(IOUt.GT.2)WRITE(8,1040)
 1040 FORMAT(//8X,' RADIAL MOMENTS <R**L>/A OF THE G.S. DENSITY'/5X,
     &       '           ( UNITS: [FM**L/A] )             '/4X,'L',11X,
     &       'L  ',11X,'L+1',11X,'L+2',11X,'L+3',11X,'L+4')
      lst = 5
      DO l = 1, lmax, lst
        l1 = l
        l2 = MIN(lmax,l1 + lst - 1)
        IF(IOUt.GT.2)WRITE(8,1050)l - 1, (GSDm(n)*api,n = l1,l2)
 1050   FORMAT(I5,5E14.7)
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
      IF(IOUt.GT.3)THEN
        WRITE(8,1060)bosc, BST(3), bosc/BST(3), BST1
 1060   FORMAT(//'  OSCILLATOR LENGTH:'/'  B(DENSITY):',F7.3,
     &         '  B(NILSSON):',F7.3,'  RATIO:',F7.4/'  B(PROTON) :',
     &         F7.3,'  B(NEUTRON):',F7.3)
        WRITE(8,1070)homeb, HOMega, hcorr
 1070   FORMAT(/'  OSCILLATOR ENERGY:'/'  E(DENSITY):',F7.3,
     &         '  E(NILSSON):',F7.3,'  RATIO:',F7.4)
        WRITE(8,1080)(SQRT(RMS(i)),i = 1,3)
 1080   FORMAT(/'  <R**2> (IN [FM]) :'/'  PROTON    :',F7.3,
     &         '  NEUTRON   :',F7.3,'  TOTAL:',F7.4)
        WRITE(8,1090)vnorm
 1090   FORMAT(/'   STRENGTH OF THE MULTIPOLE-MULTIPOLE INTERACTION:'/
     &         '  VNORM     :',F7.3,' [MEV]')
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
            DO kt = 1, ICMax
              IF(ne.GE.1)RHO(ne,lt,kt) = 0.0
              rfqqr(ne,lt,kt) = 0.0
              rfqqx(ne,lt,kt) = 0.0
              rfqqy(ne,lt,kt) = 0.0
            ENDDO
            IF(krt.EQ.1.AND.ne.GE.1)THEN
              RHOb(ne,lt,1) = 0.0
              RHOb(ne,lt,2) = 0.0
            ENDIF
          ENDDO
        ENDDO
C
        DO lt = 1, ICMax
          DO kt = 1, ICMax
            clsc(lt,kt) = 0.
            clex(lt,kt) = 0.
            ccr(lt,kt) = 0.D0
            cci(lt,kt) = 0.D0
            dcr(lt,kt) = 0.D0
            dci(lt,kt) = 0.D0
            cr1(lt,kt) = 0.D0
            cr3(lt,kt) = 0.D0
            ccm(lt,kt) = 0.D0
            ccp(lt,kt) = 0.D0
          ENDDO
          nconf(lt) = 0.0
          SREw(lt) = 0.0                                           ! nilsson_newest
          SRNew(lt) = 0.0                                          ! nilsson_newest
          SREwl(lt) = 0.0                                          ! nilsson_newest
        ENDDO
        IF(krt.EQ.1)kmax = 2
        IF(krt.EQ.2)kmax = 1
C
C--------KRT=1 inelastic excitation
C--------KRT=2 charge exchange excitation for (n,p) reaction
C
        DO k = 1, kmax
          IF(krt.EQ.1)THEN
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
            DO i = 1, 14
              lth(i) = 0
              jtw1(i) = 0
              ah(i) = 0.0
            ENDDO
            nos1 = NSP(nh,kh)
            kth = KSP(nh,kh)
            iminh = (kth - 1)/2
C fill up the arrays lth[], ah[] and jtw1[]                              ! nilsson
            l = nos1
            i = nos1 + 1 - iminh
            DO WHILE ((l.GE.0).AND.(i.GT.0))
              lth(i) = l
              jtw1(i) = 2*l + 1
              ah(i) = ASP(nh,i,kh)
              IF(i.GT.1)THEN
                i = i - 1
                lth(i) = l
                jtw1(i) = 2*l - 1
                ah(i) = ASP(nh,i,kh)
              ENDIF
              i = i - 1
              l = l - 2
            ENDDO
            IF(VAMp(nh,kh).GE.0.01D0)THEN
              DO np = 1, nlpm
                DO i = 1, 14
                  ltp(i) = 0
                  jtw2(i) = 0
                  ap(i) = 0.0
                ENDDO
                IF(UAMp(np,kp).GE.0.01D0)THEN
                  wbcs = VAMp(nh,kh)*UAMp(np,kp) + UAMp(nh,kh)
     &                   *VAMp(np,kp)
                  IF(wbcs.GE.0.01D0)THEN
                    nos2 = NSP(np,kp)
                    ktp = KSP(np,kp)
                    iminp = (ktp - 1)/2
C fill up the arrays ltp[], ap[] and jtw2[]                              ! nilsson
                    l = nos2
                    i = nos2 + 1 - iminp
                    DO WHILE ((l.GE.0).AND.(i.GT.0))
                      ltp(i) = l
                      jtw2(i) = 2*l + 1
                      ap(i) = ASP(np,i,kp)
                      IF(i.GT.1)THEN
                        i = i - 1
                        ltp(i) = l
                        jtw2(i) = 2*l - 1
                        ap(i) = ASP(np,i,kp)
                      ENDIF
                      i = i - 1
                      l = l - 2
                    ENDDO
C
                    eqq = EBCs(nh,kh) + EBCs(np,kp)
                    IF(eqq.LE.eqqx)THEN
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
                        IF(ne.EQ.0.OR.ne.EQ.nebinx)we = de3
                        em = eqq - ess(ne)
                        ep = eqq + ess(ne)
                        fe = (wide(ne) - wqq)*(1.D0/em + 1.D0/ep)
                        deqq = deqq + we*fe
                      ENDDO
                      deqq = deqq/PI
C The following statements are commented out since wqqst, dqqst
C  and eqqst are never used (h.wienke).
C                              wqqst(kc) = wqq*2.D0
C                              dqqst(kc) = deqq
                      eqq = eqq + deqq
C                              eqqst(kc) = eqq
C
C-----------------------------end of energy shift
C
                      DO i = 1, nos1 + 1 - iminh
                        DO j = 1, nos2 + 1 - iminp
                          l1 = IABS(ltp(j) - lth(i))
                          l2 = IABS(jtw2(j) - jtw1(i))/2
                          IF((i.EQ.1).AND.(j.EQ.1))THEN
                            ltmin = MAX(l1,l2)
                          ELSEIF(MAX(l1,l2).LT.ltmin)THEN
                            ltmin = MAX(l1,l2)
                          ENDIF
                          IF(MOD(ltmin - l1,2).EQ.1)ltmin = ltmin + 1
                          IF((i.EQ.1).AND.(j.EQ.1))THEN
                            ltmax = MIN(ltp(j) + lth(i),
     &                              (jtw1(i) + jtw2(j))/2) + 1
                          ELSEIF((MIN(ltp(j)+lth(i),(jtw1(i)+jtw2(j))/2)
     &                           + 1).GT.ltmax)THEN
                            ltmax = MIN(ltp(j) + lth(i),
     &                              (jtw1(i) + jtw2(j))/2) + 1
                          ENDIF
                        ENDDO
                      ENDDO
                      ltmin = ltmin + 1
                      ltmax = MIN(ltmax,ltmaxr)
                      DO n1 = 1, 2, 1
                        kth = IABS(kth)
                        DO n2 = 1, 2, 1
                          ktp1 = (IABS(ktp - kth))/2 + 1
                          IF(ltmin.LE.ltmax)THEN
                            DO ltp1 = ltmin, ltmax, 2
                              lttw = 2*(ltp1 - 1)
C
C-----------------------------------calculate the 2-qp matrix elements
C-----------------------------------REDUQQ = reduced matrix element (squared)
C-----------------------------------PHTRM = radial  matrix element
C-----------------------------------UMATQQ = full    matrix element (squared)
                              sumx = 0.0D0
                              DO i = 1, nos1 + 1 - iminh
                                DO j = 1, nos2 + 1 - iminp
                                  IF(ABS(ah(i)*ap(j)).GE.0.00001D0)THEN
                                    IA = jtw1(i)
                                    IB = jtw2(j)
                                    IC = lttw
                                    ID = 1
                                    IE = -1
                                    IG = 0
                                    CALL CLEBTRI
                                    r1 = RAC
                                    r1 = (VCC(IB,IA,IC,ktp, - kth)
     &                                *( - 1)**((IA-kth)/2))*r1
                                    r1 = SQRT(FLOAT(IA + 1)
     &                                *FLOAT(IB + 1))*r1
                                    r1 = r1/SQRT(FLOAT(IC + 1))
                                    r1 = ah(i)*ap(j)*r1
                                    IF(ABS(r1).GT.1.D-10)THEN
                                      ltr = lttw/2
                                      nrad1 = (nos1 - lth(i))/2
                                      nrad2 = (nos2 - ltp(j))/2
                                      CALL RADIAL(bosc,phtrm,nrad1,
     &                                  lth(i),nrad2,ltp(j),ltr)
                                      sumx = sumx + r1*phtrm
                                    ENDIF
                                  ENDIF
                                ENDDO
                              ENDDO
                              IF(ltr.GT.0)sumx = sumx*vnorm/rws**ltr
                              IF(ltr.EQ.0)sumx = sumx*vnorm/rws**2
                              umatqq = (sumx*wbcs)**2
                              hat = 1.D0
                              umatqq = umatqq*hat*fourpi/FLOAT(lttw + 1)
 
                              IF(umatqq.GE.1.D-08)THEN
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
                                  greenr = (emi*pxmd + epl*pymd)*umatqq
                                  greenx = wqq*(pxmd - pymd)*umatqq
                                  greeny = (pxmd - pymd)*umatqq
C
C                                         real and imaginary parts of the 2-qp
C                                         response functions
                                  rfqqr(ne,ltp1,ktp1)
     &                              = rfqqr(ne,ltp1,ktp1) + greenr
                                  rfqqx(ne,ltp1,ktp1)
     &                              = rfqqx(ne,ltp1,ktp1) + greenx
                                  rfqqy(ne,ltp1,ktp1)
     &                              = rfqqy(ne,ltp1,ktp1) + greeny
                                ENDDO
C
C calculate the 2qp response function at the fitting energy
C
                                IF(efit(ltp1,ktp1).NE.0.0D0)THEN
                                  i = 1
                                  ext = efit(ltp1,ktp1) - ESTep/10.D0
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
     &                                *umatqq
                                    greenx = wqq*(pxmd - pymd)*umatqq
                                    greeny = (pxmd - pymd)*umatqq
C
C----------------------------------------real and imaginary parts of the 2-qp
C----------------------------------------response function
                                    IF(i.EQ.1)THEN
                                      cr1(ltp1,ktp1) = cr1(ltp1,ktp1)
     &                                  + greenr
                                      ccm(ltp1,ktp1) = ccm(ltp1,ktp1)
     &                                  + greenx + 0.5D0*WIDex*greeny
                                    ELSEIF(i.EQ.2)THEN
                                      ccr(ltp1,ktp1) = ccr(ltp1,ktp1)
     &                                  + greenr
                                      xea(ltp1,ktp1) = xea(ltp1,ktp1)
     &                                  + greenx
                                      yea(ltp1,ktp1) = yea(ltp1,ktp1)
     &                                  + greeny
                                      cci(ltp1,ktp1) = cci(ltp1,ktp1)
     &                                  + greenx + 0.5D0*WIDex*greeny
C
C calculate the 1'st derivative of the 2qp response function at the fitting energy
C
                                      dcr(ltp1,ktp1) = dcr(ltp1,ktp1)
     &                                  + ((emisq - dwsx)*pxmd**2 - 
     &                                  (eplsq - dwsx)*pymd**2)*umatqq
                                      dci(ltp1,ktp1) = dci(ltp1,ktp1)
     &                                  + ((2.D0*wqq + WIDex)
     &                                  *(emi*pxmd**2 + epl*pymd**2))
     &                                  *umatqq
                                    ELSEIF(i.EQ.3)THEN
                                      cr3(ltp1,ktp1) = cr3(ltp1,ktp1)
     &                                  + greenr
                                      ccp(ltp1,ktp1) = ccp(ltp1,ktp1)
     &                                  + greenx + 0.5D0*WIDex*greeny
                                    ENDIF
                                    ext = ext + ESTep/10.0D0
                                    i = i + 1
                                  ENDDO
                                ENDIF
                              ENDIF
                            ENDDO
                          ENDIF
                          kth = -kth
                        ENDDO
                        ktp = -ktp
                      ENDDO
                    ENDIF
                  ENDIF
                ENDIF
              ENDDO
            ENDIF
          ENDDO
        ENDDO
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
          DO kt = 1, lt
C
C           INVERSE COUPLING CONSTANTS (FROM FIT TO EXP. ENERGIES)
C
            IF(efit(lt,kt).NE.0.0D0)THEN
              IF(dcr(lt,kt).NE.0.0D0)THEN
                xir = dci(lt,kt)/dcr(lt,kt)
                xpos = cci(lt,kt)*xir/(1.D0 + SQRT(1.D0 + xir**2))
                xneg = -cci(lt,kt)**2/xpos
                cpos = ccr(lt,kt) + xpos
                cneg = ccr(lt,kt) + xneg
                ccpm(1) = cpos
                ccpm(2) = cneg
C
C-----------------check   for maximum
C
C                 cr1 = rfqqr(nea - 1, lt)
                cr2 = ccr(lt,kt)
C                 cr3 = rfqqr(nea + 1, lt)
                clex(lt,kt) = 0.D0
                DO j = 1, 2
                  re1 = ccm(lt,kt)
     &                  /((ccpm(j) - cr1(lt,kt))**2 + ccm(lt,kt)**2)
                  re2 = cci(lt,kt)/((ccpm(j) - cr2)**2 + cci(lt,kt)**2)
                  re3 = ccp(lt,kt)
     &                  /((ccpm(j) - cr3(lt,kt))**2 + ccp(lt,kt)**2)
                  ddr(j) = (re3 + re1 - 2.D0*re2)/ESTep**2
                  rrr(j) = re2
                ENDDO
                IF(ddr(1).LT.0.D0.AND.ddr(2).GT.0.D0)clex(lt,kt)
     &             = ccpm(1)
                IF(ddr(2).LT.0.D0.AND.ddr(1).GT.0.D0)clex(lt,kt)
     &             = ccpm(2)
                IF(ddr(1).LT.0.D0.AND.ddr(2).LT.0.D0)THEN
                  IF(rrr(1).GT.rrr(2))clex(lt,kt) = ccpm(1)
                  IF(rrr(2).GT.rrr(1))clex(lt,kt) = ccpm(2)
                ENDIF
C                 cr1 = rfqqr(nea - 1, lt)
                cr2 = ccr(lt,kt)
C                 cr3 = rfqqr(nea + 1, lt)
                re1 = ccm(lt,kt)
     &                /((clex(lt,kt) - cr1(lt,kt))**2 + ccm(lt,kt)**2)
                re2 = cci(lt,kt)
     &                /((clex(lt,kt) - cr2)**2 + cci(lt,kt)**2)
                re3 = ccp(lt,kt)
     &                /((clex(lt,kt) - cr3(lt,kt))**2 + ccp(lt,kt)**2)
              ELSE
                clex(lt,kt) = 0.0
              ENDIF
              IF(clex(lt,kt).EQ.0.D0)THEN
                WRITE(8,1100)lt - 1, kt - 1, efit(lt,kt), ddr
 1100           FORMAT(/'WARNING: From TRISTAN:'/
     &                 'WARNING: - No fit of response function for J/K='
     &                 ,I3,I3/'WARNING: E(EXP.):',F8.2,'  2nd deriv.:',
     &                 2E13.5/
     &             'WARNING: Energy is inconsistent with 2-qp spectrum!'
     &             /'WARNING: Self-consistent response is used!')
                efit(lt,kt) = 0.D0
                clex(lt,kt) = 0.D0
                xea(lt,kt) = 0.D0
                yea(lt,kt) = 0.D0
C                 xea(lt) = rfqqx(nea, lt)
C                 yea(lt) = rfqqy(nea, lt)
              ENDIF
            ENDIF
C
            l = lt - 1
            fltwp1 = 2*l + 1
            lm = MAX(2*l - 1,1)
            IF(l.EQ.1.OR.efit(lt,kt).GT.homeb)THEN
              IF(l.GT.1)clsc(lt,kt) = vnorm*GSDm(lm)*SQRT(fltwp1)
     &                                *rdsq/(rd*rws)**l
C--------------isovector strength is chosen as 1/2 of the isoscalar strength
              IF(l.EQ.1)clsc(lt,kt) = -0.5D0*vnorm*GSDm(2)
     &                                *4.D0/(rws*SQRT(fltwp1))
            ELSE
C-----------vibrational model  (good for low-energy states)
              clsc(lt,kt) = vnorm*GSDm(l + 1)*(l + 3)
     &                      /(SQRT(fltwp1)*rws**l)
            ENDIF
C
C-----------monopole case (0+) - compressional mode
C-----------factor pi**2/6 comes from series expansion of
C-----------the   0+ transition potential
C
            IF(l.EQ.0)clsc(lt,kt) = 2.*(vnorm/rwsq)*GSDm(3)*36.D0/PI**4
            IF(efit(lt,kt).LE.0.0D0)THEN
              clex(lt,kt) = clsc(lt,kt)/FLOAT(2*(lt - 1) + 1)
              IF(kt.GT.1)clex(lt,kt) = clex(lt,kt)*2.0D0
            ENDIF
            CLRn(lt,kt) = clsc(lt,kt)/clex(lt,kt)
C
C-----------effective coupling constant is used in the response functions
C           ( if EFIT is .NE. 0 )
C
            ceff = 1./clex(lt,kt)
            cnorm = ceff*ceff
            rnorm = cnorm/CLRn(lt,kt)**2
C-----------normalization of the response function with the factor given in input
            rnorm = rnorm*CNOrin(lt,kt)
C
C-----------calculate the QRPA response function                    (rho )
C-----------calculate the QRPA deformation parameters               (beta)
C
            DO ne = 1, nebinx
              qqr = rfqqr(ne,lt,kt)*cnorm
              qqx = rfqqx(ne,lt,kt)*cnorm
              qqy = rfqqy(ne,lt,kt)*cnorm
              qqy = MAX(1.D-06,qqy)
              wqa = qqx/qqy
              wqrex = 0.5*WIDex + wqa
              qqi = wqrex*qqy
              rqr = ceff - qqr
              rho1 = rnorm*qqi/(rqr**2 + qqi**2)
              RHO(ne,lt,kt) = rho1/PI
            ENDDO
C
C-----------calculate RPA-deformation parameters beta
C-----------(integrated over an energy interval of 2*ESTEP )
C-----------(except for the first and last mesh points   )
C
            BETa(1,lt,kt) = 0.5*ESTep*(RHO(1,lt,kt) + RHO(2,lt,kt))
            BETa(nebinx,lt,kt) = 0.5*ESTep*(RHO(nebinx,lt,kt) + RHO(
     &                           nebinx - 1,lt,kt))
            DO ne = 2, nebinx - 1
              BETa(ne,lt,kt) = est3*(RHO(ne - 1,lt,kt) + 4.*RHO(ne,lt,kt
     &                         ) + RHO(ne + 1,lt,kt))
            ENDDO
            DO ne = 1, nebinx
              IF(BETa(ne,lt,kt).GE.0.0D0)THEN
                BETa(ne,lt,kt) = SQRT(BETa(ne,lt,kt))
              ENDIF
            ENDDO
          ENDDO
        ENDDO
 
C
C-----write response function on TAPE17
C
C        DO NE=1,NEBINX
C        WRITE(17,1400)EST(NE),(RHO(NE,I),I=1,MIN(10,ICMAX))
C        ENDDO
C
        IF(krt.EQ.1)WRITE(8,1110)
 1110   FORMAT(//6X,'COUPLING CONSTANTS AND RENORMALIZATION'/6X,
     &         '       ( INELASTIC EXCITATION )      '/1X,'L/K',4X,
     &         'EA  ',2X,'SELF-CON',3X,'EMPIRICAL',6X,'RATIO',7X,'VEFF',
     &         4X,'RESIDUE',6X,'WIDTH',' CONFIG.')
        IF(krt.EQ.2)WRITE(8,1120)
 1120   FORMAT(//6X,'COUPLING CONSTANTS AND RENORMALIZATION'/6X,
     &         '   ( CHARGE EXCHANGE EXCITATION )    '/1X,'L/K',4X,
     &         'EA  ',3X,'SELF-CON',3X,'EMPIRICAL',6X,'RATIO',4X,
     &         'RESIDUE',6X,'WIDTH',' CONFIG.')
        DO l = 1, ICMax
          DO k = 1, l
            clsc(l,k) = 1./clsc(l,k)
            clex(l,k) = 1./clex(l,k)
            IF(yea(l,k).NE.0.0D0)THEN
              resid = 1./yea(l,k)
              widea = 2.0*xea(l,k)/yea(l,k)
            ELSE
              resid = 0.0
              widea = 0.0
            ENDIF
            veff = vnorm*CLRn(l,k)
            WRITE(8,1130)l - 1, k - 1, efit(l,k), clsc(l,k), clex(l,k), 
     &                   CLRn(l,k), veff, resid, widea, nconf(l)
 1130       FORMAT(I2,I2,F8.3,2E11.4,2F11.4,E11.4,F11.4,I7)
          ENDDO
        ENDDO
        WRITE(8,1140)eqqx
 1140   FORMAT(8X,'CONFIGURATION SPACE IS EQQX:',F9.3,' (MEV)')
C
C--------calculate non-energy weighted (SRNEW) and energy weighted (SREW)
C--------sum rules
C
        DO ltp1 = 1, ltmaxr
          DO ne = 1, nebinx
            hhh = ESTep
            IF(ne.EQ.1.OR.ne.EQ.nebinx)hhh = 0.5*hhh
            eqq = Est(ne)
            DO ktp1 = 1, ltp1
              t2 = hhh*RHO(ne,ltp1,ktp1)
              SRNew(ltp1) = SRNew(ltp1) + t2
              SREw(ltp1) = SREw(ltp1) + t2*eqq
              IF(eqq.LE.ETMax)SREwl(ltp1) = SREwl(ltp1) + t2*eqq
            ENDDO
          ENDDO
        ENDDO
C        WRITE(8,607)RD,(LT-1,LT=1,LTMAXR)
C        607 FORMAT(1H1/9X,'BETA-VALUES FOR TARGET EXCITATIONS  RD=',
C        1 F7.4//1H ,4X,' EX',11(5X,I3,3X))
C        DO 110 NE=1,NEBINX
C        110 WRITE(8,608)EST(NE),(BETA(NE,LT),LT=1,LTMAXR)
C        608 FORMAT(1H ,F7.2,11E11.4)
      ENDDO
      END SUBROUTINE INELAS
 
!---------------------------------------------------------------------------
C
C
      SUBROUTINE RADIAL(Bdtw,Phtrm,N1,L1,N2,L2,Ltr)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8, DIMENSION(500) :: FAClog
      INTEGER :: IA, IB, IC, ID, IE, IG
      INTEGER, DIMENSION(10) :: L9
      REAL*8 :: RAC, U9
      COMMON /CRAC  / FAClog, RAC, U9, IA, IB, IC, ID, IE, IG, L9
C
C Dummy arguments
C
      REAL*8 :: Bdtw, Phtrm
      INTEGER :: L1, L2, Ltr, N1, N2
C
C Local variables
C
      REAL*8 :: fnorm, s1, sumx, t1
      INTEGER :: IABS
      INTEGER :: k1, k2, k3, k4, k5, k6, k7, k8, l, ln1, ln2, nc1, nc2, 
     &           nup1, nup1mi, nup1mx
C
C*** End of declarations rewritten by SPAG
C
      Phtrm = 0.0
      l = Ltr
      IF(MOD(l + L1 + L2,2).NE.0)RETURN
      IF(l.GT.L1 + L2.OR.l.LT.IABS(L1 - L2))RETURN
      nc1 = N1*2 + L1
      nc2 = N2*2 + L2
      IF(l.EQ.0)l = 2
      IF(l.GE.IABS(nc1 - nc2))THEN
        k1 = (l + L2 - L1)/2
        nup1mi = MAX(N1 - k1 + 1,1)
        nup1mx = N2 + 1
        IF(nup1mx.GE.nup1mi)THEN
          ln1 = L1 + N1 + 1
          ln2 = L2 + N2 + 1
          s1 = 1 - MOD(N1,2)*2
          t1 = FAClog(N2 + 1) + FAClog(ln2*2) + FAClog(ln1)
     &         - FAClog(N1 + 1) - FAClog(ln1*2) - FAClog(ln2)
          fnorm = EXP(t1*0.5)*2.0**(N1 - N2 - l)*s1
          sumx = 0.0
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
     &           - FAClog(k5) - FAClog(k6) - FAClog(k7) - FAClog(k8)
            sumx = sumx + EXP(t1)*s1
          ENDDO
          Phtrm = sumx*fnorm*Bdtw**l
        ENDIF
      ENDIF
      END SUBROUTINE RADIAL
 
!---------------------------------------------------------------------------
C
C
      SUBROUTINE CLEBTRI
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8, DIMENSION(500) :: FAClog
      INTEGER :: IA, IB, IC, ID, IE, IG
      INTEGER, DIMENSION(10) :: L9
      REAL*8 :: RAC, U9
      COMMON /CRAC  / FAClog, RAC, U9, IA, IB, IC, ID, IE, IG, L9
C
C Local variables
C
      REAL*8 :: fb, fc2, s1, sqfclg, ssterm, termlg
      INTEGER :: iabc, iabcp, iamd, iapd, ibca, ibme, ibpe, icab, icmf, 
     &           icpf, k1, k2, k3, nz, nzm1, nzmi, nzmic2, nzmic3, nzmx, 
     &           nzt1, nzt2, nzt3, nzt4, nzt5
      INTEGER :: IABS
C
C*** End of declarations rewritten by SPAG
C
      RAC = 0.0
      IF(ID + IE.EQ.IG)THEN
        k1 = IA + IB + IC
        IF(k1.EQ.2*(k1/2))THEN
          k1 = IA + IB - IC
          k2 = IC - IABS(IB - IA)
          k3 = MIN(k1,k2)
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
                              RAC = ( - 1.0)**((IA - ID)/2)/SQRT(fb)
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
                              sqfclg = 0.5*(LOG(fc2) - FAClog(iabcp + 1)
     &                                 + FAClog(iabc) + FAClog(icab)
     &                                 + FAClog(ibca) + FAClog(iapd)
     &                                 + FAClog(iamd) + FAClog(ibpe)
     &                                 + FAClog(ibme) + FAClog(icpf)
     &                                 + FAClog(icmf))
                              nzmic2 = (IB - IC - ID)/2
                              nzmic3 = (IA - IC + IE)/2
                              nzmi = MAX(MAX(0,nzmic2),nzmic3) + 1
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
     &                            - FAClog(nzt1) - FAClog(nzt2)
     &                            - FAClog(nzt3) - FAClog(nzt4)
     &                            - FAClog(nzt5)
                                ssterm = s1*EXP(termlg)
                                RAC = RAC + ssterm
                                s1 = -s1
                              ENDDO
                              IF(ABS(RAC).LT.1.0D-10)RAC = 0.0
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
      END SUBROUTINE CLEBTRI
 
!---------------------------------------------------------------------------
C
C
      SUBROUTINE TRIDIG(N,A,B,C,D,Ierr)
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      INTEGER :: Ierr, N
      REAL*8, DIMENSION(N) :: A, B, C, D
C
C Local variables
C
      REAL*8 :: alpha
      INTEGER :: i, j, k
C
C*** End of declarations rewritten by SPAG
C
      Ierr = 0
      IF(N.LE.1)THEN
        IF(D(1).EQ.0.D0)THEN
          i = 1
          GOTO 10
        ELSE
          A(1) = A(1)/D(1)
          IF(N.LE.0)Ierr = -1
          GOTO 99999
        ENDIF
      ENDIF
      alpha = D(1)
      IF(alpha.NE.0.D0)THEN
        C(1) = C(1)/alpha
        C(N) = 0.
        D(1) = A(1)/alpha
        DO i = 2, N
          alpha = D(i) - B(i)*C(i - 1)
          IF(ABS(alpha).LT.1.D-20)GOTO 10
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
   10 Ierr = i
99999 END SUBROUTINE TRIDIG
 
!---------------------------------------------------------------------------
C
      SUBROUTINE BCS(A,Gap,I3,Ndim,Chemic)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      INTEGER, DIMENSION(500,2) :: BLKsp, KSP, NSP
      REAL*8, DIMENSION(500,2) :: EBCs, ESP, UAMp, VAMp
      REAL*8, DIMENSION(201,3) :: GSD
      REAL*8, DIMENSION(201,2) :: GSP
      INTEGER, DIMENSION(2) :: IBLk
      INTEGER :: NRX
      REAL*8, DIMENSION(201) :: RST
      REAL*8, DIMENSION(201,100) :: WFR
      COMMON /BLOCK / IBLk, BLKsp
      COMMON /DENS  / GSD, GSP
      COMMON /EVBCS / EBCs, VAMp, UAMp
      COMMON /RADI  / RST, NRX
      COMMON /SPQN  / ESP, KSP, NSP
      COMMON /WAVF  / WFR
C
C Dummy arguments
C
      REAL*8 :: A, Chemic, Gap
      INTEGER :: I3, Ndim
C
C Local variables
C
      REAL*8, DIMENSION(3) :: chem, d
      REAL*8 :: dc, dvl, e, eh, ep, eta, fpi, fsq, gjj, gsq, pi, pjj, 
     &          usq, vsq, xnum
      INTEGER :: i, ii, imax, k, n, nrxx, nst
      CHARACTER(40), DIMENSION(2) :: text
C
C*** End of declarations rewritten by SPAG
C
 
 
      DATA imax/100/
      pi = 4.*ATAN(1.)
      fpi = 4.*pi
      IF(Gap.LT.0.0D0)Gap = 2.
      text(1) = 'PROTONS '
      text(2) = 'NEUTRONS'
      IF(IBLk(I3).EQ.0)THEN
C         WRITE (8,99005) text(I3), NBL(I3), LBL(I3), JBL(I3)
C99005    FORMAT (/5X,'PAIRING WITH BLOCKING FOR ',A10/5X,'N:',I3,'  L:',
C     &           I3,'  J:',I3,'/2')
        WRITE(8,1010)text(I3)
 1010   FORMAT(/5X,'PAIRING WITHOUT BLOCKING FOR ',A10/5X)
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
   10 ii = ii + 1
      IF(ii.GT.imax)WRITE(8,*)
     &                   ' INITIALISATION FOR CHEMICAL POTENTIAL FAILED'
      chem(2) = chem(1) + dc
      CALL NUMBER(Ndim,I3,gsq,A,xnum,chem(2),d(2))
      IF(d(1)*d(2).LE.0.0D0)THEN
        ii = 0
   15   ii = ii + 1
        dvl = (d(2) - d(1))/(chem(2) - chem(1))
        dc = -d(1)/dvl
        chem(2) = chem(1)
        d(2) = d(1)
        chem(1) = chem(1) + dc
        CALL NUMBER(Ndim,I3,gsq,A,xnum,chem(1),d(1))
        IF(ABS(d(1)).LT.1.D-04)THEN
          Chemic = chem(1)
          text(1) = ' BCS FOR PROTONS  '
          text(2) = ' BCS FOR NEUTRONS '
          WRITE(8,1020)text(I3), Gap, Chemic, xnum
 1020     FORMAT(//A20,'  GAP:',F8.3,'  CHEM.POT.:',F10.4,' NUMBER:',
     &           F8.1/'  NO.   N   L     J',5X,'EBCS',7X,'V',5X,'VSQ',
     &           3X,'EHOLE',3X,'EPART')
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
            IF(vsq.GE.usq)THEN
              ESP(i,I3) = eh
            ELSE
              ESP(i,I3) = ep
            ENDIF
            WRITE(8,1030)i, KSP(i,I3), NSP(i,I3), e, VAMp(i,I3), vsq, 
     &                   eh, ep
 1030       FORMAT(I5,2I4,'/2',F9.4,2F8.5,2X,2F8.3)
 
C
C--------------TEMPORARY
C--------------(SKIP CALCULATION OF MICROSC. G.S. DENSITIES)
C
            gjj = 0.0D0
            pjj = 0.0D0
            IF(vsq.LT.0.0D0)THEN
              IF(IBLk(I3).EQ.0)THEN
                gjj = 2.D0*vsq/fpi
              ELSE
                IF(BLKsp(i,I3).EQ.1)gjj = (2.D0*vsq + (usq - vsq))/fpi
              ENDIF
              pjj = 2.D0*SQRT(usq*vsq)/fpi
              DO k = 1, NRX
                fsq = WFR(k,n)**2
                GSD(k,I3) = GSD(k,I3) + gjj*fsq
                GSP(k,I3) = GSP(k,I3) + pjj*fsq
              ENDDO
            ENDIF
          ENDDO
          IF(NRX.GE.0)RETURN
          WRITE(8,1040)
 1040     FORMAT(//5X,'DENSITY AND PAIRING DENSITY'/8X,'R',6X,'DENSITY',
     &           6X,'PAIRING')
          nrxx = MIN(12.D0/(RST(2) - RST(1)),1.D0*NRX)
          nst = MAX(1,nrxx/25)
          DO n = 1, nrxx, nst
            k = MAX(1,n - 1)
            WRITE(8,1050)RST(k), GSD(k,I3), GSP(k,I3)
 1050       FORMAT(F9.2,5E13.5)
          ENDDO
        ELSE
          IF(ii.GT.imax)THEN
            WRITE(8,*)' NO CONVERGENCE FOR CHEMICAL POTENTIAL'
            STOP
          ENDIF
          GOTO 15
        ENDIF
      ELSE
        chem(1) = chem(2)
        d(1) = d(2)
        GOTO 10
      ENDIF
      END SUBROUTINE BCS
 
!---------------------------------------------------------------------------
C
      SUBROUTINE NUMBER(Ndim,I3,Gsq,A,Xnum,Chem,D)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      INTEGER, DIMENSION(500,2) :: BLKsp, KSP, NSP
      REAL*8, DIMENSION(500,2) :: ESP
      INTEGER, DIMENSION(2) :: IBLk
      COMMON /BLOCK / IBLk, BLKsp
      COMMON /SPQN  / ESP, KSP, NSP
C
C Dummy arguments
C
      REAL*8 :: A, Chem, D, Gsq, Xnum
      INTEGER :: I3, Ndim
C
C Local variables
C
      REAL*8 :: e, eta, fjj, s1, s2, vsq
      INTEGER :: n
C
C*** End of declarations rewritten by SPAG
C
C     All lines labeled with "! nilsson" fall under copyright of H.Wienke,
C     Geel 09/2005
 
 
      Xnum = 0.0
      s1 = 0.0
      s2 = 0.0
      DO n = 1, Ndim
        eta = ESP(n,I3) - Chem
        e = SQRT(eta*eta + Gsq)
        vsq = 0.5*(1.0 - eta/e)
        fjj = 2.0
        IF(IBLk(I3).NE.0)THEN
          IF(BLKsp(n,I3).NE.0)fjj = fjj + (1.0 - 2.0*vsq)/vsq
        ENDIF
        Xnum = Xnum + fjj*vsq
        s1 = s1 + fjj/e
        s2 = s2 + fjj*(1. - ESP(n,I3)/e)
      ENDDO
      D = Chem + (s2 - 2.*A)/s1
      END SUBROUTINE NUMBER
 
!---------------------------------------------------------------------------
C
      SUBROUTINE ESORT(Ndim,I3)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8, DIMENSION(500,14,2) :: ASP
      REAL*8, DIMENSION(500,2) :: EBCs, ESP, UAMp, VAMp
      INTEGER, DIMENSION(500,2) :: KK, NN
      COMMON /EVBCS / EBCs, VAMp, UAMp
      COMMON /SPQN  / ESP, KK, NN
      COMMON /SPQN2 / ASP
C
C Dummy arguments
C
      INTEGER :: I3, Ndim
C
C Local variables
C
      REAL, DIMENSION(14) :: a, ap
      INTEGER :: i, ip, k, kp, l, lp, n, np, np1
      REAL*8 :: q, qp, u, up, v, vp, x, xp
C
C*** End of declarations rewritten by SPAG
C
C     All lines labeled with "! nilsson" fall under copyright of H.Wienke,
C     Geel 09/2005
 
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
        k = KK(n,I3)
        u = UAMp(n,I3)
        v = VAMp(n,I3)
        DO l = 1, 14
          a(l) = ASP(n,l,I3)
        ENDDO
C--------L=INDEKS(N)
        DO np = np1, Ndim
          xp = ESP(np,I3)
          qp = EBCs(np,I3)
          ip = NN(np,I3)
          kp = KK(np,I3)
          up = UAMp(n,I3)
          vp = VAMp(n,I3)
          DO lp = 1, 14
            ap(lp) = ASP(np,lp,I3)
          ENDDO
C-----------LP=INDEKS(NP)
          IF(x.GT.xp)THEN
            ESP(np,I3) = x
            EBCs(np,I3) = q
            UAMp(np,I3) = u
            VAMp(np,I3) = v
            NN(np,I3) = i
            KK(np,I3) = k
            DO l = 1, 14
              ASP(np,l,I3) = a(l)
            ENDDO
C--------------INDEKS(NP)=L
            ESP(n,I3) = xp
            EBCs(n,I3) = qp
            UAMp(n,I3) = up
            VAMp(n,I3) = vp
            NN(n,I3) = ip
            KK(n,I3) = kp
            DO l = 1, 14
              ASP(n,l,I3) = ap(l)
            ENDDO
C--------------INDEKS(N)=LP
            x = xp
            q = qp
            u = up
            v = vp
            i = ip
            k = kp
            DO l = 1, 14
              a(l) = ap(l)
            ENDDO
C--------------L=LP
          ENDIF
        ENDDO
      ENDDO
      END SUBROUTINE ESORT
 
!---------------------------------------------------------------------------
 
C
C
      FUNCTION INDF(N,L,Jtw)
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      INTEGER :: Jtw, L, N
      INTEGER :: INDF
C
C Local variables
C
      INTEGER :: n0, np
C
C*** End of declarations rewritten by SPAG
C
      np = 2*(N - 1) + L
      n0 = (np*(np + 1))/2
      INDF = n0 + (Jtw + 1)/2
      END FUNCTION INDF
 
!---------------------------------------------------------------------------
      FUNCTION DWIDTH(E,W0,E0,A0)
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      REAL*8 :: A0, E, E0, W0
      REAL*8 :: DWIDTH
C
C Local variables
C
      REAL*8 :: f0, fe, g0, ge
C
C*** End of declarations rewritten by SPAG
C
C
C-----damping width of 1-phonon states
C-----anti-symmetric E-dependence is taken
      fe = EXP((E - E0)/A0)
      ge = EXP(( - (E+E0)/A0))
      f0 = 1./(1. + fe)
      g0 = 1./(1. + ge)
      DWIDTH = W0*(g0 - f0)
      END FUNCTION DWIDTH
 
!---------------------------------------------------------------------------
C
C
      SUBROUTINE SPLVL(I3,Amass,Nz,Iout)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: ALSin, BET2in, HOMega, HOMin, WIDexin
      REAL*8, DIMENSION(500,14,2) :: ASP
      INTEGER, DIMENSION(500,2) :: BLKsp, KSP, NSP
      REAL*8, DIMENSION(3) :: BST, RMS
      REAL*8, DIMENSION(8,8) :: CNOrin, EFItin
      REAL*8, DIMENSION(500,2) :: EBCs, ESP, UAMp, VAMp
      REAL*8, DIMENSION(2) :: GAP, GAPin, GRIn, VLS
      INTEGER, DIMENSION(2) :: IBLk, NHOle, NTOtal
      INTEGER :: NRX
      REAL*8, DIMENSION(201) :: RST
      COMMON /BLOCK / IBLk, BLKsp
      COMMON /EVBCS / EBCs, VAMp, UAMp
      COMMON /RADI  / RST, NRX
      COMMON /SPPA  / HOMega, VLS, RMS, BST, GAP, NHOle, NTOtal
      COMMON /SPQN  / ESP, KSP, NSP
      COMMON /SPQN2 / ASP
      COMMON /TRINP / WIDexin, GAPin, HOMin, ALSin, EFItin, CNOrin, 
     &                BET2in, GRIn
C
C Dummy arguments
C
      REAL*8 :: Amass
      INTEGER :: I3, Iout, Nz
C
C Local variables
C
      REAL*8 :: a3, ad, zall, als, chemic, dnz, e0, espx, hhh, hom, pi, 
     &          pqn, r, rd, rde, rmsd, rw0, rws, uscal, uvec, w, xnp
      REAL*8, DIMENSION(2,2) :: anp
      LOGICAL :: break
      REAL*8, DIMENSION(30) :: d
      REAL :: FLOAT
      INTEGER :: i, ifermi, imax, imin, iminh, ipr, j, jprev, jsp, k, 
     &           kh, lsp, m, n, n1, n2, ndim, nhx, nl, nmax, nnucl, nq, 
     &           nqx, numnuc, teller
      INTEGER :: INT
      REAL*8, DIMENSION(3,2) :: rnp
      CHARACTER(10), DIMENSION(2) :: text
      REAL*8, DIMENSION(30,30) :: v
      REAL*8, DIMENSION(16,2) :: vll
C
C*** End of declarations rewritten by SPAG
C
C     All lines labeled with "! nilsson" fall under copyright of H.Wienke,
C     Geel 09/2005
 
 
 
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
        IF(n.EQ.NRX)w = 1.
        rmsd = rmsd + r**4*w/(1. + EXP((r-rd)/ad))
      ENDDO
      rmsd = rmsd*hhh/3.
      rde = 0.75/(rd**3*pi*(1. + (pi*ad/rd)**2))
C
C-----calculate E0 = U(R=0) for protons and neutrons
C-----(s.p. energies are taken relative to E0)
C
      IF(I3.EQ.1)THEN
        e0 = (( - (uscal+dnz*uvec))) - 0.4*FLOAT(Nz)
     &       /a3 + 2.16*Nz/(1.2*a3)
        rde = Nz*rde
        xnp = Nz
      ELSE
        e0 = -(uscal - dnz*uvec)
        rde = (Amass - 2.*Nz)*rde
        xnp = Amass - Nz
      ENDIF
      rmsd = 4.*pi*rde*rmsd/Amass
C                                                                       ! nilsson
C     DO n = 1, 2000                                                    ! nilsson
C        iocc(n) = 0                                                    ! nilsson
C     ENDDO                                                             ! nilsson
      DO n = 1, 500
        EBCs(n,I3) = 0.
        ESP(n,I3) = 0.0
        VAMp(n,I3) = 0.0
        UAMp(n,I3) = 0.0
        BLKsp(n,I3) = 0
      ENDDO
      DO n = 1, 14
        DO m = 1, 500
          ASP(m,n,I3) = 0.0D0
        ENDDO
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
C
      hom = HOMin
      als = ALSin
      IF(hom.GT.0.0D0)HOMega = hom
C
C-----BST contains the oscillator length
C
      BST(I3) = 197.33/SQRT(938.0*HOMega)
      e0 = 0.5*(e0 - HOMega*(rws/BST(I3))**2)
      IF(als.NE.0.0D0)VLS(I3) = als/HOMega
      als = VLS(I3)
      GAP(I3) = GAPin(I3)
      IF(GAP(I3).LE.0.0D0)GAP(I3) = 12.0/SQRT(Amass)
C     ESPX=100.+ABS(E0)
      espx = 70.
      nqx = MIN(espx/HOMega,13.D0)
      IF(nqx*HOMega.LT.espx)nqx = nqx + 1
      text(1) = 'PROTONS '
      text(2) = 'NEUTRONS'
      WRITE(8,1010)text(I3), HOMega, VLS(I3), VLS(I3)*HOMega, e0
 1010 FORMAT(//,5X,'NILSSON-HAMILTONIAN FOR ',A10/'  HOMEGA:',F8.3,
     &       '  A(LS):',F8.3,' V(LS):',F8.3,
     &       ' (MEV)'/'  E0 (POTENTIAL DEPTH U(R=0)):',F8.3,' [MEV]')
      ipr = 5
      IF(Iout.GT.3)THEN
        WRITE(8,1020)(i - 1,i = 1,ipr)
 1020   FORMAT(/7X,'A(LL) (IN UNITS OF HOMEGA)'/3X,'N',5(5X,'N+',I1))
        DO n = 1, nqx + 1, ipr
          n1 = n
          n2 = MIN(nqx + 1,n1 + ipr - 1)
          WRITE(8,1030)n - 1, (vll(k,I3),k = n1,n2)
 1030     FORMAT(I4,5F8.4)
        ENDDO
      ENDIF
C     WRITE (8, 6003) TEXT(I3)
C6003 FORMAT(//5X,'SINGLE PARTICLE STATES - INPUT FOR ',A8/
C     1   '  NO.   N   L     J',5X,'ESP')
C
C-----no BCS blocking in the first run
C
      IBLk(I3) = 0
C
C-----set number of hole states to be included in the BCS calculations
C
      IF(I3.EQ.1)THEN
        nnucl = Nz
      ELSEIF(I3.EQ.2)THEN
        nnucl = INT(Amass) - Nz
      ENDIF
C-----find out the Fermi level                                           ! nilsson
      numnuc = 0
      DO n = 1, 500
        numnuc = numnuc + 2
        IF(numnuc.GE.nnucl)EXIT
      ENDDO
      ifermi = n
C-------fill up arrays                                                   ! nilsson
      n = 0
      DO nq = 0, nqx
        zall = vll(nq + 1,I3)
        DO k = 1, 2*nq + 1, 2
          imin = (k - 1)/2
          imax = nq + 1
          CALL NILSSON(d,e0,als,HOMega,nq,k,v,BET2in,zall)
          DO i = 1, imax - imin
            ESP(n + i,I3) = d(i + imin)
            NSP(n + i,I3) = nq
            KSP(n + i,I3) = k
            DO j = 1, imax - imin
              ASP(n + i,j,I3) = v(j + imin,i + imin)
            ENDDO
          ENDDO
          n = n + imax - imin
        ENDDO
      ENDDO
      nmax = n
      CALL ESORT(nmax,I3)
C-----determination of nhx                                               ! nilsson
      IF(BET2in.LT.0.001D0)THEN
        n = ifermi
        teller = 1
        DO WHILE (teller.LE.3)
          nq = NSP(n,I3)
          kh = KSP(n,I3)
          iminh = (kh - 1)/2
          lsp = nq
          i = nq + 1 - iminh
          break = .FALSE.
          DO WHILE ((lsp.GE.0).AND.(i.GT.0).AND.(.NOT.break))
            jsp = 2*lsp + 1
            IF(ASP(n,i,I3).GT.0.5)THEN
C
C j-value subshell determined                                            ! nilsson
C
              break = .TRUE.
            ELSEIF(i.GT.1)THEN
              i = i - 1
              jsp = 2*lsp - 1
              IF(ASP(n,i,I3).GT.0.5)THEN
C
C j-value subshell determined                                            ! nilsson
C
                break = .TRUE.
              ENDIF
            ENDIF
            i = i - 1
            lsp = lsp - 2
          ENDDO
          IF(n.EQ.ifermi)THEN
            jprev = jsp
          ELSEIF(jsp.NE.jprev)THEN    ! subshell is filled               ! nilsson
            jprev = jsp
            teller = teller + 1
          ENDIF
          n = n + 1
        ENDDO
        nhx = n - 2
        GRIn(I3) = ESP(nhx,I3) - ESP(ifermi,I3) + 0.1
        IF(I3.EQ.1)THEN
          WRITE(8,
     &'('' Range for constant gap. protons (MSD)'',   F6.3,''           
     &MeV'')')GRIn(I3)
          WRITE(12,
     &'('' Range for constant gap. protons (MSD)'',   F6.3,''           
     &MeV'')')GRIn(I3)
        ELSEIF(I3.EQ.2)THEN
          WRITE(8,
     &'('' Range for constant gap. neutrons (MSD)'',   F6.3,''          
     &MeV'')')GRIn(I3)
          WRITE(12,
     &'('' Range for constant gap. neutrons (MSD)'',   F6.3,''          
     &MeV'')')GRIn(I3)
        ENDIF
      ELSE
        DO n = 1, nmax
          IF(ESP(n,I3).GT.(ESP(ifermi,I3) + GRIn(I3)))EXIT
        ENDDO
        nhx = n - 1
      ENDIF
      IF(nhx.LT.ifermi + 1)nhx = ifermi + 1
      NHOle(I3) = nhx
C-----set BCS blocking                                                   ! nilsson
      IF(MOD(nnucl,2).NE.0)THEN
        IBLk(I3) = ifermi
        BLKsp(ifermi,I3) = 1
      ENDIF
      CALL BCS(xnp,GAP(I3),I3,nhx,chemic)
      DO n = nhx + 1, nmax
        EBCs(n,I3) = ESP(n,I3) - chemic
        UAMp(n,I3) = 1.0
        VAMp(n,I3) = 0.0
      ENDDO
      NTOtal(I3) = nmax
      ndim = nmax
C
C-----<R**2> is calculated
C
      RMS(I3) = 0.0
      DO nl = 1, nhx
        pqn = NSP(nl,I3) + 1.5
        RMS(I3) = RMS(I3) + pqn*2.0D0*VAMp(nl,I3)**2
      ENDDO
      RMS(I3) = RMS(I3)*BST(I3)**2
      CALL ESORT(ndim,I3)
      END SUBROUTINE SPLVL
 
!---------------------------------------------------------------------------
C
C
      SUBROUTINE RESPNS
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8, DIMENSION(11,6) :: ALPha
      REAL*8, DIMENSION(6) :: AN, FNOrm, FQ
      REAL*8, DIMENSION(NDAngecis) :: ANGle
      REAL*8, DIMENSION(3) :: BST, RMS
      REAL*8, DIMENSION(8,8) :: CLRn
      REAL*8 :: DTHeta, EOUtmi, EOUtmx, ESTep, ETMax, ETMin, FACb, 
     &          HOMega, Q0, QGRand, QMAx, QMIna, QMInb, QS1, QS2, QSTep, 
     &          RAC, ROPt, THEta1, THEta2, U9, WIDex
      REAL*8, DIMENSION(1041) :: DUMmy
      REAL*8, DIMENSION(5) :: ECEntr
      REAL*8, DIMENSION(500,2) :: ESP
      REAL*8, DIMENSION(10) :: EXTcom, FFTot
      REAL*8, DIMENSION(500) :: FAClog
      REAL*8, DIMENSION(2) :: FFAc1d, FFAc2d, FFAc3d, GAP, VLS
      REAL*8, DIMENSION(6,6) :: FNQ
      INTEGER :: IA, IB, IC12x, IC1max, IC1mxr, IC1x, IC2max, IC2mxr, 
     &           IC2x, ICC, ICMax, ID, IE, IG, KDEnvi, KEX3, KRTmax, 
     &           KRType, NAVerg, NCHanl, NEBinx, NFAc12, NN, NQ1x, NQ2x, 
     &           NTHeta, NZ
      INTEGER, DIMENSION(10) :: KEXcom, KTRl, L9, NRMax
      INTEGER, DIMENSION(500,2) :: KSP, NSP
      INTEGER, DIMENSION(2) :: NHOle, NTOtal
      REAL*8, DIMENSION(3*(NDEx + 25),8,8) :: RHO
      REAL*8, DIMENSION(3*(NDEx + 25),11,2) :: RHOb
      REAL*8, DIMENSION(21) :: SREw, SREwl, SRNew
      COMMON  RHO, SRNew, SREw, SREwl, AN, FNQ, FQ, FNOrm, ALPha, DUMmy
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
      COMMON /SPQN  / ESP, KSP, NSP
      COMMON /TRINTP/ DTHeta, ANGle, ESTep, EOUtmi, EOUtmx, ECEntr, 
     &                QSTep, QMIna, QMInb, QMAx, QGRand, FFAc1d, FFAc2d, 
     &                FFAc3d
C
C Local variables
C
      REAL*8 :: a1, basq, e0, e1, e2, eex, hhh
      REAL*8, DIMENSION(0:3*(NDEx + 25)) :: est
      REAL :: FLOAT
      INTEGER :: i, ic, iout4, ipr, k, kp1, krt, krtx, kt, lp1, lt, 
     &           ltmaxr, ne, neb, nlmax
C
C*** End of declarations rewritten by SPAG
C
C     All lines labeled with "! nilsson" fall under copyright of H.Wienke,
C     Geel 09/2005
 
                                                            ! nilsson
C
C-----in RHO, RHOB etc the increased argument NEB corresponds to
C-----increased excitation energy.
C
      iout4 = KTRl(4)                                      ! nilsson
      a1 = NZ + NN
      ltmaxr = ICMax
      DO k = 1, 2
        RMS(k) = 0.0
        CALL SPLVL(k,a1,NZ,IOUt)
        ipr = 1
        IF(ipr.NE.0)THEN
          IF(k.EQ.1)THEN
            WRITE(8,1010)
 1010       FORMAT(//10X,' PROTON SINGLE PARTICLE STATES:')
          ELSE
            WRITE(8,1020)
 1020       FORMAT(//10X,'NEUTRON SINGLE PARTICLE STATES:')
          ENDIF
          nlmax = NTOtal(k)
          WRITE(8,1030)(ESP(i,k),NSP(i,k),KSP(i,k),i = 1,nlmax)
 1030     FORMAT(6(F9.2,2I3,'/2'))
        ENDIF
      ENDDO
      basq = (NZ*BST(1)**2 + NN*BST(2)**2)/a1
      BST(3) = SQRT(basq)
      RMS(3) = (RMS(1) + RMS(2))/a1
      RMS(1) = RMS(1)/NZ
      RMS(2) = RMS(2)/NN
      DO ne = 1, NEBinx
        DO lt = 1, ICMax
          DO i = 1, 2
            RHOb(ne,lt,i) = 0.0
          ENDDO
          DO kt = 1, lt
            RHO(ne,lt,kt) = 0.0
          ENDDO
        ENDDO
      ENDDO
      CALL INELAS(est,NN,NZ,NEBinx)
      DO lp1 = 1, ltmaxr
        DO ne = 1, NEBinx
          DO kp1 = 1, lp1
            RHOb(ne,lp1,1) = RHOb(ne,lp1,1) + RHO(ne,lp1,kp1)
          ENDDO
        ENDDO
      ENDDO
      IF(KRType.GE.4)THEN
        RAC = a1/(a1 - 3.)
        IF(KRType.GE.4)THEN
          WRITE(8,*)'  ******  NOT USED  ******'
          STOP
        ENDIF
        DO lp1 = 1, ICMax
          DO kp1 = 1, lp1
            DO ne = 1, NEBinx
              RHOb(ne,lp1,2) = RHOb(ne,lp1,2) + RHO(ne,lp1,kp1)
            ENDDO
          ENDDO
        ENDDO
      ENDIF
   10 IF(iout4.NE.0)THEN
        krtx = KTRl(7) + 1
        DO krt = 1, krtx
          IF(krt.EQ.1)THEN
            WRITE(8,1040)ETMin, ETMax, ESTep
 1040       FORMAT(///10X,
     &             'RPA RESPONSE FUNCTIONS (INELASTIC EXCITATION)'/10X,
     &             'EXMIN=',F7.2,' EXMAX=',F7.2,' ESTEP=',F7.2/)
          ELSE
            WRITE(8,1050)ETMin, ETMax, ESTep
 1050       FORMAT(///10X,'RPA RESPONSE FUNCTIONS (CHARGE EXCHANGE)'/10X
     &             ,'EXMIN=',F7.2,' EXMAX=',F7.2,' ESTEP=',F7.2/)
          ENDIF
          WRITE(8,1060)(lt - 1,lt = 1,ltmaxr)
 1060     FORMAT(5X,'EX',10(4X,I4,3X))
          DO neb = 1, NEBinx
            WRITE(8,1070)est(neb), (RHOb(neb,lt,krt),lt = 1,ltmaxr)
 1070       FORMAT(' ',F6.2,10E11.4)
          ENDDO
        ENDDO
        WRITE(8,1080)(SRNew(lt),lt = 1,ltmaxr)
 1080   FORMAT(///' ','SRNEW:',11E11.4)
        WRITE(8,1090)(SREw(lt),lt = 1,ltmaxr)
 1090   FORMAT(' ','SREW :',11E11.4)
        IF(KTRl(8).EQ.( - 1))GOTO 99999
      ENDIF
      e1 = ETMin + ESTep
      DO ic = 1, ICMax
        SREw(ic) = ESTep*0.5*(ETMin*RHOb(1,ic,1) + e1*RHOb(2,ic,1))
      ENDDO
      eex = ETMin - ESTep
      hhh = ESTep/3.
      WRITE(8,1100)(ic - 1,ic = 1,ICMax)
 1100 FORMAT('1'/5X,'EWSR AS A FUNCTION OF EXCITATION ENERGY'/' ',3X,
     &       'EX ',11(5X,I3,3X))
      DO ne = 2, NEBinx, 2
        eex = eex + 2.*ESTep
        IF(ne.NE.2)THEN
          e0 = ETMin + FLOAT(ne - 1)*ESTep
          e1 = e0 - ESTep
          e2 = e1 - ESTep
          DO ic = 1, ICMax
            SREw(ic) = SREw(ic)
     &                 + hhh*(e2*RHOb(ne - 2,ic,1) + 4.*e1*RHOb(ne - 1,
     &                 ic,1) + e0*RHOb(ne,ic,1))
          ENDDO
          WRITE(8,1110)eex, (SREw(ic),ic = 1,ICMax)
 1110     FORMAT(' ',F6.2,11E11.4)
        ENDIF
      ENDDO
      KTRl(8) = -1
      GOTO 10
99999 END SUBROUTINE RESPNS
 
!---------------------------------------------------------------------------
C
C
C
C
      SUBROUTINE POLYNM(N,X,F)
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      INTEGER :: N
      REAL*8 :: X
      REAL*8, DIMENSION(N) :: F
C
C Local variables
C
      REAL :: FLOAT
      INTEGER :: i, k
      REAL*8 :: y
C
C*** End of declarations rewritten by SPAG
C
      y = FLOAT(N - 1) - 2.*X
      F(1) = 1.
      F(2) = y/FLOAT(N - 1)
      DO i = 3, N
        k = i - 1
        F(i) = (y*F(i - 1)*FLOAT(2*k - 1) - F(i - 2)*FLOAT((k-1)*(N+k-1)
     &         ))/FLOAT(k*(N - k))
      ENDDO
      END SUBROUTINE POLYNM
 
!---------------------------------------------------------------------------
      FUNCTION EHO(N,L,Jtw,Hbo,Zall,Als)
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      REAL*8 :: Zall, Als, Hbo
      INTEGER :: Jtw, L, N
      REAL*8 :: EHO
C
C Local variables
C
      REAL*8 :: ell, els, fjj, fso, rll
      INTEGER :: nq
C
C*** End of declarations rewritten by SPAG
C
C
C
C
      nq = 2*(N - 1) + L
      rll = L*(L + 1.)
      fjj = 0.25*(Jtw + 2.)*Jtw
      IF(L.EQ.0)THEN
        fso = 0.0
      ELSE
        fso = 0.5*(fjj - rll - 0.75)
      ENDIF
      ell = -Zall*(rll - 0.5*nq*(nq + 3.))
      els = -Als*fso
      EHO = Hbo*(nq + 1.5 + ell + els)
      END FUNCTION EHO
 
!---------------------------------------------------------------------------
 
C
C
      SUBROUTINE PNORM(Ndim,N,Fnorm,Fxn)
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      INTEGER :: N, Ndim
      REAL*8, DIMENSION(N) :: Fnorm
      REAL*8, DIMENSION(Ndim,N) :: Fxn
C
C Local variables
C
      REAL :: FLOAT
      REAL*8, DIMENSION(30) :: flog
      INTEGER :: j, k, ntw
      REAL*8 :: x
C
C*** End of declarations rewritten by SPAG
C
 
      flog(1) = 0.
      flog(2) = 0.
      ntw = 2*N
      DO k = 3, ntw
        flog(k) = flog(k - 1) + LOG(FLOAT(k - 1))
      ENDDO
      DO k = 1, N
        Fnorm(k) = EXP(flog(N + k) + flog(N - k + 1) - 2.*flog(N))
     &             /FLOAT(2*k - 1)
      ENDDO
      DO k = 1, N
        x = k - 1
        CALL POLYNM(N,x,flog)
        DO j = 1, N
          Fxn(k,j) = flog(j)
        ENDDO
      ENDDO
      END SUBROUTINE PNORM
 
!---------------------------------------------------------------------------
C
C
      SUBROUTINE INVERT(Ndim,N,A,B)
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      INTEGER :: N, Ndim
      REAL*8, DIMENSION(Ndim,N) :: A, B
C
C Local variables
C
      REAL*8 :: cik, hilf, pivot, s1, s2
      REAL :: FLOAT
      INTEGER :: i, indx, indy, ix, iy, j, k, l, m
      INTEGER, DIMENSION(20) :: merkx, merky
C
C*** End of declarations rewritten by SPAG
C
 
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
          IF(merkx(ix).EQ.0)THEN
            DO iy = 1, N
              IF(merky(iy).EQ.0)THEN
                IF(ABS(B(ix,iy)).GT.ABS(pivot))THEN
                  pivot = B(ix,iy)
                  indx = ix
                  indy = iy
                ENDIF
              ENDIF
            ENDDO
          ENDIF
        ENDDO
        IF(ABS(pivot).LE.0.0D0)GOTO 99999
        merkx(indx) = indy
        merky(indy) = indx
        B(indx,indy) = 1./pivot
        DO l = 1, N
          IF(l.NE.indx)THEN
            DO m = 1, N
              IF(m.NE.indy)B(l,m) = B(l,m) - B(l,indy)*B(indx,m)/pivot
            ENDDO
          ENDIF
        ENDDO
        DO ix = 1, N
          IF(ix.NE.indx)B(ix,indx) = B(ix,indy)/pivot
        ENDDO
        DO iy = 1, N
          IF(iy.NE.indy)B(indx,iy) = -B(indx,iy)/pivot
        ENDDO
      ENDDO
      DO i = 2, N
        ix = i - 1
        IF(merkx(ix).NE.ix)THEN
          DO j = i, N
            iy = j
            IF(merky(iy).EQ.ix)EXIT
          ENDDO
          DO k = 1, N
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
        IF(merky(ix).NE.ix)THEN
          DO j = i, N
            iy = j
            IF(merky(iy).EQ.ix)EXIT
          ENDDO
          DO k = 1, N
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
          IF(i.EQ.k)s1 = s1 + cik
          s2 = s2 + cik
        ENDDO
      ENDDO
      s2 = s2 - s1
      s1 = s1/FLOAT(N) - 1.
      IF(ABS(s1).GE.1.D-07.OR.ABS(s2).GE.1.D-07)THEN
        WRITE(8,1010)s1, s2
 1010   FORMAT(//' ','ERROR IN INVERT - S1,S2:',2E14.7)
      ENDIF
      DO i = 1, N
        DO j = 1, N
          A(j,i) = B(j,i)
        ENDDO
      ENDDO
99999 END SUBROUTINE INVERT
 
!---------------------------------------------------------------------------
C
      SUBROUTINE SPECTR(Idimm,Kdim,Ngle,Nq12x,Nbinx,Lc12x,Cros,Crose,
     &                  Nejc,Xsinl)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8, DIMENSION(NDAngecis) :: ANGle
      REAL*8, DIMENSION(8,8) :: CLRn
      REAL*8 :: DTHeta, EOUtmi, EOUtmx, ESTep, ETMax, ETMin, FACb, Q0, 
     &          QGRand, QMAx, QMIna, QMInb, QS1, QS2, QSTep, ROPt, 
     &          THEta1, THEta2, WIDex
      REAL*8, DIMENSION(5) :: ECEntr
      REAL*8, DIMENSION(10) :: EXTcom, FFTot
      REAL*8, DIMENSION(2) :: FAC1d, FAC2d, FAC3d
      INTEGER :: IC12x, IC1mx, IC1mxr, IC2mx, IC2mxr, ICMax, KDEnvi, 
     &           KEX3, KRTmax, KRType, LC1mx, LC2mx, NAVerg, NCHanl, 
     &           NEBinx, NFAc12, NN, NQ1x, NQ2x, NTHeta, NZ
      INTEGER, DIMENSION(10) :: KEXcom, KTRl, NRMax
      REAL*8, DIMENSION(3*(NDEx + 25),11,2) :: RHOb
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
      INTEGER :: Idimm, Kdim, Lc12x, Nbinx, Nejc, Ngle, Nq12x
      REAL*8 :: Xsinl
      REAL*8, DIMENSION(Nq12x,Lc12x,Kdim) :: Cros
      REAL*8, DIMENSION(2*NDEx,NDAngecis,NDAngecis) :: Crose
C
C Local variables
C
      REAL*8 :: a1, a2, a3, an, ay, aynorm, delta, eout, fh, piece, 
     &          pxsec, q1, q2, rb12, s1, s2, s3, sigm, sn, sumx, x, x2, 
     &          xl, xq, yl
      REAL*8, DIMENSION(5,7) :: adum
      REAL*8, DIMENSION(7,7) :: al1l2, fl1, fl2
      REAL*8, DIMENSION(15,15) :: amat, gmat
      REAL*8, DIMENSION(NDAngecis) :: csfit
      REAL*8, DIMENSION(15) :: f1, f2, zz
      REAL*8, DIMENSION(2) :: f11, f21
      REAL :: FLOAT
      REAL*8, DIMENSION(7) :: fnl1, fnl2, fnq1, fnq2
      REAL*8, DIMENSION(6,6) :: fq1, fq2
      REAL :: hhh
      INTEGER :: i, ic, icp, icpmx, icpx, ier, j, jx, k, k1, k2, k2n, 
     &           kc, kcp, kcpmx, kkp, kq, kr, krtx, kx, ky, l, l1p1, 
     &           l2p1, lc, lc1, lcp1, ln, m2, m2n, mx, my, n, n0, n1, 
     &           n2, n2n, na, nad, nangle, nc, ncm1, ndim, ne, neb, 
     &           necs, nej, nemnt, nemx, nep, nepp, nmax, nnur, np, npx, 
     &           nq, nqx, nx, ny
      INTEGER :: MAX0, MIN0
      REAL*8, DIMENSION(5) :: qq
      REAL*8, DIMENSION(3*(NDEx + 25)) :: sg
      REAL*8, DIMENSION(3*(NDEx + 25),2) :: sumpx
C
C*** End of declarations rewritten by SPAG
C
C
C     DOUBLE PRECISION Cros(Nq12x,Lc12x,Kdim), Crose(Nbinx,Ngle,Idimm)
      EQUIVALENCE(f2(1),f21)
      EQUIVALENCE(f1(1),f11)
      nej = 1
      IF(ZEJc(Nejc).EQ.1.0D0)nej = 2
      IF(ZEJc(Nejc).GT.1.0D0)THEN
        WRITE(8,*)
     &' THIS IMPLEMENTATION OF TRISTAN IS ABLE TO TREAT ONLY NEUTRON ORP
     &ROTON IN THE INCIDENT CHANNEL'
        STOP
      ENDIF
      nnur = NREs(nej)
      icpx = KTRl(1)
      icpmx = icpx + KTRl(7)
      kcpmx = icpmx + 1
      krtx = KTRl(7) + 1
      kkp = 1
      IF(KRType.GE.4)kkp = 2
      nangle = Ngle
      IF(IOUt.GT.3)WRITE(8,1010)
 1010 FORMAT('1'/20X,10('+'),5X,'MSDR - CROSS SECTIONS AND',
     &       ' ANALYZING POWERS',5X,10('+')///40X,'UNITS ARE MB AND %'/)
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
                sumx = 0.
                DO k2 = k2n, NQ1x
                  ky = ky + 1
                  sumx = sumx + fq2(ky,ny)*fq2(ky,my)
                ENDDO
                gmat(i,j) = gmat(i,j) + sumx*fq1(kx,nx)*fq1(kx,mx)
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ENDDO
C
C
      CALL INVERT(15,ndim,gmat,amat)
      IF(KEX3.NE.1)THEN
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
      REWIND(16)
      DO kr = 1, krtx
        DO na = 1, nangle
          IF(kr.EQ.1)THEN
C
C--------------one-step
C
            DO n1 = 1, NQ1x
              DO icp = 1, icpx
C
C--------------------ICP = 1   left  x-sections
C--------------------ICP = 2   right x-section
C
                READ(16)(Cros(n1,lc1,icp),lc1 = 1,LC1mx,KEX3)
                IF(KEX3.NE.1)THEN
C
C-----------------------interpolation for L1
C
                  DO ic = 1, IC1mx
                    sumx = 0.
                    j = 0
                    DO lc = 1, LC1mx, KEX3
                      j = j + 1
                      pxsec = LOG(Cros(n1,lc,icp))
                      sumx = sumx + pxsec*fl1(j,ic)
                    ENDDO
                    al1l2(ic,1) = sumx/fnl1(ic)
                  ENDDO
                  DO lcp1 = 2, LC1mx, KEX3
                    x = FLOAT(lcp1 - 1)/2.
                    CALL POLYNM(IC1mx,x,f1)
                    sumx = 0.
                    DO i = 1, IC1mx
                      sumx = sumx + f1(i)*al1l2(i,1)
                    ENDDO
                    Cros(n1,lcp1,icp) = EXP(sumx)
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
                  sumx = 0.
                  DO k = 1, NQ1x
                    kq = NQ1x - k + 1
                    pxsec = LOG(Cros(kq,l1p1,icp))
                    sumx = sumx + pxsec*fq1(k,n)
                  ENDDO
                  al1l2(n,icp) = sumx/fnq1(n)
                ENDDO
              ENDDO
              q1 = ETMin - ESTep
              DO ne = 1, Nbinx
                q1 = q1 + ESTep
                x = FLOAT(NQ1x - 1)*(q1 - ((-QMIna)))
     &              /(((-QMAx)) - ((-QMIna)))
                CALL POLYNM(NQ1x,x,f1)
                DO icp = 1, icpx
                  kcp = kcpmx*icp - icpmx
                  sumx = 0.
                  DO n1 = 1, NQ1x
                    sumx = sumx + al1l2(n1,icp)*f1(n1)
                  ENDDO
                  sg(ne) = EXP(sumx)
                  Crose(ne,na,kcp) = Crose(ne,na,kcp) + sg(ne)
     &                               *RHOb(ne,l1p1,kkp)
                ENDDO
              ENDDO
            ENDDO
C
C
C--------------two - step
C
C
            IF(NCHanl.EQ.2)GOTO 5
          ENDIF
          DO nq = 1, Nq12x
            DO icp = 1, icpx
              READ(16)(Cros(nq,KEX3*(ic-1) + 1,icp),ic = 1,IC12x)
              IF(KEX3.NE.1)THEN
C
C--------------------interpolation for (L1,L2)-dependence
C
                DO i = 1, IC1mx
                  DO j = 1, IC2mx
                    sumx = 0.
                    DO k = 1, LC1mx, KEX3
                      kc = (k - 1)/KEX3 + 1
                      DO l = 1, LC2mx, KEX3
                        lc = (l - 1)/KEX3 + 1
                        nc = (k - 1)*IC2mx + l
                        pxsec = LOG(Cros(nq,nc,icp))
                        sumx = sumx + pxsec*fl1(kc,i)*fl2(lc,j)
                      ENDDO
                    ENDDO
                    al1l2(i,j) = sumx/(fnl1(i)*fnl2(j))
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
                    sumx = 0.
                    DO k = 1, IC1mx
                      DO l = 1, IC2mx
                        sumx = sumx + al1l2(k,l)*f1(k)*f2(l)
                      ENDDO
                    ENDDO
                    Cros(nq,lc,icp) = EXP(sumx)
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
                        zz(nq) = zz(nq) + pxsec*fq1(kx,nx)*fq2(ky,ny)
                      ENDDO
                    ENDDO
                  ENDDO
                ENDDO
                DO n1 = 1, Nq12x
                  amat(n1,icp) = 0.
                  DO n2 = 1, Nq12x
                    amat(n1,icp) = amat(n1,icp) + gmat(n1,n2)*zz(n2)
                  ENDDO
                ENDDO
              ENDDO
              q2 = ETMin
              DO ne = 2, Nbinx
                q2 = q2 + ESTep
                x2 = FLOAT(NQ2x - 1)*(q2 - ((-QMInb)))
     &               /(((-QMAx)) - ((-QMInb)))
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
                      amat(nx,3 + icp) = amat(nx,3 + icp) + amat(i,icp)
     &                  *f2(ny)
                    ENDDO
                  ENDDO
                ENDDO
                DO nep = 1, ne
                  q1 = q1 + ESTep
                  xq = FLOAT(NQ1x - 1)*(q1 - ((-QMIna)))
     &                 /(((-QMAx)) - ((-QMIna)))
                  CALL POLYNM(NQ1x,xq,f1)
                  nepp = ne - nep + 1
C-----------------------next two lines are for (p,alpha) calculations (orig. as
C-----------------------came from mu)
C-----------------------IF(KR.EQ.1)RB12=RHOB(NEP,L1P1,2)*RHOB(NEPP,L2P1,1)
C-----------------------IF(KR.EQ.2)RB12=RHOB(NEP,L1P1,1)*RHOB(NEPP,L2P1,2) next
C-----------------------line substitutes previous 2 according to Horst's
C-----------------------suggestion
                  rb12 = RHOb(nep,l1p1,krtx - kr + 1)*RHOb(nepp,l2p1,kr)
                  DO icp = 1, icpx
                    sumx = 0.
                    DO nx = 1, NQ1x
                      sumx = sumx + amat(nx,icp + 3)*f1(nx)
                    ENDDO
                    IF(sumx.GT.100.D0) sumx = 100.
                    sumpx(nep,icp) = sumpx(nep,icp) + rb12*EXP(sumx)
                  ENDDO
                ENDDO
                DO icp = 1, icpx
                  fh = 2.
                  npx = ne - 1
                  sumx = sumpx(1,icp) + sumpx(ne,icp)
                  IF(ne.GE.3)THEN
                    IF(1.GE.2*MOD(ne,2))THEN
                      sumx = sumpx(1,icp) + sumpx(npx,icp)
     &                      + 1.5*(sumpx(npx,icp) + sumpx(ne,icp))
                      npx = npx - 1
                    ENDIF
                    DO np = 2, npx
                      fh = 6. - fh
                      sumx = sumx + fh*sumpx(np,icp)
                    ENDDO
                  ENDIF
                  sumpx(icp,1) = sumx*ESTep/3.
                ENDDO
                DO icp = 1, icpx
                  kcp = kcpmx*icp - icpmx + kr
                  Crose(ne,na,kcp) = Crose(ne,na,kcp) + sumpx(icp,1)
                ENDDO
              ENDDO
            ENDDO
          ENDDO
          IF(NAVerg.GT.0)THEN
            IF(KTRl(7).EQ.1.AND.kr.EQ.1)CYCLE
C
C--------------averaging of the X - section and analyzing powers
C
            ncm1 = NCHanl - 1
            DO nc = 1, ncm1
              kcp = kcpmx*(nc - 1)
              DO icp = 1, icpmx
                kcp = kcp + 1
                DO ne = 1, Nbinx
                  nemnt = MAX0(1,ne - NAVerg)
                  nemx = MIN0(Nbinx,nemnt + NAVerg)
                  sumx = 0.
                  DO neb = nemnt, nemx
                    hhh = ESTep
                    IF(neb.EQ.nemnt.OR.neb.EQ.nemx)hhh = 0.5*hhh
                    sumx = sumx + hhh*Crose(neb,na,kcp)
                  ENDDO
                  sg(ne) = sumx/delta
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
                Crose(ne,na,kx) = Crose(ne,na,kx) + Crose(ne,na,kcp)
              ENDDO
            ENDDO
          ENDDO
    5     IF(IOUt.GT.3)THEN
            WRITE(8,1020)ANGle(na), delta, 
     &                   (FAC1d(n),FAC2d(n),FAC3d(n),n = 1,NFAc12)
 1020       FORMAT(/'0',6X,'X-SECTION AND ANALYZING POWER FOR THETA=',
     &             F6.2/' ',5X,'AVERAGING:',F6.2,5X,'FACND:',2(3F7.3,3X)
     &             //' ',2X,'EOUT',3X,'1-STEP',10X,'2-STEP',11X,'TOTAL',
     &             32X,'1-STEP',10X,'2-STEP',11X,'TOTAL')
            WRITE(66,1030)ANGle(na), Nbinx
 1030       FORMAT(' THETA= ',F5.1,I5)
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
            IF(ne.NE.1)THEN
              sigm = 0.5*(Crose(ne,na,k1) + Crose(ne,na,k2))
              ay = 50.*(Crose(ne,na,k1) - Crose(ne,na,k2))/sigm
              s1 = 0.5*(Crose(ne,na,1) + Crose(ne,na,k1 + 1))
              s2 = 0.5*(Crose(ne,na,2) + Crose(ne,na,k1 + 2))
              a1 = 50.*(Crose(ne,na,1) - Crose(ne,na,k1 + 1))/sigm
              a2 = 50.*(Crose(ne,na,2) - Crose(ne,na,k1 + 2))/sigm
              IF(KTRl(7).NE.0)THEN
                s3 = 0.5*(Crose(ne,na,3) + Crose(ne,na,7))
                a3 = 50.*(Crose(ne,na,3) - Crose(ne,na,7))/sigm
              ENDIF
              IF(NFAc12.NE.0)THEN
                DO nad = 1, NFAc12
                  sn = FAC1d(nad)*s1 + FAC2d(nad)*s2
                  IF(KTRl(7).EQ.1)sn = sn + FAC3d(nad)*s3
                  aynorm = sigm/sn
                  f1(nad) = sn
                  an = FAC1d(nad)*a1 + FAC2d(nad)*a2
                  IF(KTRl(7).EQ.1)an = an + FAC3d(nad)*a3
                  f2(nad) = an*aynorm
                ENDDO
              ENDIF
              IF(IOUt.GT.3)THEN
                WRITE(8,1040)eout, s1, s2, s3, sigm, f11, a1, a2, a3, 
     &                       ay, f21
                WRITE(66,1040)eout, s1, s2, sigm
              ENDIF
C                 necs = Nbinx - ne + 2
C-----------------recover from the more dense energy grid in MSD
              necs = (Nbinx - ne)/2 + 2
              sigm = sigm/2.0
C-----------------store ddx to continuum
              IF(IDNa(2*nej,2).NE.0.AND.necs.LE.NEX(nnur) - 1)THEN
                CSEa(necs,na,nej,1) = CSEa(necs,na,nej,1) + sigm
C-----------------discrete level region is not needed since spectra are
C-----------------constructed out of discrete levels
              ELSEIF(IDNa(2*nej - 1,2).NE.0.AND.necs.GE.NEX(nnur))THEN
                CSEa(necs,na,nej,1) = CSEa(necs,na,nej,1) + sigm
              ENDIF
            ENDIF
          ENDDO
        ENDDO
      ENDDO
      IF(IOUt.GT.3)CLOSE(66)
      REWIND(14)
      k1 = kcpmx
C-----integrate angular distributions over angle (and energy)
      nmax = MIN(NDEx,Nbinx/2 + 2)
C-----if ECIS active use only continuum part of the MSD spectrum
      IF(DIRect.GT.0)nmax = MIN(nmax,NEX(nnur))
      DO ne = 1, nmax
        DO na = 1, nangle
          csfit(na) = CSEa(ne,nangle - na + 1,nej,1)
        ENDDO
        CALL LSQLEG(CANgler,csfit,nangle,qq,5,adum,ier)
        piece = 4.0*3.14159*qq(1)
        CSEmsd(ne,nej) = CSEmsd(ne,nej) + piece
        CSMsd(nej) = CSMsd(nej) + piece*DE
        Xsinl = Xsinl + piece*DE
      ENDDO
      WRITE(8,*)' '
      WRITE(8,
     &'('' Integrated MSD emission at Elab '', G15.3,'' is ''         , 
     &G15.3,'' mb'')')EINl, CSMsd(nej)
      WRITE(8,*)' '
C-----angular distributions integration *** done ***
 1040 FORMAT(' ',F6.2,6E11.4,4X,6F9.4)
      END SUBROUTINE SPECTR
 
!---------------------------------------------------------------------------
C
C
C
C
C
      SUBROUTINE ACCUMSD(Nnuc,Nnur,Nejc)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      INTEGER :: Nejc, Nnuc, Nnur
C
C Local variables
C
      REAL*8 :: coef, csm1, csm2, csmsdl, csmtot, dang, echannel, ecm, 
     &          eee, eemi, erecoil, excnq, pops, somj, swght, w, weight, 
     &          xj, xnor
      REAL*8, DIMENSION(NDAngecis) :: ddxs
      REAL*8 :: DEXP
      REAL :: FLOAT, SNGL
      INTEGER :: icsp, ie, il, irec, istart, j, na, nangle, nexrt, next
      INTEGER :: INT
      REAL*8, DIMENSION(NDLw) :: phdj
      REAL*8, DIMENSION(NDLv) :: wght
C
C*** End of declarations rewritten by SPAG
C
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:mpu*
Ccc   *                         A C C U M S D                            *
Ccc   *                                                                  *
Ccc   * Distributes PE spectrum over the residual nucleus continuum     *
Ccc   * assuming that the spin distribution is proportional to the       *
Ccc   * spin distribution of the 2-exciton states.                       *
Ccc   *                                                                  *
Ccc   * It distributes also PE contribution over the discrete levels    *
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
      IF(NEX(Nnuc).LT.1)THEN
        WRITE(8,*)' HM !! THERE MUST BE SOMETHING WRONG !!!'
        WRITE(8,*)' ACCUMSD COMPLAINS NEGATIVE ENERGY FOR'
        WRITE(8,*)' NUCLEUS NNUC=', Nnuc, ' NEX(NNUC)=', NEX(Nnuc)
        WRITE(8,*)' I HAD BETTER  S T O P'
        STOP
      ENDIF
C-----
C----- CONTINUUM
C-----
      IF(Nnuc.EQ.Nnur)THEN
        excnq = EX(NEX(Nnuc),Nnuc)
      ELSE
        excnq = EX(NEX(Nnuc),Nnuc) - Q(Nejc,Nnuc)
      ENDIF
C-----number of spectrum bins to continuum WARNING! might be negative!
      nexrt = MIN(INT((excnq-ECUt(Nnur))/DE + 1.0001),NDEcsed)
C-----total number of bins
      next = INT(excnq/DE + 1.0001)
C-----calculate spin distribution for 1p-1h states
      SIG = 2*0.26*A(Nnur)**0.66666667
      somj = 0.0
      DO j = 1, NLW
        xj = SQRT(FLOAT(j)**2 + XJLv(LEVtarg,0)**2)
        phdj(j) = 0.0
        w = (xj + 1.0)*xj/2./SIG
        IF(w.LE.50.D0)THEN
          phdj(j) = (2*xj + 1.)*DEXP( - w)
          somj = somj + phdj(j)
        ENDIF
      ENDDO
C-----distribution of the continuum MSD contribution -
C-----proportional to the p-h spin distribution shifted by the target
C-----ground state target spin XJLV(1,0)
      IF(nexrt.GT.0)THEN
        DO j = 1, NLW
          xnor = 0.5*phdj(j)/somj
          DO ie = 1, nexrt
            pops = xnor*CSEmsd(nexrt - ie + 1,Nejc)
C
C              Population increased to preserve total flux
C              as calculated by PCROSS/MSD+MSC
C
            IF(ie.EQ.1.OR.ie.EQ.nexrt)pops = 2*pops
C
            POP(ie,j,1,Nnur) = POP(ie,j,1,Nnur) + pops
            POP(ie,j,2,Nnur) = POP(ie,j,2,Nnur) + pops
          ENDDO
        ENDDO
 
C--------add MSD contribution to the population spectra
C--------used for ENDF exclusive spectra
        IF(ENDf(1).GT.0)THEN
          DO ie = 1, nexrt
            icsp = nexrt - ie + 1
C
C              Population increased to preserve total flux
C              as calculated by PCROSS/MSD+MSC
C
            pops = CSEmsd(icsp,Nejc)
C              Commented on Dec 2011 by RCN, to keep integral of spectra = XS
C              if(ie.eq.1 .or. ie.eq.nexrt) pops=2*pops
 
            POPcse(ie,Nejc,icsp,INExc(Nnur))
     &        = POPcse(ie,Nejc,icsp,INExc(Nnur)) + pops
C--------------Correct last bin (not needed for POP as for this it is done at the end)
            IF(ie.EQ.1)POPcse(ie,Nejc,icsp,INExc(Nnur))
     &         = POPcse(ie,Nejc,icsp,INExc(Nnur))
     &         - 0.5*CSEmsd(icsp,Nejc)
 
C--------------DDX using portions
            POPcseaf(ie,Nejc,icsp,INExc(Nnur)) = 1.0
C--------------DDX
C--------------Bin population by MSD (spin/parity integrated)
            POPbin(ie,Nnur) = pops
          ENDDO
        ENDIF
C--------storing continuum recoils
        IF(ENDf(1).GT.0.AND.Nejc.NE.0.AND.RECoil.GT.0)THEN
C
C           No recoils from gamma emission for the time being
C
          nangle = NDAng
          dang = 3.14159/FLOAT(nangle - 1)
          coef = 2*3.14159*dang/DERec
          ecm = EINl - EIN
          IF(Nejc.NE.0)THEN
            DO ie = 1, nexrt
              echannel = (ie - 1)*DE*AEJc(Nejc)/A(1)
              DO na = 1, nangle
                erecoil = ecm + echannel + 2*SQRT(ecm*echannel)
     &                    *CANgler(na)
                irec = erecoil/DERec + 1.001
                weight = (erecoil - (irec - 1)*DERec)/DERec
                IF(irec + 1.GT.NDErec)EXIT
                csmsdl = CSEa(nexrt - ie + 1,na,Nejc,1)*SANgler(na)*coef
                RECcse(irec,ie,Nnur) = RECcse(irec,ie,Nnur)
     &                                 + csmsdl*(1.0 - weight)
                RECcse(irec + 1,ie,Nnur) = RECcse(irec + 1,ie,Nnur)
     &            + csmsdl*weight
C
              ENDDO
            ENDDO
          ENDIF
 
        ENDIF
      ENDIF
C-----
C----- DISCRETE LEVELS
C-----
C-----return if MSD to discrete levels not used (matrix IDNa)
C
C     Discrete levels not used for alpha (please do not include levels into
C                   continuum for alpha emission)
C
C     IF (Nejc.eq.0 .or. Nejc.gt.2) return
      IF(Nejc.EQ.1.AND.IDNa(1,2).EQ.0.AND.IDNa(1,6).EQ.0)RETURN
      IF(Nejc.EQ.2.AND.IDNa(3,2).EQ.0.AND.IDNa(3,6).EQ.0)RETURN
 
      IF(Nejc.EQ.0.AND.IDNa(1,2).EQ.0)RETURN      ! Skipping discrete gammas if MSC not active
      IF(Nejc.EQ.3.AND.IDNa(11,6).EQ.0)RETURN
      IF(Nejc.EQ.4.AND.IDNa(12,6).EQ.0)RETURN
      IF(Nejc.EQ.5.AND.IDNa(13,6).EQ.0)RETURN
      IF(Nejc.EQ.6.AND.IDNa(14,6).EQ.0)RETURN
C-----discrete level contribution to recoil spectra
C-----in case only discrete levels can be populated we set nexrt to 1
C-----(NOTE: it is usually negative in such a case)
      IF(nexrt.LE.0)nexrt = 1
      IF(ENDf(1).GT.0.AND.RECoil.GT.0)THEN
        nangle = NDAng
        DO ie = nexrt, next
          echannel = (ie - 1)*DE*AEJc(Nejc)/A(1)
          DO na = 1, nangle
            erecoil = ecm + echannel + 2*SQRT(ecm*echannel)*CANgler(na)
            irec = erecoil/DERec + 1.001
            weight = (erecoil - (irec - 1)*DERec)/DERec
            IF(irec + 1.GT.NDErec)EXIT
            csmsdl = CSEa(ie,na,Nejc,1)*SANgler(na)*coef*DE
            IF(ie.EQ.nexrt)csmsdl = 0.5*csmsdl
            RECcse(irec,0,Nnur) = RECcse(irec,0,Nnur)
     &                            + csmsdl*(1.0 - weight)
            RECcse(irec + 1,0,Nnur) = RECcse(irec + 1,0,Nnur)
     &                                + csmsdl*weight
          ENDDO
        ENDDO
      ENDIF
C-----distribution of the MSD/PCROSS contribution to discrete levels
C-----
      IF(Nejc.EQ.NPRoject)THEN
C
C      Inelastic channel
C
        csm1 = 0.D0
        istart = nexrt + 1
        DO ie = istart, next
          csm1 = csm1 + CSEmsd(ie,Nejc)*DE
        ENDDO
        csm2 = csm1 - 0.5*CSEmsd(next,Nejc)*DE
 
C------MSD/PCROSS contribution is integrated over the discrete level region and
C------distributed among 2+, 3- and 4+ levels (or those close to such for
C------noninteger spin nuclei) using arbitrary weights (most to 2+ and very
C------little to 4+). Angular distributions for these levels are those
C------provided by TRISTAN or PCROSS at the closest bin.
C
        csmsdl = 0.0
        DO ie = istart, next
          csmsdl = csmsdl + CSEmsd(ie,Nejc)*DE
C        Setting it to zero to delete discrete spectra before redistributing
          IF(ENDf(1).GT.0)THEN
            IF(IDNa(1,6).GT.0.AND.Nejc.EQ.1)CSEmsd(ie,Nejc) = 0.D0
            IF(IDNa(3,6).GT.0.AND.Nejc.EQ.2)CSEmsd(ie,Nejc) = 0.D0
            IF(Nejc.GT.2)CSEmsd(ie,Nejc) = 0.D0
          ENDIF
        ENDDO
        csmsdl = csmsdl - 0.5*CSEmsd(next,Nejc)*DE
 
C
C
C      Inelastic channel
C
        csmtot = 0.D0
        swght = 0.0
        DO il = 2, NLV(Nnur)
          wght(il) = 0.0
          eemi = excnq - ELV(il,Nnur)
          IF(eemi.LT.0.0D0)EXIT
          IF(ABS(XJLv(il,Nnur) - 2.D0).LT.0.6D0.AND.LVP(il,Nnur).EQ.1)
     &       THEN
            wght(il) = 4.0/(ABS(ELV(il,Nnur) + QCC(1)) + 0.2)
            wght(il) = wght(il)**2
            swght = swght + wght(il)
          ENDIF
          IF(ABS(XJLv(il,Nnur) - 3.D0).LT.0.6D0.AND.LVP(il,Nnur)
     &       .EQ.( - 1))THEN
            wght(il) = 2.0/(ABS(ELV(il,Nnur) + QCC(2)) + 0.2)
            wght(il) = wght(il)**2
            swght = swght + wght(il)
          ENDIF
          IF(ABS(XJLv(il,Nnur) - 4.D0).LT.0.6D0.AND.LVP(il,Nnur).EQ.1)
     &       THEN
            wght(il) = 1.0/(ABS(ELV(il,Nnur) + QCC(2) - 1.) + 0.2)
            wght(il) = wght(il)**2
            swght = swght + wght(il)
          ENDIF
        ENDDO
        IF(swght.EQ.0.0D0)THEN
          WRITE(8,*)' WARNING:'
          WRITE(8,*)' WARNING: No level to put msd level contribution ', 
     &              csmsdl, ' mb'
          WRITE(8,*)' WARNING: Load everything to the ground state '
          WRITE(8,*)' WARNING: Ang. dist. of discrete levels ignored'
          WRITE(8,*)' WARNING:'
          POPlv(1,Nnur) = POPlv(1,Nnur) + csmsdl
          RETURN
        ENDIF
        csmsdl = csmsdl/swght
        DO il = 2, NLV(Nnur)
          eemi = excnq - ELV(il,Nnur)
          IF(eemi.LT.0.0D0)RETURN
          csmtot = csmtot + csmsdl*wght(il)
          POPlv(il,Nnur) = POPlv(il,Nnur) + csmsdl*wght(il)
          CSDirlev(il,Nejc) = CSDirlev(il,Nejc) + csmsdl*wght(il)
C--------Store ang. distr. for discrete levels. For each level shape of the closest CSEa
C--------bin is taken and normalized with level population  csmsdl*wght(il)
C--------Find the closest bin
          ie = eemi/DE + 1.5
          ie = MIN(ie,next)
C--------Normalization factor
          IF(CSEmsd(ie,Nejc).NE.0)THEN
            xnor = csmsdl*wght(il)/CSEmsd(ie,Nejc)
          ELSE
            xnor = 0.D0
          ENDIF
C--------Store ang. dist.
          DO na = 1, NDAng
            CSAlev(na,il,Nejc) = CSAlev(na,il,Nejc)
     &                           + xnor*CSEa(ie,na,Nejc,1)
C           Deleting the corresponding angular distribution
            IF(ENDf(1).GT.0)CSEa(ie,na,Nejc,1) = 0.D0
          ENDDO
        ENDDO
 
      ELSE
C
C     Other channels (not the inelastic)
C
        csm1 = 0.D0
        istart = nexrt + 1
        DO ie = istart, next
          csm1 = csm1 + CSEmsd(ie,Nejc)*DE
        ENDDO
        csm2 = csm1 - 0.5*CSEmsd(next,Nejc)*DE
 
        csmtot = 0.D0
        xnor = 0.D0
 
        DO il = NLV(Nnur), 1, -1
          eemi = excnq - ELV(il,Nnur)
          IF(eemi.LT.0.0D0)EXIT
 
          xnor = CSEmsd(istart,Nejc)*DE
C
C        Assigning angular distribution of the first continuum bin "istart"
C        to the angular distribution of the discrete level "il"
C
          DO na = 1, NDAng
            ddxs(na) = CSEa(istart,na,Nejc,1)
          ENDDO
          csmsdl = 0.D0
 
          DO ie = istart, next
            eee = DE*(ie - 1)
            IF(eee.GT.eemi)EXIT
            csmsdl = csmsdl + CSEmsd(ie,Nejc)*DE
            IF(ENDf(1).GT.0)THEN
C            Deleting the corresponding XS from the continuum
C              as it is moved to discrete spectra
              CSEmsd(ie,Nejc) = 0.D0
C            Deleting the corresponding angular distribution
              DO na = 1, NDAng
                CSEa(ie,na,Nejc,1) = 0.D0
              ENDDO
            ENDIF
            istart = ie + 1
          ENDDO
          POPlv(il,Nnur) = POPlv(il,Nnur) + csmsdl
          CSDirlev(il,Nejc) = CSDirlev(il,Nejc) + csmsdl
 
          csmtot = csmtot + csmsdl
C--------Normalization factor
          IF(xnor.GT.0)THEN
            xnor = csmsdl/xnor
          ELSE
            xnor = 0.D0
          ENDIF
C--------Store ang. dist.
          DO na = 1, NDAng
            CSAlev(na,il,Nejc) = CSAlev(na,il,Nejc) + xnor*ddxs(na)
          ENDDO
        ENDDO
      ENDIF
 
      IF(csm2 - csmtot.GT.0.1D0)THEN
        WRITE(8,*)'WARNING: PE discrete levels for nejc=', Nejc
        WRITE(8,*)'WARNING: Difference in in/out flux =', 
     &            SNGL(csm2 - csmtot)
      ELSE
        WRITE(8,*)' PE XS to discrete levels   ', SNGL(csmtot), 
     &            ' for nejc =', Nejc
      ENDIF
      RETURN
      END SUBROUTINE ACCUMSD
 
!---------------------------------------------------------------------------
      SUBROUTINE NILSSON(D,E0,Als,Homega,N,K,V,Beta2,Zall)
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      REAL*8 :: Zall, Als, Beta2, E0, Homega
      INTEGER :: K, N
      REAL*8, DIMENSION(30) :: D
      REAL*8, DIMENSION(30,30) :: V
C
C Local variables
C
      REAL*8, DIMENSION(30,30) :: a
      REAL*8 :: delta, ebar
      REAL*8 :: EHO, VCC
      REAL :: FLOAT
      INTEGER :: i, i1, i2, j, j1, j2, l1, l2, nl, nrot
C
C*** End of declarations rewritten by SPAG
C
CCC
CCC   ******************************************************************
CCC   *                                                                *
CCC   *                          NILSSON                               *
CCC   *                                                                *
CCC   * Sets up and diagonalizes a Nilsson Hamiltonian, with user-     *
CCC   * specified quadrupole deformation, in the spherical HO basis    *
CCC   * within a major oscillator shell*).                             *
CCC   *                                                                *
CCC   * Calls: Jacobi                                                  *
CCC   *        VCC                                                     *
CCC   *                                                                *
CCC   * input:als    - coeff. of L*S term                              *
CCC   *       HOMega - HO energy                                       *
CCC   *       n      - major HO shell q. number                        *
CCC   *       k      - proj. of spin on nucl. symmetry axis            *
CCC   *       BETa2  - quadrupole deformation                          *
CCC   *       Zall    - coeff. of L*L term                              *
CCC   *                                                                *
CCC   * output:d     - N dimensional array of s.p. level energies      *
CCC   *        v     - N*N matrix of s.p. wave functions as expansions *
CCC   *                of spherical radial HO eigenfunctions (columns) *
CCC   *                                                                *
CCC   * author: H.Wienke                                               *
CCC   * date  : 8, 2005                                                *
CCC   * *) see also R.D. Lawson, Theory of the Nuclear Shell Model     *
CCC   *  (Clarendon, Oxford, 1980)                                     *
CCC   ******************************************************************
CCC
C     copyright H.Wienke, Geel 08/2005
C
C
C
 
C
 
 
C    Fill up the Nilsson matrix
 
      delta = .946*Beta2
      ebar = (2.0/3.0)*delta*Homega
      DO i = 1, 30
        D(i) = 0.0
        DO j = 1, 30
          V(i,j) = 0.0
          a(i,j) = 0.0
        ENDDO
      ENDDO
      i2 = N + 1 ! rank matrix
      DO WHILE (i2.GT.0)
        i1 = i2
        DO WHILE (i1.GT.0)
C           first diagonal terms (l1 = l2)
          IF(i1.EQ.i2)THEN
            l1 = i1 - 1
            l2 = l1
            nl = (N - l1 + 2)/2
            j1 = 2*l1 + 1
            j2 = j1
            a(i1,i2) = EHO(nl,l1,j1,Homega,Zall,Als) + E0
            a(i1,i2) = a(i1,i2) - ebar*VCC(j1,4,j2,K,0)*VCC(j1,4,j2,1,0)
     &                 *(2.D0*FLOAT(nl - 1) + FLOAT(l1) + 1.5D0)
            IF(i1.GT.1.AND.i2.GT.1)THEN
              a(i1 - 1,i2 - 1) = EHO(nl,l1,j1 - 2,Homega,Zall,Als) + E0
              a(i1 - 1,i2 - 1) = a(i1 - 1,i2 - 1)
     &                           - ebar*VCC(j1 - 2,4,j2 - 2,K,0)
     &                           *VCC(j1 - 2,4,j2 - 2,1,0)
     &                           *(2.D0*FLOAT(nl - 1) + FLOAT(l1)
     &                           + 1.5D0)
              a(i1 - 1,i2) = ( - 1)**((j1 - 4 - j2)/2)
     &                       *SQRT(FLOAT(j1 - 1)/FLOAT(j2 + 1))
     &                       *ebar*VCC(j1 - 2,4,j2,K,0)
     &                       *VCC(j1 - 2,4,j2,1,0)
     &                       *(2.D0*FLOAT(nl - 1) + FLOAT(l1) + 1.5D0)
              a(i1,i2 - 1) = a(i1 - 1,i2)
            ENDIF
C           off-diagonal terms (l1 = l2 +- 2)
          ELSEIF(i1.EQ.(i2 - 2))THEN
            l1 = i1 - 1
            l2 = i2 - 1
            nl = (N - l2 + 2)/2
            j1 = 2*l1 + 1
            j2 = 2*l2 + 1
            a(i1,i2) = ( - 1)**((j1 - j2 - 2)/2)
     &                 *SQRT(FLOAT(j1 + 1)/FLOAT(j2 + 1))
     &                 *ebar*VCC(j1,4,j2,K,0)*VCC(j1,4,j2,1,0)*( - 2.D0)
     &                 *SQRT(FLOAT(nl)*(FLOAT(nl-1+l2) + .5D0))
            a(i2,i1) = a(i1,i2)
            a(i1,i2 - 1) = ( - 1)**((j1 - j2 - 4)/2)
     &                     *SQRT(FLOAT(j1 + 1)/FLOAT(j2 - 1))
     &                     *ebar*VCC(j1,4,j2 - 2,K,0)
     &                     *VCC(j1,4,j2 - 2,1,0)*( - 2.D0)
     &                     *SQRT(FLOAT(nl)*(FLOAT(nl-1+l2) + .5D0))
            a(i2 - 1,i1) = a(i1,i2 - 1)
            IF(i1.GT.1)THEN
              a(i1 - 1,i2) = ( - 1)**((j1 - 4 - j2)/2)
     &                       *SQRT(FLOAT(j1 - 1)/FLOAT(j2 + 1))
     &                       *ebar*VCC(j1 - 2,4,j2,K,0)
     &                       *VCC(j1 - 2,4,j2,1,0)*( - 2.D0)
     &                       *SQRT(FLOAT(nl)*(FLOAT(nl-1+l2) + .5D0))
              a(i2,i1 - 1) = a(i1 - 1,i2)
              a(i1 - 1,i2 - 1) = ( - 1)**((j1 - 2 - j2 - 4)/2)
     &                           *SQRT(FLOAT(j1 - 1)/FLOAT(j2 - 1))
     &                           *ebar*VCC(j1 - 2,4,j2 - 2,K,0)
     &                           *VCC(j1 - 2,4,j2 - 2,1,0)*( - 2.D0)
     &                           *SQRT(FLOAT(nl)*(FLOAT(nl-1+l2) + .5D0)
     &                           )
              a(i2 - 1,i1 - 1) = a(i1 - 1,i2 - 1)
            ENDIF
          ENDIF
          i1 = i1 - 2
        ENDDO
        i2 = i2 - 2
      ENDDO
      CALL JACOBI(a,N + 1,30,D,V,nrot)
      END SUBROUTINE NILSSON
 
!---------------------------------------------------------------------------
C
      FUNCTION VCC(Jx1,Jx2,Jx3,Mx1,Mx2)
CCC
CCC************************************************************************
CCC                                                                       *
CCC   Clebsch-Gordan Coefficient Routine                                  *
CCC                                                                       *
CCC   Input : JXi   - 2*spin Ji                                           *
CCC           MXi   - 2*magnetic quantumnumber Mi                         *
CCC                                                                       *
CCC   Output: VCC   - <J1,J2,M1,M2|J3,M1+M2>                              *
CCC   Calls : YXFCT                                                       *
CCC           PHASEF                                                      *
CCC   Author: P.D. Kunz, University of Colorado, Boulder, Colorado, US    *
CCC   (from CULIB8 library of routines used in DWUCK4 and CHUCK3)         *
CCC************************************************************************
CCC
      IMPLICIT REAL*8(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8, DIMENSION(0:32) :: FACt
      COMMON /FACTRL/ FACt
C
C Dummy arguments
C
      INTEGER :: Jx1, Jx2, Jx3, Mx1, Mx2
      REAL*8 :: VCC
C
C Local variables
C
      REAL*8 :: fctor, phas
      REAL :: FLOAT
      INTEGER :: IABS, MAX0, MIN0
      INTEGER :: icntr, it, j1, j2, j3, j4, j5, jt1, jt2, jt3, jt4, jt5, 
     &           jz1, jz2, jz3, m1, m2, nu, numax, numin
      REAL*8 :: PHASEF, YXFCT
C
C*** End of declarations rewritten by SPAG
C
C      EXTERNAL FACTOR
C
      VCC = 0.0
      j1 = Jx1
      j2 = Jx2
      j3 = Jx3
      m1 = Mx1
      m2 = Mx2
      IF(j1.LT.j2)THEN
        IF(j3.GE.j1)THEN
          icntr = -1
          it = j1
          j1 = j2
          j2 = it
          it = m1
          m1 = m2
          m2 = it
          GOTO 10
        ENDIF
      ELSEIF(j3.GE.j2)THEN
        icntr = 0
        GOTO 10
      ENDIF
      icntr = 1
      it = j2
      j2 = j3
      j3 = it
      m2 = -m1 - m2
   10 jz1 = (j1 + j2 - j3)/2
      IF(jz1.GE.0)THEN
        jz2 = (j1 + j3 - j2)/2
        IF(jz2.GE.0)THEN
          jz3 = (j2 + j3 - j1)/2
          IF(jz3.GE.0)THEN
            IF(j1.GE.IABS(m1))THEN
              IF(j2.GE.IABS(m2))THEN
                IF(j3.GE.IABS(m1 + m2))THEN
                  jt1 = (j1 - j3 + m2)/2
                  jt2 = (j2 - j3 - m1)/2
                  numin = MAX0(jt1,jt2,0)
                  jt3 = (j1 - m1)/2
                  jt4 = (j2 + m2)/2
                  numax = MIN0(jt3,jt4,jz1)
                  jt5 = (j2 - m2)/2
                  IF(numax.GE.numin)THEN
                    j4 = j1/2
                    j5 = j3/2
                    phas = PHASEF(numin)
                    DO nu = numin, numax
                      VCC = VCC + 
     &                      phas*(YXFCT(jt3 - nu,j4)*YXFCT(nu - jt2,j5))
     &                      /(FACt(jt4 - nu)*FACt(nu - jt1)
     &                      *FACt(jz1 - nu)*FACt(nu))
                      phas = -phas
                    ENDDO
                    fctor = YXFCT(j4,(j1 + m1)/2)*YXFCT(j4,jt3)
     &                      *YXFCT((j1 + j2 + j3)/2 + 1,jz2)
     &                      *YXFCT(j5,(j3 + m1 + m2)/2)
     &                      *YXFCT(j5,(j3 - m1 - m2)/2)*FACt(jz1)
     &                      *FACt(jz3)*FACt(jt4)*FACt(jt5)*FLOAT(j3 + 1)
                    VCC = SQRT(fctor)*VCC
                    IF(icntr.LT.0)THEN
                      VCC = VCC*PHASEF(jz1)
                    ELSEIF(icntr.NE.0)THEN
                      VCC = VCC*SQRT(FLOAT(j2 + 1)/FLOAT(j3 + 1))
     &                      *PHASEF(jt3)
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      RETURN
      END FUNCTION VCC
 
!---------------------------------------------------------------------------
      BLOCK DATA FACTOR
C
C     Factorial table
C
      IMPLICIT REAL*8(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8, DIMENSION(0:32) :: FACt
      COMMON /FACTRL/ FACt
C
C*** End of declarations rewritten by SPAG
C
C
      DATA FACt/1.0000000000E+00, 1.0000000000E+00, 2.0000000000E+00, 
     &     6.0000000000E+00, 2.4000000000E+01, 1.2000000000E+02, 
     &     7.2000000000E+02, 5.0400000000E+03, 4.0320000000E+04, 
     &     3.6288000000E+05, 3.6288000000E+06, 3.9916800000E+07, 
     &     4.7900160000E+08, 6.2270208000E+09, 8.7178291200E+10, 
     &     1.3076743680E+12, 2.0922789888E+13, 3.5568742810E+14, 
     &     6.4023737057E+15, 1.2164510041E+17, 2.4329020082E+18, 
     &     5.1090942172E+19, 1.1240007278E+21, 2.5852016739E+22, 
     &     6.2044840173E+23, 1.5511210043E+25, 4.0329146113E+26, 
     &     1.0888869450E+28, 3.0488834461E+29, 8.8417619937E+30, 
     &     2.6525285981E+32, 8.2228386542E+33, 2.6313083693E+35/
C    $         , 8.6833176188D+36, 2.9523279904D+38, 1.0333147966D+40
C    $         , 3.7199332679D+41, 1.3763753091D+43, 5.2302261747D+44
C    $         , 2.0397882081D+46, 8.1591528325D+47, 3.3452526613D+49
C    $         , 1.4050061178D+51, 6.0415263063D+52, 2.6582715748D+54
C    $         , 1.1962222087D+56, 5.5026221598D+57, 2.5862324151D+59
C    $         , 1.2413915593D+61, 6.0828186403D+62, 3.0414093202D+64
C    $         , 1.5511187533D+66/
      END BLOCK DATA FACTOR
 
!---------------------------------------------------------------------------
 
      FUNCTION PHASEF(N)
CCC
CCC************************************************************************
CCC                                                                       *
CCC   Author: P. D. Kunz, University of Colorado, Boulder, Colorado, US   *
CCC                                                                       *
CCC************************************************************************
CCC
      IMPLICIT REAL*8(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      INTEGER :: N
      REAL*8 :: PHASEF
C
C Local variables
C
      REAL*8 :: DBLE
      INTEGER :: IABS
C
C*** End of declarations rewritten by SPAG
C
      PHASEF = DBLE(1 - 2*IABS(N - 2*(N/2)))
      RETURN
      END FUNCTION PHASEF
 
!---------------------------------------------------------------------------
      FUNCTION YXFCT(M,N)
CCC
CCC************************************************************************
CCC                                                                       *
CCC   Author: P. D. Kunz, University of Colorado, Boulder, Colorado, US   *
CCC                                                                       *
CCC************************************************************************
CCC
      IMPLICIT REAL*8(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      INTEGER :: M, N
      REAL*8 :: YXFCT
C
C Local variables
C
      REAL*8 :: fctor
      INTEGER :: ictrl, nu, numax
C
C*** End of declarations rewritten by SPAG
C
C     COMPUTES NFACT/MFACT                                              YXFCT003
      YXFCT = 1.0
      numax = M - N
      IF(numax.LT.0)THEN
        ictrl = 1
        numax = -numax
        fctor = M
      ELSEIF(numax.EQ.0)THEN
        GOTO 10
      ELSE
        ictrl = 0
        fctor = N
      ENDIF
      DO nu = 1, numax
        fctor = fctor + 1.0
        YXFCT = YXFCT*fctor
      ENDDO
      IF(ictrl.EQ.0)YXFCT = 1.0/YXFCT
   10 RETURN
      END FUNCTION YXFCT
 
!---------------------------------------------------------------------------
 
      SUBROUTINE JACOBI(A,N,Np,D,V,Nrot)
C
C*** Start of declarations rewritten by SPAG
C
C PARAMETER definitions
C
      INTEGER, PARAMETER :: NMAx = 100
C
C Dummy arguments
C
      INTEGER :: N, Np, Nrot
      REAL*8, DIMENSION(Np,Np) :: A, V
      REAL*8, DIMENSION(Np) :: D
C
C Local variables
C
      REAL*8, DIMENSION(NMAx) :: b, z
      REAL*8 :: c, g, h, s, sm, t, tau, thresh
      INTEGER :: i, ip, iq, j
      REAL :: theta
C
C*** End of declarations rewritten by SPAG
C
CCC
CCC*********************************************************************
CCC                                                                    *
CCC   Diagonalizes a N*N matrix                                        *
CCC                                                                    *
CCC   Input: A    - N*N matrix to be diagonalized                      *
CCC          N    - dimension                                          *
CCC                                                                    *
CCC   Output:V    - eigenvectors (columns)                             *
CCC          D    - eigenvalues                                        *
CCC                                                                    *
CCC   Calls : none                                                     *
CCC                                                                    *
CCC   From Numerical Recipes in FORTRAN, W.H.Press, B.P.Flannery,      *
CCC   S.A.Teucholsky,W.T.Vetterling,Cambridge University Press, 1988.  *
CCC                                                                    *
CCC*********************************************************************
CCC
C
C  Local variables
C
      DO ip = 1, N
        DO iq = 1, N
          V(ip,iq) = 0.
        ENDDO
        V(ip,ip) = 1.
      ENDDO
      DO ip = 1, N
        b(ip) = A(ip,ip)
        D(ip) = b(ip)
        z(ip) = 0.
      ENDDO
      Nrot = 0
      DO i = 1, 50
        sm = 0.
        DO ip = 1, N - 1
          DO iq = ip + 1, N
            sm = sm + ABS(A(ip,iq))
          ENDDO
        ENDDO
        IF(sm.EQ.0.)RETURN
        IF(i.LT.4)THEN
          thresh = 0.2*sm/N**2
        ELSE
          thresh = 0.
        ENDIF
        DO ip = 1, N - 1
          DO iq = ip + 1, N
            g = 100.*ABS(A(ip,iq))
            IF((i.GT.4).AND.(ABS(D(ip)) + g.EQ.ABS(D(ip))).AND.
     &         (ABS(D(iq)) + g.EQ.ABS(D(iq))))THEN
              A(ip,iq) = 0.
            ELSEIF(ABS(A(ip,iq)).GT.thresh)THEN
              h = D(iq) - D(ip)
              IF(ABS(h) + g.EQ.ABS(h))THEN
                t = A(ip,iq)/h
              ELSE
                theta = 0.5*h/A(ip,iq)
                t = 1./(ABS(theta) + SQRT(1. + theta**2))
                IF(theta.LT.0.)t = -t
              ENDIF
              c = 1./SQRT(1 + t**2)
              s = t*c
              tau = s/(1. + c)
              h = t*A(ip,iq)
              z(ip) = z(ip) - h
              z(iq) = z(iq) + h
              D(ip) = D(ip) - h
              D(iq) = D(iq) + h
              A(ip,iq) = 0.
              DO j = 1, ip - 1
                g = A(j,ip)
                h = A(j,iq)
                A(j,ip) = g - s*(h + g*tau)
                A(j,iq) = h + s*(g - h*tau)
              ENDDO
              DO j = ip + 1, iq - 1
                g = A(ip,j)
                h = A(j,iq)
                A(ip,j) = g - s*(h + g*tau)
                A(j,iq) = h + s*(g - h*tau)
              ENDDO
              DO j = iq + 1, N
                g = A(ip,j)
                h = A(iq,j)
                A(ip,j) = g - s*(h + g*tau)
                A(iq,j) = h + s*(g - h*tau)
              ENDDO
              DO j = 1, N
                g = V(j,ip)
                h = V(j,iq)
                V(j,ip) = g - s*(h + g*tau)
                V(j,iq) = h + s*(g - h*tau)
              ENDDO
              Nrot = Nrot + 1
            ENDIF
          ENDDO
        ENDDO
        DO ip = 1, N
          b(ip) = b(ip) + z(ip)
          D(ip) = b(ip)
          z(ip) = 0.
        ENDDO
      ENDDO
      WRITE(8,*)'WARNING: 50 iterations should never happen'
      RETURN
      END SUBROUTINE JACOBI
