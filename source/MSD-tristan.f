Ccc   * $Rev: 2301 $
Ccc   * $Author: rcapote $
Ccc   * $Date: 2012-01-25 04:20:05 +0100 (Mi, 25 Jän 2012) $
C
      SUBROUTINE TRISTAN(Nejc,Nnuc,L1maxm,Qm,Qs,XSinl)
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
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C COMMON variables
C
      DOUBLE PRECISION AI, ALSin, ANGle(NDANGecis), AR, CLRn(8,8),      
     &                 CNOrin(8,8), CROs1(30,49,2*NDANGecis),           
     &                   CROs2(30,49,2*NDANGecis),BET2in, GRin(2),      
     &                 DTHeta, ECEntr(5), EFItin(8,8), EOUtmi, EOUtmx,
     &                 ESTep, ETMax, ETMin, EXTcom(10), FACb,
     &                 FAClog(500), FFAc1d(2), FFAc2d(2), FFAc3d(2),
     &                 FFTot(10), GAPin(2), HOMin, Q0, QGRand, QMAx,
     &                 QMIna, QMInb, QS1, QS2, QSTep, RAC,
     &                 RHOb(3*(NDEx+25),11,2), RI, ROPt, RR, THEta1,
     &                 THEta2, U0,
     &                 U9, XSinl, W0, WIDex, WIDexin, WR1(12*NDANGecis),
     &                 WR2(144*NDANGecis)
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
      COMMON /TRINP / WIDexin, GAPin, HOMin,ALSin,EFItin,CNOrin,BET2in,  ! nilsson
     &                GRin                                               ! nilsson
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
      DOUBLE PRECISION a3, ap, at, crose(2*NDEX,NDANGecis,NDANGecis),
     &                 eccm, elab, fn, q1, q2, ri0, rr0
      REAL FLOAT
      INTEGER i1, i2, ic, ic1, ic12, ic1xr, ic2, ic2xr, icp, icpx,
     &        iout2, ka, kb, kread, l1, l12x, l1maxr, l2, l2maxm,
     &        l2maxr, mtb, n, n1, n12, n1x, n2, na, nangle, nb, ne, nlr,
     &        nnb, np, npb, nq, nq12x, nwr1, nwr2, nzb
      INTEGER INT

      XSinl = 0.d0
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
      IF (IOUt.GT.3) OPEN (66,FILE = 'TAPE66',STATUS = 'UNKNOWN')
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

      IF(BET2in.EQ.0.0D+0) BET2in = DEF(1,nnuc)
	WRITE (8,*)
      WRITE (8,
     &'('' beta_2 deformation in Nilsson Hamiltonian (MSD)'', F6.3
     &)') BET2in

      DO na = 1, nangle
         ANGle(na) = ANGles(na)
      ENDDO
      elab = EIN*(A(Nnuc) + AEJc(Nejc))/A(Nnuc)
C-----energy step in spectra
c     ESTep = DE
C-----Use half of the step in MSD calculations to reduce fluctuations
      ESTep = DE/2.0
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
      WRITE (8,99020)
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
            WRITE (8,*) ' INSUFFICIENT DIMENSION OF THE WR1 ARRAY IN'
            WRITE (8,*) ' TRISTAN. MUST BE ', nwr1, ' AT LEAST'
            IF (nwr2.GT.144*NDANG) THEN
               WRITE (8,*) ' '
               WRITE (8,*) ' INSUFFICIENT DIMENSION OF THE WR2 ARRAY IN'
               WRITE (8,*) ' TRISTAN. MUST BE ', nwr2, ' AT LEAST'
            ENDIF
            STOP 'ERROR IN WR1 WR2 DIMENSIONS IN TRISTAN'
         ENDIF
CCCCCC
         REWIND (15)
         REWIND (16)
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
                  IF (icp.EQ.1) WRITE (8,99030) (ANGle(n),n = 1,nangle)
99030             FORMAT (//20X,'1-STEP INPUT LEFT CROSS SECTIONS'//' ',
     &                    ' L1',2X,'Q1',2X,9F12.2)
                  IF (icp.EQ.2) WRITE (8,99035) (ANGle(n),n = 1,nangle)
99035             FORMAT (//20X,'1-STEP INPUT RIGHT CROSS SECTIONS'//
     &                    ' ',' L1',2X,'Q1',2X,9F12.2)
                  DO i1 = 1, IC1x
                     l1 = KEX3*(i1 - 1)
                     q1 = QMAx - QSTep
                     DO nq = 1, NQ1x
                        q1 = q1 + QSTep
                        WRITE (8,99040) l1, q1,
     &                                  (CROs1(nq,i1,na),na = ka,kb)
99040                   FORMAT (' ',I3,F6.2,9E12.5)
                     ENDDO
                  ENDDO
               ENDDO
               IF (NCHanl.EQ.2) GOTO 100
            ENDIF
            DO icp = 1, icpx
               WRITE (8,99065)
               ka = nangle*(icp - 1) + 1
               kb = ka - 1 + nangle
               IF (icp.EQ.1) WRITE (8,99045) (ANGle(n),n = 1,nangle)
99045          FORMAT (//20X,'2-STEP INPUT LEFT CROSS SECTIONS'//' ',
     &                 ' L1 L2  Q1',4X,'Q2  ',9F12.2)
               IF (icp.EQ.2) WRITE (8,99050) (ANGle(n),n = 1,nangle)
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
                     WRITE (8,99055)
99055                FORMAT (/)
                     DO n2 = 1, NQ2x
                        q2 = q2 + QSTep
                        n1x = NQ1x - n2 + 1
                        q1 = q2 - QSTep
                        DO n1 = 1, n1x
                           q1 = q1 + QSTep
                           nq = nq + 1
                           WRITE (8,99060) l1, l2, q1, q2,
     &                            (CROs2(nq,ic,n),n = ka,kb)
99060                      FORMAT (' ',2I3,2F6.1,9E12.5)
                        ENDDO
                     ENDDO
                  ENDDO
               ENDDO
            ENDDO
            WRITE (8,99065)
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
     &                              CROs2,crose,Nejc,XSInl)
      CLOSE (16)
99065 FORMAT ('1')
      END
C
C
      SUBROUTINE INELAS(Est,Nn,Nz,Nebins)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C     All lines labeled with "! nilsson" fall under copyright of H.Wienke,
C     Geel 09/2005
C
C COMMON variables
C
      DOUBLE PRECISION ALSin, ANGle(NDANGecis), AU, AW,
     &                 BETa(3*(NDEx+25),8,8),BET2in,GRin(2),             ! nilsson
     &                 BST(3), BST1(2), CLRn(8,8), CNOrin(8,8), DTHeta,  ! nilsson_newest
     &                 EBCs(500,2), ECEntr(5), EFItin(8,8), EOUtmi,      ! nilsson_newest
     &                 EOUtmx, ESP(500,2), ESTep, ETMax, ETMin, FACb,
     &                 FAClog(500), FFAc1d(2), FFAc2d(2), FFAc3d(2),
     &                 FFTot(10), GAP(2), GAPin(2), GSDm(50), HOMega,
     &                 HOMin, Q0, QGRand, QMAx, QMIna, QMInb, QS1, QS2,
     &                 QSTep, RAC, RHO(3*(NDEx+25),8,8),                 ! nilsson_newest
     &                 RHOb(3*(NDEx+25),11,2),RI,
     &                 RMS(3), ROPt, RR, SREw(21), SREwl(21), SRNew(21),
     &                 U0, U9, UAMp(500,2), VAMp(500,2), VLS(2), W0,
     &                 WIDex, WIDexin,ASP(500,14,2)                      ! nilsson
      INTEGER IA, IB, IC, IC12x, IC1max, IC1x, IC2max, IC2x, ICMax, ID,
     &        IE,IG,L9(10),KSP(500,2),NHOle(2),                          ! nilsson
     &        NQ1x, NQ2x, NRMax(10), NSP(500,2),NTOtal(2)
      COMMON  RHO, SRNew, SREw, SREwl, GSDm, BETa
      COMMON /CCLQ  / IC1max, IC2max, IC1x, IC2x, IC12x, ICMax, NQ1x,
     &                NQ2x
      COMMON /CRAC  / FAClog, RAC, U9, IA, IB, IC, ID, IE, IG, L9
      COMMON /EVBCS / EBCs, VAMp, UAMp
      COMMON /INELA / WIDex, ROPt, CLRn, ETMin, ETMax, RHOb, QS1, QS2,
     &                Q0, FACb, FFTot, NRMax
      COMMON /SPPA  / HOMega, VLS, RMS, BST, GAP, NHOle, NTOtal
      COMMON /SPQN  / ESP, KSP, NSP                                      ! nilsson
      COMMON /SPQN2 / ASP                                                ! nilsson
      COMMON /TRINP / WIDexin, GAPin,HOMin, ALSin, EFItin,CNOrin,BET2in, ! nilsson
     &                GRin
      COMMON /TRINTP/ DTHeta, ANGle, ESTep, EOUtmi, EOUtmx, ECEntr,
     &                QSTep, QMIna, QMInb, QMAx, QGRand, FFAc1d, FFAc2d,
     &                FFAc3d
      COMMON /U_OPT / U0, W0, RR, RI, AU, AW
C
C Dummy arguments
C
      INTEGER Nebins, Nn, Nz
      DOUBLE PRECISION Est(0:3*(NDEx+25))
C
C Local variables
C
      DOUBLE PRECISION a1, a3, ad, aew, anp(2,2), anz, api, bosc,
     &                 cci(8,8), ccm(8,8), ccp(8,8), ccpm(2),            ! nilsson_newest
     &                 ccr(8,8),                                         ! nilsson_newest
     &                 ceff, clex(8,8), clsc(8,8), cneg, cnorm, cpos,    ! nilsson_newest
     &                 cr1(8,8), cr2, cr3(8,8), dci(8,8), dcr(8,8),      ! nilsson_newest
     &                 ddr(2),
c    &                 de3, deqq, dnz, dqqst(40000), dr, dwex, dwsx, e,  ! nilsson
     &                 de3, deqq, dnz,               dr, dwex, dwsx, e,  ! nilsson
     &                 e0, efit(8,8), efitx, egr, em, emi, emisq, ep,    ! nilsson_newest
c    &                 epl, eplsq, eqq, eqqst(40000), eqqx,ess(0:10000), ! nilsson
     &                 epl, eplsq, eqq,               eqqx,ess(0:10000), ! nilsson

     &                 est3, ext, f1, fe, ff1, fltwp1, fourpi, fpi,
     &                 greenr, greenx, greeny, hat, hcorr, homeb, phtrm,
     &                 pxmd, pymd, qqi, qqr, qqx, qqy, r, r1, rd, rdopt,
     &                 rdsq, re1, re2, re3, resid,
     &                 ah(14),ap(14)                                     ! nilsson
      REAL d1, hhh
      DOUBLE PRECISION DWIDTH
      DOUBLE PRECISION VCC                                               ! nilsson
      REAL FLOAT
      INTEGER i,jtw1(14),jtw2(14),k, kcx, kh, kmax, kp, kqq, krt,        ! nilsson
     &        krtx,l1,l2, lm, lmax, lst, lt, lth(14), ltmax, ltmaxr,     ! nilsson
     &        ltmin,ltp(14),ltp1, ltr, lttw, n, nconf(21), ne, nebinx,   ! nilsson
     &        nesx, nh, nlhm, nlpm, nos1, nos2, np, nr, nxmax,
     &        kt, ktp1,                                                  ! nilsson_newest
     &        kth, ktp, iminh,iminp,nrad1,nrad2,n1,n2                    ! nilsson
      INTEGER IABS, INT
      DOUBLE PRECISION rfqqr(0:3*(NDEx+25),8,8),
     &                 rfqqx(0:3*(NDEx+25),8,8),                         ! nilsson_newest
     &                 rfqqy(0:3*(NDEx+25),8,8),                         ! nilsson_newest
     &                 rh0, rho1, rl, rmax, rmosc, rmsgs, rnorm,
     &                 rnp(3,2), rp, rqr, rrr(2), rws, rwsq, t1, t2,
     &                 umatqq, veff, vnorm, w, wbcs, we, wgr, widas,
     &                 wide(0:10000), widea, widgr, wqa, wqq,
c     &                wqqst(40000), wqrex, x, xea(11), xir, xneg, xp,   ! nilsson
     &                               wqrex, xea(8,8), xir, xneg,         ! nilsson
     &                 xpos, yea(8,8), sum                               ! nilsson
      EQUIVALENCE (BST(1),BST1)
      DATA rnp/1.2490D0, -0.5401D0, -0.9582D0, 1.2131D0, -0.4415D0,
     &     0.8931D0/
      DATA anp/0.4899D0, -0.1236D0, 0.4686D0, 0.0741D0/
      DATA eqqx/80.0D0/
      IF(Nebins.GT.3*(NDEx+25))THEN
      WRITE(8,*)' ERROR:   INCREASE DIMENSIONS FOR RESPONSE FUNCTIONS
     & ETC. TO: ',NEBINX
      STOP
      ENDIF
C     WRITE(17,*)NEBINX,ICMAX
      fpi = 4.*PI
      fourpi = 1.0/fpi
C-----selfconsistent strength taken for the l=0 transfer field
      efit(1,1) = 0.0                                                    ! nilsson_newest
      IF (EFItin(1,1).GT.0.0D0) efit(1,1) = EFItin(1,1)                  ! nilsson_newest
      IF (EFItin(1,1).LT.0.0D0) efit(1,1) = 0.0                          ! nilsson_newest
      efit(2,1) = (GDRpar(1,1)*GDRpar(2,1)*GDRpar(3,1) + GDRpar(4,1)     ! nilsson_newest
     &          *GDRpar(5,1)*GDRpar(6,1))                                ! nilsson_newest
     &          /(GDRpar(2,1)*GDRpar(3,1) + GDRpar(5,1)*GDRpar(6,1))     ! nilsson_newest
      IF (EFItin(2,1).GT.0.0D0) efit(2,1) = EFItin(2,1)                  ! nilsson_newest
      IF (EFItin(2,1).LT.0.0D0) efit(2,1) = 0.0                          ! nilsson_newest
      efit(2,2) = (GDRpar(1,1)*GDRpar(2,1)*GDRpar(3,1) + GDRpar(4,1)     ! nilsson_newest
     &          *GDRpar(5,1)*GDRpar(6,1))                                ! nilsson_newest
     &          /(GDRpar(2,1)*GDRpar(3,1) + GDRpar(5,1)*GDRpar(6,1))     ! nilsson_newest
      IF (EFItin(2,2).GT.0.0D0) efit(2,2) = EFItin(2,2)                  ! nilsson_newest
      IF (EFItin(2,2).LT.0.0D0) efit(2,2) = 0.0                          ! nilsson_newest
      efit(3,1) = -QCC(1)                                                ! nilsson_newest
      IF (EFItin(3,1).GT.0.0D0) efit(3,1) = EFItin(3,1)                  ! nilsson_newest
      IF (EFItin(3,1).LT.0.0D0) efit(3,1) = 0.0                          ! nilsson_newest
      efit(3,2) = -QCC(1)                                                ! nilsson_newest
      IF (EFItin(3,2).GT.0.0D0) efit(3,2) = EFItin(3,2)                  ! nilsson_newest
      IF (EFItin(3,2).LT.0.0D0) efit(3,2) = 0.0                          ! nilsson_newest
      IF ((EFItin(3,2).lt.0.001).and.(EFItin(3,2).gt.-.001))             ! nilsson_newest
     &   efit(3,2)=efit(3,1)                                             ! nilsson_newest
      efit(3,3) = -QCC(1)                                                ! nilsson_newest
      IF (EFItin(3,3).GT.0.0D0) efit(3,3) = EFItin(3,3)                  ! nilsson_newest
      IF (EFItin(3,3).LT.0.0D0) efit(3,3) = 0.0                          ! nilsson_newest
      IF ((EFItin(3,3).lt.0.001).and.(EFItin(3,3).gt.-.001))             ! nilsson_newest
     &   efit(3,3)=efit(3,1)                                             ! nilsson_newest
      efit(4,1) = -QCC(2)                                                ! nilsson_newest
      IF (EFItin(4,1).GT.0.0D0) efit(4,1) = EFItin(4,1)                  ! nilsson_newest
      IF (EFItin(4,1).LT.0.0D0) efit(4,1) = 0.0                          ! nilsson_newest
      efit(4,2) = -QCC(2)                                                ! nilsson_newest
      IF (EFItin(4,2).GT.0.0D0) efit(4,2) = EFItin(4,2)                  ! nilsson_newest
      IF (EFItin(4,2).LT.0.0D0) efit(4,2) = 0.0                          ! nilsson_newest
      IF ((EFItin(4,2).lt.0.001).and.(EFItin(4,2).gt.-.001))             ! nilsson_newest
     &   efit(4,2)=efit(4,1)                                             ! nilsson_newest
      efit(4,3) = -QCC(2)                                                ! nilsson_newest
      IF (EFItin(4,3).GT.0.0D0) efit(4,3) = EFItin(4,3)                  ! nilsson_newest
      IF (EFItin(4,3).LT.0.0D0) efit(4,3) = 0.0                          ! nilsson_newest
      IF ((EFItin(4,3).lt.0.001).and.(EFItin(4,3).gt.-.001))             ! nilsson_newest
     &   efit(4,3)=efit(4,1)                                             ! nilsson_newest
      efit(4,4) = -QCC(2)                                                ! nilsson_newest
      IF (EFItin(4,4).GT.0.0D0) efit(4,4) = EFItin(4,4)                  ! nilsson_newest
      IF (EFItin(4,4).LT.0.0D0) efit(4,4) = 0.0                          ! nilsson_newest
      IF ((EFItin(4,4).lt.0.001).and.(EFItin(4,4).gt.-.001))             ! nilsson_newest
     &   efit(4,4)=efit(4,1)                                             ! nilsson_newest
      efit(5,1) = 0.0                                                    ! nilsson_newest
      IF (EFItin(5,1).GT.0.0D0) efit(5,1) = EFItin(5,1)                  ! nilsson_newest
      IF (EFItin(5,1).LT.0.0D0) efit(5,1) = 0.0                          ! nilsson_newest
      efit(5,2) = 0.0                                                    ! nilsson_newest
      IF (EFItin(5,2).GT.0.0D0) efit(5,2) = EFItin(5,2)                  ! nilsson_newest
      IF (EFItin(5,2).LT.0.0D0) efit(5,2) = 0.0                          ! nilsson_newest
      IF ((EFItin(5,2).lt.0.001).and.(EFItin(5,2).gt.-.001))             ! nilsson_newest
     &   efit(5,2)=efit(5,1)                                             ! nilsson_newest
      efit(5,3) = 0.0                                                    ! nilsson_newest
      IF (EFItin(5,3).GT.0.0D0) efit(5,3) = EFItin(5,3)                  ! nilsson_newest
      IF (EFItin(5,3).LT.0.0D0) efit(5,3) = 0.0                          ! nilsson_newest
      IF ((EFItin(5,3).lt.0.001).and.(EFItin(5,3).gt.-.001))             ! nilsson_newest
     &   efit(5,3)=efit(5,1)                                             ! nilsson_newest
      efit(5,4) = 0.0                                                    ! nilsson_newest
      IF (EFItin(5,4).GT.0.0D0) efit(5,4) = EFItin(5,4)                  ! nilsson_newest
      IF (EFItin(5,4).LT.0.0D0) efit(5,4) = 0.0                          ! nilsson_newest
      IF ((EFItin(5,4).lt.0.001).and.(EFItin(5,4).gt.-.001))             ! nilsson_newest
     &   efit(5,4)=efit(5,1)                                             ! nilsson_newest
      efit(5,5) = 0.0                                                    ! nilsson_newest
      IF (EFItin(5,5).GT.0.0D0) efit(5,5) = EFItin(5,5)                  ! nilsson_newest
      IF (EFItin(5,5).LT.0.0D0) efit(5,5) = 0.0                          ! nilsson_newest
      IF ((EFItin(5,5).lt.0.001).and.(EFItin(5,5).gt.-.001))             ! nilsson_newest
     &   efit(5,5)=efit(5,1)                                             ! nilsson_newest
      efit(6,1) = 0.0                                                    ! nilsson_newest
      IF (EFItin(6,1).GT.0.0D0) efit(6,1) = EFItin(6,1)                  ! nilsson_newest
      IF (EFItin(6,1).LT.0.0D0) efit(6,1) = 0.0                          ! nilsson_newest
      efit(6,2) = 0.0                                                    ! nilsson_newest
      IF (EFItin(6,2).GT.0.0D0) efit(6,2) = EFItin(6,2)                  ! nilsson_newest
      IF (EFItin(6,2).LT.0.0D0) efit(6,2) = 0.0                          ! nilsson_newest
      IF ((EFItin(6,2).lt.0.001).and.(EFItin(6,2).gt.-.001))             ! nilsson_newest
     &   efit(6,2)=efit(6,1)                                             ! nilsson_newest
      efit(6,3) = 0.0                                                    ! nilsson_newest
      IF (EFItin(6,3).GT.0.0D0) efit(6,3) = EFItin(6,3)                  ! nilsson_newest
      IF (EFItin(6,3).LT.0.0D0) efit(6,3) = 0.0                          ! nilsson_newest
      IF ((EFItin(6,3).lt.0.001).and.(EFItin(6,3).gt.-.001))             ! nilsson_newest
     &   efit(6,3)=efit(6,1)                                             ! nilsson_newest
      efit(6,4) = 0.0                                                    ! nilsson_newest
      IF (EFItin(6,4).GT.0.0D0) efit(6,4) = EFItin(6,4)                  ! nilsson_newest
      IF (EFItin(6,4).LT.0.0D0) efit(6,4) = 0.0                          ! nilsson_newest
      IF ((EFItin(6,4).lt.0.001).and.(EFItin(6,4).gt.-.001))             ! nilsson_newest
     &   efit(6,4)=efit(6,1)                                             ! nilsson_newest
      efit(6,5) = 0.0                                                    ! nilsson_newest
      IF (EFItin(6,5).GT.0.0D0) efit(6,5) = EFItin(6,5)                  ! nilsson_newest
      IF (EFItin(6,5).LT.0.0D0) efit(6,5) = 0.0                          ! nilsson_newest
      IF ((EFItin(6,5).lt.0.001).and.(EFItin(6,5).gt.-.001))             ! nilsson_newest
     &   efit(6,5)=efit(6,1)                                             ! nilsson_newest
      efit(6,6) = 0.0                                                    ! nilsson_newest
      IF (EFItin(6,6).GT.0.0D0) efit(6,6) = EFItin(6,6)                  ! nilsson_newest
      IF (EFItin(6,6).LT.0.0D0) efit(6,6) = 0.0                          ! nilsson_newest
      IF ((EFItin(6,6).lt.0.001).and.(EFItin(6,6).gt.-.001))             ! nilsson_newest
     &   efit(6,6)=efit(6,1)                                             ! nilsson_newest
      efit(7,1) = 0.0                                                    ! nilsson_newest
      IF (EFItin(7,1).GT.0.0D0) efit(7,1) = EFItin(7,1)                  ! nilsson_newest
      IF (EFItin(7,1).LT.0.0D0) efit(7,1) = 0.0                          ! nilsson_newest
      efit(7,2) = 0.0                                                    ! nilsson_newest
      IF (EFItin(7,2).GT.0.0D0) efit(7,2) = EFItin(7,2)                  ! nilsson_newest
      IF (EFItin(7,2).LT.0.0D0) efit(7,2) = 0.0                          ! nilsson_newest
      IF ((EFItin(7,2).lt.0.001).and.(EFItin(7,2).gt.-.001))             ! nilsson_newest
     &   efit(7,2)=efit(7,1)                                             ! nilsson_newest
      efit(7,3) = 0.0                                                    ! nilsson_newest
      IF (EFItin(7,3).GT.0.0D0) efit(7,3) = EFItin(7,3)                  ! nilsson_newest
      IF (EFItin(7,3).LT.0.0D0) efit(7,3) = 0.0                          ! nilsson_newest
      IF ((EFItin(7,3).lt.0.001).and.(EFItin(7,3).gt.-.001))             ! nilsson_newest
     &   efit(7,3)=efit(7,1)                                             ! nilsson_newest
      efit(7,4) = 0.0                                                    ! nilsson_newest
      IF (EFItin(7,4).GT.0.0D0) efit(7,4) = EFItin(7,4)                  ! nilsson_newest
      IF (EFItin(7,4).LT.0.0D0) efit(7,4) = 0.0                          ! nilsson_newest
      IF ((EFItin(7,4).lt.0.001).and.(EFItin(7,4).gt.-.001))             ! nilsson_newest
     &   efit(7,4)=efit(7,1)                                             ! nilsson_newest
      efit(7,5) = 0.0                                                    ! nilsson_newest
      IF (EFItin(7,5).GT.0.0D0) efit(7,5) = EFItin(7,5)                  ! nilsson_newest
      IF (EFItin(7,5).LT.0.0D0) efit(7,5) = 0.0                          ! nilsson_newest
      IF ((EFItin(7,5).lt.0.001).and.(EFItin(7,5).gt.-.001))             ! nilsson_newest
     &   efit(7,5)=efit(7,1)                                             ! nilsson_newest
      efit(7,6) = 0.0                                                    ! nilsson_newest
      IF (EFItin(7,6).GT.0.0D0) efit(7,6) = EFItin(7,6)                  ! nilsson_newest
      IF (EFItin(7,6).LT.0.0D0) efit(7,6) = 0.0                          ! nilsson_newest
      IF ((EFItin(7,6).lt.0.001).and.(EFItin(7,6).gt.-.001))             ! nilsson_newest
     &   efit(7,6)=efit(7,1)                                             ! nilsson_newest
      efit(7,7) = 0.0                                                    ! nilsson_newest
      IF (EFItin(7,7).GT.0.0D0) efit(7,7) = EFItin(7,7)                  ! nilsson_newest
      IF (EFItin(7,7).LT.0.0D0) efit(7,7) = 0.0                          ! nilsson_newest
      IF ((EFItin(7,7).lt.0.001).and.(EFItin(7,7).gt.-.001))             ! nilsson_newest
     &   efit(7,7)=efit(7,1)                                             ! nilsson_newest
C
C-----check that energy interval is compatible with range of
C-----experimental ex used to determine coupl. const.
C
      efitx = -10.
      DO l = 1, ICMax
         do k=1,l                                                        ! nilsson_newest
            efitx = MAX(efit(l,k),efitx)                                 ! nilsson_newest
         enddo                                                           ! nilsson_newest
      ENDDO
      nebinx = MAX(INT((efitx+5.*WIDex)/ESTep + 1.),Nebins)
      IF(NEBinx.GE.3*(NDEx+25)) THEN
         nexnew = INT(3*FLOAT(NDEx+25)/FLOAT(nebinx)*NEX(1)-1)
         WRITE(8,*) ' '
         WRITE(8,*) 'Insufficent dimensions for response function in'
         WRITE(8,*) 'MSD-tristan.f  Possible solutions:'
         WRITE(8,*) '- decrease NEX in input to ', nexnew
         WRITE(8,*) '- increase NDEX in dimension.h to', nebinx/3-24,
     &              ' and recompile the source'
         WRITE(8,*) '- start MSD at higher incident energy '
         STOP 'Insuficient dimensions in TRISTAN (see output)'
      ENDIF
      IF (nebinx.NE.Nebins .AND. IOUt.GT.3) WRITE (8,99005) nebinx*ESTep
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
         do kt = 1, ltmaxr                                               ! nilsson_newest
            xea(lt,kt) = 0.D0                                            ! nilsson_newest
            yea(lt,kt) = 0.D0                                            ! nilsson_newest
         enddo                                                           ! nilsson_newest
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
      IF (IOUt.GT.3) WRITE (8,99010) rd, ad, rh0, ROPt, rdopt
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
      IF (IOUt.GT.2) WRITE (8,99015) egr, wgr, widas, WIDex, e0, aew
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
      IF (IOUt.GT.2) WRITE (8,99020)
99020 FORMAT (//8X,' RADIAL MOMENTS <R**L>/A OF THE G.S. DENSITY'/5X,
     &        '           ( UNITS: [FM**L/A] )             '/4X,'L',11X,
     &        'L  ',11X,'L+1',11X,'L+2',11X,'L+3',11X,'L+4')
      lst = 5
      DO l = 1, lmax, lst
         l1 = l
         l2 = MIN(lmax,l1 + lst - 1)
         IF (IOUt.GT.2) WRITE (8,99025) l - 1, (GSDm(n)*api,n = l1,l2)
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
         WRITE (8,99030) bosc, BST(3), bosc/BST(3), BST1
99030    FORMAT (//'  OSCILLATOR LENGTH:'/'  B(DENSITY):',F7.3,
     &           '  B(NILSSON):',F7.3,'  RATIO:',F7.4/'  B(PROTON) :',
     &           F7.3,'  B(NEUTRON):',F7.3)
         WRITE (8,99035) homeb, HOMega, hcorr
99035    FORMAT (/'  OSCILLATOR ENERGY:'/'  E(DENSITY):',F7.3,
     &           '  E(NILSSON):',F7.3,'  RATIO:',F7.4)
         WRITE (8,99040) (SQRT(RMS(i)),i = 1,3)
99040    FORMAT (/'  <R**2> (IN [FM]) :'/'  PROTON    :',F7.3,
     &           '  NEUTRON   :',F7.3,'  TOTAL:',F7.4)
         WRITE (8,99045) vnorm
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
               do kt = 1, ICMax                                          ! nilsson_newest
                  IF (ne.GE.1) RHO(ne,lt,kt) = 0.0                       ! nilsson_newest
                  rfqqr(ne,lt,kt) = 0.0                                  ! nilsson_newest
                  rfqqx(ne,lt,kt) = 0.0                                  ! nilsson_newest
                  rfqqy(ne,lt,kt) = 0.0                                  ! nilsson_newest
               enddo                                                     ! nilsson_newest
               IF (krt.EQ.1 .AND. ne.GE.1) THEN
                  RHOb(ne,lt,1) = 0.0
                  RHOb(ne,lt,2) = 0.0
               ENDIF
            ENDDO
         ENDDO
C
         DO lt = 1, ICMax
            do kt = 1, ICMax                                             ! nilsson_newest
               clsc(lt,kt) = 0.                                          ! nilsson_newest
               clex(lt,kt) = 0.                                          ! nilsson_newest
               ccr(lt,kt) = 0.D0                                         ! nilsson_newest
               cci(lt,kt) = 0.D0                                         ! nilsson_newest
               dcr(lt,kt) = 0.D0                                         ! nilsson_newest
               dci(lt,kt) = 0.D0                                         ! nilsson_newest
               cr1(lt,kt) = 0.D0                                         ! nilsson_newest
               cr3(lt,kt) = 0.D0                                         ! nilsson_newest
               ccm(lt,kt) = 0.D0                                         ! nilsson_newest
               ccp(lt,kt) = 0.D0                                         ! nilsson_newest
            enddo                                                        ! nilsson_newest
            nconf(lt) = 0.0                                              ! nilsson_newest
            SREw(lt) = 0.0                                         ! nilsson_newest
            SRNew(lt) = 0.0                                        ! nilsson_newest
            SREwl(lt) = 0.0                                        ! nilsson_newest
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
               do i=1,14                                                 ! nilsson
                  lth(i)= 0                                              ! nilsson
                  jtw1(i) = 0                                            ! nilsson
                  ah(i) = 0.0                                            ! nilsson
               enddo                                                     ! nilsson
               nos1 = NSP(nh,kh)
               kth  = KSP(nh,kh)
               iminh = (kth -1)/2                                        ! nilsson
C fill up the arrays lth[], ah[] and jtw1[]                              ! nilsson
               l = nos1                                                  ! nilsson
               i = nos1 + 1 -iminh                                       ! nilsson
               do while ((l.ge.0).and.(i.gt.0))                          ! nilsson
                  lth(i) = l                                             ! nilsson
                  jtw1(i) = 2*l+1                                        ! nilsson
                  ah(i) = asp(nh,i,kh)                                   ! nilsson
                  if(i.gt.1) then                                        ! nilsson
                     i = i-1                                             ! nilsson
                     lth(i) = l                                          ! nilsson
                     jtw1(i) = 2*l-1                                     ! nilsson
                     ah(i) = asp(nh,i,kh)                                ! nilsson
                  endif                                                  ! nilsson
                  i = i-1                                                ! nilsson
                  l = l-2                                                ! nilsson
               enddo                                                     ! nilsson
               IF (VAMp(nh,kh).GE.0.01D0) THEN
                  DO np = 1, nlpm
                     do i=1,14                                           ! nilsson
                        ltp(i)= 0                                        ! nilsson
                        jtw2(i) = 0                                      ! nilsson
                        ap(i) = 0.0                                      ! nilsson
                     enddo                                               ! nilsson
                     IF (UAMp(np,kp).GE.0.01D0) THEN
                        wbcs = VAMp(nh,kh)*UAMp(np,kp) + UAMp(nh,kh)
     &                         *VAMp(np,kp)
                        IF (wbcs.GE.0.01D0) THEN
                           nos2 = NSP(np,kp)                             ! nilsson
                           ktp = KSP(np,kp)                              ! nilsson
                           iminp = (ktp - 1)/2                           ! nilsson
C fill up the arrays ltp[], ap[] and jtw2[]                              ! nilsson
                           l = nos2                                      ! nilsson
                           i = nos2 + 1 -iminp                           ! nilsson
                           do while ((l.ge.0).and.(i.gt.0))              ! nilsson
                              ltp(i) = l                                 ! nilsson
                              jtw2(i) = 2*l+1                            ! nilsson
                              ap(i) = asp(np,i,kp)                       ! nilsson
                              if(i.gt.1) then                            ! nilsson
                                 i = i-1                                 ! nilsson
                                 ltp(i) = l                              ! nilsson
                                 jtw2(i) = 2*l-1                         ! nilsson
                                 ap(i) = asp(np,i,kp)                    ! nilsson
                              endif                                      ! nilsson
                              i = i-1                                    ! nilsson
                              l = l-2                                    ! nilsson
                           enddo                                         ! nilsson
C
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
c The following statements are commented out since wqqst, dqqst
c  and eqqst are never used (h.wienke).
c                              wqqst(kc) = wqq*2.D0
c                              dqqst(kc) = deqq
                              eqq = eqq + deqq
c                              eqqst(kc) = eqq
C
C-----------------------------end of energy shift
C
                              do i = 1, nos1+1 - iminh                   ! nilsson
                                 do j = 1, nos2+1 - iminp                ! nilsson
                                    l1 = IABS(ltp(j)-lth(i))             ! nilsson
                                    l2 = IABS(jtw2(j)-jtw1(i))/2         ! nilsson
                                    if ((i.eq.1).and.(j.eq.1)) then      ! nilsson
                                       ltmin = MAX(l1,l2)                ! nilsson
                                    else                                 ! nilsson
                                       if (MAX(l1,l2).ge.ltmin) then     ! nilsson
                                       else                              ! nilsson
                                          ltmin = MAX(l1,l2)             ! nilsson
                                       endif                             ! nilsson
                                    endif                                ! nilsson
                                    IF (MOD(ltmin-l1,2).EQ.1)
     &                                 ltmin = ltmin + 1
                                    if ((i.eq.1).and.(j.eq.1)) then      ! nilsson
                                        ltmax = MIN(ltp(j)+lth(i),       ! nilsson
     &                                 (jtw1(i)+jtw2(j))/2)+1            ! nilsson
                                    else
                                       if ((MIN(ltp(j)+lth(i),(jtw1(i)+  ! nilsson
     &                                 jtw2(j))/2)+1).le.ltmax) then     ! nilsson
                                       else                              ! nilsson
                                       ltmax = MIN(ltp(j)+lth(i),        ! nilsson
     &                                    (jtw1(i)+jtw2(j))/2)+1         ! nilsson
                                       endif                             ! nilsson
                                    endif                                ! nilsson
                                 enddo                                   ! nilsson
                              enddo                                      ! nilsson
                              ltmin = ltmin + 1
                              ltmax = MIN(ltmax,ltmaxr)
                              do n1= 1,2,1                               ! nilsson_newest
                              kth = iabs(kth)                            ! nilsson_newest
                              do n2=1,2,1                                ! nilsson_newest
                              ktp1 = (iabs(ktp - kth))/2  +1             ! nilsson_newest
                              IF (ltmin.LE.ltmax) THEN
                                 DO ltp1 = ltmin, ltmax, 2
                                    lttw = 2*(ltp1 - 1)
C
C-----------------------------------calculate the 2-qp matrix elements
C-----------------------------------REDUQQ = reduced matrix element (squared)
C-----------------------------------PHTRM = radial  matrix element
C-----------------------------------UMATQQ = full    matrix element (squared)
                                    sum = 0.0D0                          ! nilsson
                                    do i=1,nos1+1- iminh                 ! nilsson
                                       do j=1,nos2+1- iminp              ! nilsson
                                    if (ABS(ah(i)*ap(j)).lt.0.00001D0)
     &                              then                                 ! nilsson
                                    else                                 ! nilsson
                                       IA = jtw1(i)                      ! nilsson
                                       IB = jtw2(j)                      ! nilsson
                                       IC = lttw
                                       ID = 1
                                       IE = -1
                                       IG = 0
                                       CALL CLEBTRI
                                       r1 = RAC
                                       r1 =(VCC(IB,IA,IC,ktp,-kth)*      ! nilsson_new
     &                                 (-1)**((IA-kth)/2))*r1            ! nilsson_new
                                       r1 = sqrt(float(IA+1)*            ! nilsson_new
     &                                      float(IB+1))*r1              ! nilsson_new
                                       r1 = r1/sqrt(float(IC+1))         ! nilsson_new
                                       r1 = ah(i)*ap(j)*r1               ! nilsson
                                       if (abs(r1).gt.1.D-10) then       ! nilsson
                                          ltr = lttw/2                   ! nilsson
                                          nrad1=(nos1-lth(i))/2          ! nilsson
                                          nrad2=(nos2-ltp(j))/2          ! nilsson
                                    CALL RADIAL(bosc,phtrm,nrad1,lth(i),
     &                                 nrad2,ltp(j),ltr)
                                          sum = sum + r1*phtrm           ! nilsson
                                       endif                             ! nilsson
                                    endif                                ! nilsson
                                       enddo                             ! nilsson
                                    enddo                                ! nilsson
                                    IF (ltr.GT.0)
     &                                  sum = sum*vnorm/rws**ltr         ! nilsson
                                    IF (ltr.EQ.0)
     &                              sum = sum*vnorm/rws**2               ! nilsson
                                    umatqq = (sum*wbcs)**2               ! nilsson_newest
                                    hat = 1.D0                           ! nilsson
                                    umatqq = umatqq*hat*fourpi/          ! nilsson
     &                                   FLOAT(lttw + 1)                 ! nilsson

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
                                         rfqqr(ne,ltp1,ktp1) =           ! nilsson_newest
     &                                   rfqqr(ne,ltp1,ktp1)             ! nilsson_newest
     &                                      + greenr
                                         rfqqx(ne,ltp1,ktp1) =           ! nilsson_newest
     &                                   rfqqx(ne,ltp1,ktp1)             ! nilsson_newest
     &                                      + greenx
                                         rfqqy(ne,ltp1,ktp1) =           ! nilsson_newest
     &                                   rfqqy(ne,ltp1,ktp1)             ! nilsson_newest
     &                                      + greeny
                                       ENDDO
C
C calculate the 2qp response function at the fitting energy
C
                                       IF(efit(ltp1,ktp1).NE.0.0D0)THEN
                                         i = 1
                                         ext = efit(ltp1,ktp1)
     &                                         - ESTep/10.D0
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
                                         cr1(ltp1,ktp1) =                ! nilsson_newest
     &                                   cr1(ltp1,ktp1) + greenr         ! nilsson_newest
                                         ccm(ltp1,ktp1) = ccm(ltp1,ktp1) ! nilsson_newest
     &                                      + greenx +
     &                                      0.5D0*WIDex*greeny
                                         ELSEIF (i.EQ.2) THEN
                                         ccr(ltp1,ktp1) = ccr(ltp1,ktp1) ! nilsson_newest
     &                                   + greenr                        ! nilsson_newest
                                         xea(ltp1,ktp1) = xea(ltp1,ktp1) ! nilsson_newest
     &                                   + greenx                        ! nilsson_newest
                                         yea(ltp1,ktp1) = yea(ltp1,ktp1) ! nilsson_newest
     &                                   + greeny                        ! nilsson_newest
                                         cci(ltp1,ktp1) = cci(ltp1,ktp1) ! nilsson_newest
     &                                      + greenx +
     &                                      0.5D0*WIDex*greeny
C
C calculate the 1'st derivative of the 2qp response function at the fitting energy
C
                                         dcr(ltp1,ktp1) = dcr(ltp1,ktp1) ! nilsson_newest
     &                                      + ((emisq - dwsx)*pxmd**2 -
     &                                      (eplsq - dwsx)*pymd**2)
     &                                      *umatqq
                                         dci(ltp1,ktp1) = dci(ltp1,ktp1) ! nilsson_newest
     &                                      + ((2.D0*wqq + WIDex)
     &                                      *(emi*pxmd**2 + epl*pymd**2)
     &                                      )*umatqq
                                         ELSEIF (i.EQ.3) THEN
                                         cr3(ltp1,ktp1) = cr3(ltp1,ktp1) ! nilsson_newest
     &                                   + greenr
                                         ccp(ltp1,ktp1) = ccp(ltp1,ktp1) ! nilsson_newest
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
                              kth = - kth                                ! nilsson_newest
                              ENDDO                                      ! nilsson_newest
                              ktp = - ktp                                ! nilsson_newest
                              ENDDO                                      ! nilsson_newest
                           ENDIF
                        ENDIF
                     ENDIF
                  ENDDO
               ENDIF
            ENDDO
         ENDDO
         kcx = kc
         kqq = 1
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
            do kt = 1, lt                                                ! nilsson_newest
C
C           INVERSE COUPLING CONSTANTS (FROM FIT TO EXP. ENERGIES)
C
            IF (efit(lt,kt).NE.0.0D0) THEN                               ! nilsson_newest
               IF (dcr(lt,kt).NE.0.0D0) THEN                             ! nilsson_newest
                  xir = dci(lt,kt)/dcr(lt,kt)                            ! nilsson_newest
                  xpos = cci(lt,kt)*xir/(1.D0 + SQRT(1.D0 + xir**2))     ! nilsson_newest
                  xneg = -cci(lt,kt)**2/xpos                             ! nilsson_newest
                  cpos = ccr(lt,kt) + xpos                               ! nilsson_newest
                  cneg = ccr(lt,kt) + xneg                               ! nilsson_newest
                  ccpm(1) = cpos
                  ccpm(2) = cneg
C
C-----------------check   for maximum
C
C                 cr1 = rfqqr(nea - 1, lt)
                  cr2 = ccr(lt,kt)                                       ! nilsson_newest
C                 cr3 = rfqqr(nea + 1, lt)
                  clex(lt,kt) = 0.D0                                     ! nilsson_newest
                  DO j = 1, 2
                     re1 = ccm(lt,kt)/((ccpm(j) - cr1(lt,kt))**2 +       ! nilsson_newest
     &                   ccm(lt,kt)**2)                                  ! nilsson_newest
                     re2 = cci(lt,kt)/((ccpm(j) - cr2)**2 +              ! nilsson_newest
     &                   cci(lt,kt)**2)                                  ! nilsson_newest
                     re3 = ccp(lt,kt)/((ccpm(j) - cr3(lt,kt))**2 +       ! nilsson_newest
     &                   ccp(lt,kt)**2)                                  ! nilsson_newest
                     ddr(j) = (re3 + re1 - 2.D0*re2)/ESTep**2
                     rrr(j) = re2
                  ENDDO
                  IF (ddr(1).LT.0.D0 .AND. ddr(2).GT.0.D0) clex(lt,kt)   ! nilsson_newest
     &                = ccpm(1)
                  IF (ddr(2).LT.0.D0 .AND. ddr(1).GT.0.D0) clex(lt,kt)   ! nilsson_newest
     &                = ccpm(2)
                  IF (ddr(1).LT.0.D0 .AND. ddr(2).LT.0.D0) THEN
                     IF (rrr(1).GT.rrr(2)) clex(lt,kt) = ccpm(1)         ! nilsson_newest
                     IF (rrr(2).GT.rrr(1)) clex(lt,kt) = ccpm(2)         ! nilsson_newest
                  ENDIF
C                 cr1 = rfqqr(nea - 1, lt)
                  cr2 = ccr(lt,kt)                                       ! nilsson_newest
C                 cr3 = rfqqr(nea + 1, lt)
                  re1 = ccm(lt,kt)/((clex(lt,kt) - cr1(lt,kt))**2 +      ! nilsson_newest
     &                ccm(lt,kt)**2)                                     ! nilsson_newest
                  re2 = cci(lt,kt)/((clex(lt,kt) - cr2)**2 +             ! nilsson_newest
     &                cci(lt,kt)**2)                                     ! nilsson_newest
                  re3 = ccp(lt,kt)/((clex(lt,kt) - cr3(lt,kt))**2 +      ! nilsson_newest
     &                ccp(lt,kt)**2)                                     ! nilsson_newest
               ELSE
                  clex(lt,kt) = 0.0                                      ! nilsson_newest
               ENDIF
               IF (clex(lt,kt).EQ.0.D0) THEN                             ! nilsson_newest
                  WRITE (8,99065) lt - 1,kt-1, efit(lt,kt), ddr          ! nilsson_newest
99065             FORMAT (/'WARNING: From TRISTAN:'/
     &                 'WARNING: - No fit of response function for J/K=' ! nilsson_newest
     &                  ,I3,I3/'WARNING: E(EXP.):',F8.2,'  2nd deriv.:', ! nilsson_newest
     &                   2E13.5/
     &             'WARNING: Energy is inconsistent with 2-qp spectrum!'
     &             /'WARNING: Self-consistent response is used!')
                  efit(lt,kt) = 0.D0                                     ! nilsson_newest
                  clex(lt,kt) = 0.D0                                     ! nilsson_newest
                  xea(lt,kt) = 0.D0                                      ! nilsson_newest
                  yea(lt,kt) = 0.D0                                      ! nilsson_newest
C                 xea(lt) = rfqqx(nea, lt)
C                 yea(lt) = rfqqy(nea, lt)
               ENDIF
            ENDIF
C
            l = lt - 1
            fltwp1 = 2*l + 1
            lm = MAX(2*l - 1,1)
            IF (l.EQ.1 .OR. efit(lt,kt).GT.homeb) THEN                   ! nilsson_newest
               IF (l.GT.1) clsc(lt,kt) = vnorm*GSDm(lm)*SQRT(fltwp1)     ! nilsson_newest
     &                                *rdsq/(rd*rws)**l
C--------------isovector strength is chosen as 1/2 of the isoscalar strength
               IF (l.EQ.1) clsc(lt,kt) = -0.5D0*vnorm*GSDm(2)            ! nilsson_newest
     &                                *4.D0/(rws*SQRT(fltwp1))
            ELSE
C-----------vibrational model  (good for low-energy states)
               clsc(lt,kt) = vnorm*GSDm(l + 1)*(l + 3)                   ! nilsson_newest
     &                    /(SQRT(fltwp1)*rws**l)
            ENDIF
C
C-----------monopole case (0+) - compressional mode
C-----------factor pi**2/6 comes from series expansion of
C-----------the   0+ transition potential
C
            IF (l.EQ.0) clsc(lt,kt) = 2.*(vnorm/rwsq)*                   ! nilsson_newest
     &          GSDm(3)*36.D0/PI**4                                      ! nilsson_newest
            IF (efit(lt,kt).LE.0.0D0) THEN                               ! nilsson_newest
               clex(lt,kt) = clsc(lt,kt)/float(2*(lt-1)+1)               ! nilsson_newest
               if (kt.gt.1)clex(lt,kt)=clex(lt,kt)*2.0D0                 ! nilsson_newest
            ELSE
            ENDIF
            CLRn(lt,kt) = clsc(lt,kt)/clex(lt,kt)                        ! nilsson_newest
C
C-----------effective coupling constant is used in the response functions
C           ( if EFIT is .NE. 0 )
C
            ceff = 1./clex(lt,kt)                                        ! nilsson_newest
            cnorm = ceff*ceff
            rnorm = cnorm/CLRn(lt,kt)**2                                 ! nilsson_newest
C-----------normalization of the response function with the factor given in input
            rnorm = rnorm*CNOrin(lt,kt)                                  ! nilsson_newest
C
C-----------calculate the QRPA response function                    (rho )
C-----------calculate the QRPA deformation parameters               (beta)
C
            DO ne = 1, nebinx
               qqr = rfqqr(ne,lt,kt)*cnorm                               ! nilsson_newest
               qqx = rfqqx(ne,lt,kt)*cnorm                               ! nilsson_newest
               qqy = rfqqy(ne,lt,kt)*cnorm                               ! nilsson_newest
               qqy = MAX(1.D-06,qqy)
               wqa = qqx/qqy
               wqrex = 0.5*WIDex + wqa
               qqi = wqrex*qqy
               rqr = ceff - qqr
               rho1 = rnorm*qqi/(rqr**2 + qqi**2)
               RHO(ne,lt,kt) = rho1/PI                                   ! nilsson_newest
            ENDDO
C
C-----------calculate RPA-deformation parameters beta
C-----------(integrated over an energy interval of 2*ESTEP )
C-----------(except for the first and last mesh points   )
C
            BETa(1,lt,kt) = 0.5*ESTep*(RHO(1,lt,kt) + RHO(2,lt,kt))     ! nilsson_newest
            BETa(nebinx,lt,kt) = 0.5*ESTep*(RHO(nebinx,lt,kt) +         ! nilsson_newest
     &      RHO(nebinx - 1,lt,kt))                                      ! nilsson_newest
            DO ne = 2, nebinx - 1
               BETa(ne,lt,kt) = est3*(RHO(ne - 1,lt,kt) +               ! nilsson_newest
     &         4.*RHO(ne,lt,kt) + RHO(ne + 1,lt,kt))                    ! nilsson_newest
            ENDDO
            DO ne = 1, nebinx
              if(BETa(ne,lt,kt).ge.0.0D0) then                          ! nilsson_newest
                  BETa(ne,lt,kt) = SQRT(BETa(ne,lt,kt))                 ! nilsson_newest
              else                                                      ! nilsson
              endif                                                     ! nilsson
            ENDDO
            enddo                                                       ! nilsson_newest (kt)
         ENDDO

C
C-----write response function on TAPE17
C
C        DO NE=1,NEBINX
C        WRITE(17,1400)EST(NE),(RHO(NE,I),I=1,MIN(10,ICMAX))
C        ENDDO
C
         IF (krt.EQ.1) WRITE (8,99080)
99080    FORMAT (//6X,'COUPLING CONSTANTS AND RENORMALIZATION'/6X,
     &           '       ( INELASTIC EXCITATION )      '/1X,'L/K',4X,
     &           'EA  ',2X,'SELF-CON',3X,'EMPIRICAL',6X,'RATIO',7X,
     &           'VEFF',4X,'RESIDUE',6X,'WIDTH',' CONFIG.')
         IF (krt.EQ.2) WRITE (8,99085)
99085    FORMAT (//6X,'COUPLING CONSTANTS AND RENORMALIZATION'/6X,
     &           '   ( CHARGE EXCHANGE EXCITATION )    '/1X,'L/K',4X,
     &           'EA  ',3X,'SELF-CON',3X,'EMPIRICAL',6X,'RATIO',4X,
     &           'RESIDUE',6X,'WIDTH',' CONFIG.')
         DO l = 1, ICMax
            do k = 1,l                                                   ! nilsson_newest
            clsc(l,k) = 1./clsc(l,k)                                     ! nilsson_newest
            clex(l,k) = 1./clex(l,k)                                     ! nilsson_newest
            IF (yea(l,k).NE.0.0D0) THEN                                  ! nilsson_newest
               resid = 1./yea(l,k)                                       ! nilsson_newest
               widea = 2.0*xea(l,k)/yea(l,k)                             ! nilsson_newest
            ELSE
               resid = 0.0
               widea = 0.0
            ENDIF
            veff = vnorm*CLRn(l,k)                                       ! nilsson_newest
            WRITE (8,99090)l-1,k-1, efit(l,k), clsc(l,k), clex(l,k),     ! nilsson_newest
     &            CLRn(l,k),veff, resid, widea, nconf(l)                 ! nilsson_newest
99090       FORMAT (I2,I2,F8.3,2E11.4,2F11.4,E11.4,F11.4,I7)             ! nilsson_newest
            enddo                                                        ! nilsson_newest
         ENDDO
         WRITE (8,99095) eqqx
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
               do ktp1=1,ltp1                                            ! nilsson_newest
                  t2 = hhh*RHO(ne,ltp1,ktp1)                             ! nilsson_newest
                  SRNew(ltp1) = SRNew(ltp1) + t2                         ! nilsson_newest
                  SREw(ltp1) = SREw(ltp1) + t2*eqq                       ! nilsson_newest
                  IF (eqq.LE.ETMax) SREwl(ltp1) =                        ! nilsson_newest
     &            SREwl(ltp1) + t2*eqq                                   ! nilsson_newest
               enddo                                                     ! nilsson_newest
            ENDDO
         ENDDO
C        WRITE(8,607)RD,(LT-1,LT=1,LTMAXR)
C        607 FORMAT(1H1/9X,'BETA-VALUES FOR TARGET EXCITATIONS  RD=',
C        1 F7.4//1H ,4X,' EX',11(5X,I3,3X))
C        DO 110 NE=1,NEBINX
C        110 WRITE(8,608)EST(NE),(BETA(NE,LT),LT=1,LTMAXR)
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

      DOUBLE PRECISION EBCs(500,2), ESP(500,2), GSD(201, 3),
     &                 GSP(201, 2), RST(201), UAMp(500,2), VAMp(500,2)   ! nilsson
     &                 , WFR(201,100)
      INTEGER IBLk(2), NSP(500,2), KSP(500,2), BLKSP(500,2),             ! nilsson
     &        NRX                                                        ! nilsson
      COMMON /BLOCK / IBLk, BLKSP                                        ! nilsson
      COMMON /DENS  / GSD, GSP
      COMMON /EVBCS / EBCs, VAMp, UAMp
      COMMON /RADI  / RST, NRX
      COMMON /SPQN  / ESP, KSP, NSP                                      ! nilsson
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
c         WRITE (8,99005) text(I3), NBL(I3), LBL(I3), JBL(I3)
c99005    FORMAT (/5X,'PAIRING WITH BLOCKING FOR ',A10/5X,'N:',I3,'  L:',
c     &           I3,'  J:',I3,'/2')
      ELSE
         WRITE (8,99010) text(I3)
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
      IF (ii.GT.imax) WRITE (8,*)
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
            WRITE (8,99015) text(I3), Gap, Chemic, xnum
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
               WRITE(8, 99020)i, KSP(i, I3), NSP(i, I3),                 ! nilsson
     &         e,    VAMp(i, I3), vsq, eh, ep                            ! nilsson
99020          FORMAT(I5, 2I4, '/2', F9.4, 2F8.5, 2X, 2F8.3)             ! nilsson

C
C--------------TEMPORARY
C--------------(SKIP CALCULATION OF MICROSC. G.S. DENSITIES)
C
               gjj = 0.0D0                                               ! nilsson
               pjj = 0.0D0                                               ! nilsson
               IF (vsq.LT.0.0D0) THEN
                  IF(IBLk(I3).EQ.0)THEN                                  ! nilsson
                     gjj = 2.D0*vsq/fpi                                  ! nilsson
                  ELSE                                                   ! nilsson
                     IF(BLKSP(i, I3).EQ.1)                               ! nilsson
     &               gjj = (2.d0*vsq + (usq - vsq))/fpi                  ! nilsson
                  ENDIF                                                  ! nilsson
                  pjj = 2.d0*SQRT(usq*vsq)/fpi                           ! nilsson
                  DO k = 1, NRX
                     fsq = WFR(k,n)**2
                     GSD(k,I3) = GSD(k,I3) + gjj*fsq
                     GSP(k,I3) = GSP(k,I3) + pjj*fsq
                  ENDDO
               ENDIF                                                     ! nilsson
            ENDDO
            IF (NRX.GE.0) RETURN
            WRITE (8,99025)
99025       FORMAT (//5X,'DENSITY AND PAIRING DENSITY'/8X,'R',6X,
     &              'DENSITY',6X,'PAIRING')
            nrxx = MIN(12.D0/(RST(2) - RST(1)),1.D0*NRX)
            nst = MAX(1,nrxx/25)
            DO n = 1, nrxx, nst
               k = MAX(1,n - 1)
               WRITE (8,99030) RST(k), GSD(k,I3), GSP(k,I3)
99030          FORMAT (F9.2,5E13.5)
            ENDDO
         ELSE
            IF (ii.GT.imax) THEN
               WRITE (8,*) ' NO CONVERGENCE FOR CHEMICAL POTENTIAL'
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
C     All lines labeled with "! nilsson" fall under copyright of H.Wienke,
C     Geel 09/2005

C
C COMMON variables
C
      DOUBLE PRECISION ESP(500,2)
      INTEGER KSP(500, 2), NSP(500, 2),IBLK(2),BLKSP(500,2)              ! nilsson
      COMMON /BLOCK / IBLk, BLKSP                                        ! nilsson
      COMMON /SPQN  / ESP,KSP, NSP                                       ! nilsson

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
         fjj = 2.0                                                      ! nilsson
         IF (IBLk(I3).NE.0) THEN
            IF(BLKSP(n, I3).ne.0)                                       ! nilsson
     &         fjj = fjj + (1.0 - 2.0*vsq)/vsq
         ENDIF
         Xnum = Xnum + fjj*vsq
         s1 = s1 + fjj/e
         s2 = s2 + fjj*(1. - ESP(n,I3)/e)
      ENDDO
      D = Chem + (s2 - 2.*A)/s1
      END
C
      SUBROUTINE ESORT(Ndim,I3)
C     All lines labeled with "! nilsson" fall under copyright of H.Wienke,
C     Geel 09/2005

C
C COMMON variables
C
      DOUBLE PRECISION EBCs(500, 2), ESP(500, 2), UAMp(500, 2),          ! nilsson
     &                 VAMp(500, 2), ASP(500,14,2)                       ! nilsson
      INTEGER KK(500, 2), NN(500, 2)                                     ! nilsson
      COMMON /EVBCS / EBCs, VAMp, UAMp
      COMMON /SPQN  / ESP, KK, NN                                        ! nilsson
      COMMON /SPQN2 / ASP                                                ! nilsson
C
C Dummy arguments
C
      INTEGER I3, Ndim
C
C Local variables
C
      INTEGER i, ip, k, kp, n, np, np1
      DOUBLE PRECISION q, qp, u, up, v, vp, x, xp
      DIMENSION a(14),ap(14)                                             ! nilsson
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
         k = KK(n,I3)                                                    ! nilsson
         u = UAMp(n,I3)
         v = VAMp(n,I3)
         do l = 1,14                                                     ! nilsson
            a(l)=ASP(n,l,I3)                                             ! nilsson
         enddo                                                           ! nilsson
C--------L=INDEKS(N)
         DO np = np1, Ndim
            xp = ESP(np,I3)
            qp = EBCs(np,I3)
            ip = NN(np,I3)
            kp = KK(np,I3)                                               ! nilsson
            up = UAMp(n,I3)
            vp = VAMp(n,I3)
            do lp = 1,14                                                 ! nilsson
               ap(lp)=ASP(np,lp,I3)                                      ! nilsson
            enddo                                                        ! nilsson
C-----------LP=INDEKS(NP)
            IF (x.GT.xp) THEN
               ESP(np,I3) = x
               EBCs(np,I3) = q
               UAMp(np,I3) = u
               VAMp(np,I3) = v
               NN(np,I3) = i
               KK(np,I3) = k                                             ! nilsson
               do l = 1,14                                               ! nilsson
                  ASP(np,l,I3)=a(l)                                      ! nilsson
               enddo                                                     ! nilsson
C--------------INDEKS(NP)=L
               ESP(n,I3) = xp
               EBCs(n,I3) = qp
               UAMp(n,I3) = up
               VAMp(n,I3) = vp
               NN(n,I3) = ip
               KK(n,I3) = kp                                             ! nilsson
               do l = 1,14                                               ! nilsson
                  ASP(n,l,I3)=ap(l)                                      ! nilsson
               enddo                                                     ! nilsson
C--------------INDEKS(N)=LP
               x = xp
               q = qp
               u = up
               v = vp
               i = ip
               k = kp
               do l = 1,14                                               ! nilsson
                  a(l)=ap(l)                                             ! nilsson
               enddo                                                     ! nilsson
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
C     All lines labeled with "! nilsson" fall under copyright of H.Wienke,
C     Geel 09/2005
C
C COMMON variables
C
      DOUBLE PRECISION ALSin, BET2in,BST(3), CNOrin(8,8), EBCs(500, 2),  ! nilsson
     &                 EFItin(8,8),ESP(500, 2),GAP(2), GAPin(2),GRin(2), ! nilsson
     &                 HOMega, HOMin, RMS(3), RST(201), UAMp(500, 2),    ! nilsson
     &                 VAMp(500, 2), VLS(2), WIDexin, ASP(500,14,2)      ! nilsson
      INTEGER KSP(500,2), NSP(500,2), NHOle(2), NRX, NTOtal(2),IBLK(2)   ! nilsson
      INTEGER BLKSP(500,2)                                               ! nilsson
      COMMON /BLOCK / IBLK, BLKSP                                        ! nilsson
      COMMON /EVBCS / EBCs, VAMp, UAMp
      COMMON /RADI  / RST, NRX
      COMMON /SPPA  / HOMega, VLS, RMS, BST, GAP, NHOle, NTOtal
      COMMON /SPQN  / ESP, KSP, NSP                                      ! nilsson
      COMMON /SPQN2 / ASP                                                ! nilsson
      COMMON /TRINP / WIDexin,GAPin,HOMin,ALSin, EFItin, CNOrin,BET2in,  ! nilsson
     &                GRin                                               ! nilsson


C
C Dummy arguments
C
      DOUBLE PRECISION Amass
      INTEGER I3, Iout, Nz
C
C Local variables
C
      DOUBLE PRECISION a3, ad, all, als, anp(2,2), chemic,
     &                 dnz, e0, espx, hhh, hom, pi, pqn, r, rd, rde,
     &                 rmsd, rnp(3,2), rw0, rws, uscal, uvec, vll(16,2),
     &                 w, xnp, d,v
      REAL FLOAT
      INTEGER i, ifermi, ipr, k,                                         ! nilsson
     &        n, n1, n2, ndim, nhx, nl, nnucl, nq, nqx, numnuc
      INTEGER INT, kh, lsp, jsp, teller, imin                            ! nilsson
      logical break                                                      ! nilsson
      CHARACTER*10 text(2)
      DIMENSION v(30,30), d(30)                                          ! nilsson

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
C                                                                       ! nilsson
C     DO n = 1, 2000                                                    ! nilsson
C        iocc(n) = 0                                                    ! nilsson
C     ENDDO                                                             ! nilsson
      DO n = 1, 500
         EBCs(n,I3) = 0.
         ESP(n,I3) = 0.0
         VAMp(n,I3) = 0.0
         UAMp(n,I3) = 0.0
         BLKSP(n,I3) = 0
      ENDDO
      Do n=1,14                                                         ! nilsson
         do m=1,500                                                     ! nilsson
            asp(m,n,I3)= 0.0D0                                          ! nilsson
         enddo                                                          ! nilsson
      enddo                                                             ! nilsson
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
      espx = 70.                                                         ! nilsson
      nqx = MIN(espx/HOMega,13.D0)
      IF (nqx*HOMega.LT.espx) nqx = nqx + 1
      text(1) = 'PROTONS '
      text(2) = 'NEUTRONS'
      WRITE (8,99005) text(I3), HOMega, VLS(I3), VLS(I3)*HOMega, e0
99005 FORMAT (//,5X,'NILSSON-HAMILTONIAN FOR ',A10/'  HOMEGA:',F8.3,
     &        '  A(LS):',F8.3,' V(LS):',F8.3,
     &        ' (MEV)'/'  E0 (POTENTIAL DEPTH U(R=0)):',F8.3,' [MEV]')
      ipr = 5
      IF (Iout.GT.3) THEN
         WRITE (8,99010) (i - 1,i = 1,ipr)
99010    FORMAT (/7X,'A(LL) (IN UNITS OF HOMEGA)'/3X,'N',5(5X,'N+',I1))
         DO n = 1, nqx + 1, ipr
            n1 = n
            n2 = MIN(nqx + 1,n1 + ipr - 1)
            WRITE (8,99015) n - 1, (vll(k,I3),k = n1,n2)
99015       FORMAT (I4,5F8.4)
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
      ELSE IF(I3.EQ.2)THEN
         nnucl = INT(Amass) - Nz
      ENDIF
C-----find out the Fermi level                                           ! nilsson
      numnuc = 0                                                         ! nilsson
      DO n = 1, 500                                                      ! nilsson
         numnuc = numnuc + 2                                             ! nilsson
         IF(numnuc.GE.nnucl)GOTO 300                                     ! nilsson
      ENDDO                                                              ! nilsson
  300 ifermi = n                                                         ! nilsson
C-------fill up arrays                                                   ! nilsson
      n = 0                                                              ! nilsson
      DO nq = 0, nqx                                                     ! nilsson
         all = vll(nq+1,I3)                                              ! nilsson
         do k=1, 2*nq+1,2                                                ! nilsson
            imin = (k-1)/2                                               ! nilsson
            imax = nq+1                                                  ! nilsson
            Call Nilsson(d, e0,als,HOMega, nq, k, v, BET2in, all)        ! nilsson
            Do i = 1, imax -imin                                         ! nilsson
               ESP(n+i,I3) = d(i+imin)                                   ! nilsson
               NSP(n+i,I3) = nq                                          ! nilsson
               KSP(n+i,I3) = k                                           ! nilsson
               do j= 1, imax - imin                                      ! nilsson
                  ASP(n+i,j,I3)= v(j+imin,i+imin)                        ! nilsson
               enddo                                                     ! nilsson
            enddo                                                        ! nilsson
            n = n+imax - imin                                            ! nilsson
         enddo                                                           ! nilsson
      enddo                                                              ! nilsson
      nmax = n                                                           ! nilsson
      CALL ESORT(nmax,I3)                                                ! nilsson
C-----determination of nhx                                               ! nilsson
      if (BET2in.lt.0.001D0) then                                        ! nilsson
         n = ifermi                                                      ! nilsson
         teller = 1                                                      ! nilsson
         do while (teller.le.3)                                          ! nilsson
            nq  = NSP(n,I3)                                              ! nilsson
            kh  = KSP(n,I3)                                              ! nilsson
            iminh = (kh-1)/2                                             ! nilsson
            lsp = nq                                                     ! nilsson
            i = nq + 1 - iminh                                           ! nilsson
            break = .false.                                              ! nilsson
            do while ((lsp.ge.0).and.(i.gt.0).and.(.not.break))          ! nilsson
               jsp = 2*lsp+1                                             ! nilsson
               if (asp(n,i,I3).gt.0.5) then                              ! nilsson
C
C j-value subshell determined                                            ! nilsson
C
                  break = .true.                                         ! nilsson
               else                                                      ! nilsson
                  if (i.gt.1) then                                       ! nilsson
                     i = i-1                                             ! nilsson
                     jsp = 2*lsp -1                                      ! nilsson
                     if (asp(n,i,I3).gt.0.5) then                        ! nilsson
C
C j-value subshell determined                                            ! nilsson
C
                        break = .true.                                   ! nilsson
                     else                                                ! nilsson
                     endif                                               ! nilsson
                  else                                                   ! nilsson
                  endif                                                  ! nilsson
               endif                                                     ! nilsson
               i = i-1                                                   ! nilsson
               lsp = lsp -2                                              ! nilsson
            enddo                                                        ! nilsson
            if (n.eq.ifermi) then                                        ! nilsson
               jprev = jsp                                               ! nilsson
            else                                                         ! nilsson
               if (jsp.ne.jprev) then ! subshell is filled               ! nilsson
                  jprev = jsp                                            ! nilsson
                  teller = teller + 1                                    ! nilsson
               else                                                      ! nilsson
               endif                                                     ! nilsson
            endif                                                        ! nilsson
            n = n+1                                                      ! nilsson
         enddo                                                           ! nilsson
         nhx = n-2                                                       ! nilsson
         GRin(i3)= esp(nhx,i3) - esp( ifermi,i3) + 0.1                   ! nilsson
         if (i3.eq.1) then                                               ! nilsson
            WRITE (8,                                                    ! nilsson
     &'('' Range for constant gap. protons (MSD)'',   F6.3,''            ! nilsson
     &MeV'')') GRin(i3)                                                  ! nilsson
            WRITE (12,                                                   ! nilsson
     &'('' Range for constant gap. protons (MSD)'',   F6.3,''            ! nilsson
     &MeV'')') GRin(i3)                                                  ! nilsson
         else if(i3.eq.2) then                                           ! nilsson
            WRITE (8,                                                    ! nilsson
     &'('' Range for constant gap. neutrons (MSD)'',   F6.3,''           ! nilsson
     &MeV'')') GRin(i3)                                                  ! nilsson
            WRITE (12,                                                   ! nilsson
     &'('' Range for constant gap. neutrons (MSD)'',   F6.3,''           ! nilsson
     &MeV'')') GRin(i3)                                                  ! nilsson
         else                                                            ! nilsson
         endif                                                           ! nilsson
      else                                                               ! nilsson
         do n = 1, nmax                                                  ! nilsson
         if (esp(n,I3).gt. (esp(ifermi,I3)+GRin(i3))) goto 290           ! nilsson
         enddo                                                           ! nilsson
  290    nhx = n -1                                                      ! nilsson
      endif                                                              ! nilsson
      IF (nhx.LT.ifermi + 1) nhx = ifermi + 1
      NHOLe(I3) = nhx
C-----set BCS blocking                                                   ! nilsson
      IF (MOD(nnucl,2).NE.0) THEN                                        ! nilsson
         IBLk(I3) = ifermi                                               ! nilsson
         BLKSP(ifermi,I3)= 1                                             ! nilsson
      ENDIF                                                              ! nilsson
      CALL BCS(xnp,GAP(I3),I3,nhx,chemic)
      do n=nhx+1,nmax                                                    ! nilsson
         EBCS(n,I3) = ESP(n,I3) - chemic                                 ! nilsson
         UAMp(n,I3) = 1.0                                                ! nilsson
         VAMp(n,I3) = 0.0                                                ! nilsson
      enddo
      NTOtal(I3) = nmax                                                  ! nilsson
      ndim = nmax                                                        ! nilsson
C
C-----<R**2> is calculated
C
      RMS(I3) = 0.0
      DO nl = 1, nhx
         pqn = NSP(nl, I3) + 1.5                                         ! nilsson
         RMS(I3) = RMS(I3) + pqn*2.0D0*VAMp(nl,I3)**2                    ! nilsson
      ENDDO
      RMS(I3) = RMS(I3)*BST(I3)**2
      CALL ESORT(ndim,I3)
      END
C
C
      SUBROUTINE RESPNS
C     All lines labeled with "! nilsson" fall under copyright of H.Wienke,
C     Geel 09/2005

      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C COMMON variables
C
      DOUBLE PRECISION ALPha(11,6), AN(6), ANGle(NDANGecis),
     &                 CLRn(8,8), DTHeta,DUMmy(1041),ECEntr(5),EOUtmi,
     &                 EOUtmx, ESP(500,2), ESTep, ETMax, ETMin,
     &                 EXTcom(10), FACb, FAClog(500), FFAc1d(2),
     &                 FFAc2d(2), FFAc3d(2), FFTot(10), FNOrm(6),
     &                 FNQ(6,6), FQ(6), GAP(2), HOMega, Q0, QGRand,
     &                 QMAx, QMIna, QMInb, QS1, QS2, QSTep, RAC,
     &                 RHO(3*(NDEx+25),8,8), RHOb(3*(NDEx+25),11,2),     ! nilsson_newest
     &                 RMS(3), ROPt,
     &                 SREw(21), SREwl(21), SRNew(21), THEta1, THEta2,
     &                 U9, VLS(2), WIDex, BST(3)
      INTEGER IA, IB, IC12x, IC1max, IC1mxr, IC1x, IC2max, IC2mxr,IC2x,
     &        ICC, ICMax, ID, IE, IG,NSP(500, 2),KSP(500, 2),KDEnvi,     ! nilsson
     &        KEX3,
     &        KEXcom(10), KRTmax, KRType, KTRl(10), L9(10), ! nilsson
     &        NAVerg, NCHanl, NEBinx, NFAc12, NHOle(2),
     &        NN, NQ1x, NQ2x, NRMax(10), NTHeta, NTOtal(2),! nilsson
     &        NZ
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
      COMMON /SPQN  / ESP, KSP, NSP                                      ! nilsson
      COMMON /TRINTP/ DTHeta, ANGle, ESTep, EOUtmi, EOUtmx, ECEntr,
     &                QSTep, QMIna,QMInb,QMAx,QGRand,FFAc1d,FFAc2d,
     &                FFAc3d
C
C Local variables
C
      DOUBLE PRECISION a1, basq, e0, e1, e2, eex, est(0:3*(NDEx+25)),hhh
      REAL FLOAT
      INTEGER i, ic, iout4, ipr, k, krt, krtx, lp1, lt, ltmaxr, ne, neb,
     &        nlmax,kp1,kt                                               ! nilsson_newest
C
C-----in RHO, RHOB etc the increased argument NEB corresponds to
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
               WRITE (8,99005)
99005          FORMAT (//10X,' PROTON SINGLE PARTICLE STATES:')
            ELSE
               WRITE (8,99010)
99010          FORMAT (//10X,'NEUTRON SINGLE PARTICLE STATES:')
            ENDIF
            nlmax = NTOtal(k)
            WRITE (8,99015) (ESP(i,k),NSP(i,k),KSP(i,k),i = 1,
     &                      nlmax)
99015       FORMAT (6(F9.2,2I3,'/2'))
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
            do kt = 1,lt                                                 ! nilsson_newest
               RHO(ne,lt,kt) = 0.0                                       ! nilsson_newest
            enddo                                                        ! nilsson_newest
         ENDDO
      ENDDO
      CALL INELAS(est,NN,NZ,NEBinx)
      DO lp1 = 1, ltmaxr
         DO ne = 1, NEBinx
            do kp1=1,lp1                                                 ! nilsson_newest
               RHOb(ne,lp1,1) = RHOb(ne,lp1,1) +RHO(ne,lp1,kp1)          ! nilsson_newest
            enddo                                                        ! nilsson_newest
         ENDDO
      ENDDO
      IF (KRType.GE.4) THEN
         RAC = a1/(a1 - 3.)
         IF (KRType.GE.4) THEN
            WRITE (8,*) '  ******  NOT USED  ******'
            STOP
         ENDIF
         DO lp1 = 1, ICMax
            do kp1 = 1,lp1                                               ! nilsson_newest
               DO ne = 1, NEBinx
                  RHOb(ne,lp1,2) = RHOb(ne,lp1,2)+RHO(ne,lp1,kp1)        ! nilsson_newest
               ENDDO
            enddo                                                        ! nilsson_newest
         ENDDO
      ENDIF
  100 IF (iout4.NE.0) THEN
         krtx = KTRl(7) + 1
         DO krt = 1, krtx
            IF (krt.EQ.1) THEN
               WRITE (8,99020) ETMin, ETMax, ESTep
99020          FORMAT (///10X,
     &                 'RPA RESPONSE FUNCTIONS (INELASTIC EXCITATION)'/
     &                 10X,'EXMIN=',F7.2,' EXMAX=',F7.2,' ESTEP=',F7.2/)
            ELSE
               WRITE (8,99025) ETMin, ETMax, ESTep
99025          FORMAT (///10X,'RPA RESPONSE FUNCTIONS (CHARGE EXCHANGE)'
     &                 /10X,'EXMIN=',F7.2,' EXMAX=',F7.2,' ESTEP=',
     &                 F7.2/)
            ENDIF
            WRITE (8,99030) (lt - 1,lt = 1,ltmaxr)
99030       FORMAT (5X,'EX',10(4X,I4,3X))
            DO neb = 1, NEBinx
               WRITE (8,99035) est(neb),
     &                         (RHOb(neb,lt,krt),lt = 1,ltmaxr)
99035          FORMAT (' ',F6.2,10E11.4)
            ENDDO
         ENDDO
         WRITE (8,99040) (SRNew(lt),lt = 1,ltmaxr)
99040    FORMAT (///' ','SRNEW:',11E11.4)
         WRITE (8,99045) (SREw(lt),lt = 1,ltmaxr)
99045    FORMAT (' ','SREW :',11E11.4)
         IF (KTRl(8).EQ.( - 1)) GOTO 99999
      ENDIF
      e1 = ETMin + ESTep
      DO ic = 1, ICMax
         SREw(ic) = ESTep*0.5*(ETMin*RHOb(1,ic,1) + e1*RHOb(2,ic,1))
      ENDDO
      eex = ETMin - ESTep
      hhh = ESTep/3.
      WRITE (8,99055) (ic - 1,ic = 1,ICMax)
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
            WRITE (8,99060) eex, (SREw(ic),ic = 1,ICMax)
99060       FORMAT (' ',F6.2,11E11.4)
         ENDIF
      ENDDO
      KTRl(8) = -1
      GOTO 100
99999 END
C
C
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
      DOUBLE PRECISION FUNCTION EHO(N, L, Jtw, Hbo, All, Als)
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
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
      ell = -All*(rll - 0.5*nq*(nq + 3.))
      els = -Als*fso
      EHO = Hbo*(nq + 1.5 + ell + els)
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
         WRITE (8,99005) s1, s2
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
     &                  Nejc, Xsinl)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C
C COMMON variables
C
      DOUBLE PRECISION ANGle(NDANGecis), CLRn(8,8), DTHeta, ECEntr(5),   ! nilsson_newest
     &                 EOUtmi, EOUtmx, ESTep, ETMax, ETMin, EXTcom(10),
     &                 FAC1d(2), FAC2d(2), FAC3d(2), FACb, FFTot(10),
     &                 Q0, QGRand, QMAx, QMIna, QMInb, QS1, QS2, QSTep,
     &                 RHOb(3*(NDEx+25),11,2), ROPt, THEta1, THEta2,     ! nilsson_newest
     &                 WIDex, Xsinl
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
c     DOUBLE PRECISION Cros(Nq12x,Lc12x,Kdim), Crose(Nbinx,Ngle,Idimm)
      DOUBLE PRECISION Cros(Nq12x,Lc12x,Kdim)
      DOUBLE PRECISION Crose(2*NDEX,NDANGecis,NDANGecis)
C
C Local variables
C
      DOUBLE PRECISION a1, a2, a3, adum(5,7), al1l2(7,7), amat(15,15),
     &                 an, ay, aynorm, csfit(NDANGecis), delta, eout,
     &                 f11(2), f2(15), f21(2), fh, fl1(7,7), fl2(7,7),
     &                 fnl1(7), fnl2(7), fnq1(7), fnq2(7), fq1(6,6),
     &                 fq2(6,6), gmat(15,15), piece, pxsec, q1, q2,
     &                 qq(5), rb12, s1, s2, s3, sg(3*(NDEx+25)), sigm,
     &                 sn, sum, sumpx(3*(NDEx+25),2), x, x2, xl, xq, yl,
     &                 zz(15), f1(15)
      REAL FLOAT
      REAL hhh
      INTEGER i, ic, icp, icpmx, icpx, ier, j, jx, k, k1, k2, k2n,
     &        kc, kcp, kcpmx, kkp, kq, kr, krtx, kx, ky, l, l1p1, l2p1,
     &        lc, lc1, lcp1, ln, m2, m2n, mx, my, n, n0, n1, n2, n2n,
     &        na, nad, nangle, nc, ncm1, ndim, ne, neb, necs, nej,
     &        nemnt, nemx, nep, nepp, nmax, nnur, np, npx, nq, nqx, nx,
     &        ny
      INTEGER MAX0, MIN0
      EQUIVALENCE (f2(1),f21)
      EQUIVALENCE (f1(1),f11)
      nej = 1
      IF (ZEJc(Nejc).EQ.1.0D0) nej = 2
      IF (ZEJc(Nejc).GT.1.0D0) THEN
         WRITE (8,*)
     &' THIS IMPLEMENTATION OF TRISTAN IS ABLE TO TREAT ONLY NEUTRON OR
     &PROTON IN THE INCIDENT CHANNEL'
         STOP
      ENDIF
      nnur = NREs(nej)
      icpx = KTRl(1)
      icpmx = icpx + KTRl(7)
      kcpmx = icpmx + 1
      krtx = KTRl(7) + 1
      kkp = 1
      IF (KRType.GE.4) kkp = 2
      nangle = Ngle
      IF (IOUt.GT.3) WRITE (8,99005)
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
      REWIND (16)
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
                        nemnt = MAX0(1,ne - NAVerg)
                        nemx = MIN0(Nbinx,nemnt + NAVerg)
                        sum = 0.
                        DO neb = nemnt, nemx
                           hhh = ESTep
                           IF (neb.EQ.nemnt .OR. neb.EQ.nemx)
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
               WRITE (8,99010) ANGle(na), delta,
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
                     WRITE (8,99020) eout, s1, s2, s3, sigm, f11, a1,
     &                               a2, a3, ay, f21
                     WRITE (66,99020) eout, s1, s2, sigm
                  ENDIF
C                 necs = Nbinx - ne + 2
C-----------------recover from the more dense energy grid in MSD
                  necs = (Nbinx - ne)/2 + 2
                  sigm = sigm/2.0
C-----------------store ddx to continuum
                  IF (IDNa(2*nej,2).NE.0 .AND. necs.LE.NEX(nnur) - 1)
     &                THEN
                     CSEa(necs,na,nej,1) = CSEa(necs,na,nej,1) + sigm
C-----------------discrete level region is not needed since spectra are
C-----------------constructed out of discrete levels
                  ELSEIF (IDNa(2*nej - 1,2).NE.0 .AND. necs.GE.NEX(nnur)
     &                    ) THEN
                     CSEa(necs,na,nej,1) = CSEa(necs,na,nej,1) + sigm
                  ENDIF
               ENDIF
            ENDDO
   50    ENDDO
      ENDDO
      IF(IOUT.GT.3) CLOSE(66)
      REWIND (14)
      k1 = kcpmx
C-----integrate angular distributions over angle (and energy)
      nmax = MIN(NDEx,Nbinx/2+2)
C-----if ECIS active use only continuum part of the MSD spectrum
      IF (DIRect.GT.0) nmax = MIN(nmax,NEX(nnur))
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
      WRITE(8,*) ' '
      WRITE(8,'('' Integrated MSD emission at Elab '', G15.3,'' is ''
     &         , G15.3,'' mb'')') EINl, CSMsd(nej)
      WRITE(8,*) ' '
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
     &                 wght(NDLV), xj, xnor, ddxs(NDAngecis)
      DOUBLE PRECISION csmtot,csm1,csm2,eee
      REAL FLOAT
      INTEGER icsp, ie, il, irec, j, na, nangle, nexrt, next
      INTEGER INT
      IF (NEX(Nnuc).LT.1) THEN
         WRITE (8,*) ' HM !! THERE MUST BE SOMETHING WRONG !!!'
         WRITE (8,*) ' ACCUMSD COMPLAINS NEGATIVE ENERGY FOR'
         WRITE (8,*) ' NUCLEUS NNUC=', Nnuc, ' NEX(NNUC)=', NEX(Nnuc)
         WRITE (8,*) ' I HAD BETTER  S T O P'
         STOP
      ENDIF
C-----
C----- CONTINUUM
C-----
      IF (Nnuc.EQ.Nnur) THEN
         excnq = EX(NEX(Nnuc),Nnuc)
      ELSE
         excnq = EX(NEX(Nnuc),Nnuc) - Q(Nejc,Nnuc)
      ENDIF
C-----number of spectrum bins to continuum WARNING! might be negative!
      nexrt = MIN(INT((excnq - ECUt(Nnur))/DE + 1.0001),ndecsed)
C-----total number of bins
      next = INT(excnq/DE + 1.0001)
C-----calculate spin distribution for 1p-1h states
      SIG = 2*0.26*A(Nnur)**0.66666667
      somj = 0.0
      DO j = 1, NLW
         xj = SQRT(FLOAT(j)**2 + XJLv(LEVtarg,0)**2)
         phdj(j) = 0.0
         w = (xj + 1.0)*xj/2./SIG
         IF (w.LE.50.D0) THEN
            phdj(j) = (2*xj + 1.)*DEXP( - w)
            somj = somj + phdj(j)
         ENDIF
      ENDDO
C-----distribution of the continuum MSD contribution -
C-----proportional to the p-h spin distribution shifted by the target
C-----ground state target spin XJLV(1,0)
      IF (nexrt.GT.0) THEN
         DO j = 1, NLW
            xnor = 0.5*phdj(j)/somj
            DO ie = 1, nexrt
               pops = xnor*CSEmsd(nexrt - ie + 1,Nejc)
C
C              Population increased to preserve total flux
C              as calculated by PCROSS/MSD+MSC
C
               if(ie.eq.1 .or. ie.eq.nexrt) pops=2*pops
C
               POP(ie,j,1,Nnur) = POP(ie,j,1,Nnur) + pops
               POP(ie,j,2,Nnur) = POP(ie,j,2,Nnur) + pops
            ENDDO
         ENDDO

C--------add MSD contribution to the population spectra
C--------used for ENDF exclusive spectra
         IF (ENDf(1).GT.0) THEN
            DO ie = 1, nexrt
               icsp = nexrt - ie + 1
C
C              Population increased to preserve total flux
C              as calculated by PCROSS/MSD+MSC
C
               pops = CSEmsd(icsp,Nejc)
C              Commented on Dec 2011 by RCN, to keep integral of spectra = XS
C              if(ie.eq.1 .or. ie.eq.nexrt) pops=2*pops

               POPcse(ie,Nejc,icsp,INExc(Nnur)) =
     &            POPcse(ie,Nejc,icsp,INExc(Nnur)) + pops
C--------------Correct last bin (not needed for POP as for this it is done at the end)
               IF (ie.EQ.1) POPcse(ie,Nejc,icsp,INExc(Nnur))
     &             = POPcse(ie,Nejc,icsp,INExc(Nnur))
     &             - 0.5*CSEmsd(icsp,Nejc)

C--------------DDX using portions
               POPcseaf(ie,Nejc,icsp,INExc(Nnur)) = 1.0
C--------------DDX
C--------------Bin population by MSD (spin/parity integrated)
               POPbin(ie,Nnur) = pops
            ENDDO
         ENDIF
C--------storing continuum recoils
         IF (ENDf(1).GT.0 .and. nejc.ne.0 .and. RECOIL.GT.0) THEN
C
C           No recoils from gamma emission for the time being
C
            nangle = NDANG
            dang = 3.14159/FLOAT(nangle - 1)
            coef = 2*3.14159*dang/DERec
            ecm = EINl - EIN
            IF(Nejc.NE.0) THEN
               DO ie = 1, nexrt
                  echannel = (ie - 1)*DE*AEJc(Nejc)/A(1)
                  DO na = 1, nangle
                     erecoil = ecm + echannel + 2*SQRT(ecm*echannel)
     &                         *CANgler(na)
                     irec = erecoil/DERec + 1.001
                     weight = (erecoil - (irec - 1)*DERec)/DERec
                     IF (irec + 1.GT.NDEREC) GOTO 20
                     csmsdl = CSEa(nexrt - ie + 1,na,Nejc,1)*SANgler(na)
     &                        *coef
                     RECcse(irec,ie,Nnur) = RECcse(irec,ie,Nnur)
     &                  + csmsdl*(1.0 - weight)
                     RECcse(irec + 1,ie,Nnur) = RECcse(irec + 1,ie,Nnur)
     &                  + csmsdl*weight
C
                  ENDDO
   20          ENDDO
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
      IF (Nejc.eq.1 .and. IDNa(1,2).EQ.0 .and. IDNa(1,6).EQ.0 ) return
      IF (Nejc.eq.2 .and. IDNa(3,2).EQ.0 .and. IDNa(3,6).EQ.0 ) return

      IF (Nejc.eq.0 .and. IDNa(1,2).EQ.0 ) return ! Skipping discrete gammas if MSC not active 
      IF (Nejc.eq.3 .and. IDNa(11,6).EQ.0 ) return
      IF (Nejc.eq.4 .and. IDNa(12,6).EQ.0 ) return
      IF (Nejc.eq.5 .and. IDNa(13,6).EQ.0 ) return
      IF (Nejc.eq.6 .and. IDNa(14,6).EQ.0 ) return
C-----discrete level contribution to recoil spectra
C-----in case only discrete levels can be populated we set nexrt to 1
C-----(NOTE: it is usually negative in such a case)
      IF (nexrt.LE.0) nexrt = 1
      IF (ENDf(1).GT.0 .and. RECOIL.GT.0) THEN
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
C-----distribution of the MSD/PCROSS contribution to discrete levels
C-----
      IF(Nejc.eq.NPRoject) then
C
C      Inelastic channel 
C
       csm1 = 0.d0
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
         IF (ENDf(1).GT.0) then
           if( IDNa(1,6).GT.0 .and. Nejc.eq.1 ) CSEmsd(ie,Nejc) = 0.d0
           if( IDNa(3,6).GT.0 .and. Nejc.eq.2 ) CSEmsd(ie,Nejc) = 0.d0
           if( Nejc.gt.2 ) CSEmsd(ie,Nejc) = 0.d0
	   ENDIF
       ENDDO
       csmsdl = csmsdl - 0.5*CSEmsd(next,Nejc)*DE

C
C
C      Inelastic channel
C
       csmtot = 0.d0
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
  100  IF (swght.EQ.0.0D0) THEN
         WRITE (8,*) ' WARNING:'
         WRITE (8,*) ' WARNING: No level to put msd level contribution '
     &               , csmsdl, ' mb'
         WRITE (8,*) ' WARNING: Load everything to the ground state '
         WRITE (8,*) ' WARNING: Ang. dist. of discrete levels ignored'
         WRITE (8,*) ' WARNING:'
         POPlv(1,Nnur) = POPlv(1,Nnur) + csmsdl
         RETURN
       ENDIF
       csmsdl = csmsdl/swght
       DO il = 2, NLV(Nnur)
         eemi = excnq - ELV(il,Nnur)
         IF (eemi.LT.0.0D0) RETURN
         csmtot = csmtot + csmsdl*wght(il)
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
            xnor = 0.d0
         ENDIF
C--------Store ang. dist.
         DO na = 1, NDANG
            CSAlev(na,il,Nejc) = CSAlev(na,il,Nejc)
     &                           + xnor*CSEa(ie,na,Nejc,1)
C           Deleting the corresponding angular distribution
            IF (ENDf(1).GT.0) CSEa(ie,na,Nejc,1) = 0.d0
         ENDDO
       ENDDO

      ELSE
C
C     Other channels (not the inelastic)
C
       csm1 = 0.d0
       istart = nexrt +1
       DO ie = istart, next
         csm1 = csm1 + CSEmsd(ie,Nejc)*DE
       ENDDO
       csm2 = csm1 - 0.5*CSEmsd(next,Nejc)*DE

       csmtot = 0.d0
       xnor = 0.d0

       DO il = NLV(Nnur),1,-1
         eemi = excnq - ELV(il,Nnur)
         IF (eemi.LT.0.0D0) EXIT

         xnor = CSEmsd(istart,Nejc)*DE
C
C        Assigning angular distribution of the first continuum bin "istart"
C        to the angular distribution of the discrete level "il"
C                                                    
         do na=1,NDAng 
           ddxs(na) =   CSEa(istart,na,Nejc,1)
         enddo
         csmsdl = 0.d0

         DO ie = istart, next
           eee = DE*(ie - 1)
           IF (eee.GT.eemi) EXIT 
           csmsdl = csmsdl + CSEmsd(ie,Nejc)*DE
	     IF(ENDF(1).GT.0) then
C            Deleting the corresponding XS from the continuum
C              as it is moved to discrete spectra
             CSEmsd(ie,Nejc) = 0.d0
C            Deleting the corresponding angular distribution
             do na=1,NDAng 
               CSEa(ie,na,Nejc,1) = 0.d0
             enddo
	     ENDIF
           istart = ie + 1
         ENDDO
         POPlv(il,Nnur) = POPlv(il,Nnur) + csmsdl
         CSDirlev(il,Nejc) = CSDirlev(il,Nejc) + csmsdl
 
         csmtot = csmtot + csmsdl
C--------Normalization factor
         IF (xnor.GT.0) THEN
            xnor = csmsdl/xnor
         ELSE
            xnor = 0.d0
         ENDIF
C--------Store ang. dist.
         DO na = 1, NDANG
           CSAlev(na,il,Nejc) = CSAlev(na,il,Nejc) + xnor*ddxs(na)
         ENDDO
       ENDDO
      ENDIF

      IF(csm2-csmtot.gt.0.1d0) then
       write(8,*) 'WARNING: PE discrete levels for nejc=',nejc
       write(8,*) 'WARNING: Difference in in/out flux =',
     &             sngl(csm2-csmtot)
      ELSE
       write(8,*) ' PE XS to discrete levels   ',sngl(csmtot),
     >              ' for nejc =',nejc
      ENDIF
      RETURN
      END
      Subroutine Nilsson(d,e0,als, HOMega,n,k,v, BETa2,all)
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
CCC   *       all    - coeff. of L*L term                              *
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
      DOUBLE PRECISION BETa2, HOMega,d,v, als, e0
      INTEGER n,k
C
C Local variables
C
      DOUBLE PRECISION a, all, delta, ebar
      DOUBLE PRECISION EHO, VCC

      REAL FLOAT
      INTEGER i1, i2, j1, j2, l1, l2,  nl
C
      DIMENSION a(30,30),v(30,30),d(30)


C    Fill up the Nilsson matrix

      delta = .946*BETa2
      ebar = (2.0/3.0)*delta*HOMega
      do i = 1,30
         d(i)= 0.0
         do j=1,30
            v(i,j) = 0.0
            a(i,j) = 0.0
         enddo
      enddo
      i2 = n+1   ! rank matrix
      do while (i2.gt.0)
         i1 = i2
         do while (i1.gt.0)
C           first diagonal terms (l1 = l2)
            if (i1.eq.i2) then
               l1= i1-1
               l2= l1
               nl= (n - l1+2)/2
               j1 = 2*l1 + 1
               j2 = j1
               A(i1,i2) = EHO(nl,l1,j1,HOMega,all,
     &                    als)+e0
               A(i1,i2) = A(i1,i2) - ebar*VCC(j1,4,j2,k,0)*
     &         VCC(j1,4,j2,1,0)*(2.D0*float(nl-1) + float(l1) + 1.5D0)
               if (i1.gt.1.and.i2.gt.1) then
                  A(i1-1,i2-1)= EHO (nl, l1,j1-2,HOMega,all,als)+e0
                  A(i1-1,i2-1)= A(i1-1,i2-1)-ebar*VCC(j1-2,4,
     &            j2-2,k,0)*VCC(j1-2,4,j2-2,1,0)*
     &            (2.D0*float(nl-1)+float(l1) + 1.5D0)
                  A(i1-1,i2)=(-1)**((j1-4-j2)/2)*sqrt(float(j1-1)/
     &            float(j2+1))*ebar*VCC(j1-2,4,j2,k,0)*
     &            VCC(j1-2,4,j2,1,0)*(2.D0*float(nl-1)+float(l1)+1.5D0)
                  A(i1,i2-1)=A(i1-1,i2)
               endif
C           off-diagonal terms (l1 = l2 +- 2)
            else if (i1.eq.(i2-2)) then
               l1 = i1 - 1
               l2 = i2 - 1
               nl= (n - l2+2)/2
               j1 = 2*l1 + 1
               j2 = 2*l2 + 1
               A(i1,i2) = (-1)**((j1-j2-2)/2)*sqrt(float(j1+1)/
     &         float(j2+1))*ebar*VCC(j1,4,j2,k,0)*
     &            VCC(j1,4,j2,1,0)*(-2.D0)*sqrt(float(nl)*
     &            (float(nl-1+l2)+.5D0))
               A(i2,i1)= A(i1,i2)
                  A(i1,i2-1) =(-1)**((j1-j2-4)/2)*sqrt(float(j1+1)/
     &            float(j2-1))*ebar*VCC(j1,4,j2-2,k,0)*
     &            VCC(j1,4,j2-2,1,0)*(-2.D0)*sqrt(float(nl)*
     &            (float(nl-1+l2)+.5D0))
                  A(i2-1,i1)= A(i1,i2-1)
               if (i1.gt.1) then
                  A(i1-1,i2) =(-1)**((j1-4-j2)/2)*sqrt(float(j1-1)/
     &            float(j2+1))*ebar*VCC(j1-2,4,j2,k,0)*
     &            VCC(j1-2,4,j2,1,0)*(-2.D0)*sqrt(float(nl)*
     &            (float(nl-1 + l2)+.5D0))
                  A(i2,i1-1)= A(i1-1,i2)
                  A(i1-1,i2-1)=(-1)**((j1-2-j2-4)/2)*sqrt(float(j1-1)/
     &            float(j2-1))*ebar*VCC(j1-2,4,j2-2,k,0)*
     &            VCC(j1-2,4,j2-2,1,0)*(-2.D0)*sqrt(float(nl)*
     &            (float(nl-1+l2)+.5D0))
                  A(i2-1,i1-1)= A(i1-1,i2-1)
               endif
            else
            endif
            i1 = i1-2
         enddo
         i2 = i2-2
      enddo
      Call Jacobi(a,n+1,30,d,v,nrot)
      end
C
      DOUBLE PRECISION FUNCTION VCC(JX1,JX2,JX3,MX1,MX2)
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
CCC   (from CULIB8 library of routines used in DWUCK4 and CHUCK3)         *                                                                     *
CCC************************************************************************
CCC
      IMPLICIT REAL*8(A-H,O-Z)
c      EXTERNAL FACTOR
      COMMON/FACTRL/FACT(0:32)
C
      VCC=0.0
      J1=JX1
      J2=JX2
      J3=JX3
      M1=MX1
      M2=MX2
      IF(J1.LT.J2) GO TO 20
      IF(J3.LT.J2) GO TO 30
      ICNTR=0
      GO TO 40
   20 IF(J3.LT.J1) GO TO 30
      ICNTR=-1
      IT=J1
      J1=J2
      J2=IT
      IT=M1
      M1=M2
      M2=IT
      GO TO 40
   30 ICNTR=1
      IT=J2
      J2=J3
      J3=IT
      M2=-M1-M2
   40 CONTINUE
      JZ1=(J1+J2-J3)/2
      IF(JZ1.LT.0) GO TO 150
      JZ2=(J1+J3-J2)/2
      IF(JZ2.LT.0) GO TO 150
      JZ3=(J2+J3-J1)/2
      IF(JZ3.LT.0) GO TO 150
      IF(J1-IABS(M1).LT.0) GO TO 150
      IF(J2-IABS(M2).LT.0) GO TO 150
      IF(J3-IABS(M1+M2).LT.0) GO TO 150
      JT1=(J1-J3+M2)/2
      JT2=(J2-J3-M1)/2
      NUMIN=MAX0 (JT1,JT2,0)
      JT3=(J1-M1)/2
      JT4=(J2+M2)/2
      NUMAX=MIN0 (JT3,JT4,JZ1)
      JT5=(J2-M2)/2
      IF(NUMAX.LT.NUMIN) GO TO 150
      J4=J1/2
      J5=J3/2
      PHAS=PHASEF(NUMIN)
      DO 100 NU=NUMIN,NUMAX
      VCC=VCC+PHAS      *(YXFCT(JT3-NU,J4)*YXFCT(NU-JT2,J5))
     1/(FACT(JT4-NU)*FACT(NU-JT1)*FACT(JZ1-NU)*FACT(NU))
      PHAS=-PHAS
  100 CONTINUE
      FCTOR=YXFCT(J4,(J1+M1)/2)*YXFCT(J4,JT3)*YXFCT((J1+J2+J3)/2+1,JZ2)*
     1YXFCT(J5,(J3+M1+M2)/2)*YXFCT(J5,(J3-M1-M2)/2)*FACT(JZ1)*FACT(JZ3)*
     2FACT(JT4)*FACT(JT5)*FLOAT(J3+1)
      VCC=SQRT(FCTOR)*VCC
      IF(ICNTR)120,150,110
  110 VCC=VCC*SQRT(FLOAT(J2+1)/FLOAT(J3+1))*PHASEF(JT3)
      GO TO 150
  120 VCC=VCC*PHASEF(JZ1)
  150 RETURN
      END
      BLOCK DATA FACTOR
c
c     Factorial table
c
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/FACTRL/FACT(0:32)
C
      DATA FACT/ 1.0000000000E+00, 1.0000000000E+00, 2.0000000000E+00
     1         , 6.0000000000E+00, 2.4000000000E+01, 1.2000000000E+02
     2         , 7.2000000000E+02, 5.0400000000E+03, 4.0320000000E+04
     3         , 3.6288000000E+05, 3.6288000000E+06, 3.9916800000E+07
     4         , 4.7900160000E+08, 6.2270208000E+09, 8.7178291200E+10
     5         , 1.3076743680E+12, 2.0922789888E+13, 3.5568742810E+14
     6         , 6.4023737057E+15, 1.2164510041E+17, 2.4329020082E+18
     7         , 5.1090942172E+19, 1.1240007278E+21, 2.5852016739E+22
     8         , 6.2044840173E+23, 1.5511210043E+25, 4.0329146113E+26
     9         , 1.0888869450E+28, 3.0488834461E+29, 8.8417619937E+30
     $         , 2.6525285981E+32, 8.2228386542E+33, 2.6313083693E+35/
C    $         , 8.6833176188D+36, 2.9523279904D+38, 1.0333147966D+40
C    $         , 3.7199332679D+41, 1.3763753091D+43, 5.2302261747D+44
C    $         , 2.0397882081D+46, 8.1591528325D+47, 3.3452526613D+49
C    $         , 1.4050061178D+51, 6.0415263063D+52, 2.6582715748D+54
C    $         , 1.1962222087D+56, 5.5026221598D+57, 2.5862324151D+59
C    $         , 1.2413915593D+61, 6.0828186403D+62, 3.0414093202D+64
C    $         , 1.5511187533D+66/
      END

      DOUBLE PRECISION FUNCTION PHASEF(N)
CCC
CCC************************************************************************
CCC                                                                       *
CCC   Author: P. D. Kunz, University of Colorado, Boulder, Colorado, US   *
CCC                                                                       *
CCC************************************************************************
CCC
      IMPLICIT REAL*8(A-H,O-Z)
      PHASEF=DBLE(1-2*IABS(N-2*(N/2)))
      RETURN
      END
      FUNCTION YXFCT(M,N)                                               YXFCT000
CCC
CCC************************************************************************
CCC                                                                       *
CCC   Author: P. D. Kunz, University of Colorado, Boulder, Colorado, US   *
CCC                                                                       *
CCC************************************************************************
CCC
      IMPLICIT REAL*8(A-H,O-Z)                                          YXFCT002
C     COMPUTES NFACT/MFACT                                              YXFCT003
      YXFCT=1.0                                                         YXFCT004
      NUMAX=M-N                                                         YXFCT005
      IF(NUMAX)30,100,20                                                YXFCT006
   20 ICTRL=0                                                           YXFCT007
      FCTOR=N                                                           YXFCT008
      GO TO 40                                                          YXFCT009
   30 ICTRL=1                                                           YXFCT010
      NUMAX=-NUMAX                                                      YXFCT011
      FCTOR=M                                                           YXFCT012
   40 CONTINUE                                                          YXFCT013
      DO 50 NU=1,NUMAX                                                  YXFCT014
      FCTOR=FCTOR+1.0                                                   YXFCT015
      YXFCT=YXFCT*FCTOR                                                 YXFCT016
   50 CONTINUE                                                          YXFCT017
      IF(ICTRL.EQ.0) YXFCT=1.0/YXFCT                                    YXFCT018
  100 RETURN                                                            YXFCT019
      END

      SUBROUTINE JACOBI(A,N,NP,D,V,NROT)
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
      DOUBLE PRECISION A, D, V
      INTEGER N, NP, NROT
C
C  Local variables
C
      PARAMETER (NMAX=100)
      DOUBLE PRECISION  B, Z, SM, THRESH,
     +G,H,TAU,S,T,C
      INTEGER IP, IQ
      DIMENSION A(NP,NP),D(NP),V(NP,NP),B(NMAX),Z(NMAX)
      DO 12 IP=1,N
        DO 11 IQ=1,N
          V(IP,IQ)=0.
11      CONTINUE
        V(IP,IP)=1.
12    CONTINUE
      DO 13 IP=1,N
        B(IP)=A(IP,IP)
        D(IP)=B(IP)
        Z(IP)=0.
13    CONTINUE
      NROT=0
      DO 24 I=1,50
        SM=0.
        DO 15 IP=1,N-1
          DO 14 IQ=IP+1,N
            SM=SM+ABS(A(IP,IQ))
14        CONTINUE
15      CONTINUE
        IF(SM.EQ.0.)RETURN
        IF(I.LT.4)THEN
          THRESH=0.2*SM/N**2
        ELSE
          THRESH=0.
        ENDIF
        DO 22 IP=1,N-1
          DO 21 IQ=IP+1,N
            G=100.*ABS(A(IP,IQ))
            IF((I.GT.4).AND.(ABS(D(IP))+G.EQ.ABS(D(IP)))
     *         .AND.(ABS(D(IQ))+G.EQ.ABS(D(IQ))))THEN
              A(IP,IQ)=0.
            ELSE IF(ABS(A(IP,IQ)).GT.THRESH)THEN
              H=D(IQ)-D(IP)
              IF(ABS(H)+G.EQ.ABS(H))THEN
                T=A(IP,IQ)/H
              ELSE
                THETA=0.5*H/A(IP,IQ)
                T=1./(ABS(THETA)+SQRT(1.+THETA**2))
                IF(THETA.LT.0.)T=-T
              ENDIF
              C=1./SQRT(1+T**2)
              S=T*C
              TAU=S/(1.+C)
              H=T*A(IP,IQ)
              Z(IP)=Z(IP)-H
              Z(IQ)=Z(IQ)+H
              D(IP)=D(IP)-H
              D(IQ)=D(IQ)+H
              A(IP,IQ)=0.
              DO 16 J=1,IP-1
                G=A(J,IP)
                H=A(J,IQ)
                A(J,IP)=G-S*(H+G*TAU)
                A(J,IQ)=H+S*(G-H*TAU)
16            CONTINUE
              DO 17 J=IP+1,IQ-1
                G=A(IP,J)
                H=A(J,IQ)
                A(IP,J)=G-S*(H+G*TAU)
                A(J,IQ)=H+S*(G-H*TAU)
17            CONTINUE
              DO 18 J=IQ+1,N
                G=A(IP,J)
                H=A(IQ,J)
                A(IP,J)=G-S*(H+G*TAU)
                A(IQ,J)=H+S*(G-H*TAU)
18            CONTINUE
              DO 19 J=1,N
                G=V(J,IP)
                H=V(J,IQ)
                V(J,IP)=G-S*(H+G*TAU)
                V(J,IQ)=H+S*(G-H*TAU)
19            CONTINUE
              NROT=NROT+1
            ENDIF
21        CONTINUE
22      CONTINUE
        DO 23 IP=1,N
          B(IP)=B(IP)+Z(IP)
          D(IP)=B(IP)
          Z(IP)=0.
23      CONTINUE
24    CONTINUE
      WRITE(8,*) 'WARNING: 50 iterations should never happen'
      RETURN
      END


