Ccc   * $Author: mike $
Ccc   * $Date: 2002-04-15 13:22:23 $
Ccc   * $Id: tl.f,v 1.6 2002-04-15 13:22:23 mike Exp $
C
C        ND_NLV,IPH(NDLV),LMaxCC,IDefCC,IOPSYS
C        ND_NLV - Number of discrete levels to be included in the
C        inelastic scattering calculations (COMMON GLOBAL_I)
C
C        j from 1 to ND_NLV (maximum NDCOLLEV)
C        D_ELV(j),D_XJLV(j),D_LVP(j),IPH(j),D_DEF(j,NDDEFCC)
C        energy, spin, parity, number of phonons and deformation for each level
C        Sinl - total inelastic cross section to collective levels
C
C        DEFORMED, DEFAULT_ENERGY_FUNCTIONAL
C
C        If DEFAULT_ENERGY_FUNCTIONAL=.TRUE. then
C        {
C        SCAT2 optical model energy dependence is used so strenght
C        are calculated as follows:
C        POTe(i) = pot(i,1) + pot(i,2)*Ener + pot(i,3)*Ener*Ener    +
C       *            pot(i,4)*Ener*Ener*Ener  + pot(i,5)*dlog(Ener) +
C       *            pot(i,6)*dsqrt(Ener)
C        WARNING: EMPIRE version before 2.15.6 used the following:
C        POTe(i) = pot(i,1) + pot(i,2)*Ener + pot(i,3)*Ener*Ener    +
C       *            pot(i,4)*dlog(Ener)
C        }
C        If DEFAULT_ENERGY_FUNCTIONAL=.FALSE. then
C        {
C        Strenght is fixed for given energy so POTe(i) = pot(i,1)
C        this option will be used for RIPL potential
C        }

      SUBROUTINE HITL(Stl)
C
Ccc
Ccc   ************************************************************
Ccc   *                                                 class:ppu*
Ccc   *                      H I T L                             *
Ccc   *                                                          *
Ccc   * Calculates transmission coefficients for Heavy-Ion abs-  *
Ccc   * orption using either                                     *
Ccc   * input fusion x-section or input value of critical l CRL  *
Ccc   * or using distributed fusion barrier (if CSFUS.EQ.-1.)    *
Ccc   *                                                          *
Ccc   * input:none                                               *
Ccc   *                                                          *
Ccc   * output:STL-matrix of transmission coefficients (1s=1)    *
Ccc   *                                                          *
Ccc   * calls: BNDG                                              *
Ccc   *            WHERE                                         *
Ccc   *        CCFUS                                             *
Ccc   *            BAR                                           *
Ccc   *                POTENT                                    *
Ccc   *                    POT                                   *
Ccc   *            POT                                           *
Ccc   *        PUSH                                              *
Ccc   *            F                                             *
Ccc   *            G                                             *
Ccc   *                F                                         *
Ccc   *            INTGRS                                        *
Ccc   *        XFUS                                              *
Ccc   *                                                          *
Ccc   * author: M.Herman                                         *
Ccc   * date:   18.Feb.1993                                      *
Ccc   *                                                          *
Ccc   * revision:1    by:M.Herman                 on: 2.Mar.1994 *
Ccc   * Distributed fusion barrier absorption added.             *
Ccc   *                                                          *
Ccc   * revision:2    by:A.D'Arrigo & M.Herman    on:07.Dec.1994 *
Ccc   * Coupled channel fusion added                             *
Ccc   *                                                          *
Ccc   * revision:#    by:                         on:xx.mon.199x *
Ccc   ************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C
C     COMMON variables
C
      DOUBLE PRECISION A0R, AU, ETAk, HC, RAB, REDm, V0R
      COMMON /ONE   / V0R, A0R, RAB, REDm, AU, HC, ETAk
C
C     Dummy arguments
C
      DOUBLE PRECISION Stl(NDLW)
C
C     Local variables
C
      DOUBLE PRECISION arg, clf, homega, ra, rb, rbar, rred, xfu, xfum
      LOGICAL distrb
      INTEGER i
      DOUBLE PRECISION XFUS
C
      distrb = .FALSE.
C-----if critical l given in input jump directly to Tl calculations
      IF(CRL.LE.0.D0)THEN
C-----CCFUS calculations
         IF(CSRead.EQ.( - 2.D0))THEN
            CALL CCFUS(Stl)
            RETURN
         ENDIF
C--------CCFUS calculations  *** done ****
C--------check for distribution barrier
         IF(CSRead.EQ.( - 1.D0))distrb = .TRUE.
C--------calculate projectile+target binding energy if not done already
         IF(Q(0, 1).EQ.0.D0)CALL BNDG(0, 1, Q(0, 1))
         IF(distrb)THEN
            IF(BFUs.EQ.0.0D0)THEN
C--------------calculate fusion barrier using CCFUS routine BAR
               ra = 1.233*AEJc(0)**(1./3.) - 0.978/AEJc(0)**(1./3.)
               rb = 1.233*A(0)**(1./3.) - 0.978/A(0)**(1./3.)
               RAB = ra + rb + 0.29
               rred = ra*rb/(ra + rb)
               REDm = AEJc(0)*A(0)/(AEJc(0) + A(0))
C              AU = 931.5016
C              HC = 197.3286
               AU = AMUmev
               HC = HHBarc
               V0R = 30.08*(1. - 1.8*(1. - 2.*ZEJc(0)/AEJc(0))
     &               *(1. - 2.*Z(0)/A(0)))*rred + DV - 20.
               A0R = 0.63
               ETAk = 1.43997*ZEJc(0)*Z(0)
               CALL BAR(rbar, BFUs, homega)
               WRITE(6, *)'Fusion barrier is ', BFUs, ' MeV'
            ENDIF
            IF(SIG.EQ.0.0D0)SIG = 0.05*BFUs
            IF(IOUt.GT.0)THEN
               WRITE(6, *)'Distributed fusion barrier with extra push=', 
     &                    EXPush
               WRITE(6, *)'SIG=', SIG, ' and TRUNC=', TRUnc, 
     &                    ' has been used'
            ENDIF
            CALL PUSH(EIN, A(1), AEJc(0), A(0), BFUs, EXPush, SIG, 
     &                TRUnc, Stl, NLW, NDLW)
            RETURN
         ENDIF
C--------calculation of fusion Tl's with distributed barrier model
C        *** done ***
C--------prepare starting values for searching critical l
         IF(CSRead.LE.0.0D0)THEN
            clf = CRL - 2.0*DFUs
            IF(CRL - clf.LT.3.D0)clf = CRL - 3.
            IF(clf.LT.0.D0)clf = 10.
         ELSE
            CSFus = CSRead
            clf = 1.0
         ENDIF
         xfum = 0.0
 50      xfu = XFUS(EIN, AEJc(0), A(0), DFUs, clf)
         IF(xfu.LT.CSFus)THEN
            xfum = xfu
            clf = clf + 1.
            GOTO 50
         ELSE
            CRL = clf - 1 + (CSFus - xfum)/(xfu - xfum)
         ENDIF
         NLW = CRL + MAX(5.D0, 5.0D0*DFUs)
C-----setting transmission coefficients for fusion if not distr. barr.
      ENDIF
      DO i = 1, NDLW
         arg = (CRL - i + 1)/DFUs
         arg = MIN(174.0D0, arg)
         Stl(i) = 1.0/(1.0 + EXP((-arg)))
      ENDDO
      END
C
      SUBROUTINE RIPL2EMPIRE(Nejc, Nnuc, E)
C
C-----Sets CC optical model parameters according to RIPL
C
C     E must be in lab system !!!
C
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
      DOUBLE PRECISION E
      INTEGER Nejc, Nnuc
      INTEGER i, iainp, izinp, j, NDIM1, NDIM2, NDIM3, NDIM4, NDIM5, 
     &        NDIM6, NDIM7, k
C     Common blocks and declarations for ominput11.f [July 6, 2001]
C
C     Parameter statement
C
      PARAMETER(NDIM1 = 10, NDIM2 = 11, NDIM3 = 24, NDIM4 = 30, 
     &          NDIM5 = 10, NDIM6 = 10, NDIM7 = 120)
C
      CHARACTER*1 AUThor, REFer, SUMmary
      INTEGER IREf, IZMin, IZMax, IAMin, IAMax, IMOdel, JRAnge, NCOll, 
     &        NVIb, NISotop, IZ, IA, LMAx, IDEf, IZProj, IAProj, IREl, 
     &        IDR, IPArv, NPH, IPAr, JCOul, icoll(NDCOLLEV)
C
      REAL*4 EEMin, EEMax, EPOt, RCO, ACO, POT, BANdk, DDEf, EXV, DEFv, 
     &       THEtm, BETa0, GAMma0, XMUbeta, EEX, SPIn, SPInv, EECoul, 
     &       RRCoul, RCOul0, BETa, RCOul1, RCOul2
C
      COMMON /LIB   / AUThor(80), REFer(80), SUMmary(320), IREf, EEMin, 
     &                EEMax, IZMin, IZMax, IAMin, IAMax, IMOdel, 
     &                JRAnge(6), EPOt(6, NDIM1), RCO(6, NDIM1, NDIM2), 
     &                ACO(6, NDIM1, NDIM2), POT(6, NDIM1, NDIM3), 
     &                NCOll(NDIM4), NVIb(NDIM4), NISotop, IZ(NDIM4), 
     &                IA(NDIM4), LMAx(NDIM4), BANdk(NDIM4), 
     &                DDEf(NDIM4, NDIM5), IDEf(NDIM4), IZProj, IAProj, 
     &                EXV(NDIM7, NDIM4), IPArv(NDIM7, NDIM4), IREl, IDR, 
     &                NPH(NDIM7, NDIM4), DEFv(NDIM7, NDIM4), 
     &                THEtm(NDIM7, NDIM4), BETa0(NDIM4), GAMma0(NDIM4), 
     &                XMUbeta(NDIM4), EEX(NDIM6, NDIM4), 
     &                SPIn(NDIM6, NDIM4), IPAr(NDIM6, NDIM4), 
     &                SPInv(NDIM7, NDIM4), JCOul, EECoul(NDIM1), 
     &                RRCoul(NDIM1), RCOul0(NDIM1), BETa(NDIM1), 
     &                RCOul1(NDIM1), RCOul2(NDIM1)
C
      DOUBLE PRECISION eripl, ftmp, b(6, NDIM1, 12), SEXP
      DOUBLE PRECISION ecoul, ecoul2, EF, eta, rc
      DOUBLE PRECISION vstr, rrrr, aaaa
      INTEGER IWArn, itmp, ncalc, ilv, n
      LOGICAL coll_defined
      CHARACTER*80 ch_iuf
C
C
C-----For dispersive optical model potentials
C
      DOUBLE PRECISION DELTA_WD, DELTA_WV
      DOUBLE PRECISION DOM_INT, DOM_INT_T1, DOM_INT_T2
      DOUBLE PRECISION AS, BS, CS, AV, BV, EP, eff, ea
      DOUBLE PRECISION EEE, WVE, WDE, ecutdom
      DOUBLE PRECISION dwd, dwv, WV2, WV4, WD2, WD4
C
      COMMON /ENERGY/ EEE, EF, EP
      COMMON /WENERG/ WDE, WVE
      COMMON /PDATAS/ AS, BS, CS
      COMMON /PDATAV/ AV, BV
C
      EXTERNAL DELTA_WD, DELTA_WV, WV2, WV4, WD2, WD4
C-----
C
      COMMON /LOCAL / MODelcc
C
      DATA coll_defined/.FALSE./
      SAVE coll_defined
      iainp = A(Nnuc)
      izinp = Z(Nnuc)
C
      MODelcc = 0
C
C     IF ( IMOdel.EQ.0 ) model = 'spherical nucleus model'
C     IF ( IMOdel.EQ.1 ) model = 'coupled-channels rotational model'
C     IF ( IMOdel.EQ.2 ) model = 'vibrational model'
C     IF ( IMOdel.EQ.3 ) model = 'non-axial deformed model'
      IF(IMOdel.EQ.3)THEN
         WRITE(6, *)'WARNING: NON-AXIAL DEFORMED MODEL NOT IMPLEMENTED'
         IWArn = 5
         GOTO 200
      ENDIF
C
      IF(IMOdel.EQ.1 .OR. IMOdel.EQ.2)THEN
C        Imodel not used for non-inelastic channels
         IF(iainp.NE.A(0) .OR. izinp.NE.Z(0) .OR. AEJc(Nejc).NE.AEJc(0)
     &      .OR. ZEJc(Nejc).NE.ZEJc(0))GOTO 200
      ENDIF
      MODelcc = IMOdel
      IF(IMOdel.EQ.1 .AND. (.NOT.coll_defined))THEN
C        model = 'coupled-channels rotational model'
         coll_defined = .TRUE.
         IF(NISotop.EQ.0)THEN
            WRITE(6, *)'WARNING: NONE OF THE REQUESTED ISOTOPES IS '
            WRITE(6, *)'WARNING: INCLUDED IN THE SELECTED POTENTIAL.'
            WRITE(6, *)'WARNING: FILE WITH RIPL DISCRETE LEVELS CAN NOT'
            WRITE(6, *)'WARNING: BE CREATED.                    '
            WRITE(6, *)'WARNING: DEFAULT COLLECTIVE LEVELS WILL BE USED'
            GOTO 200
         ENDIF
         ncalc = 0
         DO n = 1, NISotop
            IF(iainp.EQ.IA(n) .AND. izinp.EQ.IZ(n))THEN
               ncalc = n
               IF(IDEf(n).GT.2*LMAx(n) .OR. NCOll(n).GT.NDCOLLEV)THEN
                  WRITE(6, *)'RIPL discrete level is wrong'
                  WRITE(6, *)'Too many levels or too many deformations'
                  WRITE(6, *)'Default collective levels will be used'
                  IWArn = 6
                  GOTO 200
               ENDIF
            ENDIF
         ENDDO
         IF(ncalc.EQ.0)THEN
            WRITE(6, *)'RIPL discrete level information is not used'
            WRITE(6, *)'Default collective levels will be used'
            GOTO 200
         ENDIF
         IF(NCOll(ncalc).EQ.0)THEN
            WRITE(6, *)'RIPL discrete level information is not used'
            WRITE(6, *)'Default collective levels will be used'
            GOTO 200
         ENDIF
         OPEN(32, FILE = 'TARGET_COLL.DAT')
         WRITE(32, *)
     &       'Collective levels from RIPL CC OMP, symm.rotational model'
         WRITE(32, *)'Dyn.deformations are not used in symm.rot.model'
         WRITE(32, '(1x,i3,1x,i3,a35)')izinp, iainp, 
     &                                 ' nucleus is treated as deformed'
         WRITE(32, *)
         DO n = 1, NISotop
            IF(iainp.EQ.IA(n) .AND. izinp.EQ.IZ(n))THEN
               WRITE(32, *)
     &                 '   Ncoll  Lmax IDef  Kgs  (Def(1,j),j=2,IDef,2)'
               WRITE(32, '(3x,3I5,1x,F5.1,1x,6(e10.3,1x))')NCOll(n), 
     &               LMAx(n), IDEf(n), BANdk(n), 
     &               (DDEf(n, k), k = 2, IDEf(n), 2)
               WRITE(32, *)
               WRITE(32, *)' N   E[MeV]  K   pi Iph   Dyn.Def.'
            ENDIF
         ENDDO
         OPEN(39, FILE = 'TARGET.LEV')
         READ(39, '(A80)') ch_iuf
         icoll(1) = 1
         DO k = 2, NCOll(ncalc)
            icoll(k) = 0
            eripl = EEX(k, ncalc)
            REWIND(39)
            ilv = -1
 20         ilv = ilv + 1
C           READ(39, '(a1,1x,i3,f7.3)', END = 50)ctmp, itmp, ftmp
            READ(39, '(I3,1X,F10.6)', END = 50)itmp, ftmp 
            IF(ABS(ftmp - eripl).GT.0.010)GOTO 20
            icoll(k) = ilv
 50      ENDDO
         CLOSE(39)
C--------Setting EMPIRE global variables
         ND_nlv = NCOll(ncalc)
         LMAxcc = LMAx(ncalc)
         IDEfcc = IDEf(ncalc)
         DO k = 2, IDEfcc, 2
            D_Def(1, k) = DDEf(ncalc, k)
         ENDDO
         DO k = 1, NCOll(ncalc)
C           The deformation for excited levels is not used in the pure
C           symm.rotational model but could be used for vibrational
C           rotational model so we are setting it to 0.01
C
            WRITE(32, 
     &            '(1x,I2,1x,F7.4,1x,F4.1,1x,i2,''.'',1x,I2,1x,e10.3)')
     &            icoll(k), EEX(k, ncalc), SPIn(k, ncalc), 
     &            IPAr(k, ncalc), 0, 0.01
C
C-----------Setting EMPIRE global variables
            ICOllev(k) = icoll(k)
            D_Elv(k) = EEX(k, ncalc)
            D_Xjlv(k) = SPIn(k, ncalc)
            D_Lvp(k) = FLOAT(IPAr(k, ncalc))
            IPH(k) = 0
         ENDDO
         CLOSE(32)
         WRITE(6, *)
         WRITE(6, *)
     &       'Collective levels from RIPL CC OMP, symm.rotational model'
         WRITE(6, *)'Dyn.deformations are not used in symm.rot.model'
         DEFormed = .TRUE.
         WRITE(6, '(1x,i3,1x,i3,a35)')izinp, iainp, 
     &                                ' nucleus is treated as deformed'
         WRITE(6, *)
         WRITE(6, *)'   Ncoll  Lmax IDef  Kgs  (Def(1,j),j=2,IDef,2)'
         WRITE(6, '(3x,3I5,1x,F5.1,1x,6(e10.3,1x))')ND_nlv, LMAxcc, 
     &         IDEfcc, D_Xjlv(1), (D_Def(1, j), j = 2, IDEfcc, 2)
         WRITE(6, *)
         WRITE(6, *)' N   E[MeV]  K   pi Iph   Dyn.Def.'
         DO i = 1, ND_nlv
            WRITE(6, '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,I2,1x,e10.3)')
     &            ICOllev(i), D_Elv(i), D_Xjlv(i), D_Lvp(i), IPH(i), 
     &            0.01
         ENDDO
         WRITE(6, *)
         WRITE(6, *)
      ENDIF
      IF(IMOdel.EQ.2 .AND. (.NOT.coll_defined))THEN
C--------model = 'vibrational model'
         coll_defined = .TRUE.
         IF(NISotop.EQ.0)THEN
            WRITE(6, *)'WARNING: NONE OF THE REQUESTED ISOTOPES IS '
            WRITE(6, *)'WARNING: INCLUDED IN THE SELECTED POTENTIAL.'
            WRITE(6, *)'WARNING: FILE WITH RIPL DISCRETE LEVELS CAN NOT'
            WRITE(6, *)'WARNING: BE CREATED.                    '
            WRITE(6, *)'WARNING: DEFAULT COLLECTIVE LEVELS WILL BE USED'
            GOTO 200
         ENDIF
         ncalc = 0
         DO n = 1, NISotop
            IF(iainp.EQ.IA(n) .AND. izinp.EQ.IZ(n))THEN
               ncalc = n
               DO k = 2, NVIb(ncalc)
                  IF(NPH(k, ncalc).EQ.3)THEN
                     IWArn = 6
                     WRITE(6, *)'NPH(k,i)=3 !!! in RIPL OMP'
                     WRITE(6, *)'Default collective levels will be used'
                     GOTO 200
                  ENDIF
               ENDDO
               IF(NVIb(n).GT.NDCOLLEV)THEN
                  WRITE(6, *)'RIPL discrete level is wrong'
                  WRITE(6, *)'Too many levels'
                  WRITE(6, *)'Default collective levels will be used'
                  IWArn = 6
                  GOTO 200
               ENDIF
            ENDIF
         ENDDO
         IF(ncalc.EQ.0)THEN
            WRITE(6, *)'RIPL discrete level information is not used'
            WRITE(6, *)'Default collective levels will be used'
            GOTO 200
         ENDIF
         IF(NCOll(ncalc).EQ.0)THEN
            WRITE(6, *)'RIPL discrete level information is not used'
            WRITE(6, *)'Default collective levels will be used'
            GOTO 200
         ENDIF
         OPEN(32, FILE = 'TARGET_COLL.DAT')
         WRITE(32, *)
     &           'Collective levels from RIPL CC OMP, vibrational model'
         WRITE(32, *)
         WRITE(32, '(1x,i3,1x,i3,a35)')izinp, iainp, 
     &                                ' nucleus is treated as spherical'
         DO n = 1, NISotop
            IF(iainp.EQ.IA(n) .AND. izinp.EQ.IZ(n))THEN
               WRITE(32, *)
               WRITE(32, '(3x,a6)')'   Ncoll'
               WRITE(32, '(3x,I5)')NVIb(n)
               WRITE(32, *)
               WRITE(32, *)' N   E[MeV]  J   pi Lph   Dyn.Def.'
            ENDIF
         ENDDO
         OPEN(39, FILE = 'TARGET.LEV')
         READ(39, '(A80)') ch_iuf
         icoll(1) = 1
         DO k = 2, NVIb(ncalc)
            icoll(k) = 0
            eripl = EXV(k, ncalc)
            REWIND(39)
            ilv = -1
 60         ilv = ilv + 1
C           READ(39, '(a1,1x,i3,f7.3)', END = 100)ctmp, itmp, ftmp
            READ(39, '(I3,1X,F10.6)', END = 100)itmp, ftmp 
            IF(ABS(ftmp - eripl).GT.0.010)GOTO 60
            icoll(k) = ilv
 100     ENDDO
         CLOSE(39)
C--------Setting EMPIRE global variables
         ND_nlv = NVIb(ncalc)
         IDEfcc = 2
         LMAxcc = 0
         DO k = 1, NVIb(ncalc)
            WRITE(32, 
     &            '(1x,I2,1x,F7.4,1x,F4.1,1x,i2,''.'',1x,I2,1x,e10.3)')
     &            icoll(k), EXV(k, ncalc), SPInv(k, ncalc), 
     &            IPArv(k, ncalc), NPH(k, ncalc), DEFv(k, ncalc)
C-----------Setting EMPIRE global variables
            ICOllev(k) = icoll(k)
            D_Elv(k) = EXV(k, ncalc)
            D_Xjlv(k) = SPInv(k, ncalc)
            D_Lvp(k) = FLOAT(IPArv(k, ncalc))
            IPH(k) = NPH(k, ncalc)
            D_Def(k, 2) = DEFv(k, ncalc)
         ENDDO
         CLOSE(32)
         WRITE(6, *)
         WRITE(6, *)
     &           'Collective levels from RIPL CC OMP, vibrational model'
         WRITE(6, *)
         DEFormed = .FALSE.
         WRITE(6, '(1x,i3,1x,i3,a35)')izinp, iainp, 
     &                               ' nucleus is treated as spherical '
         WRITE(6, *)
         WRITE(6, *)'   Ncoll'
         WRITE(6, '(3x,I5)')ND_nlv
         WRITE(6, *)
         WRITE(6, *)' N   E[MeV]  J   pi Iph   Dyn.Def.'
         DO i = 1, ND_nlv
            WRITE(6, '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,I2,1x,e10.3)')
     &            ICOllev(i), D_Elv(i), D_Xjlv(i), D_Lvp(i), IPH(i), 
     &            D_Def(i, 2)
         ENDDO
         WRITE(6, *)
         WRITE(6, *)
      ENDIF
C
C     WRITE (Ko,*) ' A=' ,
C     &     iainp , ' A out of the recommended range '
 200  IF(iainp.LT.IAMin .OR. iainp.GT.IAMax)IWArn = 1
C     WRITE (Ko,*) ' Z=' ,
C     &     izinp , ' Z out of the recommended range '
      IF(izinp.LT.IZMin .OR. izinp.GT.IZMax)IWArn = 2
C
      IF(E.LT.EEMin)THEN
C        WRITE (Ko,*)
C        &      'Energy requested lower than recommended for this potential'
C        WRITE (Ko,*) E , ' RIPL emin=' , EEMin
         EEMin = E + 0.1D0
         IWArn = 3
      ENDIF
      IF(E.GT.EEMax)THEN
C        WRITE (Ko,*)
C        &     'Energy requested higher than recommended for this potential'
C        WRITE (Ko,*) E , ' RIPL emax=' , EEMax
         EEMax = E + 1000.D0
         IWArn = 4
      ENDIF
      OMEmin(Nejc, Nnuc) = EEMin
      OMEmax(Nejc, Nnuc) = EEMax
      IRElat(Nejc, Nnuc) = IREl
      eta = (XN(Nnuc) - Z(Nnuc))/A(Nnuc)
      xmas_nejc = (AEJc(Nejc)*AMUmev + XMAss_ej(Nejc))/AMUmev
      xmas_nnuc = (A(Nnuc)*AMUmev + XMAss(Nnuc))/AMUmev
      EF = -10.6D0
      EP = EF
      ea = 1000.1D0
      atar = xmas_nnuc
      ztar = FLOAT(izinp)
      ecoul2 = 0.
      rc = 0.
      ecoul = 0.4D0*FLOAT(izinp)/FLOAT(iainp)**(1.D0/3.D0)
      DO j = 1, JCOul
         IF(E.LE.EECoul(j))GOTO 300
      ENDDO
 300  rc = RRCoul(j) + RCOul0(j)/FLOAT(iainp)**(1.D0/3.D0) + RCOul1(j)
     &     *FLOAT(iainp)**( - 2.D0/3.D0) + RCOul2(j)*FLOAT(iainp)
     &     **( - 5.D0/3.D0)
      IF(rc.GT.0.D0)ecoul2 = 1.73D0*FLOAT(izinp)
     &                       /(rc*FLOAT(iainp)**(1.D0/3.D0))
      RCOul(Nejc, Nnuc) = rc
      EEE = E
      gamma = 1.D0
      IF(IREl.EQ.2)THEN
C        Target system mass in MeV
         emtar = xmas_nnuc*AMUmev
C        Total system mass in MeV
         emtot = (xmas_nnuc + xmas_nejc)*AMUmev
C        Total kinetic energy in cm
         tcm = SQRT(2*emtar*E + emtot**2) - emtot
C
C        Relativistic correction to the potential
C        (Used for example in Madland potential REF=2001)
C
C        Obtained from Dirac equation considering non relativistic target !!
C
         gamma = 1.D0 + tcm/(tcm + 2*xmas_nejc*AMUmev)
C
      ENDIF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     RIPL-II
C     1. The ordering of potentials has been changed to follow ECIS96:
C
C     i=1         real volume potential
C     i=2         imaginary volume potential
C     i=3         real surface derivative potential
C     i=4         imaginary surface derivative potential
C     i=5         real spin-orbit potential
C     i=6         imaginary spin-orbit potential
C
C     2. The number of terms possible in the potential strengths has
C     been increased from 21 to 24.  The special case potentials
C     are now:
C
C     pot(i,j,22) - Smith et al. , OMP reference # 118
C     pot(i,j,23) - Varner et al., OMP reference # 2100, 5100
C     pot(i,j,24) - Koning potentials, reference # 2404, 2405,
C     5404, 5405
C=======================================================================
C     Volume real potential: Woods-Saxon
C     ****
C     RR   = radius (fm)
C     AA   = diffuseness (fm)
C     POTE = strength (MeV)
      i = 1
C     about 140 lines are repeated 5 times, therefore they are put into
C     the file ripl_omp.inc file and included whenever necessary 
      INCLUDE "ripl_omp.inc"
      VOM(1, Nejc, Nnuc) = vstr
      RVOm(1, Nejc, Nnuc) = rrrr
      AVOm(Nejc, Nnuc) = aaaa
C     Volume imaginary potential: Woods-Saxon
C     ****
      i = 2
      IF(JRAnge(i).GT.0)THEN
         INCLUDE "ripl_omp.inc"
         WOMv(1, Nejc, Nnuc) = vstr
         RWOmv(1, Nejc, Nnuc) = rrrr
         AWOmv(Nejc, Nnuc) = aaaa
      ENDIF
C     Surface imaginary potential: if R(2) > 0., Woods-Saxon derivative
C     if R(2) < 0., Gaussian
C     ****
      i = 4
      IF(JRAnge(i).GT.0)THEN
         INCLUDE "ripl_omp.inc"
         WOMs(1, Nejc, Nnuc) = vstr
         RWOm(1, Nejc, Nnuc) = rrrr
         AWOm(Nejc, Nnuc) = aaaa
C-----if rco(2,j,1) >0.0: Woods-Saxon derivative surface potential
C-----if rco(2,j,1) <0.0: Gaussian surface potential.
         IF(RCO(2, j, 1).GT.0.0)SFIom(Nejc, Nnuc) = 1.D0
         IF(RCO(2, j, 1).LT.0.0)SFIom(Nejc, Nnuc) = -1.D0
      ENDIF
C     Real spin-orbit
C     ****
      i = 5
      IF(JRAnge(i).GT.0)THEN
         INCLUDE "ripl_omp.inc"
         VSO(1, Nejc, Nnuc) = vstr
         RVSo(1, Nejc, Nnuc) = rrrr
         AVSo(Nejc, Nnuc) = aaaa
      ENDIF
C     Imaginary spin-orbit
C     ****
      i = 6
      IF(JRAnge(i).GT.0)THEN
         INCLUDE "ripl_omp.inc"
         WSO(1, Nejc, Nnuc) = vstr
         RWSo(1, Nejc, Nnuc) = rrrr
         AWSo(Nejc, Nnuc) = aaaa
      ENDIF
      EEP(Nejc, Nnuc) = EP
      EEA(Nejc, Nnuc) = ea
      EEFermi(Nejc, Nnuc) = EF
C     Below only for dispersive optical model potentials
      IF(IDR.EQ.0)RETURN
C     Real surface contribution from Dispersive relation
C     ****
      i = 2
C     Only one energy range
      j = 1
      dwv = 0.D0
      ecutdom = 1000.D0
      IF(POT(i, j, 24).NE.0.)THEN
         AV = DBLE(INT(100000*POT(i,j,1)))/100000
         BV = DBLE(INT(100000*POT(i,j,3)))/100000
         IF(POT(i, j, 13).EQ.2)THEN
C           DOM integral
            WVE = WV2(AV, BV, EP, EEE)
            dwv = 2*DOM_INT(DELTA_WV, WV2, EF, EF + 50.D0, ecutdom, EEE, 
     &            WVE)
            IF(ea.LT.1000.D0)dwv = dwv + DOM_INT_T1(AV, EF, ea, EEE)
     &                             + DOM_INT_T2(EF, ea, EEE)
         ENDIF
         IF(POT(i, j, 13).EQ.4)THEN
C           DOM integral
            WVE = WV4(AV, BV, EP, EEE)
            dwv = 2*DOM_INT(DELTA_WV, WV4, EF, EF + 50.D0, ecutdom, EEE, 
     &            WVE)
            IF(ea.LT.1000.D0)dwv = dwv + DOM_INT_T1(AV, EF, ea, EEE)
     &                             + DOM_INT_T2(EF, ea, EEE)
         ENDIF
      ENDIF
      dwd = 0.D0
      i = 4
C     Only one energy range
      j = 1
      IF(POT(i, j, 24).NE.0.)THEN
         AS = DBLE(INT(100000*POT(i,j,1)))/100000
         BS = DBLE(INT(100000*POT(i,j,6)))/100000
         CS = DBLE(INT(100000*POT(i,j,2)))/100000
         IF(POT(i, j, 13).EQ.2)THEN
C           DOM integral
            WDE = WD2(AS, BS, CS, EP, EEE)
            dwd = 2*DOM_INT(DELTA_WD, WD2, EF, EF + 20.D0, ecutdom, EEE, 
     &            WDE)
         ENDIF
         IF(POT(i, j, 13).EQ.4)THEN
C           DOM integral
            WDE = WD4(AS, BS, CS, EP, EEE)
            dwd = 2*DOM_INT(DELTA_WD, WD4, EF, EF + 30.D0, ecutdom, EEE, 
     &            WDE)
         ENDIF
      ENDIF
C-------DWD,DWV are calculated for a given surface potential parameterization
C-------using dispersion relations
C
C     Real surface contribution
      VOMs(1, Nejc, Nnuc) = gamma*dwd
C-------Real volume contribution from Dispersive relation
C-------added to real volume potential
      VOM(1, Nejc, Nnuc) = VOM(1, Nejc, Nnuc) + gamma*dwv
      END
C
C
C
      SUBROUTINE OMPAR(Nejc, Nnuc, Eilab, Eicms, Mi, Mt, Rrmu, Ak2, 
     &                 Komp, Ko, Ikey)
C
C	IKEY < 0  :   EIlab is given
C	IKEY > 0  :   EIcms is given
C
C-----Sets optical model parameters
C
C     komp   = 29 (usually), 33(for the inelastic channel)
C     kompin = 18 (usually), 33(for the inelastic channel)
C
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
      INTEGER i, iaejcr, ianucr, ieof, ierr, ipoten, ki, Komp
C
C     Dummy arguments
C
      DOUBLE PRECISION ener, Eilab, Eicms, Mi, Mt, RMU, Ak2
      INTEGER Nejc, Nnuc, Ko
C
C     Local variables
C
      DOUBLE PRECISION a13, a2, a3, asym, omemaxr, omeminr, rcoulr, 
     &                 rnonlr, sfiomr
C
      CHARACTER*2 symbnucr, symbejcr
C
      CHARACTER*2 dum
      INTEGER izaejcr, izar
C
      LOGICAL fexist, relcal
C
      COMMON /LOCAL / MODelcc
C
C-----Erasing parameters of O.M.P.
      DO i = 1, NDVOM
         VOM(i, Nejc, Nnuc) = 0.0
         VOMs(i, Nejc, Nnuc) = 0.0
      ENDDO
      DO i = 1, NDWOM
         WOMv(i, Nejc, Nnuc) = 0.0
         WOMs(i, Nejc, Nnuc) = 0.0
      ENDDO
      DO i = 1, NDVSO
         WSO(i, Nejc, Nnuc) = 0.0
         VSO(i, Nejc, Nnuc) = 0.0
      ENDDO
      RVOm(1, Nejc, Nnuc) = 0.D0
      RWOm(1, Nejc, Nnuc) = 0.D0
      RVSo(1, Nejc, Nnuc) = 0.D0
      AVOm(Nejc, Nnuc) = 0.D0
      AWOm(Nejc, Nnuc) = 0.D0
      AVSo(Nejc, Nnuc) = 0.D0
C-----set nonlocality range to 0. (as usually is the case)
      RNOnl(Nejc, Nnuc) = 0.0
C-----set validity range to any energy (can be modified later)
      OMEmin(Nejc, Nnuc) = 0.0
      OMEmax(Nejc, Nnuc) = 1000.0
C-----set relativistic calculation key to 0
      IRElat(Nejc, Nnuc) = 0
C     Nonlocality in the Wvol is neglected ususally
      EEA(Nejc, Nnuc) = 1000.1D0
C     Arbitrarily default value
      EEFermi(Nejc, Nnuc) = -10.6
      EEP(Nejc, Nnuc) = EEFermi(Nejc, Nnuc)
      IRElat(Nejc, Nnuc) = 0
C
C-----Checking first RIPL potentials
C
      IF(RIPl_omp(Nejc))THEN
         fexist = OMPar_riplf
         IF(CCCalc)fexist = OMParfcc
C--------OMPAR.RIPL file exists
         IF(fexist)THEN
C-----------komp = 29
            ipoten = KTRlom(Nejc, Nnuc)
            CALL FINDPOT_OMPAR_RIPL(Komp, ieof, ipoten, Nnuc, Nejc)
            IF(ieof.EQ.0)THEN
C--------------Reading potential parameters from OMPAR.RIPL(OMPAR.DIR) file
               CALL READ_OMPAR_RIPL(Komp, ierr, irel)
C--------------GOTO RIPL2EMPIRE (20)
               IF(ierr.EQ.0)GOTO 50
            ENDIF
C-----------ieof<>0 or ierr<>0
            WRITE(6, *)' '
            IF(CCCalc)THEN
               WRITE(6, 
     &'('' OM parameters for '',I3,''-'',A2,'' +'',I2,''-''        ,A2,'
     &' not found in the OMPAR.DIR file'')')INT(A(Nnuc)), SYMb(Nnuc), 
     &INT(AEJc(Nejc)), SYMbe(Nejc)
            ELSE
               WRITE(6, 
     &'('' OM parameters for '',I3,''-'',A2,'' +'',I2,''-''        ,A2,'
     &' not found in the OMPAR.RIPL file'')')INT(A(Nnuc)), SYMb(Nnuc), 
     &INT(AEJc(Nejc)), SYMbe(Nejc)
            ENDIF
            WRITE(6, *)'I will resort to parameters from RIPL database'
            WRITE(6, *)' '
         ENDIF
C
C--------OMPAR.RIPL(OMPAR.DIR) file does not exists
C--------or reading error happened
C--------(reading from the RIPL database)
C
         ipoten = KTRlom(Nejc, Nnuc)
         ki = 26
C--------Searching in the RIPL database for IPOTEN catalog number
         CALL FINDPOT(ki, ieof, ipoten)
C--------Here ieof must be 0 always because we checked in input.f
         IF(ieof.GT.0)STOP 'PROBLEM with RIPL OMP library'
C--------Reading o.m.  potential parameters for IPOTEN catalog number
         CALL OMIN(ki, ieof, irel)
C
C------- Ener must be in LAB system, EFermi must be calculated from masses
C
 50      IRElat(Nejc, Nnuc) = irel
C
         relcal = .FALSE.
         IF(IRElat(Nejc, Nnuc).GT.0 .OR. RELkin)relcal = .TRUE.
         IF(Ikey.LT.0)THEN
            ener = Eilab
            CALL KINEMA(Eilab, Eicms, Mi, Mt, Rrmu, Ak2, 1, relcal)
         ELSE
            CALL KINEMA(Eilab, Eicms, Mi, Mt, Rrmu, Ak2, 2, relcal)
            ener = Eilab
         ENDIF
C
         CALL RIPL2EMPIRE(Nejc, Nnuc, ener)
         IF(CCCalc)MODelecis = MODelcc
C
C--------GOTO CHECK&SAVE
C
C        GOTO 300
C
      ELSE
C
C--------.NOT.RIPL_OMP(Nejc)
         relcal = .FALSE.
         IF(RELkin)relcal = .TRUE.
         IF(Ikey.LT.0)THEN
            ener = Eilab
            CALL KINEMA(Eilab, Eicms, Mi, Mt, Rrmu, Ak2, 1, relcal)
         ELSE
            CALL KINEMA(Eilab, Eicms, Mi, Mt, Rrmu, Ak2, 2, relcal)
            ener = Eilab
         ENDIF
C
         fexist = OMParf
         IF(CCCalc)fexist = OMParfcc
C--------OMPAR.INT exists
         IF(fexist)THEN
            REWIND Ko
 60         READ(Ko, '(19X,I6,2X,I6)', END = 80)izar, izaejcr
            READ(Ko, '(10X,5F10.4)')omeminr, omemaxr, sfiomr, rnonlr, 
     &                              rcoulr
            IF(izar.EQ.IZA(Nnuc) .AND. izaejcr.EQ.IZAejc(Nejc) .AND. 
     &         ener.GE.omeminr .AND. ener.LT.omemaxr)THEN
               OMEmin(Nejc, Nnuc) = omeminr
               OMEmax(Nejc, Nnuc) = omemaxr
               SFIom(Nejc, Nnuc) = sfiomr
               RNOnl(Nejc, Nnuc) = rnonlr
               RCOul(Nejc, Nnuc) = rcoulr
               READ(Ko, '(10X,8F10.4)')(VOM(i, Nejc, Nnuc), i = 2, NDVOM
     &                                 ), RVOm(1, Nejc, Nnuc), 
     &                                 AVOm(Nejc, Nnuc)
               READ(Ko, '(10X,8F10.4)')(WOMv(i, Nejc, Nnuc), i = 2, 
     &                                 NDWOM), RWOmv(1, Nejc, Nnuc), 
     &                                 AWOmv(Nejc, Nnuc)
               READ(Ko, '(10X,8F10.4)')(WOMs(i, Nejc, Nnuc), i = 2, 
     &                                 NDWOM), RWOm(1, Nejc, Nnuc), 
     &                                 AWOm(Nejc, Nnuc)
               READ(Ko, '(10X,8F10.4)')(VSO(i, Nejc, Nnuc), i = 2, NDVSO
     &                                 ), RVSo(1, Nejc, Nnuc), 
     &                                 AVSo(Nejc, Nnuc)
               READ(Ko, '(10X,8F10.4)')(WSO(i, Nejc, Nnuc), i = 2, NDVSO
     &                                 ), RWSo(1, Nejc, Nnuc), 
     &                                 AWSo(Nejc, Nnuc)
C
C--------------GOTO OMP depths calculation (200)
C
               GOTO 150
            ELSE
               DO i = 1, 5
                  READ(Ko, '(A2)')dum
               ENDDO
               GOTO 60
            ENDIF
 80         WRITE(6, *)' '
            IF(CCCalc)THEN
               WRITE(6, 
     &'('' OM parameters for '',I3,''-'',A2,'' +'',I2,''-''        ,A2,'
     &' not found in the OMPAR.DIR file'')')INT(A(Nnuc)), SYMb(Nnuc), 
     &INT(AEJc(Nejc)), SYMbe(Nejc)
            ELSE
               WRITE(6, 
     &'('' OM parameters for '',I3,''-'',A2,'' +'',I2,''-''        ,A2,'
     &' not found in the OMPAR.INT file'')')INT(A(Nnuc)), SYMb(Nnuc), 
     &INT(AEJc(Nejc)), SYMbe(Nejc)
            ENDIF
            WRITE(6, *)'I will resort to default parameters '
            WRITE(6, *)' '
         ENDIF
C
C--------OMPAR.INT does not exists
C--------or reading error happened
C
C--------DEFAULT EMPIRE parameters
C
         a13 = A(Nnuc)**0.333333
         asym = (XN(Nnuc) - Z(Nnuc))/A(Nnuc)
         a2 = A(Nnuc)**2
         a3 = A(Nnuc)*a2
         IF(KTRlom(Nejc, Nnuc).NE.0)THEN
            IF(AEJc(Nejc).NE.1D0 .OR. ZEJc(Nejc).NE.0.D0)THEN
               IF(AEJc(Nejc).EQ.1D0 .AND. ZEJc(Nejc).EQ.1.D0)THEN
C---------------------------------------------------------
C                 P R O T O N S
C---------------------------------------------------------
                  IF(KTRlom(Nejc, Nnuc).EQ.1)THEN
C
C--------------------Bjorklund-Fernbach parameters /proton/
C
                     AWOm(Nejc, Nnuc) = 1.2
                     AVOm(Nejc, Nnuc) = 0.65
                     AVSo(Nejc, Nnuc) = 0.65
                     RVOm(1, Nejc, Nnuc) = 1.25
                     RWOm(1, Nejc, Nnuc) = 1.25
                     RVSo(1, Nejc, Nnuc) = 1.25
                     VOM(2, Nejc, Nnuc) = 49.66
                     VOM(3, Nejc, Nnuc) = -0.424
                     VOM(4, Nejc, Nnuc) = -0.0042
                     WOMs(2, Nejc, Nnuc) = 1.5
                     WOMs(3, Nejc, Nnuc) = 4.35
                     VSO(2, Nejc, Nnuc) = 12.
                     VSO(3, Nejc, Nnuc) = -1.79
                     SFIom(Nejc, Nnuc) = -1.
                     GOTO 150
                  ENDIF
C
                  IF(KTRlom(Nejc, Nnuc).EQ.2)THEN
C
C--------------------Becchetti-Greenlees 1969 parameters /proton/
C
                     RVOm(1, Nejc, Nnuc) = 1.17
                     RWOm(1, Nejc, Nnuc) = 1.32
                     RVSo(1, Nejc, Nnuc) = 1.01
                     AVOm(Nejc, Nnuc) = 0.75
                     AWOm(Nejc, Nnuc) = 0.51 + 0.7*asym
                     AVSo(Nejc, Nnuc) = 0.75
                     VOM(2, Nejc, Nnuc) = 54.0 + 0.4*Z(Nnuc)
     &                  /a13 + 24.*asym
                     VOM(3, Nejc, Nnuc) = -0.32
                     WOMs(2, Nejc, Nnuc) = 11.8 + 12.0*asym
                     WOMs(3, Nejc, Nnuc) = -0.25
                     WOMv(2, Nejc, Nnuc) = -2.7
                     WOMv(3, Nejc, Nnuc) = 0.22
                     VSO(2, Nejc, Nnuc) = 6.2
                     SFIom(Nejc, Nnuc) = 1.
                     GOTO 150
                  ENDIF
C
                  IF(KTRlom(Nejc, Nnuc).EQ.3)THEN
C
C--------------------Menet    et al. 1971 parameters /proton/
C
                     RVOm(1, Nejc, Nnuc) = 1.25
                     RWOm(1, Nejc, Nnuc) = 1.25
                     RVSo(1, Nejc, Nnuc) = 1.25
                     AVOm(Nejc, Nnuc) = 0.65
                     AWOm(Nejc, Nnuc) = 0.47
                     AVSo(Nejc, Nnuc) = 0.47
                     VOM(2, Nejc, Nnuc) = 53.3 - 0.4*Z(Nnuc)
     &                  /a13 + 27.*asym
                     VOM(3, Nejc, Nnuc) = -0.55
                     WOMs(2, Nejc, Nnuc) = 13.5
                     VSO(2, Nejc, Nnuc) = 7.5
                     SFIom(Nejc, Nnuc) = 1.
                     GOTO 150
                  ENDIF
                  GOTO 120
               ELSEIF(AEJc(Nejc).EQ.2D0 .AND. ZEJc(Nejc).EQ.1.D0)THEN
C---------------------------------------------------------
C                 D E U T E R O N S
C---------------------------------------------------------
                  IF(KTRlom(Nejc, Nnuc).EQ.1)THEN
C
C--------------------Perey-Perey 1963 parameters /deuteron/
C
                     RVOm(1, Nejc, Nnuc) = 1.15
                     RWOm(1, Nejc, Nnuc) = 1.34
                     RVSo(1, Nejc, Nnuc) = 0.0
                     AVOm(Nejc, Nnuc) = 0.81
                     AWOm(Nejc, Nnuc) = 0.68
                     AVSo(Nejc, Nnuc) = 0.0
                     VOM(2, Nejc, Nnuc) = 81.0 + 2.0*Z(Nnuc)/a13
                     VOM(3, Nejc, Nnuc) = -0.22
                     WOMv(2, Nejc, Nnuc) = 14.4
                     WOMv(3, Nejc, Nnuc) = 0.24
                     RCOul(Nejc, Nnuc) = 1.15
                     SFIom(Nejc, Nnuc) = 1.
                     GOTO 150
                  ENDIF
                  GOTO 120
               ELSEIF(AEJc(Nejc).EQ.3D0 .AND. ZEJc(Nejc).EQ.1.D0)THEN
C---------------------------------------------------------
C                 T R I T I U M  -  3
C---------------------------------------------------------
                  IF(KTRlom(Nejc, Nnuc).EQ.1)THEN
C
C--------------------Becchetti-Greenless parameters /tritium-3/
C
                     RVOm(1, Nejc, Nnuc) = 1.20
                     RWOm(1, Nejc, Nnuc) = 1.40
                     RWOmv(1, Nejc, Nnuc) = 1.40
                     RVSo(1, Nejc, Nnuc) = 1.20
                     AVOm(Nejc, Nnuc) = 0.72
                     AWOm(Nejc, Nnuc) = 0.84
                     AWOmv(Nejc, Nnuc) = 0.84
                     AVSo(Nejc, Nnuc) = 0.72
                     VOM(2, Nejc, Nnuc) = 165.0 - 6.4*asym
                     VOM(3, Nejc, Nnuc) = -0.17
                     WOMv(2, Nejc, Nnuc) = 46.0 - 110.*asym
                     WOMv(3, Nejc, Nnuc) = -0.33
                     VSO(2, Nejc, Nnuc) = 2.5
                     RCOul(Nejc, Nnuc) = 1.30
                     SFIom(Nejc, Nnuc) = 1.
                     GOTO 150
                  ENDIF
                  GOTO 120
               ELSEIF(AEJc(Nejc).EQ.3D0 .AND. ZEJc(Nejc).EQ.2.D0)THEN
C---------------------------------------------------------
C                 H E L I U M - 3
C---------------------------------------------------------
C
                  IF(KTRlom(Nejc, Nnuc).EQ.1)THEN
C
C--------------------Becchetti-Greenless parameters /helium-3/
C
                     RVOm(1, Nejc, Nnuc) = 1.20
                     RWOm(1, Nejc, Nnuc) = 1.40
                     RWOmv(1, Nejc, Nnuc) = 1.40
                     RVSo(1, Nejc, Nnuc) = 1.20
                     AVOm(Nejc, Nnuc) = 0.72
                     AWOm(Nejc, Nnuc) = 0.88
                     AWOmv(Nejc, Nnuc) = 0.88
                     AVSo(Nejc, Nnuc) = 0.72
                     VOM(2, Nejc, Nnuc) = 151.9 + 50.*asym
                     VOM(3, Nejc, Nnuc) = -0.17
                     WOMv(2, Nejc, Nnuc) = 41.7 + 44.0*asym
                     WOMv(3, Nejc, Nnuc) = -0.33
                     VSO(2, Nejc, Nnuc) = 2.5
                     RCOul(Nejc, Nnuc) = 1.30
                     SFIom(Nejc, Nnuc) = 1.
                     GOTO 150
                  ENDIF
                  GOTO 120
               ELSEIF(AEJc(Nejc).EQ.4D0 .AND. ZEJc(Nejc).EQ.2.D0)THEN
C---------------------------------------------------------
C                 A L P H A S
C---------------------------------------------------------
                  IF(KTRlom(Nejc, Nnuc).EQ.1)THEN
C
C--------------------McFadden and Satchler parameters /alpha/
C
                     RVOm(1, Nejc, Nnuc) = 1.4
                     RWOm(1, Nejc, Nnuc) = 1.4
                     RVSo(1, Nejc, Nnuc) = 0.0
                     AVOm(Nejc, Nnuc) = 0.52
                     AWOm(Nejc, Nnuc) = 0.52
                     AWOmv(Nejc, Nnuc) = 0.52
                     AVSo(Nejc, Nnuc) = 0.0
                     VOM(2, Nejc, Nnuc) = 185.0
                     WOMv(2, Nejc, Nnuc) = 25.0
                     SFIom(Nejc, Nnuc) = 1.
                     RCOul(Nejc, Nnuc) = 1.30
                     GOTO 150
                  ENDIF
                  GOTO 100
               ELSE
                  IF(AEJc(Nejc).EQ.6D0 .AND. ZEJc(Nejc).EQ.3.D0)GOTO 100
                  IF(AEJc(Nejc).EQ.7D0 .AND. ZEJc(Nejc).EQ.3.D0)GOTO 100
                  IF(AEJc(Nejc).NE.7D0 .OR. ZEJc(Nejc).NE.4.D0)GOTO 120
                  GOTO 100
               ENDIF
C---------------------------------------------------------
C              N E U T R O N S
C---------------------------------------------------------
            ENDIF
C
            IF(KTRlom(Nejc, Nnuc).EQ.1)THEN
C
C--------------Bjorklund-Fernbach 1958 parameters/neutron/
C
               AWOm(Nejc, Nnuc) = 0.95
               AVOm(Nejc, Nnuc) = 0.65
               AVSo(Nejc, Nnuc) = 0.65
               RVOm(1, Nejc, Nnuc) = 1.25
               RVSo(1, Nejc, Nnuc) = 1.25
               RWOm(1, Nejc, Nnuc) = 1.25
               VOM(2, Nejc, Nnuc) = 49.66
               VOM(3, Nejc, Nnuc) = -0.424
               VOM(4, Nejc, Nnuc) = -0.0042
               WOMs(2, Nejc, Nnuc) = 1.5
               WOMs(3, Nejc, Nnuc) = 4.35
               VSO(2, Nejc, Nnuc) = 12.
               VSO(3, Nejc, Nnuc) = -1.79
               SFIom(Nejc, Nnuc) = -1.
               OMEmin(Nejc, Nnuc) = 0.0
               OMEmax(Nejc, Nnuc) = 1000.
               GOTO 150
            ENDIF
C
            IF(KTRlom(Nejc, Nnuc).EQ.2)THEN
C
C--------------Moldauer-1963 parameters
C
               RVOm(1, Nejc, Nnuc) = 1.16 + 0.6/a13
               RWOm(1, Nejc, Nnuc) = 1.16 + 1.1/a13
               RVSo(1, Nejc, Nnuc) = 1.16 + 0.6/a13
               AVOm(Nejc, Nnuc) = 0.62
               AWOm(Nejc, Nnuc) = 0.5
               AVSo(Nejc, Nnuc) = 0.62
               VSO(2, Nejc, Nnuc) = 7.0
               VOM(2, Nejc, Nnuc) = 46.0
               WOMs(2, Nejc, Nnuc) = 14.0
               SFIom(Nejc, Nnuc) = -1.
               OMEmin(Nejc, Nnuc) = 0.0
               OMEmax(Nejc, Nnuc) = 1000.
               GOTO 150
            ENDIF
C
            IF(KTRlom(Nejc, Nnuc).EQ.3)THEN
C
C--------------Becchetti-Greenlees-1969 parameters /neutron/
C
               RVOm(1, Nejc, Nnuc) = 1.17
               RWOm(1, Nejc, Nnuc) = 1.26
               RVSo(1, Nejc, Nnuc) = 1.01
               AVOm(Nejc, Nnuc) = 0.75
               AWOm(Nejc, Nnuc) = 0.58
               AVSo(Nejc, Nnuc) = 0.75
               VOM(2, Nejc, Nnuc) = 56.3 - 24.0*asym
               VOM(3, Nejc, Nnuc) = -0.32
               WOMs(2, Nejc, Nnuc) = 13.0 - 12.0*asym
               WOMs(3, Nejc, Nnuc) = -0.25
               WOMv(2, Nejc, Nnuc) = -1.56
               WOMv(3, Nejc, Nnuc) = 0.22
               VSO(2, Nejc, Nnuc) = 6.2
               SFIom(Nejc, Nnuc) = 1.
               OMEmin(Nejc, Nnuc) = 0.0
               OMEmax(Nejc, Nnuc) = 1000.
               GOTO 150
            ENDIF
C
            IF(KTRlom(Nejc, Nnuc).EQ.4)THEN
C
C--------------Wilmore-Hodgson-1964 parameters /neutron/
C
               RVOm(1, Nejc, Nnuc) = 1.322 - 0.00076*A(Nnuc)
     &                               + 4.E-06*a2 - 8.E-09*a3
               RWOm(1, Nejc, Nnuc) = 1.266 - 0.00037*A(Nnuc)
     &                               + 2.E-06*a2 - 4.E-09*a3
               RVSo(1, Nejc, Nnuc) = 1.322 - 0.00076*A(Nnuc)
     &                               + 4.E-06*a2 - 8.E-09*a3
               AVOm(Nejc, Nnuc) = 0.66
               AWOm(Nejc, Nnuc) = 0.48
               AVSo(Nejc, Nnuc) = 0.66
               VOM(2, Nejc, Nnuc) = 47.01
               VOM(3, Nejc, Nnuc) = -0.267
               VOM(4, Nejc, Nnuc) = -0.00018
               WOMs(2, Nejc, Nnuc) = 9.52
               WOMs(3, Nejc, Nnuc) = -0.053
               VSO(2, Nejc, Nnuc) = 7.0
               SFIom(Nejc, Nnuc) = 1.
               OMEmin(Nejc, Nnuc) = 0.0
               OMEmax(Nejc, Nnuc) = 1000.
               GOTO 150
            ENDIF
C
            IF(KTRlom(Nejc, Nnuc).EQ.5)THEN
C
C--------------Patterson et al. 1976 parameters /neutron/
C
               RVOm(1, Nejc, Nnuc) = 1.17
               RWOm(1, Nejc, Nnuc) = 1.32
               RVSo(1, Nejc, Nnuc) = 1.01
               AVOm(Nejc, Nnuc) = 0.75
               AWOm(Nejc, Nnuc) = 0.51 + 0.7*asym
               AVSo(Nejc, Nnuc) = 0.75
               VOM(2, Nejc, Nnuc) = 55.8 - 17.7*asym
               VOM(3, Nejc, Nnuc) = -0.32
               WOMs(2, Nejc, Nnuc) = 9.60 - 18.1*asym
               WOMs(3, Nejc, Nnuc) = ( - 0.22) + 0.31*asym
               WOMv(2, Nejc, Nnuc) = 1.4
               WOMv(3, Nejc, Nnuc) = 0.22
               VSO(2, Nejc, Nnuc) = 6.2
               SFIom(Nejc, Nnuc) = 1.
               OMEmin(Nejc, Nnuc) = 0.0
               OMEmax(Nejc, Nnuc) = 1000.
               GOTO 150
            ENDIF
C
            IF(KTRlom(Nejc, Nnuc).EQ.6)THEN
C
C--------------Rapaport 1979 parameters /neutron/
C
               RVOm(1, Nejc, Nnuc) = 1.198
               RWOm(1, Nejc, Nnuc) = 1.295
               RVSo(1, Nejc, Nnuc) = 1.01
               AVOm(Nejc, Nnuc) = 0.663
               AWOm(Nejc, Nnuc) = 0.59
               AVSo(Nejc, Nnuc) = 0.75
               VSO(2, Nejc, Nnuc) = 6.2
               VOM(2, Nejc, Nnuc) = 54.19 - 22.7*asym
               VOM(3, Nejc, Nnuc) = ( - 0.33) + 0.19*asym
               IF(ener.LE.15.D0)THEN
                  WOMs(2, Nejc, Nnuc) = 4.28 - 12.8*asym
                  WOMs(3, Nejc, Nnuc) = 0.4
                  WOMv(2, Nejc, Nnuc) = 0.0
                  WOMv(3, Nejc, Nnuc) = 0.0
                  OMEmin(Nejc, Nnuc) = 0.0
                  OMEmax(Nejc, Nnuc) = 15.0
               ELSE
                  WOMs(2, Nejc, Nnuc) = 14.0 - 10.4*asym
                  WOMs(3, Nejc, Nnuc) = -0.39
                  WOMv(2, Nejc, Nnuc) = -4.3
                  WOMv(3, Nejc, Nnuc) = 0.38
                  IF(IOMwrite(Nejc, Nnuc).LE.1)IOMwrite(Nejc, Nnuc) = 2
                  OMEmin(Nejc, Nnuc) = 15.0
                  OMEmax(Nejc, Nnuc) = 1000.
               ENDIF
               SFIom(Nejc, Nnuc) = 1.
               GOTO 150
            ENDIF
C
            IF(KTRlom(Nejc, Nnuc).EQ.7)THEN
C
C--------------Konshin 1988 (Rapaport 1979 with energy dep. in rvom) /neutron/
C
               RVOm(1, Nejc, Nnuc) = 1.315 - 0.0167*ener
               RWOm(1, Nejc, Nnuc) = 1.295
               RVSo(1, Nejc, Nnuc) = 1.01
               AVOm(Nejc, Nnuc) = 0.663
               AWOm(Nejc, Nnuc) = 0.59
               AVSo(Nejc, Nnuc) = 0.75
               VSO(2, Nejc, Nnuc) = 6.2
               VOM(2, Nejc, Nnuc) = 54.19 - 22.7*asym
               VOM(3, Nejc, Nnuc) = ( - 0.33) + 0.19*asym
               IF(ener.LE.15.D0)THEN
                  WOMs(2, Nejc, Nnuc) = 4.28 - 12.8*asym
                  WOMs(3, Nejc, Nnuc) = 0.4
                  WOMv(2, Nejc, Nnuc) = 0.0
                  WOMv(3, Nejc, Nnuc) = 0.0
                  OMEmin(Nejc, Nnuc) = 0.0
                  OMEmax(Nejc, Nnuc) = 15.0
               ELSE
                  WOMs(2, Nejc, Nnuc) = 14.0 - 10.4*asym
                  WOMs(3, Nejc, Nnuc) = -0.39
                  WOMv(2, Nejc, Nnuc) = -4.3
                  WOMv(3, Nejc, Nnuc) = 0.38
                  IF(IOMwrite(Nejc, Nnuc).LE.1)IOMwrite(Nejc, Nnuc) = 2
                  OMEmin(Nejc, Nnuc) = 15.0
                  OMEmax(Nejc, Nnuc) = 1000.
               ENDIF
               SFIom(Nejc, Nnuc) = 1.
               GOTO 150
            ENDIF
C
            RETURN
C----------------------------------------------------------
C           6  L I T H I U M   (and 7Li, 7Be)
C----------------------------------------------------------
 100        IF(KTRlom(Nejc, Nnuc).EQ.1)THEN
C
C--------------from Kiev
C
               RVOm(1, Nejc, Nnuc) = 1.3
               RWOm(1, Nejc, Nnuc) = 1.7
               RVSo(1, Nejc, Nnuc) = 0.0
               AVOm(Nejc, Nnuc) = 0.7
               AWOm(Nejc, Nnuc) = 0.9
               AVSo(Nejc, Nnuc) = 0.0
               VOM(2, Nejc, Nnuc) = 220
               WOMv(2, Nejc, Nnuc) = 23.4
               SFIom(Nejc, Nnuc) = 1.
               OMEmin(Nejc, Nnuc) = 0.0
               OMEmax(Nejc, Nnuc) = 1000.
               GOTO 150
            ENDIF
C-----------------------------------------------------------
 120        WRITE(6, 99001)Nejc, Nnuc, KTRlom(Nejc, Nnuc)
99001       FORMAT(1X, /, 
     &           ' INEXISTING SET OF OPTICAL MODEL PARAMETERS REQUESTED'
     &           , /, ' KTRLOM(', I2, ',', I2, ')=', I2, 
     &           ' IS OUT OF RANGE OR EJECTILE NOT INCLUDED.', /, 
     &           ' EXECUTION  S T O P P E D')
            STOP '12'
         ENDIF
C
C--------Considering energy dependence of the OM parameters
C--------WARNING: (Capote 2001)
C--------For some potentials this treatment is approximate, because
C--------we are neglecting energy dependence of the geometrical parameters
C--------calculation of o.m.p. depths
C
 150     VOM(1, Nejc, Nnuc) = VOM(2, Nejc, Nnuc) + VOM(3, Nejc, Nnuc)
     &                        *ener + VOM(4, Nejc, Nnuc)*ener**2 + 
     &                        VOM(5, Nejc, Nnuc)*LOG(ener)
         WOMv(1, Nejc, Nnuc) = WOMv(2, Nejc, Nnuc) + WOMv(3, Nejc, Nnuc)
     &                         *ener + WOMv(4, Nejc, Nnuc)*ener**2 + 
     &                         WOMv(5, Nejc, Nnuc)*LOG(ener)
         WOMs(1, Nejc, Nnuc) = WOMs(2, Nejc, Nnuc) + WOMs(3, Nejc, Nnuc)
     &                         *ener + WOMs(4, Nejc, Nnuc)*ener**2 + 
     &                         WOMs(5, Nejc, Nnuc)*LOG(ener)
         VSO(1, Nejc, Nnuc) = VSO(2, Nejc, Nnuc) + VSO(3, Nejc, Nnuc)
     &                        *ener + VSO(4, Nejc, Nnuc)*ener**2 + 
     &                        VSO(5, Nejc, Nnuc)*LOG(ener)
         WSO(1, Nejc, Nnuc) = WSO(2, Nejc, Nnuc) + WSO(3, Nejc, Nnuc)
     &                        *ener + WSO(4, Nejc, Nnuc)*ener**2 + 
     &                        WSO(5, Nejc, Nnuc)*LOG(ener)
C
C        RIPL_OMP(Nejc) IF
C
      ENDIF
C
C     CHECK & SAVE (300)
C
      WOMs(1, Nejc, Nnuc) = MAX(WOMs(1, Nejc, Nnuc), 0.D0)
      WOMv(1, Nejc, Nnuc) = MAX(WOMv(1, Nejc, Nnuc), 0.D0)
C     Some default protections
C-----set coulomb radius equal to 1.25 if not defined
      IF(RCOul(Nejc, Nnuc).EQ.0.0D0)RCOul(Nejc, Nnuc) = 1.25
C-----set volume imaginary diff. equal to surface imag. diff. if not defined
      IF(AWOmv(Nejc, Nnuc).EQ.0.0D0)AWOmv(Nejc, Nnuc) = AWOm(Nejc, Nnuc)
C-----set volume imaginary radius equal to surface imag. radius if not defined
      IF(RWOmv(1, Nejc, Nnuc).EQ.0.0D0)RWOmv(1, Nejc, Nnuc)
     &   = RWOm(1, Nejc, Nnuc)
C
C-----write O.M. potential parameters to the file OMPAR=>*.omp
C-----only if .omp file did not exist and IOMWRITE is even (to avoid repetion)
C
      iowrite = IOMwrite(Nejc, Nnuc)
      fexist = OMPar_riplf
      IF(CCCalc)THEN
         iowrite = IOMwritecc
         fexist = OMParfcc
      ENDIF
      IF(MOD(iowrite, 2).EQ.0 .AND. .NOT.fexist .AND. RIPl_omp(Nejc))
     &   THEN
         ki = 26
C--------komp = 29
         ipoten = KTRlom(Nejc, Nnuc)
         CALL FINDPOT(ki, ieof, ipoten)
         IF(ieof.GT.0)STOP 'PROBLEM with RIPL OMP library'
C--------Reading IPOTEN catalog number potential parameters
         ianucr = INT(A(Nnuc))
         symbnucr = SYMb(Nnuc)
         iaejcr = INT(AEJc(Nejc))
         symbejcr = SYMbe(Nejc)
         CALL CREATE_OMPAR(ki, Komp, ieof, Nnuc, Nejc, ianucr, symbnucr, 
     &                     iaejcr, symbejcr)
         IF(CCCalc)THEN
C--------increase IOMWRITE so that all these is not written again and again
            IOMwritecc = 1
         ELSE
C--------increase IOMWRITE so that all these is not written again and again
            IOMwrite(Nejc, Nnuc) = IOMwrite(Nejc, Nnuc) + 1
         ENDIF
      ENDIF
      fexist = OMParf
      IF(CCCalc)fexist = OMParfcc
      IF(MOD(iowrite, 2).EQ.0 .AND. .NOT.fexist .AND. 
     &   .NOT.RIPl_omp(Nejc))THEN
         WRITE(Ko, '(I3,''-'',A2,'' +'',I3,''-'',A2,5X,I6,'' +'',I6)')
     &         INT(A(Nnuc)), SYMb(Nnuc), INT(AEJc(Nejc)), SYMbe(Nejc), 
     &         IZA(Nnuc), IZAejc(Nejc)
         WRITE(Ko, '(''Emin, Emax'',5F10.4)')OMEmin(Nejc, Nnuc), 
     &         OMEmax(Nejc, Nnuc), SFIom(Nejc, Nnuc), RNOnl(Nejc, Nnuc), 
     &         RCOul(Nejc, Nnuc)
         WRITE(Ko, '(''real vol  '',8F10.4)')
     &         (VOM(i, Nejc, Nnuc), i = 2, NDVOM), RVOm(1, Nejc, Nnuc), 
     &         AVOm(Nejc, Nnuc)
         WRITE(Ko, '(''imag vol  '',8F10.4)')
     &         (WOMv(i, Nejc, Nnuc), i = 2, NDWOM), RWOmv(1, Nejc, Nnuc)
     &         , AWOmv(Nejc, Nnuc)
         WRITE(Ko, '(''imag surf '',8F10.4)')
     &         (WOMs(i, Nejc, Nnuc), i = 2, NDWOM), RWOm(1, Nejc, Nnuc), 
     &         AWOm(Nejc, Nnuc)
         WRITE(Ko, '(''real SO   '',8F10.4)')
     &         (VSO(i, Nejc, Nnuc), i = 2, NDVSO), RVSo(1, Nejc, Nnuc), 
     &         AVSo(Nejc, Nnuc)
         WRITE(Ko, '(''imag SO   '',8F10.4)')
     &         (WSO(i, Nejc, Nnuc), i = 2, NDVSO), RWSo(1, Nejc, Nnuc), 
     &         AWSo(Nejc, Nnuc)
         IF(CCCalc)THEN
C--------increase IOMWRITE so that all these is not written again and again
            IOMwritecc = 1
         ELSE
C--------increase IOMWRITE so that all these is not written again and again
            IOMwrite(Nejc, Nnuc) = IOMwrite(Nejc, Nnuc) + 1
         ENDIF
      ENDIF
C-----O.M. potential parameters written
      END
C
C
C
      SUBROUTINE CREATE_OMPAR(Ki, Komp, Ieof, Nnucr, Nejcr, Ianucr, 
     &                        Symbnucr, Iaejcr, Symbejcr)
C
C-----Read optical model parameters from the RIPL-II library and create local
C-----omp file
C
      REAL ACO, BANdk, BETa, BETa0, DEF, DEFv, ECOul, EMAx, EMIn, EPOt, 
     &     EX, EXV, GAMma0, POT, RCO, RCOul, RCOul0, SPIn, SPInv, THEtm
      REAL XMUbeta
      INTEGER i, IA, Iaejcr, IAMax, IAMin, Ianucr, IAProj, IDEf, Ieof, 
     &        IMOdel, IPAr, IPArv, IREf, IZ, IZMax, IZMin, IZProj, j, 
     &        JCOul, JRAnge
      INTEGER Ki, Komp, krange, l, LMAx, NCOll, NDIM1, NDIM2, NDIM3, 
     &        NDIM4, NDIM5, NDIM6, NDIM7, Nejcr, NISotop, Nnucr, NPH
      INTEGER NVIb
      CHARACTER*80 comment
      CHARACTER*10 potnam(6)
      CHARACTER*2 Symbnucr, Symbejcr
      PARAMETER(NDIM1 = 10, NDIM2 = 11, NDIM3 = 24, NDIM4 = 30, 
     &          NDIM5 = 10, NDIM6 = 10, NDIM7 = 120)
      CHARACTER*1 AUThor, REFer, SUMmary
      COMMON /LIB   / AUThor(80), REFer(80), SUMmary(320), IREf, EMIn, 
     &                EMAx, IZMin, IZMax, IAMin, IAMax, IMOdel, 
     &                JRAnge(6), EPOt(6, NDIM1), RCO(6, NDIM1, NDIM2), 
     &                ACO(6, NDIM1, NDIM2), POT(6, NDIM1, NDIM3), 
     &                NCOll(NDIM4), NVIb(NDIM4), NISotop, IZ(NDIM4), 
     &                IA(NDIM4), LMAx(NDIM4), BANdk(NDIM4), 
     &                DEF(NDIM4, NDIM5), IDEf(NDIM4), IZProj, IAProj, 
     &                EXV(NDIM7, NDIM4), IPArv(NDIM7, NDIM4), IREl, IDR,
     &                NPH(NDIM7, NDIM4), DEFv(NDIM7, NDIM4), 
     &                THEtm(NDIM7, NDIM4), BETa0(NDIM4), GAMma0(NDIM4), 
     &                XMUbeta(NDIM4), EX(NDIM6, NDIM4), 
     &                SPIn(NDIM6, NDIM4), IPAr(NDIM6, NDIM4), 
     &                SPInv(NDIM7, NDIM4), JCOul, ECOul(NDIM1), 
     &                RCOul(NDIM1), RCOul0(NDIM1), BETa(NDIM1), 
     &                RCOul1(NDIM1), RCOul2(NDIM1)
      DATA potnam/' Real vol.', ' Imag vol.', ' Real surf', 
     &     ' Imag surf', ' Real SO  ', ' Imag SO  '/
99001 FORMAT(80A1)
      Ieof = 0
      READ(Ki, *)IREf
      WRITE(Komp, '(3I5,4x,I3,''-'',A2,'' +'',I3,''-'',A2)')IREf, Nnucr,
     &      Nejcr, Ianucr, Symbnucr, Iaejcr, Symbejcr
C     WRITE (Ko,'(3I5,4x,I3,''-'',A2,'' +'',I3,''-'',A2)') IREf ,
C     &       Nnucr , Nejcr , Ianucr , Symbnucr , Iaejcr , Symbejcr
      DO l = 2, 7
         READ(Ki, '(a80)')comment
         WRITE(Komp, '(a80)')comment
      ENDDO
C     1   read(ki,*,end=999) iref
C     2   read(ki,1) (author(i),i=1,80)
C     3   read(ki,1) (refer(i),i=1,80)
C     4-7 read(ki,1) (summary(i),i=1,320)
C     read(ki,*) emin,emax
      READ(Ki, '(a80)')comment
      WRITE(Komp, '(a80,a10)')comment, ' Emin,Emax'
C     read(ki,*) izmin,izmax
      READ(Ki, '(a80)')comment
      WRITE(Komp, '(a80,a10)')comment, ' Zmin,Zmax'
C     read(ki,*) iamin,iamax
      READ(Ki, '(a80)')comment
      WRITE(Komp, '(a80,a10)')comment, ' Amin,Amax'
C     read(ki,*) imodel,izproj,iaproj,irel,idr
      READ(Ki, '(a80)')comment
      WRITE(Komp, '(a80,a)')comment, ' Mod,Zp,Ap,irel,idr'
      DO i = 1, 6
         READ(Ki, '(a80)')comment
         WRITE(Komp, '(a80,a10,a10)')comment, ' NEranges ', potnam(i)
         BACKSPACE(Ki)
         READ(Ki, *)JRAnge(i)
         IF(JRAnge(i).NE.0)THEN
            krange = IABS(JRAnge(i))
            DO j = 1, krange
C              read(ki,*) epot(i,j)
               READ(Ki, '(a80)')comment
               WRITE(Komp, '(a80,a19)')comment, ' Upper energy limit'
C              read(ki,*) (rco(i,j,n),n=1,ndim2)
               READ(Ki, '(a80)')comment
               WRITE(Komp, '(a80,a10)')comment, ' Radius(E)'
C              Assumed ndim2 must be greater than 7 and less than 13
C              Otherwise delete the following two lines
               READ(Ki, '(a80)')comment
               WRITE(Komp, '(a80,a10)')comment, ' Radius(E)'
C              read(ki,*) (aco(i,j,n),n=1,ndim2)
               READ(Ki, '(a80)')comment
               WRITE(Komp, '(a80,a10)')comment, ' Diffus(E)'
C              Assumed ndim2 must be greater than 7 and less than 13
C              Otherwise delete the following two lines
               READ(Ki, '(a80)')comment
               WRITE(Komp, '(a80,a10)')comment, ' Diffus(E)'
C              read(ki,*) (pot(i,j,n),n=1,ndim3)
               READ(Ki, '(a80)')comment
               WRITE(Komp, '(a80,a10)')comment, ' Vdepth(E)'
               READ(Ki, '(a80)')comment
               WRITE(Komp, '(a80,a10)')comment, ' Vdepth(E)'
               READ(Ki, '(a80)')comment
               WRITE(Komp, '(a80,a10)')comment, ' Vdepth(E)'
C              Assumed ndim3 must be greater than 19 !!!
C              Otherwise delete the following two lines
               READ(Ki, '(a80)')comment
               WRITE(Komp, '(a80,a10)')comment, ' Vdepth(E)'
            ENDDO
         ENDIF
      ENDDO
      READ(Ki, '(a80)')comment
      WRITE(Komp, '(a80,a20)')comment, ' Coul.ranges (jcoul)'
      BACKSPACE(Ki)
      READ(Ki, *)JCOul
      IF(JCOul.GT.0)THEN
         DO j = 1, JCOul
C           read(ki,*) ecoul(j),rcoul(j),rcoul0(j),beta(j)
            READ(Ki, '(a80)')comment
            WRITE(Komp, '(a80,a20)')comment, ' Ecoul,rc,rc0,beta'
         ENDDO
      ENDIF
      IF(IMOdel.GT.0)THEN
C
C--------To avoid repetition of discrete level information
C--------we are not writing here to OMPAR.*
C--------Instead a TARGET_LEV.COLL file is used
C
         NISotop = 0
         WRITE(Komp, '(1x,i4)')NISotop
      ENDIF
      WRITE(Komp, '(A8)')'++++++++'
      END
C
C
C
      SUBROUTINE READ_OMPAR_RIPL(Ko, Ierr, Irelout)
C
C-----Read RIPL optical model parameters from the local OMPAR.RIPL file
C
      REAL ACO, BANdk, BETa, BETa0, DEF, DEFv, ECOul, EMAx, EMIn, EPOt, 
     &     EX, EXV, GAMma0, POT, RCO, RCOul, RCOul0, SPIn, SPInv, THEtm
      REAL XMUbeta
      INTEGER i, IA, IAMax, IAMin, IAProj, IDEf, Ierr, IMOdel, IPAr, 
     &        IPArv, IREf, IZ, IZMax, IZMin, IZProj, j, JCOul, JRAnge, 
     &        k, Ko
      INTEGER krange, LMAx, n, NCOll, NDIM1, NDIM2, NDIM3, NDIM4, NDIM5, 
     &        NDIM6, NDIM7, NISotop, NPH, NVIb, IREl, IDR
C
      PARAMETER(NDIM1 = 10, NDIM2 = 11, NDIM3 = 24, NDIM4 = 30, 
     &          NDIM5 = 10, NDIM6 = 10, NDIM7 = 120)
      CHARACTER*1 AUThor, REFer, SUMmary
      CHARACTER*80 comment
      COMMON /LIB   / AUThor(80), REFer(80), SUMmary(320), IREf, EMIn, 
     &                EMAx, IZMin, IZMax, IAMin, IAMax, IMOdel, 
     &                JRAnge(6), EPOt(6, NDIM1), RCO(6, NDIM1, NDIM2), 
     &                ACO(6, NDIM1, NDIM2), POT(6, NDIM1, NDIM3), 
     &                NCOll(NDIM4), NVIb(NDIM4), NISotop, IZ(NDIM4), 
     &                IA(NDIM4), LMAx(NDIM4), BANdk(NDIM4), 
     &                DEF(NDIM4, NDIM5), IDEf(NDIM4), IZProj, IAProj, 
     &                EXV(NDIM7, NDIM4), IPArv(NDIM7, NDIM4), IREl, IDR, 
     &                NPH(NDIM7, NDIM4), DEFv(NDIM7, NDIM4), 
     &                THEtm(NDIM7, NDIM4), BETa0(NDIM4), GAMma0(NDIM4), 
     &                XMUbeta(NDIM4), EX(NDIM6, NDIM4), 
     &                SPIn(NDIM6, NDIM4), IPAr(NDIM6, NDIM4), 
     &                SPInv(NDIM7, NDIM4), JCOul, ECOul(NDIM1), 
     &                RCOul(NDIM1), RCOul0(NDIM1), BETa(NDIM1), 
     &                RCOul1(NDIM1), RCOul2(NDIM1)
99001 FORMAT(10A8)
C
      Ierr = 0
      READ(Ko, '(I5)', ERR = 200)IREf
      READ(Ko, 99010, ERR = 200)AUThor
      READ(Ko, 99010, ERR = 200)REFer
      READ(Ko, 99010, ERR = 200)SUMmary
      READ(Ko, 99005, ERR = 200)EMIn, EMAx
      READ(Ko, 99004, ERR = 200)IZMin, IZMax
      READ(Ko, 99004, ERR = 200)IAMin, IAMax
      READ(Ko, 99004, ERR = 200)IMOdel, IZProj, IAProj, IREl, IDR
C
      Irelout = IREl
C
      DO i = 1, 6
         READ(Ko, 99004, ERR = 200)JRAnge(i)
         IF(JRAnge(i).NE.0)THEN
            krange = IABS(JRAnge(i))
            DO j = 1, krange
               READ(Ko, 99005, ERR = 200)EPOt(i, j)
C--------------Reading radius
               READ(Ko, 99006)(RCO(i, j, n), n = 1, 7)
               IF(NDIM2.GT.7 .AND. NDIM2.LE.13)THEN
                  READ(Ko, 99007, ERR = 200)(RCO(i, j, n), n = 8, NDIM2)
               ELSE
                  READ(Ko, 99007, ERR = 200)(RCO(i, j, n), n = 8, 13)
               ENDIF
               IF(NDIM2.GT.13)THEN
C                 READ(Ko, 99007, ERR = 200)(RCO(i, j, n), n = 14,NDIM2)
                  STOP '1 IN tl.f, UNCOMMENT READ ABOVE AND REMOVE STOP'
               ENDIF
C--------------Reading diffuss
               READ(Ko, 99006, ERR = 200)(ACO(i, j, n), n = 1, 7)
               IF(NDIM2.GT.7 .AND. NDIM2.LE.13)THEN
                  READ(Ko, 99007, ERR = 200)(ACO(i, j, n), n = 8, NDIM2)
               ELSE
                  READ(Ko, 99007, ERR = 200)(ACO(i, j, n), n = 8, 13)
               ENDIF
               IF(NDIM2.GT.13)THEN
C                 READ(Ko, 99007, ERR = 200)(ACO(i, j, n), n = 14,NDIM2)
                  STOP '2 IN tl.f, UNCOMMENT READ ABOVE AND REMOVE STOP'
               ENDIF
C--------------Reading depths
               READ(Ko, 99006, ERR = 200)(POT(i, j, n), n = 1, 7)
               IF(NDIM3.GT.7 .AND. NDIM3.LE.13)THEN
                  READ(Ko, 99007, ERR = 200)(POT(i, j, n), n = 8, NDIM3)
               ELSE
                  READ(Ko, 99007, ERR = 200)(POT(i, j, n), n = 8, 13)
               ENDIF
               IF(NDIM3.GT.13 .AND. NDIM3.LE.19)THEN
                  READ(Ko, 99007, ERR = 200)
     &                 (POT(i, j, n), n = 14, NDIM3)
               ELSE
                  READ(Ko, 99007, ERR = 200)(POT(i, j, n), n = 14, 19)
               ENDIF
               IF(NDIM3.GT.19)READ(Ko, 99007, ERR = 200)
     &                             (POT(i, j, n), n = 20, NDIM3)
            ENDDO
         ENDIF
      ENDDO
      READ(Ko, 99004, ERR = 200)JCOul
      IF(JCOul.GT.0)THEN
         DO j = 1, JCOul
            READ(Ko, 99005, ERR = 200)ECOul(j), RCOul(j), RCOul0(j), 
     &                                BETa(j)
         ENDDO
      ENDIF
      IF(IMOdel.EQ.1)THEN
         READ(Ko, 99004, ERR = 200)NISotop
         DO n = 1, NISotop
            IF(IDEf(n).LE.8)THEN
               READ(Ko, 99008, ERR = 200)IZ(n), IA(n), NCOll(n), LMAx(n)
     &              , IDEf(n), BANdk(n), (DEF(n, k), k = 2, IDEf(n), 2)
            ELSE
               READ(Ko, 99008, ERR = 200)IZ(n), IA(n), NCOll(n), LMAx(n)
     &              , IDEf(n), BANdk(n), (DEF(n, k), k = 2, 8, 2)
            ENDIF
            IF(IDEf(n).GE.8)READ(Ko, 99002, ERR = 200)
     &                           (DEF(n, k), k = 2, IDEf(n), 2)
99002       FORMAT(30x, 4(1x, e10.3))
            DO k = 1, NCOll(n)
               READ(Ko, 99009, ERR = 200)EX(k, n), SPIn(k, n), 
     &              IPAr(k, n)
            ENDDO
         ENDDO
      ELSEIF(IMOdel.EQ.2)THEN
         READ(Ko, 99004, ERR = 200)NISotop
         DO n = 1, NISotop
            READ(Ko, 99004, ERR = 200)IZ(n), IA(n), NVIb(n)
            DO k = 1, NVIb(n)
               READ(Ko, 99009, ERR = 200)EXV(k, n), SPInv(k, n), 
     &              IPArv(k, n), NPH(k, n), DEFv(k, n), THEtm(k, n)
            ENDDO
         ENDDO
      ELSEIF(IMOdel.EQ.3)THEN
         READ(Ko, 99004, ERR = 200)NISotop
         DO n = 1, NISotop
            READ(Ko, 99003, ERR = 200)IZ(n), IA(n), BETa0(n), GAMma0(n), 
     &                                XMUbeta(n)
99003       FORMAT(2I5, 1p, 3(1x, e11.4))
         ENDDO
      ENDIF
      READ(Ko, '(A80)', END = 100)comment
 100  RETURN
 200  Ierr = 1
99004 FORMAT(10I5)
99005 FORMAT(4F10.3)
99006 FORMAT(f12.5, 1x, 6(e11.4))
99007 FORMAT(13x, 6(e11.4))
99008 FORMAT(5I5, f5.1, 4(1x, e10.3))
99009 FORMAT(f12.8, f7.1, 2I4, 1p, 2(1x, e11.4))
99010 FORMAT(80A1)
      END
C
C
C
C******************* subroutine omin ***************************
      SUBROUTINE OMIN(Ki, Ieof, Irelout)
C
C     routine to read optical model parameters 
C
C     Common blocks and declarations for ominput11.f [July 6, 2001] - RIPL-II
C
C     Parameter statement
C
      PARAMETER(NDIM1 = 10, NDIM2 = 11, NDIM3 = 24, NDIM4 = 30, 
     &          NDIM5 = 10, NDIM6 = 10, NDIM7 = 120)
      CHARACTER*1 AUThor, REFer, SUMmary
      COMMON /LIB   / AUThor(80), REFer(80), SUMmary(320), IREf, EMIn, 
     &                EMAx, IZMin, IZMax, IAMin, IAMax, IMOdel, 
     &                JRAnge(6), EPOt(6, NDIM1), RCO(6, NDIM1, NDIM2), 
     &                ACO(6, NDIM1, NDIM2), POT(6, NDIM1, NDIM3), 
     &                NCOll(NDIM4), NVIb(NDIM4), NISotop, IZ(NDIM4), 
     &                IA(NDIM4), LMAx(NDIM4), BANdk(NDIM4), 
     &                DEF(NDIM4, NDIM5), IDEf(NDIM4), IZProj, IAProj, 
     &                EXV(NDIM7, NDIM4), IPArv(NDIM7, NDIM4), IREl, IDR, 
     &                NPH(NDIM7, NDIM4), DEFv(NDIM7, NDIM4), 
     &                THEtm(NDIM7, NDIM4), BETa0(NDIM4), GAMma0(NDIM4), 
     &                XMUbeta(NDIM4), EX(NDIM6, NDIM4), 
     &                SPIn(NDIM6, NDIM4), IPAr(NDIM6, NDIM4), 
     &                SPInv(NDIM7, NDIM4), JCOul, ECOul(NDIM1), 
     &                RCOul(NDIM1), RCOul0(NDIM1), BETa(NDIM1), 
     &                RCOul1(NDIM1), RCOul2(NDIM1)
      Ieof = 0
      READ(Ki, *, END = 100)IREf
      READ(Ki, 99001)(AUThor(i), i = 1, 80)
      READ(Ki, 99001)(REFer(i), i = 1, 80)
      READ(Ki, 99001)(SUMmary(i), i = 1, 320)
      READ(Ki, *)EMIn, EMAx
      READ(Ki, *)IZMin, IZMax
      READ(Ki, *)IAMin, IAMax
      READ(Ki, *)IMOdel, IZProj, IAProj, IREl, IDR
C
      Irelout = IREl
C
      DO i = 1, 6
         READ(Ki, *)JRAnge(i)
         IF(JRAnge(i).NE.0)THEN
            krange = IABS(JRAnge(i))
            DO j = 1, krange
               READ(Ki, *)EPOt(i, j)
               READ(Ki, *)(RCO(i, j, n), n = 1, NDIM2)
               READ(Ki, *)(ACO(i, j, n), n = 1, NDIM2)
               READ(Ki, *)(POT(i, j, n), n = 1, NDIM3)
            ENDDO
         ENDIF
      ENDDO
      READ(Ki, *)JCOul
      IF(JCOul.GT.0)THEN
         DO j = 1, JCOul
            READ(Ki, *)ECOul(j), RCOul(j), RCOul0(j), BETa(j)
         ENDDO
      ENDIF
      IF(IMOdel.EQ.1)THEN
         READ(Ki, *)NISotop
         DO n = 1, NISotop
            READ(Ki, *)IZ(n), IA(n), NCOll(n), LMAx(n), IDEf(n), 
     &                 BANdk(n), (DEF(n, k), k = 2, IDEf(n), 2)
            DO k = 1, NCOll(n)
               READ(Ki, *)EX(k, n), SPIn(k, n), IPAr(k, n)
            ENDDO
         ENDDO
      ELSEIF(IMOdel.EQ.2)THEN
         READ(Ki, *)NISotop
         DO n = 1, NISotop
            READ(Ki, *)IZ(n), IA(n), NVIb(n)
            DO k = 1, NVIb(n)
               READ(Ki, *)EXV(k, n), SPInv(k, n), IPArv(k, n), NPH(k, n)
     &                    , DEFv(k, n), THEtm(k, n)
            ENDDO
         ENDDO
      ELSEIF(IMOdel.EQ.3)THEN
         READ(Ki, *)NISotop
         DO n = 1, NISotop
            READ(Ki, *)IZ(n), IA(n), BETa0(n), GAMma0(n), XMUbeta(n)
         ENDDO
      ENDIF
      READ(Ki, 99001, END = 100)idum
      RETURN
 100  Ieof = 1
99001 FORMAT(80A1)
      END
C
C
C
      SUBROUTINE SUMPRT(Ko)
C
C     print summary information from RIPL formatted library
C
      CHARACTER*8 ldum, proj
      CHARACTER*40 model
      CHARACTER*20 area
      PARAMETER(NDIM1 = 10, NDIM2 = 11, NDIM3 = 24, NDIM4 = 30, 
     &          NDIM5 = 10, NDIM6 = 10, NDIM7 = 120)
      CHARACTER*1 AUThor, REFer, SUMmary
      COMMON /LIB   / AUThor(80), REFer(80), SUMmary(320), IREf, EMIn, 
     &                EMAx, IZMin, IZMax, IAMin, IAMax, IMOdel, 
     &                JRAnge(6), EPOt(6, NDIM1), RCO(6, NDIM1, NDIM2), 
     &                ACO(6, NDIM1, NDIM2), POT(6, NDIM1, NDIM3), 
     &                NCOll(NDIM4), NVIb(NDIM4), NISotop, IZ(NDIM4), 
     &                IA(NDIM4), LMAx(NDIM4), BANdk(NDIM4), 
     &                DEF(NDIM4, NDIM5), IDEf(NDIM4), IZProj, IAProj, 
     &                EXV(NDIM7, NDIM4), IPArv(NDIM7, NDIM4), IREl, IDR, 
     &                NPH(NDIM7, NDIM4), DEFv(NDIM7, NDIM4), 
     &                THEtm(NDIM7, NDIM4), BETa0(NDIM4), GAMma0(NDIM4), 
     &                XMUbeta(NDIM4), EX(NDIM6, NDIM4), 
     &                SPIn(NDIM6, NDIM4), IPAr(NDIM6, NDIM4), 
     &                SPInv(NDIM7, NDIM4), JCOul, ECOul(NDIM1), 
     &                RCOul(NDIM1), RCOul0(NDIM1), BETa(NDIM1), 
     &                RCOul1(NDIM1), RCOul2(NDIM1)
99001 FORMAT(' IREF=', i5, 2x, a8, ' incident, ', a40)
C
      DATA ldum/'++++++++'/
C
      IF(IZProj.EQ.0 .AND. IAProj.EQ.1)proj = ' Neutron'
      IF(IZProj.EQ.1 .AND. IAProj.EQ.1)proj = '  Proton'
      IF(IZProj.EQ.1 .AND. IAProj.EQ.2)proj = 'Deuteron'
      IF(IZProj.EQ.1 .AND. IAProj.EQ.3)proj = '  Triton'
      IF(IZProj.EQ.2 .AND. IAProj.EQ.3)proj = '    He-3'
      IF(IZProj.EQ.2 .AND. IAProj.EQ.4)proj = '   Alpha'
      IF(IMOdel.EQ.0)model = 'spherical nucleus model'
      IF(IMOdel.EQ.1)model = 'coupled-channels rotational model'
      IF(IMOdel.EQ.2)model = 'vibrational model'
      IF(IMOdel.EQ.3)model = 'non=axial deformed model'
      iarea = MOD(IREf, 1000)
      IF(iarea.LE.99)area = 'United States (LANL)'
      IF(iarea.GE.100 .AND. iarea.LE.199)area = 'United States'
      IF(iarea.GE.200 .AND. iarea.LE.299)area = 'Japan'
      IF(iarea.GE.300 .AND. iarea.LE.399)area = 'Russia'
      IF(iarea.GE.400 .AND. iarea.LE.499)area = 'Europe'
      IF(iarea.GE.500 .AND. iarea.LE.599)area = 'China'
      IF(iarea.GE.600 .AND. iarea.LE.649)area = 'FSU'
      IF(iarea.GE.650 .AND. iarea.LE.699)area = 'India, Pakistan'
      IF(iarea.GE.700 .AND. iarea.LE.799)area = 'Others'
      IF(iarea.GE.800 .AND. iarea.LE.999)area = 'Reserved'
C
      WRITE(Ko, 99002)IREf, proj, model
99002 FORMAT(' IREF=', i5, 2x, a8, ' incident, ', a40)
      IF(IDR.GT.0)WRITE(Ko, '('' Dispersive optical model'')')
      IF(IREl.EQ.0)WRITE(Ko, '('' Non-relativistic kinematics'')')
      IF(IREl.EQ.1)WRITE(Ko, '('' Relativistic kinematics'')')
      IF(IREl.EQ.2)WRITE(Ko, 
     &'('' Relativistic kinematics + Dirac equivalent Schroedinger equat
     &ion.'')')
      iemin = INT(EMIn)
      iemax = INT(EMAx)
      WRITE(Ko, 99003)IZMin, IZMax, IAMin, IAMax, iemin, iemax
99003 FORMAT(' Z-Range=', i3, '-', i2, '  A-Range=', i4, '-', i3, 
     &       '  E-Range=', i4, '-', i3, ' MeV')
C
      DO nn = 1, 80
         n = 80 - nn + 1
         IF(AUThor(n).NE.' ')GOTO 100
      ENDDO
 100  nauth = MIN0(80, n)
      WRITE(Ko, 99004)(AUThor(n), n = 1, nauth)
99004 FORMAT(' Author(s)= ', 60A1, /12x, 20A1)
C
      DO nn = 1, 80
         n = 80 - nn + 1
         IF(REFer(n).NE.' ')GOTO 200
      ENDDO
 200  nrefer = MIN0(80, n)
      WRITE(Ko, 99005)(REFer(n), n = 1, nrefer)
99005 FORMAT(' Reference= ', 60A1, /12x, 20A1)
C
      DO nn = 1, 320
         n = 320 - nn + 1
         IF(SUMmary(n).NE.' ')GOTO 300
      ENDDO
 300  nsum = MIN0(320, n)
      WRITE(Ko, 99006)(SUMmary(n), n = 1, nsum)
99006 FORMAT('   Summary= ', 60A1, /12x, 60A1, /12x, 60A1, /12x, 60A1, 
     &       /12x, 60A1, /12x, 20A1)
C
      WRITE(Ko, 99007)(ldum, i = 1, 9)
99007 FORMAT(10A8)
      END
C
C
C
      SUBROUTINE FINDPOT_OMPAR_RIPL(Ki, Ieof, Ipoten, Nnuc, Nejc)
C
C     Find IPOTEN entry in the RIPL optical model database
C
      INTEGER Ieof, Ipoten, iref, Ki, Nejc, nejcr, Nnuc, nnucr
      CHARACTER*3 ctmp
C
      Ieof = 0
      REWIND(Ki)
      READ(Ki, '(3i5)')iref, nnucr, nejcr
      IF(Ipoten.EQ.iref .AND. Nnuc.EQ.nnucr .AND. nejcr.EQ.Nejc)THEN
         BACKSPACE(Ki)
         RETURN
      ENDIF
 100  READ(Ki, 99001, END = 200)ctmp
99001 FORMAT(A3)
      IF(ctmp.NE.'+++')GOTO 100
      READ(Ki, '(3i5)', END = 200)iref, nnucr, nejcr
      IF(iref.NE.Ipoten .OR. Nnuc.NE.nnucr .OR. nejcr.NE.Nejc)GOTO 100
      BACKSPACE(Ki)
      RETURN
 200  Ieof = 1
      END
C
C
C
      SUBROUTINE TLEVAL(Nejc, Nnuc, Nonzero)
CCC
CCC   ********************************************************************
CCC   *                                                         CLASS:PPU*
CCC   *                         T L E V A L                              *
CCC   *                                                                  *
CCC   * CALCULATES OPTICAL MODEL TRANSMISSION COEFFICIENTS AND FILLS THEM*
CCC   * INTO TL(.,.,.,.) MATRIX.                                         *
CCC   *                                                                  *
CCC   * INPUT:NEJC - EJECTILE INDEX                                      *
CCC   *       NNUC - NUCEUS INDEX (TL ARE CALCULATED FOR THE SCATTERING  *
CCC   *              OF NEJC ON NNUC)                                    *
CCC   *                                                                  *
CCC   * OUTPUT:Nonzero true if non-zero Tl's are returned                *
CCC   *                                                                  *
CCC   * CALLS:     OMTL                                                  *
CCC   *                FACT                                              *
CCC   *                PREANG                                            *
CCC   *                SCAT                                              *
CCC   *                    INTEG                                         *
CCC   *                    RCWFN                                         *
CCC   *                SETPOTS                                           *
CCC   *                    OMPAR                                         *
CCC   *                SHAPEC                                            *
CCC   *                    CGAMMA                                        *
CCC   *                SHAPEL                                            *
CCC   *                    CLEB                                          *
CCC   *                    RACAH                                         *
CCC   *                SPIN0                                             *
CCC   *                SPIN05                                            *
CCC   *                SPIN1                                             *
CCC   *                                                                  *
CCC   * AUTHOR: M.HERMAN                                                 *
CCC   * DATE:   13.FEB.1993                                              *
CCC   * REVISION:1    BY:R.CAPOTE                 ON:  .JAN.1999         *
CCC   * REVISION:2    BY:R.CAPOTE                 ON:  .JAN.2001         *
CCC   ********************************************************************
CCC
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C     COMMON variables
C
      INTEGER MAXl(NDETL)
      DOUBLE PRECISION TTLl(NDETL, 0:NDLW)
      COMMON /TRANSM/ TTLl, MAXl
C
C     Dummy arguments
C
      INTEGER Nejc, Nnuc
C
C     Local variables
C
      DOUBLE PRECISION culbar, ener, stl(NDLW)
      INTEGER i, i1, l, maxlw
      INTEGER INT, MIN0
      LOGICAL tlj_calc, Nonzero, itmp1
C
      tlj_calc = .FALSE.
      Nonzero = .FALSE.
      IF(SEJc(Nejc).GT.1.0D0)THEN
         WRITE(6, 
     &'('' SPIN OF EJECTILE A='',I3,'' AND Z='',I2,'' IS '',    F4.1,/,'
     &' SUCH A LARGE VALUE IS NOT ALLOWED IN TRANSMISSION CEOFFICIENT CA
     &LCULATIONS'',/,'' EXECUTION S T O P P E D '')')INT(AEJc(Nejc)), 
     &INT(ZEJc(Nejc)), SEJc(Nejc)
         STOP '13'
      ENDIF
C
C-----TL trans. coeff. at zero energy must be zero
C
      DO i = 1, NDETL
         LMAxtl(i, Nejc, Nnuc) = 0
         DO l = 1, NDLW
            TL(i, l, Nejc, Nnuc) = 0.D0
         ENDDO
      ENDDO
C-----Coulomb barrier (somewhat decreased) setting lower energy limit
C-----for transmission coefficient calculations
      culbar = 0.8*ZEJc(Nejc)*Z(Nnuc)/(1 + A(Nnuc)**0.6666)
C-----Capote 01/99
      IF(A(Nnuc).EQ.A(0) .AND. Z(Nnuc).EQ.Z(0) .AND. DIRect.EQ.2 .AND. 
     &   AEJc(Nejc).LE.1)THEN
C--------TARGET NUCLEUS (ELASTIC CHANNEL), incident neutron or proton
C--------Transmission coefficient matrix for elastic channel
C--------is calculated for DIRECT = 2 (CCM), ECIS code is used
C--------Preparing INPUT and RUNNING ECIS
C--------(or reading already calculated file)
C
         itmp1 = RIPl_omp(Nejc)
C        KTRlom(Nejc,Nnuc)=KTRompCC
         RIPl_omp(Nejc) = RIPl_ompcc
         IWArn = 0
         CALL TRANSINP(Nejc, Nnuc, NDETL)
         RIPl_omp(Nejc) = itmp1
C
C        IWARN=0 - 'NO Warnings'
C        IWARN=1 - 'A out of the recommended range '
C        IWARN=2 - 'Z out of the recommended range '
C        IWARN=3 - 'Energy requested lower than recommended for this potential'
C        IWARN=4 - 'Energy requested higher than recommended for this potential'
         IF(IWArn.EQ.1 .AND. IOUt.EQ.5)WRITE(6, *)
     &      ' WARNING: OMP not recommended for A=', A(Nnuc)
         IF(IWArn.EQ.2 .AND. IOUt.EQ.5)WRITE(6, *)
     &      ' WARNING: OMP not recommended for Z=', Z(Nnuc)
         IF(IWArn.EQ.3 .AND. IOUt.EQ.5)WRITE(6, *)
     &      ' WARNING: OMP not recommended for low energies in Tl calc'
         IF(IWArn.EQ.4 .AND. IOUt.EQ.5)WRITE(6, *)
     &      ' WARNING: OMP not recommended for high energies in Tl calc'
         IWArn = 0
         tlj_calc = .TRUE.
C--------transfer of the calculated transmission coeff. onto TL matrix
         DO i = 2, NDETL
            LMAxtl(i, Nejc, Nnuc) = MIN0(MAXl(i) + 1, NDLW)
            DO l = 1, LMAxtl(i, Nejc, Nnuc)
               IF(TTLl(i, l - 1).LT.1.D-10)GOTO 50
               TL(i, l, Nejc, Nnuc) = TTLl(i, l - 1)
               Nonzero = .TRUE.
            ENDDO
 50      ENDDO
         RETURN
      ENDIF
      IF(.NOT.tlj_calc)THEN
C-----
C-----do loop over energy
C-----
         IWArn = 0
         maxlw = NDLW
         DO i = 2, NDETL
            ener = ETL(i, Nejc, Nnuc)
            IF(ener.LE.culbar)THEN
               DO i1 = 1, NDLW
                  stl(i1) = 0.0
               ENDDO
            ELSE
C-----------call of the optical model routine
               IF(ener.GT.0.D0)CALL OMTL(Nejc, Nnuc, ener, maxlw, stl, 
     &            0)
               LMAxtl(i, Nejc, Nnuc) = MIN0(maxlw, NDLW)
            ENDIF
C--------transfer of the calculated transmission coeff. onto TL matrix
C--------if Tl<1.0E-10 it is set to 0 to avoid underflow in the calculations
            DO l = 1, MIN0(maxlw, NDLW)
               IF(stl(l).LT.1.0D-10)GOTO 100
               TL(i, l, Nejc, Nnuc) = stl(l)
               Nonzero = .TRUE.
            ENDDO
 100     ENDDO
C        IWARN=0 - 'NO Warnings'
C        IWARN=1 - 'A out of the recommended range '
C        IWARN=2 - 'Z out of the recommended range '
C        IWARN=3 - 'Energy requested lower than recommended for this potential'
C        IWARN=4 - 'Energy requested higher than recommended for this potential'
         IF(IWArn.EQ.1 .AND. IOUt.EQ.5)WRITE(6, *)
     &      'WARNING: OMP not recommended for A=', A(Nnuc)
         IF(IWArn.EQ.2 .AND. IOUt.EQ.5)WRITE(6, *)
     &      'WARNING: OMP not recommended for Z=', Z(Nnuc)
         IF(IWArn.EQ.3 .AND. IOUt.EQ.5)WRITE(6, *)
     &      'WARNING: Energy lower than recommended for the OMP '
         IF(IWArn.EQ.4 .AND. IOUt.EQ.5)WRITE(6, *)
     &      'WARNING: Energy higher than recommended for the OMP '
         IWArn = 0
      ENDIF
      END
C
C
C
      SUBROUTINE TLLOC(Nnuc, Nejc, Eout, Il, Frde)
Ccc
Ccc   ********************************************************************
Ccc   *                                                          class:au*
Ccc   *                         T L L O C                                *
Ccc   *                                                                  *
Ccc   * Locates the first energy in the ETL matrix that lies below       *
Ccc   * EOUT energy  and returns relative position of EOUT between       *
Ccc   * IL and IL+1 elements of ETL. To be used for the transmission     *
Ccc   * coefficients interpolation.                                      *
Ccc   *                                                                  *
Ccc   * input: NNUC - nucleus index                                      *
Ccc   *        NEJC - ejectile index                                     *
Ccc   *        EOUT - requested energy                                   *
Ccc   *                                                                  *
Ccc   * output:IL   - index of the first element of ETL below EOUT       *
Ccc   *        FRDE - relative position of eout between two ETL elements *
Ccc   *               FRDE=(EOUT-ETL(IL))/(ETL(IL+1)-ETL(IL))            *
Ccc   *                                                                  *
Ccc   * calls:none                                                       *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * author: M.Herman                                                 *
Ccc   * date:   01.June.1993                                             *
Ccc   * revision:#    by:name                     on:xx.mon.199x         *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C     Dummy arguments
C
      DOUBLE PRECISION Eout, Frde
      INTEGER Il, Nejc, Nnuc
C
C     Local variables
C
      DOUBLE PRECISION detl
      INTEGER i
      INTEGER INT
C
      IF(Eout.GT.ETL(5, Nejc, Nnuc))THEN
         Il = INT((Eout - ETL(5,Nejc,Nnuc))/DE) + 5
         Frde = (Eout - ETL(Il, Nejc, Nnuc))/DE
      ELSE
         DO i = 4, 1, -1
            IF(Eout.GT.ETL(i, Nejc, Nnuc))THEN
               Il = i
               detl = ETL(i + 1, Nejc, Nnuc) - ETL(i, Nejc, Nnuc)
               Frde = (Eout - ETL(i, Nejc, Nnuc))/detl
               RETURN
            ENDIF
         ENDDO
      ENDIF
      END
C
C
C
C PRE_ECIS package - Creation of input files for ECIS95 and processing of the ECIS output
C                        (Version: January , 2001)
C
C                         Author: Roberto Capote
C
C                  Based on PREGNASH code by Arjan Koning
C                       (Version: May 17, 1996)
C
C       Please send any comments, corrections or improvements to:
C
C                             Roberto Capote
C
C          Email-address: rcapotenoyyahoo.com
C          Email-address: rcapote@cica.es
C
C          INPUT  : NEJC - Ejectile key
C                   NNUC - Residual nucleus key
C                   NEN  - Max.Emiss. energy Index
C
C
C
      SUBROUTINE TRANSINP(Nejc, Nnuc, Nen)
C
C
C
C     --------------------------------------------------------------------
C     | Calculation of   transmission coefficients using ECIS95          |
C     |                for EMPIRE energy grid                            |
C     --------------------------------------------------------------------
C
C
C
      INCLUDE "dimension.h"
      INCLUDE "global.h"
C
C
C     COMMON variables
C
      INTEGER MAXl(NDETL)
      DOUBLE PRECISION TTLl(NDETL, 0:NDLW)
      COMMON /TRANSM/ TTLl, MAXl
C
C     Dummy arguments
C
      INTEGER Nejc, Nen, Nnuc
C
C     Local variables
C
      DOUBLE PRECISION culbar, ener
      LOGICAL fexist
      INTEGER i, ien, ien_beg, l, lmax
      INTEGER INT
      REAL SNGL
      CHARACTER*6 ctmp6
C-----data initialization
      CALL INIT(Nejc, Nnuc)
C
C-----Cleaning transmission coefficient matrix
      DO i = 1, NDETL
         MAXl(i) = 0
         DO l = 0, NDLW
            TTLl(i, l) = 0.D0
         ENDDO
      ENDDO
C
C-----This part prompts for the name of a data file. The INQUIRE
C-----statement then determines whether or not the file exists.
C-----If it does not, the program calculates new transmission coeff.
C
C-----INQUIRE about file's existence:
      WRITE(ctmp6, '(i6.6)')INT(EINl*1000)
      INQUIRE(FILE = ('TARGET-'//ctmp6//'.tl'), EXIST = fexist)
      IF(.NOT.fexist)GOTO 300
      WRITE(6, *)
C     WRITE(6, *)'TRANSM.COEFF.FILE WAS FOUND ',
C     &           ('TARGET-'//ctmp6//'.tl')
C-----Here the old calculated files should be readed
C
      OPEN(45, FILE = ('TARGET-'//ctmp6//'.tl'), FORM = 'UNFORMATTED')
      IF(IOUt.EQ.5)OPEN(46, FILE = 'TARGET-'//ctmp6//'.LST')
 100  READ(45, END = 200)lmax, ien, ener
      IF(IOUt.EQ.5)WRITE(46, '(A5,2I6,E12.6)')'LMAX:', lmax, ien, ener
C
C-----If (energy read from file do not coincide
C-----this nucleus should be recalculated (goto 300)
C
      IF(ABS(ener - ETL(ien,Nejc,Nnuc)).GT.0.0001)THEN
         CLOSE(45, STATUS = 'DELETE')
         IF(IOUt.EQ.5)CLOSE(46, STATUS = 'DELETE')
         WRITE(6, *)'ENERGY MISMATCH: ETL(ien=', ien, '...)=', 
     &              ETL(ien, Nejc, Nnuc), ' REQUESTED ENERGY=', 
     &              SNGL(ener)
         WRITE(6, *)'FILE WITH TRANSM. COEFF. HAS BEEN DELETED'
         GOTO 300
      ENDIF
      ETL(ien, Nejc, Nnuc) = ener
      MAXl(ien) = lmax
      DO l = 0, lmax
         READ(45)TTLl(ien, l)
         IF(IOUt.EQ.5)WRITE(46, *)l, TTLl(ien, l)
      ENDDO
      GOTO 100
C
 200  CLOSE(45)
      IF(IOUt.EQ.5)CLOSE(46)
      WRITE(6, *)'Transmission coefficients read from file: ', 
     &           ('TARGET-'//ctmp6//'.tl')
      RETURN
C-----Coulomb barrier (somewhat decreased) setting lower energy limit
C-----for transsmission coefficient calculations
 300  culbar = 0.8*ZEJc(Nejc)*Z(Nnuc)/(1 + A(Nnuc)**0.6666)
      ien_beg = -1
      DO i = 2, Nen
         ener = ETL(i, Nejc, Nnuc)
         IF(ener.GT.culbar)THEN
            IF(ien_beg.EQ. - 1)ien_beg = i
         ENDIF
      ENDDO
      IF(ien_beg.NE. - 1)THEN
C--------
C--------Running ECIS
C--------
         WRITE(6, *)
         WRITE(6, *)
     &          'RUNNING ECIS for transmission coefficient calculation:'
         IF(IOUt.EQ.5)THEN
            WRITE(6, '(1x,A12,I3,A3,I3,A3,F4.1)')'EJECTILE: A=', 
     &            INT(AEJc(Nejc)), ' Z=', INT(ZEJc(Nejc)), ' S=', 
     &            SEJc(Nejc)
            WRITE(6, '(1x,A12,I3,A3,I3,A3,F4.1,A3,I2)')'RESIDUAL: A=', 
     &            INT(A(Nnuc)), ' Z=', INT(Z(Nnuc)), ' S=', 
     &            SNGL(XJLv(1, Nnuc)), ' P=', INT(LVP(1, Nnuc))
         ENDIF
C--------OPEN Unit=46 for Tl output
         OPEN(UNIT = 46, STATUS = 'unknown', 
     &        FILE = ('TARGET-'//ctmp6//'.tl'), FORM = 'UNFORMATTED')
C
C--------do loop over energy
C
         DO i = ien_beg, Nen
            ener = ETL(i, Nejc, Nnuc)
            IF(ener.GT.culbar)THEN
               IF(DEFormed)THEN
                  CALL ECIS_CCVIBROT(Nejc, Nnuc, ener, .TRUE.)
               ELSE
                  CALL ECIS_CCVIB(Nejc, Nnuc, ener, .TRUE.)
               ENDIF
               CALL ECIS2EMPIRE_TR(Nejc, Nnuc, i)
            ENDIF
         ENDDO
         CLOSE(46)
         WRITE(6, *)' Transm. coeff. written to file:', 
     &              (' TARGET-'//ctmp6//'.tl')
         WRITE(6, *)
     &            ' ==================================================='
      ELSEIF(IOUt.EQ.5)THEN
         WRITE(6, '(1x,A12,I3,A3,I3,A3,F4.1)')'EJECTILE: A=', 
     &         INT(AEJc(Nejc)), ' Z=', INT(ZEJc(Nejc)), ' S=', 
     &         SEJc(Nejc)
         WRITE(6, '(1x,A12,I3,A3,I3,A3,F4.1,A3,I2)')'RESIDUAL: A=', 
     &         INT(A(Nnuc)), ' Z=', INT(Z(Nnuc)), ' S=', 
     &         SNGL(XJLv(1, Nnuc)), ' P=', INT(LVP(1, Nnuc))
         WRITE(6, *)'WARNING: FOR THIS RESIDUAL NUCLEUS'
         WRITE(6, *)'WARNING: AVAILABLE ENERGY IS ALWAYS '
         WRITE(6, *)'WARNING: BELOW COULOMB BARRIER'
         WRITE(6, *)'WARNING: CALCULATIONS NOT NEEDED!'
      ENDIF
      END
C
C
C
      SUBROUTINE ECIS2EMPIRE_TL_TRG(Nejc, Nnuc, Maxlw, Stl)
C
C     Process ECIS output to obtain Stl matrix for the incident channel
C     Reads from unit 45 and writes to unit 6
C
C     INPUT:  Nejc,Nnuc ejectile and residual nucleus index
C     OUTPUT: Maxlw, Stl(1-Maxlw)
C
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C     Dummy arguments
C
      INTEGER Maxlw, Nejc, Nnuc
      DOUBLE PRECISION Stl(NDLW)
C
C     Local variables
C
      DOUBLE PRECISION cte, dtmp, elab, jc, jj, RMU, sabs, selecis, 
     &                 sreac, sreacecis, stotecis, xmas_nejc, xmas_nnuc
      DOUBLE PRECISION DBLE
      INTEGER l, nc, nceq, ncoll, nlev
      CHARACTER*1 parc
      REAL SNGL
      Maxlw = 0
      DO l = 1, NDLW
         Stl(l) = 0.D0
      ENDDO
C     xmas_nejc = ((AEJc(Nejc)*AMUmev + XMAss_ej(Nejc))/(AMUmev + XNExc)
C     &            )
C     xmas_nnuc = ((A(Nnuc)*AMUmev + XMAss(Nnuc))/(AMUmev + XNExc))
      xmas_nejc = (AEJc(Nejc)*AMUmev + XMAss_ej(Nejc))/AMUmev
      xmas_nnuc = (A(Nnuc)*AMUmev + XMAss(Nnuc))/AMUmev
C
      ecms = EIN
      CALL KINEMA(elab, ecms, xmas_nejc, xmas_nnuc, RMU, ak2, 2, RELkin)
C
C     rmu  = reduced mass
C     rmu = xmas_nejc*xmas_nnuc/(xmas_nejc + xmas_nnuc)
C     EIN already in CMS
C     elab = EIN*(1 + xmas_nejc/xmas_nnuc)
C-----ECIS constant
      cte = PI/(W2*RMU*EIN)
C------------------------------------------
C-----| Input of transmission coefficients|
C------------------------------------------
C-----Opening ecis95 output file containing Tlj
      OPEN(UNIT = 45, STATUS = 'old', FILE = 'ecis95.tlj')
C-----JC,ParC is the channel spin and parity
C-----nceq is the number of coupled equations
 100  READ(45, '(1x,f4.1,1x,a1,1x,i4)', END = 200)jc, parc, nceq
C-----Loop over the number of coupled equations
      ncoll = 0
      DO nc = 1, nceq
C--------Reading the coupled level number nlev, the orbital momentum L,
C--------angular momentum j and Transmission coefficient Tlj,c(JC)
C--------(nlev=1 corresponds to the ground state)
         READ(45, '(1x,I2,1x,I3,1x,F5.1,1x,e15.6)', END = 200)nlev, l, 
     &        jj, dtmp
         ncoll = MAX(nlev, ncoll)
C--------Selecting only ground state
         IF(nlev.EQ.1 .AND. dtmp.GT.1.D-15)THEN
C-----------Averaging over particle and target spin, summing over channel spin JC
            Stl(l + 1) = Stl(l + 1) + (2*jc + 1)*dtmp/DBLE(2*l + 1)
     &                   /DBLE(2*SEJc(Nejc) + 1)
     &                   /DBLE(2*XJLv(1, Nnuc) + 1)
            Maxlw = MAX(Maxlw, l)
         ENDIF
      ENDDO
      GOTO 100
 200  CLOSE(45)
C-----Reaction cross section in mb
      sabs = 0.D0
      DO l = 0, Maxlw
         sabs = sabs + Stl(l + 1)*DBLE(2*l + 1)
      ENDDO
      sabs = cte*sabs
      OPEN(UNIT = 45, FILE = 'ecis95.ics', STATUS = 'old', ERR = 300)
      SINl = 0.D0
      DO l = 1, ncoll - 1
         READ(45, *, END = 300)dtmp
         SINl = SINl + dtmp
      ENDDO
 300  CLOSE(45)
      OPEN(UNIT = 45, FILE = 'ecis95.cs', STATUS = 'old', ERR = 400)
      READ(45, *)stotecis
      READ(45, *)sreacecis
      READ(45, *)selecis
 400  CLOSE(45)
      sreac = sabs + SINl
      WRITE(6, *)
      WRITE(6, *)' INCIDENT CHANNEL:'
      WRITE(6, '(A7,I6,F10.3,A10,F10.3,A10)')'  LMAX:', Maxlw, EIN, 
     &      ' MeV (CMS)', elab, ' MeV (LAB)'
      WRITE(6, *)' XS calculated using averaged Tl:'
      WRITE(6, *)' Sabs =', sabs, ' mb '
      WRITE(6, *)' Sinl =', SINl, ' mb (read from ECIS)'
      WRITE(6, *)' Sreac=', SNGL(sreac), ' mb (Sabs + Sinl)', 
     &           ' ECIS Sreac =', SNGL(sreacecis)
      WRITE(6, *)' Total XS =', SNGL(stotecis), ' mb (read from ECIS)'
      WRITE(6, *)' Reaction XS =', SNGL(sreacecis), 
     &           ' mb (read from ECIS)'
      WRITE(6, *)' Shape Elastic XS =', SNGL(selecis), 
     &           ' mb (read from ECIS)'
      WRITE(6, *)
      END
C
C
C
      SUBROUTINE ECIS2EMPIRE_TR(Nejc, Nnuc, J)
C
C     Process ECIS output to obtain TTLl,MaxL matrix for EMPIRE energy grid
C     Reads from units 45, 46 and writes to unit 6
C
C     INPUT:  Nejc  ejectile nucleus index
C             Nnuc  residual nucleus index
C             J     energy index
C     OUTPUT: TTLl(NDETL,0:NDLW),MaxL(NDETL) for all energies from 1 to NDETL
C
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C     COMMON variables
C
      INTEGER MAXl(NDETL)
      DOUBLE PRECISION TTLl(NDETL, 0:NDLW)
      COMMON /TRANSM/ TTLl, MAXl
C
C     Dummy arguments
C
      INTEGER J, Nejc, Nnuc
C
C     Local variables
C
      DOUBLE PRECISION cte, dtmp, e1, elab, jc, jj, RMU, sabs, selecis, 
     &                 sreac, sreacecis, stotecis, xmas_nejc, xmas_nnuc
      DOUBLE PRECISION DBLE
      INTEGER l, lmax, nc, nceq, ncoll, nlev
      CHARACTER*1 parc
      REAL SNGL
C
C     xmas_nejc = ((AEJc(Nejc)*AMUmev + XMAss_ej(Nejc))/(AMUmev + XNExc)
C     &            )
C     xmas_nnuc = ((A(Nnuc)*AMUmev + XMAss(Nnuc))/(AMUmev + XNExc))
      xmas_nejc = (AEJc(Nejc)*AMUmev + XMAss_ej(Nejc))/AMUmev
      xmas_nnuc = (A(Nnuc)*AMUmev + XMAss(Nnuc))/AMUmev
C
      ecms = ETL(J, Nejc, Nnuc)
      CALL KINEMA(elab, ecms, xmas_nejc, xmas_nnuc, RMU, ak2, 2, RELkin)
C
C     rmu  = reduced mass
C     rmu = xmas_nejc*xmas_nnuc/(xmas_nejc + xmas_nnuc)
C-----From CMS to LAB system
C     elab = ETL(J, Nejc, Nnuc)*(1 + xmas_nejc/xmas_nnuc)
C-----ETL is already in CMS
      e1 = ETL(J, Nejc, Nnuc)
C-----ECIS constant
      cte = PI/(W2*RMU*e1)
      MAXl(J) = 0
      lmax = 0
      DO l = 0, NDLW
         TTLl(J, l) = 0.D0
      ENDDO
C-----
C----- Input of transmission coefficients
C-----
C-----Opening ecis95 output file containing Tlj
      OPEN(UNIT = 45, STATUS = 'old', FILE = 'ecis95.tlj')
C-----JC,ParC is the channel spin and parity
C-----nceq is the number of coupled equations
 100  READ(45, '(1x,f4.1,1x,a1,1x,i4)', END = 200)jc, parc, nceq
C-----Loop over the number of coupled equations
      ncoll = 0
      DO nc = 1, nceq
C--------Reading the coupled level number nlev, the orbital momentum L,
C--------angular momentum j and Transmission coefficient Tlj,c(JC)
C--------(nlev=1 corresponds to the ground state)
         READ(45, '(1x,I2,1x,I3,1x,F5.1,1x,e15.6)', END = 200)nlev, l, 
     &        jj, dtmp
         ncoll = MAX(nlev, ncoll)
C--------Selecting only ground state
         IF(nlev.EQ.1 .AND. dtmp.GT.1.D-15)THEN
C-----------Averaging over particle and target spin, summing over channel spin JC
            TTLl(J, l) = TTLl(J, l) + (2*jc + 1)*dtmp/DBLE(2*l + 1)
     &                   /DBLE(2*SEJc(Nejc) + 1)
     &                   /DBLE(2*XJLv(1, Nnuc) + 1)
            lmax = MAX(lmax, l)
         ENDIF
      ENDDO
      GOTO 100
 200  CLOSE(45)
C-----Reaction cross section in mb
      sabs = 0.D0
      DO l = 0, lmax
         sabs = sabs + TTLl(J, l)*DBLE(2*l + 1)
      ENDDO
      sabs = cte*sabs
      OPEN(UNIT = 45, FILE = 'ecis95.ics', STATUS = 'old', ERR = 300)
      SINl = 0.D0
      DO l = 1, ncoll - 1
         READ(45, *, END = 300)dtmp
         SINl = SINl + dtmp
      ENDDO
 300  CLOSE(45)
      OPEN(UNIT = 45, FILE = 'ecis95.cs', STATUS = 'old', ERR = 400)
      READ(45, *)stotecis
      READ(45, *)sreacecis
      READ(45, *)selecis
 400  CLOSE(45)
      sreac = sabs + SINl
      WRITE(6, *)
      WRITE(6, '(A7,2I6,E12.6,A10,E12.6,A10)')'  LMAX:', lmax, J, e1, 
     &      ' MeV (CMS)', elab, ' MeV (LAB)'
      WRITE(6, *)' XS calculated using averaged Tl:'
      WRITE(6, *)' Sabs =', sabs, ' mb '
      WRITE(6, *)' Sinl =', SINl, ' mb (read from ECIS)'
      WRITE(6, *)' Sreac=', SNGL(sreac), ' mb (Sabs + Sinl)', 
     &           ' ECIS Sreac =', SNGL(sreacecis)
C     write(*,'(6(E12.6,1x))') (TTLL(j,L),L=0,LMAX)
      WRITE(6, *)
C-----Writing transmission coefficients
C     WRITE (46,'(A5,2I6,E12.6)') 'LMAX:' , lmax , J , e1
      WRITE(46)lmax, J, e1
      DO l = 0, lmax
C        WRITE (46,*) l , TTLl(J,l)
         WRITE(46)TTLl(J, l)
      ENDDO
      MAXl(J) = lmax
      END
C
C
C
      BLOCKDATA FORTL
C
      INCLUDE "pre_ecis.h"
      DATA(PARname(i), i = 1, 9)/'neutron ', 'proton  ', 'deuteron', 
     &     'triton  ', 'he-3    ', 'alpha   ', 'ION-1   ', 'ION-2   ', 
     &     'ION-3   '/
C
      DATA(NUC(i), i = 1, 103)/'H_', 'HE', 'LI', 'BE', 'B_', 'C_', 'N_', 
     &     'O_', 'F_', 'NE', 'NA', 'MG', 'AL', 'SI', 'P_', 'S_', 'CL', 
     &     'AR', 'K_', 'CA', 'SC', 'TI', 'V ', 'CR', 'MN', 'FE', 'CO', 
     &     'NI', 'CU', 'ZN', 'GA', 'GE', 'AS', 'SE', 'BR', 'KR', 'RB', 
     &     'SR', 'Y_', 'ZR', 'NB', 'MO', 'TC', 'RU', 'RH', 'PD', 'AG', 
     &     'CD', 'IN', 'SN', 'SB', 'TE', 'I_', 'XE', 'CS', 'BA', 'LA', 
     &     'CE', 'PR', 'ND', 'PM', 'SM', 'EU', 'GD', 'TB', 'DY', 'HO', 
     &     'ER', 'TM', 'YB', 'LU', 'HF', 'TA', 'W_', 'RE', 'OS', 'IR', 
     &     'PT', 'AU', 'HG', 'TL', 'PB', 'BI', 'PO', 'AT', 'RN', 'FR', 
     &     'RA', 'AC', 'TH', 'PA', 'U_', 'NP', 'PU', 'AM', 'CM', 'BK', 
     &     'CF', 'ES', 'FM', 'MD', 'NO', 'LR'/
      END
C
C
C
      SUBROUTINE INIT(Nejc, Nnuc)
      INCLUDE "dimension.h"
      INCLUDE "global.h"
      INCLUDE "pre_ecis.h"
C
C     Dummy arguments
C
      INTEGER Nejc, Nnuc
C
C     Local variables
C
      CHARACTER*2 ceject
      CHARACTER*5 ctarget
C
      INTEGER INT, NINT
C
C     +10        +20      +30        +40
C     123456789 123456789 123456789 123456789 123456789
      BECis1 = 'FFFFFFFFFFFFFFFFFFFFFFFFFFFTFFFFFFFFFFFFFFFFFFFFFF'
C     +50     +60        +70      +80        +90
C     123456789 123456789 123456789 123456789 123456789
      BECis2 = 'FFFFFFFFFFFFFTFFTTTFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'
C
C-----*** OPTICAL POTENTIALS ***
C-----Relativistic kinematics (y/n)
      IF(RELkin)THEN
         FLGrel = 'y'
      ELSE
         FLGrel = 'n'
      ENDIF
C-----Making unique name of the output file: outfile
      ctarget(1:2) = NUC(NINT(Z(Nnuc)))
      IF(A(Nnuc).LT.10)THEN
         WRITE(ctarget(3:5), '(3I1)')0, 0, INT(A(Nnuc))
      ELSEIF(A(Nnuc).LT.100)THEN
         WRITE(ctarget(3:5), '(I1,I2)')0, INT(A(Nnuc))
      ELSE
C--------A>99
         WRITE(ctarget(3:5), '(I3)')INT(A(Nnuc))
      ENDIF
      WRITE(ceject(1:2), '(I1,I1)')INT(AEJc(Nejc)), INT(ZEJc(Nejc))
      OUTfile = ctarget//'_'//ceject
CPR---write(6,'(1x,A8)') ' UNIQUE NAME OF THE OUTPUT FILE:',outfile
      END
C
C
C
      SUBROUTINE ECIS_CCVIB(Nejc, Nnuc, El, Ltlj)
C
C     -------------------------------------------------------------
C     |      Create ECIS95 input files for COUPLED CHANNELS       |
C     |        calculation of transmission coefficients in        |
C     |             HARMONIC  VIBRATIONAL   MODEL                 |
C     |               v1.0     S.Masetti 5/99                     |
C     |               v2.0     R.Capote  1/2001                   |
C     |               v3.0     R.Capote  5/2001 (DWBA added)      |
C     -------------------------------------------------------------
C
C
C     ****
C     IP = 1 NEUTRON
C     2 PROTON
C     3 DEUTERON
C     4 TRITON
C     5 HELIUM-3
C     6 ALPHA
C     >6 HEAVY ION
C
      INCLUDE "dimension.h"
      INCLUDE "global.h"
      INCLUDE "pre_ecis.h"
C
C     Dummy arguments
C
      DOUBLE PRECISION El
      LOGICAL Ltlj
      INTEGER Nejc, Nnuc
C
C     Local variables
C
      DOUBLE PRECISION elabe
      CHARACTER*1 ch
      DOUBLE PRECISION eee, elab, rmatch, xmas_nejc, xmas_nnuc, xratio, 
     &                 zerosp
      INTEGER INT, NINT
      INTEGER ip, iterm, j, ldwmax, ncoll, nd_nlvop, njmax, npp
      INTEGER*4 iwin
      INTEGER*4 PIPE
      INTEGER nwrite
C
      IF(AEJc(Nejc).EQ.1.D0 .AND. ZEJc(Nejc).EQ.0.D0)ip = 1
      IF(AEJc(Nejc).EQ.1.D0 .AND. ZEJc(Nejc).EQ.1.D0)ip = 2
      IF(AEJc(Nejc).EQ.2.D0 .AND. ZEJc(Nejc).EQ.1.D0)ip = 3
      IF(AEJc(Nejc).EQ.3.D0 .AND. ZEJc(Nejc).EQ.1.D0)ip = 4
      IF(AEJc(Nejc).EQ.3.D0 .AND. ZEJc(Nejc).EQ.2.D0)ip = 5
      IF(AEJc(Nejc).EQ.4.D0 .AND. ZEJc(Nejc).EQ.2.D0)ip = 6
      IF(AEJc(Nejc).EQ.6.D0 .AND. ZEJc(Nejc).EQ.3.D0)ip = 7
      IF(AEJc(Nejc).EQ.7.D0 .AND. ZEJc(Nejc).EQ.3.D0)ip = 8
      IF(AEJc(Nejc).EQ.7.D0 .AND. ZEJc(Nejc).EQ.4.D0)ip = 9
C-----Data initialization
      angstep = 2.5
      CALL INIT(Nejc, Nnuc)
      ECIs1 = BECis1
C-----Deformation read instead of deformation lengths
C-----for Woods Saxon form factors
      ECIs1(6:6) = 'F'
C-----Coulomb potential deformed
      ECIs1(11:11) = 'T'
C-----Real and imaginary central potential deformed
      ECIs1(12:12) = 'T'
C-----Real SO potential deformed
C     ECIs1(13:13) = 'T'
C-----Imaginary SO potential deformed
C     ECIs1(14:14) = 'T'
C-----Usual coupled equations instead of ECIS scheme is used
C     Spin-orbit potential must be not deformed !!
      ECIs1(21:21) = 'T'
C-----ECIS iteration scheme is used.
C     Shift to coupled equations if convergence is not achieved
C     ECIs1(23:23) = 'T'
      ECIs2 = BECis2
C-----Angular distribution is calculated
      ECIs2(14:14) = 'T'
C-----penetrabilities punched on cards
      ECIs2(13:13) = 'F'
      IF(Ltlj)THEN
         ECIs2(13:13) = 'T'
         ECIs2(14:14) = 'F'
         ECIs2(5:5) = 'T'
C--------Smatrix output
         ECIs2(6:6) = 'F'
      ENDIF
C     DWBA option added
      IF(DIRect.EQ.3)THEN
C-----Iteration scheme used for DWBA
         ECIs1(21:21) = 'F'
C        ECIs1(23:23) = 'F'
C        DWBA
         ECIs2(42:42) = 'T'
      ENDIF
C-----Koning keys supressed
C-----ecis2(20:20)='T'
C-----ecis2(30:30)='T'
C-----ecis2(40:40)='T'
C
C     xmas_nejc = ((AEJc(Nejc)*AMUmev + XMAss_ej(Nejc))/(AMUmev + XNExc)
C     &            )
C     xmas_nnuc = ((A(Nnuc)*AMUmev + XMAss(Nnuc))/(AMUmev + XNExc))
      xmas_nejc = (AEJc(Nejc)*AMUmev + XMAss_ej(Nejc))/AMUmev
      xmas_nnuc = (A(Nnuc)*AMUmev + XMAss(Nnuc))/AMUmev
C
C-----From cms system to Lab (ECIS do inverse convertion)
C     IF(El.LT.0)THEN
C     El = -El
C     elab = El
C     ELSE
C     ecms = El
C     CALL KINEMA(elab, ecms, xmas_nejc, xmas_nnuc, RMU, ak2, 2,
C     &               RELkin)
C     ENDIF
C-----check energy for coupling
      xratio = xmas_nnuc/(xmas_nejc + xmas_nnuc)
C     CALL SETPOTS(Nejc, Nnuc, elab)
C
      IF(El.LT.0.D0)THEN
         El = DABS( - El)
         elab = El
         ikey = -1
      ELSE
         ecms = El
         ikey = +1
      ENDIF
C
C     Transformation of energies from laboratory to center-of-mass if needed
C     is done inside SETPOTS() -> OMPAR()
C
      CALL SETPOTS(Nejc, Nnuc, elab, ecms, xmas_nejc, xmas_nnuc, RMU, 
     &             ak2, ikey)
C-----relativistic kinematics ?
      IF(IRElat(Nejc, Nnuc).GT.0 .OR. FLGrel.EQ.'y')ECIs1(8:8) = 'T'
C
C-----Only for target, find open channels
C     At least ground state is always open !!, RCN 31/03/2001
C     nd_nlvop = 0
      nd_nlvop = 1
      IF(ND_nlv.GT.0)THEN
         DO j = 2, ND_nlv
            IF(DIRect.NE.3 .OR. IPH(j).NE.2)THEN
               eee = El - D_Elv(j)/xratio
C              IF ( El*xratio.GT.D_Elv(j)+0.1 ) nd_nlvop = j
               IF(eee.GT.0.05)nd_nlvop = nd_nlvop + 1
            ENDIF
         ENDDO
      ENDIF
C     iterm = 1
      IF(nd_nlvop.EQ.1)WRITE(6, *)
     &               ' All inelastic channels are closed at this energy'
      ncoll = nd_nlvop
      iterm = 20
C     For DWBA only one iteration is used
      IF(ECIs2(42:42).EQ.'T')iterm = 1
      npp = nd_nlvop
      rmatch = 25.
      zerosp = 0.0
C-----ldwmax=2.4*1.25*AN(i)**0.33333333*0.22*sqrt(parmas(i)*e)
      ldwmax = 2.4*1.25*A(Nnuc)**0.33333333*0.22*SQRT(xmas_nejc*elab)
C-----Maximum number of channel spin
      njmax = MAX(ldwmax, 20)
C-----writing input
      OPEN(UNIT = 1, STATUS = 'unknown', FILE = 'ecVIB.inp')
C-----CARD 1 : Title
      WRITE(1, 
     &      '(f6.2,'' MeV '',a8,'' on '',i3,a2,'': VIBRATIONAL MODEL'')'
     &      )El, PARname(ip), NINT(A(Nnuc)), NUC(NINT(Z(Nnuc)))
C-----CARD 2
      WRITE(1, '(a50)')ECIs1
C-----CARD 3
      WRITE(1, '(a50)')ECIs2
C-----CARD 4
      WRITE(1, '(4i5)')ncoll, njmax, iterm, npp
C-----Matching radius
C-----CARD 5
      WRITE(1, '(10x,f10.5)')rmatch
C-----ground state
C     ch = '-'
C     IF ( LVP(1,Nnuc).GT.0 ) ch = '+'
C-----Important: Instead of using TARGET SPIN (XJLV(1,NNUC)) and PARITY(ch)
C-----A.Koning always used in PREGNASH SPIN=0, ch='+'
C-----It is justified for vibrational model and DWBA calculations
C-----so we are using zero spin here
C-----NOT TRUE for rotational model calculations (see ecis_CCrot)
C-----write(1,'(f5.2,2i2,a1,5f10.5)') zerosp,0,1,'+',EL,
      WRITE(1, '(f5.2,2i2,a1,5f10.5)')zerosp, 0, 1, '+', elab, 
     &                                SEJc(Nejc), xmas_nejc, xmas_nnuc, 
     &                                Z(Nnuc)*ZEJc(Nejc)
C-----0 phonon involved
      WRITE(1, '( )')
C-----discrete levels
      IF(nd_nlvop.GT.1)THEN
         nwrite = 1
         DO j = 2, ND_nlv
            IF(DIRect.NE.3 .OR. IPH(j).NE.2)THEN
               eee = El - D_Elv(j)/xratio
               IF(eee.GE.0.05D0)THEN
C                 DO j = 2 , nd_nlvop
                  ch = '-'
                  IF(D_Lvp(j).GT.0)ch = '+'
                  nwrite = nwrite + 1
                  WRITE(1, '(f5.2,2i2,a1,5f10.5)')D_Xjlv(j), 0, nwrite, 
     &                  ch, D_Elv(j), SEJc(Nejc), xmas_nejc, xmas_nnuc, 
     &                  Z(Nnuc)*ZEJc(Nejc)
                  IF(IPH(j).EQ.1)THEN
                     WRITE(1, '(3i5)')IPH(j), j - 1, 0
                  ELSE
C--------------two   phonon states if exist are formed from the quadrupole
C--------------quadrupole phonon spin is equal to 2+
C                    phonon
                     WRITE(1, '(3i5)')IPH(j), 1, 1
                  ENDIF
               ENDIF
            ENDIF
         ENDDO
C
C--------deformations: phonon description
C
         DO j = 2, ND_nlv
            eee = El - D_Elv(j)/xratio
            IF(eee.GE.0.05D0)THEN
C              DO j = 2 , nd_nlvop
C              only deformation for one phonon states is needed
C              &          INT(D_Xjlv(j)),D_Def(j)
               IF(IPH(j).EQ.1)WRITE(1, '(i5,5x,6f10.5)')INT(D_Xjlv(j)), 
     &                              D_Def(j, 2)
            ENDIF
         ENDDO
      ENDIF
C-----potential parameters
C-----1) groundstate
C     write(1,'(3f10.5)') v,rv,av
      IF(POTe(1).NE.0.)THEN
         WRITE(1, '(3f10.5)')POTe(1), RVOm(1, Nejc, Nnuc), 
     &                       AVOm(Nejc, Nnuc)
      ELSE
         WRITE(1, '(3f10.5)')0., 0., 0.
      ENDIF
C-----write(1,'(3f10.5)') w,rw,aw
      IF(POTe(3).NE.0.)THEN
         WRITE(1, '(3f10.5)')POTe(3), RWOmv(1, Nejc, Nnuc), 
     &                       AWOmv(Nejc, Nnuc)
      ELSE
         WRITE(1, '(3f10.5)')0., 0., 0.
      ENDIF
C-----write(1,'(3f10.5)') vd,rvd,avd
      IF(POTe(7).NE.0.)THEN
         WRITE(1, '(3f10.5)')POTe(7), RWOm(1, Nejc, Nnuc), 
     &                       AWOm(Nejc, Nnuc)
      ELSE
         WRITE(1, '(3f10.5)')0., 0., 0.
      ENDIF
C-----write(1,'(3f10.5)') wd,rwd,awd
      IF(POTe(2).NE.0.)THEN
         WRITE(1, '(3f10.5)')POTe(2), RWOm(1, Nejc, Nnuc), 
     &                       AWOm(Nejc, Nnuc)
         IF(SFIom(Nejc, Nnuc).LT.0.0D0)THEN
            WRITE(*, *)
            WRITE(*, *)' ERROR !!!'
            WRITE(*, *)' ECIS can not be used with Gaussian formfactors'
            WRITE(*, *)' for imaginary surface contribution '
            WRITE(*, *)' Change OMP potential for elastic channel'
            WRITE(6, *)
            WRITE(6, *)' ERROR !!!'
            WRITE(6, *)' ECIS can not be used with Gaussian formfactors'
            WRITE(6, *)' for imaginary surface contribution'
            WRITE(6, *)' Change OMP potential for elastic channel'
            STOP '14'
         ENDIF
      ELSE
         WRITE(1, '(3f10.5)')0., 0., 0.
      ENDIF
C-----write(1,'(3f10.5)') vvso,rrvso,aavso
      IF(POTe(4).NE.0.)THEN
         WRITE(1, '(3f10.5)')POTe(4), RVSo(1, Nejc, Nnuc), 
     &                       AVSo(Nejc, Nnuc)
      ELSE
         WRITE(1, '(3f10.5)')0., 0., 0.
      ENDIF
C-----write(1,'(3f10.5)') wwso,rwso,awso
      IF(WSO(1, Nejc, Nnuc).NE.0.)THEN
         WRITE(1, '(3f10.5)')WSO(1, Nejc, Nnuc), RWSo(1, Nejc, Nnuc), 
     &                       AWSo(Nejc, Nnuc)
      ELSE
         WRITE(1, '(3f10.5)')0., 0., 0.
      ENDIF
C-----write(1,'(3f10.5)') rc,0.,0.
      WRITE(1, '(3f10.5)')RCOul(Nejc, Nnuc), 0., 0.
      WRITE(1, '(3f10.5)')0., 0., 0.
      IF(nd_nlvop.GT.1)THEN
C-----   2) discrete levels
         DO j = 2, ND_nlv
            IF(DIRect.NE.3 .OR. IPH(j).NE.2)THEN
C              DO j = 2 , nd_nlvop
C              EEE=eninc-edis(j)/specm(k0)
C              specm(k)=resmas(k)/(parmas(k)+resmas(k))
               eee = El - D_Elv(j)/xratio
               IF(eee.GE.0.05)THEN
C                 SETPOTS  : subroutine for optical model parameters
C                 From     cms system to Lab
                  ecms = eee
                  ikey = +1
C
C                 Transformation of energies from laboratory to center-of-mass
C                 if needed is done inside SETPOTS() -> OMPAR()
C
                  CALL SETPOTS(Nejc, Nnuc, elabe, ecms, xmas_nejc, 
     &                         xmas_nnuc, RMU, ak2, ikey)
C
C                 elabe = eee*xratio
C                 CALL SETPOTS(Nejc, Nnuc, elabe)
C
C                 Capote   2001
C                 CALL SETPOTS(Nejc, Nnuc, eee)
C
C                 potential parameters
C                 write(1,'(3f10.5)') v,rv,av
                  IF(POTe(1).NE.0.)THEN
                     WRITE(1, '(3f10.5)')POTe(1), RVOm(1, Nejc, Nnuc), 
     &                     AVOm(Nejc, Nnuc)
                  ELSE
                     WRITE(1, '(3f10.5)')0., 0., 0.
                  ENDIF
C                 write(1,'(3f10.5)') w,rw,aw
                  IF(POTe(3).NE.0.)THEN
                     WRITE(1, '(3f10.5)')POTe(3), RWOmv(1, Nejc, Nnuc), 
     &                     AWOmv(Nejc, Nnuc)
                  ELSE
                     WRITE(1, '(3f10.5)')0., 0., 0.
                  ENDIF
C-----------------write(1,'(3f10.5)') vd,rvd,avd
                  IF(POTe(7).NE.0.)THEN
                     WRITE(1, '(3f10.5)')POTe(7), RWOm(1, Nejc, Nnuc), 
     &                     AWOm(Nejc, Nnuc)
                  ELSE
                     WRITE(1, '(3f10.5)')0., 0., 0.
                  ENDIF
C                 write(1,'(3f10.5)') wd,rwd,awd
                  IF(POTe(2).NE.0.)THEN
                     WRITE(1, '(3f10.5)')POTe(2), RWOm(1, Nejc, Nnuc), 
     &                     AWOm(Nejc, Nnuc)
                  ELSE
                     WRITE(1, '(3f10.5)')0., 0., 0.
                  ENDIF
C                 write(1,'(3f10.5)') vvso,rrvso,aavso
                  IF(POTe(4).NE.0.)THEN
                     WRITE(1, '(3f10.5)')POTe(4), RVSo(1, Nejc, Nnuc), 
     &                     AVSo(Nejc, Nnuc)
                  ELSE
                     WRITE(1, '(3f10.5)')0., 0., 0.
                  ENDIF
C                 write(1,'(3f10.5)') wwso,rwso,awso
                  IF(WSO(1, Nejc, Nnuc).NE.0.)THEN
                     WRITE(1, '(3f10.5)')WSO(1, Nejc, Nnuc), 
     &                     RWSo(1, Nejc, Nnuc), AWSo(Nejc, Nnuc)
                  ELSE
                     WRITE(1, '(3f10.5)')0., 0., 0.
                  ENDIF
C                 write(1,'(3f10.5)') rc,0.,0.
                  WRITE(1, '(3f10.5)')RCOul(Nejc, Nnuc), 0., 0.
                  WRITE(1, '(3f10.5)')0., 0., 0.
               ENDIF
            ENDIF
         ENDDO
      ENDIF
C
      WRITE(1, '(3f10.5)')0.D0, angstep, 180.D0
      WRITE(1, '(3hFIN)')
      CLOSE(UNIT = 1)
C
C-----Running ECIS
C
      IF(IOPsys.EQ.0)THEN
         iwin = PIPE('../source/ecis<ecVIB.inp>ECIS_VIB.out#')
      ELSE
         iwin = PIPE('ecis<ecVIB.inp>ECIS_VIB.out#')
      ENDIF
      IF(.NOT.Ltlj)THEN
         IF(DIRect.NE.3)THEN
            CALL WRITEXS(iwin, 'CC vibr. coupling')
         ELSE
            CALL WRITEXS(iwin, 'DWBA')
         ENDIF
      ENDIF
      END
C
C
C
      SUBROUTINE ECIS_CCVIBROT(Nejc, Nnuc, El, Ltlj)
C
C     -------------------------------------------------------------
C     |    Create input files for ECIS95 for COUPLED CHANNELS     |
C     |        calculation of transmission coefficients in        |
C     |             rotational-vibrational model                  |
C     |               v1.0     S.Masetti 5/99                     |
C     |               v2.0     R.Capote  1/2001                   |
C     |               v3.0     R.Capote  5/2001 (DWBA added)      |
C     -------------------------------------------------------------
C
C     ****
C     IP = 1 NEUTRON
C     2 PROTON
C     3 DEUTERON
C     4 TRITON
C     5 HELIUM-3
C     6 ALPHA
C     >6 HEAVY ION
C
      INCLUDE "dimension.h"
      INCLUDE "global.h"
      INCLUDE "pre_ecis.h"
C
C     Dummy arguments
C
      DOUBLE PRECISION El
      LOGICAL Ltlj
      INTEGER Nejc, Nnuc
C
C     Local variables
C
      CHARACTER*1 ch
      DOUBLE PRECISION eee, elab, rmatch, xmas_nejc, xmas_nnuc, xratio
      DOUBLE PRECISION elabe
      INTEGER ip, iterm, j, ldwmax, lev(NDLV), ncoll, nd_nlvop, njmax, 
     &        npho, npp, k
      INTEGER*4 iwin
      INTEGER*4 PIPE
C     INTEGER NINT
      INTEGER jdm, nwrite
C
      IF(AEJc(Nejc).EQ.1.D0 .AND. ZEJc(Nejc).EQ.0.D0)ip = 1
      IF(AEJc(Nejc).EQ.1.D0 .AND. ZEJc(Nejc).EQ.1.D0)ip = 2
      IF(AEJc(Nejc).EQ.2.D0 .AND. ZEJc(Nejc).EQ.1.D0)ip = 3
      IF(AEJc(Nejc).EQ.3.D0 .AND. ZEJc(Nejc).EQ.1.D0)ip = 4
      IF(AEJc(Nejc).EQ.3.D0 .AND. ZEJc(Nejc).EQ.2.D0)ip = 5
      IF(AEJc(Nejc).EQ.4.D0 .AND. ZEJc(Nejc).EQ.2.D0)ip = 6
      IF(AEJc(Nejc).EQ.6.D0 .AND. ZEJc(Nejc).EQ.3.D0)ip = 7
      IF(AEJc(Nejc).EQ.7.D0 .AND. ZEJc(Nejc).EQ.3.D0)ip = 8
      IF(AEJc(Nejc).EQ.7.D0 .AND. ZEJc(Nejc).EQ.4.D0)ip = 9
C     Data initialization
      angstep = 2.5
      CALL INIT(Nejc, Nnuc)
      ECIs1 = BECis1
C-----Rotational model
      ECIs1(1:1) = 'T'
C     Deformation read instead of deformation lengths
C     for Woods Saxon form factors
      ECIs1(6:6) = 'F'
C-----Coulomb potential deformed
      ECIs1(11:11) = 'T'
C-----Real and imaginary central potential deformed
      ECIs1(12:12) = 'T'
C-----Real SO potential deformed
C     ECIs1(13:13) = 'T'
C-----Imaginary SO potential deformed
C     ECIs1(14:14) = 'T'
C-----Usual coupled equations instead of ECIS scheme is used
C     Spin-orbit potential must be not deformed !!
      ECIs1(21:21) = 'T'
C-----ECIS iteration scheme is used.
C     Shift to coupled equations if convergence is not achieved
C     ECIs1(23:23) = 'T'
      ECIs2 = BECis2
C     Angular distribution is calculated
      ECIs2(14:14) = 'T'
C     Penetrabilities punched on cards
      ECIs2(13:13) = 'F'
      IF(Ltlj)THEN
         ECIs2(13:13) = 'T'
         ECIs2(14:14) = 'F'
         ECIs2(5:5) = 'T'
C        Smatrix output
         ECIs2(6:6) = 'F'
      ENDIF
C     DWBA option added
      IF(DIRect.EQ.3)THEN
C-----Iteration scheme used for DWBA
         ECIs1(21:21) = 'F'
C        ECIs1(23:23) = 'F'
C        DWBA
         ECIs2(42:42) = 'T'
      ENDIF
C-----relativistic kinematics ?
      IF(IRElat(Nejc, Nnuc).GT.0 .OR. FLGrel.EQ.'y')ECIs1(8:8) = 'T'
C
C
C     xmas_nejc = ((AEJc(Nejc)*AMUmev + XMAss_ej(Nejc))/(AMUmev + XNExc)
C     &            )
C     xmas_nnuc = ((A(Nnuc)*AMUmev + XMAss(Nnuc))/(AMUmev + XNExc))
      xmas_nejc = (AEJc(Nejc)*AMUmev + XMAss_ej(Nejc))/AMUmev
      xmas_nnuc = (A(Nnuc)*AMUmev + XMAss(Nnuc))/AMUmev
      xratio = xmas_nnuc/(xmas_nejc + xmas_nnuc)
C
C
C-----From cms system to Lab (ECIS do inverse convertion)
C     IF(El.LT.0)THEN
C     El = -El
C     elab = El
C     ELSE
C     ecms = El
C     CALL KINEMA(elab, ecms, xmas_nejc, xmas_nnuc, RMU, ak2, 2,
C     &               RELkin)
C     ENDIF
C-----check energy for coupling
      xratio = xmas_nnuc/(xmas_nejc + xmas_nnuc)
C     CALL SETPOTS(Nejc, Nnuc, elab)
C
      IF(El.LT.0.D0)THEN
         El = DABS( - El)
         elab = El
         ikey = -1
      ELSE
         ecms = El
         ikey = +1
      ENDIF
C
C     Transformation of energies from laboratory to center-of-mass if needed
C     is done inside SETPOTS() -> OMPAR()
C
      CALL SETPOTS(Nejc, Nnuc, elab, ecms, xmas_nejc, xmas_nnuc, RMU, 
     &             ak2, ikey)
C-----relativistic kinematics ?
      IF(IRElat(Nejc, Nnuc).GT.0 .OR. FLGrel.EQ.'y')ECIs1(8:8) = 'T'
C
C-----Only for target, find open channels
C     At least ground state is always open !!, RCN 31/03/2001
C     nd_nlvop = 0
      nd_nlvop = 1
      IF(ND_nlv.GT.0)THEN
         DO j = 2, ND_nlv
            eee = El - D_Elv(j)/xratio
C           IF ( El*xratio.GT.D_Elv(j)+0.1 ) nd_nlvop = j
            IF(eee.GT.0.05)nd_nlvop = nd_nlvop + 1
         ENDDO
      ENDIF
      IF(nd_nlvop.EQ.1)WRITE(6, *)
     &                'All inelastic channels are closed at this energy'
C     IF ( nd_nlvop.EQ.1 ) THEN
C     ecis2(42:42)='T'
C     iterm = 1
C     WRITE (6,*) 'All inelastic channels are closed at this energy'
C     ENDIF
      ncoll = nd_nlvop
      iterm = 20
C     For DWBA only one iteration is used
      IF(ECIs2(42:42).EQ.'T')iterm = 1
      npp = nd_nlvop
      rmatch = 25.
C     zerosp=0.0
C     ldwmax=2.4*1.25*AN(i)**0.33333333*0.22*sqrt(parmas(i)*e)
      ldwmax = 2.4*1.25*A(Nnuc)**0.33333333*0.22*SQRT(xmas_nejc*elab)
C     &         **0.33333333*0.22*SQRT(((AEJc(Nejc)*amumev+XMAss_ej(Nejc)
C     &         )/amumev)*El)
C     Maximum number of channel spin
      njmax = MAX(ldwmax, 20)
C-----Writing input
      OPEN(UNIT = 1, STATUS = 'unknown', FILE = 'ecVIBROT.inp')
C     CARD 1 : Title
      WRITE(1, 
     &'(f6.2,'' MeV '',a8,'' on '',i3,a2,'': VIBR-ROTATIONAL CC     '')'
     &)El, PARname(ip), NINT(A(Nnuc)), NUC(NINT(Z(Nnuc)))
C     CARD 2
      WRITE(1, '(a50)')ECIs1
C     CARD 3
      WRITE(1, '(a50)')ECIs2
C     CARD 4
C-----make sure that all contributions to s-wave scattering are included
      jdm = XJLv(1, Nnuc) + SEJc(Nejc) + 0.6
      WRITE(1, '(4i5,30x,i5)')ncoll, njmax, iterm, npp, jdm
C     Matching radius
C     CARD 5
      WRITE(1, '(10x,f10.5)')rmatch
      ch = '-'
      IF(LVP(1, Nnuc).GT.0)ch = '+'
C
C     Important: Instead of using TARGET SPIN (XJLV(1,NNUC)) and PARITY(ch)
C     A.Koning always used in PREGNASH SPIN=0, ch='+'
C     This is not TRUE for rotational model so we are using the target spin here
C------groundstate
C     write(1,'(f5.2,2i2,a1,5f10.5)') XJLV(1,NNUC),0,1,ch,EL,
      WRITE(1, '(f5.2,2i2,a1,5f10.5)')XJLv(1, Nnuc), 0, 1, ch, elab, 
     &                                SEJc(Nejc), xmas_nejc, xmas_nnuc, 
     &                                Z(Nnuc)*ZEJc(Nejc)
C     &                                 ((AEJc(Nejc)
C     &                                 *amumev+XMAss_ej(Nejc))/amumev) ,
C     &                                 ((A(Nnuc)*amumev+XMAss(Nnuc))
C     &                                 /amumev) , Z(Nnuc)*ZEJc(Nejc)
C
C-----Discrete levels
      IF(nd_nlvop.GT.1)THEN
         npho = 0
         nwrite = 1
         DO j = 2, ND_nlv
            eee = El - D_Elv(j)/xratio
            IF(eee.GE.0.05)THEN
C              DO j = 2 , nd_nlvop
               ch = '-'
               IF(D_Lvp(j).GT.0)ch = '+'
               nwrite = nwrite + 1
C              WRITE (1,'(f5.2,2i2,a1,5f10.5)') D_Xjlv(j) , IPH(j) , j , ch ,
C              Phonon angular momentum is transmitted through IPH array
C              IPH(j)=-1 mans phonon with Lphonon=0
               WRITE(1, '(f5.2,2i2,a1,5f10.5)')D_Xjlv(j), IPH(j), 
     &               nwrite, ch, D_Elv(j), SEJc(Nejc), xmas_nejc, 
     &               xmas_nnuc, Z(Nnuc)*ZEJc(Nejc)
               IF(IPH(j).NE.0 .AND. DIRect.NE.3)THEN
                  npho = npho + 1
                  lev(npho) = j
                  WRITE(1, '(10i5)')1, npho
               ENDIF
            ENDIF
         ENDDO
C-----   Description of phonons
         IF(npho.GT.0)THEN
C
C           In vibrational rotational model the IPH(j) array contains the
C           l orbital angular momentum of the phonon.
C           We are assuming that orbital angular momentum of the phonon is
C           equal INT(J). The k magnetic quantum number of the vibration is
C           assumed to be zero !!!! L_PHO(lev(j))  = IPH(j)
C           L3_pho(lev(j)) = 0
            DO j = 1, npho
C              IPH(lev(j)) , 0 , D_Def(lev(j))
               WRITE(1, '(2i5,6f10.5)')INT(D_Xjlv(lev(j)) + 0.1), 0, 
     &                                 D_Def(lev(j), 2)
            ENDDO
         ENDIF
      ENDIF
C-----Deformation of rotational band (only ground state band is present)
C     IdefCC   = maximum degree of deformation
C     LMaxCC   = maximum L in multipole decomposition
      WRITE(1, '(2i5,f10.5)')IDEfcc, LMAxcc, D_Xjlv(1)
C     WRITE (1,'(6f10.5)') D_Def(1)
      WRITE(1, '(6f10.5)')(D_Def(1, k), k = 2, IDEfcc, 2)
C-----Potential parameters
C-----1) groundstate
C     write(1,'(3f10.5)') v,rv,av
      IF(POTe(1).NE.0.)THEN
         WRITE(1, '(3f10.5)')POTe(1), RVOm(1, Nejc, Nnuc), 
     &                       AVOm(Nejc, Nnuc)
      ELSE
         WRITE(1, '(3f10.5)')0., 0., 0.
      ENDIF
C     write(1,'(3f10.5)') w,rw,aw
      IF(POTe(3).NE.0.)THEN
         WRITE(1, '(3f10.5)')POTe(3), RWOmv(1, Nejc, Nnuc), 
     &                       AWOmv(Nejc, Nnuc)
      ELSE
         WRITE(1, '(3f10.5)')0., 0., 0.
      ENDIF
C-----write(1,'(3f10.5)') vd,rvd,avd
      IF(POTe(7).NE.0.)THEN
         WRITE(1, '(3f10.5)')POTe(7), RWOm(1, Nejc, Nnuc), 
     &                       AWOm(Nejc, Nnuc)
      ELSE
         WRITE(1, '(3f10.5)')0., 0., 0.
      ENDIF
C     write(1,'(3f10.5)') wd,rwd,awd
      IF(POTe(2).NE.0.)THEN
         WRITE(1, '(3f10.5)')POTe(2), RWOm(1, Nejc, Nnuc), 
     &                       AWOm(Nejc, Nnuc)
         IF(SFIom(Nejc, Nnuc).LT.0.0D0)THEN
            WRITE(*, *)
            WRITE(*, *)' ERROR !!!'
            WRITE(*, *)' ECIS can not be used with Gaussian formfactors'
            WRITE(*, *)' for imaginary surface contribution '
            WRITE(*, *)' Change OMP potential for elastic channel'
            WRITE(6, *)
            WRITE(6, *)' ERROR !!!'
            WRITE(6, *)' ECIS can not be used with Gaussian formfactors'
            WRITE(6, *)' for imaginary surface contribution'
            WRITE(6, *)' Change OMP potential for elastic channel'
            STOP '16'
         ENDIF
      ELSE
         WRITE(1, '(3f10.5)')0., 0., 0.
      ENDIF
C     write(1,'(3f10.5)') vvso,rrvso,aavso
      IF(POTe(4).NE.0.)THEN
         WRITE(1, '(3f10.5)')POTe(4), RVSo(1, Nejc, Nnuc), 
     &                       AVSo(Nejc, Nnuc)
      ELSE
         WRITE(1, '(3f10.5)')0., 0., 0.
      ENDIF
C     write(1,'(3f10.5)') wwso,rwso,awso
      IF(WSO(1, Nejc, Nnuc).NE.0.)THEN
         WRITE(1, '(3f10.5)')WSO(1, Nejc, Nnuc), RWSo(1, Nejc, Nnuc), 
     &                       AWSo(Nejc, Nnuc)
      ELSE
         WRITE(1, '(3f10.5)')0., 0., 0.
      ENDIF
C     write(1,'(3f10.5)') rc,0.,0.
      WRITE(1, '(3f10.5)')RCOul(Nejc, Nnuc), 0., 0.
      WRITE(1, '(3f10.5)')0., 0., 0.
      IF(nd_nlvop.GT.1)THEN
C-----   2) discrete levels
         DO j = 2, ND_nlv
C           DO j = 2 , nd_nlvop
C           EEE=eninc-edis(j)/specm(k0)
C           specm(k)=resmas(k)/(parmas(k)+resmas(k))
            eee = El - D_Elv(j)/xratio
            IF(eee.GE.0.05)THEN
C--------SETPOTS : subroutine for optical model parameters
C--------From  cms system to Lab
               ecms = eee
               ikey = +1
C
C              Transformation of energies from laboratory to center-of-mass if
C              needed is done inside SETPOTS() -> OMPAR()
C
               CALL SETPOTS(Nejc, Nnuc, elabe, ecms, xmas_nejc, 
     &                      xmas_nnuc, RMU, ak2, ikey)
C
C              elabe = eee*xratio
C              CALL SETPOTS(Nejc, Nnuc, elabe)
C              CALL SETPOTS(Nejc, Nnuc, eee)
C
C--------Potential parameters
C              write(1,'(3f10.5)') v,rv,av
               IF(POTe(1).NE.0.)THEN
                  WRITE(1, '(3f10.5)')POTe(1), RVOm(1, Nejc, Nnuc), 
     &                                AVOm(Nejc, Nnuc)
               ELSE
                  WRITE(1, '(3f10.5)')0., 0., 0.
               ENDIF
C              write(1,'(3f10.5)') w,rw,aw
               IF(POTe(3).NE.0.)THEN
                  WRITE(1, '(3f10.5)')POTe(3), RWOmv(1, Nejc, Nnuc), 
     &                                AWOmv(Nejc, Nnuc)
               ELSE
                  WRITE(1, '(3f10.5)')0., 0., 0.
               ENDIF
C              write(1,'(3f10.5)') vd,rvd,avd
               IF(POTe(7).NE.0.)THEN
                  WRITE(1, '(3f10.5)')POTe(7), RWOm(1, Nejc, Nnuc), 
     &                                AWOm(Nejc, Nnuc)
               ELSE
                  WRITE(1, '(3f10.5)')0., 0., 0.
               ENDIF
C              write(1,'(3f10.5)') wd,rwd,awd
               IF(POTe(2).NE.0.)THEN
                  WRITE(1, '(3f10.5)')POTe(2), RWOm(1, Nejc, Nnuc), 
     &                                AWOm(Nejc, Nnuc)
               ELSE
                  WRITE(1, '(3f10.5)')0., 0., 0.
               ENDIF
C              write(1,'(3f10.5)') vvso,rrvso,aavso
               IF(POTe(4).NE.0.)THEN
                  WRITE(1, '(3f10.5)')POTe(4), RVSo(1, Nejc, Nnuc), 
     &                                AVSo(Nejc, Nnuc)
               ELSE
                  WRITE(1, '(3f10.5)')0., 0., 0.
               ENDIF
C              write(1,'(3f10.5)') wwso,rwso,awso
               IF(WSO(1, Nejc, Nnuc).NE.0.)THEN
                  WRITE(1, '(3f10.5)')WSO(1, Nejc, Nnuc), 
     &                                RVSo(1, Nejc, Nnuc), 
     &                                AVSo(Nejc, Nnuc)
               ELSE
                  WRITE(1, '(3f10.5)')0., 0., 0.
               ENDIF
C              write(1,'(3f10.5)') rc,0.,0.
               WRITE(1, '(3f10.5)')RCOul(Nejc, Nnuc), 0., 0.
               WRITE(1, '(3f10.5)')0., 0., 0.
            ENDIF
         ENDDO
      ENDIF
C
C-----Angular distribution step
C
      WRITE(1, '(3f10.5)')0.D0, angstep, 180.D0
      WRITE(1, '(3hFIN)')
      CLOSE(UNIT = 1)
C     Running ECIS
      IF(IOPsys.EQ.0)THEN
         IF(npho.GT.0)THEN
            iwin = PIPE('../source/ecis<ecVIBROT.inp>ECIS_VIBROT.out#')
         ELSE
            iwin = PIPE('../source/ecis<ecVIBROT.inp>ECIS_ROT.out#')
         ENDIF
      ELSE
      IF(npho.GT.0)THEN
            iwin = PIPE('ecis<ecVIBROT.inp>ECIS_VIBROT.out#')
         ELSE
            iwin = PIPE('ecis<ecVIBROT.inp>ECIS_ROT.out#')
         ENDIF
      ENDIF
      IF(.NOT.Ltlj)THEN
         IF(DIRect.EQ.3)THEN
            CALL WRITEXS(iwin, 'DWBA')
         ELSEIF(npho.GT.0)THEN
            CALL WRITEXS(iwin, 'CC vib-rot.')
         ELSE
            CALL WRITEXS(iwin, 'CC sym.rot.')
         ENDIF
      ENDIF
      END
C
C
C
      SUBROUTINE WRITEXS(Iwin, Sname)
C
C     Dummy arguments
C
      INTEGER*4 Iwin
      CHARACTER*(*) Sname
C
C     Local variables
C
      LOGICAL fexist
C
      IF(Iwin.EQ.0)THEN
         INQUIRE(FILE = 'ecis95.cs', EXIST = fexist)
C        IF ( fexist ) WRITE (6,*)
C        &                     'Total, reaction and elastic c.s. calculated'
C        &                     , ' with ' , Sname , ' model'
         INQUIRE(FILE = 'ecis95.ics', EXIST = fexist)
         IF(fexist)WRITE(6, *)
     &                  'Inelastic c.s. to collective levels calculated'
     &                  , ' with ', Sname, ' model'
      ELSE
         WRITE(6, *)'SYSTEM PROBLEM RUNNING ECIS in', Sname
         STOP '17'
      ENDIF
      END
C
C
C
C================================================================================
C    ALL routines below are needed only for dispersive optical model potentials
C
      REAL*8 FUNCTION DOM_INT_T1(Av, Ef, Ea, E)
C
C     Integral over E' corresponding to nonlocal additions T1(E'<<0)
C
      IMPLICIT REAL*8(A - H, O - Z)
C
      pi = 4.D0*ATAN(1.D0)
C
      ex = E - Ef
      ea2 = Ea**2
      ea3 = ea2*Ea
      ea4 = ea2*ea2
      eax = ex + Ea
C
      dden1 = 2*pi*ea2*(eax**2 + ea2)
      dnum1 = 0.5D0*pi*ex*(ea3 - ea2*eax) - ea2*ex*(ex + 2*Ea)*LOG(Ea)
     &        + ea2*eax**2*LOG(ABS(eax/Ea)) + ea2*eax**2*LOG(ABS(eax))
     &        - ea4*LOG(Ea)
C
      DOM_INT_T1 = -Av*dnum1/dden1
C
      END
C
      REAL*8 FUNCTION DOM_INT_T2(Ef, Ea, E)
C
C     Integral over E' corresponding to nonlocal additions T2(E'>>0)
C
      IMPLICIT REAL*8(A - H, O - Z)
C
      pi = 4.D0*ATAN(1.D0)
      el = Ef + Ea
      alpha = 1.65D0
C
      r1 = 1.5*SQRT(el)*LOG(ABS((el-E)/Ea))
C
      IF(E.EQ.0.D0)THEN
         r2 = 0.5*el**1.5D0*(1.D0/el - LOG(ABS(el/Ea))/Ef)
      ELSE
         r2 = 0.5*el**1.5D0/(E*Ef)
     &        *(Ef*LOG(ABS(el/(el-E))) - E*LOG(ABS(el/Ea)))
      ENDIF
C
      r3 = 2*SQRT(ABS(Ef))*(0.5D0*pi - ATAN(SQRT(el/ABS(Ef))))
C
      IF(E.GE.0.D0)THEN
         r4 = SQRT(E)*LOG(ABS((SQRT(el)+SQRT(E))/(SQRT(el)-SQRT(E))))
      ELSE
         r4 = -2.D0*SQRT(ABS(E))*(0.5D0*pi - ATAN(SQRT(ABS(el/E))))
      ENDIF
      DOM_INT_T2 = alpha/pi*(r1 + r2 + r3 + r4)
      END
      REAL*8 FUNCTION DELTA_WV(WV, Y)
      REAL*8 E, EF, A, B, EP, Y, WV, WDE, WVE
      COMMON /ENERGY/ E, EF, EP
      COMMON /WENERG/ WDE, WVE
      COMMON /PDATAV/ A, B
      DELTA_WV = (WV(A, B, EP, Y) - WVE)/((Y - EF)**2 - (E - EF)**2)
      END
C
      REAL*8 FUNCTION WV4(A, B, Ep, E)
      REAL*8 A, B, Ep, E
      WV4 = 0.D0
      IF(E.LE.Ep)RETURN
      ee = (E - Ep)**4
      WV4 = A*ee/(ee + B**4)
      END
C
      REAL*8 FUNCTION WV2(A, B, Ep, E)
      REAL*8 A, B, Ep, E
      WV2 = 0.D0
      IF(E.LE.Ep)RETURN
      ee = (E - Ep)**2
      WV2 = A*ee/(ee + B**2)
      END
C
      REAL*8 FUNCTION DELTA_WD(WD, Y)
      REAL*8 E, EF, A, B, C, EP, Y, WD, WDE, WVE
      COMMON /ENERGY/ E, EF, EP
      COMMON /WENERG/ WDE, WVE
      COMMON /PDATAS/ A, B, C
      DELTA_WD = (WD(A, B, C, EP, Y) - WDE)/((Y - EF)**2 - (E - EF)**2)
C     DELTA_WD=(WD(A,B,C,Ep,y) -
C     >          WD(A,B,C,Ep,E))
C     >          /((y-Ef)**2-(E-Ef)**2)
      END
C
      REAL*8 FUNCTION WD4(A, B, C, Ep, E)
      REAL*8 A, B, C, Ep, E, ee, arg
      WD4 = 0.D0
      IF(E.LT.Ep)RETURN
      arg = C*(E - Ep)
      IF(arg.GT.15)RETURN
      ee = (E - Ep)**4
      WD4 = A*ee/(ee + B**4)*EXP( - arg)
      END
C
      REAL*8 FUNCTION WD2(A, B, C, Ep, E)
      REAL*8 A, B, C, Ep, E, ee, arg
      WD2 = 0.D0
      IF(E.LT.Ep)RETURN
      arg = C*(E - Ep)
      IF(arg.GT.15)RETURN
      ee = (E - Ep)**2
      WD2 = A*ee/(ee + B**2)*EXP( - arg)
      END
C
      REAL*8 FUNCTION DOM_INT(DELTAF, F, Ef, Eint, Ecut, E, We_cte)
C
C     DOM integral (20 points Gauss-Legendre)
C
C     Divided in two intervals for higher accuracy
C     The first interval corresponds to peak of the integrand
C
      DOUBLE PRECISION Eint, Ef, Ecut, We_cte, E
      DOUBLE PRECISION F, wg, xg, www, xxx, DELTAF
      DOUBLE PRECISION absc1, centr1, hlgth1, resg1
      DOUBLE PRECISION absc2, centr2, hlgth2, resg2
      INTEGER j
      EXTERNAL F
      DIMENSION xg(10), wg(10)
C
C     THE ABSCISSAE AND WEIGHTS ARE GIVEN FOR THE INTERVAL (-1,1).
C     BECAUSE OF SYMMETRY ONLY THE POSITIVE ABSCISSAE AND THEIR
C     CORRESPONDING WEIGHTS ARE GIVEN.
C
C     XG - ABSCISSAE OF THE 41-POINT GAUSS-KRONROD RULE
C     WG - WEIGHTS OF THE 20-POINT GAUSS RULE
C
C GAUSS QUADRATURE WEIGHTS AND KRONROD QUADRATURE ABSCISSAE AND WEIGHTS
C AS EVALUATED WITH 80 DECIMAL DIGIT ARITHMETIC BY L. W. FULLERTON,
C BELL LABS, NOV. 1981.
C
      DATA wg(1)/0.017614007139152118311861962351853D0/
      DATA wg(2)/0.040601429800386941331039952274932D0/
      DATA wg(3)/0.062672048334109063569506535187042D0/
      DATA wg(4)/0.083276741576704748724758143222046D0/
      DATA wg(5)/0.101930119817240435036750135480350D0/
      DATA wg(6)/0.118194531961518417312377377711382D0/
      DATA wg(7)/0.131688638449176626898494499748163D0/
      DATA wg(8)/0.142096109318382051329298325067165D0/
      DATA wg(9)/0.149172986472603746787828737001969D0/
      DATA wg(10)/0.152753387130725850698084331955098D0/
C
      DATA xg(1)/0.993128599185094924786122388471320D0/
      DATA xg(2)/0.963971927277913791267666131197277D0/
      DATA xg(3)/0.912234428251325905867752441203298D0/
      DATA xg(4)/0.839116971822218823394529061701521D0/
      DATA xg(5)/0.746331906460150792614305070355642D0/
      DATA xg(6)/0.636053680726515025452836696226286D0/
      DATA xg(7)/0.510867001950827098004364050955251D0/
      DATA xg(8)/0.373706088715419560672548177024927D0/
      DATA xg(9)/0.227785851141645078080496195368575D0/
      DATA xg(10)/0.076526521133497333754640409398838D0/
C
      centr1 = 0.5D+00*(Ef + Eint)
      hlgth1 = 0.5D+00*(Eint - Ef)
      centr2 = 0.5D+00*(Ecut + Eint)
      hlgth2 = 0.5D+00*(Ecut - Eint)
C
C     COMPUTE THE 20-POINT GAUSS-KRONROD APPROXIMATION
C     TO THE INTEGRAL in TWO INTERVALS (Ef - Eint, Eint - Ecut)
C
      resg1 = 0.0D+00
      resg2 = 0.0D+00
      DO j = 1, 10
         xxx = xg(j)
         www = wg(j)
         absc1 = hlgth1*xxx
         resg1 = resg1 + 
     &           www*(DELTAF(F, centr1 - absc1) + DELTAF(F, centr1 + 
     &           absc1))
         absc2 = hlgth2*xxx
         resg2 = resg2 + 
     &           www*(DELTAF(F, centr2 - absc2) + DELTAF(F, centr2 + 
     &           absc2))
      ENDDO
C
      corr = 0.5D0*We_cte/(E - Ef)*LOG((Ecut - (E-Ef))/(Ecut + (E-Ef)))
      DOM_INT = (resg1*hlgth1 + resg2*hlgth2 + corr)*(E - Ef)
     &          /(4.D0*ATAN(1.D0))
      END
C
      DOUBLE PRECISION FUNCTION SEXP(Farg)
      DOUBLE PRECISION Farg
      SEXP = 0.D0
      IF(Farg.LT. - 15.D0)RETURN
      SEXP = DEXP(DBLE(Farg))
      END
