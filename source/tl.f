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
C     It is S because it must be compatible with global.h declarations
C     so some variables must be renamed
      INCLUDE 'ripl2empireS.h'
      dimension rlib(6),alib(6),vlib(6)
      INTEGER iainp, izinp, k
      INTEGER IWArn, ncalc, n
      LOGICAL coll_defined
      CHARACTER*80 ch_iuf
      CHARACTER*132 ctmp
      INTEGER*4 iwin
      INTEGER*4 PIPE
C
C-----For dispersive optical model potentials
C
      COMMON /LOCAL / MODelcc
C
      DATA coll_defined/.FALSE./
      SAVE coll_defined
      iainp = A(Nnuc)
      izinp = Z(Nnuc)
C********************************************************************
C     SETTING COLLECTIVE LEVELS for DIRECT CALCULATIONS
C
      MODelcc = 0
C     RCN, 09/2004
      IF(DIRECT.EQ.0) IMOdel = 0
C
C-----IF ( IMOdel.EQ.0 ) model = 'spherical nucleus model'
C-----IF ( IMOdel.EQ.1 ) model = 'coupled-channels rotational model'
C-----IF ( IMOdel.EQ.2 ) model = 'vibrational model'
C-----IF ( IMOdel.EQ.3 ) model = 'non-axial deformed model'
      IF(IMOdel.EQ.3 .AND. FIRst_ein)THEN
         WRITE(6, *)'WARNING: Non-axial deformed model not implemented'
         IWArn = 5
         GOTO 200
      ENDIF
C
      IF(IMOdel.EQ.1 .OR. IMOdel.EQ.2)THEN
C--------Imodel not used for non-inelastic channels
         IF(iainp.NE.A(0) .OR. izinp.NE.Z(0) .OR. AEJc(Nejc).NE.AEJc(0)
     &      .OR. ZEJc(Nejc).NE.ZEJc(0))GOTO 200
      ENDIF
      MODelcc = IMOdel
      IF (ND_nlv.GT.0 .and. NISotop.EQ.0) coll_defined=.TRUE.
      IF(IMOdel.EQ.1 .AND. (.NOT.coll_defined))THEN
C
C--------model = 'coupled-channels rotational model'
C
         coll_defined = .TRUE.
         IF(NISotop.EQ.0 .AND. FIRst_ein)THEN
          WRITE(6, *)'WARNING: None of the requested isotopes is '
          WRITE(6, *)'WARNING: included in the selected RIPL potential.'
          WRITE(6, *)'WARNING: Default collective levels will be used'
          GOTO 200
         ENDIF
         ncalc = 0
         DO n = 1, NISotop
          IF(iainp.EQ.IA(n) .AND. izinp.EQ.IZ(n))THEN
               ncalc = n
               IF(IDEf(n).GT.2*LMAx(n) .OR. NCOll(n).GT.NDCOLLEV)THEN
                  WRITE(6, *)'WARNING: OMP collective levels are wrong'
                  WRITE(6, *)'WARNING: Too many levels or deformations'
                  WRITE(6, *)'WARNING: Default collective levels will be
     & used'
                  IWArn = 6
                  GOTO 200
               ENDIF
          ENDIF
         ENDDO
         IF(ncalc.EQ.0)THEN
          WRITE(6, *)'WARNING: RIPL OMP collective levels are not used'
          WRITE(6, *)'WARNING: Default collective levels will be used'
          GOTO 200
         ENDIF
         IF(NCOll(ncalc).EQ.0)THEN
          WRITE(6, *)'WARNING: RIPL OMP collective levels are not used'
          WRITE(6, *)'WARNING: Default collective levels will be used'
          GOTO 200
         ENDIF
C--------Setting EMPIRE global variables
C        ND_nlv = NCOll(ncalc)
         NLD_cc = 0
         DO k = 1, ND_nlv
          if(icollev(k).LT.50) NLD_cc = NLD_cc +1
         ENDDO
         WRITE(6, *)
         WRITE(6, *)
         WRITE(6, *)'Deformation of the gsb adopted from CC RIPL OMP'
         LMAxcc = LMAx(ncalc)
         IDEfcc = IDEf(ncalc)
         DO k = 2, IDEfcc, 2
            D_Def(1, k) = DDEf(ncalc, k)
         ENDDO
         IF(NLD_cc.NE.NCOll(ncalc)) THEN
          WRITE(6, *)
     >    'WARNING: Default number of coupled levels: ',NLD_cc
          WRITE(6, *)
     >    'WARNING: is not equal ',NCOll(ncalc),' (used in CC RIPL OMP)'
         ENDIF
C
C        Joining TARGET_COLL.DAT and TARGET_COLL_RIPL.DAT files
C
         OPEN(32, FILE = 'TARGET_COLL_RIPL.DAT')
         OPEN(134, FILE = 'TARGET_COLL.DAT')
         OPEN(133, FILE = 'COLL.DAT')
         WRITE(6, *)
     &       'Collective levels from RIPL CC OMP, symm.rotational model'
         WRITE(32, *)
     &       'Collective levels from RIPL CC OMP, symm.rotational model'
         READ(134,'(A80)') ch_iuf    ! FIRST LINE
         WRITE(133,'(A80)') ch_iuf
         WRITE(6, *)'Dyn.deformations are not used in symm.rot.model'
         WRITE(32, *)'Dyn.deformations are not used in symm.rot.model'
         READ(134,'(A80)') ch_iuf        ! 2ND LINE
         WRITE(133,'(A80)') ch_iuf
         WRITE(6, '(1x,i3,1x,i3,a35)')izinp, iainp,
     &                                 ' nucleus is treated as deformed'
         WRITE(32, '(1x,i3,1x,i3,a35)')izinp, iainp,
     &                                 ' nucleus is treated as deformed'
         READ(134,'(A80)') ch_iuf        ! 3ER LINE
         WRITE(133,'(A80)') ch_iuf
         WRITE(6, *)
         WRITE(32, *)
         READ(134,'(A80)') ch_iuf        ! EMPTY LINE
         WRITE(133,'(A80)') ch_iuf
         DEFORMED=.TRUE.
         DO n = 1, NISotop
            IF(iainp.EQ.IA(n) .AND. izinp.EQ.IZ(n))THEN
               WRITE(6, *)
     &                 '   Ncoll  Lmax IDef  Kgs  (Def(1,j),j=2,IDef,2)'
               WRITE(32, *)
     &                 '   Ncoll  Lmax IDef  Kgs  (Def(1,j),j=2,IDef,2)'
               READ(134,'(A80)') ch_iuf
               WRITE(133, *)
     &                 '   Ncoll  Lmax IDef  Kgs  (Def(1,j),j=2,IDef,2)'
               WRITE(6, '(3x,3I5,1x,F5.1,1x,6(e10.3,1x))')NCOll(n),
     &               LMAx(n), IDEf(n), BANdk(n),
     &               (DDEf(n, k), k = 2, IDEf(n), 2)
               WRITE(32, '(3x,3I5,1x,F5.1,1x,6(e10.3,1x))')NCOll(n),
     &               LMAx(n), IDEf(n), BANdk(n),
     &               (DDEf(n, k), k = 2, IDEf(n), 2)
               READ(134,'(A80)') ch_iuf
               WRITE(133, '(3x,3I5,1x,F5.1,1x,6(e10.3,1x))') ND_nlv,
     &               LMAx(n), IDEf(n), BANdk(n),
     &               (DDEf(n, k), k = 2, IDEf(n), 2)
               WRITE(6, *)
               WRITE(32, *)
               READ(134,'(A80)') ch_iuf
               WRITE(133, *)
               WRITE(6, *) ' N   E[MeV]  J   pi Nph L  K  Dyn.Def.'
               WRITE(32, *)' N   E[MeV]  J   pi Nph L  K  Dyn.Def.'
               READ(134,'(A80)') ch_iuf
               WRITE(133, *)' N   E[MeV]  J   pi Nph L  K  Dyn.Def.'
            ENDIF
         ENDDO
   10    READ(134,'(A80)',END=30) ch_iuf
         WRITE(133,'(A80)') ch_iuf
         GOTO 10
   30    CLOSE(133)
         CLOSE(134,STATUS='DELETE')
C        CLOSE(134)
         IF(IOPsys.EQ.0)THEN
            ctmp = 'mv COLL.DAT TARGET_COLL.DAT'
            iwin = PIPE(ctmp)
         ELSE
            iwin = PIPE('move COLL.DAT TARGET_COLL.DAT')
         ENDIF
C
C        JOIN finished: TARGET_COLL.DAT and TARGET_COLL_RIPL.DAT files
C
         DO k = 1, NCOll(ncalc)
C-----------The deformation for excited levels is not used in the pure
C-----------symm.rotational model but could be used for vibrational
C-----------rotational model so we are setting it to 0.01
            WRITE(32,
     &            '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3)')
     &            k, EEX(k, ncalc), SPIn(k, ncalc),
     &                float(IPAr(k, ncalc)), 0 , 0,  0, 0.01
            WRITE(6,
     &            '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3)')
     &            k, EEX(k, ncalc), SPIn(k, ncalc),
     &                float(IPAr(k, ncalc)), 0 , 0,  0, 0.01
         ENDDO
         CLOSE(32)
         WRITE(6, *)
         WRITE(6, *)
      ENDIF
      IF(IMOdel.EQ.2 .AND. (.NOT.coll_defined))THEN
C
C--------model = 'vibrational model'
C
         coll_defined = .TRUE.
         IF(NISotop.EQ.0)THEN
            WRITE(6, *)'WARNING: None of the requested isotopes is '
            WRITE(6, *)'WARNING: Included in the selected potential.'
            WRITE(6, *)'WARNING: File with RIPL discrete levels can not'
            WRITE(6, *)'WARNING: be created.                    '
            WRITE(6, *)'WARNING: Default collective levels will be used'
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
                  WRITE(6, *)'RIPL OMP collective levels are wrong'
                  WRITE(6, *)'Too many levels'
                  WRITE(6, *)'Default collective levels will be used'
                  IWArn = 6
                  GOTO 200
               ENDIF
            ENDIF
         ENDDO
         IF(ncalc.EQ.0)THEN
            WRITE(6, *)'RIPL OMP collective levels are not used'
            WRITE(6, *)'Default collective levels will be used'
            GOTO 200
         ENDIF
         IF(NCOll(ncalc).EQ.0)THEN
            WRITE(6, *)'RIPL OMP collective levels are not used'
            WRITE(6, *)'Default collective levels will be used'
            GOTO 200
         ENDIF
         NLD_cc = 0
         DO k = 1, ND_nlv
          if(icollev(k).LT.50) NLD_cc = NLD_cc +1
         ENDDO
         IF(NLD_cc.NE.NVIb(ncalc)) THEN
           WRITE(6, *)
     >     'WARNING: Default number of coupled levels: ',NLD_cc
           WRITE(6, *)
     >     'WARNING: is not equal ',NVIb(ncalc),' (used in CC RIPL OMP)'
         ENDIF
         WRITE(6, *)
         WRITE(6, *)
C
C        Joining TARGET_COLL.DAT and TARGET_COLL_RIPL.DAT files
C
         OPEN(32, FILE = 'TARGET_COLL_RIPL.DAT')
         OPEN(134, FILE = 'TARGET_COLL.DAT')
         OPEN(133, FILE = 'COLL.DAT')
         WRITE(6, *)
     &           'Collective levels from RIPL CC OMP, vibrational model'
         WRITE(32, *)
     &           'Collective levels from RIPL CC OMP, vibrational model'
         READ(134,'(A80)') ch_iuf    ! FIRST LINE
         WRITE(133,'(A80)') ch_iuf
         WRITE(6, *)'Dynamical deformations should be adjusted'
         WRITE(32, *)'Dynamical deformations should be adjusted'
         READ(134,'(A80)') ch_iuf        ! 2ND LINE
         WRITE(133,'(A80)') 'Dynamical deformations should be adjusted'
         WRITE(6, '(1x,i3,1x,i3,a35)')izinp, iainp,
     &                                ' nucleus is treated as spherical'
         WRITE(32, '(1x,i3,1x,i3,a35)')izinp, iainp,
     &                                ' nucleus is treated as spherical'
         READ(134,'(A80)') ch_iuf        ! 3ER LINE
         WRITE(133,'(A80)') ch_iuf
         WRITE(6, *)
         WRITE(32, *)
         READ(134,'(A80)') ch_iuf        ! EMPTY LINE
         WRITE(133,'(A80)') ch_iuf
         DEFORMED=.FALSE.
         DO n = 1, NISotop
            IF(iainp.EQ.IA(n) .AND. izinp.EQ.IZ(n))THEN
               WRITE(6, *)   '   Ncoll'
               WRITE(32, *)  '   Ncoll'
               READ(134,'(A80)') ch_iuf
               WRITE(133, *) '   Ncoll'
               WRITE(6, '(3x,3I5,1x,F5.1,1x,6(e10.3,1x))')NCOll(n)
               WRITE(32, '(3x,3I5,1x,F5.1,1x,6(e10.3,1x))')NCOll(n)
               READ(134,'(A80)') ch_iuf
               WRITE(133,'(A80)')  ND_nlv
               WRITE(6, *)
               WRITE(32, *)
               READ(134,'(A80)') ch_iuf
               WRITE(133, *)
               WRITE(6, *) ' N   E[MeV]  J   pi Nph L  K  Dyn.Def.'
               WRITE(32, *)' N   E[MeV]  J   pi Nph L  K  Dyn.Def.'
               READ(134,'(A80)') ch_iuf
               WRITE(133, *)' N   E[MeV]  J   pi Nph L  K  Dyn.Def.'
            ENDIF
         ENDDO
   70    READ(134,'(A80)',END=80) ch_iuf
         WRITE(133,'(A80)') ch_iuf
         GOTO 70
   80    CLOSE(133)
         CLOSE(134,STATUS='DELETE')
         IF(IOPsys.EQ.0)THEN
            ctmp = 'mv COLL.DAT TARGET_COLL.DAT'
            iwin = PIPE(ctmp)
         ELSE
            iwin = PIPE('move COLL.DAT TARGET_COLL.DAT')
         ENDIF
C
C        JOIN finished: TARGET_COLL.DAT and TARGET_COLL_RIPL.DAT files
C
         DO k = 1, NVIb(ncalc)
            WRITE(32,
     &            '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3)')
     &            k, EXV(k, ncalc), SPInv(k, ncalc),
     &      float(IPAr(k, ncalc)), NPH(k, ncalc), 0, 0, DEFv(k, ncalc)
            WRITE(6,
     &            '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3)')
     &            k, EXV(k, ncalc), SPInv(k, ncalc),
     &      float(IPAr(k, ncalc)), NPH(k, ncalc), 0, 0, DEFv(k, ncalc)
         ENDDO
         CLOSE(32)
         WRITE(6, *)
         WRITE(6, *)
      ENDIF
C
C     END OF SETTING COLLECTIVE LEVELS for DIRECT CALCULATIONS
C********************************************************************
 200  IF(iainp.LT.IAMin .OR. iainp.GT.IAMax)IWArn = 1
      IF(izinp.LT.IZMin .OR. izinp.GT.IZMax)IWArn = 2
C
      IF(E.LT.EEMin)THEN
         EEMin = E + 0.1D0
         IWArn = 3
      ENDIF
      IF(E.GT.EEMax)THEN
         EEMax = E + 1000.D0
         IWArn = 4
      ENDIF
      OMEmin(Nejc, Nnuc) = EEMin
      OMEmax(Nejc, Nnuc) = EEMax
      IRElat(Nejc, Nnuc) = IREl
      xmas_nejc = (AEJc(Nejc)*AMUmev + XMAss_ej(Nejc))/AMUmev
      xmas_nnuc = (A(Nnuc)*AMUmev + XMAss(Nnuc))/AMUmev
C
C     INITIALIZING /RIPLXX
C
C     COMMON /RIPLXX/ETA,ATAR,ZTAR,TARMAS,PROJMAS,
C    &       HBARC,AMU0C2,EFErmi,RC,ENCOUL
C
      eta = (XN(Nnuc) - Z(Nnuc))/A(Nnuc)
      atar = iainp
      ztar = izinp
      tarmas = xmas_nnuc
      projmas = xmas_nejc
      hbarc = HHBarc
      amu0c2 = AMUmev
C     rc is an output variable
      encoul=0.4*ztar/atar**(1./3.)
      EFErmi = EEFermi(Nejc,Nnuc)
      call optmod(E,vlib,rlib,alib)
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
C     pot(i,j,24) - Koning potentials, reference # 2404, 2405 ...
C=======================================================================
      RCOul(Nejc, Nnuc) = rc
C     Volume real potential: Woods-Saxon
      VOM(1, Nejc, Nnuc) = vlib(1)
      RVOm(1, Nejc, Nnuc) = rlib(1)
      AVOm(Nejc, Nnuc) = alib(1)
C-----Volume imaginary potential: Woods-Saxon
      WOMv(1, Nejc, Nnuc) = vlib(2)
      RWOmv(1, Nejc, Nnuc) = rlib(2)
      AWOmv(Nejc, Nnuc) = alib(2)
C     Real surface contribution
      VOMs(1, Nejc, Nnuc) = vlib(3)
C-----Surface imaginary potential:
C-----if rco(4,1,1) >0.0: Woods-Saxon derivative surface potential
C-----if rco(4,1,1) <0.0: Gaussian surface potential.
      WOMs(1, Nejc, Nnuc) = vlib(4)
      RWOm(1, Nejc, Nnuc) = rlib(4)
      AWOm(Nejc, Nnuc) = alib(4)
      SFIom(Nejc, Nnuc) = 1.D0
      IF(RCO(4, 1, 1).LT.0.0)SFIom(Nejc, Nnuc) = -1.D0
C-----Real spin-orbit
C-----****
      VSO(1, Nejc, Nnuc) = vlib(5)
      RVSo(1, Nejc, Nnuc) = rlib(5)
      AVSo(Nejc, Nnuc) = alib(5)
C-----Imaginary spin-orbit
C-----****
      WSO(1, Nejc, Nnuc) = vlib(6)
      RWSo(1, Nejc, Nnuc) = rlib(6)
      AWSo(Nejc, Nnuc) = alib(6)
      END
C
C
      SUBROUTINE OMPAR(Nejc, Nnuc, Eilab, Eicms, Mi, Mt, Rrmu, Ak2,
     &                 Komp, Ikey)
C
C      IKEY < 0  :   EIlab is given
C      IKEY > 0  :   EIcms is given
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
      INTEGER Nejc, Nnuc
C
C     Local variables
C
      CHARACTER*2 symbnucr, symbejcr
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
C-----Nonlocality in the Wvol is neglected ususally
C     EEA(Nejc, Nnuc) = 1000.1D0
C-----Arbitrarily default value
C     EEFermi(Nejc, Nnuc) = -10.6
C     EEP(Nejc, Nnuc) = EEFermi(Nejc, Nnuc)
      IRElat(Nejc, Nnuc) = 0
C
      fexist = OMPar_riplf
      IF(CCCalc)fexist = OMParfcc
C-----OMPAR.RIPL file exists ?
      IF(fexist)THEN
C-----------komp = 29       OR 33
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
C-----Searching in the RIPL database for IPOTEN catalog number
      CALL FINDPOT(ki, ieof, ipoten)
C-----Here ieof must be 0 always because we checked in input.f
      IF(ieof.GT.0)STOP 'PROBLEM with RIPL OMP library'
C-----Reading o.m.  potential parameters for IPOTEN catalog number
      CALL OMIN(ki, ieof, irel)
C
C-----Ener must be in LAB system
C
 50   IRElat(Nejc, Nnuc) = irel
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
      WOMs(1, Nejc, Nnuc) = MAX(WOMs(1, Nejc, Nnuc), 0.D0)
      WOMv(1, Nejc, Nnuc) = MAX(WOMv(1, Nejc, Nnuc), 0.D0)
C-----Some default protections
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
      IF(MOD(iowrite, 2).EQ.0 .AND. .NOT.fexist)
     &   THEN
         ki = 26
C--------komp = 29 OR 33
         ipoten = KTRlom(Nejc, Nnuc)
         CALL FINDPOT(ki, ieof, ipoten)
         IF(ieof.GT.0) THEN
             WRITE(6,*) 'WARNING: PROBLEM with RIPL OMP library'
             STOP 'PROBLEM with RIPL OMP library'
          ENDIF
C--------Reading IPOTEN catalog number potential parameters
         ianucr = INT(A(Nnuc))
         symbnucr = SYMb(Nnuc)
         iaejcr = INT(AEJc(Nejc))
         symbejcr = SYMbe(Nejc)
         CALL CREATE_OMPAR(ki, Komp, ieof, Nnuc, Nejc, ianucr, symbnucr,
     &                     iaejcr, symbejcr)
         IF(CCCalc)THEN
C-----------increase IOMWRITE so that all these is not written again and again
            IOMwritecc = 1
         ELSE
C-----------increase IOMWRITE so that all these is not written again and again
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
      INTEGER i, j, l, Iaejcr, Ianucr, Nejcr, Nnucr, Ieof
     &
      INTEGER Ki, Komp, krange
      CHARACTER*80 comment
      CHARACTER*2 Symbnucr, Symbejcr
      INCLUDE 'ripl2empire.h'
      CHARACTER*10 potnam(6)
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
          READ(Ki, '(a80)')comment
          WRITE(Komp, '(a80,a20)')comment, ' Ecoul,RC0,RC,RC1,RC2,beta'
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
      CHARACTER*80 comment
      INCLUDE 'ripl2empire.h'
C
C-----Read RIPL optical model parameters from the local OMPAR.RIPL file
C
99001 FORMAT(10A8)
C
      Ierr = 0
C-----Next comment has been suggested by V. Avrigeanu
C     REWIND(Ko)
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
            READ(Ko, 99005, ERR = 200)ecoul(j),
     &      rcoul0(j),rcoul(j),rcoul1(j),rcoul2(j),beta(j)
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
99005 FORMAT(6F10.3)
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
      INCLUDE 'ripl2empire.h'
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
            READ(Ki, *)ecoul(j),
     &      rcoul0(j),rcoul(j),rcoul1(j),rcoul2(j),beta(j)
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
      SUBROUTINE SUMPRT(Ko)
C
C     print summary information from RIPL formatted library
C
      INCLUDE 'ripl2empire.h'
      CHARACTER*8 ldum, proj
      CHARACTER*40 model
      CHARACTER*20 area
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
      IF(iarea.GE.600 .AND. iarea.LE.649)area = 'East Europe+FSU'
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
C-----Find IPOTEN entry in the RIPL optical model database
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
      DOUBLE PRECISION culbar
      INTEGER i, l
      INTEGER INT, MIN0
      LOGICAL Nonzero
C
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
C-----TARGET NUCLEUS (ELASTIC CHANNEL), incident neutron or proton
C-----Transmission coefficient matrix for elastic channel
C-----ECIS code is used
C-----KTRlom(Nejc,Nnuc)=KTRompCC
      IWArn = 0
C-----Preparing INPUT and RUNNING ECIS
C-----(or reading already calculated file)
      CALL TRANSINP(Nejc, Nnuc, NDETL)
C-----IWARN=0 - 'NO Warnings'
C-----IWARN=1 - 'A out of the recommended range '
C-----IWARN=2 - 'Z out of the recommended range '
C-----IWARN=3 - 'Energy requested lower than recommended for this potential'
C-----IWARN=4 - 'Energy requested higher than recommended for this potential'
      IF(IWArn.EQ.1 .AND. IOUt.GE.3)WRITE(6, *)
     &      ' WARNING: OMP not recommended for A=', A(Nnuc)
      IF(IWArn.EQ.2 .AND. IOUt.GE.3)WRITE(6, *)
     &      ' WARNING: OMP not recommended for Z=', Z(Nnuc)
      IF(IWArn.EQ.3 .AND. IOUt.GE.3)WRITE(6, *)
     &      ' WARNING: OMP not recommended for low energies in Tl calc'
      IF(IWArn.EQ.4 .AND. IOUt.GE.3)WRITE(6, *)
     &      ' WARNING: OMP not recommended for high energies in Tl calc'
      IWArn = 0
C--------transfer of the calculated transmission coeff. onto TL matrix
      DO i = 2, NDETL
         LMAxtl(i, Nejc, Nnuc) = MIN0(MAXl(i) + 1, NDLW)
         DO l = 1, LMAxtl(i, Nejc, Nnuc)
            IF(TTLl(i, l - 1).LT.1.D-10)GOTO 50
            TL(i, l, Nejc, Nnuc) = TTLl(i, l - 1)
            Nonzero = .TRUE.
         ENDDO
 50   ENDDO
      RETURN
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
C     | Calculation of transmission coefficients using ECIS              |
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
      LOGICAL fexist, ltmp
      INTEGER i, ien, ien_beg, l, lmax
      INTEGER INT
      REAL SNGL
      CHARACTER*18 ctmp18
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
      WRITE(ctmp18, '(i2.2,i2.2,1h_,a2,1h-,i3.3,1h_,i6.6)')
     &         INT(ZEJc(Nejc)),INT(AEJc(Nejc)),
     &       SYMb(NNUc),INT(A(NNUc)),INT(EINl*1000)
      IOK=1
      INQUIRE(FILE = ('../TL/'//ctmp18//'.BIN'), EXIST = fexist)
      IF(.NOT.fexist) GOTO 300
C-----Here the old calculated files should be readed
      OPEN(45, FILE = ('../TL/'//ctmp18//'.BIN'), FORM = 'UNFORMATTED')
      IF(IOUt.EQ.5)OPEN(46, FILE = '../TL/'//ctmp18//'.LST')
 100  READ(45, END = 200)lmax, ien, ener
      IF(IOUt.EQ.5)WRITE(46, '(A5,2I6,E12.6)')'LMAX:', lmax, ien, ener
C
C-----If (energy read from file do not coincide
C-----this nucleus should be recalculated (goto 300)
C
      IF(ABS(ener - ETL(ien,Nejc,Nnuc)).GT.0.0001)THEN
         CLOSE(45, STATUS = 'DELETE')
         IF(IOUt.EQ.5)CLOSE(46, STATUS = 'DELETE')
         WRITE(6, *)'WARNING: ENERGY MISMATCH: ETL(ien=', ien, '...)=',
     &              ETL(ien, Nejc, Nnuc), ' REQUESTED ENERGY=',
     &              SNGL(ener)
         WRITE(6, *)'WARNING: FILE WITH TRANSM. COEFF. HAS BEEN DELETED'
         GOTO 300
      ENDIF
      ETL(ien, Nejc, Nnuc) = ener
      MAXl(ien) = lmax
      DO l = 0, lmax
         READ(45,END=220)TTLl(ien, l)
         IF(IOUt.EQ.5)WRITE(46, *)l, TTLl(ien, l)
      ENDDO
C     Capote , preeq 01/2005
      READ(45,END=220) SIGabs(ien, Nejc, Nnuc)
      GOTO 100
C
 200  CLOSE(45)
      IF(IOUt.EQ.5) CLOSE(46)
      IF(IOUt.EQ.5)
     &   WRITE(6, *)'Transmission coefficients read from file: ',
     &           ('../TL/'//ctmp18//'.BIN')
      RETURN

 220  CLOSE(45, STATUS = 'DELETE')
      IF(IOUt.EQ.5)CLOSE(46, STATUS = 'DELETE')
      DO i = 1, NDETL
        MAXl(i) = 0
        DO l = 0, NDLW
          TTLl(i, l) = 0.D0
        ENDDO
      ENDDO
      IF(IOUt.GT.0) WRITE(6, *)
     & 'WARNING: ERROR WHEN READING TLs in ',ctmp18
      IF(IOUt.EQ.5) THEN
         WRITE(6, *)'WARNING: FILE WITH TRANSM. COEFF. HAS BEEN DELETED'
         WRITE(6, *)
     &           'WARNING: TRANSM. COEFF. WILL BE CALCULATED AND STORED'
      ENDIF
C-----Coulomb barrier (somewhat decreased) setting lower energy limit
C-----for transsmission coefficient calculations
 300  culbar = 0.8*ZEJc(Nejc)*Z(Nnuc)/(1 + A(Nnuc)**0.6666)
      ien_beg = -1
      DO i = 2, Nen
         IF(ETL(i, Nejc, Nnuc).GT.culbar) then
C           IF(ien_beg.EQ. - 1)ien_beg = i
            ien_beg = i
            goto 350
         ENDIF
      ENDDO
350   IF(ien_beg.NE. - 1)THEN
C
C--------Running ECIS
C
         IF(IOUt.EQ.5) then
            WRITE(6, *)
            IF(DIRect.EQ.2 .AND. AEJc(Nejc).LE.1)THEN
              WRITE(6, *)' CC transmission coefficients used for ',
     &                 'outgoing channels'
            ELSE
              WRITE(6, *)' Spherical OM transmission coefficients',
     &                   ' used for outgoing channels'
            ENDIF
            WRITE(6, *)
         ENDIF
C--------OPEN Unit=46 for Tl output
         OPEN(UNIT = 46, STATUS = 'unknown',
     &        FILE = ('../TL/'//ctmp18//'.BIN'), FORM = 'UNFORMATTED')
C
C--------do loop over energy
C
         ltmp = A(Nnuc).EQ.A(0) .AND. Z(Nnuc).EQ.Z(0) .AND.
     &          DIRect.EQ.2 .AND. AEJc(Nejc).LE.1

         DO i = ien_beg, Nen
            ener = ETL(i, Nejc, Nnuc)
            IF( ener. LE. 0.1d-6) cycle

            IF(ltmp) THEN
C-------------Transmission coefficient matrix for incident channel
C-------------is calculated (DIRECT = 2 (CCM)) using ECIS code.
C             Only coupled levels are considered
              IF(DEFormed)THEN
C               In this case we need only CC calculation so INLkey=0
                CALL ECIS_CCVIBROT(Nejc, Nnuc, ener, 0)
                CALL ECIS2EMPIRE_TR(Nejc, Nnuc, i, .FALSE.)
              ELSE
C               EXACT (no DWBA) calculation
                CALL ECIS_CCVIB(Nejc, Nnuc, ener, .FALSE., -1)
                CALL ECIS2EMPIRE_TR(Nejc, Nnuc, i, .TRUE.)
              ENDIF

            ELSE
C
C             Spherical optical model is assumed, only one level (gs)
C
              CALL ECIS_CCVIB(Nejc, Nnuc, ener, .TRUE., 0)
              CALL ECIS2EMPIRE_TR(Nejc, Nnuc, i, .TRUE.)

            ENDIF

         ENDDO
         CLOSE(46)
         IF(IOUt.EQ.5) WRITE(6, *)' Transm. coeff. written to file:',
     &              (' ../TL/'//ctmp18//'.BIN')
         IF(IOUt.EQ.5) WRITE(6, *)
     &            ' ==================================================='
      ELSEIF(IOUt.EQ.5)THEN
         WRITE(6, '(1x,A12,I3,A3,I3,A3,F4.1)')'EJECTILE: A=',
     &         INT(AEJc(Nejc)), ' Z=', INT(ZEJc(Nejc)), ' S=',
     &         SEJc(Nejc)
         WRITE(6, '(1x,A12,I3,A3,I3,A3,F4.1,A3,I2)')'RESIDUAL: A=',
     &         INT(A(Nnuc)), ' Z=', INT(Z(Nnuc)), ' S=',
     &         SNGL(XJLv(LEVtarg, Nnuc)), ' P=', INT(LVP(LEVtarg, Nnuc))
         WRITE(6, *)'WARNING: For this residual nucleus'
         WRITE(6, *)'WARNING: available energy is always '
         WRITE(6, *)'WARNING: below coulomb barrier'
         WRITE(6, *)'WARNING: Calculations are not needed!'
         WRITE(6, *)
      ENDIF
      RETURN
      END


      SUBROUTINE ECIS2EMPIRE_TL_TRG(Nejc, Nnuc, Maxlw, stl, LVIbrat)
C
C     Process ECIS output to obtain Stl matrix for the incident channel
C     Reads from unit 45 and writes to unit 6
C
C     INPUT:  Nejc,Nnuc ejectile and residual nucleus index
C             LVIbrat = .TRUE. for vibrational model(or spherical OMP)
C             The implemented model could be applied to even-even nucleus
C             therefore the ECIS input is always constructed assuming spin 0
C             So we can not use the real spin
C             of the given nucleus XJLv(LEVtarg, Nnuc)
C
C     OUTPUT: Maxlw and Stl(1-Maxlw) are the maximum angular momentum and Tls
C             SINl is the total inelastic cross section
C
C
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C     Dummy arguments
C
      INTEGER Maxlw, Nejc, Nnuc
      DOUBLE PRECISION Stl(NDLW)
      LOGICAL LVIbrat
C
C     COMMON
C
      COMMON /ECISXS/ ELAcs, TOTcs, ABScs, SINl
C
C     Local variables
C
      DOUBLE PRECISION cte, dtmp, elab, jc, jj, RMU, sabs,
     &                 sreac, xmas_nejc, xmas_nnuc,
     &                 ELAcs, TOTcs, ABScs, SINl

      DOUBLE PRECISION DBLE
      INTEGER l, nc, nceq, ncoll, nlev
      CHARACTER*1 parc
      REAL SNGL

C------------------------------------------
C-----| Input of transmission coefficients|
C------------------------------------------
C-----Opening ecis03 output file containing Tlj
C     OPEN(UNIT = 45, STATUS = 'old', FILE = 'ecis03.tlj')
      OPEN(UNIT = 45, STATUS = 'old', FILE = 'INCIDENT.TLJ')
      READ(45, *, END = 200)  ! To skip first line <TLJs.> ..
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
     &                   /DBLE(2*XJLv(LEVtarg, Nnuc) + 1)
            Maxlw = MAX(Maxlw, l)
         ENDIF
      ENDDO
      GOTO 100
 200  CLOSE(45)

C     For vibrational the Tls must be multiplied by
      IF(LVIbrat) then
        DO l = 0, Maxlw
          Stl(l+1) = Stl(l+1)*DBLE(2*XJLv(LEVtarg, Nnuc) + 1)
        ENDDO
      ENDIF

      TOTcs = 0.d0
      ABScs = 0.d0
      ELAcs = 0.d0
C     OPEN(UNIT = 45, FILE = 'ecis03.cs', STATUS = 'old', ERR = 400)
      OPEN(UNIT = 45, FILE = 'INCIDENT.CS', STATUS = 'old', ERR = 400)
      READ(45, *, END = 400) ! Skipping first line
      IF (ZEjc(NEJc).eq.0) READ(45, *) TOTcs
      READ(45, *) ABScs
      IF (ZEjc(NEJc).eq.0) READ(45, *) ELAcs
 400  CLOSE(45)

      if(ABScs.le.0.d0) return
C
C     Estimating absorption cross section from obtained TLs
C
C     xmas_nejc = (AEJc(Nejc)*AMUmev + XMAss_ej(Nejc))/AMUmev
      xmas_nnuc = (A(Nnuc)*AMUmev + XMAss(Nnuc))/AMUmev
      xmas_nejc = EJMass(Nejc)
C     xmas_nnuc = AMAss(Nnuc)
      ecms = EIN
      CALL KINEMA(elab, ecms, xmas_nejc,xmas_nnuc,RMU, ak2, 2, RELkin)
C
C     EIN already in CMS
      cte = PI/(W2*RMU*EIN)
C-----Absorption cross section in mb
      sabs = 0.D0
      DO l = 0, Maxlw
        sabs = sabs + Stl(l + 1)*DBLE(2*l + 1)
      ENDDO
      sabs = cte*sabs

      if(sabs.le.0.d0) return

      SINl = 0.D0

      OPEN(UNIT = 45, FILE = 'INCIDENT.ICS', STATUS = 'old', ERR= 300)
      READ(45, *, END = 300) ! Skipping first line
      DO l = 1, NDCollev
        READ(45, *, END = 300)dtmp
        SINl = SINl + dtmp
      ENDDO
 300  CLOSE(45)

      IF(SINl.GT.ABScs)THEN
        WRITE(6, *)
        WRITE(6, *) ' WARNING: LOOK ECIS NON-CONVERGENCE !!'
        WRITE(6,
     &   '(5x,''**************************************************'')')
        WRITE(6,
     &   '(5x,'' Direct cross section calculation do not converge '')')
        WRITE(6,'(6x,''Inelastic cross section ='',F8.2,'' mb''/
     &            6x,''Reaction  cross section ='',F8.2,'' mb''/)')
     &        SINl, ABScs
        WRITE(6,
     &   '(5x,'' Either change OMP or change calculation method   '')')
        WRITE(6,
     &   '(5x,''        (DIRPOT)   or   (DIRECT) parameters       '')')
          WRITE(6,
     &   '(5x,'' This problem usually happens using DWBA method   '')')
          WRITE(6,
     &   '(5x,'' to treat strong coupled nuclei                   '')')
          WRITE(6,
     &   '(5x,''            CALCULATION STOPPED                   '')')
          WRITE(6,
     &   '(5x,''**************************************************'')')
          STOP 200
      ENDIF

      sreac = sabs + SINl

      IF(IOUT.EQ.5 .AND. sabs.gt.0.d0) then
          WRITE(6, *)
          WRITE(6, *)' INCIDENT CHANNEL:'
          WRITE(6, '(A7,I6,A5,F10.3,A10,F10.3,A10)')'  LMAX:', Maxlw,
     &                 ' E = ', EIN,' MeV (CMS)', elab, ' MeV (LAB)'
          WRITE(6, *)' XS calculated using averaged Tls:   Sabs =',
     &              SNGL(sabs), ' mb '
          WRITE(6, *)
     &      ' Reaction XS =', SNGL(ABScs),' mb (read from ECIS)'
          WRITE(6,*)
     &      ' ECIS/EMPIRE ratio of reaction cross section =',
     &              (ABScs-SINl)/sabs
          IF(SINl.gt.0.d0) THEN
            WRITE(6, *)' Sinl =',SNGL(SINl),' mb (read from ECIS)'
            WRITE(6, *)' Sreac=', SNGL(sreac), ' mb (Sabs + Sinl)'
          ENDIF
          WRITE(6, *)' Total XS =', SNGL(TOTcs),' mb (read from ECIS)'
          WRITE(6, *)' Shape Elastic XS =', SNGL(ELAcs),
     &               ' mb (read from ECIS)'
          WRITE(6,*)
      ENDIF
C
C     Renormalizing TLs to reproduce ecis_abs cross section
C
      DO l = 0, Maxlw
        Stl(l + 1)=Stl(l + 1)*(ABScs-SINl)/sabs
      ENDDO

      RETURN
      END
C
C
C
      SUBROUTINE ECIS2EMPIRE_TR(Nejc, Nnuc, J, LVIbrat)
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
      DOUBLE PRECISION cte, dtmp, ecms, elab, jc, jj, RMU, sabs,selecis,
     &                 sreac, sreacecis, stotecis, xmas_nejc, xmas_nnuc
      DOUBLE PRECISION DBLE
      INTEGER l, lmax, nc, nceq, ncoll, nlev
      CHARACTER*1 parc
      LOGICAL LVIbrat
      REAL SNGL

      ecms = ETL(J, Nejc, Nnuc)
C-----
C----- Input of transmission coefficients
C-----
      OPEN(UNIT = 45, STATUS = 'old', FILE = 'ecis03.tlj')
      READ(45, *, END = 200) ! Skipping one line
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
     &                   /DBLE(2*XJLv(LEVtarg, Nnuc) + 1)
            lmax = MAX(lmax, l)
         ENDIF
      ENDDO
      GOTO 100
 200  CLOSE(45)

C     For vibrational the Tls must be multiplied by
      IF(LVIbrat) then
        DO l = 0, lmax
          TTLl(J, l) = TTLl(J, l)*DBLE(2*XJLv(LEVtarg, Nnuc) + 1)
        ENDDO
      ENDIF

      stotecis = 0.d0
      selecis = 0.d0
      sreacecis = 0.d0
      OPEN(UNIT = 45, FILE = 'ecis03.cs', STATUS = 'old', ERR = 400)
      READ(45, *, END = 400) ! Skipping one line
      IF (ZEjc(NEJc).eq.0) READ(45, *, END = 400)stotecis
      READ(45, *, END = 400)sreacecis
      IF (ZEjc(NEJc).eq.0) READ(45, *, END = 400)selecis
 400  CLOSE(45)

      if(sreacecis.le.0.d0)  RETURN

      IF(IOUT.EQ.5) then

C       xmas_nejc = (AEJc(Nejc)*AMUmev + XMAss_ej(Nejc))/AMUmev
        xmas_nnuc = (A(Nnuc)*AMUmev + XMAss(Nnuc))/AMUmev
        xmas_nejc = EJMass(Nejc)
C       xmas_nnuc = AMAss(Nnuc)
        CALL KINEMA(elab,ecms,xmas_nejc,xmas_nnuc, RMU, ak2, 2, RELkin)

C-------ECIS constant
        cte = PI/(W2*RMU*ecms)

C-------Reaction cross section in mb
        sabs = 0.D0
        DO l = 0, lmax
          sabs = sabs + TTLl(J, l)*DBLE(2*l + 1)
        ENDDO
        sabs = cte*sabs

        OPEN(UNIT = 45, FILE = 'ecis03.ics', STATUS = 'old', ERR = 300)
        READ(45, *, END = 300) ! Skipping one line
        SINl = 0.D0
        DO l = 1, ncoll - 1
          READ(45, *, END = 300)dtmp
          SINl = SINl + dtmp
        ENDDO
 300    CLOSE(45)

        sreac = sabs + SINl

        IF(sabs.gt.0.d0) then
          WRITE(6, *)
          WRITE(6, '(A7,I3,A3,E12.6,A10,E12.6,A26,1x,I4)')'  LMAX:',
     &                          lmax,' E=', ecms,' MeV (CMS)', elab,
     &                          ' MeV (LAB); Energy index =',J
          WRITE(6, *)' XS calculated using averaged Tls:   Sabs =',
     &                          SNGL(sabs), ' mb '
          WRITE(6, *)
     &       ' Reaction XS =', SNGL(sreacecis),' mb (read from ECIS)'
          IF(SINl.gt.0.d0) THEN
            WRITE(6, *)' Sinl =', SNGL(SINl), ' mb (read from ECIS)'
            WRITE(6, *)' Sreac=', SNGL(sreac), ' mb (Sabs + Sinl)'
          ENDIF
        ENDIF
      ENDIF

C-----Storing transmission coefficients for EMPIRE energy grid
      WRITE(46)lmax, J, ecms
      DO l = 0, lmax
         WRITE(46)TTLl(J, l)
      ENDDO
      WRITE(46) sreacecis
      MAXl(J) = lmax
C     Capote , preeq 2002
      SIGabs(J, Nejc, Nnuc) = sreacecis
      RETURN
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
C     BECis2 = 'FFFFFFFFFFFFFTFFTTTFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'
C     For ecis03
      BECis2 = 'FFFFFFFFTFFFFTFFTTTFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'
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
      SUBROUTINE ECIS_CCVIB(Nejc, Nnuc, El, LDWBA, INLkey)
C
C     -------------------------------------------------------------
C     |      Create ECIS03 input files for COUPLED CHANNELS       |
C     |        calculation of transmission coefficients in        |
C     |             HARMONIC  VIBRATIONAL   MODEL                 |
C     |               v1.0     S.Masetti 5/99                     |
C     |               v2.0     R.Capote  1/2001                   |
C     |               v3.0     R.Capote  5/2001 (DWBA added)      |
C     |               v4.0     R.Capote  1/2005 (SOMP added)      |
C     -------------------------------------------------------------
C
C     INLkey = 0  DWBA calculation for the ground state = Spher.OMP
C     INLkey > 0  Calculation for coupled and uncoupled states = DWBA or CC
C     INLkey < 0  Calculation for coupled states only = CC
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
      LOGICAL LDWBA
      INTEGER Nejc, Nnuc, INLkey
C
C     Local variables
C
      DOUBLE PRECISION elabe
      CHARACTER*1 ch
      DOUBLE PRECISION eee, elab, xmas_nejc, xmas_nnuc, xratio,
     &                 zerosp
      INTEGER INT, NINT
      INTEGER ip, iterm, j, ldwmax, ncoll, nd_nlvop, njmax, npp
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
      ECIs2(13:13) = 'T'
C-----Cmatrix output
      ECIs2(5:5) = 'F'
C-----Smatrix output
      ECIs2(6:6) = 'F'
C     DWBA option added
      IF(DIRect.EQ.3 .OR. LDWBA)THEN
C-----Iteration scheme used for DWBA
         ECIs1(21:21) = 'F'
C        ECIs1(23:23) = 'F'
C        DWBA
         ECIs2(42:42) = 'T'
      ENDIF
C
C     xmas_nejc = (AEJc(Nejc)*AMUmev + XMAss_ej(Nejc))/AMUmev
      xmas_nejc = EJMass(Nejc)
      xmas_nnuc = (A(Nnuc)*AMUmev + XMAss(Nnuc))/AMUmev
C
      xratio = xmas_nnuc/(xmas_nejc + xmas_nnuc)
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
C-----Transformation of energies from laboratory to center-of-mass if needed
C-----is done inside SETPOTS() -> OMPAR()
C
      CALL SETPOTS(Nejc, Nnuc, elab, ecms, xmas_nejc, xmas_nnuc, RMU,
     &             ak2, ikey)
C-----relativistic kinematics ?
      IF(IRElat(Nejc, Nnuc).GT.0 .OR. FLGrel.EQ.'y')ECIs1(8:8) = 'T'
C
C-----Only for target, find open channels
C-----At least ground state is always open !!, RCN 31/03/2001
      nd_nlvop = 1
      nd_cons = 1
      IF(INLkey.NE.0) THEN
        DO j = 2, ND_nlv
          if(.NOT.LDWBA .AND. icollev(j).GT.50) cycle
          nd_cons = nd_cons + 1
          eee = El - D_Elv(j)/xratio
          IF(eee.GT.0.0001)nd_nlvop = nd_nlvop + 1
        ENDDO
        IF(.NOT.LDWBA .AND. INLkey.GT.0 .AND. nd_nlvop.EQ.1)
     &   WRITE(6, *)' All inelastic channels are closed at this energy'
       ENDIF
C
C-----Considering even closed channels in calculations
C     ncoll = ND_nlv
C     Considering only coupled levels if CC calculation
      ncoll = nd_cons
C
      iterm = 20
C-----For DWBA only one iteration is used
      IF(ECIs2(42:42).EQ.'T')iterm = 1
C-----Defining one potential for each collective level
C-----If channel is closed ground state potential is used for this level
      npp = nd_nlvop
C
      zerosp = 0.0
      ldwmax = 2.4*1.25*A(Nnuc)**0.33333333*0.22*
     & SQRT((xmas_nejc+xmas_nnuc)*elab)
C-----Maximum number of channel spin (increased to 100 for high energy scattering)
      njmax = MAX(ldwmax, 20)
      IF(INLkey.eq.0) then
C-------writing input
        OPEN(UNIT = 1, STATUS = 'unknown', FILE = 'ecSPH.inp')
C-------CARD 1 : Title
        WRITE(1,
     &      '(f6.2,'' MeV '',a8,'' on '',i3,a2,'': SPHERICAL OMP'')'
     &      )El, PARname(ip), NINT(A(Nnuc)), NUC(NINT(Z(Nnuc)))
       ELSE
C-------writing input
        OPEN(UNIT = 1, STATUS = 'unknown', FILE = 'ecVIB.inp')
C-------CARD 1 : Title
        WRITE(1,
     &      '(f6.2,'' MeV '',a8,'' on '',i3,a2,'': VIBRATIONAL MODEL'')'
     &      )El, PARname(ip), NINT(A(Nnuc)), NUC(NINT(Z(Nnuc)))
       ENDIF
C-----CARD 2
      WRITE(1, '(a50)')ECIs1
C-----CARD 3
      WRITE(1, '(a50)')ECIs2
C-----CARD 4
      WRITE(1, '(4i5)')ncoll, njmax, iterm, npp
C-----Matching radius
C-----CARD 5
      rmatch = 20.d0
      WRITE(1, '(10x,f10.5)')rmatch
C     Matching radius calculated within ECIS
C     WRITE(1, *)
C-----ground state
C     ch = '-'
C     IF ( LVP(LEVtarg,Nnuc).GT.0 ) ch = '+'
C-----Important: Instead of using TARGET SPIN (XJLV(1,NNUC)) and PARITY(ch)
C-----A.Koning always used in PREGNASH SPIN=0, ch='+'
C-----It is justified for vibrational model and DWBA calculations
C-----so we are using zero spin here
C-----NOT TRUE for rotational model calculations (see ecis_CCrot)
C     WRITE(1, '(f5.2,2i2,a1,5f10.5)')XJLV(LEVtarg,NNUC),0,1, ch, elab,
      WRITE(1, '(f5.2,2i2,a1,5f10.5)')zerosp, 0, 1, '+', elab,
     &                                SEJc(Nejc), xmas_nejc, xmas_nnuc,
     &                                Z(Nnuc)*ZEJc(Nejc)
C-----0 phonon involved
      WRITE(1, '( )')
      IF(INLkey.NE.0) then
C------discrete levels
       nwrite = 1
       DO j = 2, ND_nlv
C       All levels with icollev(j)>50 should be calculated by DWBA
        if(.NOT.LDWBA .AND. icollev(j).GT.50) cycle
        ch = '-'
        IF(D_Lvp(j).GT.0)ch = '+'
C-------If channel is closed ground state potential is used for this level
        eee = El - D_Elv(j)/xratio
        IF(eee.GE.0.0001)THEN
          nwrite = nwrite + 1
          WRITE(1, '(f5.2,2i2,a1,5f10.5)')D_Xjlv(j), 0, nwrite, ch,
     &            D_Elv(j)
        ELSE
          WRITE(1, '(f5.2,2i2,a1,5f10.5)')D_Xjlv(j), 0,      1, ch,
     &            D_Elv(j)
        ENDIF
        IF(LDWBA .OR. DIRECT.EQ.3) THEN
C         If DWBA, all states are assumed to be one phonon
          WRITE(1, '(3i5)') 1, j - 1, 0
        ELSE
          IF(IPH(j).EQ.1)THEN
            WRITE(1, '(3i5)')IPH(j), j - 1, 0
          ELSE
C-----------two  phonon states if exist are formed from the quadrupole
C-----------quadrupole phonon spin is equal to 2+ phonon
            WRITE(1, '(3i5)')IPH(j), 1, 1
          ENDIF
        ENDIF
       ENDDO
C
C------deformations: phonon description
C
       DO j = 2, ND_nlv
C       All levels with icollev(j)>50 should be calculated by DWBA
        if(.NOT.LDWBA .AND. icollev(j).GT.50) cycle
        IF(LDWBA .OR. DIRECT.EQ.3) THEN
C         If DWBA, all states are assumed to be one phonon
          WRITE(1, '(i5,5x,6f10.5)')INT(D_Xjlv(j)), D_Def(j, 2)
        ELSE
C         only one phonon states need deformations as input
          IF(IPH(j).EQ.1)WRITE(1, '(i5,5x,6f10.5)')INT(D_Xjlv(j)),
     &                        D_Def(j, 2)
        ENDIF
       ENDDO
       ENDIF
C
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
      IF(INLkey.NE.0) then
C
C------2) discrete levels
       DO j = 2, ND_nlv
C           All levels with icollev(j)>50 should be calculated by DWBA
            if(.NOT.LDWBA .AND. icollev(j).GT.50) cycle
            eee = El - D_Elv(j)/xratio
C-----------If channel is closed ground state potential is used for this level
            IF(eee.GE.0.0001)THEN
C--------------SETPOTS  : subroutine for optical model parameters
C--------------From     cms system to Lab
               ecms = eee
               ikey = +1
C--------------Transformation of energies from laboratory to center-of-mass
C--------------if needed is done inside SETPOTS() -> OMPAR()
C
               CALL SETPOTS(Nejc, Nnuc, elabe, ecms, xmas_nejc,
     &                      xmas_nnuc, RMU, ak2, ikey)
C
C--------------potential parameters
C--------------write(1,'(3f10.5)') v,rv,av
               IF(POTe(1).NE.0.)THEN
                  WRITE(1, '(3f10.5)')POTe(1), RVOm(1, Nejc, Nnuc),
     &                                AVOm(Nejc, Nnuc)
               ELSE
                  WRITE(1, '(3f10.5)')0., 0., 0.
               ENDIF
C--------------write(1,'(3f10.5)') w,rw,aw
               IF(POTe(3).NE.0.)THEN
                  WRITE(1, '(3f10.5)')POTe(3), RWOmv(1, Nejc, Nnuc),
     &                                AWOmv(Nejc, Nnuc)
               ELSE
                  WRITE(1, '(3f10.5)')0., 0., 0.
               ENDIF
C--------------write(1,'(3f10.5)') vd,rvd,avd
               IF(POTe(7).NE.0.)THEN
                  WRITE(1, '(3f10.5)')POTe(7), RWOm(1, Nejc, Nnuc),
     &                                AWOm(Nejc, Nnuc)
               ELSE
                  WRITE(1, '(3f10.5)')0., 0., 0.
               ENDIF
C--------------write(1,'(3f10.5)') wd,rwd,awd
               IF(POTe(2).NE.0.)THEN
                  WRITE(1, '(3f10.5)')POTe(2), RWOm(1, Nejc, Nnuc),
     &                                AWOm(Nejc, Nnuc)
               ELSE
                  WRITE(1, '(3f10.5)')0., 0., 0.
               ENDIF
C--------------write(1,'(3f10.5)') vvso,rrvso,aavso
               IF(POTe(4).NE.0.)THEN
                  WRITE(1, '(3f10.5)')POTe(4), RVSo(1, Nejc, Nnuc),
     &                                AVSo(Nejc, Nnuc)
               ELSE
                  WRITE(1, '(3f10.5)')0., 0., 0.
               ENDIF
C--------------write(1,'(3f10.5)') wwso,rwso,awso
               IF(WSO(1, Nejc, Nnuc).NE.0.)THEN
                  WRITE(1, '(3f10.5)')WSO(1, Nejc, Nnuc),
     &                                RWSo(1, Nejc, Nnuc),
     &                                AWSo(Nejc, Nnuc)
               ELSE
                  WRITE(1, '(3f10.5)')0., 0., 0.
               ENDIF
C--------------write(1,'(3f10.5)') rc,0.,0.
               WRITE(1, '(3f10.5)')RCOul(Nejc, Nnuc), 0., 0.
               WRITE(1, '(3f10.5)')0., 0., 0.
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
      IF(INLkey.eq.0) then
       CALL ECIS('ecSPH.inp','ECIS_SPH.out#')
      ELSE
       CALL ECIS('ecVIB.inp','ECIS_VIB.out#')
      ENDIF

      RETURN
      END
C
C
C
      SUBROUTINE ECIS_CCVIBROT(Nejc, Nnuc, El, INLkey)
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
      INTEGER Nejc, Nnuc, INLkey
C
C     Local variables
C
      CHARACTER*1 ch
      DOUBLE PRECISION eee, elab, xmas_nejc, xmas_nnuc, xratio
      DOUBLE PRECISION elabe
      INTEGER ip, iterm, j, ldwmax, lev(NDLV), ncoll, nd_nlvop, njmax,
     &        npho, npp, k, ND_cons
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
C-----Deformation read instead of deformation lengths
C-----for Woods Saxon form factors
      ECIs1(6:6) = 'F'
C-----Coulomb potential deformed
      ECIs1(11:11) = 'T'
C-----Real and imaginary central potential deformed
      ECIs1(12:12) = 'T'
C-----Real SO potential deformed
C-----ECIs1(13:13) = 'T'
C-----Imaginary SO potential deformed
C-----ECIs1(14:14) = 'T'
C-----Usual coupled equations instead of ECIS scheme is used
C-----Spin-orbit potential must be not deformed !!
      ECIs1(21:21) = 'T'
C-----ECIS iteration scheme is used.
C-----Shift to coupled equations if convergence is not achieved
      ECIs1(23:23) = 'T'
      ECIs2 = BECis2
C-----Angular distribution is calculated
      ECIs2(14:14) = 'T'
C-----Penetrabilities punched on cards
      ECIs2(13:13) = 'T'
C
C-----Cmatrix output
      ECIs2(5:5) = 'F'
C-----Smatrix output
      ECIs2(6:6) = 'F'
C-----DWBA option added
      IF(DIRect.EQ.3)THEN
C-----Iteration scheme used for DWBA
         ECIs1(21:21) = 'F'
C--------ECIs1(23:23) = 'F'
C--------DWBA
         ECIs2(42:42) = 'T'
      ENDIF
C-----relativistic kinematics ?
      IF(IRElat(Nejc, Nnuc).GT.0 .OR. FLGrel.EQ.'y')ECIs1(8:8) = 'T'
C
C     xmas_nejc = (AEJc(Nejc)*AMUmev + XMAss_ej(Nejc))/AMUmev
      xmas_nejc = EJMass(Nejc)
      xmas_nnuc = (A(Nnuc)*AMUmev + XMAss(Nnuc))/AMUmev
      xratio = xmas_nnuc/(xmas_nejc + xmas_nnuc)
C
C-----From cms system to Lab (ECIS do inverse convertion)
      IF(El.LT.0.D0)THEN
         El = DABS( - El)
         elab = El
         ikey = -1
      ELSE
         ecms = El
         ikey = +1
      ENDIF
C
C-----Transformation of energies from laboratory to center-of-mass if needed
C-----is done inside SETPOTS() -> OMPAR()
C
      CALL SETPOTS(Nejc, Nnuc, elab, ecms, xmas_nejc, xmas_nnuc, RMU,
     &             ak2, ikey)
C-----relativistic kinematics ?
      IF(IRElat(Nejc, Nnuc).GT.0 .OR. FLGrel.EQ.'y')ECIs1(8:8) = 'T'
C
C-----Only for target, find open channels
C-----At least ground state is always open !!, RCN 31/03/2001
C-----nd_nlvop = 0
      nd_nlvop = 1
      IF(ND_nlv.GT.0)THEN
         nd_cons=1
         DO j = 2, ND_nlv
C           All levels with icollev(j)>50 should be calculated by DWBA
            if(INLkey.eq.0 .AND. icollev(j).gt.50) cycle
C           All levels with icollev(j)<50 should be calculated by CC
            if(INLkey.eq.1 .AND. icollev(j).LT.50) cycle
            nd_cons = nd_cons + 1
            eee = El - D_Elv(j)/xratio
            IF(eee.GT.0.0001)nd_nlvop = nd_nlvop + 1
         ENDDO
      ENDIF
      IF(nd_nlvop.EQ.1)WRITE(6, *)
     &                'All inelastic channels are closed at this energy'
C-----ncoll = nd_nlvop
C-----Considering even closed channels in calculations
C     ncoll = ND_nlv
      ncoll = nd_cons
C
      iterm = 20
C-----For DWBA only one iteration is used
      IF(ECIs2(42:42).EQ.'T' .OR. INLkey.eq.1) iterm = 1
C
C-----Defining one potential for each collective level
C-----If channel is closed ground state potential is used for this level
      npp = nd_nlvop
C
C     zerosp=0.0
C     ldwmax=2.4*1.25*AN(i)**0.33333333*0.22*sqrt(parmas(i)*e)
      ldwmax = 2.4*1.25*A(Nnuc)**0.33333333*0.22*
     & SQRT((xmas_nejc+xmas_nnuc)*elab)
C     &         **0.33333333*0.22*SQRT(((AEJc(Nejc)*amumev+XMAss_ej(Nejc)
C     &         )/amumev)*El)
C-----Maximum number of channel spin
      njmax = MAX(ldwmax, 20)
C-----Writing input
      OPEN(UNIT = 1, STATUS = 'unknown', FILE = 'ecVIBROT.inp')
C-----CARD 1 : Title
      WRITE(1,
     &'(f6.2,'' MeV '',a8,'' on '',i3,a2,'': VIBR-ROTATIONAL CC     '')'
     &)El, PARname(ip), NINT(A(Nnuc)), NUC(NINT(Z(Nnuc)))
C-----CARD 2
      WRITE(1, '(a50)')ECIs1
C-----CARD 3
      WRITE(1, '(a50)')ECIs2
C-----CARD 4
C-----make sure that all contributions to s-wave scattering are included
      jdm = XJLv(LEVtarg, Nnuc) + SEJc(Nejc) + 0.6
      WRITE(1, '(4i5,30x,i5)')ncoll, njmax, iterm, npp, jdm
C-----CARD 5
      rmatch = 20.d0
      WRITE(1, '(10x,f10.5)')rmatch
C     Matching radius calculated within ECIS
C     WRITE(1, *)
      ch = '-'
      IF(LVP(LEVtarg, Nnuc).GT.0)ch = '+'
C
C-----Important: Instead of using TARGET SPIN (XJLV(1,NNUC)) and PARITY(ch)
C-----A.Koning always used in PREGNASH SPIN=0, ch='+'
C-----This is not TRUE for rotational model so we are using the target spin here
C------groundstate
C     write(1,'(f5.2,2i2,a1,5f10.5)') XJLV(1,NNUC),0,1,ch,EL,
      WRITE(1, '(f5.2,2i2,a1,5f10.5)')XJLv(LEVtarg, Nnuc), 0, 1, ch,
     &                                elab,
     &                                SEJc(Nejc), xmas_nejc, xmas_nnuc,
     &                                Z(Nnuc)*ZEJc(Nejc)
C-----Discrete levels
      npho = 0
      nwrite = 1
      DO j = 2, ND_nlv
C        RCN, 09/2004
C        All levels with icollev(j)>50 should be calculated by DWBA
         if(INLkey.eq.0 .AND. icollev(j).gt.50) cycle
C        All levels with icollev(j)<50 should be calculated by CC
         if(INLkey.eq.1 .AND. icollev(j).LT.50) cycle
         ch = '-'
         IF(D_Lvp(j).GT.0)ch = '+'
         nwrite = nwrite + 1
C--------If channel is closed ground state potential is used for this level
         eee = El - D_Elv(j)/xratio
         IF(eee.GE.0.0001) then
C         WRITE(1, '(f5.2,2i2,a1,5f10.5)')D_Xjlv(j), IPH(j), nwrite, ch,
C    &         D_Elv(j), SEJc(Nejc), xmas_nejc, xmas_nnuc, Z(Nnuc)
C    &         *ZEJc(Nejc)
          WRITE(1, '(f5.2,2i2,a1,5f10.5)')D_Xjlv(j), IPH(j), nwrite, ch,
     &         D_Elv(j)
         ELSE
          WRITE(1, '(f5.2,2i2,a1,5f10.5)')D_Xjlv(j), IPH(j),      1, ch,
     &         D_Elv(j)
         ENDIF
         IF(IPH(j).NE.0)THEN
            npho = npho + 1
            lev(npho) = j
            WRITE(1, '(10i5)')1, npho
         ENDIF
      ENDDO
C-----Description of phonons
      IF(npho.GT.0)THEN
C
C--------In vibrational rotational model the IPH(j) array contains the
C--------l orbital angular momentum of the phonon.
C--------We are assuming that orbital angular momentum of the phonon is
C--------equal INT(J). The k magnetic quantum number of the vibration is
C--------assumed to be zero !!!!
C--------L_PHO(lev(j))  = IPH(j)
C--------L3_pho(lev(j)) = 0
         DO j = 1, npho
C           IPH(lev(j)) , 0 , D_Def(lev(j))
            WRITE(1, '(2i5,6f10.5)')INT(D_Xjlv(lev(j)) + 0.1), 0,
     &                              D_Def(lev(j), 2)
         ENDDO
      ENDIF
C-----Deformation of rotational band (only ground state band is present)
C-----IdefCC   = maximum degree of deformation
C-----LMaxCC   = maximum L in multipole decomposition
      WRITE(1, '(2i5,f10.5)')IDEfcc, LMAxcc, D_Xjlv(1)
C     WRITE (1,'(6f10.5)') D_Def(1)
      WRITE(1, '(6f10.5)')(D_Def(1, k), k = 2, IDEfcc, 2)
C-----Potential parameters
C-----1) groundstate
C-----write(1,'(3f10.5)') v,rv,av
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
            STOP '16'
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
C-----2) discrete levels
      DO j = 2, ND_nlv
C        All levels with icollev(j)>50 should be calculated by DWBA
         if(INLkey.eq.0 .AND. icollev(j).gt.50) cycle
C        All levels with icollev(j)<50 should be calculated by CC
         if(INLkey.eq.1 .AND. icollev(j).LT.50) cycle
C--------If channel is closed ground state potential is used for this level
         eee = El - D_Elv(j)/xratio
         IF(eee.GE.0.0001)THEN
C--------SETPOTS : subroutine for optical model parameters
C--------From  cms system to Lab
            ecms = eee
            ikey = +1
C
C-----------Transformation of energies from laboratory to center-of-mass if
C-----------needed is done inside SETPOTS() -> OMPAR()
C
            CALL SETPOTS(Nejc, Nnuc, elabe, ecms, xmas_nejc, xmas_nnuc,
     &                   RMU, ak2, ikey)
C
C-----------Potential parameters
C-----------write(1,'(3f10.5)') v,rv,av
            IF(POTe(1).NE.0.)THEN
               WRITE(1, '(3f10.5)')POTe(1), RVOm(1, Nejc, Nnuc),
     &                             AVOm(Nejc, Nnuc)
            ELSE
               WRITE(1, '(3f10.5)')0., 0., 0.
            ENDIF
C-----------write(1,'(3f10.5)') w,rw,aw
            IF(POTe(3).NE.0.)THEN
               WRITE(1, '(3f10.5)')POTe(3), RWOmv(1, Nejc, Nnuc),
     &                             AWOmv(Nejc, Nnuc)
            ELSE
               WRITE(1, '(3f10.5)')0., 0., 0.
            ENDIF
C-----------write(1,'(3f10.5)') vd,rvd,avd
            IF(POTe(7).NE.0.)THEN
               WRITE(1, '(3f10.5)')POTe(7), RWOm(1, Nejc, Nnuc),
     &                             AWOm(Nejc, Nnuc)
            ELSE
               WRITE(1, '(3f10.5)')0., 0., 0.
            ENDIF
C-----------write(1,'(3f10.5)') wd,rwd,awd
            IF(POTe(2).NE.0.)THEN
               WRITE(1, '(3f10.5)')POTe(2), RWOm(1, Nejc, Nnuc),
     &                             AWOm(Nejc, Nnuc)
            ELSE
               WRITE(1, '(3f10.5)')0., 0., 0.
            ENDIF
C-----------write(1,'(3f10.5)') vvso,rrvso,aavso
            IF(POTe(4).NE.0.)THEN
               WRITE(1, '(3f10.5)')POTe(4), RVSo(1, Nejc, Nnuc),
     &                             AVSo(Nejc, Nnuc)
            ELSE
               WRITE(1, '(3f10.5)')0., 0., 0.
            ENDIF
C-----------write(1,'(3f10.5)') wwso,rwso,awso
            IF(WSO(1, Nejc, Nnuc).NE.0.)THEN
               WRITE(1, '(3f10.5)')WSO(1, Nejc, Nnuc),
     &                             RVSo(1, Nejc, Nnuc), AVSo(Nejc, Nnuc)
            ELSE
               WRITE(1, '(3f10.5)')0., 0., 0.
            ENDIF
C-----------write(1,'(3f10.5)') rc,0.,0.
            WRITE(1, '(3f10.5)')RCOul(Nejc, Nnuc), 0., 0.
            WRITE(1, '(3f10.5)')0., 0., 0.
         ENDIF
      ENDDO
C
C-----Angular distribution step
C
      WRITE(1, '(3f10.5)')0.D0, angstep, 180.D0
      WRITE(1, '(3hFIN)')
      CLOSE(UNIT = 1)
C-----Running ECIS
      IF(npho.GT.0)THEN
        CALL ECIS('ecVIBROT.inp','ECIS_VIBROT.out#')
      ELSE
        CALL ECIS('ecVIBROT.inp','ECIS_ROT.out#')
      ENDIF

      END
C
      SUBROUTINE SETPOTS(Nejc, Nnuc, Eilab, Eicms, Mi, Mt, Rrmu, Ak2,
     &                   Ikey)
C
C     Transfers o.m. parameters to ECIS
C
      INCLUDE 'dimension.h'
C
      INCLUDE 'global.h'
C
C
      DOUBLE PRECISION AAE, AQ, BETa, POT, R, RCOulomb, RRE
      DOUBLE PRECISION Rrmu, Ak2, Mi, Mt
      INTEGER i
C
C     Dummy arguments
C
      DOUBLE PRECISION Eilab, Eicms
      INTEGER Nejc, Nnuc
C
C     COMMON variables
C
C
      COMMON /POTEN1/ R(7), RRE(7), AQ(7), AAE(7), POT(7, 6)
      COMMON /POTEN2/ RCOulomb, BETa, EFErmi, EP, EA, IREl
C
C     Ener must be in LAB system
C
C     ener = Elab
C
      IF(CCCalc)THEN
C-----calculate o.m. parameters for ejectile NEJC on target NNUC at energy ENER
         komp = 33
         CALL OMPAR(Nejc, Nnuc, Eilab, Eicms, Mi, Mt, Rrmu, Ak2, komp,
     &              Ikey)
      ELSE
C-----calculate o.m. parameters for ejectile NEJC on target NNUC at energy ENER
         komp = 29
         CALL OMPAR(Nejc, Nnuc, Eilab, Eicms, Mi, Mt, Rrmu, Ak2, komp,
     &              Ikey)
      ENDIF
C-----set OMP onto EMPIRE variables
C     Energy dependence (if any) must be considered in OMPAR
C     Setting potential strength
      POTe(1) = VOM(1, Nejc, Nnuc)
      POTe(7) = VOMs(1, Nejc, Nnuc)
      POTe(2) = WOMs(1, Nejc, Nnuc)
      POTe(3) = WOMv(1, Nejc, Nnuc)
      POTe(4) = VSO(1, Nejc, Nnuc)
      POTe(6) = WSO(1, Nejc, Nnuc)
      DO i = 1, 7
         POT(i, 1) = POTe(i)
      ENDDO
C     Setting geometrical parameters
      R(1) = RVOm(1, Nejc, Nnuc)
      AQ(1) = AVOm(Nejc, Nnuc)
      R(2) = RWOm(1, Nejc, Nnuc)
      IF(SFIom(Nejc, Nnuc).LT.0.0D0)R(2) = -R(2)
      AQ(2) = AWOm(Nejc, Nnuc)
      R(7) = R(2)
      AQ(7) = AQ(2)
      R(3) = RWOmv(1, Nejc, Nnuc)
      AQ(3) = AWOmv(Nejc, Nnuc)
      R(4) = RVSo(1, Nejc, Nnuc)
      AQ(4) = AVSo(Nejc, Nnuc)
      R(6) = RWSo(1, Nejc, Nnuc)
      AQ(6) = AWSo(Nejc, Nnuc)
      RCOulomb = RCOul(Nejc, Nnuc)
      BETa = RNOnl(Nejc, Nnuc)
      EFErmi = EEFermi(Nejc, Nnuc)
C     EP = EEP(Nejc, Nnuc)
C     EA = EEA(Nejc, Nnuc)
      IREl = IRElat(Nejc, Nnuc)
      END
C
      SUBROUTINE KINEMA(EL, E1, Mi, Mt, Amu, Ak2, Iopt, Relcal)
C
C     Author: O.Bersillon (SCAT2000)
C
C***********************************************************************
C  Kinematics:   lab  <===>  CM                                        *
C    With relativistic kinematics, the reduced mass is replaced by     *
C    the reduced total energy                                          *
C----------------------------------------------------------------------*
C  EL     = current lab kinetic energy                                 *
C  E1     = current  CM kinetic energy                                 *
C  MI     = incident particle rest mass (in a.m.u.)                    *
C  MT     = target   nucleus  rest mass (in a.m.u.)                    *
C  AMU    = reduced mass                                               *
C  AK2    = CM wave number                                             *
C  IOPT   = 1   from lab to CM                                         *
C           2   from CM  to lab                                        *
C  Relcal = FALSE classical    kinematics                              *
C           TRUE  relativistic kinematics                              *
C----------------------------------------------------------------------*
C  AMUmev = a.m.u. in MeV                                              *
C----------------------------------------------------------------------*
C***********************************************************************
C
      IMPLICIT DOUBLE PRECISION(A - H, O - Z)
C
      DOUBLE PRECISION Mi, Mt, mtot
      LOGICAL Relcal
C
      COMMON /CONSTANT/ AMUmev, PI, W2L, XNExc, CETa, CSO, RMU, AMPi,
     &                  ELE2, HHBarc, AMUneu, AMUpro
C
      DATA one/1.0D+00/
      DATA two/2.0D+00/
C
C=======================================================================
C
      ck2 = (two*AMUmev)/(HHBarc**2)
C
      mtot = Mi + Mt
C
      IF(Iopt.EQ.1)THEN
C
C***********************************************************************
C        From lab to CM (the input quantity is Elab)
C***********************************************************************
C
         IF(.NOT.Relcal)THEN
C
C-----------------------------------------------------------------------
C           Classical    kinematics
C-----------------------------------------------------------------------
C
            Amu = Mi*Mt/mtot
            E1 = EL*Mt/mtot
            w2 = ck2*Amu
            Ak2 = w2*E1
         ELSE
C
C-----------------------------------------------------------------------
C           Relativistic kinematics
C-----------------------------------------------------------------------
C
C           E1 = DSQRT(mtot*mtot + two*Mt*El) - mtot
            E1 = AMUmev*mtot*
     &           (DSQRT(one + two*EL/(AMUmev*Mt*((one+Mi/Mt)**2)))
     &           - one)
            p2 = (EL*(EL + two*AMUmev*Mi))
     &           /((one + Mi/Mt)**2 + two*EL/(AMUmev*Mt))
            Ak2 = p2/(HHBarc*HHBarc)
            etoti = DSQRT((AMUmev*Mi)**2 + p2)
            etott = DSQRT((AMUmev*Mt)**2 + p2)
            Amu = etoti*etott/(etoti + etott)
            Amu = Amu/AMUmev
         ENDIF
C
C
C
C***********************************************************************
C        From  CM to lab (the input quantity is Ecm)
C***********************************************************************
C
      ELSEIF(.NOT.Relcal)THEN
C
C-----------------------------------------------------------------------
C        Classical    kinematics
C-----------------------------------------------------------------------
C
         Amu = Mi*Mt/mtot
         EL = E1*mtot/Mt
         Ak2 = ck2*Amu*E1
      ELSE
C
C-----------------------------------------------------------------------
C        Relativistic kinematics
C-----------------------------------------------------------------------
C
         EL = E1*(E1 + two*AMUmev*mtot)/(two*AMUmev*Mt)
         p2 = E1*(E1 + two*AMUmev*mtot)*(E1 + two*AMUmev*Mi)
     &        *(E1 + two*AMUmev*Mt)/((two*(E1+AMUmev*mtot))**2)
         Ak2 = p2/(HHBarc*HHBarc)
         etoti = DSQRT((AMUmev*Mi)**2 + p2)
         etott = DSQRT((AMUmev*Mt)**2 + p2)
         Amu = etoti*etott/(etoti + etott)
         Amu = Amu/AMUmev
C
C
      ENDIF
C
      END
C
C     From om-retrieve.f (RIPL interface)
C
C
      subroutine optmod(el,vlib,rlib,alib)
c
c     Routine to generate input for ECIS96 from RIPL-2 library
c
      include "ripl2empire.h"
      real*8 el,rlib,alib,vlib
      dimension rlib(6),alib(6),vlib(6),b(6,ndim1,15)
c     include "om-retrieve-cts.cmb"
c
c     To use dispersive optical model package
c
c             functions
      real*8 DOM_int_Ws,DOM_int_Wv,DOM_int_T1,DOM_int_T2,Vhf
      real*8 DOM_int,WVf,WDf,Delta_WD,Delta_WV
c             variables
      real*8 As,Bs,Cs,AAv,Bv,AAvso,Bvso,EEE,Ep,Ea,Ef
      real*8 alpha_PB,beta_PB,gamma_PB,Vnonl
      real*8 DWS,DWV,DWVso,DerDWV,DerDWS,dtmp
      real*8 WDE,WVE
      integer n,iq,nns
      common /energy/EEE,Ef,Ep
      common /Wenerg/WDE,WVE
      common /pdatas/As,Bs,Cs,nns,iq
      common /pdatav/AAv,Bv,n
c     Nonlocality constants alpha fixed according to Mahaux 1991
      real*8 AlphaV
      data AlphaV/1.65d0/
c     real*8 AlphaS
c     data AlphaS/0.235d0/
      external Delta_WV,WVf,Delta_WD,WDf
cc
c     Generate optical model parameters for ECIS
c
      pi=acos(-1.d0)
      rc=0.
      if(jcoul.lt.1)go to 194
      jc=1
      do 190 j=1,jcoul
      if(el.gt.ecoul(j)) jc=j+1
 190  continue
      jc=min0(jc,jcoul)
      rc=rcoul0(jc)*atar**(-1./3.) + rcoul(jc) +
     +   rcoul1(jc)*atar**(-2./3.) + rcoul2(jc)*atar**(-5./3.)
 194  encoul2=0.
      if(rc.gt.0.) encoul2=1.73*ztar/(rc*atar**(1./3.))
c
      do 300 i=1,6
      rlib(i)=0.
      alib(i)=0.
      vlib(i)=0.
      jab=iabs(jrange(i))
      if(jrange(i).lt.1)go to 300
      jp=1
      do 204 j=1,jab
      if(el.gt.epot(i,j)) jp=j+1
 204  continue
      j=min0(jp,jab)
      Ef=dble(int(100000*EFErmi))/100000
C
C     Ef must not be zero if pot(i,j,18)=0 !!!
C
C
C     The IF block should be added on upgrade from the om-retrieve code
      if(pot(i,j,18).ne.0.) then
        Ef=pot(i,j,18) + pot(i,j,19)*atar
        EFErmi = Ef
      endif
      elf = el - Ef
c
c     Calculate radius and diffuseness parameters
      if(rco(i,j,13).eq.0.) then
        rlib(i)=abs(rco(i,j,1)) + rco(i,j,3)*eta
     *       + rco(i,j,4)/atar + rco(i,j,5)/sqrt(atar)
     *       + rco(i,j,6)*atar**(2./3.) + rco(i,j,7)*atar
     *       + rco(i,j,8)*atar**2  + rco(i,j,9)*atar**3
     *       + rco(i,j,10)*atar**(1./3.)
     *       + rco(i,j,11)*atar**(-1./3.)
C--------------------------------------------------------------------
C     RCN, 08/2004, to handle new extension to the OMP RIPL-2 format
     *       + rco(i,j,2)*el
     *       + rco(i,j,12)*el*el
      else
C     RCN, 09/2004, to handle new extension to the OMP RIPL-2 format
        nn = int(rco(i,j,7))
        rlib(i)= ( abs(rco(i,j,1)) + rco(i,j,2)*atar ) *
     *           ( 1.d0 - ( rco(i,j,3) + rco(i,j,4)*atar ) * elf**nn/
     *           ( elf**nn + ( rco(i,j,5) + rco(i,j,6)*atar )**nn ) )
      endif
      alib(i)=abs(aco(i,j,1)) + aco(i,j,2)*el + aco(i,j,3)*eta
     *        + aco(i,j,4)/atar + aco(i,j,5)/sqrt(atar)
     *        + aco(i,j,6)*atar**(2./3.) + aco(i,j,7)*atar
     *        + aco(i,j,8)*atar**2 + aco(i,j,9)*atar**3
     *        + aco(i,j,10)*atar**(1./3.) + aco(i,j,11)*atar**(-1./3.)
c
c
      if (pot(i,j,24).eq.0.) go to 210
c
c     Special Koning-type potential formulas
c
      if (pot(i,j,24).eq.1.) then
c
c       Koning-type formulas
c
        elf = el - Ef
        if(i.eq.1) call bcoget(b,j)
        vc=0.
        if(i.eq.1 .and. b(1,j,5).ne.0.) vc =
     +  b(1,j,1)*encoul2*( b(1,j,2) - 2.*b(1,j,3)*elf +
     +  3.*b(1,j,4)*elf**2 + b(i,j,14)*b(i,j,13)*exp(-b(i,j,14)*elf) )
        nn = int(pot(i,j,13))
c       Retrieving average energy of the particle states Ep
        Ep=Ef
        if( (i.eq.2) .or. (i.eq.4) )
     >     Ep=dble(int(100000*pot(i,j,20)))/100000
        if(Ep.eq.0.) Ep=Ef
        elf = el - Ep
        iq=1
        if(i.eq.4 .and. b(4,j,12).gt.0.) iq=nint(b(4,j,12))
        vlib(i)=
     +    b(i,j,1)*( b(i,j,15) - b(i,j,2)*elf + b(i,j,3)*elf**2 -
     +    b(i,j,4)*elf**3 + b(i,j,13)*exp(-b(i,j,14)*elf) ) +
     +    b(i,j,5)*vc + b(i,j,6)*(elf**nn/(elf**nn + b(i,j,7)**nn)) +
     +    b(i,j,8)*exp(-b(i,j,9)*elf**iq)*(elf**nn/
     +    (elf**nn + b(i,j,10)**nn)) +
     +    b(i,j,11)*exp(-b(i,j,12)*elf)
      endif
      if (pot(i,j,24).eq.2.) then
c
c       Morillon-Romain formulas
c
        elf = el - Ef
        if(i.eq.1) call bcoget(b,j)
c       Vhf(E) calculated from nonlocal approximation
c          as suggested by Perey and Buck
c
        alpha_PB = dble(int(100000*b(i,j,1)))/100000
        beta_PB  = dble(int(100000*b(i,j,2)))/100000
        gamma_PB = dble(int(100000*b(i,j,3)))/100000
        EEE = dble(int(1000000*el))/1000000
        iq=1
        if(i.eq.4 .and. b(4,j,12).gt.0.) iq=nint(b(4,j,12))
        vc=0.
        Vnonl = 0.
        if(i.eq.1) then
          Vnonl = -Vhf(EEE,alpha_PB,beta_PB,gamma_PB)
C         Numerical derivative of the Vhf
          if(b(1,j,5).ne.0.) then
            Vnonlm = -Vhf(EEE-0.05,alpha_PB,beta_PB,gamma_PB)
            Vnonlp = -Vhf(EEE+0.05,alpha_PB,beta_PB,gamma_PB)
C           Coulomb correction for Hartree-Fock potential
            vc = encoul2*(Vnonlm-Vnonlp)*10.d0
          endif
        endif
        vlib(i)=
     +    Vnonl + b(i,j,5)*vc +
     +    b(i,j,6)*(elf**nn/(elf**nn + b(i,j,7)**nn)) +
     +    b(i,j,8)*exp(-b(i,j,9)*elf**iq)*(elf**nn/
     +    (elf**nn + b(i,j,10)**nn)) +
     +    b(i,j,11)*exp(-b(i,j,12)*elf)
      endif
c
c     Nonlocality consideration
c
c     Retrieving energy above which nonlocality in the volume absorptive
c               potential is considered (Ea)
c
      Ea=dble(int(100000*pot(i,j,21)))/100000
      if(Ea.eq.0.) Ea=1000.1d0
      if(i.eq.2 .and. Ea.lt.1000. .and. el.gt.(Ef+Ea) )
     +         vlib(i)= vlib(i) +
     +       AlphaV*(sqrt(el)+(Ef+Ea)**1.5/(2.*el)-1.5*sqrt(Ef+Ea))
c
      go to 300
c
 210  if (pot(i,j,23).eq.0.) go to 220
c
c     Special Varner-type potential formulas
      vlib(i)= (pot(i,j,1) + pot(i,j,2)*eta)/
     *    (1.+ exp((pot(i,j,3) - el + pot(i,j,4)*encoul2)/pot(i,j,5)))
      if(pot(i,j,6).eq.0.)go to 300
      vlib(i) = vlib(i)
     *    + pot(i,j,6)*exp((pot(i,j,7)*el - pot(i,j,8))/pot(i,j,6))
      go to 300
c
 220  if (pot(i,j,22).eq.0.) go to 230
c
c     Special Smith-type potential formulas
      vlib(i)=pot(i,j,1) + pot(i,j,2)*eta
     *    + pot(i,j,6)*exp(pot(i,j,7)*el + pot(i,j,8)*el*el)
     *    + pot(i,j,9)*el*exp(pot(i,j,10)*el**pot(i,j,11))
      if(pot(i,j,5).ne.0.)vlib(i)=vlib(i)
     *    + pot(i,j,3)*cos(2.*pi*(atar - pot(i,j,4))/pot(i,j,5))
      go to 300
c
c     Standard potential formulas
 230  vlib(i)=pot(i,j,1) + pot(i,j,7)*eta + pot(i,j,8)*encoul
     *    + pot(i,j,9)*atar + pot(i,j,10)*atar**(1./3.)
     *    + pot(i,j,11)*atar**(-2./3.) + pot(i,j,12)*encoul2
     *    + (pot(i,j,2) + pot(i,j,13)*eta + pot(i,j,14)*atar)*el
     *    + pot(i,j,3)*el*el + pot(i,j,4)*el*el*el + pot(i,j,6)*sqrt(el)
      if(el.gt.0.) vlib(i) = vlib(i) + pot(i,j,17)*encoul/el**2
     *    + (pot(i,j,5) + pot(i,j,15)*eta + pot(i,j,16)*el)*dlog(el)
 300  continue
c
c     To calculate dispersion relation contribution
c
 152  if(abs(idr).ge.2) then
c
c       Exact calculation of the dispersive contribution
c
        EEE = dble(int(1000000*el))/1000000
        i=2
c       Only one energy range
        j=1
c       Real volume contribution from Dispersive relation
        DWV=0.d0
c
        if(pot(2,1,24).ne.0) then
          AAv=dble(int(100000*b(i,j,6)))/100000
          Bv=dble(int(100000*b(i,j,7)))/100000
          n = nint( pot(i,j,13) )
          if(n.eq.0 .or. mod(n,2).eq.1)
     +      stop 'Zero or odd exponent in Wv(E) for dispersive OMP'
c     Retrieving average energy of the particle states Ep
          Ep=dble(int(100000*pot(i,j,20)))/100000
          if(Ep.eq.0.) Ep=Ef
c         if(idr.ge.2) then
c           analytical DOM integral
            DWV=DOM_INT_Wv(Ef,Ep,AAv,Bv,EEE,n,DerDWV)
            DWVder = 0.d0
c           coulomb correction for real volume potential
            if(b(1,1,5).ne.0.d0) DerDWV = -b(1,1,5)*encoul2*DerDWV
            if(b(1,1,5).ne.0.d0) then
              DWVp = DOM_INT_Wv(Ef,Ep,AAv,Bv,EEE+0.1d0,n,dtmp)
              DWVm = DOM_INT_Wv(Ef,Ep,AAv,Bv,EEE-0.1d0,n,dtmp)
              DWVder = b(1,1,5)*encoul2*(DWVm-DWVp)*5.d0
            endif
c         endif
c         if(idr.le.-2) then
c           numerical DOM integral (not needed for a time being)
c           WVE=WVf(AAv,Bv,Ep,Ef,EEE,n)
c           DWV=2*DOM_int(Delta_WV,WVf,Ef,Ef+5.*Bv,150000.d0,EEE,0.d0)
c         endif
c
c         Nonlocality correction to the DOM integral
c           (only used if Ea is non-zero)
c
          Ea=dble(int(100000*pot(i,j,21)))/100000
          if(Ea.eq.0.) Ea=1000.1d0
          Dwplus = 0.d0
          Dwmin = 0.d0
          T12der = 0.d0
          if(Ea.lt.1000.) THEN
             Dwplus = AlphaV*DOM_INT_T2(Ef,Ea,EEE)
             dtmp1 = Wvf(AAv,Bv,Ep,Ef,Ef+Ea,n)
             Dwmin = dtmp1*DOM_INT_T1(Ef,Ea,EEE)
             DWV = DWV + Dwplus + Dwmin
c            coulomb correction for nonlocal dispersive contribution
c                to real volume potential
              if(b(1,1,5).ne.0.d0) then
               if(eee.ne.0.05d0) then
                 T2p = DOM_INT_T2(Ef,Ea,EEE+0.05d0)
                 T2m = DOM_INT_T2(Ef,Ea,EEE-0.05d0)
                     T2der =  b(1,1,5)*encoul2*AlphaV*(T2m-T2p)*10.d0
                 T1p = DOM_INT_T1(Ef,Ea,EEE+0.05d0)
                 T1m = DOM_INT_T1(Ef,Ea,EEE-0.05d0)
                 T1der = b(1,1,5)*encoul2*dtmp1*(T1m-T1p)*10.d0
                 T12der =  T1der + T2der
                   else
                 T2p = DOM_INT_T2(Ef,Ea,EEE+0.1d0)
                 T2m = DOM_INT_T2(Ef,Ea,EEE-0.1d0)
                     T2der =  b(1,1,5)*encoul2*AlphaV*(T2m-T2p)*5.d0
                 T1p = DOM_INT_T1(Ef,Ea,EEE+0.1d0)
                 T1m = DOM_INT_T1(Ef,Ea,EEE-0.1d0)
                 T1der = b(1,1,5)*encoul2*dtmp1*(T1m-T1p)*5.d0
                 T12der =  T1der + T2der
               endif
             endif
          endif
        endif
        i=4
c       Only one energy range
        j=1
c       Real surface contribution from Dispersive relation
        DWS=0.d0
        if(pot(4,1,24).ne.0) then
          As=dble(int(100000*b(i,j,8)))/100000
          Bs=dble(int(100000*b(i,j,10)))/100000
          Cs=dble(int(100000*b(i,j,9)))/100000
          n = nint( pot(i,j,13) )
          if(n.eq.0 .or. mod(n,2).eq.1)
     +      stop 'Zero or odd exponent in Wd(E) for dispersive OMP'
          iq=1
          if(b(4,j,12).gt.0.) iq=nint(b(4,j,12))
c         Retrieving average energy of the particle states Ep
          Ep=dble(int(100000*pot(i,j,20)))/100000
          if(Ep.eq.0.) Ep=Ef
C         Ea=dble(int(100000*pot(i,j,21)))/100000
C         if(Ea.eq.0.) Ea=1000.1d0
          if(idr.ge.2) then
c           analytical DOM integral
            DWS = DOM_INT_Ws(Ef,Ep,As,Bs,Cs,EEE,n,DerDWS)
            DWSder = 0.d0
c           Coulomb correction for real surface potential
            if(b(1,1,5).ne.0.d0) DerDWS = -b(1,1,5)*encoul2*DerDWS
            if(b(1,1,5).ne.0.d0) then
              DWSp = DOM_INT_Ws(Ef,Ep,As,Bs,Cs,EEE+0.1d0,n,dtmp)
              DWSm = DOM_INT_Ws(Ef,Ep,As,Bs,Cs,EEE-0.1d0,n,dtmp)
              DWSder = b(1,1,5)*encoul2*(DWSm-DWSp)*5.d0
            endif
          endif
          if(idr.le.-2) then
c           numerical DOM integral
            nns=n
            WDE=WDf(As,Bs,Cs,Ep,EEE,n,iq)
            DWS = 2*DOM_int(Delta_WD,WDf,Ef,Ef+30.d0,2000.d0,EEE,WDE)
            DWSder = 0.d0
c           Coulomb correction for real surface potential
            if(b(1,1,5).ne.0.d0) then
              WDE=WDf(As,Bs,Cs,Ep,EEE+0.1d0,n,iq)
              DWSp =
     &         2*DOM_int(Delta_WD,WDf,Ef,Ef+30.d0,2000.d0,EEE+0.1d0,WDE)
              WDE=WDf(As,Bs,Cs,Ep,EEE-0.1d0,n,iq)
              DWSm =
     &         2*DOM_int(Delta_WD,WDf,Ef,Ef+30.d0,2000.d0,EEE-0.1d0,WDE)
c             Numerical derivative
              DWSder = b(1,1,5)*encoul2*(DWSm-DWSp)*5.d0
            endif
          endif
        endif
        i=6
c       Only one energy range
        j=1
c       Real spin orbit contribution from Dispersive relation
        DWVso=0.d0
c
        if(pot(6,1,24).ne.0 .and. abs(idr).eq.3) then
          AAvso=dble(int(100000*b(i,j,6)))/100000
          Bvso=dble(int(100000*b(i,j,7)))/100000
          n = nint( pot(i,j,13) )
          if(n.eq.0 .or. mod(n,2).eq.1)
     +      stop 'Zero or odd exponent in Wso(E) for dispersive OMP'
c         analytical DOM integral
          DWVso=DOM_INT_Wv(Ef,Ef,AAvso,Bvso,EEE,n,dtmp)
        endif
c       Adding real volume dispersive contribution to the real potential
c       Geometry parameters are the same as for the volume
c       potential(imag and real).
        Vhfnum = vlib(1)
        vlib(1)= vlib(1)+dwv+DWVder+T12der
c       Including real surface dispersive contribution
c       Geometry parameters are the same as for the imaginary surface
c       potential.
        vlib(3)=dws+DWSder
        alib(3)=alib(4)
        rlib(3)=rlib(4)
c       Adding real spin orbit dispersive contribution to the real spin orbit potential
c       Geometry parameters are the same as for the imaginary spin orbit
c       potential(imag and real)
        vlib(5) = vlib(5) + DWVso
        write (25,'(1x,I3,1x,I2,1x,F7.3,3x,6(f6.3,1x,f4.2,1x,f4.2))')
     &      nint(atar),nint(ztar), el, (vlib(i),rlib(i),alib(i),i=1,6)
      endif
      return
      end
c
      subroutine setr (a,b,n)
c     ******************************************************************
c     set all elements of array b(n) to real number a.
c     ******************************************************************
      dimension b(n)
      do 100 k=1,n
  100 b(k)=a
      return
      end
c
      subroutine bcoget(b,j)
c
c     Routine to compute b coefficients for Koning global potential
c
c     Modified by R.Capote to extend RIPL format
c
      include "ripl2empire.h"
      dimension b(6,ndim1,15)
c
       call setr(0.,b,90*ndim1)
c      Original Koning dependence
       b(1,j,1)  =  pot(1,j,1) + pot(1,j,2)*atar + pot(1,j,8)*eta
       if((pot(1,j,20).ne.0.) .and.
     +    (pot(1,j,14) + pot(1,j,15)*atar + pot(1,j,16)).ne.0. ) then
c        Soukhovitski dependence
         b(1,j,1)  =  pot(1,j,1) + pot(1,j,2)*atar + pot(1,j,8)*eta +
     +                pot(1,j,20)*eta/
     +               (pot(1,j,14) + pot(1,j,15)*atar + pot(1,j,16))
       endif
       b(1,j,2)  =  pot(1,j,3) + pot(1,j,4)*atar
       b(1,j,3)  =  pot(1,j,5) + pot(1,j,6)*atar
       b(1,j,4)  =  pot(1,j,7)
       b(1,j,5)  =  pot(1,j,9)
       b(1,j,11) =  pot(1,j,10) + pot(1,j,11)*atar
       b(1,j,12) =  pot(1,j,12)
c      b coefficients from 13 to 15 added for Soukhovitski potential
c      V^DISP_R
       b(1,j,13)  =  pot(1,j,16)
c      Lambda_R
       b(1,j,14)  =  pot(1,j,17)
c      V^0_R + V^A_R*(A-232)
       b(1,j,15)  =  pot(1,j,14) + pot(1,j,15)*atar
c      To preserve compatibility with RIPL-2 Koning database
c      b(i,j,15) must be equal to 1. !!! for Koning OMP
       if((pot(1,j,14) + pot(1,j,15)*atar + pot(1,j,16)).eq.0.)
     >          b(1,j,15) = 1.
C      if(abs(b(1,j,15)).lt.1.e-8) b(1,j,15) = 1.
c      Wv( Av )
       b(2,j,6)  =  pot(2,j,1) + pot(2,j,2)*atar
c      Wv( Bv )
       b(2,j,7)  =  pot(2,j,3) + pot(2,j,4)*atar
c      Wd
c
c      added A dependence for As parameter (RCN, 09/2004), i.e.  pot(4,j,7)<>0
c      Wd( As )
c      b(4,j,8)  =  pot(4,j,1) + pot(4,j,8)*eta
       b(4,j,8)  =  pot(4,j,1) + pot(4,j,8)*eta + pot(4,j,7)*atar
c      Wd( Cs )
       if(pot(4,j,3).ne.0.) then
         b(4,j,9)  =  pot(4,j,2) +
     +                pot(4,j,3)/(1.+ exp((atar-pot(4,j,4))/pot(4,j,5)))
       else
         b(4,j,9)  =  pot(4,j,2)
       endif
c      Wd( Bs )
       b(4,j,10) =  pot(4,j,6)
C      Wd( q )
       b(4,j,12) =  pot(4,j,12)
c      Vso
       b(5,j,11) =  pot(5,j,10) + pot(5,j,11)*atar
       b(5,j,12) =  pot(5,j,12)
c      Wso
       b(6,j,6)  =  pot(6,j,1)
       b(6,j,7)  =  pot(6,j,3)
      return
      end
      REAL*8 FUNCTION Vhf(einp,alpha_PB,beta_PB,gamma_PB)
c
c     According to Morillon B, Romain P, PRC70(2004)014601
c
c     Originally coded in c++ by Morillon B. and Romain P.
c
c     Coded in FORTRAN and tested by RCN, August 2004.
c
      real*8 einp,alpha_PB,beta_PB,gamma_PB,amu
      real*8 Vtmp,Etmp,miu_sur_hbar2, coef1, coef2
      integer niter
      include "ripl2empire.h"
c     getting amu
      xtmp=xkine(sngl(einp),amu)
      miu_sur_hbar2 = amu / hbarc**2
      coef1 = -0.5d0 * beta_PB**2 * miu_sur_hbar2
      coef2 =  4.0d0 * (gamma_PB * miu_sur_hbar2)**2
      niter = 0.
      Vhf = -45.d0
10    niter = niter + 1
      Vtmp = Vhf
      Etmp = einp - Vtmp
      Vhf = alpha_PB * dexp(coef1 * Etmp + coef2 * Etmp**2)
      if( abs(Vhf - Vtmp) .GT. 0.0001 .AND.  niter.LT.10000) goto 10
      return
      end
      REAL FUNCTION xkine(ei,amu)
c***********************************************************************
c     From lab to CM (the input quantity is el = Elab)
c***********************************************************************
c     RCN 08/2004, xkine calculated by relativistic kinematics when needed
      include "ripl2empire.h"
      real*8 amu
      mtot = (tarmas+projmas)
      if(irel .eq. 0) then
c
c-----------------------------------------------------------------------
c  Classical    kinematics (energy independent amu and xkine)
c-----------------------------------------------------------------------
c
          amu = projmas*tarmas / mtot * amu0c2
          xkine = tarmas / mtot
c         e1  = el*xkine
c         w2  = ck2*amu
c         ak2 = w2*e1
      else
c
c-----------------------------------------------------------------------
c  Relativistic kinematics
c-----------------------------------------------------------------------
c
c         e1  = amu0c2*mtot*
c    * (        DSQRT(1.d0 +
c    *       2.d0*el/(amu0c2*tarmas*((1.d0+projmas/tarmas)**2))) - 1.d0)
          p2  = (ei*(ei + 2.d0*amu0c2*projmas)) /
     *          ((1.d0+projmas/tarmas)**2 + 2.d0*ei/(amu0c2*tarmas))
c         ak2 = p2 / (hbarc*hbarc)
          etoti = DSQRT((amu0c2*projmas)**2 + p2)
          etott = DSQRT((amu0c2*tarmas)**2  + p2)
          amu   = etoti*etott / (etoti + etott)
c         amu   = amu / amu0c2
          xkine = etott / (etoti + etott)
      endif
      return
      end
C
C==========================================================================
C     AUTHOR: Dr. Roberto Capote Noy
c
C     e-mail: rcapotenoy@yahoo.com; r.capotenoy@iaea.org
C
C     DISPERSIVE OPTICAL MODEL POTENTIAL PACKAGE
C
c     Analytical dispersive integrals are included
c     see Quesada JM et al, Computer Physics Communications 153(2003) 97
c                           Phys. Rev. C67(2003) 067601
C
      REAL*8 FUNCTION DOM_INT_Wv(Ef,Ep,Av,Bv,E,n,DerivIntWv)
C
C      Analytical dispersive integral and its derivative for
C      Wv(E)=Av*(E-Ep)**n/( (E-Ep)**n + Bv**n )  for E>Ep
C      Wv(E)=Wv(2*Ef-E)                          for E<2Ef-Ep
C      Wv(E)=0                                     OTHERWISE
C
      IMPLICIT NONE
      REAL*8 Ef,Ep,Av,Bv,E,pi
      REAL*8 E0,Ex,Eplus,Emin,Rs,ResEmin,ResEplus
      REAL*8 DerEmin, DerEplus, Rds, DerivIntWv
      COMPLEX*16 Pj,I,Zj,Ztmp
      COMPLEX*8 Fs,Ds
      INTEGER N,j
      DATA I/(0.d0,1.d0)/
      pi=4.d0*atan(1.d0)
      E0 = Ep - Ef
      Ex = E  - Ef
      Eplus = Ex + E0
      Emin  = Ex - E0
      DOM_INT_Wv = 0.d0
      DerivIntWv = 0.d0
      ResEmin  =  Emin**n / (Emin**n + Bv**n)
      DerEmin  =  Emin**(n-1) *
     &           ( Emin**n + Bv**n*(1.d0 + n*log(dabs(Emin)) ) )
     &           / (Emin**n + Bv**n)**2
      ResEplus = -Eplus**n / (Eplus**n + Bv**n)
      DerEplus = -Eplus**(n-1) *
     &           ( Eplus**n + Bv**n*(1.d0+n*log(Eplus)) )
     &           / (Eplus**n + Bv**n)**2
C----------------------------------
C     Complex arithmetic follows
      Fs = (0.d0,0.d0)
      Ds = (0.d0,0.d0)
      do j=1,n
       Ztmp = I*(2*j-1)/dble(n)*pi
       Pj = Bv*exp(Ztmp)
       Zj = Pj * (2*Pj +Eplus -Emin) * Ex
       Zj = Zj / ( (Pj+E0) * (Pj+Eplus) * (Pj-Emin) )
       Fs = Fs + Zj*log(-Pj)
       Ds = Ds + 2*Pj*(Ex*Ex + (Pj+E0)**2)*log(-Pj)
     &           /( (Pj+Eplus)**2 * (Pj-Emin)**2 )
      enddo
      IF(ABS(IMAG(Fs)).gt.1.e-4) STOP '(F) Too big imag part in Wv'
      Rs  = REAL(Fs)
      IF(ABS(IMAG(Ds)).gt.1.e-4) STOP '(D) Too big imag part in Wv'
      Rds = REAL(Ds)
C----------------------------------
      DOM_INT_Wv = -Av/pi*
     &  (Rs/n  + ResEplus*log(Eplus) + ResEmin*log(dabs(Emin)))
      DerivIntWv = -Av/pi*( Rds/n + DerEplus + DerEmin)
      RETURN
      END
      REAL*8 FUNCTION DOM_INT_Ws(Ef,Ep,As,Bs,Cs,E,m,DerivIntWs)
C
C      Analytical dispersive integral and its derivative for
C      Ws(E)=As*(E-Ep)**m/( (E-Ep)**m + Bs**m ) * exp(-Cs*(E-Ep)) for E>Ep
C      Ws(E)=Ws(2*Ef-E)                                           for E<2Ef-Ep
C      Ws(E)=0                                                    OTHERWISE
C
      IMPLICIT NONE
      REAL*8 Ef,Ep,As,Bs,Cs,E,EIn
      COMPLEX*16 I,Pj,Zj,Ztmp,zfi
      COMPLEX*8 Fs,Ds
      REAL*8 E0,Ex,Eplus,Emin,pi
      REAL*8 Rs,ResEmin,ResEplus
      REAL*8 DerivIntWs,DerEmin,DerEplus,Rds
      INTEGER m,j
      DATA I/(0.d0,1.d0)/
      pi=4.d0*atan(1.d0)
      E0 = Ep - Ef
      Ex = E  - Ef
      Eplus = Ex + E0
      Emin  = Ex - E0
      DOM_INT_Ws = 0.d0
      DerivIntWs = 0.d0
      ResEmin  =  Emin**m / (Emin**m + Bs**m)
      DerEmin  = -Emin**(m-1) *
     &          ( Emin**m + Bs**m + ( -Cs*Emin**(m+1) +
     &            Bs**m *(-Cs*Emin+m) ) * exp(-Cs*Emin)*EIn(Cs*Emin) )
     &           / (Emin**m + Bs**m)**2
      ResEplus = -Eplus**m / (Eplus**m + Bs**m)
      DerEplus =  Eplus**(m-1) *
     &          ( Eplus**m + Bs**m + ( Cs*Eplus**(m+1) +
     &            Bs**m *(Cs*Eplus+m) ) * exp(Cs*Eplus)*EIn(-Cs*Eplus) )
     &           / (Eplus**m + Bs**m)**2
C----------------------------------
C     Complex arithmetic follows
      Fs = (0.d0,0.d0)
      Ds = (0.d0,0.d0)
      do j=1,m
       Ztmp = I*(2*j-1)/dble(m)*pi
       Pj = Bs*exp(Ztmp)
       Zj = Pj * (2*Pj +Eplus -Emin) * Ex
       Zj = Zj / (Pj+E0) / (Pj+Eplus) / (Pj-Emin)
       Fs = Fs + Zj* zfi(-Pj*Cs)
       Ds = Ds + 2*Pj*(Ex*Ex + (Pj+E0)**2)*zfi(-Pj*Cs)
     &           /( (Pj+Eplus)**2 * (Pj-Emin)**2 )
      enddo
      IF(ABS(IMAG(Fs)).gt.1.e-4) STOP '(F) Too big imag part in Wv'
      Rs  = REAL(Fs)
      IF(ABS(IMAG(Ds)).gt.1.e-4) STOP '(D) Too big imag part in Wv'
      Rds = REAL(Ds)
C----------------------------------
      DOM_INT_Ws = As/pi*(Rs/m
     &                  - ResEplus*exp(Cs*Eplus)*EIn(-Cs*Eplus)
     &                  - ResEmin*exp(-Cs*Emin)*EIn(Cs*Emin) )
      DerivIntWs = As/pi*( Rds/m + DerEplus + DerEmin)
      RETURN
      END
      real*8 function WV(A,B,Ep,Ef,E,n)
      IMPLICIT NONE
      real*8 A,B,Ep,Ef,E,ee
      integer n
      WV=0.d0
      if(E.LE.Ef) E=2.d0*Ef-E
      if(E.LT.Ep) return
      ee=(E-Ep)**n
      WV=A*ee/(ee+B**n)
      return
      end
      real*8 function WDD(A,B,C,Ep,Ef,E,m)
      IMPLICIT NONE
      real*8 A,B,C,Ep,Ef,E,ee,arg
      integer m
      WDD=0.d0
      if(E.LE.Ef) E=2.d0*Ef-E
      if(E.LT.Ep) return
      arg=C*(E-Ep)
      IF(arg.GT.15) return
      ee=(E-Ep)**m
      WDD=A*ee/(ee+B**m)*EXP(-arg)
      return
      end
      REAL*8 FUNCTION DOM_int_T1(Ef,Ea,E)
C
C     Integral over E' corresponding to nonlocal additions T1(E'<<0)
C
      IMPLICIT NONE
      real*8 E,Ea,Ef,Ex,Ea2,Eax,Pi,T11,T12,T13
      Pi=4.d0*ATAN(1.d0)
      Ex=E-Ef
      Ea2=Ea**2
      Eax=Ex+Ea
      T11 = 0.5d0*log(Ea)/Ex
      T12 =  ( (2*Ea+Ex)*log(Ea)+0.5d0*pi*Ex )
     &      /(2.*(Eax**2 + Ea2))
      T13 = -Eax**2*log(Eax)/(Ex*(Eax**2+Ea2))
      DOM_int_T1 = Ex/Pi*(T11+T12+T13)
C
      RETURN
      END
C
      REAL*8 FUNCTION DOM_int_T2(Ef,Ea,E)
C
C     Integral over E' corresponding to nonlocal additions T2(E'>>0)
C
      IMPLICIT REAL*8(A-H,O-Z)
      Pi=4.d0*ATAN(1.d0)
      El=Ef+Ea
      R1=1.5*DSQRT(El)*dLOG(abs((El-E)/Ea))
      IF(abs(E).LT.1.d-6) THEN
        R2=0.5*El**1.5d0*(1.d0/El-dlog(abs(El/Ea))/Ef)
      ELSE
        R2=0.5*El**1.5d0/(E*Ef)*
     &            ( Ef*dlog(dabs(El/(El-E))) -E*dlog(dabs(El/Ea)) )
      ENDIF
      R3=2*DSQRT(dABS(Ef))*( 0.5d0*Pi - atan( DSQRT(El/dabs(Ef)) ) )
      IF(E.GE.0.d0) THEN
        R4=DSQRT(E)*
     &           dlog(dabs( (dsqrt(El)+dsqrt(E))/(dsqrt(El)-dsqrt(E)) ))
      ELSE
        R4=-2.d0*DSQRT(dabs(E))*( 0.5d0*Pi - atan( DSQRT(dabs(El/E)) ) )
      ENDIF
      DOM_int_T2 =  1.d0/Pi*(R1+R2+R3+R4)
      RETURN
      END
C
C-----FUNCTION TO EVALUATE exp(Z)*E1(Z)
C
      complex*16 function zfi(za)
C
C Complex exponential integral function multiplied by exponential
C
C AUTHOR: J. Raynal
C
      IMPLICIT NONE
      real*8 aj
      complex*16 za,y
      integer m,i
      zfi=0.d0
      if (za.eq.0.) return
      if (dabs(dreal(za)+18.5d0).ge.25.d0) go to 3
      if (dsqrt(625.d0-(dreal(za)+18.5d0)**2)/1.665d0.lt.dabs(dimag(za))
     &) go to 3
      zfi=-.57721566490153d0-cdlog(za)
      y=1.d0
      do 1 m=1,2000
      aj=m
      y=-y*za/aj
      if (cdabs(y).lt.1.d-15*cdabs(zfi)) go to 2
    1 zfi=zfi-y/aj
    2 zfi=cdexp(za)*zfi
      return
    3 do 4 i=1,20
      aj=21-i
      zfi=aj/(za+zfi)
    4 zfi=aj/(1.d0+zfi)
      zfi=1.d0/(zfi+za)
      return
      end
C
C-----FUNCTION TO EVALUATE Ei(X)
C
      REAL*8 FUNCTION EIn(X)
      IMPLICIT NONE
      REAL*8 FAC, H, X
      INTEGER N
      EIn = 0.57721566490153d0+LOG(ABS(X))
      FAC = 1.0
      DO N = 1,100
      H = FLOAT(N)
      FAC = FAC*H
      EIn = EIn + X**N/(H*FAC)
      ENDDO
      RETURN
      END
c*******************************************
      real*8 function DELTA_WV(WVf,y)
      real*8 E,Ef,A,B,Ep,y,WVf,WDE,WVE
      integer n
      common /energy/E,Ef,Ep
      common /Wenerg/WDE,WVE
      common /pdatav/A,B,n
C
      DELTA_WV=(WVf(A,B,Ep,y,n) - WVE)
     &         /((y-Ef)**2-(E-Ef)**2)
      return
      end
C
      real*8 function WVf(A,B,Ep,Ef,E,n)
      real*8 A,B,Ep,E,Ef
      integer n
C
      WVf=0.d0
      if(E.LE.Ef) E=2.d0*Ef-E
      if(E.LE.Ep) return
C
      ee=(E-Ep)**n
      WVf=A*ee/(ee+B**n)
C
      return
      end
C
      real*8 function DELTA_WD(WDf,y)
      real*8 E,Ef,A,B,C,Ep,y,WDf,WDE,WVE
      integer m,iq
      common /energy/E,Ef,Ep
      common /Wenerg/WDE,WVE
      common /pdatas/A,B,C,m,iq
C
      DELTA_WD=(WDf(A,B,C,Ep,y,m,iq) - WDE)
     &            /((y-Ef)**2-(E-Ef)**2)
C
      return
      end
C
      real*8 function WDf(A,B,C,Ep,E,m,iq)
      real*8 A,B,C,Ep,E,ee,arg
      integer m,iq
C
      WDf=0.d0
      if(E.Lt.Ep) return
      arg=C*(E-Ep)**iq
      IF(arg.GT.15) return
      ee=(E-Ep)**m
      WDf=A*ee/(ee+B**m)*EXP(-arg)
      return
      end
C
      REAL*8 FUNCTION DOM_int(DELTAF,F,Ef,Eint,Ecut,E,WE_cte)
C
C     DOM integral (20 points Gauss-Legendre)
C
C     Divided in two intervals for higher accuracy
C     The first interval corresponds to peak of the integrand
C
      DOUBLE PRECISION Eint,Ef,Ecut,WE_cte,E
      DOUBLE PRECISION F,WG,XG,WWW,XXX,DELTAF
      DOUBLE PRECISION ABSC1,CENTR1,HLGTH1,RESG1
      DOUBLE PRECISION ABSC2,CENTR2,HLGTH2,RESG2
      INTEGER J
      EXTERNAL F
      DIMENSION XG(10),WG(10)
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
      DATA WG  (  1) / 0.0176140071 3915211831 1861962351 853 D0 /
      DATA WG  (  2) / 0.0406014298 0038694133 1039952274 932 D0 /
      DATA WG  (  3) / 0.0626720483 3410906356 9506535187 042 D0 /
      DATA WG  (  4) / 0.0832767415 7670474872 4758143222 046 D0 /
      DATA WG  (  5) / 0.1019301198 1724043503 6750135480 350 D0 /
      DATA WG  (  6) / 0.1181945319 6151841731 2377377711 382 D0 /
      DATA WG  (  7) / 0.1316886384 4917662689 8494499748 163 D0 /
      DATA WG  (  8) / 0.1420961093 1838205132 9298325067 165 D0 /
      DATA WG  (  9) / 0.1491729864 7260374678 7828737001 969 D0 /
      DATA WG  ( 10) / 0.1527533871 3072585069 8084331955 098 D0 /
C
      DATA XG( 1) / 0.9931285991 8509492478 6122388471 320 D0 /
      DATA XG( 2) / 0.9639719272 7791379126 7666131197 277 D0 /
      DATA XG( 3) / 0.9122344282 5132590586 7752441203 298 D0 /
      DATA XG( 4) / 0.8391169718 2221882339 4529061701 521 D0 /
      DATA XG( 5) / 0.7463319064 6015079261 4305070355 642 D0 /
      DATA XG( 6) / 0.6360536807 2651502545 2836696226 286 D0 /
      DATA XG( 7) / 0.5108670019 5082709800 4364050955 251 D0 /
      DATA XG( 8) / 0.3737060887 1541956067 2548177024 927 D0 /
      DATA XG( 9) / 0.2277858511 4164507808 0496195368 575 D0 /
      DATA XG(10) / 0.0765265211 3349733375 4640409398 838 D0 /
C
      CENTR1 = 0.5D+00*(Ef+Eint)
      HLGTH1 = 0.5D+00*(Eint-Ef)
      CENTR2 = 0.5D+00*(Ecut+Eint)
      HLGTH2 = 0.5D+00*(Ecut-Eint)
C
C     COMPUTE THE 20-POINT GAUSS-KRONROD APPROXIMATION
C     TO THE INTEGRAL in TWO INTERVALS (Ef - Eint, Eint - Ecut)
C
      RESG1 = 0.0D+00
      RESG2 = 0.0D+00
      DO J=1,10
      XXX=XG(J)
      WWW=WG(J)
        ABSC1 = HLGTH1*XXX
      RESG1=RESG1 + WWW*(DELTAF(F,CENTR1-ABSC1)+DELTAF(F,CENTR1+ABSC1))
        ABSC2 = HLGTH2*XXX
      RESG2=RESG2 + WWW*(DELTAF(F,CENTR2-ABSC2)+DELTAF(F,CENTR2+ABSC2))
      ENDDO
c
      CORR=0.5d0*WE_cte/(E-Ef)*dlog((Ecut-(E-Ef))/(Ecut+(E-Ef)))
      DOM_int = ( RESG1*HLGTH1+RESG2*HLGTH2 + CORR) *
     >           (E-Ef)/(acos(-1.d0))
c
      RETURN
      END
