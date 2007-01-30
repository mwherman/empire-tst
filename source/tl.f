Ccc   * $Author: Capote $
Ccc   * $Date: 2007-01-30 11:12:23 $
Ccc   * $Id: tl.f,v 1.86 2007-01-30 11:12:23 Capote Exp $

      SUBROUTINE HITL(Stl)
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
Ccc   ************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C COMMON variables
C
      DOUBLE PRECISION A0R, AU, ETAk, HC, RAB, REDm, V0R
      COMMON /ONE   / V0R, A0R, RAB, REDm, AU, HC, ETAk
C
C Dummy arguments
C
      DOUBLE PRECISION Stl(NDLW)
C
C Local variables
C
      DOUBLE PRECISION arg, clf, homega, ra, rb, rbar, rred, xfu, xfum
      LOGICAL distrb
      INTEGER i
      DOUBLE PRECISION XFUS
      distrb = .FALSE.
C-----if critical l given in input jump directly to Tl calculations
      IF (CRL.LE.0.D0) THEN
C-----CCFUS calculations
         IF (CSRead.EQ.( - 2.D0)) THEN
            CALL CCFUS(Stl)
            RETURN
         ENDIF
C--------CCFUS calculations  *** done ****
C--------check for distribution barrier
         IF (CSRead.EQ.( - 1.D0)) distrb = .TRUE.
C--------calculate projectile+target binding energy if not done already
         IF (Q(0,1).EQ.0.D0) CALL BNDG(0,1,Q(0,1))
         IF (distrb) THEN
            IF (BFUs.EQ.0.0D0) THEN
C--------------calculate fusion barrier using CCFUS routine BAR
               ra = 1.233*AEJc(0)**(1./3.) - 0.978/AEJc(0)**(1./3.)
               rb = 1.233*A(0)**(1./3.) - 0.978/A(0)**(1./3.)
               RAB = ra + rb + 0.29
               rred = ra*rb/(ra + rb)
               REDm = AEJc(0)*A(0)/(AEJc(0) + A(0))
               AU = AMUmev
               HC = HHBarc
               V0R = 30.08*(1. - 1.8*(1. - 2.*ZEJc(0)/AEJc(0))
     &               *(1. - 2.*Z(0)/A(0)))*rred + DV - 20.
               A0R = 0.63
               ETAk = 1.43997*ZEJc(0)*Z(0)
               CALL BAR(rbar,BFUs,homega)
               WRITE (6,*) 'Fusion barrier is ', BFUs, ' MeV'
            ENDIF
            IF (SIG.EQ.0.0D0) SIG = 0.05*BFUs
            IF (IOUt.GT.0) THEN
               WRITE (6,*) 'Distributed fusion barrier with extra push='
     &                     , EXPush
               WRITE (6,*) 'SIG=', SIG, ' and TRUNC=', TRUnc,
     &                     ' has been used'
            ENDIF
            CALL PUSH(EIN,A(1),AEJc(0),A(0),BFUs,EXPush,SIG,TRUnc,Stl,
     &                NLW,NDLW)
            RETURN
         ENDIF
C--------calculation of fusion Tl's with distributed barrier model
C        *** done ***
C--------prepare starting values for searching critical l
         IF (CSRead.LE.0.0D0) THEN
            clf = CRL - 2.0*DFUs
            IF (CRL - clf.LT.3.D0) clf = CRL - 3.
            IF (clf.LT.0.D0) clf = 10.
         ELSE
            CSFus = CSRead
            clf = 1.0
         ENDIF
         xfum = 0.0
   50    xfu = XFUS(EIN,AEJc(0),A(0),DFUs,clf)
         IF (xfu.LT.CSFus) THEN
            xfum = xfu
            clf = clf + 1.
            GOTO 50
         ELSE
            CRL = clf - 1 + (CSFus - xfum)/(xfu - xfum)
         ENDIF
         NLW = max(CRL + MAX(5.D0,5.0D0*DFUs),float(NDLW-2))
C-----setting transmission coefficients for fusion if not distr. barr.
      ENDIF
      DO i = 1, NDLW
         arg = (CRL - i + 1)/DFUs
         arg = MIN(174.0D0,arg)
         Stl(i) = 1.0/(1.0 + EXP((-arg)))
      ENDDO
      END


      SUBROUTINE RIPL2EMPIRE(Nejc,Nnuc,E)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C-----For dispersive optical model potentials
C-----It is ripl2empireS.h because it must be compatible
C-----with global.h declarations so some variables must be renamed
      INCLUDE 'ripl2empireS.h'
C
C COMMON variables
C
      INTEGER MODelcc
      COMMON /LOCAL / MODelcc
C
C Dummy arguments
C
      DOUBLE PRECISION E
      INTEGER Nejc, Nnuc
C
C Local variables
C
      DOUBLE PRECISION alib(6), rlib(6), vlib(6), xmas_nejc, xmas_nnuc
      CHARACTER*80 ch_iuf
      LOGICAL coll_defined
      CHARACTER*132 ctmp
      REAL FLOAT
      INTEGER iainp, izinp, k, n, ncalc, nld_cc
      INTEGER*4 iwin
      INTEGER*4 PIPE
C
C-----Sets CC optical model parameters according to RIPL
C
C-----E must be in lab system !!!
C
      DATA coll_defined/.FALSE./
      iainp = A(Nnuc)
      izinp = Z(Nnuc)
C
C-----SETTING COLLECTIVE LEVELS for DIRECT CALCULATIONS
C
      MODelcc = 0
      IF (DIRect.EQ.0) IMOdel = 0
C-----IF ( IMOdel.EQ.0 ) model = 'spherical nucleus model'
C-----IF ( IMOdel.EQ.1 ) model = 'coupled-channels rotational model'
C-----IF ( IMOdel.EQ.2 ) model = 'vibrational model'
C-----IF ( IMOdel.EQ.3 ) model = 'non-axial deformed model'
      IF (IMOdel.EQ.3 .AND. FIRst_ein) THEN
         WRITE (6,*) 'WARNING: Non-axial deformed model not implemented'
         IWArn = 5
         GOTO 300
      ENDIF
      IF (IMOdel.EQ.1 .OR. IMOdel.EQ.2) THEN
C--------Imodel not used for non-inelastic channels
         IF (iainp.NE.A(0) .OR. izinp.NE.Z(0) .OR. AEJc(Nejc).NE.AEJc(0)
     &       .OR. ZEJc(Nejc).NE.ZEJc(0)) GOTO 300
      ENDIF
      MODelcc = IMOdel
      IF (ND_nlv.GT.0 .AND. NISotop.EQ.0) coll_defined = .TRUE.
      IF (IMOdel.EQ.1 .AND. (.NOT.coll_defined)) THEN
C
C--------model = 'coupled-channels rotational model'
C
         coll_defined = .TRUE.
         IF (NISotop.EQ.0 .AND. FIRst_ein) THEN
            WRITE (6,*) 'WARNING: None of the requested isotopes is '
            WRITE (6,*)
     &               'WARNING: included in the selected RIPL potential.'
            WRITE (6,*)
     &                 'WARNING: Default collective levels will be used'
            GOTO 300
         ENDIF
         ncalc = 0
         DO n = 1, NISotop
            IF (iainp.EQ.IA(n) .AND. izinp.EQ.IZ(n)) THEN
               ncalc = n
               IF (IDEf(n).GT.2*LMAx(n) .OR. NCOll(n).GT.NDCOLLEV) THEN
                  WRITE (6,*) 'WARNING: OMP collective levels are wrong'
                  WRITE (6,*) 'WARNING: Too many levels or deformations'
                  WRITE (6,*)
     &                 'WARNING: Default collective levels will be used'
                  IWArn = 6
                  GOTO 300
               ENDIF
            ENDIF
         ENDDO
         IF (ncalc.EQ.0) THEN
            WRITE (6,*)
     &                'WARNING: RIPL OMP collective levels are not used'
            WRITE (6,*)
     &                 'WARNING: Default collective levels will be used'
            GOTO 300
         ENDIF
         IF (NCOll(ncalc).EQ.0) THEN
            WRITE (6,*)
     &                'WARNING: RIPL OMP collective levels are not used'
            WRITE (6,*)
     &                 'WARNING: Default collective levels will be used'
            GOTO 300
         ENDIF
C--------Setting EMPIRE global variables
         nld_cc = 0
         DO k = 1, ND_nlv
            IF (ICOllev(k).LT.LEVcc) nld_cc = nld_cc + 1
         ENDDO
         WRITE (6,*)
         WRITE (6,*)
         WRITE (6,*) 'Deformation of the gsb adopted from CC RIPL OMP'
         LMAxcc = LMAx(ncalc)
         IDEfcc = IDEf(ncalc)
         DO k = 2, IDEfcc, 2
            D_Def(1,k) = DDEf(ncalc,k)
         ENDDO
         IF (nld_cc.NE.NCOll(ncalc)) THEN
            WRITE (6,*) 'WARNING: Default number of coupled levels: ',
     &                  nld_cc
            WRITE (6,*) 'WARNING: is not equal ', NCOll(ncalc),
     &                  ' (used in CC RIPL OMP)'
         ENDIF
C
C--------Joining TARGET_COLL.DAT and TARGET_COLL_RIPL.DAT files
C
         OPEN (32,FILE = 'TARGET_COLL_RIPL.DAT')
         OPEN (97,FILE = 'TARGET_COLL.DAT')
         OPEN (96,FILE = 'COLL.DAT')
         WRITE (6,*)
     &       'Collective levels from RIPL CC OMP, symm.rotational model'
         WRITE (32,*)
     &       'Collective levels from RIPL CC OMP, symm.rotational model'
         READ (97,'(A80)') ch_iuf   ! FIRST LINE
         WRITE (96,'(A80)') ch_iuf
         WRITE (6,*) 'Dyn.deformations are not used in symm.rot.model'
         WRITE (32,*) 'Dyn.deformations are not used in symm.rot.model'
         READ (97,'(A80)') ch_iuf       ! 2ND LINE
         WRITE (96,'(A80)') ch_iuf
         WRITE (6,'(1x,i3,1x,i3,a35)') izinp, iainp,
     &                                 ' nucleus is treated as deformed'
         WRITE (32,'(1x,i3,1x,i3,a35)') izinp, iainp,
     &                                 ' nucleus is treated as deformed'
         READ (97,'(A80)') ch_iuf       ! 3ER LINE
         WRITE (96,'(A80)') ch_iuf
         WRITE (6,*)
         WRITE (32,*)
         READ (97,'(A80)') ch_iuf       ! EMPTY LINE
         WRITE (96,'(A80)') ch_iuf
         DEFormed = .TRUE.
         DO n = 1, NISotop
            IF (iainp.EQ.IA(n) .AND. izinp.EQ.IZ(n)) THEN
               WRITE (6,*)
     &                 '   Ncoll  Lmax IDef  Kgs  (Def(1,j),j=2,IDef,2)'
               WRITE (32,*)
     &                 '   Ncoll  Lmax IDef  Kgs  (Def(1,j),j=2,IDef,2)'
               READ (97,'(A80)') ch_iuf
               WRITE (96,*)
     &                 '   Ncoll  Lmax IDef  Kgs  (Def(1,j),j=2,IDef,2)'
               WRITE (6,'(3x,3I5,1x,F5.1,1x,6(e10.3,1x))') NCOll(n),
     &                LMAx(n), IDEf(n), BANdk(n),
     &                (DDEf(n,k),k = 2,IDEf(n),2)
               WRITE (32,'(3x,3I5,1x,F5.1,1x,6(e10.3,1x))') NCOll(n),
     &                LMAx(n), IDEf(n), BANdk(n),
     &                (DDEf(n,k),k = 2,IDEf(n),2)
               READ (97,'(A80)') ch_iuf
               WRITE (96,'(3x,3I5,1x,F5.1,1x,6(e10.3,1x))') ND_nlv,
     &                LMAx(n), IDEf(n), BANdk(n),
     &                (DDEf(n,k),k = 2,IDEf(n),2)
               WRITE (6,*)
               WRITE (32,*)
               READ (97,'(A80)') ch_iuf
               WRITE (96,*)
               WRITE (6,*) ' N   E[MeV]  J   pi Nph L  K  Dyn.Def.'
               WRITE (32,*) ' N   E[MeV]  J   pi Nph L  K  Dyn.Def.'
               READ (97,'(A80)') ch_iuf
               WRITE (96,*) ' N   E[MeV]  J   pi Nph L  K  Dyn.Def.'
            ENDIF
         ENDDO
   50    READ (97,'(A80)',END = 100) ch_iuf
         WRITE (96,'(A80)') ch_iuf
         GOTO 50
  100    CLOSE (96)
         CLOSE (97,STATUS = 'DELETE')
         IF (IOPsys.EQ.0) THEN
            ctmp = 'mv COLL.DAT TARGET_COLL.DAT'
            iwin = PIPE(ctmp)
         ELSE
            iwin = PIPE('move COLL.DAT TARGET_COLL.DAT')
         ENDIF
C
C--------JOIN finished: TARGET_COLL.DAT and TARGET_COLL_RIPL.DAT files
C
         DO k = 1, NCOll(ncalc)
C-----------The deformation for excited levels is not used in the pure
C-----------symm.rotational model but could be used for vibrational
C-----------rotational model so we are setting it to 0.01
            WRITE (32,
     &             '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3)')
     &             k, EEX(k,ncalc), SPIn(k,ncalc), FLOAT(IPAr(k,ncalc)),
     &             0, 0, 0, 0.01
            WRITE (6,'(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3)'
     &             ) k, EEX(k,ncalc), SPIn(k,ncalc),
     &               FLOAT(IPAr(k,ncalc)), 0, 0, 0, 0.01
         ENDDO
         CLOSE (32)
         WRITE (6,*)
         WRITE (6,*)
      ENDIF
      IF (IMOdel.EQ.2 .AND. (.NOT.coll_defined)) THEN
C
C--------model = 'vibrational model'
C
         coll_defined = .TRUE.
         IF (NISotop.EQ.0) THEN
            WRITE (6,*) 'WARNING: None of the requested isotopes is '
            WRITE (6,*) 'WARNING: Included in the selected potential.'
            WRITE (6,*)
     &                 'WARNING: File with RIPL discrete levels can not'
            WRITE (6,*) 'WARNING: be created.                    '
            WRITE (6,*)
     &                 'WARNING: Default collective levels will be used'
            GOTO 300
         ENDIF
         ncalc = 0
         DO n = 1, NISotop
            IF (iainp.EQ.IA(n) .AND. izinp.EQ.IZ(n)) THEN
               ncalc = n
               DO k = 2, NVIb(ncalc)
                  IF (NPH(k,ncalc).EQ.3) THEN
                     IWArn = 6
                     WRITE (6,*) 'NPH(k,i)=3 !!! in RIPL OMP'
                     WRITE (6,*)
     &                          'Default collective levels will be used'
                     GOTO 300
                  ENDIF
               ENDDO
               IF (NVIb(n).GT.NDCOLLEV) THEN
                  WRITE (6,*) 'RIPL OMP collective levels are wrong'
                  WRITE (6,*) 'Too many levels'
                  WRITE (6,*) 'Default collective levels will be used'
                  IWArn = 6
                  GOTO 300
               ENDIF
            ENDIF
         ENDDO
         IF (ncalc.EQ.0) THEN
            WRITE (6,*) 'RIPL OMP collective levels are not used'
            WRITE (6,*) 'Default collective levels will be used'
            GOTO 300
         ENDIF
         IF (NCOll(ncalc).EQ.0) THEN
            WRITE (6,*) 'RIPL OMP collective levels are not used'
            WRITE (6,*) 'Default collective levels will be used'
            GOTO 300
         ENDIF
         nld_cc = 0
         DO k = 1, ND_nlv
            IF (ICOllev(k).LT.LEVcc) nld_cc = nld_cc + 1
         ENDDO
         IF (nld_cc.NE.NVIb(ncalc)) THEN
            WRITE (6,*) 'WARNING: Default number of coupled levels: ',
     &                  nld_cc
            WRITE (6,*) 'WARNING: is not equal ', NVIb(ncalc),
     &                  ' (used in CC RIPL OMP)'
         ENDIF
         WRITE (6,*)
         WRITE (6,*)
C
C--------Joining TARGET_COLL.DAT and TARGET_COLL_RIPL.DAT files
C
         OPEN (32,FILE = 'TARGET_COLL_RIPL.DAT')
         OPEN (97,FILE = 'TARGET_COLL.DAT')
         OPEN (96,FILE = 'COLL.DAT')
         WRITE (6,*)
     &           'Collective levels from RIPL CC OMP, vibrational model'
         WRITE (32,*)
     &           'Collective levels from RIPL CC OMP, vibrational model'
         READ (97,'(A80)') ch_iuf   ! FIRST LINE
         WRITE (96,'(A80)') ch_iuf
         WRITE (6,*) 'Dynamical deformations should be adjusted'
         WRITE (32,*) 'Dynamical deformations should be adjusted'
         READ (97,'(A80)') ch_iuf       ! 2ND LINE
         WRITE (96,'(A80)') 'Dynamical deformations should be adjusted'
         WRITE (6,'(1x,i3,1x,i3,a35)') izinp, iainp,
     &                                ' nucleus is treated as spherical'
         WRITE (32,'(1x,i3,1x,i3,a35)') izinp, iainp,
     &                                ' nucleus is treated as spherical'
         READ (97,'(A80)') ch_iuf       ! 3ER LINE
         WRITE (96,'(A80)') ch_iuf
         WRITE (6,*)
         WRITE (32,*)
         READ (97,'(A80)') ch_iuf       ! EMPTY LINE
         WRITE (96,'(A80)') ch_iuf
         DEFormed = .FALSE.
         DO n = 1, NISotop
            IF (iainp.EQ.IA(n) .AND. izinp.EQ.IZ(n)) THEN
               WRITE (6,*) '   Ncoll'
               WRITE (32,*) '   Ncoll'
               READ (97,'(A80)') ch_iuf
               WRITE (96,*) '   Ncoll'
               WRITE (6,'(3x,3I5,1x,F5.1,1x,6(e10.3,1x))') NCOll(n)
               WRITE (32,'(3x,3I5,1x,F5.1,1x,6(e10.3,1x))') NCOll(n)
               READ (97,'(A80)') ch_iuf
               WRITE (96,'(A80)') ND_nlv
               WRITE (6,*)
               WRITE (32,*)
               READ (97,'(A80)') ch_iuf
               WRITE (96,*)
               WRITE (6,*) ' N   E[MeV]  J   pi Nph L  K  Dyn.Def.'
               WRITE (32,*) ' N   E[MeV]  J   pi Nph L  K  Dyn.Def.'
               READ (97,'(A80)') ch_iuf
               WRITE (96,*) ' N   E[MeV]  J   pi Nph L  K  Dyn.Def.'
            ENDIF
         ENDDO
  150    READ (97,'(A80)',END = 200) ch_iuf
         WRITE (96,'(A80)') ch_iuf
         GOTO 150
  200    CLOSE (96)
         CLOSE (97,STATUS = 'DELETE')
         IF (IOPsys.EQ.0) THEN
            ctmp = 'mv COLL.DAT TARGET_COLL.DAT'
            iwin = PIPE(ctmp)
         ELSE
            iwin = PIPE('move COLL.DAT TARGET_COLL.DAT')
         ENDIF
C
C--------JOIN finished: TARGET_COLL.DAT and TARGET_COLL_RIPL.DAT files
C
         DO k = 1, NVIb(ncalc)
            WRITE (32,
     &             '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3)')
     &             k, EXV(k,ncalc), SPInv(k,ncalc), FLOAT(IPAr(k,ncalc))
     &             , NPH(k,ncalc), 0, 0, DEFv(k,ncalc)
            WRITE (6,'(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3)'
     &             ) k, EXV(k,ncalc), SPInv(k,ncalc),
     &               FLOAT(IPAr(k,ncalc)), NPH(k,ncalc), 0, 0,
     &               DEFv(k,ncalc)
         ENDDO
         CLOSE (32)
         WRITE (6,*)
         WRITE (6,*)
      ENDIF
C
C-----END OF SETTING COLLECTIVE LEVELS for DIRECT CALCULATIONS
C
  300 IF (iainp.LT.IAMin .OR. iainp.GT.IAMax) IWArn = 1
      IF (izinp.LT.IZMin .OR. izinp.GT.IZMax) IWArn = 2
      IF (E.LT.EEMin) THEN
         EEMin = E + 0.1D0
         IWArn = 3
      ENDIF
      IF (E.GT.EEMax) THEN
         EEMax = E + 1000.D0
         IWArn = 4
      ENDIF
      OMEmin(Nejc,Nnuc) = EEMin
      OMEmax(Nejc,Nnuc) = EEMax
C     IRElat(Nejc,Nnuc) = IREl
      xmas_nejc = (AEJc(Nejc)*AMUmev + XMAss_ej(Nejc))/AMUmev
      xmas_nnuc = (A(Nnuc)*AMUmev + XMAss(Nnuc))/AMUmev
C
C-----INITIALIZING /RIPLXX
C
      ETA = (XN(Nnuc) - Z(Nnuc))/A(Nnuc)
      ATAr = iainp
      ZTAr = izinp
      TARmas = xmas_nnuc
      PROjmas = xmas_nejc
      HBArc = HHBarc
      AMU0c2 = AMUmev
      ENCoul = 0.4*ZTAr/ATAr**(1./3.)
      EFErmi = EEFermi(Nejc,Nnuc)

      CALL OPTMOD(E,vlib,rlib,alib)
C
C-----RIPL-II
C-----1. The ordering of potentials has been changed to follow ECIS96:
C
C-----i=1         real volume potential
C-----i=2         imaginary volume potential
C-----i=3         real surface derivative potential
C-----i=4         imaginary surface derivative potential
C-----i=5         real spin-orbit potential
C-----i=6         imaginary spin-orbit potential
C
C-----2. The number of terms possible in the potential strengths has
C-----been increased from 21 to 24.  The special case potentials
C-----are now:
C
C-----pot(i,j,22) - Smith et al. , OMP reference # 118
C-----pot(i,j,23) - Varner et al., OMP reference # 2100, 5100
C-----pot(i,j,24) - Koning potentials, reference # 2404, 2405 ...
C
      RCOul(Nejc,Nnuc) = RC
      ACOul(Nejc,Nnuc) = ACOu
C-----Volume real potential: Woods-Saxon
      VOM(Nejc,Nnuc) = vlib(1)*FNvvomp(Nejc,Nnuc)
      RVOm(Nejc,Nnuc) = rlib(1)*FNrvomp(Nejc,Nnuc)
      AVOm(Nejc,Nnuc) = alib(1)*FNavomp(Nejc,Nnuc)
C-----Volume imaginary potential: Woods-Saxon
      WOMv(Nejc,Nnuc) = vlib(2)*FNwvomp(Nejc,Nnuc)
      RWOmv(Nejc,Nnuc) = rlib(2)*FNrwvomp(Nejc,Nnuc)
      AWOmv(Nejc,Nnuc) = alib(2)*FNavomp(Nejc,Nnuc)
C-----Real surface contribution
      VOMs(Nejc,Nnuc) = vlib(3)*FNwsomp(Nejc,Nnuc)
C-----Surface imaginary potential:
      WOMs(Nejc,Nnuc) = vlib(4)*FNwsomp(Nejc,Nnuc)
      RWOm(Nejc,Nnuc) = rlib(4)*FNrsomp(Nejc,Nnuc)
      AWOm(Nejc,Nnuc) = alib(4)*FNasomp(Nejc,Nnuc)
      SFIom(Nejc,Nnuc) = 1.D0
C-----if rco(4,1,1) >0.0: Woods-Saxon derivative surface potential
C-----if rco(4,1,1) <0.0: Gaussian surface potential.
      IF (RCO(4,1,1).LT.0.0) SFIom(Nejc,Nnuc) = -1.D0
C-----Real spin-orbit
C-----****
      VSO(Nejc,Nnuc) = vlib(5)
      RVSo(Nejc,Nnuc) = rlib(5)
      AVSo(Nejc,Nnuc) = alib(5)
C-----Imaginary spin-orbit
C-----****
      WSO(Nejc,Nnuc) = vlib(6)
      RWSo(Nejc,Nnuc) = rlib(6)
      AWSo(Nejc,Nnuc) = alib(6)
      END


      SUBROUTINE OMPAR(Nejc,Nnuc,Eilab,Eicms,Mi,Mt,Ak2,Komp,Ikey)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C COMMON variables
C
      INTEGER MODelcc
      COMMON /LOCAL / MODelcc
C
C Dummy arguments
C
      DOUBLE PRECISION Ak2, Eicms, Eilab, Mi, Mt
      INTEGER Ikey, Komp, Nejc, Nnuc
C
C Local variables
C
      DOUBLE PRECISION ener
      LOGICAL fexist, relcal
      INTEGER iaejcr, ianucr, ieof, ierr, iowrite, ipoten, irel, ki
      INTEGER INT
      CHARACTER*2 symbejcr, symbnucr
C
C-----IKEY < 0  :   EIlab is given
C-----IKEY > 0  :   EIcms is given
C
C-----Sets optical model parameters
C
C-----komp   = 29 (usually), 33(for the inelastic channel)
C
C-----Erasing parameters of O.M.P.
      VOM(Nejc,Nnuc)  = 0.D0
      VOMs(Nejc,Nnuc) = 0.D0
      WOMv(Nejc,Nnuc) = 0.D0
      WOMs(Nejc,Nnuc) = 0.D0
      WSO(Nejc,Nnuc)  = 0.D0
      VSO(Nejc,Nnuc)  = 0.D0
      RVOm(Nejc,Nnuc) = 0.D0
      RWOm(Nejc,Nnuc) = 0.D0
      RVSo(Nejc,Nnuc) = 0.D0
      RWSo(Nejc,Nnuc) = 0.D0
      AVOm(Nejc,Nnuc) = 0.D0
      AWOm(Nejc,Nnuc) = 0.D0
      AVSo(Nejc,Nnuc) = 0.D0
      AWSo(Nejc,Nnuc) = 0.D0
C-----set nonlocality range to 0. (as usually is the case)
      RNOnl(Nejc,Nnuc) = 0.D0
C-----set validity range to any energy (can be modified later)
      OMEmin(Nejc,Nnuc) = 0.D0
      OMEmax(Nejc,Nnuc) = 1000.D0
C-----set relativistic calculation key to 0
      IRElat(Nejc,Nnuc) = 0
      fexist = OMPar_riplf
      IF (CCCalc) fexist = OMParfcc
C-----OMPAR.RIPL file exists ?
      IF (fexist) THEN
C--------komp = 29 OR 33
         ipoten = KTRlom(Nejc,Nnuc)
         CALL FINDPOT_OMPAR_RIPL(Komp,ieof,ipoten,Nnuc,Nejc)
         IF (ieof.EQ.0) THEN
C-----------Reading potential parameters from OMPAR.RIPL(OMPAR.DIR) file
            CALL READ_OMPAR_RIPL(Komp,ierr,irel)
            IRElat(Nejc,Nnuc) = irel
            IF(Komp.eq.33) IRElat(0,0) = irel
            IF (ierr.EQ.0) GOTO 100
         ENDIF
C--------ieof<>0 or ierr<>0
         IF (IOUT.EQ.5) THEN
           WRITE (6,*) ' '
           IF (CCCalc) THEN
             WRITE (6,
     &'('' OM parameters for '',I3,''-'',A2,'' +'',I2,''-''        ,A2,'
     &' not found in the OMPAR.DIR file'')') INT(A(Nnuc)), SYMb(Nnuc),
     &INT(AEJc(Nejc)), SYMbe(Nejc)
           ELSE
             WRITE (6,
     &'('' OM parameters for '',I3,''-'',A2,'' +'',I2,''-''        ,A2,'
     &' not found in the OMPAR.RIPL file'')') INT(A(Nnuc)), SYMb(Nnuc),
     &INT(AEJc(Nejc)), SYMbe(Nejc)
           ENDIF
           WRITE (6,*) 'I will resort to parameters from RIPL database'
           WRITE (6,*) ' '
         ENDIF
      ENDIF
C
C-----OMPAR.RIPL(OMPAR.DIR) file does not exists
C-----or reading error happened
C-----(reading from the RIPL database)
C
      ipoten = KTRlom(Nejc,Nnuc)
      ki = 26
C-----Searching in the RIPL database for IPOTEN catalog number
      CALL FINDPOT(ki,ieof,ipoten)
C-----Here ieof must be 0 always because we checked in input.f
      IF (ieof.GT.0) THEN
         WRITE (6,*) 'ERROR: PROBLEM with OMPAR.DIR library,RIPL #',
     &               ipoten
         STOP 'ERROR: PROBLEM with OMPAR.DIR library'
      ENDIF
C-----Reading o.m.  potential parameters for IPOTEN catalog number
      CALL OMIN(ki,ieof,irel)
      IRElat(Nejc,Nnuc) = irel
C
C-----Ener must be in LAB system
C
  100 relcal = .FALSE.
      IF (IRElat(Nejc,Nnuc).GT.0  .or. RELkin) relcal = .TRUE.
      IF (Ikey.LT.0) THEN
         ener = Eilab
         CALL KINEMA(Eilab,Eicms,Mi,Mt,Ak2,1,relcal)
      ELSE
         CALL KINEMA(Eilab,Eicms,Mi,Mt,Ak2,2,relcal)
         ener = Eilab
      ENDIF
      CALL RIPL2EMPIRE(Nejc,Nnuc,ener)
      IF (CCCalc) MODelecis = MODelcc
C     Sometimes imaginary potentials are allowed to be negatives, RCN
C     WOMs(Nejc,Nnuc) = MAX(WOMs(Nejc,Nnuc),0.D0)
C     WOMv(Nejc,Nnuc) = MAX(WOMv(Nejc,Nnuc),0.D0)
C-----Some default protections
C-----set coulomb radius equal to 1.25 if not defined
      IF (RCOul(Nejc,Nnuc).EQ.0.0D0) RCOul(Nejc,Nnuc) = 1.25
C-----set volume imaginary diff. equal to surface imag. diff. if not defined
      IF (AWOmv(Nejc,Nnuc).EQ.0.0D0) AWOmv(Nejc,Nnuc) = AWOm(Nejc,Nnuc)
C-----set volume imaginary radius equal to surface imag. radius if not defined
      IF (RWOmv(Nejc,Nnuc).EQ.0.0D0) RWOmv(Nejc,Nnuc) = RWOm(Nejc,Nnuc)
C
C-----write O.M. potential parameters to the file OMPAR=>*.omp
C-----only if .omp file did not exist and IOMWRITE is even (to avoid repetion)
C
      iowrite = IOMwrite(Nejc,Nnuc)
      fexist = OMPar_riplf
      IF (CCCalc) THEN
         iowrite = IOMwritecc
         fexist = OMParfcc
      ENDIF
      IF (MOD(iowrite,2).EQ.0 .AND. .NOT.fexist) THEN
         ki = 26
C--------komp = 29 OR 33
         ipoten = KTRlom(Nejc,Nnuc)
         CALL FINDPOT(ki,ieof,ipoten)
         IF (ieof.GT.0) THEN
            WRITE (6,*) 'ERROR: PROBLEM with OMPAR.RIPL library,RIPL #',
     &                  ipoten
            STOP 'ERROR: PROBLEM with OMPAR.RIPL library'
         ENDIF
C--------Reading IPOTEN catalog number potential parameters
         ianucr = INT(A(Nnuc))
         symbnucr = SYMb(Nnuc)
         iaejcr = INT(AEJc(Nejc))
         symbejcr = SYMbe(Nejc)
         CALL CREATE_OMPAR(ki,Komp,ieof,Nnuc,Nejc,ianucr,symbnucr,
     &                     iaejcr,symbejcr)
         IF (CCCalc) THEN
C-----------increase IOMWRITE so that all these is not written again and again
            IOMwritecc = 1
         ELSE
C-----------increase IOMWRITE so that all these is not written again and again
            IOMwrite(Nejc,Nnuc) = IOMwrite(Nejc,Nnuc) + 1
         ENDIF
      ENDIF
C-----O.M. potential parameters written
      END


      SUBROUTINE CREATE_OMPAR(Ki,Komp,Ieof,Nnucr,Nejcr,Ianucr,Symbnucr,
     &                        Iaejcr,Symbejcr)
      INCLUDE 'ripl2empire.h'
C
C
C Dummy arguments
C
      INTEGER Iaejcr, Ianucr, Ieof, Ki, Komp, Nejcr, Nnucr
      CHARACTER*2 Symbejcr, Symbnucr
C
C Local variables
C
      CHARACTER*80 comment
      INTEGER i, j, krange, l
      INTEGER IABS
      CHARACTER*10 potnam(6)
C
C-----Read optical model parameters from the RIPL-II library and create local
C-----omp file
C
      DATA potnam/' Real vol.', ' Imag vol.', ' Real surf',
     &     ' Imag surf', ' Real SO  ', ' Imag SO  '/
      Ieof = 0
      READ (Ki,*) IREf
      WRITE (Komp,'(3I5,4x,I3,''-'',A2,'' +'',I3,''-'',A2)') IREf,
     &       Nnucr, Nejcr, Ianucr, Symbnucr, Iaejcr, Symbejcr
      DO l = 2, 7
         READ (Ki,'(a80)') comment
         WRITE (Komp,'(a80)') comment
      ENDDO
C-----1   read(ki,*,end=999) iref
C-----2   read(ki,1) (author(i),i=1,80)
C-----3   read(ki,1) (refer(i),i=1,80)
C-----4-7 read(ki,1) (summary(i),i=1,320)
C     read(ki,*) emin,emax
      READ (Ki,'(a80)') comment
      WRITE (Komp,'(a80,a10)') comment, ' Emin,Emax'
C     read(ki,*) izmin,izmax
      READ (Ki,'(a80)') comment
      WRITE (Komp,'(a80,a10)') comment, ' Zmin,Zmax'
C     read(ki,*) iamin,iamax
      READ (Ki,'(a80)') comment
      WRITE (Komp,'(a80,a10)') comment, ' Amin,Amax'
C     read(ki,*) imodel,izproj,iaproj,irel,idr
      READ (Ki,'(a80)') comment
      WRITE (Komp,'(a80,a)') comment, ' Mod,Zp,Ap,irel,idr'
      DO i = 1, 6
         READ (Ki,'(a80)') comment
         WRITE (Komp,'(a80,a10,a10)') comment, ' NEranges ', potnam(i)
         BACKSPACE (Ki)
         READ (Ki,*) JRAnge(i)
         IF (JRAnge(i).NE.0) THEN
            krange = IABS(JRAnge(i))
            DO j = 1, krange
C              read(ki,*) epot(i,j)
               READ (Ki,'(a80)') comment
               WRITE (Komp,'(a80,a19)') comment, ' Upper energy limit'
C              read(ki,*) (rco(i,j,n),n=1,ndim2)
               READ (Ki,'(a80)') comment
               WRITE (Komp,'(a80,a10)') comment, ' Radius(E)'
C--------------Assumed ndim2 must be greater than 7 and less than 13
C--------------Otherwise delete the following two lines
               READ (Ki,'(a80)') comment
               WRITE (Komp,'(a80,a10)') comment, ' Radius(E)'
C              read(ki,*) (aco(i,j,n),n=1,ndim2)
               READ (Ki,'(a80)') comment
               WRITE (Komp,'(a80,a10)') comment, ' Diffus(E)'
C--------------Assumed ndim2 must be greater than 7 and less than 13
C--------------Otherwise delete the following two lines
               READ (Ki,'(a80)') comment
               WRITE (Komp,'(a80,a10)') comment, ' Diffus(E)'
C              read(ki,*) (pot(i,j,n),n=1,ndim3)
               READ (Ki,'(a80)') comment
               WRITE (Komp,'(a80,a10)') comment, ' Vdepth(E)'
               READ (Ki,'(a80)') comment
               WRITE (Komp,'(a80,a10)') comment, ' Vdepth(E)'
               READ (Ki,'(a80)') comment
               WRITE (Komp,'(a80,a10)') comment, ' Vdepth(E)'
C--------------Assumed ndim3 must be greater than 19 !!!
C--------------Otherwise delete the following two lines
               READ (Ki,'(a80)') comment
               WRITE (Komp,'(a80,a10)') comment, ' Vdepth(E)'
            ENDDO
         ENDIF
      ENDDO
      READ (Ki,'(a80)') comment
      WRITE (Komp,'(a80,a20)') comment, ' Coul.ranges (jcoul)'
      BACKSPACE (Ki)
      READ (Ki,*) JCOul
      IF (JCOul.GT.0) THEN
         DO j = 1, JCOul
            READ (Ki,'(a80)') comment
            WRITE (Komp,'(a80,a21)') comment,' Ec,R0,R,R1,R2,bet,AC'
         ENDDO
      ENDIF
      IF (IMOdel.GT.0) THEN
C
C--------To avoid repetition of discrete level information
C--------we are not writing here to OMPAR.*
C--------Instead a TARGET_LEV.COLL file is used
C
         NISotop = 0
         WRITE (Komp,'(1x,i4)') NISotop
      ENDIF
      WRITE (Komp,'(A8)') '++++++++'
      END


      SUBROUTINE READ_OMPAR_RIPL(Ko,Ierr,Irelout)
      INCLUDE 'ripl2empire.h'
C
C
C Dummy arguments
C
      INTEGER Ierr, Irelout, Ko
C
C Local variables
C
      CHARACTER*80 comment
      INTEGER i, j, k, krange, n
      INTEGER IABS
C
C-----Read RIPL optical model parameters from the local OMPAR.RIPL file
C
      Ierr = 0
      READ (Ko,'(I5)',ERR = 200) IREf
      READ (Ko,99050,ERR = 200) AUThor
      READ (Ko,99050,ERR = 200) REFer
      READ (Ko,99050,ERR = 200) SUMmary
      READ (Ko,99025,ERR = 200) EMIn, EMAx
      READ (Ko,99020,ERR = 200) IZMin, IZMax
      READ (Ko,99020,ERR = 200) IAMin, IAMax
      READ (Ko,99020,ERR = 200) IMOdel, IZProj, IAProj, IREl, IDR
      Irelout = IREl
      DO i = 1, 6
         READ (Ko,99020,ERR = 200) JRAnge(i)
         IF (JRAnge(i).NE.0) THEN
            krange = IABS(JRAnge(i))
            DO j = 1, krange
               READ (Ko,99025,ERR = 200) EPOt(i,j)
C--------------Reading radius
               READ (Ko,99030) (RCO(i,j,n),n = 1,7)
               IF (NDIM2.GT.7 .AND. NDIM2.LE.13) THEN
                  READ (Ko,99035,ERR = 200) (RCO(i,j,n),n = 8,NDIM2)
               ELSE
                  READ (Ko,99035,ERR = 200) (RCO(i,j,n),n = 8,13)
               ENDIF
               IF (NDIM2.GT.13) STOP
     &                 '1 IN tl.f, UNCOMMENT READ ABOVE AND REMOVE STOP'
C--------------Reading diffuss
               READ (Ko,99030,ERR = 200) (ACO(i,j,n),n = 1,7)
               IF (NDIM2.GT.7 .AND. NDIM2.LE.13) THEN
                  READ (Ko,99035,ERR = 200) (ACO(i,j,n),n = 8,NDIM2)
               ELSE
                  READ (Ko,99035,ERR = 200) (ACO(i,j,n),n = 8,13)
               ENDIF
               IF (NDIM2.GT.13) STOP
     &                 '2 IN tl.f, UNCOMMENT READ ABOVE AND REMOVE STOP'
C--------------Reading depths
               READ (Ko,99030,ERR = 200) (POT(i,j,n),n = 1,7)
               IF (NDIM3.GT.7 .AND. NDIM3.LE.13) THEN
                  READ (Ko,99035,ERR = 200) (POT(i,j,n),n = 8,NDIM3)
               ELSE
                  READ (Ko,99035,ERR = 200) (POT(i,j,n),n = 8,13)
               ENDIF
               IF (NDIM3.GT.13 .AND. NDIM3.LE.19) THEN
                  READ (Ko,99035,ERR = 200) (POT(i,j,n),n = 14,NDIM3)
               ELSE
                  READ (Ko,99035,ERR = 200) (POT(i,j,n),n = 14,19)
               ENDIF
               IF (NDIM3.GT.19) READ (Ko,99035,ERR = 200)
     &                                (POT(i,j,n),n = 20,NDIM3)
            ENDDO
         ENDIF
      ENDDO
      READ (Ko,99020,ERR = 200) JCOul
      IF (JCOul.GT.0) THEN
         DO j = 1, JCOul
            READ (Ko,99025,ERR = 200) ECOul(j), RCOul0(j), RCOul(j),
     &                      RCOul1(j), RCOul2(j), BETa(j), ACOul(j)
         ENDDO
      ENDIF
      IF (IMOdel.EQ.1) THEN
         READ (Ko,99020,ERR = 200) NISotop
         DO n = 1, NISotop
            IF (IDEf(n).LE.8) THEN
               READ (Ko,99040,ERR = 200) IZ(n), IA(n), NCOll(n), LMAx(n)
     &               , IDEf(n), BANdk(n), (DEF(n,k),k = 2,IDEf(n),2)
            ELSE
               READ (Ko,99040,ERR = 200) IZ(n), IA(n), NCOll(n), LMAx(n)
     &               , IDEf(n), BANdk(n), (DEF(n,k),k = 2,8,2)
            ENDIF
            IF (IDEf(n).GE.8) READ (Ko,99010,ERR = 200)
     &                              (DEF(n,k),k = 2,IDEf(n),2)
99010       FORMAT (30x,4(1x,e10.3))
            DO k = 1, NCOll(n)
               READ (Ko,99045,ERR = 200) EX(k,n), SPIn(k,n), IPAr(k,n)
            ENDDO
         ENDDO
      ELSEIF (IMOdel.EQ.2) THEN
         READ (Ko,99020,ERR = 200) NISotop
         DO n = 1, NISotop
            READ (Ko,99020,ERR = 200) IZ(n), IA(n), NVIb(n)
            DO k = 1, NVIb(n)
               READ (Ko,99045,ERR = 200) EXV(k,n), SPInv(k,n),
     &               IPArv(k,n), NPH(k,n), DEFv(k,n), THEtm(k,n)
            ENDDO
         ENDDO
      ELSEIF (IMOdel.EQ.3) THEN
         READ (Ko,99020,ERR = 200) NISotop
         DO n = 1, NISotop
            READ (Ko,99015,ERR = 200) IZ(n), IA(n), BETa0(n), GAMma0(n),
     &                                XMUbeta(n)
99015       FORMAT (2I5,1p,3(1x,e11.4))
         ENDDO
      ENDIF
      READ (Ko,'(A80)',END = 100) comment
  100 RETURN
  200 Ierr = 1
99020 FORMAT (10I5)
99025 FORMAT (7F10.3)
99030 FORMAT (f12.5,1x,6(e11.4))
99035 FORMAT (13x,6(e11.4))
99040 FORMAT (5I5,f5.1,4(1x,e10.3))
99045 FORMAT (f12.8,f7.1,2I4,1p,2(1x,e11.4))
99050 FORMAT (80A1)
      END


      SUBROUTINE OMIN(Ki,Ieof,Irelout)
C
C     routine to read optical model parameters
C
      INCLUDE 'ripl2empire.h'
C
C Dummy arguments
C
      INTEGER Ieof, Irelout, Ki
C
C Local variables
C
      INTEGER i, j, k, krange, n
      INTEGER IABS
      CHARACTER*80 idum
      Ieof = 0
      READ (Ki,*,END = 100) IREf
      READ (Ki,99005) (AUThor(i),i = 1,80)
      READ (Ki,99005) (REFer(i),i = 1,80)
      READ (Ki,99005) (SUMmary(i),i = 1,320)
      READ (Ki,*) EMIn, EMAx
      READ (Ki,*) IZMin, IZMax
      READ (Ki,*) IAMin, IAMax
      READ (Ki,*) IMOdel, IZProj, IAProj, IREl, IDR
      Irelout = IREl
      DO i = 1, 6
         READ (Ki,*) JRAnge(i)
         IF (JRAnge(i).NE.0) THEN
            krange = IABS(JRAnge(i))
            DO j = 1, krange
               READ (Ki,*) EPOt(i,j)
               READ (Ki,*) (RCO(i,j,n),n = 1,NDIM2)
               READ (Ki,*) (ACO(i,j,n),n = 1,NDIM2)
               READ (Ki,*) (POT(i,j,n),n = 1,NDIM3)
            ENDDO
         ENDIF
      ENDDO
      READ (Ki,*) JCOul
      IF (JCOul.GT.0) THEN
         DO j = 1, JCOul
            READ (Ki,*) ECOul(j), RCOul0(j), RCOul(j), RCOul1(j),
     &                  RCOul2(j), BETa(j), ACOul(j)
         ENDDO
      ENDIF
      IF (IMOdel.EQ.1) THEN
         READ (Ki,*) NISotop
         DO n = 1, NISotop
            READ (Ki,*) IZ(n), IA(n), NCOll(n), LMAx(n), IDEf(n),
     &                  BANdk(n), (DEF(n,k),k = 2,IDEf(n),2)
            DO k = 1, NCOll(n)
               READ (Ki,*) EX(k,n), SPIn(k,n), IPAr(k,n)
            ENDDO
         ENDDO
      ELSEIF (IMOdel.EQ.2) THEN
         READ (Ki,*) NISotop
         DO n = 1, NISotop
            READ (Ki,*) IZ(n), IA(n), NVIb(n)
            DO k = 1, NVIb(n)
               READ (Ki,*) EXV(k,n), SPInv(k,n), IPArv(k,n), NPH(k,n),
     &                     DEFv(k,n), THEtm(k,n)
            ENDDO
         ENDDO
      ELSEIF (IMOdel.EQ.3) THEN
         READ (Ki,*) NISotop
         DO n = 1, NISotop
            READ (Ki,*) IZ(n), IA(n), BETa0(n), GAMma0(n), XMUbeta(n)
         ENDDO
      ENDIF
      READ (Ki,99005,END = 100) idum
      RETURN
  100 Ieof = 1
99005 FORMAT (80A1)
      END


      SUBROUTINE SUMPRT(Ko)
C
C     print summary information from RIPL formatted library
C
      INCLUDE 'ripl2empire.h'
C
C
C Dummy arguments
C
      INTEGER Ko
C
C Local variables
C
      CHARACTER*20 area
      INTEGER i, iarea, iemax, iemin, n, nauth, nn, nrefer, nsum
      INTEGER INT, MIN0
      CHARACTER*8 ldum, proj
      CHARACTER*40 model
      DATA ldum/'++++++++'/
      IF (IZProj.EQ.0 .AND. IAProj.EQ.1) proj = ' Neutron'
      IF (IZProj.EQ.1 .AND. IAProj.EQ.1) proj = '  Proton'
      IF (IZProj.EQ.1 .AND. IAProj.EQ.2) proj = 'Deuteron'
      IF (IZProj.EQ.1 .AND. IAProj.EQ.3) proj = '  Triton'
      IF (IZProj.EQ.2 .AND. IAProj.EQ.3) proj = '    He-3'
      IF (IZProj.EQ.2 .AND. IAProj.EQ.4) proj = '   Alpha'
      IF (IMOdel.EQ.0) model = 'spherical nucleus model'
      IF (IMOdel.EQ.1) model = 'coupled-channels rotational model'
      IF (IMOdel.EQ.2) model = 'vibrational model'
      IF (IMOdel.EQ.3) model = 'non=axial deformed model'
      iarea = MOD(IREf,1000)
      IF (iarea.LE.99) area = 'United States (LANL)'
      IF (iarea.GE.100 .AND. iarea.LE.199) area = 'United States'
      IF (iarea.GE.200 .AND. iarea.LE.299) area = 'Japan'
      IF (iarea.GE.300 .AND. iarea.LE.399) area = 'Russia'
      IF (iarea.GE.400 .AND. iarea.LE.499) area = 'Europe'
      IF (iarea.GE.500 .AND. iarea.LE.599) area = 'China'
      IF (iarea.GE.600 .AND. iarea.LE.649) area = 'East Europe+FSU'
      IF (iarea.GE.650 .AND. iarea.LE.699) area = 'India, Pakistan'
      IF (iarea.GE.700 .AND. iarea.LE.799) area = 'Others'
      IF (iarea.GE.800 .AND. iarea.LE.999) area = 'Reserved'
      WRITE (Ko,99010) IREf, proj, model
99010 FORMAT (' IREF=',i5,2x,a8,' incident, ',a40)
      IF (IDR.GT.0) WRITE (Ko,'('' Dispersive optical model'')')
      IF (IREl.EQ.0) WRITE (Ko,'('' Non-relativistic kinematics'')')
      IF (IREl.EQ.1) WRITE (Ko,'('' Relativistic kinematics'')')
      IF (IREl.EQ.2) WRITE (Ko,
     &'('' Relativistic kinematics + Dirac equivalent Schroedinger equat
     &ion.'')')
      iemin = INT(EMIn)
      iemax = INT(EMAx)
      WRITE (Ko,99015) IZMin, IZMax, IAMin, IAMax, iemin, iemax
99015 FORMAT (' Z-Range=',i3,'-',i2,'  A-Range=',i4,'-',i3,'  E-Range=',
     &        i4,'-',i3,' MeV')
      DO nn = 1, 80
         n = 80 - nn + 1
         IF (AUThor(n).NE.' ') GOTO 100
      ENDDO
  100 nauth = MIN0(80,n)
      WRITE (Ko,99020) (AUThor(n),n = 1,nauth)
99020 FORMAT (' Author(s)= ',60A1,/12x,20A1)
      DO nn = 1, 80
         n = 80 - nn + 1
         IF (REFer(n).NE.' ') GOTO 200
      ENDDO
  200 nrefer = MIN0(80,n)
      WRITE (Ko,99025) (REFer(n),n = 1,nrefer)
99025 FORMAT (' Reference= ',60A1,/12x,20A1)
      DO nn = 1, 320
         n = 320 - nn + 1
         IF (SUMmary(n).NE.' ') GOTO 300
      ENDDO
  300 nsum = MIN0(320,n)
      WRITE (Ko,99030) (SUMmary(n),n = 1,nsum)
99030 FORMAT ('   Summary= ',60A1,/12x,60A1,/12x,60A1,/12x,60A1,/12x,
     &        60A1,/12x,20A1)
      WRITE (Ko,99035) (ldum,i = 1,9)
99035 FORMAT (10A8)
      END


      SUBROUTINE FINDPOT_OMPAR_RIPL(Ki,Ieof,Ipoten,Nnuc,Nejc)
C
C-----Find IPOTEN entry in the RIPL optical model database
C
C Dummy arguments
C
      INTEGER Ieof, Ipoten, Ki, Nejc, Nnuc
C
C Local variables
C
      CHARACTER*3 ctmp
      INTEGER iref, nejcr, nnucr
      Ieof = 0
      REWIND (Ki)
      READ (Ki,'(3i5)') iref, nnucr, nejcr
      IF (Ipoten.EQ.iref .AND. Nnuc.EQ.nnucr .AND. nejcr.EQ.Nejc) THEN
         BACKSPACE (Ki)
         RETURN
      ENDIF
  100 READ (Ki,99005,END = 200) ctmp
99005 FORMAT (A3)
      IF (ctmp.NE.'+++') GOTO 100
      READ (Ki,'(3i5)',END = 200) iref, nnucr, nejcr
      IF (iref.NE.Ipoten .OR. Nnuc.NE.nnucr .OR. nejcr.NE.Nejc) GOTO 100
      BACKSPACE (Ki)
      RETURN
  200 Ieof = 1
      END


      SUBROUTINE TLEVAL(Nejc,Nnuc,Nonzero)
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
CCC   ********************************************************************
CCC
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C COMMON variables
C
      INTEGER MAXl(NDETL)
      DOUBLE PRECISION TTLl(NDETL,0:NDLW)
      COMMON /TRANSM/ TTLl, MAXl
C
C Dummy arguments
C
      INTEGER Nejc, Nnuc
      LOGICAL Nonzero
C
C Local variables
C
      INTEGER i, l
      INTEGER INT, MIN0
      Nonzero = .FALSE.
      IF (SEJc(Nejc).GT.1.0D0) THEN
         WRITE (6,
     &'('' SPIN OF EJECTILE A='',I3,'' AND Z='',I2,'' IS '',    F4.1,/,'
     &' SUCH A LARGE VALUE IS NOT ALLOWED IN TRANSMISSION CEOFFICIENT CA
     &LCULATIONS'',/,'' EXECUTION S T O P P E D '')') INT(AEJc(Nejc)),
     &INT(ZEJc(Nejc)), SEJc(Nejc)
         STOP '13'
      ENDIF
C
C-----TL trans. coeff. at zero energy must be zero
C
      DO i = 1, NDETL
         LMAxtl(i,Nejc,Nnuc) = 0
         DO l = 1, NDLW
            TL(i,l,Nejc,Nnuc) = 0.D0
         ENDDO
      ENDDO
C-----TARGET NUCLEUS (ELASTIC CHANNEL), incident neutron or proton
C-----Transmission coefficient matrix for elastic channel
C-----ECIS code is used
C-----KTRlom(Nejc,Nnuc)=KTRompCC
      IWArn = 0
C-----Preparing INPUT and RUNNING ECIS
C-----(or reading already calculated file)
      CALL TRANSINP(Nejc,Nnuc,NDETL)
C-----IWARN=0 - 'NO Warnings'
C-----IWARN=1 - 'A out of the recommended range '
C-----IWARN=2 - 'Z out of the recommended range '
C-----IWARN=3 - 'Energy requested lower than recommended for this potential'
C-----IWARN=4 - 'Energy requested higher than recommended for this potential'
      IF ((IWArn.EQ.1 .OR. IWArn.EQ.2) .AND. IOUt.GE.5) WRITE (6,*)
     &  ' WARNING: OMP not recommended for Z,A=', Z(Nnuc),'-',A(Nnuc)
      IF (IWArn.EQ.3 .AND. IOUt.GE.5) WRITE (6,*)
     &     ' WARNING: OMP not recommended for low energies in Tl calc'
      IF (IWArn.EQ.4 .AND. IOUt.GE.5) WRITE (6,*)
     &     ' WARNING: OMP not recommended for high energies in Tl calc'
      IWArn = 0
C--------transfer of the calculated transmission coeff. onto TL matrix
      DO i = 2, NDETL
         LMAxtl(i,Nejc,Nnuc) = MIN0(MAXl(i) + 1,NDLW)
         DO l = 1, LMAxtl(i,Nejc,Nnuc)
            IF (TTLl(i,l - 1).LT.1.D-10) GOTO 100
            TL(i,l,Nejc,Nnuc) = TTLl(i,l - 1)
            Nonzero = .TRUE.
         ENDDO
  100 ENDDO
      END


      SUBROUTINE TLLOC(Nnuc,Nejc,Eout,Il,Frde)
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
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C Dummy arguments
C
      DOUBLE PRECISION Eout, Frde
      INTEGER Il, Nejc, Nnuc
C
C Local variables
C
      DOUBLE PRECISION detl
      INTEGER i
      INTEGER INT
      IF (Eout.GT.ETL(5,Nejc,Nnuc)) THEN
         Il = INT((Eout - ETL(5,Nejc,Nnuc))/DE) + 5
         Frde = (Eout - ETL(Il,Nejc,Nnuc))/DE
      ELSE
         DO i = 4, 1, -1
            IF (Eout.GT.ETL(i,Nejc,Nnuc)) THEN
               Il = i
               detl = ETL(i + 1,Nejc,Nnuc) - ETL(i,Nejc,Nnuc)
               Frde = (Eout - ETL(i,Nejc,Nnuc))/detl
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
C          INPUT  : NEJC - Ejectile key
C                   NNUC - Residual nucleus key
C                   NEN  - Max.Emiss. energy Index
C
C
C
      SUBROUTINE TRANSINP(Nejc,Nnuc,Nen)
      INCLUDE "dimension.h"
      INCLUDE "global.h"
C
C COMMON variables
C
      INTEGER MAXl(NDETL)
      DOUBLE PRECISION TTLl(NDETL,0:NDLW)
      COMMON /TRANSM/ TTLl, MAXl
C
C Dummy arguments
C
      INTEGER Nejc, Nen, Nnuc
C
C Local variables
C
      CHARACTER*3 ctldir
      CHARACTER*23 ctmp23
      DOUBLE PRECISION culbar, ener
      LOGICAL fexist, ltmp
      INTEGER i, ilv, ien, ien_beg, l, lmax
      INTEGER INT
      REAL SNGL
C     --------------------------------------------------------------------
C     | Calculation of transmission coefficients using ECIS              |
C     |                for EMPIRE energy grid                            |
C     --------------------------------------------------------------------
      DATA ctldir/'TL/'/

C-----data initialization
      CALL INIT(Nejc,Nnuc)
C-----Cleaning transmission coefficient matrix
      DO i = 1, NDETL
         MAXl(i) = 0
         DO l = 0, NDLW
            TTLl(i,l) = 0.D0
         ENDDO
      ENDDO
C
C-----This part prompts for the name of a data file. The INQUIRE
C-----statement then determines whether or not the file exists.
C-----If it does not, the program calculates new transmission coeff.
      WRITE (ctmp23,'(i3.3,i3.3,1h_,i3.3,i3.3,1h_,i9.9)')
     &       INT(ZEJc(Nejc)), INT(AEJc(Nejc)), INT(Z(Nnuc)),
     &       INT(A(Nnuc)), INT(EINl*1000000)
      INQUIRE (FILE = (ctldir//ctmp23//'.BIN'),EXIST = fexist)
      IF (.NOT.fexist) GOTO 400
C-----Here the previously calculated files should be read
      OPEN (45,FILE = (ctldir//ctmp23//'.BIN'),FORM = 'UNFORMATTED')
      IF (IOUt.EQ.5) OPEN (46,FILE = ctldir//ctmp23//'.LST')
  100 READ (45,END = 200) lmax, ien, ener, IRElat(Nejc,Nnuc)
      IF (IOUt.EQ.5) WRITE (46,'(A5,2I6,E12.6)') 'LMAX:', lmax, ien,
     &                      ener
C
C-----If energy read from the file does not coincide
C-----this nucleus should be recalculated (goto 300)
C
      IF (ABS(ener - ETL(ien,Nejc,Nnuc)).GT.0.0001) THEN
         CLOSE (45,STATUS = 'DELETE')
         IF (IOUt.EQ.5) CLOSE (46,STATUS = 'DELETE')
         IF (IOUt.EQ.5) THEN
         WRITE (6,*) 'WARNING: ENERGY MISMATCH: ETL(ien=', ien, '...)=',
     &               ETL(ien,Nejc,Nnuc), ' REQUESTED ENERGY=',
     &               SNGL(ener)
         WRITE (6,*)
     &              'WARNING: FILE WITH TRANSM. COEFF. HAS BEEN DELETED'
          ENDIF
         GOTO 400
      ENDIF
      ETL(ien,Nejc,Nnuc) = ener
      MAXl(ien) = lmax
      DO l = 0, lmax
         READ (45,END = 300) TTLl(ien,l)
         IF (IOUt.EQ.5) WRITE (46,*) l, TTLl(ien,l)
      ENDDO
      READ (45,END = 300) SIGabs(ien,Nejc,Nnuc)
      GOTO 100
  200 CLOSE (45)
      IF (IOUt.EQ.5) CLOSE (46)
      IF (IOUt.EQ.5) WRITE (6,*)
     &                      'Transmission coefficients read from file: '
     &                      , (ctldir//ctmp23//'.BIN')
      RETURN

  300 CLOSE (45,STATUS = 'DELETE')
      IF (IOUt.EQ.5) CLOSE (46,STATUS = 'DELETE')
      DO i = 1, NDETL
         MAXl(i) = 0
         DO l = 0, NDLW
            TTLl(i,l) = 0.D0
         ENDDO
      ENDDO
      IF (IOUt.GT.0) WRITE (6,*) 'WARNING: ERROR WHEN READING TLs in ',
     &                           ctmp23
      IF (IOUt.EQ.5) THEN
         WRITE (6,*)
     &              'WARNING: FILE WITH TRANSM. COEFF. HAS BEEN DELETED'
         WRITE (6,*)
     &           'WARNING: TRANSM. COEFF. WILL BE CALCULATED AND STORED'
      ENDIF
C-----Coulomb barrier (somewhat decreased) setting lower energy limit
C-----for transsmission coefficient calculations
  400 culbar = 0.8*ZEJc(Nejc)*Z(Nnuc)/(1 + A(Nnuc)**0.6666)
      ien_beg = -1
      DO i = 2, Nen
         IF (ETL(i,Nejc,Nnuc).GT.culbar) THEN
            ien_beg = i
            GOTO 500
         ENDIF
      ENDDO
  500 IF (ien_beg.NE. - 1) THEN
C
C--------Running ECIS
C
         IF (IOUt.EQ.5) THEN
            WRITE (6,*)
            IF (DIRect.EQ.2 .AND. AEJc(Nejc).LE.1) THEN
               WRITE (6,*) ' CC transmission coefficients used for ',
     &                     'outgoing channels'
            ELSE
               WRITE (6,*) ' Spherical OM transmission coefficients',
     &                     ' used for outgoing channels'
            ENDIF
            WRITE (6,*)
         ENDIF
C--------OPEN Unit=46 for Tl output
         OPEN (UNIT = 46,STATUS = 'unknown',
     &         FILE = (ctldir//ctmp23//'.BIN'),FORM = 'UNFORMATTED')
C
C--------do loop over energy
C
         ltmp = A(Nnuc).EQ.A(0) .AND. Z(Nnuc).EQ.Z(0) .AND.
     &          DIRect.EQ.2 .AND. AEJc(Nejc).LE.1

         DO i = ien_beg, Nen
            ener = ETL(i,Nejc,Nnuc)
            IF (ener.LE.0.1D-6) GOTO 550

            IF (.NOT.(ltmp)) THEN
C
C--------------Spherical optical model is assumed, only one level (gs)
C
               CALL ECIS_CCVIB(Nejc,Nnuc,ener,.TRUE.,0)
               CALL ECIS2EMPIRE_TR(Nejc,Nnuc,i,.TRUE.)
C--------------Transmission coefficient matrix for incident channel
C--------------is calculated (DIRECT = 2 (CCM)) using ECIS code.
C--------------Only coupled levels are considered
            ELSEIF (DEFormed) THEN
C--------------In this case we need only CC calculation so INLkey=0
               CALL ECIS_CCVIBROT(Nejc,Nnuc,ener,0)
               CALL ECIS2EMPIRE_TR(Nejc,Nnuc,i,.FALSE.)
            ELSE
C--------------EXACT (no DWBA) calculation
               CALL ECIS_CCVIB(Nejc,Nnuc,ener,.FALSE., - 1)
               CALL ECIS2EMPIRE_TR(Nejc,Nnuc,i,.TRUE.)

            ENDIF

  550    ENDDO

         CLOSE (46)
         IF (IOUt.EQ.5) WRITE (6,*) ' Transm. coeff. written to file:',
     &                              (ctldir//ctmp23//'.BIN')
         IF (IOUt.EQ.5) WRITE (6,*)
     &            ' ==================================================='
      ELSEIF (IOUt.EQ.5) THEN
         WRITE (6,'(1x,A12,I3,A3,I3,A3,F4.1)') 'EJECTILE: A=',
     &          INT(AEJc(Nejc)), ' Z=', INT(ZEJc(Nejc)), ' S=',
     &          SEJc(Nejc)
         ilv = 1
         If(Nnuc.eq.0) ilv = Levtarg
         WRITE (6,'(1x,A12,I3,A3,I3,A3,F4.1,A3,I2)') 'RESIDUAL: A=',
     &          INT(A(Nnuc)), ' Z=', INT(Z(Nnuc)), ' S=',
     &          SNGL(XJLv(ilv,Nnuc)), ' P=', INT(LVP(ilv,Nnuc))
         WRITE (6,*) 'WARNING: For this residual nucleus'
         WRITE (6,*) 'WARNING: available energy is always '
         WRITE (6,*) 'WARNING: below coulomb barrier'
         WRITE (6,*) 'WARNING: Calculations are not needed!'
         WRITE (6,*)
      ENDIF
      END


      SUBROUTINE ECIS2EMPIRE_TL_TRG(Nejc,Nnuc,Maxlw,Stl,Sel,Lvibrat)
C
C     Process ECIS output to obtain Stl matrix for the incident channel
C     Reads from unit 45 and writes to unit 6
C
C     INPUT:  Nejc,Nnuc ejectile and residual nucleus index
C             LVIbrat = .TRUE. for vibrational model(or spherical OMP)
C             The implemented model could be applied to even-even nucleus
C             therefore the ECIS input is always constructed assuming spin 0
C             So we can not use the real spin of the given nucleus XJLv(1, Nnuc)
C
C     OUTPUT: Maxlw and Stl(1-Maxlw) are the maximum angular momentum and Tls
C             SINl is the total inelastic cross section
C             Selast is the shape elastic cross section
C             Sel(L) contains the shape elastic cross section for a given orbital momentum L 
C             Stl(L) contains the transmission coefficient Tl(L) for a given orbital momentum L 
C
C
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C COMMON variables
C
      DOUBLE PRECISION ABScs, ELAcs, SINl, TOTcs, SINlcc
      COMMON /ECISXS/ ELAcs, TOTcs, ABScs, SINl, SINlcc
C
C Dummy arguments
C
      LOGICAL Lvibrat
      INTEGER Maxlw, Nejc, Nnuc
      DOUBLE PRECISION Stl(NDLW),Sel(NDLW)
C
C Local variables
C
      DOUBLE PRECISION ak2, dtmp, ecms, elab, jc, jj, sabs, sreac,
     &                 xmas_nejc, xmas_nnuc, sinlcont, selast
      DOUBLE PRECISION DBLE
      INTEGER ilv, l, nc, nceq, ncoll, nlev, mintsp, nc1, nc2
      LOGICAL relcal
      CHARACTER*1 parc
      REAL SNGL
      Maxlw = 0
      ncoll = 0

      DO l = 1, NDLW
       Stl(l) = 0.d0
	 Sel(l) = 0.d0
      ENDDO

      ilv = 1
      If(Nnuc.eq.0) ilv = Levtarg
C------------------------------------------
C-----| Input of transmission coefficients|
C------------------------------------------
C-----Opening ecis03 output file containing Smatrix
      OPEN(UNIT = 45, STATUS = 'old', FILE = 'ecis03.smat', ERR=90)
      READ (45,*,END = 90)   ! To skip first line <SMATRIX> ..
   80 READ (45,'(1x,f4.1,1x,a1,1x,i4,1x,i4)',END=90) 
     &     jc, parc, nceq, nctot
C     JC,ParC is the channel spin and parity
C     nceq is the total number of coupled equations
C     ncin is the number of independent coupled equations
      ncsol=nctot/nceq
C     Loop over the number of coupled equations
      do ncint=1,ncsol
        do nc=1,nceq               
C       Reading the coupled level number nlev, the orbital momentum L,
C           angular momentum j and Transmission coefficient Tlj,c(JC)
C       (nlev=1 corresponds to the ground state)
        read (45,
     &  '(1x,3(I2,1x),I3,1x,F5.1,1x,2(D15.7,1x),1x,4x,F11.8)',END=90)                     
     &  nc1,nc2,nlev,l,jj,sreal,simag,stmp                       
        IF (nlev.eq.1 .and. nc1.eq.nc2 
     &  .and. stmp.GT.1.D-15 .AND. L.LT.NDLW) 
C-----------Averaging over particle and target spin, summing over channel spin jc
C    &      stot = stot + (2*jj + 1)*(1.d0-sreal) ! /DBLE(2*L + 1)
     &      Sel(l+1) = Sel(l+1) + (2*jc + 1)*((1 - sreal)**2 + simag**2) 
     &           /DBLE(2*L + 1)
     &           /DBLE(2*SEJc(Nejc) + 1)
     &           /DBLE(2*XJLv(ilv,Nnuc) + 1)
        enddo
      enddo
      GOTO 80
   90 CLOSE(45)

      OPEN (UNIT = 45,STATUS = 'old',FILE = 'INCIDENT.TLJ', ERR=200)
      READ (45,*,END = 200)   ! To skip first line <TLJs.> ..
C-----JC,ParC is the channel spin and parity
C-----nceq is the number of coupled equations
  100 READ (45,'(1x,f4.1,1x,a1,1x,i4)',END = 200) jc, parc, nceq
C-----Loop over the number of coupled equations
      DO nc = 1, nceq
C--------Reading the coupled level number nlev, the orbital momentum L,
C--------angular momentum j and Transmission coefficient Tlj,c(JC)
C--------(nlev=1 corresponds to the ground state)
         READ (45,'(1x,I2,1x,I3,1x,F5.1,1x,e15.6)',END = 200) nlev, l,
     &         jj, dtmp
         ncoll = MAX(nlev,ncoll)
C--------Selecting only ground state
         IF (nlev.EQ.1 .AND. dtmp.GT.1.D-15 .AND. l.LT.NDLW) THEN
C-----------Averaging over particle and target spin, summing over channel spin jc
            Stl(l + 1) = Stl(l + 1) + (2*jc + 1)*dtmp/DBLE(2*l + 1)
     &                   /DBLE(2*SEJc(Nejc) + 1)
     &                   /DBLE(2*XJLv(ilv,Nnuc) + 1)
            Maxlw = MAX(Maxlw,l)
            if(Maxlw.GE.NDLW) Maxlw = NDLW - 1
         ENDIF
      ENDDO
      GOTO 100
  200 CLOSE (45)
C-----For vibrational the Tls must be multiplied by
      IF (Lvibrat) THEN
         DO l = 0, Maxlw
            Stl(l + 1) = Stl(l + 1)*DBLE(2*XJLv(ilv,Nnuc) + 1)
            Sel(l + 1) = Sel(l + 1)*DBLE(2*XJLv(ilv,Nnuc) + 1)
         ENDDO
      ENDIF

      TOTcs = 0.D0
      ABScs = 0.D0
      ELAcs = 0.D0
      SINl = 0.D0
      SINlcc = 0.D0
      sinlcont = 0.D0
      sabs = 0.D0
      selast = 0.D0
      
      OPEN (UNIT = 45,FILE = 'INCIDENT.CS',STATUS = 'old',ERR = 300)
      READ (45,*,END = 300)  ! Skipping first line
      IF (ZEJc(Nejc).EQ.0) READ (45,*) TOTcs
      READ (45,*) ABScs
      IF (ZEJc(Nejc).EQ.0) READ (45,*) ELAcs
  300 CLOSE (45)

      IF (ABScs.LE.0.D0) RETURN
C
C-----Estimating absorption cross section from obtained TLs
C
      xmas_nnuc = (A(Nnuc)*AMUmev + XMAss(Nnuc))/AMUmev
      xmas_nejc = EJMass(Nejc)
C     xmas_nnuc = AMAss(Nnuc)
      elab = EINl
      relcal = .FALSE.
      IF (IRElat(Nejc,Nnuc).GT.0 .OR. RELkin) relcal = .TRUE.
      CALL KINEMA(elab,ecms,xmas_nejc,xmas_nnuc,ak2,1,relcal)

C-----Absorption and elastic cross sections in mb
      DO l = 0, Maxlw
         sabs = sabs + Stl(l + 1)*DBLE(2*l + 1)
	   Sel(l+1) = Sel(l+1)*10.d0*PI/ak2
         selast = selast + Sel(l + 1)*DBLE(2*l + 1)
      ENDDO
      sabs = 10.d0*PI/ak2*sabs

      IF (sabs.LE.0.D0) RETURN

      CSFus = ABScs

      mintsp = mod(NINT(2*D_Xjlv(1)),2)
      OPEN (UNIT = 45,FILE = 'INCIDENT.ICS',STATUS = 'old',ERR = 400)
      READ (45,*,END = 400)  ! Skipping first line
      DO l = 1, NDCOLLEV     ! number of inelastic level
         READ (45,*,END = 400) dtmp
         IF ( (ICOller(l+1).LE.NLV(nnuc)) .AND. 
C            For odd nuclides, collective states in continuum have
C            different spin than the ground state
     &        (mod(NINT(2*D_Xjlv(l+1)),2).eq.mintsp) )THEN 
C          Discrete level scattering
           IF (ICOllev(l+1).LT.LEVcc) THEN
C             Coupled levels
              SINlcc = SINlcc + dtmp
           ELSE
C             Uncoupled discrete levels
              SINl = SINl + dtmp
           ENDIF
         ELSE
C          Scattering into continuum
           sinlcont = sinlcont + dtmp
C          Not included into inelastic as it is renormalized in main.f
C          SINl = SINl + dtmp
         ENDIF
      ENDDO
  400 CLOSE (45)
      IF (SINl+SINlcc.EQ.0.D0) RETURN
C--- SINlcc in next IF changed to SINl - BVC
      IF (SINl.GT.ABScs) THEN
         WRITE (6,*)
         WRITE (6,*) ' WARNING: POSSIBLE ECIS NON-CONVERGENCE !!'
         WRITE (6,
     &     '(5x,''**************************************************'')'
     &     )
         WRITE (6,
     &     '(5x,'' Direct cross section calculation do not converge '')'
     &     )
         WRITE (6,
     &'(6x,''Inelastic cross section ='',F8.2,'' mb''/
     &  6x,''Reaction  cross section ='',F8.2,'' mb''/)') SINl, ABScs
         WRITE (6,
     &     '(5x,'' Either change OMP or change calculation method   '')'
     &     )
         WRITE (6,
     &     '(5x,''        (DIRPOT)   or   (DIRECT) parameters       '')'
     &     )
         WRITE (6,
     &     '(5x,'' This problem usually happens using DWBA method   '')'
     &     )
         WRITE (6,
     &     '(5x,'' to treat strong coupled nuclei                   '')'
     &     )
         WRITE (6,
     &     '(5x,''            CALCULATION STOPPED                   '')'
     &     )
         WRITE (6,
     &     '(5x,''**************************************************'')'
     &     )
         STOP ' POSSIBLE ECIS NON-CONVERGENCE !!'
      ENDIF
C
C     Absorption cross section includes inelastic scattering cross section to coupled levels
C
      sreac = sabs + SINlcc
      IF (IOUt.EQ.5 .AND. sabs.GT.0.D0) THEN
         WRITE (6,*)
         WRITE (6,*) ' INCIDENT CHANNEL:'
         WRITE (6,'(A7,I6,A5,F10.3,A10,F10.3,A10)') '  LMAX:', Maxlw,
     &          ' E = ', EIN, ' MeV (CMS)', elab, ' MeV (LAB)'
         WRITE (6,*) ' XS calculated using Smat:   Selast =',
     &               SNGL(selast), ' mb '
         WRITE (6,*) ' Shape elastic XS =', SNGL(ELAcs),
     &               ' mb (read from ECIS)'
         WRITE (6,*) ' XS calculated using averaged Tls:   Sabs =',
     &               SNGL(sabs), ' mb '
         WRITE (6,*) ' Reaction XS =', SNGL(ABScs),
     &               ' mb (read from ECIS)'
         WRITE (6,*) ' ECIS/EMPIRE ratio of reaction cross section =',
     &               (ABScs - SINlcc)/sabs
         WRITE (6,*) ' Inelastic XS to coupled levels (SINlcc) =',
     &               SNGL(SINlcc), ' mb '
         WRITE (6,*)
     &      ' Inelastic XS to uncoupled discrete levels (DWBA) =',
     &               SNGL(SINl), ' mb '
         WRITE (6,*) ' Inelastic XS to the continuum (sinlcont) =',
     &               SNGL(sinlcont), ' mb '
         IF (SINlcc.GT.0.D0) THEN
            WRITE (6,*) ' Sinl =', SNGL(ABScs), ' mb (read from ECIS)'
            WRITE (6,*) ' Sreac=', SNGL(sreac), ' mb (Sabs + SINlcc)'
         ENDIF
         WRITE (6,*) ' Sreac - SINl (renormalized by DWBA) =',
     &         SNGL(ABScs - SINlcc - SINl), ' mb (Sabs - SINl)'
         WRITE (6,*) ' Total XS =', SNGL(TOTcs), ' mb (read from ECIS)'
         WRITE (6,*) ' Shape Elastic XS =', SNGL(ELAcs),
     &               ' mb (read from ECIS)'
         WRITE (6,*)

      ENDIF
C
C-----Renormalizing TLs to correct very small difference
C     between calculated and read ECIS XS
C     Discrete level inelastic scattering (not coupled levels) also included
C
      DO l = 0, Maxlw
         Stl(l + 1) = Stl(l + 1)*(ABScs - SINlcc - SINl)/sabs
      ENDDO
      CSFus = ABScs - SINlcc - SINl

      RETURN
      END


      SUBROUTINE ECIS2EMPIRE_TR(Nejc,Nnuc,J,Lvibrat)
C
C     Process ECIS output to obtain TTLl,MaxL matrix for EMPIRE energy grid
C     Reads from units 45, 46 and writes to unit 6
C
C     INPUT:  Nejc  ejectile nucleus index
C             Nnuc  residual nucleus index
C             J     energy index
C     OUTPUT: TTLl(NDETL,0:NDLW),MaxL(NDETL) for all energies from 1 to NDETL
C
C==========================================================================
C     ncoll - number of collective coupled levels
C
C     Process file ecis03.tlj
C     Average of Tlj,c(J) coefficients over J for c = gs
C     to obtain usual T(L)
C     Sigma_abs = pi/k**2 * suma [(2*L+1) T(L)]
C     Sigma_reacc = Sigma_abs(c=gs) + Sigma_inl(c = other coupled channels)
C
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C COMMON variables
C
      INTEGER MAXl(NDETL)
      DOUBLE PRECISION TTLl(NDETL,0:NDLW)
      COMMON /TRANSM/ TTLl, MAXl
C
C Dummy arguments
C
      INTEGER J, Nejc, Nnuc
      LOGICAL Lvibrat
C
C Local variables
C
      DOUBLE PRECISION ak2, dtmp, ecms, elab, jc, jj, sabs,
     &                 selecis, sinlss, sreac, sreacecis, stotecis,
     &                 xmas_nejc, xmas_nnuc
      DOUBLE PRECISION DBLE
      LOGICAL relcal
      INTEGER l, lmax, nc, nceq, ncoll, nlev
      CHARACTER*1 parc
      REAL SNGL
      lmax = 0
      ncoll = 0
      ecms = ETL(J,Nejc,Nnuc)
C-----
C----- Input of transmission coefficients
C-----
      OPEN (UNIT = 45,STATUS = 'old',FILE = 'ecis03.tlj')
      READ (45,*,END = 200)  ! Skipping one line
C-----JC,ParC is the channel spin and parity
C-----nceq is the number of coupled equations
  100 READ (45,'(1x,f4.1,1x,a1,1x,i4)',END = 200) jc, parc, nceq
C-----Loop over the number of coupled equations
      DO nc = 1, nceq
C--------Reading the coupled level number nlev, the orbital momentum L,
C--------angular momentum j and Transmission coefficient Tlj,c(JC)
C--------(nlev=1 corresponds to the ground state)
         READ (45,'(1x,I2,1x,I3,1x,F5.1,1x,e15.6)',END = 200) nlev, l,
     &         jj, dtmp
         ncoll = MAX(nlev,ncoll)
C--------Selecting only ground state
         IF (nlev.EQ.1 .AND. dtmp.GT.1.D-15 .AND. l.LE.NDLW) THEN
C-----------Averaging over particle and target spin, summing over channel spin JC
            TTLl(J,l) = TTLl(J,l) + (2*jc + 1)*dtmp/DBLE(2*l + 1)
     &                  /DBLE(2*SEJc(Nejc) + 1)
     &                  /DBLE(2*XJLv(1,Nnuc) + 1)
            lmax = MAX(lmax,l)
         ENDIF
      ENDDO
      GOTO 100
  200 CLOSE (45)
C-----For vibrational the Tls must be multiplied by
      IF (Lvibrat) THEN
         DO l = 0, lmax
            TTLl(J,l) = TTLl(J,l)*DBLE(2*XJLv(1,Nnuc) + 1)
         ENDDO
      ENDIF
      stotecis = 0.D0
      selecis = 0.D0
      sreacecis = 0.D0
      OPEN (UNIT = 45,FILE = 'ecis03.cs',STATUS = 'old',ERR = 300)
      READ (45,*,END = 300)  ! Skipping one line
      IF (ZEJc(Nejc).EQ.0) READ (45,*,END = 300) stotecis
      READ (45,*,END = 300) sreacecis
      IF (ZEJc(Nejc).EQ.0) READ (45,*,END = 300) selecis
  300 CLOSE (45)
      sinlss = 0.D0
      SIGabs(J,Nejc,Nnuc) = 0.D0
      IF (sreacecis.LE.0.D0) RETURN
      IF (IOUt.EQ.5) THEN
         xmas_nnuc = (A(Nnuc)*AMUmev + XMAss(Nnuc))/AMUmev
         xmas_nejc = EJMass(Nejc)
         relcal = .FALSE.
         IF (IRElat(Nejc,Nnuc).GT.0 .OR. RELkin) relcal = .TRUE.
         CALL KINEMA(elab,ecms,xmas_nejc,xmas_nnuc,ak2,2,relcal)
C--------Reaction cross section in mb
         sabs = 0.D0
	   if(elab.lt.0.3d0) write(6,*)
         DO l = 0, lmax
            stmp = TTLl(J,l)*DBLE(2*l + 1)
            if(elab.lt.0.3d0) write(6,303) l,stmp*10.*PI/ak2
  303       format(3x,' L =',I3,' Sabs(L) =',d12.6)       	         
	      sabs = sabs + stmp
         ENDDO
	   if(elab.lt.0.3d0) write(6,*)
         sabs = 10.*PI/ak2*sabs
         OPEN (UNIT = 45,FILE = 'ecis03.ics',STATUS = 'old',ERR = 350)
         READ (45,*,END = 350) ! Skipping one line
         sinlss = 0.D0
         DO l = 1, ncoll - 1
            READ (45,*,END = 350) dtmp
            sinlss = sinlss + dtmp
         ENDDO
  350    CLOSE (45)
         sreac = sabs + sinlss
         IF (sabs.GT.0.D0) THEN
            WRITE (6,*)
            WRITE (6,'(A7,I3,A3,E12.6,A10,E12.6,A26,1x,I4)') '  LMAX:',
     &             lmax, ' E=', ecms, ' MeV (CMS)', elab,
     &             ' MeV (LAB); Energy index =', J
            WRITE (6,*) ' XS calculated using averaged Tls:   Sabs =',
     &                  SNGL(sabs), ' mb '
            WRITE (6,*) ' Reaction XS =', SNGL(sreacecis),
     &                  ' mb (read from ECIS)'
            WRITE (6,*) ' ECIS/EMPIRE ratio of reaction XS =',
     &                    SNGL(sreacecis/sreac)
            IF (sinlss.GT.0.D0) THEN
             WRITE (6,*) ' Sinl =', SNGL(sinlss), ' mb (read from ECIS)'
             WRITE (6,*) ' Sreac=', SNGL(sreac), ' mb (Sabs + Sinl)'
            ENDIF
         ENDIF
      ENDIF
C-----Storing transmission coefficients for EMPIRE energy grid
      WRITE (46) lmax, J, ecms, IRElat(Nejc,Nnuc)
      DO l = 0, lmax
         WRITE (46) TTLl(J,l)
      ENDDO
      WRITE (46) sreacecis
      MAXl(J) = lmax
      SIGabs(J,Nejc,Nnuc) = sreacecis
      END
C
C
      BLOCKDATA FORTL
      INCLUDE "pre_ecis.h"
C
C Local variables
C
      INTEGER i
      DATA (PARname(i),i = 1,9)/'neutron ', 'proton  ', 'deuteron',
     &      'triton  ', 'he-3    ', 'alpha   ', 'ION-1   ', 'ION-2   ',
     &      'ION-3   '/
      DATA (NUC(i),i = 1,103)/'H_', 'HE', 'LI', 'BE', 'B_', 'C_', 'N_',
     &      'O_', 'F_', 'NE', 'NA', 'MG', 'AL', 'SI', 'P_', 'S_', 'CL',
     &      'AR', 'K_', 'CA', 'SC', 'TI', 'V ', 'CR', 'MN', 'FE', 'CO',
     &      'NI', 'CU', 'ZN', 'GA', 'GE', 'AS', 'SE', 'BR', 'KR', 'RB',
     &      'SR', 'Y_', 'ZR', 'NB', 'MO', 'TC', 'RU', 'RH', 'PD', 'AG',
     &      'CD', 'IN', 'SN', 'SB', 'TE', 'I_', 'XE', 'CS', 'BA', 'LA',
     &      'CE', 'PR', 'ND', 'PM', 'SM', 'EU', 'GD', 'TB', 'DY', 'HO',
     &      'ER', 'TM', 'YB', 'LU', 'HF', 'TA', 'W_', 'RE', 'OS', 'IR',
     &      'PT', 'AU', 'HG', 'TL', 'PB', 'BI', 'PO', 'AT', 'RN', 'FR',
     &      'RA', 'AC', 'TH', 'PA', 'U_', 'NP', 'PU', 'AM', 'CM', 'BK',
     &      'CF', 'ES', 'FM', 'MD', 'NO', 'LR'/
      END


      SUBROUTINE INIT(Nejc,Nnuc)
      INCLUDE "dimension.h"
      INCLUDE "global.h"
      INCLUDE "pre_ecis.h"
C
C Dummy arguments
C
      INTEGER Nejc, Nnuc
C
C Local variables
C
      CHARACTER*2 ceject
      CHARACTER*5 ctarget
      INTEGER INT, NINT
C
C                         +10       +20       +30       +40
C               123456789 123456789 123456789 123456789 123456789
      BECis1 = 'FFFFFFFFFFFFFFFFFFFFFFFFFFFTFFFFFFFFFFFFFFFFFFFFFF'
C               +50     +60        +70      +80        +90
C               123456789 123456789 123456789 123456789 123456789
C     For ecis03
      BECis2 = 'FFFFFFFFTFFFFTFFTTTFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'


C     BECis2 = 'FFFFFFFFFFFFFTFFTTTFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'
C
C-----*** OPTICAL POTENTIALS ***
C-----Relativistic kinematics (y/n)
      IF (IRElat(Nejc,Nnuc).GT.0 .or. RELkin) THEN
         FLGrel = 'y'
      ELSE
         FLGrel = 'n'
      ENDIF
C-----Making unique name of the output file: outfile
      ctarget(1:2) = NUC(NINT(Z(Nnuc)))
      IF (A(Nnuc).LT.10) THEN
         WRITE (ctarget(3:5),'(3I1)') 0, 0, INT(A(Nnuc))
      ELSEIF (A(Nnuc).LT.100) THEN
         WRITE (ctarget(3:5),'(I1,I2)') 0, INT(A(Nnuc))
      ELSE
C--------A>99
         WRITE (ctarget(3:5),'(I3)') INT(A(Nnuc))
      ENDIF
      WRITE (ceject(1:2),'(I1,I1)') INT(AEJc(Nejc)), INT(ZEJc(Nejc))
      OUTfile = ctarget//'_'//ceject
CPR---write(6,'(1x,A8)') ' UNIQUE NAME OF THE OUTPUT FILE:',outfile
      END


      SUBROUTINE ECIS_CCVIB(Nejc,Nnuc,El,Ldwba,Inlkey)
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
C-----For dispersive optical model potentials
C-----It is ripl2empireS.h because it must be compatible
C-----with global.h declarations so some variables must be renamed
      INCLUDE 'ripl2empireS.h'
C
C COMMON variables
C
      REAL*8 AAV, AS, BS, BV, CS, ETMP, EF, EP, EA, DWVNUM
      INTEGER IQ, NNS, NNV, IDRs
      COMMON /DENERGY/ ETMP, EF, EP, EA
      COMMON /PDATAS/ AS, BS, CS, NNS, IQ
      COMMON /PDATAV/ AAV, BV, NNV
      COMMON /DISPER/ DWVNUM, IDRs 
C
C Dummy arguments
C
      DOUBLE PRECISION El
      INTEGER Inlkey, Nejc, Nnuc
      LOGICAL Ldwba,lodd
C
C Local variables
C
      DOUBLE PRECISION ak2, angstep, ecms, eee, elab, elabe, rmatch,
     &                 xmas_nejc, xmas_nnuc, xratio, zerosp
      CHARACTER*1 ch
      DOUBLE PRECISION DABS
      INTEGER ikey, ip, iterm, j, ldwmax, ncollx, nd_cons, nd_nlvop,
     &        njmax, npp, nwrite
      INTEGER INT, NINT
      IF (AEJc(Nejc).EQ.1.D0 .AND. ZEJc(Nejc).EQ.0.D0) ip = 1
      IF (AEJc(Nejc).EQ.1.D0 .AND. ZEJc(Nejc).EQ.1.D0) ip = 2
      IF (AEJc(Nejc).EQ.2.D0 .AND. ZEJc(Nejc).EQ.1.D0) ip = 3
      IF (AEJc(Nejc).EQ.3.D0 .AND. ZEJc(Nejc).EQ.1.D0) ip = 4
      IF (AEJc(Nejc).EQ.3.D0 .AND. ZEJc(Nejc).EQ.2.D0) ip = 5
      IF (AEJc(Nejc).EQ.4.D0 .AND. ZEJc(Nejc).EQ.2.D0) ip = 6
      IF (AEJc(Nejc).EQ.6.D0 .AND. ZEJc(Nejc).EQ.3.D0) ip = 7
      IF (AEJc(Nejc).EQ.7.D0 .AND. ZEJc(Nejc).EQ.3.D0) ip = 8
      IF (AEJc(Nejc).EQ.7.D0 .AND. ZEJc(Nejc).EQ.4.D0) ip = 9
C-----Data initialization
C     angstep = 2.5
      angstep = 180.d0/(NANgela-1)
      CALL INIT(Nejc,Nnuc)
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
C-----ECIS iteration scheme is used. (To be checked following Harm Wienke message))
      ECIs1(21:21) = 'F'
C-----Usual coupled equations instead of ECIS scheme is used
C     for non-zero spins or energies below 3 MeV
C-----Spin-orbit potential must be not deformed !!
      if(XJLv(1,Nnuc).gt.0.d0 .OR. DABS( - El).LT.3.d0)
     >  ECIs1(21:21) = 'T'
C-----Shift to coupled equations if convergence is not achieved
      ECIs1(23:23) = 'T'
C-----Calculations at experimental angles
      IF (ICAlangs.GT.0) ECIs1(31:31) = 'T'
      ECIs2 = BECis2
C-----Angular distribution is calculated
      ECIs2(14:14) = 'T'
C-----Penetrabilities punched on cards
      ECIs2(13:13) = 'T'
C-----Legendre coefficients output
      ECIs2(15:15) = 'T'
C-----Cmatrix output
      ECIs2(5:5) = 'F'
C-----Smatrix output
      ECIs2(6:6) = 'T'
      ECIs2(10:10) = 'T'
C     DWBA option added
      IF (DIRect.EQ.3 .OR. Ldwba) THEN
C-----Iteration scheme used for DWBA (To be checked following Harm Wienke message)
         ECIs1(21:21) = 'F'
C--------DWBA
         ECIs2(42:42) = 'T'
      ENDIF
      xmas_nejc = EJMass(Nejc)
      xmas_nnuc = (A(Nnuc)*AMUmev + XMAss(Nnuc))/AMUmev
      xratio = xmas_nnuc/(xmas_nejc + xmas_nnuc)
      IF (El.LT.0.D0) THEN
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
      CALL SETPOTS(Nejc,Nnuc,elab,ecms,xmas_nejc,xmas_nnuc,ak2,ikey)
C-----relativistic kinematics ?
      IF (IRElat(Nejc,Nnuc).GT.0 .OR. FLGrel.EQ.'y') ECIs1(8:8) = 'T'

      IF (IDRs.GT.0) THEN
C      Preparing dispersive CC input for ECIS
       ECIs1(10:10)='T'! ENERGY DEPENDENT POTENTIALS BY DISPERSION RELATIONS (GS)
       ECIs1(20:20)='T'! ENERGY DEPENDENT POTENTIALS BY DISPERSION RELATIONS (EXC.LEV.)
      ENDIF
C-----Only for target, find open channels
C-----At least ground state is always open !!, RCN 31/03/2001
      nd_nlvop = 1
      nd_cons = 1
      IF (Inlkey.NE.0) THEN
         DO j = 2, ND_nlv
            IF (.NOT.Ldwba .AND. ICOllev(j).GT.LEVcc) CYCLE
            nd_cons = nd_cons + 1
            eee = El - D_Elv(j)/xratio
            IF (eee.GT.0.0001) nd_nlvop = nd_nlvop + 1
         ENDDO
         IF (.NOT.Ldwba .AND. Inlkey.GT.0 .AND. nd_nlvop.EQ.1)
     &       WRITE (6,*)
     &               ' All inelastic channels are closed at this energy'
      ENDIF
C
C-----Considering only coupled levels if CC calculation
      ncollx = nd_cons
C
      iterm = 20
C-----For DWBA only one iteration is used
      IF (ECIs2(42:42).EQ.'T') iterm = 1
C-----Defining one potential for each collective level
C-----If channel is closed ground state potential is used for this level
      npp = nd_nlvop
C
      zerosp = 0.d0
      ldwmax = 2.4*1.25*A(Nnuc)
     &         **0.33333333*0.22*SQRT((xmas_nejc + xmas_nnuc)*elab)
C-----Maximum number of channel spin (increased to 100 for high energy scattering)
      njmax = MAX(ldwmax,30)

      lodd = .false.
      IF( (mod(nint(Z(Nnuc)),2).ne.0 .or. 
     >     mod(nint(A(Nnuc)-Z(Nnuc)),2).ne.0) .and.
     >     mod(nint(A(Nnuc)),2).ne.0 ) lodd = .true.  

      IF (Inlkey.EQ.0) THEN
C-------writing input
         OPEN (UNIT = 1,STATUS = 'unknown',FILE = 'ecSPH.inp')
C-------CARD 1 : Title
         WRITE (1,
     &          '(f6.2,'' MeV '',a8,'' on '',i3,a2,'': SPHERICAL OMP'')'
     &          ) El, PARname(ip), NINT(A(Nnuc)), NUC(NINT(Z(Nnuc)))
      ELSE
C-------writing input
         OPEN (UNIT = 1,STATUS = 'unknown',FILE = 'ecVIB.inp')
C-------CARD 1 : Title
         WRITE (1,
     &      '(f6.2,'' MeV '',a8,'' on '',i3,a2,'': VIBRATIONAL MODEL'')'
     &      ) El, PARname(ip), NINT(A(Nnuc)), NUC(NINT(Z(Nnuc)))
      ENDIF
C-----CARD 2
      WRITE (1,'(a50)') ECIs1
C-----CARD 3
      WRITE (1,'(a50)') ECIs2
C-----CARD 4
      WRITE (1,'(4i5)') ncollx, njmax, iterm, npp
C-----Matching radius
C-----CARD 5
      rmatch = 20.D0
      WRITE (1,'(10x,f10.5)') rmatch
C     To obtain Legendre expansion a blank card calling for default values of the expansion
      WRITE (1,*)
C     Matching radius calculated within ECIS
C     WRITE(1, *)
C-----ground state
C     ch = '-'
C     IF ( LVP(LEVtarg,Nnuc).GT.0 ) ch = '+'
C-----Important: Instead of using TARGET SPIN (XJLV(1,NNUC)) and PARITY(ch)
C-----A.Koning always used in PREGNASH SPIN=0, ch='+'
C-----It is justified for vibrational model and DWBA calculations
C-----so we are using zero spin and positive parity herehere
C-----NOT TRUE for rotational model calculations (see ecis_CCrot)
C     WRITE(1, '(f5.2,2i2,a1,5f10.5)')XJLV(1,NNUC),0,1, ch, elab,
      WRITE (1,'(f5.2,2i2,a1,5f10.5)') zerosp, 0, 1, '+', elab,
     &                                 SEJc(Nejc), xmas_nejc, xmas_nnuc,
     &                                 Z(Nnuc)*ZEJc(Nejc)
C-----0 phonon involved
      WRITE (1,'( )')
      IF (Inlkey.NE.0) THEN
C--------discrete levels
         nwrite = 1
         DO j = 2, ND_nlv
C--------All levels with icollev(j)>LEVcc should be calculated by DWBA
            IF (.NOT.Ldwba .AND. ICOllev(j).GT.LEVcc) GOTO 100
            ch = '-'
            IF (D_Lvp(j).GT.0) ch = '+'
C-----------If channel is closed ground state potential is used for this level
            eee = El - D_Elv(j)/xratio
            dtmp = D_Xjlv(j)
            ! making integer spin for odd nuclides CC levels in DWBA calculations
            if(Ldwba .and. lodd) dtmp = INT(D_Xjlv(j))   
            IF (eee.GE.0.0001) THEN
               nwrite = nwrite + 1
               WRITE (1,'(f5.2,2i2,a1,5f10.5)') dtmp, 0, nwrite,
     &                ch, D_Elv(j)
            ELSE
               WRITE (1,'(f5.2,2i2,a1,5f10.5)') dtmp, 0, 1, ch,
     &                D_Elv(j)
            ENDIF
            IF (Ldwba .OR. DIRect.EQ.3) THEN
C--------------If DWBA, all states are assumed to be one phonon
               WRITE (1,'(3i5)') 1, j - 1, 0
            ELSEIF (IPH(j).EQ.1) THEN
               WRITE (1,'(3i5)') IPH(j), j - 1, 0
            ELSE
C-----------two  phonon states if exist are formed from the quadrupole
C-----------quadrupole phonon spin is equal to 2+ phonon
               WRITE (1,'(3i5)') IPH(j), 1, 1
            ENDIF
  100    ENDDO
C
C--------deformations: phonon description
C
         DO j = 2, ND_nlv
C-----------All levels with icollev(j)>LEVcc should be calculated by DWBA
            IF (.NOT.Ldwba .AND. ICOllev(j).GT.LEVcc) CYCLE
            IF (Ldwba .OR. DIRect.EQ.3) THEN
C--------------If DWBA, all states are assumed to be one phonon
               WRITE (1,'(i5,5x,6f10.5)') INT(D_Xjlv(j)), D_Def(j,2)
            ELSE
C--------------only one phonon states need deformations as input
               IF (IPH(j).EQ.1) WRITE (1,'(i5,5x,6f10.5)')
     &                                 INT(D_Xjlv(j)), D_Def(j,2)
            ENDIF
         ENDDO
      ENDIF
C
C-----potential parameters
C-----1) groundstate
C     write(1,'(3f10.5)') v,rv,av
      IF (VOM(Nejc,Nnuc).NE.0.) THEN
         WRITE (1,'(3f10.5)') VOM(Nejc,Nnuc)-DWVNUM, RVOm(Nejc,Nnuc),
     &                        AVOm(Nejc,Nnuc)
      ELSE
         WRITE (1,'(3f10.5)') 0., 0., 0.
      ENDIF
C-----write(1,'(3f10.5)') w,rw,aw
      IF (WOMv(Nejc,Nnuc).NE.0.) THEN
         WRITE (1,'(3f10.5)') WOMv(Nejc,Nnuc), RWOmv(Nejc,Nnuc),
     &                        AWOmv(Nejc,Nnuc)
      ELSE
         WRITE (1,'(3f10.5)') 0., 0., 0.
      ENDIF
C-----write(1,'(3f10.5)') vd,rvd,avd
      IF (VOMs(Nejc,Nnuc).NE.0.) THEN
         WRITE (1,'(3f10.5)') VOMs(Nejc,Nnuc), RWOm(Nejc,Nnuc),
     &                        AWOm(Nejc,Nnuc)
      ELSE
         WRITE (1,'(3f10.5)') 0., 0., 0.
      ENDIF
C-----write(1,'(3f10.5)') wd,rwd,awd
      IF (WOMs(Nejc,Nnuc).NE.0.) THEN
         WRITE (1,'(3f10.5)') WOMs(Nejc,Nnuc), RWOm(Nejc,Nnuc),
     &                        AWOm(Nejc,Nnuc)
         IF (SFIom(Nejc,Nnuc).LT.0.0D0) THEN
            WRITE (*,*)
            WRITE (*,*) ' ERROR !!!'
            WRITE (*,*)
     &                 ' ECIS can not be used with Gaussian formfactors'
            WRITE (*,*) ' for imaginary surface contribution '
            WRITE (*,*) ' Change OMP potential for elastic channel'
            WRITE (6,*)
            WRITE (6,*) ' ERROR !!!'
            WRITE (6,*)
     &                 ' ECIS can not be used with Gaussian formfactors'
            WRITE (6,*) ' for imaginary surface contribution'
            WRITE (6,*) ' Change OMP potential for elastic channel'
            STOP ' No Gaussian form factors in ECIS!'
         ENDIF
      ELSE
         WRITE (1,'(3f10.5)') 0., 0., 0.
      ENDIF
C-----write(1,'(3f10.5)') vvso,rrvso,aavso
      IF (VSO(Nejc,Nnuc).NE.0.) THEN
         WRITE (1,'(3f10.5)') VSO(Nejc,Nnuc), RVSo(Nejc,Nnuc),
     &                        AVSo(Nejc,Nnuc)
      ELSE
         WRITE (1,'(3f10.5)') 0., 0., 0.
      ENDIF
C-----write(1,'(3f10.5)') wwso,rwso,awso
      IF (WSO(Nejc,Nnuc).NE.0.) THEN
         WRITE (1,'(3f10.5)') WSO(Nejc,Nnuc), RWSo(Nejc,Nnuc),
     &                        AWSo(Nejc,Nnuc)
      ELSE
         WRITE (1,'(3f10.5)') 0., 0., 0.
      ENDIF
C-----write(1,'(3f10.5)') rc,0.,0.
      WRITE (1,'(3f10.5)') RCOul(Nejc,Nnuc), ACOul(Nejc,Nnuc), 0.
      WRITE (1,'(3f10.5)') 0., 0., 0.
C     IF (IDRs.GT.0) write(1,'(2I5,6F10.5)') -1,0,0.d0,DWVNUM
      IF (IDRs.GT.0) write(1,'(2I5,6F10.5)') -1,0,elab,DWVNUM

      IF (Inlkey.NE.0) THEN
C
C------2) discrete levels
         DO j = 2, ND_nlv
C           All levels with icollev(j)>LEVcc should be calculated by DWBA
            IF (.NOT.Ldwba .AND. ICOllev(j).GT.LEVcc) GOTO 200
            eee = El - D_Elv(j)/xratio
C-----------If channel is closed ground state potential is used for this level
            IF (eee.GE.0.0001) THEN
C--------------SETPOTS  : subroutine for optical model parameters
C--------------From     cms system to Lab
               ecms = eee
               ikey = +1
C--------------Transformation of energies from laboratory to center-of-mass
C--------------if needed is done inside SETPOTS() -> OMPAR()
C
               CALL SETPOTS(Nejc,Nnuc,elabe,ecms,xmas_nejc,xmas_nnuc,
     &                      ak2,ikey)
C
C--------------potential parameters
C--------------write(1,'(3f10.5)') v,rv,av
               IF (VOM(Nejc,Nnuc).NE.0.) THEN
                  WRITE (1,'(3f10.5)') VOM(Nejc,Nnuc)-DWVNUM,
     &                                 RVOm(Nejc,Nnuc),
     &                                 AVOm(Nejc,Nnuc)
               ELSE
                  WRITE (1,'(3f10.5)') 0., 0., 0.
               ENDIF
C--------------write(1,'(3f10.5)') w,rw,aw
               IF (WOMv(Nejc,Nnuc).NE.0.) THEN
                  WRITE (1,'(3f10.5)') WOMv(Nejc,Nnuc),
     &                                 RWOmv(Nejc,Nnuc),
     &                                 AWOmv(Nejc,Nnuc)
               ELSE
                  WRITE (1,'(3f10.5)') 0., 0., 0.
               ENDIF
C--------------write(1,'(3f10.5)') vd,rvd,avd
               IF (VOMs(Nejc,Nnuc).NE.0.) THEN
                  WRITE (1,'(3f10.5)') VOMs(Nejc,Nnuc),
     &                                 RWOm(Nejc,Nnuc),
     &                                 AWOm(Nejc,Nnuc)
               ELSE
                  WRITE (1,'(3f10.5)') 0., 0., 0.
               ENDIF
C--------------write(1,'(3f10.5)') wd,rwd,awd
               IF (WOMs(Nejc,Nnuc).NE.0.) THEN
                  WRITE (1,'(3f10.5)') WOMs(Nejc,Nnuc),
     &                                 RWOm(Nejc,Nnuc),
     &                                 AWOm(Nejc,Nnuc)
               ELSE
                  WRITE (1,'(3f10.5)') 0., 0., 0.
               ENDIF
C--------------write(1,'(3f10.5)') vvso,rrvso,aavso
               IF (VSO(Nejc,Nnuc).NE.0.) THEN
                  WRITE (1,'(3f10.5)') VSO(Nejc,Nnuc),
     &                                 RVSo(Nejc,Nnuc),
     &                                 AVSo(Nejc,Nnuc)
               ELSE
                  WRITE (1,'(3f10.5)') 0., 0., 0.
               ENDIF
C--------------write(1,'(3f10.5)') wwso,rwso,awso
               IF (WSO(Nejc,Nnuc).NE.0.) THEN
                  WRITE (1,'(3f10.5)') WSO(Nejc,Nnuc),
     &                                 RWSo(Nejc,Nnuc),
     &                                 AWSo(Nejc,Nnuc)
               ELSE
                  WRITE (1,'(3f10.5)') 0., 0., 0.
               ENDIF
C--------------write(1,'(3f10.5)') rc,0.,0.
               WRITE (1,'(3f10.5)')
     &                RCOul(Nejc,Nnuc), ACOul(Nejc,Nnuc), 0.
               WRITE (1,'(3f10.5)') 0., 0., 0.
C              IF (IDRs.GT.0) write(1,'(2I5,6F10.5)') -1,0,0.d0,DWVNUM
               IF (IDRs.GT.0) write(1,'(2I5,6F10.5)') -1,0,elabe,DWVNUM	   
            ENDIF
  200    ENDDO
      ENDIF

      IF (ICAlangs.GT.0) THEN
        WRITE (1,'(3f10.5)') 0.D0, 180.0D0, 180.D0
        WRITE (1,'(2i5)') ICAlangs,0
        DO i = 1, ICAlangs
          WRITE(1,'(A1,I1,I3,2I5,5X,3F10.5)') 'F', 0, NANgela, i, 0,
     1                                    1.0D0, 1.0D0, 0.0D0
          DO iang = 1, NANgela
            WRITE(1,'(5F10.5)') ANGles(iang), 1.0d3, 1.0d2, 0.0d0, 0.0d0
           END DO
         END DO
       ELSE
        WRITE (1,'(3f10.5)') 0.D0, angstep, 180.D0
       ENDIF

      WRITE (1,'(3hFIN)')
      CLOSE (UNIT = 1)
C
C-----Running ECIS
C
      IF (Inlkey.EQ.0) THEN
         CALL ECIS('ecSPH.inp','ECIS_SPH.out')
      ELSE
         CALL ECIS('ecVIB.inp','ECIS_VIB.out')
      ENDIF
      END


      SUBROUTINE ECIS_CCVIBROT(Nejc,Nnuc,El,Inlkey)
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
C     INTEGER NINT
C
      INCLUDE "dimension.h"
      INCLUDE "global.h"
      INCLUDE "pre_ecis.h"
C-----For dispersive optical model potentials
C-----It is ripl2empireS.h because it must be compatible
C-----with global.h declarations so some variables must be renamed

      INCLUDE 'ripl2empireS.h'
C
C Dummy arguments
C
      DOUBLE PRECISION El
      INTEGER Inlkey, Nejc, Nnuc
C
C COMMON variables
C
      REAL*8 DWVNUM 
      INTEGER IDRs
      COMMON /DISPER/ DWVNUM, IDRs 
C
C Local variables
C
      DOUBLE PRECISION ak2, angstep, ecms, eee, elab, elabe, rmatch,
     &                 xmas_nejc, xmas_nnuc, xratio
      CHARACTER*1 ch
      DOUBLE PRECISION DABS
      INTEGER ikey, ip, iterm, j, jdm, k, ldwmax, lev(NDLV), 
     &        nd_cons, nd_nlvop, ncollm, njmax, npho, npp, nwrite
      INTEGER INT, NINT
      IF (AEJc(Nejc).EQ.1.D0 .AND. ZEJc(Nejc).EQ.0.D0) ip = 1
      IF (AEJc(Nejc).EQ.1.D0 .AND. ZEJc(Nejc).EQ.1.D0) ip = 2
      IF (AEJc(Nejc).EQ.2.D0 .AND. ZEJc(Nejc).EQ.1.D0) ip = 3
      IF (AEJc(Nejc).EQ.3.D0 .AND. ZEJc(Nejc).EQ.1.D0) ip = 4
      IF (AEJc(Nejc).EQ.3.D0 .AND. ZEJc(Nejc).EQ.2.D0) ip = 5
      IF (AEJc(Nejc).EQ.4.D0 .AND. ZEJc(Nejc).EQ.2.D0) ip = 6
      IF (AEJc(Nejc).EQ.6.D0 .AND. ZEJc(Nejc).EQ.3.D0) ip = 7
      IF (AEJc(Nejc).EQ.7.D0 .AND. ZEJc(Nejc).EQ.3.D0) ip = 8
      IF (AEJc(Nejc).EQ.7.D0 .AND. ZEJc(Nejc).EQ.4.D0) ip = 9
C-----Data initialization
C     angstep = 2.5
      angstep = 180.d0/(NANgela-1)
      CALL INIT(Nejc,Nnuc)
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

C-----ECIS iteration scheme is used. (To be checked following Harm Wienke message)
      ECIs1(21:21) = 'F'
C-----Usual coupled equations instead of ECIS scheme is used
C     for non-zero spins or energies below 3 MeV
C-----Spin-orbit potential must be not deformed !!
      if(XJLv(1,Nnuc).gt.0.d0 .OR. DABS( - El).LT.3.d0)
     >  ECIs1(21:21) = 'T'
C-----Shift to coupled equations if convergence is not achieved
      ECIs1(23:23) = 'T'
C-----Calculations at experimental angles
      IF (ICAlangs.GT.0) ECIs1(31:31) = 'T'
      ECIs2 = BECis2
C-----Angular distribution is calculated
      ECIs2(14:14) = 'T'
C-----Penetrabilities punched on cards
      ECIs2(13:13) = 'T'
C-----Legendre coefficients output
      ECIs2(15:15) = 'T'
C-----Cmatrix output
      ECIs2(5:5) = 'F'
C-----Smatrix output
      ECIs2(6:6) = 'T'
      ECIs2(10:10) = 'T'
C-----DWBA option added
      IF (DIRect.EQ.3) THEN
C-----Iteration scheme used for DWBA (To be checked following Harm Wienke message)
         ECIs1(21:21) = 'F'
C--------ECIs1(23:23) = 'F'
C--------DWBA
         ECIs2(42:42) = 'T'
      ENDIF
      xmas_nejc = EJMass(Nejc)
      xmas_nnuc = (A(Nnuc)*AMUmev + XMAss(Nnuc))/AMUmev
      xratio = xmas_nnuc/(xmas_nejc + xmas_nnuc)
C-----From cms system to Lab (ECIS do inverse convertion)
      IF (El.LT.0.D0) THEN
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
      CALL SETPOTS(Nejc,Nnuc,elab,ecms,xmas_nejc,xmas_nnuc,ak2,ikey)
C-----relativistic kinematics ?
      IF (IRElat(Nejc,Nnuc).GT.0 .OR. FLGrel.EQ.'y') ECIs1(8:8) = 'T'

      IF (IDRs.GT.0) THEN
C      Preparing dispersive CC input for ECIS
       ECIs1(10:10)='T'! ENERGY DEPENDENT POTENTIALS BY DISPERSION RELATIONS (GS)
       ECIs1(20:20)='T'! ENERGY DEPENDENT POTENTIALS BY DISPERSION RELATIONS (EXC.LEV.)
      ENDIF
C
C-----Only for target, find open channels
C-----At least ground state is always open !!, RCN 31/03/2001
C-----nd_nlvop = 0
      nd_nlvop = 1
      IF (ND_nlv.GT.0) THEN
         nd_cons = 1
         DO j = 2, ND_nlv
C-----------All levels with icollev(j)>LEVcc should be calculated by DWBA
            IF (Inlkey.EQ.0 .AND. ICOllev(j).GT.LEVcc) CYCLE
C-----------All levels with icollev(j)<LEVcc should be calculated by CC
            IF (Inlkey.EQ.1 .AND. ICOllev(j).LT.LEVcc) CYCLE
            nd_cons = nd_cons + 1
            eee = El - D_Elv(j)/xratio
            IF (eee.GT.0.0001) nd_nlvop = nd_nlvop + 1
         ENDDO
      ENDIF
      IF (nd_nlvop.EQ.1) WRITE (6,*)
     &               ' All inelastic channels are closed at this energy'
C-----Considering even closed channels in calculations
      ncollm = nd_cons
C
      iterm = 20
C-----For DWBA only one iteration is used
      IF (ECIs2(42:42).EQ.'T' .OR. Inlkey.EQ.1) iterm = 1
C
C-----Defining one potential for each collective level
C-----If channel is closed ground state potential is used for this level
      npp = nd_nlvop
C
      ldwmax = 2.4*1.25*A(Nnuc)
     &         **0.33333333*0.22*SQRT((xmas_nejc + xmas_nnuc)*elab)
C-----Maximum number of channel spin
      njmax = MAX(ldwmax,30)
C-----Writing input
      OPEN (UNIT = 1,STATUS = 'unknown',FILE = 'ecVIBROT.inp')
C-----CARD 1 : Title
      WRITE (1,
     &'(f6.2,'' MeV '',a8,'' on '',i3,a2,'': VIBR-ROTATIONAL CC     '')'
     &) El, PARname(ip), NINT(A(Nnuc)), NUC(NINT(Z(Nnuc)))
C-----CARD 2
      WRITE (1,'(a50)') ECIs1
C-----CARD 3
      WRITE (1,'(a50)') ECIs2
C-----CARD 4
C-----make sure that all contributions to s-wave scattering are included
      ilv = 1
      If(Nnuc.eq.0) ilv = Levtarg
      jdm = XJLv(ilv,Nnuc) + SEJc(Nejc) + 0.5
      WRITE (1,'(4i5,30x,i5)') ncollm, njmax, iterm, npp, jdm
C-----CARD 5
      rmatch = 20.D0
      WRITE (1,'(10x,f10.5)') rmatch
C     To obtain Legendre expansion a blank card calling for default values of the expansion
      WRITE(1, *)
C-----Matching radius calculated within ECIS
C     WRITE(1, *)
      ch = '-'
      IF (LVP(ilv,Nnuc).GT.0) ch = '+'
      WRITE (1,'(f5.2,2i2,a1,5f10.5)') XJLv(ilv,Nnuc), 0, 1, ch,
     &                                 elab, SEJc(Nejc), xmas_nejc,
     &                                 xmas_nnuc, Z(Nnuc)*ZEJc(Nejc)
C-----Discrete levels
      npho = 0
      nwrite = 1
      DO j = 2, ND_nlv
C--------All levels with icollev(j)>LEVcc should be calculated by DWBA
         IF (Inlkey.EQ.0 .AND. ICOllev(j).GT.LEVcc) CYCLE
C--------All levels with icollev(j)<LEVcc should be calculated by CC
         IF (Inlkey.EQ.1 .AND. ICOllev(j).LT.LEVcc) CYCLE
         ch = '-'
         IF (D_Lvp(j).GT.0) ch = '+'
C--------If channel is closed ground state potential is used for this level
         eee = El - D_Elv(j)/xratio
C
C        Vibrational-rotational model modified as proposed
C        by Harm Wienke, March 31, 2005 (vibrational bands can be included)
C
C--------In vibrational-rotational model IPH(j) is phonon number
         IF (eee.GE.0.0001) THEN
           nwrite = nwrite + 1
           IF(IPH(j).ge.1) then
               WRITE (1,'(f5.2,2i2,a1,5f10.5)') D_Xjlv(j), 1, nwrite,
     &             ch, D_Elv(j)
            ELSE
               WRITE (1,'(f5.2,2i2,a1,5f10.5)') D_Xjlv(j), 0, nwrite,
     &             ch, D_Elv(j)
            ENDIF
         ELSE
           IF(IPH(j).ge.1) then
               WRITE (1,'(f5.2,2i2,a1,5f10.5)') D_Xjlv(j), 1, 1, ch,
     &             D_Elv(j)
            ELSE
               WRITE (1,'(f5.2,2i2,a1,5f10.5)') D_Xjlv(j), 0, 1, ch,
     &             D_Elv(j)
            ENDIF
         ENDIF
         IF (IPH(j).NE.0) THEN
           if(IPH(j).ne.IPH(j-1)) then
               npho = npho + 1
               lev(npho) = j
           endif
            WRITE (1,'(10i5)') 1, IPH(j)
         ENDIF
      ENDDO
C-----Description of phonons
      IF (npho.GT.0) THEN
C--------In vibrational rotational model the IPH(j) array contains the
C--------l orbital angular momentum of the phonon.
C--------We are assuming that orbital angular momentum of the phonon is
C--------equal INT(J). The k magnetic quantum number of the vibration is
C--------assumed to be zero !!!!
C--------L_PHO(lev(j))  = IPH(j)
C--------L3_pho(lev(j)) = 0
         DO j = 1, npho
            WRITE (1,'(2i5,6f10.5)') INT(D_Llv(lev(j)) + 0.1),
     &      INT(D_Klv(lev(j))+ 0.1),D_Def(lev(j),2)
         ENDDO
      ENDIF
C-----Deformation of rotational band (only ground state band is present)
C-----IdefCC   = maximum degree of deformation
C-----LMaxCC   = maximum L in multipole decomposition
      WRITE (1,'(2i5,f10.5)') IDEfcc, LMAxcc, D_Xjlv(1)
      WRITE (1,'(6f10.5)') (D_Def(1,k),k = 2,IDEfcc,2)
C-----Potential parameters
C-----1) groundstate
C-----write(1,'(3f10.5)') v,rv,av
C     write(1,'(3f10.5)') v,rv,av
      IF (VOM(Nejc,Nnuc).NE.0.) THEN
         WRITE (1,'(3f10.5)') VOM(Nejc,Nnuc)-DWVNUM, RVOm(Nejc,Nnuc),
     &                        AVOm(Nejc,Nnuc)
      ELSE
         WRITE (1,'(3f10.5)') 0., 0., 0.
      ENDIF
C-----write(1,'(3f10.5)') w,rw,aw
      IF (WOMv(Nejc,Nnuc).NE.0.) THEN
         WRITE (1,'(3f10.5)') WOMv(Nejc,Nnuc), RWOmv(Nejc,Nnuc),
     &                        AWOmv(Nejc,Nnuc)
      ELSE
         WRITE (1,'(3f10.5)') 0., 0., 0.
      ENDIF
C-----write(1,'(3f10.5)') vd,rvd,avd
      IF (VOMs(Nejc,Nnuc).NE.0.) THEN
         WRITE (1,'(3f10.5)') VOMs(Nejc,Nnuc), RWOm(Nejc,Nnuc),
     &                        AWOm(Nejc,Nnuc)
      ELSE
         WRITE (1,'(3f10.5)') 0., 0., 0.
      ENDIF
C-----write(1,'(3f10.5)') wd,rwd,awd
      IF (WOMs(Nejc,Nnuc).NE.0.) THEN
         WRITE (1,'(3f10.5)') WOMs(Nejc,Nnuc), RWOm(Nejc,Nnuc),
     &                        AWOm(Nejc,Nnuc)
         IF (SFIom(Nejc,Nnuc).LT.0.0D0) THEN
            WRITE (*,*)
            WRITE (*,*) ' ERROR !!!'
            WRITE (*,*)
     &                 ' ECIS can not be used with Gaussian formfactors'
            WRITE (*,*) ' for imaginary surface contribution '
            WRITE (*,*) ' Change OMP potential for elastic channel'
            WRITE (6,*)
            WRITE (6,*) ' ERROR !!!'
            WRITE (6,*)
     &                 ' ECIS can not be used with Gaussian formfactors'
            WRITE (6,*) ' for imaginary surface contribution'
            WRITE (6,*) ' Change OMP potential for elastic channel'
            STOP ' No Gaussian form factors in ECIS!'
         ENDIF
      ELSE
         WRITE (1,'(3f10.5)') 0., 0., 0.
      ENDIF
C-----write(1,'(3f10.5)') vvso,rrvso,aavso
      IF (VSO(Nejc,Nnuc).NE.0.) THEN
         WRITE (1,'(3f10.5)') VSO(Nejc,Nnuc), RVSo(Nejc,Nnuc),
     &                        AVSo(Nejc,Nnuc)
      ELSE
         WRITE (1,'(3f10.5)') 0., 0., 0.
      ENDIF
C-----write(1,'(3f10.5)') wwso,rwso,awso
      IF (WSO(Nejc,Nnuc).NE.0.) THEN
         WRITE (1,'(3f10.5)') WSO(Nejc,Nnuc), RWSo(Nejc,Nnuc),
     &                        AWSo(Nejc,Nnuc)
      ELSE
         WRITE (1,'(3f10.5)') 0., 0., 0.
      ENDIF
      WRITE (1,'(3f10.5)') RCOul(Nejc,Nnuc), ACOul(Nejc,Nnuc), 0.

      WRITE (1,'(3f10.5)') 0., 0., 0.

C     IF (IDRs.GT.0) write(1,'(2I5,6F10.5)') -1,0,0.d0,DWVNUM
      IF (IDRs.GT.0) write(1,'(2I5,6F10.5)') -1,0,elab,DWVNUM

C-----2) discrete levels
      DO j = 2, ND_nlv
C--------All levels with icollev(j)>LEVcc should be calculated by DWBA
         IF (Inlkey.EQ.0 .AND. ICOllev(j).GT.LEVcc) CYCLE
C--------All levels with icollev(j)<LEVcc should be calculated by CC
         IF (Inlkey.EQ.1 .AND. ICOllev(j).LT.LEVcc) CYCLE
C--------If channel is closed ground state potential is used for this level
         eee = El - D_Elv(j)/xratio
         IF (eee.GE.0.0001) THEN
C--------SETPOTS : subroutine for optical model parameters
C--------From  cms system to Lab
            ecms = eee
            ikey = +1
C
C-----------Transformation of energies from laboratory to center-of-mass if
C-----------needed is done inside SETPOTS() -> OMPAR()
C
            CALL SETPOTS(Nejc,Nnuc,elabe,ecms,xmas_nejc,xmas_nnuc,
     &                   ak2,ikey)
C
C-----------potential parameters
C-----------write(1,'(3f10.5)') v,rv,av
            IF (VOM(Nejc,Nnuc).NE.0.) THEN
               WRITE (1,'(3f10.5)') VOM(Nejc,Nnuc)-DWVNUM,
     &                              RVOm(Nejc,Nnuc),
     &                              AVOm(Nejc,Nnuc)
            ELSE
               WRITE (1,'(3f10.5)') 0., 0., 0.
            ENDIF
C-----------write(1,'(3f10.5)') w,rw,aw
            IF (WOMv(Nejc,Nnuc).NE.0.) THEN
               WRITE (1,'(3f10.5)') WOMv(Nejc,Nnuc),
     &                              RWOmv(Nejc,Nnuc),
     &                              AWOmv(Nejc,Nnuc)
            ELSE
               WRITE (1,'(3f10.5)') 0., 0., 0.
            ENDIF
C-----------write(1,'(3f10.5)') vd,rvd,avd
            IF (VOMs(Nejc,Nnuc).NE.0.) THEN
               WRITE (1,'(3f10.5)') VOMs(Nejc,Nnuc),
     &                              RWOm(Nejc,Nnuc),
     &                              AWOm(Nejc,Nnuc)
            ELSE
               WRITE (1,'(3f10.5)') 0., 0., 0.
            ENDIF
C-----------write(1,'(3f10.5)') wd,rwd,awd
            IF (WOMs(Nejc,Nnuc).NE.0.) THEN
               WRITE (1,'(3f10.5)') WOMs(Nejc,Nnuc),
     &                              RWOm(Nejc,Nnuc),
     &                              AWOm(Nejc,Nnuc)
            ELSE
               WRITE (1,'(3f10.5)') 0., 0., 0.
            ENDIF
C-----------write(1,'(3f10.5)') vvso,rrvso,aavso
            IF (VSO(Nejc,Nnuc).NE.0.) THEN
               WRITE (1,'(3f10.5)') VSO(Nejc,Nnuc),
     &                              RVSo(Nejc,Nnuc),
     &                              AVSo(Nejc,Nnuc)
            ELSE
               WRITE (1,'(3f10.5)') 0., 0., 0.
            ENDIF
C-----------write(1,'(3f10.5)') wwso,rwso,awso
            IF (WSO(Nejc,Nnuc).NE.0.) THEN
               WRITE (1,'(3f10.5)') WSO(Nejc,Nnuc),
     &                              RWSo(Nejc,Nnuc),
     &                              AWSo(Nejc,Nnuc)
            ELSE
               WRITE (1,'(3f10.5)') 0., 0., 0.
            ENDIF
            WRITE (1,'(3f10.5)')
     &                RCOul(Nejc,Nnuc), ACOul(Nejc,Nnuc), 0.
            WRITE (1,'(3f10.5)') 0., 0., 0.
C           IF (IDRs.GT.0) write(1,'(2I5,6F10.5)') -1,0,0.d0,DWVNUM
            IF (IDRs.GT.0) write(1,'(2I5,6F10.5)') -1,0,elabe,DWVNUM	
         ENDIF
      ENDDO
C
C-----Angular distribution step
C
      IF (ICAlangs.GT.0) THEN
        WRITE (1,'(3f10.5)') 0.D0, 180.0D0, 180.D0
        WRITE (1,'(2i5)') ICAlangs,0
        DO i = 1, ICAlangs
          WRITE(1,'(A1,I1,I3,2I5,5X,3F10.5)') 'F', 0, NANgela, i, 0,
     1                                    1.0D0, 1.0D0, 0.0D0
          DO iang = 1, NANgela
            WRITE(1,'(5F10.5)') ANGles(iang), 1.0D3, 1.0D2, 0.0D0, 0.0D0
           END DO
         END DO
       ELSE
        WRITE (1,'(3f10.5)') 0.D0, angstep, 180.D0
       ENDIF
      WRITE (1,'(3hFIN)')
      CLOSE (UNIT = 1)
C-----Running ECIS
      IF (npho.GT.0) THEN
         CALL ECIS('ecVIBROT.inp','ECIS_VIBROT.out')
      ELSE
         CALL ECIS('ecVIBROT.inp','ECIS_ROT.out')
      ENDIF
      END

      SUBROUTINE SETPOTS(Nejc,Nnuc,Eilab,Eicms,Mi,Mt,Ak2,Ikey)
C
C     Transfers o.m. parameters to ECIS
C
C     Ener must be in LAB system
C
C     ener = Elab
C
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C Dummy arguments
C
      DOUBLE PRECISION Ak2, Eicms, Eilab, Mi, Mt
      INTEGER Ikey, Nejc, Nnuc
C
C Local variables
C
      INTEGER komp
      IF (CCCalc) THEN
C-----calculate o.m. parameters for ejectile NEJC on target NNUC at energy ENER
         komp = 33
         CALL OMPAR(Nejc,Nnuc,Eilab,Eicms,Mi,Mt,Ak2,komp,Ikey)
      ELSE
C-----calculate o.m. parameters for ejectile NEJC on target NNUC at energy ENER
         komp = 29
         CALL OMPAR(Nejc,Nnuc,Eilab,Eicms,Mi,Mt,Ak2,komp,Ikey)
      ENDIF
      END

      SUBROUTINE KINEMA(El,E1,Mi,Mt,Ak2,Iopt,Relcal)
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
C COMMON variables
C
      DOUBLE PRECISION AMPi, AMUmev, AMUneu, AMUpro, CETa, CSO, ELE2,
     &                 HHBarc, PI, AMUele
      COMMON /CONSTANT/ AMUmev, PI, CETa, CSO, AMPi,
     &                  ELE2, HHBarc, AMUneu, AMUpro, AMUele
C
C Dummy arguments
C
      DOUBLE PRECISION Ak2, E1, El, Mi, Mt
      INTEGER Iopt
      LOGICAL Relcal
C
C Local variables
C
      DOUBLE PRECISION ck2, mtot, one, p2, two
      DOUBLE PRECISION DSQRT
      DATA one/1.0D+00/
      DATA two/2.0D+00/
      ck2 = (two*AMUmev)/(HHBarc**2)
      mtot = Mi + Mt
      IF (Iopt.EQ.1) THEN
C
C--------From lab to CM (the input quantity is Elab)
C
         IF (.NOT.Relcal) THEN
C
C-----------Classical    kinematics
C
            E1 = El*Mt/mtot
            Ak2 = ck2*Mi*Mt/mtot*E1
         ELSE
C
C-----------Relativistic kinematics
C
            E1 = AMUmev*mtot*
     &           (DSQRT(one + two*El/(AMUmev*Mt*((one+Mi/Mt)**2)))
     &           - one)
            p2 = (El*(El + two*AMUmev*Mi))
     &           /((one + Mi/Mt)**2 + two*El/(AMUmev*Mt))
            Ak2 = p2/(HHBarc*HHBarc)
C           etoti = DSQRT((AMUmev*Mi)**2 + p2)
C           etott = DSQRT((AMUmev*Mt)**2 + p2)
C           Amu = etoti*etott/(etoti + etott)
C           Amu = Amu/AMUmev
         ENDIF
C
C--------From  CM to lab (the input quantity is Ecm)
C
      ELSEIF (.NOT.Relcal) THEN
C
C--------Classical    kinematics
C
         El = E1*mtot/Mt
         Ak2 = ck2*Mi*Mt/mtot*E1
      ELSE
C
C--------Relativistic kinematics
C
         El = E1*(E1 + two*AMUmev*mtot)/(two*AMUmev*Mt)
         p2 = E1*(E1 + two*AMUmev*mtot)*(E1 + two*AMUmev*Mi)
     &        *(E1 + two*AMUmev*Mt)/((two*(E1+AMUmev*mtot))**2)
         Ak2 = p2/(HHBarc*HHBarc)
C        etoti = DSQRT((AMUmev*Mi)**2 + p2)
C        etott = DSQRT((AMUmev*Mt)**2 + p2)
C        Amu = etoti*etott/(etoti + etott)
C        Amu = Amu/AMUmev
C
C
      ENDIF
C
      END
C
C     From om-retrieve.f (RIPL-3 interface)
C     RCN,  Feb 28, 2005
C
      SUBROUTINE OPTMOD(El,Vlib,Rlib,Alib)
C
C     Routine to generate input for ECIS96 from RIPL-2 library
C
C----------------------------------------------------------------
C     Cut and paste place
C----------------------------------------------------------------
C
C     To use dispersive optical model package
C     Nonlocality constants alpha fixed according to Mahaux 1991
C
      INCLUDE "ripl2empire.h"
C
C COMMON variables
C
      REAL*8 AAV, AS, BS, BV, CS, EEE, EF, EP, WDE, WVE, EA
	REAL*8 DWVNUM
      INTEGER IQ, N, NNS, NNV, IDRs
      COMMON /DENERGY/ EEE, EF, EP, EA
      COMMON /PDATAS/ AS, BS, CS, NNS, IQ
      COMMON /PDATAV/ AAV, BV, NNV
      COMMON /WENERG/ WDE, WVE
      COMMON /DISPER/ DWVNUM, IDRs 
C
C Dummy arguments
C
      REAL*8 El
      REAL*8 Alib(6), Rlib(6), Vlib(6)
C
C Local variables
C
      REAL*8 aavso, alphav, alpha_pb, beta_pb, bvso, derdws, derdwv,
     &       dtmp, dws, dwv, dwvso, gamma_pb, vdcoul, vnonl, vscoul,
     &       vvcoul
      REAL b(6,NDIM1,15), dtmp1, dwmin, dwplus, dwsm, dwsp, elf,
     &     encoul2, pi, t12der, t1der, t1m, t1p, t2der, t2m, t2p, vc,
     &     vnonlm, vnonlp
      DOUBLE PRECISION DBLE
      REAL*8 DELTA_WD, DELTA_WV, DOM_INT, DOM_INT_T1, DOM_INT_T2,
     &       DOM_INT_WS, DOM_INT_WV, VHF, WDF, WVF
      INTEGER i, j, jab, jc, jp, nn
      INTEGER IABS, INT, MIN0, NINT
      EXTERNAL DELTA_WD, DELTA_WV, WDF, WVF
      DATA alphav/1.65D0/
C
C-----Generate optical model parameters for ECIS
C
      pi = ACOS( - 1.D0)
      RC = 0.d0
      ACOu = 0.d0

      IDRs = 0 ! Imaginary and HF geometry in dispersive potentials are the same

      IF (JCOul.GE.1) THEN
         jc = 1
         DO j = 1, JCOul
            IF (El.GT.ECOul(j)) jc = j + 1
         ENDDO
         jc = MIN0(jc,JCOul)
         RC = RCOul0(jc)*ATAr**( - 1./3.) + RCOul(jc) + RCOul1(jc)
     &        *ATAr**( - 2./3.) + RCOul2(jc)*ATAr**( - 5./3.)
         ACOu = ACOul(jc)
      ENDIF
      encoul2 = 0.
      IF (RC.GT.0.) encoul2 = 1.73*ZTAr/(RC*ATAr**(1./3.))

	DWVNUM = 0.d0

      DO i = 1, 6
         vdcoul = 0.D0
         vvcoul = 0.D0
         vscoul = 0.D0
         derdwv = 0.d0
         derdws = 0.d0
         dwvnonl = 0.d0
         vc = 0.d0
         Rlib(i) = 0.d0
         Alib(i) = 0.d0
         Vlib(i) = 0.d0
         jab = IABS(JRAnge(i))
         IF (JRAnge(i).GE.1) THEN
            jp = 1
            DO j = 1, jab
               IF (El.GT.EPOt(i,j)) jp = j + 1
            ENDDO
            j = MIN0(jp,jab)
            EF = DBLE(INT(100000*EFErmi))/100000
C-----------The IF block should be added on upgrade from the om-retrieve code
            IF (POT(i,j,18).NE.0.) THEN
               EF = POT(i,j,18) + POT(i,j,19)*ATAr
               EFErmi = EF
            ENDIF
            elf = El - EF
C
C-----------Calculate radius and diffuseness parameters
            IF (RCO(i,j,13).EQ.0.) THEN
C--------------RCN, 08/2004, to handle new extension to the OMP RIPL-2 format
               Rlib(i) = ABS(RCO(i,j,1)) + RCO(i,j,3)*ETA + RCO(i,j,4)
     &                   /ATAr + RCO(i,j,5)/SQRT(ATAr) + RCO(i,j,6)
     &                   *ATAr**(2./3.) + RCO(i,j,7)*ATAr + RCO(i,j,8)
     &                   *ATAr**2 + RCO(i,j,9)*ATAr**3 + RCO(i,j,10)
     &                   *ATAr**(1./3.) + RCO(i,j,11)*ATAr**( - 1./3.)
     &                   + RCO(i,j,2)*El + RCO(i,j,12)*El*El
            ELSE
C--------------RCN, 09/2004, to handle new extension to the OMP RIPL-2 format
               nn = INT(RCO(i,j,7))
               Rlib(i) = (ABS(RCO(i,j,1)) + RCO(i,j,2)*ATAr)
     &                   *(1.D0 - (RCO(i,j,3) + RCO(i,j,4)*ATAr)
     &                   *elf**nn/
     &                   (elf**nn + (RCO(i,j,5)+RCO(i,j,6)*ATAr)**nn))
            ENDIF

            Alib(i) = ABS(ACO(i,j,1)) + ACO(i,j,2)*El + ACO(i,j,3)
     &                *ETA + ACO(i,j,4)/ATAr + ACO(i,j,5)/SQRT(ATAr)
     &                + ACO(i,j,6)*ATAr**(2./3.) + ACO(i,j,7)
     &                *ATAr + ACO(i,j,8)*ATAr**2 + ACO(i,j,9)*ATAr**3 +
     &                ACO(i,j,10)*ATAr**(1./3.) + ACO(i,j,11)
     &                *ATAr**( - 1./3.)

            IF (i.eq.2 .and. idr.ge.2) THEN
               IF( ABS(Rlib(1)-Rlib(2)).gt.0.001 ) IDRs = 1
               IF( ABS(Alib(1)-Alib(2)).gt.0.001 ) IDRs = 1
            ENDIF

            IF (POT(i,j,24).NE.0.) THEN
C--------------Special Koning-type potential formulas
               IF (POT(i,j,24).EQ.1.) THEN
C-----------------Koning-type formulas
                  elf = El - EF
                  IF (i.EQ.1) CALL BCOGET(b,j)
                  IF (i.EQ.1 .AND. b(1,j,5).NE.0.) THEN
                     vc = b(1,j,1)
     &                    *encoul2*(b(1,j,2) - 2.*b(1,j,3)*elf + 3.*b(1,
     &                    j,4)*elf**2 + b(i,j,14)*b(i,j,13)
     &                    *EXP( - b(i,j,14)*elf))
                     vdcoul = b(i,j,5)*vc
                  ENDIF
                  nn = INT(POT(i,j,13))
C-----------------Retrieving average energy of the particle states Ep
                  EP = EF
                  IF ((i.EQ.2) .OR. (i.EQ.4))
     &                EP = DBLE(INT(100000*POT(i,j,20)))/100000
                  IF (EP.EQ.0.) EP = EF
                  elf = El - EP
                  IQ = 1
                  IF (i.EQ.4 .AND. b(4,j,12).GT.0.) IQ = NINT(b(4,j,12))
                  Vlib(i) = b(i,j,1)
     &                      *(b(i,j,15) - b(i,j,2)*elf + b(i,j,3)
     &                      *elf**2 - b(i,j,4)*elf**3 + b(i,j,13)
     &                      *EXP( - b(i,j,14)*elf)) + vdcoul + b(i,j,6)
     &                      *(elf**nn/(elf**nn + b(i,j,7)**nn))
     &                      + b(i,j,8)*EXP( - b(i,j,9)*elf**IQ)
     &                      *(elf**nn/(elf**nn + b(i,j,10)**nn))
     &                      + b(i,j,11)*EXP( - b(i,j,12)*elf)
               ENDIF
               IF (POT(i,j,24).EQ.2.) THEN
C-----------------Morillon-Romain formulas
                  elf = El - EF
                  IF (i.EQ.1) CALL BCOGET(b,j)
C-----------------Vhf(E) calculated from nonlocal approximation
C-----------------as suggested by Perey and Buck
                  alpha_pb = DBLE(INT(100000*b(i,j,1)))/100000
                  beta_pb = DBLE(INT(100000*b(i,j,2)))/100000
                  gamma_pb = DBLE(INT(100000*b(i,j,3)))/100000
                  EEE = DBLE(INT(1000000*El))/1000000
                  IQ = 1
                  IF (i.EQ.4 .AND. b(4,j,12).GT.0.) IQ = NINT(b(4,j,12))
                  vnonl = 0.
                  IF (i.EQ.1) THEN
                     vnonl = -VHF(EEE,alpha_pb,beta_pb,gamma_pb)
C--------------------Numerical derivative of the Vhf
                     IF (b(1,j,5).NE.0.) THEN
                        vnonlm = -VHF(EEE - 0.05,alpha_pb,beta_pb,
     &                           gamma_pb)
                        vnonlp = -VHF(EEE + 0.05,alpha_pb,beta_pb,
     &                           gamma_pb)
C-----------------------Coulomb correction for Hartree-Fock potential
                        vc = encoul2*(vnonlm - vnonlp)*10.D0
                        vdcoul = b(i,j,5)*vc
                     ENDIF
                  ENDIF
                  Vlib(i) = vnonl + vdcoul + b(i,j,6)
     &                      *(elf**nn/(elf**nn + b(i,j,7)**nn))
     &                      + b(i,j,8)*EXP( - b(i,j,9)*elf**IQ)
     &                      *(elf**nn/(elf**nn + b(i,j,10)**nn))
     &                      + b(i,j,11)*EXP( - b(i,j,12)*elf)
               ENDIF
C
C--------------Nonlocality consideration
C
C--------------Retrieving energy above which nonlocality in the volume absorptive
C--------------potential is considered (Ea)
C
               ea = DBLE(INT(100000*POT(i,j,21)))/100000
               IF (ea.EQ.0.) ea = 1000.1D0
               IF (i.EQ.2 .AND. ea.LT.1000. .AND. El.GT.(EF + ea))
     &             Vlib(i) = Vlib(i)
     &                       + alphav*(SQRT(El) + (EF + ea)**1.5/(2.*El)
     &                       - 1.5*SQRT(EF + ea))
            ELSEIF (POT(i,j,23).NE.0.) THEN
C--------------Special Varner-type potential formulas
               Vlib(i) = (POT(i,j,1) + POT(i,j,2)*ETA)
     &                   /(1. + EXP((POT(i,j,3)-El+POT(i,j,4)*encoul2)
     &                   /POT(i,j,5)))
               IF (POT(i,j,6).NE.0.) Vlib(i) = Vlib(i) + POT(i,j,6)
     &             *EXP((POT(i,j,7)*El - POT(i,j,8))/POT(i,j,6))
            ELSEIF (POT(i,j,22).EQ.0.) THEN
C--------------Standard potential formulas
               Vlib(i) = POT(i,j,1) + POT(i,j,7)*ETA + POT(i,j,8)
     &                   *ENCoul + POT(i,j,9)*ATAr + POT(i,j,10)
     &                   *ATAr**(1./3.) + POT(i,j,11)*ATAr**( - 2./3.)
     &                   + POT(i,j,12)*encoul2 +
     &                   (POT(i,j,2) + POT(i,j,13)*ETA + POT(i,j,14)
     &                   *ATAr)*El + POT(i,j,3)*El*El + POT(i,j,4)
     &                   *El*El*El + POT(i,j,6)*SQRT(El)
               IF (El.GT.0.) Vlib(i) = Vlib(i) + POT(i,j,17)
     &                                 *ENCoul/El**2 +
     &                                 (POT(i,j,5) + POT(i,j,15)
     &                                 *ETA + POT(i,j,16)*El)*LOG(El)
            ELSE
C--------------Special Smith-type potential formulas
               Vlib(i) = POT(i,j,1) + POT(i,j,2)*ETA + POT(i,j,6)
     &                   *EXP(POT(i,j,7)*El + POT(i,j,8)*El*El)
     &                   + POT(i,j,9)
     &                   *El*EXP(POT(i,j,10)*El**POT(i,j,11))
               IF (POT(i,j,5).NE.0.) Vlib(i) = Vlib(i) + POT(i,j,3)
     &             *COS(2.*pi*(ATAr - POT(i,j,4))/POT(i,j,5))
            ENDIF


C             Setting imaginary potentials to zero if negative !!
            IF(i.eq.2 .OR. i.eq.4) Vlib(i) = MAX(Vlib(i),0.0d0)
         ENDIF
      ENDDO
C
C-----To calculate dispersion relation contribution
C
152   IF (ABS(IDR).GE.2) THEN
C
C-------Exact calculation of the dispersive contribution
C
         EEE = DBLE(INT(1000000*El))/1000000
         i = 2
C--------Only one energy range
         j = 1
C--------Real volume contribution from Dispersive relation
         dwv = 0.D0
         IF (POT(2,1,24).NE.0) THEN
            AAV = DBLE(INT(100000*b(i,j,6)))/100000
            BV = DBLE(INT(100000*b(i,j,7)))/100000
            N = NINT(POT(i,j,13))


            NNV = N


            IF (N.EQ.0 .OR. MOD(N,2).EQ.1)
     &           STOP 'Zero or odd exponent in Wv(E) for dispersive OMP'

C-----------Retrieving average energy of the particle states Ep
            EP = DBLE(INT(100000*POT(i,j,20)))/100000
            IF (EP.EQ.0.) EP = EF
C-----------Nonlocality correction to the DOM integral
C-----------(only used if Ea is non-zero)
            ea = DBLE(INT(100000*POT(i,j,21)))/100000
            IF (ea.EQ.0.) ea = 1000.1D0

C-----------Analytical DOM integral
            dwv = DOM_INT_WV(EF,EP,AAV,BV,EEE,N,derdwv)
C-----------Coulomb correction for real volume potential
            derdwv = -b(1,1,5)*encoul2*derdwv
C-----------numerical DOM derivative (not needed for a time being)
C           DWVp = DOM_INT_Wv(Ef,Ep,AAv,Bv,EEE+0.1d0,n,dtmp)
C           DWVm = DOM_INT_Wv(Ef,Ep,AAv,Bv,EEE-0.1d0,n,dtmp)
C           DerDWV = -b(1,1,5)*encoul2*(DWVp-DWVm)*5.d0

C           if(idr.le.-2) then
C             numerical DOM integral (not needed for a time being)
C             WVE=WVf(AAv,Bv,Ep,Ef,EEE,n)
C             DWV=2*DOM_int(Delta_WV,WVf,Ef,Ef+5.*Bv,150000.d0,EEE,0.d0)
C           endif
C
            dwplus = 0.D0
            dwmin = 0.D0
            t12der = 0.D0
            IF (ea.LT.1000.) THEN
               dwplus = alphav*DOM_INT_T2(EF,ea,EEE)
               dtmp1 = WVF(AAV,BV,EP,EF,EF + ea,N)
               dwmin = dtmp1*DOM_INT_T1(EF,ea,EEE)
               dwv = dwv + dwplus + dwmin
C--------------Coulomb correction for nonlocal dispersive contribution
C--------------to real volume potential
               IF (b(1,1,5).NE.0.D0) THEN
                  IF (EEE.NE.0.05D0) THEN
                     t2p = DOM_INT_T2(EF,ea,EEE + 0.05D0)
                     t2m = DOM_INT_T2(EF,ea,EEE - 0.05D0)
                     t2der = alphav*(t2p - t2m)*10.D0
                     t1p = DOM_INT_T1(EF,ea,EEE + 0.05D0)
                     t1m = DOM_INT_T1(EF,ea,EEE - 0.05D0)
                     t1der = dtmp1*(t1p - t1m)*10.D0
                     t12der = -b(1,1,5)*encoul2*(t1der + t2der)
                  ELSE
                     t2p = DOM_INT_T2(EF,ea,EEE + 0.1D0)
                     t2m = DOM_INT_T2(EF,ea,EEE - 0.1D0)
                     t2der = alphav*(t2p - t2m)*5.D0
                     t1p = DOM_INT_T1(EF,ea,EEE + 0.1D0)
                     t1m = DOM_INT_T1(EF,ea,EEE - 0.1D0)
                     t1der = dtmp1*(t1p - t1m)*5.D0
                     t12der = -b(1,1,5)*encoul2*(t1der + t2der)
                  ENDIF
               ENDIF

            ENDIF
            vvcoul = derdwv + t12der
         ENDIF

154      i = 4
C--------Only one energy range
         j = 1
C--------Real surface contribution from Dispersive relation
         dws = 0.D0

         IF (POT(4,1,24).NE.0) THEN
            AS = DBLE(INT(100000*b(i,j,8)))/100000
            BS = DBLE(INT(100000*b(i,j,10)))/100000
            CS = DBLE(INT(100000*b(i,j,9)))/100000
            N = NINT(POT(i,j,13))


            NNS = N
            IF (N.EQ.0 .OR. MOD(N,2).EQ.1)
     &           STOP 'Zero or odd exponent in Wd(E) for dispersive OMP'

            IQ = 1
            IF (b(4,j,12).GT.0.) IQ = NINT(b(4,j,12))

C-----------Retrieving average energy of the particle states Ep
            EP = DBLE(INT(100000*POT(i,j,20)))/100000
            IF (EP.EQ.0.) EP = EF

            IF (IDR.GE.2) THEN
C--------------analytical DOM integral
               dws = DOM_INT_WS(EF,EP,AS,BS,CS,EEE,N,derdws)
C--------------Coulomb correction for real surface potential
               vscoul = -b(1,1,5)*encoul2*derdws
            ENDIF

            IF (IDR.LE. - 2) THEN
C--------------numerical DOM integral
               NNS = N
               WDE = WDF(AS,BS,CS,EP,EEE,N,IQ)
               dws = 2*DOM_INT(DELTA_WD,WDF,EF,EF + 30.D0,2000.D0,EEE,
     &               WDE)
C--------------Coulomb correction for real surface potential
               IF (b(1,1,5).NE.0.D0) THEN
                  WDE = WDF(AS,BS,CS,EP,EEE + 0.1D0,N,IQ)
                  dwsp = 2*DOM_INT(DELTA_WD,WDF,EF,EF + 30.D0,2000.D0,
     &                   EEE + 0.1D0,WDE)
                  WDE = WDF(AS,BS,CS,EP,EEE - 0.1D0,N,IQ)
                  dwsm = 2*DOM_INT(DELTA_WD,WDF,EF,EF + 30.D0,2000.D0,
     &                   EEE - 0.1D0,WDE)
C-----------------Numerical derivative
                  vscoul = -b(1,1,5)*encoul2*(dwsp - dwsm)*5.D0
               ENDIF
            ENDIF

         ENDIF

156      i = 6
C--------Only one energy range
         j = 1
C--------Real spin orbit contribution from Dispersive relation
         dwvso = 0.D0
C
         IF (POT(6,1,24).NE.0 .AND. ABS(IDR).EQ.3) THEN

            aavso = DBLE(INT(100000*b(i,j,6)))/100000
            bvso = DBLE(INT(100000*b(i,j,7)))/100000
            N = NINT(POT(i,j,13))
            IF (N.EQ.0 .OR. MOD(N,2).EQ.1) STOP
     &          'Zero or odd exponent in Wso(E) for dispersive OMP'

C----------analytical DOM integral
            dwvso = DOM_INT_WV(EF,EF,aavso,bvso,EEE,N,dtmp)

         ENDIF

C--------Adding real volume dispersive contribution to the real potential
C--------Geometry parameters are the same as for the volume potential(imag and real).
	   if (IDRs.gt.0) DWVNUM = dwv + vvcoul
         Vlib(1) = Vlib(1) + dwv + vvcoul
C--------Including real surface and Coulomb dispersive contribution
C--------Geometry parameters are the same as for the imaginary surface potential.
         Vlib(3) = dws + vscoul
         Alib(3) = Alib(4)
         Rlib(3) = Rlib(4)
C--------Adding real spin orbit dispersive contribution to the real spin orbit potential
C--------Geometry parameters are the same as for the imaginary spin orbit potential(imag and real)
         Vlib(5) = Vlib(5) + dwvso
      ENDIF
      END

      SUBROUTINE SETR(A,B,N)
C
C     ******************************************************************
C     set all elements of array b(n) to real number a.
C     ******************************************************************
C
C Dummy arguments
C
      REAL A
      INTEGER N
      REAL B(N)
C
C Local variables
C
      INTEGER k
      DO k = 1, N
         B(k) = A
      ENDDO
      END

      SUBROUTINE BCOGET(B,J)
C     Routine to compute b coefficients for Koning global potential
C
C     Modified by R.Capote to extend RIPL format
C
      INCLUDE "ripl2empire.h"
C
C Dummy arguments
C
      INTEGER J
      REAL B(6,NDIM1,15)
      CALL SETR(0.,B,90*NDIM1)

      V1 = 0.d0

      W1 = 0.d0


C-----Original Koning dependence
      B(1,J,1) = POT(1,J,1) + POT(1,J,2)*ATAr + POT(1,J,8)*ETA


C-----Soukhovitski dependence
      IF ((POT(1,J,20).NE.0.) .AND.
     &    (POT(1,J,14) + POT(1,J,15)*ATAr + POT(1,J,16)).NE.0.) B(1,J,1)
     &    = POT(1,J,1) + POT(1,J,2)*ATAr + POT(1,J,8)*ETA + POT(1,J,20)
     &      *ETA/(POT(1,J,14) + POT(1,J,15)*ATAr + POT(1,J,16))


      B(1,J,2) = POT(1,J,3) + POT(1,J,4)*ATAr
      B(1,J,3) = POT(1,J,5) + POT(1,J,6)*ATAr
      B(1,J,4) = POT(1,J,7)
      B(1,J,5) = POT(1,J,9)
      B(1,J,11) = POT(1,J,10) + POT(1,J,11)*ATAr
      B(1,J,12) = POT(1,J,12)
C-----b coefficients from 13 to 15 added for Soukhovitski potential
C-----V^DISP_R
      B(1,J,13) = POT(1,J,16)
C-----Lambda_R
      B(1,J,14) = POT(1,J,17)
C-----V^0_R + V^A_R*(A-232)
      B(1,J,15) = POT(1,J,14) + POT(1,J,15)*ATAr
C-----To preserve compatibility with RIPL-2 Koning database
C-----b(i,j,15) must be equal to 1. !!! for Koning OMP
      IF ((POT(1,J,14) + POT(1,J,15)*ATAr + POT(1,J,16)).EQ.0.)
     &    B(1,J,15) = 1.d0
C-----if(abs(b(1,j,15)).lt.1.e-8) b(1,j,15) = 1.
C-----Wv( Av )
      B(2,J,6) = POT(2,J,1) + POT(2,J,2)*ATAr
C-----Wv( Bv )
      B(2,J,7) = POT(2,J,3) + POT(2,J,4)*ATAr
C-----Wd
C
C-----added A dependence for As parameter (RCN, 09/2004), i.e.  pot(4,j,7)<>0
C-----Wd( As )
C-----b(4,j,8)  =  pot(4,j,1) + pot(4,j,8)*eta


c     added A dependence for As parameter (RCN, 09/2004), i.e.  pot(4,j,7)<>0
C     B(4,J,8) = POT(4,J,1) + POT(4,J,8)*ETA + POT(4,J,7)*ATAr


c     added A**(-1/3) dependence for As parameter (RCN, 11/2005), i.e.  pot(4,j,9)<>0


      B(4,j,8)  =  POT(4,j,1) + POT(4,j,8)*ETA + POT(4,j,7)*atar + 


     >                POT(4,j,9)*ATAr**(-1.d0/3.d0)


C-----Wd( Cs )
      IF (POT(4,J,3).NE.0.) THEN
         B(4,J,9) = POT(4,J,2) + POT(4,J,3)
     &              /(1. + EXP((ATAr-POT(4,J,4))/POT(4,J,5)))
      ELSE
         B(4,J,9) = POT(4,J,2)
      ENDIF
C-----Wd( Bs )
      B(4,J,10) = POT(4,J,6)
C-----Wd( q )
      B(4,J,12) = POT(4,J,12)
C-----Vso
      B(5,J,11) = POT(5,J,10) + POT(5,J,11)*ATAr
      B(5,J,12) = POT(5,J,12)
C-----Wso
      B(6,J,6) = POT(6,J,1)
      B(6,J,7) = POT(6,J,3)


      RETURN
      END

      REAL*8 FUNCTION VHF(Einp,Alpha_pb,Beta_pb,Gamma_pb)
C
C     According to Morillon B, Romain P, PRC70(2004)014601
C
C     Originally coded in c++ by Morillon B. and Romain P.
C
C     Coded in FORTRAN and tested by RCN, August 2004.
C
      INCLUDE "ripl2empire.h"
C
C Dummy arguments
C
      REAL*8 Alpha_pb, Beta_pb, Einp, Gamma_pb
C
C Local variables
C
      REAL*8 amu, coef1, coef2, etmp, miu_sur_hbar2, vtmp
      DOUBLE PRECISION DEXP
      INTEGER niter
      REAL SNGL
      REAL XKINE, xtmp
C-----getting amu
      xtmp=xkine(sngl(einp),amu)
      miu_sur_hbar2 = amu/HBArc**2
      coef1 = -0.5D0*Beta_pb**2*miu_sur_hbar2
      coef2 = 4.0D0*(Gamma_pb*miu_sur_hbar2)**2
      niter = 0.
      VHF = -45.D0
  100 niter = niter + 1
      vtmp = VHF
      etmp = Einp - vtmp
      VHF = Alpha_pb*DEXP(coef1*etmp + coef2*etmp**2)
      IF (ABS(VHF - vtmp).GT.0.0001 .AND. niter.LT.10000) GOTO 100
      END

      REAL FUNCTION XKINE(Ei,Amu)
C
C***********************************************************************
C     From lab to CM (the input quantity is el = Elab)
C***********************************************************************
C     RCN 08/2004, xkine calculated by relativistic kinematics when needed
      INCLUDE "ripl2empire.h"
C
C Dummy arguments
C
      REAL*8 Amu
      REAL Ei
C
C Local variables
C
      DOUBLE PRECISION DSQRT
      REAL etoti, etott, p2
      INTEGER mtot
      mtot = (TARmas + PROjmas)
      IF (IREl.EQ.0) THEN
C--------Classical    kinematics (energy independent amu and xkine)
         Amu = PROjmas*TARmas/mtot*AMU0c2
         XKINE = TARmas/mtot
      ELSE
C--------Relativistic kinematics
         p2 = (Ei*(Ei + 2.D0*AMU0c2*PROjmas))
     &        /((1.D0 + PROjmas/TARmas)**2 + 2.D0*Ei/(AMU0c2*TARmas))
         etoti = DSQRT((AMU0c2*PROjmas)**2 + p2)
         etott = DSQRT((AMU0c2*TARmas)**2 + p2)
         Amu = etoti*etott/(etoti + etott)
         XKINE = etott/(etoti + etott)
      ENDIF
      END

      REAL*8 FUNCTION DOM_INT_WV(Ef,Ep,Av,Bv,E,N,Derivintwv)
C
C     AUTHOR: Dr. Roberto Capote Noy
C
C     e-mail: r.capotenoy@iaea.org; rcapotenoy@yahoo.com
C
C     DISPERSIVE OPTICAL MODEL POTENTIAL PACKAGE
C
C     Analytical and numerical dispersive integrals are calculated
C     please cite as:
C     Quesada JM et al,, Phys. Rev. C67(2003) 067601
C     Capote R et al, J. Phys. G27(2001) B15
C
C
C      Analytical dispersive integral and its derivative for
C      Wv(E)=Av*(E-Ep)**n/( (E-Ep)**n + Bv**n )  for E>Ep
C      Wv(E)=Wv(2*Ef-E)                          for E<2Ef-Ep
C      Wv(E)=0                                     OTHERWISE
C
      IMPLICIT NONE
C
C Dummy arguments
C
      REAL*8 Av, Bv, Derivintwv, E, Ef, Ep
      INTEGER N
C
C Local variables
C
      DOUBLE PRECISION DABS, DBLE
      REAL*8 deremin, dereplus, e0, emin, eplus, ex, pi, rds, resemin,
     &       reseplus, rs
      COMPLEX*8 ds, fs
      COMPLEX*16 i, pj, zj, ztmp
      INTEGER j
      REAL REAL
      DATA i/(0.D0,1.D0)/
      pi = 4.D0*ATAN(1.D0)
      e0 = Ep - Ef
      ex = E - Ef
      eplus = ex + e0
      emin = ex - e0
      DOM_INT_WV = 0.D0
      Derivintwv = 0.D0
      resemin = emin**N/(emin**N + Bv**N)
      deremin = emin**(N - 1)
     &          *(emin**N + Bv**N*(1.D0 + N*LOG(DABS(emin))))
     &          /(emin**N + Bv**N)**2
      reseplus = -eplus**N/(eplus**N + Bv**N)
      dereplus = -eplus**(N - 1)
     &           *(eplus**N + Bv**N*(1.D0 + N*LOG(eplus)))
     &           /(eplus**N + Bv**N)**2
C----------------------------------
C-----Complex arithmetic follows
      fs = (0.D0,0.D0)
      ds = (0.D0,0.D0)
      DO j = 1, N
         ztmp = i*(2*j - 1)/DBLE(N)*pi
         pj = Bv*EXP(ztmp)
         zj = pj*(2*pj + eplus - emin)*ex
         zj = zj/((pj + e0)*(pj + eplus)*(pj - emin))
         fs = fs + zj*LOG( - pj)
         ds = ds + 2*pj*(ex*ex + (pj + e0)**2)*LOG( - pj)
     &        /((pj + eplus)**2*(pj - emin)**2)
      ENDDO
      IF (ABS(IMAG(fs)).GT.1.E-4) STOP '(F) Too big imag part in Wv'
      rs = REAL(fs)
      IF (ABS(IMAG(ds)).GT.1.E-4) STOP '(D) Too big imag part in Wv'
      rds = REAL(ds)
C----------------------------------
      DOM_INT_WV = -Av/pi*(rs/N + reseplus*LOG(eplus)
     &             + resemin*LOG(DABS(emin)))
      Derivintwv = -Av/pi*(rds/N + dereplus + deremin)
      END

      REAL*8 FUNCTION DOM_INT_WS(Ef,Ep,As,Bs,Cs,E,M,Derivintws)
C
C      Analytical dispersive integral and its derivative for
C      Ws(E)=As*(E-Ep)**m/( (E-Ep)**m + Bs**m ) * exp(-Cs*(E-Ep)) for E>Ep
C      Ws(E)=Ws(2*Ef-E)                                           for E<2Ef-Ep
C      Ws(E)=0                                                    OTHERWISE
C
      IMPLICIT NONE
C
C Dummy arguments
C
      REAL*8 As, Bs, Cs, Derivintws, E, Ef, Ep
      INTEGER M
C
C Local variables
C
      DOUBLE PRECISION DBLE
      REAL*8 deremin, dereplus, e0, emin, eplus, ex, pi, rds, resemin,
     &       reseplus, rs
      COMPLEX*8 ds, fs
      REAL*8 EIN
      COMPLEX*16 i, pj, zj, ztmp
      INTEGER j
      REAL REAL
      COMPLEX*16 ZFI
      DATA i/(0.D0,1.D0)/
      pi = 4.D0*ATAN(1.D0)
      e0 = Ep - Ef
      ex = E - Ef
      eplus = ex + e0
      emin = ex - e0
      DOM_INT_WS = 0.D0
      Derivintws = 0.D0
      resemin = emin**M/(emin**M + Bs**M)
      deremin = -emin**(M - 1)
     &          *(emin**M + Bs**M + ( - Cs*emin**(M+1) + Bs**M*
     &          (-Cs*emin+M))*EXP( - Cs*emin)*EIN(Cs*emin))
     &          /(emin**M + Bs**M)**2
      reseplus = -eplus**M/(eplus**M + Bs**M)
      dereplus = eplus**(M - 1)
     &           *(eplus**M + Bs**M + (Cs*eplus**(M+1) + Bs**M*
     &           (Cs*eplus+M))*EXP(Cs*eplus)*EIN( - Cs*eplus))
     &           /(eplus**M + Bs**M)**2
C----------------------------------
C     Complex arithmetic follows
      fs = (0.D0,0.D0)
      ds = (0.D0,0.D0)
      DO j = 1, M
         ztmp = i*(2*j - 1)/DBLE(M)*pi
         pj = Bs*EXP(ztmp)
         zj = pj*(2*pj + eplus - emin)*ex
         zj = zj/(pj + e0)/(pj + eplus)/(pj - emin)
         fs = fs + zj*ZFI( - pj*Cs)
         ds = ds + 2*pj*(ex*ex + (pj + e0)**2)*ZFI( - pj*Cs)
     &        /((pj + eplus)**2*(pj - emin)**2)
      ENDDO
      IF (ABS(IMAG(fs)).GT.1.E-4) STOP '(F) Too big imag part in Ws'
      rs = REAL(fs)
      IF (ABS(IMAG(ds)).GT.1.E-4) STOP '(D) Too big imag part in Ws'
      rds = REAL(ds)
C----------------------------------
      DOM_INT_WS = As/pi*(rs/M - reseplus*EXP(Cs*eplus)*EIN( - Cs*eplus)
     &             - resemin*EXP( - Cs*emin)*EIN(Cs*emin))
      Derivintws = As/pi*(rds/M + dereplus + deremin)
      END


      REAL*8 FUNCTION WV(A,B,Ep,Ef,E,N)
      IMPLICIT NONE
C
C Dummy arguments
C
      REAL*8 A, B, E, Ef, Ep
      INTEGER N
C
C Local variables
C
      REAL*8 ee
      WV = 0.D0
      IF (E.LE.Ef) E = 2.D0*Ef - E
      IF (E.LT.Ep) RETURN
      ee = (E - Ep)**N
      WV = A*ee/(ee + B**N)
      END


      REAL*8 FUNCTION WDD(A,B,C,Ep,Ef,E,M)
      IMPLICIT NONE
C
C Dummy arguments
C
      REAL*8 A, B, C, E, Ef, Ep
      INTEGER M
C
C Local variables
C
      REAL*8 arg, ee
      WDD = 0.D0
      IF (E.LE.Ef) E = 2.D0*Ef - E
      IF (E.LT.Ep) RETURN
      arg = C*(E - Ep)
      IF (arg.GT.15) RETURN
      ee = (E - Ep)**M
      WDD = A*ee/(ee + B**M)*EXP( - arg)
      END

      REAL*8 FUNCTION DOM_INT_T1(Ef,Ea,E)
C
C     Integral over E' corresponding to nonlocal additions T1(E'<<0)
C
      IMPLICIT NONE
C
C Dummy arguments
C
      REAL*8 E, Ea, Ef
C
C Local variables
C
      REAL*8 ea2, eax, ex, pi, t11, t12, t13
      pi = 4.D0*ATAN(1.D0)
      ex = E - Ef
      ea2 = Ea**2
      eax = ex + Ea
      t11 = 0.5D0*LOG(Ea)/ex
      t12 = ((2*Ea + ex)*LOG(Ea) + 0.5D0*pi*ex)/(2.*(eax**2 + ea2))
      t13 = -eax**2*LOG(eax)/(ex*(eax**2 + ea2))
      DOM_INT_T1 = ex/pi*(t11 + t12 + t13)
      END

      REAL*8 FUNCTION DOM_INT_T2(Ef,Ea,E)
C
C     Integral over E' corresponding to nonlocal additions T2(E'>>0)
C
      IMPLICIT REAL*8(A - H,O - Z)
C
C Dummy arguments
C
      REAL*8 E, Ea, Ef
C
C Local variables
C
      DOUBLE PRECISION DABS, DLOG, DSQRT
      REAL*8 el, pi, r1, r2, r3, r4
      pi = 4.D0*ATAN(1.D0)
      el = Ef + Ea
      r1 = 1.5*DSQRT(el)*DLOG(ABS((el-E)/Ea))
      IF (ABS(E).LT.1.D-6) THEN
         r2 = 0.5*el**1.5D0*(1.D0/el - DLOG(ABS(el/Ea))/Ef)
      ELSE
         r2 = 0.5*el**1.5D0/(E*Ef)
     &        *(Ef*DLOG(DABS(el/(el-E))) - E*DLOG(DABS(el/Ea)))
      ENDIF
      r3 = 2*DSQRT(DABS(Ef))*(0.5D0*pi - ATAN(DSQRT(el/DABS(Ef))))
      IF (E.GE.0.D0) THEN
         r4 = DSQRT(E)
     &        *DLOG(DABS((DSQRT(el)+DSQRT(E))/(DSQRT(el)-DSQRT(E))))
      ELSE
         r4 = -2.D0*DSQRT(DABS(E))*(0.5D0*pi - ATAN(DSQRT(DABS(el/E))))
      ENDIF
      DOM_INT_T2 = 1.D0/pi*(r1 + r2 + r3 + r4)
      END

      COMPLEX*16 FUNCTION ZFI(Za)
C
C-----FUNCTION TO EVALUATE exp(Z)*E1(Z)
C
C Complex exponential integral function multiplied by exponential
C
C AUTHOR: J. Raynal
C
      IMPLICIT NONE
C
C Dummy arguments
C
      COMPLEX*16 Za
C
C Local variables
C
      REAL*8 aj
      DOUBLE PRECISION DABS, DSQRT
      INTEGER i, m
      COMPLEX*16 y
      ZFI = 0.D0
      IF (Za.EQ.0.) RETURN
      IF (DABS(DREAL(Za) + 18.5D0).LT.25.D0) THEN
         IF (DSQRT(625.D0 - (DREAL(Za)+18.5D0)**2)
     &       /1.665D0.GE.DABS(DIMAG(Za))) THEN
            ZFI = -.57721566490153D0 - CDLOG(Za)
            y = 1.D0
            DO m = 1, 2000
               aj = m
               y = -y*Za/aj
               IF (CDABS(y).LT.1.D-15*CDABS(ZFI)) GOTO 20
               ZFI = ZFI - y/aj
            ENDDO
   20       ZFI = CDEXP(Za)*ZFI
            RETURN
         ENDIF
      ENDIF
      DO i = 1, 20
         aj = 21 - i
         ZFI = aj/(Za + ZFI)
         ZFI = aj/(1.D0 + ZFI)
      ENDDO
      ZFI = 1.D0/(ZFI + Za)
      END

      REAL*8 FUNCTION EIN(X)
C
C-----FUNCTION TO EVALUATE Ei(X)
C
      IMPLICIT NONE
C
C Dummy arguments
C
      REAL*8 X
C
C Local variables
C
      REAL*8 fac, h
      REAL FLOAT
      INTEGER n
      EIN = 0.57721566490153D0 + LOG(ABS(X))
      fac = 1.0
      DO n = 1, 100
         h = FLOAT(n)
         fac = fac*h
         EIN = EIN + X**n/(h*fac)
      ENDDO
      END


      REAL*8 FUNCTION DELTA_WV(WVF,Y)
C
C COMMON variables
C
      REAL*8 A, B, E, EF, EP, EA, WDE, WVE
      INTEGER N
      COMMON /DENERGY/ E, EF, EP, EA
      COMMON /PDATAV/ A, B, N
      COMMON /WENERG/ WDE, WVE
C
C Dummy arguments
C
      REAL*8 Y
      REAL*8 WVF
      DELTA_WV = (WVF(A,B,EP,Y,N) - WVE)/((Y - EF)**2 - (E - EF)**2)
      END


      REAL*8 FUNCTION WVF(A,B,Ep,Ef,Er,N)
C
C Dummy arguments
C
      REAL*8 A, B, Er, Ef, Ep
      INTEGER N
C
C Local variables
C
      REAL ee, E
      WVF = 0.D0
      E = Er
      IF (E.LE.Ef) E = 2.D0*Ef - E
      IF (E.LE.Ep) RETURN
      ee = (E - Ep)**N
      WVF = A*ee/(ee + B**N)
      END

      REAL*8 FUNCTION DELTA_WD(WDF,Y)
C
C COMMON variables
C
      REAL*8 A, B, C, E, EF, EP, WDE, WVE, EA
      INTEGER IQ, M
      COMMON /DENERGY/ E, EF, EP, EA
      COMMON /PDATAS/ A, B, C, M, IQ
      COMMON /WENERG/ WDE, WVE
C
C Dummy arguments
C
      REAL*8 Y
      REAL*8 WDF
      DELTA_WD = (WDF(A,B,C,EP,Y,M,IQ) - WDE)
     &           /((Y - EF)**2 - (E - EF)**2)
      END


      REAL*8 FUNCTION WDF(A,B,C,Ep,E,M,Iq)
C
C Dummy arguments
C
      REAL*8 A, B, C, E, Ep
      INTEGER Iq, M
C
C Local variables
C
      REAL*8 arg, ee
      WDF = 0.D0
      IF (E.LT.Ep) RETURN
      arg = C*(E - Ep)**Iq
      IF (arg.GT.15) RETURN
      ee = (E - Ep)**M
      WDF = A*ee/(ee + B**M)*EXP( - arg)
      END


      REAL*8 FUNCTION DOM_INT(DELTAF,F,Ef,Eint,Ecut,E,We_cte)
C
C     DOM integral (20 points Gauss-Legendre)
C
C     Divided in two intervals for higher accuracy
C     The first interval corresponds to peak of the integrand
C
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
C
C Dummy arguments
C
      DOUBLE PRECISION E, Ecut, Ef, Eint, We_cte
      DOUBLE PRECISION F
      DOUBLE PRECISION DELTAF
C
C Local variables
C
      DOUBLE PRECISION absc1, absc2, centr1, centr2, hlgth1, hlgth2,
     &                 resg1, resg2, wg(10), www, xg(10), xxx
      REAL corr
      DOUBLE PRECISION DLOG
      INTEGER j
      EXTERNAL F
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
C-----COMPUTE THE 20-POINT GAUSS-KRONROD APPROXIMATION
C-----TO THE INTEGRAL in TWO INTERVALS (Ef - Eint, Eint - Ecut)
C
      resg1 = 0.0D+00
      resg2 = 0.0D+00
      DO j = 1, 10
         xxx = xg(j)
         www = wg(j)
         absc1 = hlgth1*xxx
         resg1 = resg1 +
     &           www*(DELTAF(F,centr1 - absc1) + DELTAF(F,centr1 +
     &           absc1))
         absc2 = hlgth2*xxx
         resg2 = resg2 +
     &           www*(DELTAF(F,centr2 - absc2) + DELTAF(F,centr2 +
     &           absc2))
      ENDDO
      corr = 0.5D0*We_cte/(E - Ef)*DLOG((Ecut - (E-Ef))/(Ecut + (E-Ef)))
      DOM_INT = (resg1*hlgth1 + resg2*hlgth2 + corr)*(E - Ef)
     &          /(ACOS( - 1.D0))
      END
