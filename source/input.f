C*==input.spg  processed by SPAG 6.20Rc at 12:14 on  7 Jul 2004
Ccc   * $Author: Capote $
Ccc   * $Date: 2005-01-24 13:21:33 $
Ccc   * $Id: input.f,v 1.58 2005-01-24 13:21:33 Capote Exp $
      SUBROUTINE INPUT
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:iou*
Ccc   *                         I N P U T                                *
Ccc   *                                                                  *
Ccc   *     Sets default values of input parameters, reads mandatory     *
Ccc   *     input and calls READIN for optional input reading.           *
Ccc   *                                                                  *
Ccc   * input:none                                                       *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * output:none                                                      *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * calls: BNDG                                                      *
Ccc   *            WHERE                                                 *
Ccc   *        CLEAR                                                     *
Ccc   *        LEVREAD                                                   *
Ccc   *            BCDNUM                                                *
Ccc   *        PTLEVSET                                                  *
Ccc   *            PTLEVRE                                               *
Ccc   *        READIN                                                    *
Ccc   *            WHERE                                                 *
Ccc   *        READLDP                                                   *
Ccc   *            FSHELL                                                *
Ccc   *            WHERE                                                 *
Ccc   *        READNIX                                                   *
Ccc   *            SHELLC                                                *
Ccc   *                LYMASM                                            *
Ccc   *                    XI                                            *
Ccc   *                    XIMOD                                         *
Ccc   *            WHERE                                                 *
Ccc   *        ROCOL                                                     *
Ccc   *            ALIT                                                  *
Ccc   *            BARFIT                                                *
Ccc   *                LPOLY                                             *
Ccc   *            MOMFIT                                                *
Ccc   *                LPOLY                                             *
Ccc   *            RODEF                                                 *
Ccc   *                DAMPV                                             *
Ccc   *                VIBR                                              *
Ccc   *            SHCFADE                                               *
Ccc   *            SIGMAK                                                *
Ccc   *        ROEMP                                                     *
Ccc   *            BNDG (see above)                                      *
Ccc   *            DAMIRO                                                *
Ccc   *                FSHELL                                            *
Ccc   *                MOMFIT (see above)                                *
Ccc   *                ROBCS                                             *
Ccc   *                    DAMPKS                                        *
Ccc   *                    VIBR                                          *
Ccc   *                RODEF (see above)                                 *
Ccc   *                SIGMAK                                            *
Ccc   *            FSHELL                                                *
Ccc   *            PIPE                                                  *
Ccc   *            PRERO                                                 *
Ccc   *                BARFIT (see above)                                *
Ccc   *                SHCFADE                                           *
Ccc   *                SIGMAK                                            *
Ccc   *        ROGC                                                      *
Ccc   *            BARFIT (see above)                                    *
Ccc   *            PIPE                                                  *
Ccc   *            RHOU                                                  *
Ccc   *            RJJ                                                   *
Ccc   *            SHCFADE                                               *
Ccc   *            SIGMAK                                                *
Ccc   *        SMAT                                                      *
Ccc   *        TLEVAL                                                    *
Ccc   *            TRANSINP                                              *
Ccc   *                                                                  *
Ccc   * author: M.Herman                                                 *
Ccc   * date:      Jun.1994                                              *
Ccc   *      revision by: M.Herman                   November 2000       *
Ccc   *      revision by: R.Capote                   January  2001       *
Ccc   *      revision by: R.Capote                   January  2005       *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C
C COMMON variables
C
      INTEGER NCHr
      CHARACTER*10 PROjec, RESidue(NDNUC), TARget
      CHARACTER*1 cnejec
      COMMON /EXFOR / TARget, PROjec, RESidue
      COMMON /IEXFOR/ NCHr
      LOGICAL gexist
C-----Plujko_new
      INTEGER keyinput, kzz1, kaa1, keyload, keyalpa, kzz, kaa
      COMMON /MLOCOM1/ keyinput, kzz1, kaa1
      COMMON /MLOCOM2/ keyload, keyalpa, kzz, kaa
C-----Plujko_new(End)
C
C Local variables
C
      DOUBLE PRECISION aclu, ares, atmp, da, deln(150), delp, delz(98),
     &                 e2p, e3m, emaxr, qmin, qtmp, zclu, zres, ztmp
      CHARACTER*3 ca1
      DOUBLE PRECISION DATAN, DMAX1
      CHARACTER*2 deut, trit, gamma
      REAL FLOAT, SNGL
      INTEGER i, ia, iac, iae, iccerr, iend, ierr, ietl, iia, iloc, in,
     &        ip, irec, iz, izares, izatmp, j, lpar, na, nejc, nema,
     &        nemn, nemp, netl, nnuc, nnur
      INTEGER IFINDCOLL
      INTEGER INDEX, INT
      LOGICAL nonzero
      CHARACTER*2 SMAT
      CHARACTER*3 atar
      CHARACTER*1 proj
      CHARACTER*132 x4string
      INTEGER*4 iwin
      INTEGER*4 PIPE
      DATA delz/0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 2.46, 0.,
     &     2.09, 0., 1.62, 0., 1.62, 0., 1.83, 0., 1.73, 0., 1.35, 0.,
     &     1.54, 0., 1.20, 0., 1.06, 0., 1.36, 0., 1.43, 0., 1.17, 0.,
     &     1.24, 0., 1.20, 0., 1.28, 0., 1.28, 0., 1.35, 0., 1.36, 0.,
     &     1.19, 0., 1.14, 0., 1.12, 0., 1.58, 0., 1.17, 0., 1.18, 0.,
     &     1.22, 0., 0.97, 0., 0.92, 0., 0.62, 0., 0.68, 0., 0.64, 0.,
     &     0.72, 0., 0.75, 0., 0.71, 0., 0.87, 0., 0.83, 0., 0.89, 0.,
     &     0.79, 0., 0.89, 0., 0.78, 0., 0.69, 0., 0.61, 0., 0.72, 0.,
     &     0.77/
      DATA deln/0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 2.67, 0.,
     &     1.80, 0., 1.67, 0., 1.86, 0., 2.04, 0., 1.64, 0., 1.44, 0.,
     &     1.54, 0., 1.30, 0., 1.27, 0., 1.29, 0., 1.41, 0., 1.50, 0.,
     &     1.50, 0., 1.43, 0., 1.88, 0., 1.47, 0., 1.57, 0., 1.46, 0.,
     &     0.93, 0., 0.72, 0., 1.12, 0., 1.29, 0., 0.94, 0., 1.24, 0.,
     &     1.25, 0., 1.14, 0., 1.32, 0., 1.15, 0., 1.24, 0., 1.43, 0.,
     &     1.09, 0., 1.20, 0., 1.04, 0., 0.70, 0., 0.85, 0., 0.76, 0.,
     &     0.92, 0., 0.99, 0., 1.10, 0., 0.92, 0., 0.73, 0., 0.70, 0.,
     &     0.87, 0., 0.61, 0., 0.69, 0., 0.55, 0., 0.40, 0., 0.73, 0.,
     &     0.58, 0., 0.86, 0., 1.13, 0., 0.84, 0., 0.79, 0., 0.82, 0.,
     &     0.71, 0., 0.41, 0., 0.38, 0., 0.67, 0., 0.61, 0., 0.78, 0.,
     &     0.67, 0., 0.67, 0., 0.79, 0., 0.60, 0., 0.57, 0., 0.49, 0.,
     &     0.43, 0., 0.50, 0., 0.39/
C     Capote 2001, redefined below and using globally
C     DATA xnexc, amumev/8.071323, 931.1494/
      DATA deut, trit, gamma/'d ', 't ', 'g '/
      ARGred = -1.
C-----maximum argument of EXP function supported by the computer (for real*8)
      EXPmax = 700.
C-----maximum exponent of 10 supported by the computer (for real*8)
      EXPdec = 300.
      CALL CLEAR
      DO nnuc = 1, NDNUC
         EMAx(nnuc) = 0.0
      ENDDO
      iccerr = 0
C
C     Capote 2001
C     Defining global physical and mathematical constants
C     They are passed through CONSTANT common block
C
C     W2=0.04784468   (OLD SCAT)
C     W2=0.04784369   (NEA-DATA-BANK PRESCRIPTION )
C     W2=0.047837/10  (EMPIRE) converting already to mb
C     W2=0.04784467   (ECIS)
C
C     Changed to ECIS-OLD SCAT PRESCRIPTION
C     CORRESPOND TO THE FOLLOWING CONSTANT VALUES
C     AMUmev=931.5017646d0
C     hhbarc = 197.328604d0
      XNExc = 8.071323D0
C-----From SCAT2000
      ampipm = 1.395688D+02
      ampi0 = 1.349645D+02
C-----AMUmev = 9.3149386D+02
C-----according to the ENDF-6 manual (April 2001)
      AMUmev = 9.31494013D+02
      AMPi = (2.D0*ampipm + ampi0)/3.D0
      ELE2 = 1.4399652D+00
      HHBarc = 1.97327053D+02
C
C     CHECK mass values
C
      AMUneu = 1.008665d0
      AMUpro = 1.007825d0
C
C-----already converted to mb
C     w2 = 2.d0*931.49386/(197.327053)**2 = 0,047845019
      W2 = 2.D0*AMUmev/HHBarc**2/10.D0
      CETa = ELE2*DSQRT(AMUmev/2.D0)/HHBarc
      CSO = (HHBarc/AMPi)**2
      PI = 4.D0*DATAN(1.D0)
C
C
      IF(EIN.EQ.0.0D0)THEN
C
C--------default input parameters (skipped in non-first-energy calculation)
C
         FIRst_ein = .TRUE.
C--------select Meyers-Swiatecki shell corrections
         SHNix = 0.0
C--------set angles for MSD calculations
         da = 180.0/(NDANG - 1)
         DO na = 1, NDANG
            ANGles(na) = (na - 1)*da
         ENDDO
         DO na = 1, NDANG
            CANgler(na) = COS(ANGles(NDANG - na + 1)*PI/180.)
            SANgler(na) = SQRT(1.0 - CANgler(na)**2)
         ENDDO
C--------neutralize tuning factors
         DO nnuc = 0, NDNUC
            DO nejc = 0, NDEJC
               TUNe(nejc, nnuc) = 1.0
            ENDDO
         ENDDO
C--------set level density parameters
         DO nnuc = 1, NDNUC
            IZA(nnuc) = 0
            ROPaa(nnuc) = -2.0
            ROPar(1, nnuc) = 0.
            ROPar(2, nnuc) = 0.
            ROPar(4, nnuc) = 0.
            ROPar(5, nnuc) = 0.
            ATIlnor(nnuc) = 0.
            GTIlnor(nnuc) = 1.
            LVP(1, nnuc) = 0
         ENDDO
         IZA(0) = 0
         LVP(1, 0) = 0
         NNUcd = 0
         NEJcm = 0
         DEFpar = 1.
         DEFga = 0.
         DEFgw = 10.
         DEFgp = 40.
         ADIv = 0.0
         NEX(1) = 50
         FITlev = 0.0
         GCAsc = -1.0
C--------fission barrier multiplier, viscosity, and spin fade-out
         QFIs = 1.0
         BETav = 4.0
         SHRj = 24.0
         SHRd = 2.5
C--------fusion parameters
         CSRead = -2.0
         SIG = 0.0
         TRUnc = 2.0
         EXPush = 0.
         CRL = 0.0
         DFUs = 1.
         FUSred = 1.
         LEVtarg = 1
C
C--------Capote, additional input options
C
         DIRect = 0
         KTRompcc = 0
         CCCalc = .FALSE.
         IOMwritecc = 0
         MODelecis = 0
C        Relativistic kinematics
         RELkin = .FALSE.
C        Maximum energy to assume all levels are collective for DWBA calculations
C                Default value 0. i.e. none but those selected automatically
         ECUtColl = 0.
         JCUtColl = 2.
C
C        IOPSYS = 0 LINUX
C        IOPSYS = 1 WINDOWS
         IOPsys = 0
C--------Mode of EXFOR retrieval
C        IX4ret = 0 no EXFOR retrieval
C        IX4ret = 1 local MySQL server (to become 2.19 default)
C        IX4ret = 2 remote SYBASE server
C        IX4ret = 3 local EXFOR files (as in 2.18 and before)
         IX4ret = 1
C--------CCFUF parameters
         DV = 10.
         FCC = 1.
         NSCc = 4
         NACc = 0
         BETcc(1) = 0.0
         BETcc(2) = 0.0
         BETcc(3) = 0.0
         BETcc(4) = 0.0
         FLAm(1) = 2.
         FLAm(2) = 3.
         FLAm(3) = -2.
         FLAm(4) = -3.
C--------set accelleration option
         LTUrbo = 1
         TURbo = FLOAT(LTUrbo)
C--------temperature fade-out parameters of fission barrier
         TEMp0 = 1.65
         SHRt = 1.066
C--------IOUT controls output amount on FILE6
         IOUt = 1
C--------IOUt=0 no output except warnings
C--------IOUt=1 input data and essential results (all cross sections)
C--------IOUt=2 as IOUt=1 plus fusion spin distribution, yrast state
C--------       population, gamma-transition parameters, fusion barrier,
C--------       inclusive spectra
C--------IOUt=3 as IOUt=2 + gamma and particle spectra + disc. levels decay
C--------IOUt=4 as IOUt=2 + residual nuclei continuum population
C--------       (up to spin 12)
C--------IOUt=5 as IOUt=2 + transmission coefficients (up to l=12)
C--------IOUt=6 as IOUt=2 + level densities (up to spin 12)
C
C--------default input parameters for HMS
C
         LHMs = 0 !controls HMS (must be different from 0 for HMS to be used)
         NHMs = 100000   !number of events for Monte Carlo
         CHMs = 1.0      ! mult. default damp rate (for eval work to scale preq)
C
C--------default input parameters for MSD
C
         MSD = 0
C        ICOmpff must be off for DOM potential
         ICOmpff = 0  !compressional form factor off
C        ICOmpff = 1  !compressional form factor on
C
C--------default input parameters for Heidelberg MSC
C
         MSC = 0
C--------STMro selects p-h densities: 0 for closed form, 1 for microscopic
         STMro = 0.0
C--------set single particle level density parameter default in MSC as A/13.
         GDIv = 13.0
C--------NOUT controls output amount in MSC (valid range 0-4, higher the value
C--------more printout)
         NOUt = 0
C--------XNI initial exciton number (0 internal determination)
         XNI = 0.
         TORy = 4.
         EX1 = 0.0
         EX2 = 0.0
C--------GST controls gamma emission in MSC (0 no gamma, 1 with gamma)
         GST = 0.0
C--------D1FRA ratio of the spreading to total width of the GDR
         D1Fra = 0.8
C--------GDR parameters
         GDRdyn = 0.
         EGDr1 = 0.
         GGDr1 = 0.
         CSGdr1 = 0.
         EGDr2 = 0.
         GGDr2 = 0.
         CSGdr2 = 0.
         DIToro = 0.0026
         GDResh = 0.
         GDRwa1 = 0.
         GDRwa2 = 0.
         GDRspl = 0.
         EWSr1 = 1.0
         EWSr2 = 1.0
         GDRweis = 1.
C--------set options for DEGAS (exciton preequilibrium)
         DEGa = 0.0
         GDIvp = 13.0
C--------set options for PCROSS (exciton preequilibrium + cluster emission)
         PEQc = 0.0
         MFPp = 1.3
C--------set ENDF flag to 0 (no ENDF file for formatting)
         ENDf = 0.0
C--------HRTW control (0 no HRTW, 1 HRTW up to 5 MeV incident)
         LHRtw = 1
C
C--------default input parameters    *** done ***
C
C--------ejectile neutron
         AEJc(1) = 1.
         ZEJc(1) = 0.
         XNEjc(1) = AEJc(1) - ZEJc(1)
         IZAejc(1) = INT(1000.*ZEJc(1) + AEJc(1))
         iz = INT(ZEJc(1))
         SYMbe(1) = SMAT(iz)
         SEJc(1) = 0.5
C--------ejectile proton
         AEJc(2) = 1.
         ZEJc(2) = 1.
         XNEjc(2) = AEJc(2) - ZEJc(2)
         IZAejc(2) = INT(1000.*ZEJc(2) + AEJc(2))
         iz = INT(ZEJc(2))
         SYMbe(2) = SMAT(iz)
         SEJc(2) = 0.5
C--------ejectile alpha
         AEJc(3) = 4.
         ZEJc(3) = 2.
         XNEjc(3) = AEJc(3) - ZEJc(3)
         IZAejc(3) = INT(1000.*ZEJc(3) + AEJc(3))
         iz = INT(ZEJc(3))
         SYMbe(3) = SMAT(iz)
         SEJc(3) = 0.0
C--------Plujko_new
C        default values for  Key_shape and Key_GDRGFL
         KEY_shape = 0
         KEY_gdrgfl = 1
         keyinput=0
         kzz1=0
         kaa1=0
         keyload=0
         keyalpa=0
         kzz=0
         kaa=0
         IGE1 = 1
         Lqdfac = 1.0d0
         IGM1 = 0
         IGE2 = 0
C--------Plujko_new (END)
C
C--------read entire input for the first energy calculations
C--------mandatory part of the input
C--------incident energy (in LAB)
         READ(5, *)EIN
C--------target
         READ(5, *)A(0), Z(0)
         CALL PTLEVSET(A(0), Z(0), XJLv(1, 0), LVP(1, 0), e2p, e3m)
         XN(0) = A(0) - Z(0)
         IZA(0) = INT(1000*Z(0) + A(0))
         ia = INT(A(0))
         iz = INT(Z(0))
         SYMb(0) = SMAT(iz)
         NLV(0) = 1
         ELV(1, 0) = 0.0
         QCC(1) = -e2p
         QCC(2) = -e3m
C--------set target  for EXFOR retrieval
         TARget = '          '
         TARget(1:1) = '<'
         TARget(2:3) = SYMb(0)
         iend = INDEX(TARget, ' ') - 1
         TARget(iend + 1:iend + 2) = '-'
         iend = iend + 1
         WRITE(ca1, '(I3)')ia
         IF(ia.LT.10)THEN
            TARget(iend + 1:iend + 2) = ca1(3:3)
         ELSEIF(ia.LT.100)THEN
            TARget(iend + 1:iend + 3) = ca1(2:3)
         ELSE
            TARget(iend + 1:iend + 4) = ca1(1:3)
         ENDIF
C
C
C--------target ******  done  ********
C--------projectile
         READ(5, *)AEJc(0), ZEJc(0)
         IF(AEJc(0).EQ.0 .AND. ZEJc(0).EQ.0)THEN
C           GAMMA EMISSION
            SEJc(0) = 1
            lpar = -1
         ELSE
            CALL PTLEVSET(AEJc(0), ZEJc(0), SEJc(0), lpar, e2p, e3m)
         ENDIF
C--------product of target and projectile parities
         LVP(LEVtarg, 0) = LVP(LEVtarg, 0)*lpar
         XNEjc(0) = AEJc(0) - ZEJc(0)
         IZAejc(0) = INT(1000.*ZEJc(0) + AEJc(0))
         iz = INT(ZEJc(0))
         SYMbe(0) = SMAT(iz)
         QCC(3) = -e2p
         QCC(4) = -e3m
C--------set projectile  for EXFOR retrieval
         PROjec = '          '
         NCHr = 2
         IF(AEJc(0).EQ.1.0D0 .AND. ZEJc(0).EQ.0.0D0)PROjec = 'N,'
         IF(AEJc(0).EQ.1.0D0 .AND. ZEJc(0).EQ.1.0D0)PROjec = 'P,'
         IF(AEJc(0).EQ.4.0D0 .AND. ZEJc(0).EQ.2.0D0)PROjec = 'A,'
         IF(AEJc(0).EQ.2.0D0 .AND. ZEJc(0).EQ.1.0D0)PROjec = 'D,'
         IF(AEJc(0).EQ.0.0D0 .AND. ZEJc(0).EQ.0.0D0)PROjec = 'G,'
         IF(AEJc(0).EQ.3.0D0 .AND. ZEJc(0).EQ.1.0D0)PROjec = 'T,'
         IF(AEJc(0).EQ.3.0D0 .AND. ZEJc(0).EQ.2.0D0)THEN
            PROjec = 'HE3,'
            NCHr = 4
         ENDIF
C--------***** done ********
C--------NEMN  number of neutrons emitted
         READ(5, *)nemn
C--------NEMP  number of protons  emitted
         READ(5, *)nemp
C--------NEMA  number of alphas   emitted
         READ(5, *)nema
C--------NEMC  number of clusters  emitted
         READ(5, *)NEMc, aclu, zclu
         IF(NDEJC.LT.4)NEMc = 0
C--------cluster ejectile
         IF(NDEJC.GT.3)THEN
            AEJc(NDEJC) = aclu
            ZEJc(NDEJC) = zclu
            IF(aclu.EQ.2.0D0 .AND. zclu.EQ.1.0D0)SEJc(NDEJC) = 1.0
            IF(aclu.EQ.3.0D0 .AND. zclu.EQ.2.0D0)SEJc(NDEJC) = 0.5
            IF(aclu.EQ.6.0D0 .AND. zclu.EQ.3.0D0)SEJc(NDEJC) = 1.0
            IF(aclu.EQ.7.0D0 .AND. zclu.EQ.3.0D0 .OR.
     &         aclu.EQ.7.0D0 .AND. zclu.EQ.4.0D0)SEJc(NDEJC) = 1.5
            XNEjc(NDEJC) = AEJc(NDEJC) - ZEJc(NDEJC)
            IZAejc(NDEJC) = INT(1000.*ZEJc(NDEJC) + AEJc(NDEJC))
            iz = INT(ZEJc(NDEJC))
            SYMbe(NDEJC) = SMAT(iz)
         ENDIF
C--------cluster ejectile *** done ***
C--------ascribing location to each nucleus
C--------compound nucleus 1
         nnuc = 1
         A(1) = A(0) + AEJc(0)
         Z(1) = Z(0) + ZEJc(0)
         XN(1) = A(1) - Z(1)
         IZA(1) = INT(1000*Z(1) + A(1))
         iz = INT(Z(1))
         ia = INT(A(1))
         SYMb(1) = SMAT(iz)
         HIS(1) = -1.
         IF(A(1)*0.5.NE.AINT(A(1)*0.5))HIS(1) = -0.5
C--------set reaction string
         REAction(nnuc) = '(z,gamma)'
C--------set CN  for EXFOR retrieval
         RESidue(nnuc) = '          '
         RESidue(nnuc)(1:1) = '>'
         RESidue(nnuc)(2:3) = SYMb(nnuc)
         iend = INDEX(RESidue(nnuc), ' ') - 1
         RESidue(nnuc)(iend + 1:iend + 2) = '-'
         iend = iend + 1
         WRITE(ca1, '(I3)')ia
         IF(ia.LT.10)THEN
            RESidue(nnuc)(iend + 1:iend + 2) = ca1(3:3)
         ELSEIF(ia.LT.100)THEN
            RESidue(nnuc)(iend + 1:iend + 3) = ca1(2:3)
         ELSE
            RESidue(nnuc)(iend + 1:iend + 4) = ca1(1:3)
         ENDIF
C--------other decaying nuclei
C
C--------NNUcd number of decaying nuclei
C--------NNUct total number of nuclei considered
C
         NEJcm = NDEJC
         IF(aclu.EQ.0.0D0 .OR. zclu.EQ.0.0D0)NEJcm = 3
C--------Plujko_new (projectile symbol correction)
         IF(ZEJc(0).EQ.0.0D0 .AND. AEJc(0).EQ.0.0D0)SYMbe(0) = gamma
C--------Plujko_new (END of  projectile symbol correction)
C
C--------correct ejectiles symbols
         DO nejc = 1, NEJcm
            IF(ZEJc(nejc).EQ.1.0D0 .AND. AEJc(nejc).EQ.2.0D0)SYMbe(nejc)
     &         = deut
            IF(ZEJc(nejc).EQ.1.0D0 .AND. AEJc(nejc).EQ.3.0D0)SYMbe(nejc)
     &         = trit
         ENDDO
         DO iac = 0, NEMc
            DO ia = 0, nema
               DO ip = 0, nemp
                  DO in = 0, nemn
                     IF(iac + ia + ip + in.NE.0)THEN
                        atmp = A(1) - FLOAT(in)*AEJc(1) - FLOAT(ip)
     &                         *AEJc(2) - FLOAT(ia)*AEJc(3)
                        IF(NDEJC.GT.3)atmp = atmp - FLOAT(iac)
     &                     *AEJc(NDEJC)
                        ztmp = Z(1) - FLOAT(in)*ZEJc(1) - FLOAT(ip)
     &                         *ZEJc(2) - FLOAT(ia)*ZEJc(3)
                        IF(NDEJC.GT.3)ztmp = ztmp - FLOAT(iac)
     &                     *ZEJc(NDEJC)
                        izatmp = INT(1000*ztmp + atmp)
                        CALL WHERE(izatmp, nnuc, iloc)
                        IF(iloc.EQ.1)THEN
                           A(nnuc) = atmp
                           Z(nnuc) = ztmp
                           XN(nnuc) = A(nnuc) - Z(nnuc)
                           IZA(nnuc) = izatmp
                           iia = INT(A(nnuc))
                           iz = INT(Z(nnuc))
                           SYMb(nnuc) = SMAT(iz)
                           HIS(nnuc) = -1.
                           IF(A(nnuc)*0.5.NE.AINT(A(nnuc)*0.5))HIS(nnuc)
     &                        = -0.5
C--------------------------set reaction string
                           REAction(nnuc) = '(z,'
                           iend = 3
                           IF(in.NE.0)THEN
                              WRITE(cnejec, '(I1)')in
                              IF(in.GT.1)THEN
                                 REAction(nnuc)(iend + 1:iend + 1)
     &                              = cnejec
                                 iend = iend + 1
                              ENDIF
                              REAction(nnuc)(iend + 1:iend + 1) = 'n'
                              iend = iend + 1
                           ENDIF
                           IF(ip.NE.0)THEN
                              WRITE(cnejec, '(I1)')ip
                              IF(ip.GT.1)THEN
                                 REAction(nnuc)(iend + 1:iend + 1)
     &                              = cnejec
                                 iend = iend + 1
                              ENDIF
                              REAction(nnuc)(iend + 1:iend + 1) = 'p'
                              iend = iend + 1
                           ENDIF
                           IF(ia.NE.0)THEN
                              WRITE(cnejec, '(I1)')ia
                              IF(ia.GT.1)THEN
                                 REAction(nnuc)(iend + 1:iend + 1)
     &                              = cnejec
                                 iend = iend + 1
                              ENDIF
                              REAction(nnuc)(iend + 1:iend + 1) = 'a'
                              iend = iend + 1
                           ENDIF
                           IF(NDEJC.GT.3 .AND. iac.NE.0)THEN
                              WRITE(cnejec, '(I1)')iac
                              IF(iac.GT.1)THEN
                                 REAction(nnuc)(iend + 1:iend + 1)
     &                              = cnejec
                                 iend = iend + 1
                              ENDIF
C                             REAction(nnuc)(iend + 1:iend + 2) = 'li'
C                             iend = iend + 2
                          IF(AEJc(NDEJC).eq.2 .and.
     &                           ZEJc(NDEJC).eq.1) THEN
                                 REAction(nnuc)(iend + 1:iend + 1)='d'
                                 iend = iend + 1
                          ENDIF
                          IF(AEJc(NDEJC).eq.3 .and.
     &                           ZEJc(NDEJC).eq.1) THEN
                                 REAction(nnuc)(iend + 1:iend + 1)='t'
                                 iend = iend + 1
                          ENDIF
                          IF(AEJc(NDEJC).eq.3 .and.
     &                           ZEJc(NDEJC).eq.2) THEN
                                 REAction(nnuc)(iend + 1:iend + 3)='He3'
                                 iend = iend + 3
                          ENDIF
                           ENDIF
                           REAction(nnuc)(iend + 1:iend + 1) = ')'
C--------------------------set residues to be used for EXFOR retrieval
                           RESidue(nnuc) = '          '
                           RESidue(nnuc)(1:1) = '>'
                           RESidue(nnuc)(2:3) = SYMb(nnuc)
                           iend = INDEX(RESidue(nnuc), ' ') - 1
                           WRITE(ca1, '(I3)')iia
                           RESidue(nnuc)(iend + 1:iend + 2) = '-'
                           iend = iend + 1
                           IF(iia.LT.10)THEN
                              RESidue(nnuc)(iend + 1:iend + 2)
     &                           = ca1(3:3)
                           ELSEIF(iia.LT.100)THEN
                              RESidue(nnuc)(iend + 1:iend + 3)
     &                           = ca1(2:3)
                           ELSE
                              RESidue(nnuc)(iend + 1:iend + 4)
     &                           = ca1(1:3)
                           ENDIF
                        ENDIF
                     ENDIF
                  ENDDO
               ENDDO
            ENDDO
         ENDDO
C--------retrieve EXFOR data
         INQUIRE(FILE = 'EXFOR.dat', EXIST = gexist)
         IF(.NOT.gexist)THEN
            IF(IX4ret.EQ.3)CALL RETRIEVE !retrieval from the local database
            IF(IX4ret.EQ.1 .OR. IX4ret.EQ.2)THEN
C--------------on-line EXFOR retrieval from the remote database
               WRITE(atar, '(I3)')INT(A(0))
               IF(atar(1:1).EQ.' ')THEN
                  atar(1:1) = atar(2:2)
                  atar(2:2) = atar(3:3)
                  atar(3:3) = ' '
               ENDIF
               IF(atar(1:1).EQ.' ')THEN
                  atar(1:1) = atar(2:2)
                  atar(2:2) = ' '
               ENDIF
C--------------set projectile  for EXFOR retrieval
               proj = ' '
               IF(AEJc(0).EQ.1.0D0 .AND. ZEJc(0).EQ.0.0D0)proj = 'n'
               IF(AEJc(0).EQ.1.0D0 .AND. ZEJc(0).EQ.1.0D0)proj = 'p'
               IF(AEJc(0).EQ.4.0D0 .AND. ZEJc(0).EQ.2.0D0)proj = 'a'
               IF(AEJc(0).EQ.2.0D0 .AND. ZEJc(0).EQ.1.0D0)proj = 'd'
               IF(AEJc(0).EQ.0.0D0 .AND. ZEJc(0).EQ.0.0D0)proj = 'g'
               IF(AEJc(0).EQ.3.0D0 .AND. ZEJc(0).EQ.1.0D0)proj = 't'
C--------------retrieval from the remote database
               IF(SYMb(0)(2:2).EQ.' ' .AND. IX4ret.EQ.2)THEN
                  x4string =
     &'~/X4Cinda/jre/bin/java -cp jconn2.jar:x4retr.jar: x4retr x -targe
     &t:"'//SYMb(0)(1:1)//'-'//atar//';'//SYMb(0)(1:1)
     &//'-0" -Rea ct:"'//proj//',*"'//' -quant:"CS;DA;DAE;DE;CSP"#'
               ELSEIF(IX4ret.EQ.2)THEN
                  x4string =
     &'~/X4Cinda/jre/bin/java -cp jconn2.jar:x4retr.jar: x4retr x -targe
     &t:"'//SYMb(0)//'-'//atar//';'//SYMb(0)//'-0" -React:"'//proj//
     &',*"'//' -quant:"CS;DA;DAE;DE;CSP"#'
               ENDIF
C--------------retrieval from the local MySQL database
               IF(SYMb(0)(2:2).EQ.' ' .AND. IX4ret.EQ.1)THEN
                  x4string = '../scripts/X4retrieve "'//SYMb(0)(1:1)//
     &                       '-0'//';'//SYMb(0)(1:1)//'-'//atar//'" '//
     &                       '"CS;DA;DAE;DE;CSP" '//'"'//proj//',*"#'
               ELSEIF(IX4ret.EQ.1)THEN
                  x4string = '../scripts/X4retrieve "'//SYMb(0)//'-0'//
     &                       ';'//SYMb(0)//'-'//atar//'" '//
     &                       '"CS;DA;DAE;DE;CSP" '//'"'//proj//',*"#'
               ENDIF
               iwin = PIPE(x4string)
            ENDIF
         ENDIF
C--------retrieve of EXFOR data *** done ***
         NNUcd = nnuc
         NNUct = NNUcd
         DO nnuc = 1, NNUcd
            IF(A(0).EQ.A(nnuc) .AND. Z(0).EQ.Z(nnuc))NTArget = nnuc
            DO nejc = 1, NEJcm
C--------------To find inelastic channel
               IF(AEJc(0).EQ.AEJc(nejc) .AND. ZEJc(0).EQ.ZEJc(nejc))
     &            NPRoject = nejc
               ares = A(nnuc) - AEJc(nejc)
               zres = Z(nnuc) - ZEJc(nejc)
               izares = INT(1000*zres + ares)
               CALL WHERE(izares, nnur, iloc)
               IF(iloc.EQ.1)THEN
                  A(nnur) = ares
                  Z(nnur) = zres
                  XN(nnur) = A(nnur) - Z(nnur)
                  IZA(nnur) = izares
                  iz = INT(Z(nnur))
                  SYMb(nnur) = SMAT(iz)
                  HIS(nnur) = -1.
                  IF(A(nnur)*0.5.NE.AINT(A(nnur)*0.5))HIS(nnur) = -0.5
                  NNUct = NNUct + 1
               ENDIF
            ENDDO
         ENDDO
C--------end ascribing location to each nucleus
C--------next finds indexes of residues that might be needed for ENDF formatting
         CALL WHERE(IZA(1) - 1, INRes, iloc)
         IF(iloc.EQ.1)INRes = -1
         CALL WHERE(IZA(1) - 1001, IPRes, iloc)
         IF(iloc.EQ.1)IPRes = -1
         CALL WHERE(IZA(1) - 2004, IARes, iloc)
         IF(iloc.EQ.1)IARes = -1
         IF(NDEJC.EQ.4)THEN
            CALL WHERE(IZA(1) - IZAejc(NDEJC), ILIres, iloc)
            IF(iloc.EQ.1)ILIres = -1
         ELSE
            ILIres = -1
         ENDIF
C--------inteligent defaults

C--------optical model parameter set selection
         IF(AEJc(0).EQ.0 .AND. ZEJc(0).EQ.0)THEN
C           INCIDENT GAMMA
            KTRlom(0, 0) = -1
         ELSEIF(AEJc(0).LE.4.0D0)THEN
C           KTRlom(0, 0) = 1   (RCN, Bjorklund OMP must be coded into RIPL)
            KTRlom(0, 0) = 2405
         ELSE
            KTRlom(0, 0) = 0
         ENDIF

         DO i = 1, NDNUC
            IF(EIN.GT.20.0D0)THEN
C              KTRlom(1, i) = 3
               KTRlom(1, i) = 100
            ELSE
C              KTRlom(1, i) = 4
               KTRlom(1, i) = 401
            ENDIF
C           KTRlom(2, i) = 2
            KTRlom(2, i) = 4101
C           KTRlom(3, i) = 1
            KTRlom(3, i) = 9100
C           IF(NDEJC.GT.3) KTRlom(NDEJC, i) = 1
            IF(NDEJC.GT.3) KTRlom(NDEJC, i) = 9100
         ENDDO
C
C        It is probably necessary to use Koning's potential as default
C
C--------inteligent defaults *** done ***
C
         CALL READIN   !optional part of the input
C
C        Key_shape =0 --> original (up to 2.18) EMPIRE E1 stength-function
C        Key_shape =1 --> fE1=MLO1
C        Key_shape =2 --> fE1=MLO2
C        Key_shape =3 --> fE1=MLO3
C        Key_shape =4 --> fE1=EGLO
C        Key_shape =5 --> fE1=GFL
C        Key_shape =6 --> fE1=SLO
C
C--------check input for consistency
C
         IF(LHRtw.NE.0 .AND. LTUrbo.NE.1)THEN
            LTUrbo = 1
            WRITE(6, *)' '
            WRITE(6, *)' WARNING:'
            WRITE(6, *)' WARNING:  LTURBO>1 is incompatible ',
     &                 ' WARNING:  with HRTW'
            WRITE(6, *)' WARNING:  LTURBO has been set to 1'
            WRITE(6, *)' '
         ENDIF
         IF(DEGa.GT.0)GCAsc = 1.
         IF(PEQc.GT.0)GCAsc = 1.  ! PCROSS
         IF(MSC*MSD.EQ.0 .AND. (MSD + MSC).NE.0 .AND. A(nnuc)
     &      .GT.1.0D0 .AND. AEJc(0).LE.1.D0)THEN
            WRITE(6, *)' '
            WRITE(6, *)' WARNING: Normally both MSD and MSC should'
            WRITE(6, *)' WARNING: be taken into account'
            WRITE(6, *)' '
         ENDIF
         IF(MSD.NE.0 .AND. AEJc(0).NE.1.D0)THEN
            MSD = 0
            WRITE(6, *)' '
            WRITE(6, *)' WARNING: MSD calculations suppressed'
            WRITE(6, *)' WARNING: (possible for nucleons only)'
            WRITE(6, *)' '
         ENDIF
         IF(MSC.NE.0 .AND. AEJc(0).EQ.0.0D0)THEN
            MSC = 0
            WRITE(6, *)' '
            WRITE(6, *)' WARNING!!!! MSC has been turned off '
            WRITE(6, *)' WARNING!!!! (It is not allowed for '
            WRITE(6, *)' WARNING!!!! photo-nuclear reactions)'
            WRITE(6, *)' '
         ENDIF
         IF(LHRtw.NE.0 .AND. AEJc(0).EQ.0.0D0)THEN
            LHRtw = 0
            WRITE(6, *)' '
            WRITE(6, *)' WARNING!!!! HRTW has been turned off '
            WRITE(6, *)' WARNING!!!! (It is not allowed for '
            WRITE(6, *)' WARNING!!!! photo-nuclear reactions)'
            WRITE(6, *)' '
         ENDIF
         IF(FISBAR(nnuc).NE.1 .AND. FISopt(nnuc).NE. 0)THEN
            FISopt(nnuc) = 0
            WRITE(6, *)' '
            WRITE(6, *)' WARNING!!!! Subbarrier effects in the fission'
            WRITE(6, *)' WARNING!!!! channel allowed only for FISBAR=1'
            WRITE(6, *)' WARNING!!!! FISOPT has been set to 0 '
            WRITE(6, *)' '
         ENDIF
         IF(DIRect.GT.0 .AND. INT(AEJC(0)).EQ.0)THEN
            DIRect = 0
            WRITE(6, *)' '
            WRITE(6, *)' WARNING!!!! Direct mechanism is not supported '
            WRITE(6, *)' WARNING!!!! for the photo-nuclear reactions  '
            WRITE(6, *)' WARNING!!!! and has been turned off  '
            WRITE(6, *)' '

         ENDIF
C--------input consistency check  *** done ***
C
C--------setup model matrix (IDNa) defining which model is used where
C        ECIS   MSD   MSC   DEGAS   HMS   PCROSS
C        1     2     3      4      5      6
C        1 neut. disc.   x     x     0      x      x      0
C        2 neut. cont.   0     x     x      x      x      x
C        3 prot. disc.   x     x     0      x      x      0
C        4 prot. cont.   0     x     x      x      x      x
C        5 gamma         0     0     x      x      0      x
C        6 alpha. cont.  0     0     0      0      0      x
C
C--------with x=1 if used and x=0 if not.
C
C--------initialize matrix with 0's
         DO i = 1, NDREGIONS   !over ejectiles/regions
            DO j = 1, NDMODELS !over models in the order as above
               IDNa(i, j) = 0
            ENDDO
         ENDDO
C--------set ECIS (.,1)
         IF(DIRect.GT.0)THEN
            IF(NPRoject.EQ.1)THEN
               IDNa(1, 1) = 1
            ELSEIF(NPRoject.EQ.2)THEN
               IDNa(3, 1) = 1
            ENDIF
         ENDIF
C--------set MSD  (.,2) (discrete only if ECIS not used)
         IF(MSD.GT.0)THEN
            IF(NPRoject.EQ.1)THEN
               IF(DIRect.EQ.0)IDNa(1, 2) = 1
               IDNa(2, 2) = 1
            ELSEIF(NPRoject.EQ.2)THEN
               IF(DIRect.EQ.0)IDNa(3, 2) = 1
               IDNa(4, 2) = 1
            ENDIF
         ENDIF
C--------set MSC  (.,3) (note no discrete transitions in MSC)
         IF(MSC.GT.0)THEN
            IDNa(2, 3) = 1
            IDNa(4, 3) = 1
            IF(GST.GT.0)IDNa(5, 3) = 1
C-----------stop MSC charge-exchange if DEGAS,DDHMS or PCROSS active
            IF(DEGa.GT.0 .OR. LHMs.GT.0 .OR. PEQc.GT.0.)THEN
               IF(NPRoject.EQ.1)THEN
                  IDNa(4, 3) = 0
               ELSEIF(NPRoject.EQ.2)THEN
                  IDNa(2, 3) = 0
               ELSE
                  WRITE(6, *)''
                  WRITE(6, *)'DEGAS/PCROSS AND MSD/MSC ARE NOT COMPATIB'
                  WRITE(6, *)'LE IN THIS CASE. EXECUTION S T O P P E D '
                  STOP 'ILLEGAL COMBINATION DEGAS/PCROSS + MSC/MSD !!!!'
               ENDIF
            ENDIF
         ENDIF
C--------set DEGAS  (.,4) (nucleons to discrete levels NOT blocked)
         IF(DEGa.GT.0)THEN
            IDNa(1, 4) = 1
            IDNa(2, 4) = 1
            IDNa(3, 4) = 1
            IDNa(4, 4) = 1
            IDNa(5, 4) = 1
C-----------stop DEGAS gammas if calculated within MSC
            IF(GST.GT.0 .AND. MSC.GT.0)IDNa(5, 4) = 0
C-----------stop DEGAS inelastic scattering if MSC and/or MSD active
            IF(MSC.GT.0 .OR. MSD.GT.0)THEN
               IF(NPRoject.EQ.2)THEN
                  IDNa(3, 4) = 0
                  IDNa(4, 4) = 0
               ELSEIF(NPRoject.EQ.1)THEN
                  IDNa(1, 4) = 0
                  IDNa(2, 4) = 0
               ELSE
                  WRITE(6, *)''
                  WRITE(6, *)'DEGAS AND MSD/MSC ARE NOT COMPATIBLE IN '
                  WRITE(6, *)'THIS CASE. EXECUTION S T O P P E D !!!! '
                  STOP 'ILLEGAL COMBINATION DEGAS + MSC/MSD'
               ENDIF
            ENDIF
C-----------stop DEGAS particle channels if HMS active
            IF(LHMs.GT.0)THEN
               IDNa(1, 4) = 0
               IDNa(2, 4) = 0
               IDNa(3, 4) = 0
               IDNa(4, 4) = 0
            ENDIF
C-----------stop DEGAS completely if PCROSS is active
CMH---------NO, lets' leave it so that we can have DEGAS and PCROSS clusters
C           IF(PEQc.GT.0)THEN
C           IDNa(1, 4) = 0
C           IDNa(2, 4) = 0
C           IDNa(3, 4) = 0
C           IDNa(4, 4) = 0
C           IDNa(5, 4) = 0
C           ENDIF
         ENDIF
C--------set HMS  (.,5)
         IF(LHMs.GT.0)THEN
            IDNa(1, 5) = 1
            IDNa(2, 5) = 1
            IDNa(3, 5) = 1
            IDNa(4, 5) = 1
C-----------stop HMS inelastic scattering if MSC and/or MSD active
            IF(MSC.GT.0 .OR. MSD.GT.0)THEN
               IF(NPRoject.EQ.2)THEN
                  IDNa(3, 5) = 0
                  IDNa(4, 5) = 0
               ELSEIF(NPRoject.EQ.1)THEN
                  IDNa(1, 5) = 0
                  IDNa(2, 5) = 0
               ELSE
                  WRITE(6, *)''
                  WRITE(6, *)'HMS AND MSD/MSC ARE NOT COMPATIBLE IN '
                  WRITE(6, *)'THIS CASE. EXECUTION S T O P P E D !!!! '
                  STOP 'ILLEGAL COMBINATION HMS + MSC/MSD'
               ENDIF
            ENDIF
         ENDIF
C--------set PCROSS  (.,6) cluster emission
         IF(PEQc.GT.0)THEN
            IDNa(1, 6) = 0
            IDNa(2, 6) = 1
            IDNa(3, 6) = 0
            IDNa(4, 6) = 1
            IDNa(5, 6) = 1
            IDNa(6, 6) = 1
C-----------stop PCROSS gammas if calculated within MSC
            IF(GST.GT.0 .AND. MSC.GT.0)IDNa(5, 6) = 0
C-----------stop PCROSS inelastic scattering if MSC and/or MSD active
            IF(MSC.GT.0 .OR. MSD.GT.0)THEN
               IF(NPRoject.EQ.2)THEN
C                 IDNa(3, 6) = 0
                  IDNa(4, 6) = 0
               ELSEIF(NPRoject.EQ.1)THEN
C                 IDNa(1, 6) = 0
                  IDNa(2, 6) = 0
               ELSE
                  WRITE(6, *)''
                  WRITE(6, *)'PCROSS AND MSD/MSC ARE NOT COMPATIBLE IN '
                  WRITE(6, *)'THIS CASE. EXECUTION S T O P P E D !!!! '
                  STOP 'ILLEGAL COMBINATION PCROSS + MSC/MSD'
               ENDIF
            ENDIF
C-----------stop PCROSS nucleon channels if HMS active
            IF(LHMs.GT.0)THEN
               IDNa(2, 6) = 0
               IDNa(4, 6) = 0
            ENDIF
C-----------stop PCROSS nucleon and gamma channels if DEGAS active
            IF(DEGa.GT.0)THEN
               IDNa(2, 6) = 0
               IDNa(4, 6) = 0
               IDNa(5, 6) = 0
            ENDIF
         ENDIF
C--------print IDNa matrix
         WRITE(6, *)' '
         WRITE(6, *)
     &             '                      Use of preequilibrium models '
         WRITE(6, *)
     &             '                      ---------------------------- '
         WRITE(6, *)' '
         WRITE(6, *)'Exit channel       ECIS       MSD       MSC',
     &              '      DEGAS      HMS      PCROSS'
         WRITE(6, *)' '
         WRITE(6, '('' neut. disc. '',8I10)')
     &         (IDNa(1, j), j = 1, NDMODELS)
         WRITE(6, '('' neut. cont. '',8I10)')
     &         (IDNa(2, j), j = 1, NDMODELS)
         WRITE(6, '('' prot. disc. '',8I10)')
     &         (IDNa(3, j), j = 1, NDMODELS)
         WRITE(6, '('' prot. cont. '',8I10)')
     &         (IDNa(4, j), j = 1, NDMODELS)
         WRITE(6, '('' gammas      '',8I10)')
     &         (IDNa(5, j), j = 1, NDMODELS)
         WRITE(6, '('' alpha cont. '',8I10)')
     &         (IDNa(6, j), j = 1, NDMODELS)
         WRITE(6, *)' '
C--------model matrix *** done ***
C--------reset some options if OMP fitting option selected
         IF(FITomp.GT.0)THEN
            IOUt = 1
            NEXreq = 10
            GCAsc = 1
            ENDf = 1
            MSD = 0
            MSC = 0
            LHMs = 0
            DEGa = 0
            PEQc = 0
            MFPp = 1.3
            NNUcd = 1
            NNUct = 4
         ENDIF
C
C--------read nuclear deformations and masses
         CALL READNIX
C--------set projectile/ejectile masses
         DO nejc = 0, NDEJC
            EJMass(nejc) = (AEJc(nejc)*AMUmev + XMAss_ej(nejc))/AMUmev
         ENDDO
C--------read number of reasonably known levels and level density parameter 'a'
C--------according to Mebel (GC) or EMPIRE systematics (dynamic l.d.)
         CALL READLDP
C--------Capote 2001
C        IF(DIRect.GT.0 .AND. AEJc(0).LE.1.0D0)THEN
         IF(DIRect.GT.0)THEN  ! Inelastic scattering by DWBA for all particles

C--------fix-up deformations and discrete levels for ECIS coupled channels
            ierr = IFINDCOLL()

C           Defining ICOller(i)
            DO i = 2, ND_nlv
             itmp=ICOllev(i)
             if(itmp.gt.50) itmp = ICOllev(i) - 50
             ICOller(i) = itmp
            ENDDO

            IF(ierr.EQ.1)THEN
               WRITE(6, *)' WARNING: Some collective discrete levels',
     &                    '  for target nucleus not found'
               WRITE(6, *)' WARNING: check TARGET.LEV file '
            ELSEIF(ierr.EQ.2)THEN
               WRITE(6, *)' WARNING: No discrete levels for target',
     &                    ' nucleus found'
               WRITE(6, *)' WARNING: Direct cross section will not be',
     &                    ' calculated'
               WRITE(6, *)' WARNING: Setting DIRECT to 0 '
               DIRect = 0
            ELSEIF(ierr.EQ.3)THEN
               WRITE(6, *)' WARNING: Spherical odd nuclei, no hints',
     &                    ' for collective states'
               WRITE(6, *)' WARNING:  Check directly TARGET.LEV file '
               WRITE(6, *)' WARNING:  Direct cross section will not',
     &                    ' be calculated'
               WRITE(6, *)' WARNING:  setting DIRECT to 0 '
               DIRect = 0
            ENDIF
         ENDIF
C--------fix-up deformations for CCFUS coupled channels
         IF(CSRead.EQ.( - 2.0D0) .AND. AEJc(0).GT.4.0D0)THEN
            DO j = 1, NSCc
               IF(QCC(j).EQ.0.0D0)THEN
                  IF(FLAm(j).GE.0.0D0)WRITE(6, *)' Collective state ',
     &               ABS(FLAm(j)), ' in target (sequence number', j,
     &               ') has excitation energy of 0 MeV'
                  IF(FLAm(j).LE.0.0D0)WRITE(6, *)' Collective state ',
     &               ABS(FLAm(j)), ' in projectile (sequence number', j,
     &               ') has excitation energy of 0 MeV'
                  WRITE(6, *)
     &            ' Likely the code was not able to find out this state'
                  WRITE(6, *)
     &                 ' you must set this energy in the optional input'
                  iccerr = 1
               ENDIF
               IF(BETcc(j).EQ.0.0D0)THEN
                  IF(FLAm(j).LT.0.0D0)BETcc(j) = DEFprj
                  IF(FLAm(j).GT.0.0D0)BETcc(j) = DEF(1, 0)
               ENDIF
            ENDDO
            IF(iccerr.EQ.1)STOP 'CCFUS STATE MISSING'
         ENDIF
C--------fix-up deformations for coupled channels *** done ***
         DO nnuc = 0, NDNUC
            DO nejc = 0, NDEJC
               IOMwrite(nejc, nnuc) = 0
            ENDDO
         ENDDO
      ENDIF
      NLW = NDLW
      CSFus = CSRead
C-----KTRLOM Optical Model control
C-----set o.m.p. for the incident channel
C     RCN 02/2004
      KTRlom(0, 0) = 1
      IF(AEJc(0).GT.4.0D0)THEN
         KTRlom(0, 0) = 0
      ELSE
         DO nejc = 1, NDEJC
            IF(ZEJc(0).EQ.ZEJc(nejc) .AND. AEJc(0).EQ.AEJc(nejc))
     &          KTRlom(0, 0) = KTRlom(nejc, 1)
         ENDDO
      ENDIF
      IF(KTRompcc.GT.0 .AND. DIRect.EQ.2)THEN
         KTRlom(0, 0) = KTRompcc
         KTRlom(NPRoject, NTArget) = KTRompcc
      ENDIF
C-----Plujko_new (set giant resonance parameters for target)
      GDRpar(1, 0) = EGDr1
      GDRpar(2, 0) = GGDr1
      GDRpar(3, 0) = CSGdr1
      GDRpar(4, 0) = EGDr2
      GDRpar(5, 0) = GGDr2
      GDRpar(6, 0) = CSGdr2
      GDRpar(7, 0) = GDRweis
      GDRpar(8, 0) = 0.0
      GQRpar(1, 0) = 0.0
      GQRpar(2, 0) = 0.0
      GQRpar(3, 0) = 0.0
      GQRpar(4, 0) = 0.0
      GQRpar(5, 0) = 0.0
      GQRpar(6, 0) = 0.0
      GQRpar(7, 0) = 1.0
      GQRpar(8, 0) = 0.0
      GMRpar(1, 0) = 0.0
      GMRpar(2, 0) = 0.0
      GMRpar(3, 0) = 0.0
      GMRpar(4, 0) = 0.0
      GMRpar(5, 0) = 0.0
      GMRpar(6, 0) = 0.0
      GMRpar(7, 0) = 1.0
      GMRpar(8, 0) = 0.0
C-----Plujko_new (END of set giant resonance parameters for target)
C
C-----compound nucleus 1
      nnuc = 1
      IF(NEX(1).GT.NDEX)THEN
         WRITE(6, *)' NUMBER OF ENERGY BINS IN COMP. NUCL. SET TO', NDEX
         NEX(1) = max(NDEX - 2,2)
      ENDIF
C-----determination of discrete levels and pairing shift for cn
      CALL LEVREAD(nnuc)
      IF(ROPar(3, nnuc).EQ.0.0D0)THEN
         IF(Z(nnuc).GT.98.0D0 .OR. ROPaa(nnuc).LE.0.0D0)THEN
            delp = 12.0/SQRT(A(nnuc))
            IF(Z(nnuc)/2. - AINT(Z(nnuc)/2.).LT.0.01D0)ROPar(3, nnuc)
     &         = delp
         ELSE
            ROPar(3, nnuc) = delz(INT(Z(nnuc) + 0.001))
         ENDIF
         IF(XN(nnuc).GT.150.0D0 .OR. ROPaa(nnuc).LE.0.0D0)THEN
            delp = 12.0/SQRT(A(nnuc))
            IF(XN(nnuc)/2. - AINT(XN(nnuc)/2.).LT.0.01D0)ROPar(3, nnuc)
     &         = ROPar(3, nnuc) + delp
         ELSE
            ROPar(3, nnuc) = ROPar(3, nnuc)
     &                       + deln(INT(XN(nnuc) + 0.001))
         ENDIF
      ENDIF
C-----set giant resonance parameters for CN
      GDRpar(1, nnuc) = EGDr1
      GDRpar(2, nnuc) = GGDr1
      GDRpar(3, nnuc) = CSGdr1
      GDRpar(4, nnuc) = EGDr2
      GDRpar(5, nnuc) = GGDr2
      GDRpar(6, nnuc) = CSGdr2
      GDRpar(7, nnuc) = GDRweis
      GDRpar(8, nnuc) = 0.0
      GQRpar(1, nnuc) = 0.0
      GQRpar(2, nnuc) = 0.0
      GQRpar(3, nnuc) = 0.0
      GQRpar(4, nnuc) = 0.0
      GQRpar(5, nnuc) = 0.0
      GQRpar(6, nnuc) = 0.0
      GQRpar(7, nnuc) = 1.0
      GQRpar(8, nnuc) = 0.0
      GMRpar(1, nnuc) = 0.0
      GMRpar(2, nnuc) = 0.0
      GMRpar(3, nnuc) = 0.0
      GMRpar(4, nnuc) = 0.0
      GMRpar(5, nnuc) = 0.0
      GMRpar(6, nnuc) = 0.0
      GMRpar(7, nnuc) = 1.0
      GMRpar(8, nnuc) = 0.0
      IF(Q(0, 1).EQ.0.0D0)THEN
         CALL BNDG(0, 1, Q(0, 1))
Csinsin
C        Q(1,1)=4.
C        Q(0,1)=2.
         AMAss(0) = (A(0)*AMUmev + XMAss(0))/AMUmev
      ENDIF
      EINl = EIN
C     Capote 10/01
      CALL KINEMA(EINl, EIN, EJMass(0), AMAss(0), RMU, ak2, 1, RELkin)
C     EIN = EIN*A(0)/A(1)
      CALL LEVREAD(0)
      EXCn = EIN + Q(0, 1) + ELV(LEVtarg, 0)
      EMAx(1) = EXCn
C-----set Q-value for CN production
      QPRod(1) = Q(0, 1)
C-----WRITE heading on FILE12
      ia = INT(A(0))
      iae = INT(AEJc(0))
      WRITE(12, *)' '
      WRITE(12,
     &'('' REACTION '',I3,''-'',A2,''-'',I3,'' + '',I3,''-'',  A2,''-'',
     &I3,'' INCIDENT ENERGY  ''                                ,G9.3,''
     &MeV'')')INT(ZEJc(0)), SYMbe(0), iae, INT(Z(0)), SYMb(0), ia, EINl
      IF(ENDf.EQ.0.0D0)THEN
Cpr      WRITE (12,
Cpr      1'('' QFIS= '',F4.2,'' BETAV='',F5.2,'' J(1/2)='',F5.2,   '' DeltaJ
Cpr      2='',F4.2)') QFIS, BETAV, SHRJ, SHRD
Cpr      WRITE (12, '('' DEFPAR='',F7.3)') DEFPAR
         IF(DEFga.NE.0.0D0)WRITE(12,
     &        '('' DEFGA='',F7.3,'' DEFGW='',F7.3,    '' DEFGP='',F7.3)'
     &        )DEFga, DEFgw, DEFgp
C        WRITE(12,'('' SIG='',F5.1,'' TRUNC='',F5.1,'' EXPUSH='',F6.2)')
C        *SIG, TRUNC,EXPUSH
      ENDIF
      WRITE(12, '('' COMPOUND NUCLEUS ENERGY'',F9.3,'' MeV'')')EXCn
C-----WRITE heading on FILE6
      IF(IOUt.GT.0)THEN
         WRITE(6, *)' '
         WRITE(6, *)' '
         WRITE(6, *)' '
         WRITE(6, 99001)
         WRITE(6,
     &'('' Reaction '',I3,A2,''+'',I3,A2,'' at incident energy '',G9.3,'
     &' MeV'')')iae, SYMbe(0), ia, SYMb(0), EINl
         WRITE(6, 99001)
         WRITE(6, *)' '
         WRITE(6, '('' Compound nucleus energy'',F9.3,'' MeV'')')EXCn
         WRITE(6, '('' Projectile binding energy'',F8.3,'' MeV'')')
     &         Q(0, 1)
      ENDIF

c
c     eLIMINADO EL CALCULO DE direct>0 DE AQUI
C          (VER mareng)  RCN 01/2005
c

C
C-----determination of excitation energy matrix in cn
C
      ECUt(1) = ELV(NLV(1), 1)
      NEX(1) = NEXreq
      IF(FITlev.GT.0.0D0) THEN
         ECUt(1) = 0.0
C
C        RCN, 09/2004 (If ENDF ne 0, then MAx(Ncut)=40 !! (no good option)
C--------set ENDF flag to 0 (no ENDF file for formatting)
         ENDF=0
      ENDIF
C-----check whether any residue excitation is higher than CN
      qmin = 0.0
      DO i = 1, NDEJC
         CALL BNDG(i, 1, qtmp)
         IF(qtmp.LT.qmin)qmin = qtmp
      ENDDO
      IF(EMAx(1) - ECUt(1).LT.EMAx(1) - qmin)THEN
         DE = (EMAx(1) - qmin)/FLOAT(NEXreq - 1)
         NEX(1) = max(INT((EMAx(1) - ECUt(1))/DE),2)
      ENDIF
      DE = (EMAx(1) - ECUt(1))/FLOAT(NEX(1) - 1)
      DO i = 1, NEX(1)
         EX(i, 1) = ECUt(1) + FLOAT(i - 1)*DE
      ENDDO
C-----set energy bin for recoils (max. energy is increased by 5%)
      IF(AEJc(NDEJC).GT.AEJc(3))THEN
         DERec = (EINl - EIN + (EMAx(1) - MIN(0.0D0,qmin))
     &           *1.05*AEJc(NDEJC)/A(1))/FLOAT(NEXreq - 1)
      ELSE
         DERec = (EINl - EIN + (EMAx(1) - MIN(0.0D0,qmin))*1.05*AEJc(3)
     &           /A(1))/FLOAT(NEXreq - 1)
      ENDIF
C-----energy bins for recoils is increased to avoid fluctuations
C-----if these persist increase multiplier
      DERec = DERec*2.00
C-----set initial 'recoil spectrum' of CN (CM motion in LAB)
      irec = (EINl - EIN)/DERec + 1.001
C-----setting irec=1 below practically removes CM motion energy from recoils
      irec = 1
      RECcse(irec, NEX(1), 1) = 1.0
C-----calculate compound nucleus level density
      ARGred = -1.
      IF(ADIv.EQ.0.0D0)CALL ROEMP(nnuc, 0.0D0, 0.024D0)
      IF(ADIv.EQ.1.0D0)CALL ROCOL(nnuc, 0.0D0, 2.D0)
C     RCN 12/2004
C     It should be added <m2> to the input ( to use 0.146 if needed)
      IF(ADIv.EQ.2.0D0)CALL ROGC(nnuc, 0.24D0)
      IF(ADIv.EQ.3.0D0)CALL ROHFBCS(nnuc)
      IF(ADIv.GT.3.0D0)CALL ROCOL(nnuc, 0.0D0, 1.D0)
      IF(IOUt.EQ.6)THEN
         ia = INT(A(nnuc))
         WRITE(6, '(1X,/,'' LEVEL DENSITY FOR '',I3,''-'',A2,/)')ia,
     &         SYMb(nnuc)
         WRITE(6, 99002)(EX(i, nnuc),
     &      (RO(i,j,nnuc)*EXP(ARGred), j = 1, 12), i = 1, NEX(nnuc))
      ENDIF
C
C-----other decaying nuclei
C
      DO nnuc = 1, NNUcd
         DO nejc = 1, NEJcm
            ares = A(nnuc) - AEJc(nejc)
            zres = Z(nnuc) - ZEJc(nejc)
            izares = INT(1000*zres + ares)
            CALL WHERE(izares, nnur, iloc)
            IF(iloc.EQ.1)THEN
               WRITE(6, '('' NO LOCATION ASCRIBED TO NUCLEUS '')')izares
               STOP
            ENDIF
            IF(EMAx(nnur).EQ.0.0D0)THEN
C--------------determination of discrete levels and pairing shifts for rn
               IF(LVP(1, nnur).EQ.0)CALL LEVREAD(nnur)
               IF(ROPar(3, nnur).EQ.0.0D0)THEN
                  IF(Z(nnur).GT.98.0D0 .OR. ROPaa(nnur).LE.0.0D0)THEN
                     delp = 12.0/SQRT(A(nnur))
                     IF(Z(nnur)/2. - AINT(Z(nnur)/2.).LT.0.01D0)
     &                  ROPar(3, nnur) = delp
                  ELSE
                     ROPar(3, nnur) = delz(INT(Z(nnur) + 0.001))
                  ENDIF
                  IF(XN(nnur).GT.150.D0 .OR. ROPaa(nnur).LE.0.0D0)THEN
                     delp = 12.0/SQRT(A(nnur))
                     IF(XN(nnur)/2. - AINT(XN(nnur)/2.).LT.0.01D0)
     &                  ROPar(3, nnur) = ROPar(3, nnur) + delp
                  ELSE
                     ROPar(3, nnur) = ROPar(3, nnur)
     &                                + deln(INT(XN(nnur) + 0.001))
                  ENDIF
               ENDIF
C--------------determination of giant resonance parameters for residual nuclei
               GDRpar(1, nnur) = EGDr1
               GDRpar(2, nnur) = GGDr1
               GDRpar(3, nnur) = CSGdr1
               GDRpar(4, nnur) = EGDr2
               GDRpar(5, nnur) = GGDr2
               GDRpar(6, nnur) = CSGdr2
               GDRpar(7, nnur) = GDRweis
               GDRpar(8, nnur) = 0.0
               GQRpar(1, nnur) = 0.0
               GQRpar(2, nnur) = 0.0
               GQRpar(3, nnur) = 0.0
               GQRpar(4, nnur) = 0.0
               GQRpar(5, nnur) = 0.0
               GQRpar(6, nnur) = 0.0
               GQRpar(7, nnur) = 1.0
               GQRpar(8, nnur) = 0.0
               GMRpar(1, nnur) = 0.0
               GMRpar(2, nnur) = 0.0
               GMRpar(3, nnur) = 0.0
               GMRpar(4, nnur) = 0.0
               GMRpar(5, nnur) = 0.0
               GMRpar(6, nnur) = 0.0
               GMRpar(7, nnur) = 1.0
               GMRpar(8, nnur) = 0.0
            ENDIF
C-----------determination of excitation energy matrix in res. nuclei
            ECUt(nnur) = ELV(NLV(nnur), nnur)
            IF(FITlev.GT.0.0D0)ECUt(nnur) = 0.0
            IF(Q(nejc, nnuc).EQ.0.0D0)THEN
               CALL BNDG(nejc, nnuc, Q(nejc, nnuc))
               AMAss(nnuc) = (A(nnuc)*AMUmev + XMAss(nnuc))/AMUmev
            ENDIF
            emaxr = 0.0
            IF(NEX(nnuc).GT.0)emaxr = EX(NEX(nnuc), nnuc)
     &                                - Q(nejc, nnuc)
            EMAx(nnur) = DMAX1(emaxr, EMAx(nnur))
            NEX(nnur) = max(INT((EMAx(nnur) - ECUt(nnur))/DE + 1.0),0)
            NEXr(nejc, nnuc) =
     &                max(INT((emaxr - ECUt(nnur))/DE + 1.0),0)
            IF(NEX(nnur).GT.NDEX)THEN
               WRITE(6, *)' NUMBER OF BINS IN RESIDUAL NUCLEUS A=',
     &                    A(nnur), 'AND Z=', Z(nnur),
     &                    ' EXCEEDS DIMENSIONS'
               WRITE(6, *)
     &               ' YOU HAVE TO DECREASE NUMBER OF STEPS AND RESTART'
               NEX(1) = INT(NEX(1)*FLOAT(NDEX)/FLOAT(NEX(nnur))) - 1
               STOP
C              CALL CLEAR
            ENDIF
            IF(NEX(nnur).GT.0)THEN
               DO i = 1, NEX(nnur)
                  IF(Z(1).EQ.Z(nnur) .AND. FITlev.EQ.0.0D0)THEN
                     EX(NEX(nnur) - i + 1, nnur) = EMAx(nnur)
     &                  - FLOAT(i - 1)*DE
                  ELSE
                     EX(i, nnur) = ECUt(nnur) + FLOAT(i - 1)*DE
                  ENDIF
               ENDDO
            ENDIF
            IF(Z(1).EQ.Z(nnur) .AND. NEX(nnur).GT.0)ECUt(nnur)
     &         = EX(1, nnur)
C-----------determination of Q-value for isotop production
            qtmp = QPRod(nnuc) - Q(nejc, nnuc)
            IF(qtmp.GT.QPRod(nnur))QPRod(nnur) = qtmp
            IF(FITlev.GT.0.0D0)ECUt(nnur) = 0.0
C-----------determination of etl matrix and transmission coeff. calculation
C-----------first 4 elements are set independently in order to get more
C-----------precise grid at low energies. from the 5-th element on the step
C-----------is de (bin width).
C-----------determination of etl matrix
            netl = 6
            IF(NEX(nnuc).GT.0)netl = INT((EX(NEX(nnuc),nnuc) - Q(nejc,
     &                               nnuc))/DE) + 6
            IF(netl.GT.NDETL)THEN
               WRITE(6, *)
     &             ' OUT OF BOUNDARY; DECREASE NEX IN INPUT OR INCREASE'
     &             , ' NDEX IN dimension.h AND RECOMPILE'
               STOP 10
            ENDIF
            IF(NEXr(nejc, nnuc).GT.0 .AND. NEX(nnuc).GT.0)THEN
               ETL(5, nejc, nnur) = EX(NEX(nnuc), nnuc)
     &                              - EX(NEXr(nejc, nnuc), nnur)
     &                              - Q(nejc, nnuc)
            ELSE
               ETL(5, nejc, nnur) = 0.
            ENDIF
            IF(nejc.EQ.1)ETL(5, nejc, nnur) = 0.
Cpr         WRITE(6,*) 'etl(5,.),netl',etl(5,nejc,nnur),netl
Cpr         etlmax=EX(NEX(NNUC),NNUC)-Q(NEJC,NNUC)
Cpr         WRITE(6,*) 'etlmax',etlmax
            ETL(1, nejc, nnur) = 0
            ETL(2, nejc, nnur) = 0.1*ETL(5, nejc, nnur)
            ETL(3, nejc, nnur) = 0.2*ETL(5, nejc, nnur)
            ETL(4, nejc, nnur) = 0.5*ETL(5, nejc, nnur)
            DO ietl = 6, netl
               ETL(ietl, nejc, nnur) = ETL(ietl - 1, nejc, nnur) + DE
            ENDDO
C
Cpr         WRITE(6,*)
Cpr         >        'TL ENERGIES FOR TARGET A=',A(NNUR),' PROJECTILE A=',
Cpr         >        AEJC(NEJC),' Z=',ZEJC(NEJC)
Cpr         DO I=1,NETL
Cpr         WRITE(6,*) I,ETL(I,NEJC,NNUR)
Cpr         END DO
C-----------calculate tramsmission coefficients
            IF(FITlev.EQ.0)THEN
               CALL TLEVAL(nejc, nnur, nonzero)
C--------------print transmission coefficients
               IF(nonzero .AND. IOUt.EQ.5)THEN
                  WRITE(6, *)'Transmission coefficients for '
                  WRITE(6, '(1x,A14,I3,A3,I3,A3,F4.1)')'Projectile: A=',
     &                  INT(AEJc(nejc)), ' Z=', INT(ZEJc(nejc)), ' S=',
     &                  SEJc(nejc)
                  WRITE(6, '(1x,A10,I3,A3,I3,A3,F4.1,A3,I2)')
     &                  'TARGET: A=', INT(A(nnur)), ' Z=', INT(Z(nnur)),
     &                  ' S=', SNGL(XJLv(LEVtarg, nnur)), ' P=',
     &                  INT(LVP(LEVtarg, nnur))
                  DO i = 1, netl
                     IF(TL(i, 1, nejc, nnur).GT.0.0)WRITE(6, 99002)
     &                  ETL(i, nejc, nnur),
     &                  (TL(i, j, nejc, nnur), j = 1, 12)
                  ENDDO
                  WRITE(6, '(1X,/)')
               ENDIF
C--------------check of etl index determination (to be deleted)
C              IEXR=NEX(NNUC)-NEXR(NEJC,NNUC)
C              ITLC=IEXR-5
C              WRITE(6,*) 'IEXR, ITLC, Q',IEXR,ITLC,Q(NEJC,NNUC)
C              do 100 i=nex(nnuc),2,-1
C              JMAX=I-IEXR
C              do 100 j=jmax,1,-1
C              etlr=ex(i,nnuc)-ex(j,nnur)-q(nejc,nnuc)
C              jtl=i-j-itlc
C              100         WRITE(6,*) 'i,j,etlr,jtl,etl ',
C              &          i,j,etlr,jtl,etl(jtl,nejc,nnur)
C--------------check of etl index determination done
C-----------determination of etl matrix and transmission coeff.--done
            ENDIF
         ENDDO     !over ejectiles (nejc)
      ENDDO     !over nuclei (nnuc)
      WRITE(6, *)' '
      WRITE(6, *)'Total number of nuclei considered ', NNUct
      WRITE(6, *)' '
C-----calculate residual nucleus level density
      DO nnur = 2, NNUct
         IF(NEX(nnur).GE.1 .OR. FITlev.GT.0)THEN
            IF(ADIv.EQ.0.0D0)CALL ROEMP(nnur, 0.0D0, 0.024D0)
            IF(ADIv.EQ.1.0D0)CALL ROCOL(nnur, 0.D0, 2.D0)

C           RCN 12/2004
C           It should be added <m2> to the input ( to use 0.124 if needed)
            IF(ADIv.EQ.2.0D0)CALL ROGC(nnur, 0.24D0)
C           IF(ADIv.EQ.2.0D0)CALL ROGC(nnur, 0.146D0)

            IF(ADIv.EQ.3.0D0)CALL ROHFBCS(nnur)
            IF(ADIv.GT.3.0D0)CALL ROCOL(nnur, 0.D0, 1.D0)
            IF(IOUt.EQ.6)THEN
               ia = INT(A(nnur))
               WRITE(6, '(1X,/,'' LEVEL DENSITY FOR '',I3,''-'',A2,/)')
     &               ia, SYMb(nnur)
               WRITE(6, 99002)(EX(i, nnur), (RO(i,j,nnur), j = 1, 12),
     &                        i = 1, NEX(nnur))
            ENDIF
         ELSE
            ia = INT(A(nnur))
            iz = INT(Z(nnur))
         ENDIF
      ENDDO
      DO i = 1, NDLW
         DRTl(i) = 1.0
      ENDDO
C
      IF(FITlev.GT.0.D0)THEN
C--------remove potentially empty omp files
C--------OMPAR.DIR
         CLOSE(33, STATUS = 'DELETE')
C--------OMPAR.RIPL
         CLOSE(29, STATUS = 'DELETE')
         STOP 'PLOTS DONE'
      ENDIF

C---- fission input is created if it does not exist and FISSHI=0
      DO nnuc = 1, NNUct
         FISsil(nnuc) = .TRUE.
C--------the fissioning condition replaced according to  fission
C--------barriers available in RIPL-2  (MS)
         IF(Z(nnuc).LT.78.)FISsil(nnuc) = .FALSE.
         IF(FISshi(nnuc).EQ.2.)FISsil(nnuc) = .FALSE.
         IF(FISshi(nnuc).EQ.1.)THEN
            FISsil(nnuc) = .TRUE.
            xfis = 0.0205*Z(nnuc)**2/A(nnuc)
            IF(xfis.LT.0.3D0)FISsil(nnuc) = .FALSE.
         ENDIF
      ENDDO
      IF(FISshi(nnuc).EQ.0)THEN
         INQUIRE(FILE = 'FISSION.INP', EXIST = gexist)
         IF(.NOT.gexist)THEN
            OPEN(79, FILE = 'FISSION.INP', STATUS = 'UNKNOWN')
            DO nnuc = 1, NNUcd
               IF(FISsil(nnuc))CALL INPFIS(nnuc)
            ENDDO
         ENDIF
         CLOSE(79)
      ENDIF
99001 FORMAT(1X, 60('='))
99002 FORMAT(1X, 13G10.4)
      END
C*==levread.spg  processed by SPAG 6.20Rc at 12:12 on  7 Jul 2004
C
C
C
      SUBROUTINE LEVREAD(Nnuc)
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:ipu*
Ccc   *                     L E V R E A D                                *
Ccc   *                                                                  *
Ccc   *  Reads level energies, spins, parities and branching ratios from *
Ccc   *  file 13 and puts them into appropriate arrays in GLOBAL.        *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * input:NNUC - nucleus index (position) in the table               *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * output:none                                                      *
Ccc   *                                                                  *
Ccc   * calls: none                                                      *
Ccc   *                                                                  *
Ccc   * author: M.Herman                                                 *
Ccc   * date:   11.Jan.1996                                              *
Ccc   * revision:     by:                         on:                    *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C
C Dummy arguments
C
      INTEGER Nnuc
C
C Local variables
C
      CHARACTER*110 ch_iuf
      CHARACTER*3 ctmp3
      CHARACTER*5 chelem
      CHARACTER*9 finp
      DOUBLE PRECISION dum, sum, qn
      INTEGER ia, iar, ilv, iz, izr, nbr, nlvr
C     INTEGER INT, MIN
C-----set ground state in case nucleus not in file
      NLV(Nnuc) = 1
      NCOmp(Nnuc) = 1
      ELV(1, Nnuc) = 0.0
      LVP(1, Nnuc) = 1
      XJLv(1, Nnuc) = 0.0
      IF(A(Nnuc) - 2.0*INT(A(Nnuc)/2.0).GT.0.01D0)XJLv(1, Nnuc) = 0.5
C-----set ground state *** done ***
      ia = A(Nnuc) + 0.001
      iz = Z(Nnuc) + 0.001
      IF(.NOT.FILevel)THEN
C--------constructing input and filenames
         WRITE(ctmp3, '(I3.3)')iz
         finp = 'z'//ctmp3//'.dat'
         OPEN(13, FILE = '../RIPL-2/levels/'//finp, STATUS = 'OLD',
     &        ERR = 400)
      ELSE
         REWIND 13
      ENDIF
 100  READ(13, '(A5,6I5,2f12.6)', END = 300)chelem, iar, izr, nlvr,
     &     ngamr, nmax, itmp2, qn
C-----nmax is a number of levels that constitute a complete scheme as
C-----estimated by Belgya for RIPL-2.
C-----It is used, but a visual check with FITLEV is always recommended.
      IF(ia.NE.iar .OR. iz.NE.izr)THEN
         DO ilv = 1, nlvr + ngamr
            READ(13, '(A1)')dum
         ENDDO
         GOTO 100
      ELSE
C--------create file with levels (*.lev)
C--------NLV   number of levels with unique spin and parity
C--------NCOMP number of levels up to which the level scheme is estimated
C--------to be complete
C
         IF(.NOT.FILevel)THEN
            BACKSPACE(13)
            READ(13, '(A110)')ch_iuf
            WRITE(14, '(A110)')ch_iuf
         ENDIF
         IF(nlvr.NE.0)THEN
            IF(NLV(Nnuc).EQ.1 .AND. nmax.GT.1)NLV(Nnuc)
     &         = MIN(NDLV, nmax)
C-----------limit to max. of 40 levels if ENDF active
            IF(ENDf.NE.0.0D0)NLV(Nnuc) = MIN(NLV(Nnuc), 40)
            IF(NCOmp(Nnuc).EQ.1 .AND. nlvr.GT.1)NCOmp(Nnuc)
     &         = MIN(NDLV, nlvr)
            IF(.NOT.FILevel)THEN
               DO ilv = 1, nlvr + ngamr
                  READ(13, '(A110)')ch_iuf
                  WRITE(14, '(A110)')ch_iuf
               ENDDO
               DO ilv = 1, nlvr + ngamr
                  BACKSPACE(13)
               ENDDO
            ENDIF
C-----------levels for nucleus NNUC copied to file *.lev
            DO ilv = 1, NLV(Nnuc)
               READ(13, '(I3,1X,F10.6,1X,F5.1,I3,1X,E10.2,I3)')istart,
     &              ELV(ilv, Nnuc), XJLv(ilv, Nnuc), LVP(ilv, Nnuc),
     &              t12, ndbrlin
               IF(ELV(ilv, Nnuc).GT.qn)THEN
                  NLV(Nnuc) = ilv - 1
                  WRITE(6, '('' WARNING:'')')
                  WRITE(6, '('' WARNING: Element ='',A5,2x,2HZ=,I3)')
     &                  chelem, izr
                  WRITE(6,
     &'('' WARNING: Excited state '',I3,                             ''
     &is above neutron binding energy '',F6.3,                       ''
     &MeV'')')ilv, qn
                  WRITE(6, '('' WARNING: Number of levels set to '',I3)'
     &                  )NLV(Nnuc)
                  GOTO 200
               ENDIF
               IF(ilv.NE.1)THEN
                  IF(ELV(ilv, Nnuc).EQ.0.)THEN
                     WRITE(6, '('' WARNING:'')')
                     WRITE(6, '('' WARNING: Element ='',A5,2x,2HZ=,I3)')
     &                     chelem, izr
                     WRITE(6,
     &'('' WARNING: excited state '',I3,                             ''
     &has got zero excitation energy'')')ilv
                  ENDIF
                  IF(ilv.EQ.1 .AND. ELV(ilv, Nnuc).GT.4.)THEN
                     WRITE(6, '('' WARNING:'')')
                     WRITE(6, '('' WARNING: Element ='',A5,2x,2HZ=,I3)')
     &                     chelem, izr
                     WRITE(6,
     &'('' WARNING: excited state No.'',I3,                          ''
     &has energy of '',F6.3,'' MeV'')')ilv, ELV(ilv, Nnuc)
                  ENDIF
                  IF(ndbrlin.GT.NDBR)THEN
                     WRITE(6, '('' WARNING:'')')
                     WRITE(6, '('' WARNING: Element ='',A5,2x,2HZ=,I3)')
     &                     chelem, izr
                     WRITE(6,
     &'('' WARNING: too many gamma decays ='',                       I3)
     &')ndbrlin
                     WRITE(6,
     &'('' WARNING: Dimension allows for ='',                        I3)
     &')NDBR
                     WRITE(6, '('' WARNING: some gammas discarded'')')
                  ENDIF
C----------------clean BR matrix
                  DO nbr = 1, NDBR
                     BR(ilv, nbr, 1, Nnuc) = 0.
                     BR(ilv, nbr, 2, Nnuc) = 0.
                     BR(ilv, nbr, 3, Nnuc) = 0.
                  ENDDO
                  ndb = MIN(ndbrlin, NDBR)
                  sum = 0.0
                  isum = 0
                  DO nbr = 1, ndb
                     READ(13, '(39X,I4,1X,F10.3,3(1X,E10.3))')ifinal,
     &                    egamma, pgamma, pelm, xicc
C--------------------only gamma decay is considered up to now
                     IF(pelm.GT.0.)THEN
                        sum = sum + pelm
                        isum = isum + 1
                        BR(ilv, isum, 1, Nnuc) = ifinal !final level #
                        BR(ilv, isum, 2, Nnuc) = pelm   !branching
                        BR(ilv, isum, 3, Nnuc) = xicc   !int. convertion coeff.
                     ENDIF
                  ENDDO
                  IF(sum.NE.1.D0 .AND. sum.NE.0.D0)THEN
                     sum = 1.D0/sum
                     DO nbr = 1, isum
                        BR(ilv, nbr, 2, Nnuc) = BR(ilv, nbr, 2, Nnuc)
     &                     *sum
                     ENDDO
                  ENDIF
               ENDIF
            ENDDO
         ENDIF
      ENDIF
 200  IF(.NOT.FILevel)CLOSE(13)
      RETURN
 300  WRITE(6,
     &'('' WARNING: levels for nucleus A='',I3,'' Z='',I3,
     &'' not found in the RIPL database'')')ia, iz
      IF(.NOT.FILevel)CLOSE(13)
      RETURN
 400  WRITE(6, '('' WARNING: RIPL levels database not found '')')
      IF(.NOT.FILevel)CLOSE(13)
      END
C*==prinput.spg  processed by SPAG 6.20Rc at 12:12 on  7 Jul 2004
C
C
C
      SUBROUTINE PRINPUT
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:iou*
Ccc   *                      P R I N P U T                               *
Ccc   *                                                                  *
Ccc   *  Prints input parameters                                         *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * input: none                                                      *
Ccc   *                                                                  *
Ccc   * output: none                                                     *
Ccc   *                                                                  *
Ccc   * calls:none                                                       *
Ccc   *                                                                  *
Ccc   * author: M.Herman                                                 *
Ccc   * date:   11.Jan.1996                                              *
Ccc   * revision:1    by:M.Herman                 on:4.12.97             *
Ccc   * Storing the levels on file *.lev added                           *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C Local variables
C
      INTEGER i, j
      INTEGER IFIX
      REAL SNGL
      WRITE(6, *)
      WRITE(6, *)
      WRITE(6, 99008)
      WRITE(6, *)
      WRITE(6, 99001)(IFIX(SNGL(AEJc(i))), SYMbe(i), i = 1, NEJcm)
99001 FORMAT('    Nucleus   ', 12(6X, I2, A2))
      WRITE(6, *)
      DO i = 1, NNUcd
         WRITE(6, 99002)IFIX(SNGL(Z(i))), SYMb(i), IFIX(SNGL(A(i))),
     &                  (Q(j, i), j = 1, NEJcm)
99002    FORMAT(1X, I3, '-', A2, '-', I3, 4X, 12F10.3)
      ENDDO
      IF(ENDf.NE.0.0D0)THEN
         WRITE(12, *)
         WRITE(12, 99008)
         WRITE(12, *)
         WRITE(12, 99003)(IFIX(SNGL(AEJc(i))), SYMbe(i), i = 1, NEJcm)
99003    FORMAT('    Nucleus   ', 12(6X, I2, A2))
         WRITE(12, *)
         DO i = 1, NNUcd
            WRITE(12, 99004)IFIX(SNGL(Z(i))), SYMb(i), IFIX(SNGL(A(i))),
     &                      (Q(j, i), j = 1, NEJcm)
99004       FORMAT(1X, I3, '-', A2, '-', I3, 4X, 12F10.3)
         ENDDO
      ENDIF
      WRITE(6, *)
      WRITE(6, *)
      WRITE(6, 99005)
99005 FORMAT('    Nucleus   ', 6X, 'Shell Corr.  Deform.',
     &       '  Fiss. barr.')
      WRITE(6, 99006)
99006 FORMAT('              ', 6X, '  (J=0)       (J=0)    ',
     &       '    (J=0)')
      WRITE(6, *)
      DO i = 1, NNUct
         IF(EMAx(i).NE.0.0D0)WRITE(6, 99007)IFIX(SNGL(Z(i))), SYMb(i),
     &                             IFIX(SNGL(A(i))), SHC(i), DEF(1, i),
     &                             FISb(1, i)
99007    FORMAT(1X, I3, '-', A2, '-', I3, 4X, 10F12.3)
      ENDDO

      WRITE(6, *)
      IF(KTRompcc.GT.0 .AND. DIRect.GT.0) WRITE(6, *)
     &  ' inelastic o. m. parameters: RIPL catalog number ', KTRompcc
      WRITE(6, *) ' neutron   o. m. parameters: RIPL catalog number '
     &                 , KTRlom(1, 1)
      WRITE(6, *) ' proton    o. m. parameters: RIPL catalog number '
     &                 , KTRlom(2, 1)
      WRITE(6, *) ' alpha     o. m. parameters: RIPL catalog number '
     &                 , KTRlom(3, 1)
      IF(NEMc.GT.0)
     &WRITE(6, *) ' cluster o. m. parameters: RIPL catalog number '
     &              , KTRlom(NDEJC, 1)
C
      WRITE(6, *)
99008 FORMAT(10X, 'B i n d i n g    e n e r g i e s')
      END
C*==ptlevre.spg  processed by SPAG 6.20Rc at 12:12 on  7 Jul 2004
C
C
C
      SUBROUTINE PTLEVRE(Ia, Iz, Gspin, Gspar, E2p, E3m)
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:ipu*
Ccc   *                      P T L E V R E                               *
Ccc   *                                                                  *
Ccc   *  Reads from RIPL-2 OM-DEFORMATIONS.DAT energies of collective 2+ *
Ccc   *  and 3- states (Raman & Kibedi values used). If experimental     *
Ccc   *  is not available, then it will try to find them out in the file *
Ccc   *  file with discrete levels. In any case will take from this file *
Ccc   *  the ground state spin and parity.                               *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * input:IA   - A of the nucleus                                    *
Ccc   *       IZ   - Z of the nucleus                                    *
Ccc   *                                                                  *
Ccc   * output:GSPIN- ground state spin                                  *
Ccc   *        GSPAR- ground state parity (1 or -1)                      *
Ccc   *        E2P  - energy of the first 2+ level                       *
Ccc   *        E3M  - energy of the first 3- level                       *
Ccc   *                                                                  *
Ccc   * calls:none                                                       *
Ccc   *                                                                  *
Ccc   * author: M.Herman                                                 *
Ccc   * date:   11.Jan.1996                                              *
Ccc   * revision:1    by:M.Herman                 on:4.12.97             *
Ccc   * Storing the levels on file *.lev added                           *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C Dummy arguments
C
      DOUBLE PRECISION E2p, E3m, Gspin
      INTEGER Gspar, Ia, Iz
C
C Local variables
C
      CHARACTER*110 ch_iuf
      CHARACTER*3 ctmp3
      CHARACTER*5 chelem
      CHARACTER*9 finp
      DOUBLE PRECISION dum, DEF, elvr, xjlvr
      INTEGER iar, ilv, izr, nlvr, lvpr
      INTEGER nztmp, natmp, iptmp
      DOUBLE PRECISION etmp, jtmp, betatmp
      CHARACTER*6 reftmp
C
      E2p = 0.D0
      E3m = 0.D0
C     Avoiding searching of collective levels of the incident particle
      IF(Ia.EQ.AEJc(0))GOTO 300
C-----First try to find 2+ and 3- states in the RIPL om-deformations file
      OPEN(47, FILE = '../RIPL-2/optical/om-data/om-deformations.dat',
     &     STATUS = 'old', ERR = 100)
      READ(47, '(///)')
                       ! Skipping first 4 title lines
      DO i = 1, 1700
C        README file format (2i4,1x,a2,1x,f10.6,1x,f4.1,i3,i2,1x,f10.6,2x,a13)
         READ(47, '(2I4,4x,f10.6,1x,f4.1,i3,3x,f10.6,2x,a6)', END = 200,
     &        ERR = 200)nztmp, natmp, etmp, jtmp, iptmp, betatmp, reftmp
         IF(nztmp.EQ.Iz .AND. natmp.EQ.Ia .AND. jtmp.EQ.2.D0 .AND.
     &      iptmp.EQ. + 1 .AND. reftmp.EQ.'Raman2')E2p = etmp
         IF(nztmp.EQ.Iz .AND. natmp.EQ.Ia .AND. jtmp.EQ.3.D0 .AND.
     &      iptmp.EQ. - 1 .AND. reftmp.EQ.'Kibedi')E3m = etmp
      ENDDO
      GOTO 200
 100  WRITE(6, *)' WARNING: ',
     &   '../RIPL-2/optical/om-data/om-deformations.dat file not found '
      WRITE(6, *)' WARNING: ',
     &'E(2+) and E(3-) will be selected from the available target level
     &scheme'
      GOTO 300
 200  CLOSE(47)
C
C-----If missing in the RIPL om-deformations file try discrete levels file
C-----constructing input and filenames
 300  IF(.NOT.FILevel)THEN
         WRITE(ctmp3, '(I3.3)')Iz
         finp = 'z'//ctmp3//'.dat'
         OPEN(13, FILE = '../RIPL-2/levels/'//finp, STATUS = 'OLD',
     &        ERR = 500)
      ELSE
         REWIND(13)
      ENDIF
 400  READ(13, '(A5,6I5,2f12.6)', END = 500)chelem, iar, izr, nlvr,
     &     ngamr, nmax, itmp2
      IF(Ia.NE.iar .OR. Iz.NE.izr)THEN
         DO ilv = 1, nlvr + ngamr
            READ(13, '(A1)')dum
         ENDDO
         GOTO 400
      ELSE
C--------create file with levels xxx.lev
         IF(.NOT.FILevel)THEN
            BACKSPACE(13)
            READ(13, '(A110)')ch_iuf
            WRITE(14, '(A110)')ch_iuf
            DO ilv = 1, nlvr + ngamr
               READ(13, '(A110)')ch_iuf
               WRITE(14, '(A110)')ch_iuf
            ENDDO
            DO ilv = 1, nlvr + ngamr
               BACKSPACE(13)
            ENDDO
         ENDIF
C--------levels for nucleus NNUC copied to file xxx.lev
         DO ilv = 1, nlvr
            READ(13, '(I3,1X,F10.6,1X,F5.1,I3,1X,E10.2,I3)')ilvr, elvr,
     &           xjlvr, lvpr, t12, ndbrlin
            IF(ilv.EQ.1)THEN
               Gspin = xjlvr
               Gspar = lvpr
            ENDIF
C-----------skipping levels with unknown spin or parity
            IF(ilv.LE.nmax)THEN
               IF(E2p.EQ.0.0D0 .AND. xjlvr.EQ.2.D0 .AND. lvpr.EQ.1)
     &            E2p = elvr
               IF(E3m.EQ.0.0D0 .AND. xjlvr.EQ.3.D0 .AND. lvpr.EQ.( - 1))
     &            E3m = elvr
            ENDIF
         ENDDO
      ENDIF
      IF(.NOT.FILevel)CLOSE(13)
      RETURN
 500  Gspin = 0.
      IF(Ia.NE.2*(Ia/2))Gspin = 0.5
      Gspar = 1
      WRITE(6,
     &'('' LEVELS FOR NUCLEUS A='',I3,'' Z='',I3,'' NOT FOUND IN THE FIL
     &E'')')Ia, Iz
      WRITE(6,
     & '('' JUST TO BE SURE I SET G.S. PARITY TO + AND SPIN TO:'',F5.1)'
     & )Gspin
      IF(.NOT.FILevel)CLOSE(13)
      END
C*==ptlevset.spg  processed by SPAG 6.20Rc at 12:12 on  7 Jul 2004
C
C
C
      SUBROUTINE PTLEVSET(Ar, Zr, Gspin, Gspar, E2p, E3m)
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:ipu*
Ccc   *                      P T L E V S E T                             *
Ccc   *                                                                  *
Ccc   *  Sets ground state spin and parity and first 2+ and 3- levels'   *
Ccc   *  energies (latter for CCFUS and TRISTAN). For even-even nuclei   *
Ccc   *  just calls PTLEVRE. For other calls PTLEVRE for neighbouring    *
Ccc   *  nuclei and take average for 2+ and 3- energies.                 *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * input:AR   - A of the nucleus                                    *
Ccc   *       ZR   - Z of the nucleus                                    *
Ccc   *                                                                  *
Ccc   * output:GSPIN- ground state spin                                  *
Ccc   *        GSPAR- ground state parity (1 or -1)                      *
Ccc   *        E2P  - energy of the first 2+ level                       *
Ccc   *        E3M  - energy of the first 3- level                       *
Ccc   *                                                                  *
Ccc   * calls:PTLEVRE                                                    *
Ccc   *                                                                  *
Ccc   * author: M.Herman                                                 *
Ccc   * date:   11.Jan.1996                                              *
Ccc   * revision:     by:                         on:                    *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
C
C Dummy arguments
C
      DOUBLE PRECISION Ar, E2p, E3m, Gspin, Zr
      INTEGER Gspar
C
C Local variables
C
      DOUBLE PRECISION dum1, e2pd, e2pu, e3md, e3mu
      REAL FLOAT
      INTEGER ia, ium2, iz, izcor, n, ncor
      INTEGER INT
      ia = INT(Ar)
      iz = INT(Zr)
      n = ia - iz
      ncor = 0
      izcor = 0
C-----Plujko_new (define Gspin, Gspar, E2p, E3m for gamma)
      IF(ia.EQ.0)THEN
C        WRITE(*,*)'Input gamma channel  P T L E V S E T  '
         E2p = 0.D0
         E3m = 0.D0
         Gspin = 1
         Gspar = 1
         RETURN
      ENDIF
C-----Plujko_new (END define Gspin, Gspar, E2p, E3m for gamma)
C
C
      IF(FLOAT(n/2).NE.FLOAT(n)/2.0)ncor = 1
      IF(FLOAT(iz/2).NE.FLOAT(iz)/2.0)izcor = 1
      IF(ia.LE.4)THEN
         ncor = 0
         izcor = 0
      ENDIF
      IF(ncor.EQ.1 .OR. izcor.EQ.1)
     &   CALL PTLEVRE(ia - ncor - izcor, iz - izcor, dum1, ium2, e2pd,
     &   e3md)
      CALL PTLEVRE(ia, iz, Gspin, Gspar, E2p, E3m)
      IF(ncor.EQ.1 .OR. izcor.EQ.1)THEN
         CALL PTLEVRE(ia + ncor + izcor, iz + izcor, dum1, ium2, e2pu,
     &                e3mu)
         E2p = (e2pd + e2pu)/2.0
         E3m = (e3md + e3mu)/2.0
         IF(e2pd.EQ.0D0)E2p = e2pu
         IF(e2pu.EQ.0D0)E2p = e2pd
         IF(e3md.EQ.0D0)E3m = e3mu
         IF(e3mu.EQ.0D0)E3m = e3md
      ENDIF
      END
C*==readin.spg  processed by SPAG 6.20Rc at 12:12 on  7 Jul 2004
C
C
C
      SUBROUTINE READIN
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:iou*
Ccc   *                         R E A D I N                              *
Ccc   *                                                                  *
Ccc   *     Reads input variables from FILE5 and changes accordingly     *
Ccc   *     default values. Each record corresponds to a single variable *
Ccc   *     and starts with a key-word which specifies a variable (see   *
Ccc   *     list below). The keyword is followed by the variable value   *
Ccc   *     and 4 integers used eventually to specify indexes if the     *
Ccc   *     value for the array element is entered - FORMAT(A6,G10.5,3I3)*
Ccc   *     Example of the input record for scalar variable:             *
Ccc   *     QFIS   0.92                                                  *
Ccc   *                                                                  *
Ccc   * input:none                                                       *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * output:none                                                      *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * calls:none                                                       *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * author: M.Herman                                                 *
Ccc   * date:   xx.Jun.1994                                              *
Ccc   * revision:1    by:M.Herman                 on:11.Oct.1997         *
Ccc   * Tristan input added                                              *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C COMMON variables
C
      DOUBLE PRECISION ALSin, CNOrin(22), EFItin(22), GAPin(2), HOMin,
     &                 WIDexin
      COMMON /TRINP / WIDexin, GAPin, HOMin, ALSin, EFItin, CNOrin
C
C Local variables
C
      REAL FLOAT
      INTEGER i, i1, i2, i3, i4, ieof, iloc, ipoten, izar, ki, nnuc
      INTEGER INT
      CHARACTER*6 name
      CHARACTER*40 fstring
      DOUBLE PRECISION val
C-----initialization of TRISTAN input parameters
      WIDexin = 0.2
      GAPin(1) = 0.
      GAPin(2) = 0.
      HOMin = 0.0
      ALSin = 1.5
      DO i = 1, 22
         CNOrin(i) = 1.0
      ENDDO
C-----initialization of TRISTAN input parameters  *** done ***
99001 FORMAT(1X, 80('_'))
      WRITE(6, *)'                        ____________________________'
      WRITE(6, *)'                       |                            |'
      WRITE(6, *)'                       |  E M P I R E  -  2.19.b22  |'
      WRITE(6, *)'                       |                            |'
      WRITE(6, *)'                       |  marching towards LODI ;-) |'
      WRITE(6, *)'                       |____________________________|'
      WRITE(6, *)' '
      WRITE(6, *)' '
      WRITE(6, *)'Following options/parameters have been used'
      WRITE(6, *)'-------------------------------------------'
      WRITE(6, *)' '
 100  READ(5, '(A1)')name(1:1)
      IF(name(1:1).NE.'*' .AND. name(1:1).NE.'#' .AND. name(1:1).NE.'!')
     &   THEN
         BACKSPACE(5)
         READ(5, '(A6,G10.5,4I5)')name, val, i1, i2, i3, i4
         IF(name.EQ.'GO    ')THEN
C           IF(DOMP) THEN
C           WRITE(6, *)'Dispersive optical model is used'
C           Compressional form-factor is set off
C           ICOmpff = 0
C           ENDIF
C-----------Print some final input options
            IF(DIRect.eq.0) then
            ECUtColl = 0.
            SCUtColl = 0
            ENDIF
            IF(KEY_shape.EQ.0)WRITE(6,
     &         '('' E1 strength function as in EMPIRE v2.18 (EGLO)'')')
            IF(KEY_shape.EQ.1)WRITE(6,
     &         '('' E1 strength function set to MLO1'')')
            IF(KEY_shape.EQ.2)WRITE(6,
     &         '('' E1 strength function set to MLO2'')')
            IF(KEY_shape.EQ.3)WRITE(6,
     &         '('' E1 strength function set to MLO3'')')
            IF(KEY_shape.EQ.4)WRITE(6,
     &         '('' E1 strength function set to EGLO'')')
            IF(KEY_shape.EQ.5)WRITE(6,
     &         '('' E1 strength function set to GFL'')')
            IF(KEY_shape.EQ.6)WRITE(6,
     &         '('' E1 strength shape function set to SLO'')')
            IF(KEY_gdrgfl.EQ.0)WRITE(6,
     &         '('' GDR parameters from Messina systematics'')')
            IF(KEY_gdrgfl.NE.0)WRITE(6,
     &         '('' GDR parameters from RIPL-2/Plujko systematics'')')
            WRITE(6, *)' '
            IF(OMParf .OR. OMPar_riplf .OR. OMParfcc)THEN
               WRITE(6, *)'Existing, case specific, o.m.p. files: '
               WRITE(6, *)'-------------------------------------'
            ENDIF
            IF(OMParf)WRITE(6,
     &'('' Input file OMPAR.INT with internal optical model'',
     &'' parameters '')')
            IF(OMPar_riplf)WRITE(6,
     &'('' Input file OMPAR.RIPL with RIPL optical model'',
     &'' parameters '')')
            IF(OMParfcc .AND. (DIRect.EQ.1 .OR. DIRect.EQ.3))WRITE(6,
     &'('' Input file OMPAR.DIR with optical model'',
     &'' parameters to be used by ECIS '')')
            IF(DIRect.EQ.0 .AND. KTRompcc.NE.0)WRITE(6,
     &'(1X,/,         '' WARNING: No direct calculations have been selec
     &ted'',/,        '' WARNING: but DIRPOT keyword is specified.'',/,
     &                '' WARNING: Set direct keyword in the input file t
     &o a nonzero'',/,'' WARNING: value if you want to include direct co
     &ntribution.'')')
            WRITE(6, *)' '
C-----------Printout of some final input options   *** done ***
            RETURN
         ENDIF
C--------DEGAS input
         IF(name.EQ.'DEGAS ')THEN
            DEGa = val
            IF(val.GT.0) WRITE(6,
     &'('' Exciton model calculations with code DEGAS '')')
            GOTO 100
         ENDIF
         IF(name.EQ.'GDIVP ')THEN
            GDIvp = val
            WRITE(6,
     &'('' Proton s.p.l. density set to A/'',f5.2,'' in'',
     &'' code DEGAS '')')GDIvp
            GOTO 100
         ENDIF
C--------PCROSS input
         IF(name.EQ.'PCROSS')THEN
            IF(val.GT.0)THEN
               WRITE(6,
     &'('' Exciton model calculations with code PCROSS'',/,
     &  '' Cluster emission in terms of the Iwamoto-Harada model'')')
               IF(val.GT.1.05 .AND. val.LE.2.D0)MFPp = val
               WRITE(6,
     &'('' Mean free path parameter in PCROSS set to '',F4.1,
     &  '' (Default value: 1.3)'')') MFPp
               PEQc = 1.
            ELSE
               PEQc = 0.
            ENDIF
            GOTO 100
         ENDIF
C
C--------ECIS input
C
C--------In the following block one parameter -KTRompCC- is defined
C--------DIRECT is set to 1 if equal zero to allow for ECIS calc.
C
         IF(name.EQ.'DIRPOT')THEN
            IF(val.LT.0)THEN
               ki = 26
               ipoten = -INT(val)
C--------------searching in the RIPL database
               CALL FINDPOT(ki, ieof, ipoten)
               IF(ieof.EQ.0)THEN
                  val = -val
               ELSE
                  WRITE(6, *)'WARNING: Requested RIPL entry ', ipoten,
     &              ' for inelastic scattering not found'
                  GOTO 100
               ENDIF
            ELSE
               WRITE(6,
     &'('' Only RIPL OMP parameters are supported in EMPIRE 2.19'')')
               STOP
            ENDIF
            WRITE(6,
     &'('' Optical model parameters for ECIS calculation set to '',I4
     &)')INT(val)
            KTRompcc = INT(val)
            GOTO 100
         ENDIF
         IF(name.EQ.'DIRECT')THEN
            DIRect = val
            IF(DIRect.EQ.3)WRITE(6,
     &    '('' DWBA (ECIS) used for direct inelastic scattering'')')
            IF(DIRect.EQ.1 .OR. DIRect.EQ.2)WRITE(6,
     &      '('' Coupled Channels (ECIS) used for direct inelastic scatt
     &ering'')')
            IF(DIRect.EQ.2)WRITE(6,
     &      '('' Coupled Channels (ECIS) used for Tl calculations in ine
     &lastic channels'')')
            GOTO 100
         ENDIF
         IF(name.EQ.'EcDWBA')THEN
            ECUtColl = val
            JCUtColl = i1
            IF(JCUtColl.eq.0) JCUtColl=2
            WRITE(6,
     &       '('' Collective levels up to '',F5.1,'' MeV used in DWBA''
     &)' ) ECUtColl
            WRITE(6,
     &       '('' All levels with spin less or equal to '',I1,
     & '' considered in DWBA'')' ) JCUtColl
            GOTO 100
         ENDIF
C
C--------ECIS input  *** done ***
C
         IF(name.EQ.'RELKIN')THEN
            IF(val.NE.0.)THEN
               RELkin = .TRUE.
               WRITE(6, '(1x,A)')'Relativistic kinematics used'
            ELSE
               WRITE(6, '(1x,A)')
     &                        'Non-relativistic kinematics used'
            ENDIF
            GOTO 100
         ENDIF
         IF(name.EQ.'BFUS  ')THEN
            BFUs = val
            WRITE(6, '('' Fusion barrier set to '',F7.2,'' MeV'')')BFUs
            GOTO 100
         ENDIF
C--------CCFUS input
         IF(name.EQ.'FCD   ')THEN
            FCD(i1) = val
            WRITE(6,
     &'('' FCD parameter for  n='',I2,'' collective level set to'',F6.3)
     &')i1, FCD(i1)
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'DV    ')THEN
            DV = val
            WRITE(6, '('' DV barrier parameter in CCFUS set to '',F6.3)'
     &            )DV
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'FCC   ')THEN
            FCC = val
            WRITE(6, '('' FCC parameter in CCFUS set to '',F6.3)')FCC
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'NSCC  ')THEN
            NSCc = val
            WRITE(6,
     &            '('' Number of coupled channels in CCFUS set to'',I3)'
     &            )NSCc
            GOTO 100
         ENDIF
         IF(name.EQ.'NACC  ')THEN
            NACc = val
            WRITE(6,
     &          '('' Number of additional coupled channels set to'',I3)'
     &          )NACc
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'BETCC ')THEN
            BETcc(i1) = val
            WRITE(6,
     &'('' Deformation of the n='',I2,'' collective level set to'',F6.3)
     &')i1, BETcc(i1)
            GOTO 100
         ENDIF
C-----
C-----Plujko_new
C-----   init GDR & GFL for GSA
         IF(name.EQ.'GDRGFL')THEN
            KEY_gdrgfl = val
            GOTO 100
         ENDIF
C        Key_GDRGFL = 0 -  GDR parameters from Messina systematics
C        Key_GDRGFL = 1 -  GDR parameters and other data determined by gdrgfldata.f
C-----Plujko_new(End)
C-----
         IF(name.EQ.'FLAM  ')THEN
            FLAm(i1) = val
            WRITE(6,
     &'('' Multipolar. of the n='',I2,'' collective level set to'',F6.3)
     &')i1, FLAm(i1)
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'QCC   ')THEN
            QCC(i1) = val
            IF(QCC(i1).GT.0.0D0)QCC(i1) = -QCC(i1)
            WRITE(6,
     &'('' Q-value     of the n='',I2,'' collective level set to'',F6.3)
     &')i1, QCC(i1)
            GOTO 100
         ENDIF
C--------CCFUS input  ** done ***
         IF(name.EQ.'QFIS  ')THEN
            QFIs = val
            WRITE(6,
     &          '('' Liquid drop fission barriers multiplied by'',F6.3)'
     &          )QFIs
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'LEVDEN')THEN
            ADIv = val
            IF(ADIv.EQ.0.0D0)WRITE(6,
     &         '('' EMPIRE-specific level densities were selected '')')
            IF(ADIv.EQ.1.0D0)WRITE(6,
     &     '('' EMPIRE-specific level densities with fitted parameters w
     &ere selected'')')
            IF(ADIv.GT.3.0D0)WRITE(6,
     &     '('' ROCOL level densities with a=A/''  ,F5.2,'' selected'')'
     &     )ADIv
            IF(ADIv.EQ.2.0D0)WRITE(6,
     &         '('' Gilbert-Cameron level densities were selected '')')
            IF(ADIv.EQ.3.0D0)WRITE(6,
     &         '('' Microscopic HFBCS level densities were selected'')')
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'BETAV ')THEN
            BETav = val
            WRITE(6,
     &          '('' Viscosity parameter set to'',F6.3,'' 10**21 1/s'')'
     &          )BETav
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'SHRJ  ')THEN
            SHRj = val
            WRITE(6,
     &'('' Shell correction to fission barrier brougth to 1/2 at spin ''
     &,F5.1)')SHRj
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'SHRD  ')THEN
            SHRd = val
            WRITE(6,
     &          '('' Diffusness of the shell correction damping'',F6.3)'
     &          )SHRd
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'FUSRED')THEN
            FUSred = val
            WRITE(6, '('' Fusion cross section was reduced by '',F6.3)')
     &            FUSred
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'CSREAD')THEN
            CSRead = val
            IF(CSRead.GT.0.0D0)WRITE(6,
     &       '('' Fusion cross section '',F8.3,'' mb read from input'')'
     &       )CSRead
            IF(CSRead.EQ.0.0D0)THEN
               WRITE(6,
     &'('' Bass option disabled CCFUS will be used instead          '')'
     &)
               CSRead = -2.0D0
            ENDIF
            IF(CSRead.EQ.( - 1.0D0))WRITE(6,
     &'('' Fusion cross section will be calculated according to distribu
     &ted barrier model'')')
            IF(CSRead.EQ.( - 2.0D0))WRITE(6,
     &'('' Fusion cross section will be calculated using coupled channel
     & approach'')')
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'SIG   ')THEN
            SIG = val
            IF(CSRead.EQ.( - 1.0D0))WRITE(6,
     &      '('' SIGMA in the distributed barrier model set to '',F6.3)'
     &      )SIG
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'TRUNC ')THEN
            TRUnc = val
            IF(CSRead.EQ.( - 1.0D0))WRITE(6,
     & '('' Truncation in the distributed barrier model set to '',F6.3)'
     & )TRUnc
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'EXPUSH')THEN
            EXPush = val
            IF(CSRead.EQ.( - 1.0D0))
     &         WRITE(6, '('' Extrapush set to '',F6.3)')EXPush
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'CRL   ')THEN
            CRL = val
            WRITE(6, '('' Critical l-value for fusion set to '',F6.2)')
     &            CRL
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'TRGLEV')THEN
            LEVtarg = val
            WRITE(6, '('' Target excited to the level #'',I2)')LEVtarg
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'DFUS  ')THEN
            DFUs = val
            WRITE(6,
     &'('' Difusness in the transmission coefficients for fusion set to
     &'',F5.2)')DFUs
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'TEMP0 ')THEN
            TEMp0 = val
            WRITE(6,
     &'('' Temperature at which shell correction fade-out starts set to
     &'',F6.3)')TEMp0
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'SHRT  ')THEN
            SHRt = val
            WRITE(6,
     &'('' Parameter in the teperature shell correction fade-out set to
     &'',F6.3)')SHRt
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'IOUT  ')THEN
            IOUt = val
            WRITE(6,
     &            '('' Main calculations output control set to '',I2)')
     &            IOUt
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'NOUT  ')THEN
            NOUt = val
            WRITE(6, '('' MSC calculation output control set to '',I2)')
     &            NOUt
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'XNI   ')THEN
            XNI = val
            WRITE(6, '('' Initial exciton number set to '',F4.1)')XNI
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'TORY  ')THEN
            TORy = val
            WRITE(6,
     &'(''(n-p)/(n-n) interaction strength ratio set to '',F5.2)')TORy
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'EX1   ')THEN
            EX1 = val
            WRITE(6,
     &    '('' Initial nuber of excitons being neutrons set to '',F6.3)'
     &    )EX1
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'EX2   ')THEN
            EX2 = val
            WRITE(6,
     &    '('' Initial nuber of excitons being protons set to  '',F6.3)'
     &    )EX2
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'GST   ')THEN
            GST = val
            IF(GST.EQ.1.0D0)WRITE(6,
     &        '('' Gamma emission in MSC allowed'')')
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'STMRO ')THEN
            STMro = val
            IF(STMro.EQ.1.0D0)WRITE(6,
     &                 '('' Microscopic p-h state densities selected'')'
     &                 )
            IF(STMro.EQ.0.0D0)WRITE(6,
     &                 '('' Closed form p-h state densities selected'')'
     &                 )
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'GDIV  ')THEN
            GDIv = val
            WRITE(6,
     &'('' Single particle level density in MSC set to A/'',F5.2)')GDIv
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'D1FRA ')THEN
            D1Fra = val
            WRITE(6, '('' Spreading to total GDR width set to '',F5.3)')
     &            D1Fra
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'LTURBO')THEN
            LTUrbo = val
            TURbo = FLOAT(LTUrbo)
            WRITE(6, '('' Step in the angular momentum set to '',I2)')
     &            LTUrbo
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'NEX   ')THEN
            NEXreq = val
            WRITE(6,
     &'('' Number of energy steps in the integration set to '',
     &I3)')NEXreq
            GOTO 100
         ENDIF
C-----
C--------Plujko_new
C------- Insert Key for module "gamma-strength-analytic.f",
C        Key_shape - key to specify the E1 strength shape    .
C
         IF(name.EQ.'GSTRFN')THEN
C           Key_shape = 0 --> old ver.2.18 variant for E1 stength-function fE1
C           Key_shape =1 --> fE1=MLO1
C           Key_shape =2 --> fE1=MLO2
C           Key_shape =3 --> fE1=MLO3
C           Key_shape =4 --> fE1=EGLO
C           Key_shape =5 --> fE1=GFL
C           Key_shape =6 --> fE1=SLO
            KEY_shape = val
C           WRITE(6, '(" E1 strength shape key set to ", I3 )')Key_shape
            GOTO 100
         ENDIF
C--------Plujko_new (End)
C-----
         IF(name.EQ.'GDRDYN')THEN
            GDRdyn = val
            IF(GDRdyn.NE.0.D0)WRITE(6,
     &         '('' Deformation dependent GDR selected'')')
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'EGDR1 ')THEN
            EGDr1 = val
            WRITE(6, '('' GDR first hump energy set to '',F5.2)')EGDr1
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'GGDR1 ')THEN
            GGDr1 = val
            WRITE(6, '('' GDR first hump width set to '',F5.2)')GGDr1
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'CSGDR1')THEN
            CSGdr1 = val
            WRITE(6, '('' GDR first hump cross section set to '',F6.2)')
     &      CSGdr1
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'EGDR2 ')THEN
            EGDr2 = val
            WRITE(6, '('' GDR second hump energy set to '',F5.2)')EGDr2
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'GGDR2 ')THEN
            GGDr2 = val
            WRITE(6, '('' GDR second hump width set to '',F5.2)')GGDr2
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'CSGDR2')THEN
            CSGdr2 = val
            WRITE(6,
     &  '('' GDR second hump cross section set to '',        F6.2)'
     &  )CSGdr2
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'MSD   ')THEN
            MSD = val
            IF(MSD.EQ.1)WRITE(6,
     &         '('' MSD calculations with ORION+TRISTAN were selected'')
     &            ')
            IF(MSD.EQ.2)WRITE(6,
     &         '('' MSD calculations will use previous ORION results'')'
     &         )
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'MSC   ')THEN
            MSC = val
            IF(MSC.NE.0)WRITE(6,
     &         '('' Heidelberg MSC calculations were selected'')')
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'HMS   ')THEN
            LHMs = val
            IF(LHMs.NE.0)WRITE(6,
     &         '('' HMS preequilibrium calculations were selected'')')
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'NHMS  ')THEN
            IF(val.GT.99.0D0)THEN
               NHMs = val
               WRITE(6, '('' Number of events in HMS set to '',I10)')
     &               NHMs
            ENDIF
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'CHMS  ')THEN
            IF(val.GT.0.0D0)THEN
               CHMs = val
               WRITE(6,
     &  '('' Default damp rate in HMS multiplied by '',F6.3)')CHMs
            ENDIF
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'HRTW  ')THEN
            LHRtw = val
            IF(LHRtw.NE.0)WRITE(6,
     &         '('' HRTW width fluctuation correction was selected'')')
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'GDRWP ')THEN
            DIToro = val
            WRITE(6,
     &  '('' Factor in energy increase of GDR width'',F7.5)')DIToro
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'GDRWA1')THEN
            GDRwa1 = val
            WRITE(6,
     &'('' GDR first hump width increased by '',F5.2,'' MeV'')')GDRwa1
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'GDRWA2')THEN
            GDRwa2 = val
            WRITE(6,
     &'('' GDR second hump width increased by '',F5.2,'' MeV'')')GDRwa2
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'GDRESH')THEN
            GDResh = val
            WRITE(6, '('' GDR position shifted by '',F6.3,'' MeV'')')
     &            GDResh
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'GDRSPL')THEN
            GDRspl = val
            WRITE(6,
     &'('' Splitting of GDR peaks increased by '',F6.3,         '' MEV''
     &)')GDRspl
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'GDRST1  ')THEN
            EWSr1 = val
            WRITE(6,
     &'('' Gamma strength multiplied by '',F6.3,'' for the first GDR hum
     &p'')')EWSr1
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'GDRST2  ')THEN
            EWSr2 = val
            WRITE(6,
     &'('' Gamma strength multiplied by '',F6.3,'' for the second GDR hu
     &mp'')')EWSr2
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'GDRWEI')THEN
            GDRweis = val
            WRITE(6,
     &'('' Gamma strength composed of '',F7.3''% GDR + ''
     &,F7.3,''% Weisskopf'')')GDRweis*100., (1. - GDRweis)*100.0
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'DEFPAR')THEN
            DEFpar = val
            WRITE(6,
     &      '('' Dynamic deformation multiplyer '',F7.3)')DEFpar
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'DEFGA ')THEN
            DEFga = val
            WRITE(6,
     &'('' Gaussian correction to deformation (amplitude)'',F7.3)
     &')DEFga
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'DEFGW ')THEN
            DEFgw = val
            WRITE(6,
     &'('' Gaussian correction to deformation (width)'',F7.3)
     &')DEFgw
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'DEFGP ')THEN
            DEFgp = val
            WRITE(6,
     &'('' Gaussian correction to deformation (position)'',F7.3)
     &')DEFgp
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'ENDF  ')THEN
            ENDf = val
            IF(ENDf.EQ.1.0D0)WRITE(6,
     &'('' Exclusive spectra representation selected '')')
            IF(ENDf.EQ.2.0D0)WRITE(6,
     &'('' Inclusive spectra representation selected '')')
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'OMPOT ')THEN
            IF(i1.LT.1 .OR. i1.GT.NEJcm)THEN
               WRITE(6,
     &               '('' EJECTILE IDENTIFICATION '',I2,'' UNKNOWN'')')
     &               i1
               WRITE(6, '('' OPTICAL MODEL  SETTING IGNORED'')')
               GOTO 100
            ENDIF
C-----
            IF(val.LT.0)THEN
               ki = 26
               ipoten = -INT(val)
C--------------Searching in the RIPL database for i1 catalog number
               CALL FINDPOT(ki, ieof, ipoten)
               IF(ieof.EQ.0)THEN
                  val = -val
               ELSE
                  WRITE(6, *)'Requested RIPL entry ', ipoten,
     &                       ' not found, using default choice'
                  GOTO 100
               ENDIF
               WRITE(6,
     &'('' Optical model parameters for ejectile '', I1,'' set to '', I4
     &)')i1, INT(val)
C              WRITE(6,
C    &'('' (will be used if not overwritten in the OMPAR.RIPL file)'')
C    &')
            ELSE
               WRITE(6,
     &'('' Only RIPL OMP parameters are supported in EMPIRE 2.19'')')
               STOP
            ENDIF
C-----
            DO i = 1, NDNUC
               KTRlom(i1, i) = INT(val)
            ENDDO
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'BNDG  ')THEN
            IF(i3.LE.0 .OR. i3.GT.NEJcm)THEN
               WRITE(6,
     &               '('' EJECTILE IDENTIFICATION '',I2,'' UNKNOWN'')')
     &               i3
               WRITE(6, '('' BINDING ENERGY SETTING IGNORED'')')
               GOTO 100
            ENDIF
            izar = i1*1000 + i2
            CALL WHERE(izar, nnuc, iloc)
            IF(iloc.EQ.1)THEN
               WRITE(6, '('' NUCLEUS '',I3,A2,'' NOT NEEDED'')')i2,
     &               SYMb(nnuc)
               WRITE(6, '('' BINDING ENERGY SETTING IGNORED'')')
               GOTO 100
            ENDIF
            Q(i3, nnuc) = val
            WRITE(6,
     & '('' Binding energy of '',A2,'' in '',I3,A2,'' set to ''  ,F6.3)'
     & )SYMbe(i3), i2, SYMb(nnuc), val
            IF(nnuc.EQ.1 .AND. IZAejc(0).EQ.IZAejc(i3))Q(0, 1) = val
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'JSTAB ')THEN
            izar = i1*1000 + i2
            IF(izar.EQ.0)THEN
               DO i = 1, NDNUC
                  JSTab(i) = val
               ENDDO
               IF(val.GT.0.0D0)WRITE(6,
     &    '('' Stability limit set to spin '',F6.1,'' for all nuclei'')'
     &    )val
               GOTO 100
            ENDIF
            CALL WHERE(izar, nnuc, iloc)
            IF(iloc.EQ.1)THEN
               WRITE(6, '('' NUCLEUS '',I3,A2,'' NOT NEEDED'')')i2,
     &               SYMb(nnuc)
               WRITE(6, '('' SETTING STABILITY LIMIT IGNORED'')')
               GOTO 100
            ENDIF
            JSTab(nnuc) = val
            WRITE(6,
     &            '('' Stability limit in '',I3,A2,'' set to '',F6.1)')
     &            i2, SYMb(nnuc), val
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'GCROA ')THEN
            izar = i1*1000 + i2
            IF(izar.EQ.0)THEN
               DO i = 1, NDNUC
                  ROPaa(i) = val
               ENDDO
               IF(val.GT.0.0D0)WRITE(6,
     &'('' L.d. a-parameter in all nuclei multiplied by '',F6.1)') val
               IF(val.EQ.0.0D0)WRITE(6,
     &      '('' L. d. a-parameter according to Ignatyuk systematics'')'
     &      )
               IF(val.EQ.( - 1.D0))WRITE(6,
     &        '('' L. d. a-parameter according to Arthur systematics'')'
     &        )
               IF(val.EQ.( - 2.D0))WRITE(6,
     &         '('' L. d. a-parameter according to Mebel systematics'')'
     &         )
               GOTO 100
            ENDIF
            CALL WHERE(izar, nnuc, iloc)
            IF(iloc.EQ.1)THEN
               WRITE(6, '('' NUCLEUS '',I3,A2,'' NOT NEEDED'')')i2,
     &               SYMb(nnuc)
               WRITE(6, '('' L.D. a-PARAMETER SETTING IGNORED'')')
               GOTO 100
            ENDIF
            ROPaa(nnuc) = val
            WRITE(6,
     &'('' L.d. a-parameter in '',I3,A2,'' multiplied by '',F6.1)'
     & )i2, SYMb(nnuc), val
            GOTO 100
         ENDIF
         IF(name.EQ.'ATILNO')THEN
            izar = i1*1000 + i2
            IF(izar.EQ.0)THEN
               DO i = 1, NDNUC
                  ATIlnor(i) = val
               ENDDO
               WRITE(6,
     &'('' L.d. parameter in all nuclei multiplied by '',F6.1)') val
               GOTO 100
            ENDIF
            CALL WHERE(izar, nnuc, iloc)
            IF(iloc.EQ.1)THEN
               WRITE(6, '('' NUCLEUS '',I3,A2,'' NOT NEEDED'')')i2,
     &               SYMb(nnuc)
               WRITE(6, '('' NORMALIZATION OF a-tilde IGNORED'')')
               GOTO 100
            ENDIF
            ATIlnor(nnuc) = val
            WRITE(6,
     &'('' L.d. a-parameter in '',I3,A2,'' multiplied by '',F6.1)'
     &)i2, SYMb(nnuc), val
            GOTO 100
         ENDIF
C
         IF(name.EQ.'GTILNO')THEN
            izar = i1*1000 + i2
            IF(izar.EQ.0)THEN
               DO i = 1, NDNUC
                  GTIlnor(i) = val
               ENDDO
               WRITE(6,
     &'('' Single particle L.D. parameter G multiplied by '',F6.1)') val
               GOTO 100
            ENDIF
            CALL WHERE(izar, nnuc, iloc)
            IF(iloc.EQ.1)THEN
               WRITE(6, '('' NUCLEUS '',I3,A2,'' NOT NEEDED'')')i2,
     &               SYMb(nnuc)
               WRITE(6, '('' NORMALIZATION OF G-tilde IGNORED'')')
               GOTO 100
            ENDIF
            GTIlnor(nnuc) = val
            WRITE(6,
     &'('' Single particle L.D. parameter G in '',I3,A2,
     &  '' multiplied by '',        F6.1)')i2, SYMb(nnuc), val
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'GCROUX')THEN
            izar = i1*1000 + i2
            CALL WHERE(izar, nnuc, iloc)
            IF(iloc.EQ.1)THEN
               WRITE(6, '('' NUCLEUS '',I3,A2,'' NOT NEEDED'')')i2,
     &               SYMb(nnuc)
               WRITE(6, '('' L.D. PARAMETER Ux SETTING IGNORED'')')
               GOTO 100
            ENDIF
            ROPar(2, nnuc) = val
            WRITE(6,
     & '('' L.d. parameter Ux  in '',I3,A2,'' set to ''          ,F6.3)'
     & )i2, SYMb(nnuc), val
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'GCROD ')THEN
            izar = i1*1000 + i2
            CALL WHERE(izar, nnuc, iloc)
            IF(iloc.EQ.1)THEN
               WRITE(6, '('' NUCLEUS '',I3,A2,'' NOT NEEDED'')')i2,
     &               SYMb(nnuc)
               WRITE(6, '('' PAIRING SHIFT SETTING IGNORED'')')
               GOTO 100
            ENDIF
            ROPar(3, nnuc) = val
            WRITE(6,
     & '('' Pairing shift in '',I3,A2,'' set to ''          ,F6.3)'
     & )i2, SYMb(nnuc), val
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'GCROE0')THEN
            izar = i1*1000 + i2
            CALL WHERE(izar, nnuc, iloc)
            IF(iloc.EQ.1)THEN
               WRITE(6, '('' NUCLEUS '',I3,A2,'' NOT NEEDED'')')i2,
     &               SYMb(nnuc)
               WRITE(6, '('' L.D. PARAMETER E0 SETTING IGNORED'')')
               GOTO 100
            ENDIF
            ROPar(4, nnuc) = val
            WRITE(6,
     & '('' L.d. parameter Eo in '',I3,A2,'' set to ''          ,F6.3)'
     & )i2, SYMb(nnuc), val
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'GCROT ')THEN
            izar = i1*1000 + i2
            CALL WHERE(izar, nnuc, iloc)
            IF(iloc.EQ.1)THEN
               WRITE(6, '('' NUCLEUS '',I3,A2,'' NOT NEEDED'')')i2,
     &               SYMb(nnuc)
               WRITE(6, '('' L.D. PARAMETER T  SETTING IGNORED'')')
               GOTO 100
            ENDIF
            ROPar(5, nnuc) = val
            WRITE(6,
     & '('' L.d. parameter T   in '',I3,A2,'' set to ''          ,F6.3)'
     & )i2, SYMb(nnuc), val
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'FITLEV')THEN
            FITlev = val
            IF(FITlev.GT.0.0D0)WRITE(6,
     &                 '('' Cumulative plots of levels will be done '')'
     &                 )
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'FITOMP')THEN
            FITomp = val
            IF(FITomp.GT.0.0D0)WRITE(6,
     &'('' OM parameter adjustment selected,'',
     & '' (will reset several options'')')
            GOTO 100
         ENDIF
C--------Tuning factors
         IF(name.EQ.'TUNE  ')THEN
            izar = i1*1000 + i2
            CALL WHERE(izar, nnuc, iloc)
            IF(iloc.EQ.1)THEN
               WRITE(6, '('' NUCLEUS '',I3,A2,'' NOT NEEDED'')')i2,
     &               SYMb(nnuc)
               WRITE(6, '('' TUNING IGNORED'')')
               GOTO 100
            ENDIF
            TUNe(i3, nnuc) = val
            WRITE(6,
     &'('' Emission width of ejectile '',I1,'' from '',I3,A2,
     &         '' multiplied by '',F6.3)')i3, i2, SYMb(nnuc), val
            GOTO 100
         ENDIF
C--------input for TRISTAN (MSD)
         IF(name.EQ.'WIDEX ')THEN
            WIDexin = val
            IF(WIDexin.GT.0.0D0)WRITE(6,
     &'('' Experimental energy resolution in MSD set to'',F6.3,'' MeV'')
     &')WIDexin
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'COMPFF')THEN
            ICOmpff = val
            IF(ICOmpff.GT.0)THEN
               WRITE(6,
     &'('' Compressional l=0 form factor''
     &,'' used in MSD calculations'')')
            ELSE
               WRITE(6,
     &'('' Surface l=0 form factor''
     &,'' used in MSD calculations'')')
            ENDIF
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'GAPP  ')THEN
            GAPin(2) = val
            IF(GAPin(2).GT.0.0D0)WRITE(6,
     &'('' Proton  pairing gap for target in MSD set to'',F6.3,'' MeV'')
     &')GAPin(2)
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'GAPN  ')THEN
            GAPin(1) = val
            IF(GAPin(1).GT.0.0D0)WRITE(6,
     &'('' Neutron pairing gap for target in MSD set to'',F6.3,'' MeV'')
     &')GAPin(1)
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'HOMEGA')THEN
            HOMin = val
            IF(HOMin.GT.0.0D0)WRITE(6,
     &'('' Harmonic oscillator energy (hbar*omega) for MSD'',   F6.3,''
     &MeV'')')HOMin
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'ALS   ')THEN
            ALSin = val
            IF(ALSin.GT.0.0D0)WRITE(6,
     &'('' l*s coupling strength in harmonic oscill. (MSD)'',   F6.3,''
     &MeV'')')ALSin
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'EFIT  ')THEN
            EFItin(i1 + 1) = val
            IF(EFItin(i1 + 1).GT.0.0D0)WRITE(6,
     &'('' Field strength of multipolarity'',I2,'' fitted to the level a
     &t '',F6.3,'' MeV'')')i1, EFItin(i1 + 1)
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'RESNOR')THEN
            CNOrin(i1 + 1) = val
            WRITE(6,
     &'('' Response function for multipolarity'',I3,'' normalized by fac
     &tor '',F6.3)')i1, CNOrin(i1 + 1)
            GOTO 100
         ENDIF
C--------TRISTAN (MSD) input **** done ****
         IF(name.EQ.'NIXSH ')THEN
            SHNix = val
            IF(SHNix.NE.0.0D0)WRITE(6,
     &                '('' Shell corrections according to Nix-Moller'')'
     &                )
            IF(SHNix.EQ.0.0D0)WRITE(6,
     &           '('' Shell corrections according to Myers-Swiatecki'')'
     &           )
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'GCASC ')THEN
            GCAsc = val
            IF(GCAsc.NE.0.0D0)WRITE(6,
     &              '('' Full gamma cascade in the first CN selected'')'
     &              )
            IF(GCAsc.EQ.0.0D0)WRITE(6,
     &             '('' Only primary gammas in the first CN selected'')'
     &             )
            GOTO 100
         ENDIF
C-----
C-----Plujko_new : start of data input
C-----
         IF(name.EQ.'E1    ')THEN
            IF(val.GT.0.0D+0) THEN
               WRITE(6, '('' E1 photo-absorption selected'')')
            ELSE
               WRITE(6, '('' E1 photo-absorption blocked'')')
            ENDIF
            IGE1 = val
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'M1    ')THEN
            IF(val.GT.0.0D+0) THEN
               WRITE(6, '('' M1 photo-absorption selected'')')
            ELSE
               WRITE(6, '('' M1 photo-absorption blocked'')')
            ENDIF
            IGM1 = val
            GOTO 100
         ENDIF
C-----
         IF(name.EQ.'E2    ')THEN
            IF(val.GT.0.0D+0) THEN
               WRITE(6, '('' E2 photo-absorption selected'')')
            ELSE
               WRITE(6, '('' E2 photo-absorption blocked'')')
            ENDIF
            IGE2 = val
            GOTO 100
         ENDIF
C--------Plujko_new : end of data input
C-----
C-----
C-----Carlson_new : start of data input
C-----
         IF(name.EQ.'QD    ')THEN
            IF(IGE1.EQ.0) THEN
               WRITE(6, '('' Quasideuteron photo-absorption is '',
     &    '' suppressed since E1 photo-absorption is blocked!'')')
              ELSE
               Lqdfac=val
               WRITE(6,
     &'('' Quasideuteron photoabsorption cross section'',
     &  '' normalized by a factor '',F6.3)') Lqdfac
              ENDIF
            GOTO 100
         ENDIF
C--------Carlson_new : end of data input
C-----
C-----
C        fisfis d --------------
C        checking for fission data in the optional input
         IF(name.EQ.'FISSHI')THEN
            izar = i1*1000 + i2
            IF(val.EQ.0) THEN
               fstring = 'advanced treatment of fission'
            ELSEIF(val.EQ.1) THEN
               fstring = 'fission over single-humped barrier'
            ELSEIF(val.EQ.2) THEN
               fstring = 'fission ignored'
            ELSE
               fstring = 'illegal value for FISSHI'
            ENDIF
            IF(izar.EQ.0)THEN
               DO nnuc = 1, NDNUC
                  FISshi(nnuc) = val
               ENDDO
               WRITE(6,*) 'For all nuclei: ', fstring
               GOTO 100
               ENDIF
            CALL WHERE(izar, nnuc, iloc)
            IF(iloc.EQ.1)THEN
               WRITE(6, '('' NUCLEUS '',I3,A2,'' NOT NEEDED'')')i2,
     &               SYMb(nnuc)
               WRITE(6, '('' FISSHI SETTING IGNORED'')')
               GOTO 100
            ENDIF
            FISshi(nnuc) = val
            WRITE(6,*) 'For ',i2,'-',SYMb(nnuc),' ', fstring
            GOTO 100
         ENDIF
C-------------------------------------------------------------------------
         IF(name.EQ.'FISMOD')THEN
            izar = i1*1000 + i2
            IF(izar.EQ.0)THEN
               DO nnuc = 1, NDNUC
                  FISmod(nnuc) = val
               ENDDO
               WRITE(6,
     &               '('' FISMOD  in all nuclei set to '',F6.3)'
     &               ) val
               GOTO 100
               ENDIF
            CALL WHERE(izar, nnuc, iloc)
            IF(iloc.EQ.1)THEN
               WRITE(6, '('' NUCLEUS '',I3,A2,'' NOT NEEDED'')')i2,
     &               SYMb(nnuc)
               WRITE(6, '('' FISMOD SETTING IGNORED'')')
               GOTO 100
            ENDIF
            FISmod(nnuc) = val
            WRITE(6,
     &            '('' FISMOD  in '',I3,A2,'' set to ''          ,F6.3)'
     &            )i2, SYMb(nnuc), val
            GOTO 100
         ENDIF
C-------------------------------------------------------------------------
         IF(name.EQ.'FISOPT')THEN
            izar = i1*1000 + i2
            IF(izar.EQ.0)THEN
               DO nnuc = 1, NDNUC
                  FISopt(nnuc) = val
               ENDDO
               WRITE(6,
     &               '('' FISOPT  in all nuclei set to '',F6.3)'
     &               ) val
               GOTO 100
               ENDIF
            CALL WHERE(izar, nnuc, iloc)
            IF(iloc.EQ.1)THEN
               WRITE(6, '('' NUCLEUS '',I3,A2,'' NOT NEEDED'')')i2,
     &               SYMb(nnuc)
               WRITE(6, '('' FISOPT SETTING IGNORED'')')
               GOTO 100
            ENDIF
            FISopt(nnuc) = val
            WRITE(6,
     &            '('' FISOPT  in '',I3,A2,'' set to ''          ,F6.3)'
     &            )i2, SYMb(nnuc), val
            GOTO 100
         ENDIF
C-------------------------------------------------------------------------
         IF(name.EQ.'FISBAR')THEN
            izar = i1*1000 + i2
            IF(izar.EQ.0)THEN
               DO nnuc = 1, NDNUC
                  FISbar(nnuc) = val
               ENDDO
               WRITE(6,
     &               '('' FISBAR  in all nuclei set to '',F6.3)'
     &               ) val
               GOTO 100
               ENDIF
            CALL WHERE(izar, nnuc, iloc)
            IF(iloc.EQ.1)THEN
               WRITE(6, '('' NUCLEUS '',I3,A2,'' NOT NEEDED'')')i2,
     &               SYMb(nnuc)
               WRITE(6, '('' FISBAR SETTING IGNORED'')')
               GOTO 100
            ENDIF
            FISbar(nnuc) = val
            WRITE(6,
     &            '('' FISBAR  in '',I3,A2,'' set to ''          ,F6.3)'
     &            )i2, SYMb(nnuc), val
            GOTO 100
         ENDIF
C-------------------------------------------------------------------------
         IF(name.EQ.'FISDEN')THEN
            izar = i1*1000 + i2
            IF(izar.EQ.0)THEN
               DO nnuc = 1, NDNUC
                  FISden(nnuc) = val
               ENDDO
               WRITE(6,
     &               '('' FISDEN  in all nuclei set to '',F6.3)'
     &               ) val
               GOTO 100
               ENDIF
            CALL WHERE(izar, nnuc, iloc)
            IF(iloc.EQ.1)THEN
               WRITE(6, '('' NUCLEUS '',I3,A2,'' NOT NEEDED'')')i2,
     &               SYMb(nnuc)
               WRITE(6, '('' FISDEN SETTING IGNORED'')')
               GOTO 100
            ENDIF
            FISden(nnuc) = val
            WRITE(6,
     &            '('' FISDEN  in '',I3,A2,'' set to ''          ,F6.3)'
     &            )i2, SYMb(nnuc), val
            GOTO 100
         ENDIF
C-------------------------------------------------------------------------
         IF(name.EQ.'FISDIS')THEN
            izar = i1*1000 + i2
            IF(izar.EQ.0)THEN
               DO nnuc = 1, NDNUC
                  FISdis(nnuc) = val
               ENDDO
               WRITE(6,
     &               '('' FISDIS  in all nuclei set to '',F6.3)'
     &               ) val
               GOTO 100
               ENDIF
            CALL WHERE(izar, nnuc, iloc)
            IF(iloc.EQ.1)THEN
               WRITE(6, '('' NUCLEUS '',I3,A2,'' NOT NEEDED'')')i2,
     &               SYMb(nnuc)
               WRITE(6, '('' FISDIS SETTING IGNORED'')')
               GOTO 100
            ENDIF
            FISdis(nnuc) = val
            WRITE(6,
     &            '('' FISDIS  in '',I3,A2,'' set to ''          ,F6.3)'
     &            )i2, SYMb(nnuc), val
            GOTO 100
         ENDIF
C-------------------------------------------------------------------------
C        fisfis u ----------------------------------------------------
         WRITE(6, '('' INVALID KEY: '',A6,'', DISPOSITION IGNORED'')')
     &         name
      ENDIF
      GOTO 100
      END
C*==readnix.spg  processed by SPAG 6.20Rc at 12:12 on  7 Jul 2004
C
C
C
      SUBROUTINE READNIX
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:iou*
Ccc   *                         R E A D N I X                            *
Ccc   *                                                                  *
Ccc   *     Reads nuclear deformations masses and shell-corrections      *
Ccc   *     from the RIPL-2 mass file mass-frdm95.dat                    *
Ccc   *                                                                  *
Ccc   * input:none                                                       *
Ccc   *                                                                  *
Ccc   * output:none                                                      *
Ccc   *                                                                  *
Ccc   * calls:where                                                      *
Ccc   *                                                                  *
Ccc   * author: M.Herman & R.Sturiale                                    *
Ccc   * date:   27.Sep.1996                                              *
Ccc   * revision:1    by: R.Capote               on:09.2004              *
Ccc   * RIPL-2 databse used                                              *
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C Local variables
C
      DOUBLE PRECISION xmassexp,xmassth, zmn, zmx
      DOUBLE PRECISION DMAX1
      INTEGER ii, iloc, nixa, nixz, nnuc
      DIMENSION izaf(NMAsse), excess(NMAsse)
      DIMENSION beta2x(NMAsse), emicx(NMAsse)

C     OPEN(UNIT = 27, STATUS = 'unknown', FILE = '../data/mass-hms.dat')
C     READ(27, '(///i7)')nmass
C     READ(27, '(5(i7,f11.6,f7.1))')(izaf(k), excess(k), spnpar, k = 1,
C    &                              nmass)

C     RIPL-2 mass file adopted (format: (2i4,1x,a2,1x,i1,3f10.3,4f8.3))
C     Ground state properties based on the FRDM model
C     9066 masses

      OPEN(UNIT = 27, STATUS = 'OLD',
     &          FILE = '../RIPL-2/masses/mass-frdm95.dat')
C     Skipping header lines
      read(27,*)
      read(27,*)
      read(27,*)
      read(27,*)
      DO k=1,NMAsse
C  Z   A    fl    Mexp      Mth      Emic    beta2   beta3   beta4   beta6
       READ(27, '(2i4,4x,i1,3f10.3,f8.3)',END=100)
     &   nixz, nixa, iflag, xmassexp, xmassth, emicx(k), beta2x(k)
       izaf(k)=nixz*1000+nixa
       if(iflag.GE.1) then
         excess(k) = xmassexp
       else
         excess(k) = xmassth
       endif
      ENDDO
  100 CLOSE(UNIT = 27)

      DO iz = 0, 130
         DO ia = 0, 400
          RESmas(iz, ia) = 0
            EXCessmass(iz, ia) = 0
         ENDDO
      ENDDO
C
      DO k = 1, NMAsse
         iz = izaf(k)/1000
         ia = MOD(izaf(k), 1000)
       if(iz.gt.130 .OR. ia.gt.400) cycle
         RESmas(iz, ia) = REAL(ia) + excess(k)/AMUmev
         EXCessmass(iz, ia) = excess(k)
      ENDDO
C
C     nucmas: subroutine for formula of Duflo-Zuker for masses outside M-N
C
      DO iz = 6, 100
         DO ia = 2*iz - 10, 3*iz
            IF(RESmas(iz, ia).EQ.0.D0)THEN
               in = ia - iz
               CALL NUCMAS(in, iz, ebin)
               RESmas(iz, ia) = iz*AMUpro + in*AMUneu - ebin/AMUmev
               EXCessmass(iz, ia) = RESmas(iz, ia)*AMUmev - REAL(ia)
            ENDIF
         ENDDO
      ENDDO

C     mbc1 a quick/temp? solution to weird light undefined masses: define
C     resmas=A for al nuclei so far undefined
C     prvisouly i had a problem for be6 => be5 +n since mass be5 undefined
      DO iz = 1, 130
         DO ia = 1, 400
            IF(RESmas(iz, ia).EQ.0.D0)THEN
               RESmas(iz, ia) = REAL(ia)/AMUmev
               EXCessmass(iz, ia) = 0
            ENDIF
         ENDDO
      ENDDO
C
      zmx = 0.
      zmn = 200.
      DO nnuc = 0, NNUct
         zmx = DMAX1(Z(nnuc), zmx)
         zmn = MIN(Z(nnuc), zmn)
      ENDDO

      DO k = 1, NMAsse
       nixz = izaf(k)/1000
       nixa = MOD(izaf(k), 1000)
       iz = izaf(k)/1000
       ia = MOD(izaf(k), 1000)
       IF(nixz.GE.zmn .AND. nixz.LE.zmx)THEN
         CALL WHERE(izaf(k), nnuc, iloc)
         IF(iloc.EQ.0)THEN
            SHC(nnuc) = emicx(k)
            IF(SHNix.EQ.0.D0) CALL SHELLC(A(nnuc), Z(nnuc), SHC(nnuc))
            DEF(1, nnuc) = beta2x(k)
            XMAss(nnuc) = EXCessmass(iz, ia)
         ENDIF
         IF(nixz.EQ.Z(0) .AND. nixa.EQ.A(0))THEN
            SHC(0) = emicx(k)
            IF(SHNix.EQ.0.D0)CALL SHELLC(A(0), Z(0), SHC(0))
            DEF(1, 0) = beta2x(k)
          XMAss(0) = EXCessmass(iz, ia)
         ENDIF
       ELSE
         DO ii = 0, NDEJC
            IF(nixz.EQ.ZEJc(ii) .AND. nixa.EQ.AEJc(ii))THEN
               DEFprj = beta2x(k)
               XMAss_ej(ii) = EXCessmass(iz, ia)
            ENDIF
         ENDDO
       ENDIF
      ENDDO

C     Fermi energies calculated for all nuclei and projectile combinations
      DO nnuc = 0, NNUct
         iztar = Z(nnuc)
         iatar = A(nnuc)
         DO ii = 0, NDEJC
           izpro = ZEJc(ii)
           iapro = AEJc(ii)
           efermi=-0.5*( EXCessmass(iztar-izpro, iatar-iapro)
     &             -EXCessmass(iztar+izpro, iatar+iapro)
     &        +2.*EXCessmass(izpro      , iapro      ) )
           EEFermi(ii,nnuc) = efermi
         ENDDO
      ENDDO
      RETURN
      END
C*==readldp.spg  processed by SPAG 6.20Rc at 12:12 on  7 Jul 2004
C
C
C
      SUBROUTINE READLDP
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:iou*
Ccc   *                         R E A D L D P                            *
Ccc   *                                                                  *
Ccc   *     Reads level density paprameter according to Mebel and        *
Ccc   *     the discrete level below which the decay scheme is complete  *
Ccc   *     as determined by Mollnar (see RIPL CRP) from file 24         *
Ccc   *     File 24 is organized in the following way:                   *
Ccc   *     Z*1000+A                                                     *
Ccc   *     NLEVC number of the level (NLEVC=1 for g.s.)                 *
Ccc   *     AROGC - a-parameter without collective effects (G.C.)        *
Ccc   *     AROC  - a-parameter including collective effects.            *
Ccc   *     QN    - neutron binding energy (as in Iljinov & Mebel)       *
Ccc   *     DOBS  - D-observed for neutron resoances ( -- " --)          *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * input:none                                                       *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * output:none                                                      *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * calls:where                                                      *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * author: M.Herman                                                 *
Ccc   * date:   12.Jul.1997                                              *
Ccc   * revision:1    by:M. Herman                on:11.07.1998          *
Ccc   *               EMPIRE systematics for level density parameter 'a' *
Ccc   *               introduced, 'a' values compatible with the dynamic *
Ccc   *               level density model read in form file ldp.dat,     *
Ccc   *               local normalization of the systematics to the exp. *
Ccc   *               data introduced for the dynamic and Gilbert-       *
Ccc   *               Cameron level densities.                           *
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C Local variables
C
      DOUBLE PRECISION a23, acrt, ap1, ap2, ar, aroc, arogc, asys, atil,
     &                 atilave, atilsum, del, delp, dob, econd, gamma,
     &                 pi2, qn, tcrt, uexc, xr
      REAL FLOAT
      DOUBLE PRECISION FSHELL
      INTEGER iloc, ix, izamn, izamx, izar, nexp, nlevc, nnuc
C     INTEGER INT, MAX, MIN
C
      pi2 = PI**2
      izamx = 0
      izamn = 200000
      DO nnuc = 0, NNUct
         izamx = MAX(IZA(nnuc), izamx)
         izamn = MIN(IZA(nnuc), izamn)
      ENDDO
      nexp = 0
      atilsum = 0.0
 100  READ(24, '(I7,I4,2F7.2,E10.4,E10.3)', END = 200)izar, nlevc,
     &     arogc, aroc, qn, dob
      IF(izar.GE.izamn .AND. izar.LE.izamx)THEN
         CALL WHERE(izar, nnuc, iloc)
         IF(iloc.EQ.0)THEN
            NLV(nnuc) = nlevc
            DOBs(nnuc) = dob
            a23 = A(nnuc)**0.666667
C-----------set up normalization factors for level density parameter 'a'
            IF(ROPaa(nnuc).EQ.( - 2.D0) .AND. arogc.NE.0.0D0)THEN
C--------------Gilbert-Cameron (no explicit collective effects)
               IF(ADIv.EQ.2.D0)THEN
                  del = 0.0
                  delp = 12.0/SQRT(A(nnuc))
                  IF(MOD(XN(nnuc), 2.D0).EQ.0.D0)del = delp
                  IF(MOD(Z(nnuc), 2.D0).EQ.0.D0)del = del + delp
                  uexc = qn - del
                  atil = 0.114*A(nnuc) + 9.80E-2*A(nnuc)**0.666667
                  gamma = 0.051
                  asys = atil*(1.0 + SHC(nnuc)
     &                   *(1.0 - EXP((-gamma*uexc)))/uexc)
                  IF(ATIlnor(nnuc).EQ.0.D0)ATIlnor(nnuc) = arogc/asys
               ENDIF
               IF(ADIv.EQ.0.0D0)THEN
                  del = 0.
                  delp = 12./SQRT(A(nnuc))
                  IF(MOD(XN(nnuc), 2.D0).NE.0.0D0)del = delp
                  IF(MOD(Z(nnuc), 2.D0).NE.0.0D0)del = del + delp
C-----------------EMPIRE systematics with Nix-Moeller shell corrections
                  ap1 = 0.94431E-01
                  ap2 = -0.80140E-01
                  gamma = 0.75594E-01
                  IF(Z(nnuc).GE.85.D0)THEN
                     ap1 = ap1*1.2402
                     ap2 = ap2*1.2402
                     gamma = gamma*1.2494
                  ENDIF
C-----------------EMPIRE systematics with M-S shell corrections
                  IF(SHNix.EQ.0.0D0)THEN
                     ap1 = .52268E-01
                     ap2 = .13395E+00
                     gamma = .93955E-01
                     IF(Z(nnuc).GE.85.D0)THEN
                        ap1 = ap1*1.2942
                        ap2 = ap2*1.2942
                        gamma = gamma*1.2928
                     ENDIF
                  ENDIF
                  atil = ap1*A(nnuc) + ap2*a23
                  tcrt = 0.567*delp
                  ar = atil*(1.0 + SHC(nnuc)*gamma)
                  DO ix = 1, 10
                     xr = ar*tcrt**2
                     acrt = atil*FSHELL(xr, SHC(nnuc), gamma)
                     IF(ABS(acrt - ar)/acrt.LE.0.001D0)GOTO 105
                     ar = acrt
                  ENDDO
 105              econd = 1.5*acrt*delp**2/pi2
                                              !-del  !!!!!!!!!!!
                  uexc = qn + del - econd
                  asys = atil*(1.0 + SHC(nnuc)
     &                   *(1.0 - EXP((-gamma*uexc)))/uexc)
                  IF(ATIlnor(nnuc).EQ.0.0D0)ATIlnor(nnuc) = aroc/asys
               ENDIF
               atilsum = atilsum + ATIlnor(nnuc)
               nexp = nexp + 1
               IF(FITlev.GT.0.0D0)THEN
                  WRITE(6, *)' '
                  WRITE(6, *)'Nucleus A=', INT(A(nnuc)), ' Z=',
     &                       INT(Z(nnuc))
                  IF(ADIv.EQ.0.0D0 .OR. ADIv.EQ.3.0D0)WRITE(6, *)'SHC=',
     &               SHC(nnuc), ' U=', uexc, ' DELTA=', del, ' asys=',
     &               asys, ' aexp=', aroc
                  IF(ADIv.EQ.2.0D0)WRITE(6, *)'SHC=', SHC(nnuc), ' U=',
     &               uexc, ' DELTA=', del, ' asys=', asys, ' aexp=',
     &               arogc
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      GOTO 100
 200  IF(ROPaa(nnuc).EQ.( - 2.D0) .AND. nexp.GT.2)THEN
         atilave = atilsum/FLOAT(nexp)
         WRITE(6, *)' '
         WRITE(6, *)'Level density systematics normalized by factor ',
     &              atilave
         WRITE(6, *)' '
      ELSE
         WRITE(6, *)' '
         WRITE(6, *)
     &'Level density systematics NOT normalized to Dobs at neutron bindi
     &ng energy'
         WRITE(6, *)' '
         atilave = 1.0
      ENDIF
      DO nnuc = 0, NNUct
         IF(ATIlnor(nnuc).EQ.0.0D0)ATIlnor(nnuc) = atilave
      ENDDO
      END
C*==shellc.spg  processed by SPAG 6.20Rc at 12:12 on  7 Jul 2004
C
C
C
      SUBROUTINE SHELLC(Annuc, Znnuc, Shllc)
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C COMMON variables
C
      DOUBLE PRECISION BARr, SHLl, SMAss
      INTEGER IPArq
      COMMON /BLOCX / IPArq
      COMMON /FUSR  / BARr, SMAss, SHLl
C
C Dummy arguments
C
      DOUBLE PRECISION Annuc, Shllc, Znnuc
C
C Local variables
C
      DOUBLE PRECISION cbarr, cmass
      INTEGER ia, iz, nobarr
      INTEGER INT
C-----IPARQ=0 fission barrier, mass and shell correction from Myers-Swiatecki
C-----IPARQ=1 fission barrier from Krappe-Nix
      IPArq = 0
      iz = INT(Znnuc)
      ia = INT(Annuc)
C     NN=IA-IZ
      CALL LYMASM(iz, ia, cmass, cbarr, nobarr)
C     CALL TZTN
C-----DL and DW are Cameron mass and shell correction respectively
C-----DL=DELCAM(FLOAT(IA),FLOAT(IZ))
C-----DW=TZ(IZ)+TN(NN)
99001 FORMAT(//20X, 'Z =', I2, '  A=', I3)
99002 FORMAT(/1X, 'SHELL CORRECTION   M.-SW=', E10.3, '  CAMERON=',
     &       E10.3)
99003 FORMAT(/1X, 'MASS               M.-SW=', E10.3, '  CAMERON=',
     &       E10.3)
      Shllc = SHLl
      END
C*==aa0001.spg  processed by SPAG 6.20Rc at 12:12 on  7 Jul 2004
C
C
C
      BLOCKDATA
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C COMMON variables
C
      DOUBLE PRECISION AFAn, ALFa, BETa, DLT, DLTf, GAMa, PA, PB, PEP,
     &                 PN, R, RK, ZT, ZTT, ZVT
      COMMON /BLOC1 / DLT, DLTf
      COMMON /BLOC2 / AFAn, ALFa, BETa, GAMa, PN, R
      COMMON /DEF1  / PEP
      COMMON /DEF2  / PA, PB
      COMMON /RKROT / RK
      COMMON /STEP  / ZT, ZTT, ZVT
C
C     COMMON variables
C
      DATA PEP/0.24/
      DATA PA/0.45/
      DATA PB/0.87/
      DATA PN/0.6666667/
      DATA R/1.5/
      DATA RK/1.2/
      DATA DLT/12./
      DATA DLTf/14./
      DATA ZT/.333333/
      DATA ZTT/.6666667/
      DATA ZVT/1.666667/
C     DATA AI /1.,   1.,    2.,    3.,    3.,   4. ,  0., 0./,
C     *     ZI /0.,   1.,    1.,    1.,    2.,   2. ,  0., 0./,
C     *     DLM/8.072,7.289,13.136,14.950,14.932,2.425,0., 0./,
C     *     VK/ 0.,   0.7,   0.77,  0.8,   0.8,  0.83 ,0., 0./,
C     *     GAM/1.,   1.,    3.,    3.,    3.,   2.  /,
C     *     CC /0.,   0.2,   0.1,   0.07,  0.13, 0.1 /
      END
C*==lymasm.spg  processed by SPAG 6.20Rc at 12:12 on  7 Jul 2004
C
C
C     SUBROUTINE TZTN
C     THE SHELL CORRECTIONS FROM THE PROCEEDING INTERNATIONAL
C     CONFERENCE ON THE PROPERTIES OF NUCLEI FAR FROM THE REGION
C     OF BETA - STABILITY  VOL. 1 (1970)   CERN 70 - 30
C                  J.W. TRURAN AND A.G.W. CAMERON
C     IMPLICIT DOUBLE PRECISION (A-H, O-Z)
C     COMMON /BL001/ TZ(102)
C    *       /BL002/ TN(155)
C     DIMENSION TZZ(102),TNN(155)
C     DATA    TZZ/
C    *8*0.,2.500,
C    *2.349,1.936,1.596,1.061,0.341,-0.040,0.565,1.065,1.536,1.972,
C    *1.855,2.043,1.931,1.652,1.347,0.973,0.579,0.159,-0.487,-0.192,
C    *0.443,0.932,1.387,1.810,1.969,2.067,2.064,1.825,1.539,1.251,0.957,
C    *1.128,1.007,0.603,0.013,-0.635,-1.258,-1.905,-2.562,-3.266,-4.099,
C    *-3.615,-3.171,-2.814,-2.337,-1.778,-1.220,-0.694,-0.181,0.323,
C    *0.624,0.841,0.904,0.906,0.930,0.919,0.934,0.941,0.978,0.982,1.083,
C    *1.201,1.281,1.189,0.963,0.781,0.738,0.696,0.119,-0.619,-1.265,
C    *-1.898,-2.431,-1.326,-0.268,0.737,1.451,2.138,2.307,2.221,2.041,
C    *1.827,1.239,0.747,0.214,-0.263,-0.778,-1.272,-1.800,-2.302,-2.846,
C    *-3.499,-3.042/
C     DATA    TNN/ 9*0.,
C    *2.439,1.829,1.419,0.746,-0.082,-0.832,-0.960,-1.006,-1.045,-1.114,
C    *-0.900,-0.081,0.334,0.064,-0.639,-1.363,-2.138,-2.987,-4.042,
C    *-4.001,-3.582,-3.120,-2.677,-2.259,-1.778,-1.315,-0.944,-0.599,
C    *-0.285,-0.020,0.121,0.140,0.149,-0.001,-0.230,-0.604,-1.010,
C    *-1.570,-2.466,-3.489,-4.552,-4.214,-3.375,-2.526,-1.725,-0.923,
C    *-0.164,0.601,1.316,1.947,2.482,2.971,3.398,3.737,3.979,4.183,
C    *4.374,4.517,4.605,4.539,4.375,4.043,3.672,3.250,2.776,2.254,1.715,
C    *1.151,0.463,-0.237,-1.031,-1.850,-2.722,-1.663,-0.724,0.035,0.786,
C    *1.587,2.145,2.669,2.680,2.488,2.243,1.969,1.778,1.663,1.487,1.325,
C    *1.148,0.962,0.843,0.727,0.574,0.436,0.320,0.264,0.397,0.507,0.405,
C    *0.346,0.369,0.397,0.403,0.379,0.184,-0.226,-0.737,-1.305,-1.950,
C    *-2.565,-3.126,-3.721,-4.393,-5.082,-5.921,-6.712,-6.853,-5.592,
C    *-4.413,-3.333,-2.413,-1.582,-0.966,-0.421,-0.123,0.228,0.543,
C    *0.874,1.059,1.181,1.186,1.029,1.029,1.153,1.227,1.330,1.449,1.596,
C    *1.712,1.851,1.949,2.044,2.155,2.307,2.621,3.096/
C     DO 1 IZ=1,102
C  1  TZ(IZ)=TZZ(IZ)
C     DO 2 IN=1,155
C  2  TN(IN)=TNN(IN)
C     RETURN
C     END
C
C     DOUBLE PRECISION FUNCTION DELCAM(X,Y)
C      TRURAN-CAMERON-HILF- 1970
C     IMPLICIT DOUBLE PRECISION (A-H, O-Z)
C     COMMON /SPZ/ SPZ(102) /SPN/ SPN(155)
C     COMMON /STEP/   ZT,ZTT,ZVT
C     COMMON /BL001/  TZ(102)  /BL002/ TN(155)
C     DIMENSION SPZZ(102),SPNN(155)
C     DATA B0/-17.07059/,B2/36.6987/,B4/-24.545/,B6/20.0/,
C    *     G0/27.18466/,G2/-70.0/,G4/40.0/,C/-10.0/,W/-11.5/,
C    *     A1/.7820268/,A2/-1.607342E-4/,A3/-1.1131396/,A4/-.5971624/
C     SPZ = S(Z) + P(Z)
C     *-1.394* - CORR.  JUNE-87
C     DATA SPZZ /
C    * 9*0.,
C    * 0.149, 1.936,-0.524, 1.061,-1.64 ,-0.040,-0.926, 1.065, 0.086,
C    * 1.972, 0.154, 2.043, 0.587, 1.652,-0.002, 0.973,-0.818, 0.159,
C    *-1.798,-0.192,-0.718, 0.932, 0.186, 1.810, 0.52 , 2.067, 0.733,
C    * 1.825, 0.267, 1.251,-0.241, 1.128,-0.333, 0.603,-1.394,-0.635,
C    *-2.545,-1.905,-3.896,-3.266,-5.406,-3.615,-4.299,-2.814,-3.489,
C    *-1.778,-2.359,-0.694,-1.319, 0.323,-0.491, 0.841,-0.166, 0.906,
C    *-0.166, 0.919,-0.189, 0.941, 0.077, 0.982, 0.15 , 1.201, 0.567,
C    * 1.189, 0.164, 0.781,-0.102, 0.696,-0.607,-0.619,-2.08 ,-1.898,
C    *-3.145,-1.326,-1.056, 0.737, 0.658, 2.138, 1.644, 2.221, 1.744,
C    * 1.827, 0.528, 0.747,-0.347,-0.263,-1.472,-1.272,-2.483,-2.302,
C    *-3.347,-3.499,-3.533/
C     SPN = S(N) + P(N)
C     * -3.435,-6.579,+0.091 * - CORR.  JUNE-87
C     * -0.293 * - CORR.  JULE-87
C     DATA SPNN /
C    * 9*0.,
C    * 0.039, 1.829,-0.939, 0.746,-2.139,-0.832,-2.422,-1.006,-2.637,
C    *-1.114,-2.428,-0.081,-1.136, 0.064,-1.949,-1.363,-3.454,-2.987,
C    *-5.307,-4.001,-4.861,-3.120,-3.933,-2.259,-3.063,-1.315,-2.384,
C    *-0.599,-1.802,-0.02 ,-1.365, 0.14 ,-1.307,-0.001,-1.701,-0.604,
C    *-2.346,-1.57 ,-3.807,-3.489,-5.83 ,-4.214,-4.196,-2.526,-2.539,
C    *-0.923,-1.259, 0.601, 0.169, 1.947, 1.187, 2.971, 2.117, 3.737,
C    * 2.734, 4.183, 3.177, 4.517, 3.378, 4.539, 3.084, 4.043, 2.418,
C    * 3.250, 1.466, 2.254, 0.544, 1.151,-0.629,-0.237,-2.093,-1.850,
C    *-3.435,-1.663,-1.546, 0.035,-0.057, 1.587, 1.177, 2.669, 1.563,
C    * 2.488, 1.244, 1.969, 0.901, 1.663, 0.643, 1.325, 0.259, 0.962,
C    * 0.114, 0.727,-0.132, 0.436,-0.303, 0.264,-0.114, 0.507,-0.368,
C    * 0.346,-0.293, 0.397,-0.405, 0.379,-0.705,-0.226,-1.667,-1.305,
C    *-2.621,-2.565,-3.877,-3.721,-5.228,-5.082,-6.579,-6.712,-7.460,
C    *-5.592,-5.070,-3.333,-3.108,-1.582,-1.423,-0.421,-0.468, 0.228,
C    *+0.091, 0.874, 0.411, 1.181, 0.505, 1.029, 0.613, 1.153, 0.682,
C    * 1.33 , 0.967, 1.596, 1.331, 1.851, 1.338, 2.044, 1.501, 2.307,
C    * 2.064, 3.096/
Cc    PRINT 10, X,Y
Cc10  FORMAT(1X,'DELCAM  A Z',2E10.3)
C     XR=X**ZT
C     XR2=XR*XR
C     SYM=((X-2.*Y)/X)**2
C     SYM2=SYM*SYM
C     SYM3=SYM2*SYM
C     ECUR=C*XR
C     ECUL=(A1/XR+A2*XR+A3/X+A4/XR/Y**ZTT)*Y*Y
C     EWIG=W*EXP(-6.*ABS((X-2.*Y)/X))
C     ESYM=X*(B0+B2*SYM+B4*SYM2+B6*SYM3)+
C    *(G0+G2*SYM+G4*SYM2)*XR2
C     EDOB=8.07144*X-.78245*Y
C     I=X
C     J=Y
C     L=I-J
C     T1=SPZZ(J)
C     T2=SPNN(L)
Cmh---LD set to 0 by M.H. as it was undefined. Should not matter as this part
Cmh---of the code is not supposed to be executed
C     LD=0
C     IF (LD.EQ.1) T1=T1-TZ(J)
C     IF (LD.EQ.1) T2=T2-TN(L)
C     DELCAM=EDOB+ESYM+ECUR+EWIG+ECUL+T1+T2
C     DO 1 IZ=1,102
C  1  SPZ(IZ)=SPZZ(IZ)
C     DO 2 IN=1,155
C  2  SPN(IN)=SPNN(IN)
C     RETURN
C     END
C
C
C
      SUBROUTINE LYMASM(Iz, Ia, Cmass, Cbarr, Nobarr)
C
C     WILLIAM D. MYERS - 6 JULY 1970
C
      IMPLICIT DOUBLE PRECISION(a - H), DOUBLE PRECISION(O - z)
C
C COMMON variables
C
      DOUBLE PRECISION BARr, ENEx, SHLl, SMAss
      INTEGER IPArq
      COMMON /BLOCX / IPArq
      COMMON /FFSS  / ENEx
      COMMON /FUSR  / BARr, SMAss, SHLl
C
C Dummy arguments
C
      DOUBLE PRECISION Cbarr, Cmass
      INTEGER Ia, Iz, Nobarr
C
C Local variables
C
      DOUBLE PRECISION a, a1, a2, a2rt, a3, a3rt, a3rt2, acor, alevel,
     &                 cay6, coulmb, d, ee, em(10), emp(10), eps, exmt2,
     &                 ext2, f(2), ff, fuzsur, gamma, gl, oddev, parmas,
     &                 s, smalc, spw, sshell, sufnuc, sym, t, t2, test,
     &                 to, to2, tsq, un, v, volnuc, wotnuc, wterm, x,
     &                 xk(10), y(2), z, zsq, zt, ztt, zvt
      REAL FLOAT
      INTEGER i, ipq, j, n
      DOUBLE PRECISION XI, XIMOD
C     Capote 2001
C
C     COMMON /BLOCXX/ ARQ,F1Q,F1MQ,SUFNUC
C     COMMON /CCC/    WOTNUC,VOLNUC,COULMB,
C     *                A,Z,UN,A1,A2,A3,GGMMA,A3RT2,A3RT,ZSQ,
C     *                ODDEV,SYM,PARMAS,ACOR
      DATA zvt/1.6666666666/
      DATA zt/.3333333333/
      DATA ztt/.6666666667/
C
C     Capote 2001
C     DATA sr5/2.2360679775/
      DATA em/0.00, 2.00, 8.00, 14.00, 28.00, 50.00, 82.00, 126.00,
     &     184.00, 258.00/
      DATA cay1/0.0/
      DATA cay2/0.0/
      DATA cay3/2.0/
      DATA cay4/11.0/
      DATA cay5/8.07144/
      DATA cay6/7.28899/
      DATA d/.444/
      DATA c/5.8/
      DATA smalc/.325/
C
C     DMASS = REMAINDER AFTER CM - NO SHELL EFFECTS SUBTRACTED
C     SHLL = CALCULATED SHELL EFFECT
C     DIFMAS= DMASS - SHLL
C
C------------------------------
      a1 = 15.4941
C..... IPARQ=0  PARAMETRS MYERS-SWIATECKI
      IF(IPArq.EQ.0)THEN
         a2 = 17.9439
         a3 = 0.7053
         gamma = 1.7826
      ELSEIF(IPArq.EQ.1)THEN
         a2 = 24.70
         a3 = 0.74476032
         gamma = 4.0
      ELSEIF(IPArq.EQ.2)THEN
         a2 = 19.008
         a3 = 0.720
         gamma = 2.840
      ELSE
         ENEx = 0.0
         alevel = 0.1
         ampar = alevel*FLOAT(Ia)
         tsq = ENEx/ampar
         a2 = 17.9439*(1. - 0.0063157*tsq)
         a3 = 0.7053*(1. - 0.001*tsq)
         gamma = 1.7826
      ENDIF
      IF(Iz.EQ.0)THEN
         Cmass = 0.0
         RETURN
      ENDIF
      Nobarr = 0
      DO i = 1, 10
         emp(i) = em(i)**zvt
      ENDDO
      DO i = 1, 9
         xk(i) = .600*(emp(i + 1) - emp(i))/(em(i + 1) - em(i))
      ENDDO
C
C     FOR DEFINITIONS OF CAY1 AND RZ,SEE UCRL-11980
C
      cay1 = 3.28637900*a3**3
      z = FLOAT(Iz)
      zsq = z**2
      n = Ia - Iz
      un = FLOAT(n)
      a = FLOAT(Ia)
      a3rt = a**zt
      a3rt2 = a3rt**2
      a2rt = SQRT(a)
      sym = ((un - z)/a)**2
      acor = 1.00 - gamma*sym
      parmas = cay5*un + cay6*z
      volnuc = -a1*acor*a
      sufnuc = a2*acor*a3rt2
      coulmb = a3*zsq/a3rt
      fuzsur = -cay1*zsq/a
      oddev = -(1.00 + 2.00*(n/2) - un + 2.00*(Iz/2) - z)/a2rt*cay4
      wterm = -cay2*a3rt2*EXP(( - cay3*sym))
      wotnuc = parmas + coulmb + fuzsur + oddev + wterm
      SMAss = wotnuc + volnuc + sufnuc
      spw = sufnuc + wterm
      c2 = spw/a3rt2
      x = .5*coulmb/spw
      IF(x.LT.1.00)THEN
C------------------------------
         IF(IPArq.EQ.0)BARr = sufnuc*XI(x)
         IF(IPArq.EQ.2)BARr = sufnuc*XI(x)
         IF(IPArq.EQ.3)BARr = sufnuc*XI(x)
         IF(IPArq.EQ.1)BARr = sufnuc*XIMOD(x)
C------------------------------
      ELSE
         BARr = 0.0
      ENDIF
      y(1) = un
      y(2) = z
      DO j = 1, 2
         DO i = 1, 9
            IF(y(j) - em(i + 1).LE.0.D0)GOTO 50
         ENDDO
         PRINT 99001, j
99001    FORMAT('1FAILURE IN LYMASS - Y(', I1,
     &          ') EXCEEDS LAST MAGIC NO.')
         STOP
 50      f(j) = xk(i)*(y(j) - em(i)) - .600*(y(j)**zvt - emp(i))
      ENDDO
      s = (2.00/a)**ztt*(f(1) + f(2)) - smalc*a3rt
      c2d2 = c2*d**2
      ee = (c2d2 + c2d2)*(1.00 - x)
      ff = .425917710*c2d2*d*(1.00 + x + x)/a3rt
      sshell = c*s
      v = sshell/ee
      eps = 1.500*ff/ee
      IF(ee*(1.00D0 - 3.00D0*v).GT.0.00D0)THEN
C        QCALC=0.00
         SHLl = sshell
      ELSE
C
C        ESTIMATE THETA
C
         to = 1.00
C
C        ITERATE TO FIND EQUILIBRIUM THETA
C
 100     DO ipq = 1, 10
            to2 = to**2
C----------------------------------------
C           IF (TO2.GT.170.D0) PRINT 500, IZ,IA
C           500 FORMAT(1X,'LYMASM',2X,2I5)
C----------------------------------------
C           EXMT2= EXP(-TO2)
            exmt2 = 1.E-20
            IF(ABS(to2).LT.30.D0)exmt2 = EXP(( - to2))
C
            t = to - (1.00 - eps*to - v*(3.00 - to2 - to2)*exmt2)
     &          /(( - eps) + v*to*(10.00 - 4.00*to2)*exmt2)
            IF(t.LE.0.00D0)GOTO 200
            IF(ABS(t - to).LT.1.D-4)GOTO 150
            to = t
         ENDDO
         GOTO 250
 150     t2 = t**2
C        EXT2= EXP(-T2)
         ext2 = 1.E-20
         IF(ABS(t2).LT.30.D0)ext2 = EXP(( - t2))
C
         test = ee*(1.00 - eps*(t + t) - v*((4.00*t2-12.00)*t2 + 3.00)
     &          *ext2)
         IF(test.GT.0.00D0)THEN
            t2 = t**2
            SHLl = t2*(ee - ff*t) + sshell*(1.00 - t2 - t2)*EXP(( - t2))
            GOTO 300
         ENDIF
 200     to = .100
         DO i = 1, 20
            to2 = to**2
            gl = ee*(1.00 - eps*to - v*(3.00 - to2 - to2)*EXP((-to2)))
            IF(gl.GT.0.00D0)GOTO 100
         ENDDO
 250     Cmass = SMAss
         Cbarr = 0.00
         Nobarr = 1
         RETURN
      ENDIF
 300  Cmass = SMAss + SHLl
      Cbarr = BARr - SHLl
      END
C*==xi.spg  processed by SPAG 6.20Rc at 12:12 on  7 Jul 2004
C
C
C
      DOUBLE PRECISION FUNCTION XI(Z)
C
C     6-POINT LAGRANGE INTERPOLATION
C
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C
C     Dummy arguments
C
      DOUBLE PRECISION Z
C
C     Local variables
C
      DOUBLE PRECISION del, dm1, dm2, dm3, dp1, dp2, prod, w1, w2, w3,
     &                 w4, w5, w6, y(51), zbh
      REAL FLOAT, SNGL
      INTEGER IFIX
      INTEGER m
C
      DATA y/.25900, .255200, .250700, .245100, .2400, .23400, .228500,
     &     .22200, .21600, .2100, .20300, .196800, .1900, .18300,
     &     .175800, .1692400, .1620300, .1547800, .147500, .1401900,
     &     .1328400, .1254500, .1180100, .1105200, .1029600, .0953500,
     &     .0876800, .0799900, .0722900, .064600, .0569500, .0493700,
     &     .0419300, .0347600, .0281100, .0223600, .0176200, .0137300,
     &     .0105600, .0079800, .0059100, .0042500, .0029600, .0019700,
     &     .0012300, 7.1E-4, 3.6E-4, 1.5E-4, 4.E-5, 1.E-5, 0.00/
C
C     THE X VALUES ARE EVENLY SPACED - X = 0(.02)1
C
      zbh = Z*50.00
      m = IFIX(SNGL(zbh))
      del = zbh - FLOAT(m)
      m = m + 1
      IF(m.GT.51)THEN
         m = 51
      ELSEIF(del.GE.1.D-4)THEN
         IF(m.LT.3)THEN
            del = del - FLOAT(3 - m)
            m = 3
         ELSEIF(m.GT.48)THEN
            del = del + FLOAT(m - 48)
            m = 48
         ENDIF
         dm3 = del - 3.00
         prod = dm3*del
         w6 = 1.00/(1.2E2*dm3)
         dm2 = dm3 + 1.00
         prod = dm2*prod
         w5 = -1.00/(24.00*dm2)
         dm1 = dm2 + 1.00
         prod = dm1*prod
         w4 = 1.00/(12.00*dm1)
         dp1 = dm1 + 2.00
         prod = dp1*prod
         w2 = 1.00/(24.00*dp1)
         dp2 = dp1 + 1.00
         prod = dp2*prod
         w1 = -1.00/(1.2E2*dp2)
         w3 = -1.00/(12.00*del)
         XI = prod*(w1*y(m - 2) + w2*y(m - 1) + w3*y(m) + w4*y(m + 1)
     &        + w5*y(m + 2) + w6*y(m + 3))
         GOTO 99999
      ENDIF
      XI = y(m)
99999 END
C*==ximod.spg  processed by SPAG 6.20Rc at 12:12 on  7 Jul 2004
C
C
C
      DOUBLE PRECISION FUNCTION XIMOD(Z)
C
C     6-POINT LAGRANGE INTERPOLATION
C     IN MODIFIED LIQUID-DROP FORMULA
C     ( KRAPPE [ NIX --  IAEA-SM-174/12 )
C
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C Dummy arguments
C
      DOUBLE PRECISION Z
C
C Local variables
C
      DOUBLE PRECISION del, dm1, dm2, dm3, dp1, dp2, prod, w1, w2, w3,
     &                 w4, w5, w6, y(51), zbh
      REAL FLOAT, SNGL
      INTEGER IFIX
      INTEGER m
C
      DATA y/0.12200, 0.12100, 0.11980, 0.11830, 0.11690, 0.11520,
     &     0.1133, 0.11130, 0.10900, 0.10670, 0.10420, 0.10150, 0.09850,
     &     0.09540, 0.09180, 0.08780, 0.08350, 0.07900, 0.07460,
     &     0.06960, 0.06470, 0.05960, 0.05420, 0.04880, 0.04350,
     &     0.03880, 0.03400, 0.02920, 0.02460, 0.02020, 0.01580,
     &     0.01220, 0.00900, 0.00660, 0.00490, 0.00360, 0.00280,
     &     0.00220, 0.00180, 0.00140, 0.00100, 0.00090, 0.00060,
     &     0.00040, 0.00020, 0.00010, 0.00000, 0.00000, 0.00000,
     &     0.00000, 0.00000/
C
C     THE X VALUES ARE EVENLY SPACED - X = 0(.02)1
C
      zbh = Z*50.00
      m = IFIX(SNGL(zbh))
      del = zbh - FLOAT(m)
      m = m + 1
      IF(m.GT.51)THEN
         m = 51
      ELSEIF(del.GE.1.D-4)THEN
         IF(m.LT.3)THEN
            del = del - FLOAT(3 - m)
            m = 3
         ELSEIF(m.GT.48)THEN
            del = del + FLOAT(m - 48)
            m = 48
         ENDIF
         dm3 = del - 3.00
         prod = dm3*del
         w6 = 1.00/(1.2E2*dm3)
         dm2 = dm3 + 1.00
         prod = dm2*prod
         w5 = -1.00/(24.00*dm2)
         dm1 = dm2 + 1.00
         prod = dm1*prod
         w4 = 1.00/(12.00*dm1)
         dp1 = dm1 + 2.00
         prod = dp1*prod
         w2 = 1.00/(24.00*dp1)
         dp2 = dp1 + 1.00
         prod = dp2*prod
         w1 = -1.00/(1.2E2*dp2)
         w3 = -1.00/(12.00*del)
         XIMOD = prod*(w1*y(m - 2) + w2*y(m - 1) + w3*y(m) + w4*y(m + 1)
     &           + w5*y(m + 2) + w6*y(m + 3))
         GOTO 99999
      ENDIF
      XIMOD = y(m)
99999 END
C*==bndg.spg  processed by SPAG 6.20Rc at 12:12 on  7 Jul 2004
C
C
C
C
      SUBROUTINE BNDG(Nejc, Nnuc, Bnd)
Ccc
Ccc   ******************************************************************
Ccc   *                                                       class:iou*
Ccc   *                         B N D G                                *
Ccc   *                                                                *
Ccc   *           Calculates binding energis                           *
Ccc   *                                                                *
Ccc   *                                                                *
Ccc   * input:NEJC, NNUC                                               *
Ccc   *                                                                *
Ccc   *                                                                *
Ccc   * output:BND                                                     *
Ccc   *                                                                *
Ccc   *                                                                *
Ccc   *                                                                *
Ccc   * calls:where                                                    *
Ccc   *                                                                *
Ccc   *                                                                *
Ccc   * author: M.Herman & R.Sturiale                                  *
Ccc   * date:   27.Sep.1996                                            *
Ccc   * revision:1    by:M.Herman                 on: 1.Dec.1997       *
Ccc   *                                                                *
Ccc   ******************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C Dummy arguments
C
      DOUBLE PRECISION Bnd
      INTEGER Nejc, Nnuc
C
C Local variables
C
      INTEGER ar, zr, iztar, iatar, izpro, iapro
      DOUBLE PRECISION b1, b2, b3
      zr = Z(Nnuc) - ZEJc(Nejc)
      ar = A(Nnuc) - AEJc(Nejc)
      iztar = Z(Nnuc)
      iatar = A(Nnuc)
      izpro = ZEJc(Nejc)
      iapro = AEJc(Nejc)
C     b1 = A(Nnuc)*AMUmev + EXCessmass(iztar,iatar)
C     b2 = ar*AMUmev + EXCessmass(zr,ar)
C     b3 = AEJc(Nejc)*AMUmev + EXCessmass(izpro,iapro)
      b1 = A(Nnuc)*AMUmev + XMAss(Nnuc)
      b2 = ar*AMUmev + EXCessmass(zr,ar)
      b3 = AEJc(Nejc)*AMUmev + XMAss_ej(Nejc)
      Bnd = b2 + b3 - b1
      END
C*==retrieve.spg  processed by SPAG 6.20Rc at 12:12 on  7 Jul 2004
C
C
C
      SUBROUTINE RETRIEVE
Ccc   ******************************************************************
Ccc   *                                                       class:iou*
Ccc   *                     R E T R I E V E                            *
Ccc   *                                                                *
Ccc   *          Retrieves EXFOR entries relevant to EMPIRE run        *
Ccc   *                                                                *
Ccc   *                                                                *
Ccc   * input: none                                                    *
Ccc   *                                                                *
Ccc   *                                                                *
Ccc   * output: none                                                   *
Ccc   *                                                                *
Ccc   *                                                                *
Ccc   * calls: none                                                    *
Ccc   *                                                                *
Ccc   *                                                                *
Ccc   * author: M.Herman                                               *
Ccc   * date:   23.Oct.1999                                            *
Ccc   * revision:     by:                         on:                  *
Ccc   *                                                                *
Ccc   ******************************************************************
Ccc
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
      INCLUDE 'dimension.h'
C
C COMMON variables
C
      INTEGER NCHr
      CHARACTER*10 PROjec, RESidue(NDNUC), TARget
      COMMON /EXFOR / TARget, PROjec, RESidue
      COMMON /IEXFOR/ NCHr
C
C Local variables
C
      CHARACTER*80 exforec
      CHARACTER*36 filename, toplast, topname
C     INTEGER iend, nnuc
C
C
      INTEGER nnuc
C
C
C     INTEGER INDEX
      CHARACTER*115 indexrec
      CHARACTER*5 quantity(5)
      CHARACTER*10 reaction(2)
      CHARACTER*8 subent
C-----constant parameters for EXFOR retrieval
      DATA reaction/'N,F       ', 'N,TOT     '/
      DATA quantity/'CS   ', 'DAE  ', 'DA   ', 'DE   ', 'SP   '/
C-----open local file for storing retrieved EXFOR data
      OPEN(UNIT = 19, FILE = 'EXFOR.dat', STATUS = 'NEW', ERR = 99999)
C-----open EXFOR index
      OPEN(UNIT = 20, FILE = '../EXFOR/X4-INDEX.TXT', STATUS = 'OLD')
C
C-----scan EXFOR index for relevant subentries
C
 100  READ(20, '(A115)', END = 500)indexrec
      IF(indexrec(1:10).NE.TARget)GOTO 100
      IF(NCHr.EQ.2 .AND. indexrec(11:12).NE.PROjec(1:2))GOTO 100
      IF(NCHr.EQ.4 .AND. indexrec(11:14).NE.PROjec(1:4))GOTO 100
      DO nnuc = 1, NDNUC
         IF(indexrec(25:34).EQ.RESidue(nnuc))GOTO 200
      ENDDO
      IF(indexrec(25:34).NE.'>NN-1     ')THEN
         IF(indexrec(11:20).NE.reaction(1))THEN
            IF(indexrec(11:20).NE.reaction(2))GOTO 100
         ENDIF
      ENDIF
 200  IF(indexrec(41:45).NE.quantity(1))THEN
         IF(indexrec(41:45).NE.quantity(2))THEN
            IF(indexrec(41:45).NE.quantity(3))THEN
               IF(indexrec(41:45).NE.quantity(4))THEN
                  IF(indexrec(41:45).NE.quantity(5))GOTO 100
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      subent = indexrec(70:77)
C
C-----retrive EXFOR entry including top subentry 001
C
      toplast = ' '
      filename = '../EXFOR/subent'//'/'//subent(1:2)//'/'//subent(1:4)
     &           //'/'//subent(1:8)//'.txt'
      topname = '../EXFOR/subent'//'/'//subent(1:2)//'/'//subent(1:4)
     &          //'/'//subent(1:5)//'001.txt'
      IF(topname.NE.toplast)THEN
         OPEN(UNIT = 22, FILE = topname, ERR = 600, STATUS = 'OLD')
 250     READ(22, 99001, END = 300)exforec
         WRITE(19, 99001)exforec
         GOTO 250
      ENDIF
 300  toplast = topname
      CLOSE(22, ERR = 600)
      OPEN(UNIT = 22, FILE = filename, ERR = 600, STATUS = 'OLD')
 400  READ(22, 99001, END = 100)exforec
      WRITE(19, 99001)exforec
      GOTO 400
 500  CLOSE(19, ERR = 600)
      RETURN
 600  WRITE(6, *)'Not found EXFOR subentry ', subent
      GOTO 100
99001 FORMAT(A80)
99999 END
C*==ifindcoll.spg  processed by SPAG 6.20Rc at 12:12 on  7 Jul 2004
C
C
C
      INTEGER FUNCTION IFINDCOLL()
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:ipu*
Ccc   *                      F I N D C O L L                             *
Ccc   *                                                                  *
Ccc   *  Reads from the file 13 ground state spin and parity and first   *
Ccc   *  collective energies (the latter to be used by ECIS).            *
Ccc   *  Reads from the file 25 g.s. deformation to define               *
Ccc   *    nucleus structure, which is important for direct reaction     *
Ccc   *    cross section and Tlj calculations by the ECIS code           *
Ccc   *                                                                  *
Ccc   *  Uses unit 32 temporarily                                       *
Ccc   *  Some output to UNIT=6 is done                                   *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * input:IA   - A of the nucleus (through COMMON)                   *
Ccc   *       IZ   - Z of the nucleus (through COMMON)                   *
Ccc   *                                                                  *
Ccc   * output: (through COMMON)                                         *
Ccc   *        ND_NLV- number of collective states to be considered      *
Ccc   *        D_ELV(ND_NLV) - energy of each proposed collective state  *
Ccc   *        D_LVP(ND_NLV) - parity of each proposed collective state  *
Ccc   *        D_ELV(ND_NLV) - spin   of each proposed collective state  *
Ccc   *        D_DEF(ND_NLV) - dynamical deformations for each level     *
Ccc   *        DEFORMED - .TRUE. if nucleus has static deformation >0.15 *
Ccc   * return:                                                          *
Ccc   *   IFindColl = 0 (ALL POSSIBLE LEVELS FOUND)                      *
Ccc   *   IFindColl = 1 WARNING: (SOME COLLECTIVE LEVELS NOT FOUND)      *
Ccc   *   IFindColl = 2 ERROR: NO DISCRETE LEVEL INFORMATION AVAILABLE   *
Ccc   *   IFindColl = 3 SPHERICAL VIBRATIONAL ODD NUCLEUS                *
Ccc   *                 No suggestion for collective levels              *
Ccc   *                                                                  *
Ccc   * Creates files TARGET.LEV, COLLECTIVE.LEV and COLLECTIVE.TXT      *
Ccc   * TARGET_COLL.DAT   contains all collective  states to be          *
Ccc   *     considered in Coupled Channels and DWBA calculations         *
Ccc   *                                                                  *
Ccc   * TARGET.LEV       contains all discrete levels for the target     *
Ccc   *                                                                  *
Ccc   * calls:none                                                       *
Ccc   *                                                                  *
Ccc   * author: R.Capote                                                 *
Ccc   * date:   21.Jan.2001                                              *
Ccc   * revision:                                                        *
Ccc   * date:   07.March.2001                                            *
Ccc   * revision:                                                        *
Ccc   * date:   07.June.2004(RIPL-2 deformations parameters added)       *
Ccc   *                     (K=8+ included within the gs rotational band *
Ccc   * revision:                                                        *
Ccc   * date:   10.Sept.2004                                             *
Ccc   *         Non coupled levels for DWBA calculations retrieved       *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C     Local variables
C
      DOUBLE PRECISION beta2, beta3, delta_k, dtmp, dum, elvr, ftmp,
     &                 gspar, gspin, xjlvr, etmp, jtmp, betatmp
      CHARACTER*6 reftmp
      CHARACTER*100 ch_iuf, comment
      CHARACTER*5 chelem
      CHARACTER*9 finp
      CHARACTER*3 ctmp3
      DOUBLE PRECISION DBLE
      LOGICAL fexist
      INTEGER i, i0p, i20p, i21p, i3m, i4p, i6p, i8p, ia, iar, ierr,
     &        ilv, itmp, iz, izr, j, lvpr, nlvr, nztmp, natmp, iptmp
      INTEGER i1m, i5m, i22p, i31p, i41p

C
      ND_nlv = 0
      INQUIRE(FILE = 'TARGET_COLL.DAT', EXIST = fexist)
      IF(fexist)THEN
         WRITE(6, *)' '
         WRITE(6, *)'File with collective levels exists for the target'
         WRITE(6, *)'-------------------------------------------------'
         WRITE(6, *)' '
         OPEN(UNIT = 32, FILE = 'TARGET_COLL.DAT', STATUS = 'OLD')
C--------Collective levels automatically selected, pls check
         READ(32, '(a100)')comment
         WRITE(6, '(a100)')comment
C--------2nd line
         READ(32, '(a100)')comment
         WRITE(6, '(a100)')comment
C--------82 208    nucleus is treated as spherical
         READ(32, '(a100)')comment
         WRITE(6, '(a100)')comment
C--------empty line
         READ(32, '(a100)')comment
         WRITE(6, '(a100)')comment
C--------Ncoll Lmax  IDef (Def(1,j),j=2,IDef,2)
         READ(32, '(a100)')comment
         WRITE(6, '(a100)')comment
         DEFormed = .FALSE.
         IF(DEF(1, 0).NE.0.D0)DEFormed = .TRUE.
         IF(DEFormed)THEN
C-----------Number of collective levels
            READ(32, '(3x,3I5,1x,F5.1,1x,6(e10.3,1x))')ND_nlv, LMAxcc,
     &           IDEfcc, ftmp, (D_Def(1, j), j = 2, IDEfcc, 2)
            WRITE(6, '(3x,3I5,1x,F5.1,1x,6(e10.3,1x))')ND_nlv, LMAxcc,
     &            IDEfcc, ftmp, (D_Def(1, j), j = 2, IDEfcc, 2)
         ELSE
            READ(32, '(3x,3I5)')ND_nlv
            WRITE(6, '(3x,3I5)')ND_nlv
         ENDIF
C--------if nd_nlv=0 , then no collective levels will be considered
C--------setting direct to zero
         IF(ND_nlv.EQ.0)THEN
            WRITE(6, *)' WARNING: ND_NLV=0 in COLLECTIVE.LEV file'
            WRITE(6, *)' WARNING: No collective levels considered'
            WRITE(6, *)' WARNING: DIRECT has been set to 0'
C-----------setting direct to zerO
            DIRect = 0
            IFINDCOLL = 2
            RETURN
         ENDIF
C--------empty line
         READ(32, '(a100)')comment
         WRITE(6, '(a100)')comment
C--------'collective levels:'
         READ(32, '(a100)')comment
         WRITE(6, '(a100)')comment
C--------Reading ground state infomation (to avoid overwriting deformation)
C
         READ(32, '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),6e10.3)')
     &        ICOllev(1), D_Elv(1), D_Xjlv(1), D_Lvp(1), IPH(1),
     &        D_Llv(1), D_Klv(1)
         WRITE(6, '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),6e10.3)')
     &         ICOllev(1), D_Elv(1), D_Xjlv(1), D_Lvp(1), IPH(1),
     &         D_Llv(1), D_Klv(1), 0.01
         DO i = 2, ND_nlv
            READ(32,
     &           '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),6e10.3)')
     &           ICOllev(i), D_Elv(i), D_Xjlv(i), D_Lvp(i), IPH(i),
     &           D_Llv(i), D_Klv(i), D_Def(i, 2)
            WRITE(6,
     &            '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),6e10.3)')
     &            ICOllev(i), D_Elv(i), D_Xjlv(i), D_Lvp(i), IPH(i),
     &            D_Llv(i), D_Klv(i), D_Def(i, 2)
         ENDDO
         CLOSE(32)
         IFINDCOLL = 0
         RETURN
      ENDIF
      ia = A(0)
      iz = Z(0)
      DEFormed = .FALSE.
      IF(ABS(DEF(1,0)).GT.0.1)DEFormed = .TRUE.
      i20p = 0
      i21p = 0
      i0p = 0
      i4p = 0
      i6p = 0
      i8p = 0
      i1m = 0
      i3m = 0
      i5m = 0
      i22p = 0
      i41p = 0
      i31p = 0
      ND_nlv = 0
      ierr = 1
C
C-----constructing input and filenames
C
      IF(.NOT.FILevel)THEN
         WRITE(ctmp3, '(I3.3)')iz
         finp = 'z'//ctmp3//'.dat'
         OPEN(13, FILE = '../RIPL-2/levels/'//finp, STATUS = 'OLD',
     &        ERR = 600)
      ELSE
         REWIND(13)
      ENDIF
 100  READ(13, '(A5,6I5,2f12.6)', END = 600)chelem, iar, izr, nlvr,
     &     ngamr, nmax, itmp2, qn
      IF(ia.NE.iar .OR. iz.NE.izr)THEN
         DO ilv = 1, nlvr + ngamr
            READ(13, '(A1)')dum
         ENDDO
         GOTO 100
      ENDIF
      BACKSPACE(13)
      NLVs=0
      OPEN(UNIT = 32, FILE = 'TARGET.LEV', STATUS = 'UNKNOWN')
      READ(13, '(A80)')ch_iuf
      WRITE(32, '(A80)')ch_iuf
      DO ilv = 1, nlvr
         READ(13, '(I3,1X,F10.6,1X,F5.1,I3,1X,E10.2,I3)')itmp, elvr,
     &        xjlvr, lvpr, t12, ndbrlin
C        RCN 09/2004, NLVs limited by binding energy
         IF(elvr.LT.qn) THEN
         NLVs=NLVs+1
           WRITE(32, '(I3,1X,F10.6,1X,F5.1,I3,1X,E10.2,I3)')itmp, elvr,
     &         xjlvr, lvpr, t12, 0
         ENDIF
         DO nbr = 1, ndbrlin
            READ(13, '(A1)')dum
         ENDDO
      ENDDO
C
C     Maximum number of levels scanned for collectivity = 49
C       99 = 50 + 49 which is the maximum number allowed to be printed in I2 format
C
      NLVs=min(NLVs,49)

      DO ilv = 1, nlvr + ngamr
         BACKSPACE(13)
      ENDDO
      IF(.NOT.FILevel)CLOSE(13)
      REWIND(32)
      READ(32, '(A1)')dum
C-----levels for target NNUC copied to file TARGET.lev
      LMAxcc = 4
      IDEfcc = 4
      DO ilv = 1, NDCOLLEV
         DO j = 2, IDEfcc, 2
            D_Def(ilv, j) = 0
         ENDDO
      ENDDO
C
      beta2 = 0.D0
      beta3 = 0.D0
      OPEN(84, FILE = '../RIPL-2/optical/om-data/om-deformations.dat',
     &     STATUS = 'old', ERR = 200)
      READ(84, '(///)')    ! Skipping first 4 title lines
      DO i = 1, 1700
C        README file format (2i4,1x,a2,1x,f10.6,1x,f4.1,i3,i2,1x,f10.6,2x,a13)
         READ(84, '(2I4,4x,f10.6,1x,f4.1,i3,3x,f10.6,2x,a6)', END = 300,
     &        ERR = 300)nztmp, natmp, etmp, jtmp, iptmp, betatmp, reftmp
         IF(nztmp.EQ.iz .AND. natmp.EQ.ia .AND. jtmp.EQ.2.D0 .AND.
     &      iptmp.EQ. + 1 .AND. reftmp.EQ.'Raman2')beta2 = betatmp
         IF(nztmp.EQ.iz .AND. natmp.EQ.ia .AND. jtmp.EQ.3.D0 .AND.
     &      iptmp.EQ. - 1 .AND. reftmp.EQ.'Kibedi')beta3 = betatmp
      ENDDO
      GOTO 300
 200  WRITE(6, *)' WARNING: ',
     &   '../RIPL-2/optical/om-data/om-deformations.dat file not found '
      WRITE(6, *)' WARNING: ',
     &       'Default dynamical deformations 0.15(2+) and 0.05(3-) used'
      GOTO 400
 300  CLOSE(84)
      IF(beta2.NE.0.D0 .OR. beta3.NE.0.D0)THEN
         WRITE(6, '(/1x,A34/1x,A11,F7.3,A13,F7.4)')
     &         'EXPERIMENTAL DEFORMATION (RIPL-2):', 'BETA (2+) =',
     &         beta2, '  BETA (3-) =', beta3
         IF(DEFormed)THEN
            WRITE(6, *)'BETA2 ASSUMED AS GS BAND DEFORMATION'
            WRITE(6, *)
         ENDIF
      ENDIF
      IF(beta2.EQ.0.D0)THEN
         WRITE(6, *)' WARNING: ',
     &    'E(2+) level is not contained in Raman 2001 database (RIPL-2)'
         WRITE(6, *)' WARNING: ',
     &            'Default dynamical deformations 0.15(2+) will be used'
       beta2 = 0.15
      ENDIF
      IF(beta3.EQ.0.D0)THEN
         WRITE(6, *)' WARNING: ',
     &        'E(3-) level is not contained in Kibedi database (RIPL-2)'
         WRITE(6, *)' WARNING: ',
     &            'Default dynamical deformations 0.05(3-) will be used'
       beta3 = 0.05
      ENDIF
C400  DO ilv = 1, nlvr
 400  DO ilv = 1, NLVs
         READ(32, '(I3,1X,F10.6,1X,F5.1,I3,1X,E10.2,I3)')itmp, elvr,
     &        xjlvr, lvpr, t12, ndbrlin
         IF(ilv.EQ.1)THEN
            delta_k = 2.D0
            IF(xjlvr.NE.0.D0)delta_k = 1.D0
            ND_nlv = ND_nlv + 1
            ICOllev(ND_nlv) = ilv
            D_Elv(ND_nlv) = elvr
            D_Lvp(ND_nlv) = lvpr
            D_Xjlv(ND_nlv) = xjlvr
            IPH(ND_nlv) = 0
            D_Llv(ND_nlv) = 0
            D_Klv(ND_nlv) = 0
            D_Def(ND_nlv, 2) = 0.01
            IF(beta2.GT.0.D0 .AND. DEFormed)D_Def(ND_nlv, 2) = beta2
            gspin = xjlvr
            gspar = DBLE(lvpr)
         ENDIF
C
         IF(DEFormed)THEN
C-----------deformed nuclei follow
C-----------assuming all dynamical deformation equal to static deformation
c           beta2 = DEF(1, 0)
            IF(ilv.NE.1)THEN
               IF(i20p.EQ.0 .AND. xjlvr.EQ.(gspin + delta_k) .AND.
     &            lvpr.EQ.gspar)THEN
                  i20p = ilv
                  ND_nlv = ND_nlv + 1
                  ICOllev(ND_nlv) = ilv
                  D_Elv(ND_nlv) = elvr
                  D_Lvp(ND_nlv) = lvpr
                  D_Xjlv(ND_nlv) = xjlvr
                  IPH(ND_nlv) = 0
                  D_Def(ND_nlv, 2) = 0.01
                  GOTO 500
               ENDIF
               IF(i4p.EQ.0 .AND. xjlvr.EQ.(gspin + 2*delta_k) .AND.
     &            lvpr.EQ.gspar)THEN
                  i4p = ilv
                  ND_nlv = ND_nlv + 1
                  ICOllev(ND_nlv) = ilv
                  D_Elv(ND_nlv) = elvr
                  D_Lvp(ND_nlv) = lvpr
                  D_Xjlv(ND_nlv) = xjlvr
                  IPH(ND_nlv) = 0
                  D_Def(ND_nlv, 2) = 0.01
                  GOTO 500
               ENDIF
               IF(i6p.EQ.0 .AND. xjlvr.EQ.(gspin + 3*delta_k) .AND.
     &            lvpr.EQ.gspar)THEN
                  i6p = ilv
                  ND_nlv = ND_nlv + 1
                  ICOllev(ND_nlv) = ilv
                  D_Elv(ND_nlv) = elvr
                  D_Lvp(ND_nlv) = lvpr
                  D_Xjlv(ND_nlv) = xjlvr
                  IPH(ND_nlv) = 0
                  D_Def(ND_nlv, 2) = 0.01
                  GOTO 500
               ENDIF
               IF(i8p.EQ.0 .AND. xjlvr.EQ.(gspin + 4*delta_k) .AND.
     &            lvpr.EQ.gspar)THEN
                  i8p = ilv
                  ND_nlv = ND_nlv + 1
                  ICOllev(ND_nlv) = ilv
                  D_Elv(ND_nlv) = elvr
                  D_Lvp(ND_nlv) = lvpr
                  D_Xjlv(ND_nlv) = xjlvr
                  IPH(ND_nlv) = 0
                  D_Def(ND_nlv, 2) = 0.01
                  GOTO 500
               ENDIF
               IF(i10p.EQ.0 .AND. xjlvr.EQ.(gspin + 5*delta_k) .AND.
     &            lvpr.EQ.gspar)THEN
                  i10p = ilv
                  ND_nlv = ND_nlv + 1
                  ICOllev(ND_nlv) = ilv + 50
                  D_Elv(ND_nlv) = elvr
                  D_Lvp(ND_nlv) = lvpr
                  D_Xjlv(ND_nlv) = xjlvr
                  IPH(ND_nlv) = 0
                  D_Def(ND_nlv, 2) = beta2*0.25
                  GOTO 500
               ENDIF
               IF(i12p.EQ.0 .AND. xjlvr.EQ.(gspin + 6*delta_k) .AND.
     &            lvpr.EQ.gspar)THEN
                  i12p = ilv
                  ND_nlv = ND_nlv + 1
                  ICOllev(ND_nlv) = ilv + 50
                  D_Elv(ND_nlv) = elvr
                  D_Lvp(ND_nlv) = lvpr
                  D_Xjlv(ND_nlv) = xjlvr
                  IPH(ND_nlv) = 0
                  D_Def(ND_nlv, 2) = beta2*0.25
                  GOTO 500
               ENDIF
               IF(i0p.EQ.0 .AND. xjlvr.EQ.0.D0 .AND. lvpr.EQ.1)
     &            THEN
                  i0p = ilv
                  ND_nlv = ND_nlv + 1
                  ICOllev(ND_nlv) = ilv+50
                  D_Elv(ND_nlv) = elvr
                  D_Lvp(ND_nlv) = lvpr
                  D_Xjlv(ND_nlv) = xjlvr
                  IPH(ND_nlv) = 0
                  D_Def(ND_nlv, 2) = 0.01
                  GOTO 500
               ENDIF
               IF(i1m.EQ.0 .AND. xjlvr.EQ.(gspin + nint(delta_k)/2)
     &            .AND. lvpr.EQ.-1*gspar)THEN
                  i1m = ilv
                  ND_nlv = ND_nlv + 1
                  ICOllev(ND_nlv) = ilv+50
                  D_Elv(ND_nlv) = elvr
                  D_Lvp(ND_nlv) = lvpr
                  D_Xjlv(ND_nlv) = xjlvr
                  IPH(ND_nlv) = 0
                  D_Def(ND_nlv, 2) = beta3
                  GOTO 500
               ENDIF
               IF(i3m.EQ.0 .AND. lvpr.EQ.-1*gspar. AND.
     &            xjlvr.EQ.(gspin + nint(delta_k)/2 + delta_k )  )THEN
                  i3m = ilv
                  ND_nlv = ND_nlv + 1
                  ICOllev(ND_nlv) = ilv+50
                  D_Elv(ND_nlv) = elvr
                  D_Lvp(ND_nlv) = lvpr
                  D_Xjlv(ND_nlv) = xjlvr
                  IPH(ND_nlv) = 0
                  D_Def(ND_nlv, 2) = beta3
                  GOTO 500
               ENDIF
               IF(i5m.EQ.0 .AND. lvpr.EQ.-1*gspar. AND.
     &            xjlvr.EQ.(gspin + nint(delta_k)/2 + 2*delta_k )  )THEN
                  i5m = ilv
                  ND_nlv = ND_nlv + 1
                  ICOllev(ND_nlv) = ilv+50
                  D_Elv(ND_nlv) = elvr
                  D_Lvp(ND_nlv) = lvpr
                  D_Xjlv(ND_nlv) = xjlvr
                  IPH(ND_nlv) = 0
                  D_Def(ND_nlv, 2) = beta3
                  GOTO 500
               ENDIF
               IF(i21p.EQ.0 .AND. xjlvr.EQ.(gspin + delta_k)
     &            .AND. lvpr.EQ.gspar)THEN
                  i21p = ilv
                  ND_nlv = ND_nlv + 1
                  ICOllev(ND_nlv) = ilv+50
                  D_Elv(ND_nlv) = elvr
                  D_Lvp(ND_nlv) = lvpr
                  D_Xjlv(ND_nlv) = xjlvr
                  IPH(ND_nlv) = 0
                  D_Def(ND_nlv, 2) = 0.01
                  GOTO 500
               ENDIF
               IF(i22p.EQ.0 .AND. xjlvr.EQ.(gspin + delta_k)
     &            .AND. lvpr.EQ.gspar)THEN
                  i22p = ilv
                  ND_nlv = ND_nlv + 1
                  ICOllev(ND_nlv) = ilv+50
                  D_Elv(ND_nlv) = elvr
                  D_Lvp(ND_nlv) = lvpr
                  D_Xjlv(ND_nlv) = xjlvr
                  IPH(ND_nlv) = 0
                  D_Def(ND_nlv, 2) = 0.01
                  GOTO 500
               ENDIF
C              Additional levels are added for DWBA calculations
               IF(ECUtColl.gt.0. .AND. elvr.gt.ECUtColl) GOTO 600

               IF(ECUtColl.gt.0. .AND. xjlvr.LE.JCUtColl)THEN
                  ND_nlv = ND_nlv + 1
                  ICOllev(ND_nlv) = ilv+50
                  D_Elv(ND_nlv) = elvr
                  D_Lvp(ND_nlv) = lvpr
                  D_Xjlv(ND_nlv) = xjlvr
                  IPH(ND_nlv) = 0
                  D_Def(ND_nlv, 2) = 0.01
                  ierr = 0
                  if(ND_nlv.EQ.NDCOLLEV) goto 600
                  GOTO 500
               ENDIF

               IF(i20p.NE.0 .AND. i4p.NE.0 .AND. i6p.NE.0 .AND.
     &            i8p.NE.0  .AND. i0p.NE.0 .AND. i1m.NE.0 .AND.
     &            i3m.NE.0  .AND. i5m.NE.0 .AND. i21p.NE.0.AND.
     &            i22p.NE.0 .AND. i10p.NE.0 .AND. i12p.NE.0) THEN
                  ierr = 0
                  GOTO 600
               ENDIF
            ENDIF
         ELSE
C-----------spherical nuclei follow
C-----------for spherical target taking dynamical deformation equal to
C-----------0.15 (1PH 2+) and 0.05(1PH 3-) arbitrarily
C           if RIPL-2 deformation file not found OR target nucleus not found in
C           the database we assume arbitrary dynamical deformations
            IF(ilv.EQ.1)THEN
C              ground state deformation for spherical nucleus is 0.0
               D_Def(ND_nlv, 1) = 0.0
               IF(gspin.NE.0.D0)THEN
                ICOllev(ND_nlv) = ilv + 50
                WRITE(6, *)'WARNING: ONLY DWBA CALCULATIONS ALLOWED FOR'
                WRITE(6, *)'WARNING: ODD SPHERICAL NUCLEUS'
                WRITE(6, *)'WARNING: SETTING DIRECT key to 3'
                DIRect = 3
               ENDIF
               GOTO 500
            ENDIF
            IF(i20p.EQ.0 .AND. xjlvr.EQ.2.D0 .AND. lvpr.EQ.1)THEN
               i20p = ilv
               ND_nlv = ND_nlv + 1
               ICOllev(ND_nlv) = ilv
               D_Elv(ND_nlv) = elvr
               D_Lvp(ND_nlv) = lvpr
               D_Xjlv(ND_nlv) = xjlvr
               IPH(ND_nlv) = 1
               D_Def(ND_nlv, 2) = beta2
               GOTO 500
            ENDIF
            IF(i21p.EQ.0 .AND. xjlvr.EQ.2.D0 .AND. lvpr.EQ.1)THEN
               i21p = ilv
               ND_nlv = ND_nlv + 1
               ICOllev(ND_nlv) = ilv
               D_Elv(ND_nlv) = elvr
               D_Lvp(ND_nlv) = lvpr
               D_Xjlv(ND_nlv) = xjlvr
               IPH(ND_nlv) = 2
               D_Def(ND_nlv, 2) = beta2
               GOTO 500
            ENDIF
            IF(i4p.EQ.0 .AND. xjlvr.EQ.4.D0 .AND. lvpr.EQ.1)THEN
               i4p = ilv
               ND_nlv = ND_nlv + 1
               ICOllev(ND_nlv) = ilv
               D_Elv(ND_nlv) = elvr
               D_Lvp(ND_nlv) = lvpr
               D_Xjlv(ND_nlv) = xjlvr
               IPH(ND_nlv) = 2
               D_Def(ND_nlv, 2) = beta2
               GOTO 500
            ENDIF
            IF(i0p.EQ.0 .AND. xjlvr.EQ.0.D0 .AND. lvpr.EQ.1)THEN
               i0p = ilv
               ND_nlv = ND_nlv + 1
               ICOllev(ND_nlv) = ilv
               D_Elv(ND_nlv) = elvr
               D_Lvp(ND_nlv) = lvpr
               D_Xjlv(ND_nlv) = xjlvr
               IPH(ND_nlv) = 2
               D_Def(ND_nlv, 2) = beta2
               GOTO 500
            ENDIF
            IF(i22p.EQ.0 .AND. xjlvr.EQ.2.D0 .AND. lvpr.EQ.1)THEN
               i22p = ilv
               ND_nlv = ND_nlv + 1
               ICOllev(ND_nlv) = ilv+50
               D_Elv(ND_nlv) = elvr
               D_Lvp(ND_nlv) = lvpr
               D_Xjlv(ND_nlv) = xjlvr
               IPH(ND_nlv) = 2
               D_Def(ND_nlv, 2) = 0.05
               GOTO 500
            ENDIF
            IF(i3m.EQ.0 .AND. xjlvr.EQ.3.D0 .AND. lvpr.EQ. - 1)THEN
               i3m = ilv
               ND_nlv = ND_nlv + 1
               ICOllev(ND_nlv) = ilv
               D_Elv(ND_nlv) = elvr
               D_Lvp(ND_nlv) = lvpr
               D_Xjlv(ND_nlv) = xjlvr
               IPH(ND_nlv) = 1
               D_Def(ND_nlv, 2) = beta3
               GOTO 500
            ENDIF
            IF(i41p.EQ.0 .AND. xjlvr.EQ.4.D0 .AND. lvpr.EQ.1)THEN
               i41p = ilv
               ND_nlv = ND_nlv + 1
               ICOllev(ND_nlv) = ilv+50
               D_Elv(ND_nlv) = elvr
               D_Lvp(ND_nlv) = lvpr
               D_Xjlv(ND_nlv) = xjlvr
               IPH(ND_nlv) = 2
               D_Def(ND_nlv, 2) = 0.05
               GOTO 500
            ENDIF
            IF(i31p.EQ.0 .AND. xjlvr.EQ.3.D0 .AND. lvpr.EQ.1)THEN
               i31p = ilv
               ND_nlv = ND_nlv + 1
               ICOllev(ND_nlv) = ilv+50
               D_Elv(ND_nlv) = elvr
               D_Lvp(ND_nlv) = lvpr
               D_Xjlv(ND_nlv) = xjlvr
               IPH(ND_nlv) = 2
               D_Def(ND_nlv, 2) = 0.05
               GOTO 500
            ENDIF

            IF(ECUtColl.gt.0. .AND. elvr.gt.ECUtColl) GOTO 600

C           Additional levels are added for DWBA calculations
            IF(ECUtColl.gt.0. .AND. xjlvr.LE.JCUtColl)THEN
               ND_nlv = ND_nlv + 1
               ICOllev(ND_nlv) = ilv+50
               D_Elv(ND_nlv) = elvr
               D_Lvp(ND_nlv) = lvpr
               D_Xjlv(ND_nlv) = xjlvr
               IPH(ND_nlv) = 1
               D_Def(ND_nlv, 2) = 0.1
               ierr = 0
               if(ND_nlv.EQ.NDCOLLEV) goto 600
               GOTO 500
            ENDIF

            IF(i20p.NE.0 .AND. i3m.NE.0  .AND. i20p.NE.0 .AND.
     &         i21p.NE.0 .AND. i4p.NE.0  .AND. i3m.NE.0 .AND.
     &         i0p.NE.0  .AND. i22p.NE.0 .AND. i41p.NE.0 .AND.
     &         i31p.NE.0)THEN
               ierr = 0
               GOTO 600
            ENDIF
         ENDIF
 500  ENDDO
 600  IFINDCOLL = ierr
      CLOSE(32)
      OPEN(UNIT = 32, FILE = 'TARGET_COLL.DAT', STATUS = 'UNKNOWN')
      IF(.NOT.DEFormed)THEN
         WRITE(6, *)
         WRITE(6, 99001)
     &' Collective levels selected automatically from available target l
     &evels (vibrational model)           '
         WRITE(6, *)
     &   ' N < 50 for coupled levels in CC calculation'
         WRITE(6, '(1x,i3,1x,i3,a35)')iz, ia,
     &                                ' nucleus is treated as spherical'
         WRITE(32, 99001)
     &' Collective levels selected automatically from available target l
     &evels (vibrational model)'
         WRITE(32, *)
     &   ' N < 50 for coupled levels in CC calculation'
         WRITE(32, '(1x,i3,1x,i3,a35)')iz, ia,
     &                                ' nucleus is treated as spherical'
C--------Putting Coupled levels first
         DO i = 2, ND_nlv
            DO j = i + 1, ND_nlv
               IF(ICOllev(j).LT.ICOllev(i))THEN
C--------------------swapping
                     itmp = IPH(i)
                     IPH(i) = IPH(j)
                     IPH(j) = itmp
                     dtmp = D_Def(i, 2)
                     D_Def(i, 2) = D_Def(j, 2)
                     D_Def(j, 2) = dtmp
                     dtmp = D_Lvp(i)
                     D_Lvp(i) = D_Lvp(j)
                     D_Lvp(j) = dtmp
                     dtmp = D_Elv(i)
                     D_Elv(i) = D_Elv(j)
                     D_Elv(j) = dtmp
                     dtmp = D_Xjlv(i)
                     D_Xjlv(i) = D_Xjlv(j)
                     D_Xjlv(j) = dtmp
                     dtmp = ICOllev(i)
                     ICOllev(i) = ICOllev(j)
                     ICOllev(j) = dtmp
                     dtmp = D_Llv(i)
                     D_Llv(i) = D_Llv(j)
                     D_Llv(j) = dtmp
                     dtmp = D_Klv(i)
                     D_Klv(i) = D_Klv(j)
                     D_Klv(j) = dtmp
               ENDIF
            ENDDO
         ENDDO

C-----------Putting one phonon coupled states first for spherical
         DO i = 2, ND_nlv
             if(ICOllev(i).GE.50) cycle
            DO j = i + 1, ND_nlv
                if(ICOllev(j).GE.50) cycle
               IF(IPH(j).LT.IPH(i))THEN
C-----------------swapping
                     itmp = IPH(i)
                     IPH(i) = IPH(j)
                     IPH(j) = itmp
                     dtmp = D_Def(i, 2)
                     D_Def(i, 2) = D_Def(j, 2)
                     D_Def(j, 2) = dtmp
                     dtmp = D_Lvp(i)
                     D_Lvp(i) = D_Lvp(j)
                     D_Lvp(j) = dtmp
                     dtmp = D_Elv(i)
                     D_Elv(i) = D_Elv(j)
                     D_Elv(j) = dtmp
                     dtmp = D_Xjlv(i)
                     D_Xjlv(i) = D_Xjlv(j)
                     D_Xjlv(j) = dtmp
                     dtmp = ICOllev(i)
                     ICOllev(i) = ICOllev(j)
                     ICOllev(j) = dtmp
                     dtmp = D_Llv(i)
                     D_Llv(i) = D_Llv(j)
                     D_Llv(j) = dtmp
                     dtmp = D_Klv(i)
                     D_Klv(i) = D_Klv(j)
                     D_Klv(j) = dtmp
               ENDIF
            ENDDO
         ENDDO

C        ENDIF
         WRITE(6, *)
         WRITE(6, *)'   Ncoll  '
         WRITE(6, '(3x,I5)')ND_nlv
         WRITE(6, *)
         WRITE(6, *)' N   E[MeV]  J   pi Nph L  K  Dyn.Def.'
         WRITE(32, *)
         WRITE(32, *)'   Ncoll  '
         WRITE(32, '(3x,3I5)')ND_nlv
         WRITE(32, *)
         WRITE(32, *)' N   E[MeV]  J   pi Nph L  K  Dyn.Def.'
         DO i = 1, ND_nlv
            ftmp = D_Def(i, 2)
            if(i.eq.1) ftmp = 0.0
            WRITE(32,
     &            '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3)')
     &            ICOllev(i), D_Elv(i), D_Xjlv(i), D_Lvp(i), IPH(i),
     &            D_Llv(i), D_Klv(i), ftmp
            WRITE(6, '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3)'
     &            )ICOllev(i), D_Elv(i), D_Xjlv(i), D_Lvp(i), IPH(i),
     &             D_Llv(i), D_Klv(i), ftmp
         ENDDO
      ELSE
         WRITE(32, 99001)
     &' Collective levels selected automatically from available target l
     &evels (symm.rotational model)       '
         WRITE(32, *)'Dyn.deformations are not used in symm.rot.model (
     &N < 50 for coupled levels )'
         WRITE(32, '(1x,i3,1x,i3,a35)')iz, ia,
     &                                 ' nucleus is treated as deformed'
         WRITE(6, *)
         WRITE(6, 99001)
     &' Collective levels selected automatically from available target l
     &evels (symm.rotational model)       '
         WRITE(6, *)'Dyn.deformations are not used in symm.rot.model (
     &N < 50 for coupled levels )'
         WRITE(6, '(1x,i3,1x,i3,a35)')iz, ia,
     &                                ' nucleus is treated as deformed'

C--------Putting Coupled levels first
         DO i = 2, ND_nlv
            DO j = i + 1, ND_nlv
               IF(ICOllev(j).LT.ICOllev(i))THEN
C--------------------swapping
                     itmp = IPH(i)
                     IPH(i) = IPH(j)
                     IPH(j) = itmp
                     dtmp = D_Def(i, 2)
                     D_Def(i, 2) = D_Def(j, 2)
                     D_Def(j, 2) = dtmp
                     dtmp = D_Lvp(i)
                     D_Lvp(i) = D_Lvp(j)
                     D_Lvp(j) = dtmp
                     dtmp = D_Elv(i)
                     D_Elv(i) = D_Elv(j)
                     D_Elv(j) = dtmp
                     dtmp = D_Xjlv(i)
                     D_Xjlv(i) = D_Xjlv(j)
                     D_Xjlv(j) = dtmp
                     dtmp = ICOllev(i)
                     ICOllev(i) = ICOllev(j)
                     ICOllev(j) = dtmp
                     dtmp = D_Llv(i)
                     D_Llv(i) = D_Llv(j)
                     D_Llv(j) = dtmp
                     dtmp = D_Klv(i)
                     D_Klv(i) = D_Klv(j)
                     D_Klv(j) = dtmp
               ENDIF
            ENDDO
         ENDDO

         WRITE(6, *)
         WRITE(6, *)'   Ncoll  Lmax IDef  Kgs  (Def(1,j),j=2,IDef,2)'
         WRITE(6, '(3x,3I5,1x,F5.1,1x,6(e10.3,1x))')ND_nlv, LMAxcc,
     &         IDEfcc, D_Xjlv(1), (D_Def(1, j), j = 2, IDEfcc, 2)
         WRITE(6, *)
         WRITE(6, *)' N   E[MeV]  J   pi Nph L  K  Dyn.Def.'
         WRITE(32, *)
         WRITE(32, *)'   Ncoll  Lmax IDef  Kgs  (Def(1,j),j=2,IDef,2)'
         WRITE(32, '(3x,3I5,1x,F5.1,1x,6(e10.3,1x))')ND_nlv, LMAxcc,
     &         IDEfcc, D_Xjlv(1), (D_Def(1, j), j = 2, IDEfcc, 2)
         WRITE(32, *)
         WRITE(32, *)' N   E[MeV]  J   pi Nph L  K  Dyn.Def.'
         DO i = 1, ND_nlv
            ftmp =  D_Def(i, 2)
            if(i.eq.1) ftmp = 0.01
            WRITE(32,
     &            '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3)')
     &            ICOllev(i), D_Elv(i), D_Xjlv(i), D_Lvp(i), IPH(i), 0,
     &            0, ftmp
            WRITE(6, '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3)'
     &            )ICOllev(i), D_Elv(i), D_Xjlv(i), D_Lvp(i), IPH(i), 0,
     &             0, ftmp
         ENDDO
      ENDIF
      CLOSE(32)
      RETURN
C-----target nucleus does not have discrete level information available
      ierr = 2
      IFINDCOLL = ierr
      RETURN
99001 FORMAT(A91)
      END
C*==findpot.spg  processed by SPAG 6.20Rc at 12:12 on  7 Jul 2004
C
C
C
      SUBROUTINE FINDPOT(Ki, Ieof, Ipoten)
C
C-----routine to find IPOTEN entry in the RIPL optical model database
C
C
C Dummy arguments
C
      INTEGER Ieof, Ipoten, Ki
C
C Local variables
C
      CHARACTER*3 ctmp
      INTEGER iref
C
      Ieof = 0
      REWIND(Ki)
      READ(Ki, '(i5)')iref
      IF(Ipoten.EQ.iref)THEN
         BACKSPACE(Ki)
         RETURN
      ENDIF
 100  READ(Ki, 99001, END = 200)ctmp
99001 FORMAT(A3)
      IF(ctmp.NE.'+++')GOTO 100
      READ(Ki, *, END = 200)iref
      IF(iref.NE.Ipoten)GOTO 100
      BACKSPACE(Ki)
      RETURN
 200  Ieof = 1
      END
C*==inpfis.spg  processed by SPAG 6.20Rc at 12:12 on  7 Jul 2004
C
C
C
      SUBROUTINE INPFIS(Nnuc)
C
C This subroutine creates the fission.inp  which contains all the fission
C parameters independent of energy.
C
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
      COMMON /CRIT  / TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl,
     &                BET2
      COMMON /PARAM / AP1, AP2, GAMma, DEL, DELp, BF, A23, A2, NLWst
      COMMON /CRITFIS/ ACRtf(2), UCRtf(2), TCRtf(2), DETcrtf(2),
     &                 SCRtf(2), MORtcrt(NFPARAB), MPArcrt(NFPARAB),
     &                 ECOndf(2)
      COMMON /FISSMOD/ ROFism(60, 30, 3), HM(NFTRANS, 3),
     &                 EFDism(NFTRANS, 3), UGRidf(0:NFISENMAX, 3),
     &                 EFBm(3), XMInnm(3), AFIsm(3), DEFbm(3),
     &                 SHCfism(3), DELtafism(3), GAMmafism(3),
     &                 WFIsm(3), BFFm(3),NRBinfism(3)
C
      DOUBLE PRECISION ROFism, HM, EFDism, EFBm, XMInnm, DEFbm, SHCfism,
     &                 DELtafism, GAMmafism, UGRidf, AFIsm
C
C     DOUBLE PRECISION ACR, ACRt, ATIl, mm2, MORtcrt, MPArcrt, DETcrt,
C    &                 ECOnd, SCR, TCRt, UCRt, WFIsm
      DOUBLE PRECISION mm2, MORtcrt, MPArcrt, WFIsm
C
      INTEGER ka, kz, NRBarc, nrmod, BFFm
      CHARACTER*2 simb, symma, symmb
      CHARACTER*20 cara
      CHARACTER*33 cara1
      CHARACTER*55 cara2
C
C-----fundamental fission barrier
C-----FISBAR=0 RIPL-2; HF-BCS theoretical heights for simple and double barriers,
C---- information about curvatures and wells provided by the code
C
      IF(FISbar(Nnuc).EQ.0.)THEN
         OPEN(51, FILE = '../RIPL-2/fission/fis-barrier-etfsi.dat',
     &        STATUS = 'OLD')
         READ(51, '(a100)')simb
         READ(51, '(a100)')simb
         READ(51, '(a100)')simb
         READ(51, '(a100)')simb
 50      READ(51, '(2i4,1x,a2,1x,10f9.2)', END = 100)kz, ka, simb, cin,
     &        hin, ain, EFB(1), bexp, cout, hout, aout, EFB(2), bexp1
         IF(kz.NE.INT(Z(Nnuc)) .OR. ka.NE.INT(A(Nnuc)))GOTO 50
         CLOSE(51)
         NRBar = 2
         IF(EFB(2).EQ.0.)THEN
            WRITE(6, *)' 2nd FISSION BARRIER ENERGY = 0 in RIPL2 etfsi '
            WRITE(6, *)' Setting the number of fission barriers equal 1'
            NRBar = 1
         ENDIF
         NRWel = 0
C------  default values for curvatures
         IF(NRBar.EQ.1)H(1, 1) = 1.
C------  Lynn values
         IF(NRBar.EQ.2)THEN
            IF(ka/2.EQ.INT(ka/2) .AND. kz/2.EQ.INT(kz/2))THEN  ! even-even
               H(1, 1) = 1.04
               H(1, 2) = 0.6
            ENDIF
            IF(ka/2.NE.INT(ka/2))THEN  ! odd A
               H(1, 1) = 0.8
               H(1, 2) = 0.52
            ENDIF
            IF(ka/2.EQ.INT(ka/2) .AND. kz/2.NE.INT(kz/2))THEN  ! odd-odd
               H(1, 1) = 0.65
               H(1, 2) = 0.45
            ENDIF
C-------default values for the second well
            H(1, 3) = 1.
            EFB(3) = 2.
         ENDIF
         GOTO 200
 100     WRITE(6, *)' NO THEORETICAL FISSION BARRIER FOR Z=',
     &              INT(Z(Nnuc)), ' A=', INT(A(Nnuc)), ' IN RIPL-2'
         WRITE(6, *)' CHANGE FISBAR OPTION(NOW=0). EXECUTION TERMINATED'
         STOP 'Fission barrier missing'
      ENDIF
C-----FISBAR(Nnuc)=2. RIPL-2 "experimental values"
C     To be used with care, model dependent !!!!!
 200  IF(FISbar(Nnuc).EQ.2.)THEN
         OPEN(52, FILE = '../RIPL-2/fission/fis-barrier-exp.dat',
     &        STATUS = 'OLD')
         READ(52, '(a100)')simb
         READ(52, '(a100)')simb
         READ(52, '(a100)')simb
         READ(52, '(a100)')simb
 250     READ(52, '(2(1x,i3),1x,a2,2(f8.3,4a),f9.3)', END = 300)kz, ka,
     &        simb, EFB(1), symma, EFB(2), symmb, deltaf
C        below is the original Mihaela's line but this is not a RIPL-2 format
C        &        ka, simb, symma, EFB(1), H(1), symmb, EFB(2), H(2), deltaf
         IF(kz.NE.INT(Z(Nnuc)) .OR. ka.NE.INT(A(Nnuc)))GOTO 250
         CLOSE(52)
         NRBar = 2
         IF(EFB(2).EQ.0.)THEN
            WRITE(6, *)' 2nd FISSION BARRIER ENERGY = 0 in RIPL2 exper.'
            WRITE(6, *)' Setting the number of fission barriers equal 1'
            NRBar = 1
         ENDIF
         NRWel = 0
         GOTO 400
 300     WRITE(6, *)' NO EXPERIMENTAL FISSION BARRIER FOR Z=',
     &              INT(Z(Nnuc)), ' A=', INT(A(Nnuc)), ' IN RIPL-2'
         WRITE(6, *)' CHANGE FISBAR OPTION(NOW=2). EXECUTION TERMINATED'
         STOP 'Fission barrier missing'
      ENDIF
C
C
C----FISBAR(Nnuc)=1. internal library
 400  IF(FISbar(Nnuc).EQ.1.)THEN
         OPEN(81, FILE = '../data/fisbar.dat', STATUS = 'OLD')
 450     READ(81, *, END = 500)kz, ka, NRBar, NRWel,
     &                         (EFB(i), H(1, i), i = 1, NRBar)
         IF(kz.NE.INT(Z(Nnuc)) .OR. ka.NE.INT(A(Nnuc)))GOTO 450
         CLOSE(81)
         GOTO 600
 500     WRITE(6, *)' NO  FISSION BARRIER FOR Z=', INT(Z(Nnuc)), ' A=',
     &              INT(A(Nnuc)), ' IN INTERNAL LIBRARY (fisbar.dat)'
         WRITE(6, *)' CHANGE FISBAR OPTION(NOW=1). EXECUTION TERMINATED'
         STOP 'Fission barrier missing'
C
      ENDIF
C
C     Default value for curvatures and protection !!
 600  DO i = 1, NRBar
         IF(H(1, i).EQ.0)H(1, i) = 1.
      ENDDO
C
      IF(NRWel.NE.(NRBar - 1)/2)FISopt(Nnuc) = 0.
      NRBarc = NRBar - NRWel
C
C----------------------input fundamental fission barrier *** done
C------- discrete barriers---------------------------------------
      DO ibar = 1, NRBar
         EFDis(1, ibar) = 0.
         IF(A(Nnuc)/2.EQ.INT(A(Nnuc)/2))THEN
            SFDis(1, ibar)=0.0
         ELSE
            SFDis(1, ibar)=0.5
         ENDIF
         IF(XJLv(1,Nnuc).ge.0.)SFDis(1, ibar) = XJLv(1, Nnuc)
         IPFdis(1, ibar)=1
         IF(LVP(1,Nnuc).ne.IPFdis(1, ibar))
     &        IPFdis(1, ibar) = LVP(1, Nnuc)
         IF(FISdis(Nnuc).EQ.0.)NRFdis(ibar) = 1
      ENDDO
C
C---- the values below are not physical, they only enter the fission input
C---- file to help the user to edit and introduce his own values
      IF(FISdis(Nnuc).EQ.1.)THEN
         DO ib = 1, 3
            EFDis(2, ib) = 0.1
            EFDis(3, ib) = 0.2
            EFDis(4, ib) = 0.3
         ENDDO
C
         IF(A(Nnuc)/2.EQ.INT(A(Nnuc)/2))THEN
            DO ibar = 1, NRBar
               NRFdis(ibar) = 4
               SFDis(2, ibar) = 0.
               IPFdis(2, ibar) = -1
               SFDis(3, ibar) = 2.
               IPFdis(3, ibar) = 1
               SFDis(4, ibar) = 4.
               IPFdis(4, ibar) = 1
            ENDDO
         ENDIF
         IF(A(Nnuc)/2.NE.INT(A(Nnuc)/2))THEN
            DO ibar = 1, NRBar
               NRFdis(ibar) = 4
               SFDis(2, ibar) = 0.5
               IPFdis(2, ibar) = -1
               SFDis(3, ibar) = 1.5
               IPFdis(3, ibar) = 1
               SFDis(4, ibar) = 3.5
               IPFdis(4, ibar) = 1
            ENDDO
         ENDIF
      ENDIF
C
      DO ibar = 1, NRBar
         DO k = 1, NRFdis(ibar)
            H(k, ibar) = H(1, ibar)
         ENDDO
      ENDDO
C
      IF(FISmod(Nnuc).GT.0.)THEN
         nrmod = INT(FISmod(Nnuc)) + 1
         DO m = 1, nrmod
            EFBm(m) = EFB(2)
            DO k = 1, 4
               HM(k, m) = H(k, 2)
               EFDism(k, m) = EFDis(k, 2)
            ENDDO
         ENDDO
      ENDIF
C
      CALL DEFO_FIS(Nnuc)
C
      FIScon = 2.
      mm2 = 0.24*A(Nnuc)**(2./3.)
      r0 = 1.4
      nrbarc1 = NRBarc
C
C================  level densities at saddles  ===============================
      DO ibars = 1, NRBar
         IF(ibars.LE.2)THEN
            SHC(Nnuc) = SHCfis(ibars)
            DELp = DELtafis(ibars)
            GAMma = GAMmafis(ibars)
            CALL ROEMP(Nnuc, 1.0D0, AFIs(ibars))
            ACRtf(ibars) = ACRt
            UCRtf(ibars) = UCRt
            TCRtf(ibars) = TCRt
            ECOndf(ibars) = ECOnd
            DETcrtf(ibars) = DETcrt
            SCRtf(ibars) = SCR
         ENDIF
         MPArcrt(ibars) = 6*ACRt*mm2*(1. - (2./3.)*DEFfis(ibars))/PI**2
         IF(MPArcrt(ibars).LT.2.)MPArcrt(ibars) = 2.
                                                    !!!!!!!!!!!
         MORtcrt(ibars) = 0.0095616*r0**2*A(Nnuc)**(5./3.)
     &                    *(1. + (1./3.)*DEFfis(ibars))
         HJ(Nnuc, ibars) = 0.5*(1.0/MPArcrt(ibars) - 1.0/MORtcrt(ibars))
         HJ(Nnuc, ibars) = 0.5/MORtcrt(ibars)
         IF(HJ(Nnuc, ibars).LE.0.)HJ(Nnuc, ibars) = 0.0001
C
         IF(NRBarc.EQ.3 .AND. ibars.EQ.2)THEN
            MPArcrt(2) = 6*ACRt*mm2*(1. - (2./3.)*DEFeq)/PI**2
            IF(MPArcrt(2).LT.2.)MOMparcrt = 2.
                                             !!!!!!!!!!!
            MORtcrt(2) = 0.0095616*r0**2*A(Nnuc)**(5./3.)
     &                   *(1. + (1./3.)*DEFeq)
         ENDIF
      ENDDO
C
C------- writing data in FISSION.INP
      WRITE(79, '(a8)')'Isotope:'
      WRITE(79, '(a40)')'----------------------------------------'
      WRITE(79, '(4x,a2,i3,2x,a2,i3)')'Z=', INT(Z(Nnuc)), 'A=',
     &                                INT(A(Nnuc))
      WRITE(79, '(a40)')'----------------------------------------'
      IF(FISbar(Nnuc).EQ.0.)cara = 'RIPL-2  Theor values'
      IF(FISbar(Nnuc).EQ.2.)cara = 'RIPL-1  Exp. values'
      IF(FISbar(Nnuc).EQ.1.)cara = 'Internal library'
      WRITE(79, '(a8,f2.0,a28,a20)')'FISBAR =', FISbar(Nnuc),
     &                              '  Fundamental barriers from ', cara
      WRITE(79, '(a15,i1,a15,i1)')' Nr.parabolas =', NRBar,
     &                            '      Nr.wells=', NRWel
      WRITE(79, '(a8,f2.0)')'FISMOD =', FISmod(Nnuc)
      WRITE(79, *)'  '
C
      IF(NRBar.EQ.1)THEN
         WRITE(79, '(a)')'    Va      ha    (in Mev) '
         WRITE(79, '(2f8.3)')EFB(1), H(1, 1)
         WRITE(79, *)' '
         WRITE(79, '(2a10)')'h2/2J(A)', '(in MeV)'
         WRITE(79, '(f9.4)')HJ(Nnuc, 1)
         WRITE(79, *)' '
         WRITE(79, '(a10)')'Beta2(A)'
         WRITE(79, '(f9.4)')DEFfis(1)
         WRITE(79, *)' '
      ENDIF
C
      IF(NRBar.EQ.2)THEN
         IF(FISmod(Nnuc).EQ.0.)THEN
            WRITE(79, '(a)')
     &                    '    Va      ha      Vb      hb     (in Mev) '
            WRITE(79, '(4f8.3)')(EFB(i), H(1, i), i = 1, NRBar)
         ENDIF
         IF(FISmod(Nnuc).EQ.1.)THEN
            WRITE(79, '(a,1x,a)')
     &         '       Va      ha     Vb(SL)   hb(SL)   Vb(ST)   hb(ST)'
     &         , '  (in Mev) '
            WRITE(79, '(6f9.3,15x)')EFB(1), H(1, 1), EFBm(1), HM(1, 1),
     &                              EFBm(2), HM(1, 2)
         ENDIF
         IF(FISmod(Nnuc).EQ.2.)THEN
            WRITE(79, '(a,1x,a)')
     &         '      Va      ha     Vb(SL)    hb(SL)  Vb(ST1)  hb(ST1)'
     &         , '  Vb(ST2)  hb(ST2)  (in Mev) '
            WRITE(79, '(8f9.3,15x)')EFB(1), H(1, 1), EFBm(1), HM(1, 1),
     &                              EFBm(2), HM(1, 2), EFBm(3), HM(1, 3)
         ENDIF
         WRITE(79, *)' '
         WRITE(79, '(3a10)')'h2/2J(A)', 'h2/2J(B)', '(in MeV)'
         WRITE(79, '(2f9.4)')(HJ(Nnuc, i), i = 1, NRBar)
         WRITE(79, *)' '
         WRITE(79, '(2a10)')'Beta2(A)', 'Beta2(B)'
         WRITE(79, '(2f9.4)')(DEFfis(i), i = 1, NRBar)
         WRITE(79, *)' '
      ENDIF
C
      IF(NRBar.EQ.3)THEN
         IF(FISmod(Nnuc).EQ.0.)THEN
            WRITE(79, '(a,1x,a)')
     &       '    Va      ha      Vb      hb      Vi      hi  (in Mev) '
            WRITE(79, '(6f8.3,15x)')(EFB(i), H(1, i), i = 1, NRBar)
         ENDIF
         IF(FISmod(Nnuc).EQ.1.)THEN
            WRITE(79, '(a,1x,a)')
     &         '       Va      ha     Vb(SL)   hb(SL)   Vb(ST)   hb(ST)'
     &         , '    Vi      hi  (in Mev) '
            WRITE(79, '(8f9.3,15x)')EFB(1), H(1, 1), EFBm(1), HM(1, 1),
     &                              EFBm(2), HM(1, 2), EFB(3), H(1, 3)
         ENDIF
         IF(FISmod(Nnuc).EQ.2.)THEN
            WRITE(79, '(a,1x,a)')
     &         '      Va      ha     Vb(SL)    hb(SL)  Vb(ST1)  hb(ST1)'
     &         , '  Vb(ST2)  hb(ST2)   Vi      hi  (in Mev) '
            WRITE(79, '(10f9.3,15x)')EFB(1), H(1, 1), EFBm(1), HM(1, 1),
     &                               EFBm(2), HM(1, 2), EFBm(3),
     &                               HM(1, 3), EFB(3), H(1, 3)
         ENDIF
         WRITE(79, *)' '
         WRITE(79, '(4a10)')'h2/2J(A)', 'h2/2J(B)', 'h2/2J(I)',
     &                      '(in MeV)'
         WRITE(79, '(3f9.4)')(HJ(Nnuc, i), i = 1, NRBar)
         WRITE(79, *)' '
         WRITE(79, '(3a10)')'Beta2(A)', 'Beta2(B)', 'Beta2(I)'
         WRITE(79, '(3f9.4)')(DEFfis(i), i = 1, NRBar)
         WRITE(79, *)'  '
      ENDIF
C
      IF(NRBar.EQ.5)THEN
         WRITE(79, '(a,1x,a)')
     &'    Va      ha      Vb      hb      Vc       hc      Vi
     &hi      Vo      ho  (in Mev) '
         WRITE(79, '(10f8.3,15x)')(EFB(i), H(1, i), i = 1, NRBar)
         WRITE(79, *)' '
         WRITE(79, '(6a10)')'h2/2J(A)', 'h2/2J(B)', 'h2/2J(C)',
     &                      'h2/2J(I)', 'h2/2J(O)', '(in MeV)'
         WRITE(79, '(5f9.4)')(HJ(Nnuc, i), i = 1, NRBar)
         WRITE(79, *)' '
         WRITE(79, '(6a10)')'Beta2(A)', 'Beta2(B)', 'Beta2(C)',
     &                      'Beta2(I)', 'Beta2(O)', '        '
         WRITE(79, '(5f9.4)')(DEFfis(i), i = 1, NRBar)
         WRITE(79, *)' '
      ENDIF
C
      IF(FISopt(Nnuc).EQ.0.)cara1 = ' Subbarrier effects neglected '
      IF(FISopt(Nnuc).GT.0.)cara1 = ' Subbarrier effects considered'
      WRITE(79, '(a8,f2.0,a36)')'FISOPT=', FISopt(Nnuc), cara1
      WRITE(79, *)' '
C
      IF(FISopt(Nnuc).GT.0.)THEN
         WRITE(79, *)'  '
         WRITE(79, '(a161)')
     &'Parabolic energy dependent imaginary potential in the isomeric we
     &ll Wimag = W0 + W1*E + W2*E^2, where E is excitation energy with r
     &espect to the isomeric well'
         WIMag(1) = 1.
         WIMag(2) = 0.
         WIMag(3) = 0.
         WRITE(79, *)'      W0         W1         W2'
         WRITE(79, '(3f11.4)')(WIMag(i), i = 1, 3)
         WRITE(79, *)
      ENDIF
C
      DO ibar = 1, NRBar
         WRITE(79, '(a39,I2,a2,I2)')
     &                            'Number of discrete states at barrier'
     &                            , ibar, ' =', NRFdis(ibar)
         WRITE(79, *)'Jdis  Pidis   Edis    homega'
         DO nr = 1, NRFdis(ibar)
            IF(FISmod(Nnuc).EQ.0. .OR.
     &         (FISmod(Nnuc).GT.0. .AND. ibar.NE.2))
     &         WRITE(79, '(1x, 1f3.1, 1x, 1i4, 2x, 1f8.3, 1x, 1f8.3)')
     &         SFDis(nr, ibar), IPFdis(nr, ibar), EFDis(nr, ibar),
     &         H(nr, ibar)
            IF(FISmod(Nnuc).EQ.1. .AND. ibar.EQ.2)
     &         WRITE(79, '(1x, 1f3.1, 1x, 1i4,1x, 4f9.3)')
     &         SFDis(nr, ibar), IPFdis(nr, ibar), EFDism(nr, 1),
     &         HM(nr, 1), EFDism(nr, 2), HM(nr, 2)
            IF(FISmod(Nnuc).EQ.2. .AND. ibar.EQ.2)
     &         WRITE(79, '(1x, 1f3.1, 1x, 1i4,1x, 6f9.3)')
     &         SFDis(nr, ibar), IPFdis(nr, ibar), EFDism(nr, 1),
     &         HM(nr, 1), EFDism(nr, 2), HM(nr, 2), EFDism(nr, 3),
     &         HM(nr, 3)
         ENDDO
      ENDDO
      WRITE(79, *)'  '
C
      IF(NRBarc.EQ.3)THEN
         WRITE(79, *)' Veq(MeV)  Heq(MeV)  defeq'
         WRITE(79, '(3f9.3)')VEQ, HOEq, DEFeq
      ENDIF
C
      nrbarc1 = NRBarc
      IF(NRBarc.EQ.3)nrbarc1 = 2
C
      IF(FISden(Nnuc).EQ.0.)THEN
         cara2 =
     &          'Level densities at the saddle points taken from RIPL-2'
         WRITE(79, *)
         WRITE(79, '(a7,f2.0,a55)')'FISDEN=', FISden(Nnuc), cara2
         WRITE(79, *)
      ENDIF
C
      IF(FISden(Nnuc).EQ.1.)THEN
         cara2 = 'Level densities at the saddle points EMPIRE specific'
         WRITE(79, *)'  '
         WRITE(79, '(a7,f2.0,a55)')'FISDEN=', FISden(Nnuc), cara2
         WRITE(79, *)'  '
C
C        bff=1 axial, mass symmetry
C        bff=2 axial asymmetry,mass symmetry
C        bff=3 axial symmetry,mass asymmetry
C        bff=4 axial asymmetry,mass asymmetry
C
C        Values should be taken as recommended in RIPL-2 handbook
C
         BFF(1) = 2.
         BFF(2) = 3.
         SHCfis(1) = 2.5
         SHCfis(2) = 0.6
         DELtafis(1) = 12./SQRT(A(Nnuc))
         DELtafis(2) = 12./SQRT(A(Nnuc))
         GAMmafis(1) = 0.4*A(Nnuc)**( - 1./3.)
         GAMmafis(2) = 0.4*A(Nnuc)**( - 1./3.)
         AFIs(1) = 0.
         AFIs(2) = 0.
         IF(FISmod(Nnuc).GT.0.)THEN
            BFFm(1) = 1
            BFFm(2) = 3
            BFFm(3) = 3
            DO m = 1, nrmod
               SHCfism(m) = SHCfis(2)
               DELtafism(m) = DELtafis(2)
               GAMmafism(m) = GAMmafis(2)
               AFIsm(m) = AFIs(2)
            ENDDO
         ENDIF
C
         WRITE(79, *)
     &'Nuclear shape asymmetry at saddles, shell correct,    delta, gamm
     &a, asaf '
C
         DO nr = 1, nrbarc1
            IF(FISmod(Nnuc).EQ.0. .OR.
     &         (FISmod(Nnuc).GT.0. .AND. nr.NE.2))
     &         WRITE(79, '(1x, A8, 1x, I1,4x,I1, 4f9.3)')'Barrier', nr,
     &         BFF(nr), SHCfis(nr), DELtafis(nr), GAMmafis(nr), AFIs(nr)
            IF(FISmod(Nnuc).GT.0. .AND. nr.EQ.2)THEN
               DO m = 1, nrmod
                  WRITE(79, '(1x, A8, 1x, I1, 2x, I1, 1x, I1, 4f9.3)')
     &                  'Barrier', nr, m, BFFm(m), SHCfism(m),
     &                  DELtafism(m), GAMmafism(m), AFIsm(2)
               ENDDO
            ENDIF
         ENDDO
      ENDIF
C-------- the coefficients of a linear energy dependent factor adjusting
C     the fission level densities;
      DO nr = 1, nrbarc1
         ENH_ld(1, nr) = 1.
         ENH_ld(2, nr) = 0.
         ENH_ld(3, nr) = 0.
      ENDDO
C
      WRITE(79, *)'   '
      WRITE(79, '(a87)')
     &'Coefficients of a linear energy dependent factor adjusting the fi
     &ssion level densities'
      WRITE(79, '(a87)')
     &'(a0 + a1*E + a2*E**2) * rho(E,J) where E stands for excitation en
     &ergy above the barrier'
      WRITE(79, *)'                a0       a1       a2 '
C
      DO nr = 1, nrbarc1
         WRITE(79, '(1x, A8, 1x, I1, 3f9.3)')'Barrier', nr,
     &         ENH_ld(1, nr), ENH_ld(2, nr), ENH_ld(3, nr)
      ENDDO
C
      END
C*==read_inpfis.spg  processed by SPAG 6.20Rc at 12:14 on  7 Jul 2004
C
C---------------------------------------------------------------------
      SUBROUTINE READ_INPFIS(Nnuc)
C
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C
      COMMON /FISSMOD/ ROFism(60, 30, 3), HM(NFTRANS, 3),
     &                 EFDism(NFTRANS, 3), UGRidf(0:NFISENMAX, 3),
     &                 EFBm(3), XMInnm(3), AFIsm(3), DEFbm(3),
     &                 SHCfism(3), DELtafism(3), GAMmafism(3),
     &                 WFIsm(3), BFFm(3),NRBinfism(3)
C
      DOUBLE PRECISION ROFism, HM, EFDism, EFBm, XMInnm, DEFbm, WFIsm,
     &                 SHCfism, DELtafism, GAMmafism, UGRidf, AFIsm
C
C     DOUBLE PRECISION ACR, ACRt, ATIl, DETcrt, ECOnd, SCR, TCRt, UCRt
C
C
      INTEGER nrbarc1, iz, ia, BFFm
      CHARACTER*2 carz, cara3
      CHARACTER*8 cara8
      CHARACTER*33 cara1
      CHARACTER*40 line
C
C
      OPEN(79, FILE = 'FISSION.INP', STATUS = 'OLD')
 100  READ(79, '(A8)')cara8
C
      IF(cara8.NE.'Isotope:')GOTO 100
      READ(79, '(a40)')line
      READ(79, '(4x,a2,i3,2x,a2,i3)')carz, iz, cara3, ia
      IF(carz.EQ.'Z=' .AND. iz.NE.INT(Z(Nnuc)) .OR. ia.NE.INT(A(Nnuc)))
     &   GOTO 100
C
      READ(79, '(a40)')line
      READ(79, '(a8,f2.0,a28,a20)')cara8, FISbar(Nnuc)
      READ(79, '(15x,i1,15x,i1)')NRBar, NRWel
      READ(79, '(a8,f2.0)')cara8, FISmod(Nnuc)
      READ(79, *)
      READ(79, *)
C
      nrmod = INT(FISmod(Nnuc)) + 1
C
      IF(FISmod(Nnuc).EQ.0.)READ(79, *)(EFB(i), H(1, i), i = 1, NRBar)
      IF(FISmod(Nnuc).EQ.1. .AND. NRWel.EQ.1)READ(79, *)EFB(1), H(1, 1),
     &   EFBm(1), HM(1, 1), EFBm(2), HM(1, 2), EFB(3), H(1, 3)
C
C
      IF(FISmod(Nnuc).EQ.1. .AND. NRWel.EQ.0)READ(79, *)EFB(1), H(1, 1),
     &   EFBm(1), HM(1, 1), EFBm(2), HM(1, 2)
      IF(FISmod(Nnuc).EQ.2. .AND. NRWel.EQ.1)READ(79, *)EFB(1), H(1, 1),
     &   EFBm(1), HM(1, 1), EFBm(2), HM(1, 2), EFBm(3), HM(1, 3), EFB(3)
     &   , H(1, 3)
C
      IF(FISmod(Nnuc).EQ.2. .AND. NRWel.EQ.0)READ(79, *)EFB(1), H(1, 1),
     &   EFBm(1), HM(1, 1), EFBm(2), HM(1, 2), EFBm(3), HM(1, 3)
      READ(79, *)
      READ(79, *)
      READ(79, *)(HJ(Nnuc, i), i = 1, NRBar)
      READ(79, *)
      READ(79, *)
      READ(79, *)(DEFfis(i), i = 1, NRBar)
      READ(79, *)
C
      READ(79, '(a8,f2.0,a36)')cara8, FISopt(Nnuc), cara1
      READ(79, *)
C
      IF(FISopt(Nnuc).GT.0.)THEN
         READ(79, *)
         READ(79, *)
         READ(79, *)
         READ(79, '(3f11.4)')(WIMag(i), i = 1, 3)
         READ(79, *)
      ENDIF
C
      DO ibar = 1, NRBar
         READ(79, '(a39,I2,a2,I2)')line, ibaro, cara8, NRFdis(ibar)
         READ(79, *)
         DO nr = 1, NRFdis(ibar)
            IF(FISmod(Nnuc).EQ.0. .OR.
     &         (FISmod(Nnuc).GT.0. .AND. ibar.NE.2))READ(79, *)
     &         SFDis(nr, ibar), IPFdis(nr, ibar), EFDis(nr, ibar),
     &         H(nr, ibar)
            IF(FISmod(Nnuc).EQ.1. .AND. ibar.EQ.2)READ(79, *)
     &         SFDis(nr, ibar), IPFdis(nr, ibar), EFDism(nr, 1),
     &         HM(nr, 1), EFDism(nr, 2), HM(nr, 2)
            IF(FISmod(Nnuc).EQ.2. .AND. ibar.EQ.2)READ(79, *)
     &         SFDis(nr, ibar), IPFdis(nr, ibar), EFDism(nr, 1),
     &         HM(nr, 1), EFDism(nr, 2), HM(nr, 2), EFDism(nr, 3),
     &         HM(nr, 3)
         ENDDO
      ENDDO
      READ(79, *)
      NRBarc = NRBar - NRWel
C
      IF(NRBarc.EQ.3)THEN
         READ(79, *)
         READ(79, '(3f9.3)')VEQ, HOEq, DEFeq
      ENDIF
      READ(79, *)
C
      nrbarc1 = NRBarc
      IF(NRBarc.EQ.3)nrbarc1 = 2
C
      READ(79, '(a7,f2.0)')cara8, FISden(Nnuc)
      READ(79, *)
C
      IF(FISden(Nnuc).EQ.1.)THEN
         READ(79, *)
         DO ibar = 1, nrbarc1
            IF(FISmod(Nnuc).EQ.0. .OR.
     &         (FISmod(Nnuc).GT.0. .AND. ibar.NE.2))
     &         READ(79, '(1x, A8, 1x, I1,4x, I1, 4f9.3)')cara8, i,
     &         BFF(ibar), SHCfis(ibar), DELtafis(ibar), GAMmafis(ibar),
     &         AFIs(ibar)
            IF(FISmod(Nnuc).GT.0. .AND. ibar.EQ.2)THEN
               DO m = 1, nrmod
                  READ(79, '(10x, I1, 2x, I1, 1x, I1, 4f9.3)')i, mm,
     &                 BFFm(m), SHCfism(m), DELtafism(m), GAMmafism(m),
     &                 AFIsm(m)
               ENDDO
            ENDIF
         ENDDO
      ENDIF
C
      READ(79, *)
      READ(79, *)
      READ(79, *)
      READ(79, *)
C
      DO nr = 1, nrbarc1
         READ(79, '(1x, A8, 1x, I1, 3f9.3)')cara8, i, ENH_ld(1, nr),
     &        ENH_ld(2, nr), ENH_ld(3, nr)
      ENDDO
      CLOSE(79)
C
      END
C*==defo_fis.spg  processed by SPAG 6.20Rc at 12:14 on  7 Jul 2004
C-----------------------------------------------------------------------
      SUBROUTINE DEFO_FIS(Nnuc)
C
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
      DOUBLE PRECISION ho(NFPARAB), epsil(NFPARAB), ejoin(2*NFPARAB),
     &                 vjj(NFPARAB), smiu, vvqq
C-----deformations at saddles and wells and matching points--------------------
      smiu = 0.1643167*A(Nnuc)**(5./6.)
      IF(NRBar.EQ.1)THEN
         DEFfis(1) = SQRT(EFB(1))/(smiu*H(1, 1)) + DEF(1, Nnuc)
         GOTO 100
      ENDIF
      IF(NRBarc.EQ.2)THEN
         nrbarm = 3
         IF(NRWel.EQ.0)THEN
            EFB(3) = 2.
            H(1, 3) = 1.
         ENDIF
         vjj(1) = EFB(1)
         vjj(2) = EFB(3)
         vjj(3) = EFB(2)
         ho(1) = H(1, 1)
         ho(2) = H(1, 3)
         ho(3) = H(1, 2)
      ENDIF
      IF(NRBarc.EQ.3)THEN
         nrbarm = 5
         IF(NRWel.EQ.0)THEN
            EFB(4) = 2.
            H(1, 4) = 1.
            EFB(5) = 5.
            H(1, 5) = 1.2
         ENDIF
         IF(NRWel.EQ.2)THEN
            vjj(1) = EFB(1)
            vjj(2) = EFB(4)
            vjj(3) = EFB(2)
            vjj(4) = EFB(5)
            vjj(5) = EFB(3)
            ho(1) = H(1, 1)
            ho(2) = H(1, 4)
            ho(3) = H(1, 2)
            ho(4) = H(1, 5)
            ho(5) = H(1, 3)
         ENDIF
      ENDIF
      DO i = 1, nrbarm
         epsil(i) = 0.
         ejoin(i) = 0.
         DEFfis(i) = 0.
      ENDDO
      epsil(1) = SQRT(vjj(1))/(smiu*ho(1)) + DEF(1, Nnuc)
      ejoin(2) = epsil(1)
     &           + SQRT((vjj(1) - vjj(2))/(1.D0 + (ho(1)/ho(2))**2))
     &           /(smiu*ho(1))
      ejoin(1) = 2*epsil(1) - ejoin(2)
C
      DO k = 2, nrbarm
         ejoin(2*k - 1) = ejoin(2*(k - 1))
         epsil(k) = ejoin(2*(k - 1)) + (ho(k - 1)/ho(k))
     &              **2*(ejoin(2*(k-1)) - epsil(k - 1))
C
         IF(k.LT.nrbarm)ejoin(2*k) = epsil(k)
     &                               + SQRT(( - 1)**k*((vjj(k+1)-vjj(k))
     &                               )/(1.D0 + (ho(k)/ho(k+1))**2))
     &                               /(smiu*ho(k))
      ENDDO
C
      IF(NRBarc.EQ.2 .AND. NRWel.EQ.0)THEN
         DEFfis(1) = epsil(1)
         DEFfis(2) = epsil(3)
      ENDIF
      IF(NRBarc.EQ.2 .AND. NRWel.EQ.1)THEN
         DEFfis(1) = epsil(1)
         DEFfis(2) = epsil(3)
         DEFfis(3) = epsil(2)
      ENDIF
      IF(NRBarc.EQ.3)THEN
         DEFfis(1) = epsil(1)
         DEFfis(2) = epsil(3)
         DEFfis(3) = epsil(5)
         DEFfis(4) = epsil(2)
         DEFfis(5) = epsil(4)
      ENDIF
C-------parameters of the equivalent outer barrier
100   DO ibars = 1, NRBarc
         XMInn(ibars) = 0.0001
         DO nr = 1, NRFdis(ibars)
            IF(EFDis(nr, ibars).GT.XMInn(ibars))XMInn(ibars)
     &         = EFDis(nr, ibars)
         ENDDO
      ENDDO
      IF(NRBar.EQ.5)THEN
         vvqq = .4
         efbmin = MIN(EFB(2) + XMInn(2), EFB(3) + XMInn(3))
         tf2 = 1./(1. + EXP( - 2.*PI*(efbmin-vvqq-EFB(2)))/H(1, 2))
         tf3 = 1./(1. + EXP( - 2.*PI*(efbmin-vvqq-EFB(3)))/H(1, 3))
C
         VEQ = efbmin
         tdir = (1. - tf2)*(1. - tf3)
         tdir23 = tf2*tf3/(1. + SQRT(ABS(tdir)) + tdir)
         rog = LOG(1./tdir23 - 1.)
C
         HOEq = ABS(2*PI*((efbmin-vvqq-VEQ))/rog)
         beta1 = epsil(3) - SQRT(EFB(2))/(smiu*H(1, 2))
         beta2 = epsil(5) + SQRT(EFB(3))/(smiu*H(1, 3))
C
         DEFeq = (beta2 - beta1)/2. + epsil(2)
      ENDIF
      END
C*==dami_rofis.spg  processed by SPAG 6.20Rc at 12:14 on  7 Jul 2004
C
C--------------------------------------------
      SUBROUTINE DAMI_ROFIS(Nnuc, Ibars, Mmod, Afiss)
C-----continuum, level densities at saddle points
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
      COMMON /CRIT  / TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl,
     &                BET2
      COMMON /CRITFIS/ ACRtf(2), UCRtf(2), TCRtf(2), DETcrtf(2),
     &                 SCRtf(2), MORtcrt(NFPARAB), MPArcrt(NFPARAB),
     &                 ECOndf(2)
      COMMON /PARAM / AP1, AP2, GAMma, DEL, DELp, BF, A23, A2, NLWst
      COMMON /FISSMOD/ ROFism(60, 30, 3), HM(NFTRANS, 3),
     &                 EFDism(NFTRANS, 3), UGRidf(0:NFISENMAX, 3),
     &                 EFBm(3), XMInnm(3), AFIsm(3), DEFbm(3),
     &                 SHCfism(3), DELtafism(3), GAMmafism(3),
     &                 WFIsm(3), BFFm(3),NRBinfism(3)
C
      DOUBLE PRECISION ROFism, HM, EFDism, EFBm, XMInnm, DEFbm, WFIsm,
     &                 SHCfism, DELtafism, GAMmafism, UGRidf, AFIsm
C
      DOUBLE PRECISION ACR, ACRt, ATIl, MORtcrt, MPArcrt, DETcrt, ECOnd,
     &                 SCR, TCRt, UCRt, BET2
      DOUBLE PRECISION rotemp, ROFis, Afiss
C
      DOUBLE PRECISION EFDis, EFB, UGRid, XMInn, xmax
C----------------------
      CHARACTER*2 simb
      CHARACTER*50 filename
      INTEGER BFFm
C
      excn1 = EMAx(Nnuc)
      BET2 = DEFfis(Ibars)
      IF(NRBarc.EQ.3 .AND. Ibars.EQ.2) BET2 = DEFeq
C-----where continuum starts,ends,steps in between
      IF(Mmod.EQ.0)XMInn(Ibars) = 0.0001
      IF(Mmod.GT.0)XMInnm(Mmod) = 0.0001
      DO nr = 1, NRFdis(Ibars)
         IF(Mmod.EQ.0 .AND. EFDis(nr, Ibars).GT.XMInn(Ibars))
     &      XMInn(Ibars) = EFDis(nr, Ibars)
         IF(Mmod.GT.0 .AND. EFDism(nr, Mmod).GT.XMInnm(Mmod))
     &      XMInnm(Mmod) = EFDism(nr, Mmod)
      ENDDO
      IF(NRBarc.EQ.3)XMInn(2) = 0.001
C
      xmax = 5.
      IF(Mmod.EQ.0 .AND. excn1.GT.EFB(Ibars))xmax = excn1 -
     &   (EFB(Ibars) + XMInn(Ibars)) + 5.
      IF(Mmod.GT.0 .AND. excn1.GT.EFBm(Mmod))xmax = excn1 -
     &   (EFBm(Mmod) + XMInnm(Mmod)) + 5.
C
C     destepp=(xmax-xminn(ibars))/100.
      DEStepp = .2
C
      IF(Mmod.EQ.0)NRBinfis(Ibars) = INT((xmax - XMInn(Ibars))/DEStepp)
      IF(Mmod.GT.0)NRBinfism(Mmod) = INT((xmax - XMInnm(Mmod))/DEStepp)
C
C----- Initializing UGRid() for FISden(NNUc)=1  RCN 31/12/2003
      IF(Mmod.EQ.0)THEN
         DO kk = 1, NRBinfis(Ibars)
            UGRid(kk, Ibars) = XMInn(Ibars) + (kk - 1)*DEStepp
         ENDDO
      ELSE
         DO kk = 1, NRBinfism(Mmod)
            UGRidf(kk, Mmod) = XMInnm(Mmod) + (kk - 1)*DEStepp
         ENDDO
      ENDIF
C
      IF(FISden(Nnuc).EQ.1.)THEN
         FIScon = 2.
         MOMortcrt = MORtcrt(Ibars)
         MOMparcrt = MPArcrt(Ibars)
         iff = 1
         IF(Mmod.EQ.0)THEN
            GAMma = GAMmafis(Ibars)
            DELp = DELtafis(Ibars)
            SHC(Nnuc) = SHCfis(Ibars)
            iff = (BFF(Ibars))
            ACRt = ACRtf(Ibars)
            UCRt = UCRtf(Ibars)
            TCRt = TCRtf(Ibars)
            SCR = SCRtf(Ibars)
            DETcrt = DETcrtf(Ibars)
            Afiss = AFIs(Ibars)
            ECOnd = ECOndf(Ibars)
         ENDIF
         IF(Mmod.GT.0)THEN
            NRBinfis(Ibars) = NRBinfism(Mmod)
            XMInn(Ibars) = XMInnm(Mmod)
            iff = BFFm(Mmod)
            GAMma = GAMmafism(Mmod)
            DELp = DELtafism(Mmod)
            SHC(Nnuc) = SHCfism(Mmod)
            Afiss = AFIsm(Mmod)
         ENDIF
         DO jj = 1, NLW
            aaj = FLOAT(jj) + HIS(Nnuc)
            DO kk = 1, NRBinfis(Ibars)
               CALL DAMIRO(kk, Nnuc, XMInn(Ibars), DEStepp, Afiss,
     &                     rotemp, aaj, iff)
               IF(Mmod.EQ.0)ROFis(kk, jj, Ibars) = rotemp
               IF(Mmod.GT.0)ROFism(kk, jj, Mmod) = rotemp
            ENDDO
         ENDDO
      ENDIF
C-----FISDEN(Nnuc)=0 reading microscopic lev. dens. from the RIPL-2 file
      IF(FISden(Nnuc).EQ.0.)THEN
         iz = INT(Z(Nnuc))
         ia = INT(A(Nnuc))
         WRITE(filename, 99001)iz
99001    FORMAT('../RIPL-2/fission/fis-levden-hfbcs-inner/z', i3.3,
     &          '.dat')
         OPEN(UNIT = 81, FILE = filename)
         READ(81, *)
 50      READ(81, 99002, ERR = 50, END = 150)simb, izr, iar
99002    FORMAT(23x, a2, i3, 3x, i3)
         IF(simb.NE.'Z=')GOTO 50
         IF(iar.NE.ia .OR. izr.NE.iz)GOTO 50
         READ(81, *)
         READ(81, *)
         ii = 1
 100     READ(81, '(f7.2,f7.3,1x,33e9.2)')UGRid(ii, 1), t, t, t, t,
     &        (ROFis(ii, j, 1), j = 1, NFISJ)
         IF(UGRid(ii, 1).LE.0.001)GOTO 200
         IF(ii.EQ.NFISEN)GOTO 200
         ii = ii + 1
         GOTO 100
C
 150     WRITE(6, *)' NO LEV. DENS. FOR Z=', iz, ' A=', ia, ' IN HFBSC'
         WRITE(6, *)' USE OTHER LEVEL DENSITIES. EXECUTION TERMINATED '
         STOP 'HFBCS lev dens. at the inner saddle point missing'
 200     CLOSE(81)
C
         IF(NRBar.GT.1)THEN
            WRITE(filename, 99003)iz
99003       FORMAT('../RIPL-2/fission/fis-levden-hfbcs-outer/z', i3.3,
     &             '.dat')
            OPEN(UNIT = 82, FILE = filename)
            READ(82, *)
 220        READ(82, 99004, ERR = 220, END = 260)simb, izr, iar
99004       FORMAT(23x, a2, i3, 3x, i3)
            IF(simb.NE.'Z=')GOTO 220
            IF(iar.NE.ia .OR. izr.NE.iz)GOTO 220
            READ(82, *)
            READ(82, *)
            ii = 1
 240        READ(82, '(f7.2,f7.3,1x,33e9.2)')UGRid(ii, 2), t, t, t, t,
     &           (ROFis(ii, j, 2), j = 1, NFISJ)
            IF(UGRid(ii, 2).LE.0.001)GOTO 280
            IF(ii.EQ.NFISEN)GOTO 280
            ii = ii + 1
            GOTO 240
 260        WRITE(6, *)' NO LEV. DENS. FOR Z=', iz, ' A=', ia,
     &                 ' IN HFBSC'
            WRITE(6, *)
     &                ' USE OTHER LEVEL DENSITIES. EXECUTION TERMINATED'
            STOP 'HFBCS lev dens. at the ouetr saddle-point missing'
 280        CLOSE(82)
         ENDIF
C
         IF(NFISJ.LT.NLW)THEN
            DO ib = 1, NRBarc
               DO j = NFISJ + 1, NLW
                  DO ii = 1, NFISEN
                     ROFis(ii, j, ib) = 0.
                  ENDDO
               ENDDO
            ENDDO
         ENDIF
      ENDIF
C
      END

      SUBROUTINE GDRGFLDATA(Znucleus, Anucleus)
C
C    ********************************************************************
C    *                                                                  *
C    * Assignment of the GDR and GFL model parameters                   *
C    * to nucleus with atomic number 'Znucleus'and mass number'Anucleus'*  .                                                   *
C    *                                                                  *
C    *  The parameters are put in the following COMMON's:               *
C    *   -----------------------------------------------                *
C    *                                                                  *
C    *  COMMON /PARGDR/  EG1, GW1, CS1, EG2, GW2, CS2, NG               *
C    *  COMMON /GFLPARAM/ BETagfl2, S2Plusgfl                           *
C    *                                                                  *
C    *   -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --                *
C    *                                                                  *
C    * 'COMMON/PARGDR/EG1,GW1,CS1,EG2,GW2,CS2,NG'  contains             *
C    *  input for GDR parameters (all deformed nuclei are               *
C    *  considered as  axially symmetric spheroids):                    *
C    *                                                                  *
C    *  EG1= peak energy of the first peak,                             *
C    *  GW1= full width of the first peak at half-maximum,              *
C    *  CS1= peak cross section of the first peak,                      *
C    *  EG2= peak energy of the second peak,                            *
C    *  CS2= peak cross section of the second peak,                     *
C    *  GW2= full width of the second peak at half-maximum              *
C    *  CS2= peak cross section of the second peak,                     *
C    *  NG : for NG=1, single peak(spherical nucleus),                  *
C    *       for NG=2, double peaks(deformed nucleus);                  *
C    *                                                                  *
C    * 'COMMON /GFLPARAM/ BETagfl2, S2Plusgfl'  contains                *
C    *  the parameters of the GFL model:                                *
C    *                                                                  *
C    *  BETagfl2(=beta**2)= square of "deformation"                     *
C    *                     parameter 'beta' associated                  *
C    *                     with nuclear quadrupole moment,              *
C    *  S2Plus(=(E2+)*beta**2)= product of first-excited                *
C    *                          2+ state energy(in MeV)                 *
C    *                          and square of the                       *
C    *                          deformation parameter                   *
C    *                                                                  *
C    *  Setting the GDR parameters                                      *
C    *  --------------------------                                      *
C    *  Attempts are made  initially to set the GDR parameters          *
C    *  from  "beijingn.dat" file (see RIPL).                           *
C    *                                                                  *
C    *  If GDR parameters are not found in "beijing.dat" they           *
C    *  are calculated by global parametrization with effective         *
C    *  quadrupole deformation parameters from "deflib.dat"file.        *
C    *  These effective quadrupole deformation parameters were obtained *
C    *  with  the use of  "Moller.dat" file of RIPL.                    *
C    *                                                                  *
C    *                                                                  *
C    *  Setting parameters of GFL model                                 *
C    *  -------------------------------                                 *
C    *  GFL model -> S.F.Mughabghab,C.L.Dunford,Phys.Lett.B487(2000)155 *
C    *                                                                  *
C    *  Initially attemts are made to set parameter 'beta' and first-   *
C    *  -excited state energy (E2+) from"def_eff.dat" file.This file is *
C    *  prepared from data file "raman_tableI.txt" given by  S.Raman,   *
C    *  C.W.Nestor,Jr, P.Tikkanen [Atom.Data Nucl.Data Tabl. 78(2001)1; *
C    *  Table 1 for even-even nuclei].The value of deformation parameter*
C    *  '|beta2|' from "deflib.dat"file is used for 'beta' if the 'beta'*
C    *  is absent in the "def_eff.dat" file and  global parametrisation *
C    *  for parameter 'S2Plus=(E2+)*beta**2' is used in this case.      *
C    ********************************************************************
C
      IMPLICIT NONE
      INTEGER nntmp,natmp,MAXgdr
      PARAMETER (MAXgdr=6000)
      INTEGER  Key_GDRGFL, Key_shape
      INTEGER i, ka, keyload, kz, n, nana, nanz, NG, nna, nng, nnz,
     &        nzram, naram, numram, keyram, keyalpa, kaa, kzz,
     &        nnzt(MAXgdr), nnat(MAXgdr), nngt(MAXgdr)
      DOUBLE PRECISION a0, a3, aann, alambda, alpha2, b0, CS0, CS1, CS2,
     &                 csaa, csb, EG0, EG1, EG2, ea, eb, GW0, GW1, GW2,
     &                 hbetagfl, henergygfl, BETagfl2, S2Plusgfl
      DOUBLE PRECISION etmp,fjtmp,btmp
      DOUBLE PRECISION halpha2, hcs1, hcs2, he1, he2, hgw1, hgw2, pi,
     &              zz, betagfl, energygfl, Znucleus, Anucleus,
     &              he1t(MAXgdr), hgw1t(MAXgdr), etat(MAXgdr),
     &              he2t(MAXgdr), hgw2t(MAXgdr), etaeps
      COMMON /GSA/ Key_shape, Key_GDRGFL
      COMMON /PARGDR/  EG1, GW1, CS1, EG2, GW2, CS2, NG
      COMMON /GFLPARAM/ BETagfl2, S2Plusgfl
      COMMON /MLOCOM2/ keyload, keyalpa, kzz, kaa
      COMMON /MLOCOM3/ nnz(300), nna(300), nng(300),
     &        he1(300), hcs1(300),
     &        hgw1(300), he2(300), hcs2(300), hgw2(300), nanz(9000),
     &        nana(9000), halpha2(9000), hbetagfl(700),
     &        henergygfl(700), nzram(700), naram(700),numram
      DATA pi/3.141592654D0/
C
      kz = Znucleus + 0.001
      ka = Anucleus + 0.001
      IF(kz.EQ.kzz .AND. ka.EQ.kaa) return
      kzz=kz
      kaa=ka
      IF(keyload.NE.1)THEN
        keyload = 1
C       README file format (2I4,1X,A2,2I3,6F7.2,2X,A5)
        OPEN(81, FILE = '../RIPL-2/gamma/gdr-parameters-exp.dat',
     &                                       STATUS = 'old',ERR=3500)
        READ(81,'(///)') ! Skipping first 4 title lines
        DO i = 1, 270
           READ(81,'(2I4, 1x,2x,3x, i3, 6F7.2)', END = 40,
     &        ERR = 40) nnz(i), nna(i), nng(i),
     &          he1(i), hcs1(i), hgw1(i), he2(i), hcs2(i), hgw2(i)
        ENDDO
40      CLOSE(81)
C       README file format (2i4,1x,a2,f7.3,4f7.2)
41      CONTINUE
        OPEN(81, FILE = '../RIPL-2/gamma/gdr-parameters-theor.dat',
     &                                       STATUS = 'old',ERR=3550)
        READ(81,'(///)') ! Skipping first 4 title lines
        DO i = 1, MAXgdr
           READ(81,'(2I4, 1x,2x, f7.3, 4F7.2)', END = 50,
     &       ERR = 50) nnzt(i), nnat(i), etat(i),
     &          he1t(i), hgw1t(i), he2t(i), hgw2t(i)
           nngt(i)=2
           if(he1t(i).eq.he2t(i)) nngt(i)=1
        ENDDO
50      CLOSE(81)
51      OPEN(82, FILE = '../data/deflib.dat', STATUS = 'old',ERR=3600)
        READ(82, '(////)')  ! Skipping first 5 title lines
        DO i = 1, 9000
           READ(82, '((2I4, f7.3))', END = 100, ERR = 100)
     &            nanz(i), nana(i), halpha2(i)
        ENDDO
100     CLOSE(82)
101     OPEN(84, FILE =
     &       '../RIPL-2/optical/om-data/om-deformations.dat',
     &       STATUS = 'old',ERR=3700)
        READ(84,'(///)') ! Skipping first 4 title lines
        numram = 0
        DO i = 1, 700
C       README file format (2i4,1x,a2,1x,f10.6,1x,f4.1,i3,i2,1x,f10.6,2x,a13)
           READ(84,'(2I4,4x,f10.6,1x,f4.1,6x,f10.6)', END = 200,
     &        ERR = 200) nntmp,natmp,etmp,fjtmp,btmp
C          Selecting only 2+ states
           if(abs(fjtmp-2.d0).gt.0.0001) cycle
           numram=numram+1
           nzram(numram)=nntmp
           naram(numram)=natmp
           henergygfl(numram)=etmp
           hbetagfl(numram)=btmp
        ENDDO
        WRITE(6,*) 'RIPL-2 GDR parameters used'
        GO TO 201
200     CLOSE(84)
3500    WRITE(6,'(1x,A14,A39,A43)') ' WARNING: File ',
     &             '../RIPL-2/gamma/gdr-parameters-exp.dat',
     &             ' not found, theoretical RIPL-2 will be used'
        GOTO 41
3550    WRITE(6,'(1x,A14,A41,A35)') ' WARNING: File ',
     &             '../RIPL-2/gamma/gdr-parameters-theor.dat',
     &             ' not found, default GDR values will be used'
        GOTO 51
3600    WRITE(6,'(1x,A14,A18,A43)') ' WARNING: File ../data/deflib.dat',
     &             ' not found, default deformation values will be used'
        GOTO 101
3700    WRITE(6,'(1x,A14,A45,A54)') ' WARNING: File ',
     &         '../RIPL-2/optical/om-data/om-deformations.dat',
     &         ' not found, default dynamical deformation values used'
      ENDIF
201   n = ka-kz
      zz = kz
      aann = ka
      a3 = aann**0.3333333
      EG0 = 31.2/a3 + 20.6/SQRT(a3)
      GW0 = 0.026*EG0**1.91
      CS0 = 1.2*120.*n*zz/(aann*pi*GW0)
C----Plujko_new
      IF (Key_GDRGFL.NE.0) THEN
C----Plujko_new(END)
        DO i = 1, 270
          IF(kz.EQ.nnz(i) .AND. ka.EQ.nna(i))THEN
            NG = nng(i)
            EG1 = he1(i)
            CS1 = hcs1(i)
            GW1 = hgw1(i)
            EG2 = he2(i)
            CS2 = hcs2(i)
            GW2 = hgw2(i)
            GOTO 400
          ENDIF
        ENDDO
C       If GDR parameters not found they are going to be retrieved from
C       the RIPL-2 Goriely theoretical values
        DO i = 1, MAXgdr
          IF(kz.EQ.nnzt(i) .AND. ka.EQ.nnat(i))THEN
            NG = nngt(i)
            EG1 = he1t(i)
            CS1 = CS0
            GW1 = hgw1t(i)
            EG2 = he2t(i)
            CS2 = 0.d0
            GW2 = hgw2t(i)
            etaeps = etat(i)
            IF(abs(etaeps-1.d0).GT.0.0001)THEN
C             *************************************************
C             *Global GDR parameterization for deformed nuclei*
C             *(classical sum rule with correction)           *
C             *************************************************
              CS1 = CS0/3.
              CS2 = CS0*2./3.
            ENDIF
          ENDIF
        ENDDO
C----Plujko_new
      ENDIF
C----Plujko_new(END)
C     ***********************************************************
C     *Setting the deformation parameter from "deflib.dat" file *
C     *for calculation of the GDR energies and widths           *
C     ***********************************************************
      keyalpa = 1
      DO i = 1, 9000
         IF(kz.EQ.nanz(i) .AND. ka.EQ.nana(i))THEN
            alpha2 = halpha2(i)
            GOTO 300
         ENDIF
      ENDDO
      alpha2 = 0.
 300  IF(ABS(alpha2).GT.0.001)THEN
C        *************************************************
C        *Global GDR parameterization for deformed nuclei*
C        *( classical sum rule with correction)          *
C        *************************************************
         NG = 2
         alambda = (1. + 0.6*alpha2**2 + 2.*alpha2**3/35.)**0.3333333
         a0 = (1. + alpha2)/alambda
         b0 = (1. - 0.5*alpha2)/alambda
         eb = EG0*(1. - 1.51E-02*(a0 + b0)*(a0 - b0))/b0
         ea = eb/(0.911*a0/b0 + 0.089)
         csaa = CS0/3.
         csb = CS0*2./3.
         EG1 = ea
         EG2 = eb
         CS1 = csaa
         CS2 = csb
         IF(ea.GT.eb)THEN
            EG1 = eb
            EG2 = ea
            CS1 = csb
            CS2 = csaa
         ENDIF
         GW1 = 0.026*EG1**1.91
         GW2 = 0.026*EG2**1.91
      ELSE
C        *******************************************************
C        * Global GDR parameterization for spherical targets   *
C        * ( classical sum rule with correction)               *
C        *******************************************************
         NG = 1
         EG1 = EG0
         GW1 = GW0
         CS1 = CS0
         EG2 = 0.
         GW2 = 0.
         CS2 = 0.
      ENDIF
C     *********************************************************
C     * Setting the GFL parameters '|beta|' from "defeff.dat" *
C     * and 'S2Plus=(E2+)*beta**2'                            *
C     *********************************************************
 400  DO i = 1, numram
         keyram = 0
         IF(kz.EQ.nzram(i) .AND. ka.EQ.naram(i))THEN
            IF(hbetagfl(i).GT.0.)THEN
               keyram = 1
               betagfl = hbetagfl(i)
C              *****************************************************************
C              *'BETagfl2=beta**2' and  'S2Plus=(E2+)*beta**2'  -------        *
C              *parameters of the GFL model[BETagfl2=beta; ENErgygfl=E2+(MeV)] *
C              *****************************************************************
               BETagfl2 = betagfl**2
C              energygfl = henergygfl(i)*0.001
               energygfl = henergygfl(i)
C              RIPL-2 energies in MeV, RCN 06/2004
               S2Plusgfl = BETagfl2*energygfl
            ENDIF
         ENDIF
         IF(keyram.EQ.1) return
      ENDDO
C     ***********************************************************
C     * Global parametrization for 'S2Plus=(E2+)*beta**2'       *
C     * and setting the '|beta2|' from "deflib.dat" file        *
C     * as 'beta' of the GFL model.                             *
C     *---------------------------------------------------------*
C     *Setting the deformation parameter from "deflib.dat" file *
C     *for  calculation  of  the  GFL model parameter  if it is *
C     *absent ('keyalpa=0')                                    *
C     ***********************************************************
      IF(keyalpa.EQ.0)THEN
         DO i = 1, 9000
            IF(kz.EQ.nanz(i) .AND. ka.EQ.nana(i))THEN
               alpha2 = halpha2(i)
               GOTO 500
            ENDIF
         ENDDO
         alpha2 = 0.
      ENDIF
 500  BETagfl = 1.5853*ABS(alpha2)
      BETagfl2 = BETagfl**2
      S2Plusgfl = 217.156/aann**2
      RETURN
      END