Ccc   * $Author: Capote $
Ccc   * $Date: 2005-07-27 14:17:43 $
Ccc   * $Id: input.f,v 1.162 2005-07-27 14:17:43 Capote Exp $
C
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
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C COMMON variables
C
      INTEGER KAA, KAA1, KEYinput, KEYload, KZZ, KZZ1, NCHr
      CHARACTER*10 PROjec, RESidue(NDNUC), TARget
      INTEGER*4 INDexf, INDexb, BUFfer(250)
      COMMON /EXFOR / TARget, PROjec, RESidue
      COMMON /IEXFOR/ NCHr
      COMMON /MLOCOM1/ KEYinput, KZZ1, KAA1
      COMMON /MLOCOM2/ KEYload, KZZ, KAA
      COMMON /R250COM/ INDexf,INDexb,BUFfer
C
C Local variables
C
      DOUBLE PRECISION aclu, ak2, ampi0, ampipm, ares, atmp, da,
     &                 deln(150), delp, delz(98), e2p, e3m, emaxr, qmin,
     &                 qtmp, xfis, zclu, zres, ztmp, culbar
      CHARACTER*3 atar, ca1
      CHARACTER*1 cnejec, proj
      DOUBLE PRECISION DATAN, DMAX1, DSQRT
      CHARACTER*2 deut, gamma, trit
      REAL FLOAT, SNGL
      LOGICAL gexist, nonzero, fexist
      INTEGER i, ia, iac, iae, iccerr, iend, ierr, ietl, iia, iloc, in,
     &        ip, irec, itmp, iz, izares, izatmp, j, lpar, na, nejc,
     &        netl, nnuc, nnur, mulem
      INTEGER IFINDCOLL
      INTEGER INDEX, INT, ISEED, NINT
      INTEGER*4 iwin
      INTEGER*4 PIPE
      CHARACTER*2 SMAT
      CHARACTER*132 x4string
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
C-----Defining global physical and mathematical constants
C-----They are passed through CONSTANT common block
C
C-----THE FOLLOWING DEFINITIONS COPIED FROM ECIS03 code (NEA DATA BANK)
C
C-----CONSTANTS COMPUTED FROM THE FUNDAMENTAL CONSTANTS, ATOMIC MASS, HBAR*C
C-----AND ALPHA, AS GIVEN IN THE EUROPEAN PHYSICAL JOURNAL, PAGE 73, VOLUME
C-----15 (2000) REFERRING FOR THESE VALUES TO THE 1998 CODATA SET WHICH MAY
C-----BE FOUND AT http://physics.nist.gov/constants
C-----CM=931.494013 +/- 0.000037 MeV
C-----The above value is the one used also in the ENDF-6 manual (April 2001)
      AMUmev = 9.31494013D+02
C-----CHB=197.3269601 +/- 0.0000078 (*1.0E-9 eV*cm)
      HHBarc = 197.3269601D0
C-----HHBarc = 1.97327053D+02  (EMPIRE v 2.18)
      XNExc = 8.071323D0
C-----From SCAT2000
      ampipm = 1.395688D+02
      ampi0 = 1.349645D+02
      AMPi = (2.D0*ampipm + ampi0)/3.D0
C     ELE2 = e*e in MeV.fm
      ELE2 = 1.4399652D+00
C-----Neutron mass = 1.008 664 915 60(55) u
      AMUneu = 1.008665D0
C-----Proton mass = 1.007 276 466 88(13) u
      AMUpro = 1.007276D0
C-----Electron mass = 5.485 799 0945 x 10-4 u
      AMUele = 5.4857990945D-4

      CETa = ELE2*DSQRT(AMUmev/2.D0)/HHBarc
      CSO = (HHBarc/AMPi)**2
      PI = 4.D0*DATAN(1.D0)

      INQUIRE(file='R250SEED.DAT',exist=fexist)
      if(fexist) then
        OPEN(94,file='R250SEED.DAT',status='OLD')
        read(94,*)  indexf, indexb
        Do i = 1, 250
          read(94,*) buffer(i)
        ENDDO
        CLOSE(94)
      else
C       If the file R250SEED.DAT does not exist, then DEFAULT
C       starting seed is used
        iseed = 1234567
        Call R250Init(iseed)
      endif

      IF (EIN.EQ.0.0D0) THEN
C
C--------default input parameters (skipped in non-first-energy calculation)
C
         FIRst_ein = .TRUE.
C--------select Meyers-Swiatecki shell corrections
         SHNix = 0.0
C--------neutralize tuning factors and OMP normalization factors
         DO nejc = 0, NDEJC
           TUNEpe(nejc) = 1.d0
         ENDDO
         DO nnuc = 0, NDNUC
            TUNefi(nnuc) = 1.d0
            DO nejc = 0, NDEJC
               TUNe(nejc,nnuc) = 1.d0
C--------------Volume real potential
               FNvvomp(Nejc,Nnuc) = 1.d0
               FNavomp(Nejc,Nnuc) = 1.d0
C--------------Volume imaginary potential
               FNwvomp(Nejc,Nnuc) = 1.d0
C--------------Surface imaginary potential:
               FNwsomp(Nejc,Nnuc) = 1.d0
               FNasomp(Nejc,Nnuc) = 1.d0
            ENDDO
         ENDDO
C--------Set TUNe for gammas in the CN to 0.999 so that 1.0 can be used
C--------to turn off normalization to experimental Gg
         TUNe(0,1) = 0.999
         DO nnuc = 1, NDNUC
            IZA(nnuc) = 0
C-----------set level density parameters
            ROPaa(nnuc) = -2.0
            ROPar(1,nnuc) = 0.
            ROPar(2,nnuc) = 0.
            ROPar(4,nnuc) = 0.
            ROPar(5,nnuc) = 0.
            ATIlnor(nnuc) = 0.
            GTIlnor(nnuc) = 1.
            LVP(1,nnuc) = 1
C-----------set ENDF flag to 0 (no ENDF formatting)
            ENDf(nnuc) = 0.0
c-----------set Levels flag to -1 (no levels stored)
            NSTOred(nnuc) = -1
         ENDDO
         NSTOred(0) = -1
C--------set gamma-strength parameters
         DO nnuc = 1, NDNUC
            GDRpar(1,nnuc) = 0.0
            GDRpar(2,nnuc) = 0.0
            GDRpar(3,nnuc) = 0.0
            GDRpar(4,nnuc) = 0.0
            GDRpar(5,nnuc) = 0.0
            GDRpar(6,nnuc) = 0.0
            GDRpar(7,nnuc) = 1.0
         ENDDO
         IZA(0) = 0
         ENDf(0) = 0.0
         LVP(1,0) = 1
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
         TOTred = 1.
         LEVtarg = 1
C
C--------Capote, additional input options
C
         DIRect = 0
         KTRompcc = 0
         CCCalc = .FALSE.
         IOMwritecc = 0
         MODelecis = 0
         EXClusiv = .TRUE.
         WIDcoll = 0.d0
C--------Relativistic kinematics
         RELkin = .FALSE.
C--------Maximum energy to assume all levels are collective for DWBA calculations
C--------        Default value 0. i.e. none but those selected automatically
         ECUtcoll = 0.
         JCUtcoll = 2.
C
C        IOPSYS = 0 LINUX
C        IOPSYS = 1 WINDOWS
         IOPsys = 0
C--------Mode of EXFOR retrieval
C        IX4ret = 0 no EXFOR retrieval
C        IX4ret = 1 local MySQL server (2.19 default)
C        IX4ret = 2 remote SYBASE server
C        IX4ret = 3 local EXFOR files (as in 2.18 and before)
         IX4ret = 0
C--------CCFUF parameters
         DV = 10.
         FCC = 1.
         NACc = 0
C        By default only target deformation is considered
         NSCc = 4
C        Default deformation values, they are changed in ifindcoll()
         BETcc(1) = 0.15
         BETcc(2) = 0.05
         BETcc(3) = 0.15
         BETcc(4) = 0.05
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
C--------ICOmpff must be off for DOM potentials
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
         CHMax = 0.d0 ! set default to 0.2 inside PCROSS
C--------HRTW control (0 no HRTW, 1 HRTW up to EHRtw MeV incident)
         LHRtw = 1
         EHRtw = 5.d0
C--------ENDF global setting initialized to zero (no formatting)
         Nendf = 0
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
C        Default values for keys (Key_shape, Key_GDRGFL) to set
C        shape of E1 strength function and GDR parameters
         KEY_shape = 0
         KEY_gdrgfl = 1
         KEYinput = 0
         KZZ1 = 0
         KAA1 = 0
         KEYload = 0
         KZZ = 0
         KAA = 0
         IGE1 = 1
         LQDfac = 1.0D0
         IGM1 = 0
         IGE2 = 0
C        Set default 'MAXmult'
C         - maximal value (=< 10) of gamma-ray multipolarity in
C           calculation of gamma-transitions both between states in
C           continuum and from continuum to discrete levels;
C           can be changed in the optional input
C           (keyword 'GRMULT')
         MAXmult = 2
C--------read entire input for the first energy calculations
C--------mandatory part of the input
C--------incident energy (in LAB)
         READ (5,*) EIN
C        Starting value of the number of angular points
         NANgela = 37
         NDAng   = 37
         IF(EIN.GT.20. .AND. EIN.LE.50.) THEN
           NANgela = 73
           NDAng   = 73
         ELSEIF(EIN.GT.50.) THEN
           NANgela = 91
           NDAng   = 91
         ENDIF
         IF(NANgela.GT.NDAngecis) THEN
           WRITE(6,*)
     &        'FATAL: increase NANgecis in dimension.h up to ',NANgela
           STOP 'FATAL: increase NANgecis in dimension.h'
         ENDIF
C--------set angles for inelastic calculations
         da = 180.0/(NDANG - 1)
         DO na = 1, NDANG
           ANGles(na) = (na - 1)*da
         ENDDO
         DO na = 1, NDANG
           CANgler(na) = COS(ANGles(NDANG - na + 1)*PI/180.)
           SANgler(na) = SQRT(1.D0 - CANgler(na)**2)
         ENDDO
C--------target
         READ (5,*) A(0), Z(0)
         IF (A(0).LT.Z(0)) THEN
            WRITE (6,*) 'FATAL: Z > A, please correct input file'
            STOP 'FATAL: Z > A, please correct input file'
         ENDIF
         CALL PTLEVSET(A(0),Z(0),XJLv(1,0),LVP(1,0),e2p,e3m)
         XN(0) = A(0) - Z(0)
         IZA(0) = INT(1000*Z(0) + A(0))
         ia = INT(A(0))
         iz = INT(Z(0))
         SYMb(0) = SMAT(iz)
         NLV(0) = 1
         ELV(1,0) = 0.0
         QCC(1) = -e2p
         QCC(2) = -e3m
C--------set target  for EXFOR retrieval
         TARget = '          '
         TARget(1:1) = '<'
         TARget(2:3) = SYMb(0)
         iend = INDEX(TARget,' ') - 1
         TARget(iend + 1:iend + 2) = '-'
         iend = iend + 1
         WRITE (ca1,'(I3)') ia
         IF (ia.LT.10) THEN
            TARget(iend + 1:iend + 2) = ca1(3:3)
         ELSEIF (ia.LT.100) THEN
            TARget(iend + 1:iend + 3) = ca1(2:3)
         ELSE
            TARget(iend + 1:iend + 4) = ca1(1:3)
         ENDIF
C--------target ******  done  ********
C--------projectile
         READ (5,*) AEJc(0), ZEJc(0)
         IF (AEJc(0).EQ.0 .AND. ZEJc(0).EQ.0) THEN
C-----------GAMMA EMISSION
            SEJc(0) = 1
            lpar = -1
         ELSE
            CALL PTLEVSET(AEJc(0),ZEJc(0),SEJc(0),lpar,e2p,e3m)
         ENDIF
C--------product of target and projectile parities
C        LVP(LEVtarg,0) = LVP(LEVtarg,0)*lpar
C        RCN, We assume is only the target parity !!!
         XNEjc(0) = AEJc(0) - ZEJc(0)
         IZAejc(0) = INT(1000.*ZEJc(0) + AEJc(0))
         iz = INT(ZEJc(0))
         SYMbe(0) = SMAT(iz)
         QCC(3) = -e2p
         QCC(4) = -e3m
C--------set projectile  for EXFOR retrieval
         PROjec = '          '
         NCHr = 2
         IF (AEJc(0).EQ.1.0D0 .AND. ZEJc(0).EQ.0.0D0) PROjec = 'N,'
         IF (AEJc(0).EQ.1.0D0 .AND. ZEJc(0).EQ.1.0D0) PROjec = 'P,'
         IF (AEJc(0).EQ.4.0D0 .AND. ZEJc(0).EQ.2.0D0) PROjec = 'A,'
         IF (AEJc(0).EQ.2.0D0 .AND. ZEJc(0).EQ.1.0D0) PROjec = 'D,'
         IF (AEJc(0).EQ.0.0D0 .AND. ZEJc(0).EQ.0.0D0) PROjec = 'G,'
         IF (AEJc(0).EQ.3.0D0 .AND. ZEJc(0).EQ.1.0D0) PROjec = 'T,'
         IF (AEJc(0).EQ.3.0D0 .AND. ZEJc(0).EQ.2.0D0) THEN
            PROjec = 'HE3,'
            NCHr = 4
         ENDIF
C--------***** done ********
C--------NEMN  number of neutrons emitted
         READ (5,*) nemn
C--------NEMP  number of protons  emitted
         READ (5,*) nemp
C--------NEMA  number of alphas   emitted
         READ (5,*) nema
C--------NEMC  number of clusters emitted
         READ (5,*) NEMc, aclu, zclu
         IF (NDEJC.LT.4) NEMc = 0
C--------cluster ejectile
         IF (NDEJC.GT.3) THEN
            AEJc(NDEJC) = aclu
            ZEJc(NDEJC) = zclu
            IF (aclu.EQ.2.0D0 .AND. zclu.EQ.1.0D0) SEJc(NDEJC) = 1.0
            IF (aclu.EQ.3.0D0 .AND. zclu.EQ.2.0D0) SEJc(NDEJC) = 0.5
            IF (aclu.EQ.6.0D0 .AND. zclu.EQ.3.0D0) SEJc(NDEJC) = 1.0
            IF (aclu.EQ.7.0D0 .AND. zclu.EQ.3.0D0 .OR.
     &          aclu.EQ.7.0D0 .AND. zclu.EQ.4.0D0) SEJc(NDEJC) = 1.5
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
         IF (A(1)*0.5.NE.AINT(A(1)*0.5)) HIS(1) = -0.5
C        ENDf(1) = 1.0
C--------set reaction string
         REAction(nnuc) = '(z,gamma)'
C--------set CN  for EXFOR retrieval
         RESidue(nnuc) = '          '
         RESidue(nnuc)(1:1) = '>'
         RESidue(nnuc)(2:3) = SYMb(nnuc)
         iend = INDEX(RESidue(nnuc),' ') - 1
         RESidue(nnuc)(iend + 1:iend + 2) = '-'
         iend = iend + 1
         WRITE (ca1,'(I3)') ia
         IF (ia.LT.10) THEN
            RESidue(nnuc)(iend + 1:iend + 2) = ca1(3:3)
         ELSEIF (ia.LT.100) THEN
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
         IF (aclu.EQ.0.0D0 .OR. zclu.EQ.0.0D0) NEJcm = 3
         IF (ZEJc(0).EQ.0.0D0 .AND. AEJc(0).EQ.0.0D0) SYMbe(0) = gamma
C--------correct ejectiles symbols
         DO nejc = 1, NEJcm
            IF (ZEJc(nejc).EQ.1.0D0 .AND. AEJc(nejc).EQ.2.0D0)
     &          SYMbe(nejc) = deut
            IF (ZEJc(nejc).EQ.1.0D0 .AND. AEJc(nejc).EQ.3.0D0)
     &          SYMbe(nejc) = trit
         ENDDO
         DO iac = 0, NEMc
            DO ia = 0, nema
               DO ip = 0, nemp
                  DO in = 0, nemn
                     IF (iac + ia + ip + in.NE.0) THEN
                        atmp = A(1) - FLOAT(in)*AEJc(1) - FLOAT(ip)
     &                         *AEJc(2) - FLOAT(ia)*AEJc(3)
                        IF (NDEJC.GT.3) atmp = atmp - FLOAT(iac)
     &                      *AEJc(NDEJC)
                        ztmp = Z(1) - FLOAT(in)*ZEJc(1) - FLOAT(ip)
     &                         *ZEJc(2) - FLOAT(ia)*ZEJc(3)
                        IF (NDEJC.GT.3) ztmp = ztmp - FLOAT(iac)
     &                      *ZEJc(NDEJC)

C                       residues must be heavier than alpha !! (RCN)
                        if(atmp.le.4 . or. ztmp.le.2) cycle

                        izatmp = INT(1000*ztmp + atmp)
                        CALL WHERE(izatmp,nnuc,iloc)
                        IF (iloc.EQ.1) THEN
                           A(nnuc) = atmp
                           Z(nnuc) = ztmp
                           XN(nnuc) = A(nnuc) - Z(nnuc)
                           IZA(nnuc) = izatmp
                           iia = INT(A(nnuc))
                           iz = INT(Z(nnuc))
                           SYMb(nnuc) = SMAT(iz)
                           HIS(nnuc) = -1.
                           IF (A(nnuc)*0.5.NE.AINT(A(nnuc)*0.5))
     &                         HIS(nnuc) = -0.5
C--------------------------set reaction string
                           REAction(nnuc) = '(z,'
                           iend = 3
                           IF (in.NE.0) THEN
                              WRITE (cnejec,'(I1)') in
                              IF (in.GT.1) THEN
                                 REAction(nnuc)(iend + 1:iend + 1)
     &                              = cnejec
                                 iend = iend + 1
                              ENDIF
                              REAction(nnuc)(iend + 1:iend + 1) = 'n'
                              iend = iend + 1
                           ENDIF
                           IF (ip.NE.0) THEN
                              WRITE (cnejec,'(I1)') ip
                              IF (ip.GT.1) THEN
                                 REAction(nnuc)(iend + 1:iend + 1)
     &                              = cnejec
                                 iend = iend + 1
                              ENDIF
                              REAction(nnuc)(iend + 1:iend + 1) = 'p'
                              iend = iend + 1
                           ENDIF
                           IF (ia.NE.0) THEN
                              WRITE (cnejec,'(I1)') ia
                              IF (ia.GT.1) THEN
                                 REAction(nnuc)(iend + 1:iend + 1)
     &                              = cnejec
                                 iend = iend + 1
                              ENDIF
                              REAction(nnuc)(iend + 1:iend + 1) = 'a'
                              iend = iend + 1
                           ENDIF
                           IF (NDEJC.GT.3 .AND. iac.NE.0) THEN
                              WRITE (cnejec,'(I1)') iac
                              IF (iac.GT.1) THEN
                                 REAction(nnuc)(iend + 1:iend + 1)
     &                              = cnejec
                                 iend = iend + 1
                              ENDIF
C                             REAction(nnuc)(iend + 1:iend + 2) = 'li'
C                             iend = iend + 2
                              IF (AEJc(NDEJC).EQ.2 .AND. ZEJc(NDEJC)
     &                            .EQ.1) THEN
                                 REAction(nnuc)(iend + 1:iend + 1) = 'd'
                                 iend = iend + 1
                              ENDIF
                              IF (AEJc(NDEJC).EQ.3 .AND. ZEJc(NDEJC)
     &                            .EQ.1) THEN
                                 REAction(nnuc)(iend + 1:iend + 1) = 't'
                                 iend = iend + 1
                              ENDIF
                              IF (AEJc(NDEJC).EQ.3 .AND. ZEJc(NDEJC)
     &                            .EQ.2) THEN
                                 REAction(nnuc)(iend + 1:iend + 3)
     &                               = 'He3'
                                 iend = iend + 3
                              ENDIF
                           ENDIF
                           REAction(nnuc)(iend + 1:iend + 1) = ')'
C--------------------------set residues to be used for EXFOR retrieval
                           RESidue(nnuc) = '          '
                           RESidue(nnuc)(1:1) = '>'
                           RESidue(nnuc)(2:3) = SYMb(nnuc)
                           iend = INDEX(RESidue(nnuc),' ') - 1
                           WRITE (ca1,'(I3)') iia
                           RESidue(nnuc)(iend + 1:iend + 2) = '-'
                           iend = iend + 1
                           IF (iia.LT.10) THEN
                              RESidue(nnuc)(iend + 1:iend + 2)
     &                           = ca1(3:3)
                           ELSEIF (iia.LT.100) THEN
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
         INQUIRE (FILE = 'EXFOR.dat',EXIST = gexist)
         IF (.NOT.gexist) THEN
            IF (IX4ret.EQ.3) CALL RETRIEVE
                                         !retrieval from the local database
            IF (IX4ret.EQ.1 .OR. IX4ret.EQ.2) THEN
C--------------on-line EXFOR retrieval from the remote database
               WRITE (atar,'(I3)') INT(A(0))
               IF (atar(1:1).EQ.' ') THEN
                  atar(1:1) = atar(2:2)
                  atar(2:2) = atar(3:3)
                  atar(3:3) = ' '
               ENDIF
               IF (atar(1:1).EQ.' ') THEN
                  atar(1:1) = atar(2:2)
                  atar(2:2) = ' '
               ENDIF
C--------------set projectile  for EXFOR retrieval
               proj = ' '
               IF (AEJc(0).EQ.1.0D0 .AND. ZEJc(0).EQ.0.0D0) proj = 'n'
               IF (AEJc(0).EQ.1.0D0 .AND. ZEJc(0).EQ.1.0D0) proj = 'p'
               IF (AEJc(0).EQ.4.0D0 .AND. ZEJc(0).EQ.2.0D0) proj = 'a'
               IF (AEJc(0).EQ.2.0D0 .AND. ZEJc(0).EQ.1.0D0) proj = 'd'
               IF (AEJc(0).EQ.0.0D0 .AND. ZEJc(0).EQ.0.0D0) proj = 'g'
               IF (AEJc(0).EQ.3.0D0 .AND. ZEJc(0).EQ.1.0D0) proj = 't'
C--------------retrieval from the remote database
               IF (SYMb(0)(2:2).EQ.' ' .AND. IX4ret.EQ.2) THEN
                  x4string =
     &'~/X4Cinda/jre/bin/java -cp jconn2.jar:x4retr.jar: x4retr x -targe
     &t:"'//SYMb(0)(1:1)//'-'//atar//';'//SYMb(0)(1:1)
     &//'-0" -Rea ct:"'//proj//',*"'//' -quant:"CS;DA;DAE;DE;CSP"#'
               ELSEIF (IX4ret.EQ.2) THEN
                  x4string =
     &'~/X4Cinda/jre/bin/java -cp jconn2.jar:x4retr.jar: x4retr x -targe
     &t:"'//SYMb(0)//'-'//atar//';'//SYMb(0)//'-0" -React:"'//proj//
     &',*"'//' -quant:"CS;DA;DAE;DE;CSP"#'
               ENDIF
C--------------retrieval from the local MySQL database
C--------------including data for the natural element
C              IF(SYMb(0)(2:2).EQ.' ' .AND. IX4ret.EQ.1)THEN
C                 x4string = '../scripts/X4retrieve "'//SYMb(0)(1:1)//
C    &                       '-0'//';'//SYMb(0)(1:1)//'-'//atar//'" '//
C    &                       '"CS;DA;DAE;DE;CSP" '//'"'//proj//',*"#'
C              ELSEIF(IX4ret.EQ.1)THEN
C                 x4string = '../scripts/X4retrieve "'//SYMb(0)//'-0'//
C    &                       ';'//SYMb(0)//'-'//atar//'" '//
C    &                       '"CS;DA;DAE;DE;CSP" '//'"'//proj//',*"#'
C              ENDIF
C--------------data for the target isotop only
               IF (SYMb(0)(2:2).EQ.' ' .AND. IX4ret.EQ.1) THEN
                  x4string = '../scripts/X4retrieve "'//SYMb(0)(1:1)
     &                       //'-'//atar//'" '//'"CS;DA;DAE;DE;CSP" '//
     &                       '"'//proj//',*"#'
               ELSEIF (IX4ret.EQ.1) THEN
                  x4string = '../scripts/X4retrieve "'//SYMb(0)
     &                       //'-'//atar//'" '//'"CS;DA;DAE;DE;CSP" '//
     &                       '"'//proj//',*"#'
               ENDIF
               iwin = PIPE(x4string)
            ENDIF
         ENDIF
C--------retrieve of EXFOR data *** done ***
         NNUcd = nnuc
         NNUct = NNUcd
         DO nnuc = 1, NNUcd
            IF (A(0).EQ.A(nnuc) .AND. Z(0).EQ.Z(nnuc)) NTArget = nnuc
            DO nejc = 1, NEJcm
C--------------To find inelastic channel
               IF (AEJc(0).EQ.AEJc(nejc) .AND. ZEJc(0).EQ.ZEJc(nejc))
     &             NPRoject = nejc
               ares = A(nnuc) - AEJc(nejc)
               zres = Z(nnuc) - ZEJc(nejc)

C              residual nuclei must be heavier than alpha
               if(ares.le.4 . or. zres.le.2) cycle

               izares = INT(1000*zres + ares)
               CALL WHERE(izares,nnur,iloc)
               IF (iloc.EQ.1) THEN
                  A(nnur) = ares
                  Z(nnur) = zres
                  XN(nnur) = A(nnur) - Z(nnur)
                  IZA(nnur) = izares
                  iz = INT(Z(nnur))
                  SYMb(nnur) = SMAT(iz)
                  HIS(nnur) = -1.
                  IF (A(nnur)*0.5.NE.AINT(A(nnur)*0.5)) HIS(nnur) = -0.5
                  ENDf(nnur) = 2 ! RCN, July 2005
                  NNUct = NNUct + 1
               ENDIF
            ENDDO
         ENDDO
C--------end ascribing location to each nucleus

C--------inteligent defaults
         KTRlom(0,0) = 0 ! default (allows for HI reactions)
C--------optical model parameter set selection
         IF (AEJc(0).EQ.0 .AND. ZEJc(0).EQ.0) THEN
C-----------INCIDENT GAMMA
            KTRlom(0,0) = -1
         ELSEIF (AEJc(0).EQ.1 .AND. ZEJc(0).EQ.0) THEN ! neutrons
            IF (A(0).LE.220) THEN
               KTRlom(0,0) = 2405    ! Koning potential
C-------------(Morillon dispersive global potential 2407 could be used)
            ELSE
               KTRlom(0,0) = 2601    ! Soukhovitskii CC OMP
C-------------(to be replaced by Soukhovistkii & Capote dispersive CC OMP)
            ENDIF
         ELSEIF (AEJc(0).EQ.1 .AND. ZEJc(0).EQ.1) THEN
            IF (A(0).LE.220) THEN
               KTRlom(0,0) = 5405
            ELSE
               KTRlom(0,0) = 5601    ! Soukhovitskii CC OMP
C-------------(to be replaced by Soukhovistkii & Capote dispersive CC OMP)
            ENDIF
         ELSEIF (AEJc(0).EQ.2 .AND. ZEJc(0).EQ.1) THEN
            KTRlom(0,0) = 6400       ! Bojowald OMP for deuterons
         ELSEIF (AEJc(0).EQ.3 .AND. ZEJc(0).EQ.1) THEN
            KTRlom(0,0) = 7100       ! Bechetti OMP for tritons
         ELSEIF ((AEJc(0).EQ.4 .OR. AEJc(0).EQ.3) .AND. ZEJc(0).EQ.2)
     &           THEN
            KTRlom(0,0) = 9600       ! Avrigeanu OMP for He-4 and He-3
C-----------(McFadden global potential 9100 could be used)
         ENDIF
         DO i = 1, NDNUC
            KTRlom(1,i) = 2405
            IF (NPRoject.EQ.1) KTRlom(1,i) = KTRlom(0,0)
            KTRlom(2,i) = 5405
            IF (NPRoject.EQ.2) KTRlom(2,i) = KTRlom(0,0)
            KTRlom(3,i) = 9600
            IF (NPRoject.EQ.3) KTRlom(3,i) = KTRlom(0,0)
            IF (NDEJC.GT.3) THEN
               KTRlom(NDEJC,i) = 9600
               IF (AEJc(NDEJC).EQ.2 .AND. ZEJc(NDEJC).EQ.1)
     &             KTRlom(NDEJC,i) = 6400
               IF (AEJc(NDEJC).EQ.3 .AND. ZEJc(NDEJC).EQ.1)
     &             KTRlom(NDEJC,i) = 7100
               IF (NPRoject.EQ.NDEJC) KTRlom(NDEJC,i) = KTRlom(0,0)
            ENDIF
         ENDDO
C
C--------inteligent defaults *** done ***
C
         Irun = 0
         CALL READIN(Irun)   !optional part of the input
C--------Set exclusive and inclusive ENDF formatting flags
         NEXclusive = 0
         IF(NENdf.GT.0) THEN
            DO iac = 0, NEMc
               DO ia = 0, nema
                  DO ip = 0, nemp
                     DO in = 0, nemn
                        mulem = iac + ia + ip + in
                        atmp = A(1) - FLOAT(in)*AEJc(1) - FLOAT(ip)
     &                         *AEJc(2) - FLOAT(ia)*AEJc(3)
                        IF (NDEJC.GT.3) atmp = atmp - FLOAT(iac)
     &                      *AEJc(NDEJC)
                        ztmp = Z(1) - FLOAT(in)*ZEJc(1) - FLOAT(ip)
     &                         *ZEJc(2) - FLOAT(ia)*ZEJc(3)
                        IF (NDEJC.GT.3) ztmp = ztmp - FLOAT(iac)
     &                      *ZEJc(NDEJC)
C                       residues must be heavier than alpha
                        if(atmp.le.4 . or. ztmp.le.2) cycle
                        izatmp = INT(1000*ztmp + atmp)
                        CALL WHERE(izatmp,nnuc,iloc)

                        IF(mulem.LE.NENdf) THEN
                           IF (ENDf(nnuc).EQ.0) ENDf(nnuc) = 1
                        ELSE
C                          Comment the following line and uncommment the one after for all exclusive spectra
                           IF (ENDf(nnuc).EQ.0) THEN
                              ENDf(nnuc) = 2
                              EXClusiv = .FALSE.
                           ENDIF
C                          IF (ENDf(nnuc).EQ.0) ENDf(nnuc) = 1
                        ENDIF
                        IF (ENDf(nnuc).EQ.1) THEN
                           NEXclusive = NEXclusive + 1
                           IF(NEXclusive.GT.NDExclus) THEN
                             WRITE(6,*)'INSUFFICIENT DIMENSION NDExclus'
                             WRITE(6,*)'INCREASE NDExclus AND RECOMPILE'
                             STOP 'INSUFFICIENT DIMENSION NDExclus'
                           ENDIF
                           INExc(NEXclusive) = nnuc
                        ENDIF
                     ENDDO
                  ENDDO
               ENDDO
            ENDDO
	    WRITE(6,*) 'Number of exclusive nuclei :',NEXclusive
         ENDIF
C
C--------check input for consistency
C
         IF (LHRtw.NE.0 .AND. LTUrbo.NE.1) THEN
            LTUrbo = 1
            WRITE (6,*) ' '
            WRITE (6,*) ' WARNING:'
            WRITE (6,*) ' WARNING:  LTURBO>1 is incompatible ',
     &                  ' WARNING:  with HRTW'
            WRITE (6,*) ' WARNING:  LTURBO has been set to 1'
            WRITE (6,*) ' '
         ENDIF
         IF (DEGa.GT.0) GCAsc = 1.
         IF (PEQc.GT.0) GCAsc = 1.  ! PCROSS
         IF (MSC*MSD.EQ.0 .AND. (MSD + MSC).NE.0 .AND. A(nnuc)
     &       .GT.1.0D0 .AND. AEJc(0).LE.1.D0) THEN
            WRITE (6,*) ' '
            WRITE (6,*) ' WARNING: Usually MSD and MSC should both '
            WRITE (6,*) ' WARNING: be taken into account'
            WRITE (6,*) ' '
         ENDIF
         IF (MSD.NE.0 .AND. AEJc(0).NE.1.D0) THEN
            MSD = 0
            WRITE (6,*) ' '
            WRITE (6,*) ' WARNING: MSD calculations suppressed'
            WRITE (6,*) ' WARNING: (possible for nucleons only)'
            WRITE (6,*) ' '
         ENDIF
         IF (MSC.NE.0 .AND. AEJc(0).EQ.0.0D0) THEN
            MSC = 0
            WRITE (6,*) ' '
            WRITE (6,*) ' WARNING!!!! MSC has been turned off '
            WRITE (6,*) ' WARNING!!!! (It is not allowed for '
            WRITE (6,*) ' WARNING!!!! photo-nuclear reactions)'
            WRITE (6,*) ' '
         ENDIF
         IF (LHRtw.NE.0 .AND. AEJc(0).EQ.0.0D0) THEN
            LHRtw = 0
            WRITE (6,*) ' '
            WRITE (6,*) ' WARNING!!!! HRTW has been turned off '
            WRITE (6,*) ' WARNING!!!! (It is not allowed for '
            WRITE (6,*) ' WARNING!!!! photo-nuclear reactions)'
            WRITE (6,*) ' '
         ENDIF
         IF (DEGa.NE.0 .AND. AEJc(0).NE.1.D0) THEN
            WRITE (6,*) ' '
            WRITE (6,*)
     &                 'FATAL: DEGAS allowed only for incident nucleons'
            WRITE (6,*) 'FATAL: Execution STOPPED'
            STOP ' DEGAS allowed only for incident nucleons'
         ENDIF
C--------------------------------------------------------------------------
         IF (LHMs.NE.0 .AND. ENDf(1).EQ.1) THEN
            WRITE (6,*) ' '
            WRITE (6,*)
     &                 'WARNING: HMS is incompatible with ENDF=1 option'
            WRITE (6,*) 'WARNING: and has been turned off'
            LHMs = 0
         ENDIF
         IF (LHMs.NE.0 .AND. (NDANG.NE.19 .OR. NDANG.NE.37)) THEN
            WRITE (6,*) ' '
            WRITE (6,*) 'FATAL: NDANG IN dimension.h MUST BE 19 or 37'
            WRITE (6,*)
     &'FATAL: FOR COMPATIBILITY OF ANGLE GRID IN EMPIRE AND HMS.'
            WRITE (6,*)
     &'FATAL: SET NDANG TO 19 or 37 AND RECOMPILE OR GIVE UP HMS OPTION'
            STOP 'FATAL: NDANG IN dimension.h MUST BE 19 or 37 for HMS'
         ENDIF
         IF (LHMs.NE.0 .AND. AEJc(0).GT.1.D0) THEN
            WRITE (6,*) ' '
            WRITE (6,*) 'FATAL: HMS allowed only for incident nucleons'
            WRITE (6,*) 'FATAL: and gammas -  Execution STOPPED'
            STOP ' HMS allowed only for incident nucleons and gammas'
         ENDIF
C--------------------------------------------------------------------------
         IF (FISbar(nnuc).NE.1 .AND. FISopt(nnuc).NE.0) THEN
            FISopt(nnuc) = 0
            WRITE (6,*) ' '
            WRITE (6,*) ' WARNING!!!! Subbarrier effects in the fission'
            WRITE (6,*) ' WARNING!!!! channel allowed only for FISBAR=1'
            WRITE (6,*) ' WARNING!!!! FISOPT has been set to 0 '
            WRITE (6,*) ' '
         ENDIF
         IF (DIRect.GT.0 .AND. INT(AEJc(0)).EQ.0) THEN
            DIRect = 0
            WRITE (6,*) ' '
            WRITE (6,*)
     &                 ' WARNING!!!! Direct mechanism is not supported '
            WRITE (6,*) ' WARNING!!!! for photo-nuclear reactions and '
            WRITE (6,*) ' WARNING!!!! has been turned off  '
            WRITE (6,*) ' '
         ENDIF
         IF (DIRect.EQ.3 .AND.
     &       (MOD(NINT(A(0)),2).NE.0 .OR. MOD(NINT(Z(0)),2).NE.0)) THEN
            DIRect = 1
            WRITE (6,*) ' '
            WRITE (6,*) ' WARNING!!!! DWBA mechanism is not supported'
            WRITE (6,*) ' WARNING!!!! for reactions on odd nuclei    '
            WRITE (6,*) ' WARNING!!!! DIRECT 1 has been set to 1     '
            WRITE (6,*) ' WARNING!!!! Small deformation is assumed   '
            WRITE (6,*) ' '
         ENDIF
         IF (DIRect.GT.0 .AND. KTRompcc.EQ.0) THEN
            KTRompcc = KTRlom(0,0)
            WRITE (6,*)
     &' WARNING: DIRPOT keyword is not specified, but DIRECT keyword > 0
     &'
            WRITE (6,*)
     &' WARNING: Please add line: DIRPOT ',-KTRompcc,
     &' to your INPUT file'
            WRITE (6,
     &'(''  Optical model parameters for direct inelastic scattering set
     & to RIPL #'',I4)') KTRompcc
            WRITE (6,*) ' '
         ENDIF

C--------input consistency check  *** done ***
C
C--------setup model matrix (IDNa) defining which model is used where
C                        ECIS   MSD   MSC   DEGAS   HMS   PCROSS
C                        1     2     3      4      5      6
C        1 neut. disc.   x     x     0      0      x      0
C        2 neut. cont.   0     x     x      x      x      x
C        3 prot. disc.   x     x     0      0      x      0
C        4 prot. cont.   0     x     x      x      x      x
C        5 gamma         0     0     x      x      0      x
C        6 alpha. cont.  0     0     0      0      0      x
C
C--------with x=1 if used and x=0 if not.
C
C--------initialize matrix with 0's
         DO i = 1, NDREGIONS   !over ejectiles/regions
            DO j = 1, NDMODELS !over models in the order as above
               IDNa(i,j) = 0
            ENDDO
         ENDDO
C--------set ECIS (.,1)
         IF (DIRect.GT.0) THEN
            IF (NPRoject.EQ.1) THEN
               IDNa(1,1) = 1
            ELSEIF (NPRoject.EQ.2) THEN
               IDNa(3,1) = 1
            ENDIF
         ENDIF
C--------set MSD  (.,2) (with MSD=1 discrete only if ECIS not used, with MSD=2 always)
         IF (MSD.EQ.1) THEN
            IF (NPRoject.EQ.1) THEN
               IF (DIRect.EQ.0) IDNa(1,2) = 1
               IDNa(2,2) = 1
            ELSEIF (NPRoject.EQ.2) THEN
               IF (DIRect.EQ.0) IDNa(3,2) = 1
               IDNa(4,2) = 1
            ENDIF
         ELSEIF(MSD.EQ.2) THEN
            IF (NPRoject.EQ.1) THEN
               IDNa(1,2) = 1
               IDNa(2,2) = 1
            ELSEIF (NPRoject.EQ.2) THEN
               IDNa(3,2) = 1
               IDNa(4,2) = 1
            ENDIF
         ENDIF
C--------set MSC  (.,3) (note no discrete transitions in MSC)
         IF (MSC.GT.0) THEN
            IDNa(2,3) = 1
            IDNa(4,3) = 1
            IF (GST.GT.0) IDNa(5,3) = 1
C-----------stop MSC charge-exchange if DEGAS,DDHMS or PCROSS active
            IF (DEGa.GT.0 .OR. LHMs.GT.0 .OR. PEQc.GT.0.) THEN
               IF (NPRoject.EQ.1) THEN
                  IDNa(4,3) = 0
               ELSEIF (NPRoject.EQ.2) THEN
                  IDNa(2,3) = 0
               ELSE
                  WRITE (6,*) ''
                  WRITE (6,*)
     &                       'DEGAS/PCROSS AND MSD/MSC ARE NOT COMPATIB'
                  WRITE (6,*)
     &                       'LE IN THIS CASE. EXECUTION S T O P P E D '
                  STOP 'ILLEGAL COMBINATION DEGAS/PCROSS + MSC/MSD !!!!'
               ENDIF
            ENDIF
         ENDIF
C--------set DEGAS  (.,4) (nucleons to discrete levels NOT blocked)
         IF (DEGa.GT.0) THEN
            IDNa(1,4) = 1
            IDNa(2,4) = 1
            IDNa(3,4) = 1
            IDNa(4,4) = 1
            IDNa(5,4) = 1
C-----------stop DEGAS gammas if calculated within MSC
            IF (GST.GT.0 .AND. MSC.GT.0) IDNa(5,4) = 0
C-----------stop DEGAS inelastic scattering if MSC and/or MSD active
            IF (MSC.GT.0 .OR. MSD.GT.0) THEN
               IF (NPRoject.EQ.2) THEN
                  IDNa(3,4) = 0
                  IDNa(4,4) = 0
               ELSEIF (NPRoject.EQ.1) THEN
                  IDNa(1,4) = 0
                  IDNa(2,4) = 0
               ELSE
                  WRITE (6,*) ''
                  WRITE (6,*) 'DEGAS AND MSD/MSC ARE NOT COMPATIBLE IN '
                  WRITE (6,*) 'THIS CASE. EXECUTION S T O P P E D !!!! '
                  STOP 'ILLEGAL COMBINATION DEGAS + MSC/MSD'
               ENDIF
            ENDIF
C-----------stop DEGAS particle channels if HMS active
            IF (LHMs.GT.0) THEN
               IDNa(1,4) = 0
               IDNa(2,4) = 0
               IDNa(3,4) = 0
               IDNa(4,4) = 0
            ENDIF
C-----------stop DEGAS particle channels if PCROSS active
CMH---------but leave  DEGAS for gamma emission
            IF(PEQc.GT.0)THEN
               IDNa(1, 4) = 0
               IDNa(2, 4) = 0
               IDNa(3, 4) = 0
               IDNa(4, 4) = 0
            ENDIF
         ENDIF
C--------set HMS  (.,5)
         IF (LHMs.GT.0) THEN
            IDNa(1,5) = 1
            IDNa(2,5) = 1
            IDNa(3,5) = 1
            IDNa(4,5) = 1
C-----------stop HMS inelastic scattering if MSC and/or MSD active
            IF (MSC.GT.0 .OR. MSD.GT.0) THEN
               IF (NPRoject.EQ.2) THEN
                  IDNa(3,5) = 0
                  IDNa(4,5) = 0
               ELSEIF (NPRoject.EQ.1) THEN
                  IDNa(1,5) = 0
                  IDNa(2,5) = 0
               ELSE
                  WRITE (6,*) ''
                  WRITE (6,*) 'HMS AND MSD/MSC ARE NOT COMPATIBLE IN '
                  WRITE (6,*) 'THIS CASE. EXECUTION S T O P P E D !!!! '
                  STOP 'ILLEGAL COMBINATION HMS + MSC/MSD'
               ENDIF
            ENDIF
         ENDIF
C--------set PCROSS  (.,6) cluster emission
         IF (PEQc.GT.0) THEN
            IDNa(1,6) = 0
            IDNa(2,6) = 1
            IDNa(3,6) = 0
            IDNa(4,6) = 1
            IDNa(5,6) = 1
            IDNa(6,6) = 1
C-----------stop PCROSS gammas if calculated within MSC
            IF (GST.GT.0 .AND. MSC.GT.0) IDNa(5,6) = 0
C-----------stop PCROSS inelastic scattering if MSC and/or MSD active
            IF (MSC.GT.0 .OR. MSD.GT.0) THEN
               IF (NPRoject.EQ.2) THEN
C                 IDNa(3,6) = 0
                  IDNa(4,6) = 0
               ELSEIF (NPRoject.EQ.1) THEN
C                 IDNa(1,6) = 0
                  IDNa(2,6) = 0
               ELSE
                  WRITE (6,*) ''
                  WRITE (6,*)
     &                       'PCROSS AND MSD/MSC ARE NOT COMPATIBLE IN '
                  WRITE (6,*) 'THIS CASE. EXECUTION S T O P P E D !!!! '
                  STOP 'ILLEGAL COMBINATION PCROSS + MSC/MSD'
               ENDIF
            ENDIF
C-----------stop PCROSS nucleon channels if HMS active
            IF (LHMs.GT.0) THEN
               IDNa(2,6) = 0
               IDNa(4,6) = 0
            ENDIF
C-----------stop PCROSS gamma channel if DEGAS active
            IF (DEGa.GT.0) IDNa(5,6) = 0
         ENDIF
C--------print IDNa matrix
         WRITE (6,*) ' '
         WRITE (6,*)
     &             '                      Use of preequilibrium models '
         WRITE (6,*)
     &             '                      ---------------------------- '
         WRITE (6,*) ' '
         WRITE (6,*) 'Exit channel       ECIS       MSD       MSC',
     &               '      DEGAS      HMS      PCROSS'
         WRITE (6,*) ' '
         WRITE (6,'('' neut. disc. '',8I10)') (IDNa(1,j),j = 1,NDMODELS)
         WRITE (6,'('' neut. cont. '',8I10)') (IDNa(2,j),j = 1,NDMODELS)
         WRITE (6,'('' prot. disc. '',8I10)') (IDNa(3,j),j = 1,NDMODELS)
         WRITE (6,'('' prot. cont. '',8I10)') (IDNa(4,j),j = 1,NDMODELS)
         WRITE (6,'('' gammas      '',8I10)') (IDNa(5,j),j = 1,NDMODELS)
         WRITE (6,'('' alpha cont. '',8I10)') (IDNa(6,j),j = 1,NDMODELS)
         WRITE (6,*) ' '
         WRITE(12,*) ' '
         WRITE(12,*)
     &            '                      Use of preequilibrium models '
         WRITE(12,*)
     &            '                      ---------------------------- '
         WRITE(12,*) ' '
         WRITE(12,*) 'Exit channel      ECIS    MSD    MSC',
     &              '   DEGAS   HMS   PCROSS'
         WRITE(12,*) ' '
         WRITE(12,'('' n to levels   '',8I7)')(IDNa(1,j),j = 1,NDMODELS)
         WRITE(12,'('' n to continuum'',8I7)')(IDNa(2,j),j = 1,NDMODELS)
         WRITE(12,'('' p to levels   '',8I7)')(IDNa(3,j),j = 1,NDMODELS)
         WRITE(12,'('' p to continuum'',8I7)')(IDNa(4,j),j = 1,NDMODELS)
         WRITE(12,'('' g             '',8I7)')(IDNa(5,j),j = 1,NDMODELS)
         WRITE(12,'('' a             '',8I7)')(IDNa(6,j),j = 1,NDMODELS)
         WRITE(12,*) ' '
C--------model matrix *** done ***
C--------reset some options if OMP fitting option selected
         IF (FITomp.GT.0) THEN
            IOUt = 1
            NEXreq = 10
            GCAsc = 1
            MSD = 0
            MSC = 0
            LHMs = 0
            DEGa = 0
            PEQc = 0
            MFPp = 1.3
            NNUcd = 1
            NNUct = 4
         ENDIF
C--------read nuclear deformations and masses
         CALL READNIX
C--------set projectile/ejectile masses
         DO nejc = 0, NDEJC
            EJMass(nejc) = (AEJc(nejc)*AMUmev
     &                     -ZEJc(nejc)*AMUele + XMAss_ej(nejc))/AMUmev
         ENDDO
C--------read number of reasonably known levels and level density parameter 'a'
C--------according to Mebel (GC) or EMPIRE systematics (dynamic l.d.)
         CALL READLDP
C--------fix-up deformations for CCFUS coupled channels
         IF (CSRead.EQ.( - 2.0D0) .AND. AEJc(0).GT.4.0D0) THEN
            DO j = 1, NSCc
               IF (QCC(j).EQ.0.0D0) THEN
                  IF (FLAm(j).GE.0.0D0) WRITE (6,*)
     &                ' Collective state ', ABS(FLAm(j)),
     &                ' in target (sequence number', j,
     &                ') has excitation energy of 0 MeV'
                  IF (FLAm(j).LE.0.0D0) WRITE (6,*)
     &                ' Collective state ', ABS(FLAm(j)),
     &                ' in projectile (sequence number', j,
     &                ') has excitation energy of 0 MeV'
                  WRITE (6,*)
     &            ' Likely the code was not able to find out this state'
                  WRITE (6,*)
     &                 ' you must set this energy in the optional input'
                  iccerr = 1
               ENDIF
               IF (BETcc(j).EQ.0.0D0) THEN
                  IF (FLAm(j).LT.0.0D0) BETcc(j) = DEFprj
                  IF (FLAm(j).GT.0.0D0) BETcc(j) = DEF(1,0)
               ENDIF
            ENDDO
            IF (iccerr.EQ.1) STOP 'CCFUS STATE MISSING'
         ENDIF
C--------fix-up deformations for coupled channels *** done ***
         DO nnuc = 0, NDNUC
            DO nejc = 0, NDEJC
               IOMwrite(nejc,nnuc) = 0
            ENDDO
         ENDDO

      ENDIF  ! END of EIN endif block

      NLW = NDLW
      CSFus = CSRead

C-----KTRLOM Optical Model control
C-----set o.m.p. for the incident channel
      DO nejc = 1, NDEJC
C        Selecting projectile from ejectiles
         IF (ZEJc(0).EQ.ZEJc(nejc) .AND. AEJc(0).EQ.AEJc(nejc)) then
           DO i = 1, NDNUC
C            Selecting target from residuals
             IF (Z(0).EQ.Z(i) .AND. A(0).EQ.A(i)) then
                KTRlom(0,0) = KTRlom(nejc,i)
C               Setting the normalization factor for OMP (used in covariance calculation)
                FNvvomp(0,0) = FNvvomp(Nejc,i)
                FNwvomp(0,0) = FNwvomp(Nejc,i)
                FNwsomp(0,0) = FNwsomp(Nejc,i)
                FNavomp(0,0) = FNavomp(Nejc,i)
                FNasomp(0,0) = FNasomp(Nejc,i)
                 GOTO 11
             ENDIF
            ENDDO
          ENDIF
      ENDDO

11    IF (AEJc(0).GT.4.0D0) KTRlom(0,0) = 0  ! HI
      IF (AEJc(0).EQ.0.0D0) KTRlom(0,0) = -1 ! photons

      IF (KTRompcc.GT.0 .AND. DIRect.EQ.2) THEN
         KTRlom(0,0) = KTRompcc
         KTRlom(NPRoject,NTArget) = KTRompcc
      ENDIF

C-----set giant resonance parameters for target
      GDRpar(1,0) = 0.0
      GDRpar(2,0) = 0.0
      GDRpar(3,0) = 0.0
      GDRpar(4,0) = 0.0
      GDRpar(5,0) = 0.0
      GDRpar(6,0) = 0.0
      GDRpar(7,0) = 1.0
      GDRpar(8,0) = 0.0
      GQRpar(1,0) = 0.0
      GQRpar(2,0) = 0.0
      GQRpar(3,0) = 0.0
      GQRpar(4,0) = 0.0
      GQRpar(5,0) = 0.0
      GQRpar(6,0) = 0.0
      GQRpar(7,0) = 1.0
      GQRpar(8,0) = 0.0
      GMRpar(1,0) = 0.0
      GMRpar(2,0) = 0.0
      GMRpar(3,0) = 0.0
      GMRpar(4,0) = 0.0
      GMRpar(5,0) = 0.0
      GMRpar(6,0) = 0.0
      GMRpar(7,0) = 1.0
      GMRpar(8,0) = 0.0
C-----compound nucleus 1
      nnuc = 1
      IF (NEX(1).GT.NDEX) THEN
         WRITE (6,*) ' NUMBER OF ENERGY BINS IN COMP. NUCL. SET TO',
     &               NDEX
         NEX(1) = MAX(NDEX - 2,2)
      ENDIF
C-----determination of discrete levels and pairing shift for cn
      CALL LEVREAD(nnuc)
      IF (ROPar(3,nnuc).EQ.0.0D0) THEN
         IF (Z(nnuc).GT.98.0D0 .OR. ROPaa(nnuc).LE.0.0D0) THEN
            delp = 12.0/SQRT(A(nnuc))
            IF (Z(nnuc)/2. - AINT(Z(nnuc)/2.).LT.0.01D0) ROPar(3,nnuc)
     &          = delp
         ELSE
            ROPar(3,nnuc) = delz(INT(Z(nnuc) + 0.001))
         ENDIF
         IF (XN(nnuc).GT.150.0D0 .OR. ROPaa(nnuc).LE.0.0D0) THEN
            delp = 12.0/SQRT(A(nnuc))
            IF (XN(nnuc)/2. - AINT(XN(nnuc)/2.).LT.0.01D0) ROPar(3,nnuc)
     &          = ROPar(3,nnuc) + delp
         ELSE
            ROPar(3,nnuc) = ROPar(3,nnuc) + deln(INT(XN(nnuc) + 0.001))
         ENDIF
      ENDIF
C-----set giant resonance parameters for CN
      GDRpar(8,nnuc) = 0.0
      GQRpar(1,nnuc) = 0.0
      GQRpar(2,nnuc) = 0.0
      GQRpar(3,nnuc) = 0.0
      GQRpar(4,nnuc) = 0.0
      GQRpar(5,nnuc) = 0.0
      GQRpar(6,nnuc) = 0.0
      GQRpar(7,nnuc) = 1.0
      GQRpar(8,nnuc) = 0.0
      GMRpar(1,nnuc) = 0.0
      GMRpar(2,nnuc) = 0.0
      GMRpar(3,nnuc) = 0.0
      GMRpar(4,nnuc) = 0.0
      GMRpar(5,nnuc) = 0.0
      GMRpar(6,nnuc) = 0.0
      GMRpar(7,nnuc) = 1.0
      GMRpar(8,nnuc) = 0.0
      AMAss(0) = (A(0)*AMUmev + XMAss(0))/AMUmev
      IF (Q(0,1).EQ.0.0D0) CALL BNDG(0,1,Q(0,1))

      EINl = EIN
      CALL KINEMA(EINl,EIN,EJMass(0),AMAss(0),ak2,1,RELkin)
      CALL LEVREAD(0)

      IF (DIRect.GT.0 .AND. FIRst_ein) THEN
                              ! Inelastic scattering by DWBA for all particles
C--------fix-up deformations and discrete levels for ECIS coupled channels
            ierr = IFINDCOLL()
C-----------Defining ICOller(i)
            ICOller(1) = ICOllev(1)
            DO i = 2, ND_nlv
               itmp = ICOllev(i)
               IF (itmp.GE.LEVcc) itmp = ICOllev(i) - LEVcc 
               ICOller(i) = itmp
            ENDDO
            IF (ierr.EQ.1) THEN
               WRITE (6,*) ' WARNING: Some collective discrete levels',
     &                     '  for target nucleus not found'
               WRITE (6,*) ' WARNING: check TARGET.LEV file '
            ELSEIF (ierr.EQ.2) THEN
               WRITE (6,*) ' WARNING: No discrete levels for target',
     &                     ' nucleus found'
               WRITE (6,*) ' WARNING: Direct cross section will not be',
     &                     ' calculated'
               WRITE (6,*) ' WARNING: Setting DIRECT to 0 '
               DIRect = 0
            ENDIF
      ENDIF

      EXCn = EIN + Q(0,1) + ELV(LEVtarg,0)
      EMAx(1) = EXCn
C-----set Q-value for CN production
      QPRod(1) = Q(0,1)
      ia = INT(A(0))
      iae = INT(AEJc(0))
      IF (ENDf(1).EQ.0.0D0) THEN
         IF (DEFga.NE.0.0D0) WRITE (12,
     &        '('' DEFGA='',F7.3,'' DEFGW='',F7.3,    '' DEFGP='',F7.3)'
     &        ) DEFga, DEFgw, DEFgp
      ENDIF
C-----WRITE heading on FILE6
      IF (IOUt.GT.0) THEN
         WRITE (6,*) ' '
         WRITE (6,*) ' '
         WRITE (6,*) ' '
         WRITE (6,99005)
         WRITE (6,
     &'('' Reaction '',I3,A2,''+'',I3,A2,'' at incident energy '',G9.3,'
     &' MeV'')') iae, SYMbe(0), ia, SYMb(0), EINl
         WRITE (6,99005)
         WRITE (6,*) ' '
         WRITE (6,'('' Compound nucleus energy'',F9.3,'' MeV'')') EXCn
         WRITE (6,'('' Projectile binding energy'',F8.3,'' MeV'')')
     &          Q(0,1)
      ENDIF
C
C-----determination of excitation energy matrix in cn
C
      ECUt(1) = ELV(NLV(1),1)
      NEX(1) = NEXreq
      IF (FITlev.GT.0.0D0) THEN
         ECUt(1) = 0.0
C--------If ENDF ne 0, then MAx(Ncut)=40 !!
C--------set ENDF flag to 0 (no ENDF file for formatting) if FITlev > 0
         Do i = 0,NDNuc
           ENDf(i) = 0
         Enddo
      ENDIF
C-----Energy step defined according to the CN excitation energy
      DE = (EMAx(1) - ECUt(1))/FLOAT(NEX(1) - 1)
C-----check whether any residue excitation is higher than CN
      qmin = 0.0
C
C     The block below is what it should be, but it does HRTW routine crash !!!!
C     i.e. DE can not be changed for the time being, this part should be revised
C     together with width fluctuation routine
C     RCN, 14 June 2005
C
C     DO i = 1, NDEJC
C        CALL BNDG(i,1,qtmp)
C        IF (qtmp.LT.qmin) qmin = qtmp
C     ENDDO
C     IF (EMAx(1) - ECUt(1).LT.EMAx(1) - qmin) THEN
C--------Energy redefined
C        DE = (EMAx(1) - qmin)/FLOAT(NEXreq - 1)
C--------Number of steps in CN outgoing energy grid redefined
C        NEX(1) = MAX(INT((EMAx(1)-ECUt(1))/DE),2)
C     ENDIF

      WRITE( 6,'(1x,A28,F6.1,A4)')
     &       'Energy step in calculations ',DE*1000.d0,' keV'

      DO i = 1, NEX(1)
         EX(i,1) = ECUt(1) + FLOAT(i - 1)*DE
      ENDDO
C-----set energy bin for recoils (max. energy is increased by 5%)
      IF (AEJc(NDEJC).GT.AEJc(3)) THEN
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
      RECcse(irec,NEX(1),1) = 1.0
C-----calculate compound nucleus level density
      ARGred = -1.
      IF (ADIv.EQ.0.0D0) CALL ROEMP(nnuc,0.0D0,0.024D0)
      IF (ADIv.EQ.1.0D0) CALL ROCOL(nnuc,0.0D0,2.D0)
C     <m2> could be added to the input
      IF (ADIv.EQ.2.0D0) CALL ROGC(nnuc,0.24D0)
      IF (ADIv.EQ.3.0D0) CALL ROHFBCS(nnuc)
      IF (ADIv.GT.3.0D0) CALL ROCOL(nnuc,0.0D0,1.D0)
      IF (IOUt.EQ.6) THEN
         ia = INT(A(nnuc))
         WRITE (6,'(1X,/,'' LEVEL DENSITY FOR '',I3,''-'',A2,/)') ia,
     &          SYMb(nnuc)
         WRITE (6,99010) (EX(i,nnuc),(RO(i,j,nnuc)*EXP(ARGred),j = 1,12)
     &                   ,i = 1,NEX(nnuc))
      ENDIF
C
C-----other decaying nuclei
C
      DO nnuc = 1, NNUcd
         DO nejc = 1, NEJcm
            ares = A(nnuc) - AEJc(nejc)
            zres = Z(nnuc) - ZEJc(nejc)
C           residual nuclei must be heavier than alpha
            if(ares.le.4 . or. zres.le.2) cycle

            izares = INT(1000*zres + ares)
            CALL WHERE(izares,nnur,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (6,'('' NO LOCATION ASCRIBED TO NUCLEUS '',I8)')
     &                izares
               STOP
            ENDIF

            IF (EMAx(nnur).EQ.0.0D0) THEN
C--------------determination of discrete levels and pairing shifts for rn
               CALL LEVREAD(nnur)
               IF (ROPar(3,nnur).EQ.0.0D0) THEN
                  IF (Z(nnur).GT.98.0D0 .OR. ROPaa(nnur).LE.0.0D0) THEN
                     delp = 12.0/SQRT(A(nnur))
                     IF (Z(nnur)/2. - AINT(Z(nnur)/2.).LT.0.01D0)
     &                   ROPar(3,nnur) = delp
                  ELSE
                     ROPar(3,nnur) = delz(INT(Z(nnur) + 0.001))
                  ENDIF
                  IF (XN(nnur).GT.150.D0 .OR. ROPaa(nnur).LE.0.0D0) THEN
                     delp = 12.0/SQRT(A(nnur))
                     IF (XN(nnur)/2. - AINT(XN(nnur)/2.).LT.0.01D0)
     &                   ROPar(3,nnur) = ROPar(3,nnur) + delp
                  ELSE
                     ROPar(3,nnur) = ROPar(3,nnur)
     &                               + deln(INT(XN(nnur) + 0.001))
                  ENDIF
               ENDIF
C--------------determination of giant resonance parameters for residual nuclei
               GDRpar(8,nnur) = 0.0
               GQRpar(1,nnur) = 0.0
               GQRpar(2,nnur) = 0.0
               GQRpar(3,nnur) = 0.0
               GQRpar(4,nnur) = 0.0
               GQRpar(5,nnur) = 0.0
               GQRpar(6,nnur) = 0.0
               GQRpar(7,nnur) = 1.0
               GQRpar(8,nnur) = 0.0
               GMRpar(1,nnur) = 0.0
               GMRpar(2,nnur) = 0.0
               GMRpar(3,nnur) = 0.0
               GMRpar(4,nnur) = 0.0
               GMRpar(5,nnur) = 0.0
               GMRpar(6,nnur) = 0.0
               GMRpar(7,nnur) = 1.0
               GMRpar(8,nnur) = 0.0
            ENDIF
C-----------determination of excitation energy matrix in res. nuclei
            ECUt(nnur) = ELV(NLV(nnur),nnur)
            IF (FITlev.GT.0.0D0) ECUt(nnur) = 0.0
            IF (Q(nejc,nnuc).EQ.0.0D0 .OR. Q(nejc,nnuc).EQ.99)
     &                           CALL BNDG(nejc,nnuc,Q(nejc,nnuc))
            emaxr = 0.0
            IF (NEX(nnuc).GT.0) emaxr = EX(NEX(nnuc),nnuc)
     &                                  - Q(nejc,nnuc)
            EMAx(nnur) = DMAX1(emaxr,EMAx(nnur))
            NEX(nnur) = MAX(INT((EMAx(nnur)-ECUt(nnur))/DE + 1.0),0)
            NEXr(nejc,nnuc) = MAX(INT((emaxr-ECUt(nnur))/DE + 1.0),0)

C-----------Coulomb barrier (20% decreased) setting lower energy limit
            culbar = 0.d0
            IF(ZEJc(Nejc).GT.1) culbar = 0.8*ZEJc(Nejc)*Z(Nnur)*ELE2
     &         /(1.3d0*(AEJc(Nejc)**0.3333334 + A(Nnur)**0.3333334))

            IF (NEX(nnur).GT.NDEX) THEN
               WRITE (6,*)
               WRITE (6,'('' WARNING: NUMBER OF BINS '',I3,
     &                    '' IN RESIDUAL NUCLEUS '',I3,A1,A2,
     &         '' EXCEEDS DIMENSIONS '',I3)')  NEX(nnur), NINT(A(nnur)),
     &         '-',SYMb(nnur),NDEX
               WRITE (6,
     &         '(''          Reaction '',I3,A1,A2,'' -> '',I3,A1,A2,
     &           ''  +  '',I2,A1,A2,'' NEGLECTED '')')
     &          NINT(A(nnuc)),'-',SYMb(nnuc),
     &          NINT(ares),   '-',SYMb(nnur),
     &          NINT(AEJc(nejc)),'-',SYMbe(nejc)
               WRITE (6,*)
     &          '         TO CONSIDER IT, YOU HAVE TO DECREASE ',
     &          ' NEX IN THE INPUT '
               WRITE (6,*)
     &          '         OR INCREASE NDEX PARAMETER IN Dimension.h'
               WRITE (6,'('' WARNING: EMAXr : '',F7.2,
     &            ''; COULOMB BARRIER : '',F7.2)') emaxr, culbar
               WRITE (6,*)
               EMAx(nnur) = 0.d0
               NEX(nnur) = 0
               NEXr(nejc,nnuc) = 0
               Q(nejc,nnuc) = 99.d0
               CYCLE
            ENDIF

            IF (NEX(nnur).GT.0) THEN
               DO i = 1, NEX(nnur)
                  IF (Z(1).EQ.Z(nnur) .AND. FITlev.EQ.0.0D0) THEN
                     EX(NEX(nnur) - i + 1,nnur) = EMAx(nnur)
     &                  - FLOAT(i - 1)*DE
                  ELSE
                     EX(i,nnur) = ECUt(nnur) + FLOAT(i - 1)*DE
                  ENDIF
               ENDDO
            ENDIF
            IF (Z(1).EQ.Z(nnur) .AND. NEX(nnur).GT.0) ECUt(nnur)
     &          = EX(1,nnur)

            IF(Q(nejc,nnuc).GE.98.5d0) CYCLE
C-----------determination of Q-value for isotop production
            qtmp = QPRod(nnuc) - Q(nejc,nnuc)
            IF (qtmp.GT.QPRod(nnur)) QPRod(nnur) = qtmp
            IF (FITlev.GT.0.0D0) ECUt(nnur) = 0.0
C-----------determination of etl matrix and transmission coeff. calculation
C-----------first 4 elements are set independently in order to get more
C-----------precise grid at low energies. from the 5-th element on the step
C-----------is de (bin width).
C-----------determination of etl matrix
            netl = 6
            IF (NEX(nnuc).GT.0) netl =
     &         INT((EX(NEX(nnuc),nnuc) - Q(nejc,nnuc))/DE) + 6
            IF (netl.GT.NDETL) THEN
               WRITE (6,*)
     &             ' WARNING: netl = ',netl,' > NDETL = ',NDETL
               WRITE (6,
     &         '(''          Reaction '',I3,A1,A2,'' -> '',I3,A1,A2,
     &           ''  +  '',I2,A1,A2,'' NEGLECTED '')')
     &          NINT(A(nnuc)),'-',SYMb(nnuc),
     &          NINT(ares),   '-',SYMb(nnur),
     &          NINT(AEJc(nejc)),'-',SYMbe(nejc)
               IF((EX(NEX(nnuc),nnuc) - Q(nejc,nnuc)).LT.culbar) THEN
                 EMAx(nnur) = 0.d0
                 NEX(nnur) = 0
                 NEXr(nejc,nnuc) = 0
                 Q(nejc,nnuc) = 99.d0
                 CYCLE
               ELSE
                 WRITE (6,*)
     &             ' OUT OF BOUNDARY; DECREASE NEX IN INPUT OR INCREASE'
     &             , ' NDEX IN dimension.h AND RECOMPILE'
                 STOP 10
               ENDIF
            ENDIF
            IF (NEXr(nejc,nnuc).GT.0 .AND. NEX(nnuc).GT.0) THEN
               ETL(5,nejc,nnur) = EX(NEX(nnuc),nnuc)
     &                            - EX(NEXr(nejc,nnuc),nnur)
     &                            - Q(nejc,nnuc)
            ELSE
               ETL(5,nejc,nnur) = 0.
            ENDIF
            IF (nejc.EQ.1) ETL(5,nejc,nnur) = 0.
Cpr         WRITE(6,*) 'etl(5,.),netl',etl(5,nejc,nnur),netl
Cpr         etlmax=EX(NEX(NNUC),NNUC)-Q(NEJC,NNUC)
Cpr         WRITE(6,*) 'etlmax',etlmax
            ETL(1,nejc,nnur) = 0
            ETL(2,nejc,nnur) = 0.1*ETL(5,nejc,nnur)
            ETL(3,nejc,nnur) = 0.2*ETL(5,nejc,nnur)
            ETL(4,nejc,nnur) = 0.5*ETL(5,nejc,nnur)
            DO ietl = 6, netl
               ETL(ietl,nejc,nnur) = ETL(ietl - 1,nejc,nnur) + DE
            ENDDO
Cpr         WRITE(6,*)
Cpr         >        'TL ENERGIES FOR TARGET A=',A(NNUR),' PROJECTILE A=',
Cpr         >        AEJC(NEJC),' Z=',ZEJC(NEJC)
Cpr         DO I=1,NETL
Cpr         WRITE(6,*) I,ETL(I,NEJC,NNUR)
Cpr         END DO
C-----------calculate tramsmission coefficients
            IF (FITlev.EQ.0) THEN
               CALL TLEVAL(nejc,nnur,nonzero)
C--------------print transmission coefficients
               IF (nonzero .AND. IOUt.EQ.5) THEN
                  WRITE (6,*)
                  WRITE (6,*) ' Transmission coefficients for '
                  WRITE (6,'(1x,A15,I3,A3,I3,A3,F4.1)')
     &                    ' Projectile: A=', INT(AEJc(nejc)), ' Z=',
     &                   INT(ZEJc(nejc)), ' S=', SEJc(nejc)
                  WRITE (6,'(1x,A11,I3,A3,I3,A3,F4.1,A3,I2)')
     &                    ' TARGET: A=', INT(A(nnur)), ' Z=',
     &                   INT(Z(nnur)), ' S=', SNGL(XJLv(1,nnur)),
     &                   ' P=', INT(LVP(1,nnur))
                  DO i = 1, netl
                     IF (TL(i,1,nejc,nnur).GT.0.0) WRITE (6,99010)
     &                   ETL(i,nejc,nnur), (TL(i,j,nejc,nnur),j = 1,12)
                  ENDDO
                  WRITE (6,'(1X,/)')
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
      WRITE (6,*) ' '
      WRITE (6,*) 'Total number of nuclei considered ', NNUct
      WRITE (6,*) ' '
C-----calculate residual nucleus level density
      DO nnur = 2, NNUct
         IF (NEX(nnur).GE.1 .OR. FITlev.GT.0) THEN
            IF (ADIv.EQ.0.0D0) CALL ROEMP(nnur,0.0D0,0.024D0)
            IF (ADIv.EQ.1.0D0) CALL ROCOL(nnur,0.D0,2.D0)
C-----------<m2> could be added to the input ( to use 0.124 if needed)
            IF (ADIv.EQ.2.0D0) CALL ROGC(nnur,0.24D0)
C           IF (ADIv.EQ.2.0D0) CALL ROGC(nnur, 0.146D0)
            IF (ADIv.EQ.3.0D0) CALL ROHFBCS(nnur)
            IF (ADIv.GT.3.0D0) CALL ROCOL(nnur,0.D0,1.D0)
            IF (IOUt.EQ.6) THEN
               ia = INT(A(nnur))
               WRITE (6,'(1X,/,'' LEVEL DENSITY FOR '',I3,''-'',A2,/)')
     &                ia, SYMb(nnur)
               WRITE (6,99010) (EX(i,nnur),(RO(i,j,nnur),j = 1,12),
     &                         i = 1,NEX(nnur))
            ENDIF
         ELSE
            ia = INT(A(nnur))
            iz = INT(Z(nnur))
         ENDIF
      ENDDO
      DO i = 1, NDLW
         DRTl(i) = 1.0
      ENDDO
      IF (FITlev.GT.0.D0) THEN
C--------remove potentially empty omp files
C--------OMPAR.DIR
         CLOSE (33,STATUS = 'DELETE')
C--------OMPAR.RIPL
         CLOSE (29,STATUS = 'DELETE')
         STOP 'PLOTS DONE'
      ENDIF
C---- fission input is created if it does not exist and FISSHI=0
      DO nnuc = 1, NNUct
         FISsil(nnuc) = .TRUE.
         IF (FISshi(nnuc).EQ.0. .AND.
     &       (Z(nnuc).LT.78. .OR. A(nnuc).LT.224.)) FISsil(nnuc)
     &       = .FALSE.
         IF (FISshi(nnuc).EQ.1.) THEN
            xfis = 0.0205*Z(nnuc)**2/A(nnuc)
            IF (xfis.LT.0.3D0) FISsil(nnuc) = .FALSE.
         ENDIF
         IF (FISshi(nnuc).EQ.2.) FISsil(nnuc) = .FALSE.
      ENDDO
      INQUIRE (FILE = 'FISSION.INP',EXIST = gexist)
      IF (.NOT.gexist) THEN
         OPEN (79,FILE = 'FISSION.INP',STATUS = 'UNKNOWN')
         DO nnuc = 1, NNUcd
            IF (FISsil(nnuc) .AND. FISshi(nnuc).EQ.0) CALL INPFIS(nnuc)
         ENDDO
         CLOSE (79)
      ENDIF
99005 FORMAT (1X,60('='))
99010 FORMAT (1X,13G10.4)
      END
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
C Dummy arguments
C
      INTEGER Nnuc
C
C Local variables
C
      CHARACTER*5 chelem
      CHARACTER*110 ch_iuf
      CHARACTER*3 ctmp3
      DOUBLE PRECISION egamma, pelm, pgamma, qn, sum, t12, xicc
      DOUBLE PRECISION dd0tmp, dd0_unc, ss0tmp, ss0_unc, gggtmp, ggg_unc
      CHARACTER*9 finp
      CHARACTER*1 dum
      INTEGER ia, iar, ifinal, ilv, istart, isum, itmp2, iz, izr, nbr,
     &        ndb, ndbrlin, ngamr, nlvr, nmax, izatmp
      INTEGER INT
      LOGICAL LREad

      ia = A(Nnuc) + 0.001
      iz = Z(Nnuc) + 0.001

C     Looking for Dobs and Gg for compound (resonances are stored for target nucleus)
      IF (Nnuc.eq.0 .AND. (AEJc(0).EQ.1 .AND. ZEJc(0).EQ.0) ) THEN ! only for neutrons
        OPEN (47,FILE = '../RIPL-2/resonances/resonances0.dat',
     &      STATUS = 'old',ERR = 65)
        READ (47,'(///)') ! Skipping first 4 title lines
        DO i = 1, 296
          READ (47,'(2i4,  17x,2(e9.2,2x),2(f4.2,2x),2(F5.1,1x))',
     &     END = 60, ERR = 60) nztmp, natmp,
     &         dd0tmp, dd0_unc, ss0tmp, ss0_unc, gggtmp, ggg_unc
          IF (nztmp.NE.Iz .OR. natmp.NE.Ia) CYCLE
          D0_obs = dd0tmp
          D0_unc = dd0_unc
          S0_obs = ss0tmp
          S0_unc = ss0_unc
          Gg_obs = gggtmp
          Gg_unc = ggg_unc
        ENDDO
   60   CLOSE (47)
        GOTO 70
   65   WRITE (6,*) ' WARNING: ',
     &   '../RIPL-2/resonances/resonances0.dat file not found '
        WRITE (6,*) ' WARNING: D0 and gamma width are not available '
C       We could put here whatever systematics for D0 or Gg we want
      ENDIF

   70 LREad = .TRUE.
      izatmp = INT(1000*iz + ia)
      DO itmp = 0,NDNuc
        IF(NSTOred(itmp).eq.izatmp) THEN
          LREad = .FALSE.
          GOTO 50
        ENDIF
      ENDDO

   50 IF(.NOT.LREAD) then
        NLV(Nnuc) = NLV(itmp)
        NCOmp(Nnuc) = NCOmp(itmp)
        DO ilv = 1, NLV(Nnuc)
          ELV(ilv,Nnuc) = ELV(ilv,itmp)
          XJLv(ilv,Nnuc) = XJLv(ilv,itmp)
          LVP(ilv,Nnuc) = LVP(ilv,itmp)
          DO nbr = 1, NDBR
            BR(ilv,nbr,1,Nnuc) = BR(ilv,nbr,1,itmp)
            BR(ilv,nbr,2,Nnuc) = BR(ilv,nbr,2,itmp)
            BR(ilv,nbr,3,Nnuc) = BR(ilv,nbr,3,itmp)
          ENDDO
        ENDDO
      ELSE
C-------set ground state in case nucleus not in file
        NLV(Nnuc) = 1
        NCOmp(Nnuc) = 1
        ELV(1,Nnuc) = 0.0
        LVP(1,Nnuc) = 1
        XJLv(1,Nnuc) = 0.0
        IF (A(Nnuc) - 2.0*INT(A(Nnuc)/2.0).GT.0.01D0) XJLv(1,Nnuc) = 0.5
C-------set ground state *** done ***
        IF(.NOT.FILevel) THEN
C---------constructing input and filenames
           WRITE (ctmp3,'(I3.3)') iz
           finp = 'z'//ctmp3//'.dat'
           OPEN (13,FILE = '../RIPL-2/levels/'//finp,STATUS = 'OLD',
     &         ERR = 400)
           NSTored(nnuc) = izatmp
        ELSE
           REWIND 13
        ENDIF
  100   READ (13,'(A5,6I5,2f12.6)',END = 300) chelem, iar, izr, nlvr,
     &      ngamr, nmax, itmp2, qn
C-----nmax is a number of levels that constitute a complete scheme as
C-----estimated by Belgya for RIPL-2.
C-----It is used, but a visual check with FITLEV is always recommended.
        IF (ia.NE.iar .OR. iz.NE.izr) THEN
           DO ilv = 1, nlvr + ngamr
             READ (13,'(A1)') dum
           ENDDO
           GOTO 100
        ELSE
C--------create file with levels (*.lev)
C--------NLV   number of levels with unique spin and parity
C--------NCOMP number of levels up to which the level scheme is estimated
C--------to be complete
C
           IF (.NOT.FILevel) THEN
             BACKSPACE (13)
             READ (13,'(A110)') ch_iuf
             WRITE (14,'(A110)') ch_iuf
           ENDIF
           IF (nlvr.NE.0) THEN
             IF (NLV(Nnuc).EQ.1 .AND. nmax.GT.1) NLV(Nnuc)
     &          = MIN(NDLV,nmax)
C------------limit to max. of 40 levels if ENDF active
             IF (ENDf(Nnuc).NE.0.0D0) NLV(Nnuc) = MIN(NLV(Nnuc),40)
             IF (NCOmp(Nnuc).EQ.1 .AND. nlvr.GT.1) NCOmp(Nnuc)
     &          = MIN(NDLV,nlvr)
             IF (.NOT.FILevel) THEN
               DO ilv = 1, nlvr + ngamr
                  READ (13,'(A110)') ch_iuf
                  WRITE (14,'(A110)') ch_iuf
               ENDDO
               DO ilv = 1, nlvr + ngamr
                  BACKSPACE (13)
               ENDDO
             ENDIF
C------------levels for nucleus NNUC copied to file *.lev
             DO ilv = 1, NLV(Nnuc)
               READ (13,'(I3,1X,F10.6,1X,F5.1,I3,1X,E10.2,I3)') istart,
     &               ELV(ilv,Nnuc), XJLv(ilv,Nnuc), LVP(ilv,Nnuc), t12,
     &               ndbrlin
               IF (ELV(ilv,Nnuc).GT.qn) THEN
                  NLV(Nnuc) = max(ilv - 1,1)
                  WRITE (6,'('' WARNING:'')')
                  WRITE (6,'('' WARNING: Element ='',A5,2x,2HZ=,I3)')
     &                   chelem, izr
                  WRITE (6,
     &'('' WARNING: Excited state '',I3,                             ''
     &is above neutron binding energy '',F6.3,                       ''
     &MeV'')') ilv, qn
                  WRITE (6,'('' WARNING: Number of levels set to '',I3)'
     &                   ) NLV(Nnuc)
                  GOTO 200
               ENDIF
               IF (ilv.NE.1) THEN
                  IF (ELV(ilv,Nnuc).EQ.0.) THEN
                     WRITE (6,'('' WARNING:'')')
                     WRITE (6,'('' WARNING: Element ='',A5,2x,2HZ=,I3)')
     &                      chelem, izr
                     WRITE (6,
     &'('' WARNING: excited state '',I3,                             ''
     &has got zero excitation energy'')') ilv
                  ENDIF
                  IF (ilv.EQ.1 .AND. ELV(ilv,Nnuc).GT.4.) THEN
                     WRITE (6,'('' WARNING:'')')
                     WRITE (6,'('' WARNING: Element ='',A5,2x,2HZ=,I3)')
     &                      chelem, izr
                     WRITE (6,
     &'('' WARNING: excited state No.'',I3,                          ''
     &has energy of '',F6.3,'' MeV'')') ilv, ELV(ilv,Nnuc)
                  ENDIF
                  IF (ndbrlin.GT.NDBR) THEN
                     WRITE (6,'('' WARNING:'')')
                     WRITE (6,'('' WARNING: Element ='',A5,2x,2HZ=,I3)')
     &                      chelem, izr
                     WRITE (6,
     &'('' WARNING: too many gamma decays ='',                       I3)
     &') ndbrlin
                     WRITE (6,
     &'('' WARNING: Dimension allows for ='',                        I3)
     &') NDBR
                     WRITE (6,'('' WARNING: some gammas discarded'')')
                  ENDIF
C-----------------clean BR matrix
                  DO nbr = 1, NDBR
                     BR(ilv,nbr,1,Nnuc) = 0.
                     BR(ilv,nbr,2,Nnuc) = 0.
                     BR(ilv,nbr,3,Nnuc) = 0.
                  ENDDO
                  ndb = MIN(ndbrlin,NDBR)
                  sum = 0.0
                  isum = 0
                  DO nbr = 1, ndb
                     READ (13,'(39X,I4,1X,F10.3,3(1X,E10.3))') ifinal,
     &                     egamma, pgamma, pelm, xicc
C--------------------only gamma decay is considered up to now
                     IF (pelm.GT.0.) THEN
                        sum = sum + pelm
                        isum = isum + 1
                        BR(ilv,isum,1,Nnuc) = ifinal    !final level #
                        BR(ilv,isum,2,Nnuc) = pelm      !branching
                        BR(ilv,isum,3,Nnuc) = xicc      !int. convertion coeff.
                     ENDIF
                  ENDDO
                  IF (sum.NE.1.D0 .AND. sum.NE.0.D0) THEN
                     sum = 1.D0/sum
                     DO nbr = 1, isum
                        BR(ilv,nbr,2,Nnuc) = BR(ilv,nbr,2,Nnuc)*sum
                     ENDDO
                  ENDIF
               ENDIF
            ENDDO
         ENDIF
      ENDIF
  200 IF (.NOT.FILevel) CLOSE (13)
      ENDIF
      RETURN
  300 IF(FILevel) THEN
        WRITE (6,
     &  '('' WARNING: levels for nucleus A='',I3,'' Z='',I3,
     &  '' not found in local levels file (...lev) '')') ia, iz
      ELSE
        WRITE (6,
     &  '('' WARNING: levels for nucleus A='',I3,'' Z='',I3,
     &  '' not found in RIPL database '')') ia, iz
      ENDIF
      IF (.NOT.FILevel) CLOSE (13)
      RETURN
  400 WRITE (6,'('' WARNING: RIPL levels database not found '')')
      IF (.NOT.FILevel) CLOSE (13)
      END
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
      INTEGER i, j, ia, iae, iexclus
      INTEGER IFIX
      REAL SNGL

      IF (ENDf(1).NE.0.0D0 .AND. FIRst_ein) THEN
        WRITE (12,*) ' '
        IF (KTRompcc.GT.0 .AND. DIRect.GT.0) WRITE (12,*)
     &     'Inelastic o. m. parameters: RIPL catalog number ', KTRompcc
        WRITE (12,*) 'Neutron   o. m. parameters: RIPL catalog number ',
     &            KTRlom(1,1)
        WRITE (12,*) 'Proton    o. m. parameters: RIPL catalog number ',
     &            KTRlom(2,1)
        WRITE (12,*) 'Alpha     o. m. parameters: RIPL catalog number ',
     &            KTRlom(3,1)
        IF (NEMc.GT.0) WRITE (12,*)
     &               'Cluster   o. m. parameters: RIPL catalog number ',
     &            KTRlom(NDEJC,1)
        WRITE (12,*)
        WRITE (12,99050)
        WRITE (12,*)
        WRITE (12,99005) (IFIX(SNGL(AEJc(i))),SYMbe(i),i = 1,NEJcm)
        WRITE (12,*)

      ENDIF

99005 FORMAT ('    Nucleus   ',12(6X,I2,A2))
99010 FORMAT (1X,I3,'-',A2,'-',I3,4X,12F10.3)
99015 FORMAT (1X,I3,'-',A2,'-',I3,2X,'<',1x,12F10.3)

      WRITE (6,*)
      WRITE (6,*)
      WRITE (6,99050)
      WRITE (6,*)
      WRITE (6,99005) (IFIX(SNGL(AEJc(i))),SYMbe(i),i = 1,NEJcm)
      WRITE (6,*)

      iexclus = 0
      DO i = 1, NNUcd
        IF(ENDf(i).GT.0.D0) THEN
          IF (ENDf(i).EQ.1.0D0)
     &      WRITE ( 6,99010) IFIX(SNGL(Z(i))),SYMb(i),
     &                   IFIX(SNGL(A(i))), (Q(j,i),j = 1,NEJcm)
          IF (ENDf(i).EQ.2.0D0) THEN
            iexclus = 1
            WRITE ( 6,99015) IFIX(SNGL(Z(i))),SYMb(i),
     &                   IFIX(SNGL(A(i))), (Q(j,i),j = 1,NEJcm)
          ENDIF
        ELSE
          WRITE ( 6,99010) IFIX(SNGL(Z(i))),SYMb(i),
     &                   IFIX(SNGL(A(i))), (Q(j,i),j = 1,NEJcm)
        ENDIF
      ENDDO

      IF (iexclus.EQ.1) THEN
        WRITE( 6,*)
        WRITE( 6,*) ' < indicates inclusive spectra only'
      ENDIF

      IF (FIRst_ein) THEN
        DO i = 1, NNUcd
          IF(ENDf(i).GT.0.D0) THEN
            IF (ENDf(i).EQ.1.0D0)
     &        WRITE (12,99010) IFIX(SNGL(Z(i))),SYMb(i),
     &                   IFIX(SNGL(A(i))), (Q(j,i),j = 1,NEJcm)
            IF (ENDf(i).EQ.2.0D0)
     &        WRITE (12,99015) IFIX(SNGL(Z(i))),SYMb(i),
     &                   IFIX(SNGL(A(i))), (Q(j,i),j = 1,NEJcm)
          ELSE
            WRITE (12,99010) IFIX(SNGL(Z(i))),SYMb(i),IFIX(SNGL(A(i))),
     &                   (Q(j,i),j = 1,NEJcm)
          ENDIF
        ENDDO

        IF (iexclus.EQ.1) THEN
          WRITE(12,*)
          WRITE(12,*) ' < indicates inclusive spectra only'
        ENDIF

        WRITE (12,*) '                                                '
        WRITE (12,*) 'RESULTS                                         '
        WRITE (12,*) '                                                '
        WRITE (12,*) 'MF=3 Neutron cross sections                     '
        WRITE (12,*) '     EMPIRE calculations were adopted for:      '
        WRITE (12,*) '   MT=1 Total                                   '
        WRITE (12,*) '   MT=2 Elastic scattering                      '
        WRITE (12,*) '   MT=4, 51-91 Inelastic scattering             '
        WRITE (12,*) '   MT=102 Capture                               '
        WRITE (12,*) '   MT=16   (n,2n)                               '
        WRITE (12,*) '   MT=17   (n,3n)                               '
        WRITE (12,*) '   MT=22   (n,na)                               '
        WRITE (12,*) '   MT=24   (n,2na)                              '
        WRITE (12,*) '   MT=28   (n,np)                               '
        WRITE (12,*) '   MT=45   (n,npa)                              '
        WRITE (12,*) '   MT=103, 600-649 (n,p)                        '
        WRITE (12,*) '   MT=107, 800-849 (n,a)                        '
        WRITE (12,*) '   MT=112  (n,pa)                               '
        WRITE (12,*) '                                                '
        WRITE (12,*) 'MF=4 Angular distributions of secondary neutrons'
        WRITE (12,*) '     EMPIRE calculations were adopted           '
        WRITE (12,*) '                                                '
        WRITE (12,*) 'MF=6 Energy-angle distributions of reaction     '
        WRITE (12,*) '     products; EMPIRE calculations were adopted '
        WRITE (12,*) '                                                '
        WRITE (12,*) 'MF=12 Transition probablility arrays for photon '
        WRITE (12,*) '      production; taken from the RIPL-2         '
        WRITE (12,*) '                                                '
        WRITE (12,*) 'MF=14 Photon angular distributions              '
        WRITE (12,*) '      isotropic distributions were assumed      '
        WRITE (12,*) '                                                '
        WRITE (12,*) 'REFERENCES                                      '
        WRITE (12,*) '                                                '
        WRITE (12,*) '[He01] M. Herman  EMPIRE-II Statistical Model   '
        WRITE (12,*) '   Code for Nuclear Reaction Calculations, in   '
        WRITE (12,*) '   Nuclear Reaction Data and Nuclear Reactors,  '
        WRITE (12,*) '   eds. N.Paver, M. Herman and A.Gandini, ICTP  '
        WRITE (12,*) '   Lecture Notes 5 (ICTP Trieste, 2001) pp.137. '
        WRITE (12,*) '                                                '
        WRITE (12,*) '[He02] Recent Development of the Nuclear        '
        WRITE (12,*) '   Reaction Model Code Empire, M. Herman,       '
        WRITE (12,*) '   R. Capote, P. Oblozinsky, M. Sin, A. Trkov,  '
        WRITE (12,*) '   A. Ventura, V. Zerkin, International         '
        WRITE (12,*) '   Conference on Nuclear Data for Science &     '
        WRITE (12,*) '   Technology - ND2004 September 26 - October 1,'
        WRITE (12,*) '   2004, Santa Fe, New Mexico.                  '
        WRITE (12,*) '                                                '
        WRITE (12,*) '[Ri03] RIPL-2: Reference Input Parameter Library'
        WRITE (12,*) '   to be published, see www-nds.iaea.org/RIPL-2/'
        WRITE (12,*) '                                                '
        WRITE (12,*) '************************************************'
        WRITE (12,*) '                                                '
        WRITE (12,*)

        WRITE (6,*)
        WRITE (6,*)

      ENDIF

      IF (FISshi(1).NE.0) THEN
         WRITE (6,99025)
99025    FORMAT ('    Nucleus   ',6X,'Shell Corr.  Deform.',
     &           '  Fiss. barr.')
         WRITE (6,99030)
99030    FORMAT ('              ',6X,'  (J=0)       (J=0)    ',
     &           '    (J=0)')
         WRITE (6,*)
         DO i = 1, NNUct
            IF (EMAx(i).NE.0.0D0) WRITE (6,99045) IFIX(SNGL(Z(i))),
     &          SYMb(i), IFIX(SNGL(A(i))), SHC(i), DEF(1,i), FISb(1,i)
         ENDDO
      ELSE
         WRITE (6,99035)
99035    FORMAT ('    Nucleus   ',6X,'Shell Corr.  Deform.')
         WRITE (6,99040)
99040    FORMAT ('              ',6X,'  (J=0)       (J=0)    ')
         WRITE (6,*)
         DO i = 1, NNUct
            IF (EMAx(i).NE.0.0D0) WRITE (6,99045) IFIX(SNGL(Z(i))),
     &          SYMb(i), IFIX(SNGL(A(i))), SHC(i), DEF(1,i)
         ENDDO
      ENDIF

      IF (KTRompcc.GT.0 .AND. DIRect.GT.0) THEN
        WRITE (6,*)
        WRITE (6,*) ' Inelastic o. m. parameters: RIPL catalog number ',
     &            KTRompcc
        WRITE (6,*) ' Neutron   o. m. parameters: RIPL catalog number ',
     &            KTRlom(1,1)
        WRITE (6,*) ' Proton    o. m. parameters: RIPL catalog number ',
     &            KTRlom(2,1)
        WRITE (6,*) ' Alpha     o. m. parameters: RIPL catalog number ',
     &            KTRlom(3,1)
        IF (NEMc.GT.0) WRITE (6,*)
     &            ' Cluster   o. m. parameters: RIPL catalog number ',
     &            KTRlom(NDEJC,1)
      ENDIF
      WRITE (6,*)

      WRITE (12,*) ' '
      WRITE (12,*) ' '
C-----WRITE heading on FILE12
      ia = INT(A(0))
      iae = INT(AEJc(0))
      IF(LEVtarg.GT.1) THEN
      WRITE (12,
     &'('' REACTION '',I3,''-'',A2,''-'',I3,'' + '',I3,''-'',  A2,''-'',
     &I3,''m INCIDENT ENERGY''                                ,F10.5,''
     &MeV'')') INT(ZEJc(0)), SYMbe(0), iae, INT(Z(0)), SYMb(0), ia, EINl
      ELSE
      WRITE (12,
     &'('' REACTION '',I3,''-'',A2,''-'',I3,'' + '',I3,''-'',  A2,''-'',
     &I3,'' INCIDENT ENERGY ''                                ,F10.5,''
     &MeV'')') INT(ZEJc(0)), SYMbe(0), iae, INT(Z(0)), SYMb(0), ia, EINl
      ENDIF
      WRITE (12,'('' COMPOUND NUCLEUS ENERGY'',F9.3,'' MeV'')') EXCn
      WRITE (12,*)

C-----printing to the LIST.OUT for ENDF file ****** DONE *****
99045 FORMAT (1X,I3,'-',A2,'-',I3,4X,10F12.3)
99050 FORMAT (10X,'B i n d i n g    e n e r g i e s')
      END
C
C
C
      SUBROUTINE PTLEVRE(Ia,Iz,Gspin,Gspar,E2p,E3m)
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
      DOUBLE PRECISION betatmp, elvr, etmp, jtmp, t12, xjlvr
      CHARACTER*1 dum
      CHARACTER*5 chelem
      CHARACTER*110 ch_iuf
      CHARACTER*3 ctmp3
      CHARACTER*9 finp
      INTEGER i, iar, ilv, ilvr, iptmp, itmp2, izr, lvpr, natmp,
     &        ndbrlin, ngamr, nlvr, nmax, nztmp
      CHARACTER*6 reftmp
      E2p = 0.D0
      E3m = 0.D0
C-----Avoiding searching of collective levels of the incident particle
      IF (Ia.EQ.AEJc(0)) GOTO 300
C-----First try to find 2+ and 3- states in the RIPL om-deformations file
      OPEN (47,FILE = '../RIPL-2/optical/om-data/om-deformations.dat',
     &      STATUS = 'old',ERR = 100)
      READ (47,'(///)')
                       ! Skipping first 4 title lines
      DO i = 1, 1700
         READ (47,'(2I4,4x,f10.6,1x,f4.1,i3,3x,f10.6,2x,a6)',END = 200,
     &         ERR = 200) nztmp, natmp, etmp, jtmp, iptmp, betatmp,
     &                    reftmp
         IF (nztmp.EQ.Iz .AND. natmp.EQ.Ia .AND. jtmp.EQ.2.D0 .AND.
     &       iptmp.EQ. + 1 .AND. reftmp.EQ.'Raman2') E2p = etmp
         IF (nztmp.EQ.Iz .AND. natmp.EQ.Ia .AND. jtmp.EQ.3.D0 .AND.
     &       iptmp.EQ. - 1 .AND. reftmp.EQ.'Kibedi') E3m = etmp
      ENDDO
      GOTO 200
  100 WRITE (6,*) ' WARNING: ',
     &   '../RIPL-2/optical/om-data/om-deformations.dat file not found '
      WRITE (6,*) ' WARNING: ',
     &'E(2+) and E(3-) will be selected from the available target level
     &scheme'
      GOTO 300
  200 CLOSE (47)
C
C-----If missing in the RIPL om-deformations file try discrete levels file
C-----constructing input and filenames
  300 IF (.NOT.FILevel) THEN
         WRITE (ctmp3,'(I3.3)') Iz
         finp = 'z'//ctmp3//'.dat'
         OPEN (13,FILE = '../RIPL-2/levels/'//finp,STATUS = 'OLD',
     &         ERR = 500)
      ELSE
         REWIND (13)
      ENDIF
  400 READ (13,'(A5,6I5,2f12.6)',END = 500) chelem, iar, izr, nlvr,
     &      ngamr, nmax, itmp2
      IF (Ia.NE.iar .OR. Iz.NE.izr) THEN
         DO ilv = 1, nlvr + ngamr
            READ (13,'(A1)',END = 500) dum
         ENDDO
         GOTO 400
      ELSE
C--------create file with levels xxx.lev
         IF (.NOT.FILevel) THEN
            BACKSPACE (13)
            READ (13,'(A110)') ch_iuf
C           RCN, 04/2005  duplicate levels found !!
C           WRITE (14,'(A110)') ch_iuf
            DO ilv = 1, nlvr + ngamr
               READ (13,'(A110)') ch_iuf
C              RCN, 04/2005  duplicate levels found !!
C              WRITE (14,'(A110)') ch_iuf
            ENDDO
            DO ilv = 1, nlvr + ngamr
               BACKSPACE (13)
            ENDDO
         ENDIF
C--------levels for nucleus NNUC copied to file xxx.lev
         DO ilv = 1, nlvr
            READ (13,'(I3,1X,F10.6,1X,F5.1,I3,1X,E10.2,I3)') ilvr, elvr,
     &            xjlvr, lvpr, t12, ndbrlin
            IF (ilv.EQ.1) THEN
               Gspin = xjlvr
               Gspar = lvpr
            ENDIF
C-----------skipping levels with unknown spin or parity
            IF (ilv.LE.nmax) THEN
               IF (E2p.EQ.0.0D0 .AND. xjlvr.EQ.2.D0 .AND. lvpr.EQ.1)
     &             E2p = elvr
               IF (E3m.EQ.0.0D0 .AND. xjlvr.EQ.3.D0 .AND. lvpr.EQ.( - 1)
     &             ) E3m = elvr
            ENDIF
         ENDDO
      ENDIF
      IF (.NOT.FILevel) CLOSE (13)
      RETURN
  500 Gspin = 0.
      IF (Ia.NE.2*(Ia/2)) Gspin = 0.5
      Gspar = 1
      WRITE (6,
     &'('' LEVELS FOR NUCLEUS A='',I3,'' Z='',I3,'' NOT FOUND IN THE FIL
     &E'')') Ia, Iz
      WRITE (6,
     & '('' JUST TO BE SURE I SET G.S. PARITY TO + AND SPIN TO:'',F5.1)'
     & ) Gspin
      IF (.NOT.FILevel) CLOSE (13)
      END
C
C
C
      SUBROUTINE PTLEVSET(Ar,Zr,Gspin,Gspar,E2p,E3m)
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
C-----define Gspin, Gspar, E2p, E3m for gamma/deuterium
      IF ( (ia.EQ.0 .and. iz.eq.0) .OR. (ia.EQ.2 .and. iz.eq.1)) THEN
         E2p = 0.D0
         E3m = 0.D0
         Gspin = 1
         Gspar = 1
         RETURN
      ENDIF
C-----define Gspin, Gspar, E2p, E3m for neutron/proton
      IF (ia.EQ.1 .or. ia.eq.3) THEN
         E2p = 0.D0
         E3m = 0.D0
         Gspin = 0.5
         Gspar = 1
         RETURN
      ENDIF
      IF (ia.EQ.4 .and. iz.eq.2) THEN
         E2p = 0.D0
         E3m = 0.D0
         Gspin = 0.
         Gspar = 1
         RETURN
      ENDIF

      IF (FLOAT(n/2).NE.FLOAT(n)/2.0) ncor = 1
      IF (FLOAT(iz/2).NE.FLOAT(iz)/2.0) izcor = 1
      IF (ia.LE.4) THEN
         ncor = 0
         izcor = 0
      ENDIF
      IF (ncor.EQ.1 .OR. izcor.EQ.1)
     &    CALL PTLEVRE(ia - ncor - izcor,iz - izcor,dum1,ium2,e2pd,e3md)
      CALL PTLEVRE(ia,iz,Gspin,Gspar,E2p,E3m)
      IF (ncor.EQ.1 .OR. izcor.EQ.1) THEN
         CALL PTLEVRE(ia + ncor + izcor,iz + izcor,dum1,ium2,e2pu,e3mu)
         E2p = (e2pd + e2pu)/2.0
         E3m = (e3md + e3mu)/2.0
         IF (e2pd.EQ.0D0) E2p = e2pu
         IF (e2pu.EQ.0D0) E2p = e2pd
         IF (e3md.EQ.0D0) E3m = e3mu
         IF (e3mu.EQ.0D0) E3m = e3md
      ENDIF
      END
C
C
      SUBROUTINE READIN(Irun)
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
Ccc   * input:Irun = 0 for normal call from INPUT                        *
Ccc   *              1 for call initiated from within incident energies  *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * output:none                                                      *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * calls:none                                                       *
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
      INTEGER*4 INDexf, INDexb, BUFfer(250)
      COMMON /R250COM/ INDexf,INDexb,BUFfer
      COMMON /TRINP / WIDexin, GAPin, HOMin, ALSin, EFItin, CNOrin
C
C Local variables
C
      REAL FLOAT
      DOUBLE PRECISION GRAND,DRAND
      CHARACTER*40 fstring
      INTEGER i, i1, i2, i3, i4, ieof, iloc, ipoten, izar, ki, nnuc,irun
      INTEGER IPArCOV
      INTEGER INT
      CHARACTER*6 name, namee
      LOGICAL fexist
      DOUBLE PRECISION val,vale,sigma
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
C
C      By default, no covariance calculation is done
C
      IPArCOV = 0
      OPEN(95,FILE='COVAR.DAT',ACCESS='APPEND',STATUS='UNKNOWN')
      INQUIRE (FILE = 'TARGET_COLL.DAT',EXIST = fexist)

      WRITE (6,*) '                        ____________________________'
      WRITE (6,*)
     &           '                       |                            |'
      WRITE (6,*)
     &           '                       |  E M P I R E  -  2.19.b27  |'
      WRITE (6,*)
     &           '                       |                            |'
      WRITE (6,*)
     &           '                       |  marching towards LODI ;-) |'
      WRITE (6,*)
     &           '                       |____________________________|'
      WRITE (6,*) ' '
      WRITE (6,*) ' '
      WRITE (6,*) 'Following options/parameters have been used'
      WRITE (6,*) '-------------------------------------------'
      WRITE (6,*) ' '
      WRITE (12,*) '***************************************************'
      WRITE (12,*) 'FAST ENERGY REGION'
      WRITE (12,*) 'Authors:'
      WRITE (12,*) ''
      WRITE (12,*) 'EVALUATION PROCEDURE                               '
      WRITE (12,*) ''
      WRITE (12,*) 'Adopted procedure is based on careful theoretical  '
      WRITE (12,*) 'analysis utilizing available experimental data and '
      WRITE (12,*) 'nuclear reaction model calculations.               '
      WRITE (12,*) '                                                   '
      WRITE (12,*) 'Available experimental data were interpreted  using'
      WRITE (12,*) 'nuclear reaction model code EMPIRE-2.19b27 by      '
      WRITE (12,*) 'M. Herman et al [He01, He02]. This code integrates '
      WRITE (12,*) 'into a single system a number of important modules '
      WRITE (12,*) 'and features:                                      '
      WRITE (12,*) '                                                   '
      WRITE (12,*) '- Spherical and deformed Optical Model including   '
      WRITE (12,*) '  coupled-channels (ECIS03 by J. Raynal)           '
      WRITE (12,*) '- Hauser-Feshbach statistical model including      '
      WRITE (12,*) '  HRTW width fluctuation correction                '
      WRITE (12,*) '- Quantum-mechanical MSD TUL model (codes ORION &  '
      WRITE (12,*) '  TRISTAN by H. Lenske), and MSC NVWY model        '
      WRITE (12,*) '- Exciton model with angular momentum coupling     '
      WRITE (12,*) '  (code DEGAS by E. Betak and P. Oblozinsky) that  '
      WRITE (12,*) '  represents a good approximation to the DSD model '
      WRITE (12,*) '- Exciton model with Iwamoto-Harada cluster        '
      WRITE (12,*) '  emission and Kalbach systematic angular distr.   '
      WRITE (12,*) '  (code PCROSS by R.Capote et al)                  '
      WRITE (12,*) '- Complete gamma-ray cascade after emission of     '
      WRITE (12,*) '  each particle, including realistic treatment of  '
      WRITE (12,*) '  discrete transitions                             '
      WRITE (12,*) '- Access to OM segment of the RIPL-2 library [Ri03]'
      WRITE (12,*) '- Built-in input parameter files, such as masses,  '
      WRITE (12,*) '  level density, discrete levels, fission barriers '
      WRITE (12,*) '  and gamma strength functions  based on the RIPL-2'
      WRITE (12,*) '  library [Ri03]                                   '
      WRITE (12,*) '- Automatic retrieval of experimental data from the'
      WRITE (12,*) '  EXFOR/CSISRS library                             '
      WRITE (12,*) '- ENDF-6 formatting (code EMPEND by A. Trkov)      '
      WRITE (12,*) '  coupled to graphical presentation capabilities   '
      WRITE (12,*) '  (code ZVView by V. Zerkin) through the chain of  '
      WRITE (12,*) '  PrePro codes by R. Cullen                        '
      WRITE (12,*) '- Checking codes (CHECKR, FIZCON, PSYCHE)          '
      WRITE (12,*) '- Support for NJOY                                 '
      WRITE (12,*) '                                                   '
      WRITE (12,*) 'PARAMETERIZATIONS                                  '
      WRITE (12,*) '                                                   '
      WRITE (12,*) 'Following models and parameters were used in the   '
      WRITE (12,*) 'current evaluation:                                '
      WRITE (12,*) '                                                   '
      WRITE (12,*) 'Discrete levels were taken from the RIPL-2 level   '
      WRITE (12,*) 'file, based on the 1998 version of ENSDF.          '
      irun = 0
  100 IF(irun.EQ.1) RETURN
      READ (5,'(A1)') name(1:1)
      IF (name(1:1).EQ.'*' .OR. name(1:1).EQ.'#' .OR. name(1:1)
     &    .EQ.'!') GOTO 100
         BACKSPACE (5)
         READ (5,'(A6,G10.5,4I5)',ERR = 200) name, val, i1, i2, i3, i4
         IF (name.EQ.'GO    ') THEN
            CLOSE(95)
C-----------Print some final input options
            IF (DIRect.EQ.0) THEN
               ECUtcoll = 0.
               JCUtcoll = 0
            ENDIF
            IF (KEY_shape.EQ.0) WRITE (6,
     &          '('' E1 strength function set to EGLO (EMPIRE-2.18)'')')
            IF (KEY_shape.EQ.1) WRITE (6,
     &                        '('' E1 strength function set to MLO1'')')
            IF (KEY_shape.EQ.2) WRITE (6,
     &                        '('' E1 strength function set to MLO2'')')
            IF (KEY_shape.EQ.3) WRITE (6,
     &                        '('' E1 strength function set to MLO3'')')
            IF (KEY_shape.EQ.4) WRITE (6,
     &                        '('' E1 strength function set to EGLO'')')
            IF (KEY_shape.EQ.5) WRITE (6,
     &                        '('' E1 strength function set to GFL'')')
            IF (KEY_shape.EQ.6) WRITE (6,
     &                   '('' E1 strength shape function set to SLO'')')
            IF(Key_gdrgfl.EQ.0.AND.Key_shape.EQ.0)WRITE(6,
     &         '('' GDR parameters from Messina systematics'')')
            IF(Key_gdrgfl.EQ.0.AND.Key_shape.NE.0)WRITE(6,
     &         '('' GDR parameters from Plujko systematics(RIPL-2)'')')
            IF(Key_gdrgfl.EQ.1)WRITE(6,
     &         '('' GDR parameters from RIPL-2/Exp.data+'',
     &           ''Plujko systematics'')')
            IF(Key_gdrgfl.EQ.2)WRITE(6,
     &          '('' GDR parameters from RIPL-2/Exp.data+'',
     &           ''Goriely calc.'')')
C-----   print  maximal gamma-ray multipolarity  'MAXmult'
            IF(MAXmult.GT.2)WRITE(6,
     &      '('' Gamma-transition multipolarity set to '',I4)')MAXmult

            WRITE (6,*) ' '
            IF (OMPar_riplf .OR. OMParfcc) THEN
               WRITE (6,*) 'Existing, case specific, o.m.p. files: '
               WRITE (6,*) '-------------------------------------'
            ENDIF
            IF (OMPar_riplf) WRITE (6,
     &'('' Input file OMPAR.RIPL with RIPL optical model'',
     &'' parameters '')')
            IF (OMParfcc .AND. (DIRect.EQ.1 .OR. DIRect.EQ.3)) WRITE (6,
     &'('' Input file OMPAR.DIR with optical model'',
     &'' parameters to be used in inelastic scattering'')')
            IF (KEY_shape.EQ.0) WRITE (12,
     &          '('' E1 strength function set to EGLO (EMPIRE-2.18)'')')
            IF (KEY_shape.EQ.1) WRITE (12,
     &                         '('' E1 strength function set to MLO1'')'
     &                         )
            IF (KEY_shape.EQ.2) WRITE (12,
     &                         '('' E1 strength function set to MLO2'')'
     &                         )
            IF (KEY_shape.EQ.3) WRITE (12,
     &                         '('' E1 strength function set to MLO3'')'
     &                         )
            IF (KEY_shape.EQ.4) WRITE (12,
     &                         '('' E1 strength function set to EGLO'')'
     &                         )
            IF (KEY_shape.EQ.5) WRITE (12,
     &                          '('' E1 strength function set to GFL'')'
     &                          )
            IF (KEY_shape.EQ.6) WRITE (12,
     &                    '('' E1 strength shape function set to SLO'')'
     &                    )
            IF(Key_gdrgfl.EQ.0.AND.Key_shape.EQ.0)WRITE(12,
     &         '('' GDR parameters from Messina systematics'')')
            IF(Key_gdrgfl.EQ.0.AND.Key_shape.NE.0)WRITE(12,
     &         '('' GDR parameters from RIPL-2/Plujko systematics'')')
            IF(Key_gdrgfl.EQ.1)WRITE(12,
     &         '('' GDR parameters from RIPL-2/Exp.data+'',
     &           ''Plujko systematics'')')
            IF(Key_gdrgfl.EQ.2)WRITE(12,
     &          '('' GDR parameters from RIPL-2/Exp.data+'',
     &           ''Goriely calc.'')')
C-----      print  maximal gamma-ray multipolarity  'MAXmult'
            IF(MAXmult.GT.2)WRITE(12,
     &      '('' Gamma-transition multipolarity set to '',I4)')MAXmult

            IF (DIRect.EQ.0 .AND. KTRompcc.NE.0) THEN
               WRITE (6,
     &'(1X,/,         '' WARNING: No direct calculations have been selec
     &ted'',/,        '' WARNING: but DIRPOT keyword is specified.'',/,
     &                '' WARNING: Set direct keyword in the input file t
     &o a nonzero'',/,'' WARNING: value if you want to include direct co
     &ntribution.'')')
               KTRompcc = 0
            ENDIF
            WRITE (6,*) ' '
C-----------Printout of some final input options   *** done ***
            RETURN
         ENDIF
         ENTRY OPTIONS(namee, vale, i1e, i2e, i3e, i4e, irun)
         IF(irun.EQ.1) THEN
         name = namee
         val = vale
         i1 = i1e
         i2 = i2e
         i3 = i3e
         i4 = i4e
         ENDIF

C--------DEGAS input
         IF (name.EQ.'DEGAS ') THEN
            DEGa = val
            IF (val.GT.0) WRITE(6,
     &              '('' Exciton model calculations with code DEGAS '')'
     &              )
            IF (val.GT.0) WRITE(12,
     &              '('' Exciton model calculations with code DEGAS '')'
     &              )
            GOTO 100
         ENDIF
         IF (name.EQ.'GDIVP ') THEN
            if(i1.ne.0) then
                WRITE (6,
     &          '('' DEGAS proton s.p.l. density uncertainty '',
     &          '' is equal to '',i2,''%'')') i1
                 sigma = val*i1*0.01
C                GDIvp = val + grand()*sigma
                 GDIvp = val + (2*drand()-1.)*sigma
                WRITE (6,
     &       '('' DEGAS proton s.p.l. density sampled value is A/: ''
     &          ,f5.2)') GDIvp
                 IPArCOV = IPArCOV +1
                 write(95,'(1x,i5,1x,d12.6,1x,2i13)')
     &              IPArCOV, MFPp, INDexf,INDexb
            else
              GDIvp = val
              WRITE(6,
     &'('' DEGAS proton s.p.l. density set to A/'',f5.2,'' in'',
     &'' code DEGAS '')') GDIvp
              WRITE(12,
     &'('' DEGAS proton s.p.l. density set to A/'',f5.2,'' in'',
     &'' code DEGAS '')') GDIvp
            endif
            GOTO 100
         ENDIF
C--------PCROSS input
         IF (name.EQ.'PCROSS') THEN
            PEQc = 0.
            IF (val.GE.0.8 .AND. val.LE.3.D0) THEN
              PEQc = 1.
              MFPp = val
              WRITE (6,
     &'('' Exciton model calculations with code PCROSS'',/,
     &  '' Cluster emission in terms of the Iwamoto-Harada model'',/
     &  '' Kalbach systematics angular distributions (see RIPL-1)'')')
              WRITE (12,
     &'('' Exciton model calculations with code PCROSS'',/,
     &  '' Cluster emission in terms of the Iwamoto-Harada model'',/
     &  '' Kalbach systematics angular distributions (see RIPL-1)'')')
              if(i1.ne.0) then
                WRITE (6,
     &          '('' Mean free path parameter uncertainty '',
     &          '' is equal to '',i2,'' %'')') i1
                 sigma = val*i1*0.01
C                MFPp = val + grand()*sigma
                 MFPp = val + (2*drand()-1.)*sigma
                WRITE (6,
     &          '('' Mean free path parameter sampled value : '',f5.2)')
     &          MFPp
                 IPArCOV = IPArCOV +1
                 write(95,'(1x,i5,1x,d12.6,1x,2i13)')
     &              IPArCOV, MFPp, INDexf,INDexb
              else
                WRITE (6,
     &'('' Mean free path parameter in PCROSS set to '',F4.1,
     &  '' (Default: 1.3)'')') MFPp
                WRITE (12,
     &'('' Mean free path parameter in PCROSS set to '',F4.1,
     &  '' (Default: 1.3)'')') MFPp
               endif
             ENDIF
            GOTO 100
         ENDIF
         IF (name.EQ.'MAXHOL') THEN
            CHMax = 0.2
            IF (val.GE.0.1 .AND. val.LE.1.5D0) THEN
              CHMax = val
              WRITE (6,
     &'('' Max hole number in PCROSS set to'',F4.2,
     &        ''*sqrt(g*U)'')') CHMax
              WRITE (6,
     &'(''    being U the CN excitation energy '')')
              WRITE (12,
     &'('' Max hole number in PCROSS set to'',F4.2,
     &        ''*sqrt(g*U)'')') CHMax
              WRITE (12,
     &'(''    being U the CN excitation energy '')')
             ENDIF
            GOTO 100
         ENDIF
C
C--------ECIS input
C
C--------In the following block one parameter -KTRompCC- is defined
C--------DIRECT is set to 1 if equal zero to allow for ECIS calc.
C
         IF (name.EQ.'DIRPOT') THEN
            IF (val.LT.0) THEN
               ki = 26
               ipoten = -INT(val)
C--------------searching in the RIPL database
               CALL FINDPOT(ki,ieof,ipoten)
               IF (ieof.EQ.0) THEN
                  val = -val
               ELSE
                  WRITE (6,*) 'WARNING: Requested RIPL entry ', ipoten,
     &                        ' for inelastic scattering not found'
                  GOTO 100
               ENDIF
            ELSE
               WRITE (6,
     &    '('' Only RIPL OMP parameters are supported in EMPIRE 2.19'')'
     &    )
               STOP
            ENDIF
            WRITE (6,
     &'('' Optical model parameters for direct inelastic scattering set
     & to RIPL #'',I4)') INT(val)
            KTRompcc = INT(val)
            GOTO 100
         ENDIF
         IF (name.EQ.'DIRECT') THEN
            DIRect = val
            IF (DIRect.EQ.3) WRITE (6,
     &         '('' DWBA (ECIS) used for direct inelastic scattering'')'
     &         )
            IF (DIRect.EQ.1 .OR. DIRect.EQ.2) WRITE (6,
     &'('' Coupled Channels (ECIS) used for direct inelastic scattering'
     &')')
            IF (DIRect.EQ.2) WRITE (6,
     &'('' Coupled Channels (ECIS) used for Tl calculations in inelastic
     & channels'')')
            IF (DIRect.EQ.3) WRITE (12,
     &         '('' DWBA (ECIS) used for direct inelastic scattering'')'
     &         )
            IF (DIRect.EQ.1 .OR. DIRect.EQ.2) WRITE (12,
     &'('' Coupled Channels (ECIS) used for direct inelastic scattering'
     &')')
            IF (DIRect.EQ.2) WRITE (12,
     &'('' Coupled Channels (ECIS) used for Tl calculations in inelastic
     & channels'')')
            GOTO 100
         ENDIF
         IF (name.EQ.'EcDWBA') THEN
C           EcDWBA meaningless if Collective level file exists
            IF(fexist) goto 100
            ECUtcoll = val
            JCUtcoll = i1
            IF (JCUtcoll.EQ.0) JCUtcoll = 2
            WRITE (6,
     &     '('' Collective levels up to '',F5.1,'' MeV used in DWBA'' )'
     &     ) ECUtcoll
            WRITE (6,
     &'('' All levels with spin less or equal to '',I1,           '' con
     &sidered in DWBA'')') JCUtcoll
            GOTO 100
         ENDIF
C
         IF (name.EQ.'RESOLF') THEN
            IF(val.gt.0.) THEN
              WIDcoll = val
              WRITE (6,
     &     '('' Collective levels in continuum will be spread using'')')
              WRITE (6,
     &     '('' gaussian function. Gaussian sigma = 0.02+R*sqrt(E); ''
     &       ''R = '',F6.3, '' keV'' )') WIDcoll*1000
             ENDIF
            GOTO 100
         ENDIF
C
C--------ECIS input  *** done ***
C
         IF (name.EQ.'RELKIN') THEN
            IF (val.NE.0.) THEN
               RELkin = .TRUE.
               WRITE (6,'(1x,A)') 'Relativistic kinematics used'
               WRITE (12,'(1x,A)') 'Relativistic kinematics used'
            ELSE
               WRITE (6,'(1x,A)') 'Non-relativistic kinematics used'
               WRITE (12,'(1x,A)') 'Non-relativistic kinematics used'
            ENDIF
            GOTO 100
         ENDIF
         IF (name.EQ.'BFUS  ') THEN
            BFUs = val
            WRITE (6,'('' Fusion barrier set to '',F7.2,'' MeV'')') BFUs
            GOTO 100
         ENDIF
C--------CCFUS input
         IF (name.EQ.'FCD   ') THEN
            FCD(i1) = val
            WRITE (6,
     &'('' FCD parameter for  n='',I2,'' collective level set to'',F6.3)
     &') i1, FCD(i1)
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'DV    ') THEN
            DV = val
            WRITE (6,'('' DV barrier parameter in CCFUS set to '',F6.3)'
     &             ) DV
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'FCC   ') THEN
            FCC = val
            WRITE (6,'('' FCC parameter in CCFUS set to '',F6.3)') FCC
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'NSCC  ') THEN
            NSCc = val
            WRITE (6,
     &            '('' Number of coupled channels in CCFUS set to'',I3)'
     &            ) NSCc
            GOTO 100
         ENDIF

         IF (name.EQ.'NACC  ') THEN
            NACc = val
            WRITE (6,
     &          '('' Number of additional coupled channels set to'',I3)'
     &          ) NACc
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'BETCC ') THEN
            BETcc(i1) = val
            WRITE (6,
     &'('' Deformation of the n='',I2,'' collective level set to'',F6.3)
     &') i1, BETcc(i1)
            GOTO 100
         ENDIF

         IF (name.EQ.'FLAM  ') THEN
            FLAm(i1) = val
            IF(val.gt.0)
     &      WRITE (6,*)'TARGET COLLECTIVE CHANNEL FOR CCFUS DEFINED'
            IF(i2.lt.0)
     &      WRITE (6,*)'PROJECTILE COLLECTIVE CHANNEL FOR CCFUS DEFINED'
            WRITE (6,
     &'('' Multipolar. of the n='',I2,'' collective level set to'',F6.3)
     &') i1, FLAm(i1)
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'QCC   ') THEN
            QCC(i1) = val
            IF (QCC(i1).GT.0.0D0) QCC(i1) = -QCC(i1)
            WRITE (6,
     &'('' Q-value     of the n='',I2,'' collective level set to'',F6.3)
     &') i1, QCC(i1)
            GOTO 100
         ENDIF
C--------CCFUS input  ** done ***

C-----
         IF (name.EQ.'GDRGFL') THEN
            Key_GDRGFL = val + 0.001
            GOTO 100
         ENDIF
C        Key_GDRGFL = 0 and Key_shape =0 -  GDR parameters from Messina systematics
C        Key_GDRGFL > = 0 and and Key_shape > 0 -  GDR parameters of RIPL-2 introduced;
C        Key_GDRGFL = 1 -  GDR parameters and other data determined by gdrgfldata.f;
C                          experimental values or systematics of GDR parameters are set.
C        Key_GDRGFL = 2 -  GDR parameters and other data determined by gdrgfldata.f;
C                          if experimantal values of GDR parameters not found and Key_GDRGFL=2
C                          they are going to be retrieved from the RIPL-2 Goriely theoretical
C                          values and then from systematics.
C
C--------input MAXmult - maximal value (=< 10) of gamma-ray multipolarity for GSA
         IF(name.EQ.'GRMULT')THEN
            MAXmult = val + 0.001
            IF(MAXmult.GT.10) MAXmult = 10
            IF(MAXmult.LT.2) MAXmult = 2
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'QFIS  ') THEN
            QFIs = val
            WRITE (6,
     &          '('' Liquid drop fission barriers multiplied by'',F6.3)'
     &          ) QFIs
            WRITE (12,
     &          '('' Liquid drop fission barriers multiplied by'',F6.3)'
     &          ) QFIs
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'LEVDEN') THEN
            ADIv = val
            IF (ADIv.EQ.0.0D0) WRITE (6,
     &           '('' EMPIRE-specific level densities were selected '')'
     &           )
            IF (ADIv.EQ.1.0D0) WRITE (6,
     &'('' EMPIRE-specific level densities with fitted parameters were s
     &elected'')')
            IF (ADIv.GT.3.0D0) WRITE (6,
     &     '('' ROCOL level densities with a=A/''  ,F5.2,'' selected'')'
     &     ) ADIv
            IF (ADIv.EQ.2.0D0) WRITE (6,
     &           '('' Gilbert-Cameron level densities were selected '')'
     &           )
            IF (ADIv.EQ.3.0D0) WRITE (6,
     &          '('' Microscopic HFBCS level densities were selected'')'
     &          )
            IF (ADIv.EQ.0.0D0) WRITE (12,
     &           '('' EMPIRE-specific level densities '')'
     &           )
            IF (ADIv.EQ.1.0D0) WRITE (12,
     &'('' EMPIRE-specific level densities with fitted parameters'')')
            IF (ADIv.GT.3.0D0) WRITE (12,
     &     '('' ROCOL level densities with a=A/''  ,F5.2)') ADIv
            IF (ADIv.EQ.2.0D0) WRITE (12,
     &           '('' Gilbert-Cameron level densities '')'
     &           )
            IF (ADIv.EQ.3.0D0) WRITE (12,
     &          '('' Microscopic HFBCS level densities '')'
     &          )
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'BETAV ') THEN
            BETav = val
            WRITE (6,
     &          '('' Viscosity parameter set to'',F6.3,'' 10**21 1/s'')'
     &          ) BETav
            WRITE (12,
     &          '('' Viscosity parameter set to'',F6.3,'' 10**21 1/s'')'
     &          ) BETav
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'SHRJ  ') THEN
            SHRj = val
            WRITE (6,
     &'('' Shell correction to fission barrier brougth to 1/2 at spin ''
     &,F5.1)') SHRj
            WRITE (12,
     &'('' Shell correction to fission barrier brougth to 1/2 at spin ''
     &,F5.1)') SHRj
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'SHRD  ') THEN
            SHRd = val
            WRITE (6,
     &          '('' Diffusness of the shell correction damping'',F6.3)'
     &          ) SHRd
            WRITE (12,
     &          '('' Diffusness of the shell correction damping'',F6.3)'
     &          ) SHRd
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'FUSRED') THEN
            FUSred = val
            WRITE (6,'('' Fusion cross section was scaled by factor'',
     &             F6.3)') FUSred
            WRITE (12,'('' Fusion cross section was scaled by factor '',
     &             F6.3)') FUSred
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'TOTRED') THEN
            TOTred = val
            WRITE (6,'('' Total cross section was scaled by factor '',
     &             F6.3)') TOTred
            WRITE (12,'('' Total cross section was scaled by factor '',
     &             F6.3)') TOTred
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'CSREAD') THEN
            CSRead = val
            IF (CSRead.GT.0.0D0) WRITE (6,
     &       '('' Fusion cross section '',F8.3,'' mb read from input'')'
     &       ) CSRead
            IF (CSRead.EQ.0.0D0) THEN
               WRITE (6,
     &'('' Bass option disabled CCFUS will be used instead          '')'
     &)
               CSRead = -2.0D0
            ENDIF
            IF (CSRead.EQ.( - 1.0D0)) WRITE (6,
     &'('' Fusion cross section will be calculated according to distribu
     &ted barrier model'')')
            IF (CSRead.EQ.( - 2.0D0)) WRITE (6,
     &'('' Fusion cross section will be calculated using CCFUS simplifie
     &d coupled channel approach'')')
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'SIG   ') THEN
            SIG = val
            IF (CSRead.EQ.( - 1.0D0)) WRITE (6,
     &      '('' SIGMA in the distributed barrier model set to '',F6.3)'
     &      ) SIG
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'TRUNC ') THEN
            TRUnc = val
            IF (CSRead.EQ.( - 1.0D0)) WRITE (6,
     & '('' Truncation in the distributed barrier model set to '',F6.3)'
     & ) TRUnc
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'EXPUSH') THEN
            EXPush = val
            IF (CSRead.EQ.( - 1.0D0))
     &           WRITE (6,'('' Extrapush set to '',F6.3)') EXPush
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'CRL   ') THEN
            CRL = val
            WRITE (6,'('' Critical l-value for fusion set to '',F6.2)')
     &             CRL
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'TRGLEV') THEN
            LEVtarg = val
            WRITE (6,'('' Target excited to the level #'',I2)') LEVtarg
            WRITE (12,'('' Target excited to the level #'',I2)') LEVtarg
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'DFUS  ') THEN
            DFUs = val
            WRITE (6,
     &'('' Difusness in the transmission coefficients for fusion set to
     &'',F5.2)') DFUs
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'TEMP0 ') THEN
            TEMp0 = val
            WRITE (6,
     &'('' Temperature at which shell correction fade-out starts set to
     &'',F6.3)') TEMp0
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'SHRT  ') THEN
            SHRt = val
            WRITE (6,
     &'('' Parameter in the teperature shell correction fade-out set to
     &'',F6.3)') SHRt
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'IOUT  ') THEN
            IOUt = val
            WRITE (6,
     &             '('' Main calculations output control set to '',I2)')
     &             IOUt
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'NOUT  ') THEN
            NOUt = val
            WRITE (6,'('' MSC calculation output control set to '',I2)')
     &             NOUt
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'XNI   ') THEN
            XNI = val
            WRITE (6,'('' Initial exciton number set to '',F4.1)') XNI
            WRITE (12,'('' Initial exciton number '',F4.1)') XNI
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'TORY  ') THEN
            TORy = val
            WRITE (6,
     &       '(''(n-p)/(n-n) interaction strength ratio set to '',F5.2)'
     &       ) TORy
            WRITE (12,
     &       '(''(n-p)/(n-n) interaction strength ratio '',F5.2)'
     &       ) TORy
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'EX1   ') THEN
            EX1 = val
            WRITE (6,
     &   '('' Initial number of excitons being neutrons set to '',F6.3)'
     &    ) EX1
            WRITE (12,
     &    '('' Initial number of excitons being neutrons '',F6.3)') EX1
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'EX2   ') THEN
            EX2 = val
            WRITE (6,
     &   '('' Initial number of excitons being protons set to  '',F6.3)'
     &    ) EX2
            WRITE (12,
     &    '('' Initial number of excitons being protons '',F6.3)') EX2
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'GST   ') THEN
            GST = val
            IF (GST.EQ.1.0D0) WRITE (6,
     &                            '('' Gamma emission in MSC allowed'')'
     &                            )
            IF (GST.EQ.1.0D0) WRITE (12,
     &                       '('' Gamma emission in MSC considered'')')
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'STMRO ') THEN
            STMro = val
            IF (STMro.EQ.1.0D0) WRITE (6,
     &                 '('' Microscopic p-h state densities selected'')'
     &                 )
            IF (STMro.EQ.0.0D0) WRITE (6,
     &                 '('' Closed form p-h state densities selected'')'
     &                 )
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'GDIV  ') THEN
            GDIv = val
            WRITE (6,
     &      '('' Single particle level density in MSC set to A/'',F5.2)'
     &      ) GDIv
            WRITE (12,
     &      '('' Single particle level density in MSC set to A/'',F5.2)'
     &      ) GDIv
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'D1FRA ') THEN
            D1Fra = val
            WRITE (6,'('' Spreading to total GDR width set to '',F5.3)')
     &             D1Fra
            WRITE (12,'('' Spreading to total GDR width set to '',F5.3)'
     &             ) D1Fra
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'LTURBO') THEN
            LTUrbo = val
            TURbo = FLOAT(LTUrbo)
            WRITE (6,'('' Step in the angular momentum set to '',I2)')
     &             LTUrbo
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'NEX   ') THEN
            NEXreq = val
            WRITE (6,
     &'('' Number of energy steps in the integration set to '',
     &I3)') NEXreq
            GOTO 100
         ENDIF
C-----
C------- Insert Key for module "gamma-strength-analytic.f",
C        Key_shape - key to specify the E1 strength shape    .
C
         IF (name.EQ.'GSTRFN') THEN
C           Key_shape = 0 --> ver.2.18 variant of EGLO strength-function
C           Key_shape =1 --> fE1=MLO1
C           Key_shape =2 --> fE1=MLO2
C           Key_shape =3 --> fE1=MLO3
C           Key_shape =4 --> fE1=EGLO
C           Key_shape =5 --> fE1=GFL
C           Key_shape =6 --> fE1=SLO
            KEY_shape = val
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'GDRDYN') THEN
            GDRdyn = val
            IF (GDRdyn.NE.0.D0) WRITE (6,
     &                       '('' Deformation dependent GDR selected'')'
     &                       )
            GOTO 100
         ENDIF
C-----
C        SAMPLING OF OMP PARAMETERS FOR COVARIANCE CALCULATION
C
C        VOM(Nejc,Nnuc) = vlib(1)*FNvvomp(Nejc,Nnuc)
C
         IF (name.EQ.'UOMPVV') THEN
            izar = i1*1000 + i2
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (6,'('' NUCLEUS '',I3,A2,'' NOT NEEDED'')') i2,
     &                SYMb(nnuc)
               WRITE (6,
     &'('' Real volume potential depth uncertainty ignored'')')
               GOTO 100
            ENDIF
            IF (i3.GT.NDEJC) THEN
               WRITE (6,'('' UNKNOWN EJECTILE in UOMPVV '',I2)') i3
               WRITE (6,
     &'('' Real volume potential depth uncertainty ignored'')')
               GOTO 100
            ENDIF
            if(val.gt.0.) then
              WRITE (6,
     &        '('' Real volume potential depth uncertainty in '',I3,A2,
     &        '' is equal to '',f5.2,'' %'')') i2, SYMb(nnuc), val
                 sigma = val*0.01
C                FNvvomp(i3,nnuc) = 1. + grand()*sigma
                 FNvvomp(i3,nnuc) = 1. + (2*drand()-1.)*sigma
              WRITE (6,
     &        '('' Real volume potential depth sampled norm.factor : '',
     &        f5.2)') FNvvomp(i3,nnuc)
                 IPArCOV = IPArCOV +1
                 write(95,'(1x,i5,1x,d12.6,1x,2i13)')
     &              IPArCOV, FNvvomp(i3,nnuc), INDexf,INDexb
            endif
            GOTO 100
         ENDIF
C----
C        AVOm(Nejc,Nnuc) = alib(1)*FNavomp(Nejc,Nnuc)
C        AWOm(Nejc,Nnuc) = alib(3)*FNavomp(Nejc,Nnuc)
C
         IF (name.EQ.'UOMPAV') THEN
            izar = i1*1000 + i2
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (6,'('' NUCLEUS '',I3,A2,'' NOT NEEDED'')') i2,
     &                SYMb(nnuc)
               WRITE (6,
     &        '('' Volume potential diffuseness uncertainty ignored'')')
               GOTO 100
            ENDIF
            IF (i3.GT.NDEJC) THEN
               WRITE (6,'('' UNKNOWN EJECTILE in UOMPAV '',I2)') i3
               WRITE (6,
     &        '('' Volume potential diffuseness uncertainty ignored'')')
               GOTO 100
            ENDIF
            if(val.gt.0.) then
              WRITE (6,
     &        '('' Volume potential diffuseness uncertainty in '',I3,
     &        A2,'' is equal to '',f5.2,'' %'')') i2, SYMb(nnuc), val
                 sigma = val*0.01
C                FNavomp(i3,nnuc) = 1. + grand()*sigma
                 FNavomp(i3,nnuc) = 1. + (2*drand()-1.)*sigma
              WRITE (6,
     &        '('' Volume potential diffuseness sampled norm.factor : ''
     &        ,f5.2)') FNavomp(i3,nnuc)
                 IPArCOV = IPArCOV +1
                 write(95,'(1x,i5,1x,d12.6,1x,2i13)')
     &              IPArCOV, FNavomp(i3,nnuc), INDexf,INDexb
            endif
            GOTO 100
         ENDIF

C--------Volume imaginary potential
C        WOMv(Nejc,Nnuc) = vlib(2)*FNwvomp(Nejc,Nnuc)
         IF (name.EQ.'UOMPWV') THEN
            izar = i1*1000 + i2
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (6,'('' NUCLEUS '',I3,A2,'' NOT NEEDED'')') i2,
     &                SYMb(nnuc)
               WRITE (6,
     &        '('' Imag. volume potential depth uncertainty ignored'')')
               GOTO 100
            ENDIF
            IF (i3.GT.NDEJC) THEN
               WRITE (6,'('' UNKNOWN EJECTILE in UOMPWV '',I2)') i3
               WRITE (6,
     &        '('' Imag. volume potential depth uncertainty ignored'')')
               GOTO 100
            ENDIF
            if(val.gt.0.) then
              WRITE (6,
     &        '('' Imag. volume potential depth uncertainty in '',I3,A2,
     &        '' is equal to '',f5.2,'' %'')') i2, SYMb(nnuc), val
                 sigma = val*0.01
C                FNwvomp(i3,nnuc) = 1. + grand()*sigma
                 FNwvomp(i3,nnuc) = 1. + (2*drand()-1.)*sigma
              WRITE (6,
     &        '('' Imag. volume potential depth sampled norm.factor : ''
     &        ,f5.2)') FNwvomp(i3,nnuc)
                 IPArCOV = IPArCOV +1
                 write(95,'(1x,i5,1x,d12.6,1x,2i13)')
     &              IPArCOV, FNwvomp(i3,nnuc), INDexf,INDexb
            endif
            if(val.lt.0.) then
              FNwvomp(i3,nnuc) = abs(val)
              WRITE (6,
     &        '('' Imag. volume potential depth scaling factor : ''
     &        ,f5.2)') FNwvomp(i3,nnuc)
              WRITE (12,
     &        '('' Imag. volume potential depth scaling factor : ''
     &        ,f5.2)') FNwvomp(i3,nnuc)
            endif
            GOTO 100
         ENDIF

C--------Surface imaginary potential:
C        WOMs(Nejc,Nnuc) = vlib(4)*FNwsomp(Nejc,Nnuc)
         IF (name.EQ.'UOMPWS') THEN
            izar = i1*1000 + i2
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (6,'('' NUCLEUS '',I3,A2,'' NOT NEEDED'')') i2,
     &                SYMb(nnuc)
               WRITE (6,
     &      '('' Imag. surface potential depth uncertainty ignored'')')
               GOTO 100
            ENDIF
            IF (i3.GT.NDEJC) THEN
               WRITE (6,'('' UNKNOWN EJECTILE in UOMPWS '',I2)') i3
               WRITE (6,
     &      '('' Imag. surface potential depth uncertainty ignored'')')
               GOTO 100
            ENDIF
            if(val.gt.0.) then
              WRITE (6,
     &      '('' Imag. surface potential depth uncertainty in '',I3,A2,
     &        '' is equal to '',f5.2,'' %'')') i2, SYMb(nnuc), val
                 sigma = val*0.01
C                FNwsomp(i3,nnuc) = 1. + grand()*sigma
                 FNwsomp(i3,nnuc) = 1. + (2*drand()-1.)*sigma
              WRITE (6,
     &      '('' Imag. surface potential depth sampled norm.factor : '',
     &        f5.2)') FNwsomp(i3,nnuc)
                 IPArCOV = IPArCOV +1
                 write(95,'(1x,i5,1x,d12.6,1x,2i13)')
     &              IPArCOV, FNwsomp(i3,nnuc), INDexf,INDexb
            endif
            if(val.lt.0.) then
              FNwsomp(i3,nnuc) = abs(val)
              WRITE (6,
     &        '('' Imag. surface potential depth scaling factor : ''
     &        ,f5.2)') FNwsomp(i3,nnuc)
              WRITE (12,
     &        '('' Imag. surface potential depth scaling factor : ''
     &        ,f5.2)') FNwsomp(i3,nnuc)
            endif
            GOTO 100
         ENDIF
C----
C
C        FNasomp(Nejc,Nnuc) = 1.0
C
         IF (name.EQ.'UOMPAS') THEN
            izar = i1*1000 + i2
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (6,'('' NUCLEUS '',I3,A2,'' NOT NEEDED'')') i2,
     &                SYMb(nnuc)
               WRITE (6,
     & '('' Surface potential diffuseness uncertainty ignored'')')
               GOTO 100
            ENDIF
            IF (i3.GT.NDEJC) THEN
               WRITE (6,'('' UNKNOWN EJECTILE in UOMPAS '',I2)') i3
               WRITE (6,
     & '('' Surface potential diffuseness uncertainty ignored'')')
               GOTO 100
            ENDIF
            if(val.gt.0.) then
              WRITE (6,
     &        '('' Surface potential diffuseness uncertainty in '',I3,
     &        A2,'' is equal to '',f5.2,'' %'')') i2, SYMb(nnuc), val
                 sigma = val*0.01
C                FNasomp(i3,nnuc) = 1. + grand()*sigma
                 FNasomp(i3,nnuc) = 1. + (2*drand()-1.)*sigma
              WRITE (6,
     &        '('' Surface potential diffuseness sampled norm.factor :''
     &        ,f5.2)') FNasomp(i3,nnuc)
                 IPArCOV = IPArCOV +1
                 write(95,'(1x,i5,1x,d12.6,1x,2i13)')
     &              IPArCOV, FNasomp(i3,nnuc), INDexf,INDexb
            endif
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'EGDR1 ') THEN
            izar = i1*1000 + i2
            IF (izar.EQ.0) THEN
              DO i = 1, NDNUC
                GDRpar(1,i) = val
              ENDDO
              WRITE (6,
     &        '('' GDR first hump energy in all nuclei set to '',F5.2)')
     &        val
              WRITE (12,
     &        '('' GDR first hump energy in all nuclei set to '',F5.2)')
     &        val
              GOTO 100
            ENDIF
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (6,'('' NUCLEUS '',I3,A2,'' NOT NEEDED'')') i2,
     &                SYMb(nnuc)
               WRITE (6,
     &          '('' NORMALIZATION OF GDR first hump energy IGNORED'')')
               GOTO 100
            ENDIF
            if(i3.ne.0) then
              WRITE (6,
     &        '('' GDR first hump energy uncertainty in '',I3,A2,
     &        '' is equal to '',i2,''%'')') i2, SYMb(nnuc), i3
               sigma = val*i3*0.01
               GDRpar(1,nnuc) = val + grand()*sigma
              WRITE (6,
     &        '('' GDR first hump energy sampled value : '',f5.2)')
     &        GDRpar(1,nnuc)
               IPArCOV = IPArCOV +1
               write(95,'(1x,i5,1x,d12.6,1x,2i13)')
     &              IPArCOV, GDRpar(1,nnuc), INDexf,INDexb
            else
              GDRpar(1,nnuc) = val
              WRITE (6,
     &      '('' GDR first hump energy in '',I3,A2,'' set to '',F5.2)'
     &        ) i2, SYMb(nnuc), val
              WRITE (12,
     &      '('' GDR first hump energy in '',I3,A2,'' set to '',F5.2)'
     &        ) i2, SYMb(nnuc), val
             endif
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'GGDR1 ') THEN
            izar = i1*1000 + i2
            IF (izar.EQ.0) THEN
              DO i = 1, NDNUC
                GDRpar(2,i) = val
              ENDDO
              WRITE (6,
     &        '('' GDR first hump width in all nuclei set to '',F5.2)')
     &        val
              WRITE (12,
     &        '('' GDR first hump width in all nuclei set to '',F5.2)')
     &        val
              GOTO 100
            ENDIF
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (6,'('' NUCLEUS '',I3,A2,'' NOT NEEDED'')') i2,
     &                SYMb(nnuc)
               WRITE (6,
     &          '('' NORMALIZATION OF GDR first hump width IGNORED'')')
               GOTO 100
            ENDIF
            if(i3.ne.0) then
              WRITE (6,
     &        '('' GDR first hump width uncertainty in '',I3,A2,
     &        '' is equal to '',i2,''%'')') i2, SYMb(nnuc), i3
               sigma = val*i3*0.01
               GDRpar(2,nnuc) = val + grand()*sigma
              WRITE (6,
     &        '('' GDR first hump width sampled value : '',f5.2)')
     &        GDRpar(2,nnuc)
               IPArCOV = IPArCOV +1
               write(95,'(1x,i5,1x,d12.6,1x,2i13)')
     &              IPArCOV, GDRpar(2,nnuc), INDexf,INDexb
            else
              GDRpar(2,nnuc) = val
              WRITE (6,
     &      '('' GDR first hump width in '',I3,A2,'' set to '',F5.2)'
     &        ) i2, SYMb(nnuc), val
              WRITE (12,
     &      '('' GDR first hump width in '',I3,A2,'' set to '',F5.2)'
     &        ) i2, SYMb(nnuc), val
             endif
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'CSGDR1') THEN
            izar = i1*1000 + i2
            IF (izar.EQ.0) THEN
              DO i = 1, NDNUC
                GDRpar(3,i) = val
              ENDDO
              WRITE (6,
     &        '('' GDR first hump cross section in all nuclei set to ''
     &        ,F5.2)') val
              WRITE (12,
     &        '('' GDR first hump cross section in all nuclei set to ''
     &        ,F5.2)') val
              GOTO 100
            ENDIF
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (6,'('' NUCLEUS '',I3,A2,'' NOT NEEDED'')') i2,
     &                SYMb(nnuc)
               WRITE (6,
     &          '('' NORMALIZATION OF GDR first hump XS IGNORED'')')
               GOTO 100
            ENDIF
            if(i3.ne.0) then
              WRITE (6,
     &        '('' GDR first hump cross section uncertainty in '',I3,A2,
     &        '' is equal to '',i2,''%'')') i2, SYMb(nnuc), i3
               sigma = val*i3*0.01
               GDRpar(3,nnuc) = val + grand()*sigma
              WRITE (6,
     &        '('' GDR first hump cross section sampled value : ''
     &        ,f5.2)') GDRpar(3,nnuc)
               IPArCOV = IPArCOV +1
               write(95,'(1x,i5,1x,d12.6,1x,2i13)')
     &              IPArCOV, GDRpar(3,nnuc), INDexf,INDexb
            else
              GDRpar(3,nnuc) = val
              WRITE (6,
     &      '('' GDR first hump cross section in '',I3,A2,'' set to ''
     &      ,F5.2)') i2, SYMb(nnuc), val
              WRITE (12,
     &      '('' GDR first hump cross section in '',I3,A2,'' set to ''
     &      ,F5.2)') i2, SYMb(nnuc), val
             endif
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'EGDR2 ') THEN
            izar = i1*1000 + i2
            IF (izar.EQ.0) THEN
              DO i = 1, NDNUC
                GDRpar(4,i) = val
              ENDDO
              WRITE (6,
     &       '('' GDR second hump energy in all nuclei set to '',F5.2)')
     &        val
              WRITE (12,
     &       '('' GDR second hump energy in all nuclei set to '',F5.2)')
     &        val
              GOTO 100
            ENDIF
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (6,'('' NUCLEUS '',I3,A2,'' NOT NEEDED'')') i2,
     &                SYMb(nnuc)
               WRITE (6,
     &         '('' NORMALIZATION OF GDR second hump energy IGNORED'')')
               GOTO 100
            ENDIF
            if(i3.ne.0) then
              WRITE (6,
     &        '('' GDR second hump energy uncertainty in '',I3,A2,
     &        '' is equal to '',i2,''%'')') i2, SYMb(nnuc), i3
               sigma = val*i3*0.01
               GDRpar(4,nnuc) = val + grand()*sigma
              WRITE (6,
     &        '('' GDR second hump energy sampled value : '',f5.2)')
     &        GDRpar(4,nnuc)
               IPArCOV = IPArCOV +1
               write(95,'(1x,i5,1x,d12.6,1x,2i13)')
     &              IPArCOV, GDRpar(4,nnuc), INDexf,INDexb
            else
              GDRpar(4,nnuc) = val
              WRITE (6,
     &      '('' GDR second hump energy in '',I3,A2,'' set to '',F5.2)'
     &        ) i2, SYMb(nnuc), val
              WRITE (12,
     &      '('' GDR second hump energy in '',I3,A2,'' set to '',F5.2)'
     &        ) i2, SYMb(nnuc), val
             endif
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'GGDR2 ') THEN
            izar = i1*1000 + i2
            IF (izar.EQ.0) THEN
              DO i = 1, NDNUC
                GDRpar(5,i) = val
              ENDDO
              WRITE (6,
     &        '('' GDR second hump width in all nuclei set to '',F5.2)')
     &        val
              WRITE (12,
     &        '('' GDR second hump width in all nuclei set to '',F5.2)')
     &        val
              GOTO 100
            ENDIF
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (6,'('' NUCLEUS '',I3,A2,'' NOT NEEDED'')') i2,
     &                SYMb(nnuc)
               WRITE (6,
     &          '('' NORMALIZATION OF GDR FIRST HUMP WIDTH IGNORED'')')
               GOTO 100
            ENDIF
            if(i3.ne.0) then
              WRITE (6,
     &        '('' GDR second hump width uncertainty in '',I3,A2,
     &        '' is equal to '',i2,''%'')') i2, SYMb(nnuc), i3
               sigma = val*i3*0.01
               GDRpar(5,nnuc) = val + grand()*sigma
              WRITE (6,
     &        '('' GDR second hump width sampled value : '',f5.2)')
     &        GDRpar(5,nnuc)
               IPArCOV = IPArCOV +1
               write(95,'(1x,i5,1x,d12.6,1x,2i13)')
     &              IPArCOV, GDRpar(5,nnuc), INDexf,INDexb
            else
              GDRpar(5,nnuc) = val
              WRITE (6,
     &      '('' GDR second hump width in '',I3,A2,'' set to '',F5.2)'
     &        ) i2, SYMb(nnuc), val
              WRITE (12,
     &      '('' GDR second hump width in '',I3,A2,'' set to '',F5.2)'
     &        ) i2, SYMb(nnuc), val
             endif
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'CSGDR2') THEN
            izar = i1*1000 + i2
            IF (izar.EQ.0) THEN
              DO i = 1, NDNUC
                GDRpar(6,i) = val
              ENDDO
              WRITE (6,
     &        '('' GDR second hump cross section in all nuclei set to ''
     &        ,F5.2)') val
              WRITE (12,
     &        '('' GDR second hump cross section in all nuclei set to ''
     &        ,F5.2)') val
              GOTO 100
            ENDIF
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (6,'('' NUCLEUS '',I3,A2,'' NOT NEEDED'')') i2,
     &                SYMb(nnuc)
               WRITE (6,
     &          '('' NORMALIZATION OF GDR FIRST HUMP XS IGNORED'')')
               GOTO 100
            ENDIF
            if(i3.ne.0) then
              WRITE (6,
     &        '('' GDR second hump cross section uncertainty in '',I3,A2
     &        ,'' is equal to '',i2,''%'')') i2, SYMb(nnuc), i3
               sigma = val*i3*0.01
               GDRpar(6,nnuc) = val + grand()*sigma
              WRITE (6,
     &        '('' GDR second hump cross section sampled value : ''
     &        ,f5.2)') GDRpar(6,nnuc)
               IPArCOV = IPArCOV +1
               write(95,'(1x,i5,1x,d12.6,1x,2i13)')
     &              IPArCOV, GDRpar(6,nnuc), INDexf,INDexb
            else
              GDRpar(6,nnuc) = val
              WRITE (6,
     &      '('' GDR second hump cross section in '',I3,A2,'' set to ''
     &      ,F5.2)') i2, SYMb(nnuc), val
              WRITE (12,
     &      '('' GDR second hump cross section in '',I3,A2,'' set to ''
     &      ,F5.2)') i2, SYMb(nnuc), val
             endif
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'MSD   ') THEN
            MSD = val
            IF (MSD.EQ.1) WRITE (6,
     &'('' MSD calculations with ORION+TRISTAN were selected'')
     &   ')
            IF (MSD.EQ.1) WRITE (12,
     &'('' MSD calculations with ORION+TRISTAN were used'')')
            IF (MSD.EQ.2) WRITE (6,
     &         '('' including contribution to discrete levels'')')
            IF (MSD.EQ.2) WRITE (12,
     &         '('' including contribution to discrete levels'')')
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'MSC   ') THEN
            MSC = val
            IF (MSC.NE.0) WRITE (6,
     &                '('' Heidelberg MSC calculations were selected'')'
     &                )
            IF (MSC.NE.0) WRITE (12,
     &                '('' Heidelberg MSC calculations were used'')')
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'HMS   ') THEN
            LHMs = val
            IF (LHMs.NE.0) WRITE (6,
     &            '('' HMS preequilibrium calculations were selected'')'
     &            )
            IF (LHMs.NE.0) WRITE (12,
     &            '('' HMS preequilibrium calculations were used'')')
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'NHMS  ') THEN
            IF (val.GT.99.0D0) THEN
               NHMs = val
               WRITE (6,'('' Number of events in HMS set to '',I10)')
     &                NHMs
               WRITE (12,'('' Number of events in HMS '',I10)') NHMs
            ENDIF
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'CHMS  ') THEN
            IF (val.GT.0.0D0) THEN
               CHMs = val
               WRITE (6,
     &             '('' Default damp rate in HMS multiplied by '',F6.3)'
     &             ) CHMs
               WRITE (12,
     &             '('' Default damp rate in HMS multiplied by '',F6.3)'
     &             ) CHMs
            ENDIF
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'HRTW  ') THEN
            IF (val.GT.0.) THEN
              LHRtw = 1
              EHRtw = 5.d0
              IF (val.gt.1.1d0) EHRtw = val
              IF (LHRtw.NE.0) WRITE (6,
     &           '('' HRTW width fluctuation correction was selected'',
     &             '' up to '',f4.2,'' MeV'')') EHRtw
              IF (LHRtw.NE.0) WRITE (12,
     &           '('' Width fluctuations calculated within HRTW '',
     &             '' up to '',f4.2,'' MeV'')') EHRtw
            ENDIF
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'GDRWP ') THEN
            DIToro = val
            WRITE (6,
     &             '('' Factor in energy increase of GDR width'',F7.5)')
     &             DIToro
            WRITE (12,
     &             '('' Factor in energy increase of GDR width'',F7.5)')
     &             DIToro
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'GDRWA1') THEN
            GDRwa1 = val
            WRITE (6,
     &         '('' GDR first hump width increased by '',F5.2,'' MeV'')'
     &         ) GDRwa1
            WRITE (12,
     &         '('' GDR first hump width increased by '',F5.2,'' MeV'')'
     &         ) GDRwa1
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'GDRWA2') THEN
            GDRwa2 = val
            WRITE (6,
     &        '('' GDR second hump width increased by '',F5.2,'' MeV'')'
     &        ) GDRwa2
            WRITE (12,
     &        '('' GDR second hump width increased by '',F5.2,'' MeV'')'
     &        ) GDRwa2
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'GDRESH') THEN
            GDResh = val
            WRITE (6,'('' GDR position shifted by '',F6.3,'' MeV'')')
     &             GDResh
            WRITE (12,'('' GDR position shifted by '',F6.3,'' MeV'')')
     &             GDResh
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'GDRSPL') THEN
            GDRspl = val
            WRITE (6,
     &'('' Splitting of GDR peaks increased by '',F6.3,         '' MEV''
     &)') GDRspl
            WRITE (12,
     &'('' Splitting of GDR peaks increased by '',F6.3,         '' MEV''
     &)') GDRspl
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'GDRST1  ') THEN
            EWSr1 = val
            WRITE (6,
     &'('' Gamma strength multiplied by '',F6.3,'' for the first GDR hum
     &p'')') EWSr1
            WRITE (12,
     &'('' Gamma strength multiplied by '',F6.3,'' for the first GDR hum
     &p'')') EWSr1
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'GDRST2  ') THEN
            EWSr2 = val
            WRITE (6,
     &'('' Gamma strength multiplied by '',F6.3,'' for the second GDR hu
     &mp'')') EWSr2
            WRITE (12,
     &'('' Gamma strength multiplied by '',F6.3,'' for the second GDR hu
     &mp'')') EWSr2
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'GDRWEI') THEN
            GDRweis = val
            WRITE (6,
     &'('' Gamma strength composed of '',F7.3''% GDR + ''
     &,F7.3,''% Weisskopf'')') GDRweis*100., (1. - GDRweis)*100.0
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'DEFPAR') THEN
            DEFpar = val
            WRITE (6,'('' Dynamic deformation multiplyer '',F7.3)')
     &             DEFpar
            WRITE (12,'('' Dynamic deformation multiplyer '',F7.3)')
     &             DEFpar
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'DEFGA ') THEN
            DEFga = val
            WRITE (6,
     &'('' Gaussian correction to deformation (amplitude)'',F7.3)
     &') DEFga
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'DEFGW ') THEN
            DEFgw = val
            WRITE (6,
     &'('' Gaussian correction to deformation (width)'',F7.3)
     &') DEFgw
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'DEFGP ') THEN
            DEFgp = val
            WRITE (6,
     &'('' Gaussian correction to deformation (position)'',F7.3)
     &') DEFgp
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'ENDF  ') THEN
             IF(i1.eq.0 .OR. i2.eq.0) THEN
C             Setting ENDF for all emission loops
              NENdf = INT(val)
              WRITE (6,'('' ENDF formatting enabled'')')
              WRITE (6,'(
     &         '' Exclusive spectra available up to'',
     &         '' emission loop # '',I2)') NENdf
              WRITE (12,'(
     &         '' Exclusive spectra available up to'',
     &         '' emission loop # '',I2)') NENdf
                  GOTO 100
             ENDIF
             IF(val.LT.0) THEN
               WRITE (6,'('' WRONG ENDF value for NUCLEUS '',I3,A2)')
     &                i2, SYMb(nnuc)
               WRITE (6,'('' SETTING IGNORED'')')
               GOTO 100
             ENDIF
C           Setting ENDF for a single nucleus
            izar = i1*1000 + i2
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (6,'('' NUCLEUS '',I3,A2,'' NOT NEEDED'')') i2,
     &                SYMb(nnuc)
               WRITE (6,'('' ENDF SETTING IGNORED'')')
               GOTO 100
            ENDIF
            ENDf(nnuc) = INT(val)
            IF (ENDf(nnuc).EQ.1) THEN
              WRITE (6,
     &       '('' Exclusive spectra will be available for emission'',
     &         '' from nucleus '',I3,A2)') i2, SYMb(nnuc)
              WRITE (12,
     &       '('' Exclusive spectra will be available for emission'',
     &         '' from nucleus '',I3,A2)') i2, SYMb(nnuc)
            ENDIF
            IF (ENDf(nnuc).EQ.2) THEN
              WRITE (6,
     &       '('' Emission spectra from nucleus '',I3,A2,
     &         '' will be stored as inclusive'')') i2, SYMb(nnuc)
              WRITE (12,
     &       '('' Emission spectra from nucleus '',I3,A2,
     &         '' will be stored as inclusive'')') i2, SYMb(nnuc)
            ENDIF
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'OMPOT ') THEN
            IF (i1.LT.1 .OR. i1.GT.NEJcm) THEN
               WRITE (6,
     &                '('' WARNING: EJECTILE IDENTIFICATION '',I2,
     &                  '' UNKNOWN'')') i1
               WRITE (6,'('' WARNING: OPTICAL MODEL SETTING IGNORED'')')
               GOTO 100
            ENDIF
C-----
            IF (val.LT.0) THEN
               ki = 26
               ipoten = -INT(val)
C--------------Searching in the RIPL database for i1 catalog number
               CALL FINDPOT(ki,ieof,ipoten)
               IF (ieof.EQ.0) THEN
                  val = -val
               ELSE
                  WRITE (6,*) 'Requested RIPL entry ', ipoten,
     &                        ' not found, using default choice'
                  GOTO 100
               ENDIF
               WRITE (6,
     &'('' Optical model parameters for ejectile '', I1,'' set to RIPL #
     &'', I4)') i1, INT(val)
            ELSE
               WRITE (6,
     &    '('' Only RIPL OMP parameters are supported in EMPIRE 2.19'')'
     &    )
               STOP
            ENDIF
C-----
            DO i = 1, NDNUC
               KTRlom(i1,i) = INT(val)
            ENDDO
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'BNDG  ') THEN
            IF (i3.LE.0 .OR. i3.GT.NEJcm) THEN
               WRITE (6,
     &                '('' EJECTILE IDENTIFICATION '',I2,'' UNKNOWN'')')
     &                i3
               WRITE (6,'('' BINDING ENERGY SETTING IGNORED'')')
               GOTO 100
            ENDIF
            izar = i1*1000 + i2
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (6,'('' NUCLEUS '',I3,A2,'' NOT NEEDED'')') i2,
     &                SYMb(nnuc)
               WRITE (6,'('' BINDING ENERGY SETTING IGNORED'')')
               GOTO 100
            ENDIF
            Q(i3,nnuc) = val
            WRITE (6,
     & '('' Binding energy of '',A2,'' in '',I3,A2,'' set to ''  ,F7.3)'
     & ) SYMbe(i3), i2, SYMb(nnuc), val
            IF (nnuc.EQ.1 .AND. IZAejc(0).EQ.IZAejc(i3)) Q(0,1) = val
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'JSTAB ') THEN
            izar = i1*1000 + i2
            IF (izar.EQ.0) THEN
               DO i = 1, NDNUC
                  JSTab(i) = val
               ENDDO
               IF (val.GT.0.0D0) WRITE (6,
     &    '('' Stability limit set to spin '',F6.1,'' for all nuclei'')'
     &    ) val
               GOTO 100
            ENDIF
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (6,'('' NUCLEUS '',I3,A2,'' NOT NEEDED'')') i2,
     &                SYMb(nnuc)
               WRITE (6,'('' SETTING STABILITY LIMIT IGNORED'')')
               GOTO 100
            ENDIF
            JSTab(nnuc) = val
            WRITE (6,
     &             '('' Stability limit in '',I3,A2,'' set to '',F6.1)')
     &             i2, SYMb(nnuc), val
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'GCROA ') THEN
            izar = i1*1000 + i2
            IF (izar.EQ.0) THEN
               DO i = 1, NDNUC
                  ROPaa(i) = val
               ENDDO
               IF (val.GT.0.0D0) WRITE (6,
     &       '('' L. d. a-parameter set to '',F6.2,'' for all nuclei'')'
     &       ) val
               IF (val.EQ.0.0D0) WRITE (6,
     &      '('' L. d. a-parameter according to Ignatyuk systematics'')'
     &      )
               IF (val.EQ.( - 1.D0)) WRITE (6,
     &        '('' L. d. a-parameter according to Arthur systematics'')'
     &        )
               IF (val.EQ.( - 2.D0)) WRITE (6,
     &         '('' L. d. a-parameter according to Mebel systematics'')'
     &         )
               IF (val.GT.0.0D0) WRITE (12,
     &       '('' L. d. a-parameter set to '',F6.2,'' for all nuclei'')'
     &       ) val
               IF (val.EQ.0.0D0) WRITE (12,
     &      '('' L. d. a-parameter according to Ignatyuk systematics'')'
     &      )
               IF (val.EQ.( - 1.D0)) WRITE (12,
     &        '('' L. d. a-parameter according to Arthur systematics'')'
     &        )
               IF (val.EQ.( - 2.D0)) WRITE (12,
     &         '('' L. d. a-parameter according to Mebel systematics'')'
     &         )
               GOTO 100
            ENDIF
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (6,'('' NUCLEUS '',I3,A2,'' NOT NEEDED'')') i2,
     &                SYMb(nnuc)
               WRITE (6,'('' L.D. a-PARAMETER SETTING IGNORED'')')
               GOTO 100
            ENDIF
            ROPaa(nnuc) = val
            WRITE (6,
     & '('' L.d. a-parameter   in '',I3,A2,'' set to ''          ,F6.2)'
     & ) i2, SYMb(nnuc), val
            WRITE (12,
     & '('' L.d. a-parameter   in '',I3,A2,'' set to ''          ,F6.2)'
     & ) i2, SYMb(nnuc), val
            GOTO 100
         ENDIF

         IF (name.EQ.'ATILNO') THEN
            izar = i1*1000 + i2
            IF (izar.EQ.0) THEN
               DO i = 1, NDNUC
                  ATIlnor(i) = val
               ENDDO
               WRITE (6,
     &       '('' L.d. a-parameter in all nuclei multiplied by '',F6.2)'
     &       ) val
               WRITE (12,
     &       '('' L.d. a-parameter in all nuclei multiplied by '',F6.2)'
     &       ) val
               GOTO 100
            ENDIF
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (6,'('' NUCLEUS '',I3,A2,'' NOT NEEDED'')') i2,
     &                SYMb(nnuc)
               WRITE (6,'('' NORMALIZATION OF a-tilde IGNORED'')')
               GOTO 100
            ENDIF

            if(i3.ne.0) then
              WRITE (6,
     &        '('' L.d. a-parameter uncertainty in '',I3,A2,
     &        '' is equal to '',i2,''%'')') i2, SYMb(nnuc), i3
               sigma = val*i3*0.01
C              ATIlnor(nnuc) = val + grand()*sigma
               ATIlnor(nnuc) = val + (2*drand()-1.)*sigma
              WRITE (6,
     &        '('' L.d. a-parameter sampled value : '',f8.3)')
     &        ATIlnor(nnuc)
               IPArCOV = IPArCOV +1
               write(95,'(1x,i5,1x,d12.6,1x,2i13)')
     &              IPArCOV, ATIlnor(nnuc),INDexf,INDexb
            else
              ATIlnor(nnuc) = val
              WRITE (6,
     &      '('' L.d. a-parameter in '',I3,A2,'' multiplied by '',F6.2)'
     &        ) i2, SYMb(nnuc), val
             endif

            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'GTILNO') THEN
            izar = i1*1000 + i2
            IF (izar.EQ.0) THEN
               DO i = 1, NDNUC
                  GTIlnor(i) = val
               ENDDO
               WRITE (6,
     &     '('' Single particle l.d. parameter g multiplied by '',F6.2)'
     &     ) val
               WRITE (12,
     &     '('' Single particle l.d. parameter g multiplied by '',F6.2)'
     &     ) val
               GOTO 100
            ENDIF
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (6,'('' NUCLEUS '',I3,A2,'' NOT NEEDED'')') i2,
     &                SYMb(nnuc)
               WRITE (6,'('' NORMALIZATION OF G-tilde IGNORED'')')
               GOTO 100
            ENDIF

            if(i3.ne.0) then
              WRITE (6,
     &        '('' Single particle l.d. parameter g uncertainty in '',
     &        I3,A2,'' is equal to '',i2,''%'')') i2, SYMb(nnuc), i3
               sigma = val*i3*0.01
C              GTIlnor(nnuc) = val + grand()*sigma
               GTIlnor(nnuc) = val + (2*drand()-1.)*sigma
              WRITE (6,
     &        '('' Single particle l.d. parameter g sampled value : '',
     &        f8.3)') GTIlnor(nnuc)
               IPArCOV = IPArCOV +1
               write(95,'(1x,i5,1x,d12.6,1x,2i13)')
     &              IPArCOV, GTIlnor(nnuc),INDexf,INDexb
            else
              GTIlnor(nnuc) = val
              WRITE (6,
     &'('' Single particle l.d. parameter g in '',I3,A2,
     &  '' multiplied by '',        F6.2)') i2, SYMb(nnuc), val
             endif
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'GCROUX') THEN
            izar = i1*1000 + i2
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (6,'('' NUCLEUS '',I3,A2,'' NOT NEEDED'')') i2,
     &                SYMb(nnuc)
               WRITE (6,'('' L.D. PARAMETER Ux SETTING IGNORED'')')
               GOTO 100
            ENDIF
            ROPar(2,nnuc) = val
            WRITE (6,
     & '('' L.d. parameter Ux  in '',I3,A2,'' set to ''          ,F6.3)'
     & ) i2, SYMb(nnuc), val
            WRITE (12,
     & '('' L.d. parameter Ux  in '',I3,A2,'' set to ''          ,F6.3)'
     & ) i2, SYMb(nnuc), val
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'GCROD ') THEN
            izar = i1*1000 + i2
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (6,'('' NUCLEUS '',I3,A2,'' NOT NEEDED'')') i2,
     &                SYMb(nnuc)
               WRITE (6,'('' PAIRING SHIFT SETTING IGNORED'')')
               GOTO 100
            ENDIF
            ROPar(3,nnuc) = val
            WRITE (6,
     &      '('' Pairing shift in '',I3,A2,'' set to ''          ,F6.3)'
     &      ) i2, SYMb(nnuc), val
            WRITE (12,
     &      '('' Pairing shift in '',I3,A2,'' set to ''          ,F6.3)'
     &      ) i2, SYMb(nnuc), val
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'GCROE0') THEN
            izar = i1*1000 + i2
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (6,'('' NUCLEUS '',I3,A2,'' NOT NEEDED'')') i2,
     &                SYMb(nnuc)
               WRITE (6,'('' L.D. PARAMETER E0 SETTING IGNORED'')')
               GOTO 100
            ENDIF
            ROPar(4,nnuc) = val
            WRITE (6,
     &  '('' L.d. parameter Eo in '',I3,A2,'' set to ''          ,F6.3)'
     &  ) i2, SYMb(nnuc), val
            WRITE (12,
     &  '('' L.d. parameter Eo in '',I3,A2,'' set to ''          ,F6.3)'
     &  ) i2, SYMb(nnuc), val
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'GCROT ') THEN
            izar = i1*1000 + i2
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (6,'('' NUCLEUS '',I3,A2,'' NOT NEEDED'')') i2,
     &                SYMb(nnuc)
               WRITE (6,'('' L.D. PARAMETER T  SETTING IGNORED'')')
               GOTO 100
            ENDIF
            ROPar(5,nnuc) = val
            WRITE (6,
     &   '('' L.d. parameter T in '',I3,A2,'' set to ''          ,F6.3)'
     &   ) i2, SYMb(nnuc), val
            WRITE (12,
     &   '('' L.d. parameter T in '',I3,A2,'' set to ''          ,F6.3)'
     &   ) i2, SYMb(nnuc), val
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'FITLEV') THEN
            FITlev = val
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'FITOMP') THEN
            FITomp = val
            IF (FITomp.GT.0.0D0) WRITE (6,
     &'('' OM parameter adjustment selected,'',
     & '' (will reset several options'')')
            GOTO 100
         ENDIF

C--------Tuning factors
         IF (name.EQ.'TUNEPE') THEN
            IF (i1.GT.NDEJC) THEN
               WRITE (6,'('' UNKNOWN EJECTILE in PE TUNE '',I2)') i3
               GOTO 100
            ENDIF
            if(i2.gt.0.) then
              WRITE (6,
     &'('' PE emission width of ejectile '',I1,'' from '',I3,A2,
     &         '' uncertainty is equal to '',i2,'' %'')')
     &        i1, A(1), SYMb(1), i2
               sigma = val*0.01*i2
C              TUNEpe(i1) = val + grand()*sigma
               TUNEpe(i1) = val + (2*drand()-1.)*sigma
              WRITE (6,
     &'('' PE emission width of ejectile '',I1,'' from '',I3,A2,
     &  '' multiplied by '',F6.3)') i1, A(1), SYMb(1), TUNEpe(i1)
              IPArCOV = IPArCOV +1
              WRITE(95,'(1x,i5,1x,d12.6,1x,2i13)')
     &           IPArCOV, TUNEpe(i1), INDexf,INDexb
            else
              TUNEpe(i1) = val
              WRITE (6,
     &'('' PE emission width of ejectile '',I1,'' from '',I3,A2,
     &  '' multiplied by '',F6.3)') i1, NINT(A(1)), SYMb(1), val
              WRITE (12,
     &'('' PE emission width of ejectile '',I1,'' from '',I3,A2,
     &  '' multiplied by '',F6.3)') i1, NINT(A(1)), SYMb(1), val
            endif
            GOTO 100
         ENDIF

         IF (name.EQ.'TUNEFI') THEN
            izar = i1*1000 + i2
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (6,'('' NUCLEUS '',I3,A2,'' NOT NEEDED'')') i2,
     &                SYMb(nnuc)
               WRITE (6,'('' FISSION WIDTH TUNING IGNORED'')')
               GOTO 100
            ENDIF
            if(i3.gt.0.) then
              WRITE (6,
     &'('' Uncertainty of the Fission width of nucleus '',I3,A2,
     &         '' is equal to '',i2,'' %'')')
     &         i2, SYMb(nnuc), i3
               sigma = val*0.01*i3
C              TUNefi(nnuc) = val + grand()*sigma
               TUNefi(nnuc) = val + (2*drand()-1.)*sigma
              WRITE (6,
     &'('' Fission width of nucleus '',I3,A2,
     &  '' multiplied by '',F7.3)') i2, SYMb(nnuc), TUNefi(nnuc)
              IPArCOV = IPArCOV +1
               write(95,'(1x,i5,1x,d12.6,1x,2i13)')
     &       IPArCOV, TUNefi(nnuc), INDexf,INDexb
             else
              TUNefi(nnuc) = val
              WRITE (6,
     &'('' Fission width of nucleus '',I3,A2,
     &  '' multiplied by '',F7.3)') i2, SYMb(nnuc), val
              WRITE (12,
     &'('' Fission width of nucleus '',I3,A2,
     &  '' multiplied by '',F7.3)') i2, SYMb(nnuc), val
            endif
            GOTO 100
         ENDIF

         IF (name.EQ.'TUNE  ') THEN
            izar = i1*1000 + i2
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (6,'('' NUCLEUS '',I3,A2,'' NOT NEEDED'')') i2,
     &                SYMb(nnuc)
               WRITE (6,'('' TUNING IGNORED'')')
               GOTO 100
            ENDIF
            IF (i3.GT.NDEJC) THEN
               WRITE (6,'('' UNKNOWN EJECTILE in TUNE '',I2)') i3
               GOTO 100
            ENDIF
            if(i4.gt.0.) then
              WRITE (6,
     &'('' Emission width of ejectile '',I1,'' from '',I3,A2,
     &         '' uncertainty is equal to '',i2,'' %'')')
     &        i3, i2, SYMb(nnuc), i4
               sigma = val*0.01*i4
C              TUNe(i3,nnuc) = val + grand()*sigma
               TUNe(i3,nnuc) = val + (2*drand()-1.)*sigma
              WRITE (6,
     &'('' Emission width of ejectile '',I1,'' from '',I3,A2,
     &  '' multiplied by '',F7.3)') i3, i2, SYMb(nnuc), TUNe(i3,nnuc)
              IPArCOV = IPArCOV +1
               write(95,'(1x,i5,1x,d12.6,1x,2i13)')
     &       IPArCOV, TUNe(i3,nnuc), INDexf,INDexb
             else
              TUNe(i3,nnuc) = val
              WRITE (6,
     &'('' Emission width of ejectile '',I1,'' from '',I3,A2,
     &         '' multiplied by '',F7.3)') i3, i2, SYMb(nnuc), val
              WRITE (12,
     &'('' Emission width of ejectile '',I1,'' from '',I3,A2,
     &  '' multiplied by '',F7.3)') i3, i2, SYMb(nnuc), TUNe(i3,nnuc)
            endif
            GOTO 100
         ENDIF
C--------input for TRISTAN (MSD)
         IF (name.EQ.'WIDEX ') THEN
            WIDexin = val
            IF (WIDexin.GT.0.0D0) WRITE (6,
     &'('' Experimental energy resolution in MSD set to'',F6.3,'' MeV'')
     &') WIDexin
            IF (WIDexin.GT.0.0D0) WRITE (12,
     &'('' Experimental energy resolution in MSD set to'',F6.3,'' MeV'')
     &') WIDexin
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'COMPFF') THEN
            ICOmpff = val
            IF (ICOmpff.GT.0) THEN
               WRITE (6,
     &'('' Compressional l=0 form factor''
     &,'' used in MSD calculations'')')
               WRITE (12,
     &'('' Compressional l=0 form factor''
     &,'' used in MSD calculations'')')
            ELSE
               WRITE (6,
     &'('' Surface l=0 form factor''
     &,'' used in MSD calculations'')')
               WRITE (12,
     &'('' Surface l=0 form factor''
     &,'' used in MSD calculations'')')
            ENDIF
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'GAPP  ') THEN
            GAPin(2) = val
            IF (GAPin(2).GT.0.0D0) WRITE (6,
     &'('' Proton  pairing gap for target in MSD set to'',F6.3,'' MeV'')
     &') GAPin(2)
            IF (GAPin(2).GT.0.0D0) WRITE (12,
     &'('' Proton  pairing gap for target in MSD set to'',F6.3,'' MeV'')
     &') GAPin(2)
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'GAPN  ') THEN
            GAPin(1) = val
            IF (GAPin(1).GT.0.0D0) WRITE (6,
     &'('' Neutron pairing gap for target in MSD set to'',F6.3,'' MeV'')
     &') GAPin(1)
            IF (GAPin(1).GT.0.0D0) WRITE (12,
     &'('' Neutron pairing gap for target in MSD set to'',F6.3,'' MeV'')
     &') GAPin(1)
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'HOMEGA') THEN
            HOMin = val
            IF (HOMin.GT.0.0D0) WRITE (6,
     &'('' Harmonic oscillator energy (hbar*omega) for MSD'',   F6.3,''
     &MeV'')') HOMin
            IF (HOMin.GT.0.0D0) WRITE (12,
     &'('' Harmonic oscillator energy (hbar*omega) for MSD'',   F6.3,''
     &MeV'')') HOMin
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'ALS   ') THEN
            ALSin = val
            IF (ALSin.GT.0.0D0) WRITE (6,
     &'('' l*s coupling strength in harmonic oscill. (MSD)'',   F6.3,''
     &MeV'')') ALSin
            IF (ALSin.GT.0.0D0) WRITE (12,
     &'('' l*s coupling strength in harmonic oscill. (MSD)'',   F6.3,''
     &MeV'')') ALSin
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'EFIT  ') THEN
            EFItin(i1 + 1) = val
            IF (EFItin(i1 + 1).GT.0.0D0) WRITE (6,
     &'('' Field strength of multipolarity'',I2,'' fitted to the level a
     &t '',F6.3,'' MeV'')') i1, EFItin(i1 + 1)
            IF (EFItin(i1 + 1).GT.0.0D0) WRITE (12,
     &'('' Field strength of multipolarity'',I2,'' fitted to the level a
     &t '',F6.3,'' MeV'')') i1, EFItin(i1 + 1)
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'RESNOR') THEN
            CNOrin(i1 + 1) = val
            WRITE (6,
     &'('' Response function for multipolarity'',I3,'' normalized by fac
     &tor '',F6.3)') i1, CNOrin(i1 + 1)
            WRITE (12,
     &'('' Response function for multipolarity'',I3,'' normalized by fac
     &tor '',F6.3)') i1, CNOrin(i1 + 1)
            GOTO 100
         ENDIF
C--------TRISTAN (MSD) input **** done ****
         IF (name.EQ.'NIXSH ') THEN
            SHNix = val
            IF (SHNix.NE.0.0D0) WRITE (6,
     &                '('' Shell corrections according to Nix-Moller'')'
     &                )
            IF (SHNix.EQ.0.0D0) WRITE (6,
     &           '('' Shell corrections according to Myers-Swiatecki'')'
     &           )
            IF (SHNix.NE.0.0D0) WRITE (12,
     &                '('' Shell corrections according to Nix-Moller'')'
     &                )
            IF (SHNix.EQ.0.0D0) WRITE (12,
     &           '('' Shell corrections according to Myers-Swiatecki'')'
     &           )
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'GCASC ') THEN
            GCAsc = val
            IF (GCAsc.NE.0.0D0) WRITE (6,
     &              '('' Full gamma cascade in the first CN selected'')'
     &              )
            IF (GCAsc.EQ.0.0D0) WRITE (6,
     &             '('' Only primary gammas in the first CN selected'')'
     &             )
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'E1    ') THEN
            IF (val.GT.0.0D+0) THEN
               WRITE (6,'('' E1 photo-absorption selected'')')
               WRITE (12,'('' E1 photo-absorption selected'')')
            ELSE
               WRITE (6,'('' E1 photo-absorption blocked'')')
               WRITE (12,'('' E1 photo-absorption blocked'')')
            ENDIF
            IGE1 = val
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'M1    ') THEN
            IF (val.GT.0.0D+0) THEN
               WRITE (6,'('' M1 photo-absorption selected'')')
               WRITE (12,'('' M1 photo-absorption selected'')')
            ELSE
               WRITE (6,'('' M1 photo-absorption blocked'')')
               WRITE (12,'('' M1 photo-absorption blocked'')')
            ENDIF
            IGM1 = val
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'E2    ') THEN
            IF (val.GT.0.0D+0) THEN
               WRITE (6,'('' E2 photo-absorption selected'')')
               WRITE (12,'('' E2 photo-absorption selected'')')
            ELSE
               WRITE (6,'('' E2 photo-absorption blocked'')')
               WRITE (12,'('' E2 photo-absorption blocked'')')
            ENDIF
            IGE2 = val
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'QD    ') THEN
            IF (IGE1.EQ.0) THEN
               WRITE (6,
     &'('' Quasideuteron photo-absorption is '',          '' suppressed
     &since E1 photo-absorption is blocked!'')')
               WRITE (12,
     &'('' Quasideuteron photo-absorption is '',          '' suppressed
     &since E1 photo-absorption is blocked!'')')
            ELSE
               LQDfac = val
               WRITE (6,
     &'('' Quasideuteron photoabsorption cross section'',
     &  '' normalized by a factor '',F6.3)') LQDfac
               WRITE (12,
     &'('' Quasideuteron photoabsorption cross section'',
     &  '' normalized by a factor '',F6.3)') LQDfac
            ENDIF
            GOTO 100
         ENDIF
C-----
C--------checking for fission data in the optional input
         IF (name.EQ.'FISSHI') THEN
            izar = i1*1000 + i2
            IF (val.EQ.0) THEN
               fstring = 'advanced treatment of fission'
            ELSEIF (val.EQ.1) THEN
               fstring = 'fission over single-humped barrier'
            ELSEIF (val.EQ.2) THEN
               fstring = 'fission ignored'
            ELSE
               fstring = 'illegal value for FISSHI'
            ENDIF
            IF (izar.EQ.0) THEN
               DO nnuc = 1, NDNUC
                  FISshi(nnuc) = val
               ENDDO
               WRITE (6,*) 'For all nuclei: ', fstring
               WRITE (12,*) 'For all nuclei: ', fstring
               GOTO 100
            ENDIF
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (6,'('' NUCLEUS '',I3,A2,'' NOT NEEDED'')') i2,
     &                SYMb(nnuc)
               WRITE (6,'('' FISSHI SETTING IGNORED'')')
               GOTO 100
            ENDIF
            FISshi(nnuc) = val
            WRITE (6,*) 'For ', i2, '-', SYMb(nnuc), ' ', fstring
            WRITE (12,*) 'For ', i2, '-', SYMb(nnuc), ' ', fstring
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'FISMOD') THEN
            izar = i1*1000 + i2
            IF (izar.EQ.0) THEN
               DO nnuc = 1, NDNUC
                  FISmod(nnuc) = val
               ENDDO
               WRITE (6,'('' FISMOD  in all nuclei set to '',F6.3)') val
               GOTO 100
            ENDIF
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (6,'('' NUCLEUS '',I3,A2,'' NOT NEEDED'')') i2,
     &                SYMb(nnuc)
               WRITE (6,'('' FISMOD SETTING IGNORED'')')
               GOTO 100
            ENDIF
            FISmod(nnuc) = val
            WRITE (6,
     &            '('' FISMOD  in '',I3,A2,'' set to ''          ,F6.3)'
     &            ) i2, SYMb(nnuc), val
            WRITE (12,
     &            '('' FISMOD  in '',I3,A2,'' set to ''          ,F6.3)'
     &            ) i2, SYMb(nnuc), val
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'FISOPT') THEN
            izar = i1*1000 + i2
            IF (izar.EQ.0) THEN
               DO nnuc = 1, NDNUC
                  FISopt(nnuc) = val
               ENDDO
               WRITE (6,'('' FISOPT  in all nuclei set to '',F6.3)') val
               WRITE (12,'('' FISOPT  in all nuclei set to '',F6.3)')val
               GOTO 100
            ENDIF
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (6,'('' NUCLEUS '',I3,A2,'' NOT NEEDED'')') i2,
     &                SYMb(nnuc)
               WRITE (6,'('' FISOPT SETTING IGNORED'')')
               GOTO 100
            ENDIF
            FISopt(nnuc) = val
            WRITE (6,
     &            '('' FISOPT  in '',I3,A2,'' set to ''          ,F6.3)'
     &            ) i2, SYMb(nnuc), val
            WRITE (12,
     &            '('' FISOPT  in '',I3,A2,'' set to ''          ,F6.3)'
     &            ) i2, SYMb(nnuc), val
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'FISBAR') THEN
            izar = i1*1000 + i2
            IF (izar.EQ.0) THEN
               DO nnuc = 1, NDNUC
                  FISbar(nnuc) = val
               ENDDO
               WRITE (6,'('' FISBAR  in all nuclei set to '',F6.3)') val
               WRITE (12,'('' FISBAR  in all nuclei set to '',F6.3)')val
               GOTO 100
            ENDIF
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (6,'('' NUCLEUS '',I3,A2,'' NOT NEEDED'')') i2,
     &                SYMb(nnuc)
               WRITE (6,'('' FISBAR SETTING IGNORED'')')
               GOTO 100
            ENDIF
            FISbar(nnuc) = val
            WRITE (6,
     &            '('' FISBAR  in '',I3,A2,'' set to ''          ,F6.3)'
     &            ) i2, SYMb(nnuc), val
            WRITE (12,
     &            '('' FISBAR  in '',I3,A2,'' set to ''          ,F6.3)'
     &            ) i2, SYMb(nnuc), val
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'FISDEN') THEN
            izar = i1*1000 + i2
            IF (izar.EQ.0) THEN
               DO nnuc = 1, NDNUC
                  FISden(nnuc) = val
               ENDDO
               WRITE (6,'('' FISDEN  in all nuclei set to '',F6.3)') val
               WRITE (12,'('' FISDEN  in all nuclei set to '',F6.3)')val
               GOTO 100
            ENDIF
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (6,'('' NUCLEUS '',I3,A2,'' NOT NEEDED'')') i2,
     &                SYMb(nnuc)
               WRITE (6,'('' FISDEN SETTING IGNORED'')')
               GOTO 100
            ENDIF
            FISden(nnuc) = val
            WRITE (6,
     &            '('' FISDEN  in '',I3,A2,'' set to ''          ,F6.3)'
     &            ) i2, SYMb(nnuc), val
            WRITE (12,
     &            '('' FISDEN  in '',I3,A2,'' set to ''          ,F6.3)'
     &            ) i2, SYMb(nnuc), val
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'FISDIS') THEN
            izar = i1*1000 + i2
            IF (izar.EQ.0) THEN
               DO nnuc = 1, NDNUC
                  FISdis(nnuc) = val
               ENDDO
               WRITE (6,'('' FISDIS  in all nuclei set to '',F6.3)') val
               WRITE (12,'('' FISDIS  in all nuclei set to '',F6.3)')val
               GOTO 100
            ENDIF
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (6,'('' NUCLEUS '',I3,A2,'' NOT NEEDED'')') i2,
     &                SYMb(nnuc)
               WRITE (6,'('' FISDIS SETTING IGNORED'')')
               GOTO 100
            ENDIF
            FISdis(nnuc) = val
            WRITE (6,
     &            '('' FISDIS  in '',I3,A2,'' set to ''          ,F6.3)'
     &            ) i2, SYMb(nnuc), val
            WRITE (12,
     &            '('' FISDIS  in '',I3,A2,'' set to ''          ,F6.3)'
     &            ) i2, SYMb(nnuc), val
            GOTO 100
         ENDIF
C-----
         WRITE (6,'('' INVALID KEY: '',A6,'', DISPOSITION IGNORED'')')
     &          name
      GOTO 100
  200 WRITE (6,
     &'('' FATAL: INVALID FORMAT in KEY: '',A6,
     &  '', EMPIRE STOPPED'')') name
      STOP ' FATAL: INVALID FORMAT in input KEY '
      END
C
C
      SUBROUTINE READNIX
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C
C Local variables
C
      DOUBLE PRECISION beta2x(NMASSE), ebin, efermi, emicx(NMASSE),
     &                 excess(NMASSE), xmassexp, xmassth, zmn, zmx
      DOUBLE PRECISION DMAX1
      INTEGER ia, iapro, iatar, iflag, ii, iloc, in, iz, izaf(NMASSE),
     &        izpro, iztar, k, nixa, nixz, nnuc
      REAL REAL
C
C
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
      OPEN (UNIT = 27,STATUS = 'OLD',
     &      FILE = '../RIPL-2/masses/mass-frdm95.dat',ERR = 300)
C-----Skipping header lines
      READ (27,*,END = 300)
      READ (27,*)
      READ (27,*)
      READ (27,*)
      DO k = 1, NMASSE
C  Z   A    fl    Mexp      Mth      Emic    beta2   beta3   beta4   beta6
         READ (27,'(2i4,4x,i1,3f10.3,f8.3)',END = 100) nixz, nixa,
     &         iflag, xmassexp, xmassth, emicx(k), beta2x(k)
         izaf(k) = nixz*1000 + nixa
         IF (iflag.GE.1) THEN
            excess(k) = xmassexp
         ELSE
            excess(k) = xmassth
         ENDIF
      ENDDO
  100 CLOSE (UNIT = 27)
      DO iz = 0, 130
         DO ia = 0, 400
            RESmas(iz,ia) = 0
            EXCessmass(iz,ia) = 0
         ENDDO
      ENDDO
      DO k = 1, NMASSE
         iz = izaf(k)/1000
         ia = MOD(izaf(k),1000)
         IF (iz.GT.130 .OR. ia.GT.400) GOTO 200
         RESmas(iz,ia) = REAL(ia) + excess(k)/AMUmev
         EXCessmass(iz,ia) = excess(k)
  200 ENDDO
C
C-----nucmas: subroutine for formula of Duflo-Zuker for masses outside M-N
C
      DO iz = 6, 100
         DO ia = 2*iz - 10, 3*iz
            IF (RESmas(iz,ia).EQ.0.D0) THEN
               in = ia - iz
               CALL NUCMAS(in,iz,ebin)
               RESmas(iz,ia) = iz*AMUpro + in*AMUneu - ebin/AMUmev
               EXCessmass(iz,ia) = RESmas(iz,ia)*AMUmev - REAL(ia)
            ENDIF
         ENDDO
      ENDDO
C-----mbc1 a quick/temp? solution to weird light undefined masses: define
C-----resmas=A for al nuclei so far undefined
C-----prvisouly i had a problem for be6 => be5 +n since mass be5 undefined
      DO iz = 1, 130
         DO ia = 1, 400
            IF (RESmas(iz,ia).EQ.0.D0) THEN
               RESmas(iz,ia) = REAL(ia)/AMUmev
               EXCessmass(iz,ia) = 0
            ENDIF
         ENDDO
      ENDDO
      zmx = 0.
      zmn = 200.
      DO nnuc = 0, NNUct
         zmx = DMAX1(Z(nnuc),zmx)
         zmn = MIN(Z(nnuc),zmn)
      ENDDO
      DO k = 1, NMASSE
         nixz = izaf(k)/1000
         nixa = MOD(izaf(k),1000)
         iz = izaf(k)/1000
         ia = MOD(izaf(k),1000)
         IF (nixz.GE.zmn .AND. nixz.LE.zmx) THEN
            CALL WHERE(izaf(k),nnuc,iloc)
            IF (iloc.EQ.0) THEN
               SHC(nnuc) = emicx(k)
               IF (SHNix.EQ.0.D0) CALL SHELLC(A(nnuc),Z(nnuc),SHC(nnuc))
               DEF(1,nnuc) = beta2x(k)
               XMAss(nnuc) = EXCessmass(iz,ia)
               AMAss(nnuc) = (A(nnuc)*AMUmev + XMAss(nnuc))/AMUmev
            ENDIF
            IF (nixz.EQ.Z(0) .AND. nixa.EQ.A(0)) THEN
               SHC(0) = emicx(k)
               IF (SHNix.EQ.0.D0) CALL SHELLC(A(0),Z(0),SHC(0))
               DEF(1,0) = beta2x(k)
               XMAss(0) = EXCessmass(iz,ia)
            ENDIF
         ELSE
            DO ii = 0, NDEJC
               IF (nixz.EQ.ZEJc(ii) .AND. nixa.EQ.AEJc(ii)) THEN
                  DEFprj = beta2x(k)
                  XMAss_ej(ii) = EXCessmass(iz,ia)
               ENDIF
            ENDDO
         ENDIF
      ENDDO
C-----Fermi energies calculated for all nuclei and projectile combinations
      DO nnuc = 0, NNUct
         iztar = Z(nnuc)
         iatar = A(nnuc)
         DO ii = 0, NDEJC
            izpro = ZEJc(ii)
            iapro = AEJc(ii)
           if((iztar - izpro).ge.0 .and. (iatar - iapro).ge.0) then
              efermi = -0.5*(EXCessmass(iztar - izpro,iatar - iapro)
     &               - EXCessmass(iztar + izpro,iatar + iapro)
     &               + 2.*EXCessmass(izpro,iapro))
              EEFermi(ii,nnuc) = efermi
           endif
         ENDDO
      ENDDO
      RETURN
  300 WRITE (6,*) 'FATAL: File ../RIPL-2/masses/mass-frdm95.dat missing'
      STOP 'FATAL: File ../RIPL-2/masses/mass-frdm95.dat missing'
      END
C
C
C
      SUBROUTINE READLDP
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:iou*
Ccc   *                         R E A D L D P                            *
Ccc   *                                                                  *
Ccc   *     Reads level density parameter according to Mebel and         *
Ccc   *     the discrete level below which the decay scheme is complete  *
Ccc   *     as determined by Molnar-Belgya (see RIPL CRP) from file 24   *
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
Ccc   *               level density model read in from file ldp.dat,     *
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
      INTEGER INT
      pi2 = PI**2
      izamx = 0
      izamn = 200000
      DO nnuc = 0, NNUct
         izamx = MAX(IZA(nnuc),izamx)
         izamn = MIN(IZA(nnuc),izamn)
      ENDDO
      nexp = 0
      atilsum = 0.0
  100 READ (24,'(I7,I4,2F7.2,E10.4,E10.3)',END = 200) izar, nlevc,
     &      arogc, aroc, qn, dob
      IF (izar.GE.izamn .AND. izar.LE.izamx) THEN
         CALL WHERE(izar,nnuc,iloc)
         IF (iloc.EQ.0) THEN
            NLV(nnuc) = nlevc
            DOBs(nnuc) = dob
            if(D0_obs.GT.0.) DOBs(nnuc) = D0_obs
            a23 = A(nnuc)**0.666667
C-----------set up normalization factors for level density parameter 'a'
            IF (ROPaa(nnuc).EQ.( - 2.D0) .AND. arogc.NE.0.0D0) THEN
C--------------Gilbert-Cameron (no explicit collective effects)
               IF (ADIv.EQ.2.D0) THEN
                  del = 0.0
                  delp = 12.0/SQRT(A(nnuc))
                  IF (MOD(XN(nnuc),2.D0).EQ.0.D0) del = delp
                  IF (MOD(Z(nnuc),2.D0).EQ.0.D0) del = del + delp
                  uexc = qn - del
                  atil = 0.114*A(nnuc) + 9.80E-2*A(nnuc)**0.666667
                  gamma = 0.051
                  asys = atil*(1.0 + SHC(nnuc)
     &                   *(1.0 - EXP((-gamma*uexc)))/uexc)
                  IF (ATIlnor(nnuc).EQ.0.D0) ATIlnor(nnuc) = arogc/asys
C
C                 Added INITIALIZATION for ROPar(1,Nnuc) and ROPar(3,Nnuc)
C
                  ROPar(1,nnuc) = asys*ATIlnor(nnuc)
                  ROPar(3,nnuc) = del
               ENDIF
               IF (ADIv.EQ.0.0D0) THEN
                  del = 0.
                  delp = 12./SQRT(A(nnuc))
                  IF (MOD(XN(nnuc),2.D0).NE.0.0D0) del = delp
                  IF (MOD(Z(nnuc),2.D0).NE.0.0D0) del = del + delp
C-----------------EMPIRE systematics with Nix-Moeller shell corrections
                  ap1 = 0.94431E-01
                  ap2 = -0.80140E-01
                  gamma = 0.75594E-01
                  IF (Z(nnuc).GE.85.D0) THEN
                     ap1 = ap1*1.2402
                     ap2 = ap2*1.2402
                     gamma = gamma*1.2494
                  ENDIF
C-----------------EMPIRE systematics with M-S shell corrections
                  IF (SHNix.EQ.0.0D0) THEN
                     ap1 = .52268E-01
                     ap2 = .13395E+00
                     gamma = .93955E-01
                     IF (Z(nnuc).GE.85.D0) THEN
                        ap1 = ap1*1.2942
                        ap2 = ap2*1.2942
                        gamma = gamma*1.2928
                     ENDIF
                  ENDIF
                  atil = ap1*A(nnuc) + ap2*a23
                  tcrt = 0.567*delp
                  ar = atil*(1.0 + SHC(nnuc)*gamma)
                  DO ix = 1, 20
                     xr = ar*tcrt**2
                     acrt = atil*FSHELL(xr,SHC(nnuc),gamma)
                     IF (ABS(acrt - ar).LE.0.001D0*acrt) GOTO 105
                     ar = acrt
                  ENDDO
  105             econd = 1.5*acrt*delp**2/pi2
                                              !-del  !!!!!!!!!!!
                  uexc = qn + del - econd
                  asys = atil*(1.0 + SHC(nnuc)
     &                   *(1.0 - EXP((-gamma*uexc)))/uexc)
                  IF (ATIlnor(nnuc).EQ.0.0D0) ATIlnor(nnuc) = aroc/asys
C
C                 Added INITIALIZATION for ROPar(1,Nnuc) and ROPar(3,Nnuc)
C
                  ROPar(1,nnuc) = asys*ATIlnor(nnuc)
                  ROPar(3,nnuc) = del
               ENDIF
               atilsum = atilsum + ATIlnor(nnuc)
               nexp = nexp + 1
               IF (FITlev.GT.0.0D0) THEN
                  WRITE (6,*) ' '
                  WRITE (6,*) 'Nucleus A=', INT(A(nnuc)), ' Z=',
     &                        INT(Z(nnuc))
                  IF (ADIv.EQ.0.0D0 .OR. ADIv.EQ.3.0D0) WRITE (6,*)
     &                 'SHC=', SHC(nnuc), ' U=', uexc, ' DELTA=', del,
     &                ' asys=', asys, ' aexp=', aroc,' Dobs=',dob
                  IF (ADIv.EQ.2.0D0) WRITE (6,*) 'SHC=', SHC(nnuc),
     &                ' U=', uexc, ' DELTA=', del, ' asys=', asys,
     &                ' aexp=', arogc
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      GOTO 100
  200 IF (ROPaa(nnuc).EQ.( - 2.D0) .AND. nexp.GT.2) THEN
         atilave = atilsum/FLOAT(nexp)
         WRITE (6,*) ' '
         WRITE (6,*) 'Level density systematics normalized by factor ',
     &               atilave
         WRITE (6,*) ' '
      ELSE
         WRITE (6,*) ' '
         WRITE (6,*)
     &'Level density systematics NOT normalized to Dobs at neutron bindi
     &ng energy'
         WRITE (6,*) ' '
         atilave = 1.0
      ENDIF
      DO nnuc = 0, NNUct
         IF (ATIlnor(nnuc).EQ.0.0D0) ATIlnor(nnuc) = atilave
      ENDDO
      END
C
C
C
      SUBROUTINE SHELLC(Annuc,Znnuc,Shllc)
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
      CALL LYMASM(iz,ia,cmass,cbarr,nobarr)
C     CALL TZTN
C-----DL and DW are Cameron mass and shell correction respectively
C-----DL=DELCAM(FLOAT(IA),FLOAT(IZ))
C-----DW=TZ(IZ)+TN(NN)
      Shllc = SHLl
      END
C
C
C
      BLOCKDATA
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
      END
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
      SUBROUTINE LYMASM(Iz,Ia,Cmass,Cbarr,Nobarr)
C
C     WILLIAM D. MYERS - 6 JULY 1970
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
     &                 ampar, c, c2, c2d2, cay1, cay2, cay3, cay4, cay5,
     &                 cay6, coulmb, d, ee, em(10), emp(10), eps, exmt2,
     &                 ext2, f(2), ff, fuzsur, gamma, gl, oddev, parmas,
     &                 s, smalc, spw, sshell, sufnuc, sym, t, t2, test,
     &                 to, to2, tsq, un, v, volnuc, wotnuc, wterm, x,
     &                 xk(10), y(2), z, zsq, zt, ztt, zvt
      REAL FLOAT
      INTEGER i, ipq, j, n
      DOUBLE PRECISION XI, XIMOD
      DATA zvt/1.6666666666/
      DATA zt/.3333333333/
      DATA ztt/.6666666667/
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
C-----DMASS = REMAINDER AFTER CM - NO SHELL EFFECTS SUBTRACTED
C-----SHLL = CALCULATED SHELL EFFECT
C-----DIFMAS= DMASS - SHLL
C
      a1 = 15.4941
C-----IPARQ=0  PARAMETRS MYERS-SWIATECKI
      IF (IPArq.EQ.0) THEN
         a2 = 17.9439
         a3 = 0.7053
         gamma = 1.7826
      ELSEIF (IPArq.EQ.1) THEN
         a2 = 24.70
         a3 = 0.74476032
         gamma = 4.0
      ELSEIF (IPArq.EQ.2) THEN
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
      IF (Iz.EQ.0) THEN
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
C-----FOR DEFINITIONS OF CAY1 AND RZ,SEE UCRL-11980
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
      IF (x.LT.1.00) THEN
         IF (IPArq.EQ.0) BARr = sufnuc*XI(x)
         IF (IPArq.EQ.2) BARr = sufnuc*XI(x)
         IF (IPArq.EQ.3) BARr = sufnuc*XI(x)
         IF (IPArq.EQ.1) BARr = sufnuc*XIMOD(x)
      ELSE
         BARr = 0.0
      ENDIF
      y(1) = un
      y(2) = z
      DO j = 1, 2
         DO i = 1, 9
            IF (y(j) - em(i + 1).LE.0.D0) GOTO 50
         ENDDO
         PRINT 99005, j
99005    FORMAT ('1FAILURE IN LYMASS - Y(',I1,
     &           ') EXCEEDS LAST MAGIC NO.')
         STOP
   50    f(j) = xk(i)*(y(j) - em(i)) - .600*(y(j)**zvt - emp(i))
      ENDDO
      s = (2.00/a)**ztt*(f(1) + f(2)) - smalc*a3rt
      c2d2 = c2*d**2
      ee = (c2d2 + c2d2)*(1.00 - x)
      ff = .425917710*c2d2*d*(1.00 + x + x)/a3rt
      sshell = c*s
      v = sshell/ee
      eps = 1.500*ff/ee
      IF (ee*(1.00D0 - 3.00D0*v).GT.0.00D0) THEN
         SHLl = sshell
      ELSE
C--------ESTIMATE THETA
         to = 1.00
C--------ITERATE TO FIND EQUILIBRIUM THETA
  100    DO ipq = 1, 10
            to2 = to**2
            exmt2 = 1.E-20
            IF (ABS(to2).LT.30.D0) exmt2 = EXP(( - to2))
            t = to - (1.00 - eps*to - v*(3.00 - to2 - to2)*exmt2)
     &          /(( - eps) + v*to*(10.00 - 4.00*to2)*exmt2)
            IF (t.LE.0.00D0) GOTO 200
            IF (ABS(t - to).LT.1.D-4) GOTO 150
            to = t
         ENDDO
         GOTO 250
  150    t2 = t**2
         ext2 = 1.E-20
         IF (ABS(t2).LT.30.D0) ext2 = EXP(( - t2))
         test = ee*(1.00 - eps*(t + t) - v*((4.00*t2-12.00)*t2 + 3.00)
     &          *ext2)
         IF (test.GT.0.00D0) THEN
            t2 = t**2
            SHLl = t2*(ee - ff*t) + sshell*(1.00 - t2 - t2)*EXP(( - t2))
            GOTO 300
         ENDIF
  200    to = .100
         DO i = 1, 20
            to2 = to**2
            gl = ee*(1.00 - eps*to - v*(3.00 - to2 - to2)*EXP((-to2)))
            IF (gl.GT.0.00D0) GOTO 100
         ENDDO
  250    Cmass = SMAss
         Cbarr = 0.00
         Nobarr = 1
         RETURN
      ENDIF
  300 Cmass = SMAss + SHLl
      Cbarr = BARr - SHLl
      END
C
C
C
      DOUBLE PRECISION FUNCTION XI(Z)
C
C     6-POINT LAGRANGE INTERPOLATION
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
      DATA y/.25900, .255200, .250700, .245100, .2400, .23400, .228500,
     &     .22200, .21600, .2100, .20300, .196800, .1900, .18300,
     &     .175800, .1692400, .1620300, .1547800, .147500, .1401900,
     &     .1328400, .1254500, .1180100, .1105200, .1029600, .0953500,
     &     .0876800, .0799900, .0722900, .064600, .0569500, .0493700,
     &     .0419300, .0347600, .0281100, .0223600, .0176200, .0137300,
     &     .0105600, .0079800, .0059100, .0042500, .0029600, .0019700,
     &     .0012300, 7.1E-4, 3.6E-4, 1.5E-4, 4.E-5, 1.E-5, 0.00/
C
C-----THE X VALUES ARE EVENLY SPACED - X = 0(.02)1
C
      zbh = Z*50.00
      m = IFIX(SNGL(zbh))
      del = zbh - FLOAT(m)
      m = m + 1
      IF (m.GT.51) THEN
         m = 51
      ELSEIF (del.GE.1.D-4) THEN
         IF (m.LT.3) THEN
            del = del - FLOAT(3 - m)
            m = 3
         ELSEIF (m.GT.48) THEN
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
C
C
      DOUBLE PRECISION FUNCTION XIMOD(Z)
C
C     6-POINT LAGRANGE INTERPOLATION
C     IN MODIFIED LIQUID-DROP FORMULA
C     ( KRAPPE [ NIX --  IAEA-SM-174/12 )
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
C-----THE X VALUES ARE EVENLY SPACED - X = 0(.02)1
C
      zbh = Z*50.00
      m = IFIX(SNGL(zbh))
      del = zbh - FLOAT(m)
      m = m + 1
      IF (m.GT.51) THEN
         m = 51
      ELSEIF (del.GE.1.D-4) THEN
         IF (m.LT.3) THEN
            del = del - FLOAT(3 - m)
            m = 3
         ELSEIF (m.GT.48) THEN
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
C
C
      SUBROUTINE BNDG(Nejc,Nnuc,Bnd)
Ccc
Ccc   ******************************************************************
Ccc   *                                                       class:iou*
Ccc   *                         B N D G                                *
Ccc   *                                                                *
Ccc   *           Calculates binding energis                           *
Ccc   *                                                                *
Ccc   * input:NEJC, NNUC                                               *
Ccc   *                                                                *
Ccc   * output:BND                                                     *
Ccc   *                                                                *
Ccc   * calls:where                                                    *
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
      INTEGER ar, zr
      DOUBLE PRECISION b1, b2, b3
      zr = Z(Nnuc) - ZEJc(Nejc)
      ar = A(Nnuc) - AEJc(Nejc)
      b1 = A(Nnuc)*AMUmev + XMAss(Nnuc)
      b2 = ar*AMUmev + EXCessmass(zr,ar)
      b3 = AEJc(Nejc)*AMUmev + XMAss_ej(Nejc)
      Bnd = b2 + b3 - b1
      END
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
      CHARACTER*115 indexrec
      INTEGER nnuc
      CHARACTER*5 quantity(5)
      CHARACTER*10 reaction(2)
      CHARACTER*8 subent
C-----constant parameters for EXFOR retrieval
      DATA reaction/'N,F       ', 'N,TOT     '/
      DATA quantity/'CS   ', 'DAE  ', 'DA   ', 'DE   ', 'SP   '/
C-----open local file for storing retrieved EXFOR data
      OPEN (UNIT = 19,FILE = 'EXFOR.dat',STATUS = 'NEW',ERR = 99999)
C-----open EXFOR index
      OPEN (UNIT = 20,FILE = '../EXFOR/X4-INDEX.TXT',STATUS = 'OLD')
C
C-----scan EXFOR index for relevant subentries
C
  100 READ (20,'(A115)',END = 500) indexrec
      IF (indexrec(1:10).NE.TARget) GOTO 100
      IF (NCHr.EQ.2 .AND. indexrec(11:12).NE.PROjec(1:2)) GOTO 100
      IF (NCHr.EQ.4 .AND. indexrec(11:14).NE.PROjec(1:4)) GOTO 100
      DO nnuc = 1, NDNUC
         IF (indexrec(25:34).EQ.RESidue(nnuc)) GOTO 200
      ENDDO
      IF (indexrec(25:34).NE.'>NN-1     ') THEN
         IF (indexrec(11:20).NE.reaction(1)) THEN
            IF (indexrec(11:20).NE.reaction(2)) GOTO 100
         ENDIF
      ENDIF
  200 IF (indexrec(41:45).NE.quantity(1)) THEN
         IF (indexrec(41:45).NE.quantity(2)) THEN
            IF (indexrec(41:45).NE.quantity(3)) THEN
               IF (indexrec(41:45).NE.quantity(4)) THEN
                  IF (indexrec(41:45).NE.quantity(5)) GOTO 100
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
      IF (topname.NE.toplast) THEN
         OPEN (UNIT = 22,FILE = topname,ERR = 600,STATUS = 'OLD')
  250    READ (22,99005,END = 300) exforec
         WRITE (19,99005) exforec
         GOTO 250
      ENDIF
  300 toplast = topname
      CLOSE (22,ERR = 600)
      OPEN (UNIT = 22,FILE = filename,ERR = 600,STATUS = 'OLD')
  400 READ (22,99005,END = 100) exforec
      WRITE (19,99005) exforec
      GOTO 400
  500 CLOSE (19,ERR = 600)
      RETURN
  600 WRITE (6,*) 'Not found EXFOR subentry ', subent
      GOTO 100
99005 FORMAT (A80)
99999 END
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
Ccc   *        D_Xjlv(ND_NLV) - spin  of each proposed collective state  *
Ccc   *        D_DEF(ND_NLV) - dynamical deformations for each level     *
Ccc   *        DEFORMED - .TRUE. if nucleus has static deformation >0.15 *
Ccc   * return:                                                          *
Ccc   *   IFindColl = 0 (ALL POSSIBLE LEVELS FOUND)                      *
Ccc   *   IFindColl = 1 WARNING: (SOME COLLECTIVE LEVELS NOT FOUND)      *
Ccc   *   IFindColl = 2 ERROR: NO DISCRETE LEVEL INFORMATION AVAILABLE   *
Ccc   *                                                                  *
Ccc   * Creates files TARGET.LEV, COLLECTIVE.LEV and COLLECTIVE.TXT      *
Ccc   * TARGET_COLL.DAT   contains all collective  states to be          *
Ccc   *     considered in Coupled Channels and DWBA calculations         *
Ccc   *                                                                  *
Ccc   * TARGET.LEV       contains all discrete levels for the target     *
Ccc   *                                                                  *
Ccc   * calls:none                                                       *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'

      DOUBLE PRECISION drand
      Integer*4 indexf, indexb, buffer(250)
      Common/R250COM/indexf,indexb,buffer
C
C Local variables
C
      DOUBLE PRECISION beta2, beta3, betatmp, delta_k, dtmp, elvr,
     &                 etmp, ftmp, gspar, gspin, jtmp, qn, t12, xjlvr
      CHARACTER*1 dum
      CHARACTER*5 chelem
      CHARACTER*100 ch_iuf, comment
      CHARACTER*3 ctmp3
      CHARACTER*5 ctmp5
      DOUBLE PRECISION DBLE
      LOGICAL fexist,odd
      CHARACTER*9 finp
      INTEGER i, i0p, i10p, i12p, i1m, i20p, i21p, i22p, i31p, i3m,
     &        i41p, i4p, i5m, i6p, i8p, ia, iar, ierr, ilv, iptmp,
     &        itmp, itmp1, itmp2, iz, izr, j, lvpr, natmp, nbr, ndbrlin,
     &        ngamr, nlvr, nlvs, nmax, nnurec, nztmp, iccfus, ncont
      INTEGER NINT
      CHARACTER*6 reftmp

      ND_nlv = 0
      INQUIRE (FILE = 'TARGET_COLL.DAT',EXIST = fexist)
      IF (fexist) THEN
         WRITE (6,*) ' '
         WRITE (6,*) 'File with collective levels exists for the target'
         WRITE (6,*) '-------------------------------------------------'
         WRITE (6,*) ' '
         OPEN (UNIT = 32,FILE = 'TARGET_COLL.DAT',STATUS = 'OLD')
C--------Collective levels automatically selected, pls check
         READ (32,'(a100)') comment
         WRITE (6,'(a100)') comment
         WRITE (12,*)' Collective levels used in direct calcualtions'
C--------2nd line
         READ (32,'(a100)') comment
         WRITE (6,'(a100)') comment
C--------82 208    nucleus is treated as spherical
         READ (32,'(a100)') comment
         WRITE (6,'(a100)') comment
         WRITE (12,'(a100)') comment
C--------empty line
         READ (32,'(a100)') comment
         WRITE (6,'(a100)') comment
         WRITE (12,'(a100)') comment
C--------Ncoll Lmax  IDef (Def(1,j),j=2,IDef,2)
         READ (32,'(a100)') comment
         WRITE (6,'(a100)') comment
         WRITE (12,'(a100)') comment
         DEFormed = .FALSE.
         IF (ABS(DEF(1,0)).GT.0.1D0) DEFormed = .TRUE.
C
C        If odd nucleus, then rotational model is always used
C        It could be a bad approximation for a quasispherical nucleus
         IF(MOD(NINT(A(0)),2).NE.0 .OR. MOD(NINT(Z(0)),2).NE.0)
     &     DEFormed = .TRUE.

         IF (DEFormed) THEN
C-----------Number of collective levels
            READ (32,'(3x,3I5,1x,F5.1,1x,6(e10.3,1x))') ND_nlv, LMAxcc,
     &            IDEfcc, ftmp, (D_Def(1,j),j = 2,IDEfcc,2)
            WRITE (6,'(3x,3I5,1x,F5.1,1x,6(e10.3,1x))') ND_nlv, LMAxcc,
     &             IDEfcc, ftmp, (D_Def(1,j),j = 2,IDEfcc,2)
            WRITE (12,'(3x,3I5,1x,F5.1,1x,6(e10.3,1x))') ND_nlv, LMAxcc,
     &             IDEfcc, ftmp, (D_Def(1,j),j = 2,IDEfcc,2)
         ELSE
            READ (32,'(3x,3I5)') ND_nlv
            WRITE (6,'(3x,3I5)') ND_nlv
            WRITE (12,'(3x,3I5)') ND_nlv
         ENDIF
C--------if nd_nlv=0 , then no collective levels will be considered
C--------setting direct to zero
         IF (ND_nlv.EQ.0) THEN
            WRITE (6,*) ' WARNING: ND_NLV=0 in COLLECTIVE.LEV file'
            WRITE (6,*) ' WARNING: No collective levels considered'
            WRITE (6,*) ' WARNING: DIRECT has been set to 0'
C-----------setting direct to zerO
            DIRect = 0
            IFINDCOLL = 2
            RETURN
         ENDIF
C--------empty line
         READ (32,'(a100)') comment
         WRITE (6,'(a100)') comment
C--------'collective levels:'
         READ (32,'(a100)') comment
         WRITE (6,'(a100)') comment
         WRITE (12,*)' '
         WRITE (12,*)' N  E[MeV]   J  pi  Nph L  K  Dyn. Def.'
C--------Reading ground state infomation (to avoid overwriting deformation)
         READ (32,'(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),6e10.3)')
     &         ICOllev(1), D_Elv(1), D_Xjlv(1), D_Lvp(1), IPH(1),
     &         D_Llv(1), D_Klv(1)
         WRITE (6,'(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),6e10.3)')
     &          ICOllev(1), D_Elv(1), D_Xjlv(1), D_Lvp(1), IPH(1),
     &          D_Llv(1), D_Klv(1), 0.01
         WRITE (12,'(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),6e10.3)')
     &          ICOllev(1), D_Elv(1), D_Xjlv(1), D_Lvp(1), IPH(1),
     &          D_Llv(1), D_Klv(1), 0.01
         DO i = 2, ND_nlv
            READ (32,
     &          '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3,a5)')
     &          ICOllev(i), D_Elv(i), D_Xjlv(i), D_Lvp(i), IPH(i),
     &          D_Llv(i), D_Klv(i), D_Def(i,2), ctmp5
            WRITE (6,
     &          '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3,a5)')
     &          ICOllev(i), D_Elv(i), D_Xjlv(i), D_Lvp(i), IPH(i),
     &          D_Llv(i), D_Klv(i), D_Def(i,2), ctmp5
            itmp1 = ICOllev(i)
            if(itmp1.gt.LEVcc) itmp1 = itmp1 - LEVcc
            WRITE (12,
     &          '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3,a5)')
     &          itmp1, D_Elv(i), D_Xjlv(i), D_Lvp(i), IPH(i),
     &          D_Llv(i), D_Klv(i), D_Def(i,2), ctmp5
         ENDDO
         WRITE(12,*) ' '
         CLOSE (32)
         IFINDCOLL = 0
         RETURN
      ENDIF
      ia = A(0)
      iz = Z(0)
      DEFormed = .FALSE.
      IF (ABS(DEF(1,0)).GT.0.1) DEFormed = .TRUE.
C
      odd = .FALSE.
      IF(MOD(NINT(A(0)),2).NE.0 .OR. MOD(NINT(Z(0)),2).NE.0) THEN
C       If odd nucleus, then rotational model is always used
C       It could be a bad approximation for a quasispherical nucleus
        DEFormed = .TRUE.
        odd = .TRUE.
      ENDIF

      i20p = 0
      i21p = 0
      i0p = 0
      i4p = 0
      i6p = 0
      i8p = 0
      i10p = 0
      i12p = 0
      i1m = 0
      i3m = 0
      i5m = 0
      i22p = 0
      i41p = 0
      i31p = 0
      ND_nlv = 0
      ierr = 0
C
C-----constructing input and filenames
C
      IF (.NOT.FILevel) THEN
         WRITE (ctmp3,'(I3.3)') iz
         finp = 'z'//ctmp3//'.dat'
         OPEN (13,FILE = '../RIPL-2/levels/'//finp,STATUS = 'OLD',
     &         ERR = 600)
      ELSE
         REWIND (13)
      ENDIF
  100 READ (13,'(A5,6I5,2f12.6)',END = 600) chelem, iar, izr, nlvr,
     &      ngamr, nmax, itmp2, qn
      IF (ia.NE.iar .OR. iz.NE.izr) THEN
         DO ilv = 1, nlvr + ngamr
            READ (13,'(A1)') dum
         ENDDO
         GOTO 100
      ENDIF
      BACKSPACE (13)
      nlvs = 0
      OPEN (UNIT = 32,FILE = 'TARGET.LEV',STATUS = 'UNKNOWN')
      READ (13,'(A80)') ch_iuf
      WRITE (32,'(A80)') ch_iuf
      DO ilv = 1, nlvr
         READ (13,'(I3,1X,F10.6,1X,F5.1,I3,1X,E10.2,I3)') itmp, elvr,
     &         xjlvr, lvpr, t12, ndbrlin
C--------NLVs limited by binding energy
         IF (elvr.LT.qn) THEN
            nlvs = nlvs + 1
            WRITE (32,'(I3,1X,F10.6,1X,F5.1,I3,1X,E10.2,I3)') itmp,
     &             elvr, xjlvr, lvpr, t12, 0
         ENDIF
         DO nbr = 1, ndbrlin
            READ (13,'(A1)') dum
         ENDDO
      ENDDO
C
      DO ilv = 1, nlvr + ngamr
         BACKSPACE (13)
      ENDDO
      IF (.NOT.FILevel) CLOSE (13)
      REWIND (32)
      READ (32,'(A1)') dum
C-----levels for target NNUC copied to file TARGET.lev
      LMAxcc = 4
      IDEfcc = 4
      DO ilv = 1, NDCOLLEV
         DO j = 2, IDEfcc, 2
            D_Def(ilv,j) = 0
         ENDDO
      ENDDO

      iccfus = 1

      beta2 = 0.D0
      beta3 = 0.D0
      OPEN (84,FILE = '../RIPL-2/optical/om-data/om-deformations.dat',
     &      STATUS = 'old',ERR = 200)
      READ (84,'(///)')    ! Skipping first 4 title lines
      DO i = 1, 1700
         READ (84,'(2I4,4x,f10.6,1x,f4.1,i3,3x,f10.6,2x,a6)',END = 300,
     &         ERR = 300) nztmp, natmp, etmp, jtmp, iptmp, betatmp,
     &                    reftmp
         IF (nztmp.EQ.iz .AND. natmp.EQ.ia .AND. jtmp.EQ.2.D0 .AND.
     &       iptmp.EQ. + 1 .AND. reftmp.EQ.'Raman2') THEN
             beta2 = betatmp
c            CCFUS deformations
             BETcc(iccfus) = beta2
             FLAm(iccfus) = 2
             QCC(iccfus) = -etmp
             iccfus = iccfus + 1
         ENDIF
         IF (nztmp.EQ.iz .AND. natmp.EQ.ia .AND. jtmp.EQ.3.D0 .AND.
     &       iptmp.EQ. - 1 .AND. reftmp.EQ.'Kibedi') THEN
             beta3 = betatmp
c            CCFUS deformations
             BETcc(iccfus) = beta3
             FLAm(iccfus) = 3
             QCC(iccfus) = -etmp
             iccfus = iccfus + 1
         ENDIF
      ENDDO
      GOTO 300
  200 WRITE (6,*) ' WARNING: ',
     &   '../RIPL-2/optical/om-data/om-deformations.dat file not found '
C     WRITE (6,*) ' WARNING: ',
C    &       'Default dynamical deformations 0.15(2+) and 0.05(3-) used'
      GOTO 400
  300 CLOSE (84)
      IF (beta2.NE.0.D0 .OR. beta3.NE.0.D0) THEN
         WRITE (6,'(/1x,A34/1x,A11,F7.3,A13,F7.4)')
     &           'EXPERIMENTAL DEFORMATION (RIPL-2):', 'BETA (2+) =',
     &          beta2, '  BETA (3-) =', beta3
         IF (DEFormed) THEN
            WRITE (6,*) 'BETA2 ASSUMED AS GS BAND DEFORMATION'
            WRITE (6,*)
         ENDIF
      ENDIF
      IF (beta2.EQ.0.D0) THEN
         WRITE (6,*) ' WARNING: ',
     &    'E(2+) level is not contained in Raman 2001 database (RIPL-2)'
         IF(odd) then
            WRITE (6,*) ' WARNING: Odd nucleus, ',
     &            'FRDM deformation will be used for the gs band'
            WRITE (6,*) ' BETA2 = ',DEF(1,0)
            beta2 = DEF(1,0)
         ELSE
            WRITE (6,*) ' WARNING: FRDM deformation will be used'
            WRITE (6,*) ' BETA2 = ',DEF(1,0)
            beta2 = DEF(1,0)
         ENDIF
      ENDIF
      IF (beta3.EQ.0.D0) THEN
         WRITE (6,*) ' WARNING: ',
     &        'E(3-) level is not contained in Kibedi database (RIPL-2)'
         WRITE (6,*) ' WARNING: ',
     &            'Default dynamical deformations 0.05(3-) will be used'
         beta3 = 0.05
      ENDIF
      NScc = max(iccfus-1,NScc,0)

C-----Target corresponds to nnucrec = 0
C     CALL WHERE(IZA(1) - IZAejc(0),nnurec,iloc)
      nnurec = 0

  400 DO ilv = 1, nlvs
         READ (32,'(I3,1X,F10.6,1X,F5.1,I3,1X,E10.2,I3)') itmp, elvr,
     &         xjlvr, lvpr, t12, ndbrlin
C--------Skipping levels with unknown spin in the discrete level region
         IF (xjlvr.LT.0. .AND. ilv.LE.NLV(nnurec)) CYCLE
         IF (xjlvr.LT.0.) THEN ! unknown spin in continuum
C                                assigning randomly 2+,4+,3- spin
            ftmp = drand()
           xjlvr = 2
           lvpr  = 1
            if(ftmp.GT.0.3333d0 .AND. ftmp.LE.0.6666d0) THEN
             xjlvr = 4
             lvpr  = 1
            endif
            if(ftmp.GT.0.3333d0 .AND. ftmp.LE.0.6666d0) THEN
             xjlvr = 3
             lvpr  = -1
            endif
         ENDIF
         IF (ilv.EQ.1) THEN
            delta_k = 2.D0
            IF (xjlvr.NE.0.D0) delta_k = 1.D0
            ND_nlv = ND_nlv + 1
            ICOllev(ND_nlv) = ilv
            D_Elv(ND_nlv) = elvr
            D_Lvp(ND_nlv) = lvpr
            D_Xjlv(ND_nlv) = xjlvr
            IPH(ND_nlv) = 0
            D_Llv(ND_nlv) = 0
            D_Klv(ND_nlv) = 0
            D_Def(ND_nlv,2) = 0.01
            IF (beta2.GT.0.D0 .AND. DEFormed) D_Def(ND_nlv,2) = beta2
            gspin = xjlvr
            gspar = DBLE(lvpr)
         ENDIF
         IF (.NOT.(DEFormed)) THEN
C-----------spherical even-even nuclei follow
C-----------for spherical target taking dynamical deformation equal to RIPL-2 values.
C-----------If RIPL-2 deformation file not found OR target nucleus not found in
C-----------the database we take FRDM deformations
            IF (ilv.EQ.1) THEN
C--------------ground state deformation for spherical nucleus is 0.0
               D_Def(ND_nlv,1) = 0.0
               IF (gspin.NE.0.D0) THEN
                  ICOllev(ND_nlv) = ilv + LEVcc
                  WRITE (6,*)
     &                     'WARNING: ONLY DWBA CALCULATIONS ALLOWED FOR'
                  WRITE (6,*) 'WARNING: ODD SPHERICAL NUCLEUS'
                  WRITE (6,*) 'WARNING: DIRECT reset to zero'
                  DIRect = 0
               ENDIF
               GOTO 500
            ENDIF
            IF (i20p.EQ.0 .AND. xjlvr.EQ.2.D0 .AND. lvpr.EQ.1) THEN
               i20p = ilv
               ND_nlv = ND_nlv + 1
               ICOllev(ND_nlv) = ilv
               D_Elv(ND_nlv) = elvr
               D_Lvp(ND_nlv) = lvpr
               D_Xjlv(ND_nlv) = xjlvr
               IPH(ND_nlv) = 1
               D_Def(ND_nlv,2) = beta2
               GOTO 500
            ENDIF
            IF (i21p.EQ.0 .AND. xjlvr.EQ.2.D0 .AND. lvpr.EQ.1) THEN
               i21p = ilv
               ND_nlv = ND_nlv + 1
               ICOllev(ND_nlv) = ilv
               D_Elv(ND_nlv) = elvr
               D_Lvp(ND_nlv) = lvpr
               D_Xjlv(ND_nlv) = xjlvr
               IPH(ND_nlv) = 2
               D_Def(ND_nlv,2) = beta2
               GOTO 500
            ENDIF
            IF (i4p.EQ.0 .AND. xjlvr.EQ.4.D0 .AND. lvpr.EQ.1) THEN
               i4p = ilv
               ND_nlv = ND_nlv + 1
               ICOllev(ND_nlv) = ilv
               D_Elv(ND_nlv) = elvr
               D_Lvp(ND_nlv) = lvpr
               D_Xjlv(ND_nlv) = xjlvr
               IPH(ND_nlv) = 2
               D_Def(ND_nlv,2) = beta2
               GOTO 500
            ENDIF
            IF (i0p.EQ.0 .AND. xjlvr.EQ.0.D0 .AND. lvpr.EQ.1) THEN
               i0p = ilv
               ND_nlv = ND_nlv + 1
               ICOllev(ND_nlv) = ilv
               D_Elv(ND_nlv) = elvr
               D_Lvp(ND_nlv) = lvpr
               D_Xjlv(ND_nlv) = xjlvr
               IPH(ND_nlv) = 2
               D_Def(ND_nlv,2) = beta2
               GOTO 500
            ENDIF
            IF (i22p.EQ.0 .AND. xjlvr.EQ.2.D0 .AND. lvpr.EQ.1) THEN
               i22p = ilv
               ND_nlv = ND_nlv + 1
               ICOllev(ND_nlv) = ilv + LEVcc
               D_Elv(ND_nlv) = elvr
               D_Lvp(ND_nlv) = lvpr
               D_Xjlv(ND_nlv) = xjlvr
               IPH(ND_nlv) = 2
               D_Def(ND_nlv,2) = 0.05
               GOTO 500
            ENDIF
            IF (i3m.EQ.0 .AND. xjlvr.EQ.3.D0 .AND. lvpr.EQ. - 1) THEN
               i3m = ilv
               ND_nlv = ND_nlv + 1
               ICOllev(ND_nlv) = ilv
               D_Elv(ND_nlv) = elvr
               D_Lvp(ND_nlv) = lvpr
               D_Xjlv(ND_nlv) = xjlvr
               IPH(ND_nlv) = 1
               D_Def(ND_nlv,2) = beta3
               GOTO 500
            ENDIF
            IF (i41p.EQ.0 .AND. xjlvr.EQ.4.D0 .AND. lvpr.EQ.1) THEN
               i41p = ilv
               ND_nlv = ND_nlv + 1
               ICOllev(ND_nlv) = ilv + LEVcc
               D_Elv(ND_nlv) = elvr
               D_Lvp(ND_nlv) = lvpr
               D_Xjlv(ND_nlv) = xjlvr
               IPH(ND_nlv) = 2
               D_Def(ND_nlv,2) = 0.05
               GOTO 500
            ENDIF
            IF (i31p.EQ.0 .AND. xjlvr.EQ.3.D0 .AND. lvpr.EQ.1) THEN
               i31p = ilv
               ND_nlv = ND_nlv + 1
               ICOllev(ND_nlv) = ilv + LEVcc
               D_Elv(ND_nlv) = elvr
               D_Lvp(ND_nlv) = lvpr
               D_Xjlv(ND_nlv) = xjlvr
               IPH(ND_nlv) = 2
               D_Def(ND_nlv,2) = 0.05
               GOTO 500
            ENDIF
            IF (ECUtcoll.GT.0. .AND. elvr.GE.ECUtcoll) GOTO 600
C-----------Additional levels are added for DWBA calculations
            IF (ECUtcoll.GT.0. .AND. xjlvr.LE.JCUtcoll) THEN
               ND_nlv = ND_nlv + 1
               ICOllev(ND_nlv) = ilv + LEVcc
               D_Elv(ND_nlv) = elvr
               D_Lvp(ND_nlv) = lvpr
               D_Xjlv(ND_nlv) = xjlvr
               IPH(ND_nlv) = 1
               D_Def(ND_nlv,2) = 0.05
               ierr = 0
               IF (ND_nlv.NE.NDCOLLEV) GOTO 500
               GOTO 600
            ENDIF
            IF (i20p.NE.0 .AND. i3m.NE.0 .AND. i20p.NE.0 .AND.
     &          i21p.NE.0 .AND. i4p.NE.0 .AND. i3m.NE.0 .AND.
     &          i0p.NE.0 .AND. i22p.NE.0 .AND. i41p.NE.0 .AND.
     &          i31p.NE.0) THEN
               ierr = 0
               GOTO 600
            ENDIF
C-----------Deformed nuclei follow (beta2 = DEF(1, 0))
         ELSEIF (ilv.NE.1) THEN
            IF (i20p.EQ.0 .AND. xjlvr.EQ.(gspin + delta_k) .AND.
     &          lvpr.EQ.gspar) THEN
               i20p = ilv
               ND_nlv = ND_nlv + 1
               ICOllev(ND_nlv) = ilv
               D_Elv(ND_nlv) = elvr
               D_Lvp(ND_nlv) = lvpr
               D_Xjlv(ND_nlv) = xjlvr
               IPH(ND_nlv) = 0
               D_Def(ND_nlv,2) = 0.02
               GOTO 500
            ENDIF
            IF (i4p.EQ.0 .AND. xjlvr.EQ.(gspin + 2*delta_k) .AND.
     &          lvpr.EQ.gspar) THEN
               i4p = ilv
               ND_nlv = ND_nlv + 1
               ICOllev(ND_nlv) = ilv
               D_Elv(ND_nlv) = elvr
               D_Lvp(ND_nlv) = lvpr
               D_Xjlv(ND_nlv) = xjlvr
               IPH(ND_nlv) = 0
               D_Def(ND_nlv,2) = 0.02
               GOTO 500
            ENDIF
            IF (i6p.EQ.0 .AND. xjlvr.EQ.(gspin + 3*delta_k) .AND.
     &          lvpr.EQ.gspar) THEN
               i6p = ilv
               ND_nlv = ND_nlv + 1
               ICOllev(ND_nlv) = ilv
               D_Elv(ND_nlv) = elvr
               D_Lvp(ND_nlv) = lvpr
               D_Xjlv(ND_nlv) = xjlvr
               IPH(ND_nlv) = 0
               D_Def(ND_nlv,2) = 0.02
               IF(odd) goto 600
               GOTO 500
            ENDIF
            IF (i8p.EQ.0 .AND. xjlvr.EQ.(gspin + 4*delta_k) .AND.
     &          lvpr.EQ.gspar  .AND. .NOT.odd) THEN
               i8p = ilv
               ND_nlv = ND_nlv + 1
               ICOllev(ND_nlv) = ilv
               D_Elv(ND_nlv) = elvr
               D_Lvp(ND_nlv) = lvpr
               D_Xjlv(ND_nlv) = xjlvr
               IPH(ND_nlv) = 0
               D_Def(ND_nlv,2) = 0.02
               GOTO 500
            ENDIF
            IF (i10p.EQ.0 .AND. xjlvr.EQ.(gspin + 5*delta_k) .AND.
     &          lvpr.EQ.gspar  .AND. .NOT.odd) THEN
               i10p = ilv
               ND_nlv = ND_nlv + 1
               ICOllev(ND_nlv) = ilv + LEVcc
               D_Elv(ND_nlv) = elvr
               D_Lvp(ND_nlv) = lvpr
               D_Xjlv(ND_nlv) = xjlvr
               IPH(ND_nlv) = 0
               D_Def(ND_nlv,2) = beta2*0.25
               GOTO 500
            ENDIF
            IF (i12p.EQ.0 .AND. xjlvr.EQ.(gspin + 6*delta_k) .AND.
     &          lvpr.EQ.gspar  .AND. .NOT.odd) THEN
               i12p = ilv
               ND_nlv = ND_nlv + 1
               ICOllev(ND_nlv) = ilv + LEVcc
               D_Elv(ND_nlv) = elvr
               D_Lvp(ND_nlv) = lvpr
               D_Xjlv(ND_nlv) = xjlvr
               IPH(ND_nlv) = 0
               D_Def(ND_nlv,2) = beta2*0.25
               GOTO 500
            ENDIF
            IF (i0p.EQ.0 .AND. xjlvr.EQ.0.D0 .AND. lvpr.EQ.1
     &          .AND. .NOT.odd) THEN
               i0p = ilv
               ND_nlv = ND_nlv + 1
               ICOllev(ND_nlv) = ilv + LEVcc
               D_Elv(ND_nlv) = elvr
               D_Lvp(ND_nlv) = lvpr
               D_Xjlv(ND_nlv) = xjlvr
               IPH(ND_nlv) = 0
               D_Def(ND_nlv,2) = 0.02
               GOTO 500
            ENDIF
            IF (i1m.EQ.0 .AND. xjlvr.EQ.(gspin + NINT(delta_k)/2) .AND.
     &          lvpr.EQ. - 1*gspar  .AND. .NOT.odd ) THEN
               i1m = ilv
               ND_nlv = ND_nlv + 1
               ICOllev(ND_nlv) = ilv + LEVcc
               D_Elv(ND_nlv) = elvr
               D_Lvp(ND_nlv) = lvpr
               D_Xjlv(ND_nlv) = xjlvr
               IPH(ND_nlv) = 0
               D_Def(ND_nlv,2) = beta3
               GOTO 500
            ENDIF
            IF (i3m.EQ.0 .AND. lvpr.EQ. - 1*gspar .AND. .NOT.odd  .AND.
     &          xjlvr.EQ.(gspin + NINT(delta_k)/2 + delta_k)) THEN
               i3m = ilv
               ND_nlv = ND_nlv + 1
               ICOllev(ND_nlv) = ilv + LEVcc
               D_Elv(ND_nlv) = elvr
               D_Lvp(ND_nlv) = lvpr
               D_Xjlv(ND_nlv) = xjlvr
               IPH(ND_nlv) = 0
               D_Def(ND_nlv,2) = beta3
               GOTO 500
            ENDIF
            IF (i5m.EQ.0 .AND. lvpr.EQ. - 1*gspar .AND. .NOT.odd  .AND.
     &          xjlvr.EQ.(gspin + NINT(delta_k)/2 + 2*delta_k)) THEN
               i5m = ilv
               ND_nlv = ND_nlv + 1
               ICOllev(ND_nlv) = ilv + LEVcc
               D_Elv(ND_nlv) = elvr
               D_Lvp(ND_nlv) = lvpr
               D_Xjlv(ND_nlv) = xjlvr
               IPH(ND_nlv) = 0
               D_Def(ND_nlv,2) = beta3
               GOTO 500
            ENDIF
            IF (i21p.EQ.0 .AND. xjlvr.EQ.(gspin + delta_k) .AND.
     &          lvpr.EQ.gspar .AND. .NOT.odd) THEN
               i21p = ilv
               ND_nlv = ND_nlv + 1
               ICOllev(ND_nlv) = ilv + LEVcc
               D_Elv(ND_nlv) = elvr
               D_Lvp(ND_nlv) = lvpr
               D_Xjlv(ND_nlv) = xjlvr
               IPH(ND_nlv) = 0
               D_Def(ND_nlv,2) = 0.02
               GOTO 500
            ENDIF
            IF (i22p.EQ.0 .AND. xjlvr.EQ.(gspin + delta_k) .AND.
     &          lvpr.EQ.gspar .AND. .NOT.odd) THEN
               i22p = ilv
               ND_nlv = ND_nlv + 1
               ICOllev(ND_nlv) = ilv + LEVcc
               D_Elv(ND_nlv) = elvr
               D_Lvp(ND_nlv) = lvpr
               D_Xjlv(ND_nlv) = xjlvr
               IPH(ND_nlv) = 0
               D_Def(ND_nlv,2) = 0.02
               GOTO 500
            ENDIF
C--------------Additional levels are added for DWBA calculations
            IF (ECUtcoll.GT.0. .AND. elvr.GT.ECUtcoll) GOTO 600

            IF (ECUtcoll.GT.0. .AND. xjlvr.LE.JCUtcoll .AND.
     &                               .NOT.odd) THEN
               ND_nlv = ND_nlv + 1
               ICOllev(ND_nlv) = ilv + LEVcc
               D_Elv(ND_nlv) = elvr
               D_Lvp(ND_nlv) = lvpr
               D_Xjlv(ND_nlv) = xjlvr
               IPH(ND_nlv) = 0
               D_Def(ND_nlv,2) = 0.02
               ierr = 0
               IF (ND_nlv.NE.NDCOLLEV) GOTO 500
               GOTO 600
            ENDIF
            IF (ECUtcoll.GT.0. .AND. xjlvr.GT.JCUtcoll) cycle
            IF (i20p.NE.0 .AND. i4p.NE.0 .AND. i6p.NE.0 .AND.
     &          i8p.NE.0 .AND. i0p.NE.0 .AND. i1m.NE.0 .AND.
     &          i3m.NE.0 .AND. i5m.NE.0 .AND. i21p.NE.0 .AND.
     &          i22p.NE.0 .AND. i10p.NE.0 .AND. i12p.NE.0) THEN
               ierr = 0
               GOTO 600
            ENDIF
         ENDIF
  500 ENDDO
  600 IFINDCOLL = ierr
      CLOSE (32)
      OPEN (UNIT = 32,FILE = 'TARGET_COLL.DAT',STATUS = 'UNKNOWN')
      IF (.NOT.DEFormed) THEN
         WRITE (6,*)
         WRITE (6,99005)
     &' Collective levels selected automatically from available target l
     &evels (vibrational model)           '
         WRITE (6,*) 
     &          ' N <',LEVcc,' for coupled levels in CC calculation'
         WRITE (6,'(1x,i3,1x,i3,a35)') iz, ia,
     &                                ' nucleus is treated as spherical'
         WRITE (32,99005)
     &' Collective levels selected automatically from available target l
     &evels (vibrational model)'
         WRITE (32,*) 
     &          ' N <',LEVcc,' for coupled levels in CC calculation'
         WRITE (32,'(1x,i3,1x,i3,a35)') iz, ia,
     &                                ' nucleus is treated as spherical'
C--------Putting Coupled levels first
         DO i = 2, ND_nlv
            DO j = i + 1, ND_nlv
               IF (ICOllev(j).LT.ICOllev(i)) THEN
C-----------------swapping
                  itmp = IPH(i)
                  IPH(i) = IPH(j)
                  IPH(j) = itmp
                  dtmp = D_Def(i,2)
                  D_Def(i,2) = D_Def(j,2)
                  D_Def(j,2) = dtmp
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
C--------Putting one phonon coupled states first for spherical
         DO i = 2, ND_nlv
            IF (ICOllev(i).GE.LEVcc) GOTO 650
            DO j = i + 1, ND_nlv
               IF (ICOllev(j).GE.LEVcc) GOTO 620
               IF (IPH(j).LT.IPH(i)) THEN
C-----------------swapping
                  itmp = IPH(i)
                  IPH(i) = IPH(j)
                  IPH(j) = itmp
                  dtmp = D_Def(i,2)
                  D_Def(i,2) = D_Def(j,2)
                  D_Def(j,2) = dtmp
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
  620       ENDDO
  650    ENDDO
         WRITE (6,*)
         WRITE (6,*) '   Ncoll  '
         WRITE (6,'(3x,I5)') ND_nlv
         WRITE (6,*)
         WRITE (6,*) ' N   E[MeV]  J   pi Nph L  K  Dyn.Def.'
         WRITE (32,*)
         WRITE (32,*) '   Ncoll  '
         WRITE (32,'(3x,3I5)') ND_nlv
         WRITE (32,*)
         WRITE (32,*) ' N   E[MeV]  J   pi Nph L  K  Dyn.Def.'
         WRITE (12,*)
         WRITE (12,*) 'Collective levels used in direct calculations'
         WRITE (12,*)
         WRITE (12,*) '   Ncoll  '
         WRITE (12,'(3x,3I5)') ND_nlv
         WRITE (12,*)
         WRITE (12,*) ' N   E[MeV]  J   pi Nph L  K  Dyn.Def.'

          ncont = NLV(nnurec) + LEVcc
         DO i = 1, ND_nlv
            ftmp = D_Def(i,2)
            IF (i.EQ.1) ftmp = 0.0

            itmp1 = ICOllev(i)
            if(itmp1.gt.LEVcc) itmp1 = itmp1 - LEVcc
            IF (itmp1.LE.NLV(nnurec)) THEN
              WRITE (32,
     &           '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3)')
     &           ICOllev(i), D_Elv(i), D_Xjlv(i), D_Lvp(i), IPH(i),
     &           D_Llv(i), D_Klv(i), ftmp
              WRITE (12,
     &           '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3)')
     &           itmp1, D_Elv(i), D_Xjlv(i), D_Lvp(i), IPH(i),
     &           D_Llv(i), D_Klv(i), ftmp
            ELSE
              ncont = ncont + 1
               IF(ncont.GT.99) GOTO 653
              WRITE (32,
     &           '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3,a5)')
     &           ncont, D_Elv(i), D_Xjlv(i), D_Lvp(i), IPH(i),
     &           D_Llv(i), D_Klv(i), ftmp,' cont'
              WRITE (12,
     &           '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3,a5)')
     &           ncont, D_Elv(i), D_Xjlv(i), D_Lvp(i), IPH(i),
     &           D_Llv(i), D_Klv(i), ftmp,' cont'
            ENDIF
         ENDDO
 653     WRITE(12,*) ' '
      ELSE
         WRITE (32,99005)
     &' Collective levels selected automatically from available target l
     &evels (rigid rotor)       '
         WRITE (32,*)'Dyn.deformations are not used in symm.rot.model'
         WRITE (32,'(1x,i3,1x,i3,a35)') iz, ia,
     &                                 ' nucleus is treated as deformed'
         WRITE (6,*)
         WRITE (6,99005)
     &' Collective levels selected automatically from available target l
     &evels (rigid rotor)       '
         WRITE (6,*)'Dyn.deformations are not used in symm.rot.model'
         WRITE (6,'(1x,i3,1x,i3,a35)') iz, ia,
     &                                 ' nucleus is treated as deformed'
C--------Putting Coupled levels first
         DO i = 2, ND_nlv
            DO j = i + 1, ND_nlv
               IF (ICOllev(j).LT.ICOllev(i)) THEN
C-----------------swapping
                  itmp = IPH(i)
                  IPH(i) = IPH(j)
                  IPH(j) = itmp
                  dtmp = D_Def(i,2)
                  D_Def(i,2) = D_Def(j,2)
                  D_Def(j,2) = dtmp
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
         WRITE (6,*)
         WRITE (6,*) '   Ncoll  Lmax IDef  Kgs  (Def(1,j),j=2,IDef,2)'
         WRITE (6,'(3x,3I5,1x,F5.1,1x,6(e10.3,1x))') ND_nlv, LMAxcc,
     &          IDEfcc, D_Xjlv(1), (D_Def(1,j),j = 2,IDEfcc,2)
         WRITE (6,*)
         WRITE (6,*) ' N   E[MeV]  J   pi Nph L  K  Dyn.Def.'
         WRITE (32,*)
         WRITE (32,*) '   Ncoll  Lmax IDef  Kgs  (Def(1,j),j=2,IDef,2)'
         WRITE (32,'(3x,3I5,1x,F5.1,1x,6(e10.3,1x))') ND_nlv, LMAxcc,
     &          IDEfcc, D_Xjlv(1), (D_Def(1,j),j = 2,IDEfcc,2)
         WRITE (32,*)
         WRITE (32,*) ' N   E[MeV]  J   pi Nph L  K  Dyn.Def.'
         WRITE (12,*)
         WRITE (12,*) 'Collective levels used in direct calculations'
         WRITE (12,*)
         WRITE (12,*) '   Ncoll  '
         WRITE (12,'(3x,3I5)') ND_nlv
         WRITE (12,*)
         WRITE (12,*) ' N   E[MeV]  J   pi Nph L  K  Dyn.Def.'

          ncont = NLV(nnurec) + LEVcc

         DO i = 1, ND_nlv
            ftmp = D_Def(i,2)
            IF (i.EQ.1) ftmp = 0.01

            itmp1 = ICOllev(i)
            if(itmp1.gt.LEVcc) itmp1 = itmp1 - LEVcc
            IF (itmp1.LE.NLV(nnurec)) THEN
              WRITE (32,
     &           '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3)')
     &           ICOllev(i), D_Elv(i), D_Xjlv(i), D_Lvp(i), IPH(i),
     &           0, 0, ftmp
              WRITE (6,
     &           '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3)')
     &           ICOllev(i), D_Elv(i), D_Xjlv(i), D_Lvp(i), IPH(i),
     &           0, 0, ftmp
              WRITE (12,
     &           '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3)')
     &           itmp1, D_Elv(i), D_Xjlv(i), D_Lvp(i), IPH(i),
     &           0, 0, ftmp
            ELSE
              ncont = ncont + 1
               IF(ncont.GT.99) GOTO 99004
              WRITE (32,
     &           '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3,a5)')
     &           ncont, D_Elv(i), D_Xjlv(i), D_Lvp(i), IPH(i),
     &           D_Llv(i), D_Klv(i), ftmp,' cont'
              WRITE (6,
     &           '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3,a5)')
     &           ncont, D_Elv(i), D_Xjlv(i), D_Lvp(i), IPH(i),
     &           D_Llv(i), D_Klv(i), ftmp,' cont'
              WRITE (12,
     &           '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3,a5)')
     &           ncont, D_Elv(i), D_Xjlv(i), D_Lvp(i), IPH(i),
     &           D_Llv(i), D_Klv(i), ftmp,' cont'
            ENDIF
         ENDDO
99004    WRITE(12,*) ' '
      ENDIF
      CLOSE (32)
      RETURN
C-----target nucleus does not have discrete level information available
      ierr = 2
      IFINDCOLL = ierr
      RETURN
99005 FORMAT (A91)
      END
C
C
      SUBROUTINE FINDPOT(Ki,Ieof,Ipoten)
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
      Ieof = 0
      REWIND (Ki)
      READ (Ki,'(i5)') iref
      IF (Ipoten.EQ.iref) THEN
         BACKSPACE (Ki)
         RETURN
      ENDIF
  100 READ (Ki,99005,END = 200) ctmp
99005 FORMAT (A3)
      IF (ctmp.NE.'+++') GOTO 100
      READ (Ki,*,END = 200) iref
      IF (iref.NE.Ipoten) GOTO 100
      BACKSPACE (Ki)
      RETURN
  200 Ieof = 1
      END
C
C
C
      SUBROUTINE INPFIS(Nnuc)
C Creates fission.inp  which contains all the fission
C parameters independent of energy.
C
C-----fundamental fission barrier
C-----FISBAR=0 RIPL-2; HF-BCS theoretical heights for simple and double barriers,
C---- information about curvatures and wells provided by the code
C
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C
C COMMON variables
C
      DOUBLE PRECISION A2, A23, ACR, ACRt, ACRtf(2), AFIsm(NFMOD), AP1,
     &                 AP2, ATIl, BET2, BF, CSFism(NFMOD), DEFbm(NFMOD),
     &                 DEL, DELp, DELtafism(NFMOD), DEStepm(NFMOD),
     &                 DETcrt, DETcrtf(2), ECOnd, ECOndf(2), EFBm(NFMOD)
     &                 , EFDism(NFTRANS,NFMOD), GAMma, GAMmafism(NFMOD),
     &                 HM(NFTRANS,NFMOD), MORtcrt(NFPARAB),
     &                 MPArcrt(NFPARAB), ROFism(0:NFISENMAX,NDLW,NFMOD),
     &                 SCR, SCRtf(2), SHCfism(NFMOD), TCRt, TCRtf(2),
     &                 TDIrect, TDIrm(NFMOD), TFB, TFBm(NFMOD), UCRt,
     &                 UCRtf(2), UGRidf(0:NFISENMAX,NFMOD), WFIsm(NFMOD)
     &                 , XMInnm(NFMOD)
      INTEGER BFFm(NFMOD), NLWst, NRBinfism(NFMOD)
      COMMON /CRIT  / TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl,
     &                BET2
      COMMON /CRITFIS/ ACRtf, UCRtf, TCRtf, DETcrtf, SCRtf, MORtcrt,
     &                 MPArcrt, ECOndf
      COMMON /FISSMOD/ ROFism, HM, EFDism, UGRidf, EFBm, XMInnm, AFIsm,
     &                 DEFbm, SHCfism, DELtafism, GAMmafism, WFIsm,
     &                 BFFm, NRBinfism, DEStepm, TFBm, TDIrm, CSFism,
     &                 TFB, TDIrect
      COMMON /PARAM / AP1, AP2, GAMma, DEL, DELp, BF, A23, A2, NLWst
C
C Dummy arguments
C
      INTEGER Nnuc
C
C Local variables
C
      DOUBLE PRECISION ain, aout, bexp, bexp1, cin, cout, hin, hout
      CHARACTER*20 cara
      CHARACTER*33 cara1
      CHARACTER*55 cara2
      INTEGER i, ib, ibar, k, ka, kz, m, nr, nrbarc1, nrmod
      INTEGER INT
      CHARACTER*2 simb
      DO i = 1, 5
         EFB(i) = 0.
      ENDDO
      NRBar = 0
      NRWel = 0
C-----RIPLE-2 ETFSI values
      IF (FISbar(Nnuc).EQ.0.) THEN
         OPEN (51,FILE = '../RIPL-2/fission/fis-barrier-etfsi.dat',
     &         STATUS = 'OLD',ERR = 100)
         READ (51,*,END = 100)
         READ (51,*)
         READ (51,*)
         READ (51,*)
   50    READ (51,'(2i4,1x,a2,1x,10f9.2)',END = 100) kz, ka, simb, cin,
     &         hin, ain, EFB(1), bexp, cout, hout, aout, EFB(2), bexp1
         IF (kz.NE.INT(Z(Nnuc)) .OR. ka.NE.INT(A(Nnuc))) GOTO 50
         CLOSE (51)
         NRBar = 2
         IF (EFB(2).EQ.0.) THEN
            WRITE (6,*)
     &                 ' 2nd FISSION BARRIER ENERGY = 0 in RIPL2 etfsi '
            WRITE (6,*)
     &                 ' Setting the number of fission barriers equal 1'
            NRBar = 1
         ENDIF
         GOTO 300
  100    WRITE (6,*) ' NO THEORETICAL FISSION BARRIER FOR Z=',
     &               INT(Z(Nnuc)), ' A=', INT(A(Nnuc)), ' IN RIPL-2'
         WRITE (6,*) ' CHANGE FISBAR OPTION(NOW=0) '
         WRITE (6,*) ' OR IGNORE FISSION SETTING FISSHI=2'
         WRITE (6,*) ' EXECUTION TERMINATED'

         WRITE (6,*)
     &' FATAL: file ../RIPL-2/fission/fis-barrier-etfsi.dat may be missi
     &ng'

         STOP ' FATAL: Fission barrier can not be retrieved'

      ENDIF
C-----RIPL-2 "experimental" values
      IF (FISbar(Nnuc).EQ.2.) THEN
         OPEN (52,FILE = '../RIPL-2/fission/fis-barrier-exp.dat',
     &         STATUS = 'OLD',ERR = 200)
         READ (52,*,END = 200)
         READ (52,*)
         READ (52,*)
         READ (52,*)
  150    READ (52,'(2(1x,i3),3x,2(f8.3,4x),9x)',END = 200) kz, ka,
     &         EFB(1), EFB(2)

         IF (kz.NE.INT(Z(Nnuc)) .OR. ka.NE.INT(A(Nnuc))) GOTO 150
         NRBar = 2
         IF (EFB(1).EQ.0.) THEN
            EFB(1) = EFB(2)
            NRBar = 1
         ENDIF
         CLOSE (52)
         GOTO 300
  200    WRITE (6,*) ' NO EXPERIMENTAL FISSION BARRIER FOR Z=',
     &               INT(Z(Nnuc)), ' A=', INT(A(Nnuc)), ' IN RIPL-2'
         WRITE (6,*)
     &              ' CHANGE FISBAR OPTION(NOW=2). EXECUTION TERMINATED'

         WRITE (6,*)
     &     ' FATAL: file ../RIPL-2/fission/fis-barrier-exmay be missing'

         STOP ' FATAL: Fission barrier can not be retrieved'

      ENDIF
C
C-----default values for curvatures
  300 IF (NRBar.EQ.1) H(1,1) = 1.
C-----Lynn values
      IF (NRBar.EQ.2) THEN
         IF (ka/2.EQ.INT(ka/2) .AND. kz/2.EQ.INT(kz/2)) THEN   ! even-even
            H(1,1) = 1.04
            H(1,2) = 0.6
         ENDIF
         IF (ka/2.NE.INT(ka/2)) THEN                           ! odd A
            H(1,1) = 0.8
            H(1,2) = 0.52
         ENDIF
         IF (ka/2.EQ.INT(ka/2) .AND. kz/2.NE.INT(kz/2)) THEN   ! odd-odd
            H(1,1) = 0.65
            H(1,2) = 0.45
         ENDIF
C--------default values for the second well
         H(1,3) = 1.
         EFB(3) = 2.
      ENDIF
C
C-----FISBAR(Nnuc)=1. internal library
      IF (FISbar(Nnuc).EQ.1.) THEN
         OPEN (81,FILE = '../data/fisbar.dat',STATUS = 'OLD',ERR = 400)
  350    READ (81,*,END = 400) kz, ka, NRBar, NRWel,
     &                         (EFB(i),H(1,i),i = 1,NRBar)
         IF (kz.NE.INT(Z(Nnuc)) .OR. ka.NE.INT(A(Nnuc))) GOTO 350
         CLOSE (81)
         GOTO 500
  400    WRITE (6,*) ' NO  FISSION BARRIER FOR Z=', INT(Z(Nnuc)), ' A=',
     &               INT(A(Nnuc)), ' IN INTERNAL LIBRARY (fisbar.dat)'
         WRITE (6,*)
     &              ' CHANGE FISBAR OPTION(NOW=1). EXECUTION TERMINATED'

         WRITE (6,*) ' FATAL: May be file ../data/fisbar.dat missing'

         STOP ' FATAL: Fission barrier can not be retrieved'

      ENDIF
C-----Default value for curvatures and protection !!
  500 DO i = 1, NRBar
         IF (H(1,i).EQ.0) H(1,i) = 1.
      ENDDO
      IF (NRWel.NE.(NRBar - 1)/2) FISopt(Nnuc) = 0.
      NRBarc = NRBar - NRWel
      IF (FISmod(Nnuc).GT.0. .AND. NRBarc.NE.2.) THEN
         WRITE (6,*)
     &'Multi-modal fission calculated only for
     & double-humped barrier. FISMOD set to 0'
         FISmod(Nnuc) = 0.
      ENDIF
C
C----------------------input fundamental fission barrier *** done
C------- discrete barriers---------------------------------------
C
      DO ibar = 1, NRBar
         EFDis(1,ibar) = 0.
         IF (A(Nnuc)/2.EQ.INT(A(Nnuc)/2)) THEN
            SFDis(1,ibar) = 0.0
         ELSE
            SFDis(1,ibar) = 0.5
         ENDIF
         IF (XJLv(1,Nnuc).GE.0.) SFDis(1,ibar) = XJLv(1,Nnuc)
         IPFdis(1,ibar) = 1
         IF (LVP(1,Nnuc).NE.IPFdis(1,ibar)) IPFdis(1,ibar) = LVP(1,Nnuc)
         IF (FISdis(Nnuc).EQ.0.) NRFdis(ibar) = 1
      ENDDO
C
C---- the values below are not physical, they only enter the fission input
C---- file to help the user to edit and introduce his own values
C
      IF (FISdis(Nnuc).EQ.1.) THEN
         DO ib = 1, 3
            EFDis(2,ib) = 0.1
            EFDis(3,ib) = 0.2
            EFDis(4,ib) = 0.3
         ENDDO
         IF (A(Nnuc)/2.EQ.INT(A(Nnuc)/2)) THEN
            DO ibar = 1, NRBar
               NRFdis(ibar) = 4
               SFDis(2,ibar) = 0.
               IPFdis(2,ibar) = -1
               SFDis(3,ibar) = 2.
               IPFdis(3,ibar) = 1
               SFDis(4,ibar) = 4.
               IPFdis(4,ibar) = 1
            ENDDO
         ENDIF
         IF (A(Nnuc)/2.NE.INT(A(Nnuc)/2)) THEN
            DO ibar = 1, NRBar
               NRFdis(ibar) = 4
               SFDis(2,ibar) = 0.5
               IPFdis(2,ibar) = -1
               SFDis(3,ibar) = 1.5
               IPFdis(3,ibar) = 1
               SFDis(4,ibar) = 3.5
               IPFdis(4,ibar) = 1
            ENDDO
         ENDIF
      ENDIF
      DO ibar = 1, NRBar
         DO k = 1, NRFdis(ibar)
            H(k,ibar) = H(1,ibar)
         ENDDO
      ENDDO
      IF (FISmod(Nnuc).GT.0.) THEN
         nrmod = INT(FISmod(Nnuc)) + 1
         DO m = 1, nrmod
            EFBm(m) = EFB(2)
            DO k = 1, 4
               HM(k,m) = H(k,2)
               EFDism(k,m) = EFDis(k,2)
            ENDDO
         ENDDO
      ENDIF
      CALL DEFO_FIS(Nnuc)
      nrbarc1 = NRBarc
C
C================  level densities at saddles  ===============================
      IF (FISden(Nnuc).EQ.1.) THEN
C--------nuclear shape asymmetry at saddles
C           bff=1 axial, mass symmetry
C           bff=2 axial asymmetry,mass symmetry
C           bff=3 axial symmetry,mass asymmetry
C           bff=4 axial asymmetry,mass asymmetry
         BFF(1) = 1.
         IF (A(Nnuc).GT.236) BFF(1) = 2.
         BFF(2) = 3.
C--------shell corrections at saddles according to RIPL-2
         SHCfis(1) = 2.6
         IF (Z(Nnuc).GT.97.) SHCfis(1) = 2.6 - 0.1*(Z(Nnuc) - 97.)
         SHCfis(2) = 0.6 + 0.1*(Z(Nnuc) - 97.)
     &               + 0.04*(A(Nnuc) - Z(Nnuc) - 143.)
         IF (Z(Nnuc).GT.97.) SHCfis(2)
     &       = 0.6 + 0.04*(A(Nnuc) - Z(Nnuc) - 143.)
C--------pairing at saddles according to RIPL-2
         DELtafis(1) = 14./SQRT(A(Nnuc))
         DELtafis(2) = 14./SQRT(A(Nnuc))
C--------gamma in Ignatyuk's formula
         GAMmafis(1) = 2.113*ATIl*A(Nnuc)**( - 4./3.) !0.4*A(Nnuc)**( - 1./3.)
         GAMmafis(2) = 2.113*ATIl*A(Nnuc)**( - 4./3.) !0.4*A(Nnuc)**( - 1./3.)
C--------multiplier of atil
         AFIs(1) = 1.
         AFIs(2) = 1.
         IF (FISmod(Nnuc).GT.0.) THEN
            BFFm(1) = 3
            BFFm(2) = 3
            BFFm(3) = 3
            DO m = 1, nrmod
               SHCfism(m) = SHCfis(2)
               DELtafism(m) = DELtafis(2)
               GAMmafism(m) = GAMmafis(2)
               AFIsm(m) = AFIs(2)
            ENDDO
         ENDIF
      ENDIF
C-----h**2/2J
      IF (NRBarc.EQ.1) HJ(Nnuc,1) = 0.005
      IF (NRBar.EQ.2) THEN
         HJ(Nnuc,1) = 0.0050
         HJ(Nnuc,2) = 0.0025
      ENDIF
      IF (NRBar.EQ.3) THEN
         HJ(Nnuc,1) = 0.0050
         HJ(Nnuc,2) = 0.0025
         HJ(Nnuc,3) = 0.0035
      ENDIF
      IF (NRBar.EQ.5) THEN
         HJ(Nnuc,1) = 0.0050
         HJ(Nnuc,2) = 0.0025
         HJ(Nnuc,3) = 0.0017
         HJ(Nnuc,4) = 0.0035
         HJ(Nnuc,5) = 0.0020
      ENDIF
C---- writing data in FISSION.INP
      WRITE (79,'(a8)') 'Isotope:'
      WRITE (79,'(a40)') '----------------------------------------'
      WRITE (79,'(4x,a2,i3,2x,a2,i3)') 'Z=', INT(Z(Nnuc)), 'A=',
     &                                 INT(A(Nnuc))
      WRITE (79,'(a40)') '----------------------------------------'
      IF (FISbar(Nnuc).EQ.0.) cara = 'RIPL-2  Theor values'
      IF (FISbar(Nnuc).EQ.2.) cara = 'RIPL-1  Exp. values'
      IF (FISbar(Nnuc).EQ.1.) cara = 'Internal library'
      WRITE (79,'(a8,f2.0,a28,a20)') 'FISBAR =', FISbar(Nnuc),
     &                               '  Fundamental barriers from ',
     &                               cara
      WRITE (79,'(a15,i1,a15,i1)') ' Nr.parabolas =', NRBar,
     &                             '      Nr.wells=', NRWel
      WRITE (79,'(a8,f2.0)') 'FISMOD =', FISmod(Nnuc)
      WRITE (79,*) '  '
      IF (NRBar.EQ.1) THEN
         WRITE (79,'(a)') '    Va      ha    (in Mev) '
         WRITE (79,'(2f8.3)') EFB(1), H(1,1)
         WRITE (79,*) ' '
         WRITE (79,'(2a10)') 'h2/2J(A)', '(in MeV)'
         WRITE (79,'(f9.4)') HJ(Nnuc,1)
         WRITE (79,*) ' '
         WRITE (79,'(a10)') 'Beta2(A)'
         WRITE (79,'(f9.4)') DEFfis(1)
         WRITE (79,*) ' '
      ENDIF
      IF (NRBar.EQ.2) THEN
         IF (FISmod(Nnuc).EQ.0.) THEN
            WRITE (79,'(a)')
     &                    '    Va      ha      Vb      hb     (in Mev) '
            WRITE (79,'(4f8.3)') (EFB(i),H(1,i),i = 1,NRBar)
         ENDIF
         IF (FISmod(Nnuc).EQ.1.) THEN
            WRITE (79,'(a,1x,a)')
     &         '       Va      ha     Vb(SL)   hb(SL)   Vb(ST)   hb(ST)'
     &         , '  (in Mev) '
            WRITE (79,'(6f9.3,15x)') EFB(1), H(1,1), EFBm(1), HM(1,1),
     &                               EFBm(2), HM(1,2)
         ENDIF
         IF (FISmod(Nnuc).EQ.2.) THEN
            WRITE (79,'(a,1x,a)')
     &         '      Va      ha     Vb(SL)    hb(SL)  Vb(ST1)  hb(ST1)'
     &         , '  Vb(ST2)  hb(ST2)  (in Mev) '
            WRITE (79,'(8f9.3,15x)') EFB(1), H(1,1), EFBm(1), HM(1,1),
     &                               EFBm(2), HM(1,2), EFBm(3), HM(1,3)
         ENDIF
         WRITE (79,*) ' '
         WRITE (79,'(3a10)') 'h2/2J(A)', 'h2/2J(B)', '(in MeV)'
         WRITE (79,'(2f9.4)') (HJ(Nnuc,i),i = 1,NRBar)
         WRITE (79,*) ' '
         WRITE (79,'(2a10)') 'Beta2(A)', 'Beta2(B)'
         WRITE (79,'(2f9.4)') (DEFfis(i),i = 1,NRBar)
         WRITE (79,*) ' '
      ENDIF
      IF (NRBar.EQ.3) THEN
         IF (FISmod(Nnuc).EQ.0.) THEN
            WRITE (79,'(a,1x,a)')
     &       '    Va      ha      Vb      hb      Vi      hi  (in Mev) '
            WRITE (79,'(6f8.3,15x)') (EFB(i),H(1,i),i = 1,NRBar)
         ENDIF
         IF (FISmod(Nnuc).EQ.1.) THEN
            WRITE (79,'(a,1x,a)')
     &         '       Va      ha     Vb(SL)   hb(SL)   Vb(ST)   hb(ST)'
     &         , '    Vi      hi  (in Mev) '
            WRITE (79,'(8f9.3,15x)') EFB(1), H(1,1), EFBm(1), HM(1,1),
     &                               EFBm(2), HM(1,2), EFB(3), H(1,3)
         ENDIF
         IF (FISmod(Nnuc).EQ.2.) THEN
            WRITE (79,'(a,1x,a)')
     &         '      Va      ha     Vb(SL)    hb(SL)  Vb(ST1)  hb(ST1)'
     &         , '  Vb(ST2)  hb(ST2)   Vi      hi  (in Mev) '
            WRITE (79,'(10f9.3,15x)') EFB(1), H(1,1), EFBm(1), HM(1,1),
     &                                EFBm(2), HM(1,2), EFBm(3), HM(1,3)
     &                                , EFB(3), H(1,3)
         ENDIF
         WRITE (79,*) ' '
         WRITE (79,'(4a10)') 'h2/2J(A)', 'h2/2J(B)', 'h2/2J(I)',
     &                       '(in MeV)'
         WRITE (79,'(3f9.4)') (HJ(Nnuc,i),i = 1,NRBar)
         WRITE (79,*) ' '
         WRITE (79,'(3a10)') 'Beta2(A)', 'Beta2(B)', 'Beta2(I)'
         WRITE (79,'(3f9.4)') (DEFfis(i),i = 1,NRBar)
         WRITE (79,*) '  '
      ENDIF
      IF (NRBar.EQ.5) THEN
         WRITE (79,'(a,1x,a)')
     &'    Va      ha      Vb      hb      Vc       hc      Vi
     &hi      Vo      ho  (in Mev) '
         WRITE (79,'(10f8.3,15x)') (EFB(i),H(1,i),i = 1,NRBar)
         WRITE (79,*) ' '
         WRITE (79,'(6a10)') 'h2/2J(A)', 'h2/2J(B)', 'h2/2J(C)',
     &                       'h2/2J(I)', 'h2/2J(O)', '(in MeV)'
         WRITE (79,'(5f9.4)') (HJ(Nnuc,i),i = 1,NRBar)
         WRITE (79,*) ' '
         WRITE (79,'(6a10)') 'Beta2(A)', 'Beta2(B)', 'Beta2(C)',
     &                       'Beta2(I)', 'Beta2(O)', '        '
         WRITE (79,'(5f9.4)') (DEFfis(i),i = 1,NRBar)
         WRITE (79,*) ' '
      ENDIF
      IF (FISopt(Nnuc).EQ.0.) cara1 = ' Subbarrier effects neglected '
      IF (FISopt(Nnuc).GT.0.) cara1 = ' Subbarrier effects considered'
      WRITE (79,'(a8,f2.0,a36)') 'FISOPT=', FISopt(Nnuc), cara1
      WRITE (79,*) ' '
      IF (FISopt(Nnuc).GT.0.) THEN
         WRITE (79,*) '  '
         WRITE (79,'(a161)')
     &'Parabolic energy dependent imaginary potential in the isomeric we
     &ll Wimag = W0 + W1*E + W2*E^2, where E is excitation energy with r
     &espect to the isomeric well'
         WIMag(1) = 1.
         WIMag(2) = 0.
         WIMag(3) = 0.
         WRITE (79,*) '      W0         W1         W2'
         WRITE (79,'(3f11.4)') (WIMag(i),i = 1,3)
         WRITE (79,*)
      ENDIF
      DO ibar = 1, NRBar
         WRITE (79,'(a39,I2,a2,I2)')
     &                            'Number of discrete states at barrier'
     &                            , ibar, ' =', NRFdis(ibar)
         WRITE (79,*) 'Jdis  Pidis   Edis    homega'
         DO nr = 1, NRFdis(ibar)
            IF (FISmod(Nnuc).EQ.0. .OR.
     &          (FISmod(Nnuc).GT.0. .AND. ibar.NE.2))
     &           WRITE (79,'(1x, 1f3.1, 1x, 1i4, 2x, 1f8.3, 1x, 1f8.3)')
     &          SFDis(nr,ibar), IPFdis(nr,ibar), EFDis(nr,ibar),
     &          H(nr,ibar)
            IF (FISmod(Nnuc).EQ.1. .AND. ibar.EQ.2)
     &           WRITE (79,'(1x, 1f3.1, 1x, 1i4,1x, 4f9.3)')
     &          SFDis(nr,ibar), IPFdis(nr,ibar), EFDism(nr,1), HM(nr,1),
     &          EFDism(nr,2), HM(nr,2)
            IF (FISmod(Nnuc).EQ.2. .AND. ibar.EQ.2)
     &           WRITE (79,'(1x, 1f3.1, 1x, 1i4,1x, 6f9.3)')
     &          SFDis(nr,ibar), IPFdis(nr,ibar), EFDism(nr,1), HM(nr,1),
     &          EFDism(nr,2), HM(nr,2), EFDism(nr,3), HM(nr,3)
         ENDDO
      ENDDO
      WRITE (79,*) '  '
      IF (NRBarc.EQ.3) THEN
         WRITE (79,*) ' Veq(MeV)  Heq(MeV)  defeq'
         WRITE (79,'(3f9.3)') VEQ, HOEq, DEFeq
      ENDIF
      nrbarc1 = NRBarc
      IF (NRBarc.EQ.3) nrbarc1 = 2
      IF (FISden(Nnuc).EQ.0.) THEN
         cara2 =
     &          'Level densities at the saddle points taken from RIPL-2'
         WRITE (79,*)
         WRITE (79,'(a7,f2.0,a55)') 'FISDEN=', FISden(Nnuc), cara2
         WRITE (79,*)
      ENDIF
      IF (FISden(Nnuc).EQ.1.) THEN
         cara2 = 'Level densities at the saddle points EMPIRE specific'
         WRITE (79,*) '  '
         WRITE (79,'(a7,f2.0,a55)') 'FISDEN=', FISden(Nnuc), cara2
         WRITE (79,*) '  '
         WRITE (79,*)
     &'  Asymmetry  shell-corr  delta    gamma    atilf/atil at saddles'
         DO nr = 1, nrbarc1
            IF (FISmod(Nnuc).EQ.0. .OR.
     &          (FISmod(Nnuc).GT.0. .AND. nr.NE.2))
     &           WRITE (79,'(1x, A8, 1x, I1,4x,I1, 4f9.3)') 'Barrier',
     &          nr, BFF(nr), SHCfis(nr), DELtafis(nr), GAMmafis(nr),
     &          AFIs(nr)
            IF (FISmod(Nnuc).GT.0. .AND. nr.EQ.2) THEN
               DO m = 1, nrmod
                  WRITE (79,'(1x, A8, 1x, I1, 2x, I1, 1x, I1, 4f9.3)')
     &                    'Barrier', nr, m, BFFm(m), SHCfism(m),
     &                   DELtafism(m), GAMmafism(m), AFIsm(2)
               ENDDO
            ENDIF
         ENDDO
      ENDIF
C-----the coefficients of a linear energy dependent factor adjusting
C-----the fission level densities;
      DO nr = 1, nrbarc1
         ENH_ld(1,nr) = 1.
         ENH_ld(2,nr) = 0.
         ENH_ld(3,nr) = 0.
      ENDDO
      WRITE (79,*) '   '
      WRITE (79,'(a87)')
     &'Coefficients of a linear energy dependent factor adjusting the fi
     &ssion level densities'
      WRITE (79,'(a87)')
     &'(a0 + a1*E + a2*E**2) * rho(E,J) where E stands for excitation en
     &ergy above the barrier'
      WRITE (79,*) '                a0       a1       a2 '
      DO nr = 1, nrbarc1
         WRITE (79,'(1x, A8, 1x, I1, 3f9.3)') 'Barrier', nr,
     &          ENH_ld(1,nr), ENH_ld(2,nr), ENH_ld(3,nr)
      ENDDO
      END
C
C
      SUBROUTINE READ_INPFIS(Nnuc)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C COMMON variables
C
      DOUBLE PRECISION AFIsm(NFMOD), CSFism(NFMOD), DEFbm(NFMOD),
     &                 DELtafism(NFMOD), DEStepm(NFMOD), EFBm(NFMOD),
     &                 EFDism(NFTRANS,NFMOD), GAMmafism(NFMOD),
     &                 HM(NFTRANS,NFMOD), ROFism(0:NFISENMAX,NDLW,NFMOD)
     &                 , SHCfism(NFMOD), TDIrect, TDIrm(NFMOD), TFB,
     &                 TFBm(NFMOD), UGRidf(0:NFISENMAX,NFMOD),
     &                 WFIsm(NFMOD), XMInnm(NFMOD)
      INTEGER BFFm(NFMOD), NRBinfism(NFMOD)
      COMMON /FISSMOD/ ROFism, HM, EFDism, UGRidf, EFBm, XMInnm, AFIsm,
     &                 DEFbm, SHCfism, DELtafism, GAMmafism, WFIsm,
     &                 BFFm, NRBinfism, DEStepm, TFBm, TDIrm, CSFism,
     &                 TFB, TDIrect
C
C Dummy arguments
C
      INTEGER Nnuc
C
C Local variables
C
      CHARACTER*33 cara1
      CHARACTER*2 cara3, carz
      CHARACTER*8 cara8
      INTEGER i, ia, ibar, ibaro, iz, m, mm, nr, nrbarc1, nrmod
      INTEGER INT
      CHARACTER*40 line
      OPEN (79,FILE = 'FISSION.INP',STATUS = 'OLD')
  100 READ (79,'(A8)') cara8
      IF (cara8.NE.'Isotope:') GOTO 100
      READ (79,'(a40)') line
      READ (79,'(4x,a2,i3,2x,a2,i3)') carz, iz, cara3, ia
      IF (carz.EQ.'Z=' .AND. iz.NE.INT(Z(Nnuc)) .OR. ia.NE.INT(A(Nnuc)))
     &    GOTO 100
      READ (79,'(a40)') line
      READ (79,'(a8,f2.0,a28,a20)') cara8, FISbar(Nnuc)
      READ (79,'(15x,i1,15x,i1)') NRBar, NRWel
      READ (79,'(a8,f2.0)') cara8, FISmod(Nnuc)
      READ (79,*)
      READ (79,*)
      nrmod = INT(FISmod(Nnuc)) + 1
      IF (FISmod(Nnuc).EQ.0.) READ (79,*) (EFB(i),H(1,i),i = 1,NRBar)
      IF (FISmod(Nnuc).EQ.1. .AND. NRWel.EQ.1) READ (79,*) EFB(1),
     &    H(1,1), EFBm(1), HM(1,1), EFBm(2), HM(1,2), EFB(3), H(1,3)
      IF (FISmod(Nnuc).EQ.1. .AND. NRWel.EQ.0) READ (79,*) EFB(1),
     &    H(1,1), EFBm(1), HM(1,1), EFBm(2), HM(1,2)
      IF (FISmod(Nnuc).EQ.2. .AND. NRWel.EQ.1) READ (79,*) EFB(1),
     &    H(1,1), EFBm(1), HM(1,1), EFBm(2), HM(1,2), EFBm(3), HM(1,3),
     &    EFB(3), H(1,3)
      IF (FISmod(Nnuc).EQ.2. .AND. NRWel.EQ.0) READ (79,*) EFB(1),
     &    H(1,1), EFBm(1), HM(1,1), EFBm(2), HM(1,2), EFBm(3), HM(1,3)
      READ (79,*)
      READ (79,*)
      READ (79,*) (HJ(Nnuc,i),i = 1,NRBar)
      READ (79,*)
      READ (79,*)
      READ (79,*) (DEFfis(i),i = 1,NRBar)
      READ (79,*)
      READ (79,'(a8,f2.0,a36)') cara8, FISopt(Nnuc), cara1
      READ (79,*)
      IF (FISopt(Nnuc).GT.0.) THEN
         READ (79,*)
         READ (79,*)
         READ (79,*)
         READ (79,'(3f11.4)') (WIMag(i),i = 1,3)
         READ (79,*)
      ENDIF
      DO ibar = 1, NRBar
         READ (79,'(a39,I2,a2,I2)') line, ibaro, cara8, NRFdis(ibar)
         READ (79,*)
         DO nr = 1, NRFdis(ibar)
            IF (FISmod(Nnuc).EQ.0. .OR.
     &          (FISmod(Nnuc).GT.0. .AND. ibar.NE.2)) READ (79,*)
     &          SFDis(nr,ibar), IPFdis(nr,ibar), EFDis(nr,ibar),
     &          H(nr,ibar)
            IF (FISmod(Nnuc).EQ.1. .AND. ibar.EQ.2) READ (79,*)
     &          SFDis(nr,ibar), IPFdis(nr,ibar), EFDism(nr,1), HM(nr,1),
     &          EFDism(nr,2), HM(nr,2)
            IF (FISmod(Nnuc).EQ.2. .AND. ibar.EQ.2) READ (79,*)
     &          SFDis(nr,ibar), IPFdis(nr,ibar), EFDism(nr,1), HM(nr,1),
     &          EFDism(nr,2), HM(nr,2), EFDism(nr,3), HM(nr,3)
         ENDDO
      ENDDO
      READ (79,*)
      NRBarc = NRBar - NRWel
      IF (NRBarc.EQ.3) THEN
         READ (79,*)
         READ (79,'(3f9.3)') VEQ, HOEq, DEFeq
      ENDIF
      READ (79,*)
      nrbarc1 = NRBarc
      IF (NRBarc.EQ.3) nrbarc1 = 2
      READ (79,'(a7,f2.0)') cara8, FISden(Nnuc)
      READ (79,*)
      IF (FISden(Nnuc).EQ.1.) THEN
         READ (79,*)
         DO ibar = 1, nrbarc1
            IF (FISmod(Nnuc).EQ.0. .OR.
     &          (FISmod(Nnuc).GT.0. .AND. ibar.NE.2))
     &           READ (79,'(1x, A8, 1x, I1,4x, I1, 4f9.3)') cara8, i,
     &          BFF(ibar), SHCfis(ibar), DELtafis(ibar), GAMmafis(ibar),
     &          AFIs(ibar)
            IF (FISmod(Nnuc).GT.0. .AND. ibar.EQ.2) THEN
               DO m = 1, nrmod
                  READ (79,'(10x, I1, 2x, I1, 1x, I1, 4f9.3)') i, mm,
     &                  BFFm(m), SHCfism(m), DELtafism(m), GAMmafism(m),
     &                  AFIsm(m)
               ENDDO
            ENDIF
         ENDDO
      ENDIF
      READ (79,*)
      READ (79,*)
      READ (79,*)
      READ (79,*)
      DO nr = 1, nrbarc1
         READ (79,'(1x, A8, 1x, I1, 3f9.3)') cara8, i, ENH_ld(1,nr),
     &         ENH_ld(2,nr), ENH_ld(3,nr)
      ENDDO
      CLOSE (79)
      END
C
C
      SUBROUTINE DEFO_FIS(Nnuc)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C Dummy arguments
C
      INTEGER Nnuc
C
C Local variables
C
      DOUBLE PRECISION beta1, beta2, efbmin, ejoin(2*NFPARAB),
     &                 epsil(NFPARAB), ho(NFPARAB), rog, smiu, tdir,
     &                 tdir23, tf2, tf3, vjj(NFPARAB), vvqq
      INTEGER i, ibars, k, nr, nrbarm
C-----deformations at saddles and wells and matching points--------------------
      smiu = 0.1643167*A(Nnuc)**(5./6.)
      IF (NRBar.EQ.1) THEN
         DEFfis(1) = SQRT(EFB(1))/(smiu*H(1,1)) + DEF(1,Nnuc)
         GOTO 100
      ENDIF
      IF (NRBarc.EQ.2) THEN
         nrbarm = 3
         IF (NRWel.EQ.0) THEN
            EFB(3) = 2.
            H(1,3) = 1.
         ENDIF
         vjj(1) = EFB(1)
         vjj(2) = EFB(3)
         vjj(3) = EFB(2)
         ho(1) = H(1,1)
         ho(2) = H(1,3)
         ho(3) = H(1,2)
      ENDIF
      IF (NRBarc.EQ.3) THEN
         nrbarm = 5
         IF (NRWel.EQ.0) THEN
            EFB(4) = 2.
            H(1,4) = 1.
            EFB(5) = 5.
            H(1,5) = 1.2
         ENDIF
         IF (NRWel.EQ.2) THEN
            vjj(1) = EFB(1)
            vjj(2) = EFB(4)
            vjj(3) = EFB(2)
            vjj(4) = EFB(5)
            vjj(5) = EFB(3)
            ho(1) = H(1,1)
            ho(2) = H(1,4)
            ho(3) = H(1,2)
            ho(4) = H(1,5)
            ho(5) = H(1,3)
         ENDIF
      ENDIF
      DO i = 1, nrbarm
         epsil(i) = 0.
         ejoin(i) = 0.
         DEFfis(i) = 0.
      ENDDO
      epsil(1) = SQRT(vjj(1))/(smiu*ho(1)) + DEF(1,Nnuc)
      ejoin(2) = epsil(1)
     &           + SQRT((vjj(1) - vjj(2))/(1.D0 + (ho(1)/ho(2))**2))
     &           /(smiu*ho(1))
      ejoin(1) = 2*epsil(1) - ejoin(2)
      DO k = 2, nrbarm
         ejoin(2*k - 1) = ejoin(2*(k - 1))
         epsil(k) = ejoin(2*(k - 1)) + (ho(k - 1)/ho(k))
     &              **2*(ejoin(2*(k-1)) - epsil(k - 1))
         IF (k.LT.nrbarm) ejoin(2*k) = epsil(k)
     &                                 + SQRT(( - 1)**k*((vjj(k+1)
     &                                 -vjj(k)))
     &                                 /(1.D0 + (ho(k)/ho(k+1))**2))
     &                                 /(smiu*ho(k))
      ENDDO
      IF (NRBarc.EQ.2 .AND. NRWel.EQ.0) THEN
         DEFfis(1) = epsil(1)
         DEFfis(2) = epsil(3)
      ENDIF
      IF (NRBarc.EQ.2 .AND. NRWel.EQ.1) THEN
         DEFfis(1) = epsil(1)
         DEFfis(2) = epsil(3)
         DEFfis(3) = epsil(2)
      ENDIF
      IF (NRBarc.EQ.3) THEN
         DEFfis(1) = epsil(1)
         DEFfis(2) = epsil(3)
         DEFfis(3) = epsil(5)
         DEFfis(4) = epsil(2)
         DEFfis(5) = epsil(4)
      ENDIF
C-----parameters of the equivalent outer barrier
  100 DO ibars = 1, NRBarc
         XMInn(ibars) = 0.0001
         DO nr = 1, NRFdis(ibars)
            IF (EFDis(nr,ibars).GT.XMInn(ibars)) XMInn(ibars)
     &          = EFDis(nr,ibars)
         ENDDO
      ENDDO
      IF (NRBar.EQ.5) THEN
         vvqq = .4
         efbmin = MIN(EFB(2) + XMInn(2),EFB(3) + XMInn(3))
         tf2 = 1./(1. + EXP( - 2.*PI*(efbmin-vvqq-EFB(2)))/H(1,2))
         tf3 = 1./(1. + EXP( - 2.*PI*(efbmin-vvqq-EFB(3)))/H(1,3))
         VEQ = efbmin
         tdir = (1. - tf2)*(1. - tf3)
         tdir23 = tf2*tf3/(1. + SQRT(ABS(tdir)) + tdir)
         rog = LOG(1./tdir23 - 1.)
         HOEq = ABS(2*PI*((efbmin-vvqq-VEQ))/rog)
         beta1 = epsil(3) - SQRT(EFB(2))/(smiu*H(1,2))
         beta2 = epsil(5) + SQRT(EFB(3))/(smiu*H(1,3))
         DEFeq = (beta2 - beta1)/2. + epsil(2)
      ENDIF
      END
C
C
      SUBROUTINE GDRGFLDATA(Znucleus,Anucleus)
C
Ccc  ********************************************************************
Ccc  * Assignment of GGexp and D0exp for gamma-strength normalization   *
Ccc  * Assignment of the GDR and GFL model parameters                   *
Ccc  * to nucleus with atomic number 'Znucleus'and mass number'Anucleus'*
Ccc  *                                                                  *
Ccc  *  The parameters are put in the following COMMON's:               *
Ccc  *   -----------------------------------------------                *
Ccc  *                                                                  *
Ccc  *  COMMON /PARGDR/  EG1, GW1, CS1, EG2, GW2, CS2, NG               *
Ccc  *  COMMON /GFLPARAM/ BETagfl2, S2Plusgfl                           *
Ccc  *                                                                  *
Ccc  *   -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --                *
Ccc  *                                                                  *
Ccc  * 'COMMON/PARGDR/EG1,GW1,CS1,EG2,GW2,CS2,NG'  contains             *
Ccc  *  input for GDR parameters (all deformed nuclei are               *
Ccc  *  considered as  axially symmetric spheroids):                    *
Ccc  *                                                                  *
Ccc  *  EG1= peak energy of the first peak,                             *
Ccc  *  GW1= full width of the first peak at half-maximum,              *
Ccc  *  CS1= peak cross section of the first peak,                      *
Ccc  *  EG2= peak energy of the second peak,                            *
Ccc  *  CS2= peak cross section of the second peak,                     *
Ccc  *  GW2= full width of the second peak at half-maximum              *
Ccc  *  CS2= peak cross section of the second peak,                     *
Ccc  *  NG : for NG=1, single peak(spherical nucleus),                  *
Ccc  *       for NG=2, double peaks(deformed nucleus);                  *
Ccc  *                                                                  *
Ccc  * 'COMMON /GFLPARAM/ BETagfl2, S2Plusgfl'  contains                *
Ccc  *  the parameters of the GFL model:                                *
Ccc  *                                                                  *
Ccc  *  BETagfl2(=beta**2)= square of "deformation"                     *
Ccc  *                     parameter 'beta' associated                  *
Ccc  *                     with nuclear quadrupole moment,              *
Ccc  *  S2Plus(=(E2+)*beta**2)= product of first-excited                *
Ccc  *                          2+ state energy(in MeV)                 *
Ccc  *                          and square of the                       *
Ccc  *                          deformation parameter                   *
Ccc  *                                                                  *
Ccc  *  Setting the GDR parameters                                      *
Ccc  *  --------------------------                                      *
Ccc  *  Attempts are made  initially to set the GDR parameters          *
Ccc  *  from  "beijingn.dat" file (see RIPL).                           *
Ccc  *                                                                  *
Ccc  *  If GDR parameters are not found in "beijing.dat" they           *
Ccc  *  are calculated by global parametrization with effective         *
Ccc  *  quadrupole deformation parameters from "deflib.dat"file.        *
Ccc  *  These effective quadrupole deformation parameters were obtained *
Ccc  *  with  the use of  "Moller.dat" file of RIPL.                    *
Ccc  *                                                                  *
Ccc  *                                                                  *
Ccc  *  Setting parameters of GFL model                                 *
Ccc  *  -------------------------------                                 *
Ccc  *  GFL model -> S.F.Mughabghab,C.L.Dunford,Phys.Lett.B487(2000)155 *
Ccc  *                                                                  *
Ccc  *  Initially attemts are made to set parameter 'beta' and first-   *
Ccc  *  -excited state energy (E2+) from"def_eff.dat" file.This file is *
Ccc  *  prepared from data file "raman_tableI.txt" given by  S.Raman,   *
Ccc  *  C.W.Nestor,Jr, P.Tikkanen [Atom.Data Nucl.Data Tabl. 78(2001)1; *
Ccc  *  Table 1 for even-even nuclei].The value of deformation parameter*
Ccc  *  '|beta2|' from "deflib.dat"file is used for 'beta' if the 'beta'*
Ccc  *  is absent in the "def_eff.dat" file and  global parametrisation *
Ccc  *  for parameter 'S2Plus=(E2+)*beta**2' is used in this case.      *
Ccc  ********************************************************************
C
      IMPLICIT NONE
C
C PARAMETER definitions
C
      INTEGER MAXGDR
      PARAMETER (MAXGDR = 5986)
C
C COMMON variables
C
      DOUBLE PRECISION BETagfl2, CS1, CS2, EG1, EG2, GW1, GW2,
     &                 HALpha2(9000), HBEtagfl(700), HCS1(300),
     &                 HCS2(300), HE1(300), HE2(300), HENergygfl(700),
     &                 HGW1(300), HGW2(300), S2Plusgfl
      INTEGER KAA, KEYload, KEY_gdrgfl, KEY_shape, KZZ,
     &        NANa(9000), NANz(9000), NARam(700), NG, NNA(300), NNG(300)
     &        , NNZ(300), NUMram, NZRam(700)
      COMMON /GFLPARAM/ BETagfl2, S2Plusgfl
      COMMON /GSA   / KEY_shape, KEY_gdrgfl
      COMMON /MLOCOM2/ KEYload, KZZ, KAA
      COMMON /MLOCOM3/ NNZ, NNA, NNG, HE1, HCS1, HGW1, HE2, HCS2, HGW2,
     &                 NANz, NANa, HALpha2, HBEtagfl, HENergygfl, NZRam,
     &                 NARam, NUMram
      COMMON /PARGDR/ EG1, GW1, CS1, EG2, GW2, CS2, NG
C
C Dummy arguments
C
      DOUBLE PRECISION Anucleus, Znucleus
C
C Local variables
C
      DOUBLE PRECISION a0, a3, aann, alambda, alpha2, b0, betagfl, btmp,
     &                 cs0, csaa, csb, ea, eb, eg0, energygfl, etaeps,
     &                 etat(MAXGDR), etmp, fjtmp, gw0, he1t(MAXGDR),
     &                 he2t(MAXGDR), hgw1t(MAXGDR), hgw2t(MAXGDR), pi,
     &                 zz
      INTEGER i, ka, kz, n, natmp, nnat(MAXGDR), nngt(MAXGDR),
     &        nntmp, nnzt(MAXGDR)
      DATA pi/3.141592654D0/
      kz = Znucleus + 0.001
      ka = Anucleus + 0.001
      IF (kz.EQ.KZZ .AND. ka.EQ.KAA) RETURN
      KZZ = kz
      KAA = ka
      IF (KEYload.NE.1) THEN
         KEYload = 1
         OPEN (81,FILE = '../RIPL-2/gamma/gdr-parameters-exp.dat',
     &         STATUS = 'old',ERR = 450)
         READ (81,'(///)') ! Skipping first 4 title lines
         DO i = 1, 270
            READ (81,'(2I4, 1x,2x,3x, i3, 6F7.2)',END = 50,ERR = 50)
     &            NNZ(i), NNA(i), NNG(i), HE1(i), HCS1(i), HGW1(i),
     &            HE2(i), HCS2(i), HGW2(i)
         ENDDO
   50    CLOSE (81)
C        OPEN (81,FILE = '../RIPL-2/resonances/resonances0.dat',
C    &         STATUS = 'old',ERR = 450)
C        READ (81,'(///)') ! Skipping first 4 title lines
C        DO i = 1, 270
C           READ (81,'(2I4, 1x,2x,3x, i3, 6F7.2)',END = 50,ERR = 50)
C    &            NNZ(i), NNA(i), NNG(i), HE1(i), HCS1(i), HGW1(i),
C    &            HE2(i), HCS2(i), HGW2(i)
C        ENDDO
C  50    CLOSE (81)
  100    OPEN (81,FILE = '../RIPL-2/gamma/gdr-parameters-theor.dat',
     &         STATUS = 'old',ERR = 500)
         READ (81,'(///)') ! Skipping first 4 title lines
         DO i = 1, MAXGDR
            READ (81,'(2I4, 1x,2x, f7.3, 4F7.2)',END = 150,ERR = 150)
     &            nnzt(i), nnat(i), etat(i), he1t(i), hgw1t(i), he2t(i),
     &            hgw2t(i)
            nngt(i) = 2
            IF (he1t(i).EQ.he2t(i)) nngt(i) = 1
         ENDDO
  150    CLOSE (81)
  200    OPEN (82,FILE = '../data/deflib.dat',STATUS = 'old',ERR = 550)
         READ (82,'(////)') ! Skipping first 5 title lines
         DO i = 1, 9000
            READ (82,'((2I4, f7.3))',END = 250,ERR = 250) NANz(i),
     &            NANa(i), HALpha2(i)
         ENDDO
  250    CLOSE (82)
  300    OPEN (84,FILE = '../RIPL-2/optical/om-data/om-deformations.dat'
     &         ,STATUS = 'old',ERR = 600)
         READ (84,'(///)') ! Skipping first 4 title lines
         NUMram = 0
         DO i = 1, 700
            READ (84,'(2I4,4x,f10.6,1x,f4.1,6x,f10.6)',END = 400,
     &            ERR = 400) nntmp, natmp, etmp, fjtmp, btmp
C-----------Selecting only 2+ states
            IF (ABS(fjtmp - 2.D0).GT.0.0001) GOTO 350
            NUMram = NUMram + 1
            NZRam(NUMram) = nntmp
            NARam(NUMram) = natmp
            HENergygfl(NUMram) = etmp
            HBEtagfl(NUMram) = btmp
  350    ENDDO
         WRITE (6,*) ' RIPL-2 GDR parameters used'
         GOTO 700
  400    CLOSE (84)
  450    WRITE (6,'(1x,A14,A39,A43)') ' WARNING: File ',
     &                          '../RIPL-2/gamma/gdr-parameters-exp.dat'
     &                          ,
     &                     ' not found, theoretical RIPL-2 will be used'
         GOTO 100
  500    WRITE (6,'(1x,A14,A41,A35)') ' WARNING: File ',
     &                        '../RIPL-2/gamma/gdr-parameters-theor.dat'
     &                        ,
     &                     ' not found, default GDR values will be used'
         GOTO 200
  550    WRITE (6,'(1x,A14,A18,A43)')
     &                               ' WARNING: File ../data/deflib.dat'
     &                               ,
     &             ' not found, default deformation values will be used'
         GOTO 300
  600    WRITE (6,'(1x,A14,A45,A54)') ' WARNING: File ',
     &                   '../RIPL-2/optical/om-data/om-deformations.dat'
     &                   ,
     &           ' not found, default dynamical deformation values used'
      ENDIF
  700 n = ka - kz
      zz = kz
      aann = ka
      a3 = aann**0.3333333
      eg0 = 31.2/a3 + 20.6/SQRT(a3)
      gw0 = 0.026*eg0**1.91
      cs0 = 1.2*120.*n*zz/(aann*pi*gw0)
      IF (KEY_gdrgfl.NE.0) THEN
         DO i = 1, 270
            IF (kz.EQ.NNZ(i) .AND. ka.EQ.NNA(i)) THEN
               NG = NNG(i)
               EG1 = HE1(i)
               CS1 = HCS1(i)
               GW1 = HGW1(i)
               EG2 = HE2(i)
               CS2 = HCS2(i)
               GW2 = HGW2(i)
C--------------Plujko_new-2005
               IF(Key_shape.NE.5) RETURN
               GOTO 900
            ENDIF
         ENDDO
C-----Plujko_new-2005
      ENDIF

C-----Plujko_new-2005
C     If experimantal values of GDR parameters not found and Key_GDRGFL=2
C     they are going to be retrieved from the RIPL-2 Goriely theoretical
C     values. Note that in accordance with Goriely data-file all nuclei are
C     ELONGATED!?
      IF (Key_GDRGFL.EQ.2)THEN
         DO i = 1, MAXGDR
            IF (kz.EQ.nnzt(i) .AND. ka.EQ.nnat(i)) THEN
               NG = nngt(i)
               EG1 = he1t(i)
               CS1 = cs0
               GW1 = hgw1t(i)
               EG2 = he2t(i)
               CS2 = 0.D0
               GW2 = hgw2t(i)
               etaeps = etat(i)
               IF (ABS(etaeps - 1.D0).GT.0.0001) THEN
C--------------Global GDR parameterization for deformed nuclei
C--------------(classical sum rule with correction)
                  CS1 = cs0/3.
                  CS2 = cs0*2./3.
               ENDIF
C--------------Plujko_new-2005
               IF(Key_shape.NE.5)RETURN
               GOTO 900
            ENDIF
         ENDDO
      ENDIF
C-----Setting the deformation parameter from "deflib.dat" file
C-----for calculation of the GDR energies and widths
      DO i = 1, 9000
         IF (kz.EQ.NANz(i) .AND. ka.EQ.NANa(i)) THEN
            alpha2 = HALpha2(i)
            GOTO 800
         ENDIF
      ENDDO
      alpha2 = 0.
  800 IF (ABS(alpha2).GT.0.001) THEN
C--------Global GDR parameterization for deformed nuclei
C--------( classical sum rule with correction)
         NG = 2
         alambda = (1. + 0.6*alpha2**2 + 2.*alpha2**3/35.)**0.3333333
         a0 = (1. + alpha2)/alambda
         b0 = (1. - 0.5*alpha2)/alambda
         eb = eg0*(1. - 1.51E-02*(a0 + b0)*(a0 - b0))/b0
         ea = eb/(0.911*a0/b0 + 0.089)
         csaa = cs0/3.
         csb = cs0*2./3.
         EG1 = ea
         EG2 = eb
         CS1 = csaa
         CS2 = csb
         IF (ea.GT.eb) THEN
            EG1 = eb
            EG2 = ea
            CS1 = csb
            CS2 = csaa
         ENDIF
         GW1 = 0.026*EG1**1.91
         GW2 = 0.026*EG2**1.91
      ELSE
C--------Global GDR parameterization for spherical targets
C--------( classical sum rule with correction)
         NG = 1
         EG1 = eg0
         GW1 = gw0
         CS1 = cs0
         EG2 = 0.
         GW2 = 0.
         CS2 = 0.
      ENDIF
C-----Plujko_new-2005
      IF(Key_shape.NE.5)RETURN
C-----Setting the GFL parameters '|beta|' from "defeff.dat"
C-----and 'S2Plus=(E2+)*beta**2'
  900 DO i = 1, NUMram
         IF (kz.EQ.NZRam(i) .AND. ka.EQ.NARam(i)) THEN
            IF (HBEtagfl(i).GT.0.) THEN
               betagfl = HBEtagfl(i)
C--------------'BETagfl2=beta**2' and  'S2Plus=(E2+)*beta**2'  -------
C--------------parameters of the GFL model[BETagfl2=beta; ENErgygfl=E2+(MeV)]
               BETagfl2 = betagfl**2
C--------------energygfl = henergygfl(i)*0.001
               energygfl = HENergygfl(i)
C--------------RIPL-2 energies in MeV, RCN 06/2004
               S2Plusgfl = BETagfl2*energygfl
C--------------Plujko_new-2005
               RETURN
            ENDIF
         ENDIF
      ENDDO
C-----Global parametrization for 'S2Plus=(E2+)*beta**2'
C-----and setting the '|beta2|' from "deflib.dat" file
C-----as 'beta' of the GFL model.
C-----Setting the deformation parameter from "deflib.dat" file
C-----for  calculation  of  the  GFL model parameter
      DO i = 1, 9000
         IF (kz.EQ.NANz(i) .AND. ka.EQ.NANa(i)) THEN
            alpha2 = HALpha2(i)
            GOTO 1000
         ENDIF
      ENDDO
      alpha2 = 0.
 1000 betagfl = 1.5853*ABS(alpha2)
      BETagfl2 = betagfl**2
      S2Plusgfl = 217.156/aann**2
      END

C R250.F77     The R250 Pseudo-random number generator
C
C algorithm from:
C Kirkpatrick, S., and E. Stoll, 1981; A Very Fast Shift-Register
C Sequence Random Number Generator, Journal of Computational Physics,
C V. 40. p. 517
C
C see also:
C Maier, W.L., 1991; A Fast Pseudo Random Number Generator,
C                    Dr. Dobb's Journal, May, pp. 152 - 157
C
C
C Uses the Linear Congruential Method,
C the "minimal standard generator"
C Park & Miller, 1988, Comm of the ACM, 31(10), pp. 1192-1201
C for initialization
C
C
C For a review of BOTH of these generators, see:
C Carter, E.F, 1994; Generation and Application of Random Numbers,
C Forth Dimensions, Vol. XVI, Numbers 1,2 May/June, July/August
C
C ===================================================================
C
      Function lcmrand(ix)
C     The minimal standard PRNG for 31 bit unsigned integers
C     designed with automatic overflow protection
C     uses ix as the seed value if it is greater than zero
C     otherwise it is ignored
      Integer*4 ix
      Integer*4 a, b, m, q, r
      Integer*4 hi, lo, test
      Integer*4 x
      SAVE x
      Parameter (a = 16807, b = 0, m = 2147483647)
      Parameter (q = 127773, r = 2836)
C
      If ( ix .gt. 0 ) x = ix

      hi = x / q
      lo = mod( x, q )
      test = a * lo - r * hi
      if ( test .gt. 0 ) then
          x = test
      else
          x = test + m
      endif

      lcmrand = x
      return
      End


C ===================================================================
C
C  R250, call R250Init with the desired initial seed BEFORE
C  the first invocation of IRAND()
C
C ===================================================================

      Subroutine R250Init(iseed)
      Integer*4 k, mask, msb
      Integer*4 indexf, indexb, buffer(250)
      Common/R250COM/indexf,indexb,buffer
      Integer ms_bit, all_bits, half_range, step

      DATA ms_bit/Z'40000000'/
      DATA half_range/Z'20000000'/
      DATA all_bits/Z'7FFFFFFF'/

      Parameter ( step = 7 )
C
      indexf = 1
      indexb = 104
      k = iseed
      Do i = 1, 250
        buffer(i) = lcmrand( k )
        k = -1
      EndDo
      Do i = 1, 250
       if ( lcmrand( -1 ) .gt. half_range ) then
           buffer(i) = ior( buffer(i), ms_bit )
       endif
      EndDo

      msb = ms_bit
      mask = all_bits

      Do i = 0,30
        k = step * i + 4
        buffer(k) = iand( buffer(k), mask )
        buffer(k) = ior( buffer(k), msb )
        msb = msb / 2
        mask = mask / 2
      EndDo

      Return
      END

      Function drand()
C     R250 PRNG, run after R250_Init
      Integer*4 newrand
      Integer*4 indexf, indexb, buffer(250)
      REAL*8 drand, m
      Parameter (m = 2147483648.D0)
      Common/R250COM/indexf,indexb,buffer

      newrand = ieor( buffer(indexf), buffer(indexb) )
      buffer(indexf) = newrand

      indexf = indexf + 1
      if ( indexf .gt. 250 ) indexf = 1

      indexb = indexb + 1
      if ( indexb .gt. 250 ) indexb = 1

      drand = dble(newrand)/m

      return
      End

      Function grand()
C     Generator of normally distributed random numbers based on R250
      Integer*4 newrand
      Integer*4 indexf, indexb, buffer(250)
      Integer i
      REAL*8 grand, m
      Parameter (m = 2147483648.D0)
      Common/R250COM/indexf,indexb,buffer

      grand = -6.d0
       do i=1,12
        newrand = ieor( buffer(indexf), buffer(indexb) )
        buffer(indexf) = newrand

        indexf = indexf + 1
        if ( indexf .gt. 250 ) indexf = 1

        indexb = indexb + 1
        if ( indexb .gt. 250 ) indexb = 1

        grand = grand + dble(newrand)/m
       enddo

      return
      End
