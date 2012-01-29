Ccc   * $Rev: 2373 $
Ccc   * $Author: rcapote $
Ccc   * $Date: 2012-01-30 00:07:47 +0100 (Mo, 30 Jän 2012) $
      SUBROUTINE INPUT
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:iou*
Ccc   *                         I N P U T                                *
Ccc   *                                                                  *
Ccc   *     Sets default values of input parameters, READs mandatory     *
Ccc   *     input and calls READIN for optional input READing.           *
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
      INTEGER KAA, KAA1, KEYinput, KEYload, KZZ, KZZ1

      CHARACTER*120 nubar_filename
      INTEGER*4 len_nubar_filename
      CHARACTER*24 EMPireos
      INTEGER*4 INDexf, INDexb, BUFfer(250)
      COMMON /MLOCOM1/ KEYinput, KZZ1, KAA1
      COMMON /MLOCOM2/ KEYload, KZZ, KAA
      COMMON /R250COM/ INDexf,INDexb,BUFfer
C
C Local variables
C
      DOUBLE PRECISION aclu, ak2, ampi0, ampipm, ares, atmp, da,
     &         deln(150), delp, delz(98), e2p, e3m, emaxr, qmin,
     &         qtmp, xfis, zclu, zres, ztmp, culbar, e2pej, e3mej
      CHARACTER*1 cnejec
      DOUBLE PRECISION DATAN, DMAX1, DSQRT
      CHARACTER*2 deut, gamma, trit, he3, cnejec2
      REAL FLOAT
      LOGICAL gexist, fexist, calc_fiss
      INTEGER i, ia, iac, iae, iccerr, iend, ierr, ietl, iia, iloc, in,
     &        ip, irec, itmp, iz, izares, izatmp, j, lpar, na, nejc,
     &        netl, nnuc, nnur, mulem, nucmin, hh, irepeated 
      INTEGER IFINDCOLL,IFINDCOLL_CCFUS
      INTEGER INT, ISEED, NINT
      CHARACTER*2 SMAT
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
      DATA deut, trit, gamma, he3/'d ', 't ', 'g ','h '/
C-----maximum argument of EXP function supported by the computer (for real*8)
      EXPmax = 700.
C-----maximum exponent of 10 supported by the computer (for real*8)
      EXPdec = 300.
      CALL CLEAR
      DO nnuc = 1, NDNUC
         EMAx(nnuc) = 0.0
         ECUt(nnuc) = 0.0
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
C-----The above value is the one used also in the ENDF-6 manual (April 2001, 2009)
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
      AMUele = 0.00054857990945D0
C
C mn    neutron mass  1.008 664 915 78 amu 
C me    electron mass 5.485 799 110 �10-4 amu 
C mp    proton mass   1.007 276 466 88 amu 
C md    deuteron mass 2.013 553 212 71 amu 1
C mt    triton mass   3.015 500 713 amu 3
C m3He  3He mass      3.014 932 234 69 amu 1
C ma    4He mass      4.001 506 1747 amu 1
C
      CETa = ELE2*DSQRT(AMUmev/2.D0)/HHBarc
      CSO = (HHBarc/AMPi)**2
      PI = 4.D0*DATAN(1.D0)

      INQUIRE(file='R250SEED.DAT',exist=fexist)
      if(fexist) then
        OPEN(94,file='R250SEED.DAT',status='OLD')
        READ(94,*)  indexf, indexb
        Do i = 1, 250
          READ(94,*) buffer(i)
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
C--------select Myers-Swiatecki shell corrections
         SHNix = 0.0
C--------neutralize tuning factors and OMP normalization factors
         DO nejc = 0, NDEJC
           TUNEpe(nejc) = 1.d0
           rTUNEpe(nejc) = 1.d0
         ENDDO
         DO nnuc = 0, NDNUC
            TUNefi(nnuc) = 1.d0
            rTUNefi(nnuc) = 1.d0
            SHLlnor(nnuc) = 1.d0
C
C           This is the default number of smoothing points to process
C           HFB numerical fission barriers. It is hardwired, not possible to change 
C           without recompiling the EMPIRE sources. 
C
            NRSmooth(nnuc) = 5
            DO j = 1, NDLV  
               ISIsom(j,nnuc) = 0
            ENDDO
            DO nejc = 0, NDEJC
               TUNe(nejc,nnuc) = 1.d0
              rTUNe(nejc,nnuc) = 1.d0
C--------------Volume real potential
               FNvvomp(Nejc,Nnuc) = 1.d0
               FNrvomp(Nejc,Nnuc) = 1.d0
               FNavomp(Nejc,Nnuc) = 1.d0
C--------------Volume imaginary potential
               FNwvomp(Nejc,Nnuc) = 1.d0
               FNrwvomp(Nejc,Nnuc) = 1.d0
C--------------Surface imaginary potential:
               FNwsomp(Nejc,Nnuc) = 1.d0
               FNrsomp(Nejc,Nnuc) = 1.d0
               FNasomp(Nejc,Nnuc) = 1.d0
            ENDDO
         ENDDO
C--------Set TUNe for gammas in the CN to 0.999 so that 1.0 can be used
C--------to turn off normalization to experimental Gg
         TUNe(0,1) = 0.999d0
         DO nnuc = 1, NDNUC
            IZA(nnuc) = 0
C-----------set level density parameters
            ROPaa(nnuc) = -2.0
            ROPar(1,nnuc) = 0.
            ROPar(2,nnuc) = 0.
            ROPar(4,nnuc) = 0.
            ROPar(5,nnuc) = 0.
            ATIlnor(nnuc) = 0.
            LDShif(Nnuc) = 0.d0
            ROHfba(nnuc)  = -20.d0  ! default to allow for zero value
            ROHfbp(nnuc)  = -20.d0  ! default to allow for zero value
            GTIlnor(nnuc) = 1.
            LVP(1,nnuc) = 1
C
            om2_ig(nnuc)   = 0.d0
            delp_ig(nnuc)  = 0.d0
            atil_ig(nnuc)  = 0.d0
            dshift_ig(nnuc)= 0.d0           
C-----------set ENDF flag to 0 (no ENDF formatting)
            ENDf(nnuc) = 0
C-----------set ENDFA flag to 0 (no exclusive angular distributions)
            ENDfa(nnuc) = 0
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
            DO hh=1,NFHump
               FISv_n(hh,nnuc)=1.0
               FISh_n(hh,nnuc)=1.0
               FISa_n(hh,nnuc)=1.0
               FISd_n(hh,nnuc)=1.0
               FISn_n(hh,nnuc)=1.0
            ENDDO           
         ENDDO
C
         IZA(0) = 0
         LVP(1,0) = 1
         NNUcd = 0
         NEJcm = 0
         DEFga = 0.
         DEFgw = 10.
         DEFgp = 40.
         ADIv = 0.0
         NEX(1) = 60
         NEXreq = 60
         FITlev = 0.0
C--------Full gamma cascade becomes the default setting  (Jan 2011)
C--------Use GCASC input parameter to turn it off
         GCAsc =  1.0
C--------fission barrier multiplier, viscosity, and spin fade-out
         QFIs = 1.0
         BETav = 4.0           ! viscosity parameter
         SHRj = 24.0
         SHRd = 2.5            ! diffuness of the shell correction damping
C--------fusion parameters
         CSRead = -2.0
         SIG = 0.0
         TRUnc = 2.0
         EXPush = 0.
         CRL = 0.0
         DFUs = 1.
         FUSred = 1.d0
         FCCred = 1.d0
         TOTred = 1.d0
         ELAred = 1.d0
         rFUSred = 1.d0
         rFCCred = 1.d0
         rTOTred = 1.d0
         rELAred = 1.d0
         LEVtarg = 1
C
C--------Capote, additional input options
C
         DIRect = -1
         KTRompcc = 0
         SOFt = .FALSE.
         CCCalc = .FALSE.
C       
C
         NUBarread = .FALSE.

         IOPSYS = 0 !   LINUX - Default
         CALL GETENV ('OS', empireos)
         if(empireos(1:3). eq. 'Win') IOPsys = 1 ! Windows
C--------Mode of EXFOR retrieval
C        IX4ret = 0 no EXFOR retrieval
C        IX4ret = 1 copy C4 file
         IX4ret = 1

         NPRIm_g = 0       ! No primary gammas (default)
C
C        PFNS keywords
C        
         FISspe = 0
         PFNtke = 1.d0
         PFNalp = 1.d0
         PFNrat = 1.d0
         PFNniu = 1.d0
C
         IOMwritecc = 0
         MODelecis = 0
         EXClusiv = .TRUE. ! Default: All exclusive calculation
         WIDcoll = 0.05d0  ! Default = 50 keV resolution  
         DXSred = 1.d0     ! scaling factor for direct processes in deuteron induced reactions
         DEFdyn = 1.d0
         DEFsta = 1.d0
         DEFnuc = 0.d0
         RECoil = 1.d0     ! Default 
         TISomer = 1.d0    ! 1 sec. default threshold for being isomer
C        IOPran = 1 ! Default gaussian 1 sigma error
         IOPran = 0 ! MC sampling off by default, 'RANDOM' turns it on
C        IOPran = -1 ! Uniform 1 sigma error
C--------Relativistic kinematics
         RELkin = .FALSE.
C--------Maximum energy to assume all levels are collective for DWBA calculations
C--------        Default value 0. i.e. none but those selected automatically
         ECUtcoll = 0.
         JCUtcoll = 4
C--------set fission defaults
         DO nnuc = 1, NDNUC
           FISbar(nnuc) = 1     ! RIPL-3 empirical fission barriers 
C          FISbar(nnuc) = 3     ! RIPL-3 HFB barriers
C
           FISden(nnuc) = 0     ! EGSM NLD at saddle points 
C          FISden(nnuc) = 3     ! HFB NLD
C
           FISmod(nnuc) = 0     ! Single-modal fission
           FISopt(nnuc) = 0
           FISDIS(nnuc) = 0     ! no discrete transition states except fundamental
         ENDDO
C
C--------CCFUS parameters
         DV = 10.
         FCC = 1.
         NACc = 0
C        By default only target deformation is considered
C        NSCc = 2
         NSCc = 4
C        Default deformation values, they are changed in ifindcoll(),ifindcoll_CCFUS()
         BETcc(1) = 0.15d0
         BETcc(2) = 0.05d0
         BETcc(3) = 0.15d0
         BETcc(4) = 0.05d0
         FLAm(1) = 2.d0
         FLAm(2) = 3.d0
         FLAm(3) = -2.d0
         FLAm(4) = -3.d0
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
         FHMs = 0  ! Exciton densities default (Fermi gas densities FHMs->1 )
         CHMs = 1.0  ! mult. default damp rate (for eval work to scale preq)
C
C--------default input parameters for MSD
C
         MSD = 0
         EMInmsd = 5. !Emin for starting MSD calculations
C--------ICOmpff must be off for DOM potentials
         ICOmpff = 0  !compressional form factor off
C        ICOmpff = 1  !compressional form factor on
C
C--------default input parameters for Heidelberg MSC
C
         MSC = 0
C--------STMro selects p-h densities: 0 for closed form, 1 for microscopic
         STMro = 0.0
C--------set single particle level density parameter default in PE models as A/13.
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
C--------set options for PCROSS (exciton preequilibrium + cluster emission)
         PEQcont = 0.0
         PEQc = 0.0
         NPAirpe = 1  ! default is to include pairing corrections in PCROSS 
         MFPp = 1.3
         CHMax = 0.d0 ! default set to 0.54 inside PCROSS
C--------HRTW control (0 no HRTW, 1 HRTW up to EHRtw MeV incident)
         LHRtw = 0
         EHRtw = 0.d0
C--------ENDF global setting initialized to zero (no formatting)
         NENdf = 0
         NENdfa = 0
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

         IF (NDEjc.LT.6) THEN
           WRITE (8,*) ' '
           WRITE (8,*)
     >    ' WARNING: this version of EMPIRE is prepared to emit complex'
           WRITE (8,*)
     >    ' WARNING: particles, NDEjc must be >= 6 in dimension.h '
          STOP 'You have to increase NDEjc in dimension.h (set NDEjc=6)'
         ENDIF

C--------ejectile deuteron
         AEJc(4) = 2.
         ZEJc(4) = 1.
         XNEjc(4) = AEJc(4) - ZEJc(4)
         IZAejc(4) = INT(1000.*ZEJc(4) + AEJc(4))
         iz = INT(ZEJc(4))
         SYMbe(4) = SMAT(iz)
         SEJc(4) = 1.0
C--------ejectile triton
         AEJc(5) = 3.
         ZEJc(5) = 1.
         XNEjc(5) = AEJc(5) - ZEJc(5)
         IZAejc(5) = INT(1000.*ZEJc(5) + AEJc(5))
         iz = INT(ZEJc(5))
         SYMbe(5) = SMAT(iz)
         SEJc(5) = 0.5
C--------ejectile he-3
         AEJc(6) = 3.
         ZEJc(6) = 2.
         XNEjc(6) = AEJc(6) - ZEJc(6)
         IZAejc(6) = INT(1000.*ZEJc(6) + AEJc(6))
         iz = INT(ZEJc(6))
         SYMbe(6) = SMAT(iz)
         SEJc(6) = 0.5
C
C        Default values for keys (Key_shape, Key_GDRGFL) to set
C        shape of E1 strength function and GDR parameters
         KEY_shape = 1   ! MLO1 RIPL-2
         KEY_gdrgfl = 1  ! GDR parameters of RIPL introduced
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
C--------READ entire input for the first energy calculations
C--------mandatory part of the input
C--------incident energy (in LAB)
         READ (5,*) EIN
C        Starting value of the number of angular points
         NANgela = 91
         NDAng   = 91

         IF(NANgela.GT.NDAngecis) THEN
           WRITE(8,*)
     &        'ERROR: INCREASE NANgecis IN dimension.h UP TO ',NANgela
           STOP 'FATAL: INCREASE NANgecis IN dimension.h'
         ENDIF
C--------set angles for inelastic calculations
         da = 180.0/(NDAng - 1)
         DO na = 1, NDAng
           ANGles(na) = (na - 1)*da
         ENDDO
         DO na = 1, NDAng
           CANgler(na) = COS(ANGles(NDAng - na + 1)*PI/180.)
           SANgler(na) = SQRT(1.D0 - CANgler(na)**2)
         ENDDO
C--------target
         READ (5,*) A(0), Z(0)
         IF (A(0).LT.Z(0)) THEN
            WRITE (8,*) 'ERROR: Z > A, please correct input file'
            STOP 'FATAL: Z > A, please correct input file'
         ENDIF

C--------projectile
         READ (5,*) AEJc(0), ZEJc(0)
         IF (AEJc(0).EQ.0 .AND. ZEJc(0).EQ.0) THEN
C-----------GAMMA EMISSION
            SEJc(0) = 1
            lpar = -1
            e2pej = 0.d0
            e3mej = 0.d0
         ELSE
            CALL PTLEVSET(AEJc(0),ZEJc(0),SEJc(0),lpar,e2pej,e3mej)
         ENDIF

         CALL PTLEVSET(A(0),Z(0),XJLv(LEVtarg,0),LVP(LEVtarg,0),e2p,e3m)

         XN(0) = A(0) - Z(0)
         IZA(0) = INT(1000*Z(0) + A(0))

         ENDF(0) = 1

         ia = INT(A(0))
         iz = INT(Z(0))
         SYMb(0) = SMAT(iz)
         NLV(0) = 1
         ELV(1,0) = 0.0
         QCC(1) = -e2p
         QCC(2) = -e3m

         ECUtcoll = 3*30./A(0)**0.666666666666d0

C--------product of target and projectile parities
C        LVP(LEVtarg,0) = LVP(LEVtarg,0)*lpar
C        RCN, We assume is only the target parity !!!
         XNEjc(0) = AEJc(0) - ZEJc(0)
         IZAejc(0) = INT(1000.*ZEJc(0) + AEJc(0))
         iz = INT(ZEJc(0))
         SYMbe(0) = SMAT(iz)
         QCC(3) = -e2pej
         QCC(4) = -e3mej
C--------***** done ********
C
C--------NEMN  number of neutrons emitted
         READ (5,*) nemn
C--------NEMP  number of protons  emitted
         READ (5,*) nemp
C--------NEMA  number of alphas   emitted
         READ (5,*) nema
C--------NEMC  number of deuterons emitted
         READ (5,*) nemd
C--------NEMC  number of tritons   emitted
         READ (5,*) nemt
C--------NEMC  number of he3       emitted
         READ (5,*) nemh
C--------NEMC  number of clusters emitted
         READ (5,*) NEMc, aclu, zclu
         IF (NEMc.GT.0 .and. NDEjc.EQ.6) THEN
           WRITE (8,*) ' '
           WRITE (8,*) ' WARNING: TO EMIT CLUSTERS change NDEJC to ',
     >                 7,' in dimension.h'
           STOP 'You have to increase NDEjc in dimension.h'
         ENDIF
C--------cluster ejectile
         IF (NDEJC.GT.6) THEN
            AEJc(NDEJC) = aclu
            ZEJc(NDEJC) = zclu
            SEJc(NDEJC) = 0.5
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
C--------set reaction string
         REAction(nnuc) = '(z,gamma)'
C
         IF(iz. ge. 90 .and. iz. le. 98) then ! only from Th to Cf
C          retrieving NUBAR if available
           IF(IOPsys .EQ. 1) then  !Linux, Mac
C
C            CHECKING and READING the file data/nubar.endf      
C
             nubar_filename = trim(empiredir)//'/data/nubar.endf'
             len_nubar_filename = len_trim(nubar_filename) 

             INQUIRE(FILE=nubar_filename(1:len_nubar_filename),
     &               EXIST=NUBarread)
C
C            READING OF THE ENDF MF=1, MT=456 prompt nubar
C            and initialization of the ENIu_eval(Einc) ,VNIu_eval(Einc)
C            global arrays 
C
             IF(NUBarread) THEN 

               CALL READNUBAR(trim(nubar_filename),len_nubar_filename,
     &                        ierr, ia, iz)

               if(ierr.gt.0) NUBarread = .FALSE.

             ENDIF  
C
           ELSE                    !Windows
C
C             In Windows it is assumed that the NUBAR-EVAL.ENDF
C             was found so NUBarread = .TRUE. 
C
C             A new Windows only source file read_nubar_windows.f is provided
C
C             It contains:
C             1) An empty subroutine READNUBAR() that avoids
C                compilation errors of IO package in Windows
C
C             2) A replacement function fniu_nubar_eval(eincid)
C                that calculates the nubar from Th-232 nubar 
C                evaluation (0-60 MeV). It is designed to test 
C                PFNS calculations
C
              NUBarread = .TRUE.
 
           ENDIF 
         
         ENDIF
C
C--------other decaying nuclei
C
C--------NNUcd number of decaying nuclei
C--------NNUct total number of nuclei considered
C
         NEJcm = NDEJC
         IF (ZEJc(0).EQ.0.0D0 .AND. AEJc(0).EQ.0.0D0) SYMbe(0) = gamma
C--------correct ejectiles symbols
         DO nejc = 1, NEJcm
            IF (ZEJc(nejc).EQ.1.0D0 .AND. AEJc(nejc).EQ.2.0D0)
     &          SYMbe(nejc) = deut
            IF (ZEJc(nejc).EQ.1.0D0 .AND. AEJc(nejc).EQ.3.0D0)
     &          SYMbe(nejc) = trit
            IF (ZEJc(nejc).EQ.2.0D0 .AND. AEJc(nejc).EQ.3.0D0)
     &          SYMbe(nejc) = he3
         ENDDO
C
C        Please note that the order in which the array IZA(nnuc) is filled is
C        quite important.
C
         DO iac = 0, NEMc
         DO ih = 0, nemh
         DO it = 0, nemt
         DO id = 0, nemd
         DO ia = 0, nema
         DO ip = 0, nemp
         DO in = 0, nemn
           mulem = iac + ih + it + id + ia + ip + in
           IF (mulem.NE.0) THEN
             atmp = A(1) - FLOAT(in)*AEJc(1) - FLOAT(ip)*AEJc(2)
     &                   - FLOAT(ia)*AEJc(3) - FLOAT(id)*AEJc(4) 
     &                   - FLOAT(it)*AEJc(5) - FLOAT(ih)*AEJc(6)

             IF (NDEJC.GT.6) atmp = atmp - FLOAT(iac)*AEJc(NDEJC)

             ztmp = Z(1) - FLOAT(in)*ZEJc(1) - FLOAT(ip)*ZEJc(2) 
     &                   - FLOAT(ia)*ZEJc(3) - FLOAT(id)*ZEJc(4)
     &                   - FLOAT(it)*ZEJc(5) - FLOAT(ih)*ZEJc(6)

             IF (NDEJC.GT.6) ztmp = ztmp - FLOAT(iac)*ZEJc(NDEJC)

C            residues must be heavier than alpha !! (RCN)
             if(atmp.le.4 . or. ztmp.le.2) cycle
             izatmp = INT(1000*ztmp + atmp)
             CALL WHERE(izatmp,nnuc,iloc)
             IF (iloc.EQ.1) THEN
                  A(nnuc) = atmp
C  Temporary assignment of AMAss(nnuc) - permanent for nuclei not in mass table!
                  AMAss(nnuc) = atmp
                  Z(nnuc) = ztmp
                  XN(nnuc) = A(nnuc) - Z(nnuc)
                  IZA(nnuc) = izatmp
                  iia = INT(A(nnuc))
                  iz = INT(Z(nnuc))
                  SYMb(nnuc) = SMAT(iz)
                  HIS(nnuc) = -1.
                  IF (A(nnuc)*0.5.NE.AINT(A(nnuc)*0.5))
     &                HIS(nnuc) = -0.5
                  IF(NENdf.gt.0 .and. mulem.eq.in .and. in.le.4) THEN 
                   ENDf(nnuc) = 1 ! multiple neutron emission (up to 4 neutrons)
                   ENDFa(nnuc) = 1
                  ENDIF
C-----------------set reaction string
                  REAction(nnuc) = '(z,'
                  iend = 3
                  IF (in.NE.0) THEN
                   IF(in.le.9) then
                     WRITE (cnejec,'(I1)') in
                     IF (in.GT.1) THEN
                        REAction(nnuc)(iend + 1:iend + 1)
     &                     = cnejec
                        iend = iend + 1
                     ENDIF
                     REAction(nnuc)(iend + 1:iend + 1) = 'n'
                     iend = iend + 1
                   ELSE
                     WRITE (cnejec2,'(I2)') in
                     REAction(nnuc)(iend + 1:iend + 2)
     &                     = cnejec2
                        iend = iend + 2
                     REAction(nnuc)(iend + 1:iend + 1) = 'n'
                     iend = iend + 1
                   ENDIF
                  ENDIF
                  IF (ip.NE.0) THEN
                     WRITE (cnejec,'(I1)') ip
                     IF (ip.GT.1) THEN
                        REAction(nnuc)(iend + 1:iend + 1)
     &                     = cnejec
                        iend = iend + 1
                     ENDIF
                     REAction(nnuc)(iend + 1:iend + 1) = 'p'
                     iend = iend + 1
                  ENDIF

                  IF (mulem.eq.2 .and. (in.eq.1 .and. ip.eq.1) ) THEN
C                    From n,np   to   n,d   
                     iend = iend - 2
                     REAction(nnuc)(iend + 1:iend + 1) = 'd'
                     iend = iend + 1
                  ENDIF

                  IF (mulem.eq.3 .and. (in.eq.2 .and. ip.eq.1) ) THEN
C                    From n,2np   to   n,t   
                     iend = iend - 3
                     REAction(nnuc)(iend + 1:iend + 1) = 't'
                     iend = iend + 1
                  ENDIF

                  IF (mulem.eq.3 .and. (in.eq.1 .and. ip.eq.2) ) THEN
C                    From n,n2p   to   n,he3   
                     iend = iend - 3
                     REAction(nnuc)(iend + 1:iend + 1) = 'h'
                     iend = iend + 1
                  ENDIF

                  IF (mulem.eq.4 .and. (in.eq.2 .and. ip.eq.2) ) THEN
C                    From n,2n2p   to   n,a   
                     iend = iend - 4
                     REAction(nnuc)(iend + 1:iend + 1) = 'a'
                     iend = iend + 1
                  ENDIF

                  IF (mulem.eq.5 .and. (in.eq.3 .and. ip.eq.2) ) THEN
C                    From n,3n2p   to   n,na   
                     iend = iend - 4
                     REAction(nnuc)(iend + 1:iend + 2) = 'na'
                     iend = iend + 2
                  ENDIF
                                  
                  IF (ia.NE.0) THEN
                     WRITE (cnejec,'(I1)') ia
                     IF (ia.GT.1) THEN
                        REAction(nnuc)(iend + 1:iend + 1)
     &                     = cnejec
                        iend = iend + 1
                     ENDIF
                     REAction(nnuc)(iend + 1:iend + 1) = 'a'
                     iend = iend + 1
                  ENDIF
                  
                  IF (id.NE.0) THEN
                     WRITE (cnejec,'(I1)') id
                     IF (id.GT.1) THEN
                        REAction(nnuc)(iend + 1:iend + 1)
     &                     = cnejec
                        iend = iend + 1
                     ENDIF
                     REAction(nnuc)(iend + 1:iend + 1) = 'd'
                     iend = iend + 1
                  ENDIF

                  IF (it.NE.0) THEN
                     WRITE (cnejec,'(I1)') it
                     IF (it.GT.1) THEN
                        REAction(nnuc)(iend + 1:iend + 1)
     &                     = cnejec
                        iend = iend + 1
                     ENDIF
                     REAction(nnuc)(iend + 1:iend + 1) = 't'
                     iend = iend + 1
                  ENDIF

                  IF (ih.NE.0) THEN
                     WRITE (cnejec,'(I1)') ih
                     IF (ih.GT.1) THEN
                        REAction(nnuc)(iend + 1:iend + 1)
     &                     = cnejec
                        iend = iend + 1
                     ENDIF
                     REAction(nnuc)(iend + 1:iend + 1) = 'h'
                     iend = iend + 1
                  ENDIF

                  IF (NDEJC.GT.6 .AND. iac.NE.0) THEN
                     WRITE (cnejec,'(I1)') iac
                     IF (iac.GT.1) THEN
                        REAction(nnuc)(iend + 1:iend + 1)
     &                     = cnejec
                        iend = iend + 1
                     ENDIF
                     REAction(nnuc)(iend + 1:iend + 2) = 'li'
                     iend = iend + 2
                  ENDIF
                  REAction(nnuc)(iend + 1:iend + 1) = ')'
                  REAction(nnuc)(iend + 2:iend + 4) = '   '
             ENDIF
           ENDIF
         ENDDO
         ENDDO
         ENDDO
         ENDDO
         ENDDO
         ENDDO
         ENDDO
C
C--------Retrieve C4 experimental data 
         IF (IX4ret.EQ.1) CALL RETRIEVE
C--------Retrieve C4 experimental data  *** done ***
         NNUcd = nnuc
         NNUct = nnuc

         NEXclusive = 0
         DO nnuc = 1, NNUcd
            IF (A(0).EQ.A(nnuc) .AND. Z(0).EQ.Z(nnuc)) NTArget = nnuc

            ENDf(nnuc) = 1
            ENDfa(nnuc) = 1

            irepeated = 0
            do i=1,nnuc-1
              IF (A(i).EQ.A(nnuc) .AND. Z(i).EQ.Z(nnuc)) irepeated = 1
            enddo
            if(irepeated.eq.0) NEXclusive = NEXclusive + 1 

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
                  NNUct = NNUct + 1
C                 These nuclei are always considered inclusive
                  ENDf(nnur) = 2
                  ENDfa(nnur) = 2
C
               ENDIF
            ENDDO
         ENDDO

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
               KTRlom(0,0) = 2408    ! Soukhovitskii et al non-dispersive CC OMP 2601
C                                    ! Replaced by Capote, Soukhovistkii et al OMP 2408
            ENDIF
         ELSEIF (AEJc(0).EQ.1 .AND. ZEJc(0).EQ.1) THEN
            IF (A(0).LE.220) THEN
               KTRlom(0,0) = 5405
            ELSE
               KTRlom(0,0) = 5408    ! Soukhovitskii et al non-dispersive CC OMP 5601
C                                    ! Replaced by Capote, Soukhovistkii et al OMP 5408
            ENDIF
         ELSEIF (AEJc(0).EQ.2 .AND. ZEJc(0).EQ.1) THEN
            KTRlom(0,0) = 6200       ! Haixia OMP for deuterons
         ELSEIF (AEJc(0).EQ.3 .AND. ZEJc(0).EQ.1) THEN
            KTRlom(0,0) = 7100       ! Bechetti OMP for tritons
         ELSEIF (AEJc(0).EQ.3 .AND. ZEJc(0).EQ.2) THEN
            KTRlom(0,0) = 8100       ! Bechetti OMP for He-3
         ELSEIF (AEJc(0).EQ.4 .AND. ZEJc(0).EQ.2) THEN
            KTRlom(0,0) = 9600       ! Avrigeanu OMP for He-4
C-----------(McFadden global potential 9100 could be used)
         ENDIF

         DO i = 1, NDNUC
            KTRlom(1,i) = 2405
            KTRlom(2,i) = 5405
            KTRlom(3,i) = 9600
            KTRlom(4,i) = 6200
            KTRlom(5,i) = 7100
            KTRlom(6,i) = 8100
c           KTRlom(NPRoject,i) = KTRlom(0,0)
         ENDDO

C
C--------inteligent defaults *** done ***
C
         Irun = 0
         CALL READIN(Irun)   !optional part of the input

         IF( KTRlom(0,0).eq.2408 .and. DIRECT. LT. -0.1) then
            DIRECT = 1
            KTRompcc = 2408 
            WRITE (8,*) 
     &       'Default actinide CC OMP 2408 used for n + A if A > 220'
            WRITE (8,*)'Coupled channels calculations will be performed'
            WRITE (8,*)'   for the incident neutron channel (DIRECT 1) '
 	   ENDIF

         IF( KTRlom(0,0).eq.5408 .and.  DIRECT. LT. -0.1) then
            DIRECT = 1
            KTRompcc = 5408 
            WRITE (8,*) 
     &       'Default actinide CC OMP 5408 used for p + A if A > 220'
            WRITE (8,*)'Coupled channels calculations will be performed'
            WRITE (8,*)'   for the incident proton channel (DIRECT 1)  '
 	   ENDIF

         IF(DIRECT.LT.-0.1) DIRECT = 0 ! Restoring the default to zero
                                       ! if DIRECT not present in the input

         IF(DIRECT.GT.1.9) KTRlom(NPRoject,NTArget) = KTRompcc

         IF(ENDf(NTArget).EQ.10) ENDf(NTArget)=1
         IF(ENDf(1).EQ.10) ENDf(1)=1 ! for compound
         IF(ENDf(0).EQ.10) ENDf(0)=1 ! for compound

         IF(ENDfa(NTArget).EQ.10) ENDfa(NTArget)=1
         IF(ENDfa(1).EQ.10) ENDfa(1)=1 ! for compound
         IF(ENDfa(0).EQ.10) ENDfa(0)=1 ! for compound

         IF(NENdf.EQ.0) THEN

           ENDf = 0
           ENDfa = 0
           NEXclusive = 0
           EXClusiv = .FALSE.

         ELSE ! NENdf.GT.0

            DO iac = 0, NEMc
            DO ih = 0, nemh
            DO it = 0, nemt
            DO id = 0, nemd
            DO ia = 0, nema
            DO ip = 0, nemp
            DO in = 0, nemn
              mulem = iac + ia + ip + in + id + it + ih
              if(mulem.eq.0) cycle
              atmp = A(1) - FLOAT(in)*AEJc(1) - FLOAT(ip)*AEJc(2)
     &                    - FLOAT(ia)*AEJc(3) - FLOAT(id)*AEJc(4)
     &                    - FLOAT(it)*AEJc(5) - FLOAT(ih)*AEJc(6)

              IF (NDEJC.GT.6) atmp = atmp - FLOAT(iac)*AEJc(NDEJC)
              ztmp = Z(1) - FLOAT(in)*ZEJc(1) - FLOAT(ip)*ZEJc(2)
     &                    - FLOAT(ia)*ZEJc(3) - FLOAT(id)*ZEJc(4)
     &                    - FLOAT(it)*ZEJc(5) - FLOAT(ih)*ZEJc(6)

              IF (NDEJC.GT.6) ztmp = ztmp - FLOAT(iac)*ZEJc(NDEJC)

C             residues must be heavier than alpha
              if(atmp.le.4 . or. ztmp.le.2) cycle
              izatmp = INT(1000*ztmp + atmp)
              CALL WHERE(izatmp,nnuc,iloc)

              IF(ENDf(nnuc).EQ.2 .or. nnuc.EQ.NTArget) cycle
              irepeated = 0
              do i=1,nnuc-1
                IF (A(i).EQ.A(nnuc) .AND. Z(i).EQ.Z(nnuc)) irepeated = 1
              enddo
              if(irepeated.eq.1) cycle

              IF(mulem.GT.NENdf .AND. ENDf(nnuc).NE.10) THEN
                EXClusiv = .FALSE.
                ENDf(nnuc) = 2
              ENDIF
C             This nucleus requested as exclusive in the optional input
              IF(ENDf(nnuc).EQ.10) ENDf(nnuc) = 1  

              IF(mulem.GT.NENdfa .AND. ENDfa(nnuc).NE.10) THEN
                ENDfa(nnuc) = 2
              ENDIF
C             This nucleus requested as exclusive in the optional input
              IF(ENDfa(nnuc).EQ.10) ENDfa(nnuc) = 1  
            ENDDO
            ENDDO
            ENDDO
            ENDDO
            ENDDO
            ENDDO
            ENDDO
C
C           Reducing the number of exclusive nuclei 
C           by eliminating those from higher emission loops 
C
            itmp = 0          
            DO i = 1, NEXclusive
              iatmp = INT(a(i))
              iztmp = INT(z(i))
              izatmp = INT(1000*iztmp + iatmp)
              CALL WHERE(izatmp,nnuc,iloc)
              
              IF(ENDf(nnuc).EQ.1) THEN 
                IF(LHMs.EQ.0. OR. NENdfa.EQ.0) ENDfa(nnuc) = 2 
                itmp = itmp + 1
                INExc(nnuc) = itmp 
              ELSE
               ENDfa(nnuc) = 2
              ENDIF
C               write(*,'(7i5)') i,INT(a(i)),INT(z(i)),nnuc,INExc(nnuc),
C     &                                            ENDf(nnuc),ENDfa(nnuc) 
            ENDDO
            NEXclusive = itmp
C           write(*,'(a3,i5)') '***',NEXclusive
          
         ENDIF
C
C--------check input for consistency
C
         IF(NEXclusive.GT.NDExclus) THEN
            WRITE(8,*)'ERROR: NEXclusive =',NEXclusive
            WRITE(8,*)'INSUFFICIENT DIMENSION NDExclus'
            WRITE(8,*)'INCREASE NDExclus AND RECOMPILE'
            STOP 'INSUFFICIENT DIMENSION NDExclus'
         ENDIF

C
C        Checking fission input consistency 
C
C	   To add printout of fission options default values: 
C           (by setting all of them to -1)
C     
C        itmp = 0
C        DO i = 1, NDNUC
C          IF(FISbar(i).eq.3) THEN
C            itmp = 1
C            FISdis(i)=0.
C            FISopt(i)=0.
C          ENDIF
C        ENDDO
C        if(itmp .gt. 0) then
C             WRITE (8,*) 
C    & 'WARNING: Fiss. transitional states can not be used with '
C            WRITE (8,*) 
C    & 'WARNING: HFB fission barriers, FISDIS set to 0 FOR Z=', 
C    &                   NINT(Z(i)), ' A=', NINT(A(i))
C            WRITE (8,*) 
C    & 'WARNING: Optical model for fission can not be used with '
C            WRITE (8,*) 
C    & 'WARNING: HFB fission barriers, FISOPT reset to 0 FOR Z=', 
C    &                   NINT(Z(i)), ' A=', NINT(A(i))
C        endif

         WRITE (8,*)
         IF(AEJc(0).gt.4 .and. NDLW.LT.100) THEN
            WRITE (8,*)
     &'WARNING: For HI induced reactions it is recommended Lmax~150-200'
            WRITE (8,*)
     &'WARNING: Increase NDLW parameter in dimension.h and recompile'
            WRITE (8,*)         
         ENDIF
         IF(IOPran.gt.0)  ! Gaussian 1 sigma error
     &      WRITE (8,*)
     &'Uncertainty samp.-gaussian pdf. Interval: [-3*sig,3*sig]'
         IF(IOPran.lt.0)  ! Uniform error
     &      WRITE (8,*)
     &'Uncertainty samp.-uniform pdf. Interval:[-1.732*sig,1.732*sig]'
            WRITE (8,*)
C
         IF (PEQc.GT.0 .and. GCAsc.EQ.0.d0) THEN
            GCAsc = 1.
            WRITE (8,*) ' '
            WRITE (8,*) ' WARNING: For PCROSS the gamma cascade must be' 
            WRITE (8,*) ' WARNING: taken into account, GCASC set to 1'
            WRITE (8,*) ' '
         ENDIF
C
         IF (MSC*MSD.EQ.0 .AND. (MSD + MSC).NE.0 .AND. A(nnuc)
     &       .GT.1.0D0 .AND. AEJc(0).LE.1.D0) THEN
            WRITE (8,*) ' '
            WRITE (8,*) ' WARNING: Usually MSD and MSC should both '
            WRITE (8,*) ' WARNING: be taken into account'
            WRITE (8,*) ' '
         ENDIF
         IF (MSD.NE.0 .AND. AEJc(0).NE.1.D0) THEN
            MSD = 0
            WRITE (8,*) ' '
            WRITE (8,*) ' WARNING: MSD calculations suppressed'
            WRITE (8,*) ' WARNING: (possible for nucleons only)'
            WRITE (8,*) ' '
         ENDIF
         IF (MSC.NE.0 .AND. AEJc(0).EQ.0.0D0) THEN
            MSC = 0
            WRITE (8,*) ' '
            WRITE (8,*) ' WARNING!!!! MSC has been turned off '
            WRITE (8,*) ' WARNING!!!! (It is not allowed for '
            WRITE (8,*) ' WARNING!!!! photo-nuclear reactions)'
            WRITE (8,*) ' '
         ENDIF
         IF (LHRtw.NE.0 .AND. AEJc(0).EQ.0.0D0) THEN
            LHRtw = 0
            WRITE (8,*) ' '
            WRITE (8,*) ' WARNING!!!! HRTW has been turned off '
            WRITE (8,*) ' WARNING!!!! (It is not allowed for '
            WRITE (8,*) ' WARNING!!!! photo-nuclear reactions)'
            WRITE (8,*) ' '
         ENDIF
C--------------------------------------------------------------------------
         IF (LHMs.NE.0 .AND. NDAng.NE.NDAnghmx ) THEN
            WRITE (8,*)
            WRITE (8,*) 'WARNING: NDAng reset to ',NDAnghmx, 
     &                             ' for compatibility with HMS'
            WRITE (8,*)
            NANgela = NDAnghmx
            NDAng   = NDAnghmx
C--------reset angles for inelastic calculations
            da = 180.0/(NDAng - 1)
            DO na = 1, NDAng
              ANGles(na) = (na - 1)*da
            ENDDO
            DO na = 1, NDAng
              CANgler(na) = COS(ANGles(NDAng - na + 1)*PI/180.)
              SANgler(na) = SQRT(1.D0 - CANgler(na)**2)
            ENDDO
         ENDIF
c         IF (LHMs.NE.0 .AND. (NDAng.NE.19 .OR. NDAng.NE.37)) THEN
c            WRITE (8,*) ' '
c            WRITE (8,*) 'ERROR: NDAng IN dimension.h MUST BE 19 or 37'
c            WRITE (8,*)
c     &'ERROR: FOR COMPATIBILITY OF ANGLE GRID IN EMPIRE AND HMS.'
c            WRITE (8,*)
c     &'ERROR: SET NDAng TO 19 or 37 AND RECOMPILE OR GIVE UP HMS OPTION'
c            STOP 'FATAL: NDAng IN dimension.h MUST BE 19 or 37 for HMS'
c         ENDIF
         IF (LHMs.NE.0 .AND. AEJc(0).GT.1.D0) THEN
            WRITE (8,*) ' '
            WRITE (8,*) 'ERROR: HMS allowed only for incident nucleons'
            WRITE (8,*) 'ERROR: and gammas -  Execution STOPPED'
            STOP ' HMS allowed only for incident nucleons and gammas'
         ENDIF
         IF (DIRect.GT.0 .AND. INT(AEJc(0)).EQ.0) THEN
            DIRect = 0
            WRITE (8,*)' '
            WRITE (8,*)' WARNING!!!! Direct mechanism is not supported'
            WRITE (8,*)' WARNING!!!! for photo-nuclear reactions and '
            WRITE (8,*)' WARNING!!!! has been turned off  '
            WRITE (8,*)' '
         ENDIF
         IF (DIRect.GT.0 .AND. KTRompcc.EQ.0) THEN
            KTRompcc = KTRlom(0,0)
            DIRpot   = ABS(KTRompcc)
            WRITE (8,*)
     &'WARNING: DIRPOT keyword not specified, but DIRECT keyword > 0'
            WRITE (8,*)' WARNING: DIRPOT set to ',abs(KTRompcc)

            if(ABS(KTRompcc).ne.9602) then
              WRITE (8,
     &'(''  Optical model parameters for direct inelastic scattering set
     & to RIPL #'',I4)') KTRompcc
            else
              WRITE (8,
     &'(''  Optical model parameters for direct inelastic scattering set
     & to Kumar & Kailas 2007 values'')')
            endif
            WRITE (8,*) ' '
         ENDIF

C--------input consistency check  *** done ***
C
C--------setup model matrix (IDNa) defining which model is used where
C                      ECIS   MSD   MSC           HMS   PCROSS
C                        1     2     3      4      5      6
C        1 neut. disc.   x     x     0      0      x      x
C        2 neut. cont.   0     x     x      0      x      x
C        3 prot. disc.   x     x     0      0      x      x
C        4 prot. cont.   0     x     x      0      x      x
C        5 gamma         0     0     x      0      0      x
C        6 alpha. cont.  0     0     0      0      0      x
C        7 deut . cont.  0     0     0      0      0      x
C        8 trit . cont.  0     0     0      0      0      x
C        9 He-3 . cont.  0     0     0      0      0      x
C       10 LI   . cont.  0     0     0      0      0      0
C       11 alpha. cont.  0     0     0      0      0      x
C       12 deut . cont.  0     0     0      0      0      x
C       13 trit . cont.  0     0     0      0      0      x
C       14 He-3 . cont.  0     0     0      0      0      x
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
C-----------stop MSC charge-exchange if DDHMS or PCROSS active
            IF (LHMs.GT.0 .OR. PEQc.GT.0.) THEN
               IF (NPRoject.EQ.1) THEN
                  IDNa(4,3) = 0
               ELSEIF (NPRoject.EQ.2) THEN
                  IDNa(2,3) = 0
               ELSE
                  WRITE (8,*) ''
                  WRITE (8,*)
     &                       'PCROSS AND MSD/MSC ARE NOT COMPATIBLE IN '
                  WRITE (8,*)
     &                       'THIS CASE. EXECUTION S T O P P E D       '
                  STOP 'ILLEGAL COMBINATION PCROSS + MSC/MSD !!!!'
               ENDIF
            ENDIF
         ENDIF
C--------set HMS  (.,5)
         IF (LHMs.GT.0) THEN
            IDNa(1,5) = 1  ! neutron discrete levels
            IDNa(2,5) = 1
            IDNa(3,5) = 1  ! proton  discrete levels
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
                  WRITE (8,*) ''
                  WRITE (8,*) 'HMS AND MSD/MSC ARE NOT COMPATIBLE IN '
                  WRITE (8,*) 'THIS CASE. EXECUTION S T O P P E D !!!! '
                  STOP 'ILLEGAL COMBINATION HMS + MSC/MSD'
               ENDIF
            ENDIF
         ENDIF

         IF (NENdf.eq.0) THEN          
            NPRIm_g = 0
            WRITE (8,
     &'('' Primary gammas not stored: ENDF formatting is turned off'')')
              WRITE (12,
     &'('' Primary gammas not stored: ENDF formatting is turned off'')')
            RECoil = 0.d0
            WRITE (8,
     &'('' Recoils are not calculated as ENDF formatting is turned off''
     &)')
              WRITE (12,
     &'('' Recoils are not calculated as ENDF formatting is turned off''
     &)')
         ENDIF 
C
C--------check if PCROSS active
C
         IF (PEQc.eq.0.d0) THEN
C--------dismiss discrete levels key as PCROSS is not active
            PEQcont = 0.d0
            WRITE (8,
     &'('' Discrete levels turned off in PCROSS as PCROSS is off'')')
              WRITE (12,
     &'('' Discrete levels turned off in PCROSS as PCROSS is off'')')          
         ENDIF
C--------set PCROSS  (.,6) cluster emission
         IF (PEQc.GT.0.d0) THEN
            IF(PEQcont.gt.0) IDNa(1,6) = 1  ! discrete N is included even with ECIS active
            IDNa(2,6) = 1
            IF(PEQcont.gt.0) IDNa(3,6) = 1  ! discrete P is included even with ECIS active
            IDNa(4,6) = 1
            IDNa(5,6) = 1  ! gammas
            IDNa(6,6) = 1  ! cont A 
            IDNa(7,6) = 1  ! cont D 
            IDNa(8,6) = 1  ! cont T 
            IDNa(9,6) = 1  ! cont H 
            IDNa(10,6) = 0 ! cont LI
C
C           It could be calculated but probably not formatted 
C
            IF(PEQcont.gt.0) then
              IDNa(11,6) = 1 ! alpha discrete
              IDNa(12,6) = 1 ! deut  discrete
              IDNa(13,6) = 1 ! trit  discrete
              IDNa(14,6) = 1 ! He-3  discrete
            ENDIF
C-----------stop PCROSS gammas if calculated within MSC
            IF (GST.GT.0 .AND. MSC.GT.0) IDNa(5,6) = 0
C-----------stop PCROSS inelastic scattering if MSC and/or MSD active
            IF (MSC.GT.0 .OR. MSD.GT.0) THEN
               IF (NPRoject.EQ.2) THEN
                  IDNa(3,6) = 0
                  IDNa(4,6) = 0
               ELSEIF (NPRoject.EQ.1) THEN
                  IDNa(1,6) = 0
                  IDNa(2,6) = 0
               ELSE
                  WRITE (8,*) ''
                  WRITE (8,*)
     &                       'PCROSS AND MSD/MSC ARE NOT COMPATIBLE IN '
                  WRITE (8,*) 'THIS CASE. EXECUTION S T O P P E D !!!! '
                  STOP 'ILLEGAL COMBINATION PCROSS + MSC/MSD'
               ENDIF
            ENDIF
C-----------stop PCROSS nucleon channels if HMS active
            IF (LHMs.GT.0) THEN
               IDNa(2,6) = 0
               IDNa(4,6) = 0
            ENDIF
         ENDIF
C--------print IDNa matrix
         WRITE (8,*) ' '
         WRITE (8,*)
     &             '           Use of direct and preequilibrium models '
         WRITE (8,*)
     &             '           --------------------------------------- '
         WRITE (8,*) ' '
         WRITE (8,*) 'Exit channel    ECIS    MSD    MSC',
     &              '    HMS   PCROSS'
         WRITE (8,*) ' '
         WRITE (8,'('' neut. disc. '',8I7)')
     &     (IDNa(1,j),j = 1,3),(IDNa(1,j),j = 5,NDMODELS)
         WRITE (8,'('' neut. cont. '',8I7)')
     &     (IDNa(2,j),j = 1,3),(IDNa(2,j),j = 5,NDMODELS)
         WRITE (8,'('' prot. disc. '',8I7)')
     &     (IDNa(3,j),j = 1,3),(IDNa(3,j),j = 5,NDMODELS)
         WRITE (8,'('' prot. cont. '',8I7)')
     &     (IDNa(4,j),j = 1,3),(IDNa(4,j),j = 5,NDMODELS)
         WRITE (8,'('' gammas      '',8I7)')
     &     (IDNa(5,j),j = 1,3),(IDNa(5,j),j = 5,NDMODELS)
         WRITE (8,'('' alpha cont. '',8I7)')
     &     (IDNa(6,j),j = 1,3),(IDNa(6,j),j = 5,NDMODELS)
         WRITE (8,'('' deut. cont. '',8I7)')
     &     (IDNa(7,j),j = 1,3),(IDNa(7,j),j = 5,NDMODELS)
         WRITE (8,'('' trit. cont. '',8I7)')
     &     (IDNa(8,j),j = 1,3),(IDNa(8,j),j = 5,NDMODELS)
         WRITE (8,'('' He-3  cont. '',8I7)')
     &     (IDNa(9,j),j = 1,3),(IDNa(9,j),j = 5,NDMODELS)
         WRITE (8,'('' LI    cont. '',8I7)')
     &     (IDNa(10,j),j = 1,3),(IDNa(10,j),j = 5,NDMODELS)
         WRITE (8,'('' alpha disc. '',8I7)')
     &     (IDNa(11,j),j = 1,3),(IDNa(11,j),j = 5,NDMODELS)
         WRITE (8,'('' deut. disc. '',8I7)')
     &     (IDNa(12,j),j = 1,3),(IDNa(12,j),j = 5,NDMODELS)
         WRITE (8,'('' trit. disc. '',8I7)')
     &     (IDNa(13,j),j = 1,3),(IDNa(13,j),j = 5,NDMODELS)
         WRITE (8,'('' He-3  disc. '',8I7)')
     &     (IDNa(14,j),j = 1,3),(IDNa(14,j),j = 5,NDMODELS)
         WRITE (8,*) ' '

C----------------------------------------------------------
         WRITE(12,*) ' '
         WRITE(12,*)
     &             '           Use of direct and preequilibrium models '
         WRITE(12,*)
     &             '           --------------------------------------- '
         WRITE(12,*) ' '
         WRITE(12,*) 'Exit channel    ECIS    MSD    MSC',
     &              '    HMS   PCROSS'
         WRITE(12,*) ' '
         WRITE(12,'('' neut. disc. '',8I7)')
     &     (IDNa(1,j),j = 1,3),(IDNa(1,j),j = 5,NDMODELS)
         WRITE(12,'('' neut. cont. '',8I7)')
     &     (IDNa(2,j),j = 1,3),(IDNa(2,j),j = 5,NDMODELS)
         WRITE(12,'('' prot. disc. '',8I7)')
     &     (IDNa(3,j),j = 1,3),(IDNa(3,j),j = 5,NDMODELS)
         WRITE(12,'('' prot. cont. '',8I7)')
     &     (IDNa(4,j),j = 1,3),(IDNa(4,j),j = 5,NDMODELS)
         WRITE(12,'('' gammas      '',8I7)')
     &     (IDNa(5,j),j = 1,3),(IDNa(5,j),j = 5,NDMODELS)
         WRITE(12,'('' alpha cont. '',8I7)')
     &     (IDNa(6,j),j = 1,3),(IDNa(6,j),j = 5,NDMODELS)
         WRITE(12,'('' deut. cont. '',8I7)')
     &     (IDNa(7,j),j = 1,3),(IDNa(7,j),j = 5,NDMODELS)
         WRITE(12,'('' trit. cont. '',8I7)')
     &     (IDNa(8,j),j = 1,3),(IDNa(8,j),j = 5,NDMODELS)
         WRITE(12,'('' He-3  cont. '',8I7)')
     &     (IDNa(9,j),j = 1,3),(IDNa(9,j),j = 5,NDMODELS)
         WRITE(12,'('' LI    cont. '',8I7)')
     &     (IDNa(10,j),j = 1,3),(IDNa(10,j),j = 5,NDMODELS)
         WRITE(12,'('' alpha disc. '',8I7)')
     &     (IDNa(11,j),j = 1,3),(IDNa(11,j),j = 5,NDMODELS)
         WRITE(12,'('' deut. disc. '',8I7)')
     &     (IDNa(12,j),j = 1,3),(IDNa(12,j),j = 5,NDMODELS)
         WRITE(12,'('' trit. disc. '',8I7)')
     &     (IDNa(13,j),j = 1,3),(IDNa(13,j),j = 5,NDMODELS)
         WRITE(12,'('' He-3  disc. '',8I7)')
     &     (IDNa(14,j),j = 1,3),(IDNa(14,j),j = 5,NDMODELS)
         WRITE(12,*) ' '
C--------model matrix *** done ***

C--------reset some options if OMP fitting option selected
         IF (FITomp.NE.0) THEN
            IOUt = 1
            NEXreq = MIN(NEXreq,30)
            MSD = 0
            MSC = 0
            LHMs = 0
            DEGa = 0
            PEQc = 0
            MFPp = 1.3
            NNUcd = 2
            NNUct = 4
         ENDIF
C Set number of angles to minimum for first energy of automatic search
        IF (FITomp.LT.0) THEN
          NDAng = 2
          NANgela = 2
          ANGles(1) = 0.0
          ANGles(2) = 180.
         ENDIF
C--------READ nuclear deformations and masses
         CALL READNIX

C--------set projectile/ejectile masses
C
C        Values from ENDF manual 2009 are used for usual projectiles/ejectiles;
C        ions have to be calculated separately
         DO nejc = 0, min(6,NDEJC)
C          Setting projectile/ejectiles mass 
C          these are nuclear masses following ENDF Manual 2009, so electron mass is not considered
C          EJMass(nejc) = (AEJc(nejc)*AMUmev - ZEJc(nejc)*AMUele + XMAss_ej(nejc))/AMUmev
           EJMass(nejc) = AEJc(nejc) + XMAss_ej(nejc)/AMUmev
         ENDDO
C
C        Light ion nuclear mass estimated from mass excess table
C
         IF(NDEJC.gt.6)   
     &     EJMass(NDEJC) = AEJc(NDEJC) + XMAss_ej(NDEJC)/AMUmev         
C    &    (AEJc(NDEJC)*AMUmev - ZEJc(NDEJC)*AMUele + XMAss_ej(NDEJC))/AMUmev

C--------READ shell corrections of RIPL
         CALL READ_SHELL_CORR
C--------Read number of reasonably known levels and level density parameter 'a'
         CALL READLDP
C--------fix-up deformations for CCFUS coupled channels
         IF (CSRead.EQ.( - 2.0D0) .AND. AEJc(0).GT.4.0D0) THEN
C-----------fix-up deformations and discrete levels for CCFUS
            ierr = IFindColl_CCFUS()

            DO j = 1, NSCc
               IF (QCC(j).EQ.0.0D0) THEN
                  IF (FLAm(j).GE.0.0D0) THEN
                    WRITE (8,*)
     &                ' Collective state ', ABS(FLAm(j)),
     &                ' in target (sequence number', j,
     &                ') has excitation energy of 0 MeV'
                    iccerr = 1   
                  ENDIF  
                  IF (FLAm(j).LT.0.0D0) THEN
                    WRITE (8,*)
     &                ' Collective state ', ABS(FLAm(j)),
     &                ' in projectile (sequence number', j,
     &                ') has excitation energy of 0 MeV'
                    iccerr = 2
                  ENDIF
                  WRITE (8,*)
     &            ' Likely the code was not able to find out this state'
                  WRITE (8,*)
     &                 ' you must set this energy in the EMPIRE input'
                  NSCc = NSCc - 1
               ENDIF
               IF (BETcc(j).EQ.0.0D0) THEN
                  IF (FLAm(j).LT.0.0D0) BETcc(j) = DEFprj
                  IF (FLAm(j).GE.0.0D0) BETcc(j) = DEF(1,0)
               ENDIF
            ENDDO

            IF (iccerr.EQ.1) THEN
              WRITE(8,*) 
              WRITE(8,*) 
     &        ' WARNING: CCFUS COUPLED LEVEL MISSING FOR TARGET'
              WRITE(8,*) 
     &        ' WARNING: Number of inelastic levels in CCFUS =',NSCc
              IF(NSCc.LT.0) NSCc =0 
              WRITE(8,*) 
            ENDIF

            IF (iccerr.EQ.2) THEN
              WRITE(8,*) 
              WRITE(8,*) 
     &        ' WARNING: CCFUS COUPLED LEVEL MISSING FOR PROJECTILE'
              WRITE(8,*) 
     &        ' WARNING: Number of inelastic levels in CCFUS =',NSCc
              IF(NSCc.LT.0) NSCc =0 
              WRITE(8,*) 
            ENDIF

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
C     KTRlom(0,0) = KTRlom(NPRoject,NTArget)

C     Setting the normalization factor for OMP (used in covariance calculation)
C     for the incident channel
      FNvvomp(0,0) = FNvvomp (NPRoject,NTArget)
      FNwvomp(0,0) = FNwvomp (NPRoject,NTArget)
      FNwsomp(0,0) = FNwsomp (NPRoject,NTArget)
      FNavomp(0,0) = FNavomp (NPRoject,NTArget)
      FNasomp(0,0) = FNasomp (NPRoject,NTArget)
      FNrvomp(0,0) = FNrvomp (NPRoject,NTArget)
      FNrwvomp(0,0)= FNrwvomp(NPRoject,NTArget)
      FNrsomp(0,0) = FNrsomp (NPRoject,NTArget)

      IF (AEJc(0).GT.4.0D0) KTRlom(0,0) = 0  ! HI
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
C-----determination of discrete levels and pairing shift for cn
      write(8,*)
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
      IF (DIRect.GT.0 .AND. FIRst_ein  .AND. AEJc(0).LE.4 ) THEN
                              ! Inelastic scattering by DWBA for all particles
C
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
               WRITE (8,*) ' WARNING: Some collective discrete levels',
     &                     '  for target nucleus not found'
               WRITE (8,*) ' WARNING: check TARGET.LEV file '
            ELSEIF (ierr.EQ.2) THEN
               WRITE (8,*) ' WARNING: No discrete levels for target',
     &                     ' nucleus found'
               WRITE (8,*) ' WARNING: Direct cross section will not be',
     &                     ' calculated'
               WRITE (8,*) ' WARNING: Setting DIRECT to 0 '
               DIRect = 0
            ENDIF
      ENDIF

      EXCn = EIN + Q(0,1) + ELV(LEVtarg,0)
      EMAx(1) = EXCn
C-----set Q-value for CN production
      QPRod(1) = Q(0,1)
      ia = INT(A(0))
      iae = INT(AEJc(0))
      IF (ENDf(1).EQ.0) THEN
         IF (DEFga.NE.0.0D0) WRITE (12,
     &        '('' DEFGA='',F7.3,'' DEFGW='',F7.3,    '' DEFGP='',F7.3)'
     &        ) DEFga, DEFgw, DEFgp
      ENDIF
C-----WRITE heading on FILE6
      IF (IOUt.GT.0) THEN
         WRITE (8,*) ' '
         WRITE (8,*) ' '
         WRITE (8,*) ' '
         WRITE (8,'(60(''=''))')
         WRITE (8,
     &'('' Reaction '',I3,A2,''+'',I3,A2,'' at incident energy '',G9.3,'
     &' MeV'')') iae, SYMbe(0), ia, SYMb(0), EINl
         WRITE (8,'(60(''=''))')
         WRITE (8,*) ' '
         WRITE (8,'('' Compound nucleus energy'',F9.3,'' MeV'')') EXCn
         WRITE (8,'('' Projectile binding energy'',F8.3,'' MeV'')')
     &          Q(0,1)
      ENDIF
C
C-----determination of excitation energy matrix in CN
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
C     The line below introduces dependence on the NDEX in dimension.h
C     However it supress any dependence on input NEXreq
C-----check whether spectrum array can accommodate capture with this DE
      CALL CHECK_DE(EMAx(1),NDECSE)
C     CALL CHECK_DE(EMAx(1),NEXreq)

C-----check whether any residue excitation is higher than CN
      qmin = 1000.0d0
      ichanmin = -1
      DO i = 1, NDEJC
        CALL BNDG(i,1,qtmp)
         IF (qtmp.LT.qmin) then
            qmin = qtmp
            ichanmin = i
         ENDIF
      ENDDO
      
      IF(qmin.lt.0.d0) THEN
        WRITE(8,'(1x,A19)')   'Exotermic reaction '
        CALL WHERE(IZA(1)-IZAejc(ichanmin),nucmin,iloc)
C-------check whether population array can accommodate the reaction with the largest
C-------continuum using current DE, if not adjust DE
        CALL CHECK_DE(EMAx(1)-qmin-ECUt(nucmin),NDEX)
C-------check whether spectra array can accommodate the reaction with the largest
C-------continuum using current DE, if not adjust DE
        CALL CHECK_DE(EMAx(1)-qmin,NDECSE)
      ENDIF

      WRITE(8,'(1x,A28,F6.1,A4)')
     &       'Energy step in calculations ',DE*1000.d0,' keV'
      WRITE(8,'(1x,''Number of energy points ='',i3,''   NDEX ='',i3)') 
     &   NEXreq, NDEX

      IF(2*NEXreq.GT.NDEX) WRITE(8,*)  
     & 'WARNING: NDEX in dimension.h is ',NDEX,'  recommended',
     & ' value is ', 2*NEXreq     

      DO i = 1, NEX(1)
         EX(i,1) = ECUt(1) + FLOAT(i - 1)*DE
      ENDDO

C-----determination of excitation energy matrix in CN ***done***
C
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
      CALL INP_LD(nnuc)      
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
               WRITE (8,'('' NO LOCATION ASCRIBED TO NUCLEUS '',I8)')
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

C           IF (NEX(nnur).GT.NEXreq) THEN
            IF (NEX(nnur).GT.NDEX) THEN
               WRITE (8,*)

               WRITE (8,'('' WARNING: NUMBER OF BINS '',I3,
     &                    '' IN RESIDUAL NUCLEUS '',I3,A1,A2,
cC    &         '' EXCEEDS REQUESTED ENERGY STEPS '',I3,  
cC    &          NEX(nnur), NINT(A(nnur)),'-',SYMb(nnur),NEXreq
     &         '' EXCEEDS DIMENSIONS '',I3)')  NEX(nnur), NINT(A(nnur)),
     &         '-',SYMb(nnur),NDEX
               WRITE (8,
     &         '(''          Reaction '',I3,A1,A2,'' -> '',I3,A1,A2,
     &           ''  +  '',I2,A1,A2,'' NEGLECTED '')')
     &          NINT(A(nnuc)),'-',SYMb(nnuc),
     &          NINT(ares),   '-',SYMb(nnur),
     &          NINT(AEJc(nejc)),'-',SYMbe(nejc)
               WRITE (8,*)
     &          '         TO CONSIDER IT, YOU HAVE TO INCREASE ',
     &          '        NDEX PARAMETER IN dimension.h'
               WRITE (8,'('' WARNING: EMAXr : '',F7.2,
     &            ''; COULOMB BARRIER : '',F7.2)') emaxr, culbar
               WRITE (8,*)
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
C-----------determination of Q-value for isotope production
            qtmp = QPRod(nnuc) - Q(nejc,nnuc)
            IF (qtmp.GT.QPRod(nnur)) QPRod(nnur) = qtmp
            IF (FITlev.GT.0.0D0) ECUt(nnur) = 0.0
C-----------determination of etl matrix for the transmission coeff. calculation
C-----------first 4 elements are set independently in order to get more
C-----------precise grid at low energies. from the 5-th element on the step
C-----------is de (bin width).
C-----------determination of etl matrix
            netl = 6
            IF (NEX(nnuc).GT.0) netl =
     &         INT((EX(NEX(nnuc),nnuc) - Q(nejc,nnuc))/DE) + 6
            IF (netl.GT.NDETL) THEN
               WRITE (8,*)
     &             ' WARNING: netl = ',netl,' > NDETL = ',NDETL
               WRITE (8,
     &         '(''          Reaction '',I3,A1,A2,'' -> '',I3,A1,A2,
     &           ''  +  '',I2,A1,A2,'' NEGLECTED '')')
     &          NINT(A(nnuc)),'-',SYMb(nnuc),
     &          NINT(ares),   '-',SYMb(nnur),
     &          NINT(AEJc(nejc)),'-',SYMbe(nejc)
                EMAx(nnur) = 0.d0
                NEX(nnur) = 0
                NEXr(nejc,nnuc) = 0
                Q(nejc,nnuc) = 99.d0
                CYCLE
            ENDIF
            IF (NEXr(nejc,nnuc).GT.0 .AND. NEX(nnuc).GT.0) THEN
               ETL(5,nejc,nnur) = EX(NEX(nnuc),nnuc)
     &                            - EX(NEXr(nejc,nnuc),nnur)
     &                            - Q(nejc,nnuc)
            ELSE
               ETL(5,nejc,nnur) = 0.
            ENDIF
            IF (nejc.EQ.1) ETL(5,nejc,nnur) = 0.
Cpr         WRITE(8,*) 'etl(5,.),netl',etl(5,nejc,nnur),netl
Cpr         etlmax=EX(NEX(NNUC),NNUC)-Q(NEJC,NNUC)
Cpr         WRITE(8,*) 'etlmax',etlmax
            ETL(1,nejc,nnur) = 0.
            ETL(2,nejc,nnur) = 0.1*ETL(5,nejc,nnur)
            ETL(3,nejc,nnur) = 0.2*ETL(5,nejc,nnur)
            ETL(4,nejc,nnur) = 0.5*ETL(5,nejc,nnur)
            DO ietl = 6, netl
               ETL(ietl,nejc,nnur) = ETL(ietl - 1,nejc,nnur) + DE
            ENDDO
Cpr         WRITE(8,*)
Cpr         >        'TL ENERGIES FOR TARGET A=',A(NNUR),' PROJECTILE A=',
Cpr         >        AEJC(NEJC),' Z=',ZEJC(NEJC)
Cpr         DO I=1,NETL
Cpr         WRITE(8,*) I,ETL(I,NEJC,NNUR)
Cpr         END DO
         ENDDO     !over ejectiles (nejc)
      ENDDO     !over nuclei (nnuc)
      WRITE (8,*) ' '
      WRITE (8,*) 'Total number of nuclei considered :', NNUct

      IF(ENDF(1).GT.0) THEN
        WRITE(8,*) 'Number of exclusive nuclei        :',NEXclusive
        WRITE(8,*) 'Nuclei marked with < in the table below produce excl
     &usive emission spectra'
      ENDIF

      WRITE (8,*) ' '
C-----LEVEL DENSITY for residual nuclei 
      DO nnur = 2, NNUct
         IF (NEX(nnur).LE.0) cycle
         CALL INP_LD(nnur)
      ENDDO
      WRITE (8,*)

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
C
C---- fission input is created if it does not exist and FISSHI=0
C
      DO nnuc = 1, NNUct
         FISsil(nnuc) = .TRUE.
         IF (FISshi(nnuc).EQ.0. .AND.
     &       (Z(nnuc).LT.78. .OR. A(nnuc).LT.200.)) THEN
            FISsil(nnuc)= .FALSE.
         ENDIF
         IF (FISshi(nnuc).EQ.1.) THEN
            xfis = 0.0205*Z(nnuc)**2/A(nnuc)
            IF (xfis.LT.0.3D0) FISsil(nnuc) = .FALSE.
         ENDIF
         IF (FISshi(nnuc).EQ.2.) FISsil(nnuc) = .FALSE.
      ENDDO
      calc_fiss=.FALSE.
      DO nnuc = 1, NNUct
       IF (FISsil(nnuc)) then
         calc_fiss=.TRUE.
         if(AEJc(0).Gt.4. .and. FISshi(nnuc).le.0.d0) FISshi(nnuc)=1.d0
       ENDIF
      ENDDO

      INQUIRE (FILE = 'FISSION.INP',EXIST = gexist)
      IF (.NOT.gexist .and. calc_fiss) THEN
         OPEN (79,FILE = 'FISSION.INP',STATUS = 'NEW')
         DO nnuc = 1, NNUcd
            IF (FISsil(nnuc) .AND. FISshi(nnuc).EQ.0) CALL INPFIS(nnuc)
         ENDDO
         CLOSE (79)
      ENDIF
99010 FORMAT (1X,14(G10.4,1x))
      END
C
C
      SUBROUTINE INP_LD(Nnur)

      INCLUDE 'dimension.h'
      INCLUDE 'global.h'

      REAL*8 rocumul
      INTEGER Nnur,ia

      IF (ADIv.EQ.0.0D0) CALL ROEMP(nnur,0.0D0,0.024D0)
      IF (ADIv.EQ.1.0D0) CALL ROGSM(nnur)
C     IF (ADIv.EQ.2.0D0) CALL ROGCM(nnur, 0.146D0)
      IF (ADIv.EQ.3.0D0) CALL ROHFB(nnur)
C-----<m2> could be added to the input ( to use 0.124 if needed)
      IF (ADIv.EQ.4.0D0) CALL ROGC(nnur,0.24D0)
C     IF (ADIv.EQ.4.0D0) CALL ROGC(nnur, 0.146D0)

      IF (IOUt.EQ.6) THEN
         ia = INT(A(nnur))
         IF (ADIv.NE.3.0D0) THEN
            WRITE (8,'(1X,/,''  LEVEL DENSITY FOR A SINGLE PARITY '' 
     &        ,I3,''-'',A2)') ia, SYMb(nnur)
            WRITE(8,'(/2x,A23,1x,F6.3,A15,I3,A18//
     &1x,''   Ex     RHO(Ex,pi)   RHO(Ex,pi,J => ...)   '')')     
     &        'Continuum starts at Ex=',ELV( NLV(nnur),nnur),
     &        ' MeV above the ',NLV(nnur),'-th discrete level'     
            DO i = 1, NEX(nnur)
               rocumul = 0.D0
               DO j = 1, NDLW
                  rocumul = rocumul + RO(i,j,1,nnur)
               ENDDO
               
               IF(i.GE.10 .and. rocumul .LE. 0.1d0) exit               
               WRITE (8,99010) EX(i,nnur), rocumul,
     &              (RO(i,j,1,nnur),j = 1,11)
c    &                     (RO(i,j,1,nnur),j = 11,21)
c    &                     (RO(i,j,1,nnur),j = 21,31)     
           ENDDO

         ELSE

           WRITE (8,'(1X,/,''  HFB LEVEL DENSITY DEPENDENCE FOR '' 
     &        ,I3,''-'',A2)') ia, SYMb(nnur)
           WRITE(8,'(/2x,A25,1x,F5.2,A46,I3//
     &1x,''   E        RHO(E)  '')')     
     &        'Continuum starts above E=',ELV( NLV(nnur),nnur),
     &        ' MeV above the corresponding discrete level # ',NLV(nnur)     

            WRITE (8,'(1X,/,''  POSITIVE PARITY'')')
            DO i = 1, NEX(nnur)
               rocumul = 0.D0
               DO j = 1, NDLW
                  rocumul = rocumul + RO(i,j,1,nnur)
               ENDDO
               IF(i.GE.10 .and. rocumul .LE. 0.1d0) exit
               
               WRITE (8,99010) EX(i,nnur), rocumul,
     &              (RO(i,j,1,nnur),j = 1,11)
c     &                     (RO(i,j,1,nnur),j = 11,21)
c     &                     (RO(i,j,1,nnur),j = 21,31)
            ENDDO

            WRITE (8,'(1X,/,''  NEGATIVE PARITY'')')
            DO i = 1, NEX(nnur)
               rocumul = 0.D0
               DO j = 1, NDLW
                  rocumul = rocumul + RO(i,j,2,nnur)
               ENDDO
               
               IF(i.GE.10 .and. rocumul .LE. 0.1d0) exit
               WRITE (8,99010) EX(i,nnur), rocumul,
     &              (RO(i,j,2,nnur),j = 1,11)
c     &                     (RO(i,j,2,nnur),j = 11,21)
c     &                     (RO(i,j,2,nnur),j = 21,31)
            ENDDO
         ENDIF
      ENDIF
99010 FORMAT (1X,14(G10.4,1x))
      RETURN 
      END
 
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
      CHARACTER*132 ctmp
      INTEGER*4 iwin
      INTEGER*4 PIPE

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
      LOGICAL LREad, ADDnuc, fexist

      ADDnuc = .FALSE.

      ia = A(Nnuc) + 0.001
      iz = Z(Nnuc) + 0.001

C-----Check if FITLEV option was run
      INQUIRE (FILE = ('FITLEV.PS'),EXIST = fexist)
C     Looking for Dobs and Gg for compound (resonances are stored for target nucleus)
      IF (Nnuc.eq.0 .AND. (AEJc(0).EQ.1 .AND. ZEJc(0).EQ.0) ) THEN ! only for neutrons
        OPEN (47,FILE = trim(empiredir)//'/RIPL/resonances'
     &      //'/resonances0.dat',STATUS = 'old',ERR = 65)
        READ (47,'(///)') ! Skipping first 4 title lines
        DO i = 1, 296
C         READ (47,'(2i4,  17x,2(e9.2,2x),2(f4.2,2x),2(F5.1,1x))',
C         Changed to RIPL-3 file
          READ (47,
     &     '(i3,4x,i3,15x,2(e8.2,2x),1x,2(f4.2,2x),f5.1,1x,f5.1)',
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
   65   WRITE (8,*) ' WARNING: ',trim(empiredir)//
     &   '/RIPL/resonances/resonances0.dat file not found '
        WRITE (8,*) 

     &   ' WARNING: Experimental D0 and gamma width are not available '
   70   CONTINUE
      ENDIF
      IF(Gg_obs.EQ.0) THEN   !No experimental Gg - use Kopecky's spline fit
         IF(ia.LT.40) THEN
           Gg_obs = 1593000/A(Nnuc)**2   !in meV
         ELSE
            OPEN (47,FILE = trim(empiredir)//'/data/Ggamma.dat'
     &         ,STATUS='old',ERR=75)
            READ (47,'(///)') ! Skipping first 4 title lines
            DO i = 1, 250
               READ (47,'(3x,I5,F8.4)',END=75,ERR=75) natmp, gggtmp
               IF (natmp.NE.Ia) CYCLE
               Gg_obs = gggtmp*1000.0D0 !in meV
               GOTO 85
            ENDDO
   75       WRITE (8,*) 'WARNING: empire/data/Ggamma.dat file not found'
   85       CLOSE (47)
         ENDIF
      ENDIF

      LREad = .TRUE.
      izatmp = INT(1000*iz + ia)
      DO itmp = 0,NDNuc
        IF(NSTOred(itmp).eq.izatmp) THEN
          LREad = .FALSE.
          GOTO 50
        ENDIF
      ENDDO

   50 IF(.NOT.LREad) then
        NLV(Nnuc) = NLV(itmp)
        NCOmp(Nnuc) = NCOmp(itmp)
        DO ilv = 1, NLV(Nnuc)
          ELV(ilv,Nnuc) = ELV(ilv,itmp)
          XJLv(ilv,Nnuc) = XJLv(ilv,itmp)
          LVP(ilv,Nnuc) = LVP(ilv,itmp)
          ISIsom(ilv,Nnuc) = ISIsom(ilv,itmp)
          DO nbr = 1, NDBR
            BR(ilv,nbr,1,Nnuc) = BR(ilv,nbr,1,itmp)
            BR(ilv,nbr,2,Nnuc) = BR(ilv,nbr,2,itmp)
            BR(ilv,nbr,3,Nnuc) = BR(ilv,nbr,3,itmp)
          ENDDO
        ENDDO
        RETURN
      ENDIF
C-----set ground state in case nucleus not in file
C
C     Avoid overwriting the NLV(nnuc) assigned in READLDP 
C       (coming from level-density-param.dat file)
C
      IF(NLV(Nnuc).le.0) NLV(Nnuc) = 1
      IF(NCOmp(Nnuc).le.0) NCOmp(Nnuc) = 1

      ELV(1,Nnuc) = 0.0
      LVP(1,Nnuc) = 1
      XJLv(1,Nnuc) = 0.0
      IF (A(Nnuc) - 2.0*INT(A(Nnuc)/2.0).GT.0.01D0) XJLv(1,Nnuc) = 0.5
      ISIsom(1,Nnuc) = 0
C-----set ground state *** done ***
      IF(.NOT.FILevel) THEN
C-------constructing input and filenames
        WRITE (ctmp3,'(I3.3)') iz
        finp = 'z'//ctmp3//'.dat'
        OPEN (13,FILE = trim(empiredir)//'/RIPL/levels/'//finp
     &      ,STATUS = 'OLD',ERR = 400)
      ELSE
        REWIND (13)
      ENDIF
  100 READ (13,'(A5,6I5,2f12.6)',END = 300) chelem, iar, izr, nlvr,
     &      ngamr, nmax, itmp2, qn
      IF (ia.NE.iar .OR. iz.NE.izr) THEN
        DO ilv = 1, nlvr + ngamr
          READ (13,'(A1)',END = 300) dum
        ENDDO
        GOTO 100
      ELSE
C----------nmax is a number of levels that constitute a complete scheme as
C----------estimated by Belgya for RIPL. We find it generally much too high.
C----------If run with FITLEV>0 has not been executed we divide nmax by 2.
C----------A visual check with FITLEV is always HIGHLY RECOMMENDED!!!
c       IF(FITlev.EQ.0 .AND. .not.fexist .AND. nmax.GT.6) THEN
c          nmax = MIN(nmax/2 + 1, 15)
c          WRITE (8,'('' WARNING:'')')
c          WRITE (8,'('' WARNING: For isotope '',A5)')
c    &            chelem
c          WRITE (8,'('' WARNING: number of levels was reduced to '',
c    &            I3)') nmax
c          WRITE (8,'('' WARNING: since FITLEV option had not been'',
c    &            '' run before'')')
c       ENDIF
C----------create file with levels (*.lev)
C----------NLV   number of levels with unique spin and parity
C----------NCOMP number of levels up to which the level scheme is estimated
C----------to be complete
C
        IF ( (.NOT.FILevel) .OR. ADDnuc) THEN
          BACKSPACE (13)
          READ (13,'(A110)') ch_iuf
C         WRITE (14,'(A60,'' RIPL-3'')') ch_iuf
          WRITE (14,'(A110)') ch_iuf
        ENDIF
        IF (nlvr.NE.0) THEN
          IF (NLV(Nnuc).EQ.1 .AND. nmax.GT.1) NLV(Nnuc) = MIN(NDLV,nmax)
C---------limit to max. of 40 levels if ENDF active
          IF (ENDf(1).GT.0) NLV(Nnuc) = MIN(NLV(Nnuc),40)
          IF (NCOmp(Nnuc).EQ.1 .AND. nlvr.GT.1) NCOmp(Nnuc)
     &          = MIN(NDLV,nlvr)
          IF ( (.NOT.FILevel) .OR. ADDnuc) THEN
             DO ilv = 1, nlvr + ngamr
               READ (13,'(A110)') ch_iuf
               WRITE (14,'(A110)') ch_iuf
             ENDDO
             DO ilv = 1, nlvr + ngamr
               BACKSPACE (13)
             ENDDO
          ENDIF
C---------levels for nucleus NNUC copied to file *.lev
          NSTored(nnuc) = izatmp
          DO ilv = 1, NLV(Nnuc)
            READ (13,'(I3,1X,F10.6,1X,F5.1,I3,1X,E10.2,I3)') istart,
     &               ELV(ilv,Nnuc), XJLv(ilv,Nnuc), LVP(ilv,Nnuc), t12,
     &               ndbrlin
            IF (ELV(ilv,Nnuc).GT.qn) THEN
              NLV(Nnuc) = max(ilv - 1,1)
              WRITE (8,'('' WARNING:'')')
              WRITE (8,'('' WARNING: Element ='',A5,2x,2HZ=,I3)')
     &                   chelem, izr
              WRITE (8,
     &'('' WARNING: Excited state '',I3,                             ''
     &is above neutron binding energy '',F6.3,                       ''
     &MeV'')') ilv, qn
              WRITE (8,'('' WARNING: Number of levels set to '',I3)'
     &                   ) NLV(Nnuc)
              GOTO 200
            ENDIF
            IF (ilv.EQ.1 .AND. ELV(ilv,Nnuc).GT.4.) THEN
              WRITE (8,'('' WARNING:'')')
              WRITE (8,'('' WARNING: Element ='',A5,2x,2HZ=,I3)')
     &                      chelem, izr
              WRITE (8,
     &'('' WARNING: excited state No.'',I3,                          ''
     &has energy of '',F6.3,'' MeV'')') ilv, ELV(ilv,Nnuc)
            ENDIF

            IF (ilv.EQ.1 .AND. XJLv(ilv,Nnuc).LT.0.) THEN
              WRITE (8,'('' WARNING:'')')
              WRITE (8,'('' WARNING: Element ='',A5,2x,2HZ=,I3)')
     &                      chelem, izr
              WRITE (8,
     &'('' WARNING: ground-state has no assigned spin/parity '')')
              WRITE (8, '('' WARNING: assuming a default '')')
              LVP(1,Nnuc) = 1
              XJLv(1,Nnuc) = 0.0
              IF (A(Nnuc) - 2.0*INT(A(Nnuc)/2.0).GT.0.01D0)
     >           XJLv(1,Nnuc) = 0.5
              ISIsom(1,Nnuc) = 0
            ENDIF

            IF (ilv.NE.1) THEN
              IF (ELV(ilv,Nnuc).EQ.0.) THEN
                WRITE (8,'('' WARNING:'')')
                WRITE (8,'('' WARNING: Element ='',A5,2x,2HZ=,I3)')
     &                      chelem, izr
                WRITE (8,
     &'('' WARNING: excited state '',I3,                             ''
     &has got zero excitation energy'')') ilv
              ENDIF

              IF (t12.ge.TISomer) ISIsom(ilv,Nnuc) = 1

              IF (ndbrlin.GT.NDBR) THEN
                WRITE (8,'('' WARNING:'')')
                WRITE (8,'('' WARNING: Element ='',A5,2x,2HZ=,I3)')
     &                      chelem, izr
                WRITE (8,
     &'('' WARNING: too many gamma decays ='',                       I3)
     &') ndbrlin
                WRITE (8,
     &'('' WARNING: Dimension allows for ='',                        I3)
     &') NDBR
                WRITE (8,'('' WARNING: some gammas discarded'')')
              ENDIF
C-------------clean BR matrix
              DO nbr = 1, NDBR
                BR(ilv,nbr,1,Nnuc) = 0.
                BR(ilv,nbr,2,Nnuc) = 0.
                BR(ilv,nbr,3,Nnuc) = 0.
              ENDDO
              ndb = MIN(ndbrlin,NDBR)
              sum = 0.d0
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
                  BR(ilv,isum,3,Nnuc) = xicc      !int. conversion coeff.
                ENDIF
              ENDDO
              IF (sum.NE.1.D0 .AND. sum.NE.0.D0) THEN
                sum = 1.D0/sum
                DO nbr = 1, isum
                  BR(ilv,nbr,2,Nnuc) = BR(ilv,nbr,2,Nnuc)*sum
                ENDDO
              ENDIF
            ENDIF
          ENDDO  ! end of loop over levels
        ENDIF
      ENDIF
  200 IF(.NOT.ADDnuc) THEN
        IF (.NOT.FILevel) CLOSE (13)
      ELSE
        CLOSE(13)
        CLOSE(14)
        IF (IOPsys.EQ.0) THEN
          ctmp = 'cat LEVELS LEVELS.ADD>LEVELS.TMP'
          iwin = PIPE(ctmp)
          ctmp = 'mv LEVELS.TMP LEVELS'
          iwin = PIPE(ctmp)
          ctmp = 'rm LEVELS.ADD'
          iwin = PIPE(ctmp)
        ELSE
          iwin = PIPE('copy LEVELS+LEVELS.ADD LEVELS.TMP>nul')
          iwin = PIPE('move LEVELS.TMP LEVELS>nul')
          iwin = PIPE('del LEVELS.ADD>nul')
        ENDIF
        OPEN (UNIT = 13,FILE='LEVELS', STATUS='OLD')
        FILevel = .TRUE.
      ENDIF
      RETURN

  300 IF(FILevel .AND. (.NOT.ADDnuc)) THEN
        IF(FIRst_ein) WRITE (8,
     &  '('' WARNING: Levels for nucleus A='',I3,'' Z='',I3,
     &  '' not found in local file (.lev). Default RIPL levels will be
     & used'')') ia, iz
        CLOSE(13)
        WRITE (ctmp3,'(I3.3)') iz
        finp = 'z'//ctmp3//'.dat'
        OPEN (13,FILE = trim(empiredir)//'/RIPL/levels/'//finp
     &      ,STATUS = 'OLD',ERR = 400)
        CLOSE(14)
        OPEN (UNIT = 14, FILE='LEVELS.ADD')
        ADDnuc = .TRUE.
        GOTO 100
      ENDIF
      IF(FIRst_ein) WRITE (8,
     &  '('' WARNING: levels for nucleus A='',I3,'' Z='',I3,
     &  '' not found in the RIPL database '')') ia, iz

      IF(ADDnuc) THEN
        CLOSE(13)
        CLOSE(14,STATUS='DELETE')
        IF (FILevel) OPEN (UNIT = 13,FILE='LEVELS', STATUS='OLD')
      ENDIF
      RETURN

  400 WRITE (8,'('' WARNING: RIPL levels database not found '')')
      IF (.NOT.FILevel) CLOSE (13)
      RETURN
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
      INTEGER i, j, ia, iae, iexclus, izares, nejc, nnur
      REAL*8 zres, ares

      IF (FIRst_ein) THEN
        WRITE (12,*) ' '
        IF (KTRompcc.GT.0 .AND. DIRect.GT.0) WRITE (12,*)
     &   'Inelastic o. m. parameters: RIPL catalog number ', KTRompcc
        DO nejc = 1, NEJcm
          ares = A(1) - AEJc(nejc)
          zres = Z(1) - ZEJc(nejc)
C         residual nuclei must be heavier than alpha
          if(ares.le.4 . or. zres.le.2) cycle
          izares = INT(1000*zres + ares)
          CALL WHERE(izares,nnur,iloc)
          IF (iloc.EQ.1) THEN
            WRITE(8,'(''ERROR: NO LOCATION ASCRIBED TO NUCLEUS '',I8)')
     &                izares
            STOP ' FATAL: in PRINPUT: NUCLEUS NOT FOUND' 
          ENDIF
          IF(nejc.eq.1) WRITE (12,*) 
     &      'Neutron   o. m. parameters: RIPL catalog number ',
     &      KTRlom(nejc,nnur)
 	    IF(nejc.eq.2) WRITE (12,*) 
     &      'Proton    o. m. parameters: RIPL catalog number ',
     &       KTRlom(nejc,nnur)
 	    IF(nejc.eq.3) THEN
C            Special case, 9602 RIPL OMP number is used for Kumar & Kailas OMP
             if(ABS(KTRlom(nejc,nnur)).ne.9602) then
               WRITE (12,*) 
     &          'Alpha     o. m. parameters: RIPL catalog number ',
     &           ABS(KTRlom(nejc,nnur))
             else
               WRITE (12,*) 
     &          'Alpha     o. m. parameters: Kumar & Kailas 2007 '
             endif
          ENDIF
 	    IF(nejc.eq.4) WRITE (12,*) 
     &      'Deuteron  o. m. parameters: RIPL catalog number ',
     &       KTRlom(nejc,nnur)
 	    IF(nejc.eq.5) WRITE (12,*) 
     &      'Triton    o. m. parameters: RIPL catalog number ',
     &       KTRlom(nejc,nnur)
 	    IF(nejc.eq.6) WRITE (12,*) 
     &      'He-3      o. m. parameters: RIPL catalog number ',
     &       KTRlom(nejc,nnur)
          IF (NEMc.GT.0) WRITE (12,*)
     &      'Cluster   o. m. parameters: RIPL catalog number ',
     &       KTRlom(NDEJC,nnur)
	  ENDDO

        WRITE (12,*)
        WRITE (12,99060)
        WRITE (12,*)
        WRITE (12,99007) (IFIX(SNGL(AEJc(i))),SYMbe(i),i = 1,NEJcm)
        WRITE (12,*)
        WRITE (12,99012) (EJMass(j),j = 1,NEJcm)
        WRITE (12,*)
        WRITE (12,99050)
        WRITE (12,*)
        WRITE (12,99005) (IFIX(SNGL(AEJc(i))),SYMbe(i),i = 1,NEJcm)
        WRITE (12,*)

      ENDIF

99005 FORMAT ('    Nucleus   ',12(6X,I2,A2))
99007 FORMAT ('              ',12(6X,I2,A2))
99010 FORMAT (1X,I3,'-',A2,'-',I3,4X,12F10.3)
99015 FORMAT (1X,I3,'-',A2,'-',I3,2X,'<',1x,12F10.3)
99012 FORMAT (1X,10x,4X,12(F10.6,1x))

99045 FORMAT(1X,I3,'-',A2,'-',I3,4X,10F12.3)
99050 FORMAT(25x,'B i n d i n g    e n e r g i e s [MeV]')
99060 FORMAT(25x,'E j e c t i l e    m a s s e s   [amu]')

      WRITE (8,*)
      WRITE (8,99060)
      WRITE (8,*)
      WRITE (8,99007) (IFIX(SNGL(AEJc(i))),SYMbe(i),i = 1,NEJcm)
      WRITE (8,*)
      WRITE (8,99012) (EJMass(j),j = 1,NEJcm)
      WRITE (8,*)
      WRITE (8,*)
      WRITE (8,99050)
      WRITE (8,*)
      WRITE (8,99005) (IFIX(SNGL(AEJc(i))),SYMbe(i),i = 1,NEJcm)
      WRITE (8,*)

      iexclus = 0
      DO i = 1, NNUcd
        IF(ENDf(i).GT.0) THEN
          IF (ENDf(i).EQ.1)
     &      WRITE (8,99010) IFIX(SNGL(Z(i))),SYMb(i),
     &                   IFIX(SNGL(A(i))), (Q(j,i),j = 1,NEJcm)
          IF (ENDf(i).EQ.2) THEN
            iexclus = 1
            WRITE (8,99015) IFIX(SNGL(Z(i))),SYMb(i),
     &                   IFIX(SNGL(A(i))), (Q(j,i),j = 1,NEJcm)
          ENDIF
        ELSE
          WRITE (8,99010) IFIX(SNGL(Z(i))),SYMb(i),
     &                   IFIX(SNGL(A(i))), (Q(j,i),j = 1,NEJcm)
        ENDIF
      ENDDO

      IF (iexclus.EQ.1) THEN
        WRITE(8,*)
        WRITE(8,*) ' < indicates inclusive spectra only'
      ENDIF

      IF (FIRst_ein) THEN
        DO i = 1, NNUcd
          IF(ENDf(i).GT.0) THEN
            IF (ENDf(i).EQ.1)
     &        WRITE (12,99010) IFIX(SNGL(Z(i))),SYMb(i),
     &                   IFIX(SNGL(A(i))), (Q(j,i),j = 1,NEJcm)
            IF (ENDf(i).EQ.2)
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
        WRITE (12,*) 'RESULTS:                                        '
        IF(FISspe.GT.0 .and. NUBarread) THEN
        WRITE (12,*) 'MF=1                                            '                
        WRITE (12,*) '   MT=456 nubar adopted from evaluated libraries'
        ENDIF
        WRITE (12,*) '                                                '
        WRITE (12,*) 'MF=3 Neutron cross sections                     '
        WRITE (12,*) '     EMPIRE calculations were adopted for:      '
        WRITE (12,*) '                                                '
        WRITE (12,*) '   MT=1 Total                                   '
        WRITE (12,*) '   MT=2 Elastic scattering                      '
        WRITE (12,*) '   MT=4, 51-91 Inelastic scattering             '
        WRITE (12,*) '   MT=102 Capture                               '
        WRITE (12,*) '   MT=16   (n,2n)                               '
        WRITE (12,*) '   MT=17   (n,3n)                               '
        WRITE (12,*) '   MT=18   (n,f)                                '
        WRITE (12,*) '   MT=22   (n,na)                               '
        WRITE (12,*) '   MT=24   (n,2na)                              '
        WRITE (12,*) '   MT=28   (n,np+pn)                            '
        WRITE (12,*) '   MT=37   (n,4n)                               '
        WRITE (12,*) '   MT=45   (n,npa)                              '
        WRITE (12,*) '   MT=103, 600-649 (n,p)                        '
        WRITE (12,*) '   MT=104, (n,d)                                '
        WRITE (12,*) '   MT=105, (n,t)                                '
        WRITE (12,*) '   MT=106, (n,He-3)                             '
        WRITE (12,*) '   MT=107, 800-849 (n,a)                        '
        WRITE (12,*) '   MT=112  (n,pa)                               '
        WRITE (12,*) '                                                '
        WRITE (12,*) 'MF=4 Angular distributions                      '
        WRITE (12,*) '     EMPIRE calculations were adopted           '
        WRITE (12,*) '                                                '
        IF(FISspe.GT.0) THEN
        WRITE (12,*) 'MF=5 Energy distributions                       '
        WRITE (12,*) '   MT=18 PFNS: EMPIRE calculations using        '
        IF(FISspe.eq.1) 
     >  WRITE (12,*) '           Madland-Nix (Los Alamos) model  [MN] '
        IF(FISspe.eq.2)
     >  WRITE (12,*) '           Kornilov et al parameterization [KO] '
        ENDIF
        WRITE (12,*) '                                                '
        WRITE (12,*) 'MF=6 Energy-angle distributions of reaction     '
        WRITE (12,*) '     products; EMPIRE calculations were adopted '
        WRITE (12,*) '                                                '
        WRITE (12,*) '     Primary capture gammas are entered as      '
        WRITE (12,*) '     discrete lines                             '
        WRITE (12,*) '                                                '
        WRITE (12,*) 'MF=12 Transition probability arrays for photon  '
        WRITE (12,*) '      production; taken from the RIPL library   '
        WRITE (12,*) '                                                '
        WRITE (12,*) 'MF=14 Photon angular distributions              '
        WRITE (12,*) '      isotropic distributions were assumed      '
        WRITE (12,*) '                                                '
        WRITE (12,*) 'REFERENCES                                      '
        WRITE (12,*) '                                                '
        WRITE (12,*) '[EMP]                                           '
        WRITE (12,*) '  M.Herman, R.Capote, B.Carlson, P.Oblozinsky,  '
        WRITE (12,*) '  M.Sin, A.Trkov, H.Wienke and V.Zerkin         '
        WRITE (12,*) '                                                '
        WRITE (12,*) ' "EMPIRE: Nuclear Reaction Model Code System    '
        WRITE (12,*) '           for data evaluation"                 '
        WRITE (12,*) '  Nuclear Data Sheets 108 (2007) 2655-2715      '
        WRITE (12,*) '                                                '
        WRITE (12,*) '[EMP-man]                                       '
        WRITE (12,*) '  M.Herman, R.Capote, A.Trkov, M.Sin, B.Carlson,'
        WRITE (12,*) '  P.Oblozinsky, C.Mattoon, H.Wienke, S. Hoblit, '
        WRITE (12,*) '  Young-Sik Cho, V. Plujko and V.Zerkin         '
        WRITE (12,*) '                                                '
        WRITE (12,*) ' "EMPIRE: Modular system for nuclear reaction   '
        WRITE (12,*) '     calculations and nuclear data evaluation", '
        WRITE (12,*) ' Users'' manual, to be published as IAEA(Vienna)'
        WRITE (12,*) '  and BNL(Upton,NY) technical reports, 2012     ' 
        WRITE (12,*) '                                                '
        WRITE (12,*) '[RIPL]                                          '
        WRITE (12,*) '  R.Capote, M.Herman, P.Oblozinsky, P.G.Young,  '
        WRITE (12,*) '  S.Goriely, T.Belgya, A.V.Ignatyuk, A.J.Koning,'
        WRITE (12,*) '  S.Hilaire, V.A.Plujko, M.Avrigeanu,           '
        WRITE (12,*) '  Zhigang Ge, Yinlu Han, S.Kailas, J.Kopecky,   '
        WRITE (12,*) '  V.M.Maslov, G.Reffo, M.Sin,                   '
        WRITE (12,*) '  E.Sh.Soukhovitskii and P. Talou               '
        WRITE (12,*) '                                                '
        WRITE (12,*) ' "RIPL - Reference Input Parameter Library for  '
        WRITE (12,*) '         Calculation of Nuclear Reactions and   '          
        WRITE (12,*) '         Nuclear Data Evaluations",             '
        WRITE (12,*) '                                                '
        WRITE (12,*) '  Nuclear Data Sheets 110 (2009) 3107-3214      '
        WRITE (12,*) '                                                '
        WRITE (12,*) '  Data available online at                      '
        WRITE (12,*) '   http://www-nds.iaea.org/RIPL-3/              '
        WRITE (12,*) '                                                '
        WRITE (12,*) '[MN] D.G.Madland and J.R.Nix,                   '
        WRITE (12,*) '     Nuc. Sci. Eng. 81, (1982) 213              '
        WRITE (12,*) '                                                '
        WRITE (12,*) '[KK] N.V.Kornilov, A.B.Kagalenko, F.-J.Hambsch  '
        WRITE (12,*) '     Phys. At. Nuclei 62 (1999) pp 173-185      '
        WRITE (12,*) '                                                '
        WRITE (12,*) '************************************************'
        WRITE (12,*)
        WRITE (12,*)

        WRITE (8,*)
        WRITE (8,*)

      ENDIF

      IF (FISshi(1).NE.0) THEN
         WRITE (8,99025)
99025    FORMAT ('    Nucleus   ',6X,'Shell Corr.  Deform.',
     &           '  Fiss. barr.')
         WRITE (8,99030)
99030    FORMAT ('              ',6X,'  (J=0)       (J=0)    ',
     &           '    (J=0)')
         WRITE (8,*)
C        DO i = 1, NNUcd
         DO i = 1, NNUct
            IF (EMAx(i).NE.0.0D0) WRITE (8,99045) IFIX(SNGL(Z(i))),
     &          SYMb(i), IFIX(SNGL(A(i))), SHC(i), DEF(1,i), FISb(1,i)
         ENDDO
      ELSE
         WRITE (8,99035)
99035    FORMAT ('    Nucleus   ',6X,'Shell Corr.  Deform.')
         WRITE (8,99040)
99040    FORMAT ('              ',6X,'  (J=0)       (J=0)    ')
         WRITE (8,*)
         DO i = 1, NNUct
            IF (EMAx(i).NE.0.0D0) WRITE (8,99045) IFIX(SNGL(Z(i))),
     &          SYMb(i), IFIX(SNGL(A(i))), SHC(i), DEF(1,i)
         ENDDO
      ENDIF

      IF (FIRst_ein) THEN
        WRITE (8,*)
        IF (KTRompcc.GT.0 .AND. DIRect.GT.0) WRITE (8,*)
     &   'Inelastic o. m. parameters: RIPL catalog number ', KTRompcc
        IF (KTRlom(0,0).NE.KTRompcc .AND. DIRect.GT.0) WRITE (8,*)
     &   'WARNING: The inelastic OMP is not equal to incident OMP ', 
     &     KTRlom(0,0)
        DO nejc = 1, NEJcm
          ares = A(1) - AEJc(nejc)
          zres = Z(1) - ZEJc(nejc)
C         residual nuclei must be heavier than alpha
          if(ares.le.4 . or. zres.le.2) cycle
          izares = INT(1000*zres + ares)
          CALL WHERE(izares,nnur,iloc)
          IF (iloc.EQ.1) THEN
            WRITE(8,'(''ERROR: NO LOCATION ASCRIBED TO NUCLEUS '',I8)')
     &                izares
            STOP ' FATAL: in PRINPUT: NUCLEUS NOT FOUND' 
          ENDIF
          IF(nejc.eq.1) WRITE (8,*) 
     &      'Neutron   o. m. parameters: RIPL catalog number ',
     &      KTRlom(nejc,nnur)
 	    IF(nejc.eq.2) WRITE (8,*) 
     &      'Proton    o. m. parameters: RIPL catalog number ',
     &       KTRlom(nejc,nnur)
 	    IF(nejc.eq.3) THEN
C            Special case, 9602 RIPL OMP number is used for Kumar & Kailas OMP
             if(ABS(KTRlom(nejc,nnur)).ne.9602) then
               WRITE (8,*) 
     &          'Alpha     o. m. parameters: RIPL catalog number ',
     &           ABS(KTRlom(nejc,nnur))
             else
               WRITE (8,*) 
     &          'Alpha     o. m. parameters: Kumar & Kailas 2007 '
             endif
          ENDIF
 	    IF(nejc.eq.4) WRITE (8,*) 
     &      'Deuteron  o. m. parameters: RIPL catalog number ',
     &       KTRlom(nejc,nnur)
 	    IF(nejc.eq.5) WRITE (8,*) 
     &      'Triton    o. m. parameters: RIPL catalog number ',
     &       KTRlom(nejc,nnur)
 	    IF(nejc.eq.6) WRITE (8,*) 
     &      'He-3      o. m. parameters: RIPL catalog number ',
     &       KTRlom(nejc,nnur)
          IF (NEMc.GT.0) WRITE (8,*)
     &      'Cluster   o. m. parameters: RIPL catalog number ',
     &       KTRlom(NDEJC,nnur)
	  ENDDO
      ENDIF  
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
Ccc   *  Reads from RIPL OM-DEFORMATIONS.DAT energies of collective 2+ *
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
      CHARACTER*3 ctmp3
      CHARACTER*9 finp
      INTEGER i, iar, ilv, ilvr, iptmp, itmp2, izr, lvpr, natmp,
     &        ndbrlin, ngamr, nlvr, nmax, nztmp
      CHARACTER*6 reftmp
      E2p = 0.D0
      E3m = 0.D0
C-----Avoiding searching of collective levels of the incident particle
      IF (Ia.EQ.AEJc(0) .and. Ia.le.4) GOTO 300
C-----First try to find 2+ and 3- states in the RIPL om-deformations file
      OPEN (47,FILE = trim(empiredir)//'/RIPL/optical/om-data'
     &      //'/om-deformations.dat',STATUS = 'old',ERR = 100)
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
  100 WRITE (8,*) ' WARNING: ',trim(empiredir)//
     &   '/RIPL/optical/om-data/om-deformations.dat not found '
      WRITE (8,*) ' WARNING: ',
     &'E(2+) and E(3-) will be selected from the available target level
     &scheme'
      GOTO 300
  200 CLOSE (47)
C
C-----If missing in the RIPL om-deformations file try discrete levels file
C-----constructing input and filenames
  300 WRITE (ctmp3,'(I3.3)') Iz
      finp = 'z'//ctmp3//'.dat'
      OPEN (32,FILE = trim(empiredir)//'/RIPL/levels/'//finp
     &      ,STATUS = 'OLD', ERR = 500)
  400 READ (32,'(A5,6I5,2f12.6)',END = 500) chelem, iar, izr, nlvr,
     &      ngamr, nmax, itmp2
      IF (Ia.NE.iar .OR. Iz.NE.izr) THEN
         DO ilv = 1, nlvr + ngamr
            READ (32,'(A1)',END = 500) dum
         ENDDO
         GOTO 400
      ELSE
         DO ilv = 1, nlvr
            READ (32,'(I3,1X,F10.6,1X,F5.1,I3,1X,E10.2,I3)') ilvr, elvr,
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
      CLOSE (32)
      RETURN
  500 Gspin = 0.
      IF (Ia.NE.2*(Ia/2)) Gspin = 0.5
      Gspar = 1
      WRITE (8,
     &'('' LEVELS FOR NUCLEUS A='',I3,'' Z='',I3,'' NOT FOUND IN THE RIP
     &L DATABASE'')') Ia, Iz
      WRITE (8,
     & '('' JUST TO BE SURE I SET G.S. PARITY TO + AND SPIN TO:'',F5.1)'
     & ) Gspin


      RETURN
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
      DOUBLE PRECISION ALSin, CNOrin(8,8), EFItin(8,8), GAPin(2), HOMin,
     &                 WIDexin, BET2in, GRIn(2)
      INTEGER*4 INDexf, INDexb, BUFfer(250)
      COMMON /R250COM/ INDexf,INDexb,BUFfer
      COMMON /TRINP / WIDexin, GAPin, HOMin, ALSin, EFItin, CNOrin,
     &                BET2in, GRIn
C
C Local variables
C
      DOUBLE PRECISION GRAND,DRAND
      CHARACTER*40 fstring
      INTEGER i, i1, i2, i3, i4, ieof, iloc, ipoten, izar, ki, nnuc,irun
      INTEGER IPArCOV
      INTEGER INT
      CHARACTER*5 source_rev, emp_rev
      CHARACTER*6 name, namee, emp_nam, emp_ver
      CHARACTER*35 char
      CHARACTER*13 char1
      LOGICAL fexist
      DOUBLE PRECISION val,vale,sigma,shelss,quant,ecutof
C-----initialization of TRISTAN input parameters
      WIDexin = 0.2d0
      GAPin(1) = 0.d0
      GAPin(2) = 0.d0
      HOMin = 0.d0
      ALSin = 1.5d0
      BET2in = 0.d0
      GRIn(1) = 5.d0
      GRIn(2) = 5.d0
      DO i = 1, 8                                                        ! nilsson_newest
         do j = 1,8                                                      ! nilsson_newest
            CNOrin(i,j) = 1.d0                                           ! nilsson_newest
            EFItin(i,j) = 0.0D0                                          ! nilsson_newest
         enddo                                                           ! nilsson_newest
      ENDDO
C-----initialization of TRISTAN input parameters  *** done ***
C
C      By default, no covariance calculation is done
C
      IPArCOV = 0
C     Moved to io.h
C     OPEN(95,FILE='COVAR.DAT',STATUS='UNKNOWN')
C-----Go to the end of the COVAR.DAT file
C  10 READ(95,*,END=11) dum
C     GOTO 10
   11 CONTINUE

      WRITE (8,*)'                        __________________________'
      WRITE (8,*)'                       |                          |'
      open(200,file=trim(empiredir)//"/version",status='OLD',
     >    ERR=753)
C     VERSIONNUMBER = 3.1
C     VERSIONNAME   = RIVOLI
      read(200,'(16x,A5)',ERR=753,END=753) emp_ver
      read(200,'(16x,A6)',ERR=753,END=753) emp_nam
      close(200)

      WRITE(8,'(A44,A5,A3)') 
     > '                        |    E M P I R E  -  ',emp_ver ,'  |'
      WRITE (8,*)
     > '                       |                          |'
      WRITE (8,'(A33,A6,A13)')
     > '                        |          ',emp_nam,'            |'
      WRITE (8,*)
     > '                       |                          |'
      GOTO 754
  753	WRITE (8,*)
     > '                       |    E M P I R E  -  3.1   |'
      WRITE (8,*)
     > '                       |                          |'
      WRITE (8,*)
     > '                       |          Rivoli          |'
C
  754 open(200,file=trim(empiredir)//"/source/.svn/entries",
     &status='OLD',ERR=755)
      read(200,'(3/,A5,7/,A5)',ERR=755,END=755) emp_rev,source_rev
      close(200)
      WRITE(8,20) emp_rev
   20 FORMAT(24X,'| SVN empire     rev. ',A5,'|')
      WRITE(8,30) source_rev
   30 FORMAT(24X,'| empire/source  rev. ',A5,'|')
      GOTO 756
  755	emp_rev = "     "
      WRITE (8,*)'                       |       Not under SVN      |'
C
  756 WRITE (8,*)'                       |                          |'
      WRITE (8,*)'                       |    Sao Jose dos Campos   |'
      WRITE (8,*)'                       |     Brazil, May 2011     |'
      WRITE (8,*)'                       |    Upton, New York       |'
      WRITE (8,*)'                       |      USA, Jan 2012       |'
      WRITE (8,*)'                       |__________________________|'
      WRITE (8,*) ' '
      WRITE (8,*) ' '
      WRITE (8,*) 'Following options/parameters have been used'
      WRITE (8,*) '-------------------------------------------'
      WRITE (8,*) ' '
      WRITE (12,*) '***************************************************'
      WRITE (12,*) 'FAST ENERGY REGION'
      WRITE (12,*) 'Authors:'
      WRITE (12,*) '_________________________________'
      WRITE (12,*) ''
      WRITE (12,*) 'Nuclear reaction model code EMPIRE-',
     > trim(emp_ver), ' ',trim(emp_nam)

      if(emp_rev(1:5).ne."     ") then
        WRITE (12,35) trim(emp_rev)
   35 FORMAT(1X,'(SVN revision ',A,') by M. Herman et al [EMP].',5X,' ')
      else
        WRITE (12,*)
     >             '(not under SVN) by M. Herman et al [EMP].          '
      endif
      WRITE (12,*) ''
      WRITE (12,*) 'CROSS-SECTION EVALUATION PROCEDURE                 '
      WRITE (12,*) ''
      WRITE (12,*) 'Adopted procedure is based on careful theoretical  '
      WRITE (12,*) 'analysis utilizing available experimental data and '
      WRITE (12,*) 'nuclear reaction model calculations.               '
      WRITE (12,*) 
      WRITE (12,*) 'This code integrates into a single system a number '
      WRITE (12,*) 'of important modules and features:                 '
      WRITE (12,*) '                                                   '
      WRITE (12,*) '- Spherical and deformed Optical Model including   '
      WRITE (12,*) '  coupled-channels code ECIS06 by J. Raynal        '
      WRITE (12,*) '- Soft-rotator deformed Optical Model including    '
      WRITE (12,*) '  coupled-channels code OPTMAN by E.Soukhovitskii  '
      WRITE (12,*) '  and coworkers                                    '
      WRITE (12,*) '- Hauser-Feshbach statistical model including      '
      WRITE (12,*) '  HRTW width fluctuation correction, and the       '
      WRITE (12,*) '  optical model for fission with partial damping   '
      WRITE (12,*) '- Quantum-mechanical MSD TUL model (codes ORION &  '
      WRITE (12,*) '  TRISTAN by H.Lenske), and MSC NVWY model         '
      WRITE (12,*) '- Exciton model with Iwamoto-Harada cluster        '
      WRITE (12,*) '  emission and Kalbach systematic angular distr.   '
      WRITE (12,*) '  (code PCROSS by R.Capote et al)                  '
      WRITE (12,*) '- Hybrid Monte Carlo preequilibrium model(M.Blann) '
      WRITE (12,*) '  (code DDHMS by M.Chadwick, mod. by B.V.Carlson)  '
      WRITE (12,*) '- Complete gamma-ray cascade after emission of     '
      WRITE (12,*) '  each particle, including realistic treatment of  '
      WRITE (12,*) '  discrete transitions                             '
      WRITE (12,*) '- Access to OM segment of the RIPL library [RIPL]  '
      WRITE (12,*) '- Built-in input parameter files, such as masses,  '
      WRITE (12,*) '  level density, discrete levels, fission barriers '
      WRITE (12,*) '  and gamma strength functions based on the RIPL   '
      WRITE (12,*) '  library [RIPL]                                   '
      WRITE (12,*) '- Automatic retrieval of experimental data from the'
      WRITE (12,*) '  EXFOR/CSISRS library                             '
      WRITE (12,*) '- ENDF-6 formatting (code EMPEND by A.Trkov)       '
      WRITE (12,*) '  coupled to graphical presentation capabilities   '
      WRITE (12,*) '  (code ZVView by V. Zerkin) through the chain of  '
      WRITE (12,*) '  PrePro codes by D. Cullen                        '
      WRITE (12,*) '- ENDF checking codes (CHECKR, FIZCON, PSYCHE)     '
      WRITE (12,*) '- Support for NJOY                                 '
      WRITE (12,*) '                                                   '
      WRITE (12,*) 'PARAMETERIZATIONS                                  '
      WRITE (12,*) '                                                   '
      WRITE (12,*) 'Following models and parameters were used in the   '
      WRITE (12,*) 'current evaluation:                                '
      WRITE (12,*) '                                                   '
      WRITE (12,*) 'Discrete levels were taken from the RIPL-3 level   '
      WRITE (12,*) 'file, based on the 2007 version of ENSDF.          '
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
            IF (KEY_shape.EQ.0) WRITE (8,
     &          '('' E1 strength function set to EGLO (EMPIRE-2.18)'')')
            IF (KEY_shape.EQ.1) WRITE (8,
     &                        '('' E1 strength function set to MLO1'')')
            IF (KEY_shape.EQ.2) WRITE (8,
     &                        '('' E1 strength function set to MLO2'')')
            IF (KEY_shape.EQ.3) WRITE (8,
     &                        '('' E1 strength function set to MLO3'')')
            IF (KEY_shape.EQ.4) WRITE (8,
     &                        '('' E1 strength function set to EGLO'')')
            IF (KEY_shape.EQ.5) WRITE (8,
     &                        '('' E1 strength function set to GFL'')')
            IF (KEY_shape.EQ.6) WRITE (8,
     &                   '('' E1 strength shape function set to SLO'')')
            IF(Key_gdrgfl.EQ.0.AND.Key_shape.EQ.0)WRITE(8,
     &         '('' GDR parameters from Messina systematics'')')
            IF(Key_gdrgfl.EQ.0.AND.Key_shape.NE.0)WRITE(8,
     &         '('' GDR parameters from Plujko systematics(RIPL)'')')
            IF(Key_gdrgfl.EQ.1)WRITE(8,
     &         '('' GDR parameters from RIPL/Exp.data+'',
     &           ''Plujko systematics'')')
            IF(Key_gdrgfl.EQ.2)WRITE(8,
     &          '('' GDR parameters from RIPL/Exp.data+'',
     &           ''Goriely calc.'')')
C-----   print  maximal gamma-ray multipolarity  'MAXmult'
            IF(MAXmult.GT.2)WRITE(8,
     &      '('' Gamma-transition multipolarity set to '',I4)')MAXmult

            WRITE (8,*) ' '
            IF (OMPar_riplf .OR. OMParfcc) THEN
               WRITE (8,*) 'Existing, case specific, o.m.p. files: '
               WRITE (8,*) '-------------------------------------'
            ENDIF
            IF (OMPar_riplf) WRITE (8,
     &'('' Input file OMPAR.RIPL with RIPL optical model'',
     &'' parameters '')')
            IF (OMParfcc .AND. (DIRect.EQ.1 .OR. DIRect.EQ.3)) WRITE (8,
     &'('' Input file OMPAR.DIR with optical model'',
     &'' parameters to be used in inelastic scattering'')')
C
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
     &         '('' GDR parameters from RIPL/Plujko systematics'')')
            IF(Key_gdrgfl.EQ.1)WRITE(12,
     &         '('' GDR parameters from RIPL/Exp.data+'',
     &           ''Plujko systematics'')')
            IF(Key_gdrgfl.EQ.2)WRITE(12,
     &          '('' GDR parameters from RIPL/Exp.data+'',
     &           ''Goriely calc.'')')
C-----      print  maximal gamma-ray multipolarity  'MAXmult'
            IF(MAXmult.GT.2)WRITE(12,
     &      '('' Gamma-transition multipolarity set to '',I4)')MAXmult

            IF (DIRect.EQ.0 .AND. KTRompcc.NE.0) THEN
               WRITE (8, '(1X,/,
     &'' WARNING: No direct calculations have been selected (DIRECT=0)''
     &,/, '' WARNING:   but DIRPOT keyword is specified.'',/,
     &    '' WARNING: Set DIRECT > 0 in the input file'',/,
     &    '' WARNING:   to include direct contribution.'')')
               KTRompcc = 0
            ENDIF
            WRITE (8,*) ' '
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

C--------PCROSS input
         IF (name.EQ.'PCROSS') THEN
            PEQc = 0.
            IF (val.GE.0.8 .AND. val.LE.3.D0) THEN
              PEQc = 1.
              MFPp = val
              WRITE (8,
     &'('' Exciton model calculations with code PCROSS'',/,
     &  '' Cluster emission in terms of the Iwamoto-Harada model'',/
     &  '' Kalbach systematics angular distributions (see RIPL-1)'')')
              WRITE (12,
     &'('' Exciton model calculations with code PCROSS'',/,
     &  '' Cluster emission in terms of the Iwamoto-Harada model'',/
     &  '' Kalbach systematics angular distributions (see RIPL-1)'')')
              if(i1.ne.0 .and. IOPran.ne.0) then
                WRITE (8,
     &          '('' Mean free path parameter uncertainty '',
     &          '' is equal to '',i2,'' %'')') i1
                 sigma = val*i1*0.01
                IF(IOPran.gt.0) then
                  MFPp = val + grand()*sigma
                ELSE
                  MFPp = val + 1.732d0*(2*drand()-1.)*sigma
                ENDIF
                WRITE (8,
     &          '('' Mean free path parameter sampled value : '',f5.2)')
     &          MFPp
                 IPArCOV = IPArCOV +1
                 write(95,'(1x,i5,1x,d12.6,1x,2i13)')
     &              IPArCOV, MFPp, INDexf,INDexb
              else
                WRITE (8,
     &'('' Mean free path parameter in PCROSS set to '',F4.1,
     &  '' (Recommended ~ 1.5)'')') MFPp
                WRITE (12,
     &'('' Mean free path parameter in PCROSS set to '',F4.1,
     &  '' (Recommended ~ 1.5)'')') MFPp
               endif
             ENDIF
            GOTO 100
         ENDIF
C
         IF (name.EQ.'PEDISC') THEN
            PEQcont = 0
            IF (val.GE.0.1) THEN
              PEQcont = 1
              WRITE (8,
     &'('' Discrete levels included in PCROSS calculations'')')
              WRITE (12,
     &'('' Discrete levels included in PCROSS calculations'')')
             ELSE
              WRITE (8,
     &'('' Discrete levels not included in PCROSS calculations'')')
              WRITE (12,
     &'('' Discrete levels not included in PCROSS calculations'')')
             ENDIF
            GOTO 100
         ENDIF
C
         IF (name.EQ.'PEPAIR') THEN
            IF (val.LE.0) THEN
              Npairpe = 0
              WRITE (8,
     &'('' Pairing corrections are not considered in PCROSS calculations
     &'')')
              WRITE (12,
     &'('' Pairing corrections are not considered in PCROSS calculations
     &'')')
             ELSE
              WRITE (8,
     &'('' Pairing corrections are considered in exciton model LDs'')')
              WRITE (12,
     &'('' Pairing corrections are considered in exciton model LDs'')')
             ENDIF
            GOTO 100
         ENDIF
C
         IF (name.EQ.'MAXHOL') THEN
            CHMax = 0.54
            IF (val.GE.0.1 .AND. val.LE.1.5D0) THEN
              CHMax = val
              WRITE (8,
     &'('' Max hole number in PCROSS set to '',F4.2,
     &        ''*sqrt(g*U)'')') CHMax
              WRITE (8,
     &'(''    being U the CN excitation energy '')')
              WRITE (12,
     &'('' Max hole number in PCROSS set to '',F4.2,
     &        ''*sqrt(g*U)'')') CHMax
              WRITE (12,
     &'(''    being U the CN excitation energy '')')
             ENDIF
            GOTO 100
         ENDIF
C
C--------ECIS input
C
         IF (name.EQ.'EcDWBA') THEN
C           EcDWBA meaningless if Collective level file exists
            INQUIRE (FILE = 'TARGET_COLL.DAT',EXIST = fexist)
            IF(fexist) then
              WRITE(8,*) 
     &        ' WARNING: Collective levels for DWBA calculations  '
              WRITE(8,*) 
     &        ' WARNING:  can not be automatically selected if the'
              WRITE(8,*) 
     &        ' WARNING:  collective level file *-lev.col exists !'
              goto 100
            ENDIF
            ECUtcoll = val
            JCUtcoll = i1
            ecutof = 2.*30./A(0)**0.666666666666d0
            IF (ECUtcoll.LT.0.1 .or. ECUtcoll.GT.ecutof) ECUtcoll=ecutof
            IF (JCUtcoll.EQ.0 .or. JCUTcoll.GT.8) JCUtcoll = 4
C
            WRITE (8,
     &     '('' Collective levels up to '',F5.1,'' MeV used in DWBA'' )'
     &     ) ECUtcoll
            WRITE (8,
     &'('' All levels with spin less or equal to '',I1,           '' con
     &sidered in DWBA'')') JCUtcoll
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'OMPOT ') THEN
            IF (i1.LT.1 .OR. i1.GT.NEJcm) THEN
               WRITE (8,
     &                '('' WARNING: EJECTILE IDENTIFICATION '',I2,
     &                  '' UNKNOWN'')') i1
               WRITE (8,'('' WARNING: OPTICAL MODEL SETTING IGNORED'')')
               GOTO 100
            ENDIF
C-----
! historically val and -val meant different potentials. The '+val'
! potentials are no longer in EMPIRE, so no need to treat differently:
            ki = 26 ! file id for 'om-parameter-u.dat' in RIPL
            ipoten = ABS(val)
C--------------Searching in the RIPL database for i1 catalog number
            CALL FINDPOT(ki,ieof,ipoten)
            IF (ieof.NE.0) THEN
                WRITE (8,*) 'Requested RIPL entry ', ipoten,
     &                        ' not found, using default choice'
                GOTO 100
            ENDIF
            if(ipoten.ne.9602) then
                WRITE (8,
     &'('' Optical model parameters for ejectile '', I1,'' set to RIPL #
     &'', I4)') i1, ipoten
            else
                WRITE (8,
     &'('' Optical model parameters for ejectile '', I1,
     & '' set to Kumar & Kailas 2007 values'')') i1
            endif
C-----
            DO i = 1, NDNUC
               KTRlom(i1,i) = ipoten
            ENDDO
            GOTO 100
         ENDIF
C
C--------In the following block one parameter -KTRompCC- is defined
C--------DIRECT is set to 1 if equal zero to allow for ECIS calc.
C
         IF (name.EQ.'DIRPOT') THEN
! historically val and -val meant different potentials. The '+val'
! potentials are no longer in EMPIRE, so no need to treat differently:
             ki = 26 ! file id for 'om-parameter-u.dat' in RIPL
             ipoten = ABS(val)
             CALL FINDPOT(ki,ieof,ipoten)
             IF (ieof.NE.0) THEN
                 WRITE (8,*) 'WARNING: Requested RIPL entry ', ipoten,
     &                        ' for inelastic scattering not found'
                 GOTO 100
             ENDIF
             WRITE (8,
     &'('' Optical model parameters for direct inelastic scattering set
     & to RIPL #'',I4)') ipoten
             KTRompcc = ipoten
             KTRLOM(0,0) = ipoten
             GOTO 100
         ENDIF
C
         IF (name.EQ.'DIRECT') THEN
            DIRect = val
            IF (DIRect.EQ.3) WRITE (8,
     &         '('' DWBA (ECIS) used for direct inelastic scattering'')'
     &         )
            IF (DIRect.EQ.1 .OR. DIRect.EQ.2) WRITE (8,
     &'('' Coupled Channels Method used for direct inelastic scattering'
     &')')
            IF (DIRect.EQ.2) WRITE (8,
     &'('' Coupled Channels Method used for Tl calculations in inelastic
     & channels'')')
            IF (DIRect.EQ.3) WRITE (12,
     &         '('' DWBA (ECIS) used for direct inelastic scattering'')'
     &         )
            IF (DIRect.EQ.1 .OR. DIRect.EQ.2) WRITE (12,
     &'('' Coupled Channels Method used for direct inelastic scattering'
     &')')
            IF (DIRect.EQ.2) WRITE (12,
     &'('' Coupled Channels Method used for Tl calculations in inelastic
     & channels'')')
            GOTO 100
         ENDIF
C
         IF (name.EQ.'RESOLF') THEN
            IF(val.gt.0.) THEN
              WIDcoll = val
              WRITE (8,
     &     '('' Collective levels in continuum will be spREAD using'')')
              WRITE (8,
     &     '('' Gaussian function. Gaussian sigma = 0.02+R*sqrt(E); ''
     &       ''R = '',F6.3, '' keV'' )') WIDcoll*1000
             ENDIF
            GOTO 100
         ENDIF
C
C--------ECIS input  *** done ***
C
         IF (name.EQ.'PRGAMM') THEN
            IF(val.gt.0.d0) THEN
              NPRIm_g = 1
              WRITE (8 ,'('' Primary gammas calculated and stored'')')
              WRITE (12,'('' Primary gammas calculated and stored'')')
             ELSE
              WRITE (8 ,'('' Primary gammas not stored'')')
              WRITE (12,'('' Primary gammas not stored'')')
             ENDIF
            GOTO 100
         ENDIF

         IF (name.EQ.'RELKIN') THEN
            IF (val.NE.0.) THEN
               RELkin = .TRUE.
               WRITE (8,'(1x,A)') 'Relativistic kinematics used'
               WRITE (12,'(1x,A)') 'Relativistic kinematics used'
            ELSE
               WRITE (8,'(1x,A)') 'Non-relativistic kinematics used'
               WRITE (12,'(1x,A)') 'Non-relativistic kinematics used'
            ENDIF
            GOTO 100
         ENDIF

         IF (name.EQ.'BFUS  ') THEN
            BFUs = val
            WRITE (8,'('' Fusion barrier set to '',F7.2,'' MeV'')') BFUs
            GOTO 100
         ENDIF
C--------CCFUS input
         IF (name.EQ.'FCD   ') THEN
            FCD(i1) = val
            WRITE (8,
     &'('' FCD parameter for  n='',I2,'' collective level set to'',F6.3)
     &') i1, FCD(i1)
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'DV    ') THEN
            DV = val
            WRITE (8,'('' DV barrier parameter in CCFUS set to '',F6.3)'
     &             ) DV
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'FCC   ') THEN
            FCC = val
            WRITE (8,'('' FCC parameter in CCFUS set to '',F6.3)') FCC
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'NSCC  ') THEN
            NSCc = val
            WRITE (8,
     &            '('' Number of coupled channels in CCFUS set to'',I3)'
     &            ) NSCc
            GOTO 100
         ENDIF

         IF (name.EQ.'NACC  ') THEN
            NACc = val
            WRITE (8,
     &          '('' Number of additional coupled channels set to'',I3)'
     &          ) NACc
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'BETCC ') THEN
            BETcc(i1) = val
            WRITE (8,
     &'('' Deformation of the n='',I2,'' collective level set to'',F6.3)
     &') i1, BETcc(i1)
            GOTO 100
         ENDIF

         IF (name.EQ.'DEFNUC ') THEN
            DEF(1,0) = val
            WRITE (8,
     &'('' Deformation of the target nucleus set to'',F6.3)') val
            GOTO 100
         ENDIF

         IF (name.EQ.'FLAM  ') THEN
            FLAm(i1) = val
            IF(val.gt.0)
     &      WRITE (8,*)'TARGET COLLECTIVE CHANNEL FOR CCFUS DEFINED'
            IF(i2.lt.0)
     &      WRITE (8,*)'PROJECTILE COLLECTIVE CHANNEL FOR CCFUS DEFINED'
            WRITE (8,
     &'('' Multipolar. of the n='',I2,'' collective level set to'',F6.3)
     &') i1, FLAm(i1)
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'QCC   ') THEN
            QCC(i1) = val
            IF (QCC(i1).GT.0.0D0) QCC(i1) = -QCC(i1)
            WRITE (8,
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
C        Key_GDRGFL > = 0 and and Key_shape > 0 -  GDR parameters of RIPL introduced;
C        Key_GDRGFL = 1 -  GDR parameters and other data determined by gdrgfldata.f;
C                          experimental values or systematics of GDR parameters are set.
C        Key_GDRGFL = 2 -  GDR parameters and other data determined by gdrgfldata.f;
C                          if experimental values of GDR parameters not found and Key_GDRGFL=2
C                          they are going to be retrieved from the RIPL Goriely theoretical
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
            WRITE (8,
     &          '('' Liquid drop fission barriers multiplied by'',F6.3)'
     &          ) QFIs
            WRITE (12,
     &          '('' Liquid drop fission barriers multiplied by'',F6.3)'
     &          ) QFIs
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'LEVDEN') THEN
            IF(val.lt.0 .or. val.gt.3) THEN
              WRITE (8,'('' ERROR: LEVDEN ='',I1)') NINT(val)
              WRITE (8,
     &    '('' ERROR: LEVDEN must be 0,1,2; default EGSM = 0 used '')')
              GOTO 100
            ENDIF
            ADIv = val
            IF (ADIv.EQ.0.0D0) WRITE (8,
     & '('' EMPIRE-specific level densities (J>>K aprox.) selected '')')
            IF (ADIv.EQ.1.0D0) WRITE (8,
     &           '('' GSM (Ignatyuk) level densities selected '')')
            IF (ADIv.EQ.2.0D0) WRITE (8,
     &           '('' Gilbert-Cameron level densities selected '')'
     &           )
            IF (ADIv.EQ.3.0D0) WRITE (8,
     &          '('' Microscopic parity dependent HFB level densities se
     &lected'')')
            IF (ADIv.EQ.4.0D0) WRITE (8,
     & '('' Gilbert-Cameron (EMPIRE 2.18) level densities selected '')')
            IF (ADIv.EQ.0.0D0) WRITE (12,
     &           '('' EMPIRE-specific level densities (J>>K aprox.)'')')
            IF (ADIv.EQ.1.0D0) WRITE (12,
     &           '('' GSM level densities (Ignatyuk)  '')')
            IF (ADIv.EQ.2.0D0) WRITE (12,
     &           '('' Gilbert-Cameron level densities '')')
            IF (ADIv.EQ.3.0D0) WRITE (12,
     &     '('' Microscopic parity dependent HFB level densities '')')
            IF (ADIv.EQ.4.0D0) WRITE (12,
     & '('' Gilbert-Cameron (EMPIRE 2.18) level densities selected '')')
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'BETAV ') THEN
            BETav = val
            WRITE (8,
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
            WRITE (8,
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
            WRITE (8,
     &         '('' Diffuseness of the shell correction damping'',F6.3)'
     &          ) SHRd
            WRITE (12,
     &         '('' Diffuseness of the shell correction damping'',F6.3)'
     &          ) SHRd
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'ISOMER') THEN
         IF(val.le.1.d0) THEN
              WRITE (8,
     &        '('' Minimum half life of the considered isomers < 1s !''
     &         ,F6.3)') val
              WRITE (8,'('' Value reset to 1 s'')')
              TISomer = 1.d0
         GOTO 100
         ENDIF
            TISomer = val
            WRITE (8,
     &       '('' Minimum half life of the considered isomers : '',
     &         F6.3,2H S)') val
            WRITE (12,
     &       '('' Minimum half life of the considered isomers : '',
     &         F6.3,2H S)') val
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'FCCRED') THEN
            if(i1.ne.0 .and. IOPran.ne.0) then
                WRITE (8,
     &          '('' Direct cross section uncertainty '',
     &          '' is equal to '',i2,'' %'')') i1
                 sigma = val*i1*0.01
                IF(IOPran.gt.0) then
                  IF(rFCCred.eq.1.d0) rFCCred = grand()
                  FCCred = val + rFCCred*sigma
                ELSE
                  IF(rFCCred.eq.1.d0) rFCCred = drand()
                  FCCred = val + 1.732d0*(2*rFCCred-1.)*sigma
                ENDIF
                WRITE (8,
     &     '('' Direct cross section was scaled by factor ''
     &          ,f6.3)') FCCred
                IPArCOV = IPArCOV +1
                write(95,'(1x,i5,1x,d12.6,1x,2i13)')
     &             IPArCOV, FCCred, INDexf,INDexb
            else
                FCCred = val
                WRITE (8,
     &      '('' Direct cross section was scaled by factor '',
     &            F6.3)') FCCred
                WRITE (12,
     &      '('' Direct cross section was scaled by factor '',
     &           F6.3)') FCCred
            endif
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'FUSRED') THEN
            if(i1.ne.0 .and. IOPran.ne.0) then
                WRITE (8,
     &          '('' Fusion cross section uncertainty '',
     &          '' is equal to '',i2,'' %'')') i1
                 sigma = val*i1*0.01
                IF(IOPran.gt.0) then
                  IF(rFUSred.eq.1.d0) rFUSred = grand()
                  FUSred = val + rFUSred*sigma
                ELSE
                  IF(rFUSred.eq.1.d0) rFUSred = drand()
                  FUSred = val + 1.732d0*(2*rFUSred-1.)*sigma
                ENDIF
                WRITE (8,
     &      '('' Fusion cross section was scaled by factor ''
     &          ,f6.3)') FUSred
                IPArCOV = IPArCOV +1
                write(95,'(1x,i5,1x,d12.6,1x,2i13)')
     &             IPArCOV, FUSred, INDexf,INDexb
            else
                FUSred = val
                WRITE (8,
     &      '('' Fusion cross section was scaled by factor '',
     &            F6.3)') FUSred
                WRITE (12,
     &      '('' Fusion cross section was scaled by factor '',
     &           F6.3)') FUSred
            endif
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'TOTRED') THEN
            if(i1.ne.0 .and. IOPran.ne.0) then
                WRITE (8,
     &          '('' Total cross section uncertainty '',
     &          '' is equal to '',i2,'' %'')') i1
                sigma = val*i1*0.01
                IF(IOPran.gt.0) then
                   IF(rTOTred.eq.1.d0) rTOTred = grand()
                   TOTred = val + rTOTred*sigma
                ELSE
                   IF(rTOTred.eq.1.d0) rTOTred = drand()
                   TOTred = val + 1.732d0*(2*rTOTred-1.)*sigma
                ENDIF
                WRITE (8,
     &          '('' Total cross section was scaled by factor ''
     &          ,f6.3)') TOTred
                IPArCOV = IPArCOV +1
                write(95,'(1x,i5,1x,d12.6,1x,2i13)')
     &             IPArCOV, TOTred, INDexf,INDexb
            else
                TOTred = val
                WRITE (8,
     &      '('' Total cross section was scaled by factor '',
     &          F6.3)') TOTred
                WRITE (12,
     &      '('' Total cross section was scaled by factor '',
     &          F6.3)') TOTred
            endif
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'ELARED') THEN
            if(i1.ne.0 .and. IOPran.ne.0) then
                WRITE (8,
     &          '('' Shape elastic cross section uncertainty '',
     &          '' is equal to '',i2,'' %'')') i1
                sigma = val*i1*0.01
                IF(IOPran.gt.0) then
                   IF(rELAred.eq.1.d0) rELAred = grand()
                   ELAred = val + rELAred*sigma
                ELSE
                   IF(rELAred.eq.1.d0) rELAred = drand()
                   ELAred = val + 1.732d0*(2*rELAred-1.)*sigma
                ENDIF
                WRITE (8,
     &          '('' Shape elastic cross section was scaled by factor ''
     &          ,f6.3)') ELAred
                IPArCOV = IPArCOV +1
                write(95,'(1x,i5,1x,d12.6,1x,2i13)')
     &             IPArCOV, ELAred, INDexf,INDexb
            else
                ELAred = val
                WRITE (8,
     &      '('' Shape elastic cross section was scaled by factor '',
     &          F6.3)') ELAred
                WRITE (12,
     &      '('' Shape elastic cross section was scaled by factor '',
     &          F6.3)') ELAred
            endif
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'CSREAD') THEN
            CSRead = val
            IF (CSRead.GT.0.0D0) WRITE (8,
     &       '('' Fusion cross section '',F8.3,'' mb read from input'')'
     &       ) CSRead

            IF (CSRead.LE.0 .AND. AEJc(0).LE.4.0D0) THEN
              WRITE (8,'('' CSRead value in input ignored: '')') CSRead
              CSRead = -2.0D0
              GOTO 100
            ENDIF

            IF (CSRead.EQ.0.0D0) THEN
               WRITE (8,
     &'('' Bass option disabled CCFUS will be used instead          '')'
     &)
               CSRead = -2.0D0
            ENDIF
            IF (CSRead.EQ.( - 1.0D0)) WRITE (8,
     &'('' Fusion cross section will be calculated according to distribu
     &ted barrier model'')')
            IF (CSRead.EQ.( - 2.0D0)) WRITE (8,
     &'('' Fusion cross section will be calculated using CCFUS simplifie
     &d coupled channel approach'')')
            IF (CSRead.EQ.( - 3.0D0)) WRITE (8,
     &'('' Fusion cross section will be calculated using CCFUS uncoupled
     & barriers'')')
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'SIG   ') THEN
            SIG = val
            IF (CSRead.EQ.( - 1.0D0)) WRITE (8,
     &      '('' SIGMA in the distributed barrier model set to '',F6.3)'
     &      ) SIG
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'TRUNC ') THEN
            TRUnc = val
            IF (CSRead.EQ.( - 1.0D0)) WRITE (8,
     & '('' Truncation in the distributed barrier model set to '',F6.3)'
     & ) TRUnc
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'EXPUSH') THEN
            EXPush = val
            IF (CSRead.EQ.( - 1.0D0))
     &           WRITE (8,'('' Extrapush set to '',F6.3)') EXPush
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'CRL   ') THEN
            CRL = val
            WRITE (8,'('' Critical l-value for fusion set to '',F6.2)')
     &             CRL
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'TRGLEV') THEN
            LEVtarg = val
            WRITE (8,'('' Target excited to the level #'',I2)') LEVtarg
            WRITE (12,'('' Target excited to the level #'',I2)') LEVtarg
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'DFUS  ') THEN
            DFUs = val
            WRITE (8,
     &'('' Difusness in the transmission coefficients for fusion set to
     &'',F5.2)') DFUs
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'TEMP0 ') THEN
            TEMp0 = val
            WRITE (8,
     &'('' Temperature at which shell correction fade-out starts set to
     &'',F6.3)') TEMp0
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'SHRT  ') THEN
            SHRt = val
            WRITE (8,
     &'('' Parameter in the teperature shell correction fade-out set to
     &'',F6.3)') SHRt
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'IOUT  ') THEN
            IOUt = val
            WRITE (8,
     &             '('' Main calculations output control set to '',I2)')
     &             IOUt
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'NOUT  ') THEN
            NOUt = val
            WRITE (8,'('' MSC calculation output control set to '',I2)')
     &             NOUt
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'XNI   ') THEN
            XNI = val
            WRITE (8,'('' Initial exciton number set to '',F4.1)') XNI
            WRITE (12,'('' Initial exciton number '',F4.1)') XNI
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'TORY  ') THEN
            TORy = val
            WRITE (8,
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
            WRITE (8,
     &   '('' Initial number of excitons being neutrons set to '',F6.3)'
     &    ) EX1
            WRITE (12,
     &    '('' Initial number of excitons being neutrons '',F6.3)') EX1
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'EX2   ') THEN
            EX2 = val
            WRITE (8,
     &   '('' Initial number of excitons being protons set to  '',F6.3)'
     &    ) EX2
            WRITE (12,
     &    '('' Initial number of excitons being protons '',F6.3)') EX2
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'GST   ') THEN
            GST = val
            IF (GST.EQ.1.0D0) WRITE (8,
     &                       '('' Gamma emission in MSC considered'')')
            IF (GST.EQ.1.0D0) WRITE (12,
     &                       '('' Gamma emission in MSC considered'')')

            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'STMRO ') THEN
            STMro = val
            IF (STMro.EQ.1.0D0) WRITE (8,
     &                 '('' Microscopic p-h state densities selected'')'
     &                 )
            IF (STMro.EQ.0.0D0) WRITE (8,
     &                 '('' Closed form p-h state densities selected'')'
     &                 )
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'GDIV  ') THEN
            GDIv = val
            WRITE (8,
     &       '('' Single particle level density in PE models set to A/''
     &           ,F5.2)'
     &      ) GDIv
            WRITE (12,
     &       '('' Single particle level density in PE models set to A/''
     &           ,F5.2)'
     &      ) GDIv
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'D1FRA ') THEN
            D1Fra = val
            WRITE (8,'('' Spreading to total GDR width set to '',F5.3)')
     &             D1Fra
            WRITE (12,'('' Spreading to total GDR width set to '',F5.3)'
     &             ) D1Fra
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'NEX   ') THEN
            NEXreq = val
            IF (val.GT.NDEX-1) NEXreq = NDEX-1
            WRITE (8,
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
            IF (GDRdyn.NE.0.D0) WRITE (8,
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
               WRITE (8,'('' NUCLEUS A,Z ='',I3,'','',I3,
     &                '' NOT NEEDED'')') i2,i1
               WRITE (8,
     &'('' Real volume potential depth uncertainty ignored'')')
               GOTO 100
            ENDIF
            IF (i3.GT.NDEJC) THEN
               WRITE (8,'('' UNKNOWN EJECTILE in UOMPVV '',I2)') i3
               WRITE (8,
     &'('' Real volume potential depth uncertainty ignored'')')
               GOTO 100
            ENDIF
            if(val.gt.0. .and. IOPran.ne.0) then
              WRITE (8,
     &        '('' Real volume potential depth uncertainty in '',I3,A2,
     &        '' is equal to '',f5.2,'' %'')') i2, SYMb(nnuc), val
                 sigma = val*0.01
              IF(IOPran.gt.0) then
                FNvvomp(i3,nnuc) = 1. + grand()*sigma
              ELSE
                FNvvomp(i3,nnuc) = 1. + 1.732d0*(2*drand()-1.)*sigma
              ENDIF
              WRITE (8,
     &        '('' Real volume potential depth sampled norm.factor : '',
     &        f5.2)') FNvvomp(i3,nnuc)
                 IPArCOV = IPArCOV +1
                 write(95,'(1x,i5,1x,d12.6,1x,2i13)')
     &              IPArCOV, FNvvomp(i3,nnuc), INDexf,INDexb
            endif
            if(val.lt.0.) then
              FNvvomp(i3,nnuc) = abs(val)
              WRITE (8,
     &        '('' Real volume potential depth in '',I3,A2,
     &        '' scaled by '',f5.2)') i2, SYMb(nnuc), abs(val)
              WRITE (12,
     &        '('' Real volume potential depth in '',I3,A2,
     &        '' scaled by '',f5.2)') i2, SYMb(nnuc), abs(val)
            endif
            GOTO 100
         ENDIF
C----
C
C--------Volume real and imaginary imaginary potential diffuseness
C        AVOm(Nejc,Nnuc) = alib(1)*FNavomp(Nejc,Nnuc)
C        AWOm(Nejc,Nnuc) = alib(3)*FNavomp(Nejc,Nnuc)
C
         IF (name.EQ.'UOMPAV') THEN
            izar = i1*1000 + i2
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (8,'('' NUCLEUS A,Z ='',I3,'','',I3,
     &                '' NOT NEEDED'')') i2,i1
               WRITE (8,
     &        '('' Volume potential diffuseness uncertainty ignored'')')
               GOTO 100
            ENDIF
            IF (i3.GT.NDEJC) THEN
               WRITE (8,'('' UNKNOWN EJECTILE in UOMPAV '',I2)') i3
               WRITE (8,
     &        '('' Volume potential diffuseness uncertainty ignored'')')
               GOTO 100
            ENDIF
            if(val.gt.0. .and. IOPran.ne.0) then
              WRITE (8,
     &        '('' Volume potential diffuseness uncertainty in '',I3,
     &        A2,'' is equal to '',f5.2,'' %'')') i2, SYMb(nnuc), val
                 sigma = val*0.01
              IF(IOPran.gt.0) then
                FNavomp(i3,nnuc) = 1. + grand()*sigma
              ELSE
                FNavomp(i3,nnuc) = 1. + 1.732d0*(2*drand()-1.)*sigma
              ENDIF
              WRITE (8,
     &        '('' Volume potential diffuseness sampled norm.factor : ''
     &        ,f5.2)') FNavomp(i3,nnuc)
                 IPArCOV = IPArCOV +1
                 write(95,'(1x,i5,1x,d12.6,1x,2i13)')
     &              IPArCOV, FNavomp(i3,nnuc), INDexf,INDexb
            endif
            if(val.lt.0.) then
              FNavomp(i3,nnuc) = abs(val)
              WRITE (8,
     &        '('' Volume potential diffuseness in '',I3,A2,
     &        '' scaled by '',f5.2)') i2, SYMb(nnuc), abs(val)
              WRITE (12,
     &        '('' Volume potential diffuseness in '',I3,A2,
     &        '' scaled by '',f5.2)') i2, SYMb(nnuc), abs(val)
            endif
            GOTO 100
         ENDIF
C--------Volume imaginary potential depth
C        WOMv(Nejc,Nnuc) = vlib(2)*FNwvomp(Nejc,Nnuc)
         IF (name.EQ.'UOMPWV') THEN
            izar = i1*1000 + i2
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (8,'('' NUCLEUS A,Z ='',I3,'','',I3,
     &                '' NOT NEEDED'')') i2,i1
               WRITE (8,
     &        '('' Imag. volume potential depth uncertainty ignored'')')
               GOTO 100
            ENDIF
            IF (i3.GT.NDEJC) THEN
               WRITE (8,'('' UNKNOWN EJECTILE in UOMPWV '',I2)') i3
               WRITE (8,
     &        '('' Imag. volume potential depth uncertainty ignored'')')
               GOTO 100
            ENDIF
            if(val.gt.0. .and. IOPran.ne.0) then
              WRITE (8,
     &        '('' Imag. volume potential depth uncertainty in '',I3,A2,
     &        '' is equal to '',f5.2,'' %'')') i2, SYMb(nnuc), val
                 sigma = val*0.01
              IF(IOPran.gt.0) then
                FNwvomp(i3,nnuc) = max(1. + grand()*sigma,0.d0)
              ELSE
                FNwvomp(i3,nnuc) =
     &            max(1. + 1.732d0*(2*drand()-1.)*sigma,0.d0)
              ENDIF
              WRITE (8,
     &        '('' Imag. volume potential depth sampled norm.factor : ''
     &        ,f5.2)') FNwvomp(i3,nnuc)
                 IPArCOV = IPArCOV +1
                 write(95,'(1x,i5,1x,d12.6,1x,2i13)')
     &              IPArCOV, FNwvomp(i3,nnuc), INDexf,INDexb
            endif
            if(val.lt.0.) then
              FNwvomp(i3,nnuc) = abs(val)
              WRITE (8,
     &        '('' Imag. volume potential depth in '',I3,A2,
     &        '' scaled by '',f5.2)') i2, SYMb(nnuc), abs(val)
              WRITE (12,
     &        '('' Imag. volume potential depth in '',I3,A2,
     &        '' scaled by '',f5.2)') i2, SYMb(nnuc), abs(val)
            endif
            GOTO 100
         ENDIF

C--------Surface imaginary potential depth:
C        WOMs(Nejc,Nnuc) = vlib(4)*FNwsomp(Nejc,Nnuc)
         IF (name.EQ.'UOMPWS') THEN
            izar = i1*1000 + i2
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (8,'('' NUCLEUS A,Z ='',I3,'','',I3,
     &                '' NOT NEEDED'')') i2,i1
               WRITE (8,
     &      '('' Imag. surface potential depth uncertainty ignored'')')
               GOTO 100
            ENDIF
            IF (i3.GT.NDEJC) THEN
               WRITE (8,'('' UNKNOWN EJECTILE in UOMPWS '',I2)') i3
               WRITE (8,
     &      '('' Imag. surface potential depth uncertainty ignored'')')
               GOTO 100
            ENDIF
            if(val.gt.0. .and. IOPran.ne.0) then
              WRITE (8,
     &      '('' Imag. surface potential depth uncertainty in '',I3,A2,
     &        '' is equal to '',f5.2,'' %'')') i2, SYMb(nnuc), val
                 sigma = val*0.01
              IF(IOPran.gt.0) then
                FNwsomp(i3,nnuc) = max(1. + grand()*sigma,0.d0)
              ELSE
                FNwsomp(i3,nnuc) =
     &            max(1. + 1.732d0*(2*drand()-1.)*sigma,0.d0)
              ENDIF
              WRITE (8,
     &      '('' Imag. surface potential depth sampled norm.factor : '',
     &        f5.2)') FNwsomp(i3,nnuc)
                 IPArCOV = IPArCOV +1
                 write(95,'(1x,i5,1x,d12.6,1x,2i13)')
     &              IPArCOV, FNwsomp(i3,nnuc), INDexf,INDexb
            endif
            if(val.lt.0.) then
              FNwsomp(i3,nnuc) = abs(val)
              WRITE (8,
     &        '('' Imag. surface potential depth in '',I3,A2,
     &        '' scaled by '',f5.2)') i2, SYMb(nnuc), abs(val)
              WRITE (12,
     &        '('' Imag. surface potential depth in '',I3,A2,
     &        '' scaled by '',f5.2)') i2, SYMb(nnuc), abs(val)
            endif
            GOTO 100
         ENDIF
C----
C--------Surface imaginary potential diffuseness:
C        AWOm(Nejc,Nnuc) = alib(4)*FNasomp(Nejc,Nnuc)
         IF (name.EQ.'UOMPAS') THEN
            izar = i1*1000 + i2
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (8,'('' NUCLEUS A,Z ='',I3,'','',I3,
     &                '' NOT NEEDED'')') i2,i1
               WRITE (8,
     & '('' Surface potential diffuseness uncertainty ignored'')')
               GOTO 100
            ENDIF
            IF (i3.GT.NDEJC) THEN
               WRITE (8,'('' UNKNOWN EJECTILE in UOMPAS '',I2)') i3
               WRITE (8,
     & '('' Surface potential diffuseness uncertainty ignored'')')
               GOTO 100
            ENDIF
            if(val.gt.0. .and. IOPran.ne.0) then
              WRITE (8,
     &        '('' Surface potential diffuseness uncertainty in '',I3,
     &        A2,'' is equal to '',f5.2,'' %'')') i2, SYMb(nnuc), val
                 sigma = val*0.01
              IF(IOPran.gt.0) then
                FNasomp(i3,nnuc) = 1. + grand()*sigma
              ELSE
                FNasomp(i3,nnuc) = 1. + 1.732d0*(2*drand()-1.)*sigma
              ENDIF
              WRITE (8,
     &        '('' Surface potential diffuseness sampled norm.factor :''
     &        ,f5.2)') FNasomp(i3,nnuc)
                 IPArCOV = IPArCOV +1
                 write(95,'(1x,i5,1x,d12.6,1x,2i13)')
     &              IPArCOV, FNasomp(i3,nnuc), INDexf,INDexb
            endif
            if(val.lt.0.) then
              FNasomp(i3,nnuc) = abs(val)
              WRITE (8,
     &        '('' Imag. surface potential diff. in '',I3,A2,
     &        '' scaled by '',f5.2)') i2, SYMb(nnuc), abs(val)
              WRITE (12,
     &        '('' Imag. surface potential diff. in '',I3,A2,
     &        '' scaled by '',f5.2)') i2, SYMb(nnuc), abs(val)
            endif
            GOTO 100
         ENDIF
C----
C--------Surface imaginary potential radius:
C        RWOm(Nejc,Nnuc) = rlib(4)*FNrsomp(Nejc,Nnuc)
         IF (name.EQ.'UOMPRS') THEN
            izar = i1*1000 + i2
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (8,'('' NUCLEUS A,Z ='',I3,'','',I3,
     &                '' NOT NEEDED'')') i2,i1
               WRITE (8,
     & '('' Surface potential radius uncertainty ignored'')')
               GOTO 100
            ENDIF
            IF (i3.GT.NDEJC) THEN
               WRITE (8,'('' UNKNOWN EJECTILE in UOMPRS'',I2)') i3
               WRITE (8,
     & '('' Surface potential radius uncertainty ignored'')')
               GOTO 100
            ENDIF
            if(val.gt.0. .and. IOPran.ne.0) then
              WRITE (8,
     &        '('' Surface potential radius uncertainty in '',I3,
     &        A2,'' is equal to '',f5.2,'' %'')') i2, SYMb(nnuc), val
                 sigma = val*0.01
              IF(IOPran.gt.0) then
                FNrsomp(i3,nnuc) = 1. + grand()*sigma
              ELSE
                FNrsomp(i3,nnuc) = 1. + 1.732d0*(2*drand()-1.)*sigma
              ENDIF
              WRITE (8,
     &        '('' Surface potential radius sampled norm. factor :''
     &        ,f5.2)') FNrsomp(i3,nnuc)
                 IPArCOV = IPArCOV +1
                 write(95,'(1x,i5,1x,d12.6,1x,2i13)')
     &              IPArCOV, FNrsomp(i3,nnuc), INDexf,INDexb
            endif
            if(val.lt.0.) then
              FNrsomp(i3,nnuc) = abs(val)
              WRITE (8,
     &        '('' Imag. surface potential radius in '',I3,A2,
     &        '' scaled by '',f5.2)') i2, SYMb(nnuc), abs(val)
              WRITE (12,
     &        '('' Imag. surface potential radius in '',I3,A2,
     &        '' scaled by '',f5.2)') i2, SYMb(nnuc), abs(val)
            endif
            GOTO 100
         ENDIF
C----
C--------Volume imaginary potential radius:
C        RWOmv(Nejc,Nnuc) = rlib(2)*FNrwvomp(Nejc,Nnuc)
         IF (name.EQ.'UOMPRW') THEN
            izar = i1*1000 + i2
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (8,'('' NUCLEUS A,Z ='',I3,'','',I3,
     &                '' NOT NEEDED'')') i2,i1
               WRITE (8,
     & '('' Volume imaginary potential radius uncertainty ignored'')')
               GOTO 100
            ENDIF
            IF (i3.GT.NDEJC) THEN
               WRITE (8,'('' UNKNOWN EJECTILE in UOMPRW'',I2)') i3
               WRITE (8,
     & '('' Volume imaginary potential radius uncertainty ignored'')')
               GOTO 100
            ENDIF
            if(val.gt.0. .and. IOPran.ne.0) then
              WRITE (8,
     &        '('' Volume imaginary potential radius uncertainty in '',
     &        I3,A2,'' is equal to '',f5.2,'' %'')') i2, SYMb(nnuc), val
              sigma = val*0.01
              IF(IOPran.gt.0) then
                FNrwvomp(i3,nnuc) = 1. + grand()*sigma
              ELSE
                FNrwvomp(i3,nnuc) = 1. + 1.732d0*(2*drand()-1.)*sigma
              ENDIF
              WRITE (8,
     &        '('' Volume imaginary potential radius sampled factor :''
     &        ,f5.2)') FNrwvomp(i3,nnuc)
                 IPArCOV = IPArCOV +1
                 write(95,'(1x,i5,1x,d12.6,1x,2i13)')
     &              IPArCOV, FNrwvomp(i3,nnuc), INDexf,INDexb
            endif
            if(val.lt.0.) then
              FNrwvomp(i3,nnuc) = abs(val)
              WRITE (8,
     &        '('' Imag. volume potential radius in '',I3,A2,
     &        '' scaled by '',f5.2)') i2, SYMb(nnuc), abs(val)
              WRITE (12,
     &        '('' Imag. volume potential radius in '',I3,A2,
     &        '' scaled by '',f5.2)') i2, SYMb(nnuc), abs(val)
            endif
            GOTO 100
         ENDIF
C----
C--------Volume real potential radius:
C        RVOm(Nejc,Nnuc) = rlib(1)*FNrvomp(Nejc,Nnuc)
         IF (name.EQ.'UOMPRV') THEN
            izar = i1*1000 + i2
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (8,'('' NUCLEUS A,Z ='',I3,'','',I3,
     &                '' NOT NEEDED'')') i2,i1
               WRITE (8,
     & '('' Volume real potential radius uncertainty ignored'')')
               GOTO 100
            ENDIF
            IF (i3.GT.NDEJC) THEN
               WRITE (8,'('' UNKNOWN EJECTILE in UOMPRV'',I2)') i3
               WRITE (8,
     & '('' Volume real potential radius uncertainty ignored'')')
               GOTO 100
            ENDIF
            if(val.gt.0. .and. IOPran.ne.0) then
              WRITE (8,
     &        '('' Volume real potential radius uncertainty in '',
     &        I3,A2,'' is equal to '',f5.2,'' %'')') i2, SYMb(nnuc), val
              sigma = val*0.01
              IF(IOPran.gt.0) then
                FNrvomp(i3,nnuc) = 1. + grand()*sigma
              ELSE
                FNrvomp(i3,nnuc) = 1. + 1.732d0*(2*drand()-1.)*sigma
              ENDIF
              WRITE (8,
     &        '('' Volume real potential radius sampled factor :''
     &        ,f5.2)') FNrvomp(i3,nnuc)
                 IPArCOV = IPArCOV +1
                 write(95,'(1x,i5,1x,d12.6,1x,2i13)')
     &              IPArCOV, FNrvomp(i3,nnuc), INDexf,INDexb
            endif
            if(val.lt.0.) then
              FNrvomp(i3,nnuc) = abs(val)
              WRITE (8,
     &        '('' Volume real potential radius in '',I3,A2,
     &        '' scaled by '',f5.2)') i2, SYMb(nnuc), abs(val)
              WRITE (12,
     &        '('' Volume real potential radius in '',I3,A2,
     &        '' scaled by '',f5.2)') i2, SYMb(nnuc), abs(val)
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
              WRITE (8,
     &        '('' GDR first hump energy in all nuclei set to '',F5.2)')
     &        val
              WRITE (12,
     &        '('' GDR first hump energy in all nuclei set to '',F5.2)')
     &        val
              GOTO 100
            ENDIF
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (8,'('' NUCLEUS A,Z ='',I3,'','',I3,
     &                '' NOT NEEDED'')') i2,i1
               WRITE (8,
     &          '('' NORMALIZATION OF GDR first hump energy IGNORED'')')
               GOTO 100
            ENDIF
            if(i3.ne.0 .and. IOPran.ne.0) then
              WRITE (8,
     &        '('' GDR first hump energy uncertainty in '',I3,A2,
     &        '' is equal to '',i2,''%'')') i2, SYMb(nnuc), i3
              sigma = val*i3*0.01
              IF(IOPran.gt.0) then
                GDRpar(1,nnuc) = val + grand()*sigma
              ELSE
                GDRpar(1,nnuc) = val + 1.732d0*(2*drand()-1.)*sigma
              ENDIF
              WRITE (8,
     &        '('' GDR first hump energy sampled value : '',f5.2)')
     &        GDRpar(1,nnuc)
               IPArCOV = IPArCOV +1
               write(95,'(1x,i5,1x,d12.6,1x,2i13)')
     &              IPArCOV, GDRpar(1,nnuc), INDexf,INDexb
            else
              GDRpar(1,nnuc) = val
              WRITE (8,
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
              WRITE (8,
     &        '('' GDR first hump width in all nuclei set to '',F5.2)')
     &        val
              WRITE (12,
     &        '('' GDR first hump width in all nuclei set to '',F5.2)')
     &        val
              GOTO 100
            ENDIF
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (8,'('' NUCLEUS A,Z ='',I3,'','',I3,
     &                '' NOT NEEDED'')') i2,i1
               WRITE (8,
     &          '('' NORMALIZATION OF GDR first hump width IGNORED'')')
               GOTO 100
            ENDIF
            if(i3.ne.0 .and. IOPran.ne.0) then
              WRITE (8,
     &        '('' GDR first hump width uncertainty in '',I3,A2,
     &        '' is equal to '',i2,''%'')') i2, SYMb(nnuc), i3
              sigma = val*i3*0.01
              IF(IOPran.gt.0) then
                GDRpar(2,nnuc) = val + grand()*sigma
              ELSE
                GDRpar(2,nnuc) = val + 1.732d0*(2*drand()-1.)*sigma
              ENDIF
              WRITE (8,
     &        '('' GDR first hump width sampled value : '',f5.2)')
     &        GDRpar(2,nnuc)
               IPArCOV = IPArCOV +1
               write(95,'(1x,i5,1x,d12.6,1x,2i13)')
     &              IPArCOV, GDRpar(2,nnuc), INDexf,INDexb
            else
              GDRpar(2,nnuc) = val
              WRITE (8,
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
              WRITE (8,
     &        '('' GDR first hump cross section in all nuclei set to ''
     &        ,F5.2)') val
              WRITE (12,
     &        '('' GDR first hump cross section in all nuclei set to ''
     &        ,F5.2)') val
              GOTO 100
            ENDIF
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (8,'('' NUCLEUS A,Z ='',I3,'','',I3,
     &                '' NOT NEEDED'')') i2,i1
               WRITE (8,
     &          '('' NORMALIZATION OF GDR first hump XS IGNORED'')')
               GOTO 100
            ENDIF
            if(i3.ne.0 .and. IOPran.ne.0) then
              WRITE (8,
     &        '('' GDR first hump cross section uncertainty in '',I3,A2,
     &        '' is equal to '',i2,''%'')') i2, SYMb(nnuc), i3
              sigma = val*i3*0.01
              IF(IOPran.gt.0) then
                GDRpar(3,nnuc) = val + grand()*sigma
              ELSE
                GDRpar(3,nnuc) = val + 1.732d0*(2*drand()-1.)*sigma
              ENDIF
              WRITE (8,
     &        '('' GDR first hump cross section sampled value : ''
     &        ,f5.2)') GDRpar(3,nnuc)
               IPArCOV = IPArCOV +1
               write(95,'(1x,i5,1x,d12.6,1x,2i13)')
     &              IPArCOV, GDRpar(3,nnuc), INDexf,INDexb
            else
              GDRpar(3,nnuc) = val
              WRITE (8,
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
              WRITE (8,
     &       '('' GDR second hump energy in all nuclei set to '',F5.2)')
     &        val
              WRITE (12,
     &       '('' GDR second hump energy in all nuclei set to '',F5.2)')
     &        val
              GOTO 100
            ENDIF
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (8,'('' NUCLEUS A,Z ='',I3,'','',I3,
     &                '' NOT NEEDED'')') i2,i1
               WRITE (8,
     &         '('' NORMALIZATION OF GDR second hump energy IGNORED'')')
               GOTO 100
            ENDIF
            if(i3.ne.0 .and. IOPran.ne.0) then
              WRITE (8,
     &        '('' GDR second hump energy uncertainty in '',I3,A2,
     &        '' is equal to '',i2,''%'')') i2, SYMb(nnuc), i3
              sigma = val*i3*0.01
              IF(IOPran.gt.0) then
                GDRpar(4,nnuc) = val + grand()*sigma
              ELSE
                GDRpar(4,nnuc) = val + 1.732d0*(2*drand()-1.)*sigma
              ENDIF
              WRITE (8,
     &        '('' GDR second hump energy sampled value : '',f5.2)')
     &        GDRpar(4,nnuc)
               IPArCOV = IPArCOV +1
               write(95,'(1x,i5,1x,d12.6,1x,2i13)')
     &              IPArCOV, GDRpar(4,nnuc), INDexf,INDexb
            else
              GDRpar(4,nnuc) = val
              WRITE (8,
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
              WRITE (8,
     &        '('' GDR second hump width in all nuclei set to '',F5.2)')
     &        val
              WRITE (12,
     &        '('' GDR second hump width in all nuclei set to '',F5.2)')
     &        val
              GOTO 100
            ENDIF
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (8,'('' NUCLEUS A,Z ='',I3,'','',I3,
     &                '' NOT NEEDED'')') i2,i1
               WRITE (8,
     &          '('' NORMALIZATION OF GDR FIRST HUMP WIDTH IGNORED'')')
               GOTO 100
            ENDIF
            if(i3.ne.0 .and. IOPran.ne.0) then
              WRITE (8,
     &        '('' GDR second hump width uncertainty in '',I3,A2,
     &        '' is equal to '',i2,''%'')') i2, SYMb(nnuc), i3
              sigma = val*i3*0.01
              IF(IOPran.gt.0) then
                GDRpar(5,nnuc) = val + grand()*sigma
              ELSE
                GDRpar(5,nnuc) = val + 1.732d0*(2*drand()-1.)*sigma
              ENDIF
              WRITE (8,
     &        '('' GDR second hump width sampled value : '',f5.2)')
     &        GDRpar(5,nnuc)
               IPArCOV = IPArCOV +1
               write(95,'(1x,i5,1x,d12.6,1x,2i13)')
     &              IPArCOV, GDRpar(5,nnuc), INDexf,INDexb
            else
              GDRpar(5,nnuc) = val
              WRITE (8,
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
              WRITE (8,
     &        '('' GDR second hump cross section in all nuclei set to ''
     &        ,F5.2)') val
              WRITE (12,
     &        '('' GDR second hump cross section in all nuclei set to ''
     &        ,F5.2)') val
              GOTO 100
            ENDIF
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (8,'('' NUCLEUS A,Z ='',I3,'','',I3,
     &                '' NOT NEEDED'')') i2,i1
               WRITE (8,
     &          '('' NORMALIZATION OF GDR FIRST HUMP XS IGNORED'')')
               GOTO 100
            ENDIF
            if(i3.ne.0 .and. IOPran.ne.0) then
              WRITE (8,
     &        '('' GDR second hump cross section uncertainty in '',I3,A2
     &        ,'' is equal to '',i2,''%'')') i2, SYMb(nnuc), i3
              sigma = val*i3*0.01
              IF(IOPran.gt.0) then
                GDRpar(6,nnuc) = val + grand()*sigma
              ELSE
                GDRpar(6,nnuc) = val + 1.732d0*(2*drand()-1.)*sigma
              ENDIF
              WRITE (8,
     &        '('' GDR second hump cross section sampled value : ''
     &        ,f5.2)') GDRpar(6,nnuc)
               IPArCOV = IPArCOV +1
               write(95,'(1x,i5,1x,d12.6,1x,2i13)')
     &              IPArCOV, GDRpar(6,nnuc), INDexf,INDexb
            else
              GDRpar(6,nnuc) = val
              WRITE (8,
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
            IF (MSD.EQ.1) WRITE (8,
     &'('' MSD calculations with ORION+TRISTAN were selected'')
     &   ')
            IF (MSD.EQ.1) WRITE (12,
     &'('' MSD calculations with ORION+TRISTAN were used'')')
            IF (MSD.EQ.2) WRITE (8,
     &         '('' including contribution to discrete levels'')')
            IF (MSD.EQ.2) WRITE (12,
     &         '('' including contribution to discrete levels'')')
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'MSC   ') THEN
            MSC = val
            IF (MSC.NE.0) WRITE (8,
     &                '('' Heidelberg MSC calculations were selected'')'
     &                )
            IF (MSC.NE.0) WRITE (12,
     &                '('' Heidelberg MSC calculations were used'')')
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'HMS   ') THEN
            LHMs = val
            IF (LHMs.NE.0) WRITE (8,
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
               WRITE (8,'('' Number of events in HMS set to '',I10)')
     &                NHMs
               WRITE (12,'('' Number of events in HMS '',I10)') NHMs
            ENDIF
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'FHMS  ') THEN
            IF (val.LT.1.0D0) THEN
               FHMs = 0
               WRITE (8,
     &             '('' Exciton densities are used in DDHMS '')')
               WRITE (12,
     &             '('' Exciton densities are used in DDHMS '')')
             ELSE IF(val.LT.1.99D0) THEN
               FHMs = 1
               WRITE (8,
     &             '('' Fermi gas densities are used in DDHMS '')')
               WRITE (12,
     &             '('' Fermi gas densities are used in DDHMS '')')
             ELSE IF(val.LT.2.99D0) THEN
               FHMs = 2
               WRITE (8,
     &         '('' Exact NR Fermi gas densities are used in DDHMS '')')
               WRITE (12,
     &         '('' Exact NR Fermi gas densities are used in DDHMS '')')
             ELSE 
               FHMs = 3
               WRITE (8,
     &       '('' Exact rel. Fermi gas densities are used in DDHMS '')')
               WRITE (12,
     &       '('' Exact rel. Fermi gas densities are used in DDHMS '')')
            ENDIF
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'CHMS  ') THEN
            IF (val.GT.0.0D0) THEN
               CHMs = val
               WRITE (8,
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
            IF (val.GT.0) THEN
              LHRtw = 1
              EHRtw = val
              IF (LHRtw.NE.0) WRITE (8,
     &           '('' HRTW width fluctuation correction was selected'',
     &             '' up to '',f4.2,'' MeV'')') EHRtw
              IF (LHRtw.NE.0) WRITE (12,
     &           '('' Width fluctuations calculated within HRTW '',
     &             '' up to '',f4.2,'' MeV'')') EHRtw
            ELSE
               LHRtw = 0
               EHRtw = 0.d0
            ENDIF
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'GDRWP ') THEN
            DIToro = val
            WRITE (8,
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
            WRITE (8,
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
            WRITE (8,
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
            WRITE (8,'('' GDR position shifted by '',F6.3,'' MeV'')')
     &             GDResh
            WRITE (12,'('' GDR position shifted by '',F6.3,'' MeV'')')
     &             GDResh
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'GDRSPL') THEN
            GDRspl = val
            WRITE (8,
     &'('' Splitting of GDR peaks increased by '',F6.3,         '' MEV''
     &)') GDRspl
            WRITE (12,
     &'('' Splitting of GDR peaks increased by '',F6.3,         '' MEV''
     &)') GDRspl
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'GDRST1') THEN
            EWSr1 = val
            WRITE (8,
     &'('' Gamma strength multiplied by '',F6.3,'' for the first GDR hum
     &p'')') EWSr1
            WRITE (12,
     &'('' Gamma strength multiplied by '',F6.3,'' for the first GDR hum
     &p'')') EWSr1
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'GDRST2') THEN
            EWSr2 = val
            WRITE (8,
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
            WRITE (8,
     &'('' Gamma strength composed of '',F7.3''% GDR + ''
     &,F7.3,''% Weisskopf'')') GDRweis*100., (1. - GDRweis)*100.0
            GOTO 100
         ENDIF
C----
         IF (name.EQ.'DEFGA ') THEN
            DEFga = val
            WRITE (8,
     &'('' Gaussian correction to deformation (amplitude)'',F7.3)
     &') DEFga
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'DEFGW ') THEN
            DEFgw = val
            WRITE (8,
     &'('' Gaussian correction to deformation (width)'',F7.3)
     &') DEFgw
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'DEFGP ') THEN
            DEFgp = val
            WRITE (8,
     &'('' Gaussian correction to deformation (position)'',F7.3)
     &') DEFgp
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'ENDF  ') THEN
             IF(i1.eq.0 .AND. i2.eq.0) THEN
C              Setting ENDF for all emission loops
               NENdf = INT(val)
               IF(NENdf.GT.0) THEN
                 WRITE (8,'('' ENDF formatting enabled'')')
                 WRITE (8,'(
     &            '' Exclusive spectra available up to'',
     &            '' emission loop # '',I2)') NENdf
                 WRITE (12,'('' ENDF formatting enabled'')')
                 WRITE (12,'(
     &            '' Exclusive spectra available up to'',
     &            '' emission loop # '',I2)') NENdf
                     GOTO 100
               ENDIF
               IF(NENdf.EQ.0) THEN
                 WRITE ( 8,'('' ENDF formatting disabled'')')
                 WRITE (12,'('' ENDF formatting disabled'')')
                 GOTO 100
               ENDIF
             ENDIF
             IF(val.LT.0) THEN
               WRITE (8,'('' WRONG ENDF value in input'',I3)')
     &                i2
               WRITE (8,'('' SETTING IGNORED'')')
               GOTO 100
             ENDIF
C           Setting ENDF for a single nucleus
            izar = i1*1000 + i2
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (8,'('' NUCLEUS A,Z ='',I3,'','',I3,
     &                '' NOT NEEDED'')') i2,i1
               WRITE (8,'('' ENDF SETTING IGNORED'')')
               GOTO 100
            ENDIF
            ENDf(nnuc) = INT(val)
            IF (ENDf(nnuc).EQ.1) THEN
              ENDf(nnuc) = 10  ! using as a flag 
              WRITE (8,
     &       '('' Exclusive spectra will be available for emission'',
     &         '' from nucleus '',I3,A2)') i2, SYMb(nnuc)
              WRITE (12,
     &       '('' Exclusive spectra will be available for emission'',
     &         '' from nucleus '',I3,A2)') i2, SYMb(nnuc)
            ENDIF
            IF (ENDf(nnuc).EQ.2) THEN
              WRITE (8,
     &       '('' Emission spectra from nucleus '',I3,A2,
     &         '' will be stored as inclusive'')') i2, SYMb(nnuc)
              WRITE (12,
     &       '('' Emission spectra from nucleus '',I3,A2,
     &         '' will be stored as inclusive'')') i2, SYMb(nnuc)
            ENDIF
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'ENDFA  ') THEN
             IF(i1.eq.0 .AND. i2.eq.0) THEN
C              Setting ENDF for all emission loops
               NENdfa = INT(val)
               IF(NENdfa.GT.0) THEN
                 WRITE (8,'('' ENDF formatting enabled'')')
                 WRITE (8,'(
     &            '' Exclusive DDXSs available up to'',
     &            '' emission loop # '',I2)') NENdfa
                 WRITE (12,'('' ENDF formatting enabled'')')
                 WRITE (12,'(
     &            '' Exclusive DDXSs available up to'',
     &            '' emission loop # '',I2)') NENdfa
                     GOTO 100
               ENDIF
               IF(NENdfa.EQ.0) THEN
                 WRITE ( 8,'('' Exclusive DDXSs disabled'')')
                 WRITE (12,'('' Exclusive DDXSs disabled'')')
                 GOTO 100
               ENDIF
             ENDIF
             IF(val.LT.0) THEN
               WRITE (8,'('' WRONG ENDFA value in input'',I3)')
     &                i2
               WRITE (8,'('' SETTING IGNORED'')')
               GOTO 100
             ENDIF
C           Setting ENDFA for a single nucleus
            izar = i1*1000 + i2
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (8,'('' NUCLEUS A,Z ='',I3,'','',I3,
     &                '' NOT NEEDED'')') i2,i1
               WRITE (8,'('' ENDFA SETTING IGNORED'')')
               GOTO 100
            ENDIF
            ENDfa(nnuc) = INT(val)
            IF (ENDfa(nnuc).EQ.1) THEN
              ENDfa(nnuc) = 10  ! using as a flag 
              WRITE (8,
     &       '('' Exclusive DDXS will be available for emission'',
     &         '' from nucleus '',I3,A2)') i2, SYMb(nnuc)
              WRITE (12,
     &       '('' Exclusive DDXS will be available for emission'',
     &         '' from nucleus '',I3,A2)') i2, SYMb(nnuc)
            ENDIF
            IF (ENDfa(nnuc).EQ.2) THEN
              WRITE (8,
     &       '('' Emission spectra from nucleus '',I3,A2,
     &         '' will be stored as inclusive'')') i2, SYMb(nnuc)
              WRITE (12,
     &       '('' Emission spectra from nucleus '',I3,A2,
     &         '' will be stored as inclusive'')') i2, SYMb(nnuc)
            ENDIF
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'BNDG  ') THEN
            IF (i3.LE.0 .OR. i3.GT.NEJcm) THEN
               WRITE (8,
     &                '('' EJECTILE IDENTIFICATION '',I2,'' UNKNOWN'')')
     &                i3
               WRITE (8,'('' BINDING ENERGY SETTING IGNORED'')')
               GOTO 100
            ENDIF
            izar = i1*1000 + i2
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (8,'('' NUCLEUS A,Z ='',I3,'','',I3,
     &                '' NOT NEEDED'')') i2,i1
               WRITE (8,'('' BINDING ENERGY SETTING IGNORED'')')
               GOTO 100
            ENDIF
            if(i4.ne.0 .and. IOPran.ne.0) then
               WRITE (8,
     & '('' Binding energy uncertainty of the projectile '',A2,'' in '',
     &   I3,A2,'' is equal to '',i2,''%'')')
     &   SYMbe(i3), i2, SYMb(nnuc), i4
               sigma = val*i4*0.01
               IF(IOPran.gt.0) then
                 Q(i3,nnuc) = val + grand()*sigma
               ELSE
                 Q(i3,nnuc) = val + 1.732d0*(2*drand()-1.)*sigma
               ENDIF
               WRITE (8,
     & '('' Binding energy of '',A2,'' in '',I3,A2,
     &   '' sampled value ''  ,F7.3)'
     & ) SYMbe(i3), i2, SYMb(nnuc), Q(i3,nnuc)
            else
               Q(i3,nnuc) = val
               WRITE (8,
     & '('' Binding energy of '',A2,'' in '',I3,A2,'' set to ''  ,F7.3)'
     & ) SYMbe(i3), i2, SYMb(nnuc), val
            endif
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
               IF (val.GT.0.0D0) WRITE (8,
     &    '('' Stability limit set to spin '',F6.1,'' for all nuclei'')'
     &    ) val
               GOTO 100
            ENDIF
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (8,'('' NUCLEUS A,Z ='',I3,'','',I3,
     &                '' NOT NEEDED'')') i2,i1
               WRITE (8,'('' SETTING STABILITY LIMIT IGNORED'')')
               GOTO 100
            ENDIF
            JSTab(nnuc) = val
            WRITE (8,
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
               IF (val.GT.0.0D0) WRITE (8,
     &       '('' L. d. a-parameter set to '',F6.2,'' for all nuclei'')'
     &       ) val
               IF (val.EQ.0.0D0) WRITE (8,
     &      '('' L. d. a-parameter according to Ignatyuk systematics'')'
     &      )
               IF (val.EQ.( - 1.D0)) WRITE (8,
     &        '('' L. d. a-parameter according to Arthur systematics'')'
     &        )
               IF (val.EQ.( - 2.D0)) WRITE (8,
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
               WRITE (8,'('' NUCLEUS A,Z ='',I3,'','',I3,
     &                '' NOT NEEDED'')') i2,i1
               WRITE (8,'('' L.D. a-PARAMETER SETTING IGNORED'')')
               GOTO 100
            ENDIF
            ROPaa(nnuc) = val
            WRITE (8,
     & '('' L.d. a-parameter   in '',I3,A2,'' set to ''          ,F6.2)'
     & ) i2, SYMb(nnuc), val
            WRITE (12,
     & '('' L.d. a-parameter   in '',I3,A2,'' set to ''          ,F6.2)'
     & ) i2, SYMb(nnuc), val
            GOTO 100
         ENDIF
C-----
         IF (NAME.EQ.'SHELNO') THEN
            izar = i1*1000 + i2
            IF (izar.EQ.0) THEN
             if(i3.ne.0 .and. IOPran.ne.0) then
              WRITE (8,
     &        '('' Shell correction uncertainty '',
     &        '' is equal to '',i2,''%'')') i3
              sigma = val*i3*0.01
              IF(IOPran.gt.0) then
                shelss = val + grand()*sigma
              ELSE
                shelss = val + 1.732d0*(2*drand()-1.)*sigma
              ENDIF
              DO i = 0, NDNUC
                SHLlnor(i) = shelss
              ENDDO
              WRITE (8,
     &       '('' Shell correction in all nuclei multiplied by '',F6.2)'
     &       ) shelss
              IPArCOV = IPArCOV +1
              write(95,'(1x,i5,1x,d12.6,1x,2i13)')
     &          IPArCOV, shelss,INDexf,INDexb
             else
              DO i = 0, NDNUC
                SHLlnor(i) = val
              ENDDO
              WRITE (8,
     &       '('' Shell correction in all nuclei multiplied by '',F6.2)'
     &       ) val
             endif
             GOTO 100
            ENDIF
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (8,'('' NUCLEUS A,Z ='',I3,'','',I3,
     &                '' NOT NEEDED'')') i2,i1
               WRITE (8,'('' NORMALIZATION OF 
     &                dw shell correction IGNORED'')')
               GOTO 100
            ENDIF

            if(i3.ne.0 .and. IOPran.ne.0) then
              WRITE (8,
     &        '('' Shell correction dw uncertainty in '',I3,A2,
     &        '' is equal to '',i2,''%'')') i2, SYMb(nnuc), i3
               sigma = val*i3*0.01
              IF(IOPran.gt.0) then
                SHLlnor(nnuc) = val + grand()*sigma
              ELSE
                SHLlnor(nnuc) = val + 1.732d0*(2*drand()-1.)*sigma
              ENDIF
              WRITE (8,
     &        '('' Shell correction dw sampled value : '',f8.3)')
     &        SHLlnor(nnuc)
               IPArCOV = IPArCOV +1
               write(95,'(1x,i5,1x,d12.6,1x,2i13)')
     &              IPArCOV, SHLlnor(nnuc),INDexf,INDexb
            else
              SHLlnor(nnuc) = val
              WRITE (8,
     &        '('' Shell correction dw in '',I3,A2,'' multiplied by
     &        '',F6.2)') i2, SYMb(nnuc), val
            endif

            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'ATILNO') THEN
            izar = i1*1000 + i2
            IF (izar.EQ.0) THEN
             if(i3.ne.0 .and. IOPran.ne.0) then
              WRITE (8,
     &        '('' Global L.d. a-parameter uncertainty '',
     &        '' is equal to '',i2,''%'')') i3
              sigma = val*i3*0.01
              IF(IOPran.gt.0) then
                atilss = val + grand()*sigma
              ELSE
                atilss = val + 1.732d0*(2*drand()-1.)*sigma
              ENDIF
              DO i = 1, NDNUC
                ATIlnor(i) = atilss
              ENDDO
              WRITE (8,
     &       '('' L.d. a-parameter in all nuclei multiplied by '',F6.2)'
     &       ) atilss
              IPArCOV = IPArCOV +1
              write(95,'(1x,i5,1x,d12.6,1x,2i13)')
     &          IPArCOV, atilss,INDexf,INDexb
             else
              DO i = 1, NDNUC
                ATIlnor(i) = val
              ENDDO
              WRITE (8,
     &       '('' L.d. a-parameter in all nuclei multiplied by '',F6.2)'
     &       ) val
             endif
             GOTO 100
            ENDIF
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (8,'('' NUCLEUS A,Z ='',I3,'','',I3,
     &                '' NOT NEEDED'')') i2,i1
               WRITE (8,'('' NORMALIZATION OF a-tilde IGNORED'')')
               GOTO 100
            ENDIF

            if(i3.ne.0 .and. IOPran.ne.0) then
              WRITE (8,
     &        '('' L.d. a-parameter uncertainty in '',I3,A2,
     &        '' is equal to '',i2,''%'')') i2, SYMb(nnuc), i3
               sigma = val*i3*0.01
              IF(IOPran.gt.0) then
                ATIlnor(nnuc) = val + grand()*sigma
              ELSE
                ATIlnor(nnuc) = val + 1.732d0*(2*drand()-1.)*sigma
              ENDIF
              WRITE (8,
     &        '('' L.d. a-parameter sampled value : '',f8.3)')
     &        ATIlnor(nnuc)
               IPArCOV = IPArCOV +1
               write(95,'(1x,i5,1x,d12.6,1x,2i13)')
     &              IPArCOV, ATIlnor(nnuc),INDexf,INDexb
            else
              ATIlnor(nnuc) = val
              WRITE (8,
     &      '('' L.d. a-parameter in '',I3,A2,'' multiplied by '',F6.2)'
     &        ) i2, SYMb(nnuc), val
            endif

            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'LDSHIF') THEN
            izar = i1*1000 + i2
            IF (izar.EQ.0) THEN
               DO nnuc = 1, NDNUC
                  LDShif(nnuc) = val - 1.0
               ENDDO
               WRITE (8,'('' LDSHIFT  in all nuclei set to '',F6.3)')
     &                val
               WRITE (12,'('' LDSHIFT  in all nuclei set to '',F6.3)')
     &                val
               GOTO 100
            ENDIF
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (8,'('' NUCLEUS A,Z ='',I3,'','',I3,
     &                '' NOT NEEDED'')') i2,i1
               WRITE (8,'('' FISBAR SETTING IGNORED'')')
               GOTO 100
            ENDIF
            LDShif(nnuc) = val - 1.0
            WRITE (8,
     &           '('' LDSHIFT  in '',I3,A2,'' set to ''          ,F6.3)'
     &            ) i2, SYMb(nnuc), val
            WRITE (12,
     &           '('' LDSHIFT  in '',I3,A2,'' set to ''          ,F6.3)'
     &            ) i2, SYMb(nnuc), val
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'GTILNO') THEN
            izar = i1*1000 + i2
            IF (izar.EQ.0) THEN
               DO i = 1, NDNUC
                  GTIlnor(i) = val
               ENDDO
               WRITE (8,
     &     '('' Single particle l.d. parameter g multiplied by '',F6.2)'
     &     ) val
               WRITE (12,
     &     '('' Single particle l.d. parameter g multiplied by '',F6.2)'
     &     ) val
               GOTO 100
            ENDIF
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (8,'('' NUCLEUS A,Z ='',I3,'','',I3,
     &                '' NOT NEEDED'')') i2,i1
               WRITE (8,'('' NORMALIZATION OF G-tilde IGNORED'')')
               GOTO 100
            ENDIF

            if(i3.ne.0 .and. IOPran.ne.0) then
              WRITE (8,
     &        '('' Single particle l.d. parameter g uncertainty in '',
     &        I3,A2,'' is equal to '',i2,''%'')') i2, SYMb(nnuc), i3
              sigma = val*i3*0.01
              IF(IOPran.gt.0) then
                GTIlnor(nnuc) = val + grand()*sigma
              ELSE
                GTIlnor(nnuc) = val + 1.732d0*(2*drand()-1.)*sigma
              ENDIF
              WRITE (8,
     &        '('' Single particle l.d. parameter g sampled value : '',
     &        f8.3)') GTIlnor(nnuc)
               IPArCOV = IPArCOV +1
               write(95,'(1x,i5,1x,d12.6,1x,2i13)')
     &              IPArCOV, GTIlnor(nnuc),INDexf,INDexb
            else
              GTIlnor(nnuc) = val
              WRITE (8,
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
               WRITE (8,'('' NUCLEUS A,Z ='',I3,'','',I3,
     &                '' NOT NEEDED'')') i2,i1
               WRITE (8,'('' L.D. PARAMETER Ux SETTING IGNORED'')')
               GOTO 100
            ENDIF
            ROPar(2,nnuc) = val
            WRITE (8,
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
               WRITE (8,'('' NUCLEUS A,Z ='',I3,'','',I3,
     &                '' NOT NEEDED'')') i2,i1
               WRITE (8,'('' PAIRING SHIFT SETTING IGNORED'')')
               GOTO 100
            ENDIF
            ROPar(3,nnuc) = val
            WRITE (8,
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
               WRITE (8,'('' NUCLEUS A,Z ='',I3,'','',I3,
     &                '' NOT NEEDED'')') i2,i1
               WRITE (8,'('' L.D. PARAMETER E0 SETTING IGNORED'')')
               GOTO 100
            ENDIF
            ROPar(4,nnuc) = val
            WRITE (8,
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
               WRITE (8,'('' NUCLEUS A,Z ='',I3,'','',I3,
     &                '' NOT NEEDED'')') i2,i1
               WRITE (8,'('' L.D. PARAMETER T  SETTING IGNORED'')')
               GOTO 100
            ENDIF
            ROPar(5,nnuc) = val
            WRITE (8,
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
            IF (FITomp.GT.0) WRITE (8,
     &'('' OM parameter adjustment has been performed,'',
     & '' (several options will be reset.'')')
            IF (FITomp.LT.0) WRITE (8,
     &'('' Automatic OM parameter adjustment was selected,'',
     & '' (several options will be reset.'')')
            GOTO 100
         ENDIF

C-----Sensitivity calculations for KALMAN
         IF (name.EQ.'KALMAN') THEN
            KALman = val
            IF (KALman.GT.0) WRITE (8,
     &'('' Sensitivity calculations will be performed,'',
     & '' (will reset ENDF option to 0'')')
            GOTO 100
         ENDIF

C--------Tuning factors
         IF (name.EQ.'TUNEPE') THEN
            IF (i1.GT.NDEJC) THEN
               WRITE (8,'('' UNKNOWN EJECTILE in PE TUNE '',I2)') i1
               GOTO 100
            ENDIF
            if(i2.gt.0. .and. IOPran.ne.0) then
              WRITE (8,
     &'('' PE emission width of ejectile '',I1,'' from '',I3,A2,
     &         '' uncertainty is equal to '',i2,'' %'')')
     &        i1, NINT(A(1)), SYMb(1), i2
              sigma = val*0.01*i2
              IF(IOPran.gt.0) then
                IF(rTUNEpe(i1).eq.1.d0) rTUNEpe(i1)=grand()
                TUNEpe(i1) = val + rTUNEpe(i1)*sigma
              ELSE
                IF(rTUNEpe(i1).eq.1.d0) rTUNEpe(i1)=drand()
                TUNEpe(i1) = val + 1.732d0*(2*rTUNEpe(i1)-1.)*sigma
              ENDIF
              WRITE (8,
     &'('' PE emission width of ejectile '',I1,'' from '',I3,A2,
     &  '' multiplied by '',F6.3)') i1, NINT(A(1)), SYMb(1),
     & TUNEpe(i1)
              IPArCOV = IPArCOV +1
              WRITE(95,'(1x,i5,1x,d12.6,1x,2i13)')
     &           IPArCOV, TUNEpe(i1), INDexf,INDexb
            else
              TUNEpe(i1) = val
              WRITE (8,
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
               WRITE (8,'('' NUCLEUS A,Z ='',I3,'','',I3,
     &                '' NOT NEEDED'')') i2,i1
               WRITE (8,'('' FISSION DECAY WIDTH TUNING IGNORED'')')
               GOTO 100
            ENDIF
            if(i3.gt.0. .and. IOPran.ne.0) then
              WRITE (8,
     &'('' Uncertainty of the Fission decay width of nucleus '',I3,A2,
     &         '' is equal to '',i2,'' %'')')
     &         i2, SYMb(nnuc), i3
              sigma = val*0.01*i3
              IF(IOPran.gt.0) then
                IF(rTUNEfi(nnuc).eq.1.d0) rTUNEfi(nnuc)=grand()
                TUNefi(nnuc) = val + rTUNEfi(nnuc)*sigma
              ELSE
                IF(rTUNEfi(nnuc).eq.1.d0) rTUNEfi(nnuc)=drand()
                TUNefi(nnuc) = val + 1.732d0*(2*rTUNEfi(nnuc)-1.)*sigma
              ENDIF
              WRITE (8,
     &'('' Fission decay width of nucleus '',I3,A2,
     &  '' multiplied by '',F7.3)') i2, SYMb(nnuc), TUNefi(nnuc)
              IPArCOV = IPArCOV +1
               write(95,'(1x,i5,1x,d12.6,1x,2i13)')
     &       IPArCOV, TUNefi(nnuc), INDexf,INDexb
 
            else
 
              TUNefi(nnuc) = val
              WRITE (8,
     &'('' Fission decay width of nucleus '',I3,A2,
     &  '' multiplied by '',F7.3)') i2, SYMb(nnuc), val
              WRITE (12,
     &'('' Fission decay width of nucleus '',I3,A2,
     &  '' multiplied by '',F7.3)') i2, SYMb(nnuc), val
 
            endif
            GOTO 100
         ENDIF

         IF (name.EQ.'TUNE  ') THEN
            izar = i1*1000 + i2
            IF (izar.EQ.0) THEN
               DO nnuc = 1, NDNUC
                  TUNe(i3,nnuc) = val
               ENDDO
               WRITE (8,*) 'Strength function for ',i3,
     &            ' in all nuclei scaled by ', val
               WRITE (12,*) 'Strength function for ',i3,
     &            ' in all nuclei scaled by ', val
               GOTO 100
            ENDIF
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (8,'('' NUCLEUS A,Z ='',I3,'','',I3,
     &                '' NOT NEEDED'')') i2,i1
               WRITE (8,'('' TUNING IGNORED'')')
               GOTO 100
            ENDIF
            IF (i3.GT.NDEJC) THEN
               WRITE (8,'('' UNKNOWN EJECTILE in TUNE '',I2)') i3
               GOTO 100
            ENDIF
            if(i4.gt.0. .and. IOPran.ne.0) then
              WRITE (8,
     &'('' Emission width of ejectile '',I1,'' from '',I3,A2,
     &         '' uncertainty is equal to '',i2,'' %'')')
     &        i3, i2, SYMb(nnuc), i4
              sigma = val*0.01*i4
              IF(IOPran.gt.0) then
                IF(rTUNE(i3,nnuc).eq.1.d0) rTUNE(i3,nnuc)=grand()
                TUNe(i3,nnuc) = val + rTUNE(i3,nnuc)*sigma
              ELSE
                IF(rTUNE(i3,nnuc).eq.1.d0) rTUNE(i3,nnuc)=drand()
                TUNe(i3,nnuc) = val +
     &                          1.732d0*(2*rTUNE(i3,nnuc)-1.)*sigma
              ENDIF
              WRITE (8,
     &'('' Emission width of ejectile '',I1,'' from '',I3,A2,
     &  '' multiplied by '',F7.3)') i3, i2, SYMb(nnuc), TUNe(i3,nnuc)
              IPArCOV = IPArCOV +1
               write(95,'(1x,i5,1x,d12.6,1x,2i13)')
     &       IPArCOV, TUNe(i3,nnuc), INDexf,INDexb
             else
              TUNe(i3,nnuc) = val
              WRITE (8,
     &'('' Emission width of ejectile '',I1,'' from '',I3,A2,
     &         '' multiplied by '',F7.3)') i3, i2, SYMb(nnuc), val
              WRITE (12,
     &'('' Emission width of ejectile '',I1,'' from '',I3,A2,
     &  '' multiplied by '',F7.3)') i3, i2, SYMb(nnuc), TUNe(i3,nnuc)
            endif
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'DEFDYN') THEN
            IF(val.gt.0) DEFdyn = val
            IF(i1.gt.0. .and. IOPran.ne.0) then
              WRITE (8,
     &'('' DWBA dynamical deformation uncertainty is equal to '',
     &i2,'' %'')') i1
              sigma = val*0.01*i1
              IF(IOPran.gt.0) then
                DEFdyn = val + grand()*sigma
              ELSE
                DEFdyn = val + 1.732d0*(2*drand()-1.)*sigma
              ENDIF
              IF (DEFdyn.LT.0.) DEFdyn = val
              WRITE (8,
     &'('' DWBA dynamical deformations multiplied by'',F6.3)') DEFdyn
              IPArCOV = IPArCOV +1
              write(95,'(1x,i5,1x,d12.6,1x,2i13)')
     &           IPArCOV, DEFdyn, INDexf,INDexb
            ELSE
              WRITE (8,
     &'('' DWBA dynamical deformations multiplied by'',F6.3)') val
              WRITE (12,
     &'('' DWBA dynamical deformations multiplied by'',F6.3)') val
           ENDIF
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'DEFSTA') THEN
            IF(val.gt.0) DEFsta = val
            IF(i1.gt.0. .and. IOPran.ne.0) then
              WRITE (8,
     &'('' CC static deformation uncertainty is equal to '',
     &i2,'' %'')') i1
              sigma = val*0.01*i1
              IF(IOPran.gt.0) then
                DEFsta = val + grand()*sigma
              ELSE
                DEFsta = val + 1.732d0*(2*drand()-1.)*sigma
              ENDIF
              IF (DEFsta.LT.0.) DEFsta = val
              WRITE (8,
     &'('' CC static deformation multiplied by'',F6.3)') DEFSTA
              IPArCOV = IPArCOV +1
              write(95,'(1x,i5,1x,d12.6,1x,2i13)')
     &           IPArCOV, DEFsta, INDexf,INDexb
            ELSE
              WRITE (8,
     &'('' CC static deformation multiplied by'',F6.3)') val
              WRITE (12,
     &'('' CC static deformation multiplied by'',F6.3)') val
            ENDIF
            GOTO 100
         ENDIF

C--------input for TRISTAN (MSD)
         IF (name.EQ.'MSDMIN') THEN
            EMInmsd = val
            WRITE (8,
     &'('' MSD calculations starting at '',F6.3,'' MeV'')
     &') EMInmsd
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'WIDEX ') THEN
            WIDexin = val
            IF (WIDexin.GT.0.0D0) WRITE (8,
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
               WRITE (8,
     &'('' Compressional l=0 form factor''
     &,'' used in MSD calculations'')')
               WRITE (12,
     &'('' Compressional l=0 form factor''
     &,'' used in MSD calculations'')')
               WRITE (8,
     &'('' This option is not compatible with''
     &,'' the use of dispersive optical model''
     &,'' for the incident channel'')')
               WRITE (12,
     &'('' This option is not compatible with''
     &,'' the use of dispersive optical model''
     &,'' for the incident channel'')')
            ELSE
               WRITE (8,
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
            IF (GAPin(2).GT.0.0D0) WRITE (8,
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
            IF (GAPin(1).GT.0.0D0) WRITE (8,
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
            IF (HOMin.GT.0.0D0) WRITE (8,
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
            IF (ALSin.GT.0.0D0) WRITE (8,
     &'('' l*s coupling strength in harmonic oscill. (MSD)'',   F6.3,''
     &MeV'')') ALSin
            IF (ALSin.GT.0.0D0) WRITE (12,
     &'('' l*s coupling strength in harmonic oscill. (MSD)'',   F6.3,''
     &MeV'')') ALSin
            GOTO 100
         ENDIF
C-----
       IF (name.EQ.'EFIT  ') THEN
            EFItin(i1 + 1,i2+1) = val                                    ! nilsson
            IF (EFItin(i1 + 1,i2+1).GT.0.0D0) then                       ! nilsson
               if (i2.eq.0) then                                         ! nilsson
                  WRITE (8,
     &'('' Field strength of multipolarity'',I2,'' or multipolarity/k'', ! nilsson
     &I2''/''I1,'' fitted to the level at '',F6.3,'' MeV'')')i1, i1,i2,  ! nilsson
     &EFItin(i1 + 1,i2 + 1)                                              ! nilsson
                  WRITE (12,
     &'('' Field strength of multipolarity'',I2,'' or multipolarity/k'', ! nilsson
     &I2''/''I1,'' fitted to the level at '',F6.3,'' MeV'')')i1, i1,i2,  ! nilsson
     &EFItin(i1 + 1,i2 + 1)                                              ! nilsson
               else                                                      ! nilsson
                  WRITE (8,
     &'('' Field strength of multipolarity/k'',I2''/''I1,''fitted to the ! nilsson
     & level at '',F6.3,'' MeV'')') i1,i2,EFItin(i1 + 1,i2 + 1)          ! nilsson
                  WRITE (12,
     &'('' Field strength of multipolarity/k'',I2''/''I1,''fitted to the ! nilsson
     & level at '',F6.3,'' MeV'')') i1,i2,EFItin(i1 + 1,i2 + 1)          ! nilsson
               endif                                                     ! nilsson
            ENDIF                                                        ! nilsson
            GOTO 100
         ENDIF
C-----
c        IF (name.EQ.'EFIT  ') THEN
c           EFItin(i1 + 1) = val
c           IF (EFItin(i1 + 1).GT.0.0D0) WRITE (8,
c    &'('' Field strength of multipolarity'',I2,'' fitted to the level a
c    &t '',F6.3,'' MeV'')') i1, EFItin(i1 + 1)
c           IF (EFItin(i1 + 1).GT.0.0D0) WRITE (12,
c    &'('' Field strength of multipolarity'',I2,'' fitted to the level a
c    &t '',F6.3,'' MeV'')') i1, EFItin(i1 + 1)
c           GOTO 100
c        ENDIF
C-----
         IF (name.EQ.'RESNOR') THEN
            IF(i1.EQ.0) THEN
               DO i = 1, 5
                  DO j = 1,i + 1
                     CNOrin(i + 1,j) = val
                  ENDDO
               ENDDO
               WRITE (8,
     &'('' All response functions in MSD normalized by factor '',F6.3)')
     &         val
               WRITE (12,
     &'('' All response functions in MSD normalized by factor '',F6.3)')
     &         val
               GOTO 100
            ELSE IF(i2.EQ.0) THEN                                        ! nilsson
               DO j = 1, i1 + 1                                          ! nilsson
                  CNOrin(i1 + 1,j) = val                                 ! nilsson
               ENDDO                                                     ! nilsson
               WRITE (8,
     &'('' Response function for l transfer'',I2,'' or l/k transfer''    ! nilsson
     &I2,''/''I1,'' normalized by factor'',F6.3)') i1,i1,i2, val         ! nilsson
               WRITE (12,
     &'('' Response function for l transfer'', I3,'' or l/k transfer''   ! nilsson
     &I2,''/''I1,'' normalized by factor '',F6.3)') i1,i1,i2, val        ! nilsson
               GOTO 100
            ELSE
               CNOrin(i1 + 1,i2 + 1) = val                               ! nilsson
               WRITE (8,
     &'('' Response function for l/k transfer'', I2,''/''I1,'' normalize ! nilsson
     &d by factor '',F6.3)') i1,i2, CNOrin(i1 + 1,i2 + 1)                ! nilsson
               WRITE (12,
     &'('' Response function for l/k transfer'', I2,''/''I1,'' normalize ! nilsson
     &d by factor '',F6.3)') i1,i2, CNOrin(i1 + 1,i2 + 1)                ! nilsson
               GOTO 100
            ENDIF
         ENDIF
c        IF (name.EQ.'RESNOR') THEN
c           IF(i1.EQ.0) THEN
c              DO i=1,5
c                 CNOrin(i + 1) = val
c              ENDDO
c              WRITE (8,
c    &'('' Response functions for first 5 l transfers in MSD normalized
c    &by factor '',F6.3)') val
c              WRITE (12,
c    &'('' Response functions for first 5 l transfers in MSD normalized
c    &by factor '',F6.3)') val
c           GOTO 100
c           ENDIF
c           CNOrin(i1 + 1) = val
c           WRITE (8,
c    &'('' Response function for l transfer'',I3,'' normalized by factor
c    & '',F6.3)') i1, CNOrin(i1 + 1)
c           WRITE (12,
c    &'('' Response function for l transfer'',I3,'' normalized by factor
c    & '',F6.3)') i1, CNOrin(i1 + 1)
c           GOTO 100
c        ENDIF
C-----
         IF (name.EQ.'DEFMSD') THEN
            BET2in = val
            IF (BET2in.GT.0.0D0) WRITE (8,
     &'('' beta_2 deformation in Nilsson Hamiltonian (MSD)'', F6.3
     &)') BET2in
            IF (BET2in.GT.0.0D0) WRITE (12,
     &'('' beta_2 deformation in Nilsson Hamiltonian (MSD)'', F6.3
     &)') BET2in
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'GRANGP') THEN
            GRin(1) = val
            IF (GRin(1).GT.0.0D0) WRITE (8,
     &'('' Energy range for pairing calculations (MSD prot.)'',F6.3,''
     &MeV'')') GRin(1)
            IF (GRin(1).GT.0.0D0) WRITE (12,
     &'('' Energy range for pairing calculations (MSD prot.)'',F6.3,''
     &MeV'')') GRin(1)
            GOTO 100
         ENDIF
C----
         IF (name.EQ.'GRANGN') THEN
            GRin(2) = val
            IF (GRin(2).GT.0.0D0) WRITE (8,
     &'('' Energy range for pairing calculations (MSD neut.)'',F6.3,''
     & MeV'')') GRin(2)
            IF (GRin(2).GT.0.0D0) WRITE (12,
     &'('' Energy range for pairing calculations (MSD neut.)'',F6.3,''
     & MeV'')') GRin(2)
            GOTO 100
         ENDIF
C--------TRISTAN (MSD) input **** done ****

         IF (name.EQ.'NIXSH ') THEN
            SHNix = val
            IF (SHNix.NE.0.0D0) WRITE (8,
     &                '('' Shell corrections according to Nix-Moller'')'
     &                )
            IF (SHNix.EQ.0.0D0) WRITE (8,
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
            IF (GCAsc.GT.0.0D0) WRITE (8,
     &       '('' Full gamma cascade in the first CN selected'')')
            IF (GCAsc.EQ.0.0D0) WRITE (8,
     &       '('' Full gamma cascade is not followed'')')
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'E1    ') THEN
            IF (val.GT.0.0D+0) THEN
               WRITE (8,'('' E1 photo-absorption selected'')')
               WRITE (12,'('' E1 photo-absorption selected'')')
            ELSE
               WRITE (8,'('' E1 photo-absorption blocked'')')
               WRITE (12,'('' E1 photo-absorption blocked'')')
            ENDIF
            IGE1 = val
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'M1    ') THEN
            IF (val.GT.0.0D+0) THEN
               WRITE (8,'('' M1 photo-absorption selected'')')
               WRITE (12,'('' M1 photo-absorption selected'')')
            ELSE
               WRITE (8,'('' M1 photo-absorption blocked'')')
               WRITE (12,'('' M1 photo-absorption blocked'')')
            ENDIF
            IGM1 = val
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'E2    ') THEN
            IF (val.GT.0.0D+0) THEN
               WRITE (8,'('' E2 photo-absorption selected'')')
               WRITE (12,'('' E2 photo-absorption selected'')')
            ELSE
               WRITE (8,'('' E2 photo-absorption blocked'')')
               WRITE (12,'('' E2 photo-absorption blocked'')')
            ENDIF
            IGE2 = val
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'QD    ') THEN
            IF (IGE1.EQ.0) THEN
               WRITE (8,
     &'('' Quasideuteron photo-absorption is '',          '' suppressed
     &since E1 photo-absorption is blocked!'')')
               WRITE (12,
     &'('' Quasideuteron photo-absorption is '',          '' suppressed
     &since E1 photo-absorption is blocked!'')')
            ELSE
               LQDfac = val
               WRITE (8,
     &'('' Quasideuteron photoabsorption cross section'',
     &  '' normalized by a factor '',F6.3)') LQDfac
               WRITE (12,
     &'('' Quasideuteron photoabsorption cross section'',
     &  '' normalized by a factor '',F6.3)') LQDfac
            ENDIF
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'DXSRED') THEN
            IF (val.LE.0.d0) THEN
             DXSred = 0.d0
               WRITE ( 8,
     &'('' Deuteron break-up/pick-up suppressed!'')')
               WRITE (12,
     &'('' Deuteron break-up/pick-up suppressed!'')')
            ELSE
               DXSred = val
               WRITE ( 8,
     &'('' Deuteron break-up/pick-up cross section'',
     &  '' normalized by a factor '',F6.3)') DXSred
               WRITE (12,
     &'('' Deuteron break-up/pick-up cross section'',
     &  '' normalized by a factor '',F6.3)') DXSred
            ENDIF
            GOTO 100
         ENDIF
C-----
C-----shift parameter used to adjust HFB LD
         IF (name.EQ.'ROHFBP') THEN
            izar = i1*1000 + i2
            IF (izar.EQ.0) THEN
             if(i3.ne.0 .and. IOPran.ne.0) then
              WRITE (8,
     &        '('' Global GS HFB L.D. shift uncertainty '',
     &        '' is equal to '',i2,''%'')') i3
              sigma = val*i3*0.01
              IF(IOPran.gt.0) then
                atilss = val + grand()*sigma
              ELSE
                atilss = val + 1.732d0*(2*drand()-1.)*sigma
              ENDIF
              DO i = 1, NDNUC
                ROHfbp(nnuc) = atilss
              ENDDO
              WRITE (8,
     &'('' GS HFB L.D. shift in all nuclei set to '',F8.3)') atilss
              IPArCOV = IPArCOV +1
              write(95,'(1x,i5,1x,d12.6,1x,2i13)')
     &          IPArCOV, atilss,INDexf,INDexb
             else
              DO i = 1, NDNUC
                ROHfbp(nnuc) = val
              ENDDO
              WRITE (8,'('' GS HFB L.D. shift in all nuclei set to '',
     & F8.3)') val
             endif
             GOTO 100
            ENDIF

            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
             WRITE (8,'('' NUCLEUS A,Z ='',I3,'','',I3,
     &                '' NOT NEEDED'')') i2,i1
             WRITE (8,'(''  GS HFB L.D. SHIFT SETTING IGNORED'')')
             GOTO 100
            ENDIF
            if(i3.ne.0 .and. IOPran.ne.0) then
             WRITE (8,
     &        '('' GS HFB L.D. shift uncertainty in '',I3,A2,
     &        '' is equal to '',i2,''%'')') i2, SYMb(nnuc), i3
             sigma = val*i3*0.01
             IF(IOPran.gt.0) then
               ROHfbp(nnuc) = val + grand()*sigma
             ELSE
               ROHfbp(nnuc) = val + 1.732d0*(2*drand()-1.)*sigma
             ENDIF
             WRITE (8,
     &        '('' GS HFB L.D. shift sampled value : '',f8.3)')
     &       ROHfbp(nnuc)
             IPArCOV = IPArCOV +1
             write(95,'(1x,i5,1x,d12.6,1x,2i13)')
     &              IPArCOV, ROHfbp(nnuc),INDexf,INDexb
            else
              ROHfbp(nnuc) = val
              WRITE (8,
     &      '('' GS HFB L.D. shift in '',I3,A2,'' set to '',F8.3)'
     &        ) i2, SYMb(nnuc), val
              WRITE (12,
     &      '('' GS HFB L.D. shift in '',I3,A2,'' set to '',F8.3)'
     &        ) i2, SYMb(nnuc), val
            endif
            GOTO 100
         ENDIF
C-----pseudo a-parameter used to adjust HFB LD
         IF (name.EQ.'ROHFBA') THEN
            izar = i1*1000 + i2
            IF (izar.EQ.0) THEN
             if(i3.ne.0 .and. IOPran.ne.0) then
              WRITE (8,
     &        '('' Global GS HFB normalization uncertainty '',
     &        '' is equal to '',i2,''%'')') i3
              sigma = val*i3*0.01
              IF(IOPran.gt.0) then
                atilss = val + grand()*sigma
              ELSE
                atilss = val + 1.732d0*(2*drand()-1.)*sigma
              ENDIF
              DO i = 1, NDNUC
                ROHfba(nnuc) = atilss
              ENDDO
              WRITE (8,
     &'('' GS HFB L.D. norm in all nuclei set to '',F8.3)') atilss
              IPArCOV = IPArCOV +1
              write(95,'(1x,i5,1x,d12.6,1x,2i13)')
     &          IPArCOV, atilss,INDexf,INDexb
             else
              DO i = 1, NDNUC
                ROHfba(nnuc) = val
              ENDDO
              WRITE (8,'('' GS HFB L.D. norm in all nuclei set to '',
     & F8.3)') val
             endif
             GOTO 100
            ENDIF

            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
             WRITE (8,'('' NUCLEUS A,Z ='',I3,'','',I3,
     &                '' NOT NEEDED'')') i2,i1
             WRITE (8,'(''  GS HFB L.D. norm SETTING IGNORED'')')
             GOTO 100
            ENDIF
            if(i3.ne.0 .and. IOPran.ne.0) then
             WRITE (8,
     &        '('' GS HFB L.D. normalization uncertainty in '',I3,A2,
     &        '' is equal to '',i2,''%'')') i2, SYMb(nnuc), i3
             sigma = val*i3*0.01
             IF(IOPran.gt.0) then
               ROHfba(nnuc) = val + grand()*sigma
             ELSE
               ROHfba(nnuc) = val + 1.732d0*(2*drand()-1.)*sigma
             ENDIF
             WRITE (8,
     &        '('' GS HFB L.D. norm sampled value : '',f8.3)')
     &       ROHfba(nnuc)
             IPArCOV = IPArCOV +1
             write(95,'(1x,i5,1x,d12.6,1x,2i13)')
     &              IPArCOV, ROHfbp(nnuc),INDexf,INDexb
            else
              ROHfba(nnuc) = val
              WRITE (8,
     &      '('' GS HFB L.D. norm in '',I3,A2,'' set to '',F8.3)'
     &        ) i2, SYMb(nnuc), val
              WRITE (12,
     &      '('' GS HFB L.D. norm in '',I3,A2,'' set to '',F8.3)'
     &        ) i2, SYMb(nnuc), val
            endif
            GOTO 100
         ENDIF
C--------FISSION
C--------checking for fission data in the optional input
         IF (name.EQ.'FISSHI') THEN
            izar = i1*1000 + i2
            IF (val.EQ.0) THEN
               fstring = 'advanced treatment of fission'
            ELSEIF (val.EQ.1) THEN
               fstring = 'HI-fission over 1-humped barrier'
            ELSEIF (val.EQ.2) THEN
               fstring = 'fission ignored'
            ELSE
              WRITE (8,
     &          '('' ERROR: FISSHI must be 0,1,2; default 0 used '')')
              FISSHI = 0
              GOTO 100
            ENDIF
            IF (izar.EQ.0) THEN
               DO nnuc = 1, NDNUC
                  FISshi(nnuc) = val
               ENDDO
               WRITE (8,*) 'For all nuclei: ', fstring
               WRITE (12,*) 'For all nuclei: ', fstring
               GOTO 100
            ENDIF
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (8,'('' NUCLEUS A,Z ='',I3,'','',I3,
     &                '' NOT NEEDED'')') i2,i1
               WRITE (8,'('' FISSHI SETTING IGNORED'')')
               GOTO 100
            ENDIF
            FISshi(nnuc) = val
            WRITE (8,*) 'For ', i2, '-', SYMb(nnuc), ' ', fstring
            WRITE (12,*) 'For ', i2, '-', SYMb(nnuc), ' ', fstring
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'FISMOD') THEN
            IF(val.lt.0 .or. val.gt.2) THEN
              WRITE (8,'('' ERROR: FISMOD ='',I1)') NINT(val)
              WRITE (8,
     &          '('' ERROR: FISMOD must be 0,1,2; default 0 used '')')
              GOTO 100
            ENDIF
            izar = i1*1000 + i2
            IF (izar.EQ.0) THEN
               DO nnuc = 1, NDNUC
                  FISmod(nnuc) = val
               ENDDO
               IF(val.eq.0) then
                 WRITE (8,
     & '('' Single-modal fission is assumed for all fiss. nuclei'')') 
                 WRITE (12,
     & '('' Single-modal fission is assumed for all fiss. nuclei'')') 
               ENDIF
               IF(val.eq.1) then
                 WRITE (8 , 
     & '('' Two-modal fission is assumed for all fiss. nuclei'')') 
                 WRITE (12, 
     & '('' Two-modal fission is assumed for all fiss. nuclei'')') 
               ENDIF
               IF(val.eq.2) then
                 WRITE (8 , 
     & '('' Three-modal fission is assumed for all fiss. nuclei'')') 
                 WRITE (12, 
     & '('' Three-modal fission is assumed for all fiss. nuclei'')') 
               ENDIF
               GOTO 100
            ENDIF
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (8,'('' WARNING: NUCLEUS A,Z ='',I3,'','',I3,
     &                '' NOT NEEDED'')') i2,i1
               WRITE (8,'('' WARNING: FISMOD SETTING IGNORED'')')
               GOTO 100
            ENDIF

            FISmod(nnuc) = val

            IF(val.eq.0) then
               WRITE (8 ,'('' Single-modal fission is assumed for '' 
     &             I3,A2,'' (FISMOD=0)'')') i2, SYMb(nnuc)
               WRITE (12,'('' Single-modal fission is assumed for '' 
     &             I3,A2,'' (FISMOD=0)'')') i2, SYMb(nnuc)
            ENDIF
            IF(val.eq.1) then
               WRITE (8 ,'('' Two-modal fission is assumed for '' 
     &             I3,A2,'' (FISMOD=1)'')') i2, SYMb(nnuc)
               WRITE (12,'('' Two-modal fission is assumed for '' 
     &             I3,A2,'' (FISMOD=1)'')') i2, SYMb(nnuc)
            ENDIF
            IF(val.eq.2) then
               WRITE (8 ,'('' Three-modal fission is assumed for '' 
     &             I3,A2,'' (FISMOD=2)'')') i2, SYMb(nnuc)
               WRITE (12,'('' Three-modal fission is assumed for '' 
     &             I3,A2,'' (FISMOD=2)'')') i2, SYMb(nnuc)
            ENDIF
            GOTO 100
         ENDIF
C-----
C        FISopt(Nnuc).EQ.0. = '  Full damping model (Ind.Barr.)'
C        FISopt(Nnuc).EQ.1. = '  Optical model for fission     '
C        FISopt(Nnuc).EQ.2. = '  Complex fission potential, isomeric fission'

         IF (name.EQ.'FISOPT') THEN
            IF(val.lt.0 .or. val.gt.2) THEN
              WRITE (8,'('' ERROR: FISOPT ='',I1)') NINT(val)
              WRITE (8,
     &          '('' ERROR: FISOPT must be 0,1,2; default 0 used '')')
              GOTO 100
            ENDIF
            izar = i1*1000 + i2
            IF (izar.EQ.0) THEN
               DO nnuc = 1, NDNUC
                  FISopt(nnuc) = val
               ENDDO
               IF(val.eq.0) then
                 WRITE (8,
     & '('' Independent fiss. barriers (full damping assumed) '')') 
                 WRITE (12,
     & '('' Independent fiss. barriers (full damping assumed) '')') 
               ENDIF
               IF(val.eq.1) then
                 WRITE (8 , 
     & '('' Optical model for fission with partial damping used '')') 
                 WRITE (12, 
     & '('' Optical model for fission with partial damping used '')') 
               ENDIF
               IF(val.eq.2) then
                 WRITE (8 , 
     &'('' Isomeric fission model including partial damping assumed'')') 
                 WRITE (12, 
     &'('' Isomeric fission model including partial damping assumed'')') 
               ENDIF
               GOTO 100
            ENDIF
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (8,'('' WARNING: NUCLEUS A,Z ='',I3,'','',I3,
     &                '' NOT NEEDED'')') i2,i1
               WRITE (8,'('' WARNING: FISOPT SETTING IGNORED'')')
               GOTO 100
            ENDIF

            FISopt(nnuc) = val

            IF(val.eq.0) then
               WRITE (8 ,
     & '('' Independent fiss. barriers (full damping) for nucleus '', 
     &             I3,A2,'' (FISOPT=0)'')') i2, SYMb(nnuc)
               WRITE (12, 
     & '('' Independent fiss. barriers (full damping) for nucleus '', 
     &             I3,A2,'' (FISOPT=0)'')') i2, SYMb(nnuc)
            ENDIF
            IF(val.eq.1) then
               WRITE (8 ,
     &'('' Optical model for fission with partial damping used for '', 
     &             I3,A2,'' (FISOPT=1)'')') i2, SYMb(nnuc)
               WRITE (12,
     &'('' Optical model for fission with partial damping used for '', 
     &             I3,A2,'' (FISOPT=1)'')') i2, SYMb(nnuc)
            ENDIF
            IF(val.eq.2) then
               WRITE (8 ,
     &'('' Isomeric fission model with partial damping used for '', 
     &             I3,A2,'' (FISOPT=2)'')') i2, SYMb(nnuc)
               WRITE (12,
     &'('' Isomeric fission model with partial damping used for '', 
     &             I3,A2,'' (FISOPT=2)'')') i2, SYMb(nnuc)
            ENDIF
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'FISBAR') THEN
            IF(val.lt.0 .or. val.GT.3) THEN
              WRITE (8,'('' ERROR: FISBAR ='',I1)') NINT(val)
              WRITE (8,
     &          '('' ERROR: FISBAR must be 0,1,2,3; default 1 used '')')
              GOTO 100
            ENDIF
            izar = i1*1000 + i2
            IF (izar.EQ.0) THEN
               DO nnuc = 1, NDNUC
                  FISbar(nnuc) = val
               ENDDO
               IF(val.eq.0) then
                 WRITE (8,
     &    '('' EMPIRE internal fission barriers used for all nuclei'')') 
                 WRITE (12,
     &    '('' EMPIRE internal fission barriers used for all nuclei'')') 
               ENDIF
               IF(val.eq.1) then
                 WRITE (8,
     &    '('' RIPL empirical fission barriers used for all nuclei'')')
                 WRITE (12,
     &    '('' RIPL empirical fission barriers used for all nuclei'')')
               ENDIF
               IF(val.eq.2) then
                 WRITE (8,
     &      '('' Parabolic approx. to RIPL HFB fiss. barr. used for all 
     &nuclei'')') 
                 WRITE (12,
     &      '('' Parabolic approx. to RIPL HFB fiss. barr. used for all 
     &nuclei'')') 
               ENDIF
               IF(val.eq.3) then
                 WRITE (8,
     &  '('' RIPL HFB numerical fission barriers used for all nuclei''
     &          )') 
                 WRITE (12,
     &  '('' RIPL HFB numerical fission barriers used for all nuclei''
     &          )') 
               ENDIF
               GOTO 100
            ENDIF
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (8,'('' WARNING: NUCLEUS A,Z ='',I3,'','',I3,
     &                '' NOT NEEDED'')') i2,i1
               WRITE (8,'('' WARNING: FISBAR SETTING IGNORED'')')
               GOTO 100
            ENDIF

            FISbar(nnuc) = val
C
C-----Fundamental barrier heights
C-----FISBAR(Nnuc)=0 EMPIRE
C-----FISBAR(Nnuc)=1 Maslov
C-----FISBAR(Nnuc)=2 HFB parabolic
C-----FISBAR(Nnuc)=3 HFB numeric
C
            IF(val.eq.0) then
               WRITE (8,
     &          '('' EMPIRE internal fission barriers used for '',
     &             I3,A2,'' (FISBAR=0)'')') i2, SYMb(nnuc)
               WRITE (12,
     &          '('' EMPIRE internal fission barriers used for '',
     &             I3,A2,'' (FISBAR=0)'')') i2, SYMb(nnuc)
            ENDIF
            IF(val.eq.1) then
               WRITE (8,
     &          '('' RIPL empirical fission barriers used for '',
     &             I3,A2,'' (FISBAR=1)'')') i2, SYMb(nnuc)
               WRITE (12,
     &          '('' RIPL empirical fission barriers used for '',
     &             I3,A2,'' (FISBAR=1)'')') i2, SYMb(nnuc)
            ENDIF
            IF(val.eq.2) then
               WRITE (8,
     &      '('' Parabolic approx. to RIPL HFB fiss. barr. used for '',
     &             I3,A2,'' (FISBAR=2)'')') i2, SYMb(nnuc)
               WRITE (12,
     &      '('' Parabolic approx. to RIPL HFB fiss. barr. used for '',
     &             I3,A2,'' (FISBAR=2)'')') i2, SYMb(nnuc)
            ENDIF
            IF(val.eq.3) then
               WRITE (8,
     &          '('' RIPL HFB numerical fission barriers used for '',
     &             I3,A2,'' (FISBAR=3)'')') i2, SYMb(nnuc)
               WRITE (12,
     &          '('' RIPL HFB numerical fission barriers used for '',
     &             I3,A2,'' (FISBAR=3)'')') i2, SYMb(nnuc)
            ENDIF
            GOTO 100
         ENDIF
C--------
C--------FISDEN(Nnuc)= 0 EMPIRE
C--------FISDEN(Nnuc)= 3 HFB
C        
         IF (name.EQ.'FISDEN') THEN
            IF(val.ne.0 .and. val.ne.3) THEN
              WRITE (8,'('' ERROR: FISDEN ='',I1)') NINT(val)
              WRITE (8,
     &          '('' ERROR: FISDEN must be 0 or 3; default 0 used '')')
              GOTO 100
            ENDIF
            izar = i1*1000 + i2
            IF (izar.EQ.0) THEN
               DO nnuc = 1, NDNUC
                  FISden(nnuc) = val
               ENDDO
               IF(val.eq.0) then
                 WRITE (8,
     &  '('' EGSM (J>>K approx.) used for fission LD for all nuclei'')') 
                 WRITE (12,
     &  '('' EGSM (J>>K approx.) used for fission LD for all nuclei'')') 
               ENDIF
               IF(val.eq.3) then
                 WRITE (8 ,'('' HFB fission LD used for all nuclei'')') 
                 WRITE (12,'('' HFB fission LD used for all nuclei'')') 
               ENDIF
               GOTO 100
            ENDIF
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (8,'('' WARNING: NUCLEUS A,Z ='',I3,'','',I3,
     &                '' NOT NEEDED'')') i2,i1
               WRITE (8,'('' WARNING: FISDEN SETTING IGNORED'')')
               GOTO 100
            ENDIF

            FISden(nnuc) = val

            IF(val.eq.0) then
               WRITE (8,
     &          '('' EGSM (J>>K approx.) used for fission LD for '',
     &             I3,A2,'' (FISDEN=0)'')') i2, SYMb(nnuc)
               WRITE (12,
     &          '('' EGSM (J>>K approx.) used for fission LD for '',
     &             I3,A2,'' (FISDEN=0)'')') i2, SYMb(nnuc)
            ENDIF
            IF(val.eq.3) then
               WRITE (8 ,'('' RIPL HFB fission LD used for '',
     &             I3,A2,'' (FISDEN=3)'')') i2, SYMb(nnuc)
               WRITE (12,'('' RIPL HFB fission LD used for '',
     &             I3,A2,'' (FISDEN=3)'')') i2, SYMb(nnuc)
            ENDIF
            GOTO 100
         ENDIF
C-----
         IF (name.EQ.'FISDIS') THEN
            IF(val.lt.0 .or. val.gt.1) THEN
              WRITE (8,'('' ERROR: FISDIS ='',I1)') NINT(val)
              WRITE (8,
     &          '('' ERROR: FISDIS must be 0 or 1; default 0 used '')')
              GOTO 100
            ENDIF
            izar = i1*1000 + i2
            IF (izar.EQ.0) THEN
               DO nnuc = 1, NDNUC
                  FISdis(nnuc) = val
               ENDDO
               IF(val.eq.0) then
                 WRITE (8,
     & '('' Fission transition states above saddles not considered'')') 
                 WRITE (12,
     & '('' Fission transition states above saddles not considered'')') 
               ENDIF
               IF(val.eq.1) then
                 WRITE (8 , 
     &    '('' Fission transition states by Maslov considered'')') 
                 WRITE (12, 
     &    '('' Fission transition states by Maslov considered'')') 
               ENDIF
               GOTO 100
            ENDIF
            CALL WHERE(izar,nnuc,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (8,'('' WARNING: NUCLEUS A,Z ='',I3,'','',I3,
     &                '' NOT NEEDED'')') i2,i1
               WRITE (8,'('' WARNING: FISDIS SETTING IGNORED'')')
               GOTO 100
            ENDIF

            FISdis(nnuc) = val

            IF(val.eq.0) then
               WRITE (8,
     &          '('' No fission transition states above saddles for '',
     &             I3,A2,'' (FISDIS=0)'')') i2, SYMb(nnuc)
               WRITE (12,
     &          '('' No fission transition states above saddles for '',
     &             I3,A2,'' (FISDIS=0)'')') i2, SYMb(nnuc)
            ENDIF
            IF(val.eq.1) then
               WRITE (8 ,
     &     '('' Fission transition states by Maslov considered for '',
     &             I3,A2,'' (FISDIS=1)'')') i2, SYMb(nnuc)
               WRITE (12,
     &     '('' Fission transition states by Maslov considered for '',
     &             I3,A2,'' (FISDIS=1)'')') i2, SYMb(nnuc)
            ENDIF
            GOTO 100
         ENDIF
C--------------------------------------------------------------------------
         IF (name.EQ.'FISSPE') THEN
            if(nint(val).ge.0 .and. nint(val).le.2) THEN
              if(nint(val).gt.0) then
                FISspe = nint(val)
                if(FISspe.eq.1) THEN 
                  WRITE (8,*) 
     &         ' WARNING: Los Alamos model will be implemented in 2012'
                  WRITE (8,*) 
     &         ' WARNING:      changing to Kornilov Model             '
                 FISspe = 2
                ENDIF                
                WRITE (8,*) 'Prompt fission neutron spectra calculated'
                if(FISspe.eq.2) WRITE (8,*) ' using Kornilov''s model'
                if(FISspe.eq.1) WRITE (8,*) ' using Los Alamos model'
              else
                WRITE (8,*)
     &           'Prompt fission neutron spectra not calculated'
              endif
            else
               WRITE (8,
     &         '('' WARNING: Fission spectrum key out of range (0-2) '',
     &         i2)') nint(val)
            endif
            GOTO 100
         ENDIF
C
C        PFNS scaling parameters below : 
C        PFNTKE, PFNrat PFNALP for both Kornilov & LA models 
C
C        PFNNIU is the nubar parameter
C
         IF (name.EQ.'PFNTKE') THEN
            if(i1.ne.0 .and. IOPran.ne.0) then
              WRITE (8,
     &        '('' Global Fission TKE normalization uncertainty '',
     &        '' is equal to '',i2,''%'')') i1
              sigma = val*i1*0.01
              IF(IOPran.gt.0) then
                atilss = val + grand()*sigma
              ELSE
                atilss = val + 1.732d0*(2*drand()-1.)*sigma
              ENDIF
              
              PFNtke = atilss

              WRITE (8,
     &        '('' Fission TKE norm sampled value : '',f8.3)')
     &       atilss
              IPArCOV = IPArCOV +1
              write(95,'(1x,i5,1x,d12.6,1x,2i13)')
     &          IPArCOV, atilss,INDexf,INDexb
 
            else

              PFNtke = val
              WRITE (8,'('' Fission TKE norm in all nuclei set to '',
     & F8.3)') val
            endif
            GOTO 100
	   ENDIF
C-----         
         IF (name.EQ.'PFNRAT') THEN
            if(i1.ne.0 .and. IOPran.ne.0) then
              WRITE (8,
     &    '('' Global PFN R=TLight/THeavy normalization uncertainty '',
     &        '' is equal to '',i2,''%'')') i1
              sigma = val*i1*0.01
              IF(IOPran.gt.0) then
                atilss = val + grand()*sigma
              ELSE
                atilss = val + 1.732d0*(2*drand()-1.)*sigma
              ENDIF
              
              PFNrat = atilss

              WRITE (8,
     &    '('' PFN R=TLight/THeavy norm sampled value : '',f8.3)')
     &       atilss
              IPArCOV = IPArCOV +1
              write(95,'(1x,i5,1x,d12.6,1x,2i13)')
     &          IPArCOV, atilss,INDexf,INDexb
 
            else

              PFNrat = val
              WRITE (8,
     &    '('' PFN R=TLight/THeavy norm in all nuclei set to '',
     & F8.3)') val
            endif
            GOTO 100
	   ENDIF
C-----         
         IF (name.EQ.'PFNALP') THEN
            if(i1.ne.0 .and. IOPran.ne.0) then
              WRITE (8,
     & '('' Global PFN alpha (Efrag ~ alpha*TKE) norm uncertainty '',
     &        '' is equal to '',i2,''%'')') i1
              sigma = val*i1*0.01
              IF(IOPran.gt.0) then
                atilss = val + grand()*sigma
              ELSE
                atilss = val + 1.732d0*(2*drand()-1.)*sigma
              ENDIF
              
              PFNalp = atilss

              WRITE (8,
     & '('' PFN alpha (Efrag ~ alpha*TKE) norm sampled value : '',
     & f8.3)') atilss
              IPArCOV = IPArCOV +1
              write(95,'(1x,i5,1x,d12.6,1x,2i13)')
     &          IPArCOV, atilss,INDexf,INDexb
 
            else

              PFNalp = val
              WRITE (8,
     & '('' PFN alpha (Efrag ~ alpha*TKE) norm in all nuclei set to '',
     & F8.3)') val
            endif
            GOTO 100
	   ENDIF
C-----         
         IF (name.EQ.'PFNNIU') THEN
            if(i1.ne.0 .and. IOPran.ne.0) then
              WRITE (8,
     &        '('' Global PFN NUBAR normalization uncertainty '',
     &        '' is equal to '',i2,''%'')') i1
              sigma = val*i1*0.01
              IF(IOPran.gt.0) then
                atilss = val + grand()*sigma
              ELSE
                atilss = val + 1.732d0*(2*drand()-1.)*sigma
              ENDIF
              
              PFNniu = atilss

              WRITE (8,
     &        '('' PFN NUBAR norm sampled value : '',f8.3)')
     &       atilss
              IPArCOV = IPArCOV +1
              write(95,'(1x,i5,1x,d12.6,1x,2i13)')
     &          IPArCOV, atilss,INDexf,INDexb
 
            else

              PFNniu = val
              WRITE (8,'('' PFN NUBAR norm in all nuclei set to '',
     & F8.3)') val

            endif
            GOTO 100
	   ENDIF
C-----         
         IF (name.EQ.'FISVF1') THEN
            char =' Fission barrier first hump height  '
            char1=' first hump  ' 
            CALL ADJUST(i1,i2,i3,iloc,izar,nnuc,quant,val,char, char1)
            IF (izar.EQ.0) THEN
             DO i = 1, NDNUC
               FISv_n(1,i) = val
             ENDDO
             WRITE (8,'(a35,''multiplied in all nuclei by '',1f6.2)' )
     &          char, val
             WRITE (12,'(a35,''multiplied in all nuclei by '',1f6.2)' )
     &          char, val
            ELSE
             FISv_n(1,nnuc) = quant
             WRITE (8 ,'(a35,''in '',I3,A2,'' set to '',F6.3)')
     &          char, i2, SYMb(nnuc), quant
             WRITE (12,'(a35,''in '',I3,A2,'' set to '',F6.3)')
     &          char, i2, SYMb(nnuc), quant
            ENDIF 
            GOTO 100
         ENDIF
C-----       
      IF (name.EQ.'FISVF2') THEN
         char =' Fission barrier second hump height '
         char1='second hump  '
         CALL ADJUST(i1,i2,i3,iloc,izar,nnuc,quant,val,
     &               char, char1)
         IF (izar.EQ.0) THEN
            DO i = 1, NDNUC
               FISv_n(2,i) = val
            ENDDO
            WRITE (8,'(a35,''multiplied in all nuclei by '',1f6.2)' )
     &          char,val
            WRITE (12,'(a35,''multiplied in all nuclei by '',1f6.2)' )
     &          char,val
            goto 100
         ENDIF
         FISv_n(2,nnuc)=quant
         goto 100
       ENDIF
C-----       
      IF (name.EQ.'FISVF3') THEN
         char =' Fission barrier third hump height  '
         char1=' third hump  '
         CALL ADJUST(i1,i2,i3,iloc,izar,nnuc,quant,val,
     &               char, char1)
         IF (izar.EQ.0) THEN
            DO i = 1, NDNUC
               FISv_n(3,i) = val
            ENDDO
            WRITE (8,'(a35,''multiplied in all nuclei by '',1f6.2)' )
     &          char,val
            WRITE (12,'(a35,''multiplied in all nuclei by '',1f6.2)' )
     &          char,val
            goto 100
         ENDIF
         FISv_n(3,nnuc)=quant
         goto 100
       ENDIF      
C-----       
      IF (name.EQ.'FISHO1') THEN
         char =' Fission barrier first hump width   '
         char1=' first hump  '
         CALL ADJUST(i1,i2,i3,iloc,izar,nnuc,quant,val,
     &               char, char1)
         IF (izar.EQ.0) THEN
            DO i = 1, NDNUC
               FISh_n(1,i) = val
            ENDDO
            WRITE (8,'(a35,''multiplied in all nuclei by '',1f6.2)' )
     &          char,val
            WRITE (12,'(a35,''multiplied in all nuclei by '',1f6.2)' )
     &          char,val
            goto 100
         ENDIF
         FISh_n(1,nnuc)=quant
         goto 100
       ENDIF
C-----       
      IF (name.EQ.'FISHO2') THEN
         char =' Fission barrier second hump width  '
         char1='second hump  '
         CALL ADJUST(i1,i2,i3,iloc,izar,nnuc,quant,val,
     &               char, char1)
         IF (izar.EQ.0) THEN
            DO i = 1, NDNUC
               FISh_n(2,i) = val
            ENDDO
            WRITE (8,'(a35,''multiplied in all nuclei by '',1f6.2)' )
     &          char,val
            WRITE (12,'(a35,''multiplied in all nuclei by '',1f6.2)' )
     &          char,val
            goto 100
         ENDIF
         FISh_n(2,nnuc)=quant
         goto 100
       ENDIF
C-----       
      IF (name.EQ.'FISHO3') THEN
         char =' Fission barrier third hump width   '
         char1=' third hump  '
         CALL ADJUST(i1,i2,i3,iloc,izar,nnuc,quant,val,
     &               char, char1)
         IF (izar.EQ.0) THEN
            DO i = 1, NDNUC
               FISh_n(3,i) = val
            ENDDO
            WRITE (8,'(a35,''multiplied in all nuclei by '',1f6.2)' )
     &          char,val
            WRITE (12,'(a35,''multiplied in all nuclei by '',1f6.2)' )
     &          char,val
            goto 100
         ENDIF
         FISh_n(3,nnuc)=quant
         goto 100
       ENDIF
C-----       
      IF (name.EQ.'FISAT1') THEN
         char =' L.d.a-parameter for first saddle   '
         char1='first-saddle '
         CALL ADJUST(i1,i2,i3,iloc,izar,nnuc,quant,val,
     &               char, char1)
         IF (izar.EQ.0) THEN
            DO i = 1, NDNUC
               FISa_n(1,i) = val
            ENDDO
            WRITE (8,'(a35,''multiplied in all nuclei by '',1f6.2)' )
     &          char,val
            WRITE (12,'(a35,''multiplied in all nuclei by '',1f6.2)' )
     &          char,val
            goto 100
         ENDIF
         FISa_n(1,nnuc)=quant
         goto 100
       ENDIF
C----             
      IF (name.EQ.'FISAT2') THEN
         char =' L.d. a-parameter for second saddle '
         char1='second saddle '
         CALL ADJUST(i1,i2,i3,iloc,izar,nnuc,quant,val,
     &               char, char1)
         IF (izar.EQ.0) THEN
            DO i = 1, NDNUC
               FISa_n(2,i) = val
            ENDDO
            WRITE (8,'(a35,''multiplied in all nuclei by '',1f6.2)' )
     &          char,val
            WRITE (12,'(a35,''multiplied in all nuclei by '',1f6.2)' )
     &          char,val
            goto 100
         ENDIF
         FISa_n(2,nnuc)=quant
         goto 100
       ENDIF
C-----       
      IF (name.EQ.'FISAT3') THEN
         char =' L.d. a-parameter for third saddle  '
         char1='third saddle '
         CALL ADJUST(i1,i2,i3,iloc,izar,nnuc,quant,val,
     &               char, char1)
         IF (izar.EQ.0) THEN
            DO i = 1, NDNUC
               FISa_n(3,i) = val
            ENDDO
            WRITE (8,'(a35,''multiplied in all nuclei by '',1f6.2)' )
     &          char,val
            WRITE (12,'(a35,''multiplied in all nuclei by '',1f6.2)' )
     &          char,val
            goto 100
         ENDIF
         FISa_n(3,nnuc)=quant
         goto 100
       ENDIF
C-----       
      IF (name.EQ.'FISDL1') THEN
         char =' L.d.DEL-parameter for first saddle '
         char1='first-saddle '
         CALL ADJUST(i1,i2,i3,iloc,izar,nnuc,quant,val,
     &               char, char1)
         IF (izar.EQ.0) THEN
            DO i = 1, NDNUC
               FISd_n(1,i) = val
            ENDDO
            WRITE (8,'(a35,''multiplied in all nuclei by '',1f6.2)' )
     &          char,val
            WRITE (12,'(a35,''multiplied in all nuclei by '',1f6.2)' )
     &          char,val
            goto 100
         ENDIF
         FISd_n(1,nnuc)=quant
         goto 100
       ENDIF
C----             
      IF (name.EQ.'FISDL2') THEN
         char =' L.d.DEL-parameter for second saddle '
         char1='second saddle'
         CALL ADJUST(i1,i2,i3,iloc,izar,nnuc,quant,val,
     &               char, char1)
         IF (izar.EQ.0) THEN
            DO i = 1, NDNUC
               FISd_n(2,i) = val
            ENDDO
            WRITE (8,'(a35,''multiplied in all nuclei by '',1f6.2)' )
     &          char,val
            WRITE (12,'(a35,''multiplied in all nuclei by '',1f6.2)' )
     &          char,val
            goto 100
         ENDIF
         FISd_n(2,nnuc)=quant
         goto 100
       ENDIF
C-----       
      IF (name.EQ.'FISDL3') THEN
         char =' L.d.DEL-parameter for third saddle '
         char1='third saddle '
         CALL ADJUST(i1,i2,i3,iloc,izar,nnuc,quant,val,
     &               char, char1)
         IF (izar.EQ.0) THEN
            DO i = 1, NDNUC
               FISd_n(3,i) = val
            ENDDO
            WRITE (8,'(a35,''multiplied in all nuclei by '',1f6.2)' )
     &          char,val
            WRITE (12,'(a35,''multiplied in all nuclei by '',1f6.2)' )
     &          char,val
            goto 100
         ENDIF
         FISd_n(3,nnuc)=quant
         goto 100
       ENDIF
C-----       
      IF (name.EQ.'FISVE1') THEN
         char =' Vib. enhancement for first saddle  '
         char1='first-saddle '
         CALL ADJUST(i1,i2,i3,iloc,izar,nnuc,quant,val,
     &               char, char1)
         IF (izar.EQ.0) THEN
            DO i = 1, NDNUC
               FISn_n(1,i) = val
            ENDDO
            WRITE (8,'(a35,''multiplied in all nuclei by '',1f6.2)' )
     &          char,val
            WRITE (12,'(a35,''multiplied in all nuclei by '',1f6.2)' )
     &          char,val
            goto 100
         ENDIF
         FISn_n(1,nnuc)=quant
         goto 100
       ENDIF
C----             
      IF (name.EQ.'FISVE2') THEN
         char =' Vib. enhancement for second saddle '
         char1='second saddle'
         CALL ADJUST(i1,i2,i3,iloc,izar,nnuc,quant,val,
     &               char, char1)
         IF (izar.EQ.0) THEN
            DO i = 1, NDNUC
               FISn_n(2,i) = val
            ENDDO
            WRITE (8,'(a35,''multiplied in all nuclei by '',1f6.2)' )
     &          char,val
            WRITE (12,'(a35,''multiplied in all nuclei by '',1f6.2)' )
     &          char,val
            goto 100
         ENDIF
         FISn_n(2,nnuc)=quant
         goto 100
       ENDIF
C-----       
      IF (name.EQ.'FISVE3') THEN
         char =' Vib. enhancement  for third saddle '
         char1='third saddle '
         CALL ADJUST(i1,i2,i3,iloc,izar,nnuc,quant,val,
     &               char, char1)
         IF (izar.EQ.0) THEN
            DO i = 1, NDNUC
               FISn_n(3,i) = val
            ENDDO
            WRITE (8,'(a35,''multiplied in all nuclei by '',1f6.2)' )
     &          char,val
            WRITE (12,'(a35,''multiplied in all nuclei by '',1f6.2)' )
     &          char,val
            goto 100
         ENDIF
         FISn_n(3,nnuc)=quant
         goto 100
       ENDIF
C-----
         IF (name.EQ.'RANDOM') THEN
            if(nint(val).eq.0) goto 100             
            if(nint(val).gt.0) then
              IOPran = 1
              WRITE (8,*)
     &          'Gaussian pdf assumed to calculate uncertainty'
            else if(nint(val).lt.0) then
              IOPran = -1
              WRITE (8,*)
     &          'Uniform pdf assumed to calculate uncertainty'
            endif
            INQUIRE(file='R250SEED.DAT',exist=fexist)
            if(.not.fexist) then
C             If the file R250SEED.DAT does not exist,
C             then starting seed is read
              iseed = abs(nint(val))
              if(iseed.le.1) iseed=1234567
              WRITE (8,*) 'Random seeds :', 1, 104
              Call R250Init(iseed)
            else
              OPEN(94,file='R250SEED.DAT',status='OLD')
              READ(94,*)  indexf, indexb
              CLOSE(94)
              WRITE (8,*) 'Random seeds :', indexf, indexb
            endif
C--------------------------------------------------------------------------
            GOTO 100
         ENDIF

         IF (name.EQ.'RECOIL') THEN
            RECoil = val
C           IF (RECOIL.LT.0.d0) RECoil = 1.d0
            IF (RECOIL.NE.0.d0) RECoil = 1.d0
            IF (RECOIL.eq.0.d0)
     &      WRITE (8,'('' Recoils are not calculated'')')
            GOTO 100
         ENDIF
C--------------------------------------------------------------------------
         IF (name(1:3).EQ.'FIT') GOTO 100
C-----
C-----
         WRITE (8,'('' INVALID KEY: '',A6,'', DISPOSITION IGNORED'')')
     &          name
      GOTO 100
  200 WRITE (8,
     &'('' ERROR: INVALID FORMAT in KEY: '',A6,
     &  '', EMPIRE STOPPED'')') name
      STOP ' FATAL: INVALID FORMAT in input KEY '
      END
C
      SUBROUTINE ADJUST(i1,i2,i3,iloc,izar,nnuc,quant,val,char,
     &                  char1)

      INCLUDE 'dimension.h'
      INCLUDE 'global.h'

      REAL*8 quant,val,sigma
      REAL*8 GRAND,DRAND
      INTEGER i1,i2,i3,iloc,nnuc
      CHARACTER*35 char
      CHARACTER*13 char1

      izar = i1*1000 + i2
      IF (izar.EQ.0) RETURN

      CALL WHERE(izar,nnuc,iloc)
      IF (iloc.EQ.1) THEN
         WRITE (8,'('' NUCLEUS A,Z ='',I3,'','',I3,
     &                '' NOT NEEDED'')') i2,i1
         WRITE (8,
     &         '('' NORMALIZATION OF'',a12,''IGNORED'')')char1
         return
      ENDIF

      IF(i3.ne.0 .and. IOPran.ne.0) THEN
         WRITE (8,'(a35,'' uncertainty in '',I3,
     &         A2,'' is equal to '',i2,''%'')') char,i2, SYMb(nnuc), i3
         sigma = val*i3*0.01
         IF(IOPran.gt.0) then
            quant = val + grand()*sigma
         ELSE
            quant = val + 1.732d0*(2*drand()-1.)*sigma
         ENDIF
         WRITE (8,'(a35,''factor sampled value : '',f8.3)')char, quant
      ELSE
         quant = val
         WRITE (8, '(a35,'' in '',I3,A2,
     &        '' multiplied by '',F6.2)'  )char, i2, SYMb(nnuc),val
      ENDIF

      RETURN
      END
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
Ccc   *     from the RIPL mass file mass-frdm95.dat                    *
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
Ccc   * RIPL database used                                             *
Ccc   ********************************************************************
Ccc
      OPEN (UNIT = 27,STATUS = 'OLD',
     &      FILE = trim(empiredir)//'/RIPL/masses/mass-frdm95.dat'
     &      ,ERR = 300)
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
C-----mass10: subroutine for formula of Duflo-Zuker for masses outside M-N
C
      DO iz = 6, 100
         DO ia = 2*iz - 10, 3*iz
            IF (RESmas(iz,ia).EQ.0.D0) THEN
               in = ia - iz
               if (in.le.0) cycle
               CALL mass10(in,iz,ebin)
               RESmas(iz,ia) = iz*AMUpro + in*AMUneu - ebin/AMUmev
               EXCessmass(iz,ia) = RESmas(iz,ia)*AMUmev - REAL(ia)
            ENDIF
         ENDDO
      ENDDO
C-----mbc1 a quick/temp? solution to weird light undefined masses: define
C-----resmas=A for all nuclei so far undefined
C-----previously i had a problem for be6 => be5 +n since mass be5 undefined
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
               IF(DEF(1,0).EQ.0.d0) DEF(1,0) = beta2x(k)
               XMAss(0) = EXCessmass(iz,ia)
            ENDIF
         ELSE
            DO ii = 0, NDEJC
             IF (nixz.EQ.ZEJc(ii) .AND. nixa.EQ.AEJc(ii)) THEN
              DEFprj = beta2x(k)
              XMAss_ej(ii) = EXCessmass(iz,ia)
C
C             Rounded ENDF values of nuclear masses
C
C              mn    neutron mass  1.008 665 amu 
C              mp    proton mass   1.007 276 amu 
C              ma    4He mass      4.001 506 amu 
C              md    deuteron mass 2.013 553 amu 
C              mt    triton mass   3.015 501 amu 
C              m3He  3He mass      3.014 932 amu 

C              me    electron mass 5.485 799�10-4 amu 
C
               IF (nixa.EQ.1 .AND. nixz.EQ.0)
     >           XMAss_ej(ii)=(AMUneu-1.d0)*AMUmev
               IF (nixa.EQ.1 .AND. nixz.EQ.1)
     >           XMAss_ej(ii)=(AMUpro-1.d0)*AMUmev
               IF (nixa.EQ.4 .AND. nixz.EQ.2)
     &           XMAss_ej(ii)=(4.001506d0-4.d0)*AMUmev  ! he4
               IF (nixa.EQ.2 .AND. nixz.EQ.1)
     &           XMAss_ej(ii)=(2.013553d0-2.d0)*AMUmev  ! deut
               IF (nixa.EQ.3 .AND. nixz.EQ.1)
     &           XMAss_ej(ii)=(3.015501d0-3.d0)*AMUmev  ! triton 
               IF (nixa.EQ.3 .AND. nixz.EQ.2)
     &           XMAss_ej(ii)=(3.014932d0-3.d0)*AMUmev  ! he3
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
  300 WRITE (8,*) 'ERROR: File '//
     &  trim(empiredir)//'/RIPL/masses/mass-frdm95.dat missing'  
      STOP 'FATAL: File ../RIPL/masses/mass-frdm95.dat missing'
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
Ccc   *     Reads level density parameters from file 24                  *
Ccc   *     AROGC - a-parameter without collective effects (G.C.)        *
Ccc   *     AROC  - a-parameter including collective effects (EGSM)      *
Ccc   *     QN    - neutron binding energy (as in Iljinov & Mebel)       *
Ccc   *     DOBS  - D-observed for neutron resoances ( -- " --)          *
Ccc   *                                                                  *
Ccc   * input:none                                                       *
Ccc   *                                                                  *
Ccc   * output:none                                                      *
Ccc   *                                                                  *
Ccc   * calls:where                                                      *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * author: M.Herman                                                 *
Ccc   * date:   12.Jul.1997                                              *
Ccc   * revision:2    by:R. Capote                on:11.04.2011          *
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C Local variables
C
      DOUBLE PRECISION a23, acrt, ap1, ap2, ar, aroc, arogc, asys, atil,
     &                 del, delp, dob, econd, gamma,
     &                 pi2, qn, tcrt, uexc, xr, ddob, esh, dap, dam
      DOUBLE PRECISION om2_gsm,delp_gsm,asys_gsm,asyserr_gsm,dshift_gsm

      DOUBLE PRECISION FSHELL
      INTEGER iloc, ix, izamn, izamx, izar, nnuc, iz, nlevc
      INTEGER INT
      pi2 = PI**2
      izamx = 0
      izamn = 200000
      DO nnuc = 0, NNUct
         izamx = MAX(IZA(nnuc),izamx)
         izamn = MIN(IZA(nnuc),izamn)
      ENDDO
C-----Set EGSM normalization factors for each Z
      DO iz = 1,NDZmax
         ATIlnoz(iz) = 1.0 !default
      ENDDO
      IF (ADIv.EQ.0.0D0) THEN
         OPEN(31, FILE= trim(empiredir)//
     &   '/RIPL/densities/total/level-densities-egsm-norm.dat',
     &   STATUS='OLD')
         READ (31,'(///)')
   90    READ (31,'(I5,F8.3)',END = 95) iz,atiln
         ATIlnoz(iz) = atiln
         GOTO 90
   95    CONTINUE
         CLOSE(31)
      ENDIF

      IF (ADIv.EQ.3.0D0 .and. FITLEV.LE.0.1) THEN
         WRITE(8,'(1X)')
         WRITE(8,'(1X)')
         WRITE(8,'(4X,''L e v e l  d e n s i t y  p a r a m e t e r s  n
     & o r m a l i z a t i o n'')')
         WRITE(8,'(4X,54(''-''))')
      ENDIF
      IF (ADIv.NE.3.0D0 .and. FITLEV.LE.0.1) THEN
         WRITE(8,'(1X)')
         WRITE(8,'(1X)')
         WRITE(8,'(4X,''L e v e l  d e n s i t y  p a r a m e t e r s  a
     &(Qn)'')')
         WRITE(8,'(4X,54(''-''))')
         WRITE(8,'(1X)')
         WRITE(8,'(3X,''Nucleus    a_exp     a_sys.   int. nor.  '',
     &               ''ext. nor. a_final'')')
         WRITE(8,'(1X)')
      ENDIF
      IF (FITLEV.GT.0) THEN
         WRITE(8,'(1X)')
         WRITE(8,'(1X)')
         WRITE(8,'(4X,''L e v e l  d e n s i t y  p a r a m e t e r s  f
     & i t'')')
         WRITE(8,'(4X,54(''-''))')
      ENDIF
C
C     reading from the RIPL level-densities-par.dat file 
C       EGSM[RIPL-3], G&C[old EMPIRE] and GSM[RIPL]
C
C     Skipping header
C
      READ (24,'(///)')
C               (2I4,1x,a2,f4.1,1x,F7.3,3E14.5,5f8.4,I4,1x,5(f8.3))
  100 READ (24,'(2I4,8x,F7.3,3E14.5,5f8.4,I4,1x,5(f8.3))',END = 200)
     &  nixz, nixa, qn, dob, ddob, esh, dap,aroc,dam,ftmp,arogc,nlevc,
     &  om2_gsm,delp_gsm,asys_gsm,asyserr_gsm,dshift_gsm

      izar = nixz*1000 + nixa
      IF (izar.GE.izamn .AND. izar.LE.izamx) THEN
         CALL WHERE(izar,nnuc,iloc)
         IF (iloc.EQ.0) THEN
C
C           Taking the default number of discrete levels from the EMPIRE file  
C                              ../data/level-density-par.dat

C           NLV(nnuc)   = MIN(NDLV,nlevc)
C           NCOmp(nnuc) = MIN(NDLV,nlevc)
C
            DOBs(nnuc) = dob
            IF(D0_obs.GT.0.) DOBs(nnuc) = D0_obs
            a23 = A(nnuc)**0.666667

C-----------Set up normalization factors for level density parameter 'a'
C-----------for all level density models except HFB
C

C-----------Gilbert-Cameron (no explicit collective effects)
            IF (ADIv.EQ.2.D0) THEN
              ! for the time being, G&C not refitted !
              del = 0.0
              delp = 12.0/SQRT(A(nnuc))
              IF (MOD(XN(nnuc),2.D0).EQ.0.D0) del = delp
              IF (MOD(Z(nnuc),2.D0).EQ.0.D0) del = del + delp
              uexc = qn - del                  
C-------------Mebel's  parametrization (taken from the INC code for the case
C-------------of no collective enhancements) normalized to existing exp. data
              IF (ROPaa(Nnuc).EQ.( - 2.0D0)) THEN
                atil = 0.114*A(Nnuc) + 9.80E-2*A(Nnuc)**0.666667
                gamma = -0.051d0
                asys = atil*FSHELL(uexc,SHC(Nnuc),-gamma)
                atiln =  arogc/asys
              ELSE
                atiln = 1.0   
              ENDIF                  
            ENDIF 
                 
C-----------EMPIRE specific (EGSM) with RIPL shell corrections
            IF (ADIv.EQ.0 .OR. ADIv.EQ.1) THEN
              CALL EGSMsys(ap1,ap2,gamma,del,delp,nnuc)
              atil = ap1*A(nnuc) + ap2*a23
              tcrt = 0.567*delp
              ar = atil*(1.0 + SHC(nnuc)*gamma)
              DO ix = 1, 20
                 xr = ar*tcrt**2
                 acrt = atil*FSHELL(xr,SHC(nnuc),gamma)
                 IF (ABS(acrt - ar).LE.0.001D0*acrt) EXIT
                 ar = acrt
              ENDDO
              econd = 1.5*acrt*delp**2/pi2
              uexc = qn + del - econd
              asys = atil*FSHELL(uexc,SHC(nnuc),gamma)
              atiln =  aroc/asys
            ENDIF

            IF(ATIlnor(nnuc).EQ.0) THEN
              ATIlnor(nnuc) = atiln
            ELSE
              ATIlnor(nnuc) = ATIlnor(nnuc)*atiln
            ENDIF
C           Initialization of ROPar(1,Nnuc) and ROPar(3,Nnuc) (for GC and PCROSS)
            ROPar(1,nnuc) = asys*ATIlnor(nnuc)
            ROPar(3,nnuc) = del

C-----------Print resulting level density parameters
            IF (FITlev.GT.0.0D0) THEN
               WRITE (8,*) ' '
               WRITE(8,'(1X)')
               WRITE(8,
     &           '(3X,''Nucleus    a_exp     a_sys.   int. nor.  '',
     &           ''ext. nor. a_final'')')
               IF (ADIv.EQ.0.0D0)
     &         WRITE(8,'(I3,''-'',A2,''-'',I3, 5(2x,F8.5))')
     &         INT(Z(nnuc)), SYMb(nnuc), INT(A(nnuc)),
     &         aroc, asys, atiln, ATIlnor(nnuc)/atiln, ROPar(1,nnuc)
               IF (ADIv.EQ.2.0D0)
     &         WRITE(8,'(I3,''-'',A2,''-'',I3, 5(2x,F8.5))')
     &         INT(Z(nnuc)), SYMb(nnuc), INT(A(nnuc)),
     &         arogc, asys, atiln, ATIlnor(nnuc)/atiln, ROPar(1,nnuc)
               WRITE(8,*)
               IF (ADIv.EQ.0.0D0 .OR. ADIv.EQ.2.0D0) then 
                  WRITE (8,*)
     &              ' SHC=', sngl(SHC(nnuc)), ' U=', sngl(uexc)
                  WRITE (8,*) 
     &              ' DELTA=', sngl(del),' Dobs=',sngl(dob)
               ENDIF
               IF (ADIv.EQ.2.0D0) then
                  WRITE (8,*)
     &              ' SHC=', sngl(SHC(nnuc)), ' U=', sngl(uexc)
                  WRITE (8,*) 
     &              ' DELTA=', sngl(del),' Dobs=',sngl(dob)
               ENDIF
               WRITE(8,*) '========================'
            ELSE
               IF (ADIv.EQ.0.0D0)
     &         WRITE(8,'(I3,''-'',A2,''-'',I3, 5(2x,F8.5))')
     &         INT(Z(nnuc)), SYMb(nnuc), INT(A(nnuc)),
     &         aroc, asys, atiln, ATIlnor(nnuc)/atiln, ROPar(1,nnuc)
               IF (ADIv.EQ.2.0D0)
     &         WRITE(8,'(I3,''-'',A2,''-'',I3, 5(2x,F8.5))')
     &         INT(Z(nnuc)), SYMb(nnuc), INT(A(nnuc)),
     &         arogc, asys, atiln, ATIlnor(nnuc)/atiln, ROPar(1,nnuc)
            ENDIF
         ENDIF
      ENDIF
      GOTO 100
  200 WRITE(8,'(1X)')
      WRITE(8,'(3X,''Nucleus                   final  norm.'')')
      WRITE(8,'(1X)')
      DO nnuc = 1, NNUct
         IF(ATIlnor(nnuc).LE.0.) then
           WRITE(8,'(I3,''-'',A2,''-'',I3, 20X,2x,1F8.5)')
     &     INT(Z(nnuc)), SYMb(nnuc), INT(A(nnuc)),
     &     ATIlnoz(INT(Z(nnuc)))
           ATIlnor(nnuc) = 1.d0
         ELSE
           WRITE(8,'(I3,''-'',A2,''-'',I3, 20X,2x,1F8.5)')
     &     INT(Z(nnuc)), SYMb(nnuc), INT(A(nnuc)),
     &     ATIlnor(nnuc)
         ENDIF

      ENDDO
      RETURN
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
         PRINT 99995, j
99995    FORMAT (' FAILURE IN LYMASS - Y(',I1,
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
Ccc   *           Calculates binding energies                          *
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
      INTEGER ar, zr, ap, zp, ac, zc
      DOUBLE PRECISION b1, b2, b3
      zc = Z(Nnuc)
      ac = A(Nnuc)
      zp = ZEJc(Nejc)
      ap = AEJc(Nejc)
      zr = Z(Nnuc) - ZEJc(Nejc)
      ar = A(Nnuc) - AEJc(Nejc)
      b1 = A(Nnuc)*AMUmev + EXCessmass(zc,ac)
      b2 = ar*AMUmev + EXCessmass(zr,ar)
C
C     The calculated ejectile mass below correspond to the use
C     of atomic masses 
C     Obtained Q-values consistent with all available calculators (NUDAT, LUND, etc)
      b3 = AEJc(Nejc)*AMUmev + EXCessmass(zp,ap)
C
C     The calculated ejectile mass below correspond to the use
C     of nuclear masses dtripped of electrons (as recom in ENDF manual) 
C     b3 = AEJc(nejc)*AMUmev + XMAss_ej(nejc)

      Bnd = b2 + b3 - b1
      END
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
Ccc   * output: none                                                   *
Ccc   *                                                                *
Ccc   * calls: pipe                                                    *
Ccc   *                                                                *
Ccc   *                                                                *
Ccc   * author: M.Herman                                               *
Ccc   * date:   09.Apr.2011                                            *
Ccc   * revision:     by:                         on:                  *
Ccc   *                                                                *
Ccc   ******************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C Local variables
C
      CHARACTER*13 caz
      CHARACTER*64 filename
      INTEGER*4 pipe
      INTEGER*4 iwin
      CHARACTER*132 ctmp
      LOGICAL fexist

C     write(6,*)trim(empiredir)//trim(filename)
      INQUIRE (FILE = 'C4.DAT', EXIST = fexist)
      IF (fexist) RETURN  ! SKIPPING EXP. DATA RETRIEVAL AND SORTING IF C4.DAT EXISTS
C
C-----define target file name
C
      if(SYMb(0)(2:2).eq.' ') then
        write(caz,'(I3.3,3A1,I3.3,A3)')
     &   int(Z(0)),'_',SYMb(0)(1:1),'_', int(A(0)),'.c4'
      else
        write(caz,'(I3.3,A1,A2,A1,I3.3,A3)')
     &   int(Z(0)),'_',SYMb(0)(1:2),'_', int(A(0)),'.c4'
      endif
      
C-----concatenate file name with the projectile path
      IF(IZAejc(0) .EQ. 1) THEN
        filename = '/EXFOR/neutrons/'//trim(caz)
      ELSEIF(IZAejc(0) .EQ. 1001) THEN
        filename = '/EXFOR/protons/'//trim(caz)
      ELSEIF(IZAejc(0) .EQ. 0) THEN
        filename = '/EXFOR/gammas/'//trim(caz)
      ELSE
        WRITE (8,
     & '('' WARNING: No EXFOR retrievals for complex projectiles'')')
        WRITE (8,*)     
        RETURN
      ENDIF
C     write(6,*)trim(empiredir)//trim(filename)
      INQUIRE (FILE = trim(empiredir)//trim(filename), EXIST = fexist)
      IF (.NOT. fexist) THEN
        WRITE (*,
     & '(''  WARNING: No experimental data in IAEA EXFOR-C4 file:'')')
        write (*,*)' ',trim(empiredir)//trim(filename)
        WRITE (*,*)
        WRITE (8,
     & '('' WARNING: No experimental data in IAEA EXFOR-C4 file:'')')
        write (8,*)' ',trim(empiredir)//trim(filename)
        WRITE (8,*)
        RETURN 
      ENDIF

C-----Create full command string
      IF(IOPsys .EQ. 0) then  !Linux, Mac
        ctmp = 'cp '//trim(empiredir)//trim(filename)//' TMP.c4'
      ELSE                    !Windows
         ctmp = 'copy '//trim(empiredir)//trim(filename)//' TMP.c4'
      ENDIF 
C-----copy EXFOR file to the working directory
      write (8,*) ctmp
      iwin = pipe(ctmp) 

      write(*,*) ' '
      WRITE (8,*) 
     &  '  Retrieving and sorting EXFOR(C4) file: ',trim(filename)
      WRITE (*,*) 
     &  '  Retrieving and sorting EXFOR(C4) file: ',trim(filename)
      write(*,*) ' '

      IF(IOPsys .EQ. 0) then  !Linux, Mac
        ctmp = trim(empiredir)//'/scripts/sortc4 TMP'
      ELSE                    !Windows
        ctmp = trim(empiredir)//'/scripts/sortc4.bat TMP'
      ENDIF 
      iwin = pipe(ctmp) 
      
      IF(IOPsys .EQ. 0) then  !Linux, Mac
        ctmp = 'mv TMP.c4 C4.DAT'
      ELSE                    !Windows
        ctmp = 'move TMP.c4 C4.DAT'
      ENDIF 
      iwin = pipe(ctmp) 

      RETURN
      END
C
C
C
      INTEGER FUNCTION IFindColl_CCFUS()
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:ipu*
Ccc   *                    F I N D C O L L _ C C F U S                   *
Ccc   *                                                                  *
Ccc   *  Reads from the file 13 ground state spin and parity and first   *
Ccc   *  collective energies (the latter to be used by ECIS).            *
Ccc   *  Reads from the file 25 g.s. deformation to define               *
Ccc   *    nucleus structure, which is important for CCFUS code          *
Ccc   *                                                                  *
Ccc   *  Uses unit 32 temporarily                                        *
Ccc   *  Some output to UNIT=6 is done                                   *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * input:IA   - A of the nucleus (through COMMON)                   *
Ccc   *       IZ   - Z of the nucleus (through COMMON)                   *
Ccc   *                                                                  *
Ccc   * output: (through COMMON)                                         *
Ccc   * return:                                                          *
Ccc   *   IFindColl = 0 (ALL POSSIBLE LEVELS FOUND)                      *
Ccc   *   IFindColl = 1 WARNING: (SOME COLLECTIVE LEVELS NOT FOUND)      *
Ccc   *   IFindColl = 2 ERROR: NO DISCRETE LEVEL INFORMATION AVAILABLE   *
Ccc   *                                                                  *
Ccc   * calls:none                                                       *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C Local variables
C
      DOUBLE PRECISION beta2, beta3, betatmp, etmp, jtmp
      INTEGER i, ia, ierr, iptmp, iz, natmp, nztmp, iccfus
      CHARACTER*6 reftmp

      ierr = 0
      iccfus = 0
      ia = A(0)
      iz = Z(0)

      beta2 = 0.D0
      beta3 = 0.D0
      OPEN (84,FILE = trim(empiredir)//
     &      '/RIPL/optical/om-data/om-deformations.dat',
     &      STATUS = 'old',ERR = 200)
      READ (84,'(///)')    ! Skipping first 4 title lines
      DO i = 1, 1700
         READ (84,'(2I4,4x,f10.6,1x,f4.1,i3,3x,f10.6,2x,a6)',END = 250,
     &         ERR = 250) nztmp, natmp, etmp, jtmp, iptmp, betatmp,
     &                    reftmp
         IF (nztmp.EQ.iz .AND. natmp.EQ.ia .AND. jtmp.EQ.2.D0 .AND.
     &       iptmp.EQ. + 1 .AND. reftmp.EQ.'Raman2') THEN
             iccfus = iccfus + 1
             beta2 = betatmp
c            CCFUS deformations
             BETcc(iccfus) = beta2
             FLAm(iccfus) = 2
             QCC(iccfus) = -etmp
             WRITE (8,'(/1x,A41/1x,A11,F7.3)')
     &           'TARGET EXPERIMENTAL DEFORMATION (RIPL):', 
     &           'BETA (2+) =',beta2
         ENDIF
         IF (nztmp.EQ.iz .AND. natmp.EQ.ia .AND. jtmp.EQ.3.D0 .AND.
     &       iptmp.EQ. - 1 .AND. reftmp.EQ.'Kibedi') THEN
             iccfus = iccfus + 1
             beta3 = betatmp
c            CCFUS deformations
             BETcc(iccfus) = beta3
             FLAm(iccfus) = 3
             QCC(iccfus) = -etmp
             WRITE (8,'(/1x,A41/1x,A11,F7.3)')
     &           'TARGET EXPERIMENTAL DEFORMATION (RIPL):', 
     &           'BETA (3-) =',beta3
         ENDIF
      ENDDO
 250  IF (beta2.EQ.0.D0) THEN
         ierr = 1
         WRITE (8,*) ' WARNING: ',
     &    'E(2+) level not found in Raman 2001 database (RIPL)'
         WRITE (8,*) ' WARNING: ',
     &       'Default dynamical deformations 0.15 (2+) used'
      ENDIF
      IF (beta3.EQ.0.D0) THEN
         ierr = 1
         WRITE (8,*) ' WARNING: ',
     &        'E(3-) level not found in Kibedi database (RIPL)'
         WRITE (8,*) ' WARNING: ',
     &       'Default dynamical deformations 0.05 (3-) used'
      ENDIF
      IF(AEJc(0).LE.4) GOTO 350
      ia = AEJc(0)
      iz = ZEJc(0)
      close(84)
      beta2 = 0.D0
      beta3 = 0.D0
      OPEN (84,FILE = trim(empiredir)//
     &      '/RIPL/optical/om-data/om-deformations.dat',
     &      STATUS = 'old',ERR = 200)
      READ (84,'(///)')    ! Skipping first 4 title lines
      DO i = 1, 1700
         READ (84,'(2I4,4x,f10.6,1x,f4.1,i3,3x,f10.6,2x,a6)',END = 300,
     &         ERR = 300) nztmp, natmp, etmp, jtmp, iptmp, betatmp,
     &                    reftmp
         IF (nztmp.EQ.iz .AND. natmp.EQ.ia .AND. jtmp.EQ.2.D0 .AND.
     &       iptmp.EQ. + 1 .AND. reftmp.EQ.'Raman2') THEN
             iccfus = iccfus + 1
             beta2 = betatmp
c            CCFUS deformations
             BETcc(iccfus) = beta2
             FLAm(iccfus) = -2
             QCC(iccfus) = -etmp
             WRITE (8,'(/1x,A39/1x,A11,F7.3)')
     &           'PROJ EXPERIMENTAL DEFORMATION (RIPL):', 
     &           'BETA (2+) =',beta2
         ENDIF
         IF (nztmp.EQ.iz .AND. natmp.EQ.ia .AND. jtmp.EQ.3.D0 .AND.
     &       iptmp.EQ. - 1 .AND. reftmp.EQ.'Kibedi') THEN
             iccfus = iccfus + 1
             beta3 = betatmp
c            CCFUS deformations
             BETcc(iccfus) = beta3
             FLAm(iccfus) = -3
             QCC(iccfus) = -etmp
             WRITE (8,'(/1x,A39/1x,A11,F7.3)')
     &           'PROJ EXPERIMENTAL DEFORMATION (RIPL):', 
     &           'BETA (3-) =',beta3
         ENDIF
      ENDDO
 300  IF (beta2.EQ.0.D0) THEN
         ierr = 1
         WRITE (8,*) ' WARNING: ',
     &    'E(2+) level not found in Raman 2001 database (RIPL)'
         WRITE (8,*) ' WARNING: ',
     &       'Default dynamical deformations 0.15 (2+) used'
      ENDIF
      IF (beta3.EQ.0.D0) THEN
         ierr = 1
         WRITE (8,*) ' WARNING: ',
     &        'E(3-) level not found in Kibedi database (RIPL)'
         WRITE (8,*) ' WARNING: ',
     &       'Default dynamical deformations 0.05 (3-) used'
      ENDIF
      GOTO 350
  200 WRITE (8,*) ' WARNING: ',trim(empiredir)//
     &   '/RIPL/optical/om-data/om-deformations.dat not found '
      WRITE (8,*) ' WARNING: ',
     &       'Default dynamical deformations 0.15(2+) and 0.05(3-) used'
      ierr = 2
      GOTO 400

  350 CLOSE (84)
      NScc = max(iccfus,NScc,0)

  400 IFINDCOLL_CCFUS = ierr

      RETURN
      END
      
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
     &                 etmp, ftmp, gspar, gspin, jtmp, qn, t12, xjlvr,
     &                 egrcoll(0:3,3),ggrcoll(0:3,3),
     &                 betahegor, betalegor, betagmr, betagqr,
     &                 sgmr, sgqr, sgor
      CHARACTER*1 dum
      CHARACTER*5 chelem
      CHARACTER*80 comment
      CHARACTER*80 ch_iuf
      CHARACTER*3 ctmp3
      CHARACTER*5 ctmp5
      DOUBLE PRECISION DBLE
      LOGICAL fexist,odd
      CHARACTER*9 finp
      INTEGER i, i0p, i10p, i12p, i1m, i20p, i21p, i22p, i31p, i3m,
     &        i41p, i4p, i5m, i6p, i8p, ia, iar, ierr, ilv, iptmp,
     &        itmp, itmp1, itmp2, iz, izr, j, lvpr, natmp, nbr, ndbrlin,
     &        ngamr, nlvr, nlvs, nmax, nnurec, nztmp, ncont
      INTEGER NINT
      CHARACTER*6 reftmp

      ND_nlv = 0
C-----Target corresponds to nnurec = 0
C     CALL WHERE(IZA(1) - IZAejc(0),nnurec,iloc)
      nnurec = 0

C
C     Giant multipole resonances following TALYS
C
c     For each L multipolarity Energy Weighted Sum Rule (EWSR) applies:
c     SUM_i(E_i*beta_i)=57.5*A**(-5/3)*L*(L+1)
C
      sgmr=23.*A(0)**(-5./3.)
      egrcoll(0,1)=18.7-0.025*A(0)
      ggrcoll(0,1)=3.
      sgqr=575.*A(0)**(-5./3.)
      egrcoll(2,1)=65.*A(0)**(-1./3.)
      ggrcoll(2,1)=85.*A(0)**(-2./3.)
      sgor=1208.*A(0)**(-5./3.)
      sleor=0.3*sgor
      egrcoll(3,1)=31.*A(0)**(-1./3.)
      ggrcoll(3,1)=5.
      sheor=0.7*sgor
      egrcoll(3,2)=115.*A(0)**(-1./3.)
      ggrcoll(3,2)=9.3-A(0)/48.

      INQUIRE (FILE = 'TARGET_COLL.DAT',EXIST = fexist)
      IF (fexist) THEN
         WRITE (8,*) ' '
         WRITE (8,*) 'File with collective levels exists for the target'
         WRITE (8,*) '-------------------------------------------------'
         WRITE (8,*) ' '
         OPEN (UNIT = 32,FILE = 'TARGET_COLL.DAT',
     &     STATUS = 'OLD',ERR=5432)
C--------Collective levels automatically selected, pls check
         READ (32,'(a80)',END=5432) comment
         WRITE (8,'(a80)') comment
         WRITE (12,*) ' Collective levels used in direct calculations'
C--------2nd line
         READ (32,'(a80)') comment
         WRITE (8,'(a80)') comment
C--------82 208    nucleus is treated as spherical
         READ (32,'(a80)') comment
         WRITE (8,'(a80)') comment
         WRITE (12,'(a80)') comment
C        '  40  90    nucleus is treated as dynamically deformed                                              '
         idefault = SCAN(comment(34:40),'sphe')                  
         if(idefault.eq.0) then
C
C          Soft rotator optical model
C
           do i=1,5 
             READ (32,'(a80)') comment
             WRITE (8,'(a80)') comment
             WRITE (12,'(a80)') comment
           enddo
C----------empty line
           READ (32,'(a80)') comment
           WRITE (8,'(a80)') comment
           WRITE (12,'(a80)') comment
C----------Ncoll 
           READ (32,'(a80)') comment
           WRITE (8,'(a80)') comment
           WRITE (12,'(a80)') comment
           DEFormed = .FALSE.
           SOFt = .TRUE.
           READ (32,'(3x,3I5)') ND_nlv
           WRITE (8,'(3x,3I5)') ND_nlv
           WRITE (12,'(3x,3I5)') ND_nlv
           IF (ND_nlv.EQ.0) THEN
            WRITE (8,*) ' WARNING: ND_NLV=0 in -col.lev file'
            WRITE (8,*) ' WARNING: No collective levels considered'
            WRITE (8,*) ' WARNING: DIRECT has been set to 0'
C-----------setting DIRect to zero
            DIRect = 0
            IFINDCOLL = 2
            RETURN
           ENDIF
C----------empty line
           READ (32,'(a80)') comment
           WRITE (8,'(a80)') comment
C----------'collective levels:'
           READ (32,'(a80)') comment
           WRITE (8,'(a80)') comment
           WRITE (12,*)' '
           WRITE (12,'(a80)') comment
C----------Reading ground state information (to avoid overwriting deformation)
           READ(32,
     &        '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3,1x,I2)')
     &           ICOllev(1), D_Elv(1), D_Xjlv(1), D_Lvp(1), IPH(1),
     &           D_Llv(1), D_Klv(1), ftmp, D_nno(1)
           WRITE(8,
     &        '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3,1x,I2)')
     &           ICOllev(1), D_Elv(1), D_Xjlv(1), D_Lvp(1), IPH(1),
     &           D_Llv(1), D_Klv(1), ftmp, D_nno(1)
           WRITE(12,
     &        '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3,1x,I2)')
     &           ICOllev(1), D_Elv(1), D_Xjlv(1), D_Lvp(1), IPH(1),
     &           D_Llv(1), D_Klv(1), ftmp, D_nno(1)
C          mintsp = mod(NINT(2*D_Xjlv(1)),2)
           igreson = 0
           DO i = 2, ND_nlv
            READ (32,
     &     '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3,1x,i2,A5)')
     &          ICOllev(i), D_Elv(i), D_Xjlv(i), D_Lvp(i), IPH(i),
     &          D_Llv(i), D_Klv(i), ftmp, D_nno(i), ctmp5  
C           For odd nuclides, collective states in continuum have
C           different spin than the ground state
C           if ( mod(NINT(2*D_Xjlv(i)),2).ne.mintsp) ctmp5 = ' cont'

            if (D_Elv(i) .gt. ELV( NLV(0),0)) then
		    ctmp5 = ' cont'
	      else
		    ctmp5 = '     '
            endif
C
C           For covariance calculation of dynamical deformation
            D_Def(i,2) = ftmp*DEFdyn
C
            WRITE (8,
     &     '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3,1x,i2,A5)')
     &          ICOllev(i), D_Elv(i), D_Xjlv(i), D_Lvp(i), IPH(i),
     &          D_Llv(i), D_Klv(i), D_Def(i,2), D_nno(i), ctmp5
            itmp1 = ICOllev(i)
            if(itmp1.gt.LEVcc) itmp1 = itmp1 - LEVcc

C           IF(D_Def(i,2).GT.0.05d0 .and. ICOllev(i).GE.LEVcc) then
C           Giant Resonances flag: negative deformation 
            IF(D_Def(i,2).LT.0 .and. ICOllev(i).GE.LEVcc) then
              IF(int(D_Xjlv(i)).eq.0) ctmp5=' GMR'
              IF(int(D_Xjlv(i)).eq.2) ctmp5=' GQR'
              IF(int(D_Xjlv(i)).eq.3) ctmp5=' GOR'
              igreson = 1
            ENDIF
            WRITE (12,
     &     '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3,1x,i2,A5)')
     &          itmp1, D_Elv(i), D_Xjlv(i), D_Lvp(i), IPH(i),
     &          D_Llv(i), D_Klv(i), abs(D_Def(i,2)), D_nno(i), ctmp5
C
C           Skipping Giant Resonances (negative deformation) 
C           IF(D_Def(i,2).GT.0 .AND.
C    &            INT(Aejc(0)).eq.1 .and. INT(Zejc(0)).eq.0   ) then
C
C           CHECKING EWSR (only for neutrons)
C
            IF( INT(Aejc(0)).eq.1 .and. INT(Zejc(0)).eq.0   ) then
              ftmp = abs(D_Def(i,2))
              if(ftmp.le.0.01d0 .and. ICOllev(i).LE.LEVcc)
     >                            ftmp = abs(D_Def(1,2)) ! coupled levels
              betasq=ftmp*ftmp
              edis1=D_Elv(i)
              jdis1=int(D_Xjlv(i))

              if (jdis1.eq.0) sgmr = sgmr  - betasq*edis1
              if (jdis1.eq.2) sgqr = sgqr  - betasq*edis1
              if (jdis1.eq.3) sleor= sleor - betasq*edis1
              if(sgmr.gt.0)  isgmr = ICOllev(i)
              if(sgqr.gt.0)  isgqr = ICOllev(i)
              if(sleor.gt.0) isgor = ICOllev(i)            

            ENDIF
           ENDDO
       else
C
C          Rigid rotor (deformed) or vibrational (spherical) optical model
C
C----------empty line
           READ (32,'(a80)') comment
           WRITE (8,'(a80)') comment
           WRITE (12,'(a80)') comment
C----------Ncoll Lmax  IDef (Def(1,j),j=2,IDef,2)
           READ (32,'(a80)') comment
           WRITE (8,'(a80)') comment
           WRITE (12,'(a80)') comment
           DEFormed = .FALSE.
           IF (ABS(DEF(1,0)).GT.0.1D0) DEFormed = .TRUE.

           SOFt = .FALSE.
C
C          If odd nucleus, then rotational model is always used
C          It could be a bad approximation for a quasispherical nucleus
           IF(MOD(NINT(A(0)),2).NE.0 .OR. MOD(NINT(Z(0)),2).NE.0)
     &       DEFormed = .TRUE.

           IF (DEFormed) THEN
C-----------Number of collective levels
            READ (32,'(3x,3I5,1x,F5.1,1x,6(e10.3,1x))') ND_nlv, LMAxcc,
     &            IDEfcc, ftmp, (D_Def(1,j),j = 2,IDEfcc,2)
C           For covariance calculation of static deformation
            DO j= 2,IDEfcc,2
                D_Def(1,j) = D_Def(1,j)*DEFsta
            ENDDO
C
            WRITE (8,'(3x,3I5,1x,F5.1,1x,6(e10.3,1x))') ND_nlv, LMAxcc,
     &             IDEfcc, ftmp, (D_Def(1,j),j = 2,IDEfcc,2)
            WRITE (12,'(3x,3I5,1x,F5.1,1x,6(e10.3,1x))') ND_nlv, LMAxcc,
     &             IDEfcc, ftmp, (D_Def(1,j),j = 2,IDEfcc,2)
           ELSE
            READ (32,'(3x,3I5)') ND_nlv
            WRITE (8,'(3x,3I5)') ND_nlv
            WRITE (12,'(3x,3I5)') ND_nlv
           ENDIF
C----------if nd_nlv=0 , then no collective levels will be considered
C----------setting DIRect to zero
           IF (ND_nlv.EQ.0) THEN
            WRITE (8,*) ' WARNING: ND_NLV=0 in COLLECTIVE.LEV file'
            WRITE (8,*) ' WARNING: No collective levels considered'
            WRITE (8,*) ' WARNING: DIRECT has been set to 0'
C-----------setting DIRect to zero
            DIRect = 0
            IFINDCOLL = 2
            RETURN
           ENDIF
C----------empty line
           READ (32,'(a80)') comment
           WRITE (8,'(a80)') comment
C----------'collective levels:'
           READ (32,'(a80)') comment
           WRITE (8,'(a80)') comment
           WRITE (12,*)' '
           WRITE (12,'(a80)') comment
C----------Reading ground state information (to avoid overwriting deformation)
           READ(32,
     &        '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3)')
     &         ICOllev(1), D_Elv(1), D_Xjlv(1), D_Lvp(1), IPH(1),
     &         D_Llv(1), D_Klv(1)
           WRITE(8,'(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3)')
     &          ICOllev(1), D_Elv(1), D_Xjlv(1), D_Lvp(1), IPH(1),
     &          D_Llv(1), D_Klv(1), 0.01
           WRITE(12,'(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3)'
     &          ) ICOllev(1), D_Elv(1), D_Xjlv(1), D_Lvp(1), IPH(1),
     &          D_Llv(1), D_Klv(1), 0.01
C
C          mintsp = mod(NINT(2*D_Xjlv(1)),2)
           igreson = 0
           DO i = 2, ND_nlv
            READ (32,
     &     '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3,3x,A5)')
     &          ICOllev(i), D_Elv(i), D_Xjlv(i), D_Lvp(i), IPH(i),
     &          D_Llv(i), D_Klv(i), ftmp, ctmp5

C           For odd nuclides, collective states in continuum have
C           different spin than the ground state
C           if ( mod(NINT(2*D_Xjlv(i)),2).ne.mintsp) ctmp5 = ' cont'

            if (D_Elv(i) .gt. ELV( NLV(0),0)) then
		    ctmp5 = ' cont'
	      else
		    ctmp5 = '     '
            endif
C
C           For covariance calculation of dynamical deformation
            D_Def(i,2) = ftmp*DEFdyn
C
C           IF(D_Def(i,2).GT.0.05d0 .and. ICOllev(i).GE.LEVcc) then
C           Giant Resonances flag: negative deformation 
            IF(D_Def(i,2).LT.0 .and. ICOllev(i).GE.LEVcc) then
              IF(int(D_Xjlv(i)).eq.0) ctmp5=' GMR'
              IF(int(D_Xjlv(i)).eq.2) ctmp5=' GQR'
              IF(int(D_Xjlv(i)).eq.3) ctmp5=' GOR'
              igreson = 1
            ENDIF

            WRITE (8,
     &     '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3,3x,A5)')
     &          ICOllev(i), D_Elv(i), D_Xjlv(i), D_Lvp(i), IPH(i),
     &          D_Llv(i), D_Klv(i), D_Def(i,2), ctmp5
            itmp1 = ICOllev(i)
            if(itmp1.gt.LEVcc) itmp1 = itmp1 - LEVcc

            WRITE (12,
     &     '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3,3x,A5)')
     &          itmp1, D_Elv(i), D_Xjlv(i), D_Lvp(i), IPH(i),
     &          D_Llv(i), D_Klv(i), abs(D_Def(i,2)), ctmp5
C
C
C           Skipping Giant Resonances (negative deformation) 
C           IF(D_Def(i,2).GT.0 .AND.
C    &            INT(Aejc(0)).eq.1 .and. INT(Zejc(0)).eq.0   ) then
C
C           CHECKING EWSR (only for neutrons)
C
C           IF(D_Def(i,2).GT.0.d0 .AND.
C    &            INT(Aejc(0)).eq.1 .and. INT(Zejc(0)).eq.0   ) then
            IF( INT(Aejc(0)).eq.1 .and. INT(Zejc(0)).eq.0   ) then
              ftmp = abs(D_Def(i,2))
              if(ftmp.le.0.01d0 .and. ICOllev(i).LE.LEVcc)
     >                            ftmp = abs(D_Def(1,2)) ! coupled levels
              betasq=ftmp*ftmp
              edis1=D_Elv(i)
              jdis1=int(D_Xjlv(i))
              if (jdis1.eq.0) sgmr = sgmr  - betasq*edis1
              if (jdis1.eq.2) sgqr = sgqr  - betasq*edis1
              if (jdis1.eq.3) sleor= sleor - betasq*edis1
              if(sgmr.gt.0)  isgmr = ICOllev(i)
              if(sgqr.gt.0)  isgqr = ICOllev(i)
              if(sleor.gt.0) isgor = ICOllev(i)            
		ENDIF

           ENDDO
         ENDIF
         WRITE(12,*) ' '
         CLOSE (32)

         icoupled = 0 
         DO i = 1,ND_nlv
          if(ICOllev(i).LT.LEVcc) icoupled = icoupled + 1
         ENDDO
         write(8,*)
         write(8,*)
     >       '----------------------------------------------------'
         write(8,*)'  States with number < ',LEVcc, 
     >             '  in the file *-lev.col coupled'
         write(8,*)'  Number of coupled states =',icoupled
         write(8,*)
     >       '----------------------------------------------------'
         write(8,*)
         if(igreson.eq.0 .and.
     &            INT(Aejc(0)).eq.1 .and. INT(Zejc(0)).eq.0   ) then
           if (sgmr.gt.0.) betagmr=sqrt(sgmr/egrcoll(0,1))
           if (sgqr.gt.0.) betagqr=sqrt(sgqr/egrcoll(2,1))
           if (sleor.gt.0.) betalegor=sqrt(sleor/egrcoll(3,1))
           if (sheor.gt.0.) betahegor=sqrt(sheor/egrcoll(3,2))
           write(8,*)
     >       '===================================================='
           write(8,*)'  Energy Weighted Sum Rules for GIANT RESONANCES'
           write(8,*)
           write(8,*)
     > ' You can add Giant Resonances wiht Beta<0 to collective levels'
           write(8,*)
     > '   by editing the collective level file *-lev.col '
           write(8,*)
     > ' If GR deformation is zero, then EWSR is exhausted'
           write(8,*)
     > ' Negative deformation flags the Giant Resonances  '
           write(8,*)
     >       '____________________________________________________'
           write(8,*) '            EWSR       Uexc    Width    Beta '
           write(8,*)
     >       '____________________________________________________'
           if(betagmr.LT.1.d0)
     >       write(8,'(1x,A7,2x,d12.6,1x,3(f6.3,2x),i3)') '  GMR :',
     >       sgmr,egrcoll(0,1),ggrcoll(0,1),-betagmr,isgmr
           if(betagqr.LT.1.d0)
     >       write(8,'(1x,A7,2x,d12.6,1x,3(f6.3,2x),i3)') '  GQR :',
     >       sgqr,egrcoll(2,1),ggrcoll(2,1),-betagqr,isgqr
           if(betalegor.LT.1.d0)
     >       write(8,'(1x,A7,2x,d12.6,1x,3(f6.3,2x),i3)') 'leGOR :',
     >       sleor,egrcoll(3,1),ggrcoll(3,1),-betalegor, isgor
           if(betahegor.LT.1.d0)
     >       write(8,'(1x,A7,2x,d12.6,1x,3(f6.3,2x),i3)') 'heGOR :',
     >       sheor,egrcoll(3,2),ggrcoll(3,2),-betahegor, isgor
           write(8,*)
     >       '===================================================='
           write(8,*)
         endif

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
        SOFt = .FALSE.
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
         OPEN (13,FILE = trim(empiredir)//'/RIPL/levels/'//finp
     &      ,STATUS = 'OLD',ERR = 600)
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

      beta2 = 0.D0
      beta3 = 0.D0
      OPEN (84,FILE = trim(empiredir)//
     &      '/RIPL/optical/om-data/om-deformations.dat',
     &      STATUS = 'old',ERR = 200)
      READ (84,'(///)')    ! Skipping first 4 title lines
      DO i = 1, 1700
         READ (84,'(2I4,4x,f10.6,1x,f4.1,i3,3x,f10.6,2x,a6)',END = 300,
     &         ERR = 300) nztmp, natmp, etmp, jtmp, iptmp, betatmp,
     &                    reftmp
         IF (nztmp.EQ.iz .AND. natmp.EQ.ia .AND. jtmp.EQ.2.D0 .AND.
     &       iptmp.EQ. + 1 .AND. reftmp.EQ.'Raman2') THEN
             beta2 = betatmp
         ENDIF
         IF (nztmp.EQ.iz .AND. natmp.EQ.ia .AND. jtmp.EQ.3.D0 .AND.
     &       iptmp.EQ. - 1 .AND. reftmp.EQ.'Kibedi') THEN
             beta3 = betatmp
         ENDIF
      ENDDO
      GOTO 300
  200 WRITE (8,*) ' WARNING: ',trim(empiredir)//
     &   '/RIPL/optical/om-data/om-deformations.dat not found '
C     WRITE (8,*) ' WARNING: ',
C    &       'Default dynamical deformations 0.15(2+) and 0.05(3-) used'
      GOTO 400
  300 CLOSE (84)
      IF (beta2.NE.0.D0 .OR. beta3.NE.0.D0) THEN
         WRITE (8,'(/1x,A34/1x,A11,F7.3,A13,F7.4)')
     &           'EXPERIMENTAL DEFORMATION (RIPL):', 'BETA (2+) =',
     &          beta2, '  BETA (3-) =', beta3
         IF (DEFormed) THEN
            WRITE (8,*) 'BETA2 ASSUMED AS GS BAND DEFORMATION'
            WRITE (8,*)
         ENDIF
      ENDIF
      IF (beta2.EQ.0.D0) THEN
         WRITE (8,*) ' WARNING: ',
     &    'E(2+) level not found in Raman 2001 database (RIPL)'
         IF(odd) then
            WRITE (8,*) ' WARNING: Odd nucleus, ',
     &            'FRDM deformation will be used for the gs band'
            WRITE (8,*) ' BETA2 = ',DEF(1,0)
            beta2 = DEF(1,0)
         ELSE
            WRITE (8,*) ' WARNING: FRDM deformation will be used'
            WRITE (8,*) ' BETA2 = ',DEF(1,0)
            beta2 = DEF(1,0)
         ENDIF
      ENDIF
      IF (beta3.EQ.0.D0) THEN
         WRITE (8,*) ' WARNING: ',
     &        'E(3-) level not found in Kibedi database (RIPL)'
         WRITE (8,*) ' WARNING: ',
     &            'Default dynamical deformations 0.05(3-) will be used'
         beta3 = 0.05
      ENDIF

  400 DO ilv = 1, nlvs
         READ (32,'(I3,1X,F10.6,1X,F5.1,I3,1X,E10.2,I3)') itmp, elvr,
     &         xjlvr, lvpr, t12, ndbrlin
C
C--------Skipping levels with unknown spin in the discrete level region
         IF (xjlvr.LT.0. .AND. ilv.LE.NLV(nnurec)) CYCLE

         IF(ilv + LEVcc.gt.99) THEN
           WRITE (8,*)
     &     'WARNING: Max.number of uncoupled coll. levels (99) reached'
            GOTO 600
         ENDIF

         IF (xjlvr.LT.0.) THEN ! unknown spin in continuum
C                                assigning randomly 2+,4+,3- spin
           ftmp = drand()
           xjlvr = 2
           lvpr  = 1
           if(ftmp.GT.0.3333d0 .AND. ftmp.LE.0.6666d0) THEN
             xjlvr = 4
             lvpr  = 1
           endif
           if(ftmp.GT.0.66666d0) THEN
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
C           RCN 0811 
            IPH(ND_nlv) = 1
C
            D_Llv(ND_nlv) = 0
            D_Klv(ND_nlv) = 0
            D_Def(ND_nlv,2) = 0.01
            IF (beta2.GT.0.D0 .AND. DEFormed) D_Def(ND_nlv,2) = beta2
            gspin = xjlvr
            gspar = DBLE(lvpr)
         ENDIF

         IF (.NOT.(DEFormed)) THEN
C-----------spherical even-even nuclei follow
C-----------for spherical target taking dynamical deformation equal to RIPL values.
C-----------If RIPL deformation file not found OR target nucleus not found in
C-----------the database we take FRDM deformations
            IF (ilv.EQ.1) THEN
C--------------ground state deformation for spherical nucleus is 0.0
               D_Def(ND_nlv,1) = 0.0
               IF (gspin.NE.0.D0) THEN
                  ICOllev(ND_nlv) = ilv + LEVcc
                  WRITE (8,*)
     &                    ' WARNING: ONLY DWBA CALCULATIONS ALLOWED FOR'
                  WRITE (8,*) ' WARNING: ODD SPHERICAL NUCLEUS'
                  WRITE (8,*) ' WARNING: DIRECT reset to 3'
                  DIRect = 3
               ENDIF
               GOTO 500
            ENDIF
            IF (i20p.EQ.0 .AND. xjlvr.EQ.2.D0 .AND. lvpr.EQ.1) THEN
               i20p = ilv
               ND_nlv = ND_nlv + 1
               ICOllev(ND_nlv) = ilv
               IF (gspin.NE.0.D0 .or. DIRECT.EQ.3)                         ! check
     >                   ICOllev(ND_nlv) = ICOllev(ND_nlv) + LEVcc         ! check
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
               IF (gspin.NE.0.D0 .or. DIRECT.EQ.3)
     >                   ICOllev(ND_nlv) = ICOllev(ND_nlv) + LEVcc
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
               IF (gspin.NE.0.D0 .or. DIRECT.EQ.3)
     >                   ICOllev(ND_nlv) = ICOllev(ND_nlv) + LEVcc
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
               IF (gspin.NE.0.D0 .or. DIRECT.EQ.3)
     >                   ICOllev(ND_nlv) = ICOllev(ND_nlv) + LEVcc
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
               IPH(ND_nlv) = 1
               D_Def(ND_nlv,2) = 0.05
               GOTO 500
            ENDIF
            IF (i1m.EQ.0 .AND. xjlvr.EQ.1.d0 .AND. lvpr.EQ.-1) THEN
               i1m = ilv
               ND_nlv = ND_nlv + 1
               ICOllev(ND_nlv) = ilv + LEVcc
               D_Elv(ND_nlv) = elvr
               D_Lvp(ND_nlv) = lvpr
               D_Xjlv(ND_nlv) = xjlvr
               IPH(ND_nlv) = 1
               D_Def(ND_nlv,2) = beta3
               GOTO 500
            ENDIF
            IF (i3m.EQ.0 .AND. xjlvr.EQ.3.d0 .AND. lvpr.EQ.-1) THEN
               i3m = ilv
               ND_nlv = ND_nlv + 1
               ICOllev(ND_nlv) = ilv 
               IF (DIRECT.EQ.3) ICOllev(ND_nlv) = ICOllev(ND_nlv)+LEVcc
               D_Elv(ND_nlv) = elvr
               D_Lvp(ND_nlv) = lvpr
               D_Xjlv(ND_nlv) = xjlvr
               IPH(ND_nlv) = 1
               D_Def(ND_nlv,2) = beta3
               GOTO 500
            ENDIF
            IF (i5m.EQ.0 .AND. xjlvr.EQ.5.d0 .AND. lvpr.EQ.-1) THEN
               i5m = ilv
               ND_nlv = ND_nlv + 1
               ICOllev(ND_nlv) = ilv + LEVcc
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
               IPH(ND_nlv) = 1
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
               IPH(ND_nlv) = 1
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
     &          i0p.NE.0 ) THEN
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
C              IF (gspin.NE.0.D0 .or. DIRECT.EQ.3)
               IF (DIRECT.EQ.3)
     &            ICOllev(ND_nlv) = ICOllev(ND_nlv) + LEVcc
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
C              IF (gspin.NE.0.D0 .or. DIRECT.EQ.3)
               IF (DIRECT.EQ.3)
     >                   ICOllev(ND_nlv) = ICOllev(ND_nlv) + LEVcc
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
C              IF (gspin.NE.0.D0 .or. DIRECT.EQ.3)
               IF (DIRECT.EQ.3)
     >                   ICOllev(ND_nlv) = ICOllev(ND_nlv) + LEVcc
               D_Elv(ND_nlv) = elvr
               D_Lvp(ND_nlv) = lvpr
               D_Xjlv(ND_nlv) = xjlvr
               IPH(ND_nlv) = 0
               D_Def(ND_nlv,2) = 0.02
               IF(odd) goto 600
               GOTO 500
            ENDIF
            IF (i8p.EQ.0 .AND. xjlvr.EQ.(gspin + 4*delta_k) .AND.
     &          lvpr.EQ.gspar) THEN
C    &          lvpr.EQ.gspar  .AND. .NOT.odd) THEN
               i8p = ilv
               ND_nlv = ND_nlv + 1
               ICOllev(ND_nlv) = ilv
C              IF (gspin.NE.0.D0 .or. DIRECT.EQ.3)
               IF (DIRECT.EQ.3)
     >                   ICOllev(ND_nlv) = ICOllev(ND_nlv) + LEVcc
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
               D_Def(ND_nlv,2) = 0.02
               GOTO 500
            ENDIF
c           IF (i12p.EQ.0 .AND. xjlvr.EQ.(gspin + 6*delta_k) .AND.
c    &          lvpr.EQ.gspar  .AND. .NOT.odd) THEN
c              i12p = ilv
c              ND_nlv = ND_nlv + 1
c              ICOllev(ND_nlv) = ilv + LEVcc
c              D_Elv(ND_nlv) = elvr
c              D_Lvp(ND_nlv) = lvpr
c              D_Xjlv(ND_nlv) = xjlvr
c              IPH(ND_nlv) = 0
c              D_Def(ND_nlv,2) = beta2*0.25
c              GOTO 500
c            ENDIF
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
     &          i22p.NE.0 ) THEN
C    &          i22p.NE.0 .AND. i10p.NE.0 .AND. i12p.NE.0) THEN
               ierr = 0
               GOTO 600
            ENDIF
         ENDIF
  500 ENDDO
  600 IFINDCOLL = ierr
      CLOSE (32)
      OPEN (UNIT = 32,FILE = 'TARGET_COLL.DAT',STATUS = 'UNKNOWN')
      IF (.NOT.DEFormed) THEN
         WRITE (8,*)
         WRITE (8,'(A76)')
     &' Collective levels selected automatically from target levels (vib
     &rat. model)'
         WRITE (8,*)
     &          ' N <',LEVcc,' for coupled levels in CC calculation'
         WRITE (8,'(1x,i3,1x,i3,a35)') iz, ia,
     &                                ' nucleus is treated as spherical'
         WRITE (32,'(A76)')
     &' Collective levels selected automatically from target levels (vib
     &rat. model)'
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
            IF (ICOllev(i).GE.LEVcc) cycle
            DO j = i + 1, ND_nlv
               IF (ICOllev(j).GE.LEVcc) cycle
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
            ENDDO
         ENDDO
         WRITE (8,*)
         WRITE (8,*) '   Ncoll  '
         WRITE (8,'(3x,I5)') ND_nlv
         WRITE (8,*)
         WRITE (8,*) ' N   E[MeV]  J   pi Nph L  K   Dyn.Def.'
         WRITE (32,*)
         WRITE (32,*) '   Ncoll  '
         WRITE (32,'(3x,3I5)') ND_nlv
         WRITE (32,*)
         WRITE (32,*)' N   E[MeV]  J   pi Nph L  K   Dyn.Def.'
         WRITE (12,*)
         WRITE (12,*) 'Collective levels used in direct calculations'
         WRITE (12,*)
         WRITE (12,*) '   Ncoll  '
         WRITE (12,'(3x,3I5)') ND_nlv
         WRITE (12,*)
         WRITE (12,*)' N   E[MeV]  J   pi Nph L  K   Dyn.Def.'

         ncont = NLV(nnurec) + LEVcc
         mintsp = mod(NINT(2*D_Xjlv(1)),2)
         DO i = 1, ND_nlv
            ftmp = D_Def(i,2)
            IF (i.EQ.1) ftmp = 0.0
            itmp1 = ICOllev(i)
            if(itmp1.gt.LEVcc) itmp1 = itmp1 - LEVcc
            IF (itmp1.LE.NLV(nnurec) .and.
C              For odd nuclides, collective states in continuum have
C              different spin than the ground state
     &         (mod(NINT(2*D_Xjlv(i)),2).eq.mintsp) )THEN
              WRITE (32,
     &           '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3)')
     &           ICOllev(i), D_Elv(i), D_Xjlv(i), D_Lvp(i), IPH(i),
     &           D_Llv(i), D_Klv(i), ftmp
              WRITE (12,
     &           '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3)')
     &           itmp1, D_Elv(i), D_Xjlv(i), D_Lvp(i), IPH(i),
     &           D_Llv(i), D_Klv(i), ftmp
              WRITE (8,
     &           '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3)')
     &           ICOllev(i), D_Elv(i), D_Xjlv(i), D_Lvp(i), IPH(i),
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
              WRITE (8,
     &           '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3,a5)')
     &           ncont, D_Elv(i), D_Xjlv(i), D_Lvp(i), IPH(i),
     &           D_Llv(i), D_Klv(i), ftmp,' cont'
            ENDIF
         ENDDO
 653     WRITE(12,*) ' '
      ELSE
         WRITE (8,*)
         WRITE (8,'(A75)')
     &' Collective levels selected automatically from target levels (rig
     &id rotor) '
         WRITE (8,*)
     &          ' N <',LEVcc,' for coupled levels in CC calculation'
         WRITE (8,'(1x,i3,1x,i3,a35)') iz, ia,
     &                                 ' nucleus is treated as deformed'
         WRITE (32,'(A75)')
     &' Collective levels selected automatically from target levels (rig
     &id rotor) '
         WRITE (32,*)
     &          ' N <',LEVcc,' for coupled levels in CC calculation'
         WRITE (32,'(1x,i3,1x,i3,a35)') iz, ia,
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
         WRITE (8,*)
         WRITE (8,*) '   Ncoll  Lmax IDef  Kgs  (Def(1,j),j=2,IDef,2)'
         WRITE (8,'(3x,3I5,1x,F5.1,1x,6(e10.3,1x))') ND_nlv, LMAxcc,
     &          IDEfcc, D_Xjlv(1), (D_Def(1,j),j = 2,IDEfcc,2)
         WRITE (8,*)
         WRITE (8,*) ' N   E[MeV]  J   pi Nph L  K   Dyn.Def.'

         WRITE (32,*)
         WRITE (32,*) '   Ncoll  Lmax IDef  Kgs  (Def(1,j),j=2,IDef,2)'
         WRITE (32,'(3x,3I5,1x,F5.1,1x,6(e10.3,1x))') ND_nlv, LMAxcc,
     &          IDEfcc, D_Xjlv(1), (D_Def(1,j),j = 2,IDEfcc,2)
         WRITE (32,*)
         WRITE (32,*)' N   E[MeV]  J   pi Nph L  K   Dyn.Def.'

         WRITE (12,*)
         WRITE (12,*) 'Collective levels used in direct calculations'
         WRITE (12,*)
         WRITE (12,*) '   Ncoll  '
         WRITE (12,'(3x,3I5)') ND_nlv
         WRITE (12,*)
         WRITE (12,*)' N   E[MeV]  J   pi Nph L  K   Dyn.Def.'

         ncont = NLV(nnurec) + LEVcc
         mintsp = mod(NINT(2*D_Xjlv(1)),2)

         DO i = 1, ND_nlv
            ftmp = D_Def(i,2)
            IF (i.EQ.1) ftmp = 0.01

            itmp1 = ICOllev(i)
            if(itmp1.gt.LEVcc) itmp1 = itmp1 - LEVcc
            IF (itmp1.LE.NLV(nnurec) .and.
C             For odd nuclides, collective states in continuum have
C             different spin than the ground state
     &        (mod(NINT(2*D_Xjlv(i)),2).eq.mintsp) )THEN
              WRITE (32,
     &           '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3)')
     &           ICOllev(i), D_Elv(i), D_Xjlv(i), D_Lvp(i), IPH(i),
     &           0, 0, ftmp
              WRITE (8,
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
              WRITE (8,
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
 5432 WRITE (8,*) ' WARNING: EMPTY or missing -col.lev file'
      WRITE (8,*) ' WARNING: No collective levels considered'
      WRITE (8,*) ' WARNING: DIRECT has been set to 0'
C-----setting DIRect to zero
      DIRect = 0
      ierr = 2
      IFINDCOLL = ierr
      RETURN
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
  100 READ (Ki,'(A3)',END = 200) ctmp
      IF (ctmp.NE.'+++') GOTO 100
      READ (Ki,*,END = 200) iref
      IF (iref.NE.Ipoten) GOTO 100
      BACKSPACE (Ki)
      RETURN
  200 Ieof = 1
      END
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
Ccc  *  Initially attempts are made to set parameter 'beta' and first-  *
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
      CHARACTER*64 EMPiredir
      COMMON /GFLPARAM/ BETagfl2, S2Plusgfl
      COMMON /GSA   / KEY_shape, KEY_gdrgfl
      COMMON /MLOCOM2/ KEYload, KZZ, KAA
      COMMON /MLOCOM3/ NNZ, NNA, NNG, HE1, HCS1, HGW1, HE2, HCS2, HGW2,
     &                 NANz, NANa, HALpha2, HBEtagfl, HENergygfl, NZRam,
     &                 NARam, NUMram
      COMMON /PARGDR/ EG1, GW1, CS1, EG2, GW2, CS2, NG

      COMMON /GLOBAL_E/ EMPiredir
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
         OPEN (81,FILE = trim(empiredir)//'/RIPL/gamma'
     &      //'/gdr-parameters-exp.dat',STATUS = 'old',ERR = 450)
         READ (81,'(///)') ! Skipping first 4 title lines
         DO i = 1, 270
            READ (81,'(2I4, 1x,2x,3x, i3, 6F7.2)',END = 50,ERR = 50)
     &            NNZ(i), NNA(i), NNG(i), HE1(i), HCS1(i), HGW1(i),
     &            HE2(i), HCS2(i), HGW2(i)
         ENDDO
   50    CLOSE (81)
  100    OPEN (81,FILE = trim(empiredir)//'/RIPL/gamma/'
     &      //'gdr-parameters-theor.dat',STATUS = 'old',ERR = 500)
         READ (81,'(///)') ! Skipping first 4 title lines
         DO i = 1, MAXGDR
            READ (81,'(2I4, 1x,2x, f7.3, 4F7.2)',END = 150,ERR = 150)
     &            nnzt(i), nnat(i), etat(i), he1t(i), hgw1t(i), he2t(i),
     &            hgw2t(i)
            nngt(i) = 2
            IF (he1t(i).EQ.he2t(i)) nngt(i) = 1
         ENDDO
  150    CLOSE (81)
  200    OPEN (82,FILE = trim(empiredir)//'/data/deflib.dat'
     &      ,STATUS = 'old',ERR = 550)
         READ (82,'(////)') ! Skipping first 5 title lines
         DO i = 1, 9000
            READ (82,'((2I4, f7.3))',END = 250,ERR = 250) NANz(i),
     &            NANa(i), HALpha2(i)
         ENDDO
  250    CLOSE (82)
  300    OPEN (84,FILE = trim(empiredir)//'/RIPL/optical/om-data'
     &      //'/om-deformations.dat',STATUS = 'old',ERR = 600)
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
         WRITE (8,*) ' RIPL GDR parameters used'
         GOTO 700
  400    CLOSE (84)
  450    WRITE (8,'(1x,A14,A42,A43)') ' WARNING: File ',
     &                      'empire/RIPL/gamma/gdr-parameters-exp.dat'
     &                          ,
     &                     ' not found, theoretical RIPL will be used'
         GOTO 100
  500    WRITE (8,'(1x,A14,A43,A42)') ' WARNING: File ',
     &                    'empire/RIPL/gamma/gdr-parameters-theor.dat'
     &                        ,
     &                     ' not found, default GDR values will be used'
         GOTO 200
  550    WRITE (8,'(1x,A14,A18,A43)')
     &                           ' WARNING: File empire/data/deflib.dat'
     &                               ,
     &             ' not found, default deformation values will be used'
         GOTO 300
  600    WRITE (8,*) ' WARNING: ',trim(empiredir)//
     &   '/RIPL/optical/om-data/om-deformations.dat',
     &   ' not found, default dynamical deformation values used'
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
C     If experimental values of GDR parameters not found and Key_GDRGFL=2
C     they are going to be retrieved from the RIPL Goriely theoretical
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
C--------------RIPL energies in MeV, RCN 06/2004
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

      Function grand_old()
C     Generator of normally distributed random numbers based on R250
      Integer*4 newrand
      Integer*4 indexf, indexb, buffer(250)
      Integer i
      REAL*8 grand_old, m
      Parameter (m = 2147483648.D0)
      Common/R250COM/indexf,indexb,buffer

      grand_old = -6.d0
      do i=1,12
        newrand = ieor( buffer(indexf), buffer(indexb) )
        buffer(indexf) = newrand

        indexf = indexf + 1
        if ( indexf .gt. 250 ) indexf = 1

        indexb = indexb + 1
        if ( indexb .gt. 250 ) indexb = 1

        grand_old = grand_old + dble(newrand)/m
      enddo

      return
      End

      FUNCTION grand() RESULT(fn_val)
      REAL*8 fn_val
      !     Local variables
      REAL   :: s = 0.449871, t = -0.386595, a = 0.19600, b = 0.25472,
     &          r1 = 0.27597, r2 = 0.27846, u, v, x, y, q, half = 0.5
      !     Generate P = (u,v) uniform in rectangle enclosing acceptance region
      Integer*4 indexf, indexb, buffer(250)
      REAL*8 drand
      Common/R250COM/indexf,indexb,buffer
10    continue
      DO
          u = drand()
          v = drand()

        v = 1.7156 * (v - half)
      !     Evaluate the quadratic form
        x = u - s
        y = ABS(v) - t
        q = x**2 + y*(a*y - b*x)
      !     Accept P if inside inner ellipse
        IF (q < r1) EXIT
      !     Reject P if outside outer ellipse
        IF (q > r2) CYCLE
      !     Reject P if outside acceptance region
        IF (v**2 < -4*LOG(u)*u**2) EXIT
      END DO
      !     Return ratio of P's coordinates as the normal deviate
      fn_val = v/u
C     if(ABS(v).gt.3.d0*ABS(u)) goto 10
      RETURN
      END FUNCTION grand

      SUBROUTINE CHECK_DE(Energy,Limit)
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:ppu*
Ccc   *                    C H E C K _ D E                               *
Ccc   *                                                                  *
Ccc   *  Checks whether the size of energy bin DE is big enough to       *
Ccc   *  ensure that population and spectra arrays are sufficiently      *
Ccc   *  dimensioned. If not, it adjusts number of bins in the CN until  *
Ccc   *  DE is big enough.                                               *
Ccc   *                                                                  *
Ccc   *  Input: Energy - energy that has to be handled                   *
Ccc   *         Limit  - dimension that must not be exceeded             *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
      INTEGER Limit
      REAL*8 Energy
      IF(INT(Energy/DE+1).LT.Limit) RETURN
   10 NEX(1) = NEX(1) - 1
      IF(NEX(1).EQ.1) THEN
         WRITE(8,*) 'ERROR DEFINING ENERGY STEP'
         WRITE(8,*) 'REPORT TO mwherman@bnl.gov      '
         STOP ' FATAL ERROR DEFINING ENERGY STEP'
      ENDIF
      DE = (EMAx(1) - ECUt(1))/FLOAT(NEX(1) - 1)
      IF(INT(Energy/DE+1).GE.Limit) GOTO 10
      WRITE(8,*) ' WARNING: Number of energy steps set to ',NEX(1)
      RETURN
      END

