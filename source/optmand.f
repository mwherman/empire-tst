C      VERSION 12 (Nov 2011, LANE CONSISTENT COULOMB CORRECTION
C      WITH DISPERSIVE OPTICAL POTENTIAL RELATIONS, GLOBAL SEARCH, NATURAL
C      FOR TOTAL AND ISOTOPES RATIO ADJUSTMENT, LOW ENERGY RESONANCES)
C
C      FULL DOUBLE PRECISION (REAL*8 = DOUBLE PRECISION, COMPLEX*16 = DOUBLE COMPLEX)
C      To allow automatic extension to quadruple precision (REAL*16, COMPLEX*32)
C
C      AUTHORS:
C
C      EFREM SOUKHOVITSKI - NON-AXIAL SOFT-ROTATOR NUCLEAR MODEL THEORY, MAIN
C                           CC COMPUTATIONAL ALGORITHMS AND CODING, LANE
C                           CONSISTENNCY AND(P,N) CS
C                           E-MAIL: esukhov@sosny.bas-net.by
C
C      SATOSHI CHIBA      - THEORY DEVELOPMENT in COOPERATION WITH
C                           E. SOUKHOVITSKI, E-MAIL: chiba.satoshi@jaea.go.jp
C
C      ROBERTO CAPOTE     - DISPERSIVE OPTICAL MODEL POTENTIAL RELATIONS,
C                           LANE CONSISTENNCY AND (P,N) CS,RIPL INTERFACE,
C                           E-MAIL: R.CapoteNoy@iaea.org
C
C      JOSE M. QUESADA    - DISPERSIVE OPTICAL MODEL POTENTIAL RELATIONS,
C                           LANE CONSISTENCY,E-MAIL: quesada@us.es
C
C      MAIN REFERENCES:   1. E.SH. SOUKHOVITSKII, S. CHIBA, R. CAPOTE, JOSE M.
C                            QUESADA, S. KUNIEDA and G.B. MOROGOVSKII, TECHNICAL
C                            REPORT,JAEA-DATA/CODE 2008-025, JAPAN ATOMIC ENERGY
C                            AGENCY, 2008.
C                         2. E.SH. SOUKHOVITSKII, S. CHIBA, O.IWAMOTO, K.SHIBATA
C                            T. FUKAHORI and G.B. MOROGOVSKII, TECHNICAL REPORT,
C                            JAERI-DATA/CODE 2005-002, JAPAN ATOMIC ENERGY
C                            INSTITUTE, 2005.
C                         3. E.SH. SOUKHOVITSKII, R. CAPOTE, J.M. QUESADA,
C                            S. CHIBA, PHYS. REV. C72, 024604 (2005).
C                         4. J.M. QUESADA, R. CAPOTE, E.Sh. SOUKHOVITSKI and
C                            S. CHIBA, PHYS. REV. C76, 057602 (2007).
C                         5. MORE DETAILS, MANUAL,CODE'S SOURCE FILES AND INPUTS
C                            WITH VERIOUS CC OPTICAL POTENTIALS CAN BE FOUND ON:
C                            http://www-nds.iaea.org/RIPL-3/
C
C
C  ****************************************************************
      PROGRAM OPTMAN12
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8, DIMENSION(800) :: A
      CHARACTER(20) :: FNAme
      INTEGER :: MEApp, MECha, MECul, MEDis, MEHam, MEHao, MEJob, MEPot, 
     &           MEPri, MERel, MERip, MERrr, MERzz, MESha, MESho, MESol, 
     &           MEVol
      COMMON /INOUT / FNAme
      COMMON /LOFAC / A
      COMMON /MENU  / MEJob, MEPot, MEHam, MECha, MEPri, MESol, MESha, 
     &                MESho, MEHao, MEApp, MEVol, MERel, MECul, MERzz, 
     &                MERrr, MEDis, MERip
C
C Local variables
C
      REAL*8 :: dtmp
      LOGICAL :: empire
      INTEGER :: i, j
      REAL*8, DIMENSION(400) :: lfa
      REAL, DIMENSION(20) :: title
C
C*** End of declarations rewritten by SPAG
C
C     SUBROUTINE OPTMAN12(fname)
C  *************************************************************
 
 
 
C----------------------------------------------------------------------------
C----------------------------------------------------------------------------
C     FACTORIAL CALCULATION AVOIDING A LONG DATA STATEMENT (common /LOFAC/A)
C            but keeping the same precision
C   (a long data statement was producing errors/warnings with some compilers)
C
      DO i = 1, 800
        A(i) = 0.D0
      ENDDO
      lfa(1) = 0.D0
      DO i = 2, 400
        dtmp = i
        lfa(i) = LOG(dtmp) + lfa(i - 1)
      ENDDO
      DO j = 6, 800
        IF(MOD(j,2).EQ.0)A(j) = lfa(j/2 - 1)
C       if(j.lt.15) write(*,*) j,A(j)
      ENDDO
C
C     FORMER BLOCK DATA VALUES
C     DATA A/0.d0,0.d0,0.d0,0.d0,
C    *0.,.693147180559945309417232D+00,0.,.179175946922805500081248D+01,
C    *0.,.317805383034794561964694D+01,0.,.478749174278204599424770D+01,
C    *0.,.657925121201010099506018D+01,0.,.852516136106541430016553D+01,
C    *0.,.106046029027452502284172D+02,0.,.128018274800814696112077D+02,
C----------------------------------------------------------------------------
C----------------------------------------------------------------------------
C
C     Logical variable EMPIRE
C     EMPIRE = .TRUE.  -> OPTMAN used within the EMPIRE system
C     EMPIRE = .FALSE. -> OPTMAN used in stand-alone mode
C
      empire = .TRUE.
 
      IF(empire)THEN
C--------------------- EMPIRE related i/o changes ----------------------
C       Input filename fixed to OPTMAN.INP for EMPIRE
        OPEN(UNIT = 20,FILE = 'OPTMAN.INP',STATUS = 'OLD')
C       Output root filename fixed to ecis06 for EMPIRE
        FNAme = 'ecis06'
C       Output filename fixed to OPTMAN.OUT for EMPIRE
 
        OPEN(UNIT = 21,FILE = 'OPTMAN.OUT')
 
        WRITE(21,'(5x,A)')
     &                 '***********************************************'
        WRITE(21,'(5x,A)')
     &                 '*      CODE OPTMAN VERSION 12 ( Nov 2011)     *'
        WRITE(21,'(5x,A)')
     &                 '*  DISPERSIVE RELATIONS AND LANE CONSISTENCY  *'
        WRITE(21,'(5x,A)')
     &                 '*      LANE CONSISTENT COULOMB CORRECTION     *'
        WRITE(21,'(5x,A)')
     &                 '*            GLOBAL POTENTIAL SEARCH          *'
        WRITE(21,'(5x,A)')
     &                 '*  OTHER BANDS LEVELS COUPLING OPTION USING   *'
        WRITE(21,'(5x,A)')
     &                 '*   ROTATIONAL MODEL POTENTIAL MULTIPOLES     *'
        WRITE(21,'(5x,A)')
     &                 '*---------------------------------------------*'
        WRITE(21,'(5x,A)')
     &                 '*   COMPATIBLE WITH THE EMPIRE SYSTEM         *'
        WRITE(21,'(5x,A)')
     &                 '***********************************************'
 
      ELSE
C--------------------- FOR NORMAL OPERATION (NOT EMPIRE) ---------------
        WRITE(*,'(A)')' ***********************************************'
        WRITE(*,'(A)')' *      CODE OPTMAN VERSION 12 ( Nov 2011)     *'
        WRITE(*,'(A)')' *  DISPERSIVE RELATIONS AND LANE CONSISTENCY  *'
        WRITE(*,'(A)')' *      LANE CONSISTENT COULOMB CORRECTION     *'
        WRITE(*,'(A)')' *            GLOBAL POTENTIAL SEARCH          *'
        WRITE(*,'(A)')' *  OTHER BANDS LEVELS COUPLING OPTION USING   *'
        WRITE(*,'(A)')' *   ROTATIONAL MODEL POTENTIAL MULTIPOLES     *'
        WRITE(*,'(A)')' ***********************************************'
C
        WRITE(*,'(1X,A40)')'INPUT FILE NAME (without extension) ? =>'
        READ(*,'(A20)')FNAme
        IF(FNAme(1:1).EQ.'')FNAme = 'OPTMAN'
C       open(unit=20,file=TRIM(fname)//'.inp',STATUS='OLD')
C
C       This is the right statement, but produces MSF internal compiler error
C       open(unit=20,file=TRIM(fname)        ,STATUS='OLD')
C
C       Adding a virtual space '' solves the compiler problem
        OPEN(UNIT = 20,FILE = TRIM(FNAme)//'.INP',STATUS = 'OLD')
C
        OPEN(UNIT = 21,FILE = TRIM(FNAme)//'.OUT',STATUS = 'NEW')
 
        WRITE(21,'(5x,A)')
     &                 '***********************************************'
        WRITE(21,'(5x,A)')
     &                 '*      CODE OPTMAN VERSION 12 ( Nov 2011)     *'
        WRITE(21,'(5x,A)')
     &                 '*  DISPERSIVE RELATIONS AND LANE CONSISTENCY  *'
        WRITE(21,'(5x,A)')
     &                 '*      LANE CONSISTENT COULOMB CORRECTION     *'
        WRITE(21,'(5x,A)')
     &                 '*            GLOBAL POTENTIAL SEARCH          *'
        WRITE(21,'(5x,A)')
     &                 '*  OTHER BANDS LEVELS COUPLING OPTION USING   *'
        WRITE(21,'(5x,A)')
     &                 '*   ROTATIONAL MODEL POTENTIAL MULTIPOLES     *'
        WRITE(21,'(5x,A)')
     &                 '*---------------------------------------------*'
        WRITE(21,'(5x,A)')
     &                 '*   COMPATIBLE WITH THE EMPIRE SYSTEM         *'
        WRITE(21,'(5x,A)')
     &                 '***********************************************'
 
C       TRANSME ARRAY FOR TRANSITIONS TL
        OPEN(UNIT = 22,FILE = 'TRANSME')
C       GNASH ARRAY FOR TRANSITIONS TLJ, J=L+ - 1/2
        OPEN(UNIT = 24,FILE = 'GNASH')
        OPEN(UNIT = 23,FILE = 'CR-SECT')
        OPEN(UNIT = 25,FILE = 'ANG-DIST')
        OPEN(UNIT = 26,FILE = 'ANG-POL')
        OPEN(UNIT = 27,FILE = 'ANGDIS-yw')
C
C       EMPIRE FILES FOR CROSS SECTIONS, TLs
C       ecis06 format, printout of TLs,LEG and ANG distributions
C                      is supressed for the time being
C
        OPEN(UNIT = 93,FILE = TRIM(FNAme)//'.cs')
        OPEN(UNIT = 98,FILE = TRIM(FNAme)//'.ics')
C       open(unit=92,file=TRIM(fname)//'.tl')
C       open(unit=96,file=TRIM(fname)//'.leg')
C       open(unit=97,file=TRIM(fname)//'.ang')
 
      ENDIF
 
      CALL THORA(21)
 
      READ(20,1020)title
      WRITE(21,'(7x,20A4/)')title
      READ(20,1010)MEJob, MEPot, MEHam, MEPri, MESol, MESha, MESho, 
     &             MEHao, MEApp, MEVol, MERel, MECul, MERzz, MERrr, 
     &             MEDis, MERip
 
C     FOR EMPIRE OPERATION MEPRI IS SET TO 99
      IF(empire)MEPri = 99
 
 1010 FORMAT(20I2)
 1020 FORMAT(20A4)
C#1   MEJOP 1-JOB STANDARD*2-JOB WITH POTENTIAL OPTIMIZATION.
C#2   MEPOT 1-POT-AL OF ROT.MODEL  YL0* 2-POT-AL EXPANDED BY BETTA
C#3   MEHAM 1-RM* 2-VM* 3-DCHM* 6-5PARM* 4-FDM* 5-5PAR0M* 7-COUPL.GB
C#4   MEPRI OUTPUT MANAGEMENT * 0-MINIMUM (99 = EMPIRE I.E. NO SCREEN OUTPUT AND LIMITED FILE OUTPUT
C           OUTPUT MANAGEMENT * 99- FOR EMPIRE I.E. NO SCREEN OUTPUT AND LIMITED FILE OUTPUT
C#5   MESOL 1-OPTIMIZED 2-EXACT >3-ITERATION METHODS
C           3-ZERO APPROX..-SPHERICAL OPTICAL MODEL
C          >3-ZERO APPROX. HAS THIS NUMBER OF COUPLED EQ.
C#6   MESHA 1-QUADR*2-+HEXAD.AXIAL.* 3-+HEXAD.N.AXIAL. DEF BY  GAM.
C     *4-COMMON CASE.
C#7   MESHO 0-NO *1-AXIAL.*2-NON-AXIAL OCTUPOLE DEFORMATION
C#8   MEHAO 0-NO *1-CONSIDER. OF OCT OSC. * 2-SIMMET. OCTUPOLE OSC. ScaLED BY \beta^2
C           3-2-SIMMET. OCTUPOLE OSC. NOT SCALED BY \beta^2
C#9   MEAPP 0-EXECT SOLUTION; *1-QUICK SOLUTION WITHOUT LEVEL'S  POTENTIAL DEPENDANCE
C           2- The most common case
C#10  MEVOL 0-STANDARD SOLUTION; *>1- ACCOUNT OF NUCLEAR VOLUME
C     CONSERVATION
C#11  MEREL 0-STANDARD SOLUTION;
C           1-ACCOUNT OF RELATIVISTIC KINEM AND POTEN. DEPENDENCE
C           2-ACCOUNT OF RELATIVISTIC KINEMATICS
C           3-ACCOUNT OF RELATIVISTIC KINEM AND REAL POTEN. DEPENDENCE
C#12  MECUL 0-COULOMB CORRECTION PROPORTIONAL TO DERIVATIVE OF REAL PORENTIAL
C           1-CONSTANT
C           2-LANE CONSISTANT COULOMB CORRECTION NUCLEAR POTENTIAL ENERGY FOR PROTONS EQUAL TO
C           INCIDENT ENERGY - CDE, APPLIED TO BOTH REAL AND UMAGINARY POTENTIAL
C           3-LANE CONSISTANT COULOMB CORRECTION NUCLEAR POTENTIAL ENERGY FOR PROTONS EQUAL TO
C           INCIDENT ENERGY - CDE, APPLIED TO REAL POTENTIAL ONLY
C#13  MERZZ 0-CHARGE RADIUS -CONSTANT
C           1-ENERGY DEPENDENT
C#14  MERRR 0-REAL RADIUS IS ENERGY INDEPENDENT
C           1-REAL RADIUS IS ENERGY DEPENDENT
C#15  MEDIS 0-WITHOUT ACCOUNT OF DISPERSION RELATIONS BETWEEN REAL AND IMAGINARY POTENTIALS
C           1-ACCOUNT OF DISPERSION RELATIONS BETWEEN REAL AND IMAGINARY POTENTIALS
C           2-ACCOUNT OF DISPERSION RELATIONS BETWEEN REAL AND IMAGINARY POTENTIALS,
C           EXCLUDING SPIN ORBIT POTENTIAL
C#16  MERIP 0-IN ABCT READS POTENTIAL ONE TIME WITH ANALITICAL DEPENDENCIES FOR ALL ENERGIES
C           1-ABCT READS POTENTIAL BLOCKS FOR EACH ENERGY TO USE RIPL COMPILED INPUTS
C
      IF(MEJob.NE.2)THEN
C
C        OMP CALCULATION
C
        CALL ABCT
 
      ELSE
C
C        OMP FITTING
C
        CALL DATET
 
      ENDIF
 
      CLOSE(20)
      CLOSE(21)
 
C
C     RETURN
C
      END PROGRAM OPTMAN12
 
!---------------------------------------------------------------------------
C     *******************************************************
      SUBROUTINE ABCT
C     *******************************************************
      IMPLICIT DOUBLE PRECISION(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: AC, AC0, AC1, AD, AD0, AD1, ALAso, ALAvr, ALAwd, ALF, 
     &          ALFnew, AMB0, AMG0, AMO, AMUo, ANEu, AR, AR0, AR1, AS, 
     &          AS0, AS1, ASP, AT, AW, AW0, AW1, AZ, BB32, BB42, BET0, 
     &          BET3, BET4, BMO, BNDc, CCOul, CISo, CMO, CONz, CSR, CSS, 
     &          CST, DELg, DPAr, EA, EFErmn, EFErmp, EN, ETA, ETO, GAM0, 
     &          GAMde, GAMg, GSHape, HW, HWO, PDIs, RC, RD, RR, RRBwc, 
     &          RRWid, RS, RW, RZ, RZBwc, RZWid, SF0, SF1, SF2, VD, VR, 
     &          VR0, VR1, VR2, VR3, VRD, VRDc, VRLa, VS
      REAL*8, DIMENSION(10) :: BET, ERN, GNN, GREn
      REAL*8, DIMENSION(20) :: BETb, CM, CSN, EL, ES, WN, WNK
      REAL*8, DIMENSION(20,180) :: COEf
      REAL*8, DIMENSION(90) :: COEfi, COEfr
      REAL*8, DIMENSION(20,90) :: COPh
      REAL*8, DIMENSION(20,150) :: DISc
      REAL*8, DIMENSION(50) :: EE
      CHARACTER(20) :: FNAme
      INTEGER, DIMENSION(180) :: INC, INR, JS
      INTEGER :: INCc, INCr, KODma, LAS, LKK, LLMa, MEApp, MECha, MECul, 
     &           MEDis, MEHam, MEHao, MEJob, MEPot, MEPri, MERel, MERip, 
     &           MERrr, MERzz, MESha, MESho, MESol, MEVol, MTEt, NCLl, 
     &           NCMa, NJ, NMAx, NPD, NREsn, NSMa, NSS, NST, NUF, NUI, 
     &           NUR
      INTEGER, DIMENSION(10) :: JCOn, JMN, LON, NEL
      INTEGER, DIMENSION(20) :: JJ, JO, KO, NCA, NNB, NNG, NNO, NPI, 
     &                          NPO, NTU, NUMb
      INTEGER, DIMENSION(50) :: MCHae
      REAL*8, DIMENSION(180,150) :: PL
      REAL*8, DIMENSION(150) :: TET
      REAL*8 :: WC, WC0, WC1, WCA1, WCBw, WCIso, WCWid, WD, WD0, WD1, 
     &          WDA1, WDBw, WDIso, WDShi, WDWid, WDWid2, WS0, WS1, WSBw, 
     &          WSWid, ZNUc
      COMMON /COUL  / CONz, ETA, COPh
      COMMON /COULCO/ COEfr, COEfi
      COMMON /CS1   / CSN, CM
      COMMON /CSB   / CST, CSR, NST
      COMMON /DISCAN/ DISc, PL, COEf, LKK
      COMMON /DISK  / TET, MTEt
      COMMON /DISPE / VD, VRDc, EA, WDIso
      COMMON /DISPE2/ VRD, WDShi, WDWid2, ALFnew
      COMMON /ENA   / EN, EL, BET, NUR, NMAx, NPD, LAS
      COMMON /ENB   / EE, MCHae
      COMMON /INOUT / FNAme
      COMMON /INRM  / AMO, BMO, CMO, BB42, GAMg, DELg
      COMMON /JNN   / CSS, INCc, NCLl, NSS, NJ, INCr
      COMMON /MENU  / MEJob, MEPot, MEHam, MECha, MEPri, MESol, MESha, 
     &                MESho, MEHao, MEApp, MEVol, MERel, MECul, MERzz, 
     &                MERrr, MEDis, MERip
      COMMON /NCLMA / LLMa, NCMa, NSMa, KODma
      COMMON /NU    / NUI, NUF
      COMMON /POT1  / VR3, VR2, VR1, VR0, WC1, WC0, WD1, WD0, VS, AC0, 
     &                AR0, AW0, AD0, AS0
      COMMON /POT2  / AR1, AC1, AW1, AD1, AS1
      COMMON /POT3  / BNDc, WDA1, WCA1, CCOul, CISo, WCIso, WS0, WS1
      COMMON /POTB  / WNK, WN, VR, WC, WD
      COMMON /POTD  / ALAvr, VRLa, WCBw, WCWid, WDBw, WDWid, ALAwd, 
     &                EFErmn, EFErmp, ALAso, PDIs, WSBw, WSWid, RRBwc, 
     &                RRWid, RZBwc, RZWid
      COMMON /QNB   / JO, NPO, KO, NCA
      COMMON /QNBBAND/ NUMb, BETb
      COMMON /QNSB  / INC, INR, JS
      COMMON /RAD   / RR, RC, RD, RW, RS, AR, AC, AW, AD, AS, ALF, AT, 
     &                ANEu, RZ, ZNUc, ASP, AZ
      COMMON /RESONI/ ERN, GNN, GREn, LON, JMN, JCOn, NEL, NREsn
      COMMON /SF12  / SF0, SF1, SF2
      COMMON /SHAMO / BET3, ETO, AMUo, HWO, BB32, DPAr
      COMMON /SHEM1 / HW, AMG0, AMB0, GAM0, BET0, BET4, GAMde, GSHape
      COMMON /SHEMM / ES, JJ, NTU, NNB, NNG, NNO, NPI
C
C Local variables
C
      REAL*8 :: ami, asq, ati, caac, caar, card, carr, cavr, enc, rel
      CHARACTER(1) :: CPAR
      INTEGER :: i, i1, ii, ilev, k, kod, l, lcoul, m, meha1, meshh, n, 
     &           nurrr
      CHARACTER(4), DIMENSION(2) :: ifor1, ismod, mpot
      CHARACTER(4), DIMENSION(9) :: ifor2
      CHARACTER(4), DIMENSION(3) :: ifor3, ihmod, itext
      CHARACTER(4), DIMENSION(5) :: ifor4
      CHARACTER(4), DIMENSION(4) :: ifor5
      CHARACTER(4), DIMENSION(7) :: mmod, ipot
      INTEGER :: NINT
C
C*** End of declarations rewritten by SPAG
C
 
 
      DATA itext, mmod/'HAMI', 'LTON', 'IAN ', ' RV ', ' VM ', 'DCHM', 
     &     ' FDM', '5PA0', ' 5PM', 'CLGB'/, ipot, mpot/'POTE', 'NTIA', 
     &     'L EX', 'PAND', 'ED  ', 'BY  ', '    ', 'YL0 ', 'BET0'/, 
     &     ifor1, ifor2, ifor3/'WITH', ' AC.', 'AXIA', 'L HE', 'XADE', 
     &     'CAPO', 'LE D', 'EFOR', 'MATI', 'ONS ', '    ', '    ', 
     &     '|NON', ' NON'/, ismod, ifor4, ihmod, ifor5/'    ', ' NON', 
     &     'AXIA', 'L OC', 'TUP0', 'LE  ', '    ', 'RID.', 'SOFT', 
     &     'SOFT', '    ', 'DEFO', 'RMAT', 'IONS'/
 
      IF(MEHam.GT.1)READ(20,1300)HW, AMB0, AMG0, GAM0, BET0, BET4, BB42, 
     &                           GAMg, DELg, BET3, ETO, AMUo, HWO, BB32, 
     &                           GAMde, DPAr, GSHape
C=======================================================================
C     LLMA-MAXIMUM MOMENTUM L
C     NCMA-MAXIMUM NUMBER OF COUPLED EQ.
C     NSMA-NUMBER OF SYSTEMS WITH  J AND PARITY
C     KODMA-SWITCH: 0-COUPLED STATES ARE ORDERED ONE BY ONE, NO MORE
C                   THAN NCMA
C                 1:-COUPLED STATES ARE ORDERED BY GROWING MOMENTUM L
C                    NO MORE THAN NCMA
C=======================================================================
 
      READ(20,1010)NUR, NST, NPD, LAS, MTEt, LLMa, NCMa, NSMa, KODma
 
 1010 FORMAT(20I3)
      IF(LLMa.EQ.0.OR.LLMa.GT.89)LLMa = 89
      IF(NCMa.EQ.0.OR.NCMa.GT.200)NCMa = 200
      IF(NSMa.EQ.0.OR.NSMa.GT.180)NSMa = 180
      IF(NST.LT.0)THEN
        OPEN(99,FILE = 'OMINPUT.INP')
        READ(99,*)NST
        READ(99,*,END = 5)(EE(i),i = 1,NST)
    5   CLOSE(99)
        WRITE(*,*)NST
        DO i = 1, NST
          WRITE(*,*)EE(i)
          MCHae(i) = 0
        ENDDO
      ELSE
        READ(20,1300)(EE(i),i = 1,NST)
        READ(20,1290)(MCHae(i),i = 1,NST)
      ENDIF
      IF(MTEt.NE.0)THEN
        READ(20,1300)(TET(i),i = 1,MTEt)
      ENDIF
      IF(MEHam.GT.1)THEN
        READ(20,1320)(EL(i),JO(i),NPO(i),NTU(i),NNB(i),NNG(i),NNO(i),
     &               NCA(i),i = 1,NUR)
      ELSE
        READ(20,1310)(EL(i),JO(i),NPO(i),KO(i),NCA(i),NUMb(i),BETb(i),
     &               i = 1,NUR)
      ENDIF
C====================================================================
C     VR=VR0+VR1*EN+VR2*EN*EN      AR=AR0+AR1*EN
C===================================================================
C                WD=WD0+WD1*EN     AD=AD0+AD1*EN
C     EN<BNDC    WC=WC0+WC1*EN     AC=AC0+AC1*EN
C ====================================================================
C                WD=WD0+WD1*BNDC+(EN-BNDC)*WDA1
C     EN>BNDC    WC=WC0+WC1*BNDC+(EN-BNDC)*WCA1
C                AD=AD0+AD1+BNDC
C====================================================================
 
      READ(20,1010)NREsn
      IF(NREsn.NE.0)THEN
        READ(20,1020)(ERN(i),GNN(i),GREn(i),LON(i),JMN(i),JCOn(i),
     &               NEL(10),i = 1,NREsn)
 1020   FORMAT(3E12.6,4I3)
      ENDIF
 
      READ(20,1300)ANEu, ASP, AT, ZNUc, EFErmn, EFErmp
      READ(20,1300)VR0, VR1, VR2, VR3, VRLa, ALAvr, WD0, WD1, WDA1, 
     &             WDBw, WDWid, ALAwd, WC0, WC1, WCA1, WCBw, WCWid, 
     &             BNDc, VS, ALAso, WS0, WS1, WSBw, WSWid, RR, RRBwc, 
     &             RRWid, PDIs, AR0, AR1, RD, AD0, AD1, RC, AC0, AC1, 
     &             RW, AW0, AW1, RS, AS0, AS1, RZ, RZBwc, RZWid, AZ, 
     &             CCOul, ALF, CISo, WCIso, WDIso, EA, WDShi, WDWid2, 
     &             ALFnew, VRD, cavr, carr, caar, card, caac, ati
 
 
 
C      !!!!!!!!COMPARING WITH DATET YOU NEED TO INPUT ATI - REFERENCE NUCLEI NUMBER IN ABCT !!!!
      IF(MEPri.NE.99)PRINT 1030, ASP, AT
      WRITE(21,1030)ASP, AT
 1030 FORMAT(7X,'INTERACTION OF PARTICLE, HAVING SPIN =',F5.2/19X,
     &       'WITH NUCLEI',2X,'A=',F12.7/20X,'COUPLED CHANNELS METHOD')
      meshh = MESha - 1
      IF(MERel.EQ.0.AND.MEPri.NE.99)PRINT 1040
      IF(MERel.EQ.0)WRITE(21,1040)
 1040 FORMAT(22X,'NEWTON KINEMATICS')
      IF(MERel.EQ.1.AND.MEPri.NE.99)PRINT 1050
      IF(MERel.EQ.1)WRITE(21,1050)
 1050 FORMAT(5X,'RELATIVISTIC KINEMATICS AND POTENTIAL DEPENDENCE')
      IF(MERel.EQ.2.AND.MEPri.NE.99)PRINT 1060
      IF(MERel.EQ.2)WRITE(21,1060)
 1060 FORMAT(20X,'RELATIVISTIC KINEMATICS')
      IF(MERel.EQ.3.AND.MEPri.NE.99)PRINT 1070
      IF(MERel.EQ.3)WRITE(21,1070)
 1070 FORMAT(3X,'RELATIVISTIC KINEMATICS AND REAL POTENTIAL DEPENDENCE')
C
      IF(MEDis.EQ.0.AND.MEPri.NE.99)PRINT 1080
      IF(MEDis.EQ.0)WRITE(21,1080)
 1080 FORMAT(6X,'OPTICAL POTENTIAL WITHOUT DISPERSIVE RELATIONSHIPS')
      IF(MEDis.GE.1.AND.MEPri.NE.99)PRINT 1090
      IF(MEDis.GE.1)WRITE(21,1090)
 1090 FORMAT(6X,'OPTICAL POTENTIAL WITH THE DISPERSIVE RELATIONSHIPS')
C
      IF(MECul.EQ.0.AND.MEPri.NE.99)PRINT 1100
      IF(MECul.EQ.0)WRITE(21,1100)
 1100 FORMAT(5X,'COULOMB CORRECTION PROPORTIONAL REAL POTENTIAL DER-VE')
      IF(MECul.EQ.1.AND.MEPri.NE.99)PRINT 1110
      IF(MECul.EQ.1)WRITE(21,1110)
 1110 FORMAT(15X,' COULOMB CORRECTION IS CONSTANT')
      IF(MECul.EQ.2.AND.MEPri.NE.99)PRINT 1120
      IF(MECul.EQ.2)WRITE(21,1120)
 1120 FORMAT(/7X,' LANE CONSISTENT, EFFECTIVE PROTON ENERGY = E-CME,'/13
     &       X,'BOTH FOR REAL AND IMAGINARY POTENTIALS'/)
      IF(MECul.EQ.3.AND.MEPri.NE.99)PRINT 1130
      IF(MECul.EQ.3)WRITE(21,1130)
 1130 FORMAT(/7X,' LANE CONSISTENT, EFFECTIVE PROTON ENERGY = E-CME,'/20
     &       X,' FOR REAL POTENTIAL ONLY'/)
C
      IF(MERzz.EQ.0.AND.MEPri.NE.99)PRINT 1140
      IF(MERzz.EQ.0)WRITE(21,1140)
 1140 FORMAT(22X,'CHARGE RADIUS IS CONSTANT')
      IF(MERzz.EQ.1.AND.MEPri.NE.99)PRINT 1150
      IF(MERzz.EQ.1)WRITE(21,1150)
 1150 FORMAT(15X,' CHARGE RADIUS IS ENERGY DEPENDENT')
C
      IF(MERrr.EQ.0.AND.MEPri.NE.99)PRINT 1160
      IF(MERrr.EQ.0)WRITE(21,1160)
 1160 FORMAT(22X,'REAL RADIUS IS CONSTANT')
      IF(MERrr.EQ.1.AND.MEPri.NE.99)PRINT 1170
      IF(MERrr.EQ.1)WRITE(21,1170)
 1170 FORMAT(15X,' REAL RADIUS IS ENERGY DEPENDENT')
C
      IF(MESha.GT.1.AND.MEPri.NE.99)PRINT 1180, ifor1, ifor3(meshh), 
     &   ifor2
      IF(MESha.GT.1)WRITE(21,1180)ifor1, ifor3(meshh), ifor2
 1180 FORMAT(10X,14A4)
      IF(MEPri.NE.99)PRINT 1190, itext, mmod(MEHam), ipot, mpot(MEPot)
      WRITE(21,1190)itext, mmod(MEHam), ipot, mpot(MEPot)
 1190 FORMAT(/10X,4A4,6X,8A4)
      meha1 = MEHao + 1
      IF(MESho.GT.0.AND.MEPri.NE.99)PRINT 1200, ifor1, ismod(MESho), 
     &   ifor4, ihmod(meha1), ifor5
      IF(MESho.GT.0)WRITE(21,1200)ifor1, ismod(MESho), ifor4, 
     &                            ihmod(MEHao), ifor5
 1200 FORMAT(10X,13A4)
      IF(MEPri.NE.99)PRINT 1240, NUR, NPD, LAS
      WRITE(21,1240)NUR, NPD, LAS
      IF(MEHam.GT.1)THEN
        IF(MEPri.NE.99)PRINT 1220, 
     &                       (i,EL(i),JO(i),NTU(i),NNB(i),NNG(i),NNO(i),
     &                       NPO(i),NCA(i),i = 1,NUR)
        WRITE(21,1220)(i,EL(i),JO(i),NTU(i),NNB(i),NNG(i),NNO(i),NPO(i),
     &                NCA(i),i = 1,NUR)
        IF(MEPri.NE.99)PRINT 1210, HW, AMB0, AMG0, GAM0, BET0, BET4, 
     &                       BB42, GAMg, DELg, BET3, ETO, AMUo, HWO, 
     &                       BB32, GAMde, DPAr, GSHape
        WRITE(21,1210)HW, AMB0, AMG0, GAM0, BET0, BET4, BB42, GAMg, 
     &                DELg, BET3, ETO, AMUo, HWO, BB32, GAMde, DPAr, 
     &                GSHape
 1210   FORMAT(/22X,'PARAMETERS OF HAMILTONIAN '/5X,'HW=',F12.5,3X,
     &         'AMB0=',F8.5,3X,'AMG0=',F8.5,3X,'GAM0=',F8.5,3X,'BET0=',
     &         F8.5/5X,'BET4=',F10.5,3X,'BB42=',F8.5,3X,'GAMG=',F8.5,3X,
     &         'DELG=',F8.5/5X,'BET3=',F10.5,3X,'ETO=',F9.5,3X,'AMUO=',
     &         F8.5,3X,'HWO=',F8.5,4X,'BB32=',F8.5,3X/5X,'GAMDE=',F9.5,
     &         3X,'DPAR=',F8.4,3X,'GSHAPE=',F8.5//)
        IF(MEHam.GT.2)CALL PREQU
 
 
 1220   FORMAT(//16X,'ENERGY ',4X,'LEVEL''S SPIN*2',4X,'NTU  ',6X,
     &         'NNB  ',6X,'NNG',9X,'NNO',9X,'NPO',9X,
     &         'NCA'//(1X,I4,8X,E14.7,7I11))
 1230   FORMAT(//16X,'ENERGY',5X,'LEVEL''S SPIN*2',3X,'PARITY',10X,
     &         'BAND*2',10X,'NCA',8X,'NUMB',9X,
     &         'BETB'//(1X,I4,6X,E12.5,I11,I14,I15,I15,I11,E19.5))
 1240   FORMAT(/15X,'NUMBER OF COUPLED LEVELS=',I3,5X,'NPD =',I2/14X,
     &         'NUMBER OF TERMS IN POTENTIAL EXPANSION= ',2X,I2)
      ELSE
        IF(MEPri.NE.99)PRINT 1230, 
     &                       (i,EL(i),JO(i),NPO(i),KO(i),NCA(i),NUMb(i),
     &                       BETb(i),i = 1,NUR)
        WRITE(21,1230)(i,EL(i),JO(i),NPO(i),KO(i),NCA(i),NUMb(i),BETb(i)
     &                ,i = 1,NUR)
      ENDIF
      IF(MEPri.NE.99)PRINT 1250
      WRITE(21,1250)
 1250 FORMAT(/15X,'POTENTIAL   PARAMETERS   V(R)')
      IF(MEPri.NE.99)PRINT 1260, VR0, VR1, VR2, RR, AR0, AR1, WD0, WD1, 
     &                     VR3, RD, AD0, AD1, WC0, WC1, RC, AC0, AC1, 
     &                     RW, AW0, AW1, VS, RS, AS0, AS1, ALF, ANEu, 
     &                     RZ, AZ, BNDc, WDA1, WCA1, CCOul, CISo, WCIso, 
     &                     WS0, WS1, VRLa, ALAvr, WCBw, WCWid, WDBw, 
     &                     WDWid, ALAwd, EFErmn, EFErmp, ALAso, PDIs, 
     &                     WSBw, WSWid, RRBwc, RRWid, RZBwc, RZWid, EA, 
     &                     WDIso, WDShi, WDWid2, ALFnew, VRD, cavr, 
     &                     carr, caar, card, caac
      WRITE(21,1260)VR0, VR1, VR2, RR, AR0, AR1, WD0, WD1, VR3, RD, AD0, 
     &              AD1, WC0, WC1, RC, AC0, AC1, RW, AW0, AW1, VS, RS, 
     &              AS0, AS1, ALF, ANEu, RZ, AZ, BNDc, WDA1, WCA1, 
     &              CCOul, CISo, WCIso, WS0, WS1, VRLa, ALAvr, WCBw, 
     &              WCWid, WDBw, WDWid, ALAwd, EFErmn, EFErmp, ALAso, 
     &              PDIs, WSBw, WSWid, RRBwc, RRWid, RZBwc, RZWid, EA, 
     &              WDIso, WDShi, WDWid2, ALFnew, VRD, cavr, carr, caar, 
     &              card, caac
 
 
 1260 FORMAT(/1X,'VR0=',F7.3,5X,'VR1=',F7.4,5X,'VR2=',F10.7,2X,'RR=',
     &       F7.4,5X,'AR0=',F7.4,5X,'AR1=',F7.4/1X,'WD0=',F7.4,5X,
     &       'WD1=',F7.4,5X,'VR3=',F10.7,2X,'RD=',F7.4,5X,'AD0=',F7.4,
     &       5X,'AD1=',F7.4/1X,'WC0=',F7.4,5X,'WC1=',F7.4,21X,'RC=',
     &       F7.4,5X,'AC0=',F7.4,5X,'AC1=',F7.4/49X,'RW=',F7.4,5X,
     &       'AW0=',F7.4,5X,'AW1=',F7.4/1X,'VSO=',F7.4,37X,'RS=',F7.4,
     &       5X,'AS0=',F7.4,5X,'AS1=',F7.4/1X,'ALF=',F7.4,5X,'ANEU=',
     &       F7.4,20X,'RZ=',F7.4,5X,'AZ0=',F7.4,/1X,'BNDC=',F7.2,4X,
     &       'WDA1=',F7.4,4X,'WCA1=',F7.4,4X,'CCOUL=',F7.4,5X,'CISO=',
     &       F7.3,4X,'WCISO=',F7.3/1X,'WS0=',F7.4,5X,'WS1=',F7.4,5X,
     &       'VRLA=',F7.4,4X,'ALAVR=',F8.5,4X,'WCBW=',F7.4,4X,'WCWID=',
     &       F7.4,/1X,'WDBW=',F7.4,4X,'WDWID=',F7.4,3X,'ALAWD=',F7.4,3X,
     &       'EFERMN=',F7.3,4X,'EFERMP=',F7.3,2X,'ALASO=',F7.4,/1X,
     &       'PDIS=',F7.4,4X,'WSBW=',F7.4,4X,'WSWID=',F7.2,3X,'RRBWC=',
     &       F7.4,5X,'RRWID=',F6.2,4X,'RZBWC=',F7.4,/1X,'RZWID=',F7.4,
     &       3X,'EA=',F9.5,4X,'WDISO=',F7.3,3X,'WDSHI=',F7.2,5X,
     &       'WDWID2=',F7.2,2X,'ALFNEW=',F6.3,/1X,'VRD=',F8.3,4X,
     &       'CAVR=',F8.5,3X,'CARR=',F9.6,2X,'CAAR=',F9.6,4X,'CARD=',
     &       F9.6,2X,'CAAC=',F9.6/)
 
      IF(MEPri.NE.99)PRINT 1270, ZNUc, ati
      WRITE(21,1270)ZNUc, ati
 1270 FORMAT(/10X,'NUCLEUS CHARGE = ',F7.4,5x,'NUCLEUS   MASS = ',F7.4/)
      IF(MEHam.LE.1)THEN
        IF(NPD.NE.0)THEN
          READ(20,1300)(BET(i),i = 2,NPD,2)
          IF(MEPri.NE.99)PRINT 1280, (i,BET(i),i = 2,NPD,2)
          WRITE(21,1280)(i,BET(i),i = 2,NPD,2)
 1280     FORMAT(6X,'NPD',5X,'DEFORMATION PARAMETER VALUES'/(6X,I2,13X,
     &           F7.4))
 1290     FORMAT(36I2)
 1300     FORMAT(6E12.7)
 1310     FORMAT(E12.7,5I2,E12.7)
 1320     FORMAT(E12.7,7I2)
        ENDIF
      ENDIF
      asq = AT**(1./3.)
 
 
      VRLa = VRLa + cavr*(AT - ati)
      RR = (RR + carr*(AT - ati))*asq
      RC = RC*asq
      RD = (RD + card*(AT - ati))*asq
      RW = RW*asq
      RS = RS*asq
      RZ = RZ*asq
      AR0 = AR0 + caar*(AT - ati)
      AC0 = AC0 + caac*(AT - ati)
      kod = KODma
      nurrr = NUR
 
 
      DO ii = 1, NST
        IF(ii.NE.1)THEN
          IF(MERip.EQ.1)THEN
            READ(20,*)
            READ(20,1300)VR0, VR1, VR2, VR3, VRLa, ALAvr, WD0, WD1, 
     &                   WDA1, WDBw, WDWid, ALAwd, WC0, WC1, WCA1, WCBw, 
     &                   WCWid, BNDc, VS, ALAso, WS0, WS1, WSBw, WSWid, 
     &                   RR, RRBwc, RRWid, PDIs, AR0, AR1, RD, AD0, AD1, 
     &                   RC, AC0, AC1, RW, AW0, AW1, RS, AS0, AS1, RZ, 
     &                   RZBwc, RZWid, AZ, CCOul, ALF, CISo, WCIso, 
     &                   WDIso, EA, WDShi, WDWid2, ALFnew, VRD, cavr, 
     &                   carr, caar, card, caac
            WRITE(21,1260)VR0, VR1, VR2, RR, AR0, AR1, WD0, WD1, VR3, 
     &                    RD, AD0, AD1, WC0, WC1, RC, AC0, AC1, RW, AW0, 
     &                    AW1, VS, RS, AS0, AS1, ALF, ANEu, RZ, AZ, 
     &                    BNDc, WDA1, WCA1, CCOul, CISo, WCIso, WS0, 
     &                    WS1, VRLa, ALAvr, WCBw, WCWid, WDBw, WDWid, 
     &                    ALAwd, EFErmn, EFErmp, ALAso, PDIs, WSBw, 
     &                    WSWid, RRBwc, RRWid, RZBwc, RZWid, EA, WDIso, 
     &                    WDShi, WDWid2, ALFnew, VRD, cavr, carr, caar, 
     &                    card, caac
 
            asq = AT**(1./3.)
            RR = RR*asq
            RC = RC*asq
            RD = RD*asq
            RW = RW*asq
            RS = RS*asq
            RZ = RZ*asq
          ENDIF
        ENDIF
 
 
        EN = EE(ii)
 
C     CREATING LEVELS FOR (P,N) ANALOG STATES CALCULATIONS
 
        IF(EE(ii).GE.EL(nurrr)*(AT + 1.007825032D0)/AT + 0.5D0)THEN
          IF(MEHam.GE.1.AND.MCHae(ii).EQ.1)NUR = nurrr
          IF(MEHam.GE.1.AND.MCHae(ii).EQ.1)GOTO 10
        ENDIF
        DO ilev = 1, nurrr
          IF(NCA(ilev).EQ.NCA(1))NUR = ilev
        ENDDO
 
 
 
 
   10   MECha = MCHae(ii)
        ANEu = 1.008664924
        IF(MECha.EQ.1)ANEu = 1.007825032
        ami = 939.56536
        IF(MECha.EQ.1)ami = 938.272029
        rel = (EN + ami)/ami
        IF(MERel.EQ.0)rel = 1.
        enc = EN*AT/(AT + ANEu*rel)
        DO i1 = 1, NUR
          IF(enc.LE.EL(i1))GOTO 15
        ENDDO
        NMAx = NUR
        GOTO 20
   15   NMAx = i1 - 1
   20   KODma = kod
        IF(NMAx.LT.NUR)KODma = 0
        CALL RIPAT
        CALL ASFUT
        IF(MEHam.GT.1)CALL KNCOE
        CALL QUANT
        IF(MTEt.NE.0)THEN
          NUI = 1
          NUF = NMAx
          CALL DISCA
          IF(MEPri.NE.99)PRINT 1330
          WRITE(21,1330)
 1330     FORMAT(/23X,'ANGULAR DISTRIBUTIONS OF SCATTERED PARTICLES'/
     &           '    ANGLES[deg]      gs          1st       ...      ')
          IF(MEPri.NE.99)WRITE(27,'(F10.6,2I3)')EN, MTEt, NMAx
          DO m = 1, MTEt
            IF(MEPri.NE.99)PRINT 1350, TET(m), (DISc(k,m),k = 1,NMAx)
            WRITE(21,1350)TET(m), (DISc(k,m),k = 1,NMAx)
            IF(MEPri.NE.99)WRITE(27,1340)TET(m), (DISc(k,m),k = 1,NMAx)
 1340       FORMAT(E13.6,(8E13.6),(12E13.6))
          ENDDO
 1350     FORMAT(2X,F10.3,3X,(8E11.3),(12E11.3))
          DO l = 1, 40
            IF(COEfr(l).EQ.0.AND.COEfi(l).EQ.0.)lcoul = l - 1
            IF(COEfr(l).EQ.0.AND.COEfi(l).EQ.0.)EXIT
          ENDDO
          IF(MEPri.NE.99)THEN
            PRINT 1360, EN, LKK, lcoul
            WRITE(26,1360)EN, LKK, lcoul
            WRITE(26,1390)EN, CST, CSR, (CSN(k),k = 1,NMAx)
 1360       FORMAT(/3X,'EN=',F9.3,3X,'LKK=',I3,3X,'LCOUL=',I3)
            DO n = 1, NMAx
              PRINT 1370, (COEf(n,l),l = 1,LKK)
              WRITE(26,1370)(COEf(n,l),l = 1,LKK)
              IF(ETA.NE.0.AND.n.LE.1)THEN
                PRINT 1380, (COEfr(l),COEfi(l),l = 1,lcoul)
                WRITE(26,1380)(COEfr(l),COEfi(l),l = 1,lcoul)
              ENDIF
            ENDDO
 1370       FORMAT(/14X,'LEGENDRE COEFFICIENTS FOR  SCATTERED NUCLEONS'/
     &             16X,'ANGULAR DISTRIBUTIONS - NUCLEAR AMPLITUDE'/(1X,
     &             6E15.7))
 1380       FORMAT(/14X,'LEGENDRE COEFFICIENTS FOR  SCATTERED PROTONS'/
     &             16X,'ANGULAR DISTRIBUTIONS - COULOMB AMPLITUDE'/(1X,
     &             6E15.7))
          ENDIF
        ENDIF
        IF(MECha.NE.0)THEN
 
          IF(MEPri.NE.99)PRINT 1500, EN, CST, CSN(1), CSR, CST - CSN(1)
          WRITE(21,1500)EN, CST, CSN(1), CSR, CST - CSN(1)
 
          IF(MEPri.NE.99)WRITE(25,1410)EN
        ELSE
C RCN
          IF(EN.LT.2.D0)THEN
            IF(MEPri.NE.99)PRINT 1480, EN, CST, CSN(1), CSR, 
     &                           CST - CSN(1), 
     &                           SQRT((CST - CSR)/0.125663706D0)
            WRITE(21,1480)EN, CST, CSN(1), CSR, CST - CSN(1), 
     &                    SQRT((CST - CSR)/0.125663706D0)
          ELSE
            IF(MEPri.NE.99)PRINT 1490, EN, CST, CSN(1), CSR, 
     &                           CST - CSN(1)
            WRITE(21,1490)EN, CST, CSN(1), CSR, CST - CSN(1)
          ENDIF
C RCN
          IF(MEPri.NE.99)WRITE(25,1400)EN
        ENDIF
        IF(MEPri.NE.99)WRITE(23,1390)EN, CST, CSN(1), CST - CSN(1), CSR, 
     &                               (CSN(k),k = 2,nurrr)
 1390   FORMAT(1P24E14.5)
        IF(MEPri.NE.99)THEN
          DO m = 1, MTEt
            PRINT 1350, TET(m), (DISc(k,m),k = 1,NMAx)
            WRITE(25,1350)TET(m), (DISc(k,m),k = 1,NMAx)
          ENDDO
        ENDIF
 1400   FORMAT(///1X,'NEUTRON ENERGY =',F10.6)
 1410   FORMAT(///1X,'PROTON  ENERGY =',F10.6)
C     IF(MEPRI.NE.99) PRINT 130,(K,CSN(K),K=1,NMAX)
C     WRITE(21,130)(K,CSN(K),K=1,NMAX)
        IF(MEPri.NE.99)PRINT 1510, 
     &                       (k,EL(k),0.5*JO(k),CPAR(NPO(k)),CSN(k),
     &                       k = 1,NMAx)
        WRITE(21,1510)(k,EL(k),0.5*JO(k),CPAR(NPO(k)),CSN(k),k = 1,NMAx)
        IF(MEPri.NE.99)PRINT 1520, SF0, SF1, SF2
        WRITE(21,1520)SF0, SF1, SF2
C RCN
C
C     CROSS SECTION FILES
C
 
        IF(MEPri.EQ.99)THEN
          OPEN(UNIT = 93,FILE = TRIM(FNAme)//'.CS')
          OPEN(UNIT = 98,FILE = TRIM(FNAme)//'.ICS')
        ENDIF
 
        IF(ETA.EQ.0.)THEN
C       WRITE(93,'(10H<CROSS-S.>,F10.2,F10.5,F10.2,2I5)')
          WRITE(93,1420)ANEu, EN, AT, NINT(0.5*JO(1)), 3
        ELSE
C       WRITE(93,'(10H<CROSS-S.>,F10.2,F10.5,F10.2,2I5)')
          WRITE(93,1420)ANEu, EN, AT, NINT(0.5*JO(1)), 1
        ENDIF
 1420   FORMAT('<CROSS-S.>',F10.2,1P,D20.8,0P,F10.2,2I5)
 1430   FORMAT('<INE.C.S.>',F10.2,1P,D20.8,0P,F10.2,2I5)
C     WRITE(98,'(10H<INE.C.S.>,F10.2,F10.5,F10.2,2I5)')
        WRITE(98,1430)ANEu, EN, AT, NINT(0.5*JO(1)), NMAx - 1
 
        IF(ETA.EQ.0.)THEN
C
C       TOTAL
C       WRITE(93,'(1X,E14.8)') CST*1000.
          WRITE(93,1440)CST*1000.D0
 1440     FORMAT(1P,D12.5)
C
C       INELASTIC TO LEVELS
          DO k = 2, NMAx
C         WRITE(98,'(1X,E14.8)') CSN(K)*1000.
            WRITE(98,1440)CSN(k)*1000.D0
          ENDDO
C
C       REACTION + INELASTIC TO LEVELS
C       WRITE(93,'(1X,E14.8)') (CST - CSN(1))*1000.
          WRITE(93,1440)(CST - CSN(1))*1000.D0
C
C       ELASTIC
C         WRITE(93,'(1X,E14.8)') CSN(1)*1000.
          WRITE(93,1440)CSN(1)*1000.D0
 
        ELSE
C
C       INELASTIC TO LEVELS
          DO k = 2, NMAx
C         WRITE(98,'(1X,E14.8)') CSN(K)*1000.
            WRITE(98,1440)CSN(k)*1000.D0
          ENDDO
C
C       REACTION + INELASTIC TO LEVELS
C       WRITE(93,'(1X,E14.8)') (CST - CSN(1))*1000.
          WRITE(93,1440)(CST - CSN(1))*1000.D0
 
        ENDIF
 
        IF(MEPri.EQ.99)THEN
          CLOSE(93)
          CLOSE(98)
C
          OPEN(UNIT = 96,FILE = TRIM(FNAme)//'.LEG')
          OPEN(UNIT = 97,FILE = TRIM(FNAme)//'.ANG')
C
C       IF(ETA.EQ.0.) WRITE(96,'(10H<LEGENDRE>,F10.2,F10.5,F10.2,2I5)')
          IF(ETA.EQ.0.)WRITE(96,1450)ANEu, EN, AT, NINT(0.5*JO(1)), NMAx
 1450     FORMAT('<LEGENDRE>',F10.2,1P,D20.8,0P,F10.2,2I5)
C       WRITE(97,'(10H<ANG.DIS.>,F10.2,F10.5,F10.2,2I5)')
          WRITE(97,1460)ANEu, EN, AT, NINT(0.5*JO(1)), NMAx
 1460     FORMAT('<ANG.DIS.>',F10.2,1P,D20.8,0P,F10.2,2I5)
          DO k = 1, NMAx
            IF(ETA.EQ.0.)WRITE(96,
     &                      '(2I5,'' COUPLED LEVEL, NUMBER OF VALUES'')'
     &                      )k, LKK
            IF(NPO(k).EQ. + 1)WRITE(97,'(I5,F5.1,A1,I4,I5)')k, 0.5*JO(k)
     &                              , '+', 1, MTEt
C                                                          Cross section printed
C    *      WRITE(97,'(I5,F5.1,A1,I4,I5)') K,0.5*JO(K),'+',0,MTET
            IF(NPO(k).EQ. - 1)WRITE(97,'(I5,F5.1,A1,I4,I5)')k, 0.5*JO(k)
     &                              , '-', 1, MTEt
C                                                          Cross section printed
C    *      WRITE(97,'(I5,F5.1,A1,I4,I5)') K,0.5*JO(K),'-',0,MTET
            IF(ETA.EQ.0.)THEN
              DO l = 1, LKK
C             WRITE(96,'(2I5,1P,D20.10)') K,L-1,COEF(K,L)
C                                               OUTPUT IN MB
                WRITE(96,'(2I5,1P,D20.10)')k, l - 1, 1000.D0*COEf(k,l)
              ENDDO
            ENDIF
            DO m = 1, MTEt
              WRITE(97,1470)0, TET(m), DISc(k,m)*1000.D0, 
     &                      'CROSS-SECTION   '
C           WRITE(97,1038)   TET(M),DISC(K,M)*1000.d0,'CROSS-SECTION   '
C1038       FORMAT (I3,1P,2D12.5,5X,4A4,A2)                             RESU-648
 1470         FORMAT(I3,1P,2D12.5,5X,A16)
            ENDDO
          ENDDO
          CLOSE(96)
          CLOSE(97)
        ENDIF
 
 1480   FORMAT(/1X,'NEUTRON ENERGY =',F10.6/1X,'TOTAL  CR-SECT.=',
     &         F10.6/1X,'ELASTIC  CR-SECT. =',F10.6/1X,
     &         'REACTION CR-SECT. =',F10.6/1X,
     &         'REACTION CR-SECT. incl. coupled levels =',F10.6/1X,
     &         'SCATTERING RADIUS =',F10.6)
 1490   FORMAT(/1X,'NEUTRON ENERGY =',F10.6/1X,'TOTAL  CR-SECT.=',
     &         F10.6/1X,'ELASTIC  CR-SECT. =',F10.6/1X,
     &         'REACTION CR-SECT. =',F10.6/1X,
     &         'REACTION CR-SECT. incl. coupled levels =',F10.6)
C RCN
 1500   FORMAT(/1X,'PROTON  ENERGY =',F10.6/1X,'TOTAL  CR-SECT.=',
     &         F10.6/1X,'REACTION CR-SECT. =',F10.6/1X,
     &         'REACTION CR-SECT. incl. coupled levels =',F10.6)
C 130 FORMAT(/3X,'Nlev',17X,'CR-SECT. OF LEVEL EXCITATION '
C    */(1X,I5,25X,F10.6))
 1510   FORMAT(/2x,'Nlev',4X,'Elev',3x,'Jpi',9x,
     &         'CR-SECT(Nlev)'/(2X,I2,3X,F7.4,2x,F4.1,A1,10X,F10.6))
 1520   FORMAT(/30X,'STRENGTH  FUNCTIONS'/1X,'SF0=',E15.7,8X,'SF1=',
     &         E15.7,8X,'SF2=',E15.7/)
        NUR = nurrr
        CALL THORA(21)
      ENDDO
      RETURN
      END SUBROUTINE ABCT
 
!---------------------------------------------------------------------------
C     *******************************************************
      SUBROUTINE DATET
C     *******************************************************
      IMPLICIT DOUBLE PRECISION(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: AC, AC0, AC1, ACG, AD, AD0, AD1, ALAso, ALAvr, ALAwd, 
     &          ALF, ALFnew, AMB0, AMG0, AMO, AMUo, ANEu, AR, AR0, AR1, 
     &          ARG, AS, AS0, AS1, ASP, AT, AW, AW0, AW1, AZ, BB32, 
     &          BB42, BET0, BET3, BET4, BMO, BNDc, C, CAAc, CAAr, CARd, 
     &          CARr, CAVr, CCOul, CISo, CMO, CSR, CST, DELg, DPAr, EA, 
     &          EFErmn, EFErmp, EN, EPS1, ETO, FM, FU, GAM0, GAMde, 
     &          GAMg, GSHape, HW, HWO, PDIs, RC, RCG, RD, RDG, RR, 
     &          RRBwc, RRG, RRWid, RS, RSG, RW, RWG, RZ, RZBwc, RZG
      REAL*8, DIMENSION(10) :: AMB0is, AMG0is, AMUois, ATIs, BB32is, 
     &                         BB42is, BET, BET0is, BET3is, BET4is, 
     &                         DELgis, DPAris, EFIsn, EFIsp, ETOis, 
     &                         GAM0is, GAMdis, GAMgis, GSHaeis, HWIs, 
     &                         HWOis, WEI, ZNUcis
      REAL*8, DIMENSION(20) :: BETb, EL, ES, WN, WNK
      REAL*8, DIMENSION(10,20) :: BETbis, ELIs
      REAL*8, DIMENSION(10,10) :: BETis, ERIs, GNIs, GREis
      REAL*8, DIMENSION(10,50) :: CSNat, DCSnat, DRAt, DS1, DS2, DSR, 
     &                            DST, EEIs, RATio, SE1, SE2, SRE, STE
      REAL*8, DIMENSION(150) :: DISg
      REAL*8, DIMENSION(10,50,5,150) :: DSD, SNGd, TED
      REAL*8, DIMENSION(10,50,5) :: DSN, SNE
      REAL*8, DIMENSION(50) :: EE
      REAL*8, DIMENSION(25) :: EP, EPSgr, GR, GRR, X, X1, X2
      INTEGER, DIMENSION(10,10) :: JCOis, JMIs, LOIs, NELa
      INTEGER, DIMENSION(20) :: JJ, JO, KO, NCA, NNB, NNG, NNO, NPI, 
     &                          NPO, NTU, NUMb
      INTEGER, DIMENSION(10,20) :: JOIs, KOIs, NCAis, NNBis, NNGis, 
     &                             NNOis, NPOis, NTUis, NUMbis
      INTEGER :: KODma, LAS, LLMa, MEApp, MEBet, MECha, MECul, MEDis, 
     &           MEHam, MEHao, MEIis, MEJob, MELev, MENuc, MEPot, MEPri, 
     &           MERel, MERes, MERip, MERrr, MERzz, MESha, MESho, MESol, 
     &           MEVol, NCMa, NMAx, NPD, NRL, NSMa, NST, NUF, NUI, NUR, 
     &           NV
      INTEGER, DIMENSION(50) :: MCHae
      INTEGER, DIMENSION(10,50) :: MCHais, NGD, NGN, NNAt, NR, NRAt, 
     &                             NSF1, NSF2, NT
      INTEGER, DIMENSION(10) :: MESois, NREs, NSTis, NURis
      INTEGER, DIMENSION(10,50,5) :: MTD, NFD, NFN, NID, NIN
      INTEGER, DIMENSION(73) :: NPJ
      REAL*8 :: RZWid, VD, VR, VR0, VR1, VR2, VR3, VRD, VRDc, VRG, VRLa, 
     &          VS, WC, WC0, WC1, WCA1, WCBw, WCIso, WCWid, WD, WD0, 
     &          WD1, WDA1, WDBw, WDIso, WDShi, WDWid, WDWid2, WS0, WS1, 
     &          WSBw, WSWid, ZNUc
      COMMON /CSB   / CST, CSR, NST
      COMMON /DISPE / VD, VRDc, EA, WDIso
      COMMON /DISPE2/ VRD, WDShi, WDWid2, ALFnew
      COMMON /ENA   / EN, EL, BET, NUR, NMAx, NPD, LAS
      COMMON /ENB   / EE, MCHae
      COMMON /EXPE1 / SNE, DSN, NGD, NGN, NIN, NFN
      COMMON /EXPE2 / DISg, TED, SNGd, DSD, NPJ, MTD, NID, NFD
      COMMON /EXPE3 / SE1, SE2, DS1, DS2, NSF1, NSF2
      COMMON /EXPER / STE, SRE, DST, DSR, NT, NR, NRAt, NNAt
      COMMON /GLOBI / RRG, RCG, RDG, RWG, RSG, RZG, VRG, ARG, ACG
      COMMON /GLOBIN/ NSTis, MCHais, JOIs, NPOis, KOIs, NCAis, NTUis, 
     &                NNBis, NNGis, NNOis, NURis, MESois, NUMbis, 
     &                BETbis, MENuc, MEBet, MEIis, MERes, MELev
      COMMON /GLOBRE/ EEIs, ELIs, ATIs, ZNUcis, EFIsn, EFIsp, BETis, 
     &                CAVr, CARr, CAAr, CARd, CAAc, RATio, DRAt, CSNat, 
     &                DCSnat, WEI
      COMMON /GLOHAM/ HWIs, AMB0is, AMG0is, GAM0is, BET0is, BET4is, 
     &                BB42is, GAMgis, DELgis, BET3is, ETOis, AMUois, 
     &                HWOis, BB32is, GAMdis, DPAris, GSHaeis
      COMMON /INRM  / AMO, BMO, CMO, BB42, GAMg, DELg
      COMMON /MENU  / MEJob, MEPot, MEHam, MECha, MEPri, MESol, MESha, 
     &                MESho, MEHao, MEApp, MEVol, MERel, MECul, MERzz, 
     &                MERrr, MEDis, MERip
      COMMON /NCLMA / LLMa, NCMa, NSMa, KODma
      COMMON /NU    / NUI, NUF
      COMMON /OPB   / C, GRR, FM, EPS1, NRL
      COMMON /OPT   / X, FU, GR, X1, X2, EP, EPSgr, NV
      COMMON /POT1  / VR3, VR2, VR1, VR0, WC1, WC0, WD1, WD0, VS, AC0, 
     &                AR0, AW0, AD0, AS0
      COMMON /POT2  / AR1, AC1, AW1, AD1, AS1
      COMMON /POT3  / BNDc, WDA1, WCA1, CCOul, CISo, WCIso, WS0, WS1
      COMMON /POTB  / WNK, WN, VR, WC, WD
      COMMON /POTD  / ALAvr, VRLa, WCBw, WCWid, WDBw, WDWid, ALAwd, 
     &                EFErmn, EFErmp, ALAso, PDIs, WSBw, WSWid, RRBwc, 
     &                RRWid, RZBwc, RZWid
      COMMON /QNB   / JO, NPO, KO, NCA
      COMMON /QNBBAND/ NUMb, BETb
      COMMON /RAD   / RR, RC, RD, RW, RS, AR, AC, AW, AD, AS, ALF, AT, 
     &                ANEu, RZ, ZNUc, ASP, AZ
      COMMON /RESON / ERIs, GNIs, GREis, NREs, LOIs, JMIs, JCOis, NELa
      COMMON /SHAMO / BET3, ETO, AMUo, HWO, BB32, DPAr
      COMMON /SHEM1 / HW, AMG0, AMB0, GAM0, BET0, BET4, GAMde, GSHape
      COMMON /SHEMM / ES, JJ, NTU, NNB, NNG, NNO, NPI
C
C Local variables
C
      INTEGER :: i, iis, k, kev, l, m, meha1, meshh, mtet, ng, nresn
      CHARACTER(4), DIMENSION(2) :: ifor1, ismod, mpot
      CHARACTER(4), DIMENSION(9) :: ifor2
      CHARACTER(4), DIMENSION(3) :: ifor3, ihmod, itext
      CHARACTER(4), DIMENSION(5) :: ifor4
      CHARACTER(4), DIMENSION(4) :: ifor5
      CHARACTER(4), DIMENSION(7) :: mmod, ipot
C
C*** End of declarations rewritten by SPAG
C
      DATA itext, mmod/'HAMI', 'LTON', 'IAN ', ' RV ', ' VM ', 'DCHM', 
     &     ' FDM', '5PA0', ' 5PM', 'CLGB'/, ipot, mpot/'POTE', 'NTIA', 
     &     'L EX', 'PAND', 'ED B', 'Y   ', '    ', 'YL0 ', 'BET0'/, 
     &     ifor1, ifor2, ifor3/'WITH', ' AC.', 'AXIA', 'L HE', 'XADE', 
     &     'CAPO', 'LE D', 'EFOR', 'MATI', 'ONS ', '    ', '    ', 
     &     '|NON', ' NON'/, ismod, ifor4, ihmod, ifor5/'    ', ' NON', 
     &     'AXIA', 'L OC', 'TUPO', 'LE  ', '    ', 'RID.', 'SOFT', 
     &     'SOFT', '    ', 'DEFO', 'RMAT', 'IONS'/
      READ(20,1010)MENuc, MEBet, MEIis, MERes, MELev
C=======================================================================
C
C     MENUC-NUMBER OF ADJUSTED ISOTOPES
C     MEBET-NUMBER OF ISOTOPE DEFORMATIONS OF WHICH ARE ADJUSTED
C     MEIIS-NUMBER OF ISOTOPE FOR WHICH RESONSNCES ARE TO BE ADJUSTED
C     MERES-NUMBER OF RESONANCE FOR A CHOZEN ISOTOPE THAT IS TO BE ADJUSTED
C     MELEV-NUMBER OF LEVEL OF ISOTOPE WITH NUMBER MEBET DEFORMATION FOR WHICH IS TO BE ADJUSTED
C     LLMA-MAXIMUM MOMENTUM L
C     NCMA-MAXIMUM NUMBER OF COUPLED EQ.
C     NSMA-NUMBER OF SYSTEMS WITH  J AND PARITY
C     KODMA-SWITCH: 0-COUPLED STATES ARE ORDERED ONE BY ONE, NO MORE
C                   THAN NCMA
C                 1:-COUPLED STATES ARE ORDERED BY GROWING MOMENTUM L
C                    NO MORE THAN NCMA
C=======================================================================
      READ(20,1010)NUR, NST, NPD, LAS, mtet, LLMa, NCMa, NSMa, KODma
 1010 FORMAT(20I3)
      IF(LLMa.EQ.0.OR.LLMa.GT.89)LLMa = 89
      IF(NCMa.EQ.0.OR.NCMa.GT.200)NCMa = 200
      IF(NSMa.EQ.0.OR.NSMa.GT.180)NSMa = 180
      READ(20,1010)(NSTis(i),NURis(i),MESois(i),NREs(i),i = 1,MENuc)
      READ(20,1300)(WEI(i),i = 1,MENuc)
      DO iis = 1, MENuc
        NUR = NURis(iis)
        NST = NSTis(iis)
        nresn = NREs(iis)
 
        IF(MEHam.GT.1)READ(20,1300)HWIs(iis), AMB0is(iis), AMG0is(iis), 
     &                             GAM0is(iis), BET0is(iis), BET4is(iis)
     &                             , BB42is(iis), GAMgis(iis), 
     &                             DELgis(iis), BET3is(iis), ETOis(iis), 
     &                             AMUois(iis), HWOis(iis), BB32is(iis), 
     &                             GAMdis(iis), DPAris(iis), 
     &                             GSHaeis(iis)
 
        READ(20,1300)(EEIs(iis,i),i = 1,NST)
        READ(20,1290)(MCHais(iis,i),i = 1,NST)
 
        IF(MEHam.GT.1)THEN
          READ(20,1320)(ELIs(iis,i),JOIs(iis,i),NPOis(iis,i),
     &                 NTUis(iis,i),NNBis(iis,i),NNGis(iis,i),
     &                 NNOis(iis,i),NCAis(iis,i),i = 1,NUR)
        ELSE
          READ(20,1310)(ELIs(iis,i),JOIs(iis,i),NPOis(iis,i),KOIs(iis,i)
     &                 ,NCAis(iis,i),NUMbis(iis,i),BETbis(iis,i),i = 1,
     &                 NUR)
        ENDIF
C====================================================================
C     VR=VR0+VR1*EN+VR2*EN*EN      AR=AR0+AR1*EN
C===================================================================
C                WD=WD0+WD1*EN     AD=AD0+AD1*EN
C     EN<BNDC    WC=WC0+WC1*EN     AC=AC0+AC1*EN
C ====================================================================
C                WD=WD0+WD1*BNDC+(EN-BNDC)*WDA1
C     EN>BNDC    WC=WC0+WC1*BNDC+(EN-BNDC)*WCA1
C                AD=AD0+AD1+BNDC
C====================================================================
        IF(nresn.NE.0)THEN
          READ(20,1020)(ERIs(iis,i),GNIs(iis,i),GREis(iis,i),LOIs(iis,i)
     &                 ,JMIs(iis,i),JCOis(iis,i),NELa(iis,i),i = 1,
     &                 nresn)
 
 
 1020     FORMAT(3E12.6,4I3)
        ENDIF
        READ(20,1300)ANEu, ASP, ATIs(iis), ZNUcis(iis), EFIsn(iis), 
     &               EFIsp(iis)
 
 
      ENDDO
 
 
      READ(20,1300)VR0, VR1, VR2, VR3, VRLa, ALAvr, WD0, WD1, WDA1, 
     &             WDBw, WDWid, ALAwd, WC0, WC1, WCA1, WCBw, WCWid, 
     &             BNDc, VS, ALAso, WS0, WS1, WSBw, WSWid, RR, RRBwc, 
     &             RRWid, PDIs, AR0, AR1, RD, AD0, AD1, RC, AC0, AC1, 
     &             RW, AW0, AW1, RS, AS0, AS1, RZ, RZBwc, RZWid, AZ, 
     &             CCOul, ALF, CISo, WCIso, WDIso, EA, WDShi, WDWid2, 
     &             ALFnew, VRD, CAVr, CARr, CAAr, CARd, CAAc
      DO i = 1, MENuc
        PRINT 1030, ASP, ATIs(i)
        WRITE(21,1030)ASP, ATIs(i)
 1030   FORMAT(7X,'INTERACTION OF PARTICLE HAVING SPIN =',F5.2/19X,
     &         'WITH NUCLEI',2X,'A=',F12.7/20X,
     &         'COUPLED CHANNELS METHOD')
      ENDDO
      meshh = MESha - 1
      IF(MERel.EQ.0)PRINT 1040
      IF(MERel.EQ.0)WRITE(21,1040)
 1040 FORMAT(22X,'NEWTON KINEMATICS')
      IF(MERel.EQ.1)PRINT 1050
      IF(MERel.EQ.1)WRITE(21,1050)
 1050 FORMAT(5X,'RELATIVISTIC KINEMATICS AND POTENTIAL DEPENDENCE')
      IF(MERel.EQ.2)PRINT 1060
      IF(MERel.EQ.2)WRITE(21,1060)
 1060 FORMAT(20X,'RELATIVISTIC KINEMATICS')
      IF(MERel.EQ.3)PRINT 1070
      IF(MERel.EQ.3)WRITE(21,1070)
 1070 FORMAT(3X,'RELATIVISTIC KINEMATICS AND REAL POTENTIAL DEPENDENCE')
C
      IF(MEDis.EQ.0)PRINT 1080
      IF(MEDis.EQ.0)WRITE(21,1080)
 1080 FORMAT(6X,'OPTICAL POTENTIAL WITHOUT DISPERSIVE RELATIONSHIPS')
      IF(MEDis.GE.1)PRINT 1090
      IF(MEDis.GE.1)WRITE(21,1090)
 1090 FORMAT(6X,'OPTICAL POTENTIAL WITH THE DISPERSIVE RELATIONSHIPS')
C
      IF(MECul.EQ.0)PRINT 1100
      IF(MECul.EQ.0)WRITE(21,1100)
 1100 FORMAT(5X,'COULOMB CORRECTION PROPORTIONAL REAL POTENTIAL DER-VE')
      IF(MECul.EQ.1)PRINT 1110
      IF(MECul.EQ.1)WRITE(21,1110)
 1110 FORMAT(15X,' COULOMB CORRECTION IS CONSTANT')
      IF(MECul.EQ.2)PRINT 1120
      IF(MECul.EQ.2)WRITE(21,1120)
 1120 FORMAT(/7X,' LANE CONSISTENT, EFFECTIVE PROTON ENERGY = E-CME,'/13
     &       X,'BOTH FOR REAL AND IMAGINARY POTENTIALS'/)
      IF(MECul.EQ.3)PRINT 1130
      IF(MECul.EQ.3)WRITE(21,1130)
 1130 FORMAT(/7X,' LANE CONSISTENT, EFFECTIVE PROTON ENERGY = E-CME,'/20
     &       X,' FOR REAL POTENTIAL ONLY'/)
C
      IF(MERzz.EQ.0)PRINT 1140
      IF(MERzz.EQ.0)WRITE(21,1140)
 1140 FORMAT(22X,'CHARGE RADIUS IS CONSTANT')
      IF(MERzz.EQ.1)PRINT 1150
      IF(MERzz.EQ.1)WRITE(21,1150)
 1150 FORMAT(15X,' CHARGE RADIUS IS ENERGY DEPENDENT')
 
C
      IF(MERrr.EQ.0)PRINT 1160
      IF(MERrr.EQ.0)WRITE(21,1160)
 1160 FORMAT(22X,'REAL RADIUS IS CONSTANT')
      IF(MERrr.EQ.1)PRINT 1170
      IF(MERrr.EQ.1)WRITE(21,1170)
 1170 FORMAT(15X,' REAL RADIUS IS ENERGY DEPENDENT')
C
      IF(MESha.GT.1)PRINT 1180, ifor1, ifor3(meshh), ifor2
      IF(MESha.GT.1)WRITE(21,1180)ifor1, ifor3(meshh), ifor2
 1180 FORMAT(10X,14A4)
      PRINT 1190, itext, mmod(MEHam), ipot, mpot(MEPot)
      WRITE(21,1190)itext, mmod(MEHam), ipot, mpot(MEPot)
 1190 FORMAT(/10X,4A4,6X,8A4)
      meha1 = MEHao + 1
      IF(MESho.GT.0)PRINT 1200, ifor1, ismod(MESho), ifor4, ihmod(meha1)
     &                    , ifor5
      IF(MESho.GT.0)WRITE(21,1200)ifor1, ismod(MESho), ifor4, 
     &                            ihmod(MEHao), ifor5
 1200 FORMAT(10X,13A4)
      DO iis = 1, MENuc
        NUR = NURis(iis)
        NST = NSTis(iis)
        MESol = MESois(iis)
 
        IF(MEHam.GT.1)THEN
          DO i = 1, NUR
            EL(i) = ELIs(iis,i)
            JO(i) = JOIs(iis,i)
            NTU(i) = NTUis(iis,i)
            NNB(i) = NNBis(iis,i)
            NNG(i) = NNGis(iis,i)
            NNO(i) = NNOis(iis,i)
            NPO(i) = NPOis(iis,i)
            NCA(i) = NCAis(iis,i)
            NUMb(i) = NUMbis(iis,i)
            BETb(i) = BETbis(iis,i)
          ENDDO
          HW = HWIs(iis)
          AMB0 = AMB0is(iis)
          AMG0 = AMG0is(iis)
          GAM0 = GAM0is(iis)
          BET0 = BET0is(iis)
          BET4 = BET4is(iis)
          BB42 = BB42is(iis)
          GAMg = GAMgis(iis)
          DELg = DELgis(iis)
          BET3 = BET3is(iis)
          ETO = ETOis(iis)
          AMUo = AMUois(iis)
          HWO = HWOis(iis)
          BB32 = BB32is(iis)
          GAMde = GAMdis(iis)
          DPAr = DPAris(iis)
          PRINT 1220, (i,EL(i),JO(i),NTU(i),NNB(i),NNG(i),NNO(i),NPO(i),
     &          NCA(i),i = 1,NUR)
          WRITE(21,1220)(i,EL(i),JO(i),NTU(i),NNB(i),NNG(i),NNO(i),
     &                  NPO(i),NCA(i),i = 1,NUR)
          PRINT 1210, HW, AMB0, AMG0, GAM0, BET0, BET4, BB42, GAMg, 
     &          DELg, BET3, ETO, AMUo, HWO, BB32, GAMde, DPAr, GSHape
          WRITE(21,1210)HW, AMB0, AMG0, GAM0, BET0, BET4, BB42, GAMg, 
     &                  DELg, BET3, ETO, AMUo, HWO, BB32, GAMde, DPAr, 
     &                  GSHape
 1210     FORMAT(/22X,'PARAMETERS OF HAMILTONIAN '/5X,'HW=',F12.5,3X,
     &           'AMB0=',F8.5,3X,'AMG0=',F8.5,3X,'GAM0=',F8.5,3X,
     &           'BET0=',F8.5/5X,'BET4=',F10.5,3X,'BB42=',F8.5,3X,
     &           'GAMG=',F8.5,3X,'DELG=',F8.5/5X,'BET3=',F10.5,3X,
     &           'ETO=',F9.5,3X,'AMUO=',F8.5,3X,'HWO=',F8.5,4X,'BB32=',
     &           F8.5,3X/5X,'GAMDE=',F9.5,3X,'DPAR=',F8.4,3X,'GSHAPE=',
     &           F8.5//)
          IF(MEHam.GT.2)CALL PREQU
 1220     FORMAT(//16X,'ENERGY ',4X,'LEVEL''S SPIN*2',4X,'NTU  ',6X,
     &           'NNB  ',6X,'NNG',9X,'NNO',9X,'NPO',9X,
     &           'NCA'//(1X,I4,8X,E14.7,7I11))
 1230     FORMAT(//16X,'ENERGY',5X,'LEVEL''S SPIN*2',3X,'PARITY',10X,
     &           'BAND*2',10X,'NCA',8X,'NUMB',9X,
     &           'BETB'//(1X,I4,6X,E12.5,I11,I14,I15,I15,I11,E19.5))
 1240     FORMAT(/15X,'NUMBER OF COUPLED LEVELS=',I3,5X,'NPD =',I2/14X,
     &           'NUMBER OF TERMS IN POTENTIAL EXPANSION= ',2X,I2)
        ELSE
          DO i = 1, NUR
            EL(i) = ELIs(iis,i)
            JO(i) = JOIs(iis,i)
            NPO(i) = NPOis(iis,i)
            KO(i) = KOIs(iis,i)
            NCA(i) = NCAis(iis,i)
            NUMb(i) = NUMbis(iis,i)
            BETb(i) = BETbis(iis,i)
          ENDDO
 
          PRINT 1240, NUR, NPD, LAS
          WRITE(21,1240)NUR, NPD, LAS
 
          PRINT 1230, (i,EL(i),JO(i),NPO(i),KO(i),NCA(i),NUMb(i),BETb(i)
     &          ,i = 1,NUR)
          WRITE(21,1230)(i,EL(i),JO(i),NPO(i),KO(i),NCA(i),NUMb(i),
     &                  BETb(i),i = 1,NUR)
        ENDIF
        PRINT 1250
        WRITE(21,1250)
 1250   FORMAT(/15X,'POTENTIAL   PARAMETERS   V(R)')
        PRINT 1260, VR0, VR1, VR2, RR, AR0, AR1, WD0, WD1, VR3, RD, AD0, 
     &        AD1, WC0, WC1, RC, AC0, AC1, RW, AW0, AW1, VS, RS, AS0, 
     &        AS1, ALF, ANEu, RZ, AZ, BNDc, WDA1, WCA1, CCOul, CISo, 
     &        WCIso, WS0, WS1, VRLa, ALAvr, WCBw, WCWid, WDBw, WDWid, 
     &        ALAwd, EFErmn, EFErmp, ALAso, PDIs, WSBw, WDWid, RRBwc, 
     &        RRWid, RZBwc, RZWid, EA, WDIso, WDShi, WDWid2, ALFnew, 
     &        VRD, CAVr, CARr, CAAr, CARd, CAAc
        WRITE(21,1260)VR0, VR1, VR2, RR, AR0, AR1, WD0, WD1, VR3, RD, 
     &                AD0, AD1, WC0, WC1, RC, AC0, AC1, RW, AW0, AW1, 
     &                VS, RS, AS0, AS1, ALF, ANEu, RZ, AZ, BNDc, WDA1, 
     &                WCA1, CCOul, CISo, WCIso, WS0, WS1, VRLa, ALAvr, 
     &                WCBw, WCWid, WDBw, WDWid, ALAwd, EFErmn, EFErmp, 
     &                ALAso, PDIs, WSBw, WSWid, RRBwc, RRWid, RZBwc, 
     &                RZWid, EA, WDIso, WDShi, WDWid2, ALFnew, VRD, 
     &                CAVr, CARr, CAAr, CARd, CAAc
 1260   FORMAT(/1X,'VR0=',F7.3,5X,'VR1=',F7.4,5X,'VR2=',F10.7,2X,'RR=',
     &         F7.4,5X,'AR0=',F7.4,5X,'AR1=',F7.4/1X,'WD0=',F7.4,5X,
     &         'WD1=',F7.4,5X,'VR3=',F10.7,2X,'RD=',F7.4,5X,'AD0=',F7.4,
     &         5X,'AD1=',F7.4/1X,'WC0=',F7.4,5X,'WC1=',F7.4,21X,'RC=',
     &         F7.4,5X,'AC0=',F7.4,5X,'AC1=',F7.4/49X,'RW=',F7.4,5X,
     &         'AW0=',F7.4,5X,'AW1=',F7.4/1X,'VSO=',F7.4,37X,'RS=',F7.4,
     &         5X,'AS0=',F7.4,5X,'AS1=',F7.4/1X,'ALF=',F7.4,5X,'ANEU=',
     &         F7.4,20X,'RZ=',F7.4,5X,'AZ0=',F7.4,/1X,'BNDC=',F7.2,4X,
     &         'WDA1=',F7.4,4X,'WCA1=',F7.4,4X,'CCOUL=',F7.4,5X,'CISO=',
     &         F7.3,4X,'WCISO=',F7.3/1X,'WS0=',F7.4,5X,'WS1=',F7.4,5X,
     &         'VRLA=',F7.4,4X,'ALAVR=',F8.5,4X,'WCBW=',F7.4,4X,
     &         'WCWID=',F7.4,/1X,'WDBW=',F7.4,4X,'WDWID=',F7.4,3X,
     &         'ALAWD=',F7.4,3X,'EFERMN=',F7.3,4X,'EFERMP=',F7.3,2X,
     &         'ALASO=',F7.4,/1X,'PDIS=',F7.4,4X,'WSBW=',F7.4,4X,
     &         'WSWID=',F7.2,3X,'RRBWC=',F7.4,5X,'RRWID=',F6.2,4X,
     &         'RZBWC=',F7.4,/1X,'RZWID=',F7.4,3X,'EA=',F9.5,4X,
     &         'WDISO=',F7.3,3X,'WDSHI=',F7.2,5X,'WDWID2=',F7.2,2X,
     &         'ALFNEW=',F6.3,/1X,'VRD=',F8.3,4X,'CAVR=',F8.5,3X,
     &         'CARR=',F9.6,2X,'CAAR=',F9.6,4X,'CARD=',F9.6,2X,'CAAC=',
     &         F9.6)
        PRINT 1270, (ZNUcis(i),i = 1,MENuc)
        WRITE(21,1270)(ZNUcis(i),i = 1,MENuc)
 1270   FORMAT(/30X,'NUCLEUS CHARGE = ',F7.4/)
        IF(MEHam.LE.1)THEN
          IF(NPD.NE.0)THEN
            READ(20,1300)(BETis(iis,i),i = 2,NPD,2)
            PRINT 1280, (i,BETis(iis,i),i = 2,NPD,2)
            WRITE(21,1280)(i,BETis(iis,i),i = 2,NPD,2)
 1280       FORMAT(6X,'NPD',5X,
     &             'DEFORMATION PARAMETER VALUES'/(6X,I2,13X,F7.4))
          ENDIF
        ENDIF
      ENDDO
 1290 FORMAT(36I2)
 1300 FORMAT(6E12.7)
 1310 FORMAT(E12.7,5I2,E12.7)
 1320 FORMAT(E12.7,7I2)
 1330 FORMAT(2E12.7,2I2)
      RRG = RR
      RCG = RC
      RDG = RD
      RWG = RW
      RSG = RS
      RZG = RZ
      VRG = VRLa
      ARG = AR0
      ACG = AC0
 
      READ(20,1360)(NPJ(i),i = 1,73)
      PRINT 1340
      WRITE(21,1340)
 
      WRITE(21,1340)
 1340 FORMAT(/10X,'PARAMETERS ADJUSTED'/)
      PRINT 1350, (NPJ(i),i = 1,73)
      WRITE(21,1350)(NPJ(i),i = 1,73)
 1350 FORMAT(1X,6I2)
 1360 FORMAT(6I2)
      DO iis = 1, MENuc
        NST = NSTis(iis)
        NUR = NURis(iis)
        MESol = MESois(iis)
        DO i = 1, NST
          PRINT 1370, EEIs(iis,i)
          WRITE(21,1370)EEIs(iis,i)
 1370     FORMAT(//6X,'EXPERIMENTAL DATA FOR ENERGY=',F10.6,1X,'MeV'/)
          READ(20,1290)NT(iis,i), NR(iis,i), NGN(iis,i), NGD(iis,i), 
     &                 NSF1(iis,i), NSF2(iis,i), NRAt(iis,i), 
     &                 NNAt(iis,i)
          PRINT 1350, NT(iis,i), NR(iis,i), NGN(iis,i), NGD(iis,i), 
     &          NSF1(iis,i), NSF2(iis,i), NRAt(iis,i), NNAt(iis,i)
          WRITE(21,1350)NT(iis,i), NR(iis,i), NGN(iis,i), NGD(iis,i), 
     &                  NSF1(iis,i), NSF2(iis,i), NRAt(iis,i), 
     &                  NNAt(iis,i)
          READ(20,1300)STE(iis,i), DST(iis,i), SRE(iis,i), DSR(iis,i), 
     &                 RATio(iis,i), DRAt(iis,i)
          IF(NNAt(iis,i).NE.0)READ(20,1300)CSNat(iis,i), DCSnat(iis,i)
          PRINT 1380, STE(iis,i), DST(iis,i), SRE(iis,i), DSR(iis,i), 
     &          RATio(iis,i), DRAt(iis,i), CSNat(iis,i), DCSnat(iis,i)
          WRITE(21,1380)STE(iis,i), DST(iis,i), SRE(iis,i), DSR(iis,i), 
     &                  RATio(iis,i), DRAt(iis,i), CSNat(iis,i), 
     &                  DCSnat(iis,i)
 1380     FORMAT(1X,6E12.7)
          IF(NSF1(iis,i).NE.0.OR.NSF2(iis,i).NE.0)THEN
            READ(20,1300)SE1(iis,i), DS1(iis,i), SE2(iis,i), DS2(iis,i)
            PRINT 1380, SE1(iis,i), DS1(iis,i), SE2(iis,i), DS2(iis,i)
            WRITE(21,1380)SE1(iis,i), DS1(iis,i), SE2(iis,i), DS2(iis,i)
          ENDIF
          ng = NGN(iis,i)
          IF(ng.NE.0)THEN
            READ(20,1330)(SNE(iis,i,k),DSN(iis,i,k),NIN(iis,i,k),
     &                   NFN(iis,i,k),k = 1,ng)
            PRINT 1390, 
     &            (SNE(iis,i,k),DSN(iis,i,k),NIN(iis,i,k),NFN(iis,i,k),
     &            k = 1,ng)
            WRITE(21,1390)(SNE(iis,i,k),DSN(iis,i,k),NIN(iis,i,k),
     &                    NFN(iis,i,k),k = 1,ng)
 1390       FORMAT(1X,2E12.7,2I3)
          ENDIF
          ng = NGD(iis,i)
          IF(ng.NE.0)THEN
            READ(20,1290)(NID(iis,i,k),NFD(iis,i,k),MTD(iis,i,k),k = 1,
     &                   ng)
            PRINT 1350, (NID(iis,i,k),NFD(iis,i,k),MTD(iis,i,k),k = 1,
     &            ng)
            WRITE(21,1350)(NID(iis,i,k),NFD(iis,i,k),MTD(iis,i,k),k = 1,
     &                    ng)
            DO k = 1, ng
              m = MTD(iis,i,k)
              READ(20,1300)(TED(iis,i,k,l),SNGd(iis,i,k,l),DSD(iis,i,k,l
     &                     ),l = 1,m)
              PRINT 1380, (TED(iis,i,k,l),SNGd(iis,i,k,l),DSD(iis,i,k,l)
     &              ,l = 1,m)
              WRITE(21,1380)(TED(iis,i,k,l),SNGd(iis,i,k,l),DSD(iis,i,k,
     &                      l),l = 1,m)
            ENDDO
          ENDIF
 1400     FORMAT(2E12.6,2I3)
        ENDDO
      ENDDO
      kev = 0
      IF(NPJ(1).EQ.1)THEN
        kev = kev + 1
        X(kev) = VR0
      ENDIF
      IF(NPJ(2).EQ.1)THEN
        kev = kev + 1
        X(kev) = VR1
      ENDIF
      IF(NPJ(3).EQ.1)THEN
        kev = kev + 1
        X(kev) = VR2
      ENDIF
      IF(NPJ(4).EQ.1)THEN
        kev = kev + 1
        X(kev) = VR3
      ENDIF
      IF(NPJ(5).EQ.1)THEN
        kev = kev + 1
        X(kev) = VRLa
      ENDIF
      IF(NPJ(6).EQ.1)THEN
        kev = kev + 1
        X(kev) = ALAvr
      ENDIF
      IF(NPJ(7).EQ.1)THEN
        kev = kev + 1
        X(kev) = WD0
      ENDIF
      IF(NPJ(8).EQ.1)THEN
        kev = kev + 1
        X(kev) = WD1
      ENDIF
      IF(NPJ(9).EQ.1)THEN
        kev = kev + 1
        X(kev) = WDA1
      ENDIF
      IF(NPJ(10).EQ.1)THEN
        kev = kev + 1
        X(kev) = WDBw
      ENDIF
      IF(NPJ(11).EQ.1)THEN
        kev = kev + 1
        X(kev) = WDWid
      ENDIF
      IF(NPJ(12).EQ.1)THEN
        kev = kev + 1
        X(kev) = ALAwd
      ENDIF
      IF(NPJ(13).EQ.1)THEN
        kev = kev + 1
        X(kev) = WC0
      ENDIF
      IF(NPJ(14).EQ.1)THEN
        kev = kev + 1
        X(kev) = WC1
      ENDIF
      IF(NPJ(15).EQ.1)THEN
        kev = kev + 1
        X(kev) = WCA1
      ENDIF
      IF(NPJ(16).EQ.1)THEN
        kev = kev + 1
        X(kev) = WCBw
      ENDIF
      IF(NPJ(17).EQ.1)THEN
        kev = kev + 1
        X(kev) = WCWid
      ENDIF
      IF(NPJ(18).EQ.1)THEN
        kev = kev + 1
        X(kev) = BNDc
      ENDIF
      IF(NPJ(19).EQ.1)THEN
        kev = kev + 1
        X(kev) = VS
      ENDIF
      IF(NPJ(20).EQ.1)THEN
        kev = kev + 1
        X(kev) = ALAso
      ENDIF
      IF(NPJ(21).EQ.1)THEN
        kev = kev + 1
        X(kev) = WS0
      ENDIF
      IF(NPJ(22).EQ.1)THEN
        kev = kev + 1
        X(kev) = WS1
      ENDIF
      IF(NPJ(23).EQ.1)THEN
        kev = kev + 1
        X(kev) = WSBw
      ENDIF
      IF(NPJ(24).EQ.1)THEN
        kev = kev + 1
        X(kev) = WSWid
      ENDIF
      IF(NPJ(25).EQ.1)THEN
        kev = kev + 1
        X(kev) = RR
      ENDIF
      IF(NPJ(26).EQ.1)THEN
        kev = kev + 1
        X(kev) = RRBwc
      ENDIF
      IF(NPJ(27).EQ.1)THEN
        kev = kev + 1
        X(kev) = RRWid
      ENDIF
      IF(NPJ(28).EQ.1)THEN
        kev = kev + 1
        X(kev) = PDIs
      ENDIF
      IF(NPJ(29).EQ.1)THEN
        kev = kev + 1
        X(kev) = AR0
      ENDIF
      IF(NPJ(30).EQ.1)THEN
        kev = kev + 1
        X(kev) = AR1
      ENDIF
      IF(NPJ(31).EQ.1)THEN
        kev = kev + 1
        X(kev) = RD
      ENDIF
      IF(NPJ(32).EQ.1)THEN
        kev = kev + 1
        X(kev) = AD0
      ENDIF
      IF(NPJ(33).EQ.1)THEN
        kev = kev + 1
        X(kev) = AD1
      ENDIF
      IF(NPJ(34).EQ.1)THEN
        kev = kev + 1
        X(kev) = RC
      ENDIF
      IF(NPJ(35).EQ.1)THEN
        kev = kev + 1
        X(kev) = AC0
      ENDIF
      IF(NPJ(36).EQ.1)THEN
        kev = kev + 1
        X(kev) = AC1
      ENDIF
      IF(NPJ(37).EQ.1)THEN
        kev = kev + 1
        X(kev) = RW
      ENDIF
      IF(NPJ(38).EQ.1)THEN
        kev = kev + 1
        X(kev) = AW0
      ENDIF
      IF(NPJ(39).EQ.1)THEN
        kev = kev + 1
        X(kev) = AW1
      ENDIF
      IF(NPJ(40).EQ.1)THEN
        kev = kev + 1
        X(kev) = RS
      ENDIF
      IF(NPJ(41).EQ.1)THEN
        kev = kev + 1
        X(kev) = AS0
      ENDIF
      IF(NPJ(42).EQ.1)THEN
        kev = kev + 1
        X(kev) = AS1
      ENDIF
      IF(NPJ(43).EQ.1)THEN
        kev = kev + 1
        X(kev) = RZ
      ENDIF
      IF(NPJ(44).EQ.1)THEN
        kev = kev + 1
        X(kev) = RZBwc
      ENDIF
      IF(NPJ(45).EQ.1)THEN
        kev = kev + 1
        X(kev) = RZWid
      ENDIF
      IF(NPJ(46).EQ.1)THEN
        kev = kev + 1
        X(kev) = AZ
      ENDIF
      IF(NPJ(47).EQ.1)THEN
        kev = kev + 1
        X(kev) = CCOul
      ENDIF
      IF(NPJ(48).EQ.1)THEN
        kev = kev + 1
        X(kev) = ALF
      ENDIF
      IF(NPJ(49).EQ.1)THEN
        kev = kev + 1
        X(kev) = CISo
      ENDIF
      IF(NPJ(50).EQ.1)THEN
        kev = kev + 1
        X(kev) = WCIso
      ENDIF
      IF(NPJ(51).EQ.1)THEN
        kev = kev + 1
        X(kev) = WDIso
      ENDIF
      IF(NPJ(52).EQ.1)THEN
        kev = kev + 1
        X(kev) = EA
      ENDIF
      IF(NPJ(53).EQ.1)THEN
        kev = kev + 1
        X(kev) = WDShi
      ENDIF
      IF(NPJ(54).EQ.1)THEN
        kev = kev + 1
        X(kev) = WDWid2
      ENDIF
      IF(NPJ(55).EQ.1)THEN
        kev = kev + 1
        X(kev) = ALFnew
      ENDIF
      IF(NPJ(56).EQ.1)THEN
        kev = kev + 1
        X(kev) = VRD
      ENDIF
      IF(NPJ(57).EQ.1)THEN
        kev = kev + 1
        X(kev) = BET0is(MEBet)
      ENDIF
      IF(NPJ(58).EQ.1)THEN
        kev = kev + 1
        X(kev) = BET3is(MEBet)
      ENDIF
      IF(NPJ(59).EQ.1)THEN
        kev = kev + 1
        X(kev) = BET4is(MEBet)
      ENDIF
      IF(NPJ(60).EQ.1)THEN
        kev = kev + 1
        X(kev) = BETis(MEBet,2)
      ENDIF
      IF(NPJ(61).EQ.1)THEN
        kev = kev + 1
        X(kev) = BETis(MEBet,4)
      ENDIF
      IF(NPJ(62).EQ.1)THEN
        kev = kev + 1
        X(kev) = BETis(MEBet,6)
      ENDIF
      IF(NPJ(63).EQ.1)THEN
        kev = kev + 1
        X(kev) = AMUo
      ENDIF
      IF(NPJ(64).EQ.1)THEN
        kev = kev + 1
        X(kev) = AMG0
      ENDIF
      IF(NPJ(65).EQ.1)THEN
        kev = kev + 1
        X(kev) = CAVr
      ENDIF
      IF(NPJ(66).EQ.1)THEN
        kev = kev + 1
        X(kev) = CARr
      ENDIF
      IF(NPJ(67).EQ.1)THEN
        kev = kev + 1
        X(kev) = CAAr
      ENDIF
      IF(NPJ(68).EQ.1)THEN
        kev = kev + 1
        X(kev) = CARd
      ENDIF
      IF(NPJ(69).EQ.1)THEN
        kev = kev + 1
        X(kev) = CAAc
      ENDIF
      IF(NPJ(70).EQ.1)THEN
        kev = kev + 1
        X(kev) = ERIs(MEIis,MERes)
      ENDIF
      IF(NPJ(71).EQ.1)THEN
        kev = kev + 1
        X(kev) = ABS(GNIs(MEIis,MERes))
      ENDIF
      IF(NPJ(72).EQ.1)THEN
        kev = kev + 1
        X(kev) = ABS(GREis(MEIis,MERes))
      ENDIF
      IF(NPJ(73).EQ.1)THEN
        kev = kev + 1
        X(kev) = ABS(BETbis(MEBet,MELev))
      ENDIF
      NV = kev
      READ(20,1300)(EP(k),k = 1,NV)
      PRINT 1380, (EP(k),k = 1,NV)
      WRITE(21,1380)(EP(k),k = 1,NV)
      READ(20,1300)FU
      CALL SEART
      RETURN
      END SUBROUTINE DATET
 
!---------------------------------------------------------------------------
C     *******************************************************
      SUBROUTINE SEART
C     *******************************************************
      IMPLICIT DOUBLE PRECISION(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: C, EPS1, FM, FU
      REAL*8, DIMENSION(25) :: EP, EPSgr, GR, GRR, X, X1, X2
      INTEGER :: NRL, NV
      COMMON /OPB   / C, GRR, FM, EPS1, NRL
      COMMON /OPT   / X, FU, GR, X1, X2, EP, EPSgr, NV
C
C Local variables
C
      REAL*8 :: hs, hs1
      INTEGER :: i, ll, n1c, ncc, ni, nnk, nx, nxx
      CHARACTER(4) :: nan, rnan
C
C*** End of declarations rewritten by SPAG
C
      DATA nan/' NaN'/
      OPEN(UNIT = 30,FILE = 'CHECK')
 
      ncc = 1
      ni = 0
      DO i = 1, NV
        X1(i) = X(i)
        X2(i) = X(i) + EP(i)*2.
      ENDDO
      IF(FU.EQ.0.)CALL XISQT
      FM = FU
      PRINT 1010, ni, FM
      WRITE(21,1010)ni, FM
      PRINT 1020, (i,X(i),i = 1,NV)
      WRITE(21,1020)(i,X(i),i = 1,NV)
 1010 FORMAT(1X,'NI=',I5,14X,'FM=',D30.15)
 1020 FORMAT(1X,5(3X,'NV',13X,'X',6X)/(1X,5(I5,D30.15)))
   10 nxx = 0
      n1c = 0
      FU = FM
      DO i = 1, NV
        X(i) = X1(i)
        IF(ABS(X2(i) - X1(i)) - ABS(EP(i)).LE.0.)nxx = nxx + 1
      ENDDO
      IF(nxx.EQ.NV)GOTO 50
      CALL DEFGT
      ll = 0
      nnk = 0
      DO i = 1, NV
        X2(i) = X1(i)
      ENDDO
      ni = ni + 1
      C = 0.
      DO i = 1, NV
        GRR(i) = GR(i)
        C = C + GRR(i)**2
      ENDDO
      EPS1 = FM/C
   20 nx = 0
      DO i = 1, NV
        EPSgr(i) = EPS1*GRR(i)
        IF(ABS(EPSgr(i)).GT.0.3*ABS(X1(i)))GOTO 40
        IF(ABS(EPSgr(i)) - ABS(EP(i)).LE.0.)nx = nx + 1
      ENDDO
      IF(nnk.EQ.0)nx = 0
   30 DO i = 1, NV
        hs = EPS1*GRR(i)
        hs1 = X1(i)
        X(i) = hs1 - hs
      ENDDO
      CALL XISQT
 
      WRITE(30,1030)FU
      PRINT 1030, FU
 1030 FORMAT(E12.5)
 
      BACKSPACE 30
      READ(30,1040)rnan
 1040 FORMAT(A4)
      PRINT 1040, rnan, nan
      IF(rnan.NE.nan)THEN
 
        IF(FU.LT.FM)THEN
          DO i = 1, NV
            X1(i) = X(i)
          ENDDO
          ll = ll + 1
          IF(ll.GT.3)EPS1 = EPS1*5.
          n1c = 1
          ncc = 0
          FM = FU
          PRINT 1010, ni, FM
          WRITE(21,1010)ni, FM
          PRINT 1020, (i,X(i),i = 1,NV)
          WRITE(21,1020)(i,X(i),i = 1,NV)
          GOTO 30
        ENDIF
      ENDIF
      IF(nx.EQ.NV)GOTO 10
      IF(n1c.EQ.1)ncc = ncc + 1
      IF(ncc.GE.2)THEN
        EPS1 = -EPS1
        ncc = 0
        GOTO 30
      ENDIF
   40 EPS1 = EPS1/5.
      nnk = 1
      ll = 0
      ncc = 1
      GOTO 20
 
   50 RETURN
      END SUBROUTINE SEART
 
!---------------------------------------------------------------------------
C     *******************************************************
      SUBROUTINE DEFGT
C     *******************************************************
      IMPLICIT DOUBLE PRECISION(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: C, EPS1, FM, FU
      REAL*8, DIMENSION(25) :: EP, EPSgr, GR, GRR, X, X1, X2
      INTEGER :: NRL, NV
      COMMON /OPB   / C, GRR, FM, EPS1, NRL
      COMMON /OPT   / X, FU, GR, X1, X2, EP, EPSgr, NV
C
C Local variables
C
      REAL*8 :: dl, f1
      INTEGER :: i
C
C*** End of declarations rewritten by SPAG
C
      f1 = FU
      DO i = 1, NV
        dl = EP(i)
        X(i) = X(i) + dl
        CALL XISQT
        GR(i) = (FU - f1)/dl
        X(i) = X(i) - dl
      ENDDO
      RETURN
      END SUBROUTINE DEFGT
 
!---------------------------------------------------------------------------
C     *******************************************************
      SUBROUTINE XISQT
C     *******************************************************
      IMPLICIT DOUBLE PRECISION(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: AC, AC0, AC1, ACG, AD, AD0, AD1, ALAso, ALAvr, ALAwd, 
     &          ALF, ALFnew, AMB0, AMG0, AMO, AMUo, ANEu, AR, AR0, AR1, 
     &          ARG, AS, AS0, AS1, ASP, AT, AW, AW0, AW1, AZ, BB32, 
     &          BB42, BET0, BET3, BET4, BMO, BNDc, CAAc, CAAr, CARd, 
     &          CARr, CAVr, CCOul, CISo, CMO, CSR, CSS, CST, DELg, DPAr, 
     &          EA, EFErmn, EFErmp, EN, ETO, FU, GAM0, GAMde, GAMg, 
     &          GSHape, HW, HWO, PDIs, RC, RCG, RD, RDG, RR, RRBwc, RRG, 
     &          RRWid, RS, RSG, RW, RWG, RZ, RZBwc, RZG, RZWid, SF0
      REAL*8, DIMENSION(10) :: AMB0is, AMG0is, AMUois, ATIs, BB32is, 
     &                         BB42is, BET, BET0is, BET3is, BET4is, 
     &                         DELgis, DPAris, EFIsn, EFIsp, ERN, ETOis, 
     &                         GAM0is, GAMdis, GAMgis, GNN, GREn, 
     &                         GSHaeis, HWIs, HWOis, WEI, ZNUcis
      REAL*8, DIMENSION(20) :: BETb, CM, CSN, EL, ES, WN, WNK
      REAL*8, DIMENSION(10,20) :: BETbis, ELIs
      REAL*8, DIMENSION(10,10) :: BETis, ERIs, GNIs, GREis
      REAL*8, DIMENSION(20,180) :: COEf
      REAL*8, DIMENSION(10,50) :: CSNat, DCSnat, DRAt, DS1, DS2, DSR, 
     &                            DST, EEIs, RATio, SE1, SE2, SRE, STE
      REAL*8, DIMENSION(20,150) :: DISc
      REAL*8, DIMENSION(150) :: DISg, TET
      REAL*8, DIMENSION(10,50,5,150) :: DSD, SNGd, TED
      REAL*8, DIMENSION(10,50,5) :: DSN, SNE
      REAL*8, DIMENSION(50) :: EE
      REAL*8, DIMENSION(25) :: EP, EPSgr, GR, X, X1, X2
      INTEGER, DIMENSION(180) :: INC, INR, JS
      INTEGER :: INCc, INCr, KODma, LAS, LKK, LLMa, MEApp, MEBet, MECha, 
     &           MECul, MEDis, MEHam, MEHao, MEIis, MEJob, MELev, MENuc, 
     &           MEPot, MEPri, MERel, MERes, MERip, MERrr, MERzz, MESha, 
     &           MESho, MESol, MEVol, MTEt, NCLl, NCMa, NJ, NMAx, NPD, 
     &           NREsn, NSMa, NSS, NST, NUF, NUI, NUR, NV
      INTEGER, DIMENSION(10,10) :: JCOis, JMIs, LOIs, NELa
      INTEGER, DIMENSION(10) :: JCOn, JMN, LON, MESois, NEL, NREs, 
     &                          NSTis, NURis
      INTEGER, DIMENSION(20) :: JJ, JO, KO, NCA, NNB, NNG, NNO, NPI, 
     &                          NPO, NTU, NUMb
      INTEGER, DIMENSION(10,20) :: JOIs, KOIs, NCAis, NNBis, NNGis, 
     &                             NNOis, NPOis, NTUis, NUMbis
      INTEGER, DIMENSION(50) :: MCHae
      INTEGER, DIMENSION(10,50) :: MCHais, NGD, NGN, NNAt, NR, NRAt, 
     &                             NSF1, NSF2, NT
      INTEGER, DIMENSION(10,50,5) :: MTD, NFD, NFN, NID, NIN
      INTEGER, DIMENSION(73) :: NPJ
      REAL*8, DIMENSION(180,150) :: PL
      REAL*8 :: SF1, SF2, VD, VR, VR0, VR1, VR2, VR3, VRD, VRDc, VRG, 
     &          VRLa, VS, WC, WC0, WC1, WCA1, WCBw, WCIso, WCWid, WD, 
     &          WD0, WD1, WDA1, WDBw, WDIso, WDShi, WDWid, WDWid2, WS0, 
     &          WS1, WSBw, WSWid, ZNUc
      COMMON /CS1   / CSN, CM
      COMMON /CSB   / CST, CSR, NST
      COMMON /DISCAN/ DISc, PL, COEf, LKK
      COMMON /DISK  / TET, MTEt
      COMMON /DISPE / VD, VRDc, EA, WDIso
      COMMON /DISPE2/ VRD, WDShi, WDWid2, ALFnew
      COMMON /ENA   / EN, EL, BET, NUR, NMAx, NPD, LAS
      COMMON /ENB   / EE, MCHae
      COMMON /EXPE1 / SNE, DSN, NGD, NGN, NIN, NFN
      COMMON /EXPE2 / DISg, TED, SNGd, DSD, NPJ, MTD, NID, NFD
      COMMON /EXPE3 / SE1, SE2, DS1, DS2, NSF1, NSF2
      COMMON /EXPER / STE, SRE, DST, DSR, NT, NR, NRAt, NNAt
      COMMON /GLOBI / RRG, RCG, RDG, RWG, RSG, RZG, VRG, ARG, ACG
      COMMON /GLOBIN/ NSTis, MCHais, JOIs, NPOis, KOIs, NCAis, NTUis, 
     &                NNBis, NNGis, NNOis, NURis, MESois, NUMbis, 
     &                BETbis, MENuc, MEBet, MEIis, MERes, MELev
      COMMON /GLOBRE/ EEIs, ELIs, ATIs, ZNUcis, EFIsn, EFIsp, BETis, 
     &                CAVr, CARr, CAAr, CARd, CAAc, RATio, DRAt, CSNat, 
     &                DCSnat, WEI
      COMMON /GLOHAM/ HWIs, AMB0is, AMG0is, GAM0is, BET0is, BET4is, 
     &                BB42is, GAMgis, DELgis, BET3is, ETOis, AMUois, 
     &                HWOis, BB32is, GAMdis, DPAris, GSHaeis
      COMMON /INRM  / AMO, BMO, CMO, BB42, GAMg, DELg
      COMMON /JNN   / CSS, INCc, NCLl, NSS, NJ, INCr
      COMMON /MENU  / MEJob, MEPot, MEHam, MECha, MEPri, MESol, MESha, 
     &                MESho, MEHao, MEApp, MEVol, MERel, MECul, MERzz, 
     &                MERrr, MEDis, MERip
      COMMON /NCLMA / LLMa, NCMa, NSMa, KODma
      COMMON /NU    / NUI, NUF
      COMMON /OPT   / X, FU, GR, X1, X2, EP, EPSgr, NV
      COMMON /POT1  / VR3, VR2, VR1, VR0, WC1, WC0, WD1, WD0, VS, AC0, 
     &                AR0, AW0, AD0, AS0
      COMMON /POT2  / AR1, AC1, AW1, AD1, AS1
      COMMON /POT3  / BNDc, WDA1, WCA1, CCOul, CISo, WCIso, WS0, WS1
      COMMON /POTB  / WNK, WN, VR, WC, WD
      COMMON /POTD  / ALAvr, VRLa, WCBw, WCWid, WDBw, WDWid, ALAwd, 
     &                EFErmn, EFErmp, ALAso, PDIs, WSBw, WSWid, RRBwc, 
     &                RRWid, RZBwc, RZWid
      COMMON /QNB   / JO, NPO, KO, NCA
      COMMON /QNBBAND/ NUMb, BETb
      COMMON /QNSB  / INC, INR, JS
      COMMON /RAD   / RR, RC, RD, RW, RS, AR, AC, AW, AD, AS, ALF, AT, 
     &                ANEu, RZ, ZNUc, ASP, AZ
      COMMON /RESON / ERIs, GNIs, GREis, NREs, LOIs, JMIs, JCOis, NELa
      COMMON /RESONI/ ERN, GNN, GREn, LON, JMN, JCOn, NEL, NREsn
      COMMON /SF12  / SF0, SF1, SF2
      COMMON /SHAMO / BET3, ETO, AMUo, HWO, BB32, DPAr
      COMMON /SHEM1 / HW, AMG0, AMB0, GAM0, BET0, BET4, GAMde, GSHape
      COMMON /SHEMM / ES, JJ, NTU, NNB, NNG, NNO, NPI
C
C Local variables
C
      REAL*8 :: ami, aprn, asq, calnat, cbm, cmb, cstr, enc, ff, fi, 
     &          fum, fuu, ratios, rel, sng, sumwei, wcst, weight
      CHARACTER(1) :: CPAR
      INTEGER :: i, i1, ie, iiis, iis, ilev, k, kev, kg, kod, m, meis, 
     &           ng, nnra, nnt, nnti, nntm, nurrr
      REAL*8, DIMENSION(25) :: xprn
C
C*** End of declarations rewritten by SPAG
C
 
 
 
 
      aprn = 1.D0/ATIs(1)**(1.D0/3.D0)
      aprn = 1.D0
 
      kev = 0
      IF(NPJ(1).EQ.1)THEN
        kev = kev + 1
        VR0 = X(kev)
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(2).EQ.1)THEN
        kev = kev + 1
        VR1 = X(kev)
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(3).EQ.1)THEN
        kev = kev + 1
        VR2 = X(kev)
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(4).EQ.1)THEN
        kev = kev + 1
        VR3 = X(kev)
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(5).EQ.1)THEN
        kev = kev + 1
        VRG = ABS(X(kev))
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(6).EQ.1)THEN
        kev = kev + 1
        ALAvr = X(kev)
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(7).EQ.1)THEN
        kev = kev + 1
        WD0 = ABS(X(kev))
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(8).EQ.1)THEN
        kev = kev + 1
        WD1 = ABS(X(kev))
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(9).EQ.1)THEN
        kev = kev + 1
        WDA1 = ABS(X(kev))
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(10).EQ.1)THEN
        kev = kev + 1
        WDBw = ABS(X(kev))
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(11).EQ.1)THEN
        kev = kev + 1
        WDWid = ABS(X(kev))
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(12).EQ.1)THEN
        kev = kev + 1
        ALAwd = ABS(X(kev))
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(13).EQ.1)THEN
        kev = kev + 1
        WC0 = ABS(X(kev))
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(14).EQ.1)THEN
        kev = kev + 1
        WC1 = ABS(X(kev))
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(15).EQ.1)THEN
        kev = kev + 1
        WCA1 = ABS(X(kev))
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(16).EQ.1)THEN
        kev = kev + 1
        WCBw = X(kev)
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(17).EQ.1)THEN
        kev = kev + 1
        WCWid = X(kev)
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(18).EQ.1)THEN
        kev = kev + 1
        BNDc = X(kev)
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(19).EQ.1)THEN
        kev = kev + 1
        VS = X(kev)
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(20).EQ.1)THEN
        kev = kev + 1
        ALAso = X(kev)
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(21).EQ.1)THEN
        kev = kev + 1
        WS0 = X(kev)
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(22).EQ.1)THEN
        kev = kev + 1
        WS1 = X(kev)
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(23).EQ.1)THEN
        kev = kev + 1
        WSBw = X(kev)
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(24).EQ.1)THEN
        kev = kev + 1
        WSWid = ABS(X(kev))
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(25).EQ.1)THEN
        kev = kev + 1
        RRG = ABS(X(kev))
        xprn(kev) = X(kev)*aprn
      ENDIF
      IF(NPJ(26).EQ.1)THEN
        kev = kev + 1
        RRBwc = X(kev)
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(27).EQ.1)THEN
        kev = kev + 1
        RRWid = X(kev)
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(28).EQ.1)THEN
        kev = kev + 1
        PDIs = X(kev)
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(29).EQ.1)THEN
        kev = kev + 1
        ARG = ABS(X(kev))
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(30).EQ.1)THEN
        kev = kev + 1
        AR1 = X(kev)
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(31).EQ.1)THEN
        kev = kev + 1
        RDG = X(kev)
        xprn(kev) = X(kev)*aprn
      ENDIF
      IF(NPJ(32).EQ.1)THEN
        kev = kev + 1
        AD0 = ABS(X(kev))
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(33).EQ.1)THEN
        kev = kev + 1
        AD1 = ABS(X(kev))
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(34).EQ.1)THEN
        kev = kev + 1
        RCG = ABS(X(kev))
        xprn(kev) = X(kev)*aprn
      ENDIF
C      rc=rr
      IF(NPJ(35).EQ.1)THEN
        kev = kev + 1
        ACG = ABS(X(kev))
        xprn(kev) = X(kev)
      ENDIF
C      ac0=ar0
      IF(NPJ(36).EQ.1)THEN
        kev = kev + 1
        AC1 = X(kev)
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(37).EQ.1)THEN
        kev = kev + 1
        RWG = X(kev)
        xprn(kev) = X(kev)*aprn
      ENDIF
      IF(NPJ(38).EQ.1)THEN
        kev = kev + 1
        AW0 = ABS(X(kev))
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(39).EQ.1)THEN
        kev = kev + 1
        AW1 = X(kev)
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(40).EQ.1)THEN
        kev = kev + 1
        RSG = X(kev)
        xprn(kev) = X(kev)*aprn
      ENDIF
      IF(NPJ(41).EQ.1)THEN
        kev = kev + 1
        AS0 = X(kev)
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(42).EQ.1)THEN
        kev = kev + 1
        AS1 = X(kev)
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(43).EQ.1)THEN
        kev = kev + 1
        RZG = X(kev)
        xprn(kev) = X(kev)*aprn
      ENDIF
      IF(NPJ(44).EQ.1)THEN
        kev = kev + 1
        RZBwc = X(kev)
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(45).EQ.1)THEN
        kev = kev + 1
        RZWid = X(kev)
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(46).EQ.1)THEN
        kev = kev + 1
        AZ = ABS(X(kev))
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(47).EQ.1)THEN
        kev = kev + 1
        CCOul = X(kev)
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(48).EQ.1)THEN
        kev = kev + 1
        ALF = X(kev)
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(49).EQ.1)THEN
        kev = kev + 1
        CISo = X(kev)
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(50).EQ.1)THEN
        kev = kev + 1
        WCIso = ABS(X(kev))
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(51).EQ.1)THEN
        kev = kev + 1
        WDIso = ABS(X(kev))
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(52).EQ.1)THEN
        kev = kev + 1
        EA = ABS(X(kev))
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(53).EQ.1)THEN
        kev = kev + 1
        WDShi = X(kev)
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(54).EQ.1)THEN
        kev = kev + 1
        WDWid2 = X(kev)
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(55).EQ.1)THEN
        kev = kev + 1
        ALFnew = ABS(X(kev))
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(56).EQ.1)THEN
        kev = kev + 1
        VRD = ABS(X(kev))
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(57).EQ.1)THEN
        kev = kev + 1
        BET0is(MEBet) = X(kev)
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(58).EQ.1)THEN
        kev = kev + 1
        cbm = BET3is(MEBet)/AMUois(MEBet)
        BET3is(MEBet) = X(kev)
        AMUois(MEBet) = BET3is(MEBet)/cbm
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(59).EQ.1)THEN
        kev = kev + 1
        BET4is(MEBet) = X(kev)
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(60).EQ.1)THEN
        kev = kev + 1
        BETis(MEBet,2) = X(kev)
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(61).EQ.1)THEN
        kev = kev + 1
        BETis(MEBet,4) = X(kev)
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(62).EQ.1)THEN
        kev = kev + 1
        BETis(MEBet,6) = X(kev)
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(63).EQ.1)THEN
        kev = kev + 1
        cmb = AMUois(MEBet)**2*BB32
        cbm = BET3is(MEBet)/AMUois(MEBet)
        AMUois(MEBet) = X(kev)
        xprn(kev) = X(kev)
        IF(BET3.EQ.0.)BB32 = cmb/AMUois(MEBet)**2
        IF(BET3.NE.0.)BET3is(MEBet) = AMUois(MEBet)*cbm
      ENDIF
      IF(NPJ(64).EQ.1)THEN
        kev = kev + 1
        AMG0 = ABS(X(kev))
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(65).EQ.1)THEN
        kev = kev + 1
        CAVr = X(kev)
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(66).EQ.1)THEN
        kev = kev + 1
        CARr = X(kev)
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(67).EQ.1)THEN
        kev = kev + 1
        CAAr = X(kev)
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(68).EQ.1)THEN
        kev = kev + 1
        CARd = X(kev)
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(69).EQ.1)THEN
        kev = kev + 1
        CAAc = X(kev)
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(70).EQ.1)THEN
        kev = kev + 1
        ERIs(MEIis,MERes) = X(kev)
        IF(NELa(MEIis,MERes).NE.0)ERIs(MEIis,NELa(MEIis,MERes)) = X(kev)
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(71).EQ.1)THEN
        kev = kev + 1
        GNIs(MEIis,MERes) = ABS(X(kev))
        IF(NELa(MEIis,MERes).NE.0)GNIs(MEIis,NELa(MEIis,MERes))
     &     = ABS(X(kev))
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(72).EQ.1)THEN
        kev = kev + 1
        GREis(MEIis,MERes) = ABS(X(kev))
        IF(NELa(MEIis,MERes).NE.0)GREis(MEIis,NELa(MEIis,MERes))
     &     = ABS(X(kev))
        xprn(kev) = X(kev)
      ENDIF
      IF(NPJ(73).EQ.1)THEN
        kev = kev + 1
        BETbis(MEBet,MELev) = ABS(X(kev))
        xprn(kev) = X(kev)
      ENDIF
      FU = 0.
      IF(NPJ(58).EQ.1.OR.NPJ(63).EQ.1)CALL PREQU
      nnt = 0
      kod = KODma
      nntm = 0
      fum = 0.D0
      DO iis = 1, MENuc
        NST = NSTis(iis)
        meis = 1
        DO ie = 1, NST
          nnra = 0
          wcst = 0.D0
          sumwei = 0.D0
          iiis = iis
    5     EFErmn = EFIsn(iiis)
          EFErmp = EFIsp(iiis)
          ZNUc = ZNUcis(iiis)
          AT = ATIs(iiis)
          EN = EEIs(iis,ie)
          NREsn = NREs(iiis)
          weight = WEI(iiis)
          WRITE(21,1010)AT, ZNUc
 1010     FORMAT(//1X,
     &    'ADJUSTING TO EXPERIMENTAL DATA FOR NUCLEUS WITH MASS NUMBER='
     &    ,F7.3,1X,'AND CHARGE=',F7.3//)
          MESol = MESois(iiis)
          MECha = MCHais(iis,ie)
          IF(MEHam.EQ.1)THEN
            DO i = 2, NPD, 2
              BET(i) = BETis(iiis,i)
            ENDDO
            BETb(MELev) = BETbis(iiis,MELev)
          ELSE
            HW = HWIs(iiis)
            AMB0 = AMB0is(iiis)
            AMG0 = AMG0is(iiis)
            GAM0 = GAM0is(iiis)
            BET0 = BET0is(iiis)
            BET4 = BET4is(iiis)
            BB42 = BB42is(iiis)
            GAMg = GAMgis(iiis)
            DELg = DELgis(iiis)
            BET3 = BET3is(iiis)
            ETO = ETOis(iiis)
            AMUo = AMUois(iiis)
            HWO = HWOis(iiis)
            BB32 = BB32is(iiis)
            GAMde = GAMdis(iiis)
            DPAr = DPAris(iiis)
            EN = EEIs(iis,ie)
            MECha = MCHais(iis,ie)
          ENDIF
          asq = AT**(1./3.)
          VRLa = VRG + CAVr*(AT - ATIs(1))
          RR = (RRG + CARr*(AT - ATIs(1)))*asq
          RC = RCG*asq
          RD = (RDG + CARd*(AT - ATIs(1)))*asq
          RW = RWG*asq
          RS = RSG*asq
          RZ = RZG*asq
 
 
          AR0 = ARG + CAAr*(AT - ATIs(1))
          AC0 = ACG + CAAc*(AT - ATIs(1))
 
 
          NUR = NURis(iiis)
          nurrr = NUR
          IF(MEHam.GT.1)THEN
            DO i = 1, NUR
              EL(i) = ELIs(iiis,i)
              JO(i) = JOIs(iiis,i)
              NTU(i) = NTUis(iiis,i)
              NNB(i) = NNBis(iiis,i)
              NNG(i) = NNGis(iiis,i)
              NNO(i) = NNOis(iiis,i)
              NPO(i) = NPOis(iiis,i)
              NCA(i) = NCAis(iiis,i)
            ENDDO
            IF(meis.EQ.1)CALL PREQU
            meis = 0
          ELSE
            DO i = 1, NUR
              EL(i) = ELIs(iiis,i)
              JO(i) = JOIs(iiis,i)
              NPO(i) = NPOis(iiis,i)
              KO(i) = KOIs(iiis,i)
              NCA(i) = NCAis(iiis,i)
              NUMb(i) = NUMbis(iiis,i)
              BETb(i) = BETbis(iiis,i)
              NUMb(i) = NUMbis(iiis,i)
              BETb(i) = BETbis(iiis,i)
 
 
            ENDDO
          ENDIF
 
          IF(NREsn.NE.0)THEN
            DO i = 1, NREsn
              ERN(i) = ERIs(iiis,i)
              GNN(i) = GNIs(iiis,i)
              GREn(i) = GREis(iiis,i)
              LON(i) = LOIs(iiis,i)
              JMN(i) = JMIs(iiis,i)
              JCOn(i) = JCOis(iiis,i)
              NEL(i) = NELa(iiis,i)
            ENDDO
          ENDIF
C     CREATING LEVELS FOR (P,N) ANALOG STATES CALCULATIONS
          IF(EEIs(iis,ie).GE.EL(nurrr)*(AT + 1.007825032D0)/AT + 0.5D0)
     &       THEN
            IF(MEHam.GE.1.AND.MCHais(iis,ie).EQ.1)NUR = nurrr
            IF(MEHam.GE.1.AND.MCHais(iis,ie).EQ.1)GOTO 10
          ENDIF
          DO ilev = 1, nurrr
            IF(NCAis(iis,ilev).EQ.NCAis(iis,1))NUR = ilev
          ENDDO
 
 
   10     ANEu = 1.008664924
          IF(MECha.EQ.1)ANEu = 1.007825032
          ami = 939.56536
          IF(MECha.EQ.1)ami = 938.272029
          rel = (EN + ami)/ami
          IF(MERel.EQ.0)rel = 1.
          enc = EN*AT/(AT + ANEu*rel)
          DO i1 = 1, NUR
            IF(enc.LE.EL(i1))GOTO 15
          ENDDO
          NMAx = NUR
          GOTO 20
   15     NMAx = i1 - 1
   20     KODma = kod
          IF(NMAx.LT.NUR)KODma = 0
          CALL RIPAT
          CALL ASFUT
          IF(MEHam.GT.1)CALL KNCOE
          CALL QUANT
          IF(nnra.NE.1.)THEN
            IF(MECha.NE.0)THEN
              PRINT 1040, EN, CST, CSR
              WRITE(21,1040)EN, CST, CSR
            ELSEIF(EN.GT.2.D0)THEN
              PRINT 1030, EN, CST, CSR, (CST - CSR)
              WRITE(21,1030)EN, CST, CSR, (CST - CSR)
            ELSE
              PRINT 1020, EN, CST, CSR, (CST - CSR), 
     &              SQRT((CST - CSR)/0.125663706D0)
              WRITE(21,1020)EN, CST, CSR, (CST - CSR), 
     &                      SQRT((CST - CSR)/0.125663706D0)
            ENDIF
 
            PRINT 1050, (k,EL(k),0.5*JO(k),CPAR(NPO(k)),CSN(k),k = 1,
     &            NMAx)
            WRITE(21,1050)(k,EL(k),0.5*JO(k),CPAR(NPO(k)),CSN(k),k = 1,
     &                    NMAx)
 
C 103 PRINT 130,(K,CSN(K),K=1,NMAX)
C     WRITE(21,130)(K,CSN(K),K=1,NMAX)
            IF(EN.LE.0.75)THEN
              PRINT 1060, SF0, SF1, SF2
              WRITE(21,1060)SF0, SF1, SF2
 1020         FORMAT(/1X,'NEUTRON ENERGY =',F10.6/1X,'TOTAL  CR-SECT.=',
     &               F10.6/1X,'REACTION CR-SECT. =',F10.6/1X,
     &               'TOTAL DIRECT CR-SECT.(ELASTIC + DIR.LEV EXCIT.) ='
     &               ,F10.6/1X,'SCATTERING RADIUS =',F10.6)
 1030         FORMAT(/1X,'NEUTRON ENERGY =',F10.6/1X,'TOTAL  CR-SECT.=',
     &               F10.6/1X,'REACTION CR-SECT. =',F10.6/1X,
     &               'TOTAL DIRECT CR-SECT.(ELASTIC + DIR.LEV EXCIT.) ='
     &               ,F10.6)
 1040         FORMAT(/1X,'PROTON  ENERGY =',F10.6/1X,'TOTAL  CR-SECT.=',
     &               F10.6/1X,'REACTION CR-SECT. =',F10.6)
C 130 FORMAT(/3X,'NMAX',17X,'CR-SECT. OF LEVEL EXCITATION '
C    */(1X,I5,25X,F10.6))
 1050         FORMAT(/2x,'Nlev',4X,'Elev',3x,'Jpi',9x,
     &               'CR-SECT(Nlev)'/(2X,I2,3X,F7.4,2x,F4.1,A1,10X,
     &               F10.6))
 1060         FORMAT(/30X,'STRENGTH  FUNCTIONS'/1X,'SF0=',E15.7,8X,
     &               'SF1=',E15.7,8X,'SF2=',E15.7)
              IF(NSF1(iis,ie).EQ.1)THEN
                nnt = nnt + 1
                FU = FU + ((SE1(iis,ie) - SF0)/DS1(iis,ie))**2
                fuu = ((SE1(iis,ie) - SF0)/DS1(iis,ie))**2
                WRITE(21,1070)fuu, SF0, SE1(iis,ie), DS1(iis,ie)
 1070           FORMAT(/1X,'FU FOR S0 STRENGTH FUNCTION=',E14.7,
     &                 '    CALC :',E14.7,' EXP :',E14.7,' +/- ',E14.7/)
              ENDIF
              IF(NSF2(iis,ie).EQ.1)THEN
                nnt = nnt + 1
                FU = FU + ((SE2(iis,ie) - SF1)/DS2(iis,ie))**2
                fuu = ((SE2(iis,ie) - SF1)/DS2(iis,ie))**2
                WRITE(21,1080)fuu, SF1, SE2(iis,ie), DS2(iis,ie)
 1080           FORMAT(/1X,'FU FOR S1 STRENGTH FUNCTION=',E14.7,
     &                 '    CALC :',E14.7,' EXP :',E14.7,' +/- ',E14.7/)
              ENDIF
            ENDIF
            IF(NT(iis,ie).EQ.1)THEN
              nnt = nnt + 1
              FU = FU + ((STE(iis,ie) - CST)/DST(iis,ie))**2
              fuu = ((STE(iis,ie) - CST)/DST(iis,ie))**2
              WRITE(21,1090)fuu, CST, STE(iis,ie), DST(iis,ie)
 1090         FORMAT(/1X,'FU FOR TOTAL CS=',E14.7,'    CALC :',F10.4,
     &               ' EXP :',F10.4,' +/- ',F9.4/)
            ENDIF
            IF(NR(iis,ie).EQ.1)THEN
              nnt = nnt + 1
              FU = FU + ((SRE(iis,ie) - CSR)/DSR(iis,ie))**2
              fuu = ((SRE(iis,ie) - CSR)/DSR(iis,ie))**2
              WRITE(21,1100)fuu, CSR, SRE(iis,ie), DSR(iis,ie)
 1100         FORMAT(/1X,'FU FOR REACTION CS=',E14.7,'    CALC :',F10.4,
     &               ' EXP :',F10.4,' +/- ',F9.4/)
            ENDIF
            ng = NGN(iis,ie)
            IF(ng.NE.0)THEN
              DO kg = 1, ng
                nnt = nnt + 1
                NUI = NIN(iis,ie,kg)
                NUF = NFN(iis,ie,kg)
                sng = 0.
                DO i = NUI, NUF
                  sng = sng + CSN(i)
                ENDDO
                fuu = ((SNE(iis,ie,kg) - sng)/DSN(iis,ie,kg))**2
                IF(NUI.NE.NUF.OR.NUI.EQ.1)THEN
                  WRITE(21,1110)fuu, sng, SNE(iis,ie,kg), DSN(iis,ie,kg)
 1110             FORMAT(/1X,
     &                   'FU FOR ELASTIC SCATTERING CS "LOW ENERGY" R ='
     &                   ,E14.7,'    CALC :',F10.4,' EXP :',F10.4,
     &                   ' +/- ',F9.4/)
                ENDIF
                FU = FU + fuu
              ENDDO
            ENDIF
            ng = NGD(iis,ie)
            IF(ng.NE.0)THEN
              DO kg = 1, ng
                nnt = nnt + 1
                NUI = NID(iis,ie,kg)
                NUF = NFD(iis,ie,kg)
                MTEt = MTD(iis,ie,kg)
                DO m = 1, MTEt
                  TET(m) = TED(iis,ie,kg,m)
                ENDDO
                CALL DISCA
                DO m = 1, MTEt
                  DISg(m) = 0.
                  DO i = NUI, NUF
                    DISg(m) = DISg(m) + DISc(i,m)
                  ENDDO
                ENDDO
                PRINT 1120
                WRITE(21,1120)
 1120           FORMAT(/23X,
     &                 'ANGULAR DISTRIBUTIONS OF SCATTERED PARTICLES'/)
                PRINT 1130, (m,TET(m),SNGd(iis,ie,kg,m),DISg(m),m = 1,
     &                MTEt)
                WRITE(21,1130)(m,TET(m),SNGd(iis,ie,kg,m),DISg(m),m = 1,
     &                        MTEt)
 1130           FORMAT(1X,2('MTET',2X,'ANGL(CENT)',1X,'EXP. C.-S.',1X,
     &                 'CALC. C.-S. ')/(1X,2(I3,3D12.5)))
                ff = 0.
                DO m = 1, MTEt
                  ff = ff + 
     &                 ((SNGd(iis,ie,kg,m) - DISg(m))/DSD(iis,ie,kg,m))
     &                 **2
                ENDDO
                fuu = ff/MTEt
                WRITE(21,1140)fuu
 1140           FORMAT(/1X,'FU FOR THIS ANGULAR DATA GROUP IS=',E14.7/)
                FU = FU + ff/MTEt
              ENDDO
            ENDIF
 
            NUR = nurrr
 
            IF(NRAt(iis,ie).EQ.0.AND.NNAt(iis,ie).EQ.0)CYCLE
            IF(NRAt(iis,ie).NE.0.AND.NNAt(iis,ie).NE.0.AND.NRAt(iis,ie)
     &         .NE.NNAt(iis,ie))WRITE(21,1150)
 1150       FORMAT(1X,
     &           'BOTH NRAT AND NNAT NON-EQUAL 0, THAN SHOULD BE EQUAL!'
     &           )
            IF(NRAt(iis,ie).NE.0.AND.NNAt(iis,ie).NE.0.AND.NRAt(iis,ie)
     &         .NE.NNAt(iis,ie))STOP
          ENDIF
 
          ratios = 2.D0*(cstr - CST)/(cstr + CST)
          IF(nnra.EQ.1.AND.NRAt(iis,ie).NE.0)WRITE(21,1160)cstr, CST
 1160     FORMAT(/1X,'CS FOR RATIO AT THIS ENERGY ARE=',E14.7,2X,'AND',
     &           2X,E14.7/)
          cstr = CST
          wcst = wcst + weight*cstr
          sumwei = sumwei + weight
 
          IF(NRAt(iis,ie).NE.0)iiis = NRAt(iis,ie)
          IF(NNAt(iis,ie).NE.0)iiis = NNAt(iis,ie)
          nnra = nnra + 1
          meis = 1
          IF(nnra.LT.2)GOTO 5
 
 
          IF(NRAt(iis,ie).NE.0)THEN
            nnt = nnt + 1
            fuu = ((ratios - RATio(iis,ie))/DRAt(iis,ie))**2
            WRITE(21,1170)fuu, ratios, RATio(iis,ie), DRAt(iis,ie)
            FU = FU + fuu
          ENDIF
 
          IF(NNAt(iis,ie).NE.0)THEN
            nnt = nnt + 1
            calnat = wcst/sumwei
            fuu = ((calnat - CSNat(iis,ie))/DCSnat(iis,ie))**2
            WRITE(21,1180)fuu, calnat, CSNat(iis,ie), DCSnat(iis,ie)
            FU = FU + fuu
 
 1170       FORMAT(1X,'FU FOR RATIO IS=',E14.7,'  CALC :',E14.7,
     &             ' EXP :',E14.7,' +/- ',E14.7/)
 
 1180       FORMAT(1X,'FU FOR NATURAL TOTAL IS=',E14.7,'  CALC :',E14.7,
     &             ' EXP :',E14.7,' +/- ',E14.7/)
          ENDIF
 
          meis = 1
        ENDDO
 
        nnti = nnt - nntm
        fi = FU - fum
        fum = FU
        nntm = nnt
        fi = fi/nnti
 
        WRITE(21,1190)ATIs(iis), fi
 1190   FORMAT(/1X,'NUCLEUS MASS IS=',F12.7,5X,'FU FOR NUCLEUS IS=',
     &         E14.7/)
 
 
 
      ENDDO
      FU = FU/nnt
C      PRINT 138,(I,X(I),I=1,KEV)
C      WRITE(21,138)(I,X(I),I=1,KEV)
      PRINT 1200, (i,xprn(i),i = 1,kev)
      WRITE(21,1200)(i,xprn(i),i = 1,kev)
 1200 FORMAT(1X,6(3X,'KEV',6X,'X',6X)/(1X,6(I5,E14.7)))
      PRINT 1210, FU
      WRITE(21,1210)FU
 1210 FORMAT(/1X,'FU=',E14.7)
      IF(NPJ(63).EQ.1.OR.NPJ(58).EQ.1)PRINT 1220, AMUo
      IF(NPJ(63).EQ.1.OR.NPJ(58).EQ.1)WRITE(21,1220)AMUo
 1220 FORMAT(40X,'AMUO=',F20.12/)
      CALL THORA(21)
      RETURN
      END SUBROUTINE XISQT
 
!---------------------------------------------------------------------------
 
      FUNCTION CPAR(Integ)
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      INTEGER :: Integ
      CHARACTER(1) :: CPAR
C
C*** End of declarations rewritten by SPAG
C
      CPAR = '*'
      IF(Integ.EQ. - 1)CPAR = '-'
      IF(Integ.EQ. + 1)CPAR = '+'
      RETURN
      END FUNCTION CPAR
 
!---------------------------------------------------------------------------
 
C     *******************************************************
C     END of optmand
C     *******************************************************
C     Start of ccrd
C     *******************************************************
      SUBROUTINE POTET
C     *******************************************************
      IMPLICIT DOUBLE PRECISION(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: AC, AD, ALF, ANEu, AR, AS, ASP, AT, AW, AZ, DE, EA, R, 
     &          RC, RD, RR, RS, RW, RZ, VD, VP, VR, VRDc, WC, WD, WDIso, 
     &          WP, ZNUc
      REAL*8, DIMENSION(20) :: WN, WNK
      COMMON /DISPE / VD, VRDc, EA, WDIso
      COMMON /POTB  / WNK, WN, VR, WC, WD
      COMMON /POTEB / R, DE, VP, WP
      COMMON /RAD   / RR, RC, RD, RW, RS, AR, AC, AW, AD, AS, ALF, AT, 
     &                ANEu, RZ, ZNUc, ASP, AZ
C
C Local variables
C
      REAL*8 :: arc1, brc1, brc2, ex1, ex2, x, xx
C
C*** End of declarations rewritten by SPAG
C
      x = (R - RR*DE)/AR
      IF(x.GT.23)THEN
        ex1 = EXP( - x)
        arc1 = -ex1
      ELSE
        ex1 = EXP(x)
        arc1 = -1.D0/(1.D0 + ex1)
      ENDIF
      IF(WC.NE.0.D0)THEN
        x = (R - RC*DE)/AC
        xx = ((R - RW*DE)/AW)**2
        IF(x.GT.23)THEN
          ex1 = EXP( - x)
          brc1 = -ALF*ex1 - (1.D0 - ALF)*EXP( - xx)
        ELSE
          ex1 = EXP(x)
          brc1 = -ALF/(1.D0 + ex1) - (1.D0 - ALF)*EXP( - xx)
        ENDIF
      ENDIF
      x = (R - RD*DE)/AD
      IF(x.GT.23)THEN
        ex2 = EXP( - x)
        brc2 = -4.D0*ex2
      ELSE
        ex2 = EXP(x)
        brc2 = -4.D0*ex2/(1.D0 + ex2)/(1.D0 + ex2)
      ENDIF
      VP = arc1*VR + brc2*VD + brc1*VRDc
      WP = brc1*WC + brc2*WD
      RETURN
      END SUBROUTINE POTET
 
!---------------------------------------------------------------------------
C     *******************************************************
      SUBROUTINE ASFUT
C     *******************************************************
      IMPLICIT DOUBLE PRECISION(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: AC, AD, ALF, ANEu, AR, AS, ASP, AT, AW, AZ, CONz, EN, 
     &          ETA, RC, RD, RK, RR, RS, RW, RZ, STEp, VR, WC, WD, 
     &          WSTep, X, ZNUc
      REAL*8, DIMENSION(10) :: BET
      REAL*8, DIMENSION(20,90) :: COPh, FBI1, FBI2, FBR1, FBR2, FNI1, 
     &                            FNI2, FNR1, FNR2
      REAL*8, DIMENSION(20) :: EL, WN, WNK
      REAL*8, DIMENSION(90) :: FBI, FBR, FNI, FNR
      INTEGER, DIMENSION(20) :: JO, KO, NCA, NPO
      INTEGER :: KODma, LAS, LLMa, LMA1, MEApp, MECha, MECul, MEDis, 
     &           MEHam, MEHao, MEJob, MEPot, MEPri, MERel, MERip, MERrr, 
     &           MERzz, MESha, MESho, MESol, MEVol, NCMa, NH1, NMAx, 
     &           NPD, NSMa, NUR
      COMMON /BNWI  / FNR, FNI, FBR, FBI, X, LMA1
      COMMON /COUL  / CONz, ETA, COPh
      COMMON /ENA   / EN, EL, BET, NUR, NMAx, NPD, LAS
      COMMON /FBN   / FBR1, FBI1, FNR2, FNI2, FNR1, FNI1, FBR2, FBI2
      COMMON /MENU  / MEJob, MEPot, MEHam, MECha, MEPri, MESol, MESha, 
     &                MESho, MEHao, MEApp, MEVol, MERel, MECul, MERzz, 
     &                MERrr, MEDis, MERip
      COMMON /NCLMA / LLMa, NCMa, NSMa, KODma
      COMMON /POTB  / WNK, WN, VR, WC, WD
      COMMON /QNB   / JO, NPO, KO, NCA
      COMMON /RAD   / RR, RC, RD, RW, RS, AR, AC, AW, AD, AS, ALF, AT, 
     &                ANEu, RZ, ZNUc, ASP, AZ
      COMMON /STR   / STEp, RK, NH1
      COMMON /WSTE  / WSTep
C
C Local variables
C
      REAL*8 :: ami, coph0, derel, eirelm, eirelt, rel, relpot, ww
      INTEGER :: i, k, llma1, lma2
C
C*** End of declarations rewritten by SPAG
C
      ami = 939.56536
      IF(MECha.EQ.1)ami = 938.272029
      rel = (EN + ami)/ami
      IF(MERel.EQ.0)rel = 1.D+0
      relpot = 1.D0
      IF(rel.NE.1.)relpot = 2.D+0*(EN + ami)/(2.D+0*ami + EN)
 
      eirelm = SQRT(rel**2*AT**2 + 2.D+0*rel*ANEu*AT + ANEu**2)
      eirelt = AT*SQRT(rel**2*ANEu**2 + 2.D+0*rel*ANEu*AT + AT**2)
      derel = SQRT(2.D+0*rel*ANEu*AT + AT**2 + ANEu**2)
      ww = 4.8257984E-2*eirelm*eirelt/(ANEu*eirelm + eirelt)
     &     /derel*(ANEu/1.008664924)
      IF(MERel.EQ.1.OR.MERel.EQ.3)ww = ww*relpot
 
 
      llma1 = LLMa + 1
      lma2 = 0.5252113*SQRT(VR*ww)*RR + 4.D+0
      LMA1 = 1.65*(WNK(1)*RK + 1.83) + 4.D+0
      IF(LMA1.GT.llma1)LMA1 = llma1
      DO i = 1, NUR
        COPh(i,1) = 0.
        X = WNK(i)*(RK - STEp)
 
        ETA = CONz/WNK(i)
        IF(MECha.EQ.1.AND.NCA(1).NE.NCA(i))ETA = 0.D0
 
 
        IF(ETA.GT.400.)THEN
          IF(MEPri.NE.99)PRINT 1010, i, ETA
          WRITE(21,1010)i, ETA
          PAUSE 'WARNING, SEE OUTPUT FILE !!!'
        ENDIF
 
 1010   FORMAT(9X,'WARNING! INCIDENT ENERGY IS TOO CLOSE TO',I3,3X,
     &         'LEVEL'/10X,
     &         'IT MAY CAUSE ERROR IN CALCULATED VALUES!!!!!!'//14X,
     &         'YOU CAN MOVE ENERGY BY 0.01MEV,',4X,'ETA=',E12.7,2E12.5)
 
        IF(ETA.NE.0.)CALL COPHA
        COPh(i,1) = COPh(20,1)
        IF(ETA.EQ.0.)COPh(i,1) = 0.
        IF(i.GT.NMAx)THEN
          LMA1 = lma2 + 1
          IF(LMA1.GT.llma1)LMA1 = llma1
          WSTep = 0.D0
          CALL BESIM
        ELSE
          CALL BENEC
        ENDIF
        DO k = 1, LMA1
          IF(k.LT.LMA1)THEN
            COPh(i,k + 1) = COPh(i,k) + ATAN(ETA/k)
          ENDIF
          FBR1(i,k) = FBR(k)
          FBI1(i,k) = FBI(k)
          FNR1(i,k) = FNR(k)
          FNI1(i,k) = FNI(k)
        ENDDO
        X = WNK(i)*(RK + STEp)
        IF(i.GT.NMAx)THEN
          WSTep = WNK(i)*2.D0*STEp
          CALL BESIM
        ELSE
          CALL BENEC
        ENDIF
        DO k = 1, LMA1
          IF(k.LT.LMA1)THEN
            COPh(i,k + 1) = COPh(i,k) + ATAN(ETA/k)
          ENDIF
          FBR2(i,k) = FBR(k)
          FBI2(i,k) = FBI(k)
          FNR2(i,k) = FNR(k)
          FNI2(i,k) = FNI(k)
        ENDDO
      ENDDO
      coph0 = COPh(1,1)
      DO i = 1, 20
        DO k = 1, 90
          COPh(i,k) = COPh(i,k) - coph0
        ENDDO
      ENDDO
      RETURN
      END SUBROUTINE ASFUT
 
!---------------------------------------------------------------------------
C     *******************************************************
      SUBROUTINE BENEC
C     *******************************************************
C***  FBR, FNR - F AND G ASSYMPTOTIC FUNCTIONS FOR OPENED CHANNELS
      IMPLICIT DOUBLE PRECISION(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: CONz, ETA, X
      REAL*8, DIMENSION(20,90) :: COPh
      REAL*8, DIMENSION(90) :: FBI, FBR, FNI, FNR
      INTEGER :: LMA1
      COMMON /BNWI  / FNR, FNI, FBR, FBI, X, LMA1
      COMMON /COUL  / CONz, ETA, COPh
C
C Local variables
C
      REAL*8 :: accur, ets, rhs, stepc, xk
      REAL*8, DIMENSION(91) :: fc, fcp, gc, gcp
      INTEGER :: k, k1, k2, ln, maxl, minl, numbr
C
C*** End of declarations rewritten by SPAG
C
      IF(ETA.EQ.0.)THEN
        ln = 3
        FNI(1) = 0.D0
        FNI(2) = 0.D0
        FBI(2) = 0.D0
        FBI(1) = 0.D0
        FBR(1) = SIN(X)
        FNR(1) = COS(X)
        FBR(2) = SIN(X)/X - COS(X)
        FNR(2) = COS(X)/X + SIN(X)
        IF(LMA1.LT.3)GOTO 10
      ELSE
        minl = 0
        maxl = LMA1 - 1
        rhs = X
        ets = ETA
        accur = 10.D-14
C      STEPC=100
        stepc = 999.0
        ln = 1
        numbr = 3
C      CALL RCWFN(RHS,ETS,MINL,MAXL,FC,FCP,GC,GCP,ACCUR,STEPC)
        CALL RCWF(rhs,ets,minl,maxl,fc,fcp,gc,gcp,accur,stepc,numbr)
      ENDIF
      DO k = ln, LMA1
        FBI(k) = 0.D0
        FNI(k) = 0.D0
        IF(ETA.EQ.0.)THEN
          k1 = k - 1
          k2 = k - 2
          xk = (k + k - 3)/X
          FNR(k) = xk*FNR(k1) - FNR(k2)
          FBR(k) = xk*FBR(k1) - FBR(k2)
        ELSE
          FBR(k) = fc(k)
          FNR(k) = gc(k)
        ENDIF
      ENDDO
   10 RETURN
      END SUBROUTINE BENEC
 
!---------------------------------------------------------------------------
C     *******************************************************
      SUBROUTINE BESIM
C     *******************************************************
C***  FBR, FNR - F AND G ASSYMPTOTIC FUNCTIONS FOR CLOSED CHANNELS,
C***  SUCH, THAT G+iF IS DECREASING SPHERICAL WAVE
      IMPLICIT DOUBLE PRECISION(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: CONz, ETA, RK, STEp, WSTep, X
      REAL*8, DIMENSION(20,90) :: COPh
      REAL*8, DIMENSION(90) :: FBI, FBR, FNI, FNR
      INTEGER :: LMA1, NH1
      COMMON /BNWI  / FNR, FNI, FBR, FBI, X, LMA1
      COMMON /COUL  / CONz, ETA, COPh
      COMMON /STR   / STEp, RK, NH1
      COMMON /WSTE  / WSTep
C
C Local variables
C
      REAL*8 :: ex1, ex2, xk
      INTEGER :: k, k1, k2
C
C*** End of declarations rewritten by SPAG
C
      IF(ETA.LE.0.)THEN
        ex1 = EXP(X)
        ex2 = 1./ex1
        FBR(1) = 0.D0
        FBI(1) = (ex1 + ex2)/2.
        FNR(1) = (ex1 - ex2)/2.
        FNI(1) = 0.D0
        FBR(2) = FBI(1)/X - FNR(1)
        FBI(2) = 0.D0
        FNR(2) = 0.D0
        FNI(2) = FBI(1) - FNR(1)/X
        IF(LMA1.GE.3)THEN
          DO k = 3, LMA1
            k1 = k - 1
            k2 = k - 2
            xk = (k + k - 3)/X
            FBR(k) = xk*FBI(k1) - FBR(k2)
            FBI(k) = -xk*FBR(k1) - FBI(k2)
            FNR(k) = xk*FNI(k1) - FNR(k2)
            FNI(k) = -xk*FNR(k1) - FNI(k2)
          ENDDO
        ENDIF
        RETURN
      ENDIF
      X = X - WSTep
      CALL BESIMC
      RETURN
      END SUBROUTINE BESIM
 
!---------------------------------------------------------------------------
C*******************************************************************
      SUBROUTINE WITTFU
C*******************************************************************
C
C     Calculates Whittaker functions W(-ETA, L+1/2, 2X) for
C     L=0 to L=LMA1-1, and X>0., using integral presentation form
C     See: M. Abramowitz and I. Stegun " Handbook of Mathematical
C      Functions" 1964, formulas 13.1.33, 13.2.5
C
      IMPLICIT DOUBLE PRECISION(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: ALOw, ALW, AUP, CONz, EPSin, ETA, SUMcur, X
      REAL*8, DIMENSION(20,90) :: COPh
      REAL*8, DIMENSION(90) :: FBI, FBR, FNI, FNR
      INTEGER :: LMA1
      COMMON /BNWI  / FNR, FNI, FBR, FBI, X, LMA1
      COMMON /COUL  / CONz, ETA, COPh
      COMMON /INTEG / SUMcur, ALOw, AUP, ALW, EPSin
C
C Local variables
C
      REAL*8 :: eps, st, wit
      REAL*8 :: GAMMA
      INTEGER :: i
C
C*** End of declarations rewritten by SPAG
C
      DO i = 1, LMA1
        ALW = i - 1.
        AUP = 0.D+00
        eps = 1.D-07
        st = 2.*i
        IF(i + ETA.GT.st)st = i + ETA
        wit = 0.
        EPSin = eps
    5   ALOw = AUP
        AUP = ALOw + st
        CALL SIMPSW
        wit = wit + SUMcur
        IF(SUMcur/wit.LE.eps)THEN
          FNI(i) = wit*EXP( - X - ETA*LOG(2.D+00*X))
     &             /GAMMA(1.D+00 + ALW + ETA)
        ELSE
          EPSin = eps*wit/SUMcur
          GOTO 5
        ENDIF
      ENDDO
      RETURN
      END SUBROUTINE WITTFU
 
!---------------------------------------------------------------------------
C*******************************************************************
      SUBROUTINE SIMPSW
C***********************************************************************
      IMPLICIT DOUBLE PRECISION(A - h,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: ALOw, ALW, AUP, CONz, EPSin, ETA, SUMcur, X
      REAL*8, DIMENSION(20,90) :: COPh
      REAL*8, DIMENSION(90) :: FBI, FBR, FNI, FNR
      INTEGER :: LMA1
      COMMON /BNWI  / FNR, FNI, FBR, FBI, X, LMA1
      COMMON /COUL  / CONz, ETA, COPh
      COMMON /INTEG / SUMcur, ALOw, AUP, ALW, EPSin
C
C Local variables
C
      REAL*8 :: c1, h, s2, s4, sumin, t3, y
      INTEGER :: i1, k1, nn
C
C*** End of declarations rewritten by SPAG
C
      sumin = 0.
      nn = 4
      h = (AUP - ALOw)/nn
      y = ALOw
C     IF(Y.EQ.0.) T3=0.
      t3 = 0.D0
      IF(y.NE.0.)t3 = y**(ALW + ETA)*EXP( - y)*(1.D+00 + y/2./X)
     &                **(ALW - ETA)
      y = AUP
C     IF(Y.EQ.0.) T3=T3
      IF(y.NE.0.)t3 = t3 + y**(ALW + ETA)*EXP( - y)*(1.D+00 + y/2./X)
     &                **(ALW - ETA)
      y = (ALOw + AUP)/2.
      s2 = 0.D0
C     IF(Y.EQ.0.) S2=0.
      IF(y.NE.0.)s2 = y**(ALW + ETA)*EXP( - y)*(1.D+00 + y/2./X)
     &                **(ALW - ETA)
   10 s4 = 0.D0
      k1 = nn - 1
      y = ALOw + h
      i1 = 1
   20 IF(y.EQ.0.)s4 = s4
      IF(y.NE.0.)s4 = s4 + y**(ALW + ETA)*EXP( - y)*(1.D+00 + y/2./X)
     &                **(ALW - ETA)
      IF(i1.LT.k1)THEN
        i1 = i1 + 2
        y = y + 2.*h
        GOTO 20
      ELSE
        SUMcur = h/3.*(t3 + 2.*s2 + 4.*s4)
        c1 = ABS(1. - sumin/SUMcur)
        IF(c1.GT.EPSin)THEN
          sumin = SUMcur
          s2 = s2 + s4
          h = h*0.5
          nn = nn*2
          GOTO 10
        ENDIF
      ENDIF
      SUMcur = sumin + (SUMcur - sumin)*1.066666
      RETURN
      END SUBROUTINE SIMPSW
 
!---------------------------------------------------------------------------
C     ***********************************************************************
      SUBROUTINE BESIMR
C     *******************************************************
C***  FBR, FNR - F AND G ASSYMPTOTIC FUNCTIONS FOR CLOSED
C***  CHARGED PARTICLES CHANNELS,
C***  SUCH, THAT G+iF IS DECREASING SPHERICAL WAVE
      IMPLICIT DOUBLE PRECISION(a - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: CONz, ETA, RK, STEp, WSTep, X
      REAL*8, DIMENSION(20,90) :: COPh
      REAL*8, DIMENSION(90) :: FBI, FBR, FNI, FNR
      INTEGER :: LMA1, NH1
      COMMON /BNWI  / FNR, FNI, FBR, FBI, X, LMA1
      COMMON /COUL  / CONz, ETA, COPh
      COMMON /STR   / STEp, RK, NH1
      COMMON /WSTE  / WSTep
C
C Local variables
C
      REAL*8 :: a, b, rho, ws10
      REAL*8, DIMENSION(91) :: f, fd, g, gd, sigma
      INTEGER :: i, k, l
C
C*** End of declarations rewritten by SPAG
C
      rho = X
      l = LMA1 - 1
      DO k = 1, LMA1
        FNI(k) = 0.D0
        FBR(k) = 0.D0
        FBI(k) = 3.D0
        FNR(k) = 1.D0
      ENDDO
      IF(WSTep.NE.0.)THEN
        a = 1.D0
        b = 2.D0
        ws10 = WSTep/3.D+02
        DO i = 1, 300
          rho = rho + ws10*(i - 1)
          CALL COCL(g,gd,f,fd,sigma,ETA,rho,l)
          DO k = 1, LMA1
            a = a + a/f(k)*fd(k)*ws10
            b = b + b/g(k)*gd(k)*ws10
C      PRINT 999, FNI(K),FBR(K),x,eta,WSTEP,K
C 999 FORMAT (5E12.4,I8)
C      PAUSE 1
          ENDDO
        ENDDO
        DO k = 1, LMA1
          FBI(k) = a + b
          FNR(k) = b - a
C      PRINT 999, FNI(K),FBR(K),x,eta,WSTEP,K
C 999 FORMAT (5E12.4,I8)
C      PAUSE 1
        ENDDO
      ENDIF
      RETURN
      END SUBROUTINE BESIMR
 
!---------------------------------------------------------------------------
C     ***********************************************************************
      SUBROUTINE BESIMC
C     ***********************************************************************
C***  TWO LINEAR  INDEPENDENT CLOSED CHANNELS COULOMB FUNCTIONS
C***  EQUAL UNITY AT K*(X-STEP) AND INTEGRATED TO K*(X+STEP) USING
C***  SECOND DERIVATIVE FROM FROM COULOMB EQUATION.
C***  LINEAR INDEPENDENT, AS EQUAL IN FIRST MATCHING POINT K*(X-STEP) AND DIFFERENT
C***  AT THE NEXT INTEGRATION  POINT
      IMPLICIT DOUBLE PRECISION(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: CONz, ETA, RK, STEp, WSTep, X
      REAL*8, DIMENSION(20,90) :: COPh
      REAL*8, DIMENSION(90) :: FBI, FBR, FNI, FNR
      INTEGER :: LMA1, NH1
      COMMON /BNWI  / FNR, FNI, FBR, FBI, X, LMA1
      COMMON /COUL  / CONz, ETA, COPh
      COMMON /STR   / STEp, RK, NH1
      COMMON /WSTE  / WSTep
C
C Local variables
C
      REAL*8 :: all1, eta2, f0, f1, f2, g0, g1, g2, rho, wsn, wsn2, x2
      INTEGER :: i, k, nste, nste1
C
C*** End of declarations rewritten by SPAG
C
      rho = X
      eta2 = ETA*2.D0
      nste = 10
      nste1 = nste - 1
      DO k = 1, LMA1
        FNI(k) = 0.D0
        FBR(k) = 0.D0
        FBI(k) = 1.D0
        FNR(k) = 1.D0
      ENDDO
      IF(WSTep.EQ.0.)RETURN
      DO k = 1, LMA1
        all1 = k*(k - 1.D0)
        f0 = 1.D0
        g0 = 1.D0
        f1 = f0 - 2.D0/nste
        g1 = g0 + 5.D0/nste
        wsn = WSTep/nste
        wsn2 = wsn*wsn
        X = rho
        x2 = X*X
        DO i = 1, nste1
          X = X + wsn
          f2 = 2.D0*f1 - f0 + wsn2*(all1/x2 + eta2/X + 1.D0)*f1
          g2 = 2.D0*g1 - g0 + wsn2*(all1/x2 + eta2/X + 1.D0)*g1
          f0 = f1
          f1 = f2
          g0 = g1
          g1 = g2
        ENDDO
        FBI(k) = f2
        FNR(k) = g2
C      PRINT 999, FBI(K),FNR(K),x,eta,WSTEP,K
C 999 FORMAT (5E12.4,I8)
C      PAUSE 1
      ENDDO
      RETURN
      END SUBROUTINE BESIMC
 
!---------------------------------------------------------------------------
C     ***********************************************************************
C 25/07/02
      SUBROUTINE COCL(G,Gd,F,Fd,Sigma,Eta,Rho,L)
C CLOSED CHANNEL DECREASING COULOMB FUNCTIONS
C INPUT VARIABLES: ETA: COULOMB PARAMETER; ETA >= 0
C                  RHO: |K|*R VALUE
C                  L:    MAXIMUM L VALUE
C OUTPUT VARIABLES: SIGMA(I)=0 FOR I = 1 TO L+1
C       F(I):  DECREASING SOLUTION AT (ETA,RHO) FOR I = 1 TO L+1
C       FD(I): DERIVATIVE OF F(I) FOR I = 1 TO L+1
C       G(I):  INCREASING SOLUTION FOR I = 1 TO L+1
C       GD(I): DERIVATIVE OF F(I) FOR I = 1 TO L+1
C THE FUNCTIONS ARE RENORMALISED TO 1 AND SUCH THAT  F*GD - G*FD = 1
C***********************************************************************
      IMPLICIT DOUBLE PRECISION(a - h,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      REAL*8 :: Eta, Rho
      INTEGER :: L
      REAL*8, DIMENSION(1) :: F, Fd, G, Gd, Sigma
C
C Local variables
C
      REAL*8 :: a, am, b, c, d, h, rhoa
      INTEGER :: i, is, js, lp1, m, m1, m2
      LOGICAL :: ifeqal
      REAL*8, DIMENSION(7) :: s
      REAL*8, DIMENSION(3) :: t
C
C*** End of declarations rewritten by SPAG
C
      DATA s/7*0.D0/
      IF(Eta.LT.0.D0.OR.Rho.LE.0.D0)THEN
        WRITE(6,1010)Eta, Rho
        STOP
 1010   FORMAT(' COCL  ***  ETA =',1P,D13.5,',   RHO=',D13.5,
     &         '   ARGUMENT OUT OF RANGE  ')
      ELSE
        lp1 = L + 1
        IF(Eta.LT.1.D-6)THEN
          F(1) = 1.D0
          Fd(1) = -1.D0
        ELSE
          is = 7
          m = 10.D0*Rho + 1.D0
          h = m
          h = Rho/h
          rhoa = 10.D0*(Eta + 1.D0)
          ifeqal = rhoa.LT.Rho
          IF(ifeqal)rhoa = Rho
          m = rhoa/h + 0.5D0
          rhoa = h*m
          IF(.NOT.ifeqal.AND.rhoa.LT.Rho + 1.5D0*h)rhoa = Rho + 2.D0*h
C EXPANSION IN POWERS OF 1/RHOA
    5     c = 1.D0/rhoa
          a = 1.D0
          b = a
          d = 0.D0
          DO m = 1, 26
            am = m
            a = -a*0.5D0*(Eta + am - 1.D0)*(Eta + am)*c/am
            b = b + a
            d = d - a*am*c
          ENDDO
          F(1) = 1.D0
          Fd(1) = d/b - 1.D0 - Eta/rhoa
          IF(.NOT.(ifeqal))THEN
            s(is) = b
            IF(is.NE.7)THEN
C BACKWARD INTEGRATION
              a = 2.D0 + 1.D0/1.2D0*h*h
              b = 1.D0/6.D0*h*Eta
              c = 1.D0 - 1.D0/12.D0*h*h
              m1 = rhoa/h - 0.5D0
              m2 = Rho/h - 1.5D0
              am = m1
              t(2) = b/(am + 1.D0)
              t(3) = b/am
              js = m1
              DO is = m2, m1
                DO i = 1, 6
                  s(i) = s(i + 1)/s(7)
                ENDDO
                t(1) = t(2)
                t(2) = t(3)
                am = js - 1
                t(3) = b/am
                s(7) = ((a + 10.D0*t(2))*s(6) - (c - t(1))*s(5))
     &                 /(c - t(3))
                js = js - 1
              ENDDO
              F(1) = 1.D0
              Fd(1) = (1.D0/60.D0*(s(1) - s(7)) + 0.15D0*(s(6) - s(2))
     &                + 0.75D0*(s(3) - s(5)))/(h*s(4))
            ELSE
              is = 6
              rhoa = rhoa + h
              s(7) = s(7)*EXP(h - Eta*LOG(1.D0 - h/rhoa))
              GOTO 5
            ENDIF
          ENDIF
        ENDIF
C RECURRENCE FOR L > 0
C RECURRENCE FOR L >
        c = 1.D0/Rho
        IF(L.GT.0)THEN
          DO m = 1, L
            am = m
            a = Eta/am
            b = a + c*am
            F(m + 1) = 1.D0
            Fd(m + 1) = (a*a - 1.D0)/(b - Fd(m)) - b
          ENDDO
        ENDIF
      ENDIF
      DO m = 1, lp1
        G(m) = 1.D0
        Gd(m) = 1.D0 + Fd(m)
        Sigma(m) = 0.D0
      ENDDO
      RETURN
      END SUBROUTINE COCL
 
!---------------------------------------------------------------------------
 
C     *******************************************************
C 29/05/86  ibm version
      SUBROUTINE COCLOLD(G,Gd,F,Fd,Sigma,Eta,Rho,L)
C closed channel decreasing coulomb functions
C input variables: eta: coulomb parameter; eta >= 0
C                  rho: |k|*r value
C                  l:    maximum l value
C output variables: sigma(i)=0 for i = 1 to l+1
C       f(i):  decreasing solution at (eta,rho) for i = 1 to l+1
C       fd(i): derivative of f(i) for i = 1 to l+1
C       g(i):  increasing solution for i = 1 to l+1
C       gd(i): derivative of f(i) for i = 1 to l+1
C the functions are renormalised to 1 and such that  f*gd - g*fd = 1
C***********************************************************************
      IMPLICIT DOUBLE PRECISION(a - h,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      REAL*8 :: Eta, Rho
      INTEGER :: L
      REAL*8, DIMENSION(1) :: F, Fd, G, Gd, Sigma
C
C Local variables
C
      REAL*8 :: a, am, b, c, d, h, rhoa
      INTEGER :: i, is, js, lp1, m, m1, m2
      LOGICAL :: ifeqal
      REAL*8, DIMENSION(7) :: s
      REAL*8, DIMENSION(3) :: t
C
C*** End of declarations rewritten by SPAG
C
      DATA s/7*0.D0/
      IF(Eta.LT.0.D0.OR.Rho.LE.0.D0)THEN
        WRITE(6,1010)Eta, Rho
        STOP
 1010   FORMAT(' cocl  ***  eta =',1p,d13.5,',  rho= ',d13.5,
     &         '    argument out of range  ')
      ELSE
        lp1 = L + 1
        IF(Eta.LT.1.D-6)THEN
          F(1) = 1.D0
          Fd(1) = -1.D0
        ELSE
          is = 7
          m = 10.D0*Rho + 3.D0
          h = m
          h = Rho/h
          rhoa = 10.D0*(Eta + 1.D0)
          ifeqal = rhoa.LT.Rho
          m = rhoa/h + 0.5D0
          rhoa = h*m
          IF(ifeqal.OR.rhoa.LT.Rho + 1.5D0*h)rhoa = Rho + 2.D0*h
C expansion in powers of 1/rhoa
    5     c = 1.D0/rhoa
          a = 1.D0
          b = 1.D0 - c*Eta
          F(1) = a
          Fd(1) = b
          DO m = 1, 26
            am = m
            d = 0.5D0*(Eta + am - 1.D0)*(Eta + am)*c/am
            a = -a*d
            b = -b*d - a*c
            F(1) = F(1) + a
            Fd(1) = Fd(1) + b
          ENDDO
          IF(.NOT.(ifeqal))THEN
            s(is) = F(1)
            IF(is.NE.7)THEN
C backward integration
              a = 2.D0 + 1.D0/1.2D0*h*h
              b = 1.D0/6.D0*h*Eta
              c = 1.D0 - 1.D0/12.D0*h*h
              m1 = rhoa/h - 0.5D0
              m2 = Rho/h - 1.5D0
              am = m1
              t(2) = b/(am + 1.D0)
              t(3) = b/am
              js = m1
              DO is = m2, m1
                DO i = 1, 6
                  s(i) = s(i + 1)/s(7)
                ENDDO
                t(1) = t(2)
                t(2) = t(3)
                am = js - 1
                t(3) = b/am
                s(7) = ((a + 10.D0*t(2))*s(6) - (c - t(1))*s(5))
     &                 /(c - t(3))
                js = js - 1
              ENDDO
              F(1) = 1.D0
              Fd(1) = (1.D0/60.D0*(s(1) - s(7)) + 0.15D0*(s(6) - s(2))
     &                + 0.75D0*(s(3) - s(5)))/(h*s(4))
            ELSE
              is = 6
              rhoa = rhoa + h
              s(7) = s(7)*EXP(h - Eta*LOG(1.D0 - h/rhoa))
              GOTO 5
            ENDIF
          ENDIF
        ENDIF
C recurrence for l > 0
        c = 1.D0/Rho
        IF(L.GT.0)THEN
          DO m = 1, L
            am = m
            a = Eta/am
            b = a + c*am
            F(m + 1) = 1.D0
            Fd(m + 1) = (a*a - 1.D0)/(b - Fd(m)) - b
          ENDDO
        ENDIF
      ENDIF
      DO m = 1, lp1
        G(m) = 1.D0
        Gd(m) = 1.D0 + Fd(m)
        Sigma(m) = 0.D0
      ENDDO
      RETURN
      END SUBROUTINE COCLOLD
 
!---------------------------------------------------------------------------
C     *******************************************************
      SUBROUTINE QUANT
C     *******************************************************
      IMPLICIT DOUBLE PRECISION(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C PARAMETER definitions
C
      INTEGER, PARAMETER :: LLLmax = 230
C
C COMMON variables
C
      REAL*8 :: AC, AD, ALF, ANEu, AR, AS, ASP, AT, AW, AZ, CSR, CSS, 
     &          CST, EN, RC, RD, RK, RR, RS, RW, RZ, SF0, SF1, SF2, 
     &          STEp, VR, WC, WD, ZNUc
      REAL*8, DIMENSION(10) :: BET, ERN, GNN, GREn
      REAL*8, DIMENSION(200,200) :: CI, CR
      REAL*8, DIMENSION(20) :: CM, CSN, EL, WN, WNK
      CHARACTER(20) :: FNAme
      INTEGER, DIMENSION(180) :: INC, INR, JNO, JS, LNO, NS1
      INTEGER :: INCc, INCr, JSO, JSS, KODma, LAS, LLMa, MEApp, MECha, 
     &           MECul, MEDis, MEHam, MEHao, MEJob, MEPot, MEPri, MERel, 
     &           MERip, MERrr, MERzz, MESha, MESho, MESol, MEVol, NCLl, 
     &           NCMa, NH1, NJ, NMAx, NN1, NPD, NPIo, NPIs, NREsn, NSMa, 
     &           NSPi, NSS, NST, NUR
      INTEGER, DIMENSION(10) :: JCOn, JMN, LON, NEL
      INTEGER, DIMENSION(180,250) :: JNJ, LNJ, NNJ
      INTEGER, DIMENSION(250) :: JNJ1, KPJ1, LNJ1, NNJ1
      INTEGER, DIMENSION(20) :: JO, KO, NCA, NPO
      REAL*8, DIMENSION(100) :: SSI, SSR, TL, TRL
      REAL*8, DIMENSION(180,19) :: TD1
      REAL*8, DIMENSION(100,2) :: TRLj
      COMMON /CMAT  / CR, CI
      COMMON /CS1   / CSN, CM
      COMMON /CSB   / CST, CSR, NST
      COMMON /ENA   / EN, EL, BET, NUR, NMAx, NPD, LAS
      COMMON /INOUT / FNAme
      COMMON /JNN   / CSS, INCc, NCLl, NSS, NJ, INCr
      COMMON /LNP   / JSS, NPIs, NPIo, JSO, NN1, LNO, NS1, JNO, NSPi
      COMMON /MENU  / MEJob, MEPot, MEHam, MECha, MEPri, MESol, MESha, 
     &                MESho, MEHao, MEApp, MEVol, MERel, MECul, MERzz, 
     &                MERrr, MEDis, MERip
      COMMON /NCLMA / LLMa, NCMa, NSMa, KODma
      COMMON /POTB  / WNK, WN, VR, WC, WD
      COMMON /PRON  / TL, SSR, SSI
      COMMON /QNB   / JO, NPO, KO, NCA
      COMMON /QNS1  / LNJ1, JNJ1, NNJ1, KPJ1
      COMMON /QNSB  / INC, INR, JS
      COMMON /QNSBD / LNJ, JNJ, NNJ
      COMMON /RAD   / RR, RC, RD, RW, RS, AR, AC, AW, AD, AS, ALF, AT, 
     &                ANEu, RZ, ZNUc, ASP, AZ
      COMMON /RESONI/ ERN, GNN, GREn, LON, JMN, JCOn, NEL, NREsn
      COMMON /SF12  / SF0, SF1, SF2
      COMMON /STR   / STEp, RK, NH1
      COMMON /TDB   / TD1
      COMMON /TROUT / TRL, TRLj
C
C Local variables
C
      REAL*8 :: aden, ai, ak2, ami, amumev, anoir, anorm, 
     &          anormm, apan, ci1, ci2, ck2, cmri, cmrr, con, cong, cr1, 
     &          cro, croe, csel, csnr, csnrin, csrer, csrint, cstjpi, 
     &          cstr, cstrt, derel, dtmp, ebound, ecms, eirelm, eirelt, 
     &          epc, eri, eric, fir, gnne, gtotn, hhbarc, jc, jj, p0, 
     &          p1, p2, pr0, rel, relpot, rp00, rs1, sabs, sen, sil, 
     &          sinlcc, srl, t1, tdr1, tll, wnkr, wnkr1, wnkri, wrr, 
     &          wrrre, ww
      REAL*8 :: DBLE
      INTEGER :: i, i1, i2, ic, ii, im, imp, ir, itmp, jm, jnum, l, ll, 
     &           lma2, lmax, lmax1, ltlmax, mjnj, mkpj, mll, mlnj, mnc, 
     &           mnnj, n, n1, n1m, n2, n2p, n3, nc, nceq, nch, ndel, 
     &           nj1, nl, nlev, no1, not2, not1, numbtl
      INTEGER :: INT, NINT
      CHARACTER(1) :: parc
      REAL*8, DIMENSION(250) :: stl
C
C*** End of declarations rewritten by SPAG
C
 
 
 
 
      ami = 939.56536
      IF(MECha.EQ.1)ami = 938.272029D0
C     NSPI=IDINT(ASP*2+0.001)
      NSPi = INT(ASP*2 + 0.001)
      ndel = 1
      IF(NSPi/2*2.EQ.NSPi)ndel = 0
      DO l = 1, 100
        TL(l) = 0.D0
        TRLj(l,1) = 0.D0
        TRLj(l,2) = 0.D0
        SSR(l) = 0.D0
        SSI(l) = 0.D0
      ENDDO
      lmax = 1.65*(WNK(1)*RK + 1.83) + 3
      lmax1 = lmax + 1
      IF(lmax.GT.LLMa)lmax1 = LLMa + 1
      rel = (EN + ami)/ami
      IF(MERel.EQ.0)rel = 1.D+0
      IF(rel.NE.1.)relpot = 2.D+0*(EN + ami)/(2.D+0*ami + EN)
 
 
      eirelm = SQRT(rel**2*AT**2 + 2.D+0*rel*ANEu*AT + ANEu**2)
      eirelt = AT*SQRT(rel**2*ANEu**2 + 2.D+0*rel*ANEu*AT + AT**2)
      derel = SQRT(2.D+0*rel*ANEu*AT + AT**2 + ANEu**2)
      ww = 4.8257984E-2*eirelm*eirelt/(ANEu*eirelm + eirelt)
     &     /derel*(ANEu/1.008664924)
      IF(MERel.EQ.1.OR.MERel.EQ.3)ww = ww*relpot
 
 
      lma2 = 0.5252113*SQRT(VR*ww)*RR + 4.D+0
      IF(MEPri.GE.10.AND.MEPri.LT.99)PRINT 1010, RK, ww, WNK(1), lma2, 
     &   lmax
      IF(MEPri.GE.10.AND.MEPri.LT.99)WRITE(21,1010)RK, ww, WNK(1), lma2, 
     &   lmax
 1010 FORMAT(1X,'RK=',D15.7,1X,'WW=',D15.7,1X,'WNK(1)=',D15.7,1X,
     &       'LMA2=',I3,1X,'LMAX=',I3/)
      SF0 = 0.D0
      SF1 = 0.D0
      SF2 = 0.D0
      rs1 = 1.23*AT**(1./3.) + 0.8
      wrr = WN(1)*rs1*rs1
      sen = SQRT(EN)*3141.593
      p0 = sen
      p1 = sen*3.*wrr/(1. + wrr)
      p2 = sen*5.*wrr**2/(9. + 3.*wrr + wrr**2)
      NSS = 0
      DO n = 1, NMAx
        CSN(n) = 0.
      ENDDO
      CST = 0.D0
      cstrt = 0.D0
      csrint = 0.D0
 
 
      IF(MEPri.GE.10.AND.MEPri.LT.99)PRINT 1020
      IF(MEPri.GE.10.AND.MEPri.LT.99)WRITE(21,1020)
 1020 FORMAT(20X,'LNJ1(I)     JNJ1(I)      NNJ1(I) ')
      DO not2 = 1, 300
        not1 = not2 - 1
        NJ = 0
        DO no1 = 1, 2
          JSS = JO(1) + ndel + 2*not1*( - 1)**no1
          IF(JSS.LT.0)NJ = NJ + 2
          IF(JSS.GE.0)THEN
            nj1 = 0
            DO nch = 1, 2
              NSS = NSS + 1
              JS(NSS) = JSS
              n1 = 0
              n2 = 0
              n3 = 0
              NPIs = ( - 1)**nch
              DO i1 = 1, NUR
                lmax = 1.65*(WNK(1)*RK + 1.83) + 3
                IF(i1.GT.NMAx)lmax = lma2
                IF(lmax.GT.LLMa)lmax = LLMa
                JSO = JO(i1)
                NPIo = NPO(i1)
                CALL LOGMO
                IF(NN1.NE.0)THEN
                  DO i2 = 1, NN1
                    IF(LNO(i2).GT.lmax)EXIT
                    IF(n1.GE.250)EXIT
                    n1 = n1 + 1
                    IF(i1.EQ.1)n2 = n2 + 1
                    LNJ1(n1) = LNO(i2)
                    JNJ1(n1) = JNO(i2)
                    NNJ1(n1) = i1
                    KPJ1(n1) = NPO(i1)
                    LNJ(NSS,n1) = LNJ1(n1)
                    JNJ(NSS,n1) = JNJ1(n1)
                    NNJ(NSS,n1) = NNJ1(n1)
                  ENDDO
                ENDIF
              ENDDO
              IF(n2.NE.0)THEN
                IF(KODma.GT.0)THEN
                  n2p = n2 + 1
                  n1m = n1 - 1
                  DO im = n2p, n1m
                    mll = LNJ1(im)
                    imp = im + 1
                    DO jm = imp, n1
                      IF(mll.GT.LNJ1(jm))THEN
                        mll = LNJ1(jm)
                        mlnj = LNJ1(im)
                        mjnj = JNJ1(im)
                        mnnj = NNJ1(im)
                        mkpj = KPJ1(im)
                        LNJ1(im) = LNJ1(jm)
                        JNJ1(im) = JNJ1(jm)
                        NNJ1(im) = NNJ1(jm)
                        KPJ1(im) = KPJ1(jm)
                        LNJ(NSS,im) = LNJ1(jm)
                        JNJ(NSS,im) = JNJ1(jm)
                        NNJ(NSS,im) = NNJ1(jm)
                        LNJ1(jm) = mlnj
                        JNJ1(jm) = mjnj
                        NNJ1(jm) = mnnj
                        KPJ1(jm) = mkpj
                        LNJ(NSS,jm) = mlnj
                        JNJ(NSS,jm) = mjnj
                        NNJ(NSS,jm) = mnnj
                      ENDIF
                    ENDDO
                  ENDDO
                ENDIF
                INCc = n2
C     NEXT TWO CARDS JUST TO OVERCOME THE ERROR OR  MICROSOFT FPS COMPILER
C     v.1.00, YOU CAN DELETE THEM USING OTHER COMPILERS
                IF(MEPri.LT.99)PRINT 1030, (WNK(ii),ii = 1,NUR)
 1030           FORMAT(5E12.5)
                IF(n2.GT.NCMa)INCc = NCMa
                INC(NSS) = INCc
                NCLl = n1
                IF(MEPri.LT.99)PRINT 1040, n1, NCMa
 1040           FORMAT(/2X,'GENERATED CC NUMBER=',I3,3X,
     &                 'ALLOWED CC NUMBER=',I3/)
                IF(n1.GT.NCMa)NCLl = NCMa
                DO mnc = 1, NCLl
                  IF(NNJ1(mnc).LE.NMAx)n3 = n3 + 1
                ENDDO
                INCr = n3
                INR(NSS) = INCr
                IF(MEPri.GE.10.AND.MEPri.LT.99)PRINT 1050, 
     &             (LNJ1(nl),JNJ1(nl),NNJ1(nl),nl = 1,NCLl)
                IF(MEPri.GE.10.AND.MEPri.LT.99)WRITE(21,1050)
     &             (LNJ1(nl),JNJ1(nl),NNJ1(nl),nl = 1,NCLl)
 1050           FORMAT(15(I3,I3,I2))
                IF(LAS.NE.0)THEN
                  IF(MEHam.EQ.1.AND.NPD.EQ.0)THEN
                  ENDIF
                ENDIF
C     CALL KNDIT
                CALL CMATC
                cong = (JSS + 1)/(JO(1) + 1.)
                con = 12.566372*cong/WN(1)/100./(NSPi + 1.D0)
                CSS = con*CSS
                IF(MEPri.LT.99)PRINT 1140, JSS, CSS
C     IF(CSS.LT.1.E-4) NJ=NJ+1
 
                IF(NSS.LE.1)THEN
                  fir = WNK(1)*rs1
                  csel = fir**2
 
                  fir = fir - ATAN(fir)
                  csel = csel + 3.D0*fir**2
 
                  fir = fir - ATAN(3.D0*fir/(3.D0 - fir**2))
                  csel = csel + 5.D0*fir**2
 
                  fir = fir - 
     &                  ATAN(fir*(15.D0 - fir)**2/(15.D0 - 6.D0*fir**2))
                  csel = (csel + 7.D0*fir**2)*con/cong*(NSPi + 1.D0)
                ENDIF
 
                cstjpi = 0.D0
                DO ic = 1, INCc
                  cstjpi = cstjpi + ABS(CI(ic,ic)*con)
 
                  cstr = 0.D0
                  csnr = 0.D0
                  csnrin = 0.D0
                  csrer = 0.D0
                  tdr1 = 0.D0
 
                  IF(NREsn.NE.0)THEN
 
                    ebound = 6.D0
                    cro = EXP( - 2.D0*(AT/8.D0*ebound))
                    croe = EXP(2.D0*(AT/8.D0*(EN-ebound)))
                    anormm = (1.D0 - cro)/(1.D0 - 2.D0*cro + croe)
                    anorm = 1.D0 - anormm
C     IF(MEPRI.LT.99) PRINT 919, EN,ANORM,ANORMM,EBOUND,CRO,CROE,CONG
C     PAUSE 777
                    anormm = 1.D0
                    anorm = 1.D0
 
                    DO i = 1, NREsn
 
                      IF(JCOn(i).EQ.JSS.AND.LON(i).EQ.LNJ1(ic).AND.
     &                   JMN(i).EQ.JNJ1(ic))THEN
 
                        eri = ERN(i)
                        rel = (ABS(eri) + ami)/ami
                        IF(MERel.EQ.0.OR.eri.LE.0.)rel = 1.D+0
                        IF(MERel.NE.0.AND.eri.GT.0.)
     &                     eric = (rel**2 - 1.D+0)*ami/2.D+0
                        apan = ANEu/1.0086652
                        wnkri = 0.219677*SQRT(ABS(eric))
     &                          *AT/SQRT(AT**2 + 2.D+0*ANEu*rel*AT + 
     &                          ANEu**2)*SQRT(apan)
                        wnkr = wnkri*rs1
                        wnkr1 = WNK(1)*rs1
                        wrrre = wnkr**2
                        rp00 = wnkr1/wnkr
                        IF(LON(i).EQ.0)pr0 = rp00
                        IF(LON(i).EQ.1)pr0 = rp00*wrr/(1. + wrr)
     &                     /wrrre*(1.D0 + wrrre)
                        IF(LON(i).EQ.2)
     &                     pr0 = rp00*wrr**2/(9. + 3.*wrr + wrr**2)
     &                     /wrrre**2*(9. + 3.*wrrre + wrrre**2)
                        IF(LON(i).EQ.3)
     &                     pr0 = rp00*wrr**3/(225. + 45.*wrr + 
     &                     6.*wrr**2 + wrr**3)
     &                     /wrrre**3/(225. + 45.*wrrre + 6.*wrrre**2 + 
     &                     wrrre**3)
 
                        IF(LON(i).EQ.0)fir = wnkr1
                        IF(LON(i).EQ.1)fir = wnkr1 - ATAN(wnkr1)
                        IF(LON(i).EQ.2)fir = wnkr1 - 
     &                     ATAN(3.D0*wnkr1/(3.D0 - wnkr1**2))
                        IF(LON(i).EQ.3)fir = wnkr1 - 
     &                     ATAN(wnkr1*(15.D0 - wnkr1)
     &                     **2/(15.D0 - 6.D0*wnkr1**2))
 
                        gnne = GNN(i)*pr0
                        gtotn = GREn(i) + gnne
                        IF(NEL(i).NE.0)gtotn = GREn(i)
     &                     + gnne + GNN(NEL(i))*pr0
                        aden = (EN - ERN(i))**2 + gtotn**2/4.D0
                        cmrr = gnne*gtotn/4.D0/aden
                        cmri = -gnne*(EN - ERN(i))/2.D0/aden
 
 
 
                        CR(ic,ic) = anorm*CR(ic,ic) + cmrr*anormm
                        CI(ic,ic) = anorm*CI(ic,ic) + cmri*anormm
                        GOTO 5
                        CR(ic,ic) = anorm*CR(ic,ic)
                        CI(ic,ic) = anorm*CI(ic,ic)
 
 
 
                        cstr = cstr + gnne*gtotn/aden*con/4.D0
                        csnr = csnr + gnne*gnne/aden*con/4.D0
                        csnrin = csnrin - 
     &                           (gnne*gtotn*SIN(fir)**2 - (EN - ERN(i))
     &                           *gnne*SIN(2.D0*fir))/aden*con/2.D0
                        csrer = csrer + gnne*GREn(i)/aden*con/4.D0
                        tdr1 = csrer*4.D0/con*anormm
 
    5                   IF(MEPri.LT.99)THEN
                          PRINT 1060, cstr, csnr, csrer, csnrin
                          PRINT 1060, con, gnne, gtotn, aden
                          PRINT 1060, cmri, cmrr, CI(ic,ic), CR(ic,ic)
                        ENDIF
 1060                   FORMAT(5E12.5)
                      ENDIF
                    ENDDO
                  ENDIF
 
                  cstrt = cstrt + cstr
                  csrint = csrint + csnrin
C   8 CST=CST+CI(IC,IC)*CON+CSTR+CSNRIN
                  CST = CST + CI(ic,ic)*con
                ENDDO
 
 
                IF(MECha.EQ.0)epc = 3.E-4
                IF(MECha.NE.0)epc = 3.E-9
                IF(CSS.LT.epc.AND.cstjpi.LT.epc)NJ = NJ + 1
                IF(MEPri.LT.99)PRINT 1070, CST
 1070           FORMAT(6X,F20.10)
                DO i1 = 1, NMAx
                  CM(i1) = 0.
                  DO ic = 1, INCc
                    DO ir = 1, INCr
                      IF(i1.EQ.NNJ1(ir))CM(i1) = CM(i1) + CR(ic,ir)
     &                   **2 + CI(ic,ir)**2
                    ENDDO
                  ENDDO
                  CSN(i1) = CSN(i1) + CM(i1)*con
                ENDDO
                DO ic = 1, INCc
                  jnum = (3 + JNJ1(ic) - 2*LNJ1(ic))/2
                  ci2 = CI(ic,ic)
                  t1 = ci2
C     TL,SR,SI,SFI -FOR SPIN=1/2, CHANGE 2 by NSPI+1 IN FORMULAE
                  IF(LNJ1(ic).EQ.0)SF0 = SF0 + ci2*cong
                  IF(LNJ1(ic).EQ.1)SF1 = SF1 + ci2*cong
                  IF(LNJ1(ic).EQ.2)SF2 = SF2 + ci2*cong
                  DO ir = 1, INCr
                    anoir = 1.D0
                    IF(ic.NE.ir)anoir = anorm
                    anoir = 1.D0
                    cr1 = CR(ic,ir)*anoir
                    ci1 = CI(ic,ir)*anoir
                    t1 = t1 - cr1*cr1 - ci1*ci1
                    IF(LNJ1(ic).EQ.0)SF0 = SF0 - (cr1*cr1 + ci1*ci1)
     &                 *cong
                    IF(LNJ1(ic).EQ.1)SF1 = SF1 - (cr1*cr1 + ci1*ci1)
     &                 *cong
                    IF(LNJ1(ic).EQ.2)SF2 = SF2 - (cr1*cr1 + ci1*ci1)
     &                 *cong
                  ENDDO
                  TD1(NSS,ic) = t1*4.D0 + tdr1
                  IF(MEPri.LT.99)PRINT 1060, t1, tdr1, TD1(NSS,ic)
C     PAUSE 100
 
                  TL(LNJ1(ic) + 1) = TL(LNJ1(ic) + 1) + (JSS + 1)*4.*t1
                  TRLj(LNJ1(ic) + 1,jnum) = TRLj(LNJ1(ic) + 1,jnum)
     &              + (JSS + 1)*4.*t1
                  SSR(LNJ1(ic) + 1) = SSR(LNJ1(ic) + 1)
     &                                + (1. - 2.*CI(ic,ic))*(JSS + 1)
                  SSI(LNJ1(ic) + 1) = SSI(LNJ1(ic) + 1) + 2.*CR(ic,ic)
     &                                *(JSS + 1)
                ENDDO
                IF(NSS.GE.NSMa)GOTO 10
              ELSE
                NSS = NSS - 1
                nj1 = nj1 + 1
                IF(nj1.EQ.2)GOTO 10
              ENDIF
            ENDDO
C     IF(NJ.EQ.2*(NSPI+1)) GO TO 7
            IF(NJ.EQ.4)GOTO 10
            IF(not1.EQ.0)EXIT
          ENDIF
        ENDDO
      ENDDO
   10 NJ = NSS
      IF(NREsn.NE.0)THEN
 
        IF(MEPri.LT.99)THEN
          PRINT *, ' RENORMALIZATON FROM RESONANCES'
          PRINT 1060, anorm, CST, CSN(1), cstrt, csrint
        ENDIF
        CST = (CST - CSN(1))*anorm + CSN(1) + cstrt + csrint
        CSN(1) = CSN(1) + csrint
        ltlmax = LLLmax
        WRITE(21,*)
        WRITE(21,*)' RENORMALIZATON FROM RESONANCES'
        WRITE(21,1060)anorm, CST, CSN(1), cstrt, csrint
      ENDIF
C     PAUSE 999
      IF(MEJob.NE.2)THEN
 
        IF(MEPri.LT.99)PRINT 1080
        WRITE(21,1080)
 1080   FORMAT(//1X,'ORB. MOMENT',14X,'TRANSITIONS',12X,'SR',18X,'SI'/)
 
 
        DO l = 1, lmax1
          ll = l - 1
          tll = TL(l)/2./(JO(1) + 1.)/(2.*ll + 1.)
C     TRLJ(L,1)=TRLJ(L,1)/2./(JO(1)+1.)/(2.*LL+1)
C     TRLJ(L,2)=TRLJ(L,2)/2./(JO(1)+1.)/(2.*LL+1)
          IF(ll.EQ.0)TRLj(1,1) = tll
          IF(tll.GT.1.D-15)ltlmax = l
          IF(ll.NE.0)THEN
            TRLj(l,1) = TRLj(l,1)/2./(JO(1) + 1.)/(ll)
            IF(TRLj(l,1).GT.1.E-15)ltlmax = l
          ENDIF
          TRLj(l,2) = TRLj(l,2)/2./(JO(1) + 1.)/(ll + 1)
          IF(TRLj(l,2).GT.1.E-15)ltlmax = l
          srl = SSR(l)/2./(JO(1) + 1.)/(2.*ll + 1.)
          sil = SSI(l)/2./(JO(1) + 1.)/(2.*ll + 1.)
          TRL(l) = tll
          IF(MEPri.LT.99)PRINT 1140, ll, tll, srl, sil
          WRITE(21,1140)ll, tll, srl, sil
        ENDDO
 
        ltlmax = MIN(ltlmax,LLLmax - 1)
C
C     Formatting the output for reaction codes (e.g. EMPIRE,GNASH)
C     RCN
C
        IF(MEPri.EQ.99)THEN
C
C      EMPIRE (ecis06 formatted output)
C
          numbtl = 0
          DO l = 1, ltlmax
            IF(NPO(1)*( - 1)**(l - 1).EQ. + 1)THEN
              IF(l.NE.1.AND.TRLj(l,1).GT.0.)numbtl = numbtl + 1
              IF(TRLj(l,2).GT.0.)numbtl = numbtl + 1
            ENDIF
            IF(NPO(1)*( - 1)**(l - 1).EQ. - 1)THEN
              IF(l.NE.1.AND.TRLj(l,1).GT.0.)numbtl = numbtl + 1
              IF(TRLj(l,2).GT.0.)numbtl = numbtl + 1
            ENDIF
          ENDDO
C----------------
C      from ecis06
 1090     FORMAT('<TLJ     >',F10.2,1P,D20.8,0P,F10.2,2I5)
 1100     FORMAT(1X,F9.1,4X,A1,1X,I4)
 1110     FORMAT(1X,I2,I6,F9.1,2X,1P,D18.8,0P)
C----------------
 
          OPEN(UNIT = 92,FILE = TRIM(FNAme)//'.TLJ')
 
C      WRITE(92,'(10H<TLJ     >,F10.2,F10.5,F10.2,2I5)')
          WRITE(92,1090)ANEu, EN, AT, NINT(0.5*JO(1)), numbtl
          DO l = 1, ltlmax
            ll = l - 1
            IF(NPO(1)*( - 1)**ll.EQ. - 1)CYCLE
            IF(l.NE.1.AND.TRLj(l,1).GT.0.)THEN
C         WRITE(92,'(1X,F4.1,1X,A1,1X,I4)') L-1-0.5,'+',1
C         WRITE(92,'(1X,I2,I4,F6.1,2X,1P,D14.7,0P,3X)')
              WRITE(92,1100)l - 1 - 0.5, '+', 1
              WRITE(92,1110)1, l - 1, l - 1 - 0.5, TRLj(l,1)
            ENDIF
            IF(TRLj(l,2).GT.0.)THEN
C         WRITE(92,'(1X,F4.1,1X,A1,1X,I4)') L-1+0.5,'+',1
C         WRITE(92,'(1X,I2,I4,F6.1,2X,1P,D14.7,0P,3X)')
              WRITE(92,1100)l - 1 + 0.5, '+', 1
              WRITE(92,1110)1, l - 1, l - 1 + 0.5, TRLj(l,2)
            ENDIF
          ENDDO
          DO l = 1, ltlmax
            ll = l - 1
            IF(NPO(1)*( - 1)**ll.EQ. + 1)CYCLE
            IF(l.NE.1.AND.TRLj(l,1).GT.0.)THEN
C         WRITE(92,'(1X,F4.1,1X,A1,1X,I4)') L-1-0.5,'-',1
C         WRITE(92,'(1X,I2,I4,F6.1,2X,1P,D14.7,0P,3X)')
              WRITE(92,1100)l - 1 - 0.5, '-', 1
              WRITE(92,1110)1, l - 1, l - 1 - 0.5, TRLj(l,1)
            ENDIF
            IF(TRLj(l,2).GT.0.)THEN
C         WRITE(92,'(1X,F4.1,1X,A1,1X,I4)') L-1+0.5,'-',1
C         WRITE(92,'(1X,I2,I4,F6.1,2X,1P,D14.7,0P,3X)')
              WRITE(92,1100)l - 1 + 0.5, '-', 1
              WRITE(92,1110)1, l - 1, l - 1 + 0.5, TRLj(l,2)
            ENDIF
          ENDDO
          CLOSE(92)
 
          stl = 0.D0
 
          OPEN(45,STATUS = 'old',FILE = TRIM(FNAme)//'.TLJ',ERR = 20)
 
          READ(45,*,END = 20)   ! To skip first line <TLJs.> ..
C------JC,ParC is the channel spin and parity
C------nceq is the number of coupled equations
   15     READ(45,'(1x,f9.1,4x,a1,1x,i4)',END = 20)jc, parc, nceq   ! ecis06
C------Loop over the number of coupled equations
          DO nc = 1, nceq
C--------Reading the coupled level number nlev, the orbital momentum L,
C--------angular momentum j and Transmission coefficient Tlj,c(JC)
C--------(nlev=1 corresponds to the ground state)
            READ(45,*,END = 20,ERR = 20)nlev, l, jj, dtmp
C--------Selecting only ground state
            IF(dtmp.GT.1.D-15.AND.l.LT.LLLmax)THEN
C-----------Averaging over particle and target spin, summing over channel spin jc
              stl(l + 1) = stl(l + 1) + (2*jc + 1)*dtmp/DBLE(2*l + 1)
     &                     /DBLE(2*0.5D0 + 1)/DBLE(JO(1) + 1)
            ENDIF
          ENDDO
          GOTO 15
   20     CLOSE(45)
C
        ELSE
C
C      GNASH and others
C
          WRITE(22,1130)EN, lmax, (TRL(l),l = 1,lmax1)
          WRITE(24,1120)EN, LLMa, TRLj(1,2), 
     &                  (TRLj(l,1),TRLj(l + 1,1),TRLj(l,2),TRLj(l + 1,2)
     &                  ,l = 2,LLMa,2)
C
        ENDIF
 
 1120   FORMAT(E12.6,I3/(6E11.4))
 1130   FORMAT(E12.6,I3/(6F11.8))
 1140   FORMAT(1X,I5,10X,4F20.10)
      ENDIF
C     IF(MEPRI.NE.99) PRINT 111,LNJ1(IC),CR(IC,IC),CI(IC,IC)
C 111 FORMAT (3X,I10,2F20.10)
 
      CSR = CST - CSN(1)
      sinlcc = 0.D0
      IF(NMAx.GE.2)THEN
        DO n = 2, NMAx
          sinlcc = sinlcc + CSN(n)
        ENDDO
      ENDIF
C  55 CSR=CSR-CSN(N)
      SF0 = SF0/p0
      SF1 = SF1/p1
      SF2 = SF2/p2
 
      IF(MEPri.EQ.99)THEN
C
C       for EMPIRE
C
        OPEN(46,FILE = TRIM(FNAme)//'_INC.LST')
        WRITE(46,'(A5,I6,1X,D12.6)')'LMAX:', ltlmax, EN
        DO l = 1, ltlmax
          WRITE(46,*)l - 1, stl(l)
        ENDDO
        WRITE(46,'(1x,A27,2x,6(D12.6,1x))')'EL,TOT,REAC,INEL,CC,CSFus:', 
     &        1000.D0*CSN(1), 1000.D0*CST, 1000.D0*CSR, 0.D0, 
     &        1000.D0*sinlcc, 1000.D0*(CSR - sinlcc)
C       WRITE (46,'(1x,I6)') 123456
C       DO l = 1, ltlmax
C         WRITE (46,*) l-1, SNGL(sel(l))
C       ENDDO
        CLOSE(46)
 
        OPEN(45,FILE = TRIM(FNAme)//'.INC',FORM = 'UNFORMATTED')
        IF(MERel.EQ.0)THEN
          itmp = 0
          WRITE(45)ltlmax, EN, itmp
        ELSE
          itmp = 1
          WRITE(45)ltlmax, EN, itmp
        ENDIF
        DO l = 1, ltlmax
          WRITE(45)stl(l)
        ENDDO
        WRITE(45)1000.D0*CSN(1), 1000.D0*CST, 1000.D0*CSR, 0.D0, 
     &           1000.D0*sinlcc, 1000.D0*(CSR - sinlcc)
C
C       A new flag is introduced to signal storage of the Shape elastic XS (Sel(L))
C
C       l = 123456
C       WRITE (45) l
C       DO l = 1, ltlmax
C         WRITE (45) sel(l)
C       ENDDO
        CLOSE(45)
 
 
C***********************************************************************
C  Kinematics:   lab  ===>  CM                                         *
C    With relativistic kinematics, the reduced mass is replaced by     *
C    the reduced total energy                                          *
C----------------------------------------------------------------------*
C  EN     = current lab kinetic energy                                 *
C  ecms   = current  CM kinetic energy                                 *
C  AI     = incident particle rest mass (in a.m.u.)                    *
C  AT     = target   nucleus  rest mass (in a.m.u.)                    *
C  AK2    = CM wave number                                             *
C----------------------------------------------------------------------*
C  AMUmev = a.m.u. in MeV                                              *
C----------------------------------------------------------------------*
C-------CONSTANTS COMPUTED FROM THE FUNDAMENTAL CONSTANTS, ATOMIC MASS, HBAR*C
C-------AND ALPHA, AS GIVEN IN THE EUROPEAN PHYSICAL JOURNAL, PAGE 73, VOLUME
C-------15 (2000) REFERRING FOR THESE VALUES TO THE 1998 CODATA SET WHICH MAY
C-------BE FOUND AT http://physics.nist.gov/constants
C-------CM=931.494013 +/- 0.000037 MeV
C-------The above value is the one used also in the ENDF-6 manual (April 2001, 2009)
C       AMUmev = 9.31494013D+02
C       CARBUN=931.49378D0  , this is the OPTMAN mass unit
        amumev = 9.31494013D+02
C-------CHB=197.3269601 +/- 0.0000078 (*1.0E-9 eV*cm)
        hhbarc = 197.3269601D0
 
        ai = ANEu
        ck2 = (2.D0*amumev)/(hhbarc**2)
C
C-------From lab to CM (the input quantity is EN)
C
C
        IF(MERel.EQ.0)THEN
C---------Classical    kinematics
          ecms = EN*AT/(ai + AT)
          ak2 = ck2*ai*AT/(ai + AT)*ecms
        ELSE
C
C---------Relativistic kinematics
          ecms = amumev*(ai + AT)
     &           *(SQRT(1.D0 + 2.D0*EN/(amumev*AT*((1.D0+ai/AT)**2)))
     &           - 1.D0)
          p2 = (EN*(EN + 2.D0*amumev*ai))
     &         /((1.D0 + ai/AT)**2 + 2.D0*EN/(amumev*AT))
          ak2 = p2/(hhbarc*hhbarc)
        ENDIF
C       10 factor converts to mbarns
        dtmp = 10.D0*4.D0*ATAN(1.D0)/ak2
        sabs = 0.D0
        DO l = 0, ltlmax
          sabs = sabs + stl(l + 1)*DBLE(2*l + 1)
        ENDDO
C       write(*,*) 'Test       :',dtmp*sabs,1000*(CSR-SINLcc)
 
C       Renormalizing TLs
        IF(dtmp.GT.0.D0.AND.sabs.GT.0.D0)THEN
          DO l = 0, ltlmax
            stl(l + 1) = stl(l + 1)/(dtmp*sabs)*1000.D0*(CSR - sinlcc)
          ENDDO
          sabs = 0.D0
          DO l = 0, ltlmax
            sabs = sabs + stl(l + 1)*DBLE(2*l + 1)
          ENDDO
C         write(*,*) 'Test renorm:',dtmp*sabs,1000*(CSR-SINLcc)
        ENDIF
 
      ENDIF
 
      CSR = CSR - sinlcc
 
      RETURN
      END SUBROUTINE QUANT
 
!---------------------------------------------------------------------------
C     *******************************************************
      SUBROUTINE CHLOG
C     *******************************************************
      IMPLICIT DOUBLE PRECISION(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      INTEGER, DIMENSION(180) :: JNO, LNO, NS1
      INTEGER :: JSO, JSS, NN1, NPIo, NPIs, NSPi
      COMMON /LNP   / JSS, NPIs, NPIo, JSO, NN1, LNO, NS1, JNO, NSPi
C
C Local variables
C
      INTEGER :: IABS
      INTEGER :: k1, k2, l, n
C
C*** End of declarations rewritten by SPAG
C
      n = 0
      k1 = IABS(JSO - JSS) + 2
      k2 = JSO + JSS + 2
      DO l = k1, k2, 2
        n = n + 1
        IF(NPIo*( - 1)**((l-3)/2)*NPIs.LT.0)THEN
          LNO(n) = (l - 1)/2
          NS1(n) = 2
        ELSE
          LNO(n) = (l - 3)/2
          NS1(n) = 1
        ENDIF
      ENDDO
      NN1 = n
      RETURN
      END SUBROUTINE CHLOG
 
!---------------------------------------------------------------------------
C     *******************************************************
      SUBROUTINE RACAH
C     *******************************************************
      IMPLICIT DOUBLE PRECISION(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8, DIMENSION(800) :: A
      INTEGER :: JA, JB, JC, JD, JE, JF
      REAL*8 :: W
      COMMON /LOFAC / A
      COMMON /RACB  / JA, JB, JC, JD, JE, JF, W
C
C Local variables
C
      REAL*8 :: c1, cl, cl1, fln
      INTEGER :: i, k, nb, nb1, nb2, nb3, nb4, nf, nm, nm1, nm2, nm3
C
C*** End of declarations rewritten by SPAG
C
      W = 0.
      nb1 = JA + JB + JC
      nb2 = JC + JD + JE
      nb3 = JA + JE + JF
      nb4 = JB + JD + JF
      nm1 = JA + JB + JD + JE
      nm2 = JA + JC + JD + JF
      nm3 = JB + JC + JE + JF
      nm = nm1
      nb = nb1
      IF(nm2.LT.nm)THEN
        nm = nm2
      ENDIF
      IF(nm3.LT.nm)THEN
        nm = nm3
      ENDIF
      IF(nb2.GT.nb)THEN
        nb = nb2
      ENDIF
      IF(nb3.GT.nb)THEN
        nb = nb3
      ENDIF
      IF(nb4.GT.nb)THEN
        nb = nb4
      ENDIF
      IF(nb.LE.nm)THEN
        nf = JA + JB - JC
        fln = A(nf + 2)
        cl = fln
        nf = JA - JB + JC
        fln = A(nf + 2)
        cl = cl + fln
        nf = JB + JC - JA
        fln = A(nf + 2)
        cl = cl + fln
        nf = nb1 + 2
        fln = A(nf + 2)
        cl = cl - fln
        nf = JC + JD - JE
        fln = A(nf + 2)
        cl = cl + fln
        nf = JC - JD + JE
        fln = A(nf + 2)
        cl = cl + fln
        nf = JD + JE - JC
        fln = A(nf + 2)
        cl = cl + fln
        nf = nb2 + 2
        fln = A(nf + 2)
        cl = cl - fln
        nf = JA + JE - JF
        fln = A(nf + 2)
        cl = cl + fln
        nf = JA - JE + JF
        fln = A(nf + 2)
        cl = cl + fln
        nf = JE + JF - JA
        fln = A(nf + 2)
        cl = cl + fln
        nf = nb3 + 2
        fln = A(nf + 2)
        cl = cl - fln
        nf = JB + JD - JF
        fln = A(nf + 2)
        cl = cl + fln
        nf = JB - JD + JF
        fln = A(nf + 2)
        cl = cl + fln
        nf = JD + JF - JB
        fln = A(nf + 2)
        cl = cl + fln
        nf = nb4 + 2
        fln = A(nf + 2)
        cl = 0.5*(cl - fln)
        nb = nb + 2
        nm = nm + 2
        DO k = nb, nm, 2
          c1 = 1.
          nf = k
          fln = A(nf + 2)
          cl1 = cl + fln
          i = k - 2
          nf = i - nb1
          fln = A(nf + 2)
          cl1 = cl1 - fln
          nf = i - nb2
          fln = A(nf + 2)
          cl1 = cl1 - fln
          nf = i - nb3
          fln = A(nf + 2)
          cl1 = cl1 - fln
          nf = i - nb4
          fln = A(nf + 2)
          cl1 = cl1 - fln
          nf = nm1 - i
          fln = A(nf + 2)
          cl1 = cl1 - fln
          nf = nm2 - i
          fln = A(nf + 2)
          cl1 = cl1 - fln
          nf = nm3 - i
          fln = A(nf + 2)
          cl1 = cl1 - fln
          IF((nm1 + i)/4*4.NE.nm1 + i)c1 = -1.
          W = W + c1*EXP(cl1)
        ENDDO
      ENDIF
      RETURN
      END SUBROUTINE RACAH
 
!---------------------------------------------------------------------------
C     *******************************************************
      SUBROUTINE SOSIT
C     *******************************************************
      IMPLICIT DOUBLE PRECISION(a - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: AC, AD, ALF, ANEu, AR, AS, ASP, AT, AW, AZ, CBEt0, CSS, 
     &          EN, RC, RD, RK, RR, RS, RW, RZ, STEp, VR, WC, WD, ZNUc
      REAL*8, DIMENSION(40000) :: B, CVNrpn, E, F, G, P, Q, R, S, T, V, 
     &                            VI1, VI2, VI3, VI4, VR1, VR2, VR3, 
     &                            VR4, W, X4, X5, X6, Y4, Y5, Y6, Z
      REAL*8, DIMENSION(10) :: BET
      REAL*8, DIMENSION(720000) :: CVNc
      REAL*8, DIMENSION(160000) :: CVNr
      REAL*8, DIMENSION(300) :: DEF, VL
      REAL*8, DIMENSION(20) :: EL, WN, WNK
      REAL*8, DIMENSION(300,40000) :: FIEm, FREm
      REAL*8, DIMENSION(200,200) :: FIF1, FIF2, FRF1, FRF2
      INTEGER :: INCc, INCr, LAS, MEApp, MECha, MECul, MEDis, MEHam, 
     &           MEHao, MEJob, MEPot, MEPri, MERel, MERip, MERrr, MERzz, 
     &           MESha, MESho, MESol, MEVol, NCLl, NH1, NJ, NMAx, NPD, 
     &           NSS, NUR
      INTEGER, DIMENSION(250) :: JNJ1, KPJ1, LNJ1, NNJ1
      INTEGER, DIMENSION(20) :: JO, KO, NCA, NPO
      REAL*8, DIMENSION(120000) :: PII, PRR, PVV, PWW, VSL, WSL
      REAL*8, DIMENSION(5,300) :: POL
      REAL*8, DIMENSION(5400) :: PRC, PVC
      REAL*8, DIMENSION(600000) :: PV, PW
      REAL*8, DIMENSION(200) :: VI5e, VR5e
      COMMON /AB    / DEF, VL, VSL, POL, PV, PW, WSL
      COMMON /ABCOUL/ PVC, PRC, CVNc
      COMMON /CONT  / P, R, W, V, E, F, S, B, Q, T, G, Z
      COMMON /CV    / CVNr
      COMMON /CVOL  / CBEt0
      COMMON /CVPN  / CVNrpn
      COMMON /ENA   / EN, EL, BET, NUR, NMAx, NPD, LAS
      COMMON /FFV   / FRF1, FRF2, FIF1, FIF2
      COMMON /FUEC  / FREm, FIEm
      COMMON /JNN   / CSS, INCc, NCLl, NSS, NJ, INCr
      COMMON /MENU  / MEJob, MEPot, MEHam, MECha, MEPri, MESol, MESha, 
     &                MESho, MEHao, MEApp, MEVol, MERel, MECul, MERzz, 
     &                MERrr, MEDis, MERip
      COMMON /POTB  / WNK, WN, VR, WC, WD
      COMMON /POTPN / PVV, PWW, PRR, PII
      COMMON /QNB   / JO, NPO, KO, NCA
      COMMON /QNS1  / LNJ1, JNJ1, NNJ1, KPJ1
      COMMON /RAD   / RR, RC, RD, RW, RS, AR, AC, AW, AD, AS, ALF, AT, 
     &                ANEu, RZ, ZNUc, ASP, AZ
      COMMON /STR   / STEp, RK, NH1
      COMMON /VRI   / X4, X5, X6, Y4, Y5, Y6, VR1, VR2, VR3, VR4, VI1, 
     &                VI2, VI3, VI4
      COMMON /VRII  / VR5e, VI5e
C
C Local variables
C
      REAL*8 :: a, aa, acs, c, c2, c3, cj1, potr2, potr5, pv1, pvs, pw1, 
     &          pws, sop, spi2, st, st2, step1, ui, ur, vi5l, vr5l, wi2, 
     &          wi3, wi4, wr2, wr3, wr4, x5n, x6l, xw5, y5n, y6l, yw5
      INTEGER :: icll, jj, k, k1, k2, kc1, kc2, kk, kl, kn, l, la, laf, 
     &           lai, lalas, las1, las2, las8, lasc, ll, ll1, ll2, lll, 
     &           ln, lp1, lpn1, m, m4, mf, mla, mm, mm1, mm2, mmc, mnu, 
     &           mnuk, mnukl, mnul, mnula, mnulak, mnulal, mnulkl, 
     &           mnulln, mnuln, n, nb, ncl2, ncla, nclk, nh, nhtt, nn, 
     &           npi1, npi2, nu, nuk, nul, nun
C
C*** End of declarations rewritten by SPAG
C
C     BELOW CARD IS NECESSARY IF MEMORY IS LESS THAN 32Mb
 
C     WRITE(21,'(1X,11I6,1X,6(f8.4,1X))') NCLL
 
      spi2 = ASP*(ASP + 1.)
      ncl2 = NCLl*NCLl
      IF(MEPot.EQ.1)THEN
        las2 = (LAS + 2)/2
        las1 = las2 - 1
      ELSE
        las2 = LAS + 1
        las1 = LAS
      ENDIF
      ncla = NCLl*las1
      las8 = 9
      lasc = 1
      IF(las2.GE.3)las8 = 18
      IF(las2.GE.3)lasc = 2
      icll = NCLl*las8
      mnula = 300*NUR*las2
      mla = 300*las2
      mnu = 300*NUR
      nh = NH1
      st = 1.
      m = 8
      m4 = 4*m
      mf = m4 - 1
      step1 = STEp/m
 
C     K-NUMBER OF INDEPENDENT SOLUTION
 
      DO k = 1, NCLl
        nclk = (k - 1)*NCLl
        kc1 = icll*(k - 1)
        k1 = (k - 1)*las1 - 1
        st2 = step1*step1/240.
        ll1 = LNJ1(k)
        npi1 = KPJ1(k)
        nuk = NNJ1(k)
        mnulak = mnula*(nuk - 1)
        mnuk = mnu*(nuk - 1)
        c = st*(step1)**(ll1 + 1)
        c2 = st*(2.*step1)**(ll1 + 1)
        c3 = st*(3.*step1)**(ll1 + 1)
 
C     L-LINE OF INDEPENDENT SOLUTION
 
        DO l = 1, NCLl
          kl = nclk + l
          kc2 = kc1 + las8*(l - 1)
          k2 = k1 + (l - 1)*ncla
          ll = LNJ1(l)
          nul = NNJ1(l)
          nu = nul
          mnulkl = mnulak + mla*(nul - 1)
          mnukl = mnuk + 300*(nul - 1)
          npi2 = KPJ1(l)
          jj = JNJ1(l)
          cj1 = jj/2.
          sop = (cj1 - ll)*(cj1 + ll + 1) - spi2
          X4(kl) = 0.
          Y4(kl) = 0.
          X5(kl) = 0.
          Y5(kl) = 0.
          VR1(kl) = 0.
          VI1(kl) = 0.
          ur = 0.
          ui = 0.
 
 
 
          c = CVNrpn(kl)
C     IF(NU.NE.NUN.AND.JO(NU).EQ.JO(NUN).AND. JO(NU).EQ.0) C=1.D0
          IF(NCA(nu).NE.NCA(nuk).AND.JO(nu).EQ.JO(nuk))
     &       ur = ur + PV(mnulkl + 1)*c
          IF(NCA(nu).NE.NCA(nuk).AND.JO(nu).EQ.JO(nuk))
     &       ui = ui + PW(mnulkl + 1)*c
 
 
 
 
C     IF(NU.NE.NUK.AND.JO(NU).EQ. JO(NUK)) UR=UR+PVV(MNUKL+1)*CVNRPN(KL)
C     IF(NU.NE.NUK.AND.JO(NU).EQ. JO(NUK)) UI=UI+PWW(MNUKL+1)*CVNRPN(KL)
 
 
 
          acs = 0.
          IF(las2.GE.2)THEN
            DO lll = 2, las2
              lp1 = mnulkl + lll
              pv1 = PV(lp1)
              pw1 = PW(lp1)
              IF(lll.EQ.2)pvs = pv1
              IF(lll.EQ.2)pws = pw1
              ll2 = k2 + lll
              a = CVNr(ll2)
              IF(npi1.NE.npi2)THEN
                ur = ur - a*pw1
                ui = ui + a*pv1
              ELSE
                ur = ur + a*pv1
                ui = ui + a*pw1
              ENDIF
              IF(MEPot.NE.1)THEN
                IF(lll.LE.3)THEN
                  lai = 3
                  laf = 5
                  IF(lll.EQ.3)lai = 1
                  IF(lll.EQ.3)laf = 9
                  DO la = lai, laf
                    lalas = lasc*(la - 1) + lll - 1
                    lp1 = lalas
                    pv1 = PVC(lp1)
                    ll2 = kc2 + lalas
                    a = CVNc(ll2)
                    IF(lll.EQ.3.AND.la.EQ.1)acs = -a*CBEt0
                    IF(la.NE.1)THEN
                      IF(npi1.NE.npi2)THEN
                        ui = ui + a*pv1
                      ELSE
                        ur = ur + a*pv1
                      ENDIF
                    ENDIF
                  ENDDO
                ENDIF
              ENDIF
            ENDDO
            IF(MEVol.NE.0)THEN
              ur = ur + acs*pvs
              ui = ui + acs*pws
            ENDIF
          ENDIF
          potr2 = PV(mnulkl + 1) + VSL(mnukl + 1)*sop + VL(1)
     &            *ll*(ll + 1) - WN(nu)
          IF(l.EQ.k)THEN
            X4(kl) = st*(3.*step1)**(ll + 1)
            X5(kl) = st*(4.*step1)**(ll + 1)
            ur = ur + potr2
            ui = ui + PW(mnulkl + 1) + WSL(mnukl + 1)*sop
            IF(ll.EQ.1)VR1(kl) = 2.*st
          ENDIF
          VR2(kl) = ur*c
          VI2(kl) = ui*c
          ur = 0.
          ui = 0.
 
 
 
 
          c = CVNrpn(kl)
C     IF(NU.NE.NUN.AND.JO(NU).EQ.JO(NUN).AND. JO(NU).EQ.0) C=1.D0
          IF(NCA(nu).NE.NCA(nuk).AND.JO(nu).EQ.JO(nuk))
     &       ur = ur + PV(mnulkl + 2)*c
          IF(NCA(nu).NE.NCA(nuk).AND.JO(nu).EQ.JO(nuk))
     &       ui = ui + PW(mnulkl + 2)*c
 
 
C     IF(NU.NE.NUK.AND.JO(NU).EQ. JO(NUK)) UR=UR+PVV(MNUKL+2)*CVNRPN(KL)
C     IF(NU.NE.NUK.AND.JO(NU).EQ. JO(NUK)) UI=UI+PWW(MNUKL+2)*CVNRPN(KL)
 
 
 
          acs = 0.
          IF(las2.GE.2)THEN
            DO lll = 2, las2
              ll2 = las2 + lll + mnulkl
              pv1 = PV(ll2)
              pw1 = PW(ll2)
              IF(lll.EQ.2)pvs = pv1
              IF(lll.EQ.2)pws = pw1
              ll2 = k2 + lll
              a = CVNr(ll2)
              IF(npi1.NE.npi2)THEN
                ur = ur - a*pw1
                ui = ui + a*pv1
              ELSE
                ur = ur + a*pv1
                ui = ui + a*pw1
              ENDIF
              IF(MEPot.NE.1)THEN
                IF(lll.LE.3)THEN
                  lai = 3
                  laf = 5
                  IF(lll.EQ.3)lai = 1
                  IF(lll.EQ.3)laf = 9
                  DO la = lai, laf
                    lalas = lasc*(la - 1) + lll - 1
                    lp1 = lalas + las8
                    pv1 = PVC(lp1)
                    ll2 = kc2 + lalas
                    a = CVNc(ll2)
                    IF(lll.EQ.3.AND.la.EQ.1)acs = -a*CBEt0
                    IF(la.NE.1)THEN
                      IF(npi1.NE.npi2)THEN
                        ui = ui + a*pv1
                      ELSE
                        ur = ur + a*pv1
                      ENDIF
                    ENDIF
                  ENDDO
                ENDIF
              ENDIF
            ENDDO
            IF(MEVol.NE.0)THEN
              ur = ur + acs*pvs
              ui = ui + acs*pws
            ENDIF
          ENDIF
          ll2 = las2 + 1 + mnulkl
          potr2 = PV(ll2) + VSL(mnukl + 2)*sop + VL(2)*ll*(ll + 1)
     &            - WN(nu)
          IF(l.EQ.k)ur = ur + potr2
          IF(l.EQ.k)ui = ui + PW(ll2) + WSL(mnukl + 2)*sop
          VR3(kl) = ur*c2
          VI3(kl) = ui*c2
          ur = 0.
          ui = 0.
 
 
 
          c = CVNrpn(kl)
C     IF(NU.NE.NUN.AND.JO(NU).EQ.JO(NUN).AND. JO(NU).EQ.0) C=1.D0
          IF(NCA(nu).NE.NCA(nuk).AND.JO(nu).EQ.JO(nuk))
     &       ur = ur + PV(mnulkl + 3)*c
          IF(NCA(nu).NE.NCA(nuk).AND.JO(nu).EQ.JO(nuk))
     &       ui = ui + PW(mnulkl + 3)*c
 
 
C      IF(NU.NE.NUK.AND.JO(NU).EQ. JO(NUK)) UR=UR+PVV(MNUKL+3)*CVNRPN(KL)
C     IF(NU.NE.NUK.AND.JO(NU).EQ. JO(NUK)) UI=UI+PWW(MNUKL+3)*CVNRPN(KL)
 
C      WRITE(21,'(1X,3I6,1X,6(f8.4,1X))') k,l,ll,PV(LL2),VSL(MNUKL+2),
C     *SOP,VL(2)
 
C     IF(NU.NE.NUK.AND.JO(NU).EQ. JO(NUK))
C     *     WRITE(21,'(1X,11I6,1X,9(f8.4,1X))') NCLL,NU,NUK,KL, MNUKL,
C     *JNJ1(L), JNJ1(K),LNJ1(L),LNJ1(K), JO(NU),
C     *jO(NUK),PVV(MNUKL+3),CVNRPN(KL)
 
 
 
 
          acs = 0.
          IF(las2.GE.2)THEN
            DO lll = 2, las2
              ll2 = las2*2 + lll + mnulkl
              pv1 = PV(ll2)
              pw1 = PW(ll2)
              IF(lll.EQ.2)pvs = pv1
              IF(lll.EQ.2)pws = pw1
              ll2 = k2 + lll
              a = CVNr(ll2)
              IF(npi1.NE.npi2)THEN
                ur = ur - a*pw1
                ui = ui + a*pv1
              ELSE
                ur = ur + a*pv1
                ui = ui + a*pw1
              ENDIF
              IF(MEPot.NE.1)THEN
                IF(lll.LE.3)THEN
                  lai = 3
                  laf = 5
                  IF(lll.EQ.3)lai = 1
                  IF(lll.EQ.3)laf = 9
                  DO la = lai, laf
                    lalas = lasc*(la - 1) + lll - 1
                    lp1 = lalas + las8*2
                    pv1 = PVC(lp1)
                    ll2 = kc2 + lalas
                    a = CVNc(ll2)
                    IF(lll.EQ.3.AND.la.EQ.1)acs = -a*CBEt0
                    IF(la.NE.1)THEN
                      IF(npi1.NE.npi2)THEN
                        ui = ui + a*pv1
                      ELSE
                        ur = ur + a*pv1
                      ENDIF
                    ENDIF
                  ENDDO
                ENDIF
              ENDIF
            ENDDO
            IF(MEVol.NE.0)THEN
              ur = ur + acs*pvs
              ui = ui + acs*pws
            ENDIF
          ENDIF
          ll2 = las2*2 + 1 + mnulkl
          potr2 = PV(ll2) + VSL(mnukl + 3)*sop + VL(3)*ll*(ll + 1)
     &            - WN(nu)
          IF(l.EQ.k)ur = ur + potr2
          IF(l.EQ.k)ui = ui + PW(ll2) + WSL(mnukl + 3)*sop
          VR4(kl) = ur*c3
          VI4(kl) = ui*c3
          P(kl) = VR1(kl)
          R(kl) = VI1(kl)
        ENDDO
      ENDDO
      DO mm = 4, mf
        mm1 = (mm - 1)*las2
        mm2 = mm1 + 1
        mmc = las8*(mm - 1)
C     L-LINE OF INDEPENDENT SOLUTION
 
        DO l = 1, NCLl
          lpn1 = (l - 1)*NCLl
          kc1 = icll*(l - 1)
          k1 = (l - 1)*ncla - 1
          DO k = 1, NCLl
            VR5e(k) = 0.
            VI5e(k) = 0.
          ENDDO
          ll = LNJ1(l)
          jj = JNJ1(l)
          nu = NNJ1(l)
          mnulal = mnula*(nu - 1)
          mnul = mnu*(nu - 1)
          npi1 = KPJ1(l)
          cj1 = jj/2.
          sop = (cj1 - ll)*(cj1 + ll + 1) - spi2
 
C     N-NUMBER OF LINE OF SOLUTION COUPLED WITH L-LINE
 
          DO n = 1, NCLl
            ln = lpn1 + n
            kc2 = kc1 + las8*(n - 1)
            k2 = k1 + (n - 1)*las1
            npi2 = KPJ1(n)
            nun = NNJ1(n)
            mnulln = mnulal + mla*(nun - 1)
            mnuln = mnul + 300*(nun - 1)
            potr5 = PV(mnulln + mm2) + VSL(mnuln + mm)*sop + VL(mm)
     &              *ll*(ll + 1) - WN(nu)
            ur = 0.
            ui = 0.
            c = CVNrpn(ln)
C     IF(NU.NE.NUN.AND.JO(NU).EQ.JO(NUN).AND. JO(NU).EQ.0) C=1.D0
            IF(NCA(nu).NE.NCA(nun).AND.JO(nu).EQ.JO(nun))
     &         ur = ur + PV(mnulln + mm2)*c
            IF(NCA(nu).NE.NCA(nun).AND.JO(nu).EQ.JO(nun))
     &         ui = ui + PW(mnulln + mm2)*c
 
C     write (21,9797) mm,ur, ui,2.2, l,n
C 9797 format(i5, 3e12.5,2i5)
 
C      WRITE(21,'(1X,4I6,1X,6(f8.4,1X))')mm,l,n
 
C     WRITE(21,'(1X,4I6,1X,6(f8.4,1X))')mm,l,n,ll,
C     *PV(MNULLN+MM2),pw(MNULLN+MM2),VSL(MNUKL+mm),SOP,VL(mm)
 
C     IF(NU.NE.NUn.AND.JO(NU).EQ. JO(NUn))
C     *     WRITE(21,'(1X,12I5,1X,6(f8.4,1X))') NCLL,NU,NUn,ln, MNUln,
C     *JNJ1(L), JNJ1(n),LNJ1(L),LNJ1(n), JO(NU),
C     *jO(NUn),mm, PVV(MNUln+mm),Pww(MNUln+mm),CVNRPN(ln),WN(NU)
 
 
            acs = 0.
            IF(las2.GE.2)THEN
              DO lll = 2, las2
                ll2 = mm1 + lll + mnulln
                pv1 = PV(ll2)
                pw1 = PW(ll2)
                IF(lll.EQ.2)pvs = pv1
                IF(lll.EQ.2)pws = pw1
                ll2 = k2 + lll
                a = CVNr(ll2)
                IF(npi1.NE.npi2)THEN
                  ur = ur - a*pw1
                  ui = ui + a*pv1
                ELSE
                  ur = ur + a*pv1
C     DIAGONAL COUPLING FOR (P,N)
 
 
 
 
                  ui = ui + a*pw1
                ENDIF
                IF(MEPot.NE.1)THEN
                  IF(lll.LE.3)THEN
                    lai = 3
                    laf = 5
                    IF(lll.EQ.3)lai = 1
                    IF(lll.EQ.3)laf = 9
                    DO la = lai, laf
                      lalas = lasc*(la - 1) + lll - 1
                      lp1 = lalas + mmc
                      pv1 = PVC(lp1)
                      ll2 = kc2 + lalas
                      a = CVNc(ll2)
                      IF(lll.EQ.3.AND.la.EQ.1)acs = -a*CBEt0
                      IF(la.NE.1)THEN
                        IF(npi1.NE.npi2)THEN
                          ui = ui + a*pv1
                        ELSE
                          ur = ur + a*pv1
                        ENDIF
                      ENDIF
                    ENDDO
                  ENDIF
                ENDIF
              ENDDO
              IF(MEVol.NE.0)THEN
                ur = ur + acs*pvs
                ui = ui + acs*pws
              ENDIF
            ENDIF
            IF(l.EQ.n)ur = ur + potr5
            IF(l.EQ.n)ui = ui + PW(mm2 + mnulln) + WSL(mnuln + mm)*sop
 
 
C      write (21,9797) mm,ur, ui, potr5,l,n
 
            DO k = 1, NCLl
              kn = (k - 1)*NCLl + n
              x5n = X5(kn)
              y5n = Y5(kn)
              VR5e(k) = VR5e(k) + ur*x5n - ui*y5n
              VI5e(k) = VI5e(k) + ur*y5n + ui*x5n
            ENDDO
          ENDDO
 
C     K-NUMBER OF INDEPENDENT SOLUTION
 
          DO k = 1, NCLl
            nclk = (k - 1)*NCLl
            kl = nclk + l
            vr5l = VR5e(k)
            vi5l = VI5e(k)
            wr2 = VR2(kl)
            wr3 = VR3(kl)
            wr4 = VR4(kl)
            wi2 = VI2(kl)
            wi3 = VI3(kl)
            wi4 = VI4(kl)
            xw5 = X5(kl)
            yw5 = Y5(kl)
            x6l = 2.*xw5 - X4(kl)
     &            + st2*(299.*vr5l - 176.*wr4 + 194.*wr3 - 96.*wr2 + 
     &            19.*VR1(kl))
            y6l = 2.*yw5 - Y4(kl)
     &            + st2*(299.*vi5l - 176.*wi4 + 194.*wi3 - 96.*wi2 + 
     &            19.*VI1(kl))
            VR1(kl) = wr2
            VR2(kl) = wr3
            VR3(kl) = wr4
            VR4(kl) = vr5l
            VI1(kl) = wi2
            VI2(kl) = wi3
            VI3(kl) = wi4
            VI4(kl) = vi5l
            X4(kl) = xw5
            Y4(kl) = yw5
            X6(kl) = x6l
            Y6(kl) = y6l
          ENDDO
        ENDDO
        IF(mm/m*m.NE.mm)THEN
          IF(mm.EQ.mf)THEN
            DO nn = 1, ncl2
              FREm(5,nn) = x6l
              FIEm(5,nn) = y6l
              Q(nn) = X6(nn)
              T(nn) = Y6(nn)
            ENDDO
          ENDIF
        ELSEIF(mm.LT.2*m)THEN
          DO nn = 1, ncl2
            FREm(1,nn) = 0.
            FIEm(1,nn) = 0.
            FREm(2,nn) = x6l
            FIEm(2,nn) = y6l
            W(nn) = VR4(nn)
            V(nn) = VI4(nn)
          ENDDO
        ELSEIF(mm.EQ.2*m)THEN
          DO nn = 1, ncl2
            FREm(3,nn) = x6l
            FIEm(3,nn) = y6l
            S(nn) = VR4(nn)
            B(nn) = VI4(nn)
          ENDDO
        ELSEIF(mm.LE.3*m)THEN
          DO nn = 1, ncl2
            FREm(4,nn) = x6l
            FIEm(4,nn) = y6l
            G(nn) = VR4(nn)
            Z(nn) = VI4(nn)
            E(nn) = X4(nn)
            F(nn) = Y4(nn)
          ENDDO
        ENDIF
        DO l = 1, ncl2
          X5(l) = X6(l)
          Y5(l) = Y6(l)
        ENDDO
      ENDDO
      DO nb = 1, ncl2
        VR1(nb) = P(nb)
        VI1(nb) = R(nb)
        VR2(nb) = W(nb)
        VI2(nb) = V(nb)
        VR3(nb) = S(nb)
        VI3(nb) = B(nb)
        VR4(nb) = G(nb)
        VI4(nb) = Z(nb)
        X4(nb) = E(nb)
        Y4(nb) = F(nb)
        X5(nb) = Q(nb)
        Y5(nb) = T(nb)
      ENDDO
      st2 = STEp*STEp/240.
      nhtt = 5
      DO mm = m4, nh
        nhtt = nhtt + 1
        mm1 = (mm - 1)*las2
        mmc = las8*(mm - 1)
        mm2 = mm1 + 1
        DO l = 1, NCLl
          lpn1 = (l - 1)*NCLl
          kc1 = icll*(l - 1)
          k1 = (l - 1)*ncla - 1
          DO k = 1, NCLl
            VR5e(k) = 0.
            VI5e(k) = 0.
          ENDDO
          ll = LNJ1(l)
          jj = JNJ1(l)
          nu = NNJ1(l)
          mnulal = mnula*(nu - 1)
          mnul = mnu*(nu - 1)
          npi1 = KPJ1(l)
          cj1 = jj/2.
          sop = (cj1 - ll)*(cj1 + ll + 1) - spi2
          DO n = 1, NCLl
            ln = lpn1 + n
            npi2 = KPJ1(n)
            nun = NNJ1(n)
            mnulln = mnulal + mla*(nun - 1)
            mnuln = mnul + 300*(nun - 1)
            potr5 = PV(mnulln + mm2) + VSL(mnuln + mm)*sop + VL(mm)
     &              *ll*(ll + 1) - WN(nu)
            kc2 = kc1 + las8*(n - 1)
            k2 = k1 + (n - 1)*las1
            ur = 0.
            ui = 0.
 
            c = CVNrpn(ln)
C      IF(NU.NE.NUN.AND.JO(NU).EQ.JO(NUN).AND. JO(NU).EQ.0) C=1.D0
            IF(NCA(nu).NE.NCA(nun).AND.JO(nu).EQ.JO(nun))
     &         ur = ur + PV(mnulln + mm2)*c
            IF(NCA(nu).NE.NCA(nun).AND.JO(nu).EQ.JO(nun))
     &         ui = ui + PW(mnulln + mm2)*c
 
C           write (21,9797) mm,ur, ui,2.2, l,n
C
C      WRITE(21,'(1X,4I6,1X,6(f8.4,1X))')mm,l,n
 
C     WRITE(21,'(1X,4I6,1X,6(f8.4,1X))')mm,l,n,ll,
C     *PV(MNULLN+MM2),pw(MNULLN+MM2),VSL(MNUKL+mm),SOP,VL(mm)
 
C     IF(NU.NE.NUn.AND.JO(NU).EQ. JO(NUn))
C     *     WRITE(21,'(1X,12I5,1X,6(f8.4,1X))') NCLL,NU,NUn,ln, MNUln,
C     *JNJ1(L), JNJ1(n),LNJ1(L),LNJ1(n), JO(NU),
C     *jO(NUn),mm, PVV(MNUln+mm),Pww(MNUln+mm),CVNRPN(ln)
 
 
 
 
            acs = 0.
            IF(las2.GE.2)THEN
              DO lll = 2, las2
                ll2 = mm1 + lll + mnulln
                pv1 = PV(ll2)
                pw1 = PW(ll2)
                IF(lll.EQ.2)pvs = pv1
                IF(lll.EQ.2)pws = pw1
                ll2 = k2 + lll
                a = CVNr(ll2)
                IF(npi1.NE.npi2)THEN
                  ur = ur - a*pw1
                  ui = ui + a*pv1
                ELSE
                  ur = ur + a*pv1
 
C     DIAGONAL COUPLING FOR (P,N)
 
 
 
 
                  ui = ui + a*pw1
                ENDIF
                IF(MEPot.NE.1)THEN
                  IF(lll.LE.3)THEN
                    lai = 3
                    laf = 5
                    IF(lll.EQ.3)lai = 1
                    IF(lll.EQ.3)laf = 9
                    DO la = lai, laf
                      lalas = lasc*(la - 1) + lll - 1
                      lp1 = lalas + mmc
                      pv1 = PVC(lp1)
                      ll2 = kc2 + lalas
                      a = CVNc(ll2)
                      IF(lll.EQ.3.AND.la.EQ.1)acs = -a*CBEt0
                      IF(la.NE.1)THEN
                        IF(npi1.NE.npi2)THEN
                          ui = ui + a*pv1
                        ELSE
                          ur = ur + a*pv1
                        ENDIF
                      ENDIF
                    ENDDO
                  ENDIF
                ENDIF
              ENDDO
              IF(MEVol.NE.0)THEN
                ur = ur + acs*pvs
                ui = ui + acs*pws
              ENDIF
            ENDIF
            IF(l.EQ.n)ur = ur + potr5
            IF(l.EQ.n)ui = ui + PW(mm2 + mnulln) + WSL(mnuln + mm)*sop
            DO k = 1, NCLl
              kn = (k - 1)*NCLl + n
              x5n = X5(kn)
              y5n = Y5(kn)
              VR5e(k) = VR5e(k) + ur*x5n - ui*y5n
              VI5e(k) = VI5e(k) + ur*y5n + ui*x5n
            ENDDO
          ENDDO
          DO k = 1, NCLl
            nclk = (k - 1)*NCLl
            kl = nclk + l
            vr5l = VR5e(k)
            vi5l = VI5e(k)
            wr2 = VR2(kl)
            wr3 = VR3(kl)
            wr4 = VR4(kl)
            wi2 = VI2(kl)
            wi3 = VI3(kl)
            wi4 = VI4(kl)
            xw5 = X5(kl)
            yw5 = Y5(kl)
            x6l = 2.*xw5 - X4(kl)
     &            + st2*(299.*vr5l - 176.*wr4 + 194.*wr3 - 96.*wr2 + 
     &            19.*VR1(kl))
            y6l = 2.*yw5 - Y4(kl)
     &            + st2*(299.*vi5l - 176.*wi4 + 194.*wi3 - 96.*wi2 + 
     &            19.*VI1(kl))
            VR1(kl) = wr2
            VR2(kl) = wr3
            VR3(kl) = wr4
            VR4(kl) = vr5l
            VI1(kl) = wi2
            VI2(kl) = wi3
            VI3(kl) = wi4
            VI4(kl) = vi5l
            X4(kl) = xw5
            Y4(kl) = yw5
            X6(kl) = x6l
            Y6(kl) = y6l
            FREm(nhtt,kl) = x6l
            FIEm(nhtt,kl) = y6l
            IF(mm - nh + 2.LT.0)THEN
            ELSEIF(mm - nh + 2.EQ.0)THEN
              FRF1(k,l) = x6l
              FIF1(k,l) = y6l
            ELSEIF(mm.EQ.nh)THEN
              FRF2(k,l) = x6l
              FIF2(k,l) = y6l
            ENDIF
          ENDDO
        ENDDO
        DO l = 1, ncl2
          X5(l) = X6(l)
          Y5(l) = Y6(l)
        ENDDO
        IF(NCLl.GT.20)THEN
          IF(mm.EQ.m4 + 2)THEN
            DO k = 1, NCLl
              nclk = (k - 1)*NCLl
              kk = nclk + k
              ll1 = LNJ1(k)
              aa = (STEp*(mm + 1)*2.178281/(2.*ll + 1.))**(ll1 + 1)
     &             /X5(kk)
              DO l = 1, NCLl
                kl = nclk + l
                X4(kl) = X4(kl)*aa
                X5(kl) = X5(kl)*aa
                Y4(kl) = Y4(kl)*aa
                Y5(kl) = Y5(kl)*aa
                VR1(kl) = VR1(kl)*aa
                VR2(kl) = VR2(kl)*aa
                VR3(kl) = VR3(kl)*aa
                VR4(kl) = VR4(kl)*aa
                VI1(kl) = VI1(kl)*aa
                VI2(kl) = VI2(kl)*aa
                VI3(kl) = VI3(kl)*aa
                VI4(kl) = VI4(kl)*aa
              ENDDO
            ENDDO
          ENDIF
        ENDIF
      ENDDO
      RETURN
      END SUBROUTINE SOSIT
 
!---------------------------------------------------------------------------
C     *******************************************************
      SUBROUTINE MASCT
C     *******************************************************
      IMPLICIT DOUBLE PRECISION(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8, DIMENSION(40000) :: ABI, ABR, BMI, BMR
      REAL*8, DIMENSION(400) :: AUI, AUR
      REAL*8, DIMENSION(10) :: BET
      REAL*8, DIMENSION(200,200) :: CI, CR, FIF1, FIF2, FRF1, FRF2
      REAL*8, DIMENSION(180,10,200) :: CID, CRD
      REAL*8 :: CSS, EN, VR, WC, WD
      REAL*8, DIMENSION(20) :: EL, WN, WNK
      REAL*8, DIMENSION(20,90) :: FBI1, FBI2, FBR1, FBR2, FNI1, FNI2, 
     &                            FNR1, FNR2
      INTEGER :: INCc, INCr, INFor, LAS, MEApp, MECha, MECul, MEDis, 
     &           MEHam, MEHao, MEJob, MEPot, MEPri, MERel, MERip, MERrr, 
     &           MERzz, MESha, MESho, MESol, MEVol, NCLl, NJ, NMAx, NPD, 
     &           NSS, NUR
      INTEGER, DIMENSION(250) :: JNJ1, KPJ1, LNJ1, NNJ1
      COMMON /AUK   / AUR, AUI
      COMMON /CCMAT / CRD, CID
      COMMON /CMAT  / CR, CI
      COMMON /CVINV / BMR, BMI, ABR, ABI
      COMMON /ENA   / EN, EL, BET, NUR, NMAx, NPD, LAS
      COMMON /FBN   / FBR1, FBI1, FNR2, FNI2, FNR1, FNI1, FBR2, FBI2
      COMMON /FFV   / FRF1, FRF2, FIF1, FIF2
      COMMON /INF   / INFor
      COMMON /JNN   / CSS, INCc, NCLl, NSS, NJ, INCr
      COMMON /MENU  / MEJob, MEPot, MEHam, MECha, MEPri, MESol, MESha, 
     &                MESho, MEHao, MEApp, MEVol, MERel, MECul, MERzz, 
     &                MERrr, MEDis, MERip
      COMMON /POTB  / WNK, WN, VR, WC, WD
      COMMON /QNS1  / LNJ1, JNJ1, NNJ1, KPJ1
C
C Local variables
C
      REAL*8 :: aabi, aabr, bbmi, bbmr, c, cci, ccr, zn
      REAL*8 :: c1, c2, d1, d2, f1i, f2i, ff1, ff2, g1i, g2i, gg1, gg2, 
     &          rf1, rf2, rg1, rg2, zni
      INTEGER :: i, i1, ik, il, j, j1, jk, k, k1, l, l1, lk, nr
C
C*** End of declarations rewritten by SPAG
C
      CSS = 0.
      DO i = 1, NCLl
        nr = NNJ1(i)
        l = LNJ1(i) + 1
        IF(nr.GT.NMAx)THEN
          rf1 = FBR1(nr,l)
          rf2 = FBR2(nr,l)
          f2i = FBI2(nr,l)
          f1i = FBI1(nr,l)
          rg1 = FNR1(nr,l)
          rg2 = FNR2(nr,l)
          g1i = FNI1(nr,l)
          g2i = FNI2(nr,l)
          zni = rf2*g1i + f2i*rg1 - rf1*g2i - f1i*rg2
        ELSE
          ff1 = FBR1(nr,l)
          ff2 = FBR2(nr,l)
          gg1 = FNR1(nr,l)
          gg2 = FNR2(nr,l)
          zn = ff2*gg1 - ff1*gg2
        ENDIF
        DO k = 1, NCLl
          k1 = (k - 1)*NCLl + i
          c1 = FRF1(k,i)
          c2 = FRF2(k,i)
          d1 = FIF1(k,i)
          d2 = FIF2(k,i)
          IF(nr.GT.NMAx)THEN
            BMR(k1) = (d2*rf1 + c2*f1i - d1*rf2 - c1*f2i)/zni
            BMI(k1) = (d2*f1i - c2*rf1 - d1*f2i + c1*rf2)/zni
            ABR(k1) = (d2*rg1 + c2*g1i - d1*rg2 - c1*g2i)/zni - BMI(k1)
            ABI(k1) = (d2*g1i - c2*rg1 - d1*g2i + c1*rg2)/zni + BMR(k1)
          ELSE
            BMR(k1) = (c2*ff1 - c1*ff2)/zn
            BMI(k1) = (d2*ff1 - d1*ff2)/zn
            ABR(k1) = (c2*gg1 - c1*gg2)/zn - BMI(k1)
            ABI(k1) = (d2*gg1 - d1*gg2)/zn + BMR(k1)
          ENDIF
        ENDDO
      ENDDO
      INFor = 1
      CALL INMAT
      DO i = 1, NCLl
        i1 = (i - 1)*NCLl
        DO k = 1, NCLl
          nr = NNJ1(k)
          c = SQRT(WNK(nr)/WNK(1))
          c1 = SQRT(ABS(WN(nr)/WN(1)))
          ccr = 0.
          cci = 0.
          DO l = 1, NCLl
            il = i1 + l
            l1 = (l - 1)*NCLl
            lk = l1 + k
            aabr = ABR(il)
            bbmr = BMR(lk)
            aabi = ABI(il)
            bbmi = BMI(lk)
            ccr = ccr - aabr*bbmr + aabi*bbmi
            cci = cci - aabr*bbmi - aabi*bbmr
          ENDDO
          IF(i.LE.INCc.AND.k.LE.INCr)THEN
            CSS = CSS + (ccr*ccr + cci*cci)*c1
          ENDIF
C     IF(MEPRI.LT.99) PRINT 111,CCR,CCI
          CR(i,k) = ccr*c
          CI(i,k) = cci*c
          IF(i.LE.10)THEN
            CRD(NSS,i,k) = ccr*c
            CID(NSS,i,k) = cci*c
          ENDIF
        ENDDO
      ENDDO
      IF(MESol.NE.2)THEN
        DO i = 1, NCLl
          i1 = (i - 1)*NCLl
          DO k = 1, NCLl
            ik = i1 + k
            ABR(ik) = BMR(ik)
            ABI(ik) = BMI(ik)
          ENDDO
        ENDDO
        INFor = 2
        CALL INMAT
        DO i = 1, NCLl
          i1 = (i - 1)*NCLl
          DO k = 1, NCLl
            ik = i1 + k
            AUR(ik) = 0.
            AUI(ik) = 0.
            DO j = 1, NCLl
              j1 = (j - 1)*NCLl
              jk = j1 + k
              AUR(ik) = AUR(ik) + CR(i,j)*ABR(jk) - CI(i,j)*ABI(jk)
              AUI(ik) = AUI(ik) + CI(i,j)*ABR(jk) + CR(i,j)*ABI(jk)
            ENDDO
          ENDDO
        ENDDO
      ENDIF
C      DO 11 I=1,NCLL
C      I1=(I-1)*NCLL
C      DO 11 K=1,NCLL
C      IK=I1+K
C      ABR(IK)=AUR(IK)
C      ABI(IK)=AUI(IK)
C   11 CONTINUE
C      INFOR=3
C      CALL INMAT
C      DO 12 I=1,NCLL
C      I1=(I-1)*NCLL
C      DO 12 K=1,NCLL
C      IK=I1+K
C      AUR(IK)=ABR(IK)
C      AUI(IK)=ABI(IK)
C   12 CONTINUE
C     IF(MEPRI.NE.99) PRINT 967,CR(1,2),CI(1,2)
C 967 FORMAT(1X,'M1',2D20.7)
C     WRITE (16) CR,CI,LNJ1,JNJ1,NNJ1
C     IF(MEPRI.NE.99) PRINT 9998,
C    *   (LNJ1(I8),JNJ1(I8),NNJ1(I8),I8=1,NCLL),NCLL
C9999 FORMAT(10X,'MASCT')
C7999 FORMAT(10X,'MASC1')
C9998 FORMAT(3I5)
      RETURN
      END SUBROUTINE MASCT
 
!---------------------------------------------------------------------------
C     *******************************************************
      SUBROUTINE INMAT
C     *******************************************************
      IMPLICIT DOUBLE PRECISION(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8, DIMENSION(40000) :: ABI, ABR, BMI, BMR, FI1, FI2, FR1, FR2
      REAL*8 :: CSS
      INTEGER :: INCc, INCr, LG, NCLl, NJ, NSS
      COMMON /CVINV / BMR, BMI, ABR, ABI
      COMMON /FV    / FR1, FR2, FI1, FI2
      COMMON /IW    / LG
      COMMON /JNN   / CSS, INCc, NCLl, NSS, NJ, INCr
C
C Local variables
C
      REAL*8 :: ab, ffr1, fr
      INTEGER :: k, k1, kl, km, l, m, m1, mn, n, nl
C
C*** End of declarations rewritten by SPAG
C
      LG = NCLl
      DO k = 1, NCLl
        k1 = (k - 1)*NCLl
        DO l = 1, NCLl
          kl = k1 + l
          FI1(kl) = ABI(kl)
          FI2(kl) = ABR(kl)
        ENDDO
      ENDDO
      CALL INVER
      DO k = 1, NCLl
        k1 = (k - 1)*NCLl
        DO l = 1, NCLl
          kl = k1 + l
          FR2(kl) = FI2(kl)
          ffr1 = ABR(kl)
          DO m = 1, NCLl
            km = k1 + m
            ab = ABI(km)
            m1 = (m - 1)*NCLl
            DO n = 1, NCLl
              mn = m1 + n
              nl = (n - 1)*NCLl + l
              ffr1 = ffr1 + ab*FI2(mn)*FI1(nl)
            ENDDO
          ENDDO
          FR1(kl) = ffr1
        ENDDO
      ENDDO
      DO k = 1, NCLl
        k1 = (k - 1)*NCLl
        DO l = 1, NCLl
          kl = k1 + l
          FI2(kl) = FR1(kl)
        ENDDO
      ENDDO
      CALL INVER
      DO k = 1, NCLl
        k1 = (k - 1)*NCLl
        DO l = 1, NCLl
          kl = k1 + l
          ABR(kl) = FI2(kl)
          ab = 0.
          DO m = 1, NCLl
            km = k1 + m
            fr = FR2(km)
            m1 = (m - 1)*NCLl
            DO n = 1, NCLl
              mn = m1 + n
              nl = (n - 1)*NCLl + l
              ab = ab - fr*FI1(mn)*FI2(nl)
            ENDDO
          ENDDO
          ABI(kl) = ab
        ENDDO
      ENDDO
      RETURN
      END SUBROUTINE INMAT
 
!---------------------------------------------------------------------------
C     *******************************************************
      SUBROUTINE INVER
C     *******************************************************
      IMPLICIT DOUBLE PRECISION(a - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8, DIMENSION(40000) :: FI1, FI2, FR1, FR2
      INTEGER :: INFor, LG, MEApp, MECha, MECul, MEDis, MEHam, MEHao, 
     &           MEJob, MEPot, MEPri, MERel, MERip, MERrr, MERzz, MESha, 
     &           MESho, MESol, MEVol
      COMMON /FV    / FR1, FR2, FI1, FI2
      COMMON /INF   / INFor
      COMMON /IW    / LG
      COMMON /MENU  / MEJob, MEPot, MEHam, MECha, MEPri, MESol, MESha, 
     &                MESho, MEHao, MEApp, MEVol, MERel, MECul, MERzz, 
     &                MERrr, MEDis, MERip
C
C Local variables
C
      REAL*8 :: a, aa, elem, piv, sumd, sumn
      REAL*8, DIMENSION(40000) :: aiamai, amai, amm
      REAL*8 :: eps
      INTEGER :: i, i1, i1g, ij, ik, im, iter, j, k, k1, k1g, kj, 
     &           kk, km, lg2, m
C
C*** End of declarations rewritten by SPAG
C
C     DET=1.0
      eps = 1.0D-7
      lg2 = LG*LG
      DO i = 1, lg2
        amm(i) = FI2(i)
      ENDDO
      DO k = 1, LG
        k1g = (k - 1)*LG
        kk = k1g + k
C     DET=DET*FI2(KK)
        piv = 1.0D+00/FI2(kk)
        DO j = 1, LG
          kj = k1g + j
          FI2(kj) = FI2(kj)*piv
        ENDDO
        FI2(kk) = piv
        DO i = 1, LG
          i1g = (i - 1)*LG
          ik = i1g + k
          IF(i.NE.k)THEN
            elem = FI2(ik)
            FI2(ik) = 0.0
            DO j = 1, LG
              ij = i1g + j
              kj = k1g + j
              FI2(ij) = FI2(ij) - elem*FI2(kj)
            ENDDO
          ENDIF
        ENDDO
      ENDDO
      sumd = 0.D+00
      sumn = 0.D+00
      DO i = 1, LG
        i1 = LG*(i - 1)
        DO m = 1, LG
          aa = 0.D+00
          DO k = 1, LG
            k1 = LG*(k - 1)
            ik = i1 + k
            km = k1 + m
            a = amm(ik)*FI2(km)
            aa = aa + a
          ENDDO
          IF(i.EQ.m)sumd = sumd + ABS(aa)
          IF(i.NE.m)sumn = sumn + ABS(aa)
        ENDDO
      ENDDO
      sumd = sumd/LG
C     IF(MEPRI.NE.99) PRINT 99,SUMD,SUMN
C  99 FORMAT(10X,'M ATRIX ',2D20.8)
      IF(sumn.GT.eps.OR.ABS(sumd - 1.D0).GE.eps)THEN
C
C     ITTERATIONS TO MAKE INVERTED MATRIX ACCURATE
C
        iter = 0
    5   iter = iter + 1
        DO i = 1, LG
          i1 = LG*(i - 1)
          DO m = 1, LG
            aa = 0.D+00
            DO k = 1, LG
              k1 = LG*(k - 1)
              ik = i1 + k
              km = k1 + m
              a = amm(ik)*FI2(km)
              aa = aa + a
            ENDDO
            im = i1 + m
            amai(im) = aa
          ENDDO
        ENDDO
        DO i = 1, LG
          i1 = LG*(i - 1)
          DO m = 1, LG
            aa = 0.D+00
            DO k = 1, LG
              k1 = LG*(k - 1)
              ik = i1 + k
              km = k1 + m
              a = FI2(ik)*amai(km)
              aa = aa + a
            ENDDO
            im = i1 + m
            aiamai(im) = aa
          ENDDO
        ENDDO
        DO i = 1, lg2
          FI2(i) = 2.D+00*FI2(i) - aiamai(i)
        ENDDO
        sumd = 0.D+00
        sumn = 0.D+00
        DO i = 1, LG
          i1 = LG*(i - 1)
          DO m = 1, LG
            aa = 0.D+00
            DO k = 1, LG
              k1 = LG*(k - 1)
              ik = i1 + k
              km = k1 + m
              a = amm(ik)*FI2(km)
              aa = aa + a
            ENDDO
            IF(i.EQ.m)sumd = sumd + ABS(aa)
            IF(i.NE.m)sumn = sumn + ABS(aa)
          ENDDO
        ENDDO
        sumd = sumd/LG
        IF(sumn.GT.eps.OR.ABS(sumd - 1.D0).GE.eps)THEN
          IF(iter.LE.2)GOTO 5
          IF(MEPri.NE.99)PRINT 1010, sumn, sumd, INFor, iter
          WRITE(21,1010)sumn, sumd, INFor, iter
 1010     FORMAT(10X,'WARNING! MATRIX IS POORLY INVERTED'/5X,
     &           'SUM OF NON-DIAG. ELEM-S=',D11.5,', DIAGONAL=',D11.5,
     &           ',INFOR=',I2,',ITER=',I2)
        ENDIF
      ENDIF
      RETURN
      END SUBROUTINE INVER
 
!---------------------------------------------------------------------------
C     *******************************************************
      SUBROUTINE PLEGA
C     *******************************************************
      IMPLICIT DOUBLE PRECISION(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8, DIMENSION(20,180) :: COEf
      REAL*8, DIMENSION(20,150) :: DISc
      INTEGER :: LKK, MTEt
      REAL*8, DIMENSION(180,150) :: PL
      REAL*8, DIMENSION(150) :: TET
      COMMON /DISCAN/ DISc, PL, COEf, LKK
      COMMON /DISK  / TET, MTEt
C
C Local variables
C
      REAL*8 :: ak, teta
      INTEGER :: k, m
C
C*** End of declarations rewritten by SPAG
C
      DO m = 1, MTEt
        teta = TET(m)*4.D0*ATAN(1.D0)/180.D0
        PL(1,m) = 1.D0
        PL(2,m) = COS(teta)
        DO k = 3, 180
          ak = k - 1.
          PL(k,m) = ((2.D0*ak - 1.D0)*COS(teta)*PL(k - 1,m)
     &              - (ak - 1.D0)*PL(k - 2,m))/ak
        ENDDO
      ENDDO
      RETURN
      END SUBROUTINE PLEGA
 
!---------------------------------------------------------------------------
C     *******************************************************
      SUBROUTINE DISCA
C     *******************************************************
      IMPLICIT DOUBLE PRECISION(a - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: AKG, CONz, CSS, EN, ETA, VR, W, WC, WD
      REAL*8, DIMENSION(10) :: BET
      REAL*8, DIMENSION(200,200) :: CI, CR
      REAL*8, DIMENSION(180,10,200) :: CID, CRD
      REAL*8, DIMENSION(20,180) :: COEf
      REAL*8, DIMENSION(90) :: COEfi, COEfr
      REAL*8, DIMENSION(20,90) :: COPh
      REAL*8, DIMENSION(20,150) :: DISc
      REAL*8, DIMENSION(20) :: EL, WN, WNK
      INTEGER, DIMENSION(180) :: INC, INR, JS
      INTEGER :: INCc, INCr, J, J1, J2, JA, JB, JC, JD, JE, JF, KODma, 
     &           LAS, LKK, LLMa, M, M1, M2, MTEt, NCLl, NCMa, NJ, NMAx, 
     &           NPD, NSMa, NSS, NUF, NUI, NUR
      INTEGER, DIMENSION(180,250) :: JNJ, LNJ, NNJ
      INTEGER, DIMENSION(250) :: JNJ1, KPJ1, LNJ1, NNJ1
      INTEGER, DIMENSION(20) :: JO, KO, NCA, NPO
      REAL*8, DIMENSION(180,150) :: PL
      REAL*8, DIMENSION(150) :: TET
      COMMON /CCMAT / CRD, CID
      COMMON /CMAT  / CR, CI
      COMMON /COUL  / CONz, ETA, COPh
      COMMON /COULCO/ COEfr, COEfi
      COMMON /DISCAN/ DISc, PL, COEf, LKK
      COMMON /DISK  / TET, MTEt
      COMMON /ENA   / EN, EL, BET, NUR, NMAx, NPD, LAS
      COMMON /JNN   / CSS, INCc, NCLl, NSS, NJ, INCr
      COMMON /KG    / J1, J2, M1, M2, J, M, AKG
      COMMON /NCLMA / LLMa, NCMa, NSMa, KODma
      COMMON /NU    / NUI, NUF
      COMMON /POTB  / WNK, WN, VR, WC, WD
      COMMON /QNB   / JO, NPO, KO, NCA
      COMMON /QNS1  / LNJ1, JNJ1, NNJ1, KPJ1
      COMMON /QNSB  / INC, INR, JS
      COMMON /QNSBD / LNJ, JNJ, NNJ
      COMMON /RACB  / JA, JB, JC, JD, JE, JF, W
C
C Local variables
C
      REAL*8 :: a, a1, a2, aa, aac, aci, aci1, aci2, acii, acin, acr, 
     &          acr1, acr2, acrn, acrr, alst2, argc, b, coe, coi, coii, 
     &          cor, corr, couli, coulr, sit22, teta
      INTEGER :: IABS
      INTEGER :: ic, ir, j1f, j1i, j2f, j2i, js1, js2, k1, k2, l, l1, 
     &           l1f, l1i, l2f, l2i, lfo, lio, lk, ll, lll, lln, ln, n, 
     &           n1c, n1f, n1i, n1r, n2c, n2f, n2i, n2r, nu1, nu2
C
C*** End of declarations rewritten by SPAG
C
      CALL PLEGA
      LKK = 1
      ETA = CONz/WNK(1)
      DO n = 1, NMAx
        DO l = 1, 180
          IF(l.LE.90.AND.n.EQ.1)COEfr(l) = 0.
          IF(l.LE.90.AND.n.EQ.1)COEfi(l) = 0.
          COEf(n,l) = 0.
        ENDDO
        DO M = 1, MTEt
          DISc(n,M) = 0.
        ENDDO
      ENDDO
      ic = JO(1)
      a1 = 200.D0*WN(1)*(ic + 1.D0)
      DO k1 = 1, NJ
        js1 = JS(k1)
        n1i = INC(k1)
        n1f = INR(k1)
        DO n1c = 1, n1i
          l1i = LNJ(k1,n1c)
          j1i = JNJ(k1,n1c)
          corr = COS(2.D0*COPh(1,l1i + 1))
          coii = SIN(2.D0*COPh(1,l1i + 1))
          acrr = CRD(k1,n1c,n1c)
          acii = CID(k1,n1c,n1c)
          acrn = acrr*corr - acii*coii
          acin = acrr*coii + acii*corr
          DO n1r = 1, n1f
            l1f = LNJ(k1,n1r)
            j1f = JNJ(k1,n1r)
            nu1 = NNJ(k1,n1r)
            cor = COS(COPh(1,l1i + 1) + COPh(nu1,l1f + 1))
            coi = SIN(COPh(1,l1i + 1) + COPh(nu1,l1f + 1))
            IF(KODma.GT.0)THEN
              IF(nu1.LT.NUI)CYCLE
              IF(nu1.GT.NUF)CYCLE
            ELSE
              IF(nu1.LT.NUI)CYCLE
              IF(nu1.GT.NUF)GOTO 5
            ENDIF
            ir = JO(nu1)
            acr = CRD(k1,n1c,n1r)
            aci = CID(k1,n1c,n1r)
            acr1 = acr*cor - aci*coi
            aci1 = acr*coi + aci*cor
            a = (js1 + 1.D0)*SQRT((j1i + 1.D0)*(j1f + 1.D0))/a1*( - 1)
     &          **((ir - ic)/2)
            a2 = a
            DO k2 = k1, NJ
              IF(k2.NE.k1)a2 = 2.D0*a
              js2 = JS(k2)
              n2i = INC(k2)
              n2f = INR(k2)
              DO n2c = 1, n2i
                l2i = LNJ(k2,n2c)
                j2i = JNJ(k2,n2c)
                DO n2r = 1, n2f
                  nu2 = NNJ(k2,n2r)
                  IF(KODma.GT.0)THEN
                    IF(nu2.LT.nu1)CYCLE
                    IF(nu2.GT.nu1)CYCLE
                  ELSE
                    IF(nu2.LT.nu1)CYCLE
                    IF(nu2.GT.nu1)EXIT
                  ENDIF
                  l2f = LNJ(k2,n2r)
                  j2f = JNJ(k2,n2r)
                  cor = COS(COPh(1,l2i + 1) + COPh(nu1,l2f + 1))
                  coi = SIN(COPh(1,l2i + 1) + COPh(nu1,l2f + 1))
                  ln = IABS(l2f - l1f) + 1
                  lk = l1f + l2f + 1
                  ll = l1i + l2i + 1
                  lll = ll + lk
                  IF(lll/2*2.EQ.lll)THEN
                    lln = IABS(l1i - l2i) + 1
                    IF(lln.GT.ln)ln = lln
                    IF(ll.LT.lk)lk = ll
                    lio = IABS(j1i - j2i)/2 + 1
                    lfo = (j1i + j2i)/2 + 1
                    lln = IABS(j1f - j2f)/2 + 1
                    ll = (j1f + j2f)/2 + 1
                    IF(lln.GT.lio)lio = lln
                    IF(ll.LT.lfo)lfo = ll
                    lln = IABS(js1 - js2)/2 + 1
                    ll = (js1 + js2)/2 + 1
                    IF(lln.GT.lio)lio = lln
                    IF(ll.LT.lfo)lfo = ll
                    IF(lio.GT.ln)ln = ln + (lio - ln + 1)/2*2
                    IF(lfo.LT.lk)lk = lk - (lk - lfo + 1)/2*2
                    IF(ln.LE.lk)THEN
                      acr = CRD(k2,n2c,n2r)
                      aci = CID(k2,n2c,n2r)
                      acr2 = acr*cor - aci*coi
                      aci2 = acr*coi + aci*cor
                      b = a2*(js2 + 1.D0)
     &                    *SQRT((j2i + 1.D0)*(j2f + 1.D0))
     &                    *(acr1*acr2 + aci1*aci2)*( - 1)
     &                    **((j1i + j2i + j1f + j2f)/2)
                      IF(lk.GT.LKK)LKK = lk
                      DO l = ln, lk, 2
                        l1 = l - 1
                        JA = js1
                        JB = j1f
                        JE = js2
                        JD = j2f
                        JC = ir
                        JF = l1*2
                        CALL RACAH
                        aa = b*W
                        JB = j1i
                        JD = j2i
                        JC = ic
                        CALL RACAH
                        aa = aa*W
                        J1 = j1i
                        J2 = j2i
                        M1 = 1
                        M2 = -1
                        J = JF
                        M = 0
                        CALL KLEGO
                        aa = aa*AKG
                        J1 = j1f
                        J2 = j2f
                        CALL KLEGO
                        aa = aa*AKG
                        DO M = 1, MTEt
                          DISc(nu1,M) = DISc(nu1,M) + PL(l,M)*aa
                        ENDDO
                        COEf(nu1,l) = COEf(nu1,l) + aa
                      ENDDO
                    ENDIF
                  ENDIF
                ENDDO
              ENDDO
            ENDDO
          ENDDO
          IF(ETA.NE.0.)THEN
            IF(NUI.LE.1)THEN
              COEfr(l1i + 1) = COEfr(l1i + 1) + acrn/a1*(js1 + 1.D0)
     &                         /(2.D0*l1i + 1.D0)/a1*2
              COEfi(l1i + 1) = COEfi(l1i + 1) + acin/a1*(js1 + 1.D0)
     &                         /(2.D0*l1i + 1.D0)/a1*2
              DO M = 1, MTEt
                teta = TET(M)*4.D0*ATAN(1.D0)/180.D0
                sit22 = SIN(teta/2.D0)**2
                alst2 = LOG(SIN(teta/2.D0))
                argc = 2.D0*(COPh(1,1) - ETA*alst2)
C     COULOMB AMPLITUDE * by 2K
                coulr = -ETA/sit22*COS(argc)
                couli = -ETA/sit22*SIN(argc)
C     Cmat*Frez*+Frex*Cmat*=2.*ACC
                aac = (coulr*acrn + couli*acin)*(js1 + 1.D0)
                DISc(1,M) = DISc(1,M) + PL(l1i + 1,M)/a1*aac
              ENDDO
            ENDIF
          ENDIF
    5   ENDDO
      ENDDO
      IF(ETA.NE.0.)THEN
        IF(NUI.LE.1)THEN
          DO M = 1, MTEt
            teta = TET(M)*4.D0*ATAN(1.D0)/180.D0
            DISc(1,M) = DISc(1,M) + ETA**2/a1/2.D0/SIN(teta/2.D0)
     &                  **4*(ic + 1.D0)
          ENDDO
        ENDIF
      ENDIF
      DO n = NUI, NUF
        coe = COEf(n,1)
        DO l = 1, LKK
          coe = 1.D0
          COEf(n,l) = COEf(n,l)/coe/(2*l - 1)
        ENDDO
      ENDDO
      RETURN
      END SUBROUTINE DISCA
 
!---------------------------------------------------------------------------
C     ***********************************************************
      SUBROUTINE ECISS
C     ***********************************************************
      IMPLICIT DOUBLE PRECISION(a - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: ABI1, ABI2, ABR1, ABR2, AC, AD, AIA, ALF, AMB0, AMG0, 
     &          ANEu, ANI1, ANI2, ANR1, ANR2, AR, ARA, AS, ASP, AT, AW, 
     &          AZ, BET0, BET4, CBEt0, CFI1, CFI2, CFR1, CFR2, CIC, CRC, 
     &          CSS, EN, GAM0, GAMde, GSHape, HW, RC, RD, RK, RR, RS, 
     &          RW, RZ, STEp, VR, WC, WD, ZNUc
      REAL*8, DIMENSION(20) :: APA, BPA, CY, EL, WN, WNK
      REAL*8, DIMENSION(400) :: AUI, AUR
      REAL*8, DIMENSION(10) :: BET
      REAL*8, DIMENSION(200,200) :: CI, CR
      REAL*8, DIMENSION(180,10,200) :: CID, CRD
      REAL*8, DIMENSION(200) :: CIH, CII, CIP, CIS, CRH, CRI, CRP, CRS
      REAL*8, DIMENSION(40,200) :: CIT, CRT
      REAL*8, DIMENSION(40) :: CT
      REAL*8, DIMENSION(720000) :: CVNc
      REAL*8, DIMENSION(160000) :: CVNr
      REAL*8, DIMENSION(40000) :: CVNrpn
      REAL*8, DIMENSION(20,90) :: FBI1, FBI2, FBR1, FBR2, FNI1, FNI2, 
     &                            FNR1, FNR2
      REAL*8, DIMENSION(300,200) :: FIA, FIH, FII, FRA, FRH, FRI
      REAL*8, DIMENSION(300,40000) :: FIEm, FREm
      REAL*8, DIMENSION(300,11) :: FIS, FRS
      INTEGER :: INCc, INCr, JSO, JSS, LAS, LPA, MEApp, MECha, MECul, 
     &           MEDis, MEHam, MEHao, MEJob, MEPot, MEPri, MERel, MERip, 
     &           MERrr, MERzz, MESha, MESho, MESol, MEVol, MPA, NCLl, 
     &           NECi, NH1, NHI, NJ, NMAx, NN1, NPD, NPIo, NPIs, NSPi, 
     &           NSS, NUR
      INTEGER, DIMENSION(250) :: JNJ1, KPJ1, LNJ1, NNJ1
      INTEGER, DIMENSION(180) :: JNO, LNO, NS1
      INTEGER, DIMENSION(20) :: JO, KO, NCA, NPO
      INTEGER, DIMENSION(200) :: NPAd
      REAL*8, DIMENSION(600000) :: PI, PR
      REAL*8, DIMENSION(120000) :: PII, PRR, PSL, PVV, PWW, WPSl
      REAL*8, DIMENSION(300) :: PL
      REAL*8, DIMENSION(5400) :: PRC, PVC
      COMMON /ABCOUL/ PVC, PRC, CVNc
      COMMON /ABEC  / PL, PSL, PR, PI, WPSl
      COMMON /AUK   / AUR, AUI
      COMMON /CCMAT / CRD, CID
      COMMON /CMAT  / CR, CI
      COMMON /CRIC  / CRH, CIH, CRI, CII, CRS, CIS
      COMMON /CRIT  / CRP, CIP, CRT, CIT, NPAd
      COMMON /CV    / CVNr
      COMMON /CVOL  / CBEt0
      COMMON /CVPN  / CVNrpn
      COMMON /ECI   / NECi, NHI
      COMMON /ENA   / EN, EL, BET, NUR, NMAx, NPD, LAS
      COMMON /FBN   / FBR1, FBI1, FNR2, FNI2, FNR1, FNI1, FBR2, FBI2
      COMMON /FIRA  / FRA, FIA
      COMMON /FUEC  / FREm, FIEm
      COMMON /FUNC  / FRH, FIH, FRI, FII, FRS, FIS
      COMMON /JNN   / CSS, INCc, NCLl, NSS, NJ, INCr
      COMMON /LNP   / JSS, NPIs, NPIo, JSO, NN1, LNO, NS1, JNO, NSPi
      COMMON /MATT  / ABR1, ABI1, ANR1, ANI1, ABR2, ABI2, ANR2, ANI2, 
     &                CFR1, CFI1, CFR2, CFI2, CRC, CIC, ARA, AIA
      COMMON /MENU  / MEJob, MEPot, MEHam, MECha, MEPri, MESol, MESha, 
     &                MESho, MEHao, MEApp, MEVol, MERel, MECul, MERzz, 
     &                MERrr, MEDis, MERip
      COMMON /PAD   / CT, APA, BPA, CY, MPA, LPA
      COMMON /POTB  / WNK, WN, VR, WC, WD
      COMMON /POTPN / PVV, PWW, PRR, PII
      COMMON /QNB   / JO, NPO, KO, NCA
      COMMON /QNS1  / LNJ1, JNJ1, NNJ1, KPJ1
      COMMON /RAD   / RR, RC, RD, RW, RS, AR, AC, AW, AD, AS, ALF, AT, 
     &                ANEu, RZ, ZNUc, ASP, AZ
      COMMON /SHEM1 / HW, AMG0, AMB0, GAM0, BET0, BET4, GAMde, GSHape
      COMMON /STR   / STEp, RK, NH1
C
C Local variables
C
      REAL*8 :: a, acon, acs, akw, al, am, ben, btn, c, chi, chr, cj1, 
     &          con, cong, coupl, csi, csid, csur, epeci, fi, fr, poi, 
     &          por, poti, potr, pv1, pvs, pws, qsi1, qsi2, qsi3, qsr1, 
     &          qsr2, qsr3, sop, spi2, st, st12, st2, st6, sumc, sumi, 
     &          sumt, ui, ur, wi1, wi2, wi3, wr1, wr2, wr3, zn
      INTEGER :: ic, ic1, icll, ii, ij, ir, it, iter, jj, jll, jm, k1, 
     &           k2, kc1, kc2, l, l1, la, lac, laf, lai, lalas, las1, 
     &           las2, las8, lasc, lc2, lf, li, lic, liso, lisor, ll, 
     &           ll2, lll, llll, llnc, lm, lp, lp1, lpa1, mall, mla, 
     &           mll, mnl, mnll, mnllir, mnu, mnula, mnull, 
     &           mnulll, ncla, npi1, npi2, ns, nslu, nu, nuir
C
C*** End of declarations rewritten by SPAG
C
C     BELOW CARD IS NECESSARY IF MEMORY IS LESS THAN 32Mb
C     NEXT TWO CARDS JUST TO OVERCOME THE ERROR OR  MICROSOFT FPS COMPILER
C     v.1.00, YOU CAN DELETE THEM USING OTHER COMPILERS
      IF(MEPri.NE.99)PRINT 1010, (WNK(ii),ii = 1,NUR)
 1010 FORMAT(5E12.5)
      spi2 = ASP*(ASP + 1.)
      cong = (JSS + 1)/(JO(1) + 1.)
      con = 12.566372*cong/WN(1)/100./(NSPi + 1)
      IF(MEHam.EQ.1)akw = 1. + BET(2)*2.5
      IF(MEHam.NE.1)akw = 1. + BET0*2.5
      nslu = 1
      IF(MEJob.GT.3)nslu = 2
      llnc = INCc
      NECi = 1
   10 IF(nslu.EQ.2)akw = 1.
      IF(nslu.EQ.2)llnc = NCLl
      IF(MEPot.EQ.1)THEN
        las2 = (LAS + 2)/2
        las1 = las2 - 1
      ELSE
        las2 = LAS + 1
        las1 = LAS
      ENDIF
      ncla = NCLl*las1
      las8 = 9
      lasc = 1
      IF(las2.GE.3)las8 = 18
      IF(las2.GE.3)lasc = 2
      icll = NCLl*las8
      mnula = 300*NUR*las2
      mla = 300*las2
      mnu = 300*NUR
      st = 1.
      st2 = STEp*STEp
      st12 = st2/12.
      st6 = st2/6.
      DO l = 1, llnc
        lll = (l - 1)*(ncla + las1) - 1
        lc2 = (l - 1)*(icll + las8)
        ll = LNJ1(l)
        st = 1./(18.D+00)**ll
        jj = JNJ1(l)
        nu = NNJ1(l)
        mnulll = mnula*(nu - 1) + mla*(nu - 1)
        mnull = mnu*(nu - 1) + 300*(nu - 1)
C       IF((EN-EL(NU)/AT*(AT+ANEU)).LT.0) ST=EXP(-5.*LL)
        cj1 = jj/2.
        sop = (cj1 - ll)*(cj1 + ll + 1) - spi2
        FRH(1,l) = 0.
        FIH(1,l) = 0.
        FRH(2,l) = st*STEp**(ll + 1)
        FIH(2,l) = 0.
        qsr1 = 0.
        qsi1 = 0.
        IF(ll.EQ.1)qsr1 = -st*st6
        potr = PSL(mnull + 2)*sop + PL(2)*ll*(ll + 1) - WN(nu)
     &         + PR(mnulll + las2 + 1)
        poti = PI(mnulll + las2 + 1)*akw + WPSl(mnull + 2)*sop
        li = las2 + 2 + mnulll
        lf = 2*las2 + mnulll
        acs = 0.
        IF(li.LE.lf)THEN
          DO la = li, lf
            a = CVNr(lll + la - las2 - mnulll)
            potr = potr + PR(la)*a
            poti = poti + PI(la)*a*akw
            IF(MEPot.NE.1)THEN
              llll = la - li + 1
              IF(llll.LE.2)THEN
                lai = 3
                laf = 5
                IF(llll.EQ.2)lai = 1
                IF(llll.EQ.2)laf = 9
                IF(llll.EQ.1)pvs = PR(la)
                IF(llll.EQ.1)pws = PI(la)*akw
                DO lac = lai, laf
                  lalas = lasc*(lac - 1) + llll
                  lp1 = lalas + las8
                  pv1 = PRC(lp1)
                  ll2 = lc2 + lalas
                  a = CVNc(ll2)
                  IF(llll.EQ.2.AND.lac.EQ.1)acs = -a*CBEt0
                  IF(lac.NE.1)THEN
                    potr = potr + a*pv1
                  ENDIF
                ENDDO
              ENDIF
            ENDIF
          ENDDO
          IF(MEVol.NE.0)THEN
            potr = potr + acs*pvs
            poti = poti + acs*pws
          ENDIF
        ENDIF
        qsr2 = FRH(2,l)*(1. - st12*potr)
        qsi2 = -FRH(2,l)*st12*poti
        poi = poti + st6*potr*poti
        por = potr + st12*(potr*potr - poti*poti)
        ur = (qsr2*por - qsi2*poi)*st2
        ui = (qsr2*poi + qsi2*por)*st2
        DO ns = 3, NHI
          qsr3 = 2.*qsr2 - qsr1 + ur
          qsi3 = 2.*qsi2 - qsi1 + ui
          lic = (ns - 1)*las8
          li = (ns - 1)*las2 + 1 + mnulll
          potr = PSL(mnull + ns)*sop + PL(ns)*ll*(ll + 1) - WN(nu)
     &           + PR(li)
          poti = PI(li)*akw + WPSl(mnull + ns)*sop
          li = li + 1
          lf = ns*las2 + mnulll
          lp = lll - las2*(ns - 1)
          acs = 0.
          IF(li.LE.lf)THEN
            DO la = li, lf
              a = CVNr(lp + la - mnulll)
              potr = potr + PR(la)*a
              poti = poti + PI(la)*a*akw
              IF(MEPot.NE.1)THEN
                llll = la - li + 1
                IF(llll.LE.2)THEN
                  lai = 3
                  laf = 5
                  IF(llll.EQ.2)lai = 1
                  IF(llll.EQ.2)laf = 9
                  IF(llll.EQ.1)pvs = PR(la)
                  IF(llll.EQ.1)pws = PI(la)*akw
                  DO lac = lai, laf
                    lalas = lasc*(lac - 1) + llll
                    lp1 = lalas + lic
                    pv1 = PRC(lp1)
                    ll2 = lc2 + lalas
                    a = CVNc(ll2)
                    IF(llll.EQ.2.AND.lac.EQ.1)acs = -a*CBEt0
                    IF(lac.NE.1)THEN
                      potr = potr + a*pv1
                    ENDIF
                  ENDDO
                ENDIF
              ENDIF
            ENDDO
            IF(MEVol.NE.0)THEN
              potr = potr + acs*pvs
              poti = poti + acs*pws
            ENDIF
          ENDIF
          por = potr + st12*(potr*potr - poti*poti)
          poi = poti + st6*potr*poti
          ur = (qsr3*por - qsi3*poi)*st2
          ui = (qsr3*poi + qsi3*por)*st2
          FRH(ns,l) = qsr3 + ur/12.
          FIH(ns,l) = qsi3 + ui/12.
C     ANS=(NS-1.)*STEP
C     IF(MEPRI.NE.99) PRINT 111,FRH(NS,L),FIH(NS,L),ANS
          qsr1 = qsr2
          qsi1 = qsi2
          qsr2 = qsr3
          qsi2 = qsi3
        ENDDO
        l1 = ll + 1
        ABR1 = FBR1(nu,l1)
        ABI1 = FBI1(nu,l1)
        ANR1 = FNR1(nu,l1)
        ANI1 = FNI1(nu,l1)
        ABR2 = FBR2(nu,l1)
        ABI2 = FBI2(nu,l1)
        ANR2 = FNR2(nu,l1)
        ANI2 = FNI2(nu,l1)
        CFR1 = FRH(NHI - 2,l)
        CFI1 = FIH(NHI - 2,l)
        CFR2 = FRH(NHI,l)
        CFI2 = FIH(NHI,l)
C     IF(MEPRI.NE.99) PRINT 222,L,LL,NCLL,NU
C 222 FORMAT (2X,20I5)
C     IF(MEPRI.NE.99) PRINT 111,CFR1,CFI1,CFR2,CFI2
 1020   FORMAT(2X,2E13.5,4I10)
C     IF(MEPRI.NE.99) PRINT 111,ABR1,ABI1,ANR1,ANI1
C     IF(MEPRI.NE.99) PRINT 111,ABR2,ABI2,ANR2,ANI2
        CALL MATCH
        zn = ARA*ARA + AIA*AIA
C        PRINT 1111,ARA,AIA,CRC,CIC,LL,L
C1111 FORMAT(4E10.2,4I3)
        CRH(l) = (ARA*CRC + AIA*CIC)/zn
        CIH(l) = (ARA*CIC - AIA*CRC)/zn
        IF(nslu.EQ.2)THEN
          DO ns = 1, NHI
            CIC = FIH(ns,l)
            CRC = FRH(ns,l)
            FRH(ns,l) = (ARA*CRC + AIA*CIC)/zn
            FIH(ns,l) = (ARA*CIC - AIA*CRC)/zn
          ENDDO
        ELSE
          CRS(l) = CRH(l)
          CIS(l) = CIH(l)
          DO ns = 1, NHI
            CIC = FIH(ns,l)
            CRC = FRH(ns,l)
            FRS(ns,l) = (ARA*CRC + AIA*CIC)/zn
            FIS(ns,l) = (ARA*CIC - AIA*CRC)/zn
          ENDDO
        ENDIF
C 444    FORMAT(2X,3E15.7,9I5)
      ENDDO
      nslu = nslu + 1
      IF(nslu.EQ.2)GOTO 10
      CSS = 0.
C     IF(MEPRI.NE.99) PRINT 333
C333  FORMAT('    SECOND PART')
      mall = NCLl
      IF(MESol.LE.NCLl)mall = MESol
      DO ic = 1, INCc
        ic1 = (ic - 1)*mall
        DO l = 1, NCLl
          CRI(l) = 0.
          CII(l) = 0.
          CRP(l) = 0.
          CIP(l) = 0.
          CRT(1,l) = 0.
          CIT(1,l) = 0.
          NPAd(l) = 1
          DO ns = 1, NHI
            FRI(ns,l) = 0.
            FII(ns,l) = 0.
          ENDDO
        ENDDO
        IF(MESol.GT.3)THEN
          DO mll = 1, mall
            CRI(mll) = CR(ic,mll)
            CII(mll) = CI(ic,mll)
            CRP(mll) = CR(ic,mll)
            CIP(mll) = CI(ic,mll)
            CRT(1,mll) = CR(ic,mll)
            CIT(1,mll) = CI(ic,mll)
            DO ns = 1, NHI
              FRI(ns,mll) = 0.
              FII(ns,mll) = 0.
              DO jll = 1, mall
                jm = (jll - 1)*mall + mll
                ij = ic1 + jll
                FRI(ns,mll) = FRI(ns,mll) - AUR(ij)*FREm(ns,jm)
     &                        + AUI(ij)*FIEm(ns,jm)
                FII(ns,mll) = FII(ns,mll) - AUR(ij)*FIEm(ns,jm)
     &                        - AUI(ij)*FREm(ns,jm)
              ENDDO
            ENDDO
          ENDDO
        ELSE
          CRI(ic) = CRS(ic)
          CII(ic) = CIS(ic)
          CRP(ic) = CRS(ic)
          CIP(ic) = CIS(ic)
          CRT(1,ic) = CRS(ic)
          CIT(1,ic) = CIS(ic)
          DO ns = 1, NHI
            FRI(ns,ic) = FRS(ns,ic)
            FII(ns,ic) = FIS(ns,ic)
          ENDDO
 1030     FORMAT(2E15.5)
        ENDIF
        iter = 1
        ben = 1
   15   csi = 0.
        iter = iter + 1
        IF(iter.GT.25)GOTO 20
        IF(MEHam.NE.1)ben = ben*BET0
        IF(MEHam.EQ.1)ben = ben*BET(2)
        DO l = 1, NCLl
          lll = (l - 1)*(ncla + las1) - 1
 
          liso = (l - 1)*NCLl
 
          lc2 = (l - 1)*(icll + las8)
          ll = LNJ1(l)
          st = 1./(18.D+00)**ll
          jj = JNJ1(l)
          nu = NNJ1(l)
          mnulll = mnula*(nu - 1) + mla*(nu - 1)
          mnull = mnu*(nu - 1) + 300*(nu - 1)
          mnll = mnula*(nu - 1)
          mnl = mnu*(nu - 1)
C       IF((EN-EL(NU)/AT*(AT+ANEU)).LT.0) ST=EXP(-5.*LL)
          npi1 = KPJ1(l)
C     NEXT TWO CARDS JUST TO OVERCOME THE ERROR OR  MICROSOFT FPS COMPILER
C     v.1.00, YOU CAN DELETE THEM USING OTHER COMPILERS
          IF(WNK(nu)/WNK(1).LE.0.)PRINT 1040, WNK(nu), WNK(1), nu
 1040     FORMAT(2E12.3,I3)
          c = SQRT(WNK(nu)/WNK(1))
          cj1 = jj/2.
          sop = (cj1 - ll)*(cj1 + ll + 1) - spi2
          FRI(1,l) = 0.
          FII(1,l) = 0.
          FRI(2,l) = st*STEp**(ll + 1)
          FII(2,l) = 0.
          qsr1 = 0.
          qsi1 = 0.
          IF(ll.EQ.1)qsr1 = -st*st6
          potr = PSL(mnull + 2)*sop + PL(2)*ll*(ll + 1) - WN(nu)
     &           + PR(mnulll + las2 + 1)
          poti = PI(mnulll + las2 + 1) + WPSl(mnull + 2)*sop
 
 
          li = las2 + 2 + mnulll
          lf = 2*las2 + mnulll
          acs = 0.
          IF(li.LE.lf)THEN
            DO la = li, lf
              a = CVNr(lll + la - las2 - mnulll)
              potr = potr + PR(la)*a
              poti = poti + PI(la)*a
              IF(MEPot.NE.1)THEN
                llll = la - li + 1
                IF(llll.LE.2)THEN
                  lai = 3
                  laf = 5
                  IF(llll.EQ.2)lai = 1
                  IF(llll.EQ.2)laf = 9
                  IF(llll.EQ.1)pvs = PR(la)
                  IF(llll.EQ.1)pws = PI(la)
                  DO lac = lai, laf
                    lalas = lasc*(lac - 1) + llll
                    lp1 = lalas + las8
                    pv1 = PRC(lp1)
                    ll2 = lc2 + lalas
                    a = CVNc(ll2)
                    IF(llll.EQ.2.AND.lac.EQ.1)acs = -a*CBEt0
                    IF(lac.NE.1)THEN
                      potr = potr + a*pv1
                    ENDIF
                  ENDDO
                ENDIF
              ENDIF
            ENDDO
            IF(MEVol.NE.0)THEN
              potr = potr + acs*pvs
              poti = poti + acs*pws
            ENDIF
          ENDIF
          qsr2 = FRI(2,l)*(1. - st12*potr)
          qsi2 = -FRI(2,l)*st12*poti
          por = potr + st12*(potr*potr - poti*poti)
          poi = poti + st6*potr*poti
          ur = (qsr2*por - qsi2*poi)*st2
          ui = (qsr2*poi + qsi2*por)*st2
          wr1 = 0.
          wi1 = 0.
          wr2 = 0.
          wi2 = 0.
          k1 = (l - 1)*ncla - 1
          kc1 = icll*(l - 1)
          DO ir = 1, NCLl
C        IF(L.LE.MALL.AND.IR.GT.MALL) GO TO 11
C        IF(L.GT.MALL.AND.IR.GT.1) GO TO 11
C        IF(L.GT.MALL) GO TO 11
            npi2 = KPJ1(ir)
            nuir = NNJ1(ir)
            lisor = liso + ir
            mnllir = mnll + mla*(nuir - 1)
            k2 = k1 + (ir - 1)*las1
            kc2 = kc1 + las8*(ir - 1)
            li = las2 + 2 + mnllir
            IF(ir.NE.l)THEN
C      LI=LAS2+2+MNLLIR
              lf = 2*las2 + mnllir
              potr = 0.
              poti = 0.
              acs = 0.
              IF(li.LE.lf)THEN
                DO la = li, lf
                  a = CVNr(k2 + la - las2 - mnllir)
                  IF(npi1.NE.npi2)THEN
                    potr = potr - PI(la)*a
                    poti = poti + PR(la)*a
                  ELSE
                    potr = potr + PR(la)*a
                    poti = poti + PI(la)*a
                  ENDIF
                  IF(MEPot.NE.1)THEN
                    llll = la - li + 1
                    IF(llll.LE.2)THEN
                      lai = 3
                      laf = 5
                      IF(llll.EQ.2)lai = 1
                      IF(llll.EQ.2)laf = 9
                      IF(llll.EQ.1)pvs = PR(la)
                      IF(llll.EQ.1)pws = PI(la)
                      DO lac = lai, laf
                        lalas = lasc*(lac - 1) + llll
                        lp1 = lalas + las8
                        pv1 = PRC(lp1)
                        ll2 = kc2 + lalas
                        a = CVNc(ll2)
                        IF(llll.EQ.2.AND.lac.EQ.1)acs = -a*CBEt0
                        IF(lac.NE.1)THEN
                          IF(npi1.NE.npi2)THEN
                            poti = poti + a*pv1
                          ELSE
                            potr = potr + a*pv1
                          ENDIF
                        ENDIF
                      ENDDO
                    ENDIF
                  ENDIF
                ENDDO
 
 
 
                coupl = CVNrpn(lisor)
C      IF(NU.NE.NUIR.AND.JO(NU).EQ. JO(NUIR).AND.JO(NU).EQ.0) COUPL=1.D0
                IF(NCA(nu).NE.NCA(nuir).AND.JO(nu).EQ.JO(nuir))
     &             potr = potr + PR(li - 1)*coupl
                IF(NCA(nu).NE.NCA(nuir).AND.JO(nu).EQ.JO(nuir))
     &             poti = poti + PI(li - 1)*coupl
 
 
 
 
 
 
 
                IF(MEVol.NE.0)THEN
                  potr = potr + acs*pvs
                  poti = poti + acs*pws
                ENDIF
              ENDIF
              fr = FRI(2,ir)
              fi = FII(2,ir)
              wr2 = wr2 + potr*fr - poti*fi
              wi2 = wi2 + potr*fi + poti*fr
            ENDIF
          ENDDO
          DO ns = 3, NHI
            wr3 = 0.
            wi3 = 0.
            DO ir = 1, NCLl
C        IF(L.LE.MALL.AND.IR.GT.MALL) GO TO 14
C        IF(L.GT.MALL.AND.IR.GT.1) GO TO 14
C        IF(L.GT.MALL) GO TO 14
              lisor = liso + ir
              npi2 = KPJ1(ir)
              nuir = NNJ1(ir)
              mnllir = mnll + mla*(nuir - 1)
              k2 = k1 + (ir - 1)*las1
              kc2 = kc1 + las8*(ir - 1)
C      IF(IR.EQ.L) GO TO 14
              li = (ns - 1)*las2 + 2 + mnllir
              IF(ir.NE.l)THEN
                lic = (ns - 1)*las8
                lc2 = (l - 1)*(icll + las8)
                potr = 0.
                poti = 0.
                lf = ns*las2 + mnllir
                lp = k2 - (ns - 1)*las2
                acs = 0.
                IF(li.LE.lf)THEN
                  DO la = li, lf
                    a = CVNr(lp + la - mnllir)
                    IF(npi1.NE.npi2)THEN
                      potr = potr - PI(la)*a
                      poti = poti + PR(la)*a
                    ELSE
                      potr = potr + PR(la)*a
                      poti = poti + PI(la)*a
                    ENDIF
                    IF(MEPot.NE.1)THEN
                      llll = la - li + 1
                      IF(llll.LE.2)THEN
                        lai = 3
                        laf = 5
                        IF(llll.EQ.2)lai = 1
                        IF(llll.EQ.2)laf = 9
                        IF(llll.EQ.1)pvs = PR(la)
                        IF(llll.EQ.1)pws = PI(la)
                        DO lac = lai, laf
                          lalas = lasc*(lac - 1) + llll
                          lp1 = lalas + lic
                          pv1 = PRC(lp1)
                          ll2 = kc2 + lalas
                          a = CVNc(ll2)
                          IF(llll.EQ.2.AND.lac.EQ.1)acs = -a*CBEt0
                          IF(lac.NE.1)THEN
                            IF(npi1.NE.npi2)THEN
                              poti = poti + a*pv1
                            ELSE
                              potr = potr + a*pv1
                            ENDIF
                          ENDIF
                        ENDDO
                      ENDIF
                    ENDIF
                  ENDDO
 
 
 
                  coupl = CVNrpn(lisor)
C      IF(NU.NE.NUIR.AND.JO(NU).EQ. JO(NUIR).AND.JO(NU).EQ.0) COUPL=1.D0
                  IF(NCA(nu).NE.NCA(nuir).AND.JO(nu).EQ.JO(nuir))
     &               potr = potr + PR(li - 1)*coupl
                  IF(NCA(nu).NE.NCA(nuir).AND.JO(nu).EQ.JO(nuir))
     &               poti = poti + PI(li - 1)*coupl
 
C        IF(NU.NE.NUIR.AND.JO(NU).EQ. JO(NUIR))
C     *print 567, lisor,nu,nuir,jo(nu),jo(nuir),li,PR(LI-1),Pi(LI-1),
C     *     CVNRPN(LISOR)
C  567 Format (6i4,5e12.3)
C      IF(NU.NE.NUIR.AND.JO(NU).EQ. JO(NUIR))pause
 
                  IF(MEVol.NE.0)THEN
                    potr = potr + acs*pvs
                    poti = poti + acs*pws
                  ENDIF
                ENDIF
                fr = FRI(ns,ir)
                fi = FII(ns,ir)
                wr3 = wr3 + potr*fr - poti*fi
                wi3 = wi3 + potr*fi + poti*fr
              ENDIF
            ENDDO
            qsr3 = 2.*qsr2 - qsr1 + ur + (wr3 + 10.*wr2 + wr1)*st12
            qsi3 = 2.*qsi2 - qsi1 + ui + (wi3 + 10.*wi2 + wi1)*st12
            li = (ns - 1)*las2 + 1 + mnulll
            potr = PSL(mnull + ns)*sop + PL(ns)*ll*(ll + 1) - WN(nu)
     &             + PR(li)
            poti = PI(li) + WPSl(mnull + ns)*sop
 
 
 
            li = li + 1
            lf = ns*las2 + mnulll
            lp = lll - las2*(ns - 1)
            acs = 0.
            IF(li.LE.lf)THEN
              DO la = li, lf
                a = CVNr(lp + la - mnulll)
                potr = potr + PR(la)*a
                poti = poti + PI(la)*a
                IF(MEPot.NE.1)THEN
                  llll = la - li + 1
                  IF(llll.LE.2)THEN
                    lai = 3
                    laf = 5
                    IF(llll.EQ.2)lai = 1
                    IF(llll.EQ.2)laf = 9
                    IF(llll.EQ.1)pvs = PR(la)
                    IF(llll.EQ.1)pws = PI(la)
                    DO lac = lai, laf
                      lalas = lasc*(lac - 1) + llll
                      lp1 = lalas + lic
                      pv1 = PRC(lp1)
                      ll2 = lc2 + lalas
                      a = CVNc(ll2)
                      IF(llll.EQ.2.AND.lac.EQ.1)acs = -a*CBEt0
                      IF(lac.NE.1)THEN
                        potr = potr + a*pv1
                      ENDIF
                    ENDDO
                  ENDIF
                ENDIF
              ENDDO
              IF(MEVol.NE.0)THEN
                potr = potr + acs*pvs
                poti = poti + acs*pws
              ENDIF
            ENDIF
            por = potr + st12*(potr*potr - poti*poti)
            poi = poti + st6*potr*poti
            ur = (qsr3*por - qsi3*poi)*st2
            ui = (qsr3*poi + qsi3*por)*st2
            FRI(ns,l) = qsr3 + ur/12.
            FII(ns,l) = qsi3 + ui/12.
            wr1 = wr2
            wi1 = wi2
            wr2 = wr3
            wi2 = wi3
            qsr1 = qsr2
            qsi1 = qsi2
            qsr2 = qsr3
            qsi2 = qsi3
          ENDDO
          l1 = ll + 1
          ABR1 = FBR1(nu,l1)
          ABI1 = FBI1(nu,l1)
          ANR1 = FNR1(nu,l1)
          ANI1 = FNI1(nu,l1)
          ABR2 = FBR2(nu,l1)
          ABI2 = FBI2(nu,l1)
          ANR2 = FNR2(nu,l1)
          ANI2 = FNI2(nu,l1)
          CFR1 = FRI(NHI - 2,l)
          CFI1 = FII(NHI - 2,l)
          CFR2 = FRI(NHI,l)
          CFI2 = FII(NHI,l)
C
C     IF(MEPRI.NE.99) THEN
C       PRINT 222,L,LL,NCLL,NU
C       PRINT 111,CFR1,CFI1,CFR2,CFI2
C       PRINT 111,ABR1,ABI1,ANR1,ANI1
C       PRINT 111,ABR2,ABI2,ANR2,ANI2
C     ENDIF
C
          CALL MATCH
          IF(ic.EQ.l)ARA = ARA - 1.
          chr = CRH(l)
          chi = CIH(l)
C     IF(MEPRI.NE.99) THEN
C       PRINT 1111,ARA,AIA,CRC,CIC,LL,L,ITER
C       PRINT 1111,ARA,AIA,CRC,CIC,CHR,CHI,L
C     ENDIF
C1111 FORMAT(6E10.2,3I2)
          CRC = (CRC - ARA*chr + AIA*chi)*c
          CIC = (CIC - ARA*chi - AIA*chr)*c
          CRT(iter,l) = (CRC - CRI(l))/ben
          CIT(iter,l) = (CIC - CII(l))/ben
          IF(CRT(iter,l).EQ.0..OR.CIT(iter,l).EQ.0.)THEN
C     IF(MEPRI.NE.99) PRINT 111,CRT(ITER,L),CIT(ITER,L),L,NSS,ITER,NCLL
            IF(iter.LE.3)THEN
              CR(ic,l) = CRC
              CI(ic,l) = CIC
            ENDIF
            NPAd(l) = 0
          ENDIF
          CRI(l) = CRC
          CII(l) = CIC
          DO ns = 1, NHI
            CRC = FRI(ns,l)
            CIC = FII(ns,l)
            fr = FRH(ns,l)
            fi = FIH(ns,l)
            FRI(ns,l) = CRC - ARA*fr + AIA*fi
            FII(ns,l) = CIC - ARA*fi - AIA*fr
          ENDDO
        ENDDO
        IF(iter.LT.3.OR.iter/2*2.EQ.iter)GOTO 15
        sumc = 0.
        sumi = 0.
        sumt = 0.
        LPA = iter/2
        lpa1 = LPA + 1
        MPA = LPA
C     IF(MEPRI.NE.99) PRINT 1999,ITER,MPA,LPA
C1999 FORMAT(3I6)
        DO l = 1, INCr
C         GO TO 27
          IF(NPAd(l).NE.0)THEN
            DO it = 1, iter
              CT(it) = CRT(it,l)
            ENDDO
C     IF(MEPRI.NE.99) PRINT 1999,ITER,MPA,LPA
            CALL PADE
            al = 0.
            am = 0.
            DO lm = 1, lpa1
              IF(MEHam.NE.1)btn = BET0**(lm - 1)
              IF(MEHam.EQ.1)btn = BET(2)**(lm - 1)
              al = al + APA(lm)*btn
              am = am + BPA(lm)*btn
            ENDDO
            CR(ic,l) = al/am
            DO it = 1, iter
              CT(it) = CIT(it,l)
            ENDDO
            CALL PADE
            al = 0.
            am = 0.
            DO lm = 1, lpa1
              IF(MEHam.NE.1)btn = BET0**(lm - 1)
              IF(MEHam.EQ.1)btn = BET(2)**(lm - 1)
              al = al + APA(lm)*btn
              am = am + BPA(lm)*btn
            ENDDO
            CI(ic,l) = al/am
          ENDIF
          CRC = CR(ic,l)
          CIC = CI(ic,l)
          csi = csi + CRC*CRC + CIC*CIC
          sumc = sumc + ABS(CRP(l) - CRC) + ABS(CIP(l) - CIC)
          IF(ic.EQ.l)sumi = sumi + ABS(CIP(l) - CIC)
          IF(ic.EQ.l)sumt = sumt + CIC*con
          IF(ABS(CRC).GT.1.D+3)GOTO 20
          IF(ABS(CIC).GT.1.D+3)GOTO 20
          CRD(NSS,ic,l) = CR(ic,l)
          CID(NSS,ic,l) = CI(ic,l)
          CRP(l) = CRC
          CIP(l) = CIC
        ENDDO
        csur = csi*con
        acon = con
        IF(MEPri.NE.99)PRINT 1050, sumc, sumi, acon, csi, csur, sumt
        IF(iter.EQ.3)GOTO 15
 
        epeci = 0.0005/con
        IF(sumi.GT.epeci)GOTO 15
C     RCN: CHECK
C     A division by zero occurs if optimization allowed in MSF
C
        epeci = 0.0005D0/con/2/SQRT(csi)
        IF(sumc.GT.epeci)GOTO 15
C
C     HIGH ANGULAR MOMENTUM CASE : SCATTERING EQUAL TOTAL, TO
C     AVOID NEGATIVE TRANSMISSIONS AS ABSORPTION IS VERY SMALL.
C
        IF(sumt.LT.0.OR.csi.GT.CI(ic,ic))THEN
          csid = csi - CI(ic,ic)*CI(ic,ic)
          CI(ic,ic) = 0.5D0*(1. - SQRT(1.D0 - 4.*csid))
          CID(NSS,ic,ic) = CI(ic,ic)
          csi = CI(ic,ic)
        ENDIF
C
        CSS = CSS + csi
 1050   FORMAT(2X,4E8.2,2E14.7)
      ENDDO
      IF(MEPri.NE.99)PRINT 1060, NSS, NCLl, iter
 1060 FORMAT(3I5,E20.7)
      RETURN
   20 NECi = 0
      RETURN
      END SUBROUTINE ECISS
 
!---------------------------------------------------------------------------
C     ***********************************************************
      SUBROUTINE MATCH
C     ***********************************************************
      IMPLICIT DOUBLE PRECISION(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: ABI1, ABI2, ABR1, ABR2, AIA, ANI1, ANI2, ANR1, ANR2, 
     &          ARA, CFI1, CFI2, CFR1, CFR2, CIC, CRC
      COMMON /MATT  / ABR1, ABI1, ANR1, ANI1, ABR2, ABI2, ANR2, ANI2, 
     &                CFR1, CFI1, CFR2, CFI2, CRC, CIC, ARA, AIA
C
C Local variables
C
      REAL*8 :: fwi1, fwi2, fwr1, fwr2, zni, znr
C
C*** End of declarations rewritten by SPAG
C
      znr = ABR2*ANR1 - ABI2*ANI1 - ABR1*ANR2 + ABI1*ANI2
      zni = ABI2*ANR1 + ABR2*ANI1 - ABR1*ANI2 - ABI1*ANR2
      IF(zni.NE.0.)THEN
        CRC = (CFR1*ABI2 + CFI1*ABR2 - CFR2*ABI1 - CFI2*ABR1)/zni
        CIC = (CFI1*ABI2 + CFR2*ABR1 - CFR1*ABR2 - CFI2*ABI1)/zni
        fwr1 = ANR1 - ABI1
        fwi1 = ANI1 + ABR1
        fwr2 = ANR2 - ABI2
        fwi2 = ANI2 + ABR2
        ARA = (CFR2*fwi1 + CFI2*fwr1 - CFR1*fwi2 - CFI1*fwr2)/zni
        AIA = (CFI2*fwi1 - CFR2*fwr1 - CFI1*fwi2 + CFR1*fwr2)/zni
      ELSE
        CRC = (CFR1*ABR2 - CFR2*ABR1)/znr
        CIC = (CFI1*ABR2 - CFI2*ABR1)/znr
        ARA = (CFR2*ANR1 - CFI2*ABR1 - CFR1*ANR2 + CFI1*ABR2)/znr
        AIA = (CFI2*ANR1 + CFR2*ABR1 - CFR1*ABR2 - CFI1*ANR2)/znr
      ENDIF
      RETURN
      END SUBROUTINE MATCH
 
!---------------------------------------------------------------------------
C     ***********************************************************
      SUBROUTINE PADE
C     ***********************************************************
      IMPLICIT DOUBLE PRECISION(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8, DIMENSION(400) :: AMA
      REAL*8, DIMENSION(20) :: APA, BPA, CY
      REAL*8, DIMENSION(40) :: CT
      INTEGER :: LPA, MAM, MPA
      COMMON /MATI  / AMA, MAM
      COMMON /PAD   / CT, APA, BPA, CY, MPA, LPA
C
C Local variables
C
      REAL*8 :: ap, bp
      INTEGER :: i, i1, ij, j, k, k1, ki, l, lma, lmpa, lpa1, mk, 
     &           mpa1
C
C*** End of declarations rewritten by SPAG
C
C     kkk = 9
C     PRINT 100,CT
C     PRINT 200,MPA,LPA,KKK
C 200 FORMAT(5I10)
      IF(MPA.GT.0)THEN
        DO i = 1, MPA
          i1 = (i - 1)*MPA
          DO j = 1, MPA
            ij = i1 + j
            lmpa = LPA - MPA + i + j
            IF(lmpa.LT.1)THEN
              AMA(ij) = 0.
            ELSE
              AMA(ij) = CT(lmpa)
            ENDIF
          ENDDO
        ENDDO
        MAM = MPA
        CALL MATIN
        DO i = 1, MPA
          CY(i) = CT(LPA + i + 1)
        ENDDO
        DO k = 1, MPA
          k1 = (k - 1)*MPA
          mk = MPA + 2 - k
          bp = 0.
          DO i = 1, MPA
            ki = k1 + i
            bp = bp - AMA(ki)*CY(i)
          ENDDO
          BPA(mk) = bp
        ENDDO
      ENDIF
      BPA(1) = 1.
      lpa1 = LPA + 1
      mpa1 = MPA + 1
      DO l = 1, lpa1
        ap = CT(l)
        lma = l
        IF(l.GT.mpa1)lma = mpa1
        IF(lma.GE.2)THEN
          DO i = 2, lma
            ap = ap + BPA(i)*CT(l - i + 1)
          ENDDO
        ENDIF
        APA(l) = ap
      ENDDO
C     PRINT 100,APA,BPA
C 100 FORMAT (2X,6E20.7)
      RETURN
      END SUBROUTINE PADE
 
!---------------------------------------------------------------------------
C     ***********************************************************
      SUBROUTINE MATIN
C     ***********************************************************
      IMPLICIT DOUBLE PRECISION(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8, DIMENSION(400) :: AMA
      INTEGER :: MAM
      COMMON /MATI  / AMA, MAM
C
C Local variables
C
      REAL*8 :: elem, piv
      INTEGER :: i, i1, ij, ik, j, k, k1, kj, kk
C
C*** End of declarations rewritten by SPAG
C
      DO k = 1, MAM
        k1 = (k - 1)*MAM
        kk = k1 + k
        piv = 1./AMA(kk)
        DO j = 1, MAM
          kj = k1 + j
          AMA(kj) = AMA(kj)*piv
        ENDDO
        AMA(kk) = piv
        DO i = 1, MAM
          i1 = (i - 1)*MAM
          ik = i1 + k
          IF(i.NE.k)THEN
            elem = AMA(ik)
            AMA(ik) = 0.
            DO j = 1, MAM
              ij = i1 + j
              kj = k1 + j
              AMA(ij) = AMA(ij) - elem*AMA(kj)
            ENDDO
          ENDIF
        ENDDO
      ENDDO
      RETURN
      END SUBROUTINE MATIN
 
!---------------------------------------------------------------------------
C     ***********************************************************
      SUBROUTINE RIPAT
C     ***********************************************************
      IMPLICIT DOUBLE PRECISION(a - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: AC, AC0, AC1, AD, AD0, AD1, ALAso, ALAvr, ALAwd, ALF, 
     &          ALFnew, AMI, ANEu, AR, AR0, AR1, AS, AS0, AS1, ASP, AT, 
     &          AW, AW0, AW1, AZ, BNDc, CCOul, CISo, CONz, DE, EA, 
     &          EFErm, EFErmn, EFErmp, EN, ENCon, ETA, PDIs, R, RC, RD, 
     &          RK, RR, RRBwc, RRWid, RS, RW, RZ, RZBwc, RZWid, ST, 
     &          STEp, VD, VHF, VP, VR, VR0, VR1, VR2, VR3, VRD, VRDc, 
     &          VRLa, VRLanp, VS, WC, WC0, WC1, WCA1, WCBw, WCIso, 
     &          WCWid, WD, WD0, WD1, WDA1, WDBw, WDIso, WDShi, WDWid
      REAL*8, DIMENSION(10) :: BET
      REAL*8, DIMENSION(20,90) :: COPh
      REAL*8, DIMENSION(720000) :: CVNc
      REAL*8, DIMENSION(300) :: DEF, PL, PZI, VL
      REAL*8, DIMENSION(20) :: EL, WN, WNK
      INTEGER, DIMENSION(20) :: JO, KO, NCA, NPO
      INTEGER :: LAS, MEApp, MECha, MECul, MEDis, MEHam, MEHao, MEJob, 
     &           MEPot, MEPri, MERel, MERip, MERrr, MERzz, MESha, MESho, 
     &           MESol, MEVol, NAN1, NAN2, NECi, NH, NH1, NHI, NMAx, 
     &           NPD, NUR
      REAL*8, DIMENSION(600000) :: PI, PR, PV, PW
      REAL*8, DIMENSION(120000) :: PII, PRR, PSL, PVV, PWW, VSL, WPSl, 
     &                             WSL
      REAL*8, DIMENSION(5,300) :: POL
      REAL*8, DIMENSION(5400) :: PRC, PVC
      REAL*8 :: WDWid2, WP, WS0, WS1, WSBw, WSWid, WW4, ZNUc
      COMMON /AB    / DEF, VL, VSL, POL, PV, PW, WSL
      COMMON /ABCOUL/ PVC, PRC, CVNc
      COMMON /ABEC  / PL, PSL, PR, PI, WPSl
      COMMON /COUL  / CONz, ETA, COPh
      COMMON /COULON/ PZI
      COMMON /DISPE / VD, VRDc, EA, WDIso
      COMMON /DISPE2/ VRD, WDShi, WDWid2, ALFnew
      COMMON /ECI   / NECi, NHI
      COMMON /ENA   / EN, EL, BET, NUR, NMAx, NPD, LAS
      COMMON /MENU  / MEJob, MEPot, MEHam, MECha, MEPri, MESol, MESha, 
     &                MESho, MEHao, MEApp, MEVol, MERel, MECul, MERzz, 
     &                MERrr, MEDis, MERip
      COMMON /POT1  / VR3, VR2, VR1, VR0, WC1, WC0, WD1, WD0, VS, AC0, 
     &                AR0, AW0, AD0, AS0
      COMMON /POT2  / AR1, AC1, AW1, AD1, AS1
      COMMON /POT3  / BNDc, WDA1, WCA1, CCOul, CISo, WCIso, WS0, WS1
      COMMON /POTB  / WNK, WN, VR, WC, WD
      COMMON /POTD  / ALAvr, VRLa, WCBw, WCWid, WDBw, WDWid, ALAwd, 
     &                EFErmn, EFErmp, ALAso, PDIs, WSBw, WSWid, RRBwc, 
     &                RRWid, RZBwc, RZWid
      COMMON /POTEB / R, DE, VP, WP
      COMMON /POTPN / PVV, PWW, PRR, PII
      COMMON /QNB   / JO, NPO, KO, NCA
      COMMON /RAD   / RR, RC, RD, RW, RS, AR, AC, AW, AD, AS, ALF, AT, 
     &                ANEu, RZ, ZNUc, ASP, AZ
      COMMON /RIP   / ST, NH, NAN1, NAN2
      COMMON /STR   / STEp, RK, NH1
      COMMON /VHFS  / WW4, VHF, ENCon, VRLanp, EFErm, AMI
C
C Local variables
C
      REAL*8 :: a, an, apan, b, c, cccoul, ccde, cde, cde12, 
     &          coenh, conz12, conzz, davern, daverp, derel, dtmp, dvso, 
     &          e, eee, efenu12, eint, eirelm, eirelt, ene, enef, 
     &          eneshid, epaver, ex, ex1, exd, exdm, exx, fpvc, pdc, 
     &          pdis1, pim, pz, rac, rad, rar, raw, rel, relpot, rkc, 
     &          rkd, rlam, rlam1, rrr, rzin, step1, t12d, t12m, t12p, 
     &          v2, v4, vcul, vculc, vdcul, vdd, vdd1, vdis, vdisd, 
     &          vdisp, vdp, vdp1, vhfm, vhfp, viso, vrdir, vrdirc, vslc, 
     &          vslf, vso, vv, w2, w4, wcc, wcp, wcul
      REAL*8 :: DOM_INT_T1, DOM_INT_T2, DOM_INT_WS, DOM_INT_WV
      INTEGER :: i, i1, ii, ii2, iic, ik, in, ir, k, kit, kk, kl, l, l1, 
     &           l1ap, lac, lalali, lalalr, lalas, lalasi, lalasr, lam, 
     &           las1, las2, lasc, ll2, ll2ap, m, m4, mla, mnu, mnu1, 
     &           mnu2, mnula, mnula1, mnula2, mup, n, nh2, nh3, nit, 
     &           nt0, nt1, nt2, nt3, nt4, nu1, nu2
      INTEGER :: INT
      REAL*8 :: wdd, wdul, wiso, wso, wv, wviso, ww, www, x, xx, y, y1, 
     &          y2, y3, y4, yc1, yc2, yc3
C
C*** End of declarations rewritten by SPAG
C
C     BELOW CARD IS NECESSARY IF MEMORY IS LESS THAN 32Mb
 
      DO i = 1, 600000
        IF(i.LE.120000)THEN
          PVV(i) = 0.D0
          PWW(i) = 0.D0
          PRR(i) = 0.D0
          PII(i) = 0.D0
          PSL(i) = 0.D0
          WPSl(i) = 0.D0
          VSL(i) = 0.D0
          WSL(i) = 0.D0
        ENDIF
        PV(i) = 0.D0
        PW(i) = 0.D0
        PR(i) = 0.D0
        PI(i) = 0.D0
      ENDDO
 
      ccde = 0.0
      cccoul = CCOul
      IF(MECul.GE.2)ccde = CCOul
      IF(MECul.GE.2)CCOul = 0.0
      cde = ccde*MECha*ZNUc/AT**(1.D0/3.D0)
      cde12 = ccde*ZNUc/AT**(1.D0/3.D0)
 
      VRDc = 0.0
      VD = 0.0
      vdisp = 0.0
      AMI = 939.56536
      IF(MECha.EQ.1)AMI = 938.272029
      davern = 0.00
      daverp = 0.00
      EFErm = EFErmn
      epaver = EFErm + davern
      IF(MECha.EQ.1)EFErm = EFErmp
      IF(MECha.EQ.1)epaver = EFErm + daverp
      eint = EN
      EN = EN - EFErm - cde
 
      CALL POTVOL
      EN = eint
 
      rzin = RZ
      IF(MERzz.NE.0)THEN
        IF(MERzz.EQ.1)RZ = RZ*(1.D00 - RZBwc*(EN - EFErm)**PDIs/((EN-
     &                     EFErm)**PDIs + RZWid**PDIs))
      ENDIF
 
      rrr = RR
      apan = ANEu/1.0086652
C
C           RR ENERGY DEPENDENCE
C
      IF(MERrr.EQ.1)RR = RR*(1.D00 - RRBwc*(EN - EFErm)**PDIs/((EN-EFErm
     &                   )**PDIs + RRWid**PDIs))
      DO i = 1, NUR
        rel = (EN + AMI)/AMI
        IF(MERel.EQ.0)rel = 1.D+0
        e = EN - EL(i)/AT*(AT + ANEu*rel)
        rel = (ABS(e) + AMI)/AMI
        IF(MERel.EQ.0.OR.e.LE.0.)rel = 1.D+0
        IF(MERel.NE.0.AND.e.GT.0.)e = (rel**2 - 1.D+0)*AMI/2.D+0
 
        WNK(i) = 0.219677*SQRT(ABS(e))
     &           *AT/SQRT(AT**2 + 2.D+0*ANEu*rel*AT + ANEu**2)
     &           *SQRT(apan)
        WN(i) = WNK(i)*WNK(i)
        IF(EN - EL(i)/AT*(AT + ANEu*rel).LE.0.)WN(i) = -WN(i)
      ENDDO
 
 
      rel = (EN + AMI)/AMI
      IF(MERel.EQ.0)rel = 1.D+0
      IF(rel.NE.1.)relpot = 2.D+0*(EN + AMI)/(2.D+0*AMI + EN)
 
      eirelm = SQRT(rel**2*AT**2 + 2.D+0*rel*ANEu*AT + ANEu**2)
      eirelt = AT*SQRT(rel**2*ANEu**2 + 2.D+0*rel*ANEu*AT + AT**2)
      derel = SQRT(2.D+0*rel*ANEu*AT + AT**2 + ANEu**2)
      ww = 4.8257984E-2*eirelm*eirelt/(ANEu*eirelm + eirelt)/derel*apan
      IF(MERel.EQ.1.OR.MERel.EQ.3)ww = ww*relpot
      CONz = MECha*ZNUc*eirelm*eirelt/(ANEu*eirelm + eirelt)
     &       /derel*3.458814365D-2*apan
      conz12 = ZNUc*eirelm*eirelt/(ANEu*eirelm + eirelt)
     &         /derel*3.458814365D-2*apan
 
      conzz = CONz
 
 
 
 
      enef = EN - EFErm - cde
      EN = EN - cde
 
      viso = CISo*(AT - 2.*ZNUc)/AT
      wiso = WCIso*(AT - 2.*ZNUc)/AT
      wviso = WDIso*(AT - 2.*ZNUc)/AT
      IF(MECha.EQ.0)viso = -viso
      IF(MECha.EQ.0)wiso = -wiso
      IF(MECha.EQ.0)wviso = -wviso
      VRLanp = VRLa + viso
 
 
C      WCP=Wv(Ef-Ea)=Wv(Ef+Ea)
 
      wcp = (WCBw + wviso)*(EA)**PDIs/(EA**PDIs + WCWid**PDIs)
 
C     TWO OPTIONS FOR COULOMB CORRECTIONS
C     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
 
      vrdir = ((VR1 + 2.*VR2*enef + 3.*VR3*enef**2 - VRLa*ALAvr*EXP(-
     &        ALAvr*enef)))*(1.D0 + viso/(VR0 + VRLa))*( - 1.D0)
 
      IF(MEDis.NE.0)THEN
C    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C     VHF DERIVATIVE AT EN
        eee = EN
        EN = EN - 0.005D+00
        ENCon = EN
 
        CALL VHFROM
        vhfm = VHF
 
        EN = EN + 0.01D+00
        ENCon = EN
 
        CALL VHFROM
        vhfp = VHF
 
        vrdir = (vhfm - vhfp)*100
        EN = eee
 
C    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
 
        vdisp = DOM_INT_WV(EFErm,epaver,WCBw + wviso,WCWid,INT(PDIs),EN,
     &          vdisd)
 
        t12d = 0.D0
        IF(EA.GT.0.D0.AND.WC.NE.0.D0)THEN
          t12p = wcp*DOM_INT_T1(EFErm,EA,EN + 0.01)
     &           + ALFnew*DOM_INT_T2(EFErm,EA,EN + 0.01)
          t12m = wcp*DOM_INT_T1(EFErm,EA,EN - 0.01)
     &           + ALFnew*DOM_INT_T2(EFErm,EA,EN - 0.01)
          t12d = (t12m - t12p)*50
        ENDIF
        vrdirc = vdisd + t12d
 
        vdp = DOM_INT_WS(EFErm,epaver,WDBw + wiso,WDWid,ALAwd,INT(PDIs),
     &        EN,vdd)
 
        vdcul = vdd*MECha*ZNUc/AT**0.333333333*CCOul
 
        pdis1 = PDIs - 1
C
        wdul = 0.
        wcul = 0.
 
        wcul = 0.
C
        vculc = MECha*ZNUc/AT**0.333333333*CCOul*vrdirc
      ENDIF
      vcul = MECha*ZNUc/AT**0.333333333*CCOul*vrdir
 
      IF(MECul.EQ.1)vcul = MECha*ZNUc/AT**0.333333333*CCOul
C
      mup = -1.91301
      IF(MECha.NE.0)mup = 2.2928
      mup = 0.
C
      VR = (VR0 + VR1*enef + VR2*enef**2 + VR3*enef**3 + 
     &     VRLa*EXP( - ALAvr*enef))*(1.D00 + viso/(VR0 + VRLa)) + vcul
 
      IF(MEDis.NE.0)THEN
C    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C     VHF AND VR AT EN
 
        ENCon = EN
 
        CALL VHFROM
        VR = VHF + vcul
 
 
C    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
        vdis = vdisp
 
        VD = vdp + vdcul
 
        WRITE(21,1010)EN, VRLa, ALAvr, enef, viso, vcul, VR0, VR, VD, 
     &                vdp, vdcul, vdis, vdisd, vdd, t12d
 
 1010   FORMAT(/2X,' POTENTIAL VALUES (I) FOR INCIDENT ENERGY=',F11.6,
     &         1X,'MeV:'//,3X,'VRLA=',F8.3,3X,'ALAVR=',F8.3,3X,'   Ef=',
     &         F8.3,3X,'VISO=',F8.3/3X,'VCUL=',F8.3,3X,'  VR0=',F8.3,3X,
     &         '   VR=',F8.3/3X,'  VD=',F8.3,3X,'  VDP=',F8.3,3X,
     &         'VDCUL=',F8.3/3X,'VDIS=',F8.3,3X,'VDISD=',F8.3,3X,
     &         '  VDD=',F8.3,3X,'T12D=',F8.3/)
 
C     VR = VR + VDIS
        VRDc = vdis + vculc
      ENDIF
 
 
 
      IF(EN.LT.BNDc)THEN
        WD = WD0 + WD1*enef + (WDBw + wiso)*EXP( - ALAwd*enef)
     &       *enef**PDIs/(enef**PDIs + WDWid**PDIs) + wdul
        WC = WC0 + WC1*enef + (WCBw + wviso)
     &       *enef**PDIs/(enef**PDIs + WCWid**PDIs) + wcul
      ELSE
        WD = WD0 + WD1*BNDc + (enef - BNDc)*WDA1 + (WDBw + wiso)
     &       *EXP( - ALAwd*enef)*enef**PDIs/(enef**PDIs + WDWid**PDIs)
     &       + wdul
        WC = WC0 + WC1*BNDc + (enef - BNDc)*WCA1 + (WCBw + wviso)
     &       *enef**PDIs/(enef**PDIs + WCWid**PDIs) + wcul
      ENDIF
 
 
 
 
C     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
      rel = (EN + AMI)/AMI
      IF(rel.LT.1.D00)rel = 1.D00
      eirelm = SQRT(rel**2*AT**2 + 2.D+0*rel*ANEu*AT + ANEu**2)
      eirelt = AT*SQRT(rel**2*ANEu**2 + 2.D+0*rel*ANEu*AT + AT**2)
      derel = SQRT(2.D+0*rel*ANEu*AT + AT**2 + ANEu**2)
C        ENCON=(REL**2-1.D0)*AMI*AT*(ANEU+AT)/DEREL**2/2.D+0
      WW4 = 1.2064496D-2*eirelm*eirelt/(ANEu*eirelm + eirelt)/derel*apan
 
C     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      WW4 = 1.
      vso = VS*EXP( - WW4*ALAso*(enef + cde))
      IF(MEDis.NE.0)THEN
        IF(MEDis.NE.2)THEN
 
          dvso = DOM_INT_WV(EFErm,epaver,WSBw,WSWid,INT(PDIs),EN + cde,
     &           dtmp)
          vso = vso + dvso
        ENDIF
 
        IF(EA.GT.0.D0.AND.WC.NE.0.D0)THEN
          VRDc = wcp*DOM_INT_T1(EFErm,EA,EN)
     &           + ALFnew*DOM_INT_T2(EFErm,EA,EN) + VRDc
          IF(EN.GT.EFErm + EA)WC = WC + ALFnew*(SQRT(EN) + (EFErm + EA)
     &                             **1.5D0/(2.D0*EN)
     &                             - 1.5D0*SQRT(EFErm + EA))
        ENDIF
      ENDIF
 
      wso = WS0 + WS1*enef + WSBw*(enef + cde)
     &      **PDIs/((enef + cde)**PDIs + WSWid**PDIs)
      IF(WD.LT.0.)WD = 0.D0
      IF(WC.LT.0.)WC = 0.D0
 
      EN = eint
      WRITE(21,1020)EN, VR, VRDc, VD, WC, WD, vso, dvso, wso, VHF, vcul, 
     &              vculc, relpot
      IF(MEPri.NE.99)PRINT 1020, EN, VR, VRDc, VD, WC, WD, vso, dvso, 
     &                     wso, VHF, vcul, vculc, relpot
 1020 FORMAT(/2X,' POTENTIAL VALUES (II) FOR INCIDENT ENERGY=',F11.6,1X,
     &       'MeV:'//,3X,' VR=',F8.3,3X,'VRDC(DWv)=',F8.3,3X,'  DWs=',
     &       F8.3,3X,' Wv=',F8.3/3X,' Ws=',F8.3,3X,'      VSO=',F8.3,3X,
     &       ' DVSO=',F8.3,3X,'WSO=',F8.3/3X,'VHF=',F8.3,3X,
     &       '     VCUL=',F8.3,3X,'VCULC=',F8.3,3X,'RELPOT=',F8.3/)
 
      AR = AR0 + AR1*enef
      AW = AW0 + AW1*enef
      AC = AC0 + AC1*enef
      AD = AD0 + AD1*enef
      IF(enef.GT.BNDc)AD = AD0 + AD1*BNDc
      AS = AS0 + AS1*enef
 
 
      RK = RR + (LOG(VR/(EN-EL(NMAx))) + 10.)*AR
 
      IF(WD.EQ.0.)rkd = RK
      IF(WD.NE.0.)THEN
        rkd = RD + (LOG(WD/(EN-EL(NMAx))) + 10.)*AD
      ENDIF
      IF(rkd.GT.RK)RK = rkd
C       IF(MECHA.NE.0) RK=RK*4.D0
C      IF(MECHA.NE.0) RK=RK+ 3.0*RR
C      IF(RK.GT.30.) RK=30.
      rkc = 3.*(MECha*ZNUc*RR**2/EN)**0.3333333
 
 
 
      IF(rkc.GT.RK)RK = rkc
 
      IF(MECha.NE.0)RK = RK
 
 
 
      NH = RK/AR/0.3 + 1
      NH1 = RK/AD/0.8 + 1
      nh2 = RK/AC/0.8 + 1
      nh3 = RK*WNK(1)/0.5 + 1
      IF(NH1.GT.NH)NH = NH1
      IF(nh2.GT.NH)NH = nh2
      IF(nh3.GT.NH)NH = nh3
      IF(NH.GT.270)NH = 270
      las1 = LAS + 1
      las2 = (LAS + 2)/2
      IF(MEPot.NE.1)THEN
        las2 = LAS + 1
      ENDIF
      IF(MEPot.EQ.1)THEN
        IF(NPD.NE.0)THEN
          an = 0.
          DO l = 2, NPD, 2
            an = an + ABS(BET(l))*l*las2
          ENDDO
          NAN1 = an*25
          NAN1 = 2*NAN1 + 2
          IF(NAN1.LT.40)NAN1 = 40
          IF(NAN1.GT.298)NAN1 = 298
          NAN2 = NAN1 + 1
          ST = 1./(NAN2 - 1)
          DO n = 1, NAN2
            x = ST*(n - 1)
            a = 0.2820947917739D0
            b = x*a
            POL(1,n) = a
            DEF(n) = 1.
            DO k = 3, las1, 2
              kk = k - 1
              c = ((2.*kk - 1.D0)*x*b - (kk - 1.D0)*a)/kk
C     POL - SPHERICAL FUNCTIONS L-1,ANGLE
              POL((kk + 2)/2,n) = c*SQRT(2.D0*kk + 1.D0)
              IF(kk.LE.NPD)DEF(n) = DEF(n) + BET(kk)
     &                              *c*SQRT(2.D0*kk + 1.D0)
              a = b
              b = c
              kk = kk + 1
              c = ((2.*kk - 1.D0)*x*b - (kk - 1.D0)*a)/kk
              a = b
              b = c
            ENDDO
          ENDDO
        ENDIF
      ENDIF
      pim = 1.996852
      mnula = 300*NUR*las2
      lalas = 9
      lasc = 1
      IF(las2.GE.3)lalas = 18
      IF(las2.GE.3)lasc = 2
      mla = 300*las2
      mnu = 300*NUR
      DO nu1 = 1, NUR
        mnula1 = mnula*(nu1 - 1)
        mnu1 = mnu*(nu1 - 1)
        DO nu2 = 1, NUR
          mnula2 = mnula1 + mla*(nu2 - 1)
          mnu2 = mnu1 + 300*(nu2 - 1)
          EN = eint - (EL(nu1) + EL(nu2))/2
C     if(nu1.ne.nu2)EN=EINT-ABS(EL(NU1)-EL(NU2))/2.
          IF(nu1.NE.nu2)EN = eint - (EL(nu1) + EL(nu2))/2.
          IF(MEApp.EQ.1)EN = eint
 
 
 
C     SPHERICAL PART OF THE POTENTIAL AND COUPLING IN PARTITIONS  !!!!!!!!
 
          IF(NCA(nu1).EQ.NCA(nu2))THEN
 
 
            viso = CISo*(AT - 2.*ZNUc)/AT
            wiso = WCIso*(AT - 2.*ZNUc)/AT
            wviso = WDIso*(AT - 2.*ZNUc)/AT
            IF(MECha.EQ.0)viso = -CISo*(AT - 2.*ZNUc)/AT
            IF(MECha.EQ.0)wiso = -WCIso*(AT - 2.*ZNUc)/AT
            IF(MECha.EQ.0)wviso = -WDIso*(AT - 2.*ZNUc)/AT
 
 
 
 
 
 
            IF(MECha.EQ.1.AND.NCA(1).NE.NCA(nu1))
     &         viso = -CISo*(AT - 2.*ZNUc - 2.D0)/AT
            IF(MECha.EQ.1.AND.NCA(1).NE.NCA(nu1))
     &         wiso = -WCIso*(AT - 2.*ZNUc - 2.D0)/AT
            IF(MECha.EQ.1.AND.NCA(1).NE.NCA(nu1))
     &         wviso = -WDIso*(AT - 2.*ZNUc - 2.D0)/AT
 
 
C      WCP=Wv(Ef-Ea)=Wv(Ef+Ea)
 
            wcp = (WCBw + wviso)*(EA)**PDIs/(EA**PDIs + WCWid**PDIs)
 
 
 
 
 
            IF(MECha.EQ.1.AND.NCA(1).NE.NCA(nu1))CONz = 0.D0
            IF(MECha.EQ.0.AND.NCA(1).NE.NCA(nu1))CONz = conz12
            IF(MECha.EQ.1.AND.NCA(1).EQ.NCA(nu1))CONz = conz12
            IF(MECha.EQ.0.AND.NCA(1).NE.NCA(nu1))CONz = 0.D0
 
            IF(MECha.EQ.1.AND.NCA(1).NE.NCA(nu1))efenu12 = EFErmn
            IF(MECha.EQ.0.AND.NCA(1).NE.NCA(nu1))efenu12 = EFErmp
            IF(MECha.EQ.1.AND.NCA(1).EQ.NCA(nu1))efenu12 = EFErmp
            IF(MECha.EQ.0.AND.NCA(1).EQ.NCA(nu1))efenu12 = EFErmn
 
 
 
 
 
 
            IF(MECha.EQ.1.AND.NCA(1).NE.NCA(nu1))enef = EN - efenu12
            IF(MECha.EQ.1.AND.NCA(1).NE.NCA(nu1))EN = EN
 
            IF(MECha.EQ.1.AND.NCA(1).EQ.NCA(nu1))enef = EN - efenu12 - 
     &         cde12
            IF(MECha.EQ.1.AND.NCA(1).EQ.NCA(nu1))EN = EN - cde12
 
 
            IF(MECha.EQ.0.AND.NCA(1).NE.NCA(nu1))enef = EN - efenu12 - 
     &         cde12
            IF(MECha.EQ.0.AND.NCA(1).NE.NCA(nu1))EN = EN - cde12
 
            IF(MECha.EQ.0.AND.NCA(1).EQ.NCA(nu1))enef = EN - efenu12
            IF(MECha.EQ.0.AND.NCA(1).EQ.NCA(nu1))EN = EN
 
 
 
C     TWO OPTIONS FOR COULOMB CORRECTIONS
C
 
            coenh = 1.D0
 
 
            VRLanp = VRLa + coenh*viso
 
 
 
            vrdir = ((VR1 + 2.*VR2*enef + 3.*VR3*enef**2 - VRLa*ALAvr*
     &              EXP(-ALAvr*enef)))*(1.D00 + viso/(VR0 + VRLa))
     &              *( - 1.D0)
 
            IF(MEDis.NE.0)THEN
 
 
C     VHF DERIVATIVE AT EN
              eee = EN
              EN = EN - 0.005D+00
              ENCon = EN
 
              CALL VHFROM
              vhfm = VHF
 
              EN = EN + 0.01D+00
              ENCon = EN
 
              CALL VHFROM
              vhfp = VHF
 
              vrdir = (vhfm - vhfp)*100
              EN = eee
 
C     DISPERSIVE WC CONTRIBUTION TO VR
 
 
 
 
 
 
              vdisp = DOM_INT_WV(EFErm,epaver,WCBw + wviso,WCWid,
     &                INT(PDIs),EN,vdisd)
 
 
 
 
 
              t12d = 0.D0
              IF(EA.GT.0.D0.AND.WC.NE.0.D0)THEN
                t12p = wcp*DOM_INT_T1(EFErm,EA,EN + 0.01)
     &                 + ALFnew*DOM_INT_T2(EFErm,EA,EN + 0.01)
                t12m = wcp*DOM_INT_T1(EFErm,EA,EN - 0.01)
     &                 + ALFnew*DOM_INT_T2(EFErm,EA,EN - 0.01)
                t12d = (t12m - t12p)*50.
              ENDIF
              vrdirc = vdisd + t12d
 
 
              vculc = MECha*ZNUc/AT**0.333333333*CCOul*vrdirc
              wdul = 0.
              wcul = 0.
 
 
C     DISPERSIVE WD CONTRIBUTION TO VR
 
 
              IF(ALAwd.NE.0.D0)THEN
                vdp = DOM_INT_WS(EFErm,epaver,WDBw + wiso,WDWid,ALAwd,
     &                INT(PDIs),EN,vdd)
              ELSE
                vdp = DOM_INT_WV(EFErm,epaver,WDBw + wiso,WDWid,
     &                INT(PDIs),EN,vdd)
              ENDIF
 
              IF(EN.GT.WDShi.AND.ALAwd.EQ.0.D+00)THEN
                vdp1 = vdp
                vdd1 = vdd
                vdp = DOM_INT_WV(EFErm,WDShi,WDBw + wiso,WDWid2,
     &                INT(PDIs),EN,vdd)
                vdp = vdp1 - vdp
                vdd = vdd1 - vdd
              ENDIF
 
              IF(EN.GT.EA)vdp = vdp + VRD*(EN - EA)
              IF(EN.GT.EA)vdd = vdd + VRD
 
 
              vdcul = vdd*MECha*ZNUc/AT**0.333333333*CCOul
 
 
C
              pdis1 = PDIs - 1
 
C
              wdul = 0.
 
              wcul = 0.
            ENDIF
 
            vcul = MECha*ZNUc/AT**0.333333333*CCOul*vrdir
            IF(MECul.EQ.1)vcul = MECha*ZNUc/AT**0.333333333*CCOul
 
 
 
            VR = (VR0 + VR1*enef + VR2*enef**2 + VR3*enef**3 + 
     &           VRLa*EXP( - ALAvr*enef))*(1.D00 + viso/(VR0 + VRLa))
     &           + vcul
 
            IF(MEDis.NE.0)THEN
 
C    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
 
 
 
C     VHF AND VR AT EN
 
 
 
              ENCon = EN
 
              CALL VHFROM
 
              VR = VHF + vcul
              VRDc = vdisp + vculc
 
 
C         ENEF=ENCON-EFERM
 
C    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
 
 
 
 
 
              VD = vdp + vdcul
            ENDIF
 
 
 
 
 
 
            ene = enef
            IF(MECul.EQ.3)ene = ene + cde
            eneshid = ene - WDShi + EFErm
 
            IF(ene.LT.BNDc)THEN
              WD = WD0 + WD1*ene + (WDBw + wiso)*EXP( - ALAwd*ene)
     &             *ene**PDIs/(ene**PDIs + WDWid**PDIs) + wdul
              IF(ene + EFErm.GT.WDShi.AND.ALAwd.EQ.0.D+00)
     &           WD = WD - (WDBw + wiso)
     &           *eneshid**PDIs/(eneshid**PDIs + WDWid2**PDIs)
 
              WC = WC0 + WC1*ene + (WCBw + wviso)
     &             *ene**PDIs/(ene**PDIs + WCWid**PDIs) + wcul
            ELSE
              WD = WD0 + WD1*BNDc + (ene - BNDc)*WDA1 + (WDBw + wiso)
     &             *EXP( - ALAwd*ene)
     &             *ene**PDIs/(ene**PDIs + WDWid**PDIs) + wdul
              IF(ene + EFErm.GT.WDShi.AND.ALAwd.EQ.0.D+00)
     &           WD = WD - (WDBw + wiso)
     &           *eneshid**PDIs/(eneshid**PDIs + WDWid2**PDIs)
              WC = WC0 + WC1*BNDc + (ene - BNDc)*WCA1 + (WCBw + wviso)
     &             *ene**PDIs/(ene**PDIs + WCWid**PDIs) + wcul
            ENDIF
 
 
CCCCC
 
 
 
C     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            rel = (EN + AMI)/AMI
            IF(rel.LT.1.D00)rel = 1.D00
            eirelm = SQRT(rel**2*AT**2 + 2.D+0*rel*ANEu*AT + ANEu**2)
            eirelt = AT*SQRT(rel**2*ANEu**2 + 2.D+0*rel*ANEu*AT + AT**2)
            derel = SQRT(2.D+0*rel*ANEu*AT + AT**2 + ANEu**2)
C        ENCON=(REL**2-1.D0)*AMI*AT*(ANEU+AT)/DEREL**2/2.D+0
            WW4 = 1.2064496D-2*eirelm*eirelt/(ANEu*eirelm + eirelt)
     &            /derel*apan
 
C     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            WW4 = 1.
            vso = VS*EXP( - WW4*ALAso*(enef + cde))
 
            IF(MEDis.NE.0)THEN
              IF(MEDis.NE.2)THEN
 
 
                dvso = DOM_INT_WV(EFErm,epaver,WSBw,WSWid,INT(PDIs),
     &                 EN + cde,dtmp)
                vso = vso + dvso
              ENDIF
 
              IF(EA.GT.0.D0.AND.WC.NE.0.D0)THEN
                VRDc = wcp*DOM_INT_T1(EFErm,EA,EN)
     &                 + ALFnew*DOM_INT_T2(EFErm,EA,EN) + VRDc
                IF(EN.GT.EFErm + EA)
     &             WC = WC + ALFnew*(SQRT(EN) + (EFErm + EA)
     &             **1.5D0/(2.D0*EN) - 1.5D0*SQRT(EFErm + EA))
              ENDIF
            ENDIF
 
            wso = WS0 + WS1*enef + WSBw*(enef + cde)
     &            **PDIs/((enef + cde)**PDIs + WSWid**PDIs)
C     WD=WD+WISO
            IF(WD.LE.0.)WD = 0.
            IF(WC.LE.0.)WC = 0.
            AR = AR0 + AR1*enef
            AW = AW0 + AW1*enef
            AC = AC0 + AC1*enef
            AD = AD0 + AD1*enef
            IF(enef.GT.BNDc)AD = AD0 + AD1*BNDc
            AS = AS0 + AS1*enef
            IF(MEPot.NE.1)THEN
              rar = RR/AR
              rac = RC/AC
              raw = RW/AW
              rad = RD/AD
            ENDIF
            m = 8
            m4 = 4*m
            kl = 0
            in = 1
            kit = m
            nit = 1
            ik = m4 - 1
C
            STEp = RK/NH
            step1 = STEp/m
C
            DO k = 1, 2
              DO i = in, ik
                lalasi = lalas*(i - 1)
                l1 = (i - 1)*las2 + mnula2
                l1ap = (i - 1)*las2
                IF(k.EQ.2.AND.i.EQ.ik)THEN
                  R = 0.
                  ir = 1
                  i1 = mnula2
                  iic = mnu2 + 1
                ELSE
                  R = step1*(i - kl)
                  vslc = -mup*CONz/RZ**3*pim
                  IF(R.GT.RZ)vslc = -mup*CONz/R**3*pim
                  x = (R - RS)/AS
                  ii = mnu2 + i
                  IF(x.GT.23)THEN
                    ex1 = EXP( - x)
                    vslf = -pim*ex1/AS/R
                  ELSE
                    ex1 = EXP(x)
                    vslf = -pim*ex1/AS/R/(1.D0 + ex1)/(1.D0 + ex1)
                  ENDIF
                  VL(i) = 1.D0/R/R
                  VSL(ii) = vslf*vso*ww
                  WSL(ii) = vslf*wso*ww
                  IF(MERel.EQ.1.OR.MERel.EQ.3)VSL(ii)
     &               = vslf*vso*ww/relpot
                  IF(MERel.EQ.1.OR.MERel.EQ.3)WSL(ii)
     &               = vslf*wso*ww/relpot
                  IF(i/kit*kit.EQ.i)THEN
                    ir = i/kit + nit
                    lalasr = lalas*(ir - 1)
                    i1 = (ir - 1)*las2 + mnula2
                    iic = mnu2 + ir
                    PL(ir) = VL(i)
                    PSL(iic) = VSL(ii)
                    WPSl(iic) = WSL(ii)
                  ENDIF
                ENDIF
                IF(AZ.GT.0.)THEN
                  CALL SPHEPOT
                  pz = 2.*DE*CONz
                ELSE
                  pz = CONz/RZ*(3. - R*R/RZ/RZ)
                  IF(R.GE.RZ)pz = 2.*CONz/R
                ENDIF
                IF(MEPot.EQ.1)THEN
                  DO l = 1, las2
 
C      IF(NU1.NE.NU2.AND.L.EQ.1) GO TO 9
C      IF(NCA(NU1).NE.NCA(NU2)) GO TO 9
 
                    pdc = 0.
                    ll2 = l1 + l
                    ll2ap = l1ap + l
                    IF(MEApp.NE.0)THEN
                      IF(nu1.NE.nu2)GOTO 5
                      IF(nu1.NE.1)GOTO 5
                    ENDIF
                    DE = 1.
                    IF(l.NE.1)THEN
                      lac = 2*(l - 1)
                      IF(lac.LE.NPD)THEN
                        IF(R.LE.RZ)pdc = 6.*CONz/(2.*lac + 1.)
     &                     *R**lac/RZ**(lac + 1)*BET(lac)
                        IF(R.GT.RZ)pdc = 6.*CONz/(2.*lac + 1.)
     &                     *RZ**lac/R**(lac + 1)*BET(lac)
 
C     HIGHER COULOMB MULTIPOLES AS PROPOSES BY SATCHLER
 
                        IF(las2.NE.1)THEN
                          IF(R.LE.RZ)fpvc = 6.*CONz/(2.*lac + 1)
     &                       *(1. - lac)*R**lac/RZ**(lac + 1)
                          IF(R.GT.RZ)fpvc = 6.*CONz/(2.*lac + 1)
     &                       *(lac + 2)*RZ**lac/R**(lac + 1)
                          IF(lac.EQ.2)pdc = pdc + 
     &                       fpvc*(0.180223752*BET(2)
     &                       + 0.241795536*BET(4))*BET(2)
                          IF(lac.EQ.4)pdc = pdc + 
     &                       fpvc*(0.241795536*BET(2)
     &                       + 0.163839774*BET(4))*BET(2)
                          IF(lac.EQ.6)pdc = pdc + 
     &                       fpvc*0.238565132*BET(2)*BET(4)
                        ENDIF
                      ENDIF
                    ENDIF
C
                    IF(NPD.NE.0)DE = DEF(1)
                    CALL POTET
                    IF(NPD.EQ.0)THEN
                      PV(ll2) = VP*ww
                      IF(l.EQ.1)PV(ll2) = PV(ll2) + pz
                      PW(ll2) = WP*ww
                      IF(MERel.EQ.3)PW(ll2) = PW(ll2)/relpot
                    ELSE
                      vv = VP*POL(l,1)
                      wv = WP*POL(l,1)
                      n = NAN2
                      DE = DEF(n)
                      CALL POTET
                      vv = vv + VP*POL(l,n)
                      wv = wv + WP*POL(l,n)
                      v4 = 0.
                      w4 = 0.
                      DO n = 2, NAN2, 2
                        DE = DEF(n)
                        CALL POTET
                        v4 = v4 + VP*POL(l,n)
                        w4 = w4 + WP*POL(l,n)
                      ENDDO
                      v2 = 0.
                      w2 = 0.
                      DO n = 3, NAN1, 2
                        DE = DEF(n)
                        CALL POTET
                        v2 = v2 + VP*POL(l,n)
                        w2 = w2 + WP*POL(l,n)
                      ENDDO
 
C      write(21,1799) vr,vd,vrdc,wc,wd
C1799 format(6e12.7)
C      pause 1
 
 
 
 
 
                      PV(ll2) = (vv + 4.*v4 + 2.*v2)
     &                          *ST*ww*4.1887902047864D0
                      PW(ll2) = (wv + 4.*w4 + 2.*w2)
     &                          *ST*ww*4.1887903047864D0
                      IF(MERel.EQ.3)PW(ll2) = PW(ll2)/relpot
                      IF(l.NE.1)PV(ll2) = PV(ll2) + pdc
                      IF(l.EQ.1)THEN
                        PV(ll2) = PV(ll2)*0.2820947917739D0 + pz
                        PW(ll2) = PW(ll2)*0.2820947917739D0
                      ENDIF
                    ENDIF
                    GOTO 10
    5               PV(ll2) = PV(ll2ap)
                    PW(ll2) = PW(ll2ap)
   10               IF(i/kit*kit.EQ.i)THEN
                      ii2 = i1 + l
                      PR(ii2) = PV(ll2)
                      PI(ii2) = PW(ll2)
                    ENDIF
                  ENDDO
                ELSE
                  IF(las2.NE.1)THEN
                    DO lac = 1, 9
                      lalali = lalasi + lasc*(lac - 1)
                      lalalr = lalasr + lasc*(lac - 1)
                      lam = lac - 1
                      IF(R.EQ.0.)rlam = 0.0
                      IF(R.EQ.0.)rlam1 = 0.0
                      IF(R.NE.0.)rlam = R**lam
                      IF(R.NE.0.)rlam1 = R**(lam + 1)
                      PVC(lalali + 1) = 6.*CONz/(2.*lam + 1)
     &                                  *rlam/RZ**(lam + 1)
                      IF(R.GT.RZ)PVC(lalali + 1) = 6.*CONz/(2.*lam + 1)
     &                   *RZ**lam/rlam1
C     PVC(LALALI+1)=0.0
                      IF(las2.NE.2)THEN
                        PVC(lalali + 2) = 6.*CONz/(2.*lam + 1)
     &                    *(1. - lam)*rlam/RZ**(lam + 1)
C     PVC(LALALI+2)=6.*CONZ/(2.*LAM+1)*(LAM+2.)*RLAM/RZ**(LAM+1)
                        IF(R.GT.RZ)PVC(lalali + 2)
     &                     = 6.*CONz/(2.*lam + 1)*(lam + 2)
     &                     *RZ**lam/rlam1
                      ENDIF
C     PVC(LALALI+2)=0.0
                    ENDDO
                  ENDIF
                  nt0 = l1 + 1
                  nt1 = l1 + 2
                  nt2 = l1 + 3
                  nt3 = l1 + 4
                  nt4 = l1 + 5
                  x = (R - RR)/AR
                  IF(x.GT.23.)THEN
                    ex1 = EXP( - x)
                    PV(nt0) = -VR*ex1*ww + pz
                    IF(LAS.NE.0)THEN
C     PV(NT1)=PV(NT0)*RAR
                      PV(nt1) = (PV(nt0) - pz)*rar
                      IF(LAS.NE.1)THEN
                        PV(nt2) = PV(nt1)*rar/2.D0
                        IF(LAS.NE.2)THEN
                          PV(nt3) = PV(nt2)*rar/3.D0
                          IF(LAS.NE.3)THEN
                            PV(nt4) = PV(nt3)*rar/4.D0
                          ENDIF
                        ENDIF
                      ENDIF
                    ENDIF
                  ELSE
                    ex = EXP(x)
                    ex1 = 1.D0 + ex
                    exd = 1.D0/ex1
                    exdm = 1.D0 - exd
                    PV(nt0) = -VR*exd*ww + pz
                    IF(LAS.NE.0)THEN
                      PV(nt1) = (PV(nt0) - pz)*exdm*rar
                      IF(LAS.NE.1)THEN
                        y1 = 1.D0 - 2.D0*exd
                        PV(nt2) = PV(nt1)*y1*rar/2.D0
                        IF(LAS.NE.2)THEN
                          y2 = 1.D0 - 6.D0*exd*exdm
                          PV(nt3) = PV(nt2)/y1*y2*rar/3.D0
                          IF(LAS.NE.3)THEN
                            y3 = 1. - 
     &                           exd*(14.D0 - exd*(36.D0 - 24.D0*exd))
                            PV(nt4) = PV(nt3)/y2*y3*rar/4.D0
                          ENDIF
                        ENDIF
                      ENDIF
                    ENDIF
                  ENDIF
                  x = (R - RC)/AC
                  y = (R - RW)/AW
                  xx = y*y
                  IF(xx.GT.180.)exx = 0.
                  IF(xx.LE.180.)exx = EXP( - xx)
                  IF(x.GT.23.)THEN
                    ex1 = EXP( - x)
                    wcc = -WC*ALF*ex1*ww
                    www = -WC*(1. - ALF)*exx*ww
                    IF(MERel.EQ.3)wcc = wcc/relpot
                    IF(MERel.EQ.3)www = www/relpot
                    PW(nt0) = wcc + www
                    PV(nt0) = PV(nt0) + wcc*VRDc/WC
                    IF(LAS.NE.0)THEN
                      wcc = wcc*rac
                      www = www*2.*raw*y
                      PW(nt1) = wcc + www
                      PV(nt1) = PV(nt1) + wcc*VRDc/WC
                      IF(LAS.NE.1)THEN
                        wcc = wcc*rac/2.
                        y1 = 2.*y*y - 1.
                        www = www/y*y1*raw/2.
                        PW(nt2) = wcc + www
                        PV(nt2) = PV(nt2) + wcc*VRDc/WC
                        IF(LAS.NE.2)THEN
                          wcc = wcc*rac/3.
                          y2 = y**3*2. - y*3.
                          www = www/y1*y2*raw*2./3.
                          PW(nt3) = wcc + www
                          PV(nt3) = PV(nt3) + wcc*VRDc/WC
                          IF(LAS.NE.3)THEN
                            wcc = wcc*rac/4.
                            www = www/y2*(y**4*4. - y*y*12. + 3.)*raw/4.
                            PW(nt4) = wcc + www
                            PV(nt4) = PV(nt4) + wcc*VRDc/WC
                          ENDIF
                        ENDIF
                      ENDIF
                    ENDIF
                  ELSE
                    ex = EXP(x)
                    ex1 = 1.D0 + ex
                    exd = 1.D0/ex1
                    exdm = 1.D0 - exd
                    wcc = -WC*ALF*exd*ww
                    www = -WC*(1.D0 - ALF)*exx*ww
                    IF(MERel.EQ.3)wcc = wcc/relpot
                    IF(MERel.EQ.3)www = www/relpot
                    PW(nt0) = wcc + www
                    PV(nt0) = PV(nt0) + wcc*VRDc/WC
                    IF(LAS.NE.0)THEN
                      wcc = wcc*exdm*rac
                      www = www*2.*raw*y
                      PW(nt1) = wcc + www
                      PV(nt1) = PV(nt1) + wcc*VRDc/WC
                      IF(LAS.NE.1)THEN
                        yc1 = 1. - 2.*exd
                        wcc = wcc*yc1*rac/2.
                        y1 = 2.*y*y - 1.
                        www = www/y*y1*raw/2.
                        PW(nt2) = wcc + www
                        PV(nt2) = PV(nt2) + wcc*VRDc/WC
                        IF(LAS.NE.2)THEN
                          yc2 = 1. - 6.*exd*exdm
                          wcc = wcc/yc1*yc2*rac/3.
                          y2 = y**3*2. - y*3.
                          www = www/y1*y2*raw*2./3.
                          PW(nt3) = wcc + www
                          PV(nt3) = PV(nt3) + wcc*VRDc/WC
                          IF(LAS.NE.3)THEN
                            yc3 = 1. - exd*(14. - exd*(36. - 24.*exd))
                            wcc = wcc/yc2*yc3*rac/4.
                            www = www/y2*(y**4*4. - y*y*12. + 3.)*raw/4.
                            PW(nt4) = wcc + www
                            PV(nt1) = PV(nt1) + wcc*VRDc/WC
                          ENDIF
                        ENDIF
                      ENDIF
                    ENDIF
                  ENDIF
                  x = (R - RD)/AD
                  IF(x.GT.23.)THEN
                    ex1 = EXP( - x)
                    wdd = -4.D0*WD*ex1*ww
                    IF(MERel.EQ.3)wdd = wdd/relpot
                    PW(nt0) = PW(nt0) + wdd
                    PV(nt0) = PV(nt0) + wdd/WD*VD
                    IF(LAS.NE.0)THEN
                      wdd = wdd*rad
                      PW(nt1) = PW(nt1) + wdd
                      PV(nt1) = PV(nt1) + wdd/WD*VD
                      IF(LAS.NE.1)THEN
                        wdd = wdd*rad/2.D0
                        PW(nt2) = PW(nt2) + wdd
                        PV(nt2) = PV(nt2) + wdd/WD*VD
                        IF(LAS.NE.2)THEN
                          wdd = wdd*rad/3.D0
                          PW(nt3) = PW(nt3) + wdd
                          PV(nt3) = PV(nt3) + wdd/WD*VD
                          IF(LAS.NE.3)THEN
                            wdd = wdd*rad/4.
                            PW(nt4) = PW(nt4) + wdd
                            PV(nt4) = PV(nt4) + wdd/WD*VD
                          ENDIF
                        ENDIF
                      ENDIF
                    ENDIF
                  ELSE
                    ex = EXP(x)
                    ex1 = 1.D0 + ex
                    exd = 1.D0/ex1
                    exdm = 1.D0 - exd
                    wdd = -4.D0*WD*exdm*exd*ww
                    IF(MERel.EQ.3)wdd = wdd/relpot
                    PW(nt0) = PW(nt0) + wdd
                    PV(nt0) = PV(nt0) + wdd/WD*VD
                    IF(LAS.NE.0)THEN
                      y1 = 1.D0 - 2.D0*exd
                      wdd = wdd*y1*rad
                      PW(nt1) = PW(nt1) + wdd
                      PV(nt1) = PV(nt1) + wdd/WD*VD
                      IF(LAS.NE.1)THEN
                        y2 = 1.D0 - 6.D0*exd*exdm
                        wdd = wdd/y1*y2*rad/2.D0
                        PW(nt2) = PW(nt2) + wdd
                        PV(nt2) = PV(nt2) + wdd/WD*VD
                        IF(LAS.NE.2)THEN
                          y3 = 1.D0 - 
     &                         exd*(14.D0 - exd*(36.D0 - 24.D0*exd))
                          wdd = wdd/y2*y3*rad/3.D0
                          PW(nt3) = PW(nt3) + wdd
                          PV(nt3) = PV(nt3) + wdd/WD*VD
                          IF(LAS.NE.3)THEN
                            y4 = 1.D0 - 
     &                           exd*(30.D0 - exd*(150.D0 - exd*(240.D0-
     &                           120.D0*exd)))
                            wdd = wdd/y3*y4*rad/4.D0
                            PW(nt4) = PW(nt4) + wdd
                            PV(nt4) = PV(nt4) + wdd/WD*VD
                          ENDIF
                        ENDIF
                      ENDIF
                    ENDIF
                  ENDIF
                  IF(i/kit*kit.EQ.i)THEN
                    PR(i1 + 1) = PV(nt0)
                    PI(i1 + 1) = PW(nt0)
                    IF(LAS.NE.0)THEN
                      PR(i1 + 2) = PV(nt1)
                      PI(i1 + 2) = PW(nt1)
                      DO lac = 1, 8
                        lalali = lalasi + lasc*(lac - 1)
                        lalalr = lalasr + lasc*(lac - 1)
                        lam = lac - 1
                        PRC(lalalr + 1) = PVC(lalali + 1)
                        IF(las2.NE.2)THEN
                          PRC(lalalr + 2) = PVC(lalali + 2)
                        ENDIF
                      ENDDO
                      IF(LAS.NE.1)THEN
                        PR(i1 + 3) = PV(nt2)
                        PI(i1 + 3) = PW(nt2)
                        IF(LAS.NE.2)THEN
                          PR(i1 + 4) = PV(nt3)
                          PI(i1 + 4) = PW(nt3)
                          IF(LAS.NE.3)THEN
                            PR(i1 + 5) = PV(nt4)
                            PI(i1 + 5) = PW(nt4)
                          ENDIF
                        ENDIF
                      ENDIF
                    ENDIF
                  ENDIF
                ENDIF
              ENDDO
              kl = m4 - 4
              in = m4
              kit = 1
              nit = 5 - m4
              ik = NH + kl + 2
              step1 = STEp
            ENDDO
            NH1 = NH + kl
            NHI = NH + 2
          ENDIF
 
C      write(21,1799) vr,vd,vrdc,wc,wd,enef,WDBW,WISO
C 1799 format(8e12.5)
C      pause 1
 
        ENDDO
      ENDDO
      EN = eint
      RR = rrr
      RZ = rzin
 
C      write(21,1799) vr,vd,vrdc,wc,wd
C 1799 format(6e12.5)
C      pause 1
      CCOul = cccoul
      CONz = conzz
 
C           WRITE(21,9997) RK, STEP,CONZ ,3.302222, NH1
C9997  FORMAT(4E12.5,I3)
      CALL RIPAT1
      RETURN
      END SUBROUTINE RIPAT
 
!---------------------------------------------------------------------------
C     ***********************************************************
      SUBROUTINE RIPAT1
C     ***********************************************************
      IMPLICIT DOUBLE PRECISION(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: AC, AC0, AC1, AD, AD0, AD1, ALAso, ALAvr, ALAwd, ALF, 
     &          ALFnew, AMI, ANEu, AR, AR0, AR1, AS, AS0, AS1, ASP, AT, 
     &          AW, AW0, AW1, AZ, BNDc, CCOul, CISo, CONz, DE, EA, 
     &          EFErm, EFErmn, EFErmp, EN, ENCon, ETA, PDIs, R, RC, RD, 
     &          RK, RR, RRBwc, RRWid, RS, RW, RZ, RZBwc, RZWid, ST, 
     &          STEp, VD, VHF, VP, VR, VR0, VR1, VR2, VR3, VRD, VRDc, 
     &          VRLa, VRLanp, VS, WC, WC0, WC1, WCA1, WCBw, WCIso, 
     &          WCWid, WD, WD0, WD1, WDA1, WDBw, WDIso, WDShi, WDWid
      REAL*8, DIMENSION(10) :: BET
      REAL*8, DIMENSION(20,90) :: COPh
      REAL*8, DIMENSION(720000) :: CVNc
      REAL*8, DIMENSION(300) :: DEF, PL, PZI, VL
      REAL*8, DIMENSION(20) :: EL, ES, WN, WNK
      INTEGER, DIMENSION(20) :: JJ, JO, KO, NCA, NNB, NNG, NNO, NPI, 
     &                          NPO, NTU
      INTEGER :: LAS, MEApp, MECha, MECul, MEDis, MEHam, MEHao, MEJob, 
     &           MEPot, MEPri, MERel, MERip, MERrr, MERzz, MESha, MESho, 
     &           MESol, MEVol, NAN1, NAN2, NECi, NH, NH1, NHI, NMAx, 
     &           NPD, NUR
      REAL*8, DIMENSION(600000) :: PI, PR, PV, PW
      REAL*8, DIMENSION(120000) :: PII, PRR, PSL, PVV, PWW, VSL, WPSl, 
     &                             WSL
      REAL*8, DIMENSION(5,300) :: POL
      REAL*8, DIMENSION(5400) :: PRC, PVC
      REAL*8 :: WDWid2, WP, WS0, WS1, WSBw, WSWid, WW4, ZNUc
      COMMON /AB    / DEF, VL, VSL, POL, PV, PW, WSL
      COMMON /ABCOUL/ PVC, PRC, CVNc
      COMMON /ABEC  / PL, PSL, PR, PI, WPSl
      COMMON /COUL  / CONz, ETA, COPh
      COMMON /COULON/ PZI
      COMMON /DISPE / VD, VRDc, EA, WDIso
      COMMON /DISPE2/ VRD, WDShi, WDWid2, ALFnew
      COMMON /ECI   / NECi, NHI
      COMMON /ENA   / EN, EL, BET, NUR, NMAx, NPD, LAS
      COMMON /MENU  / MEJob, MEPot, MEHam, MECha, MEPri, MESol, MESha, 
     &                MESho, MEHao, MEApp, MEVol, MERel, MECul, MERzz, 
     &                MERrr, MEDis, MERip
      COMMON /POT1  / VR3, VR2, VR1, VR0, WC1, WC0, WD1, WD0, VS, AC0, 
     &                AR0, AW0, AD0, AS0
      COMMON /POT2  / AR1, AC1, AW1, AD1, AS1
      COMMON /POT3  / BNDc, WDA1, WCA1, CCOul, CISo, WCIso, WS0, WS1
      COMMON /POTB  / WNK, WN, VR, WC, WD
      COMMON /POTD  / ALAvr, VRLa, WCBw, WCWid, WDBw, WDWid, ALAwd, 
     &                EFErmn, EFErmp, ALAso, PDIs, WSBw, WSWid, RRBwc, 
     &                RRWid, RZBwc, RZWid
      COMMON /POTEB / R, DE, VP, WP
      COMMON /POTPN / PVV, PWW, PRR, PII
      COMMON /QNB   / JO, NPO, KO, NCA
      COMMON /RAD   / RR, RC, RD, RW, RS, AR, AC, AW, AD, AS, ALF, AT, 
     &                ANEu, RZ, ZNUc, ASP, AZ
      COMMON /RIP   / ST, NH, NAN1, NAN2
      COMMON /SHEMM / ES, JJ, NTU, NNB, NNG, NNO, NPI
      COMMON /STR   / STEp, RK, NH1
      COMMON /VHFS  / WW4, VHF, ENCon, VRLanp, EFErm, AMI
C
C Local variables
C
      REAL*8 :: apan, carbun, cccoul, ccde, cde, coenh, conzz, 
     &          davern, derel, eee, efenu12, eint, eirelm, 
     &          eirelt, ene, enef, eneshid, epaver, ex, ex1, exd, exdm, 
     &          exx, pz, rac, rad, rar, raw, rel, 
     &          relpot, rrr, rzin, step1, t12d, t12m, t12p, v2, v4, 
     &          vcul, vculc, vdcul, vdd, vdd1, vdisd, vdisp, vdp, vdp1, 
     &          vhfm, vhfp, viso, vrdir, vrdirc, vv, w2, w4, wcbwnp, 
     &          wcc, wcp, wcul, wdbwnp, wdd, wdul, wiso, wv, wviso, ww, 
     &          www, x, xx, y, y1, y2, y3, y4, yc1
      REAL*8 :: DOM_INT_T1, DOM_INT_WS, DOM_INT_WV
      INTEGER :: i, i1, ii2, ik, in, ir, k, kit, kl, l, l1, 
     &           l1ap, lalas, las2, ll2, 
     &           m, m4, mla, mnu, mnu1, mnu2, mnula, mnula1, 
     &           mnula2, n, nit, nt0, nt1, nt2, nt3, nt4, nu1, nu2
      INTEGER :: INT
      REAL*8 :: yc2, yc3
C
C*** End of declarations rewritten by SPAG
C
C     BELOW CARD IS NECESSARY IF MEMORY IS LESS THAN 32Mb
 
      conzz = CONz
 
      apan = ANEu/1.0086652
 
      ccde = 0.0
      cccoul = CCOul
      CCOul = CCOul/2.0
      IF(MECul.GE.2)ccde = CCOul
      IF(MECul.GE.2)CCOul = 0.0
      cde = ccde*MECha*ZNUc/AT**(1.D0/3.D0)
 
 
      VRDc = 0.0
      VD = 0.0
      vdisp = 0.0
      carbun = 931.49378
      AMI = 939.56536
      IF(MECha.EQ.1)AMI = 938.272029
      davern = 0.00
      EFErm = EFErmn
      epaver = EFErm + davern
      IF(MECha.EQ.1)EFErm = EFErmp
 
 
C           THREE CARDS FOR U238 CHARGE, DELETE THEN FOR USUAL CALCUL.
C           TWO CARDS AT THE END OF SUBROUTINE
      rzin = RZ
      IF(MERzz.NE.0)THEN
        IF(MERzz.EQ.1)RZ = RZ*(1.D00 - RZBwc*(EN - EFErm)**PDIs/((EN-
     &                     EFErm)**PDIs + RZWid**PDIs))
      ENDIF
C             RZ=9.100
C             IF(EN.GT.26.) RZ=9.100-0.31324222*(EN-26.)
C             IF(EN.GT.26.) RZ=RZ-(RZ-6.28082)/9.*(EN-26.)
C             IF(EN.GT.35.) RZ=6.28082
      rrr = RR
      apan = ANEu/1.0086652
C
C           RR ENERGY DEPENDENCE
C
C     IF(MERRR.EQ.1)   RR=RR*(1.D00-RRBWC*EN**2/(EN**2+RRWID**2))
      IF(MERrr.EQ.1)RR = RR*(1.D00 - RRBwc*(EN - EFErm)**PDIs/((EN-EFErm
     &                   )**PDIs + RRWid**PDIs))
 
 
 
 
 
      rel = (EN + AMI)/AMI
      IF(MERel.EQ.0)rel = 1.D+0
      IF(rel.NE.1.)relpot = 2.D+0*(EN + AMI)/(2.D+0*AMI + EN)
 
 
      eirelm = SQRT(rel**2*AT**2 + 2.D+0*rel*ANEu*AT + ANEu**2)
      eirelt = AT*SQRT(rel**2*ANEu**2 + 2.D+0*rel*ANEu*AT + AT**2)
      derel = SQRT(2.D+0*rel*ANEu*AT + AT**2 + ANEu**2)
      ww = 4.8257984E-2*eirelm*eirelt/(ANEu*eirelm + eirelt)/derel*apan
      IF(MERel.EQ.1.OR.MERel.EQ.3)ww = ww*relpot
 
 
 
 
 
 
 
 
      las2 = (LAS + 2)/2
      IF(MEPot.NE.1)THEN
        las2 = LAS + 1
      ENDIF
 
      mnula = 300*NUR*las2
      lalas = 9
      IF(las2.GE.3)lalas = 18
      mla = 300*las2
      mnu = 300*NUR
 
      eint = EN
 
      DO nu1 = 1, NUR
        mnula1 = mnula*(nu1 - 1)
        mnu1 = mnu*(nu1 - 1)
        DO nu2 = 1, NUR
          mnula2 = mnula1 + mla*(nu2 - 1)
          mnu2 = mnu1 + 300*(nu2 - 1)
          EN = eint - (EL(nu1) + EL(nu2))/2
C       if(nu1.ne.nu2)EN=EINT-ABS(EL(NU1)-EL(NU2))/2.
          IF(nu1.NE.nu2)EN = eint - (EL(nu1) + EL(nu2))/2.
          IF(MEApp.EQ.1)EN = eint
 
 
 
C     COUPLINGIN BETWEEN LEVELS OF THE DIFFERENT PARTITIONS    !!!!!!!!
 
 
          IF(NCA(nu1).NE.NCA(nu2))THEN
            IF(MEHam.NE.1)THEN
              IF(NTU(nu1).NE.NTU(nu2).OR.NNB(nu1).NE.NNB(nu2).OR.
     &           NNG(nu1).NE.NNG(nu2).OR.NNO(nu1).NE.NNO(nu2))CYCLE
            ENDIF
 
 
            efenu12 = (EFErmn + EFErmp)/2.D0
 
            enef = EN - efenu12 - cde
 
            EN = EN - cde
 
 
 
            coenh = 1.D0
C
C     IF(MECUL.GE.2)    COENH=1.D0+ALAVR*VRLA*EXP(-ALAVR*ENEF)
 
 
 
 
            viso = CISo*2.D0*SQRT(AT - 2.D0*ZNUc)/AT
            wiso = WCIso*2.D0*SQRT(AT - 2.D0*ZNUc)/AT
            wviso = WDIso*2.D0*SQRT(AT - 2.D0*ZNUc)/AT
            VRLanp = viso*coenh
            wcbwnp = wviso
            wdbwnp = wiso
 
 
C      WCP=Wv(Ef-Ea)=Wv(Ef+Ea)
 
            wcp = (wcbwnp)*(EA)**PDIs/(EA**PDIs + WCWid**PDIs)
 
C      write(21,1799) vcp,wcbwnp
 
C      pause 89
 
 
C     TWO OPTIONS FOR COULOMB CORRECTIONS
C
 
 
            vrdir = (VR1 + 2.*VR2*enef + 3.*VR3*enef**2 - 
     &              VRLa*ALAvr*EXP( - ALAvr*enef))*viso/(VR0 + VRLa)
     &              *( - 1.D0)
 
 
            IF(MEDis.NE.0)THEN
C    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C     VHF DERIVATIVE AT EN
              eee = EN
              EN = EN - 0.005D+00
              ENCon = EN
 
              CALL VHFROM
              vhfm = VHF
 
              EN = EN + 0.01D+00
              ENCon = EN
 
              CALL VHFROM
              vhfp = VHF
 
              vrdir = (vhfm - vhfp)*100
              EN = eee
 
 
 
C     DISPERSIVE CONTRIBUTION TO VR
 
              vdisp = DOM_INT_WV(EFErm,epaver,wcbwnp,WCWid,INT(PDIs),EN,
     &                vdisd)
 
 
 
 
 
C     NO ISOSCALOR PART OF Wv
 
              IF(EA.GT.0.D0)THEN
                t12p = wcp*DOM_INT_T1(EFErm,EA,EN + 0.01)
                t12m = wcp*DOM_INT_T1(EFErm,EA,EN - 0.01)
                t12d = (t12m - t12p)*50.
              ENDIF
 
 
 
 
 
              vrdirc = vdisd + t12d
 
 
 
 
              vculc = MECha*ZNUc/AT**0.333333333*CCOul*vrdirc
              wdul = 0.
              wcul = 0.
 
 
 
 
C     DISPERSIVE WD CONTRIBUTION TO VR
 
 
 
              IF(ALAwd.NE.0.D0)THEN
                vdp = DOM_INT_WS(EFErm,epaver,wdbwnp,WDWid,ALAwd,
     &                INT(PDIs),EN,vdd)
              ELSE
                vdp = DOM_INT_WV(EFErm,epaver,wdbwnp,WDWid,INT(PDIs),EN,
     &                vdd)
              ENDIF
 
              IF(EN.GT.WDShi.AND.ALAwd.EQ.0.D+00)THEN
                vdp1 = vdp
                vdd1 = vdd
                vdp = DOM_INT_WV(EFErm,WDShi,wdbwnp,WDWid2,INT(PDIs),EN,
     &                vdd)
                vdp = vdp1 - vdp
                vdd = vdd1 - vdd
              ENDIF
              vdcul = vdd*MECha*ZNUc/AT**0.333333333*CCOul
              wdul = 0.
              wcul = 0.
            ENDIF
 
            vcul = MECha*ZNUc/AT**0.333333333*CCOul*vrdir
            IF(MECul.EQ.1)vcul = MECha*ZNUc/AT**0.333333333*CCOul
 
C
 
            VR = (VR0 + VR1*enef + VR2*enef**2 + VR3*enef**3 + 
     &           VRLa*EXP( - ALAvr*enef))*viso/(VR0 + VRLa) + vcul
 
            IF(MEDis.NE.0)THEN
 
C    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C     VHF AND VR AT EN
 
              ENCon = EN
 
              CALL VHFROM
 
 
              VR = VHF + vcul   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
              VRDc = vdisp + vculc
 
 
C         ENEF=ENCON-EFERM
 
C    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
 
 
 
 
 
              VD = vdp + vdcul
            ENDIF
 
 
            ene = enef               !!!!!!!!!!!!!!!!!!!!!!!!
 
            IF(MECul.EQ.3)ene = ene + cde
 
 
            eneshid = ene - WDShi + EFErm
 
            IF(ene.LT.BNDc)THEN
              WD = WD0 + WD1*ene + (wdbwnp)*EXP( - ALAwd*ene)
     &             *ene**PDIs/(ene**PDIs + WDWid**PDIs) + wdul
              IF(ene + EFErm.GT.WDShi.AND.ALAwd.EQ.0.D+00)
     &           WD = WD - (wdbwnp)
     &           *eneshid**PDIs/(eneshid**PDIs + WDWid2**PDIs)
 
              WC = WC0 + WC1*ene + (wcbwnp)
     &             *ene**PDIs/(ene**PDIs + WCWid**PDIs) + wcul
            ELSE
              WD = WD0 + WD1*BNDc + (ene - BNDc)*WDA1 + (wdbwnp)
     &             *EXP( - ALAwd*ene)
     &             *ene**PDIs/(ene**PDIs + WDWid**PDIs) + wdul
              IF(ene + EFErm.GT.WDShi.AND.ALAwd.EQ.0.D+00)
     &           WD = WD - (wdbwnp)
     &           *eneshid**PDIs/(eneshid**PDIs + WDWid2**PDIs)
              WC = WC0 + WC1*BNDc + (ene - BNDc)*WCA1 + (wcbwnp)
     &             *ene**PDIs/(ene**PDIs + WCWid**PDIs) + wcul
            ENDIF
 
 
 
 
 
 
 
 
C     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            rel = (EN + AMI)/AMI
            IF(rel.LT.1.D00)rel = 1.D00
            eirelm = SQRT(rel**2*AT**2 + 2.D+0*rel*ANEu*AT + ANEu**2)
            eirelt = AT*SQRT(rel**2*ANEu**2 + 2.D+0*rel*ANEu*AT + AT**2)
            derel = SQRT(2.D+0*rel*ANEu*AT + AT**2 + ANEu**2)
C        ENCON=(REL**2-1.D0)*AMI*AT*(ANEU+AT)/DEREL**2/2.D+0
            WW4 = 1.2064496D-2*eirelm*eirelt/(ANEu*eirelm + eirelt)
     &            /derel*apan
 
C     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            WW4 = 1.
C       VSO=VS*EXP(-WW4*ALASO*(ENEF+CDE))
 
            IF(MEDis.NE.0)THEN
              IF(MEDis.EQ.2)THEN
              ENDIF
 
 
 
 
C
C     NO ISOSCALAR CONTRIBUTION FROM NON SYMMETRIC IMAGINARY POSITIVE POTENTIAL Wv
C
              IF(EA.GT.0.D0)VRDc = wcp*DOM_INT_T1(EFErm,EA,EN) + VRDc
            ENDIF
 
 
 
C     WD=WD+WISO
            IF(WD.LE.0.)WD = 0.
            IF(WC.LE.0.)WC = 0.
            AR = AR0 + AR1*enef
            AW = AW0 + AW1*enef
            AC = AC0 + AC1*enef
            AD = AD0 + AD1*enef
            IF(enef.GT.BNDc)AD = AD0 + AD1*BNDc
            AS = AS0 + AS1*enef
            rar = RR/AR
            rac = RC/AC
            raw = RW/AW
            rad = RD/AD
            m = 8
            m4 = 4*m
            kl = 0
            in = 1
            kit = m
            nit = 1
            ik = m4 - 1
 
 
C
            STEp = RK/NH
            step1 = STEp/m
 
C
            DO k = 1, 2
              DO i = in, ik
                ir = i/kit + nit
                i1 = (ir - 1)*las2 + mnula2
                l1 = (i - 1)*las2 + mnula2
                l1ap = (i - 1)*las2
                IF(k.EQ.2.AND.i.EQ.ik)THEN
                  R = 0.D0
                ELSE
                  R = step1*(i - kl)
                ENDIF
 
C     NON-DIAGONAL TERMS BY ISOSPIN
 
 
C      write(21,9999)vr,vd,vrdc,wc,wd,ww, 7.77777777
C 9999 format(11e12.5)
                IF(MEPot.EQ.1)THEN
 
                  DO l = 1, las2
 
                    pz = 0.D0
                    ll2 = l1 + l
 
                    DE = DEF(1)
                    CALL POTET
 
                    vv = VP*POL(l,1)
                    wv = WP*POL(l,1)
                    n = NAN2
                    DE = DEF(n)
                    CALL POTET
                    vv = vv + VP*POL(l,n)
                    wv = wv + WP*POL(l,n)
                    v4 = 0.
                    w4 = 0.
                    DO n = 2, NAN2, 2
                      DE = DEF(n)
                      CALL POTET
                      v4 = v4 + VP*POL(l,n)
                      w4 = w4 + WP*POL(l,n)
                    ENDDO
                    v2 = 0.
                    w2 = 0.
                    DO n = 3, NAN1, 2
                      DE = DEF(n)
                      CALL POTET
                      v2 = v2 + VP*POL(l,n)
                      w2 = w2 + WP*POL(l,n)
                    ENDDO
 
 
                    IF(JO(nu1).NE.JO(nu2).OR.l.NE.1)THEN
                      IF(l.EQ.1)CYCLE
                    ENDIF
 
 
 
C      GO TO 99                                           !!!!!!!CHECK!!!!!!!
 
 
                    PV(ll2) = (vv + 4.D0*v4 + 2.D0*v2)
     &                        *ST*ww*4.1887902047864D0
                    PW(ll2) = (wv + 4.D0*w4 + 2.D0*w2)
     &                        *ST*ww*4.1887903047864D0
                    IF(MERel.EQ.3)PW(ll2) = PW(ll2)/relpot
 
C      write(21,1234) i,l,r
C 1234 format(2i5,e12.5)
C     write(21,9999)vr,vd,vrdc,wc,wd,ww, 7.77777777
C      write(21,9999)v2,v4,vv, 7.77777777,w2,w4,wv, pv(LL2),pWW(LL2), 5.5
 
                    IF(k.EQ.2.AND.i.EQ.ik)THEN
                      R = 0.D0
 
                      ir = 1
                    ELSE
                      R = step1*(i - kl)
 
                      IF(i/kit*kit.NE.i)CYCLE
                      ir = i/kit + nit
                    ENDIF
                    i1 = (ir - 1)*las2 + mnula2
                    ii2 = i1 + l
                    PR(ii2) = PV(ll2)
                    PI(ii2) = PW(ll2)
 
 
 
                  ENDDO
                  CYCLE
                ELSE
                  pz = 0.D0
                  nt0 = l1 + 1
                  nt1 = l1 + 2
                  nt2 = l1 + 3
                  nt3 = l1 + 4
                  nt4 = l1 + 5
                  x = (R - RR)/AR
                  IF(x.GT.23.)THEN
                    ex1 = EXP( - x)
                    IF(JO(nu1).EQ.JO(nu2))PV(nt0) = -VR*ex1*ww
                    IF(LAS.NE.0)THEN
                      PV(nt1) = ( - VR*ex1*ww - pz)*rar
                      IF(LAS.NE.1)THEN
                        PV(nt2) = PV(nt1)*rar/2.D0
                        IF(LAS.NE.2)THEN
                          PV(nt3) = PV(nt2)*rar/3.D0
                          IF(LAS.NE.3)THEN
                            PV(nt4) = PV(nt3)*rar/4.D0
                          ENDIF
                        ENDIF
                      ENDIF
                    ENDIF
                  ELSE
                    ex = EXP(x)
                    ex1 = 1.D0 + ex
                    exd = 1.D0/ex1
                    exdm = 1.D0 - exd
                    IF(JO(nu1).EQ.JO(nu2))PV(nt0) = -VR*exd*ww
                    IF(LAS.NE.0)THEN
                      PV(nt1) = ( - VR*exd*ww - pz)*exdm*rar
                      IF(LAS.NE.1)THEN
                        y1 = 1.D0 - 2.D0*exd
                        PV(nt2) = PV(nt1)*y1*rar/2.D0
                        IF(LAS.NE.2)THEN
                          y2 = 1.D0 - 6.D0*exd*exdm
                          PV(nt3) = PV(nt2)/y1*y2*rar/3.D0
                          IF(LAS.NE.3)THEN
                            y3 = 1.D0 - 
     &                           exd*(14.D0 - exd*(36.D0 - 24.D0*exd))
                            PV(nt4) = PV(nt3)/y2*y3*rar/4.D0
                          ENDIF
                        ENDIF
                      ENDIF
                    ENDIF
                  ENDIF
                  x = (R - RC)/AC
                  y = (R - RW)/AW
                  xx = y*y
                  IF(xx.GT.180.)exx = 0.
                  IF(xx.LE.180.)exx = EXP( - xx)
                  IF(x.LE.23.)THEN
                    ex = EXP(x)
                    ex1 = 1.D0 + ex
                    exd = 1.D0/ex1
                    exdm = 1.D0 - exd
                    wcc = -WC*ALF*exd*ww
                    www = -WC*(1. - ALF)*exx*ww
                    IF(MERel.EQ.3)wcc = wcc/relpot
                    IF(MERel.EQ.3)www = www/relpot
                    IF(JO(nu1).EQ.JO(nu2))PW(nt0) = wcc + www
                    IF(JO(nu1).EQ.JO(nu2).AND.WC.NE.0.D0)PV(nt0)
     &                 = PV(nt0) + wcc*VRDc/WC
                    IF(LAS.EQ.0)GOTO 5
                    wcc = wcc*exdm*rac
                    www = www*2.D0*raw*y
                    PW(nt1) = wcc + www
                    IF(WC.NE.0.D0)PV(nt1) = PV(nt1) + wcc*VRDc/WC
                    IF(LAS.EQ.1)GOTO 5
                    yc1 = 1.D0 - 2.D0*exd
                    wcc = wcc*yc1*rac/2.D0
                    y1 = 2.D0*y*y - 1.D0
                    www = www/y*y1*raw/2.D0
                    PW(nt2) = wcc + www
                    IF(WC.NE.0.D0)PV(nt2) = PV(nt2) + wcc*VRDc/WC
                    IF(LAS.EQ.2)GOTO 5
                    yc2 = 1.D0 - 6.D0*exd*exdm
                    wcc = wcc/yc1*yc2*rac/3.D0
                    y2 = y**3*2.D0 - y*3.D0
                    www = www/y1*y2*raw*2.D0/3.D0
                    PW(nt3) = wcc + www
                    IF(WC.NE.0.D0)PV(nt3) = PV(nt3) + wcc*VRDc/WC
                    IF(LAS.EQ.3)GOTO 5
                    yc3 = 1.D0 - exd*(14.D0 - exd*(36.D0 - 24.D0*exd))
                    wcc = wcc/yc2*yc3*rac/4.
                    www = www/y2*(y**4*4. - y*y*12.D0 + 3.D0)*raw/4.D0
                    PW(nt4) = wcc + www
                    IF(WC.NE.0.D0)PV(nt4) = PV(nt4) + wcc*VRDc/WC
                  ENDIF
                  ex1 = EXP( - x)
                  wcc = -WC*ALF*ex1*ww
                  www = -WC*(1.D0 - ALF)*exx*ww
                  IF(MERel.EQ.3)wcc = wcc/relpot
                  IF(MERel.EQ.3)www = www/relpot
                  IF(JO(nu1).EQ.JO(nu2))PW(nt0) = wcc + www
                  IF(JO(nu1).EQ.JO(nu2).AND.WC.NE.0.D0)PV(nt0) = PV(nt0)
     &               + wcc*VRDc/WC
                  IF(LAS.NE.0)THEN
                    wcc = wcc*rac
                    www = www*2.D0*raw*y
                    PW(nt1) = wcc + www
                    IF(WC.NE.0.D0)PV(nt1) = PV(nt1) + wcc*VRDc/WC
                    IF(LAS.NE.1)THEN
                      wcc = wcc*rac/2.D0
                      y1 = 2.D0*y*y - 1.D0
                      www = www/y*y1*raw/2.D0
                      PW(nt2) = wcc + www
                      IF(WC.NE.0.D0)PV(nt2) = PV(nt2) + wcc*VRDc/WC
                      IF(LAS.NE.2)THEN
                        wcc = wcc*rac/3.
                        y2 = y**3*2.D0 - y*3.D0
                        www = www/y1*y2*raw*2.D0/3.D0
                        PW(nt3) = wcc + www
                        IF(WC.NE.0.D0)PV(nt3) = PV(nt3) + wcc*VRDc/WC
                        IF(LAS.NE.3)THEN
                          wcc = wcc*rac/4.D0
                          www = www/y2*(y**4*4.D0 - y*y*12.D0 + 3.D0)
     &                          *raw/4.D0
                          PW(nt4) = wcc + www
                          IF(WC.NE.0.D0)PV(nt4) = PV(nt4) + wcc*VRDc/WC
                        ENDIF
                      ENDIF
                    ENDIF
                  ENDIF
                ENDIF
    5           x = (R - RD)/AD
                IF(x.GT.23.)THEN
                  ex1 = EXP( - x)
                  wdd = -4.D0*WD*ex1*ww
                  IF(MERel.EQ.3)wdd = wdd/relpot
                  IF(JO(nu1).EQ.JO(nu2))PW(nt0) = PW(nt0) + wdd
                  IF(JO(nu1).EQ.JO(nu2).AND.WD.NE.0.D0)PV(nt0) = PV(nt0)
     &               + wdd/WD*VD
                  IF(LAS.NE.0)THEN
                    wdd = wdd*rad
                    PW(nt1) = PW(nt1) + wdd
                    IF(WD.NE.0.D0)PV(nt1) = PV(nt1) + wdd/WD*VD
                    IF(LAS.NE.1)THEN
                      wdd = wdd*rad/2.D0
                      PW(nt2) = PW(nt2) + wdd
                      IF(WD.NE.0.D0)PV(nt2) = PV(nt2) + wdd/WD*VD
                      IF(LAS.NE.2)THEN
                        wdd = wdd*rad/3.D0
                        PW(nt3) = PW(nt3) + wdd
                        IF(WD.NE.0.D0)PV(nt2) = PV(nt2) + wdd/WD*VD
                        IF(LAS.NE.3)THEN
                          wdd = wdd*rad/4.D0
                          PW(nt4) = PW(nt4) + wdd
                          IF(WD.NE.0.D0)PV(nt4) = PV(nt4) + wdd/WD*VD
                        ENDIF
                      ENDIF
                    ENDIF
                  ENDIF
                ELSE
                  ex = EXP(x)
                  ex1 = 1.D0 + ex
                  exd = 1.D0/ex1
                  exdm = 1.D0 - exd
                  wdd = -4.D0*WD*exdm*exd*ww
                  IF(MERel.EQ.3)wdd = wdd/relpot
                  IF(JO(nu1).EQ.JO(nu2))PW(nt0) = PW(nt0) + wdd
                  IF(JO(nu1).EQ.JO(nu2).AND.WD.NE.0.D0)PV(nt0) = PV(nt0)
     &               + wdd/WD*VD
                  IF(LAS.NE.0)THEN
                    y1 = 1.D0 - 2.D0*exd
                    wdd = wdd*y1*rad
                    PW(nt1) = PW(nt1) + wdd
                    IF(WD.NE.0.D0)PV(nt1) = PV(nt1) + wdd/WD*VD
                    IF(LAS.NE.1)THEN
                      y2 = 1.D0 - 6.D0*exd*exdm
                      wdd = wdd/y1*y2*rad/2.D0
                      PW(nt2) = PW(nt2) + wdd
                      IF(WD.NE.0.D0)PV(nt2) = PV(nt2) + wdd/WD*VD
                      IF(LAS.NE.2)THEN
                        y3 = 1.D0 - exd*(14.D0 - exd*(36.D0 - 24.D0*exd)
     &                       )
                        wdd = wdd/y2*y3*rad/3.D0
                        PW(nt3) = PW(nt3) + wdd
                        IF(WD.NE.0.D0)PV(nt3) = PV(nt3) + wdd/WD*VD
                        IF(LAS.NE.3)THEN
                          y4 = 1. - 
     &                         exd*(30.D0 - exd*(150.D0 - exd*(240.D0-
     &                         120.D0*exd)))
                          wdd = wdd/y3*y4*rad/4.D0
                          PW(nt4) = PW(nt4) + wdd
                          IF(WD.NE.0.D0)PV(nt4) = PV(nt4) + wdd/WD*VD
                        ENDIF
                      ENDIF
                    ENDIF
                  ENDIF
                ENDIF
                IF(i/kit*kit.EQ.i)THEN
                  PR(i1 + 1) = PV(nt0)
                  PI(i1 + 1) = PW(nt0)
 
C      print 876, i1,nt0,Nu1,Nu2, JO(NU1),JO(NU2),PR(I1+1),PI(I1+1)
C  876 FORMAT (6i7,6e12.3)
C      pause 22
 
                  IF(LAS.NE.0)THEN
                    PR(i1 + 2) = PV(nt1)
                    PI(i1 + 2) = PW(nt1)
                    IF(LAS.NE.1)THEN
                      PR(i1 + 3) = PV(nt2)
                      PI(i1 + 3) = PW(nt2)
                      IF(LAS.NE.2)THEN
                        PR(i1 + 4) = PV(nt3)
                        PI(i1 + 4) = PW(nt3)
                        IF(LAS.NE.3)THEN
                          PR(i1 + 5) = PV(nt4)
                          PI(i1 + 5) = PW(nt4)
                        ENDIF
                      ENDIF
                    ENDIF
                  ENDIF
                ENDIF
 
              ENDDO
              kl = m4 - 4
              in = m4
              kit = 1
              nit = 5 - m4
              ik = NH + kl + 2
              step1 = STEp
            ENDDO
            NH1 = NH + kl
            NHI = NH + 2
          ENDIF
        ENDDO
      ENDDO
      EN = eint
      RR = rrr
      RZ = rzin
      CCOul = cccoul
      CONz = conzz
 
 
C      write(21,1799) vr,vd,vrdc,wc,wd, enef,WDBWNP
C 1799 format(7e12.5)
C     pause 2
 
C     WRITE(21,9997) RK, STEP,CONZ ,2.2222222, NH1
C9997  FORMAT(4E12.5, I3)
      RETURN
      END SUBROUTINE RIPAT1
 
!---------------------------------------------------------------------------
C     ***********************************************************
      SUBROUTINE CMATC
C     ***********************************************************
      IMPLICIT DOUBLE PRECISION(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8, DIMENSION(10) :: BET
      REAL*8 :: CSS, EN
      REAL*8, DIMENSION(20) :: EL
      INTEGER :: INCc, INCr, LAS, MEApp, MECha, MECul, MEDis, MEHam, 
     &           MEHao, MEJob, MEPot, MEPri, MERel, MERip, MERrr, MERzz, 
     &           MESha, MESho, MESol, MEVol, NCLl, NECi, NHI, NJ, NMAx, 
     &           NPD, NSS, NUR
      COMMON /ECI   / NECi, NHI
      COMMON /ENA   / EN, EL, BET, NUR, NMAx, NPD, LAS
      COMMON /JNN   / CSS, INCc, NCLl, NSS, NJ, INCr
      COMMON /MENU  / MEJob, MEPot, MEHam, MECha, MEPri, MESol, MESha, 
     &                MESho, MEHao, MEApp, MEVol, MERel, MECul, MERzz, 
     &                MERrr, MEDis, MERip
C
C Local variables
C
      INTEGER :: iec, incrt, msolin, ncllt
C
C*** End of declarations rewritten by SPAG
C
      msolin = MESol
C      IF(NSS.LT.5) MESOL=2
      IF(MESol.LE.3)THEN
        CALL KNDIT
        IF(MESol.LT.2)THEN
          IF(NSS.GE.5)THEN
            iec = INCc*(BET(2)*40. + 3)
            IF(NCLl.GT.iec)GOTO 20
          ENDIF
        ELSEIF(MESol.NE.2)THEN
          GOTO 20
        ENDIF
      ELSE
        ncllt = NCLl
        incrt = INCr
        IF(MESol.LT.INCr)INCr = MESol
        IF(MESol.LT.NCLl)NCLl = MESol
        CALL KNDIT
        CALL SOSIT
        CALL MASCT
        NCLl = ncllt
        INCr = incrt
        IF(MESol.GE.ncllt.AND.MEPri.NE.99)PRINT 1010, NSS, NCLl
        IF(MESol.GE.ncllt)GOTO 30
        CALL KNDIT
        GOTO 20
      ENDIF
   10 CALL SOSIT
      MESol = 2
      CALL MASCT
      IF(MEPri.NE.99)PRINT 1010, NSS, NCLl
 1010 FORMAT(2I5,E20.7)
      GOTO 30
   20 CALL ECISS
      IF(NECi.EQ.0)GOTO 10
   30 MESol = msolin
      RETURN
      END SUBROUTINE CMATC
 
!---------------------------------------------------------------------------
C*******************************************************************************
      SUBROUTINE RCWF(Rho,Eta,Minl,Maxl,Fc,Fcp,Gc,Gcp,Accur,Step,Numbr)
      IMPLICIT DOUBLE PRECISION(A - h,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      REAL*8 :: Accur, Eta, Rho, Step
      INTEGER :: Maxl, Minl, Numbr
      REAL*8, DIMENSION(1) :: Fc, Fcp, Gc, Gcp
C
C Local variables
C
      REAL*8 :: acc, ai, ar, bi, br, d, del, di, dk, dp, dq, dr, eta2, 
     &          etah, etar, f, fp, fpl, fpl1, g, gp, gpl, gpl1, h, h2, 
     &          h2ll, k, k1, k2, k3, k4, m1, m2, m3, m4, p, pace, pl, 
     &          pmx, q, r, r3, rh, rh2, rho2, ri, rl, s, sl, t, tf, tfp, 
     &          turn, w, wi, xll1
      REAL :: FLOAT
      INTEGER :: i2, ktr, ktrp, l, lmax, lmin1, lp, num
C
C*** End of declarations rewritten by SPAG
C
C *** COULOMB WAVEFUNCTIONS CALCULATED AT R = RHO BY THE
C *** CONTINUED-FRACTION METHOD OF STEED   MINL,MAXL ARE ACTUAL L-VALUES
C *** SEE BARNETT FENG STEED AND GOLDFARB COMPUTER PHYSICS COMMUN 1974
C *** RCWF            - A MODIFICATION OF THE REAL COULOMB
C *** WAVEFUNCTION PROGRAM RCWFN.  A.R. BARNETT.
C *** REF. IN COMP. PHYS. COMMUN. 11 (1976) 141
C  INSERT AFTER CARD 48 THE NEXT SIX CARDS
C *** CPC 8 (1974) 377-395
C *** IF NUMBR = 1 FC(MAXL) TO FC(MINL) ARE RETURNED  ARB MARCH 1975
C *** IF NUMBR = 2 FC AND GC ARRAYS ARE RETURNED
C *** IF NUMBR = 3 FC,GC,FCP,GCP ARRAYS ARE RETURNED
      num = 1
      IF(Numbr.GE.1.AND.Numbr.LE.3)num = Numbr
      pace = Step
      acc = Accur
      IF(pace.LT.100.0)pace = 100.0
      IF(acc.LT.1.0E-15.OR.acc.GT.1.0E-6)acc = 1.0E-6
      r = Rho
      ktr = 1
      lmax = Maxl
      lmin1 = Minl + 1
      xll1 = FLOAT(Minl*lmin1)
      eta2 = Eta*Eta
      turn = Eta + SQRT(eta2 + xll1)
      IF(r.LT.turn.AND.ABS(Eta).GE.1.0E-6)ktr = -1
      ktrp = ktr
   10 etar = Eta*r
      rho2 = r*r
      pl = FLOAT(lmax + 1)
      pmx = pl + 0.5
C *** CONTINUED FRACTION FOR FP(MAXL)/F(MAXL)  XL IS F  XLPRIME IS FP **
      fp = Eta/pl + pl/r
      dk = etar*2.0
      del = 0.0
      d = 0.0
      f = 1.0
      k = (pl*pl - pl + etar)*(2.0*pl - 1.0)
      IF(pl*pl + pl + etar.NE.0.0)THEN
   15   h = (pl*pl + eta2)*(1.0 - pl*pl)*rho2
        k = k + dk + pl*pl*6.0
        d = 1.0/(d*h + k)
        del = del*(d*k - 1.0)
        IF(pl.LT.pmx)del = -r*(pl*pl + eta2)*(pl + 1.0)*d/pl
        pl = pl + 1.0
        fp = fp + del
        IF(d.LT.0.0)f = -f
        IF(pl.GT.20000.)GOTO 40
CC      IF(ABS(DEL/FP).GE.ACC) GO TO 3
C  REPLACE CARD 91 BY THE NEXT CARD
        IF(ABS(del).GE.ABS(fp)*acc)GOTO 15
        fp = f*fp
        IF(lmax.NE.Minl)THEN
          Fc(lmax + 1) = f
CC      FCP(LMAX+1) = FP
C  REPLACE CARD 95 BY NEXT TWO CARDS
          fpl = fp
          IF(num.EQ.3)Fcp(lmax + 1) = fp
C *** DOWNWARD RECURSION TO MINL FOR F AND FP, ARRAYS GC,GCP ARE STORAGE
          l = lmax
C  INSERT NEXT CARD AFTER 97
          ri = 1.0/r
          DO lp = lmin1, lmax
            pl = FLOAT(l)
CC      GC (L+1) = ETA/PL + PL/R
CC      GCP(L+1) = SQRT(ETA2 + PL*PL)/PL
CC      FC (L)   = (GC(L+1)*FC(L+1) + FCP(L+1))/GCP(L+1)
CC      FCP(L)   =  GC(L+1)*FC(L)   - GCP(L+1)*FC(L+1)
C  REPLACE CARDS 100 - 103 BY NEXT 10 CARDS
            sl = Eta/pl + pl*ri
            rl = SQRT(1.D0 + eta2/(pl*pl))
            Fc(l) = (sl*Fc(l + 1) + fpl)/rl
            fpl1 = sl*Fc(l) - rl*Fc(l + 1)
            fpl = fpl1
            IF(num.NE.1)THEN
              Gc(l + 1) = rl
              IF(num.GT.2)THEN
                Gcp(l + 1) = sl
                Fcp(l) = fpl1
              ENDIF
            ENDIF
            l = l - 1
          ENDDO
          f = Fc(lmin1)
CC      FP = FCP(LMIN1)
C  REPLACE CARD 106 BY THE NEXT CARD
          fp = fpl1
        ENDIF
        IF(ktrp.EQ. - 1)THEN
          r = turn
          tf = f
          tfp = fp
          lmax = Minl
          ktrp = 1
          GOTO 10
        ELSE
C *** REPEAT FOR R = TURN IF RHO LT TURN
C *** NOW OBTAIN P + I.Q FOR MINL FROM CONTINUED FRACTION (32)
C *** REAL ARITHMETIC TO FACILITATE CONVERSION TO IBM USING DOUBLE PRECISION
          p = 0.D0
          q = r - Eta
          pl = 0.D0
          ar = -(eta2 + xll1)
          ai = Eta
          br = 2.D0*q
          bi = 2.D0
          wi = 2.D0*Eta
          dr = br/(br*br + bi*bi)
          di = -bi/(br*br + bi*bi)
          dp = -(ar*di + ai*dr)
          dq = (ar*dr - ai*di)
   20     p = p + dp
          q = q + dq
          pl = pl + 2.D0
          ar = ar + pl
          ai = ai + wi
          bi = bi + 2.D0
          d = ar*dr - ai*di + br
          di = ai*dr + ar*di + bi
          t = 1.D0/(d*d + di*di)
          dr = t*d
          di = -t*di
          h = br*dr - bi*di - 1.D0
          k = bi*dr + br*di
          t = dp*h - dq*k
          dq = dp*k + dq*h
          dp = t
          IF(pl.GT.46000.D0)GOTO 40
          IF(ABS(dp) + ABS(dq).GE.(ABS(p) + ABS(q))*acc)GOTO 20
          p = p/r
          q = q/r
C *** SOLVE FOR FP,G,GP AND NORMALISE F  AT L=MINL
          g = (fp - p*f)/q
          gp = p*g - q*f
          w = 1.D0/SQRT(fp*g - f*gp)
          g = w*g
          gp = w*gp
          IF(ktr.NE.1)THEN
            f = tf
            fp = tfp
            lmax = Maxl
C *** RUNGE-KUTTA INTEGRATION OF G(MINL) AND GP(MINL) INWARDS FROM TURN
C ***             SEE FOX AND MAYERS 1968 PG 202
            IF(Rho.LT.0.2*turn)pace = 999.D0
            r3 = 1.D0/3.0D0
            h = (Rho - turn)/(pace + 1.D0)
CC      H2 = 0.5*H
C  REPLACE CARD 158 BY THE NEXT CARD
            h2 = 0.5D0*h
C      I2 = IFIX(PACE + 0.001)
            i2 = pace + 0.001D0
            etah = Eta*h
            h2ll = h2*xll1
            s = (etah + h2ll/r)/r - h2
   25       rh2 = r + h2
            t = (etah + h2ll/rh2)/rh2 - h2
            k1 = h2*gp
            m1 = s*g
            k2 = h2*(gp + m1)
            m2 = t*(g + k1)
            k3 = h*(gp + m2)
            m3 = t*(g + k2)
            m3 = m3 + m3
            k4 = h2*(gp + m3)
            rh = r + h
            s = (etah + h2ll/rh)/rh - h2
            m4 = s*(g + k3)
            g = g + (k1 + k2 + k2 + k3 + k4)*r3
            gp = gp + (m1 + m2 + m2 + m3 + m4)*r3
            r = rh
            i2 = i2 - 1
            IF(ABS(gp).GT.1.0D300)GOTO 40
            IF(i2.GE.0)GOTO 25
            w = 1.D0/(fp*g - f*gp)
          ENDIF
        ENDIF
      ELSE
        r = r + 1.0E-6
        GOTO 10
      ENDIF
C *** UPWARD RECURSION FROM GC(MINL) AND GCP(MINL),STORED VALUES ARE R,S
C *** RENORMALISE FC,FCP FOR EACH L-VALUE
CC8     GC (LMIN1) = G
CC      GCP(LMIN1) = GP
CC      IF(LMAX.EQ.MINL) GO TO 10
C  REPLACE CARDS 185 - 187 BY THE NEXT 6 CARDS
CC      IF(NUM.GE.2) GC (LMIN1) = G
C  REPLACE CARD A056 BY
   30 IF(num.GE.2)Gc(lmin1) = g
      IF(num.EQ.3)Gcp(lmin1) = gp
      IF(num.EQ.3)Fcp(lmin1) = fp*w
      Fc(lmin1) = f*w
      IF(lmax.EQ.Minl)RETURN
      gpl = gp
      DO l = lmin1, lmax
CC      T        = GC(L+1)
CC      GC (L+1) = (GC(L)*GC (L+1) - GCP(L))/GCP(L+1)
CC      GCP(L+1) =  GC(L)*GCP(L+1) - GC(L+1)*T
CC      FC (L+1) = W*FC (L+1)
CC9     FCP(L+1) = W*FCP(L+1)
C  REPLACE CARDS 189 - 193 BY THE NEXT 11 CARDS
        IF(num.NE.1)THEN
          IF(num.EQ.2)sl = Eta/FLOAT(l) + FLOAT(l)*ri
          IF(num.EQ.3)sl = Gcp(l + 1)
          rl = Gc(l + 1)
          Gc(l + 1) = (sl*Gc(l) - gpl)/rl
          gpl1 = rl*Gc(l) - sl*Gc(l + 1)
          gpl = gpl1
          IF(num.GE.3)THEN
            Gcp(l + 1) = gpl1
            Fcp(l + 1) = Fcp(l + 1)*w
          ENDIF
        ENDIF
        Fc(l + 1) = Fc(l + 1)*w
      ENDDO
CC      FC (LMIN1) = FC (LMIN1)*W
CC      FCP(LMIN1) = FCP(LMIN1)*W
CC      RETURN
CC10    FC (LMIN1) = W*F
CC      FCP(LMIN1) = W*FP
      RETURN
   40 w = 0.D0
      g = 0.D0
      gp = 0.D0
      GOTO 30
      END SUBROUTINE RCWF
 
!---------------------------------------------------------------------------
C     ******************************************************************
      SUBROUTINE RCWFN(Rhs,Ets,Minl,Maxl,Fc,Fcp,Gc,Gcp,Accur,Stepc)
C     ******************************************************************
      IMPLICIT DOUBLE PRECISION(A - h,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      REAL*8 :: Accur, Ets, Rhs, Stepc
      INTEGER :: Maxl, Minl
      REAL*8, DIMENSION(1) :: Fc, Fcp, Gc, Gcp
C
C Local variables
C
      REAL*8 :: acc, ai, ar, bi, br, c1, c2, cnt, d, del, di, dk, dp, 
     &          dq, dr, eta, eta2, etah, etar, f, fp, fsous, g, gp, gpg, 
     &          gpmax, h, h2, h2ll, half, hundr, k, k1, k2, k3, k4, m1, 
     &          m2, m3, m4, one, oone, otwo, p, page, pl, pmx, q, r, r3, 
     &          rh, rh2, rho, rho2, s, six, t, tf, tfp, thrn, turn, two, 
     &          twous, w, wi, xll1, zer
      INTEGER :: i2, ktr, ktrp, l, lmax, lmin1, lp
C
C*** End of declarations rewritten by SPAG
C
      DATA gpmax, one, hundr, c1, c2, half, two, zer, six, twous, fsous, 
     &     otwo, thrn, oone/1.0D+60, 1.0D+00, 0.1D+02, 1.0D-12, 1.0D-06, 
     &     0.5D+00, 0.2D+01, 0.0D+00, 0.6D+01, 0.2D+05, 0.46D+05, 
     &     0.2D+00, 0.999D+03, 0.1D-02/
      page = Stepc
      acc = Accur
      rho = Rhs
      eta = Ets
      IF(page.LT.hundr)page = hundr
      IF(acc.LT.c1.OR.acc.GT.c2)acc = c2
      r = rho
      ktr = 1
      lmax = Maxl
      lmin1 = Minl + 1
      xll1 = DFLOAT(Minl*lmin1)
      eta2 = eta*eta
      turn = eta + SQRT(eta2 + xll1)
      IF(r.LT.turn.AND.ABS(eta).GE.c2)ktr = -1
      ktrp = ktr
   10 etar = eta*r
      rho2 = r*r
      pl = DFLOAT(lmax + 1)
      pmx = pl + half
      fp = eta/pl + pl/r
      dk = etar*two
      del = zer
      d = zer
      f = one
      k = (pl*pl - pl + etar)*(two*pl - one)
      IF((pl*pl + pl + etar).NE.zer)THEN
   15   h = (pl*pl + eta2)*(one - pl*pl)*rho2
        k = k + dk + pl*pl*six
        d = one/(d*h + k)
        del = del*(d*k - one)
        IF(pl.LT.pmx)del = -r*(pl*pl + eta2)*(pl + one)*d/pl
        pl = pl + one
        fp = fp + del
        IF(d.LT.zer)f = -f
        IF(pl.GT.twous)GOTO 40
        IF(ABS(del/fp).GE.acc)GOTO 15
        fp = f*fp
        IF(lmax.NE.Minl)THEN
          Fc(lmax + 1) = f
          Fcp(lmax + 1) = fp
          l = lmax
          DO lp = lmin1, lmax
            pl = DFLOAT(l)
            Gc(l + 1) = eta/pl + pl/r
            Gcp(l + 1) = SQRT(eta2 + pl*pl)/pl
            Fc(l) = (Gc(l + 1)*Fc(l + 1) + Fcp(l + 1))/Gcp(l + 1)
            Fcp(l) = Gc(l + 1)*Fc(l) - Gcp(l + 1)*Fc(l + 1)
            l = l - 1
          ENDDO
          f = Fc(lmin1)
          fp = Fcp(lmin1)
        ENDIF
        IF(ktrp.EQ. - 1)THEN
          r = turn
          tf = f
          tfp = fp
          lmax = Minl
          ktrp = 1
          GOTO 10
        ELSE
          p = zer
          q = r - eta
          pl = zer
          ar = -(eta2 + xll1)
          ai = eta
          br = two*q
          bi = two
          wi = two*eta
          dr = br/(br*br + bi*bi)
          di = -bi/(br*br + bi*bi)
          dp = -(ar*di + ai*dr)
          dq = ar*dr - ai*di
   20     p = p + dp
          q = q + dq
          pl = pl + two
          ar = ar + pl
          ai = ai + wi
          bi = bi + two
          d = ar*dr - ai*di + br
          di = ai*dr + ar*di + bi
          t = one/(d*d + di*di)
          dr = d*t
          di = -t*di
          h = br*dr - bi*di - one
          k = bi*dr + br*di
          t = dp*h - dq*k
          dq = dp*k + dq*h
          dp = t
          IF(pl.GT.fsous)GOTO 40
          cnt = (ABS(dp) + ABS(dq)) - ((ABS(p) + ABS(q))*acc)
          IF(cnt.GT.0)GOTO 20
          p = p/r
          q = q/r
          g = (fp - p*f)/q
          gp = p*g - q*f
          w = one/SQRT(fp*g - f*gp)
          g = w*g
          gp = w*gp
          IF(ktr.NE.1)THEN
            f = tf
            fp = tfp
            lmax = Maxl
            IF(rho.LT.(otwo*turn))page = thrn
            r3 = 0.333333333333333333333D+00
            h = (rho - turn)/(page + one)
            h2 = half*h
            i2 = page + oone
            etah = eta*h
            h2ll = h2*xll1
            s = (etah + h2ll/r)/r - h2
   25       rh2 = r + h2
            t = (etah + h2ll/rh2)/rh2 - h2
            k1 = h2*gp
            m1 = s*g
            k2 = h2*(gp + m1)
            m2 = t*(g + k1)
            k3 = h*(gp + m2)
            m3 = t*(g + k2)
            m3 = m3 + m3
            k4 = h2*(gp + m3)
            rh = r + h
            s = (etah + h2ll/rh)/rh - h2
            m4 = s*(g + k3)
            g = g + (k1 + k2 + k2 + k3 + k4)*r3
            gp = gp + (m1 + m2 + m2 + m3 + m4)*r3
            r = rh
            i2 = i2 - 1
            gpg = gp
            IF(ABS(gpg).GT.gpmax)GOTO 40
            IF(i2.GE.0)GOTO 25
            w = one/(fp*g - f*gp)
          ENDIF
        ENDIF
      ELSE
        r = r + c2
        GOTO 10
      ENDIF
   30 Gc(lmin1) = g
      Gcp(lmin1) = gp
      IF(lmax.EQ.Minl)THEN
        Fc(lmin1) = w*f
        Fcp(lmin1) = w*fp
      ELSE
        DO l = lmin1, lmax
          t = Gc(l + 1)
          Gc(l + 1) = (Gc(l)*Gc(l + 1) - Gcp(l))/Gcp(l + 1)
          Gcp(l + 1) = Gc(l)*Gcp(l + 1) - Gc(l + 1)*t
          Fc(l + 1) = w*Fc(l + 1)
          Fcp(l + 1) = w*Fcp(l + 1)
        ENDDO
        Fcp(lmin1) = Fcp(lmin1)*w
        Fc(lmin1) = Fc(lmin1)*w
      ENDIF
      GOTO 50
   40 w = zer
      g = zer
      gp = zer
      GOTO 30
   50 RETURN
      END SUBROUTINE RCWFN
 
!---------------------------------------------------------------------------
C     ***********************************************************
      SUBROUTINE LOGMO
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      INTEGER, DIMENSION(180) :: JNO, LNO, NS1
      INTEGER :: JSO, JSS, NN1, NPIo, NPIs, NSPi
      COMMON /LNP   / JSS, NPIs, NPIo, JSO, NN1, LNO, NS1, JNO, NSPi
C
C Local variables
C
      INTEGER :: IABS
      INTEGER :: il, im, in, ja, jo, jo2, kj1, kj2, kl1, kl2, la, lm, 
     &           lo, lo2, n, n1m, ns1a
C
C*** End of declarations rewritten by SPAG
C
C     ***********************************************************
      n = 0
      kj1 = IABS(JSO - JSS) + 2
      kj2 = JSO + JSS + 2
      DO jo2 = kj1, kj2, 2
        jo = jo2 - 2
        kl1 = IABS(jo - NSPi) + 2
        kl2 = jo + NSPi + 2
        DO lo2 = kl1, kl2, 2
          lo = lo2 - 2
          IF(NPIo*NPIs*( - 1)**(lo/2).GT.0)THEN
            n = n + 1
            LNO(n) = lo/2
            JNO(n) = jo
            NS1(n) = jo - lo
          ENDIF
        ENDDO
      ENDDO
      NN1 = n
      n1m = NN1 - 1
      IF(NN1.GT.1)THEN
        DO im = 1, n1m
          lm = LNO(im)
          in = im + 1
          DO il = in, NN1
            la = LNO(il)
            IF(lm.GT.la)THEN
              ja = JNO(il)
              ns1a = NS1(il)
              JNO(il) = JNO(im)
              LNO(il) = lm
              NS1(il) = NS1(im)
              JNO(im) = ja
              LNO(im) = la
              NS1(im) = ns1a
              lm = la
            ENDIF
          ENDDO
        ENDDO
      ENDIF
      RETURN
      END SUBROUTINE LOGMO
 
!---------------------------------------------------------------------------
C     *****************************************************************
      SUBROUTINE KLEGO1
C     *****************************************************************
      IMPLICIT DOUBLE PRECISION(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8, DIMENSION(800) :: A
      REAL*8 :: AKG
      INTEGER :: J, J1, J2, M, M1, M2
      COMMON /KG    / J1, J2, M1, M2, J, M, AKG
      COMMON /LOFAC / A
C
C Local variables
C
      REAL*8 :: c1, cl, cl1, fln
      INTEGER :: i1, k, n, nb, nf, nf1, nf2, nf3, nf4, nf5, nm
      INTEGER :: IABS
C
C*** End of declarations rewritten by SPAG
C
      IF(M1 + M2.EQ.M)THEN
        IF(IABS(J1 - J2).LE.J)THEN
          IF(J1 + J2.GE.J)THEN
            nf = J1 + J2 - J
            IF(nf/2*2.EQ.nf)THEN
              k = J1 + J2 + J
              IF(M1.NE.0.OR.M.NE.0.OR.k/4*4.EQ.k)THEN
                fln = A(nf + 2)
                cl = fln
                nf = J1 - J2 + J
                fln = A(nf + 2)
                cl = cl + fln
                nf = J + J2 - J1
                fln = A(nf + 2)
                cl = cl + fln
                nf = J1 + J2 + J + 2
                fln = A(nf + 2)
                cl = cl - fln
                nf = J1 + M1
                fln = A(nf + 2)
                cl = cl + fln
                nf = J1 - M1
                fln = A(nf + 2)
                cl = cl + fln
                nf = J2 + M2
                fln = A(nf + 2)
                cl = cl + fln
                nf = J2 - M2
                fln = A(nf + 2)
                cl = cl + fln
                nf = J + M
                fln = A(nf + 2)
                cl = cl + fln
                nf = J - M
                fln = A(nf + 2)
                cl = 0.5*(cl + fln)
                nf1 = J1 + J2 - J
                nf2 = J1 - M1
                nf3 = J2 + M2
                nf4 = J - J2 + M1
                nf5 = J - J1 - M2
                nb = nf1
                nm = -nf4
                IF(nf2.LT.nb)THEN
                  nb = nf2
                ENDIF
                IF(nf3.LT.nb)THEN
                  nb = nf3
                ENDIF
                IF( - nf5.GT.nm)nm = -nf5
                IF(nm.LT.0)nm = 0
                nm = nm + 2
                nb = nb + 2
                AKG = 0.
                IF(nb.GE.nm)THEN
                  DO i1 = nm, nb, 2
                    c1 = 1.
                    n = i1 - 2
                    nf = n
                    fln = A(nf + 2)
                    cl1 = cl - fln
                    nf = nf1 - n
                    fln = A(nf + 2)
                    cl1 = cl1 - fln
                    nf = nf2 - n
                    fln = A(nf + 2)
                    cl1 = cl1 - fln
                    nf = nf3 - n
                    fln = A(nf + 2)
                    cl1 = cl1 - fln
                    nf = nf4 + n
                    fln = A(nf + 2)
                    cl1 = cl1 - fln
                    nf = nf5 + n
                    fln = A(nf + 2)
                    cl1 = cl1 - fln
                    IF(n/4*4.NE.n)c1 = -1.D0
                    AKG = AKG + c1*EXP(cl1)
                  ENDDO
                  AKG = AKG*SQRT(J + 1.D0)
                ENDIF
                GOTO 10
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      AKG = 0.
   10 RETURN
      END SUBROUTINE KLEGO1
 
!---------------------------------------------------------------------------
C     *******************************************************
      SUBROUTINE COPHA
C     *******************************************************
      IMPLICIT DOUBLE PRECISION(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: CONz, ETA
      REAL*8, DIMENSION(20,90) :: COPh
      COMMON /COUL  / CONz, ETA, COPh
C
C Local variables
C
      REAL*8 :: alf, arg, bet, co, col, error, sum7
      INTEGER :: l1, ll, ll1
C
C*** End of declarations rewritten by SPAG
C
      DO l1 = 1, 1000
        arg = ETA/l1
        alf = ATAN(arg)
        bet = SQRT(ETA**2 + l1**2)
        co = alf*(l1 - 0.5) + ETA*(LOG(bet) - 1.D0) - SIN(alf)/12.D0/bet
        sum7 = SIN(9.D0*alf)/1188.D0/bet**9
        error = ABS(sum7/co)
        IF(error.LT.10D-10)GOTO 10
      ENDDO
      PRINT 1010, error
      WRITE(21,1010)error
 1010 FORMAT(10X,'WARNING! ERROR IN COULOMB PHASE>',D12.5)
   10 ll1 = l1
      col = co + SIN(3.D0*alf)/360.D0/bet**3 - SIN(5.D0*alf)
     &      /1260.D0/bet**5 + SIN(7.D0*alf)/1680.D0/bet**7 - sum7
      ll = ll1
      IF(ll.NE.1)THEN
   15   ll = ll - 1
        col = col - ATAN(ETA/ll)
        IF(ll.GT.1)GOTO 15
      ENDIF
      COPh(20,1) = col
      RETURN
      END SUBROUTINE COPHA
 
!---------------------------------------------------------------------------
C     *******************************************************
      SUBROUTINE SPHEPOT
C     ***********************************************************
      IMPLICIT DOUBLE PRECISION(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: AC, AD, ALF, ANEu, AR, AS, ASP, AT, AW, AZ, DE, R, RC, 
     &          RD, RR, RS, RW, RZ, VP, WP, ZNUc
      COMMON /POTEB / R, DE, VP, WP
      COMMON /RAD   / RR, RC, RD, RW, RS, AR, AC, AW, AD, AS, ALF, AT, 
     &                ANEu, RZ, ZNUc, ASP, AZ
C
C Local variables
C
      REAL*8 :: an, ano, pdz, po0, po1, rgr, st, v2, v4, vv
      INTEGER :: n, nan1, nan2
C
C*** End of declarations rewritten by SPAG
C
      DE = RZ + 20.*AZ
      rgr = DE
      IF(R.GE.rgr)DE = 1.D0/R
      IF(R.LT.rgr)THEN
        CALL SPHER
        ano = DE
C     write(22,33) r,pp,de
C  33 FORMAT(6E12.4)
        DE = RZ + 20.*AZ
        an = DE/AZ + 2.
        nan1 = an*10
        nan1 = 2*nan1 + 2
        IF(nan1.GT.96)nan1 = 96
        nan2 = nan1 + 1
        st = DE/(nan2 - 1)
        vv = 0.D+00
        pdz = 1./DE**2
        CALL SPHER
        pdz = pdz*DE
        vv = vv + pdz
        v4 = 0.
        DO n = 2, nan2, 2
          DE = st*(n - 1)
          pdz = 1./DE**2
          CALL SPHER
          pdz = pdz*DE
          v4 = v4 + pdz
        ENDDO
        v2 = 0.
        DO n = 3, nan1, 2
          DE = st*(n - 1)
          pdz = 1./DE**2
          CALL SPHER
          pdz = pdz*DE
          v2 = v2 + pdz
        ENDDO
        DE = (vv + 4.*v4 + 2.*v2)*st/3.
        po0 = DE/ano + 1./rgr
        DE = R
        IF(R.EQ.0.)po1 = 0.
        IF(R.NE.0.)THEN
          an = DE/AZ + 2.
          nan1 = an*5
          nan1 = 2*nan1 + 2
          IF(nan1.GT.48)nan1 = 48
          nan2 = nan1 + 1
          st = DE/(nan2 - 1)
          vv = 0.D+00
          pdz = 1./DE**2
          CALL SPHER
          pdz = pdz*DE
          vv = vv + pdz
          v4 = 0.
          DO n = 2, nan2, 2
            DE = st*(n - 1)
            pdz = 1./DE**2
            CALL SPHER
            pdz = pdz*DE
            v4 = v4 + pdz
          ENDDO
          v2 = 0.
          DO n = 3, nan1, 2
            DE = st*(n - 1)
            pdz = 1./DE**2
            CALL SPHER
            pdz = pdz*DE
            v2 = v2 + pdz
          ENDDO
          po1 = (vv + 4.*v4 + 2.*v2)*st/3.
        ENDIF
        DE = po0 - po1/ano
      ENDIF
C     PRINT 33,R,PP, DE,PO0,PO1,ANO
      RETURN
      END SUBROUTINE SPHEPOT
 
!---------------------------------------------------------------------------
C     ***********************************************************
      SUBROUTINE SPHER
C     ***********************************************************
      IMPLICIT DOUBLE PRECISION(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: AC, AD, ALF, ANEu, AR, AS, ASP, AT, AW, AZ, DE, R, RC, 
     &          RD, RR, RS, RW, RZ, VP, WP, ZNUc
      COMMON /POTEB / R, DE, VP, WP
      COMMON /RAD   / RR, RC, RD, RW, RS, AR, AC, AW, AD, AS, ALF, AT, 
     &                ANEu, RZ, ZNUc, ASP, AZ
C
C Local variables
C
      REAL*8 :: an, ex, ex1, exd, pdz, st, v2, v4, vv, x, y
      INTEGER :: n, nan1, nan2
C
C*** End of declarations rewritten by SPAG
C
      IF(DE.NE.0.)THEN
        an = DE/AZ + 2.
        nan1 = an*10
        nan1 = 2*nan1 + 2
        IF(nan1.GT.96)nan1 = 96
        nan2 = nan1 + 1
        st = DE/(nan2 - 1)
C     PRINT 88,ST,NAN2
C     STOP
        vv = 0.D+00
        x = (DE - RZ)/AZ
        IF(x.GT.23.)THEN
          pdz = EXP( - x)*DE**2
        ELSE
          ex = EXP(x)
          ex1 = 1.D0 + ex
          exd = 1.D0/ex1
          pdz = exd*DE**2
        ENDIF
        vv = vv + pdz
        v4 = 0.
        DO n = 2, nan2, 2
          y = st*(n - 1)
          x = (y - RZ)/AZ
          IF(x.GT.23.)THEN
            pdz = EXP( - x)*y**2
          ELSE
            ex = EXP(x)
            ex1 = 1.D0 + ex
            exd = 1.D0/ex1
            pdz = exd*y**2
 1010       FORMAT(4E12.3)
          ENDIF
          v4 = v4 + pdz
        ENDDO
        v2 = 0.
        DO n = 3, nan1, 2
          y = st*(n - 1)
          x = (y - RZ)/AZ
          IF(x.GT.23.)THEN
            pdz = EXP( - x)*y**2
          ELSE
            ex = EXP(x)
            ex1 = 1.D0 + ex
            exd = 1.D0/ex1
            pdz = exd*y**2
          ENDIF
          v2 = v2 + pdz
        ENDDO
        DE = (vv + 4.D0*v4 + 2.D0*v2)*st/3.D0
      ENDIF
      RETURN
      END SUBROUTINE SPHER
 
!---------------------------------------------------------------------------
C     ***********************************************************
      SUBROUTINE POTVOL
C     ***********************************************************
      IMPLICIT DOUBLE PRECISION(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: AC, AC0, AC1, AD, AD0, AD1, ALAso, ALAvr, ALAwd, ALF, 
     &          ANEu, AR, AR0, AR1, AS, AS0, AS1, ASP, AT, AW, AW0, AW1, 
     &          AZ, BNDc, CBEt0, CCOul, CISo, EFErmn, EFErmp, EN, PDIs, 
     &          RC, RD, RR, RRBwc, RRWid, RS, RW, RZ, RZBwc, RZWid, VR0, 
     &          VR1, VR2, VR3, VRLa, VS, WC0, WC1, WCA1, WCBw, WCIso, 
     &          WCWid, WD0, WD1, WDA1, WDBw, WDWid, WS0, WS1, WSBw, 
     &          WSWid, ZNUc
      REAL*8, DIMENSION(10) :: BET
      REAL*8, DIMENSION(20) :: EL
      INTEGER :: LAS, MEApp, MECha, MECul, MEDis, MEHam, MEHao, MEJob, 
     &           MEPot, MEPri, MERel, MERip, MERrr, MERzz, MESha, MESho, 
     &           MESol, MEVol, NMAx, NPD, NUR
      COMMON /CVOL  / CBEt0
      COMMON /ENA   / EN, EL, BET, NUR, NMAx, NPD, LAS
      COMMON /MENU  / MEJob, MEPot, MEHam, MECha, MEPri, MESol, MESha, 
     &                MESho, MEHao, MEApp, MEVol, MERel, MECul, MERzz, 
     &                MERrr, MEDis, MERip
      COMMON /POT1  / VR3, VR2, VR1, VR0, WC1, WC0, WD1, WD0, VS, AC0, 
     &                AR0, AW0, AD0, AS0
      COMMON /POT2  / AR1, AC1, AW1, AD1, AS1
      COMMON /POT3  / BNDc, WDA1, WCA1, CCOul, CISo, WCIso, WS0, WS1
      COMMON /POTD  / ALAvr, VRLa, WCBw, WCWid, WDBw, WDWid, ALAwd, 
     &                EFErmn, EFErmp, ALAso, PDIs, WSBw, WSWid, RRBwc, 
     &                RRWid, RZBwc, RZWid
      COMMON /RAD   / RR, RC, RD, RW, RS, AR, AC, AW, AD, AS, ALF, AT, 
     &                ANEu, RZ, ZNUc, ASP, AZ
C
C Local variables
C
      REAL*8 :: a42, al, am, an, cbetc, cbetd, cbetw, ex, ex1, exd, 
     &          exdm, exx, pv0, pv1, pv2, pv3, pwc0, pwc1, pwc2, 
     &          pwc3, pwd0, pwd1, pwd2, pwd3, pww0, pww1, 
     &          pww2, pww3, r, rac, rad, rar, raw, rm, rrr, st, 
     &          vir0, vir1, vir2, vir3, wcc, wdd, wic0, wic1, wic2, 
     &          wic3, wid0, wid1, wid2, wid3, wiw0, wiw1, wiw2, wiw3, 
     &          www, x, xx, y, y1, y2, y3, y4, yc1, yc2, yc3
      INTEGER :: is, n, nan1, nan2
C
C*** End of declarations rewritten by SPAG
C
C     CALCULATES INTEGRALS OF FORM FACTOR, IT'S FIRST DERIVATIVE
C
C     MULTIPLIED BY (-1)*(Ri/Ai) AND IT'S SECOND DERIVATIVE
C     MULTIPLIED BY (1/2)*(Ri/Ai)**2
C     INTEGRALS BY RADIUS FROM ZERO TO INFINITY. TO GET VOLUME
C     INTEGRALS MULTIPLY BY 4*PI
      CBEt0 = 1.
      is = 2
      rrr = RR
      IF(MERrr.EQ.1)RR = RR*(1.D00 - RRBwc*EN**PDIs/(EN**2 + RRWid**PDIs
     &                   ))
      AR = AR0 + AR1*EN
      AW = AW0 + AW1*EN
      AC = AC0 + AC1*EN
      AD = AD0 + AD1*EN
      IF(EN.GT.BNDc)AD = AD0 + AD1*BNDc
      rm = RR
      rar = RR/AR
      rad = RD/AD
      raw = RW/AW
      rac = RC/AC
      IF(RW.GT.rm)rm = RW
      IF(RC.GT.rm)rm = RC
      IF(RD.GT.rm)rm = RD
      am = AR
      IF(AW.LT.am)am = AW
      IF(AC.LT.am)am = AC
      IF(AD.LT.am)am = AD
      al = AR
      IF(AW.GT.al)al = AW
      IF(AC.GT.al)al = AC
      IF(AD.GT.al)al = AD
      an = rm/am + 23.*al/am
      nan1 = an*15
      nan1 = 2*nan1 + 2
      IF(nan1.GT.148)nan1 = 148
      nan2 = nan1 + 1
      st = (rm + 23.*al)/(nan2 - 1)
      a42 = 4.D0*st/3.D0
C
      vir0 = 0.
      vir1 = 0.
      vir2 = 0.
      vir3 = 0.
      wic0 = 0.
      wic1 = 0.
      wic2 = 0.
      wic3 = 0.
      wid0 = 0.
      wid1 = 0.
      wid2 = 0.
      wid3 = 0.
      wiw0 = 0.
      wiw1 = 0.
      wiw2 = 0.
      wiw3 = 0.
   10 DO n = is, nan2, 2
        r = st*(n - 1)
        x = (r - RR)/AR
        IF(x.GT.23.)THEN
          ex1 = EXP( - x)
          pv0 = ex1
          IF(LAS.NE.0)THEN
            pv1 = pv0*rar
            IF(LAS.NE.1)THEN
              pv2 = pv1*rar/2.
              IF(LAS.NE.2)THEN
                pv3 = pv2*rar/3.
              ENDIF
            ENDIF
          ENDIF
        ELSE
          ex = EXP(x)
          ex1 = 1. + ex
          exd = 1./ex1
          exdm = 1. - exd
          pv0 = exd
          IF(LAS.NE.0)THEN
            pv1 = pv0*exdm*rar
            IF(LAS.NE.1)THEN
              y1 = 1. - 2.*exd
              pv2 = pv1*y1*rar/2.
              IF(LAS.NE.2)THEN
                y2 = 1. - 6.*exd*exdm
                pv3 = pv2/y1*y2*rar/3.
                IF(LAS.NE.3)THEN
                  y3 = 1. - exd*(14. - exd*(36. - 24.*exd))
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
        x = (r - RC)/AC
        y = (r - RW)/AW
        xx = y*y
        IF(xx.GT.180.)exx = 0.
        IF(xx.LE.180.)exx = EXP( - xx)
        IF(x.GT.23.)THEN
          ex1 = EXP( - x)
          wcc = ALF*ex1
          www = (1. - ALF)*exx
          pwc0 = wcc
          pww0 = www
          IF(LAS.NE.0)THEN
            wcc = wcc*rac
            www = www*2.*raw*y
            pwc1 = wcc
            pww1 = www
            IF(LAS.NE.1)THEN
              wcc = wcc*rac/2.
              y1 = 2.*y*y - 1.
              www = www/y*y1*raw/2.
              pwc2 = wcc
              pww2 = www
              IF(LAS.NE.2)THEN
                wcc = wcc*rac/3.
                y2 = y**3*2. - y*3.
                www = www/y1*y2*raw*2./3.
                pwc3 = wcc
                pww3 = www
                IF(LAS.NE.3)THEN
                  wcc = wcc*rac/4.
                  www = www/y2*(y**4*4. - y*y*12. + 3.)*raw/4.
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ELSE
          ex = EXP(x)
          ex1 = 1. + ex
          exd = 1./ex1
          exdm = 1. - exd
          wcc = ALF*exd
          www = (1. - ALF)*exx
          pwc0 = wcc
          pww0 = www
          IF(LAS.NE.0)THEN
            wcc = wcc*exdm*rac
            www = www*2.*raw*y
            pwc1 = wcc
            pww1 = www
            IF(LAS.NE.1)THEN
              yc1 = 1. - 2.*exd
              wcc = wcc*yc1*rac/2.
              y1 = 2.*y*y - 1.
              www = www/y*y1*raw/2.
              pwc2 = wcc
              pww2 = www
              IF(LAS.NE.2)THEN
                yc2 = 1. - 6.*exd*exdm
                wcc = wcc/yc1*yc2*rac/3.
                y2 = y**3*2. - y*3.
                www = www/y1*y2*raw*2./3.
                pwc3 = wcc
                pww3 = www
                IF(LAS.NE.3)THEN
                  yc3 = 1. - exd*(14. - exd*(36. - 24.*exd))
                  wcc = wcc/yc2*yc3*rac/4.
                  www = www/y2*(y**4*4. - y*y*12. + 3.)*raw/4.
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
        x = (r - RD)/AD
        IF(x.GT.23.)THEN
          ex1 = EXP( - x)
          wdd = 4.*ex1
          pwd0 = wdd
          IF(LAS.NE.0)THEN
            wdd = wdd*rad
            pwd1 = wdd
            IF(LAS.NE.1)THEN
              wdd = wdd*rad/2.
              pwd2 = wdd
              IF(LAS.NE.2)THEN
                wdd = wdd*rad/3.
                pwd3 = wdd
                IF(LAS.NE.3)THEN
                  wdd = wdd*rad/4.
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ELSE
          ex = EXP(x)
          ex1 = 1. + ex
          exd = 1./ex1
          exdm = 1. - exd
          wdd = 4.*exdm*exd
          pwd0 = wdd
          IF(LAS.NE.0)THEN
            y1 = 1. - 2.*exd
            wdd = wdd*y1*rad
            pwd1 = wdd
            IF(LAS.NE.1)THEN
              y2 = 1. - 6.*exd*exdm
              wdd = wdd/y1*y2*rad/2.
              pwd2 = wdd
              IF(LAS.NE.2)THEN
                y3 = 1. - exd*(14. - exd*(36. - 24.*exd))
                wdd = wdd/y2*y3*rad/3.
                pwd3 = wdd
                IF(LAS.NE.3)THEN
                  y4 = 1. - exd*(30. - exd*(150. - exd*(240.-120.*exd)))
                  wdd = wdd/y3*y4*rad/4.
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
        vir0 = vir0 + r**2*pv0*a42
        vir1 = vir1 + r**2*pv1*a42
        vir2 = vir2 + r**2*pv2*a42
        vir3 = vir3 + r**2*pv3*a42
        wic0 = wic0 + r**2*pwc0*a42
        wic1 = wic1 + r**2*pwc1*a42
        wic2 = wic2 + r**2*pwc2*a42
        wic3 = wic3 + r**2*pwc3*a42
        wid0 = wid0 + r**2*pwd0*a42
        wid1 = wid1 + r**2*pwd1*a42
        wid2 = wid2 + r**2*pwd2*a42
        wid3 = wid3 + r**2*pwd3*a42
        wiw0 = wiw0 + r**2*pww0*a42
        wiw1 = wiw1 + r**2*pww1*a42
        wiw2 = wiw2 + r**2*pww2*a42
        wiw3 = wiw3 + r**2*pww3*a42
      ENDDO
      is = is + 1
      nan2 = nan1
      a42 = 2.*st/3.
      IF(is.LE.3)GOTO 10
      cbetc = 0.D0
      cbetd = 0.D0
      cbetw = 0.D0
      IF(wic1.NE.0.)cbetc = wic2/wic1
      IF(wid1.NE.0.)cbetd = wid2/wid1
      IF(wiw1.NE.0.)cbetw = wiw2/wiw1
      IF(MEVol.GT.1)THEN
        CBEt0 = vir2/vir1
      ENDIF
      WRITE(21,1010)vir0, vir1, vir2, vir3, CBEt0
 1010 FORMAT(/2X,
     &'SPHERICAL VOLUME INTEGRALS OF REAL POTENTIAL F-FACTORS AND DERIVA
     &TIVES:'/2X,'VIR0=',F8.3,2X,'VIR1=',F8.3,2X,'VIR2=',F8.3,2X,
     &'VIR3=',F8.3,2X,'CBET0=',F8.3)
      WRITE(21,1020)wic0, wic1, wic2, wic3, cbetc
 1020 FORMAT(2X,
     &'SPHERICAL VOLUME INTEGRALS OF IMAGINARY (WC) F-FACTORS AND DERIVA
     &TIVES:'/2X,'WIC0=',F8.3,2X,'WIC1=',F8.3,2X,'WIC2=',F8.3,2X,
     &'WIC3=',F8.3,2X,'CBETC=',F8.3)
      WRITE(21,1030)wid0, wid1, wid2, wid3, cbetd
 1030 FORMAT(2X,
     &'SPHERICAL VOLUME INTEGRALS OF IMAGINARY (WD) F-FACTORS AND DERIVA
     &TIVES:'/2X,'WID0=',F8.3,2X,'WID1=',F8.3,2X,'WID2=',F8.3,2X,
     &'WID3=',F8.3,2X,'CBETD=',F8.3)
      WRITE(21,1040)wiw0, wiw1, wiw2, wiw3, cbetw
 1040 FORMAT(2X,
     &'SPHERICAL VOLUME INTEGRALS OF IMAGINARY (WW) F-FACTORS AND DERIVA
     &TIVES:'/2X,'WIW0=',F8.3,2X,'WIW1=',F8.3,2X,'WIW2=',F8.3,2X,
     &'WIW3=',F8.3,2X,'CBETW=',F8.3)
      RR = rrr
      RETURN
      END SUBROUTINE POTVOL
 
!---------------------------------------------------------------------------
C     ***********************************************************
      SUBROUTINE VHFROM
C     ***********************************************************
      IMPLICIT DOUBLE PRECISION(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: AC, AD, ALAso, ALAvr, ALAwd, ALF, AMI, ANEu, AR, AS, 
     &          ASP, AT, AW, AZ, EFErm, EFErmn, EFErmp, ENCon, PDIs, RC, 
     &          RD, RR, RRBwc, RRWid, RS, RW, RZ, RZBwc, RZWid, VHF, 
     &          VRLa, VRLanp, WCBw, WCWid, WDBw, WDWid, WSBw, WSWid, 
     &          WW4, ZNUc
      COMMON /POTD  / ALAvr, VRLa, WCBw, WCWid, WDBw, WDWid, ALAwd, 
     &                EFErmn, EFErmp, ALAso, PDIs, WSBw, WSWid, RRBwc, 
     &                RRWid, RZBwc, RZWid
      COMMON /RAD   / RR, RC, RD, RW, RS, AR, AC, AW, AD, AS, ALF, AT, 
     &                ANEu, RZ, ZNUc, ASP, AZ
      COMMON /VHFS  / WW4, VHF, ENCon, VRLanp, EFErm, AMI
C
C Local variables
C
      REAL*8 :: alav, apan, const, derel, dir, eirelm, eirelt, ematch, 
     &          rng, rel, step, val, value, vrlnp, ww4216
      INTEGER :: ifpot
C
C*** End of declarations rewritten by SPAG
C
C     WW4 - ENERGY CONVERSION FACTOR DEVIDED BY 4.
C     ALAVR - POTENTIAL NON-LOCALITY IN FM.
C     ENCON - INCIDENT ENERGY
C     EMATCH - MATCHING ENERGY
C     VAL - VALUE OF VHF REAL POTENTIAL AT MATCHING POINT,
C      USING EXPONENTIAL DECAY FORMULAE
C     DIR - DERIVATIVE OF VHT POTENTIAL AT MATCHING POINT
C     USING EXPONENTIAL DECAY FORMULAE
 
      apan = ANEu/1.0086652D0
      ifpot = 0
      ematch = 200.00D0
 
      vrlnp = VRLanp
      alav = ALAvr
      GOTO 20
 
      IF(ifpot.NE.0)THEN
        IF(ematch.LE.ENCon)GOTO 20
        rel = (ematch + AMI)/AMI
        IF(rel.LT.1.D00)rel = 1.D00
        eirelm = SQRT(rel**2*AT**2 + 2.D+0*rel*ANEu*AT + ANEu**2)
        eirelt = AT*SQRT(rel**2*ANEu**2 + 2.D+0*rel*ANEu*AT + AT**2)
        derel = SQRT(2.D+0*rel*ANEu*AT + AT**2 + ANEu**2)
C        ENCON=(REL**2-1.D0)*AMI*AT*(ANEU+AT)/DEREL**2/2.D+0
        WW4 = 1.2064496D-2*eirelm*eirelt/(ANEu*eirelm + eirelt)
     &        /derel*apan
 
        val = VRLanp*EXP( - ALAvr**2*(ematch - EFErm)*WW4)
        dir = -val*ALAvr**2*WW4*(1.D+00 + (ematch - EFErm)
     &        /(ematch + AMI))
 
 
C     ALAVR - MARCHED NON-LOCALITY RANGE FOR PEREY BUCK FORMULA
C     VRLANP - MATCHED VHFO FOR PEREY BUCK FORMULA
 
        ALAvr = SQRT
     &          ( - dir/val/(1.D+00 + dir + (ematch+val)/(ematch+AMI))
     &          /WW4)
        VRLanp = val*EXP(ALAvr**2*(ematch + val)*WW4)
      ENDIF
 
C     MORILLIOM ROMAIM FORMULAE
C     VHF AT EN BELOW EMATCH, CALCULATED BY PEREY BUCK FORMULA,
C      ACCOUNTING RELATIVISTIC EFFECTS
C     WITH PARAMETERS MATCHED WITH EXPONENTIAL DECAY DEPENDENCE
 
      rel = (ENCon + AMI)/AMI
      IF(rel.LT.1.D00)rel = 1.D00
      eirelm = SQRT(rel**2*AT**2 + 2.D+0*rel*ANEu*AT + ANEu**2)
      eirelt = AT*SQRT(rel**2*ANEu**2 + 2.D+0*rel*ANEu*AT + AT**2)
      derel = SQRT(2.D+0*rel*ANEu*AT + AT**2 + ANEu**2)
C        ENCON=(REL**2-1.D0)*AMI*AT*(ANEU+AT)/DEREL**2/2.D+0
      WW4 = 1.2064496D-2*eirelm*eirelt/(ANEu*eirelm + eirelt)/derel*apan
      const = VRLanp*EXP( - WW4*ALAvr**2*ENCon)
      rng = 0.000D+00
      ww4216 = WW4**2*16.D+00
      VHF = 0.D+00
      step = 45.D+00
   10 VHF = VHF + step
      IF(VHF.GT.5.45D+02)THEN
        PRINT 1010, ALAvr, ENCon, value, WW4
        WRITE(21,1010)ALAvr, ENCon, value, WW4
 1010   FORMAT(10X,'ERROR! CHECK ALAVR OR VHF SIGN:NO SOLUTION FOR VHF'/
     &         5X,'ALAVR=',D11.5,', ENCON=',D11.5,',VALUE=',D11.5,
     &         ',WW4=',D11.5)
        STOP
      ELSE
        value = const*EXP( - WW4*ALAvr**2*VHF + 
     &          ww4216*(rng**4*(ENCon+VHF))**2)
        IF(value.LT.VHF)THEN
          VHF = VHF - step
          step = step/3.0
          IF(step.GE.1.D-04)GOTO 10
        ELSEIF(value.NE.VHF)THEN
          GOTO 10
        ENDIF
        VRLanp = vrlnp
        ALAvr = alav
        RETURN
      ENDIF
 
C     VHF AT EN AS EXPONENTIAL DECAY, WITH ALAVR AS NON-LOCALITY RANGE,
C     WITH REDUCED MASS ACCOUNTING RELATIVISTIC EFFECTS
 
 
C     CALCULATIONS OF RELATIVISTIC REDUCED MASS OF SYSTEM DEVIDED BY 2*h**2
 
   20 rel = (ENCon + AMI)/AMI
      IF(rel.LT.1.D00)rel = 1.D00
      eirelm = SQRT(rel**2*AT**2 + 2.D+0*rel*ANEu*AT + ANEu**2)
      eirelt = AT*SQRT(rel**2*ANEu**2 + 2.D+0*rel*ANEu*AT + AT**2)
      derel = SQRT(2.D+0*rel*ANEu*AT + AT**2 + ANEu**2)
C      ENCON=(REL**2-1.D0)*AMI*AT*(ANEU+AT)/DEREL**2/2.D+0
      WW4 = 1.2064496D-2*eirelm*eirelt/(ANEu*eirelm + eirelt)/derel*apan
 
C     THIS IS NON RELATIVISTIC REDUCED MASS OF SYSTEM DEVIDED BY 2*h**2
      WW4 = 1.2064496D-2*AT/(ANEu + AT)*apan
 
C     The line below is in the spirit of Morillon and Romain formulation
C     VHF=VRLANP*EXP(-ALAVR**2*(ENCON-EFERM)*WW4)
C
C     To follow our published papers on PRC
C
      VHF = VRLanp*EXP( - ALAvr*(ENCon - EFErm))
      RETURN
      END SUBROUTINE VHFROM
 
!---------------------------------------------------------------------------
C     *******************************************************
      SUBROUTINE THORA(Iout)
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      INTEGER :: Iout
C
C Local variables
C
      REAL*8, SAVE :: begday, begtim, difti1, diftim, endday, endtim
      CHARACTER(8) :: date
      INTEGER, DIMENSION(8) :: dt
      INTEGER :: INT, NINT
      LOGICAL :: never_called
      CHARACTER(10) :: time
      CHARACTER(5) :: zone
C
C*** End of declarations rewritten by SPAG
C
C     *******************************************************
C     AUTHOR: R. Capote, March 2005
C
C     * $Date: 2007/09/03 14:20:35 $
C     * $Id: thora.f,v 1.6 2007/09/03 14:20:35 Capote Exp $
C
C     GIVES THE TIME ELAPSED SINCE THE FIRST CALL
C     Note: Elapsed time must be less than one month
C
C     (g77, g95, LAHEY, and MS FORTRAN compatible)
C
      DATA never_called/.TRUE./
 
      IF(never_called)THEN
        never_called = .FALSE.
        CALL DATE_AND_TIME(date,time,zone,dt)
        begday = dt(3)
        begtim = dt(5)*3600.D0 + dt(6)*60.D0 + dt(7) + dt(8)/1000.D0
        WRITE(Iout,1010)time(1:2), time(3:4), time(5:6), date(7:8), 
     &                  date(5:6), date(1:4)
      ELSE
        CALL DATE_AND_TIME(date,time,zone,dt)
        endtim = dt(5)*3600.D0 + dt(6)*60.D0 + dt(7) + dt(8)/1000.D0
        endday = dt(3)
        endtim = endtim + (endday - begday)*86400.D0
        diftim = (endtim - begtim)/60.D0
        difti1 = (diftim - INT(diftim))*60.D0
        WRITE(Iout,1020)INT(diftim), NINT(difti1)
      ENDIF
 
      RETURN
 1010 FORMAT(/10X,'Start time: ',A2,':',A2,'.',A2,' (',A2,'-',A2,'-',A4,
     &       ')'/)
 1020 FORMAT(//10X,' Calculation time: ',I3,' min ',I2,' s'//)
C====================================================================
      END SUBROUTINE THORA
 
!---------------------------------------------------------------------------
C     *******************************************************
C     END of ccrd
C     *******************************************************
C     *******************************************************
C     START of knditd
C     *******************************************************
C     ****************************************************************
      SUBROUTINE KNCOE
C     *******************************************************
      IMPLICIT DOUBLE PRECISION(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: AKG, AMB0, AMG0, AMO, AMUo, BB32, BB42, BET0, BET3, 
     &          BET4, BMO, CMO, DELg, DPAr, EN, ETO, GAM0, GAMde, GAMg, 
     &          GSHape, HW, HWO
      REAL*8, DIMENSION(15) :: ALK11, ALK21, ALK22, ALK31, ALK32, ALK41, 
     &                         ALK42, ALK43, B0Lk1, B0Lk2, B0O1, B0O2, 
     &                         B1Lk21, B1Lk22, B1O21, B1O22, B2Lk31, 
     &                         B2Lk32, B2Lk33, B2O31, B2O32, B2O33, BO2
      REAL*8, DIMENSION(10) :: BET
      REAL*8, DIMENSION(20) :: EL
      INTEGER :: J, J1, J2, LAS, M, M1, M2, MEApp, MECha, MECul, MEDis, 
     &           MEHam, MEHao, MEJob, MEPot, MEPri, MERel, MERip, MERrr, 
     &           MERzz, MESha, MESho, MESol, MEVol, NMAx, NPD, NUR
      REAL*8, DIMENSION(20,20,14) :: TRIg
      COMMON /ALL   / ALK11, ALK21, ALK22, ALK31, ALK32, ALK41, ALK42, 
     &                ALK43, B0Lk1, B0Lk2, B1Lk21, B1Lk22, B2Lk31, 
     &                B2Lk32, B2Lk33
      COMMON /ENA   / EN, EL, BET, NUR, NMAx, NPD, LAS
      COMMON /INRM  / AMO, BMO, CMO, BB42, GAMg, DELg
      COMMON /KG    / J1, J2, M1, M2, J, M, AKG
      COMMON /MENU  / MEJob, MEPot, MEHam, MECha, MEPri, MESol, MESha, 
     &                MESho, MEHao, MEApp, MEVol, MERel, MECul, MERzz, 
     &                MERrr, MEDis, MERip
      COMMON /OLL   / B0O1, B1O21, B1O22, B2O31, B2O32, B2O33, BO2, B0O2
      COMMON /SHAMO / BET3, ETO, AMUo, HWO, BB32, DPAr
      COMMON /SHEM1 / HW, AMG0, AMB0, GAM0, BET0, BET4, GAMde, GSHape
      COMMON /TRIGF / TRIg
C
C Local variables
C
      REAL*8 :: a30, a32, a40, a42, a44, abck, abk, ak1, ak2, ak3, ak4, 
     &          bet42, bk1, bk2, bk3, bk4, bk5, bk6, bk7, bk8, bk9, c1, 
     &          c5, c6325, c7, c75, c95, ck1, ck10, ck11, ck2, ck3, ck4, 
     &          ck5, ck6, ck7, ck8, ck9, cq5, cq7, cq95, csdg, csgg, 
     &          csq75, dk1, dk10, dk11, dk12, dk13, dk2, dk3, dk4, dk5, 
     &          dk6, dk7, dk8, dk9, ok1, ok2, ok3, ok4, ok5, ol1, ol2, 
     &          ol3, ol4, ol5, ol6, ol7, ol8, om1, om2, om3, om4, om5, 
     &          om6, om7, om8, on1, on2
      INTEGER :: i, j223, j32, j3m, j3p, j4m, j4p, ji2, ji223, ji3, 
     &           ji32, ji33, ji4, jj2, jj3, jj4, jma, jmi, k223, k32, 
     &           k33, k34, kk2, kk3, kk4, lk
      INTEGER :: IABS
      INTEGER, DIMENSION(5) :: nla
      REAL*8 :: on3, on4, sq2, sq32, ssdg, ssgg
C
C*** End of declarations rewritten by SPAG
C
      DATA nla/0, 1, 3, 6, 10/
      sq2 = SQRT(2.D0)
      c1 = sq2
C     SQ32=2.828427D0
      sq32 = sq2**3
      c95 = 1.8D0
      cq95 = 1.34164D0
      c75 = 7.D0/5.D0
      csq75 = SQRT(c75)
      c6325 = 63.D0/25.D0
      c6325 = SQRT(c6325)
      DO i = 1, 15
        ALK11(i) = 0.D0
        ALK21(i) = 0.D0
        ALK22(i) = 0.D0
        ALK31(i) = 0.D0
        ALK32(i) = 0.D0
        ALK41(i) = 0.D0
        ALK42(i) = 0.D0
        ALK43(i) = 0.D0
        B0Lk1(i) = 0.D0
        B0Lk2(i) = 0.D0
        B1Lk21(i) = 0.D0
        B1Lk22(i) = 0.D0
        B2Lk31(i) = 0.D0
        B2Lk32(i) = 0.D0
        B2Lk33(i) = 0.D0
        B0O1(i) = 0.D0
        B1O21(i) = 0.D0
        B1O22(i) = 0.D0
        BO2(i) = 0.D0
        B0O2(i) = 0.D0
        B2O31(i) = 0.D0
        B2O32(i) = 0.D0
        B2O33(i) = 0.D0
      ENDDO
      IF(MESha.NE.1)THEN
        bet42 = BET4*BET4
        IF(MESha.GT.2)THEN
          c7 = 7.D0/12.D0
          c5 = 5.D0/12.D0
          cq5 = SQRT(c5)
          cq7 = SQRT(c7)
          IF(MESha.EQ.4)csdg = COS(DELg)
          IF(MESha.NE.4)THEN
            GAMg = GAM0
            csdg = cq7*COS(3.D0*GAM0)
          ENDIF
          ssdg = SQRT(1.D0 - csdg*csdg)
          csgg = COS(GAMg)
          ssgg = SIN(GAMg)
          a40 = cq7*csdg + cq5*ssdg*csgg
          a42 = -ssdg*ssgg/sq2
          a44 = (cq5*csdg - cq7*ssdg*csgg)/sq2
        ELSE
          a40 = 1.D0
          a42 = 0.D0
          a44 = 0.D0
        ENDIF
      ENDIF
      ALK11(2) = 1.D0
      ALK11(3) = 1.D0/sq2
      IF(LAS.NE.1)THEN
        DO jj2 = 1, 3
          ji2 = 4*(jj2 - 1)
          J1 = 4
          J2 = 4
          J = ji2
          M1 = 0
          M2 = 0
          M = 0
          CALL KLEGO
          ak1 = AKG
          DO kk2 = 1, jj2
            lk = nla(jj2) + kk2
            SELECT CASE(kk2)
            CASE(2)
              M1 = 0
              M2 = 4
              M = 4
              CALL KLEGO
              ak4 = AKG
              ALK21(lk) = ak1*ak4*sq2
            CASE(3)
              M1 = 4
              M = 8
              CALL KLEGO
              ak3 = AKG
              ALK21(lk) = ak1*ak3/2.D0
            CASE DEFAULT
              M1 = 4
              M2 = -4
              CALL KLEGO
              ak2 = AKG
              ALK21(lk) = ak1*ak1
              ALK22(lk) = ak1*ak2
            END SELECT
          ENDDO
          IF(LAS.NE.2)THEN
            j3m = IABS(jj2 - 2) + 1
            j3p = jj2 + 1
            DO jj3 = j3m, j3p
              ji3 = 4*(jj3 - 1)
              J2 = ji2
              M1 = 0
              M2 = 0
              J = ji3
              M = 0
              CALL KLEGO
              bk1 = AKG
              abk = ak1*bk1
              DO kk3 = 1, jj3
                lk = nla(jj3) + kk3
                SELECT CASE(kk3)
                CASE(2)
                  M1 = 0
                  M = 4
                  CALL KLEGO
                  bk6 = AKG
                  M1 = 4
                  M2 = 0
                  CALL KLEGO
                  bk7 = AKG
                  M1 = -4
                  M2 = 8
                  CALL KLEGO
                  bk9 = AKG
                  ALK31(lk) = ALK31(lk)
     &                        + abk*(sq2*ak4*bk6 + ak1*bk7/sq2)
                  ALK32(lk) = ALK32(lk)
     &                        + abk*(ak3*bk9/sq32 + ak2*bk7/sq2)
                CASE(3)
                  M1 = 0
                  M = 8
                  CALL KLEGO
                  bk5 = AKG
                  M1 = 4
                  M2 = 4
                  CALL KLEGO
                  bk3 = AKG
                  ALK31(lk) = ALK31(lk) + abk*(ak3*bk5/2.D0 + ak4*bk3)
                CASE(4)
                  M2 = 8
                  M = 12
                  CALL KLEGO
                  bk8 = AKG
                  ALK31(lk) = ALK31(lk) + abk*ak3*bk8
                CASE DEFAULT
                  M1 = -4
                  M2 = 4
                  CALL KLEGO
                  bk2 = AKG
                  ALK31(lk) = ALK31(lk) + abk*ak1*bk1
                  ALK32(lk) = ALK32(lk) + abk*(bk1*ak2 + ak4*bk2)
                END SELECT
              ENDDO
              IF(LAS.NE.3)THEN
                j4m = IABS(jj3 - 1) + 1
                j4p = jj3 + 1
                DO jj4 = j4m, j4p
                  ji4 = 4*(jj4 - 1)
                  J2 = ji3
                  M1 = 0
                  M2 = 0
                  J = ji4
                  M = 0
                  CALL KLEGO
                  ck1 = AKG
                  abck = abk*ck1
                  DO kk4 = 1, jj4
                    lk = nla(jj4) + kk4
                    SELECT CASE(kk4)
                    CASE(2)
                      M1 = 0
                      M = 4
                      CALL KLEGO
                      ck3 = AKG
                      M1 = 4
                      M2 = 0
                      CALL KLEGO
                      ck4 = AKG
                      M1 = -4
                      M2 = 8
                      CALL KLEGO
                      ck5 = AKG
                      ALK41(lk) = ALK41(lk)
     &                            + abck*(ck3*(ak4*bk6*sq2 + bk7*ak1/sq2
     &                            ) + ck4*bk1*ak1/sq2)
                      ALK42(lk) = ALK42(lk)
     &                            + abck*(ck3*(ak3*bk9/sq32 + bk7*ak2/
     &                            sq2)
     &                            + ck5*(ak3*bk5/sq32 + ak4*bk3/sq2)
     &                            + ck4*(bk1*ak2/sq2 + ak4*bk2/sq2))
                    CASE(3)
                      M1 = 0
                      M = 8
                      CALL KLEGO
                      ck6 = AKG
                      M1 = 4
                      M2 = 4
                      CALL KLEGO
                      ck7 = AKG
                      M1 = -4
                      M2 = 12
                      CALL KLEGO
                      ck8 = AKG
                      ALK41(lk) = ALK41(lk)
     &                            + abck*(ck6*(ak3*bk5/2.D0 + ak4*bk3)
     &                            + ck7*(ak4*bk6 + bk7*ak1/2.D0))
                      ALK42(lk) = ALK42(lk)
     &                            + abck*(ck7*(ak3*bk9/4.D0 + bk7*ak2/
     &                            2.D0) + ck8*ak3*bk8/4.)
                    CASE(4)
                      M1 = 0
                      M = 12
                      CALL KLEGO
                      ck11 = AKG
                      M1 = 4
                      M2 = 8
                      CALL KLEGO
                      ck10 = AKG
                      ALK41(lk) = ALK41(lk)
     &                            + abck*(ck11*ak3*bk8/sq32 + ck10*
     &                            (ak3*bk5/sq32 + ak4*bk3/sq2))
                    CASE(5)
                      M2 = 12
                      M = 16
                      CALL KLEGO
                      ck9 = AKG
                      ALK41(lk) = ALK41(lk) + abck*ck9*ak3*bk8/4.D0
                    CASE DEFAULT
                      M1 = -4
                      M2 = 4
                      CALL KLEGO
                      ck2 = AKG
                      ALK41(lk) = ALK41(lk) + abck*ck1*bk1*ak1
                      ALK42(lk) = ALK42(lk)
     &                            + abck*(ck1*(bk1*ak2 + ak4*bk2)
     &                            + ck2*(ak4*bk6 + bk7*ak1/2.D0))
                      ALK43(lk) = ALK43(lk)
     &                            + abck*ck2*(ak3*bk9/4.D0 + bk7*ak2/
     &                            2.D0)
                    END SELECT
                  ENDDO
                ENDDO
              ENDIF
            ENDDO
          ENDIF
        ENDDO
      ENDIF
      IF(MESha.NE.1)THEN
        B0Lk1(4) = a40*BET4
        B0Lk1(5) = a42*BET4
        B0Lk1(6) = a44*BET4
        IF(LAS.NE.1)THEN
          DO jj2 = 1, 5
            ji2 = 4*(jj2 - 1)
            J1 = 8
            J2 = 8
            J = ji2
            M1 = 0
            M2 = 0
            M = 0
            CALL KLEGO
            ck1 = AKG
            DO kk2 = 1, jj2
              lk = nla(jj2) + kk2
              SELECT CASE(kk2)
              CASE(2)
                M1 = 0
                M2 = 4
                M = 4
                CALL KLEGO
                ck4 = AKG
                M1 = -4
                M2 = 8
                CALL KLEGO
                ck5 = AKG
                B0Lk2(lk) = ck1*2.D0*(a40*a42*ck4 + a42*a44*ck5)
     &                      *c95*bet42
              CASE(3)
                M1 = 4
                M2 = 4
                M = 8
                CALL KLEGO
                ck6 = AKG
                M1 = 0
                M2 = 8
                CALL KLEGO
                ck7 = AKG
                B0Lk2(lk) = ck1*(a42*a42*ck6 + 2.D0*a40*a44*ck7)
     &                      *c95*bet42
              CASE(4)
                M1 = 4
                M = 12
                CALL KLEGO
                ck8 = AKG
                B0Lk2(lk) = ck1*2.D0*a42*a44*ck8*c95*bet42
              CASE(5)
                M1 = 8
                M = 16
                CALL KLEGO
                ck9 = AKG
                B0Lk2(lk) = ck1*a44*a44*ck9*c95*bet42
              CASE DEFAULT
                M1 = 4
                M2 = -4
                CALL KLEGO
                ck2 = AKG
                M1 = 8
                M2 = -8
                CALL KLEGO
                ck3 = AKG
                B0Lk2(lk) = ck1*(a40*a40*ck1 + 2.D0*a42*a42*ck2 + 2.D0*
     &                      a44*a44*ck3)*c95*bet42
              END SELECT
            ENDDO
            IF(jj2.GE.2.AND.jj2.LE.4)THEN
              J1 = 4
              J2 = 8
              M1 = 0
              M2 = 0
              M = 0
              CALL KLEGO
              bk1 = AKG
              DO kk2 = 1, jj2
                lk = nla(jj2) + kk2
                SELECT CASE(kk2)
                CASE(2)
                  M1 = 0
                  M2 = 4
                  M = 4
                  CALL KLEGO
                  bk3 = AKG
                  M1 = 4
                  M2 = 0
                  CALL KLEGO
                  bk4 = AKG
                  M1 = -4
                  M2 = 8
                  CALL KLEGO
                  bk5 = AKG
                  B1Lk21(lk) = 2.D0*bk1*a42*bk3*cq95*BET4
                  B1Lk22(lk) = 2.D0*bk1*(a40*bk4 + a44*bk5)
     &                         *cq95*BET4/sq2
                CASE(3)
                  M1 = 0
                  M2 = 8
                  M = 8
                  CALL KLEGO
                  bk6 = AKG
                  M1 = 4
                  M2 = 4
                  CALL KLEGO
                  bk7 = AKG
                  B1Lk21(lk) = 2.D0*bk1*a44*bk6*cq95*BET4
                  B1Lk22(lk) = 2.D0*bk1*a42*bk7*cq95*BET4/sq2
                CASE(4)
                  M2 = 8
                  M = 12
                  CALL KLEGO
                  bk8 = AKG
                  B1Lk22(lk) = 2.D0*bk1*a44*bk8*cq95*BET4/sq2
                CASE DEFAULT
                  M1 = 4
                  M2 = -4
                  CALL KLEGO
                  bk2 = AKG
                  B1Lk21(lk) = 2.D0*bk1*a40*bk1*cq95*BET4
                  B1Lk22(lk) = 2.D0*bk1*a42*bk2*cq95*BET4*sq2
                END SELECT
              ENDDO
            ENDIF
          ENDDO
          IF(LAS.NE.2)THEN
            DO jj2 = 1, 3
              jmi = IABS(jj2 - 3) + 1
              jma = jj2 + 2
              ji2 = 4*(jj2 - 1)
              J1 = 4
              J2 = 4
              J = ji2
              M1 = 0
              M2 = 0
              M = 0
              CALL KLEGO
              ak1 = AKG
              M1 = 4
              M2 = -4
              CALL KLEGO
              ak2 = AKG
              M1 = 0
              M2 = 4
              M = 4
              CALL KLEGO
              ak3 = AKG
              M1 = 4
              M = 8
              CALL KLEGO
              ak4 = AKG
              DO jj3 = jmi, jma
                ji3 = 4*(jj3 - 1)
                J1 = ji2
                J2 = 8
                J = ji3
                M1 = 0
                M2 = 0
                M = 0
                CALL KLEGO
                dk1 = AKG
                DO kk3 = 1, jj3
                  lk = nla(jj3) + kk3
                  SELECT CASE(kk3)
                  CASE(2)
                    M1 = 4
                    M2 = 0
                    M = 4
                    CALL KLEGO
                    dk4 = AKG
                    M1 = 0
                    M2 = 4
                    CALL KLEGO
                    dk5 = AKG
                    M1 = 8
                    M2 = -4
                    CALL KLEGO
                    dk6 = AKG
                    M1 = -4
                    M2 = 8
                    CALL KLEGO
                    dk7 = AKG
                    B2Lk31(lk) = B2Lk31(lk)
     &                           + 3.D0*ak1*dk1*ak1*a42*dk5*cq95*BET4
                    B2Lk32(lk) = B2Lk32(lk)
     &                           + 3.D0*ak1*dk1*(ak2*a42*dk5 + 
     &                           ak4*a42*dk6/2.)*cq95*BET4
                    B2Lk33(lk) = B2Lk33(lk)
     &                           + 3.D0*ak1*dk1*(ak3*a40*dk4 + 
     &                           ak3*a44*dk7)*cq95*BET4*sq2
                  CASE(3)
                    M1 = 8
                    M2 = 0
                    M = 8
                    CALL KLEGO
                    dk8 = AKG
                    M1 = 4
                    M2 = 4
                    CALL KLEGO
                    dk9 = AKG
                    M1 = 0
                    M2 = 8
                    CALL KLEGO
                    dk10 = AKG
                    B2Lk31(lk) = B2Lk31(lk)
     &                           + 3.D0*ak1*dk1*ak1*a44*dk10*cq95*BET4
                    B2Lk32(lk) = B2Lk32(lk)
     &                           + 3.D0*ak1*dk1*(ak3*a44*dk10 + 
     &                           ak4*a40*dk8/2.D0)*cq95*BET4
                    B2Lk33(lk) = B2Lk33(lk)
     &                           + 3.D0*ak1*dk1*ak3*a42*dk9*cq95*BET4*
     &                           sq2
                  CASE(4)
                    M1 = 8
                    M2 = 4
                    M = 12
                    CALL KLEGO
                    dk11 = AKG
                    M1 = 4
                    M2 = 8
                    CALL KLEGO
                    dk12 = AKG
                    B2Lk32(lk) = B2Lk32(lk)
     &                           + 3.D0*ak1*dk1*ak4*a42*dk11*cq95*BET4/
     &                           2.D0
                    B2Lk33(lk) = B2Lk33(lk)
     &                           + 3.D0*ak1*dk1*ak3*a44*dk12*cq95*BET4*
     &                           sq2
                  CASE(5)
                    M1 = 8
                    M = 16
                    CALL KLEGO
                    dk13 = AKG
                    B2Lk32(lk) = B2Lk32(lk)
     &                           + 3.D0*ak1*dk1*ak4*a44*dk13*cq95*BET4/
     &                           2.D0
                  CASE DEFAULT
                    M1 = 4
                    M2 = -4
                    CALL KLEGO
                    dk2 = AKG
                    M1 = 8
                    M2 = -8
                    CALL KLEGO
                    dk3 = AKG
                    B2Lk31(lk) = B2Lk31(lk)
     &                           + 3.D0*ak1*dk1*ak1*a40*dk1*cq95*BET4
                    B2Lk32(lk) = B2Lk32(lk)
     &                           + 3.D0*ak1*dk1*(ak2*a40*dk1 + 
     &                           ak4*a44*dk3)*cq95*BET4
                    B2Lk33(lk) = B2Lk33(lk)
     &                           + 3.D0*ak1*dk1*ak3*a42*dk2*cq95*BET4*
     &                           sq2*2.D0
                  END SELECT
                ENDDO
              ENDDO
            ENDDO
          ENDIF
        ENDIF
      ENDIF
      IF(MESho.NE.0)THEN
        IF(MESho.EQ.2)THEN
          a30 = COS(ETO)
          a32 = SIN(ETO)/c1
        ELSE
          a30 = 1.D0
          a32 = 0.D0
        ENDIF
        B0O1(2) = a30
        B0O1(3) = a32
        IF(LAS.NE.1)THEN
          DO j32 = 1, 4
            IF(j32.NE.4)THEN
              ji32 = 4*(j32 - 1) + 2
              J1 = 4
              J2 = 6
              J = ji32
              M1 = 0
              M2 = 0
              M = 0
              CALL KLEGO
              ok1 = AKG
              DO k32 = 1, j32
                lk = nla(j32) + k32
                SELECT CASE(k32)
                CASE(2)
                  M2 = 0
                  M = 4
                  CALL KLEGO
                  ok3 = AKG
                  M1 = 0
                  M2 = 4
                  CALL KLEGO
                  ok4 = AKG
                  B1O21(lk) = 2.D0*ok1*ok4*a32*csq75
                  B1O22(lk) = ok1*ok3*c1*a30*csq75
                CASE(3)
                  M1 = 4
                  M = 8
                  CALL KLEGO
                  ok5 = AKG
                  B1O22(lk) = ok1*ok5*c1*a32*csq75
                CASE DEFAULT
                  M1 = 4
                  M2 = -4
                  CALL KLEGO
                  ok2 = AKG
                  B1O21(lk) = 2.D0*ok1**2*a30*csq75
                  B1O22(lk) = ok1*ok2*c1*a32*csq75*2.D0
                END SELECT
              ENDDO
            ENDIF
            IF(MESha.NE.1)THEN
              J1 = 6
              J1 = 8
              J = ji32
              M1 = 0
              M2 = 0
              M = 0
              CALL KLEGO
              ol1 = AKG
              DO k34 = 1, j32
                lk = nla(j32) + k34
                SELECT CASE(k34)
                CASE(2)
                  M2 = 0
                  M = 4
                  CALL KLEGO
                  ol3 = AKG
                  M1 = 0
                  M2 = 4
                  CALL KLEGO
                  ol5 = AKG
                  M1 = -4
                  M2 = 8
                  CALL KLEGO
                  ol4 = AKG
                  B0O2(lk) = 2.D0*ol1*(ol3*a32*a40 + ol5*a30*a42 + ol4*
     &                       a32*a44)*c6325*BET4
                CASE(3)
                  M1 = 4
                  M2 = 4
                  M = 8
                  CALL KLEGO
                  ol6 = AKG
                  M1 = 0
                  M2 = 8
                  CALL KLEGO
                  ol7 = AKG
                  B0O2(lk) = 2.D0*ol1*(ol6*a32*a42 + ol7*a30*a44)
     &                       *c6325*BET4
                CASE(4)
                  M2 = 4
                  M = 12
                  CALL KLEGO
                  ol8 = AKG
                  B0O2(lk) = 2.D0*ol1*(ol8*a32*a44)*c6325*BET4
                CASE DEFAULT
                  M1 = 4
                  M2 = -4
                  CALL KLEGO
                  ol2 = AKG
                  B0O2(lk) = 2.D0*ol1*(ol1*a30*a40 + 2.D0*ol2*a32*a42)
     &                       *c6325*BET4
                END SELECT
              ENDDO
            ENDIF
            ji33 = 4*(j32 - 1)
            J1 = 6
            J2 = 6
            J = ji33
            M1 = 0
            M2 = 0
            M = 0
            CALL KLEGO
            om1 = AKG
            DO k33 = 1, j32
              lk = nla(j32) + k33
              SELECT CASE(k33)
              CASE(2)
                M1 = 0
                M2 = 4
                M = 4
                CALL KLEGO
                om3 = AKG
                BO2(lk) = om1*a30*a32*2.D0*om3*c75
              CASE(3)
                M1 = 4
                M = 8
                CALL KLEGO
                om4 = AKG
                BO2(lk) = om1*a32**2*om4*c75
              CASE(4)
              CASE DEFAULT
                M1 = 4
                M2 = -4
                CALL KLEGO
                om2 = AKG
                BO2(lk) = om1*(a30**2*om1 + 2.D0*om2*a32**2)*c75
              END SELECT
            ENDDO
            IF(LAS.NE.2)THEN
              IF(j32.EQ.4)EXIT
              J1 = 4
              J2 = 4
              J = 4*(j32 - 1)
              M1 = 0
              M2 = 0
              M = 0
              CALL KLEGO
              on1 = AKG
              M1 = 4
              M2 = -4
              CALL KLEGO
              on2 = AKG
              M1 = 0
              M2 = 4
              M = 4
              CALL KLEGO
              on3 = AKG
              M1 = 4
              M = 8
              CALL KLEGO
              on4 = AKG
              jmi = 1
              jma = j32 + 1
              IF(j32.EQ.1)jmi = 2
              DO j223 = jmi, jma
                ji223 = 4*(j223 - 1) + 2
                J1 = 4*(j32 - 1)
                J2 = 6
                J = ji223
                M1 = 0
                M2 = 0
                M = 0
                CALL KLEGO
                om1 = AKG
                DO k223 = 1, j223
                  lk = nla(j223) + k223
                  SELECT CASE(k223)
                  CASE(2)
                    M1 = 4
                    M2 = 0
                    M = 4
                    CALL KLEGO
                    om3 = AKG
                    M1 = 0
                    M2 = 4
                    CALL KLEGO
                    om4 = AKG
                    M1 = 8
                    M2 = -4
                    CALL KLEGO
                    om5 = AKG
                    B2O31(lk) = B2O31(lk)
     &                          + on1*om1*on1*om4*a32*3.D0*csq75
                    B2O32(lk) = B2O32(lk)
     &                          + on1*om1*(on2*om4 + on4*om5/2.D0)
     &                          *a32*3.D0*csq75
                    B2O33(lk) = B2O33(lk)
     &                          + on1*om1*a30*on3*om3/c1*3.D0*csq75
                  CASE(3)
                    M2 = 0
                    M = 8
                    CALL KLEGO
                    om6 = AKG
                    M1 = 4
                    M2 = 4
                    CALL KLEGO
                    om7 = AKG
                    B2O31(lk) = B2O31(lk)
     &                          + on1*om1*on1*om7*a32*3.D0*csq75
                    B2O32(lk) = B2O32(lk)
     &                          + on1*om1*(on2*om7*a32 + on4*om6*a30/
     &                          2.D0)*3.D0*csq75
                  CASE(4)
                    M1 = 8
                    M = 12
                    CALL KLEGO
                    om8 = AKG
                    B2O32(lk) = B2O32(lk)
     &                          + on1*om1*on4*om8*a32*1.5D0*csq75
                  CASE DEFAULT
                    M1 = 4
                    M2 = -4
                    CALL KLEGO
                    om2 = AKG
                    B2O31(lk) = B2O31(lk)
     &                          + on1*om1*on1*om1*a30*3.D0*csq75
                    B2O32(lk) = B2O32(lk)
     &                          + on1*om1*a30*on2*om1*3.D0*csq75
                    B2O33(lk) = B2O33(lk)
     &                          + on1*om1*a32*on3*om2*c1*3.D0*csq75
                  END SELECT
                ENDDO
              ENDDO
            ENDIF
          ENDDO
        ENDIF
      ENDIF
      RETURN
      END SUBROUTINE KNCOE
 
!---------------------------------------------------------------------------
C     *******************************************************
      SUBROUTINE KNDIT
C     *******************************************************
      IMPLICIT DOUBLE PRECISION(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8, DIMENSION(31,16) :: AIT
      REAL*8 :: AKG, AMB0, AMG0, AMO, AMUo, BB32, BB42, BET0, BET3, 
     &          BET4, BMO, CG1, CG2, CG3, CG4, CMO, CSG11, CSG12, CSG13, 
     &          CSG21, CSG22, CSG31, CSS, DELg, DPAr, EN, ETO, GAM0, 
     &          GAMde, GAMg, GSHape, HW, HWO, SG1, SG2, SG3, SG4, W
      REAL*8, DIMENSION(15) :: ALK11, ALK21, ALK22, ALK31, ALK32, ALK41, 
     &                         ALK42, ALK43, B0Lk1, B0Lk2, B0O1, B0O2, 
     &                         B1Lk21, B1Lk22, B1O21, B1O22, B2Lk31, 
     &                         B2Lk32, B2Lk33, B2O31, B2O32, B2O33, BO2
      REAL*8, DIMENSION(20) :: ANB, BETb, EGB, EL, EP0, EP1, EP12, EP2, 
     &                         EPB, EPG, ES, GIT, PT, XI
      REAL*8, DIMENSION(20,2) :: ANG, CD
      REAL*8, DIMENSION(10) :: BET
      REAL*8, DIMENSION(720000) :: CVNc
      REAL*8, DIMENSION(160000) :: CVNr
      REAL*8, DIMENSION(40000) :: CVNrpn
      REAL*8, DIMENSION(2,8,8) :: FOG
      REAL*8, DIMENSION(6,6,2) :: FOK
      REAL*8, DIMENSION(20,20,4) :: FOV
      INTEGER :: INCc, INCr, J, J1, J2, JA, JB, JC, JD, JE, JF, JSO, 
     &           JSS, LAS, M, M1, M2, MEApp, MECha, MECul, MEDis, MEHam, 
     &           MEHao, MEJob, MEPot, MEPri, MERel, MERip, MERrr, MERzz, 
     &           MESha, MESho, MESol, MEVol, NCLl, NJ, NMAx, NN1, NPD, 
     &           NPIo, NPIs, NSPi, NSS, NUR
      INTEGER, DIMENSION(20) :: JJ, JO, KO, NCA, NNB, NNG, NNO, NPI, 
     &                          NPO, NTU, NUMb
      INTEGER, DIMENSION(250) :: JNJ1, KPJ1, LNJ1, NNJ1
      INTEGER, DIMENSION(180) :: JNO, LNO, NS1
      REAL*8, DIMENSION(5400) :: PRC, PVC
      REAL*8, DIMENSION(20,20,14) :: TRIg
      COMMON /ABCOUL/ PVC, PRC, CVNc
      COMMON /ALL   / ALK11, ALK21, ALK22, ALK31, ALK32, ALK41, ALK42, 
     &                ALK43, B0Lk1, B0Lk2, B1Lk21, B1Lk22, B2Lk31, 
     &                B2Lk32, B2Lk33
      COMMON /CV    / CVNr
      COMMON /CVPN  / CVNrpn
      COMMON /ENA   / EN, EL, BET, NUR, NMAx, NPD, LAS
      COMMON /FOK12 / FOK
      COMMON /INRM  / AMO, BMO, CMO, BB42, GAMg, DELg
      COMMON /JNN   / CSS, INCc, NCLl, NSS, NJ, INCr
      COMMON /KG    / J1, J2, M1, M2, J, M, AKG
      COMMON /LNP   / JSS, NPIs, NPIo, JSO, NN1, LNO, NS1, JNO, NSPi
      COMMON /MENU  / MEJob, MEPot, MEHam, MECha, MEPri, MESol, MESha, 
     &                MESho, MEHao, MEApp, MEVol, MERel, MECul, MERzz, 
     &                MERrr, MEDis, MERip
      COMMON /OLL   / B0O1, B1O21, B1O22, B2O31, B2O32, B2O33, BO2, B0O2
      COMMON /OVKN  / FOV, CG1, SG1, CG2, SG2, CSG11, CG4, SG4, CSG22, 
     &                CSG31, CSG13, CG3, SG3, CSG21, CSG12
      COMMON /QNB   / JO, NPO, KO, NCA
      COMMON /QNBBAND/ NUMb, BETb
      COMMON /QNS1  / LNJ1, JNJ1, NNJ1, KPJ1
      COMMON /RACB  / JA, JB, JC, JD, JE, JF, W
      COMMON /SHAMO / BET3, ETO, AMUo, HWO, BB32, DPAr
      COMMON /SHEM1 / HW, AMG0, AMB0, GAM0, BET0, BET4, GAMde, GSHape
      COMMON /SHEM2 / GIT, EP0, EP1, EP2, EP12, ANG, EPB, EPG, EGB, AIT, 
     &                PT, FOG, XI, ANB, CD
      COMMON /SHEMM / ES, JJ, NTU, NNB, NNG, NNO, NPI
      COMMON /TRIGF / TRIg
C
C Local variables
C
      REAL*8 :: aa, aaa, ab, ait1, ait2, akgs, alk, b12bgs, bkg, bt1, 
     &          bt2, btgs, cc, ccc, ck1, clk, foij1, foij2, 
     &          foij3, fok1, fok2, sq2, su, su1, su2
      INTEGER :: i, icll, iill, jn1, jn2, jo1, jo12, jo2, jo22, jps, k, 
     &           k1, k1pn, k2, k2pn, ka, kc1, kc2, ki1, ki2, kic1, 
     &           kic2, kk, kma1, kma2, kmi1, kmi2, ko1, ko2, ks1, ks2, 
     &           l, la, lam, las2, las8, lasc, li2, lic2, lk, lk1, lk2, 
     &           ll2, llc2, llic2, lllc2, ln1, ln2, n, ncla, ncll4, 
     &           ncllx2, nk1, nk2, npcv, npi1, npi2, nu, nu1, nu2, 
     &           numb1, numb2
      INTEGER, DIMENSION(5) :: nla
C
C*** End of declarations rewritten by SPAG
C
C     BELOW CARD IS NO NECESSARY IF MEMORY IS MORE THAN 32Mb
      DATA nla/0, 1, 3, 6, 10/
      sq2 = SQRT(2.D0)
C     SQ32=2.828427D0
      iill = NCLl*NCLl*2*9
      ncllx2 = NCLl*NCLl
      ncll4 = ncllx2*4
      DO i = 1, iill
        IF(i.LE.ncllx2)CVNrpn(i) = 0.D0
        IF(i.LE.ncll4)CVNr(i) = 0.D0
        CVNc(i) = 0.D0
      ENDDO
      las2 = LAS
      icll = NCLl*9*LAS
      las8 = LAS*9
      lasc = 1
      las8 = 9
      IF(las2.GE.2)lasc = 2
      IF(las2.GE.2)las8 = 18
      IF(MEHam.EQ.1)THEN
        JC = JSS
        las2 = LAS/2
        ncla = NCLl*las2
        btgs = BET(2)
        DO k = 1, NCLl
          k1 = (k - 1)*ncla
          k1pn = (k - 1)*NCLl
          nu = NNJ1(k)
          ko1 = KO(nu)
          jo1 = JO(nu)
          ln1 = LNJ1(k)
          J1 = JNJ1(k)
          bt1 = BETb(nu)
          numb1 = NUMb(nu)
          IF(numb1.EQ.0)bt1 = BET(2)
          jn1 = J1
          JA = J1
          JB = jo1
          DO kk = 1, NCLl
            k2 = k1 + (kk - 1)*las2
            k2pn = k1pn + kk
            IF(NPD.NE.0)THEN
              ln2 = LNJ1(kk)
              J2 = JNJ1(kk)
              jn2 = J2
              JE = J2
              cc = SQRT((J1 + 1)*(J2 + 1)/12.566371D0)
              nu1 = NNJ1(kk)
              bt2 = BETb(nu1)
              numb2 = NUMb(nu1)
              IF(numb2.EQ.0)bt2 = BET(2)
              b12bgs = SQRT(bt1*bt2/btgs**2)
              b12bgs = bt1*bt2/btgs**2
              ko2 = KO(nu1)
              jo2 = JO(nu1)
              JD = jo2
              jps = JSS - 1 - jo2 + J1 + J2 + ln2 - ln1
              jps = jps - jps/4*4
              IF(jps.NE.0)THEN
                ccc = -cc
              ELSE
                ccc = cc
              ENDIF
              DO l = 1, las2
                ll2 = k2 + l
                IF(numb1.EQ.0.AND.numb2.EQ.0)THEN
                ENDIF
C     IF(NUMB1.NE.0.AND. NUMB2.NE.0) GO TO 140
 
C     OTHER THAN K=2 AND NEGATIVE PARITY BANDS USE HIGHER THAN LAMBDA=2
C     AND LAMBDA=3 EXPANSIONs
C     IF(L.GT.1) GO TO 140
 
                M1 = -1
                M2 = 1
                lam = 2*l
                IF(NPO(nu).NE.NPO(nu1))lam = lam + 1
                IF(NPO(nu).EQ.NPO(nu1).OR.l.LE.1)THEN
 
                  J = lam*2
                  M = 0
                  CALL KLEGO
                  bkg = AKG
 
C     COUPLING OF NON-AXIAL BAND LEVELS
 
                  IF(ko1.EQ.ko2)THEN
 
C    AXIAL ROTATOR COUPLING, WITH NORMALIZATION ACCOUNTING
C    EFFECTIVE DEFORMATIONS OF OTHER BANDS
 
 
                    J1 = jo2
                    J2 = 2*lam
                    M1 = KO(nu1)
                    M2 = 0
                    M = ko1
                    J = jo1
                    CALL KLEGO
                    AKG = AKG*SQRT(J1 + 1.D0)
                  ELSE
                    IF(l.GT.1)GOTO 10
 
C     JOSES'S FORMULAS
                    IF(ko1.NE.4.OR.ko2.NE.0)THEN
                      IF(ko1.EQ.0.AND.ko2.EQ.4)THEN
 
 
 
                        J1 = jo2
                        J2 = 2*lam
                        J = jo1
                        M1 = 4
                        M2 = -4
                        M = 0
                        CALL KLEGO
                        AKG = AKG*SQRT(J1 + 1.D0)
 
                        GOTO 5
C     END JOSES'S FORMULAS
 
                        akgs = 0.D0
                        J1 = jo2
                        J2 = 2*lam
                        J = jo1
                        IF(ko1.EQ.ko2 - 4)THEN
                          M1 = ko2
                          M2 = -4
                          M = ko1
                          CALL KLEGO
                          akgs = AKG
                        ENDIF
                        IF(ko1.EQ.ko2 + 4)THEN
                          M1 = ko2
                          M2 = 4
                          M = ko1
                          CALL KLEGO
                          akgs = akgs + AKG
                        ENDIF
                        IF(4 - ko2.EQ.ko1)THEN
                          M1 = -ko2
                          M2 = 4
                          M = ko1
                          CALL KLEGO
                          akgs = akgs + AKG*( - 1)**(J1/2)
                        ENDIF
                        AKG = akgs*SQRT(J1 + 1.D0)/2.D0
                        GOTO 5
                      ENDIF
                    ENDIF
                    J1 = jo1
                    J2 = 2*lam
                    J = jo2
                    M1 = 4
                    M2 = -4
                    M = 0
                    CALL KLEGO
 
                    AKG = AKG*SQRT(J1 + 1.D0)*( - 1.D0)**(J1/2)
                  ENDIF
    5             JF = lam*2
                  CALL RACAH
                  CVNr(ll2) = ccc*bkg*AKG*W
                  IF(NPO(nu).NE. - 1.OR.NPO(nu1).NE. - 1)THEN
                    CVNr(ll2) = CVNr(ll2)*b12bgs**l
                  ENDIF
 
C     NECESSARY TO DIVIDE BY SIN(GAMMA)/SQRT(2.)!!! for actinides
 
                  IF(ko1.EQ.4.AND.ko2.EQ.4)CVNr(ll2) = CVNr(ll2)/0.1D0
 
                  GOTO 15
                ENDIF
   10           CVNr(ll2) = 0.D0
   15           J1 = jn1
                J2 = jn2
              ENDDO
 
 
              IF(jo1.EQ.jo2.AND.NCA(nu).NE.NCA(nu1))THEN
                IF(numb1.EQ.0.AND.numb2.EQ.0)THEN
 
                  cc = SQRT((J1 + 1)*(J2 + 1)/12.566371D0)
                  nu1 = NNJ1(kk)
                  jo2 = JO(nu1)
                  JD = jo2
                  jps = JSS - 1 - jo2 + J1 + J2 + ln2 - ln1
                  jps = jps - jps/4*4
                  IF(jps.NE.0)THEN
                    ccc = -cc
                  ELSE
                    ccc = cc
                  ENDIF
                  M1 = -1
                  M2 = 1
                  lam = 0
                  J = 0
                  M = 0
                  CALL KLEGO
                  bkg = AKG
                  J1 = jo2
                  J2 = 2*lam
                  M1 = KO(nu1)
                  M2 = 0
                  M = ko1
                  J = jo1
                  CALL KLEGO
                  AKG = AKG*SQRT(J1 + 1.D0)
                  JF = 0
                  CALL RACAH
                  CVNrpn(k2pn) = ccc*bkg*AKG*W
                  J1 = jn1
                  J2 = jn2
                ENDIF
              ENDIF
            ENDIF
C     WRITE(21,'(1X,8I8,1X,6(f8.4,1X))') NCLL,K,KK,KL, JO1,JO2,NCA(NU)
C     *,NCA(NU1)
C     *,CVNRPN(K2PN)
 
          ENDDO
        ENDDO
      ELSE
        icll = NCLl*las8
        ncla = NCLl*las2
        DO k = 1, NCLl
          k1 = (k - 1)*ncla
          ki1 = (k - 1)*las2
          k1pn = (k - 1)*NCLl
          kc1 = icll*(k - 1)
          kic1 = las8*(k - 1)
          nu1 = NNJ1(k)
          jo1 = JO(nu1)
          ln1 = LNJ1(k)*2
          jn1 = JNJ1(k)
          npi1 = ( - 1)**NNO(nu1)
          jo12 = jo1/2
          kmi1 = 1 - ( - 1)**jo12*npi1
          kma1 = 2*(jo12/2)
          nk1 = (kma1 - kmi1 + 2)/2
          DO kk = k, NCLl
            k2 = k1 + (kk - 1)*las2
            k2pn = k1pn + kk
            ki2 = ki1 + (kk - 1)*ncla
            kc2 = kc1 + (kk - 1)*las8
            kic2 = kic1 + (kk - 1)*icll
            nu2 = NNJ1(kk)
            jo2 = JO(nu2)
            ln2 = LNJ1(kk)*2
            jn2 = JNJ1(kk)
            jo22 = jo2/2
            npi2 = ( - 1)**NNO(nu2)
            kmi2 = 1 - ( - 1)**jo22*npi2
            kma2 = 2*(jo22/2)
            nk2 = (kma2 - kmi2 + 2)/2
            aa = ( - 1)**((JSS - jo2 - 1 + jn1 + jn2)/2 + (ln2 - ln1)/4)
     &           *SQRT((jn1 + 1.D0)*(jn2 + 1.D0)*(jo1 + 1.D0)
     &           /12.56663708D0)
            npcv = 1
            IF(ln2.LT.ln1)npcv = -1
            fok1 = FOK(NNO(nu1) + 1,NNO(nu2) + 1,1)
            fok2 = FOK(NNO(nu1) + 1,NNO(nu2) + 1,2)
            IF(BET3.NE.0.)fok1 = fok1*BET3
            IF(BET3.NE.0.)fok2 = fok2*BET3**2
            IF(BET3.EQ.0.)fok1 = fok1*AMUo
            IF(BET3.EQ.0.)fok2 = fok2*AMUo**2
            CG1 = TRIg(nu1,nu2,1)
            SG1 = TRIg(nu1,nu2,2)
            CG2 = TRIg(nu1,nu2,3)
            SG2 = TRIg(nu1,nu2,4)
            CSG11 = TRIg(nu1,nu2,5)
            CG3 = TRIg(nu1,nu2,6)
            SG3 = TRIg(nu1,nu2,7)
            CSG21 = TRIg(nu1,nu2,8)
            CSG12 = TRIg(nu1,nu2,9)
            CG4 = TRIg(nu1,nu2,10)
            SG4 = TRIg(nu1,nu2,11)
            CSG22 = TRIg(nu1,nu2,12)
            CSG31 = TRIg(nu1,nu2,13)
            CSG13 = TRIg(nu1,nu2,14)
            foij1 = FOV(nu1,nu2,1)*BET0
            IF(las2.GE.2)foij2 = FOV(nu1,nu2,2)*BET0**2
            IF(las2.GE.3)foij3 = FOV(nu1,nu2,3)*BET0**3
            DO l = 1, las2
              aaa = aa*FOV(nu1,nu2,l)*BET0**l
              ll2 = k2 + l
              li2 = ki2 + l
              llc2 = kc2 + l
              lic2 = kic2 + l
              su1 = 0.
              DO la = 1, 5
C     NEXT TWO CARDS JUST TO OVERCOME THE ERROR OR  MICROSOFT FPS COMPILER
C     v.1.00, YOU CAN DELETE THEM USING OTHER COMPILERS
                IF(la.GT.5.AND.MEPri.LT.99)PRINT 1010, la
 1010           FORMAT(I3)
                IF(l.NE.1.OR.la.EQ.2)THEN
                  IF(l.NE.2.OR.la.LT.4)THEN
                    IF(l.NE.3.OR.la.LT.5)GOTO 20
                  ENDIF
                ENDIF
                IF(MESha.GE.1)THEN
                  IF(l.NE.1.OR.la.EQ.3)GOTO 20
                ENDIF
                IF(MESho.EQ.0)CYCLE
                IF(l.EQ.1.AND.la.NE.2)CYCLE
                IF(la.GT.4)CYCLE
   20           lam = 4*(la - 1)
                su2 = 0.
                DO n = 1, la
                  alk = 0.
                  lk = nla(la) + n
                  IF(npi1.NE.npi2)THEN
                    lam = 4*(la - 1) + 2
                    IF(MESho.NE.0.AND.MEHao.NE.0)THEN
                      SELECT CASE(l)
                      CASE(2)
                        SELECT CASE(n)
                        CASE(2)
                          IF(MEHao.EQ.2)THEN
                            alk = ((B1O21(lk)*CG1 + B1O22(lk)*SG1)
     &                            *foij2 + B0O2(lk)*foij1)/foij2*fok1
                          ELSE
                            alk = ((B1O21(lk)*CG1 + B1O22(lk)*SG1)
     &                            *foij1 + B0O2(lk))/foij2*fok1
                          ENDIF
                        CASE(3)
                          IF(MEHao.EQ.2)THEN
                            alk = ((B1O21(lk)*CG1 + B1O22(lk)*SG1)
     &                            *foij2 + B0O2(lk)*foij1)/foij2*fok1
                          ELSE
                            alk = ((B1O21(lk)*CG1 + B1O22(lk)*SG1)
     &                            *foij1 + B0O2(lk))/foij2*fok1
                          ENDIF
                        CASE(4)
                          IF(MEHao.EQ.2)THEN
                            alk = ((B1O21(lk)*CG1 + B1O22(lk)*SG1)
     &                            *foij2 + B0O2(lk)*foij1)/foij2*fok1
                          ELSE
                            alk = ((B1O21(lk)*CG1 + B1O22(lk)*SG1)
     &                            *foij1 + B0O2(lk))/foij2*fok1
                          ENDIF
                        CASE DEFAULT
                          IF(MEHao.EQ.2)THEN
                            alk = ((B1O21(lk)*CG1 + B1O22(lk)*SG1)
     &                            *foij2 + B0O2(lk)*foij1)/foij2*fok1
                          ELSE
                            alk = ((B1O21(lk)*CG1 + B1O22(lk)*SG1)
     &                            *foij1 + B0O2(lk))/foij2*fok1
                          ENDIF
                        END SELECT
                      CASE(3)
                        SELECT CASE(n)
                        CASE(2)
                          IF(MEHao.EQ.2)THEN
                            alk = (B2O31(lk)*CG2 + B2O32(lk)
     &                            *SG2 + B2O33(lk)*CSG11*2.D0)*fok1
                          ELSE
                            alk = (B2O31(lk)*CG2 + B2O32(lk)
     &                            *SG2 + B2O33(lk)*CSG11*2.D0)
     &                            *fok1*foij2/foij3
                            GOTO 25
                          ENDIF
                        CASE(3)
                        CASE(4)
                          IF(MEHao.EQ.2)THEN
                            alk = (B2O31(lk)*CG2 + B2O32(lk)
     &                            *SG2 + B2O33(lk)*CSG11*2.D0)*fok1
                          ELSE
                            alk = (B2O31(lk)*CG2 + B2O32(lk)
     &                            *SG2 + B2O33(lk)*CSG11*2.D0)
     &                            *fok1*foij2/foij3
                          ENDIF
                          GOTO 25
                        CASE DEFAULT
                          IF(MEHao.EQ.2)THEN
                            alk = (B2O31(lk)*CG2 + B2O32(lk)
     &                            *SG2 + B2O33(lk)*CSG11*2.D0)*fok1
                          ELSE
                            alk = (B2O31(lk)*CG2 + B2O32(lk)
     &                            *SG2 + B2O33(lk)*CSG11*2.D0)
     &                            *fok1*foij2/foij3
                          ENDIF
                          GOTO 25
                        END SELECT
                        IF(MEHao.EQ.2)THEN
                          alk = (B2O31(lk)*CG2 + B2O32(lk)
     &                          *SG2 + B2O33(lk)*CSG11*2.D0)*fok1
                        ELSE
                          alk = (B2O31(lk)*CG2 + B2O32(lk)
     &                          *SG2 + B2O33(lk)*CSG11*2.D0)
     &                          *fok1*foij2/foij3
                        ENDIF
                      CASE(4)
                        GOTO 30
                      CASE DEFAULT
                        IF(n.EQ.2)THEN
                          IF(MEHao.EQ.2)THEN
                            alk = B0O1(lk)*fok1
                          ELSE
                            alk = B0O1(lk)*fok1/foij1
                          ENDIF
                        ELSEIF(MEHao.EQ.2)THEN
                          alk = B0O1(lk)*fok1
                        ELSE
                          alk = B0O1(lk)*fok1/foij1
                        ENDIF
                      END SELECT
                    ENDIF
                  ELSE
                    SELECT CASE(l)
                    CASE(2)
                      SELECT CASE(n)
                      CASE(2)
                        alk = ALK21(lk)*CSG11
                        IF(MESha.GT.1)THEN
                          alk = alk + B0Lk2(lk)
     &                          /foij2 + (B1Lk21(lk)*CG1 + B1Lk22(lk)
     &                          *SG1)*foij1/foij2
                          IF(MESho.NE.0)THEN
                            IF(MEHao.EQ.2)THEN
                              alk = alk + BO2(lk)*fok2
                            ELSE
                              alk = alk + BO2(lk)*fok2/foij2
                            ENDIF
                          ENDIF
                        ENDIF
                      CASE(3)
                        alk = ALK21(lk)*SG2
                        IF(MESha.GT.1)THEN
                          alk = alk + B0Lk2(lk)
     &                          /foij2 + (B1Lk21(lk)*CG1 + B1Lk22(lk)
     &                          *SG1)*foij1/foij2
                          IF(MESho.NE.0)THEN
                            IF(MEHao.EQ.2)THEN
                              alk = alk + BO2(lk)*fok2
                            ELSE
                              alk = alk + BO2(lk)*fok2/foij2
                            ENDIF
                          ENDIF
                        ENDIF
                      CASE DEFAULT
                        alk = ALK21(lk)*CG2 + ALK22(lk)*SG2
                        IF(MESha.GT.1)THEN
                          alk = alk + B0Lk2(lk)
     &                          /foij2 + (B1Lk21(lk)*CG1 + B1Lk22(lk)
     &                          *SG1)*foij1/foij2
                          IF(MESho.NE.0)THEN
                            IF(MEHao.EQ.2)THEN
                              alk = alk + BO2(lk)*fok2
                            ELSE
                              alk = alk + BO2(lk)*fok2/foij2
                            ENDIF
                          ENDIF
                        ENDIF
                      END SELECT
                      GOTO 25
                    CASE(3)
                      SELECT CASE(n)
                      CASE(2)
                        alk = ALK31(lk)*CSG21 + ALK32(lk)*SG3
                        IF(MESha.GT.1)THEN
                          alk = alk + (B2Lk31(lk)*CG2 + B2Lk32(lk)*SG2 + 
     &                          B2Lk33(lk)*CSG11)*foij2/foij3
                        ENDIF
                        GOTO 25
                      CASE(3)
                        alk = ALK31(lk)*CSG12
                        IF(MESha.GT.1)THEN
                          alk = alk + (B2Lk31(lk)*CG2 + B2Lk32(lk)*SG2 + 
     &                          B2Lk33(lk)*CSG11)*foij2/foij3
                        ENDIF
                        GOTO 25
                      CASE(4)
                        alk = ALK31(lk)*SG3
                        IF(MESha.LE.1)GOTO 25
                        alk = alk + 
     &                        (B2Lk31(lk)*CG2 + B2Lk32(lk)*SG2 + B2Lk33
     &                        (lk)*CSG11)*foij2/foij3
                      CASE DEFAULT
                        alk = ALK31(lk)*CG3 + ALK32(lk)*CSG12
                        IF(MESha.GT.1)THEN
                          alk = alk + (B2Lk31(lk)*CG2 + B2Lk32(lk)*SG2 + 
     &                          B2Lk33(lk)*CSG11)*foij2/foij3
                        ENDIF
                        GOTO 25
                      END SELECT
                    CASE(4)
                    CASE DEFAULT
                      IF(n.EQ.2)THEN
                        alk = ALK11(lk)*SG1
                        IF(MESha.GT.1)THEN
                          alk = alk + B0Lk1(lk)/foij1
                        ENDIF
                      ELSE
                        alk = ALK11(lk)*CG1
                        IF(MESha.GT.1)THEN
                          alk = alk + B0Lk1(lk)/foij1
                        ENDIF
                      ENDIF
                      GOTO 25
                    END SELECT
                    SELECT CASE(n)
                    CASE(2)
                      alk = ALK41(lk)*CSG31 + ALK42(lk)*CSG13
                    CASE(3)
                      alk = ALK41(lk)*CSG22 + ALK42(lk)*SG4
                    CASE(4)
                      alk = ALK41(lk)*CSG13
                    CASE(5)
                      alk = ALK41(lk)*SG4
                    CASE DEFAULT
                      alk = ALK41(lk)*CG4 + ALK42(lk)*CSG22 + ALK43(lk)
     &                      *SG4
                    END SELECT
                  ENDIF
   25             IF(alk.NE.0.D0)THEN
                    su = 0.
                    J1 = jn1
                    J2 = jn2
                    J = lam
                    M1 = -1
                    M2 = 1
                    M = 0
                    CALL KLEGO
                    ab = AKG
                    JA = jn1
                    JB = jo1
                    JE = jn2
                    JD = jo2
                    JC = JSS
                    JF = lam
                    CALL RACAH
                    ab = ab*W
                    DO lk1 = 1, nk1
                      ks1 = 2*(kmi1 + 2*(lk1 - 1))
                      ait1 = AIT(nu1,lk1)
                      IF(ks1.EQ.0)ait1 = ait1/sq2
                      DO lk2 = 1, nk2
                        ks2 = 2*(kmi2 + 2*(lk2 - 1))
                        ait2 = AIT(nu2,lk2)
                        IF(ks2.EQ.0)ait2 = ait2/sq2
                        ka = 4*(n - 1)
                        J1 = jo1
                        J2 = lam
                        J = jo2
                        M1 = ks1
                        M2 = ka
                        M = ks2
                        CALL KLEGO
                        ck1 = AKG
                        M1 = -ks1
                        CALL KLEGO
                        clk = ck1 + ( - 1)**jo12*AKG*npi1
                        IF(ka.NE.0)THEN
                          M1 = ks1
                          M2 = -ka
                          CALL KLEGO
                          clk = clk + AKG
                        ENDIF
                        su = su + clk*ait1*ait2
                      ENDDO
                    ENDDO
                    su2 = su2 + su*alk
                  ENDIF
                ENDDO
                IF(l.EQ.2)su2 = su2*1.4104730D0/SQRT(lam + 1.D0)
                IF(l.EQ.3)su2 = su2*0.8897014D0/SQRT(lam + 1.D0)
                IF(l.EQ.4)su2 = su2*0.5612105D0/SQRT(lam + 1.D0)
                su1 = su1 + su2*ab
                IF(l.LE.2)THEN
                  lllc2 = llc2 + (lam/2)*lasc
                  llic2 = lic2 + (lam/2)*lasc
                  CVNc(lllc2) = su2*ab*aaa
C     CVNC(LLLC2)=0.
                  IF(kk.NE.k)CVNc(llic2) = CVNc(lllc2)
                  IF(npi1.NE.npi2)CVNc(lllc2) = CVNc(lllc2)*npcv
                  IF(npi1.NE.npi2)CVNc(llic2) = -CVNc(lllc2)
                ENDIF
C      if(lam/2.eq.0) write (21,9999)jo1,jo2,l,kc2,k,kk,llc2,cvnc(lllc2)
C9999 format(7i6,e12.4)
   30         ENDDO
              CVNr(ll2) = su1*aaa
              IF(kk.NE.k)CVNr(li2) = CVNr(ll2)
              IF(npi1.NE.npi2)CVNr(ll2) = CVNr(ll2)*npcv
              IF(npi1.NE.npi2)CVNr(li2) = -CVNr(ll2)
 
 
 
            ENDDO
          ENDDO
        ENDDO
 
 
        DO k = 1, NCLl
          k1pn = (k - 1)*NCLl
          nu = NNJ1(k)
          jo1 = JO(nu)
          ln1 = LNJ1(k)*2
          npi1 = ( - 1)**NNO(nu)
          DO kk = 1, NCLl
            k2pn = k1pn + kk
            nu1 = NNJ1(kk)
            jo2 = JO(nu1)
            ln2 = LNJ1(kk)*2
            npi2 = ( - 1)**NNO(nu1)
            CVNrpn(k2pn) = 0.D0
 
            IF(jo1.EQ.jo2.AND.NCA(nu).NE.NCA(nu1).AND.NTU(nu)
     &         .EQ.NTU(nu1).AND.NNB(nu).EQ.NNB(nu1).AND.NNG(nu)
     &         .EQ.NNG(nu1).AND.NNO(nu).EQ.NNO(nu1))THEN
              CVNrpn(k2pn) = 1.D0
              npcv = 1
            ENDIF
C     IF(LN2-LN1.LT.0) NPCV=-1
C     IF(NPI1.NE.NPI2) CVNRPN(K2PN)=CVNRPN(K2PN)*NPCV
C     IF(NPI1.NE.NPI2)CVNRPN(K2PN)=-CVNRPN(K2PN)
 
 
          ENDDO
 
 
 
 
        ENDDO
      ENDIF
      RETURN
      END SUBROUTINE KNDIT
 
!---------------------------------------------------------------------------
C     *****************************************************************
      SUBROUTINE PREQU
C     *****************************************************************
      IMPLICIT DOUBLE PRECISION(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8, DIMENSION(31,16) :: AIT
      REAL*8 :: AMB0, AMG0, AMUo, ANG1, ANG2, ANU1, ANU2, BB32, BET0, 
     &          BET3, BET4, CD1, CD2, CG1, CG2, CG3, CG4, CSG11, CSG12, 
     &          CSG13, CSG21, CSG22, CSG31, DG1, DG2, DPAr, EN, ETO, 
     &          FOLac, FOLag, FOLar, FOLas, GAM0, GAMde, GSHape, HW, 
     &          HWO, P1, P2, SG1, SG2, SG3, SG4, X1, X2
      REAL*8, DIMENSION(20) :: ANB, ANO, EGB, EL, EP0, EP1, EP12, EP2, 
     &                         EPB, EPG, ES, GIT, PT, XI
      REAL*8, DIMENSION(20,2) :: ANG, CD
      REAL*8, DIMENSION(10) :: BET
      REAL*8, DIMENSION(2,8,8) :: FOG
      REAL*8, DIMENSION(6,6,2) :: FOK
      REAL*8, DIMENSION(20,20,4) :: FOV
      INTEGER, DIMENSION(20) :: JO, JU, KO, NCA, NNB, NNG, NNO, NPI, 
     &                          NPO, NTU
      INTEGER :: LAS, MEApp, MECha, MECul, MEDis, MEHam, MEHao, MEJob, 
     &           MEPot, MEPri, MERel, MERip, MERrr, MERzz, MESha, MESho, 
     &           MESol, MEVol, NMAx, NNT, NNTg, NPD, NUR
      REAL*8, DIMENSION(20,20,14) :: TRIg
      COMMON /AA    / ANO
      COMMON /ENA   / EN, EL, BET, NUR, NMAx, NPD, LAS
      COMMON /FOK12 / FOK
      COMMON /MENU  / MEJob, MEPot, MEHam, MECha, MEPri, MESol, MESha, 
     &                MESho, MEHao, MEApp, MEVol, MERel, MECul, MERzz, 
     &                MERrr, MEDis, MERip
      COMMON /OVKN  / FOV, CG1, SG1, CG2, SG2, CSG11, CG4, SG4, CSG22, 
     &                CSG31, CSG13, CG3, SG3, CSG21, CSG12
      COMMON /OVLB  / X1, P1, ANU1, X2, P2, ANU2, FOLar, NNT
      COMMON /OVLG  / ANG1, ANG2, FOLag, FOLac, FOLas, CD1, CD2, DG1, 
     &                DG2, NNTg
      COMMON /QNB   / JO, NPO, KO, NCA
      COMMON /SHAMO / BET3, ETO, AMUo, HWO, BB32, DPAr
      COMMON /SHEM1 / HW, AMG0, AMB0, GAM0, BET0, BET4, GAMde, GSHape
      COMMON /SHEM2 / GIT, EP0, EP1, EP2, EP12, ANG, EPB, EPG, EGB, AIT, 
     &                PT, FOG, XI, ANB, CD
      COMMON /SHEMM / ES, JU, NTU, NNB, NNG, NNO, NPI
      COMMON /TRIGF / TRIg
C
C Local variables
C
      REAL*8 :: co, ebm, si
      INTEGER :: i, io1, io2, ju1, ju2, kg1, kg2, la, nom, nom1
C
C*** End of declarations rewritten by SPAG
C
      si = SIN(GAM0 + GSHape)
      co = COS(GAM0 + GSHape)
      CG1 = co
      SG1 = si
      SG2 = si*si
      CG2 = co*co
      CSG11 = si*co
      CG3 = CG1*CG2
      SG3 = SG1*SG2
      CSG21 = CG2*SG1
      CSG12 = SG2*CG1
      CG4 = CG2*CG2
      SG4 = SG2*SG2
      CSG22 = CG2*SG2
      CSG31 = CG3*SG1
      CSG13 = SG3*CG1
      FOLar = 1.
      DO i = 1, NUR
        JU(i) = JO(i)/2
      ENDDO
      CALL SHEM
      IF(MESho.NE.0)THEN
        nom = 0
        DO i = 1, NUR
          IF(nom.LT.NNO(i))nom = NNO(i)
        ENDDO
        nom1 = nom + 1
        la = 1
        IF(LAS.GE.2)la = 2
        DO io1 = 1, nom1
          ANU1 = ANO(io1)
          DO io2 = io1, nom1
            ANU2 = ANO(io2)
            DO NNT = 1, la
              FOLar = BET3**NNT
              IF(MEHao.NE.0)THEN
                IF(MEHao.GE.2)THEN
                  FOLar = 0.
                  ebm = EXP( - (BET3/AMUo)**2)
                  IF(NNT.EQ.1.AND.io1.NE.io2)
     &               FOLar = BET3/SQRT(1.D0 - ebm**2)
                  IF(NNT.EQ.2.AND.io1.EQ.io2)FOLar = AMUo**2/2.D0 + 
     &               BET3**2/(1. - ( - 1)**io1*ebm)
                  IF(MEHao.GE.2)GOTO 5
                ENDIF
                X1 = -BET3/AMUo
                X2 = X1
                P1 = BET3
                P2 = BET3
                IF(BET3.EQ.0.)CALL OVLAO
                IF(BET3.NE.0.)CALL OVLAB
              ENDIF
    5         IF(BET3.NE.0.)FOLar = FOLar/BET3**NNT
              IF(BET3.EQ.0.)FOLar = FOLar/AMUo**NNT
              IF(NNT.EQ.1.AND.io1.EQ.io2)FOLar = 0.D0
              IF(NNT.EQ.2.AND.io1.NE.io2)FOLar = 0.D0
              FOK(io1,io2,NNT) = FOLar
              IF(io1.NE.io2)FOK(io2,io1,NNT) = FOK(io1,io2,NNT)
              IF(MEPri.LT.99)THEN
                PRINT 1010, io1, io2, NNT, FOLar, ANU1, ANU2
                WRITE(21,1020)io1, io2, NNT, FOLar, ANU1, ANU2
              ENDIF
 1010         FORMAT(3I4,3E20.7)
 1020         FORMAT(1X,'IO1=',I3,1X,'IO2=',I3,1X,'NNT=',I3,2X,'FOLAR=',
     &               D15.7,1X,'ANU1=',D15.7,2X,'ANU2=',D15.7)
            ENDDO
          ENDDO
        ENDDO
      ENDIF
      DO ju1 = 1, NUR
        kg1 = (3 - ( - 1)**NNO(ju1))/2
        DG1 = 0.
        IF(kg1.EQ.2)DG1 = GAMde
        ANG1 = ANG(NNG(ju1) + 1,kg1)
        CD1 = CD(NNG(ju1) + 1,kg1)
        X1 = XI(ju1)
        P1 = PT(ju1)
        ANU1 = ANB(ju1)
        DO ju2 = ju1, NUR
          kg2 = (3 - ( - 1)**NNO(ju2))/2
          DG2 = 0.
          IF(kg2.EQ.2)DG2 = GAMde
          ANG2 = ANG(NNG(ju2) + 1,kg2)
          CD2 = CD(NNG(ju2) + 1,kg2)
          X2 = XI(ju2)
          P2 = PT(ju2)
          ANU2 = ANB(ju2)
          IF(MEHam.GT.4)CALL OVLAGE
          TRIg(ju1,ju2,1) = CG1
          IF(ju1.NE.ju2)TRIg(ju2,ju1,1) = CG1
          TRIg(ju1,ju2,2) = SG1
          IF(ju1.NE.ju2)TRIg(ju2,ju1,2) = SG1
          TRIg(ju1,ju2,3) = CG2
          IF(ju1.NE.ju2)TRIg(ju2,ju1,3) = CG2
          TRIg(ju1,ju2,4) = SG2
          IF(ju1.NE.ju2)TRIg(ju2,ju1,4) = SG2
          TRIg(ju1,ju2,5) = CSG11
          IF(ju1.NE.ju2)TRIg(ju2,ju1,5) = CSG11
          TRIg(ju1,ju2,6) = CG3
          IF(ju1.NE.ju2)TRIg(ju2,ju1,6) = CG3
          TRIg(ju1,ju2,7) = SG3
          IF(ju1.NE.ju2)TRIg(ju2,ju1,7) = SG3
          TRIg(ju1,ju2,8) = CSG21
          IF(ju1.NE.ju2)TRIg(ju2,ju1,8) = CSG21
          TRIg(ju1,ju2,9) = CSG12
          IF(ju1.NE.ju2)TRIg(ju2,ju1,9) = CSG12
          TRIg(ju1,ju2,10) = CG4
          IF(ju1.NE.ju2)TRIg(ju2,ju1,10) = CG4
          TRIg(ju1,ju2,11) = SG4
          IF(ju1.NE.ju2)TRIg(ju2,ju1,11) = SG4
          TRIg(ju1,ju2,12) = CSG22
          IF(ju1.NE.ju2)TRIg(ju2,ju1,12) = CSG22
          TRIg(ju1,ju2,13) = CSG31
          IF(ju1.NE.ju2)TRIg(ju2,ju1,13) = CSG31
          TRIg(ju1,ju2,14) = CSG13
          IF(ju1.NE.ju2)TRIg(ju2,ju1,14) = CSG13
          FOLar = 1.
          DO NNT = 1, LAS
            IF(MEHam.NE.4)CALL OVLAB
            FOV(ju1,ju2,NNT) = FOLar
            IF(ju1.NE.ju2)FOV(ju2,ju1,NNT) = FOV(ju1,ju2,NNT)
            IF(MEPri.LT.99)THEN
              PRINT 1030, ju1, ju2, NNT, FOV(ju1,ju2,NNT), ANU1, ANU2
              WRITE(21,1040)ju1, ju2, NNT, FOV(ju1,ju2,NNT), ANU1, ANU2
            ENDIF
 1030       FORMAT(3I5,3E17.7)
 1040       FORMAT(1X,'JU1=',I2,1X,'JU2=',I2,1X,'NNT=',I2,1X,
     &             'FOV(JU1,JU2,NNT)=',D15.7,2X,'ANU1=',D15.7,2X,
     &             'ANU2=',D15.7)
          ENDDO
        ENDDO
      ENDDO
      RETURN
      END SUBROUTINE PREQU
 
!---------------------------------------------------------------------------
C     *******************************************************
C     END of knditd
C     *******************************************************
C     *******************************************************
C     START of shemsofd
C     *******************************************************
C     *****************************************************************
      SUBROUTINE FUDNU
C     *****************************************************************
      IMPLICIT DOUBLE PRECISION(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: ANU, CDV, DN, DNV, Y
      COMMON /FUDN  / Y, DN, ANU, DNV, CDV
C
C Local variables
C
      REAL*8 :: an2, c2n2, cpn, cpn2, df, dv, ex4, fm, gn2p1, gn2p12, 
     &          gnp1, pi, pin, pin2, spn, spn2, sq2, sqp2, sqpin, vm, 
     &          yn, yn1, yy, z2, z2m
      REAL*8 :: GAMMA
      INTEGER :: m
C
C*** End of declarations rewritten by SPAG
C
      sq2 = SQRT(2.D0)
      yy = Y
      z2 = Y**2
      an2 = ANU/2.D0
      ex4 = EXP( - z2/4.D0)
      gnp1 = GAMMA(ANU + 1.D0)
      m = 1
      pi = 4.D0*ATAN(1.D0)
      pin = pi*ANU
      pin2 = pi*an2
      sqpin = 0.564189583548D0
      IF(ABS(yy).GT.5.AND.ABS(yy).GT.ANU)THEN
        yn = z2**an2
        yn1 = z2**(an2 + 0.5D0)
        cpn = COS(pin)
        spn = SIN(pin)
        sqp2 = sq2*sqpin
        fm = 1.D0
        df = fm
    5   z2m = z2*(2*m)
        vm = (2*m + ANU)*(2*m - 1 + ANU)/z2m
        IF(ABS(vm).LE.1.D0)THEN
          fm = fm*vm
          df = df + fm
          IF(ABS(fm).GE.1.D-6)THEN
            m = m + 1
            GOTO 5
          ENDIF
        ENDIF
        m = 1
        fm = 1
        dv = fm
   10   z2m = z2*(2*m)
        vm = -(ANU - 2*m + 1)*(ANU - 2*m + 2)/z2m
        IF(ABS(vm).LE.1.D0)THEN
          fm = fm*vm
          dv = dv + fm
          IF(ABS(fm).GE.1.D-6)THEN
            m = m + 1
            GOTO 10
          ENDIF
        ENDIF
        IF(Y.LE.0)THEN
          DN = -sqp2*spn*gnp1/ex4/yn1*df + cpn*ex4*yn*dv
          DNV = -spn*ex4/gnp1*yn*dv - cpn*sqp2/ex4/yn1*df
        ELSE
          DN = ex4*yn*dv
          DNV = sqp2/ex4/yn1*df
        ENDIF
      ELSE
        cpn2 = COS(pin2)
        spn2 = SIN(pin2)
        gn2p12 = GAMMA(an2 + 0.5D0)
        gn2p1 = GAMMA(an2 + 1.D0)
        c2n2 = (2.D0)**an2
        fm = 1.D0
        vm = 1.D0
        df = fm
        dv = vm
   15   z2m = z2/m
        fm = fm*z2m/(2*m - 1)*(m - 1 - an2)
        vm = vm*z2m/(2*m + 1)*(m - 0.5D0 - an2)
        df = df + fm
        dv = dv + vm
        IF((ABS(fm) + ABS(vm))/(ABS(df) + ABS(dv)).LT.1.D-6)THEN
          DN = c2n2*sqpin*ex4*(cpn2*gn2p12*df + sq2*Y*spn2*gn2p1*dv)
          DNV = ex4/c2n2*( - spn2/gn2p1*df + sq2*Y*cpn2/gn2p12*dv)
        ELSE
          m = m + 1
          GOTO 15
        ENDIF
      ENDIF
      RETURN
      END SUBROUTINE FUDNU
 
!---------------------------------------------------------------------------
C     *****************************************************************
      SUBROUTINE MATAM
C     *****************************************************************
      IMPLICIT DOUBLE PRECISION(a - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8, DIMENSION(16,16) :: AM, TM
      REAL*8 :: AMO, BB42, BMO, CMO, DELg, GAM, GAMg
      REAL*8, DIMENSION(31,16) :: EIN
      INTEGER :: IS, ITAu, KMI, NK, NO
      COMMON /INRM  / AMO, BMO, CMO, BB42, GAMg, DELg
      COMMON /MAT   / GAM, IS, NO
      COMMON /VEC   / TM, AM, EIN, NK, KMI, ITAu
C
C Local variables
C
      REAL*8 :: a, ab16, ab16ik, ab8, b, c, c4
      INTEGER :: i, j, kma, ks
C
C*** End of declarations rewritten by SPAG
C
      KMI = 1 - ( - 1)**(IS + NO)
      kma = 2*(IS/2)
      NK = (kma - KMI + 2)/2
      CALL INERMO
      a = AMO
      b = BMO
      c = CMO
      c4 = c/4.D0
      ab8 = (a + b)/8.
      ab16 = (a - b)/16.
      DO j = 1, NK
        DO i = 1, j
          ks = KMI + 2*(i - 1)
          IF(i.EQ.j)THEN
            AM(i,j) = ab8*(IS*(IS + 1.D0) - ks*ks) + c4*ks*ks
          ELSEIF(i + 1.EQ.j)THEN
            ab16ik = ab16*SQRT((IS + ks + 2.D0)*(IS - ks - 1.D0)
     &               *(IS + ks + 1.D0)*(IS - ks))
            IF(ks.EQ.0)THEN
              AM(i,j) = ab16ik*(1 + ( - 1)**(IS + NO))/SQRT(2.D0)
            ELSE
              AM(i,j) = ab16ik
            ENDIF
          ELSE
            AM(i,j) = 0.D0
          ENDIF
        ENDDO
      ENDDO
      CALL VECNO
      RETURN
      END SUBROUTINE MATAM
 
!---------------------------------------------------------------------------
C     *****************************************************************
      SUBROUTINE VECNO
C     *****************************************************************
      IMPLICIT DOUBLE PRECISION(a - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8, DIMENSION(16,16) :: AM, AM1, TM, TM1
      REAL*8, DIMENSION(31,16) :: EIN
      REAL*8 :: GAM
      INTEGER :: IS, ITAu, KMI, NK, NO
      COMMON /MAT   / GAM, IS, NO
      COMMON /VEC   / TM, AM, EIN, NK, KMI, ITAu
      COMMON /VEC1  / TM1, AM1
C
C Local variables
C
      REAL*8 :: a, aco, al, amii, amij, amjj, asi, b, e, e2a, em, t1
      INTEGER :: i, i1, im, j, j1, jm, k, l
C
C*** End of declarations rewritten by SPAG
C
      DO i = 1, NK
        DO j = 1, NK
          IF(i.EQ.j)THEN
            TM(i,j) = 1.D0
            TM1(i,j) = 1.D0
          ELSE
            TM(i,j) = 0.D0
            TM1(i,j) = 0.D0
          ENDIF
          IF(i.LE.j)THEN
            AM1(i,j) = AM(i,j)
          ENDIF
        ENDDO
      ENDDO
      IF(NK.EQ.1)GOTO 20
   10 im = 1
      jm = 2
      a = ABS(AM(1,2))
      IF(NK.NE.2)THEN
        DO j = 3, NK
          j1 = j - 1
          DO i = 1, j1
            b = ABS(AM(i,j))
            IF(a.LT.b)THEN
              a = b
              im = i
              jm = j
            ENDIF
          ENDDO
        ENDDO
      ENDIF
      amii = AM(im,im)
      amjj = AM(jm,jm)
      amij = AM(im,jm)
      al = ATAN(2.D0*amij/(amii - amjj))/2.D0
      aco = COS(al)
      asi = SIN(al)
      DO l = 1, NK
        DO k = 1, l
          IF(k.EQ.im)THEN
            IF(l.EQ.im)THEN
              AM1(k,l) = aco*(aco*amii + asi*amij)
     &                   + asi*(aco*amij + asi*amjj)
            ELSEIF(l.EQ.jm)THEN
              AM1(k,l) = 0.D0
            ELSE
              AM1(k,l) = aco*AM(im,l) + asi*AM(jm,l)
            ENDIF
          ELSEIF(k.EQ.jm)THEN
            IF(l.EQ.jm)THEN
              AM1(k,l) = -asi*( - asi*amii + aco*amij)
     &                   + aco*( - asi*amij + aco*amjj)
            ELSE
              AM1(k,l) = -asi*AM(im,l) + aco*AM(jm,l)
            ENDIF
          ELSEIF(l.EQ.im)THEN
            AM1(k,l) = aco*AM(k,im) + asi*AM(k,jm)
          ELSEIF(l.EQ.jm)THEN
            AM1(k,l) = -asi*AM(k,im) + aco*AM(k,jm)
          ENDIF
        ENDDO
      ENDDO
      DO k = 1, NK
        DO l = 1, NK
          IF(k.EQ.im)THEN
            TM1(k,l) = aco*TM(im,l) - asi*TM(jm,l)
          ELSEIF(k.EQ.jm)THEN
            TM1(k,l) = asi*TM(im,l) + aco*TM(jm,l)
          ENDIF
        ENDDO
      ENDDO
      e2a = 0.D0
      DO k = 1, NK
        DO l = 1, NK
          TM(k,l) = TM1(k,l)
          IF(k.LE.l)THEN
            IF(k.LT.l)e2a = e2a + AM1(k,l)**2
            AM(k,l) = AM1(k,l)
          ENDIF
        ENDDO
      ENDDO
      IF(e2a.GE.1.D-8)GOTO 10
   20 IF(IS.NE.0)THEN
        DO i = 1, NK
          i1 = i + 1
          em = AM(i,i)
          im = i
          IF(i1.LE.NK)THEN
            DO j = i1, NK
              e = AM(j,j)
              IF(em.GT.e)THEN
                em = e
                im = j
              ENDIF
            ENDDO
          ENDIF
          DO j = 1, NK
            t1 = TM(j,im)
            TM(j,im) = TM(j,i)
            TM(j,i) = t1
          ENDDO
          EIN(IS + 1,i) = em
          AM(im,im) = AM(i,i)
          AM(i,i) = em
        ENDDO
      ENDIF
      IF(IS.EQ.0)EIN(1,1) = 0.D0
      IF(IS.EQ.0)TM(1,1) = 1.D0
      RETURN
      END SUBROUTINE VECNO
 
!---------------------------------------------------------------------------
 
C     *****************************************************************
      BLOCK DATA 
C     *****************************************************************
      IMPLICIT DOUBLE PRECISION(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8, DIMENSION(4) :: AN
      REAL*8, DIMENSION(6) :: B0, B1
      REAL*8, DIMENSION(5) :: B2, B3, C0, C1, C2, C3
      COMMON /SENU  / B0, B1, C0, C1, AN, B2, C2, B3, C3
C
C*** End of declarations rewritten by SPAG
C
      DATA B0/ - 22.11505D0, 6.64844D0, .769968D0, -.622406D0, 
     &     .107521D0, -.007364D0/, B1/ - 17.87240D0, 4.66396D0, 
     &     2.94376D0, -2.03488D0, .51202D0, -.051336D0/, 
     &     C0/ - 2.32285D0, 2.65365D0, -1.077629D0, .173691D0, 
     &     -.007545D0/, C1/ - .630875D0, 1.198105D0, -.727493D0, 
     &     .164266D0, -.0091289D0/, AN/ - 2.D0, -2.2D0, -2.4D0, -2.6D0/
      DATA B2/ - 15.007288D0, 5.405502D0, 1.117805D0, -.831844D0, 
     &     .110628D0/, B3/ - 12.206497D0, 4.48481D0, 1.679626D0, 
     &     -1.226475D0, .190465D0/, C2/.82866D0, -.587769D0, -.005974D0, 
     &     .0581198D0, -.00321186D0/, C3/1.191310D0, -1.284630D0, 
     &     .368034D0, -.00188923D0, .000426352D0/
      END BLOCK DATA
 
!---------------------------------------------------------------------------
C     ***************************************************************
      SUBROUTINE EIT12
C     *****************************************************************
      IMPLICIT DOUBLE PRECISION(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8, DIMENSION(31,16) :: AIT, EIN
      REAL*8, DIMENSION(16,16) :: AM, TM
      REAL*8 :: AMB0, AMG0, AMO, ANG1, ANG2, ANU, BB42, BET0, BET4, BMO, 
     &          CD1, CD2, CDV, CMO, DELg, DET, DG1, DG2, DN, DNV, EPIt1, 
     &          EPIt12, EPIt2, FOLac, FOLag, FOLas, GAM, GAM0, GAMde, 
     &          GAMg, GSHape, HW, XIT, XIT1, Y
      REAL*8, DIMENSION(20) :: ANB, EGB, EP0, EP1, EP12, EP2, EPB, EPG, 
     &                         GIT, PT, XI
      REAL*8, DIMENSION(20,2) :: ANG, CD
      REAL*8, DIMENSION(2,8,8) :: FOG
      INTEGER :: IS, ITAu, KMI, MEApp, MECha, MECul, MEDis, MEHam, 
     &           MEHao, MEJob, MEPot, MEPri, MERel, MERip, MERrr, MERzz, 
     &           MESha, MESho, MESol, MEVol, NK, NNTg, NO, NROot
      COMMON /EIT   / EPIt1, EPIt2, EPIt12
      COMMON /FUDN  / Y, DN, ANU, DNV, CDV
      COMMON /INRM  / AMO, BMO, CMO, BB42, GAMg, DELg
      COMMON /MAT   / GAM, IS, NO
      COMMON /MENU  / MEJob, MEPot, MEHam, MECha, MEPri, MESol, MESha, 
     &                MESho, MEHao, MEApp, MEVol, MERel, MECul, MERzz, 
     &                MERrr, MEDis, MERip
      COMMON /OVLG  / ANG1, ANG2, FOLag, FOLac, FOLas, CD1, CD2, DG1, 
     &                DG2, NNTg
      COMMON /SHEM1 / HW, AMG0, AMB0, GAM0, BET0, BET4, GAMde, GSHape
      COMMON /SHEM2 / GIT, EP0, EP1, EP2, EP12, ANG, EPB, EPG, EGB, AIT, 
     &                PT, FOG, XI, ANB, CD
      COMMON /VEC   / TM, AM, EIN, NK, KMI, ITAu
      COMMON /XI12  / XIT, XIT1, DET, NROot
C
C Local variables
C
      REAL*8 :: a1d, a2d, ab1dm, ab1dp, ab2dm, ab2dp, ak, ak1, aka, 
     &          aka1, amom, amoo, amop, b1d, b2d, bmom, bmoo, bmop, 
     &          c1d, c2d, cmom, cmoo, cmop, d, dd, degam, dele, 
     &          su1m, su2m, sum1, sum2, sum3
      INTEGER :: i, inga, ingm, ingp, ita, ka, nga, ngd, ngu, nr1
C
C*** End of declarations rewritten by SPAG
C
      IF(MEHam.EQ.5.OR.MEHam.EQ.7)THEN
        EPIt1 = 0.D0
        EPIt2 = 0.D0
        EPIt12 = 0.D0
      ELSE
        amoo = AMO
        bmoo = BMO
        cmoo = CMO
        degam = 0.001D0*GAM
        GAM = GAM0 + degam
        CALL INERMO
        amop = AMO
        bmop = BMO
        cmop = CMO
        GAM = GAM0 - degam
        CALL INERMO
        amom = AMO
        bmom = BMO
        cmom = CMO
        GAM = GAM0
        a1d = (amop - amom)/degam/2.D0
        b1d = (bmop - bmom)/degam/2.D0
        c1d = (cmop - cmom)/degam/2.D0
        a2d = (amop + amom - 2.D0*amoo)/degam/degam
        b2d = (bmop + bmom - 2.D0*bmoo)/degam/degam
        c2d = (cmop + cmom - 2.D0*cmoo)/degam/degam
        ab1dm = a1d - b1d
        ab1dp = a1d + b1d
        ab2dm = a2d - b2d
        ab2dp = a2d + b2d
        ngd = 2
        sum3 = 0.D0
        ngu = NROot
        nr1 = NROot + 1
        ingp = ngu + ngd + 1
        IF(ingp.GT.4)ingp = 4
        ingm = ngu - ngd + 1
        IF(ingm.LT.1)ingm = 1
        ANG1 = ANG(NROot + 1,NO)
        CD1 = CD(NROot + 1,NO)
        DO inga = ingm, ingp
          nga = inga - 1
          ANG2 = ANG(inga,NO)
          CD2 = CD(inga,NO)
          DO ita = 1, NK
            sum1 = 0.D0
            sum2 = 0.D0
            DO i = 1, NK
              ak = TM(i,ITAu)
              aka = TM(i,ita)
              ka = KMI + (i - 1)*2
              sum1 = sum1 + ak*aka*ka*ka
              d = 1.D0
              dd = 0.D0
              IF(ka.EQ.0)dd = 1.D0
              IF(ka.EQ.0)d = SQRT(2.D0)
              IF(i.NE.NK)THEN
                ak1 = TM(i + 1,ITAu)
                aka1 = TM(i + 1,ita)
                sum2 = sum2 + (ak1*aka + ak*aka1)*(1.D0 + ( - 1)**IS*dd)
     &                 /d*SQRT((IS + ka + 2.D0)*(IS + ka + 1.D0)
     &                 *(IS - ka - 1.D0)*(IS - ka))
              ENDIF
            ENDDO
            IF(ita.EQ.ITAu.AND.ngu.EQ.nga)THEN
              EPIt1 = ab1dp/8.D0*IS*(IS + 1) + (0.25D0*c1d - ab1dp/8.D0)
     &                *sum1 + ab1dm/16.D0*sum2
              EPIt1 = EPIt1*FOG(1,nr1,inga)
              EPIt2 = ab2dp/8.D0*IS*(IS + 1) + (0.25D0*c2d - ab2dp/8.D0)
     &                *sum1 + ab2dm/16.D0*sum2
              EPIt2 = EPIt2*FOG(2,nr1,inga)/2.
            ELSEIF(ita.NE.ITAu.AND.ngu.NE.nga)THEN
              su1m = (0.25D0*c1d - ab1dp/8.D0)*sum1 + ab1dm/16.D0*sum2
              IF(ITAu.EQ.ita)su1m = su1m + ab1dp/8.D0*IS*(IS + 1.D0)
              su1m = su1m*FOG(1,nr1,inga)
              su2m = (0.25D0*c2d - ab2dp/8.D0)*sum1 + ab2dm/16.D0*sum2
              IF(ITAu.EQ.ita)su2m = su2m + ab2dp/8.*IS*(IS + 1.)
              su2m = su2m*FOG(2,nr1,inga)/2.D0
              dele = 2.D0/AMG0**2*(ANG1 - ANG2) + EIN(IS + 1,ITAu)
     &               - EIN(IS + 1,ita)
              sum3 = sum3 + (su1m + su2m)**2/dele
            ENDIF
          ENDDO
        ENDDO
        EPIt12 = sum3
      ENDIF
      RETURN
      END SUBROUTINE EIT12
 
!---------------------------------------------------------------------------
C     *****************************************************************
      SUBROUTINE ANUDF
C     *****************************************************************
      IMPLICIT DOUBLE PRECISION(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8, DIMENSION(4) :: AN
      REAL*8 :: ANU, CDV, DET, DN, DNV, XIT, XIT1, Y
      REAL*8, DIMENSION(6) :: B0, B1
      REAL*8, DIMENSION(5) :: B2, B3, C0, C1, C2, C3
      INTEGER :: NROot
      COMMON /FUDN  / Y, DN, ANU, DNV, CDV
      COMMON /SENU  / B0, B1, C0, C1, AN, B2, C2, B3, C3
      COMMON /XI12  / XIT, XIT1, DET, NROot
C
C Local variables
C
      REAL*8 :: GAMMA
      INTEGER :: nrp1
      REAL*8 :: y2, y3, y4, y5, z2
C
C*** End of declarations rewritten by SPAG
C
      ANU = NROot
      nrp1 = NROot + 1
      IF(XIT + 5.LE.0)THEN
        IF(XIT.LT. - 10.D0)ANU = ANU
        IF(XIT.GE. - 10.D0)THEN
          z2 = 2.D0*XIT*XIT
          ANU = ANU + EXP( - z2/2.D0)*z2**(ANU + 0.5D0)
     &          *0.39894228D0*(1.D0 - (ANU*ANU + ANU + 1.D0)/z2)
     &          /GAMMA(1.D0 + ANU)
        ENDIF
      ELSE
        Y = 5.D0 + XIT
        y2 = Y*Y
        y3 = Y*y2
        y4 = Y*y3
        IF(XIT.LE.AN(nrp1))THEN
          y5 = Y*y4
          SELECT CASE(nrp1)
          CASE(2)
            ANU = ANU + Y*EXP(B1(1) + B1(2)*Y + B1(3)*y2 + B1(4)
     &            *y3 + B1(5)*y4 + B1(6)*y5)
          CASE(3)
            ANU = ANU + Y*EXP(B2(1) + B2(2)*Y + B2(3)*y2 + B2(4)
     &            *y3 + B2(5)*y4)
          CASE(4)
            ANU = ANU + Y*EXP(B3(1) + B3(2)*Y + B3(3)*y2 + B3(4)
     &            *y3 + B3(5)*y4)
          CASE(5,6,7)
            GOTO 5
          CASE DEFAULT
            ANU = ANU + Y*EXP(B0(1) + B0(2)*Y + B0(3)*y2 + B0(4)
     &            *y3 + B0(5)*y4 + B0(6)*y5)
          END SELECT
        ELSE
          SELECT CASE(nrp1)
          CASE(2)
            ANU = ANU + C1(1) + C1(2)*Y + C1(3)*y2 + C1(4)*y3 + C1(5)*y4
          CASE(3)
            ANU = ANU + C2(1) + C2(2)*Y + C2(3)*y2 + C2(4)*y3 + C2(5)*y4
          CASE(4)
            ANU = ANU + C3(1) + C3(2)*Y + C3(3)*y2 + C3(4)*y3 + C3(5)*y4
          CASE(5,6,7)
            GOTO 5
          CASE DEFAULT
            ANU = ANU + C0(1) + C0(2)*Y + C0(3)*y2 + C0(4)*y3 + C0(5)*y4
          END SELECT
        ENDIF
        GOTO 10
    5   PRINT 1010
        WRITE(21,1010)
 1010   FORMAT(10X,'THIS CASE IS NOT REALIZED NROOT>=4')
      ENDIF
   10 RETURN
      END SUBROUTINE ANUDF
 
!---------------------------------------------------------------------------
C     *****************************************************************
      SUBROUTINE DETX12
C     *****************************************************************
      IMPLICIT DOUBLE PRECISION(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: ANU, CDV, DET, DN, DNV, XIT, XIT1, Y
      INTEGER :: NROot
      COMMON /FUDN  / Y, DN, ANU, DNV, CDV
      COMMON /XI12  / XIT, XIT1, DET, NROot
C
C Local variables
C
      REAL*8 :: ux1, ux2, vx1, vx2
C
C*** End of declarations rewritten by SPAG
C
      Y = XIT
      CALL FUDNU
      ux1 = DN
      vx1 = DNV
      Y = XIT1
      CALL FUDNU
      ux2 = DN
      vx2 = DNV
      DET = ux1*vx2 - ux2*vx1
      RETURN
      END SUBROUTINE DETX12
 
!---------------------------------------------------------------------------
C     *****************************************************************
      SUBROUTINE ANDET0
C     *****************************************************************
      IMPLICIT DOUBLE PRECISION(a - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: ANU, CDV, DET, DN, DNV, XIT, XIT1, Y
      INTEGER :: NROot
      COMMON /FUDN  / Y, DN, ANU, DNV, CDV
      COMMON /XI12  / XIT, XIT1, DET, NROot
C
C Local variables
C
      REAL*8 :: a, step
      INTEGER :: nr
C
C*** End of declarations rewritten by SPAG
C
      ANU = 0.D0
      nr = 0
   10 step = 0.1D0
      CALL DETX12
      a = DET
C     IF(A.EQ.0.) GO TO 2
      IF(ABS(a).GT.1.D-10)THEN
   15   ANU = ANU + step
        CALL DETX12
        IF(a*DET.LT.0)THEN
          ANU = ANU - step
          step = step/5.D0
          IF(step.GE.1.D-6)GOTO 15
        ELSEIF(a*DET.NE.0)THEN
          GOTO 15
        ENDIF
      ENDIF
      nr = nr + 1
      IF(nr.LE.NROot)THEN
        ANU = ANU + 0.01D0
        GOTO 10
      ENDIF
      CDV = -DN/DNV
      RETURN
      END SUBROUTINE ANDET0
 
!---------------------------------------------------------------------------
C     *****************************************************************
      SUBROUTINE OVLAG
C     *****************************************************************
      IMPLICIT DOUBLE PRECISION(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: AMB0, AMG0, ANG1, ANG2, ANU, BET0, BET4, CD1, CD2, CDV, 
     &          DG1, DG2, DN, DNV, FOLac, FOLag, FOLas, GAM0, GAMde, 
     &          GSHape, HW, Y
      INTEGER :: NNTg
      COMMON /FUDN  / Y, DN, ANU, DNV, CDV
      COMMON /OVLG  / ANG1, ANG2, FOLag, FOLac, FOLas, CD1, CD2, DG1, 
     &                DG2, NNTg
      COMMON /SHEM1 / HW, AMG0, AMB0, GAM0, BET0, BET4, GAMde, GSHape
C
C Local variables
C
      REAL*8 :: c1, cbg2, dn13, dn23, fnr1, fnr2, g3, pi3, s1m1, s1m2, 
     &          s2m1, s2m2, step, sum1, sum2, x, y0, ygh
      INTEGER :: i, ns2, nstep
C
C*** End of declarations rewritten by SPAG
C
      c1 = SQRT(2.D0)
      g3 = 3.D0*GAM0
      IF(NNTg.EQ.11)cbg2 = -27.D0/4.D0*AMG0**3*COS(g3)/SIN(g3)**3*c1
      ygh = -GSHape/AMG0*c1
C     PI3=1.0471975512D0
      pi3 = 4.D0/3.D0*ATAN(1.D0)
      nstep = 4.D0*(ANG1 + ANG2 + 4.D0)
      step = pi3/AMG0/nstep*c1
      sum2 = 0.D0
      s1m2 = 0.D0
      s2m2 = 0.D0
      sum1 = 0.D0
      s1m1 = 0.D0
      s2m1 = 0.D0
      y0 = -GAM0/AMG0*c1
      ns2 = nstep - 1
      DO i = 1, ns2
        ANU = ANG1
        Y = y0 + step*i
        x = (Y + ygh)**NNTg
        IF(NNTg.EQ.11)x = (Y + ygh + cbg2)**2
        CALL FUDNU
        dn13 = DN + CD1*DNV
        ANU = ANG2
        CALL FUDNU
        dn23 = DN + CD2*DNV
        IF(i/2*2.EQ.i)THEN
          sum2 = sum2 + x*dn13*dn23
          s1m2 = s1m2 + dn13**2
          s2m2 = s2m2 + dn23**2
        ELSE
          sum1 = sum1 + x*dn13*dn23
          s1m1 = s1m1 + dn13**2
          s2m1 = s2m1 + dn23**2
        ENDIF
      ENDDO
      fnr1 = s1m2*2.D0 + s1m1*4.D0
      fnr2 = s2m2*2.D0 + s2m1*4.D0
      FOLag = sum2*2.D0 + sum1*4.D0
      FOLag = FOLag/SQRT(fnr1*fnr2)*(AMG0/c1)**NNTg
      IF(NNTg.EQ.11)FOLag = FOLag/(AMG0/c1)**NNTg
      RETURN
      END SUBROUTINE OVLAG
 
!---------------------------------------------------------------------------
C     *****************************************************************
      SUBROUTINE KLEGO
C     *****************************************************************
      IMPLICIT DOUBLE PRECISION(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8, DIMENSION(800) :: A
      REAL*8 :: AKG
      INTEGER :: J, J1, J2, M, M1, M2
      COMMON /KG    / J1, J2, M1, M2, J, M, AKG
      COMMON /LOFAC / A
C
C Local variables
C
      REAL*8 :: c1, cl, cl1, fln
      INTEGER :: i1, k, n, nb, nf, nf1, nf2, nf3, nf4, nf5, nm
      INTEGER :: IABS
C
C*** End of declarations rewritten by SPAG
C
      IF(J1.GE.IABS(M1))THEN
        IF(J2.GE.IABS(M2))THEN
          IF(J.GE.IABS(M))THEN
            IF(M1 + M2.EQ.M)THEN
              IF(IABS(J1 - J2).LE.J)THEN
                IF(J1 + J2.GE.J)THEN
                  nf = J1 + J2 - J
                  IF(nf/2*2.EQ.nf)THEN
                    k = J1 + J2 + J
                    IF(M1.NE.0.OR.M.NE.0.OR.k/4*4.EQ.k)THEN
                      fln = A(nf + 2)
                      cl = fln
                      nf = J1 - J2 + J
                      fln = A(nf + 2)
                      cl = cl + fln
                      nf = J + J2 - J1
                      fln = A(nf + 2)
                      cl = cl + fln
                      nf = J1 + J2 + J + 2
                      fln = A(nf + 2)
                      cl = cl - fln
                      nf = J1 + M1
                      fln = A(nf + 2)
                      cl = cl + fln
                      nf = J1 - M1
                      fln = A(nf + 2)
                      cl = cl + fln
                      nf = J2 + M2
                      fln = A(nf + 2)
                      cl = cl + fln
                      nf = J2 - M2
                      fln = A(nf + 2)
                      cl = cl + fln
                      nf = J + M
                      fln = A(nf + 2)
                      cl = cl + fln
                      nf = J - M
                      fln = A(nf + 2)
                      cl = 0.5*(cl + fln)
                      nf1 = J1 + J2 - J
                      nf2 = J1 - M1
                      nf3 = J2 + M2
                      nf4 = J - J2 + M1
                      nf5 = J - J1 - M2
                      nb = nf1
                      nm = -nf4
                      IF(nf2.LT.nb)THEN
                        nb = nf2
                      ENDIF
                      IF(nf3.LT.nb)THEN
                        nb = nf3
                      ENDIF
                      IF( - nf5.GT.nm)nm = -nf5
                      IF(nm.LT.0)nm = 0
                      nm = nm + 2
                      nb = nb + 2
                      AKG = 0.D0
                      IF(nb.GE.nm)THEN
                        DO i1 = nm, nb, 2
                          c1 = 1.D0
                          n = i1 - 2
                          nf = n
                          fln = A(nf + 2)
                          cl1 = cl - fln
                          nf = nf1 - n
                          fln = A(nf + 2)
                          cl1 = cl1 - fln
                          nf = nf2 - n
                          fln = A(nf + 2)
                          cl1 = cl1 - fln
                          nf = nf3 - n
                          fln = A(nf + 2)
                          cl1 = cl1 - fln
                          nf = nf4 + n
                          fln = A(nf + 2)
                          cl1 = cl1 - fln
                          nf = nf5 + n
                          fln = A(nf + 2)
                          cl1 = cl1 - fln
                          IF(n/4*4.NE.n)c1 = -1.D0
                          AKG = AKG + c1*EXP(cl1)
                        ENDDO
                        AKG = AKG*SQRT(J + 1.D0)
                      ENDIF
                      GOTO 10
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      AKG = 0.D0
   10 RETURN
      END SUBROUTINE KLEGO
 
!---------------------------------------------------------------------------
C     *****************************************************************
      SUBROUTINE TRLAG
C     *****************************************************************
      IMPLICIT DOUBLE PRECISION(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: AMB0, AMG0, AMUo, ANG1, ANG2, ANU, BB32, BET0, BET3, 
     &          BET4, CD1, CD2, CDV, DG1, DG2, DN, DNV, DPAr, ETO, 
     &          FOLac, FOLag, FOLas, FOLc2, FOLc3, FOLs2, GAM0, GAMde, 
     &          GSHape, HW, HWO, Y
      INTEGER :: NNTg
      COMMON /FUDN  / Y, DN, ANU, DNV, CDV
      COMMON /OVLG  / ANG1, ANG2, FOLag, FOLac, FOLas, CD1, CD2, DG1, 
     &                DG2, NNTg
      COMMON /OVLG1 / FOLc2, FOLs2, FOLc3
      COMMON /SHAMO / BET3, ETO, AMUo, HWO, BB32, DPAr
      COMMON /SHEM1 / HW, AMG0, AMB0, GAM0, BET0, BET4, GAMde, GSHape
C
C Local variables
C
      REAL*8 :: c1, co, co2, co3, df, dn13, dn23, dy1, dy2, fnr1, fnr2, 
     &          pi3, s1m1, s1m2, s2m1, s2m2, scm1, scm12, scm13, scm2, 
     &          scm22, scm23, si, si2, ssm1, ssm12, ssm2, ssm22, step, 
     &          y0
      INTEGER :: i, ns2, nstep
C
C*** End of declarations rewritten by SPAG
C
C     PI3=1.0471975512D0
      pi3 = 4.D0/3.D0*ATAN(1.D0)
      c1 = SQRT(2.D0)
      nstep = 4.D0*(ANG1 + ANG2 + 4.D0)
      step = pi3/AMG0/nstep*c1
      scm2 = 0.D0
      ssm2 = 0.D0
      s1m2 = 0.D0
      s2m2 = 0.D0
      scm1 = 0.D0
      ssm1 = 0.D0
      s1m1 = 0.D0
      s2m1 = 0.D0
      scm22 = 0.D0
      ssm22 = 0.D0
      scm12 = 0.D0
      ssm12 = 0.D0
      scm13 = 0.D0
      scm23 = 0.D0
      y0 = -c1*GAM0/AMG0
      dy1 = -c1*DG1/AMG0
      dy2 = -c1*DG2/AMG0
      ns2 = nstep - 1
      DO i = 1, ns2
        ANU = ANG1
        Y = y0 + step*i
        si = SIN(GAM0 + AMG0*Y/c1)
        co = COS(GAM0 + AMG0*Y/c1)
        si2 = SIN(2*(GAM0 + AMG0*Y/c1))
        co2 = COS(2*(GAM0 + AMG0*Y/c1))
        co3 = COS(3*(GAM0 + AMG0*Y/c1))
        Y = y0 + step*i + dy1
        CALL FUDNU
        dn13 = DN + CD1*DNV
        ANU = ANG2
        Y = y0 + step*i + dy2
        CALL FUDNU
        dn23 = DN + CD2*DNV
        IF(i/2*2.EQ.i)THEN
          ssm2 = ssm2 + si*dn13*dn23
          scm2 = scm2 + co*dn13*dn23
          ssm22 = ssm22 + si2*dn13*dn23
          scm22 = scm22 + co2*dn13*dn23
          scm23 = scm23 + co3*dn13*dn23
          s1m2 = s1m2 + dn13**2
          s2m2 = s2m2 + dn23**2
        ELSE
          ssm1 = ssm1 + si*dn13*dn23
          scm1 = scm1 + co*dn13*dn23
          ssm12 = ssm12 + si2*dn13*dn23
          scm13 = scm13 + co3*dn13*dn23
          scm12 = scm12 + co2*dn13*dn23
          s1m1 = s1m1 + dn13**2
          s2m1 = s2m1 + dn23**2
        ENDIF
      ENDDO
      fnr1 = s1m2*2.D0 + s1m1*4.D0
      fnr2 = s2m2*2.D0 + s2m1*4.D0
      FOLas = ssm2*2.D0 + ssm1*4.D0
      FOLac = scm2*2.D0 + scm1*4.D0
      FOLs2 = ssm22*2.D0 + ssm12*4.D0
      FOLc2 = scm22*2.D0 + scm12*4.D0
      FOLc3 = scm23*2.D0 + scm13*4.D0
      df = SQRT(fnr1*fnr2)
      FOLas = FOLas/df
      FOLac = FOLac/df
      FOLs2 = FOLs2/df
      FOLc2 = FOLc2/df
      FOLc3 = FOLc3/df
      RETURN
      END SUBROUTINE TRLAG
 
!---------------------------------------------------------------------------
C     *****************************************************************
      SUBROUTINE OVLAGE
C     *****************************************************************
      IMPLICIT DOUBLE PRECISION(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: AMB0, AMG0, AMUo, ANG1, ANG2, ANU, BB32, BET0, BET3, 
     &          BET4, CD1, CD2, CDV, CG1, CG2, CG3, CG4, CSG11, CSG12, 
     &          CSG13, CSG21, CSG22, CSG31, DG1, DG2, DN, DNV, DPAr, EN, 
     &          ETO, FOLac, FOLag, FOLas, GAM0, GAMde, GSHape, HW, HWO, 
     &          SG1, SG2, SG3, SG4, Y
      REAL*8, DIMENSION(10) :: BET
      REAL*8, DIMENSION(20) :: EL
      REAL*8, DIMENSION(20,20,4) :: FOV
      INTEGER :: LAS, NMAx, NNTg, NPD, NUR
      COMMON /ENA   / EN, EL, BET, NUR, NMAx, NPD, LAS
      COMMON /FUDN  / Y, DN, ANU, DNV, CDV
      COMMON /OVKN  / FOV, CG1, SG1, CG2, SG2, CSG11, CG4, SG4, CSG22, 
     &                CSG31, CSG13, CG3, SG3, CSG21, CSG12
      COMMON /OVLG  / ANG1, ANG2, FOLag, FOLac, FOLas, CD1, CD2, DG1, 
     &                DG2, NNTg
      COMMON /SHAMO / BET3, ETO, AMUo, HWO, BB32, DPAr
      COMMON /SHEM1 / HW, AMG0, AMB0, GAM0, BET0, BET4, GAMde, GSHape
C
C Local variables
C
      REAL*8 :: anor, c1, c11, c12, c21, c22, c31, c32, c41, c42, co, 
     &          co2dn, co3dn, codn, cs111, cs112, cs121, cs122, cs131, 
     &          cs132, cs211, cs212, cs221, cs222, cs311, cs312, dn1, 
     &          dn12, dn2, dy1, dy2, pi3, s11, s12, s21, s22, s31, s32, 
     &          s41, s42, si, si2dn, si3dn, sidn, step, suf1, suf11, 
     &          suf12, suf2, suf21, suf22, y0
      INTEGER :: i, ns2, nstep
C
C*** End of declarations rewritten by SPAG
C
C     PI3=1.0471975512D0
      pi3 = 4.D0/3.D0*ATAN(1.D0)
      c1 = SQRT(2.D0)
      nstep = 4.*(ANG1 + ANG2 + 4.)
      step = pi3/AMG0/nstep*c1
      suf11 = 0.D0
      suf12 = 0.D0
      suf21 = 0.D0
      suf22 = 0.D0
      c11 = 0.D0
      s11 = 0.D0
      c12 = 0.D0
      s12 = 0.D0
      c21 = 0.D0
      c22 = 0.D0
      s21 = 0.D0
      s22 = 0.D0
      cs111 = 0.D0
      cs112 = 0.D0
      c31 = 0.D0
      c32 = 0.D0
      s31 = 0.D0
      s32 = 0.D0
      cs121 = 0.D0
      cs122 = 0.D0
      cs211 = 0.D0
      cs212 = 0.D0
      c41 = 0.D0
      c42 = 0.D0
      s41 = 0.D0
      s42 = 0.D0
      cs221 = 0.D0
      cs222 = 0.D0
      cs311 = 0.D0
      cs312 = 0.D0
      cs131 = 0.D0
      cs132 = 0.D0
      y0 = -c1*GAM0/AMG0
      dy1 = -c1*DG1/AMG0
      dy2 = -c1*DG2/AMG0
      ns2 = nstep - 1
      DO i = 1, ns2
        Y = y0 + step*i
        si = SIN(GAM0 + GSHape + AMG0*Y/c1)
        co = COS(GAM0 + GSHape + AMG0*Y/c1)
        ANU = ANG1
        Y = y0 + step*i + dy1
        CALL FUDNU
        dn1 = DN + CD1*DNV
        ANU = ANG2
        Y = y0 + step*i + dy2
        CALL FUDNU
        dn2 = DN + CD2*DNV
        dn12 = dn1*dn2
        IF(i/2*2.EQ.i)THEN
          suf12 = suf12 + dn1*dn1
          suf22 = suf22 + dn2*dn2
          codn = co*dn12
          sidn = si*dn12
          c12 = c12 + codn
          s12 = s12 + sidn
          IF(LAS.NE.1)THEN
            co2dn = co*codn
            si2dn = si*sidn
            c22 = c22 + co2dn
            s22 = s22 + si2dn
            cs112 = cs112 + co*sidn
            IF(LAS.NE.2)THEN
              co3dn = co*co2dn
              si3dn = si*si2dn
              c32 = c32 + co3dn
              s32 = s32 + si3dn
              cs122 = cs122 + co*si2dn
              cs212 = cs212 + si*co2dn
              IF(LAS.NE.3)THEN
                c42 = c42 + co*co3dn
                s42 = s42 + si*si3dn
                cs222 = cs222 + co2dn*si2dn/dn12
                cs312 = cs312 + si*co3dn
                cs132 = cs132 + co*si3dn
              ENDIF
            ENDIF
          ENDIF
        ELSE
          suf11 = suf11 + dn1*dn1
          suf21 = suf21 + dn2*dn2
          codn = co*dn12
          sidn = si*dn12
          c11 = c11 + codn
          s11 = s11 + sidn
          IF(LAS.NE.1)THEN
            co2dn = co*codn
            si2dn = si*sidn
            c21 = c21 + co2dn
            s21 = s21 + si2dn
            cs111 = cs111 + co*sidn
            IF(LAS.NE.2)THEN
              co3dn = co*co2dn
              si3dn = si*si2dn
              c31 = c31 + co3dn
              s31 = s31 + si3dn
              cs121 = cs121 + co*si2dn
              cs211 = cs211 + si*co2dn
              IF(LAS.NE.3)THEN
                c41 = c41 + co*co3dn
                s41 = s41 + si*si3dn
                cs221 = cs221 + co2dn*si2dn/dn12
                cs311 = cs311 + si*co3dn
                cs131 = cs131 + co*si3dn
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDDO
      suf1 = suf11*4.D0 + suf12*2.D0
      suf2 = suf21*4.D0 + suf22*2.D0
      anor = SQRT(suf1*suf2)
      CG1 = (c11*4.D0 + c12*2.D0)/anor
      SG1 = (s11*4.D0 + s12*2.D0)/anor
      IF(LAS.NE.1)THEN
        CG2 = (c21*4.D0 + c22*2.D0)/anor
        SG2 = (s21*4.D0 + s22*2.D0)/anor
        CSG11 = (cs111*4.D0 + cs112*2.D0)/anor
        IF(LAS.NE.2)THEN
          CG3 = (c31*4.D0 + c32*2.D0)/anor
          SG3 = (s31*4.D0 + s32*2.D0)/anor
          CSG21 = (cs211*4.D0 + cs212*2.D0)/anor
          CSG12 = (cs121*4.D0 + cs122*2.D0)/anor
          IF(LAS.NE.3)THEN
            CG4 = (c41*4.D0 + c42*2.D0)/anor
            SG4 = (s41*4.D0 + s42*2.D0)/anor
            CSG22 = (cs221*4.D0 + cs222*2.D0)/anor
            CSG31 = (cs311*4.D0 + cs312*2.D0)/anor
            CSG13 = (cs131*4.D0 + cs132*2.D0)/anor
          ENDIF
        ENDIF
      ENDIF
      RETURN
      END SUBROUTINE OVLAGE
 
!---------------------------------------------------------------------------
C     *****************************************************************
      SUBROUTINE OVLAB
C     *****************************************************************
      IMPLICIT DOUBLE PRECISION(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: ANU, ANU1, ANU2, CDV, DN, DNV, FOLar, P1, P2, X1, X2, Y
      INTEGER :: NNT
      COMMON /FUDN  / Y, DN, ANU, DNV, CDV
      COMMON /OVLB  / X1, P1, ANU1, X2, P2, ANU2, FOLar, NNT
C
C Local variables
C
      REAL*8 :: ah, c, c1, dn12, dn13, dn22, dn23, fnr1, fnr2, fuyn, 
     &          s1m1, s1m2, s2m1, s2m2, sqx1, sqx2, step, step1, step2, 
     &          sum1, sum2, yy
      INTEGER :: n
C
C*** End of declarations rewritten by SPAG
C
      ah = 0.2D0
      c1 = SQRT(2.D0)
      c = X2*P1/P2/X1
      IF(c.GT.1.)THEN
        step2 = ah
        step1 = ah/c
      ELSE
        step1 = ah
        step2 = ah*c
      ENDIF
      step = -step1*P1/X1/c1
      n = 1
      sqx2 = c1*X2
      sqx1 = c1*X1
      ANU = ANU1
      Y = sqx1 + step1
      CALL FUDNU
      dn12 = DN
      ANU = ANU2
      Y = sqx2 + step2
      CALL FUDNU
      dn22 = DN
      sum2 = 0.D0
      s1m2 = 0.D0
      s2m2 = 0.D0
      fuyn = step**NNT
      IF(NNT.EQ.10)fuyn = (step - 1.D0)/step/step
      IF(NNT.EQ.11)fuyn = (step**3 - 1.D0)/step/step
      IF(NNT.EQ.12)fuyn = step - 1.D0
      IF(NNT.EQ.13)fuyn = 1.D0/step/step
      sum1 = fuyn*dn12*dn22
      s1m1 = dn12*dn12
      s2m1 = dn22*dn22
   10 n = n + 1
      yy = step*n
      fuyn = yy**NNT
      IF(NNT.EQ.10)fuyn = (yy - 1.D0)/yy/yy
      IF(NNT.EQ.11)fuyn = (yy**3 - 1.D0)/yy/yy
      IF(NNT.EQ.12)fuyn = yy - 1.
      IF(NNT.EQ.13)fuyn = 1./yy/yy
      ANU = ANU1
      Y = sqx1 + n*step1
      CALL FUDNU
      dn13 = DN
      ANU = ANU2
      Y = sqx2 + n*step2
      CALL FUDNU
      dn23 = DN
      IF(n/2*2.EQ.n)THEN
        sum2 = sum2 + fuyn*dn13*dn23
        s1m2 = s1m2 + dn13*dn13
        s2m2 = s2m2 + dn23*dn23
      ELSE
        sum1 = sum1 + fuyn*dn13*dn23
        s1m1 = s1m1 + dn13*dn13
        s2m1 = s2m1 + dn23*dn23
      ENDIF
      IF(ABS(dn13).GE.1.D-4*SQRT(s1m1).OR.ABS(dn23).GE.1.D-4*SQRT(s2m1))
     &   GOTO 10
      fnr1 = s1m2*2.D0 + s1m1*4.D0
      fnr2 = s2m2*2.D0 + s2m1*4.D0
      FOLar = sum2*2.D0 + sum1*4.D0
      FOLar = FOLar/SQRT(fnr1*fnr2)
      RETURN
      END SUBROUTINE OVLAB
 
!---------------------------------------------------------------------------
C     *****************************************************************
      SUBROUTINE OVLAO
C     *****************************************************************
      IMPLICIT DOUBLE PRECISION(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: AMUo, ANU, ANU1, ANU2, BB32, BET3, CDV, DN, DNV, DPAr, 
     &          ETO, FOLar, HWO, P1, P2, X1, X2, Y
      INTEGER :: NNT
      COMMON /FUDN  / Y, DN, ANU, DNV, CDV
      COMMON /OVLB  / X1, P1, ANU1, X2, P2, ANU2, FOLar, NNT
      COMMON /SHAMO / BET3, ETO, AMUo, HWO, BB32, DPAr
C
C Local variables
C
      REAL*8 :: ah, c1, dn12, dn13, dn22, dn23, fnr1, fnr2, fuyn, s1m1, 
     &          s1m2, s2m1, s2m2, step, stepp, sum1, sum2, yy
      INTEGER :: n
C
C*** End of declarations rewritten by SPAG
C
      ah = 0.2D0
      c1 = SQRT(2.D0)
      stepp = ah
      step = stepp*AMUo/c1
      n = 1
      ANU = ANU1
      Y = stepp
      CALL FUDNU
      dn12 = DN
      ANU = ANU2
      CALL FUDNU
      dn22 = DN
      sum2 = 0.D0
      s1m2 = 0.D0
      s2m2 = 0.D0
      fuyn = step**NNT
      sum1 = fuyn*dn12*dn22
      s1m1 = dn12*dn12
      s2m1 = dn22*dn22
   10 n = n + 1
      yy = step*n
      fuyn = yy**NNT
      ANU = ANU1
      Y = n*stepp
      CALL FUDNU
      dn13 = DN
      ANU = ANU2
      CALL FUDNU
      dn23 = DN
      IF(n/2*2.EQ.n)THEN
        sum2 = sum2 + fuyn*dn13*dn23
        s1m2 = s1m2 + dn13*dn13
        s2m2 = s2m2 + dn23*dn23
      ELSE
        sum1 = sum1 + fuyn*dn13*dn23
        s1m1 = s1m1 + dn13*dn13
        s2m1 = s2m1 + dn23*dn23
      ENDIF
      IF(ABS(dn13).GE.1.D-4*SQRT(s1m1).OR.ABS(dn23).GE.1.D-4*SQRT(s2m1))
     &   GOTO 10
      fnr1 = s1m2*2.D0 + s1m1*4.D0
      fnr2 = s2m2*2.D0 + s2m1*4.D0
      FOLar = sum2*2.D0 + sum1*4.D0
      FOLar = FOLar/SQRT(fnr1*fnr2)
      RETURN
      END SUBROUTINE OVLAO
 
!---------------------------------------------------------------------------
C     *****************************************************************
      SUBROUTINE INERMO
C     *****************************************************************
      IMPLICIT DOUBLE PRECISION(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: AMB0, AMG0, AMO, AMUo, BB32, BB42, BET0, BET3, BET4, 
     &          BMO, CMO, DELg, DPAr, ETO, GAM, GAM0, GAMde, GAMg, 
     &          GSHape, HW, HWO, PBEt2, PBEt3
      INTEGER :: IS, MEApp, MECha, MECul, MEDis, MEHam, MEHao, MEJob, 
     &           MEPot, MEPri, MERel, MERip, MERrr, MERzz, MESha, MESho, 
     &           MESol, MEVol, NO
      COMMON /INRM  / AMO, BMO, CMO, BB42, GAMg, DELg
      COMMON /MAT   / GAM, IS, NO
      COMMON /MENU  / MEJob, MEPot, MEHam, MECha, MEPri, MESol, MESha, 
     &                MESho, MEHao, MEApp, MEVol, MERel, MECul, MERzz, 
     &                MERrr, MEDis, MERip
      COMMON /PB    / PBEt2, PBEt3
      COMMON /SHAMO / BET3, ETO, AMUo, HWO, BB32, DPAr
      COMMON /SHEM1 / HW, AMG0, AMB0, GAM0, BET0, BET4, GAMde, GSHape
C
C Local variables
C
      REAL*8 :: a30, a32, a40, a42, a44, ab1, ab2, arg1, arg2, arg3, c1, 
     &          c2, c3, c30, c5, c7, cq5, cq7, csdg, csgg, pbe22, 
     &          pbet32, pi23, ssdg, ssgg
C
C*** End of declarations rewritten by SPAG
C
      pi23 = 2.0943951024D0
      c1 = SQRT(2.D0)
      c2 = 3.16227766017D0
      c3 = 2.64575131106D0
      arg1 = GAM - pi23
      arg2 = GAM - 2.D0*pi23
      arg3 = GAM
      arg1 = GAM - pi23 + GSHape
      arg2 = GAM - 2.*pi23 + GSHape
      arg3 = GAM + GSHape
      AMO = SIN(arg1)**2
      BMO = SIN(arg2)**2
      CMO = SIN(arg3)**2
      pbe22 = 1./PBEt2**2
      IF(MESha.LT.2)THEN
      ELSEIF(MESha.EQ.2)THEN
        AMO = AMO + BB42*2.5D0*pbe22
        BMO = BMO + BB42*2.5D0*pbe22
      ELSE
        c7 = 7.D0/12.D0
        c5 = 5.D0/12.D0
        cq7 = SQRT(c7)
        cq5 = SQRT(c5)
        IF(MESha.EQ.4)csdg = COS(DELg)
        IF(MESha.NE.4)THEN
          GAMg = GAM
          csdg = cq7*COS(3.D0*GAM)
        ENDIF
        ssdg = SQRT(1.D0 - csdg*csdg)
        csgg = COS(GAMg)
        ssgg = SIN(GAMg)
        a40 = cq7*csdg + cq5*ssdg*csgg
        a42 = -ssdg*ssgg/c1
        a44 = (cq5*csdg - cq7*ssdg*csgg)/c1
        ab1 = 2.5D0*a40*a40 + 4.*a42*a42 + a44*a44
        ab2 = 1.5D0*c2*a40*a42 + c3*a42*a44
        AMO = AMO + BB42*(ab1 + ab2)*pbe22
        BMO = BMO + BB42*(ab1 - ab2)*pbe22
        CMO = CMO + 2.D0*BB42*(a42*a42 + 4.D0*a44*a44)*pbe22
      ENDIF
      pbet32 = PBEt3**2*pbe22
      IF(MESho.LT.1)THEN
      ELSEIF(MESho.EQ.1)THEN
        AMO = AMO + BB32*1.5D0*pbet32
        BMO = BMO + BB32*1.5D0*pbet32
      ELSE
        c30 = SQRT(30.D0)
        a30 = COS(ETO)
        a32 = SIN(ETO)/c1
        ab1 = 1.5D0*a30*a30 + 2.D0*a32*a32
        ab2 = 0.5D0*c30*a30*a32
        AMO = AMO + BB32*(ab1 + ab2)*pbet32
        BMO = BMO + BB32*(ab1 - ab2)*pbet32
        CMO = CMO + 2.D0*BB32*a32*a32*pbet32
      ENDIF
      AMO = 1.D0/AMO
      BMO = 1.D0/BMO
      CMO = 1.D0/CMO
      RETURN
      END SUBROUTINE INERMO
 
!---------------------------------------------------------------------------
C     *************************************************************
      FUNCTION GAMMA(Xx)
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      REAL*8 :: Xx
      REAL*8 :: GAMMA
C
C Local variables
C
      REAL*8, DIMENSION(6) :: cof
      REAL*8 :: fpf, half, one, ser, stp, tmp, x
      INTEGER :: j
C
C*** End of declarations rewritten by SPAG
C
C     *************************************************************
      DATA cof, stp/76.18009173D0, -86.50532033D0, 24.01409822D0, 
     &     -1.231739516D0, .120858003D-2, -.536382D-5, 2.50662827465D0/
      DATA half, one, fpf/0.5D0, 1.0D0, 5.5D0/
      x = Xx - one
      tmp = x + fpf
      tmp = tmp**(x + half)*EXP( - tmp)
      ser = one
      DO j = 1, 6
        x = x + one
        ser = ser + cof(j)/x
      ENDDO
      GAMMA = tmp*stp*ser
      RETURN
      END FUNCTION GAMMA
 
!---------------------------------------------------------------------------
C     *****************************************************************
      SUBROUTINE TRANS
C     *****************************************************************
      IMPLICIT DOUBLE PRECISION(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8, DIMENSION(31,16) :: AIT
      REAL*8 :: AKG, AMB0, AMG0, AMO, AMUo, ANG1, ANG2, ANU, ANU1, ANU2, 
     &          BB32, BB42, BET0, BET3, BET4, BMO, CD1, CD2, CDV, CMO, 
     &          DELg, DG1, DG2, DN, DNV, DPAr, EN, ETO, FOLac, FOLag, 
     &          FOLar, FOLas, FOLc2, FOLc3, FOLs2, GAM, GAM0, GAMde, 
     &          GAMg, GSHape, HW, HWO, P1, P2, X1, X2, Y
      REAL*8, DIMENSION(20) :: ANB, ANO, EGB, EL, EP0, EP1, EP12, EP2, 
     &                         EPB, EPG, ES, GIT, PT, XI
      REAL*8, DIMENSION(20,2) :: ANG, CD
      REAL*8, DIMENSION(21,21) :: B0, B2, B3, B4
      REAL*8, DIMENSION(10) :: BET
      REAL*8, DIMENSION(2,8,8) :: FOG
      INTEGER :: IS, J, J1, J2, LAS, M, M1, M2, MEApp, MECha, MECul, 
     &           MEDis, MEHam, MEHao, MEJob, MEPot, MEPri, MERel, MERip, 
     &           MERrr, MERzz, MESha, MESho, MESol, MEVol, NMAx, NNT, 
     &           NNTg, NO, NPD, NUR
      INTEGER, DIMENSION(20) :: JU, NNB, NNG, NNO, NPI, NTU
      INTEGER, DIMENSION(25) :: NPJ
      COMMON /AA    / ANO
      COMMON /ENA   / EN, EL, BET, NUR, NMAx, NPD, LAS
      COMMON /FUDN  / Y, DN, ANU, DNV, CDV
      COMMON /INRM  / AMO, BMO, CMO, BB42, GAMg, DELg
      COMMON /KG    / J1, J2, M1, M2, J, M, AKG
      COMMON /MAT   / GAM, IS, NO
      COMMON /MENU  / MEJob, MEPot, MEHam, MECha, MEPri, MESol, MESha, 
     &                MESho, MEHao, MEApp, MEVol, MERel, MECul, MERzz, 
     &                MERrr, MEDis, MERip
      COMMON /NP    / NPJ
      COMMON /OVLB  / X1, P1, ANU1, X2, P2, ANU2, FOLar, NNT
      COMMON /OVLG  / ANG1, ANG2, FOLag, FOLac, FOLas, CD1, CD2, DG1, 
     &                DG2, NNTg
      COMMON /OVLG1 / FOLc2, FOLs2, FOLc3
      COMMON /SHAMO / BET3, ETO, AMUo, HWO, BB32, DPAr
      COMMON /SHEM1 / HW, AMG0, AMB0, GAM0, BET0, BET4, GAMde, GSHape
      COMMON /SHEM2 / GIT, EP0, EP1, EP2, EP12, ANG, EPB, EPG, EGB, AIT, 
     &                PT, FOG, XI, ANB, CD
      COMMON /SHEMM / ES, JU, NTU, NNB, NNG, NNO, NPI
      COMMON /TR    / B2, B4, B3
      COMMON /TR1   / B0
C
C Local variables
C
      REAL*8 :: a1, a2, a3, a30, a32, a4, a40, a42, a44, ac00, ac2, 
     &          ac22, ac24, ac3, ac4, agg4, ago4, aog3, aonu1, aonu2, 
     &          aqg2, aqo2, as2, as24, as3, as4, bb1, bb2, bb3, bb4, 
     &          bc2, bc3, bc4, bgg4, bgo4, bog3, bqg2, bqo2, bs00, bs2, 
     &          bs22, bs24, bs3, bs4, c00, c1, c3g0, c512, c712, cc4, 
     &          cgg4, cgo4, cogo, cq512, cq57, cq59, cq712, cq79, cs24, 
     &          cs4, csdg, csgg, df, di, ebm, flc2, fls2, foks, foks1, 
     &          folar1, folar2, g1, g2, g3, g4, ggg00, ggg20, ggg22, 
     &          ggg24, ggg40
      REAL*8 :: ggg42, ggg44, goo00, goo02, goo20, goo22, goo24, gqg00, 
     &          gqg02, gqg20, gqg22, gqg24, gqg42, gqg44, gqq00, gqq20, 
     &          gqq22, gqq24, oc1, oc2, oog00, oog02, oog20, oog22, 
     &          oog42, oqo00, oqo02, oqo20, oqo22, pi, po1, po2, q1, q3, 
     &          q4, qc2ga, qcg1a, qcg1b, qg0a, qg0b, qgg00, qgg02, 
     &          qgg20, qgg40, qgg42, qo0a, qo0b, qoo00, qoo02, qoo20, 
     &          qqg00, qqg02, qqg20, qqg22, qqg42, qqq00, qqq22, qs2gb, 
     &          qsg1a, qsg1b, sigo, sq5, sqpi, ssdg, ssgg,sumx,sum1,t1
      INTEGER :: i, jf, ji, jif, jj, jkf, jki, kf, kg1, kg2, ki, kmf, 
     &           kmi, ng1f, ng1i, nkf, nki, no1f, no1i
      INTEGER :: IABS
C
C*** End of declarations rewritten by SPAG
C
      IF(MEPri.LT.99)PRINT 1010
      WRITE(21,1010)
      IF(MEHam.EQ.5.AND.MEPri.LT.99)PRINT 1030
      IF(MEHam.EQ.5)WRITE(21,1030)
      IF(MEHam.EQ.3.AND.MEPri.LT.99)PRINT 1040
      IF(MEHam.EQ.3)WRITE(21,1040)
      IF(MEPri.LT.99)PRINT 1050
      WRITE(21,1050)
      IF(MEPri.LT.99)PRINT 1020
      WRITE(21,1020)
 1010 FORMAT(30X,'E2-TRANSITION PROBABILITIES B(E2:I->F)')
 1020 FORMAT(1X,120('-'))
 1030 FORMAT(20X,'( CALCULATIONS INVOLVE  5-PARAM. MODEL')
 
 1040 FORMAT(20X,'( CALCULATIONS INVOLVE DAVYDOV-CHABAN MODEL')
 1050 FORMAT(20X,'TAKING INTO ACCOUNT SQUARE A(LAM,MU) TERMS )')
 1060 FORMAT(30X,'E3-TRANSITION PROBABILITIES B(E3;I->F)')
 1070 FORMAT(30X,' E4-TRANSITION PROBABILITIES B(E4:I->F)')
      c1 = SQRT(2.D0)
      pi = 4.D0*ATAN(1.D0)
      sqpi = SQRT(pi)
      sq5 = SQRT(5.D0)
      cq57 = SQRT(5.D0/7.D0)
      cq59 = SQRT(5.D0)/3.D0
      cq79 = SQRT(7.D0)/3.D0
      c00 = 5.D0*sq5/21.D0/sqpi
      IF(MESha.LT.2)THEN
      ELSEIF(MESha.EQ.2)THEN
C     LAMBDA4 AXIAL DEFORMATION
        a40 = 1.D0
        a42 = 0.D0
        a44 = 0.D0
      ELSE
C     LAMBDA4 NON-AXIAL DEFORMATION
        c712 = 7.D0/12.D0
        c512 = 5.D0/12.D0
        cq712 = SQRT(c712)
        cq512 = SQRT(c512)
        IF(MESha.EQ.4)csdg = COS(DELg)
        IF(MESha.NE.4)THEN
          GAMg = GAM
          csdg = cq712*COS(3.D0*GAM)
        ENDIF
        ssdg = SQRT(1.D0 - csdg*csdg)
        csgg = COS(GAMg)
        ssgg = SIN(GAMg)
        a40 = cq712*csdg + cq512*ssdg*csgg
        a42 = -ssdg*ssgg/c1
        a44 = (cq512*csdg - cq712*ssdg*csgg)/c1
      ENDIF
      IF(MESho.LT.1)THEN
      ELSEIF(MESho.EQ.1)THEN
C     LAMBDA3 AXIAL DEFORMATION
        a30 = 1.D0
        a32 = 0.D0
      ELSE
C     LAMBDA3 NON-AXIAL DEFORMATION
        a30 = COS(ETO)
        a32 = SIN(ETO)/c1
      ENDIF
      sigo = SIN(GAM0 + GSHape)
      cogo = COS(GAM0 + GSHape)
      FOLac = COS(GAM0 + GSHape)
      FOLas = SIN(GAM0 + GSHape)
      FOLc2 = COS(2.D0*(GAM0 + GSHape))
      FOLs2 = SIN(2.D0*(GAM0 + GSHape))
      flc2 = COS(GAM0 + GSHape)**2
      fls2 = SIN(GAM0 + GSHape)**2
      FOLc3 = COS(3.D0*(GAM0 + GSHape))
      c3g0 = COS(3.D0*(GAM0 + GSHape))
      foks = BET3
      foks1 = BET3*BET3
C
C     VECTOR COEFFICIENTS FOR E2-,E3- E4-TRANSITIONS
C
      qqq00 = -0.5345224838D0
      qqq22 = -qqq00
      gqq00 = 0.7171371656D0
      gqq20 = 0.1195228609D0
      gqq22 = 0.4629100500D0
      gqq24 = 1.D0
      gqg00 = -0.5096471915D0
      gqg20 = 0.5921565255D0
      gqg22 = -0.2038588766D0
      gqg02 = gqg20
      gqg42 = 0.3302891295D0
      gqg44 = 0.7135060680D0
      gqg24 = gqg42
      qgg00 = cq59*gqg00
      qgg20 = cq59*gqg22
      qgg02 = cq59*gqg02
      qgg42 = cq59*gqg24
      qgg40 = cq59*gqg44
      qqg00 = cq59*gqq00
      qqg20 = cq59*gqq22
      qqg22 = qqg20
      qqg02 = cq59*gqq20
      qqg42 = cq59*gqq24
      goo00 = -0.4834937784D0
      goo20 = 0.5640760748D0
      goo02 = 0.1395726315D0
      goo24 = -0.6741998625D0
      goo22 = goo02
      ggg00 = 0.4022911408D0
      ggg20 = -0.245844586D0
      ggg40 = 0.3128931094D0
      ggg22 = ggg20
      ggg42 = 0.560968194D0
      ggg44 = ggg40
      ggg24 = ggg42
      oqo00 = -0.5163977795D0
      oqo20 = 0.5773502692D0
      oqo02 = oqo20
      oqo22 = 0.D0
      oog00 = -cq79*goo00
      oog20 = -cq79*goo20
      oog22 = -cq79*goo22
      oog02 = -cq79*goo20
      oog42 = -cq79*goo24
      qoo00 = -cq57*oqo00
      qoo02 = -cq57*oqo02
      qoo20 = -cq57*oqo22
C
C     CONSTANTS FOR E2-TRANSITIONS
C     Q20=3.*Z*e*RR*2*BETTA20/SQRT(5*PI)
C     CONST=5*Q20**2/16/PI   RESULTS TO BE MULTIPLIED BY
C
      qcg1a = 6.D0*BET4/sqpi
      qsg1a = qcg1a*c1
      qc2ga = sq5/sqpi*BET0
      qg0a = 3.D0*BET4**2/sq5/sqpi/BET0
      qo0a = 7.D0/sqpi/sq5/BET0
C     QO0A=7.D0/SQPI/SQ5/BET0*BET3**2
      qsg1b = qcg1a/c1
      qcg1b = qcg1a
      qs2gb = qc2ga/c1
      qg0b = qg0a*2.D0
      qo0b = qo0a*2.D0
      ac2 = 1.D0 + qcg1a*qqg00**2*a40
      as2 = qsg1a*qqg00*qqg20*a42
      ac22 = qc2ga*qqq00**2
      aqg2 = qg0a*qgg00*(2.D0*qgg40*a44**2 + 2.D0*qgg20*a42**2 + 
     &       qgg00*a40**2)
      aqo2 = qo0a*qoo00*(qoo00*a30**2 + 2.D0*qoo20*a32**2)
      bs2 = 1.D0/c1 + qsg1b*qqg00*(qqg02*a40 + qqg42*a44)
      bc2 = qcg1b*qqg00*qqg22*a42
      bs22 = qs2gb*qqq00*qqq22
      bqg2 = qg0b*qgg00*(qgg02*a40*a42 + qgg42*a42*a44)
      bqo2 = qo0b*qoo00*qoo02*a30*a32
C
C     CONSTANTS FOR E3-TRANSITIONS
C     Q30=3.*Z*e*RR*3*BETTA30/SQRT(7*PI)
C     CONST=7*Q30**2/16/PI RESULTS TO BE MULTIPLIED BY
C
      oc1 = 2.5D0*sq5/sqpi*BET0
      oc2 = 7.5D0*BET4/sqpi
      ac3 = oc1*oqo00**2*COS(ETO)
      as3 = oc1*oqo00*oqo20*SIN(ETO)*0.5D0
      ac00 = COS(ETO)
      aog3 = oc2*oog00*(oog00*COS(ETO)*a40 + c1*oog20*SIN(ETO)*a42)
      bc3 = oc1*oqo00*oqo22*SIN(ETO)/c1
      bs3 = oc1*oqo00*oqo02*COS(ETO)/c1
      bs00 = SIN(ETO)/c1
      bog3 = oc2*oog00*(oog22*COS(ETO)*a42 + SIN(ETO)
     &       /c1*(oog02*a40 + oog42*a44))
C
C     CONSTANTS FOR E4-TRANSITIONS
C     Q40=3.*Z*e*RR*4*BETTA40/SQRT(9*PI)
C     CONST=9*Q40**2/16/PI  RESULTS TO BE MULTIPLIED BY
C
      a1 = 2.5D0/sqpi*BET0**2/BET4
      a2 = 3.D0*sq5/sqpi*BET0
      a3 = 4.5D0/sqpi*BET4
      a4 = 3.5D0/sqpi/BET4
C     A4=3.5D0/SQPI/BET4*BET3**2
      bb1 = a1/c1
      bb2 = a2
      bb3 = a3*2.D0
      bb4 = a4*2.D0
      g1 = a1/2.D0
      g2 = a2
      g3 = a3
      g4 = a4
      ac4 = a2*gqg00**2*a40
      as4 = a2*gqg00*gqg20*c1*a42
      ac24 = a1*gqq00**2
      as24 = a1*gqq00*gqq20
      agg4 = a3*ggg00*(ggg00*a40**2 + 2.D0*ggg20*a44**2 + 
     &       2.D0*ggg40*a44**2)
      ago4 = a4*goo00*(goo00*a30**2 + 2.D0*goo20*a32**2)
      bc4 = bb2*gqg00*gqg22*a42
      bs4 = bb2*gqg00*(gqg02*a40 + gqg42*a44)/c1
      bs24 = bb1*gqq00*gqq22
      bgg4 = bb3*ggg00*(ggg22*a40*a42 + ggg42*a44*a42)
      bgo4 = bb4*goo00*goo22*a30*a32
      cc4 = g2*gqg00**2*a44
      cs4 = g2*gqg00*gqg24*a42/c1
      cs24 = g1*gqg00*gqg24
      cgg4 = g3*ggg00*(2.D0*ggg44*a40*a44 + ggg24*a42**2)
      cgo4 = g4*goo00*goo24*a32**2
C     CALCULATIONS OF E2-TRANSITION PROBABILITIES
      J2 = 4
      DO i = 1, NUR
        ji = JU(i)
        po1 = ( - 1)**NNO(i)
        DG1 = 0.
        IF(po1.NE.1)DG1 = GAMde
        kg1 = (3 - po1)/2
        ng1i = NNG(i) + 1
        ANG1 = ANG(ng1i,kg1)
        no1i = NNO(i) + 1
        aonu1 = ANO(no1i)
        CD1 = CD(ng1i,kg1)
        J1 = 2*ji
        kmi = 1 - ( - 1)**(ji + NNO(i))
        nki = (2*(ji/2) - kmi + 2)/2
        DO jj = 1, NUR
          jf = JU(jj)
          po2 = ( - 1)**NNO(jj)
          DG2 = 0.
          IF(po2.NE.1)DG2 = GAMde
          kg2 = (3 - po2)/2
          ng1f = NNG(jj) + 1
          ANG2 = ANG(ng1f,kg2)
          no1f = NNO(jj) + 1
          aonu2 = ANO(no1f)
          CD2 = CD(ng1f,kg2)
          J = 2*jf
          jif = IABS(ji - jf)
          IF(jif.LE.2)THEN
            IF(ji.NE.0.OR.jf.NE.0)THEN
              IF(po1.EQ.po2)THEN
                IF(MEHao.NE.0)THEN
                  IF(MEHao.GE.2)THEN
                    ebm = EXP( - (BET3/AMUo)**2)
                    foks1 = AMUo**2/2.D0 + 
     &                      BET3**2/(1.D0 - ( - 1)**no1i*ebm)
                    IF(MEHao.GE.2)GOTO 5
                  ENDIF
                  NNT = 2
                  X1 = -BET3/AMUo
                  X2 = X1
                  P1 = BET3
                  P2 = BET3
                  ANU1 = aonu1
                  ANU2 = aonu2
                  IF(BET3.EQ.0.D0)CALL OVLAO
                  IF(BET3.NE.0.D0)CALL OVLAB
                  foks1 = FOLar
                ENDIF
    5           IF(MEHam.GE.5)CALL TRLAG
                X1 = XI(i)
                X2 = XI(jj)
                P1 = PT(i)
                P2 = PT(jj)
                ANU1 = ANB(i)
                ANU2 = ANB(jj)
                folar2 = 1.D0
                IF(MEHam.NE.4)THEN
                  NNT = 3
                  CALL OVLAB
                  folar2 = FOLar
                ENDIF
                NNT = 2
                IF(MEHam.NE.4)CALL OVLAB
                IF(MEHam.EQ.4)FOLar = 1.
                folar1 = FOLar
                IF(MEHao.EQ.2)foks1 = foks1*folar1*BET0**2
                NNT = 1
                IF(MEHam.NE.4)CALL OVLAB
                IF(MEHam.EQ.4)FOLar = 1.
                NNTg = 1
                IF(MEHam.GT.4)CALL OVLAG
                kmf = 1 - ( - 1)**(jf + NNO(jj))
                nkf = (2*(jf/2) - kmf + 2)/2
                sumx = 0.D0
                DO jki = 1, nki
                  ki = kmi + 2*(jki - 1)
                  di = 1.D0
                  IF(ki.EQ.0)di = c1
                  DO jkf = 1, nkf
                    kf = kmf + 2*(jkf - 1)
                    sum1 = 0.D0
                    df = 1.D0
                    IF(kf.EQ.0)df = c1
                    M = 2*kf
                    IF(kf.EQ.ki)THEN
                      M1 = 2*ki
                      M2 = 0
                      CALL KLEGO
                      t1 = AKG
                      IF(kf.EQ.0)THEN
                        M1 = -2*ki
                        CALL KLEGO
                        t1 = t1 + AKG*( - 1)**(ji + NNO(i))
                      ENDIF
                      sum1 = sum1 + 
     &                       t1*((ac2*FOLac + as2*FOLas)*FOLar + ac22*
     &                       FOLc2*folar1 + aqg2 + aqo2*foks1)
                    ENDIF
                    t1 = 0.
                    IF((ki + 2).EQ.kf)THEN
                      M1 = 2*ki
                      M2 = 4
                      CALL KLEGO
                      t1 = AKG
                    ENDIF
                    IF((ki - 2).EQ.kf)THEN
                      M1 = 2*ki
                      M2 = -4
                      CALL KLEGO
                      t1 = t1 + AKG
                    ENDIF
                    IF((2 - ki).EQ.kf)THEN
                      M1 = -2*ki
                      M2 = 4
                      CALL KLEGO
                      t1 = t1 + AKG*( - 1)**(ji + NNO(i))
                    ENDIF
                    sum1 = sum1 + 
     &                     t1*((bc2*FOLac + bs2*FOLas)*FOLar + bs22*
     &                     FOLs2*folar1 + bqg2 + bqo2*foks1)
                    sumx = sumx + sum1*AIT(i,jki)*AIT(jj,jkf)/di/df
                  ENDDO
                ENDDO
                IF(i.NE.jj)THEN
                  B2(i,jj) = sumx*sumx
                  IF(MEPri.LT.99)PRINT 1020
                  WRITE(21,1020)
                  IF(MEPri.LT.99)PRINT 1090, ES(i), ji, NTU(i), NNB(i), 
     &                                 NNG(i), NNO(i), ES(jj), jf, 
     &                                 NTU(jj), NNB(jj), NNG(jj), 
     &                                 NNO(jj), B2(i,jj)
                  WRITE(21,1090)ES(i), ji, NTU(i), NNB(i), NNG(i), 
     &                          NNO(i), ES(jj), jf, NTU(jj), NNB(jj), 
     &                          NNG(jj), NNO(jj), B2(i,jj)
                  IF(MEPri.LT.99)PRINT 1020
                  WRITE(21,1020)
                  IF(MEPri.LT.99)PRINT 1080, FOLac, FOLas, FOLar, cogo, 
     &                                 sigo, foks1, FOLag
                  WRITE(21,1080)FOLac, FOLas, FOLar, cogo, sigo, foks1, 
     &                          FOLag
 1080             FORMAT(5X,'I(COS)=',D11.4,'  I(SIN)=',D11.4,
     &                   '  FOLAR=',D11.4,'  COS(G0)=',D11.4,
     &                   '  SIN(G0)=',D11.4,'  FOKS1=',D11.4,' FOLAG=',
     &                   D11.4//)
 1090             FORMAT(5X,'B(E2:(',D11.4,')JI=',I2,'TAU=',I1,'NB=',I1,
     &                   'NG=',I1,'NO=',I1,'--->(',D11.4,')JF=',I2,
     &                   'TAU=',I1,'NB=',I1,'NG=',I1,'NO=',I1,')=',
     &                   D11.4)
                ELSE
                  q1 = SQRT(ji*(2.D0*ji - 1.D0)
     &                 /((ji+1.D0)*(2.D0*ji+1.)*(2.D0*ji+3.)))
                  q1 = q1*sumx*SQRT(2.D0*ji + 1.D0)
                  IF(MEPri.LT.99)PRINT 1100, ES(i), ji, NTU(i), NNB(i), 
     &                                 NNG(i), NNO(i), q1
                  WRITE(21,1100)ES(i), ji, NTU(i), NNB(i), NNG(i), 
     &                          NNO(i), q1
 1100             FORMAT(10X,
     &                   'AVERAGE VALUE OF LAMBDA2 MOMENT FOR STATE  E='
     &                   ,D11.4,'JI=',I2,'TAU=',I1,'NB=',I1,'NG=',I1,
     &                   'NO=',I1,3X,'DEVIDED BY Q20=',D11.4)
                  CYCLE
                ENDIF
              ENDIF
            ENDIF
          ENDIF
C     LAMBDA0 TRANSITION PROBABILITIES
          IF(jif.EQ.0)THEN
            IF(i.NE.jj)THEN
              IF(MEHam.NE.4)THEN
                B0(i,jj) = (BET0*BET0*folar1 + c00*BET0**3*folar2*FOLc3)
     &                     **2
                IF(MEPri.LT.99)PRINT 1020
                WRITE(21,1020)
                IF(MEPri.LT.99)PRINT 1110, ES(i), ji, NTU(i), NNB(i), 
     &                               NNG(i), NNO(i), ES(jj), jf, NTU(jj)
     &                               , NNB(jj), NNG(jj), NNO(jj), 
     &                               B0(i,jj)
                WRITE(21,1110)ES(i), ji, NTU(i), NNB(i), NNG(i), NNO(i), 
     &                        ES(jj), jf, NTU(jj), NNB(jj), NNG(jj), 
     &                        NNO(jj), B0(i,jj)
 1110           FORMAT(5X,'B(E0:(',D11.4,')JI=',I2,'TAU=',I1,'NB=',I1,
     &                 'NG=',I1,'NO=',I1,'--->(',D11.4,')JF=',I2,'TAU=',
     &                 I1,'NB=',I1,'NG=',I1,'NO=',I1,')=',D11.4)
                IF(MEPri.LT.99)PRINT 1120, FOLc3, folar2, folar1, c3g0
                WRITE(21,1120)FOLc3, folar2, folar1, c3g0
 1120           FORMAT(5X,'I(COS(3*GAM0))=',D11.4,' FOLAR2 =',D11.4,
     &                 ' FOLAR1=',D11.4,'  COS(3*G0)=',D11.4/)
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDDO
      IF(MESha.GE.2)THEN
        IF(MEPri.LT.99)PRINT 1070
        WRITE(21,1070)
        IF(MEPri.LT.99)PRINT 1020
        WRITE(21,1020)
C     CALCULATIONS OF E4- TRANSITION PROBABILITIES
        J2 = 8
        DO i = 1, NUR
          ji = JU(i)
          po1 = ( - 1)**NNO(i)
          DG1 = 0.
          IF(po1.NE.1)DG1 = GAMde
          kg1 = (3 - po1)/2
          ng1i = NNG(i) + 1
          ANG1 = ANG(ng1i,kg1)
          no1i = NNO(i) + 1
          aonu1 = ANO(no1i)
          CD1 = CD(ng1i,kg1)
          J1 = 2*ji
          kmi = 1 - ( - 1)**(ji + NNO(i))
          nki = (2*(ji/2) - kmi + 2)/2
          DO jj = 1, NUR
            jf = JU(jj)
            po2 = ( - 1)**NNO(jj)
            DG2 = 0.
            IF(po2.NE.1)DG2 = GAMde
            kg2 = (3 - po2)/2
            ng1f = NNG(jj) + 1
            ANG2 = ANG(ng1f,kg2)
            no1f = NNO(jj) + 1
            aonu2 = ANO(no1f)
            CD2 = CD(ng1f,kg2)
            J = 2*jf
            jif = IABS(ji - jf)
            IF(jif.LE.4)THEN
              IF(ji.NE.0.OR.jf.NE.0)THEN
                IF(po1.EQ.po2)THEN
                  IF(MEHao.NE.0)THEN
                    IF(MEHao.GE.2)THEN
                      ebm = EXP( - (BET3/AMUo)**2)
                      foks1 = AMUo**2/2.D0 + 
     &                        BET3**2/(1.D0 - ( - 1)**no1i*ebm)
                      IF(MEHao.GE.2)GOTO 10
                    ENDIF
                    NNT = 2
                    X1 = -BET3/AMUo
                    X2 = X1
                    P1 = BET3
                    P2 = BET3
                    ANU1 = aonu1
                    ANU2 = aonu2
                    IF(BET3.EQ.0.D0)CALL OVLAO
                    IF(BET3.NE.0.D0)CALL OVLAB
                    foks1 = FOLar
                  ENDIF
   10             IF(MEHam.GE.5)CALL TRLAG
                  flc2 = (1.D0 + FOLc2)/2.D0
                  fls2 = (1.D0 - FOLc2)/2.D0
                  X1 = XI(i)
                  X2 = XI(jj)
                  P1 = PT(i)
                  P2 = PT(jj)
                  ANU1 = ANB(i)
                  ANU2 = ANB(jj)
                  NNT = 2
                  IF(MEHam.NE.4)CALL OVLAB
                  IF(MEHam.EQ.4)FOLar = 1.D0
                  folar1 = FOLar
                  IF(MEHao.EQ.2)foks1 = foks1*folar1*BET0**2
                  NNT = 1
                  IF(MEHam.NE.4)CALL OVLAB
                  IF(MEHam.EQ.4)FOLar = 1.
                  kmf = 1 - ( - 1)**(jf + NNO(jj))
                  nkf = (2*(jf/2) - kmf + 2)/2
                  sumx = 0.D0
                  DO jki = 1, nki
                    ki = kmi + 2*(jki - 1)
                    di = 1.D0
                    IF(ki.EQ.0)di = c1
                    DO jkf = 1, nkf
                      kf = kmf + 2*(jkf - 1)
                      sum1 = 0.D0
                      df = 1.D0
                      IF(kf.EQ.0)df = c1
                      M = 2*kf
                      IF(kf.EQ.ki)THEN
                        M1 = 2*ki
                        M2 = 0
                        CALL KLEGO
                        t1 = AKG
                        IF(kf.EQ.0)THEN
                          M1 = -2*ki
                          CALL KLEGO
                          t1 = t1 + AKG*( - 1)**(ji + NNO(i))
                        ENDIF
                        sum1 = sum1 + t1*(a40 + (ac4*FOLac + as4*FOLas)
     &                         *FOLar + (ac24*flc2 + as24*fls2)
     &                         *folar1 + agg4 + ago4*foks1)
                      ENDIF
                      t1 = 0.
                      IF((ki + 2).EQ.kf)THEN
                        M1 = 2*ki
                        M2 = 4
                        CALL KLEGO
                        t1 = AKG
                      ENDIF
                      IF((ki - 2).EQ.kf)THEN
                        M1 = 2*ki
                        M2 = -4
                        CALL KLEGO
                        t1 = t1 + AKG
                      ENDIF
                      IF((2 - ki).EQ.kf)THEN
                        M1 = -2*ki
                        M2 = 4
                        CALL KLEGO
                        t1 = t1 + AKG*( - 1)**(ji + NNO(i))
                      ENDIF
                      sum1 = sum1 + t1*(a42 + (bc4*FOLac + bs4*FOLas)
     &                       *FOLar + bs24*FOLs2*folar1 + bgg4 + 
     &                       bgo4*foks1)
                      t1 = 0.
                      IF((ki + 4).EQ.kf)THEN
                        M1 = 2*ki
                        M2 = 8
                        CALL KLEGO
                        t1 = t1 + AKG
                      ENDIF
                      IF((ki - 4).EQ.kf)THEN
                        M1 = 2*ki
                        M2 = -8
                        CALL KLEGO
                        t1 = t1 + AKG
                      ENDIF
                      IF((4 - ki).EQ.kf)THEN
                        M1 = -2*ki
                        M2 = 8
                        CALL KLEGO
                        t1 = t1 + ( - 1)**ji*AKG
                      ENDIF
                      sum1 = sum1 + t1*(a44 + (cc4*FOLac + cs4*FOLas)
     &                       *FOLar + cs24*fls2*folar1 + cgg4 + 
     &                       cgo4*foks1)
                      sumx = sumx + sum1*AIT(i,jki)*AIT(jj,jkf)/di/df
                    ENDDO
                  ENDDO
                  IF(i.NE.jj)THEN
                    B4(i,jj) = sumx*sumx
                    IF(MEPri.LT.99)PRINT 1020
                    WRITE(21,1020)
                    IF(MEPri.LT.99)PRINT 1140, ES(i), ji, NTU(i), NNB(i)
     &                 , NNG(i), NNO(i), ES(jj), jf, NTU(jj), NNB(jj), 
     &                 NNG(jj), NNO(jj), B4(i,jj)
                    WRITE(21,1140)ES(i), ji, NTU(i), NNB(i), NNG(i), 
     &                            NNO(i), ES(jj), jf, NTU(jj), NNB(jj), 
     &                            NNG(jj), NNO(jj), B4(i,jj)
                    IF(MEPri.LT.99)PRINT 1020
                    WRITE(21,1020)
                    IF(MEPri.LT.99)PRINT 1130, flc2, fls2, FOLs2, 
     &                 folar1, foks1
                    WRITE(21,1130)flc2, fls2, FOLs2, folar1, foks1
 1130               FORMAT(5X,'I(CS**2)',D11.4,'I(SS**2)=',D11.4,
     &                     'I(SS(2G0))',D11.4,' FOLAR1=',D11.4,
     &                     ' FOKS1=',D11.4//)
 1140               FORMAT(5X,'B(E4:(',D11.4,')JI=',I2,'TAU=',I1,'NB=',
     &                     I1,'NG=',I1,'NO=',I1,'--->(',D11.4,')JF=',I2,
     &                     'TAU=',I1,'NB=',I1,'NG=',I1,'NO=',I1,')=',
     &                     D11.4)
                  ELSE
                    q4 = sumx
                    IF(MEPri.LT.99)PRINT 1150, ES(i), ji, NTU(i), NNB(i)
     &                 , NNG(i), NNO(i), q4
                    WRITE(21,1150)ES(i), ji, NTU(i), NNB(i), NNG(i), 
     &                            NNO(i), q4
 1150               FORMAT(10X,
     &                   'AVERAGE VALUE OF LAMBDA4 MOMENT FOT STATE  E='
     &                   ,D11.4,'JI=',I2,'TAU=',I1,'NB=',I1,'NG=',I1,
     &                   'NO=',I1,3X,'DEVIDED BY Q40=',D11.4)
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDDO
        ENDDO
        IF(MEHao.NE.0)THEN
          IF(MEPri.LT.99)PRINT 1060
          WRITE(21,1060)
C     CALCULATIONS OF E3- TRANSITION PROBABILITIES
          J2 = 6
          DO i = 1, NUR
            po1 = ( - 1)**NNO(i)
            DG1 = 0.D0
            IF(po1.NE.1)DG1 = GAMde
            kg1 = (3 - po1)/2
            ji = JU(i)
            ng1i = NNG(i) + 1
            ANG1 = ANG(ng1i,kg1)
            no1i = NNO(i) + 1
            aonu1 = ANO(no1i)
            CD1 = CD(ng1i,kg1)
            J1 = 2*ji
            kmi = 1 - ( - 1)**(ji + NNO(i))
            nki = (2*(ji/2) - kmi + 2)/2
            DO jj = 1, NUR
              jf = JU(jj)
              po2 = ( - 1)**NNO(jj)
              DG2 = 0.D0
              IF(po2.NE.1)DG2 = GAMde
              kg2 = (3 - po2)/2
              ng1f = NNG(jj) + 1
              ANG2 = ANG(ng1f,kg2)
              no1f = NNO(jj) + 1
              aonu2 = ANO(no1f)
              CD2 = CD(ng1f,kg2)
              J = 2*jf
              jif = IABS(ji - jf)
              IF(jif.LE.3)THEN
                IF(po1.NE.po2)THEN
                  IF(ji.NE.0.OR.jf.NE.0)THEN
                    IF(MEHao.NE.0)THEN
                      IF(MEHao.GE.2)THEN
                        ebm = EXP( - (BET3/AMUo)**2)
                        foks = BET3/SQRT(1.D0 - ebm**2)
                        IF(MEHao.GE.2)GOTO 15
                      ENDIF
                      NNT = 1
                      X1 = -BET3/AMUo
                      X2 = X1
                      P1 = BET3
                      P2 = BET3
                      ANU1 = aonu1
                      ANU2 = aonu2
                      IF(BET3.EQ.0.D0)CALL OVLAO
                      IF(BET3.NE.0.D0)CALL OVLAB
                      foks = FOLar
                    ENDIF
   15               foks = foks/BET3
                    IF(MEHao.EQ.2)foks = foks/BET0
                    IF(MEHam.GE.5)CALL TRLAG
                    NNT = 2
                    X1 = XI(i)
                    X2 = XI(jj)
                    P1 = PT(i)
                    P2 = PT(jj)
                    ANU1 = ANB(i)
                    ANU2 = ANB(jj)
                    IF(MEHam.NE.4)CALL OVLAB
                    IF(MEHam.EQ.4)FOLar = 1.
                    folar1 = FOLar
                    NNT = 1
                    IF(MEHam.NE.4)CALL OVLAB
                    IF(MEHam.EQ.4)FOLar = 1.
                    kmf = 1 - ( - 1)**(jf + NNO(jj))
                    nkf = (2*(jf/2) - kmf + 2)/2
                    sumx = 0.
                    DO jki = 1, nki
                      ki = kmi + 2*(jki - 1)
                      di = 1.
                      IF(ki.EQ.0)di = c1
                      DO jkf = 1, nkf
                        kf = kmf + 2*(jkf - 1)
                        sum1 = 0.D0
                        df = 1.D0
                        IF(kf.EQ.0)df = c1
                        M = 2*kf
                        IF(kf.EQ.ki)THEN
                          M1 = 2*ki
                          M2 = 0
                          CALL KLEGO
                          t1 = AKG
                          IF(kf.EQ.0)THEN
                            M1 = -2*ki
                            CALL KLEGO
                            t1 = t1 + AKG*( - 1)**(ji + NNO(i))
                          ENDIF
                          IF(MEHao.EQ.2)sum1 = sum1 + 
     &                       t1*((ac3*FOLac + as3*FOLas)
     &                       *folar1 + (ac00 + aog3)*FOLar)*foks*BET0
                          IF(MEHao.NE.2)THEN
                            sum1 = sum1 + 
     &                             t1*(ac00 + (ac3*FOLac + as3*FOLas)
     &                             *FOLar + aog3)*foks
                          ENDIF
                        ENDIF
                        t1 = 0.
                        IF((ki + 2).EQ.kf)THEN
                          M1 = 2*ki
                          M2 = 4
                          CALL KLEGO
                          t1 = AKG
                          t1 = t1 + AKG*( - 1)**(ji + NNO(i))
                        ENDIF
                        IF((ki - 2).EQ.kf)THEN
                          M1 = 2*ki
                          M2 = -4
                          CALL KLEGO
                          t1 = t1 + AKG
                        ENDIF
                        IF((2 - ki).EQ.kf)THEN
                          M1 = -2*ki
                          M2 = 4
                          CALL KLEGO
                          t1 = t1 + AKG*( - 1)**(ji + NNO(i))
                        ENDIF
                        IF(MEHao.EQ.2)sum1 = sum1 + 
     &                     t1*((bc3*FOLac + bs3*FOLas)
     &                     *folar1 + (bs00 + aog3)*FOLar)*foks*BET0
                        IF(MEHao.NE.2)THEN
                          sum1 = sum1 + 
     &                           t1*(bs00 + (bc3*FOLac + bs3*FOLas)
     &                           *FOLar + bog3)*foks
                        ENDIF
                        sumx = sumx + sum1*AIT(i,jki)*AIT(jj,jkf)/di/df
                      ENDDO
                    ENDDO
                    IF(i.EQ.jj)THEN
                      q3 = sumx
                      IF(MEPri.LT.99)PRINT 1160, ES(i), ji, NTU(i), 
     &                   NNB(i), NNG(i), NNO(i), q3
                      WRITE(21,1160)ES(i), ji, NTU(i), NNB(i), NNG(i), 
     &                              NNO(i), q3
 1160                 FORMAT(10X,
     &                   'AVERAGE VALUE OF LAMBDA3 MOMENT FOT STATE  E='
     &                   ,D11.4,'JI=',I2,'TAU=',I1,'NB=',I1,'NG=',I1,
     &                   'NO=',I1,3X,'DEVIDED BY Q30=',D11.4)
                    ENDIF
                    B3(i,jj) = sumx*sumx
                    IF(MEPri.LT.99)PRINT 1020
                    WRITE(21,1020)
                    IF(MEPri.LT.99)PRINT 1170, ES(i), ji, NTU(i), NNB(i)
     &                 , NNG(i), NNO(i), ES(jj), jf, NTU(jj), NNB(jj), 
     &                 NNG(jj), NNO(jj), B3(i,jj)
                    WRITE(21,1170)ES(i), ji, NTU(i), NNB(i), NNG(i), 
     &                            NNO(i), ES(jj), jf, NTU(jj), NNB(jj), 
     &                            NNG(jj), NNO(jj), B3(i,jj)
 1170               FORMAT(5X,'B(E3:(',D11.4,')JI=',I2,'TAU=',I1,'NB=',
     &                     I1,'NG=',I1,'NO=',I1,'--->(',D11.4,')JF=',I2,
     &                     'TAU=',I1,'NB=',I1,'NG=',I1,'NO=',I1,')=',
     &                     D11.4)
                    IF(MEPri.LT.99)PRINT 1180, foks
                    WRITE(21,1180)foks
 1180               FORMAT(5X,'FOKS=',D11.4/)
                  ENDIF
                ENDIF
              ENDIF
            ENDDO
          ENDDO
        ENDIF
      ENDIF
      STOP
      END SUBROUTINE TRANS
 
!---------------------------------------------------------------------------
C     *****************************************************************
      SUBROUTINE SHEMTEK
C     *****************************************************************
      IMPLICIT DOUBLE PRECISION(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8, DIMENSION(31,16) :: AIT, EIN
      REAL*8, DIMENSION(16,16) :: AM, TM
      REAL*8 :: AMB0, AMG0, AMO, AMUo, ANG1, ANG2, ANU, ANU1, ANU2, 
     &          BB32, BB42, BET0, BET3, BET4, BMO, CD1, CD2, CDV, CMO, 
     &          DELg, DET, DG1, DG2, DN, DNV, DPAr, EN, EPIt1, EPIt12, 
     &          EPIt2, ETO, FOLac, FOLag, FOLar, FOLas, GAM, GAM0, 
     &          GAMde, GAMg, GSHape, HW, HWO, P1, P2, PBEt2, PBEt3, X1, 
     &          X2, XIT, XIT1, Y
      REAL*8, DIMENSION(20) :: ANB, ANO, EGB, EL, EP0, EP1, EP12, EP2, 
     &                         EPB, EPG, ES, GIT, PT, XI
      REAL*8, DIMENSION(20,2) :: ANG, CD
      REAL*8, DIMENSION(10) :: BET, FOLb, FOLga, FOLgb, FOLks
      REAL*8, DIMENSION(2,8,8) :: FOG
      INTEGER :: IS, ITAu, KMI, LAS, MEApp, MECha, MECul, MEDis, MEHam, 
     &           MEHao, MEJob, MEPot, MEPri, MERel, MERip, MERrr, MERzz, 
     &           MESha, MESho, MESol, MEVol, NK, NMAx, NNT, NNTg, NO, 
     &           NPD, NROot, NUR
      INTEGER, DIMENSION(20) :: JU, NNB, NNG, NNO, NPI, NTU
      INTEGER, DIMENSION(25) :: NPJ
      COMMON /AA    / ANO
      COMMON /EIT   / EPIt1, EPIt2, EPIt12
      COMMON /ENA   / EN, EL, BET, NUR, NMAx, NPD, LAS
      COMMON /FUDN  / Y, DN, ANU, DNV, CDV
      COMMON /INRM  / AMO, BMO, CMO, BB42, GAMg, DELg
      COMMON /MAT   / GAM, IS, NO
      COMMON /MENU  / MEJob, MEPot, MEHam, MECha, MEPri, MESol, MESha, 
     &                MESho, MEHao, MEApp, MEVol, MERel, MECul, MERzz, 
     &                MERrr, MEDis, MERip
      COMMON /NP    / NPJ
      COMMON /OVLB  / X1, P1, ANU1, X2, P2, ANU2, FOLar, NNT
      COMMON /OVLG  / ANG1, ANG2, FOLag, FOLac, FOLas, CD1, CD2, DG1, 
     &                DG2, NNTg
      COMMON /PB    / PBEt2, PBEt3
      COMMON /SGB   / FOLgb, FOLb, FOLks, FOLga
      COMMON /SHAMO / BET3, ETO, AMUo, HWO, BB32, DPAr
      COMMON /SHEM1 / HW, AMG0, AMB0, GAM0, BET0, BET4, GAMde, GSHape
      COMMON /SHEM2 / GIT, EP0, EP1, EP2, EP12, ANG, EPB, EPG, EGB, AIT, 
     &                PT, FOG, XI, ANB, CD
      COMMON /SHEMM / ES, JU, NTU, NNB, NNG, NNO, NPI
      COMMON /VEC   / TM, AM, EIN, NK, KMI, ITAu
      COMMON /XI12  / XIT, XIT1, DET, NROot
C
C Local variables
C
      REAL*8 :: a1d, aa, ab1dm, ab1dp, abodm, abodp, ak, ak1, aka, aka1, 
     &          amb4, amo2, amom, amop, aod, ap, ap2, ap6, b1d, 
     &          bb320, bb420, bmom, bmop, bod, c1, c1d, cbg1, cmom, 
     &          cmop, cod, d, dd, debet, dpi, ebg1, edo, edo1, ee, egb1, 
     &          epib1, epibo1, epioo, g3, gamm, pi3, pit, pit0, sum1, 
     &          sum2
      INTEGER :: i, ino, j, jp1, ka, nb, ng, ngd, ngm, ngm1, ngmp, nom, 
     &           nom1
C
C*** End of declarations rewritten by SPAG
C
      amb4 = AMB0**4
      PBEt2 = 1.D0
      PBEt3 = 1.D0
C     PI3=1.0471975512D0
      pi3 = 4.D0/3.D0*ATAN(1.D0)
      c1 = SQRT(2.D0)
      amo2 = AMUo**2/2.
      IF(MEHao.NE.0)THEN
        nom = 0
        DO i = 1, NUR
          IF(nom.LT.NNO(i))nom = NNO(i)
        ENDDO
        nom1 = nom + 1
        XIT = -BET3/AMUo
        X1 = XIT
        X2 = XIT
        P1 = BET3
        P2 = BET3
        DO i = 1, nom1
          NROot = i - 1
          IF(BET3.EQ.0.)ANO(i) = 2*NROot + 1.D0
          IF(BET3.NE.0.)THEN
            CALL ANUDF
            ANO(i) = ANU
          ENDIF
          IF(MEHao.GE.1)THEN
            IF(MEHao.EQ.1)THEN
              ANU1 = ANU
              ANU2 = ANU
              IF(BET3.EQ.0.)THEN
                FOLb(i) = (2.D0*ANO(i) + 1.D0)*amo2
              ELSE
                NNT = 1
                CALL OVLAB
                FOLb(i) = (FOLar - BET3)/BET3
              ENDIF
            ELSE
              FOLb(i) = amo2*(1.D0 + ( - 1)**NO*EXP( - (BET3/AMUo)**2))
     &                  + BET3**2
            ENDIF
          ENDIF
        ENDDO
      ENDIF
      ngd = 2
      IF(MEHam.EQ.5.OR.MEHam.EQ.7)ngd = 0
      ngm = 0
      IF(MEHam.GT.4)THEN
        DO i = 1, NUR
          IF(ngm.LT.NNG(i))ngm = NNG(i)
        ENDDO
        gamm = GAM0
        ngmp = ngm + ngd + 1
        DO ino = 1, 2
          DO i = 1, ngmp
            NROot = i - 1
            XIT = -c1*GAM0/AMG0
            XIT1 = c1*(pi3 - GAM0)/AMG0
            IF(XIT1.GT.7.D0)THEN
              XIT = -GAM0/AMG0
              CALL ANUDF
              ANG(i,ino) = ANU
              CD(i,ino) = 0.D0
            ELSE
              CALL ANDET0
              ANG(i,ino) = ANU
              CD(i,ino) = CDV
            ENDIF
          ENDDO
          GAM0 = GAM0 + GAMde
        ENDDO
        GAM0 = gamm
        ngm1 = ngm + 1
        DO i = 1, ngm1
          jp1 = i + ngd
          ANG1 = ANG(i,1)
          CD1 = CD(i,1)
          DO j = i, jp1
            ANG2 = ANG(j,1)
            CD2 = CD(j,1)
            IF(MEHam.NE.5.AND.MEHam.NE.7)THEN
              NNTg = 1
              CALL OVLAG
              FOG(1,i,j) = FOLag
              FOG(1,j,i) = FOG(1,i,j)
              NNTg = 2
              CALL OVLAG
              FOG(2,i,j) = FOLag
              FOG(2,j,i) = FOG(2,i,j)
            ENDIF
            IF(i.EQ.j)THEN
              IF(MEHam.EQ.7)THEN
                NNTg = 11
                CALL OVLAG
                FOLgb(i) = FOLag
              ENDIF
            ENDIF
          ENDDO
        ENDDO
        GAM = GAM0
        IF(BET3.EQ.0.D0.OR.MEHao.EQ.2)PBEt3 = 0.
        IF(MESha.NE.1)THEN
          bb420 = BB42
          debet = 0.001D0*bb420
          BB42 = bb420 + debet
          CALL INERMO
          amop = AMO
          bmop = BMO
          cmop = CMO
          BB42 = bb420 - debet
          CALL INERMO
          amom = AMO
          bmom = BMO
          cmom = CMO
          BB42 = bb420
          a1d = (amop - amom)/debet/2.D0
          b1d = (bmop - bmom)/debet/2.D0
          c1d = (cmop - cmom)/debet/2.D0
          ab1dm = a1d - b1d
          ab1dp = a1d + b1d
        ENDIF
        IF(MESho.NE.0)THEN
          bb320 = BB32
          IF(BET3.EQ.0.OR.MEHao.EQ.2)PBEt3 = 0.001D0/BB32*GAM**2
          IF(BET3.NE.0.AND.MEHao.NE.2)THEN
            debet = 0.001D0*bb320
            BB32 = bb320 + debet
          ENDIF
          CALL INERMO
          amop = AMO
          bmop = BMO
          cmop = CMO
          IF(BET3.EQ.0.D0.OR.MEHao.EQ.2)PBEt3 = 0.D0
          IF(BET3.NE.0.D0.AND.MEHao.NE.2)THEN
            BB32 = bb320 - debet
          ENDIF
          CALL INERMO
          amom = AMO
          bmom = BMO
          cmom = CMO
          BB32 = bb320
          IF(BET3.EQ.0.D0.OR.MEHao.EQ.2)debet = 0.0005D0*GAM**2
          aod = (amop - amom)/debet/2.D0
          bod = (bmop - bmom)/debet/2.D0
          cod = (cmop - cmom)/debet/2.D0
          abodm = aod - bod
          abodp = aod + bod
          IF(BET3.EQ.0.D0.OR.MEHao.EQ.2)PBEt3 = 0.D0
          gamm = GAM0
        ENDIF
      ENDIF
      DO i = 1, NUR
        IS = JU(i)
        ITAu = NTU(i)
        ng = NNG(i)
        nb = NNB(i)
        NO = NNO(i)
        NROot = ng
        GAM = GAM0
        IF(( - 1)**NO.NE.1)GAM = GAM + GAMde
        CALL MATAM
        EP0(i) = EIN(IS + 1,ITAu)
        IF(MEHam.GT.4)THEN
          CALL EIT12
          GIT(i) = GAM
          EP1(i) = EPIt1
          EP2(i) = EPIt2
          EP12(i) = EPIt12
        ENDIF
        DO j = 1, NK
          AIT(i,j) = TM(j,ITAu)
        ENDDO
        IF(MEHam.EQ.4)EGB(i) = EP0(i)
        IF(MEHam.EQ.4)CYCLE
        IF(MEHam.NE.3)THEN
          ANU = ANG(ng + 1,(3 - (-1)**NO)/2)
          EPG(i) = (ANU + 0.5D0)*2.D0/AMG0**2 + EPIt1 + EPIt2 + EPIt12
          ee = EP0(i) + EPG(i) - EPG(1)
        ENDIF
        IF(MEHam.EQ.3)ee = EP0(i)
        IF(MEHao.NE.0)THEN
          edo = DPAr*( - 1)**(NO + 1)
          IF(i.EQ.1)edo1 = edo
          IF(MEHao.EQ.2)ee = ee + edo - edo1
        ENDIF
        aa = amb4*ee
        pit0 = 1.D0
        dpi = 100.D0
        IF(aa.EQ.0.D0)THEN
          pit = 1.D0
        ELSE
    5     pit = pit0 + dpi
          IF((pit - 1.)*pit**3.LT.aa)THEN
            pit0 = pit
            GOTO 5
          ELSEIF((pit - 1.)*pit**3.NE.aa)THEN
            dpi = dpi/5.D0
            IF(dpi.GT.1.D-8)GOTO 5
          ENDIF
        ENDIF
        PT(i) = pit
        XIT = -pit/AMB0*(4.D0 - 3.D0/pit)**0.25D0
        XI(i) = XIT
        NROot = nb
        ap = AMB0/pit
        ap2 = ap*ap
        ap6 = ap2*ap2*ap2
        CALL ANUDF
        ANB(i) = ANU
        epibo1 = 0.D0
        P1 = pit
        P2 = pit
        X1 = XIT
        X2 = XIT
        ANU1 = ANU
        ANU2 = ANU
        IF(MESha.NE.1.OR.MESho.NE.0)THEN
          sum1 = 0.D0
          sum2 = 0.D0
          DO j = 1, NK
            ak = TM(j,ITAu)
            aka = TM(j,ITAu)
            ka = KMI + (j - 1)*2
            sum1 = sum1 + ak*aka*ka*ka
            d = 1.D0
            dd = 0.D0
            IF(ka.EQ.0)dd = 1.D0
            IF(ka.EQ.0)d = SQRT(2.D0)
            IF(j.NE.NK)THEN
              ak1 = TM(j + 1,ITAu)
              aka1 = TM(j + 1,ITAu)
              sum2 = sum2 + (ak1*aka + ak*aka1)
     &               *(1.D0 + ( - 1)**(IS + NO)*dd)
     &               /d*SQRT((IS + ka + 2.D0)*(IS + ka + 1.D0)
     &               *(IS - ka - 1.D0)*(IS - ka))
            ENDIF
          ENDDO
          IF(MESho.EQ.0)GOTO 10
          epioo = abodp/8.D0*IS*(IS + 1) + (0.25D0*cod - abodp/8.D0)
     &            *sum1 + abodm/16.D0*sum2
          IF(MEHao.GE.1)THEN
            NNT = 13
            CALL OVLAB
            epibo1 = HW*BB32*AMB0**2*FOLar*epioo*FOLb(NO + 1)
          ENDIF
          IF(BET3.NE.0.D0)THEN
            NNT = 10
            CALL OVLAB
            IF(MEHao.NE.2)THEN
              epibo1 = -HW*BB32*FOLar*AMB0**2*epioo + epibo1
            ENDIF
          ENDIF
        ENDIF
        IF(MESha.EQ.1)epib1 = 0.D0
        IF(MESha.EQ.1)GOTO 20
        IF(MESho.GT.0)GOTO 15
   10   NNT = 10
        CALL OVLAB
   15   epib1 = ab1dp/8.D0*IS*(IS + 1) + (0.25D0*c1d - ab1dp/8.D0)
     &          *sum1 + ab1dm/16.D0*sum2
        epib1 = epib1*FOLar*AMB0**2*BB42*HW
   20   EPB(i) = epib1
        EGB(i) = HW*((ANU + 0.5D0)*SQRT(4.D0 - 3.D0/pit)
     &           + 0.5D0*ap2*ee + 0.5D0*ap6*ee*ee) - epib1 + epibo1
        IF(MEHao.EQ.1)EGB(i) = EGB(i) + HWO*(ANO(NO + 1) + 0.5D0)
        IF(MEHam.EQ.7)THEN
          g3 = 3.D0*GAM0
          cbg1 = 1.D0/AMG0**4 + 
     &           81.D0/4.D0*(1.D0/SIN(g3)**2 + 3.D0*COS(g3)**2/SIN(g3)
     &           **4)
          NNT = 11
          X1 = XIT
          P1 = pit
          X2 = XIT
          P2 = pit
          ANU1 = ANU
          ANU2 = ANU
          CALL OVLAB
          ebg1 = HW*AMB0**2/4.D0*BET0**2*cbg1*FOLar*FOLgb(ng + 1)
     &           *AMG0**2
          EGB(i) = EGB(i) + ebg1
        ENDIF
      ENDDO
      egb1 = EGB(1)
      DO i = 1, NUR
        EGB(i) = EGB(i) - egb1
      ENDDO
      RETURN
      END SUBROUTINE SHEMTEK
 
!---------------------------------------------------------------------------
C     *****************************************************************
      SUBROUTINE SHEM
C     *****************************************************************
      IMPLICIT DOUBLE PRECISION(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8, DIMENSION(31,16) :: AIT, EIN
      REAL*8, DIMENSION(16,16) :: AM, TM
      REAL*8 :: AMB0, AMG0, AMO, AMUo, ANG1, ANG2, ANU, ANU1, ANU2, 
     &          BB32, BB42, BET0, BET3, BET4, BMO, CD1, CD2, CDV, CMO, 
     &          DELg, DET, DG1, DG2, DN, DNV, DPAr, EN, EPIt1, EPIt12, 
     &          EPIt2, ETO, FOLac, FOLag, FOLar, FOLas, GAM, GAM0, 
     &          GAMde, GAMg, GSHape, HW, HWO, P1, P2, PBEt2, PBEt3, X1, 
     &          X2, XIT, XIT1, Y
      REAL*8, DIMENSION(20) :: ANB, ANO, EGB, EL, EP0, EP1, EP12, EP2, 
     &                         EPB, EPG, ES, GIT, PT, XI
      REAL*8, DIMENSION(20,2) :: ANG, CD
      REAL*8, DIMENSION(10) :: BET, FOLb, FOLga, FOLgb, FOLks
      REAL*8, DIMENSION(2,8,8) :: FOG
      INTEGER :: IS, ITAu, KMI, LAS, MEApp, MECha, MECul, MEDis, MEHam, 
     &           MEHao, MEJob, MEPot, MEPri, MERel, MERip, MERrr, MERzz, 
     &           MESha, MESho, MESol, MEVol, NK, NMAx, NNT, NNTg, NO, 
     &           NPD, NROot, NUR
      INTEGER, DIMENSION(20) :: JU, NNB, NNG, NNO, NPI, NTU
      INTEGER, DIMENSION(25) :: NPJ
      COMMON /AA    / ANO
      COMMON /EIT   / EPIt1, EPIt2, EPIt12
      COMMON /ENA   / EN, EL, BET, NUR, NMAx, NPD, LAS
      COMMON /FUDN  / Y, DN, ANU, DNV, CDV
      COMMON /INRM  / AMO, BMO, CMO, BB42, GAMg, DELg
      COMMON /MAT   / GAM, IS, NO
      COMMON /MENU  / MEJob, MEPot, MEHam, MECha, MEPri, MESol, MESha, 
     &                MESho, MEHao, MEApp, MEVol, MERel, MECul, MERzz, 
     &                MERrr, MEDis, MERip
      COMMON /NP    / NPJ
      COMMON /OVLB  / X1, P1, ANU1, X2, P2, ANU2, FOLar, NNT
      COMMON /OVLG  / ANG1, ANG2, FOLag, FOLac, FOLas, CD1, CD2, DG1, 
     &                DG2, NNTg
      COMMON /PB    / PBEt2, PBEt3
      COMMON /SGB   / FOLgb, FOLb, FOLks, FOLga
      COMMON /SHAMO / BET3, ETO, AMUo, HWO, BB32, DPAr
      COMMON /SHEM1 / HW, AMG0, AMB0, GAM0, BET0, BET4, GAMde, GSHape
      COMMON /SHEM2 / GIT, EP0, EP1, EP2, EP12, ANG, EPB, EPG, EGB, AIT, 
     &                PT, FOG, XI, ANB, CD
      COMMON /SHEMM / ES, JU, NTU, NNB, NNG, NNO, NPI
      COMMON /VEC   / TM, AM, EIN, NK, KMI, ITAu
      COMMON /XI12  / XIT, XIT1, DET, NROot
C
C Local variables
C
      REAL*8 :: a1d, aa, ab1dm, ab1dp, abodm, abodp, ak, ak1, aka, aka1, 
     &          amb4, amo2, amom, amop, aod, ap, ap2, ap6, b1d, 
     &          bb320, bb420, bmom, bmop, bod, c1, c1d, cbg1, cmom, 
     &          cmop, cod, d, dd, debet, dpi, ebg1, ebm, edo, edo1, ee, 
     &          egb1, epib1, epibo1, epioo, fol, g3, gamm, pi, pi3, pit, 
     &          pit0, sqpi, sum1, sum2
      REAL*8 :: ERFC2
      INTEGER :: i, ino, j, jp1, ka, nb, ng, ngd, ngm, ngm1, ngmp, nom, 
     &           nom1
C
C*** End of declarations rewritten by SPAG
C
      pi = 4.D0*ATAN(1.D0)
      sqpi = SQRT(pi)
      amb4 = AMB0**4
      PBEt2 = 1.D0
      PBEt3 = 1.D0
C     PI3=1.0471975512D0
      pi3 = pi/3.D0
      c1 = SQRT(2.D0)
      amo2 = AMUo**2/2.D0
      IF(MEHao.NE.0)THEN
        nom = 0
        DO i = 1, NUR
          IF(nom.LT.NNO(i))nom = NNO(i)
        ENDDO
        nom1 = nom + 1
        XIT = -BET3/AMUo
        X1 = XIT
        X2 = XIT
        P1 = BET3
        P2 = BET3
        DO i = 1, nom1
          NROot = i - 1
          IF(BET3.EQ.0.D0)ANO(i) = 2*NROot + 1.
          IF(BET3.NE.0.D0)THEN
            CALL ANUDF
            ANO(i) = ANU
          ENDIF
          IF(MEHao.EQ.1)THEN
            ANU1 = ANU
            ANU2 = ANU
            IF(BET3.EQ.0.D0)THEN
              FOLb(i) = (2.D0*ANO(i) + 1.D0)*amo2
            ELSE
              NNT = 1
              CALL OVLAB
              FOLb(i) = (FOLar - BET3)/BET3
            ENDIF
          ELSE
            ebm = EXP( - XIT**2)
            fol = ebm*( - 1.D0/XIT/sqpi*(1 - (-1)**i) + ( - 1)**i)
     &            - ERFC2( - XIT)
            FOLb(i) = fol/(1 - ( - 1)**i*ebm)
          ENDIF
        ENDDO
      ENDIF
      ngd = 2
      IF(MEHam.EQ.5.OR.MEHam.EQ.7)ngd = 0
      ngm = 0
      IF(MEHam.GT.4)THEN
        DO i = 1, NUR
          IF(ngm.LT.NNG(i))ngm = NNG(i)
        ENDDO
        gamm = GAM0
        ngmp = ngm + ngd + 1
        DO ino = 1, 2
          DO i = 1, ngmp
            NROot = i - 1
            XIT = -c1*GAM0/AMG0
            XIT1 = c1*(pi3 - GAM0)/AMG0
            IF(XIT1.GT.7.D0)THEN
              XIT = -GAM0/AMG0
              CALL ANUDF
              ANG(i,ino) = ANU
              CD(i,ino) = 0.D0
            ELSE
              CALL ANDET0
              ANG(i,ino) = ANU
              CD(i,ino) = CDV
            ENDIF
          ENDDO
          GAM0 = GAM0 + GAMde
        ENDDO
        GAM0 = gamm
        ngm1 = ngm + 1
        DO i = 1, ngm1
          jp1 = i + ngd
          ANG1 = ANG(i,1)
          CD1 = CD(i,1)
          DO j = i, jp1
            ANG2 = ANG(j,1)
            CD2 = CD(j,1)
            IF(MEHam.NE.5.AND.MEHam.NE.7)THEN
              NNTg = 1
              CALL OVLAG
              FOG(1,i,j) = FOLag
              FOG(1,j,i) = FOG(1,i,j)
              NNTg = 2
              CALL OVLAG
              FOG(2,i,j) = FOLag
              FOG(2,j,i) = FOG(2,i,j)
            ENDIF
            IF(i.EQ.j)THEN
              IF(MEHam.EQ.7)THEN
                NNTg = 11
                CALL OVLAG
                FOLgb(i) = FOLag
              ENDIF
            ENDIF
          ENDDO
        ENDDO
        GAM = GAM0
        IF(BET3.EQ.0.)PBEt3 = 0.D0
        IF(MESha.NE.1)THEN
          bb420 = BB42
          debet = 0.001D0*bb420
          BB42 = bb420 + debet
          CALL INERMO
          amop = AMO
          bmop = BMO
          cmop = CMO
          BB42 = bb420 - debet
          CALL INERMO
          amom = AMO
          bmom = BMO
          cmom = CMO
          BB42 = bb420
          a1d = (amop - amom)/debet/2.D0
          b1d = (bmop - bmom)/debet/2.D0
          c1d = (cmop - cmom)/debet/2.D0
          ab1dm = a1d - b1d
          ab1dp = a1d + b1d
        ENDIF
        IF(MESho.NE.0)THEN
          bb320 = BB32
          IF(BET3.EQ.0.D0)PBEt3 = 0.001D0/BB32*GAM**2
          IF(BET3.NE.0.D0)THEN
            debet = 0.001*bb320
            BB32 = bb320 + debet
          ENDIF
          CALL INERMO
          amop = AMO
          bmop = BMO
          cmop = CMO
          IF(BET3.EQ.0.D0)PBEt3 = 0.D0
          IF(BET3.NE.0.D0)THEN
            BB32 = bb320 - debet
          ENDIF
          CALL INERMO
          amom = AMO
          bmom = BMO
          cmom = CMO
          BB32 = bb320
          IF(BET3.EQ.0.D0)debet = 0.0005D0*GAM**2
          aod = (amop - amom)/debet/2.D0
          bod = (bmop - bmom)/debet/2.D0
          cod = (cmop - cmom)/debet/2.D0
          abodm = aod - bod
          abodp = aod + bod
          IF(BET3.EQ.0.)PBEt3 = 0.D0
          gamm = GAM0
        ENDIF
      ENDIF
      DO i = 1, NUR
        IS = JU(i)
        ITAu = NTU(i)
        ng = NNG(i)
        nb = NNB(i)
        NO = NNO(i)
        NROot = ng
        GAM = GAM0
        IF(( - 1)**NO.NE.1)GAM = GAM + GAMde
        CALL MATAM
        EP0(i) = EIN(IS + 1,ITAu)
        IF(MEHam.GT.4)THEN
          CALL EIT12
          GIT(i) = GAM
          EP1(i) = EPIt1
          EP2(i) = EPIt2
          EP12(i) = EPIt12
        ENDIF
        DO j = 1, NK
          AIT(i,j) = TM(j,ITAu)
        ENDDO
        IF(MEHam.EQ.4)EGB(i) = EP0(i)
        IF(MEHam.EQ.4)CYCLE
        IF(MEHam.NE.3)THEN
          ANU = ANG(ng + 1,(3 - (-1)**NO)/2)
          EPG(i) = (ANU + 0.5D0)*2./AMG0**2 + EPIt1 + EPIt2 + EPIt12
          ee = EP0(i) + EPG(i) - EPG(1)
        ENDIF
        IF(MEHam.EQ.3)ee = EP0(i)
        IF(MEHao.NE.0)THEN
          edo = DPAr*( - 1)**(NO + 1)
          IF(i.EQ.1)edo1 = edo
          IF(MEHao.EQ.2)ee = ee + edo - edo1
        ENDIF
        aa = amb4*ee
        pit0 = 1.D0
        dpi = 100.D0
        IF(aa.EQ.0.D0)THEN
          pit = 1.
        ELSE
    5     pit = pit0 + dpi
          IF((pit - 1.D0)*pit**3.LT.aa)THEN
            pit0 = pit
            GOTO 5
          ELSEIF((pit - 1.D0)*pit**3.NE.aa)THEN
            dpi = dpi/5.
            IF(dpi.GT.1.D-8)GOTO 5
          ENDIF
        ENDIF
        PT(i) = pit
        XIT = -pit/AMB0*(4.D0 - 3.D0/pit)**0.25D0
        XI(i) = XIT
        NROot = nb
        ap = AMB0/pit
        ap2 = ap*ap
        ap6 = ap2*ap2*ap2
        CALL ANUDF
        ANB(i) = ANU
        epibo1 = 0.D0
        P1 = pit
        P2 = pit
        X1 = XIT
        X2 = XIT
        ANU1 = ANU
        ANU2 = ANU
        IF(MESha.NE.1.OR.MESho.NE.0)THEN
          sum1 = 0.D0
          sum2 = 0.D0
          DO j = 1, NK
            ak = TM(j,ITAu)
            aka = TM(j,ITAu)
            ka = KMI + (j - 1)*2
            sum1 = sum1 + ak*aka*ka*ka
            d = 1.D0
            dd = 0.D0
            IF(ka.EQ.0)dd = 1.D0
            IF(ka.EQ.0)d = SQRT(2.D0)
            IF(j.NE.NK)THEN
              ak1 = TM(j + 1,ITAu)
              aka1 = TM(j + 1,ITAu)
              sum2 = sum2 + (ak1*aka + ak*aka1)
     &               *(1.D0 + ( - 1)**(IS + NO)*dd)
     &               /d*SQRT((IS + ka + 2.D0)*(IS + ka + 1.D0)
     &               *(IS - ka - 1.D0)*(IS - ka))
            ENDIF
          ENDDO
          IF(MESho.EQ.0)GOTO 10
          epioo = abodp/8.D0*IS*(IS + 1) + (0.25D0*cod - abodp/8.D0)
     &            *sum1 + abodm/16.D0*sum2
          IF(MEHao.GE.1)THEN
            NNT = 13
            CALL OVLAB
            epibo1 = HW*BB32*AMB0**2*FOLar*epioo*FOLb(NO + 1)
          ENDIF
          NNT = 10
          CALL OVLAB
          IF(BET3.NE.0.D0)THEN
            epibo1 = -HW*BB32*FOLar*AMB0**2*epioo + epibo1
          ENDIF
        ENDIF
        IF(MESha.EQ.1)epib1 = 0.D0
        IF(MESha.EQ.1)GOTO 20
        IF(MESho.GT.0)GOTO 15
   10   NNT = 10
        CALL OVLAB
   15   epib1 = ab1dp/8.D0*IS*(IS + 1) + (0.25D0*c1d - ab1dp/8.D0)
     &          *sum1 + ab1dm/16.D0*sum2
        epib1 = epib1*FOLar*AMB0**2*BB42*HW
   20   EPB(i) = epib1
        EGB(i) = HW*((ANU + 0.5D0)*SQRT(4. - 3./pit) + 0.5*ap2*ee + 
     &           0.5*ap6*ee*ee) - epib1 + epibo1
        IF(MEHao.EQ.1)EGB(i) = EGB(i) + HWO*(ANO(NO + 1) + 0.5D0)
        IF(MEHao.EQ.3)EGB(i) = EGB(i) + edo
        IF(MEHam.EQ.7)THEN
          g3 = 3.D0*GAM0
          cbg1 = 1.D0/AMG0**4 + 
     &           81.D0/4.D0*(1.D0/SIN(g3)**2 + 3.D0*COS(g3)**2/SIN(g3)
     &           **4)
          NNT = 11
          X1 = XIT
          P1 = pit
          X2 = XIT
          P2 = pit
          ANU1 = ANU
          ANU2 = ANU
          CALL OVLAB
          ebg1 = HW*AMB0**2/4.0*BET0**2*cbg1*FOLar*FOLgb(ng + 1)*AMG0**2
          EGB(i) = EGB(i) + ebg1
        ENDIF
      ENDDO
      egb1 = EGB(1)
      DO i = 1, NUR
        EGB(i) = EGB(i) - egb1
      ENDDO
      RETURN
      END SUBROUTINE SHEM
 
!---------------------------------------------------------------------------
C **********************************************************************
      FUNCTION ERFC2(Xx)
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      REAL*8 :: Xx
      REAL*8 :: ERFC2
C
C Local variables
C
      REAL*8 :: a1, a2, a3, a4, a5, p, p1, r1, r2, r3, r4, r5
C
C*** End of declarations rewritten by SPAG
C
C **********************************************************************
C     Abramowitz & Stegun, p.299, Eq. 7.1.26 (Error < 1.5E-7)
C
      DATA p, a1, a2, a3, a4, a5/0.3275911D0, 0.254829592D0, 
     &     -0.284496736D0, 1.421413741D0, -1.453152027D0, 1.061405429D0/
      p1 = EXP( - Xx*Xx)
      r1 = 1.D0/(1. + p*Xx)
      r2 = r1*r1
      r3 = r2*r1
      r4 = r3*r1
      r5 = r4*r1
      ERFC2 = (a5*r5 + a4*r4 + a3*r3 + a2*r2 + a1*r1)*p1
      RETURN
      END FUNCTION ERFC2
 
!---------------------------------------------------------------------------
C     *******************************************************
C     END of shemsofd
C     *******************************************************
C     START of dispersive
C     *******************************************************
C==========================================================================
C     AUTHOR: Dr. Roberto Capote Noy
C
C     e-mail: r.capotenoy@iaea.org ; rcapotenoy@yahoo.com;
C
C     DISPERSIVE OPTICAL MODEL POTENTIAL PACKAGE
C
C     Analytical dispersive integrals are included
C     see Quesada JM, Capote R et al,
C             Computer Physics Communications 153(2003) 97
C             Phys. Rev. C67(2003) 067601
C
C     Dispersive integral's derivatives calculated by Dr.J.M.Quesada
C
      FUNCTION DOM_INT_WV(Ef,Ep,Av,Bv,N,Einc,Derivintwv)
C
C     Analytical dispersive integral and its derivative for
C     Wv(E)=Av*(E-Ep)**n/( (E-Ep)**n + Bv**n )  for E>Ep
C     Wv(E)=Wv(2*Ef-E)                          for E<2Ef-Ep
C     Wv(E)=0                                     OTHERWISE
C
      IMPLICIT NONE
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      REAL*8 :: Av, Bv, Derivintwv, Ef, Einc, Ep
      INTEGER :: N
      REAL*8 :: DOM_INT_WV
C
C Local variables
C
      REAL*8 :: DBLE
      REAL*8 :: deremin, dereplus, e, e0, emin, eplus, ex, pi, rds, 
     &          resemin, reseplus, rs
      COMPLEX*16 :: ds, fs, i, pj, zj, ztmp
      INTEGER :: is, j
      REAL :: REAL
C
C*** End of declarations rewritten by SPAG
C
 
 
      DATA i/(0.D0,1.D0)/
 
      pi = 4.D0*ATAN(1.D0)
 
      is = 1
      e = Einc
      IF(Einc.LE.Ef)THEN
        e = 2.D0*Ef - Einc
C       Odd function
        is = -1
      ENDIF
 
      e0 = Ep - Ef
      ex = e - Ef
      eplus = ex + e0
      emin = ex - e0
      DOM_INT_WV = 0.D0
      Derivintwv = 0.D0
 
      resemin = emin**N/(emin**N + Bv**N)
 
      deremin = emin**(N - 1)*(emin**N + Bv**N*(1.D0 + N*LOG(ABS(emin)))
     &          )/(emin**N + Bv**N)**2
 
      reseplus = -eplus**N/(eplus**N + Bv**N)
 
      dereplus = -eplus**(N - 1)
     &           *(eplus**N + Bv**N*(1.D0 + N*LOG(eplus)))
     &           /(eplus**N + Bv**N)**2
 
C----------------------------------
C     Complex arithmetic follows
C
      fs = (0.D0,0.D0)
      ds = (0.D0,0.D0)
      DO j = 1, N
        ztmp = i*(2*j - 1)/DBLE(N)*pi
        pj = Bv*EXP(ztmp)
        zj = pj*(2*pj + eplus - emin)*ex
        zj = zj/((pj + e0)*(pj + eplus)*(pj - emin))
        fs = fs + zj*LOG( - pj)
        ds = ds + 2*pj*(ex*ex + (pj + e0)**2)*LOG( - pj)
     &       /((pj + eplus)**2*(pj - emin)**2)
      ENDDO
 
      IF(ABS(IMAG(fs)).GT.1.D-4)STOP 'Too big imag part in Wv'
      IF(ABS(IMAG(ds)).GT.1.D-4)STOP 'Too big imag deriv in Wv'
      rs = REAL(fs)
      rds = REAL(ds)
C----------------------------------
 
      DOM_INT_WV = -Av/pi*is*(rs/N + reseplus*LOG(eplus)
     &             + resemin*LOG(ABS(emin)))
 
C     Sign of derivative changed
C     DerivIntWv = -Av/pi*IS*( Rds/n + DerEplus + DerEmin)
      Derivintwv = Av/pi*is*(rds/N + dereplus + deremin)
 
      RETURN
      END FUNCTION DOM_INT_WV
 
!---------------------------------------------------------------------------
 
      FUNCTION DOM_INT_WS(Ef,Ep,As,Bs,Cs,M,Einc,Derivintws)
C
C     Analytical dispersive integral and its derivative for
C     Ws(E)=As*(E-Ep)**m/( (E-Ep)**m + Bs**m ) * exp(-Cs*(E-Ep)) for E>Ep
C     Ws(E)=Ws(2*Ef-E)                                           for E<2Ef-Ep
C     Ws(E)=0                                                    OTHERWISE
C
      IMPLICIT NONE
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      REAL*8 :: As, Bs, Cs, Derivintws, Ef, Einc, Ep
      INTEGER :: M
      REAL*8 :: DOM_INT_WS
C
C Local variables
C
      REAL*8 :: DBLE
      REAL*8 :: deremin, dereplus, e, e0, emin, eplus, ex, pi, rds, 
     &          resemin, reseplus, rs
      COMPLEX*16 :: ds, fs, i, pj, zj, ztmp
      REAL*8 :: EIN
      INTEGER :: is, j
      REAL :: REAL
      COMPLEX*16 :: ZFI
C
C*** End of declarations rewritten by SPAG
C
 
      DATA i/(0.D0,1.D0)/
 
      pi = 4.D0*ATAN(1.D0)
 
      is = 1
      e = Einc
      IF(Einc.LE.Ef)THEN
        e = 2.D0*Ef - Einc
C       Odd function
        is = -1
      ENDIF
 
      e0 = Ep - Ef
      ex = e - Ef
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
C
      fs = (0.D0,0.D0)
      ds = (0.D0,0.D0)
      DO j = 1, M
        ztmp = i*(2*j - 1)/DBLE(M)*pi
        pj = Bs*EXP(ztmp)
        zj = pj*(2*pj + eplus - emin)*ex
        zj = zj/(pj + e0)/(pj + eplus)/(pj - emin)
        fs = fs + zj*ZFI( - pj*Cs)
        ds = ds + 2*pj*(ex*ex + (pj + e0)**2)*ZFI( - pj*Cs)
     &       /((pj + eplus)**2*(pj - emin)**2)
      ENDDO
 
      IF(ABS(IMAG(fs)).GT.1.D-4)STOP 'Too big imag part in Ws'
      IF(ABS(IMAG(ds)).GT.1.D-4)STOP 'Too big imag deriv in Ws'
      rs = REAL(fs)
      rds = REAL(ds)
C----------------------------------
 
      DOM_INT_WS = As/pi*is*(rs/M - reseplus*EXP(Cs*eplus)
     &             *EIN( - Cs*eplus) - resemin*EXP( - Cs*emin)
     &             *EIN(Cs*emin))
C     Sign of derivative changed
C     DerivIntWs =  As/pi*IS*( Rds/m + DerEplus + DerEmin)
      Derivintws = -As/pi*is*(rds/M + dereplus + deremin)
 
      RETURN
      END FUNCTION DOM_INT_WS
 
!---------------------------------------------------------------------------
 
      FUNCTION WV(A,B,Ep,Ef,E,N)
      IMPLICIT NONE
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      REAL*8 :: A, B, E, Ef, Ep
      INTEGER :: N
      REAL*8 :: WV
C
C Local variables
C
      REAL*8 :: ee
C
C*** End of declarations rewritten by SPAG
C
 
      WV = 0.D0
      IF(E.LE.Ef)E = 2.D0*Ef - E
      IF(E.LT.Ep)RETURN
 
      ee = (E - Ep)**N
      WV = A*ee/(ee + B**N)
 
      RETURN
      END FUNCTION WV
 
!---------------------------------------------------------------------------
 
      FUNCTION WDD(A,B,C,Ep,Ef,E,M)
      IMPLICIT NONE
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      REAL*8 :: A, B, C, E, Ef, Ep
      INTEGER :: M
      REAL*8 :: WDD
C
C Local variables
C
      REAL*8 :: arg, ee
C
C*** End of declarations rewritten by SPAG
C
 
      WDD = 0.D0
      IF(E.LE.Ef)E = 2.D0*Ef - E
      IF(E.LT.Ep)RETURN
 
      arg = C*(E - Ep)
      IF(arg.GT.15)RETURN
      ee = (E - Ep)**M
      WDD = A*ee/(ee + B**M)*EXP( - arg)
      RETURN
      END FUNCTION WDD
 
!---------------------------------------------------------------------------
 
      FUNCTION DOM_INT_T1(Ef,Ea,E)
C
C     Integral over E' corresponding to nonlocal additions T1(E'<<0)
C
      IMPLICIT NONE
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      REAL*8 :: E, Ea, Ef
      REAL*8 :: DOM_INT_T1
C
C Local variables
C
      REAL*8 :: ea2, eax, ex, pi, t11, t12, t13
C
C*** End of declarations rewritten by SPAG
C
 
      pi = 4.D0*ATAN(1.D0)
 
      ex = E - Ef
      ea2 = Ea**2
      eax = ex + Ea
 
      t11 = 0.5D0*LOG(Ea)/ex
      t12 = ((2*Ea + ex)*LOG(Ea) + 0.5D0*pi*ex)/(2.*(eax**2 + ea2))
      t13 = -eax**2*LOG(eax)/(ex*(eax**2 + ea2))
 
      DOM_INT_T1 = ex/pi*(t11 + t12 + t13)
C
      RETURN
      END FUNCTION DOM_INT_T1
 
!---------------------------------------------------------------------------
C
      FUNCTION DOM_INT_T2(Ef,Ea,E)
C
C     Integral over E' corresponding to nonlocal additions T2(E'>>0)
C
      IMPLICIT NONE
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      REAL*8 :: E, Ea, Ef
      REAL*8 :: DOM_INT_T2
C
C Local variables
C
      REAL*8 :: el, pi
C
C*** End of declarations rewritten by SPAG
C
 
      pi = 4.D0*ATAN(1.D0)
      el = Ef + Ea
      DOM_INT_T2 = 1.D0/pi*(SQRT(ABS(Ef))
     &             *ATAN((2*SQRT(el*ABS(Ef)))/(el-ABS(Ef)))
     &             + el**1.5D0/(2*Ef)*LOG(Ea/el))
 
      IF(E.GT.el)THEN
 
        DOM_INT_T2 = DOM_INT_T2 + 
     &               1.D0/pi*(SQRT(E)*LOG((SQRT(E)+SQRT(el))
     &               /(SQRT(E)-SQRT(el))) + 1.5D0*SQRT(el)
     &               *LOG((E-el)/Ea) + el**1.5D0/(2*E)*LOG(el/(E-el)))
 
      ELSEIF(E.EQ.el)THEN
 
        DOM_INT_T2 = DOM_INT_T2 + 1.D0/pi*1.5D0*SQRT(el)
     &               *LOG((2**(4.D0/3.D0)*el)/Ea)
 
      ELSEIF(E.GT.0.D0.AND.E.LE.el)THEN
 
        DOM_INT_T2 = DOM_INT_T2 + 
     &               1.D0/pi*(SQRT(E)*LOG((SQRT(E)+SQRT(el))
     &               /(SQRT(el)-SQRT(E))) + 1.5D0*SQRT(el)
     &               *LOG((el-E)/Ea) + el**1.5D0/(2.D0*E)*LOG(el/(el-E))
     &               )
 
      ELSEIF(E.EQ.0.D0)THEN
 
        DOM_INT_T2 = DOM_INT_T2 + 1.D0/pi*(0.5*el**(1./3.) + LOG(el/Ea)
     &               + 0.5D0*SQRT(el))
 
      ELSE
 
        DOM_INT_T2 = DOM_INT_T2 + 
     &               1.D0/pi*( - SQRT(ABS(E))*ATAN(2*(SQRT(el-ABS(E)))
     &               /(el-ABS(E))) + 1.5D0*SQRT(el)*LOG((el-E)/Ea)
     &               + el**1.5D0/(2.D0*E)*LOG(el/(el-E)))
 
      ENDIF
      RETURN
      END FUNCTION DOM_INT_T2
 
!---------------------------------------------------------------------------
C
C-----FUNCTION TO EVALUATE exp(Z)*E1(Z)
C
      FUNCTION ZFI(Za)
C
C Complex exponential integral function multiplied by exponential
C
C AUTHOR: J. Raynal
C
      IMPLICIT NONE
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      COMPLEX*16 :: Za
      COMPLEX*16 :: ZFI
C
C Local variables
C
      REAL*8 :: aj
      INTEGER :: i, m
      REAL :: REAL
      COMPLEX*16 :: y
C
C*** End of declarations rewritten by SPAG
C
      ZFI = 0.D0
      IF(Za.EQ.0.D0)RETURN
C     if (ABS(dreal(za)+18.5d0).ge.25.d0) go to 3
C     if (SQRT(625.d0-(dreal(za)+18.5d0)**2)/1.665d0.lt.ABS(dimag(za))
      IF(ABS(REAL(Za) + 18.5D0).LT.25.D0)THEN
        IF(SQRT(625.D0 - (REAL(Za)+18.5D0)**2)/1.665D0.GE.ABS(IMAG(Za)))
     &     THEN
C     zfi=-.57721566490153d0-cdlog(za)
          ZFI = -.57721566490153D0 - LOG(Za)
          y = 1.D0
          DO m = 1, 2000
            aj = m
            y = -y*Za/aj
C     if (cABS(y).lt.1.d-15*cABS(zfi)) go to 2
            IF(ABS(y).LT.1.D-15*ABS(ZFI))EXIT
            ZFI = ZFI - y/aj
          ENDDO
C   2 zfi=cEXP(za)*zfi
          ZFI = EXP(Za)*ZFI
          RETURN
        ENDIF
      ENDIF
      DO i = 1, 20
        aj = 21 - i
        ZFI = aj/(Za + ZFI)
        ZFI = aj/(1.D0 + ZFI)
      ENDDO
      ZFI = 1.D0/(ZFI + Za)
      RETURN
      END FUNCTION ZFI
 
!---------------------------------------------------------------------------
 
C
C-----FUNCTION TO EVALUATE Ei(X)
C
      FUNCTION EIN(X)
      IMPLICIT NONE
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      REAL*8 :: X
      REAL*8 :: EIN
C
C Local variables
C
      REAL*8 :: fac, h
      INTEGER :: n
C
C*** End of declarations rewritten by SPAG
C
      EIN = 0.57721566490153D0 + LOG(ABS(X))
      fac = 1.0
      DO n = 1, 100
        h = FLOAT(n)
        fac = fac*h
        EIN = EIN + X**n/(h*fac)
      ENDDO
      RETURN
      END FUNCTION EIN
C     *******************************************************
C     END of dispersive
C     *******************************************************
