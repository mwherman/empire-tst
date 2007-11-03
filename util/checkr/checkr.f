!
! **********************************************************************
! *   PROGRAM CHECKR
! *
! *   PROGRAM TO CHECK FORMAT VALIDITY OF AN ENDF-5 OR -6 FORMAT
! *   EVALUATED DATA FILE
! *
! *         VERSION 7.0    OCTOBER 2004     C.L.DUNFORD
! *                        1. MODIFIED TO PROVIDE A MODULE FOR THE NEA
! *                           MODLIB PROJECT
! *                        2. ALLOW ENERGY DEPENDENT DELAYED FISSION
! *                           GROUP PARAMETERS.
! *                        3. CHECK FOR PROPER ZSA SYMBOL FOR FIRST
! *                           FIELD ON RECORD 5 OF 1-451
! *                        4. PERMIT USER TO SUPPLY BATCH INPUT FILE
! *                             NAME
! *                        5. ADD ERROR MESSAGE IF ELASTIC TRANS-
! *                           FORMATION MATRIX IS GIVEN
! *                        6. LIMIT ON ANGULAR POINTS IN FILE 4
! *                              INCREASED TO 201
! *                        7. LIMIT ON NUMBER OF ENERGIES AT WHICH
! *                              ANGULAR DISTRIBUTIONS IN FILES 4 AND 6
! *                               (TAB2) INCREASED TO 2000
! *                        8. LIMIT ON NUMBER OF SUBSECTIONS IN FILE 6
! *                              INCREASED TO 2000
! *                        9. ALLOW BOTH THE A AND R PARAMETERS TO BE
! *                           ENERGY DEPENDENT FOR KALBACH-MANN
! *                       10. ALLOW LPT=15 FOR CHARGED-PARTICLE ELASTIC
! *                           SCATTERING
! *                       11. ALLOW STABLE NUCLEI IN 8-457 (NST=1)
! *                       12. REMOVED FORTRAN LINE CONTROLS FROM OUTPUT
! *                       13. ADDED COMMAND LINE INPUT TO UNIX AND
! *                           WINDOWS VERSIONS. NOTE: ONLY INPUT AND
! *                           OUTPUT FILE NAMES CAN BE GIVEN. DEFAULT
! *                           OPTIONS ARE ASSUMED UNLESS THIRD
! *                           PARAMETER IS N.
! *         VERSION 7.01   APRIL 2005     C.L.DUNFORD
! *                        1. CORRECTED CHECKS IN 8-457 FOR NST=1
! *                        2. ALLOW 0-nn-1 AS A MATERIAL FOR DECAY DATA
! *                        3. RDTAB1 AND RDTAB2 DID NOT BUFFER ALL
! *                           PHYSICAL RECORDS
! *                        4. SET SUCCESS FLAG AFTER RETURN FROM BEGIN
! *                        5. ALLOW NVER TO BE A YEAR 1990 TO CURRENT
! *                        6. CORRECTED SYMBOL GENERATION FOR SECOND
! *                            THIRD METASTABLE STATE (Kellett, NEA)
! *                        7. ADDED SYMBOL XX FOR UNNAMED ELEMENTS
! *                        8. ALLOW SECTION TO BE CHECKED EVEN IF MT
! *                            NUMBER WRONG
! *                        9. REMOVE ERRONEOUS ERROR CHECK ON ELFS
! *                       10. ALLOWED EMAX DOWN TO 1.0 MEV FOR OTHER
! *                           THAN INCIDENT NEUTRONS.
! *
! *         VERSION 7.02   MAY 2005     C.L.DUNFORD
! *                        1. ONLY ERRORS REPORTED IN OUTPUT
! *
! *         VERSION 7.03   APRIL 2006     M. HERMAN
! *                        1. WARNING SUBROUTINE INTRODUCED
! *                           'MT ALLOWED IN DERIVED FILES' CLSSIFIED AS
! *                           WARNING INSTEAD OF AN ERROR
! *
! *         VERSION 7.04   Sept  2007     R. Capote
! *                        1. Can be compiled with GNU g95
! *
! *      REFER ALL COMMENTS AND INQUIRIES TO
! *
! *         NATIONAL NUCLEAR DATA CENTER
! *         BUILDING 197D
! *         BROOKHAVEN NATIONAL LABORATORY
! *         P.O. BOX 5000
! *         UPTON, NY 11973-5000
! *         USA
! *
! *      TELEPHONE           631-344-2902
! *      E-MAIL              NNDC@BNL.GOV
! *
!***********************************************************************
!
!     TO CUSTOMIZE THIS SOURCE RUN SETMDC
!        ANS  -  ANSI STANDARD BATCH MODE VERSION
!        VMS  -  COMMAND MODE FOR VMS OPERATING SYSTEM
!        WIN  -  COMMAND MODE FOR PC USING DIGITAL VISUAL FORTRAN
!        UNX  -  COMMAND MODE FOR UNIX USING LAHEY FORTRAN
!        DVF  -  GRAPHICAL MODE FOR PC USING DIGITAL VISUAL FORTRAN
!        LWI  -  GRAPHICAL MODE FOR UNIX USING LAHEY WINTERACTER
!        MOD  -  MODULE FOR THE MODLIB PROJECT OF NEA WPEC
!
!     THE "ANS" VERSION MEETS F95 STANDARDS FOR FIXED OR FREE FORMAT
!       SOURCE
!     THE "VMS" VERSION WILL COMPILE WITH EITHER THE FORTRAN-77 OR
!       FORTRAN-90 VMS COMPILER
!     THE "DVF" VERSION HAS A WINDOWS GRAPHICAL INTERFACE. IT WILL
!       COMPILE WITH THE DIGITAL VISUAL FORTRAN COMPILER RUNNING
!       UNDER WINDOWS
!     THE "LWI" VERSION HAS A X-WINDOWS GRAPHICAL INTERFACE. IT WILL
!       COMPILE WITH THE LAHEY FORTRAN COMPILER WITH WINTERACTER
!       RUNNING UNDER UNIX
!
!***********************************************************************
!+++MDC+++
!...VMS, ANS, WIN, UNX
!
!     MAIN PROGRAM FOR NON-WINDOWS IMPLEMENTATION OF CHECKR
!
      PROGRAM CHECKR
!
      IMPLICIT NONE
!...LWI, DVF, MOD
!/!
!/!     MODULE IMPLEMENTATION OF CHECKR FOR MODLIB AND WINDOWS
!/!
!/      MODULE CHECKR
!/!
!/      IMPLICIT NONE
!/!
!/      PRIVATE
!/!
!/      PUBLIC :: RUN_CHECKR
!/      PUBLIC :: CHECKR_INPUT, CHECKR_DATA, CHECKR_SUCCESS
!...LWI, DVF
!/      PUBLIC :: IRERUN
!---MDC---
!
!     CHECKR VERSION NUMBER
!
!+++MDC+++
!...VMS, UNX, ANSI, WIN, LWI, DVF
      CHARACTER(LEN=*), PARAMETER :: VERSION = '7.03'
!...MOD
!/      CHARACTER(LEN=*), PARAMETER :: VERSION = '7.03'
!---MDC---
!
!     DEFINE VARIABLE PRECISION
!
      INTEGER(KIND=4), PARAMETER :: I4 = SELECTED_INT_KIND(8)
      INTEGER(KIND=4), PARAMETER :: R4 = SELECTED_REAL_KIND(6,37)
      INTEGER(KIND=4), PARAMETER :: R8 = SELECTED_REAL_KIND(15,307)
!
!     STANDARD FORTRAN INPUT AND OUTPUT UNITS
!
      INTEGER(KIND=I4) :: NIN
      INTEGER(KIND=I4), PARAMETER :: INPUT0 = 5, IOUT=6
!
!     ENDF DISK FILE INPUT AND CHECKING OUTPUT FORTRAN UNITS
!
      INTEGER(KIND=I4), PARAMETER :: JIN=20,JOUT=21
!
!     FINAL FORTRAN OUTPUT UNIT
!
      INTEGER(KIND=I4) :: NOUT
!
!     IMDC  FLAG FOR COMPILER OPTION
!     TFMT  FORMAT FOR INTERACTIVE INPUT PROMPT
!     STATUS PARAMETER FOR OPENING NEW FILE
!
!+++MDC+++
!...ANS
!/      INTEGER(KIND=I4), PARAMETER :: IMDC = 0
!/      CHARACTER(LEN=*), PARAMETER :: TFMT = '(A)'
!/      CHARACTER(LEN=*), PARAMETER :: OSTATUS = 'REPLACE'
!...VMS
!/      INTEGER(KIND=I4), PARAMETER :: IMDC = 1
!/      CHARACTER(LEN=*), PARAMETER :: TFMT = '(/A,$)'
!/      CHARACTER(LEN=*), PARAMETER :: OSTATUS = 'NEW'
!...WIN
!/      INTEGER(KIND=I4), PARAMETER :: IMDC = 2
!/      CHARACTER(LEN=*), PARAMETER :: TFMT = '(/A,$)'
!/      CHARACTER(LEN=*), PARAMETER :: OSTATUS = 'REPLACE'
!...UNX
      INTEGER(KIND=I4), PARAMETER :: IMDC = 3
      CHARACTER(LEN=*), PARAMETER :: TFMT = '(/A,$)'
      CHARACTER(LEN=*), PARAMETER :: OSTATUS = 'REPLACE'
!...DVF
!/      INTEGER(KIND=I4), PARAMETER :: IMDC = 4
!/      CHARACTER(LEN=*), PARAMETER :: TFMT = '(A)'
!/      CHARACTER(LEN=*), PARAMETER :: OSTATUS = 'REPLACE'
!...LWI
!/      INTEGER(KIND=I4), PARAMETER :: IMDC = 5
!/      CHARACTER(LEN=*), PARAMETER :: TFMT = '(A)'
!/      CHARACTER(LEN=*), PARAMETER :: OSTATUS = 'REPLACE'
!...MOD
!/      INTEGER(KIND=I4), PARAMETER :: IMDC = 6
!/      CHARACTER(LEN=*), PARAMETER :: TFMT = '(A)'
!/      CHARACTER(LEN=*), PARAMETER :: OSTATUS = 'REPLACE'
!---MDC---
!
!     COMMAND LINE INPUT TEXT AND TEXT LENGTH
!
      CHARACTER(LEN=100) :: INPAR
      INTEGER(KIND=I4) :: ILENP
!
      TYPE CHECKR_INPUT
         CHARACTER(LEN=100) :: INFIL
         CHARACTER(LEN=100) :: OUTFIL
         INTEGER(KIND=I4) :: MATMIN
         INTEGER(KIND=I4) :: MATMAX
      END TYPE CHECKR_INPUT
!
      TYPE(CHECKR_INPUT) CHECKR_DATA
!
!     FLAG TO INDICATE WHETHER MULTIPLE INPUT FILES CAN BE SELECTED
!
      INTEGER(KIND=I4) :: IONEPASS        !  0, YES;  1, NO
!
!     FLAG TO INDICATE SUCCESS OR FAILURE OF STANEF EXECUTION
!
      INTEGER(KIND=I4) :: CHECKR_SUCCESS, IRERUN
!
!     FILE (TAPE) LABEL FROM FIRST RECORD
!
      CHARACTER(LEN=66) :: TLABEL
      INTEGER(KIND=I4) :: LABEL
!
!     FLAG TO INDICATE A SEQUENCED FILE; UNSEQUENCED IF BLANK
!
      CHARACTER(LEN=5) :: ASEQ
!
!     LIBRARY, VERSION, SUBLIBRARY, MOD NUMBER AND FORMAT OF
!         MATERIAL BEING PROCESSED
!
      INTEGER(KIND=I4) :: NLIB,NVER,NSUB,NMOD,NFOR
!
!     MATERIAL, FILE, AND SECTION NUMBER CURRENTLY BEING PROCESSED
!
      INTEGER(KIND=I4) :: MATO,MFO,MTO
!
!     MATERIAL, FILE, AND SECTION NUMBER OF LAST ERROR DETECTED
!
      INTEGER(KIND=I4) :: MATERR,MFERR,MTERR
!
!     1000*Z + A OF MATERIAL CURRENTLY BEING PROCESSED
!        AWR IS THE RATIO OF THE MATERIAL MASS TO THAT OF THE NEUTRON
!        AWI IS THE RATIO OF THE PROJECTILE MASS TO THE THAT OF NEUTRON
!        STA = 0.0, STABLE MATERIAL; STA = 1.0 RADIOACTIVE MATERIAL
!
      REAL(KIND=R4) :: ZA,AWR,AWI,STA
!
!     LIS   IS THE STATE NUMBER (0 FOR GROUND) OF THE MATERIAL
!     LISO  IS THE ISOMER STATE NUMBER OF THE MATERIAL
!
      INTEGER(KIND=I4) :: LIS,LISO
!
!     LDRV   IS THE DERIVED FILE FLAG
!     LRP    IS THE RESONANCE PARAMETER FLAG
!     LFI    IS THE FISSION FLAG
!
      INTEGER(KIND=I4) :: LDRV,LRP,LFI
!
!     FLAG INDICATING THE PRESENCE OF NUBAR IN FILE 1
!
      INTEGER(KIND=I4) :: I452
!
!     NUMBER OF SECTIONS IN MF=12 AND MF=13 FOR THE CURRENT MATERIAL
!
      INTEGER(KIND=I4) :: N12S
!
!     MT OF LAST ENCOUNTERED LUMPED COVARIANCE
!
      INTEGER(KIND=I4) :: MTLP
!
!     CONTENTS OF FIELDS ON A HEAD/CONT RECORD
!
      INTEGER(KIND=I4) :: L1H,L2H,N1H,N2H
      REAL(KIND=R4) :: C1H,C2H
!
!     SIZE LIMITS FOR ALL DATA TABLES
!
      INTEGER(KIND=I4), PARAMETER :: NIRMAX=20   ! INTERPOLATION REGIONS
      INTEGER(KIND=I4), PARAMETER :: NPTSMAX=50000 ! POINTS IN A TAB 1
      INTEGER(KIND=I4), PARAMETER :: NPTS2MAX=2000 ! POINTS IN A TAB 2
!
!     ALLOWED INTERPOLATION SCHEMES
!
      INTEGER(KIND=I4), PARAMETER :: INTMAX=6
!
!     CONTENTS OF FIRST RECORD AND INTERPOLATION TABLE FOR A TAB1 RECORD
!
      INTEGER(KIND=I4) :: L1,L2,NR,NP
      INTEGER(KIND=I4), DIMENSION(NIRMAX) :: NBT,JNT
      REAL(KIND=R4) :: C1,C2
!
!     CONTENTS OF FIRST RECORD AND INTERPOLATION TABLE FOR A TAB2 RECORD
!
      INTEGER(KIND=I4) :: L12,L22,NR2,NP2
      INTEGER(KIND=I4), DIMENSION(NIRMAX) :: NBT2,JNT2
      REAL(KIND=R4) :: C12,C22
!
!     CONTENTS OF FIRST RECORD AND FIRST FOUR DATA FIELDS OF A LIST
!        RECORD
!
      INTEGER(KIND=I4) :: L1L,L2L,NPL,N2L
      REAL(KIND=R4) :: C1L,C2L
      REAL(KIND=R4), DIMENSION(4) :: BIN,BIN1
!
!     POSSIBLE DATA REPETITION RATES ON A LIST RECORD
!
      INTEGER(KIND=I4), PARAMETER :: NREP6 = 6,NREP12 = 12,NREP18=18
!
!     TEXT CONTENTS ON A TEXT RECORD
!
      CHARACTER(LEN=66) :: TEXT
!
!     MATERIAL, FILE, SECTION, AND SEQUENCE NUMBER OF CURRENT RECORD
!
      INTEGER(KIND=I4) :: MATP,MFP,MTP,NSEQP
!
!     SEQUENCE NUMBER OF THE CONT-LIKE PART OF A TAB OR LIST RECORD
!
      INTEGER(KIND=I4) :: NSEQP1
!
!     ABSOLUTE RECORD NUMBER OF CURRENT RECORD
!
      INTEGER(KIND=I4) :: ISEQ
!
!     CHARACTER IMAGE OF CURRENT RECORD
!
      CHARACTER(LEN=80) :: IFIELD
!
      INTEGER(KIND=I4) :: MAT,MF,MT,NSEQ
!
!     LOCATIONS OF THE BEGINNING OF EACH ENDF DATA FIELD
!
      INTEGER(KIND=I4), DIMENSION(7) :: IBR
      DATA IBR/1,12,23,34,45,56,67/
!
!     LOCATIONS OF THE BEGINNING OF EACH ENDF ID FIELD
!
      INTEGER(KIND=I4), DIMENSION(5) :: IBR1
      DATA IBR1/67,71,73,76,81/
!
!     CURRENT YEAR
!
      INTEGER(KIND=I4) :: IYR
!
!     ERROR FLAG
!
      INTEGER(KIND=I4) :: IERX
!
!     END OF FILE FLAG
!
      INTEGER(KIND=I4) :: IFIN
!
!     ERROR MESSAGE TEXT
!
      CHARACTER(LEN=80) :: EMESS
!
!     MAXIMUM TOLERANCE FOR DIFFERENCE TWO FLOATING POINT NUMBERS
!       SAID TO BE EQUAL
!
      REAL(KIND=R4), PARAMETER ::  EPS = .00001
!
!     NXC   NUMBER OF SECTIONS ENCOUNTERED
!     NXC0  NUMBER OF SECTIONS IN THE DIRECTORY
!
      INTEGER(KIND=I4), PARAMETER :: NSECMAX=1000
      INTEGER(KIND=I4) :: NXC,NXC0
      INTEGER(KIND=I4), DIMENSION(NSECMAX,2):: INDX
!
!     SET SOME IMPORTANT LIMITS
!
!          FILE 1 COMMENTS
      INTEGER(KIND=I4), PARAMETER :: NCDMI=3,NCDMA=999999 !COMMENTS
!          FILE 1 NUBAR
      INTEGER(KIND=I4), PARAMETER :: NCOMAX=4   ! NUBAR COEFFICIENTS
!          FILE 2 RESONANCE DATA
      INTEGER(KIND=I4), PARAMETER :: NISMAX=20  ! NUMBER OF ISOTOPES
      INTEGER(KIND=I4), PARAMETER :: NERM6=12,NERM5=2 ! RESONANCE RANGES
      INTEGER(KIND=I4), PARAMETER :: LRFM6=6,LRFM5=2 ! ALLOWED REPRES
      INTEGER(KIND=I4), PARAMETER :: NSCLMAX=50  ! SCATTERING LENGTHS
      INTEGER(KIND=I4), PARAMETER :: NLSMAX=4   ! RESONANCE L-VALUES
      INTEGER(KIND=I4), PARAMETER :: NLSCMAX=20   ! L-VALUES FOR DSIGMA
      INTEGER(KIND=I4), PARAMETER :: NJSMAX=6   ! RESONANCE J-VALUES
      INTEGER(KIND=I4), PARAMETER :: NRESMAX=5000 ! RESONANCES PER L
      INTEGER(KIND=I4), PARAMETER :: NGREMAX=1,NFREMAX=1,NIREMAX=4,      &      
     &              NCREMAX=4      ! MAXIMUM OF REACTIONS PER TYPE
      INTEGER(KIND=I4), PARAMETER :: URNEMAX = 250 !UR ENERGY POINTS
!          FILE 4 SECONDARY ANGULAR DATA
      INTEGER(KIND=I4), PARAMETER :: NES4MAX=2000 ! NUMBER OF E(INC)
      INTEGER(KIND=I4), PARAMETER :: NLEGMAX=64  ! LEGENDRE COEFS
      INTEGER(KIND=I4), PARAMETER :: NANGMAX=201 ! ANGLE POINTS
!          FILE 5 SECONDARY ENERGY DATA
      INTEGER(KIND=I4), PARAMETER :: NSUBSMAX=100!  SUBSECTION LIMIT
      INTEGER(KIND=I4), PARAMETER :: NES5MAX=200 ! NUMBER OF E(INC)
      INTEGER(KIND=I4), PARAMETER :: NEDISMAX=1000 ! ENERGY POINTS
!          FILE 6 SECONDARY ENERGY-ANGLE DATA
      INTEGER(KIND=I4), PARAMETER :: NSUBS6MAX=2000! SUBSECTION LIMIT
      INTEGER(KIND=I4), PARAMETER :: NES6MAX=NES4MAX ! NUMBER OF E(INC)
!          FILE 7 THERMAL SCATTERING LAW
      INTEGER(KIND=I4), PARAMETER :: NPSAMAX=3     ! NONPRINCIPAL ATOMS
      INTEGER(KIND=I4), PARAMETER :: NSMTMAX=100   ! SUBSEQUENT TEMPS
!          FILE 8 FISSION YIELDS AND DECAY DATA
      INTEGER(KIND=I4), PARAMETER :: NFPMAX=2500   ! NUMBER OF YIELDS
      INTEGER(KIND=I4), PARAMETER :: NDSTMAX=100 ! PRODUCT STATES
!          FILES 12 AND 13 PHOTON PRODUCTION
      INTEGER(KIND=I4), PARAMETER :: NPHMAX=1000 ! DISCRETE PHOTONS
!
!+++MDC+++
!...VMS, ANS, WIN, UNX
!
!     EXECUTE THE CHECKR PROGRAM WHEN A STAND ALONE PROGRAM
!
      CALL RUN_CHECKR
!
!     TERMINATE JOB
!
      IF(CHECKR_SUCCESS.EQ.0) THEN
         WRITE(IOUT,'(/A)') '   '
         STOP '     JOB COMPLETED SUCCESSFULLY'
      ELSE
         WRITE(IOUT,'(/A)') '   '
         STOP '     JOB TERMINATED'
      END IF
!---MDC---
!
      CONTAINS
!
!***********************************************************************
!
      SUBROUTINE RUN_CHECKR
!
!     EXECUTES CHECKING PROCESS
!
!     INFIL = INPUT FILE SPECIFICATION
!     OUTFIL = OUTPUT FILE SPECIFICATION
!     IW    = N  (ALL MATERIALS IN FILE PROCESSED)
!           = Y  (SPECIFY A MATERIAL OR RANGE OF MATERIALS)
!
!     IF IW IS YES, THEN ANOTHER RECORD IS REQUIRED
!
!     MATMIN  = MATERIAL NUMBER OF FIRST OR ONLY MATERIAL
!     MATMAX  = MATERIAL NUMBER OF LAST MATERIAL OR A RANGE
!
!     A COMMA OR A DASH BETWEEN MATERIAL NUMBERS INDICATES A RANGE
!
!          MATMIN - SELECTS THIS MATERIAL AND ALL OTHERS WITH A LARGER
!                   MATERIAL NUMBER
!          -MATMAX  SELECTS THIS MATERIAL AND ALL OTHERS WITH A SMALLER
!                  MATERIAL NUMBER
!
! **********************************************************************
!
      IMPLICIT NONE
!
      CHARACTER(LEN=1), INTRINSIC :: CHAR
      INTEGER(KIND=I4), INTRINSIC :: MOD, ICHAR
!
      INTEGER(KIND=I4) :: IQUIT ! FLAG TO INDICATE WHETHER OR NOT TO EXIT       
      INTEGER(KIND=I4) :: IFIND ! FLAGS WHETHER DESIRED MATERIAL FOUND
      INTEGER(KIND=I4) :: IFL2
      INTEGER(KIND=I4) :: INDX1,INDX2
      INTEGER(KIND=I4) :: N
!
      CHARACTER(LEN=*), PARAMETER :: DASHES = REPEAT('-',80)
!
!     OUTPUT PROGRAM IDENTIFICATION
!
      CHECKR_SUCCESS = 0
      IF(IMDC.LT.4) THEN
         WRITE(IOUT,'(/2A)') ' PROGRAM CHECKR VERSION ',VERSION
      END IF
!
!     CHECK FOR COMMAND LINE INPUT
!
      IONEPASS = 0
      CALL GET_FROM_CLINE
!
!     INITIALIZE RUN
!
   10 CALL BEGIN(IQUIT)
      IF(IQUIT.GT.0)  THEN
         IF(IONEPASS.EQ.1) CHECKR_SUCCESS = 1
         GO TO 100
      END IF
!
!     CHECK LABEL AND FIND STARTING MATERIAL
!
      CALL SEARCH(IFIND)
      IF(IFIND.EQ.0)   GO TO 50
!
!     UNEXPECTED END OF FILE ENCOUNTERED
!
   20 IF(IERX.EQ.2) THEN
         IF(IMDC.LT.4) THEN
            WRITE(IOUT,'(//5X,2A)')  'END OF FILE ENCOUNTERED BEFORE ', &       
     &                      'TEND RECORD FOUND!'
         END IF
         IF(NOUT.NE.IOUT)   THEN
            WRITE(NOUT,'(//5X,2A)')  'END OF FILE ENCOUNTERED BEFORE ', &       
     &                      'TEND RECORD FOUND!'
         END IF
         IF(NOUT.NE.IOUT)   CLOSE(UNIT=NOUT)
         CLOSE(UNIT=JIN)
         CHECKR_SUCCESS = 1
         GO TO 100
      END IF
!
!     PROCESS NEXT SECTION
!
      IF(MAT.NE.MATO)  THEN  ! NEW MATERIAL
         IF(CHECKR_DATA%MATMAX.NE.0.AND.MAT.GT.CHECKR_DATA%MATMAX)      &       
     &               GO TO 50
         NSEQP1 = NSEQP
         MATO = MAT
         MFO = 0
         IFL2 = 0
         MTLP = 850
         I452 = 0
         N12S = 0
         MFO = 0
         WRITE(NOUT,'(A/1X,A,I5)')  CHAR(12),'CHECK MATERIAL',MATO
         IF(NOUT.NE.IOUT)  THEN
            IF(IMDC.LT.4) WRITE(IOUT,'(/A)')  '   '
         END IF
      END IF
      IF(MF.NE.MFO)   THEN           ! NEW FILE
         MFO = MF
         IF(MFO.EQ.1) THEN
            IFL2 = 0
         ELSE IF(MFO.EQ.2) THEN
            IFL2 = 1
         ELSE
            IF(IFL2.EQ.0.AND.LRP.GE.0)  THEN
               WRITE(EMESS,'(A,I3,A)') 'LRP =',LRP,                     &       
     &          ' REQUIRES THE PRESENCE OF FILE 2, BUT IT IS MISSING.'
               CALL ERROR_MESSAGE(0)
               IFL2 = 1
            END IF
         END IF
      END IF
!
!     NEW SECTION
!
   35 MTO = MT
!
!     IN INTERACTIVE MODE OUTPUT CURRENT SECTION ID TO TERMINAL
!
      IF(NOUT.NE.IOUT) CALL OUT_STATUS
!
!     CHECK THE NEW SECTION
!
      CALL CHKSEC
      IF(IERX.EQ.2)   GO TO 20
!
!     CHECK FOR MISSING SECTIONS
!
      IF(MAT.NE.MATO.OR.IFIN.NE.0)   THEN
         IF(NXC.GT.0) THEN
            WRITE(NOUT,'(//)')
            DO N=1,NXC
               IF(INDX(N,2).EQ.1) THEN
                  INDX1 = INDX(N,1)/1000
                  INDX2 = MOD(INDX(N,1),1000)
                  WRITE(EMESS,'(A,I5,3X,A,I3,3X,A,I4,3X,A)')            &       
     &                 'SECTION MAT=',MATO,'MF=',INDX1,'MT=',           &       
     &                 INDX2,'IS MISSING'
                  CALL ERROR_MESSAGE(0)
               END IF
            END DO
         END IF
      END IF
!
!     END OF FILE 1 CHECK THAT VALUE OF LFI AND PRESENCE OF NUBAR
!         IS COMPATIBLE
!
      IF(MF.NE.MFO.AND.MFO.EQ.1)   THEN
         IF(LFI.EQ.1.AND.I452.NE.1.AND.MOD(NSUB,10).EQ.0) THEN
            EMESS = 'LFI INCORRECT OR NUBAR MISSING         PRECEDING '
            CALL ERROR_MESSAGE(NSEQP)
         END IF
         IF(LFI.NE.1.AND.I452.EQ.1)  THEN
            EMESS = 'LFI SHOULD BE SET TO 1                 PRECEDING '
            CALL ERROR_MESSAGE(NSEQP)
         END IF
      END IF
!
!     CHECK END OF TAPE FLAG
!
      IF(IFIN.EQ.0) THEN
        IF(CHECKR_DATA%MATMAX.EQ.0.OR.MAT.LE.CHECKR_DATA%MATMAX)        &       
     &           GO TO 20
      END IF
!
!     CLOSE FILES
!
   50 IF(NOUT.NE.IOUT)   CLOSE(UNIT=NOUT)
      CLOSE(UNIT=JIN)
!
!     SEE IF ONE PASS LIMIT SET
!
      IF(IONEPASS.EQ.0) GO TO 10
!
  100 RETURN
      END SUBROUTINE RUN_CHECKR
!
!***********************************************************************
!
      SUBROUTINE BEGIN(IQUIT)
!
!     ROUTINE TO SET UP JOB
!
      IMPLICIT NONE
!
      INTEGER(KIND=I4) :: IQUIT
!
!      CHARACTER(LEN=*), INTRINSIC :: TRIM
      INTEGER(KIND=I4), INTRINSIC :: LEN_TRIM
!
      CHARACTER(LEN=1) :: IW
      CHARACTER(LEN=11) ::  ADATE
      LOGICAL(KIND=I4) :: IEXIST
      INTEGER(KIND=I4) :: IC
!
!     INITIALIZE PROCESSING CONTROL VARIABLES
!
      IERX = 0
      MATO = 0
      MFO = 0
      MTO = 0
      MATERR = 0
      MFERR = 0
      MTERR = 0
      NFOR = 0
      IFIN = 0
      NOUT = IOUT
   10 IQUIT = 0
!
!     INITIALIZE TO STANDARD OPTIONS
!
      IF(IMDC.LT.4) THEN
         CHECKR_DATA%INFIL = '*'
         CHECKR_DATA%OUTFIL = '*'
         CHECKR_DATA%MATMIN = 0
         CHECKR_DATA%MATMAX = 0
      END IF
      SELECT CASE (IMDC)
         CASE (0)
            IW = 'N'
            IONEPASS = 0
         CASE (1,2,3)
            IF(ILENP.NE.0)  THEN
               CALL TOKEN(INPAR,'%',1,CHECKR_DATA%INFIL)
               CALL TOKEN(INPAR,'%',2,CHECKR_DATA%OUTFIL)
               CALL TOKEN(INPAR,'%',3,IW)
               IC = ICHAR(IW)
               IF(IC.GT.96.AND.IC.LT.123) IW = CHAR(IC-32)
               IF(IW.EQ.' ') THEN
                  IW = 'Y'
               ELSE IF(IW.NE.'Y'.AND.IW.NE.'N') THEN
                  IW = '*'
               END IF
               IONEPASS = 1
            ELSE
               IW = '*'
               IONEPASS = 0
            END IF
         CASE (4,5,6)
            IW = 'N'
            IONEPASS = 1
      END SELECT
!
!     GET INPUT FILE SPECIFICATION
!
      IF(IMDC.LT.4) THEN
         IF(CHECKR_DATA%INFIL.EQ.'*') THEN
            IF(IMDC.NE.0) THEN
               WRITE(IOUT,FMT=TFMT)                                     &       
     &             ' Input File Specification             - '
            END IF
            READ(NIN,'(A)') CHECKR_DATA%INFIL
         ELSE
            WRITE(IOUT,'(/2A)') ' Input file - ',                       &       
     &                TRIM(CHECKR_DATA%INFIL)
         END IF
      END IF
!
!     SEE IF INPUT INDICATES JOB TERMINATION
!
      IF(CHECKR_DATA%INFIL.EQ.' '.OR.CHECKR_DATA%INFIL.EQ.'DONE') THEN
         IQUIT = 1
         GO TO 100
      END IF
!
!     MAKE SURE INPUT FILE EXISTS
!
      INQUIRE(FILE=CHECKR_DATA%INFIL,EXIST=IEXIST)
      IF(.NOT.IEXIST)  THEN
         IF(IMDC.LT.4) THEN
            WRITE(IOUT,'(/A/)')  '       COULD NOT FIND INPUT FILE'
         END IF
         SELECT CASE (IMDC)
            CASE (1,2,3)
               IF(IONEPASS.EQ.0) GO TO 10
         END SELECT
         IQUIT = 1
         CHECKR_SUCCESS = 1
         GO TO 100
      END IF
!
!     GET OUTPUT FILE SPECIFICATION
!
      IF(IMDC.LT.4) THEN
         IF(CHECKR_DATA%OUTFIL.EQ.'*' ) THEN
            IF(IMDC.NE.0) THEN
               WRITE(IOUT,FMT=TFMT)                                     &       
     &              ' Output Message File Specification    - '
            END IF
            READ(NIN,'(A)') CHECKR_DATA%OUTFIL
         ELSE
            WRITE(IOUT,'(/2A)') ' Output file - ',                      &       
     &              TRIM(CHECKR_DATA%OUTFIL)
         END IF
      END IF
      IF(CHECKR_DATA%OUTFIL.NE.' ')  THEN
         NOUT = JOUT             ! SETS FORTRAN OUTPUT UNIT IF DISK FILE
      END IF
!
!     CHECK IF ENTIRE TAPE TO BE PROCESSED (INTERACTIVE MODE ONLY)
!
      IF(IMDC.NE.0) THEN
         IF(IW.EQ.'*') THEN
            IF(IMDC.LT.4) THEN
               WRITE(IOUT,FMT=TFMT)                                     &       
     &                ' Check Entire File (Y(es),N(o))?  '
               READ(NIN,'(A)')  IW
               IC = ICHAR(IW)
               IF(IC.GT.96.AND.IC.LT.123) IW = CHAR(IC-32)
            END IF
         END IF
      END IF
!
!     GET MATERIAL NUMBER RANGE (ALL) IF DEFAULT NOT SELECTED
!
      IF(IMDC.EQ.0.OR.(IW.EQ.'N'.AND.IMDC.LT.4)) THEN
         CALL SELECT_MATS
      END IF
!
!     OPEN INPUT AND OUTPUT FILES
!
      OPEN(UNIT=JIN,ACCESS='SEQUENTIAL',STATUS='OLD',                   &       
     &          FILE=CHECKR_DATA%INFIL,ACTION='READ')
      IF(NOUT.NE.6) THEN
!+++MDC+++
!...VMS
!/         OPEN(UNIT=NOUT,ACCESS='SEQUENTIAL',STATUS=OSTATUS,           &       
!/     &       FILE=CHECKR_DATA%OUTFIL,CARRIAGECONTROL='LIST')
!...WIN, DVF, UNX, LWI, ANS, MOD
         OPEN(UNIT=NOUT,ACCESS='SEQUENTIAL',STATUS=OSTATUS,             &       
     &       FILE=CHECKR_DATA%OUTFIL)
!---MDC---
      END IF
!
!     OUTPUT SELECTED OPTIONS
!
      CALL DATE(ADATE)
      IF(IMDC.LT.4)  WRITE(IOUT,'(/A)') ' '
      IF(NOUT.NE.IOUT) THEN
         WRITE(NOUT,'(A///2A,30X,2A/)')                                 &       
     &             CHAR(12),'PROGRAM CHECKR VERSION ',                  &       
     &             VERSION,'Run on ',ADATE
      END IF
      WRITE(NOUT,'(2A)')                                                &       
     &   'Input File Specification------------------------',            &       
     &   TRIM(CHECKR_DATA%INFIL)
      IF(CHECKR_DATA%MATMIN.EQ.0.AND.CHECKR_DATA%MATMAX.EQ.0)   THEN
         WRITE(NOUT,'(A)')  'Check the Entire File'
       ELSE
         WRITE(NOUT,'(A,I4,A,I4)')                                      &       
     &        'Check Materials---------------------------------',       &       
     &             CHECKR_DATA%MATMIN,' to ',CHECKR_DATA%MATMAX
      END IF
!
  100 RETURN
      END SUBROUTINE BEGIN
!
!***********************************************************************
!
      SUBROUTINE SELECT_MATS
!
!     SUBROUTINE GET MATERIALS TO BE EXTRACTED FROM INPUT
!
      IMPLICIT NONE
!
      INTEGER(KIND=I4), INTRINSIC :: LEN_TRIM, INDEX
!
      CHARACTER(LEN=15) :: MATSIN
      CHARACTER(LEN=10) :: BUF
      CHARACTER(LEN=4) :: BUF1,BUF2
      INTEGER(KIND=I4) :: IDASH
      INTEGER(KIND=I4) :: LBUF
!
!     GET USER INPUT
!
      WRITE(IOUT,'(A)') ' '
      WRITE(IOUT,FMT=TFMT) '     Enter Range of MAT Numbers - '
      READ(NIN,'(A)')  MATSIN
!
!     BLANK RESPONSE IS THE SAME AS SELECTING ALL
!
      IF(MATSIN.EQ.' ')  THEN
         CHECKR_DATA%MATMIN = 0
         CHECKR_DATA%MATMAX = 0
         GO TO 100
      END IF
!
!     ANALYZE THE USER INPUT
!
      CALL TOKEN(MATSIN,',',1,BUF)
      IDASH = INDEX(BUF,'-')
      IF(IDASH.GT.0) THEN
         LBUF = LEN_TRIM(BUF)
         IF(IDASH.EQ.1) THEN
            BUF1 = '   1'
            BUF2 = BUF(2:)
         ELSE IF(IDASH.EQ.LBUF) THEN
            BUF2 = '9999'
            BUF1 = BUF(1:LBUF-1)
         ELSE
            BUF1 = BUF(1:IDASH-1)
            BUF2 = BUF(IDASH+1:)
         END IF
      ELSE
         BUF1 = BUF
         CALL TOKEN(MATSIN,',',2,BUF2)
      END IF
!
!     CONVERT FROM ASCII
!
      CHECKR_DATA%MATMIN = 1
      CHECKR_DATA%MATMAX = 9999
      READ(BUF1,'(BN,I4)',ERR=20) CHECKR_DATA%MATMIN
   20 READ(BUF2,'(BN,I4)',ERR=25) CHECKR_DATA%MATMAX
!
!     SET THE MATERIAL NUMBER LIMITS
!
   25 IF(CHECKR_DATA%MATMIN.LE.0) THEN
         CHECKR_DATA%MATMIN = 1
      END IF
      IF(CHECKR_DATA%MATMAX.LT.CHECKR_DATA%MATMIN)  THEN
         CHECKR_DATA%MATMAX = CHECKR_DATA%MATMIN
      END IF
      IF(CHECKR_DATA%MATMIN.EQ.1.AND.CHECKR_DATA%MATMAX.EQ.9999) THEN
         CHECKR_DATA%MATMIN = 0
         CHECKR_DATA%MATMAX = 0
      END IF
!
  100 RETURN
      END SUBROUTINE SELECT_MATS
!
!***********************************************************************
!
      SUBROUTINE SEARCH(IFIND)
!
!     ROUTINE TO CHECK TAPE LABEL AND SEARCH FOR STARTING RECORD
!
      INTEGER(KIND=I4) :: IFIND ! FLAG IF FIRST DESIRED MATERIAL IS FOUND       
!
!     INITIALIZE TO NOT FOUND
!
      IFIND = 0
!
!     READ FIRST CARD
!
      READ(JIN,'(A)',END=90) IFIELD
!
!     PARSE FIRST CARD TO SEE IF IT IS A LABEL
!
      ASEQ = IFIELD(76:80)
      IF(ASEQ.EQ.' ')    WRITE(NOUT,'(A//)')                             &      
     &        'FILE IS UNSEQUENCED. NO SEQUENCE TESTS WILL BE DONE'
      ISEQ = 1
      NSEQ = 0
      READ(IFIELD,'(A,I4,I2,I3,I5)',ERR=20)  TLABEL,MAT,MF,MT,NSEQ
!
!     A LABELED TAPE?
!
      IF(MF.NE.0.OR.MT.NE.0)  THEN
         TLABEL = 'TAPE IS NOT LABELED'
         LABEL = 0
!        WRITE(NOUT,'(/A/)') 'TAPE BEING PROCESSED IS NOT LABELED'
         GO TO 60
      ELSE
         LABEL = MAT
!        WRITE(NOUT,'(/2A,I5/4X,2A)') 'TAPE BEING PROCESSED IS ',       &       
!    &           'NUMBERED',LABEL,'LABEL IS  ',TLABEL
      END IF
      GO TO 40
!
!     IF READING ERROR ASSUME A PROPER LABEL AND GO ON
!
   20 WRITE(NOUT,'(5X,A//)')                                            &       
     &        'FORMAT ERROR IN FIRST RECORD, PROPER LABEL ASSUMED'
      TLABEL = 'LABEL RECORD IS NOT READABLE'
      LABEL = 0
      ISEQ = 1
!
!     READ NEXT CARD
!
   40 READ(JIN,'(A)',END=90) IFIELD
      ISEQ = ISEQ + 1
      NSEQ = 0
      READ(IFIELD,'(66X,I4,I2,I3,I5)',ERR=50) MAT,MF,MT,NSEQ
!
!     MT=0, FOUND ANOTHER LABEL
!
   50 IF(ASEQ.EQ.' ') NSEQ = ISEQ
      IF(MT.EQ.0.AND.MF.EQ.0)   THEN
         WRITE(NOUT,'(36X,A)')  'TAPE HAS TOO MANY LABELS'
         LABEL = MAT
         GO TO 40
      END IF
!
!     LOOK FOR BEGINNING OF FIRST MATERIAL REQUESTED
!
   60 IF(CHECKR_DATA%MATMIN.GT.0)   THEN
         DO WHILE(MAT.LT.CHECKR_DATA%MATMIN)
            READ(JIN,'(A)',END=90)  IFIELD
            ISEQ = ISEQ + 1
            NSEQ = 0
            READ(IFIELD,'(66X,I4,I2,I3,I5)',ERR=65) MAT,MF,MT,NSEQ
   65       IF(MAT.LT.0)  GO TO 70
            IF(ASEQ.EQ.' ') NSEQ = ISEQ
         END DO
         IF(MAT.GT.CHECKR_DATA%MATMAX) GO TO 70
      END IF
      GO TO 75
!
!     FAILED TO FIND A MATERIAL
!
   70 IF(CHECKR_DATA%MATMIN.EQ.CHECKR_DATA%MATMAX) THEN
         IF(CHECKR_DATA%MATMIN.EQ.0) THEN
            EMESS = 'INPUT FILE DOES NOT CONTAIN ANY ENDF EVALUATIONS'
         ELSE
            WRITE(EMESS,'(A,I5)')                                       &       
     &           'INPUT FILE DOES NOT CONTAIN MATERIAL',                &       
     &           CHECKR_DATA%MATMIN
         END IF
      ELSE
         WRITE(EMESS,'(A,I5,A,I5)')                                     &       
     &        'INPUT FILE DOES NOT CONTAIN ANY MATERIALS',              &       
     &         CHECKR_DATA%MATMIN,' TO',CHECKR_DATA%MATMAX
      END IF
      WRITE(NOUT,'(/A)')  EMESS
      IF(NOUT.NE.IOUT) THEN
         IF(IMDC.LT.4) WRITE(IOUT,'(10X,A)')  EMESS
      END IF
      GO TO 100
!
!     FOUND BEGINNING OF FIRST MATERIAL REQUESTED
!
   75 READ(IFIELD,'(2E11.4,4I11)',ERR=80)  C1H,C2H,L1H,L2H,N1H,N2H
      GO TO 85
   80 CALL HEADRD(C1H,C2H,L1H,L2H,N1H,N2H,MAT,MF,MT,NSEQ)
   85 IFIND = 1
      NSEQP = NSEQ
      GO TO 100
!
!     UNEXPECTED END OF FILE
!
   90 IERX = 2
!
  100 RETURN
      END SUBROUTINE SEARCH
!
!***********************************************************************
!
      SUBROUTINE CHKSEC
!
!     CONTROLS CHECK OF A SECTION BASED ON ITS FILE NUMBER (MF)
!
      IMPLICIT NONE
!
      INTEGER(KIND=I4), INTRINSIC :: MOD
!
      INTEGER(KIND=I4) :: ISUBTP
!
!     TEST THAT ZA AND AWR ARE THE SAME AS IN MF=1 MT=451
!
      IF(MTO.NE.451)  THEN
         CALL TEST2F(C1H,ZA,'ZA')
         CALL TEST2F(C2H,AWR,'AWR')
      END IF
!
!     PROCESS THE SECTION IF A VALID MF IS FOUND
!
      ISUBTP = MOD(NSUB,10)
      SELECT CASE (MF)    ! BRANCH BASED ON FILE
         CASE (1)
            CALL CKF1
         CASE (2)
            IF(LRP.LT.0) THEN
               CALL MF_ERRORS(1)
            ELSE
               IF(ISUBTP.NE.0)   THEN
                  CALL MF_ERRORS(2)
               ELSE
                  CALL CKF2
               END IF
            END IF
         CASE (3)
            IF(ISUBTP.NE.0.AND.ISUBTP.NE.3)  THEN
               CALL MF_ERRORS(2)
            ELSE
               CALL CKF3
            END IF
         CASE (4)
            IF(NFOR.GE.6.AND.NSUB.NE.10) THEN
               CALL MF_ERRORS(3)
            ELSE
               CALL CKF4
            END IF
         CASE (5)
            IF(NFOR.GE.6.AND.(NSUB.NE.10.AND.NSUB.NE.4)) THEN
               CALL MF_ERRORS(2)
            ELSE
               CALL CKF5
            END IF
         CASE (6)
            IF(NFOR.LT.6) THEN
               EMESS = 'MF=6 NOT ALLOWED PRIOR TO ENDF-6'
               CALL ERROR_MESSAGE(0)
            END IF
            IF(ISUBTP.NE.0) THEN
               CALL MF_ERRORS(2)
            ELSE
               CALL CKF6
            END IF
         CASE (7)
            IF(NFOR.GE.6.AND.NSUB.NE.12)  THEN
               CALL MF_ERRORS(2)
            ELSE
               CALL CKF7
            END IF
         CASE (8)
            IF(ISUBTP.EQ.2.OR.ISUBTP.EQ.3)  THEN
               CALL MF_ERRORS(2)
            ELSE
               CALL CKF8
            END IF
         CASE (9:10)
            IF(ISUBTP.NE.0)  THEN
               CALL MF_ERRORS(2)
            ELSE
               CALL CKF9
            END IF
         CASE (12:13)
            IF(NFOR.GE.6.AND.NSUB.NE.10)   THEN
               CALL MF_ERRORS(3)
            ELSE
               CALL CKF12
            END IF
         CASE (14)
            IF(NFOR.GE.6.AND.NSUB.NE.10)   THEN
               CALL MF_ERRORS(3)
            ELSE
               CALL CKF14
            END IF
         CASE (15)
            IF(NFOR.GE.6.AND.NSUB.NE.10)   THEN
               CALL MF_ERRORS(3)
            ELSE
               CALL CKF15
            END IF
         CASE (23)
            IF(NFOR.GE.6.AND.(NSUB.NE.3.AND.NSUB.NE.113))  THEN
               CALL MF_ERRORS(2)
            ELSE
               CALL CKF23
            END IF
         CASE (26)
            IF(NFOR.GE.6.AND.NSUB.NE.113)  THEN
               CALL MF_ERRORS(2)
            ELSE
               CALL CKF26
            END IF
         CASE (27)
            IF(NFOR.GE.6.AND.NSUB.NE.3)  THEN
               CALL MF_ERRORS(2)
            ELSE
               CALL CKF23
            END IF
         CASE (28)
            IF(NFOR.GE.6.AND.NSUB.NE.6)  THEN
               CALL MF_ERRORS(2)
            ELSE
               CALL CKF28
            END IF
         CASE (31)
            IF(NFOR.GE.6.AND.(NSUB.NE.10.AND.NSUB.NE.4))  THEN
               CALL MF_ERRORS(2)
            ELSE
               CALL CKF33
            END IF
         CASE (32)
            IF(ISUBTP.NE.0) THEN
               CALL MF_ERRORS(2)
            ELSE
               CALL CKF32
            END IF
         CASE (33)
            IF(ISUBTP.NE.0)  THEN
               CALL MF_ERRORS(2)
            ELSE
               CALL CKF33
            END IF
         CASE (34)
            IF(ISUBTP.NE.0)  THEN
               CALL MF_ERRORS(2)
            ELSE
               CALL CKF34
            END IF
         CASE (35)
            IF(NFOR.GT.6.AND.(NSUB.NE.10.AND.NSUB.NE.4))  THEN
               CALL MF_ERRORS(2)
            ELSE
               CALL CKF35
            END IF
         CASE (40)
            IF(ISUBTP.NE.0)  THEN
               CALL MF_ERRORS(2)
            ELSE
               CALL CKF40
            END IF
         CASE DEFAULT
            CALL MF_ERRORS(4)
      END SELECT
!
!     MOVE ON TO NEXT SECTION
!
      CALL CHKHD
!
      RETURN
      END SUBROUTINE CHKSEC
!
!***********************************************************************
!
      SUBROUTINE MF_ERRORS(IPATH)
!
!     ROUTINE TO OUTPUT MF RELATED ERROR MESSAGES
!
      IMPLICIT NONE
!
      CHARACTER(LEN=80) :: EMESSP
      INTEGER(KIND=I4) :: IPATH
!
!     SET ERROR FLAG
!
      IERX = 1
      SELECT CASE (IPATH)
         CASE (1)
            EMESSP = 'CANNOT EXIST WHEN LRP = -1'
         CASE (2)
            EMESSP = 'NOT ALLOWED FOR NSUB ='
         CASE (3)
            EMESSP = 'ALLOWED ONLY IN A NEUTRON DATA SUBLIBRARY'
         CASE (4)
            EMESSP = 'IS NOT PERMITTED'
      END SELECT
      IF(IPATH.EQ.2) THEN
         WRITE(EMESSP(23:28),'(I6)') NSUB
      END IF
      WRITE(EMESS,'(A,I3,1X,A)') 'FILE',MF,EMESS
      CALL ERROR_MESSAGE(0)
!
      RETURN
      END SUBROUTINE MF_ERRORS
!
!***********************************************************************
!
      SUBROUTINE CHKHD
!
!     ROUTINE TO CHECK NEXT CONTROL RECORD AND MOVE ON TO NEXT SECTION
!
      IMPLICIT NONE
!
      INTEGER(KIND=I4) :: I
      INTEGER(KIND=I4) :: NSEQB
!
      IF(IERX.NE.0) GO TO 50
!
!     READ CONTROL CARD, SEND EXPECTED
!
      CALL RDHEAD(I)
      IF(IERX.GE.1) GO TO 50
      SELECT CASE (I)
!********ITS A HEAD CARD, SEND MISSING
         CASE (1)
            CALL CONTROL_ERRORS(3)
            GO TO 25
!********ITS A SEND CARD, OK
         CASE (2)
            IF(MF.NE.MFO)   CALL CONTROL_ERRORS(10)
            IF(MAT.NE.MATO)  CALL CONTROL_ERRORS(11)
            GO TO 20
!********ITS A FEND CARD, SEND MISSING
         CASE (3)
            CALL CONTROL_ERRORS(3)
            GO TO 30
!********FEND AND SEND CARDS MISSING
         CASE (4)
            CALL CONTROL_ERRORS(-2)
            GO TO 40
!********SEND, FEND,AND MEND CARDS MISSING
         CASE (5)
            CALL CONTROL_ERRORS(1)
      END SELECT
      GO TO 100
!
!     READ CONTROL CARD, FEND OR HEAD EXPECTED
!
   20 CALL RDHEAD(I)
      IF(IERX.GE.1) GO TO 50
   25 SELECT CASE(I)
!********ITS A HEAD CARD, OK
         CASE (1)
            IF(MAT.LT.MATO)   THEN
               CALL ORDER_ERRORS(1)
               GO TO 100
            ELSE IF(MAT.GT.MATO) THEN
               CALL CONTROL_ERRORS(4)
               GO TO 100
            END IF
            IF(MF.LT.MFO)  THEN
               CALL ORDER_ERRORS(2)
               GO TO 100
            ELSE IF(MF.GT.MFO) THEN
               CALL CONTROL_ERRORS(5)
               GO TO 100
            END IF
            IF(MT.LT.MTO)  THEN
               CALL ORDER_ERRORS(3)
               GO TO 100
            ELSE IF(MT.EQ.MTO) THEN
               CALL ORDER_ERRORS(4)
               GO TO 50
            END IF
!********ITS A SEND CARD, TOO MANY
         CASE (2)
            CALL CONTROL_ERRORS(7)
            GO TO 20
!********ITS A FEND CARD, OK
         CASE (3)
            IF(MAT.NE.MATO) CALL CONTROL_ERRORS(11)
            GO TO 30
!********ITS A MEND CARD, FEND MISSING
         CASE (4)
            CALL CONTROL_ERRORS(5)
            GO TO 40
!********ITS A TEND CARD, FEND AND MEND MISSING
         CASE(5)
            CALL CONTROL_ERRORS(-4)
      END SELECT
      GO TO 100
!
!     READ CONTROL CARD, MEND OR HEAD EXPECTED
!
   30 CALL RDHEAD(I)
      IF(IERX.GE.1) GO TO 50
      SELECT CASE (I)
!********ITS A HEAD CARD, OK
         CASE (1)
            IF(MAT.LT.MATO)  THEN
               CALL ORDER_ERRORS(5)
               GO TO 100
            ELSE IF(MAT.GT.MATO) THEN
               CALL CONTROL_ERRORS(6)
               GO TO 100
            END IF
            IF(MF.LT.MFO)   THEN
               CALL ORDER_ERRORS(6)
            ELSE IF(MF.EQ.MFO) THEN
               CALL ORDER_ERRORS(8)
            END IF
!********ITS A SEND CARD, MISPLACED
         CASE (2)
            CALL ORDER_ERRORS(9)
            GO TO 30
!********ITS A FEND CARD, TOO MANY
         CASE (3)
            CALL CONTROL_ERRORS(8)
            GO TO 30
!******ITS A MEND CARD, OK
         CASE (4)
            NSEQ = 0
            GO TO 40
!********ITS A TEND CARD, MEND CARD MISSING
         CASE (5)
            CALL CONTROL_ERRORS(-6)
      END SELECT
      GO TO 100
!
!     READ CONTROL CARD, HEAD EXPECTED
!
   40 CALL RDHEAD(I)
      IF(IERX.ge.1) GO TO 50
      SELECT CASE (I)
!********ITS A HEAD CARD, OK
         CASE (1)
            IF(MAT.LT.MATO) THEN
               CALL ORDER_ERRORS(7)
            ELSE IF(MAT.EQ.MATO) THEN
               CALL ORDER_ERRORS(10)
            END IF
!********ITS A SEND CARD, MISPLACED
         CASE (2)
            CALL ORDER_ERRORS(11)
            GO TO 40
!********ITS A FEND CARD, MISPLACED
         CASE (3)
            CALL ORDER_ERRORS(12)
            GO TO 40
!********ITS A MEND CARD, TOO MANY
         CASE (4)
            CALL CONTROL_ERRORS(9)
            GO TO 40
!********END OF TAPE.
         CASE (5)
            IFIN = 1
      END SELECT
      GO TO 100
!
!     IF FATAL ERROR FOUND, SKIP REST OF SECTION
!
   50 IERX = 0
      IF(MTP.EQ.0)   GO TO 60
      NSEQB = NSEQ
!
!     READ TO END OF SECTION
!
      DO WHILE (MT.NE.0)
         READ(JIN,'(A)',END=90) IFIELD
         ISEQ = ISEQ + 1
         NSEQ = 0
         READ(IFIELD,'(66X,I4,I2,I3,I5)',ERR=55)  MAT,MF,MT,NSEQ
  55     IF(ASEQ.EQ.' ') NSEQ = ISEQ
      END DO
!
!     MESSAGE TO USER ABOUT SECTION SKIPPED
!
      WRITE(EMESS,'(2A,I6,A,I6)') 'SECTION CANNOT BE',                  &       
     &      ' CHECKED FROM SEQUENCE NUMBER ',NSEQB,' TO',NSEQ
      CALL ERROR_MESSAGE(0)
   60 IF(MAT.LT.0)  THEN
         CALL CONTROL_ERRORS(1)
      ELSE IF (MAT.EQ.0) THEN
         CALL CONTROL_ERRORS(-2)
         GO TO 40
      ELSE
         IF(MF.NE.MFO)   CALL CONTROL_ERRORS(10)
         IF(MAT.NE.MATO)  CALL CONTROL_ERRORS(11)
         GO TO 20
      END IF
      GO TO 100
!
!     UNEXPECTED END OF FILE
!
   90 IERX = 2
!
  100 RETURN
      END SUBROUTINE CHKHD
!
!***********************************************************************
!
      SUBROUTINE CONTROL_ERRORS(I)
!
!     ROUTINE TO OUTPUT CONTROL RECORD RELATED ERROR MESSAGES
!
      IMPLICIT NONE
!
      INTEGER(KIND=I4) :: I
!
      INTEGER(KIND=I4), INTRINSIC :: IABS
!
      INTEGER(KIND=I4) :: II
!
      II = IABS(I)
      SELECT CASE (II)
         CASE (1)
            EMESS = 'SEND, FEND AND MEND CARDS MISSING'
         CASE (2)
            EMESS = 'SEND AND FEND CARDS MISSING'
         CASE (3)
            EMESS = 'SEND CARD MISSING'
         CASE (4)
            EMESS = 'FEND AND MEND CARDS MISSING'
         CASE (5)
            EMESS = 'FEND CARD MISSING'
         CASE (6)
            EMESS = 'MEND CARD MISSING'
         CASE (7)
            EMESS = 'TOO MANY SEND CARDS'
         CASE (8)
            EMESS = 'TOO MANY FEND CARDS'
         CASE (9)
            EMESS = 'TOO MANY MEND CARDS'
         CASE (10)
            EMESS = 'MF INCORRECT'
         CASE (11)
            EMESS = 'MAT INCORRECT'
         CASE DEFAULT
            EMESS = 'UNDEFINED ERROR'
      END SELECT
!
!     PUT OUT ERROR MESSAGE
!
      CALL ERROR_MESSAGE(NSEQP)
!
!     SET ERROR FLAG
!
      IF(I.LT.0) IFIN = 1
!
      RETURN
      END SUBROUTINE CONTROL_ERRORS
!
!***********************************************************************
!
      SUBROUTINE ORDER_ERRORS(I)
!
!     ROUTINE TO OUTPUT ORGANIZATION RELATED ERROR MESSAGES
!
      IMPLICIT NONE
!
      INTEGER(KIND=I4) :: I
!
      SELECT CASE (I)
         CASE (1)
             EMESS = 'FEND AND MEND CARDS MISSING, MAT OUT OF ORDER'
         CASE (2)
             EMESS = 'FEND CARD MISSING, MF OUT OF ORDER'
         CASE (3)
             EMESS = 'MT OUT OF ORDER'
         CASE (4)
             EMESS = 'MT REPEATED'
         CASE (5)
             EMESS = 'MEND CARD MISSING, MAT OUT OF ORDER'
         CASE (6)
             EMESS = 'MF OUT OF ORDER'
         CASE (7)
             EMESS = 'MAT OUT OF ORDER'
         CASE (8)
             EMESS = 'MF REPEATED OR MISPLACED FEND CARD'
         CASE (9)
             EMESS = 'MISPLACED SEND CARD'
         CASE (10)
             EMESS = 'MAT REPEATED OR MISPLACED MEND CARD'
         CASE (11)
             EMESS = 'MISPLACED SEND CARD'
         CASE (12)
             EMESS = 'MISPLACED FEND CARD'
      END SELECT
!
!     PUT OUT ERROR MESSAGE
!
      CALL ERROR_MESSAGE(NSEQP)
!
      RETURN
      END SUBROUTINE ORDER_ERRORS
!
!***********************************************************************
!
      SUBROUTINE CKF1
!
!     ROUTINE TO CHECK FILE 1 DATA
!
      IMPLICIT NONE
!
      INTEGER(KIND=I4) :: NUREP,N
!
      INTEGER(KIND=I4), PARAMETER :: LNUMAX=2,LNUMAXS4=1
!
!     MF = 1, MT = 451
!
      IF(MF.EQ.1.AND.MT.EQ.451)   THEN  ! COMMENTS AND DIRECTORY
         CALL CKS451
         NUREP = 0
         GO TO 100
      END IF
!
!     TEST FOR VALID MT NUMBER FOR THE VALUE OF MF
!
      CALL TESTFT(MTO,MFO)
!
!     CHECK FOR PRESENCE IN DIRECTORY
!
      CALL TESTD(MFO,MTO)
      IF(IERX.EQ.1)    GO TO 100
!
      SELECT CASE (MT)
!
         CASE (452)              ! TOTAL NUBAR
            I452 = 1
            NUREP = L2H
            CALL TEST1(NUREP,1,LNUMAX,'LNU',2)
!
         CASE (455)              ! DELAYED NUBAR
            CALL TESTP(1,452)
            CALL TEST1(L1H,0,1,'LDG',2)
            IF(NSUB.EQ.4) THEN
               CALL TEST1(L2H,1,LNUMAXS4,'LNU',2)
            ELSE
               CALL TEST1(L2H,2,LNUMAX,'LNU',2)
            END IF
            IF(NUREP.NE.0.AND.NUREP.NE.L2H)  THEN
               WRITE(EMESS,'(A,I2,A)') 'LNU =',L2H,                     &       
     &          ' IN MT = 455 REQUIRES THAT LNU IN MT = 452 BE THE SAME'
               CALL ERROR_MESSAGE(0)
            END IF
            IF(L1H.EQ.0) THEN
               CALL RDLIST
            ELSE
               CALL RDTAB2(0)
               DO N=1,NP2
                  CALL RDLIST
               END DO
            END IF
!
         CASE (456)              ! PROMPT NUBAR
            CALL TESTP(1,452)
            IF(NSUB.EQ.4) THEN
               CALL TEST1(L2H,1,LNUMAXS4,'LNU',2)
            ELSE
               CALL TEST1(L2H,2,LNUMAX,'LNU',2)
            END IF
            IF(NUREP.NE.0.AND.NUREP.NE.L2H)  THEN
               WRITE(EMESS,'(A,I2,A)') 'LNU =',L2H,                     &       
     &          ' IN MT = 456 REQUIRES THAT LNU IN MT = 452 BE THE SAME'
               CALL ERROR_MESSAGE(0)
            END IF
!
         CASE (458)          ! ENERGY RELEASE IN FISSION
            CALL RDLIST
            CALL TEST2(NPL,18,'NPL')
            CALL TEST2(N2L,9,'N2L')
            GO TO 100
!
         CASE DEFAULT
            GO TO 100
!
      END SELECT
!
!     CHECK NUBAR REPRESENTATION
!
      IF(IERX.EQ.0) THEN
         IF(L2H.EQ.1)  THEN
            CALL RDLIST
            CALL TEST1(NPL,1,NCOMAX,'NC',1)
         ELSE
            CALL RDTAB1
         END IF
      END IF
!
  100 RETURN
      END SUBROUTINE CKF1
!
!***********************************************************************
!
      SUBROUTINE CKS451
!
!     ROUTINE TO CHECK MT = 451
!
      IMPLICIT NONE
!
!      CHARACTER(LEN=*), INTRINSIC :: TRIM
      INTEGER(KIND=I4), INTRINSIC :: IFIX, MOD, MIN0
!
      CHARACTER(LEN=11) :: ZSA
      INTEGER(KIND=I4) :: IZ,IA,ISTA,IZA,IZ1
      INTEGER(KIND=I4) :: NREL
      INTEGER(KIND=I4) :: NCD,NID
      INTEGER(KIND=I4) :: MATCHK
      INTEGER(KIND=I4) :: JPART,JTYPE
      INTEGER(KIND=I4) :: IEF
      INTEGER(KIND=I4) :: MFT,MFTP
      INTEGER(KIND=I4) :: N1
      INTEGER(KIND=I4) :: K,N,NC
      REAL(KIND=R4) :: EMAX,TEMP
!
      INTEGER(KIND=I4), PARAMETER :: NPARTS = 8
      INTEGER(KIND=I4), DIMENSION(NPARTS), PARAMETER ::                  &      
     &         IPART = (/0,1,11,1001,1002,1003,2003,2004/)
!
!     DEFINE ELEMENT SYMBOLS
!
      INTEGER(KIND=I4), PARAMETER :: IELM=113
      CHARACTER(LEN=2), DIMENSION(IELM), PARAMETER ::                   &       
     &  ELEMNT = (/'nn',                                                &       
     &       'H ','He','Li','Be','B ','C ','N ','O ','F ','Ne',         &       
     &       'Na','Mg','Al','Si','P ','S ','Cl','Ar','K ','Ca',         &       
     &       'Sc','Ti','V ','Cr','Mn','Fe','Co','Ni','Cu','Zn',         &       
     &       'Ga','Ge','As','Se','Br','Kr','Rb','Sr','Y ','Zr',         &       
     &       'Nb','Mo','Tc','Ru','Rh','Pd','Ag','Cd','In','Sn',         &       
     &       'Sb','Te','I ','Xe','Cs','Ba','La','Ce','Pr','Nd',         &       
     &       'Pm','Sm','Eu','Gd','Tb','Dy','Ho','Er','Tm','Yb',         &       
     &       'Lu','Hf','Ta','W ','Re','Os','Ir','Pt','Au','Hg',         &       
     &       'Tl','Pb','Bi','Po','At','Rn','Fr','Ra','Ac','Th',         &       
     &       'Pa','U ','Np','Pu','Am','Cm','Bk','Cf','Es','Fm',         &       
     &       'Md','No','Lr','Rf','Db','Sg','Bh','Hs','Mt','Ds',         &       
     &       'Rg','XX'/)
!
!     SET CONTROL PARAMETERS FROM THE FIRST MATERIAL RECORD
!
      ZA = C1H
      AWR = C2H
      LRP = L1H
      LFI = L2H
      NLIB = N1H
      NMOD = N2H
      IZ = IFIX(ZA)/1000
      IA = MOD(IFIX(ZA),1000)
!
!     CHECK PARAMETERS
!
      CALL TEST1(LRP,-1,2,'LRP',1)
      CALL TEST1(LFI,0,1,'LFI',1)
      CALL TEST1(NLIB,0,99,'LIB',1)
      CALL TEST1(NMOD,0,20,'MOD',1)
!
!     READ THE NEXT CONTROL RECORD AND SET PARAMETERS
!
      CALL RDCONT
      STA = C2H
      LIS = L1H
      LISO = L2H
      NFOR = N2H
!
!     CHECK PARAMETERS
!
      ISTA = IFIX(STA)
      CALL TEST1(ISTA,0,1,'STA',1)
      IF(LIS.EQ.0)  THEN
         CALL TEST2F(C1H,0.,'ELIS')
         CALL TEST2(LISO,0,'LISO')
      ELSE
         CALL TEST1(LISO,1,LIS,'LISO',1)
      END IF
!
!     ENDF-V FORMAT FILE
!
      IF(NFOR.EQ.0)   THEN
         NFOR = 5
         NSUB = 10
         SELECT CASE (NLIB)
            CASE (2:4,35)
               NVER = 1
            CASE (5)
               NVER = 2
            CASE (6)
               NVER = 3
            CASE DEFAULT
               NVER = 5
         END SELECT
         AWI = 1.
         NSEQP1 = NSEQP1 - 1
         CALL TEST1(LRP,0,1,'LRP',1)
         NSEQP1 = NSEQP1 + 1
         GO TO 40
      ELSE
         CALL TEST1(NFOR,6,6,'NFOR',1)
      END IF
!
!     ENDF-VI OR LATER FORMAT, READ ANOTHER CONTROL RECORD
!
      CALL RDCONT
      AWI = C1H
      EMAX = C2H
      NREL = L1H
      NSUB = N1H
      NVER = N2H
      NFOR = MAX0(6,NFOR)
!
!     CHECK NREL
!
      CALL TEST1(NREL,0,20,'NREL',1)
!
!     CHECK EMAX
!
      SELECT CASE (NSUB)
         CASE (10)
            CALL TEST1F(EMAX,20.E+6,500.E+6,'EMAX')
         CASE (3,113)
            CALL TEST1F(EMAX,20.E+6,100.E+9,'EMAX')
         CASE (4,6)
            CALL TEST2F(EMAX,0.,'EMAX')
         CASE DEFAULT
            CALL TEST1F(EMAX,1.E+6,500.E+6,'EMAX')
      END SELECT
!
!     CHECK MAT NUMBER AGAINST ZA FOR ENDF/B
!
      IF(NFOR.GE.6) THEN
         IF((NLIB.EQ.0.OR.NLIB.EQ.4)) THEN
            CALL MATASS(IZ,IA,LISO,MATCHK)
         ELSE IF(NLIB.EQ.2) THEN
            CALL MATASS_JEF(IZ,IA,LISO,MATCHK)
         ELSE
            GO TO 10
         END IF
         IF(MATCHK.NE.MAT) THEN
            IF(MATCHK.EQ.0)  THEN
               WRITE(EMESS,'(A,I4,A,F8.1,A,I2)')                        &       
     &               'ASSIGNED MATERIAL NUMBER (MAT=',MAT,              &       
     &               ') NOT CONSISTENT WITH ZA = ',ZA,' LISO =',LISO
               CALL ERROR_MESSAGE(0)
            ELSE
               WRITE(EMESS,'(A,I5)')                                    &       
     &                '    MATERIAL NUMBER SHOULD BE',MATCHK
               CALL ERROR_MESSAGE(0)
            END IF
         END IF
      END IF
!
!     CHECK VALUES OF CONTROL VARIABLES
!
  10  IF(NVER.GE.1990.AND.NVER.LE.IYR) GO TO 15
      CALL TEST1(NVER,0,99,'NVER',1)
!
!     CHECK FOR A VALID SUB LIBRARY NUMBER
!
  15  NSEQP1 = NSEQP1 + 1
      JPART = NSUB/10
      JTYPE = MOD(NSUB,10)
      DO K=1,NPARTS
         IF(JPART.EQ.IPART(K))  THEN
            IF(JTYPE.LT.0.OR.JTYPE.GT.6)   GO TO 20
            IF(JPART.EQ.0.AND.JTYPE.EQ.2)  GO TO 20
            IF(JPART.EQ.1.AND.JTYPE.GT.2)  GO TO 20
            IF(JPART.EQ.11.AND.JTYPE.NE.3)  GO TO 20
            IF(JPART.GT.11.AND.JTYPE.GT.1)  GO TO 20
            GO TO 30
         END IF
      END DO
      WRITE(EMESS,'(A,I6)')                                             &       
     &          'POSSIBLE INVALID SUBLIBRARY NUMBER NSUB =',NSUB
      CALL ERROR_MESSAGE(NSEQP1)
      GO TO 40
   20 WRITE(EMESS,'(A,I6)')                                             &       
     &          'INVALID SUBLIBRARY NUMBER NSUB =',NSUB
      CALL ERROR_MESSAGE(NSEQP1)
      GO TO 40
!*****CHECK LRP AGAINST NSUB
   30 IEF = 0
      IF(NSUB.EQ.10) THEN
         IF(LRP.EQ.-1)  IEF = 1
      ELSE
         IF(LRP.EQ.0)   IEF = 1
      END IF
      IF(IEF.EQ.1) THEN
         WRITE(EMESS,'(A,I2,A,I5)')                                     &       
     &                 'INVALID FLAG LRP=',LRP,' FOR NSUB=',NSUB
         CALL ERROR_MESSAGE(NSEQP1-2)
      END IF
!
!     PROCESS LAST CONTROL RECORD
!
   40 CALL RDCONT
!
!     CHECK PARAMETERS
!
      LDRV = L1H
      TEMP = C1H
      NCD = N1H
      IF(NFOR.GE.6)   THEN
         CALL TEST1(LDRV,0,999,'LDRV',1)
         IF(LDRV.EQ.0)   THEN
            CALL TEST2F(TEMP,0.,'TEMP')
            IF(LRP.EQ.2)  THEN
               EMESS = 'LRP=2 ONLY ALLOWED IN DERIVED FILES'
               CALL ERROR_MESSAGE(NSEQP1-3)
            END IF
        END IF
      ELSE
         CALL TEST2(LDRV,0,'LDRV')
         CALL TEST2F(TEMP,0.,'TEMP')
         CALL TEST1(NCD,NCDMI,NCDMA,'NCD',1)
      END IF
!
!     BUILD Z-S-A FOR CARD 5, FIELD 1 TEST
!
      IF(NSUB.NE.12) THEN
         ZSA = ' '
         IZA = IFIX(ZA+.001)
         IA = MOD(IZA,1000)
         IZ = IZA/1000
         IZ1 = MIN0((IZ+1),IELM)
         WRITE(ZSA,'(I3,3A,I3)') IZ,'-',ELEMNT(IZ1),'-',IA
         IF(LISO.GE.3) THEN
            ZSA(11:11) = 'O'
         ELSE IF(LISO.GE.2) THEN
            ZSA(11:11) = 'N'
         ELSE IF(LISO.GE.1) THEN
           ZSA(11:11) = 'M'
         END IF
      END IF
!
!     READ IN COMMENT RECORDS
!
      IF(NFOR.GE.6)  THEN
         NID = 5
      ELSE
         NID = 2
      END IF
      IF(IMDC.LT.4) WRITE(IOUT,'(1X,A)') ' '
      DO NC=1,NCD
         CALL RDTEXT
         IF(IERX.EQ.1)   GO TO 100
         IF(NC.EQ.1) THEN
            IF(NSUB.NE.12.AND.ZSA.NE.TEXT(1:11)) THEN
               EMESS = 'ZSYNAM SHOULD BE '//TRIM(ZSA)//' NOT '//        &       
     &                 TRIM(TEXT(1:11))
               CALL ERROR_MESSAGE(NSEQP)
            END IF
         END IF
         IF(NC.LE.NID)   THEN
            IF(IMDC.LT.4)  WRITE(IOUT,'(1X,A66)')   TEXT
         END IF
      END DO
      IF(IMDC.LT.4) WRITE(IOUT,'(1X,A)') ' '
!
!     PROCESS THE DIRECTORY
!
      NXC = N2H
      NXC0 = NXC
      CALL TEST1(NXC,0,NSECMAX,'NXC',2)
      IF(IERX.EQ.1) GO TO 100
!
!     MESSAGE IF DIRECTORY MISSING
!
      IF(NXC0.EQ.0) THEN
         EMESS = 'DIRECTORY MISSING'
         CALL ERROR_MESSAGE(NSEQP)
         EMESS = '    ALL TESTS WHICH DEPEND ON ITS PRESENCE WILL '//   &       
     &             'NOT BE DONE'
         CALL ERROR_MESSAGE(0)
         GO TO 100
      END IF
!
!     CHECK THE DIRECTORY
!
      MFTP = 0
      N1 = 0
      DO N=1,NXC
         CALL RDCONT
!********SECTION MOD NUMBER CANNOT EXCEED THE MATERIAL MOD NUMBER
         CALL TEST1(N2H,0,NMOD,'MOD',1)
!********FIRST ENTRY IN THE DIRECTORY MUST BE 1/451
         MFT = 1000*L1H + L2H
         IF(N.EQ.1.AND.MFT.NE.1451) THEN
            EMESS = 'FIRST DIRECTORY ENTRY IS NOT MF=1, MT=451'
            CALL ERROR_MESSAGE(NSEQP)
         END IF
!***********DIRECTORY MUST BE IEASING ORDER OF MF AND MT
         IF(MFT.LE.MFTP)   THEN
            EMESS = 'DIRECTORY OUT OF ORDER'
            CALL ERROR_MESSAGE(NSEQP)
         ELSE
!***********STORE MF AND MT IN THE INDEX
            N1 = N1 + 1
            INDX(N1,1) = MFT
            INDX(N1,2) = 1
            MFTP = MFT
         END IF
         IF(IERX.EQ.1)   GO TO 100
      END DO
      NXC = N1
!
!     CHECK THAT SECTION IS IN THE DIRECTORY
!
  100 CALL TESTD(MFO,MTO)
!
      RETURN
      END SUBROUTINE CKS451
!
!***********************************************************************
!
      SUBROUTINE MATASS(IZ,IA,LIS0,MATCHK)
!
!     FUNCTION TO CONVERT A  CHARGE NUMBER, A MASS NUMBER AND A
!       LEVEL NUMBER INTO A MATERIAL NUMBER FOR ENDF/B.
!
      IMPLICIT NONE
!
      INTEGER(KIND=I4) :: IZ,IA,LIS0,MATCHK
!
      INTEGER(KIND=I4) :: NMAT
      INTEGER(KIND=I4) :: ICENT
      INTEGER(KIND=I4) :: IASS
      INTEGER(KIND=I4) :: N
!
      INTEGER(KIND=I4), PARAMETER :: NELS = 103
      INTEGER(KIND=I4), DIMENSION (NELS), PARAMETER ::                   &      
     &   IMEAN = (/                                                     &       
     &   1,3,6,9,10,12,14,16,19,20,23,24,27,28,31,32,35,36,39,40,45,46, &       
     &   50,50,55,54,59,58,63,64,69,70,75,74,79,78,85,84,89,90,93,92,   &       
     &   99,96,103,102,107,106,113,112,121,120,127,124,133,130,138,136, &       
     &   141,142,139,144,151,152,159,156,165,162,169,168,175,174,180,   &       
     &   180,185,184,191,190,197,196,203,204,209,206,203,211,212,223,   &       
     &   225,227,229,234,230,235,235,240,240,240,241,240,245,248,252/)
!
      INTEGER(KIND=I4), DIMENSION(4) :: ISP
      DATA ISP /9920,9945,9965,9980/
!
      INTEGER(KIND=I4), PARAMETER :: NCOMPS = 18
      INTEGER(KIND=I4), DIMENSION(NCOMPS), PARAMETER::                   &      
     &        ZACOMP = (/101,102,103,107,111,112,113,126,127,128,       &       
     &                   131,133,134,137,140,158,175,176/)
!
!     INITIALIZE
!
      MATCHK = 0
      IF(IZ.LT.0.OR.IA.LT.0)   GO TO 100
      IF(IZ.EQ.0)  GO TO 50
!*****ONLY GROUND STATE AND TWO LEVELS ALLOWED
      IF(LIS0.LT.0.OR.LIS0.GT.2)   GO TO 100
!*****INITIALIZE ELEMENT RELATED PORTION OF THE MATERIAL NUMBER
      IF(IZ.LE.99)   THEN
         NMAT = 100*IZ
      ELSE
         NMAT = ISP(IZ-99)
      END IF
!
!     INTERPRET MASS NUMBER
!
      IASS = 0
!*****CHECK FOR NATURAL ELEMENT
      IF(IA.NE.0)   THEN
!*****GET MASS DEPENDENT PART OF MATERIAL NUMBER
         ICENT = IMEAN(IZ)
         IF(IZ.LT.99)  THEN
            IASS = 3*(IA-ICENT) + 25 + LIS0
            IF(IZ.EQ.48.AND.IA.GE.127)   IASS = IASS - (IA-126)
         ELSE
            IASS = IA - ICENT + 1
         END IF
      END IF
      IF(IASS.LT.0.OR.IASS.GT.99)   GO TO 100
!*****COMPLETE MASS NUMBER
      MATCHK = NMAT + IASS
      GO TO 100
!
!     COMPOUNDS
!
   50 DO N=1,NCOMPS
         IF(IA.EQ.ZACOMP(N)) THEN
            MATCHK = ZACOMP(N) - 100
            GO TO 100
         END IF
      END DO
!
  100 RETURN
      END SUBROUTINE MATASS
!
!***********************************************************************
!
      SUBROUTINE MATASS_JEF(IZ,IA,LIS0,MATCHK)
!
!     FUNCTION TO CONVERT A  CHARGE NUMBER, A MASS NUMBER AND A
!       LEVEL NUMBER INTO A MATERIAL NUMBER FOR JEF.
!
      IMPLICIT NONE
!
      INTEGER(KIND=I4) :: IZ,IA,LIS0, MATCHK
!
      INTEGER(KIND=I4) :: NMAT
      INTEGER(KIND=I4) :: IDIFF
!
      INTEGER(KIND=I4), PARAMETER :: NELS = 104
      INTEGER(KIND=I4), DIMENSION (NELS), PARAMETER ::                  &       
     &    IMEAN = (/0,1,3,6,9,10,12,14,16,19,20,23,24,27,28,31,32,      &       
     &             35,36,39,40,45,46,50,50,55,54,59,58,63,64,69,        &       
     &             70,75,74,79,78,85,84,89,90,93,92,97,96,103,          &       
     &             102,107,106,113,112,121,120,127,124,133,130,         &       
     &             138,136,141,142,139,144,151,152,159,156,165,         &       
     &             162,169,168,175,174,180,180,185,184,191,190,         &       
     &             197,196,203,204,209,206,203,211,212,223,225,         &       
     &             227,229,234,230,235,235,240,240,240,241,242,         &       
     &             247,250,253/)
!
      INTEGER(KIND=I4), DIMENSION(4) :: ISP
      DATA ISP/9930,9960,9980,9990/
!
      MATCHK = 0
      IDIFF = IA - IMEAN(IZ+1)
      NMAT = 100*IZ
      IF(IZ.GE.100) NMAT = ISP(IZ-99)
!
      IF(IA.NE.0) THEN
         IF(IZ.LT.99) NMAT = NMAT + 3*(IDIFF) + 25 + LIS0
         IF(IZ.EQ.48.AND.IA.GE.127) NMAT = NMAT - (IA-126)
         IF(IZ.GE.99.AND.IZ.LE.101) THEN
            IF(IDIFF.LT.7) THEN
               NMAT = NMAT + IDIFF
            END IF
            IF(IDIFF.GE.7.AND.IDIFF.LE.16) THEN
               NMAT = NMAT + 2*IDIFF - 7 + LIS0
            END IF
            IF(IDIFF.GT.16) THEN
               NMAT = NMAT + IDIFF + 10
            END IF
         END IF
         IF(IZ.GT.101) NMAT = NMAT + IDIFF
      END IF
      MATCHK = NMAT
!
      RETURN
      END SUBROUTINE MATASS_JEF
!
!***********************************************************************
!
      SUBROUTINE CKF2
!
!     ROUTINE TO CHECK FILE 2 DATA
!
      IMPLICIT NONE
!
      INTEGER(KIND=I4) :: NIS
      INTEGER(KIND=I4) :: MLRP
      INTEGER(KIND=I4) :: JPOT
      INTEGER(KIND=I4) :: LFW,LRU
      INTEGER(KIND=I4) :: NER,NERM
      INTEGER(KIND=I4) :: N,NI
!
!     TEST FOR A VALID MT NUMBER
!
      CALL TESTFT(MTO,MFO)
!
!     SEE IF SECTION IS IN THE DIRECTORY
!
      CALL TESTD(MFO,MTO)
      IF(IERX.EQ.1)   GO TO 100
!
!     TEST NUMBER OF ISOTOPES
!
      NIS = N1H
      CALL TEST1(NIS,1,NISMAX,'NIS',1)
!
!     PROCESS EACH ISOTOPE
!
      MLRP = 0
      JPOT = 0
      DO NI=1,NIS
         CALL RDCONT
!********TEST ZA AND ABUNDANCE
         IF(NIS.EQ.1)  THEN
            CALL TEST2F(C1H,ZA,'ZAI')
            CALL TEST2F(C2H,1.0,'ABN')
         END IF
!********TEST LFW
         LFW = L2H
         CALL TEST1(LFW,0,1,'LFW',2)
!********TEST NUMBER OF ENERGY RANGES
         NER = N1H
         IF(NFOR.GE.6) THEN
            NERM = NERM6
         ELSE
            NERM = NERM5
         END IF
         CALL TEST1(NER,1,NERM,'NER',1)
         IF(IERX.EQ.1) GO TO 100
!
!        PROCESS EACH ENERGY RANGE
!
         DO N=1,NER
            CALL RDCONT
!***********TEST LRU
            LRU = L1H
            CALL TEST1(LRU,0,2,'LRU',2)
            IF(LRU.GT.0) MLRP = 1
            IF(NFOR.LT.6)  THEN
               CALL TEST2(N1H,0,'N1H')
               CALL TEST2(N2H,0,'N2H')
            END IF
!
!           BRANCH ON REPRESENTATION
!
            SELECT CASE (LRU)
!**************ONLY POTENTIAL SCATTERING
               CASE (0)
                  CALL NORCHECK(NER,LFW)
                  JPOT = 1
               CASE (1)
!**************RESOLVED RESONANCE REGION
                  CALL RR_CHECK
               CASE (2)
!**************UNRESOLVED RESONANCE REGION
                  IF(N.NE.NER) THEN
                     EMESS = 'ONLY ONE UNRESOLVED RESONANCE REGION IS '
                     CALL ERROR_MESSAGE(0)
                     EMESS = ' PERMITTED AND IT MUST BE THE LAST ONE.'
                     CALL ERROR_MESSAGE(NSEQP)
                  END IF
                  CALL UR_CHECK(LFW)
            END SELECT
            IF(IERX.EQ.1)  GO TO 100
         END DO
      END DO
!
!     CHECK FOR VALID LRP FLAG IN FILE 1/451
!
      IF(MLRP.EQ.0) THEN
         IF(LRP.NE.0)  THEN
            EMESS ='LRP FLAG MUST BE ZERO IN 1/451 BECAUSE ONLY '//     &       
     &              'POTENTIAL'
            CALL ERROR_MESSAGE(0)
            EMESS = '    SCATTERING IS GIVEN IN FILE 2 FOR ALL '//      &       
     &              'ISOTOPES.'
            CALL ERROR_MESSAGE(0)
         END IF
      ELSE
         IF(LRP.EQ.0)  THEN
            EMESS = 'LRP FLAG MAY NOT BE ZERO IN 1/451 BECAUSE '//      &       
     &              'RESONANCE '
            CALL ERROR_MESSAGE(0)
            EMESS = 'PARAMETERS ARE GIVEN FOR AT LEAST ONE ISOTOPE.'
            CALL ERROR_MESSAGE(0)
         END IF
      END IF
!
!     CHECK FOR PROPER USE OF LRU = 0
!
      IF(NFOR.GE.6.AND.NIS.NE.1) THEN
         IF(JPOT.EQ.1.AND.MLRP.EQ.0)  THEN
            EMESS = 'THERE MUST BE RESONANCE PARAMETERS GIVEN FOR '//   &       
     &          'AT LEAST ONE ISOTOPE.'
            CALL ERROR_MESSAGE(0)
         END IF
      END IF
!
  100 RETURN
      END SUBROUTINE CKF2
!
!***********************************************************************
!
      SUBROUTINE RR_CHECK
!
!     ROUTINE TO CHECK THE RESOLVED RESONANCE REGION
!
      IMPLICIT NONE
!
      INTEGER(KIND=I4) :: LRF,LRFM
      INTEGER(KIND=I4) :: NRO
      INTEGER(KIND=I4) :: NAPS,NAPSM
      INTEGER(KIND=I4) :: NLS,NLSC
      REAL(KIND=R4) :: AP
!
      LRF = L2H
!
!     TEST SCATTERING LENGTH REPRESENTATION
!
      NRO = N1H
      IF(NFOR.GE.6)  THEN
         CALL TEST1(NRO,0,1,'NRO',1)
      END IF
      IF(NFOR.GE.6)  THEN
         NAPS = N2H
         IF(NRO.EQ.1) THEN
            NAPSM = 2
         ELSE
            NAPSM = 1
         END IF
         CALL TEST1(NAPS,0,NAPSM,'NAPS',1)
         IF(IERX.EQ.1)  GO TO 100
      END IF
!
!     READ ENERGY DEPENDENT SCATTERING LENGTH
!
      IF(NRO.GT.0) THEN
         CALL RDTAB1
         CALL TEST1(NP,1,NSCLMAX,'NP',1)
         IF(IERX.EQ.1) GO TO 100
      END IF
!
!     TEST LRF
!
      IF(NFOR.GE.6) THEN
         LRFM = LRFM6
      ELSE
         LRFM = LRFM5
      END IF
      CALL TEST1(LRF,1,LRFM,'LRF',2)
      IF(IERX.EQ.1) GO TO 100
!
!     GENERALIZED R-MATRIX NOT IMPLEMENTED
!
      IF(LRF.EQ.5)  THEN
         IF(NLIB.NE.0)  THEN
            EMESS = 'LRF = 5, GENERALIZED R-MATRIX '//                  &       
     &            'REPRESENTATION NOT YET IMPLEMENTED'
            CALL ERROR_MESSAGE(0)
         ELSE
            EMESS = 'LRF = 5, GENERALIZED R-MATRIX '//                  &       
     &             'REPRESENTATION NOT ALLOWED IN ENDF/B-VI'
            CALL ERROR_MESSAGE(0)
         END IF
         IERX = 1
         GO TO 100
      END IF
!
!     READ NUMBER OF PARTIAL WAVES
!
      CALL RDCONT
!*****CHECK NUMBER OF PARTIAL WAVES (L-VALUES)
      NLS = N1H
      IF(NFOR.GE.6) THEN
         NLSC = N2H
         CALL TEST1(NLS,1,NLSMAX,'NLS',1)
         IF(LRF.EQ.3.OR.LRF.EQ.6)  THEN
            CALL TEST1(NLSC,1,NLSCMAX,'NLSC',1)
            IF(NLSC.LT.NLS)  THEN
               EMESS = 'NLSC CANNOT BE LESS THAN NLS'
               CALL ERROR_MESSAGE(NSEQP)
            END IF
            CALL TEST1(L1H,0,1,'LAD',1)
         END IF
      END IF
!
!     CHECK VALUE OF AP
!
      AP = C2H
      IF(NFOR.GE.6) THEN
         IF(LRF.EQ.6.OR.(NAPS.NE.2.AND.NRO.EQ.1))                       &       
     &        CALL TEST2F(AP,0.0,'AP')
      END IF
!
!     ALL PARAMETERIZATIONS
!
   80 SELECT CASE (LRF)
         CASE (1:2)
            CALL CHECK_BW(NLS,LRF)
         CASE (3)
            IF(NFOR.LE.5.AND.NLIB.EQ.0)  THEN
               EMESS = 'LRF = 3, REICH-MOORE REPRESENTATION NOT '//     &       
     &             'ALLOWED IN ENDF/B-V'
               CALL ERROR_MESSAGE(0)
            END IF
            CALL CHECK_BW(NLS,LRF)
         CASE (4)
            CALL CHECK_AA(NLS)
         CASE (6)
            CALL CHECK_HR(NLS)
      END SELECT
!
  100 RETURN
      END SUBROUTINE RR_CHECK
!
!***********************************************************************
!
      SUBROUTINE CHECK_BW(NLS,LRF)
!
!     ROUTINE TO CHECK BW, MULTI-LEVEL BW AND RM REPRESENTATIONS
!
      IMPLICIT NONE
!
      INTEGER(KIND=I4) :: NLS,LRF
!
      INTEGER(KIND=I4) :: NRS,NUM
      INTEGER(KIND=I4) :: NL
!
      INTEGER(KIND=I4), PARAMETER :: NREP=6
!
      DO NL=1,NLS
         CALL RDLIST
!********CHECK ON COMPETITIVE WIDTH
         IF(LRF.LE.2)   THEN
            IF(L2L.EQ.0)   THEN
               CALL TEST2F(C2L,0.0,'QX')
            ELSE
               CALL TEST1(L2L,0,1,'LRX',1)
               IF(L2L.EQ.1.AND.C2L.GE.0.0) THEN
                  EMESS = 'QX MUST BE LESS THAN 0.0'
                  CALL ERROR_MESSAGE(NSEQP)
               END IF
            END IF
         END IF
!********CHECK ON NUMBER OF RESONANCES ALLOWED
         NRS = N2L
         CALL TEST1(NRS,1,NRESMAX,'NRS',1)
         IF(IERX.EQ.1) GO TO 100
!********CHECK NUMBER OR DATA ITEMS PER RESONANCE
         NUM = NPL/NREP
         CALL TEST2(NRS,NUM,'NRS')
      END DO
!
  100 RETURN
      END SUBROUTINE CHECK_BW
!
!***********************************************************************
!
      SUBROUTINE CHECK_AA(NLS)
!
!     ROUTINE TO CHECK ADLER-ADLER REPRESENTATION
!
      IMPLICIT NONE
!
      INTEGER(KIND=I4) :: NLS
!
      INTEGER(KIND=I4) :: NJS
      INTEGER(KIND=I4) :: NX,NRS,NUM
      INTEGER(KIND=I4) :: LI
      INTEGER(KIND=I4) :: NL,NJ
!
!     PROCESS ADLER-ADLER REPRESENTATION
!
      CALL RDLIST
!*****TEST NUMBER OF SETS OF BACKGROUND PARAMETERS
      NX = N2L
      CALL TEST1(NX,1,3,'NX',1)
!*****TEST NUMBER OF PARAMETERS PER SET
      NUM = NPL/NREP6
      CALL TEST2(NX,NUM,'NX')
!*****TEST LI
      LI = L1L
      IF(NLIB.EQ.0)  THEN
         CALL TEST1(LI,5,7,'LI',1)
         IF(LI.EQ.6) THEN
            EMESS ='LI=6 NOT PERMITTED'
            CALL ERROR_MESSAGE(NSEQP)
         END IF
      ELSE
         CALL TEST1(LI,1,7,'LI',1)
      END IF
      IF(IERX.EQ.1) GO TO 100
!
!     PROCESS EACH PARTIAL WAVE (L VALUE)
!
      DO NL=1,NLS
         CALL RDCONT
!********TEST NUMBER OF J STATES
         NJS = N1H
         CALL TEST1(NJS,1,NJSMAX,'NJS',1)
         IF(IERX.EQ.1) GO TO 100
!
!        PROCESS EACH J STATE
!
         DO NJ=1,NJS
            CALL RDLIST
!********TEST NUMBER OR RESONANCES
            NRS = N2L
            CALL TEST1(NRS,1,NRESMAX,'NRS',1)
            NUM = NPL/NREP12
            CALL TEST2(NRS,NUM,'NRS')
            IF(IERX.EQ.1) GO TO 100
         END DO
      END DO
!
  100 RETURN
      END SUBROUTINE CHECK_AA
!
!***********************************************************************
!
      SUBROUTINE CHECK_HR(NLS)
!
!     ROUTINE TO CHECK THE HYBRID R-MATRIX REPRESENTATION
!
      IMPLICIT NONE
!
      INTEGER(KIND=I4) :: NLS
!
      INTEGER(KIND=I4) :: NSS,NJS,NLSJ
      INTEGER(KIND=I4) :: NGRE,NFRE,NIRE,NCRE,NMTRE
      INTEGER(KIND=I4) :: NCR,LIL,IJI
      INTEGER(KIND=I4) :: L
      INTEGER(KIND=I4) :: LBK,LPS
      INTEGER(KIND=I4) :: NUM
      INTEGER(KIND=I4) :: NL,NS,NJ
!
      INTEGER(KIND=I4), PARAMETER :: NMTRES=4
      INTEGER(KIND=I4), DIMENSION(NMTRES) :: MTRE
!
!     HYBRID R-FUNCTION
!
      CALL RDCONT
!*****CHECK REACTION CHANNEL COUNTS
      NGRE = L1H
      CALL TEST1(NGRE,0,NGREMAX,'NGRE',0)
      NFRE = L2H
      CALL TEST1(NFRE,0,NFREMAX,'NFRE',0)
      NIRE = N1H
      CALL TEST1(NIRE,0,NIREMAX,'NIRE',0)
      NCRE = N2H
      CALL TEST1(NCRE,0,NCREMAX,'NCRE',0)
      NMTRE = NIRE + NCRE
      IF(NMTRE.GT.NMTRES) THEN
         WRITE(EMESS,'(A,I2)')                                          &       
     &            'NUMBER OF COMPETING REACTIONS EXCEEDS ',NMTRES
         CALL ERROR_MESSAGE(NSEQP)
      END IF
      CALL RDCONT
!*****REACTION CHANNEL DEFINITIONS
      MTRE(1) = L1H
      MTRE(2) = L2H
      MTRE(3) = N1H
      MTRE(4) = N2H
      CALL CKMTRE(MTRE,NIRE,NCRE)
!*****CHECK REACTION CHANNEL Q-VALUES
      CALL RDLIST
      CALL TEST2(NPL,NMTRES,'NPL')
      DO IJI=1,NMTRES
         IF(MTRE(IJI).EQ.0.AND.BIN1(IJI).NE.0.0) THEN
            WRITE(EMESS,'(A,I1,A,I1,A)')                                &       
     &            'IF MTRE',IJI,' IS 0 THEN QRE',IJI,' MUST BE 0.0'
            CALL ERROR_MESSAGE(NSEQP)
         END IF
      END DO
!*****READ ANY CHARGED PARTICLE PENETRABILITIES
      IF(NCRE.GT.0)  THEN
         DO NCR=1,NCRE
            DO LIL=1,4
               CALL RDTAB1
            END DO
         END DO
      END IF
!
!     PROCESS EACH L, S, AND J VALUE
!
      DO NL=1,NLS
         CALL RDCONT
         L = L1H
         CALL TEST1(L,0,NLSMAX-1,'L',1)
         NSS = N1H
!********CHANNEL SPIN
         DO NS=1,NSS
            CALL RDCONT
            NJS = N1H
!*****TOTAL SPIN
            DO NJ=1,NJS
               CALL RDLIST
!*****CHECK BACKGROUND AND PHASE SHIFT FLAGS
               LBK = L1L
               CALL TEST1(LBK,0,1,'LBK',1)
               LPS = L2L
               CALL TEST1(LPS,0,1,'LPS',1)
!*****TEST NUMBER OR RESONANCES
               NLSJ = N2L
               CALL TEST1(NLSJ,1,NRESMAX,'NLSJ',1)
               NUM = NPL/NREP12
               CALL TEST2(NLSJ,NUM,'NLSJ')
               IF(IERX.EQ.1) GO TO 100
!*****READ BACKGROUND
               IF(LBK.NE.0) THEN
                  CALL RDTAB1
                  CALL RDTAB1
               END IF
!*****READ PHASE SHIFTS
               IF(LPS.NE.0)  THEN
                  CALL RDTAB1
                  CALL RDTAB1
               END IF
            END DO
         END DO
      END DO
!
  100 RETURN
      END SUBROUTINE CHECK_HR
!
!***********************************************************************
!
      SUBROUTINE UR_CHECK(LFW)
!
!     ROUTINE TO CHECK THE UNRESOLVED RESONANCE REGION
!
      IMPLICIT NONE
!
      INTEGER(KIND=I4) :: LFW
!
      INTEGER(KIND=I4) :: LRF
      INTEGER(KIND=I4) :: LSSF,LSSFC
      INTEGER(KIND=I4) :: NLS,NJS
      INTEGER(KIND=I4) :: NL,NJ
      INTEGER(KIND=I4) :: NE,NET
      INTEGER(KIND=I4) :: NUM
!
      INTEGER(KIND=I4), PARAMETER :: LSSFC6 = 1,LSSFC5 = 0
!
!     PROCESS UNRESOLVED RESONANCE REGION
!
      LRF = L2H
      CALL TEST1(LRF,1,2,'LRF',1)
!
!     ALL PARAMETERS ENERGY DEPENDENT
!
      IF(LRF.EQ.2) THEN
         CALL RDCONT
!*****CHECK LSSF FLAG (WHETHER TO ADD RESONANCE CONTRIBUTION TO FILE 3)
         IF(NFOR.GE.6) THEN
            LSSFC = LSSFC6
         ELSE
            LSSFC = LSSFC5
         END IF
         LSSF = L1H
         CALL TEST1(LSSF,0,LSSFC,'LSSF',1)
         IF(IERX.EQ.1)   GO TO 100
!********PROCESS EACH L VALUE
         NLS = N1H
         CALL TEST1(NLS,1,NLSMAX,'NLS',1)
         IF(IERX.EQ.1)   GO TO 100
         DO NL=1,NLS
            CALL RDCONT
!***********PROCESS EACH J VALUE
            NJS = N1H
            CALL TEST1(NJS,1,NJSMAX,'NJS',1)
            IF(IERX.EQ.1)    GO TO 100
            DO NJ=1,NJS
               CALL RDLIST
               NE = N2L
               CALL TEST1(NE,1,URNEMAX,'NE',1)
               NET = (NPL-6)/NREP6
               CALL TEST2(NE,NET,'NE')
            END DO
         END DO
      ELSE
!
!     ONLY FISSION WIDTH IS ENERGY DEPENDENT
!
         IF(LFW.EQ.0)   THEN
!********NO FISSION WIDTH
            CALL RDCONT
!***********TEST NUMBER OF L VALUES
            NLS = N1H
            CALL TEST1(NLS,1,NLSMAX,'NLS',1)
            IF(IERX.EQ.1) GO TO 100
!***********PROCESS EACH L STATE
            DO NL=1,NLS
               CALL RDLIST
               NJS = N2L
               CALL TEST1(NJS,1,NJSMAX,'NJS',1)
               NUM = NPL/NREP6
               CALL TEST2(NJS,NUM,'NJS')
               IF(IERX.EQ.1)   GO TO 100
            END DO
         ELSE
!
!     FISSION WIDTH ENERGY DEPENDENT.   OTHERS NOT
!
            CALL RDLIST
!***********CHECK NUMBER OF L VALUES
            NLS = N2L
            CALL TEST1(NLS,1,NLSMAX,'NLS',1)
!***********CHECK NUMBER OF ENERGY POINTS
            CALL TEST1(NPL,1,URNEMAX,'NE',1)
            IF(IERX.EQ.1) GO TO 100
!***********CHECK EACH L STATE
            DO NL=1,NLS
               CALL RDCONT
               NJS = N1H
               CALL TEST1(NJS,1,NJSMAX,'NJS',1)
               IF(IERX.EQ.1) GO TO 100
!**************CHECK EACH J STATE
               DO NJ=1,NJS
                  CALL RDLIST
                  IF(IERX.EQ.1) GO TO 100
               END DO
            END DO
         END IF
      END IF
!
  100 RETURN
      END SUBROUTINE UR_CHECK
!
!***********************************************************************
!
      SUBROUTINE NORCHECK(NER,LFW)
!
!     ROUTINE TO CHECK THE WHEN ONLY POTENTIAL SCATTERING GIVEN
!
      IMPLICIT NONE
!
      INTEGER(KIND=I4) :: LFW,LRF
!
      INTEGER(KIND=I4) :: NER
      INTEGER(KIND=I4) :: NLS
!
!     NO RESONANCE PARAMETERS GIVEN, ONLY A SCATTERING LENGTH
!
      IF(NFOR.GE.6)   THEN
         CALL TEST1(N1H,0,0,'NRO',2)
         CALL TEST1(N2H,0,0,'NAPS',2)
         IF(IERX.EQ.1)   GO TO 100
      END IF
      LRF = L2H
      CALL TEST2(LRF,0,'LRF')
      CALL TEST2(NER,1,'NER')
      CALL TEST2(LFW,0,'LFW')
      CALL RDCONT
      NLS = N1H
      CALL TEST2(NLS,0,'NLS')
!
  100 RETURN
      END SUBROUTINE NORCHECK
!
!***********************************************************************
!
      SUBROUTINE CKMTRE(MTRE,NIRE,NCRE)
!
!     ROUTINE TO CHECK FOR VALID REACTION MT'S IN HYBRID R-FUNCTION
!
      IMPLICIT NONE
!
      INTEGER(KIND=I4) :: NIRE,NCRE
      INTEGER(KIND=I4), DIMENSION(*) :: MTRE
!
      INTEGER(KIND=I4) :: NMTRE
      INTEGER(KIND=I4) :: NFND
      INTEGER(KIND=I4) :: ICHK
      INTEGER(KIND=I4) :: IGRP
      INTEGER(KIND=I4) :: IOPRE,IOCUR
      INTEGER(KIND=I4) :: IMTERR
      INTEGER(KIND=I4) :: I,J
!
      INTEGER(KIND=I4), DIMENSION(4) :: MTERR
      INTEGER(KIND=I4), DIMENSION(2) :: MTSUM
!
      INTEGER(KIND=I4), PARAMETER :: NMTS = 9
      INTEGER(KIND=I4), DIMENSION (NMTS)::  MTSFND
      INTEGER(KIND=I4), DIMENSION (NMTS), PARAMETER::                    &      
     &              VALMTS = (/51,52,53,54,103,104,105,106,107/)
      INTEGER(KIND=I4), DIMENSION (NMTS), PARAMETER::                    &      
     &              GRPMTS = (/1,1,1,1,2,2,2,2,2/)
!
!     INITIALIZE
!
      NFND = 0
      MTERR = -1
      MTSUM = 0
!
!     SEE HOW MANY COMPETITIVE REACTIONS GIVEN
!
      NMTRE = 0
      DO I=1,4
         IF(MTRE(I).EQ.0)  GO TO 10
         NMTRE = NMTRE + 1
      END DO
   10 IF(NMTRE.EQ.0)   GO TO 100
!
!     CHECK ALL POSSIBLE VALUES TO SEE IF ONE INCLUDED
!
      DO I=1,NMTS
         ICHK = VALMTS(I)
         MTSFND(I) = 0
!********DONE IF ALL INPUT VALUES FOUND AND VALID
         IF(NFND.GT.NMTRE)   GO TO 40
!
!     CHECK ALL INPUT TO SEE IF ONE EQUALS CURRENT VALID VALUE
!
         DO J=1,NMTRE
            IF(MTRE(J).EQ.ICHK)  THEN
!**************VALID VALUE
               NFND = NFND + 1
               MTERR(J) = 0
               IF(MTSFND(I).EQ.0) THEN
                  MTSFND(I) = J
               ELSE
!**************DUPLICATE
                  MTERR(J) = 1
               END IF
!
!     ADD TO COUNT OF PROPER REACTION GROUP
!
               IGRP = GRPMTS(I)
               MTSUM(IGRP) = MTSUM(IGRP) + 1
            END IF
         END DO
      END DO
!
!     CHECK ORDER OF REACTIONS SPECIFIED
!
   40 IOPRE = 0
      DO I=1,NMTS
         IOCUR = MTSFND(I)
         IF(IOCUR.NE.0) THEN
            IF(IOCUR.GE.IOPRE)  THEN
               IOPRE = IOCUR
            ELSE
               MTERR(IOCUR) = MTERR(IOCUR) + 2
            END IF
         END IF
      END DO
!
!     OUTPUT ERRORS FOR REACTION MTS
!
      DO I=1,NMTRE
         IMTERR = MTERR(I)
         IF(IMTERR.LT.0)  THEN
            WRITE(EMESS,'(A,I4,A,I2,A)')                                &       
     &         'REACTION MT',MTRE(I),' FOR MTRE',I,' IS INVALID'
            CALL ERROR_MESSAGE(NSEQP)
         ELSE IF(IMTERR.GT.0) THEN
            IF(IMTERR.EQ.1.OR.IMTERR.EQ.3)   THEN
               WRITE(EMESS,'(A,I4,A,I2,A)')                             &       
     &            'REACTION MT',MTRE(I),' FOR MTRE',I,' IS A DUPLICATE'
               CALL ERROR_MESSAGE(NSEQP)
            END IF
            IF(IMTERR.EQ.2.OR.IMTERR.EQ.3)   THEN
               WRITE(EMESS,'(I4,A,I2,A)')                               &       
     &            'REACTION MT',MTRE(I),' FOR MTRE',I,' IS OUT OF ORDER'
               CALL ERROR_MESSAGE(NSEQP)
            END IF
         END IF
      END DO
!
!     OUTPUT ERRORS IN REACTION COUNT
!
  100 IF(NIRE.NE.MTSUM(1)) THEN
         EMESS = 'NIRE DOES NOT EQUAL THE NUMBER OF COMPETITIVE '//     &       
     &         'INELASTIC REACTIONS'
         CALL ERROR_MESSAGE(0)
      END IF
      IF(NCRE.NE.MTSUM(2)) THEN
         EMESS = 'NCRE DOES NOT EQUAL THE NUMBER OF COMPETITIVE '//     &       
     &         'CHARGED PARTICLE REACTIONS'
         CALL ERROR_MESSAGE(0)
      END IF
!
      RETURN
      END SUBROUTINE CKMTRE
!
!***********************************************************************
!
      SUBROUTINE CKF3
!
!     ROUTINE TO CHECK FILE 3 DATA
!
      IMPLICIT NONE
!
!     TEST FOR A VALID MT NUMBER
!
      CALL TESTFT(MTO,MFO)
!
!     SEE IF SECTION IS IN THE DIRECTORY
!
      CALL TESTD(MFO,MTO)
      IF(IERX.EQ.1)   GO TO 100
!
!     READ THE DATA
!
      CALL RDTAB1
!
  100 RETURN
      END SUBROUTINE CKF3
!
!***********************************************************************
!
      SUBROUTINE CKF4
!
!     ROUTINE TO CHECK FILE 4 DATA
!
      IMPLICIT NONE
!
      INTEGER(KIND=I4) :: LVT,LTT,LCT
      INTEGER(KIND=I4) :: NM,NPLT
      INTEGER(KIND=I4) :: LI
      INTEGER(KIND=I4) :: NE
      INTEGER(KIND=I4) :: N
      REAL(KIND=R4) :: EM1
!
!     TEST FOR A VALID MT NUMBER
!
      CALL TESTFT(MTO,MFO)
!
!     SEE IF SECTION IS IN THE DIRECTORY
!
      CALL TESTD(MFO,MTO)
      IF(IERX.EQ.1)   GO TO 100
!
!     THE EQUIVALENT SECTION MUST EXIST IN FILE 3
!
      CALL TESTP(3,MTO)
!*****TEST TRANSFORMATION MATRIX FLAG
      LVT = L1H
      CALL TEST1(LVT,0,1,'LVT',2)
      IF(LVT.EQ.1) THEN
         EMESS = ' THE ELASTIC TRANSFORMATION MATRIX IS NO'
         CALL ERROR_MESSAGE(0)
         EMESS = '       LONGER SUPPORTED'
         CALL ERROR_MESSAGE(NSEQP)
      END IF
!*****TEST ANGULAR REPRESENTATION FLAG
      LTT = L2H
      CALL TEST1(LTT,0,3,'LTT',2)
      IF(IERX.EQ.1) GO TO 100
!
!     TEST TRANSFORMATION MATRIX IF PRESENT
!
      IF(LVT.GT.0)   THEN
         IF(NLIB.EQ.0.AND.NVER.EQ.7) THEN
            EMESS = 'ELASTIC Transformation Matrix not allowed for'//   &       
     &              ' ENDF/B-VII'
            CALL ERROR_MESSAGE(NSEQP1)
         END IF
         CALL RDLIST
!********TEST ORDER OF MATRIX
         NM = N2L
         CALL TEST1(NM,1,NLEGMAX,'NM',1)
!********CHECK NUMBER OF ELEMENTS
         NPLT = (NM+1)*(NM+1)
         CALL TEST2(NPL,NPLT,'NK')
!********CHECK LAB-CM FLAG
         LCT = L2L
         CALL TEST1(LCT,1,2,'LCT',1)
         LI = L1L
         IF(IERX.EQ.1) GO TO 100
      ELSE
!
!     NO TRANSFORMATION MATRIX
!
         CALL RDCONT
!********CHECK LAB-CM FLAG
         LCT = L2H
         CALL TEST1(LCT,1,2,'LCT',1)
         LI = L1H
         IF(IERX.EQ.1)   GO TO 100
      END IF
!
!     CHECK ISOTROPY FLAG
!
      CALL TEST1(LI,0,1,'LI',2)
      IF(IERX.EQ.1)   GO TO 100
!
!     ALL DISTRIBUTIONS ISOTROPIC
!
      IF (LI.EQ.1)  THEN
!********LTT MUST BE ZERO
         CALL TEST2(LTT,0,'LTT')
         GO TO 100
      END IF
!
!     ALL DISTRIBUTIONS NOT ISOTROPIC
!
      CALL TEST1(LTT,1,3,'LTT',2)
      IF(IERX.EQ.1)  GO TO 100
      CALL RDTAB2(0)
!*****CHECK FOR NUMBER OF INCIDENT ENERGIES
      NE = NP2
      CALL TEST1(NE,1,NES4MAX,'NE',1)
      IF(IERX.EQ.1)   GO TO 100
!
!     LEGENDRE COEFFICIENTS
!
      IF(LTT.NE.2)   THEN
         DO N=1,NE
            CALL RDLIST
!***********TEST NUMBER OF COEFFICIENTS
            CALL TEST1(NPL,1,NLEGMAX,'NL',1)
            IF(IERX.EQ.1) GO TO 100
         END DO
         IF(LTT.EQ.1)  GO TO 100
!
!     MULTIPLE REPRESENTATION
!
         EM1 = C2L
         CALL RDTAB2(0)
!********CHECK FOR NUMBER OF INCIDENT ENERGIES
         NE = NP2
         CALL TEST1(NE,1,NES4MAX,'NE',1)
         IF(IERX.EQ.1)   GO TO 100
      END IF
!
!     TABULAR
!
      DO N=1,NE
         CALL RDTAB1
!********CHECK NUMBER OF ANGLES
         CALL TEST1(NP,2,NANGMAX,'NP',1)
         IF(LTT.EQ.3.AND.N.EQ.1) THEN
            IF(C2.NE.EM1) THEN
               WRITE(EMESS,'(A,1PE11.4,A)')                             &       
     &            ' THE FIRST TABULAR ENERGY MUST BE AT ',EM1,'EV,'
               CALL ERROR_MESSAGE(NSEQP)
            END IF
         END IF
         IF(IERX.EQ.1) GO TO 100
      END DO
!
  100 RETURN
      END SUBROUTINE CKF4
!
!***********************************************************************
!
      SUBROUTINE CKF5
!
!     ROUTINE TO CHECK FILE 5 DATA
!
      IMPLICIT NONE
!
      INTEGER(KIND=I4) :: MFPR
      INTEGER(KIND=I4) :: NK
      INTEGER(KIND=I4) :: LF
      INTEGER(KIND=I4) :: NE
      INTEGER(KIND=I4) :: N,NM
!
      INTEGER(KIND=I4), PARAMETER :: LFMAX=12
!
!     TEST FOR A VALID MT NUMBER
!
      CALL TESTFT(MTO,MFO)
!
!     SEE IF SECTION IS IN THE DIRECTORY
!
      CALL TESTD(MFO,MTO)
      IF(IERX.EQ.1)   GO TO 100
!
!     THE EQUIVALENT SECTION MUST EXIST IN FILE 3 (FILE 1 IF MT=455)
!
      IF(MT.EQ.455)   THEN
         MFPR = 1
      ELSE
         MFPR = 3
      END IF
      IF(NSUB.NE.4.OR.MT.NE.18)  CALL TESTP(MFPR,MTO)
!
!     CHECK NUMBER OF SUBSECTIONS
!
      NK = N1H
      CALL TEST1(NK,1,NSUBSMAX,'NK',1)
!
!     PROCESS EACH SUBSECTION
!
      DO N=1,NK
         CALL RDTAB1
!********TEST NUMBER OF POINTS IN PROBABILITY TABLE
         CALL TEST1(NP,1,NES5MAX,'NP',1)
!********TEST FOR VALID DISTRIBUTION LAW
         LF = L2
         CALL TEST1(LF,1,LFMAX,'LF',2)
         IF(IERX.EQ.1) GO TO 100
         SELECT CASE (LF)
!
!        TABULATED DISTRIBUTION   LF=1
!
            CASE (1)
               CALL RDTAB2(0)
               NE = NP2
               CALL TEST1(NE,1,NES5MAX,'NE',1)
               IF(IERX.EQ.1)   GO TO 100
               DO NM=1,NE
                  CALL RDTAB1
                  CALL TEST1(NP,2,NEDISMAX,'NF',1)
                  IF(IERX.EQ.1)   GO TO 100
               END DO
!
!           GENERAL EVAPORATION SPECTRUM  LF=5
!
            CASE (5)
               CALL RDTAB1
               CALL TEST1(NP,1,NES5MAX,'NE',1)
               IF(IERX.EQ.1)   GO TO 100
               CALL RDTAB1
               CALL TEST1(NP,2,NEDISMAX,'NF',1)
               IF(IERX.EQ.1)   GO TO 100
!
!           MAXWELLIAN, EVAPORATION OR MADLAND-NIX SPECTRUM LF=7,9,12
!
            CASE (7,9,12)
               CALL RDTAB1
               CALL TEST1(NP,1,NES5MAX,'NE',1)
               IF(IERX.EQ.1) GO TO 100
!
!           WATT SPECTRUM    LF=11
!
            CASE (11)
               DO NM=1,2
                  CALL RDTAB1
                  CALL TEST1(NP,1,NES5MAX,'NE',1)
                  IF(IERX.EQ.1)   GO TO 100
               END DO
!
!           INVALID DISTRIBUTION LAW
!
            CASE DEFAULT
               WRITE(EMESS,'(A,I3,A)')  'LF=',LF,' IS NOT ALLOWED'
               CALL ERROR_MESSAGE(NSEQP1)
               IERX = 1
               GO TO 100
         END SELECT
      END DO
!
  100 RETURN
      END SUBROUTINE CKF5
!
!***********************************************************************
!
      SUBROUTINE CKF6
!
!     ROUTINE TO CHECK FILE 6 DATA
!
      IMPLICIT NONE
!
      INTEGER(KIND=I4), INTRINSIC :: MOD
!
      INTEGER(KIND=I4) :: MFM1,MFM2,ISET
      INTEGER(KIND=I4) :: LCT
      INTEGER(KIND=I4) :: NK
      INTEGER(KIND=I4) :: LAW
      INTEGER(KIND=I4) :: LANG,LEP,NEP,NEPM
      INTEGER(KIND=I4) :: NA,ND
      INTEGER(KIND=I4) :: NE
      INTEGER(KIND=I4) :: LTP,LIDP,NL
      INTEGER(KIND=I4) :: NW,NWT
      INTEGER(KIND=I4) :: NPSX
      INTEGER(KIND=I4) :: NMU
      INTEGER(KIND=I4) :: I,N,NU
!
      INTEGER(KIND=I4), PARAMETER :: LAWMAX=7
      INTEGER(KIND=I4), PARAMETER :: LANGMAX=15,LEPMAX=5
      INTEGER(KIND=I4), PARAMETER :: NPSXMAX=20
!
!     TEST FOR A VALID MT NUMBER
!
      CALL TESTFT(MTO,MFO)
!
!     SEE IF SECTION IS IN THE DIRECTORY
!
      CALL TESTD(MFO,MTO)
      IF(IERX.EQ.1)   GO TO 100
!
!     THE EQUIVALENT SECTION MUST EXIST IN FILE 3
!
      CALL TESTP(3,MTO)
!
!     SECTION CANNOT EXIST IN EITHER FILE 4 OR FILE 5
!
      MFM2 = MF - 2
      CALL TESTS(MFM2,MT,ISET)
      IF(NXC0.NE.0.AND.ISET.NE.1.AND.ISET.NE.3)    THEN
         WRITE(EMESS,'(A,I3,A,I3,A)')                                   &       
     &       'THIS SECTION REQUIRES THAT SECTION', MFM2,'/',MT,         &       
     &       ' NOT BE PRESENT'
         CALL ERROR_MESSAGE(0)
      END IF
      MFM1 = MF - 1
      CALL TESTS(MFM1,MT,ISET)
      IF(NXC0.NE.0.AND.ISET.NE.1.AND.ISET.NE.3)    THEN
         WRITE(EMESS,'(A,I3,A,I3,A)')                                   &       
     &       'THIS SECTION REQUIRES THAT SECTION', MFM1,'/',MT,         &       
     &       ' NOT BE PRESENT'
         CALL ERROR_MESSAGE(0)
      END IF
!
!     CHECK LAB-CM FLAG
!
      LCT = L2H
      IF(LCT.NE.3) CALL TEST1(LCT,1,2,'LCT',1)
!
!     CHECK NUMBER OF SUBSECTIONS
!
      NK = N1H
      CALL TEST1(NK,1,NSUBS6MAX,'NK',1)
!
!     LOOP OVER SUBSECTIONS
!
      DO I=1,NK
         CALL RDTAB1
         LAW = L2
         CALL TEST1(LAW,0,LAWMAX,'LAW',2)
         IF(LCT.EQ.3.AND.LAW.NE.1) THEN
            EMESS = 'LCT CAN BE 3 ONLY IF LAW = 1'
            CALL ERROR_MESSAGE(NSEQP1)
         END IF
         IF(IERX.EQ.1)   GO TO 100
!
         SELECT CASE (LAW)
!
!        UNKNOWN LAW
!
            CASE (0)
!
!        TABULAR LAW
!
            CASE (1)
               CALL RDTAB2(2)
!**************TEST ANGULAR REPRESENTATION
               LANG = L12
               CALL TEST1(LANG,1,LANGMAX,'LANG',2)
               IF(LANG.GT.2.AND.LANG.LT.11) THEN
                  WRITE(EMESS,'(A,I2)')                                 &       
     &                 'INVALID ANGULAR REPRESENTATION LANG=',LANG
                  CALL ERROR_MESSAGE(NSEQP1)
               END IF
!**************TEST SECONDARY ENERGY INTERPOLATION
               LEP = L22
               CALL TEST1(LEP,1,LEPMAX,'LEP',1)
!**************TEST NUMBER OF INCIDENT ENERGIES
               NE = NP2
               CALL TEST1(NE,1,NES6MAX,'NE',1)
               IF(IERX.EQ.1)   GO TO 100
!**************PROCESS EACH INCIDENT ENERGY
               DO N=1,NE
                  CALL RDLIST
!*****************CHECK ANGULAR PARAMETER
                  ND = L1L
                  NA = L2L
                  NW = NPL
                  NEP = N2L
                  IF(LANG.EQ.1)   THEN
                     CALL TEST1(NA,0,NLEGMAX,'NA',1)
                  ELSE IF(LANG.EQ.2)   THEN
                     CALL TEST1(NA,1,2,'NA',1)
                  ELSE
                     CALL TEST1(NA,0,2*NANGMAX+1,'NA',1)
                     IF(MOD(NA,2).NE.0)   THEN
                        EMESS = 'NA MUST BE EVEN'
                        CALL ERROR_MESSAGE(NSEQP1)
                     END IF
                  END IF
                  NWT = NEP*(NA+2)
                  NEPM = NPTSMAX/(NA+2)
!********************CHECK NUMBER OF SECONDARY ENERGIES
                  CALL TEST1(NEP,1,NEPM,'NEP',1)
!*********************CHECK NUMBER OF DISCRETE POINTS
                  CALL TEST1(ND,0,NEP,'ND',1)
!*********************CHECK NUMBER OF CONTINUUM POINTS
                  CALL TEST1(NEP-ND,0,NEDISMAX,'NEP-ND',1)
                  NWT = NEP*(NA+2)
                  CALL TEST2(NW,NWT,'NW')
                  IF(IERX.EQ.1)   GO TO 100
               END DO
!
!           DISCRETE TWO-BODY LAW
!
            CASE (2)
!
               CALL RDTAB2(0)
!**************TEST NUMBER OF INCIDENT ENERGIES
               NE = NP2
               CALL TEST1(NE,1,NES5MAX,'NE',1)
               IF(IERX.EQ.1)  GO TO 100
!**************PROCESS EACH INCIDENT ENERGY
               DO N=1,NE
                  CALL RDLIST
                  LTP = L1L
                  NL = N2L
!*****************LEGENDRE REPRESENTATION
                  IF(LTP.EQ.0)    THEN
                     CALL TEST1(NL,1,NLEGMAX,'NL',1)
                     NWT = NL
!*****************TABULAR REPRESENTATION
                  ELSE
                     CALL TEST1(LTP,12,15,'LTP',2)
                     IF(LTP.EQ.13) THEN
                        WRITE(EMESS,'(A,I3,A)')                         &       
     &                     'LTP =',LTP,' NOT PERMITTED FOR LAW=5'
                        CALL ERROR_MESSAGE(NSEQP1)
                     END IF
                     IF(IERX.EQ.1)   GO TO 100
                     CALL TEST1(NL,2,NANGMAX,'NL',1)
                     NWT = 2*NL
                  END IF
                  CALL TEST2(NPL,NWT,'NW')
                  IF(IERX.EQ.1)   GO TO 100
               END DO
!
!           ISOTROPIC DISCRETE EMISSION AND DISCRETE TWO-BODY RECOILS
!
            CASE (3:4)
!
!           COULOMB ELASTIC LAW
!
            CASE (5)
               CALL RDTAB2(0)
!**************TEST IDENTICAL PARTICLE FLAG
               LIDP = L12
               CALL TEST1(LIDP,0,1,'LIDP',1)
!**************TEST NUMBER OF INCIDENT ENERGIES
               NE = NP2
               CALL TEST1(NE,1,NES5MAX,'NE',1)
               IF(IERX.EQ.1)    GO TO 100
               DO N=1,NE
                  CALL RDLIST
                  NL = N2L
                  NW = NPL
                  LTP = L1L
!*****************LEGENDRE EXPANSIONS
                  IF(LTP.EQ.1)   THEN
                     CALL TEST1(NL,0,NLEGMAX,'NL',1)
                     IF(LIDP.EQ.0)  THEN
                        NWT = 4*NL + 3
                     ELSE IF(LIDP.EQ.1)  THEN
                        NWT = 3*NL + 3
                     END IF
                  ELSE IF(LTP.EQ.2)   THEN
                     CALL TEST1(NL,0,NLEGMAX,'NL',1)
                     NWT = NL + 1
!*****************ANGULAR TABULATIONS
                  ELSE IF(LTP.EQ.12.OR.LTP.EQ.14)   THEN
                     CALL TEST1(NL,1,NANGMAX,'NL',1)
                     NWT = 2*NL
                  ELSE
                     WRITE(EMESS,'(A,I2)')                              &       
     &                      'INVALID ANGULAR REPRESENTATION LTP=',LTP
                     CALL ERROR_MESSAGE(NSEQP1)
                     IERX = 1
                  END IF
!*****************CHECK LENGTH OF LIST
                  CALL TEST2(NW,NWT,'NW')
                  IF(IERX.EQ.1)   GO TO 100
               END DO
!
!           N-BODY PHASE SPACE
!
            CASE (6)
               CALL RDCONT
!**************REASONABLE NUMBER OF PARTICLES COUNTED
               NPSX = NP
               CALL TEST1(NPSX,1,NPSXMAX,'NPSX',1)
!
!           LABORATORY ANGLE-ENERGY LAW
!
            CASE(7)
               CALL RDTAB2(0)
               NE = NP2
               CALL TEST1(NE,1,NES5MAX,'NE',1)
               IF(IERX.EQ.1)      GO TO 100
               DO N=1,NE
                  CALL RDTAB2(0)
                  NMU = NP2
                  CALL TEST1(NMU,2,NANGMAX,'NMU',1)
                  IF(IERX.EQ.1)      GO TO 100
                  DO NU=1,NMU
                     CALL RDTAB1
                     NEP = NP
                     CALL TEST1(NEP,2,NEDISMAX,'NEP',1)
                     IF(IERX.EQ.1)  GO TO 100
                  END DO
               END DO
!
!           INVALID DISTRIBUTION LAW
!
            CASE DEFAULT
               WRITE(EMESS,'(A,I3,A)')  'LAW=',LAW,' IS NOT ALLOWED'
               CALL ERROR_MESSAGE(NSEQP1)
               IERX = 1
               GO TO 100
         END SELECT
      END DO
!
  100 RETURN
      END SUBROUTINE CKF6
!
!***********************************************************************
!
      SUBROUTINE CKF7
!
!     ROUTINE TO CHECK FILE 7 DATA
!
      IMPLICIT NONE
!
      INTEGER(KIND=I4) :: LTHR
      INTEGER(KIND=I4) :: LAT,LASYM,LLN,NS
      INTEGER(KIND=I4) :: NUM
      INTEGER(KIND=I4) :: NB
      INTEGER(KIND=I4) :: LT
      INTEGER(KIND=I4) :: K,N,NN,NNN
!
      REAL(KIND=R4) :: T,VAR
      REAL(KIND=R4), DIMENSION(3) :: BFLAG
!
!     TEST FOR A VALID MT NUMBER
!
      CALL TESTFT(MTO,MFO)
!
!     SEE IF SECTION IS IN THE DIRECTORY
!
      CALL TESTD(MFO,MTO)
      IF(IERX.EQ.1)   GO TO 100
!
!     CHECK FOR VALID VALUE OF LTHR
!
      LTHR = L1H
      IF(NFOR.GE.6.AND.MT.EQ.2)  THEN
         CALL TEST1(LTHR,1,2,'LTHR',2)
         IF(IERX.EQ.1)   GO TO 100
      ELSE
         CALL TEST2(LTHR,0,'LTHR')
      END IF
!
!     INCOHERENT INELASTIC SCATTERING
!
      IF(LTHR.EQ.0)   THEN
         IF(MT.NE.4)  THEN
            EMESS = 'INELASTIC SCATTERING SHOULD BE IN MT=4'
            CALL ERROR_MESSAGE(0)
         END IF
         LAT = L2H
         CALL TEST1(LAT,0,1,'LAT',1)
         LASYM = N1H
         CALL TEST1(LASYM,0,1,'LASY',1)
         CALL RDLIST
         LLN = L1L
         CALL TEST1(LLN,0,1,'LLN',1)
         NS = N2L
         CALL TEST1(NS,0,NPSAMAX,'NS',1)
         NUM = (NPL-6)/NREP6
         CALL TEST2(NS,NUM,'NS')
         IF(IERX.EQ.1)   GO TO 100
         DO NNN=1,NS
            BFLAG(NNN) = BIN(NNN+1)
         END DO
!
!        PROCESS ALL BETA VALUES
!
         IF(BIN(1).GT.0.)  THEN
            CALL RDTAB2(0)
            IF(IERX.EQ.1) GO TO 100
            NB = NP2
            DO N=1,NB
               CALL RDTAB1
               LT = L1
               CALL TEST1(LT,0,NSMTMAX,'LT',1)
               IF(IERX.EQ.1) GO TO 100
               IF(LT.GT.0)   THEN
                  T = C1
                  VAR = C2
                  DO K=1,LT
                     CALL RDLIST
                     IF(C1L.LE.T)   THEN
                        EMESS = 'TEMPERATURES NOT IN INCREASING ORDER'
                        CALL ERROR_MESSAGE(0)
                     ELSE
                        T = C1L
                     END IF
                     CALL TEST2F(C2L,VAR,'C2')
                     CALL TEST2(NPL,NP,'NPL')
                     CALL TEST1(L1L,1,INTMAX,'INT',1)
                  END DO
               END IF
            END DO
         END IF
!
!        PROCESS EFFECTIVE TEMPERATURE RECORD
!
         IF(NFOR.GE.6)    THEN
            CALL RDTAB1
            IF(NS.LE.0)   GO TO 100
            DO NN=1,NS
               IF(BFLAG(NN).EQ.0.)  CALL RDTAB1
            END DO
         END IF
!
!     ELASTIC SCATTERING
!
      ELSE
         IF(MT.NE.2)   THEN
            EMESS = 'ELASTIC SCATTERING SHOULD BE IN MT=2'
            CALL ERROR_MESSAGE(0)
         END IF
!
!        COHERENT ELASTIC SCATTERING
!
         IF(LTHR.EQ.1)  THEN
            CALL RDTAB1
            LT = L1
            T = C1
            IF(LT.NE.0) THEN
               T = C1
               VAR = C2
               DO N=1,LT
                  CALL RDLIST
                  IF(C1L.LE.T)  THEN
                     EMESS = 'TEMPERATURES NOT IN INCREASING ORDER'
                     CALL ERROR_MESSAGE(NSEQP1)
                  ELSE
                     T = C1L
                  END IF
                  CALL TEST2F(C2L,VAR,'C2')
                  CALL TEST2(NPL,NP,'NPL')
                  CALL TEST1(L1L,1,INTMAX,'INT',1)
               END DO
             END IF
!
!        INCOHERENT ELASTIC SCATTERING
!
         ELSE
            CALL RDTAB1
         END IF
      END IF
!
  100 RETURN
      END SUBROUTINE CKF7
!
!***********************************************************************
!
      SUBROUTINE CKF8
!
!     ROUTINE TO CHECK FILE 8 DATA
!
      IMPLICIT NONE
!
      INTEGER(KIND=I4) :: LEP1
      INTEGER(KIND=I4) :: NFP,NUM
      INTEGER(KIND=I4) :: NS,NO
      INTEGER(KIND=I4) :: LFSO,LFSP
      INTEGER(KIND=I4) :: MATPR
      INTEGER(KIND=I4) :: LMF,NND
      INTEGER(KIND=I4) :: K,N
      REAL(KIND=R4) :: ZAP,ELFS
!
!     TEST FOR A VALID MT NUMBER
!
      CALL TESTFT(MTO,MFO)
!
!     SEE IF SECTION IS IN THE DIRECTORY
!
      CALL TESTD(MFO,MTO)
      IF(IERX.EQ.1)   GO TO 100
!
!     FISSION PRODUCT YIELDS
!
      IF(MT.EQ.454.OR.MT.EQ.459)   THEN
         LEP1 = L1H
         DO K=1,LEP1
            CALL RDLIST
            NFP = N2L
            CALL TEST1(NFP,1,NFPMAX,'NFP',1)
            NUM = NPL/4
            CALL TEST2(NFP,NUM,'NFP')
            IF(K.EQ.1)  THEN
               CALL TEST2(LEP1-1,L1L,'LE')
            ELSE
               CALL TEST1(L1L,1,INTMAX,'INT',1)
            END IF
            IF(IERX.EQ.1)   GO TO 100
         END DO
!
!     RADIOACTIVE DECAY DATA
!
      ELSE IF(MT.EQ.457)   THEN
         CALL CHK_RAD
      ELSE
!
!     REACTION PRODUCT DESCRIPTIONS
!
         CALL TEST2(L1H,LIS,'LIS')
         CALL TEST2(L2H,LISO,'LISO')
         NS = N1H
         NO = N2H
         CALL TEST1(NS,1,NDSTMAX,'NS',1)
         LFSP = -1
         DO N=1,NS
            IF(NO.EQ.1)  THEN
               CALL RDCONT
               ZAP = C1H
               ELFS = C2H
               LMF = L1H
               LFSO = L2H
               NND = N1H
               MATPR = N2H
               CALL TEST2(NND,0,'6*ND')
            ELSE
               CALL RDLIST
               ZAP = C1L
               ELFS = C2L
               LFSO = L2L
               LMF = L1L
               NND = NPL
               MATPR = N2L
            END IF
!***********CHECK INCREASING ORDER OF FINAL STATES
            IF (LFSO.LE.LFSP)   THEN
               EMESS = 'DATA NOT GIVEN IN ORDER OF INCREASING LFSO'
               CALL ERROR_MESSAGE(NSEQP1)
            END IF
            LFSP = LFSO
!***********CHECK THAT ZAP IS NOT ZERO
            IF(ZAP.EQ.0.0) THEN
               EMESS = 'ZAP CANNOT BE 0.0'
               CALL ERROR_MESSAGE(NSEQP1)
            ENDIF
!***********CHECK THAT MATP IS ZERO, USE IS OBSOLETE
            IF(MATPR.NE.0) THEN
               EMESS = 'MATP IS OBSOLETE, SHOULD BE SET TO 0'
               CALL ERROR_MESSAGE(NSEQP1)
            ENDIF
!***********CHECK FOR VALID LMF
            IF(LMF.EQ.3) THEN
               CALL TESTP(3,MTO)
            ELSE IF(LMF.EQ.6) THEN
               CALL TESTP(6,MTO)
               CALL TESTP(3,MTO)
            ELSE IF(LMF.NE.9.AND.LMF.NE.10) THEN
               WRITE(EMESS,'(A,I5,A)')'LMF =',LMF,' NOT ALLOWED'
               CALL ERROR_MESSAGE(NSEQP1)
               GO TO 100
            END IF
         END DO
      END IF
!
  100 RETURN
      END SUBROUTINE CKF8
!
!***********************************************************************
!
      SUBROUTINE CHK_RAD
!
!     SUBROUTINE TO CHECK RADIOACTIVE DECAY DATA
!
      IMPLICIT NONE
!
      REAL(KIND=R4), INTRINSIC :: FLOAT
!
      INTEGER(KIND=I4) :: NSP,NST
      INTEGER(KIND=I4) :: NDKT
      INTEGER(KIND=I4) :: NUM
      INTEGER(KIND=I4) :: ISTP,IS
      INTEGER(KIND=I4) :: LCON,LCOV
      INTEGER(KIND=I4) :: NER
      INTEGER(KIND=I4) :: LB,NPP
      INTEGER(KIND=I4) :: N,NN
      REAL(KIND=R4) :: STYPE
!
!     CHECK FOR CORRECT INITIAL STATE FLAGS
!
      CALL TEST2(L1H,LIS,'LIS')
      CALL TEST2(L2H,LISO,'LISO')
!
      NSP = N2H
      NST = N1H
      CALL TEST1(NST,0,1,'NST',1)
!
!     PROCESS AVERAGE DECAY ENERGIES
!
      CALL RDLIST
      IF(NST.EQ.0) THEN
         IF(NPL.NE.34) THEN
            CALL TEST2(NPL,6,'NI')
            IF (IERX.EQ.1)  GO TO 100
         END IF
      ELSE
         CALL TEST2(NPL,6,'NI')
      END IF
!
!     PROCESS DECAY MODES
!
      CALL RDLIST
      NDKT = N2L
      IF(NDKT.LE.0.AND.STA.EQ.1.0) THEN
         EMESS = 'NO DECAY MODES GIVEN'
         CALL ERROR_MESSAGE(NSEQP)
         IERX = 1
         GO TO 100
      END IF
      IF(NST.EQ.0) THEN
         NUM = NPL/NREP6
         CALL TEST2(NDKT,NUM,'NDK')
      ELSE
         CALL TEST2(NDKT,0,'NDK')
      END IF
      IF(IERX.EQ.1)   GO TO 100
!
!     PROCESS DECAY SPECTRA
!
      ISTP = -1
      IF(NSP.EQ.0) GO TO 100
      DO N=1,NSP
         CALL RDLIST
         CALL TEST2(NPL,6,'NI')
         STYPE = C2L
         IS = IFIX(STYPE) + 1
!********CHECK FOR VALID STYPE
         IF((IS.LT.1.OR.IS.GT.10).OR.IS.EQ.4) THEN
            WRITE(EMESS,'(A,F4.1,A)') 'STYPE ',STYPE,' NOT VALID'
            CALL ERROR_MESSAGE(NSEQP1)
            IS = 11
!********MAKE SURE STYPES IN INCREASING ORDER
         ELSE
            IF(IS.LE.ISTP)   THEN
               WRITE(EMESS,'(A,F4.1,A)') 'STYPE ',STYPE,' OUT OF ORDER'
               CALL ERROR_MESSAGE(NSEQP1)
            END IF
            ISTP = IS
         END IF
         LCON = L1L
         CALL TEST1(LCON,0,2,'LCON',2)
         IF(IERX.EQ.1)   GO TO 100
!********CONTINUUM SPECTRA NOT ALLOWED FOR SPONTANEOUS FISSION FRAGMENTS
         IF(IS.EQ.7.AND.LCON.NE.0.)  THEN
            EMESS = 'NO DISCRETE SPECTRA ALLOWED FOR STYPE = 6.'
            CALL ERROR_MESSAGE(NSEQP1)
         END IF
         NER = N2L
         IF(LCON.NE.1) THEN
            IF(NER.EQ.0)   THEN
               IF(IS.NE.6.AND.IS.NE.7) THEN
                  WRITE(EMESS,'(A,I1,A,F4.1)')  'LCON=',LCON,           &       
     &               ' AND NER=0 NOT ALLOWED FOR STYPE = ',STYPE
                  CALL ERROR_MESSAGE(0)
                  GO TO 30
               END IF
            END IF
         ELSE
            IF(NER.NE.0)   THEN
               WRITE(EMESS,'(A,F4.1)')                                  &       
     &            'LCON=1 REQUIRES NER=0 FOR STYPE = ',STYPE
               CALL ERROR_MESSAGE(0)
            END IF
            GO TO 30
         END IF
         DO NN=1,NER
            CALL RDLIST
            IF(IERX.EQ.1)   GO TO 100
!***********CHECK LENGTH OF LIST
            IF(NPL.NE.6) THEN
               IF(IS-1.NE.0) THEN
                  WRITE(EMESS,'(A,F4.1)')                               &       
     &               'NPL MUST BE 6 FOR STYPE = ',FLOAT(IS-1)
                  CALL ERROR_MESSAGE(NSEQP1)
               ELSE
                  IF(NPL.NE.8.AND.NPL.NE.12) THEN
                     EMESS = 'NPL MUST BE 8 OR 12 FOR STYPE = 0.0'
                     CALL ERROR_MESSAGE(NSEQP1)
                  END IF
               END IF
            END IF
         END DO
   30    IF(LCON.GT.0)   THEN
            CALL RDTAB1
            LCOV = L2
            CALL TEST1(LCOV,0,1,'LCOV',1)
            IF(LCOV.GT.0)   THEN
               CALL RDLIST
               LB = L2L
               CALL TEST1(LB,2,2,'LB',1)
               NPP = N2L
               NUM = NPL/2
               CALL TEST2(NPP,NUM,'NPP')
            END IF
         END IF
      END DO
!
  100 RETURN
      END SUBROUTINE CHK_RAD
!
!***********************************************************************
!
      SUBROUTINE CKF9
!
!     ROUTINE TO CHECK FILE 9 AND FILE 10 DATA
!
      IMPLICIT NONE
!
      INTEGER(KIND=I4), INTRINSIC :: MOD
!
      INTEGER(KIND=I4) :: MFM1,ISET,INAT
      INTEGER(KIND=I4) :: NS,LFSO,LFSP,IZAP,IZAPP
      INTEGER(KIND=I4) :: N
!
!     TEST FOR A VALID MT NUMBER
!
      CALL TESTFT(MTO,MFO)
!
!     SEE IF SECTION IS IN THE DIRECTORY
!
      CALL TESTD(MFO,MTO)
      IF(IERX.EQ.1)   GO TO 100
!
!     EQUIVALENT SECTION MUST EXIST IN FILE 8
!
      CALL TESTP(8,MTO)
!
!     SECTION CANNOT EXIST IN BOTH FILE 9 AND FILE 10
!
      IF(MF.EQ.10)   THEN
         MFM1 = MF - 1
         CALL TESTS(MFM1,MT,ISET)
         IF(NXC0.NE.0.AND.ISET.NE.1.AND.ISET.NE.3)    THEN
            WRITE(EMESS,'(A,I3,A,I3,A)')                                &       
     &        'THIS SECTION REQUIRES THAT SECTION',MFM1,'/',MT,         &       
     &        ' NOT BE PRESENT'
            CALL ERROR_MESSAGE(0)
         END IF
      ELSE
!
!     MULTIPLICITIES REQUIRE EQUIVALENT SECTION MUST EXIST IN FILE 3
!
         CALL TESTP(3,MTO)
      END IF
!
!     CHECK THE STATE OF TARGET NUCLEUS IS CORRECT
!
      CALL TEST2(L1H,LISO,'LISO')
!
!     CHECK NUMBER OF EXCITED STATES
!
      NS = N1H
      CALL TEST1(NS,1,NDSTMAX,'NS',1)
      IF(IERX.EQ.1)   GO TO 100
!
!     PROCESS EACH EXCITED FINAL STATE
!
      IF(MOD(IFIX(C1H),1000).NE.0) THEN
         INAT = 0
      ELSE
         INAT = 1
      END IF
      LFSP = -1
      IZAPP = 0
      DO N=1,NS
         CALL RDTAB1
         IZAP = L1
         LFSO = L2
!
!        IZAP ZERO ONLY FOR ISOTOPIC EVALUATION
!
         IF(IZAP.EQ.0) THEN
            IF(INAT.EQ.1) THEN
               EMESS = 'IZAP CANNOT BE ZERO FOR A NATURAL ELEMENT'
               CALL ERROR_MESSAGE(NSEQP1)
            ELSE
!***********CHECK SUBSECTION ORDER FOR CASE IZAP = 0
               IF(IZAPP.NE.0) THEN
                  EMESS = 'CANNOT MIX DIFFERENT IZAP FORMALISMS'
                  CALL ERROR_MESSAGE(NSEQP1)
                  GO TO 90
               END IF
               IF (LFSO.LE.LFSP)  THEN
                  EMESS = 'DATA NOT GIVEN IN ORDER OF INCREASING LFSO'
                  CALL ERROR_MESSAGE(NSEQP1)
               ELSE
                  LFSP = LFSO
               END IF
            END IF
!
!        NEW REPRESENTATION USING A NON ZERO IZAP
!
         ELSE
            IF(IZAP.LT.IZAPP) THEN
               EMESS = 'DATA NOT GIVEN IN ORDER OF INCREASING IZAP'
               CALL ERROR_MESSAGE(NSEQP1)
            ELSE IF(IZAP.EQ.IZAPP) THEN
               IF (LFSO.LE.LFSP)  THEN
                  EMESS = 'DATA NOT GIVEN IN ORDER OF INCREASING LFSO'
                  CALL ERROR_MESSAGE(NSEQP1)
               ELSE
                  LFSP = LFSO
               END IF
            ELSE
               IZAPP = IZAP
               LFSP = -1
            END IF
         END IF
   90   IF(IERX.EQ.1)   GO TO 100
      END DO
!
  100 RETURN
      END SUBROUTINE CKF9
!
!***********************************************************************
!
      SUBROUTINE CKF12
!
!     ROUTINE TO CHECK FILE 12 AND 13 DATA
!
      IMPLICIT NONE
!
      INTEGER(KIND=I4) :: LO
      INTEGER(KIND=I4) :: MFM1,ISET
      INTEGER(KIND=I4) :: NK
      INTEGER(KIND=I4) :: LP,LF
      INTEGER(KIND=I4) :: LG,NS
      INTEGER(KIND=I4) :: NUM
      INTEGER(KIND=I4) :: J2
!
!     TEST FOR A VALID MT NUMBER
!
      CALL TESTFT(MTO,MFO)
!
!     SEE IF SECTION IS IN THE DIRECTORY
!
      CALL TESTD(MFO,MTO)
      IF(IERX.EQ.1)   GO TO 100
!
!     FILE 12 CHECKS
!
      IF(MF.EQ.12)   THEN
!********CHECK THAT FILE 3 EXISTS FOR THE MT
         CALL TESTP(3,MTO)
!********CHECK FOR A VALID LO
         LO = L1H
         CALL TEST1(LO,1,2,'LO',2)
!
!     FILE 13 CHECKS
!
      ELSE
!*******SAME SECTION CANNOT EXIST IN FILE 12
         MFM1 = MF - 1
         CALL TESTS(MFM1,MT,ISET)
         IF(NXC0.NE.0.AND.ISET.NE.1.AND.ISET.NE.3)    THEN
            WRITE(EMESS,'(A,I3,A,I3,A)')                                &       
     &         'THIS SECTION REQUIRES THAT SECTION',MFM1,'/',MT,        &       
     &         ' NOT BE PRESENT'
            CALL ERROR_MESSAGE(0)
         END IF
!********L1(LO) MUST BE 0
         LO = L1H
         CALL TEST2(L1H,0,'L1')
      END IF
!
!     LO=1 MULTIPLICITIES OR PRODUCTION CROSS SECTIONS
!
      IF(LO.LE.1)   THEN
         NK = N1H
         CALL TEST1(NK,1,NPHMAX,'NK',1)
         CALL TESTNK12(NK,0)
!
!        READ IN REDUNDANT TOTAL
!
         IF(NK.GE.2) THEN
            CALL RDTAB1
            CALL TEST2(L2,0,'LF')
            IF(IERX.EQ.1)   GO TO 100
         END IF
!
!        PROCESS ALL PARTIALS
!
         IF(NK.EQ.0)    NK = 1
         DO J2=1,NK
            CALL RDTAB1
            IF(C1.EQ.0.)  THEN
               CALL TESTS(15,MT,ISET)
               IF(ISET.GE.3)  THEN
                  EMESS = 'CONTINUUM PHOTONS REQUIRE A SECTION IN '//   &       
     &                    'MF=15'
                  CALL ERROR_MESSAGE(0)
               END IF
            END IF
            LP = L1
            CALL TEST1(LP,0,2,'LP',1)
            LF = L2
            CALL TEST1(LF,1,2,'LF',1)
            CALL TEST1(NP,2,NEDISMAX,'NP',1)
            IF((C1.EQ.0..AND.LF.NE.1).OR.(C1.NE.0..AND.LF.EQ.1)) THEN
               WRITE(EMESS,'(A,I3,A,1PE12.4,A)')                        &       
     &            'LF=',LF,'  EG=',C1,'  ARE INCONSISTENT'
               CALL ERROR_MESSAGE(NSEQP1)
            END IF
            IF(IERX.EQ.1) GO TO 100
         END DO
      ELSE IF(LO.EQ.2) THEN
!
!     LO=2 TRANSITION PROBABILITIES
!
         LG = L2H
         NS = N1H
         CALL TEST1(LG,1,2,'LG',1)
         CALL TEST1(NS,1,NPHMAX,'NS',1)
         CALL RDLIST
         CALL TEST1(N2L,1,NS,'NT',1)
         LP = L1L
         CALL TEST1(LP,0,2,'LP',1)
         IF(LG.LT.1.OR.LG.GT.2)  LG = (NPL/N2L)-1
         NUM = NPL/(LG+1)
         CALL TEST2(N2L,NUM,'NT')
      END IF
!
  100 RETURN
      END SUBROUTINE CKF12
!
!***********************************************************************
!
      SUBROUTINE TESTNK12(NK,IPATH)
!
!     ROUTINE TO TEST THAT NUMBER OF PHOTONS IN MF=14 FOR A SECTION IS
!       THE SAME AS THE NUMBER IN MF=12 OF MF=13
!
      IMPLICIT NONE
!
      INTEGER(KIND=I4) :: NK,IPATH
!
      INTEGER(KIND=I4) :: N
      INTEGER(KIND=I4), DIMENSION(NSECMAX) :: MTS,NKS
!
      IF(IPATH.EQ.0) THEN
         N12S = N12S + 1
         MTS(N12S) = MT
         NKS(N12S) = NK
      ELSE
         DO N=1,N12S
            IF(MT.EQ.MTS(N)) THEN
               IF(NKS(N).NE.NK) THEN
                  WRITE(EMESS,'(A,I3,A,I4,A)')                          &       
     &               'NK =',NK,' MUST BE EQUAL TO',NKS(N),              &       
     &               ' AS IN FILE 12 OR 13'
                  CALL ERROR_MESSAGE(NSEQP1)
                  GO TO 100
               END IF
            END IF
         END DO
      END IF
!
  100 RETURN
      END SUBROUTINE TESTNK12
!
!***********************************************************************
!
      SUBROUTINE CKF14
!
!     ROUTINE TO CHECK FILE 14 DATA
!
      IMPLICIT NONE
!
      INTEGER(KIND=I4), INTRINSIC :: MOD
!
      INTEGER(KIND=I4) :: MFM1,MFM2,ISET
      INTEGER(KIND=I4) :: NK,NI
      INTEGER(KIND=I4) :: LI,LTT
      INTEGER(KIND=I4) :: NE
      INTEGER(KIND=I4) :: N,M
!
!     TEST FOR A VALID MT NUMBER
!
      CALL TESTFT(MTO,MFO)
!
!     SEE IF SECTION IS IN THE DIRECTORY
!
      CALL TESTD(MFO,MTO)
      IF(IERX.EQ.1)   GO TO 100
!
!     SECTION MUST ALSO EXIST IN FILE 12 OR FILE 13
!
      MFM1 = MF - 1
      MFM2 = MF - 2
      CALL TESTS(MFM2,MT,ISET)
      IF(MOD(ISET,2).EQ.1)   THEN
         CALL TESTS(MFM1,MT,ISET)
         IF(MOD(ISET,2).EQ.1)   THEN
            WRITE(EMESS,'(A,I3,A,I3,A,I3,A,I3)')                        &       
     &        'THIS SECTION REQUIRES THE PRESENCE OF SECTION',MFM2,     &       
     &        '/',MT,' OR',MFM1,'/',MT
            CALL ERROR_MESSAGE(0)
         END IF
      END IF
!
!     CHECK NUMBER OF SUBSECTIONS AND ISOTROPY FLAG
!
      NK = N1H
      CALL TEST1(NK,1,NPHMAX,'NK',1)
      CALL TESTNK12(NK,1)
      LI = L1H
      CALL TEST1(LI,0,1,'LI',1)
!
!     ALL PHOTONS NOT ISOTROPIC
!
      IF(LI.NE.0) GO TO 100
      NI = N2H
      CALL TEST1(NI,0,NK,'NI',1)
      IF(IERX.EQ.1) GO TO 100
!*****IF NK=NI ALL PHOTONS ARE ISOTROPIC
      IF(NI.EQ.NK) THEN
         WRITE(EMESS,'(A,I4,A)') 'LI=0 AND NK=NI=',NK,' USE LI=1'
         CALL ERROR_MESSAGE(NSEQP1)
      END IF
!
!     CHECK FOR VALID REPRESENTATION
!
      LTT = L2H
      CALL TEST1(LTT,1,2,'LTT',2)
      IF(IERX.EQ.1) GO TO 100
!
!     PROCESS EACH SUBSECTION
!
      DO N=1,NK
!
!        ISOTROPIC PHOTON
!
         IF(N.LE.NI) THEN
            CALL RDCONT
!
!        ANISOTROPIC PHOTON
!
         ELSE
            CALL RDTAB2(0)
            NE = NP2
            NR = NR2
            CALL TEST1(NE,1,NEDISMAX,'NE',1)
!
!           PROCESS EACH ENERGY
!
            DO M=1,NE
               IF(LTT.NE.2) THEN
                  CALL RDLIST
                  CALL TEST1(NPL,1,NLEGMAX,'NPL',1)
               ELSE
                  CALL RDTAB1
                  CALL TEST1(NP,2,NANGMAX,'NP',1)
               END IF
               IF(IERX.EQ.1) GO TO 100
            END DO
         END IF
      END DO
!
  100 RETURN
      END SUBROUTINE CKF14
!
!***********************************************************************
!
      SUBROUTINE CKF15
!
!     ROUTINE TO CHECK FILE 15 DATA
!
      IMPLICIT NONE
!
      INTEGER(KIND=I4), INTRINSIC :: MOD
      INTEGER(KIND=I4) :: MFM1,MFM2,ISET
!
      INTEGER(KIND=I4) :: NC
      INTEGER(KIND=I4) :: LF
      INTEGER(KIND=I4) :: NE
      INTEGER(KIND=I4) :: I,NM
!
!     TEST FOR A VALID MT NUMBER
!
      CALL TESTFT(MTO,MFO)
!
!     SEE IF SECTION IS IN THE DIRECTORY
!
      CALL TESTD(MFO,MTO)
      IF(IERX.EQ.1)   GO TO 100
!
!     SECTION MUST ALSO EXIST IN FILE 12 OR FILE 13
!
      MFM1 = MF - 2
      MFM2 = MF - 3
      CALL TESTS(MFM2,MT,ISET)
      IF(MOD(ISET,2).EQ.1)   THEN
         CALL TESTS(MFM1,MT,ISET)
         IF(MOD(ISET,2).EQ.1)   THEN
            WRITE(EMESS,'(A,I3,A,I3,A,I3,A,I3)')                        &       
     &        'THIS SECTION REQUIRES THE PRESENCE OF SECTION',MFM2,     &       
     &        '/',MT,' OR',MFM1,'/',MT
            CALL ERROR_MESSAGE(0)
         END IF
      END IF
!
!     ONLY ONE CONTINUUM SUBSECTION ALLOWED
!
      NC = N1H
      CALL TEST1(NC,1,1,'NC',1)
!
!     PROCESS ALL SUBSECTIONS
!
      DO I=1,NC
         CALL RDTAB1
         CALL TEST1(NP,1,NES5MAX,'NP',1)
         LF = L2
         CALL TEST1(LF,1,1,'LF',1)
         IF(IERX.EQ.1) GO TO 100
!
!        PROCESS A CONTINUUM DISTRIBUTION
!
         CALL RDTAB2(0)
         NE = NP2
         CALL TEST1(NE,1,NES5MAX,'NE',1)
         IF(IERX.EQ.1) GO TO 100
         DO NM=1,NE
            CALL RDTAB1
            CALL TEST1(NP,1,NEDISMAX,'NP',1)
            IF(IERX.EQ.1) GO TO 100
         END DO
      END DO
!
  100 RETURN
      END SUBROUTINE CKF15
!
!***********************************************************************
!
      SUBROUTINE CKF23
!
!     ROUTINE TO CHECK FILE 23 AND 27 DATA
!
      IMPLICIT NONE
!
      REAL(KIND=R4), INTRINSIC :: ABS
!
      REAL(KIND=R4) :: ZTEST
!
!     TEST FOR A VALID MT NUMBER
!
      CALL TESTFT(MTO,MFO)
!
!     SEE IF SECTION IS IN THE DIRECTORY
!
      CALL TESTD(MFO,MTO)
      IF(IERX.EQ.1)   GO TO 100
!
!     A SECTION IN FILE 27 REQUIRES THAT THE SAME SECTION EXIST IN
!       FILE 23
!
      IF(MF.EQ.27)  THEN
         IF(MT.NE.505.AND.MT.NE.506) CALL TESTP(23,MTO)
      END IF
!
!     READ THE TAB1 RECORD
!
      CALL RDTAB1
!
!     TEST THAT THE CHARGE NUMBER IS CORRECT IN FILE 27
!
      IF(MF.EQ.27)   THEN
         ZTEST = ZA - 1000.0*C2
         IF(ABS(ZTEST).GT.1000.0)   THEN
            EMESS = 'Z IS IN ERROR'
            CALL ERROR_MESSAGE(NSEQP1)
         END IF
      END IF
!
  100 RETURN
      END SUBROUTINE CKF23
!
!***********************************************************************
!
      SUBROUTINE CKF26
!
!     ROUTINE TO CHECK FILE 26 DATA
!
      IMPLICIT NONE
!
      INTEGER(KIND=I4) :: NK
      INTEGER(KIND=I4) :: LAW
      INTEGER(KIND=I4) :: LANG,LEP
      INTEGER(KIND=I4) :: NE,NEP
      INTEGER(KIND=I4) :: NW,NL,NWT
      INTEGER(KIND=I4) :: ND,NA
      INTEGER(KIND=I4) :: NAT
      INTEGER(KIND=I4) :: I,N
!
!     TEST FOR A VALID MT NUMBER
!
      CALL TESTFT(MTO,MFO)
!
!     SEE IF SECTION IS IN THE DIRECTORY
!
      CALL TESTD(MFO,MTO)
      IF(IERX.EQ.1)   GO TO 100
!
!     A SECTION IN FILE 26 REQUIRES THAT THE SAME SECTION EXIST IN
!       FILE 23
!
      CALL TESTP(23,MTO)
!
!     CHECK NUMBER OF SUBSECTIONS
!
      NK = N1H
      CALL TEST1(NK,1,NSUBS6MAX,'NK',1)
!
!     LOOP OVER LAWS
!
      DO I=1,NK
         CALL RDTAB1
         LAW = L2
         SELECT CASE (LAW)
!
!           CONTINUUM DISTRIBUTION LAW
!
            CASE (1)
               CALL RDTAB2(2)
!**************TEST ANGULAR REPRESENTATION
               LANG = L12
               CALL TEST2(LANG,1,'LANG')
!**************TEST SECONDARY ENERGY INTERPOLATION
               LEP = L22
               CALL TEST1(LEP,1,INTMAX,'LEP',1)
!**************TEST NUMBER OF INCIDENT ENERGIES
               NE = NP2
               CALL TEST1(NE,1,NES5MAX,'NE',1)
               IF(IERX.EQ.1)   GO TO 100
               DO N=1,NE
                  CALL RDLIST
!*****************CHECK ANGULAR PARAMETER
                  ND = L1L
                  CALL TEST2(ND,0,'ND')
                  NA = L2L
                  CALL TEST2(NA,0,'NA')
                  NEP = N2L
                  NW = NPL
                  NAT = (NW/NEP) - 2
                  CALL TEST2(NA,NAT,'NA')
                  IF(IERX.EQ.1)   GO TO 100
               END DO
!
!           TWO-BODY ANGULAR DISTRIBUTION LAW
!
            CASE (2)
               CALL RDTAB2(2)
!**************TEST NUMBER OF INCIDENT ENERGIES
               NE = NP2
               CALL TEST1(NE,1,NES5MAX,'NE',1)
               IF(IERX.EQ.1)  GO TO 100
               DO N=1,NE
                  CALL RDLIST
                  LANG = L1L
                  NW = NPL
                  NL = N2L
!*****************LEGENDRE REPRESENTATION
                  IF(LANG.EQ.0)    THEN
                     CALL TEST1(NL,1,NLEGMAX,'NL',1)
                     NWT = NL
!*****************TABULAR REPRESENTATION
                  ELSE
                     CALL TEST1(LANG,12,14,'LANG',2)
                     IF(IERX.EQ.1)   GO TO 100
                     CALL TEST1(NL,2,NANGMAX,'NL',1)
                     NWT = 2*NL
                  END IF
                  CALL TEST2(NW,NWT,'NW')
               END DO
!
!           ENERGY TRANSFER FOR EXCITATION
!
            CASE (8)
               CALL RDTAB1
!
!        INVALID DISTRIBUTION LAW
!
            CASE DEFAULT
               WRITE(EMESS,'(A,I3,A)')  'LAW=',LAW,' IS NOT ALLOWED'
               CALL ERROR_MESSAGE(NSEQP1)
               IERX = 1
         END SELECT
      END DO
!
  100 RETURN
      END SUBROUTINE CKF26
!
!***********************************************************************
!
      SUBROUTINE CKF28
!
!     ROUTINE TO CHECK FILE 28 DATA
!
      IMPLICIT NONE
!
      INTEGER(KIND=I4) :: NSS
      INTEGER(KIND=I4) :: NTR,NW,NWT
      INTEGER(KIND=I4) :: I
      REAL(KIND=R4) :: SUBI
!
!     TEST FOR A VALID MT NUMBER
!
      CALL TESTFT(MTO,MFO)
!
!     SEE IF SECTION IS IN THE DIRECTORY
!
      CALL TESTD(MFO,MTO)
      IF(IERX.EQ.1)   GO TO 100
!
!     CHECK NUMBER OF SUBSHELLS
!
      NSS = N1H
      CALL TEST1(NSS,1,NSUBS6MAX,'NSS',1)
!
!     LOOP OVER SUBSHELLS
!
      DO I=1,NSS
         CALL RDLIST
         SUBI = C1L
         CALL TEST1F(SUBI,1.,100.,'SUBI')
         NTR = N2L
         NW = NPL
         NWT = 6*(1+NTR)
         CALL TEST2(NW,NWT,'NW')
      END DO
!
  100 RETURN
      END SUBROUTINE CKF28
!
!***********************************************************************
!
      SUBROUTINE CKF32
!
!     ROUTINE TO CHECK FILE 32 DATA
!
      IMPLICIT NONE
!
      INTEGER(KIND=I4) :: NIS,LRU,LRUM
      INTEGER(KIND=I4) :: LFW
      INTEGER(KIND=I4) :: NER,NERM
      INTEGER(KIND=I4) :: N,NI
!
!     TEST FOR A VALID MT NUMBER
!
      CALL TESTFT(MTO,MFO)
!
!     SEE IF SECTION IS IN THE DIRECTORY
!
      CALL TESTD(MFO,MTO)
      IF(IERX.EQ.1)   GO TO 100
!
!     THE EQUIVALENT SECTION MUST EXIST IN FILE 2
!
      CALL TESTP(2,MTO)
!
!     TEST NUMBER OF ISOTOPES
!
      NIS = N1H
      CALL TEST1(NIS,1,NISMAX,'NIS',1)
!
!     PROCESS EACH ISOTOPE
!
      DO NI=1,NIS
         CALL RDCONT
         IF(NIS.EQ.1)  THEN
            CALL TEST2F(C1H,ZA,'ZAI')
            CALL TEST2F(C2H,1.0,'ABN')
         END IF
!********TEST LFW
         LFW = L2H
         CALL TEST1(LFW,0,1,'LFW',2)
!********TEST NUMBER OF ENERGY RANGES
         NER = N1H
         IF(NFOR.GE.6)  THEN
            NERM = NERM6
         ELSE
            NERM = NERM5
         END IF
         CALL TEST1(NER,1,NERM,'NER',1)
         IF(IERX.EQ.1) GO TO 100
!
!        PROCESS EACH ENERGY RANGE
!
         DO N=1,NER
            CALL RDCONT
!***********TEST LRU
            LRU = L1H
            IF(NFOR.GE.6)   THEN
               LRUM = 2
            ELSE
               LRUM = 1
            END IF
            CALL TEST1(LRU,1,LRUM,'LRU',2)
            IF(IERX.EQ.1)  GO TO 100
            IF(NFOR.LT.6)  THEN
               CALL TEST2(N1H,0,'N1H')
               CALL TEST2(N2H,0,'N2H')
            END IF
!
!           BRANCH ON REPRESENTATION
!
            IF(LRU.EQ.1)  THEN
               CALL RR_COV
            ELSE
!**************CHECK FOR ONLY ONE REGION
               IF(N.NE.NER) THEN
                  EMESS = 'ONLY ONE UNRESOLVED RESONANCE REGION IS '
                  CALL ERROR_MESSAGE(0)
                  EMESS = ' PERMITTED AND IT MUST BE THE LAST ONE.'
                  CALL ERROR_MESSAGE(NSEQP)
               END IF
               CALL UR_COV
            END IF
            IF(IERX.EQ.1)   GO TO 100
         END DO
      END DO
!
  100 RETURN
      END SUBROUTINE CKF32
!
!***********************************************************************
!
      SUBROUTINE RR_COV
!
!     ROUTINE TO CHECK RESOLVED RESONANCE REGION COVARIANCES
!
      IMPLICIT NONE
!
      INTEGER(KIND=I4) :: LRF,LRFM,LSSF,NRO,NAPS,NAPSM,NIT
      INTEGER(KIND=I4) :: NLS,NRS,NUM,NSRS,NLRS
      INTEGER(KIND=I4) :: MPAR,MPARM,LCOMP
      INTEGER(KIND=I4) :: NRB,NCOUNT,NVS,IDP,IDPM
      INTEGER(KIND=I4) :: LB,LT,NE,NE2,NPM,NTT,NEC
      INTEGER(KIND=I4) :: I1,I3,NL
      REAL(KIND=R4) :: AP
!
      LRF = L2H
!
!     SCATTERING LENGTH
!
      NRO = N1H
      NAPS = N2H
      IF(NRO.EQ.1) THEN
         NAPSM = 2
      ELSE
         NAPSM = 1
      END IF
      IF(NFOR.GE.6)  THEN
         CALL TEST1(NAPS,0,NAPSM,'NAPS',1)
         IF(IERX.EQ.1)  GO TO 100
      END IF
!
!     PROCESS ENERGY DEPENDENT SCATTERING LENGTH
!
      IF(NRO.NE.0)  THEN
         CALL RDCONT
         NIT = N2H
         DO I3=1,NIT
            CALL RDLIST
!***********CHECK FOR VALID LB VALUE
            LB = L2L
            CALL TEST1(LB,0,6,'LB',2)
            IF(IERX.EQ.1) GO TO 100
!***********CHECK CONSISTENCY OF FLAGS WITH LAW LB
            LT = L1L
            NE = N2L
            NE2 = N2L*2
            IF(LB.LE.2) THEN
!**************LB = 0, 1, OR 2
               CALL TEST2(LT,0,'LT')
               CALL TEST2(NPL,NE2,'NT')
            ELSE IF(LB.EQ.3.OR.LB.EQ.4) THEN
!**************LB = 3 OR 4
               NPM = N2L - 1
               CALL TEST1(LT,1,NPM,'LT',1)
               CALL TEST2(NPL,NE2,'NT')
            ELSE IF(LB.EQ.5) THEN
!**************LB = 5
               CALL TEST1(LT,0,1,'LS',1)
               NTT = NE*(NE-1) + 1
               IF(LT.EQ.1) NTT = NE*(NE+1)/2
               CALL TEST2(NPL,NTT,'NT')
            ELSE IF(LB.EQ.6) THEN
!**************LB = 6
               CALL TEST2(LT,0,'LT')
               NEC=(NPL-1)/NE
               CALL TEST1(NEC,1,200,'NEC',1)
               NTT=NE*NEC+1
               CALL TEST2(NPL,NTT,'NT')
            END IF
         END DO
      END IF
!
!     TEST LRF
!
      IF(NFOR.GE.6) THEN
         LRFM = LRFM6
      ELSE
         LRFM = LRFM5
      END IF
      CALL TEST1(LRF,1,LRFM,'LRF',2)
      IF(IERX.EQ.1) GO TO 100
!
!     READ NUMBER OF PARTIAL WAVES
!
      CALL RDCONT
!
!     CHECK VALUE OF AP
!
      AP = C2H
      IF(NFOR.GE.6) THEN
         IF(NAPS.NE.2.AND.NRO.EQ.1)  CALL TEST2F(AP,0.0,'AP')
      END IF
!
!     CHECK LSSF AND LCOMP
!
      LSSF = L1H
      CALL TEST2(LSSF,0,'LSSF')
      LCOMP = L2H
      IF(NFOR.GE.6)  THEN
         CALL TEST1(LCOMP,0,1,'LCMP',2)
         IF(IERX.EQ.1)   GO TO 100
      ELSE
         CALL TEST2(LCOMP,0,'LCMP')
      END IF
!
!     CHECK NUMBER OF PARTIAL WAVES (L-VALUES)
!
      NLS = N1H
      IF(LCOMP.EQ.0)  THEN
         CALL TEST1(NLS,1,NLSMAX,'NLS',1)
      ELSE
         CALL TEST2(NLS,0,'NLS')
      END IF
!
!     SINGLE AND MULTILEVEL BREIT-WIGNER ENDF-5 STYLE
!
      IF(LCOMP.NE.1) THEN
         DO NL=1,NLS
            CALL RDLIST
!***********CHECK ON NUMBER OF RESONANCES ALLOWED
            NRS = N2L
            CALL TEST1(NRS,1,NRESMAX,'NRS',2)
            IF(IERX.EQ.1) GO TO 100
!***********CHECK NUMBER OF DATA ITEMS PER RESONANCE
            NUM = NPL/NREP18
            CALL TEST2(NRS,NUM,'NRS')
         END DO
         GO TO 100
      END IF
!
!     NEW STYLE FORMATS
!
      CALL RDCONT
      NSRS = N1H
      NLRS = N2H
!
!     SHORT RANGE CORRELATIONS
!
      IF(NSRS.GT.0)  THEN
         DO I1=1,NSRS
            CALL RDLIST
            MPAR = L1L
            IF(LRF.EQ.4) THEN
               MPARM = 8
            ELSE
               MPARM = 5
            ENDIF
            CALL TEST1(MPAR,1,MPARM,'MPAR',1)
            NRB = N2L
            CALL TEST1(NRB,1,250,'NRB',1)
            NCOUNT = NRB*MPAR
            NVS = NCOUNT*(NCOUNT+1)/2
            NTT = NVS + 6*NRB
            IF(LRF.EQ.4)  THEN
               NTT = NTT + 6*NRB
            ELSE
               NTT = NVS + 6*NRB
            END IF
            CALL TEST2(NPL,NTT,'NPL')
         END DO
      ELSE IF(NLRS.EQ.0)  THEN
!
!     LONG RANGE CORRELATIONS
!
         DO I1=1,NLRS
            CALL RDLIST
!***********CHECK IDP FLAG
            IDP = L1L
            IF(LRF.EQ.4) THEN
               IDPM = 8
            ELSE
               IDPM = 5
            END IF
            CALL TEST1(IDP,1,IDPM,'IDP',1)
!***********CHECK FOR VALID LB VALUE
            LB = L2L
            CALL TEST1(LB,-1,5,'LB',2)
            IF(IERX.EQ.1) GO TO 100
!***********CHECK CONSISTENCY OF FLAGS WITH LAW LB
            NE = N2L
            NE2 = N2L*2
            SELECT CASE (LB)
!**************LB = -1, 0, 1, OR 2
               CASE (-1:2)
                 CALL TEST2(LT,0,'LT')
                 CALL TEST2(NPL,NE2,'NT')
!**************LB = 3 OR 4
               CASE (3:4)
                  WRITE(EMESS,'(A,I2,A)') 'LB=',LB,' NOT ALLOWED'
                  CALL ERROR_MESSAGE(NSEQP1)
!**************LB = 5
               CASE (5)
                  CALL TEST1(LT,0,1,'LS',1)
                  IF(LT.EQ.1) THEN
                     NTT = NE*(NE+1)/2
                  ELSE
                     NTT = NE*(NE-1) + 1
                  END IF
                  CALL TEST2(NPL,NTT,'NT')
            END SELECT
         END DO
      END IF
!
  100 RETURN
      END SUBROUTINE RR_COV
!
!***********************************************************************
!
      SUBROUTINE UR_COV
!
!     ROUTINE TO CHECK UNRESOLVED RESONANCE REGION COVARIANCES
!
      IMPLICIT NONE
!
      INTEGER(KIND=I4) :: LRF,LSSF,NLS,NJS
      INTEGER(KIND=I4) :: NCOUNT,MPAR,NPAR,NPART,NTT
      INTEGER(KIND=I4) :: NL
!
      LRF = L2H
      CALL TEST1(LRF,1,1,'LRF',2)
      IF(IERX.EQ.1)   GO TO 100
!
!     ALL PARAMETERS ENERGY INDEPENDENT
!
      CALL RDCONT
!*****CHECK LSSF FLAG
      LSSF = L1H
      CALL TEST1(LSSF,0,1,'LSSF',1)
!*****CHECK NUMBER OF L VALUES
      NLS = N1H
      CALL TEST1(NLS,1,NLSMAX,'NLS',1)
!
!     PROCESS EACH L VALUE
!
      NCOUNT = 0
      DO NL=1,NLS
         CALL RDLIST
!********CHECK NUMBER OF J VALUES
         NJS = N2L
         CALL TEST1(NJS,1,NJSMAX,'NJS',1)
         CALL TEST2(NPL,NREP6*NJS,'NPL')
         NCOUNT = NCOUNT + NJS
      END DO
!
!     UNRESOLVED COVARIANCE MATRIX
!
      CALL RDLIST
      MPAR = L1L
      CALL TEST1(MPAR,1,5,'MPAR',1)
      NPAR = N2L
      NPART = MPAR*NCOUNT
      CALL TEST2(NPAR,NPART,'NPAR')
      NTT = NPAR*(NPAR+1)/2
      CALL TEST2(NPL,NTT,'NPL')
!
  100 RETURN
      END SUBROUTINE UR_COV
!
!***********************************************************************
!
      SUBROUTINE CKF33
!
!     ROUTINE TO CHECK FILE 31 AND 33 DATA
!
      IMPLICIT NONE
!
      INTEGER(KIND=I4) :: ISET
      INTEGER(KIND=I4) :: MTL
      INTEGER(KIND=I4) :: NL,NC,NI
      INTEGER(KIND=I4) :: MAT1,MAT1B,MT1,MT1B
      INTEGER(KIND=I4) :: IE
      INTEGER(KIND=I4) :: I1
!
!     TEST FOR A VALID MT NUMBER
!
      CALL TESTFT(MTO,MFO)
!
!     SEE IF SECTION IS IN THE DIRECTORY
!
      CALL TESTD(MFO,MTO)
      IF(IERX.EQ.1)   GO TO 100
!
!     LUMPED OR NON-LUMPED COVARIANCE
!
      MTL = L2H
      IF(MT.GE.851.AND.MT.LE.870)   THEN
!********A LUMPED COVARIANCE CANNOT REFER TO A LUMPED COVARIANCE
         CALL TEST2(MTL,0,'MTL')
      ELSE
!*******A SECTION IN FILE 31 OR 33 REQUIRES THAT THE SAME SECTION EXIST
!******* IN FILE 1 OR 3 RESPECTIVELY FOR A NON-LUMPED COVARIANCE
         CALL TESTP(MFO-30,MTO)
!********ALLOWED LUMPED MT'S ARE BETWEEN 851 AND 870
         IF(MTL.NE.0)   CALL TEST1(MTL,851,870,'MTL',1)
      END IF
!
!     LUMPED PARTIAL COVARIANCE
!
      IF(MTL.NE.0) THEN
!
!        CHECK INDEX FOR REQUIRED SECTION LATER IN THE CURRENT FILE
!
         CALL TESTS(MF,MTL,ISET)
         IF(ISET.GE.3)  THEN
            WRITE(EMESS,'(A,I3,A)') 'MTL=',MTL,' MAY BE INCORRECT'
            CALL ERROR_MESSAGE(NSEQP)
            EMESS = '    COVARIANCE SECTION FOR MT = MTL '//            &       
     &             'IS MISSING'
            CALL ERROR_MESSAGE(0)
         END IF
!
!        CHECK SEQUENCING OF MTL-VALUES
!
         IF(MTL.EQ.(MTLP+1).OR.MTL.EQ.MTLP) THEN
            MTLP = MTL
         ELSE
            WRITE(EMESS,'(A,I3,A)')                                     &       
     &         'MTL=',MTL,' IS ASSIGNED OUT OF ORDER'
            CALL ERROR_MESSAGE(NSEQP)
         END IF
         GO TO 100
      END IF
!
!     ORDINARY PARTIAL COVARIANCES
!
!*****TEST THAT THE NUMBER OF SUBSECTIONS, NL>0
      NL = N2H
      CALL TEST1(NL,1,NSUBSMAX,'NL',1)
      IF(IERX.EQ.1) GO TO 100
!
!     LOOP OVER SUBSECTIONS
!
      DO I1=1,NL
         CALL RDCONT
         MAT1 = L1H
         MT1 = L2H
         NC = N1H
         NI = N2H
!
!        CHECK SEQUENCING OF MAT1 AND MT1
!
         IF(I1.LE.1) THEN
            MAT1B = 0
            MT1B = MT
            IF(MAT1.NE.0.OR.MT1.NE.MT) THEN
               EMESS = 'FIRST SUBSECTION MUST BE MAT1=0, MT1=MT'
               CALL ERROR_MESSAGE(NSEQP)
            END IF
         ELSE
            IE = 0
            IF(MAT1.EQ.MAT1B) THEN
               IF(MT1.LE.MT1B)  IE = 1
            ELSE IF(MAT1.LT.MAT1B) THEN
               IE = 1
            END IF
            IF(IE.EQ.0) THEN
               MT1B = MT1
               MAT1B = MAT1
            ELSE
               WRITE(EMESS,'(A,I4,A,I3,A)')                             &       
     &            'SUBSECTION MAT1/MT1 = ',MAT1,'/',MT1,                &       
     &            ' OUT OF ORDER'
               CALL ERROR_MESSAGE(NSEQP)
            END IF
         END IF
!
!        CHECK INDEX FOR REQUIRED SECTIONS LATER IN THE CURRENT FILE
!
         IF(MAT1.EQ.0)   THEN
            CALL TESTS(MF,MT1,ISET)
            IF(ISET.GE.3) THEN
               WRITE(EMESS,'(A,I3,A,I3,A)')                             &       
     &              'REQUIRED COVARIANCE SECTION',MF,'/',MT1,' MISSING'
               CALL ERROR_MESSAGE(NSEQP)
            END IF
         END IF
!
!        PROCESS SUB-SUBSECTIONS
!
         CALL CHK_COVSUB(NC,NI,MT1,MAT1)
      END DO
!
  100 RETURN
      END SUBROUTINE CKF33
!
!***********************************************************************
!
      SUBROUTINE CKF34
!
!     ROUTINE TO CHECK FILE 34 DATA
!
      IMPLICIT NONE
!
      INTEGER(KIND=I4) :: ISET
      INTEGER(KIND=I4) :: NMT1
      INTEGER(KIND=I4) :: LEZERO
      INTEGER(KIND=I4) :: NSEQPT
      INTEGER(KIND=I4) :: MAT1,MT1,MT1B
      INTEGER(KIND=I4) :: NL,NL1,NSS
      INTEGER(KIND=I4) :: LTT
      INTEGER(KIND=I4) :: IE
      INTEGER(KIND=I4) :: NI
      INTEGER(KIND=I4) :: LL,LLP,LL1,LL1P
      INTEGER(KIND=I4) :: I1,I2,I3
!
!     TEST FOR A VALID MT NUMBER
!
      CALL TESTFT(MTO,MFO)
!
!     SEE IF SECTION IS IN THE DIRECTORY
!
      CALL TESTD(MFO,MTO)
      IF(IERX.EQ.1)   GO TO 100
!
!     A SECTION IN FILE 34 REQUIRES THAT THE SAME SECTION EXIST IN
!        FILE 4
!
      CALL TESTP(MFO-30,MTO)
!
!     TEST THE NUMBER OF SUBSECTIONS, NMT1
!
      NMT1 = N2H
      CALL TEST1(NMT1,1,NSUBSMAX,'NMT1',1)
!
!     CHECK LTT
!
      LTT = L2H
      IF(LTT.NE.1.AND.LTT.NE.3)  THEN
         EMESS = 'LTT MUST BE EITHER 1 OR 3'
         CALL ERROR_MESSAGE(NSEQP)
      END IF
      LEZERO = 0
      NSEQPT = NSEQP
!
!     LOOP OVER SUBSECTIONS
!
      DO I1=1,NMT1
         CALL RDCONT
         MAT1 = L1H
         MT1 = L2H
         NL = N1H
         NL1 = N2H
         NSS = NL*NL1
         IF(MT.EQ.MT1) THEN
            NSS = NL*(NL+1)/2
         ELSE
            NSS = NL*NL1
         END IF
!
!        CHECK FOR ILLEGAL CROSS MATERIAL COVARIANCES
!
         IF(MAT1.NE.0)  THEN
            EMESS = 'CROSS MATERIAL COVARIANCES ARE NOT ALLOWED'
            CALL ERROR_MESSAGE(NSEQP)
         END IF
!
!        CHECK SEQUENCING OF MT1
!
         IF(I1.EQ.1) THEN
            MT1B = MT
            IF(MT1.NE.MT) THEN
               EMESS = 'FIRST SUBSECTION MUST BE MT1=MT'
               CALL ERROR_MESSAGE(NSEQP)
            END IF
         ELSE
            IF(MT1.LE.MT1B)   THEN
               WRITE(EMESS,'(A,I3,A)')                                  &       
     &              'SUBSECTION MT1 = ',MT1,' OUT OF ORDER'
               CALL ERROR_MESSAGE(NSEQP)
            ELSE
               MT1B = MT1
            END IF
         END IF
!
!        CHECK INDEX FOR REQUIRED SECTIONS LATER IN THE CURRENT FILE
!
         CALL TESTS(MF,MT1,ISET)
         IF(ISET.GE.3) THEN
            WRITE(EMESS,'(A,I3,A)')                                     &       
     &         'REQUIRED COVARIANCE SECTION 34/',MT1,' MISSING'
            CALL ERROR_MESSAGE(NSEQP)
         END IF
!
!     PROCESS ALL SUB-SUBSECTIONS
!
         LLP = -1
         LL1P = -1
         DO I2=1,NSS
            CALL RDCONT
            LL = L1H
            LL1 = L2H
            IF(LL.EQ.0.OR.LL1.EQ.0)  LEZERO = 1
!
!           CHECK ON THE ORDER OF L AND L1
!
            IE = 0
            IF(LL.LT.LLP) THEN
               IE = 1
            ELSE IF(LL.EQ.LLP) THEN
               IF(LL1.LE.LL1P)   IE = 1
            END IF
            IF(IE.EQ.0) THEN
               LLP = LL
               LL1P = LL1
            ELSE
               WRITE(EMESS,'(A,I3,A,I2,A,I2,A)')                        &       
     &            'SUB-SUBSECTION MT1/L/L1 = ',MT1,'/',LL,'/',          &       
     &             LL1,' OUT OF ORDER'
               CALL ERROR_MESSAGE(NSEQP1)
            END IF
!
!           PROCESS EACH LIST RECORD IN THE SUB-SUBSECTION
!
            NI = N2H
            DO I3=1,NI
               CALL RDLIST
               CALL TEST_LB(MAT1,MT1)
            END DO
         END DO
      END DO
!
!     CHECK CONSISTANCY OF LTT AND LOWEST LEGENDRE ORDER
!
      IF(LEZERO.GT.0.AND.LTT.NE.3) THEN
         EMESS = 'LTT MUST BE 3 WHEN L=0 LEGENDRE TERMS INCLUDED'
         CALL ERROR_MESSAGE(NSEQPT)
      END IF
!
  100 RETURN
      END SUBROUTINE CKF34
!
!***********************************************************************
!
      SUBROUTINE CKF35
!
!     ROUTINE TO CHECK FILE 35 DATA
!
      IMPLICIT NONE
!
      INTEGER(KIND=I4) :: LS,LB
      INTEGER(KIND=I4) :: NK
      INTEGER(KIND=I4) :: NE,NTT
      INTEGER(KIND=I4) :: I1
      REAL(KIND=R4) :: EL,EU
!
!     TEST FOR A VALID MT NUMBER
!
      CALL TESTFT(MTO,MFO)
!
!     SEE IF SECTION IS IN THE DIRECTORY
!
      CALL TESTD(MFO,MTO)
      IF(IERX.EQ.1)   GO TO 100
!
!     A SECTION IN FILE 35 REQUIRES THAT THE SAME SECTION EXIST IN
!        FILE 5
!
      CALL TESTP(MFO-30,MTO)
!
!     TEST THE NUMBER OF SUBSECTIONS, NK
!
      NK = N1H
      CALL TEST1(NK,1,NSUBSMAX,'NK',1)
!
!     LOOP OVER SUBSECTIONS
!
      DO I1=1,NK
         CALL RDLIST
!
!        CHECK FOR VALID LS AND LB VALUE
!
         LS = L1L
         CALL TEST1(LS,1,1,'LS',1)
         LB = L2L
         CALL TEST1(LB,7,7,'LB',1)
!
!        CHECK CONTINUITY OF ENERGY BOUNDARIES
!
         EL = C1L
         IF(I1.GT.1.AND.EL.NE.EU) THEN
            WRITE(EMESS,'(A,I2)')                                       &       
     &           'LOWER ENERGY OF SUBSECTION ',I1,' DOES NOT'
            CALL ERROR_MESSAGE(0)
            WRITE(EMESS,'(12X,A,I2)')                                   &       
     &           'MATCH UPPER ENERGY OF SUBSECTION ',I1-1
            CALL ERROR_MESSAGE(NSEQP1)
         END IF
         EU = C2L
!
!        CHECK CONSISTENCY OF FLAGS WITH LAW LB=7
!
         LS = L1L
         NE = N2L
         CALL TEST2(LS,1,'LS')
         NTT = NE*(NE+1)/2
         CALL TEST2(NPL,NTT,'NT')
      END DO
!
  100 RETURN
      END SUBROUTINE CKF35
!
!***********************************************************************
!
      SUBROUTINE CKF40
!
!     ROUTINE TO CHECK FILE 40 DATA
!
      IMPLICIT NONE
!
      INTEGER(KIND=I4) :: NS,NL
      INTEGER(KIND=I4) :: NC,NI
      INTEGER(KIND=I4) :: LFS,LFSP
      INTEGER(KIND=I4) :: MAT1,MF1,MT1,LFS1
      INTEGER(KIND=I4) :: MAT1B,MF1B,MT1B,LFS1B
      INTEGER(KIND=I4) :: IE,ISET
      INTEGER(KIND=I4) :: I1,N
!
!     TEST FOR A VALID MT NUMBER
!
      CALL TESTFT(MTO,MFO)
!
!     SEE IF SECTION IS IN THE DIRECTORY
!
      CALL TESTD(MFO,MTO)
      IF(IERX.EQ.1)   GO TO 100
!
!     A SECTION IN FILE 40 REQUIRES THAT THE SAME SECTION EXIST IN
!       FILE 10
!
      CALL TESTP(MFO-30,MTO)
!
!     CHECK THE STATE OF TARGET NUCLEUS IS CORRECT
!
      CALL TEST2(L1H,LIS,'LIS')
!
!     CHECK NUMBER OF EXCITED STATES
!
      NS = N1H
      CALL TEST1(NS,1,NSUBSMAX,'NS',1)
!
!     PROCESS EACH EXCITED FINAL STATE
!
      LFSP = -1
      DO N=1,NS
         CALL RDCONT
         LFS = L2H
         IF (LFS.LE.LFSP) THEN
            EMESS = 'DATA NOT GIVEN IN ORDER OF INCREASING LFS'
            CALL ERROR_MESSAGE(NSEQP1)
         ELSE
            LFSP = LFS
         END IF
!
!     PROCESS EACH COVARIANCE MATRIX
!
         NL = N2H
         DO I1=1,NL
            CALL RDCONT
            MAT1 = L1H
            MT1 = L2H
            NC = N1H
            NI = N2H
            MF1 = IFIX(C1H)
            LFS1 = IFIX(C2H)
            IF(LFS1.NE.0.AND.MF1.NE.10) THEN
               EMESS = 'XLFS1 MUST BE ZERO WHEN XMF1 IS NOT 10.0'
               CALL ERROR_MESSAGE(NSEQP)
            END IF
!
!           CHECK SEQUENCING OF MAT1, MF1, MT1 AND LFS1
!
            IF(I1.EQ.1) THEN
               MAT1B = 0
               MF1B = 0
               MT1B = MT
               LFS1B = LFS
               IF(MAT1.NE.0.OR.MF1.NE.10.OR.MT1.NE.MT.OR.               &       
     &                 LFS1.NE.LFS) THEN
                  EMESS = 'FIRST SUB-SUBSECTION MUST BE MAT1=0, MF1=10,'
                  CALL ERROR_MESSAGE(0)
                  EMESS = '    MT1=MT AND LFS1=LFS'
                  CALL ERROR_MESSAGE(NSEQP)
               END IF
            ELSE
               IE = 0
               IF(MAT1.LT.MAT1B)  THEN
                  IE = 1
               ELSE IF(MAT1.EQ.MAT1B) THEN
                  IF(MF1.LT.MF1B)   THEN
                     IE = 1
                  ELSE IF(MF1.EQ.MF1B) THEN
                     IF(MT1.LT.MT1B)   THEN
                        IE = 1
                     ELSE IF(MT1.EQ.MT1B) THEN
                        IF(LFS1.LE.LFS1B) IE = 1
                     END IF
                  END IF
               END IF
               IF(IE.EQ.0) THEN
                  MAT1B = MAT1
                  MF1B = MF1
                  MT1B = MT1
                  LFS1B = LFS1
               ELSE
                  WRITE(EMESS,'(A,I4,A,I2,A,I3,A,I2)')                  &       
     &                'SUB-SUBSECTION MAT1/MF1/MT1/LFS1 = ',MAT1,'/',   &       
     &                         MF1,'/',MT1,'/',LFS1
                  CALL ERROR_MESSAGE(0)
                  EMESS = 'OUT OF ORDER'
                  CALL ERROR_MESSAGE(NSEQP)
               END IF
            END IF
!
!           CHECK INDEX FOR REQUIRED SECTIONS LATER IN THE CURRENT FILE
!
            IF(MAT1.EQ.0)   THEN
               CALL TESTS(MF,MT1,ISET)
               IF(ISET.GE.3) THEN
                  WRITE(EMESS,'(A,I3,A,I3,A)')                          &       
     &                    'REQUIRED COVARIANCE SECTION',MF1,'/',MT1,    &       
     &                    ' MISSING'
                  CALL ERROR_MESSAGE(NSEQP)
               END IF
            END IF
!
!           PROCESS SUB-SUBSECTIONS
!
            CALL CHK_COVSUB(NC,NI,MT1,MAT1)
         END DO
      END DO
!
  100 RETURN
      END SUBROUTINE CKF40
!
!***********************************************************************
!
      SUBROUTINE CHK_COVSUB(NC,NI,MT1T,MAT1T)
!
!     ROUTINE TO CHECK COVARIANCE SUBSECTIONS
!
      IMPLICIT NONE
!
      INTEGER(KIND=I4) :: NC,NI
      INTEGER(KIND=I4) :: MAT1T,MT1T
!
      INTEGER(KIND=I4) :: LTY
      INTEGER(KIND=I4) :: MATS
      INTEGER(KIND=I4) :: I2,I3
!
!     NC AND NI BOTH CANNOT BE ZERO
!
      IF(NC.EQ.0.AND.NI.EQ.0)  THEN
         EMESS = 'NC=0 AND NI=0 ILLEGAL, INDICATES EMPTY SUBSECTION'
         CALL ERROR_MESSAGE(NSEQP)
         GO TO 100
      END IF
!
!     PROCESS NC TYPE SUB-SUBSECTIONS
!
      IF(NC.GT.0) THEN
         DO I2=1,NC
            CALL RDCONT
            CALL RDLIST
!
!           CHECK FOR VALID LTY
!
            LTY = L2H
            CALL TEST1(LTY,0,3,'LTY',1)
!
!           CHECK CONSISTENCY OF DERIVATION FLAG LTY WITH MAT1 AND MT1.
!
            IF(LTY.EQ.0)  THEN
               CALL TEST2(MT1T,MT,'MT1')
               CALL TEST2(MAT1T,0,'MAT1')
            ELSE IF(LTY.EQ.1) THEN
               MATS = L1L
               IF(MATS.EQ.MAT)  THEN
                  EMESS = 'FOR LTY=1, MATS CANNOT = MAT'
                  CALL ERROR_MESSAGE(NSEQP1)
               END IF
            END IF
         END DO
      END IF
!
!     PROCESS NI TYPE SUB-SUBSECTIONS
!
      IF(NI.GT.0) THEN
         DO I3=1,NI
            CALL RDLIST
            CALL TEST_LB(MAT,MT)
         END DO
      END IF
!
  100 RETURN
      END SUBROUTINE CHK_COVSUB
!
!***********************************************************************
!
      SUBROUTINE TEST_LB(MAT1I,MT1I)
!
!     CHECK FOR VALID LB VALUE IN A COVARIANCE SECTION
!
      IMPLICIT NONE
!
      INTEGER(KIND=I4) :: MAT1I,MT1I
!
      INTEGER(KIND=I4) :: LB,LBM
      INTEGER(KIND=I4) :: LT
      INTEGER(KIND=I4) :: NE,NTT
      INTEGER(KIND=I4) :: NE2,NEC
      INTEGER(KIND=I4) :: NPM
!
!     TEST THAT VALUE OF LB IS IN ALLOWED RANGE
!
      LB = L2L
      IF(MF.EQ.31.OR.MF.EQ.33) THEN
         LBM = 8
      ELSE
         LBM = 6
      END IF
      CALL TEST1(LB,0,LBM,'LB',2)
      IF(IERX.EQ.1) GO TO 100
!
!     CHECK CONSISTENCY OF FLAGS WITH LAW LB
!
      LT = L1L
      NE = N2L
      NE2 = N2L*2
      SELECT CASE (LB)
!********LB = 0, 1, OR 2
         CASE (0:2)
            CALL TEST2(LT,0,'LT')
            CALL TEST2(NPL,NE2,'NT')
!********LB = 3 OR 4
         CASE (3:4)
            IF(MF.EQ.34) THEN
               WRITE(EMESS,'(A,I1,A)')  'LB=',LB,' INVALID'
               CALL ERROR_MESSAGE(NSEQP1)
               IERX = 1
            ELSE
               NPM = N2L - 1
               CALL TEST1(LT,1,NPM,'LT',1)
               CALL TEST2(NPL,NE2,'NT')
            END IF
!********LB = 5
         CASE (5)
            CALL TEST1(LT,0,1,'LS',1)
            IF(LT.EQ.1) THEN
               NTT = NE*(NE+1)/2
            ELSE
               NTT = NE*(NE-1) + 1
           END IF
            CALL TEST2(NPL,NTT,'NT')
!********LB = 6
         CASE (6)
            IF(MAT1I.EQ.0.AND.MT1I.EQ.MT) THEN
               EMESS = 'LB=6 INVALID, SELF COVARIANCE IS SYMMETRIC'
               CALL ERROR_MESSAGE(NSEQP1)
            END IF
            CALL TEST2(LT,0,'LT')
            NEC=(NPL-1)/NE
            CALL TEST1(NEC,1,NES5MAX,'NEC',1)
            NTT=NE*NEC+1
            CALL TEST2(NPL,NTT,'NT')
!********LB = 7
         CASE (7)
            WRITE(EMESS,'(A,I1,A)')  'LB=',LB,' INVALID'
            CALL ERROR_MESSAGE(NSEQP1)
            IERX = 1
!********LB = 8
         CASE (8)
            CALL TEST2(LT,0,'LT')
            CALL TEST2(NPL,NE2,'NT')
      END SELECT
!
  100 RETURN
      END SUBROUTINE TEST_LB
!
!***********************************************************************
!
      SUBROUTINE RDTEXT
!
!     PROCESS A TEXT RECORD
!
      IMPLICIT NONE
!
!     READ IN RECORD
!
      READ(JIN,'(A)',END=90) IFIELD
      ISEQ = ISEQ + 1
      NSEQP = 0
      READ(IFIELD,'(A,I4,I2,I3,I5)',ERR=30)  TEXT,MATP,MFP,MTP,NSEQP
      IF(ASEQ.EQ.' ') NSEQP = ISEQ
      GO TO 40
!
!     ERROR SO TRY TO READ EACH FIELD SEPARATELY
!
   30 IF(ASEQ.EQ.' ') NSEQP = ISEQ
      CALL SEQRD(1,MATP,MFP,MTP,NSEQP)
!
!     TEST CONTENTS OF RECORD ID FIELDS
!
   40 NSEQP1 = NSEQP
      CALL TESTL
      GO TO 100
!
!     UNEXPECTED END OF FILE
!
   90 IERX = 2
!
  100 RETURN
      END SUBROUTINE RDTEXT
!
!***********************************************************************
!
      SUBROUTINE RDCONT
!
!     PROCESS A CONT RECORD
!
      IMPLICIT NONE
!
!     READ IN RECORD
!
      READ(JIN,'(A)',END=90) IFIELD
      ISEQ = ISEQ + 1
      NSEQP = 0
      READ(IFIELD,'(2E11.4,4I11,I4,I2,I3,I5)',ERR=30)                   &       
     &          C1H,C2H,L1H,L2H,N1H,N2H,MATP,MFP,MTP,NSEQP
      IF(ASEQ.EQ.' ') NSEQP = ISEQ
      GO TO 40
!
!     ERROR SO TRY TO READ EACH FIELD SEPARATELY
!
   30 IF(ASEQ.EQ.' ') NSEQP = ISEQ
      CALL HEADRD(C1H,C2H,L1H,L2H,N1H,N2H,MATP,MFP,MTP,NSEQP)
!
!     TEST CONTENTS OF RECORD ID FIELDS
!
   40 NSEQP1 = NSEQP
      CALL TESTL
      GO TO 100
!
!     UNEXPEXTED END OF FILE
!
   90 IERX = 2
!
  100 RETURN
      END SUBROUTINE RDCONT
!
!***********************************************************************
!
      SUBROUTINE RDTAB1
!
!     PROCESS A TAB1 RECORD
!
      IMPLICIT NONE
!
      INTEGER(KIND=I4) :: NI,NF
      INTEGER(KIND=I4) :: I,N
      REAL(KIND=R4), DIMENSION(6) :: ANT
!
!     READ IN CONT-LIKE RECORD
!
      READ(JIN,'(A)',END=90) IFIELD
      ISEQ = ISEQ + 1
      NSEQP = 0
      READ(IFIELD,'(2E11.4,4I11,I4,I2,I3,I5)',ERR=10)                   &       
     &                 C1,C2,L1,L2,NR,NP,MATP,MFP,MTP,NSEQP
      IF(ASEQ.EQ.' ') NSEQP = ISEQ
      GO TO 20
!
!     ERROR SO TRY TO READ EACH FIELD SEPARATELY
!
   10 IF(ASEQ.EQ.' ') NSEQP = ISEQ
      CALL HEADRD(C1,C2,L1,L2,NR,NP,MATP,MFP,MTP,NSEQP)
!
!     TEST RECORD CONTENTS
!
   20 NSEQP1 = NSEQP
      CALL TESTL
!
!     CHECK LIMITS FOR INTERPOLATION REGIONS AND NUMBER OF TAB1 RECORDS
!       FOLLOWING
!
      CALL TEST1(NR,1,NIRMAX,'NR',2)
      IF(LDRV.EQ.0)  THEN
         CALL TEST1(NP,1,NPTSMAX,'NP',1)
         IF(IERX.EQ.1) GO TO 100
      END IF
!
!     PROCESS INTERPOLATION TABLE
!
      NI = 1
      DO I=1,NR,3
         NF = NI + 2
         ISEQ = ISEQ + 1
         NSEQP = 0
         READ(JIN,'(A)',END=90) IFIELD
         READ(IFIELD,'(6I11,I4,I2,I3,I5)',ERR=40,END=90)                &       
     &              (NBT(N),JNT(N),N=NI,NF),MATP,MFP,MTP,NSEQP
         IF(ASEQ.EQ.' ') NSEQP = ISEQ
         GO TO 50
!
!        ERROR SO TRY TO READ EACH FIELD SEPARATELY
!
   40    IF(ASEQ.EQ.' ') NSEQP = ISEQ
         CALL INTRD(NBT(NI),JNT(NI),MATP,MFP,MTP,NSEQP)
!
!        TEST CONTENTS OF RECORD ID FIELDS
!
   50    CALL TESTL
         NI = NI + 3
      END DO
!
!     TEST CONTENTS OF INTERPOLATION TABLE
!
      CALL TESTIN(NBT,JNT,NR,NP,0)
!
!     PROCESS DATA TABLE
!
      DO N=1,NP,3
         ISEQ = ISEQ + 1
         READ(JIN,'(A)',END=90) IFIELD
         READ(IFIELD,'(6E11.4,I4,I2,I3,I5)',ERR=70,END=90)              &       
     &                            ANT,MATP,MFP,MTP,NSEQP
         GO TO 80
!
!        ERROR SO TRY TO READ EACH FIELD SEPARATELY
!
   70    CALL DATARD(ANT,MATP,MFP,MTP,NSEQP)
!
!        TEST CONTENTS OF RECORD ID FIELDS
!
   80    CALL TESTL
      END DO
      GO TO 100
!
!     UNEXPECTED END OF FILE
!
   90 IERX = 2
!
  100 RETURN
      END SUBROUTINE RDTAB1
!
!***********************************************************************
!
      SUBROUTINE RDTAB2(IBMOD)
!
!     PROCESS A TAB2 RECORD
!
      IMPLICIT NONE
!
      INTEGER(KIND=I4) :: IBMOD
!
      INTEGER(KIND=I4) :: ICMOD
      INTEGER(KIND=I4) :: NI,NF
      INTEGER(KIND=I4) :: I,N
!
!     READ IN CONT-LIKE RECORD
!
      READ(JIN,'(A)',END=90) IFIELD
      ISEQ = ISEQ + 1
      NSEQP = 0
      READ(IFIELD,'(2E11.4,4I11,I4,I2,I3,I5)',ERR=30)                   &       
     &        C12,C22,L12,L22,NR2,NP2,MATP,MFP,MTP,NSEQP
      IF(ASEQ.EQ.' ') NSEQP = ISEQ
      GO TO 40
!
!     ERROR SO TRY TO READ EACH FIELD SEPARATELY
!
   30 IF(ASEQ.EQ.' ') NSEQP = ISEQ
      CALL HEADRD(C12,C22,L12,L22,NR2,NP2,MATP,MFP,MTP,NSEQP)
!
!     TEST CONTENTS OF RECORD ID FIELDS
!
   40 NSEQP1 = NSEQP
      CALL TESTL
!
!     CHECK LIMITS FOR INTERPOLATION REGIONS AND NUMBER OF TAB1 RECORDS
!       FOLLOWING
!
      CALL TEST1(NR2,1,NIRMAX,'NR2',2)
      CALL TEST1(NP2,1,NPTS2MAX,'NP2',1)
      IF(IERX.EQ.1) GO TO 100
!
!     PROCESS INTERPOLATION TABLE
!
      NI = 1
      DO I=1,NR2,3
         NF = NI + 2
         ISEQ = ISEQ + 1
         NSEQP = 0
         READ(JIN,'(A)',END=90) IFIELD
         READ(IFIELD,'(6I11,I4,I2,I3,I5)',ERR=60,END=90)                &       
     &             (NBT2(N),JNT2(N),N=NI,NF),MATP,MFP,MTP,NSEQP
         IF(ASEQ.EQ.' ') NSEQP = ISEQ
         GO TO 70
!
!        ERROR SO TRY TO READ EACH FIELD SEPARATELY
!
   60    IF(ASEQ.EQ.' ') NSEQP = ISEQ
         CALL INTRD(NBT2(NI),JNT2(NI),MATP,MFP,MTP,NSEQP)
!
!        TEST CONTENTS OF RECORD ID FIELDS
!
   70    CALL TESTL
         NI=NI+3
      END DO
!
!     TEST CONTENTS OF INTERPOLATION TABLE
!
      ICMOD = IBMOD
      CALL TESTIN(NBT2,JNT2,NR2,NP2,ICMOD)
      GO TO 100
!
!     UNEXPECTED END OF FILE
!
   90 IERX = 2
!
  100 RETURN
      END SUBROUTINE RDTAB2
!
!***********************************************************************
!
      SUBROUTINE RDLIST
!
!     PROCESS A LIST RECORD
!
      IMPLICIT NONE
!
      INTEGER(KIND=I4) :: NN
      INTEGER(KIND=I4) :: I,N
      REAL(KIND=R4), DIMENSION(6) :: ANT
!
!     READ IN CONT-LIKE RECORD
!
      READ(JIN,'(A)',END=90) IFIELD
      ISEQ = ISEQ + 1
      NSEQP = 0
      READ(IFIELD,'(2E11.4,4I11,I4,I2,I3,I5)',ERR=10)                   &       
     &         C1L,C2L,L1L,L2L,NPL,N2L,MATP,MFP,MTP,NSEQP
      IF(ASEQ.EQ.' ') NSEQP = ISEQ
      GO TO 20
!
!     ERROR SO TRY TO READ EACH FIELD SEPARATELY
!
   10 IF(ASEQ.EQ.' ') NSEQP = ISEQ
      CALL HEADRD(C1L,C2L,L1L,L2L,NPL,N2L,MATP,MFP,MTP,NSEQP)
!
!     TEST CONTENTS OF RECORD ID FIELDS
!
   20 NSEQP1 = NSEQP
      CALL TESTL
!
!     CHECK LIMIT FOR NUMBER OF ENTRIES IN THE LIST RECORD
!
      CALL TEST1(NPL,1,NPTSMAX,'NPL',1)
!
!     PROCESS DATA TABLE
!
      DO N=1,NPL,6
         ISEQ = ISEQ + 1
         NSEQP = 0
         READ(JIN,'(6E11.4,I4,I2,I3,I5)',ERR=40,END=90)                 &       
     &                            ANT,MATP,MFP,MTP,NSEQP
         IF(ASEQ.EQ.' ') NSEQP = ISEQ
         GO TO 50
!
!        ERROR SO TRY TO READ EACH FIELD SEPARATELY
!
   40  IF(ASEQ.EQ.' ') NSEQP = ISEQ
         CALL DATARD(ANT,MATP,MFP,MTP,NSEQP)
!
!        TEST CONTENTS OF RECORD ID FIELDS
!
   50    CALL TESTL
         IF (N.EQ.1)  THEN
            DO I=1,4
               BIN1(I) = ANT(I)
            END DO
         END IF
         IF(N.LE.19)  THEN
            NN = (N-1)/6 + 1
            BIN(NN) = ANT(1)
         END IF
      END DO
      GO TO 100
!
!     UNEXPECTED END OF FILE
!
   90 IERX = 2
!
  100 RETURN
      END SUBROUTINE RDLIST
!
!***********************************************************************
!
      SUBROUTINE RDHEAD(I)
!
!     PROCESS A HEAD RECORD
!
!     SUBROUTINE RETURNS I WITH THE FOLLOWING MEANINGS
!          I=1, HEAD RECORD
!            2, SEND RECORD
!            3, FEND RECORD
!            4, MEND RECORD
!            5, TEND RECORD
!
      IMPLICIT NONE
!
      INTEGER(KIND=I4)  :: I
!
      INTEGER(KIND=I4)  :: NSEQT
!
!     READ IN RECORD
!
      READ(JIN,'(A)',END=80) IFIELD
      ISEQ = ISEQ + 1
      NSEQP = 0
      READ(IFIELD,'(2E11.4,4I11,I4,I2,I3,I5)',ERR=30)                   &       
     &          C1H,C2H,L1H,L2H,N1H,N2H,MAT,MF,MT,NSEQP
      IF(ASEQ.EQ.' ') NSEQP = ISEQ
      GO TO 40
!
!     ERROR SO TRY TO READ EACH FIELD SEPARATELY
!
   30 IF(ASEQ.EQ.' ') NSEQP = ISEQ
      CALL HEADRD(C1H,C2H,L1H,L2H,N1H,N2H,MAT,MF,MT,NSEQP)
!
!     CHECK SEQUENCE NUMBER
!
   40 NSEQP1 = NSEQP
      IF(NFOR.LT.6) THEN
         IF(MAT.LT.0) THEN
            NSEQT = 0
         ELSE
            NSEQT = NSEQ + 1
         END IF
      ELSE
         IF(MAT.LT.0) THEN
            NSEQT = 0
         ELSE IF(MF.EQ.0) THEN
            NSEQT = 0
         ELSE IF(MT.EQ.0) THEN
            NSEQT = 99999
         ELSE
            NSEQT = NSEQ + 1
         END IF
      END IF
      IF(NSEQP.NE.NSEQT.AND.ASEQ.NE.' ')  THEN
         EMESS = 'OUT OF SEQUENCE AT'
         CALL ERROR_MESSAGE(NSEQP)
      END IF
      IF(MT.EQ.0.AND.NFOR.GT.5) THEN
         NSEQ = 0
      ELSE
         NSEQ = NSEQP
      END IF
!
!     NEITHER MF NOR MT MAY EVER BE NEGATIVE
!
      IF(MF.LT.0.OR.MT.LT.0)   GO TO 90
!
!     NEGATIVE MAT NUMBER, SO CHECK FOR TEND RECORD
!
      IF(MAT.LT.0)   THEN
         IF(MF.NE.0.AND.MT.NE.0)   GO TO 90
         I = 5
         GO TO 100
      END IF
!
!     A HEAD RECORD
!
      IF(MT.NE.0) THEN
         IF(MF.EQ.0.OR.MAT.EQ.0)   GO TO 90
         I = 1
!
!     A SEND RECORD
!
      ELSE IF(MF.NE.0) THEN
         IF(MAT.EQ.0)  GO TO 90
         I = 2
!
!     A FEND RECORD
!
      ELSE IF(MAT.NE.0)  THEN
         I = 3
!
!     A MEND RECORD
!
      ELSE
         I = 4
      END IF
      GO TO 100
!
!     UNEXPECTED END OF FILE
!
   80 IERX = 2
      GO TO 100
!
!     BAD CONTROL CARD
!
   90 EMESS = 'IMPROPER CONTROL CARD AT'
      CALL ERROR_MESSAGE(NSEQP)
      IERX = 1
!
  100 RETURN
      END SUBROUTINE RDHEAD
!
!***********************************************************************
!
      SUBROUTINE HEADRD(C1T,C2T,L1T,L2T,N1T,N2T,MATT,MFT,MTT,NSEQT)
!
!     SUBROUTINE TO DECODE AN INPUT HEAD OR CONT RECORD WITH A FORMAT
!       ERROR
!
      IMPLICIT NONE
!
      INTEGER(KIND=I4) :: L1T,L2T,N1T,N2T
      INTEGER(KIND=I4) :: MATT,MFT,MTT
      INTEGER(KIND=I4) :: NSEQT
!
      CHARACTER(LEN=11) :: RFIELD
      INTEGER(KIND=I4) :: K
      REAL(KIND=R4) :: C1T,C2T
!
      INTEGER(KIND=I4), DIMENSION(4) :: L
      REAL(KIND=R4), DIMENSION(2) :: C
!
!     PROCESS FLOATING POINT FIELDS
!
      DO K=1,2
         C(K) = 0.
         RFIELD = IFIELD(IBR(K):IBR(K+1)-1)
         READ(RFIELD,'(E11.4)',ERR=10) C(K)
   10    CONTINUE
      END DO
      C1T = C(1)
      C2T = C(2)
!
!     PROCESS INTEGER FIELDS
!
      DO K=3,6
         L(K-2) = 0
         RFIELD = IFIELD(IBR(K):IBR(K+1)-1)
         READ(RFIELD,'(I11)',ERR=20)   L(K-2)
   20    CONTINUE
      END DO
      L1T = L(1)
      L2T = L(2)
      N1T = L(3)
      N2T = L(4)
!
!     PROCESS ID FIELDS
!
      CALL SEQRD(2,MATT,MFT,MTT,NSEQT)
!
      RETURN
      END SUBROUTINE HEADRD
!
!***********************************************************************
!
      SUBROUTINE INTRD(NBTT,INTT,MATT,MFT,MTT,NSEQT)
!
!     SUBROUTINE TO DECODE AN INPUT TAB2 RECORD WITH A FORMAT ERROR
!
      IMPLICIT NONE
!
      INTEGER(KIND=I4) :: MATT,MFT,MTT
      INTEGER(KIND=I4) :: NSEQT
      INTEGER(KIND=I4), DIMENSION(3) :: NBTT,INTT
!
      CHARACTER(LEN=11) :: RFIELD
      INTEGER(KIND=I4) :: KK,K
!
      INTEGER(KIND=I4), DIMENSION(6) :: L
!
!     PROCESS INTEGER FIELDS
!
      DO K=1,6
         L(K) = 0
         RFIELD = IFIELD(IBR(K):IBR(K+1)-1)
         READ(RFIELD,'(I11)',ERR=10)   L(K)
   10    CONTINUE
      END DO
      KK = 1
      DO K=1,3
         NBTT(K) = L(KK)
         INTT(K) = L(KK+1)
         KK = KK + 2
      END DO
!
!     PROCESS ID FIELDS
!
      CALL SEQRD(3,MATT,MFT,MTT,NSEQT)
!
      RETURN
      END SUBROUTINE INTRD
!
!***********************************************************************
!
      SUBROUTINE DATARD(ANT,MATT,MFT,MTT,NSEQT)
!
!     SUBROUTINE TO DECODE AN INPUT DATA RECORD WITH A FORMAT ERROR
!
      IMPLICIT NONE
!
      INTEGER(KIND=I4) :: MATT,MFT,MTT
      INTEGER(KIND=I4) :: NSEQT
      REAL(KIND=R4), DIMENSION(6) :: ANT
!
      CHARACTER(LEN=11) :: RFIELD
      INTEGER(KIND=I4) :: K
!
!     PROCESS FLOATING POINT FIELDS
!
      DO K=1,6
         ANT(K) = 0.
         RFIELD = IFIELD(IBR(K):IBR(K+1)-1)
         READ(RFIELD,'(E11.4)',ERR=10) ANT(K)
   10    CONTINUE
      END DO
!
!     PROCESS ID FIELDS
!
      CALL SEQRD(4,MATT,MFT,MTT,NSEQT)
!
      RETURN
      END SUBROUTINE DATARD
!
!***********************************************************************
!
      SUBROUTINE SEQRD(IFMT,MATR,MFR,MTR,NSEQR)
!
!     SUBROUTINE TO DECODE THE ID PORTION OF A RECORD WITH A FORMAT
!       ERROR AND OUTPUT ERROR MESSAGE
!
      IMPLICIT NONE
!
      INTEGER(KIND=I4) :: MATR,MFR,MTR
      INTEGER(KIND=I4) :: NSEQR
      INTEGER(KIND=I4) :: IFMT
!
      CHARACTER(LEN=5) :: RFIELD
      CHARACTER(LEN=25) :: FMT
      INTEGER(KIND=I4) :: NB
      INTEGER(KIND=I4) :: K
!
      INTEGER(KIND=I4), DIMENSION(4) :: L
!
!     PROCESS INTEGER FIELDS
!
      DO K=1,4
         L(K) = 0
         RFIELD = ' '
         NB = 6 + IBR1(K) - IBR1(K+1)
         RFIELD(NB:) = IFIELD(IBR1(K):IBR1(K+1)-1)
         READ(RFIELD,'(I5)',ERR=10)   L(K)
   10    CONTINUE
      END DO
      MATR = L(1)
      MFR = L(2)
      MTR = L(3)
      NSEQR = L(4)
!
!     ERROR MESSAGE FOR INCORRECT CARD FORMAT
!
      SELECT CASE (IFMT)
         CASE (1)
            FMT = '(A66,I4,I2,I3,I5)'
         CASE (2)
            FMT = '(2E11.4,4I11,I4,I2,I3,I5)'
         CASE (3)
            FMT = '(6I11,I4,I2,I3,I5)'
         CASE (4)
            FMT = '(6E11.4,I4,I2,I3,I5)'
      END SELECT
      EMESS = 'FORMAT ERROR IN CARD NO. '//IFIELD(76:80)
      CALL ERROR_MESSAGE(0)
      EMESS = '    EXPECT FORMAT '//FMT
      CALL ERROR_MESSAGE(0)
      EMESS = '    READ   '//IFIELD
      CALL ERROR_MESSAGE(0)
!
      RETURN
      END SUBROUTINE SEQRD
!
!***********************************************************************
!
      SUBROUTINE TESTIN(NBTT,JNTT,NRT,NPT,IBMOD)
!
!     ROUTINE TO TEST INTERPOLATION TABLE
!
      IMPLICIT NONE
!
      INTEGER(KIND=I4) :: NRT,NPT,IBMOD
      INTEGER(KIND=I4) :: NBTT(NIRMAX),JNTT(NIRMAX)
!
      INTEGER(KIND=I4), INTRINSIC :: MOD
!
      INTEGER(KIND=I4) :: JJNT,KNT
      INTEGER(KIND=I4) :: JNTMIN,JNTMAX
      INTEGER(KIND=I4) :: M
!
!     THERE MUST BE AT LEAST ONE INTERPOLATION REGION
!
      IF(NRT.LT.1)   GO TO 90
!
!     NUMBER OF REGIONS CANNOT EXCEED NUMBER OF POINTS
!
      IF(NPT.LT.NRT) GO TO 90
!
!     REGION BOUNDRY POINTS MUST BE IN INCREASING ORDER
!
      IF(NRT.GT.1)  THEN
         DO M=2,NRT
            IF(NBTT(M).LE.NBTT(M-1))  GO TO 90
         END DO
      END IF
!
!     LAST BOUNDRY MUST BE AT LAST POINT
!
      IF(NBTT(NRT).NE.NPT) GO TO 90
!
!     FIRST BOUNDRY CANNOT BE BELOW FIRST POINT
!
      IF(NBTT(1).LT.1)   GO TO 90
      JNTMIN = 1
      JNTMAX = INTMAX
      IF(NFOR.GE.6.AND.NSUB/10.GE.1001) THEN
         IF(MF.EQ.3.OR.MF.EQ.6)   JNTMAX = 6
      END IF
!
!     ALL INTERPOLATION LAWS MUST BE VALID
!
      DO M=1,NRT
         KNT = JNTT(M)/10
         IF(KNT.GT.IBMOD)    GO TO 90
         JJNT = MOD(JNTT(M),10)
         IF(JJNT.LT.JNTMIN.OR.JJNT.GT.JNTMAX)   GO TO 90
      END DO
      GO TO 100
!
!     ERROR MESSAGE
!
   90 EMESS = 'INTERPOLATION TABLE INCORRECT'
      CALL ERROR_MESSAGE(NSEQP1)
!
  100 RETURN
      END SUBROUTINE TESTIN
!
!***********************************************************************
!
      SUBROUTINE TESTL
!
!     ROUTINE TO TEST CARD LABEL
!
      IMPLICIT NONE
!
!     MAT NUMBER CANNOT CHANGE
!
      IF(MATP.NE.MAT)  THEN
         EMESS = 'MAT INCORRECT'
         CALL ERROR_MESSAGE(NSEQP)
      END IF
!
!     MF NUMBER CANNOT CHANGE
!
      IF(MFP.NE.MF)  THEN
         EMESS = 'MF INCORRECT'
         CALL ERROR_MESSAGE(NSEQP)
      END IF
!
!     MT NUMBER CANNOT CHANGE
!
      IF(MTP.NE.MT)  THEN
         EMESS = 'MT INCORRECT'
         CALL ERROR_MESSAGE(NSEQP)
      END IF
!
!     CHECK SEQUENCE NUMBER
!
      IF(NSEQP.NE.NSEQ+1.AND.ASEQ.NE.' ')    THEN
         EMESS = 'OUT OF SEQUENCE AT'
         CALL ERROR_MESSAGE(NSEQP)
      END IF
      NSEQ = NSEQP
!
      RETURN
      END SUBROUTINE TESTL
!
!***********************************************************************
!
      SUBROUTINE TEST1(N,NA,NB,KXXX,IERR)
!
!     ROUTINE TO TEST RANGE OF AN INTEGER
!
      IMPLICIT NONE
!
      CHARACTER(LEN=*) :: KXXX
      INTEGER(KIND=I4) :: N,NA,NB
      INTEGER(KIND=I4) :: IERR
!
!     IF N IS GE NA, AND LE NB, OK
!
      IF(N.LT.NA.OR.N.GT.NB) THEN
         WRITE(EMESS,'(2A,I6,A,I6,A,I6)')                               &       
     &               KXXX,' =',N,' OUT OF RANGE',NA,' -',NB
         CALL ERROR_MESSAGE(NSEQP1)
         IF(IERR.GT.1)   IERX = 1
      END IF
!
      RETURN
      END SUBROUTINE TEST1
!
!***********************************************************************
!
      SUBROUTINE TEST2(N1,N2,KXXX)
!
!     ROUTINE TO TEST THAT N1=N2
!
      IMPLICIT NONE
!
      CHARACTER(LEN=*) :: KXXX
      INTEGER(KIND=I4) :: N1,N2
!
      IF(N1.NE.N2)  THEN
         WRITE(EMESS,'(2A,I4)') KXXX,' SHOULD BE SET TO ',N2
         CALL ERROR_MESSAGE(NSEQP1)
      END IF
!
      RETURN
      END SUBROUTINE TEST2
!
!***********************************************************************
!
      SUBROUTINE TEST1F(F,FMIN,FMAX,KXXX)
!
!     ROUTINE TO TEST RANGE OF AN FLOATING POINT NUMBER
!
      IMPLICIT NONE
!
      CHARACTER(LEN=*) :: KXXX
      REAL(KIND=R4) :: F,FMIN,FMAX
!
!     IF F IS GE FMIN, AND LE FMAX, OK
!
      IF(F.LT.FMIN.OR.F.GT.FMAX) THEN
         WRITE(EMESS,'(2A,1PE9.2,A,1PE9.2,A,1PE9.2)')                   &       
     &            KXXX,' =',F,' OUT OF RANGE',FMIN,' -',FMAX
         CALL ERROR_MESSAGE(NSEQP1)
      END IF
      IERX = 0
!
  100 RETURN
      END SUBROUTINE TEST1F
!
!***********************************************************************
!
      SUBROUTINE TEST2F(A,B,KXXX)
!
!     ROUTINE TO TEST FOR EQUALITY OF FLOATING POINT NUMBERS, A = B
!
      IMPLICIT NONE
!
      REAL(KIND=R4) :: A,B
      CHARACTER(LEN=*) :: KXXX
!
      REAL(KIND=R4), INTRINSIC :: ABS
!
      REAL(KIND=R4) :: RTEST
!
      RTEST = ABS(A)
      IF(B.NE.0.)  RTEST = ABS((B-A)/B)
      IF(RTEST.GT.EPS)  THEN
         WRITE(EMESS,'(2A,1PE13.6)')                                    &       
     &                KXXX,' SHOULD BE SET TO ',B
         CALL ERROR_MESSAGE(NSEQP)
      END IF
!
      RETURN
      END SUBROUTINE TEST2F
!
!***********************************************************************
!
      SUBROUTINE TESTFT(MTT0,MFT0)
!
!     SUBROUTINE TO CHECK FOR A VALID MT AS DEFINED BY APPENDIX B OF
!      ENDF-102. ALSO CHECKS TO SEE IF MT VALID FOR THE FILE (MF).
!
      IMPLICIT NONE
!
      INTEGER(KIND=I4) :: MTT0,MFT0
!
      INTEGER(KIND=I4) :: MTT,MFT
      INTEGER(KIND=I4) :: MTCAT
      INTEGER(KIND=I4) :: MT3
      INTEGER(KIND=I4) :: IDX,IPAR
      INTEGER(KIND=I4) :: IPART
      INTEGER(KIND=I4) :: IEVAL
      INTEGER(KIND=I4) :: N
!
      INTEGER(KIND=I4), PARAMETER:: NPARTS = 7
      INTEGER(KIND=I4), DIMENSION(NPARTS), PARAMETER ::                  &      
     &            IPZA = (/0,1,1001,1002,1003,2003,2004/)
      INTEGER(KIND=I4), DIMENSION(NPARTS), PARAMETER ::                  &      
     &            IELAS = (/0,50,600,650,700,750,800/)
!
!     INITIALIZE
!
      MTCAT = 0
      MTT = MTT0
      MFT = MFT0
      IERX = 0
!
!     CONVERT MT IF INPUT FILE IS IN ENDF-V FORMAT
!
      IF(NFOR.LT.6)   THEN
         IF(MTT.GE.600.AND.MTT.LE.699)  THEN
            IF(MTT.EQ.602) THEN
               MTT = 522
            ELSE
               GO TO 10
            END IF
         ELSE IF(MTT.GE.700.AND.MTT.LE.799)   THEN
            IDX = MTT - 700
            IPAR = IDX/20
            IDX = IDX - 20*IPAR
            IF(IDX.EQ.19)  THEN
               IDX = 48
            ELSE IF(IDX.EQ.18)   THEN
               IDX = 49
            END IF
            MTT = 600 + 50*IPAR + IDX
         ELSE IF(MTT.GE.800.AND.MTT.LE.850)   THEN
            GO TO 10
         END IF
      END IF
!
!     DETERMINE THE CATEGORY OF THE MT NUMBER FOR VALIDATION
!
   10 IF(MTT.GT.0.AND.MTT.LT.999)    THEN
         CALL GET_MTCAT(MTT,MT3,MTCAT)
      END IF
!
!     A VALID MT WILL HAVE AN MTCAT NOT EQUAL ZERO
!
      IF(MTCAT.EQ.0)   THEN
         WRITE(EMESS,'(A,I4,A)') 'MT=',MTT0,' INVALID'
         CALL ERROR_MESSAGE(NSEQP1)
         GO TO 100
      END IF
!
!     CHECK FOR PROPER ASSIGNMENT OF ELASTIC MT
!
      IF(MTCAT.EQ.4)    THEN
         IPART = NSUB/10
         DO N=1,NPARTS
            IF(IPART.EQ.IPZA(N))   THEN
               IF(MTT.EQ.IELAS(N))    THEN
                  WRITE(EMESS,'(A,I4,A)')                               &       
     &              'MT=',MTT0,' SHOULD BE USED FOR ELASTIC SCATTERING'
                  CALL ERROR_MESSAGE(NSEQP1)
               END IF
               GO TO 30
            END IF
         END DO
      END IF
!
!     CHECK FOR MT'S ALLOWED ONLY IN DERIVED FILES
!
   30 IF(NFOR.GE.6.AND.LDRV.EQ.0)   THEN
         IF(MTT.EQ.10.OR.MTT.EQ.27.OR.MTT.EQ.101.OR.                    &       
     &                  (MTT.GE.201.AND.MTT.LE.450))  THEN
            WRITE(EMESS,'(A,I4,A)')                                     &       
     &              'MT=',MTT0,' ALLOWED ONLY IN DERIVED FILES'
            CALL WARNING_MESSAGE(NSEQP1)
         END IF
      END IF
!
!     VALIDATE MT FOR CURRENT MF
!
      CALL VALID_MFMT(MTT,MT3,MFT,MTCAT,IEVAL)
!
!     MT IS VALID FOR FILE MF BUT NOT FOR NSUB
!
      IF(IEVAL.EQ.1) THEN
         WRITE(EMESS,'(A,I4,A,I6,A)')                                   &       
     &           'MT=',MTT0,' FOR NSUB=',NSUB,' INVALID'
         CALL ERROR_MESSAGE(NSEQP1)
      ELSE IF(IEVAL.EQ.2) THEN
!
!     MT IS VALID BUT NOT FOR FILE MF
!
         WRITE(EMESS,'(A,I4,A,I3,A)')                                   &       
     &           'MT=',MTT0,' FOR MF=',MFT0,' INVALID'
         CALL ERROR_MESSAGE(NSEQP1)
      END IF
!
  100 RETURN
      END SUBROUTINE TESTFT
!
!***********************************************************************
!
      SUBROUTINE GET_MTCAT(MTT,MT3,MTCAT)
!
!     EACH VALID MT IS ASSIGNED TO A CATEGORY (MTCAT)
!
      IMPLICIT NONE
!
      INTEGER(KIND=I4) :: MTT,MT3,MTCAT
!
      INTEGER(KIND=I4), INTRINSIC :: MOD
!
      INTEGER(KIND=I4) :: MT7
!
      MTCAT = 0
      MT3 = MTT
      IF(MTT.LE.150)     GO TO 50
!
!     RESONANCE PARAMETERS  151 - 200
!
      IF(MTT.GE.151.AND.MTT.LE.200) THEN
         IF(MTT.EQ.151)   MTCAT = 2
!
!     PARTICLE PRODUCTION REACTIONS, MUBAR XI AND GAMMA  201 - 300
!
      ELSE IF(MTT.GE.201.AND.MTT.LE.300) THEN
         IF(MTT.LT.218) THEN
            IF(NFOR.GE.6.OR.(MTT.GE.203.AND.MTT.LE.207))  MTCAT = 5
         ELSE
            IF(MTT.GE.251.AND.MTT.LE.253) MTCAT = 3
         END IF
!
!     REACTION IN ENERGY DEPOSIT REGION  301 - 450
!
      ELSE IF(MTT.GT.301.AND.MTT.LE.450) THEN
!********GET CORRESPONDING REACTION MT
         IF(MTT.LE.420)  THEN
            MT3 = MTT - 300
            GO TO 50
         END IF
!
!     MT = 451 - 500
!
      ELSE IF(MTT.GE.451.AND.MTT.LE.500) THEN
         IF(MTT.EQ.451.OR.MTT.EQ.458) THEN
            MTCAT = 1
         ELSE IF(MTT.EQ.454.OR.MTT.EQ.457.OR.MTT.EQ.459) THEN
            MTCAT = 8
         ELSE IF(MTT.EQ.452.OR.MTT.EQ.456) THEN
            MTCAT = 11
         ELSE IF(MTT.EQ.455) THEN
            MTCAT = 12
         ELSE IF(MTT.EQ.500)  THEN
            MTCAT = 3
         END IF
!
!     PHOTON AND ELECTRON INTERACTION   501 - 599
!
      ELSE IF(MTT.GE.501.AND.MTT.LE.599)   THEN
         IF(MTT.EQ.501.OR.MTT.EQ.502.OR.MTT.EQ.504)   THEN
            MTCAT = 9
         ELSE IF(MTT.EQ.505.OR.MTT.EQ.506)   THEN
            MTCAT = 13
         ELSE IF((MTT.GE.515.AND.MTT.LE.517).OR.MTT.EQ.522)   THEN
            MTCAT = 9
         ELSE IF(MTT.EQ.523.OR.(MTT.GE.526.AND.MTT.LE.528)) THEN
            MTCAT = 15
         ELSE IF(MTT.EQ.533) THEN
            MTCAT = 16
         ELSE IF(MTT.GE.534.AND.MTT.LE.599) THEN
            MTCAT = 14
         END IF
!
!     SINGLE CHARGED PARTICLE EMISSION  600 - 849
!
      ELSE IF(MTT.GE.600.AND.MTT.LE.849)   THEN
         MT7 = MOD(MTT-600,50)
         IF(MT7.EQ.0)  THEN
            MTCAT = 4
         ELSE IF((MT7.EQ.48.AND.NFOR.LT.6).OR.MT7.EQ.49) THEN
            MTCAT = 5
         ELSE
            MTCAT = 6
         END IF
!
!     LUMPED MT'S  850 - 999
!
      ELSE IF(MTT.GE.850.AND.MTT.LE.999) THEN
         IF(MTT.GE.851.AND.MTT.LE.870)   THEN
            MTCAT = 10
         END IF
      END IF
      GO TO 100
!
!     MT'S FROM 1 TO 15
!
   50 IF(MT3.LE.11) THEN
         IF(MT3.EQ.1) THEN
            IF(NSUB/10.NE.1)  GO TO 100
            MTCAT = 5
         ELSE IF(MT3.EQ.2) THEN
            MTCAT = 4
         ELSE IF(MT3.EQ.3.OR.MT3.EQ.11) THEN
            MTCAT = 5
         ELSE IF(MT3.EQ.4) THEN
            MTCAT = 7
         ELSE IF(MT3.EQ.5.OR.MT3.EQ.10) THEN
            IF(NFOR.LT.6)   THEN
               WRITE(EMESS,'(A,I2,A)')                                  &       
     &            'MT=',MTT,' NOT ALLOWED PRIOR TO ENDF-6'
               CALL ERROR_MESSAGE(0)
            END IF
            MTCAT = 5
         ELSE IF(MT3.GE.6.AND.MT3.LE.9) THEN
            IF(NFOR.GE.6) GO TO 100
            MTCAT = 4
         ELSE
            GO TO 100
         END IF
!
!     MT'S FROM 16 TO 49
!
      ELSE IF(MT3.GE.16.AND.MT3.LE.49) THEN
         IF(MT3.GE.16.AND.MT3.LE.30)  THEN
            IF(NSUB/10.NE.1.AND.(MT3.EQ.20.OR.MT3.EQ.21))  GO TO 100
            MTCAT = 5
         ELSE IF(MT3.GE.32.AND.MT3.LE.38)  THEN
            IF(NSUB/10.NE.1.AND.MT3.EQ.38)  GO TO 100
            MTCAT = 5
         ELSE IF(MT3.GE.41.AND.MT3.LE.45)  THEN
            IF(NFOR.LT.6) GO TO 100
            MTCAT = 5
         ELSE IF(MT3.GE.46.AND.MT3.LE.49)  THEN
            IF(NFOR.GE.6) GO TO 100
            MTCAT = 4
         ELSE
            GO TO 100
         END IF
!
!     SINGLE NEUTRON EMISSION  50 - 99
!
      ELSE IF(MT3.GE.50.AND.MT3.LE.99) THEN
         IF(MT3.EQ.50)   THEN
            IF(NFOR.LT.6.OR.NSUB/10.EQ.1)   GO TO 100
            MTCAT = 4
         ELSE IF(MT3.GE.51.AND.MT3.LE.90)   THEN
            MTCAT = 6
         ELSE IF(MT3.EQ.91)   THEN
            MTCAT = 5
         ELSE
            GO TO 100
         END IF
!
!     MT'S BETWEEN 100 AND 150
!
      ELSE IF(MT3.GE.100.AND.MT3.LE.150)   THEN
         IF(MT3.GE.101.AND.MT3.LE.109)   THEN
            MTCAT= 5
         ELSE IF(MT3.GE.111.AND.MT3.LE.117)   THEN
            MTCAT= 5
         ELSE IF(MT3.EQ.120)   THEN
            IF(NFOR.GE.6)  GO TO 100
            MTCAT = 5
         ELSE
            GO TO 100
         END IF
      END IF
!
!     ALL VALID ENERGY DEPOSIT REACTIONS ARE MTCAT = 3
!
      IF(MT3.NE.MTT)  MTCAT = 3
!
  100 RETURN
      END SUBROUTINE GET_MTCAT
!
!***********************************************************************
!
      SUBROUTINE VALID_MFMT(MTT,MT3,MFT,MTCAT,IEVAL)
!
!     CHECK THAT THE VALID MT IS PERMITTED IN FILE MF
!
      IMPLICIT NONE
!
      INTEGER(KIND=I4) :: MTT,MT3,MFT
      INTEGER(KIND=I4) :: MTCAT
      INTEGER(KIND=I4) :: IEVAL
!
      INTEGER(KIND=I4), INTRINSIC :: MOD
!
      INTEGER(KIND=I4) :: ISUBTP
!
      IEVAL = 0
      SELECT CASE (MTCAT)
!*******MTCAT=1;   VALID ONLY IN FILE 1
         CASE (1)
            IF(MFT.EQ.1)   THEN
               IF(NFOR.GE.6)   THEN
                  IF(MTT.NE.451.AND.NSUB.NE.10)   IEVAL = 1
               END IF
            ELSE
               IEVAL = 2
            END IF
!********MTCAT=2;   RESONANCE PARAMETERS
         CASE (2)
            IF(MFT.NE.2.AND.MFT.NE.32)   IEVAL = 2
!********MTCAT=3;   VALID ONLY IN FILE 3
         CASE (3)
            IF(MFT.EQ.3)  THEN
               IF(MTT.EQ.500.AND.NSUB.LT.10010) IEVAL = 1
            ELSE
               IEVAL = 2
            END IF
!********MTCAT=4;   DISCRETE PARTICLE SCATTERING NO GAMMA PRODUCTION
         CASE (4)
            IF(MFT.GE.3.AND.MFT.LE.6)   GO TO 100
            IF(MFT.GE.8.AND.MFT.LE.10)   GO TO 100
            IF(NFOR.GE.6.AND.(MFT.EQ.7.AND.MTT.EQ.2))   GO TO 100
            IF(MFT.GE.33.AND.MFT.LE.40)   GO TO 100
            IEVAL = 2
!********MTCAT=5;   CONTINUUM PARTICLE SCATTERING WITH GAMMA PRODUCTION
         CASE (5)
            IF(MFT.LE.2)  IEVAL = 2
            IF(MFT.EQ.3)   GO TO 100
            IF(MFT.GT.5)   THEN
               IF(MFT.EQ.6)   GO TO 100
               IF(MFT.GE.8.AND.MFT.LE.10)   GO TO 100
               IF(MFT.GE.13.AND.MFT.LE.15)  GO TO 100
               IF(MFT.EQ.12.AND.L1H.EQ.1)   GO TO 100
               IF(NSUB.EQ.4.AND.(MFT.NE.35.OR.MT3.NE.18))  IEVAL = 2
               IF(MFT.GE.33.AND.MFT.LE.40)   GO TO 100
               IEVAL = 2
            END IF
            IF(MT3.EQ.101.OR.MT3.EQ.102)  IEVAL = 2
            IF(MT3.GE.111.AND.MT3.LE.121) IEVAL = 2
            IF(NSUB.EQ.4.AND.(MFT.NE.5.OR.MT3.NE.18))  IEVAL = 2
!********MTCAT=6;   DISCRETE PARTICLE SCATTERING WITH GAMMA PRODUCTION
         CASE (6)
            IF(MFT.GE.3.AND.MFT.LE.6)   GO TO 100
            IF(MFT.GE.8.AND.MFT.LE.10)   GO TO 100
            IF(MFT.GE.12.AND.MFT.LE.15)  GO TO 100
            IF(MFT.GE.33.AND.MFT.LE.40)   GO TO 100
            IEVAL = 2
!********MTCAT=7;   NEUTRON INELASTIC SCATTERING
         CASE (7)
            IF(MFT.GE.3.AND.MFT.LE.10)   GO TO 100
            IF(MFT.GE.13.AND.MFT.LE.15)  GO TO 100
            IF(MFT.EQ.12.AND.L1H.EQ.1)   GO TO 100
            IF(MFT.GE.33.AND.MFT.LE.40)   GO TO 100
            IEVAL = 2
!********MTCAT=8;   VALID IN FILE 8 ONLY
         CASE (8)
            IF(MFT.EQ.8) THEN
               IF(NFOR.GE.6)   THEN
                  ISUBTP = MOD(NSUB,10)
                  IF(MTT.EQ.457.AND.NSUB.NE.4)  IEVAL = 1
                  IF((MTT.EQ.454.OR.MTT.EQ.459).AND.                    &       
     &                (ISUBTP.NE.1.AND.NSUB.NE.5)) IEVAL = 1
               END IF
            ELSE
               IEVAL = 2
            END IF
!********MTCAT=9;   PHOTON INTERACTION DATA
         CASE (9)
            IF(MFT.EQ.23.OR.MFT.EQ.27) THEN
               IF(NSUB.NE.3) IEVAL = 1
            ELSE
               IEVAL = 2
            END IF
!********MTCAT=10;  LUMPED COVARIANCES
         CASE (10)
            IF(MFT.NE.31.AND.MFT.NE.33)   IEVAL = 2
!********MTCAT=11;  TOTAL AND PROMPT FISSION NEUTRON PRODUCTION
         CASE (11)
            IF(MFT.EQ.1.OR.MFT.EQ.31)   THEN
               IF(NFOR.GE.6.AND.(NSUB.NE.10.AND.NSUB.NE.4))  IEVAL = 1
            ELSE
               IEVAL = 2
           END IF
!********MTCAT=12;  DELAYED FISSION NEUTRON PRODUCTION
         CASE (12)
            IF(MFT.EQ.1.OR.MFT.EQ.5.OR.MFT.EQ.31) THEN
               IF(NFOR.GE.6.AND.(NSUB.NE.10.AND.NSUB.NE.4))  IEVAL = 1
            ELSE
               IEVAL = 2
            END IF
!********MTCAT=13; ANOMALOUS SCATTERING FACTORS
         CASE (13)
            IF(MFT.NE.27.OR.NFOR.LT.6)  IEVAL = 2
!********MTCAT=14; PHOTOELECTRIC OR ELECTRO-ATOMIC SUBSHELL CROSS
!***********SECTIONS
         CASE (14)
            IF((MFT.NE.23.AND.MFT.NE.26).OR.NFOR.LT.6)  IEVAL = 2
!********MTCAT=15;   ELECTROATOMIC INTERACTION DATA
         CASE (15)
            IF(MFT.EQ.23.OR.MFT.EQ.26) THEN
               IF(NSUB.NE.113) IEVAL = 1
            ELSE
               IEVAL = 2
            END IF
!********MTCAT=16;   ATOMIC RELAXATION DATA
         CASE (16)
            IF(MFT.NE.28)   IEVAL = 2
      END SELECT
!
  100 RETURN
      END SUBROUTINE VALID_MFMT
!
!***********************************************************************
!
      SUBROUTINE TESTD(MF1,MT1)
!
!     ROUTINE TO TEST WHETHER MF1,MT1 IS IN THE DIRECTORY. IF NOT, IT
!       IS ADDED TO THE INDEX AND PROPERLY FLAGGED
!
      IMPLICIT NONE
!
      INTEGER(KIND=I4) :: MF1,MT1
!
      INTEGER(KIND=I4) :: INDXT
      INTEGER(KIND=I4) :: IU
      INTEGER(KIND=I4) :: I,J,K,N
!
!     DETERMINE PACKED VALUE
!
      INDXT=1000*MF1+MT1
!
!     LOOK IT UP
!
      IF(NXC.GT.0) THEN
         DO N=1,NXC
            IF(INDX(N,1).GT.INDXT) GO TO 10
            IF(INDX(N,1).EQ.INDXT) GO TO 90
         END DO
      ELSE
         GO TO 90
      END IF
      N = NXC + 1
   10 IF(IERX.NE.0) GO TO 100
!
!     SECTION NOT IN THE DIRECTORY, SO ADD IT TO THE INDEX
!
      IF(NXC.GE.250) GO TO 100
      NXC = NXC + 1
      IF(N.NE.NXC) THEN
         IU = NXC - N
         DO I=1,IU
            K = NXC - I
            DO J=1,2
               INDX(K+1,J) = INDX(K,J)
            END DO
         END DO
      END IF
      INDX(N,1) = INDXT
      INDX(N,2) = 2
!
!     WRITE ERROR MESSAGE
!
      IF(NXC0.NE.0) THEN
         WRITE(EMESS,'(A,I3,A,I3,A)')                                   &       
     &             'SECTION',MF1,'/',MT1,' NOT IN DIRECTORY'
      END IF
      CALL ERROR_MESSAGE(0)
      GO TO 100
!
!     SECTION IN DIRECTORY SO FLAG IT AS FOUND
!
   90 INDX(N,2)=0
!
  100 RETURN
      END SUBROUTINE TESTD
!
!***********************************************************************
!
      SUBROUTINE TESTP(MF1,MT1)
!
!     ROUTINE TO CHECK FOR THE PRESENCE OF REQUIRED SECTION MT IN
!       FILE MF
!
      IMPLICIT NONE
!
      INTEGER(KIND=I4) :: MF1,MT1
!
      INTEGER(KIND=I4) :: MFTEM,MTTEM
      INTEGER(KIND=I4) :: ISET
!
      MFTEM = MF1
      MTTEM = MT1
      CALL TESTS(MFTEM,MTTEM,ISET)
      IF(ISET.EQ.1.OR.ISET.EQ.3) THEN
         WRITE(EMESS,'(A,I3,A,I3)')                                     &       
     &       'THIS SECTION REQUIRES THE PRESENCE OF SECTION',MFTEM,     &       
     &       '/',MTTEM
         CALL ERROR_MESSAGE(0)
      END IF
!
      RETURN
      END SUBROUTINE TESTP
!
!***********************************************************************
!
      SUBROUTINE TESTS(MFT,MTT,ISET)
!
!     CHECK MATERIAL INDEX TO SEE WHETHER OR NOT REACTION MT IS
!     IS PRESENT IN FILE MF.  NO CHANGES ARE MADE IN THE INDEX ARRAY.
!     THE STATUS IS INDICATED BY THE VARIABLE ISET AS FOLLOWS--
!       ISET=0, IF THE REACTION IS IN THE MATERIAL INDEX, AND HAS
!               BEEN LOCATED IN THE DATA FILE,
!           =1, IF THE REACTION IS IN THE INDEX, BUT HAS NOT YET BEEN
!               FOUND IN THE DATA,
!           =2, IF THE REACTION IS IN THE DATA, BUT NOT THE INDEX,
!           =3, IF THE REACTION IS IN NEITHER THE DATA NOR THE INDEX.
!
      IMPLICIT NONE
!
      INTEGER(KIND=I4) :: MFT,MTT,ISET
!
      INTEGER(KIND=I4) :: JNDX
      INTEGER(KIND=I4) :: N
!
      IF(NXC.EQ.0) GO TO 100
!
!     DETERMINE PACKED VALUE
!
      JNDX = 1000*MFT + MTT
!
!     SEARCH THE INDEX
!
      DO N=1,NXC
         IF(INDX(N,1).GT.JNDX) GO TO 20
         IF(INDX(N,1).EQ.JNDX) GO TO 30
      END DO
   20 ISET = 3
      GO TO 100
   30 ISET = INDX(N,2)
!
  100 RETURN
      END SUBROUTINE TESTS
!
!***********************************************************************
!
      SUBROUTINE WARNING_MESSAGE(JSEQ)
!
!     ROUTINE TO OUTPUT ERROR MESSAGE IN STANDARD FORM
!
      IMPLICIT NONE
!
      INTEGER(KIND=I4) :: JSEQ
!
      INTEGER(KIND=I4), INTRINSIC :: LEN_TRIM
!
      INTEGER(KIND=I4) :: NEMES
!
!     PUT OUT ERROR MESSAGE
!
      IF(MATO.NE.MATERR.OR.MFO.NE.MFERR.OR.MTO.NE.MTERR) THEN
         WRITE(NOUT,'(//A,I4,A,I2,A,I3 )') '  WARNING(S)       IN MAT=',&
     &          MATO,', MF=',MFO,', MT=',MTO
         MATERR = MATO
         MFERR = MFO
         MTERR = MTO
      END IF
      IF(JSEQ.NE.0) THEN
         IF(ASEQ.NE.' ') THEN
            WRITE(NOUT,'(5X,2A,I6)')  EMESS(1:49),'SEQUENCE NUMBER',JSEQ
         ELSE
            WRITE(NOUT,'(5X,2A,I6)')  EMESS(1:49),'RECORD NUMBER',ISEQ
         END IF
      ELSE
         IF(EMESS.EQ.' ') THEN
            NEMES = 1
         ELSE
            NEMES = LEN_TRIM(EMESS)
         END IF
         WRITE(NOUT,'(5X,A)')  EMESS(1:NEMES)
      END IF
!
      RETURN
      END SUBROUTINE WARNING_MESSAGE
!
!***********************************************************************
!
      SUBROUTINE ERROR_MESSAGE(JSEQ)
!
!     ROUTINE TO OUTPUT ERROR MESSAGE IN STANDARD FORM
!
      IMPLICIT NONE
!
      INTEGER(KIND=I4) :: JSEQ
!
      INTEGER(KIND=I4), INTRINSIC :: LEN_TRIM
!
      INTEGER(KIND=I4) :: NEMES
!
!     PUT OUT ERROR MESSAGE
!
      IF(MATO.NE.MATERR.OR.MFO.NE.MFERR.OR.MTO.NE.MTERR) THEN
         WRITE(NOUT,'(//A,I4,A,I2,A,I3 )') '  ERROR(S) FOUND IN MAT=',  &
     &          MATO,', MF=',MFO,', MT=',MTO
         MATERR = MATO
         MFERR = MFO
         MTERR = MTO
      END IF
      IF(JSEQ.NE.0) THEN
         IF(ASEQ.NE.' ') THEN
            WRITE(NOUT,'(5X,2A,I6)')  EMESS(1:49),'SEQUENCE NUMBER',JSEQ
         ELSE
            WRITE(NOUT,'(5X,2A,I6)')  EMESS(1:49),'RECORD NUMBER',ISEQ
         END IF
      ELSE
         IF(EMESS.EQ.' ') THEN
            NEMES = 1
         ELSE
            NEMES = LEN_TRIM(EMESS)
         END IF
         WRITE(NOUT,'(5X,A)')  EMESS(1:NEMES)
      END IF
!
      RETURN
      END SUBROUTINE ERROR_MESSAGE
!
!***********************************************************************
!
      SUBROUTINE TOKEN(INSTR,DELIM,ITOK,OUTSTR)
!
!     SUBROUTINE TO EXTRACT A STRING FROM A STRING WITH DELIMITERS
!
      IMPLICIT NONE
!
      CHARACTER(LEN=*) :: INSTR,OUTSTR,DELIM
      INTEGER(KIND=I4) :: ITOK
!
      INTEGER(KIND=I4), INTRINSIC :: LEN_TRIM, INDEX
!
      INTEGER(KIND=I4) :: ILEN,JLEN
      INTEGER(KIND=I4) :: ITOKP
      INTEGER(KIND=I4) :: IBEG
      INTEGER(KIND=I4) :: I
!
!     INITIALIZE
!
      OUTSTR = ' '
      ILEN = LEN_TRIM(INSTR)
      JLEN = LEN_TRIM(DELIM)
      IF(ITOK.EQ.0.OR.ILEN.EQ.0)   GO TO 100
      IF(JLEN.EQ.0)  THEN
         IF(ITOK.EQ.1)    OUTSTR = INSTR
         GO TO 100
      END IF
!
!     FIND ITOK-TH DELIMITER
!
      ITOKP = 1 - JLEN
      DO I=1,ITOK
         IBEG = ITOKP + JLEN
         IF(IBEG.LE.ILEN)  THEN
            ITOKP = INDEX(INSTR(IBEG:),DELIM) + IBEG - 1
            IF(ITOKP.LT.IBEG) ITOKP = ILEN + 1
         ELSE
            GO TO 100
         END IF
         IF(I.EQ.ITOK)  THEN
            IF(ITOKP.GT.IBEG) OUTSTR = INSTR(IBEG:ITOKP-1)
            GO TO 100
         END IF
      END DO
!
  100 RETURN
      END SUBROUTINE TOKEN
!
!***********************************************************************
!
      SUBROUTINE DATE(ADATE)
!
!     RETURNS DATE AS A CHARACTER STRING OF 11 CHARACTERS IN THE
!          FORM  DD-MMM-YYYY
!
      IMPLICIT NONE
!
      CHARACTER(LEN=*) :: ADATE
      CHARACTER(LEN=8) :: PDATE
      CHARACTER(LEN=4) :: YR
      CHARACTER(LEN=2) :: DD
      INTEGER(KIND=I4) :: MON
!
      CHARACTER(LEN=3), DIMENSION(12) :: MONTHS
      DATA MONTHS/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep',&       
     &            'Oct','Nov','Dec'/
!
!     GET THE DATE AND TIME AS A CHARACTER STRING
!
      CALL DATE_AND_TIME(PDATE)
      READ(PDATE,'(A4,I2,A2)') YR,MON,DD
      ADATE = DD//'-'//MONTHS(MON)//'-'//YR
      READ(YR,'(I4)') IYR
!
      RETURN
      END SUBROUTINE DATE
!
!***********************************************************************
!
      SUBROUTINE GET_FROM_CLINE
!
!     GET CONTENTS OF COMMAND LINE OR NAME OF BATCH INPUT FILE
!
      IMPLICIT NONE
!
!+++MDC+++
!...VMS
!/      INTEGER(KIND=2) :: ILENP2
!...ANS
!/      CHARACTER(LEN=100) :: CFILE
!---MDC---
!
      INPAR = ' '
      ILENP = 0
      NIN = INPUT0
!
!+++MDC+++
!...VMS
!/      CALL LIB$GET_FOREIGN(INPAR,,ILENP2)
!/      ILENP = ILENP2
!...UNX
!      CALL GETCL(INPAR)
!      ilenp = LEN_TRIM(INPAR)
!...DVF
      CALL GETARG(1,INPAR)
      ilenp = LEN_TRIM(INPAR)
!...ANS
!/      WRITE(IOUT,'(A)')                                               &       
!/     &    ' Control File Specification        - '
!/      READ(NIN,'(A)') CFILE
!/      NIN = 19
!/      OPEN(UNIT=NIN,FILE=CFILE,STATUS='OLD')
!---MDC---
!
      RETURN
      END SUBROUTINE GET_FROM_CLINE
!
!***********************************************************************
!
      SUBROUTINE OUT_STATUS
!
!     DISPLAYS THE IDENTIFICATION OF THE SECTION BEING PROCESSED
!
      IMPLICIT NONE
!
      IF(MAT.GT.0.AND.MF.GT.0.AND.MT.GT.0) THEN
!+++MDC+++
!...VMS, ANS, WIN, UNX, MOD
         WRITE(IOUT,'(5X,A,I5,A,I3,A,I4)')                              &       
     &         'PROCESSING MAT=',MAT,', MF=',MF,', MT=',MT
!...DVF, LWI
!/         IF(IRERUN.EQ.0) CALL ENDF_RUN_STATUS(MAT,MF,MT)
!---MDC---
      END IF
!
      RETURN
      END SUBROUTINE OUT_STATUS
!
!+++MDC+++
!...VMS, ANS, WIN, UNX
      END PROGRAM CHECKR
!...LWI, DVF, MOD
!/      END MODULE CHECKR
!---MDC---
 
