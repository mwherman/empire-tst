      PROGRAM SIXTAB
C-Title  : SIXTAB
C-Purpose: Convert ENDF file MF6 to Law 7 representation
C-Author : Andrej Trkov                        e-mail: A.Trkov@iaea.org
C-A        International Atomic Energy Agency     tel: +43 1 2600 21712
C-A        Nuclear Data Section                   fax: +43 1 2600 7
C-A        Wagramerstrasse 5
C-A        P.O.Box 100
C-A        A-1400 Vienna
C-A        AUSTRIA
C-Version: Original code March 2001
C-V  03/01 Copy Law-7 data unchanged (A.Trkov).
C-V  03/02 Use FINEND function for interpolation (A.Trkov).
C-V  03/12 - Fix discrete energy (isotropic) particles in MF6 (A.Trkov).
C-V        - Generalise CM-Lab conversion for all particles
C-V        - Convert MF6 Law 2 Legendre (LANG=2) to pointwise (LANG=12).
C-V  05/06 Open scratch file once in the main program and rewind
C-V        (problem with too many OPEN statements with Lahey compiler).
C-M
C-M  Manual for Program SIXTAB
C-M  =========================
C-M
C-M  The SIXTAB program belongs to a group of IAEA codes for ENDF
C-M  file verification that complement the ENDF Pre-Processing
C-M  codes (PrePro) by D.E.Cullen [1], which are designed to perform
C-M  various operations on the basic ENDF-formatted evaluated nuclear
C-M  data files. They provide basic tools to the user for simple,
C-M  machine-independent interpretation and visualisation of the
C-M  contents of the evaluated nuclear data files.
C-M
C-M  The double differential data in an ENDF file MF 6 can be
C-M  represented in various forms, including Legendre coefficients,
C-M  Kalback-Mann formalism, pointwise representation, CM or Lab
C-M  frame of reference, etc. This may make subsequent handling of
C-M  the files rather difficult. The purpose of SIXTAB is to convert
C-M  MF 6 data into pointwise representation in the Lab system
C-M  (Law 7 ENDF format option).
C-M
C-M  WARNINGS:
C-M  1. The resulting file is generally not a substitute for the
C-M     original file due to the assumed limitations built into
C-M     the code. The number of angles in general may be insufficient
C-M     to accurately represent the original data.
C-M  2. The full complexity of the ENDF file MF 6  representation is
C-M     not coded at present:
C-M      - Only Law 1 Legendre (LANG=1,2) or tabulated (LANG>10)
C-M        representations are allowed. Laws 0, 3 and 4 have the
C-M        distributions defined by kinematics and are not affected.
C-M        Data for Laws 2 are considered acceptable. Only the
C-M        conversion from Legendre representation (LANG=0) to
C-M        pointwise form (LANG=12) is performed.
C-M      - Conversion to Lab from CM co-ordinate system is done
C-M        only for neutrons, which is assumed to be the first
C-M        outgoing particle.
C-M      - It is assumed that neutron is the incident particle.
C-M
C-M  Instructions
C-M  ------------
C-M  Several forms of input are allowed:
C-M
C-M   A.  If the file "SIXTAB.INP" exists, the file is scanned for
C-M       input instructions. They may be given in the "old style"
C-M       or in the more flexible keyword-oriented style.
C-M
C-M   B.  If the file "SIXTAB.INP" does not exist, and the first record
C-M       on the default input does NOT begin with the string "$* ",
C-M       then the default input is assumed to be the source ENDF file
C-M       and the default output is the reformatted output ENDF file.
C-M       This allows the use of "pipes" on Unix. On PC-Dos it is
C-M       compiler-dependent.
C-M
C-M   C.  If the file "SIXTAB.INP" does not exist, and the first record
C-M       on the default input begins with the string "$* ", the
C-M       default input is read as a set of keyword-oriented input
C-M       instructions. This option is useful to divert the default
C-M       input to a real input file from command line.
C-M       WARNING: To use this feature make sure that SIXTAB.INP file
C-M                does not exist on the default directory.
C-M
C-M  Old-style input instructions format:
C-M  Two records are expected from input:
C-M  - Name of the source ENDF file.
C-M  - Name of the target ENDF file to be written.
C-M
C-M  Keyword-oriented input instructions:
C-M  The format follows a few simple rules, which are worth noting
C-M  - Parameters are read in columns of 10. 
C-M  - The first 10 columns are interpreted as the keyword.
C-M  - Records with blanks in the first 10 columns are ignored, so
C-M    comments may be entered freely in the input file, provided
C-M    they appear on separate records and start after column 10.
C-M  - Only keywords that are recognised by the program are processed.
C-M  - The absence of a keyword for a particular parameter activates
C-M    the default value of the parameter preset in the code.
C-M  These rules allow the input instructions for several programs to
C-M  appear on the same input file, provided all programs follow the
C-M  above rules. This significantly reduces input file maintenance
C-M  for more complicated sequences of code execution and possible
C-M  archival of the input file for Quality Assurance purposes.
C-M
C-M  Keyword
C-M  '$* SIXTAB ' The main keyword of the SIXTAB program. All
C-M               preceeding input is ignored.
C-M  '   FLNINP ' Name of the source ENDF input file is given in
C-M               columns 11-50.
C-M  '   FLNOUT ' Name of the target ENDF output file is given in
C-M               columns 11-50.
C-M  '   NANGLES' Number of discrete angles for tabulation are defined
C-M               columns 11-20.
C-M  '   ENDSIX ' This keyword signals the end of SIXTAB input. All
C-M               instructions that follow are ignored.
C-M
C-M  File units:
C-M   Name No. Description:
C-M    LIN  1  Input instructions file, if not read from default input.
C-M    LEN  2  Source ENDF input file, if not read from default input.
C-M    LOU  3  Target ENDF output file, if not written to the default
C-M            output.
C-M    LKB  5  Default input.
C-M    LTT  6  Default output.
C-
      PARAMETER    (NCM=2, MXMT=400)
      CHARACTER*66  C66,CMT(NCM)
      CHARACTER*40  FLIN,FLEN,FLOU
      CHARACTER*40  FLSC
      DIMENSION     MTI(MXMT),QQI(MXMT)
      LOGICAL       EXST
C*
      DATA FLIN/'SIXTAB.INP'/
     2     FLEN/'ENDF.DAT'/
     3     FLOU/'ENDFNEW.DAT'/
      DATA LIN,LEN,LOU,LKB,LTT/ 1, 2, 3, 5, 6 /
C* Scratch file unit number and name
      DATA LSC/19/
      DATA FLSC/'SIXTAB.TMP'/
      OPEN (UNIT=LSC,FILE=FLSC,STATUS='UNKNOWN')
C*
C* Default number of angles
      NCS=33
C*
C* Check for the existence of the input file
      INQUIRE(FILE=FLIN,EXIST=EXST)
      IF(EXST) GO TO 14
C* SIXTAB Input does not exist - try interpreting default input
      IER=1
      GO TO 15
C* SIXTAB Input exists -Process the input file
   14 OPEN(UNIT=LIN,FILE=FLIN,STATUS='OLD')
      IER=0
      CALL SIXINP(LIN,LTT,FLEN,FLOU,NCS,IER)
      CLOSE(UNIT=LIN)
      IF(IER.EQ.0) GO TO 16
C* Remedial action if fatal error on input is encountered:
C* Print a warning to default output and try reading default input
   15 CALL SIXINP(LKB,LTT,FLEN,FLOU,NCS,IER)
      FLIN='Keyboard'
      IF(IER.EQ.0) GO TO 16
C* Try input/output ENDF files from default input/output
      LEN=LKB
      LOU=LTT
      LTT=0
      MATH=0
      MFH =0
      MTH =0
      C66 =' SIXTAB Processing of ENDF files '
     1   //'                                 '
      GO TO 17
C*
C* Input instructions processed
   16 OPEN (UNIT=LEN,FILE=FLEN,STATUS='OLD',ERR=84)
      OPEN (UNIT=LOU,FILE=FLOU,STATUS='UNKNOWN')
      WRITE(LTT,900) ' SIXTAB - Convert ENDF MF6 to tabular   '
      WRITE(LTT,900) ' ------------------------------------   '
      WRITE(LTT,900)
      WRITE(LTT,900) '              Input instructions file : ',FLIN
      WRITE(LTT,900) '               Source input ENDF file : ',FLEN
      WRITE(LTT,900) '              Target output ENDF file : ',FLOU
      WRITE(LTT,900)
      WRITE(LTT,903) '            Number of discrete angles : ',NCS
C*
C* Copy the header card
      READ (LEN,901) C66,MATH,MFH,MTH,NS
   17 WRITE(LOU,901) C66,MATH,MFH,MTH,NS
C* Edit the comment section to identify changes
      CMT(1)='**********************'//
     1       ' SIXTAB Version 03/12 '//
     1       '**********************'
      WRITE(CMT(2),991) NCS
   20 CALL EDTMF1(LEN,LOU,CMT,NCM,C66,MATH,MFH,MTH,IER)
      IF(MATH.LT.0) GO TO 80
      IF(IER.EQ.1 .AND.LTT.GT.0) THEN
        WRITE(LTT,901) ' WARNING - No comments section on file  '
      END IF
      MATD=MATH
      MFD =MFH
      MTD =MTH
      NS  =0
      MTT =-1
      NQI =0
C* Copy up to MF6
      READ (LEN,901) C66,MATH,MFH,MTH
   30 NS=NS+1
      IF(MTH.EQ.0) NS=99999
      IF(MFH.EQ.0) NS=0
      IF(NS.GT.99999) NS=0
      WRITE(LOU,901) C66,MATH,MFH,MTH,NS
      IF(MTH.EQ.0) NS=0
      READ (LEN,901) C66,MATH,MFH,MTH
      IF     (MATH.EQ.0) THEN
        NS=0
        WRITE(LOU,901) C66,MATH,MFH,MTH,NS
        GO TO 20
      ELSE IF(MATH.LT.0) THEN
        GO TO 80
      END IF
C* Save the QI value from MF3 for each reaction
      IF(MFH.EQ.3 .AND. (MTH.NE.0 .AND. MTH.NE.MTT)) THEN
        NS=NS+1
        IF(NS.GT.99999) NS=0
        WRITE(LOU,901) C66,MATH,MFH,MTH,NS
        READ (LEN,901) C66,MATH,MFH,MTH
        READ (C66,911) QM,QI
        NQI=NQI+1
        IF(NQI.GT.MXMT) STOP 'SIXTAB ERROR - MXMT limit exceeded'
        MTI(NQI)=MTH
        QQI(NQI)=QI
        MTT=MTH
      END IF
      IF(MFH.NE.6) GO TO 30
C* Process the ENDF file MF6
  40  CONTINUE
      DO I=1,NQI
        IF(MTH.EQ.MTI(I)) QI=QQI(I)
      END DO
      CALL MF6LW7(LEN,LOU,LTT,LSC,QI,NCS,C66,MATH,MFH,MTH,IER)
      IF(IER.NE.0) THEN
        READ (LEN,901) C66,MATH,MFH,MTH
        IF(MFH.EQ.0) GO TO 50
        GO TO 40
      END IF
C*
C* Skip the rest of the section
   42 READ (LEN,901) C66,MATH,MFH,MTH
      IF(MTH.NE.0) GO TO 42
      READ (LEN,901) C66,MATH,MFH,MTH
      IF(MFH.EQ.6) GO TO 40
C* Copy the rest of the file
   50 NS=NS+1
      IF(MTH.EQ.0) NS=99999
      IF(MFH.EQ.0) NS=0
      IF(NS.GT.99999) NS=0
      WRITE(LOU,901) C66,MATH,MFH,MTH,NS
      IF(MTH.EQ.0) NS=0
      READ (LEN,901) C66,MATH,MFH,MTH
      IF     (MATH.EQ.0) THEN
        NS=0
        WRITE(LOU,901) C66,MATH,MFH,MTH,NS
        GO TO 20
      ELSE IF(MATH.LT.0) THEN
        GO TO 80
      END IF
      GO TO 50
C* File processing finished
   80 WRITE(LOU,901) C66,MATH,MFH,MTH, 0
      STOP 'SIXTAB Completed'
C*
C* Error traps
   82 STOP 'SIXTAB ERROR - Illegal input'
   84 STOP 'SIXTAB ERROR - Source ENDF file invalid'
C*
  900 FORMAT(2A40)
  901 FORMAT(A66,I4,I2,I3,I5)
  903 FORMAT(A40,I4)
  910 FORMAT(8A10)
  911 FORMAT(2F11.0,4I11)
  991 FORMAT(' Neutron distributions in MF6 con',
     2       'verted to Law 7 using',I4,' cosines')
      END
      SUBROUTINE SIXINP(LIN,LTT,FLEN,FLOU,NCS,IER)
C-Title  : Subroutine SIXINP
C-Purpose: Process input instructions for SIXTAB
      PARAMETER    (MXKW=10)
      CHARACTER*40  FLEN,FLOU
      CHARACTER*10  KWRD(MXKW),REC(8),C10
C*
      DATA KWRD
     1/'$* SIXTAB ','   FLNINP ','   FLNOUT ','   NANGLES','          '
     1,'          ','          ','          ','          ','   ENDSIX '/
C*
      READ (LIN,901,ERR=800) REC
      C10=REC(1)
      IF(C10(1:3).EQ.'$* ') GO TO 81
C* If IER not zero on input - old style input not allowed
      IF(IER.NE.0) RETURN
C*
C* Old-style input
C*
C* Define input and output ENDF files
      FLEN=REC(1)//REC(2)//REC(3)//REC(4)
      READ (LIN,901,ERR=800) REC
      FLOU=REC(1)//REC(2)//REC(3)//REC(4)
      RETURN
C*
C* Keyword-oriented input
C*
C* Skip to the main SIXTAB input keyword
   80 READ (LIN,901,ERR=800) REC
   81 IF(REC(1).NE.KWRD(1)) GO TO 80
      IER=0
C* Read SIXTAB input
   90 READ (LIN,901,ERR=800,END=200) REC
C* Identify the keyword
      DO 96 I=1,MXKW
      IK=I
      IF(REC(1).EQ.KWRD(I)) GO TO 98
   96 CONTINUE
C* Unidentified keyword encountered
      WRITE(LTT,904) REC(1)
      GO TO 90
C* Process the input records
   98 CONTINUE
      GO TO(110,120,130,140,150,160,170,180,190,200),IK
C* Input options:
C* '$* SIXTAB '
  110 GO TO 90
C* '   FLNINP '
  120 FLEN=REC(2)//REC(3)//REC(4)//REC(5)
      GO TO 90
C* '   FLNOUT '
  130 FLOU=REC(2)//REC(3)//REC(4)//REC(5)
      GO TO 90
C* '   NANGLES'
  140 READ (REC(2),902) I
      IF(I.GT.0) NCS=I
      GO TO 90
C* Unused keywords
  150 CONTINUE
  160 CONTINUE
  170 CONTINUE
  180 CONTINUE
  190 CONTINUE
      GO TO 90
C* '   ENDSIX '
  200  RETURN
C*
C* Error trapped
  800 IER=1
      RETURN
C*
  901 FORMAT(8A10)
  902 FORMAT(BN,I10)
  904 FORMAT(' Unrecognised keyword "',A10,'"')
      END
      SUBROUTINE MF6LW7(LEN,LOU,LTT,LSC,QI,NCS,C66,MAT0,MF0,MT0,IER)
C-Title  : Subroutine MF6LW7
C-Purpose: Adapted fix6 routine of NJOY
C-Author : A. Trkov, International Atomic Energy Agency, Vienna
C-Dscription:
C-D A section of file 6 using legendre or tabulated angular distributions
C-D is converted into Law 7 format with NCS angles. Intermediate results
C-D are written to scratch file on unit LSC.
C-
      PARAMETER       (MXMU=50, MXPL=100, MXNB=20, MXRW=20000)
      CHARACTER*66     B66,C66
      CHARACTER*8      CLANG
      DOUBLE PRECISION PAR,CLB,P1,P2,PP
      DIMENSION RWO(MXRW),NBT(MXNB),INT(MXNB)
      DIMENSION AMU(MXMU),PLL(MXPL)
C*
      DATA PI /3.141592654/
C* Masses of primary light particles
c...  DATA AWN/1.008665/
c...  DATA AWH/1.007273/
c...  DATA AWD/2.013558/
c...  DATA AWT/3.015505/
c...  DATA AW3/3.014960/
c...  DATA AWA/4.001505/
C* AMDC Audi-Wapstra mass tables 2003 "http://www-nds.iaea.org/amdc/"
C* Subtract electron mass and add ionisation energy defect
      AWN = 1.008664916
      AWH = 1.007825032 -   0.00054857991 + 0.000000015
      AWD = 2.014101778 -   0.00054857991 + 0.000000015
      AWT = 3.016049278 -   0.00054857991 + 0.000000015
      AW3 = 3.016029319 - 2*0.00054857991 + 0.000000085
      AWA = 4.002603254 - 2*0.00054857991 + 0.000000085
C* Atomic mass constant (amu to eV)
      DATA AMUEV/931.494013E6/
C*
      B66='                                 '
     1  //'                                 '
      ZERO=0
      NS  =0
      REWIND LSC
C* Assume the projectile is neutron
      IZPR=1
C* Define cosines for NCS angle intervals
      IF(NCS.GT.MXMU) STOP 'SIXTAB ERROR - MXMU limit exceeded'
      DPI=PI/(NCS-1)
      DO I=1,NCS
        AMU(I)=COS(PI-(I-1)*DPI)
      END DO
C*
C* Interpret the header CONT record
      READ(C66,902) ZA,AWR, L1,LCT, NK, N2
      IZA =NINT(ZA)
      IER =0
C* Co-ordinate system flag: save original and preset output
      LCT0=LCT
      LCT1=LCT
C* Start the conversion process
      IF(LTT.GT.0) WRITE(LTT,960) MT0
C* Begin loop over all outgoing particles
      DO 800 IK=1,NK
      NSIG=0
      SIGMX=0
      LXE=1
      LXS=LXE+(MXRW-LXE)/2
      KX =MXRW-LXS
C* Process the multiplicities for this particle
      CALL RDTAB1(LEN,ZAP,AWP,LIP,LAW0, NR, NP, NBT,INT
     1           ,RWO(LXE),RWO(LXS),KX,IER)
      IF(IER.NE.0) STOP 'SIXTAB ERROR - MXRW limit exceeded'
      IZAP=NINT(ZAP)
      LAW =LAW0
C* Set the co-ordinate system flag LCT
      IF(LCT0.EQ.3) THEN
        IA=IZAP-1000*(IZAP/1000)
        IF(IA.GT.4) THEN
          LCT=1
        ELSE
          LCT=2
        END IF
      END IF
C* Redefine the desired Law for data representation
      IF(LAW0.EQ.1) THEN
        LAW=7
C* Redefine co-ordinate system to Lab
        LCT1=1
      END IF
C* Define the mass of the incident particle
      IF     (IZPR.EQ.   1) THEN
        AM1=AWN
      ELSE IF(IZPR.EQ.1001) THEN
        AM1=AWH
      ELSE IF(IZPR.EQ.1002) THEN
        AM1=AWD
      ELSE IF(IZPR.EQ.1003) THEN
        AM1=AWT
      ELSE IF(IZPR.EQ.2003) THEN
        AM1=AW3
      ELSE IF(IZPR.EQ.2004) THEN
        AM1=AWA
      ELSE
        STOP 'SIXTAB ERROR - Unknown incident particle'
      END IF
C* Mass of the target nucleus
      AM2=AWR*AWN
C* Mass of the primary outgoing particle
      AM3=AWP*AWN
C* Mass ratios: AMR=target/projectile; ADS=ejectile/projectile
      AMR=AM2/AM1
      ADS=AM3/AM1
C*
      IF(IK.EQ.1) THEN
C* Write the header CONT record if processing the first particle
        CALL WRCONT(LOU,MAT0,MF0,MT0,NS,ZA,AWR,L1,LCT1,NK,N2)
      END IF
C* Write the multiplicities for this particle to the output file
      CALL WRTAB1(LOU,MAT0,MF0,MT0,NS,ZAP,AWP,LIP,LAW
     1           , NR, NP, NBT,INT,RWO(LXE),RWO(LXS))
C*
      IF     (LAW0.EQ.1) THEN
C* Proceed to process Law 1 data (continuum energy-angle distributions)
        GO TO 130
C* Copy other format representations
      ELSE IF(LAW0.EQ.0 .OR. LAW0.EQ.3 .OR. LAW0.EQ.4) THEN
C*      No action necessary for Law 0, 3, 4
        GO TO 800
      ELSE IF(LAW0.EQ.2 .OR. LAW0.EQ.5 ) THEN
C*      Copy the data for Law 2, 5
        IF(LAW0.EQ.5 .AND. LTT.GT.0)
     1    WRITE(LTT,900) ' WARNING - Conversion not done for Law  ',LAW0
        CALL RDTAB2(LEN,C1,C2,L1,L2,NR,NE,NBT,INT,IER)
        CALL WRTAB2(LOU,MAT0,MF0,MT0,NS,C1,C2,L1,L2
     1             ,NR,NE,NBT,INT)
        NMX=MXRW
        DO IE=1,NE
          CALL RDLIST(LEN,C1,EI,LANG,L2,NW,NL,RWO,NMX,IER)
          IF(IER.NE.0) STOP 'SIXTAB ERROR - MXRW limit exceeded'
          IF(LAW0.EQ.2 .AND. LANG.EQ.0) THEN
C*        Convert Legendre to pointwise distribution if necessary
            CALL LEGPNT(NW,NL,RWO,MXRW)
            LANG=12
          END IF
          CALL WRLIST(LOU,MAT0,MF0,MT0,NS,C1,EI,LANG,L2,NW,NL,RWO)
        END DO
        GO TO 800
      ELSE IF(LAW.EQ.7) THEN
C*      Copy the data for Law 7
        CALL RDTAB2(LEN,C1,C2,L1,L2,NR,NE,NBT,INT,IER)
        CALL WRTAB2(LOU,MAT0,MF0,MT0,NS,C1,C2,L1,L2
     1             ,NR,NE,NBT,INT)
        NMX=MXRW
        DO IE=1,NE
          CALL RDTAB2(LEN,C1,C2,L1,L2,NRM,NMU,NBT,INT,IER)
          CALL WRTAB2(LOU,MAT0,MF0,MT0,NS,C1,C2,L1,L2
     1               ,NRM,NMU,NBT,INT)
          KX=NMX/2
          LX=1+KX
          DO IM=1,NMU
            CALL RDTAB1(LEN,C1,ACOS,L1,L2,NRP,NEP,NBT,INT
     &                 ,RWO,RWO(LX),KX,IER)
            IF(IER.NE.0) STOP 'SIXTAB ERROR - MXRW limit exceeded'
            CALL WRTAB1(LOU,MAT0,MF0,MT0,NS,C1,ACOS,L1,L2
     1                 ,NRP,NEP,NBT,INT,RWO,RWO(LX))
          END DO
        END DO
        GO TO 800
      ELSE
C*      Don't know how to process - file incomplete - terminate.
        IF(LTT.GT.0) THEN
          WRITE(LTT,961) LAW0,IZAP
        END IF
        STOP 'SIXTAB ERROR - Can not convert the data'
      END IF
C*
C* Read Law 1 format specifications for outgoing energies
  130 CALL RDTAB2(LEN,C1,C2,LANG ,LEP ,NR,NE,NBT,INT,IER)
      IF(LTT.GT.0) THEN
        IF     (LANG.EQ.1)THEN
          CLANG='Legendre'
        ELSE IF(LANG.EQ.2)THEN
          CLANG='Kalbach '
          IF(LCT.NE.2)
     1    WRITE(LTT,900) ' WARNING - Kalbach requires CM co-ordin.'
        ELSE
          CLANG='Unknown '
        END IF
        WRITE(LTT,962) CLANG,IZAP
      END IF
C* Redefine konstants for Law 7
      L1=0
      L2=0
C* Write the first TAB2 record for inc. energies of this section
      CALL WRTAB2(LOU,MAT0,MF0,MT0,NS,C1,C2,L1,L2
     1           ,NR,NE,NBT,INT)
C* Begin loop over incident particle energies
      LX1=1
      DO 600 I=1,NE
C* Read in the data for this energy
      NMX=MXRW-LX1
      CALL RDLIST(LEN,C1,EIN,ND,NA,NW,NEP,RWO(LX1),NMX,IER)
      IF(IER.NE.0)
     1  STOP 'MF6LW7 ERROR - reading incident energy LIST'
      IF(ND.GT.0) THEN
C*
C* Case: Discrete particle energies present
        IF(LTT.GT.0 .AND. NA.GT.0) THEN
          WRITE(LTT,900) ' WARNING - Discrete particle energies  '
          WRITE(LTT,901) '                    at incident energy ',EIN
          WRITE(LTT,900) '          Angular distributions ignored'
        END IF
C* Write the TAB2 record defining incident energy and cosine list
        C1 =0
        NRM=1
        NMU=2
        NBT(1)=2
        INT(1)=2
        CALL WRTAB2(LOU,MAT0,MF0,MT0,NS,C1,EIN, 0, 0
     1           ,NRM,NMU,NBT,INT)
        LXE=LX1+NW
        LXS=(LXE+MXRW)/2
        NXS=(LXS-LXE)/3
        LXE1=LXE +NXS
        LXE2=LXE1+NXS
        LXS1=LXS +NXS
        LXS2=LXS1+NXS
C* Process the continuum contribution (if present)
        IF(ND.LT.NEP) THEN
          NEPP=NEP-ND
          DO IP=1,NEPP
            RWO(LXE-1+IP)=RWO(LX1+(NA+2)*(ND+IP-1)  )
            RWO(LXS-1+IP)=RWO(LX1+(NA+2)*(ND+IP-1)+1)
          END DO
C*        Linearise the continuum distribution
          NR=1
          NBT(1)=NEPP
          INT(1)=LEP
          EPS=0.005
          CALL VECLIN(NR,NEPP,NBT,INT,RWO(LXE),RWO(LXS),NXS,EPS)
        ELSE
C* If only discrete lines present, preset the range to zero
          NEPP=2
          RWO(LXE  )=ABS(RWO(LX1+(NA+2)*(ND-1)))
          RWO(LXS  )=0
          RWO(LXE+1)=ABS(RWO(LX1))
          RWO(LXS+1)=0
          IF(RWO(LXE).GT.RWO(LXE+1)) THEN
            PRINT *,'SIXTAB WARNING - Discrete points'
     &             ,' not in descending order'
            XE=RWO(LXE)
            RWO(LXE  )=RWO(LXE+1)
            RWO(LXE+1)=XE
          END IF
        END IF
C* Add contributions from discrete lines (Gaussian NEP1=21 points)
        DO IP=1,ND
          EPS=0.02
          NEP1=21
          EG =ABS(RWO(LX1+(NA+2)*(IP-1)))
          FF=RWO(LX1+(NA+2)*(IP-1)+1)
          IF(FF.GT.0) THEN
C* Generate Gaussian around energy Eg
            CALL FNGAUS(EG,NEP1,RWO(LXE1),RWO(LXS1),EPS)
C* Generate union grid with previous distribution
            CALL UNIGRD(NEPP,RWO(LXE),NEP1,RWO(LXE1)
     &                 ,NEP2,RWO(LXE2),NXS)
C* Interpolate previous distribution to the union grid
            CALL FITGRD(NEPP,RWO(LXE),RWO(LXS)
     &                 ,NEP2,RWO(LXE2),RWO(LXS2))
C* Save the previous distribution
            NEPP=NEP2
            DO K=1,NEPP
              RWO(LXE-1+K)=RWO(LXE2-1+K)
              RWO(LXS-1+K)=RWO(LXS2-1+K)
            END DO
C* Interpolate discrete Gaussian distribution to the union grid
            CALL FITGRD(NEP1,RWO(LXE1),RWO(LXS1)
     &                 ,NEP2,RWO(LXE2),RWO(LXS2))
C* Add the discrete Gaussian distribution
            DO K=1,NEPP
              RWO(LXS-1+K)=RWO(LXS-1+K)+RWO(LXS2-1+K)*FF
            END DO
          END IF
        END DO
C* Normalise the distribution
        E2=RWO(LXE)
        F2=RWO(LXS)
        YTG=0
        DO K=2,NEPP
          E1=E2
          F1=F2
          E2=RWO(LXE-1+K)
          F2=RWO(LXS-1+K)
          YTG=YTG+(E2-E1)*(F2+F1)/2
        END DO
C* Check the integral normalisation factor
        IF(LTT.GT.0 .AND. ABS(YTG-1).GT.5.E-3) THEN
          WRITE(LTT,900) ' WARNING - Discrete particle energies   '
          WRITE(LTT,901) '                     at incident energy ',EIN
          WRITE(LTT,901) '          Spectrum normalisation factor ',YTG
        END IF
        YTG=YTG*2
        DO K=1,NEPP
          RWO(LXS-1+K)=RWO(LXS-1+K)/YTG
        END DO
C* Write the distribution for extreme cosines (-1, +1)
        NRP= 1
        NBT(1)=NEPP
        INT(1)=2
        RMU=-1
        CALL WRTAB1(LOU,MAT0,MF0,MT0,NS, C1,RMU, 0, 0
     1             ,NRP,NEPP,NBT,INT,RWO(LXE),RWO(LXS))
        RMU= 1
        CALL WRTAB1(LOU,MAT0,MF0,MT0,NS, C1,RMU, 0, 0
     1             ,NRP,NEPP,NBT,INT,RWO(LXE),RWO(LXS))
        GO TO 600
      END IF
C*
C* Case: Normal Law-1 distribution with angular dependence
      IF(LANG.EQ.2 .AND. NA.NE.1)
     1  STOP 'MF6LW7 ERROR - Kalbach requires NA=1'
      LXE =LX1+NW
      LXX =LX1+(MXRW-LXE)/2
      KX  =MXRW-LXX
      NCYC=NA+2
      NL  =NA+1
      NMU =NCS
      IF( NL.LE.1) NMU=2
C* Linear interpolation between cosines
      INTM=2
C* Write the TAB2 record with incident energy and the cosine grid
      C1  =0.
      L1  =0
      L2  =0
      NRM =1
      NBT(1)=NMU
      INT(1)=INTM
      CALL WRTAB2(LOU,MAT0,MF0,MT0,NS,C1,EIN,L1,L2
     1           ,NRM,NMU,NBT,INT)
C*
C* Begin loop over the cosines for law 7
      SIG =0
      DO 500 IMU=1,NMU
      IF     (IMU.EQ.  1) THEN
        ACOS=-1.
      ELSE IF(IMU.EQ.NMU) THEN
        ACOS=+1.
      ELSE
        ACOS=AMU(IMU)
      END IF
C* Reconstruct the energy distribution for this cosine
      IP =0
      JP =0
      SEN=0
C* Begin loop over secondary particle energies
      ECM0=0
  200 IP=IP+1
      ECM=RWO(LX1+NCYC*(IP-1))
C* Check that outgoing particle energies are monotonic increasing
      IF(ECM.LT.ECM0) THEN
        IF(LTT.GT.0) WRITE(LTT,964) IZAP,EIN,ECM
        GO TO 408
      END IF
      ECM0=ECM
      CSN=ACOS
      ELB=ECM
      DRV=1
      IF (LCT.NE.1 .AND. ECM.GT.0 ) THEN
C* Convert CM to Lab co-ordinate system
C* Coding from NJOY-99 manual for GROUPR page VII-35
C* Input cosine grid is defined to be the in Lab system
        CLB=DBLE(ACOS)
C* Equation (125) c = PAR * sqrt( EIN / ELB)
        PAR=SQRT(DBLE(ADS))/DBLE(AMR+1)
C* From Equations (123) and (125) solve quadratic for the squareroot of
C* the outgoing particle energy SQRT(ELB) in the Lab system
        P1=PAR*CLB*SQRT(DBLE(EIN))
        P2=DBLE(ECM)-PAR*PAR*DBLE(EIN)*(1-CLB**2)
C* Test for cosine values below minimum
        IF(P2.LT.0) THEN
          P2 =0
          DRV=0
c...      IF(IMU.LT.NMU .AND. AMU(IMU+1).LE.CMN) DRV=0
        END IF
C* Square the components to get the outgoing particle energy ELB
        P2 =SQRT(P2)
        PP =P1*P1+P2*P2
        P2 =2*P1*P2
        P1 =PP
C* Mathematical constraint is on CLB^2 (absolute value of cosine).
C* P2 has same sign as CLB. For backward scattering CLB<0, so the
C* smaller root should be taken --> use +P2
        ELB=P1+P2
C* Calculate corresponding cosine in the CM system from Equation (124)
        CSN=SQRT(ELB/ECM)*(CLB-PAR*SQRT(DBLE(EIN/ELB)))
C* Check the limits
        QQ=-1
        IF (CSN.LT.QQ) CSN=QQ
        QQ=1
        IF (CSN.GT.QQ) CSN=QQ
C* Add jacobian for the CM-to-Lab transformation
        DRV=DRV*SQRT(ELB/ECM)
      END IF
C*
C* Process the angular distributions
      IF (LANG.EQ. 1) THEN
C*
C* Calculate the probability from Legendre polynomials
        IF(NA.GE.MXPL) STOP 'SIXTAB ERROR - MXPL limit exceeded'
        CALL PLNLEG(CSN,PLL,NA)
        FMU=0
        DO LL=1,NL
          FMU=FMU+0.5*(2*LL-1)*PLL(LL)*RWO(LX1+NCYC*(IP-1)+LL)
        END DO
      ELSE IF(LANG.EQ.2) THEN
C*
C* Calculate the probability from Kalbach-Mann representation
        SS=RWO(LX1+NCYC*(IP-1)+1)
        RR=RWO(LX1+NCYC*(IP-1)+2)
        AA=BACH(IZPR,IZAP,IZA,EIN,ECM)
        FMU=SS*AA*(COSH(AA*CSN)+RR*SINH(AA*CSN))/(2*SINH(AA))
      ELSE IF(LANG.GT.10) THEN
C*
C* Calculate the probability from pointwise representation
        MMU=(NCYC-1)/2
        DO KMU=1,MMU-1
          LL=LX1+5+2*KMU+NCYC*(IP-1)
          IF (CSN.GE.RWO(LL).AND.CSN.LE.RWO(LL+2)) GO TO 303
        END DO
        FMU=0.
        GO TO 400
  303   INTI=LANG-10
        FMU =FINEND(INT,CSN,RWO(LL),RWO(LL+1),RWO(LL+2),RWO(LL+3))
      ELSE
        IF(LTT.GT.0)
     &  WRITE(LTT,902) ' SIXTAB ERROR - Illegal value of LANG   ',LANG
        STOP 'SIXTAB ERROR - Illegal value of LANG'
      END IF
C* Processing of distribution for one outgoing energy point completed
  400 CONTINUE
C*
C* Enter energy point - skip if energy less then previous point
      IF(IP.GT.1 .AND. ELB.LT.RWO(LXE+JP-1)) GO TO 408
      RWO(LXE+JP)=ELB
      RWO(LXX+JP)=FMU*DRV
      JP=JP+1
      IF(JP.GE.KX) STOP 'MF6LW7 ERROR - MXRW capacity exceeded'
C* Continue with the secondary energy loop
  408 IF(IP.LT.NEP) GO TO 200
C*
C* All secondary particles energies processed
      IF(JP.EQ. 1 ) THEN
C* Check if single point
        EPS=1.E-3
        ELB=RWO(LXE-1+JP)
        RWO(LXE+JP)=ELB*(1+EPS)
        RWO(LXX+JP)=0
        JP=JP+1
      END IF
      NRP=1
      C1 =0
      L1 =0
      L2 =0
      NS0=0
C* Write the spectrum distribution for this cosine
      CALL WRTAB1(LSC,MAT0,MF0,MT0,NS0,C1,ACOS,L1,L2
     1           , NRP,JP,NBT,INT,RWO(LXE),RWO(LXX))
C* Add contribution to the integral
      SEN=0
      E1 =RWO(LXE)
      F1 =RWO(LXX)
      DO J=2,JP
        E2=RWO(LXE-1+J)
        F2=RWO(LXX-1+J)
        IF(LEP.EQ.1) THEN
          FF=F1
        ELSE
          FF=(F1+F2)/2
        END IF
        SEN=SEN+FF*(E2-E1)
        F1=F2
        E1=E2
      END DO
      IF(IMU.GT.1) THEN
        JNTM=INTM-10*(INTM/10)
        IF(JNTM.EQ.1) THEN
          FF=SEN0
        ELSE
          FF=(SEN+SEN0)/2
        END IF
        SIG = SIG + FF*(ACOS-ACOS0)
      END IF
      ACOS0= ACOS
      SEN0 = SEN
C* Continue with the cosine loop
  500 CONTINUE
C* Check the integral normalisation factor
        IF(LTT.GT.0 .AND. ABS(SIG-1).GT.1.E-2) THEN
          NSIG=NSIG+1
          IF(ABS(SIG-1).GT.SIGMX) THEN
            SIGMX=SIG-1
            EINMX=EIN
          END IF
        END IF
C* Copy from scratch to output and normalise distributions
      REWIND LSC
      DO 550 IMU=1,NMU
      CALL RDTAB1(LSC,C1,ACOS,L1,L2,NRP, JP, NBT,INT
     1           ,RWO(LXE),RWO(LXX),KX,IER)
      DO 542 J=1,JP
      RWO(LXX+J-1)=RWO(LXX+J-1)/SIG
  542 CONTINUE
      CALL WRTAB1(LOU,MAT0,MF0,MT0,NS,C1,ACOS,L1,L2
     1           , NRP,JP,NBT,INT,RWO(LXE),RWO(LXX))
  550 CONTINUE
      REWIND LSC
C* Continue with the incident energy loop
  600 CONTINUE
C* Print cumulative error message for spectrum normalisation
      IF(LTT.GT.0 .AND. NSIG.GT.0) THEN
        SIGMX=MIN(99999999.9,SIGMX*100)
        WRITE(LTT,900) ' WARNING - Spectrum normalis. at points ',NSIG
        WRITE(LTT,903) '            Max normalisation dif. [%]  ',SIGMX
        WRITE(LTT,901) '                    at incident energy  ',EINMX
      END IF
C* Continue with the particle loop
  800 CONTINUE
C* Section processed - write SEND record
      NS=99998
      CALL WRTEXT(LOU,MAT0,MF0,  0,NS,B66)
      RETURN
C*
  900 FORMAT(A40,I6)
  901 FORMAT(A40,1P,E11.3)
  902 FORMAT(2F11.0,4I11,I4,I2,I3,I5)
  903 FORMAT(A40,F11.1)
  960 FORMAT(/' Converting MF=6, MT=',I3,' to Law 7 format')
  961 FORMAT(' SIXTAB ERROR - Can not process Law',I3,' for ZAP',I6)
  962 FORMAT(12X,A8,' Representation for particle IZAP',I6)
  964 FORMAT(' SIXTAB WARNING - Format error ZAP,Ein,Ep',I6,1P,2E10.3)
      END
      SUBROUTINE LEGPNT(NW,NL,RWO,MXRW)
C-Title  : Subroutine LEGPNT
C-Purpose: Convert Legendre distribution to pointwise form
      DIMENSION  RWO(MXRW)
      NW0=NW
      NL0=NL
C* NXP - number of angular points
C* LXC - address of Legendre components
C* LXP - address of the angle/distribution pairs
      NXP=2*NL+2
      LXC=NW+2
      LXP=LXC+NL+1
      IF(LXP+2*NXP.GT.MXRW) STOP 'LEGPNT ERROR - MXRW limit exceeded'
C* Insert the implied P0 Legendre component
      DO L=1,NL
        RWO(LXC-L)=RWO(LXC-1-L)
      END DO
      RWO(1)=0.5
C* Angular cosine increment
      DCS=2
      DCS=DCS/(NXP-1)
C* loop over output points
      DO I=1,NXP
C*      Cosine point
        AC=-1+(I-1)*DCS
        IF(I.EQ.  1) AC=-1
        IF(I.EQ.NXP) AC= 1
C*      Legendre components for this cosine
        CALL PLNLEG(AC,RWO(LXC),NL)
C*      Distribution value at this cosine
        FF=RWO(1)*RWO(LXC)
        DO L=1,NL
          FF=FF+(2*L-1)*RWO(1+L)*RWO(LXC+L)/2
        END DO
C*      Save the cosine and the distribution
        RWO(LXP+2*(I-1)  )=AC
        RWO(LXP+2*(I-1)+1)=FF
      END DO
C* Define new field parameters for pointwise data
      NL=NXP
      NW=NL*2
C* Move the pointwise data
      DO I=1,NW
        RWO(I)=RWO(LXP-1+I)
      END DO
C*
      RETURN
      END
      FUNCTION BACH(IZA1,IZA2,IZAT,E,EP)
C     ******************************************************************
C     Compute the Kalbach 'a' parameter
C     (Routine borrowed from the NJOY-97 code by R.E.MacFarlane)
C     Changes: Forced uppercase, STOP on error,
C              single precision real formal parameters.
C     Formal parameters:
C       IZA1   Projectile ZA designation.
C       IZA2   Emitted particle ZA designation.
C       IZAT   Target ZA designation.
C       E      Incident energy of the projectile.
C       EP     Energy of the outgoing particle.
C     ******************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                          GROUPR.5410 
      CHARACTER*60 STRNG
      DOUBLE PRECISION NC,NB                                                          UP4.72   
      REAL   BACH,E,EP
      DATA THIRD,TWOTH,FOURTH/.333333333D0,.666666667D0,1.33333333D0/
      DATA C1,C2,C3,C4,C5,C6/15.68D0,-28.07D0,-18.56D0,33.22D0,
     1 -0.717D0,1.211D0/
      DATA S2,S3,S4,S5/2.22D0,8.48D0,7.72D0,28.3D0/
      DATA BRK1,BRK2,HALF/130.D0,41.D0,0.5D0/
      DATA B1,B2,B3/0.04D0,1.8D-6,6.7D-7/
      DATA TOMEV/1.D-6/
C
      IZA=IZAT
      IF (IZA.EQ. 6000) IZA= 6012
      IF (IZA.EQ.12000) IZA=12024
      IF (IZA.EQ.14000) IZA=14028
      IF (IZA.EQ.16000) IZA=16032
      IF (IZA.EQ.17000) IZA=17035
      IF (IZA.EQ.19000) IZA=19039
      IF (IZA.EQ.20000) IZA=20040
      IF (IZA.EQ.22000) IZA=22048
      IF (IZA.EQ.23000) IZA=23051
      IF (IZA.EQ.24000) IZA=24052
      IF (IZA.EQ.26000) IZA=26056
      IF (IZA.EQ.50000) IZA=50120
      IF (IZA.EQ.74000) IZA=74184
      IF (IZA.EQ.82000) IZA=82208
      AA=MOD(IZA,1000)
      IF (AA.EQ.0.) THEN
         STOP 'BACH ERROR - dominant isotope not known'
      ENDIF
      ZA=INT(IZA/1000)
      AC=AA+MOD(IZA1,1000)
      ZC=ZA+INT(IZA1/1000)
      AB=AC-MOD(IZA2,1000)
      ZB=ZC-INT(IZA2/1000)
      NA=NINT(AA-ZA)
      NB=NINT(AB-ZB)
      NC=NINT(AC-ZC)
      SA=C1*(AC-AA)
     1   +C2*((NC-ZC)**2/AC-(NA-ZA)**2/AA)
     2   +C3*(AC**TWOTH-AA**TWOTH)
     3   +C4*((NC-ZC)**2/AC**FOURTH-(NA-ZA)**2/AA**FOURTH)
     4   +C5*(ZC**2/AC**THIRD-ZA**2/AA**THIRD)
     5   +C6*(ZC**2/AC-ZA**2/AA)
      IF (IZA1.EQ.1002) SA=SA-S2
      IF (IZA1.EQ.1003) SA=SA-S3
      IF (IZA1.EQ.2003) SA=SA-S4
      IF (IZA1.EQ.2004) SA=SA-S5
      SB=C1*(AC-AB)
     1   +C2*((NC-ZC)**2/AC-(NB-ZB)**2/AB)
     2   +C3*(AC**TWOTH-AB**TWOTH)
     3   +C4*((NC-ZC)**2/AC**FOURTH-(NB-ZB)**2/AB**FOURTH)
     4   +C5*(ZC**2/AC**THIRD-ZB**2/AB**THIRD)
     5   +C6*(ZC**2/AC-ZB**2/AB)
      IF (IZA2.EQ.1002) SB=SB-S2
      IF (IZA2.EQ.1003) SB=SB-S3
      IF (IZA2.EQ.2003) SB=SB-S4
      IF (IZA2.EQ.2004) SB=SB-S5
      ECM=AA*E/AC
      EA=ECM*TOMEV+SA
      EB=TOMEV*EP*AC/AB+SB
      X1=EB
      IF (EA.GT.BRK1) X1=BRK1*EB/EA
      X3=EB
      IF (EA.GT.BRK2) X3=BRK2*EB/EA
      FA=1
      IF (IZA1.EQ.2004) FA=0
      FB=1
      IF (IZA2.EQ.1) FB=HALF
      IF (IZA2.EQ.2004) FB=2
      BACH=B1*X1+B2*X1**3+B3*FA*FB*X3**4
      RETURN
      END
      SUBROUTINE PLNLEG(U,PL,NL)
C-Title  : PLNLEG Subroutine
C-Purpose: Evaluate Legendre polynomials up to order NL
C-Author : A.Trkov, Institute J.Stefan, Ljubljana, Slovenia (1997)
C-Description:
C-D  Given the argument value U in the interval [-1,1], the
C-D  polynomials up to order NL are calculated by a recurrence
C-D  relation and stored in PL.
C-
      DIMENSION PL(1)
      PL(1)=1.
      IF(NL.LT.1) RETURN
      PL(2)=U
      IF(NL.LT.2) RETURN
      DO 20 L=2,NL
      G=U*PL(L)
      H=G-PL(L-1)
      PL(L+1)=H+G-H/L
   20 CONTINUE
      RETURN
      END
      SUBROUTINE EDTMF1(LEN,LOU,CMT,NCT,C66,MATD,MFD,MTD,IER)
C-Title  : Subroutine EDTMF1
C-Purpose: Edit ENDF file MF1 MT451 comments section
      CHARACTER*66  CMT(NCT),C66
C*
      IER=0
      IC =0
C* Process the section header records
      READ (LEN,901) C66,MATD,MFD,MTD,NS
      IF(MATD.LT.0) RETURN
      IF(MFD.NE.1 .AND. MTD.NE.451) THEN
        IER=1
        RETURN
      END IF
      NS=NS-1
C* Copy the first three records
   20 IC =IC+1
      NS =NS+1
      WRITE(LOU,901) C66,MATD,MFD,MTD,NS
      READ (LEN,901) C66,MATD,MFD,MTD
      IF(IC.LT.3) GO TO 20
C* Correct the length of the comments section
      READ (C66(45:55),904) NDC
      WRITE(C66(45:55),904) NDC+NCT
      NS =NS+1
      WRITE(LOU,901) C66,MATD,MFD,MTD,NS
C* Copy original comments
      DO 22 IC=1,NDC
      READ (LEN,901) C66,MATD,MFD,MTD
      NS=NS+1
      WRITE(LOU,901) C66,MATD,MFD,MTD,NS
   22 CONTINUE
C* Add NCT comments from CMT
      DO 24 IC=1,NCT
      NS=NS+1
      WRITE(LOU,901) CMT(IC),MATD,MFD,MTD,NS
   24 CONTINUE
C* Copy up to and including SEND
   30 READ (LEN,901) C66,MATD,MFD,MTD
      NS=NS+1
      IF(MTD.EQ.0) NS=99999
      WRITE(LOU,901) C66,MATD,MFD,MTD,NS
      IF(MTD.GT.0) GO TO 30
      RETURN
C*
  901 FORMAT(A66,I4,I2,I3,I5)
  904 FORMAT(6I11,I4,I2,I3,I5)
      END
