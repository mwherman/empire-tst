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
C-M
C-M  Manual for Program SIXTAB
C-M  =========================
C-M
C-M  The SIXTAB program belongs to a group of codes, which form
C-M  a supplement to the IAEA sequence of ENDF Pre-Processing codes
C-M  (PrePro) by D.E.Cullen [1], which are designed to perform
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
C-M        Data for Laws 2 and 3 are not converted. They are copied
C-M        as they are.
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
      PARAMETER    (NCM=2)
      CHARACTER*66  C66,CMT(NCM)
      CHARACTER*40  FLIN,FLEN,FLOU
      LOGICAL       EXST
C*
      DATA FLIN/'SIXTAB.INP'/
     2     FLEN/'ENDF.DAT'/
     3     FLOU/'ENDFNEW.DAT'/
      DATA LIN,LEN,LOU,LKB,LTT/ 1, 2, 3, 5, 6 /
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
      CALL SIXINP(LIN,LTT,FLEN,FLOU,NSC,IER)
      CLOSE(UNIT=LIN)
      IF(IER.EQ.0) GO TO 16
C* Remedial action if fatal error on input is encountered:
C* Print a warning to default output and try reading default input
   15 CALL SIXINP(LKB,LTT,FLEN,FLOU,NSC,IER)
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
      READ (LEN,901) C66,MATH,MFH,MTH
   17 WRITE(LOU,901) C66,MATH,MFH,MTH
C* Edit the comment section to identify changes
      CMT(1)='**********************'//
     1       ' SIXTAB Version 00/10 '//
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
C* Copy up to MF6
      READ (LEN,901) C66,MATH,MFH,MTH
   30 WRITE(LOU,901) C66,MATH,MFH,MTH
      READ (LEN,901) C66,MATH,MFH,MTH
      IF     (MATH.EQ.0) THEN
        WRITE(LOU,901) C66,MATH,MFH,MTH
        GO TO 20
      ELSE IF(MATH.LT.0) THEN
        GO TO 80
      END IF
      IF(MFH.NE.6) GO TO 30
C* Process the ENDF file MF6
  40  CONTINUE
      CALL MF6LW7(LEN,LOU,LTT,NCS,C66,MATH,MFH,MTH,IER)
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
   50 WRITE(LOU,901) C66,MATH,MFH,MTH
      READ (LEN,901) C66,MATH,MFH,MTH
      IF     (MATH.EQ.0) THEN
        WRITE(LOU,901) C66,MATH,MFH,MTH
        GO TO 20
      ELSE IF(MATH.LT.0) THEN
        GO TO 80
      END IF
      GO TO 50
C* File processing finished
   80 WRITE(LOU,901) C66,MATH,MFH,MTH
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
  991 FORMAT(' Neutron distributions in MF6 con',
     2       'verted to Law 7 using',I4,' cosines')
      END
      SUBROUTINE SIXINP(LIN,LTT,FLEN,FLOU,NSC,IER)
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
      SUBROUTINE MF6LW7(LEN,LOU,LTT,NCS,C66,MAT0,MF0,MT0,IER)
C-Title  : Subroutine MF6LW7
C-Purpose: Adapted fix6 routine of NJOY
C-Dscription:
C-D A section of file 6 using legendre or tabulated angular distributions
C-D is converted into Law 7 format with NCS angles. Intermediate results
C-D are written to scratch file on unit LSC.
C-
      PARAMETER      (MXMU=50, MXPL=20, MXRW=10000)
      CHARACTER*66    B66,C66
      CHARACTER*40    FLSC
      CHARACTER*8     CLANG
      DIMENSION RWO(MXRW),NBT(20),INT(20)
      DIMENSION AMU(MXMU),PLL(MXPL)
C*
      DATA PI /3.141592654/
      DATA ANEU/1.008665/
      DATA APRO/1.007273/
      DATA ADEU/2.013558/
      DATA ATRI/3.015505/
      DATA AHE3/3.014960/
      DATA AHE4/4.001505/
C* Scratch file unit number and name
      DATA LSC/19/
      DATA FLSC/'SIXTAB.TMP'/
C*
      B66='                                 '
     1  //'                                 '
      ZERO=0
      NS  =0
      OPEN (UNIT=LSC,FILE=FLSC,STATUS='UNKNOWN')
C* Assume the projectile is neutron
      IZPR=1
C* Define cosines for NCS angle intervals
      DO 110 I=1,NCS
      AMU(I)=COS(PI-(I-1)*PI/(NCS-1))
  110 CONTINUE
C*
      READ(C66,902) ZA,AWR, L1,LCT, NK, N2
      IZA =NINT(ZA)
      IER =0
C* Co-ordinate system flag: save original and preset output
      LCT0=LCT
      LCT1=LCT
C* Start the conversion process
      IF(LTT.GT.0) WRITE(LTT,960) MT0
      DO 800 IK=1,NK
C* Begin loop over all outgoing particles
      LXE=1
      LXS=LXE+(MXRW-LXE)/2
      KX =MXRW-LXS
      CALL RDTAB1(LEN,ZAP,AWP,LIP,LAW0, NR, NP, NBT,INT
     1           ,RWO(LXE),RWO(LXS),KX,IER)
      IZAP=NINT(ZAP)
      LAW =LAW0
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
      IF(IK.EQ.1) THEN
C* Check that neutron is the first particle
        IF(IZAP.NE.1) THEN
          IF(LTT.GT.0)
     1    WRITE(LTT,900) ' WARNING - Neutron not first particle   '
        END IF
C* Write the header record before the first particle data
        CALL WRCONT(LOU,MAT0,MF0,MT0,NS,ZA,AWR,L1,LCT1,NK,N2)
      END IF
C* Write the particle multiplicities to the output file
      CALL WRTAB1(LOU,MAT0,MF0,MT0,NS,ZAP,AWP,LIP,LAW
     1           , NR, NP, NBT,INT,RWO(LXE),RWO(LXS))
C*
      IF     (LAW0.EQ.1) THEN
C* Proceed to process Law 1 data
        GO TO 130
C* Copy other format representations
      ELSE IF(LAW0.EQ.0 .OR. LAW0.EQ.3 .OR. LAW0.EQ.4) THEN
C*      No action necessary for Law 0
        GO TO 800
      ELSE IF(LAW0.EQ.2 .OR. LAW0.EQ.5) THEN
C*      Copy the data for Law 2,5
        IF(LTT.GT.0)
     1    WRITE(LTT,900) ' WARNING - Conversion not done for Law  ',LAW0
        CALL RDTAB2(LEN,C1,C2,L1,L2,NR,NE,NBT,INT,IER)
        CALL WRTAB2(LOU,MAT0,MF0,MT0,NS,C1,C2,L1,L2
     1             ,NR,NE,NBT,INT)
        NMX=MXRW
        DO 120 IE=1,NE
        CALL RDLIST(LEN,C1,C2,L1,L2,N1,N2,RWO,NMX,IER)
        CALL WRLIST(LOU,MAT0,MF0,MT0,NS,C1,C2,L1,L2,N1,N2,RWO)
  120   CONTINUE
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
      CALL WRTAB2(LOU,MAT0,MF0,MT0,NS,C1,C2,L1,L2
     1           ,NR,NE,NBT,INT)
C* Begin loop over incident particle energies
      LX1=1
      DO 600 I=1,NE
C* Read in the data for this energy
      NMX=MXRW-LX1
      CALL RDLIST(LEN,C1,EIN,ND,NA,NW,NEP,RWO(LX1),NMX,IER)
C...
      IF(ND.GT.0) THEN
        PRINT *,'SIXTAB ERROR - Discrete particle energies not coded'
        STOP 'SIXTAB ERROR - No coding available'
      END IF
C...
      IF(IER.NE.0)
     1  STOP 'MF6LW7 ERROR - reading Energy data'
      IF(LANG.EQ.2 .AND. NA.NE.1)
     1  STOP 'MF6LW7 ERROR - Kalbach requires NA=1'
      LXE =LX1+NW
      LXX =LX1+(MXRW-LXE)/2
      KX  =MXRW-LXX
      NCYC=NA+2
      NL  =NA+1
      NMU =NCS
      IF( NL.LE.1) NMU=2
      IF(LCT.EQ.1) THEN
C* Linear interpolation between cosines
        INTM=2
      ELSE
C* If converting from lab, force unit base interpolation for cosines
C... But CHECKR doesn't like it (???)
C...    INTM=22
        INTM=2
      END IF
C* Write the TAB2 record for the cosine grid
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
      EP0=0
  200 IP=IP+1
      EP=RWO(LX1+NCYC*(IP-1))
C* Check that outgoing particle energies are monotonic increasing
      IF(EP.LT.EP0) THEN
        WRITE(LTT,964) IZAP,EIN,EP
        GO TO 408
      END IF
      EP0=EP
      CSN=ACOS
      ELB=EP
      DRV=1
      IF (LCT.EQ.1 .OR. IZAP.NE.1) GO TO 212
C* Convert CM to Lab co-ordinate system assuming incident neutrons
C* WARNING: applied to outgoing neutrons only
      AW1=AWR+1
      CLB=CSN
C* Minimum lab cosine
      CMN=-1
C...  QQ=1-AW1*AW1*EP/EIN
      QQ=1-DBLE(AW1*AW1*EP/EIN)
      IF (QQ.GT.0) CMN=-SQRT(QQ)
      IF (CLB.LT.CMN) CLB=CMN
C* Outgoing particle energy in the laboratory system
C...  QQ=EP-EIN*(1-CLB*CLB)/(AW1*AW1)
      QQ=EP-EIN*(1-DBLE(CLB)**2)/(AW1*AW1)
      IF (QQ.LT.0) QQ=0
      ELB=CLB*SQRT(EIN)/AW1+SQRT(QQ)
      ELB=ELB*ELB
C* Calculate corresponding cosine in the CM system
      IF (EP.GT.0.) CSN=CLB*SQRT(ELB/EP)-SQRT(EIN/EP)/AW1
C* Check the limits
      QQ=-1
      IF (CSN.LT.QQ) CSN=QQ
      QQ=1
      IF (CSN.GT.QQ) CSN=QQ
C* Add jacobian for the CM-to-Lab transformation
      IF (EP.NE.ZERO) DRV=DRV*SQRT(ELB/EP)
  212 IF (LANG.GT.10) GO TO 300
      IF (LANG.EQ. 2) GO TO 240
C*
C* Calculate the probability from Legendre polynomials
      CALL PLNLEG(CSN,PLL,NA)
      FMU=0
      DO 220 LL=1,NL
      FMU=FMU+0.5*(2*LL-1)*PLL(LL)*RWO(LX1+NCYC*(IP-1)+LL)
  220 CONTINUE
      GO TO 400
C*
C* Calculate the probability from Kalbach-Mann representation
  240 CONTINUE
      SS=RWO(LX1+NCYC*(IP-1)+1)
      RR=RWO(LX1+NCYC*(IP-1)+2)
      AA=BACH(IZPR,IZAP,IZA,EIN,EP)
      FMU=SS*AA*(COSH(AA*CSN)+RR*SINH(AA*CSN))/(2*SINH(AA))
      GO TO 400
C*
C* Calculate the probability from pointwise representation
  300 MMU=(NCYC-1)/2
      DO 302 KMU=1,MMU-1
      LL=LX1+5+2*KMU+NCYC*(IP-1)
      IF (CSN.GE.RWO(LL).AND.CSN.LE.RWO(LL+2)) GO TO 303
  302 CONTINUE
      FMU=0.
      GO TO 400
  303 INTI=LANG-10
      CALL INTRPE(RWO(LL),RWO(LL+1),RWO(LL+2),RWO(LL+3),CSN,FMU,INTI)
C* Processing of distribution for one outgoing energy point completed
  400 CONTINUE
C*
C* Enter energy point - skip if energy less then previous point
      IF(IP.GT.1 .AND. ELB.LT.RWO(LXE+JP-1)) GO TO 407
      FMU1=FMU*DRV
      RWO(LXE+JP)=ELB
      RWO(LXX+JP)=FMU1
      JP=JP+1
C* Integrate over outgoing particle energy for normalisation
      IF(JP.GT.1) THEN
        IF(LEP.EQ.2) THEN
          FMUJ=0.5*(FMU1+FMU0)
        ELSE
          FMUJ=FMU0
        END IF
        SEN=SEN + FMUJ*(ELB-ELB0)
      END IF
      FMU0=FMU1
      ELB0=ELB
  407 IF(JP.GE.KX) STOP 'MF6LW7 ERROR - MXRW capacity exceeded'
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
        IF(LEP.EQ.2) THEN
          FMUJ=0.5*FMU1
        ELSE
          FMUJ=FMU1
        END IF
        SEN=SEN + FMUJ*ELB*EPS
      END IF
      C1 =0
      L1 =0
      L2 =0
      NRP=1
      NS0=0
      NBT(1)=JP
      INT(1)=LEP
      CALL WRTAB1(LSC,MAT0,MF0,MT0,NS0,C1,ACOS,L1,L2
     1           , NRP,JP,NBT,INT,RWO(LXE),RWO(LXX))
C* Add contribution to the integral
      IF(IMU.GT.1) THEN
        JNTM=INTM-10*(INTM/10)
        IF(JNTM.EQ.2) THEN
          SENJ=0.5*(SEN+SEN0)
        ELSE
          SENJ=SEN0
        END IF
        SIG = SIG + SENJ*(ACOS-ACOS0)
      END IF
      ACOS0= ACOS
      SEN0 = SEN
C* Continue with the cosine loop
  500 CONTINUE
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
C* Continue with the particle loop
  800 CONTINUE
C* Section processed - write SEND record
      CALL WRTEXT(LOU,MAT0,MF0,  0,NS,B66)
      CLOSE(UNIT=LSC)
      RETURN
C*
  900 FORMAT(A40,I6)
  902 FORMAT(2F11.0,4I11,I4,I2,I3,I5)
  960 FORMAT(/' Converting MF=6, MT=',I3,' to Law 7 format')
  961 FORMAT(' SIXTAB ERROR - Can not process Law',I3,' for ZAP',I6)
  962 FORMAT(12X,A8,' Representation for particle IZAP',I6)
  964 FORMAT(' SIXTAB WARNING - Format error ZAP,Ein,Ep',I6,1P,2E10.3)
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
      SUBROUTINE INTRPE(X1,Y1,X2,Y2,X,Y,INT)
C-Title  : Subroutine INTRPE
C-Purpose: Interpolate function between two point
      ZERO=0
C*
      IF (Y1.EQ.Y2) GO TO 100
      IF ( X.EQ.X1) GO TO 100
      GO TO (100,200,300,400,500),INT
C* Histogram
  100 Y=Y1
      RETURN
C* Linear
  200 Y=Y1+(X-X1)*(Y2-Y1)/(X2-X1)
      RETURN
C* Linear in ln(x)
  300 IF (X1*X2.LE.ZERO) GO TO 100
      Y=Y1+LOG(X/X1)*(Y2-Y1)/LOG(X2/X1)
      RETURN
C* Linear in ln(y)
  400 IF (Y1*Y2.LE.ZERO) GO TO 100
      Y=Y1*EXP((X-X1)*LOG(Y2/Y1)/(X2-X1))
      RETURN
C* Linear in ln(y), ln(x)
  500 IF (Y1*Y2.LE.ZERO) GO TO 100
      IF (X1*X2.LE.ZERO) GO TO 100
      Y=Y1*EXP(LOG(X/X1)*LOG(Y2/Y1)/LOG(X2/X1))
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
      READ (LEN,901) C66,MATD,MFD,MTD
      IF(MATD.LT.0) RETURN
      IF(MFD.NE.1 .AND. MTD.NE.451) THEN
        IER=1
        RETURN
      END IF
C* Copy the first three records
   20 IC =IC+1
      WRITE(LOU,901) C66,MATD,MFD,MTD
      READ (LEN,901) C66,MATD,MFD,MTD
      IF(IC.LT.3) GO TO 20
C* Correct the length of the comments section
      READ (C66(45:55),904) NDC
      WRITE(C66(45:55),904) NDC+NCT
      WRITE(LOU,901) C66,MATD,MFD,MTD
C* Copy original comments
      DO 22 IC=1,NDC
      READ (LEN,901) C66,MATD,MFD,MTD
      WRITE(LOU,901) C66,MATD,MFD,MTD
   22 CONTINUE
C* Add NCT comments from CMT
      DO 24 IC=1,NCT
      WRITE(LOU,901) CMT(IC),MATD,MFD,MTD
   24 CONTINUE
C* Copy up to and including SEND
   30 READ (LEN,901) C66,MATD,MFD,MTD
      WRITE(LOU,901) C66,MATD,MFD,MTD
      IF(MTD.GT.0) GO TO 30
      RETURN
C*
  901 FORMAT(A66,I4,I2,I3,I5)
  904 FORMAT(6I11,I4,I2,I3,I5)
      END
