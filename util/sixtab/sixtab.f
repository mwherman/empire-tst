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
C-Version: Original code October 2000
C-M
C-M  Manual for Program SIXTAB
C-M  =========================
C-M
C-M  The SIXTAB program is an exrtension of the IAEA PrePro codes by
C-M  D.E.Cullen [1], which are designed to perform various operations
C-M  on the basic ENDF-formatted evaluated nuclear data files to
C-M  simplify the interpretation of the results.
C-M
C-M  The double differential data in an ENDF file MF 6 can be represented
C-M  in various forms, including Legendre coefficients, Kalback-Mann
C-M  formalism, pointwise representation, CM or Lab frame of reference.
C-M  This may make subsequent handling of the files rather difficult.
C-M  The purpose of SIXTAB is to convert MF 6 data into pointwise
C-M  representation in the Lab system (Law 7 ENDF format option).
C-M
C-M  WARNINGS:
C-M  1. The resulting file is generally not a substitute for the
C-M     original file due to the assumed limitations built into
C-M     the code. The number of angles in general may be insufficient
C-M     to accurately represent the original data.
C-M  2. The full complexity of the ENDF file MF 6  representation is
C-M     not coded at present:
C-M      - The data for the first particle only are processed at
C-M        present.
C-M      - Only Law 1 Legendre (LANG=1) or tabulated (LANG>10)
C-M        representations are allowed.
C-M
C-M  Instructions
C-M  ------------
C-M  Several forms of input are allowed:
C-M
C-M   A.  If the file "SIXTAB.INP" exists, the file is scanned for
C-M       input instructions. They may be given in the "old style"
C-M       or in a more flexible keyword-oriented mode.
C-M
C-M   A.  If the file "SIXTAB.INP" does not exist, and the first record
C-M       on the default input does NOT begin with the string "$* ",
C-M       the default input is the source ENDF file and the default
C-M       output is the reformatted output ENDF file.
C-M
C-M   B.  If the file "SIXTAB.INP" does not exist, and the first record
C-M       on the default input begins with the string "$* ", the
C-M       default input is read as a set of keyword-oriented input
C-M       instructions.
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
C* Default the number of angles
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
      WRITE(LTT,900) ' SIXTAB - Extract Data from ENDF files  '
      WRITE(LTT,900) ' -------------------------------------  '
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

   98 PRINT *,REC

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
C-D is converted into Law 7 format with NCS angles.
C-D WARNING - At present only the first section is converted.
C-
      PARAMETER      (MXMU=50, MXPL=20, MXRW=10000)
      CHARACTER*66    B66,C66
      DIMENSION RWO(MXRW),NBT(20),INT(20)
      DIMENSION AMU(MXMU),PLL(MXPL)
C*
      B66='                                 '
     1  //'                                 '
      PI=3.141592654
C* Define cosines for NCS angle intervals
      DO 110 I=1,NCS
      AMU(I)=COS(PI-(I-1)*PI/(NCS-1))
  110 CONTINUE
C*
      READ(C66,902) ZA,AWR, L1,LCT, NK, N2
C... Warning - only the first section is converted
      NK =1
      IER=0
C* Start the conversion process
      IF(LTT.GT.0) WRITE(LTT,960) MT0
      DO 800 IK=1,NK
C* Read Law-dependent structure
      LXE=1
      LXS=LXE+(MXRW-LXE)/2
      KX =MXRW-LXS
      CALL RDTAB1(LEN,ZAP,AWP,LIP,LAW, NR, NP, NBT,INT
     1           ,RWO(LXE),RWO(LXS),KX,IER)
      IZAP=NINT(ZAP)
      IF(IK.EQ.1) THEN
        IF(LAW.NE.1 .OR. IZAP.NE.1) THEN
C* Ignore section if not Law 1 and Neutron
          IF(LTT.GT.0) WRITE(LTT,961) LAW,IZAP
C         IER=1
          RETURN
        ELSE
          NS  =0
          LCT1=1
          CALL WRCONT(LOU,MAT0,MF0,MT0,NS,ZA,AWR,L1,LCT1,NK,N2)
        END IF
      END IF
C* Redefine Law for data representation
      LAW=7
C* Write the particle multiplicities to the output file
      CALL WRTAB1(LOU,MAT0,MF0,MT0,NS,ZAP,AWP,LIP,LAW
     1           , NR, NP, NBT,INT,RWO(LXE),RWO(LXS))
C* Read format specifications for outgoing energies
      CALL RDTAB2(LEN,C1,C2,LANG ,LEP ,NR,NE,NBT,INT,IER)
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
      IF(IER.NE.0) THEN
        STOP 'MF6LW7 ERROR - reading Energy data'
      END IF
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
C* If converting from lab, force unit base interpolation
        INTM=22
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
C* Begin loop over secondary particle energies
  200 IP=IP+1
      EP=RWO(LX1+NCYC*(IP-1))
      CSN=ACOS
      ELB=EP
      DRV=1
      IF (LCT.EQ.1) GO TO 212
C* Convert CM to Lab co-ordinate system
      AW1=AWR+1
      CLB=CSN
C*  Minimum lab cosine (= zero particle energy in C.M.)
      CMN=-1
      QQ=1-AW1*AW1*EP/EIN
      IF (QQ.GT.0) CMN=SQRT(QQ)
      IF (CLB.LT.CMN) CLB=CMN                                                           UP38.42   
C* Outgoing particle energy in the laboratory system
      QQ=EP-EIN*(1-CLB*CLB)/(AW1*AW1)
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
      ZERO=0
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
C... At present this option is not coded
      IF(LANG.EQ.2)
     1STOP 'MF6LW7 ERROR - Kalbach representation not coded'
C* Check parameters for consistency
      IF(LCT.NE.2)
     1STOP 'MF6LW7 ERROR - Kalbach requires CM reference frame'
      IF(NA.NE.1)
     1STOP 'MF6LW7 ERROR - Kalbach requires NA=1'
C...  CALL KALBAH(ELB,CSN,AWR,AWP...
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
C*
C* Distribution for one point processed
  400 CONTINUE
C* Enter energy point - skip if energy less then previous point
      IF(IP.GT.1 .AND. ELB.LE.RWO(LXE+JP-1)) GO TO 407
      RWO(LXE+JP)=ELB
      RWO(LXX+JP)=FMU*DRV
      JP=JP+1
  407 IF(JP.GE.KX) STOP 'MF6LW7 ERROR - MXRW capacity exceeded'
C* Continue with the secondary energy loop
      IF(IP.LT.NEP) GO TO 200
C*
C* All secondary particles energies processed
      IF(JP.EQ. 1 ) THEN
C* Check if single point
        RWO(LXE+JP)=RWO(LXE-1+JP)*2
        RWO(LXX+JP)=0
        JP=JP+1
      END IF
      C1 =0
      L1 =0
      L2 =0
      NRP=1
      NBT(1)=JP
      INT(1)=LEP
      CALL WRTAB1(LOU,MAT0,MF0,MT0,NS,C1,ACOS,L1,L2
     1           , NRP,JP,NBT,INT,RWO(LXE),RWO(LXX))
C* Continue with the cosine loop
  500 CONTINUE
C* Continue with the incident energy loop
  600 CONTINUE
C* Continue with the particle loop
  800 CONTINUE
C* Section processed - write SEND record
      CALL WRTEXT(LOU,MAT0,MF0,  0,NS,B66)
      RETURN
C*
  902 FORMAT(2F11.0,4I11,I4,I2,I3,I5)
  960 FORMAT(/' Converting MF=6, MT=',I3,' to Law 7 format')
  961 FORMAT(' WARNING - Can not process Law',I3,' for ZAP',I6)
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
      SUBROUTINE RDTAB1(LEN,C1,C2,L1,L2,N1,N2,NBT,INT,EN,XS,NMX,IER)
C-Title  : Subroutine RDTAB1
C-Purpose: Read an ENDF TAB1 record
      DIMENSION    NBT(100),INT(100)
      DIMENSION    EN(NMX), XS(NMX)
C*
      READ (LEN,902) C1,C2,L1,L2,N1,N2
      READ (LEN,903) (NBT(J),INT(J),J=1,N1)
      JP=N2
      IF(N2.GT.NMX) THEN
        JP=NMX
        IER=22
      END IF
      READ (LEN,904) (EN(J),XS(J),J=1,JP)
      RETURN
C*
  902 FORMAT(2F11.0,4I11)
  903 FORMAT(6I11)
  904 FORMAT(6F11.0)
      END
      SUBROUTINE RDTAB2(LEN,C1,C2,L1,L2,N1,N2,NBT,INT,IER)
C-Title  : Subroutine RDTAB2
C-Purpose: Read an ENDF TAB2 record
      DIMENSION    NBT(100),INT(100)
C*
      READ (LEN,902) C1,C2,L1,L2,N1,N2
      READ (LEN,903) (NBT(J),INT(J),J=1,N1)
      RETURN
C*
  902 FORMAT(2F11.0,4I11)
  903 FORMAT(6I11)
      END
      SUBROUTINE RDLIST(LEN,C1,XX,L1,L2,N1,N2,YY,NMX,IER)
C-Title  : Subroutine RDLIST
C-Purpose: Read an ENDF LIST record
      DIMENSION    YY(NMX)
C*
      READ (LEN,902) C1,XX,L1,L2,N1,N2
      JP=N1
      IF(N1.GT.NMX) THEN
        JP=NMX
        IER=22
      END IF
      READ (LEN,904) (YY(J),J=1,JP)
      RETURN
C*
  902 FORMAT(2F11.0,4I11)
  904 FORMAT(6F11.0)
      END
      SUBROUTINE WRTEXT(LIB,MAT,MF,MT,NS,REC)
C-Title  : WRTEXT Subroutine
C-Purpose: Write a text record to an ENDF file
      CHARACTER*66  REC
      NS=NS+1
      WRITE(LIB,40) REC,MAT,MF,MT,NS
      RETURN
   40 FORMAT(A66,I4,I2,I3,I5)
      END
      SUBROUTINE WRCONT(LIB,MAT,MF,MT,NS,C1,C2,L1,L2,N1,N2)
C-Title  : WRCONT Subroutine
C-Purpose: Write a CONT record to an ENDF file
      CHARACTER*11  BLN,REC(6)
      DATA BLN/'           '/
      DO 10 I=1,6
      REC(I)=BLN
   10 CONTINUE
      IF( (C1.EQ.0. .AND. C2.EQ.0.) .AND.
     1    (L1.EQ.0  .AND. L2.EQ.0 ) .AND.
     2    (N1.EQ.0  .AND. N2.EQ.0 ) ) GO TO 12
      CALL CHENDF(C1,REC(1))
      CALL CHENDF(C2,REC(2))
      WRITE(REC(3),20) L1
      WRITE(REC(4),20) L2
      WRITE(REC(5),20) N1
      WRITE(REC(6),20) N2
   12 NS=NS+1
      WRITE(LIB,40) (REC(J),J=1,6),MAT,MF,MT,NS
      RETURN
   20 FORMAT(I11)
   40 FORMAT(6A11,I4,I2,I3,I5)
      END
      SUBROUTINE WRTAB1(LIB,MAT,MF,MT,NS,C1,C2,L1,L2
     1                 ,NR,NP,NBT,INT,X,Y)
C-Title  : WRTAB1 Subroutine
C-Purpose: Write a TAB1 record to an ENDF file
      CHARACTER*11  BLN,REC(6)
      DIMENSION     NBT(1),INT(1),X(1),Y(1)
      DATA BLN/'           '/
C* First line of the TAB1 record
      CALL CHENDF(C1,REC(1))
      CALL CHENDF(C2,REC(2))
      WRITE(REC(3),42) L1
      WRITE(REC(4),42) L2
      WRITE(REC(5),42) NR
      WRITE(REC(6),42) NP
      NS=NS+1
      WRITE(LIB,40) (REC(J),J=1,6),MAT,MF,MT,NS
C* Write interpolation data
      N =0
   20 I =0
   22 REC(I+1)=BLN
      REC(I+2)=BLN
      IF(N.GE.NR) GO TO 24
      N =N+1
      WRITE(REC(I+1),42) NBT(N)
      WRITE(REC(I+2),42) INT(N)
   24 I =I +2
      IF(I.LT.6) GO TO 22
      NS=NS+1
      WRITE(LIB,40) (REC(J),J=1,6),MAT,MF,MT,NS
      IF(N.LT.NR) GO TO 20
C* Loop for all argument&function pairs
      N =0
   30 I =0
   32 REC(I+1)=BLN
      REC(I+2)=BLN
      IF(N.GE.NP) GO TO 34
      N =N+1
      CALL CHENDF(X(N),REC(I+1))
      CALL CHENDF(Y(N),REC(I+2))
   34 I =I+2
      IF(I.LT.6) GO TO 32
      NS=NS+1
      WRITE(LIB,40) (REC(J),J=1,6),MAT,MF,MT,NS
      IF(N.LT.NP) GO TO 30
      RETURN
   40 FORMAT(6A11,I4,I2,I3,I5)
   42 FORMAT(I11)
      END
      SUBROUTINE WRTAB2(LIB,MAT,MF,MT,NS,C1,C2,L1,L2
     1                 ,NR,NZ,NBT,INT)
C-Title  : WRTAB2 Subroutine
C-Purpose: Write a TAB2 record to an ENDF file
      CHARACTER*11  BLN,REC(6)
      DIMENSION     NBT(1),INT(1)
      DATA BLN/'           '/
C* First line of the TAB2 record
      CALL CHENDF(C1,REC(1))
      CALL CHENDF(C2,REC(2))
      WRITE(REC(3),42) L1
      WRITE(REC(4),42) L2
      WRITE(REC(5),42) NR

      WRITE(REC(6),42) NZ
      NS=NS+1
      WRITE(LIB,40) (REC(J),J=1,6),MAT,MF,MT,NS
C* Write interpolation data
      N =0
   20 I =0
   22 REC(I+1)=BLN
      REC(I+2)=BLN
      IF(N.GE.NR) GO TO 24
      N =N+1
      WRITE(REC(I+1),42) NBT(N)
      WRITE(REC(I+2),42) INT(N)
   24 I =I +2
      IF(I.LT.6) GO TO 22
      NS=NS+1
      WRITE(LIB,40) (REC(J),J=1,6),MAT,MF,MT,NS
      IF(N.LT.NR) GO TO 20
      RETURN
   40 FORMAT(6A11,I4,I2,I3,I5)
   42 FORMAT(I11)
      END
      SUBROUTINE WRLIST(LIB,MAT,MF,MT,NS,C1,C2,L1,L2,NPL,N2,BN)
C-Title  : WRLIST Subroutine
C-Purpose: Write a LIST record to an ENDF file
      CHARACTER*11  BLN,REC(6)
      DIMENSION     BN(1)
      DATA BLN/'           '/
C* First line of the TAB2 record
      CALL CHENDF(C1,REC(1))
      CALL CHENDF(C2,REC(2))
      WRITE(REC(3),42) L1
      WRITE(REC(4),42) L2
      WRITE(REC(5),42) NPL
      WRITE(REC(6),42) N2
      NS=NS+1
      WRITE(LIB,40) (REC(J),J=1,6),MAT,MF,MT,NS
      IF(NPL.EQ.0) RETURN
C* Write data
      N =0
   20 I =0
   22 REC(I+1)=BLN
      IF(N.GE.NPL) GO TO 24
      N =N+1
      CALL CHENDF(BN(N),REC(I+1))
   24 I =I +1
      IF(I.LT.6) GO TO 22
      NS=NS+1
      WRITE(LIB,40) (REC(J),J=1,6),MAT,MF,MT,NS
      IF(N.LT.NPL) GO TO 20
      RETURN
   40 FORMAT(6A11,I4,I2,I3,I5)
   42 FORMAT(I11)
      END
      SUBROUTINE CHENDF(FF,CH)
C-Title  : CHENDF Subroutine
C-Purpose: Pack value into 11-character string
      CHARACTER*1  SN
      CHARACTER*11 CH
      CH=' 0.00000+00'
      FA=ABS(FF)
      IA=0
   20 IF(FA.LT.1.0E-30 ) RETURN
      IF(FA.LT.9.999950) GO TO 40
      FA=FA*0.1
      IA=IA+1
      GO TO 20
   40 IF(FA.GE.0.999995) GO TO 50
      FA=FA*10.
      IA=IA-1
      GO TO 40
   50 SN='+'
      IF(IA.LT.0) THEN
        SN='-'
        IA=-IA
      END IF
      IF(FF.LT.0) FA=-FA
      WRITE(CH,80) FA,SN,IA
      RETURN
   80 FORMAT(F8.5,A1,I2.2)
      END

