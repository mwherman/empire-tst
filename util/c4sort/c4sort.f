      PROGRAM C4SORT
C-Title  : Program C4SORT
C-Purpose: Sort EXFOR file in C4 format
C-Author : Andrej Trkov                        e-mail: A.Trkov@iaea.org
C-A        International Atomic Energy Agency     tel: +43 1 2600 21712
C-A        Nuclear Data Section                   fax: +43 1 2600 7
C-A        Wagramerstrasse 5
C-A        P.O.Box 100
C-A        A-1400 Vienna
C-A        AUSTRIA
C-Version: Original code March 2001
C-V  01/05 Increase MXEN from 2000 to 5000 (A.Trkov)
C-M
C-M  Program C4SORT Users' Guide
C-M  ===========================
C-M
C-M  The C4SORT program belongs to the ENDVER group of codes in
C-M  support of nuclear data evaluation. The ENDVER package
C-M  requires the the IAEA sequence of ENDF Pre-Processing codes
C-M  (PrePro) by D.E.Cullen [1], which are designed to perform
C-M  various operations on the basic ENDF-formatted evaluated nuclear
C-M  data files. They provide basic tools to the user for simple,
C-M  machine-independent interpretation and visualisation of the
C-M  contents of the evaluated nuclear data files.
C-M
C-M  The EXFOR data in computational C4 format, which are generated
C-M  by the X4TOC4 code, are transferred to output in the order in
C-M  which they appear in the source EXFOR file. For plotting
C-M  purposes (for example, with PLOTC4) it is advantageous if the
C-M  data in computational file are sorted by MAT/MF/MT numbers
C-M  and the incident particle energy. The C4SORT code performs
C-M  this task.
C-M
C-M  Instructions
C-M  ------------
C-M  Several forms of input are allowed:
C-M
C-M   A.  If the file "C4SORT.INP" exists, the file is scanned for
C-M       input instructions. They may be given in the "old style"
C-M       or in the more flexible keyword-oriented style.
C-M
C-M   B.  If the file "C4SORT.INP" does not exist, and the first record
C-M       on the default input does NOT begin with the string "$* ",
C-M       then the default input is assumed to be the source EXFOR file
C-M       and the default output is the sorted output EXFOR file.
C-M       This allows the use of "pipes" on Unix. On PC-Dos it is
C-M       compiler-dependent.
C-M
C-M   C.  If the file "C4SORT.INP" does not exist, and the first record
C-M       on the default input begins with the string "$* ", the
C-M       default input is read as a set of keyword-oriented input
C-M       instructions. This option is useful to divert the default
C-M       input to a real input file from command line.
C-M       WARNING: To use this feature make sure that C4SORT.INP file
C-M                does not exist on the default directory.
C-M
C-M  Old-style input instructions format:
C-M  Two records are expected from input:
C-M  - Name of the source EXFOR file in C4 format.
C-M  - Name of the sorted EXFOR file to be written.
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
C-M  '$* C4SORT ' The main keyword of the C4SORT program. All
C-M               preceeding input is ignored.
C-M  '   FLC4IN ' Name of the source ENDF input file is given in
C-M               columns 11-50.
C-M  '   FLC4OU ' Name of the target ENDF output file is given in
C-M               columns 11-50.
C-M  '   ALIAS  ' Equivalent ZA/MF/MT identifiers can be defined;
C-M               for example, to force experimental data measured
C-M               for a natural element to apply to an isotope.
C-M               Additional information is read from columns:
C-M                11-20  ZA1 designation on the source file.
C-M                21-25  MF1 designation on the source file.
C-M                26-30  MT1 designation on the source file.
C-M                31-40  ZA2 designation on the sorted file.
C-M                41-45  MF2 designation on the sorted file.
C-M                46-50  MT2 designation on the sorted file.
C-M               WARNING: Material substitution for comparison
C-M                        is strictly incorrect and potentially
C-M                        misleading. For this reason the use of
C-M                        ALIAS has been restricted in the sense
C-M                        that the user requires to define
C-M                        explicitly the ZA/MF/MT numbers to be
C-M                        equivalenced (i.e. no implicit assumptions
C-M                        are allowed, all entries ZA1,MF1,MT1 and
C-M                        ZA2,MF2,MT2 must be specified for the
C-M                        conversion to take place).
C-M               NOTE: This option is available only in the
C-M                     keyword-oriented input.
C-M  '   ENDC4S ' This keyword signals the end of C4SORT input. All
C-M               instructions that follow are ignored.
C-M
C-M  File units:
C-M   Name No. Description:
C-M    LIN  1  Input instructions file, if not read from default input.
C-M    LC4  2  Source EXFOR data file in C4 format, if not read from
C-M            the default input.
C-M    LOU  3  Sorted EXFOR data file in C4 format (output).
C-M    LSC  4  C4SORT.TMP scratch file.
C-M    LKB  5  Default input.
C-M    LTT  6  Default output.
C-
      PARAMETER    (MXEN=5000,LCH=30,MXAL=200)
      CHARACTER*132 REC
      CHARACTER*40  FLIN,FLC4,FLOU,FLSC
      CHARACTER*35  REF
      CHARACTER*20  ZAMT
      CHARACTER*14  ALIA(2,MXAL)
      CHARACTER*(LCH)  ENT(MXEN)
      CHARACTER*11  EIN
      LOGICAL       EXST
      DIMENSION     IDX(MXEN),LNE(MXEN),ID1(MXEN),ID2(MXEN)
C* Preset the ZA and reference strings
      DATA REF/'###################################'/
      DATA ZAMT/'####################'/
C* Files and logical file unit numbers
      DATA FLIN/'C4SORT.INP'/
     2     FLC4/'C4.DAT'/
     3     FLOU/'C4.DAT'/
     4     FLSC/'C4SORT.TMP'/
      DATA LIN,LC4,LOU,LSC,LKB,LTT/ 1, 2, 3, 4, 5, 6 /
C*
C* Check for the existence of the input file
      INQUIRE(FILE=FLIN,EXIST=EXST)
      IF(EXST) GO TO 14
C* C4SORT Input does not exist - try interpreting default input
      IER=1
      GO TO 15
C* C4SORT Input exists -Process the input file
   14 OPEN(UNIT=LIN,FILE=FLIN,STATUS='OLD')
      IER=0
      NALI=MXAL
      CALL C4SINP(LIN,LTT,FLC4,FLOU,NALI,ALIA,IER)
      CLOSE(UNIT=LIN)
      IF(IER.EQ.0) GO TO 16
C* Remedial action if fatal error on input is encountered:
C* Print a warning to default output and try reading default input
   15 NALI=MXAL
      CALL C4SINP(LKB,LTT,FLC4,FLOU,NALI,ALIA,IER)
      FLIN='Keyboard'
      IF(IER.EQ.0) GO TO 16
C* Try input/output ENDF files from default input/output
      LC4=LKB
      LOU=LTT
      LTT=0
      MATH=0
      GO TO 17
C*
C* Input instructions processed
   16 OPEN (UNIT=LC4,FILE=FLC4,STATUS='OLD')
      OPEN (UNIT=LSC,FILE=FLSC,STATUS='UNKNOWN')
C* Write the banner to output
      WRITE(LTT,900) ' C4SORT - Sort data in a C4 file        '
      WRITE(LTT,900) ' -------------------------------        '
      WRITE(LTT,900)
      WRITE(LTT,900) '              Input instructions file : ',FLIN
      WRITE(LTT,900) '       Source EXFOR file in C4 format : ',FLC4
      WRITE(LTT,900) '       Sorted EXFOR file in C4 format : ',FLOU
      WRITE(LTT,900)
   17 CONTINUE
C*
C* Begin processing the source file
      NEN=0
      JLN=0
C* Copy records to scratch until the reference changes
   20 READ (LC4,901,END=200) REC
C* Change any alias ZA/MF/MT designations
      CALL CALIAS(NALI,ALIA,REC)
      WRITE(LSC,901) REC
      JLN=JLN+1
      IF(REC(1:20).EQ.ZAMT .AND. REC(98:132).EQ.REF) GO TO 20
C* Reference changed - decode the incident particle energy
      READ (REC(23:31),931) FIN
      WRITE(EIN,932) FIN
C* Save the entry data
      IF(NEN.GE.MXEN) STOP 'C4SORT ERROR - MXEN Limit exceeded'
      NEN=NEN+1
      LNE(NEN)=JLN
      ENT(NEN)=REC(1:19)//EIN
      REF =REC(98:132)
      ZAMT=REC(1:20)
C* Proceed to next record
      GO TO 20
C* Source C4 file processed
  200 WRITE(LTT,921) ' Number of EXFOR entries              : ',NEN
      WRITE(LTT,921) ' Total number of records              : ',JLN
      CLOSE(UNIT=LC4)
C*
C* Sort the entries
      CALL SRTTCH(NEN,LCH,ID1,ID2,ENT)
      WRITE(LTT,921) ' Indices sorted - begin writing file    '
C*
C* Write the sorted EXFOR file from scratch
      OPEN (UNIT=LOU,FILE=FLOU,STATUS='UNKNOWN')
      REWIND LSC
      KLN=JLN
      JLN=0
      DO 420 I=1,NEN
      ILN=ID2(I)
      ILN=LNE(ILN)
      IF(JLN.GT.ILN) THEN
        REWIND LSC
        JLN=0
      END IF
C* Skip up to ILN-th record
  414 IF(JLN.EQ.ILN) GO TO 416
      READ (LSC,901) REC
      JLN=JLN+1
      GO TO 414
C* Copy records for the same reference to output
  416 READ (REC,941) MF,XS
C* Suppress printout of negative cross sections
      IF(MF.NE.3 .OR. XS.GT.0) WRITE(LOU,901) REC
      IF(JLN.GE.KLN) GO TO 420
      REF=REC(98:132)
      READ (LSC,901) REC
      JLN=JLN+1
      IF(REF.EQ.REC(98:132)) GO TO 416
C* Proceed to the next data set
  420 CONTINUE
C*
C* All data processed
  600 CLOSE(UNIT=LOU)
      STOP 'C4SORT Completed'
C*
  900 FORMAT(2A40)
  901 FORMAT(A132)
  921 FORMAT(A40,I6)
  931 FORMAT(BN,F9.0)
  932 FORMAT(F11.1)
  941 FORMAT(11X,I4,25X,F9.0)
      END
      SUBROUTINE C4SINP(LIN,LTT,FLC4,FLOU,NALI,ALIA,IER)
C-Title  : Subroutine C4SINP
C-Purpose: Process input instructions for C4SORT
      PARAMETER    (MXKW=10)
      CHARACTER*40  FLC4,FLOU
      CHARACTER*14  ALIA(2,1)
      CHARACTER*10  KWRD(MXKW),REC(8),C10
C*
      DATA KWRD
     1/'$* C4SORT ','   FLC4IN ','   FLC4OU ','          ','          '
     1,'   ALIAS  ','          ','          ','          ','   ENDC4S '/
C* Set upper limit on alias and preset actual number
      MXAL=NALI
      NALI=0
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
      FLC4=REC(1)//REC(2)//REC(3)//REC(4)
      READ (LIN,901,ERR=800) REC
      FLOU=REC(1)//REC(2)//REC(3)//REC(4)
      RETURN
C*
C* Keyword-oriented input
C*
C* Skip to the main C4SORT input keyword
   80 READ (LIN,901,ERR=800) REC
   81 IF(REC(1).NE.KWRD(1)) GO TO 80
      IER=0
C* Read C4SORT input
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
C* '$* C4SORT '
  110 GO TO 90
C* '   FLC4IN '
  120 FLC4=REC(2)//REC(3)//REC(4)//REC(5)
      GO TO 90
C* '   FLC4OU '
  130 FLOU=REC(2)//REC(3)//REC(4)//REC(5)
      GO TO 90
C* Unused keywords
  140 CONTINUE
  150 CONTINUE
      GO TO 90
C* '   ALIAS  '
  160 NALI=NALI+1
      IF(NALI.GT.MXAL) STOP 'C4SINP ERROR - MXAL Limit exceeded'
      READ (REC(2),902) IZA1
      READ (REC(3),961) MF1,MT1
      READ (REC(4),902) IZA2
      READ (REC(5),961) MF2,MT2
      WRITE(ALIA(1,NALI),962) IZA1,MF1,MT1
      WRITE(ALIA(2,NALI),962) IZA2,MF2,MT2
      GO TO 90
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
  961 FORMAT(BN,2I5)
  962 FORMAT(I6,I4,I4)
      END
      SUBROUTINE CALIAS(NALI,ALIA,REC)
C-Title  : Subroutine CALIAS
C-Purpose: Process alias ZA designations in EXFOR in C4 format
C-Description:
C-D A record of EXFOR data in C4 format is checked against a
C-D list of NALI alias strings in ALIA of ZA/MF/MT reaction
C-D designators. If a matching set of 14 character in ALIA(1,i)
C-D is found, it is replaced by ALIA(2,i). Note that the "state"
C-D designator of the target nuclide is not checked.
C-
      CHARACTER*132 REC
      CHARACTER*14  ALIA(2,1),ALII,TEST
      IF(NALI.LT.1) RETURN
      TEST=REC(6:19)
      DO 20 I=1,NALI
      ALII=ALIA(1,I)
      IF(ALII( 1: 6).NE.TEST( 1: 6)) GO TO 20
      IF(ALII( 8:10).NE.TEST( 8:10).AND.ALII( 8:10).NE.'   ' ) GO TO 20
      IF(ALII(11:14).NE.TEST(11:14).AND.ALII(11:14).NE.'    ') GO TO 20
        ALII=ALIA(2,I)
        TEST( 1: 6)=ALII( 1: 6)
        IF(ALII( 8:10).NE.'   ' ) TEST( 8:10)=ALII( 8:10)
        IF(ALII(11:14).NE.'    ') TEST(11:14)=ALII(11:14)
        REC(6:19)=TEST
        RETURN
   20 CONTINUE
      RETURN
      END
      SUBROUTINE SRTTCH(N,K,L,M,X)
C-Title  : SRTTCH subroutine
C-Purpose: Perform a sort in ascending order by Tree sort method
C-Description:
C-D Sort in ascending order the vector of N characters of length K
C-D stored in array X. The actual entries in X remain unaffected,
C-D but on exit:
C-D   L (integer array) contains the relative positions of the
C-D     consecutive array entries in a sorted sequence
C-D     (i-th entry in X is placed L(i)-th in the sorted sequence).
C-D   M (integer array) contains the addresses of the consecutive 
C-D     array entries to produce a sorted sequence
C-D     (i-th entry in the sorted sequence is M(i)-th entry in X).
C-D The coding has been translated from Basic by A.Trkov. The
C-D differences from the original program are the following:
C-D  - the definitions of arrays L and M has been redefined for
C-D    convenience.
C-D NOTE: This routine is complementary to the SRTTRE routine,
C-D       which operates on real numbers.
C-Reference: Moj Mikro (1989)
C... Coding marked with "..." does not work with g77 on Linux (???)
C...  CHARACTER X*(K)
C...  DIMENSION L(1),M(1),X(1)
      CHARACTER X*1
      DIMENSION L(1),M(1),X(K,1)
C...
      L(1)= 0
      M(1)= 0
      DO 20 I=2,N
      L(I)= 0
      M(I)= 0
      J   = 1
C...8 IF(X(I).GT.X(J)) GO TO 15
C...8 IF(X(1,I)(1:K).GT.X(1,J)(1:K)) GO TO 15
    8 IF(X(1,I)(1:K).GE.X(1,J)(1:K)) GO TO 15
C...
      IF(L(J).EQ.0)    GO TO 12
      J = L(J)
      GO TO 8
   12 M(I)=-J
      L(J)= I
      GO TO 20
   15 IF(M(J).LE.0) GO TO 18
      J = M(J)
      GO TO 8
   18 M(I)= M(J)
      M(J)= I
   20 CONTINUE
      J = 1
      I = 0
      GO TO 25
   24 J = L(J)
   25 IF(L(J).GT.0) GO TO 24
   27 I    = I+1
      L(J) = I
      IF(M(J)) 33,35,31
   31 J    = M(J)
      GO TO 25
   33 J    =-M(J)
      GO TO 27
   35 DO 38 I=1,N
      J = L(I)
      M(J)=I
   38 CONTINUE
      RETURN
      END
