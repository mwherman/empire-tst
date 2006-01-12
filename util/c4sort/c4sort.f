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
C-V  03/07 Implement ALIASLV to modify level energies (A.Trkov)
C-V  03/08 Sort differential data in ascending energy (A.Trkov)
C-V  03/10 Minor fix for eof on C4 file (A.Trkov)
C-V  04/01 Increase MXEN from 5000 to 40000 (A.Trkov)
C-V  04/06 - Resequence DDX ang.distrib. at Eo into spectra at Ang.
C-V        - Paging buffer for sorting to increase efficiency
C-V        - Sort by outgoing particles. (A.Trkov)
C-V  04/07 Fix format reading metastable targets (A.Trkov)
C-V  04/09 Sort discrete level angular distributions by level E (A.Trkov)
C-V  05/07 - Major reorganisation of coding, blocking separately the coding
C-V          for discrete level processing and for sorting the double
C-V          differential data.
C-V        - Match discrete level energies to RIPL database (A.Trkov).
C-V  05/11 - Fix bugs.
C-V  05/12 - Increase MXIR from 12000 to 80000 (A.Trkov)
C-V        - Increase MXEN from 40000 to 80000 (A.Trkov)
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
C-M  Note:
C-M  The entries in a C4 file may appear in any order any the file
C-M  size may be large. Depending on the actual ordering of the data
C-M  on a C4 file, the sorting sequence may become complex, requiring
C-M  the file to be rewound and processed again. For this reason the
C-M  sorting process may take a considerable amount of time.
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
C-M  Three records are expected from input:
C-M  - Name of the source EXFOR file in C4 format.
C-M  - Name of the sorted EXFOR file to be written.
C-M  - Full path to the nuclide level files, including the directory
C-M    delimiter at the end (\ on Windows, / on Linux, etc).
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
C-M  '   ALIASLV' Equivalent ZA/E-LVL can be defined to correct
C-M               level energies that appear in an EXFOR file.
C-M                11-20  ZA1 designation on the source file.
C-M                21-30  Level energy in the source file.
C-M                31-40  ZA2 designation on the sorted file.
C-M                41-50  Level energy in the sorted file.
C-M  '   DPATH  ' Full path to the nuclide level files, including the
C-M               directory delimiter at the end
C-M               (\ on Windows, / on Linux, etc).
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
C-M
C-M  Dimension limits:
C-M    MXEN - maximum number of EXFOR entries or sections by which
C-M           sorting is done.
C-M    LCH  - Length of the comparison string for sorting
C-M           (defined by the sorting logic - do not change).
C-M    MXAL - Maximum number of input alias strings.
C-M    MXIR - Page buffer size (increasing the buffer size
C-M           increases sorting efficiency for datasets where
C-M           extensive re-ordering is required; e.g. high
C-M           resolution cross section measurements at different
C-M           angles which must be grouped to angular distributions
C-M           at different energies).
C-
      PARAMETER       (MXEN=80000,MXAL=20,LCH=52,MXIR=80000)
      PARAMETER       (MEL1=20)
      CHARACTER*132    REC,RC1,RC6(MXIR)
      CHARACTER*80     DPATH
      CHARACTER*40     FLIN,FLC4,FLOU,FLSC
      CHARACTER*35     REF
      CHARACTER*20     ZAMT,CH20,ELL(MXIR)
      CHARACTER*15     EIN
      CHARACTER*14     ALIA(4,MXAL)
      CHARACTER*(LCH)  ENT(MXEN)
      CHARACTER*9      AOU,POU,ELV,ELW
      CHARACTER*3      LB3
      CHARACTER*1      EN1(LCH,MXEN),EL1(MEL1,MXIR), CHAR1, CHAR2
      LOGICAL          EXST
      DIMENSION        IDX(MXEN),LNE(MXEN),ID1(MXEN),ID2(MXEN)
     &                ,NSE(MXEN),ID3(MXIR),ID4(MXIR)
      EQUIVALENCE     (ENT(1),EN1(1,1))
      EQUIVALENCE     (ELL(1),EL1(1,1))
C* Preset the ZA and reference strings
      DATA REF/'###################################'/
      DATA ZAMT/'####################'/
C* Files and logical file unit numbers
      DATA FLIN/'C4SORT.INP'/
     2     FLC4/'C4.DAT'/
     3     FLOU/'C4.DAT'/
     4     FLSC/'C4SORT.TMP'/
      DATA LIN,LC4,LOU,LSC,LKB,LTT/ 1, 2, 3, 4, 5, 6 /
      NSET=0
      MPG =20000
      FLV =-1
C*
C* Default path to RIPL nuclide level energy files
      DPATH='../Sources/Inputs/Levels/'
      LPATH=25
C*
C* Process the input
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
      CALL C4SINP(LIN,LTT,FLC4,FLOU,NALI,ALIA,DPATH,LPATH,IER)
      CLOSE(UNIT=LIN)
      IF(IER.EQ.0) GO TO 16
C* Remedial action if fatal error on input is encountered:
C* Print a warning to default output and try reading default input
   15 NALI=MXAL
      CALL C4SINP(LKB,LTT,FLC4,FLOU,NALI,ALIA,DPATH,LPATH,IER)
      FLIN='Keyboard'
      IF(IER.EQ.0) GO TO 16
C* Try input/output ENDF files from default input/output
      LC4=LKB
      LOU=LTT
      LTT=0
      MATH=0
      GO TO 17
C*
C* Input instructions processed - start processing the C4 file
   16 OPEN (UNIT=LC4,FILE=FLC4,STATUS='OLD')
      OPEN (UNIT=LSC,FILE=FLSC,STATUS='UNKNOWN')
C* Write the banner to output
      WRITE(LTT,900) ' C4SORT - Sort data in a C4 file        '
      WRITE(LTT,900) ' -------------------------------        '
      WRITE(LTT,900)
      WRITE(LTT,900) '              Input instructions file : ',FLIN
      WRITE(LTT,900) '       Source EXFOR file in C4 format : ',FLC4
      WRITE(LTT,900) '       Sorted EXFOR file in C4 format : ',FLOU
      WRITE(LTT,900) ' Directory path to nuclear level files: ',DPATH

      WRITE(LTT,900)
      IF(NALI.GT.0) THEN
        WRITE(LTT,900) ' Aliased quantities:                    '
        WRITE(LTT,900) ' Old:  MAT  MF  MT      ELVL New:  MAT  '
     &                ,'MF  MT      ELVL                        '
        DO I=1,NALI
          WRITE(LTT,*) '    ',ALIA(1,I),ALIA(3,I),ALIA(2,I),ALIA(4,I)
        END DO
      END IF
   17 CONTINUE
C*
C* Begin processing the source file
      NEN=0
      JLN=0
      MLN=0
      MOU=0
C* Copy records to scratch until the reference changes
   20 NSET=NSET+1
      READ (LC4,901,END=200) REC

c...      print *,nset,'"',rec(1:30)

   24 IF(REC(2:20).EQ.'                   ') GO TO 200
C* Change any alias ZA/MF/MT designations
      CALL CALIAS(NALI,ALIA,REC)
C* Decode the MF number
      READ (REC(13:15), * ,END=801,ERR=801) MF
C* Decode the incident particle energy
      READ (REC(23:31),931,ERR=801) FIN
C* Decode secondary or discrete level energy
      READ (REC(77:85),931,ERR=801) FLV
C* Adjust the level energy if necessary (and possible)
      LB3=REC(95:97)
      IF(FLV.GT.0) THEN
        IF(LB3.EQ.'LVL' .OR. LB3.EQ.'EXC') THEN
          READ (REC(1:20),902,ERR=801) IZA
          FLV0=FLV
          CALL MCHLVL(IZA,FLV,DPATH,LPATH,IER)
c...
c...          print *,'"',rec(1:20),'"',iza,flv0,flv,ier
c...
        END IF
        WRITE(ELW,931) FLV
        REC(77:85)=ELW
      END IF
C* Special treatment discrete level and secondary energy reactions
      IF(MF.EQ.3 .AND. NINT(FLV).GT.0) GO TO 30
      IF(MF.EQ.4 .AND. NINT(FLV).GT.0) GO TO 30
      IF(MF.GE.4 .AND. MF.LE.6) GO TO 60
C*
      IF(REC( 1: 20).EQ.ZAMT .AND. REC(98:132).EQ. REF) THEN
C* Record identified to belong to the same set - process next record
        WRITE(LSC,901) REC
        JLN=JLN+1
        GO TO 20
      END IF
C* On change in ZA, MF, MT or reference - make an index entry
C* Save the incident particle energy
      READ (REC(23:31),931,ERR=801) FIN
      WRITE(EIN,932) FIN
C...C* If range of levels is given, redefine level energy to average
C...      READ (REC(77:85),931,ERR=801) FLV
C...      IF(MF.EQ.3 .AND. NINT(FLV).GT.0) THEN
C...        READ(REC(86:94),931,ERR=801) FLD
C...        IF(NINT(FLD).GE.NINT(FLV)) THEN
C...          FL1=FLV
C...          FLV=(FLD+FL1)/2
C...          FLD=(FLD-FL1)/2
C...          WRITE(ELV,931) FLD
C...          REC(86:94)=ELV
C...        END IF
C...        WRITE(ELV,931) FLV
C...        REC(77:85)=ELV
C...      END IF
C* Save the number of points for the previous entry
      IF(NEN.GT.0) NSE(NEN)=NSET
C...
C...      if(nen.gt.0) print *,'      at20 nen,nset',nen,nset
C...
C* Save the new record entry
      WRITE(LSC,901) REC
      JLN=JLN+1
      IF(NEN.GE.MXEN) STOP 'C4SORT ERROR - MXEN Limit exceeded'
      NEN=NEN+1
      LNE(NEN)=JLN
C* Modify "gamma" flag so that gamma distributions appear last
      IF(REC(73:76).EQ.' 0.9') REC(68:76)='   9999.9'
C* Define level energies for discrete level reactions (if given)
      ELV='         '
      LB3=REC(95:97)
      IF((MF.EQ.3 .OR. MF.EQ.4) .AND.
     &   (LB3.EQ.'LVL' .OR. LB3.EQ.'EXC') ) THEN
        READ(REC(77:85),931,ERR=801) DMY
        WRITE(ELV,931) DMY
      END IF
C* Identify outgoing particle (if relevant)
      IF(MF.EQ.3 .OR. MF.EQ.5 .OR. MF.EQ.6) THEN
        POU=REC(68:76)
      ELSE
        POU='         '
      END IF
      IF(MF.EQ.3) THEN
        ENT(NEN)=REC(1:19)//POU//ELV//EIN
      ELSE
        ENT(NEN)=REC(1:19)//POU//EIN//ELV
      END IF
      REF =REC(98:132)
      ZAMT=REC(1:20)
      IFIN=NINT(FIN)
      IFLV=NINT(FLV)
c...
c...      print '(i5,3a)',nen,ent(nen),' ',ref//' in30'
c...
      NSET=0
C* Proceed to next record
      GO TO 20
C*
C* Special treatment discrete level and secondary energy reactions
C* Re-sequence MF3 data of same type and author by level energy
   30 MF1 =MF
      REF =REC(98:132)
      ZAMT=REC(1:20)
      IFIN=NINT(FIN)
      IFLV=NINT(FLV)
      IR  =0
   32 IR  =IR+1
      IF(IR.GT.MXIR) STOP 'C4SORT ERROR - MXIR limit exceeded (32)'
c...
c...        print *,ir,rec
c...
C* If range of levels/energies is given, redefine
C...      READ(REC(86:94),931,ERR=801) FLD
C...      IF(NINT(FLD).GE.NINT(FLV)) THEN
C...        FL1=FLV
C...        FLV=(FLD+FL1)/2
C...        FLD=(FLD-FL1)/2
C...        WRITE(ELW,931) FLD
C...        REC(86:94)=ELW
C...      END IF
C...      WRITE(ELV,931) FLV
C...      REC(77:85)=ELV
C*
C* Standardise level/secondary energy format
      WRITE(ELW,931) FLV
      WRITE(POU,938) MIN(999999999,NINT(FIN))
C* Save record to RC6 field and sorting string to ELL
      REC(77:85)=ELW
      RC6(IR)=REC
      ELL(IR)=ELW//POU//'  '
C*
C* Read a new record from the C4 file
      READ (LC4,901,END=200) REC
      IF(REC(1:20).EQ.'                    ') GO TO 34
C* Decode the MF number
      READ (REC(13:15), * ,END=801,ERR=801) MF
C* Decode the incident particle energy
      READ (REC(23:31),931,ERR=801) FIN
C* Adjust the level energy if necessary (and possible)
      READ(REC(77:85),931,ERR=801) FLV
      LB3=REC(95:97)
      IF(FLV.GT.0 .AND. (LB3.EQ.'LVL' .OR. LB3.EQ.'EXC')) THEN
        READ (REC(1:20),902,ERR=801) IZA
        FLV0=FLV
        CALL MCHLVL(IZA,FLV,DPATH,LPATH,IER)
c...
c...          print *,'"',rec(1:20),'"',iza,flv0,flv,ier
c...
        WRITE(ELW,931) FLV
        REC(77:85)=ELW
      END IF
C* Check for change in IZA, MF, MT
      IF(REC( 1: 20).NE.ZAMT) GO TO 34
C* Check for change in reference
      IF(REC(98:132).NE. REF) GO TO 34
C* Check for change in incident energy (if MF4)
      IF(MF.NE.3 .AND. NINT(FIN)  .NE.IFIN) GO TO 34
C* Check for the presence of level or secondary energy
      IF(MF.EQ.3 .AND. FLV.EQ.0) GO TO 34
      GO TO 32
C*
C* Block for one data type/author read - Sort by level
   34 CALL SRTTCH(IR,MEL1,ID3,ID4,EL1)
C* Write the sorted set from saved record sequence in RC6
      ELV='#########'
c...
c...      do j=1,ir
c...         print *,rc6(j)
c...      end do
c...
      DO I=1,IR
        L=ID4(I)
        RC1 =RC6(L)
C* Add record to scratch file
        WRITE(LSC,901) RC1
        JLN =JLN+1
C* If Elv changes, make a new record entry
        CH20=ELL(L)
c...
c...        print *,'ch20,elv','"',ch20,'"',elv,'"'
c...
        IF(CH20(1:9).NE.ELV) THEN
C* Save the number of points for the previous entry
          IF(NEN.GT.0) NSE(NEN)=NSET
c...
c...      if(nen.gt.0) print *,'      at34 nen,nset',nen,nset
c...
C* Save the new entry data
          IF(NEN.GE.MXEN) STOP 'C4SORT ERROR - MXEN Limit exceeded'
          NEN=NEN+1
          LNE(NEN)=JLN
          REF =RC1(98:132)
          ZAMT=RC1(1:20)
          READ (RC1(23:31),931) FIN
          READ (RC1(77:85),931) FLV
          WRITE(EIN,932) FIN
          CH20=ELL(L)
          ELV =CH20(1:9)
          IFIN=NINT(FIN)
          IFLV=NINT(FLV)
          ENT(NEN)=RC1(1:19)//RC1(68:76)//ELV//EIN
c...
c...        print '(i5,3a)',nen,ent(nen),' ',ref//' in24'//ELL(L)
c...
          NSET=0
        END IF
        NSET=NSET+1
        JSET=JSET+1
      END DO
c...
c...       WRITE(41,*) 'i,lne,nse,id2,ent'
c...       DO I=1,NEN
c...         WRITE(41,'(4I6,A52)') I,LNE(I),NSE(I),ID2(I),ENT(I)
c...       END DO
c...       STOP
cc...
      IF(REC(1:20).EQ.'                    ') GO TO 200
      GO TO 24
C*
C* Re-sequence double differential data by angles/incident energies
   60 CONTINUE
      REF =REC(98:132)
      ZAMT=REC(1:20)
      IFIN=NINT(FIN)
c...
c...        if(nen.gt.0) print *,'      at60 nen,nset',nen,nset
c...
      IR  =0
   62 IR  =IR+1
      IF(IR.GT.MXIR) STOP 'C4SORT ERROR - MXIR limit exceeded (62)'
C* Save record to RC6 field and sorting string to ELL
      ELW=REC(59:67)
      IF(ELW.NE.'         ') THEN
        READ (ELW,931) ANG
        WRITE(ELW,936) ANG
        REC(59:67)=ELW
        WRITE(ELW,936) ANG+1
      END IF
      RC6(IR)=REC
      IF(MF.EQ.4) THEN
C* Sort MF4 by incident energy and angle
        WRITE(POU,938) MIN(999999999,NINT(FIN))
        ELL(IR)=POU//ELW//'  '
      ELSE
C* Sort MF6 by angle and outgoing energy
        READ (REC(77:85),931) FLV
        WRITE(POU,938) MIN(999999999,NINT(FLV))
        ELL(IR)=ELW//POU//'  '
      END IF
C* Read the next record
      READ (LC4,901,END=200) REC
      IF(REC(1:20).EQ.'                    ') GO TO 64
C* Change any alias ZA/MF/MT designations
      CALL CALIAS(NALI,ALIA,REC)
C* Decode the MF number
      READ (REC(13:15), * ,END=801,ERR=801) MF
C* Decode the incident particle energy
      READ (REC(23:31),931,ERR=801) FIN
C* Check for change in IZI, IZA, MF, MT, reference or incident energy
      IF(REC( 1: 20).NE.ZAMT) GO TO 64
      IF(REC(98:132).NE. REF) GO TO 64
      IF(MF.NE.4 .AND. NINT(FIN)  .NE.IFIN) GO TO 64
      GO TO 62
C*
C* Block for one data type/author read - Sort by level
   64 CONTINUE
c...
c...          print *,'Begin sorting DDX, IR',IR
c...          do j=1,ir
c...            print *,j,'"',ELL(j),'"'
c...          end do
c...
      CALL SRTTCH(IR,MEL1,ID3,ID4,EL1)
c...
c...          print *,'Sorting completed'
c...
C* Write the sorted set from saved record sequence in RC6
      POU='#########'
c...
c...      do j=1,ir
c...         print *,rc6(j)
c...      end do
c...
      DO I=1,IR
        L=ID4(I)
        RC1 =RC6(L)
C* Add record to scratch file
        WRITE(LSC,901) RC1
        JLN =JLN+1
C* On change of incident energy(MF4) or angle(MF6), make a new record entry
        CH20=ELL(L)
        IF(CH20(1:9).NE.POU) THEN
C* Save the number of points for the previous entry
          IF(NEN.GT.0) NSE(NEN)=NSET
c...
c...          if(nen.gt.0) print *,'      at64 nen,nset',nen,nset
c...          print *,'ELL,pou',ch20,pou
c...
C* Save the new entry data
          IF(NEN.GE.MXEN) STOP 'C4SORT ERROR - MXEN Limit exceeded'
          NEN=NEN+1
          LNE(NEN)=JLN
          RC1 =RC6(L)
          REF =RC1(98:132)
          READ(RC1(13:15), * ,END=801,ERR=801) MF
          ZAMT=RC1(1:20)
          EIN =RC1(23:31)
          READ (EIN,931) FIN
          IF(MF.EQ.4) THEN
            WRITE(EIN,932) FIN
            ENT(NEN)=RC1(1:19)//'         '//EIN//'         '
            WRITE(POU,938) MIN(999999999,NINT(FIN))
          ELSE
            WRITE(EIN,932) FIN
            AOU=RC1(59:67)
            READ (AOU,931) ANG
            WRITE(AOU,936) ANG+1
            POU=RC1(68:76)
            IF(POU(6:9).EQ.' 0.9' .OR.
     &         POU(6:9).EQ.'  .9') POU='   9999.9'
            ENT(NEN)=RC1(1:19)//POU//EIN//AOU
          END IF
          CH20=ELL(L)
          IFIN=NINT(FIN)
c...
c...          print '(i5,3a)',nen,ent(nen),' ',ref//' in64'
c...
          NSET=0
        END IF
        NSET=NSET+1
        JSET=JSET+1
      END DO
c
c      WRITE(41,*) 'i,lne,nse,id2,ent'
c      DO I=1,NEN
c        WRITE(41,'(4I6,A52)') I,LNE(I),NSE(I),ID2(I),ENT(I)
c      END DO
c      STOP
c
      IF(REC(1:20).EQ.'                    ') GO TO 200
      GO TO 24
C*
C* Source C4 file processed
  200 IF(NEN.GT.0) NSE(NEN)=NSET
c...
c...        if(nen.gt.0) print *,'      at200 nen,nset',nen,nset
c...
      WRITE(LTT,921) ' Number of EXFOR entries to sort      : ',NEN
      WRITE(LTT,921) ' Total number of records              : ',JLN
      CLOSE(UNIT=LC4)
C*
C* Sort the entries
      CALL SRTTCH(NEN,LCH,ID1,ID2,EN1)
C...
c...      PRINT *,'Writing sorted entry list to c4sort.scr'
      OPEN (UNIT=41,FILE='c4sort.scr',STATUS='UNKNOWN')
      WRITE(41,*) 'i,id2,lne,nse,ent'

      DO I=1,NEN
        J=I
        J=ID2(I)
        WRITE(41,'(4I6,A52)') I,J,LNE(J),NSE(J),ENT(J)
      END DO
      CLOSE(UNIT=41)
C...
      WRITE(LTT,921) ' Indices sorted - begin writing file    '
C*
C* Write the sorted EXFOR file from scratch
      OPEN (UNIT=LOU,FILE=FLOU,STATUS='UNKNOWN')
      KLN=JLN
      I1=KLN+1
C* Loop over different entries
      DO 420 I=1,NEN
        J   =ID2(I)
        ILN =LNE(J)
        NSET=NSE(J)
c...
c...          print *,'Sorted',j
c...          print *,'I,I1,Iln,Nset',I,I1,Iln,Nset
c...
C* If indexing is correct, this should not happen
        IF( ( ILN.LE. 0 .OR. ILN.GT.KLN) .OR.
     &      (NSET.LE. 0 .OR. ILN+NSET-1.GT.KLN) ) THEN
          PRINT *,'I,J,ILN,NSET,KLN',I,J,ILN,NSET,KLN
          STOP 'C4SORT ERROR - Unknown indexing fault'
        END IF
        IF(ILN.LT.I1) THEN
C* Rewind if next entry ILN appears before the current buffer
          REWIND LSC
          I2=0
          I1=I2-MXIR+1
        END IF
        DO J=1,NSET
          JLN=ILN-1+J
          DO WHILE (I2.LT.JLN)
            I1=I2+1
            I2=MIN(KLN,I2+MXIR)
            MR=I2-I1+1
            DO L=1,MR
              READ (LSC,901) RC6(L)
            END DO
          END DO
          JJ=JLN-I1+1
c...          print *,'i1,i2,jln',i1,i2,jln,JJ
          REC=RC6(JJ)
CMH Move year to adjucent to the accession number
          DO ichar=98,122
             IF(REC(ichar:ichar) .EQ.'(' ) THEN
                CHAR1=REC(ichar+1:ichar+1)
                CHAR2=REC(ichar+2:ichar+2)
                REC(ichar:ichar)=' '
                REC(ichar+1:ichar+1)=' '
                REC(ichar+2:ichar+2)=' '
                REC(ichar+3:ichar+3)=' '
                REC(119:119)='('
                REC(120:120)=CHAR1
                REC(121:121)=CHAR2
                REC(122:122)=')'
                GOTO 599
             ENDIF
          ENDDO
  599     CONTINUE        
          
C* Suppress printout of negative cross sections
          READ (REC,941,ERR=801) MF,XS
          IF(MF.NE.3 .OR. XS.GT.0) WRITE(LOU,901) REC
C* Print a message after every MPG sorted records
          MLN=MLN+1
          IF(MLN.GT.MPG) THEN
            MOU=MOU+MPG
            MLN=MLN-MPG
            WRITE(LTT,921)'                       Sorted records : ',MOU
          END IF
        END DO
C* Proceed to the next data set
  420 CONTINUE
      WRITE(LTT,921)'                       Sorted records : ',KLN
C*
C* All data processed
  600 CLOSE(UNIT=LOU)
      STOP 'C4SORT Completed'
C* Error trap
  801 WRITE(*,900) ' C4SORT ERROR - Illegal C4 record :     '    
      WRITE(*,900) '"'//REC(1:39),REC(40:78)//'"'
      STOP 'C4SORT ERROR - reading C4 file'
C*
  900 FORMAT(2A40)
  901 FORMAT(A132)
  902 FORMAT(5X,I6)
  921 FORMAT(A40,I6)
  931 FORMAT(BN,F9.0)
  932 FORMAT(F15.3)
  936 FORMAT(F9.6)
  938 FORMAT(I9)
  941 FORMAT(12X,I3,25X,F9.0)
      END
      SUBROUTINE C4SINP(LIN,LTT,FLC4,FLOU,NALI,ALIA,DPATH,LPATH,IER)
C-Title  : Subroutine C4SINP
C-Purpose: Process input instructions for C4SORT
      PARAMETER    (MXKW=10)
      CHARACTER*80  DPATH
      CHARACTER*40  FLC4,FLOU
      CHARACTER*14  WORD,ALIA(4,1)
      CHARACTER*10  KWRD(MXKW),REC(8),C10
C*
      DATA KWRD
     1/'$* C4SORT ','   FLC4IN ','   FLC4OU ','          ','          '
     1,'   ALIAS  ','   ALIASLV','   DPATH  ','          ','   ENDC4S '/
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
      READ (LIN,901,END=200) REC
      DPATH=REC(1)//REC(2)//REC(3)//REC(4)
     &    //REC(5)//REC(6)//REC(7)//REC(8)
C* Determine the length of the directory path string
      IF(DPATH(1:1).EQ.'-') THEN
        LPATH=-1
        RETURN
      END IF
      LPATH=80
      DO WHILE (LPATH.GT.1 .AND. DPATH(LPATH:LPATH).EQ.' ')
        LPATH=LPATH-1
      END DO
      IF(LPATH.EQ.0) THEN
        DPATH='./'
        LPATH=2
      END IF
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
      IF(MF1.EQ.0) MF2=0
      IF(MF2.EQ.0) MF2=MF1
      IF(MT1.EQ.0) MT2=0
      IF(MT2.EQ.0) MT2=MT1
      WRITE(WORD,962) IZA1,MF1,MT1
      ALIA(1,NALI)=WORD
      WRITE(WORD,962) IZA2,MF2,MT2
      IF(MF2.EQ.0) WORD(10:10)=' '
      IF(MT2.EQ.0) WORD(14:14)=' '
      ALIA(2,NALI)=WORD
      ALIA(3,NALI)='          '
      ALIA(4,NALI)='          '
      GO TO 90
C* '   ALIASLV'
  170 NALI=NALI+1
      IF(NALI.GT.MXAL) STOP 'C4SINP ERROR - MXAL Limit exceeded'
      READ (REC(2),902) IZA1
      READ (REC(3),903) ELV1
      READ (REC(4),902) IZA2
      READ (REC(5),903) ELV2
      WRITE(ALIA(1,NALI),964) IZA1
      WRITE(ALIA(2,NALI),964) IZA2
      WRITE(ALIA(3,NALI),963) ELV1
      WRITE(ALIA(4,NALI),963) ELV2
      GO TO 90
C* '   DPATH  '
  180 DPATH=REC(2)//REC(3)//REC(4)//REC(5)//REC(6)//REC(7)//REC(8)
      IF(DPATH(1:1).EQ.'-') THEN
        LPATH=-1
        GO TO 90
      END IF
      LPATH=70
      DO WHILE (LPATH.GT.1 .AND. DPATH(LPATH:LPATH).EQ.' ')
        LPATH=LPATH-1
      END DO
      IF(LPATH.EQ.0) THEN
        DPATH='./'
        LPATH=2
      END IF
      GO TO 90
C* Unused
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
  903 FORMAT(BN,F10.0)
  904 FORMAT(' Unrecognised keyword "',A10,'"')
  961 FORMAT(BN,2I5)
  962 FORMAT(I6,I4,I4)
  963 FORMAT(F10.2)
  964 FORMAT(I6,'        ')
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
      CHARACTER*14  ALIA(4,1),ALII,TEST
      IF(NALI.LT.1) RETURN
      TEST=REC(6:19)
      DO 20 I=1,NALI
      ALII=ALIA(1,I)
C* Test matching ZA
      IF(ALII( 1: 6).NE.TEST( 1: 6)) GO TO 20
C* Test matching MF (if not blank)
      IF(ALII( 8:10).NE.TEST( 8:10).AND.ALII( 8:10).NE.'   ' ) GO TO 20
C* Test matching MF (if not blank)
      IF(ALII(11:14).NE.TEST(11:14).AND.ALII(11:14).NE.'    ') GO TO 20
        ALII=ALIA(2,I)
C*        Substitute ZA
        TEST( 1: 6)=ALII( 1: 6)
C*        Substitute MF/MT if not blank
        IF(ALII( 8:10).NE.'   ' ) TEST( 8:10)=ALII( 8:10)
        IF(ALII(11:14).NE.'    ') TEST(11:14)=ALII(11:14)
        REC(6:19)=TEST
        ALII=ALIA(3,I)
C*        Check if level energies are to be substituted
        IF(ALII(1:10).NE.'          ') THEN
C*        Check the values of level energies
          READ (REC(76:85),'(F10.0)') ELV0
          READ (ALII      ,'(F10.0)') ELV1
          IF(NINT(100*ELV0).EQ.NINT(100*ELV1)) THEN
C*        Substitute level energy, if not blank
            ALII=ALIA(4,I)
            REC(76:85)=ALII(1:10)
          END IF
        END IF
   20 CONTINUE
      RETURN
      END
      SUBROUTINE SRTTCH(N,K,L,M,X)
C-Title  : SRTTCH subroutine
C-Purpose: Perform a sort in ascending order by Tree sort method
C-Description:
C-D Sort in ascending order the vector of N characters strings of
C-D length K stored in array X. The actual entries in X remain
C-D unaffected, but on exit:
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
      SUBROUTINE MCHLVL(IZA,FLVL,DPATH,LPATH,IER)
C-Title  : Subroutine MCHLVL
C-Purpose: Redefine level energy that most closely matches RIPL
C-D
C-D INPUT:  IZA   integer ZA designation (1000*Z+A)
C-D         FLVL  initial level energy (to be matched)
C-D OUTPUT: FLVL  most closely matching level energy
C-
      PARAMETER       (MXLVL=400)
      DOUBLE PRECISION ELVL
      CHARACTER*80     DPATH
      DIMENSION        ELVL(MXLVL)
C*
      SAVE IZA0,IFND,ELVL
C*
      DATA IZA0,IFND/ -1, 0 /
C*
      IF(IZA.NE.IZA0) THEN
        IZ=IZA/1000
        IA=IZA-IZ*1000
        CALL read_levels_RIPL(IA,IZ,ELVL,NLVL,MXLVL,DPATH,LPATH,IER)
        IFND=1
        IF(IER.NE.0) THEN
          IFND=0
          PRINT *,'ERROR Reading RIPL energy level file for ZA',IZA
          IF(IER.EQ.3) STOP 'MCHLVL ERROR - MXLVL limit exceeded'
c...      STOP    'ERROR Reading RIPL energy level file'
        END IF
      END IF
c...
c...      print *,'iza,iza0,ifnd,ier',iza,iza0,ifnd,ier
c...
      IZA0=IZA
      IF(IFND.NE.1) THEN
        IER=1
        RETURN
      END IF
C* Find the closest match
      FI=FLVL*1E-6
      DL=FI
      JL=1
      DO I=1,NLVL
        EI=ELVL(I)
        DD=ABS(EI-FI)
        IF(DD.LT.DL) THEN
          JL=I
          DL=DD
        END IF
      END DO
      FLVL=ELVL(JL)*1E6
      RETURN
      END
      subroutine read_levels_RIPL(ia,iz,elev,nlev,mlev,dpath,lpath,ierr)
C-Title  : Subroutine read_levels_RIPL
C-Purpose: Read the nuclear level files from the RIPL database
C-Author : R. Capote Noy, IAEA, Vienna, Austria
C-Version: 2005/07 (original code)
C-D
C-D INPUT:  IA,IZ are the mass and atomic number
C-D         mlev              maximum available size of array elev
C-D         dpath             path to the RIPL level directory files
C-D         lpath             length of the RIPL level directory path
C-D OUTPUT: elev(i),i=1,nlev  Array containing the levels' energies for nlev levels
C-D         ierr              Error flag
C-D                       0 = no error
C-D                       1 = Levels file not found
C-D                       2 = Isotope not found
C-D                       3 = mlev limit for array size 'elev' exceeded
C-
      implicit none
c
c     dummy arguments
c
      character*80 dpath
      integer ia,iz, nlev, mlev, lpath, ierr
      double precision elev(*)
c   
c     local variables
c
      character*1 cdum
      character*3 ctmp3
      character*8 cinp
      character*5 chelem
      integer iar,izr,nlvr,ngamr,lvpr,ndbrlin,ilv,nbr
      integer itmp,itmp1,itmp2
      double precision qn, elvr,xjlvr,t12

      WRITE (ctmp3,'(I3.3)') iz
      cinp = 'z'//ctmp3//'.dat'
      OPEN (13,FILE = dpath(1:lpath)//cinp,STATUS = 'OLD',
     &         ERR = 200)

  100 READ (13,'(A5,6I5,2f12.6)',END = 300) chelem, iar, izr, nlvr,
     &      ngamr, itmp1, itmp2, qn
      IF (qn.GT.mlev) THEN
C       array size of elev exceeded
        ierr=3
        CLOSE (13)
        return
      ENDIF
      IF (ia.NE.iar .OR. iz.NE.izr) THEN
         DO ilv = 1, nlvr + ngamr
            READ (13,'(A1)') cdum
         ENDDO
         GOTO 100
      ENDIF
      nlev = 0
      DO ilv = 1, nlvr
         READ (13,'(I3,1X,F10.6,1X,F5.1,I3,1X,E10.2,I3)',END = 300) 
     &         itmp, elvr,xjlvr, lvpr, t12, ndbrlin
C--------Number of levels limited by binding energy
         IF (elvr.LT.qn) THEN
            nlev = nlev + 1
            elev(nlev) = elvr
         ENDIF
         DO nbr = 1, ndbrlin
            READ (13,'(A1)') cdum
         ENDDO
      ENDDO
      CLOSE (13)
C     Proper return
      ierr = 0
      return
C Error traps
C     Levels' file not found
  200 ierr = 1

      print *,'ERROR opening file',dpath(1:lpath)//cinp

      return
C     Isotope not found
  300 ierr = 2
      return
      end
