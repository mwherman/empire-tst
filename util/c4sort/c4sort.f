      PROGRAM C4SORT
C-Title  : Program C4SORT
C-Purpose: Sort EXFOR file in C4 format
C-Author : Andrej Trkov, IAEA
C-A        Present address:                       e-mail: Andrej.Trkov@ijs.si
C-A        "Jozef Stefan" Institute               tel: +386 1 5885 324
C-A        Jamova 39
C-A        1000 Ljubljana
C-A        Slovenia
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
C-V  06/12 Another fix to format reading metastable targets (A.Trkov)
C-V  07/04 Convert E2 to LVL for MT51 if Elo=Ehi (A.Trkov)
C-V  08/01 Correct ZA of residual nucleus when searching level energies.
C-V  08/02 Sort also by metastable products (A. Trkov).
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
C-M    LES  4  Cross section standards in ENDF format.
C-M    LKB  5  Default input.
C-M    LTT  6  Default output.
C-M    LSC  7  C4SORT.TMP scratch file.
C-M    LSC  8  C4SORT.TM2 second scratch file.
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
      PARAMETER       (MXEN=80000,MXAL=20,LCH=53,MXIR=80000,MXMT=30)
      PARAMETER       (MEL1=20,MXNP=10000,MXRW=20000)
      CHARACTER*132    REC,RC1,RC6(MXIR)
      CHARACTER*80     DPATH
      CHARACTER*40     BLNK,FLIN,FLC4,FLOU,FLES,FLSC,FLS2
      CHARACTER*35     REF
      CHARACTER*20     ZAMT,CH20,ELL(MXIR)
      CHARACTER*15     EIN
      CHARACTER*14     ALIA(4,MXAL)
      CHARACTER*(LCH)  ENT(MXEN),ENTI
      CHARACTER*9      AOU,POU,ELV,ELW
      CHARACTER*3      LB3
      CHARACTER*1      EN1(LCH,MXEN),EL1(MEL1,MXIR), CHAR1, CHAR2
      LOGICAL          EXST
      DIMENSION        IDX(MXEN),LNE(MXEN),ID1(MXEN),ID2(MXEN)
     &                ,NSE(MXEN),ID3(MXIR),ID4(MXIR)
     &                ,MATS(MXMT),MTS(MXMT),ELO(MXMT),EHI(MXMT)
     &                ,EST(MXNP),XST(MXNP),DST(MXNP),RWO(MXRW)
      EQUIVALENCE     (ENT(1),EN1(1,1))
      EQUIVALENCE     (ELL(1),EL1(1,1))
C* Preset the ZA and reference strings
      DATA REF/'###################################'/
      DATA ZAMT/'####################'/
C* Files and logical file unit numbers
      DATA BLNK/'                                        '/
     1     FLIN/'C4SORT.INP'/
     2     FLC4/'C4.DAT'/
     3     FLOU/'C4.DAT'/
     4     FLES/'                                        '/
     7     FLSC/'C4SORT.TMP'/
     7     FLS2/'C4SORT.TM2'/
      DATA LIN,LC4,LOU,LES,LKB,LTT,LSC,LS2/ 1, 2, 3,-4, 5, 6, 7, 8 /
      NSET=0
      MPG =20000
      FLV =-1
C*
C* Default path to RIPL nuclide level energy files
c...  DPATH='../Sources/Inputs/Levels/'
c...  LPATH=25
      DPATH='../../RIPL-2/levels/'
      LPATH=20
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
      CALL C4SINP(LIN,LTT,FLC4,FLOU,FLES,NALI,ALIA,DPATH,LPATH,IER)
      CLOSE(UNIT=LIN)
      IF(IER.EQ.0) GO TO 16
C* Remedial action if fatal error on input is encountered:
C* Print a warning to default output and try reading default input
   15 NALI=MXAL
      CALL C4SINP(LKB,LTT,FLC4,FLOU,FLES,NALI,ALIA,DPATH,LPATH,IER)
      FLIN='Keyboard'
      IF(IER.EQ.0) GO TO 16
C* Try input/output ENDF files from default input/output
      LC4=LKB
      LOU=LTT
      LTT=0
      MATH=0
      GO TO 19
C*
C* Input instructions processed - start processing the C4 file
   16 OPEN (UNIT=LC4,FILE=FLC4,STATUS='OLD')
      OPEN (UNIT=LSC,FILE=FLSC,STATUS='UNKNOWN')
c...
c...      print *,'standards file',fles,les
c...
      IF(FLES.NE.BLNK) THEN
C*
C* Process the cross section standards file to define ratios
        LES=ABS(LES)
        OPEN (UNIT=LES,FILE=FLES,STATUS='OLD',ERR=802)
        CALL LSTSTD(LES,MXMT,MXNP,NMT,MATS,MTS,ELO,EHI,EST,XST)
c...
c..    print *,'Standard reactions nmt',nmt
c...    do j=1,nmt
c...      print *,mats(j),mts(j),elo(j),ehi(j)
c...    end do
c...
C* Convert ratios to cross sections when possible - write to scratch
        OPEN (UNIT=LS2,FILE=FLS2,STATUS='UNKNOWN')
        IZA1=-1
        MT1 =-1
   17   READ (LC4,901,END=18) REC
        READ (REC(13:15),*) MF
        IF(MF.EQ.203) THEN
          READ(REC(59:76),*) RMT,RZA
          MTR =IFIX(RMT)
          IZAR=10*IFIX(RZA)
          DO I=1,NMT
            IF(MATS(I).EQ.IZAR .AND. MTS(I).EQ.MTR) THEN
C*            Matching standard reaction found - check energy            
C...
C...            print *,'"',rec( 1:49),'"'
C...            print *,'Require standard mat,mt',izar,mtr,iza1,mt1
C...
              READ (REC(23:31),931,ERR=801) FIN
              IF(FIN.GE.ELO(I) .AND. FIN.LE.EHI(I)) THEN
C*              Energy range of the standard is valid - retrieve              
                IF(MTR.NE.MT1 .OR. IZAR.NE.IZA1) THEN
C...
c...              print *,'      Load Standard mf,mt,za',mf,mtr,izar
C...
                  MF1 =3
                  MT1 =MTR
                  IZA1=IZAR
                  ZA1 =IZAR*0.1
                  MST =0
                  CALL GETSTD(LES,MXNP,ZA1,MF1,MT1,MST,QM,QI
     &                       ,NP,EST,XST,DST,RWO,MXRW)
                END IF
                INR=2
C*              Cross section standard and its absolute uncertainty
                RR=FINTXS(FIN,EST,XST,NP,INR,IER)
                DR=FINTXS(FIN,EST,DST,NP,INR,IER)
C*              Measured cross section ratio and its absolute uncertainty
                READ (REC(41:49),931,ERR=801) FXS
                READ (REC(50:58),931,ERR=801) DXS
C...
c...            print *,'np,fin,fxs,dxs,rr,dr',np,fin,fxs,dxs,rr,dr
C...
C*              Sum errors from measurement and standard
                DXS=DXS/FXS
                DR =DR /RR
                DXS=SQRT(DXS*DXS+DR*DR)
                FXS=FXS*RR
                DXS=DXS*FXS
                CALL CH9PCK(FXS,POU)
                REC(41:49)=POU
                CALL CH9PCK(DXS,POU)
                REC(50:58)=POU
                REC(59:76)='                  '
C*              Label reaction as converted from ratio by inserting "R"                
                II=122
                DO WHILE(II.GT.98 .AND. REC(II-1:II-1).EQ.' ')
                  II=II-1
                END DO
                REC(II:II)='R'
C...
c...                print *,ii,'"',rec(98:ii),'"'
C...
                REC(12:15)='   3'
                RC1=REC
                WRITE(LS2,901) REC
                GO TO 17
              END IF
            END IF
          END DO
        END IF
C* Suppress entry if the same is preceeded by the ratio entry
        IF(REC(1:40).NE.RC1(1:40) .OR.
     &     REC(98:II-1).NE.RC1(98:II-1) .OR.
     &     REC(II+1:130).NE.RC1(II+1:130))
     &  WRITE(LS2,901) REC
        GO TO 17
C* Reaction ratios converted, C4 file copied to scratch-2        
   18   CLOSE(UNIT=LC4)
        LC4=LS2
        REWIND LS2
      END IF
C* Write the banner to output
      WRITE(LTT,900) ' C4SORT - Sort data in a C4 file        '
      WRITE(LTT,900) ' -------------------------------        '
      WRITE(LTT,900) BLNK
      WRITE(LTT,900) '              Input instructions file : ',FLIN
      WRITE(LTT,900) '       Source EXFOR file in C4 format : ',FLC4
      WRITE(LTT,900) '       Sorted EXFOR file in C4 format : ',FLOU
      IF(LES.GT.0)
     &WRITE(LTT,900) '      Cross sect.React.Standards file : ',FLES
      WRITE(LTT,900) ' Directory path to nuclear level files: ',DPATH
      WRITE(LTT,900) BLNK
      IF(NALI.GT.0) THEN
        WRITE(LTT,900) ' Aliased quantities:                    '
        WRITE(LTT,900) ' Old:  MAT  MF  MT      ELVL New:  MAT  '
     &                ,'MF  MT      ELVL                        '
        DO I=1,NALI
          WRITE(LTT,*) '    ',ALIA(1,I),ALIA(3,I),ALIA(2,I),ALIA(4,I)
        END DO
      END IF
   19 CONTINUE
C*
C* Begin processing the source file
      NEN=0
      JLN=0
      MLN=0
      MOU=0
C* Copy records to scratch until the reference changes
   20 NSET=NSET+1
      READ (LC4,901,END=200) REC
c...
c...  print *,'nset',nset,'"',rec(1:30)
c...
   24 IF(REC(2:20).EQ.'                   ') GO TO 200
C* Change any alias ZA/MF/MT designations
      CALL CALIAS(NALI,ALIA,REC)
C* Decode the MF number
      READ (REC(13:15), * ,END=801,ERR=801) MF
      READ (REC(16:19), * ,END=801,ERR=801) MT
C* Decode the incident particle energy
      READ (REC(23:31),931,ERR=801) FIN
C* Decode secondary or discrete level energy
      READ (REC(77:85),931,ERR=801) FLV
C* Convert outgoing particle energy to level energy, if applicable
C* (same coding also after label 30)
      LB3=REC(95:97)
      IF(LB3.EQ.' E2' .AND. MT.EQ.51) THEN
        READ (REC(86:94),931,ERR=801) FLV1
        IF(NINT(FLV1).EQ.0 .OR. NINT(FLV).EQ.NINT(FLV1)) THEN
          LB3='LVL'
          REC(95:97)=LB3
          REC(86:94)='         '
C*        -- Approximately convert level energy to CM
          READ (REC(3: 5),*) AP
          READ (REC(9:11),*) AT
          IF(AT.GT.0) FLV=FLV*(AP+AT)/AT
        END IF
      END IF
C* Adjust the level energy if necessary (and possible)
      IF(FLV.GT.0) THEN
        IF(LB3.EQ.'LVL' .OR. LB3.EQ.'EXC') THEN
          READ (REC(1:20),902,ERR=801) IZI,IZA,MFC4,MTC4
          CALL MTTOZA(IZI,IZA,IZX,MTC4)
          FLV0=FLV
c...
c...      print *,'Calling MCHLVL',IZX,FLV,LPATH,IER
c... &           ,'"',DPATH(1:LPATH),'"'
c...
          CALL MCHLVL(IZX,FLV,DPATH,LPATH,IER)
c...
c...      print *,'"',rec(1:20),'"',iza,flv0,flv,ier
c...
        END IF
        IF(FLV.LT.1.E8) THEN
          WRITE(ELW,938) NINT(FLV)
          REC(77:85)=ELW
        END IF
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
c...  if(nen.gt.0) print *,'      at 20 nen,nset',nen,nset
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
        WRITE(ELV,938) NINT(DMY)
      END IF
C* Identify outgoing particle (if relevant)
      IF(MF.EQ.3 .OR. MF.EQ.5 .OR. MF.EQ.6) THEN
        POU=REC(68:76)
      ELSE
        POU='         '
      END IF
      IF(MF.EQ.3) THEN
        ENTI=REC(1:20)//POU//ELV//EIN
      ELSE
        ENTI=REC(1:20)//POU//EIN//ELV
      END IF
      IF(ENTI(20:20).EQ.'T' .OR. ENTI(20:20).EQ.'+') ENTI(20:20)=' '
      IF(ENTI(20:20).EQ.'1') ENTI(20:20)='M'
      IF(ENTI(20:20).EQ.'2') ENTI(20:20)='N'
      IF(ENTI(20:20).EQ.'3') ENTI(20:20)='O'
      ENT(NEN)=ENTI
      REF =REC(98:132)
      ZAMT=REC(1:20)
      IFIN=NINT(FIN)
      IFLV=NINT(FLV)
c...
c...  print '(i5,3a)',nen,ent(nen),' ',ref//' in30'
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
      WRITE(ELW,938) NINT(FLV)
      WRITE(POU,938) MIN(999999999,NINT(FIN))
C* Save record to RC6 field and sorting string to ELL
      IF(FLV.LT.1.E8) REC(77:85)=ELW
      RC6(IR)=REC
      ELL(IR)=ELW//POU//'  '
C*
C* Read a new record from the C4 file
      READ (LC4,901,END=200) REC
      IF(REC(1:20).EQ.'                    ') GO TO 34
C* Decode the MF number
      READ (REC(13:15), * ,END=801,ERR=801) MF
      READ (REC(16:19), * ,END=801,ERR=801) MT
C* Decode the incident particle energy
      READ (REC(23:31),931,ERR=801) FIN
C* Adjust the level energy if necessary (and possible)
      READ(REC(77:85),931,ERR=801) FLV
      LB3=REC(95:97)
      IF(LB3.EQ.' E2' .AND. MT.EQ.51) THEN
        READ (REC(86:94),931,ERR=801) FLV1
        IF(NINT(FLV1).EQ.0 .OR. NINT(FLV).EQ.NINT(FLV1)) THEN
          LB3='LVL'
          REC(95:97)=LB3
          REC(86:94)='         '
C*        -- Approximately convert level energy to CM
          READ (REC(3: 5),*) AP
          READ (REC(9:11),*) AT
          IF(AT.GT.0) FLV=FLV*(AP+AT)/AT
        END IF
      END IF
      IF(FLV.GT.0 .AND. (LB3.EQ.'LVL' .OR. LB3.EQ.'EXC')) THEN
        READ (REC(1:20),902,ERR=801) IZI,IZA,MFC4,MTC4
        CALL MTTOZA(IZI,IZA,IZX,MTC4)
        FLV0=FLV
c...
c...      print *,'Calling MCHLVL',IZX,FLV,LPATH,IER
c... &           ,'"',DPATH(1:LPATH),'"'
c...
        CALL MCHLVL(IZX,FLV,DPATH,LPATH,IER)
c...
c...      print *,'"',rec(1:20),'"',iza,flv0,flv,ier
c...
        IF(FLV.LT.1.E8) THEN
          WRITE(ELW,938) NINT(FLV)
          REC(77:85)=ELW
        END IF
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
          ENTI=RC1(1:20)//RC1(68:76)//ELV//EIN
          IF(ENTI(20:20).EQ.'T' .OR. 
     &       ENTI(20:20).EQ.'+') ENTI(20:20)=' '
          IF(ENTI(20:20).EQ.'1') ENTI(20:20)='M'
          IF(ENTI(20:20).EQ.'2') ENTI(20:20)='N'
          IF(ENTI(20:20).EQ.'3') ENTI(20:20)='O'
          ENT(NEN)=ENTI
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
c...         WRITE(41,'(4I6,A53)') I,LNE(I),NSE(I),ID2(I),ENT(I)
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
c...    print *,'      at 60 nen,nset',nen,nset
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
      IEN=1
      READ (LC4,901,END=64) REC
      IEN=0
c...
c...  print *,'ir',ir,'"',rec(1:30)
c...
      IF(REC(1:20).EQ.'                    ') GO TO 64
C* Change any alias ZA/MF/MT designations
      CALL CALIAS(NALI,ALIA,REC)
C* Decode the MF number
      READ (REC(13:15), * ,END=801,ERR=801) MF
C* Decode the incident particle energy
      READ (REC(23:31),931,ERR=801) FIN
c...
c...  print *,'zamt,rec,ref',zamt,'"',rec(1:30),rec(98:132)
c...
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
c...      if(nen.gt.0) print *,'      at64 nen,nset',nen,nset
c...      print *,'ELL,pou',ch20,pou
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
            ENTI=RC1(1:20)//'         '//EIN//'         '
            IF(ENTI(20:20).EQ.'T' .OR.
     &         ENTI(20:20).EQ.'+') ENTI(20:20)=' '
            IF(ENTI(20:20).EQ.'1') ENTI(20:20)='M'
            IF(ENTI(20:20).EQ.'2') ENTI(20:20)='N'
            IF(ENTI(20:20).EQ.'3') ENTI(20:20)='O'
            ENT(NEN)=ENTI
            WRITE(POU,938) MIN(999999999,NINT(FIN))
          ELSE
            WRITE(EIN,932) FIN
            AOU=RC1(59:67)
            READ (AOU,931) ANG
            WRITE(AOU,936) ANG+1
            POU=RC1(68:76)
            IF(POU(6:9).EQ.' 0.9' .OR.
     &         POU(6:9).EQ.'  .9') POU='   9999.9'

            ENTI=RC1(1:20)//POU//EIN//AOU
            IF(ENTI(20:20).EQ.'T' .OR.
     &         ENTI(20:20).EQ.'+') ENTI(20:20)=' '
            IF(ENTI(20:20).EQ.'1') ENTI(20:20)='M'
            IF(ENTI(20:20).EQ.'2') ENTI(20:20)='N'
            IF(ENTI(20:20).EQ.'3') ENTI(20:20)='O'
            ENT(NEN)=ENTI
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
c        WRITE(41,'(4I6,A53)') I,LNE(I),NSE(I),ID2(I),ENT(I)
c      END DO
c      STOP
c
      IF(IEN.EQ.1. OR. REC(1:20).EQ.'                    ') GO TO 200
      GO TO 24
C*
C* Source C4 file processed
  200 IF(NEN.GT.0) NSE(NEN)=NSET
c...
c...    if(nen.gt.0) print *,'      at 200 nen,nset',nen,nset
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
        WRITE(41,'(4I6,A53)') I,J,LNE(J),NSE(J),ENT(J)
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
c...
c...          print *,'i1,i2,jln',i1,i2,jln,JJ
c...
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
  802 WRITE(*,900) ' C4SORT ERROR - Non existent standards: ',FLES
      STOP 'C4SORT ERROR - Non existen standards file'
C*
  900 FORMAT(2A40)
  901 FORMAT(A132)
  902 FORMAT(I5,I6,2I4)
  921 FORMAT(A40,I6)
  931 FORMAT(BN,F9.0)
  932 FORMAT(F15.3)
  936 FORMAT(F9.6)
  938 FORMAT(I9)
  941 FORMAT(12X,I3,25X,F9.0)
      END
      SUBROUTINE MTTOZA(IZI,IZA,JZA,MT)
C-Title  : Subroutine EMTIZA
C-Purpose: Given projectile IZI, target IZA,  MT, assign residual JZA
      IF     (MT.EQ.  2) THEN
        JZA=IZA
      ELSE IF(MT.EQ.  4) THEN
        JZA=IZA+IZI-   1
      ELSE IF(MT.EQ. 11) THEN
        JZA=IZA+IZI-   2-1002
      ELSE IF(MT.EQ. 16) THEN
        JZA=IZA+IZI-   2
      ELSE IF(MT.EQ. 17) THEN
        JZA=IZA+IZI-   3
      ELSE IF(MT.EQ. 22) THEN
        JZA=IZA+IZI-   1-2004
      ELSE IF(MT.EQ. 23) THEN
        JZA=IZA+IZI-   1-3*2004
      ELSE IF(MT.EQ. 24) THEN
        JZA=IZA+IZI-   2-2004
      ELSE IF(MT.EQ. 25) THEN
        JZA=IZA+IZI-   3-2004
      ELSE IF(MT.EQ. 28) THEN
        JZA=IZA+IZI-   1-1001
      ELSE IF(MT.EQ. 29) THEN
        JZA=IZA+IZI-   1-2*2004
      ELSE IF(MT.EQ. 30) THEN
        JZA=IZA+IZI-   2-2*2004
      ELSE IF(MT.EQ. 32) THEN
        JZA=IZA+IZI-   1-1002
      ELSE IF(MT.EQ. 33) THEN
        JZA=IZA+IZI-   1-1003
      ELSE IF(MT.EQ. 34) THEN
        JZA=IZA+IZI-   1-2003
      ELSE IF(MT.EQ. 35) THEN
        JZA=IZA+IZI-   1-1002-2*2004
      ELSE IF(MT.EQ. 36) THEN
        JZA=IZA+IZI-   1-1003-2*2004
      ELSE IF(MT.EQ. 37) THEN
        JZA=IZA+IZI-   4
      ELSE IF(MT.EQ. 41) THEN
        JZA=IZA+IZI-   2-1001
      ELSE IF(MT.EQ. 42) THEN
        JZA=IZA+IZI-   3-1001
      ELSE IF(MT.EQ. 44) THEN
        JZA=IZA+IZI-   1-2*1001
      ELSE IF(MT.EQ. 45) THEN
        JZA=IZA+IZI-   1-1001-2004
      ELSE IF(MT.GE. 50 .AND. MT.LE.91) THEN
        JZA=IZA+IZI-   1
      ELSE IF(MT.EQ.102) THEN
        JZA=IZA+IZI
      ELSE IF(MT.EQ.103) THEN
        JZA=IZA+IZI-1001
      ELSE IF(MT.EQ.104) THEN
        JZA=IZA+IZI-1002
      ELSE IF(MT.EQ.105) THEN
        JZA=IZA+IZI-1003
      ELSE IF(MT.EQ.106) THEN
        JZA=IZA+IZI-2003
      ELSE IF(MT.EQ.107) THEN
        JZA=IZA+IZI-2004
      ELSE IF(MT.EQ.108) THEN
        JZA=IZA+IZI-2*2004
      ELSE IF(MT.EQ.109) THEN
        JZA=IZA+IZI-3*2004
      ELSE IF(MT.EQ.111) THEN
        JZA=IZA+IZI-2*1001
      ELSE IF(MT.EQ.112) THEN
        JZA=IZA+IZI-1001-2004
      ELSE IF(MT.EQ.113) THEN
        JZA=IZA+IZI-1003-2*2004
      ELSE IF(MT.EQ.114) THEN
        JZA=IZA+IZI-1002-2*2004
      ELSE IF(MT.EQ.115) THEN
        JZA=IZA+IZI-1001-1002
      ELSE IF(MT.EQ.116) THEN
        JZA=IZA+IZI-1001-1003
      ELSE IF(MT.EQ.117) THEN
        JZA=IZA+IZI-1002-2004
      ELSE IF(MT.GE.600 .AND. MT.LE.649) THEN
        JZA=IZA+IZI-1001
      ELSE IF(MT.GE.650 .AND. MT.LE.699) THEN
        JZA=IZA+IZI-1002
      ELSE IF(MT.GE.700 .AND. MT.LE.749) THEN
        JZA=IZA+IZI-1003
      ELSE IF(MT.GE.750 .AND. MT.LE.799) THEN
        JZA=IZA+IZI-2003
      ELSE IF(MT.GE.800 .AND. MT.LE.849) THEN
        JZA=IZA+IZI-2004
      ELSE
        JZA=0
      END IF
      RETURN
      END
      SUBROUTINE CH9PCK(FLT,CH9)
C-Title  : Subroutine CH9PCK
C-Purpose: Pack floating point number into 9-character fiels
      CHARACTER*9  CH9
      CHARACTER*15 C15
      WRITE(C15,'(1P,E15.8)') FLT
      L=15
      IF(C15(14:14).EQ.'0') THEN
       L=14
       C15(14:14)=C15(15:15)
      END IF
      CH9=C15(1:9)
      K=L-12
      DO I=1,K
        CH9(9-K+I:9-K+I)=C15(L-K+I:L-K+I)
      END DO
      RETURN
      END
      SUBROUTINE LSTSTD(LES,MXMT,MXNP,NMT,MATS,MTS,ELO,EHI,EN,XS)
C-Title  : Subroutine LSTSTD
C-Purpose: List the contents of the ENDF Cross Section Standards file
      PARAMETER (MXNB=40)
      CHARACTER*66 C66
      DIMENSION  MATS(MXMT),MTS(MXMT),ELO(MXMT),EHI(MXMT)
     &          ,EN(MXNP),XS(MXNP)
      DIMENSION  NBT(MXNB),INT(MXNB)
      NMT=0
      REWIND LES
      MAT=0
      MT0=0
C* Start processing the ENDF file
   20 CALL RDTEXT(LES,MAT,MF,MT,C66,IER)
      IF(IER.NE. 0) GO TO 90
      IF(MT.EQ.MT0) GO TO 20
      MT0=MT
      IF(MT.EQ.  0) GO TO 20
      IF(MF.NE.  3) GO TO 20
C* Identified MF3 section
      READ (C66,92) ZA
C* Read cross sections from the TAB1 record
      CALL RDTAB1(LES,C1,C2,L1,L2,N1,N2,NBT,INR,EN,XS,MXNP,IER)
      IF(IER.EQ. 9 ) STOP 'LSTSTD ERROR - MXNP Limit exceeded'
      IF(N1.GT.MXNB) STOP 'LSTSTD ERROR - MXNB Limit exceeded'
      LIS0=0
      IZA =10*NINT(ZA)+LIS0
      NMT=NMT+1
      MATS(NMT)=IZA
c...
c...        print *,nmt,mats(nmt),mt,N1,N2
c...
      MTS(NMT) =MT
      ELO(NMT)=EN(1)
      IF(ABS(ELO(NMT)-0.0253).LT.1.E-6) ELO(NMT)=EN(2)
      EHI(NMT)=EN(N2)
      IF(IER.EQ.0) GO TO 20
   90 RETURN
C*
   92 FORMAT(2F11.0,5I11)   
      END      
      SUBROUTINE GETSTD(LES,MXNP,ZA1,MF1,MT1,MST,QM,QI
     &                 ,NP,EN,XS,DX,RWO,MXRW)
C-Title  : Subroutine LSTSTD
C-Purpose: Get cross section and its absolute uncertainty
      PARAMETER (MXNB=40)
      CHARACTER*66 C66
      DIMENSION  EN(MXNP),XS(MXNP),DX(MXNP),RWO(MXRW)
      DIMENSION  NBT(MXNB),INT(MXNB)
C*
      REWIND LES
      DO I=1,MXNP
        DX(I)=0
      END DO
      NP =0
      I33=0
C* If MF10, define metastable state
      JST=1
      IF(MF1.EQ.10) JST=MST+1
      MF=MF1
      MT=MT1
C*
C* Search the ENDF file for section MT1 in file MF3
      CALL FINDMT(LES,ZA1,ZA,AWR,L1,L2,NC,N2,MAT,MF,MT,IER)
      IF(IER.NE. 0) GO TO 90
C*      
C* Reaction found - read cross sections from the TAB1 record
      IF(MF.EQ.10 .AND. JST.GT.N1) THEN
C*      Requested metastable state higher than available in MF10
        NP=0
        RETURN
      END IF
      DO J=1,JST
        CALL RDTAB1(LES,QM,QI,L1,L2,N1,NP,NBT,INT,EN,XS,MXNP,IER)
        IF(IER.EQ. 9 ) STOP 'GETSTD ERROR - MXNP Limit exceeded'
        IF(N1.GT.MXNB) STOP 'GETSTD ERROR - MXNB Limit exceeded'
      END DO
      IF(N1.NE.1 .OR. INT(1).NE.2) THEN
        PRINT *,'WARNING - used lin-lin interpolation instead of',INT(1)
      END IF
c...
c...      print *,'done mf/mt',mf,mt
c...
      IF(MF1.NE.3) GO TO 90
      MF=33
      MT=MT1
C*
C* Search the ENDF file for section MT1 in file MF33
      CALL FINDMT(LES,ZA1,ZA,AWR,L1,L2,NC,N2,MAT,MF,MT,IER)
      IF(IER.NE. 0) GO TO 90
c...
c...      print *,'Found mf,mt',mf,mt
c...
C*
C* Reaction found - read the covariance matrix
      MTL=L2
      NL =N2
C* If reaction is a constituent of a lumped reaction uncertainties
C* cannot be calculated
      IF(MTL.GT.0) GO TO 90
C... Current coding limitation
      IF(NL.GT.1) THEN
        PRINT *,'WARNING - Multiple sections in MF33 for MT',MT
        NL=1
      END IF
C...
C* Loop over all sections
      DO I=1,NL
        CALL RDHEAD(LES,MAT,MF,MT,XMF1,XLFS1,MATX,MTX,NC,NI,IER)
        IF(IER.NE.0) STOP 'RDHEAD ERROR reading MF33 (2)'
        IF(MATX.NE.0 .OR. MTX.NE.MT1) GO TO 90
C... Current coding limitation
        IF(NC.GT.0) THEN
          PRINT *,'WARNING - NC sections present in MF33 for MT',MT
          GO TO 90
        END IF
C...
        DO J=1,NI
          CALL RDLIST(LES,C1,C2,LT,LB,NT,NE,RWO,MXRW,IER)
          IF(IER.NE.0) THEN
            PRINT *, 'RDHEAD ERROR reading LIST in MF33',IER
            GO TO 90
          END IF
          IF(LB.EQ.1) THEN
C* Process section with LB=1 representation
            IF(LT.NE.0) THEN
C*            Warn about unsupported sections
              PRINT *, 'RDHEAD WARNING - unsupported MT/LB/LT',MT,LB,LT
            END IF
            LE =1
            LD =LE+NE
            LL =LD+NE
C*          Sort array to separate out energy and variance vector
            DO K=1,NT
              RWO(LL-1+K)=RWO(K)
            END DO
            DO K=1,NE
              RWO(LE-1+K)=RWO(LL+2*K-2)
              RWO(LD-1+K)=RWO(LL+2*K-1)
            END DO
            INR=1
C*          Approximately convert variances to lin-lin form
c...            ZR=0
c...            DD=MAX(ZR, RWO(LD)-(RWO(LD+1)-RWO(LD))/2 )
c...            D2=RWO(LD)
c...            DO K=3,NE
c...              D1=D2
c...              D2=RWO(LD-2+K)
c...              RWO(LD-2+K)=(D1+D2)/2
c...            END DO
c...            RWO(LD)=DD
c...            RWO(LD-1+NE)=D2+(D2-D1)/2
c...            INR=2
C*          Interpolate variance to cross section grid
            INR=1
            DO K=1,NP
              EIN=EN(K)
              DD=FINTXS(EIN,RWO,RWO(LD),NE,INR,IER)
              DX(K)=DX(K)+DD
c...
c...              print *,'  i,e,x,d',k,ein,xs(k),dx(k)
c...
            END DO
          ELSE IF(LB.EQ.5) THEN
C* Process section with LB=5 representation
            LS=LT
            LD=NE+1
            LL=LD
            DO K=2,NE
c...
c...                print *,'      ee,dx',rwo(k-1),rwo(ll)
c...
C*            Pick diagonal elements
              RWO(LD-2+K)=RWO(LL)
C*            Increment index to next diagonal (asymmetric/symmetric)
              IF(LS.EQ.0) THEN
                LL=LL+NE-1
              ELSE
                LL=LL+NE+1-K
              END IF
            END DO
            INR=1
C*          Approximately convert variances to lin-lin form
C...            ZR=0
C...            DD=MAX(ZR, RWO(LD)-(RWO(LD+1)-RWO(LD))/2 )
C...            D2=RWO(LD)
C...            DO K=3,NE
C...              D1=D2
C...              D2=RWO(LD-2+K)
C...              RWO(LD-2+K)=(D1+D2)/2
C...            END DO
C...            RWO(LD)=DD
C...            RWO(LD-1+NE)=D2+(D2-D1)/2
C...            INR=2
C*          Interpolate variance to cross section grid
            DO K=1,NP
              EIN=EN(K)
              DD=FINTXS(EIN,RWO,RWO(LD),NE,INR,IER)
              DX(K)=DX(K)+DD
c...
c...              print *,'  i,e,x,d',k,ein,xs(k),dx(k)
c...
            END DO
          ELSE
C* Warn about unsupported sections
            PRINT *, 'RDHEAD WARNING - unsupported LB in MT',MT,LB
          END IF
        END DO
        I33=I33+1
      END DO
C*
C* Convert variance to absolute uncertainty
   90 IF(I33.GT.0) THEN
        DO K=1,NP
          EIN=EN(K)
          XSI=XS(K)
          DDI=DX(K)
          DX(K)=SQRT(DDI)*XSI
        END DO
      END IF
C* All processing completed
      RETURN
      END      
      SUBROUTINE FINDMT(LEF,ZA0,ZA,AW,L1,L2,N1,N2,MAT,MF,MT,IER)
C-Title  : Subroutine FINDMT
C-Purpose: Find specified reaction in an ENDF file
C-Description:
C-D  The routine finds the specified material, file or section
C-D  in an ENDF file. The material may be characterised by the
C-D  ZA0 number defined as Z*1000+A+LIS0/10 (the decimal point
C-D  allows for isomeric states). Alternately, if ZA0<0, the
C-D  absolute integer value is interpreted as the required MAT
C-D  number. If MF and MT are non-zero, the file is scanned
C-D  until the value on the file matches the input value.
C-D
C-D  Notes:
C-D  - The search for metastable states by ZA0 is only possible
C-D    if MF1 MT451 data are on the file.
C-D  - In this case the actual file position is on the second
C-D    record of this section.
C-D  - Once the required ZA is identified, the ZA0 value is
C-D    redefined to -MAT to search by the MAT number in
C-D    consecutive searches, if required.
C-D
C-D  Error flags:
C-D  IER = 0  Normal termination.
C-D        1  Specified material not found.
C-D        2  End-of-file before material found.
C-D        3  Read error.
C-
      CHARACTER*66 C66
C* Initialise
      IER= 0
      MF0=MF
      MT0=MT
      MF =-1
      MT =-1
      MMM=-1
      ZA = 0
      IF     (ZA0.LT.0) THEN
        MAT0=-ZA0+0.1
      ELSE IF(ZA0.GT.0) THEN
        IZA0=ZA0*10
      ELSE
        CALL RDTEXT(LEF,MAT,MF,MT,C66,IER)
        IF(IER.GT.0) GO TO 80
        MAT0=-1
        GO TO 21
      END IF
C*
C* Loop to find the specified material
   20 CALL RDTEXT(LEF,MAT,MF,MT,C66,IER)
      IF(IER.GT.0 .OR. MAT.LT.0) GO TO 80
      IF(ZA0.LT.0) THEN
C* Case: Search by MAT number
        IF(MAT.NE.MAT0) GO TO 20
      ELSE
C* Case: Search by ZA number (including decimal LIS0)
        IF(MT.EQ.0) GO TO 20
        IF(MAT.EQ.MMM ) GO TO 20
        MMM=MAT
        READ (C66,92) ZA
        IZA=ZA*10
        IF(MF.EQ.1. AND. MT.EQ.451) THEN
          READ (LEF,92) DD,DD,LIS,LIS0
          IZA=IZA+LIS0
        END IF
        IF(IZA.NE.IZA0) GO TO 20
        ZA=IZA*0.1
        ZA0=-MAT
      END IF
C* Loop to find the file number
   21 IF(MF0.EQ. 0) GO TO 30
   22 IF(MF0.EQ.MF) GO TO 30
      CALL RDTEXT(LEF,MAT,MF,MT,C66,IER)
      IF(IER.GT.0 .OR. MAT.LE.0) GO TO 80
      GO TO 22
C* Loop to find the reaction type number
   30 IF(MT0.EQ. 0) GO TO 40
   32 IF(MT0.EQ.MT) GO TO 40
      CALL RDTEXT(LEF,MAT,MF,MT,C66,IER)
      IF(IER.GT.0 .OR. MAT.LE.0) GO TO 80
      IF(MF0.GT.0 .AND. MF.GT.MF0) GO TO 20
      GO TO 32
C* Normal termination
   40 READ (C66,92) ZA,AW,L1,L2,N1,N2
      RETURN
C*
C* Error traps
   80 IER=IER+1
      RETURN
C*
   92 FORMAT(2F11.0,4I11.0,I4,I2,I3,I5)
      END
      FUNCTION FINTXS(EIN,EN,XS,NP,INR,IER)
C-Title  : Function FINTXS
C-Purpose: Interpolate the cross section table to EIN
      DIMENSION EN(NP),XS(NP)
      IF     (EIN.LT.EN(1)) THEN
        FINTXS=XS(1)
        RETURN
      ELSE IF(EIN.GT.EN(NP)) THEN
        FINTXS=XS(NP)
        RETURN
      END IF
      DO I=2,NP
        I2=I
        IF(EN(I).GE.EIN) GO TO 22
      END DO
      IER=11
   22 I1=I2-1
      IF(INR.EQ.2) THEN
        FF=XS(I1)+(XS(I2)-XS(I1))*(EIN-EN(I1))/(EN(I2)-EN(I1))
      ELSE
        FF=XS(I1)
      END IF
      FINTXS=FF
      RETURN
      END
      SUBROUTINE RDTEXT(LEF,MAT,MF,MT,REC,IER)
C-Title  : RDTEXT Subroutine
C-Purpose: Read a text record to an ENDF file
      CHARACTER*66  REC
      READ (LEF,40,END=81,ERR=82) REC,MAT,MF,MT
      IER=0
      RETURN
   81 IER=1
      RETURN
   82 IER=2
      RETURN
   40 FORMAT(A66,I4,I2,I3,I5)
      END
      SUBROUTINE RDHEAD(LEF,MAT,MF,MT,C1,C2,L1,L2,N1,N2,IER)
C-Title  : Subroutine RDHEAD
C-Purpose: Read an ENDF HEAD record
C-Description:
C-D  The HEAD record of an ENDF file is read. The following error
C-D  conditions are trapped by setting the IER flag:
C-D    IER = 0  Normal termination
C-D          1  End-of-file
C-D          2  Read error
C-
      READ (LEF,92) C1,C2,L1,L2,N1,N2,MAT,MF,MT
      RETURN
   92 FORMAT(2F11.0,4I11.0,I4,I2,I3,I5)
      END
      SUBROUTINE RDTAB1(LEF,C1,C2,L1,L2,N1,N2,NBT,INR,EN,XS,NMX,IER)
C-Title  : Subroutine RDTAB1
C-Purpose: Read an ENDF TAB1 record
C-Description:
C-D  The TAB1 record of an ENDF-formatted file is read.
C-D  Error condition:
C-D    IER=1  End-of-file
C-D        2  Read error
C-D        9  Available field length NMX is exceeded.
C-
      DIMENSION    NBT(1),INR(1)
      DIMENSION    EN(NMX), XS(NMX)
C*
      IER=0
      READ (LEF,902,END=100,ERR=200) C1,C2,L1,L2,N1,N2
      READ (LEF,903,END=100,ERR=200) (NBT(J),INR(J),J=1,N1)
      JP=N2
      IF(N2.GT.NMX) THEN
        JP=NMX
        IER=9
      END IF
      READ (LEF,904,END=100,ERR=200) (EN(J),XS(J),J=1,JP)
      RETURN
  100 IER=1
      RETURN
  200 IER=2
      RETURN
C*
  902 FORMAT(2F11.0,4I11)
  903 FORMAT(6I11)
  904 FORMAT(6F11.0)
      END
      SUBROUTINE RDLIST(LEF,C1,C2,L1,L2,N1,N2,VK,MVK,IER)
C-Title  : Subroutine RDLIST
C-Purpose: Read an ENDF LIST record
C-Description:
C-D LEF     ENDF file unit number
C-D C1...N2 parameters of the LIST record
C-D VK      Array of the LIST 
C-D MVK     Maximum size of the VK array
C-D IER     Error flag with the following meaning:
C-D     -1  Maximum array size exceeded
C-
      DOUBLE PRECISION RUFL,RR(6)
      DIMENSION    VK(1)
C*
      READ (LEF,902) C1,C2,L1,L2,N1,N2
      IF(N1+5.GT.MVK) THEN
        IER=-1
        RETURN
      END IF
      IF(N1.EQ.0) RETURN
C* Read the LIST2 entries, watch for underflow
      NUFL=0
      RUFL=1
      DO J=1,N1,6
        READ (LEF,903) (RR(K),K=1,6)
        DO K=1,6
          IF(RR(K).NE.0 .AND. ABS(RR(K)).LT.1.E-30) THEN
            NUFL=NUFL+1
            IF(ABS(RR(K)).LT.ABS(RUFL)) RUFL=RR(K)
          END IF
          VK(J-1+K)=RR(K)
        END DO
      END DO
      IF(NUFL.GT.0) THEN
        PRINT *,' RDLIST WARNING - Underflow conditions',NUFL
        PRINT *,'                        Minimum number',RUFL
      END IF
      RETURN
C*
  902 FORMAT(2F11.0,4I11)
  903 FORMAT(6F11.0)
      END
      SUBROUTINE C4SINP(LIN,LTT,FLC4,FLOU,FLES,NALI,ALIA
     &                 ,DPATH,LPATH,IER)
C-Title  : Subroutine C4SINP
C-Purpose: Process input instructions for C4SORT
      PARAMETER    (MXKW=10)
      CHARACTER*80  DPATH
      CHARACTER*40  FLC4,FLOU,FLES
      CHARACTER*14  WORD,ALIA(4,1)
      CHARACTER*10  KWRD(MXKW),REC(8),C10
C*
      DATA KWRD
     1/'$* C4SORT ','   FLC4IN ','   FLC4OU ','   FLESTD ','          '
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
C* '   FLESTD '
  140 FLES=REC(2)//REC(3)//REC(4)//REC(5)
      GO TO 90
C* Unused keywords
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
      SAVE IZA0,IFND,NLVL,ELVL
C*
      DATA IZA0,IFND/ -1, 0 /
C*
c...
c...      print *,'MCHLVL IZA,IZA0',IZA,IZA0
c...
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
c...      print *,'MCHLVL iza,iza0,ifnd,ier',iza,iza0,ifnd,ier
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
      CLOSE (13)
      return
      end
