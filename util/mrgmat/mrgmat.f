      PROGRAM MRGMAT
C-Title  : MRGMAT Program
C-Purpose: Merge ENDF material files into a single library
C-Author : A.Trkov, Institute J.Stefan, Ljubljana, Slovenia
C-Version: 2000 - Original code
C-V  01/04 Increase MXMT from 800 to 2000 (A.Trkov)
C-V  01/11 Improve diagnostics for duplicate materials (A.Trkov)
C-V  02/05 Process Windows-NT directory listing (A.Trkov)
C-V  03/01 Fix bug in interactive input (A.Trkov)
C-V  03/11 Extend path length
C-V        Update ENDF file numbering (col.75-80)
C-V  05/09 Support for Windows-XP directory format (A.Trkov)
C-V  06/12 Increase MXMT from 2000 to 4000 (A.Trkov)
C-V  07/11 Test for empty files (A. Trkov)
C-V  08/10 Trap attempts to open non-existent ENDF files (A. Trkov)
C-V  11/04 Fix case of merging a file with a single material (A. Trkov)
C-M
C-M  Manual for Program MRGMAT
C-M  =========================
C-M    The program reads the list of filenames containing
C-M  nuclear data in ENDF format and copies them to a scratch
C-M  file MRGMAT.TMP (fixed filename). The MAT numbers are stored.
C-M  The scratch file is then processed, writing the data sorted
C-M  by ascending MAT numbers onto MRGMAT.OUT (fixed filename).
C-M  Data sets with duplicate MAT numbers are skipped (the data
C-M  that appear first on the list are copied to output).
C-M    The list of filenames may be typed from keyboard input
C-M  and is terminated by "-" or an end-of-file mark.
C-M  Alternatively, the default input may be redirected to a
C-M  list file. The list file format from the DIR command of
C-M  Windows-DOS (95,98,NT) is recognised automatically and can
C-M  be used instead.
C-
      PARAMETER    (MXMT=4000)
      CHARACTER*80  RECI,FLNM
      CHARACTER*66  CH66,HH66
      CHARACTER*40  BLNK,PATH,FLTM,FLOU,FLER
      DIMENSION     MTLS(MXMT),IZLS(MXMT)
C*
      DATA BLNK/'                                        '/
     1     FLTM/'MRGMAT.TMP'/
     1     FLOU/'MRGMAT.OUT'/
     9     FLER/'MRGMAT.LOG'/
C*
      DATA LIN,LTT,LOU,LTM,LEN,LER/ 5, 6, 1, 3, 2, 7 /
C*
      WRITE(LTT,91) ' MRGMAT - ENDF Material Merging Utility '
      WRITE(LTT,91) ' ====================================== '
      WRITE(LTT,91)
      PATH=BLNK
      LPTH=0
      IDOS=0
      IFL =0
      NMT =0
      IDU =0
      JS  =0
      KMT =MXMT
      OPEN (UNIT=LTM,FILE=FLTM,STATUS='UNKNOWN')
      OPEN (UNIT=LOU,FILE=FLOU,STATUS='UNKNOWN')
      OPEN (UNIT=LER,FILE=FLER,STATUS='UNKNOWN')
      WRITE(LER,91) ' MRGMAT - ENDF Material Merging Utility '
      WRITE(LER,91) ' ====================================== '
      WRITE(LER,91)
C* Read the directory listing
   20 WRITE(LTT,91) '$Enter the source ENDF file           : '
   21 READ (LIN,93,END=60) RECI
c...  IF(RECI(1:40).EQ.BLNK) GO TO 60
      IF(RECI(1: 1).EQ.'-' ) GO TO 60
C* Allow piping DIR listing of Dos (Windows 95,98,NT):
C* Skip header records in the DIR list of DOS
      IF(RECI(1:20).EQ.BLNK(1:20)) GO TO 21
C* Skip "Volume" information from a directory listing
      IF(RECI( 1: 8).EQ.' Volume ') GO TO 21
C* End of directory record in the DIR list of DOS
      IF(RECI(12:18).EQ.'file(s)' .OR.
     1   RECI(18:24).EQ.'File(s)' ) GO TO 60
C* Identify directory path in the DIR list of DOS
      IF(RECI(1:8).EQ.' Directo') THEN
        WRITE(LOU,95) ' MRGMAT files on  '//RECI, 0, 0, 0
        PATH=RECI(15:54)
   22   IF(PATH(LPTH+1:LPTH+1).NE.' ') THEN
          LPTH=LPTH+1
          GO TO 22
        END IF
        IF(PATH(LPTH:LPTH).NE.'\') THEN
          LPTH=LPTH+1
          IF(LPTH.GT.40) THEN
            WRITE(LTT,91) ' MRGMAT ERROR - Path length exceeded    '
            WRITE(LTT,93) RECI
            STOP 'MRGMAT ERROR - Pathlength too long'
          END IF
          PATH(LPTH:LPTH)='\'
        END IF
        WRITE(LTT,91) ' Process Windows directory listing      '
        IDOS=1
        GO TO 20
      END IF
C* Extract the filenames but skip directories
      IF(IDOS.NE.0) THEN
        IF(RECI(25:29).EQ.'<DIR>') THEN
C*        Windows-NT directory listing
          WRITE(LTT,91) ' Identified Windows-NT directory listing'
          IDOS=2
          GO TO 21
        ELSE IF(RECI(22:26).EQ.'<DIR>') THEN
C*        Windows-XP directory listing
          WRITE(LTT,91) ' Identified Windows-XP directory listing'
          IDOS=3
          GO TO 21
        END IF
        IF(IDOS.LT.2 .AND. RECI(15:15).EQ.':') IDOS=2
        IF(IDOS.EQ.2 .AND. RECI(39:39).NE.' ') IDOS=3
        IF(RECI(16:20).EQ.'<DIR>') GO TO 21
        IF(IDOS.EQ.1) THEN
          RECI(9:9)='.'
          LNAM=0
          DO 24 I=1,12
          IF(RECI(I:I).EQ.' ') GO TO 24
          LNAM=LNAM+1
          IF(LNAM.EQ.I) GO TO 24
            RECI(LNAM:LNAM)=RECI(I:I)
            RECI(I:I)=' '
   24     CONTINUE
          FLNM=PATH(1:LPTH)//RECI(1:LNAM)
        ELSE IF(IDOS.EQ.2) THEN
C* Case: Windows-NT directory listing
          FLNM=PATH(1:LPTH)//RECI(40:80)
        ELSE
C* Case: Windows-XP directory listing
          FLNM=PATH(1:LPTH)//RECI(37:80)
        END IF
      ELSE
C* Simple ASCII file list
        FLNM=RECI
      END IF
C* Open the file
      WRITE(LTT,91) ' Processing ENDF file                 : ',FLNM
      WRITE(LER,91) ' Processing ENDF file                 : ',FLNM
      OPEN (UNIT=LEN,FILE=FLNM,STATUS='OLD',ERR=25)
      GO TO 26
C*    Trap non-existent files
   25 WRITE(LTT,91) '            ERRORR - File does not exist'
      WRITE(LER,91) '            ERRORR - File does not exist'
      GO TO 20
C*    Process the file
   26 IFL =IFL+1
C* Header card
      READ (LEN,95,ERR=43,END=43) CH66,MAT,MF,MT,IS
      WRITE(LTT,95) ' '//CH66(1:65)
      WRITE(LTT,95)
      WRITE(LER,95) ' '//CH66(1:65)
      WRITE(LER,95)
      IF(MAT.GT. 0 .AND. (MF.EQ.1 .AND. MT.EQ.451)) THEN
C* Fix case when header record is missing
        READ (CH66,94,ERR=40) ZA0,AWR,IDM,IDM,IDM,IDM
        GO TO 42
      END IF
C* Merge files to a scratch file recording MAT and ZA numbers
   40 READ (LEN,95,END=43) CH66,MAT,MF,MT,IS
   42 IF(MAT.LT. 0) GO TO 44
      WRITE(LTM,95) CH66,MAT,MF,MT
      IF(MAT.GT. 0) THEN
        IF(MA0.NE.MAT) THEN
          MA0=MAT
          READ (CH66,94) ZA0
          READ (LEN,95) CH66,MAT,MF,MT,IS
          READ (CH66,94) DMY,DMY,LIS,LIS0
          IZ0=10*NINT(ZA0)+LIS0
          WRITE(LTM,95) CH66,MAT,MF,MT
        END IF
      ELSE
        NMT=NMT+1
        IF(NMT.GT.MXMT) STOP 'MRGMAT ERROR - MXMT Limit exceeded'
        MTLS(NMT)=MA0
        IZLS(NMT)=IZ0
      END IF
      GO TO 40
C* Trap incomplete or empty files
   43 WRITE(LTT,91) ' WARNING - Incomplete file (No TEND)  : ',FLNM
      WRITE(LER,91) ' WARNING - Incomplete file (No TEND)  : ',FLNM
C* One ENDF file processed - try next one
   44 CLOSE(UNIT=LEN)
      GO TO 20
C* All files written to temporary file
   60 KM1 =KMT
      KMT =0
      JMT =10000
      IZ0 =-1
      IF(IDOS.EQ.0)
     1WRITE(LOU,99) 0, 0, 0
      IDOS=1
C* Find the next material to be written
      DO 62 I=1,NMT
      IF(MTLS(I).EQ.JMT) THEN
        WRITE(LTT,97) JMT,0.1*IZ0
        WRITE(LER,97) JMT,0.1*IZ0
        IF(IZLS(I).NE.IZ0) THEN
          WRITE(LTT,98) 0.1*IZLS(I)
          WRITE(LER,98) 0.1*IZLS(I)
        END IF
        MTLS(I)=-JMT
        IDU =IDU+1
        GO TO 62
      END IF
      IF(MTLS(I).GT.0 .AND. IZLS(I).EQ.IZ0) THEN
        WRITE(LTT,96) 0.1*IZ0,MTLS(I)
        WRITE(LER,96) 0.1*IZ0,MTLS(I)
        MTLS(I)=-JMT
        IDU =IDU+1
        GO TO 62
      END IF
      IF(MTLS(I).LT.  0) GO TO 62
      IF(MTLS(I).GT.JMT) GO TO 62
        KMT=I
        JMT=MTLS(I)
        IZ0=IZLS(I)
   62 CONTINUE
      IF(KMT.LE.0) GO TO 80
      IF(KMT.LT.KM1) THEN
C... lahey compiler has problems with rewinding very long files
C...    REWIND(UNIT=LTM)
        CLOSE(UNIT=LTM)
        OPEN (UNIT=LTM,FILE=FLTM,STATUS='OLD')
      END IF
      MTLS(KMT)=-JMT
C* Copy the file to output
   70 READ (LTM,95) CH66,MAT,MF,MT,IS
      IF(MAT.LT.0) THEN
        WRITE(LTT,98) FLOAT(JMT)
        WRITE(LER,98) FLOAT(JMT)
        GO TO 60
      END IF
      IF(MAT.NE.JMT) GO TO 70
   72 JS  =JS+1
      IF(JS.GT.99999) JS=0
      IF(MT.EQ.0 .AND. MF.GT.0) JS=99999
      WRITE(LOU,95) CH66,MAT,MF,MT,JS
      READ (LTM,95,END=60) CH66,MAT,MF,MT,IS
      IF(MAT.GT. 0) GO TO 72
      JS  =0
      WRITE(LOU,95) CH66,MAT,MF,MT,JS
      GO TO 60
C*
   80 CH66=BLNK//BLNK(1:26)
      WRITE(LOU,95) CH66,-1, 0, 0, 0
      CLOSE(UNIT=LOU)
      WRITE(LTT,92)
      WRITE(LTT,92) ' Number of ENDF files processed       : ',IFL
      WRITE(LTT,92) ' Number of materials processed        : ',NMT
      WRITE(LTT,92) ' Number of duplicate materials skipped: ',IDU
      WRITE(LER,92)
      WRITE(LER,92) ' Number of ENDF files processed       : ',IFL
      WRITE(LER,92) ' Number of materials processed        : ',NMT
      WRITE(LER,92) ' Number of duplicate materials skipped: ',IDU
      CLOSE(UNIT=LER)
      STOP 'MRGMAT Completed'
C*
   91 FORMAT(A40,A80)
   92 FORMAT(A40,I5)
   93 FORMAT(A80)
   94 FORMAT(2F11.0,4I11)
   95 FORMAT(A66,I4,I2,I3,I5)
   96 FORMAT(' WARNING - Duplicate ZA',F9.1,'  MAT',I6)
   97 FORMAT(' WARNING - Duplicate MAT',I6,'  ZA',F9.1)
   98 FORMAT('           ERROR   -    Skipped ZA',F9.1)
   99 FORMAT(' MRGMAT merge ENDF files from list      '
     1      ,'                          ',I4,I2,I3,I5)
C*
      END
