      PROGRAM ENDRES
C-Title  : Program ENDRES
C-Purpose: Insert resonance data into an ENDF file
C-Author : A. Trkov, IAEA-NDS, Vienna, Austria
C-Version: November 2003
C-M
C-M  Manual for ENDRES Program
C-M  =========================
C-M
C-M  The program is primarily intended to complete a partial ENDF
C-M  file produced from a nuclear model calculation by inserting the
C-M  resonance parameter data from another ENDF file.
C-M
C-M  Procedures
C-M  The following tasks are performed:
C-M  - The resonance parameters MF 2 are inserted into the file.
C-M  - The energy range of the cross sections for total, elastic,
C-M    capture and fission (if present) are extended down to 1.E-5 eV.
C-M  - The elastic angular distributions are extended down to
C-M    1.E-5 eV by repeating the distribution of the first available
C-M    energy point.
C-M  - Photon spectra in MF 6 are extrapolated to thermal energies in
C-M    a similar way like the elastic angular distributions.
C-M    
C-
      PARAMETER    (MXRW=10000, MXNB=20, MXMT=100)
C*
      CHARACTER*66  BL66,CH66,HD66
      CHARACTER*40  BLNK,FLNM,FLEM,FLRR,FLOU,FLLG
      CHARACTER*11  CHZA,CHAW
C*
      DIMENSION     NBT(MXNB),INR(MXNB),MTLS(MXMT),ELOW(MXMT)
      DIMENSION     RWO(MXRW)
C*
      DATA BLNK/'                                        '/
     1     FLEM/'empire.end                              '/
     2     FLRR/'mughrr.end                              '/
     3     FLOU/'endf.dat'/
     4     FLLG/'endres.lst'/
      DATA LEM,LRR,LOU,LLG,LKB,LTT
     &    /  1,  2,  3,  4,  5,  6 /
C*
      NMT=0
      ELE=1.E-5
C-F  Write ENDRES banner
      WRITE(LTT,691) ' ENDRES - Add Resonance Data to ENDF    '
      WRITE(LTT,691) ' ===================================    '
      WRITE(LTT,691)
C-F  Initialise constants
      BL66=BLNK//BLNK(1:26)
      NS  =-1
C-F  Define input and output files
C*   Source ENDF file to be edited
      GO TO 12
   11 WRITE(LTT,691) ' ERROR - REDO: File does not exist      ',FLEM
   12 WRITE(LTT,691) '$Enter source ENDF file to be edited  : '
      READ (LKB,691) FLNM
      IF(FLNM.NE.BLNK) FLEM=FLNM
      OPEN (UNIT=LEM,FILE=FLEM,STATUS='OLD',ERR=11)
      CALL RDTEXT(LEM,M1,M2,M3,HD66,IER)
      WRITE(LTT,696) ' File header: '//HD66(1:52)
C*   Source resonance parameter ENDF file
      GO TO 16
   15 WRITE(LTT,691) ' ERROR - REDO: File does not exist      ',FLRR
   16 WRITE(LTT,691) '$Enter res.param.  ENDF file          : '
      READ (LKB,691) FLNM
      IF(FLNM.NE.BLNK) FLRR=FLNM
      OPEN (UNIT=LRR,FILE=FLRR,STATUS='OLD',ERR=15)
      CALL RDTEXT(LRR,I1,I2,I3,CH66,IER)
      WRITE(LTT,696) ' File header: '//CH66(1:52)
C*   Output ENDF file
      WRITE(LTT,691) '$Enter output ENDF file               : '
      READ (LKB,691) FLNM
      IF(FLNM.NE.BLNK) FLOU=FLNM
      OPEN (UNIT=LOU,FILE=FLOU,STATUS='UNKNOWN')
C-F  Define the LSSF flag
      LSSF=0
      WRITE(LTT,691) ' Enter LSSF flag                        '
      WRITE(LTT,691) ' LSSF=0 Calculate x.s. from URRP        '
      WRITE(LTT,691) '$     1 URRP for self-shielding only :  '
      READ (LKB,691,END=18) FLNM
      IF(FLNM.NE.BLNK) READ (FLNM,698) LSSF
C* ENDRES log file
   18 OPEN (UNIT=LLG,FILE=FLLG,STATUS='UNKNOWN')
      WRITE(LLG,691) ' ENDRES - Add Resonance Data to ENDF    '
      WRITE(LLG,691) ' ===================================    '
      WRITE(LLG,691)
      WRITE(LLG,691) ' Source ENDF file to be edited        : ',FLEM
      WRITE(LLG,691) '                          File header : ',HD66
      WRITE(LLG,691) ' Source resonance parameter file      : ',FLRR
      WRITE(LLG,691) '                          File header : ',CH66
      WRITE(LLG,691)
      WRITE(LLG,691) ' Output ENDF file                     : ',FLOU
      WRITE(LLG,691) '                          File header : ',HD66
C* Write the header to output and log file
      CALL WRTEXT(LOU, M1,M2,M3,NS,HD66)
C*
C-F  Identify ZA and AWR on the source file to be edited
  100 CALL RDTEXT(LEM,MAT,MF,MT,CH66,IER)
      IF(MAT.LT.0) GO TO 274
      IF(MF.GT.1) THEN
        WRITE(LTT,691) ' ERROR - No MF 1 present on ENDF file : ',FLEM
        WRITE(LLG,691) ' ERROR - No MF 1 present on ENDF file : ',FLEM
        GO TO 284
      END IF
      READ (CH66,891) ZA0,AWR
      IZA=NINT(ZA0)
      CHZA=CH66( 1:11)
      CHAW=CH66(12:22)
C*
C-F  Find equivalent nuclide (matching ZA) on the resonance file
      MFR=0
      MTR=0
      ERH=0
      CALL FINDMT(LRR,ZA0,ZA,AW,L1,L2,N1,N2,MAR,MFR,MTR,IER)
      IF(IER.GT.0) THEN
        IF     (IER.EQ.1) THEN
          WRITE(LTT,692) ' ERROR - No Resonance MAT matching ZA : ',IZA
          WRITE(LLG,692) ' ERROR - No Resonance MAT matching ZA : ',IZA
        ELSE IF(IER.EQ.2) THEN
          WRITE(LTT,691) ' ERROR - EOF on ENDF file             : ',FLEM
          WRITE(LLG,691) ' ERROR - EOF on ENDF file             : ',FLEM
        ELSE
          WRITE(LTT,691) ' ERROR - reading ENDF file            : ',FLEM
          WRITE(LLG,691) ' ERROR - reading ENDF file            : ',FLEM
        END IF
        WRITE(LTT,691) '         Source file copied unchanged '
        WRITE(LLG,691) '         Source file copied unchanged '
        GO TO 180
      END IF
C* Set the flag for the presence of resonance parameters
      CH66(23:33)='          1'
C* Copy the beginning of file MF1 from the source ENDF file
      CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
      CALL RDTEXT(LEM,MAT,MF,MT,CH66,IER)
      CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
      CALL RDTEXT(LEM,MAT,MF,MT,CH66,IER)
      CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
      CALL RDTEXT(LEM,MAT,MF,MT,CH66,IER)
      READ (CH66,891) C1,C2,L1,L2,N1,N2
      NX=N1+3
      CALL WRCONT(LOU,MAT,MF,MT,NS,C1,C2,L1,L2,NX,N2)
C* Copy the comments
      DO I=1,N1
        CALL RDTEXT(LEM,MAT,MF,MT,CH66,IER)
        CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
      END DO
C* Add to comments the signature of ENDRES
      CH66=' *** ENDRESS - Merging source and resonance ENDF files'
     *     //BLNK
      CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
      CH66='             Source ENDF '//FLEM
      CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
      CH66='          Resonance ENDF '//FLRR
      CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
C* Copy the rest of MF1
  110 CALL RDTEXT(LEM,MAT,MF,MT,CH66,IER)
      CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
      IF(MF.NE.0) GO TO 110
C* Check AWR consistency on the resonance file
      IF(ABS(AWR-AW).GT.AWR*1.E-4) THEN

        PRINT *,'WARNING - Unmatched AWR',AWR,AW

      END IF
C*
C-F  Process the resonance data and define the energy range
      IF(MF.EQ.2 .AND. MT.EQ.151) REWIND LRR
  120 CALL RDTEXT(LRR,MA1,MF,MT,CH66,IER)
      IF(MA1.NE.MA1 .OR. MF.NE.2 .OR. MT.NE.151) GO TO 120
      CH66( 1:11)=CHZA
      CH66(12:22)=CHAW
      CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
      READ (CH66,891) XZA,XAWR,IDMY,IDMY,NIS,IDMY
      CALL RDTEXT(LRR,MA1,MF,MT,CH66,IER)
      READ (CH66,891) XZA,XABN,IDMY,LFW,NER,IDMY
      CH66( 1:11)=CHZA
      CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
C* Process a resonance section
  122 CALL RDTEXT(LRR,MA1,MF,MT,CH66,IER)
      CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
      READ (CH66,891) ERL,EH,LRU,LRF,NRO,NAPS
      IF(LRU.EQ.0) THEN
C* Case: no resonance parameters
        GO TO 128
      ELSE IF(LRU.EQ.1) THEN
C* Case: Resolved resonance range present -  copy to output
        ERH=EH
        IF(NIS.GT.1) THEN
          WRITE(LTT,691) ' ENDRES WARNING - Multi-isotope file    '
          WRITE(LTT,691) '                  limited support       '
          WRITE(LLG,691) ' ENDRES WARNING - Multi-isotope file    '
          WRITE(LLG,691) '                  limited support       '
          GO TO 128
        END IF
C*       No special procedures needed for single range evaluations
        IF(NER.EQ.1) GO TO 128
C*       Processing the resolved resonance range
        IF(LRF.EQ.1 .OR. LRF.EQ.2 .OR. LRF.EQ.3) THEN
C*         Copy the energy-dependent radius
          IF(NRO.NE.0) THEN
            CALL RDTEXT(LRR,MA1,MF,MT,CH66,IER)
            READ (CH66,891) DMY,DMY,IDMY,IDMY,NR,IDMY
            CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
            NN=(NR-1)/3+1
            DO I=1,NN
              CALL RDTEXT(LRR,MA1,MF,MT,CH66,IER)
              CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
            END DO
          END IF
          CALL RDTEXT(LRR,MA1,MF,MT,CH66,IER)
          CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
          READ (CH66,891) DMY,DMY,IDMY,IDMY,NLS,IDMY
          DO L=1,NLS
            CALL RDTEXT(LRR,MA1,MF,MT,CH66,IER)
            CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
            READ (CH66,891) DMY,DMY,IDMY,IDMY,IDMY,NRS
            DO J=1,NRS
              CALL RDTEXT(LRR,MA1,MF,MT,CH66,IER)
              CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
            END DO
          END DO
          GO TO 122
        ELSE

          WRITE(LTT,692) ' ENDRES ERROR - No coding fo NER= 2/LRF=',LRF  
          WRITE(LLG,692) ' ENDRES ERROR - No coding fo NER= 2/LRF=',LRF  

          STOP 'ENDRES ERROR - No coding fo NER=2/LRF'

        END IF

      ELSE IF(LRU.EQ.2) THEN
C* Case: Unresolved resonance range present - process LSSF and copy
        CALL RDTEXT(LRR,MA1,MF,MT,CH66,IER)
        READ (CH66,891) DMY,DMY,LSSF1
        WRITE(CH66(23:33),892) LSSF
        CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
        IF(LSSF.EQ.0) ERH=EH
      ELSE
C* Case: Unknown LRU - copy as is to output
        WRITE(LTT,692) ' ENDRES WARNING - Unrecognised LRU =    ',LRU
        WRITE(LTT,691) '                  limited support       '
        GO TO 128
      END IF
C* Copy the rest of the resonance file to output
  128 CALL RDTEXT(LRR,MA1,MF,MT,CH66,IER)
      CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
      IF(MF.NE.0) GO TO 128
C*
C-F  Find the pointwise cross sections in the source file
  130 CALL RDTEXT(LEM,MA1,MF,MT,CH66,IER)
      IF(IER.GT.0 .OR. MA1.LT.0) THEN

        PRINT *,'WARNING - No pointwise data on source file'

      END IF
      IF(MA1.NE.MAT .OR. MF.NE.3) GO TO 130
      IF(MT.NE.1) THEN

        PRINT *,'WARNING - First MT not total'

      END IF
C*
C-F  Add zero background to all non-threshold reactions
  132 CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
C* Read the cross sections
      NMX=MXRW/2
      L1 =1
      L2 =1+NMX
      K1 =L1+2
      K2 =L2+2
      NM2=NMX-2
      CALL RDTAB1(LEM,QM,QI,LL,LR,NR,NP,NBT,INR
     &           ,RWO(K1),RWO(K2),NMX,IER)
C* First energy point for the present reaction
      E1 =RWO(K1)
C* Low energy limit on file is determined from any reaction with Q>0
      IF(QI.GE.0) ELE=E1
C*
C* Correct the total, elastic, fission, capture
      IF(MT.EQ.1 .OR. MT.EQ.2 .OR. MT.EQ.18 .OR. MT.EQ.102) THEN
C* Check the first energy on the file
        IF(E1.GT.ERH) THEN

          PRINT *,'WARNING - Erh lower than first data point of MT',MT

        END IF
C* Skip points that overlap with the resonance range
  134   IF(RWO(K1+1).LE.ERH) THEN
          K1=K1+1
          K2=K2+1
          NP=NP-1
          GO TO 134
        END IF
C* Interpolate cross section to the upper resonance range energy
        RWO(K2)=RWO(K2)+(RWO(K2+1)-RWO(K2))
     &                 *(ERH-RWO(K1))/(RWO(K1+1)-RWO(K1))
        RWO(K1)=ERH
        E1=ERH
      END IF
C* Check threshold energy
      ETH=-QI*(AWR+1)/AWR
      IF(ETH.LT.ELE) THEN
C* Add points below threshold
        EAD=MAX(ETH,ERL)
        NMT=NMT+1
        MTLS(NMT)=MT
        ELOW(NMT)=EAD
        K1=K1-2
        K2=K2-2
        NP=NP+2
        ND=NP-NBT(NR)
        DO J=1,NR
          NBT(J)=NBT(J)+ND
        END DO
        RWO(K1  )=EAD
        RWO(K1+1)=E1
        RWO(K2  )=0
        RWO(K2+1)=0
      END IF
      CALL WRTAB1(LOU,MAT,MF,MT,NS,QM,QI,LL,LR
     1                 ,NR,NP,NBT,INR,RWO(K1),RWO(K2))
C* Write the SEND record and check the next MT
      CALL RDTEXT(LEM,MA1,MF,MT,CH66,IER)
      CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
      CALL RDTEXT(LEM,MA1,MF,MT,CH66,IER)
      IF(MF.NE.0) GO TO 132
C* All pointwise cross sections processed - write the FEND record
      CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
C*
C-F  Check threshold in files MF=4
  140 CALL RDTEXT(LEM,MA1,MF,MT,CH66,IER)
      IF(IER.EQ.1 .OR. MA1.LE.0) THEN
        MAT=-1
        GO TO 274
      END IF
      IF(MF.EQ.0) THEN
        CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
        GO TO 140
      END IF
      IF(MF.GT.4) GO TO 150
      DO I=1,NMT
        IF(MTLS(I).EQ.MT) THEN

          PRINT *,'Change MF/MT',MF,MT

C* Copy the header record
          READ (CH66,891) C1,C2,LVT,LTT,N1,N2
          CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
C* Copy the transformation matrix, if given
          IF(LVT.EQ.1) THEN
            CALL RDLIST(LEM,C1,C2,LI,LCT,NK,NM,RWO,MXRW,IER)
            CALL WRLIST(LOU,MAT,MF,MT,NS,C1,C2,LI,LCT,NK,NM,RWO)
          ELSE
            CALL RDHEAD(LEM,MA1,MF,MT,C1,C2,LI,LCT,N1,N2,IER)
            CALL WRCONT(LOU,MAT,MF,MT,NS,C1,C2,LI,LCT,N1,N2)
          END IF
C* Modify the TAB2 record for one extra energy point
          CALL RDTAB2(LEM,C1,C2,L1,L2,NR,NE,NBT,INR,IER)
          NE =NE+1
          DO J=1,NR
            NBT(J)=NBT(J)+1
          END DO
          CALL WRTAB2(LOU,MAT,MF,MT,NS,C1,C2,L1,L2
     1                   ,NR,NE,NBT,INR)
C* Duplicate the first distribution to ERL and copy the rest
          IF(LTT.EQ.1) THEN
            CALL RDLIST(LEM,C1,EE,LT,L2,N1,NL,RWO,MXRW,IER)
            CALL WRLIST(LOU,MAT,MF,MT,NS,C1,ERL,LT,L2,N1,NL,RWO)
            CALL WRLIST(LOU,MAT,MF,MT,NS,C1,EE ,LT,L2,N1,NL,RWO)
            DO J=3,NE
              CALL RDLIST(LEM,C1,EE,LT,L2,N1,NL,RWO,MXRW,IER)
              CALL WRLIST(LOU,MAT,MF,MT,NS,C1,EE ,LT,L2,N1,NL,RWO)
            END DO
          ELSE IF(LTT.EQ.2) THEN
            NK=MXRW/2
            K1=1
            K2=K1+NK
            CALL RDTAB1(LEM,C1,EE,LT,L2,NR,NP
     &                 ,NBT,INR,RWO(K1),RWO(K2),NK,IER)
            CALL WRTAB1(LOU,MAT,MF,MT,NS,C1,ERL,LT,L2,NR,NP
     &                 ,NBT,INR,RWO(K1),RWO(K2))
            CALL WRTAB1(LOU,MAT,MF,MT,NS,C1,EE,LT,L2,NR,NP
     &                 ,NBT,INR,RWO(K1),RWO(K2))
            DO J=3,NE
              CALL RDTAB1(LEM,C1,EE,LT,L2,NR,NP
     &                   ,NBT,INR,RWO(K1),RWO(K2),NK,IER)
              CALL WRTAB1(LOU,MAT,MF,MT,NS,C1,EE,LT,L2,NR,NP
     &                   ,NBT,INR,RWO(K1),RWO(K2))
            END DO
          ELSE
            PRINT *,'WARNING - No coding for MF/MT/LTT',MF,MT,LTT
          END IF
          GO TO 142
        END IF
      END DO
C* Copy the rest of the section unchanged
      CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
  142 CALL RDTEXT(LEM,MA1,MF,MT,CH66,IER)
      CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
      IF(MT.GT.0) GO TO 142
      GO TO 140
C*
C-F  Check threshold in files MF=5
  150 IF(MF.GT.5) GO TO 160
      DO I=1,NMT
        IF(MTLS(I).EQ.MT) THEN

          PRINT *,'Change MF/MT',MF,MT
          PRINT *,'WARNING - No coding for MF/MT',MF,MT

        CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
        GO TO 152
        END IF
      END DO
C* Copy the rest of the section unchanged
      CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
  152 CALL RDTEXT(LEM,MA1,MF,MT,CH66,IER)
      CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
      IF(MT.GT.0) GO TO 152
      GO TO 140
C*
C-F  Check threshold in files MF=6
  160 IF(MF.GT.6) GO TO 170
      DO I=1,NMT
        IF(MTLS(I).EQ.MT) THEN

          PRINT *,'Change MF/MT',MF,MT

C* Copy the header record
          READ (CH66,891) C1,C2,L1,LCT,NK,N2
          CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
C* Loop over all particles
          DO K=1,NK
C* Duplicate the first multiplicity to ERL and copy the rest
            LK =MXRW/2
            LK1=LK-1
            K1 =1
            K11=K1+1
            K2 =K1+LK
            K21=K2+1
            CALL RDTAB1(LEM,C1,C2,LIP,LAW,NR,NP
     &                 ,NBT,INR,RWO(K11),RWO(K21),LK1,IER)
            NP=NP+1
            RWO(K1)=ERL
            RWO(K2)=RWO(K2+1)
            CALL WRTAB1(LOU,MAT,MF,MT,NS,C1,C2,LIP,LAW,NR,NP
     &                 ,NBT,INR,RWO(K1),RWO(K2))
            IF     (LAW.EQ.0) THEN
C* No distributions given for LAW=0 - copy the rest of the file
              GO TO 162
            ELSE IF(LAW.EQ.1) THEN
C* Continuum energy-angle distributions (LAW=1)
              CALL RDTAB2(LEM,C1,C2,LANG,LEP,NR,NE,NBT,INR,IER)
              NE =NE+1
              DO J=1,NR
                NBT(J)=NBT(J)+1
              END DO
              CALL WRTAB2(LOU,MAT,MF,MT,NS,C1,C2,LANG,LEP
     1                   ,NR,NE,NBT,INR)
C* Duplicate the first distribution to ERL and copy the rest
              CALL RDLIST(LEM,C1,EE,LD,NA,NW,NEP,RWO,MXRW,IER)
              CALL WRLIST(LOU,MAT,MF,MT,NS,C1,ERL,LD,NA,NW,NEP,RWO)
              CALL WRLIST(LOU,MAT,MF,MT,NS,C1,EE ,LD,NA,NW,NEP,RWO)
              DO J=3,NE
                CALL RDLIST(LEM,C1,EE,LD,NA,NW,NEP,RWO,MXRW,IER)
                CALL WRLIST(LOU,MAT,MF,MT,NS,C1,EE ,LD,NA,NW,NEP,RWO)
              END DO
            ELSE
C* Unsupported representation encountered - copy as is
              PRINT *,'WARNING - No coding for MF/MT/LAW',MF,MT,LAW
              GO TO 162
            END IF
          END DO
          GO TO 162
        END IF
      END DO
C* Copy the rest of the section unchanged
      CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
  162 CALL RDTEXT(LEM,MA1,MF,MT,CH66,IER)
      CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
      IF(MT.GT.0) GO TO 152
      GO TO 140
C*
C-F  Check threshold in files MF>6
  170 IF(MF.GE.30) GO TO 180
      DO I=1,NMT
        IF(MTLS(I).EQ.MT) THEN

          PRINT *,'Change MF/MT',MF,MT
          PRINT *,'WARNING - No coding for MF/MT',MF,MT

        CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
        GO TO 172
        END IF
      END DO
C* Copy the rest of the section unchanged
      CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
  172 CALL RDTEXT(LEM,MA1,MF,MT,CH66,IER)
      CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
      IF(MT.GT.0) GO TO 172
      GO TO 140
C* Last explicitly treated file/section processed
  180 CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
C*
C-F  Copy the rest of the file
  270 CALL RDTEXT(LEM,MA1,MF,MT,CH66,IER)
      IF(IER.EQ.1 .OR. MA1.LE.0) THEN
        CH66=BL66
        MAT=MA1
      END IF
  274 CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
      IF(MAT.GT.0) GO TO 270
      IF(MAT.EQ.0) GO TO 100
C-F  Processing completed
      STOP 'ENDRES Completed'
C* Error trap
  284 STOP 'ENDRES ERROR - Processing terminated'
C*
  691 FORMAT(2A40)
  692 FORMAT(A40,I6)
  696 FORMAT(A66)
  698 FORMAT(BN,I10)
  891 FORMAT(2F11.0,4I11)
  892 FORMAT(I11)
      END
