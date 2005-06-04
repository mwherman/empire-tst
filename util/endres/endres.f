      PROGRAM ENDRES
C-Title  : Program ENDRES
C-Purpose: Insert resonance data into an ENDF file
C-Author : A. Trkov, IAEA-NDS, Vienna, Austria
C-Version: November 2003
C-V  04/11 Fix bug in correcting threshold x-sect in MF 3.
C-V  05/02 Improve threshold treatment implementing:
C-V        - threshold energy correction,
C-V        - addition of points below threshold where necessary.
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
C-External routines from DXSEND.F
C-E  RDTEXT, WRTEXT, FINDMT, WRCONT, RDHEAD, RDTAB1, RDTAB2, RDLIST
C-E  WRTAB1, WRTAB2, WRLIST, CHENDF
C-
      PARAMETER    (MXRW=10000, MXNB=20, MXMT=100)
C*
      CHARACTER*66  BL66,CH66,HD66
      CHARACTER*40  BLNK,FLNM,FLEM,FLRR,FLOU,FLLG
      CHARACTER*11  CHZA,CHAW

      REAL*8 Q
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
      SMALL=1.E-4
      NMT=0
      ELE=1.E-5
      ERL=ELE
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
      WRITE(LLG,691) HD66
      WRITE(LLG,691)
      WRITE(LLG,691) ' Source resonance parameter file      : ',FLRR
      WRITE(LLG,691) '                          File header : '
      WRITE(LLG,691) CH66
      WRITE(LLG,691)
      WRITE(LLG,691) ' Output ENDF file                     : ',FLOU
      WRITE(LLG,691) '                          File header : '
      WRITE(LLG,691) HD66
      WRITE(LLG,691)
C* Write the header to output and log file
      IF(M1.EQ.0) M1=7777
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
      READ (CH66,891) ZAM,AWR,LRP,LFI,NLIB,NMOD
      ZA0=ZAM
      IZA=NINT(ZA0)
      CHZA=CH66( 1:11)
      CHAW=CH66(12:22)
C*
C-F  Find equivalent nuclide (matching ZA) on the resonance file
      MFR=0
      MTR=0
      ERH=0
      CALL FINDMT(LRR,ZA0,ZA,AW,L1R,L2R,N1R,N2R,MAR,MFR,MTR,IER)
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
      LRP=1
      CALL WRCONT(LOU,MAT,MF,MT,NS,ZAM,AWR,LRP,LFI,NLIB,NMOD)
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

        WRITE(LTT,*) ' WARNING - Unmatched AWR',AWR,AW
        WRITE(LLG,*) ' WARNING - Unmatched AWR',AWR,AW

      END IF
C*
C-F  Process the resonance data and define the energy range
      IF(MFR.EQ.2 .AND. MTR.EQ.151) REWIND LRR
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
      READ (CH66,891) EL,EH,LRU,LRF,NRO,NAPS
      IF(LRU.EQ.0) THEN
C* Case: no resonance parameters
        GO TO 128
      ELSE IF(LRU.EQ.1) THEN
C* Case: Resolved resonance range present -  copy to output
        ERL=EL
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
C*
          WRITE(LTT,692) ' ENDRES ERROR - No coding fo NER= 2/LRF=',LRF  
          WRITE(LLG,692) ' ENDRES ERROR - No coding fo NER= 2/LRF=',LRF  
C*
          STOP 'ENDRES ERROR - No coding fo NER=2/LRF'
C*
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

        WRITE(LTT,*) 'WARNING - No pointwise data on source file'
        WRITE(LLG,*) 'WARNING - No pointwise data on source file'

      END IF
      IF(MA1.NE.MAT .OR. MF.NE.3) GO TO 130
      IF(MT.NE.1) THEN

        WRITE(LTT,*) 'WARNING - First MT not total'
        WRITE(LLG,*) 'WARNING - First MT not total'

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
C*
C* Correct first energy for the total, elastic, fission, capture
      IF(MT.EQ.1 .OR. MT.EQ.2 .OR. MT.EQ.18 .OR. MT.EQ.102) THEN
C* Check the first energy on the file
        IF(E1.GT.ERH) THEN

          WRITE(LTT,*)
     &    ' WARNING - ERH lower than first data point of MT',MT
          WRITE(LLG,*)
     &    ' WARNING - ERH lower than first data point of MT',MT

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
C* Define threshold energy
      ETH=-QI*(AWR+1)/AWR
C* Low energy limit on file is used for reactions with Q>0
      ELL=MAX(ETH,E1)
      IF(ETH.LT.ELL .AND. ELL.GT.ELE) THEN
C* Check points below threshold
c...
c...    print *,'mt,k1,ele,eth,ell,erl',mt,k1,ele,eth,ell,erl
c...
        EAD=MAX(ETH,ERL)
        NMT=NMT+1
        MTLS(NMT)=MT
        ELOW(NMT)=EAD
        IF(ELL-EAD.LT.SMALL*ELL) THEN
C* Adjust the threshold energy
          RWO(K1)=EAD
        ELSE
C* Add points below the first point
          K1=K1-2
          K2=K2-2
          NP=NP+2
          ND=NP-NBT(NR)
          DO J=1,NR
            NBT(J)=NBT(J)+ND
          END DO
          RWO(K1  )=EAD
          RWO(K1+1)=ELL
          RWO(K2  )=0
          RWO(K2+1)=0
        END IF
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
c...
c...  print *,(MTLS(J),J=1,NMT)
c...
C*
C-F  Check threshold in files MF>=4
  140 CALL RDTEXT(LEM,MA1,MF,MT,CH66,IER)
      IF(IER.EQ.1 .OR. MA1.LT.0) THEN
        MAT=-1
        CH66=BL66
        GO TO 274
      END IF
      IF(MF.EQ.0) THEN
        IF(MA1.EQ.0) THEN
          MAT=0
          CH66=BL66
        END IF
        CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
        GO TO 140
      END IF
      IF(MF.GT.4) GO TO 150
      DO I=1,NMT
        IF(MTLS(I).EQ.MT) THEN
          EAD=ELOW(I)

          WRITE(LTT,*) ' Change MF/MT',MF,MT,' lower energy to',EAD
          WRITE(LLG,*) ' Change MF/MT',MF,MT,' lower energy to',EAD

C* Copy the header record
          READ (CH66,891) C1,C2,LVT,LTTE,N1,N2
          CALL WRCONT(LOU,MAT,MF,MT,NS,C1,C2,LVT,LTTE,N1,N2)
C* Copy the transformation matrix, if given
          IF(LVT.EQ.1) THEN
            CALL RDLIST(LEM,C1,C2,LI,LCT,NK,NM,RWO,MXRW,IER)
            CALL WRLIST(LOU,MAT,MF,MT,NS,C1,C2,LI,LCT,NK,NM,RWO)
          ELSE
            CALL RDHEAD(LEM,MA1,MF,MT,C1,C2,LI,LCT,N1,N2,IER)
            CALL WRCONT(LOU,MAT,MF,MT,NS,C1,C2,LI,LCT,N1,N2)
          END IF
          IF(LTTE.NE.0) THEN
C* Modify the TAB2 record for one extra energy point
            CALL RDTAB2(LEM,C1,C2,L1,L2,NR,NE,NBT,INR,IER)
          END IF
          IF(LTTE.EQ.0) THEN
            GO TO 142
C* Duplicate the first distribution to EAD and copy the rest
          ELSE IF(LTTE.EQ.1) THEN
            CALL RDLIST(LEM,A1,EE,LT,L2,N1,NL,RWO,MXRW,IER)
            IF(ABS(EE-EAD).LT.SMALL*EE) THEN
              EE=EAD
              WRITE(LTT,*) '      Adjust threshold'
              WRITE(LLG,*) '      Adjust threshold'
              CALL WRTAB2(LOU,MAT,MF,MT,NS,C1,C2,L1,L2
     1                   ,NR,NE,NBT,INR)
            ELSE
              NE1=NE+1
              DO J=1,NR
                NBT(J)=NBT(J)+1
              END DO
              CALL WRTAB2(LOU,MAT,MF,MT,NS,C1,C2,L1,L2
     1                   ,NR,NE1,NBT,INR)
              CALL WRLIST(LOU,MAT,MF,MT,NS,A1,EAD,LT,M2,N1,NL,RWO)
            END IF
            CALL WRLIST(LOU,MAT,MF,MT,NS,A1,EE ,LT,M2,N1,NL,RWO)
            DO J=2,NE
              CALL RDLIST(LEM,C1,EE,LT,L2,N1,NL,RWO,MXRW,IER)
              CALL WRLIST(LOU,MAT,MF,MT,NS,C1,EE ,LT,L2,N1,NL,RWO)
            END DO
          ELSE IF(LTTE.EQ.2) THEN
            NK=MXRW/2
            K1=1
            K2=K1+NK
            K3=NR+1
            CALL RDTAB1(LEM,A1,EE,LT,M2,NRR,NP
     &                 ,NBT(K3),INR(K3),RWO(K1),RWO(K2),NK,IER)
            IF(ABS(EE-EAD).LT.SMALL*EE) THEN
              EE=EAD
              WRITE(LTT,*) '      Adjust threshold'
              WRITE(LLG,*) '      Adjust threshold'
              CALL WRTAB2(LOU,MAT,MF,MT,NS,C1,C2,L1,L2
     &                   ,NR,NE,NBT,INR)
            ELSE
              NE1=NE+1
              DO J=1,NR
                NBT(J)=NBT(J)+1
              END DO
              CALL WRTAB2(LOU,MAT,MF,MT,NS,C1,C2,L1,L2
     &                   ,NR,NE1,NBT,INR)
              CALL WRTAB1(LOU,MAT,MF,MT,NS,A1,EAD,LT,M2,NRR,NP
     &                   ,NBT(K3),INR(K3),RWO(K1),RWO(K2))
            END IF
            CALL WRTAB1(LOU,MAT,MF,MT,NS,A1,EE,LT,M2,NRR,NP
     &                 ,NBT(K3),INR(K3),RWO(K1),RWO(K2))
            DO J=2,NE
              CALL RDTAB1(LEM,C1,EE,LT,L2,NR,NP
     &                   ,NBT,INR,RWO(K1),RWO(K2),NK,IER)
              CALL WRTAB1(LOU,MAT,MF,MT,NS,C1,EE,LT,L2,NR,NP
     &                   ,NBT,INR,RWO(K1),RWO(K2))
            END DO
          ELSE
            WRITE(LTT,*) ' WARNING - No coding for MF/MT/LTT',MF,MT,LTTE
            WRITE(LLG,*) ' WARNING - No coding for MF/MT/LTT',MF,MT,LTTE
            CALL WRTAB2(LOU,MAT,MF,MT,NS,C1,C2,L1,L2
     1                 ,NR,NE,NBT,INR)
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
          EAD=ELOW(I)

          WRITE(LTT,*) ' Change MF/MT',MF,MT,' lower energy to',EAD
          WRITE(LLG,*) ' Change MF/MT',MF,MT,' lower energy to',EAD

          WRITE(LTT,*) ' WARNING - No coding for MF/MT',MF,MT
          WRITE(LLG,*) ' WARNING - No coding for MF/MT',MF,MT

        READ (CH66,891) C1,C2,L1,L2,N1,N2
        CALL WRCONT(LOU,MAT,MF,MT,NS,C1,C2,L1,L2,N1,N2)
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
          EAD=ELOW(I)

          WRITE(LTT,*) 'Change MF/MT',MF,MT,' lower energy to',EAD
          WRITE(LLG,*) 'Change MF/MT',MF,MT,' lower energy to',EAD

C* Copy the header record
          READ (CH66,891) C1,C2,L1,LCT,NK,N2
          CALL WRCONT(LOU,MAT,MF,MT,NS,C1,C2,L1,LCT,NK,N2)
C* Loop over all particles
          DO K=1,NK
            LK =MXRW/2
            LK1=LK-1
            K1 =1
            K11=K1+1
            K2 =K1+LK
            K21=K2+1
            CALL RDTAB1(LEM,C1,C2,LIP,LAW,NR,NP
     &                 ,NBT,INR,RWO(K11),RWO(K21),LK1,IER)
            EE=RWO(K11)
            IF(ABS(EE-EAD).LT.SMALL*EE) THEN
              EE=EAD
              RWO(K11)=EE
              WRITE(LTT,*) '      Adjust threshold TAB1'
              WRITE(LLG,*) '      Adjust threshold TAB1'
            ELSE
C* Duplicate the first multiplicity to EAD and copy the rest
              NP=NP+1
              RWO(K1)=EAD
              RWO(K2)=RWO(K21)
              DO J=1,NR
                NBT(J)=NBT(J)+1
              END DO
              K11=K1
              K21=K2
              WRITE(LTT,*) '      Add TAB1 data point at eV',EAD
              WRITE(LLG,*) '      Add TAB1 data point at eV',EAD
            END IF
            CALL WRTAB1(LOU,MAT,MF,MT,NS,C1,C2,LIP,LAW,NR,NP
     &                 ,NBT,INR,RWO(K11),RWO(K21))
            IF     (LAW.EQ.0) THEN
C* No distributions given for LAW=0 - copy the rest of the file
              GO TO 162
            ELSE IF(LAW.EQ.1) THEN
C* Continuum energy-angle distributions (LAW=1)
              CALL RDTAB2(LEM,C1,C2,LANG,LEP,NR,NE,NBT,INR,IER)
C* Duplicate the first distribution to EAD and copy the rest
              CALL RDLIST(LEM,A1,EE,LD,NA,NW,NEP,RWO,MXRW,IER)
              IF(ABS(EE-EAD).LT.SMALL*EE) THEN
                EE=EAD
                WRITE(LTT,*) '      Adjust threshold TAB2'
                WRITE(LLG,*) '      Adjust threshold TAB2'
                CALL WRTAB2(LOU,MAT,MF,MT,NS,C1,C2,LANG,LEP
     &                     ,NR, NE,NBT,INR)
              ELSE
                WRITE(LTT,*) '      Add TAB2 data point at eV',EAD
                WRITE(LLG,*) '      Add TAB2 data point at eV',EAD
                NE1=NE+1
                DO J=1,NR
                  NBT(J)=NBT(J)+1
                END DO
                CALL WRTAB2(LOU,MAT,MF,MT,NS,C1,C2,LANG,LEP
     &                     ,NR,NE1,NBT,INR)
                CALL WRLIST(LOU,MAT,MF,MT,NS,A1,EAD,LD,NA,NW,NEP,RWO)
              END IF
              CALL WRLIST(LOU,MAT,MF,MT,NS,A1, EE,LD,NA,NW,NEP,RWO)
              DO J=2,NE
                CALL RDLIST(LEM,C1,EE,LD,NA,NW,NEP,RWO,MXRW,IER)
                CALL WRLIST(LOU,MAT,MF,MT,NS,C1,EE ,LD,NA,NW,NEP,RWO)
              END DO
            ELSE IF(LAW.EQ.2) THEN
C* Discrete two-body scattering (LAW=2)
              CALL RDTAB2(LEM,C1,C2,L1,L2,NR,NE,NBT,INR,IER)
C* Duplicate the first distribution to EAD and copy the rest
              CALL RDLIST(LEM,A1,EE,LANG,L2,NW,NL,RWO,MXRW,IER)
              IF(ABS(EE-EAD).LT.SMALL*EE) THEN
                EE=EAD
                WRITE(LTT,*) '      Adjust threshold TAB2'
                WRITE(LLG,*) '      Adjust threshold TAB2'
                CALL WRTAB2(LOU,MAT,MF,MT,NS,C1,C2,L1,L2
     &                     ,NR, NE,NBT,INR)
              ELSE
                WRITE(LTT,*) '      Add TAB2 data point at eV',EAD
                WRITE(LLG,*) '      Add TAB2 data point at eV',EAD
                NE1=NE+1
                DO J=1,NR
                  NBT(J)=NBT(J)+1
                END DO
                CALL WRTAB2(LOU,MAT,MF,MT,NS,C1,C2,L1,L2
     &                     ,NR,NE1,NBT,INR)
                CALL WRLIST(LOU,MAT,MF,MT,NS,A1,EAD,LANG,L2,NW,NL,RWO)
              END IF
              CALL WRLIST(LOU,MAT,MF,MT,NS,A1, EE,LANG,L2,NW,NL,RWO)
              DO J=2,NE
                CALL RDLIST(LEM,C1,EE,LANG,L2,NW,NL,RWO,MXRW,IER)
                CALL WRLIST(LOU,MAT,MF,MT,NS,C1,EE ,LANG,L2,NW,NL,RWO)
              END DO
            ELSE IF(LAW.EQ.3 .OR. LAW.EQ.4) THEN
C* No LAW-dependent structure - copy as is
              GO TO 162
            ELSE
C* Unsupported representation encountered - copy as is
              WRITE(LTT,*)' WARNING - Copy as is MF/MT/LAW',MF,MT,LAW
              WRITE(LLG,*)' WARNING - Copy as is MF/MT/LAW',MF,MT,LAW
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
C-F  Check threshold in files MF9,10
  170 IF(MF.LT.9 .OR. MF.GT.10) GO TO 180
      DO I=1,NMT
        IF(MTLS(I).EQ.MT) THEN
          EAD=ELOW(I)
          READ (CH66,891) C1,C2,L1,L2,NS,N2
          CALL WRCONT(LOU,MAT,MF,MT,NS,C1,C2,L1,L2,NS,N2)
          WRITE(LTT,*) ' Change MF/MT',MF,MT,' lower energy to',EAD
          WRITE(LLG,*) ' Change MF/MT',MF,MT,' lower energy to',EAD

          WRITE(LTT,*) ' WARNING - No coding for MF/MT',MF,MT
          WRITE(LLG,*) ' WARNING - No coding for MF/MT',MF,MT

          DO J=1,NS
            NMX=MXRW/2
            L1 =1
            L2 =1+NMX
            K1 =L1+1
            K2 =L2+1
            NM2=NMX-2
            CALL RDTAB1(LEM,QM,QI,LL,LR,NR,NP,NBT,INR
     &                 ,RWO(K1),RWO(K2),NMX,IER)
            CALL WRTAB1(LOU,MAT,MF,MT,NS,QM,QI,LL,LR
     &                 ,NR,NP,NBT,INR,RWO(K1),RWO(K2))
          END DO
        END IF
      END DO
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
      SUBROUTINE WRTEXT(LIB,MAT,MF,MT,NS,REC)
C-Title  : WRTEXT Subroutine
C-Purpose: Write a text record to an ENDF file
      CHARACTER*66  REC
      NS=NS+1
      IF(NS.GT.99999) NS=0
      WRITE(LIB,40) REC,MAT,MF,MT,NS
      RETURN
   40 FORMAT(A66,I4,I2,I3,I5)
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
      IF(NS.GT.99999) NS=0
      WRITE(LIB,40) (REC(J),J=1,6),MAT,MF,MT,NS
      RETURN
   20 FORMAT(I11)
   40 FORMAT(6A11,I4,I2,I3,I5)
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
C-D    IER=9 on exit if available field length NMX is exceeded.
C-
      DIMENSION    NBT(100),INR(100)
      DIMENSION    EN(NMX), XS(NMX)
C*
      READ (LEF,902) C1,C2,L1,L2,N1,N2
      READ (LEF,903) (NBT(J),INR(J),J=1,N1)
      JP=N2
      IF(N2.GT.NMX) THEN
        JP=NMX
        IER=9
      END IF
      READ (LEF,904) (EN(J),XS(J),J=1,JP)
      RETURN
C*
  902 FORMAT(2F11.0,4I11)
  903 FORMAT(6I11)
  904 FORMAT(6F11.0)
      END
      SUBROUTINE RDTAB2(LEF,C1,C2,L1,L2,N1,N2,NBT,INR,IER)
C-Title  : Subroutine RDTAB2
C-Purpose: Read an ENDF TAB2 record
      DIMENSION    NBT(100),INR(100)
C*
      READ (LEF,902) C1,C2,L1,L2,N1,N2
      READ (LEF,903) (NBT(J),INR(J),J=1,N1)
      RETURN
C*
  902 FORMAT(2F11.0,4I11)
  903 FORMAT(6I11)
      END
      SUBROUTINE RDLIST(LEF,C1,C2,L1,L2,N1,N2,VK,MVK,IER)
C-Title  : Subroutine RDLIST
C-Purpose: Read an ENDF LIST record
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
      SUBROUTINE WRTAB1(LIB,MAT,MF,MT,NS,C1,C2,L1,L2
     1                 ,NR,NP,NBT,INR,X,Y)
C-Title  : WRTAB1 Subroutine
C-Purpose: Write a TAB1 record to an ENDF file
      CHARACTER*11  BLN,REC(6)
      DIMENSION     NBT(1),INR(1),X(1),Y(1)
      DATA BLN/'           '/
C* First line of the TAB1 record
      CALL CHENDF(C1,REC(1))
      CALL CHENDF(C2,REC(2))
      WRITE(REC(3),42) L1
      WRITE(REC(4),42) L2
      WRITE(REC(5),42) NR
      WRITE(REC(6),42) NP
      NS=NS+1
      IF(NS.GT.99999) NS=0
      WRITE(LIB,40) (REC(J),J=1,6),MAT,MF,MT,NS
C* Write interpolation data
      N =0
   20 I =0
   22 REC(I+1)=BLN
      REC(I+2)=BLN
      IF(N.GE.NR) GO TO 24
      N =N+1
      WRITE(REC(I+1),42) NBT(N)
      WRITE(REC(I+2),42) INR(N)
   24 I =I +2
      IF(I.LT.6) GO TO 22
      NS=NS+1
      IF(NS.GT.99999) NS=0
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
      IF(NS.GT.99999) NS=0
      WRITE(LIB,40) (REC(J),J=1,6),MAT,MF,MT,NS
      IF(N.LT.NP) GO TO 30
      RETURN
   40 FORMAT(6A11,I4,I2,I3,I5)
   42 FORMAT(I11)
      END
      SUBROUTINE WRTAB2(LIB,MAT,MF,MT,NS,C1,C2,L1,L2
     1                 ,NR,NZ,NBT,INR)
C-Title  : WRTAB2 Subroutine
C-Purpose: Write a TAB2 record to an ENDF file
      CHARACTER*11  BLN,REC(6)
      DIMENSION     NBT(1),INR(1)
      DATA BLN/'           '/
C* First line of the TAB2 record
      CALL CHENDF(C1,REC(1))
      CALL CHENDF(C2,REC(2))
      WRITE(REC(3),42) L1
      WRITE(REC(4),42) L2
      WRITE(REC(5),42) NR
      WRITE(REC(6),42) NZ
      NS=NS+1
      IF(NS.GT.99999) NS=0
      WRITE(LIB,40) (REC(J),J=1,6),MAT,MF,MT,NS
C* Write interpolation data
      N =0
   20 I =0
   22 REC(I+1)=BLN
      REC(I+2)=BLN
      IF(N.GE.NR) GO TO 24
      N =N+1
      WRITE(REC(I+1),42) NBT(N)
      WRITE(REC(I+2),42) INR(N)
   24 I =I +2
      IF(I.LT.6) GO TO 22
      NS=NS+1
      IF(NS.GT.99999) NS=0
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
      IF(NS.GT.99999) NS=0
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
      IF(NS.GT.99999) NS=0
      WRITE(LIB,40) (REC(J),J=1,6),MAT,MF,MT,NS
      IF(N.LT.NPL) GO TO 20
      RETURN
   40 FORMAT(6A11,I4,I2,I3,I5)
   42 FORMAT(I11)
      END
      SUBROUTINE CHENDF(FF,CH)
C-Title  : CHENDF Subroutine
C-Purpose: Pack value into 11-character string
C-Version:
C-V  05/02 Double precision internal arithmetic to avoid roundoff error
      REAL*8       FA
      CHARACTER*1  SN
      CHARACTER*11 CH
      CH=' 0.000000+0'
      FA=ABS(FF)
      IA=0
   20 IF(FA.LT.   1.0D-30) RETURN
      IF(FA.LT.9.999950D0) GO TO 40
      FA=FA/10
      IA=IA+1
      GO TO 20
   40 IF(FA.GE.0.999995D0) GO TO 50
      FA=FA*10
      IA=IA-1
      GO TO 40
   50 SN='+'
      IF(IA.LT.0) THEN
        SN='-'
        IA=-IA
      END IF
      IF(FF.LT.0) FA=-FA
      IF(IA.LE.9) THEN
        WRITE(CH,81) FA,SN,IA
      ELSE IF(IA.GT.9 .AND. IA.LE.99) THEN
        WRITE(CH,82) FA,SN,IA
      ELSE
        WRITE(CH,83) FA,SN,IA
      END IF
      RETURN
   81 FORMAT(F9.6,A1,I1)
   82 FORMAT(F8.5,A1,I2.2)
   83 FORMAT(F7.4,A1,I3.3)
      END
