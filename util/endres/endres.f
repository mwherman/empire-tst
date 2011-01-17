Ccc   * $Author: Capote $
Ccc   * $Date: 2009-11-01 01:01:20 $
Ccc   * $Id: auxiliary.f,v 1.42 2009-11-01 01:01:20 Capote Exp $

      PROGRAM ENDRES
C-Title  : Program ENDRES
C-Purpose: Insert resonance data into an ENDF file
C-Author : A. Trkov, IAEA-NDS, Vienna, Austria
C-Version: November 2003
C-V  04/11 Fix bug in correcting threshold x-sect in MF 3.
C-V  05/02 Improve threshold treatment implementing:
C-V        - threshold energy correction,
C-V        - addition of points below threshold where necessary.
C-V  05/05 Allow processing of MF2 with only spin and radius given.
C-V  05/10 Copy comments from the resonance file.
C-V  06/01 - Allow multiple resonance ranges,
C-V        - Copy resonance covariance data, if present.
C-V  07/06 - Fix processing of MF10 thresholds.
C-V        - Convert MF10/MT102 into MF9/MT102.
C-V  07/09 - Fix interpolation table for NR>1.
C-V        - Fix rare cases of E-threshold mismatch in TAB2 for LAW=2
C-V        - Fix copying of MF33 (no adjustment to energy boundaries)
C-V  08/04 - Fix conversion MF10/MT102 to MF9/MT102 when MF8 present.
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
      CHARACTER*66  BL66,CH66,CR66,HD66
      CHARACTER*40  BLNK,FLNM,FLEM,FLRR,FLOU,FLLG,FLSC,FLS1
      CHARACTER*11  CHZA,CHAW
C*
      DOUBLE PRECISION Q
C*
      DIMENSION     NBT(MXNB),INR(MXNB),MTLS(MXMT),ELOW(MXMT)
      DIMENSION     RWO(MXRW)
C*
      DATA BLNK/'                                        '/
     1     FLEM/'empire.end                              '/
     2     FLRR/'mughrr.end                              '/
     3     FLOU/'endf.dat'/
     4     FLLG/'endres.lst'/
     7     FLSC/'endres.scr'/
     8     FLS1/'endres1.scr'/
      DATA LEM,LRR,LOU,LLG,LKB,LTT,LSC,LS1
     &    /  1,  2,  3,  4,  5,  6,  7, 8 /
C*
      SMALL=1.E-4
      TINY =2.E-6
      NMT=0
      ELE=1.E-5
      ERL=ELE
      NF10=0
      NF09=0
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
C* Convert MF10/MT102 into MF9/MT102 on scratch file FLS1 unit LS1
      CALL MKEMF9(MF9102,LEM,LS1,FLS1,ELE,RWO,MXRW)
      REWIND LEM
      CALL RDTEXT(LEM,M1,M2,M3,HD66,IER)
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
C* Open the scratch file
      OPEN (UNIT=LSC,FILE=FLSC,STATUS='UNKNOWN')
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
      WRITE(LLG,691) BLNK
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
      WRITE(LLG,691) BLNK
C*
      WRITE(LTT,691) ' Source ENDF file to be edited        : ',FLEM
      WRITE(LTT,691) '                          File header : ',HD66
      WRITE(LTT,691) HD66
      WRITE(LTT,691)
      WRITE(LTT,691) ' Source resonance parameter file      : ',FLRR
      WRITE(LTT,691) '                          File header : '
      WRITE(LTT,691) CH66
      WRITE(LTT,691)
      WRITE(LTT,691) ' Output ENDF file                     : ',FLOU
      WRITE(LTT,691) '                          File header : '
      WRITE(LTT,691) HD66
      WRITE(LTT,691) BLNK
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
      READ (CH66,891) C1,C2,L1,L2,N1,NC
C* Copy the comments from the source ENDF file to scratch
      NSS=0
      DO I=1,N1
        CALL RDTEXT(LEM,MAT,MF,MT,CH66,IER)
        CALL WRTEXT(LSC,MAT,MF,MT,NSS,CH66)
      END DO
C* Add to comments as signature of ENDRES
      CH66=' *** ENDRESS - Merging source and resonance ENDF files'
     *     //BLNK
      CALL WRTEXT(LSC,MAT,MF,MT,NSS,CH66)
      CH66='             Source ENDF '//FLEM
      CALL WRTEXT(LSC,MAT,MF,MT,NSS,CH66)
      CH66='          Resonance ENDF '//FLRR
      CALL WRTEXT(LSC,MAT,MF,MT,NSS,CH66)
      NX=N1+3
c...
c...      print *,'nx,n1,mfr',nx,n1,mfr
c...
      IF(MFR.EQ.1) THEN
C* Copy comments from the resonance file (2 records read in FINDMT)
        CALL RDTEXT(LRR,MAR,MFR,MTR,CH66,IER)
        CALL RDTEXT(LRR,MAR,MFR,MTR,CH66,IER)
        READ (CH66,891) C1,C2,L1,L2,N1,N2
        CALL RDTEXT(LRR,MAR,MFR,MTR,CH66,IER)
        CALL RDTEXT(LRR,MAR,MFR,MTR,CH66,IER)
        DO I=3,N1
          CALL RDTEXT(LRR,MAR,MFR,MTR,CH66,IER)
          IF(CH66(4:4).NE.'-' .OR. I.GT.5) THEN
            CALL WRTEXT(LSC,MAT,MFR,MTR,NSS,CH66)
            NX=NX+1
          END IF
        END DO
      END IF
C* Copy comments from scratch to output ENDF file
      REWIND LSC
      CALL WRCONT(LOU,MAT,MF,MT,NS,C1,C2,L1,L2,NX,NC)
      DO I=1,NX
        CALL RDTEXT(LSC,MAT,MF,MT,CH66,IER)
        CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
      END DO
      CLOSE(UNIT=LSC)
C*
C* Copy the rest of MF1
  110 CALL RDTEXT(LEM,MAT,MF,MT,CH66,IER)
      IF(MT.EQ.0) NS=99998
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
      NS=0
  120 CALL RDTEXT(LRR,MA1,MF,MT,CH66,IER)
      IF(MA1.NE.MAR .OR. MF.NE.2 .OR. MT.NE.151) GO TO 120
      CH66( 1:11)=CHZA
      CH66(12:22)=CHAW
      CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
      READ (CH66,891) XZA,XAWR,IDMY,IDMY,NIS,IDMY
      CALL RDTEXT(LRR,MA1,MF,MT,CH66,IER)
      READ (CH66,891) XZA,XABN,IDMY,LFW,NER,IDMY
      CH66( 1:11)=CHZA
      CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
      JER=0
C* Process a resonance section
  122 JER=JER+1
      CALL RDTEXT(LRR,MA1,MF,MT,CH66,IER)
      CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
      READ (CH66,891) EL,EH,LRU,LRF,NRO,NAPS
      IF(LRU.EQ.0) THEN
C* Case: no resonance parameters
        ERL=ELE
        ERH=ELE
        GO TO 126
      ELSE IF(LRU.EQ.1) THEN
C* Case: Resolved resonance range present -  copy to output
        IF(JER.EQ.1) ERL=EL
        ERH=EH
        IF(NIS.GT.1) THEN
          WRITE(LTT,691) ' ENDRES WARNING - Multi-isotope file    '
          WRITE(LTT,691) '                  limited support       '
          WRITE(LLG,691) ' ENDRES WARNING - Multi-isotope file    '
          WRITE(LLG,691) '                  limited support       '
          GO TO 126
        END IF
C*       No special procedures needed for single range evaluations
        IF(NER.EQ.1) GO TO 126
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
          IF(JER.LT.NER) GO TO 122
        ELSE
C*
          WRITE(LTT,692) ' ENDRES ERROR - No coding fo NER >1/LRF=',LRF  
          WRITE(LLG,692) ' ENDRES ERROR - No coding fo NER >1/LRF=',LRF  
C*
          STOP 'ENDRES ERROR - No coding fo NER>1/LRF'
C*
        END IF
      ELSE IF(LRU.EQ.2) THEN
C* Case: Unresolved resonance range present - process LSSF
        CALL RDTEXT(LRR,MA1,MF,MT,CH66,IER)
        READ (CH66,891) DMY,DMY,LSSF1,L2,NLS,N2
        WRITE(CH66(23:33),892) LSSF
        CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
        IF(LSSF.EQ.0) ERH=EH
C*      --Copy parameters: all energy-independent, no fission widths
        IF(LRF.EQ.1) THEN
          IF(LFW.EQ.0) THEN
C*            Fission widths not given
            DO L=1,NLS
              CALL RDTEXT(LRR,MA1,MF,MT,CH66,IER)
              CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
              READ (CH66,891) DMY,DMY,L1,L2,N1,NJS
              DO J=1,NJS
                CALL RDTEXT(LRR,MA1,MF,MT,CH66,IER)
                CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
              END DO
            END DO
            IF(JER.LT.NER) GO TO 122
          ELSE
C*          --Fission widths given (energy-dependent)
            NE=NLS
            NLS=N2
            DO L=1,NLS
              N  =(NE+5)/6
              DO J=1,N
                CALL RDTEXT(LRR,MA1,MF,MT,CH66,IER)
                CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
              END DO
              CALL RDTEXT(LRR,MA1,MF,MT,CH66,IER)
              CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
              READ (CH66,891) DMY,DMY,L1,L2,NJS,N2
              N  =(NE+6+5)/6
              DO J=1,NJS
                DO K=1,N
                  CALL RDTEXT(LRR,MA1,MF,MT,CH66,IER)
                  CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
                END DO
              END DO
            END DO
            IF(JER.LT.NER) GO TO 122
          END IF
        ELSE
C*        --Copy parameters: all energy-dependent
          DO L=1,NLS
            CALL RDTEXT(LRR,MA1,MF,MT,CH66,IER)
            CH66(1:11)=CHAW
            CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
            READ (CH66,891) DMY,DMY,L1,L2,NJS,N2
            DO J=1,NJS
              CALL RDTEXT(LRR,MA1,MF,MT,CH66,IER)
              CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
              READ (CH66,891) DMY,DMY,L1,L2,N1,NE
              N  =NE+1
              DO K=1,N
                CALL RDTEXT(LRR,MA1,MF,MT,CH66,IER)
                CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
              END DO
            END DO
          END DO
          IF(JER.LT.NER) GO TO 122
        END IF
      ELSE
C* Case: Unknown LRU - copy as is to output
        WRITE(LTT,692) ' ENDRES WARNING - Unrecognised LRU =    ',LRU
        WRITE(LTT,691) '                  limited support       '
        GO TO 126
      END IF
C* Copy the rest of the resonance file to output
  126 CALL RDTEXT(LRR,MA1,MF,MT,CH66,IER)
      IF(MT.EQ.0) NS=99998
      CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
      NS=0
      IF(MF.NE.0) GO TO 126
C*
C-F  Check if the file contains resonance covariance data
      MF32=0
      DO WHILE (IER.EQ.0 .AND. MA1.GT.0)
        CALL RDTEXT(LRR,MA1,MFR,MTR,CR66,IER)
        IF(MFR.EQ.32) MF32=1
        IF(MFR.GE.32) EXIT
      END DO
      CR66( 1:11)=CHZA
      CR66(12:22)=CHAW
C*
C-F  Find the pointwise cross sections in the source file
  130 CALL RDTEXT(LEM,MA1,MF,MT,CH66,IER)
      IF(IER.GT.0 .OR. MA1.LT.0) THEN
        WRITE(LTT,*) 'WARNING - No pointwise data on source file'
        WRITE(LLG,*) 'WARNING - No pointwise data on source file'
        GO TO 180
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
      K1 =L1+3
      K2 =L2+3
      NM2=NMX-3
      CALL RDTAB1(LEM,QM,QI,LL,LR,NR,NP,NBT,INR
     &           ,RWO(K1),RWO(K2),NM2,IER)
C* First energy point for the present reaction
      E1 =RWO(K1)
C*
C* Correct first energy for the total, elastic, fission, capture
      IF(MT.EQ.1 .OR. MT.EQ.2 .OR. MT.EQ.18 .OR. MT.EQ.102) THEN
C* Check the first energy on the file
        IF(E1.GT.ERH) THEN
          WRITE(LTT,692) ' WARNING - ERH < first data point of MT ',MT
          WRITE(LTT,693) '                          at energy [eV]',E1
          WRITE(LTT,693) '              Flat extrapolation to [eV]',ERH
          WRITE(LLG,692) ' WARNING - ERH < first data point of MT ',MT
          WRITE(LLG,693) '                          at energy [eV]',E1
          WRITE(LLG,693) '              Flat extrapolation to [eV]',ERH
          K1=K1-1
          K2=K2-1
          NP=NP+1
          E1=ERH
          RWO(K1)=ERH
          RWO(K2)=RWO(K2+1)
          DO K=1,NR
            NBT(K)=NBT(K)+1
          END DO
        ELSE
C* Skip points that overlap with the resonance range
          DO WHILE (RWO(K1+1).LE.ERH .AND. NP.GT.1)
            K1=K1+1
            K2=K2+1
            NP=NP-1
          END DO
C* Interpolate cross section to the upper resonance range energy
          RWO(K2)=RWO(K2)+(RWO(K2+1)-RWO(K2))
     &                   *(ERH-RWO(K1))/(RWO(K1+1)-RWO(K1))
          RWO(K1)=ERH
          E1=ERH
        END IF
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
          DO WHILE(NBT(1).LT.2)
            NR=NR-1
            IF(NR.LT.1) STOP 'ENDRES ERROR - Inconsistent NBT'
            DO J=1,NR
              NBT(J)=NBT(J+1)
              INR(J)=INR(J+1)
            END DO
          END DO
          IF(INR(1).EQ.2) THEN
            IF(NR.GT.1) NBT(1)=NBT(1)+2
          ELSE
            DO J=1,NR
              NBT(NR+2-J)=NBT(NR+1-J)
              INR(NR+2-J)=INR(NR+1-J)
            END DO
            NR=NR+1
            NBT(1)=3
            INR(1)=2
          END IF
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
      IF(MT.EQ.0) NS=99998
      CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
      IF(MT.EQ.0) NS=0
      CALL RDTEXT(LEM,MA1,MF,MT,CH66,IER)
      IF(MF.NE.0) GO TO 132
C* All pointwise cross sections processed - write the FEND record
      NS=-1
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
        NS=-1
        CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
        GO TO 140
      END IF
      IF(MF.GT.4) GO TO 150
      DO I=1,NMT
        IF(MTLS(I).EQ.MT) THEN
          EAD=ELOW(I)
          WRITE(LTT,*) ' Check MF/MT',MF,MT
          WRITE(LLG,*) ' Check MF/MT',MF,MT
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
              IF(ABS(EE-EAD).GT.TINY*EE) THEN
                WRITE(LTT,*) '      Adjust threshold from',EE,' to',EAD
                WRITE(LLG,*) '      Adjust threshold from',EE,' to',EAD
              END IF
              EE=EAD
              CALL WRTAB2(LOU,MAT,MF,MT,NS,C1,C2,L1,L2
     1                   ,NR,NE,NBT,INR)
            ELSE
              WRITE(LTT,*) '      Add lower energy',EAD
              WRITE(LLG,*) '      Add lower energy',EAD
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
              IF(ABS(EE-EAD).GT.TINY*EE) THEN
                WRITE(LTT,*) '      Adjust threshold'
                WRITE(LLG,*) '      Adjust threshold'
              END IF
              EE=EAD
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
      IF(MT.EQ.0) NS=99998
      CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
      IF(MT.GT.0) GO TO 142
      NS=-1
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
      IF(MT.EQ.0) NS=99998
      CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
      IF(MT.GT.0) GO TO 152
      NS=0
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
              IF(ABS(EE-EAD).GT.TINY*EE) THEN
                WRITE(LTT,*) '      Adjust threshold TAB1'
                WRITE(LLG,*) '      Adjust threshold TAB1'
              END IF
              EE=EAD
              RWO(K11)=EE
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
                IF(ABS(EE-EAD).GT.TINY*EE) THEN
                  WRITE(LTT,*) '      Adjust threshold TAB2'
                  WRITE(LLG,*) '      Adjust threshold TAB2'
                END IF
                EE=EAD
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
                IF(ABS(EE-EAD).GT.TINY*EE) THEN
                  WRITE(LTT,*) '      Adjust threshold TAB2'
                  WRITE(LLG,*) '      Adjust threshold TAB2'
                END IF
                EE=EAD
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
  161 CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
  162 CALL RDTEXT(LEM,MA1,MF,MT,CH66,IER)
      IF(MT.EQ.0) NS=99998
      CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
      IF(MT.GT.0) GO TO 162
      NS=0
      GO TO 140
  170 IF(MF.LT. 8) GO TO 161
      IF(MF.GT.10) GO TO 180
C*
C-F  Check that MF8 points to MF9 for MT102
      IF(MF.EQ.8) THEN
        IF(MT.NE.102) GO TO 161
        READ (CH66,891) C1,C2,L1,L2,NFS,N2
        CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
        MF9=9
        DO I=1,NFS
          CALL RDHEAD(LEM,MA1,MF,MT,C1,C2,MFX,L2,N1,N2,IER)
          CALL WRCONT(LOU,MAT,MF,MT,NS,C1,C2,MF9,L2,N2,N2)
        END DO
        GO TO 162
      END IF
C*
C-F  Check threshold in files MF9,10
C...
C...       print *,'found mf/mt',mf,mt
C...
      IF(MF.EQ.9) NF09=NF09+1
      NS=0
      IF(MF.EQ.9 .AND. (MT.NE.102 .OR. MF9102.NE.1)) GO TO 178
      IF(MF9102.GT.0) THEN
        IF(MT.EQ.102) THEN
C* Skip MF9/MT102 if it exists
  171     CALL RDTEXT(LEM,MA1,MF1,MT1,HD66,IER)
          IF(MT1.NE.0) GO TO 171
        END IF
C* Insert MF9/MT102 calculated from MF10/MT102
              WRITE(LTT,*) 'Change MF/MT',MF,102
     &                    ,' to MF9'
              WRITE(LLG,*) 'Change MF/MT',MF,102
     &                    ,' to MF9'
        REWIND LS1
  172   CALL RDTEXT(LS1,MA1,MF1,MT1,HD66,IER)
        CALL WRTEXT(LOU,MAT,MF1,MT1,NS,HD66)
        IF(MT1.NE.0 .AND. IER.EQ.0) GO TO 172
        NF09=NF09+1
        MF9102=0
      END IF
      IF(MF.EQ.10 .AND. NF09.GT.0) THEN
C*      -- FEND Record if MF10
        CALL WRTEXT(LOU,MAT,IZRO,IZRO,NS,BL66)
        NF09=0
      END IF
      DO I=1,NMT
        IF(MTLS(I).EQ.MT) THEN
C* Case: Threshold candidate for adjustment
          EAD=ELOW(I)
          READ (CH66,891) C1,C2,L1,L2,NK,N2
          IF(MT.NE.102) THEN
            CALL WRCONT(LOU,MAT,MF,MT,NS,C1,C2,L1,L2,NK,N2)
          END IF
          DO J=1,NK
            NMX=MXRW/2
            L1 =1
            L2 =1+NMX
            K1 =L1+1
            K2 =L2+1
            NM2=NMX-2
            CALL RDTAB1(LEM,QM,QI,LL,LR,NR,NP,NBT,INR
     &                 ,RWO(K1),RWO(K2),NMX,IER)
            IF(QI.GE.0) THEN
              WRITE(LTT,*) 'Change MF/MT/ZA',MF,MT,LL
     &                    ,' lower energy to',EAD
              WRITE(LLG,*) 'Change MF/MT/ZA',MF,MT,LL
     &                    ,' lower energy to',EAD
              RWO(K1)=EAD
            END IF
            IF(MT.NE.102) CALL WRTAB1(LOU,MAT,MF,MT,NS,QM,QI,LL,LR
     &                               ,NR,NP,NBT,INR,RWO(K1),RWO(K2))
          END DO
          NS=99998
         IF(MF.EQ.10) NF10=NF10+1
C*        -- Process the SEND record
          CALL RDTEXT(LEM,MA1,MF,MT0,CH66,IER)
          IF(MT0.NE.0) THEN
            PRINT *,'ERROR - Reading format MF10,MT',MT
            STOP 'ERROR - Reading format MF10'
          END IF
          IF(MT.NE.102) CALL WRTEXT(LOU,MAT,MF,MT0,NS,CH66)
C*        -- Read first record of the next section
          CALL RDTEXT(LEM,MA1,MF,MT,CH66,IER)
          IF(MF.LE.10) GO TO 170
        END IF
      END DO
C* Case: Copy other MF9/MF10 reactions
  178 DO WHILE(MT.GT.0)
        CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
        CALL RDTEXT(LEM,MA1,MF,MT,CH66,IER)
      END DO
      NS=99998
      IF(MF.EQ.10) NF10=NF10+1
C*    -- Process the SEND record of non-threshold reactions
      CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
C*    -- Read first record of the next section
      CALL RDTEXT(LEM,MA1,MF,MT,CH66,IER)
      IF(MF.EQ.10) GO TO 170
      IF(MF.EQ. 0 .AND. NF10.EQ.0) GO TO 270
C* Last explicitly treated file/section processed - write FEND record
      NS=-1
  180 CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
C*
C-F  Copy the rest of the file
  270 CALL RDTEXT(LEM,MA1,MF,MT,CH66,IER)
C* Skip MF32 from original file if present on the resonance file
      IF(MF.EQ.32 .AND. MF32.EQ.1) THEN
        DO WHILE(MF.EQ.32)
          CALL RDTEXT(LEM,MA1,MF,MT,CH66,IER)
        END DO
      END IF
C* Insert MF32 from the resonance file if present
      IF(MF32.EQ.1 .AND.
     &  (MF.GT.32 .OR. IER.EQ.1 .OR. MA1.LE.0) ) THEN
        CALL WRTEXT(LOU,MAT,MFR,MTR,NS,CR66)
        IER1=0
        DO WHILE(MFR.EQ.32 .AND. IER1.EQ.0)
          CALL RDTEXT(LRR,MA2,MFR,MTR,HD66,IER1)
          CALL WRTEXT(LOU,MAT,MFR,MTR,NS,HD66)
        END DO
        MF32=0
      END IF
C* Check for last record
      IF(IER.EQ.1 .OR. MA1.LE.0) THEN
        CH66=BL66
        MAT=MA1
      END IF
  274 IF(MT.EQ.0) NS=99998
      IF(MF.EQ.0) NS=-1
      CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
c...
c...  print *,' printed: mat,mf,mt,ns',mat,mf,mt,ns
c...
      IF(MT.EQ.0) NS=0
      IF(MAT.GT.0) GO TO 270
      IF(MAT.EQ.0) GO TO 100
C-F  Processing completed
      STOP 'ENDRES Completed'
C* Error trap
  284 STOP 'ENDRES ERROR - Processing terminated'
C*
  691 FORMAT(2A40)
  692 FORMAT(A40,I6)
  693 FORMAT(A40,1P,E10.3)
  696 FORMAT(A66)
  698 FORMAT(BN,I10)
  891 FORMAT(2F11.0,4I11)
  892 FORMAT(I11)
      END
      SUBROUTINE MKEMF9(MF9102,LEM,LS1,FLS1,ELE,RWO,MXRW)
C-Title  : Subroutine MKEMF9
C-Purpose: Prepare MF9 section from MF10 data
C-Description:
C-D  Read the ENDF file on Unit LEM. If MF3/MT102 and MF10/MT102 data
C-D  are present:
C-D  - MF9102  flag is set to 1
C-D  - Cross sections from MF10 are read and normalised by the
C-D    total in MF3 and written on file FLS1 on Unit LS1 as MF9.
C-
      CHARACTER*40 FLS1
      CHARACTER*66 CH66,BL66
      DIMENSION    RWO(MXRW)
      DIMENSION    NBT(20),INR(20)
      BL66="                                 "//
     &     "                                 "
C* Define "small" cross section
      SMALL=1E-12
      MF9102=0
      MF3   =3
      MFR   =10
      MTR   =102
      IER   =0
C* Work array indices:
C* LE1,LX1 original energy/cross section
C* LE2,LX2 total cross section on union grid
C* LE3,LX3 auxilliary array
      NMX=MXRW/6
      LE1=1
      LX1=LE1+NMX
      LE2=LX1+NMX
      LX2=LE2+NMX
      LE3=LX2+NMX
      LX3=LE3+NMX
   10 CALL RDTEXT(LEM,MAT,MF,MT,CH66,IER)
      IF(IER.NE. 0 ) GO TO 90
      IF(MF .EQ.MF3 .AND. MT.EQ.MTR) THEN
C* Read FM3/MF102 (total cross section)
        READ (CH66(12:22),*) AWR
        CALL RDTAB1(LEM,QM,QI,LL,LR,NR,NP2,NBT,INR
     &             ,RWO(LE2),RWO(LX2),NMX,IER)
        IF(IER.NE.0) THEN
          PRINT *,'ERROR reading TAB1 for MF/MT',mf,mt
          STOP 'ENDRES ERROR - MXRW limit exceeded'
        END IF
      END IF
      IF(MF .GT.MFR) GO TO 90
      IF(MF .NE.MFR .OR. MT .NE.MTR) GO TO 10
C* Read FM10/MF102 and write MF9/MT102 to scratch file
      OPEN (UNIT=LS1,FILE=FLS1,STATUS='UNKNOWN')
      MF =9
      MT =MTR
      CALL WRTEXT(LS1,MAT,MF,MT,NS,CH66)
      READ (CH66(45:55),*) NK
c...
      print *,'nk',nk
c...
      DO I=1,NK
c...
        print *,'reading tab1',i
c...
        CALL RDTAB1(LEM,QM,QI,LL,LR,NR,NP,NBT,INR
     &             ,RWO(LE1),RWO(LX1),NMX,IER)
        IF(IER.NE.0) THEN
          PRINT *,'ERROR reading TAB1 for MF/MT',mf,mt
          STOP 'ENDRES ERROR - MXRW limit exceeded'
        END IF
c...
        print *,'writing tab1',i
c...
C*      -- Interpolate accumulated total to the isomer grid
        CALL FITGRD(NP2,RWO(LE2),RWO(LX2),NP,RWO(LE1),RWO(LX3))
C*      -- Normalise by the total
        DO J=1,NP
C...
C...          print *,j,rwo(le1-1+j),rwo(lx1-1+j),rwo(lx2-1+j)
C...
          IF(RWO(LX2-1+J).GT.SMALL) THEN
            RWO(LX1-1+J)=RWO(LX1-1+J)/RWO(LX2-1+J)
          ELSE
            RWO(LX1-1+J)=1.0/NK
          END IF
        END DO
C* Adjust the lower energy range
        IF(QI.GE.0) THEN
          ETH=ELE
        ELSE
          ETH=-QI*(AWR+1)/AWR
        END IF
        RWO(LE1)=ETH
C...
        print *,'            ',i
C...
C* Write the data set to scratch
        CALL WRTAB1(LS1,MAT,MF,MT,NS,QM,QI,LL,LR
     &             ,NR,NP,NBT,INR,RWO(LE1),RWO(LX1))
      END DO
      NS=99999
      IZ=0
      CALL WRTEXT(LS1,MAT,MF,IZ,NS,BL66)
C* Set flag MF9102 and finish
      MF9102=1

      print *,'MF9 written'

      RETURN
C*
C* Trap condition when no data found
   90 CONTINUE
   
        print *,'WARNING - no MF10 data found'

      RETURN
      END
      SUBROUTINE UNIGRD(NEP1,EN1,NEP2,EN2,NEU,EUN,KX)
C-Title  : Subroutine UNIGRID
C-Purpose: Generate union grid from two sets
C-Description:
C-D  Two grids EN1 and EN2 containing NEP1 and NEP2 points respectively
C-D  are merged into EUN with NEU points. Special care is taken to
C-D  retain double points within a grid that allow for function
C-D  discontinuities.
C-
      DIMENSION EN1(NEP1),EN2(NEP2),EUN(KX)
      IF(NEP2.LE.0) GO TO 30
      IF(NEP1.LE.0) GO TO 34
      NEU=0
      J1 =1
      J2 =1
C*
C* Select the grid set for the next point
    8 IF(EN1(J1).GT.EN2(J2)) GO TO 20
C*
C* Add a point from first set
   10 NEU=NEU+1
      IF(NEU.GT.KX) THEN
        PRINT *,'UNIGRD ERROR in grid-1:',NEU,KX
        STOP 'UNIGRD ERROR - KX limit exceeded'
      END IF
      EUN(NEU)=EN1(J1)
C* Test for coincident points in the second grid
      IF(J2.LE.NEP2) THEN
        IF(EN1(J1).EQ.EN2(J2)) J2=J2+1
      END IF
      J1 =J1+1
C* Test for the end of the grid set
      IF(J1.GT.NEP1 .AND. J2.GT.NEP2) GO TO 40
      IF(J1.GT.NEP1) GO TO 20
C* Test for double points in a grid
      IF(EN1(J1).EQ.EUN(NEU)) GO TO 10
      IF(J2.GT.NEP2) GO TO 10
      GO TO 8
C*
C* Add a point from second set
   20 NEU=NEU+1
      IF(NEU.GT.KX) THEN
         PRINT *,'NEU,KX',NEU,KX
         STOP 'UNIGRD ERROR - KX limit exceeded'
      END IF
      EUN(NEU)=EN2(J2)
C* Test for coincident points in the first grid
      IF(J1.LE.NEP1) THEN
        IF(EN1(J1).EQ.EN2(J2)) J1=J1+1
      END IF
      J2 =J2+1
C* Test for the end of the grid set
      IF(J1.GT.NEP1 .AND. J2.GT.NEP2) GO TO 40
      IF(J2.GT.NEP2) GO TO 10
C* Test for double points in a grid
      IF(EN2(J2).EQ.EUN(NEU)) GO TO 20
      IF(J1.GT.NEP1) GO TO 20
      GO TO 8
C*
C* Add all points from the first set
   30 NEU=NEP1
      DO 32 J=1,NEU
      EUN(J)=EN1(J)
   32 CONTINUE
      GO TO 40
C*
C* Add all points from the second set
   34 NEU=NEP2
      DO 36 J=1,NEU
      EUN(J)=EN2(J)
   36 CONTINUE
C*
C* All points processed
   40 CONTINUE
      RETURN
      END
      SUBROUTINE FITGRD(NEP1,EN1,XS1,NEP2,EN2,XS2)
C-Title  : Subroutine FITGRD
C-Purpose: Interpolate a tabulated function to a given grid
C-Description:
C-D Function XS1 at NEP1 argument values in EN1 is interpolated to
C-D NEP2 values XS2 corresponding to argument values in EN2
      DIMENSION EN1(NEP1),XS1(NEP1),EN2(NEP2),XS2(NEP2)
C*
      IF(NEP1.LE.0) THEN
        DO J=1,NEP2
          XS2(J)=0
        END DO
      END IF
      J2=1
      J1=1
C* Test terminal condition - last point in the given grid
   10 IF(J2.GT.NEP2) RETURN
C*
C* Select case for the next point
      IF     (EN1(J1).EQ.EN2(J2)) THEN
C* Case: copy coincident points
        XS2(J2)=XS1(J1)
        IF(J1.LT.NEP1) J1=J1+1
        J2=J2+1
        GO TO 10
      ELSE IF(EN1(J1).GT.EN2(J2)) THEN
C* Case: insert point by interpolation or zero below threshold
        JJ=J1-1
        XX=0
        IF(JJ.GT.0) XX= XS1(JJ)
     1    + (XS1(J1)-XS1(JJ))*(EN2(J2)-EN1(JJ))/(EN1(J1)-EN1(JJ))
        XS2(J2)= XX
        J2=J2+1
        GO TO 10
      ELSE
        IF(J1.LT.NEP1) THEN
C* Case: skip points in original grid
          J1=J1+1
        ELSE
C* Case: ZERO beyond last point in original grid
          XS2(J2)=0
          J2=J2+1
        END IF
        GO TO 10
      END IF
C*
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
