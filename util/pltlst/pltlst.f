      PROGRAM PLTLST
C-Title  : Program PLTLST
C-Purpose: Prepare a list of EXFOR data that can be compared to ENDF
C-Author : A. Trkov, International Atomic Energy Agency, Vienna, Austria
C-Version: 2004/01 original code
C-V  2004/07 Fix format for metastable targets (A.Trkov)
C-V  2004/09 Include inelastic discrete level x-sect. (A.Trkov)
C-V  2005/02 - Deactivate limit of 2 points minimum (A.Trkov)
C-V          - Add projectile ZA in columns
C-V  2006/03 - Re-activate IEX=<2-point minimum for MF>3
C-V            Add MF 1 MT 452,455,456 to list
C-V          - Add MF 4 MT=MT+40000 for cross sections at fixed angle
C-V  2006/11 Process discrete level partial reactions.
C-V  2007/03 Suppress listing MT reactions of 40000 series (input change!)
C-V  2008/01 Apply 1-degree tolerance on angles for 40000 series
C-V  2008/02 Add metastable products (MF10) (A. Trkov).
C-V  2009/01 Add option to force major cross sections and/or 
C-V          double-differential spectra to the list (A. Trkov).
C-V  2009/02 Fix small bug in sequencing the forced entries.
C-V  2011/11 Fix ZA of projectile for added reactions.
C-V  2012/03 - Guard cosines>1 in C4 - interpret as degrees
C-V          - Add mu-bar to the list of reactions (MF3/MT251),
C-V  2012/07 Improve the checking against illegal cosines.
C-M
C-M  Manual for Program PLTLST
C-M  -------------------------
C-M
C-M  The EXFOR file in computational C4 format is scanned. Entries that
C-M  can be plotted are collected and a summary listing of such data
C-M  sets in generated. The output format is equivalent in format to
C-M  the one produced by the PLOTC4 code and may serve as the index
C-M  to the LSTTAB code for comparing EXFOR data to the contents of
C-M  evaluated nuclear data files in ENDF format.
C-M
C-M  Instructions:
C-M  Two input records may be specified:
C-M  - Name of the file with EXFOR data in C4 format (default C4.DAT)
C-M  - Name of the output list file (default PLOTC4.LST)
C-M  - Flags in any order (until EOF):
C-M      "no4000"  Suppress MT reactions of 4000 series
C-M      "xsmajor" Force major cross sections to the list
C-M      "xsddx"   Force double-differential cross sections to the list.
C-M
C-M  The default input filename is PLTLST.INP. If the file does not
C-M  exist, the program tries to read the entries from the default
C-M  input. If an end-of-file is encountered on the input file, the
C-M  remaining entries assume their default values.
C-
      PARAMETER   (MXAN=200,MXDDE=7,MXDDA=2,MXXS=5)
      LOGICAL      EXST
      CHARACTER*1  CHA0,CHA1,CHB0,CHB1,MS0,MS1,MSX
      CHARACTER*2  CH(100)
      CHARACTER*3  CHC0,CHC1
      CHARACTER*10 CH10(3),CX10(3),BL10
      CHARACTER*26 REF
      CHARACTER*40 BLNK,FLNM,FLEX,FLLS,FLIN
      CHARACTER*80 RC1,RC2
      CHARACTER*130    REC
      DIMENSION    ANG(MXAN),ANS(MXAN),KAN(MXAN),ING(MXAN),KNG(MXAN)
      DIMENSION    EDDX(MXDDE),ADDX(MXDDA),MJRXS(MXXS)
      DATA BLNK/'                                        '/
     &     FLEX/'C4.DAT'/
     &     FLLS/'PLOTC4.LST'/
     &     FLIN/'PLTLST.INP'/
      DATA LEX,LLS,LIN,LTT/ 1, 2, 5, 6 /
      DATA PI/ 3.1415926 /
      DATA CH
     1 /'H ','He','Li','Be','B ','C ','N ','O ','F ','Ne'
     2 ,'Na','Mg','Al','Si','P ','S ','Cl','Ar','K ','Ca'
     3 ,'Sc','Ti','V ','Cr','Mn','Fe','Co','Ni','Cu','Zn'
     4 ,'Ga','Ge','As','Se','Br','Kr','Rb','Sr','Y ','Zr'
     5 ,'Nb','Mo','Tc','Ru','Rh','Pd','Ag','Cd','In','Sn'
     6 ,'Sb','Te','I ','Xe','Cs','Ba','La','Ce','Pr','Nd'
     7 ,'Pm','Sm','Eu','Gd','Tb','Dy','Ho','Er','Tm','Yb'
     8 ,'Lu','Hf','Ta','W ','Re','Os','Ir','Pt','Au','Hg'
     9 ,'Tl','Pb','Bi','Po','At','Rn','Fr','Ra','Ac','Th'
     * ,'Pa','U ','Np','Pu','Am','Cm','Bk','Cf','Es','Fm'/
C* Tolerances for grouping data sets
C* ETOL Fractional tolerance for differentiating incident energies of
C*      differential and double differential data.
C* ITOL Angle difference for differentiating energy distributions at
C*      fixed angles.
C* MTOL Angle difference (degrees) for differentiating energy-dependent
C*      cross sections at fixed angles.
      DATA ETOL,ITOL,MTOL/ 0.015, 5, 1 /
      DATA BL10/'          '/
      DATA EDDX/ 2.E6, 10.E6, 14.E6, 20.E6, 60.E6, 100.E6, 150.E6/
      DATA ADDX/ 20., 160./
      DATA MJRXS/ 1, 4, 16, 18, 102 /
      NO4000=0
      NXSMJR=0
      NDDXN =0
      NDDXA =1
      DDXN  =0
      IZX   =0
      IAX   =0
      MSX   =' '
      RC1   =' '
      DO I=1,MXAN
        KAN(I)=0
        KNG(I)=0
        ANS(I)=0
      END DO
      IZI1=0
      IZA1=0
      IZP1=1
      MS1 =' '
      MF1 =0
      MT1 =0
      CHA1=' '
      CHB1=' '
      CHC1=' '
      ENR1=0
      DEN1=0
      XSR1=0
      DXS1=0
      PRA1=0
      PRB1=0
      LVL1=0
      ELV1=0
      REF1=0
      NEN1=0
      NSU1=0
      IZIX=0
C* Write the banner
      WRITE(LTT,903) BLNK
      WRITE(LTT,903) ' PLTLST - Generate listing of EXFOR data'
      WRITE(LTT,903) ' ---------------------------------------'
      WRITE(LTT,903) BLNK
C*
C* Check for the existence of the input file
      INQUIRE(FILE=FLIN,EXIST=EXST)
      IF(EXST) OPEN(UNIT=LIN,FILE=FLIN,STATUS='OLD')
C* If input does not exist, try reading the default input
      IF(.NOT. EXST)
     &WRITE(LTT,903) ' Enter the C4 filename                  '
      READ (LIN,903,END=10) FLNM
      IF(FLNM.NE.BLNK) FLEX=FLNM
      IF(.NOT. EXST)
     &WRITE(LTT,903) ' Enter the output list filename         '
      READ (LIN,903,END=10) FLNM
      IF(FLNM.NE.BLNK) FLLS=FLNM
C* Read flags until blank or EOF
      IF(.NOT. EXST)
     &WRITE(LTT,903) ' Enter additional commands (end=CTRL^Z) '
    8 READ (LIN,903,END=10) FLNM
      IF(FLNM.EQ.BLNK) GO TO 10
      IF(FLNM(1:6).EQ.'no4000') THEN
        NO4000=1
        WRITE(LTT,903) ' Suppress MT reactions of 4000 series   '
      END IF
      IF(FLNM(1:6).EQ.'xsddx') THEN
        NDDXN=1
        WRITE(LTT,903) ' Force double-differen. x.s. to the list'
      END IF
c... Temporarily inactive until tested!!!
      IF(FLNM(1:7).EQ.'xsmajor') THEN
        NXSMJR=1
        WRITE(LTT,903) ' Force major cross sections to the list '
      END IF
      GO TO 8
C* Open the files
   10 OPEN (UNIT=LEX,FILE=FLEX,STATUS='OLD')
      OPEN (UNIT=LLS,FILE=FLLS,STATUS='UNKNOWN')
C* Write the banner
      WRITE(LLS,910)
      WRITE(LLS,912)
      WRITE(LLS,910)
C*
C* Read the first C4 record
      IEX=1
      IDX=0
  17  READ (LEX,901,END=80) REC
      IF(REC(1:1).EQ.'#') GO TO 17
      IF(REC(1:40).EQ.BLNK) GO TO 80
      READ (REC,902) IZI0,IZA0,MS0,MF0,MT0,CHA0,CHB0,ENR0,DEN0,XSR0,DXS0
     &              ,PRA0,PRB0,PRC0,PRD0,CHC0,REF0,NEN0,NSU0
      NAN=0
      MAN=0
      IZP0=1
      LVL0=0
      ELV0=0
      ELZ0=0
      IF(MT0.EQ.   2) IZP0=IZI0
      IF(MT0.EQ.   4) IZP0=IZI0
      IF(MT0.EQ. 102) IZP0=   0
      IF(MT0.EQ. 103) IZP0=1001
      IF(MT0.EQ. 601) IZP0=1001
      IF(MT0.EQ. 104) IZP0=1002
      IF(MT0.EQ. 105) IZP0=1003
      IF(MT0.EQ. 106) IZP0=2003
      IF(MT0.EQ. 107) IZP0=2004
      IF(MT0.EQ. 801) IZP0=2004
      IF(MT0.EQ.9000) IZP0=PRB0
      IF(MT0.EQ.51 .OR. MT0.EQ.601 .OR. MT0.EQ.801) LVL0=1
      IF(PRC0 .NE. 0) THEN
C*      -- Define level energy or range (if applicable)
        LVL0=1
        ELV0=PRC0
        ELZ0=0
        IF(PRD0.GT.0) THEN
          ELV0=MAX(PRC0,PRD0)
          IF(MT0.NE.9000) ELZ0=MIN(PRC0,PRD0)
        END IF
      END IF
      IF(CHA0.EQ.'T' .OR. CHA0.EQ.'+') CHA0=' '
      IF(CHA0.EQ.'1') CHA0='M'
      IF(CHA0.EQ.'2') CHA0='N'
      IF(CHA0.EQ.'3') CHA0='O'
C*    -- mu-bar
      IF(MF0.EQ.154) THEN
        IF(MT0.EQ.2 .AND. NINT(PRA0).EQ.1) THEN
          MF0=3
          MT0=251
        END IF
      END IF
C*
C* Process all C4 records and check for changes
   20 CONTINUE
      IEF =1
      MMF =99
      MMT =999
   18 READ (LEX,901,END=40) REC
      IF(REC(1:1).EQ.'#') GO TO 18
      IF(REC(1:40).EQ.BLNK) GO TO 40
      READ (REC,902) IZI1,IZA1,MS1,MF1,MT1,CHA1,CHB1,ENR1,DEN1,XSR1,DXS1
     &              ,PRA1,PRB1,PRC1,PRD1,CHC1,REF1,NEN1,NSU1
      IEF =0
      IZP1=1
      LVL1=0
      ELV1=0
      ELZ1=0
      IF(MT1.EQ. 102) IZP1=   0
      IF(MT1.EQ. 103) IZP1=1001
      IF(MT1.EQ. 104) IZP1=1002
      IF(MT1.EQ. 105) IZP1=1003
      IF(MT1.EQ. 106) IZP1=2003
      IF(MT1.EQ. 107) IZP1=2004
      IF(MT0.EQ.9000) IZP1=PRB1
      IF(MT1.EQ.   2) IZP1=IZI1
      IF(MT1.EQ.   4) IZP1=IZI1
      IF(MT1.EQ.  51) IZP1=IZI1
      IF(MT1.EQ. 601) IZP1=1001
      IF(MT1.EQ. 801) IZP1=2004
      IF(MT1.EQ.51 .OR. MT1.EQ.601 .OR. MT1.EQ.801) LVL1=1
      IF(PRC1 .NE. 0) THEN
C*      -- Define level energy or range (if applicable)
        LVL1=1
        ELV1=PRC1
        ELZ1=0
        IF(PRD1.GT.0) THEN
          ELV1=MAX(PRC1,PRD1)
          IF(MT1.NE.9000) ELZ1=MIN(PRC1,PRD1)
        END IF
      END IF
      IF(CHA1.EQ.'T' .OR. CHA1.EQ.'+') CHA1=' '
      IF(CHA1.EQ.'1') CHA1='M'
      IF(CHA1.EQ.'2') CHA1='N'
      IF(CHA1.EQ.'3') CHA1='O'
C*    -- mu-bar
      IF(MF1.EQ.154) THEN
        IF(MT1.EQ.2 .AND. NINT(PRA1).EQ.1) THEN
          MF1=3
          MT1=251
c...
c...      print *,'mf0,mt0,pra0,mf1,mt1,pra1',mf0,mt0,pra0,mf1,mt1,pra1
c...
        ELSE
          GO TO 20
        END IF
      END IF
C* Ignore MT 51 without specified level energies
      IF(MT1.EQ.51 .AND. ELV1.EQ.0) THEN
        GO TO 20
      END IF
C* Mark for printout if any of the parameters change
c...
C...  if(mf1.eq.3 .and. mt1.eq.102) print *,'mf1,mt1,m0,m1'
C... &                                      ,mf1,mt1,cha1,cha0
c...
      IF(IZI1.NE.IZI0) GO TO 40
      IF(IZA1.NE.IZA0) GO TO 40
      IF(MS1 .NE.MS0 ) GO TO 40
      IF(MF1 .NE.MF0 ) GO TO 40
      IF(MT1 .NE.MT0 ) GO TO 40
      IF(CHA0.NE.CHA1) GO TO 40
C* Mark partial inelastic MF 3 where level energy changes
      IF(MF1 .EQ. 3 .AND. (ELV1.NE.ELV0 .OR. ELZ1.NE.ELZ0)) GO TO 40
C* Mark ang.distr. MF 4 where level energy changes and save angle
      IF(MF1 .EQ. 4) THEN
        IF(ELV1.NE.ELV0 .OR. ELZ1.NE.ELZ0) GO TO 40
        IF(ABS(PRA1).GT.1) THEN
          ANJ=PRA1
          WRITE(LTT,905) ' PLTLST WARNING - Angular cosine        ',ANJ
          WRITE(LTT,905) '                  assumed to be degrees '
        ELSE
          ANJ=ACOS(PRA1)*180/PI
        END IF
        JAN=NINT(ANJ)
        NEW=1
        IF(MAN.GT.0) THEN
          DO J=1,MAN
            IAN=ING(J)
            IF(ABS(IAN-JAN).LE.MTOL) THEN
              NEW=0
              KNG(J)=KNG(J)+1
            END IF
          END DO
        END IF
        IF(NEW.EQ.1) THEN
          MAN=MAN+1
          IF(MAN.GT.MXAN) STOP 'PLTLST ERROR - MXAN Limit exceeded'
          ING(MAN)=JAN
        END IF
        NEW=1
        IF(NAN.GT.0) THEN
          DO J=1,NAN
            IAN=NINT(ANG(J))
            IF(ABS(IAN-JAN).LE.ITOL) THEN
              NEW=0
              KAN(J)=KAN(J)+1
              ANS(J)=ANS(J)+ANJ
            END IF
          END DO
        END IF
        IF(NEW.EQ.1) THEN
          NAN=NAN+1
          IF(NAN.GT.MXAN) STOP 'PLTLST ERROR - MXAN Limit exceeded'
          ANG(NAN)=ANJ
          ANS(NAN)=ANJ
          KAN(NAN)=1
        END IF
      END IF
C* Mark spectra MF 5 where outgoing particle angle changes
      IF(MF1 .EQ. 5 .AND. PRA1.NE.PRA0) GO TO 40
C* Mark spectra MF 6 where outgoing particle angle changes
      IF(MF1 .EQ. 6 .AND. PRA1.NE.PRA0) GO TO 40
C* Mark spectra where outgoing particle changes
      IF(IZP1.NE.IZP0) GO TO 40
C* Mark entries for MF>3 where incident particle energy changes
      DE=ABS(ENR1-ENR0)
      IF((MF1 .GT. 3 .AND. MF1.NE.10) .AND. DE.GT.ENR0*ETOL) GO TO 40
C* None of the print conditions is satisfied - add to the data count
      IEX=IEX+1
      GO TO 20
C*
C* Reaction/Energy/Particle change - print record for previous set
   40 CONTINUE
C* Exclude printout for the following conditions
C* - MF out of range
c...
C...  if(mf1.eq.3 .and. mt1.eq.102) print *,'passed 40'
c...
      IF(MF0.NE.1 .AND. MF0.NE.3 .AND. MF0.NE.10 .AND.
     &   MF0.NE.4 .AND. MF0.NE.5 .AND. MF0.NE.6) GO TO 60
C* - MT out of range
      IF(MT0.GT.999 .AND. MT0.NE.9000) GO TO 60
C* - Insufficient number of points (this also excludes distributions
C*   which are not suitably sorted and would result in excessive output)
c...
c...  print *,'iex,mf0,mt0',iex,mf0,mt0
c...
c...  IF(IEX.LE.2) GO TO 60
      IF(IEX.LE.2 .AND.
     &  (MF0.GT.3 .AND. MF0.NE.10)) GO TO 50
C*
C* Printout conditions satisfied - prepare output record
      IZ=IZA0/1000
      IA=IZA0-1000*IZ
      DO I=1,3
        CH10(I)='          '
      END DO
C* Consider different cases
      MMF=MF0
      MMT=MT0
      IF     (MF0.EQ.3) THEN
C* - MF3 discrete level energy (if present)
        IF(LVL0.NE.0) THEN
          WRITE(CH10(3),'(1P,E10.4E1)') ELV0
          IF(ELZ0.GT.0) THEN
            WRITE(CH10(2),'(1P,E8.2E1)') ELZ0
            CH10(3)(1:1)='+'
          END IF
        END IF
        IF(CHA0.NE.' ') THEN
            MMF=10
            IF     (CHA0.EQ.'G') THEN
              WRITE(CH10(3),'(F10.1)') 0.
            ELSE IF(CHA0.EQ.'M') THEN
              WRITE(CH10(3),'(F10.1)') 1.
            ELSE IF(CHA0.EQ.'N') THEN
              WRITE(CH10(3),'(F10.1)') 2.
            ELSE IF(CHA0.EQ.'O') THEN
              WRITE(CH10(3),'(F10.1)') 3.
            ELSE
              PRINT *,'Unrecognised metastable state flag ',CHA0
            END IF
        END IF
      ELSE IF(MF0.EQ.4) THEN
C* - MF4 incident particle energy and level
        WRITE(CH10(1),'(1P,E10.3E1)') ENR0
        IF(LVL0.NE.0) THEN
          WRITE(CH10(3),'(1P,E10.4E1)') ELV0
          IF(ELZ0.GT.0) THEN
            WRITE(CH10(2),'(1P,E8.2E1)') ELZ0
            CH10(3)(1:1)='+'
          END IF
        END IF
      ELSE IF(MF0.EQ.5) THEN
C* - MF 5 incident particle energy
        WRITE(CH10(1),'(1P,E10.3E1)') ENR0
      ELSE IF(MF0.EQ.6) THEN
C* - MF 6 incident particle energy and outgoing particle angle
        WRITE(CH10(1),'(1P,E10.3E1)') ENR0
C... Print 2 decimal places for consistency with PLOTC4 output
C...    WRITE(CH10(2),'(F8.1)') ACOS(PRA0)*180/PI
        WRITE(CH10(2),'(F8.2)') ACOS(PRA0)*180/PI
      END IF
C* Check for close-lying discrete levels
      RC2=RC1
      WRITE(RC1,914) IZ,CH(IZ),IA,MS0,IZP0,MF0,MT0,IEX,CH10,IDX
      IF(MMF.EQ.3 .AND. IDX.GT.0) THEN
        IF(RC1(1:26).EQ.RC2(1:26)) THEN
          READ (RC1(63:72),904) FLV
          READ (RC2(63:72),904) FL1
          IFLV=NINT(FLV)
          IFL1=NINT(FL1)
          IF(ABS(IFLV-IFL1).LE.150) THEN
            RC1=RC2
            GO TO 60
          END IF
        END IF
      END IF
C* Check for any entries forced from input
   42 MFX=0
      IF(IDX.EQ.0) THEN
        IZX   =IZ
        IAX   =IA
        MTX   =MMT
        MSX   =MS0
      END IF
      IF(NXSMJR.GT.0) THEN
C*      -- Force major cross sections listing
        IF(IEF.NE.0) THEN
          IF(IDX.LE.0) STOP 'PLTLST ERROR - No data in C4 file'
          GO TO 43
        END IF
        IF(IDX.GT.0 .AND. (IZ.NE.IZX .OR. IA.NE.IAX)) GO TO 43
        IF(MMF.GT.3 .AND. MMF.NE.10) GO TO 43
        IF(MMF.EQ.3) THEN
          IF(NXSMJR.GT.MXXS) GO TO 44
          IF(MMT.GT.MJRXS(NXSMJR)) THEN
            GO TO 43
          ELSE
            IF(MMT.EQ.MJRXS(NXSMJR)) NXSMJR=NXSMJR+1
          END IF
        END IF
        GO TO 44
C*      -- Set output record
   43   IF(NXSMJR.GT.MXXS) GO TO 44
        MTX   =MJRXS(NXSMJR)
        NXSMJR=NXSMJR+1
        IF(MTX.EQ.18 .AND.IZX.LT.90) GO TO 48
        MFX   =3
        IZIX  =IZI0
        IF     (MTX.EQ.  1) THEN
          IZPX  =0
        ELSE IF(MTX.EQ.  4) THEN
          IZPX  =IZIX
        ELSE IF(MTX.EQ. 16) THEN
          IZPX  =1
        ELSE IF(MTX.EQ.102) THEN
          IZPX  =0
        ELSE
          IZPX  =0
        END IF
        CX10(1)='          '
        CX10(2)='          '
        CX10(3)='          '
        GOTO 48
      END IF
C*
   44 IF(NDDXN.GT.0 .AND. NDDXN.LT.MXDDE) THEN
      IF( MMF.LT.6) GO TO 48
      IF((MMF.EQ.6 .AND. MMT.LT.9000) .OR. IZI0.GT.1) GO TO 48
C* Force double-differential spectra listing
        IF(IEF.NE.0) THEN
          IF(IDX.LE.0) STOP 'PLTLST ERROR - No data in C4 file'
          GO TO 46
        END IF
        IF(IDX.GT.0 .AND. (IZ.NE.IZX .OR. IA.NE.IAX)) GO TO 46
        IF(MMF.GT.6 .AND. MMF.NE.10) GO TO 46
        IF(MMF.EQ.6 .AND. IZP0.EQ.1) THEN
          IF(MMT.GT.9000) GO TO 46
          IF(MMT.EQ.9000) THEN
            IF(ABS(ENR0-EDDX(NDDXN)).GT. 0.5E6) THEN
              IF(NDDXN.EQ.1 .OR. ENR0.GT.EDDX(NDDXN)) GO TO 46
            ELSE
              IF(NDDXN.LT.MXDDE) NDDXN=NDDXN+1
            END IF
          END IF
        END IF
        GO TO 48
C*      -- Set output record
   46   IF(IZIX.EQ.0) IZIX=IZI0
        IZPX  =IZIX
        MFX   =6
        MTX   =9000
        WRITE(CX10(1),'(1P,E10.3E1)') EDDX(NDDXN)
        WRITE(CX10(2),'(F8.2)')       ADDX(NDDXA)
        CX10(3)='          '
        IF(NDDXA.LT.MXDDA) THEN
          NDDXA=NDDXA+1
        ELSE
          NDDXN=NDDXN+1
          NDDXA=1
        END IF
      END IF
   48 IF(MFX.NE.0) THEN
C*        -- Force neutron emission spectra
          IDX=IDX+1
          WRITE(LLS,914) IZX,CH(IZX),IAX,MSX,IZPX,MFX,MTX,0
     &                  ,CX10,IDX,IZIX
          GO TO 42
      END IF
      IF(MMF.GE.99) GO TO 80
C* Write a record to output list file
C* Suppress elastic angular distributions for charged particles
      IF(MMF.NE.4 .OR. MMT.NE.2 .OR. IZI0.LE.1) THEN
        IDX=IDX+1
        WRITE(LLS,914) IZ,CH(IZ),IA,MS0,IZP0,MMF,MMT,IEX,CH10,IDX,IZI0
      END IF
      MMF=99
      MMT=999
      IZX=IZ
      IAX=IA
      MSX=MS0
   50 CONTINUE
C...
C...  print *,'NO4000,mf0,mf1,mt0,mt1,ief',NO4000,mf0,mf1,mt0,mt1,ief
C...
C* Override NO4000 for incident charged particles
C...  IF(NO4000.EQ.1) GO TO 60
      IF(NO4000.EQ.1 .AND.
     &  (IZI0.LE.1 .OR. MF0.NE.4 .OR. MT0.NE.2)) GO TO 60
C*
      IF( MF0.EQ.4 .AND.
     &   (MF1.GT.MF0 .OR. MT1.NE.MT0 .OR. IEF.EQ.1)) THEN
        CH10(1)='          '
C* Use fine angular mesh for 40000 series
        DO J=1,MAN
C*        -- Sort angles in descending order
          JAN=ING(1)
          KK =1
          DO K=1,MAN
            IAN=ING(K)
            IF(IAN.GT.JAN) THEN
              KK=K
              JAN=IAN
            END IF
          END DO
          WRITE(CH10(2),'(F8.0)') FLOAT(JAN)
          ING(KK)=-JAN
          JEX=KNG(KK)
          JT0=MT0+40000
          IF(JEX.GT.1) THEN
            IDX=IDX+1
            WRITE(LLS,914) IZ,CH(IZ),IA,MS0,IZP0
     &                    ,MF0,JT0,JEX,CH10,IDX,IZI0
          END IF
        END DO
        MAN=0
      END IF
C*
C* Reset parameters
   60 IEX=1
      IZI0=IZI1
      IZA0=IZA1
      IZP0=IZP1
      MS0 =MS1
      MF0 =MF1
      MT0 =MT1
      CHA0=CHA1
      CHB0=CHB1
      CHC0=CHC1
      ENR0=ENR1
      DEN0=DEN1
      XSR0=XSR1
      DXS0=DXS1
      PRA0=PRA1
      PRB0=PRB1
      LVL0=LVL1
      ELV0=ELV1
      ELZ0=ELZ1
      REF0=REF1
      NEN0=NEN1
      NSU0=NSU1
      IF(IEF.EQ.1) GO TO 42
      GO TO 20
C* All processing completed
   80 WRITE(LLS,910)
      WRITE(LTT,903) BLNK
      STOP 'PLTLST Completed'
C*
  901 FORMAT(A130)
  902 FORMAT(I5,I6,A1,I3,I4,2A1,1X,8F9.0,A3,A26,I4,I3)
  903 FORMAT(2A40)
  904 FORMAT(BN,F10.0)
  905 FORMAT(A40,1P,E10.3)
  910 FORMAT(' ======================================='
     &      ,'====================================  ======')
  912 FORMAT(' MATERIAL   ZAOUT  MF   MT  EVAL. EXPR. '
     &      ,'EXPR.    E-INC ANG-OUT ELV/E-OUT IDX    PROJ'/
     &       '                            PNTS. PNTS. '
     &      ,' REF.       EV     DEG        EV    ')
  914 FORMAT(I3,'-',A2,'-',I3,A1,I6,I4,I5,6X,I6,6X,A10,A8,A10,I4,I8)
      END
