      PROGRAM EMPEND
C-Title  : EMPEND Program
C-Purpose: Convert EMPIRE output into ENDF format
C-Author : A.Trkov
C-Version: 1996 Original code (ENEA, Bologna, Italy)
C-V  98/10 Final version for distribution.
C-V  99/11 Minor corrections to fix bugs.
C-V  00/11 Implement formatting of elastic ang.distrib.
C-V  00/12 Process total cross section.
C-V  02/05 Fix MF4 processing for discrete levels.
C-V  02/09 - Fix thresholds for multi-particle emission
C-V        - Photon branching ratios for the last level (MF12)
C-V        - Atomic weight ratios for target and recoils.
C-V  02/11 Implement continuum photon spectra processing.
C-M  
C-M  Manual for Program EMPEND
C-M  =========================
C-M  
C-M  The EMPIRE output is processed and converted into ENDF 
C-M  format. The ENDF formatted output file has to obey the
C-M  rule of ordering the reactions in increasing order by
C-M  the reaction MT number, so several sweeps of the EMPIRE
C-M  file are made:
C-M   - In the first sweep the cross sections and the
C-M     corresponding reaction Q-values are extracted.
C-M   - In the next sweep all reactions for which particle
C-M     spectra are given are identified.
C-M   - Another sweep is made for each reaction requiring
C-M     an ENDF file-4 section. Particularly, these
C-M     data are entered for discrete level reactions.
C-M   - Next follows a sweep for each reaction having
C-M     energy-angle correlated outgoing particle distributions.
C-M   - Finally, a sweep is made for the remaining reactions,
C-M     particularly the (n,gamma) reaction, for which the
C-M     particle distributions are coded in ENDF Files-12, 14
C-M     and 15.
C-M  
C-M  The cross section data found on the file are fitted by
C-M  a cubic spline and entered into the output ENDF file on
C-M  a user-defined dense energy grid, thinned to the specified
C-M  tolerance and taking reaction thresholds into account.
C-M  If desired, the spline interpolation may be suppressed
C-M  and the energy points found on the file are entered
C-M  directly into the ENDF formatted file.
C-M  
C-M  The angular distributions for discrete level reactions
C-M  that appear in the ENDF file-4 sections are extracted from
C-M  the spectra on the EMPIRE output file, interpolated to
C-M  the appropriate energy, if necessary.
C-M
C-M  The correlated energy-angle distributions for continuum
C-M  reactions that appear in ENDF file-6 sections are entered
C-M  in Legendre polynomial representation in the centre-of-
C-M  mass coordinate system. The maximum Legendre order is
C-M  limited to 12. For reactions with relatively smooth
C-M  angular distributions, the number of coefficients is
C-M  reduced accordingly.
C-M
C-M  Photon production reactions, which remain to be specified,
C-M  particularly the (n,gamma) reaction, are given in the ENDF
C-M  files-12, 14 and 15. Photon multiplicity is stored in 
C-M  file-12. Isotropic angular distribution is assumed and
C-M  written to file 14. The particle energy distribution is
C-M  written to file15.
C-M
C-M  Instructions:
C-M  The program can be executed interactively from a terminal
C-M  screen. The required input is entered in response to the
C-M  prompts, which are the following:
C-M   - The name of the EMPIRE output file to be processed.
C-M   - The name of the ENDF formatted file to be written.
C-M   - Number of subintervals per incident neutron energy 
C-M     interval on the EMPIRE output file. The subintervals
C-M     define the fine energy mesh on the ENDF formatted
C-M     file. If zero is entered, only the points on the
C-M     EMPIRE output are entered to the ENDF formatted file.
C-M   - Thinning tolerance limit [%] to reduce the number of
C-M     cross section points. Data points which, can be
C-M     reproduced from the neighbouring points by linear 
C-M     interpolation to within the specified tolerance, are 
C-M     removed. Entering a negative value for the thinning
C-M     tolerance limit causes thinning to be suppressed.
C-M   - ENDF material number identifier.
C-M
C-M  To monitor the formatting process for quality assurance
C-M  purposes, the EMPEND.LOG file is written in which the
C-M  details of the formatting process are recorded. A limited
C-M  amount of checking is done. An entry to the log file is
C-M  added in the follwing cases:
C-M   - The cross section obtained by integrating the spectrum
C-M     should agree with the value given directly in the
C-M     EMPIRE output file. If the difference exceeds 2%,
C-M     a warning message is written, giving the reaction
C-M     MT number, the incident particle energy, the expected
C-M     cross section (i.e. the value given directly in the
C-M     EMPIRE output file) and the percent difference.
C-M   - The angular distributions are fitted to determine the
C-M     Legendre polynomial expansion coefficients. If the
C-M     distribution reconstructed from the Legendre polynomial
C-M     coefficients differs from the pointwise values on the
C-M     EMPIRE output file by more than 5%, a warning message
C-M     is written, giving the reaction MT number, the outgoing
C-M     particle ZA identifier, the incident and the outgoing
C-M     particle energies and the percent difference in the
C-M     fitted distribution from the pointwise value on the
C-M     file.
C-M  Additional messages monitor the progress of the data
C-M  formatting process.
C-M
C-
C* MXE - Maximum number of incident particle energy points.
C* MXT - Maximum number of reactions (including discrete levels).
C* MXM - Maximum number of residual nuclei.
C* MXR - Lengrh of the real work array RWO.
C* MXI - Length of the integer work array IWO.
      PARAMETER   (MXE=100,MXT=200,MXM=60,MXR=120000,MXI=4000)
      CHARACTER*40 BLNK,FLNM,FLN1,FLN2,FLER
      CHARACTER*80 REC
      DIMENSION    EIN(MXE),QQM(MXT),QQI(MXT)
     1            ,RWO(MXR),IWO(MXI)
C* Filenames and logical file units
      DATA LIN,LOU,LKB,LTT,LER / 1, 2, 5, 6, 7 /
      DATA BLNK/'                                        '/
     1    ,FLN1/'empire.out'/
     2    ,FLN2/'empire.end'/
     3    ,FLER/'empend.log'/
C* Default cross section linear interpolation tolerance limit 0%
      DATA ERR/0./
      DATA ZRO/0./,RWO/MXR*0./
C*
C* Define input parameters - Write banner to terminal
      WRITE(LTT,991) ' EMPEND - Convert EMPIRE output to ENDF '
      WRITE(LTT,991) ' ====================================== '
      WRITE(LTT,991)
C* Define the source file
   12 WRITE(LTT,991) ' Default source filename (EMPIRE out.): ',FLN1
      WRITE(LTT,991) '$          Enter new name to redefine : '
      READ (LKB,991) FLNM
      IF(FLNM.NE.BLNK) FLN1=FLNM
      OPEN(UNIT=LIN,FILE=FLN1,STATUS='OLD',ERR=12)
C* Define the output file
   14 WRITE(LTT,991) ' Default output filename              : ',FLN2
      WRITE(LTT,991) '$          Enter new name to redefine : '
      READ (LKB,991) FLNM
      IF(FLNM.NE.BLNK) FLN2=FLNM
      OPEN (UNIT=LOU,FILE=FLN2,STATUS='UNKNOWN')
C* Define the number of points for cross sections fine mesh
      WRITE(LTT,991) ' Number of x-s fine mesh subintervals   '
      WRITE(LTT,991) '$    Enter blank to use original mesh : '
      READ (LKB,992) NEP
      IF(NEP.EQ.0) GO TO 15
C* Define the linear interpolation tolerance limit
      WRITE(LTT,991) ' Interp. thinning tolerance limit [%] : '
      WRITE(LTT,991) '$      Enter negative for No-thinning : '
      READ (LKB,994) X
      IF(X.GT.0) ERR=X*0.01
C* Define the MAT number
   15 WRITE(LTT,991) '$Enter the ENDF material MAT number   : '
      READ (LKB,992) MAT
      IF(MAT.LE.0)   MAT=1111
C* Define the test printout control parameters (-1, 0, MT)
C... Hardwired specifications
      IPRNT=0
C* If specific MT requested, define also energy range and particle
      IF(IPRNT.GT.0) THEN
C* Incident particle energy range
        EI1  =0.
        EI2  =2.E7
C* Outgoing particle energy range
        EO1  =0.
        EO2  =2.E7
C* Outgoing particle ZA identifier
        NZA1 =0
        NZA2 =99999
      END IF
C*
C* Write the input options to Log-file
   16 OPEN (UNIT=LER,FILE=FLER,STATUS='UNKNOWN')
      WRITE(LER,991) ' EMPEND - Convert EMPIRE output to ENDF '
      WRITE(LER,991) ' ====================================== '
      WRITE(LER,991)
      WRITE(LER,991) ' Source EMPIRE output filename        : ',FLN1
      WRITE(LER,991) ' ENDF-6 formatted output filename     : ',FLN2
      WRITE(LER,995) ' Number of x-s fine mesh subintervals   ',NEP
      WRITE(LER,996) ' Interp. thinning tolerance limit [%] : ',ERR*100.
      WRITE(LER,991)
C*
C* Define work array addresses
      MTH=1
      IZB=MTH+MXT
      LBI=IZB+MXM
      IF(LBI.GT.MXI) STOP 'EMPEND ERROR - MXI limit exceeded'
      LXS=1
      LBE=LXS+MXE*MXT
      LSC=LBE+3*MXM
      LXR=MXR-LSC
      IF(LSC.GT.MXR) STOP 'EMPEND ERROR - MXR limit exceeded'
C*
C* Read the EMPIRE output file to extract the cross sections
      CALL REAMF3(LIN,LTT,LER,MXE,MXT,MXM
     1           ,EIN,RWO(LXS),QQM,QQI,IWO(MTH),IWO(IZB),RWO(LBE)
     1           ,IZA,AWR,NEN,NXS)
C* Summ MF 5 contributions if necessary
      CALL SUMMF5(NXS,NEN,IWO(MTH),RWO(LXS),MXE,MXT)
C* Write the ENDF file header record to output
      REC=' EMPEND Processing file : '//FLN1
      NS =-1
      CALL WRTEXT(LOU, 0, 0, 0,NS,REC)
C* Write the ENDF file-1 data
      EMX=EIN(NEN)
      CALL WRIMF1(LOU,MAT,IZA,AWR,EMX,NS)
C* Write the ENDF file-3 data
      CALL WRIMF3(LOU,MXE,MXT,LXR
     1           ,EIN,RWO(LXS),QQM,QQI,IWO(MTH),RWO(LSC)
     1           ,MAT,IZA,AWR,NEN,NEP,NXS,ERR,NS)
      WRITE(LTT,991)
      WRITE(LTT,995) ' Processed reaction cross sections    : ',NXS
      WRITE(LTT,995) '        Number of input energy points : ',NEN
      WRITE(LTT,991)
      WRITE(LER,991)
      WRITE(LER,995) ' Processed reaction cross sections    : ',NXS
      WRITE(LER,995) '        Number of input energy points : ',NEN
      WRITE(LER,991)
      ELO=EIN(1)
C*
C* Scan the EMPIRE output for all reactions with energy/angle distrib.
      REWIND LIN
      JT6=MXI-LBI
      CALL SCNMF6(LIN,NT6,IWO(LBI),JT6)
      IF(NT6.LE.0) GO TO 880
      JT4=0
      JT6=0
C*
C* Read the EMPIRE output file to extract angular distributions
  400 JT6=JT6+1
      MT6=IWO(LBI-1+JT6)
C* Process discrete levels if continuum reactions present
      IF(MT6.NE.  2 .AND.
     1   MT6.NE.  5 .AND.
     1   MT6.NE. 91 .AND.
     1   MT6.NE.649 .AND.
     1   MT6.NE.849) GO TO 490
      LE=LBE
      LG=LE+NEN+2
      LA=LG+NEN+2
      LX=MXR-LA
      IF(LA.GT.MXR) STOP 'EMPEND ERROR - MXR limit exceeded'
      REWIND LIN
      JPRNT=-1
      IF(MT6.EQ.2) JPRNT=IPRNT
C* Reading angular distributions - MF6 flagged negative
      CALL REAMF6(LIN,LTT,LER,EIN,RWO(LXS),NEN,RWO(LE),RWO(LG),RWO(LA)
     1           ,IWO(MTH),-MT6,QQM,QQI,AWR,ELO,NXS,NK,LCT,MXE,LX
     2           ,JPRNT,EI1,EI2,EO1,EO2,NZA1,NZA2)
      IF(NK.LE.0) GO TO 490
C* Write the ENDF file-4 data
      MT4=2
  420 CALL WRIMF4(LOU,IWO(MTH),QQM,QQI,NXS,MT6,RWO(LA)
     1           ,MT4,MAT,IZA,AWR,LCT,NS)
      IF(MT4.LE.0) GO TO 490
      WRITE(LTT,995) ' Processed angular distrib. for MT    : ',MT4
      WRITE(LTT,991)
      WRITE(LER,995) ' Processed angular distrib. for MT    : ',MT4
      WRITE(LER,991)
      JT4=JT4+1
      MT4=MT4+1
      GO TO 420
  490 IF(JT6.LT.NT6) GO TO 400
C* Angular distribution data processed
      IF(JT4.GT.0)
     1CALL WRCONT(LOU,MAT, 0, 0,NS,ZRO,ZRO, 0, 0, 0, 0)
      JT6=0
      DO 492 I=1,NXS
      IWO(MTH-1+I)=ABS(IWO(MTH-1+I))
  492 CONTINUE
C* Read the EMPIRE output file to extract energy/angle distrib.
  600 LE=LBE
      LG=LE+NEN+2
      LA=LG+NEN+2
      LX=MXR-LA
      IF(LA.GT.MXR) STOP 'EMPEND ERROR - MXR limit exceeded'
      MT6=IWO(LBI+JT6)
      IF(MT6.EQ.2) GO TO 620
      REWIND LIN
      CALL REAMF6(LIN,LTT,LER,EIN,RWO(LXS),NEN,RWO(LE),RWO(LG),RWO(LA)
     1           ,IWO(MTH),MT6,QQM,QQI,AWR,ELO,NXS,NK,LCT,MXE,LX
     2           ,IPRNT,EI1,EI2,EO1,EO2,NZA1,NZA2)
      IF(NK.LE.0) GO TO 800
C* Write the ENDF file-6 data
      CALL WRIMF6(LOU,RWO(LA),MT6,MAT,IZA,AWR,NK,LCT,NS)
      WRITE(LTT,995) ' Processed energ./ang. distrib. for MT: ',MT6
      WRITE(LTT,995) '         Number of outgoing particles : ',NK
      WRITE(LTT,991)
      WRITE(LER,995) ' Processed energ./ang. distrib. for MT: ',MT6
      WRITE(LER,995) '         Number of outgoing particles : ',NK
      WRITE(LER,991)
  620 JT6=JT6+1
      IF(JT6.LT.NT6) GO TO 600
C* Energy/angle distribution data processed
  800 IF(JT6.GT.0)
     1CALL WRCONT(LOU,MAT, 0, 0,NS,ZRO,ZRO, 0, 0, 0, 0)
C* Process discrete level photon production
C*   MXLI - maximum number of discrete levels
C*   MXLJ - maximum number of transitions from a level
      MXLI=100
      MXLJ=20
      JPP=0
      LNB=LBI
      LLB=LNB+MXLI
      IF(LLB+MXLI*MXLJ.GT.MXI)
     1STOP 'EMPEND ERROR - MXI limit exceeded'
      LEL=LBE
      LBR=LEL+MXLI
      LSC=LBR+MXLI*MXLJ
      IF(LSC+MXLI*2.GT.MXR)
     1STOP 'EMPEND ERROR - MXR limit exceeded'
      DO 890 I=1,NXS
      IF     (IWO(MTH-1+I).EQ. 51) THEN
        JZA=IZA
        MT =50
        MT0=MT
      ELSE IF(IWO(MTH-1+I).EQ.600) THEN
        JZA=IZA-1000
        MT =600
        MT0=MT
      ELSE IF(IWO(MTH-1+I).EQ.800) THEN
        JZA=IZA-2003
        MT =800
        MT0=MT
      ELSE
        GO TO 890
      END IF
      REWIND LIN
      CALL REMF12(LIN,LTT,LER,JZA,NLV,RWO(LEL),IWO(LNB),IWO(LLB)
     1           ,RWO(LBR),MXLI,MXLJ)
      CALL WRMF12(LOU,MAT,MT0,IZA,AWR,NLV,RWO(LEL),IWO(LNB),IWO(LLB)
     1           ,RWO(LBR),RWO(LSC),MXLI,MXLJ,NS)
      WRITE(LTT,995) ' Processed discrete level photon prod.: ',MT
      WRITE(LTT,995) '                     Number of levels : ',NLV
      WRITE(LTT,991)
      WRITE(LER,995) ' Processed discrete level photon prod.: ',MT
      WRITE(LER,995) '                     Number of levels : ',NLV
      WRITE(LER,991)
      IF(NLV.GT.0) JPP=JPP+1
  890 CONTINUE
C* Photon production data from discrete levels processed
      IF(JPP.GT.0)
     1CALL WRCONT(LOU,MAT, 0, 0,NS,ZRO,ZRO, 0, 0, 0, 0)
C* Process photon angular distribution for discrete levels (isotropic)
      JPP  =0
      LV50 =0
      LV600=0
      LV800=0
      DO 892 I=1,NXS
      IF     (IWO(MTH-1+I).GE. 51 .AND. IWO(MTH-1+I).LT. 91) THEN
C* Count levels of the MT50 series
        LV50=LV50+1
      ELSE IF(IWO(MTH-1+I).GE.601 .AND. IWO(MTH-1+I).LT.649) THEN
C* Count levels of the MT800 series
        LV600=LV600+1
      ELSE IF(IWO(MTH-1+I).GE.801 .AND. IWO(MTH-1+I).LT.849) THEN
C* Count levels of the MT800 series
        LV800=LV800+1
      ELSE
        GO TO 892
      END IF
  892 CONTINUE
      IF(LV50 .GT.0) THEN
C* Write photon distributions for MT50 series
        JPP=JPP+1
        MT0=50
        CALL WRMF14(LOU,MAT,MT0,IZA,AWR,LV50 ,NS)
      END IF
      IF(LV600.GT.0) THEN
C* Write photon distributions for MT600 series
        JPP=JPP+1
        MT0=600
        CALL WRMF14(LOU,MAT,MT0,IZA,AWR,LV600,NS)
      END IF
      IF(LV800.GT.0) THEN
C* Write photon distributions for MT800 series
        JPP=JPP+1
        MT0=800
        CALL WRMF14(LOU,MAT,MT0,IZA,AWR,LV800,NS)
      END IF
C* Photon angular distributions from discrete levels processed
      IF(JPP.GT.0)
     1CALL WRCONT(LOU,MAT, 0, 0,NS,ZRO,ZRO, 0, 0, 0, 0)
C* Write the ENDF material and file SEND records
      CALL WRCONT(LOU,  0, 0, 0,NS,ZRO,ZRO, 0, 0, 0, 0)
C*
  880 CALL WRCONT(LOU, -1, 0, 0,NS,ZRO,ZRO, 0, 0, 0, 0)
      STOP 'EMPEND Completed'
C*
  991 FORMAT(2A40)
  992 FORMAT(BN,I10)
  994 FORMAT(BN,F10.0)
  995 FORMAT(A40,4I5)
  996 FORMAT(A40,F10.4)
      END
      SUBROUTINE EMTIZA(IZA,JZA,MT)
C-Title  : Subroutine EMTIZA
C-Purpose: Assign MT number from target and residual ZA
      MT =0
      IF     (JZA  .EQ. IZA+   1) THEN
C* Radiative capture cross section
        MT =102
      ELSE IF(JZA  .EQ. IZA     ) THEN
C* Discrete levels inelastic scattering cross section
        MT =50
      ELSE IF(JZA  .EQ. IZA-1000) THEN
C* Discrete levels (n,p) cross section
        MT =600
      ELSE IF(JZA  .EQ. IZA-2003) THEN
C* Discrete levels (n,a) cross section
        MT =800
      ELSE IF(JZA  .EQ. IZA-1   ) THEN
C* (n,2n) cross section
        MT =   16
      ELSE IF(JZA  .EQ. IZA-2   ) THEN
C* (n,3n) cross section
        MT =   17
      ELSE IF(JZA  .EQ. IZA-2004) THEN
C* (n,na) cross section
        MT =   22
      ELSE IF(JZA  .EQ. IZA-2005) THEN
C* (n,2na) cross section
        MT =   24
      ELSE IF(JZA  .EQ. IZA-2006) THEN
C* (n,3na) cross section
        MT =   25
      ELSE IF(JZA  .EQ. IZA-1001) THEN
C* (n,np) cross section
        MT =   28
      ELSE IF(JZA  .EQ. IZA-1002) THEN
C* (n,2np) cross section
        MT =   41
      ELSE IF(JZA  .EQ. IZA-1003) THEN
C* (n,3np) cross section
        MT =   42
      ELSE IF(JZA  .EQ. IZA-3005) THEN
C* (n,npa) cross section
        MT =   45
      ELSE IF(JZA  .EQ. IZA-1001) THEN
C* (n,a) cross section
        MT =  107
      ELSE IF(JZA  .EQ. IZA-4007) THEN
C* (n,2a) cross section
        MT =  108
      ELSE IF(JZA  .EQ. IZA-1999) THEN
C* (n,2p) cross section
        MT =  111
      ELSE IF(JZA  .EQ. IZA-3004) THEN
C* (n,pa) cross section
        MT =  112
      END IF
      RETURN
      END
      SUBROUTINE EMTCHR(PTST,MT)
C-Title  : Subroutine EMTCHR
C-Purpose: Assign MT number from reaction string
      CHARACTER*8 PTST
      MT=0
      IF(PTST.EQ.' (n,x)  ') MT=  5
      IF(PTST.EQ.' (n,n)  ') MT= 91
      IF(PTST.EQ.' (n,2n) ') MT= 16
      IF(PTST.EQ.' (n,3n) ') MT= 17
      IF(PTST.EQ.' (n,np) ') MT= 28
      IF(PTST.EQ.' (n,na) ') MT= 22
      IF(PTST.EQ.' (n,gamm') MT= 102
      IF(PTST.EQ.' (n,p)  ') MT=649
      IF(PTST.EQ.' (n,a)  ') MT=849
      RETURN
      END
      SUBROUTINE POUCHR(PTST,KZAK,AWP)
C-Title  : Subroutine POUCHR
C-Purpose: Assign outgoing particle ZAP from character string
C-Description:
C-D  The input string PTST identifies the outgoing particle.
C-D  On output KZAK is the particle ZA designation and AWP is its
C-D  atomic weight ratio relative to the projectile.
C-D  Special cases:
C-D    KZAP=999999  for recoils (data must be define externally)
C-D    KZAP<0       for unrecognised particles.
C-
      CHARACTER*8  PTST
C*
      IF      (PTST.EQ.'neutrons') THEN
        KZAK=1
        AWP =1.
      ELSE IF (PTST.EQ.'gammas  ') THEN
        KZAK=0
        AWP =0.
      ELSE IF (PTST.EQ.'protons ') THEN
        KZAK=1001
        AWP =0.99862
      ELSE IF (PTST.EQ.'alphas  ') THEN
        KZAK=2004
        AWP =3.96713
      ELSE IF (PTST.EQ.'recoils ') THEN
        KZAK=999999
        AWP =999999
      ELSE
C* Unidentified outgoing particle
        KZAK=-1
        AWP =-1
      END  IF
      RETURN
      END
      SUBROUTINE YLDPOU(YI,MT,KZAP)
C-Title  : Subroutine YLDPOU
C-Purpose: Define yield YI of particla KZAP in reaction MT
      YI=0
      IF     (KZAP.EQ.   1) THEN
C* Outgoing neutrons
        IF(MT.EQ.22 .OR. MT.EQ.23 .OR. MT.EQ.28 .OR.
     1    (MT.GE.32.AND.MT.LE.36) .OR. MT.EQ.45 .OR. MT.EQ.91) YI=1
        IF(MT.EQ.11 .OR. MT.EQ.16 .OR. MT.EQ.24 .OR. MT.EQ.41) YI=2
        IF(MT.EQ.17 .OR. MT.EQ.25 .OR. MT.EQ.42) YI=3
      ELSE IF(KZAP.EQ.1001) THEN
C* Outgoing protons
        IF(MT.EQ.28 .OR. MT.EQ.103) YI=1
        IF(MT.EQ.44) YI=2
      ELSE IF(KZAP.EQ.1002) THEN
C* Outgoing deuterons
        IF(MT.EQ.11 .OR. MT.EQ.32 .OR. MT.EQ.35 .OR. MT.EQ.104) YI=1
      ELSE IF(KZAP.EQ.2003) THEN
C* Outgoing He-3
        IF(MT.EQ.34 .OR. MT.EQ.106) YI=1
      ELSE IF(KZAP.EQ.2004) THEN
C* Outgoing alphas
        IF(MT.EQ.22 .OR. MT.EQ.24 .OR. MT.EQ.25 .OR. MT.EQ.45 .OR.
     1     MT.EQ.107) YI=1
        IF(MT.EQ.29 .OR. MT.EQ.30 .OR. MT.EQ.35 .OR. MT.EQ.36 .OR.
     1     MT.EQ.108) YI=2
      ELSE
C* Recoils
        YI=1
      END IF
      RETURN
      END
      SUBROUTINE SUMMF5(NXS,NPT,MTH,XSR,MXE,MXT)
C-Title  : Subroutine SUMMF5
C-Purpose: Sum reactions contributing to MT 5
      DIMENSION  MTH(MXT),XSR(MXE,MXT)
C* Check if MT 5 is defined
      DO 20 IX=1,NXS
      I5=IX
      IF(MTH(IX).EQ.5) GO TO 30
   20 CONTINUE
      RETURN
C* Clear the MT 5 cross section array
   30 DO 40 J=1,NPT
      XSR(J,I5)=0
   40 CONTINUE
C* Add contributing x-sect identified by MT<0
      DO 60 IX=1,NXS
      IF(MTH(IX).GE.0) GO TO 60
C* Loop over all energy points
      DO 50 J=1,NPT
      XSR(J,I5)=XSR(J,I5)+XSR(J,IX)
   50 CONTINUE
   60 CONTINUE
      RETURN
      END
      SUBROUTINE SCNMF6(LIN,NT6,MTH,MXI)
C-Title  : SCNMF6 Subroutine
C-Purpose: Scan EMPIRE output for all react. with energy/angle distrib.
      CHARACTER*136 REC
      DIMENSION MTH(1)
C*
      NT6=0
  110 READ (LIN,891,END=200) REC
      IF(REC(1:14).EQ.'  Elastic angu'    ) THEN
        MT=2
        GO TO 112
      END IF
      IF(REC(1:14).NE.'  Spectrum of '    ) GO TO 110
C* Identify the reaction and assign the MT number
      CALL EMTCHR(REC(23:30),MT)
      IF(MT .EQ.0) GO TO 110
  112 IF(NT6.GT.0) THEN
C* Check if already processed
        DO 120 I=1,NT6
        IF(MTH(I).EQ.MT) GO TO 110
  120   CONTINUE
      END IF
      NT6=NT6+1
      IF(NT6.GT.MXI) STOP 'SCNMF6 ERROR - MXI limit exceeded'
      MTH(NT6)=MT
      GO TO 110
C* Sort in ascending order
  200 IF(NT6.LT.2) RETURN
  210 ISW=0
      DO 220 I=2,NT6
      IF(MTH(I-1).LT.MTH(I)) GO TO 220
      MM=MTH(I)
      MTH(I  )=MTH(I-1)
      MTH(I-1)=MM
      ISW=1
  220 CONTINUE
      IF(ISW.NE.1) RETURN
      GO TO 210
  891 FORMAT(A136)
      END

      SUBROUTINE RDANGD(LIN,LOR,LHI,NPT,RWO,MXR)
C-Title  : Subroutine RDANGD
C-Purpose: Read angular distributions
C-Description:
C-D  LIN  Logical unit number of the EMPIRE short output.
C-D  LOR  Maximum allowed Legendre order.
C-D  LHI  Maximum fitted Legendre order (output).
C-D  RWO  Work array, which contains on exit a packed matrix RWO(LHI+2,NPT)
C-D       Each of the NPT rows contains the outgoing particle energy
C-D       and LHI+1 Legendre coefficients of the angular distribution
C-D       expansion.
C-D  MXR  Maximum size of the work array RWO.
C-
C* Maximum number of angles
      PARAMETER     (MXA=80)
      CHARACTER*136  REC
      DIMENSION      ANG(MXA),DST(MXA)
      DIMENSION      RWO(MXR)
C* Permissible tolerance for fitted angular distributions (fraction)
      DATA ETOL/ 0.010 /
C*
      DATA PI/3.1415926/
C*
      NPT=0
      LHI=0
      KXA=1
      JXA=1
C* Check if angles are given (no angles for isotropic distributions)
      READ (LIN,891) REC
      IF(REC(24:38).EQ.'               ') GO TO 438
C* Read the angles at which the distributions are given (8 per row)
        KXA=8
        J1 =1
  432   READ (REC,806) (ANG(J),J=J1,KXA)
  433   IF(ANG(KXA).EQ.0) THEN
          KXA=KXA-1
          GO TO 433
        ELSE IF(ANG(KXA).LT.180.) THEN
          READ (LIN,891) REC
          KXA=KXA+8
          IF(KXA.GT.MXA) STOP 'REAMF6 ERROR - MXA limit exceeded'
          J1 =J1 +8
          GO TO 432
        END IF
        JXA=KXA
C* Convert angles from degrees to cosines
        DO 436 J=1,KXA
          ANG(J)=COS(PI*ANG(J)/180.)
  436   CONTINUE
C*
  438 EOU=0.
      LL =1
C* Read angular distributions until a blank line is encountered
  450 READ (LIN,891) REC
      IF(REC(1:20).EQ.'                    ') GO TO 700
      READ (REC,807)          EOU,(DST(J),J=1,8)
      IF(JXA.GT.8) READ (LIN,809) (DST(J),J=9,JXA)
C* Suppress negative energies (if present)
      IF(EOU.LT.0) GO TO 450
      EOU=EOU*1.E6
      NPT=NPT+1
C* Convert to Legendre polynomials and store
      LPU=LL+1
      LS =LPU+LOR+2
      IF(LS.GT.MXR) STOP 'EMPEND ERROR - MXR limit exceeded in RDANG'
      LC =MXR-LS
      LOO=LOR
      CALL LSQLGV(ANG,DST,KXA,RWO(LPU),0,LOO,ETOL,ERR,RWO(LS),LC)
      LHI=MAX(LHI,LOO)
      RWO(LL)=EOU
c      JPRNT=0
cC*
cC* Check for printout on exceeding tolerance limits
c      IF(ERR.GT.5.*ETOL .OR. ERR.LT.0) THEN
c        WRITE(LTT,907) MT,IFIX(ZAP+0.1),EE,EOU,ERR*100.
c        WRITE(LER,907) MT,IFIX(ZAP+0.1),EE,EOU,ERR*100.
c        IF(IPRNT.EQ.0) JPRNT=1
cC* Check for specific reaction printout
c      ELSE IF
c     1  (IPRNT.EQ.MT                    .AND.
c     2  (  EE .GE.EI1 .AND. EE .LE.EI2) .AND.
c     3  (  EOU.GE.EO1 .AND. EOU.LE.EO2)) THEN
c        JPRNT=1
c      END IF
cC* Check for differences in the fitted angular distributions
cC* Fine angular mesh includes midpoints and endpoints at +/- 1
c      KZXA=2*KXA+1
c      ZANG(1)= 1.
c      IF(KXA*2.GT.MXZ) STOP 'EMPEND ERROR - MXZ limit exceeded'
c      DO 452 I=1,KXA
c      IF(I.GT.1)
c     1  ZANG(2*I-1)=0.5*(ANG(I)+ANG(I-1))
c        ZANG(2*I  )=     ANG(I)
c        ZLEG(2*I-1)=POLLG1(ZANG(2*I-1),RWO(LPU),LOO)
c        ZLEG(2*I  )=POLLG1(ZANG(2*I  ),RWO(LPU),LOO)
c        EL=ABS(DST(I)-ZLEG(2*I))
c        IF(IPRNT.GE.0 .AND. (EL .GT. 40.*ETOL*DST(I))) JPRNT=1
c  452 CONTINUE
c      ZANG(KZXA)=-1.
c      ZLEG(KZXA)=POLLG1(ZANG(KZXA),RWO(LPU),LOO)

C* Required coefficients are defined without the (2*l+1) term and
C* yielding "one" after integration over (-1,1).
C* For (l > 0) divide by (2*l+1) to conform with ENDF rules.
C* Calculate values at endpoints PA(+1), PB(-1).
      PA=RWO(LPU)
      PB=PA
      PP=PA
      SS=1.
      DO 453 L=1,LOR
      IF(L.GT.LOO) RWO(LPU+L)=0.
      SS=-SS
      PA= PA+RWO(LPU+L)
      PB= PB+RWO(LPU+L)*SS
      RWO(LPU+L)=RWO(LPU+L)/FLOAT(2*L+1)
  453 CONTINUE

cC* Check for negative ( < -EZERO ) values at endpoints PA(+1), PB(-1).
c      EZERO=1.E-10
c      IF(PA.LT.-EZERO .OR. PB.LT.-EZERO) THEN
c        IF(IPRNT.EQ.0) JPRNT=1
c        WRITE(LTT,906) MT,IFIX(ZAP+0.1),EE,EOU
c        WRITE(LER,906) MT,IFIX(ZAP+0.1),EE,EOU
c      END IF
cC* Check for isotropic distributions (suppress printout)
c      IF(LOR.LT.1) JPRNT=0
cC* Execute test printout
c      IF(JPRNT.NE.0) THEN
cC*      Fitted values to the "curves" file
c        WRITE(LCU,931) LOO
c        DO 454 I=1,KZXA
c        WRITE(LCU,934) ZANG(I),ZLEG(I)
c  454   CONTINUE
c        WRITE(LCU,934)
cC*      Original values to the "points" file
c        WRITE(LPT,932) EE,EOU,MT,IFIX(ZAP+0.1)
c        DO 455 I=1,KXA
c        WRITE(LPT,934) ANG(I),0.,0.,DST(I)
c  455   CONTINUE
c        WRITE(LPT,934)
c      END IF

C*
  700 IF(NPT.LE.1) GO TO 800
      LH2=LHI+2
C* Pack the data into compact matrix RWO(LH2,NPT)
      II =LHI
      JJ =LOR
      DO 720 I=2,NPT
        DO 710 J=1,LH2
          RWO(II+J)=RWO(JJ+J)
  710   CONTINUE
        II =II+LHI
        JJ =JJ+LOR
  720 CONTINUE
C*
  800 RETURN
C*
  806 FORMAT(6X,8(5X,F10.4))
  807 FORMAT(BN,F10.5,F14.4,7F15.4)
  809 FORMAT(9X,8F15.4)
  891 FORMAT(A136)
      END
      SUBROUTINE REAMF3(LIN,LTT,LER,MXE,MXT,MXM
     1                 ,EIN,XSC,QQM,QQI,MTH,IZB,BEN
     1                 ,IZA,AWR,NEN,NXS)
C-Title  : REAMF3 Subroutine
C-Purpose: Read EMPIRE output to be converted into ENDF format
C-Description:
C-D  MTH  Array contains MT numbers of identified reactions. The MT
C-D       numbers of reactions contributing to MT 5 in the high-energy
C-D       type of file is flagged by adding MT+1000.
C-D  NEN  Counts the Number of energy points
C-D  NXS  Counts the Number of reaction types
C-
      CHARACTER*2  CH
      CHARACTER*30 CHEN
      CHARACTER*80 REC,COM
      DIMENSION    EIN(MXE),XSC(MXE,MXT),QQM(MXT),QQI(MXT)
     1            ,MTH(MXT),IZB(MXM),BEN(3,MXM)
C* Neutron mass (ENDF-6 Formats Manual App. H.4, april 2001)
      AWN = 1.008664916
C* Search for the reaction header cards
C*   NEN counts the Number of energy points
C*   NXS counts the Number of reaction types
      NXS=0
      NEN=0
C*
C* Search EMPIRE output for specific strings
  110 READ (LIN,891,END=700) REC
      IF(REC(1:10).EQ.' REACTION '                  ) GO TO 200
      IF(REC(1:18).EQ.'  Decaying nucleus'          ) GO TO 210
      IF(REC(1:28).EQ.'  Spectrum of recoils  (n,x)') GO TO 220
      IF(REC(1:10).EQ.' TOTAL  CR'                  ) GO TO 290
      GO TO 110
C* Identify projectile, target and energy
  200 READ (REC(11:20),802) KZ,CH,KA
      IF(KZ.NE.0 .AND. KA.NE.1) STOP 'EMPEND ERROR - Invalid projectile'
      READ (REC(24:33),802) IZ,CH,IA
      IZA=IZ*1000+IA
      AWR=IA
C* Read and check the energy
  201 READ (REC(51:60),994) EE
      EE = EE*1.E6
      IF(NEN.LE.0 .OR. EE.GT.EIN(NEN)) GO TO 206
C* Skip double energy points
      WRITE(LTT,902) ' EMPEND WARNING - Double energy point eV',EE
  202 READ (LIN,891,END=700) REC
      IF(REC(1:10).NE.' REACTION '        ) GO TO 202
      GO TO 201
C*
  206 NEN=NEN+1
      IF(NEN.GT.MXE) STOP 'EMPEND ERROR - MXE limit exceeded'
      EIN(NEN)=EE
      CHEN=REC(51:80)
C* Read the binding energies of the last nucleon for all residuals
  207 READ (LIN,891) REC
      IF(REC(1:10).NE.'    Nucleu') GO TO 207
      READ (LIN,891)
      IMT=0
  208 IMT=IMT+1
      IF(IMT.GT.MXM) STOP 'EMPEND ERROR - MXM limit exceeded'
      READ (LIN,804) JZ,CH,JA,BEN(1,IMT),BEN(2,IMT),BEN(3,IMT)
      JZA=JZ*1000+JA
      IZB(IMT)=JZA
      IF(JZA.NE.0) GO TO 208
      IMT=IMT-1
      GO TO 110
C*
C* Next product nucleus data
  210 READ (REC(20:29),802) JZ,CH,JA
      JZA=JZ*1000+JA
      IF(JZA  .EQ. IZA  ) THEN
        READ (REC(37:46),994) AWR
        AWR=AWR/AWN
      END IF
      CALL EMTIZA(IZA,JZA,MT)
      IF(MT.EQ.0) THEN
C* For unidentified products with non-zero x-sect. print warning
  212   READ (LIN,891) REC
        IF(REC(13:22).NE.'production') GO TO 212
        READ (REC,803) XS
        IF(XS.GT.0) THEN
          COM=' WARNING - Skip'//REC(2:22)//REC(38:52)//' at'//CHEN
          WRITE(LTT,891) COM
          WRITE(LER,891) COM
        END IF
        GO TO 110
      END IF
C* Test for radiative capture cross section
      IF(MT.EQ.102) GO TO 300
C* Test for discrete levels inelastic scattering cross section
      IF(MT.EQ. 50) THEN
        MT0=50
        GO TO 350
      END IF
C* Test for discrete levels (n,p) and (n,a) cross sections
      IF(MT.EQ.600 .OR. MT.EQ.800) THEN
        MT0=MT
        GO TO 350
      END IF
C* All other cross sections are processed in the same way
      GO TO 310
C* High energy format MT5 - identify contributing reaction
  220 READ (REC(35:40),995) JZA
      CALL EMTIZA(IZA,JZA,MTK)
      IF(MTK.LE.0) THEN
        WRITE(LTT,904) ' EMPEND WARNING - Processing target ZA  ',IZA
        WRITE(LTT,902) '        Could not define MT for residue ',JZA
        WRITE(LER,904) ' EMPEND WARNING - Processing target ZA  ',IZA
        WRITE(LER,902) '        Could not define MT for residue ',JZA
        GO TO 110
      END IF
C* Exclude MT 600, 800 and redefine 50 to 91
      IF(MTK.EQ.600 .OR. MTK.EQ.800) GO TO 110
      IF(MTK.EQ.50) MTK=91
      M5=0
      DO 222 I=1,NXS
      MTI=MTH(I)

      IF(MTI.EQ.MTK)
     1   write(ler,*)' Added to MT 5: MT,IZA,JZA',MTK,IZA,JZA

      IF(MTI.EQ.MTK) MTH(I)=-MTI
      IF(MTI.EQ. 5 ) M5=1
  222 CONTINUE
      IF(M5.EQ.0) THEN
        NXS=NXS+1
        MTH(NXS)=5
        QQM(NXS)=0
        QQI(NXS)=0
      END IF
      GO TO 110
C*
C* Read the total cross section
  290 CONTINUE
      READ (REC,808) XS
      MT=1
      QI=0
      QM=0
      GO TO 392
C*
C* Read the radiative capture cross section
  300 READ (LIN,891) REC
      IF(REC(13:22).NE.'production') GO TO 300
      READ (REC,803) XS
      IF(XS.LE.0) GO TO 110
      IF(NXS.LE.0) GO TO 304
      DO 303 I=1,NXS
      IXS=I
      MTI=ABS(MTH(I))
      IF(MTI.EQ.MT) GO TO 306
  303 CONTINUE
  304 NXS=NXS+1
      IF(NXS.GT.MXT) STOP 'EMPEND ERROR - MXT limit exceeded'
      IXS=NXS
      MTH(IXS)=MT
      DO 305 I=1,IMT
      IF(IZB(I).EQ.IZA+1) QQM(IXS)=1.E6*BEN(1,I)
  305 CONTINUE
      QQI(IXS)=QQM(IXS)
  306 XSC(NEN,IXS)=XS*1.E-3
      GO TO 110
C*
C* Read multiple neutron (and particle) emission cross sections
  310 READ (LIN,891) REC
      IF(REC(13:22).NE.'production') GO TO 310
  311 READ (REC,803) XS
      IF(XS.LE.0) GO TO 110
C* Test if reaction is already registered
      IF(NXS.LE.0) GO TO 314
      DO 313 I=1,NXS
      IXS=I
      MTI=ABS(MTH(I))
      IF(MTI.EQ.MT) GO TO 320
  313 CONTINUE
  314 NXS=NXS+1
      IF(NXS.GT.MXT) STOP 'EMPEND ERROR - MXT limit exceeded'
      IXS=NXS
      MTH(IXS)=MT
C*
C* Reconstruct Q values from MT and the binding energies
      QQM(IXS)=0.
      DO 318 I=1,IMT
      KZA=IZB(I)
C* Consider reactions after first neutron emission:
      IF(KZA.EQ.IZA) THEN
C*      Neutron emission: (n,2n) (n,3n) (n,2n+a) (n,2n+p)
        IF(MT.EQ.16 .OR. MT.EQ.17 .OR. MT.EQ.24 .OR. MT.EQ.41)
     1    QQM(IXS)=QQM(IXS)-1.E6*BEN(1,I)
C*      Proton emission (n,n+p) (n,n+p+a)
        IF(MT.EQ.28 .OR. MT.EQ.45)
     1    QQM(IXS)=QQM(IXS)-1.E6*BEN(2,I)
C*      Alpha emission (n,n+a)
        IF(MT.EQ.22)
     1    QQM(IXS)=QQM(IXS)-1.E6*BEN(3,I)
C* Consider reactions after two-neutrons emission:
      ELSE IF(KZA.EQ.IZA-1) THEN
C*      Neutron emission: (n,3n)
        IF(MT.EQ.17)
     1    QQM(IXS)=QQM(IXS)-1.E6*BEN(1,I)
C*      Proton emission (n,2n+p)
        IF(MT.EQ.41)
     1    QQM(IXS)=QQM(IXS)-1.E6*BEN(2,I)
C*      Alpha emission (n,2n+a)
        IF(MT.EQ.24)
     1    QQM(IXS)=QQM(IXS)-1.E6*BEN(3,I)
C* Consider reactions after three-neutrons emission:
      ELSE IF(KZA.EQ.IZA-2) THEN
C*      Neutron emission: (n,4n)
        IF(MT.EQ.37)
     1    QQM(IXS)=QQM(IXS)-1.E6*BEN(1,I)
C*      Proton emission: (n,3n+p)
        IF(MT.EQ.42)
     1    QQM(IXS)=QQM(IXS)-1.E6*BEN(2,I)
C*      Alpha emission: (n,3n+a)
        IF(MT.EQ.23)
     1    QQM(IXS)=QQM(IXS)-1.E6*BEN(3,I)
C* Consider reactions after neutron and proton emission:
      ELSE IF(KZA.EQ.IZA-1001) THEN
C*      Alpha emission: (n,n+p+a)
        IF(MT.EQ.45)
     1    QQM(IXS)=QQM(IXS)-1.E6*BEN(3,I)
C* Consider reactions after neutron capture:
      ELSE IF(KZA.EQ.IZA+1) THEN
C*      Proton emission (n,p) (n,p+a)
        IF(MT.EQ.103 .OR. MT.EQ.112)
     1    QQM(IXS)=QQM(IXS)-1.E6*BEN(2,I)
C* Consider reactions after neutron capture and proton emission:
      ELSE IF(KZA.EQ.IZA+1-1001) THEN
C*      Alpha emission (n,p+a)
        IF(MT.EQ.112)
     1    QQM(IXS)=QQM(IXS)+1.E6*(BEN(1,I)-BEN(3,I))
      END IF
  318 CONTINUE
      QQI(IXS)=QQM(IXS)
  320 XSC(NEN,IXS)=XS*1.E-3
      GO TO 110
C*
C* Process discrete levels - (n,n'), (n,p), (n,a)
  350 READ (LIN,891)
      READ (LIN,891) REC
      IF(REC(13:22).EQ.'production') THEN
C* Special case when no discrete levels are given
        IF(MT0.EQ.50 ) MT=  4
        IF(MT0.EQ.600) MT=103
        IF(MT0.EQ.800) MT=107
        GO TO 311
      END IF
C* Next could be elastic, discrete level or continuum cross section
  351 READ (LIN,891) REC
      IF(REC( 1:10).EQ.' ELASTIC C') GO TO 370
      IF(REC(13:22).EQ.'production') GO TO 390
      IF(REC(11:30).NE.'Discrete level popul') GO TO 351
C* Positioned to read discrete levels
      READ (LIN,891)
      READ (LIN,891)
      READ (LIN,891)
      XI=0.
  352 READ (LIN,805) IL,EL,XS
      XI=XI+XS
      MT=MT0-1+IL
      IF(MT.EQ.50) GO TO 352
      IF(IL.LE.0 ) GO TO 351
C     IF(XS.LE.0 ) GO TO 352
      IF(NXS.LE.0) GO TO 354
      DO 353 I=1,NXS
      IXS=I
      MTI=ABS(MTH(I))
      IF(MTI.EQ.MT) GO TO 360
  353 CONTINUE
  354 NXS=NXS+1
      IF(NXS.GT.MXT) STOP 'EMPEND ERROR - MXT limit exceeded'
      IXS=NXS
      MTH(IXS)=MT
C* Reconstruct Q values from MT and the binding energies
      QQM(IXS)=0.
      DO 358 I=1,IMT
      KZA=IZB(I)
      IF(KZA.EQ.IZA+1 .AND.(MT0.EQ.600.OR.MT0.EQ.800) )
     1                                 QQM(IXS)=QQM(IXS)+1.E6*BEN(1,I)
      IF(KZA.EQ.IZA+1 .AND.(MT0.EQ.600) )
     1                                 QQM(IXS)=QQM(IXS)-1.E6*BEN(2,I)
      IF(KZA.EQ.IZA+1 .AND.(MT0.EQ.800) )
     1                                 QQM(IXS)=QQM(IXS)-1.E6*BEN(3,I)
  358 CONTINUE
  360 QM =QQM(IXS)
      QI =QM-1.E6*EL
      QQI(IXS)=QI
      XSC(NEN,IXS)=XS*1.E-3
      GO TO 352
C* Positioned to read the elastic cross section
  370 MT=2
      READ (REC,808) XE
      DO 373 I=1,NXS
      IXS=I
      MTI=ABS(MTH(I))
      IF(MTI.EQ.MT) GO TO 376
  373 CONTINUE
      NXS=NXS+1
      IF(NXS.GT.MXT) STOP 'EMPEND ERROR - MXT limit exceeded'
      IXS=NXS
      MTH(IXS)=MT
C* Q value is zero for elastic
      QQM(IXS)=0.
      QQI(IXS)=0.
  376 XSC(NEN,IXS)=XE*1.E-3
      GO TO 351
C* Positioned to read continuum cross section (subtract discrete levels)
  390 CONTINUE
      READ (REC,803) XC
      XS=XC-XI
      IF(XC.GT.0 .AND. ABS(XS/XC).LT.2.E-5) XS=0.
C* Skip continuum if its contribution is negligible
      IF(XS.LE.0) GO TO 110
C* Add continuum to the list
      IF(MT0.EQ.50 ) MT=91
      IF(MT0.EQ.600) MT=649
      IF(MT0.EQ.800) MT=849
C* Test if reaction is already registered
  392 IF(NXS.LE.0  ) GO TO 394
      DO 393 I=1,NXS
      IXS=I
      MTI=ABS(MTH(I))
      IF(MTI.EQ.MT) GO TO 396
  393 CONTINUE
  394 NXS=NXS+1
      IF(NXS.GT.MXT) STOP 'EMPEND ERROR - MXT limit exceeded'
      IXS=NXS
C* Reaction QM and QI values from the last discrete level
      MTH(IXS)=MT
      QQM(IXS)=QM
      QQI(IXS)=QI
  396 XSC(NEN,IXS)=XS*1.E-3
      GO TO 110
C* All data read
 700  RETURN
C*
  802 FORMAT(I3,1X,A2,1X,I3)
  803 FORMAT(37X,F12.0)
  804 FORMAT(1X,I3,1X,A2,1X,I3,4X,3F10.0)
  805 FORMAT(I12,F10.0,16X,F12.0)
  806 FORMAT(BN,8X,8F15.0)
  808 FORMAT(24X,F12.0)
  891 FORMAT(A80)
  902 FORMAT(A40,1P,E10.3)
  904 FORMAT(A40,I10)
  994 FORMAT(BN,F10.0)
  995 FORMAT(BN,I6)
      END
      SUBROUTINE REAMF6(LIN,LTT,LER,EIN,XSC,NE3,EIS,GAM,RWO,MTH,MT6
     1                 ,QQM,QQI,AWR,ELO,NXS,NK,LCT,MXE,MXR
     2                 ,IPRNT,EI1,EI2,EO1,EO2,NZA1,NZA2)
C-Title  : REAMF6 Subroutine
C-Purpose: Read EMPIRE output energy/angle distrib. for each MT
C-Version:
C-V  00/03 Define Unit base linear interpolation between incident E.
C-
C* Number of input angles MXA and number of particles per react. MXP
      PARAMETER    (MXA=80, MXP=20, MXZ=160)
      CHARACTER*8   POUT(MXP)
      CHARACTER*40  FLPT,FLCU
      CHARACTER*136 REC
      DIMENSION     EIN(1),XSC(MXE,1),EIS(1),GAM(1),QQM(1),QQI(1)
     1             ,RWO(1),ANG(MXA),MTH(1)
      DIMENSION     IZAK(MXP),ZANG(MXZ),ZLEG(MXZ)
C*
      DATA PI/3.1415926/
C* Permissible tolerance for fitted angular distributions (fraction)
      DATA ETOL/ 0.010 /
C* Test print filenames and logical file units
      DATA FLCU/'angdis.cur'/
     1     FLPT/'angdis.pnt'/
      DATA LCU,LPT/ 31,32/
C* Particle masses [amu] (ENDF-6 Formats Manual App. H.4)
C* Neutron, proton, deuteron, triton, He-2, alpha
      AWN = 1.008664916
      AWH = 1.007276467
      AWD = 2.013553213
      AWT = 3.016049268
      AW3 = 3.014932235
      AWA = 4.001506175
C* Test print files
      IF(IPRNT.GE.0) THEN
        OPEN (UNIT=LCU,FILE=FLCU,STATUS='UNKNOWN')
        OPEN (UNIT=LPT,FILE=FLPT,STATUS='UNKNOWN')
      END IF
C* Search for the reaction header cards
C*   NE6 counts the Number of energy points
      NE6N=0
      LCT=2
      IT =0
      NK =0
      IK =0
      MTX=0
      NP =0
      E0 =-1.
  110 READ (LIN,891,END=700) REC
      IF(REC(1:10).EQ.' REACTION '        ) GO TO 200
      IF(REC(1:14).EQ.'  Elastic angu'    ) GO TO 400
      IF(REC(1:14).EQ.'  Spectrum of '    ) GO TO 600
      GO TO 110
C* Read the energy
  200 READ (REC(51:60),994) EE
      EE=EE*1.E6
      IF(EE.GT.E0) GO TO 110
C* Skip to next energy point
  202 READ (LIN,891,END=700) REC
      IF(REC(1:10).NE.' REACTION '        ) GO TO 202
      E0=-1.
      GO TO 200
C*
C* Read the elastic angular distributions
  400 IF(MT6.GE.0) GO TO 110
      MT =2
      JT6=ABS(MT6)
      IF(MT.NE.JT6) GO TO 110
      IF(IT.GT.0 .AND. MT.GE.MTX) GO TO 420
C* Case: Define new reaction, check if it has been processed
      MTC=MT
      DO 410 I=1,NXS
      IF(MTH(I).NE.MT) GO TO 410
        MTX=MT
        NE6=0
        LBL=1
        IT =I
        IK =1
        NK =1
        GO TO 420
  410 CONTINUE
C*       Reaction not on the list from MF3 reactions - skip the data
      GO TO 110
C* Case: Reaction already defined - check for matching
  420 IF(MTH(IT).NE.MT) GO TO 110
C* Check threshold
      ETH=MAX((-QQI(IT))*(AWR+1)/AWR, ELO )
      IF(EE.LT.ETH) GO TO 110
C* Reaction matched - Define pointwise cross section at the same energy
      XS3=0.
      JE3=0
  425 JE3=JE3+1
      EN3=EIN(JE3)
      XS3=XSC(JE3,IT)
      IF(ABS((EN3-EE)/EE).GT.1E-6 .AND. JE3.LT.NE3) GO TO 425
C* If not defined, Define the general File-6 data (HEAD and TAB1 rec.)
      IF(NE6.GT.0) GO TO 430 
C* Preset the particle multiplicity
      Y  =1.
      ZAP=1.
      AWP=1.
      NP =2
      LIP=0
      LAW=1
      NR =1
      E1 =EE
      Y1 =Y
      E2 =EE
      Y2 =Y
      LEP=2
C     LEP=1
      LANG=1
      NRA=1
C...  INA=2
C...  INA=12
C* Unit base linear interpolation between incident neutron energies
      INA=22
C* Reserve the space in the Real array
      LXA=LBL
      LBL=LXA+12+2*NP
C* Define specific File-6 data for all incident energies
  430 L6 =LBL
      LL =L6 + 4
      NW =0
      NEP=0
C* Read the angles
      READ (LIN,891) REC
  431 READ (LIN,891) REC
C* Define the order LOR of Legendre expansion (=NA in ENDF)
      LOR=0
      LHI=0
      KXA=1
      JXA=1
      IF(REC(24:38).EQ.'               ') GO TO 438
C* Read angles for non-isotropic distributions - Max.Legendre order LOR
        LOR=64
        KXA=8
        J1 =1
C* Read the angles at which the distributions are given (8 per row)
  432   READ (REC,806) (ANG(J),J=J1,KXA)
  433   IF(ANG(KXA).EQ.0) THEN
          KXA=KXA-1
          GO TO 433
        ELSE IF(ANG(KXA).LT.180.) THEN
          READ (LIN,891) REC
          KXA=KXA+8
          IF(KXA.GT.MXA) STOP 'REAMF6 ERROR - MXA limit exceeded'
          J1 =J1 +8
          GO TO 432
        END IF
C* If less than 9 angles, calculate intermediate points by interpolation
        JXA=KXA
        IF(KXA.GT.1 .AND. KXA.LE.8) THEN
C...    IF(KXA.GT.1) THEN
          KXA=JXA*2-1
          ANG(KXA)=ANG(JXA)
          DO 434 J=2,JXA
            ANG(KXA+2-2*J)=ANG(JXA+1-J)
            ANG(KXA+3-2*J)=0.5*(ANG(KXA+2-2*J)+ANG(KXA+4-2*J))
  434       CONTINUE
        END IF
C* Convert angles from degrees to cosines
        DO 436 J=1,KXA
          ANG(J)=COS(PI*ANG(J)/180.)
  436   CONTINUE
C*
  438 LO1=LOR+1
      EOU=0.
C* Read energy\angle distributions
      READ (LIN,891) REC
      LD =LL+LOR+2
      LS =LD+KXA
      LPU=LL+1
      IF(LS.GT.MXR) STOP 'EMPEND ERROR - MXR limit exceeded in REAMF6'
      READ (REC,807) EOU,(RWO(LD-1+J),J=1,8)
      IF(JXA.GT.8)
     1READ (LIN,809)     (RWO(LD-1+J),J=9,JXA)
      IF(JXA.NE.KXA) THEN
C* Interpolate to double the mesh in case less than 9 angles given
          RWO(LD-1+KXA)=RWO(LD-1+JXA)
          DO 441 J=2,JXA
            RWO(LD+KXA+1-2*J)=     RWO(LD+JXA-J)
            RWO(LD+KXA+2-2*J)=0.5*(RWO(LD+KXA+1-2*J)+RWO(LD+KXA+3-2*J))
  441       CONTINUE
      END IF
      EOU=EOU*1.E6
C* Convert to Legendre polynomials and store
      LC =MXR-LS
      LOO=LOR
      CALL LSQLGV(ANG,RWO(LD),KXA,RWO(LPU),0,LOO,ETOL,ERR,RWO(LS),LC)
      LHI=MAX(LHI,LOO)
      JPRNT=0
C* Check for specific reaction printout
      IF(ERR.GT.5.*ETOL .OR. ERR.LT.0) THEN
        WRITE(LTT,907) MT,IFIX(ZAP+0.1),EE,EOU,ERR*100.
        WRITE(LER,907) MT,IFIX(ZAP+0.1),EE,EOU,ERR*100.
        IF(IPRNT.EQ.0) JPRNT=1
C* Check for specific reaction printout
      ELSE IF
     1  (IPRNT.EQ.MT                    .AND.
     2  (  EE .GE.EI1 .AND. EE .LE.EI2) .AND.
     3  (  EOU.GE.EO1 .AND. EOU.LE.EO2)) THEN
        JPRNT=1
      END IF
C* Check for differences in the fitted angular distributions
C* Fine angular mesh includes midpoints and endpoints at +/- 1
      KZXA=2*KXA+1
      ZANG(1)= 1.
      IF(KXA*2.GT.MXZ) STOP 'EMPEND ERROR - MXZ limit exceeded'
      DO 442 I=1,KXA
      IF(I.GT.1)
     1  ZANG(2*I-1)=0.5*(ANG(I)+ANG(I-1))
        ZANG(2*I  )=     ANG(I)
        ZLEG(2*I-1)=POLLG1(ZANG(2*I-1),RWO(LPU),LOO)
        ZLEG(2*I  )=POLLG1(ZANG(2*I  ),RWO(LPU),LOO)
        EL=ABS(RWO(LD-1+I)-ZLEG(2*I))
        IF(IPRNT.GE.0 .AND. (EL .GT. 40.*ETOL*RWO(LD-1+I))) JPRNT=1
  442 CONTINUE
      ZANG(KZXA)=-1.
      ZLEG(KZXA)=POLLG1(ZANG(KZXA),RWO(LPU),LOO)
C* Required coefficients are defined without the (2*l+1) term and
C* yielding "one" after integration over (-1,1).
C* For (l > 0) divide by (2*l+1) to conform with ENDF rules.
C* Calculate values at endpoints PA(+1), PB(-1).
      PA=RWO(LPU)
      PB=PA
      PP=PA
      SS=1.
      DO 443 L=1,LOR
      IF(L.GT.LOO) RWO(LPU+L)=0.
      SS=-SS
      PA= PA+RWO(LPU+L)
      PB= PB+RWO(LPU+L)*SS
      RWO(LPU+L)=RWO(LPU+L)/FLOAT(2*L+1)
  443 CONTINUE
C* Check for negative ( < -EZERO ) values at endpoints PA(+1), PB(-1).
      EZERO=1.E-10
      IF(PA.LT.-EZERO .OR. PB.LT.-EZERO) THEN
        IF(IPRNT.EQ.0) JPRNT=1
        WRITE(LTT,906) MT,IFIX(ZAP+0.1),EE,EOU
        WRITE(LER,906) MT,IFIX(ZAP+0.1),EE,EOU
      END IF
C* Check for isotropic distributions (suppress printout)
      IF(LOR.LT.1) JPRNT=0
C* Execute test printout
      IF(JPRNT.NE.0) THEN
C*      Fitted values to the "curves" file
        WRITE(LCU,931) LOO
        DO 444 I=1,KZXA
        WRITE(LCU,934) ZANG(I),ZLEG(I)
  444   CONTINUE
        WRITE(LCU,934)
C*      Original values to the "points" file
        WRITE(LPT,932) EE,EOU,MT,IFIX(ZAP+0.1)
        DO 445 I=1,KXA
        WRITE(LPT,934) ANG(I),0.,0.,RWO(LD-1+I)
  445   CONTINUE
        WRITE(LPT,934)
      END IF
C* Save the outgoing particle energy
  446 RWO(LL)=EOU
C* Increment indices in the storage array
      LL =LL+LOR+2
      NW =NW+LOR+2
      NEP=NEP+1
      IF(LL+LO1.GT.MXR)
     1STOP 'EMPEND ERROR - MXR limit exceeded in REAMF6'
C* Normalize distributions, pack to max. common Legendre order
      NE6=NE6+1
      LL =L6 + 4
      LL1=LL
      RWO(LL   )=RWO(LL1   )
      DO 462 IA=1,LO1
      RWO(LL+IA)=RWO(LL1+IA)
  462 CONTINUE
      LL1=LL1+LOR+2
      LL =LL +LHI+2
C*
      LBL=LL
      RWO(L6    )=EE
      RWO(L6 + 1)=LHI
      RWO(L6 + 2)=NEP*(LHI+2)
      RWO(L6 + 3)=NEP
C* Check if all reactions are given at each incident energy
      EIS(NE6)=EE
      GO TO 110
C*
C* Read the energy/angle distribution data
  600 IF(MT6.LT.0) THEN
        IF( -MT6.LT.100 .AND. REC(15:22).NE.'neutrons') GO TO 110
        IF((-MT6.GE.600 .AND. -MT6.LE.649) .AND.
     1                        REC(15:22).NE.'protons ') GO TO 110
        IF((-MT6.GE.800 .AND. -MT6.LE.849) .AND.
     1                        REC(15:22).NE.'alphas  ') GO TO 110
      END IF
      MT =0
      E0 =EE
      JT6=ABS(MT6)
C* Identify the reaction and assign the MT number
      KZAK=0
      IF(REC(15:22).EQ.'recoils ') READ (REC(35:58),808) KZAK
      CALL  EMTCHR(REC(23:30),MT)
      IF(MT.EQ. 0 ) GO TO 110
      IF(MT.NE.JT6) GO TO 110
      IF(IT.GT.0 .AND. MT.GE.MTX) GO TO 620
C* Case: Define new reaction, check if it has been processed
      MTC=MT
      IF(MT6.LT.0) THEN
        IF(MT.EQ. 91) MTC= 51
        IF(MT.EQ.649) MTC=600
        IF(MT.EQ.849) MTC=800
      END IF
      DO 608 I=1,NXS
      IF(MTH(I).EQ.MTC) ETH=MAX((-QQI(I))*(AWR+1.)/AWR, ELO )
  608 CONTINUE
      DO 610 I=1,NXS
      IF(MTH(I).NE.MT) GO TO 610
        IF(EE.LT.ETH) GO TO 110
        MTX=MT
        NE6=0
        LBL=1
        IT =I
        IK =1
        NK =1
        POUT(IK)=REC(15:22)
        IZAK(IK)=KZAK
        GO TO 620
  610 CONTINUE
C*       Reaction not on the list from MF3 reactions - skip the data
      GO TO 110
C* Case: Reaction already defined - check for matching
  620 IF(MTH(IT).NE.MT) GO TO 110
      IF(REC(15:22).EQ.POUT(IK) .AND. KZAK.EQ.IZAK(IK)) GO TO 624
      DO 622 JK=1,NK
      IF(REC(15:22).EQ.POUT(JK) .AND. KZAK.EQ.IZAK(JK)) GO TO 110
  622 CONTINUE
      NK=NK+1
      IF(NK.GT.MXP) STOP 'REAMF6 ERROR - MXP limit exceeded'
      POUT(NK)=REC(15:22)
      IZAK(NK)=KZAK
      GO TO 110
C* Reaction matched - check the threshold
  624 IF(EE.LT.ETH) GOTO 202
C* Define the maximum energy of the outgoing particle (available energy)
      EMX=EE*AWR/(AWR+1.)+QQM(IT)
C* Special case for continuum reactions which may have discrete levels:
C*  - Truncate max.energy except if MT6<0(used to define MF4 ang.distr.)
C*  - Gamma particle energy equals total available energy
      IF(MT6.GT.0 .AND. POUT(IK).NE.'gammas  ') THEN
        IF(MT.EQ. 91) EMX=EE*AWR/(AWR+1.)+QQI(IT)
        IF(MT.EQ.649) EMX=EE*AWR/(AWR+1.)+QQI(IT)
        IF(MT.EQ.849) EMX=EE*AWR/(AWR+1.)+QQI(IT)
      END IF
C     IF(EMX.LT.0) GO TO 110
C* Define the pointwise cross section at the same energy
      XS3=0.
      JE3=0
  625 JE3=JE3+1
      EN3=EIN(JE3)
      XS3=XSC(JE3,IT)
      IF(ABS((EN3-EE)/EE).GT.1E-6 .AND. JE3.LT.NE3) GO TO 625
C* If not defined, Define the general File-6 data (HEAD and TAB1 rec.)
  626 IF(NE6.GT.0) GO TO 630 
C* Preset the particle multiplicity
      Y  =1.
      IF      (POUT(IK).EQ.'neutrons') THEN
        ZAP=1.
        AWP=1.
        IF(MT.EQ.16) Y  =2.
        IF(MT.EQ.17) Y  =3.
        NP =2
      ELSE IF (POUT(IK).EQ.'gammas  ') THEN
        ZAP=0.
        AWP=0.
C* If gamma is first particle, assume No.of points for yields=x.s.

        IF(MT.EQ.102) PRINT *,'NP,NE6N',NP,NE6N

        IF(NE6N.GT.0) THEN
          NP =NE6N
        ELSE
          NP =NE3
        END IF
      ELSE IF (POUT(IK).EQ.'protons ') THEN
        ZAP=1001.
        AWP=AWH/AWN
        NP =2
      ELSE IF (POUT(IK).EQ.'alphas  ') THEN
        ZAP=2004.
        AWP=AWA/AWN
        NP =2
      ELSE IF (POUT(IK).EQ.'recoils ') THEN
        READ (REC(35:58),808) IZAP,AWP
        AWP=AWP/AWN
        ZAP=IZAP
        NP =2
      ELSE
C* Unidentified outgoing particle
         NK=NK-1
         IF(IK.GT.NK) GO TO 110
         DO 627 JK=IK,NK
         POUT(JK)=POUT(JK+1)
  627    CONTINUE
         GO TO 110
      END  IF
C*
      LIP=0
      LAW=1
      NR =1
      E1 =EE
      Y1 =Y
      E2 =EE
      Y2 =Y
      LEP=2
C     LEP=1
      LANG=1
      NRA=1
C...  INA=2
C...  INA=12
C* Unit base linear interpolation between incident neutron energies
      INA=22
C* Reserve the space in the Real array
      LXA=LBL
      LBL=LXA+12+2*NP
C* Define specific File-6 data for all incident energies
  630 L6 =LBL
      LL =L6 + 4
      NW =0
      NEP=0
      E1 =MIN(E1,EE)
      E2 =MAX(E2,EE)
C* Read the angles
  631 READ (LIN,891) REC
      IF(REC(1:8).NE.' Energy ') GO TO 631
C* Define the order LOR of Legendre expansion (=NA in ENDF)
      LOR=0
      LHI=0
      KXA=1
      JXA=1
      IF(REC(24:38).EQ.'               ') GO TO 638
C* Read angles for non-isotropic distributions - Max.Legendre order LOR
C...    LOR=12
        LOR=22
        KXA=8
        J1 =1
C* Read the angles at which the distributions are given (8 per row)
  632   READ (REC,806) (ANG(J),J=J1,KXA)
  633   IF(ANG(KXA).EQ.0) THEN
          KXA=KXA-1
          GO TO 633
        ELSE IF(ANG(KXA).LT.180.) THEN
          READ (LIN,891) REC
          KXA=KXA+8
          IF(KXA.GT.MXA) STOP 'REAMF6 ERROR - MXA limit exceeded'
          J1 =J1 +8
          GO TO 632
        END IF
C* If less than 9 angles, calculate intermediate points by interpolation
        JXA=KXA
        IF(KXA.GT.1 .AND. KXA.LE.8) THEN
          KXA=JXA*2-1
          ANG(KXA)=ANG(JXA)
          DO 634 J=2,JXA
            ANG(KXA+2-2*J)=ANG(JXA+1-J)
            ANG(KXA+3-2*J)=0.5*(ANG(KXA+2-2*J)+ANG(KXA+4-2*J))
  634       CONTINUE
        END IF
C* Convert angles from degrees to cosines
        DO 636 J=1,KXA
          ANG(J)=COS(PI*ANG(J)/180.)
  636   CONTINUE
C*
  638 LO1=LOR+1
      EOU=0.
      EOL=0.
      PEU=0.
      PEL=0.
      SPC=0.
      LPU=LL+1
C* Read energy\angle distributions
  640 READ (LIN,891) REC
      IF(REC(1:8).EQ.'        ') THEN
        IF(NEP.LE.0) THEN
          GO TO 640
        ELSE
          GO TO 650
        END IF
      END IF
      LD =LL+LOR+2
      LS =LD+KXA
      IF(LS.GT.MXR) STOP 'EMPEND ERROR - MXR limit exceeded in REAMF6'
      READ (REC,807,ERR=650) ERD,(RWO(LD-1+J),J=1,8)
      IF(JXA.GT.8)
     1READ (LIN,809)     (RWO(LD-1+J),J=9,JXA)
C* Suppress negative outgoing particle energies (if present)
      IF(ERD.LT.0) GO TO 640
C* Interpolate to double the mesh in case less than 9 angles given
      IF(JXA.NE.KXA) THEN
          RWO(LD-1+KXA)=RWO(LD-1+JXA)
          DO 641 J=2,JXA
            RWO(LD+KXA+1-2*J)=     RWO(LD+JXA-J)
            RWO(LD+KXA+2-2*J)=0.5*(RWO(LD+KXA+1-2*J)+RWO(LD+KXA+3-2*J))
  641       CONTINUE
      END IF
C* Convert to Legendre polynomials and store
      LC =MXR-LS
      LOO=LOR
      ELL=EOL
      EOL=EOU
      EOU=ERD*1.E6
      PE3=PEL
      PEL=PEU
      LPL=LPU
      LPU=LL+1
      CALL LSQLGV(ANG,RWO(LD),KXA,RWO(LPU),0,LOO,ETOL,ERR,RWO(LS),LC)
      LHI=MAX(LHI,LOO)
      JPRNT=0
C* Check for specific reaction printout
      IF(ERR.GT.5.*ETOL .OR. ERR.LT.0) THEN
        WRITE(LTT,907) MT,IFIX(ZAP+0.1),EE,EOU,ERR*100.
        WRITE(LER,907) MT,IFIX(ZAP+0.1),EE,EOU,ERR*100.
        IF(IPRNT.EQ.0) JPRNT=1
C* Check for specific reaction printout
      ELSE IF
     1  (IPRNT.EQ.MT                    .AND.
     2  (  EE .GE.EI1 .AND. EE .LE.EI2) .AND.
     3  (  EOU.GE.EO1 .AND. EOU.LE.EO2)) THEN
        JPRNT=1
      END IF
C* Check for differences in the fitted angular distributions
C* Fine angular mesh includes midpoints and endpoints at +/- 1
      KZXA=2*KXA+1
      ZANG(1)= 1.
      DO 642 I=1,KXA
      IF(I.GT.1)
     1  ZANG(2*I-1)=0.5*(ANG(I)+ANG(I-1))
        ZANG(2*I  )=     ANG(I)
        ZLEG(2*I-1)=POLLG1(ZANG(2*I-1),RWO(LPU),LOO)
        ZLEG(2*I  )=POLLG1(ZANG(2*I  ),RWO(LPU),LOO)
        EL=ABS(RWO(LD-1+I)-ZLEG(2*I))
        IF(IPRNT.GE.0 .AND. (EL .GT. 40.*ETOL*RWO(LD-1+I))) JPRNT=1
  642 CONTINUE
      ZANG(KZXA)=-1.
      ZLEG(KZXA)=POLLG1(ZANG(KZXA),RWO(LPU),LOO)
C* Required coefficients are defined without the (2*l+1) term and
C* yielding "one" after integration over (-1,1).
C* For (l > 0) divide by (2*l+1) to conform with ENDF rules.
C* Calculate values at endpoints PA(+1), PB(-1).
      PA=RWO(LPU)
      PB=PA
      PP=PA
      SS=1.
      DO 643 L=1,LOR
      IF(L.GT.LOO) RWO(LPU+L)=0.
      SS=-SS
      PA= PA+RWO(LPU+L)
      PB= PB+RWO(LPU+L)*SS
      RWO(LPU+L)=RWO(LPU+L)/FLOAT(2*L+1)
  643 CONTINUE
C* Check for negative ( < -EZERO ) values at endpoints PA(+1), PB(-1).
      EZERO=1.E-10
      IF(PA.LT.-EZERO .OR. PB.LT.-EZERO) THEN
        IF(IPRNT.EQ.0) JPRNT=1
        WRITE(LTT,906) MT,IFIX(ZAP+0.1),EE,EOU
        WRITE(LER,906) MT,IFIX(ZAP+0.1),EE,EOU
      END IF
C* Check for isotropic distributions (suppress printout)
      IF(LOR.LT.1) JPRNT=0
C* Execute test printout
      IF(JPRNT.NE.0) THEN
C*      Fitted values to the "curves" file
        WRITE(LCU,931) LOR
        DO 644 I=1,KZXA
        WRITE(LCU,934) ZANG(I),ZLEG(I)
  644   CONTINUE
        WRITE(LCU,934)
C*      Original values to the "points" file
        WRITE(LPT,932) EE,EOU,MT,IFIX(ZAP+0.1)
        DO 645 I=1,KXA
        WRITE(LPT,934) ANG(I),0.,0.,RWO(LD-1+I)
  645   CONTINUE
        WRITE(LPT,934)
      END IF
C* Adjust the available outgoing particle energy
C     EMP=EMX
      EMP=EMX* (AWR+1.-AWP)/(AWR+1.)
C* Implement the max.energy loss limit
      IF(EOU.LT.EMP) GO TO 646
      IF(EOU.LE.EOL) GO TO 646
C* Preserve contribution to the integral
      IF(RWO(LPL).GT.0 .AND. RWO(LPU).EQ.0) THEN
C* Case: Define the distribution at EMP by scaling the last
C*       non-zero point such that the integral is preserved
        DP =(EOU-EMP)/(EMP-ELL)
        PP =(1.+DP)
        CALL FLDSCL(LO1,PP,RWO(LPL),RWO(LPL))
        SPC=SPC+0.5*PEL*DP*(EOL-ELL)
        PEL= RWO(LPL)
      ELSE
C* Case: Interpolate between EOU and EOL to EMP and discard
C*       the distribution above EMP (assumed to belong to the
C*       explicitly treated discrete levels)
        CALL FLDINT(LO1,EOL,RWO(LPL),EOU,RWO(LPU),EMP,RWO(LPU))
      END IF
      EOU=EMP
C* Save the outgoing particle energy
  646 RWO(LL)=EOU
      PEU=RWO(LPU)
C* Check for multiple zero distributions
      IF(NEP.LT.2) GO TO 647
      IF(PEU.NE.0 .OR. PEL.NE.0 .OR. PE3.NE.0) GO TO 647
      RWO(LL-LOR-2)=EOU
      IF(EOU.LT.EMP) GO TO 640
      GO TO 648
C* Add contribution to the integral
  647 IF(NEP.GT.0) SPC=SPC+0.5*(PEU+PEL)*(EOU-EOL)
C* Increment indices in the storage array
      LL =LL+LOR+2
      NW =NW+LOR+2
      NEP=NEP+1
      IF(LL+LO1.GT.MXR)
     1STOP 'EMPEND ERROR - MXR limit exceeded in REAMF6'
      IF(EOU.LT.EMP) GO TO 640
C* Check that the last point is zero at EMP
  648 IF(PEU.EQ.0) GO TO 650
      SPC=SPC+0.5*PEU*(EOU-EMP)
C*      Shift energy - double points cause problems in HEATR
      RWO(LL)=EMP*1.00001
      DO 649 L=1,LO1
      RWO(LL+L)=0.
  649 CONTINUE
      LL =LL+LOR+2
      NW =NW+LOR+2
      NEP=NEP+1
C*
C* Insert the incident particle threshold energy if necessary
  650 IF(NE6.GT.0 .OR. EE.LE.ETH) GO TO 660
C     IF(ETH.LT.ELO) GO TO 660
      IF(LL.GT.MXR) STOP 'EMPEND ERROR - MXR limit exceeded in REAMF6'
      NW=LL-L6
      DO 652 J=1,NW
      RWO(LL+8-J)=RWO(LL-J)
  652 CONTINUE
      LL =LL + 8
      RWO(L6    )=ETH
      RWO(L6 + 1)=0
      RWO(L6 + 2)=4
      RWO(L6 + 3)=2
      DE=1.E-5
      IF(LEP.EQ.2) THEN
        PE=2./DE
      ELSE
        PE=1./DE
      END IF
      RWO(L6+4)=0.
      RWO(L6+5)=PE
      RWO(L6+6)=DE
      RWO(L6+7)=0.
      L6 =L6 + 8
      NE6=NE6+1
C* Save the energy and the cross section
      IF(IK.EQ.1) THEN
        EIS(NE6)=ETH
      ELSE
        IF(EIS(NE6).NE.ETH) THEN
           WRITE(LTT,995) ' EMPEND ERROR - Wrong MF6 E-mesh for MT ',MT6
           WRITE(LER,995) ' EMPEND ERROR - Wrong MF6 E-mesh for MT ',MT6
           STOP 'EMPEND ERROR - Inconsistent E-mesh for particles'
        END IF
      END IF
      GAM(NE6)=1.
      E1=ETH
C*
C* Normalize distributions
  660 NE6=NE6+1
      LL =L6 + 4
      LL1=LL
C* Check for consistency (only if MT6>0)
      IF(MT6.GT.0) THEN
        IF(ZAP.EQ.1. .AND.XS3.GT.0 ) THEN
           XSP=SPC*4.E-9*PI/Y
           DFP=100*(XSP-XS3)/XS3
           IF(ABS(DFP).GT.2.) THEN
             WRITE(LTT,909) MT,EE,XS3,DFP
             WRITE(LER,909) MT,EE,XS3,DFP

             write(ltt, * ) 'xsp,spc,y',xsp,spc,y
             write(ler, * ) 'xsp,spc,y',xsp,spc,y

           END IF
        END IF
        IF(SPC.LE.0) THEN
          DE=RWO(LL1+LOR+2)-RWO(LL1)
          IF(DE.LE.0) DE=1.E-5
          IF(LEP.EQ.2) THEN
            PE=2./DE
          ELSE
            PE=1./DE
          END IF
          RWO(LL1      )=0.
          RWO(LL1+1    )=PE
          RWO(LL1+LOR+2)=DE
          RWO(LL1+LOR+3)=0.
          LHI=0
          LO1=1
          NEP=2
          SPC=1
        END IF
      ELSE
        SPC=1
      END IF
C*
C* Pack to max. common Legendre order
      DO 664 IP=1,NEP
        RWO(LL   )=RWO(LL1   )
        DO 662 IA=1,LO1
          RWO(LL+IA)=RWO(LL1+IA)/SPC
  662   CONTINUE
        LL1=LL1+LOR+2
        LL =LL +LHI+2
  664 CONTINUE
C*
      LBL=LL
      RWO(L6    )=EE
      RWO(L6 + 1)=LHI
      RWO(L6 + 2)=NEP*(LHI+2)
      RWO(L6 + 3)=NEP
C
* Check if all reactions are given at each incident energy
      IF(IK.EQ.1) THEN
        EIS(NE6)=EE
      ELSE
        IF(EIS(NE6).NE.EE) THEN
           WRITE(LTT,995) ' EMPEND ERROR - Wrong MF6 E-mesh for MT ',MT6
           WRITE(LER,995) ' EMPEND ERROR - Wrong MF6 E-mesh for MT ',MT6
           STOP 'EMPEND ERROR - Inconsistent E-mesh for particles'
        END IF
      END IF
C* Gamma multiplicity
      IF(ZAP.EQ.0) THEN
        IF(XS3.GT.0) THEN
          GAM(NE6)=SPC*1.E-9/XS3
        ELSE
          GAM(NE6)=1.
        END IF
      END IF
      GO TO 110
C* File read - Pack general reaction data into real array
  700 IF(NK.LE.0) RETURN
      RWO(LXA   )=ZAP
      RWO(LXA+ 1)=AWP
      RWO(LXA+ 2)=LIP
      RWO(LXA+ 3)=LAW
      RWO(LXA+ 4)=NR
      RWO(LXA+ 5)=NP
      LAE=LXA+6
      LAG=LAE+NP
      LA1=LAG+NP
      IF(ZAP.NE.0) THEN
        NE6N=NE6
        RWO(LXA+ 6)=E1
        RWO(LXA+ 7)=E2
        RWO(LXA+ 8)=Y1
        RWO(LXA+ 9)=Y2
      ELSE
        IF(NP.EQ.0) THEN
          WRITE(LTT,995) ' EMPEND ERROR - Gamma spect. first  MF 6'
          WRITE(LER,995) ' EMPEND ERROR - Gamma spect. first  MF 6'
          STOP ' EMPEND ERROR - gamma spectrum appears first'
        END IF
        DO 702 I=1,NP
        RWO(LAE-1+I)=EIS(I)
        RWO(LAG-1+I)=GAM(I)
  702   CONTINUE
      END IF
      RWO(LA1   )=LANG
      RWO(LA1+ 1)=LEP
      RWO(LA1+ 2)=NRA
      RWO(LA1+ 3)=NE6
      RWO(LA1+ 4)=NE6
      RWO(LA1+ 5)=INA
C* Reaction data read - check for next outgoing particle
      IF(IK.GE.NK) GO TO 720
      IK =IK+1
      NE6=0
      REWIND LIN
      GO TO 110
C* All data for a reaction processed
  720 MT     = MTH(IT)
      MTH(IT)=-MT
      RETURN
C*
  802 FORMAT(I3,1X,A2,1X,I3)
  803 FORMAT(37X,F12.0)
  804 FORMAT(1X,I3,1X,A2,1X,I3,4X,3F10.0)
  805 FORMAT(I12,F10.0,16X,F12.0)
  806 FORMAT(6X,8(5X,F10.4))
  807 FORMAT(BN,F10.5,F14.4,7F15.4)
  808 FORMAT(BN,I6,6X,F12.0)
  809 FORMAT(9X,8F15.4)
  891 FORMAT(A136)
  906 FORMAT(' EMPEND WARNING - MT',I3,' IZA',I5
     1      ,' Ein',1P,E10.3,' Eou',E10.3,' Ang.distrib. -ve')
  907 FORMAT(' EMPEND WARNING - MT',I3,' IZA',I5
     1      ,' Ein',1P,E10.3,' Eou',E10.3,' Ang.Fit Dif',0P,F6.1,'%')
  909 FORMAT(' EMPEND WARNING - MT',I3,' E',1P,E10.3
     1      ,'eV  Expected x.s.',E10.3,'b  Dif.',0P,F6.1,'%')
  931 FORMAT('P(',I2.2,') Fit')
  932 FORMAT(1P,'Ei',E7.2E1,' Eo',E7.2E1,' MT',I3,' PZA',I5)
  934 FORMAT(1P,6E11.4)
  994 FORMAT(BN,F10.0)
  995 FORMAT(A40,I4)
      END
      SUBROUTINE REMF12(LIN,LTT,LER,IZA,NLV,ENL,NBR,LBR,BRR,MXLI,MXLJ)
C-Title  : REMF12 Subroutine
C-Purpose: Read EMPIRE output discrete levels
      CHARACTER*2    CH
      CHARACTER*136  REC
      DIMENSION      NBR(MXLI),ENL(MXLI)
     1              ,LBR(MXLJ,MXLI),BRR(MXLJ,MXLI)
C*
C* Search for the product nucleus data
   20 READ (LIN,891,END=90) REC
      IF(REC(1:18).NE.'  Decaying nucleus') GO TO 20
      READ (REC(20:29),802) JZ,CH,JA
      JZA=JZ*1000+JA
C* Test for discrete levels inelastic scattering cross section
      IF(JZA  .NE. IZA    ) GO TO 20
C* Process discrete levels
   35 READ (LIN,891) REC
      IF(REC(13:22).EQ.'production') GO TO 90
      IF(REC(11:30).NE.'Discrete level popul') GO TO 35
C* Positioned to read discrete levels
      READ (LIN,891)
      READ (LIN,891)
      READ (LIN,891)
      XI=0.
   36 READ (LIN,891) REC
      READ (REC,805) IL,EL,X,JL
      IF(IL.LE.0) GO TO 70
      IF(IL.GT.MXLI) STOP 'REMF12 ERROR - MXLI Limit exceeded'
      IF(JL.GT.MXLJ) STOP 'REMF12 ERROR - MXLJ Limit exceeded'
      IF(JL.LE.0) THEN
        IF(IL.GT.1) THEN
C* Trap levels with no branching ratios given
          WRITE(LTT,912) IL
          WRITE(LER,912) IL
        END IF
C* Assume 100% transition to ground level
        JL=1
        LBR(1,IL)=EL
        BRR(1,IL)=1
        GO TO 39
      END IF
C* Read the branching ratios
      IF(REC(60:60).EQ.'.') THEN
C*      Format OF EMPIRE-2.17 and later
        J2=7
        READ (REC,806,ERR=92) IL,EL,X,JL,(LBR(J,IL),BRR(J,IL),J= 1,J2)
        IF(JL.GT.MXLJ) STOP 'REMF12 ERROR - FORMAT 806 Limit exceeded'
   37   J1=J2+1
        IF(J1.LE.JL) THEN
          J2=MIN(JL,J1+6)
          READ(LIN,807) (LBR(J,IL),BRR(J,IL),J=J1,J2)
          GO TO 37
        END IF
      ELSE
C*      Format up to EMPIRE-2.16 (Montenotte)
        READ (REC,805,ERR=92) IL,EL,X,JL,(LBR(J,IL),BRR(J,IL),J=1,JL)
        DO 38 J=1,JL
        BRR(J,IL)=BRR(J,IL)/100
   38   CONTINUE
      END IF
   39 NLV=IL
      NBR(IL)=JL
      ENL(IL)=EL*1.E6
      GO TO 36
C* Branching ratios for all discrete levels processed
   70 CONTINUE
      RETURN
C* No discrete level data given
   90 NLV=0
      RETURN
C* Trap read error
   92 STOP 'EMPEND ERROR reading MF12 data'
C*
  802 FORMAT(I3,1X,A2,1X,I3)
  805 FORMAT(I12,F10.0,15X,F13.0,I3,13(I3,F3.0))
  806 FORMAT(I12,F10.0,13X,F15.0,I3, 7(I4,F7.0))
  807 FORMAT(53X,7(I4,F7.0))
  891 FORMAT(A136)
  912 FORMAT(' EMPEND WARNING - No b.r. data for level',I3/
     1       '                  Transfer to ground state assumed')
      END
      SUBROUTINE WRIMF1(LOU,MAT,IZA,AWR,EMX,NS)
C-Title  : WRIMF1 Subroutine
C-Purpose: Write comment section (file-1) data in ENDF-6 format
      CHARACTER*66 REC
      CHARACTER*2  C2,CH(100)
      DATA CH
     1 /' H','He','Li','Be',' B',' C',' N',' O',' F','Ne'
     2 ,'Na','Mg','Al','Si',' P',' S','Cl','Ar',' K','Ca'
     3 ,'Sc','Ti',' V','Cr','Mn','Fe','Co','Ni','Cu','Zn'
     4 ,'Ga','Ge','As','Se','Br','Kr','Rb','Sr',' Y','Zr'
     5 ,'Nb','Mo','Tc','Ru','Rh','Pd','Ag','Cd','In','Sn'
     6 ,'Sb','Te',' I','Xe','Cs','Ba','La','Ce','Pr','Nd'
     7 ,'Pm','Sm','Eu','Gd','Tb','Dy','Ho','Er','Tm','Yb'
     8 ,'Lu','Hf','Ta',' W','Re','Os','Ir','Pt','Au','Hg'
     9 ,'Tl','Pb','Bi','Po','At','Rn','Fr','Ra','Ac','Th'
     * ,'Pa',' U','Np','Pu','Am','Cm','Bk','Cf','Es','Fm'/
C*
      IZ  = IZA/1000
      IA  = IZA-1000*IZ
      C2  ='??'
      IF(IZ.GT.0 .AND.IZ.LE.100) C2=CH(IZ)
      WRITE(REC,92) IZ,C2,IA
C*
      MF  = 1
      MT  = 451
      ZA  = IZA
      LRP =-1
      LFI = 0
      NLIB= 8
      NMOD= 0
C*
      ELIS= 0.
      STA = 0.
      LIS = 0
      LISO= 0
      NFOR= 6
C*
      AWI = 1.
      NSUB= 10
      NVER= 3
C*
      TEMP= 0.
      LDRV= 0
      NWD = 1
      NXC = 0
C*
      CALL WRCONT(LOU,MAT,MF,MT,NS,  ZA,AWR,LRP,LFI,NLIB,NMOD)
      CALL WRCONT(LOU,MAT,MF,MT,NS,ELIS,STA,LIS,LISO, 0 ,NFOR)
      CALL WRCONT(LOU,MAT,MF,MT,NS,AWI ,EMX,  0,  0,NSUB,NVER)
      CALL WRCONT(LOU,MAT,MF,MT,NS,TEMP, 0.,LDRV, 0, NWD, NXC)
      CALL WRTEXT(LOU,MAT,MF,MT,NS,REC)
      CALL WRCONT(LOU,MAT,MF, 0,NS, 0. , 0., 0, 0, 0, 0)
      CALL WRCONT(LOU,MAT, 0, 0,NS, 0. , 0., 0, 0, 0, 0)
C*
      RETURN
C*
   92 FORMAT(I3,'-',A2,'-',I3,' EMPIRE Calculation ',36X)
      END
      SUBROUTINE WRIMF3(LOU,MXE,MXT,MXR
     1                 ,EIN,XSC,QQM,QQI,MTH,RWO
     1                 ,MAT,IZA,AWR,NEN,NEP,NXS,ERR,NS)
C-Title  : WRIMF3 Subroutine
C-Purpose: Write cross section (file MF3) data in ENDF-6 format
      DIMENSION    EIN(MXE),XSC(MXE,MXT),MTH(MXT),QQM(MXT),QQI(MXT)
     1            ,RWO(MXR),NBT(1),INT(1)
C* Cross sections are set to zero if Log(cross-sect.) < SMALL
      DATA SMALL,ZRO/-34., 0./
C* Initialize constants
      QM=0.
      QI=0.
C*
C* Write file MF3 (cross section data)
      MF =3
      ZA =IZA
      DO 360 JT=1,NXS
C* Select MT numbers in ascending order
      IT =0
      MT =999
      DO 302 J=1,NXS
      IF(MTH(J).LT.   0 .OR.
     1   MTH(J).GT.1000 .OR.
     2   MTH(J).GT.  MT) GO TO 302
      IT =J
      MT =MTH(IT)
  302 CONTINUE
      IF(IT.EQ.0) GO TO 360
C* Define the thresholdand max. energy
c     ETH=MAX((-QQI(IT))*(AWR+1.)/AWR , EIN(1) )
c     EMX=EIN(NEN)
c     IF(ETH.GE.EMX) THEN
c       MTH(IT)=MTH(IT)+10000
c       GO TO 360
c     END IF
C* Consider the output energy mesh
      IF(NEP.GT.0) GO TO 320
C* Case: Enter original points
      NEO=NEN
      LX =1
      LY =LX+NEO
      LBL=LY+NEO
      DO 310 J=1,NEN
      RWO(LX-1+J)=EIN(J)
      RWO(LY-1+J)=XSC(J,IT)
  310 CONTINUE
      GO TO 350
C* Case: Expand the cross section set by spline interpolation
  320 NEO=NEP*NEN *2
      LX =1
      LY =LX+NEO
      MX =LY+NEO
      MY =MX+NEN+1
      LS =MY+NEN+1
      LBL=MX
      IF(LS+3*NEN+MAX(NEN,NEO).GT.MXR)
     1 STOP 'EMPEND ERROR - MXR limit exceeded in WRIMF3'
C* Define the threshold
      ETH=MAX((-QQI(IT))*(AWR+1.)/AWR , EIN(1) )
      XSL=SMALL
C* Define the input energy points for the spline fit
      ME =0
      DO 322 J=1,NEN
        IF(EIN(J).LT.ETH) GO TO 322
        IF(EIN(J).GT.ETH .AND. ME.EQ.0) THEN
C* Threshold case
          RWO(MX+ME)=ETH
          RWO(MY+ME)=SMALL
          ME=ME+1
        END IF
C* Normal case
        RWO(MX+ME)=EIN(J)
        IF(XSC(J,IT).GT.0) THEN
          XSL=ALOG(XSC(J,IT))
          RWO(MY+ME)=XSL
        ELSE
          RWO(MY+ME)=SMALL
        END IF
        IF(ME.NE.1 .OR. RWO(MY+ME).GT.SMALL) ME=ME+1
  322 CONTINUE
C* Check that there are NON-ZERO points above threshold
      IF(ME.LT.2 .OR.
     1  (ME.EQ.2 .AND.XSL.LE.SMALL)) THEN
        MTH(IT)=MTH(IT)+10000
        GO TO 360
      END IF
C* Define the output grid for spline interpolated function
      NE1=1
      E2 =RWO(MX)
      RWO(LX)=E2
      RWO(LY)=RWO(MY)
      DO 324 J=2,NEN
        E1 =E2
        E2 =EIN(J)
        DE=(E2-E1)/FLOAT(NEP)
        DO 323 K=1,NEP
          EE= E1+(K-1)*DE
          IF(EE.LE.1.001*ETH) GO TO 323
          RWO(LX+NE1)=EE
          NE1=NE1+1
  323   CONTINUE
  324 CONTINUE
      RWO(LX+NE1)=RWO(MX-1+ME)
      RWO(LY+NE1)=RWO(MY-1+ME)
      NE1=NE1+1
      IF(NE1.LE.2) GO TO 332
C* Log-lin interpolate from threshold
      JX=MX
      JY=MY
      JE=ME
      KX=LX
      KY=LY
      KE=NE1
C* Linearly interpolate the first non-zero x-sect on output grid
  326 IF(RWO(JY).GT.SMALL) GO TO 330
      RWO(KY)=RWO(JY)
     1 +(RWO(JY+1)-RWO(JY))*(RWO(KX)-RWO(JX))/(RWO(JX+1)-RWO(JX))
      KX=KX+1
      KY=KY+1
      KE=KE-1
      IF(KE.LT.1) GO TO 332
      IF(RWO(KX).LT.RWO(JX+1)) GO TO 326
      IF(JE.LE.2) GO TO 332
      JX=JX+1
      JY=JY+1
      JE=JE-1
      GO TO 326
C* Spline fit log of the cross section (oner non-zero range)
  330 F1=0.
      F2=0.
      CALL FINSP3(RWO(JX),RWO(JY),JE
     1           ,RWO(KX),RWO(KY),RWO(KY),KE,F1,F2,RWO(LS)) 
C* Convert back to cross sections from log
  332 DO 336 J=1,NE1
      R=RWO(LY-1+J)
      RWO(LY-1+J)=0.
      IF(R.GT.SMALL) RWO(LY-1+J)=EXP(R)
  336 CONTINUE
C* Thin the cross section to ERR lin.interp. tolerance limit
      NEO=NE1
      IF(ERR.GT.0)
     1CALL THINXS(RWO(LX),RWO(LY),NE1,RWO(LX),RWO(LY),NEO,ERR)
C* Write HEAD record
  350 CALL WRCONT(LOU,MAT,MF,MTH(IT),NS, ZA,AWR, 0, 0, 0, 0)
C* Write TAB1 record
      NR    =1
      INT(1)=2
      NBT(1)=NEO
      CALL WRTAB1(LOU,MAT,MF,MTH(IT),NS,QQM(IT),QQI(IT), 0, 0
     1           ,NR,NEO,NBT,INT,RWO(LX),RWO(LY))
C* Write CONT record - end of data set
      CALL WRCONT(LOU,MAT,MF, 0,NS,ZRO,ZRO, 0, 0, 0, 0)
      MTH(IT)=MTH(IT)+10000
  360 CONTINUE
      CALL WRCONT(LOU,MAT, 0, 0,NS,ZRO,ZRO, 0, 0, 0, 0)
C* Change back the sign of the MT numbers that were flagged -ve
      DO 362 I=1,NXS
      IF(MTH(I).GT.1000) MTH(I)=MTH(I)-10000
  362 CONTINUE
C*
      RETURN
      END
      SUBROUTINE WRIMF4(LOU,MTH,QQM,QQI,NXS,MT6,RWO
     1                 ,MT,MAT,IZA,AWR,LCT,NS)
C-Title  : WRIMF4 Subroutine
C-Purpose: Write angular distributions (file-4) data in ENDF-6 format
      PARAMETER   (MXQ=80)
      DIMENSION    RWO(1),QQM(1),QQI(1),MTH(1),NBT(1),INT(1)
      DIMENSION    QQ(MXQ)
C* Tolerance limit for energy levels (eV)
      DATA DLVL/1.E3/
C*
      DATA ZRO/0./
C* Find the appropriate discrete level MT number
      IF(MT6.EQ. 91 .OR.
     1   MT6.EQ.  5) MT=MAX(MT, 51)
      IF(MT6.EQ.649) MT=MAX(MT,600)
      IF(MT6.EQ.849) MT=MAX(MT,800)
      DO 20 JT=1,NXS
      IT=JT
      IF(ABS(MTH(IT)).EQ.MT ) GO TO 22
   20 CONTINUE
      GO TO 80
C*
C* Write file MF4 angular distributions (first outgoing particle)
   22 MF =4
      ZA =IZA
      LTT=1
      LVT=0
      LI =0
      LL =1
      ZAP=RWO(LL)
      IF(ZAP.EQ.0 .OR. ZAP.GT.2004.) THEN
        STOP 'WRIMF4 ERROR - Illegal first particle'
      END IF
      AWP=RWO(LL+1)
      NP =RWO(LL+5)+0.1
      NE =RWO(LL+2*NP+9)+0.1
      JE =NE
      NR =1
      LL =LL+12+2*NP
      JE =NE
      J2 =0
      INT(1)= 2
      ETH=MAX((-QQI(IT))*(AWR+1.)/AWR, RWO(LL))
C*
C* Loop over the incident particle energies
      DO 40 IE=1,NE
      TT  =0.
      LT  =0
      EIN =RWO(LL  )
      NA  =RWO(LL+1)+0.1
      NW  =RWO(LL+2)+0.1
      NEP =RWO(LL+3)+0.1
      NA1 =NA+1
      LL  =LL+4
C* Determine the outgoing particle energy
      IF(MT.EQ.2) THEN
        EOU=0
      ELSE
        EOU=(EIN*AWR/(AWR+1)-QQM(IT)+QQI(IT))*(AWR+1-AWP)/(AWR+1)
      END IF
      IF(EIN-ETH.LT.-1.E-4 .OR.
     1   EOU/EIN.LT.-1.E-4) THEN
C* Skip points below threshold
        JE=JE-1
        LL=LL+NW
        GO TO 40
      END IF
C* Print header CONT and TAB2 records
      IF(J2.EQ.0) THEN
        IF(ABS(EIN-ETH).LT.1.E-4*EIN) THEN
          EIN=ETH
        ELSE
C*        Set the threshold data and flag J2
          JE=JE+1
          J2=1
          NL=2
          QQ(1)=0
          QQ(2)=0
        END IF
        NBT(1)=JE
        CALL WRCONT(LOU,MAT,MF,MT,NS, ZA,AWR,LVT,LTT, 0, 0)
        CALL WRCONT(LOU,MAT,MF,MT,NS, 0.,AWR, LI,LCT, 0, 0)
        CALL WRTAB2(LOU,MAT,MF,MT,NS, 0.,0., 0, 0
     1             ,NR,JE,NBT,INT)
C* Print threshold distribution (if necessary)
        IF(J2.EQ.1)
     1  CALL WRLIST(LOU,MAT,MF,MT,NS,TT,ETH,LT, 0,NL, 0,QQ)
        J2 =J2+1
      END IF
C* Process the main data block
      RR  =0.
      L2  =LL
      E2  =RWO(L2)
C*
      IF(NEP.LE.1) THEN
C* Copy the coefficients if a single point is given
        IF(NA1.GT.MXQ) STOP 'EMPEND ERROR - MXQ Lim.in WRIMF4 exceeded'
        CALL FLDMOV(NA1,RWO(L2+1),QQ)
        IF(QQ(1).NE.0) RR  =1./QQ(1)
      ELSE
C* Linearly interpolate Legendre coefficients to EOU
        IEP =1
   38   IEP =IEP+1
        L1  =L2
        E1  =E2
        L2  =L1+NA+2
        E2  =RWO(L2)
        IF(ABS(EOU-E2).LT.DLVL .OR.
     1    (E2.LT.EOU .AND. IEP.GE.NEP) ) THEN
C* Last point or matching level to within tolerance
            CALL FLDMOV(NA1,RWO(L2+1),QQ)
            IF(QQ(1).NE.0) RR  =1./QQ(1)
C...           print *,' Match MT,Ein,Eou,E2',MT,EIN,EOU,E2,qqi(it)
C...           print *,' iep,nep',iep,nep
C...           print *,e1,(rwo(l1+j),j=1,NA1)
C...           print *,e2,(rwo(l2+j),j=1,NA1)
C...           read (*,'(a1)') yes
        ELSE
C* Try next point or linearly interpolate
          IF(E2.LT.EOU) GO TO 38
          CALL FLDINT(NA1,E1,RWO(L1+1),E2,RWO(L2+1),EOU,QQ)
          IF(QQ(1).NE.0) RR  =1./QQ(1)
C...           print *," Inter MT,Ein,Eou,E1,E2",MT,EIN,EOU,E1,E2,qqi(it)
C...           print *,e1,(rwo(l1+j),j=1,NA1)
C...           print *,e2,(rwo(l2+j),j=1,NA1)
C...           read (*,'(a1)') yes
        END IF
      END IF
      IF(NA.LE.0) THEN
        NA=1
        QQ(2)=0.
      ELSE
        CALL FLDSCL(NA,RR,QQ(2),QQ(2))
      END IF
      NL=NA
C* Make the Legendre order even
      IF(NL/2 .NE. (NL+1)/2) THEN
        NL=NL+1
        QQ(1+NL)=0.
      END IF
C* Reduce the trailing zero Legendre coefficients
   39 IF(NL.GT.2 .AND. (QQ(NL).EQ.0 .AND. QQ(1+NL).EQ.0) ) THEN
        NL=NL-2
        GO TO 39
      END IF
      CALL WRLIST(LOU,MAT,MF,MT,NS,TT,EIN,LT, 0,NL, 0,QQ(2))
      LL  =LL+NW
   40 CONTINUE
      IF(J2.GT.0)
     1CALL WRCONT(LOU,MAT,MF, 0,NS,ZRO,ZRO, 0, 0, 0, 0)
      RETURN
C* All discrete levels processed - nothing to write
   80 MT=0
      RETURN
      END
      SUBROUTINE WRIMF6(LOU,RWO,MT,MAT,IZA,AWR,NK,LCT,NS)
C-Title  : WRIMF6 Subroutine
C-Purpose: Write energy/angle (file-6) data in ENDF-6 format
      DIMENSION    RWO(1),NBT(1),INT(1)
      DATA ZRO/0./
C*
C* Write file MF6 (energy/angle distributions)
      MF =6
      ZA =IZA
      CALL WRCONT(LOU,MAT,MF,MT,NS, ZA,AWR, 0,LCT,NK, 0)
      LL =1
C* Loop over outgoing particles
      DO 60 IK=1,NK
      ZAP=RWO(LL  )
      AWP=RWO(LL+1)
      LIP=RWO(LL+2)+0.1
      LAW=RWO(LL+3)+0.1
      NR =RWO(LL+4)+0.1
      NP =RWO(LL+5)+0.1
      NBT(1)=NP
      INT(1)=2
      LAE=LL+6
      LAG=LAE+NP
      LA1=LAG+NP
      CALL WRTAB1(LOU,MAT,MF,MT,NS,ZAP,AWP,LIP,LAW
     1           ,NR,NP,NBT,INT,RWO(LAE),RWO(LAG))
      LANG  =RWO(LA1   )+0.1
      LEP   =RWO(LA1+ 1)+0.1
      NR    =RWO(LA1+ 2)+0.1
      NE    =RWO(LA1+ 3)+0.1
      NBT(1)=RWO(LA1+ 4)+0.1
      INT(1)=RWO(LA1+ 5)+0.1
      CALL WRTAB2(LOU,MAT,MF,MT,NS,0.,0.,LANG,LEP
     1           ,NR,NE,NBT,INT)
      LL=LL+12+2*NP
C* Loop over the incident particle energies
      DO 40 IE=1,NE
      ND  =0
      EOU =RWO(LL  )
      NA  =RWO(LL+1)+0.1
      NW  =RWO(LL+2)+0.1
      NEP =RWO(LL+3)+0.1
      LL  =LL+4
      CALL WRLIST(LOU,MAT,MF,MT,NS,0.,EOU,ND,NA,NW,NEP,RWO(LL))
      LL  =LL+NW
   40 CONTINUE
   60 CONTINUE
      CALL WRCONT(LOU,MAT,MF, 0,NS,ZRO,ZRO, 0, 0, 0, 0)
C*
      RETURN
      END
      SUBROUTINE WRMF12(LOU,MAT,MT0,IZA,AWR,NLV,ENL,NBR,LBR,BRR
     1                 ,RWO,MXLI,MXLJ,NS)
C-Title  : WRMF12 Subroutine
C-Purpose: Write MF 12 data in ENDF-6 format
      DIMENSION      NBR(MXLI),ENL(MXLI),RWO(2,MXLI)
     1              ,LBR(MXLJ,MXLI),BRR(MXLJ,MXLI)
C*
      ZRO=0
      ZA =IZA
      MF =12
      L0 =2
      LG =1
      LP =0
C* Loop over all discrete levels above ground state
      DO 120 LL=2,NLV
C* Define reaction type MT number
      MT=MT0+LL-1
C* Number of levels below the present one
      NS=LL-1
C* Energy of the level
      ES=ENL(LL)
C* Number of levels with non-zero branching ratio
      NT=NBR(LL)
C* Head record
      CALL WRCONT(LOU,MAT,MF,MT,NS, ZA,AWR, L0, LG, NS,  0)
C* List record
      DO 110 JT=1,NT
C* Determine the energy level of the final state
      LE=LBR(JT,LL)
      RWO(1,JT)=ENL(LE)
C* Determine the branching fraction for this level
      RWO(2,JT)=BRR(JT,LL)
  110 CONTINUE
      CALL WRLIST(LOU,MAT,MF,MT,NS,ES, 0., LP,  0,2*NT,NT,RWO)
C* Section end
      CALL WRCONT(LOU,MAT,MF, 0,NS,ZRO,ZRO, 0, 0, 0, 0)
C*
  120 CONTINUE
      RETURN
      END
      SUBROUTINE WRMF14(LOU,MAT,MT0,IZA,AWR,NLV,NS)
C-Title  : WRMF14 Subroutine
C-Purpose: Write MF 14 data in ENDF-6 format
      ZRO=0
      ZA =IZA
      MF =14
C* Loop over all discrete levels
      DO 140 LL=1,NLV
C* Define reaction type MT number
      MT=MT0+LL
C* Assume isotropic photon distribution
      CALL WRCONT(LOU,MAT,MF,MT,NS, ZA,AWR, 1, 0, 1, 0)
C* Section end
      CALL WRCONT(LOU,MAT,MF, 0,NS,ZRO,ZRO, 0, 0, 0, 0)
  140 CONTINUE
      RETURN
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
      SUBROUTINE FINSP3(XI,YI,N,XO,YO,Y1,M,F1,F2,SC)
C-Title  : FINSP3 Subroutine
C-Purpose: Interpolate a set of points using cubic spline
C-Description:
C-D  Interpolate a set of points using cubic splines. The parameters
C-D  are the following:
C-D    N   number of points on input argument mesh 
C-D   XI   input argument mesh (array of N values in momotonic order)
C-D   YI   function values corresponding to XI(i)
C-D    M   number of points in the output mesh
C-D   XO   output argument mesh (array of M values in monotonic order)
C-D   YO   interpolated function values corresponding to XO(i) (Output)
C-D   Y1   interpolated derivative values corresponding to XO(i) (Output)
C-D        NOTE: if derivatives ara not required, the same array may be
C-D              specified for Y0 and Y1 (i.e.: implicit equivalence)
C-D   F1   second derivative in the first interval (usually zero)
C-D   F2   second derivative in the last  interval (usually zero)
C-D   SC   scratch array of length 3N+Max(N,M) (or more)
C-D
C-Extern.: MTXDG3
C-Literat: K.E.Atkinson, An Introduction to Numerical Analysis,
C-L        John Wiley and Sons, New York, (1978)
C-Author : A.Trkov, Institute J.Stefan, Ljubljana, Slovenia (1991)
      DIMENSION XI(*), YI(*), XO(*), YO(*), Y1(*), SC(*)
      IF(N.LT.3) GO TO 80
C* Define the function and the tri-diagonal matrix
      N3 = N*3
      N2 = N-2
      HO = XO(2) - XO(1)
      H2 = XI(2) - XI(1)
      Z2 = YI(2) - YI(1)
      K  = 1
      IF(HO*H2.LT.0) K =-1
      SC(N3+1)=F1
      SC(N3+2)=0.5*(F1+F2)
      SC(N3+N)=F2
      IF(N.EQ.3) GO TO 40
      DO 30 I=1,N2
      H1 = H2
      H2 = XI(I+2) - XI(I+1)
      IF(H1*H2.LE.0) STOP 'FINSP3 non-monotonic argument'
      Z1 = Z2
      Z2 = YI(I+2) - YI(I+1)
      I3 =(I-1)*3
      SC(1+I3) = H1     / 6.
      SC(2+I3) =(H1+H2) / 3.
      SC(3+I3) =    H2  / 6.
      SC(I+N3) = Z2/H2  - Z1/H1
   30 CONTINUE
C* Solve for the second derivatives of the interpolated function
      CALL MTXDG3 (SC,SC(N3+1),SC(N3+2),N2,0)
C* Interpolate to the specified output grid
   40 L = 1
      IF(K.LT.0) L = N-1
      DO 70 I=1,M
      X = XO(I)
C* Find the appropriate input argument mesh interval
   62 X1 = XI(L+1) - X
      X0 = X       - XI(L)
      IF(X0*X1.GE.0) GO TO 64
      L1= L + K
      IF(L1.LT.1 .OR. L1.GE.N) GO TO 64
      L = L1
      GO TO 62
C* Calculate the interpolated derivative and function
   64 H1 = XI(L+1) - XI(L)
      Y1(I)= (  - X1*X1*SC(N3+L) +    X0*X0*SC(N3+L+1) ) / (H1 * 2.)
     1     - (          YI(   L) -          YI(   L+1) ) /  H1
     2     + (          SC(N3+L) -          SC(N3+L+1) ) *  H1 / 6.
      YO(I)= ( X1*X1*X1*SC(N3+L) + X0*X0*X0*SC(N3+L+1) ) / (H1 * 6.)
     1     + (    X1 *  YI(   L) +    X0 *  YI(   L+1) ) /  H1
     2     - (       X1*SC(N3+L) +       X0*SC(N3+L+1) ) *  H1 / 6.
   70 CONTINUE
      RETURN
C* Special case when less than 3 input points
   80 C0 = YI(1)
      C1 = 0.
      IF(N.LT.2) GO TO 81
      C1 = (YI(2)-YI(1))/(XI(2)-XI(1))
      C0 = C0 - C1*XI(1)
   81 DO 82 I=1,M
      Y1(I) =      C1
      YO(I) = C0 + C1*XO(I)
   82 CONTINUE
      RETURN
      END
      SUBROUTINE MTXDG3(A,F,X,N,IF)
C-Title  : MTXDG3 subroutine
C-Purpose: Tridiagonal Matrix solver, Gauss elimination, no pivoting
C-Description:
C-D Solve a set of linear simultaneous equations  A x = F  (order n)
C-D assuming matrix A is tridiagonal, rows stored in first index of A.
C-D Crout-Choleski matrix decomposition, no pivoting (A = LU).
C-D Options: if=0  -decompose matrix and solve
C-D             1  -decompose matrix only
C-D             2  -solve for new F assuming matrix is decomposed
C-Author : A.Trkov , Institute J.Stefan Ljubljana, 1986
      DIMENSION A(3,N),F(N),X(N)
      N1=N-1
      IF(IF.GT.1) GO TO 45
C* Matrix decomposition - forward sweep  (A = LU)
      IF(N.LT.2) GO TO 42
      A(3,1)=-A(3,1) /  A(2,1)
      DO 40 I=2,N1
      A(2,I)= A(2,I) + A(1,I)*A(3,I-1)
      A(3,I)=-A(3,I) / A(2,I)
   40 CONTINUE
      A(2,N)= A(2,N) + A(1,N)*A(3,N1)
   42 IF(IF.GT.0) RETURN
C* Forward sweep (p = L-1 F)
   45 F(1)=  F(1)                / A(2,1)
      DO 50 I=2,N
      F(I)= (F(I)-A(1,I)*F(I-1)) / A(2,I)
   50 CONTINUE
C* Backward sweep (x = U-1 p)
      X(N)=F(N)
      DO 60 I=1,N1
      NI=N-I
      X(NI)= F(NI) + A(3,NI)*X(NI+1)
   60 CONTINUE
      RETURN
      END
      SUBROUTINE FLDMOV(N,A,B)
C-Title  : FLDMOV Subroutine
C-Sample : CALL FLDMOV(N,A,B)
C-Purpose: Move N words from array A to array B
C-Extern.: none
C-Author : A.Trkov, "J.Stefan" Inst. Ljubljana, Slovenia (1989)
C-
      DIMENSION A(1),B(1)
      DO 10 I=1,N
   10 B(I) = A(I)
      RETURN
      END
      SUBROUTINE FLDINT(N,XA,A,XB,B,X,C)
C-Title  : FLDINT Subroutine
C-Sample : CALL FLDINT(N,XA,A,XB,B,X,C)
C-Purpose: Interpolate N words from arrays A and B given at XA, XB
C-P        into array C given at X
C-Extern.: none
C-Author : A.Trkov, "J.Stefan" Inst. Ljubljana, Slovenia (1989)
C-
      DIMENSION A(1),B(1),C(1)
      R = (X-XA)/(XB-XA)
      DO 10 I=1,N
      C(I) = A(I) + R*( B(I) - A(I) )
   10 CONTINUE
      RETURN
      END
      SUBROUTINE FLDSCL(N,R,A,B)
C-Title  : FLDSCL Subroutine
C-Sample : CALL FLDSCL(N,R,A,B)
C-Purpose: Scale N elements of array A by R and store in B
C-Extern.: none
C-Author : A.Trkov, "J.Stefan" Inst. Ljubljana, Slovenia (1989)
C-Version: 97/07 - separate A, B fields
C-
      DIMENSION A(1),B(1)
      DO 10 I=1,N
      B(I) = A(I)*R
   10 CONTINUE
      RETURN
      END
      SUBROUTINE THINXS(XI,YI,N,XO,YO,M,ERR)
C-Title  : THINXS Subroutine
C-Purpose: Thin the data to within the specified tolerance
C-Version: 1996 Original code
C-V  02/04 A.Trkov: Fix bug (missing one but last point).
C-Author : A.Trkov, ENEA, Bologna, 1996
C-Description:
C-D  Excessive data points need to be removed such that the remaining
C-D  points can be linearly interpolated to within the specified
C-D  tolerance. The formal parameters are:
C-D    N   number of points on input argument mesh 
C-D   XI   input argument mesh (array of N values in momotonic order)
C-D   YI   function values corresponding to XI(i)
C-D    M   number of points in the output mesh
C-D   XO   output argument mesh (array of M values in monotonic order)
C-D   YO   interpolated function values corresponding to XO(i) (Output)
C-D  ERR   fractional tolerance on the interpolated array.
C-D
      DIMENSION XI(*), YI(*), XO(*), YO(*)
      FINT(X,XA,XB,YA,YB)=YA+(YB-YA)*(X-XA)/(XB-XA)
      K=3
      M=1
      I=1
      XO(M)=XI(I)
      YO(M)=YI(I)
C* Begin loop over data points
   10 IF(K.GE.N) GO TO 60
   12 L1=I+1
      L2=K-1
      DO 20 L=L1,L2
      IF(XI(K).EQ.XI(I)) GO TO 20
      Y=FINT(XI(L),XI(I),XI(K),YI(I),YI(K))
      E=ABS(Y-YI(L))
      IF(YI(L).NE.0) E=ABS(E/YI(L))
      IF(E.GT.ERR) GO TO 40
   20 CONTINUE
C* Linear interpolation adequate - increase the test interval
      K=K+1
      GO TO 10
C* Add the previous point at K-1 - lin.interpolation violated
   40 M=M+1
      I=K-1
      K=I+2
      XO(M)=XI(I)
      YO(M)=YI(I)
      IF(K.LE.N) GO TO 12
C* Add the last point
   60 M=M+1
      XO(M)=XI(N)
      YO(M)=YI(N)
      RETURN
      END
      SUBROUTINE LSQLGV(XP,YP,NP,QQ,LMI,LMX,EMM,ERR,RWO,MXR)
C-Title  : LSQLGV Subroutine
C-Purpose: Least-squares fitting by variable order Legendre polynomials
C-Author : A.Trkov, Institute J.Stefan, Ljubljana, Slovenia (1997)
C-Description:
C-D  The Least-squares method is used with a progressively increasing
C-D  Legendre polynomial order to fit a set of NP data points YP(i)
C-D  given at argument values XP(i), which must be in monotonic order
C-D  and in the range X:[-1,1].
C-D    The search for an adequate polynomial order starts at LMI and
C-D  proceeds up to LMX or NP-2, whichever is smaller. If LMX is
C-D  smaller or equal to LMI, the second condition prevails. The
C-D  procedure is terminated earlier if the maximum relative difference
C-D  between an input and a calculated point value is smaller than EMM.
C-D  On exit, ERR contains the actual max.relative difference between
C-D  the input and the fitted data.
C-D    A scratch array RWO of length MXR is needed, where the value of
C-D  MXR does not exceed (LMX+4)*(LMX+1) .
C-D    On output, the Legendre coefficients are stored in QQ. The actual
C-D  order of Legendre polynomials used is contained in LMX.
C-External: LSQLEG, MTXGUP, PLNLEG, POLLG1
C-
      DIMENSION  XP(1),YP(1),QQ(1),RWO(1)
      ERR=0.
      NLG=0
      LMX=MIN(LMX,NP-1)
C* Check if zero-order
      QQ(1)=YP(1)
      IF(NP.LT.2) GO TO 40
      SY =0.
      DO 14 I=2,NP
      SY =SY + 0.5*(YP(I)+YP(I-1))*(XP(I)-XP(I-1))
   14 CONTINUE
      QQ(1)=SY/(XP(NP)-XP(1))
      IF(LMX.LT.1) GO TO 22
C* Clear the coefficients field
      DO 16 L=1,LMX
      QQ(L+1)=0.
   16 CONTINUE
C*
C* Loop to find the appropriate Legendre order
      L1=MAX(1,LMI)
      LL=LMX+2
      L =L1
   20 NLG=L
      N1 =NLG+1
      IF(LL+(NLG+1)*(NLG+3).GT.MXR) 
     1 STOP 'EMPEND ERROR - MXR limit exceeded in LSQLGV'
      CALL LSQLEG(XP,YP,NP,RWO,N1,RWO(LL),JER)
C* Trap zero-determinant
      IF(JER.NE.0) THEN
        NLG=NLG-1
        GO TO 22
      END IF
      DO 21 I=1,N1
      QQ(I)=RWO(I)
   21 CONTINUE
C* Check absolute difference between input and calculated points ERR
C* Check for negative distributions ( ENE < -EPZER0 )
   22 ERR=0.
      ENE=0.
      EPZERO=1.E-10
      DO 24 IP=1,NP
      YCI=POLLG1(XP(IP),QQ,NLG)
      IF(YCI.EQ.0) GO TO 24
C     RER=ABS((YCI-YP(IP))/YCI)
      RER=ABS((YCI-YP(IP))/QQ(1))
      IF(RER.GT.ERR) ERR=RER
      IF(YCI+EPZERO.LT.ENE) ENE=YCI
   24 CONTINUE
      IF(ENE*QQ(1).LT.0) ERR=ENE
C* Try next L
      L =L+1
      IF(L.GT.LMX) GO TO 40
      IF(JER.EQ.0 .AND.
     1  (ERR.GT.EMM .OR. ERR.LT.0)) GO TO 20
C*
   40 LMX=NLG
      RETURN
      END
      SUBROUTINE LSQLEG(XP,YP,NP,QQ,N1,AA,IER)
C-Title  : LSQLEG Subroutine
C-Purpose: Fit Legendre coefficients to a set of data
C-Description:
C-D
      DIMENSION  XP(NP),YP(NP),QQ(N1),AA(N1,1)
C* Perform linear transformation of the coordinate system
      IER=0
      LF=N1+1
      LP=LF+1
C* Clear the matrix
      DO 22 I=1,N1
      AA(I,LF)=0.
      DO 22 J=1,N1
      AA(J,I)=0.
   22 CONTINUE
C* Set up the matrix
      NLG=N1-1
      DO 40 M=1,NP
C* Calculate Legendre polynomials
      CALL PLNLEG(XP(M),AA(1,LP),NLG)
      DO 30 I=1,N1
      PI=AA(I,LP)
      AA(I,LF)=AA(I,LF)+YP(M)*PI
      DO 30 J=I,N1
      PJ=AA(J,LP)
      AA(J,I)=AA(J,I)+PI*PJ
      AA(I,J)=AA(J,I)
   30 CONTINUE
   40 CONTINUE
C* Solve the system of equations
      CALL MTXGUP(AA,AA(1,LF),QQ,N1,LDIG,DET)
      IF(DET.EQ.0) GO TO 80
      RETURN
C* Trap zero determinant
   80 IER=1
      RETURN
      END
      FUNCTION POLLG1(UU,QL,NL)
C-Title  : POLLG1 Function
C-Purpose: Legendre polynomial Sum( Ql* Pl(u) ) function
C-Description:
C-D  Evaluate Legendre polynomial expansion of order NL with 
C-D  coefficients QL at argument value UU in the interval [-1,1]
C-Author : A.Trkov, Institute J.Stefan, Ljubljana, Slovenia, (1997)
C-
      PARAMETER (MXPL=80)
      DIMENSION QL(1),PL(MXPL)
      IF(NL.GE.MXPL) STOP 'POLLG1 ERROR - Array PL capacity exceeded'
      CALL  PLNLEG(UU,PL,NL)
      N1=NL+1
      SS=0.
      DO 20 L=1,N1
      SS=SS+QL(L)*PL(L)
 20   CONTINUE
      POLLG1=SS
      RETURN
      END
      SUBROUTINE PLNLEG(UU,PL,NL)
C-Title  : PLNLEG Subroutine
C-Purpose: Evaluate Legendre polynomials up to order NL
C-Author : A.Trkov, Institute J.Stefan, Ljubljana, Slovenia (1997)
C-Description:
C-D  Given the argument value UU in the interval [-1,1], the
C-D  polynomials up to order NL are calculated by a recurrence
C-D  relation and stored in PL.
C-
      DIMENSION PL(1)
      PL(1)=1.
      IF(NL.LT.1) RETURN
      PL(2)=UU
      IF(NL.LT.2) RETURN
      DO 20 L=2,NL
      PL(L+1)=(PL(L)*UU*FLOAT(2*L-1)-PL(L-1)*FLOAT(L-1))/FLOAT(L)
   20 CONTINUE
      RETURN
      END
      SUBROUTINE MTXGUP(A,F,X,N,LDIG,DET)
C-Title  : MTXGUP subroutine
C-Purpose: Matrix solver, Gauss elimination, part.pivoting
C-Description:
C-D Decompose matrix A, calculate determinant and/or solve a set of 
C-D linear simultaneous equations  A x = F  (order n) using Gauss 
C-D elimination technique with partial pivoting by rows.
C-Author : A.Trkov , Institute J.Stefan, Ljubljana, Slovenia
C-Version: 1984 Original coding
C-V 93/03 - improved zero-determinant trapping
C-V 00/11 - further refinement of zero-determinant trapping (A.Trkov)
C-
      DIMENSION A(N,N),F(N),X(N)
      DET=1.
      ER =1.
      DO 40 I=2,N
      I1=I-1
C* Find the pivot
      A1=0.
      DO 10 K=I1,N
      IF(ABS(A(K,I1)).LT.A1) GO TO 10
      A1=ABS(A(K,I1))
      K1=K
   10 CONTINUE
      IF(I1.LT.2) GO TO 12
      IF(ABS(A1/A0) .GT.1.E-5) GO TO 12
      DET=0.
      RETURN
   12 A0 =A1
      DET=DET*A1
      IF(K1.LT.I) GO TO 20
      A1=A(K1,I1)
      A(K1,I1)=A(I1,I1)
      A(I1,I1)=A1
      A1=F(K1)
      F(K1)=F(I1)
      F(I1)=A1
   20 DO 30 J=I,N
      X(J)=A(J,I1)/A(I1,I1)
      A(J,I1)=0.
   30 F(J)=F(J)-F(I1)*X(J)
      DO 40 J=I,N
      IF(K1.LT.I) GO TO 35
      A1=A(K1,J)
      A(K1,J)=A(I1,J)
      A(I1,J)=A1
   35 DO 40 K=I,N
      A1=A(K,J)
      A2=A1-A(I1,J)*X(K)
      IF(ABS(A1).GT.0.) ER=AMIN1(ER,ABS(A2/A1))
      A(K,J)=A2
   40 CONTINUE
C* Estimate number of digits lost due to subtraction
      LDIG=-ALOG10(ER+1.E-33)+1.
C* Solve by backward substitution
   45 DO 60 I=2,N
      I1=N-I+2
      X(I1)=F(I1)/A(I1,I1)
      J1=N+1-I
      DO 60 J=1,J1
      F(J)=F(J)-X(I1)*A(J,I1)
   60 CONTINUE
      X(1)=F(1)/A(1,1)
      RETURN
      END
