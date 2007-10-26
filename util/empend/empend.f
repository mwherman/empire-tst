Ccc   * $Author: Capote $
Ccc   * $Date: 2007-10-26 21:29:08 $ 
Ccc   * $Id: empend.f,v 1.47 2007-10-26 21:29:08 Capote Exp $ 

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
C-V  03/06 Define MT numbers for incident protons.
C-V  03/07 Fix processing of double differential data for protons.
C-V  04/06 - Process discrete levels flagged with -ve energies.
C-V        - Let EMPIRE handle the max. outgoing particle energy.
C-V  04/07 - Fix ordering of levels in MF12,
C-V        - Read in values in double precision to avoid underflow.
C-V        - Fix MTs and yields of minor reactions (37, 41, etc).
C-V  04/07 - Add (n,5n) MT47 (temporarily deactivated 23jun05)
C-V        - Check for absence of distribution data (NP=0 in MF6).
C-V  04/09 - Check for normalisation overflow in WRIMF4
C-V  04/10 Implement formatting for incident alphas and photons.
C-V  05/04 - Copy comments to MF1/MT451.
C-V        - Upgrade to proces partially exclusive/inclusive spectra.
C-V        - Allow metastable targets.
C-V  05/06 Increase MXR from 200K to 800K.
C-V  05/08 - Improved logic to reduce convergence problems of fitting
C-V          angular distributions.
C-V        - Add unassigned reactions to MT5 with yields for neutrons,
C-V          protons and alpha particles.
C-V        - Fix yields and pseudo-threshold distributions in MF6/MT5.
C-V  06/02 - Input NLIB, ALAB, EDATE, AUTHOR strings.
C-V  07/02 - Trivial change for single-character element designation.
C-V  07/03 - Remove extra SEND record after MF1 data.
C-V  07/05 - Remove redundant messages about ang.dist.interpolation.
C-V        - Fix small bug in gamma-yields at threshold.
C-V        - Read isomer-production cross sections.
C-V  07/10 - Add scattering radius and target spin in File 2.
C-V        - Improve radioisotope production formatting in MF10.
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
C* Limits on array sizes:
C* MXE - Maximum number of incident particle energy points.
C* MXT - Maximum number of reactions (including discrete levels).
C* MXM - Maximum number of residual nuclei.
C* MXR - Lengrh of the real work array RWO.
C* MXI - Length of the integer work array IWO.
      PARAMETER   (MXE=200,MXT=400,MXM=60,MXR=800000,MXI=8000,MLV=3)
      CHARACTER*11 ALAB,EDATE,AUTHOR(3)
      CHARACTER*40 BLNK,FLNM,FLN1,FLN2,FLER
      CHARACTER*80 REC
C* Particle masses (neutron, proton, deuteron, triton, He-3, alpha)
      COMMON /PMASS/ AWN,AWH,AWD, AWT, AW3,AWA
      DIMENSION    EIN(MXE),QQM(MXT),QQI(MXT)
     1            ,RWO(MXR),IWO(MXI),LVLF(MLV),LVMT(MLV),LVIZ(MLV)
c     DATA RWO/MXR*0./
C* Filenames and logical file units
      DATA LIN,LOU,LKB,LTT,LER / 1, 2, 5, 6, 7 /
      DATA BLNK/'                                        '/
     1    ,FLN1/'empire.out'/
     2    ,FLN2/'empire.end'/
     3    ,FLER/'empend.log'/
C* Default cross section linear interpolation tolerance limit 0%
      DATA ERR/0./
      DATA ZRO/0./,RWO/MXR*0./
C* Particle masses (neutron, proton, deuteron, triton, He-3, alpha)
C* ENDF-6 Formats Manual App. H.4, april 2001
C...  AWN = 1.008664916
C...  AWH = 1.007276467
C...  AWD = 2.013553213
C...  AWT = 3.016049268
C...  AW3 = 3.014932235
C...  AWA = 4.001506175
C* AMDC Audi-Wapstra mass tables 2003 "http://www-nds.iaea.org/amdc/"
C* Subtract electron mass and add ionisation energy defect
      AWN = 1.008664916
      AWH = 1.007825032 -   0.00054857991 + 0.000000015
      AWD = 2.014101778 -   0.00054857991 + 0.000000015
      AWT = 3.016049278 -   0.00054857991 + 0.000000015
      AW3 = 3.016029319 - 2*0.00054857991 + 0.000000085
      AWA = 4.002603254 - 2*0.00054857991 + 0.000000085
C* Define the test printout control parameters
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
C* Default ENDF specifications
      NLIB= 8
c...  NMOD= 0
      NMOD= 1
      ALAB='EMPIRE     '
      EDATE='           '
      AUTHOR(1)='           '
      AUTHOR(2)='           '
      AUTHOR(3)='           '
      SPI =0
      STF0=0
      GAMG=0
      D0LV=0
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
C* Define the NLIB number
      WRITE(LTT,991) '$Enter the NLIB number                : '
      READ (LKB,992,END=16) NLIB
      IF(NLIB.LE.0)   NLIB=8
C* Define ALAB, EDATE, AUTHOR strings.
      WRITE(LTT,991) '$Enter ALAB, EDATE, AUTHOR string     : '
      READ (LKB,997,END=16) ALAB,EDATE,AUTHOR
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
C* Process cross section data
      WRITE(LTT,991)
      WRITE(LTT,991) ' BEGIN PROCESSING CROSS SECTION DATA    '
      WRITE(LER,991)
      WRITE(LER,991) ' BEGIN PROCESSING CROSS SECTION DATA    '
C* Define work array addresses
      MTH=1
      IZB=MTH+MXT
      LBI=IZB+MXM
      LSI=LBI+MXT
      IF(LSI.GT.MXI) STOP 'EMPEND ERROR - MXI limit exceeded'
      LXS=1
      LBE=LXS+MXE*MXT
      LSC=LBE+3*MXM
      LXR=MXR-LSC
      IF(LSC.GT.MXR) STOP 'EMPEND ERROR - MXR limit exceeded'
C*
C* Read the EMPIRE output file to extract the cross sections
      CALL REAMF3(LIN,LTT,LER,MXE,MXT,MXM
     1           ,EIN,RWO(LXS),QQM,QQI,IWO(MTH),IWO(IZB),RWO(LBE)
     1           ,IZI,IZA,LISO,AWR,SPI,STF0,GAMG,D0LV,NEN,NXS)
      IF(NEN.LE.0) THEN
        WRITE(LTT,995) ' EMPEND ERROR - Number of energy points:',NEN
        WRITE(LER,995) ' EMPEND ERROR - Number of energy points:',NEN
        STOP 'EMPEND ERROR - Zero energy entries in EMPIRE output'
      END IF
C*
C* Scan the EMPIRE output for all reactions with energy/angle distrib.
      REWIND LIN
      JT6=MXI-LBI
      CALL SCNMF6(LIN,NT6,IWO(LBI),JT6,IZI)
C* Summ MF 5 contributions as necessary
      CALL SUMMF5(IZI,IZA,NXS,NEN,IWO(MTH),NT6,IWO(LBI)
     &           ,RWO(LXS),QQM,QQI,MXE,MXT)
C* Write the ENDF file header record to output
      REC=' EMPEND Processing file : '//FLN1
      NS =-1
      CALL WRTEXT(LOU, 0, 0, 0,NS,REC)
C* Check if nuclide is fissile
      CALL CHKFIS(NXS,NEN,IWO(MTH),RWO(LXS),MXE,MXT,LFI)
C* Write the ENDF file-1 data
      EMX=EIN(NEN)
      CALL WRIMF1(LIN,LOU,MAT,IZI,IZA,LISO,LFI,NLIB,NMOD
     &           ,ALAB,EDATE,AUTHOR,AWR,EMX,NS)
C* Write tabulated ENDF file-1 data (nu-bar)
      NS=-1
      MF= 1
      CALL WRIMF3(LOU,MXE,MXT,LXR,MF
     1           ,EIN,RWO(LXS),QQM,QQI,IWO(MTH),RWO(LSC)
     1           ,MAT,IZI,IZA,AWR,NEN,NEP,NXS,ERR,NS)
      NS=-1
      CALL WRIMF2(LOU,MXE,MXT,EIN,RWO(LXS),IWO(MTH)
     &           ,MAT,IZA,AWR,SPI,STF0,GAMG,D0LV,NEN,NXS,NS)
C* Write the ENDF file-3 data
      MF=3
      CALL WRIMF3(LOU,MXE,MXT,LXR,MF
     1           ,EIN,RWO(LXS),QQM,QQI,IWO(MTH),RWO(LSC)
     1           ,MAT,IZI,IZA,AWR,NEN,NEP,NXS,ERR,NS)
      WRITE(LTT,991)
      WRITE(LTT,995) ' Processed reaction cross sections    : ',NXS
      WRITE(LTT,995) '        Number of input energy points : ',NEN
      WRITE(LTT,991)
      WRITE(LER,991)
      WRITE(LER,995) ' Processed reaction cross sections    : ',NXS
      WRITE(LER,995) '        Number of input energy points : ',NEN
      WRITE(LER,991)
      DO I=1,NXS
        JT=IWO(MTH-1+I)
        IF(JT.GT.1000 .AND. JT.LE.4999) THEN
          JT=JT-1000*(JT/1000)
          WRITE(LTT,995) '       Cross section too small for MT : ',JT
          WRITE(LER,995) '       Cross section too small for MT : ',JT
        END IF
      END DO
      ELO=EIN(1)
C*
      WRITE(LTT,991)
      WRITE(LTT,991) ' List of MT numbers for MF3             '
      WRITE(LTT,999) (IWO(MTH-1+J),J=1,NXS)
      WRITE(LTT,991) ' List of MT numbers for MF4/MF6         '
      WRITE(LTT,998) (IWO(LBI-1+J),J=1,NT6)
      WRITE(LER,991)
      WRITE(LER,991) ' List of MT numbers for MF3             '
      WRITE(LER,999) (IWO(MTH-1+J),J=1,NXS)
      WRITE(LER,991) ' List of MT numbers for MF4/MF6         '
      WRITE(LER,998) (IWO(LBI-1+J),J=1,NT6)
C*
C* Stop processing if no energy/angle distributions present
      IF(NT6.LE.0) GO TO 880
      JT4=0
      JT6=0
C*
C* Process angular distribution data
      WRITE (LTT,991)
      WRITE (LTT,991) ' BEGIN PROCESSING ANGULAR DISTRIB.DATA  '
      WRITE (LER,991)
      WRITE (LER,991) ' BEGIN PROCESSING ANGULAR DISTRIB.DATA  '
C*
C* Read the EMPIRE output file to extract angular distributions
  400 JT6=JT6+1
      MT6=IWO(LBI-1+JT6)
C* Process discrete levels if continuum reactions present
      IF(MT6.NE.  2 .AND.
     1   MT6.NE. 91 .AND.
     1   MT6.NE.649 .AND.
     1   MT6.NE.849) GO TO 490
      LE=LBE
      LG=LE+NEN+2
      LA=LG+NEN+2
      LX=MXR-LA
      IF(LA.GT.MXR) STOP 'EMPEND ERROR - MXR limit exceeded'
      REWIND LIN
C* Disable ANGDIS printout for bad Legendre fits except for elastic
C* Discrete level distrib. checked with continuum when processing MF6
      JPRNT=IPRNT
c...
c...      JPRNT=-1
c...      IF(MT6.EQ.2) JPRNT=IPRNT
c...
C* Reading angular distributions - MF6 flagged negative
      KT6=-MT6
c...
c...      print *,'processing MT',MT6
c...
      CALL REAMF6(LIN,LTT,LER,EIN,RWO(LXS),NEN,RWO(LE),RWO(LG),RWO(LA)
     1           ,IWO(MTH),KT6,IZI,QQM,QQI,AWR,ELO,NXS,NK,LCT,MXE,LX
     2           ,JPRNT,EI1,EI2,EO1,EO2,NZA1,NZA2)
      IF(NK.LE.0) GO TO 490
C* Write the ENDF file-4 data
      MT4=2
  420 CALL WRIMF4(LOU,LTT,LER,IWO(MTH),QQM,QQI,NXS,MT6,RWO(LA)
     1           ,MT4,MAT,IZA,IZI,AWR,LCT,NS)
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
      IF(JT4.GT.0) THEN
        NS=0
        CALL WRCONT(LOU,MAT, 0, 0,NS,ZRO,ZRO, 0, 0, 0, 0)
      END IF
      JT6=0
      DO 492 I=1,NXS
      IWO(MTH-1+I)=ABS(IWO(MTH-1+I))
  492 CONTINUE
C*
C* Process double differential data
      WRITE(LTT,991)
      WRITE(LTT,991) ' BEGIN PROCESSING DOUBLE-DIFERENTL.DATA '
      WRITE(LER,991)
      WRITE(LER,991) ' BEGIN PROCESSING DOUBLE-DIFERENTL.DATA '
C* Check if yields for unassigned reactions need to be printed
      NK=0
      DO I=1,NT6
        IF(IWO(LBI-1+I).EQ.5) GO TO 600
        IF(IWO(LBI-1+I).EQ.201) NK=NK+1
        IF(IWO(LBI-1+I).EQ.203) NK=NK+1
        IF(IWO(LBI-1+I).EQ.207) NK=NK+1
      END DO
      CALL WRMF6Y(LOU,MXE,MXT,LXR
     1           ,EIN,RWO(LXS),QQM,QQI,IWO(MTH),RWO(LSC)
     1           ,MAT,IZI,IZA,AWR,NEN,NEP,NXS,ERR,NS)
      WRITE(LTT,995) ' Processed energ./ang. distrib. for MT: ',5
      WRITE(LTT,995) '         Number of outgoing particles : ',NK
      WRITE(LTT,991)
      WRITE(LER,995) ' Processed energ./ang. distrib. for MT: ',5
      WRITE(LER,995) '         Number of outgoing particles : ',NK
      WRITE(LER,991)
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
     1           ,IWO(MTH),MT6,IZI,QQM,QQI,AWR,ELO,NXS,NK,LCT,MXE,LX
     2           ,IPRNT,EI1,EI2,EO1,EO2,NZA1,NZA2)
c...
c...      print *,'Read data for',nk,' particles'
c...      print '(1p,10e12.3)',(rwo(la-1+j),j=1,120)
c...
      IF(NK.LE.0) GO TO 620
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
      IF(JT6.GT.0) THEN
        NS=0
        CALL WRCONT(LOU,MAT, 0, 0,NS,ZRO,ZRO, 0, 0, 0, 0)
      END IF
C*
C* Write radioisotope and isomer production data (MF10)
      WRITE(LTT,991)
      WRITE(LTT,991) ' BEGIN WRITING ACTIVATION DATA          '
      WRITE(LER,991)
      WRITE(LER,991) ' BEGIN WRITING ACTIVATION DATA          '
      N10=0
      NRC=0
      MF =10
      LIW=MTH+NXS
      LXI=MXI-LIW
      CALL WRMF10(LOU,MXE,MXT,LXI,LXR,MF
     1           ,EIN,RWO(LXS),QQM,QQI,IWO(MTH),IWO(LIW),RWO(LSC)
     1           ,MAT,IZI,IZA,AWR,NEN,NEP,NXS,ERR,N10,NRC,NS)
      WRITE(LTT,995) ' Number of MT sections written        : ',N10
      WRITE(LTT,995) '            Total number of reactions : ',NRC
      WRITE(LTT,991)
      WRITE(LER,995) ' Number of MT sections written        : ',N10
      WRITE(LER,995) '            Total number of reactions : ',NRC
      WRITE(LER,991)
C*
C* Process discrete level photon production
      WRITE(LTT,991)
      WRITE(LTT,991) ' BEGIN PROCESSING DISCRETE PHOTON DATA  '
      WRITE(LER,991)
      WRITE(LER,991) ' BEGIN PROCESSING DISCRETE PHOTON DATA  '
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
C* Select discrete level reactions to be processed
C* NOTE - Reactions going to ground states produce no photons
      DO L=1,MLV
        LVLF(L)=0
      END DO
      DO I=1,NXS
        IF     (IWO(MTH-1+I).GE. 51 .AND. IWO(MTH-1+I).LT. 91) THEN
C* Count levels of the MT50 series, set mt and IZP
          LVLF(1)=LVLF(1)+1
          LVMT(1)=51
          LVIZ(1)=1
        ELSE IF(IWO(MTH-1+I).GE.601 .AND. IWO(MTH-1+I).LT.649) THEN
C* Count levels of the MT600 series, set mt and IZP
          LVLF(2)=LVLF(2)+1
          LVMT(2)=601
          LVIZ(2)=1001
        ELSE IF(IWO(MTH-1+I).GE.801 .AND. IWO(MTH-1+I).LT.849) THEN
C* Count levels of the MT800 series, set mt and IZP
          LVLF(3)=LVLF(3)+1
          LVMT(3)=801
          LVIZ(3)=2004
        END IF
      END DO
C* Process selecte discrete level reactions
      DO L=1,MLV
        IF(LVLF(L).GT.0) THEN
          JZA=IZA+IZI-LVIZ(L)
          NL1=1
          MT0=LVMT(L)-NL1
          REWIND LIN
          CALL REMF12(LIN,LTT,LER,JZA,NLV,RWO(LEL),IWO(LNB),IWO(LLB)
     1               ,RWO(LBR),MXLI,MXLJ)
          CALL WRMF12(LOU,MAT,MT0,IZA,AWR,NLV,NL1,RWO(LEL)
     1               ,IWO(LNB),IWO(LLB),RWO(LBR),RWO(LSC),MXLI,MXLJ,NS)
          KLV=NLV-NL1
          WRITE(LTT,995) ' Processed discrete level photon prod.: ',MT0
          WRITE(LTT,995) '           Number of processed levels : ',KLV
          WRITE(LTT,991)
          WRITE(LER,995) ' Processed discrete level photon prod.: ',MT0
          WRITE(LER,995) '           Number of processed levels : ',KLV
          WRITE(LER,991)
          JLV=LVLF(L)
          IF(KLV.NE.JLV) THEN
          WRITE(LTT,995) ' REMF12 WARNING - expected No.of levels:',JLV
          WRITE(LER,995) ' REMF12 WARNING - expected No.of levels:',JLV
          END IF
          IF(NLV.GT.0) JPP=JPP+1
        END IF
      END DO
C* Photon production data from discrete levels processed
      IF(JPP.GT.0) THEN
        NS=0
        CALL WRCONT(LOU,MAT, 0, 0,NS,ZRO,ZRO, 0, 0, 0, 0)
      END IF
C* Process photon angular distribution for discrete levels (isotropic)
      JPP  =0
      DO L=1,MLV
        IF(LVLF(L).GT.0) THEN
          JZA=IZA+IZI-LVIZ(L)
          MT0=LVMT(L)
          JLV=LVLF(L)
          CALL WRMF14(LOU,MAT,MT0,IZA,AWR,JLV ,NS)
          JPP=JPP+1
        END IF
      END DO
C* Photon angular distributions from discrete levels processed
      IF(JPP.GT.0) THEN
        NS=0
        CALL WRCONT(LOU,MAT, 0, 0,NS,ZRO,ZRO, 0, 0, 0, 0)
      END IF
C* Write the ENDF material and file SEND records
      NS=0
      CALL WRCONT(LOU,  0, 0, 0,NS,ZRO,ZRO, 0, 0, 0, 0)
C*
  880 NS=0
      CALL WRCONT(LOU, -1, 0, 0,NS,ZRO,ZRO, 0, 0, 0, 0)
      STOP 'EMPEND Completed'
C*
  991 FORMAT(2A40)
  992 FORMAT(BN,I10)
  994 FORMAT(BN,F10.0)
  995 FORMAT(A40,4I5)
  996 FORMAT(A40,F10.4)
  997 FORMAT(6A11)
  998 FORMAT(10I5)
  999 FORMAT(10I8)
      END
      SUBROUTINE MTTOZA(IZI,IZA,JZA,MT)
C-Title  : Subroutine EMTIZA
C-Purpose: Given projectile IZI, target IZA, residual JZA, assign MT
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
      SUBROUTINE EMTIZA(IZI,IZA,JZA,MT)
C-Title  : Subroutine EMTIZA
C-Purpose: Given projectile IZI, target IZA, residual JZA, assign MT
      MT =0
C*
      IF(IZI.EQ.0) THEN
C* INCIDENT PHOTONS
        IF     (JZA  .EQ. IZA+IZI-   1) THEN
C* (g,n)
          MT = 50
        ELSE IF(JZA  .EQ. IZA+IZI-1004) THEN
C* (g,2nd) cross section
          MT = 11
        ELSE IF(JZA  .EQ. IZA+IZI-   2) THEN
C* (g,2n) cross section
          MT = 16
        ELSE IF(JZA  .EQ. IZA+IZI-   3) THEN
C* (g,3n) cross section
          MT = 17
        ELSE IF(JZA  .EQ. IZA+IZI-2005) THEN
C* (g,na) cross section
          MT = 22
        ELSE IF(JZA  .EQ. IZA+IZI-6013) THEN
C* (g,n3a) cross section
          MT = 23
        ELSE IF(JZA  .EQ. IZA+IZI-2006) THEN
C* (g,2na) cross section
          MT = 24
        ELSE IF(JZA  .EQ. IZA+IZI-2007) THEN
C* (g,3na) cross section
          MT = 25
        ELSE IF(JZA  .EQ. IZA+IZI-1002) THEN
C* (g,np) cross section
          MT = 28
        ELSE IF(JZA  .EQ. IZA+IZI-4009) THEN
C* (g,n2a) cross section
          MT = 29
        ELSE IF(JZA  .EQ. IZA+IZI-4010) THEN
C* (g,2n2a) cross section
          MT = 30
        ELSE IF(JZA  .EQ. IZA+IZI-1003) THEN
C* (g,nd) cross section
          MT = 32
        ELSE IF(JZA  .EQ. IZA+IZI-1004) THEN
C* (g,nt) cross section
          MT = 33
c... Conflicts with (a,a) reaction
c...        ELSE IF(JZA  .EQ. IZA+IZI-2004) THEN
c...C* (g,n+He3) cross section
c...          MT = 34
        ELSE IF(JZA  .EQ. IZA+IZI-5011) THEN
C* (g,nd2a) cross section
          MT = 35
        ELSE IF(JZA  .EQ. IZA+IZI-5012) THEN
C* (g,nt2a) cross section
          MT = 36
        ELSE IF(JZA  .EQ. IZA+IZI-   4) THEN
C* (g,4n) cross section
          MT = 37
        ELSE IF(JZA  .EQ. IZA+IZI-1003) THEN
C* (g,2np) cross section
          MT = 41
        ELSE IF(JZA  .EQ. IZA+IZI-1004) THEN
C* (g,3np) cross section
          MT = 42
        ELSE IF(JZA  .EQ. IZA+IZI-2003) THEN
C* (g,n2p) cross section
          MT = 44
        ELSE IF(JZA  .EQ. IZA+IZI-3006) THEN
C* (g,npa) cross section
          MT = 45
        ELSE IF(JZA  .EQ. IZA+IZI   -5) THEN
C* (g,5n) cross section
c...          MT = 47
        ELSE IF(JZA  .EQ. IZA+IZI     ) THEN
C* (g,g') radiative capture cross section
          MT =102
c...      MT =  3
        ELSE IF(JZA  .EQ. IZA+IZI-1001) THEN
C* (g,p) cross section
          MT =103
        ELSE IF(JZA  .EQ. IZA+IZI-1002) THEN
C* (g,d) cross section
          MT =104
        ELSE IF(JZA  .EQ. IZA+IZI-1003) THEN
C* (g,t) cross section
          MT =105
        ELSE IF(JZA  .EQ. IZA+IZI-2003) THEN
C* (g,He3) cross section
          MT =106
        ELSE IF(JZA  .EQ. IZA+IZI-2004) THEN
C* (g,a) cross section
          MT =107
        ELSE IF(JZA  .EQ. IZA+IZI-4008) THEN
C* (g,2a) cross section
          MT =108
        ELSE IF(JZA  .EQ. IZA+IZI-6012) THEN
C* (g,3a) cross section
          MT =109
        ELSE IF(JZA  .EQ. IZA+IZI-2002) THEN
C* (g,2p) cross section
          MT =111
        ELSE IF(JZA  .EQ. IZA+IZI-3005) THEN
C* (g,pa) cross section
          MT =112
        ELSE IF(JZA  .EQ. IZA+IZI-5011) THEN
C* (g,t2a) cross section
          MT =113
        ELSE IF(JZA  .EQ. IZA+IZI-5010) THEN
C* (g,d2a) cross section
          MT =114
        ELSE IF(JZA  .EQ. IZA+IZI-2003) THEN
C* (g,pd) cross section
          MT =115
        ELSE IF(JZA  .EQ. IZA+IZI-2004) THEN
C* (g,pt) cross section
          MT =116
        ELSE IF(JZA  .EQ. IZA+IZI-3006) THEN
C* (g,da) cross section
          MT =117
        END IF
      ELSE IF(IZI.EQ.1) THEN
C* INCIDENT NEUTRONS
        IF     (JZA  .EQ. IZA+IZI     ) THEN
C* Radiative capture cross section
          MT =102
        ELSE IF(JZA  .EQ. IZA+IZI   -1) THEN
C* Discrete levels inelastic scattering cross section
          MT =50
        ELSE IF(JZA  .EQ. IZA+IZI-1001) THEN
C* Discrete levels (n,p) cross section
          MT =600
        ELSE IF(JZA  .EQ. IZA+IZI-2004) THEN
C* Discrete levels (n,a) cross section
          MT =800
        ELSE IF(JZA  .EQ. IZA+IZI   -2) THEN
C* (n,2n) cross section
          MT =   16
        ELSE IF(JZA  .EQ. IZA+IZI   -3) THEN
C* (n,3n) cross section
          MT =   17
        ELSE IF(JZA  .EQ. IZA+IZI-2005) THEN
C* (n,na) cross section
          MT =   22
        ELSE IF(JZA  .EQ. IZA+IZI-2006) THEN
C* (n,2na) cross section
          MT =   24
        ELSE IF(JZA  .EQ. IZA+IZI-2007) THEN
C* (n,3na) cross section
          MT =   25
        ELSE IF(JZA  .EQ. IZA+IZI-1002) THEN
C* (n,np) cross section
          MT =   28
        ELSE IF(JZA  .EQ. IZA+IZI   -4) THEN
C* (n,4n) cross section
          MT =   37
        ELSE IF(JZA  .EQ. IZA+IZI-1003) THEN
C* (n,2np) cross section
          MT =   41
        ELSE IF(JZA  .EQ. IZA+IZI-1004) THEN
C* (n,3np) cross section
          MT =   42
        ELSE IF(JZA  .EQ. IZA+IZI-2003) THEN
C* (n,n2p) cross section
          MT =44
        ELSE IF(JZA  .EQ. IZA+IZI-3006) THEN
C* (n,npa) cross section
          MT =   45
        ELSE IF(JZA  .EQ. IZA+IZI   -5) THEN
C* (n,5n) cross section
C...          MT =   47
C... Conflicts with (n,2np)
C...    ELSE IF(JZA  .EQ. IZA+IZI-2003) THEN
C* (n,He3) cross section
C...          MT =  106
        ELSE IF(JZA  .EQ. IZA+IZI-4008) THEN
C* (n,2a) cross section
          MT =  108
        ELSE IF(JZA  .EQ. IZA+IZI-2002) THEN
C* (n,2p) cross section
          MT =  111
        ELSE IF(JZA  .EQ. IZA+IZI-3005) THEN
C* (n,pa) cross section
          MT =  112
        END IF
C*
      ELSE IF(IZI.EQ.1001) THEN
C* INCIDENT PROTONS
        IF     (JZA  .EQ. IZA+IZI  -1) THEN
C* Discrete level (p,n) cross section
          MT =50
        ELSE IF(JZA  .EQ. IZA+IZI  -2) THEN
C* (p,2nd) cross section
          MT =11
        ELSE IF(JZA  .EQ. IZA+IZI  -2) THEN
C* (p,2n) cross section
          MT =16
        ELSE IF(JZA  .EQ. IZA+IZI  -3) THEN
C* (p,3n) cross section
          MT =17
        ELSE IF(JZA  .EQ. IZA+IZI-2005) THEN
C* (p,na) cross section
          MT =22
        ELSE IF(JZA  .EQ. IZA-5012) THEN
C* (p,n3a) cross section
          MT =23
        ELSE IF(JZA  .EQ. IZA-1005) THEN
C* (p,2na) cross section
          MT =24
        ELSE IF(JZA  .EQ. IZA-1006) THEN
C* (p,3na) cross section
          MT =25
        ELSE IF(JZA  .EQ. IZA-   1) THEN
C* (p,np) cross section
          MT =28
        ELSE IF(JZA  .EQ. IZA-3008) THEN
C* (p,n2a) cross section
          MT =29
        ELSE IF(JZA  .EQ. IZA-3009) THEN
C* (p,2n2a) cross section
          MT =30
        ELSE IF(JZA  .EQ. IZA-   2) THEN
C* (p,nd) cross section
          MT =32
        ELSE IF(JZA  .EQ. IZA-   3) THEN
C* (p,nt) cross section
          MT =33
c... Conflicts with (n,a) reaction
c...        ELSE IF(JZA  .EQ. IZA-1003) THEN
c...C* (p,n+He3) cross section
c...          MT =34
        ELSE IF(JZA  .EQ. IZA-4010) THEN
C* (p,nd2a) cross section
          MT =35
        ELSE IF(JZA  .EQ. IZA-4011) THEN
C* (p,nt2a) cross section
          MT =36
        ELSE IF(JZA  .EQ. IZA- 997) THEN
C* (p,4n) cross section
          MT =37
        ELSE IF(JZA  .EQ. IZA-   2) THEN
C* (p,2np) cross section
          MT =41
        ELSE IF(JZA  .EQ. IZA-   3) THEN
C* (p,3np) cross section
          MT =42
        ELSE IF(JZA  .EQ. IZA-1002) THEN
C* (p,n2p) cross section
          MT =44
        ELSE IF(JZA  .EQ. IZA-2005) THEN
C* (p,npa) cross section
          MT =45
        ELSE IF(JZA  .EQ. IZA+IZI-5) THEN
C* (p,5n) cross section
C...          MT =   47
        ELSE IF(JZA  .EQ. IZA+1001) THEN
C* Radiative capture cross section
          MT =102
        ELSE IF(JZA  .EQ. IZA     ) THEN
C* Discrete levels (p,p) cross section
          MT =600
        ELSE IF(JZA  .EQ. IZA-   1) THEN
C* (p,d) cross section
          MT =104
        ELSE IF(JZA  .EQ. IZA-   2) THEN
C* (p,t) cross section
          MT =105
        ELSE IF(JZA  .EQ. IZA-1002) THEN
C* (p,He3) cross section
          MT =106
        ELSE IF(JZA  .EQ. IZA-1003) THEN
C* Discrete level (p,a) cross section
          MT =800
        ELSE IF(JZA  .EQ. IZA-3007) THEN
C* (p,2a) cross section
          MT =108
        ELSE IF(JZA  .EQ. IZA-5011) THEN
C* (p,3a) cross section
          MT =109
        ELSE IF(JZA  .EQ. IZA-1001) THEN
C* (p,2p) cross section
          MT =111
        ELSE IF(JZA  .EQ. IZA-2004) THEN
C* (p,pa) cross section
          MT =112
        ELSE IF(JZA  .EQ. IZA-4010) THEN
C* (p,t2a) cross section
          MT =113
        ELSE IF(JZA  .EQ. IZA-4009) THEN
C* (p,d2a) cross section
          MT =114
        ELSE IF(JZA  .EQ. IZA-1002) THEN
C* (p,pd) cross section
          MT =115
        ELSE IF(JZA  .EQ. IZA-1003) THEN
C* (p,pt) cross section
          MT =116
        ELSE IF(JZA  .EQ. IZA-2005) THEN
C* (p,da) cross section
          MT =117
        END IF
      ELSE IF(IZI.EQ.2004) THEN
C* INCIDENT ALPHAS
        IF     (JZA  .EQ. IZA+IZI-   1) THEN
C* Discrete level (a,n) cross section
          MT =50
        ELSE IF(JZA  .EQ. IZA+IZI-1004) THEN
C* (a,2nd) cross section
          MT =11
        ELSE IF(JZA  .EQ. IZA+IZI-   2) THEN
C* (a,2n) cross section
          MT =16
        ELSE IF(JZA  .EQ. IZA+IZI-   3) THEN
C* (a,3n) cross section
          MT =17
        ELSE IF(JZA  .EQ. IZA+IZI-2005) THEN
C* (a,na) cross section
          MT =22
        ELSE IF(JZA  .EQ. IZA+IZI-6013) THEN
C* (a,n3a) cross section
          MT =23
        ELSE IF(JZA  .EQ. IZA+IZI-2006) THEN
C* (a,2na) cross section
          MT =24
        ELSE IF(JZA  .EQ. IZA+IZI-2007) THEN
C* (a,3na) cross section
          MT =25
        ELSE IF(JZA  .EQ. IZA+IZI-1002) THEN
C* (a,np) cross section
          MT =28
        ELSE IF(JZA  .EQ. IZA+IZI-4009) THEN
C* (a,n2a) cross section
          MT =29
        ELSE IF(JZA  .EQ. IZA+IZI-4010) THEN
C* (a,2n2a) cross section
          MT =30
        ELSE IF(JZA  .EQ. IZA+IZI-1003) THEN
C* (a,nd) cross section
          MT =32
        ELSE IF(JZA  .EQ. IZA+IZI-1004) THEN
C* (a,nt) cross section
          MT =33
c... Conflicts with (a,a) reaction
c...        ELSE IF(JZA  .EQ. IZA+IZI-2004) THEN
c...C* (a,n+He3) cross section
c...          MT =34
        ELSE IF(JZA  .EQ. IZA+IZI-5011) THEN
C* (a,nd2a) cross section
          MT =35
        ELSE IF(JZA  .EQ. IZA+IZI-5012) THEN
C* (a,nt2a) cross section
          MT =36
        ELSE IF(JZA  .EQ. IZA+IZI-   4) THEN
C* (a,4n) cross section
          MT =37
        ELSE IF(JZA  .EQ. IZA+IZI-1003) THEN
C* (a,2np) cross section
          MT =41
        ELSE IF(JZA  .EQ. IZA+IZI-1004) THEN
C* (a,3np) cross section
          MT =42
        ELSE IF(JZA  .EQ. IZA+IZI-2003) THEN
C* (a,n2p) cross section
          MT =44
        ELSE IF(JZA  .EQ. IZA+IZI-3006) THEN
C* (a,npa) cross section
          MT =45
        ELSE IF(JZA  .EQ. IZA+IZI   -4) THEN
C* (a,4n) cross section
          MT =   47
        ELSE IF(JZA  .EQ. IZA+IZI     ) THEN
C* (a,g) radiative capture cross section
          MT =102
        ELSE IF(JZA  .EQ. IZA+IZI-1001) THEN
C* (a,p) cross section
          MT =103
        ELSE IF(JZA  .EQ. IZA+IZI-1002) THEN
C* (a,d) cross section
          MT =104
        ELSE IF(JZA  .EQ. IZA+IZI-1003) THEN
C* (a,t) cross section
          MT =105
        ELSE IF(JZA  .EQ. IZA+IZI-2003) THEN
C* (a,He3) cross section
          MT =106
        ELSE IF(JZA  .EQ. IZA+IZI-2004) THEN
C* Discrete level (a,a) cross section
          MT =800
        ELSE IF(JZA  .EQ. IZA+IZI-4008) THEN
C* (a,2a) cross section
          MT =108
        ELSE IF(JZA  .EQ. IZA+IZI-6012) THEN
C* (a,3a) cross section
          MT =109
        ELSE IF(JZA  .EQ. IZA+IZI-2002) THEN
C* (a,2p) cross section
          MT =111
        ELSE IF(JZA  .EQ. IZA+IZI-3005) THEN
C* (a,pa) cross section
          MT =112
        ELSE IF(JZA  .EQ. IZA+IZI-5011) THEN
C* (a,t2a) cross section
          MT =113
        ELSE IF(JZA  .EQ. IZA+IZI-5010) THEN
C* (a,d2a) cross section
          MT =114
        ELSE IF(JZA  .EQ. IZA+IZI-2003) THEN
C* (a,pd) cross section
          MT =115
        ELSE IF(JZA  .EQ. IZA+IZI-2004) THEN
C* (a,pt) cross section
          MT =116
        ELSE IF(JZA  .EQ. IZA+IZI-3006) THEN
C* (a,da) cross section
          MT =117
        END IF
      END IF
      RETURN
      END
      SUBROUTINE EMTCHR(PTST,MT,IZI)
C-Title  : Subroutine EMTCHR
C-Purpose: Assign MT number from reaction string
      CHARACTER*8 PTST
C* Force incident particle designation "z" for backward compatibility
      PTST(3:3)='z'
      MT=0
C* Assign MT numbers
        IF(PTST.EQ.' (z,x)  ') MT=  5
        IF(PTST.EQ.' (z,2n) ') MT= 16
        IF(PTST.EQ.' (z,3n) ') MT= 17
        IF(PTST.EQ.' (z,fiss') MT= 18
        IF(PTST.EQ.' (z,np) ') MT= 28
        IF(PTST.EQ.' (z,na) ') MT= 22
        IF(PTST.EQ.' (z,2na)') MT= 24
        IF(PTST.EQ.' (z,4n) ') MT= 37
        IF(PTST.EQ.' (z,2np)') MT= 41
        IF(PTST.EQ.' (z,3np)') MT= 42
c...        IF(PTST.EQ.' (z,5n) ') MT= 47
        IF(PTST.EQ.' (z,n)  ') MT= 91
c...        IF(PTST.EQ.' (z,n)  ') THEN
c...          IF(IZI.EQ.1) THEN
c...            MT= 91
c...          ELSE
c...            MT=  4
c...          END IF
c...        END IF
        IF(PTST.EQ.' (z,gamm') MT=102
        IF(PTST.EQ.' (z,2p) ') MT=111
        IF(PTST.EQ.' (z,p)  ') THEN
          IF(IZI.EQ.1) THEN
            MT=649
          ELSE
            MT=103
          END IF
        END IF
        IF(PTST.EQ.' (z,a)  ') THEN
          IF(IZI.EQ.1) THEN
            MT=849
          ELSE
            MT=107
          END IF
        END IF
      RETURN
      END
      SUBROUTINE QVALUE(IMT,MT,IZA,IZB,BEN,QQM)
C-Title  : Subroutine QVALUE
C-Purpose: Reconstruct Q-value from reaction and binding energies
      DIMENSION  IZB(IMT),BEN(3,IMT)
      QQM=0
      IF(IMT.LE.0) RETURN
        DO I=1,IMT
          KZA=IZB(I)
C* Consider the radiative capture reaction
          IF(MT.EQ. 102 .AND. KZA.EQ.IZA+1) QQM=1.E6*BEN(1,I)
C* Consider reactions after first neutron emission:
          IF(KZA.EQ.IZA) THEN
C*      Neutron emission: (n,2n) (n,3n) (n,2n+a) (n,2n+p)
            IF(MT.EQ.16 .OR. MT.EQ.17 .OR. MT.EQ.24 .OR. MT.EQ.41)
     1        QQM=QQM-1.E6*BEN(1,I)
C*      Proton emission (n,n+p) (n,n+p+a)
            IF(MT.EQ.28 .OR. MT.EQ.45)
     1        QQM=QQM-1.E6*BEN(2,I)
C*      Alpha emission (n,n+a)
            IF(MT.EQ.22)
     1        QQM=QQM-1.E6*BEN(3,I)
C* Consider reactions after two-neutrons emission:
          ELSE IF(KZA.EQ.IZA-1) THEN
C*      Neutron emission: (n,3n)
            IF(MT.EQ.17)
     1        QQM=QQM-1.E6*BEN(1,I)
C*      Proton emission (n,2n+p)
            IF(MT.EQ.41)
     1        QQM=QQM-1.E6*BEN(2,I)
C*      Alpha emission (n,2n+a)
            IF(MT.EQ.24)
     1        QQM=QQM-1.E6*BEN(3,I)
C* Consider reactions after three-neutrons emission:
          ELSE IF(KZA.EQ.IZA-2) THEN
C*      Neutron emission: (n,4n)
            IF(MT.EQ.37)
     1        QQM=QQM-1.E6*BEN(1,I)
C*      Proton emission: (n,3n+p)
            IF(MT.EQ.42)
     1        QQM=QQM-1.E6*BEN(2,I)
C*      Alpha emission: (n,3n+a)
            IF(MT.EQ.23)
     1        QQM=QQM-1.E6*BEN(3,I)
C* Consider reactions after neutron and proton emission:
          ELSE IF(KZA.EQ.IZA-1001) THEN
C*      Alpha emission: (n,n+p+a)
            IF(MT.EQ.45)
     1        QQM=QQM-1.E6*BEN(3,I)
C* Consider reactions after neutron capture:
          ELSE IF(KZA.EQ.IZA+1) THEN
C*      Proton emission (n,p) (n,p+a)
            IF(MT.EQ.103 .OR. MT.EQ.112)
     1        QQM=QQM-1.E6*BEN(2,I)
C* Consider reactions after neutron capture and proton emission:
          ELSE IF(KZA.EQ.IZA+1-1001) THEN
C*      Alpha emission (n,p+a)
            IF(MT.EQ.112)
     1        QQM=QQM+1.E6*(BEN(1,I)-BEN(3,I))
          END IF
        END DO
      RETURN
      END
      SUBROUTINE POUCHR(PTST,KZAK,AWP)
C-Title  : Subroutine POUCHR
C-Purpose: Assign light particle ZA and AWR from character string
C-Description:
C-D  The input string PTST identifies the light particle.
C-D  If KZAK>0, it identifies the particle ZA.
C-D  On output KZAK is the particle ZA designation and AWP is its
C-D  atomic weight ratio relative to the neutron.
C-D  Special cases:
C-D    KZAP=999999  for recoils (data must be define externally)
C-D    KZAP<0       for unrecognised particles.
C-
      CHARACTER*8  PTST
C* Particle masses (neutron, proton, deuteron, triton, He-3, alpha)
      COMMON /PMASS/ AWN,AWH,AWD, AWT, AW3,AWA
C*
      IZAK=KZAK
      IF      (PTST.EQ.'neutrons' .OR. IZAK.EQ.   1) THEN
        KZAK=1
        AWP =1
      ELSE IF (PTST.EQ.'gammas  ' .OR. IZAK.EQ.   0) THEN
        KZAK=0
        AWP =0
      ELSE IF (PTST.EQ.'protons ' .OR. IZAK.EQ.1001) THEN
        KZAK=1001
        AWP =AWH/AWN
      ELSE IF (PTST.EQ.' 2-d    ' .OR. IZAK.EQ.1002) THEN
        KZAK=1002
        AWP =AWD/AWN
      ELSE IF (PTST.EQ.'alphas  ' .OR. IZAK.EQ.2004) THEN
        KZAK=2004
        AWP =AWA/AWN
      ELSE IF (PTST.EQ.'recoils ') THEN
        KZAK=999999
        AWP =999999
      ELSE
C* Unidentified particle
        KZAK=-1
        AWP =-1
      END  IF
      RETURN
      END
      SUBROUTINE YLDPOU(YI,MT,KZAP)
C-Title  : Subroutine YLDPOU
C-Purpose: Define yield YI of particla KZAP in reaction MT
C-Author : A. Trkov
C-Reference: Common routine to EMPEND and DXSEND
C-Description:
C-D  Multiplicity YI of the particle with ZA designation KZAP for a
C-D  reaction MT is given with the following convention:
C-D    YI > 0  Multiplicity for the reaction is fixed and equal to YI
C-D       = 0  Particles may be produced by the reaction, but the
C-D            multiplicity has to be obtained from other sources
C-D       < 0  Particle cannot be produced from this reaction
C-D  
      YI=-1
      IF     (KZAP.EQ.   1) THEN
C* Outgoing neutrons
        IF(MT.EQ. 3 .OR. MT.EQ. 5 .OR.
     &    (MT.GE.18.AND. MT.LE.21).OR. MT.EQ.38) YI=0
        IF(MT.EQ. 2 .OR. MT.EQ. 4 .OR. MT.EQ.18 .OR.
     &     MT.EQ.22 .OR. MT.EQ.23 .OR.
     &    (MT.GE.28.AND. MT.LE.29).OR.
     &    (MT.GE.32.AND.MT.LE.36) .OR. MT.EQ.44 .OR. MT.EQ.45 .OR.
     &    (MT.GE.50.AND.MT.LE.91)) YI=1
        IF(MT.EQ.11 .OR. MT.EQ.16 .OR. MT.EQ.24 .OR.
     &     MT.EQ.30 .OR. MT.EQ.41) YI=2
        IF(MT.EQ.17 .OR. MT.EQ.25 .OR. MT.EQ.42) YI=3
        IF(MT.EQ.37) YI=4
        IF(MT.EQ.47) YI=5
      ELSE IF(KZAP.EQ.1001) THEN
C* Outgoing protons
        IF(MT.EQ. 5) YI=0
        IF(MT.EQ.28  .OR. (MT.GE.41 .AND.MT.LE.42) .OR.
     &     MT.EQ.45  .OR. MT.EQ.103 .OR. MT.EQ.112 .OR.
     &    (MT.GE.600.AND. MT.LE.649)) YI=1
        IF(MT.EQ.44 .OR. MT.EQ.111) YI=2
      ELSE IF(KZAP.EQ.1002) THEN
C* Outgoing deuterons
        IF(MT.EQ. 5) YI=0
        IF(MT.EQ.11 .OR. MT.EQ.32 .OR. MT.EQ.35 .OR. MT.EQ.104) YI=1
      ELSE IF(KZAP.EQ.2003) THEN
C* Outgoing He-3
        IF(MT.EQ. 5) YI=0
        IF(MT.EQ.34 .OR. MT.EQ.106) YI=1
      ELSE IF(KZAP.EQ.2004) THEN
C* Outgoing alphas
        IF(MT.EQ. 5) YI=0
        IF(MT.EQ. 22 .OR. MT.EQ.24 .OR. MT.EQ.25 .OR. MT.EQ.45 .OR.
     &     MT.EQ.107 .OR. MT.EQ.112.OR.(MT.GE.800.AND.MT.LE.849)) YI=1
        IF(MT.EQ.29 .OR. MT.EQ.30 .OR. MT.EQ.35 .OR. MT.EQ.36 .OR.
     &     MT.EQ.108) YI=2
        IF(MT.EQ.109) YI=3
      ELSE IF(KZAP.EQ.0) THEN
C* Outgoing photons
        YI=0
      ELSE
C* Recoils
        IF(MT.EQ.5) THEN
          YI=0
        ELSE
          YI=1
        END IF
      END IF
      RETURN
      END
      SUBROUTINE CHKFIS(NXS,NEN,MTH,XSR,MXE,MXT,LFI)
C-Title  : Subroutine CHKFIS
C-Purpose: Check for non-zero fission cross section
C-Description:
C-D  Scan cross sections in array XSR(i,j) given at energies Ei
C-D  for reaction MTH(j) equal 18. If non-zero cross sections are
C-D  encountered, set flag LFI=1, otherwise, LFI=0.
C-
      DIMENSION  MTH(MXT),XSR(MXE,MXT)
      LFI=0
      DO I=1,NXS
        IF(MTH(I).EQ.18) THEN
C*        Fission reaction found
          DO J=1,NEN
C*          Check for non-zero fission cross section
            IF(XSR(J,I).GT.0) LFI=1
          END DO
        END IF
      END DO
      RETURN
      END
      SUBROUTINE SUMMF5(IZI,IZA,NXS,NPT,MTH,NT6,MT6,XSR,QQM,QQI,MXE,MXT)
C-Title  : Subroutine SUMMF5
C-Purpose: Sum reactions contributing to MT 5
C-Version:
C-V  07/10 Extensive changes, change convention for MT5 summation
C-Description:
C-D Scan all MT reaction values in MTH for MT>5
C-D Compare with MT values in MT6 for reactions having differential data
C-D If no differential data are present, change MT=10*ZAP+LFS where ZAP
C-D is the reaction product ZA designation and LFS=5. This will ensure
C-D that the reaction cross section will also be processable in MF10.
C-D The exception are discrete level reactions, which do not necessarily
C-D require differential data (if isotropic distributions are assumed).
C-D   All reactions with MT>9999 and LFS>4 (if any) will be added to
C-D MT5. Furthermore, all reactions are scanned to construct special
C-D particle production cross sections MT201, 203 and 207.
C-
      DIMENSION  MTH(MXT),MT6(MXT),XSR(MXE,MXT),QQM(MXT),QQI(MXT)
C* Process only if NT6>0
      IF(NT6.LE.0) RETURN
      MT5 =0
      I5  =0
      I201=0
      I203=0
      I207=0
      DO IX=1,NXS
        MT=MTH(IX)
        IF(MT.EQ.  5) I5  =IX
        IF(MT.EQ.201) I201=IX
        IF(MT.EQ.203) I203=IX
        IF(MT.EQ.207) I207=IX
C* Consider eligible reactions
        IF((MT.GT.  5 .AND. MT.LT. 50 .AND. MT.NE.18) .OR.
     &     (MT.GE. 91 .AND. MT.LT.200).OR.
     &     (MT.EQ.649 .OR.  MT.EQ.699 .OR.
     &      MT.EQ.749 .OR.  MT.EQ.799 .OR. MT.EQ.849) ) THEN
C* Compare MT values in MT6 for reactions having differential data
          M5=1
          DO J6=1,NT6
            IF(MT.EQ.MT6(J6)) M5=0
          END DO
          IF(M5.EQ.1) THEN
C* MT with no differential data flagged for summation into MT5
C* and inclusion in MF10 (set LFS to 5)
            CALL MTTOZA(IZI,IZA,JZA,MT)
            PRINT *,'IZI,IZA,JZA,MT',IZI,IZA,JZA,MT
            IF(JZA.GT.0) MT=10*JZA+5
C           MT=-MT
            MTH(IX)=MT
          END IF
        END IF
C* Count any other reactions that need to be added to MT 5
        IF(MT.GT.9999) THEN
          M5=MT-10*(MT/10)
          IF(M5.GE.5) MT5=MT5+1
        END IF
      END DO
      IF(MT5.LE.0) RETURN
      IF(NXS+3.GT.MXT) STOP 'EMPEND ERROR - MXT limit exceeded'
C* Create MT 5 if not present
      IF(I5.LE.0) THEN
        NXS=NXS+1
        MTH(NXS)=5
        I5 =NXS
        DO J=1,NPT
          XSR(J,I5)=0
        END DO
        QQM(I5)=-1.E12
      END IF
C* Create MT 201 if not present (flagged -ve for no spectra)
      IF(I201.LE.0) THEN
        NXS=NXS+1
        MTH(NXS)=-201
        I201=NXS
        DO J=1,NPT
          XSR(J,I201)=0
        END DO
        QQM(I201)=-1.E12
      END IF
C* Create MT 203 if not present (flagged -ve for no spectra)
      IF(I203.LE.0) THEN
        NXS=NXS+1
        MTH(NXS)=-203
        I203=NXS
        DO J=1,NPT
          XSR(J,I203)=0
        END DO
        QQM(I203)=-1.E12
      END IF
C* Create MT 207 if not present (flagged -ve for no spectra)
      IF(I207.LE.0) THEN
        NXS=NXS+1
        MTH(NXS)=-207
        I207=NXS
        DO J=1,NPT
          XSR(J,I207)=0
        END DO
        QQM(I207)=-1.E12
      END IF
C* Add contribution of reactions without differential data to MT5
      DO IX=1,NXS
        MT=MTH(IX)
        M5=0
        IF(MT.GT.9999) THEN
          M5=MT-10*(MT/10)
          IF(M5.GE.5) THEN
            DO J=1,NPT
              XSR(J,I5)=XSR(J,I5)+XSR(J,IX)
            END DO
            QQM(I5)=MAX(QQM(IX),QQM(I5))
            QQI(I5)=QQM(I5)

            print *,' Addint to MT5 production of ZA/Q',MT/10,QQM(I5)

          END IF
        END IF
C.        IF(MT.LT.0 .AND.
C.     &     (-MT.LT.201 .OR. -MT.GT.207) .AND.
C.     &     (-MT.LT.450 .OR. -MT.GE.460)) THEN
C.C...
C.          print *,' Adding to MT5 contribution from MT/Q',MT,QQM(IX)
C.C...
        KM=MT
        IF(MT.GT.9999) KM=MT/10
C* Add alpha production to MT207
   20   IF(KM.EQ. 22.OR. KM.EQ. 23.OR. KM.EQ. 24.OR.
     &     KM.EQ. 25.OR. KM.EQ. 29.OR. KM.EQ. 30.OR.
     &     KM.EQ. 35.OR. KM.EQ. 36.OR. KM.EQ. 45.OR.
     &     KM.EQ.107.OR. KM.EQ.108.OR. KM.EQ.109.OR.
     &     KM.EQ.112.OR.(KM.GT.999 .AND. M5.GE.5)) THEN
          IF(KM.LE.999) THEN
            KZAP=2004
            CALL YLDPOU(YI,KM,KZAP)
          ELSE
            KM=KM-2004
            YI=1
          END IF
          DO J=1,NPT
            XSR(J,I207)=XSR(J,I207)+YI*XSR(J,IX)
          END DO
          QQM(I207)=MAX(QQM(IX),QQM(I207))
          QQI(I207)=QQM(I207)
        END IF
        IF(KM.GT.IZA+IZI) GO TO 20
C* Add proton production to MT203
   40   IF(KM.EQ. 28.OR. KM.EQ. 41.OR. KM.EQ.42 .OR.
     &     KM.EQ. 44.OR. KM.EQ. 45.OR.
     &     KM.EQ.103.OR. KM.EQ.111.OR. KM.EQ.112.OR.
     &     KM.EQ.115.OR. KM.EQ.116.OR.(KM.GT.999 .AND. M5.GE.5)) THEN
          IF(KM.LE.999) THEN
            KZAP=1001
            CALL YLDPOU(YI,KM,KZAP)
          ELSE
            KM=KM-1001
            YI=1
          END IF
          DO J=1,NPT
            XSR(J,I203)=XSR(J,I203)+YI*XSR(J,IX)
          END DO
          QQM(I203)=MAX(QQM(IX),QQM(I203))
          QQI(I203)=QQM(I203)
        END IF
        IF(KM.GT.IZA+IZI) GO TO 40
C* Add neutron production to MT201
   60   IF(KM.EQ.11  .OR. KM.EQ.16 .OR. KM.EQ.17 .OR.
     &    (KM.GE.22 .AND. KM.LE.25).OR.
     &    (KM.GE.28 .AND. KM.LE.49).OR.
     &    (KM.GT.999.AND. M5.GE.5)) THEN
          IF(KM.LE.999) THEN
            KZAP=1
            CALL YLDPOU(YI,KM,KZAP)
          ELSE
            KM=KM-1
            YI=1
          END IF
          DO J=1,NPT
            XSR(J,I201)=XSR(J,I201)+YI*XSR(J,IX)
          END DO
          QQM(I201)=MAX(QQM(IX),QQM(I201))
          QQI(I201)=QQM(I201)
        END IF
      END DO
      RETURN
      END
      SUBROUTINE SCNMF6(LIN,NT6,MTH,MXI,IZI)
C-Title  : SCNMF6 Subroutine
C-Purpose: Scan EMPIRE output for all react. with energy/angle distrib.
      CHARACTER*136 REC
      DIMENSION MTH(1)
C*
      NT6=0
  110 READ (LIN,891,END=200) REC
C* Test for elastic angular distributions of neutral particles
      IF(IZI.LT.1000 .AND. REC(1:14).EQ.'  Elastic angu'    ) THEN
        MT=2
        GO TO 120
      END IF
      IF(REC(1:14).NE.'  Spectrum of '    ) GO TO 110
C* Identify the reaction and assign the MT number
      CALL EMTCHR(REC(23:30),MT,IZI)
      IF(MT .EQ.0) GO TO 110
C* Spectrum printout encountered - begin processing
  120 IF(NT6.GT.0) THEN
C* Check if already processed
        DO I=1,NT6
          IF(MTH(I).EQ.MT) GO TO 110
        END DO
      END IF
      NT6=NT6+1
      IF(NT6.GT.MXI) STOP 'SCNMF6 ERROR - MXI limit exceeded'
      MTH(NT6)=MT
      GO TO 110
C*
C* File processed - sort MT numbers in ascending order
  200 IF(NT6.LT.2) RETURN
  210 ISW=0
      DO I=2,NT6
        IF(MTH(I-1).GT.MTH(I)) THEN
          MM=MTH(I)
          MTH(I  )=MTH(I-1)
          MTH(I-1)=MM
          ISW=1
        END IF
      END DO
      IF(ISW.NE.1) RETURN
      GO TO 210
C*
  802 FORMAT(I3,1X,A2,1X,I3)
  891 FORMAT(A136)
      END
      SUBROUTINE RDANGF(LIN,NEN,NAN,RWO,MXR,ANG,MXA,MT,ZAP,LTT,LER)
C-Title  : Subroutine RDANGF
C-Purpose: Read angular distributions
C-Description:
C-D The correlated energy/angle double differential cross sections from
C-D EMPIRE output are read into the work array. The following conditioning
C-D of the data is performed:
C-D  - Multiple energy points with zero distribution are skipped
C-D    unless they are from discrete level data.
C-D  - Zero distributions over limited range of angles are forced
C-D    to half the value of the neighbours until all are positive.
C-D  - If the distribution is angle-integrated, it is divided by 4*Pi
C-D    for consistency to simplify post-processing.
C-D
C-D Formal parameters have the following meaning:
C-D  LIN  Logical unit number of the EMPIRE short output.
C-D  NEN  Number of outfoing particle energies.
C-D  NAN  Number of angles at which the distribution is tabulated
C-D  RWO  Work array, which contains on exit a packed matrix of
C-D       dimensions RWO(LHI+2,NPT).
C-D       Each of the NEN rows contains:
C-D        - outgoing particle energy
C-D        - NAN points of the distribution at angles ANG.
C-D  MXR  Maximum size of the work array RWO.
C-D  ANG  Cosines of angles at which the distributions are tabulated.
C-D  MXA  Maximum number of cosines.
C-D  MT   ENDF reaction designation.
C-D  ZAP  ZA designation of the outgoing particle
C-
C* Maximum number of angles MDA, local array.
      PARAMETER     (MDA=120)
      DOUBLE PRECISION DD(MDA),DDJ,PI,FF
      CHARACTER*136  REC
      DIMENSION      ANG(MXA)
      DIMENSION      RWO(MXR)
C* Permissible tolerance for interpolated angular distributions (fraction)
      DATA ETOL/ 0.010 /
C*
      DATA PI/3.1415926D0/
C*
      IZA=NINT(ZAP)
      NEN=0
      LD =1
      NZZ=0
      EOO=-1.E12
C-F Check if angles are given (No. of angles NAN=1 if isotropic)
      READ (LIN,891) REC
      IF(REC(1:40).EQ.'                                        ') THEN
        FF =1/(4*PI)
        NAN=1
      ELSE
C-F Read the angles at which the distributions are given (8 per row)
        FF =1
        NAN=8
        J1 =1
   20   READ (REC,806,ERR=802) (ANG(J),J=J1,NAN)
        IF(ANG(NAN).GT.0 .AND. NINT(ANG(NAN)).LT.180) THEN
          J1=NAN+1
          NAN=NAN+8
          IF(NAN.GT.MXA) STOP 'RDANGF ERROR - MXA limit exceeded'
          IF(NAN.GT.MDA) STOP 'RDANGF ERROR - MDA limit exceeded'
          READ (LIN,891) REC
          GO TO 20
        END IF
        DO WHILE (ANG(NAN).EQ.0 .AND.NAN.GT.1)
          NAN=NAN-1
        END DO
C* Convert angles from degrees to cosines
        DO J=1,NAN
          ANG(J)=COS(PI*ANG(J)/180)
        END DO
      END IF
C*
C-F Read angular distributions until a blank line is encountered
   40 READ (LIN,891) REC
      IF(REC(1:20).EQ.'                    ') GO TO 80
      JXA=MIN(NAN,8)
C* Read distribution in double precision to avoid underflow
      READ (REC,807,ERR=802) EE,(DD(J),J=1,JXA)
      IF(NAN.GT.8) READ (LIN,809,ERR=802) (DD(J),J=9,NAN)
C* Suppress negative energies (unless processing discrete data)
      IF(MT.GT.0 .AND. EE.LT.0) GO TO 40
      IF(LD+1+NAN.GT.MXR) STOP 'RDANGF ERROR - MXR limit exceeded'
      EOU=EE*1.E6
C*    -- Shift duplicate energies
      IF(EOU.LE.EOO) EOU=EOU*1.00001
      EOO=EOU
      RWO(LD)=EOU
C* Check for zero or negative distributions
      NEG=0
      NEP=0
      KZE=0
      DO J=1,NAN
        DDJ=DD(J)
        IF(DDJ.GT.0) THEN
C*          Count non-zero distributions
          NEP=NEP+1
          KZE=0
        ELSE
          DDJ=0
C*          Count all zero-distribution entries
          NEG=NEG+1
C*          Count trailing zero-distribution entries at backward angles
          KZE=KZE+1
        END IF
        RWO(LD+J)=DDJ*FF
      END DO
C* Mark point with all-zero distributions (except if discrete level)
      IF(NEP.EQ. 0) THEN
        NEG=0
        IF(EOU.GE.0) NZZ=NZZ+1
      ELSE
        NZZ=0
      END IF
      IF(NEG.GT. 0) THEN
C* Force zero points to half of the average of neighbours until all >0
        IF(NAN.GT.2) THEN
          KZERO=1
          DO WHILE (KZERO.GT.0)
            KZERO=0
            DO J=3,NAN
              IF(RWO(LD+J-1).LE.0) THEN
                KZERO=1
                DDJ=MAX(1.E-30, (RWO(LD+J-2)+RWO(LD+J))/2)
                IF(DDJ.GT.1E-20) DDJ=DDJ/2
                RWO(LD+J-1)=DDJ
              END IF
            END DO
          END DO
        END IF
C*          Print warning on negative distributions
C...    WRITE(LTT,906) MT,NINT(ZAP),EIN,EOU,NEG,NAN
C...    WRITE(LER,906) MT,NINT(ZAP),EIN,EOU,NEG,NAN
      END IF
C* Check for multiple zero distributions
      IF(NZZ.GT.2) THEN
        EOU=RWO(LD)
        RWO(LD-1-NAN)=EOU
        NZZ=NZZ-1
      ELSE
        LD=LD+1+NAN
        NEN=NEN+1
      END IF
      GO TO 40
C*
C* Check that the last point is a zero distribution
   80 IF(NZZ.GT.1) NEN=NEN-(NZZ-1)
      IF(NEN.GT.1 .AND. NZZ.LT.1) THEN
        NEN=NEN+1
        RWO(LD)=EOU*1.00001
        DO J=1,NAN
          RWO(LD+J)=0
        END DO
      END IF
c...
c...    if(mt.eq. 5) then
c...      print *,'direct mt,za',mt,nint(zap),nen
c...      ld=1
c...      jm=min(nan+1,5)
c...      do i=1,nen
c...        print *,(rwo(ld-1+j),j=1,jm)
c...        ld=ld+nan+1
c...      end do
c...    end if
c...
      IF(NEN.LT.3) GO TO 82
C*
C* Remove points that can be reproduced by linear interpolation
      LD1=1
      LD2=LD1+1+NAN
      LD3=LD2+1+NAN
      NDEL=0
      DO I=3,NEN
        E1=RWO(LD1)
        E2=RWO(LD2)
        E3=RWO(LD3)
        ISTAY=1
C*    --Check point 2 (excluding discrete levels)
        IF(E2.GT.0) THEN
          ISTAY=0
          DO J=1,NAN
            F1=RWO(LD1+J)
            F2=RWO(LD2+J)
            F3=RWO(LD3+J)
            FI=F1+(E2-E1)*(F3-F1)/(E3-E1)
            IF(F2.GT.0) THEN
              DF= ABS(FI/F2-1)
              IF(DF.GT.ETOL) ISTAY=1
            ELSE
              ISTAY=1
            END IF
          END DO
        END IF
        IF(ISTAY.EQ.1) THEN
C*      --Point 2 stays, move points 2 and 3 and redefine
          JD1=LD1+1+NAN
          JD2=JD1+1+NAN
          RWO(JD1)=E2
          RWO(JD2)=E3
          DO J=1,NAN
            RWO(JD1+J)=RWO(LD2+J)
            RWO(JD2+J)=RWO(LD3+J)
          END DO
          LD1=JD1
          LD2=JD2
        ELSE
C*      --Point 2 to be removed, rename 3 to 2
c...
c...          print *,'remove mt,za,eo',mt,nint(zap),e2
c...
          LD2=LD3
          E2 =E3
          NDEL=NDEL+1
        END IF
C*    --Process next point
        IF(I.LT.NEN) LD3=LD3+1+NAN
      END DO
C*  --Move the last point
      JD1=LD1+1+NAN
      JD2=JD1+1+NAN
      RWO(JD1)=E2
      RWO(JD2)=E3
      DO J=1,NAN
        RWO(JD1+J)=RWO(LD2+J)
        RWO(JD2+J)=RWO(LD3+J)
      END DO
      NEN=NEN-NDEL
C*
C* All processing completed
c...
c...    if(mt.eq. 5) then
c...      print *,'adjusted mt,za',mt,nint(zap),nen
c...      ld=1
c...      jm=min(nan+1,5)
c...      do i=1,nen
c...        print *,(rwo(ld-1+j),j=1,jm)
c...        ld=ld+nan+1
c...      end do
c...    if(ndel.gt.0)  stop
c...    end if
c...
   82 RETURN
C*
C* Error trap reading input record
  802 WRITE(LTT,912) REC
      WRITE(LER,912) REC
      STOP 'EMPEND ERROR - reading file'
C*
  806 FORMAT(6X,8(5X,F10.4))
  807 FORMAT(BN,F10.5,F14.4,7F15.4)
  809 FORMAT(9X,8F15.4)
  891 FORMAT(A136)
  906 FORMAT(' EMPEND WARNING - MT',I4,' IZA',I5
     1      ,' Ein',1P,E10.3,' Eou',E10.3/17X
     2      ,' Input angular distribution negative at',I3
     3      ,' out of',I3,' angle(s)')
  912 FORMAT(' EMPEND ERROR - in RDANG reading EMPIRE output record:'/
     1       ' "',A70,'"')
      END
      SUBROUTINE ANGLEG(NAN,ANG,NEN,DST,LOMX,LOR,PLG,MXR,RWO,MT,EIN,ZAP
     &                 ,IPRNT,LTT,LER,L92,LCU,LPT,EI1,EI2,EO1,EO2)
C-Title  : Subroutine ANGLEG
C-Purpose: Convert angular distributions to Legendre polynomials
C-Description:
C-D  NAN        Number of angles (i)
C-D  ANG(i)     Cosines of angles at which distributions are tabulated
C-D  NEN        Number of outgoing particle energies (j)
C-D  DST(1,j)   Outgoing particle energy Ej [eV]
C-D  DST(1+i,j) Distribution at cosines C(i) for energy E(j)
C-D  LOMX       Maximum Legendre order.
C-D  LOR        Highest Legendre order after fitting.
C-D  PLG(1,j)   =DST(1,j)
C-D  PLG(1+i,j) Legendre coefficients corresponding to DST(1+i,j)
C-D  MXR        Maximum size of the work array RWO
C-D  RWO        Work array.
C-D
C-D  Note: it is permissible to use implicit equivalence between DST
C-D  and PLG by specifying the same array when calling the routine.
C-
      PARAMETER  (MAN=120)
      DIMENSION   SCR(MAN)
      DIMENSION   ANG(NAN),DST(1),PLG(1),RWO(MXR)
C* Permissible tolerance for fitted angular distributions (fraction)
      DATA ETOL/ 0.010 /
C*
      LD   =1
      LP   =1
      LOR  =0
      LOX  =MIN(LOMX,NAN-1)
      IF(LOX.GT.MAN) STOP 'ANGLEG ERROR - MAN limit exceeded'
      NOFIT=0
C*
C* Loop over all outgoing energy points
      DO I=1,NEN
        JPRNT  =0
        EOU    =DST(LD)
        DO J=1,LOX
          SCR(J)=0
        END DO
c...
c...       print *,'    Do lsqlgv for energy point',i,' of',nen
c...
C*        Fit Legendre polynomials
        LOO    =LOX
        CALL LSQLGV(ANG,DST(LD+1),NAN,SCR,0,LOO,ETOL,ERR,RWO,MXR)
        LOR=MAX(LOR,LOO)
c...
c...       print *,'    Done lsqlgv LOR',LOR
C...    if(abs(mt).eq.649) then
C...        print *,'LOX,lor,loo',LOX,lor,loo,mt,ein
C...        if(nan.gt.1) print *,(ang(j),j=1,4),'...',ang(nan)
C...        print *,(dst(ld-1+j),j=1,5),'...',dst(ld+nan)
C...        print *,eou,(scr(j),j=1,4)
C...            stop
C...    end if
C*
C* Check for printout on exceeding tolerance limits
        IF(ERR.GT.5.*ETOL .OR. ERR.LT.0) THEN
C*        Count the number of points with convergence problems
          NOFIT=NOFIT+1
C*        Print a warning when convergence problems are encountered
c...
c...        WRITE(LTT,907) MT,NINT(ZAP),EIN,EOU,ERR*100.
c...        WRITE(LER,907) MT,NINT(ZAP),EIN,EOU,ERR*100.
c...
c...        write(*,*) 'Ein,Eou,Loo',Ein,Eou,Loo
c...        write(*,*) scr(1),(scr(1+L)/(2*L+1)/scr(1),L=1,Loo)
c...
          IF(IPRNT.EQ.0) JPRNT=1
C*        Check for specific reaction printout
        ELSE IF
     1    (IPRNT.EQ.MT                    .AND.
     2    (  EIN.GE.EI1 .AND. EIN.LE.EI2) .AND.
     3    (  EOU.GE.EO1 .AND. EOU.LE.EO2)) THEN
          JPRNT=1
        END IF
C* Check for isotropic distributions (suppress printout)
        IF(LOO.LT.1) JPRNT=0
C* Check for differences in the fitted angular distributions
C* at meshpoints and midpoints
        IF(JPRNT.NE.0) THEN
C*        Plotting instructions to the "input" file on unit L92
C*        Original values to the "points" file file on unit LPT
C*        Fitted values to the "curves" file file on unit LCU
          WRITE(L92,*) 'EMPEND Plotting MT',MT,' Err',ERR
          WRITE(L92,*) 'Ein',EIN,' Eou',EOU,' Particle ZA',NINT(ZAP)
          WRITE(L92,821) 0,0,0,0
          WRITE(L92,821) 0,2,0
          IF(EOU.GE.0) THEN
            WRITE(LPT,932) EIN,EOU,MT,IFIX(ZAP+0.1)
          ELSE
            WRITE(LPT,933) EIN,EOU,MT,IFIX(ZAP+0.1)
          END IF
          WRITE(LCU,931) LOO
C*
          DO K=2,NAN
            L=NAN+2-K
            WRITE(LPT,934) ANG(L),0.,0.,DST(LD+L)
C*       -- Function at mesh point
            ANI=ANG(L)
            DSI=POLLG1(ANI,SCR,LOO)
            WRITE(LCU,934) ANI,DSI
C*       -- Function at 1/4 to the next mesh point
            ANI=(3*ANG(L)+  ANG(L-1))/4
            DSI=POLLG1(ANI,SCR,LOO)
            WRITE(LCU,934) ANI,DSI
C*       -- Function at 2/4 to the next mesh point
            ANI=(2*ANG(L)+2*ANG(L-1))/4
            DSI=POLLG1(ANI,SCR,LOO)
            WRITE(LCU,934) ANI,DSI
C*       -- Function at 3/4 to the next mesh point
            ANI=(  ANG(L)+3*ANG(L-1))/4
            DSI=POLLG1(ANI,SCR,LOO)
            WRITE(LCU,934) ANI,DSI
          END DO
          ANI=ANG(1)
          DSI=POLLG1(ANI,SCR,LOO)
          WRITE(LPT,934) ANG(L),0.,0.,DST(LD+1)
          WRITE(LCU,934) ANI,DSI
          WRITE(LCU,934)
          WRITE(LPT,934)
        END IF
C*
C*      End of test prints
C*
C* Save Legendre coefficients to main output array
C*   -- For (l > 0) divide by (2*l+1) to conform with ENDF rules.
        PLG(LP  )=EOU
        PLG(LP+1)=SCR(1)
        DO L=1,LOX
          IF(L.LE.LOO) THEN
            PLG(LP+1+L)=SCR(1+L)/(2*L+1)
          ELSE
            PLG(LP+1+L)=0
          END IF
        END DO
        LD=LD+NAN+1
        LP=LP+LOX+2
      END DO
c...
c...          print *,'        LOX,lor,loo,mt',LOX,lor,loo,mt
c...          do i=1,nen
c...            lp=1+(i-1)*(lox+2)
c...            print *,lp,(plg(lp-1+j),j=1,5)
c...            if(abs(mt).gt.2) stop
c...          end do

C* Re-pack the array of Legendre coefficients
      LP1=1
      LP2=1
      DO I=1,NEN
        NLG=LOX+2
        DO J=1,NLG
          PLG(LP2-1+J)=PLG(LP1-1+J)
        END DO
        LP1=LP1+NLG
        LP2=LP2+LOR+2
      END DO
c...
c...          do i=1,nen
c...            lp=1+(i-1)*(lor+2)
c...            print *,lp,(plg(lp-1+j),j=1,5)
c...            if(abs(mt).gt.2) stop
c...          end do

C* Print warning in case of badly fitted distributions
      IF(NOFIT.GT.0) THEN
        WRITE(LTT,908) MT,NINT(ZAP),EIN,NOFIT
        WRITE(LER,908) MT,NINT(ZAP),EIN,NOFIT
      END IF
c...
c...      print *,'              exit angleg'
c...
      RETURN
C*
  821 FORMAT(22X,4I11)
  907 FORMAT(' EMPEND WARNING - MT',I4,' IZA',I5
     1      ,' Ein',1P,E10.3,' Eou',E10.3,' Ang.Fit Dif',0P,F6.1,'%')
  908 FORMAT(' EMPEND WARNING - MT',I4,' IZA',I5
     1      ,' Ein',1P,E10.3,' bad ang.distr. fit for',I3,' Eout')
  931 FORMAT('P(',I2.2,') Fit')
  932 FORMAT(1P,'Ei',E7.2E1,' Eo',E7.2E1,' MT',I3,' PZA',I5)
  933 FORMAT(1P,'Ei',E7.2E1,' Eo',E7.1E1,' MT',I3,' PZA',I5)
  934 FORMAT(1P,6E11.4)
      END
      SUBROUTINE REAMF3(LIN,LTT,LER,MXE,MXT,MXM
     1                 ,EIN,XSC,QQM,QQI,MTH,IZB,BEN
     1                 ,IZI,IZA,LISO,AWR,SPI,STF0,GAMG,D0LV,NEN,NXS)
C-Title  : REAMF3 Subroutine
C-Purpose: Read EMPIRE output to be converted into ENDF format
C-Description:
C-D  Cross sections and nu-bar are extracted from Empire output. The
C-D  energies are stored in array EIN, containing NEN energy values
C-D  Ej on exit. The cross sections and nu-bar are collected in array
C-D  XSC(j,i) for 'i' reaction channels.
C-D
C-D  MTH  Array contains MT numbers of identified reactions. The MT
C-D       numbers of reactions contributing to MT 5 in the high-energy
C-D       type of file is flagged by adding MT+1000.
C-D  NEN  Counts the Number of energy points
C-D  NXS  Counts the Number of reaction types
C-
      CHARACTER*2  CH
      CHARACTER*8  PTST
      CHARACTER*30 CHEN
      CHARACTER*80 REC,COM
C* Declare XS,XC,XI double precision to avoid underflow on reading
      DOUBLE PRECISION XS,XC,XI
      DIMENSION    EIN(MXE),XSC(MXE,MXT),QQM(MXT),QQI(MXT)
     1            ,MTH(MXT),IZB(MXM),BEN(3,MXM)
C* Particle masses (neutron, proton, deuteron, triton, He-3, alpha)
      COMMON /PMASS/ AWN,AWH,AWD, AWT, AW3,AWA
C* Search for the reaction header cards
C*   NEN counts the Number of energy points
C*   NXS counts the Number of reaction types
C*   IMT count of the nucleons for which binding energies are given
      NXS=0
      NEN=0
      IMT=0
C*
C* Search EMPIRE output for specific strings
  110 READ (LIN,891,END=700) REC
      IF(REC( 1:10).EQ.' REACTION '                  ) GO TO 200
      IF(REC( 1:18).EQ.'  Decaying nucleus'          ) GO TO 210
      IF(REC(13:35).EQ.'ground state population'     ) GO TO 220
      IF(REC(13:35).EQ.'isomer state population'     ) GO TO 224
      IF(REC( 1:10).EQ.' TOTAL  CR'                  ) GO TO 290
c...  IF(REC( 5:20).EQ.'fission  cross s'            ) THEN
      IF(REC( 2:19).EQ.'Tot. fission cross'          ) THEN
        QQ=2.0E8
        MT=18
        READ(REC,809) XS
        GO TO 312
      END IF
      IF(REC(2:14).EQ.' Multiplicity') THEN
C* Average number of (prompt) neutrons per fission
        QQ=0
        MT=456
C...    MT=452
        IF(REC(15:20).EQ.' (nue)') THEN
          READ(REC(21:30),994) XS
        ELSE
          READ(REC(15:24),994) XS
        END IF
C...
C...        print *,' nubar',ee,xs
C...
        GO TO 312
      END IF
      GO TO 110
C* Identify projectile, target and energy
  200 READ (REC(11:20),802) KZ,CH,KA
      IZI=KZ*1000+KA
      IF(IZI.NE.   0 .AND. IZI.NE.   1 .AND.
     &   IZI.NE.1001 .AND. IZI.NE.2004) THEN
        WRITE(LTT,904) ' EMPEND ERROR - Invalid projectile ZA   ',IZI
c...    STOP 'EMPEND ERROR - Invalid projectile'
      END IF
      PTST='        '
      CALL POUCHR(PTST,IZI,AWI)
      READ (REC(24:33),802) IZ,CH,IA
      IZA=IZ*1000+IA
      AWR=IA
C* Allow for metastable targets
      IF     (REC(34:34).EQ.' ') THEN
        LISO=0
      ELSE IF(REC(34:34).EQ.'m') THEN
        LISO=1
      ELSE IF(REC(34:34).EQ.'n') THEN
        LISO=2
      ELSE
        WRITE(LTT,891) ' EMPEND WARNING - Invalid metastable sta'//
     &                 'te of target '//REC(24:34)//'                '
        WRITE(LER,891) ' EMPEND WARNING - Invalid metastable sta'//
     &                 'te of target '//REC(24:34)//'                '
        LISO=0
      END IF
C* Read and check the energy
  201 READ (REC(51:60),994) EE
      EE = EE*1.E6
      IF(NEN.LE.0) GO TO 206
      IF(EE.GT.EIN(NEN)) GO TO 206
C* Skip double energy points
      WRITE(LTT,902) ' EMPEND WARNING - Non-monotonic point eV',EE
      WRITE(LER,902) ' EMPEND WARNING - Non-monotonic point eV',EE
  202 READ (LIN,891,END=700) REC
      IF(REC(1:10).NE.' REACTION '        ) GO TO 202
      GO TO 201
C*
  206 NEN=NEN+1
      IF(NEN.GT.MXE) STOP 'EMPEND ERROR - MXE limit exceeded'
      EIN(NEN)=EE
      CHEN=REC(51:80)
C* Read the binding energies of the last nucleon, if given
C* (Omission allowed in new files where Q values are specified explicitly)
  207 READ (LIN,891) REC
C* Read the scattering radius
      IF(REC( 1:26).EQ.'       Scattering radius ='  ) THEN
        QQ=0
        MT=9151
        READ(REC(27:33),*) XS
        GO TO 312
      END IF
      IF(REC( 1:26).EQ.'       Calc. Strength func'  ) THEN
        READ(REC(37:42),*) STF0
      END IF
      IF(REC(1:10).EQ.' FUSION CR') GO TO 110
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
C* If target nucleus, read the mass
      IF(JZA  .EQ. IZA  ) THEN
        READ (REC(37:46),994) AWR
        AWR=AWR/AWN
      END IF
C* Read the reaction Q-value
      READ (REC(56:65),994) QQ
      QQ=QQ*1.E6
      QI=QQ
C* Assign MT number from residual ZA
      CALL EMTIZA(IZI,IZA,JZA,MT)
      IF(MT.EQ.0) MT=10*JZA+5
C...      IF(MT.EQ.0) THEN
C...C* Add to MT5 unidentified products with non-zero x-sect., print warning
C...  212   READ (LIN,891) REC
C...        IF(REC(13:22).NE.'production') GO TO 212
C...        READ (REC,803) XS
C...c...
C...c...        print *,'izi,iza,jza,mt',izi,iza,jza,mt,rec(2:50)
C...c...
C...        IF(XS.GT.0) THEN
C...          COM=' WARNING >>MT 5'//REC(2:22)//REC(37:52)//' at'//CHEN
C...          WRITE(LTT,891) COM
C...          WRITE(LER,891) COM
C...          MT=5
C...          GO TO 312
C...        END IF
C...        GO TO 110
C...      END IF
C* Test for discrete levels inelastic, (n,p) and (n,a) cross sections
      IF(MT.EQ. 50 .OR. MT.EQ.600 .OR. MT.EQ.800) THEN
        MT0=MT
        GO TO 350
      END IF
C* All other cross sections are processed in the same way
      GO TO 310
C*
C* Isomer production cross section - ground state
  220 CONTINUE
      EISO=0
      MISO=0
      GO TO 226
C* Isomer production cross section - ground state
  224 CONTINUE
      READ (REC(59:65),*) EISO
      EISO=EISO*1.E6
      MISO=1
  226 QI  =QQ-EISO
      MT  =JZA*10+MISO
      GO TO 311
C*
C* Read the total cross section but exclude incident charged particles
  290 IF(IZI.GE.1000) GO TO 110
      READ (REC,808) XS
      MT=1
      QQ=0
      QI=0
      QM=0
      GO TO 392
C*
C* General processing of cross sections
  310 READ (LIN,891) REC
      IF(REC(1:4).EQ.' Gg =') THEN
        READ(REC(5:13),*) GAMG
        READ (LIN,891) REC
        READ(REC(5:13),*) D0LV
        GO TO 310
      END IF
      IF(REC(13:22).NE.'production') GO TO 310
C* Read the cross section
  311 READ (REC,803) XS
      IF(XS.LE.0) GO TO 110
C* Reconstruct Q-values from MT and the binding energies
      IF(QQ.EQ.0 .AND. MT.NE.50) THEN
        CALL QVALUE(IMT,MT,IZA,IZB,BEN,QQ)
      END IF
C* Test if reaction is already registered
C*  - ENDF MT-numbers <999
C*  - 10*ZA+LFS for isomer production
C*  - 5000-9999 special cases (e.g. 9151 for the scattering radius)
  312 IF(NXS.GT.0) THEN
        DO I=1,NXS
          IXS=I
          MTI=ABS(MTH(I))
          IF(MTI.EQ.    MT ) THEN
            IF(ABS(MT).LE.9999) THEN
              GO TO 320
            ELSE
C*            Check level energy for isomer production (MT>9999)
              IF(QI.EQ.QQI(I)) GO TO 320
C*            -- If level energy different, increment the MT
              MT=MT+1
C*            -- Increment the MT in the stored array
C...          MTI=MTI+1
C...          IF(MTH(I).LT.0) MTI=-MTI
C...          MTH(I)=MTI
              MT=MT+1
            END IF
          END IF
        END DO
      END IF
      NXS=NXS+1
      IF(NXS.GT.MXT) STOP 'EMPEND ERROR - MXT limit exceeded'
      IXS=NXS
      MTH(IXS)=MT
C...      IF(MT.EQ.5) THEN
C...C* Reserve space for proton and alpha production of unidentified reactions
C...        IF(NXS+2.GT.MXT) STOP 'EMPEND ERROR - MXT limit exceeded'
C...        MT201=IXS+1
C...        MT203=IXS+2
C...        MT207=IXS+3
C...        NXS  =NXS+3
C...        MTH(MT201)=201
C...        MTH(MT203)=203
C...        MTH(MT207)=207
C...      END IF
C* Save Q-values and cross section for this reaction
      QQM(IXS)=QQ
      QQI(IXS)=QI
C* Add cross section contribution
  320 IF((MT.GE. 450 .AND. MT.LT. 460) .OR.
     &   (MT.GE.1000 .AND. MT.LE.9999)) THEN
        XSC(NEN,IXS)=XS
      ELSE
        XS=XS*1.E-3
        XSC(NEN,IXS)=XSC(NEN,IXS)+XS
      END IF
C...      IF(MT.GT.9999) THEN
C...        LFS=MT-10*(MT/10)
C...        IF(LFS.GE.5) THEN
C...          XS=XS*1000
C...          MT=5
C...          GO TO 312
C...        END IF
C...      END IF
C...      IF(MT.NE.5) GO TO 110
C...C* Special treatment for MT5 (summing neutron, proton and alpha prod.)
C...        KZA=IZA+IZI-JZA
C...C* Sum alpha production of unidentified reactions
C...        DO WHILE (KZA.GE.2004)
C...          XSC(NEN,MT207)=XSC(NEN,MT207)+XS
C...          KZA=KZA-2004
C...        END DO
C...C* Sum proton production of unidentified reactions
C...        DO WHILE (KZA.GE.1001)
C...          XSC(NEN,MT203)=XSC(NEN,MT203)+XS
C...          KZA=KZA-1001
C...        END DO
C...C* Sum alpha production of unidentified reactions
C...        DO WHILE (KZA.GE.   1)
C...          XSC(NEN,MT201)=XSC(NEN,MT201)+XS
C...          KZA=KZA-0001
C...        END DO
C...c...
C...c...        print *,'MT5 iza,jza,qj,qq',iza,jza,qqm(ixs),qq
C...c...
C...        QQM(IXS)=MAX(QQ,QQM(IXS))
C...        QQI(IXS)=QQM(IXS)
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
  351 READ (LIN,891,END=710) REC
      IF(REC( 1:10).EQ.' ELASTIC C') GO TO 370
      IF(REC(13:22).EQ.'production') GO TO 390
      IF(REC(11:30).NE.'Discrete level popul') GO TO 351
C* Positioned to read discrete levels
      READ (LIN,891)
      READ (LIN,891)
      READ (LIN,891)
      XI=0
      JL=0
C* For incident neutrons allow ground state for other particles
      IF(IZI.EQ.   1 .AND. MT0.NE. 50) JL=-1
C* For incident protons allow ground state for other particles
      IF(IZI.EQ.1001 .AND. MT0.NE.600) JL=-1
C* For incident alphas allow ground state for other particles
      IF(IZI.EQ.2004 .AND. MT0.NE.800) JL=-1
C* Loop reading discrete level cross sections
  352 READ (LIN,805) IL,EL,II,X,XS,NBR
C*      -- Assign spin from first level when product=target
      IF(IZA.EQ.JZA .AND. IL.EQ.1) SPI=X
      DO WHILE (NBR.GT.7)
        READ (LIN,891)
        NBR=NBR-7
      END DO
C* Test for last level (IL=0 from reading blank line)
      IF(IL.LE.0 ) GO TO 351
      EL=EL*1.E6
      XI=XI+XS
c...
c...  if(izi.eq.1 .and. nint(el/1000).eq.nint(qq/1000))
c... 1    print *,' discrete level il,el,xs,jl',il,xs,jl,qq
c...
C* Exclude level that results in the same energy state (=elastic by def.)
      IF(IZI.EQ.1 .AND. NINT(EL/1000).EQ.NINT(QQ/1000)) GO TO 352
c... Inelastic scattering on metastable target to ground is MT51, not 50
c...  MT=MT0-1+IL
      JL=JL+1
      MT=MT0+JL
C* Check if level alredy registered
      IF(NXS.GT.0) THEN
        DO I=1,NXS
          IXS=I
          MTI=ABS(MTH(I))
          IF(MTI.EQ.MT) GO TO 360
        END DO
      END IF
C* Enter new discrete level for this reaction
      NXS=NXS+1
      IF(NXS.GT.MXT) STOP 'EMPEND ERROR - MXT limit exceeded'
      IXS=NXS
      MTH(IXS)=MT
      QM=QQ
C* Reconstruct Q values from MT and the binding energies if necessary
      IF(QQ.EQ.0 .AND. MT0.NE.50 .AND. IMT.GT.0) THEN
        QM=0
        DO I=1,IMT
          KZA=IZB(I)
          IF(KZA.EQ.IZA+IZI) THEN
            print *,' Q-value calc. brom binding energies for MT',MT
            IF(MT0.EQ.600.OR.MT0.EQ.800) QM=QM+1.E6*BEN(1,I)
            IF(MT0.EQ.600)               QM=QM-1.E6*BEN(2,I)
            IF(MT0.EQ.800)               QM=QM-1.E6*BEN(3,I)
          END IF
        END DO
      END IF
      QI      =QM-EL
      QQM(IXS)=QM
      QQI(IXS)=QI
C* Enter cross section for this discrete level
  360 XSC(NEN,IXS)=XS*1.E-3
C*      Save QI for this level
      QI=QQI(IXS)
      QM=QQM(IXS)
      GO TO 352
C* Read the elastic cross section but exclude incident charged particles
  370 IF(IZI.GE.1000) GO TO 351
      MT=2
      READ (REC,808) XE
c...C* Read the level energy in the case of a metastable target
c...      READ (REC(52:61),994) QQ
c...      QQ=QQ*1.0E6
      DO I=1,NXS
        IXS=I
        MTI=ABS(MTH(I))
        IF(MTI.EQ.MT) GO TO 376
      END DO
      NXS=NXS+1
      IF(NXS.GT.MXT) STOP 'EMPEND ERROR - MXT limit exceeded'
      IXS=NXS
      MTH(IXS)=MT
C* Q value for elastic scattering may be non-zero for metastable targets
      QQM(IXS)=QQ
      QQI(IXS)=QQ
  376 XSC(NEN,IXS)=XE*1.E-3
      GO TO 351
C* Positioned to read continuum cross section (subtract discrete levels)
  390 CONTINUE
      READ (REC,803) XC
      XS=XC-XI
      IF(XC.GT.0 .AND. XS/XC.LT.2.E-5) THEN
c...        CPC=100*XS/XC
c...        print *
c...        print *, ' ---            Skip continuum for MT : ',MT0
c...        print *, '                      Incident energy : ',EE
c...        print *, '                  Total cross section : ',XC
c...        print *, '       Sum of discrete cross sections : ',XI
c...        print *, '       Continuum contribution percent : ',CPC
        XS=0.
      END IF
C* Skip continuum if its contribution is negligible
      IF(XS.LE.0) GO TO 110
C* Add continuum to the list
      IF(MT0.EQ. 50) MT= 91
      IF(MT0.EQ.600) MT=649
      IF(MT0.EQ.800) MT=849
C* Test if reaction is already registered
  392 IF(NXS.GT.0  ) THEN
        DO I=1,NXS
          IXS=I
          MTI=ABS(MTH(I))
          IF(MTI.EQ.MT) GO TO 396
        END DO
      END IF
      NXS=NXS+1
      IF(NXS.GT.MXT) STOP 'EMPEND ERROR - MXT limit exceeded'
      IXS=NXS
C* Reaction QM value from the last discrete level
C* Reaction QI may be lower due to (z,g+x) reactions
      QIE=-0.99*EE*AWR/(AWR+AWI)
      QI =MAX(QI,QIE)
      MTH(IXS)=MT
      QQM(IXS)=QM
      QQI(IXS)=QI
  396 XSC(NEN,IXS)=XS*1.E-3
      GO TO 110
C*
C* All data read
  700 RETURN
C*
C* Error traps
  710 WRITE(LTT,904) ' EMPEND ERROR - REAMF3 processing MT    ',MT0
      WRITE(LTT,902) '                              at energy ',EE
      STOP 'EMPEND ERROR - Reading MF3 data'
C*
  802 FORMAT(I3,1X,A2,1X,I3)
  803 FORMAT(37X,F12.0)
  804 FORMAT(1X,I3,1X,A2,1X,I3,4X,3F10.0)
  805 FORMAT(I12,F10.0,I5,F8.0,3X,F12.0,I3)
  806 FORMAT(BN,8X,8F15.0)
  808 FORMAT(24X,F12.0)
c.809 FORMAT(26X,F12.0)
  809 FORMAT(30X,F10.0)
  891 FORMAT(A80)
  902 FORMAT(A40,1P,E10.3)
  904 FORMAT(A40,I10)
  994 FORMAT(BN,F10.0)
  995 FORMAT(BN,I6)
      END
      SUBROUTINE REAMF6(LIN,LTT,LER,EIN,XSC,NE3,EIS,YLD,RWO,MTH,MT6
     1                 ,IZI,QQM,QQI,AWR,ELO,NXS,NK,LCT,MXE,MXR
     2                 ,IPRNT,EI1,EI2,EO1,EO2,NZA1,NZA2)
C-Title  : REAMF6 Subroutine
C-Purpose: Read EMPIRE output energy/angle distrib. for each MT
C-Version:
C-V  00/03 Define Unit base linear interpolation between incident E.
C-V  03/02 Major reorganisation of the routine
C-
C* No.of input angles MXA, fine grid angles MXZ, particles/reaction MXP
      PARAMETER    (MXA=200, MXZ=400, MXP=200)
      CHARACTER*8   POUT(MXP),PTST
      CHARACTER*40  FL92,FLPT,FLCU
      CHARACTER*136 REC
C* Particle masses (neutron, proton, deuteron, triton, He-3, alpha)
      COMMON /PMASS/ AWN,AWH,AWD, AWT, AW3,AWA
      DIMENSION     EIN(1),XSC(MXE,1),EIS(1),YLD(1),QQM(1),QQI(1)
     1             ,RWO(1),ANG(MXA),DST(MXA),MTH(1)
      DIMENSION     IZAK(MXP),AWPK(MXP),ZANG(MXZ),ZLEG(MXZ)
C*
      DATA PI/3.1415926/
      DATA SMALL/1.E-5 /
C* Maximum Legendre order
      DATA LOMX/ 64 /
C* Test print filenames and logical file units
      DATA FL92/'angdis.p92'/
     &     FLCU/'angdis.cur'/
     &     FLPT/'angdis.pnt'/
      DATA L92,LCU,LPT/-30,31,32/
C* Test print files
      IF(IPRNT.GE.0) THEN
        IF(L92.LT.0) THEN
          L92=-L92
          OPEN (UNIT=L92,FILE=FL92,STATUS='UNKNOWN')
          OPEN (UNIT=LCU,FILE=FLCU,STATUS='UNKNOWN')
          OPEN (UNIT=LPT,FILE=FLPT,STATUS='UNKNOWN')
          WRITE(L92,821) 0.5,14., 0.5,10., 1, 1, 1.2
          WRITE(L92,822)  1,  1, 1,  0,  4, 0, 0
          WRITE(L92, * ) 'Angle                                   '
     &                  ,'Cosine'
          WRITE(L92, * ) 'Distribution                            '
     &                  ,'mb/MeV/St'
        END IF
      END IF
C* Initialise indices
C* NE6 counts the Number of energy points
      JT6=ABS(MT6)
      MTC=JT6
      LCT=2
      NE6=0
      LBL=1
      IK =0
      NK =0
      NSK=0
      NYL=1
c...      NP =0
c...      IT =0
      PTST='        '
      CALL POUCHR(PTST,IZI,AWI)
C*
C* For elastic angular distributions define reaction and particles
      IF(MT6.EQ.-2) THEN
        MT=2
        MTC=MT
        NK =1
        POUT(NK)='neutrons'
        IZAK(NK)=1
        GO TO 200
      END IF
C*
C* For other reactions scan the file to identify all outgoing particles
  110 READ (LIN,891,END=140) REC
      IF(REC(1:14).NE.'  Spectrum of '    ) GO TO 110
C...
c...      print *,'mt6',mt6,rec(14:30)
C...
      IF(MT6.LT.0) THEN
C* For discrete level reactions consider only neutrons, protons, alphas
        PTST=REC(15:22)
        IF     ((-MT6.GT.  4 .AND. -MT6.LT.100) .AND.
     1                        PTST.EQ.'neutrons') THEN
          MTC= 51
        ELSE IF((-MT6.GE.600 .AND. -MT6.LE.649) .AND.
     1                        PTST.EQ.'protons ') THEN
          MTC=600
        ELSE IF((-MT6.GE.800 .AND. -MT6.LE.849) .AND.
     1                        PTST.EQ.'alphas  ') THEN
          MTC=800
        ELSE
          GO TO 110
        END IF
      END IF
C* Identify reaction and check if it matches the required given by JT6
      CALL EMTCHR(REC(23:30),MT,IZI)
C...
c...      print *,'     Assigned MT, requested JT,MTC',MT,JT6,MTC
C...
      IF(MT.EQ. 0 .OR. MT.NE.JT6) GO TO 110
C...
c...      print *,'     Found MT,JT,MTC',MT,JT6,MTC
C...
C* Assign KZAK to outgoing particle ZA for unique react. identification
      PTST=REC(15:22)
      KZAK=-1
      CALL POUCHR(PTST,KZAK,AWP)
      IF(KZAK.GE.999999) THEN
        READ (REC(35:58),808) KZAK,AWP
        AWP=AWP/AWN
      END IF
C* Check if this particle for this reaction is already registered
      IF(NK.GT.0) THEN
        DO 122 JK=1,NK
        IF(PTST.EQ.POUT(JK) .AND. KZAK.EQ.IZAK(JK)) GO TO 110
  122   CONTINUE
      END IF
C* New particle for this reaction identified
      NK=NK+1
      IF(NK.GT.MXP) STOP 'EMPEND ERROR - MXP limit exceeded'
C...
c...       print *,'    New NK',NK,' particle ',PTST,KZAK
C...
      POUT(NK)=PTST
      IZAK(NK)=KZAK
      AWPK(NK)=AWP
      IF(NK.LE.1) GO TO 110
C* Sort in ascending ZA order but placing gamma last
      DO I=2,NK
        K=NK+2-I
        IF((IZAK(K-1) .GT.0 .AND. KZAK.GE.IZAK(K-1)) .OR.
     &      KZAK.EQ.0) GO TO 110
        POUT(K)=POUT(K-1)
        IZAK(K)=IZAK(K-1)
        AWPK(K)=AWPK(K-1)
        POUT(K-1)=PTST
        IZAK(K-1)=KZAK
        AWPK(K-1)=AWP
      END DO
      GO TO 110
C* File scanned for particles - eliminate unknown particles
  140 IK=0
      JK=NK
      NK=0
      DO 142 J=1,JK
      IF(IZAK(J).GE.0) THEN
        NK=NK+1
        POUT(NK)=POUT(J)
        IZAK(NK)=IZAK(J)
        AWPK(NK)=AWPK(J)
      ELSE
C* Unidentified outgoing particle
        WRITE(LTT,910) POUT(J),MTC
        WRITE(LER,910) POUT(J),MTC
      END IF
  142 CONTINUE
C*
C* Process the spectra - Search for the reaction header cards
  200 IK=IK+1
      REWIND LIN
      WRITE(LTT,921) POUT(IK),IZAK(IK),MTC
      WRITE(LER,921) POUT(IK),IZAK(IK),MTC
c...      NE6N=0
      NE6= 0
      MTX= 0
      E0 =-1
      YL0= 1
      ETEF=0
C* Find the cross section index
      DO I=1,NXS
        IF(MTH(I).EQ.MTC) THEN
          ETH=MAX(-QQI(I)*(AWR+AWI)/AWR, ELO )
c...
c...    print *,'  mt6,MTC,q,aw,eth,elo',mt6,MTC,qqi(i),awr,eth,elo,e0
c...
          MT =MTC
          MTX=MT
          IT =I
C* Find pseudo-threshold energy (if applicable)
          DO J=1,NE3
            EN3=EIN(J)
            XS3=XSC(J,IT)
            IF(XS3.GT.0) EXIT
            IF(EN3.GT.ETH) ETEF=EN3
          END DO
          GO TO 210
        END IF
      END DO
C* Reaction not on the list of MF3 reactions - skip the data
      WRITE(LTT,912) JT6
      NK=0
      RETURN
C*
C* Search the EMPIRE output for specific strings
  210 READ (LIN,891,END=700) REC
      IF(REC(1:10).EQ.' REACTION '        ) GO TO 300
      IF(REC(1:14).EQ.'  Elastic angu'    ) GO TO 400
      IF(REC(1:14).EQ.'  Spectrum of '    ) GO TO 600
      IF(REC(1:26).EQ.'    fission  cross section') THEN
C*      Count fission reactions
        READ (REC(27:38),992) X
        IF(X.GT.1.E-10) NFIS=NFIS+1
      END IF
      GO TO 210
C* Read the incident particle energy
  300 READ (REC(51:60),994) EE
      NFIS=0
      JFIS=0
      EE=EE*1.E6
      IF(EE.GT.E0 .AND. (EE-ETH).GT.-EE*SMALL) GO TO 210
C* Skip to next energy if current less or equal to previous point
  302 READ (LIN,891,END=700) REC
      IF(REC(1:10).NE.' REACTION '        ) GO TO 302
      E0=-1.
      GO TO 300
C*
C* Read the elastic angular distributions
  400 IF(MT6.NE.-2) GO TO 210
      XS3=0.
C* Find the matching energy grid point index of the cross sections
      JE3=0
  425 JE3=JE3+1
      EN3=EIN(JE3)
      XS3=XSC(JE3,IT)
      IF(ABS(EN3-EE).GT.EE*SMALL .AND. JE3.LT.NE3) GO TO 425
C* If no matching index is found the work array is corrupted (???!!!)
      IF(JE3.LE.0) STOP 'REAMF6 ERROR - Corrupted work array?'
      IF(NE6.GT.0) GO TO 430
C* Define the general File-4 data (HEAD and TAB1 rec.) on first point
c...
C...       print *,'je3,ne6,xs3',je3,ne6,xs3
c...
      ZAP=1.
      AWP=1.
      NP =2
      LIP=0
      LAW=1
      NR =1
      E1 =EE
      Y1 =YL0
      E2 =EE
      Y2 =YL0
      NYL=1
      LEP=2
C     LEP=1
      LANG=1
      NRA=1
C* Unit base linear interpolation between incident neutron energies
      INA=22
C* Reserve the space in the Real array
      LXA=LBL
      LBL=LXA+12+2*NP
      LPK=LBL
      LB1=LBL
      IF(LBL.GT.MXR) STOP 'EMPEND ERROR - MXR limit exceeded'
C* Define specific File-6 data for all incident energies
  430 L6 =LBL
C* Process the angular distribution for this energy
      l64=l6+4
      LMX=MXR-L64
      READ (LIN,891) REC
      EMP=0
      CALL RDANGF(LIN,NEP,NAN,RWO(L64),LMX,ANG,MXA,MTC,ZAP,LTT,LER)
      LSC=L64+NEP*(NAN+1)
      LMX=MXR-LSC
      LOX=MIN(LOMX,NAN-1)
      CALL ANGLEG(NAN,ANG,NEP,RWO(L64),LOX,LHI,RWO(L64),LMX,RWO(LSC)
     &           ,MTC,EE,ZAP,IPRNT,LTT,LER,L92,LCU,LPT,EI1,EI2,EO1,EO2)
C* Only one distribution should be present
      IF(NEP.NE.1) THEN
        PRINT *,'WARNING - Several distributions for elastic'
c       GO TO 210
      END IF
      RWO(L6    )=EE
      RWO(L6 + 1)=LHI
      RWO(L6 + 2)=LHI+2
      RWO(L6 + 3)=1
C* Increment indices in the storage array
      LBL=L6 + 4 + LHI + 2
      NE6=NE6+1
      EIS(NE6)=EE
      GO TO 210
C*
C* Read the energy/angle distribution data
  600 CONTINUE
      MT =0
      E0 =EE
C* Check if particle is to be processed
      PTST=REC(15:22)
      IF(PTST.NE.POUT(IK)) GO TO 210
      IF(PTST.EQ.'recoils ') THEN
        READ (REC(35:58),808) KZAK
c...
c...    print *,rec(15:41),pout(ik),izak(ik)
c...
        IF(KZAK.NE.IZAK(IK)) GO TO 210
      END IF
C* Identify the reaction and assign the MT number
      CALL EMTCHR(REC(23:30),MT,IZI)
      IF(MT.EQ. 0 ) GO TO 210
      IF(MT.NE.JT6) GO TO 210
c...
c...  print *,'processing energy ne6,ee,eth,mt',ne6,ee,eth,mt,mt6
c...
C* Define the pointwise cross section at the same energy
      XS3=0.
      JE3=0
  615 JE3=JE3+1
      EN3=EIN(JE3)
      XS3=XSC(JE3,IT)
      IF(ABS(EN3-EE).GT.EE*SMALL .AND. JE3.LT.NE3) GO TO 615
c...
c...  print *,'  processing energy',ne6+1,ee,' xs=',xs3
c...
C* Skip this energy if cross section is zero
      IF(XS3.LE.0) GO TO 210
C*
C* Initialise parameters if first energy point
      IF(NE6.GT.0) GO TO 620
C* Preset the particle multiplicity for specific reactions
      KZAK=IZAK(IK)
      AWP =AWPK(IK)
      CALL YLDPOU(YL0,MT,KZAK)
c...
c...  print *,'  mt,ne6,kzak,yl0',mt,ne6,kzak,yl0
c...
      IF(YL0.GT.0) THEN
        NYL=1
        NP=2
        YLD( 1)=YL0
        YLD(NP)=YL0
      ELSE
        NYL=0
        NP =0
      END IF
C*
C...
c...  print *,'  particle ZAP,AWP',Kzak,awp
c...
      IZAP=KZAK
      ZAP=KZAK
      LIP=0
      LAW=1
      NR =1
      E1 =EE
      Y1 =YL0
      E2 =EE
      Y2 =YL0
      LEP=2
C     LEP=1
      LANG=1
      NRA=1
C* Unit base linear interpolation between incident neutron energies
      INA=22
C* Reserve space for yields in the Real array
      LXA=LBL
      LPK=LXA+12+2*(NE3+1)
      LB1=LPK
c...
c...       PRINT *,'      REAMF6 IK,LXA,LPK,LB1',IK,LXA,LPK,LB1
c...
  620 CONTINUE
c
c...      print *,'   ee,e1,e2',ee,e1,e2
c...      print *,'       jt6,je3,xs,nxs',jt6,je3,xs3,nxs,NE6
c
C*
C* Process correlated energy/angle distribution for this energy
      L6 =LB1
      L64=L6 + 4
      NW =0
      NEP=0
      E1 =MIN(E1,EE)
      E2 =MAX(E2,EE)
      LHI=0
      KXA=1
      JXA=1
C* Set the available outgoing particle energy
C...      EMP=EMX
C...      EMP=EMX* (AWR+AWI-AWP)/(AWR+AWI)
C... Maximum particle energy should be set within the EMPIRE code!!!
C... Set EMP=0 to skip testing in RDANG
C...
      EMP=0
      LMX=MXR-L64
      READ (LIN,891)
      READ (LIN,891)
      CALL RDANGF(LIN,NEP,NAN,RWO(L64),LMX,ANG,MXA,MT6,ZAP,LTT,LER)
c...
c...       print *,'  Done rdangf NEP',NEP
c...       if(nint(zap).eq.2004) then
c...          print *,rec
c...          print *,'nep,nan,mt6,ee',nep,nan,mt6,ee
c...           do j=1,nep
c...           do j=1,10
c...             print *,(rwo((j-1)*(nan+1)+k+l64-1),k=1,5)
c...           end do
c...           stop
c...       end if
c...
      LSC=L64+NEP*(NAN+1)
      LMX=MXR-LSC
      LOX=MIN(LOMX,NAN-1)
      CALL ANGLEG(NAN,ANG,NEP,RWO(L64),LOX,LHI,RWO(L64),LMX,RWO(LSC)
     &           ,MT6,EE,ZAP,IPRNT,LTT,LER,L92,LCU,LPT,EI1,EI2,EO1,EO2)
c...
c...       print *,'  Done angleg NEP',NEP
c...       if(nint(zap).eq.2004) then
c...          print *,'LOMX,LHI,NEP',LOMX,LHI,NEP,mt6,ee
c...          do j=1,nep
c...          do j=1,10
c...            print *,(rwo((j-1)*(lhi+2)+k+l64-1),k=1,4)
c...          end do
c...          stop
c...       end if
c...
C* Calculate the integral of the spectrum
      SPC=0
      EOU=RWO(L64)
      PEU=RWO(L64+1)
      LI =L64
      DO I=2,NEP
        LI =LI+LHI+2
        EOL=EOU
        PEL=PEU
        EOU=RWO(LI)
        PEU=RWO(LI+1)
        SPC=SPC+(PEU+PEL)*(EOU-EOL)/2
      END DO
C* If the integral is zero, skip this energy point
      IF(SPC.LE.0) THEN
        WRITE(LTT,914) JT6,IZAP,EE,XS3,SPC
        WRITE(LER,914) JT6,IZAP,EE,XS3,SPC
        IF(NE6.EQ.0) ETEF=MAX(ETEF,EE)
        GO TO 210
      END IF
C* Normalise the distribution
      LO1=LHI+1
      L64=L6 +4
      DO I=1,NEP
        DO L=1,LO1
          RWO(L64+L)=RWO(L64+L)/SPC
        END DO
        L64=L64+LHI+2
      END DO
      LB1=L64
C* Scale distribution integral by 4*Pi to get the cross section
C* Scale by 1.E-9 to change mb/MeV into b/eV
      SPC=SPC*4.E-9*PI
C* Pack the size indices into the array
      RWO(L6    )=EE
      RWO(L6 + 1)=LHI
      RWO(L6 + 2)=NEP*(LHI+2)
      RWO(L6 + 3)=NEP
C* Insert the incident particle threshold energy if necessary
c...
c...      print *,'ne6,izap,ee,eth,etef',ne6,izap,ee,eth,etef
c...
      INSE=0
      IF(NE6.EQ.0 .AND. EE.GT.ETH) THEN
        INSE=1
        EINS=ETH
        IF(ZAP.EQ.0) THEN
C* Scale the gamma yield by the ratio of total available energy
C* and the available energy at threshold
          EAVE=-QQI(IT)+ EE*AWR/(AWR+AWI)
          EAV0=-QQI(IT)+ETH*AWR/(AWR+AWI)
          YINS=(EAV0/EAVE)*(SPC/XS3)
        ELSE
          YINS=YL0
        END IF
      END IF
  630 IF(INSE.GT.0) THEN
        IF(IZAP.EQ.0 .AND.
     &    (MT.EQ.91 .OR. MT.EQ.649 .OR. MT.EQ. 849)) THEN
C*          Duplicate existing points for continuum reactions
c...
c...          print *,'duplicating energy',rwo(l6),' to',eth
c...
          NW =4+NEP*(LHI+2)
          LB1=L6+NW+NW
          IF(LB1.GT.MXR) STOP 'EMPEND ERROR - MXR limit exceeded'
          DO J=1,NW
            RWO(LB1-J)=RWO(L6+NW-J)
          END DO
C*          Insert the threshold distribution (delta function)
          RWO(L6  )=EINS
          RWO(L6+1)=LHI
          RWO(L6+2)=NEP*(LHI+2)
          RWO(L6+3)=NEP
          L6 =L6+NW
        ELSE
C*          Shift the existing points forward by 8 words
c...
c...          print *,'inserting Eth before energy',rwo(l6),' Eth',eth
c...
          NW =4+NEP*(LHI+2)
          LB1=L6+NW+8
          IF(LB1.GT.MXR) STOP 'EMPEND ERROR - MXR limit exceeded'
          DO J=1,NW
            RWO(LB1-J)=RWO(L6+NW-J)
          END DO
C*          Insert the threshold distribution (delta function)
          DE=SMALL
          IF(LEP.EQ.2) THEN
            PE=2./DE
          ELSE
            PE=1./DE
          END IF
          RWO(L6  )=EINS
          RWO(L6+1)=0
          RWO(L6+2)=4
          RWO(L6+3)=2
          RWO(L6+4)=0
          RWO(L6+5)=PE
          RWO(L6+6)=DE
          RWO(L6+7)=0
          L6 =L6+8
        END IF
C*        Save the energy and the yield
        NE6=NE6+1
        EIS(NE6)=EINS
        YLD(NE6)=YINS
        E1=ETH
      END IF
C* Insert the pseudo-threshold energy if necessary
      IF(ETEF.GT.0) THEN
c...
c...          print *,'pseudo-threshold energy',rwo(l6),' to',eth
c...
        INSE=1
        EINS=ETEF
        IF(ZAP.EQ.0) THEN
          YINS=(SPC/XS3)
        ELSE
          YINS=0
        END IF
        ETEF=0
        GO TO 630
      END IF
C*
C* Distributions for one incident energy processed - Normalize 
      NE6=NE6+1
c...
c...          print *,'      Processed energy ne6',ne6,ee,eth
c...          print '(1p,10e12.3)',(rwo(j),j=lpk,lb1)
c...
      IF(MT6.LT.0) GO TO 210
C* Particle multiplicity for MT5 or gamma from integral/x.s. ratio
      IF(JT6.EQ.5 .OR. IZAP.EQ.0) THEN
        NP=NE6
        EIS(NE6)=EE
        YLD(NE6)=SPC/XS3
c...
c...        print *,'zap,ee,spc,xs3,ne6',izap,ee,spc,xs3,ne6
c...
      ELSE
C* Check for consistency (neutrons only, exclude fission)
        IF(IZAP.EQ.1 .AND.XS3.GT.0 ) THEN
           IF(YL0.GT.1E-12) THEN
             XSP=SPC/YL0
           ELSE
             XSP=0
           END IF
c...
c...            print *,'spc,y,xsp,xs3',spc,yl0,xsp,xs3
c...
           DFP=100*(XSP-XS3)/XS3
           IF(ABS(DFP).GT.2. .AND. MT.NE.18) THEN
             WRITE(LTT,909) MT,EE,XS3,DFP
             WRITE(LER,909) MT,EE,XS3,DFP
           END IF
        END IF
      END IF
      GO TO 210
C*
C-F File read - Add general reaction data into the packed array
  700 IF(NK.LE.0) RETURN
c...
c...        PRINT *,'REAMF6 IK,LXA',IK,LXA
c...
      IF(NE6.LE.0) THEN
        IZAP=IZAK(IK)
        IF(IZAP.EQ.0) THEN
          WRITE(LTT,995) ' EMPEND ERROR - No gamma yields for MT  ',JT6
          WRITE(LER,995) ' EMPEND ERROR - No gamma yields for MT  ',JT6
          STOP ' EMPEND ERROR - undefined yields'
        ELSE
          WRITE(LTT,995) ' EMPEND WARNING - Reaction MT           ',JT6
          WRITE(LTT,995) '      No distribution data for particle ',IZAP
          WRITE(LER,995) ' EMPEND WARNING - Reaction MT           ',JT6
          WRITE(LER,995) '      No distribution data for particle ',IZAP
          NSK=NSK+1
          GO TO 704
        END IF
      END IF
      RWO(LXA   )=ZAP
      RWO(LXA+ 1)=AWP
      RWO(LXA+ 2)=LIP
      RWO(LXA+ 3)=LAW
      RWO(LXA+ 4)=NR
      RWO(LXA+ 5)=NP
      LAE=LXA+6
      LAG=LAE+NP
      LA1=LAG+NP
c...
c...        print *,'np,ne6,nyl',np,ne6,nyl
c...
      IF(NYL.EQ.1) THEN
        RWO(LAE  )=E1
        RWO(LAE+1)=E2
        RWO(LAG  )=YL0
        RWO(LAG+1)=YL0
      ELSE
        DO I=1,NP
          RWO(LAE-1+I)=EIS(I)
          RWO(LAG-1+I)=YLD(I)
        END DO
      END IF
      RWO(LA1   )=LANG
      RWO(LA1+ 1)=LEP
      RWO(LA1+ 2)=NRA
      RWO(LA1+ 3)=NE6
      RWO(LA1+ 4)=NE6
      RWO(LA1+ 5)=INA
C* Compact the array (No. of pts. for yields .le. NE3+1)
      LP1=LXA+12+2*NP
      IF(LP1.GT.LPK) THEN
        PRINT *,'NP,NE3,LXA,LP1,LPK',NP,NE3,LXA,LP1,LPK
        STOP 'REAMF6 ERROR - Work array corrupted'
      END IF
c...
c...      PRINT *,'      REAMF6 pck IK,LXA,LP1,LPK,LL',IK,LXA,LP1,LPK,LL
c...      PRINT *,'      REAMF6 pck MT,IK,NK,LB1,LP1',MT6,IK,NK,LB1,LP1
c...
      NW=LB1-LPK
      DO I=1,NW
        RWO(LP1-1+I)=RWO(LPK-1+I)
      END DO
      LBL=LP1+NW
c...
c...          print '(1p,10e12.3)',(rwo(lxa-1+j),j=1,122)
c...          print '(1p,10e12.3)',(rwo(j),j=1,122)
c...          print *,'ee,eth',ee,eth
c...
C* Reaction data read - check for next outgoing particle
  704 IF(IK.LT.NK) THEN
        REWIND LIN
        GO TO 200
      END IF
C
C* All data for a reaction processed
  720 MT     = MTH(IT)
      MTH(IT)=-MT
C* Check for skipped particles
      NK=NK-NSK
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
  821 FORMAT(4F11.0,2I11,F4.2)
  822 FORMAT(6I11,I4)
  891 FORMAT(A136)
  909 FORMAT(' EMPEND WARNING - MT',I3,' E',1P,E10.3
     1      ,'eV  Expected x.s.',E10.3,'b  Dif.',0P,F6.1,'%')
  910 FORMAT(' EMPEND WARNING - Can not recognise spectrum for '
     1      ,A8,' MT',I8)
  912 FORMAT(' EMPEND WARNING - Spectrum not processed for MT',I6/
     1       '                  No cross section data available')
  914 FORMAT(' EMPEND WARNING - skip MT',I4,' Zap',I6,' Ein'
     1      ,1P,E10.3,' xs',E9.2,' Intg.=',E9.2)
  921 FORMAT(' Processing outgoing ',A8,' ZAP',I6,' for MT',I4)
  992 FORMAT(BN,F12.0)
  994 FORMAT(BN,F10.0)
  995 FORMAT(A40,I6)
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
C* Test for discrete levels cross section
      IF(JZA  .NE. IZA    ) GO TO 20
C* Process discrete levels
   35 READ (LIN,891) REC
      IF(REC(13:22).EQ.'production') GO TO 20
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
C* Trap levels with no branching ratios given
        IF(IL.GT.1) THEN
C* Assume 100% transition to ground level
          WRITE(LTT,912) IL
          WRITE(LER,912) IL
          JL=1
          LBR(1,IL)=EL
          BRR(1,IL)=1
        END IF
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
      SUBROUTINE WRIMF1(LIN,LOU,MAT,IZI,IZA,LISO,LFI,NLIB,NMOD
     &                 ,ALAB,EDATE,AUTHOR,AWR,EMX,NS)
C-Title  : WRIMF1 Subroutine
C-Purpose: Write comment section (file-1) data in ENDF-6 format
      CHARACTER*66 REC
      CHARACTER*40 FLSC
      CHARACTER*11 BL11,ZSYMAM,ALAB,EDATE,AUTHOR(3)
      CHARACTER*8  PTST
      CHARACTER*2  C2,CH(100)
      CHARACTER*1  ST
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
      DATA PTST/'        '/
      DATA BL11/'           '/
      DATA FLSC/'empmf1.tmp'/
      DATA LSC / 21 /
C* Process comments from the EMPIRE output file
      OPEN (UNIT=LSC,FILE=FLSC,STATUS='UNKNOWN')
      REWIND LIN
      NTXT= 0
      REC='                                 '//
     &    '                                 '
      DO WHILE (REC(1:10).NE.' REACTION ')
        WRITE(LSC,93) REC
        NTXT=NTXT+1
        READ (LIN,93) REC
      END DO
C*
      IZ  = IZA/1000
      IA  = IZA-1000*IZ
      C2  ='??'
      IF(IZ.GT.0 .AND.IZ.LE.100) C2=CH(IZ)
      IF(LISO.EQ.1) THEN
        ST='m'
      ELSE IF(LISO.EQ.2) THEN
        ST='n'
      ELSE
        ST=' '
      END IF
      WRITE(ZSYMAM,92) IZ,C2,IA,ST
C*
      MF  = 1
      MT  = 451
      ZA  = IZA
      LRP =-1
C*
      ELIS= 0.
      STA = 0.
      LIS = LISO
      NFOR= 6
C*
      CALL POUCHR(PTST,IZI,AWI)
      IF     (IZI.EQ.   0) THEN
        NSUB=    0
      ELSE IF(IZI.EQ.   1) THEN
        NSUB=   10
      ELSE IF(IZI.EQ.1001) THEN
        NSUB=10010
      ELSE IF(IZI.EQ.2004) THEN
        NSUB=20040
      ELSE
        PRINT *,'ERROR - NSUB undefined, Invalid inc. particle',IZI
        STOP 'ERROR - NSUB Undefined'
      END IF
      NVER= 3
C*
      TEMP= 0.
      LDRV= 0
      NWD = NTXT+2
      NXC = 0
C*
      CALL WRCONT(LOU,MAT,MF,MT,NS,  ZA,AWR,LRP,LFI,NLIB,NMOD)
      CALL WRCONT(LOU,MAT,MF,MT,NS,ELIS,STA,LIS,LISO, 0 ,NFOR)
      CALL WRCONT(LOU,MAT,MF,MT,NS,AWI ,EMX,  0,  0,NSUB,NVER)
      CALL WRCONT(LOU,MAT,MF,MT,NS,TEMP, 0.,LDRV, 0, NWD, NXC)
      REC=ZSYMAM//ALAB//EDATE//AUTHOR(1)//AUTHOR(2)//AUTHOR(3)
      CALL WRTEXT(LOU,MAT,MF,MT,NS,REC)
      REC=BL11//BL11//BL11//BL11//BL11//BL11
      CALL WRTEXT(LOU,MAT,MF,MT,NS,REC)
      REWIND LSC
      DO I=1,NTXT
        READ (LSC,93) REC
        CALL WRTEXT(LOU,MAT,MF,MT,NS,REC)
      END DO
      CLOSE(UNIT=LSC)
      NS=99998
      CALL WRCONT(LOU,MAT,MF, 0,NS, 0. , 0., 0, 0, 0, 0)
C*
      RETURN
C*
   92 FORMAT(I3,'-',A2,'-',I3,A1)
   93 FORMAT(A66)
      END
      SUBROUTINE WRIMF2(LOU,MXE,MXT,EIN,XSC,MTH,MAT,IZA
     &                 ,AWR,SPI,STF0,GAMG,D0LV,NEN,NXS,NS)
C-Title  : WRIMF2 Subroutine
C-Purpose: Write MF2 (scattering radius) in ENDF-6 format
C-Description:
C-D LOU      Output unit for the ENDF file
C-D MXE      Maximum umber of incident energy points
C-D          (length of arrays EIN, XSC)
C-D MXT      Maximum number of reactions in the XSC array
C-D EIN(i)   Array of incident particle energies Ei
C-D XSC(i,j) Array of cross sections for all reactions (j)
C-D MTH(j)   Array of MT numbers for all reactions
C-D MAT      Material identifier
C-D IZA      ZA designation of the target =1000Z+A
C-D AWR      Atomic mass ratio of the target to neutron
C-D SPI      Spin of the target
C-D STF0     S0-strength function
C-D GAMG     Average gamma width
C-D D0LV     Average level spacing
C-D NEN      Actual number of energy points
C-D NXS      Actual number of cross section sets
C-D NS       ENDF record sequence number
C-D
C-D ENDF-6 format does not allow energy-dependent scattering radius
C-D to be specified when no resonance parameters are given.
C-
      DIMENSION NBT(20),INT(20)
      DIMENSION EIN(MXE),XSC(MXE,MXT),MTH(MXT)
C* Constants
      IZR=0
      ZRO=0
C* Fixed values of parameters
      MF =2
      MT =151
      NIS=1
      ABN=1
C* Define remaining constants
      ZA =IZA
      EL =EIN(1)
      EH =EIN(NEN)
C* Assume fission widths in URR are not given
      LFW=0
C* Check for the scattering radius
      NRO=0
C* Find the reaction index of the scattering radius
      IR =0
      DO J=1,NXS
        IF(MTH(J).EQ.9151) IR=J
      END DO
      IF(IR.GT.0) THEN
C* Determine the range of validity
        EL =EIN(1)
        EH =EL
        LR=1
        DO I=1,NEN
          IF(XSC(I,IR).GT.0) THEN
            LR=I
            EH=EIN(I)
          END IF
        END DO
        IF(LR.GT.1) THEN
C...WARNING... NRO=1 currently not allowed in ENDF format
          NRO=1
C...
          NR =1
          NP =LR
          NBT(1)=NP
          INT(1)=2
        END IF
C* Take the first value of the scattering radius if NRO=0
        AP=XSC(1,IR)
      ELSE
        AP=1.35*AWR**(2./3)
      END IF
C* Write the file
      NS=0
      CALL WRCONT(LOU,MAT, MF, MT,NS, ZA,AWR,IZR,IZR,NIS,IZR)
      CALL WRCONT(LOU,MAT, MF, MT,NS, ZA,ABN,IZR,LFW,NER,IZR)
      CALL WRCONT(LOU,MAT, MF, MT,NS, EL, EH,IZR,IZR,NRO,IZR)
      IF(NRO.GT.0) THEN
        CALL WRTAB1(LOU,MAT,MF,MT,NS,ZRO,ZRO,IZR,IZR,NR,NP
     &             ,NBT,INT,EIN,XSC(1,IR))
      END IF
      CALL WRCONT(LOU,MAT, MF, MT,NS,SPI, AP,IZR,IZR,IZR,IZR)
C* SEND, FEND records          
      CALL WRCONT(LOU,MAT, MF,IZR,NS,ZRO,ZRO,IZR,IZR,IZR,IZR)
      CALL WRCONT(LOU,MAT,IZR,IZR,NS,ZRO,ZRO,IZR,IZR,IZR,IZR)
      RETURN
      END
      SUBROUTINE WRIMF3(LOU,MXE,MXT,MXR,MF
     1                 ,EIN,XSC,QQM,QQI,MTH,RWO
     1                 ,MAT,IZI,IZA,AWR,NEN,NEP,NXS,ERR,NS)
C-Title  : WRIMF3 Subroutine
C-Purpose: Write cross section (file MF3) data in ENDF-6 format
C-Description:
C-D  Sort printout in ascending order of MT numbers
C-D  Skip reactions flagged -ve
C-D  Processed reactions are flagged by adding 1000 to MT
C-
      CHARACTER*8  PTST
      DIMENSION    EIN(MXE),XSC(MXE,MXT),MTH(MXT),QQM(MXT),QQI(MXT)
     1            ,RWO(MXR),NBT(1),INT(1)
C* Cross sections are set to zero if Ln(cross-sect.) < SMALL
      DATA XSMALL,SMALL,ZRO/ 1.0E-34, -34., 0./
      DATA PTST/'        '/
C* Initialize constants
      QM=0.
      QI=0.
      CALL POUCHR(PTST,IZI,AWI)
C*
C* Write file MF3 (cross section data) or MF1 (tabulated data)
      ZA =IZA
      DO 360 JT=1,NXS
C* Select MT numbers in ascending order
      IT =0
      MT =1000
      DO J=1,NXS
C* Select the cross section with the lowest MT number
C* Exclude cross sections flagged "processed" (MT>1000)
C* Special processing of MF 1 tabulated data (MT 450-460)
        MTJ=MTH(J)
        IF((MF.EQ.1 .AND. (-MTJ.GT.450 .AND. -MTJ.LT.460)) .OR.
     &     (MF.EQ.3 .AND. (MTJ.GT.   0 .AND.
     &                     MTJ.LT.1000 .AND.
     &                     MTJ.LT.  MT) ) ) THEN
            IT =J
            MT =ABS(MTJ)
        END IF
      END DO
      IF(IT.EQ.0) GO TO 360
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
      ETH=MAX((-QQI(IT))*(AWR+AWI)/AWR , EIN(1) )
      XSL=SMALL
      XMX=SMALL
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
          XMX=MAX(XMX,XSL)
        ELSE
          RWO(MY+ME)=SMALL
        END IF
        IF(ME.NE.1 .OR. RWO(MY+ME).GT.SMALL) ME=ME+1
c...
c...    print *,'j,N,me,mt,xsl,e,x'
c... &          ,j,NEN,me,mth(it),xsl,ein(j),xsc(j,it),XMX
c...
  322 CONTINUE
C* Check that there are NON-ZERO points above threshold
      IF(ME.LT.2 .OR.
     1  (ME.EQ.2 .AND.XMX.LE.SMALL)) THEN
C*        Flag MT "+2000" to prevent processing double differential data
        MTH(IT)=MTH(IT)+2000
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
C* Check fitted data and remove zero x-sect points below threshold
  350 KX=1
C* Find pseudo-threshold
      DO WHILE (KX.LT.NEN .AND. XSC(KX+1,IT).LT.XSMALL)
        KX=KX+1
      END DO
      EPT=EIN(KX)
C* Remove redundant zeroes below pseudo-threshold
      DO WHILE (RWO(LX+1).LT.EPT)
        NEO=NEO-1
        LX=LX+1
        LY=LY+1
        RWO(LX  )=RWO(LX-1)
        RWO(LY  )=0
        RWO(LY+1)=0
      END DO
C* Write HEAD record
      L1=0
      L2=0
      N1=0
      N2=0
      IF(MF.EQ.1) L2=2
      CALL WRCONT(LOU,MAT,MF,MT,NS, ZA,AWR,L1,L2,N1,N2)
C* Write TAB1 record
      NR    =1
      INT(1)=2
      NBT(1)=NEO
      CALL WRTAB1(LOU,MAT,MF,MT,NS,QQM(IT),QQI(IT), 0, 0
     1           ,NR,NEO,NBT,INT,RWO(LX),RWO(LY))
C* Write CONT record - end of data set
      NS=99998
      CALL WRCONT(LOU,MAT,MF, 0,NS,ZRO,ZRO, 0, 0, 0, 0)
      NS=0
C* Flag reaction as processed by adding 1000 to the MT
      MTFLG=ABS(MTH(IT))
      IF(MTFLG.LT.1000) THEN
        MTFLG=MTFLG+1000
        IF(MTH(IT).LT.0) MTFLG=-MTFLG
        MTH(IT)=MTFLG
      END IF
  360 CONTINUE
C* All cross sections processed
      NS=-1
      CALL WRCONT(LOU,MAT, 0, 0,NS,ZRO,ZRO, 0, 0, 0, 0)
C* Change back the MT numbers that were flagged "+1000"
      DO IT=1,NXS
        MTFLG=ABS(MTH(IT))
        IF(MTFLG.GT.1000 .AND. MTFLG.LT.4999) THEN
          MTFLG=MTFLG-1000
          IF(MTH(IT).LT.0) MTFLG=-MTFLG
          MTH(IT)=MTFLG
        END IF
      END DO
C*
      RETURN
      END
      SUBROUTINE WRIMF4(LOU,LTT,LER,MTH,QQM,QQI,NXS,MT6,RWO
     1                 ,MT,MAT,IZA,IZI,AWR,LCT,NS)
C-Title  : WRIMF4 Subroutine
C-Purpose: Write angular distributions (file-4) data in ENDF-6 format
      PARAMETER   (MXQ=80)
      CHARACTER*8  PTST
      DOUBLE PRECISION DEIN,DAWR,DAWI,DAWP,DQQI
      DIMENSION    RWO(1),QQM(1),QQI(1),MTH(1),NBT(1),INT(1)
      DIMENSION    QQ(MXQ)
C* Tolerance limit for energy levels (eV)
      DATA DLVL/1.E3/
C*
      DATA ZRO/0./
      DATA PTST/'        '/
C*
      CALL POUCHR(PTST,IZI,AWI)
C* Find the appropriate discrete level MT number
      IF(MT6.EQ. 91 .OR.
     1   MT6.EQ.  5) MT=MAX(MT, 50)
      IF(MT6.EQ.649) MT=MAX(MT,600)
      IF(MT6.EQ.849) MT=MAX(MT,800)
      IF(IZI.EQ.1 .AND. MT.EQ.50) MT=MT+1
      DO 20 JT=1,NXS
      IT=JT
      IF(ABS(MTH(IT)).EQ.MT ) GO TO 22
   20 CONTINUE
      GO TO 80
C*
C* Write file MF4 angular distributions (first outgoing particle)
   22 MF =4
      ZA =IZA
      LTTE=1
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
      ETH=MAX((-QQI(IT))*(AWR+AWI)/AWR, RWO(LL))
C*
C* Loop over the incident particle energies
      DO 40 IE=1,NE
      TT  =0.
      LT  =0
      EIN =RWO(LL  )
      NA  =NINT(RWO(LL+1))
      NW  =NINT(RWO(LL+2))
      NEP =NINT(RWO(LL+3))
      NA1 =NA+1
      LL  =LL+4
C* Determine the outgoing particle energy
      IF(MT.EQ.2) THEN
C*      Process all energies for elastic
        EOU=0
      ELSE
C*      Determine the outgoing particle energy for discrete levels
c...    DEIN=DBLE(EIN)
c...    DAWR=DBLE(AWR)
c...    DAWI=DBLE(AWI)
c...    DAWP=DBLE(AWP)
c...    DQQI=DBLE(QQI(IT))
c...    EOU=(DEIN*DAWR/(DAWR+DAWI)+DQQI)*((DAWR+DAWI-DAWP)/(DAWR+DAWI))
c...
        EOU=(EIN*AWR/(AWR+AWI)+QQI(IT))*((AWR+AWI-AWP)/(AWR+AWI))
c...
        EOU=-EOU
      END IF
      IF(EIN-ETH.LT.-1.E-4 .OR.
     1   ABS(EOU)/EIN.LT.-1.E-4) THEN
C* Skip points below threshold
        JE=JE-1
        LL=LL+NW
        GO TO 40
      END IF
C...
C...  print *,'writing Ein,Eou',EIN,Eou
C...
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
        CALL WRCONT(LOU,MAT,MF,MT,NS, ZA,AWR,LVT,LTTE, 0, 0)
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
C* Check if discrete level data are present
      IF(E2.GE.0) EOU=ABS(EOU)
C*
      IF(NEP.LE.1) THEN
C* Copy the coefficients if a single point is given
        IF(NA1.GT.MXQ) STOP 'EMPEND ERROR - MXQ Lim.in WRIMF4 exceeded'
        CALL FLDMOV(NA1,RWO(L2+1),QQ)
      ELSE
C* Linearly interpolate Legendre coefficients to EOU
        IEP =1
   38   IEP =IEP+1
        L1  =L2
        E1  =E2
        L2  =L1+NA+2
        E2  =RWO(L2)
        TST =DLVL*EIN*1E-6
C* Read until EOU is enclosed by E1 and E2
        IF(E2.LT.EOU .AND. IEP.LT.NEP) GO TO 38
c...
c...    if(-eou.gt.2.40e6 .and. -eou.lt.2.41e6) then
c...      print *,'EIN,EOU,e1,e2,AWR,AWI,AWP,QQI'
c... &            ,EIN,EOU,e1,e2,AWR,AWI,AWP,QQI(IT)
c...    end if
c...
C* Check the closest
        DE1=ABS(EOU-E1)
        DE2=ABS(EOU-E2)
        IF(MIN(DE1,DE2).GT.TST .AND. NA1.GT.1) THEN
C* No matching levels, linearly interpolate (except if isotropic)
          CALL FLDINT(NA1,E1,RWO(L1+1),E2,RWO(L2+1),EOU,QQ)
          WRITE(LTT,910) 4,MT,EIN,QQM(IT)-QQI(IT),EOU,E1,E2
          WRITE(LER,910) 4,MT,EIN,QQM(IT)-QQI(IT),EOU,E1,E2
C...
C...          print *,'Inter MT,Ein,Eou,E1,E2,Elvl',MT,EIN,EOU,E1,E2
C... 1               ,QQM(IT)-QQI(IT)
C...          print *,e1,(rwo(l1+j),j=1,NA1)
C...          print *,e2,(rwo(l2+j),j=1,NA1)
C...          read (*,'(a1)') yes
C...
C...          print *,'AWR,AWI,AWP,QI',AWR,AWI,AWP,QQI(IT)
C...
        ELSE IF(DE1.LT.DE2) THEN
C* Move lower point
          CALL FLDMOV(NA1,RWO(L1+1),QQ)
c...
c...          print *,'Match MT,Ein,Eou,E1,Elvl',MT,EIN,EOU,E1
c... 1               ,QQM(IT)-QQI(IT)
c...
        ELSE
C* Move upper point
          CALL FLDMOV(NA1,RWO(L2+1),QQ)
c...
c...          print *,'Match MT,Ein,Eou,E2,Elvl',MT,EIN,EOU,E2
c... 1               ,QQM(IT)-QQI(IT)
c...
        END IF
      END IF
      IF(NA.LE.0 .OR. ABS(QQ(1)).LT.1.E-20) THEN
        NA=1
        QQ(2)=0.
      ELSE
        RR  =1./QQ(1)
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
      IF(J2.GT.0) THEN
        NS=99998
        CALL WRCONT(LOU,MAT,MF, 0,NS,ZRO,ZRO, 0, 0, 0, 0)
        NS=0
      END IF
      RETURN
C* All discrete levels processed - nothing to write
   80 MT=0
      RETURN
  910 FORMAT(' EMPEND WARNING - MF/MT/Ein/Elvl/Eout'
     &      ,I3,I4,1P,E10.3E1,2E12.5E1/
     &       '                  Distribution interpolated between   '
     &      ,2E12.5E1)
      END
      SUBROUTINE WRMF6Y(LOU,MXE,MXT,MXR
     1                 ,EIN,XSC,QQM,QQI,MTH,RWO
     1                 ,MAT,IZI,IZA,AWR,NEN,NEP,NXS,ERR,NS)
C-Title  : WRMF6Y Subroutine
C-Purpose: Write unassigned react.yields (MF6) data in ENDF-6 format
C-Description:
C-D  Proton and alpha producing reaction cross sections that do not
C-D  have distributions given or do not have MT numbers are stored
C-D  separately. If partly-inclusive spectra are not given, the
C-D  yields for proton and alpha production are given in MF6.
C-
      CHARACTER*8  PTST
      DIMENSION    EIN(MXE),XSC(MXE,MXT),MTH(MXT),QQM(MXT),QQI(MXT)
     1            ,RWO(MXR),NBT(1),INT(1)
      DATA PTST/'        '/
C* Initialize constants
      QM=0.
      QI=0.
      CALL POUCHR(PTST,IZI,AWI)
C* Identify indices of MT 5,201,203,207
      NK =0
      MT5=0
      MT201=0
      MT203=0
      MT207=0
      DO I=1,NXS
        IF(IABS(MTH(I)).EQ.  5) MT5  =I
        IF(IABS(MTH(I)).EQ.201) MT201=I
        IF(IABS(MTH(I)).EQ.203) MT203=I
        IF(IABS(MTH(I)).EQ.207) MT207=I
      END DO
      IF(MT5.LE.0) RETURN
C* Convert neutron yields to multiplicities
      IF(MT201.GT.0) THEN
        JK201=0
        DO I=1,NEN
          X5  =XSC(I,MT5)
          X201=XSC(I,MT201)
          IF(X201.GT.0) THEN
            XSC(I,MT201)=X201/X5
            JK201=1
          END IF
        END DO
        NK=NK+JK201
      END IF
C* Convert proton yields to multiplicities
      IF(MT203.GT.0) THEN
        JK203=0
        DO I=1,NEN
          X5  =XSC(I,MT5)
          X203=XSC(I,MT203)
          IF(X203.GT.0) THEN
            XSC(I,MT203)=X203/X5
            JK203=1
          END IF
        END DO
        NK=NK+JK203
      END IF
C* Convert alpha yields to multiplicities
      IF(MT207.GT.0) THEN
        JK207=0
        DO I=1,NEN
          X5  =XSC(I,MT5)
          X207=XSC(I,MT207)
          IF(X207.GT.0) THEN
            XSC(I,MT207)=X207/X5
            JK207=1
          END IF
        END DO
        NK=NK+JK207
      END IF
C*
C* Write file MF6/MT5 (neutron, proton and alpha yield data only)
      MF =6
      MT =5
      ZA =IZA
      LCT=1
      LIP=1
C* Flag unknown distribution (LAW=0)
      LAW=0
C* Write HEAD record
      CALL WRCONT(LOU,MAT,MF,MT,NS, ZA,AWR, 0,LCT,NK, 0)
C*
      LE  =1
      LX  =MXR/2
      ETH =-QQI(MT5)*(AWR+AWI)/AWR
C* Write the yield for neutrons, protons and alphas
      IZAP=1
      IXS =MT201
      JK  =JK201
  200 IF(JK.GT.0) THEN
        CALL POUCHR(PTST,IZAP,AWP)
C* Remove duplicate points
        NP  =2
        IF(ETH.GE.EIN(1)) THEN
          RWO(LE  )=ETH
          RWO(LX  )=0
          I1=1
          DO WHILE (I1.LT.NEN .AND. ETH.GE.EIN(I1))
            I1=I1+1
          END DO
        ELSE
          RWO(LE  )=EIN(1)
          RWO(LX  )=XSC(1,IXS)
          I1=2
        END IF
        RWO(LE+1)=EIN(I1)
        RWO(LX+2)=XSC(I1,IXS)
        I1=I1+1
        DO I=I1,NEN
          IF(XSC(I,IXS).NE.XSC(I-1,IXS)) NP=NP+1
          RWO(LE-1+NP)=EIN(I)
          RWO(LX-1+NP)=XSC(I,IXS)
        END DO
C* Write TAB1 record
        ZAP   =IZAP
        NR    =1
        INT(1)=2
        NBT(1)=NP
        CALL WRTAB1(LOU,MAT,MF,MT,NS,ZAP,AWP,LIP,LAW
     1             ,NR,NP,NBT,INT,RWO(LE),RWO(LX))
      END IF
C* Repeat for other particles
      IF(IZAP.EQ.1) THEN
        IZAP=1001
        IXS =MT203
        JK  =JK203
        GO TO 200
      ELSE IF(IZAP.EQ.1001) THEN
        IZAP=2004
        IXS=MT207
        JK =JK207
        GO TO 200
      END IF
C* Write CONT record - end of data set
      NS=99998
      CALL WRCONT(LOU,MAT,MF, 0,NS,ZRO,ZRO, 0, 0, 0, 0)
      NS=0
C*
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
      DO IK=1,NK
        ZAP=RWO(LL  )
        AWP=RWO(LL+1)
        LIP=RWO(LL+2)+0.1
        LAW=RWO(LL+3)+0.1
        NR =RWO(LL+4)+0.1
        NP =RWO(LL+5)+0.1
c...
c...      print *,'mf,mt,np,zap,awp',mf,mt,np,zap,awp
c...
        NBT(1)=NP
        INT(1)=2
        LAE=LL+6
        LAG=LAE+NP
        LA1=LAG+NP
        CALL WRTAB1(LOU,MAT,MF,MT,NS,ZAP,AWP,LIP,LAW
     &             ,NR,NP,NBT,INT,RWO(LAE),RWO(LAG))
        LANG  =RWO(LA1   )+0.1
        LEP   =RWO(LA1+ 1)+0.1
        NR    =RWO(LA1+ 2)+0.1
        NE    =RWO(LA1+ 3)+0.1
        NBT(1)=RWO(LA1+ 4)+0.1
        INT(1)=RWO(LA1+ 5)+0.1
        CALL WRTAB2(LOU,MAT,MF,MT,NS,0.,0.,LANG,LEP
     &             ,NR,NE,NBT,INT)
        LL=LL+12+2*NP
C* Loop over the incident particle energies
        IF(NE.GT.0) THEN
          DO IE=1,NE
            ND  =0
            EOU =RWO(LL  )
            NA  =RWO(LL+1)+0.1
            NW  =RWO(LL+2)+0.1
            NEP =RWO(LL+3)+0.1
            LL  =LL+4
            CALL WRLIST(LOU,MAT,MF,MT,NS,0.,EOU,ND,NA,NW,NEP,RWO(LL))
            LL  =LL+NW
          END DO
        END IF
      END DO
      NS=99998
      CALL WRCONT(LOU,MAT,MF, 0,NS,ZRO,ZRO, 0, 0, 0, 0)
      NS=0
C*
      RETURN
      END
      SUBROUTINE WRMF10(LOU,MXE,MXT,MXI,MXR,MF
     1                 ,EIN,XSC,QQM,QQI,MTH,IWO,RWO
     1                 ,MAT,IZI,IZA,AWR,NEN,NEP,NXS,ERR,N10,NRC,NS)
C-Title  : WRMF10 Subroutine
C-Purpose: Write activation cross sections (file MF10) in ENDF-6 format
C-Description:
C-D - Print activation reactions stored with MT=10*ZAP+LFS
C-D   (LFS=5 is a flag for lumped reactions into MT5; set LFS=0)
C-D - Determine MT numbers internally
C-D - Sort printout in ascending order of MT numbers
C-
      CHARACTER*8  PTST
      DIMENSION    EIN(MXE),XSC(MXE,MXT),MTH(MXT),QQM(MXT),QQI(MXT)
     1            ,RWO(MXR),IWO(MXI),NBT(1),INT(1)
C* Cross sections are set to zero if Ln(cross-sect.) < SMALL
      DATA XSMALL,SMALL,ZRO,IZRO/ 1.0E-34, -34., 0., 0/
      DATA PTST/'        '/
C*
C* Scan activation cross sections for associated MT numbers
      N10=0
      NRC=0
      ZRO=0
      CALL POUCHR(PTST,IZI,AWI)
c...
      print '(10I8)',(mth(i),i=1,nxs)
c...
      IF(NXS.GT.MXI) STOP 'WRMF10 ERROR - MXI limit exceeded'
      DO I=1,NXS
        IF(MTH(I).GT.9999) THEN
C*        -- Assign MT number from residual ZA
          JZA=MTH(I)/10
          CALL EMTIZA(IZI,IZA,JZA,MT)
          IF(MT.EQ. 50) MT=  4
          IF(MT.EQ.600) MT=103
          IF(MT.EQ.800) MT=107
          IF(MT.EQ.0) THEN
            MT=5
          ELSE
C*          -- Check if reaction is to be lumped with MT5
C... does not work - sign redefined???
C...        DO J=1,NXS
C...          IF(MT.EQ.-MTH(J)) THEN
C...            MT=5
C...            EXIT
C...          END IF
C...        END DO
          END IF
          IWO(I)=MT
          NRC=NRC+1
        ELSE
          IWO(I)=1000
        END IF
      END DO
C* Find reaction set with lowest MT number - save index list in LRC
  100 LRC=1+NXS
      JXS=0
      MT =1000
      DO I=1,NXS
        IF(IWO(I).LT.MT) THEN
          JXS=1
          IWO(LRC-1+JXS)=I
          MT=IWO(I)
        ELSE IF(IWO(I).EQ.MT) THEN
          JXS=JXS+1
          IF(LRC-1+JXS.GT.MXI) STOP 'WRMF10 ERROR - MXI limit exceeded'
          IWO(LRC-1+JXS)=I
        END IF
      END DO
C* Check if last reaction
      IF(MT.GE.1000) GO TO 200
c...
      print *,'found',jxs,' reaction(s) for MT',mt,' at index'
     &       ,(iwo(lrc-1+j),j=1,jxs)
c...
C*
C* Save reaction labels for sorting - save list for current MT in LZA
      LZA=LRC+JXS
      IF(LZA+JXS.GT.MXI) STOP 'WRMF10 ERROR - MXI limit exceeded'
      DO J=1,JXS
        L=IWO(LRC-1+J)
        IWO(LZA-1+J)=MTH(L)
      END DO
C*
C* Write HEAD record
      L1=0
      L2=0
      N2=0
      ZA=IZA
      CALL WRCONT(LOU,MAT,MF,MT,NS, ZA,AWR,L1,L2,JXS,N2)
C*
C* Sort constituent reactions by residual in ascending order
      JRC=0
  120 MRC=2000000
      IT =0
      DO J=1,JXS
        IF(IWO(LZA-1+J).LT.MRC) THEN
C*        -- Found index with lower reaction number
          IT=IWO(LRC-1+J)
          JX=J
          MRC=IWO(LZA-1+J)
        END IF
      END DO
      IF(IT.LE.0) GO TO 200
C* Flag residual for this reaction processed
      IWO(LZA-1+JX)=IWO(LZA-1+JX)+2000000
      JZA=MTH(IT)/10
      LFS=MTH(IT)-JZA*10
      IF(LFS.GE.5) LFS=LFS-5
c...
c...  print *,'    writing reaction at index',IT
c... &       ,' residual',jza,' state',lfs
c...
      NEO=NEP*NEN *2
      LX =1
      LY =LX+NEO
      MX =LY+NEO
      MY =MX+NEN+1
      LS =MY+NEN+1
      LBL=MX
      IF(LS+3*NEN+MAX(NEN,NEO).GT.MXR)
     1 STOP 'EMPEND ERROR - MXR limit exceeded in WRMF10'
C* Define the threshold
      ETH=MAX((-QQI(IT))*(AWR+AWI)/AWR , EIN(1) )
      XSL=SMALL
      XMX=SMALL
C* Define the input energy points for the spline fit
      ME =0
      ISMAL=0
      DO J=1,NEN
        IF(EIN(J).GE.ETH) THEN
          IF(EIN(J).GT.ETH .AND. ME.EQ.0) THEN
C* Threshold case
            RWO(MX+ME)=ETH
            RWO(MY+ME)=SMALL
            ME=ME+1
c...   
c...           print *,'threshold me,e,x',me,rwo(mx-1+me),rwo(my-1+me)
c...   
          END IF
          RWO(MX+ME)=EIN(J)
          IF(XSC(J,IT).GT.0) THEN
C* Normal case - enter previous point if small
            IF(ISMAL.GT.1) THEN
              RWO(MX+ME)=EIN(J-1)
              RWO(MY+ME)=SMALL
              ME=ME+1
c...
c...              print *,'after me,e,x',me,rwo(mx-1+me),rwo(my-1+me)
c...
              RWO(MX+ME)=EIN(J)
            END IF
C* Normal case
            XSL=ALOG(XSC(J,IT))
            RWO(MY+ME)=XSL
            XMX=MAX(XMX,XSL)
            ISMAL=0
            ME=ME+1
c...
c...              print *,'normal me,e,x',me,rwo(mx-1+me),rwo(my-1+me)
c...
          ELSE
C* Special treatment of small cross sections
            RWO(MY+ME)=SMALL
            ISMAL=ISMAL+1
            IF(ISMAL.EQ.1 .OR. J.EQ.NEN) THEN
              ME=ME+1
c...
c...              print *,'small me,e,x',me,rwo(mx-1+me),rwo(my-1+me)
c...
            END IF
          END IF
c...
c...    print *,'j,N,me,mt,xsl,e,x'
c... &          ,j,NEN,me,mth(it),xsl,ein(j),xsc(j,it),XMX
c...
        END IF
      END DO
C* Define the output grid for spline interpolated function
      FNP=NEP
      IF(ME.LE.3) FNP=1
      NE1=1
      E2 =RWO(MX)
      RWO(LX)=E2
      RWO(LY)=RWO(MY)
      DO J=2,ME
        E1 =E2
        E2 =RWO(MX-1+J)
        DE=(E2-E1)/FNP
        DO K=1,NEP
          EE= E1+(K-1)*DE
          IF(EE.GT.1.001*ETH) THEN
            RWO(LX+NE1)=EE
            RWO(LY+NE1)=RWO(MY-1+J)
            NE1=NE1+1
          END IF
        END DO
      END DO
      RWO(LX+NE1)=RWO(MX-1+ME)
      RWO(LY+NE1)=RWO(MY-1+ME)
      NE1=NE1+1
      IF(ME.LE.3) GO TO 132
C* Log-lin interpolate from threshold
      JX=MX
      JY=MY
      JE=ME
      KX=LX
      KY=LY
      KE=NE1
C* Linearly interpolate the first non-zero x-sect on output grid
  126 IF(RWO(JY).GT.SMALL) GO TO 130
      RWO(KY)=RWO(JY)
     1 +(RWO(JY+1)-RWO(JY))*(RWO(KX)-RWO(JX))/(RWO(JX+1)-RWO(JX))
      KX=KX+1
      KY=KY+1
      KE=KE-1
      IF(KE.LT.1) GO TO 132
      IF(RWO(KX).LT.RWO(JX+1)) GO TO 126
      IF(JE.LE.2) GO TO 132
      JX=JX+1
      JY=JY+1
      JE=JE-1
      GO TO 126
C* Spline fit log of the cross section (oner non-zero range)
  130 F1=0.
      F2=0.
      CALL FINSP3(RWO(JX),RWO(JY),JE
     &           ,RWO(KX),RWO(KY),RWO(KY),KE,F1,F2,RWO(LS)) 
C* Convert back to cross sections from log
  132 DO J=1,NE1
        R=RWO(LY-1+J)
        RWO(LY-1+J)=0.
        IF(R.GT.SMALL) RWO(LY-1+J)=EXP(R)
c...        
c...        print *,j,rwo(lx-1+j),rwo(ly-1+j)
c...
      END DO
C* Thin the cross section to ERR lin.interp. tolerance limit
      NEO=NE1
      IF(ERR.GT.0)
     1CALL THINXS(RWO(LX),RWO(LY),NE1,RWO(LX),RWO(LY),NEO,ERR)
C* Check fitted data and remove zero x-sect points below threshold
      KX=1
C* Find pseudo-threshold
      DO WHILE (KX.LT.NEN .AND. 
     &         (XSC(KX,IT).LT.XSMALL .AND. XSC(KX+1,IT).LT.XSMALL))
        KX=KX+1
      END DO
      EPT=EIN(KX)
C* Remove redundant zeroes below pseudo-threshold
      DO WHILE (RWO(LX+1).LT.EPT)
        NEO=NEO-1
        LX=LX+1
        LY=LY+1
        RWO(LX  )=RWO(LX-1)
        RWO(LY  )=0
        RWO(LY+1)=0
      END DO
C* Write TAB1 record
      NR    =1
      INT(1)=2
      NBT(1)=NEO
      CALL WRTAB1(LOU,MAT,MF,MT,NS,QQM(IT),QQI(IT),JZA,LFS
     1           ,NR,NEO,NBT,INT,RWO(LX),RWO(LY))
C*
C* Proceed to the next reaction of the current MT
      JRC=JRC+1
c...
c...  print *,'      reaction',jrc,' for mt/za',mt,jza,' processed'
c...
      IF(JRC.LT.JXS) GO TO 120
C* Flag reactions processed and try the next MT set
      NS=99998
      CALL WRCONT(LOU,MAT,MF,IZRO,NS,ZRO,ZRO,IZRO,IZRO,IZRO,IZRO)
      N10=N10+1
      DO J=1,JXS
        L=IWO(LRC-1+J)
        IWO(L)=IWO(L)+1000
      END DO
c...
c...  print *,'     wrote cont for',n10,'-th reaction mt',mt
c...  print *,(iwo(iwo(lrc-1+j)),j=1,jxs)
c...
      GO TO 100
C*
C* All reactions processed
  200 CONTINUE
C* Write the FEND record
      IF(N10.GT.0) THEN
        NS=0
        CALL WRCONT(LOU,MAT, 0, 0,NS,ZRO,ZRO, 0, 0, 0, 0)
      END IF
      RETURN
      END
      SUBROUTINE WRMF12(LOU,MAT,MT0,IZA,AWR,NLV,NL1,ENL,NBR,LBR,BRR
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
C* Loop over all discrete levels
      DO LL=1,NLV
C* Number of levels below the present one
        NS=NL1+LL-1
C* Define reaction type MT number
C...    MT=MT0+NS
        MT=MT0+LL-1
C* Energy of the level
C...    ES=ENL(NS+1)
        ES=ENL(LL)
C* Number of levels with non-zero branching ratio
C...    NT=NBR(NS+1)
        NT=NBR(LL)
        IF(NT.GT.0) THEN
C* Head record
          CALL WRCONT(LOU,MAT,MF,MT,NS, ZA,AWR, L0, LG, NS,  0)
C* List record
          DO JT=1,NT
C* Determine the energy level of the final state (sort in descending order)
c... Level energies sorted in descending order (fix error that reappeared?)
c...        LE=LBR(NT+1-JT,LL)
C...        LE=LBR(JT,NS+1)
            LE=LBR(JT,LL)
            RWO(1,JT)=ENL(LE)
C* Determine the branching fraction for this level
c...        RWO(2,JT)=BRR(NT+1-JT,LL)
C...        RWO(2,JT)=BRR(JT,NS+1)
            RWO(2,JT)=BRR(JT,LL)
          END DO
          CALL WRLIST(LOU,MAT,MF,MT,NS,ES, 0., LP,  0,2*NT,NT,RWO)
C* Section end
          NS=99998
          CALL WRCONT(LOU,MAT,MF, 0,NS,ZRO,ZRO, 0, 0, 0, 0)
          NS=0
        END IF
C*
      END DO
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
      MT=MT0+LL-1
C* Assume isotropic photon distribution
      CALL WRCONT(LOU,MAT,MF,MT,NS, ZA,AWR, 1, 0, 1, 0)
C* Section end
      NS=99998
      CALL WRCONT(LOU,MAT,MF, 0,NS,ZRO,ZRO, 0, 0, 0, 0)
      NS=0
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
      IF(NS.GT.99999) NS=0
      IF(MT.EQ.0)     NS=99999
      IF(MF.EQ.0)     NS=0
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
      IF(NZ.LE.0) RETURN
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
C-D  proceeds up to LMX or NP*2/3, whichever is smaller. If LMX is
C-D  smaller or equal to LMI, the second condition prevails. The
C-D  procedure is terminated earlier if the maximum relative difference
C-D  between an input and a calculated point value is smaller than EMM.
C-D    A scratch array RWO of length MXR is needed, where the value of
C-D  MXR does not exceed LP*4+(LMX+4)*(LMX+1) .
C-D    On output, the Legendre coefficients are stored in QQ. The actual
C-D  order of Legendre polynomials used is contained in LMX.
C-D  On exit, ERR contains the actual maximum difference between
C-D  the input and the fitted data relative to the average. If the
C-D  fitted distribution is negative, ERR contains the most negative
C-D  value.
C-External: LSQLEG, MTXGUP, PLNLEG, POLLG1
C-
      PARAMETER (MXEH=120)
      DIMENSION  ERHI(MXEH)
      DIMENSION  XP(1),YP(1),QQ(1),RWO(1)
      ERR=0
      NLG=0
      LM0=LMX
      LM1=LM0+1
      LST=0
      LO1=1
c...
c...      LMX=MIN(LMX,NP-1)
c...
C* Check if zero-order
      QQ(1)=YP(1)
      IF(NP.LT.2) GO TO 40
      SY =0.
      DO I=2,NP
        SY =SY + 0.5*(YP(I)+YP(I-1))*(XP(I)-XP(I-1))
      END DO
      QQ(1)=SY/(XP(NP)-XP(1))
      IF(LMX.LT.1) GO TO 30
C* Clear the coefficients field
      DO L=1,LM1
        QQ(L+1)=0.
      END DO
C* Save the input points, allow for quadrupling the mesh
      MXP=4*NP
      NNP=NP
      LXP=1
      LYP=LXP+MXP
      LLG=LYP+MXP
      DO I=1,NP
        RWO(LXP-1+I)=XP(I)
        RWO(LYP-1+I)=YP(I)
      END DO
C*
C* Loop to find the appropriate Legendre order
      L1 =MAX(1,LMI)
      LL =LLG+LMX+2
      LO1=L1
   20 NLG=LO1
      N1 =NLG+1
      LMM=MIN(LM0,NNP*2/3)
      IF(LL+(NLG+1)*(NLG+3).GT.MXR) 
     1 STOP 'EMPEND ERROR - MXR limit exceeded in LSQLGV'
      CALL LSQLEG(RWO(LXP),RWO(LYP),NNP,RWO(LLG),N1,RWO(LL),JER)
      IF(LST.NE.0) GO TO 40
C* Trap zero-determinant
      IF(JER.NE.0) THEN
        NLG=NLG-1
        GO TO 30
      END IF
C* Save the coefficients
      DO I=1,LM1
        IF(I.LE.N1) THEN
          QQ(I)=RWO(LLG-1+I)
        ELSE
          QQ(I)=0
        END IF
      END DO
C* Check absolute difference between input and calculated points ERR
C* and for negative distributions
   30 ERR=0
      YNP=YP(1)
      YNM=YP(1)
      KNP=0
      KNM=0
      JNP=0
      JNM=0
      JRE=0
      DO IP=1,NP
        YCI=POLLG1(XP(IP),QQ,NLG)
C       RER=ABS((YCI-YP(IP))/YCI)
C       RER=ABS((YCI-YP(IP))/QQ(1))
        RER=ABS((YCI-YP(IP))/MAX(YCI,QQ(1)))
        IF(RER.GT.ERR) THEN
          ERR=RER
          JRE=IP
        END IF
C* Test minimum value of distribution at mesh point
        IF(YCI.LT.YNP) THEN
          IF(YCI.LT.0) KNP=KNP+1
          JNP=IP
          YNP=YCI
        END IF
C* Test minimum value of distribution at midpoint
        IF(IP.LT.NP) THEN
          XPI=(XP(IP)+XP(IP+1))/2
          YCI=POLLG1(XPI,QQ,NLG)
          IF(YCI.LT.YNM) THEN
            IF(YCI.LT.0) KNM=KNM+1
            JNM=IP
            YNM=YCI
          END IF
        END IF
      END DO
c...
c...      PRINT *,EMM,ERR,YNP,YNM,JNM,LO1,JER
c...
      YNP=YNP/QQ(1)
      YNM=YNM/QQ(1)
      IF(YNP.LT.0) ERR=MIN(ERR,YNP)
      IF(YNM.LT.0) ERR=MIN(ERR,YNM)
C*
C* Take corrective action
      IF(KNP.GT. 0 ) THEN
C* Case: Distribution negative at mesh point - increase L
        IF(LO1.LT.LMM) THEN
          IF(LO1.GE.MXEH) STOP 'LSQLGV ERROR - MXEH Limit exceeded'
          ERHI(LO1)=ERR
          LO1=LO1+1
          GO TO 20
        END IF
      END IF
      IF(ERR.GT.EMM) THEN
C* Case: Tolerance limit not satisfied
        IF(LO1.LT.LMM .AND. JER.EQ.0) THEN
C*          Try increasing the order of approximation
c...
c...        print *,'            Increase order to',LO1
c...
          IF(LO1.GE.MXEH) STOP 'LSQLGV ERROR - MXEH Limit exceeded'
          ERHI(LO1)=ERR
          LO1=LO1+1
          GO TO 20
        ELSE
c...
c...        print *,'            Double the mesh to',1+(NNP-1)*2
c...
C*          Try Doubling the mesh
          IF(NNP.LE.MXP/3 .AND. ERR.GT.EMM*2) THEN
            JNP=1+(NNP-1)*2
            DO J=2,NNP
              RWO(LXP+JNP+3-2*J)= RWO(LXP+NNP+1-J)
              RWO(LXP+JNP+2-2*J)=(RWO(LXP+NNP+1-J)+RWO(LXP+NNP  -J))/2
              RWO(LYP+JNP+3-2*J)= RWO(LYP+NNP+1-J)
              RWO(LYP+JNP+2-2*J)=(RWO(LYP+NNP+1-J)+RWO(LYP+NNP  -J))/2
            END DO
            NNP=JNP
            ERHI(LO1)=10*EMM
            GO TO 20
          END IF
        END IF
      END IF
C* Case: Distribution is negative - force extra points
      IF((KNM.GT.0 .OR. KNP.GT.0) .AND. NNP.LT.MXP) THEN
C*    Force extra point if distribution negative at midpoint
        IF(YNP.LT.YNM) THEN
          IP=JNP
        ELSE
          IP=JNM
        END IF
        K=NNP-IP
        DO J=1,K
          RWO(LXP+NNP+1-J)=RWO(LXP+NNP-J)
          RWO(LYP+NNP+1-J)=RWO(LYP+NNP-J)
        END DO
        RWO(LXP+IP)=(RWO(LXP-1+IP)+RWO(LXP+1+IP))/2
        YP1=RWO(LYP-1+IP)
        YP2=RWO(LYP+1+IP)
C*      Assign average value to midpoint
C...        YPA=(YP1+YP2)/2
C*      Assign log-average value to midpoint
        YPA=SQRT(YP1*YP2)
        RWO(LYP+IP)=YPA
        NNP=NNP+1
C...
c...        print *,'insert',ip,rwo(lyp+ip),yp1,yp2
c...     1         ,' at',rwo(lxp+ip),rwo(lxp-1+ip),rwo(lxp+1+ip)
c...
c...        print *,(rwo(lxp-1+j),j=1,nnp)
c...        print *,(rwo(lyp-1+j),j=1,nnp)
c...
        ERHI(LO1)=10*EMM
        GO TO 20
      END IF
C*
C* Check the improvement in last increments of order
c...
      ERHI(LO1)=ERR
      ELS=ERR
      LL0=LO1
c...
C...      print *,'Fitted order: l,els,err',lO1,els,emm
c...
c###  DO WHILE (LO1.GT.1 .AND. ERHI(LO1-1).GT.0 .AND.
C### &          ERHI(LO1-1).LT.2.0*EMM .AND. 
C### &          ERHI(LO1-1).LT.1.2*ELS)
C*      Reduce order as long as error <1.2*last and <2.0*max
      DO WHILE (LO1.GT.1 .AND. ERHI(LO1-1).GT.0 .AND.
     &          ERHI(LO1-1).LT.1.2*ELS)
C*      Reduce order as long as error <1.2*last
        LO1=LO1-1
        ELS=MIN(ELS,ERHI(LO1))
      END DO
c...
c...  if(ll0.ge.64 .and. lo1.ge.64) print *,'Limit 64',erhi(lo1-1),err
c...
      LST=1
      IF(LO1.LT.LL0) GO TO 20
C*
C* Terminate
   40 LMX=NLG
c...
c...      print *,'jnm',jnm,NP,nnp,LMI,LMX,EMM,ERR,ynp,ynm
c...
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
      DO I=1,N1
        AA(I,LF)=0
        DO J=1,N1
          AA(J,I)=0
        END DO
      END DO
C* Set up the matrix
      NLG=N1-1
      DO M=1,NP
C* Calculate Legendre polynomials
        CALL PLNLEG(XP(M),AA(1,LP),NLG)
        DO I=1,N1
          PI=AA(I,LP)
          AA(I,LF)=AA(I,LF)+YP(M)*PI
          DO J=I,N1
            PJ=AA(J,LP)
            AA(J,I)=AA(J,I)+PI*PJ
            AA(I,J)=AA(J,I)
          END DO
        END DO
      END DO
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
      L2=2
      PL(L2)=UU
      IF(NL.LT.2) RETURN
      DO 20 L=2,NL
      PL(L+1)=( PL(L)*UU*(2*L-1) - PL(L-1)*(L-1) )/L
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
