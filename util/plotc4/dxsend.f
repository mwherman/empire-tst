      SUBROUTINE DXSEND(LEF,ZA0,ZAP,MT0,KEA,EIN,PAR,EPS,ENR,DXS
     1                 ,RWO,NEN,MEN,MRW,LTT)
C-Title  : Subroutine DXSEND
C-Purpose: Extract double differential cross sections from ENDF
C-Author : A.Trkov, International Atomic Energy Agency, Vienna, Austria.
C-Version: 15-Dec-2000 (Original code).
C-Description:
C-D  The function of this routine is an extension of DXSEN1, which
C-D  retrieves the differential cross section at a specified incident
C-D  particle energy and scattering angle. If a special MT number
C-D  is specified (for example, MT=5 for particle emission where
C-D  particle is defined by its ZA designation in ZAP), DXSEN1
C-D  is called recursively for all neutron emission reactions and
C-D  all contributions are summed.
C-D  For details see the description of the DXSEN1 routine.
C-Extern.: DXSEN1
C-
      CHARACTER*66 C66
      DIMENSION    RWO(MRW),ENR(MEN),DXS(MEN)
     1            ,LST(100)
C*
      LOOP=0
      LX  =1
      NE1 =0
      KRW =MRW
      MT  =MT0
      IF(MT0.NE.5) GO TO 40
C* Find all neutron emission reactions
      REWIND LEF
      CALL SKIPSC(LEF)
      IZAP=ZAP+0.1
      IF(IZAP.NE.1) GO TO 24
C* Prepare reaction list for neutron emission
   20 MF=3
      MT=0
      CALL FINDMT(LEF,ZA0,ZA,MAT,MF,MT,C66,IER)
      IF(IER.NE.0) GO TO 30
      CALL SKIPSC(LEF)
C* Select contributiong reactions
      IF(MT.EQ.11) GO TO 22
      IF(MT.GE.16 .AND. MT.LE.17) GO TO 22
      IF(MT.GE.22 .AND. MT.LE.25) GO TO 22
      IF(MT.GE.28 .AND. MT.LE.30) GO TO 22
      IF(MT.GE.32 .AND. MT.LE.37) GO TO 22
      IF(MT.GE.41 .AND. MT.LE.45) GO TO 22
      IF(MT.EQ.91) GO TO 22
C* Process elastic and discrete inelastic only if requesting
C* energy distributions or aver. energy ang.distrib.
      IF( (KEA.EQ.2  .OR. (KEA.EQ.1 .AND. PAR.LE.0)) .AND.
     1  ( MT.EQ. 2   .OR. ( MT.GE.50 .AND. MT.LE.90)) ) GO TO 22
C...  IF(MT.EQ. 2 .OR. (MT.GE.50 .AND. MT.LE.90) ) GO TO 22
      GO TO 20
   22 LOOP=LOOP+1
      LST(LOOP)=MT
      GO TO 20
C*
   24 CONTINUE
C* Not coded for other particles
      STOP 'DXSEND ERROR - No coding for requested particle'
C* All reactions found - begin processing
   30 CONTINUE
      IL=1
      MT=LST(IL)
C*
C* Retrieve the double differential cross section energy distribution
   40 CALL DXSEN1(LEF,ZA0,ZAP,MT,KEA,EIN,PAR,EPS,ENR,DXS
     1           ,RWO(LX),NEN,MEN,KRW,IER)
      IF( IER.NE.0) THEN
        IF(LTT.GT.0) THEN
        WRITE(LTT,903) ' DXSEND WARNING - Error condition flag  ',IER
        WRITE(LTT,903) '              Failed reconstruct. for MT',MT
        END IF
        GO TO 54
      END IF
      IF(LOOP.EQ.0) GO TO 90
      IF( NE1.EQ.0) GO TO 50
      IF( NEN.EQ.0) GO TO 54
C* Move the previously saved distribution in the work field
      LX=MRW/2
      DO 42 I=1,NE1
      RWO(LX-1+I)=RWO(NE1+I)
   42 CONTINUE
C* Generate the union grid
      LUE= 1+NE1
      LUX=LX+NE1
      KX =MRW-LUX
      CALL UNIGRD(NEN,ENR,NE1,RWO,NE2,RWO(LUE),KX)
C* Interpolate current distribution to the union grid
      CALL FITGRD(NEN,ENR,DXS,NE2,RWO(LUE),RWO(LUX))
      NEN=NE2
      DO 44 I=1,NEN
      ENR(I)=RWO(LUE-1+I)
      DXS(I)=RWO(LUX-1+I)
   44 CONTINUE
C* Interpolate saved distribution to the union grid
      CALL FITGRD(NE1,RWO,RWO(LX),NE2,RWO(LUE),RWO(LUX))
C* Add the current to the saved distribution
      DO 46 I=1,NEN
      DXS(I)=DXS(I)+RWO(LUX-1+I)
   46 CONTINUE
C* Save the summed distribution 
   50 LX=1+NEN*2
      KRW=MRW-LX
      NE1=NEN
      DO 52 I=1,NEN
      RWO(I    )=ENR(I)
      RWO(I+NEN)=DXS(I)
   52 CONTINUE
C* Select next reaction
   54 IF(IL.GE.LOOP) GO TO 90
      IL=IL+1
      MT=LST(IL)
      GO TO 40
C*
C* All processing completed
   90 RETURN
C*
  903 FORMAT(A40,I4)
      END
      SUBROUTINE DXSEN1(LEF,ZA0,ZAP0,MT0,KEA,EIN,PAR,EPS,ENR,DXS
     1                 ,RWO,NEN,MEN,MRW,IER)
C-Title  : Subroutine DXSEN1
C-Purpose: Extract cross sect. and differential cross sect. from ENDF
C-Author : A.Trkov, International Atomic Energy Agency, Vienna, Austria.
C-Version: 15-Dec-2000 (Original code).
C-Description:
C-D  The routine reads an ENDF file and extract cross sections (KEA=0),
C-D  differential cross section (angular distributions KEA=1 or energy
C-D  spectra KEA=2, parameter PAR < -2) and double differential cross
C-D  sections (correlated energy/angle distributions with the same
C-D  conventions for KEA. Parameter PAR is the requested outgoing
C-D  particle energy when the correlated angular distribution are
C-D  requested. Similarly, PAR is the cosine of the scattering angle
C-D  when the spectrum at a particular scattering angle is requested.
C-D  Differential cross sections are output in the Lab co-ordinate
C-D  system.
C-D
C-D  Formal parameters are:
C-D  LEF  - File unit number from which the ENDF file is read.
C-D  ZA0  - Requested nuclide identification number. If ZA>0 it is
C-D         given in terms of Z*1000+A+LIS0/10 where Z is the atomic
C-D         number, A the mass number and LIS0 the metastable state
C-D         number. When ZA<0 it implies the ENDF material MAT number.
C-D  ZAP0 - Outgoing particle ZA designation (ZAP0=1 for neutrons).
C-D  MT0  - Requested reaction number. Broadly this follows the ENDF
C-D         conventions.
C-D  KEA  - Control flag to select retrieval of cross section (KEA=0)
C-D         angular distributions (KEA=1) of energy spectra (KEA=2).
C-D  EIN  - Incident particle energy (eV).
C-D  PAR  - Fixed parameter when requesting differential data:
C-D         KEA=1, PAR is the requested outgoing particle energy.
C-D                A value PAR <= -2 implies integrated distribution
C-D                over all angles.
C-D         KEA=2, PAR is the requested scattering angle (cosine).
C-D                A value PAR <= -2 implies angle integrated energy
C-D                distribution.
C-D  EPS  - Resolution broadening parameter is used for the two-body
C-D         scattering reactions like the elastic and discrete inelastic
C-D         cross sections  where in principle the energy distribution
C-D         is a delta function. For such reactions the energy
C-D         distribution is displayed as a Gaussian distribution where
C-D         EPS the fractional half-width at half-maximum. Such
C-D         representation is convenient for comparison with measured
C-D         data.
C-D  ENR  - Argument vector of the assembled output cross section.
C-D  DXS  - Function vector of the assembled output cross section.
C-D  RWO  - Work array of length MRW.
C-D  NEN  - Number of points in the assembled output cross section
C-D         vector.
C-D  MEN  - Available size of ENR and DXS arrays.
C-D  MRW  - Available size of the RWO work array.
C-D  IER  - Error flag, which is zero on exit if data assembly is
C-D         completed successfully.
C-D
C-Extern.: SKIPSC,FINDMT,RDTAB1,RDTAB2,RDLIST,FINT2D,YTGEOU,FNGAUS,
C-E        FYTG2D,UNIGRD,FITGRD
C-
      CHARACTER*66 C66
      DIMENSION    RWO(MRW),ENR(MEN),DXS(MEN)
      DIMENSION    NBT(100),INR(100)
      DIMENSION    AMU(100),PMU(100)
C*
      DATA PI/3.14159265/
C*
C* Check the requested type of output
      AIN=-2
      EOU=-2
      IF     (KEA.EQ.2) THEN
C* Energy spectrum at fixed scattering angle requested
        AIN=PAR
      ELSE IF(KEA.EQ.1) THEN
C* Angular distribution at fixed outgoing particle energy requested
        EOU=PAR
      END IF
C* Cosine of the scattering angle
      ALB=AIN
C*
      REWIND LEF
      CALL SKIPSC(LEF)
C* Retrieve the cross section on MF3
      MF =3
      MT =MT0
      CALL FINDMT(LEF,ZA0,ZA,MAT,MF,MT,C66,IER)
      IF(IER.NE.0) GO TO 90
      READ (C66,902) C1,AWR
      NX=MRW/2
      LX=NX+1
      CALL RDTAB1(LEF,C1,QQ,L1,L2,NR,NP,NBT,INR
     1           ,RWO,RWO(LX),NX,IER)
      IF(IER.NE.0) GO TO 90
      IF(EIN.GT.0) THEN
        IF(EIN.LT.RWO(1) .OR. EIN.GT.RWO(NP)) THEN
C* Case: Required point is below thershold or above last point
          NEN=0
          GO TO 90
        END IF
      END IF
      IF(NR.NE.1 .OR. INR(1).GT.2) THEN
        IER=21
        GO TO 90
      END IF
C* Case: Cross section is required
      IF(KEA.EQ.0) THEN
        NEN=NP
        DO 32 I=1,NEN
        ENR(I)=RWO(     I)
        DXS(I)=RWO(LX-1+I)
   32   CONTINUE
        GO TO 90
      END IF
C* Case: Proceed with the retrieval of differential data
      XS=FINTXS(EIN,RWO,RWO(LX),NP,INR,IER)
      IF(IER.NE.0) RETURN
C*
C* Find the energy/angle distribution data
      MF =0
      MT =MT0
      CALL FINDMT(LEF,ZA0,ZA,MAT,MF,MT,C66,IER)
      IF(IER.EQ.0 .AND. MF.EQ.6) GO TO 60
C* No MF4/5/6 possible only for two-body scattering
      IF(IER.NE.0 .AND.
     1  (MT0.NE.2 .AND. (MT0.LT.50 .OR. MT0.GT.90) ) ) GO TO 90
C* Preset isotropic CM angular distribution for this MT
      LCT=2
      NR =1
      INR(1)=2
      NE1=11
      DO 41 I=1,NE1
      ENR(I)=-1+(I-1)*2.0/(NE1-1)
      DXS(I)=0.5
   41 CONTINUE
      ENR(NE1)=1
      IF(IER.NE.0) THEN
C* Check for error condition
        PRINT *,' WARNING - CM isotropic ang.distrib.assumed'
        GO TO 45
      END IF
C*
C* Process angular distribution data in MF 4
   42 READ (C66,902) C1,AWR,LVT,LTT,N1,N2
      IF(LTT.EQ.0) GO TO 45
      IF(LTT.EQ.1) THEN
        PRINT *,' WARNING - Processing not coded for MF4 LTT',LTT
        IER=41
        GO TO 45
      END IF
C* Secondary particle angular distributions MF4 processing
      CALL RDLIST(LEF,C1,C2,LI,LCT,NK,NM,RWO,IER)
C* Split the work array RWO to operate on function and argument
C* LXE - argument
C* LXX - function
C* KX  - Max. permissible number of points per set
      KX =MRW/2
      LXE=1
      LXX=LXE+KX
C* Read incident energy definitions
      CALL RDTAB2(LEF,C1,C2,L1,L2,NR,NE,NBT,INR,IER)
      NM=NR+1
      LXE=1
      KX =(MRW-LXE)/2
      LXX=LXE+KX
      NE1=0
      EI1=0
      DO 44 IE=1,NE
C* For each incident energy read the angular distribution
      CALL RDTAB1(LEF,TEMP,EI2,LT,L2,NRP,NEP1,NBT(NM),INR(NM)
     1           ,RWO(LXE),RWO(LXX),KX,IER)
      IF(IER.NE.0) STOP 'DENXS1 ERROR reading ang.distributions'
C...  IF(NRP.GT.1)
C... 1 PRINT *,' WARNING - Multiple A interp.ranges for MF 4, MT',MT
C* Lin-interpolate outgoing particle energy distributions
      INE=2
      CALL FINT2D(EIN,EI1,NE1 ,ENR     ,DXS     ,INR(NM)
     1               ,EI2,NEP1,RWO(LXE),RWO(LXX),INE,KX)
   44 CONTINUE
C* Integrate over cosine
   45 EA=ENR(1)
      EB=ENR(NE1)
      CALL YTGEOU(SS,EA,EB,NE1,ENR,DXS,INR)
C* Convert to Lab coordinate system if necessary
      IF(LCT.EQ.2) THEN
        GAM=SQRT( EIN/ (EIN*AWR*AWR+QQ*AWR*(AWR+1) ) )
        DO 46 I=1,NE1
        XCM=ENR(I)
        XLB=(GAM+XCM)/SQRT(1+GAM*GAM+2*GAM*XCM)
        DMC=2*GAM*XLB + SQRT(1-GAM*GAM*(1-XLB*XLB))
     1     +(GAM*XLB)**2 / SQRT(1-GAM*GAM*(1-XLB*XLB))
        ENR(I)=XLB
        DXS(I)=DXS(I)*DMC
   46   CONTINUE
      END IF
      YL=1
      IF(KEA.EQ.1) THEN
C* Case: Angular distribution
        YL=1/SS
        GO TO 80
      END IF
      INA=2
      IF(ALB.GE.-1) THEN
C* Case: Specified outgoing particle cosine
        YL=FINTXS(ALB,ENR,DXS,NE1,INA,IER)/SS
        XLB=ALB
      ELSE
C* Case: Angle-integrated energy distribution
        YL =0.5
        DO 47 I=1,NE1
        DXS(I)=DXS(I)*ENR(I)
   47   CONTINUE
        EA=ENR(1)
        EB=ENR(NE1)
        CALL YTGEOU(XLB,EA,EB,NE1,ENR,DXS,INR)
      END IF
      IF(MT0.NE.2 .AND. (MT0.LT.50 .OR. MT0.GT.90) ) GO TO 50
C*
C* Assume two-body scattering for elastic&inelastic scattering
      IER=0
C* Available energy and distribution (gausssian)
      EAV=EIN + QQ*(1+AWR)/AWR
      ECM=EAV*(AWR/(1+AWR))**2
      ACM=-GAM*(1-XLB*XLB) + XLB*SQRT(1-GAM*GAM*(1-XLB*XLB))
      EOU=ECM + ( EIN + 2*ACM*(AWR+1)*SQRT(EIN*ECM) )/(AWR+1)**2
      NE1=21
      CALL FNGAUS(EOU,NE1,ENR,DXS,EPS)
      GO TO 80
C*
C* Process energy distribution data in MF 5
   50 IF(MF.EQ.4) THEN
        MF =5
        MT =MT0
        CALL FINDMT(LEF,ZA0,ZA,MAT,MF,MT,C66,IER)
      END IF
C...
C... Secondary particle energy distributions MF 5 not coded
      PRINT *,' WARNING - Processing not coded for MF5 MT',MT0
      IER=31
      GO TO 90
C*
C* Process coupled energy/angle distributions MF6 (first particle only)
   60 READ (C66,902) C1,AWR, L1,LCT,NK,N2
      JNK=0
   61 JNK=JNK+1
C* Split the work array RWO to operate on function and argument
C* 1   - First argument
C* LX  - First function
C* LXE - Second argument
C* LXX - Second function
C* KX  - Max. permissible number of points per set
      KX =MRW/2
      LX =KX+1
      LD =KX/2
      LXE= 1+LD
      LXX=LX+LD
      KX2=KX/2
      LXF= 1+KX2
      LXY=LX+KX2
C* Retrieve the neutron yield
      CALL RDTAB1(LEF,ZAP,AWP,LIP,LAW,NR,NP,NBT,INR
     1           ,RWO,RWO(LX),KX,IER)
      IF(LAW.NE.7) THEN
        STOP 'DENXS1 ERROR - Not Law-7 in MF 6'
      END IF
      IF(NINT(ZAP).NE.NINT(ZAP0)) THEN
        IF(JNK.GE.NK) STOP 'DENXS1 ERROR - Particle not found'
C* Skip the subsection for this particle
        CALL RDTAB2(LEF,C1,C2,L1,L2,NR,NE,NBT,INR,IER)
        NM=NR+1
        DO 64 IE=1,NE
        CALL RDTAB2(LEF,C1,EI2,L1,L2,NRM,NMU,NBT(NM),INR(NM),IER)
        NO=NM+NMU
        LXE=1
        LXX=LX
        DO 62 IM=1,NMU
        CALL RDTAB1(LEF,C1,AI2,L1,L2,NRP,NEP2,NBT(NO),INR(NO)
     1             ,RWO(LXE),RWO(LXX),KX,IER)
   62   CONTINUE
   64   CONTINUE
        GO TO 61
      END IF
      IF(NR.NE.1 .OR. INR(1).GT.2) IER=21
      YL=FINTXS(EIN,RWO,RWO(LX),NP,INR,IER)
C* Read incident energy definitions
      CALL RDTAB2(LEF,C1,C2,L1,L2,NR,NE,NBT,INR,IER)
      NM=NR+1
      NE1=0
      EI1=0
      DO 74 IE=1,NE
C* Read incident cosine definitions
      CALL RDTAB2(LEF,C1,EI2,L1,L2,NRM,NMU,NBT(NM),INR(NM),IER)
      IF(IER.NE.0) STOP 'DENXS1 ERROR reading distributions'
C...  IF(NRM.GT.1)
C... 1 PRINT *,' WARNING - Multiple E interp.ranges for MF 6, MT',MT
      NO=NM+NMU
      KX =MRW-LXX
      NEP1=0
      AI1 =-1
      DO 72 IM=1,NMU
C* For each cosine and energy read the distributions
      CALL RDTAB1(LEF,C1,AI2,L1,L2,NRP,NEP2,NBT(NO),INR(NO)
     1           ,RWO(LXE),RWO(LXX),KX,IER)
      IF(IER.NE.0) STOP 'DENXS1 ERROR reading distributions'
C...  IF(NRP.GT.1)
C... 1 PRINT *,' WARNING - Multiple A interp.ranges for MF 6, MT',MT
C* Integrate over energy for this cosine
      AMU(IM)=AI2
      EA=RWO(LXE)
      EB=RWO(LXE-1+NEP2)
      CALL YTGEOU(PMU(IM),EA,EB,NEP2,RWO(LXE),RWO(LXX),INR(NO))
      IF(KEA.NE.2) THEN
C* Case: Assemble the angular distribution at the required energy
        RWO(     IM)=AI2
        IF(EOU.GT.0) THEN
C*       Case: Specified outgoing particle energy
          FF=0
          IF(EOU.LE.RWO(LXE-1+NEP2))
     1    FF=FINTXS(EOU,RWO(LXE),RWO(LXX),NEP2,INR(NO),IER)
          RWO(LX-1+IM)=FF
        ELSE
C*       Case: Average outgoing particle energy
          RWO(LX-1+IM)=PMU(IM)
        END IF
        NEP1=NMU
      ELSE
        IF(AIN.LT.-1) THEN
C* Case: Integrate between distributions for two cosines
          CALL FYTG2D(    NEP1,RWO     ,RWO(LX)
     1               ,AI1,NEP3,RWO(LXF),RWO(LXY),INR(NM)
     1               ,AI2,NEP2,RWO(LXE),RWO(LXX),INR(NO),KX2)
        ELSE
C* Case: Interpolate between distributions for two cosines
          CALL FINT2D(ALB,AI1,NEP1,RWO     ,RWO(LX) ,INR(NM)
     1                   ,AI2,NEP2,RWO(LXE),RWO(LXX),INR(NO),KX)
        END IF
      END IF
   72 CONTINUE
C* Integrate over all cosines to check normalisation
C... Should calculate the proper lower bound for the cosine !!!
      A1=AMU(1)
      A2=AMU(NMU)
      INM=INR(NM)
      INM=INM-10*(INM/10)
      CALL YTGEOU(SS,A1,A2,NMU,AMU,PMU,INM)
      IF(AIN.LT.-1) SS=SS*(A2-A1)
      DO 73 I=1,NEP1
      RWO(LX-1+I)=RWO(LX-1+I)/SS
   73 CONTINUE
C...  IF(ABS(SS-1).GT.0.01) PRINT *,'MAT,MT,Ei,SS',ZA0,MT0,EI2,SS
C*
C* Lin-interpolate between distributions for two incident energies
      INE=2
      MX=MRW-LX
      CALL FINT2D(EIN,EI1,NE1 ,ENR,DXS    ,INR
     1               ,EI2,NEP1,RWO,RWO(LX),INE,MX)
C*
   74 CONTINUE
C*
C* Distribution assembled - check that the last point is zero
   80 NEN=NE1
      IF(KEA.EQ.2 .AND. DXS(NEN).NE.0) THEN
        NEN=NEN+1
        ENR(NEN)=ENR(NEN-1)
        DXS(NEN)=0
      END IF
C* Scale the distribution by the cross section
      SS=YL*XS/(2*PI)
      IF(KEA.EQ.2 .AND. PAR.LT.-1) SS=4*PI*SS
      DO 84 I=1,NEN
      DXS(I)=SS*DXS(I)
   84 CONTINUE
C*
   90 RETURN
C*
  902 FORMAT(2F11.0,4I11)
      END
      SUBROUTINE FINDMT(LEF,ZA0,ZA,MAT,MF,MT,C66,IER)
C-Title  : Subroutine FINDMT
C-Purpose: Find specified reaction in an ENDF file
C-Description:
C-D  The routine finds the specified material, file or section
C-D  in an ENDF file. The material may be characterised by the
C-D  ZA0 number defined as Z*1000+A+LIS0/10 (the decimal point
C-D  allows for isomeric states). Alternately, if ZA0<0, the
C-D  absolute integer value is interpreted as the required MAT
C-D  number. If MF and MT are non-zero, the file is scanned
C-D  until the value on the file matches the input value. The
C-D  first record of the material (or section) is placed in the
C-D  character string C66.
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
        READ (LEF,91,END=82,ERR=83) C66,MAT,MF,MT
        MAT0=-1
        GO TO 21
      END IF
C*
C* Loop to find the specified material
   20 READ (LEF,91,END=82,ERR=83) C66,MAT,MF,MT
      IF(MAT.LT.0) GO TO 81
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
      READ (LEF,91,END=82,ERR=83) C66,MAT,MF,MT
      IF(MAT.LE.0) GO TO 81
      GO TO 22
C* Loop to find the reaction type number
   30 IF(MT0.EQ. 0) GO TO 40
   32 IF(MT0.EQ.MT) GO TO 40
      READ (LEF,91,END=82,ERR=83) C66,MAT,MF,MT
      IF(MF0.GT.0 .AND. MF.GT.MF0) GO TO 20
      IF(MAT.LT.0) GO TO 81
      GO TO 32
C* Normal termination
   40 IF(ZA.EQ.0) READ (C66,92) ZA
      RETURN
C*
C* Error traps
   81 IER=1
      RETURN
   82 IER=2
      RETURN
   83 IER=3
      RETURN
C*
   91 FORMAT(A66,I4,I2,I3,I5)
   92 FORMAT(2F11.0,4I11.0,I4,I2,I3,I5)
      END
      SUBROUTINE SKIPSC(LEF)
C-Title  : Subroutine SKIPSC
C-Purpose: Skip current section in an ENDF file
C-
      CHARACTER*66 C66
   20 READ (LEF,91) C66,MAT,MF,MT
      IF(MT.NE.0) GO TO 20
      RETURN
C*
   91 FORMAT(A66,I4,I2,I3,I5)
      END
      SUBROUTINE RDTAB1(LEF,C1,C2,L1,L2,N1,N2,NBT,INR,EN,XS,NMX,IER)
C-Title  : Subroutine RDTAB1
C-Purpose: Read an ENDF TAB1 record
      DIMENSION    NBT(100),INR(100)
      DIMENSION    EN(NMX), XS(NMX)
C*
      READ (LEF,902) C1,C2,L1,L2,N1,N2
      READ (LEF,903) (NBT(J),INR(J),J=1,N1)
      JP=N2
      IF(N2.GT.NMX) THEN
        JP=NMX
        IER=22
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
      SUBROUTINE RDLIST(LEF,C1,C2,L1,L2,N1,N2,VK,IER)
C-Title  : Subroutine RDLIST
C-Purpose: Read an ENDF LIST record
      DIMENSION    VK(1)
C*
      READ (LEF,902) C1,C2,L1,L2,N1,N2
      IF(N1.GT.0) READ (LEF,903) (VK(J),J=1,N1)
      RETURN
C*
  902 FORMAT(2F11.0,4I11)
  903 FORMAT(6F11.0)
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
      DO 20 I=2,NP
      I2=I
      IF(EN(I).GE.EIN) GO TO 22
   20 CONTINUE
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
      SUBROUTINE FINT2D(AI, AI1,NEP1,EN1,XS1 ,INR
     1                    , AI2,NEP2,EN2,XS2 ,INE, MEP2)
C-Title  : Subroutine FINT2D
C-Purpose: Interpolate tabulated distribution
C-Description:
C-D  A pair of tabulated distribution with arguments in EN1 and EN2,
C-D  function values XS1 and XS2 defined over NEP1 and NEP2 points
C-D  and given at the independent arguments AI1 and AI2, respectively
C-D  are interpolated. Interpolation law for the distribution is
C-D  INE=1 (histogram) or INE=2 (linear) are allowed. Distributions
C-D  are first linearised (if necessary) and defined on a union
C-D  grid. Then they are interpolated to the independent argument
C-D  value AI. Allowed interpolation laws are INR=2 (linear),
C-D  INR=12 (corresponding point) of INR=22 (unit base).
C-D  Special features:
C-D  - The interpolated distribution is normalised such that its
C-D    integral is equal to the interpolated integrals of the 
C-D    distributions at AI1 and AI2. This makes a difference
C-D    when interpolating just above threshold where the distri-
C-D    bution is essentially a delta function.
C-D  - Unit base interpolation allows for the offset of the first
C-D    point (i.e. interval E1 - En is translated to 0 - 1).
C-D
C-D  On exit the interpolated function is placed into EN1, XS1 and
C-D  the number of points NEP1 is redefined accordingly.
C-D
C-D  WARNING:
C-D  The size of EN1, XS1 and EN2, XS2 arreys are assumed to be
C-D  at least 2*NEP1 and 2*NEP2, respectively. Array elements above
C-D  NEP1 and NEP2 are used as scratch area.
C-D
      DIMENSION  EN1(NEP1),XS1(NEP1), EN2(MEP2),XS2(MEP2)
C*
      IF(INE.GT.2) STOP 'Log interpolation in MF 6'
C* Convert second set of points to linearly interpolable form
C*
C* Case: Previous point already above required - no action
      IF(AI1.GE.AI .AND. NEP1.GT.0) GO TO 40
C*
C* Begin processing the distribution at the current point
      IF(INE.EQ.1) THEN
C* Convert histogram interpolation to linear
        J=2*NEP2-1
        IF(J.GT.MEP2) THEN
C...      PRINT *,'J,MEP2',J,MEP2
          STOP 'FINT2D ERROR - MEP2 limit exceeded'
        END IF
        DO 20 I=2,NEP2
        J=J-1
        EN2(J)=EN2(NEP2+2-I)
        XS2(J)=XS2(NEP2+1-I)
        J=J-1
        EN2(J)=EN2(NEP2+1-I)
        XS2(J)=XS2(NEP2+1-I)
   20   CONTINUE
        NEP2=2*NEP2-2
      END IF
C* Check if first set or point below the required argument
      IF(NEP1.GT.0  .AND. AI2.GT.AI) GO TO 22
C*
C* Case: Move table to the first field
      DO 21 I=1,NEP2
      EN1(I)=EN2(I)
      XS1(I)=XS2(I)
   21 CONTINUE
      AI1=AI2
      NEP1=NEP2
      GO TO 40
C*
C* Case: Interpolation required - check interpolation law and integrals
   22 INN=2
      CALL YTGEOU(EI1,EN1(1),EN1(NEP1),NEP1,EN1,XS1,INN)
      CALL YTGEOU(EI2,EN2(1),EN2(NEP2),NEP2,EN2,XS2,INN)
      EIN=EI1+(EI2-EI1)*(AI-AI1)/(AI2-AI1)
      LU1=1
      LU2=1
   24 IN2=INR/10
      IF     (IN2.EQ.1) THEN
C* Corresponding point interpolation
        IF(NEP1.NE.NEP2) THEN
          INR=INR+10
          GO TO 24
        END IF
        DO 26 I=1,NEP1
        EN1(I)=EN1(I)+(EN2(I)-EN1(I))*(AI-AI1)/(AI2-AI1)
   26   CONTINUE
        LUNI=1
        NEU=NEP1
        GO TO 33
      ELSE IF(IN2.EQ.2) THEN
C* Unit base interpolation
        LU1=NEP1+1
C...    ED1=0
        ED1=EN1(1)
        EU1=EN1(NEP1)-ED1
        DO 27 I=1,NEP1
        EN1(NEP1+I)=(EN1(I)-ED1)/EU1
   27   CONTINUE
        LU2=NEP2+1
C...    ED2=0
        ED2=EN2(1)
        EU2=EN2(NEP2)-ED2
        DO 28 I=1,NEP2
        EN2(NEP2+I)=(EN2(I)-ED2)/EU2
   28   CONTINUE
        EDN=ED1+(ED2-ED1)*(AI-AI1)/(AI2-AI1)
        EUN=EU1+(EU2-EU1)*(AI-AI1)/(AI2-AI1)
      END IF
C* Make a union grid
   30 LUNI=LU2+NEP2
      KX=MEP2-LUNI
      CALL UNIGRD(NEP1,EN1(LU1),NEP2,EN2(LU2),NEU,EN2(LUNI),KX)
C* Interpolate first distribution to the union grid
      CALL FITGRD(NEP1,EN1(LU1),XS1,NEU,EN2(LUNI),XS2(LUNI))
      DO 32 I=1,NEU
      EN1(I)=EN2(LUNI-1+I)
      XS1(I)=XS2(LUNI-1+I)
   32 CONTINUE
C* Interpolate second distribution to the union grid
      CALL FITGRD(NEP2,EN2(LU2),XS2,NEU,EN2(LUNI),XS2(LUNI))
C* Interpolate distributions to the required argument
   33 DO 34 I=1,NEU
      X1=XS1(I)
      XX=XS2(LUNI-1+I)
      IF(AI1.NE.AI2) XX=X1+(XX-X1)*(AI-AI1)/(AI2-AI1)
      XS1(I)=XX
   34 CONTINUE
      AI1=AI
      NEP1=NEU
C* Convert back unit base to energy if unit-base interpolation
      IF(IN2.EQ.2) THEN
        DO 36 I=1,NEP1
        EN1(I)=EDN+EN1(I)*EUN
   36   CONTINUE
      END IF
C* Normalise to the interpolated integral
      INN=2
      CALL YTGEOU(EIO,EN1(1),EN1(NEP1),NEP1,EN1,XS1,INN)
      IF(EIO.LE.0) GO TO 40
      SS=EIN/EIO
      DO 38 I=1,NEP1
      XS1(I)=XS1(I)*SS
   38 CONTINUE
C*
   40 CONTINUE
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
      DIMENSION EN1(NEP1),EN2(NEP2),EUN(NEP2)
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
C...    PRINT *,'UNIGRD ERROR in grid-1:',NEU,KX
        STOP 'UNIGRD ERROR - KX limit exceeded'
      END IF
      EUN(NEU)=EN1(J1)
C* Test for coincident points in the second grid
      IF(EN1(J1).EQ.EN2(J2) .AND. J2.LE.NEP2) J2=J2+1
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
C...     PRINT *,'NEU,KX',NEU,KX
         STOP 'UNIGRD ERROR - KX limit exceeded'
      END IF
      EUN(NEU)=EN2(J2)
C* Test for coincident points in the first grid
      IF(EN1(J1).EQ.EN2(J2) .AND. J1.LE.NEP1) J1=J1+1
      J2 =J2+1
C* Test for the end of the grid set
      IF(J1.GT.NEP1 .AND. J2.GT.NEP2) GO TO 40
      IF(J2.GT.NEP2) GO TO 10
C* Test for double points in a grid
      IF(EN2(J2).EQ.EUN(NEU)) GO TO 20
      IF(J1.GT.NEP1) GO TO 20
      GO TO 8
C*
C* All points processed
   40 CONTINUE
      RETURN
      END
      SUBROUTINE FITGRD(NEP1,EN1,XS1,NEP2,EN2,XS2)
C-Title  : Subroutine FITGRD
C-Purpose: Interpolate a tabulated function to a given grid
      DIMENSION EN1(NEP1),XS1(NEP1),EN2(NEP2),XS2(NEP2)
C*
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
      SUBROUTINE FNGAUS(ELV,NEN,ENR,DXS,EPS)
C-Title  : Subroutine FGAUS
C-Purpose: Tabulate normalised Gaussian function at NEN points
      DIMENSION ENR(NEN),DXS(NEN)
C*
C* Define constant A such that Gauss(A*(E-ELV))=1/2 at E=ELV*(1+EPS)
      AA =0.83/(EPS*ELV)
C* Define range of tabulation +/- 5*EPS*ELV
      DE =ELV*EPS*5
      EB =ELV+DE
      EA =ELV-DE
C* Normalisation constant is the integral
      SS=0
      DE =(EB-EA)/(NEN-1)
      DO 20 I=1,NEN
      EE =EA+(I-1)*DE
      XX =AA*(EE-ELV)
      FF =EXP(-XX*XX)
      ENR(I)=EE
      DXS(I)=FF
      IF(I.GT.1) SS=SS+0.5*(FF+F0)*(EE-E0)
      F0=FF
      E0=EE
   20 CONTINUE
C* Normalise
      DO 22 I=1,NEN
      DXS(I)=DXS(I)/SS
   22 CONTINUE
C*
      RETURN
      END
      SUBROUTINE YTGEOU(PMU,EA,EB,NEP,EOU,DXS,INR)
C-Title  : Subroutine YTGEOU
C-Purpose: Integrate a pointwise tabulated function over EA - EB
      DIMENSION  EOU(NEP),DXS(NEP)
C*
      PMU=0
      E2=EOU(1)
      F2=DXS(1)
      DO 20 I=2,NEP
      E1=E2
      F1=F2
      E2=EOU(I)
      F2=DXS(I)
C* Treat lower energy bound
      IF(E2.LT.EA) GO TO 20
      IF(E1.LT.EA) THEN
        IF(INR.EQ.2) F1=F1+(F2-F1)*(EA-E1)/(E2-E1)
        E1=EA
      END IF
C* Treat upper energy bound
      IF(E1.GT.EB) GO TO 20
      IF(E2.GT.EB) THEN
        IF(INR.EQ.2) F1=F1+(F2-F1)*(EB-E1)/(E2-E1)
        E2=EB
      END IF
C* Define average function value over the interval
      IF(INR.EQ.1) THEN
        FF=F1
      ELSE
        FF=0.5*(F2+F1)
      END IF
C* Add interval contribution to the integral
      PMU=PMU+FF*(E2-E1)
   20 CONTINUE
C*
      RETURN
      END
      SUBROUTINE FYTG2D(NEP0,EN0,XS0
     1            , AI1,NEP1,EN1,XS1 ,INR
     1            , AI2,NEP2,EN2,XS2 ,INE, MEP2)
C-Title  : Subroutine FYTG2D
C-Purpose: Integrate tabulated distribution
      DIMENSION  EN0(NEP0),XS0(NEP0),
     1           EN1(NEP1),XS1(NEP1),
     2           EN2(MEP2),XS2(MEP2)
C*
      IF(INE.GT.2) STOP 'Log interpolation in MF 6'
C* Convert second set of points to linearly interpolable form
      IF(INE.EQ.1) THEN
C* Convert histogram interpolation to linear
        J=2*NEP2-1
        IF(J.GT.MEP2) THEN
C...      PRINT *,'J,MEP2',J,MEP2
          STOP 'FYTG2D ERROR - MEP2 limit exceeded'
        END IF
        DO 20 I=2,NEP2
        J=J-1
        EN2(J)=EN2(NEP2+2-I)
        XS2(J)=XS2(NEP2+1-I)
        J=J-1
        EN2(J)=EN2(NEP2+1-I)
        XS2(J)=XS2(NEP2+1-I)
   20   CONTINUE
        NEP2=2*NEP2-2
      END IF
      IF(NEP0.LE.0) THEN
C*
C* First set of points - initialise integral
        DO 22 I=1,NEP2
        EN0(I)=EN2(I)
        EN1(I)=EN2(I)
        XS0(I)=0
        XS1(I)=XS2(I)
   22   CONTINUE
        NEP0=NEP2
        NEP1=NEP2
        AI1=AI2
        GO TO 40
      END IF
C*
C* Make a union grid
      LU1=1
      LU2=1
   30 LUNI=LU2+NEP2
      KX=MEP2-LUNI
      CALL UNIGRD(NEP1,EN1(LU1),NEP2,EN2(LU2),NEU,EN2(LUNI),KX)
C* Interpolate integrated distribution to the union grid
      CALL FITGRD(NEP0,EN0(LU1),XS0,NEU,EN2(LUNI),XS2(LUNI))
      DO 31 I=1,NEU
      EN0(I)=EN2(LUNI-1+I)
      XS0(I)=XS2(LUNI-1+I)
   31 CONTINUE
C* Interpolate first distribution to the union grid
      CALL FITGRD(NEP1,EN1(LU1),XS1,NEU,EN2(LUNI),XS2(LUNI))
      DO 32 I=1,NEU
      EN1(I)=EN2(LUNI-1+I)
      XS1(I)=XS2(LUNI-1+I)
   32 CONTINUE
C* Interpolate second distribution to the union grid
      CALL FITGRD(NEP2,EN2(LU2),XS2,NEU,EN2(LUNI),XS2(LUNI))
C* Add the contribution to the integral
      DO 34 I=1,NEU
      IF(INR.EQ.1) THEN
        FF=XS2(I)*(AI2-AI1)
      ELSE
        FF=0.5*(XS1(I)+XS2(I))*(AI2-AI1)
      END IF
      XS0(I)=XS0(I)+FF
   34 CONTINUE
      AI1=AI2
      NEP1=NEU
      NEP0=NEU
C* Integration completed
   40 RETURN
      END

