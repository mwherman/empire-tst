      SUBROUTINE DXSEND(LEF,ZA0,ZAP,MF0,MT0,KEA,EIN,PAR,EPS,ENR,DXS
     1                 ,RWO,NEN,MEN,MRW,LTT,ELV)
C-Title  : Subroutine DXSEND
C-Purpose: Extract double differential cross sections from ENDF
C-Author : A.Trkov, International Atomic Energy Agency, Vienna, Austria.
C-Version: 15-Dec-2000 (Original code).
C-V  01/03 - Correct trapping of unknown MF4 data representation.
C-V        - Add MT5 to sumation list for input MT0=5.
C-V  02/01 - Allow processing of MF23,26 & other outgoing particles.
C-V  02/05 - Make MT103,107 redundant when MT600,800 series present.
C-V        - Add parameter ELV to match angular distributions of
C-V          discrete inelastic levels by level energy rather than
C-V          MT number.
C-V        - Fix spectra of charged particles.
C-V        - Add photon spectra.
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
C* Limits: Max.No.of reactions=MXL, interp.ranges=MXI
      PARAMETER   (MXL=200,MXI=100)
C*
      DIMENSION    RWO(MRW),ENR(MEN),DXS(MEN)
     1            ,LST(MXL)
      DIMENSION    NBT(MXI),INR(MXI)
C*
      LOOP=0
      LX  =1
      NE1 =0
      NEN =0
      KRW =MRW
      MF  =MF0
      MT  =MT0
      I600=0
      I800=0
C*
C* Find the appropriate discrete level for inelastic angular distrib.
      IF(ELV.GT.0 .AND.
     1  (MF0.EQ.4 .AND. (MT0.GE.51 .AND. MT0.LT.91) ) ) THEN
C* Find the matching energy level for discrete inelastic ang.distr.
        REWIND LEF
        CALL SKIPSC(LEF)
        MF=3
        DE=ELV
   12   MT=0
        CALL FINDMT(LEF,ZA0,ZA,AW,L1,L2,N1,N2,MAT,MF,MT,IER)
        IF(MT.GT.90 .OR. IER.NE.0) THEN
C* No more discrete levels - use last
          MT=MT0
          GO TO 60
        END IF
        IF(MT.LE.50) THEN
C* Try next reaction
          CALL SKIPSC(LEF)
          GO TO 12
        END IF
C* Inelastic level found, check energy level
        CALL RDTAB2(LEF,QM,QI,L1,L2,NR,NP,NBT,INR,IER)
        CALL SKIPSC(LEF)
        EE=-QI
        IF(ABS(EE-ELV).LT.DE) THEN
          MT0=MT
          DE =ABS(EE-ELV)
        END IF
        IF(EE.LT.ELV) GO TO 12
        MT=MT0
      END IF
C*
C* Check if reaction summation is required
      IF(MT0.NE.5) GO TO 60
C* Find particle emission reactions
      REWIND LEF
      CALL SKIPSC(LEF)
      IZAP=ZAP+0.1
      IF(IZAP.EQ.1) THEN
C*
C* Neutron emission: prepare reaction list
   20   MF=3
        MT=0
        CALL FINDMT(LEF,ZA0,ZA,AW,L1,L2,N1,N2,MAT,MF,MT,IER)
        IF(IER.NE.0) GO TO 50
        CALL SKIPSC(LEF)
C* Select contributing reactions
        IF(MT.EQ. 5) GO TO 22
        IF(MT.EQ.11) GO TO 22
        IF(MT.GE.16 .AND. MT.LE.17) GO TO 22
        IF(MT.GE.16 .AND. MT.LE.17) GO TO 22
        IF(MT.EQ.18) GO TO 22
        IF(MT.GE.28 .AND. MT.LE.30) GO TO 22
        IF(MT.GE.32 .AND. MT.LE.37) GO TO 22
        IF(MT.GE.41 .AND. MT.LE.45) GO TO 22
        IF(MT.EQ.91) GO TO 22
C* Process elastic and discrete inelastic only if requesting
C* energy distributions or aver. energy ang.distrib.
        IF( ( KEA.EQ.2 .OR.
     &       (KEA.EQ.1 .AND. PAR.LT.0) ) .AND.
     &      (  MT.EQ.2 .OR.
     &       ( MT.GE.50.AND. MT.LE.90) ) ) GO TO 22
        GO TO 20
   22   LOOP=LOOP+1
        IF(LOOP.GT.MXL) STOP 'DXSEND ERROR - MXL Limit exceeded'
        LST(LOOP)=MT
        GO TO 20
C*
      ELSE IF(IZAP.EQ.1001) THEN
C*
C* Proton emission: prepare reaction list
   24   MF=3
        MT=0
        CALL FINDMT(LEF,ZA0,ZA,AW,L1,L2,N1,N2,MAT,MF,MT,IER)
        IF(IER.NE.0) GO TO 50
        CALL SKIPSC(LEF)
C* Select contributing reactions
        IF( MT.EQ.  5                  .OR.
     &      MT.EQ. 28                  .OR.
     &     (MT.GE. 41 .AND. MT.LE. 42) .OR.
     &     (MT.EQ. 44 .AND. MT.LE. 45) .OR.
     &      MT.EQ.103                  .OR.
     &     (MT.GE.111 .AND. MT.LE.112) .OR.
     &     (MT.GE.115 .AND. MT.LE.116) .OR.
     &     (MT.GE.600 .AND. MT.LE.649)) THEN
          LOOP=LOOP+1
          LST(LOOP)=MT
          IF(MT.GE.600 .AND. MT.LE.649) I600=1
        END IF
        GO TO 24
      ELSE IF(IZAP.EQ.2004) THEN
C*
C* Alpha emission: prepare reaction list
   34   MF=3
        MT=0
        CALL FINDMT(LEF,ZA0,ZA,AW,L1,L2,N1,N2,MAT,MF,MT,IER)
        IF(IER.NE.0) GO TO 50
        CALL SKIPSC(LEF)
C* Select contributing reactions
        IF( MT.EQ.  5                  .OR.
     &     (MT.GE. 22 .AND. MT.LE. 25) .OR.
     &     (MT.GE. 29 .AND. MT.LE. 30) .OR.
     &     (MT.GE. 35 .AND. MT.LE. 36) .OR.
     &      MT.EQ. 45                  .OR.
     &     (MT.GE.107 .AND. MT.LE.109) .OR.
     &     (MT.GE.112 .AND. MT.LE.114) .OR.
     &      MT.EQ.117                  .OR.
     &     (MT.GE.800 .AND. MT.LE.849)) THEN
          LOOP=LOOP+1
          LST(LOOP)=MT
          IF(MT.GE.800 .AND. MT.LE.849) I800=1
        END IF
        GO TO 34
      ELSE IF(IZAP.EQ.0) THEN
C*
C* Gamma emission: suppress angular distributions
        IF(KEA.EQ.1) THEN
          IF(LTT.GT.0) THEN
          WRITE(LTT,903) ' DXSEND WARNING - No Angular distrib.   '
          WRITE(LTT,903) '                       No coding for ZAP',IZAP
          NEN=0
          GO TO 90
          END IF
        END IF
C* Prepare reaction list
   44   MF=3
        MT=0
        CALL FINDMT(LEF,ZA0,ZA,AW,L1,L2,N1,N2,MAT,MF,MT,IER)
        IF(IER.NE.0) GO TO 50
        CALL SKIPSC(LEF)
C* Select contributiong reactions
        IF( MT.EQ.  5                  .OR.
     &     (MT.GE. 11 .AND. MT.LE.199) .OR.
     &     (MT.GT.600 .AND. MT.LE.649) .OR.
     &     (MT.GT.800 .AND. MT.LE.849)) THEN
          LOOP=LOOP+1
          LST(LOOP)=MT
          IF(MT.GE.600 .AND. MT.LE.649) I600=1
          IF(MT.GE.800 .AND. MT.LE.849) I800=1
        END IF
        GO TO 44
      ELSE
C* Not coded for other particles
C...    STOP 'DXSEND ERROR - No coding for requested particle'
        PRINT *,'DXSEND WARNING - Not coded for eject.ZAP',IZAP
        GO TO 90
      END IF
C* All reactions found - begin processing
   50 IF(LOOP.LT.1) THEN
        WRITE(LTT,903) ' DXSEND WARNING - No data on file for MT',MT0
        GO TO 90
      END IF
C* Exclude cumulative reactions if discrete levels present
      IL=0
      DO 52 I=1,LOOP
      IL=IL+1
      LST(IL)=LST(I)
      IF(LST(I).EQ.103 .AND. I600.EQ.1) IL=IL-1
      IF(LST(I).EQ.107 .AND. I800.EQ.1) IL=IL-1
   52 CONTINUE
      LOOP=IL
      IL=1
      ML=0
      MT=LST(IL)
C...
c...      print *,(lst(j),j=1,loop)
C*
C* Retrieve the double differential cross section energy distribution
   60 CALL DXSEN1(LEF,ZA0,ZAP,MF0,MT,KEA,EIN,PAR,EPS,ENR,DXS
     1           ,RWO(LX),NE,MEN,KRW,IER)
      IF( IER.NE.0) THEN
        IF(LTT.GT.0) THEN
        WRITE(LTT,903) ' DXSEND WARNING - Error condition flag  ',IER
        WRITE(LTT,903) '            Failed reconstruction for MT',MT
        END IF
        ML=ML+1
        GO TO 74
      END IF
      NEN=NE
      IF(LOOP.EQ.0) GO TO 90
      IF( NE1.EQ.0) GO TO 70
      IF( NEN.EQ.0) THEN
        NEN=NE1
        GO TO 74
      END IF
C* Move the previously saved distribution in the work field
      LX=MRW/2
      DO 62 I=1,NE1
      RWO(LX-1+I)=RWO(NE1+I)
   62 CONTINUE
C* Generate the union grid
      LUE= 1+NE1
      LUX=LX+NE1
      KX =MRW-LUX
      CALL UNIGRD(NEN,ENR,NE1,RWO,NE2,RWO(LUE),KX)
C* Interpolate current distribution to the union grid
      CALL FITGRD(NEN,ENR,DXS,NE2,RWO(LUE),RWO(LUX))
      NEN=NE2
      DO 64 I=1,NEN
      ENR(I)=RWO(LUE-1+I)
      DXS(I)=RWO(LUX-1+I)
   64 CONTINUE
C* Interpolate saved distribution to the union grid
      CALL FITGRD(NE1,RWO,RWO(LX),NE2,RWO(LUE),RWO(LUX))
C* Add the current to the saved distribution
      DO 66 I=1,NEN
      DXS(I)=DXS(I)+RWO(LUX-1+I)
   66 CONTINUE
C* Save the summed distribution 
   70 LX=1+NEN*2
      KRW=MRW-LX
      NE1=NEN
      DO 72 I=1,NEN
      RWO(I    )=ENR(I)
      RWO(I+NEN)=DXS(I)
   72 CONTINUE
C* Select next reaction
   74 IF(IL.GE.LOOP) GO TO 90
      IL=IL+1
      MT=LST(IL)
      GO TO 60
C*
C* All processing completed
   90 CONTINUE
      IF(ML.GT.0 .AND. LOOP.GT.0) WRITE(LTT,904) ML,LOOP
      RETURN
C*
  903 FORMAT(A40,I4)
  904 FORMAT(' DXSEND WARNING - failed reconstructing'
     1       ,I3,' out of',I3,' reactions')
      END
      SUBROUTINE DXSEN1(LEF,ZA0,ZAP0,MF0,MT0,KEA,EIN,PAR,EPS,ENR,DXS
     1                 ,RWO,NEN,MEN,MRW,IER)
C-Title  : Subroutine DXSEN1
C-Purpose: Extract cross sect. and differential cross sect. from ENDF
C-Author : A.Trkov, International Atomic Energy Agency, Vienna, Austria.
C-Version: 18-May-2001 (Original code).
C-V  01/07 - Fix neutron multiplicity for MF 4/5
C-V  02/01 - Allow processing of MF23,26, other out.particles.
C-V          Note the redefinition of PAR to degrees instead
C-V          of cosine when requesting double-differential
C-V          energy spectra (KEA=2).
C-V  02/05 - Fix processing of MT600,600 series angular distributions.
C-V        - Provisionally add photon spectra (assumed isotropic).
C-V  02/10 - Add photon spectra from (n,gamma)
C-V          WARNING: Photon emission assumed isotropic.
C-Description:
C-D  The routine reads an ENDF file and extract cross sections (KEA=0),
C-D  differential cross section (angular distributions KEA=1 or energy
C-D  spectra KEA=2, parameter PAR < 0) and double differential cross
C-D  sections (correlated energy/angle distributions with the same
C-D  conventions for KEA. Parameter PAR is the requested outgoing
C-D  particle energy when the correlated angular distribution are
C-D  requested. Similarly, PAR is the scattering angle (degrees)
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
C-D  MF0  - Requested file number (ENDF conventions).
C-D  MT0  - Requested reaction number. Broadly this follows the ENDF
C-D         conventions.
C-D  KEA  - Control flag to select retrieval:
C-D           0  cross sections,
C-D           1  angular distributions,
C-D           2  energy spectra.
C-D  EIN  - Incident particle energy (eV).
C-D  PAR  - Fixed parameter when requesting differential data:
C-D         KEA=0, MF0=10, PAR is the requested metastable state
C-D                (0=ground, 1=first metastable, etc.).
C-D         KEA=1, PAR is the requested outgoing particle energy.
C-D         KEA=2, PAR is the requested scattering angle (degrees).
C-D                A value PAR < 0 implies angle integrated energy
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
C-D         completed successfully. Currently defined error flags
C-D         include the following:
C-D          1  Specified material not found.
C-D          2  End-of-file before material found.
C-D          3  General read error.
C-D         21  Multiple interpolation ranges and/or law other
C-M             than lin-lin encountered.
C-D         22  Array limit MRW exceeded.
C-D         23  Correlated energy-angle distributions not in Law-7
C-D             representation.
C-D         24  Requested particle not found.
C-D         31  Processing not coded for specified reaction.
C-D
C-Extern.: SKIPSC,FINDMT,RDTAB1,RDTAB2,RDLIST,FINT2D,YTGEOU,FNGAUS,
C-E        FYTG2D,UNIGRD,FITGRD
C-
      PARAMETER   (MIW=100)
      DIMENSION    IWO(MIW)
      DIMENSION    RWO(MRW),ENR(MEN),DXS(MEN)
      DIMENSION    NBT(100),INR(100)
      DIMENSION    AMU(100),PMU(100)
C*
      DATA PI/3.14159265/
C...
C...      PRINT *,'ZA0,ZAP0,MF0,MT0,KEA,EIN,PAR'
C...     1        ,nint(ZA0),nint(ZAP0),MF0,MT0,KEA,EIN,PAR
C...
C*
C* Check the requested type of output
      MF = MF0
      NEN= 0
      DEG=-2
      AIN=-2
      EOU=-2
      SAN= 1/(2*PI)
      MST =1+NINT(PAR)
      IF     (KEA.EQ.2) THEN
C* Case: Energy spectrum at fixed scattering angle requested
        MST=1
        DEG=PAR
        IF(DEG.GE.0) THEN
          AIN=COS(DEG*PI/180)
        ELSE
          SAN= 1
        END IF
      ELSE IF(KEA.EQ.1) THEN
C* Case: Angular distribution at fixed outgoing particle energy requested
        MST=1
        EOU=PAR
      END IF
C*
      REWIND LEF
      CALL SKIPSC(LEF)
C*
C* Define particle multiplicities
      YL =1
      IF(KEA.EQ.0) GO TO 30
      IZAP0=NINT(ZAP0)
      IF(IZAP0.EQ.1) THEN
C* Neutron emission reactions
C*        (n,2n+x)
        IF(MT0.EQ.11 .OR.
     &     MT0.EQ.16 .OR.
     &     MT0.EQ.21 .OR.
     &     MT0.EQ.24 .OR.
     &     MT0.EQ.30 .OR.
     &     MT0.EQ.41) YL=2
C*        (n,3n+x)
        IF(MT0.EQ.17 .OR.
     &     MT0.EQ.25 .OR.
     &     MT0.EQ.38 .OR.
     &     MT0.EQ.42) YL=3
C*        (n,fission)
C*        Ignore second chance fission (use total fission only)
        IF(MT0.EQ.19) GO TO 900
        IF(MT0.EQ.18) THEN
          MF =1
          MT =452
          CALL FINDMT(LEF,ZA0,ZA,AWR,L1,LNU,N1,N2,MAT,MF,MT,IER)
          IF(IER.NE.0) GO TO 900
          IF     (LNU.EQ.1) THEN
            CALL RDLIST(LEF,C1,C2,L1,L2,NC,N2,RWO,MRW,IER)
            NN=NC-1
            YL=POLYNX(EIN,RWO,NN)
          ELSE IF(LNU.EQ.2) THEN
            NX=MRW/2
            LX=NX+1
            CALL RDTAB1(LEF,C1,C2,L1,L2,NR,NP,NBT,INR
     1                 ,RWO,RWO(LX),NX,IER)
            IF(IER.NE.0) GO TO 900
            YL=FINTXS(EIN,RWO,RWO(LX),NP,INR(1),IER)
          ELSE
            PRINT *,'DXSEN1 ERROR - Invalid LNU=',LNU,' for NuBar'
            STOP    'DXSEN1 ERROR - Invalid LNU for NuBar'
          END IF
        END IF
      ELSE IF(IZAP0.EQ.1001) THEN
C* Proton emission reactions
        IF(MT0.EQ. 44 .OR.
     &     MT0.EQ.111       ) YL=2
      ELSE IF(IZAP0.EQ.2004) THEN
C* Alpha emission reactions
        IF(MT0.EQ. 29 .OR.
     &     MT0.EQ. 30 .OR.
     &     MT0.EQ. 35 .OR.
     &     MT0.EQ. 36 .OR.
     &     MT0.EQ.108 .OR.
     &     MT0.EQ.113 .OR.
     &     MT0.EQ.114       ) YL=2
      END IF
C*
C* Retrieve the cross section on MF3
   30 IF     (MF0.GE.3 .AND. MF0.LE.6 ) THEN
        MF=3
      ELSE IF(MF0.EQ.10)                THEN
        MF=10
      ELSE IF(MF0.EQ.23 .OR. MF0.EQ.26) THEN
        MF=23
      ELSE
        PRINT *,'DXSEN1 ERROR - Illegal MF output request',MF0
        STOP 'DXSEN1 ERROR - Illegal MF output request'
      END IF
      MT =MT0
      CALL FINDMT(LEF,ZA0,ZA,AWR,L1,L2,NS,N2,MAT,MF,MT,IER)
      IF(IER.NE.0) GO TO 900
      NX=MRW/2
      LX=NX+1
C* Read TAB1 cross sections (can be more than 1 if MF 10)
      DO 31 J=1,MST
      CALL RDTAB1(LEF,QM,QI,L1,L2,NR,NP,NBT,INR
     1           ,RWO,RWO(LX),NX,IER)
      IF(IER.NE.0) GO TO 900
   31 CONTINUE
      IF(EIN.GT.0) THEN
        IF(EIN.LT.RWO(1) .OR. EIN.GT.RWO(NP)) THEN
C* Case: Required point is below thershold or above last point
          NEN=0
          GO TO 900
        END IF
      END IF
      IF(NR.NE.1 .OR. INR(1).GT.2) THEN
        IER=21
        GO TO 900
      END IF
C* Case: Cross section is required on output - finish processing
      IF(KEA.EQ.0) THEN
        NEN=NP
        DO 32 I=1,NEN
        ENR(I)=RWO(     I)
        DXS(I)=RWO(LX-1+I)
   32   CONTINUE
        GO TO 900
      END IF
C* Case: Proceed with the retrieval of differential data
      ETOP=RWO(NP)
      XS=FINTXS(EIN,RWO,RWO(LX),NP,INR(1),IER)
      IF(IER.NE.0 .OR. XS .LE.0) THEN
        NEN=0
        GO TO 900
      END IF
C*
C* Find the energy/angle distribution data
   34 MF =0
      MT =MT0
      CALL FINDMT(LEF,ZA0,ZA,AWR,L1,L2,N1,N2,MAT,MF,MT,IER)
C* Error trapping when no data found in the ENDF file
      IF(IER.NE.0) THEN
C* No MF4/5/6 possible only for two-body (isotropic) reactions
        IF     (IZAP0.EQ.   1) THEN
          IF(MT0.EQ.2 .OR. (MT0.GT.50 .AND. MT0.LT.91) ) GO TO 40
        ELSE IF(IZAP0.EQ.1001) THEN
          IF(MT0.GE.600 .AND. MT0.LT.649) GO TO 40
        ELSE IF(IZAP0.EQ.2004) THEN
          IF(MT0.GE.800 .AND. MT0.LT.849) GO TO 40
        END IF
        IER=25
        PRINT *,'WARNING - No differential data for MT',MT0
        GO TO 900
      END IF
C* Gamma production only from MF 6,12,14,15
      IF(IZAP0.EQ.   0) THEN
        IF(MF.EQ.12) REWIND LEF
        IF(MF.EQ. 4 .OR. MF.EQ. 5 .OR. MF.EQ.12) THEN
          MF =12
          IF     (MT0.GT. 50 .AND. MT0.LT. 91) THEN
            MT =51
          ELSE IF(MT0.GT.600 .AND. MT0.LT.649) THEN
            MT =601
          ELSE IF(MT0.GT.650 .AND. MT0.LT.699) THEN
            MT =651
          ELSE IF(MT0.GT.700 .AND. MT0.LT.749) THEN
            MT =701
          ELSE IF(MT0.GT.750 .AND. MT0.LT.799) THEN
            MT =751
          ELSE IF(MT0.GT.800 .AND. MT0.LT.849) THEN
            MT=801
          END IF
          CALL SKIPSC(LEF)
          CALL FINDMT(LEF,ZA0,ZA,AWR,L1,L2,N1,N2,MAT,MF,MT,IER)
          IF(IER.NE.0) GO TO 140
          GO TO 120
        END IF
      END IF
C*
C* Select appropriate file processing
      IF(MF.EQ. 6) GO TO  60
      IF(MF.EQ. 4) GO TO  40
        CALL SKIPSC(LEF)
        GO TO 34
C*
C* Process MF 4 data - Preset isotropic CM angular distributions
   40 LCT=2
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
        PRINT *,'WARNING - CM isotropic ang.distrib.assumed'
        GO TO 45
      END IF
C*
C* Process angular distribution data in MF 4
   42 LVT=L1
      LTT=L2
      IF(LTT.EQ.0) GO TO 45
      IF(LTT.NE.2) THEN
        PRINT *,'WARNING - Can not process MF4 LTT',LTT
        PRINT *,'          Distributions should be linearised'
        IER=41
        GO TO 45
      END IF
C* Secondary particle angular distributions MF4 processing
      IF(LVT.LE.1) THEN
        CALL RDLIST(LEF,C1,C2,LI,LCT,NK,NM,RWO,MRW,IER)
      ELSE
        PRINT *,'WARNING - Illegal value for MF4       LVT=',LVT
        IER=41
        GO TO 45
      END IF
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
C* Lin-interpolate angular distributions over incident energies
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
        GAM=SQRT( EIN/ (EIN*AWR*AWR+QI*AWR*(AWR+1) ) )
        DO 46 I=1,NE1
        XCM=ENR(I)
        XLB=(GAM+XCM)/SQRT(1+GAM*GAM+2*GAM*XCM)
        DMC=2*GAM*XLB + SQRT(1-GAM*GAM*(1-XLB*XLB))
     1     +(GAM*XLB)**2 / SQRT(1-GAM*GAM*(1-XLB*XLB))
        ENR(I)=XLB
        DXS(I)=DXS(I)*DMC
   46   CONTINUE
      END IF
      IF(KEA.EQ.1) THEN
C* Case: Angular distribution
        YL=YL/SS
        IF(EOU.LT.0) GO TO 800
      END IF
      INA=2
      IF(DEG.GE.0) THEN
C* Case: Specified outgoing particle cosine
        YA=FINTXS(AIN,ENR,DXS,NE1,INA,IER)/SS
        XLB=AIN
        YL =YL*YA
      ELSE
C* Case: Angle-integrated energy distribution
        DO 47 I=1,NE1
        DXS(I)=DXS(I)*ENR(I)
   47   CONTINUE
        EA=ENR(1)
        EB=ENR(NE1)
        CALL YTGEOU(XLB,EA,EB,NE1,ENR,DXS,INR)
      END IF
C*
C* Assume two-body scattering for elastic & inelastic scattering
C* and other discrete-lever reactions
      IF(MT0.EQ.2 .OR. (MT0.GT.50 .AND. MT0.LE.99) ) THEN
        AWO=1
        GO TO 48
      ELSE IF(MT0.GE.600 .AND. MT0.LE.649) THEN
        AWO=1
        GO TO 48
      ELSE IF(MT0.GE.800 .AND. MT0.LE.849) THEN
        AWO=4
        GO TO 48
      ELSE
        GO TO 50
      END IF
   48 IER=0
C* Available energy and distribution (gausssian)
C...              Check AWo and Eou definition !!!
      EAV=EIN + QI*(1+AWR)/AWR
      ECM=EAV*(AWR/(1+AWR))**2
      GAM=SQRT( EIN/ (EIN*AWR*AWR+QI*AWR*(AWR+1) ) )
      ACM=-GAM*(1-XLB*XLB) + XLB*SQRT(1-GAM*GAM*(1-XLB*XLB))
      EOU=ECM + ( EIN + 2*ACM*(AWR+AWO)*SQRT(EIN*ECM) )/(AWR+AWO)**2
C...
C...  EOU=EAV*(AWR+1)/(AWR+1-AWO)
C...
      NE1=21
      CALL FNGAUS(EOU,NE1,ENR,DXS,EPS)
      GO TO 800
C*
C* Process energy distribution data in MF 5
   50 IF(MF.EQ.4) THEN
        MF =5
        MT =MT0
        CALL FINDMT(LEF,ZA0,ZA,AW,L1,L2,NK,N2,MAT,MF,MT,IER)
      END IF
      NX=MRW/2
      LX=NX+1
      IF(MF.EQ.5 .AND. NK.GT.1) THEN
C...    No multiple representations are allowed
        PRINT *,'WARNING - No mult.represent.allowed for MF5 MT',MT0
        IER=31
        GO TO 900
      END IF
      JK=0
   51 JK=JK+1
C* Read weights for the distributions (MF5) or multiplicities(MF26)
      CALL RDTAB1(LEF,C1,C2,L1,LF,NR,NP,NBT,INR
     1           ,RWO,RWO(LX),NX,IER)
      IF     (MF.EQ. 5) THEN
C* Interpret Pk(e) - assumed equal 1 for a single representation
      ELSE IF(MF.EQ.26) THEN
C* Interpret particle multiplicity
        ZAP=C1
        IF(NINT(ZAP).NE.NINT(ZAP0)) THEN
C* If not the right particle try next one
          CALL RDTAB2(LEF,C1,C2,L1,L2,NR,NE,NBT,INR,IER)
          MX=MRW-LXE
          DO 52 IE=1,NE
          CALL RDLIST(LEF,C1,C2,L1,L2,N1,N2,RWO(LXE),MX,IER)
   52     CONTINUE
          IF(JK.LT.NK) GO TO 51
          PRINT *,' WARNING - No matching particle ZAP',ZAP0
          STOP 'DENXS1 - ZAP not matched'
        END IF
        YL=FINTXS(EIN,RWO,RWO(LX),NP,INR(1),IER)
      END IF
C* Process the distributions
      IF(LF.EQ.1) THEN
C* Pointwise representation (Law 1)
        LEP=1
        LPP=LX
        LXE=LEP+NP
        LXX=LPP+NP
        KX =NX -NP
        NM =2
        Y1 =0
        EI1=0
        NE1=0
        CALL RDTAB2(LEF,C1,C2,LANG,LEP,NR,NE,NBT,INR,IER)
        DO 54 IE=1,NE
C* For each incident energy read the outg.particle energy distribution
        IF(MF.EQ.5) THEN
          CALL RDTAB1(LEF,C1,EI2,L1,L2,NRP,NF,NBT(NM),INR(NM)
     1               ,RWO(LXE),RWO(LXX),KX,IER)
          INE=INR(NM)
          IF(NRP.GT.1)
     1      PRINT *,'WARNING - Multiple Eout int.rng. MF 5, MT',MT
          IF(INE.GT.2) THEN
            PRINT *,'WARNING - Non-linear interp. for MF 5, MT',MT
            INE=2
          END IF
        ELSE
          MX=MRW-LXE
          CALL RDLIST(LEF,C1,EI2,L1,L2,N1,NF,RWO(LXE),MX,IER)
          IF(NF.GT.LXX-LXE) STOP 'DXSEN1 ERROR - Array limit'
          INE=LEP
          IF(INE.GT.2) THEN
            PRINT *,'WARNING - Non-linear interp. for MF/MT',MF,MT
            INE=2
          END IF
          DO 53 I=1,NF
          RWO(LXE-1+I)=RWO(LXE-2+2*I)
          RWO(LXX-1+I)=RWO(LXE-1+2*I)
   53     CONTINUE
        END IF
        IF(IER.NE.0) STOP 'DENXS1 ERROR reading energy.distrib.'
        IF(KEA.EQ.2) THEN
Case: Interpolate outgoing particle energy distributions
          CALL FINT2D(EIN,EI1,NE1 ,ENR     ,DXS     ,INR
     1                   ,EI2,NF  ,RWO(LXE),RWO(LXX),INE,KX)
        ELSE
Case: Interpolate distrib. to a given outgoing particle energy
          EA=RWO(LXE)
          EB=RWO(LXE-1+NF)
          CALL YTGEOU(SS,EA,EB,NF,RWO(LXE),RWO(LXX),INE)
          Y2=FINTXS(EOU,RWO(LXE),RWO(LXX),NF,INE,IER)/SS
          IF(EIN.GE.EI1 .AND. EIN.LE.EI2) THEN
            YY=( Y1 + (Y2-Y1)*(EIN-EI1)/(EI2-EI1) )
            YL=YL*YY
            GO TO 800
          END IF
        END IF
   54   CONTINUE
      ELSE IF(LF.EQ.11) THEN
C* Energy dependent Watt spectrum (Law 11)
        NX=MRW/2
        LX=NX+1
        UPEN=C1
        IF(EIN-UPEN.LE.0) THEN
          NEN=0
          GO TO 900
        END IF
C* Read and interpolate Watt parameter Wa
        CALL RDTAB1(LEF,C1,C2,L1,L2,NR,NP,NBT,INR
     1             ,RWO,RWO(LX),NX,IER)
        INE=INR(1)
        IF(NR.GT.1) THEN
          PRINT *,'WARNING - Multiple interp. ranges in MF5 MT',MT0
        END IF
        IF(INE.GT.2) THEN
          PRINT *,'WARNING - Non-linear interpol.law in MF5 MT',MT0
        END IF
        WA=FINTXS(EIN,RWO(1),RWO(LX),NP,INE,IER)
C* Read and interpolate Watt parameter Wb
        CALL RDTAB1(LEF,C1,C2,L1,L2,NR,NP,NBT,INR
     1             ,RWO,RWO(LX),NX,IER)
        INE=INR(1)
        IF(NR.GT.1) THEN
          PRINT *,'WARNING - Multiple interp. ranges in MF5 MT',MT0
        END IF
        IF(INE.GT.2) THEN
          PRINT *,'WARNING - Non-linear interpol.law in MF5 MT',MT0
        END IF
        WB=FINTXS(EIN,RWO(1),RWO(LX),NP,INE,IER)
        IF(KEA.EQ.2) THEN
Case: Generate outgoing particle energy distributions
          NE1=101
C*         Limit the table to the upper energy on the ENDF file
          EE =MIN(ETOP,EIN-UPEN)
          DE =EE/(NE1-1)
          DO 58 I=1,NE1
          EE =(I-1)*DE
          ENR(I)=EE
          DXS(I)=SPWATT(EE,EIN,UPEN,WA,WB)
   58     CONTINUE
        ELSE
Case: Calculate values at the given outgoing particle energy
          YL=YL*SPWATT(EOU,EIN,UPEN,WA,WB)
          GO TO 800
        END IF
      ELSE
C...    Secondary particle energy distributions MF 5 not coded
        PRINT *,'WARNING - Processing not coded for MF5 MT',MT0
        PRINT *,'                                      Law',LF
        IER=31
        GO TO 900
      END IF
      GO TO 800
C*
C* Process coupled energy/angle distributions MF6 of selected particle
      CALL FINDMT(LEF,ZA0,ZA,AWR,L1,L2,N1,N2,MAT,MF,MT,IER)
   60 LCT=L2
      NK =N1
      JNK=0
   61 JNK=JNK+1
C* Split the work array RWO to operate on function and argument
C* 1   - First argument
C* LX  - First function
C* LXE - Second argument
C* LXX - Second function
C* LXF - Third argument (angle-integrated case only)
C* LXY - Third function (angle-integrated case only)
C* KX2 - Max. permissible number of points per set
      KX =MRW/2
      LX =KX+1
      LD =KX/2
      LXE= 1+LD
      LXX=LX+LD
      KX2=LD/2
      LXF= 1+KX2
      LXY=LX+KX2
C* Retrieve the neutron yield
      CALL RDTAB1(LEF,ZAP,AWP,LIP,LAW,NR,NP,NBT,INR
     1           ,RWO,RWO(LX),KX,IER)
      IF(LAW.NE.7) THEN
C*        Set IER to flag error and terminate if not Law-7 in MF 6
        IER=23
        GO TO 900
      END IF
      IF(NINT(ZAP).NE.NINT(ZAP0)) THEN
        IF(JNK.GE.NK) THEN
          PRINT *,'DENXS1 WARNING - Particle not found',NINT(ZAP)
          IER=24
          GO TO 900
        END IF
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
      YL=FINTXS(EIN,RWO,RWO(LX),NP,INR(1),IER)
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
      IF(KEA.EQ.1) THEN
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
        IF(DEG.LT.0) THEN
C* Case: Integrate between distributions for two cosines
          CALL FYTG2D(    NEP1,RWO     ,RWO(LX)
     1               ,AI1,NEP3,RWO(LXF),RWO(LXY),INR(NM)
     1               ,AI2,NEP2,RWO(LXE),RWO(LXX),INR(NO),KX2)
        ELSE
C* Case: Interpolate between distributions for two cosines
          CALL FINT2D(AIN,AI1,NEP1,RWO     ,RWO(LX) ,INR(NM)
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
      IF(DEG.LT.0) SS=SS*(A2-A1)
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
      IF(KEA.EQ.2 .AND. DEG.LT.0) SAN=SAN*2
      GO TO 800
C*
C* Photon emission data
C... Assume isotropic scattering - no coding to read MF 14
  120 IF(KEA.NE.2) GO TO 150
      LO=L1
c...  IF( (MT0.GT. 50 .AND. MT0.LT. 91) .OR.
c... 1    (MT0.GT.600 .AND. MT0.LT.649) .OR.
c... 1    (MT0.GT.800 .AND. MT0.LT.849) ) THEN
      IF(LO.EQ.2) THEN
C* Transition probability arrays given
        LG =0
        LV =0
        LLI=1
        RWO(LLI)=0
        LLI=LLI+1
C* Read photon branching ratios to all discrete-leves up to the present
  122   LV =LV +1
        IWO(LV)=LLI
C* Changing LF between data sets is currently not accomodated
        IF(LG.GT.0 .AND. LG.NE.L2)
     1    STOP 'DXSEN1 ERROR - Redefined LF reading MF 12'
        LO =L1
        LG =L2
        NS =N1
        IF(LO.NE.2) THEN
          IER=43
          PRINT *,'WARNING - No coding for MF 12, MT',MT0,' LO',LO
          GO TO 900
        END IF
        LL =LV*(LG+1)
        DO 123 J=1,LL
        RWO(LLI+J)=0
  123   CONTINUE
        LX =MRW-LLI
        CALL RDLIST(LEF,ES,C2,LP,L2,NW,NT,RWO(LLI+1),LX,IER)
        IF(NW.GT.LL) THEN
          IER=43
          PRINT *,'ERROR - in MT',MT0,' Used space',NW
     1           ,' Exceeds reserved space for',LV,' levels'
          GO TO 900
        END IF
        RWO(LLI)=ES
        LLI=LLI+1+LL
C* Read the data for the next level
        CALL SKIPSC(LEF)
        MT =0
        CALL FINDMT(LEF,ZA0,ZA,AWR,L1,L2,N1,N2,MAT,MF,MT,IER)
        IF(IER.EQ.0 .AND. MT.LT.MT0) GO TO 122
        IWO(LV+1)=LLI
C*
C* All levels up to the current one read - sum the yields
c....
c....        print *,'     Processing MF/MT',MF,MT0
c....
        CALL SUMYLD(LV,LG,IWO,RWO,GTO,NT,NW)
C* Process the gamma lines
        NX =(MRW-NW)/2
        LXE=1+NW
        LXB=LXE+NX
        NEN=21
        NE1=1
        ENR(1)=RWO(1)
        DXS(1)=0
        DO 128 JT=1,NT
          EOU=RWO(1+(JT-1)*(LG+1))
          IF(EOU.LE.0) THEN
            PRINT *,'WARNING Gamma Eou=',EOU,' for mt,jt',MT0,JT
            GO TO 128
          END IF
          CALL FNGAUS(EOU,NEN,RWO(LXE),RWO(LXB),EPS)
          LUE=LXE+NEN
          LUX=LXB+NEN
          KX =NX -NEN
          CALL UNIGRD(NE1,ENR,NEN,RWO(LXE),NE2,RWO(LUE),KX)
C* Interpolate current distribution to the union grid
          CALL FITGRD(NE1,ENR,DXS,NE2,RWO(LUE),RWO(LUX))
          NE1=NE2
          DO 124 I=1,NE1
            ENR(I)=RWO(LUE-1+I)
            DXS(I)=RWO(LUX-1+I)
  124     CONTINUE
C* Interpolate saved distribution to the union grid
          CALL FITGRD(NEN,RWO(LXE),RWO(LXB),NE2,RWO(LUE),RWO(LUX))
C* Assume isotropic photon distributions
          ANG=0.5
C* Add the current to the saved distribution
          FRC=ANG*RWO(2+(JT-1)*(LG+1))
          IF(LG.EQ.2) FRC=FRC*RWO(3+(JT-1)*(LG+1))
          DO 126 I=1,NE1
            DXS(I)=DXS(I)+RWO(LUX-1+I)*FRC
  126     CONTINUE
  128   CONTINUE
        GO TO 800
      ELSE IF(LO.EQ.1) THEN
C* Multiplicities given
        NK=N1
C* Dummy-read the total photon multiplicity
        LXE=1
        LXX=MRW/2
        KX =LXX-LXE
        IF(NK.GT.1) CALL RDTAB1(LEF,C1,C2,L1,L2,NR,NP,NBT,INR
     1                         ,RWO(LXE),RWO(LXX),KX,IER)
C* Read photon data for all subsections
        DO 138 IK=1,NK
C* Photon energy and yield
        CALL RDTAB1(LEF,EG,ES,LP,LF,NR,NP,NBT,INR
     1             ,RWO(LXE),RWO(LXX),KX,IER)
        YL=FINTXS(EIN,RWO(LXE),RWO(LXX),NP,INR,IER)
        IF(IER.NE.0) THEN
          PRINT *,'ERROR READING MF/MT/IER',MF,MT,IER
          STOP 'DXSEND1 ERROR - Reading MF12'
        END IF
C* Modify photon energy for primary photons
        IF(LP.EQ.2) EG=EG+EIN*AWR/(AWR+1)
        IF(LF.EQ.1) THEN
C* Normalised tabulated function
          IF(IK.LT.NK) THEN
            PRINT *,'DXSEN1 WARNING - Tabulated distr. not last'
          END IF
          MF=15
          CALL FINDMT(LEF,ZA0,ZA,AWR,L1,L2,NC,N2,MAT,MF,MT,IER)
C* Only one section is allowed at present
          IF(NC.GT.1) THEN
            PRINT *,'DXSEN1 WARNING >1 section for MF/MT',MF,MT
            STOP 'DXSEN1 ERROR >1 section for MF 15'
          END IF
C* Read the fractional contribution of the section 
          CALL RDTAB1(LEF,C1,C2,L1,LF,NR,NP,NBT,INR
     1               ,RWO(LXE),RWO(LXX),KX,IER)
          YL=YL*FINTXS(EIN,RWO(LXE),RWO(LXX),NP,INR,IER)
          IF(LF.NE.1) THEN
            IER=99
            PRINT *,'WARNING - No coding for MF 15, MT',MT0,' LF',LF
            GO TO 900
          END IF
          EI1=0
          NE1=0
C* Read interpolation data for the incident energies
          CALL RDTAB2(LEF,C1,C2,L1,L2,NR,NE,NBT,INR,IER)
          NM =2
          DO 134 IE=1,NE
C* For each incident energy read the outg.particle energy distribution
          CALL RDTAB1(LEF,C1,EI2,L1,L2,NRP,NF,NBT(NM),INR(NM)
     1               ,RWO(LXE),RWO(LXX),KX,IER)
          IF(IER.NE.0) THEN
            PRINT *,'DXSEN1 ERROR - Reading MF/MT',MF,MT
            STOP 'DENXS1 ERROR reading energy.distrib.'
          END IF
          INE=INR(NM)
          IF(NRP.GT.1)
     1      PRINT *,'WARNING - Multiple Eout int.rng. MF 15, MT',MT
          IF(INE.GT.2) THEN
            PRINT *,'WARNING - Non-linear interp. for MF 15, MT',MT
            INE=2
          END IF
C* Interpolate outgoing particle energy distributions
          CALL FINT2D(EIN,EI1,NE1 ,ENR     ,DXS     ,INR
     1                   ,EI2,NF  ,RWO(LXE),RWO(LXX),INE,KX)
  134     CONTINUE
C*
        ELSE IF(LF.EQ.2) THEN
C* Discrete photon energy
          IER=99
          PRINT *,'WARNING - No coding for MF 12, MT',MT0,' LO',LO
          GO TO 900
        ELSE
          PRINT *,'ERROR - Illegal flag MF/MT/LF',MF,MT,LF
          STOP 'DXSEND1 ERROR - Illegal LF in MF 12'
        END IF
  138   CONTINUE
      ELSE
        IER=99
        PRINT *,'WARNING - No coding for MF 12, MT',MT0,' LO',LO
        GO TO 900
      END IF
      GO TO 800
C*
C* Photon angular distributions
  140 CONTINUE
        IER=99
        PRINT *,'WARNING - No coding for MF',MF,' MT',MT0
        GO TO 900
C* Photon continuum energy distributions
  150 CONTINUE
        IER=99
        PRINT *,'WARNING - No coding for MF 15, MT',MT0
        GO TO 900
C*
C* Distribution assembled - check that the last point is zero
  800 NEN=NE1
      IF(KEA.EQ.2 .AND. DXS(NEN).NE.0) THEN
        NEN=NEN+1
        ENR(NEN)=ENR(NEN-1)
        DXS(NEN)=0
      END IF
C* Scale the distribution by the cross section
      SS=YL*XS*SAN
      DO 804 I=1,NEN
      DXS(I)=SS*DXS(I)
  804 CONTINUE
C*
  900 RETURN
C*
  902 FORMAT(2F11.0,4I11)
      END
      SUBROUTINE SUMYLD(LV,LG,ID,RWO,GTO,NG,LW)
C-Title  : Subroutine SUMYLD
C-Purpose: Sum yields from lower levels and redefine gamma energies
C-Description:
C-D  LV  Total number of levels
C-D  LG  Flag for the number of parameters:
C-D       1  Branching ratios only
C-D       2  Branching ratios and gamma fractions given.
C-D  ID  index array giving the starting address of the data
C-D      for a each level in the work array RWO. Level zero
C-D      (ground state) only contains the level energy  in the
C-D      first word or array RWO. The data for each level consist
C-D      of pairs of numbers, giving the final level energy and
C-D      the branching ratio to that level.
C-D  GTO Sum of all gamma fractions - it can be used for
C-D      checking purposes. It should be close to 1.
C-D  NG  Final number of gamma lines packed into the output array.
C-D  LW  Length of the packed output vector.
C-
      DIMENSION  ID(1),RWO(1)
C* Make sure that the decaying levels form a complete set.
C* Fill missing levels
      DO 40 I=1,LV
        LI=ID(I)
        EL=RWO(1)
        ND=ID(I+1)-LI
C...
C...    PRINT *,I,LI,ND,(RWO(LI-1+J),J=1,ND)
C...
        IF(I.LT.2) GO TO 40
        DO 26 J=1,LV
C* Test J-th level at EA
          EA=RWO(LI+1)
C* Apply test to final level intervals of the present level
          DO 24 K=2,I
            LK=LI+1+(K-1)*(LG+1)
            EB=RWO(LK)
C* Check if tested level fals in between a tabulated interval
            EE=(EB+EA)*1.E-5
C...
C...        PRINT *,'     Elvl',EL,EA,EB,(EL-EA)*(EL-EB),EE
C...
            IF((EL-EA)*(EL-EB).GE.-EE) GO TO 23
C* Insert missing level and shift the rest
            LJ=ID(I+1)-(LG+1)
            LL=LJ-LK
C...
C...           PRINT *,'               Shift LJ,LK',LJ,LK
C...
            DO 22 L=1,LL
              RWO(LJ-L+LG+1)=RWO(LJ-L)
   22       CONTINUE
            RWO(LK  )=EL
            RWO(LK+1)=0
            EB=EL
   23       EA=EB
   24     CONTINUE
          LL=ID(J)
          EL=RWO(LL)
   26   CONTINUE
C...
C...        PRINT *,I,LI,ND,(RWO(LI-1+J),J=1,ND)
C...
   40 CONTINUE
C*
C* Transform final level energies to gamma energies
      DO 50 I=1,LV
        LI=ID(I)
        EL=RWO(LI)
C* Process final states of current level
        DO 46 J=1,I
          LJ=LI+1+(J-1)*(LG+1)
          EG=EL-RWO(LJ)
          RWO(LJ)=EG
   46   CONTINUE
C...
C...      LJ=ID(I+1)-1
C...      PRINT *,I,(RWO(J),J=LI,LJ)
C...
   50 CONTINUE
C*
C* Define gamma intensities and level population (work backwards)
      DO 54 I=1,LV
        LI=ID(I)
        RWO(LI)=0
   54 CONTINUE
      RWO(LI)=1
      GTO=0
C* Overwrite level energies with level population
      DO 80 I=1,LV
        JV =LV+1-I
        LI =ID(JV)
        GLV=RWO(LI)
C...
C...    PRINT *,'LVL,GLV',JV,GLV
C...
        DO 74 J=1,JV
          KJ =LI+2+(J-1)*(LG+1)
          GJ =RWO(KJ)
          GV =GLV*GJ
          RWO(KJ)=GV
          KV =JV-J
          IF(KV.GT.0) THEN
            K=ID(KV)
            RWO(K)=RWO(K)+GV
          ELSE
            GTO=GTO+GV
          END IF
C...
C...      PRINT *,'  L,J,GJ,GV',LV+1-I,KV,GJ,GV
C...
   74   CONTINUE
C...
C...    LI=ID(I  )
C...    LJ=ID(I+1)-1
C...    PRINT *,I,(RWO(J),J=LI,LJ)
C...
   80 CONTINUE
C...
C...      PRINT *,'                        *** Next ***'
C...      PRINT *,0,GTO
C...      DO 82 I=1,LV
C...        LI=ID(I  )
C...        LJ=ID(I+1)-1
C...        PRINT *,I,(RWO(J),J=LI,LJ)
C...   82 CONTINUE
C...
C*
C* Pack the gamma energies and yields
      LI=1
      LW=1
      NG=0
      DO 88 I=1,LV
        LI=LI+1
        DO 86 J=1,I
          IF(RWO(LI+2).LE.0) GO TO 85
          RWO(LW)=RWO(LI+1)
          DO 84 K=1,LG
            RWO(LW+K)=RWO(LI+1+K)
   84     CONTINUE
          NG=NG+1
          LW=LW+1+LG
   85     LI=LI+1+LG
   86   CONTINUE
   88 CONTINUE
C...
C...          PRINT *,'Gamma energies and intensities'
C...          DO 92 I=1,NG
C...          K=(I-1)*(LG+1)
C...          PRINT *,I,RWO(1+K),RWO(2+K)
C...   92     CONTINUE
C...
C...          IF(LV.GT.2) STOP
C...
      RETURN
      END
      FUNCTION SPWATT(EE,EI,EU,WA,WB)
C-Title  : Function SPWATT
C-Purpose: Calculate the Watt fission spectrum value at energy EE
      DATA PI/3.1415926/
C* Upper limit of the final particle energy
      EPF=EI-EU
C* Normalisation constant
      E0=SQRT(PI*WA*WA*WA*WB/4)*EXP(WA*WB/4)/2
      E1=SQRT(EPF/WA)
      E2=SQRT(WA*WB/4)
      E3=ERRFN1(E1-E2)
      E4=ERRFN1(E1+E2)
      E5=WA*EXP(-EPF/WA)*SINH(SQRT(EPF*WB))
      CNRM=E0*(E3+E4)-E3
C* Spectrum
      SPWATT=EXP(-EE/WA)*SINH(SQRT(EE*WB))/CNRM
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
   40 READ (C66,92) ZA,AW,L1,L2,N1,N2
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
C-Description:
C-D  The TAB1 record of an ENDF-formatted file is read.
C-D  Error condition:
C-D    IER=22 on exit if available field length NMX is exceeded.
C-
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
      SUBROUTINE RDLIST(LEF,C1,C2,L1,L2,N1,N2,VK,MVK,IER)
C-Title  : Subroutine RDLIST
C-Purpose: Read an ENDF LIST record
      DIMENSION    VK(1)
C*
      READ (LEF,902) C1,C2,L1,L2,N1,N2
      IF(N1.GT.MVK) THEN
        IER=-1
        RETURN
      END IF
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
        J=2*NEP2
        IF(J.GT.MEP2) THEN
C...      PRINT *,'J,MEP2',J,MEP2
          STOP 'FINT2D ERROR - MEP2 limit exceeded'
        END IF
        DO 20 I=2,NEP2
        J=J-1
        EN2(J)=EN2(NEP2+2-I)
        XS2(J)=XS2(NEP2+2-I)
        J=J-1
        EN2(J)=EN2(NEP2+2-I)
        XS2(J)=XS2(NEP2+1-I)
   20   CONTINUE
        NEP2=2*NEP2-1
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
      X2=XS2(LUNI-1+I)
      XX=X2
      IF(AI1.NE.AI2) XX=X1+(X2-X1)*(AI-AI1)/(AI2-AI1)
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
      EA =MAX(1.E-5,EA)
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

        J=2*NEP2
        IF(J.GT.MEP2) THEN
C...      PRINT *,'J,MEP2',J,MEP2
          STOP 'FINT2D ERROR - MEP2 limit exceeded'
        END IF
        DO 20 I=2,NEP2
        J=J-1
        EN2(J)=EN2(NEP2+2-I)
        XS2(J)=XS2(NEP2+2-I)
        J=J-1
        EN2(J)=EN2(NEP2+2-I)
        XS2(J)=XS2(NEP2+1-I)
   20   CONTINUE
        NEP2=2*NEP2-1
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
   30 LUNI=1+NEP2
      KX=MEP2-LUNI
      CALL UNIGRD(NEP1,EN1,NEP2,EN2,NEU,EN2(LUNI),KX)
C* Interpolate integrated distribution to the union grid
      CALL FITGRD(NEP0,EN0,XS0,NEU,EN2(LUNI),XS2(LUNI))
      DO 31 I=1,NEU
      EN0(I)=EN2(LUNI-1+I)
      XS0(I)=XS2(LUNI-1+I)
   31 CONTINUE
C* Interpolate first distribution to the union grid
      CALL FITGRD(NEP1,EN1,XS1,NEU,EN2(LUNI),XS2(LUNI))
      DO 32 I=1,NEU
      EN1(I)=EN2(LUNI-1+I)
      XS1(I)=XS2(LUNI-1+I)
   32 CONTINUE
C* Interpolate second distribution to the union grid
      CALL FITGRD(NEP2,EN2,XS2,NEU,EN2(LUNI),XS2(LUNI))
C* Add the contribution to the integral
      DO 34 I=1,NEU
      EN2(I)=EN2(LUNI-1+I)
      XS2(I)=XS2(LUNI-1+I)
      IF(INR.EQ.1) THEN
        FF=XS2(I)*(AI2-AI1)
      ELSE
        FF=0.5*(XS1(I)+XS2(I))*(AI2-AI1)
      END IF
      XS0(I)=XS0(I)+FF
      XS1(I)=XS2(I)
   34 CONTINUE
      AI1=AI2
      NEP0=NEU
      NEP1=NEU
      NEP2=NEU
C* Integration completed
   40 RETURN
      END
      FUNCTION ERRFN1(X)
C-Title  : ERRFN1 function
C-Purpose: Calculate the Error function
C-Description:
C-D calculate the error function with accuracy better than 2.5e-5
C-D Ref.:Handbook of Math.Funct...,Abramowitz,Stegun
C-D
C-Author : A.Trkov, J.Stefan Institute,Ljubljana,Slovenia, 1985
      DATA P,A1,A2,A3/ .47047, .3480242, -.0958798, .7478556/
      ERRFN1=1.
      IF(X.GT.4.) RETURN
      T=1./(1.+P*X)
      ERRFN1=1.-T*(A1+T*(A2+T*A3))*EXP(-X*X)
      RETURN
      END
      FUNCTION POLYNX(X,C,NC)
C-Title  : POLINX function
C-Purpose: Polynomial Pn(x) of order NC with NC+1 coefficients C(i)
      DIMENSION C(1)
      NC1=NC+1
      F  =C(NC1)
      IF(NC.LT.1) GO TO 20
      DO 10 I=1,NC
      F  =F*X + C(NC1-I)
   10 CONTINUE
   20 POLYNX = F
      RETURN
      END
