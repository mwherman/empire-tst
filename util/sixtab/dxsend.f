      SUBROUTINE DXSELM(LEF,NUC,ZEL,FRC,ZAP,MF0,MT0,KEA,EIN,PAR,EPS
     1                 ,ENR,DXS,UXS,RWO,NEN,MEN,MRW,LTT,ELV)
C-Title  : Subroutine DXSELM
C-Purpose: Extract double differential cross sections from ENDF
C-Author : A.Trkov, International Atomic Energy Agency, Vienna, Austria.
C-Version: 15-Dec-2003 (Original code).
C-V  06/02 Add UXS parameter for uncertainty of DXS
C-Description:
C-D  The function of this routine is an extension of DXSEND and DXSEN1
C-D  routines, which retrieves the differential cross section at a
C-D  specified incident particle energy and scattering angle. The
C-D  hierarchy of the routines is as follows:
C-D    DXSELM - main entry, looping DXSEND for all nuclides (isotopes)
C-D             in a mixture (element), if necessary.
C-D    DXSEND - called from DXSEND, looping over all contributing
C-D             reactions (e.g. summing over reactions emitting a
C-D             specific nuclide to produce particle emission
C-D             spectrum for that particle).
C-D    DXSEN1 - called from DXSEND, retrieving the data from the
C-D             ENDF file and reconstructing the desired cross
C-D             section or spectrum.
C-D  For details see the description of DXSEND and DXSEN1 routines.
C-D  Formal parameters are:
C-D  LEF  - File unit number from which the ENDF file is read.
C-D  NUC  - Number of nuclides in a mixture (if NUC=0, a single
C-D         nuclide with fractional abundance =1 is assumed).
C-D  ZEL  - Requested nuclide list. The nuclides are identified by
C-D         their ZA identification number. If ZA>0 it is given in
C-D         terms of Z*1000+A+LIS0/10 where Z is the atomic number,
C-D         A the mass number and LIS0 the metastable state number.
C-D         When ZA<0 it implies the ENDF material MAT number.
C-D  FRC    Fractional abundance of nuclides from the list.
C-D  ZAP  - Outgoing particle ZA designation (ZAP=1 for neutrons).
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
C-D  UXS  - Absolute uncertainty of the function vector.
C-D  RWO  - Work array of length MRW.
C-D  NEN  - Number of points in the assembled output cross section
C-D         vector.
C-D  MEN  - Available size of ENR and DXS arrays.
C-D  MRW  - Available size of the RWO work array.
C-D  LTT  - Unit number for printing error messages and warnings.
C-D         If less or equal zero, printout is suppressed and
C-D         LTT is set equal to -IER where the values imply:
C-D           1  Specified material not found.
C-D           2  End-of-file before material found.
C-D           3  General read error.
C-D           9  Array limit MRW exceeded.
C-D          11  Multiple interpolation ranges and/or law other
C-D              than lin-lin encountered.
C-D          12  Correlated energy-angle distributions not in Law-7
C-D              representation.
C-D          13  Processing not coded for specified reaction.
C-D          14  Requested particle not found.
C-D  ELV  - Discrete level energy where discrete levels are not
C-D         identified by their MT number.
C-D
C-Extern.: DXSEND,DXSEN1
C*
      DIMENSION    RWO(MRW),ENR(MEN),DXS(MEN),UXS(MEN),ZEL(1),FRC(1)
C*
      NE1=0
      IEL=1
      ML =0
      LX =1
      KRW=MRW
      IZA=ABS(ZEL(IEL))+0.01
      IF(ZEL(IEL).LT.0) IZA=-IZA
      DO I=1,MEN
        DXS(I)=0
        UXS(I)=0
      END DO
      IF(NUC.GT.0) GO TO 60
C* Check if the specified material exist as is
      MF=0
      MT=0
      ZE=ZEL(IEL)
      CALL FINDMT(LEF,ZE,ZA,AWR,L1,L2,NS,N2,MAT,MF,MT,IER)
      IF(IER.EQ.0) THEN
        REWIND LEF
        GO TO 60
      END IF
C* Reconstruct elemental composition
      IA=IZA-1000*(IZA/1000)
      IF(IZA.LT.0 .OR. IA.NE.0) THEN
        IER=1
        IF(LTT.GT.0) THEN
          WRITE(LTT,903) ' DXSELM WARNING - Nuclide not found   : ',IZA
        ELSE
          LTT=-IER
        END IF
        RETURN
      END IF
      IZ=IZA/1000
      CALL ELMABN(IZ,NUC,ZEL,FRC)
C...
C...      PRINT *,'IZ,NUC,IEL',IZ,NUC
C...      DO I=1,NUC
C...        PRINT *,ZEL(I),FRC(I)
C...      END DO
C...
C*
C* Retrieve the distribution for the specified nuclide
   60 ZA=ZEL(IEL)
      IZA=NINT(ZA)
      KK =KEA
      PP =PAR
      MF =MF0
      MT =MT0
      CALL DXSEND(LEF,ZA,ZAP,MF,MT,KK,EIN,PP,EPS,ENR,DXS,UXS
     1           ,RWO(LX),NE,MEN,KRW,LTT,ELV)
      IF(NE.LE.0) THEN
        IF(LTT.GT.0 .AND. NUC.GT.1) THEN
        WRITE(LTT,903) ' DXSELM WARNING - Missing contrib. from ',IZA
        END IF
        ML=ML+1
        GO TO 74
      END IF
      NEN=NE
      IF(NUC.EQ.0) GO TO 90
C* Multiply by the fractional abundance
      IF(LTT.GT.0) THEN
        IF(FRC(IEL).LT.1.E-12) THEN
        WRITE(LTT,903) ' DXSELM WARNING - zero abun. for nuclide',IZA
        END IF
      END IF
      DO I=1,NE
        DXS(I)=DXS(I)*FRC(IEL)
      END DO
      IF(NE1.EQ.0) GO TO 70
C* Begin processing the previously accumulated distribution
      IF( NEN.EQ.0) THEN
        NEN=NE1
        GO TO 74
      END IF
C* Move the previously saved distribution in the work field
      LX=MRW/2
      DO I=1,NE1
        RWO(LX-1+I)=RWO(NE1+I)
      END DO
C* Generate the union grid
      LUE= 1+NE1
      LUX=LX+NE1
      KX =MRW-LUX
      CALL UNIGRD(NEN,ENR,NE1,RWO,NE2,RWO(LUE),KX)
C* Interpolate current distribution to the union grid
      CALL FITGRD(NEN,ENR,DXS,NE2,RWO(LUE),RWO(LUX))
      NEN=NE2
      DO I=1,NEN
        ENR(I)=RWO(LUE-1+I)
        DXS(I)=RWO(LUX-1+I)
      END DO
C* Interpolate saved distribution to the union grid
      CALL FITGRD(NE1,RWO,RWO(LX),NE2,RWO(LUE),RWO(LUX))
C* Add the current to the saved distribution
      DO I=1,NEN
        DXS(I)=DXS(I)+RWO(LUX-1+I)
      END DO
C* Save the summed distribution 
   70 LX=1+NEN*2
      KRW=MRW-LX
      NE1=NEN
      DO I=1,NEN
        RWO(I    )=ENR(I)
        RWO(I+NEN)=DXS(I)
      END DO
C* Select next reaction
   74 IF(IEL.LT.NUC) THEN
        IEL=IEL+1
        GO TO 60
      END IF
C*
C* All processing completed
   90 CONTINUE
      IF(ML.GT.0 .AND. NUC.GT.0) THEN
        WRITE(LTT,904) ML,NUC
      END IF
c...
c...      print *,'nen',nen
C...      print *,enr(1),dxs(1)
C...      print *,enr(nen),dxs(nen)
c...
      RETURN
C*
  901 FORMAT(2A40)
  903 FORMAT(A40,I6)
  904 FORMAT(' DXSELM WARNING - failed reconstructing'
     1       ,I3,' out of',I3,' nuclides')
      END
      SUBROUTINE ELMABN(IZ,NUC,ZEL,FRC)
C-Title  : Subroutine ELMABN
C-Purpose: Define fractional isotopic abundance of elements
C-Version:
C-V 04/11 Add data A>60
      PARAMETER  (MXIZ=24,MXEL=100)
C* Main array containing the abundances
      DIMENSION ZAB(MXIZ,MXEL),ZEL(1),FRC(1)
C* Equivalenced fields for the data statement
      DIMENSION Z00(MXIZ,  4 )
      DIMENSION Z05(MXIZ,  3 )
      DIMENSION Z08(MXIZ,  2 )
      DIMENSION Z10(MXIZ,  4 )
      DIMENSION Z14(MXIZ,  4 )
      DIMENSION Z18(MXIZ,  2 )
      DIMENSION Z20(MXIZ,  4 )
      DIMENSION Z24(MXIZ,  4 )
      DIMENSION Z28(MXIZ,  2 )
      DIMENSION Z30(MXIZ,  4 )
      DIMENSION Z34(MXIZ,  4 )
      DIMENSION Z38(MXIZ,  2 )
      DIMENSION Z40(MXIZ,  4 )
      DIMENSION Z44(MXIZ,  4 )
      DIMENSION Z48(MXIZ,  2 )
      DIMENSION Z50(MXIZ,  4 )
      DIMENSION Z54(MXIZ,  4 )
      DIMENSION Z58(MXIZ,  2 )
      DIMENSION Z60(MXIZ,  4 )
      DIMENSION Z64(MXIZ,  4 )
      DIMENSION Z68(MXIZ,  2 )
      DIMENSION Z70(MXIZ,  4 )
      DIMENSION Z74(MXIZ,  4 )
      DIMENSION Z78(MXIZ,  2 )
      DIMENSION Z80(MXIZ,  4 )
      DIMENSION Z84(MXIZ,  4 )
      DIMENSION Z88(MXIZ,  2 )
      DIMENSION Z90(MXIZ,  4 )
      DIMENSION Z94(MXIZ,  4 )
      DIMENSION Z98(MXIZ,  2 )
      EQUIVALENCE ( ZAB(1,  1 ), Z00(1,1) )
      EQUIVALENCE ( ZAB(1,  5 ), Z05(1,1) )
      EQUIVALENCE ( ZAB(1,  8 ), Z08(1,1) )
      EQUIVALENCE ( ZAB(1, 10 ), Z10(1,1) )
      EQUIVALENCE ( ZAB(1, 14 ), Z14(1,1) )
      EQUIVALENCE ( ZAB(1, 18 ), Z18(1,1) )
      EQUIVALENCE ( ZAB(1, 20 ), Z20(1,1) )
      EQUIVALENCE ( ZAB(1, 24 ), Z24(1,1) )
      EQUIVALENCE ( ZAB(1, 28 ), Z28(1,1) )
      EQUIVALENCE ( ZAB(1, 30 ), Z30(1,1) )
      EQUIVALENCE ( ZAB(1, 34 ), Z34(1,1) )
      EQUIVALENCE ( ZAB(1, 38 ), Z38(1,1) )
      EQUIVALENCE ( ZAB(1, 40 ), Z40(1,1) )
      EQUIVALENCE ( ZAB(1, 44 ), Z44(1,1) )
      EQUIVALENCE ( ZAB(1, 48 ), Z48(1,1) )
      EQUIVALENCE ( ZAB(1, 50 ), Z50(1,1) )
      EQUIVALENCE ( ZAB(1, 54 ), Z54(1,1) )
      EQUIVALENCE ( ZAB(1, 58 ), Z58(1,1) )
      EQUIVALENCE ( ZAB(1, 60 ), Z60(1,1) )
      EQUIVALENCE ( ZAB(1, 64 ), Z64(1,1) )
      EQUIVALENCE ( ZAB(1, 68 ), Z68(1,1) )
      EQUIVALENCE ( ZAB(1, 70 ), Z70(1,1) )
      EQUIVALENCE ( ZAB(1, 74 ), Z74(1,1) )
      EQUIVALENCE ( ZAB(1, 78 ), Z78(1,1) )
      EQUIVALENCE ( ZAB(1, 80 ), Z80(1,1) )
      EQUIVALENCE ( ZAB(1, 84 ), Z84(1,1) )
      EQUIVALENCE ( ZAB(1, 88 ), Z88(1,1) )
      EQUIVALENCE ( ZAB(1, 90 ), Z90(1,1) )
      EQUIVALENCE ( ZAB(1, 94 ), Z94(1,1) )
      EQUIVALENCE ( ZAB(1, 98 ), Z98(1,1) )
C*
      DATA Z00/
     &  1001.,0.999885, 1002.,0.000115,     0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &  2003.,.00000137,2004.,0.99999863,   0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &  3006.,0.0759,   3007.,0.9241,       0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &  4009.,1.0,         0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0./
      DATA Z05/
     &  5010.,0.199,    5011.,0.801,        0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &  6012.,0.9893,   6013.,0.0107,       0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &  7014.,0.99632,  7015.,0.00368,      0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0./
      DATA Z08/
     &  8016.,0.99757,  8017.,0.00038,   8018.,0.00205,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &  9019.,1.0,         0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0./
      DATA Z10/
     & 10020.,0.9048,  10021.,0.0027,   10022.,0.0925,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     & 11023.,1.0,         0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     & 12024.,0.7899,  12025.,0.10,     12026.,0.1101,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     & 13027.,1.0,         0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0./
      DATA Z14/
     & 14028.,0.922297,14029.,0.046832,     0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     & 15031.,1.0,         0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     & 16032.,0.9493,  16033.,0.0076,   16034.,0.0429,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     & 17035.,0.7578,  17037.,0.2422,       0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0./
      DATA Z18/
     & 18036.,0.003365,18038.,0.000632, 18040.,0.996003,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     & 19039.,0.932581,19040.,0.000117, 19041.,0.067302,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0./
       DATA Z20/
     & 20040.,0.9694,  20042.,0.00647,  20043.,0.00135,
     & 20044.,0.0209,  20046.,0.00004,  20048.,0.00187,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     & 21021.,1.0,         0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     & 22046.,0.0825,  22047.,0.0744,   22048.,0.7372,
     & 22049.,0.0541,  22050.,0.0518,       0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     & 23050.,0.0025,  23051.,0.9975,       0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0./
       DATA Z24/
     & 24050.,0.04345, 24052.,0.83789,  24053.,0.09501,
     & 24054.,0.02365,     0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     & 25055.,1.0,         0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     & 26054.,0.05845, 26056.,0.91754,  26057.,0.02119,
     & 26058.,0.00282,     0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     & 27059.,1.0,         0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0./
       DATA Z28/
     & 28058.,0.680769,28060.,0.262231, 28061.,0.011399,
     & 28062.,0.036345,28064.,0.009256,     0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     & 29063.,0.6917,  29065.,0.3083,       0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0./
      DATA Z30/
     & 30064.,0.4863,  30066.,0.2790,   30067.,0.0410,
     & 30068.,0.1875,  30070.,0.0062,       0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     & 31069.,0.60108, 31071.,0.39892,      0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     & 32070.,0.2084,  32072.,0.2754,   32073.,0.0773,
     & 32074.,0.3628,  32076.,0.0761,       0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     & 33075.,1.0,         0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0./
      DATA Z34/
     & 34074.,0.0089,  34076.,0.0937,   34077.,0.0763,
     & 34078.,0.2377,  34080.,0.4961,   34082.,0.0873,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     & 35079.,0.5069,  35081.,0.4931,       0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     & 36078.,0.0035,  36080.,0.0228,   36082.,0.1158,
     & 36083.,0.1149,  36084.,0.570,    36086.,0.1730,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     & 37085.,0.7217,  37087.,0.2783,       0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0./
      DATA Z38/
     & 38084.,0.0056,  38086.,0.0986,   38087.,0.070,
     & 38088.,0.8258,      0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     & 39089.,1.0,         0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0./
      DATA Z40/
     & 40090.,0.5145,  40091.,0.1122,   40092.,0.1715,
     & 40094.,0.1738,  40096.,0.0280,       0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     & 41093.,1.0,         0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     & 42092.,0.1484,  42094.,0.0925,   42095.,0.1592,
     & 42096.,0.1668,  42097.,0.0955,   42098.,0.2413,
     & 42100.,0.0963,      0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     & 43000.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0./
      DATA Z44/
     & 44096.,0.0554,  44098.,0.0187,   44099.,0.1276,
     & 44100.,0.1260,  44101.,0.1706,   44102.,0.3155,
     & 44104.,0.1862,      0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     & 45103.,1.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     & 46102.,0.0102,  46104.,0.1114,   46105.,0.2233,
     & 46106.,0.2733,  46108.,0.2646,   46110.,0.1172,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     & 47107.,0.51839, 47109.,0.48161,      0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0./
      DATA Z48/
     & 48106.,0.0125,  48108.,0.0089,   48110.,0.1280,
     & 48111.,0.1280,  48112.,0.2413,   48113.,0.1222,
     & 48114.,0.2873,  48116.,0.0749,       0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     & 49113.,0.0429,  49115.,0.9571,       0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0./
      DATA Z50/
     & 50112.,0.0097,  50114.,0.0066,   50115.,0.0034,
     & 50116.,0.1454,  50117.,0.0768,   50118.,0.2422,
     & 50119.,0.0859,  50120.,0.3258,   50122.,0.0463,
     & 50124.,0.0579,      0.,0.,           0.,0.,
     & 51121.,0.5721,  51123.,0.4279,       0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     & 52120.,0.0009,  52122.,0.0255,   52123.,0.0089,
     & 52124.,0.0474,  52125.,0.0707,   52126.,0.1884,
     & 52128.,0.3174,  52130.,0.3408,       0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     & 53127.,1.0,         0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0./
      DATA Z54/
     & 54124.,0.009,   54126.,0.0009,   54128.,0.0192,
     & 54129.,0.2644,  54130.,0.0408,   54131.,0.2118,
     & 54132.,0.2689,  54134.,0.1044,   54136.,0.0887,
     &     0.,0.,          0.,0.,           0.,0.,
     & 55133.,1.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     & 56130.,0.00106, 56132.,0.00101,  56134.,0.02417,
     & 56135.,0.06592, 56136.,0.07854,  56137.,0.11232,
     & 56138.,0.71698,     0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     & 57138.,0.00090, 57139.,0.99910,      0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0./
      DATA Z58/
     & 58136.,0.00185, 58138.,0.00251,  58140.,0.88450,
     & 58142.,0.11114,     0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     & 59141.,1.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0./
      DATA Z60/
     & 60142.,0.373,   60143.,0.122,    60144.,0.238,
     & 60145.,0.083,   60146.,0.172,    60148.,0.057,
     & 60150.,0.056,       0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     & 62144.,0.0307,  62147.,0.1499,   62148.,0.1124,
     & 62149.,0.1382,  62150.,0.0738,   62152.,0.2675,
     & 62154.,0.2275,      0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     & 63151.,0.4781,  63153.,0.5219,       0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0./
      DATA Z64/
     & 64152.,0.0020,  64154.,0.0218,   64155.,0.1480,
     & 64156.,0.2047,  64157.,0.1565,   64158.,0.2484,
     & 64160.,0.2186,      0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     & 65159.,1.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     & 66156.,0.0006,  66158.,0.0010,   66160.,0.0234,
     & 66161.,0.1891,  66162.,0.2551,   66163.,0.2490,
     & 66164.,0.2818,      0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     & 67165.,1.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0./
      DATA Z68/
     & 68162.,0.0014,  68164.,0.0161,   68166.,0.3361,
     & 68167.,0.2293,  68168.,0.2678,   68170.,0.1493,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     & 69169.,1.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0./
      DATA Z70/
     & 70168.,0.0013,  70170.,0.0304,   70171.,0.1428,
     & 70172.,0.2183,  70173.,0.1613,   70174.,0.3183,
     & 70176.,0.1276,      0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     & 71175.,0.9741,  71176.,0.0259,       0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     & 72174.,0.0016,  72176.,0.0526,   72177.,0.1860,
     & 72178.,0.2728,  72179.,0.13629,  72180.,0.3508,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     & 73180.,0.00012, 73181.,0.99988,      0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0./
      DATA Z74/
     & 74180.,0.0012,  74182.,0.2650,   74183.,0.1431,
     & 74184.,0.3064,  74186.,0.2842,       0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     & 75185.,0.3740,  75187.,0.6260,       0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     & 76184.,0.0002,  76186.,0.0159,   76187.,0.0196,
     & 76188.,0.1324,  76189.,0.1615,   76190.,0.2626,
     & 76192.,0.4078,      0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     & 77191.,0.373,   77193.,0.627,        0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0./
      DATA Z78/
     & 78190.,0.00014, 78191.,0.00782,  78194.,0.32967,
     & 78195.,0.33832, 78196.,0.25242,  78198.,0.07163,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     & 79197.,1.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0./
      DATA Z80/
     & 80196.,0.0015,  80198.,0.0997,   80199.,0.1687,
     & 80200.,0.2310,  80201.,0.1318,   80202.,0.2986,
     & 80204.,0.0687,      0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     & 81203.,0.29524, 81205.,0.70476,      0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     & 82204.,0.014,   82206.,0.241,    82207.,0.221,
     & 82208.,0.524,       0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     & 83209.,1.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0./
      DATA Z84/
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0./
      DATA Z88/
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0./
      DATA Z90/
     & 90232.,1.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     & 92234.,0.000055,92235.,0.007200, 92238.,0.99274,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0./
      DATA Z94/
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0./
      DATA Z98/
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0.,
     &     0.,0.,          0.,0.,           0.,0./
C*
      NUC=0
c...
C...  IF(IZ.GE.60) STOP 'ERROR - Data for IZ>=59 not availabe'
c...
      DO I=1,MXIZ
        ZEL(I)=ZAB(2*I-1,IZ)
        FRC(I)=ZAB(2*I  ,IZ)
        IF(NINT(ZEL(I)).LE.0) RETURN
        NUC=NUC+1
      END DO
      RETURN
      END
      SUBROUTINE DXSEND(LEF,ZA0,ZAP0,MF0,MT0,KEA,EIN,PAR,EPS
     1                 ,ENR,DXS,UXS,RWO,NEN,MEN,MRW,LTT,ELV)
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
C-V  04/01 Improved diagnostics.
C-V  04/09 - Allow discrete level matching for cross sections
C-V        - Redefine the requested level energy the nearest actual level
C-V  05/09 Implement coding for particle production cross sections
C-V        (except photon production).
C-V  06/02 Add UXS array to contain uncertainty in DXS.
C-Description:
C-D  The function of this routine is an extension of DXSEN1, which
C-D  retrieves the differential cross section at a specified incident
C-D  particle energy and scattering angle. If a special MT number
C-D  is specified (for example, MT=5 for particle emission where
C-D  particle is defined by its ZA designation in ZAP0), DXSEN1
C-D  is called recursively for all neutron emission reactions and
C-D  all contributions are summed.
C-D  For details see the description of the DXSEN1 routine.
C-Extern.: DXSEN1
C-
C* Limits: Max.No.of reactions=MXL, interp.ranges=MXI
      PARAMETER   (MXL=200,MXI=100,MXE=20)
      CHARACTER*40 MSG(MXE)
      DIMENSION    NBT(MXI),INR(MXI),LST(MXL)
C*
      DIMENSION    RWO(MRW),ENR(MEN),DXS(MEN),UXS(MEN)
      DATA MSG/
     1  'Specified material not found            ',
     2  'End-of-file before material found       ',
     3  'General read error                      ',
     4  '?','?','?','?','?',
     9  'Array limit MRW exceeded                ',
     D  '?',
     1  'Multiple interp. ranges and/or law not 2',
     2  'Correlated ener/ang distr. not in Law-7 ',
     3  'Processing not coded for specified react',
     4  'Requested particle not found            ',
     5  'No differential data present            ',
     6  '?','?','?','?',
     D  'Unknown error ?                         '/
C*
      LOOP=0
      LX  =1
      NE1 =0
      NEN =0
      KRW =MRW
      MF  =MF0
      MT  =MT0
      IINL=0
      I600=0
      I800=0
      MINL=0
      M600=0
      M800=0
      IF(KEA.EQ.0 .AND. MT0/10000.NE.4) THEN
C* Do not include particle multiplicities for cross sections
        ZAP =-1
      ELSE
        ZAP=ZAP0
      END IF
c...
c...  print *,'DXSEND: ZA0,ZAP0,MF0,MT0,KEA,EIN,PAR,EPS'
c...  print *,         ZA0,ZAP0,MF0,MT0,KEA,EIN,PAR,EPS
c...
C*
C* Find the appropriate discrete level for inelastic angular distrib.
      IF(ELV.GT.0 .AND. (MF0.EQ.3 .OR. MF0.EQ.4) .AND.
     &  ( (MT0.GE. 51 .AND. MT0.LT. 91) .OR.
     &    (MT0.GE.600 .AND. MT0.LT.649) .OR.
     &    (MT0.GE.800 .AND. MT0.LT.849) ) ) THEN
C* Find the matching energy level for discrete inelastic ang.distr.
        REWIND LEF
        CALL SKIPSC(LEF)
        MF=3
        DE=ELV
        EL1=ELV
   12   MT=0
        CALL FINDMT(LEF,ZA0,ZA,AW,L1,L2,N1,N2,MAT,MF,MT,IER)
        IF(MT.GT.90 .OR. IER.NE.0) THEN
C* No more discrete levels - use last
          MT =MT0
          ELV=EL1
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
          EL1=EE
        END IF
        IF(EE.LT.ELV) GO TO 12
C* Assign MT number corresponding to the level nearest to requested
        MT =MT0
C* Redefine the requested level energy with the actual
        ELV=EL1
      END IF
C*
C* Check if reaction summation is required (request MT=5)
      IF(MT0.NE.5) GO TO 60
C* Find particle emission reactions
      REWIND LEF
      CALL SKIPSC(LEF)
      ZAP =ZAP0
      IZAP=ZAP+0.1
C*
C* Prepare reaction list for light particles in MT5
C*
      IF(IZAP.GE.1 .AND.IZAP.LE.2004) THEN
        I18=0
   20   MF=3
        MT=0
        CALL FINDMT(LEF,ZA0,ZA,AW,L1,L2,N1,N2,MAT,MF,MT,IER)
        IF(IER.NE.0) GO TO 50
        CALL SKIPSC(LEF)
C* Select contributing reactions (yield >= 0)
        CALL  YLDPOU(YI,MT,IZAP)
        IF(MT.EQ.18) I18=1
C* Skip chance fission if total fission is given
        IF(I18.EQ.8 .AND.
     &    (MT.EQ.19 .OR. MT.EQ.20 .OR. MT.EQ.21 .OR. MT.EQ.38)) YI=-1
        IF(YI.GE.0) THEN
          LOOP=LOOP+1
          IF(LOOP.GT.MXL) STOP 'DXSEND ERROR - MXL Limit exceeded'
          LST(LOOP)=MT
        END IF
        IF(MT.GE. 50 .AND. MT.LE. 91) IINL=1
        IF(MT.GE.600 .AND. MT.LE.649) I600=1
        IF(MT.GE.800 .AND. MT.LE.849) I800=1
        GO TO 20
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
        IF(IER.NE.0) THEN
          IF(KEA.GT.0) THEN
            REWIND LEF
            GO TO 46
          ELSE
            GO TO 50
          END IF
        END IF
        CALL SKIPSC(LEF)
C* Select contributiong reactions
        IF( MT.EQ.  5                  .OR.
     &     (MT.GE. 11 .AND. MT.LE.199) .OR.
     &     (MT.GT.600 .AND. MT.LE.649) .OR.
     &     (MT.GT.800 .AND. MT.LE.849)) THEN
          LOOP=LOOP+1
          LST(LOOP)=MT
          IF(MT.GE. 50 .AND. MT.LE. 91) IINL=1
          IF(MT.GE.600 .AND. MT.LE.649) I600=1
          IF(MT.GE.800 .AND. MT.LE.849) I800=1
        END IF
        GO TO 44
C* Add reactions with wholy contained cross sections in MF13
   46   MF=13
        MT=0
        CALL FINDMT(LEF,ZA0,ZA,AW,L1,L2,N1,N2,MAT,MF,MT,IER)
        IF(IER.NE.0) GO TO 50
        CALL SKIPSC(LEF)
C* If cumulative gamma production present, set flag to exclude partials
          IF(MT.EQ.  4) MINL=1
          IF(MT.EQ.103) M600=1
          IF(MT.EQ.107) M800=1
C* Check for double counting of reactions
        DO I=1,LOOP
          IF(LST(I).EQ.MT) GO TO 46
        END DO
        LOOP=LOOP+1
        LST(LOOP)=MT
        GO TO 46
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
      DO I=1,LOOP
        IL=IL+1
        LST(IL)=LST(I)
        IF( LST(I).EQ.  4 .AND. IINL.EQ.1) IL=IL-1
        IF( LST(I).EQ.103 .AND. I600.EQ.1) IL=IL-1
        IF( LST(I).EQ.107 .AND. I800.EQ.1) IL=IL-1
        IF((LST(I).GT. 50 .AND. LST(I).LE. 99) .AND. MINL.EQ.1) IL=IL-1
        IF((LST(I).GT.600 .AND. LST(I).LE.649) .AND. M600.EQ.1) IL=IL-1
        IF((LST(I).GT.800 .AND. LST(I).LE.849) .AND. M800.EQ.1) IL=IL-1
      END DO
      LOOP=IL
      IL=1
      ML=0
      MT=LST(IL)
C...
      PRINT *,'Contributing reactions:',(lst(j),j=1,loop)
C...
C*
C* Retrieve the double differential cross section energy distribution
   60 CALL DXSEN1(LEF,ZA0,ZAP,MF0,MT,KEA,EIN,PAR,EPS,ENR,DXS,UXS
     1           ,RWO(LX),NE,MEN,KRW,IER)
c...
c...  print *,' Add contribution from MT/NE',mt,ne
c...
      IF( IER.NE.0) THEN
        IF(LTT.GT.0) THEN
        IF(IER.LT.MXE) THEN
        WRITE(LTT,901) ' DXSEND WARNING - Error encountered   : '
     &                 ,MSG(IER)
        ELSE
        WRITE(LTT,903) ' DXSEND WARNING - Error condition flag: ',IER
        END IF
        WRITE(LTT,903) '            Failed reconstruction for MF',MF0
        WRITE(LTT,903) '                                      MT',MT
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
      DO I=1,NE1
        RWO(LX-1+I)=RWO(NE1+I)
      END DO
C* Generate the union grid
      LUE= 1+NE1
      LUX=LX+NE1
      KX =MRW-LUX
      CALL UNIGRD(NEN,ENR,NE1,RWO,NE2,RWO(LUE),KX)
C* Interpolate current distribution to the union grid
      CALL FITGRD(NEN,ENR,DXS,NE2,RWO(LUE),RWO(LUX))
      NEN=NE2
      DO I=1,NEN
        ENR(I)=RWO(LUE-1+I)
        DXS(I)=RWO(LUX-1+I)
      END DO
C* Interpolate saved distribution to the union grid
      CALL FITGRD(NE1,RWO,RWO(LX),NE2,RWO(LUE),RWO(LUX))
C* Add the current to the saved distribution
      DO I=1,NEN
        DXS(I)=DXS(I)+RWO(LUX-1+I)
      END DO
C* Save the summed distribution 
   70 LX=1+NEN*2
      KRW=MRW-LX
      NE1=NEN
      DO I=1,NEN
        RWO(I    )=ENR(I)
        RWO(I+NEN)=DXS(I)
      END DO
C* Select next reaction
   74 IF(IL.LT.LOOP) THEN
        IL=IL+1
        MT=LST(IL)
        GO TO 60
      END IF
C*
C* All processing completed
   90 CONTINUE
      IF(ML.GT.0 .AND. LOOP.GT.0) WRITE(LTT,904) ML,LOOP
c...
C...      print *,'MT0,nen',mt0,nen
C...      print *,enr(1),dxs(1)
C...      print *,enr(nen),dxs(nen)
c...
      RETURN
C*
  901 FORMAT(2A40)
  903 FORMAT(A40,I6)
  904 FORMAT(' DXSEND WARNING - failed reconstructing'
     1       ,I3,' out of',I3,' reactions')
      END
      SUBROUTINE DXSEN1(LEF,ZA0,ZAP0,MF0,MT0,KEA,EIN,PAR,EPS
     1                 ,ENR,DXS,UXS,RWO,NEN,MEN,MRW,IER)
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
C-V  03/01 - Add MF5 Maxwellian fission spectrum representation.
C-V        - Add LO=1, LF=2 capability for MF12 processing.
C-V  03/11 - Guard cross section retrieval against overflow.
C-V  03/12 - Implement MF6 Law2 LANG>10 processing.
C-V  06/01 - Add evaporation spectrum to options in MF 5.
C-V  06/02 - Allow multiple section representation in MF 5.
C-V        - Add UXS array to contain uncertainty of DXS
C-V  06/03 Implement retrieval oc cross sections at fixed angle
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
C-D           ZAP0 >= 0 particle multiplicities to be calculated,
C-D                <  0 only cross sections are required.
C-D  MF0  - Requested file number (ENDF conventions).
C-D  MT0  - Requested reaction number. Broadly this follows the ENDF
C-D         conventions. The main exceptions are:
C-D           MT0= MT + 40000 to request cross sections at fixed
C-D                           outgoing particle angle
C-D  KEA  - Control flag to select retrieval:
C-D           0  cross sections,
C-D           1  angular distributions,
C-D           2  energy spectra.
C-D  EIN  - Incident particle energy (eV).
C-D  PAR  - Fixed parameter when requesting differential data:
C-D         KEA=0, MF0=10, PAR is the requested metastable state
C-D                (0=ground, 1=first metastable, etc.).
C-D                MT0=MT+40000, PAR is the requested scattering
C-D                angle (degrees).
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
C-D  UXS  - Absolute uncertainty of the function vector.
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
C-D          9  Array limit MRW exceeded.
C-D         11  Multiple interpolation ranges and/or law other
C-D             than lin-lin encountered.
C-D         12  Correlated energy-angle distributions not in Law-7
C-D             representation.
C-D         13  Processing not coded for specified reaction.
C-D         14  Requested particle not found.
C-D
C-Extern.: SKIPSC,FINDMT,RDTAB1,RDTAB2,RDLIST,FINT2D,YTGEOU,FNGAUS,
C-E        FYTG2D,UNIGRD,FITGRD,VECLIN,FINEND
C-
c...
      double precision  sre,sim,are,aim,aa
c...
      PARAMETER   (MIW=100, MXLEG=100)
      DIMENSION    IWO(MIW)
      DIMENSION    RWO(MRW),ENR(MEN),DXS(MEN),UXS(MEN)
      DIMENSION    NBT(100),INR(100)
      DIMENSION    AMU(100),PMU(100)
      DIMENSION    PLEG(MXLEG)
C*
      DATA PI/3.14159265/
C...
c...  PRINT *,'DXSEN1:ZA0,ZAP0,MF0,MT0,KEA,EIN,PAR'
c... 1        ,nint(ZA0),nint(ZAP0),MF0,MT0,KEA,EIN,PAR
C...
C*
C* Check the requested type of output
      IER= 0
      MF = MF0
      MT = MT0
      NEN= 0
      DEG=-2
      AIN=-2
      EOU=-2
      SAN= 1/(2*PI)
      MST= 1+NINT(PAR)
      AWP=-1
      ZAP= ZAP0
      IZAP0=NINT(ZAP0)
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
      ELSE
C* Case: Cross section at fixed angle
        IF(MT0/10000.EQ.4) THEN
          MT=MT0-40000
          DEG=PAR
          IF(DEG.GE.0) THEN
            AIN=COS(DEG*PI/180)
          ELSE
            SAN= 1
          END IF
        END IF
      END IF
C*
      REWIND LEF
      CALL SKIPSC(LEF)
C*
C* Define particle multiplicities
      YL=1
      IF(IZAP0.LT.0) GO TO 30
      CALL YLDPOU(YL,MT0,IZAP0)
C* Special treatment for nu-bar and neutrons from fission
      IF(IZAP0.EQ.1) THEN
        IF(MT0.EQ.19) GO TO 900
        IF(MT0.EQ.18 .OR.
     &    (MF0.EQ.1 .AND. MT0.EQ.452 .OR. MT0.EQ.456)) THEN
          MF =1
          IF(MT0.EQ.18) THEN
            MT =452
          ELSE
            MT =MT0
          END IF
          CALL FINDMT(LEF,ZA0,ZA,AWR,L1,LNU,N1,N2,MAT,MF,MT,IER)
c...
c...      print *,'found mat/mf/mt/ier',za0,mf,mt,ier
c...
          IF(IER.NE.0) THEN
            REWIND LEF
            IF(MT0.EQ.18) THEN
              MT=456
            ELSE
              IF(MT.EQ.456) THEN
                MT=452
                PRINT *,' WARNING - MT 456 Substituted with 452'
              ELSE IF(MT.EQ.452) THEN
                PRINT *,' WARNING - MT 452 Substituted with 456'
                MT=456
              ELSE
                GO TO 900
              END IF
            END IF
            CALL FINDMT(LEF,ZA0,ZA,AWR,L1,LNU,N1,N2,MAT,MF,MT,IER)
          END IF
          IF(IER.NE.0) GO TO 900
          IF     (LNU.EQ.1) THEN
            CALL RDLIST(LEF,C1,C2,L1,L2,NC,N2,RWO,MRW,IER)
            NN=NC-1
            IF(MF0.NE.1) THEN
C* Case: Neutron yield at incident energy from polynomial representation
              YL=POLYNX(EIN,RWO,NN)
C...
C...          print *,'nu-bar from LNU=1 is',yl
C...
            ELSE
C* Case: Assemble nu-bar from polynomial representation
              EUP=20.E+6
              EE = 1.E-5
              NEN=1
              DO WHILE(EE.LT.EUP)
                ENR(I)=EE
                DXS(I)=POLYNX(EE,RWO,NN)
                EE=EE*10
                NEN=NEN+1
              END DO
              ENR(NEN)=EUP
              DXS(NEN)=POLYNX(EUP,RWO,NN)
              GO TO 900
            END IF
          ELSE IF(LNU.EQ.2) THEN
            NX=MRW/2
            LX=NX+1
            CALL RDTAB1(LEF,C1,C2,L1,L2,NR,NP,NBT,INR
     1                 ,RWO,RWO(LX),NX,IER)
            IF(IER.NE.0) GO TO 900
            IF(MF0.NE.1) THEN
C* Case: Neutron yield at incident energy from pointwise representation
              YL=FINTXS(EIN,RWO,RWO(LX),NP,INR(1),IER)
c...
c...          print *,'nu-bar from LNU=2 is',yl
c...
            ELSE
C* Case: Assemble nu-bar distribution from pointwise representation
              NEN=NP
              DO I=1,NEN
                ENR(I)=RWO(I)
                DXS(I)=RWO(LX-1+I)
              END DO
              GO TO 900
            END IF
          ELSE
            PRINT *,'DXSEN1 ERROR - Invalid LNU=',LNU,' for NuBar'
            STOP    'DXSEN1 ERROR - Invalid LNU for NuBar'
          END IF
        END IF
      END IF
C*
C* Retrieve the cross section on MF3
   30 IF     (MF0.EQ.1) THEN
        MF=1
      ELSE IF(MF0.GE.3 .AND. MF0.LE.6 ) THEN
        MF=3
      ELSE IF(MF0.EQ.10)                THEN
        MF=10
      ELSE IF(MF0.EQ.23 .OR. MF0.EQ.26) THEN
        MF=23
      ELSE
        PRINT *,'DXSEN1 ERROR - Illegal MF output request',MF0
        STOP 'DXSEN1 ERROR - Illegal MF output request'
      END IF
      NX =MRW/4
      LE=1
      LX=LE+NX
      LU=LX+NX
      LBL=LU+NX
C* Suppress searching for covariance data unless MF0=3
      IF(MF0.EQ.3) THEN
        MTJ= MT0
      ELSE
        MTJ=-MT0
      END IF
      CALL GETSTD(LEF,NX,ZA0,MF,MTJ,MST,QM,QI
     &           ,NP,RWO(LE),RWO(LX),RWO(LU),RWO(LBL),NX)
C...
C...  print *,'Found MAT,MF,MT,NP,Ei,ZAp',nint(za0),MF,MTJ,NP,ein,izap0
C...
      IF(NP.EQ.0) THEN
        IER=1
        IF(KEA.EQ.2 .AND. IZAP0.EQ.0) THEN
C* If no data found and photon spectrum requested, try MF 13
          REWIND LEF
          MF =13
          MT =MT0
          CALL GETSTD(LEF,NX,ZA0,MF,MT,MST,QM,QI
     &               ,NP,RWO(LE),RWO(LX),RWO(LU),RWO(LBL),NX)
          IF(NP.GT.0) GO TO 120
        END IF
        GO TO 900
      END IF
      IF(EIN.GT.0) THEN
        IF(EIN.LT.RWO(LE) .OR. EIN.GT.RWO(LE-1+NP)) THEN
C* Case: Required point is below thershold or above last point
          NEN=0
          GO TO 900
        END IF
      END IF
C* Case: Cross section is required on output - finish processing
      IF(KEA.EQ.0) THEN
        NEN=NP
        IF(NEN.GT.MEN) THEN
          PRINT *,' WARNING - Arry limit MEN exceeded, output first',MEN
          NEN=MEN
        END IF
        YY=1
        IF(YL.GT.0) YY=YL
        DO I=1,NEN
          ENR(I)=RWO(LE-1+I)
          DXS(I)=RWO(LX-1+I)*YY
          UXS(I)=RWO(LU-1+I)*YY
        END DO
c...
c...     print *,'   Done mf/mt/izap0/yl',mf,mt,zap0,izap0,yl
c...
        IF(MT0.LT.1000 .AND. (IZAP0.LT.0 .OR. YL.GT.0)) GO TO 900
C* Find particle yields when these are not implicit in MT
        IF(IZAP0.EQ.0) THEN
          print *,'WARNING - photon multiplicities not coded (only MF6)'
        END IF
        REWIND LEF
        MFJ=6
        MTJ=MT
        CALL FINDMT(LEF,ZA0,ZA,AWR,L1,L2,N1,N2,MAT,MFJ,MTJ,IER)
C* Error trapping when no data found in the ENDF file
        IF(IER.NE.0) THEN
          PRINT *,'WARNING - No MF 6 data for MT',MT
          GO TO 900
        END IF
        LCT=L2
        NK =N1
        JNK=0
C* Split the work array RWO to operate on function and argument
C* LE  - First argument
C* LX  - First function
C* LXE - Second argument
C* LXX - Second function
        KX =MRW/2
        LE =1
        LX =KX+1
        LD =KX/2
        LXE=LE+LD
        LXX=LX+LD
C* Loop over particles
   31   JNK=JNK+1
C* Retrieve the particle yield
        CALL RDTAB1(LEF,ZAP,AWP,LIP,LAW,NR,NP,NBT,INR
     &             ,RWO(LE),RWO(LX),KX,IER)
C* Check for matching particle - else skip section
        IF(NINT(ZAP).NE.NINT(ZAP0)) THEN
c...
c...        print *,'Skipping particle/mf/mt/law',nint(zap),mf,mt,law
c...
          IF(JNK.GE.NK) THEN
C* Particle not found - yield assumed zero
            NEN=0
            PRINT *,'WARNING - Zero yield for MT',MT0, ' particle',IZAP0
            GO TO 900
          END IF
C* Skip the subsection for this particle
          IF     (LAW.EQ.0 .OR. LAW.EQ.3 .OR. LAW.EQ.4) THEN
C*         No subsection for Law 0, 3, 4
          ELSE IF(LAW.EQ.2 .OR.LAW.EQ.5) THEN
C*         Skip subsection for Law 2
           CALL RDTAB2(LEF,C1,C2,L1,L2,NR,NE,NBT,INR,IER)
           MX=MRW-LXX
           DO IE=1,NE
             CALL RDLIST(LEF,C1,C2,L1,L2,N1,N2,RWO(LXX),MX,IER)
             IF(IER.NE.0) THEN
               STOP 'DXSEN1 ERROR - Skipping MF6 Law 2 data in RDLIST'
             END IF
           END DO
          ELSE IF(LAW.EQ.7) THEN
C*         Skip subsection for Law 7
            CALL RDTAB2(LEF,C1,C2,L1,L2,NR,NE,NBT,INR,IER)
            NM=NR+1
            DO IE=1,NE
              CALL RDTAB2(LEF,C1,EI2,L1,L2,NRM,NMU
     &                   ,NBT(NM),INR(NM),IER)
              NO=NM+NMU
              DO IM=1,NMU
                CALL RDTAB1(LEF,C1,AI2,L1,L2,NRP,NEP2
     &                     ,NBT(NO),INR(NO)
     &                     ,RWO(LE),RWO(LX),KX,IER)
              END DO
            END DO
          ELSE
C*           Unsupported Law - cannot skip the section for this particle
C*           Set IER to flag error and terminate
            PRINT *,'DXSEND ERROR - Skipping MF/MT/Law',MF,MT,LAW
            IER=12
            GO TO 900
          END IF
          GO TO 31
        END IF
C* Define union grid of cross sections and yields at LXE
        CALL UNIGRD(NEN,ENR,NP,RWO(LE),NE2,RWO(LXE),LD)
C* Interpolate cross sections on union grid to LXX
        CALL FITGRD(NEN,ENR,DXS,NE2,RWO(LXE),RWO(LXX))
C* Interpolate yields to union grid into DXS
        CALL FITGRD(NP,RWO(LE),RWO(LX),NE2,RWO(LXE),DXS)
C* Save energy union grid and multiply cross section by the yield
        NEN=NE2
        DO I=1,NEN
          ENR(I)=RWO(LXE-1+I)
          DXS(I)=DXS(I)*RWO(LXX-1+I)
        END DO
C* Process cross section at fixed angle for charged particles
        IF(LAW.EQ.5 .AND. DEG.GE.0) THEN
          CALL RDTAB2(LEF,SPI,C2,LIDP,L2,NR,NE,NBT,INR,IER)
          MX=MRW-LXX
          IF(MX.LE.0) STOP 'DXSEND ERROR - Array capacity exceeded'
C* Loop over incident particle energies
C...
C...      open(unit=81,file='SIGMT.cur',status='unknown')
C...      open(unit=82,file='SIGMA.cur',status='unknown')
C...      open(unit=83,file='SIGMB.cur',status='unknown')
          open(unit=84,file='SIGMC.cur',status='unknown')
C...      write(81,*) 'Endtab'
C...      write(82,*) 'Endtab interference'
C...      write(83,*) 'Endtab scattering'
          write(83,*) 'Endtab Coulomb'
C...
          DO IE=1,NE
C* Read the angular distribution
            CALL RDLIST(LEF,C1,EE,LTP,L2,NW,NL,RWO(LXX),MX,IER)
            IF(IER.NE.0) THEN
              print *,'ERROR reading LIST record IER',IER
              STOP 'DXSEN1 ERROR - Reading Law 5 data in RDLIST'
            END IF
C* Coulomb scattering contribution
C*          -- Projectile mass, charge and target charge
            CALL PROMAS(IZAP0,AM1)
            ZZI=IZAP0/1000
            ZZT=NINT(ZA)/1000
C*          -- define constants (Table 1, Appendix H, ENDF-102 manual)
            PCP=6.58211889E-16
            RAL=137.03599976
            AMUEV=9.31494013E8
            CC=299792458
            AA =AWR/AWP
            AK =2*AMUEV/(PCP*CC*1E14)**2
            AE =AMUEV/(2*RAL*RAL)
              AKA=SQRT(AK*AM1*EE)*AA/(AA+1)
              AET=ZZT*ZZI*SQRT(AE*AM1/EE)
              IF(LIDP.EQ.0) THEN
                SIGC=AET*AET/(AKA*AKA*(1-AIN)**2)
              ELSE
                A0 =LOG((1+AIN)/(1-AIN))
                A1 =COS(AET*A0)
                A2 =A1* (-1)**NINT(2*SPI) /(2*SPI+1)
                A3 =A2+ (1+AIN*AIN)/(1-AIN*AIN)
                SIGC=A3* 2 * (AET*AET/(AKA*AKA*(1-AIN*AIN)))
              END IF
            RWO(LE-1+IE)=EE
            RWO(LX-1+IE)=SIGC
            IF     (LTP.EQ.1) THEN
C* Case: Nuclear amplitude expansion (Eq.6.11,6.12 of ENDF-102 manual)
              IF(LIDP.EQ.0) THEN
C*              -- Nuclear interference term
                ATH=AET*ALOG((1-AIN)/2)
                ARE=COS(ATH)
                AIM=SIN(ATH)
C*              -- Evaluate Legendre polynomials
                NL2=NL*2
                IF(NL2.GE.MXLEG)
     &            STOP 'DXSEND ERROR - MXLEG limit exceeded'
                CALL PLNLEG(AIN,PLEG,NL2)
C*              -- Prepare legendre coefficients
                NL1=NL+1
                SRE=0
                SIM=0
                DO L=1,NL1
C*                -- Real part of interference term
                  SRE=SRE+dble(RWO(LXX+2*NL+2*L-1)*PLEG(L))*(2*L-1)/2
C*                -- Imaginary part of interference term
                  SIM=SIM+dble(RWO(LXX+2*NL+2*L  )*PLEG(L))*(2*L-1)/2
                END DO
C*              -- Nuclear scattering term (order 2*NL)
                NL21=2*NL+1
                AAS =0
                DO L=1,NL21
                  AAS=AAS+dble(RWO(LXX-1+L)*PLEG(L))*(2*L-1)/2
                END DO
c...
c...            print *,'deg,ain',deg,ain
c...            print *,'leg',(pleg(l),l=1,nl21)
c...            print *,'cob',(rwo(lxx-1+l),l=1,nl21)
c...            if(ie.eq.2) stop
c...
C*              -- Add the terms
                AAI=(ARE*SRE-AIM*SIM)*2*dble(AET)/(1-dble(AIN))
                RWO(LX-1+IE)=RWO(LX-1+IE)-AAI+AAS
C...
C...            write(81,'(f11.2,1p,e11.4)') EE/1000,RWO(LX-1+IE)*1000
C...            write(82,'(f11.2,1p,e11.4)') EE/1000,-AAI*1000
C...            write(83,'(f11.2,1p,e11.4)') EE/1000,AAS*1000
                write(84,'(f11.2,1p,e11.4)') EE/1000,SIGC*1000
C...            PRINT *,EE/1000,RWO(LX-1+IE),AA,ARE,SRE,AIM,SIM
C...            
              ELSE
                PRINT *,'Programming incomplete for LTP/LIDP',LTP,LIDP
                STOP 'DXSEND ERROR - MF6 LAW 5 unsupported LTP/LIDP'
              END IF
            ELSE IF(LTP.GE.12 .AND. LTP.LE.15) THEN
C* Case: Tabulated distribution of Pni
              NC=NL
              IF(NW+NC.GT.LD) STOP 'DXSEND ERROR - MRW limit exceeded'
              LX1=LXX+NW
C*            -- Separate distribution (LX1) from the cosine (LXX)
              DO J=1,NC
                RWO(LX1-1+J)=RWO(LXX+1+(J-1)*2)
                RWO(LXX-1+J)=RWO(LXX  +(J-1)*2)
              END DO
C*            -- Linearise with EXS accuracy, if necessary
              INC=LTP-10
              IF(INC.GT.2) THEN
                NE1=NE+1
                INR(NE1)=INC
                NBT(NE1)=NC
                KX=MIN(NW,MX-LX1)
                EXS=0.01
                CALL VECLIN(NRP,NF,NBT(NE1),INR(NE1)
     &                     ,RWO(LXX),RWO(LX1),KX,EXS)
                INC=2
              END IF
C* Collect distributions at fixed angle for the current energy
              INX=2
              RWO(LX-1+IE)=RWO(LX-1+IE)
     &                    +FINTXS(AIN,RWO(LXX),RWO(LX1),NC,INC,IER1)
     &                    *FINTXS(EE,ENR,DXS,NEN,INX,IER2)
            ELSE
              PRINT *,'Reading Law 5 - unsupported LTP',LTP
              STOP 'DXSEND ERROR - MF6 LAW 5 unsupported LTP'
            END IF
          END DO
C* Define union grid of cross sections and angular cross sect. at LXE
          CALL UNIGRD(NEN,ENR,NE,RWO(LE),NE2,RWO(LXE),LD)
C* Interpolate angular cross section to union grid at LXX
          CALL FITGRD(NE,RWO(LE),RWO(LX),NE2,RWO(LXE),RWO(LXX))
C* Save cross sections on energy union grid
          NEN=NE2
          DO I=1,NEN
            ENR(I)=RWO(LXE-1+I)
            DXS(I)=RWO(LXX-1+I)
          END DO
        END IF
        GO TO 900
      END IF
C* Case: Proceed with the retrieval of differential data
      ETOP=RWO(NP)
      INR(1)=2
      XS=FINTXS(EIN,RWO(LE),RWO(LX),NP,INR(1),IER)
C...
C...      print *,'     Cross section=',xs,ein,ier
C...
      IF(IER.NE.0 .OR. XS .LE.0) THEN
        NEN=0
        GO TO 900
      END IF
      REWIND LEF
C*
C* Find the energy/angle distribution data
   34 MF =0
      MT =MT0
      CALL FINDMT(LEF,ZA0,ZA,AWR,L1,L2,N1,N2,MAT,MF,MT,IER)
c...
c...      print *,'find1 mf,mt,ier',mat,mf,mt,ier
c...
C* Error trapping when no data found in the ENDF file
      IF(IER.NE.0) THEN
C* No MF4/5/6 possible only for discrete level (two-body isotr.) react.
        IF     (IZAP0.EQ.   1) THEN
          IF(MT0.EQ.2 .OR. (MT0.GT.50 .AND. MT0.LT.91) ) GO TO 40
        ELSE IF(IZAP0.EQ.1001) THEN
          IF(MT0.GE.600 .AND. MT0.LT.649) GO TO 40
        ELSE IF(IZAP0.EQ.2004) THEN
          IF(MT0.GE.800 .AND. MT0.LT.849) GO TO 40
        END IF
        IER=15
        PRINT *,'WARNING - No differential data for MT',MT0
        GO TO 900
      END IF
C* Gamma production only from MF 6,12,13,14,15
      IF(IZAP0.EQ.   0) THEN
        IF(MF.EQ.12 .OR. MF.EQ.13) REWIND LEF
        IF(MF.EQ. 4 .OR. MF.EQ. 5 .OR. MF.EQ.12 .OR. MF.EQ.13) THEN
          IF(MF.LT.12) MF =12
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
   36     CALL SKIPSC(LEF)
          JF=MF
          JT=MT
          CALL FINDMT(LEF,ZA0,ZA,AWR,L1,L2,N1,N2,MAT,JF,JT,IER)
          IF(IER.NE.0) THEN
            IF(MF.EQ.12) THEN
              REWIND LEF
              MF=13
              GO TO 36
            ELSE
              GO TO 140
            END IF
          END IF
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
      DO I=1,NE1
        ENR(I)=-1+(I-1)*2.0/(NE1-1)
        DXS(I)=0.5
      END DO
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
C* Assume two-body scattering for elastic & inelastic scattering
C* and other discrete-level reactions for INCIDENT NEUTRONS
      IF(MT0.EQ.2 .OR. (MT0.GT.50 .AND. MT0.LT.91) ) THEN
        AWP=1
      ELSE IF(MT0.GE.600 .AND. MT0.LT.649) THEN
        AWP=1
      ELSE IF(MT0.GE.800 .AND. MT0.LT.849) THEN
        AWP=4
      ELSE
        AWP=-1
      END IF
      IF(LTT.EQ.0) GO TO 45
      IF(LTT.NE.2) THEN
        PRINT *,'WARNING - Can not process MF/MT/LTT',MF,MT,LTT
        PRINT *,'          Distributions not pointwise'
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
      NM =NR+1
      NE1=0
      EI1=0
      DO IE=1,NE
C* For each incident energy read the angular distribution
        CALL RDTAB1(LEF,TEMP,EI2,LT,L2,NRP,NEP1,NBT(NM),INR(NM)
     1             ,RWO(LXE),RWO(LXX),KX,IER)
        IF(IER.NE.0) THEN
          PRINT *,'ERROR READING MF/MT/IER',MF,MT,IER
          STOP 'DXSEN1 ERROR reading ang.distributions'
        END IF
C...    IF(NRP.GT.1)
C... 1   PRINT *,' WARNING - Multiple A interp.ranges for MF 4, MT',MT
C* Lin-interpolate angular distributions over incident energies
        INE=2
        CALL FINT2D(EIN,EI1,NE1 ,ENR     ,DXS     ,INR(NM)
     1                 ,EI2,NEP1,RWO(LXE),RWO(LXX),INE,KX)
      END DO
C*
C* Angular distribution at incident energy Ein processed
C* Calculate  integral SS over distribution DXS at cosines ENR
   45 EA=ENR(1)
      EB=ENR(NE1)
      INT=INR(1)
      CALL YTGEOU(SS,EA,EB,NE1,ENR,DXS,INT)
C*
C* Process energy distributions for two-body reactions (AWP>0)
C* Definitions:
C*  XCM - cosine os scattering angle in CM system
C*  XLB - cosine os scattering angle in Lab system
C*  AWR - mass ratio of target and projectile (=A)
C*  AWP - mass ratio of ejectile and projectile (=A-dash)
C* Kinematics equations for 2-body problem form ENDF-102 Appendix E
C* Equation (E.3)
      IF(AWP.GT.0) THEN
        BET=(AWR*(AWR+1-AWP)/AWP)*( 1+(1+AWR)*QI/(AWR*EIN) )
        BET=SQRT(BET)
        DO I=1,NE1
          IF(LCT.EQ.2) THEN
C*          CM co-ordinate system conversion to Lab
            XCM=ENR(I)
C*          Lab cosine of scattering: equation (E.11)
            SBT= BET*BET + 1 + 2*XCM*BET
            QBT= SQRT( SBT )
            XLB=(1+BET*XCM)/QBT
C*          Jacobian of the transformation dXCM/dXLB (derivative of E.11)
            DCM=(SBT*QBT)/(BET*BET*(BET+XCM))
          ELSE
C*          Lab co-ordinate system
            XLB=ENR(I)
C*          CM cosine of scattering: inverse equation (E.11)
C*          (the larger root of the quadratic equation is taken)
          
            XCM=(XLB*XLB-1 + XLB*SQRT(XLB*XLB+BET*BET-1) )/BET
            SBT= BET*BET + 1 + 2*XCM*BET
            DCM=1
          END IF
          EO =EIN*SBT*AWP/((AWR+1)*(AWR+1))
C*        Outgoing particle energy: equation (E.10)
          ENR(I)=XLB
          DXS(I)=DXS(I)*DCM
          RWO(LXE-1+I)=EO
        END DO
      END IF
c...
c...      print *,'spectrum at Ei',EIN,' for MF4/MT',MT
c...      do i=1,ne1
c...        print *,'cos,Eou,dst',enr(i),rwo(lxe-1+i),dxs(i)
c...      end do
c...
      IF(KEA.EQ.1) THEN
C* Case: Angular distribution (no further processing required)
        YL=YL/SS
        IF(EOU.LT.0) GO TO 800
      END IF
      INA=2
      IF(DEG.GE.0) THEN
C* Case: Specified outgoing particle cosine
        XLB=AIN
        YA =FINTXS(XLB,ENR,DXS,NE1,INA,IER)/SS
        YL =YL*YA
        IF(AWP.GT.0) THEN
C*        Interpolate precomputed Eou corresponding to Xlb
          IER=0
          EO =FINTXS(XLB,ENR,RWO(LXE),NE1,INA,IER)
          NE1=21
          CALL FNGAUS(EO ,NE1,ENR,DXS,EPS)
          GO TO 800
        END IF
      ELSE
C* Case: Angle-integrated energy distribution
C*       Calculate cosine*distribution
        DO I=1,NE1
          RWO(LXX-1+I)=ENR(I)*DXS(I)
        END DO
        EA=ENR(1)
        EB=ENR(NE1)
C*        Integrate cosine*distribution
        CALL YTGEOU(SPS,EA,EB,NE1,ENR,RWO(LXX),INA)
C*        Integrate distribution
        CALL YTGEOU(SDS,EA,EB,NE1,ENR,DXS,INA)
C*        Average cosine of the outgoing particle in the Lab system
        XLB=SPS/SDS
      END IF
      IF(AWP.LT.0) GO TO 50
C* Interpolate precomputed Eou corresponding to XLB
      IER=0
      EO =FINTXS(XLB,ENR,RWO(LXE),NE1,INA,IER)
      NE1=21
      CALL FNGAUS(EO ,NE1,ENR,DXS,EPS)
      GO TO 800
C*
C* Process energy distribution data in MF 5
   50 IF(MF.EQ.4) THEN
        MF =5
        MT =MT0
        CALL FINDMT(LEF,ZA0,ZA,AW,L1,L2,NK,N2,MAT,MF,MT,IER)
C...
c...    print *,'Found ZA0,ZA,AW,NK,MAT,MF,MT,IER'
c...    print *,       ZA0,ZA,AW,NK,MAT,MF,MT,IER
C...
        IF(IER.NE.0) THEN
          PRINT *,'DXSEN1 WARNING - No MF 5 for MT',MT
          IER=14
          GO TO 900
        END IF
      END IF
      IF(MF.EQ.5 .AND. NK.GT.1) THEN
C...    No multiple representations are allowed
        PRINT *,'WARNING - Mult.representation for MF5 MT',MT0
C...    IER=13
C...    GO TO 900
      END IF
      NEN=0
C* Subdivide the work array into half
C* LE - argument address
C* LX - Function address
C* NX - Max. number of entries
      NX=MRW/2
      LE=1
      LX=LE+NX
      PK =1
      JK =0
   51 JK =JK+1
C* Read weights for the distributions (MF5) or multiplicities(MF26)
      CALL RDTAB1(LEF,C1,C2,L1,LF,NR,NP,NBT,INR
     1           ,RWO(LE),RWO(LX),NX,IER)
      IF     (MF.EQ. 5) THEN
C* Interpret Pk(e) - equal 1 for single region representation
        PK=FINTXS(EIN,RWO(LE),RWO(LX),NP,INR(1),IER)
      ELSE IF(MF.EQ.26) THEN
C* Interpret particle multiplicity
        ZAP=C1
        IF(NINT(ZAP).NE.NINT(ZAP0)) THEN
C* If not the right particle skip to next one
          CALL RDTAB2(LEF,C1,C2,L1,L2,NR,NE,NBT,INR,IER)
          DO IE=1,NE
            CALL RDLIST(LEF,C1,C2,L1,L2,N1,N2,RWO(LX),NX,IER)
          END DO
          IF(JK.LT.NK) GO TO 51
          PRINT *,' WARNING - No matching particle ZAP',ZAP0
          STOP 'DXSEN1 - ZAP not matched'
        END IF
        YL=FINTXS(EIN,RWO(LE),RWO(LX),NP,INR(1),IER)
      END IF
C* Process the distributions
      IF(LF.EQ.1) THEN
C* Pointwise representation (Law 1)
        KX =NX /2
        LXE=LE +KX
        LXX=LX +KX
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
C* Linearise to tolerance EXS, if necessary
          EXS=0.01
          IF(NRP.GT.1) CALL VECLIN(NRP,NF,NBT(NM),INR(NM)
     1                            ,RWO(LXE),RWO(LXX),KX,EXS)
          INE=2
        ELSE
          MX=MRW-LXE
          CALL RDLIST(LEF,C1,EI2,L1,L2,N1,NF,RWO(LXE),MX,IER)
          IF(NF.GT.LXX-LXE) STOP 'DXSEN1 ERROR - Array limit'
          INE=LEP
          IF(INE.GT.2) THEN
            PRINT *,'WARNING - Non-linear interp. for MF/MT',MF,MT
            INE=2
          END IF
          DO I=1,NF
            RWO(LXE-1+I)=RWO(LXE-2+2*I)
            RWO(LXX-1+I)=RWO(LXE-1+2*I)
          END DO
        END IF
        IF(IER.NE.0) THEN
          PRINT *,'ERROR READING MF/MT/IER',MF,MT,IER
          STOP 'DXSEN1 ERROR reading energy.distrib.'
        END IF
        IF(KEA.EQ.2) THEN
Case: Interpolate outgoing particle energy distributions
          INT=INR(1)
          CALL FINT2D(EIN,EI1,NE1 ,RWO(LE) ,RWO(LX) ,INT
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
C* Add contribution for one representation
        CALL UNIGRD(NEN,ENR,NE1,RWO(LE),NE2,RWO(LXE),KX)
C* Interpolate previous distribution to the union grid
        CALL FITGRD(NEN,ENR,DXS,NE2,RWO(LXE),RWO(LXX))
        NEN=NE2
        DO I=1,NEN
          ENR(I)=RWO(LXE-1+I)
          DXS(I)=RWO(LXX-1+I)
        END DO
C* Add current distribution on the same grid
        CALL FITGRD(NE1,RWO(LE),RWO(LX),NE2,RWO(LXE),RWO(LXX))
        DO I=1,NEN
          DXS(I)=DXS(I)+RWO(LXX-1+I)*PK
        END DO
        IF(JK.LT.NK) GO TO 51
        NE1=NEN
      ELSE IF(LF.EQ.7 .OR. LF.EQ.9) THEN
C* Energy dependent Maxwellian fission spectrum (Law 7) OR
C* Evaporation spectrum (Law 9)
        NX=MRW/2
        LX=NX+1
        UPEN=C1
        IF(EIN-UPEN.LE.0) THEN
          NEN=0
          GO TO 900
        END IF
C* Read and interpolate Maxwellian temperature parameter Theta
        CALL RDTAB1(LEF,C1,C2,L1,L2,NR,NP,NBT,INR
     1             ,RWO,RWO(LX),NX,IER)
        INE=INR(1)
        IF(NR.GT.1) THEN
          PRINT *,'WARNING - Multiple interp. ranges in MF5 MT',MT0
        END IF
        IF(INE.GT.2) THEN
          PRINT *,'WARNING - Non-linear interpol.law in MF5 MT',MT0
        END IF
        THETA=FINTXS(EIN,RWO(1),RWO(LX),NP,INE,IER)
        IF(KEA.EQ.2) THEN
Case: Generate outgoing particle energy distributions
          NE1=101
C*         Limit the table to the upper energy on the ENDF file
          EE =MIN(ETOP,EIN-UPEN)
          DE =EE/(NE1-1)
          DO I=1,NE1
            EE =(I-1)*DE
            ENR(I)=EE
            DXS(I)=SPMAXW(EE,EIN,UPEN,THETA)
          END DO
        ELSE
C* Case: Calculate values at the given outgoing particle energy
          IF(LF.EQ.7) THEN
C*            Maxwellian fission spectrum
            YL=YL*SPMAXW(EOU,EIN,UPEN,THETA)
          ELSE
C*            Evaporation spectrum
            YL=YL*SPEVAP(EOU,EIN,UPEN,THETA)
          END IF
          GO TO 800
        END IF
      ELSE IF(LF.EQ.9) THEN
C* Evaporation spectrum (Law 9)
        NX=MRW/2
        LX=NX+1
        UPEN=C1
        IF(EIN-UPEN.LE.0) THEN
          NEN=0
          GO TO 900
        END IF
C* Read and interpolate the temperature parameter Theta
        CALL RDTAB1(LEF,C1,C2,L1,L2,NR,NP,NBT,INR
     1             ,RWO,RWO(LX),NX,IER)
        INE=INR(1)
        IF(NR.GT.1) THEN
          PRINT *,'WARNING - Multiple interp. ranges in MF5 MT',MT0
        END IF
        IF(INE.GT.2) THEN
          PRINT *,'WARNING - Non-linear interpol.law in MF5 MT',MT0
        END IF
        THETA=FINTXS(EIN,RWO(1),RWO(LX),NP,INE,IER)
        IF(KEA.EQ.2) THEN
Case: Generate outgoing particle energy distributions
          NE1=101
C*         Limit the table to the upper energy on the ENDF file
          EE =MIN(ETOP,EIN-UPEN)
          DE =EE/(NE1-1)
          DO 56 I=1,NE1
          EE =(I-1)*DE
          ENR(I)=EE
          DXS(I)=SPMAXW(EE,EIN,UPEN,THETA)
   56     CONTINUE
        ELSE
Case: Calculate values at the given outgoing particle energy
          YL=YL*SPMAXW(EOU,EIN,UPEN,THETA)
          GO TO 800
        END IF

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
        IER=13
        GO TO 900
      END IF
      GO TO 800
C*
C* Process coupled energy/angle distributions MF6 of selected particle
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
C* Retrieve the particle yield
      CALL RDTAB1(LEF,ZAP,AWP,LIP,LAW,NR,NP,NBT,INR
     1           ,RWO,RWO(LX),KX,IER)
C* Check for matching particle - else skip section
      IF(NINT(ZAP).NE.NINT(ZAP0)) THEN
c...
c...    print *,'Skipping particle/mf/mt/law',nint(zap),mf,mt,law
c...
        IF(JNK.GE.NK) THEN
          PRINT *,'DXSEN1 WARNING - Particle not found',NINT(ZAP0)
          IER=14
          GO TO 900
        END IF
C* Skip the subsection for this particle
        IF     (LAW.EQ.0) THEN
C*       No subsection for Law 0
        ELSE IF(LAW.EQ.2) THEN
C*        Skip subsection for Law 2
          CALL RDTAB2(LEF,C1,C2,L1,L2,NR,NE,NBT,INR,IER)
          MX=MRW-LXX
          DO IE=1,NE
            CALL RDLIST(LEF,C1,C2,L1,L2,N1,N2,RWO(LXX),MX,IER)
            IF(IER.NE.0) THEN
              STOP 'DXSEN1 ERROR - Skipping MF6 Law 2 data in RDLIST'
            END IF
          END DO
        ELSE IF(LAW.EQ.3) THEN
C*        No subsection for Law 3
        ELSE IF(LAW.EQ.4) THEN
C*        No subsection for Law 4
        ELSE IF(LAW.EQ.7) THEN
C*        Skip subsection for Law 7
          CALL RDTAB2(LEF,C1,C2,L1,L2,NR,NE,NBT,INR,IER)
          NM=NR+1
          DO IE=1,NE
            CALL RDTAB2(LEF,C1,EI2,L1,L2,NRM,NMU,NBT(NM),INR(NM),IER)
            NO=NM+NMU
            LXE=1
            LXX=LX
            DO IM=1,NMU
              CALL RDTAB1(LEF,C1,AI2,L1,L2,NRP,NEP2,NBT(NO),INR(NO)
     1                   ,RWO(LXE),RWO(LXX),KX,IER)
            END DO
          END DO
        ELSE
C*        Unsupported Law - cannot skip the section for this particle
C*        Set IER to flag error and terminate
          PRINT *,'DXSEND ERROR - Skipping MF/MT/Law',MF,MT,LAW
          IER=12
          GO TO 900
        END IF
        GO TO 61
      END IF
C* Particle found - extract tye yield
      CALL VECLIN(NR,NP,NBT,INR,RWO,RWO(LX),KX,EPS)
      YL6=FINTXS(EIN,RWO,RWO(LX),NP,INR(1),IER)
      IF(MT.EQ.18) THEN
C* Neutron multiplicities for fission are included in MF1
        YL=YL*YL6
      ELSE
        YL=YL6
      END IF
C* Check the data representation for this particle
c...
c...      print *,'processing particle/mf/mt/law',nint(zap),mf,mt,law
c...
      IF(LAW.EQ.7) GO TO 70
      IF(LAW.EQ.2) GO TO 62
C*        Unsupported Law - cannot skip the section for this particle
C*        Set IER to flag error and terminate
          PRINT *,'DXSEND ERROR - Processing MF/MT/Law',MF,MT,LAW
          IER=12
          GO TO 900
C*
C* Process MF 6 Law 2 data
   62 CONTINUE
        CALL RDTAB2(LEF,C1,C2,L1,L2,NR,NE,NBT,INR,IER)
C...    IF(NR.GT.1)
C... 1   PRINT *,' WARNING - Multiple interp.ranges for MF 6, MT',MT
        NE1=0
        EI1=0
        MX=MRW-LXX
        DO IE=1,NE
C* Process the angular distribution for each incident energy
          CALL RDLIST(LEF,C1,EI2,LANG,L2,NW,NEP1,RWO(LXX),MX,IER)
          IF(IER.NE.0) THEN
            STOP 'DXSEN1 ERROR - Reading MF6 Law 2 data in RDLIST'
          END IF
          IF(LANG.LT.11) THEN
            PRINT *,'WARNING - Cannot process MF/MT/LANG',MF,MT,LANG
            PRINT *,'          Distributions not pointwise'
            IER=41
            GO TO 900
          END IF
          DO J=1,NEP1
            RWO(LXE-1+J)=RWO(LXX+2*J-2)
            RWO(LXX-1+J)=RWO(LXX+2*J-1)
          END DO
C* Linearise the angular distribution
          EPL=0.005
          NRA=1
          LRA=NR+1
          NBT(LRA)=NEP1
          INR(LRA)=LANG-10
          CALL VECLIN(NRA,NEP1,NBT(LRA),INR(LRA)
     1               ,RWO(LXE),RWO(LXX),LD,EPL)
C* Lin-interpolate angular distributions over incident energies
          INA=2
          INE=2
          CALL FINT2D(EIN,EI1,NE1 ,ENR     ,DXS     ,INA
     1                   ,EI2,NEP1,RWO(LXE),RWO(LXX),INE,LD)
        END DO
C* The rest of processing is identifal to MF 4 case
        GO TO 45
C*
C* Process MF 6 Law 7 data
   70 CONTINUE
C* Read incident energy definitions
      CALL RDTAB2(LEF,C1,C2,L1,L2,NR,NE,NBT,INR,IER)
      NM=NR+1
      NE1=0
      EI1=0
      DO 74 IE=1,NE
C* Read incident cosine definitions
      CALL RDTAB2(LEF,C1,EI2,L1,L2,NRM,NMU,NBT(NM),INR(NM),IER)
      IF(IER.NE.0 .AND. IER.NE.11) THEN
         PRINT *,'ERROR READING TAB2 i(En) MF/MT/IER',IE,MF,MT,IER
         STOP 'DXSEN1 ERROR reading distributions'
      END IF
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
      IF(IER.NE.0 .AND. IER.NE.11) THEN
        PRINT *,'ERROR READING TAB1 i(cos) MF/MT/IER',IM,MF,MT,IER
        STOP 'DXSEN1 ERROR reading distributions'
      END IF
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
      DO I=1,NEP1
        RWO(LX-1+I)=RWO(LX-1+I)/SS
      END DO
C...  IF(ABS(SS-1).GT.0.01) PRINT *,'MAT,MT,Ei,SS',ZA0,MT0,EI2,SS
C*
C* Lin-interpolate between distributions for two incident energies
      INE=2
      MX=MRW-LX
      CALL FINT2D(EIN,EI1,NE1 ,ENR,DXS    ,INR
     1               ,EI2,NEP1,RWO,RWO(LX),INE,MX)
C*
   74 CONTINUE
      GO TO 800
C*
C* Photon emission data
C... Assume isotropic scattering - no coding to read MF 14
  120 IF(MF.EQ.13) THEN
C* File MF13 has same structure as MF12/LO=1 but includes XS
        XS=1
        L1=1
      END IF
      IF(KEA.NE.2) GO TO 140
      LO=L1
      LV =0
      LG =0
      LLI=1
      RWO(LLI)=0
      LLI=LLI+1
C* Read photon branching ratios to all discrete-leves up to the present
  122 LV =LV +1
      IWO(LV)=LLI
C...
c...      print *,'Request: mat/mf/mt,lo,lvl',mat,mf,mt,lo,lv
C...
      IF(LO.EQ.2) THEN
C* Transition probability arrays given
C* Changing LF between data sets is currently not accomodated
        IF(LG.GT.0 .AND. LG.NE.L2)
     1    STOP 'DXSEN1 ERROR - Redefined LG reading MF 12'
        LG =L2
        NS =N1
        LL =LV*(LG+1)
        DO J=1,LL
          RWO(LLI+J)=0
        END DO
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
        MM =MT
        MT =0
        CALL FINDMT(LEF,ZA0,ZA,AWR,L1,L2,N1,N2,MAT,MF,MT,IER)
        IF(IER.EQ.0 .AND.
     1     (MM+1.EQ.MT .AND. MT.LE.MT0) ) GO TO 122
        IWO(LV+1)=LLI
C*
C* All levels up to the current one read - sum the yields
c...
c...    print *,'     Summing discr.level yields MF/MT,LV',MF,MT0,LV
c...
        CALL SUMYLD(LV,LG,IWO,RWO,GTO,NT,NW)
        LLI=1
c...
c...    CALL SUMYLG(LV,LG,IWO,RWO,GTO,NT,NW,RWO(LLI))
C* Process the gamma lines
        NX =(MRW-(LLI+NW))/2
        LXE=LLI+NW
        LXB=LXE+NX
        NEN=21
        NE1=1
        ENR(1)=RWO(1)
        DXS(1)=0
        DO 128 JT=1,NT
          EOU=RWO(LLI+(JT-1)*(LG+1))
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
        NK =N1
        NE1=0
C* Subdivide the available work array
        LXE=1
        LE =MRW/2
        KXX=LE
        KX =LE/2
        LXX=LXE+KX
        LX =LE +KX
C* Dummy-read the total photon multiplicity
        IF(NK.GT.1) CALL RDTAB1(LEF,C1,C2,L1,L2,NR,NP,NBT,INR
     1                         ,RWO(LXE),RWO(LXX),KXX,IER)
C* Read photon data for all subsections
        DO 138 IK=1,NK
C* Photon energy and yield
        CALL RDTAB1(LEF,EG,ES,LP,LF,NR,NP,NBT,INR
     1             ,RWO(LXE),RWO(LXX),KXX,IER)
        IF(IER.NE.0) THEN
          PRINT *,'ERROR READING MF/MT/IER',MF,MT,IER
          STOP 'DXSEND1 ERROR - Reading MF12'
        END IF
        YLK=FINTXS(EIN,RWO(LXE),RWO(LXX),NP,INR,IER)
        IF(IER.NE.0) THEN
          PRINT *,'ERROR READING MF/MT/IER',MF,MT,IER
          STOP 'DXSEND1 ERROR - Reading MF12'
        END IF
C* Modify photon energy for primary photons
        IF(LP.EQ.2) EG=EG+EIN*AWR/(AWR+1)
C* Consider various data representations in MF12
        IF(LF.EQ.1) THEN
C* Normalised tabulated function given in file MF15
          IF(IK.LT.NK) THEN
            PRINT *,'DXSEN1 WARNING - Tabulated distr. not last'
          END IF
          MF=15
          CALL FINDMT(LEF,ZA0,ZA,AWR,L1,L2,NC,N2,MAT,MF,MT,IER)
          IF(IER.NE.0) THEN
            PRINT *,'WARNING - No tabulated data for MF15 MT',MT0
            GO TO 800
          END IF
C* Only one section for tabulated distributions is allowed at present
          IF(NC.GT.1) THEN
            PRINT *,'DXSEN1 WARNING >1 section for MF/MT',MF,MT
            STOP 'DXSEN1 ERROR >1 section for MF 15'
          END IF
C* Read the fractional contribution of the section 
          CALL RDTAB1(LEF,C1,C2,L1,LF,NR,NP,NBT,INR
     1               ,RWO(LE),RWO(LX),KX,IER)
          YLT=FINTXS(EIN,RWO(LE),RWO(LX),NP,INR,IER)
          IF(LF.NE.1) THEN
            IER=99
            PRINT *,'WARNING - No coding for MF 15, MT',MT0,' LF',LF
            GO TO 900
          END IF
          EI1=0
          NEN=0
C* Read interpolation data for the incident energies
          CALL RDTAB2(LEF,C1,C2,L1,L2,NR,NE,NBT,INR,IER)
          NM =2
          DO 134 IE=1,NE
C* For each incident energy read the outg.particle energy distribution
          CALL RDTAB1(LEF,C1,EI2,L1,L2,NRP,NF,NBT(NM),INR(NM)
     1               ,RWO(LE),RWO(LX),KX,IER)
          IF(IER.NE.0) THEN
            PRINT *,'DXSEN1 ERROR - Reading MF/MT',MF,MT
            STOP 'DXSEN1 ERROR reading energy.distrib.'
          END IF
C* Scale by the fractional contribution
          DO J=1,NF
            RWO(LX-1+J)=RWO(LX-1+J)*YLT
          END DO
C* Linearise to tolerance EXS, if necessary
          EXS=0.01
          IF(NRP.GT.1) CALL VECLIN(NRP,NF,NBT(NM),INR(NM)
     1                            ,RWO(LE),RWO(LX),KX,EXS)
          INE=INR(NM)
C* Interpolate outgoing particle energy distributions
          CALL FINT2D(EIN,EI1,NEN ,RWO(LXE),RWO(LXX),INR
     1                   ,EI2,NF  ,RWO(LE) ,RWO(LX) ,INE,KX)
  134     CONTINUE
C*
        ELSE IF(LF.EQ.2) THEN
C* Discrete photon energy
          NEN=21
          CALL FNGAUS(EG,NEN,RWO(LXE),RWO(LXX),EPS)
        ELSE
          PRINT *,'ERROR - Illegal flag MF/MT/LF',MF,MT,LF
          STOP 'DXSEND1 ERROR - Illegal LF in MF 12'
        END IF
C* Generate the union grid
        LUE=LXE+NEN
        LUX=LXX+NEN
        KX =NX -NEN
        CALL UNIGRD(NE1,ENR,NEN,RWO(LXE),NE2,RWO(LUE),KX)
C* Interpolate accumulated distribution to the union grid
        CALL FITGRD(NE1,ENR,DXS,NE2,RWO(LUE),RWO(LUX))
        NE1=NE2
        DO I=1,NE1
          ENR(I)=RWO(LUE-1+I)
          DXS(I)=RWO(LUX-1+I)
        END DO
C* Interpolate distribution for the current photon to the union grid
        CALL FITGRD(NEN,RWO(LXE),RWO(LXX),NE1,RWO(LUE),RWO(LUX))
C* Assume isotropic photon distributions
        ANG=0.5
C* Add the current to the saved distribution
        FRC=ANG*YLK
        DO I=1,NE1
          DXS(I)=DXS(I)+RWO(LUX-1+I)*FRC
        END DO
  138   CONTINUE
      ELSE
        IER=13
        PRINT *,'WARNING - No coding for MF 12, MT',MT0,' LO',LO
        GO TO 900
      END IF
      GO TO 800
C*
C* Photon angular distributions
  140 CONTINUE
        IER=13
        PRINT *,'WARNING - No coding/data for MF',MF,' MT',MT0
        GO TO 900
C*
C* Distribution assembled - check that the last point is zero
  800 NEN=NE1
      IF(KEA.EQ.2) THEN
        IF(DXS(NEN).NE.0) THEN
          NEN=NEN+1
          ENR(NEN)=ENR(NEN-1)
          DXS(NEN)=0
        END IF
        IF(DEG.LT.0) SAN=SAN*2
      END IF
C* Scale the distribution by the cross section
      SS=YL*XS*SAN
      DO I=1,NEN
        DXS(I)=SS*DXS(I)
      END DO
c...
c...      print *,'nen',nen
c...      print *,enr(1),dxs(1)
c...      print *,enr(nen),dxs(nen)
c...
C*
  900 RETURN
C*
  902 FORMAT(2F11.0,4I11)
      END
      SUBROUTINE PROMAS(IZA,AMS)
C-Title  : Subroutine PROMAS
C-Purpose: Define projectile mass from its ZA designation
C-
C* AMDC Audi-Wapstra mass tables 2003 "http://www-nds.iaea.org/amdc/"
C* Subtract electron mass and add ionisation energy defect
      IF(IZA.EQ.   1) THEN
        AMS = 1.008664916
      ELSE IF(IZA.EQ.1001) THEN
        AMS = 1.007825032 -   0.00054857991 + 0.000000015
      ELSE IF(IZA.EQ.1002) THEN
        AMS = 2.014101778 -   0.00054857991 + 0.000000015
      ELSE IF(IZA.EQ.1003) THEN
        AMS = 3.016049278 -   0.00054857991 + 0.000000015
      ELSE IF(IZA.EQ.2003) THEN
        AMS = 3.016029319 - 2*0.00054857991 + 0.000000085
      ELSE IF(IZA.EQ.2004) THEN
        AMS = 4.002603254 - 2*0.00054857991 + 0.000000085
      ELSE
        AMS=0
      END IF
      RETURN
      END
      SUBROUTINE GETSTD(LES,MXNP,ZA1,MF1,MT1,MST,QM,QI
     &                 ,NP,EN,XS,DX,RWO,MXRW)
C-Title  : Subroutine LSTSTD
C-Purpose: Get cross section and its absolute uncertainty
C-Description:
C-D Negative MT1 is a flag to skip processing the covariance data
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
      MT=ABS(MT1)
      IF(MT.GT.40000) MT=MT-40000
C*
C* Search the ENDF file for section MT in file MF3
      CALL FINDMT(LES,ZA1,ZA,AWR,L1,L2,N1,N2,MAT,MF,MT,IER)
c...
c...  print *,'getstd za1,mat,mf,mt,ier',za1,mat,mf,mt,ier
c...
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
        PRINT *,'WARNING - forced lin-lin interp. instead of',INT(1)
      END IF
c...
c...      print *,'done mf/mt',mf,mt
c...
C* Negative MT1 is a flag to skip processing the covariance data
      IF(MF1.NE.3 .OR. MT1.LT.0) GO TO 90
      MF=33
      MT=MT1
C*
C* Search the ENDF file for section MT1 in file MF33
      CALL FINDMT(LES,ZA1,ZA,AWR,L1,L2,N1,N2,MAT,MF,MT,IER)
c...
c...          print *,'Found mf,mt,ier',mf,mt,ier
c...
      IF(IER.NE. 0) GO TO 90
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
      SUBROUTINE YLDPOU(YI,MT,KZAP)
C-Title  : Subroutine YLDPOU
C-Purpose: Define yield YI of particla KZAP in reaction MT
C-Author : A. Trkov
C-Version: 06/05 Fix yield for elastic.
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
        IF(MT.EQ. 2 .OR. MT.EQ. 4 .OR. MT.EQ.22 .OR. MT.EQ.23 .OR.
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
        IF(MT.EQ. 2 .OR. MT.EQ.28  .OR. (MT.GE.41 .AND.MT.LE.42) .OR.
     &     MT.EQ.45  .OR. MT.EQ.103 .OR. MT.EQ.112 .OR.
     &    (MT.GE.600.AND. MT.LE.649)) YI=1
        IF(MT.EQ.44 .OR. MT.EQ.111) YI=2
      ELSE IF(KZAP.EQ.1002) THEN
C* Outgoing deuterons
        IF(MT.EQ. 5) YI=0
        IF(MT.EQ.2 .OR. MT.EQ.11 .OR. MT.EQ.32 .OR. MT.EQ.35 .OR.
     &     MT.EQ.104) YI=1
      ELSE IF(KZAP.EQ.2003) THEN
C* Outgoing He-3
        IF(MT.EQ. 5) YI=0
        IF(MT.EQ. 2 .OR. MT.EQ.34 .OR. MT.EQ.106) YI=1
      ELSE IF(KZAP.EQ.2004) THEN
C* Outgoing alphas
        IF(MT.EQ. 5) YI=0
        IF(MT.EQ. 2 .OR.
     &     MT.EQ. 22 .OR. MT.EQ.24 .OR. MT.EQ.25 .OR. MT.EQ.45 .OR.
     &     MT.EQ.107 .OR. MT.EQ.112.OR.(MT.GE.800.AND.MT.LE.849)) YI=1
        IF(MT.EQ.29 .OR. MT.EQ.30 .OR. MT.EQ.35 .OR. MT.EQ.36 .OR.
     &     MT.EQ.108) YI=2
        IF(MT.EQ.109) YI=3
      ELSE
C* Recoils
        YI=1
      END IF
      RETURN
      END
      SUBROUTINE SUMYLG(LV,LG,ID,RWO,GTO,NG,LW,GAM)
C-Title  : Subroutine SUMYLG
C-Purpose: Sum yields from lower levels and redefine gamma energies
C-Version: October 2004 - rewritten to correct flawed logic
C-Description:
C-D  LV  Total number of levels
C-D  LG  Flag for the number of parameters:
C-D       1  Branching ratios only
C-D       2  Branching ratios and gamma fractions given.
C-D  ID  index array giving the starting address of the data
C-D      for a each level in the work array RWO.
C-D  RWO Level energy and branching information. Level zero
C-D      (ground state) only contains the level energy  in the
C-D      first word or array RWO. The data for each level consist
C-D      of pairs of numbers, giving the final level energy and
C-D      the branching ratio to that level.
C-D  GTO Sum of all gamma fractions - it can be used for
C-D      checking purposes. It should be close to 1.
C-D  NG  Final number of gamma lines packed into the output array GAM.
C-D  LW  Length of the packed output vector.
C-D  GAM Output array of packed gamma energies and yields.
C-
      DIMENSION  ID(1),RWO(1),GAM(1)
      RETURN
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
C...        PRINT *,'Lvl,Adr,ND',I,LI,ND,(RWO(LI-1+J),J=1,ND)
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
            DO L=1,LL
              RWO(LJ-L+LG+1)=RWO(LJ-L)
            END DO
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
      FUNCTION SPMAXW(EE,EI,EU,THETA)
C-Title  : Function SPMAXW
C-Purpose: Calculate the Maxwellian fission spectrum value at energy EE
      DATA PI/3.1415926/
C* Normalisation constant
      E0=(EI-EU)/THETA
      E1=SQRT(E0)
      CNRM=( 0.5*SQRT(PI)*ERRFN1(E1)-E1*EXP(-E0) ) * THETA**1.5
C* Spectrum
      SPMAXW=SQRT(EE)*EXP(-EE/THETA)/CNRM
      RETURN
      END
      FUNCTION SPEVAP(EE,EI,EU,THETA)
C-Title  : Function SPEVAP
C-Purpose: Calculate the EVAPORATION spectrum value at energy EE
C* Normalisation constant
      E0=(EI-EU)/THETA
      CNRM=THETA*THETA*(1 - (1+E0)*EXP(-E0) )
C* Spectrum
      SPEVAP=EE*EXP(-EE/THETA)/CNRM
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
      SUBROUTINE SKIPSC(LEF)
C-Title  : Subroutine SKIPSC
C-Purpose: Skip current section in an ENDF file
C-
      CHARACTER*66 C66
   20 CALL RDTEXT(LEF,MAT,MF,MT,C66,IER)
      IF(MT.NE.0) GO TO 20
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
      SUBROUTINE RDTAB2(LEF,C1,C2,L1,L2,N1,N2,NBT,INR,IER)
C-Title  : Subroutine RDTAB2
C-Purpose: Read an ENDF TAB2 record
C-D  Error condition:
C-D    IER=1  End-of-file
C-D        2  Read error
      DIMENSION    NBT(N1),INR(N1)
C*
      READ (LEF,902,END=100,ERR=200) C1,C2,L1,L2,N1,N2
      READ (LEF,903,END=100,ERR=200) (NBT(J),INR(J),J=1,N1)
      RETURN
  100 IER=1
      RETURN
  200 IER=2
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
      FUNCTION FINTXS(EIN,EN,XS,NP,INR,IER)
C-Title  : Function FINTXS
C-Purpose: Interpolate the cross section table to EIN
C-Description:
C-D  EIN  Incident energy
C-D  EN   Array of energies
C-D  XS   Array of corresponding cross sections
C-D  NP   Number of points in the cross section array
C-D  INR  Interpolation law (INR=1,2 allowed)
C-D  IER  = 0 - normal termination
C-D        11 - requested point outside interpolation range
      DIMENSION EN(NP),XS(NP)
      IER=0
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
      SUBROUTINE WRTAB1(LIB,MAT,MF,MT,NS,C1,C2,L1,L2
     1                 ,NR,NP,NBT,INR,X,Y)
C-Title  : WRTAB1 Subroutine
C-Purpose: Write a TAB1 record to an ENDF file
      CHARACTER*11  BLN,REC(6)
      DIMENSION     NBT(NR),INR(NR),X(NP),Y(NP)
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
      DIMENSION     NBT(NR),INR(NR)
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
C-D  are interpolated. Distributions are first linearised (if necessary)
C-D  and defined on a union grid. Then they are interpolated to the
C-D  independent argument value AI. Allowed interpolation laws are
C-D  INR=1 (histogram), INR=2 (linear), INR=12 (corresponding point)
C-D  or INR=22 (unit base). Special features:
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
      DIMENSION  EN1(1),XS1(1), EN2(MEP2),XS2(MEP2)
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
      DO I=1,NEP2
        EN1(I)=EN2(I)
        XS1(I)=XS2(I)
      END DO
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
C...    PRINT *,'UNIGRD ERROR in grid-1:',NEU,KX
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
C...     PRINT *,'NEU,KX',NEU,KX
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
      DO I=1,NEN
        DXS(I)=DXS(I)/SS
      END DO
C*
      RETURN
      END
      SUBROUTINE YTGEOU(PMU,EA,EB,NEP,EOU,DXS,INR)
C-Title  : Subroutine YTGEOU
C-Purpose: Integrate a pointwise tabulated function over EA - EB
C-Author : A.Trkov, IAEA, Vienna, Austria
C-Version:
C-V  05/01 Fix interpolation at energy boundaries
C* WARNING: Only lin-lin or histogram interpolation allowed
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
      IF(E1.GE.EB) RETURN
C* Treat upper energy bound
      IF(E2.GT.EB) THEN
        IF(INR.LT.2) THEN
          F2=F1
        ELSE
          F2=F1+(F2-F1)*(EB-E1)/(E2-E1)
        END IF
        E2=EB
      END IF
C* Define average function value over the interval
      FF=0.5*(F2+F1)
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
      DIMENSION  EN0(MEP2),XS0(MEP2),
     1           EN1(MEP2),XS1(MEP2),
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
        DO I=1,NEP2
          EN0(I)=EN2(I)
          EN1(I)=EN2(I)
          XS0(I)=0
          XS1(I)=XS2(I)
        END DO
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
      DO I=1,NEU
        EN0(I)=EN2(LUNI-1+I)
        XS0(I)=XS2(LUNI-1+I)
      END DO
C* Interpolate first distribution to the union grid
      CALL FITGRD(NEP1,EN1,XS1,NEU,EN2(LUNI),XS2(LUNI))
      DO I=1,NEU
        EN1(I)=EN2(LUNI-1+I)
        XS1(I)=XS2(LUNI-1+I)
      END DO
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
      SUBROUTINE VECLIN(NR,NP,NBT,INR,ENR,XSR,MXPT,EPS)
C-Title  : Subroutine VECLIN
C-Purpose: Expand tabulated vector to be lin-lin interpolable
C-Extern.: FINEND
      DIMENSION  NBT(NR),INR(NR),ENR(MXPT),XSR(MXPT)
C*
      IIN=1
      I1 =1
      I2 =2
      MP =NP
C* Check interpolation between points
      DO I=2,MP
        IF(I.GT.NBT(IIN)) IIN=IIN+1
        IF(IIN.GT.NR) STOP 'VECLIN ERROR - Illegal interpolation range'
        INT=INR(IIN)
        IF(INT.LE.2) GO TO 40
        E1=ENR(I1)
        X1=XSR(I1)
        E2=ENR(I2)
        X2=XSR(I2)
        EA=E1
        XA=X1
C* Check the midpoint of the remaining interval
   20   EE=(EA+E2)/2
        XX=FINEND(INT,EE,E1,X1,E2,X2)
        XL=FINEND(  2,EE,EA,XA,E2,X2)
        IF(ABS(XL-XX).GT.EPS*XX) THEN
C* Interpolation tolerance not satisfied
C* Insert a point and shift the rest
          ES=E2
          XS=X2
          DO J=I2,NP
            ER=ENR(J+1)
            XR=XSR(J+1)
            ENR(J+1)=ES
            XSR(J+1)=XS
            ES=ER
            XS=XR
          END DO
          ENR(I2)=EE
          XSR(I2)=XX
          EA=EE
          XA=XX
          I2=I2+1
          NP=NP+1
          GO TO 20
        END IF
   40   I1=I2
        I2=I2+1
      END DO
C* Update the interpolation range information
      NR=1
      NBT(1)=NP
      INR(1)=2
      RETURN
      END
      FUNCTION FINEND(INT,EE,E1,X1,E2,X2)
C-Title  : Function FINEND
C-Purpose: Interpolate between two points according to ENDF interp.flags
      ZERO  =0
      FINEND=X1
C* Histogram interpolation or flat function
      IF(INT.EQ.1 .OR. X1.EQ.X2 .OR. EE.EQ.E1) RETURN
C* Other ENDF interpolation laws
      IF     (INT.EQ.2) THEN
C* Linear
        FINEND=X1+(EE-E1)*(X2-X1)/(E2-E1)
        RETURN
      ELSE IF(INT.EQ.3) THEN
C* Linear in ln(x)
        IF(E1*E2.LE.ZERO) RETURN
        FINEND=X1+(X2-X1)*LOG(EE/E1)/LOG(E2/E1)
        RETURN
      ELSE IF(INT.EQ.4) THEN
C* Linear in ln(y)
        FINEND=X1*(X2/X1)**((EE-E1)/(E2-E1))
        RETURN
      ELSE IF(INT.EQ.5) THEN
C* Log-log
        IF (X1*X2.LE.ZERO) RETURN
        IF (E1*E2.LE.ZERO) RETURN
        FINEND=X1*(X2/X1)**(LOG(EE/E1)/LOG(E2/E1))
        RETURN
      ELSE
        PRINT *,'FINEND ERROR - Illegal interpolation flag request',INT
        STOP 'FINEND ERROR - Illegal interpolation flag'
      END IF
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
