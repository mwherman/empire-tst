Ccc   * $Rev: 2526 $
Ccc   * $Author: shoblit $
Ccc   * $Date: 2012-02-09 21:34:11 +0100 (Do, 09 Feb 2012) $
 
      SUBROUTINE KUNIEDA(Izat)
      IMPLICIT REAL*8(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: AC0, AC1, AD0, AD1, ALAso, ALAvr, ALAwd, ALF, AR1, ARO, 
     &          AS1, ASO, AW0, AW1, AZ, BNDc, CCOul, CISo, PDIs, RC, RD, 
     &          RR, RRBwc, RRWid, RS, RW, RZ, RZBwc, RZWid, VR0, VR1, 
     &          VR2, VR3, VRLa, VS, WC0, WC1, WCA1, WCBw, WCIso, WCWid, 
     &          WD0, WD1, WDA1, WDBw, WDWid, WS1, WSBw, WSO, WSWid
      COMMON /OMP   / VR0, VR1, VR2, VR3, VRLa, ALAvr, WD0, WD1, WDA1, 
     &                WDBw, WDWid, ALAwd, WC0, WC1, WCA1, WCBw, WCWid, 
     &                BNDc, VS, ALAso, WSO, WS1, WSBw, WSWid, RR, RRBwc, 
     &                RRWid, PDIs, ARO, AR1, RD, AD0, AD1, RC, AC0, AC1, 
     &                RW, AW0, AW1, RS, ASO, AS1, RZ, RZBwc, RZWid, AZ, 
     &                CCOul, ALF, CISo, WCIso
C
C Dummy arguments
C
      INTEGER :: Izat
C
C Local variables
C
      REAL*8 :: at, at3
      REAL*8 :: DBLE
      INTEGER :: izt
C
C*** End of declarations rewritten by SPAG
C
 
C     target mass and charge
C     -----------------------
      at = MOD(Izat,1000)
      at = DBLE(at)
      at3 = 1.D0/(at**0.3333333333)
      izt = Izat/1000
 
C     real volume depth : V_R^0
C     --------------------------
      VR0 = -34.40D0 - 6.18D-2*at + 3.37D-4*at**2 - 6.52D-7*at**3
      IF(izt.EQ.12)VR0 = -39.0D0
      IF(izt.EQ.13)VR0 = -39.0D0
      IF(izt.EQ.14)VR0 = -39.0D0
      IF(izt.EQ.20)VR0 = -37.5D0
      IF(izt.EQ.26)VR0 = -37.0D0
      IF(izt.EQ.28)VR0 = -37.0D0
      IF(izt.EQ.29)VR0 = -37.0D0
      IF(izt.EQ.39)VR0 = -37.5D0
      IF(izt.EQ.40)VR0 = -37.5D0
      IF(izt.EQ.41)VR0 = -38.0D0
      IF(izt.EQ.42)VR0 = -38.0D0
      IF(izt.EQ.50)VR0 = -38.0D0
      IF(izt.EQ.58)VR0 = -38.5D0
      IF(izt.EQ.73)VR0 = -38.5D0
      IF(izt.EQ.74)VR0 = -38.5D0
      IF(izt.EQ.79)VR0 = -38.5D0
      IF(izt.EQ.82)VR0 = -38.5D0
      IF(izt.EQ.83)VR0 = -38.5D0
      IF(izt.EQ.90)VR0 = -38.5D0
      IF(izt.EQ.92)VR0 = -39.0D0
 
C     real volume depth : V_R^1
C     --------------------------
      VR1 = 0.027D0
 
C     real volume depth : V_R^2
C     --------------------------
      VR2 = 0.000120D0
 
C     real volume depth : V_R^3
C     --------------------------
      VR3 = 0.00000035D0
 
C     real volume depth : V_R^{DISP}
C     -------------------------------
      VRLa = 94.88D0
 
C     lambda for real volume depth : lambda_R
C     ----------------------------------------
      ALAvr = 1.05D-2 + 1.63D-4*VR0
      IF(izt.EQ.13)ALAvr = 0.00430D0
      IF(izt.EQ.20)ALAvr = 0.00450D0
      IF(izt.EQ.26)ALAvr = 0.00450D0
      IF(izt.EQ.28)ALAvr = 0.00450D0
      IF(izt.EQ.29)ALAvr = 0.00450D0
      IF(izt.EQ.73)ALAvr = 0.00423D0
      IF(izt.EQ.74)ALAvr = 0.00423D0
      IF(izt.EQ.79)ALAvr = 0.00423D0
      IF(izt.EQ.82)ALAvr = 0.00425D0
      IF(izt.EQ.83)ALAvr = 0.00425D0
      IF(izt.EQ.90)ALAvr = 0.00425D0
      IF(izt.EQ.92)ALAvr = 0.00417D0
C     ------------------------------------------
 
C     imaginary surface depth : W_D^{DISP}
C     --------------------------------------
      WDBw = 11.08D0 + 6.83D-5*at**2
      IF(izt.EQ.12)WDBw = 11.5D0
      IF(izt.EQ.13)WDBw = 11.0D0
      IF(izt.EQ.14)WDBw = 11.0D0
      IF(izt.EQ.20)WDBw = 11.0D0
      IF(izt.EQ.26)WDBw = 11.0D0
      IF(izt.EQ.28)WDBw = 11.0D0
      IF(izt.EQ.29)WDBw = 11.5D0
      IF(izt.EQ.39)WDBw = 11.5D0
      IF(izt.EQ.40)WDBw = 11.5D0
      IF(izt.EQ.41)WDBw = 12.0D0
      IF(izt.EQ.42)WDBw = 12.0D0
      IF(izt.EQ.50)WDBw = 12.8D0
      IF(izt.EQ.58)WDBw = 12.5D0
      IF(izt.EQ.73)WDBw = 13.0D0
      IF(izt.EQ.74)WDBw = 13.0D0
      IF(izt.EQ.79)WDBw = 13.5D0
      IF(izt.EQ.82)WDBw = 14.0D0
      IF(izt.EQ.83)WDBw = 14.0D0
      IF(izt.EQ.90)WDBw = 15.0D0
      IF(izt.EQ.92)WDBw = 15.0D0
 
C     imaginary surface depth : WID_D
C     --------------------------------------
      WDWid = 12.72D0 - 1.54D-2*at + 7.14D-5*at**2
      IF(izt.EQ.12)WDWid = 10.0D0
      IF(izt.EQ.13)WDWid = 10.0D0
      IF(izt.EQ.14)WDWid = 10.0D0
      IF(izt.EQ.20)WDWid = 10.0D0
      IF(izt.EQ.26)WDWid = 12.0D0
      IF(izt.EQ.28)WDWid = 12.0D0
      IF(izt.EQ.29)WDWid = 12.0D0
      IF(izt.EQ.39)WDWid = 12.0D0
      IF(izt.EQ.40)WDWid = 12.0D0
      IF(izt.EQ.41)WDWid = 12.0D0
      IF(izt.EQ.42)WDWid = 12.0D0
      IF(izt.EQ.50)WDWid = 12.0D0
      IF(izt.EQ.58)WDWid = 12.0D0
      IF(izt.EQ.73)WDWid = 12.0D0
      IF(izt.EQ.74)WDWid = 12.0D0
      IF(izt.EQ.79)WDWid = 12.0D0
      IF(izt.EQ.82)WDWid = 13.0D0
      IF(izt.EQ.83)WDWid = 13.0D0
      IF(izt.EQ.90)WDWid = 13.0D0
      IF(izt.EQ.92)WDWid = 13.0D0
 
C     lambda for imaginary surface depth : lambda_D
C     -----------------------------------------------
      ALAwd = 0.0140D0
 
C     imaginary volume depth : W_V^{DISP}
C     -------------------------------------
      WCBw = 17.0D0
 
C     imaginary volume depth : WID_V
C      ------------------------------------
      WCWid = 105.05D0 - 14.13D0/(1.D0 + EXP((at-100.21D0)/13.47D0))
      IF(izt.LE.14)WCWid = 90.0D0
      IF(izt.EQ.20)WCWid = 90.0D0
      IF(izt.EQ.26)WCWid = 90.0D0
      IF(izt.EQ.28)WCWid = 90.0D0
      IF(izt.EQ.29)WCWid = 95.0D0
      IF(izt.EQ.39)WCWid = 95.0D0
      IF(izt.EQ.40)WCWid = 95.0D0
      IF(izt.EQ.41)WCWid = 98.0D0
      IF(izt.EQ.42)WCWid = 95.0D0
      IF(izt.EQ.50)WCWid = 103.0D0
      IF(izt.EQ.58)WCWid = 105.0D0
      IF(izt.EQ.73)WCWid = 105.0D0
      IF(izt.EQ.74)WCWid = 105.0D0
      IF(izt.EQ.79)WCWid = 105.0D0
      IF(izt.EQ.82)WCWid = 105.0D0
      IF(izt.EQ.83)WCWid = 105.0D0
      IF(izt.EQ.90)WCWid = 105.0D0
      IF(izt.EQ.92)WCWid = 105.0D0
 
 
C     real, imaginary spin-orbit depth (the global K-D OMP)
C     ------------------------------------------------------
      VS = 5.922D0 + 0.003D0*at
      ALAso = 0.005D0
      WSBw = -3.1D0
      WSWid = 160.D0
 
 
 
 
 
C     radius parameter for real-volume, imaginary-surface and imajinary-volume
C     potential (no mass dipendence)
C     ---------------------------------------------------------------------------
      RR = 1.210D0
 
C     diffuseness parameter for real-volume, imaginary-surface and imajinary-volume
C     --------------------------------------------------------------------------------
      ARO = 0.49D0 + 3.22D-3*at - 2.07D-5*at**2 + 4.47D-8*at**3
      IF(izt.EQ.12)ARO = 0.560D0
      IF(izt.EQ.13)ARO = 0.520D0
      IF(izt.EQ.14)ARO = 0.540D0
      IF(izt.EQ.20)ARO = 0.610D0
      IF(izt.EQ.26)ARO = 0.610D0
      IF(izt.EQ.28)ARO = 0.620D0
      IF(izt.EQ.29)ARO = 0.620D0
      IF(izt.EQ.39)ARO = 0.650D0
      IF(izt.EQ.40)ARO = 0.635D0
      IF(izt.EQ.41)ARO = 0.650D0
      IF(izt.EQ.42)ARO = 0.650D0
      IF(izt.EQ.50)ARO = 0.655D0
      IF(izt.EQ.58)ARO = 0.650D0
      IF(izt.EQ.73)ARO = 0.660D0
      IF(izt.EQ.74)ARO = 0.660D0
      IF(izt.EQ.79)ARO = 0.660D0
      IF(izt.EQ.82)ARO = 0.671D0
      IF(izt.EQ.83)ARO = 0.670D0
      IF(izt.EQ.90)ARO = 0.685D0
      IF(izt.EQ.92)ARO = 0.685D0
 
C     radius parameter for spin-orbit potential (the K-D global omp)
C     ----------------------------------------------------------------
      RS = 1.18D0 - 0.65D0*at3
 
C     diffuseness parameter for spin-orbit potential (the K-D global omp)
C     ---------------------------------------------------------------------
      ASO = 0.59D0
 
C     Coulomb radius parameter
C     -------------------------
      RZ = 1.264D0
 
C     diffuseness parameter for the Coulomb potential
C     ------------------------------------------------
      AZ = 0.341D0
 
 
 
 
 
C     Coulomb coefficient : C_{coul}
C     --------------------------------
      CCOul = 0.9D0
 
C     isospin coefficient for real volume : C_{viso}
C     ------------------------------------------------
      CISo = 24.3D0
 
C     isospin coefficient for imaginary surface : C_{wiso}
C     -----------------------------------------------------
      WCIso = 18.0D0
      RETURN
      END SUBROUTINE KUNIEDA
 
 
 
