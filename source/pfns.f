Ccc   * $Rev: 2526 $
Ccc   * $Author: shoblit $
Ccc   * $Date: 2012-02-09 21:34:11 +0100 (Do, 09 Feb 2012) $
 
      SUBROUTINE GET_FRAGMPFNS(Fragmpfns,Emiss_en,Nen_emis,Eincid,Af,Zf,
     &                         Emed,Tequiv,Qval,Deltae,Pfntke,Pfnrat,
     &                         Pfnalp)
C
C     See N.V.Kornilov, A.B.Kagalenko and F.-J.Hambsch
C     Phys. At. Nuclei 62 (1999) pp 173-185
C
C
      IMPLICIT NONE
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      REAL*8 :: Af, Deltae, Eincid, Emed, Pfnalp, Pfnrat, Pfntke, Qval, 
     &          Tequiv, Zf
      INTEGER :: Nen_emis
      REAL*8, DIMENSION(Nen_emis) :: Emiss_en, Fragmpfns
C
C Local variables
C
      REAL*8 :: alpha, alpha0, cndef, coeff, e, efkin, eniuh, eniul, 
     &          erel, fmed, ftmp, hfdef, lfdef, r, thcf, thf, tlcf, tlf, 
     &          u0cf, ux
      REAL*8 :: BIND, FWATT, TKE
      REAL*8 :: DSQRT
      REAL :: FLOAT
      INTEGER :: i, iaf, iah, ial, izf, izh, izl
      INTEGER :: NINT
C
C*** End of declarations rewritten by SPAG
C
C     Dummy parameters
 
C     Local variables
C     real*8 fpost, fnscn, tscn, wscn
 
C     Mass of heavy fragment fixed following Malinovskii
      DATA iah/140/
 
C     Kornilov model parameters
      DATA thcf/0.8868D0/, r/1.248D0/, u0cf/32.9D0/
 
C     Scission neutrons for Th-232 following Lovchikova et al (Phys.At.Nuclei 67 (2004) p890)
C     data wscn/0.10d0/ ! "wscn,tscn" fitted to get scission neutron temperature (0.38 +/- 0.04)
C     data tscn/0.40d0/ !
 
      Emed = 0.D0
      Tequiv = 0.D0
      Fragmpfns = 0.D0
 
      iaf = NINT(Af)
      izf = NINT(Zf)
 
C     Parametrization of the Total Kinetic Energy in fission
      efkin = TKE(izf,iaf,Eincid)*Pfntke    ! PFNtke is the scaling factor
 
      ftmp = BIND(iaf - izf,izf,cndef)
 
C     Malinovskii parametrization of the heavy fragment charge
      izh = (Zf/Af)*iah - 0.5
      izl = izf - izh
      ial = iaf - iah
 
      ftmp = BIND(iah - izh,izh,hfdef)
      ftmp = BIND(ial - izl,izl,lfdef)
C     Total energy release in fission from mass excess for CN, heavy and light fragments
      erel = cndef - hfdef - lfdef
 
      alpha0 = 1.D0
C     Adjusted values (RCN, February 2006)
      IF(izf.LE.91)alpha0 = 0.947D0  ! Th, Pa and Ac chains
      IF(izf.EQ.92)alpha0 = 0.933D0  ! U chain
      IF(izf.EQ.94)alpha0 = 0.873D0  ! Pu chain
      IF(izf.EQ.95)alpha0 = 0.808D0  ! Am chain = Np237
C--------------------------------------------------------------------------
C     Following Kornilov's paper
      IF(izf.EQ.90)alpha0 = 0.947D0   ! Th232 target and all thorium chain
      IF(iaf.EQ.234.AND.izf.EQ.92)alpha0 = 0.92D0      ! U233  target
      IF(iaf.EQ.236.AND.izf.EQ.92)alpha0 = 0.936D0     ! U235  target
      IF(iaf.EQ.239.AND.izf.EQ.92)alpha0 = 0.88D0      ! U238  target
      IF(iaf.EQ.238.AND.izf.EQ.93)alpha0 = 0.808D0     ! Np237 target
      IF(iaf.EQ.240.AND.izf.EQ.94)alpha0 = 0.873D0     ! Pu240 target
      IF(iaf.EQ.253.AND.izf.EQ.98)alpha0 = 0.809D0     ! Cf252 target
C--------------------------------------------------------------------------
 
C     if scission neutrons are considered then no reduction of
C            the CMS energy is needed
C     if(wscn.ne.0.d0) alpha0 = 1.d0
C     Following Maslov we interpolate alpha from 10 to 12.1 MeV
      alpha = alpha0
      IF(Eincid.GT.10.AND.Eincid.LE.12.)alpha = alpha0 + 
     &   0.05*(10.D0 - Eincid)
      IF(Eincid.GT.12.)alpha = alpha0 - 0.05*2.D0   ! the value at eincid = 12
C
      alpha = alpha*Pfnalp    ! reduction of TKE for fragments
 
      eniul = FLOAT(iah)/FLOAT(ial*iaf)*alpha*efkin
      eniuh = FLOAT(ial)/FLOAT(iah*iaf)*alpha*efkin
 
      tlcf = thcf*r*Pfnrat
 
      ux = erel - efkin + Eincid + Qval
 
      IF(ux.LT.0)RETURN
 
      coeff = DSQRT(252./Af*ux/u0cf)
C     Following formulae (7) of the paper
      tlf = tlcf*coeff
      thf = thcf*coeff
      ftmp = 0.D0
      Emed = 0.D0
      DO i = 1, Nen_emis
        e = Emiss_en(i)
        Fragmpfns(i) = 0.5D0*(FWATT(e,eniuh,thf) + FWATT(e,eniul,tlf))
C       fpost = 0.5d0*( fwatt(e,EniuH,Thf) + fwatt(e,EniuL,Tlf) )
C       fnscn = fscn(e,tscn)
C       fragmPFNS(i) = wscn*fnscn + (1.d0 - wscn)*fpost
        IF(i.GT.1)THEN
          fmed = (Fragmpfns(i) + Fragmpfns(i - 1))*0.5
          Emed = Emed + fmed*Deltae*(Emiss_en(i) + Emiss_en(i - 1))
     &           *0.5D0
          ftmp = ftmp + fmed*Deltae
        ENDIF
      ENDDO
      IF(ftmp.GT.0)Emed = Emed/ftmp
      Tequiv = 2.D0/3.D0*Emed
 
      RETURN
      END SUBROUTINE GET_FRAGMPFNS
 
!---------------------------------------------------------------------------
 
      SUBROUTINE GET_FRAGMPFNS_LANL(Fragmpfns,Emiss_en,Nen_emis,Eincid,
     &                              Af,Zf,Emed,Tequiv,Qval,Deltae,
     &                              Pfntke,Pfnrat,Pfnalp)
C
C     See D. Madland original paper (NSE) on LA model
C
      IMPLICIT NONE
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: EMIn, EPLus
      COMMON /EPARAM/ EPLus, EMIn
C
C Dummy arguments
C
      REAL*8 :: Af, Deltae, Eincid, Emed, Pfnalp, Pfnrat, Pfntke, Qval, 
     &          Tequiv, Zf
      INTEGER :: Nen_emis
      REAL*8, DIMENSION(Nen_emis) :: Emiss_en, Fragmpfns
C
C Local variables
C
      REAL*8 :: abserr, cndef, e, efkin, eniuh, eniul, erel, fmed, ftmp, 
     &          ftmp1, ftmp2, hfdef, lfdef, r, thf, tlf, tm, ux
      REAL*8 :: BIND, GAUSS_INT1, SH_LAB, SL_LAB, TKE
      REAL*8 :: DSQRT
      REAL :: FLOAT
      INTEGER :: i, iaf, iah, ial, izf, izh, izl
      INTEGER :: NINT
C
C*** End of declarations rewritten by SPAG
C
C     Dummy parameters
 
C     Local variables
C     real*8 fpost, fnscn, tscn, wscn
 
 
C     Mass of heavy fragment fixed following Malinovskii
      DATA iah/140/
C     Ratio of the light to heavy fragments' Temperature taken from Kornilov
C     data r/1.248d0/  !Kornilov value
      DATA r/1.000D0/  !Original LANL value
C     Scission neutrons for Th-232 following Lovchikova et al (Phys.At.Nuclei 67 (2004) p890)
C     data wscn/0.10d0/ ! "wscn,tscn" fitted to get scission neutron temperature (0.38 +/- 0.04)
C     data tscn/0.40d0/ !
 
 
      Emed = 0.D0
      Tequiv = 0.D0
      Fragmpfns = 0.D0
 
      iaf = NINT(Af)
      izf = NINT(Zf)
 
      ftmp = BIND(iaf - izf,izf,cndef)
 
C     Parametrization of the Total Kinetic Energy in fission
      efkin = TKE(izf,iaf,Eincid)*Pfntke     ! PFNtke is the scaling factor
 
C     Malinovskii parametrization of the heavy fragment charge
      izh = (Zf/Af)*iah - 0.5
      izl = izf - izh
      ial = iaf - iah
 
      ftmp = BIND(iah - izh,izh,hfdef)
      ftmp = BIND(ial - izl,izl,lfdef)
C     Total energy release in fission from mass excess for CN, heavy and light fragments
      erel = cndef - hfdef - lfdef
 
C     if scission neutrons are considered
C     if(wscn.ne.0.d0) alpha0 = 1.d0
C
      eniul = FLOAT(iah)/FLOAT(ial*iaf)*efkin*Pfnalp    ! reduction of TKE for fragments
      eniuh = FLOAT(ial)/FLOAT(iah*iaf)*efkin*Pfnalp
 
      ux = erel - efkin + Eincid + Qval
 
      IF(ux.LT.0)RETURN
C
C     Level density assumed porportional to A/11
      tm = DSQRT(ux/(iaf/11.D0))
C
C     LA model (eq.11 CPC)
      tlf = tm*r*Pfnrat     ! r = Tlf/Thf ~ expected around 1.2 (original LA model value =1)
      thf = tm
 
      ftmp = 0.D0
      DO i = 1, Nen_emis
        e = Emiss_en(i)
 
        IF(e.LT.0.00001D0)CYCLE
 
        EPLus = (DSQRT(e) + DSQRT(eniul))**2
        EMIn = (DSQRT(e) - DSQRT(eniul))**2
        ftmp1 = GAUSS_INT1(SL_LAB,0.D0,tlf,abserr)
     &          /(2*tlf*tlf*DSQRT(eniul))
 
        EPLus = (DSQRT(e) + DSQRT(eniuh))**2
        EMIn = (DSQRT(e) - DSQRT(eniuh))**2
        ftmp2 = GAUSS_INT1(SH_LAB,0.D0,thf,abserr)
     &          /(2*thf*thf*DSQRT(eniuh))
 
        Fragmpfns(i) = 0.5D0*(ftmp1 + ftmp2)
 
C       fpost = 0.5d0*( fwatt(e,EniuH,Thf) + fwatt(e,EniuL,Tlf) )
C       fnscn = fscn(e,tscn)
C       fragmPFNS(i) = wscn*fnscn + (1.d0 - wscn)*fpost
 
        IF(i.GT.1)THEN
          fmed = (Fragmpfns(i) + Fragmpfns(i - 1))*0.5
          Emed = Emed + fmed*Deltae*(Emiss_en(i) + Emiss_en(i - 1))
     &           *0.5D0
          ftmp = ftmp + fmed*Deltae
        ENDIF
      ENDDO
 
      IF(ftmp.GT.0)Emed = Emed/ftmp
      Tequiv = 2.D0/3.D0*Emed
 
      RETURN
      END SUBROUTINE GET_FRAGMPFNS_LANL
 
!---------------------------------------------------------------------------
 
      FUNCTION TKE(Izf,Iaf,Einc)
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      REAL*8 :: Einc
      INTEGER :: Iaf, Izf
      REAL*8 :: TKE
C
C Local variables
C
      REAL*8 :: en, ftmp
      REAL :: REAL
C
C*** End of declarations rewritten by SPAG
C
C
C     Malinovskii et al, INDC(CCP)-277, IAEA 1987 (VANT N2,1987) in russian
C     Itkis et al, Yad.Fiz.52(1990) 23; Fiz.Elem.Ch.At. Yadra 29(1998) 389
C
 
      en = Einc
      IF(en.LT.0.D0)en = 0.D0
 
      IF(Izf.EQ.92)THEN   ! Uranium
        TKE = 169.24
        IF(en.LT.0.0001D0)THEN
          TKE = TKE + 0.4*en
C       Exact energy should be 7.80 Mev - Bn(IAF,ZAF), For U-238 Bn=6.15
        ELSEIF(en.GT.0.0001D0.AND.en.LE.1.65)THEN
          TKE = TKE + ( - 0.423 + 0.044*(Iaf - 230))*en
        ELSE
          TKE = TKE + (0.144 - 0.047*(Iaf - 230))*en
        ENDIF
        RETURN
      ENDIF
 
      IF(Izf.EQ.93)THEN   ! Neptunium
        TKE = 172.43
        IF(en.LE.0.0001D0)THEN
          TKE = TKE + 0.25*en
C       Exact energy should be 7.80 Mev - Bn(IAF,ZAF), For Np-237 Bn=6.57
        ELSEIF(en.GT.0.0001D0.AND.en.LE.1.23)THEN
          TKE = TKE - 0.14*en
        ELSE
          TKE = TKE - 0.22*en
        ENDIF
        RETURN
      ENDIF
 
      IF(Izf.EQ.94)THEN   ! Plutonium
        TKE = 176.4
        IF(en.LE.0.0001D0)THEN
          TKE = TKE + 0.06*en
C       Exact energy should be 7.80 Mev - Bn(IAF,ZAF), For Pu-239 Bn=5.65
        ELSEIF(en.GT.0.0001D0.AND.en.LE.2.15)THEN
          TKE = TKE - 0.12*en
        ELSE
          TKE = TKE - 0.21*en
        ENDIF
        RETURN
      ENDIF
 
      IF(Izf.EQ.95)THEN   ! Americium
        TKE = 179.74
        IF(en.LE.0.0001D0)THEN
          TKE = TKE + 0.08*en
        ELSE
          TKE = TKE - 0.25*en
        ENDIF
        RETURN
      ENDIF
 
      IF(Izf.EQ.96)THEN   ! Curium
        TKE = 183.23 - 0.338*(Iaf - 240)
        IF(Iaf.LE.242)TKE = 183.23 - 0.338*2
        IF(en.LE.0.0001D0)THEN
          TKE = TKE + 0.07*en
        ELSE
          TKE = TKE + 0.11*en
        ENDIF
        RETURN
      ENDIF
 
      IF(Izf.EQ.97)THEN   ! Bekerelium
        TKE = 185.08 - 0.405*(Iaf - 249)
        TKE = TKE + 0.1*en
        RETURN
      ENDIF
 
      IF(Izf.EQ.98)THEN   ! Californium
        TKE = 193.79 - 0.472*(Iaf - 240)
        TKE = TKE + 0.1*en
        RETURN
      ENDIF
C-------------------------------------------------------------------------
C
C     Itkis et al, Yad.Fiz.52(1990) 23; Fiz.Elem.Ch.At. Yadra 29(1998) 389
C
      ftmp = REAL(Izf)**2/REAL(Iaf)**(1.D0/3.D0)
      IF(ftmp.LE.900.)THEN
        TKE = 0.131*ftmp
      ELSE
        TKE = 0.104*ftmp + 24.3
      ENDIF
 
      IF(Izf.EQ.89)THEN   ! Actinium
        TKE = 0.104*ftmp + 23.3
        TKE = TKE + 0.4*en
        RETURN
      ENDIF
 
      IF(Izf.EQ.90)THEN   ! Thorium
C       TKE = 0.104*ftmp + 23.85
C       TKE = 0.104*ftmp + 24.05                 ! for delta <& 0 and beta2 = 0
C       TKE = 0.104*ftmp + 23.7                  ! for delta <& 0 and beta2 <>0
C       TKE = 0.104*ftmp + 25.1                  ! for delta = 0 and S2n = 0
C       TKE = 0.104*ftmp + 24.85                 ! for delta = 0 and S2n <& 0
C       TKE = 0.112*ftmp + 13.7                  ! Vladuca
C       TKE = 0.104*ftmp + 23.95                 ! for delta <& 0 and S2n <& 0
 
C       TKE = 0.104*ftmp + 23.7                  ! for delta = 0 and S2n = 0
C       TKE = TKE + 0.37*en                      ! eval
 
C       TKE = 0.104*ftmp + 25.1
C       TKE = 0.104*ftmp + 25.6
C       TKE = TKE + 0.15*en
 
        TKE = 0.104*ftmp + 25.75                 ! for delta = 0 and S2n <& 0
        TKE = TKE + 0.50*en                      ! eval
        RETURN
      ENDIF
 
      IF(Izf.EQ.91)THEN   ! Protoactinium
        TKE = 166.7 + 0.016D0*(ftmp - 1332.D0)
C       TKE = 166.1 + 0.021d0*(ftmp-1332.d0)
        TKE = TKE + 0.37*en
        RETURN
      ENDIF
C
C     Itkis
      IF(Izf.EQ.92)THEN   ! Uranium
        TKE = 0.104*ftmp + 27.35
        TKE = TKE + 0.4*en
        RETURN
      ENDIF
 
      RETURN
      END FUNCTION TKE
 
!---------------------------------------------------------------------------
 
      FUNCTION FWATT(E,Eniu,T)
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      REAL*8 :: E, Eniu, T
      REAL*8 :: FWATT
C
C Local variables
C
      REAL*8 :: b, pi
      REAL*8 :: DEXP, DSQRT
C
C*** End of declarations rewritten by SPAG
C
      DATA pi/3.1415926D0/
      FWATT = 0.D0
      IF(E.LE.0.1D-5)E = 0.1D-5
      IF(T.LE.0.1D-5)RETURN
      IF((E + Eniu).GT.50.D0*T)RETURN
      b = 4*Eniu/T/T
      FWATT = 2.D0/T*DSQRT(E/(pi*T))*DEXP( - (E + Eniu)/T)
     &        *SINH(DSQRT(b*E))/DSQRT(b*E)
      RETURN
      END FUNCTION FWATT
 
!---------------------------------------------------------------------------
 
      FUNCTION FMAXW(E,T)
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      REAL*8 :: E, T
      REAL*8 :: FMAXW
C
C Local variables
C
      REAL*8 :: DEXP, DSQRT
      REAL*8 :: pi
C
C*** End of declarations rewritten by SPAG
C
      DATA pi/3.1415926D0/
      FMAXW = 0.D0
      IF(E.LE.0.1D-5)E = 0.1D-5
      IF(T.LE.0.1D-5)RETURN
      IF(E.GT.50.D0*T)RETURN
      FMAXW = 2.D0/T*DSQRT(E/(pi*T))*DEXP( - E/T)
      RETURN
      END FUNCTION FMAXW
 
!---------------------------------------------------------------------------
 
      FUNCTION FSCN(E,T)
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      REAL*8 :: E, T
      REAL*8 :: FSCN
C
C Local variables
C
      REAL*8 :: DEXP
C
C*** End of declarations rewritten by SPAG
C
      FSCN = 0.D0
      IF(E.LE.0.1D-5)E = 0.1D-5
      IF(T.LE.0.1D-5)RETURN
      IF(E.GT.50.D0*T)RETURN
      FSCN = 1.D0/T*(E/T)*DEXP( - E/T)
      RETURN
      END FUNCTION FSCN
 
!---------------------------------------------------------------------------
 
      FUNCTION BIND(Nn,Nz,Exc)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: AMPi, AMUele, AMUmev, AMUneu, AMUpro, CETa, CSO, ELE2, 
     &          HHBarc, PI
      REAL*8, DIMENSION(0:130,0:400) :: EXCessmass, RESmas
      COMMON /CONSTANT/ AMUmev, PI, CETa, CSO, AMPi, ELE2, HHBarc, 
     &                  AMUneu, AMUpro, AMUele
      COMMON /XMASS / EXCessmass, RESmas
C
C Dummy arguments
C
      REAL*8 :: Exc
      INTEGER :: Nn, Nz
      REAL*8 :: BIND
C
C Local variables
C
      REAL*8 :: DBLE
C
C*** End of declarations rewritten by SPAG
C
C     real*4 e  ! for mass10 CALL
C--------------------------------------------------------------------c
C                                                                    c
C       J. Duflo and A.P. Zuker  Feb 23, 1996  10 parameters formula c
C       Reference:                                                   c
C             Phys. Rev. C52, 1995  (for the 28 param. formula)      c
C             and Private Communication to AMDC, February 1996.      c
C                                                                    c
C       Microscopic   calculation   of   nuclear  masses   with   10 c
C       parameters.  Fit  to the 1810 measured  masses from Helium-4 c
C       and up with a root-mean-square deviation (rms) of 506 keV.   c
C--------------------------------------------------------------------c
C     call mass10(nn,nz,e)
C     exc=nz*7.28903+nn*8.07138-e
C     bind = dble(e)
 
C     Rounded values
C
C mn    neutron mass  1.008 665 amu
C me    electron mass 5.485 799�10-4 amu
C mp    proton mass   1.007 276 amu
C md    deuteron mass 2.013 553 amu 1
C mt    triton mass   3.015 501 amu 3
C m3He  3He mass      3.014 932 amu 1
C ma    4He mass      4.001 506 amu 1
 
 
      Exc = EXCessmass(Nz,Nz + Nn)
C     bind = dble(nz*7.28903+nn*8.07138 - exc)
C     Corrected values
 
      BIND = DBLE(Nz*(AMUpro + AMUele) + Nn*AMUneu - Exc)
C     print *, ' N=',nn,' Z=',nz,' B.E.=',e,' MassExcess=', exc
      RETURN
      END FUNCTION BIND
 
!---------------------------------------------------------------------------
 
      SUBROUTINE MASS10(Nx,Nz,E)     ! Duflo-Zuker fevrier 1996
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      REAL*8 :: E
      INTEGER :: Nx, Nz
C
C Local variables
C
      REAL :: a, de, den, fact, pi, px, r, ra, rc, t, vm, z2
      REAL, DIMENSION(10) :: b, dyda
      REAL, DIMENSION(2) :: dei, dx, oei, op, os, pp, qx, y
      INTEGER :: i, i2, id, imax, ip, ipm, j, ju, k, kk, l, moc, mss, 
     &           ncum, ndef
      INTEGER, DIMENSION(2) :: n2, nn
      INTEGER, DIMENSION(18,2) :: noc
      REAL, DIMENSION(0:8,2,2) :: onp
C
C*** End of declarations rewritten by SPAG
C
C Calculation of binding energy E (nx neutrons,nz protons)
      DATA b/0.7043, 17.7418, 16.2562, 37.5562, 53.9017, 0.4711, 2.1307, 
     &     0.0210, 40.5356, 6.0632/
C*********
      nn(1) = Nx
      nn(2) = Nz
      a = Nx + Nz
      t = ABS(Nx - Nz)
      r = a**(1./3.)
      rc = r*(1. - .25*(t/a)**2)   !      Charge radius
      ra = (rc*rc)/r
C--------
      z2 = Nz*(Nz - 1)
      dyda(1) = ( - z2 + .76*z2**(2./3.))/rc
                                        ! Coulomb energy
C********                          ! beginning of main loop
      DO ndef = 1, 2               !      ndef=1  spherical
        ju = 0                     !      ndef=2  deformed
        y(ndef) = 0.
        IF(ndef.EQ.2)ju = 4        !      nucleons associated to deform.
        DO kk = 2, 10
          dyda(kk) = 0.
        ENDDO
C--------                          ! beginning of loop over N and Z
        DO j = 1, 2
          DO l = 1, 18
            noc(l,j) = 0
          ENDDO
          DO l = 1, 2
            DO k = 0, 8
              onp(k,l,j) = 0.
            ENDDO
          ENDDO
          n2(j) = 2*(nn(j)/2)      !      (for pairing calculation)
          ncum = 0
          i = 0
C--------
    5     i = i + 1                !     sub-shells (ssh) j and r filling
          i2 = (i/2)*2
          IF(i2.NE.i)THEN
            id = i + 1             !             for ssh j
          ELSE
            id = i*(i - 2)/4       !             for ssc r
          ENDIF
          ncum = ncum + id
          IF(ncum.LT.nn(j))THEN
            noc(i,j) = id          !     nb of nucleons in each ssh
            GOTO 5
          ENDIF
C--------
          imax = i + 1             !     imax = last subshell nb
          ip = (i - 1)/2           !     HO number (p)
          ipm = i/2
          pp(j) = ip
          moc = nn(j) - ncum + id
          noc(i,j) = moc - ju      !     nb of nucleons in last ssh
          noc(i + 1,j) = ju
          IF(i2.NE.i)THEN          !     ssh j
            oei(j) = moc + ip*(ip - 1)
                                   !       nb of nucleons in last EI shell
            dei(j) = ip*(ip + 1) + 2
                                   !       size of the EI shell
          ELSE                     !     ssh r
            oei(j) = moc - ju      !       nb of nucleons in last EI shell
            dei(j) = (ip + 1)*(ip + 2) + 2
                                   !       size of the EI shell
          ENDIF
          qx(j) = oei(j)*(dei(j) - oei(j) - ju)/dei(j)
                                                ! n*(D-n)/D        S3(j)
          dx(j) = qx(j)*(2*oei(j) - dei(j))     ! n*(D-n)*(2n-D)/D  Q
          IF(ndef.EQ.2)qx(j) = qx(j)/SQRT(dei(j))
                                                ! scaling for deformed
C--------
          DO i = 1, imax                        ! Amplitudes
            ip = (i - 1)/2
            fact = SQRT((ip + 1.)*(ip + 2.))
            onp(ip,1,j) = onp(ip,1,j) + noc(i,j)/fact
                                                !    for FM term
            vm = -1.
            IF((2*(i/2)).NE.i)vm = .5*ip        !    for spin-orbit term
            onp(ip,2,j) = onp(ip,2,j) + noc(i,j)*vm
          ENDDO
C--------
          op(j) = 0.
          os(j) = 0.
          DO ip = 0, ipm           !       FM and SO terms
            pi = ip
            den = ((pi + 1)*(pi + 2))**(3./2.)
            op(j) = op(j) + onp(ip,1,j)                          ! FM
            os(j) = os(j) + onp(ip,2,j)*(1. + onp(ip,1,j))*(pi*pi/den)
     &              + onp(ip,2,j)*(1. - onp(ip,1,j))*((4*pi - 5)/den)
                                                                 ! SO
          ENDDO
          op(j) = op(j)*op(j)
        ENDDO
C--------                          ! end of loop over  N and Z
        dyda(2) = op(1) + op(2)           !   Master term (FM): volume
        dyda(3) = -dyda(2)/ra             !                     surface
        dyda(2) = dyda(2) + os(1) + os(2) !   FM + SO
        dyda(4) = -t*(t + 2)/(r*r)        !   isospin term : volume
        dyda(5) = -dyda(4)/ra             !                : surface
        IF(ndef.EQ.1)THEN                 ! sph.
          dyda(6) = dx(1) + dx(2)         !   S3  volume
          dyda(7) = -dyda(6)/ra           !       surface
          px = SQRT(pp(1)) + SQRT(pp(2))
          dyda(8) = qx(1)*qx(2)*(2**px)   !   QQ sph.
        ELSE                              ! def.
          dyda(9) = qx(1)*qx(2)           !   QQ deform.
        ENDIF
        dyda(5) = t*(1 - t)/(a*ra**3) + dyda(5)
                                          !   "Wigner term"
C--------                                 !   PAIRING
        IF(n2(1).NE.nn(1).AND.n2(2).NE.nn(2))dyda(10) = t/a
        IF(Nx.GT.Nz)THEN
          IF(n2(1).EQ.nn(1).AND.n2(2).NE.nn(2))dyda(10) = 1 - t/a
          IF(n2(1).NE.nn(1).AND.n2(2).EQ.nn(2))dyda(10) = 1
        ELSE
          IF(n2(1).EQ.nn(1).AND.n2(2).NE.nn(2))dyda(10) = 1
          IF(n2(1).NE.nn(1).AND.n2(2).EQ.nn(2))dyda(10) = 1 - t/a
        ENDIF
        IF(n2(2).EQ.nn(2).AND.n2(1).EQ.nn(1))dyda(10) = 2 - t/a
C--------
        DO mss = 2, 10
          dyda(mss) = dyda(mss)/ra
        ENDDO
        DO mss = 1, 10
          y(ndef) = y(ndef) + dyda(mss)*b(mss)
        ENDDO
C--------                            ! end of main loop
      ENDDO
      de = y(2) - y(1)
      E = y(2)                       ! Binding Energy for def. nuclides
      IF(de.LE.0..OR.Nz.LE.50)E = y(1)
                                     !                spherical nuclides
      RETURN
      END SUBROUTINE MASS10
 
!---------------------------------------------------------------------------
 
      REAL*8 FUNCTION GAUSS_LAGUERRE_INT(F,Abserr)
      IMPLICIT NONE
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      REAL*8 :: Abserr
      REAL*8, EXTERNAL :: F
C
C Local variables
C
      INTEGER :: j
      REAL*8 :: resk08, resk10
      REAL*8, DIMENSION(10) :: wg08, wg10, xg08, xg10
C
C*** End of declarations rewritten by SPAG
C
C
C     THE ABSCISSAE AND WEIGHTS ARE GIVEN FOR THE INTERVAL (0,INF).
C
C     GAUSS LAGUERRE values for n=10 POINTS
C
C1     0.13779347054     0.308441115765
C2     0.729454549503    0.401119929155
C3     1.80834290174     0.218068287612
C4     3.40143369785     0.0620874560987
C5     5.55249614006     0.00950151697517
C6     8.33015274676     0.000753008388588
C7     11.8437858379     2.82592334963E-005
C8     16.2792578314     4.24931398502E-007
C9     21.996585812      1.83956482398E-009
C10    29.9206970123     9.91182721958E-013
C
      DATA wg10(1)/0.308441115765D0/
      DATA wg10(2)/0.401119929155D0/
      DATA wg10(3)/0.218068287612D0/
      DATA wg10(4)/0.0620874560987D0/
      DATA wg10(5)/0.00950151697517D0/
      DATA wg10(6)/0.000753008388588D0/
      DATA wg10(7)/2.82592334963D-05/
      DATA wg10(8)/4.24931398502D-07/
      DATA wg10(9)/1.83956482398D-009/
      DATA wg10(10)/9.91182721958D-013/
 
      DATA xg10(1)/0.13779347054D0/
      DATA xg10(2)/0.729454549503D0/
      DATA xg10(3)/1.80834290174D0/
      DATA xg10(4)/3.40143369785D0/
      DATA xg10(5)/5.55249614006D0/
      DATA xg10(6)/8.33015274676D0/
      DATA xg10(7)/11.8437858379D0/
      DATA xg10(8)/16.2792578314D0/
      DATA xg10(9)/21.996585812D0/
      DATA xg10(10)/29.9206970123D0/
C
C     GAUSS LAGUERRE values for n=8 POINTS
C
C1    0.170279632305    0.369188589342
C2    0.903701776799    0.418786780814
C3    2.25108662987     0.175794986637
C4    4.26670017029     0.0333434922612
C5    7.04590540239     0.00279453623523
C6    10.7585160102     9.07650877338E-005
C7    15.7406786413     8.48574671626E-007
C8    22.8631317369     1.04800117487E-009
C
      DATA wg08(1)/0.369188589342D0/
      DATA wg08(2)/0.418786780814D0/
      DATA wg08(3)/0.175794986637D0/
      DATA wg08(4)/0.0333434922612D0/
      DATA wg08(5)/0.00279453623523D0/
      DATA wg08(6)/9.07650877338D-05/
      DATA wg08(7)/8.48574671626D-07/
      DATA wg08(8)/1.04800117487D-09/
C
      DATA xg08(1)/0.170279632305D0/
      DATA xg08(2)/0.903701776799D0/
      DATA xg08(3)/2.25108662987D0/
      DATA xg08(4)/4.26670017029D0/
      DATA xg08(5)/7.04590540239D0/
      DATA xg08(6)/10.7585160102D0/
      DATA xg08(7)/15.7406786413D0/
      DATA xg08(8)/22.8631317369D0/
C
C
      resk08 = 0.D0
      DO j = 1, 8
        resk08 = resk08 + wg08(j)*F(xg08(j))
      ENDDO
 
      resk10 = 0.D0
      DO j = 1, 10
        resk10 = resk10 + wg10(j)*F(xg10(j))
      ENDDO
 
      GAUSS_LAGUERRE_INT = resk10
      Abserr = ABS(resk10 - resk08)
 
      RETURN
      END FUNCTION GAUSS_LAGUERRE_INT
 
!---------------------------------------------------------------------------
 
      FUNCTION SL_LAB(T)
      IMPLICIT NONE
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: EMIn, EPLus, TT
      COMMON /EPARAM/ EPLus, EMIn
      COMMON /TPARAM/ TT
C
C Dummy arguments
C
      REAL*8 :: T
      REAL*8 :: SL_LAB
C
C Local variables
C
      REAL*8 :: abserr, ftmp1, ftmp2
      REAL*8, EXTERNAL :: FKLT, GAUSS_INT1, GAUSS_LAGUERRE_INT, SKLT
C
C*** End of declarations rewritten by SPAG
C
      TT = T
 
      SL_LAB = 0.D0
      ftmp1 = GAUSS_LAGUERRE_INT(FKLT,abserr) ! eq.7 for normalization
      IF(ftmp1.EQ.0.D0)RETURN
 
      ftmp2 = GAUSS_INT1(SKLT,EMIn,EPLus,abserr)
 
      SL_LAB = T*ftmp2/ftmp1
 
      RETURN
      END FUNCTION SL_LAB
 
!---------------------------------------------------------------------------
 
      FUNCTION SH_LAB(T)
      IMPLICIT NONE
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: EMIn, EPLus, TT
      COMMON /EPARAM/ EPLus, EMIn
      COMMON /TPARAM/ TT
C
C Dummy arguments
C
      REAL*8 :: T
      REAL*8 :: SH_LAB
C
C Local variables
C
      REAL*8 :: abserr, ftmp1, ftmp2
      REAL*8, EXTERNAL :: FKHT, GAUSS_INT1, GAUSS_LAGUERRE_INT, SKHT
C
C*** End of declarations rewritten by SPAG
C
      TT = T
 
      SH_LAB = 0.D0
      ftmp1 = GAUSS_LAGUERRE_INT(FKHT,abserr)     ! eq.7 for normalization
      IF(ftmp1.EQ.0.D0)RETURN
 
      ftmp2 = GAUSS_INT1(SKHT,EMIn,EPLus,abserr)
C     write(*,*) sngl(ftmp2),sngl(abserr/ftmp2)
      SH_LAB = T*ftmp2/ftmp1
 
      RETURN
      END FUNCTION SH_LAB
 
!---------------------------------------------------------------------------
 
      FUNCTION FEPSL(T)
      IMPLICIT NONE
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: TT
      COMMON /TPARAM/ TT
C
C Dummy arguments
C
      REAL*8 :: T
      REAL*8 :: FEPSL
C
C Local variables
C
      REAL*8 :: abserr, ftmp1, ftmp2
      REAL*8, EXTERNAL :: EKLT, FKLT, GAUSS_LAGUERRE_INT
C
C*** End of declarations rewritten by SPAG
C
      TT = T
 
      FEPSL = 0.D0
      ftmp1 = GAUSS_LAGUERRE_INT(FKLT,abserr)     ! eq.7 for normalization
      IF(ftmp1.EQ.0.D0)RETURN
 
      ftmp2 = GAUSS_LAGUERRE_INT(EKLT,abserr) ! eq.11 for light, integrand
      FEPSL = ftmp2/ftmp1
 
      RETURN
      END FUNCTION FEPSL
 
!---------------------------------------------------------------------------
 
      FUNCTION FEPSH(T)
      IMPLICIT NONE
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: TT
      COMMON /TPARAM/ TT
C
C Dummy arguments
C
      REAL*8 :: T
      REAL*8 :: FEPSH
C
C Local variables
C
      REAL*8 :: abserr, ftmp1, ftmp2
      REAL*8, EXTERNAL :: EKHT, FKHT, GAUSS_LAGUERRE_INT
C
C*** End of declarations rewritten by SPAG
C
      TT = T
 
      FEPSH = 0.D0
      ftmp1 = GAUSS_LAGUERRE_INT(FKHT,abserr)     ! eq.7 for normalization
      IF(ftmp1.EQ.0.D0)RETURN
 
      ftmp2 = GAUSS_LAGUERRE_INT(EKHT,abserr) ! eq.11 for light, integrand
      FEPSH = ftmp2/ftmp1
 
      RETURN
      END FUNCTION FEPSH
 
!---------------------------------------------------------------------------
 
      FUNCTION FKLT(X)
      IMPLICIT NONE
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: T
      COMMON /TPARAM/ T
C
C Dummy arguments
C
      REAL*8 :: X
      REAL*8 :: FKLT
C
C Local variables
C
      REAL*8 :: sigc, xx
C
C*** End of declarations rewritten by SPAG
C
      xx = T*X
      CALL INTERPOL1(1,xx,sigc)
      FKLT = sigc*X*T*T
      RETURN
      END FUNCTION FKLT
 
!---------------------------------------------------------------------------
 
      FUNCTION FKHT(X)
      IMPLICIT NONE
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: T
      COMMON /TPARAM/ T
C
C Dummy arguments
C
      REAL*8 :: X
      REAL*8 :: FKHT
C
C Local variables
C
      REAL*8 :: sigc, xx
C
C*** End of declarations rewritten by SPAG
C
      xx = T*X
      CALL INTERPOL1(2,xx,sigc)
      FKHT = sigc*X*T*T
      RETURN
      END FUNCTION FKHT
 
!---------------------------------------------------------------------------
 
      FUNCTION EKLT(X)
      IMPLICIT NONE
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: T
      COMMON /TPARAM/ T
C
C Dummy arguments
C
      REAL*8 :: X
      REAL*8 :: EKLT
C
C Local variables
C
      REAL*8 :: sigc, xx
C
C*** End of declarations rewritten by SPAG
C
      xx = T*X
      CALL INTERPOL1(1,xx,sigc)
      EKLT = sigc*X*X*T**4
      RETURN
      END FUNCTION EKLT
 
!---------------------------------------------------------------------------
 
      FUNCTION EKHT(X)
      IMPLICIT NONE
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: T
      COMMON /TPARAM/ T
C
C Dummy arguments
C
      REAL*8 :: X
      REAL*8 :: EKHT
C
C Local variables
C
      REAL*8 :: sigc, xx
C
C*** End of declarations rewritten by SPAG
C
      xx = T*X
      CALL INTERPOL1(2,xx,sigc)
      EKHT = sigc*X*X*T**4
      RETURN
      END FUNCTION EKHT
 
!---------------------------------------------------------------------------
 
      FUNCTION SKLT(X)
      IMPLICIT NONE
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: T
      COMMON /TPARAM/ T
C
C Dummy arguments
C
      REAL*8 :: X
      REAL*8 :: SKLT
C
C Local variables
C
      REAL*8 :: DEXP, DSQRT
      REAL*8 :: sigc, xx
C
C*** End of declarations rewritten by SPAG
C
      SKLT = 0.D0
      xx = X/T
      IF(xx.GT.30.D0)RETURN
      CALL INTERPOL1(1,X,sigc)
      SKLT = DEXP( - xx)*sigc*DSQRT(X)
      RETURN
      END FUNCTION SKLT
 
!---------------------------------------------------------------------------
 
      FUNCTION SKHT(X)
      IMPLICIT NONE
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: T
      COMMON /TPARAM/ T
C
C Dummy arguments
C
      REAL*8 :: X
      REAL*8 :: SKHT
C
C Local variables
C
      REAL*8 :: DEXP, DSQRT
      REAL*8 :: sigc, xx
C
C*** End of declarations rewritten by SPAG
C
      SKHT = 0.D0
      xx = X/T
      IF(xx.GT.30.D0)RETURN
      CALL INTERPOL1(2,X,sigc)
      SKHT = DEXP( - xx)*sigc*DSQRT(X)
      RETURN
      END FUNCTION SKHT
 
!---------------------------------------------------------------------------
 
C     REAL*8 FUNCTION FFKT(x)
C     REAL*8 xx, x, sigc
C     COMMON /tparam/T
C     xx = T*x
C     CALL INTERPOL1(3,xx,sigc)
C     FFKT = sigc*xx*tres
C     RETURN
C     END
 
 
C====================================================================
      SUBROUTINE INTERPOL1(Iopt,X,Sigc)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8, DIMENSION(500) :: EEH, EEL, EENc, SIGh, SIGl, SIGncf
      INTEGER :: NRH, NRL, NRNc
      COMMON /INP_SP5/ EENc, SIGncf, NRNc
      COMMON /SIGLH / NRL, NRH, EEL, SIGl, EEH, SIGh
C
C Dummy arguments
C
      INTEGER :: Iopt
      REAL*8 :: Sigc, X
C
C Local variables
C
      REAL :: aa, b
      REAL*8, DIMENSION(500) :: ee, ss
      INTEGER :: i, j, nrc
C
C*** End of declarations rewritten by SPAG
C
C=========================================
 
 
 
C
      IF(Iopt.EQ.1)THEN
        nrc = NRL
        DO i = 1, nrc
          ee(i) = EEL(i)
          ss(i) = SIGl(i)
        ENDDO
      ENDIF
C
      IF(Iopt.EQ.2)THEN
        nrc = NRH
        DO i = 1, nrc
          ee(i) = EEH(i)
          ss(i) = SIGh(i)
        ENDDO
      ENDIF
C
      IF(Iopt.EQ.3.OR.Iopt.EQ.4)THEN
        nrc = NRNc
        DO i = 1, nrc
          ee(i) = EENc(i)
          ss(i) = SIGncf(i)
        ENDDO
      ENDIF
 
      IF(X.LT.ee(1))THEN
        Sigc = ss(1)
        RETURN
      ENDIF
 
      IF(X.GT.ee(nrc))THEN
        Sigc = ss(nrc)
        RETURN
      ENDIF
C
C     interpolation lineaire
C
      DO j = 1, nrc - 1
        IF(X.GE.ee(j))THEN
          IF(X.LE.ee(j + 1))THEN
            aa = (ss(j + 1) - ss(j))/(ee(j + 1) - ee(j))
            b = ss(j) - aa*ee(j)
            Sigc = aa*X + b
            RETURN
          ENDIF
        ENDIF
      ENDDO
      RETURN
      END SUBROUTINE INTERPOL1
 
!---------------------------------------------------------------------------
 
C=========================================
      SUBROUTINE INPUT_SPEC
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8, DIMENSION(500) :: EEHh, EELl, EENc, SIGhh, SIGll, SIGncf
      CHARACTER(64) :: EMPiredir
      CHARACTER(72) :: EMPtitle
      INTEGER :: NRHh, NRLl, NRNc
      COMMON /GLOBAL_E/ EMPiredir, EMPtitle
      COMMON /INP_SP5/ EENc, SIGncf, NRNc
      COMMON /SIGLH / NRLl, NRHh, EELl, SIGll, EEHh, SIGhh
C
C Local variables
C
      INTEGER :: i
C
C*** End of declarations rewritten by SPAG
C
C=========================================
 
 
 
      OPEN(UNIT = 23,FILE = TRIM(EMPiredir)//'/data/CNxs.dat',
     &     STATUS = 'old',ERR = 10)
      READ(23,*)NRNc
      DO i = 1, NRNc
        READ(23,*)EENc(i), SIGncf(i)
C              write(*,*)eenc(i),signcf(i)
      ENDDO
      CLOSE(23)
 
      OPEN(UNIT = 23,FILE = TRIM(EMPiredir)//'/data/LFxs.dat',
     &     STATUS = 'old',ERR = 20)
      READ(23,*)NRLl
      DO i = 1, NRLl
        READ(23,*)EELl(i), SIGll(i)
C              write(*,*)eell(i),sigll(i)
      ENDDO
      CLOSE(23)
      OPEN(UNIT = 23,FILE = TRIM(EMPiredir)//'/data/HFxs.dat',
     &     STATUS = 'old',ERR = 30)
      READ(23,*)NRHh
      DO i = 1, NRHh
        READ(23,*)EEHh(i), SIGhh(i)
C              write(*,*)eehh(i),sighh(i)
      ENDDO
      CLOSE(23)
      RETURN
   10 WRITE(8,*)'ERROR: ../data/CNxs.data not found !'
      STOP 'ERROR: ../data/CNxs.data not found !'
   20 WRITE(8,*)'ERROR: ../data/LFxs.data not found !'
      STOP 'ERROR: ../data/LFxs.data not found !'
   30 WRITE(8,*)'ERROR: ../data/HFxs.data not found !'
      STOP 'ERROR: ../data/HFxs.data not found !'
      END SUBROUTINE INPUT_SPEC
 
!---------------------------------------------------------------------------
 
      FUNCTION GAUSS_INT1(F,Ea,Eb,Abserr)
      IMPLICIT NONE
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      REAL*8 :: Abserr, Ea, Eb
      REAL*8 :: F
      REAL*8 :: GAUSS_INT1
C
C Local variables
C
      REAL*8 :: absc, abscm1, centr1, fsum, fval1, fval1m1, fval2, 
     &          fval2m1, hlgth1, resg1, resk1
      INTEGER :: j, jtw, jtwm1
      REAL*8, DIMENSION(10) :: wg
      REAL*8, DIMENSION(21) :: wgk, xgk
C
C*** End of declarations rewritten by SPAG
C
C
C     EXTERNAL F
C
C
C
C     THE ABSCISSAE AND WEIGHTS ARE GIVEN FOR THE INTERVAL (-1,1).
C     BECAUSE OF SYMMETRY ONLY THE POSITIVE ABSCISSAE AND THEIR
C     CORRESPONDING WEIGHTS ARE GIVEN.
C
C     XG - ABSCISSAE OF THE 41-POINT GAUSS-KRONROD RULE
C     WG - WEIGHTS OF THE 20-POINT GAUSS RULE
C
C GAUSS QUADRATURE WEIGHTS AND KRONROD QUADRATURE ABSCISSAE AND WEIGHTS
C AS EVALUATED WITH 80 DECIMAL DIGIT ARITHMETIC BY L. W. FULLERTON,
C BELL LABS, NOV. 1981.
C
      DATA wg(1)/0.017614007139152118311861962351853D0/
      DATA wg(2)/0.040601429800386941331039952274932D0/
      DATA wg(3)/0.062672048334109063569506535187042D0/
      DATA wg(4)/0.083276741576704748724758143222046D0/
      DATA wg(5)/0.101930119817240435036750135480350D0/
      DATA wg(6)/0.118194531961518417312377377711382D0/
      DATA wg(7)/0.131688638449176626898494499748163D0/
      DATA wg(8)/0.142096109318382051329298325067165D0/
      DATA wg(9)/0.149172986472603746787828737001969D0/
      DATA wg(10)/0.152753387130725850698084331955098D0/
C
      DATA xgk(1)/0.998859031588277663838315576545863D0/
      DATA xgk(2)/0.993128599185094924786122388471320D0/
      DATA xgk(3)/0.981507877450250259193342994720217D0/
      DATA xgk(4)/0.963971927277913791267666131197277D0/
      DATA xgk(5)/0.940822633831754753519982722212443D0/
      DATA xgk(6)/0.912234428251325905867752441203298D0/
      DATA xgk(7)/0.878276811252281976077442995113078D0/
      DATA xgk(8)/0.839116971822218823394529061701521D0/
      DATA xgk(9)/0.795041428837551198350638833272788D0/
      DATA xgk(10)/0.746331906460150792614305070355642D0/
      DATA xgk(11)/0.693237656334751384805490711845932D0/
      DATA xgk(12)/0.636053680726515025452836696226286D0/
      DATA xgk(13)/0.575140446819710315342946036586425D0/
      DATA xgk(14)/0.510867001950827098004364050955251D0/
      DATA xgk(15)/0.443593175238725103199992213492640D0/
      DATA xgk(16)/0.373706088715419560672548177024927D0/
      DATA xgk(17)/0.301627868114913004320555356858592D0/
      DATA xgk(18)/0.227785851141645078080496195368575D0/
      DATA xgk(19)/0.152605465240922675505220241022678D0/
      DATA xgk(20)/0.076526521133497333754640409398838D0/
      DATA xgk(21)/0.000000000000000000000000000000000D0/
C
      DATA wgk(1)/0.003073583718520531501218293246031D0/
      DATA wgk(2)/0.008600269855642942198661787950102D0/
      DATA wgk(3)/0.014626169256971252983787960308868D0/
      DATA wgk(4)/0.020388373461266523598010231432755D0/
      DATA wgk(5)/0.025882133604951158834505067096153D0/
      DATA wgk(6)/0.031287306777032798958543119323801D0/
      DATA wgk(7)/0.036600169758200798030557240707211D0/
      DATA wgk(8)/0.041668873327973686263788305936895D0/
      DATA wgk(9)/0.046434821867497674720231880926108D0/
      DATA wgk(10)/0.050944573923728691932707670050345D0/
      DATA wgk(11)/0.055195105348285994744832372419777D0/
      DATA wgk(12)/0.059111400880639572374967220648594D0/
      DATA wgk(13)/0.062653237554781168025870122174255D0/
      DATA wgk(14)/0.065834597133618422111563556969398D0/
      DATA wgk(15)/0.068648672928521619345623411885368D0/
      DATA wgk(16)/0.071054423553444068305790361723210D0/
      DATA wgk(17)/0.073030690332786667495189417658913D0/
      DATA wgk(18)/0.074582875400499188986581418362488D0/
      DATA wgk(19)/0.075704497684556674659542775376617D0/
      DATA wgk(20)/0.076377867672080736705502835038061D0/
      DATA wgk(21)/0.076600711917999656445049901530102D0/
C
C     Integrating from Ea to Eint
      centr1 = 0.5D+00*(Ea + Eb)
      hlgth1 = 0.5D+00*(Eb - Ea)
C
C     COMPUTE THE 41-POINT GAUSS-KRONROD APPROXIMATION TO
C     THE INTEGRAL, AND ESTIMATE THE ABSOLUTE ERROR.
C
      resg1 = 0.0D+00
      resk1 = wgk(21)*F(centr1)
      DO j = 1, 10
        jtw = j*2
        jtwm1 = jtw - 1
        absc = hlgth1*xgk(jtw)
        fval1 = F(centr1 - absc)
        fval2 = F(centr1 + absc)
        fsum = fval1 + fval2
        abscm1 = hlgth1*xgk(jtwm1)
        fval1m1 = F(centr1 - abscm1)
        fval2m1 = F(centr1 + abscm1)
        resg1 = resg1 + wg(j)*fsum
        resk1 = resk1 + wgk(jtw)*fsum + wgk(jtwm1)*(fval1m1 + fval2m1)
      ENDDO
      GAUSS_INT1 = resk1*hlgth1
      Abserr = ABS((resk1 - resg1)*hlgth1)
C     RETURN
      END FUNCTION GAUSS_INT1
 
!---------------------------------------------------------------------------
 
C*******************************************************************
C     NOT USED FOR THE TIME BEING
C     These functions could calculate NUBAR, they need to be tested
C
C     calculation of bn for all nuclei has to be imporved as done in GET_PFNS_...
C
      FUNCTION FNIULANL(En,Iaf,Izf)
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      REAL*8 :: En
      INTEGER :: Iaf, Izf
      REAL*8 :: FNIULANL
C
C Local variables
C
      REAL*8 :: abserr, bn, bnh, bnl, cndef, eg0, egn, ekin_ave, 
     &          eps, erel, erel1, erel2, erel3, erel4, erel5, fniua, 
     &          fniub, ftmp, ftmp1, ftmp2, hfdef, lfdef, p0, p1, p2, 
     &          pnorm, r, s2n1, s2n2, s2n3, s2n4, s2n5, sn, sn1, sn2, 
     &          sn3, sn4, sn5, tm, tmh, tml, uexcit, ux
      REAL*8 :: BIND, FEPSH, FEPSL, GAUSS_INT1, TKE
      REAL*8 :: DSQRT
      REAL :: FLOAT
      INTEGER :: iah, ial, izh, izh0, izl
C
C*** End of declarations rewritten by SPAG
C
C
C     See D. Madland original paper (NSE) on LA model
C
C     Following Malinovskii prescription for nubar calculation
C
 
 
 
C     Mass of heavy fragment fixed following Malinovskii
      DATA iah/140/
C     Average gamma energy release Eg = eg0 + niu*egn
      DATA eg0/4.42D0/, egn/0.99D0/
C     Ratio of the light to heavy fragments' Temperature taken from Kornilov
      DATA r/1.248D0/
C
C     Normal distribution
C     P(j)=1/sqrt(2*pi)*exp(-j^2/2) ; s^2=1.
C     P(0)=0.399   P(1)=P(-1)=0.242  P(-2)=0.054
C     Normalization factor:
C     P0=P(-1)+P(0)+P(1)+P(-2)=0.937
C     data p2/0.054d0/,p1/0.242d0/,p0/0.399d0/,pnorm/0.937d0/
C
C     Unik UCD distribution
C     P(j)=1/sqrt(c*pi)*exp(-j^2/c) ; c=2*(s^2 + 1/12); s^2=0.40 +/- 0.05
C     P(0)=0.574   P(1)=P(-1)=0.204 P(2)=P(-2)=0.009
C     Normalization factor:
C     P0=P(-1)+P(0)+P(1)=0.982 for three points
C     data p1/0.204d0/,p0/0.574d0/,pnorm/0.982d0/
C     P0=P(-2)+P(-1)+P(0)+P(1)=0.991 for four points
      DATA p2/0.009/, p1/0.204D0/, p0/0.574D0/, pnorm/0.991D0/
C     P0=P(-2)+P(-1)+P(0)+P(1)+P(2)=1.000 for five points
C     data p2/0.009/,p1/0.204d0/,p0/0.574d0/,pnorm/1.d0/
 
      FNIULANL = 0.D0
 
C     Following Vladuca
C     ftmp = real(izf)**2/real(iaf)
C     egn = 6.71 - 0.156*ftmp ! p
C     eg0 = 0.75 + 0.088*ftmp ! q
 
                                    ! get mass excess CNdef for fiss.nucleus
      bn = BIND(Iaf - Izf,Izf,cndef) - BIND(Iaf - Izf - 1,Izf,ftmp)
                                    ! get Bn of the neutron in fiss. nucleus
 
      uexcit = En + ABS(bn)
 
      ekin_ave = TKE(Izf,Iaf,En)
 
C----------------------------------
C     First fragment Ak (Reference)
C     Malinovskii parametrization of the heavy fragment charge
      izh0 = FLOAT(Izf)/FLOAT(Iaf)*iah - 0.5
      izh = izh0
      izl = Izf - izh
      ial = Iaf - iah
 
      bnh = BIND(iah - izh,izh,hfdef) - BIND(iah - izh - 1,izh,ftmp)
      bnl = BIND(ial - izl,izl,lfdef) - BIND(ial - izl - 1,izl,ftmp)
      sn1 = 0.5*(bnh + bnl)
      bnh = BIND(iah - izh,izh,hfdef) - BIND(iah - izh - 2,izh,ftmp)
      bnl = BIND(ial - izl,izl,lfdef) - BIND(ial - izl - 2,izl,ftmp)
      s2n1 = 0.5*(bnh + bnl)
C     Total energy release in fission from mass excess for CN, heavy and light fragments
      erel1 = cndef - hfdef - lfdef
C---------------------------
C     Second fragment Zh - 1
      izh = izh0 - 1
      izl = Izf - izh
      ial = Iaf - iah
      bnh = BIND(iah - izh,izh,hfdef) - BIND(iah - izh - 1,izh,ftmp)
      bnl = BIND(ial - izl,izl,lfdef) - BIND(ial - izl - 1,izl,ftmp)
      sn2 = 0.5*(bnh + bnl)
      bnh = BIND(iah - izh,izh,hfdef) - BIND(iah - izh - 2,izh,ftmp)
      bnl = BIND(ial - izl,izl,lfdef) - BIND(ial - izl - 2,izl,ftmp)
      s2n2 = 0.5*(bnh + bnl)
C     Total energy release in fission from mass excess for CN, heavy and light fragments
      erel2 = cndef - hfdef - lfdef
C---------------------------
C     Third fragment Zh + 1
      izh = izh0 + 1
      izl = Izf - izh
      ial = Iaf - iah
      bnh = BIND(iah - izh,izh,hfdef) - BIND(iah - izh - 1,izh,ftmp)
      bnl = BIND(ial - izl,izl,lfdef) - BIND(ial - izl - 1,izl,ftmp)
      sn3 = 0.5*(bnh + bnl)
      bnh = BIND(iah - izh,izh,hfdef) - BIND(iah - izh - 2,izh,ftmp)
      bnl = BIND(ial - izl,izl,lfdef) - BIND(ial - izl - 2,izl,ftmp)
      s2n3 = 0.5*(bnh + bnl)
C     Total energy release in fission from mass excess for CN, heavy and light fragments
      erel3 = cndef - hfdef - lfdef
C---------------------------
C     Fourth fragment Zh - 2
      izh = izh0 - 2
      izl = Izf - izh
      ial = Iaf - iah
      bnh = BIND(iah - izh,izh,hfdef) - BIND(iah - izh - 1,izh,ftmp)
      bnl = BIND(ial - izl,izl,lfdef) - BIND(ial - izl - 1,izl,ftmp)
      sn4 = 0.5*(bnh + bnl)
      bnh = BIND(iah - izh,izh,hfdef) - BIND(iah - izh - 2,izh,ftmp)
      bnl = BIND(ial - izl,izl,lfdef) - BIND(ial - izl - 2,izl,ftmp)
      s2n4 = 0.5*(bnh + bnl)
C     Total energy release in fission from mass excess for CN, heavy and light fragments
      erel4 = cndef - hfdef - lfdef
C---------------------------
C     Fifth fragment Zh + 2 (to average selecting either (Zh + 2) or (Zh - 2)
C     Only four fragments considered to avoid odd-even effect
      izh = izh0 + 2
      izl = Izf - izh
      ial = Iaf - iah
      bnh = BIND(iah - izh,izh,hfdef) - BIND(iah - izh - 1,izh,ftmp)
      bnl = BIND(ial - izl,izl,lfdef) - BIND(ial - izl - 1,izl,ftmp)
      sn5 = 0.5*(bnh + bnl)
      bnh = BIND(iah - izh,izh,hfdef) - BIND(iah - izh - 2,izh,ftmp)
      bnl = BIND(ial - izl,izl,lfdef) - BIND(ial - izl - 2,izl,ftmp)
      s2n5 = 0.5*(bnh + bnl)
C     Total energy release in fission from mass excess for CN, heavy and light fragments
      erel5 = cndef - hfdef - lfdef
 
C===========================================================================
C     FINAL AVERAGE
C
C     S1n = (Sn1*p0 + (Sn2 + Sn3)*p1 + Sn5*p2)/pnorm
C     S2n = (S2n1*p0 + (S2n2 + S2n3)*p1 + S2n4*p2)/pnorm
C     Sn = 0.5*( S1n + 0.5*S2n)
      sn = (sn1*p0 + (sn2 + sn3)*p1 + sn4*p2)/pnorm
 
      erel = (erel1*p0 + (erel2 + erel3)*p1 + erel4*p2)/pnorm
 
      ux = erel + uexcit - ekin_ave
      IF(ux.LT.0)RETURN
C
C     Level density assumed porportional to A/11
      tm = DSQRT(ux/(Iaf/11.D0))
C
C     LA model (eq.11 CPC)
      tml = tm*r
      tmh = tm
 
      ftmp1 = GAUSS_INT1(FEPSL,0.D0,tml,abserr)/(tml*tml)
C     write(*,*) sngl(TmL), sngl(abserr/ftmp1), sngl(ftmp1)
 
      ftmp2 = GAUSS_INT1(FEPSH,0.D0,tmh,abserr)/(tmh*tmh)
C     write(*,*) sngl(TmH), sngl(abserr/ftmp2), sngl(ftmp2)
 
C     The two lines below correspond to LA model with constant reaction cross section
C     eps   = 4./3.*dsqrt((Erel + uexcit - Ekin_ave)/(iaf/11.d0)) = 4/3*Tm
C     eps   = 4./3.*dsqrt((Erel + uexcit - Ekin_ave)/acc)
      eps = ftmp1 + ftmp2
 
C     Prompt gamma emission
C     Following Kornilov (instead of constant 4.42 according to Malinovskii)
C     eg0 = Sn*0.9
 
      fniua = (ux - eg0)/(eps + sn + egn)
 
      WRITE(*,
     &'('' Einc='',f6.3,'' Ux='',f6.3,'' Erel='',f5.1,          '' Ex=''
     &,f6.3,'' TKE='',f5.1,'' Bn='',f5.1)')En, ux, erel, uexcit, 
     &ekin_ave, bn
 
      WRITE(*,
     &'('' Eg='',f6.3,'' Enf='',f6.3,          '' Sn='',f6.3,'' nuA='',f
     &6.3,'' LANL'')')eg0, eps, sn, fniua
 
C     S1n = (Sn1*p0 + (Sn2 + Sn3)*p1 + Sn5*p2)/pnorm
C     S2n = (S2n1*p0 + (S2n2 + S2n3)*p1 + S2n5*p2)/pnorm
C     Sn = 0.5*( S1n + 0.5*S2n)
      sn = (sn1*p0 + (sn2 + sn3)*p1 + sn5*p2)/pnorm
 
      erel = (erel1*p0 + (erel2 + erel3)*p1 + erel5*p2)/pnorm
 
      ux = erel + uexcit - ekin_ave
      IF(ux.LT.0)RETURN
C
C     Level density assumed porportional to A/11
      tm = DSQRT(ux/(Iaf/11.D0))
C
C     LA model (eq.11 CPC)
      tml = tm*r
      tmh = tm
 
      ftmp1 = GAUSS_INT1(FEPSL,0.D0,tml,abserr)/(tml*tml)
C     write(*,*) sngl(TmL), sngl(abserr/ftmp1), sngl(ftmp1)
 
      ftmp2 = GAUSS_INT1(FEPSH,0.D0,tmh,abserr)/(tmh*tmh)
C     write(*,*) sngl(TmH), sngl(abserr/ftmp2), sngl(ftmp2)
 
C     The two lines below correspond to LA model with constant reaction cross section
C     eps   = 4./3.*dsqrt((Erel + uexcit - Ekin_ave)/(iaf/11.d0)) = 4/3*Tm
C     eps   = 4./3.*dsqrt((Erel + uexcit - Ekin_ave)/acc)
      eps = ftmp1 + ftmp2
 
      fniub = (ux - eg0)/(eps + sn + egn)
 
      WRITE(*,
     &'('' Einc='',f6.3,'' Ux='',f6.3,'' Erel='',f5.1,          '' Ex=''
     &,f6.3,'' TKE='',f5.1,'' Bn='',f5.1)')En, ux, erel, uexcit, 
     &ekin_ave, bn
 
      WRITE(*,
     &'('' Eg='',f6.3,'' Enf='',f6.3,          '' Sn='',f6.3,'' nuB='',f
     &6.3,'' LANL'')')eg0, eps, sn, fniub
 
      FNIULANL = 0.5*(fniua + fniub)
 
      RETURN
      END FUNCTION FNIULANL
 
!---------------------------------------------------------------------------
C
      FUNCTION FNIU(En,Iaf,Izf)
C
C     See N.V.Kornilov, A.B.Kagalenko and F.-J.Hambsch
C     Phys. At. Nuclei 62 (1999) pp 173-185
C
C     Following Malinovskii prescription for nubar calculation
C
      IMPLICIT NONE
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      REAL*8 :: En
      INTEGER :: Iaf, Izf
      REAL*8 :: FNIU
C
C Local variables
C
      REAL*8 :: abserr, bn, bnh, bnl, cndef, coeff, eg, eg0, egn, 
     &          ekin_ave, eps, erel, erel1, erel2, erel3, erel4, erel5, 
     &          fniua, fniub, ftmp, ftmp1, ftmp2, hfdef, lfdef, p0, p1, 
     &          p2, pnorm, r, sn, sn1, sn2, sn3, sn4, sn5, thcf, tlcf, 
     &          tmh, tml, u0cf, uexcit, ux
      REAL*8 :: BIND, FEPSH, FEPSL, GAUSS_INT1, TKE
      REAL*8 :: DSQRT
      REAL :: FLOAT
      INTEGER :: iah, ial, izh, izh0, izl
C
C*** End of declarations rewritten by SPAG
C
C     real*8 S2n1,S2n2,S2n3,S2n4,S2n5
C
 
C     Mass of heavy fragment fixed following Malinovskii
      DATA iah/140/
C     Average gamma energy release Eg = eg0 + niu*egn
C     Following Kornilov (instead of constant 4.42 according to Malinovskii)
C     eg0 = Sn*0.9
      DATA eg0/4.42D0/, egn/0.99D0/
C     Kornilov parameterization
      DATA thcf/0.8868D0/, r/1.248D0/, u0cf/32.9D0/
C
C     Normal distribution
C     P(j)=1/sqrt(2*pi)*exp(-j^2/2) ; s^2=1.
C     P(0)=0.399   P(1)=P(-1)=0.242  P(-2)=0.054
C     Normalization factor:
C     P0=P(-1)+P(0)+P(1)+P(-2)=0.937
C     data p2/0.054d0/,p1/0.242d0/,p0/0.399d0/,pnorm/0.937d0/
C
C     Unik UCD distribution
C     P(j)=1/sqrt(c*pi)*exp(-j^2/c) ; c=2*(s^2 + 1/12); s^2=0.40 +/- 0.05
C     P(0)=0.574   P(1)=P(-1)=0.204 P(2)=P(-2)=0.009
C     Normalization factor:
C     P0=P(-1)+P(0)+P(1)=0.982 for three points
C     data p1/0.204d0/,p0/0.574d0/,pnorm/0.982d0/
C     P0=P(-2)+P(-1)+P(0)+P(1)=0.991 for four points
      DATA p2/0.009/, p1/0.204D0/, p0/0.574D0/, pnorm/0.991D0/
C     P0=P(-2)+P(-1)+P(0)+P(1)+P(2)=1.000 for five points
C     data p2/0.009/,p1/0.204d0/,p0/0.574d0/,pnorm/1.d0/
 
C     Following Vladuca
C     ftmp = real(izf)**2/real(iaf)
C     egn = 6.71 - 0.156*ftmp ! p
C     eg0 = 0.75 + 0.088*ftmp ! q
 
      FNIU = 0.D0

      ! get mass excess CNdef for fiss.nucleus
      bn = BIND(Iaf - Izf,Izf,cndef) - BIND(Iaf - Izf - 1,Izf,ftmp)
      ! get Bn of the neutron in fiss. nucleus
 
      uexcit = En + bn
 
      ekin_ave = TKE(Izf,Iaf,En)
 
C----------------------------------
C     First fragment Ak (Reference)
C     Malinovskii parametrization of the heavy fragment charge
      izh0 = FLOAT(Izf)/FLOAT(Iaf)*iah - 0.5
      izh = izh0
      izl = Izf - izh
      ial = Iaf - iah
      bnh = BIND(iah - izh,izh,hfdef) - BIND(iah - izh - 1,izh,ftmp)
      bnl = BIND(ial - izl,izl,lfdef) - BIND(ial - izl - 1,izl,ftmp)
      sn1 = 0.5*(bnh + bnl)
      bnh = BIND(iah - izh,izh,hfdef) - BIND(iah - izh - 2,izh,ftmp)
      bnl = BIND(ial - izl,izl,lfdef) - BIND(ial - izl - 2,izl,ftmp)
C     S2n1 = 0.5*(Bnh+Bnl)
C     Total energy release in fission from mass excess for CN, heavy and light fragments
      erel1 = cndef - hfdef - lfdef
C---------------------------
C     Second fragment Zh - 1
      izh = izh0 - 1
      izl = Izf - izh
      ial = Iaf - iah
      bnh = BIND(iah - izh,izh,hfdef) - BIND(iah - izh - 1,izh,ftmp)
      bnl = BIND(ial - izl,izl,lfdef) - BIND(ial - izl - 1,izl,ftmp)
      sn2 = 0.5*(bnh + bnl)
      bnh = BIND(iah - izh,izh,hfdef) - BIND(iah - izh - 2,izh,ftmp)
      bnl = BIND(ial - izl,izl,lfdef) - BIND(ial - izl - 2,izl,ftmp)
C     S2n2 = 0.5*(Bnh+Bnl)
C     Total energy release in fission from mass excess for CN, heavy and light fragments
      erel2 = cndef - hfdef - lfdef
C---------------------------
C     Third fragment Zh + 1
      izh = izh0 + 1
      izl = Izf - izh
      ial = Iaf - iah
      bnh = BIND(iah - izh,izh,hfdef) - BIND(iah - izh - 1,izh,ftmp)
      bnl = BIND(ial - izl,izl,lfdef) - BIND(ial - izl - 1,izl,ftmp)
      sn3 = 0.5*(bnh + bnl)
      bnh = BIND(iah - izh,izh,hfdef) - BIND(iah - izh - 2,izh,ftmp)
      bnl = BIND(ial - izl,izl,lfdef) - BIND(ial - izl - 2,izl,ftmp)
C     S2n3 = 0.5*(Bnh+Bnl)
C     Total energy release in fission from mass excess for CN, heavy and light fragments
      erel3 = cndef - hfdef - lfdef
C---------------------------
C     Fourth fragment Zh - 2
      izh = izh0 - 2
      izl = Izf - izh
      ial = Iaf - iah
      bnh = BIND(iah - izh,izh,hfdef) - BIND(iah - izh - 1,izh,ftmp)
      bnl = BIND(ial - izl,izl,lfdef) - BIND(ial - izl - 1,izl,ftmp)
      sn4 = 0.5*(bnh + bnl)
      bnh = BIND(iah - izh,izh,hfdef) - BIND(iah - izh - 2,izh,ftmp)
      bnl = BIND(ial - izl,izl,lfdef) - BIND(ial - izl - 2,izl,ftmp)
C     S2n4 = 0.5*(Bnh+Bnl)
C     Total energy release in fission from mass excess for CN, heavy and light fragments
      erel4 = cndef - hfdef - lfdef
C---------------------------
C     Fifth fragment Zh + 2 (to avergae selecting either (Zh + 2) or (Zh - 2)
C     Only four fragments considered to avoid odd-even effect
      izh = izh0 + 2
      izl = Izf - izh
      ial = Iaf - iah
      bnh = BIND(iah - izh,izh,hfdef) - BIND(iah - izh - 1,izh,ftmp)
      bnl = BIND(ial - izl,izl,lfdef) - BIND(ial - izl - 1,izl,ftmp)
      sn5 = 0.5*(bnh + bnl)
      bnh = BIND(iah - izh,izh,hfdef) - BIND(iah - izh - 2,izh,ftmp)
      bnl = BIND(ial - izl,izl,lfdef) - BIND(ial - izl - 2,izl,ftmp)
C     S2n5 = 0.5*(Bnh+Bnl)
C     Total energy release in fission from mass excess for CN, heavy and light fragments
      erel5 = cndef - hfdef - lfdef
 
C===========================================================================
 
C     FINAL AVERAGE
C     S1n = (Sn1*p0 + (Sn2 + Sn3)*p1 + Sn4*p2)/pnorm
C     S2n = (S2n1*p0 + (S2n2 + S2n3)*p1 + S2n4*p2)/pnorm
C     Sn = 0.5*( S1n + 0.5*S2n)
 
      sn = (sn1*p0 + (sn2 + sn3)*p1 + sn4*p2)/pnorm
 
      erel = (erel1*p0 + (erel2 + erel3)*p1 + erel4*p2)/pnorm
 
      ux = erel + uexcit - ekin_ave
      IF(ux.LT.0)RETURN
 
C     Kornilov is based on Cf-252 PFNS fitting
      coeff = DSQRT(252.D0/Iaf*ux/u0cf)
C     Following formulae (7) of the paper
      tlcf = thcf*r
      tml = tlcf*coeff
      tmh = thcf*coeff
 
      ftmp1 = GAUSS_INT1(FEPSL,0.D0,tml,abserr)/(tml*tml)
C     write(*,*) sngl(TmL), sngl(abserr/ftmp1), sngl(ftmp1)
 
      ftmp2 = GAUSS_INT1(FEPSH,0.D0,tmh,abserr)/(tmh*tmh)
C     write(*,*) sngl(TmH), sngl(abserr/ftmp2), sngl(ftmp2)
 
      eps = ftmp1 + ftmp2
 
C     Following Kornilov (instead of constant 4.42 according to Malinovskii)
C     eg0 = Sn*0.9
 
      fniua = (ux - eg0)/(eps + sn + egn)
 
      eg = eg0 + fniua*egn
 
      WRITE(*,
     &'('' Einc='',f6.3,'' Ux='',f6.3,'' Erel='',f5.1,          '' Ex=''
     &,f6.3,'' TKE='',f5.1,'' Bn='',f5.1)')En, ux, erel, uexcit, 
     &ekin_ave, bn
 
      WRITE(*,
     &'('' Eg='',f6.3,'' Enf='',f6.3,          '' Sn='',f6.3,'' nuB='',f
     &6.3,'' LANL'')')eg, eps, ABS(sn), fniua
 
C     S1n = (Sn1*p0 + (Sn2 + Sn3)*p1 + Sn5*p2)/pnorm
C     S2n = (S2n1*p0 + (S2n2 + S2n3)*p1 + S2n5*p2)/pnorm
C     Sn = 0.5*( S1n + 0.5*S2n)
      sn = (sn1*p0 + (sn2 + sn3)*p1 + sn5*p2)/pnorm
 
      erel = (erel1*p0 + (erel2 + erel3)*p1 + erel5*p2)/pnorm
 
      ux = erel + uexcit - ekin_ave
      IF(ux.LT.0)RETURN
 
C     Kornilov is based on Cf-252 PFNS fitting
      coeff = DSQRT(252.D0/Iaf*ux/u0cf)
C     Following formulae (7) of the paper
      tlcf = thcf*r
      tml = tlcf*coeff
      tmh = thcf*coeff
 
      ftmp1 = GAUSS_INT1(FEPSL,0.D0,tml,abserr)/(tml*tml)
C     write(*,*) sngl(TmL), sngl(abserr/ftmp1), sngl(ftmp1)
 
      ftmp2 = GAUSS_INT1(FEPSH,0.D0,tmh,abserr)/(tmh*tmh)
C     write(*,*) sngl(TmH), sngl(abserr/ftmp2), sngl(ftmp2)
 
      eps = ftmp1 + ftmp2
 
C     Following Kornilov (instead of constant 4.42 according to Malinovskii)
C     eg0 = Sn*0.9
 
      fniub = (ux - eg0)/(eps + sn + egn)
 
      eg = eg0 + fniub*egn
 
      WRITE(*,
     &'('' Einc='',f6.3,'' Ux='',f6.3,'' Erel='',f5.1,          '' Ex=''
     &,f6.3,'' TKE='',f5.1,'' Bn='',f5.1)')En, ux, erel, uexcit, 
     &ekin_ave, bn
 
      WRITE(*,
     &'('' Eg='',f6.3,'' Enf='',f6.3,          '' Sn='',f6.3,'' nuB='',f
     &6.3,'' LANL'')')eg, eps, ABS(sn), fniub
 
      FNIU = 0.5*(fniua + fniub)
 
      RETURN
      END FUNCTION FNIU
 
 
