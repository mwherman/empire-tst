      SUBROUTINE OMTL(Nejc, Nnuc, Energ, Lmax, Stl, Ipr)
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:mpu*
Ccc   *                         S C A T 2                                *
Ccc   *                                                                  *
Ccc   *                     (by O. Bersillon)                            *
Ccc   *                                                                  *
Ccc   * Calculates spherical optical model transmission coefficients.    *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * input:NEJC - index of projectile (ejectile)                      *
Ccc   *       NNUC - index of the target (residual) nucleus              *
Ccc   *       ENERG- channel energy                                      *
Ccc   *       Ipr  - Key for printing results                            *
Ccc   *                                                                  *
Ccc   * output:LMAX - maximum l                                          *
Ccc   *        STL  - matrix of transmission coefficients (in l)         *
Ccc   *                                                                  *
C***********************************************************************
C     *                 *                                                *
C     *  PROGRAM SCAT2  *                                                *
C     *                 *                                                *
C     *******************                                                *
C                                                                        *
C     JULY      1977                                                     *
C     REV.1   JULY      1979                                             *
C     REV.2   JULY      1982                                             *
C     REV.3   OCTOBER   1991                                             *
C     REV.4   FEBRUARY 2000 (R.CAPOTE)                                   *
C     Imaginary spin orbit capabilites added                             *
C     Fixed potential parameters can be used                             *
C     (user defined energy functional for V,r,a)                         *
C     only r(i),a(i),pot(i,1) are used                                   *
C     REV.4.3 MARCH 2000 (R.CAPOTE)                                      *
C     Real surface potential added with exactly                          *
C     the same form factor as surface absorption                         *
C     To be used for DR potentials                                       *
C     Strength of this potential = DELTA V_d(E)                          *
C     must be obtained by Dispersion relations externally                *
C     REV.4.4 OCTOBER 2001 (R.CAPOTE)                                    *
C     Relativistic kinematics was added using Bersillon's SCAT2000 subr. *
C*************************************************************************
C     REFERENCE :                                                        *
C     O.BERSILLON                                                        *
C     SCAT2 : UN PROGRAMME DE MODELE OPTIQUE SPHERIQUE                   *
C     CEA-N-2227, NEANDC(E) 220'L', INDC(FR) 49/L                        *
C     OCTOBER 1981                                                       *
C*************************************************************************
C     O.BERSILLON                                                        *
C     SERVICE DE PHYSIQUE ET TECHNIQUES NUCLEAIRES                       *
C     CENTRE D'ETUDES DE BRUYERES-LE-CHATEL                              *
C     B.P. 12                                                            *
C     91680 BRUYERES-LE-CHATEL       FRANCE                              *
C     TEL.: (1) 69.26.54.14                                              *
C*************************************************************************
C     Output files:                                                      *
C     -------------                                                      *
C     is       16  Output listing file                      seq.  133   f*
C     is3      19  Summary output                           seq.   80   f*
C------------------------------------------------------------------------*
C*************************************************************************
C     SOME IMPORTANT INFORMATION:                                                        *
C     ===========                                                        *
C     IDA   Angular distribution calculation option                      *
C     =  1  calculate shape elastic angular distribution for             *
C     uniformly spaced angles from 0 to 180 degrees by                   *
C     step of 5 degrees                                                  *
C     =  2  calculate shape elastic angular distribution for             *
C     uniformly spaced cosines from 1 to -1 by                           *
C     step of 0.02                                                       *
C     =  0  no angular distribution calculation                          *
C     = -1  calculate also Legendre polynomial coefficients              *
C     (neutrons only)                                                    *
C     = -2  calculate also Legendre polynomial coefficients              *
C     (neutrons only)                                                    *
C                                                                        *
C     Energ Incident energy (MeV)                                        *
C     if Energ > 0.  center of mass energies                             *
C              < 0.  laboratory energies                                 *
C                                                                        *
C     IZT   Atomic number of the target                                  *
C     IMT   Mass   number of the target                                  *
C                                                                        *
C     IP    Type of incident particule                                   *
C     IPOT  Type of built-in parameters                                  *
C                                                                        *
C     IP   =  1 neutron      IPOT = -1 (always USER defined potential)   *
C     Potential depth are directly read instead of coefficients          *
C     Geometrical parameters are also fixed                              *
C     Only one energy is allowed !!!                                     *
C     so it should be calculated by user for a given energy functional   *
C     (DEFAULT_ENERGY_FUNCTIONAL is set to FALSE)                        *
C                                                                        *
C     This version assumes IPOT is always < 0  (Capote 2001)             *
C                                                                        *
C     read(ie,*) r(1),re(1),a(1),ae(1),(pot(1,i),i=1,6)                  *
C     ****                                                               *
C
C     Includes dispersive contribution from the                          *
C
C     volume imaginary potential (with the same geometry)                *
C
C                                                                        *
C     Real potential: Woods-Saxon                                        *
C     R (1)    = radius (fm)                                             *
C     RE(1)    = linear energy dependence of the radius                  *
C     A (1)    = diffuseness (fm)                                        *
C     AE(1)    = linear energy dependence of the diffuseness             *
C     POT(1,i) = strength parameters                                     *
C     v = pot(1,1) + pot(1,2)*e + pot(1,3)*e*e + pot(1,4)*e*e*e          *
C     pot(1,5)*ln(e) * pot(1,6)*sqrt(e)                                  *
C                                                                        *
C     read(ie,*) r(2),re(2),a(2),ae(2),(pot(2,i),i=1,6)                  *
C     ****                                                               *
C     Surface imaginary potential: if R(2) > 0., Woods-Saxon derivative  *
C     if R(2) < 0., Gaussian                                             *
C     R (2)    = radius (fm)                                             *
C     RE(2)    = linear energy dependence of the radius                  *
C     A (2)    = diffuseness (fm)                                        *
C     AE(2)    = linear energy dependence of the diffuseness             *
C     POT(2,i) = strength parameters                                     *
C     wd = pot(2,1) + pot(2,2)*e + pot(2,3)*e*e + pot(2,4)*e*e*e         *
C     pot(2,5)*ln(e) * pot(2,6)*sqrt(e)                                  *
C                                                                        *
C     read(ie,*) r(3),re(3),a(3),ae(3),(pot(3,i),i=1,6)                  *
C     ****                                                               *
C     Volume imaginary potential: Woods-Saxon                            *
C     R (3)    = radius (fm)                                             *
C     RE(3)    = linear energy dependence of the radius                  *
C     A (3)    = diffuseness (fm)                                        *
C     AE(3)    = linear energy dependence of the diffuseness             *
C     POT(3,i) = strength parameters                                     *
C     wv = pot(3,1) + pot(3,2)*e + pot(3,3)*e*e + pot(3,4)*e*e*e         *
C     pot(3,5)*ln(e) * pot(3,6)*sqrt(e)                                  *
C                                                                        *
C     read(ie,*) r(4),re(4),a(4),ae(4),(pot(4,i),i=1,6)                  *
C     ****                                                               *
C     Spin-orbit                                                         *
C     R (4)    = radius (fm)                                             *
C     RE(4)    = linear energy dependence of the radius                  *
C     A (4)    = diffuseness (fm)                                        *
C     AE(4)    = linear energy dependence of the diffuseness             *
C     POT(4,i) = strength parameters                                     *
C     vso = pot(4,1) + pot(4,2)*e + pot(4,3)*e*e + pot(4,4)*e*e*e        *
C     pot(4,5)*ln(e) * pot(4,6)*sqrt(e)                                  *
C                                                                        *
C     read(ie,*) r(5),re(5),a(5),ae(5),(pot(5,i),i=1,6)                  *
C     ****                                                               *
C     NOT USED                                                           *
C                                                                        *
C     Added by R.Capote, 2001                                            *
C     read(ie,*) r(6),re(6),a(6),ae(6),(pot(6,i),i=1,6)                  *
C     ****                                                               *
C     Imaginary Spin-orbit                                               *
C     R (6)    = radius (fm)                                             *
C     RE(6)    = linear energy dependence of the radius                  *
C     A (6)    = diffuseness (fm)                                        *
C     AE(6)    = linear energy dependence of the diffuseness             *
C     POT(6,i) = strength parameters                                     *
C     wso = pot(6,1) + pot(6,2)*e + pot(6,3)*e*e + pot(6,4)*e*e*e        *
C     pot(6,5)*ln(e) * pot(6,6)*sqrt(e)                                  *
C                                                                        *
C   Added by R.Capote, 2001                                              *
C   read(ie,*) r(7),re(7),a(7),ae(7),DELTA_Vs,Eff,Ep                     *
C   ****                                                                 *
C     (for Dispersive optical model)                                     *
C     Dispersive contribution from                                       *
C             Surface imaginary potential: Woods-Saxon derivative        *
C                                                                        *
C     pot(7,1) = DELTA_Vs must be calculated by dispersive relation      *
C     pot(7,2) = Eff Fermi energy                                        *
C     pot(7,3) = Ep  Average energy of the particle states               *
C                                                                        *
C       R (7)    = radius (fm)                                           *
C       RE(7)    = linear energy dependence of the radius                *
C       A (7)    = diffuseness (fm)                                      *
C       AE(7)    = linear energy dependence of the diffuseness           *
C                                                                        *
C     read(ie,*) rcoul,beta                                              *
C     ****                                                               *
C     RCOUL = Coulomb radius                                             *
C     BETA  = nonlocality range                                          *
C     if BETA .ne. 0. the imaginary potential is pure DWS                *
C                                                                        *
C*************************************************************************
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
      DOUBLE PRECISION AE, AK2, AQ, BETa, E1, ELAb, CETal, CSO, G, PIL, 
     &                 POT, R, RCOulomb, RE, rrmu, SE
      DOUBLE PRECISION SI, SR, ST, TC, W2L, xmas_npro, xmas_ntrg, zero, 
     &                 ZI, ZT
      INTEGER i, iba, ida, IE, imt, ip, ipl, ipot, IS, IS1, IS2, IS3, 
     &        IS4, izt, j, LFMAX, LTCMAX, na
      PARAMETER(LTCMAX = NDTL, LFMAX = 4*NDTL + 2)
      REAL*8 MI, MT
      CHARACTER ref*10
C
C     Dummy arguments
C
      DOUBLE PRECISION Energ
      INTEGER Ipr, Lmax, Nejc, Nnuc
      DOUBLE PRECISION Stl(NDLW)
C
C     COMMON variables
C
C     CAPOTE 2001
C     PI,W2,CETA,CSO are defined globally, PIL,W2L,CETAL,CSOL locally
C---------------------------------------------------------------------
      COMMON /CONST / MI, SI, ZI, MT, ZT, PIL, AK2, CETal, W2L, CSOl
      COMMON /ENER  / E1, ELAb
      COMMON /FACT  / G(LFMAX)
      COMMON /INOUT / IE, IS, IS1, IS2, IS3, IS4
      COMMON /POTEN1/ R(7), RE(7), AQ(7), AE(7), POT(7, 6)
C     COMMON /POTEN2/ RCOulomb, BETa
C
      COMMON /POTEN2/ RCOulomb, BETa, EFErmi, EP, EA, IREl
      COMMON /TCE   / TC(LTCMAX)
      COMMON /XS    / SE, SR, ST
C
      DATA zero/0.0D+00/
C
C
      IS = 31
      IS1 = 6
      IS3 = 10
C     not used
      IE = 5
      IS2 = 33
      IS4 = 35
      OPEN(IS, FILE = 'SCAT2.OUT', STATUS = 'UNKNOWN')
C
      PIL = PI
      W2L = W2*10.D0
      CETal = CETa
      CSOl = CSO
C
      ida = 1
      iba = 0
C---------------------------------------------------
      IF(ida.NE.0)THEN
         CALL FCT()
         CALL PREANG(ida, na)
      ENDIF
      xmas_npro = (AEJc(Nejc)*AMUmev + XMAss_ej(Nejc))/AMUmev
      xmas_ntrg = (A(Nnuc)*AMUmev + XMAss(Nnuc))/AMUmev
      izt = Z(Nnuc)
      imt = A(Nnuc)
      ZT = DFLOAT(izt)
C     mt = dfloat(imt)
      MT = xmas_ntrg
C     MI = AEJc(Nejc)
      MI = xmas_npro
      ZI = ZEJc(Nejc)
      SI = SEJc(Nejc)
      ipl = IFIX(SNGL(2.0*SI + 1.001))
      ipot = KTRlom(Nejc, Nnuc)
      IF(Nejc.EQ.0)THEN
         IF(AEJc(Nejc).EQ.1.D0 .AND. ZEJc(Nejc).EQ.0.D0)
     &      ipot = KTRlom(1, 1)
         IF(AEJc(Nejc).EQ.1.D0 .AND. ZEJc(Nejc).EQ.1.D0)
     &      ipot = KTRlom(2, 1)
         IF(AEJc(Nejc).EQ.4.D0 .AND. ZEJc(Nejc).EQ.2.D0)
     &      ipot = KTRlom(3, 1)
      ENDIF
C
C     ****
C     IP =
C     1 NEUTRON
C     2 PROTON
C     3 DEUTERON
C     4 TRITON
C     5 HELIUM-3
C     6 ALPHA
C
      IF(AEJc(Nejc).EQ.1.D0 .AND. ZEJc(Nejc).EQ.0.D0)ip = 1
      IF(AEJc(Nejc).EQ.1.D0 .AND. ZEJc(Nejc).EQ.1.D0)ip = 2
      IF(AEJc(Nejc).EQ.2.D0 .AND. ZEJc(Nejc).EQ.1.D0)ip = 3
      IF(AEJc(Nejc).EQ.3.D0 .AND. ZEJc(Nejc).EQ.1.D0)ip = 4
      IF(AEJc(Nejc).EQ.3.D0 .AND. ZEJc(Nejc).EQ.2.D0)ip = 5
      IF(AEJc(Nejc).EQ.4.D0 .AND. ZEJc(Nejc).EQ.2.D0)ip = 6
      IF(AEJc(Nejc).EQ.6.D0 .AND. ZEJc(Nejc).EQ.3.D0)ip = 7
      IF(AEJc(Nejc).EQ.7.D0 .AND. ZEJc(Nejc).EQ.3.D0)ip = 8
      IF(AEJc(Nejc).EQ.7.D0 .AND. ZEJc(Nejc).EQ.4.D0)ip = 9
C
C     Initialisations
C
      DO i = 1, 7
         AQ(i) = zero
         AE(i) = zero
         R(i) = zero
         RE(i) = zero
         DO j = 1, 6
            POT(i, j) = zero
         ENDDO
      ENDDO
      BETa = zero
      RCOulomb = zero
      DO j = 1, LTCMAX
         TC(j) = zero
      ENDDO
      ref = 'USER'
C
C     Capote 2001
C     Potential depths must be calculated in LAB system
C     CALL SETPOTS(Nejc, Nnuc, ELAB)
C
      IF(Energ.LT.zero)THEN
         ELAb = DABS(Energ)
         ikey = -1
      ELSE
         E1 = Energ
         ikey = +1
      ENDIF
C
C     Transformation of energies from laboratory to center-of-mass if needed
C     is done inside SETPOTS() -> OMPAR()
C
      CALL SETPOTS(Nejc, Nnuc, ELAb, E1, MI, MT, rrmu, AK2, ikey)
C
      CALL SCAT(Lmax, Ipr, rrmu)
C
      IF(ipl.EQ.1)THEN
         CALL SPIN0(Lmax, Ipr)
      ELSEIF(ipl.EQ.2)THEN
         CALL SPIN05(Lmax, Ipr)
      ELSEIF(ipl.EQ.3)THEN
         CALL SPIN1(Lmax, Ipr)
      ENDIF
      IF(Ipr.NE.0)WRITE(IS3, FMT = '(1p,4e10.3)')ELAb, SR, SE, ST
      IF(ida.NE.0 .AND. ip.EQ.1)CALL SHAPEL2(Lmax, ida, na, ipl, Ipr)
      IF(ida.NE.0 .AND. ip.NE.1)CALL SHAPEC(Lmax, ida, na, ipl, Ipr)
C-----transfer o.m. Tl onto STL matrix and set them to 0 if lower than 1E-15
      DO i = 1, MIN0(Lmax, NDTL)
         IF(TC(i).GT.1.0D-15)THEN
            Stl(i) = TC(i)
         ELSE
            Stl(i) = 0.
         ENDIF
      ENDDO
      CLOSE(IS)
      END
C
      COMPLEX*16 FUNCTION CGAMMA(Z)
C***********************************************************************
C     Calculates complex gamma function.                               *
C     Z must be declared complex in the calling program.               *
C                                                                      *
C     Reference:   Y.L.Luke                                            *
C                  The special functions and their approximations      *
C                  vol.2, Academic Press, New York and London          *
C                  (1969) p.304-305                                    *
C***********************************************************************
      IMPLICIT DOUBLE PRECISION(A - h, O - Z)
      DOUBLE PRECISION const, fk, fk1, g, half, one, pi, x, zero
      INTEGER k, l
      COMPLEX*16 h, s, u, v, Z
      DIMENSION g(16)
C
      DATA pi/3.141592653589793D+00/
      DATA g/41.624436916439068, -51.224241022374774, 
     &     +11.338755813488977, -0.747732687772388, +0.008782877493061, 
     &     -0.000001899030264, +0.000000001946335, -0.000000000199345, 
     &     +0.000000000008433, +0.000000000001486, -0.000000000000806, 
     &     +0.000000000000293, -0.000000000000102, +0.000000000000037, 
     &     -0.000000000000014, +0.000000000000006/
C
C
      DATA zero/0.0D+00/, one/1.0D+00/, half/0.5D+00/
C
C
      const = 2.506628274631001D+00
      u = Z
      x = DBLE(u)
      IF(x.GE.one)THEN
         v = u
         l = 3
      ELSEIF(x.GE.zero)THEN
         v = u + one
         l = 2
      ELSE
         v = one - u
         l = 1
      ENDIF
      h = one
      s = g(1)
      DO k = 2, 16
         fk = k - 2
         fk1 = fk + one
         h = ((v - fk1)/(v + fk))*h
         s = s + g(k)*h
      ENDDO
      h = v + 4.5D+00
      CGAMMA = const*CDEXP((v - half)*CDLOG(h) - h)*s
      IF(l.EQ.1)THEN
         CGAMMA = pi/(CDSIN(pi*u)*CGAMMA)
         RETURN
      ELSEIF(l.EQ.2)THEN
         CGAMMA = CGAMMA/u
         RETURN
      ELSEIF(l.EQ.3)THEN
         RETURN
      ENDIF
      END
C
C
      DOUBLE PRECISION FUNCTION CLEBG(Aj1, Aj2, Aj3, Am1, Am2, Am3)
C***********************************************************************
C     Calculate Clebsch-Gordan coefficients                            *
C                                                                      *
C     Attention:  cg(j1,j2,j3,;m1,m2,m3) = (-1)**(j1+j2-m3)*           *
C                                          3-J(j1,j2,j3;m1,m2,-m3)     *
C                                          ---                         *
C                                                                      *
C     from    John.G. Wills     ORNL-TM-1949 (august 1967)             *
C                            et Comp.Phys.Comm. 2(1971)381             *
C                                                                      *
C     O.Bersillon     august 1977                                      *
C                                                                      *
C***********************************************************************
      IMPLICIT DOUBLE PRECISION(a - h, O - Z)
      INCLUDE 'dimension.h'
      DOUBLE PRECISION a, Aj1, Aj2, Aj3, Am1, Am2, Am3, b, c, d, e, eps, 
     &                 f, G, h, one, q, s, t, two
      DOUBLE PRECISION x, zero
      INTEGER i, i1, i10, i11, i2, i3, i4, i5, i6, i7, i8, i9, il, is1, 
     &        j, j1, j2, j3, k, l
      INTEGER la, lb, LFMAX, m, m1, m2, m3, n
      PARAMETER(LFMAX = 4*NDTL + 2)
C     parameter ( ltcmax=100, LFMAX=4*NDTL+2)
      DIMENSION i(11)
      COMMON /FACT  / G(LFMAX)
      EQUIVALENCE(i(1), i1)
      EQUIVALENCE(i(2), i2)
      EQUIVALENCE(i(3), i3)
      EQUIVALENCE(i(4), i4)
      EQUIVALENCE(i(5), i5)
      EQUIVALENCE(i(6), i6)
      EQUIVALENCE(i(7), i7)
      EQUIVALENCE(i(8), i8)
      EQUIVALENCE(i(9), i9)
      EQUIVALENCE(i(10), i10)
      EQUIVALENCE(i(11), i11)
C
      DATA zero/0.0D+00/, one/1.0D+00/, two/2.0D+00/, eps/0.001D0/
C
C
      is1 = 6
      CLEBG = zero
C
C     Convert the arguments to integer
C
      j1 = IDINT(two*Aj1 + eps)
      j2 = IDINT(two*Aj2 + eps)
      j3 = IDINT(two*Aj3 + eps)
      m1 = IDINT(two*Am1 + SIGN(eps, Am1))
      m2 = IDINT(two*Am2 + SIGN(eps, Am2))
      m3 = IDINT(two*Am3 + SIGN(eps, Am3))
C
C     Test m1 + m2 = m3
C
      IF(m1 + m2.NE.m3)RETURN
C
C     Test table size
C
      i(10) = (j1 + j2 + j3)/2 + 2
      n = i(10)
      i(11) = j3 + 2
      IF(i(10).GT.LFMAX)THEN
         WRITE(is1, *)' '
         WRITE(is1, 99001)i(10), LFMAX, Aj1, Aj2, Aj3, Am1, Am2, Am3
99001    FORMAT(' ', ' FACTORIAL TABLE SIZE IN CLEBG ', 2I5, 6F5.1)
         WRITE(is1, *)
     &           ' Increase lfmax parameter in OM-scat2.f and recompile'
         WRITE(6, *)' LFMAX is actually set to ', LFMAX
         STOP 'IN CLEBG OF SCAT2'
      ENDIF
      i(1) = j1 + j2 - j3
      i(2) = j2 + j3 - j1
      i(3) = j3 + j1 - j2
      i(4) = j1 - m1
      i(5) = j1 + m1
      i(6) = j2 - m2
      i(7) = j2 + m2
      i(8) = j3 - m3
      i(9) = j3 + m3
C
C     Check i(j) = even, triangular inequality, m less than j,
C     find number of terms
C
      DO j = 1, 9
         k = i(j)/2
         IF(i(j).NE.2*k)RETURN
         IF(k.LT.0)RETURN
         IF(k.LT.n)n = k
         i(j) = k + 1
      ENDDO
      IF(m3.NE.0 .OR. m1.NE.0 .OR. m1.NE.1)THEN
         il = 0
         la = i1 - i5
         lb = i1 - i6
         IF(il.LT.la)il = la
         IF(il.LT.lb)il = lb
C
C        Form coefficients of sum
C
         c = (G(i11) - G(i11 - 1) + G(i1) + G(i2) + G(i3) - G(i10)
     &       + G(i4) + G(i5) + G(i6) + G(i7) + G(i8) + G(i9))/two
         j1 = i1 - il
         j2 = i4 - il
         j3 = i7 - il
         m1 = il + 1
         m2 = il - la + 1
         m3 = il - lb + 1
         c = c - G(j1) - G(j2) - G(j3) - G(m1) - G(m2) - G(m3)
         c = DEXP(c)
         IF((il - 2*(il/2)).NE.0)c = -c
         IF(n.LT.0)RETURN
         IF(n.EQ.0)THEN
            CLEBG = c
            RETURN
         ELSE
C
C           Form sum
C
            a = j1 - 1
            b = j2 - 1
            h = j3 - 1
            d = m1
            e = m2
            f = m3
            s = one
            q = n - 1
            DO j = 1, n
               t = (a - q)/(d + q)*(b - q)/(e + q)*(h - q)/(f + q)
               s = one - s*t
               q = q - one
            ENDDO
            CLEBG = c*s
            RETURN
         ENDIF
      ELSE
C
C        Special formula for m3 = 0 and m1 = 0 or 1/2
C
         k = i10/2
         IF(i10.EQ.2*k)THEN
            k = 0
         ELSE
            k = 1
         ENDIF
         IF(m1.EQ.0)THEN
            l = 0
            IF(k.NE.0)RETURN
         ELSEIF(m1.EQ.1)THEN
            l = 1
         ENDIF
         x = l
         m = i3 + (i1 + k + 1)/2 - l
         m1 = i10/2 + k
         m2 = i4 + i5
         m3 = i6 + i7
         j1 = (i1 + 1 - k)/2
         j2 = (i2 + 1 + k - l)/2
         j3 = (i3 + 1 + k - l)/2
         CLEBG = DEXP
     &           ((G(i11) - G(i11-1) + G(i1) + G(i2) + G(i3) - G(i10))
     &           /two + G(m1) - G(j1) - G(j2) - G(j3)
     &           + x*(G(3) - (G(m2)-G(m2-1)+G(m3)-G(m3-1))/two))
         IF((m - 2*(m/2)).NE.0)CLEBG = -CLEBG
         RETURN
      ENDIF
      END
C
C
C
      SUBROUTINE FCT()
C***********************************************************************
C     Calculate factorial logarithms from 0! ( =1.) up to (idim-1)!    *
C                                                                      *
C                     k! = fact(k+1)                                   *
C***********************************************************************
      IMPLICIT DOUBLE PRECISION(A - H, O - Z)
      DOUBLE PRECISION FACto, zero
      INTEGER k, LFMAX
      INCLUDE 'dimension.h'
      PARAMETER(LFMAX = 4*NDTL + 2)
      COMMON /FACT  / FACto(LFMAX)
C
      DATA zero/0.D0/
      FACto(1) = zero
      FACto(2) = zero
      DO k = 3, LFMAX
         FACto(k) = FACto(k - 1) + DLOG(DFLOAT(k - 1))
      ENDDO
      END
C
C
      SUBROUTINE INTEG(Npt, H, Sl, Spo)
C***********************************************************************
C     Integrate the Schroedinger equation                              *
C                                                                      *
C     See:      A.C.Allison                                            *
C     Journal of Computational Physics  6(1970)378-391                 *
C***********************************************************************
      IMPLICIT DOUBLE PRECISION(A - H, O - Z)
      DOUBLE PRECISION AK2, det, eps, ETA, f1, f2, f3, H, h212, one, PI, 
     &                 POTi, POTr, PSIi, PSIip, PSIr, PSIrp, r, r2, s1
      DOUBLE PRECISION s2, SI, Sl, Spo, ten, two, u, VSO, w, W2, WSO, y, 
     &                 zero, ZI, ZT
      INTEGER i, IE, IS, IS1, IS2, IS3, IS4, j, m, n, Npt, npt3, npt5
      REAL*8 MI, MT
      DIMENSION y(2, 305), u(305), w(305)
      DIMENSION f1(2, 2), f2(2, 2), f3(2, 2), s1(2), s2(2)
      COMMON /CONST / MI, SI, ZI, MT, ZT, PI, AK2, ETA, W2, CSO
      COMMON /INOUT / IE, IS, IS1, IS2, IS3, IS4
      COMMON /POTN  / POTi(305), POTr(305), VSO(305), WSO(305)
      COMMON /PSI   / PSIr, PSIrp, PSIi, PSIip
C
      DATA zero/0.0D+00/, one/1.0D+00/, two/2.0D+00/
C
      DATA ten/1.0D+01/, eps/0.001D0/
C
C
C=======================================================================
C     Integration formula
C
C     2         -1           2               2
C     y   = (i+h *f   /12)  *((2*i-10*h *f /12)*y -(i+h *f   /12)*y   ))
C     n+1         n+1                    n      n        n-1      n-1
C
C     f1 = f(n-1) * h**2/12
C     f2 = f( n ) * h**2/12
C     f3 = f(n+1) * h**2/12
C     s1 = i + f1
C     s2 = 2*i - 10*f2
C     det = determinant ( I + f3 )
C=======================================================================
C
      h212 = H*H/1.2D+01
      npt3 = Npt + 3
      npt5 = Npt + 5
      DO n = 1, npt3
         u(n) = POTr(n) + Spo*VSO(n)
C        Capote 2001, added Imaginary Spin Orbit
C        w(n) = poti(n)
         w(n) = POTi(n) + Spo*WSO(n)
      ENDDO
C
C     y(1,n) =   real    part of the wave function
C     y(2,n) = imaginary part of the wave function
C
C     Initial conditions
C
      y(1, 1) = zero
      y(2, 1) = zero
      y(1, 2) = eps
      y(2, 2) = eps
C
      f1(1, 1) = zero
      f1(1, 2) = zero
      f1(2, 1) = zero
      f1(2, 2) = zero
C
      f2(1, 1) = -one
      f2(1, 2) = zero
      f2(2, 1) = zero
      f2(2, 2) = -one
C
C     Start integration
C
      DO n = 3, npt5
         m = n - 2
         r = DFLOAT(m)*H
         r2 = r*r
C
         f3(1, 1) = (AK2 - Sl/r2 - u(m))*h212
         f3(1, 2) = ( + w(m))*h212
         f3(2, 1) = ( - w(m))*h212
         f3(2, 2) = (AK2 - Sl/r2 - u(m))*h212
C
         s1(1) = (one + f1(1, 1))*y(1, n - 2) + f1(1, 2)*y(2, n - 2)
         s1(2) = (one + f1(2, 2))*y(2, n - 2) + f1(2, 1)*y(1, n - 2)
C
         s2(1) = (two - ten*f2(1, 1))*y(1, n - 1) - ten*f2(1, 2)
     &           *y(2, n - 1)
         s2(2) = (two - ten*f2(2, 2))*y(2, n - 1) - ten*f2(2, 1)
     &           *y(1, n - 1)
C
         det = (f3(1, 1) + one)*(f3(2, 2) + one) - f3(1, 2)*f3(2, 1)
C
         y(1, n) = (f3(2, 2) + one)*(s2(1) - s1(1)) - f3(1, 2)
     &             *(s2(2) - s1(2))
         y(2, n) = (f3(1, 1) + one)*(s2(2) - s1(2)) - f3(2, 1)
     &             *(s2(1) - s1(1))
         y(1, n) = y(1, n)/det
         y(2, n) = y(2, n)/det
C
         DO i = 1, 2
            DO j = 1, 2
               f1(i, j) = f2(i, j)
               f2(i, j) = f3(i, j)
            ENDDO
         ENDDO
      ENDDO
C
C     Calculation of the derivatives
C
      n = Npt + 2
      PSIr = y(1, n)
      PSIi = y(2, n)
      PSIrp = (y(1, n + 3) - y(1, n - 3) + 9.*(y(1,n-2) - y(1,n+2))
     &        + 45.*(y(1,n+1) - y(1,n-1)))/(60.*H)
      PSIip = (y(2, n + 3) - y(2, n - 3) + 9.*(y(2,n-2) - y(2,n+2))
     &        + 45.*(y(2,n+1) - y(2,n-1)))/(60.*H)
      END
C
C
      SUBROUTINE PREANG(Ida, Na)
C***********************************************************************
C     Calculate Legendre and associated Legendre polynomials           *
C     if ida = 1 equidistant angles between 0 and 180 degrees          *
C                by step of 2.5                                        *
C            = 2 angles whose cosines are equidistant between          *
C                -1 and 1 by step of 0.02                              *
C***********************************************************************
      IMPLICIT DOUBLE PRECISION(A - H, O - Z)
      INCLUDE 'dimension.h'
      DOUBLE PRECISION AK2, ANG, CANg, d, deg, ETA, one, PI, PL, PL1, 
     &                 PL2, rad, SI, three, two, W2, x, xl, zero, ZI
      DOUBLE PRECISION ZT
      INTEGER i, Ida, idaa, IE, IS, IS1, IS2, IS3, IS4, l, LTCMAX, Na
      PARAMETER(LTCMAX = NDTL)
C     parameter ( ltcmax=100, LFMAX=4*NDTL+2)
      REAL*8 MI, MT
      COMMON /INOUT / IE, IS, IS1, IS2, IS3, IS4
      COMMON /ANGULAR/ ANG(73), CANg(101), PL(2*LTCMAX - 1, 101), 
     &                 PL1(2*LTCMAX - 1, 101), PL2(2*LTCMAX - 1, 101)
      COMMON /CONST / MI, SI, ZI, MT, ZT, PI, AK2, ETA, W2, CSO
C
C
      DATA zero/0.0D+00/, one/1.0D+00/, two/2.0D+00/, three/3.D0/
C
      DATA deg/180.D0/
C
C
      rad = deg/PI
      idaa = IABS(Ida)
      IF(idaa.EQ.1)THEN
C
C        Calculate cosines of equally spaced angles
C        WARNING NA must be less than 101 !!!
C
         Na = 73
         d = deg/DFLOAT(Na - 1)
         IF(Na.GT.101)THEN
            WRITE(6, *)' '
            WRITE(6, *)'FATAL ERROR!'
            WRITE(6, *)'TOO MANY ANGULAR POINTS REQUESTED FROM SCAT2'
            STOP
         ENDIF
C        do i=1,na
C        ang(i) = dfloat(i-1)*d
         DO i = 1, Na
            ANG(i) = DBLE(i - 1)*d
            CANg(i) = DCOS(ANG(i)/rad)
            IF(DABS(CANg(i)).LT.1.0D-06)CANg(i) = zero
         ENDDO
      ELSEIF(idaa.EQ.2)THEN
C
C        Calculate equally spaced cosines
C
C
C        WARNING NA must be less than 101 !!!
         Na = 101
         d = two/DFLOAT(Na - 1)
         DO i = 1, Na
            CANg(i) = one - DFLOAT(i - 1)*d
            IF(DABS(CANg(i)).LT.1.0D-06)CANg(i) = zero
         ENDDO
      ENDIF
C
C     Calculate Legendre polynomials
C
      DO i = 1, Na
         x = CANg(i)
         PL(1, i) = one
         PL(2, i) = x
         DO l = 3, 2*LTCMAX - 1
            xl = DFLOAT(l - 1)
            PL(l, i) = ((two*xl - one)*x*PL(l - 1, i) - (xl - one)
     &                 *PL(l - 2, i))/xl
         ENDDO
      ENDDO
C
C     Calculate associated Legendre polynomials
C
      DO i = 1, Na
         x = CANg(i)
         PL1(1, i) = zero
         PL1(2, i) = DSQRT(one - x*x)
         DO l = 3, 2*LTCMAX - 1
            xl = DFLOAT(l - 1)
            PL1(l, i) = ((two*xl - one)*x*PL1(l - 1, i) - xl*PL1(l - 2, 
     &                  i))/(xl - one)
         ENDDO
      ENDDO
      DO i = 1, Na
         x = CANg(i)
         PL2(1, i) = zero
         PL2(2, i) = zero
         PL2(3, i) = three*(one - x*x)
         DO l = 4, 2*LTCMAX - 1
            xl = DFLOAT(l - 1)
            PL2(l, i) = ((two*xl - one)*x*PL2(l - 1, i) - (xl + one)
     &                  *PL2(l - 2, i))/(xl - two)
         ENDDO
      ENDDO
      END
C
C
      DOUBLE PRECISION FUNCTION RACAH(A, B, C, D, E, F)
C***********************************************************************
C     Calculate Racah coefficients      w(a,b,c,d;e,f)                 *
C                                                                      *
C     Attention:   w(a,b,c,d;e,f) = (-1)**(a+b+c+d)*6-J(a,b,e;d,c,f)   *
C                                                   ---                *
C                                                                      *
C     from John.G.Wills     ORNL-TM-1949 (august 1967)                 *
C                       and Comp.Phys.Comm. 2(1971)381                 *
C                                                                      *
C     O.Bersillon     august 1977                                      *
C                                                                      *
C***********************************************************************
      IMPLICIT DOUBLE PRECISION(A - h, o - z)
      INCLUDE 'dimension.h'
      DOUBLE PRECISION A, B, C, D, E, eps, F, G, h, o, one, p, q, r, s, 
     &                 t, two, v, w, x
      DOUBLE PRECISION y, z, zero
      INTEGER i, i1, i10, i11, i12, i13, i14, i15, i16, i2, i3, i4, i5, 
     &        i6, i7, i8, i9, il, j, j1
      INTEGER j2, j3, j4, j5, j6, j7, ja, jb, jc, jd, je, jf, k, LFMAX, 
     &        n
      PARAMETER(LFMAX = 4*NDTL + 2)
C     parameter ( ltcmax=100, LFMAX=4*NDTL+2)
      DIMENSION i(16)
      COMMON /FACT  / G(LFMAX)
      EQUIVALENCE(i(1), i1)
      EQUIVALENCE(i(2), i2)
      EQUIVALENCE(i(3), i3)
      EQUIVALENCE(i(4), i4)
      EQUIVALENCE(i(5), i5)
      EQUIVALENCE(i(6), i6)
      EQUIVALENCE(i(7), i7)
      EQUIVALENCE(i(8), i8)
      EQUIVALENCE(i(9), i9)
      EQUIVALENCE(i(10), i10)
      EQUIVALENCE(i(11), i11)
      EQUIVALENCE(i(12), i12)
      EQUIVALENCE(i(13), i13)
      EQUIVALENCE(i(14), i14)
      EQUIVALENCE(i(15), i15)
      EQUIVALENCE(i(16), i16)
C
C
C
      DATA zero/0.0D+00/, one/1.0D+00/, two/2.0D+00/, eps/0.001D0/
C
C
      RACAH = zero
C
C     Convert arguments to integer and make usefull combinations
C
      ja = IDINT(two*A + eps)
      jb = IDINT(two*B + eps)
      jc = IDINT(two*C + eps)
      jd = IDINT(two*D + eps)
      je = IDINT(two*E + eps)
      jf = IDINT(two*F + eps)
      i1 = ja + jb - je
      i2 = jb + je - ja
      i3 = je + ja - jb
      i4 = jc + jd - je
      i5 = jd + je - jc
      i6 = je + jc - jd
      i7 = ja + jc - jf
      i8 = jc + jf - ja
      i9 = jf + ja - jc
      i10 = jb + jd - jf
      i11 = jd + jf - jb
      i12 = jf + jb - jd
      i13 = ja + jb + je
      i14 = jc + jd + je
      i15 = ja + jc + jf
      i16 = jb + jd + jf
C
C     Check triangular inequalities, find no. of terms in sum,
C     divide i's by 2
C
      n = i16
      DO j = 1, 12
         k = i(j)/2
         IF(i(j).NE.2*k)RETURN
         IF(k.LT.0)RETURN
         IF(k.LT.n)n = k
         i(j) = k + 1
      ENDDO
C
C     Find minimum value of summation index
C
      il = 0
      DO j = 13, 16
         i(j) = i(j)/2
         IF(il.LT.i(j))il = i(j)
      ENDDO
      j1 = il - i13 + 1
      j2 = il - i14 + 1
      j3 = il - i15 + 1
      j4 = il - i16 + 1
      j5 = i13 + i4 - il
      j6 = i15 + i5 - il
      j7 = i16 + i6 - il
      h = -
     &    DEXP((G(i1) + G(i2) + G(i3) - G(i13+2) + G(i4) + G(i5) + G(i6)
     &    - G(i14+2) + G(i7) + G(i8) + G(i9) - G(i15+2) + G(i10)
     &    + G(i11) + G(i12) - G(i16+2))/two + G(il + 2) - G(j1) - G(j2)
     &    - G(j3) - G(j4) - G(j5) - G(j6) - G(j7))
      IF((j5 - 2*(j5/2)).NE.0)h = -h
      IF(n.LT.0)RETURN
      IF(n.EQ.0)THEN
         RACAH = h
         RETURN
      ELSE
         s = one
         q = n - 1
         p = il + 2
         r = j1
         o = j2
         v = j3
         w = j4
         x = j5 - 1
         y = j6 - 1
         z = j7 - 1
         DO j = 1, n
            t = (p + q)/(r + q)*(x - q)/(o + q)*(y - q)/(v + q)*(z - q)
     &          /(w + q)
            s = one - s*t
            q = q - one
         ENDDO
         RACAH = h*s
      ENDIF
      END
C
C
      SUBROUTINE RCWFN(Rho, Eta, Minl, Maxl, Fc, Fcp, Gc, Gcp, Accur, 
     &                 Step)
C***********************************************************************
C     coulomb wave functions calculated at r = rho                     *
C     by the continued-fraction method of J.W.Steed                    *
C     MINL, MAXL are actual l-values                                   *
C                                                                      *
C     See: A.R.Barnett, D.H.Feng, J.W.Steed and L.J.B.Goldfarb         *
C     Computer Physics Communications  8(1974)377-395                  *
C***********************************************************************
      IMPLICIT DOUBLE PRECISION(A - h, O - Z)
      INCLUDE 'dimension.h'
      DOUBLE PRECISION acc, Accur, ai, ar, bi, br, d, del, di, dk, dp, 
     &                 dq, dr, Eta, eta2, etah, etar, f, Fc, Fcp
      DOUBLE PRECISION fp, g, Gc, Gcp, gp, gpmax, h, h2, h2ll, one, p, 
     &                 pace, pl, pmx, q, r, r3, rh, rh2, Rho
      DOUBLE PRECISION rho2, s, Step, t, tf, tfp, turn, two, w, wi, 
     &                 xll1, zero
      INTEGER i2, ktr, ktrp, l, lmax, lmin1, lp, LTCMAX, Maxl, Minl
      DOUBLE PRECISION k, k1, k2, k3, k4, m1, m2, m3, m4
      PARAMETER(LTCMAX = NDTL)
C     parameter ( ltcmax=100)
      DIMENSION Fc(LTCMAX), Fcp(LTCMAX), Gc(LTCMAX), Gcp(LTCMAX)
C
      DATA zero/0.0D+00/, one/1.0D+00/, two/2.0D+00/
C
      gpmax = 1.0D+38
      pace = Step
      acc = Accur
      IF(pace.LT.1.0D+02)pace = 1.0D+02
      IF(acc.LT.1.0D-15 .OR. acc.GT.1.0D-06)acc = 1.0D-06
      r = Rho
      ktr = 1
      lmax = Maxl
      lmin1 = Minl + 1
      xll1 = DFLOAT(Minl*lmin1)
      eta2 = Eta*Eta
      turn = Eta + DSQRT(eta2 + xll1)
      IF(r.LT.turn .AND. DABS(Eta).GE.1.0D-06)ktr = -1
      ktrp = ktr
 100  etar = Eta*r
      rho2 = r*r
      pl = DFLOAT(lmax + 1)
      pmx = pl + 0.5D+00
C
C     Continued fraction for fp(maxl)/f(maxl), xl is f, xlprime is fp
C
      fp = Eta/pl + pl/r
      dk = etar*two
      del = zero
      d = zero
      f = one
      k = (pl*pl - pl + etar)*(two*pl - one)
      IF(pl*pl + pl + etar.NE.zero)THEN
 150     h = (pl*pl + eta2)*(one - pl*pl)*rho2
         k = k + dk + pl*pl*6.0D+00
         d = one/(d*h + k)
         del = del*(d*k - one)
         IF(pl.LT.pmx)del = -r*(pl*pl + eta2)*(pl + one)*d/pl
         pl = pl + one
         fp = fp + del
         IF(d.LT.zero)f = -f
         IF(pl.GT.2.0D+04)GOTO 300
         IF(DABS(del/fp).GE.acc)GOTO 150
         fp = f*fp
         IF(lmax.NE.Minl)THEN
            Fc(lmax + 1) = f
            Fcp(lmax + 1) = fp
C
C           Downward recursion to minl for f and fp, arrays gc, gcp are storage
C
            l = lmax
            DO lp = lmin1, lmax
               pl = DFLOAT(l)
               Gc(l + 1) = Eta/pl + pl/r
               Gcp(l + 1) = DSQRT(eta2 + pl*pl)/pl
               Fc(l) = (Gc(l + 1)*Fc(l + 1) + Fcp(l + 1))/Gcp(l + 1)
               Fcp(l) = Gc(l + 1)*Fc(l) - Gcp(l + 1)*Fc(l + 1)
               l = l - 1
            ENDDO
            f = Fc(lmin1)
            fp = Fcp(lmin1)
         ENDIF
         IF(ktrp.EQ. - one)THEN
            r = turn
            tf = f
            tfp = fp
            lmax = Minl
            ktrp = 1
            GOTO 100
         ELSE
C
C           repeat for r = turn if rho lt turn
C           now obtain p + i.q for minl from continued fraction (32)
C           real arithmetic to facilitate conversion to ibm using real*8
C
            p = zero
            q = r - Eta
            pl = zero
            ar = -(eta2 + xll1)
            ai = Eta
            br = two*q
            bi = two
            wi = two*Eta
            dr = br/(br*br + bi*bi)
            di = -bi/(br*br + bi*bi)
            dp = -(ar*di + ai*dr)
            dq = ar*dr - ai*di
 160        p = p + dp
            q = q + dq
            pl = pl + two
            ar = ar + pl
            ai = ai + wi
            bi = bi + two
            d = ar*dr - ai*di + br
            di = ai*dr + ar*di + bi
            t = one/(d*d + di*di)
            dr = d*t
            di = -t*di
            h = br*dr - bi*di - one
            k = bi*dr + br*di
            t = dp*h - dq*k
            dq = dp*k + dq*h
            dp = t
            IF(pl.GT.4.6D+04)GOTO 300
            IF(DABS(dp) + DABS(dq).GE.(DABS(p) + DABS(q))*acc)GOTO 160
            p = p/r
            q = q/r
C
C           Solve for fp, g, gp and normalise f at l = minl
C
            g = (fp - p*f)/q
            gp = p*g - q*f
            w = one/DSQRT(fp*g - f*gp)
            g = w*g
            gp = w*gp
            IF(ktr.NE.1)THEN
               f = tf
               fp = tfp
               lmax = Maxl
C
C              Runge-kutta integration of g(minl) and gp(minl) inwards from turn
C
               IF(Rho.LT.0.2D+00*turn)pace = 999.0
               r3 = one/3.0D+00
               h = (Rho - turn)/(pace + one)
               h2 = 0.5D+00*h
               i2 = IDINT(pace + 1.0D-03)
               etah = Eta*h
               h2ll = h2*xll1
               s = (etah + h2ll/r)/r - h2
 170           rh2 = r + h2
               t = (etah + h2ll/rh2)/rh2 - h2
               k1 = h2*gp
               m1 = s*g
               k2 = h2*(gp + m1)
               m2 = t*(g + k1)
               k3 = h*(gp + m2)
               m3 = t*(g + k2)
               m3 = m3 + m3
               k4 = h2*(gp + m3)
               rh = r + h
               s = (etah + h2ll/rh)/rh - h2
               m4 = s*(g + k3)
               g = g + (k1 + k2 + k2 + k3 + k4)*r3
               gp = gp + (m1 + m2 + m2 + m3 + m4)*r3
               r = rh
               i2 = i2 - 1
               IF(DABS(gp).GT.gpmax)GOTO 300
               IF(i2.GE.0)GOTO 170
               w = one/(fp*g - f*gp)
            ENDIF
         ENDIF
      ELSE
         r = r + 1.0D-06
         GOTO 100
      ENDIF
C
C     Upward recursion from gc(minl) and gcp(minl), stored values are r,
C     Renormalise fc, fcp for each l-value
C
 200  Gc(lmin1) = g
      Gcp(lmin1) = gp
      IF(lmax.EQ.Minl)THEN
         Fc(lmin1) = w*f
         Fcp(lmin1) = w*fp
      ELSE
         DO l = lmin1, lmax
            t = Gc(l + 1)
            Gc(l + 1) = (Gc(l)*Gc(l + 1) - Gcp(l))/Gcp(l + 1)
            Gcp(l + 1) = Gc(l)*Gcp(l + 1) - Gc(l + 1)*t
            Fc(l + 1) = w*Fc(l + 1)
            Fcp(l + 1) = w*Fcp(l + 1)
         ENDDO
         Fc(lmin1) = Fc(lmin1)*w
         Fcp(lmin1) = Fcp(lmin1)*w
      ENDIF
      GOTO 99999
 300  w = zero
      g = zero
      gp = zero
      GOTO 200
99999 END
C
C
      SUBROUTINE SCAT(Lmax, Ipr, Rrmu)
C***********************************************************************
C     Calculate transmission coefficients t(l,j)                       *
C     and the matrix elements eta(l,j)                                 *
C***********************************************************************
      IMPLICIT DOUBLE PRECISION(A - h, O - Z)
      INCLUDE 'dimension.h'
      DOUBLE PRECISION A, accur, AE, ak, AK2, argmin, av, BETa, BI, BR, 
     &                 c1, c2, CETa, d1, dt1, dt2, dt3, dt4, dt6, E1
      DOUBLE PRECISION ELAb, eps, epstl, eti, etr, fc, fcp, fj, fjmin, 
     &                 fl, gc, gcp, h, one, p1, p2, p3
      DOUBLE PRECISION p4, p5, PI, POT, pote, POTi, POTr, PSIi, PSIip, 
     &                 PSIr, PSIrp, R, r1, r2, r3, r4, r6, RCOul, 
     &                 rcoulb, RE
      DOUBLE PRECISION rho, rint, rm, rv, seven, SI, sl, spo, step, T, 
     &                 t1, t2, t3, t4, t6, two, u1, u2, vcl, volint
      DOUBLE PRECISION VSO, w2, WSO, y1, y2, yy, zero, ZI, ZT, zz
      INTEGER i, IE, ipl, Ipr, IS, IS1, IS2, IS3, IS4, j, k, l, lm1, 
     &        Lmax, lmaxc, LTCMAX, npt, npt3
      PARAMETER(LTCMAX = NDTL)
C     parameter ( ltcmax=100, LFMAX=4*NDTL+2)
      CHARACTER*10 type(7)
      REAL*8 MI, MT, mt3
      REAL*8 a1, a2, a3, a4, a5
      DIMENSION av(7), rv(7), pote(7), volint(7)
      DIMENSION u1(7), y1(7)
      DIMENSION fc(LTCMAX), fcp(LTCMAX), gc(LTCMAX), gcp(LTCMAX)
      COMMON /CONST / MI, SI, ZI, MT, ZT, PI, AK2, CETa, W2L, CSO
      COMMON /ENER  / E1, ELAb
      COMMON /INOUT / IE, IS, IS1, IS2, IS3, IS4
C
      COMMON /POTEN1/ R(7), RE(7), A(7), AE(7), POT(7, 6)
C     COMMON /POTEN2/ RCOul, BETa
C
      COMMON /POTEN2/ RCOul, BETa, EFErmi, EP, EA, IREl
      COMMON /POTN  / POTi(305), POTr(305), VSO(305), WSO(305)
      COMMON /PSI   / PSIr, PSIrp, PSIi, PSIip
      COMMON /TLJ   / BR(3, LTCMAX), BI(3, LTCMAX), T(3, LTCMAX)
      DATA type/'REAL      ', 'IMAG.SURF1', 'IMAG.VOLUM', 'VSO (REAL)', 
     &     'IMAG.SURF2', 'WSO (IMAG)', 'REAL.SURF '/
      DATA argmin/ - 1.745D+02/, zero/0.0D+00/, epstl/1.0D-10/
      DATA eps/1.0D-03/, one/1.0D+00/, two/2.0D+00/, seven/7.0D+00/
C
      DO i = 1, 3
         DO j = 1, LTCMAX
            BR(i, j) = zero
            BI(i, j) = zero
            T(i, j) = zero
         ENDDO
      ENDDO
C
C     Capote 2001
C     Constants
C
C
      w2 = W2L*Rrmu
      ak = DSQRT(AK2)
      zz = ZI*ZT
      eta = CETa*zz*DSQRT(Rrmu/E1)
      vcl = ak*eta/w2
      mt3 = MT**(1.D0/3.D0)
C
      rcoulb = RCOul*mt3
C
C     Radius, Diffuseness, Strength
C
      DO i = 1, 7
         rv(i) = (DABS(R(i)))*mt3
C
         av(i) = A(i)
C
         IF(av(i).LE.zero)av(i) = one
C
         pote(i) = POT(i, 1)
C
         IF(i.LE.3 .AND. pote(i).LT.zero)pote(i) = zero
C
      ENDDO
C
C
C     Matching radius
C
      r1 = rv(1) + seven*A(1)
      r2 = rv(2) + seven*A(2)
      r3 = rv(3) + seven*A(3)
      r4 = rv(4) + seven*A(4)
      r6 = rv(6) + seven*A(6)
      rm = 1.5*DMAX1(r1, r2, r3, r4, r6)
      rho = ak*rm
C
C     Coulomb functions at the matching radius
C
      accur = 1.0D-14
      step = 999.0
C
C     Calculate only usefull l-values
C
      lmaxc = IDINT(2.5*rho**0.8 + 3.5)
      lmaxc = MIN0(lmaxc, LTCMAX)
      CALL RCWFN(rho, eta, 0, lmaxc - 1, fc, fcp, gc, gcp, accur, step)
C
C     Calculate potentials (h = integration step)
C
      npt = 200
      npt3 = npt + 3
      h = rm/DFLOAT(npt)
      IF(BETa.NE.zero)THEN
         pote(3) = zero
         c2 = BETa*BETa/16.
         c1 = 4.*w2*c2
         d1 = DEXP(4.*c2*AK2)
      ENDIF
C
      t1 = one/DEXP(rv(1)/av(1))
      t2 = one/DEXP(rv(2)/av(2))
      t3 = one/DEXP(rv(3)/av(3))
      t4 = one/DEXP(rv(4)/av(4))
      t6 = one/DEXP(rv(6)/av(6))
C
      dt1 = DEXP(h/av(1))
      dt2 = DEXP(h/av(2))
      dt3 = DEXP(h/av(3))
      dt4 = DEXP(h/av(4))
      dt6 = DEXP(h/av(6))
C
      DO i = 1, npt3
         rint = DFLOAT(i)*h
C
C        Real potential
C
         t1 = t1*dt1
         POTr(i) = +pote(1)/(one + t1)
C
C
C        Imaginary volume potential
C
         t3 = t3*dt3
         POTi(i) = +pote(3)/(one + t3)
C
C
C
         IF(R(2).GT.zero)THEN
C
C           Imaginary surface potential (Woods-Saxon derivative)
C
            t2 = t2*dt2
            POTi(i) = POTi(i) + 4.0*pote(2)*t2/((one + t2)**2)
C
C
C
C           Dispersive real surface contribution
C
C           Real surface potential (Woods-Saxon derivative)
C
C           (with the imaginary surface geometry)
C
            POTr(i) = POTr(i) + 4.0*pote(7)*t2/((one + t2)**2)
C
C
         ELSEIF(R(2).LT.zero)THEN
C
C           Imaginary surface potential (Gaussian)
C
            yy = -(((rint-rv(2))/av(2))**2)
            IF(yy.GT.argmin)POTi(i) = POTi(i) + pote(2)*DEXP(yy)
         ENDIF
C
C
         IF(BETa.GT.zero)THEN
C
C           Local equivalent of a non local potential
C
            p1 = c2/(POTr(i)*POTr(i) + POTi(i)*POTi(i))
            p4 = t1/(one + t1)
            p5 = t2/(one + t2)
            p2 = -POTr(i)*p4/av(1)
            p3 = POTi(i)*(one - 2.*p5)/av(2)
            p4 = p2*(one - 2.*p4)/av(1)
            p5 = POTi(i)*(1. - 6.*p5*(1. - p5))/(av(2)*av(2))
            u2 = p1*((POTr(i)*p2 + POTi(i)*p3)*2./rint + POTr(i)
     &           *p4 + POTi(i)*p5)
            y2 = p1*((POTr(i)*p3 - POTi(i)*p2)*2./rint + POTr(i)
     &           *p5 - POTi(i)*p4)
            y1(1) = POTi(i)/(d1*d1 + 2.*d1*c1*POTr(i))
            u1(1) = (POTr(i) + POTi(i)*c1*y1(1))/(d1 + c1*POTr(i))
            DO k = 1, 6
               p1 = c1*y1(k) - y2
               p2 = DSIN(p1)
               p1 = DCOS(p1)
               p3 = one/(d1*DEXP(c1*u1(k) - u2))
               u1(k + 1) = (POTr(i)*p1 + POTi(i)*p2)*p3
               y1(k + 1) = (POTi(i)*p1 - POTr(i)*p2)*p3
            ENDDO
            p1 = u1(7) - 2.*u1(6) + u1(5)
            IF(p1.EQ.zero)THEN
               POTr(i) = u1(7)
            ELSE
               POTr(i) = u1(7) - ((u1(7) - u1(6))**2)/p1
            ENDIF
            p2 = y1(7) - 2.*y1(6) + y1(5)
            IF(p2.EQ.zero)THEN
               POTi(i) = y1(7)
            ELSE
               POTi(i) = y1(7) - ((y1(7) - y1(6))**2)/p2
            ENDIF
         ENDIF
C
C        Real + Coulomb potentials
C
         IF(zz.NE.zero)THEN
            IF(rint.GT.rcoulb)THEN
               POTr(i) = POTr(i) - 2.0*vcl/rint
            ELSE
               POTr(i) = POTr(i) - vcl*(3.0 - (rint/rcoulb)**2)/rcoulb
            ENDIF
         ENDIF
C
C        Spin-orbit potential
C
         t4 = t4*dt4
C        VSO(i) = +2.043655*pote(4)*t4/(av(4)*rint*((one+t4)**2))
         VSO(i) = CSO*pote(4)*t4/(av(4)*rint*((one+t4)**2))
C
C        Imaginary Spin-orbit potential (Capote 2001)
C
         t6 = t6*dt6
C        WSO(i) = +2.043655*pote(6)*t6/(av(6)*rint*((one+t6)**2))
         WSO(i) = CSO*pote(6)*t6/(av(6)*rint*((one+t6)**2))
         POTr(i) = -POTr(i)*w2
         POTi(i) = -POTi(i)*w2
         VSO(i) = -VSO(i)*w2
         WSO(i) = -WSO(i)*w2
      ENDDO
      ipl = IDINT(two*SI + one + eps)
C
C     For each l-value
C
      DO l = 1, lmaxc
         Lmax = l
         fl = DFLOAT(l - 1)
         sl = fl*(fl + one)
         fj = fl - SI - one
         fjmin = DABS(fl - SI)
         DO j = 1, ipl
            fj = fj + one
            IF(fj.GE.fjmin)THEN
               spo = fj*(fj + one) - fl*(fl + one) - SI*(SI + one)
               IF(SI.EQ.one)spo = spo/two
C
               CALL INTEG(npt, h, sl, spo)
C
C              psir  =                     real    part of the internal function
C              psirp = derivative of the   real    part of the internal function
C              psii  =                   imaginary part of the internal function
C              psiip = derivative of the imaginary part of the internal function
C
               t1 = fc(l)
               t2 = fcp(l)*ak
               t3 = gc(l)
               t4 = gcp(l)*ak
               a1 = -(t1*PSIrp - t2*PSIr + t3*PSIip - t4*PSIi)
               a2 = -(t1*PSIip - t2*PSIi - t3*PSIrp + t4*PSIr)
               a3 = +(t1*PSIrp - t2*PSIr - t3*PSIip + t4*PSIi)
               a4 = +(t1*PSIip - t2*PSIi + t3*PSIrp - t4*PSIr)
               a5 = a1*a1 + a2*a2
C
C              R.Capote 11/99 to avoid division by 0 under charged particle
C              threshold
               IF(a5.NE.0.D0)THEN
C
C                 a1  =    real    part of the denominator
C                 a2  =  imaginary part of the denominator
C                 a3  =    real    part of the numerator
C                 a4  =  imaginary part of the numerator
C                 a5  =   modulus       of the denominator
C                 etr = 1.0 - real part of the matrix element
C                 eti =  imaginary part of the matrix element
C
                  etr = one - (a3*a1 + a2*a4)/a5
                  eti = (a4*a1 - a2*a3)/a5
                  BR(j, l) = etr
                  BI(j, l) = eti
                  T(j, l) = one - (a3*a3 + a4*a4)/a5
               ENDIF
C
            ENDIF
         ENDDO
         IF(BR(ipl, l).LE.zero)GOTO 100
         IF(DABS(T(ipl,l)).LT.DABS(T(ipl,1)*epstl))GOTO 100
      ENDDO
      GOTO 200
 100  DO j = 1, ipl
         BR(j, Lmax) = zero
         BI(j, Lmax) = zero
         T(j, Lmax) = zero
      ENDDO
      Lmax = Lmax - 1
 200  lm1 = Lmax - 1
      volint(1) = 4.0*PI*rv(1)*pote(1)*(one*rv(1)*rv(1) + (PI*av(1))**2)
     &            /(3.0*MT)
      IF(rv(2).GT.zero)volint(2) = 16.0*PI*rv(2)*rv(2)*av(2)*pote(2)
     &                             *(one + ((PI*av(2)/rv(2))**2)/3.0)/MT
      IF(rv(7).GT.zero)volint(7) = 16.0*PI*rv(7)*rv(7)*av(7)*pote(7)
     &                             *(one + ((PI*av(7)/rv(7))**2)/3.0)/MT
      volint(3) = 4.0*PI*rv(3)*pote(3)*(one*rv(3)*rv(3) + (PI*av(3))**2)
     &            /(3.0*MT)
C
      volint(4) = zero
      volint(5) = zero
      volint(6) = zero
      IF(Ipr.GT.0)THEN
         WRITE(IS, 99001)
C
C        Formats
C
99001    FORMAT(' ', 14x, 
     &          'TRANSMISSION COEFFICIENTS CALCULATED FROM THE', 
     &          ' FOLLOWING OPTICAL MODEL PARAMETERS')
         WRITE(IS, 99002)
99002    FORMAT(' ', 14x, 80('*'), ///)
         WRITE(IS, 99003)ZI, MI, ZT, MT
99003    FORMAT(' ', 34x, 'CHARGE', 29x, 'MASS', //, 15x, 'PROJECTILE', 
     &          10x, f6.1, 20x, 1p, e13.6, /, 15x, 'TARGET', 14x, 0p, 
     &          f6.1, 20x, 1p, e13.6, //)
         WRITE(IS, 99004)
99004    FORMAT(
     &' POTENTIALS PARAMETERS CALCULATED FOR A GIVEN ENERGY FUNCTIONAL B
     &Y USER'/)
         WRITE(IS, 99005)ELAb, E1
99005    FORMAT(/' LABORATORY ENERGY : ', f9.3, 
     &          ' MeV'/'        CMS ENERGY : ', f9.3, ' MeV'/)
         WRITE(IS, 99006)ak, eta, rm, h, npt, lm1
C
99006    FORMAT(' ', 'K =', 1p, e12.5, 5x, 'ETA =', e12.5, 9x, 'RM =', 
     &          e12.5, 2x, 'DR =', e12.5, 8x, i4, ' POINTS', 10x, 
     &          'LMAX =', i3, /)
         IF(R(2).LT.zero)THEN
            WRITE(IS, *)
            WRITE(IS, *)
     &             'Gaussian FORM-FACTOR is used for surface absoprtion'
            WRITE(IS, *)
         ENDIF
         WRITE(IS, 99007)
99007    FORMAT(' ', 'POTENTIAL ', 2x, 'STRENGTH', 2x, '  r0  ', 2x, 
     &          'RADIUS', 2x, 'DIFFUSENESS', 2x, 'VOLUME INTEG.')
         DO j = 1, 7
C
            IF(av(j).EQ.1 .AND. pote(j).EQ.0)av(j) = 0.
            IF(j.NE.5)WRITE(IS, 99008)type(j), pote(j), ABS(R(j)), 
     &                                rv(j), av(j), volint(j)
99008       FORMAT(' ', a10, 1x, f9.4, 1x, f7.4, 1x, f7.4, 3x, f7.4, 6x, 
     &             f8.2)
         ENDDO
         WRITE(IS, 99009)rcoulb, BETa, EFErmi, EP
99009    FORMAT(' COULOMB   ', 11x, 
     &          f7.4//' NON LOCALITY PARAMETER(BETA)=', 
     &          f7.4//' FERMI ENERGY =', f9.4, 
     &          ' MeV'//' AVERAGE ENERGY OF SPL Ep =', f9.4, ' MeV'/)
         IF(EA.LT.1000.D0)THEN
            WRITE(IS, 99010)
99010       FORMAT(' NONLOCALITY IN VOLUME ABSORPTION IS CONSIDERED :')
            WRITE(IS, 99011)EA
99011       FORMAT('   ENERGY ABOVE WHICH NONLOCALITY IS ASSUMED Ea =', 
     &             f9.4, ' MeV'//)
         ELSE
            WRITE(IS, 99012)
99012       FORMAT(' NONLOCALITY IN VOLUME ABSORPTION IS NOT CONSIDERED'
     &             //)
         ENDIF
         WRITE(IS, 99013)
99013    FORMAT(' ', 56x)
      ENDIF
      END
C
C
C
      SUBROUTINE SHAPEC(Lmax, Ida, Na, Ipl, Ipr)
C***********************************************************************
C     Shape elastic differential cross-section (charged-particles)     *
C***********************************************************************
      IMPLICIT DOUBLE PRECISION(a - H, O - z)
      INCLUDE 'dimension.h'
      DOUBLE PRECISION ak, AK2, al, ANG, arg, BI, BR, CANg, cost, dsig, 
     &                 dsigr, ELAcs, ELAda, ETA, one, PI, PL, PL1, PL2
      DOUBLE PRECISION pol20, pol21, pol22, polar, rad, rap, SI, sig0, 
     &                 sigl, sint, som, T, ten, three, tmpang, TOTcs, 
     &                 two, twosr, W2, zero
      DOUBLE PRECISION ZI, ZT
      INTEGER i, Ida, IE, Ipl, Ipr, IS, IS1, IS2, IS3, IS4, j, jmax, 
     &        jmin, l, Lmax, LTCMAX, Na, naa, NELang
C
      PARAMETER(LTCMAX = NDTL)
C     parameter ( ltcmax=100, LFMAX=4*NDTL+2)
      CHARACTER angtyp(2)*9
      COMPLEX*16 a(101), b(101), c(101), d(101), e(101)
      COMPLEX*16 ai, onec, zeroc
      COMPLEX*16 CGAMMA
      COMPLEX*16 csigl(LTCMAX), etac(3, LTCMAX), fc(101)
      COMPLEX*16 c1(LTCMAX), c2(LTCMAX), c3(LTCMAX), c4(LTCMAX), 
     &           c5(LTCMAX)
      COMPLEX*16 z, z1, z2, z3, z4, z5
      REAL*8 MI, MT
      DIMENSION sigl(LTCMAX), dsig(101), dsigr(101)
      DIMENSION rap(101), polar(101), pol20(101), pol21(101), pol22(101)
      DIMENSION tmpang(101)
      COMMON /ANGULAR/ ANG(73), CANg(101), PL(2*LTCMAX - 1, 101), 
     &                 PL1(2*LTCMAX - 1, 101), PL2(2*LTCMAX - 1, 101)
      COMMON /CONST / MI, SI, ZI, MT, ZT, PI, AK2, ETA, W2, CSO
      COMMON /INOUT / IE, IS, IS1, IS2, IS3, IS4
      COMMON /TLJ   / BR(3, LTCMAX), BI(3, LTCMAX), T(3, LTCMAX)
C
C-----next COMMON is to transfer elastic ddx to Empire
      COMMON /ELASCAT/ ELAda(101), TOTcs, ELAcs, NELang
C
      DATA ai/(0.0, 1.0)/
      DATA onec/(1.0, 0.0)/
      DATA zeroc/(0.0, 0.0)/
      DATA angtyp/'     TETA', 'COS(TETA)'/
C
      DATA zero/0.0D+00/, one/1.0D+00/, two/2.0D+00/, three/3.D0/
C
      DATA ten/1.0D+01/
C
C
C
      twosr = DSQRT(two)
C
      NELang = Na
      IS3 = 7
      rad = 1.8D+02/PI
      ak = SQRT(AK2)
C
C     Initialisations
C
      DO i = 1, Na
         rap(i) = one
         dsig(i) = zero
         dsigr(i) = one
         polar(i) = zero
         fc(i) = zeroc
      ENDDO
      DO i = 1, 101
         pol20(i) = 0.0
         pol21(i) = 0.0
         pol22(i) = 0.0
      ENDDO
C
C     Store angles or cosines into a temporary array
C
      IF(Ida.EQ.1)THEN
         DO i = 1, Na
            tmpang(i) = ANG(i)
         ENDDO
      ELSEIF(Ida.EQ.2)THEN
         DO i = 1, Na
            tmpang(i) = CANg(i)
         ENDDO
      ENDIF
C
      DO l = 1, Lmax
         csigl(l) = onec
         DO i = 1, Ipl
            etac(i, l) = DCMPLX(one - BR(i, l), BI(i, l))
         ENDDO
      ENDDO
C
      IF(ETA.GT.zero)THEN
C
C        Coulomb phase shifts
C
         z = DCMPLX(one, ETA)
         z = CGAMMA(z)
C        sig0 = dimag(cdlog(z))
         sig0 = DIMAG(LOG(z))
C
         som = zero
         DO l = 1, Lmax
            al = DFLOAT(l)
            sigl(l) = sig0 + som
            csigl(l) = CDEXP(two*ai*sigl(l))
            som = som + DATAN2(ETA, al)
         ENDDO
C
C        Coulomb diffusion amplitude
C
         DO i = 1, Na
            arg = (one - CANg(i))/two
            IF(arg.NE.zero)THEN
               z = -ai*ETA*DLOG(arg) + two*ai*sig0
               fc(i) = -ETA*CDEXP(z)/(two*ak*arg)
               dsigr(i) = CDABS(fc(i))**2
               dsigr(i) = ten*dsigr(i)
            ENDIF
         ENDDO
      ENDIF
C
      IF(Ipl.EQ.1)THEN
C
C        spin 0 particles
C        ****************
         DO l = 1, Lmax
            al = DFLOAT(l - 1)
            c1(l) = csigl(l)*(two*al + one)*(onec - etac(1, l))
         ENDDO
C
         DO i = 1, Na
            z1 = zeroc
            DO l = 1, Lmax
               z1 = z1 + c1(l)*PL(l, i)
            ENDDO
            a(i) = fc(i) + ai*z1/(two*ak)
            dsig(i) = CDABS(a(i))**2
            dsig(i) = ten*dsig(i)
         ENDDO
C
         DO i = 2, Na
            rap(i) = dsig(i)/dsigr(i)
         ENDDO
C
      ELSEIF(Ipl.EQ.2)THEN
C
C        spin 1/2 particles
C        ******************
         DO l = 1, Lmax
            al = DFLOAT(l - 1)
            c1(l) = csigl(l)
     &              *((al + one)*(onec - etac(2,l)) + al*(onec - etac(1,
     &              l)))
            c2(l) = csigl(l)*(etac(2, l) - etac(1, l))
         ENDDO
C
         DO i = 1, Na
            z1 = zeroc
            z2 = zeroc
            DO l = 1, Lmax
               z1 = z1 + c1(l)*PL(l, i)
               z2 = z2 + c2(l)*PL1(l, i)
            ENDDO
            a(i) = fc(i) + ai*z1/(two*ak)
            b(i) = -ai*z2/(two*ak)
            dsig(i) = CDABS(a(i))**2 + CDABS(b(i))**2
            dsig(i) = ten*dsig(i)
            polar(i) = 2000.0*DIMAG(a(i)*DCONJG(b(i)))/dsig(i)
         ENDDO
C
         IF(ETA.GT.zero)THEN
            DO i = 2, Na
               rap(i) = dsig(i)/dsigr(i)
            ENDDO
         ENDIF
C
      ELSEIF(Ipl.EQ.3)THEN
C
C        spin 1 particles
C        ****************
         DO l = 1, Lmax
            al = DFLOAT(l - 1)
            c1(l) = -csigl(l)
     &              *((al + one)*(onec - etac(3,l)) + (al)*(onec - 
     &              etac(1,l)))
            c2(l) = -csigl(l)
     &              *((al + two)*(onec - etac(3,l)) + (two*al + one)
     &              *(onec - etac(2,l)) + (al - one)*(onec - etac(1,l)))
            c3(l) = -csigl(l)*((onec - etac(3,l)) - (onec - etac(1,l)))
            c4(l) = -csigl(l)
     &              *((al*(al+two))*(onec - etac(3,l)) - (two*al + one)
     &              *(onec - etac(2,l)) - (al - one)*(al + one)
     &              *(onec - etac(1,l)))
            c5(l) = -csigl(l)*((al)*(onec - etac(3,l)) - (two*al + one)
     &              *(onec - etac(2,l)) + (al + one)*(onec - etac(1,l)))
         ENDDO
C
         DO i = 2, Na
            cost = PL(2, i)
            sint = DSQRT(one - cost*cost)
            z1 = zeroc
            z2 = zeroc
            z3 = zeroc
            z4 = zeroc
            z5 = zeroc
            DO l = 1, Lmax
               al = DFLOAT(l - 1)
               z1 = z1 + c1(l)*PL(l, i)
               z2 = z2 + c2(l)*PL(l, i)/two
               z3 = z3 + c3(l)*PL1(l, i)/twosr
            ENDDO
C
            DO l = 2, Lmax
               al = DFLOAT(l - 1)
               z4 = z4 + c4(l)*PL1(l, i)/(al*(al + one)*twosr)
               z5 = z5 + c5(l)*PL2(l, i)/(al*(al + one)*two)
            ENDDO
C
            a(i) = fc(i) + z1/(two*ai*ak)
            b(i) = fc(i) + z2/(two*ai*ak)
            c(i) = z3/(two*ai*ak)
            d(i) = z4/(two*ai*ak)
            e(i) = z5/(two*ai*ak)
C           chk  = b(i) - a(i) - e(i) + twosr*(d(i) - c(i))*cost/sint
            dsig(i) = CDABS(a(i))
     &                **2 + two*(CDABS(b(i))**2 + CDABS(c(i))**2 + 
     &                CDABS(d(i))**2 + CDABS(e(i))**2)
            dsig(i) = ten*dsig(i)/three
            polar(i) = 1.0D+03*two*twosr*DIMAG(a(i)*DCONJG(c(i)) + b(i)
     &                 *DCONJG(d(i)) + d(i)*DCONJG(e(i)))
     &                 /(three*dsig(i))
            pol20(i) = 1.0D+02*(one - ten*(CDABS(a(i))**2 + two*CDABS(d(
     &                 i))**2)/dsig(i))/twosr
            pol21(i) = -1.0D+03*twosr*DBLE((a(i)*DCONJG(c(i)) - b(i)*
     &                 DCONJG(d(i)) + d(i)*DCONJG(e(i))))
     &                 /(dsig(i)*DSQRT(three))
            pol22(i) = 1.0D+03*(two*DBLE((b(i)*DCONJG(e(i))))
     &                 - CDABS(c(i))**2)/(dsig(i)*DSQRT(three))
         ENDDO
C
         DO i = 2, Na
            rap(i) = dsig(i)/dsigr(i)
         ENDDO
C
      ENDIF
C
      IF(Ipr.GT.0)THEN
C
C        Print angular distribution
C
         WRITE(IS, 99001)
C--------------------  done ---------------
C
C        Formats
C
99001    FORMAT(' ', 46x, 'SHAPE ELASTIC DIFFERENTIAL CROSS-SECTION', /, 
     &          ' ', 46x, 40('*'), /, ' ', 56x, 'CENTER-OF-MASS SYSTEM', 
     &          ///)
C
C        Spin 0 and 1/2 particles
         naa = Na/2 + 1
         WRITE(IS, 99002)angtyp(Ida), angtyp(Ida)
99002    FORMAT(/' ', 4x, a9, 3x, 'DSIG(TETA)', 2x, 'DSIGR(TETA)', 3x, 
     &          'DSIG/DSIGR', 1x, 'POLARIZATION', 2x, 4x, a9, 3x, 
     &          'DSIG(TETA)', 2x, 'DSIGR(TETA)', 3x, 'DSIG/DSIGR', 1x, 
     &          'POLARIZATION', /)
         DO i = 1, naa
            jmin = 2*(i - 1) + 1
            jmax = MIN0(2*i, Na)
            WRITE(IS, 99003)(tmpang(j), dsig(j), dsigr(j), rap(j), 
     &                      polar(j), j = jmin, jmax)
99003       FORMAT(' ', 1p, 5E13.5, 2x, 5E13.5)
         ENDDO
      ELSE
C
C        Spin 1 particles
         naa = Na
         WRITE(IS, 99004)angtyp(Ida), angtyp(Ida)
99004    FORMAT(/' ', 4x, a9, 3x, 'DSIG(TETA)', 2x, 'DSIGR(TETA)', 3x, 
     &          'DSIG/DSIGR', 2x, 'POLAR. VECT', 4x, 'POLAR. 20', 4x, 
     &          'POLAR. 21', 4x, 'POLAR. 22', 4x, a9, /)
         DO i = 1, naa
            WRITE(IS, 99005)tmpang(i), dsig(i), dsigr(i), rap(i), 
     &                      polar(i), pol20(i), pol21(i), pol22(i), 
     &                      ANG(i)
99005       FORMAT(' ', 1p, 9E13.5)
         ENDDO
      ENDIF
C
C     if(ipr .gt. 0) then
C     if(ipl .ne. 3) then
C     do i=1,na
C     write(is3,9040) tmpang(i),dsig(i),dsigr(i),rap(i),polar(i)
C     end do
C     else
C     do i=1,na
C     write(is3,9040) tmpang(i),dsig(i),dsigr(i),rap(i),polar(i),
C     *      pol20(i),pol21(i),pol22(i)
C     end do
C     endif
C     endif
C-----transmission of elastic ddx to Empire
      IF(Ida.EQ.1 .AND. Ipr.GT.0)THEN
         DO i = 1, Na
            ELAda(i) = dsig(i)
         ENDDO
      ENDIF
99006 FORMAT(1p, 8E10.3)
      END
C
C
      SUBROUTINE SHAPEL2(Lmax, Ida, Na, Ipl, Ipr)
C***********************************************************************
C     Shape elastic differential cross-section for neutrons            *
C                                                                      *
C     BB are Blatt and Biedenharn coefficients, see tabulated values   *
C     in L.C.Biedenharn  ORNL-1501 (1953)                              *
C***********************************************************************
      IMPLICIT DOUBLE PRECISION(A - H, O - Z)
      INCLUDE 'dimension.h'
      DOUBLE PRECISION aj1, aj2, aj2max, aj2min, AK2, al, al1, al2, ANG, 
     &                 b1, bb, BI, bi1, bl, BR, br1, C, cg, CLEBG, da
      DOUBLE PRECISION eight, ELAcs, ELAda, eps, ETA, four, one, PI, PL, 
     &                 PL1, PL2, rac, RACAH, rad, s, SI, som, sym, T, 
     &                 ten
      DOUBLE PRECISION TOTcs, two, W2, z1, z2, zero, ZI, ZT
      INTEGER i, Ida, idaa, IE, ij1, ij2, il, il1, il2, il2max, il2min, 
     &        imax, imin, Ipl, Ipr, IS, IS1, IS2, IS3, IS4
      INTEGER j, js1, js2, kpar, l, lm, lma, Lmax, lmax2, lmi, LTCMAX, 
     &        Na, naa, ncoup, NELang, nl
C
      PARAMETER(LTCMAX = NDTL)
C     parameter ( ltcmax=100, LFMAX=4*NDTL+2)
      REAL*8 MI, MT
      DIMENSION bl(61), da(101)
      COMMON /ANGULAR/ ANG(73), C(101), PL(2*LTCMAX - 1, 101), 
     &                 PL1(2*LTCMAX - 1, 101), PL2(2*LTCMAX - 1, 101)
      COMMON /CONST / MI, SI, ZI, MT, ZT, PI, AK2, ETA, W2, CSO
      COMMON /INOUT / IE, IS, IS1, IS2, IS3, IS4
      COMMON /TLJ   / BR(3, LTCMAX), BI(3, LTCMAX), T(3, LTCMAX)
C
C-----next COMMON is to transfer elastic ddx to Empire
C
      COMMON /ELASCAT/ ELAda(101), TOTcs, ELAcs, NELang
C
C
C
      DATA zero/0.0D+00/, one/1.0D+00/, two/2.0D+00/
C
      DATA four/4.D0/, ten/1.0D+01/, eps/0.001D0/, eight/8.0D+00/
C
C
      NELang = Na
      rad = 1.8D+02/PI
      idaa = IABS(Ida)
      lmax2 = 2*Lmax - 1
      ncoup = 0
      DO il = 1, lmax2
         al = DFLOAT(il - 1)
         som = zero
         DO il1 = 1, Lmax
            al1 = DFLOAT(il1 - 1)
            aj1 = al1 - SI - one
            il2min = MAX0(il1, IABS(il1 - il) + 1)
            kpar = il + il1 + il2min
            IF(2*(kpar/2).EQ.kpar)il2min = il2min + 1
            il2max = MIN0(il1 + il - 1, Lmax)
            IF(il2min.LE.il2max)THEN
               DO ij1 = 1, Ipl
                  aj1 = aj1 + one
                  js1 = 100*il1 + ij1
                  IF(aj1.GE.DABS(al1 - SI))THEN
                     br1 = BR(ij1, il1)
                     bi1 = BI(ij1, il1)
                     z1 = (two*al1 + one)*(two*aj1 + one)
                     aj2min = DMAX1(aj1, DABS(aj1 - al)) - eps
                     aj2max = aj1 + al + eps
                     DO il2 = il2min, il2max, 2
                        al2 = DFLOAT(il2 - 1)
                        aj2 = al2 - SI - one
                        DO ij2 = 1, Ipl
                           aj2 = aj2 + one
                           js2 = 100*il2 + ij2
                           IF((aj2 - aj2min)*(aj2 - aj2max).LE.zero)THEN
                              IF(js1.NE.js2)THEN
                                 sym = 2.0
                              ELSE
                                 sym = 1.0
                              ENDIF
                              cg = CLEBG(al1, al2, al, zero, zero, zero)
                              rac = RACAH(al1, aj1, al2, aj2, SI, al)
                              z2 = (two*al2 + one)*(two*aj2 + one)
                              bb = z1*z2*cg*cg*rac*rac
                              s = br1*BR(ij2, il2) + bi1*BI(ij2, il2)
                              som = som + bb*sym*s
                              ncoup = ncoup + 1
                           ENDIF
                        ENDDO
                     ENDDO
                  ENDIF
               ENDDO
            ENDIF
         ENDDO
         bl(il) = ten*som/(eight*AK2)
      ENDDO
C
C     Angular distribution
C
      DO i = 1, Na
         som = zero
         DO l = 1, lmax2
            som = som + bl(l)*PL(l, i)
         ENDDO
         da(i) = som
      ENDDO
C
C     Integral of the angular distribution
C
      som = zero
      IF(idaa.EQ.1)THEN
         DO i = 2, Na
            som = som + (da(i)*DSIN(ANG(i)/rad) + da(i - 1)
     &            *DSIN(ANG(i-1)/rad))/two
         ENDDO
         som = som*2.5/rad
      ELSEIF(idaa.EQ.2)THEN
         DO i = 1, Na
            som = som + da(i)
         ENDDO
         som = som - (da(1) + da(Na))/two
         som = som*0.02
      ENDIF
      som = two*PI*som
      IF(Ipr.GT.0)THEN
C
C        Print angular distribution
C
         WRITE(IS1, 99001)
C--------------------  done ---------------
C
C        Formats
C
99001    FORMAT(' ', 46x, 'SHAPE ELASTIC DIFFERENTIAL CROSS-SECTION', /, 
     &          ' ', 46x, 40('*'), /, ' ', 56x, 'CENTER-OF-MASS SYSTEM', 
     &          ///)
         naa = Na/4 + 1
         IF(idaa.EQ.1)THEN
            WRITE(IS1, 99002)
99002       FORMAT(' ', 5x, 4('    TETA ', 2x, 'D.SIGMA/D.OMEGA', 6x), 
     &             /)
            DO i = 1, naa
               imin = 4*(i - 1) + 1
               imax = MIN0(4*i, Na)
               WRITE(IS1, 99004)(ANG(j), da(j), j = imin, imax)
            ENDDO
         ELSEIF(idaa.EQ.2)THEN
            WRITE(IS1, 99003)
99003       FORMAT(' ', 5x, 4('COS(TETA)', 2x, 'D.SIGMA/D.OMEGA', 6x), 
     &             /)
            DO i = 1, naa
               imin = 4*(i - 1) + 1
               imax = MIN0(4*i, Na)
               WRITE(IS1, 99004)(C(j), da(j), j = imin, imax)
            ENDDO
         ENDIF
         b1 = four*PI*bl(1)
         WRITE(IS1, 99005)som, b1
         WRITE(IS, 99005)som, b1
C        write(is,9050) ncoup
C
C        Print Legendre coefficients (ENDF prescription)
C
         DO l = 2, lmax2
            al = DFLOAT(l - 1)
            bl(l) = bl(l)/((two*al + one)*bl(1))
         ENDDO
         bl(1) = one
         WRITE(IS1, 99006)
         WRITE(IS, 99006)
         nl = (lmax2 + 4)/5
         DO l = 1, nl
            lmi = 5*(l - 1)
            lma = MIN0(5*l, lmax2) - 1
            WRITE(IS1, 99007)(lm, bl(lm + 1), lm = lmi, lma)
            WRITE(IS, 99007)(lm, bl(lm + 1), lm = lmi, lma)
         ENDDO
      ENDIF
C-----transmission of elastic ddx to Empire
      IF(Ida.EQ.1 .AND. Ipr.GT.0)THEN
         DO i = 1, Na
            ELAda(i) = da(i)
         ENDDO
      ENDIF
99004 FORMAT(' ', 5x, 4(1p, e12.5, 2x, e12.5, 6x))
99005 FORMAT(/, 2x, 'INTEGRAL =', 1p, e12.5, ' mb', 10x, 'BL(0) =', 
     &       e12.5, ' mb')
C9050 format(1h ,90x,7hNCOUP =,i8)
99006 FORMAT(/10x, 'LEGENDRE COEFFICIENTS(ENDF prescription)'//, ' ', 
     &       5(2x, 'L', 7x, 'BL(L)', 10x), /)
99007 FORMAT(' ', 5(i3, 3x, 1p, e14.7, 5x))
      END
C
C
      SUBROUTINE SPIN0(Lmax, Ipr)
C***********************************************************************
C     Print transmission coefficients T(l,j)                           *
C     Calculate cross sections: - compound nucleus                     *
C                                 shape elastic                        *
C                                 total                                *
C     for spin 0 incident particles                                    *
C***********************************************************************
      IMPLICIT DOUBLE PRECISION(A - H, O - Z)
      INCLUDE 'dimension.h'
      DOUBLE PRECISION AK2, al2, BI, BR, E1, ELAb, ELAcs, ELAda, ETA, 
     &                 one, PI, SE, SI, SR, ST, T, TC, ten, tm
      DOUBLE PRECISION TOTcs, two, W2, zero, ZI, ZT
      INTEGER IE, Ipr, IS, IS1, IS2, IS3, IS4, k, l, Lmax, LTCMAX, 
     &        NELang
C
      PARAMETER(LTCMAX = NDTL)
C     parameter ( ltcmax=100, LFMAX=4*NDTL+2)
      REAL*8 MI, MT
      COMMON /CONST / MI, SI, ZI, MT, ZT, PI, AK2, ETA, W2, CSO
      COMMON /ENER  / E1, ELAb
      COMMON /INOUT / IE, IS, IS1, IS2, IS3, IS4
      COMMON /TCE   / TC(LTCMAX)
      COMMON /TLJ   / BR(3, LTCMAX), BI(3, LTCMAX), T(3, LTCMAX)
      COMMON /XS    / SE, SR, ST
      COMMON /ELASCAT/ ELAda(101), TOTcs, ELAcs, NELang
C
      DATA zero/0.0D+00/, one/1.0D+00/, two/2.0D+00/, ten/1.0D+01/
      SE = zero
      SR = zero
      ST = zero
      DO l = 1, Lmax
         k = l - 1
         al2 = two*DFLOAT(l - 1) + one
         tm = T(1, l)
         TC(l) = tm
         SR = SR + al2*(two*BR(1, l) - BR(1, l)*BR(1, l) - BI(1, l)
     &        *BI(1, l))
         SE = SE + al2*(BR(1, l)*BR(1, l) + BI(1, l)*BI(1, l))
         ST = ST + al2*two*BR(1, l)
      ENDDO
      WRITE(IS, 99001)
99001 FORMAT(' ', 3x, 'L', 7x, 'TC(L)', 3x, '1 - ETA R', 7x, 'ETA I', /)
      DO l = 1, Lmax
         k = l - 1
         WRITE(IS, 99002)k, TC(l), BR(1, l), BI(1, l)
99002    FORMAT(' ', i4, 1p, 3E12.4)
      ENDDO
C
C     Cross sections (in mb)
C
      SE = ten*SE*PI/AK2
      SR = ten*SR*PI/AK2
      ST = ten*ST*PI/AK2
      IF(Ipr.NE.0)THEN
         WRITE(IS, 99004)ST, SR, SE
         WRITE(IS1, 99004)ST, SR, SE
      ENDIF
      ELAcs = SE
      TOTcs = ST
C
C     Formats
C
99003 FORMAT(1p, 6E11.4)
99004 FORMAT(//, 
     &       '  Results provided by Spherical Optical Model code SCAT2:'
     &       , //, 2x, 'Total cross section         :', e14.7, ' mb', /, 
     &       2x, 'Absorption cross section    :', e14.7, ' mb', /, 2x, 
     &       'Shape elastic cross section :', e14.7, ' mb', ///)
      END
C
C
      SUBROUTINE SPIN05(Lmax, Ipr)
C***********************************************************************
C     Print transmission coefficients T(l,j)                           *
C     Calculate cross sections: - compound nucleus                     *
C                                 shape elastic                        *
C                                 total                                *
C     for spin 1/2 incident particles                                  *
C***********************************************************************
      IMPLICIT DOUBLE PRECISION(A - H, O - Z)
      INCLUDE 'dimension.h'
      DOUBLE PRECISION A, AE, AK2, al, al1, al2, b1, b2, BI, BR, E1, 
     &                 ELAb, ELAcs, ELAda, ETA, four, one, p1, PI
      DOUBLE PRECISION POT, R, r2, RE, rp, s0, s1, SE, SI, SR, ST, T, 
     &                 TC, ten, tm, TOTcs, two, W2, zero, ZI
      DOUBLE PRECISION ZT
      INTEGER i, IE, Ipr, IS, IS1, IS2, IS3, IS4, k, l, Lmax, LTCMAX, 
     &        NELang
C
      PARAMETER(LTCMAX = NDTL)
C     parameter ( ltcmax=100, LFMAX=4*NDTL+2)
      REAL*8 MI, MT
      COMMON /CONST / MI, SI, ZI, MT, ZT, PI, AK2, ETA, W2, CSO
      COMMON /ENER  / E1, ELAb
      COMMON /INOUT / IE, IS, IS1, IS2, IS3, IS4
C
      COMMON /POTEN1/ R(7), RE(7), A(7), AE(7), POT(7, 6)
      COMMON /TCE   / TC(LTCMAX)
      COMMON /TLJ   / BR(3, LTCMAX), BI(3, LTCMAX), T(3, LTCMAX)
      COMMON /XS    / SE, SR, ST
      COMMON /ELASCAT/ ELAda(101), TOTcs, ELAcs, NELang
      DATA zero/0.0D+00/, one/1.0D+00/, two/2.0D+00/
C
      DATA four/4.D0/, ten/1.0D+01/
C
      SE = zero
      SR = zero
      ST = zero
      DO l = 1, Lmax
         k = l - 1
         al = DFLOAT(k)
         al1 = al + one
         al2 = two*al + one
         tm = (al1*T(2, l) + al*T(1, l))/al2
         TC(l) = tm
         SR = SR + al1*(two*BR(2, l) - BR(2, l)*BR(2, l) - BI(2, l)
     &        *BI(2, l))
     &        + al*(two*BR(1, l) - BR(1, l)*BR(1, l) - BI(1, l)*BI(1, l)
     &        )
         SE = SE + al1*(BR(2, l)*BR(2, l) + BI(2, l)*BI(2, l))
     &        + al*(BR(1, l)*BR(1, l) + BI(1, l)*BI(1, l))
         ST = ST + al1*(two*BR(2, l)) + al*(two*BR(1, l))
      ENDDO
      WRITE(IS, 99001)
99001 FORMAT(' ', 3x, 'L', 7x, 'TC(L)', 3x, '1 - ETA R', 7x, 'ETA I', 
     &       9x, 'T(L,L-1/2)', 3x, '1 - ETA R', 7x, 'ETA I', 9x, 
     &       'T(L,L+1/2)', 3x, '1 - ETA R', 7x, 'ETA I', /)
      DO l = 1, Lmax
         k = l - 1
         al = DFLOAT(k)
         al1 = al + one
         al2 = two*al + one
         b1 = (al1*BR(2, l) + al*BR(1, l))/al2
         b2 = (al1*BI(2, l) + al*BI(1, l))/al2
         WRITE(IS, 99002)k, TC(l), b1, b2, 
     &                   (T(i, l), BR(i, l), BI(i, l), i = 1, 2)
99002    FORMAT(' ', i4, 1p, 3E12.4, 2(7x, 3E12.4))
      ENDDO
C
C     Cross sections (in mb)
C
      SE = ten*SE*PI/AK2
      SR = ten*SR*PI/AK2
      ST = ten*ST*PI/AK2
      IF(Ipr.GT.0)THEN
         WRITE(IS, 99004)ST, SR, SE
         WRITE(IS1, 99004)ST, SR, SE
      ENDIF
      IF(E1.LT.1.0D-01)THEN
         s0 = TC(1)/(two*PI*SQRT(1.0D+06*E1))
         rp = (R(1) + RE(1))*(MT**0.333333333)
         r2 = rp*rp
         p1 = (AK2*r2)/(one + AK2*r2)
         s1 = TC(2)/(two*PI*p1*SQRT(1.0D+06*E1))
         rp = SQRT(SE/(four*ten*PI))
         IF(Ipr.GT.0)WRITE(IS1, 99005)s0, s1, rp
         WRITE(IS, 99005)s0, s1, rp
      ENDIF
      ELAcs = SE
      TOTcs = ST
C
C     Formats
C
99003 FORMAT(1p, 6E11.4)
99004 FORMAT(//, 
     &       '  Results provided by Spherical Optical Model code SCAT2:'
     &       , //, 2x, 'Total cross section         :', e14.7, ' mb', /, 
     &       2x, 'Absorption cross section    :', e14.7, ' mb', /, 2x, 
     &       'Shape elastic cross section :', e14.7, ' mb', ///)
99005 FORMAT(' ', 27x, 'Strength functions   S0', 1pe14.7, /, ' ', 48x, 
     &       'S1', e14.7, /, ' ', 33x, 'Scattering radius', e14.7, //)
      END
C
C
      SUBROUTINE SPIN1(Lmax, Ipr)
C***********************************************************************
C     Print transmission coefficients T(l,j)                           *
C     Calculate cross sections: - compound nucleus                     *
C                                 shape elastic                        *
C                                 total                                *
C     for spin 1 incident particles                                    *
C***********************************************************************
      IMPLICIT DOUBLE PRECISION(A - H, O - Z)
      INCLUDE 'dimension.h'
      DOUBLE PRECISION AK2, al, al1, al2, al3, b1, b2, BI, BR, E1, ELAb, 
     &                 ELAcs, ELAda, ETA, one, PI, SE, SI, SR
      DOUBLE PRECISION ST, T, TC, ten, three, tm, TOTcs, two, W2, zero, 
     &                 ZI, ZT
      INTEGER i, IE, Ipr, IS, IS1, IS2, IS3, IS4, k, l, Lmax, LTCMAX, 
     &        NELang
      PARAMETER(LTCMAX = NDTL)
C     parameter ( ltcmax=100, LFMAX=4*NDTL+2)
      REAL*8 MI, MT
      COMMON /CONST / MI, SI, ZI, MT, ZT, PI, AK2, ETA, W2, CSO
      COMMON /ENER  / E1, ELAb
      COMMON /INOUT / IE, IS, IS1, IS2, IS3, IS4
      COMMON /TCE   / TC(LTCMAX)
      COMMON /TLJ   / BR(3, LTCMAX), BI(3, LTCMAX), T(3, LTCMAX)
      COMMON /XS    / SE, SR, ST
      COMMON /ELASCAT/ ELAda(101), TOTcs, ELAcs, NELang
      DATA zero/0.0D+00/, one/1.0D+00/, two/2.0D+00/
C
      DATA three/3.D0/, ten/1.0D+01/
C
      SE = zero
      SR = zero
      ST = zero
      DO l = 1, Lmax
         k = l - 1
         al = DFLOAT(l - 1)
         al1 = two*al - one
         al2 = two*al + one
         al3 = two*al + three
         tm = (al3*T(3, l) + al2*T(2, l) + al1*T(1, l))/(three*al2)
         TC(l) = tm
         SR = SR + al3*(two*BR(3, l) - BR(3, l)*BR(3, l) - BI(3, l)
     &        *BI(3, l))
     &        + al2*(two*BR(2, l) - BR(2, l)*BR(2, l) - BI(2, l)
     &        *BI(2, l))
     &        + al1*(two*BR(1, l) - BR(1, l)*BR(1, l) - BI(1, l)
     &        *BI(1, l))
         SE = SE + al3*(BR(3, l)*BR(3, l) + BI(3, l)*BI(3, l))
     &        + al2*(BR(2, l)*BR(2, l) + BI(2, l)*BI(2, l))
     &        + al1*(BR(1, l)*BR(1, l) + BI(1, l)*BI(1, l))
         ST = ST + al3*two*BR(3, l) + al2*two*BR(2, l)
     &        + al1*two*BR(1, l)
      ENDDO
      WRITE(IS, 99001)
99001 FORMAT(' ', 1x, 'L', 7x, 'TC(L)', 3x, '1 - ETA R', 7x, 'ETA I', 
     &       3x, 'T(L,L-1)', 1x, '1 - ETA R', 5x, 'ETA I', 5x, 'T(L,L)', 
     &       1x, '1 - ETA R', 5x, 'ETA I', 3x, 'T(L,L+1)', 1x, 
     &       '1 - ETA R', 5x, 'ETA I', /)
      DO l = 1, Lmax
         k = l - 1
         al = DFLOAT(k)
         al1 = two*al - one
         al2 = two*al + one
         al3 = two*al + three
         b1 = (al3*BR(3, l) + al2*BR(2, l) + al1*BR(1, l))/(three*al2)
         b2 = (al3*BI(3, l) + al2*BI(2, l) + al1*BI(1, l))/(three*al2)
         WRITE(IS, 99002)k, TC(l), b1, b2, 
     &                   (T(i, l), BR(i, l), BI(i, l), i = 1, 3)
99002    FORMAT(' ', i2, 1p, 3E12.4, 3(1x, 3E10.3))
      ENDDO
C
C     Cross sections (in mb)
C
      SE = SE/three
      SR = SR/three
      ST = ST/three
      SE = ten*SE*PI/AK2
      SR = ten*SR*PI/AK2
      ST = ten*ST*PI/AK2
      IF(Ipr.GT.0)THEN
         WRITE(IS, 99004)ST, SR, SE
         WRITE(IS1, 99004)ST, SR, SE
      ENDIF
      ELAcs = SE
      TOTcs = ST
C
C     Formats
C
99003 FORMAT(1p, 6E11.4)
99004 FORMAT(//, 
     &       '  Results provided by Spherical Optical Model code SCAT2:'
     &       , //, 2x, 'Total cross section         :', e14.7, ' mb', /, 
     &       2x, 'Absorption cross section    :', e14.7, ' mb', /, 2x, 
     &       'Shape elastic cross section :', e14.7, ' mb', ///)
      END
C
C
      SUBROUTINE SETPOTS(Nejc, Nnuc, Eilab, Eicms, Mi, Mt, Rrmu, Ak2, 
     &                   Ikey)
C
C     Transfers o.m. parameters to ECIS  or SCAT2
C
      INCLUDE 'dimension.h'
C
      INCLUDE 'global.h'
C
C
      DOUBLE PRECISION AAE, AQ, BETa, POT, R, RCOulomb, RRE
      DOUBLE PRECISION Rrmu, Ak2, Mi, Mt
      INTEGER i
C
C     Dummy arguments
C
      DOUBLE PRECISION Eilab, Eicms
      INTEGER Nejc, Nnuc
C
C     COMMON variables
C
C
      COMMON /POTEN1/ R(7), RRE(7), AQ(7), AAE(7), POT(7, 6)
      COMMON /POTEN2/ RCOulomb, BETa, EFErmi, EP, EA, IREl
C
C     Ener must be in LAB system
C
C     ener = Elab
C
      IF(CCCalc)THEN
C-----calculate o.m. parameters for ejectile NEJC on target NNUC at energy ENER
         komp = 33
         ko = 33
C        CALL OMPAR(Nejc, Nnuc, ener, komp, ko)
         CALL OMPAR(Nejc, Nnuc, Eilab, Eicms, Mi, Mt, Rrmu, Ak2, komp, 
     &              ko, Ikey)
      ELSE
C-----calculate o.m. parameters for ejectile NEJC on target NNUC at energy ENER
         komp = 29
         ko = 18
C        CALL OMPAR(Nejc, Nnuc, ener, komp, ko)
         CALL OMPAR(Nejc, Nnuc, Eilab, Eicms, Mi, Mt, Rrmu, Ak2, komp, 
     &              ko, Ikey)
      ENDIF
C-----set OMP onto SCAT variables
C     Energy dependence (if any) must be considered in OMPAR
C     Setting potential strength
      POTe(1) = VOM(1, Nejc, Nnuc)
C     Capote, july 2001, real surface potential
      POTe(7) = VOMs(1, Nejc, Nnuc)
      POTe(2) = WOMs(1, Nejc, Nnuc)
      POTe(3) = WOMv(1, Nejc, Nnuc)
      POTe(4) = VSO(1, Nejc, Nnuc)
      POTe(6) = WSO(1, Nejc, Nnuc)
C     Setting geometrical parameters for SCAT2
      DO i = 1, 7
         POT(i, 1) = POTe(i)
      ENDDO
      R(1) = RVOm(1, Nejc, Nnuc)
      AQ(1) = AVOm(Nejc, Nnuc)
      R(2) = RWOm(1, Nejc, Nnuc)
      IF(SFIom(Nejc, Nnuc).LT.0.0D0)R(2) = -R(2)
      AQ(2) = AWOm(Nejc, Nnuc)
C     Capote, july 2001, transfer of real surface potential
      R(7) = R(2)
      AQ(7) = AQ(2)
      R(3) = RWOmv(1, Nejc, Nnuc)
      AQ(3) = AWOmv(Nejc, Nnuc)
      R(4) = RVSo(1, Nejc, Nnuc)
      AQ(4) = AVSo(Nejc, Nnuc)
      R(6) = RWSo(1, Nejc, Nnuc)
      AQ(6) = AWSo(Nejc, Nnuc)
      RCOulomb = RCOul(Nejc, Nnuc)
      BETa = RNOnl(Nejc, Nnuc)
      EFErmi = EEFermi(Nejc, Nnuc)
      EP = EEP(Nejc, Nnuc)
      EA = EEA(Nejc, Nnuc)
      IREl = IRElat(Nejc, Nnuc)
      END
C
      SUBROUTINE KINEMA(El, E1, Mi, Mt, Amu, Ak2, Iopt, Relcal)
C
C     Author: O.Bersillon (SCAT2000)
C
C***********************************************************************
C  Kinematics:   lab  <===>  CM                                        *
C    With relativistic kinematics, the reduced mass is replaced by     *
C    the reduced total energy                                          *
C----------------------------------------------------------------------*
C  EL     = current lab kinetic energy                                 *
C  E1     = current  CM kinetic energy                                 *
C  MI     = incident particle rest mass (in a.m.u.)                    *
C  MT     = target   nucleus  rest mass (in a.m.u.)                    *
C  AMU    = reduced mass                                               *
C  AK2    = CM wave number                                             *
C  IOPT   = 1   from lab to CM                                         *
C           2   from CM  to lab                                        *
C  IRELAT = 0   classical    kinematics                                *
C           1   relativistic kinematics                                *
C----------------------------------------------------------------------*
C  AMUmev = a.m.u. in MeV                                              *
C----------------------------------------------------------------------*
C  Called by MAIN                                                      *
C***********************************************************************
C
      IMPLICIT DOUBLE PRECISION(A - H, O - Z)
C
      DOUBLE PRECISION Mi, Mt, mtot
      LOGICAL Relcal
C
C     common /inout/  ie,is,is1,is2,is3,is4
C     common /physcon/ ampipm,ampi0,amu0c2,e2,hbarc
C
      COMMON /CONSTANT/ AMUmev, PI, W2L, XNExc, CETa, CSO, RMU, AMPi, 
     &                  ELE2, HHBarc
C
      DATA one/1.0D+00/
      DATA two/2.0D+00/
C
C=======================================================================
C
      ck2 = (two*AMUmev)/(HHBarc**2)
C
      mtot = Mi + Mt
C
      IF(Iopt.EQ.1)THEN
C
C***********************************************************************
C        From lab to CM (the input quantity is Elab)
C***********************************************************************
C
         IF(.NOT.Relcal)THEN
C
C-----------------------------------------------------------------------
C           Classical    kinematics
C-----------------------------------------------------------------------
C
            Amu = Mi*Mt/mtot
            E1 = El*Mt/mtot
            w2 = ck2*Amu
            Ak2 = w2*E1
         ELSE
C
C-----------------------------------------------------------------------
C           Relativistic kinematics
C-----------------------------------------------------------------------
C
            E1 = DSQRT(mtot*mtot + two*Mt*El) - mtot
            E1 = AMUmev*mtot*
     &           (DSQRT(one + two*El/(AMUmev*Mt*((one+Mi/Mt)**2)))
     &           - one)
            p2 = (El*(El + two*AMUmev*Mi))
     &           /((one + Mi/Mt)**2 + two*El/(AMUmev*Mt))
            Ak2 = p2/(HHBarc*HHBarc)
            etoti = DSQRT((AMUmev*Mi)**2 + p2)
            etott = DSQRT((AMUmev*Mt)**2 + p2)
            Amu = etoti*etott/(etoti + etott)
            Amu = Amu/AMUmev
         ENDIF
C
C
C
C***********************************************************************
C        From  CM to lab (the input quantity is Ecm)
C***********************************************************************
C
      ELSEIF(.NOT.Relcal)THEN
C
C-----------------------------------------------------------------------
C        Classical    kinematics
C-----------------------------------------------------------------------
C
         Amu = Mi*Mt/mtot
         El = E1*mtot/Mt
         Ak2 = ck2*Amu*E1
      ELSE
C
C-----------------------------------------------------------------------
C        Relativistic kinematics
C-----------------------------------------------------------------------
C
         El = E1*(E1 + two*AMUmev*mtot)/(two*AMUmev*Mt)
         p2 = E1*(E1 + two*AMUmev*mtot)*(E1 + two*AMUmev*Mi)
     &        *(E1 + two*AMUmev*Mt)/((two*(E1+AMUmev*mtot))**2)
         Ak2 = p2/(HHBarc*HHBarc)
         etoti = DSQRT((AMUmev*Mi)**2 + p2)
         etott = DSQRT((AMUmev*Mt)**2 + p2)
         Amu = etoti*etott/(etoti + etott)
         Amu = Amu/AMUmev
C
C
      ENDIF
C
      END
