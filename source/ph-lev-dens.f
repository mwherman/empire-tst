Ccc   * $Author: mike $
Ccc   * $Date: 2001-08-21 15:36:16 $
Ccc   * $Id: ph-lev-dens.f,v 1.2 2001-08-21 15:36:16 mike Exp $
C
      DOUBLE PRECISION FUNCTION WT(In, Ip, Ih, X)
C
C     calculates conditional p-h state densities according
C     to Nucl. Phys. A430(1984)69 (including all necessary factors)
C
C     IP - particle number
C     IH - hole number
C     X  - excitation energy
C     G  - single particle density
C
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C
C COMMON variables
C
      DOUBLE PRECISION B, E, FACt(100), G
      COMMON /EB    / E, B, FACt, G
C
C Dummy arguments
C
      INTEGER Ih, In, Ip
      DOUBLE PRECISION X
C
C Local variables
C
      DOUBLE PRECISION W
C
C
      WT = W(Ip, Ih, X)/FACt(Ip + 1)/FACt(Ih + 1)*G**In
      END
C
C
      DOUBLE PRECISION FUNCTION W(Ip, Ih, X)
C
C     calculates conditional p-h state densities according
C     to Nucl. Phys. A430(1984)69 without g**n/p!/h!
C
C     IP - particle number
C     IH - hole number
C     X  - excitation energy
C     B  - binding energy
C
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C
C COMMON variables
C
      DOUBLE PRECISION B, E, FACt(100), G
      COMMON /EB    / E, B, FACt, G
C
C Dummy arguments
C
      INTEGER Ih, Ip
      DOUBLE PRECISION X
C
C Local variables
C
      INTEGER INT
      INTEGER ix
      DOUBLE PRECISION pix
      DOUBLE PRECISION W1, W2, W3
C
C
      W = 0.
      IF(X.LE.0.0D0)RETURN
      IF(Ip.EQ.0 .AND. Ih.EQ.0)RETURN
      ix = INT(X/B)
      pix = ABS(ix*B - X)
      IF(pix.LT.1.D-8)ix = ix - 1
      IF(Ip.NE.0)THEN
         IF(Ih.EQ.0)THEN
            IF(ix.GE.Ip)RETURN
            W = W2(Ip, 0, ix, X)
            GOTO 99999
         ELSE
C--------check of the E>PB condition
            IF(ix.LT.Ip)THEN
C-----------W2 is eq.7a of reference above without g**(p+h)/p!/h! factor
               W = W2(Ip, Ih, ix, X)
               RETURN
            ENDIF
            IF(Ip.GE.Ih)THEN
C-----------W1 is eq.7b of reference above without g**(p+h)/p!/h! factor
               W = W1(Ip, Ih, Ih - 1, X)
               RETURN
            ENDIF
            W = W2(Ip, Ih, Ip - 1, X) + W3(Ip, Ih, Ip - 1, X)
            RETURN
         ENDIF
      ENDIF
      W = W2(0, Ih, 0, X)
99999 END
C
C
      SUBROUTINE GDOWN(Yd, Ip, Ih, X)
C
C     calculates gamma down for multistep compound according to
C     Nucl. Phys. A435(1985)67
C     commented statements correspond to the original formulation
C     of the reference above
C     the actual version accounts for the factor 1/2 as pointed
C     out by Oblozinsky (Nucl. Phys. A453(1986)127)
C
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C
C COMMON variables
C
      DOUBLE PRECISION B, E, FACt(100), G
      COMMON /EB    / E, B, FACt, G
C
C Dummy arguments
C
      INTEGER Ih, Ip
      DOUBLE PRECISION X, Yd
C
C Local variables
C
      DOUBLE PRECISION e1b, r, ro1, roph, xh2
      INTEGER ip1
      DOUBLE PRECISION W
C
C
      e1b = X - B
      roph = 0.
C     XH2=IH/2.
      xh2 = Ih
      ip1 = Ip - 1
      r = W(Ip, Ih + 2, X)
      roph = roph + r*xh2
      r = W(Ip - 1, Ih + 3, X)
      roph = roph + r*Ip
      IF(e1b.GT.0.D0)THEN
         r = W(Ip, Ih + 2, e1b)
         roph = roph - r*xh2
         ro1 = 0.
         r = W(ip1, Ih + 3, e1b)
         ro1 = ro1 + r
         r = W(ip1, Ih + 1, e1b)
         ro1 = ro1 + r*B*B/2.
         r = W(ip1, Ih + 2, e1b)
         ro1 = ro1 + B*r
         roph = roph - ro1*Ip
      ENDIF
      Yd = roph/2.
      END
C
C
      SUBROUTINE ZERO(Y0, Ip, Ih, X)
      IMPLICIT DOUBLE PRECISION(a - H), DOUBLE PRECISION(O - Z)
C
C
C COMMON variables
C
      DOUBLE PRECISION B, E, FACt(100), G
      COMMON /EB    / E, B, FACt, G
C
C Dummy arguments
C
      INTEGER Ih, Ip
      DOUBLE PRECISION X, Y0
C
C Local variables
C
      DOUBLE PRECISION a, a1, e2b, roph
      DOUBLE PRECISION W
C
C
      e2b = E - 2*B
      roph = W(Ip - 1, Ih, X)
      a = roph*B*Ih
      a1 = 0.
      IF(e2b.LT.X)THEN
         roph = W(Ip - 2, Ih + 2, X)
         a1 = a1 - roph
         roph = W(Ip - 2, Ih + 1, X)
         a1 = a1 + roph*(X - e2b)
         IF(e2b.GT.0.D0)THEN
            roph = W(Ip - 2, Ih + 2, e2b)
            a1 = a1 + roph
         ENDIF
      ENDIF
      a1 = a1*(Ip - 1)/2
      Y0 = (a + a1)*Ip
      END
C
C
      SUBROUTINE MINUS(Ym, Ip, Ih, X)
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C
C COMMON variables
C
      DOUBLE PRECISION B, E, FACt(100), G
      COMMON /EB    / E, B, FACt, G
C
C Dummy arguments
C
      INTEGER Ih, Ip
      DOUBLE PRECISION X, Ym
C
C Local variables
C
      DOUBLE PRECISION rp, rp1
      DOUBLE PRECISION W
C
C
      IF(Ip.NE.2 .OR. Ih.NE.1)THEN
         rp = W(2, 1, E - X)
         rp1 = W(Ip - 2, Ih - 1, X)
         Ym = rp*rp1*Ip*(Ip - 1)*Ih/2.
         RETURN
      ENDIF
      Ym = 0.
      END
C
C
      DOUBLE PRECISION FUNCTION W1(J, L, K, X)
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C
C COMMON variables
C
      DOUBLE PRECISION B, E, FACt(100), G
      COMMON /EB    / E, B, FACt, G
C
C Dummy arguments
C
      INTEGER J, K, L
      DOUBLE PRECISION X
C
C Local variables
C
      DOUBLE PRECISION a1, a2, s, sum, sum1, x1, z1
      INTEGER i, ii, j1, k1, l1, m, m1
C
C
C-----seems to be 7b
      j1 = J + 1
      x1 = X - J*B
      l1 = L - 1
      IF(K.LE.l1)THEN
         k1 = K + 1
         a1 = x1**l1
         s = -1.
         sum = 0.
         DO i = 1, J
            ii = i - 1
            s = -s
            sum1 = 0.
            a2 = (J - ii)*B
            z1 = x1/a2
            DO m = 1, k1
               m1 = m - 1
               z1 = z1*a2/x1
               sum1 = sum1 + z1/FACt(J + m)/FACt(L - m1)
            ENDDO
            a2 = a2**J
            sum = sum + a2/FACt(i)/FACt(j1 - ii)*sum1*s
         ENDDO
         W1 = sum*a1*FACt(j1)
         RETURN
      ENDIF
      WRITE(50, 99001)J, L, K, X
99001 FORMAT(1X, 'ERROR', 5X, 'W1(', I1, ',', I1, ',', I1, ',', E12.5, 
     &       ')')
      STOP
      END
C
C
      DOUBLE PRECISION FUNCTION W2(J, L, K, X)
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C
C COMMON variables
C
      DOUBLE PRECISION B, E, FACt(100), G
      COMMON /EB    / E, B, FACt, G
C
C Dummy arguments
C
      INTEGER J, K, L
      DOUBLE PRECISION X
C
C Local variables
C
      INTEGER i, ii, j1, jl, k1
      DOUBLE PRECISION s, sum, xx
C
C
C-----looks like 7a
      j1 = J + 1
      IF(K.EQ.0 .AND. X.EQ.0.0D0)THEN
         W2 = 0.
         RETURN
C
      ENDIF
      IF(J.LT.0 .OR. K.GT.15)THEN
         WRITE(6, *)'message from W2 J, L, K,', J, L, K
         W2 = 0.0
         RETURN
      ENDIF
      jl = J + L - 1
      k1 = K + 1
      s = -1.
      sum = 0.
      DO i = 1, k1
         ii = i - 1
         s = -s
         xx = X - ii*B
         xx = xx**jl
         sum = sum + s/FACt(i)/FACt(j1 - ii)*xx
      ENDDO
      W2 = sum/FACt(jl + 1)*FACt(j1)
      END
C
C
      DOUBLE PRECISION FUNCTION W3(J, L, K, X)
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C
C COMMON variables
C
      DOUBLE PRECISION B, E, FACt(100), G
      COMMON /EB    / E, B, FACt, G
C
C Dummy arguments
C
      INTEGER J, K, L
      DOUBLE PRECISION X
C
C Local variables
C
      DOUBLE PRECISION a1, a2, s, sum, sum1, x1, z1
      INTEGER i, ii, j1, j11, k1, m, m1
C
C
C-----seems to be second part of 7c
      x1 = X - J*B
      j1 = J - 1
      j11 = J + 1
      k1 = K + 1
      IF(K.LE.j1)THEN
         a1 = x1**L
         s = 1.
         sum = 0.
         DO i = 1, J
            ii = i - 1
            s = -s
            sum1 = 0.
            a2 = (J - ii)*B
            z1 = a2/x1
            DO m = 1, k1
               m1 = m - 1
               z1 = z1*x1/a2
               sum1 = sum1 + z1/FACt(L + m)/FACt(J - m1)
            ENDDO
            a2 = a2**j1
            sum = sum + a2/FACt(i)/FACt(j11 - ii)*s*sum1
         ENDDO
         W3 = sum*a1*FACt(j11)
         RETURN
      ENDIF
      WRITE(50, 99001)J, L, K, X
99001 FORMAT(1X, 'ERROR', 5X, 'W3(', I1, ',', I1, ',', I1, ',', E12.5, 
     &       ')')
      STOP
      END
C
C
      DOUBLE PRECISION FUNCTION WOBL(Ip, Ih, U, Ni)
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C
C COMMON variables
C
      DOUBLE PRECISION B, E, FACt(100), G
      COMMON /EB    / E, B, FACt, G
C
C Dummy arguments
C
      INTEGER Ih, Ip, Ni
      DOUBLE PRECISION U
C
C Local variables
C
      INTEGER i, ii, ipm, n
      DOUBLE PRECISION s, w
C
C
C
C     calculates conditional state densities according to Oblozinsky
C     Nucl. Phys. A453(1986)127 formula 13; without factor
C     g**(p+h)/p!h!(n-1)! and neglecting well depth
C
      WOBL = 0.0
      IF(U.LE.0.0D0)RETURN
      n = Ip + Ih + Ni
      IF(Ip.GE.0 .AND. Ih.GE.0 .AND. n.NE.0)THEN
         ipm = AINT(U/B)
         ipm = MIN(Ip, ipm)
         ipm = ipm + 1
         s = -1.0
         DO ii = 1, ipm
            i = ii - 1
            s = -s
            w = (U - i*B)**n
            w = w*s/FACt(ii)/FACt(Ip - i + 1)
            WOBL = WOBL + w
         ENDDO
         WOBL = WOBL*FACt(Ip + 1)
         RETURN
      ENDIF
      WRITE(6, 99001)Ip, Ih, n
99001 FORMAT(1X, /1X, ' ERROR IN WOBL CALL P=', I2, '  H=', I2, '  N=', 
     &       I2, '    WOBL=0.0 RETURNED', /)
      END
C
C
      SUBROUTINE BACK(Yb, Ip, Ih)
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C
C COMMON variables
C
      DOUBLE PRECISION B, E, FACt(100), G
      COMMON /EB    / E, B, FACt, G
C
C Dummy arguments
C
      INTEGER Ih, Ip
      DOUBLE PRECISION Yb
C
C Local variables
C
      DOUBLE PRECISION ebe
      INTEGER n1, n2
      DOUBLE PRECISION WOBL
C
C
C
C     calculates density of accessible states (conditional) for internal
C     backward transitions  (without g/w(p,h,e,-1) factor)
C     using Oblozinsky's formula for cond. st. den.
C
      Yb = 0.0
      n1 = Ip + Ih - 1
      n2 = n1 - 1
      IF(n2.LE.1)RETURN
      IF(E.LE.0.0D0)RETURN
      Yb = WOBL(Ip - 2, Ih - 1, E, 2) + WOBL(Ip - 1, Ih - 2, E, 2)
     &     *(Ih - 1)/(Ip - 1)
      ebe = E - B
      IF(ebe.GT.0.0D0)Yb = Yb - WOBL(Ip - 2, Ih - 1, ebe, 2)
     &                     - B*n1*WOBL(Ip - 2, Ih - 1, ebe, 1)
     &                     - 0.5*B*B*n1*n2*WOBL(Ip - 2, Ih - 1, ebe, 0)
      Yb = 0.5*Ip*(Ip - 1)*Ih*Yb
      END
C
C
      DOUBLE PRECISION FUNCTION VQ(Ip, Ih, U)
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C
C COMMON variables
C
      DOUBLE PRECISION B, E, FACt(100), G
      COMMON /EB    / E, B, FACt, G
C
C Dummy arguments
C
      INTEGER Ih, Ip
      DOUBLE PRECISION U
C
C Local variables
C
      DOUBLE PRECISION c, ub, w1, w2, w3, w4, w5, w6, w7, w8
      INTEGER ih1, ip1, n
      DOUBLE PRECISION WOBL
C
C
C
C     calculates avrage of imaginary part of o.m. pot. given as W=C*E**2
C     exciton distribution function OM(P-1,H,E-EP)/OM(P,H,E) is used as
C     weighting funtion in the case of particles (analogous for holes)
C
      VQ = 0.0
      IF(Ip.NE.0 .OR. Ih.NE.0)THEN
         IF(Ip.GE.0 .AND. Ih.GE.0)THEN
            IF(U.LE.0.0D0)RETURN
            n = Ip + Ih
            ip1 = Ip - 1
            ih1 = Ih - 1
            ub = U - B
            c = 0.003
            w1 = WOBL(ip1, Ih, U, 0)
            w2 = WOBL(ip1, Ih, ub, 0)
            w3 = WOBL(ip1, Ih, U, 2)
            w4 = WOBL(ip1, Ih, ub, 2)
            w5 = WOBL(ip1, Ih, ub, 1)
            w6 = WOBL(ip1, Ih, ub, 0)
            w7 = WOBL(Ip, ih1, U, 2)
            w8 = WOBL(Ip, ih1, U, 0)
C-----------particle part
            VQ = 2.0/n/(n + 1)*(w3 - w4) - 2.0/n*w5*B - w6*B*B
            VQ = VQ*Ip*c/n/(w1 - w2)
C-----------hole part
            VQ = VQ + 2.0*c*Ih*w7/(n*n*(n + 1)*w8)
            RETURN
         ENDIF
      ENDIF
      WRITE(6, 99001)Ip, Ih, U
99001 FORMAT(1X, 'ERROR IN VQ INPUT: P=', I2, ' H=', I2, ' E=', F8.4, 
     &       '  VQ=0 RETURNED')
      END
C
      SUBROUTINE TRATES
C
C
C
      WRITE(6, *)'TRATES NOT IMPLEMENTED'
      END
C
C
      DOUBLE PRECISION FUNCTION ROPHM(N, I, E, G)
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C
C Dummy arguments
C
      DOUBLE PRECISION E, G
      INTEGER I, N
C
C
      WRITE(6, *)'MICROSCOPIC PARTIAL LEVEL DENSITIES NOT IMPLEMENTED'
      ROPHM = 0.0
      END
C
C
C
      DOUBLE PRECISION FUNCTION WILLI(N, X)
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C
C COMMON variables
C
      DOUBLE PRECISION BP, E, FACt(100), G
      COMMON /EB    / E, BP, FACt, G
C
C Dummy arguments
C
      INTEGER N
      DOUBLE PRECISION X
C
C
C
C     calculates p-h state densities according to williams formula
C     (without g**n/p!h! factor which is contained in omj)
C
      WILLI = 0.0
      IF(X.LT.0.D0 .OR. N.LE.0)RETURN
      WILLI = X**(N - 1)/FACt(N)
      END
C
C
      DOUBLE PRECISION FUNCTION OMJ(N, Ip, Ih, J, S, Ngs)
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C
C COMMON variables
C
      DOUBLE PRECISION BP, E, FACt(100), G, SIGnx
      COMMON /EB    / E, BP, FACt, G
      COMMON /PRSI  / SIGnx
C
C Dummy arguments
C
      INTEGER Ih, Ip, J, N, Ngs
      DOUBLE PRECISION S
C
C Local variables
C
      DOUBLE PRECISION sig, w, xj
C
C
C
C     calculates spin dependent factor in state density including
C     1/2 for parity and g**n/p!h! missing in w function
C     the latter factor is set to 1 when microscopic densities are
C     used (ngs=1)
C
      OMJ = 0.
      IF(N.LE.0 .OR. Ip.LT.0 .OR. Ih.LT.0)RETURN
      sig = SIGnx*N
      xj = J + S
      w = (xj + 1.0)*xj/2./sig
      IF(w.GT.50.D0)RETURN
      OMJ = 1.
      IF(Ngs.EQ.0)OMJ = G**N/FACt(Ip + 1)/FACt(Ih + 1)
      OMJ = OMJ*(2*xj + 1.)*EXP(( - w))/4./2.50663/sig**1.5
C     2.50663 STANDS FOR SQRT(2*PI)
      END
C
C
      DOUBLE PRECISION FUNCTION WOB1(X, Np, Nh, F)
Ccc   **************************************************************************
Ccc   *                                                              class:PPU *
Ccc   *                            W O B 1                                     *
Ccc   *                                                                        *
Ccc   * Calculates the partial nuclear state density of a given excited        *
Ccc   * particle-hole configuration by means of the Betak-Dobes formula        *
Ccc   * [1] accounting for nuclear potential finite-depth, for one-            *
Ccc   * component Fermi-gas. If the nucleon binding energy is specified        *
Ccc   * the bound-state densities according to Oblozinsky [2] are calculated.  *
Ccc   *                                                                        *
Ccc   * [1] C  2. E. Betak and J. Dobes, Z. Phys. A279, 319 (1976)             *
Ccc   * [2] P.Oblozinsky, Nucl.Phys. A453,127(1986), Eqs.(7,9)                 *
Ccc   *                                                                        *
Ccc   * Coded by M. AVRIGEANU, INPE-BUCHAREST, SEPTEMBER 1997.                 *
Ccc   * Extracted from RIPL-1.                                                 *
Ccc   *                                                                        *
Ccc   * Input: X - excitation energy [MeV]                                     *
Ccc   *        NP - number of particles                                        *
Ccc   *        NH - number of holes                                            *
Ccc   *        F  - nuclear potential depth                                    *
Ccc   * In common /EB/:                                                        *
Ccc   *        B  - neutron biding for conditional state densities             *
Ccc   *        G  - single particle state density [1/MeV]                      *
Ccc   *                                                                        *
Ccc   * Output: WOB1 - p-h level density at X                                  *
Ccc   *                                                                        *
Ccc   *                                                                        *
Ccc   * Adapted by M. Herman                                                   *
Ccc   * date:   30.05.2000                                                     *
Ccc   * revision:#    by:name                     on:xx.mon.199x               *
Ccc   *                                                                        *
Ccc   *                                                                        *
Ccc   *                                                                        *
Ccc   **************************************************************************
      IMPLICIT DOUBLE PRECISION(A - h), DOUBLE PRECISION(O - Z)
C
C
C COMMON variables
C
      DOUBLE PRECISION B, E, FACt(100), G
      COMMON /EB    / E, B, FACt, G
C
C Dummy arguments
C
      DOUBLE PRECISION F, X
      INTEGER Nh, Np
C
C Local variables
C
      DOUBLE PRECISION alpha, aph, ch, cp, d, ecor, ecor1, h, p, sum, 
     &                 t1, t2
      DOUBLE PRECISION FCTR
      REAL FLOAT
      INTEGER i, ii, j, jj, n, nn
C
C
      FCTR(n) = FACt(n + 1)
      WOB1 = 0.
      nn = Np + Nh
      p = FLOAT(Np)
      h = FLOAT(Nh)
      t1 = (G**(p + h))/FCTR(nn - 1)
      t2 = FCTR(Np)*FCTR(Nh)
CIN   Alpha=(P*(P+1.)+H*(H-1.))/(2.*G)
CIN   Aph=(P*(P+1.)+H*(H-3.))/(4.*G)
      alpha = (p*p + h*h)/(2.*G)
      aph = (p*(p - 1.) + h*(h - 1.))/(4.*G)
      IF((X + alpha - p*B - h*F).GT.0.)RETURN
      sum = 0.
      DO ii = 1, Np + 1
         i = ii - 1
         DO jj = 1, Nh + 1
            j = jj - 1
            ecor = X - aph - i*B - j*F
            ecor1 = X - alpha - i*B - j*F
            IF(ecor1.GT.0.)THEN
               d = ( - 1.)**(i + j)
               cp = FCTR(Np)/(FCTR(Np - i)*FCTR(i))
               ch = FCTR(Nh)/(FCTR(Nh - j)*FCTR(j))
               sum = sum + d*cp*ch*ecor**(nn - 1)/t2
            ENDIF
         ENDDO
      ENDDO
      WOB1 = t1*sum
      END
