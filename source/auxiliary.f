Ccc   * $Author: mike $
Ccc   * $Date: 2001-07-09 17:33:39 $
Ccc   * $Id: auxiliary.f,v 1.1.1.1 2001-07-09 17:33:39 mike Exp $
C
      SUBROUTINE CLEAR
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:apu*
Ccc   *                         C L E A R                                *
Ccc   *                                                                  *
Ccc   *             Sets to 0 some matrices and variables                *
Ccc   *                                                                  *
Ccc   * input:none                                                       *
Ccc   *                                                                  *
Ccc   * output:none                                                      *
Ccc   *                                                                  *
Ccc   * calls:none                                                       *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * author: M.Herman                                                 *
Ccc   * date:   11.Apr.1994                                              *
Ccc   * revision:#    by:name                     on:xx.mon.199x         *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C
C Local variables
C
      INTEGER ilw, irec, j, nang, necse, nejc, netl, nnex, nnlv, nnuc
C
C
C
      XNI = 0.
      TORy = 4.
      EX1 = 0.0
      EX2 = 0.0
      NLW = 0
      CSFus = 0.0
      CRL = 0.0
      DENhf = 0.0
      CSMsd(1) = 0.0
      CSMsd(2) = 0.0
      DO nejc = 0, NDEJC
         CSEmis(nejc, 0) = 0.0
         DO necse = 1, NDECSE
            CSE(necse, nejc, 0) = 0.0
            DO nang = 1, NDANG
               CSEa(necse, nang, nejc, 0) = 0.0
            ENDDO
         ENDDO
      ENDDO
      DO nnuc = 1, NDNUC
         POPmax(nnuc) = 0.0
         CSPrd(nnuc) = 0.0
         EX(1, nnuc) = 0.0
         JSTab(nnuc) = 0.0
         DO j = 1, NDLW
            FISb(j, nnuc) = 0.0
         ENDDO
         DO nejc = 1, NDEJC
            DO netl = 1, NDETL
               ETL(netl, nejc, nnuc) = 0.0
               LMAxtl(netl, nejc, nnuc) = 0
               DO ilw = 1, NDLW
                  TL(netl, ilw, nejc, nnuc) = 0.0
               ENDDO
            ENDDO
         ENDDO
         DO nnex = 1, NDEX
            EX(nnex, nnuc) = 0.0
            DO ilw = 1, NDLW
               RO(nnex, ilw, nnuc) = 0.0
               ROF(nnex, ilw, nnuc) = 0.0
               POP(nnex, ilw, 1, nnuc) = 0.0
               POP(nnex, ilw, 2, nnuc) = 0.0
            ENDDO
            DO irec = 1, NDEREC
               RECcse(irec, nnex, nnuc) = 0.0
               RECcse(irec, 0, nnuc) = 0.0
            ENDDO
         ENDDO
         EX(NDEX + 1, nnuc) = 0.0
         DO nnlv = 1, NDLV
            POPlv(nnlv, nnuc) = 0.0
         ENDDO
         DO nejc = 0, NDEJC
            CSEmis(nejc, nnuc) = 0.0
            DO necse = 1, NDECSE
               CSE(necse, nejc, nnuc) = 0.0
               DO nang = 1, NDANG
                  CSEa(necse, nang, nejc, nnuc) = 0.0
               ENDDO
            ENDDO
            DO nang = 1, NDANG
               CSA(nang, nejc, nnuc) = 0.0
            ENDDO
         ENDDO
      ENDDO
      DO nejc = 0, NDEJC
         SCRtem(nejc) = 0.0
         DO ilw = 1, NDLW
            DO nnex = 1, NDEX
               SCRt(nnex, ilw, 1, nejc) = 0.0
               SCRt(nnex, ilw, 2, nejc) = 0.0
            ENDDO
         ENDDO
         DO nnlv = 1, NDLV
            SCRtl(nnlv, nejc) = 0.0
         ENDDO
      ENDDO
      DO necse = 1, NDECSE
         CSEmsd(necse, 1) = 0.0
         CSEmsd(necse, 2) = 0.0
         DO nejc = 0, NDEJC
            AUSpec(necse, nejc) = 0.0
         ENDDO
         DO nang = 1, NDANG
            ANCsea(necse, nang, 1) = 0.0
            ANCsea(necse, nang, 2) = 0.0
            APCsea(necse, nang, 1) = 0.0
            APCsea(necse, nang, 2) = 0.0
         ENDDO
      ENDDO
      DO nang = 1, NDANG
         DO nnlv = 1, NDLV
            DO nejc = 1, NDEJC
               CSAlev(nang, nnlv, nejc) = 0.0
            ENDDO
         ENDDO
      ENDDO
      DO j = 1, NDLW
         REDmsc(j, 1) = 1.0
         REDmsc(j, 2) = 1.0
      ENDDO
C
      END
C
      SUBROUTINE INTGRS(A, B, Y, Dintg)
CCCC *****************************************************************
CCCC * Calculates an integral                                        *
CCCC *****************************************************************
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C
C Dummy arguments
C
      DOUBLE PRECISION A, B, Dintg
      DOUBLE PRECISION Y
C
C Local variables
C
      DOUBLE PRECISION d, dintg1, prec, sum, x
      INTEGER i, n
      EXTERNAL Y
C
C
      prec = 0.01
      sum = 0.
      n = 1
      d = B - A
      sum = Y(A) + Y(B)
      dintg1 = sum*d*.5
 100  x = A - d*.5
      DO i = 1, n
         x = x + d
         sum = sum + 2*Y(x)
      ENDDO
      d = d*.5
      Dintg = sum*d*.5
      IF(ABS(Dintg - dintg1) - prec*ABS(Dintg).GT.0.D0)THEN
         IF(n.GT.200)THEN
            WRITE(6, '('' STEPS IN INTGRS >200, PRECISION LOST'')')
            GOTO 99999
         ENDIF
         n = n + n
         dintg1 = Dintg
         GOTO 100
      ENDIF
99999 END
C
C
      SUBROUTINE LSQLGV(Xp, Yp, Np, Qq, Lmi, Lmx, Emm, Err, Rwo, Mxr)
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
C-D  MXR does not exceed (LMX+3)*(LMX+1) .
C-D    On output, the Legendre coefficients are stored in QQ. The actual
C-D  order of Legendre polynomials used is contained in LMX.
C-External: LSQLEG, MTXGUP, PLNLEG, POLLG1
C-
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C
C Dummy arguments
C
      DOUBLE PRECISION Emm, Err
      INTEGER Lmi, Lmx, Mxr, Np
      DOUBLE PRECISION Qq(Lmx), Rwo(Mxr), Xp(Np), Yp(Np)
C
C Local variables
C
      DOUBLE PRECISION d1, rer, sy, yci
      DOUBLE PRECISION DMAX1
      INTEGER i, ip, jer, l, l1, lmx1, n1, nlg
      DOUBLE PRECISION POLLG1
C
C
      lmx1 = Lmx
      nlg = 0
      Lmx = MIN(Lmx, Np - 1)
C*    Check if zero-order
      Qq(1) = Yp(1)
      IF(Np.GE.2)THEN
         sy = 0.
         DO i = 2, Np
            sy = sy + 0.5*(Yp(i) + Yp(i - 1))*(Xp(i) - Xp(i - 1))
         ENDDO
         Qq(1) = sy/(Xp(Np) - Xp(1))
         IF(Lmx.GE.1)THEN
C*          Clear the coefficients field
            DO l = 1, Lmx
               Qq(l + 1) = 0.
            ENDDO
C*
C*          Loop to find the appropriate Legendre order
            l1 = MAX(1, Lmi)
            DO l = l1, Lmx
               IF((nlg + 1)*(nlg + 3).GT.Mxr)GOTO 100
               nlg = l
               n1 = nlg + 1
               CALL LSQLEG(Xp, Yp, Np, Qq, n1, Rwo, jer)
               IF(jer.NE.0)THEN
                  nlg = nlg - 1
                  GOTO 100
               ENDIF
C*             Check the difference between input and calculated points
               Err = 0.
               DO ip = 1, Np
                  d1 = POLLG1(Xp(ip), Qq(1), nlg)
                  yci = d1
                  IF(yci.NE.0.0D0)rer = ABS((yci - Yp(ip))/yci)
                  Err = DMAX1(rer, Err)
               ENDDO
               IF(Err.LE.Emm)GOTO 100
            ENDDO
C*
         ENDIF
      ENDIF
 100  Lmx = nlg
      END
C
C
      SUBROUTINE LSQLEG(Xp, Yp, Np, Qq, N1, Aa, Ier)
C-Title  : LSQLEG Subroutine
C-Purpose: Fit Legendre coefficients to a set of data
C-Description:
C-D
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C
C Dummy arguments
C
      INTEGER Ier, N1, Np
      DOUBLE PRECISION Aa(N1, N1 + 2), Qq(N1), Xp(Np), Yp(Np)
C
C Local variables
C
      DOUBLE PRECISION det, pi, pj
      INTEGER i, j, ldig, lf, lp, m, nlg
C
C
C*    Perform linear transformation of the coordinate system
      Ier = 0
      lf = N1 + 1
      lp = lf + 1
C*    Clear the matrix
      DO i = 1, N1
         Aa(i, lf) = 0.
         DO j = 1, N1
            Aa(j, i) = 0.
         ENDDO
      ENDDO
C*    Set up the matrix
      nlg = N1 - 1
      DO m = 1, Np
C*       Calculate Legendre polynomials
         CALL PLNLEG(Xp(m), Aa(1, lp), nlg)
         DO i = 1, N1
            pi = Aa(i, lp)
            Aa(i, lf) = Aa(i, lf) + Yp(m)*pi
            DO j = i, N1
               pj = Aa(j, lp)
               Aa(j, i) = Aa(j, i) + pi*pj
               Aa(i, j) = Aa(j, i)
            ENDDO
         ENDDO
      ENDDO
C*    Solve the system of equations
      CALL MTXGUP(Aa, Aa(1, lf), Qq, N1, ldig, det)
      IF(det.NE.0.0D0)RETURN
      Ier = 1
      END
C
C
      DOUBLE PRECISION FUNCTION POLLG1(Uu, Ql, Nl)
C-Title  : POLLG1 Function
C-Purpose: Legendre polynomial Sum( Ql* Pl(u) ) function
C-Description:
C-D  Evaluate Legendre polynomial expansion of order NL with
C-D  coefficients QL at argument value UU in the interval [-1,1]
C-Author : A.Trkov, Institute J.Stefan, Ljubljana, Slovenia, (1997)
C-
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C
C Dummy arguments
C
      INTEGER Nl
      DOUBLE PRECISION Uu
      DOUBLE PRECISION Ql(Nl + 1)
C
C Local variables
C
      INTEGER l, n1
      DOUBLE PRECISION pl(20), ss
C
C
C     DIMENSION QL(1),PL(20)
      IF(Nl.GE.20)STOP 'POLLG1 ERROR - Array PL capacity exceeded'
      CALL PLNLEG(Uu, pl, Nl)
      n1 = Nl + 1
      ss = 0.
      DO l = 1, n1
         ss = ss + Ql(l)*pl(l)
      ENDDO
      POLLG1 = ss
      END
C
C
      SUBROUTINE PLNLEG(Uu, Pl, Nl)
C-Title  : PLNLEG Subroutine
C-Purpose: Evaluate Legendre polynomials up to order NL
C-Author : A.Trkov, Institute J.Stefan, Ljubljana, Slovenia (1997)
C-Description:
C-D  Given the argument value UU in the interval [-1,1], the
C-D  polynomials up to order NL are calculated by a recurrence
C-D  relation and stored in PL.
C-
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C
C Dummy arguments
C
      INTEGER Nl
      DOUBLE PRECISION Uu
      DOUBLE PRECISION Pl(Nl + 1)
C
C Local variables
C
      REAL FLOAT
      INTEGER l
C
C
C     DIMENSION PL(1)
      Pl(1) = 1.
      IF(Nl.LT.1)RETURN
      Pl(2) = Uu
      IF(Nl.LT.2)RETURN
      DO l = 2, Nl
         Pl(l + 1) = (Pl(l)*Uu*FLOAT(2*l - 1) - Pl(l - 1)*FLOAT(l - 1))
     &               /FLOAT(l)
      ENDDO
      END
C
C
      SUBROUTINE MTXGUP(A, F, X, N, Ldig, Det)
C-Title  : MTXGUP subroutine
C-Purpose: Matrix solver, Gauss elimination, part.pivoting
C-Description:
C-D Decompose matrix A, calculate determinant and/or solve a set of
C-D linear simultaneous equations  A x = F  (order n) using Gauss
C-D elimination technique with partial pivoting by rows.
C-Author : A.Trkov , Institute J.Stefan Ljubljana, 1984
C-Version: 93/3 - improved zero-determinant trapping
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C
C Dummy arguments
C
      DOUBLE PRECISION Det
      INTEGER Ldig, N
      DOUBLE PRECISION A(N, N), F(N), X(N)
C
C Local variables
C
      DOUBLE PRECISION a1, a2, er
      INTEGER i, i1, j, j1, k, k1
C
C
      Det = 1.
      er = 1.
      DO i = 2, N
         i1 = i - 1
C*       Find the pivot
         a1 = 0.
         DO k = i1, N
            IF(ABS(A(k,i1)).GE.a1)THEN
               a1 = ABS(A(k, i1))
               k1 = k
            ENDIF
         ENDDO
         IF(ABS(a1/Det).LE.1.D-6 .AND. i1.GE.2)THEN
            Det = 0.
            RETURN
         ENDIF
         Det = Det*a1
C        DET=DET+LOG10(ABS(A(I1,I1)))
         IF(k1.GE.i)THEN
            a1 = A(k1, i1)
            A(k1, i1) = A(i1, i1)
            A(i1, i1) = a1
            a1 = F(k1)
            F(k1) = F(i1)
            F(i1) = a1
         ENDIF
         DO j = i, N
            X(j) = A(j, i1)/A(i1, i1)
            A(j, i1) = 0.
            F(j) = F(j) - F(i1)*X(j)
         ENDDO
         DO j = i, N
            IF(k1.GE.i)THEN
               a1 = A(k1, j)
               A(k1, j) = A(i1, j)
               A(i1, j) = a1
            ENDIF
            DO k = i, N
               a1 = A(k, j)
               a2 = a1 - A(i1, j)*X(k)
               IF(ABS(a1).GT.0.D0)er = MIN(er, ABS(a2/a1))
               A(k, j) = a2
            ENDDO
         ENDDO
      ENDDO
C*    Estimate number of digits lost due to subtraction
      Ldig = ( - LOG10(er + 1.E-33)) + 1.
C*    Solve by backward substitution
      DO i = 2, N
         i1 = N - i + 2
         X(i1) = F(i1)/A(i1, i1)
         j1 = N + 1 - i
         DO j = 1, j1
            F(j) = F(j) - X(i1)*A(j, i1)
         ENDDO
      ENDDO
      X(1) = F(1)/A(1, 1)
      END
C
      SUBROUTINE MATIN(As, Bs, N, M, Determ)
      IMPLICIT DOUBLE PRECISION(a - H), DOUBLE PRECISION(O - Z)
      INCLUDE 'dimension.h'
C
C
C Dummy arguments
C
      DOUBLE PRECISION Determ
      INTEGER M, N
      DOUBLE PRECISION As(NDMSCS, NDMSCS), Bs(NDMSCS)
C
C Local variables
C
      DOUBLE PRECISION a(NDMSCS, NDMSCS), amax, b(NDMSCS), pivot(NDMSCS)
     &                 , sog, swap, t
      INTEGER i, icolum, indeks(NDMSCS, 2), ipivot(NDMSCS), irow, j, 
     &        jcolum, jrow, k, l, l1, nnn
C
C
C
C     matrix inversion with accompanying solution of liner equations
C
      EQUIVALENCE(irow, jrow)
      EQUIVALENCE(icolum, jcolum)
      EQUIVALENCE(amax, t, swap)
      DO j = 1, N
         b(j) = Bs(j)
         DO i = 1, N
            a(i, j) = As(i, j)
         ENDDO
      ENDDO
      DO j = 1, N
         indeks(j, 1) = 0
         indeks(j, 2) = 0
      ENDDO
      sog = 0.0
C     initialization
      Determ = 1.0
      DO j = 1, N
         pivot(j) = 0.0
         ipivot(j) = 0
      ENDDO
      DO i = 1, N
C
C        search for pivot element
C
         amax = pivot(1)*sog
         nnn = 0
         DO j = 1, N
            IF(ipivot(j).NE.1)THEN
               DO k = 1, N
                  IF(ipivot(k).GT.1)GOTO 100
                  IF(ipivot(k).NE.1)THEN
                     IF(ABS(amax) - ABS(a(j,k)).LT.0.D0)THEN
                        irow = j
                        icolum = k
                        amax = a(j, k)
                        nnn = nnn + 1
                     ENDIF
                  ENDIF
               ENDDO
            ENDIF
         ENDDO
         IF(nnn.LE.0)GOTO 100
         ipivot(icolum) = ipivot(icolum) + 1
C
C        interchange rows to put pivot element on diagonal
C
         IF(irow.NE.icolum)THEN
            Determ = -Determ
            DO l = 1, N
               swap = a(irow, l)
               a(irow, l) = a(icolum, l)
               a(icolum, l) = swap
            ENDDO
            IF(M.GT.0)THEN
               swap = b(irow)
               b(irow) = b(icolum)
               b(icolum) = swap
            ENDIF
         ENDIF
         indeks(i, 1) = irow
         indeks(i, 2) = icolum
         pivot(i) = a(icolum, icolum)
         a(icolum, icolum) = 1.0
         DO l = 1, N
            a(icolum, l) = a(icolum, l)/pivot(i)
         ENDDO
C
C        reduce non-pivot rows
C
         IF(M.GT.0)b(icolum) = b(icolum)/pivot(i)
         DO l1 = 1, N
            IF(l1.NE.icolum)THEN
               t = a(l1, icolum)
               a(l1, icolum) = 0.0
               DO l = 1, N
                  a(l1, l) = a(l1, l) - a(icolum, l)*t
               ENDDO
               IF(M.GT.0)b(l1) = b(l1) - b(icolum)*t
            ENDIF
         ENDDO
      ENDDO
C
C     interchange columns
C
      DO i = 1, N
         l = N + 1 - i
         IF(indeks(l, 1).NE.indeks(l, 2))THEN
            jrow = indeks(l, 1)
            jcolum = indeks(l, 2)
            DO k = 1, N
               swap = a(k, jrow)
               a(k, jrow) = a(k, jcolum)
               a(k, jcolum) = swap
            ENDDO
         ENDIF
      ENDDO
 100  DO j = 1, N
         b(j) = b(j)*ipivot(j)
         DO k = 1, N
            a(j, k) = a(j, k)*ipivot(j)*ipivot(k)
         ENDDO
      ENDDO
      DO j = 1, N
         DO i = 1, N
            As(i, j) = a(i, j)
         ENDDO
      ENDDO
      END
C
C
C
C
      SUBROUTINE MATIN1(As, Bs, N, M, Determ)
      IMPLICIT DOUBLE PRECISION(a - H), DOUBLE PRECISION(O - Z)
      INCLUDE 'dimension.h'
C
C
C PARAMETER definitions
C
      INTEGER ISD, NG
      PARAMETER(ISD = 0, NG = 4*NDMSCS - 3*ISD)
C
C Dummy arguments
C
      DOUBLE PRECISION Determ
      INTEGER M, N
      DOUBLE PRECISION As(NG, NG), Bs(NG)
C
C Local variables
C
      DOUBLE PRECISION a(NG, NG), amax, b(NG), pivot(NG), sog, swap, t
      INTEGER i, icolum, indeks(NG, 2), ipivot(NG), irow, j, jcolum, 
     &        jrow, k, l, l1, nnn
C
C
C
C     matrix inversion with accompanying solution of liner equations
C
      EQUIVALENCE(irow, jrow)
      EQUIVALENCE(icolum, jcolum)
      EQUIVALENCE(amax, t, swap)
      DO j = 1, N
         b(j) = Bs(j)
         DO i = 1, N
            a(i, j) = As(i, j)
         ENDDO
      ENDDO
      DO j = 1, N
         indeks(j, 1) = 0
         indeks(j, 2) = 0
      ENDDO
      sog = 0.0
C     initialization
      Determ = 1.0
      DO j = 1, N
         pivot(j) = 0.0
         ipivot(j) = 0
      ENDDO
      DO i = 1, N
C
C        search for pivot element
C
         amax = pivot(1)*sog
         nnn = 0
         DO j = 1, N
            IF(ipivot(j).NE.1)THEN
               DO k = 1, N
                  IF(ipivot(k).GT.1)GOTO 100
                  IF(ipivot(k).NE.1)THEN
                     IF(ABS(amax) - ABS(a(j,k)).LT.0.D0)THEN
                        irow = j
                        icolum = k
                        amax = a(j, k)
                        nnn = nnn + 1
                     ENDIF
                  ENDIF
               ENDDO
            ENDIF
         ENDDO
         IF(nnn.LE.0)GOTO 100
         ipivot(icolum) = ipivot(icolum) + 1
C
C        interchange rows to put pivot element on diagonal
C
         IF(irow.NE.icolum)THEN
            Determ = -Determ
            DO l = 1, N
               swap = a(irow, l)
               a(irow, l) = a(icolum, l)
               a(icolum, l) = swap
            ENDDO
            IF(M.GT.0)THEN
               swap = b(irow)
               b(irow) = b(icolum)
               b(icolum) = swap
            ENDIF
         ENDIF
         indeks(i, 1) = irow
         indeks(i, 2) = icolum
         pivot(i) = a(icolum, icolum)
         a(icolum, icolum) = 1.0
         DO l = 1, N
            a(icolum, l) = a(icolum, l)/pivot(i)
         ENDDO
C
C        reduce non-pivot rows
C
         IF(M.GT.0)b(icolum) = b(icolum)/pivot(i)
         DO l1 = 1, N
            IF(l1.NE.icolum)THEN
               t = a(l1, icolum)
               a(l1, icolum) = 0.0
               DO l = 1, N
                  a(l1, l) = a(l1, l) - a(icolum, l)*t
               ENDDO
               IF(M.GT.0)b(l1) = b(l1) - b(icolum)*t
            ENDIF
         ENDDO
      ENDDO
C
C     interchange columns
C
      DO i = 1, N
         l = N + 1 - i
         IF(indeks(l, 1).NE.indeks(l, 2))THEN
            jrow = indeks(l, 1)
            jcolum = indeks(l, 2)
            DO k = 1, N
               swap = a(k, jrow)
               a(k, jrow) = a(k, jcolum)
               a(k, jcolum) = swap
            ENDDO
         ENDIF
      ENDDO
 100  DO j = 1, N
         b(j) = b(j)*ipivot(j)
         DO k = 1, N
            a(j, k) = a(j, k)*ipivot(j)*ipivot(k)
         ENDDO
      ENDDO
      DO j = 1, N
         DO i = 1, N
            As(i, j) = a(i, j)
         ENDDO
      ENDDO
      END
C
      FUNCTION SMAT(Iz)
C
C-----RETURNS CHEMICAL SYMBOL OF AN ELEMENT WITH Z=IZ
C
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C
C Dummy arguments
C
      INTEGER Iz
      CHARACTER*2 SMAT
C
C Local variables
C
      CHARACTER*2 mat(0:110)
C
C
      DATA mat/'n ', 'p ', 'He', 'Li', 'Be', 'B ', 'C ', 'N ', 'O ', 
     &     'F ', 'Ne', 'Na', 'Mg', 'Al', 'Si', 'P ', 'S ', 'Cl', 'Ar', 
     &     'K ', 'Ca', 'Sc', 'Ti', 'V ', 'Cr', 'Mn', 'Fe', 'Co', 'Ni', 
     &     'Cu', 'Zn', 'Ga', 'Ge', 'As', 'Se', 'Br', 'Kr', 'Rb', 'Sr', 
     &     'Y ', 'Zr', 'Nb', 'Mo', 'Tc', 'Ru', 'Rh', 'Pd', 'Ag', 'Cd', 
     &     'In', 'Sn', 'Sb', 'Te', 'I ', 'Xe', 'Cs', 'Ba', 'La', 'Ce', 
     &     'Pr', 'Nd', 'Pm', 'Sm', 'Eu', 'Gd', 'Tb', 'Dy', 'Ho', 'Er', 
     &     'Tm', 'Yb', 'Lu', 'Hf', 'Ta', 'W ', 'Re', 'Os', 'Ir', 'Pt', 
     &     'Au', 'Hg', 'Tl', 'Pb', 'Bi', 'Po', 'At', 'Rn', 'Fr', 'Ra', 
     &     'Ac', 'Th', 'Pa', 'U ', 'Np', 'Pu', 'Am', 'Cm', 'Bk', 'Cf', 
     &     'Es', 'Fm', 'Md', 'No', 'Lr', 'Rf', 'Ha', 'Sg', 'Ns', 'Hs', 
     &     'Mt', '??'/
      IF(Iz.GT.109)THEN
         SMAT = mat(110)
         RETURN
      ENDIF
      SMAT = mat(Iz)
      END
C
      SUBROUTINE WHERE(Izaf, Nnuc, Iloc)
Ccc
Ccc   ********************************************************************
Ccc   *                                                          class:au*
Ccc   *                         W H E R E                                *
Ccc   *                                                                  *
Ccc   * Locates position of the IZAF nucleus in matrices (last index)    *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * input: IZAF=Z*1000+A                                             *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * output:NNUC - position (last index)                              *
Ccc   *        ILOC - =0 if nucleus has been found                       *
Ccc   *               =1 if nucleus has not been found, in this case     *
Ccc   *                  NNUC is the first free location.                *
Ccc   *                                                                  *
Ccc   * calls:none                                                       *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * author: M.Herman                                                 *
Ccc   * date:   22.Feb.1994                                              *
Ccc   * revision:#    by:name                     on:xx.mon.199x         *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C
C Dummy arguments
C
      INTEGER Iloc, Izaf, Nnuc
C
C
      Iloc = 0
      DO Nnuc = 1, NDNUC
         IF(IZA(Nnuc).EQ.Izaf)RETURN
         IF(IZA(Nnuc).EQ.0)THEN
            Iloc = 1
            RETURN
         ENDIF
      ENDDO
      WRITE(6, *)
     &' INSUFFICIENT MEMORY ALLOCATION TO ACOMODATE ALL REQUESTED NUCLEI
     &'
      WRITE(6, *)' INCREASE NDNUC PARAMETER IN GLOBAL AND RECOMPILE'
      WRITE(6, *)' EXECUTION STOPPED'
      STOP
      END
C
C
C
      SUBROUTINE WHEREJC(Izaf, Nejc, Iloc)
Ccc
Ccc   ********************************************************************
Ccc   *                                                          class:au*
Ccc   *                         W H E R E J C                            *
Ccc   *                                                                  *
Ccc   * Locates position of the IZAF ejectile in matrices                *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * input: IZAF=Z*1000+A                                             *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * output:NEJC - position                                           *
Ccc   *        ILOC - =0 if ejectile has been found                      *
Ccc   *               =1 if ejectile has not been found                  *
Ccc   *                                                                  *
Ccc   * calls:none                                                       *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * author: M.Herman                                                 *
Ccc   * date:   21.Feb.2000                                              *
Ccc   * revision:#    by:name                     on:xx.mon.20xx         *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C
C Dummy arguments
C
      INTEGER Iloc, Izaf, Nejc
C
C
      Iloc = 0
      DO Nejc = 1, NDEJC
         IF(IZAejc(Nejc).EQ.Izaf)RETURN
      ENDDO
      Iloc = 1
      WRITE(6, *)' WHEREJC HAS BEEN ASKED FOR UNKNOWN EJECTILE', Izaf
      WRITE(6, *)' EXECUTION STOPPED'
      STOP
      END
C
      SUBROUTINE MTXINV(A, X, Y, Mx, My, N, Eps, Irflag)
C
C-Title  : MTXINV subroutine
C-Purpose: Matrix inversion, Gauss-Jordan method, full pivoting
C-Description:
C-D   A(N,N)      - Matrix to be inverted
C-D   X(N),Y(N)   -
C-D   MX(N),MY(N) - Pivot interchange vector pointers,
C-D   N           - Matrix order
C-D   EPS         - If det(A) < EPS  matrix is considered singular
C-D   IRFLAG      - Error flag: 0 - normal termination, 1 - singularity
C-Author : Dr.J.Arkuszewski, EIR, Wurenlingen, Switzerland, (1986)
C-    Note : Comments rearranged and name changed from XIRTAM to MTXINV
C*    Pivot determination
C
C
C Dummy arguments
C
      DOUBLE PRECISION Eps
      INTEGER Irflag, N
      DOUBLE PRECISION A(N, N), X(N), Y(N)
      INTEGER Mx(N), My(N)
C
C Local variables
C
      DOUBLE PRECISION delta, piv, z
      INTEGER i, j, k, l
C
      delta = 1.0
      DO k = 1, N
         piv = 0.0
         DO i = k, N
            DO j = k, N
               IF(ABS(A(i,j)).GT.ABS(piv))THEN
                  piv = A(i, j)
                  Mx(k) = i
                  My(k) = j
               ENDIF
            ENDDO
         ENDDO
         delta = delta*piv
C*       Test for matrix singularity
         IF(ABS(delta).LT.Eps)THEN
            Irflag = 1
            RETURN
         ENDIF
C*       Interchange of pivot row with k row
         l = Mx(k)
         IF(l.NE.k)THEN
            DO j = 1, N
               z = A(l, j)
               A(l, j) = A(k, j)
               A(k, j) = z
            ENDDO
         ENDIF
C*       Interchange of pivot column with k column
         l = My(k)
         IF(l.NE.k)THEN
            DO i = 1, N
               z = A(i, l)
               A(i, l) = A(i, k)
               A(i, k) = z
            ENDDO
         ENDIF
C*       Jordan interchange
         DO j = 1, N
            IF(j.EQ.k)THEN
               X(j) = 1.0/piv
               Y(j) = 1.0
            ELSE
               X(j) = -A(k, j)/piv
               Y(j) = A(j, k)
            ENDIF
            A(k, j) = 0.0
            A(j, k) = 0.0
         ENDDO
         DO i = 1, N
            DO j = 1, N
               A(i, j) = A(i, j) + X(j)*Y(i)
            ENDDO
         ENDDO
      ENDDO
C*    Matrix ordering
      DO k = N, 1, -1
         l = Mx(k)
         IF(l.NE.k)THEN
            DO i = 1, N
               z = A(i, l)
               A(i, l) = A(i, k)
               A(i, k) = z
            ENDDO
         ENDIF
         l = My(k)
         IF(l.NE.k)THEN
            DO j = 1, N
               z = A(l, j)
               A(l, j) = A(k, j)
               A(k, j) = z
            ENDDO
         ENDIF
      ENDDO
      Irflag = 0
      END
C
C
      SUBROUTINE FINSP2(Di, Yi, N, Xo, Yo, Y1, M, F1, F2, Sc)
C*--FINSP21726
C-Title  : FINSP2 Subroutine
C-Purpose: Interpolate a function using quadratic splines
C-Description:
C-D  Interpolate a fuction for which average values over a set of
C-D  intervals are available. Quadratic splines are fitted by
C-D  determining the Legednre polynomial coefficients for each
C-D  interval, such that the fitted function is continuous in the
C-D  function and the first derivative values. The parameters
C-D  are the following:
C-D    N   number of points on input argument mesh
C-D   DI   input argument increment mesh (array of N values)
C-D   YI   average function values corresponding to DI(i)
C-D    M   number of points in the output mesh
C-D   XO   output argument mesh (array of M values in monotonic order.
C-D        It is implicitly assumed that the first value corresponds
C-D        to the origin of the input argument increments).
C-D   YO   interpolated function values corresponding to XO(i) (Output)
C-D   Y1   interpolated derivative values corresponding to XO(i) (Output)
C-D        NOTE: if derivatives ara not required, the same array may be
C-D              specified for YO and Y1 (i.e.: implicit equivalence)
C-D   F1   P2 coefficient in the first interval (usually zero)
C-D   F2   P2 coefficient in the last  interval (usually zero)
C-D   SC   scratch array of length 6N-4. On exit, the P1 and P2
C-D        Legendre polynomial expansion coefficients are contained
C-D        at SC(1) and SC(N+1) for all intervals, respectively.
C-D        Note that P0 coefficients correspond to to YI by defiition.
C-D
C-Extern.: MTXDG3
C-Author : A.Trkov, Institute J.Stefan, Ljubljana, Slovenia (1994)
      DOUBLE PRECISION d0, d1, d2, Di, F1, F2, Sc, u, x0, Xo, Y1, Yi, Yo
      INTEGER i, k, l, l1, l2, lf, lm, M, N, n2
      DIMENSION Di(1), Yi(1), Xo(1), Yo(1), Y1(1), Sc(1)
      IF(N.LT.1)RETURN
C*    Define the tri-diagonal matrix and the RHS vector
      n2 = N - 2
      l1 = 1
      l2 = l1 + N
      Sc(l1) = 0.
      Sc(l2) = F1
      Sc(l2 + N - 1) = F2
      IF(N.GE.2)THEN
         IF(N.GE.3)THEN
            lm = l2 + N
            lf = lm + n2*3
            d1 = 1./Di(1)
            d2 = 1./Di(2)
            d0 = 1./Di(3)
            DO i = 1, n2
               d0 = d1
               d1 = d2
               d2 = 1./Di(i + 2)
               Sc(lm + 3*i - 3) = 2.*d0*(d1 + d2)
               Sc(lm + 3*i - 2) = 2.*(d0*d2 + d1*(2.*d0 + 3.*d1 + 2.*d2)
     &                            )
               Sc(lm + 3*i - 1) = 2.*d2*(d1 + d0)
               Sc(lf + i - 1) = Yi(i)*d0*(d1 + d2) - Yi(i + 1)
     &                          *(2.*d0*d2 + d1*(d0 + d2)) + Yi(i + 2)
     &                          *d2*(d1 + d0)
            ENDDO
            Sc(lf + n2 - 1) = Sc(lf + n2 - 1) - 2.*d2*(d1 + d0)*F2
            d0 = 1./Di(1)
            d1 = 1./Di(2)
            d2 = 1./Di(3)
            Sc(lf) = Sc(lf) - 2.*d0*(d1 + d2)*F1
C*          Solve for the P2 coefficient of the interpolated function
            CALL MTXDG3(Sc(lm), Sc(lf), Sc(l2 + 1), n2, 0)
         ENDIF
C*       Calculate the P1 coefficients
         d1 = 1./Di(1)
         d2 = 1./Di(2)
         DO i = 2, N
            d0 = d1
            d1 = d2
            d2 = 1./Di(i + 1)
            Sc(l1 - 2 + i) = (d1*( - Yi(i-1) + Yi(i)) - 2.*d1*Sc(l2 + i
     &                       - 1) - (3.*d0 + d1)*Sc(l2 + i - 2))
     &                       /(d0 + d1)
         ENDDO
         Sc(l1 - 1 + N) = (d1*( - Yi(N-1) + Yi(N)) + (d1 + 3.*d2)
     &                    *Sc(l2 + N - 1) + 2.*d1*Sc(l2 + N - 2))
     &                    /(d1 + d2)
      ENDIF
C*
C*    Interpolate to the specified output grid
      l = 1
      k = 1
      IF(Di(1)*(Xo(M) - Xo(1)).LT.0D0)k = -1
      IF(k.LT.0)l = N
      x0 = Xo(1)
      DO i = 1, M
 50      u = 2.*(Xo(i) - x0)/Di(l) - 1.
         IF(u.GT.1.0D0)THEN
            IF(l + k.GE.1 .AND. l + k.LE.N)THEN
               x0 = x0 + Di(l)
               l = l + k
               GOTO 50
            ENDIF
         ENDIF
         Y1(i) = (Sc(l1 - 1 + l) + Sc(l2 - 1 + l)*3.*u)*2./Di(l)
         Yo(i) = Yi(l) + Sc(l1 - 1 + l)*u + Sc(l2 - 1 + l)
     &           *0.5*(3.*u*u - 1.)
      ENDDO
      END
C
      SUBROUTINE MTXDG3(A, F, X, N, Im)
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
      DOUBLE PRECISION A, F, X
      INTEGER i, Im, N, n1, ni
      DIMENSION A(3, N), F(N), X(N)
      n1 = N - 1
      IF(Im.LE.1)THEN
C*       Matrix decomposition - forward sweep  (A = LU)
         IF(N.GE.2)THEN
            A(3, 1) = -A(3, 1)/A(2, 1)
            DO i = 2, n1
               A(2, i) = A(2, i) + A(1, i)*A(3, i - 1)
               A(3, i) = -A(3, i)/A(2, i)
            ENDDO
            A(2, N) = A(2, N) + A(1, N)*A(3, n1)
         ENDIF
         IF(Im.GT.0)RETURN
      ENDIF
C*    Forward sweep (p = L-1 F)
      F(1) = F(1)/A(2, 1)
      DO i = 2, N
         F(i) = (F(i) - A(1, i)*F(i - 1))/A(2, i)
      ENDDO
C*    Backward sweep (x = U-1 p)
      X(N) = F(N)
      DO i = 1, n1
         ni = N - i
         X(ni) = F(ni) + A(3, ni)*X(ni + 1)
      ENDDO
      END
