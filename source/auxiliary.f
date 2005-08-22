Ccc   * $Author: Capote $
Ccc   * $Date: 2005-08-22 20:11:19 $
Ccc   * $Id: auxiliary.f,v 1.26 2005-08-22 20:11:19 Capote Exp $
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
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C
C Local variables
C
      INTEGER ie, il, ilw, inecse, irec, j, nang, necse, nejc, nejcd,
     &        netl, nnex, nnlv, nnuc
C
C
C
C Local variables
C
      F_Print = F_Print + 10
      XNI = 0.
      TORy = 4.
      EX1 = 0.0
      EX2 = 0.0
      NLW = 0
      CSFus = 0.0
      TOTcsfis = 0.0
      CRL = 0.0
      DENhf = 0.0
      DO nejc = 0, NDEJC
         CSEmis(nejc,0) = 0.0
         CSMsd(nejc) = 0.0
         CSHms(nejc) = 0.0
         if (nejc.le.2) CSMsc(nejc) = 0.0
         DO necse = 1, NDECSE
            CSE(necse,nejc,0) = 0.0
            CSEmsd(necse,nejc) = 0.0
            CSEhms(necse,nejc) = 0.0
            CSEfis(necse,nejc) = 0.0
            AUSpec(necse,nejc) = 0.0
            DO nang = 1, NDANG
               CSEa(necse,nang,nejc,0) = 0.0
               CSEa(necse,nang,nejc,1) = 0.0
               CSEahms(necse,nang,nejc) = 0.0
            ENDDO
         ENDDO
      ENDDO
      DO nejc = 0, NEJcm
         DO il = 1, NDLV
            REClev(il,nejc) = 0.0
            CSDirlev(il,nejc) = 0.0
         ENDDO
         DO ie = 1, NDECSE
            AUSpec(ie,nejc) = 0.0
         ENDDO
      ENDDO
      DO nnuc = 1, NDNUC
         POPmax(nnuc) = 0.0
         CSPrd(nnuc) = 0.0
         EX(1,nnuc) = 0.0
         JSTab(nnuc) = 0.0
         QPRod(nnuc) = -1000.0
         FISden(nnuc) = 1.0
         DO j = 1, NDLW
            FISb(j,nnuc) = 0.0
         ENDDO
         DO nejc = 1, NDEJC
            DO netl = 1, NDETL
               ETL(netl,nejc,nnuc) = 0.0
               LMAxtl(netl,nejc,nnuc) = 0
               DO ilw = 1, NDLW
                  TL(netl,ilw,nejc,nnuc) = 0.0
               ENDDO
            ENDDO
         ENDDO
         DO nnex = 1, NDEX
            EX(nnex,nnuc) = 0.0
            TNUc(nnex,nnuc) = 0.0
            UEXcit(nnex,nnuc) = 0.0
            TNUcf(nnex,nnuc) = 0.0
            DO ilw = 1, NDLW
               RO(nnex,ilw,nnuc) = 0.0
               ROF(nnex,ilw,nnuc) = 0.0
               POP(nnex,ilw,1,nnuc) = 0.0
               POP(nnex,ilw,2,nnuc) = 0.0
            ENDDO
            DO irec = 1, NDEREC
               RECcse(irec,nnex,nnuc) = 0.0
               RECcse(irec,0,nnuc) = 0.0
            ENDDO
         ENDDO
         EX(NDEX + 1,nnuc) = 0.0
         DO nnlv = 1, NDLV
            POPlv(nnlv,nnuc) = 0.0
         ENDDO
         DO nejc = 0, NDEJC
            CSEmis(nejc,nnuc) = 0.0
            DO necse = 1, NDECSE
               CSE(necse,nejc,nnuc) = 0.0
            ENDDO
         ENDDO
      ENDDO
      DO nnuc = 1, NDExclus
         DO necse = 0, NDEX_D
            DO inecse = 1, NDECSED
               DO nejcd = 0, NDEJCD
                  POPcseaf(necse,nejcd,inecse,nnuc) = 0.0
               ENDDO
               DO nejc = 0, NDEJC
                  POPcse(necse,nejc,inecse,nnuc) = 0.0
               ENDDO
            ENDDO
         ENDDO
      ENDDO
      DO nejc = 0, NDEJC
         SCRtem(nejc) = 0.0
         DO ilw = 1, NDLW
            DO nnex = 1, NDEX
               SCRt(nnex,ilw,1,nejc) = 0.0
               SCRt(nnex,ilw,2,nejc) = 0.0
            ENDDO
         ENDDO
         DO nnlv = 1, NDLV
            SCRtl(nnlv,nejc) = 0.0
         ENDDO
      ENDDO
      DO nang = 1, NDANG
         DO nnlv = 1, NDLV
            DO nejc = 0, NDEJC
               CSAlev(nang,nnlv,nejc) = 0.0
            ENDDO
         ENDDO
      ENDDO
      DO j = 1, NDLW
         REDmsc(j,1) = 1.0
         REDmsc(j,2) = 1.0
      ENDDO
      END


      SUBROUTINE INTGRS(A,B,Y,Dintg)
CCCC *****************************************************************
CCCC * Calculates an integral                                        *
CCCC *****************************************************************
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
      prec = 0.01
      sum = 0.
      n = 1
      d = B - A
      sum = Y(A) + Y(B)
      dintg1 = sum*d*.5
  100 x = A - d*.5
      DO i = 1, n
         x = x + d
         sum = sum + 2*Y(x)
      ENDDO
      d = d*.5
      Dintg = sum*d*.5
      IF (ABS(Dintg - dintg1) - prec*ABS(Dintg).GT.0.D0) THEN
         IF (n.GT.200) THEN
            WRITE (6,'('' STEPS IN INTGRS >200, PRECISION LOST'')')
            GOTO 99999
         ENDIF
         n = n + n
         dintg1 = Dintg
         GOTO 100
      ENDIF
99999 END


      SUBROUTINE LSQLGV(Xp,Yp,Np,Qq,Lmi,Lmx,Emm,Err,Rwo,Mxr)
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
      INTEGER i, ip, jer, l, l1, n1, nlg
      DOUBLE PRECISION POLLG1
      nlg = 0
      Lmx = MIN(Lmx,Np - 1)
C*    Check if zero-order
      Qq(1) = Yp(1)
      IF (Np.GE.2) THEN
         sy = 0.
         DO i = 2, Np
            sy = sy + 0.5*(Yp(i) + Yp(i - 1))*(Xp(i) - Xp(i - 1))
         ENDDO
         Qq(1) = sy/(Xp(Np) - Xp(1))
         IF (Lmx.GE.1) THEN
C*          Clear the coefficients field
            DO l = 1, Lmx
               Qq(l + 1) = 0.
            ENDDO
C*
C*          Loop to find the appropriate Legendre order
            l1 = MAX(1,Lmi)
            DO l = l1, Lmx
               IF ((nlg + 1)*(nlg + 3).GT.Mxr) GOTO 100
               nlg = l
               n1 = nlg + 1
               CALL LSQLEG(Xp,Yp,Np,Qq,n1,Rwo,jer)
               IF (jer.NE.0) THEN
                  nlg = nlg - 1
                  GOTO 100
               ENDIF
C*             Check the difference between input and calculated points
               Err = 0.
               DO ip = 1, Np
                  d1 = POLLG1(Xp(ip),Qq(1),nlg)
                  yci = d1
                  IF (yci.NE.0.0D0) rer = ABS((yci - Yp(ip))/yci)
                  Err = DMAX1(rer,Err)
               ENDDO
               IF (Err.LE.Emm) GOTO 100
            ENDDO
C*
         ENDIF
      ENDIF
  100 Lmx = nlg
      END


      SUBROUTINE LSQLEG(Xp,Yp,Np,Qq,N1,Aa,Ier)
C-Title  : LSQLEG Subroutine
C-Purpose: Fit Legendre coefficients to a set of data
C-Description:
C-D
C
C
C Dummy arguments
C
      INTEGER Ier, N1, Np
      DOUBLE PRECISION Aa(N1,N1 + 2), Qq(N1), Xp(Np), Yp(Np)
C
C Local variables
C
      DOUBLE PRECISION det, pi, pj
      INTEGER i, j, ldig, lf, lp, m, nlg
C*    Perform linear transformation of the coordinate system
      Ier = 0
      lf = N1 + 1
      lp = lf + 1
C*    Clear the matrix
      DO i = 1, N1
         Aa(i,lf) = 0.
         DO j = 1, N1
            Aa(j,i) = 0.
         ENDDO
      ENDDO
C*    Set up the matrix
      nlg = N1 - 1
      DO m = 1, Np
C*       Calculate Legendre polynomials
         CALL PLNLEG(Xp(m),Aa(1,lp),nlg)
         DO i = 1, N1
            pi = Aa(i,lp)
            Aa(i,lf) = Aa(i,lf) + Yp(m)*pi
            DO j = i, N1
               pj = Aa(j,lp)
               Aa(j,i) = Aa(j,i) + pi*pj
               Aa(i,j) = Aa(j,i)
            ENDDO
         ENDDO
      ENDDO
C*    Solve the system of equations
      CALL MTXGUP(Aa,Aa(1,lf),Qq,N1,ldig,det)
      IF (det.NE.0.0D0) RETURN
      Ier = 1
      END


      DOUBLE PRECISION FUNCTION POLLG1(Uu,Ql,Nl)
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
C
C Dummy arguments
C
C
C Local variables
C
C
C
C     DIMENSION QL(1),PL(20)
      IF (Nl.GE.20) STOP 'POLLG1 ERROR - Array PL capacity exceeded'
      CALL PLNLEG(Uu,pl,Nl)
      n1 = Nl + 1
      ss = 0.
      DO l = 1, n1
         ss = ss + Ql(l)*pl(l)
      ENDDO
      POLLG1 = ss
      END


      SUBROUTINE PLNLEG(Uu,Pl,Nl)
C-Title  : PLNLEG Subroutine
C-Purpose: Evaluate Legendre polynomials up to order NL
C-Author : A.Trkov, Institute J.Stefan, Ljubljana, Slovenia (1997)
C-Description:
C-D  Given the argument value UU in the interval [-1,1], the
C-D  polynomials up to order NL are calculated by a recurrence
C-D  relation and stored in PL.
C-
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
      Pl(1) = 1.
      IF (Nl.LT.1) RETURN
      Pl(2) = Uu
      IF (Nl.LT.2) RETURN
      DO l = 2, Nl
         Pl(l + 1) = (Pl(l)*Uu*FLOAT(2*l - 1) - Pl(l - 1)*FLOAT(l - 1))
     &               /FLOAT(l)
      ENDDO
      END


      SUBROUTINE MTXGUP(A,F,X,N,Ldig,Det)
C-Title  : MTXGUP subroutine
C-Purpose: Matrix solver, Gauss elimination, part.pivoting
C-Description:
C-D Decompose matrix A, calculate determinant and/or solve a set of
C-D linear simultaneous equations  A x = F  (order n) using Gauss
C-D elimination technique with partial pivoting by rows.
C-Author : A.Trkov , Institute J.Stefan Ljubljana, 1984
C-Version: 93/3 - improved zero-determinant trapping
C
C
C Dummy arguments
C
      DOUBLE PRECISION Det
      INTEGER Ldig, N
      DOUBLE PRECISION A(N,N), F(N), X(N)
C
C Local variables
C
      DOUBLE PRECISION a1, a2, er
      INTEGER i, i1, j, j1, k, k1
C
      Det = 1.
      er = 1.
      DO i = 2, N
         i1 = i - 1
C*       Find the pivot
         a1 = 0.
         DO k = i1, N
            IF (ABS(A(k,i1)).GE.a1) THEN
               a1 = ABS(A(k,i1))
               k1 = k
            ENDIF
         ENDDO
         IF (ABS(a1/Det).LE.1.D-6 .AND. i1.GE.2) THEN
            Det = 0.
            RETURN
         ENDIF
         Det = Det*a1
         IF (k1.GE.i) THEN
            a1 = A(k1,i1)
            A(k1,i1) = A(i1,i1)
            A(i1,i1) = a1
            a1 = F(k1)
            F(k1) = F(i1)
            F(i1) = a1
         ENDIF
         DO j = i, N
            X(j) = A(j,i1)/A(i1,i1)
            A(j,i1) = 0.
            F(j) = F(j) - F(i1)*X(j)
         ENDDO
         DO j = i, N
            IF (k1.GE.i) THEN
               a1 = A(k1,j)
               A(k1,j) = A(i1,j)
               A(i1,j) = a1
            ENDIF
            DO k = i, N
               a1 = A(k,j)
               a2 = a1 - A(i1,j)*X(k)
               IF (ABS(a1).GT.0.D0) er = MIN(er,ABS(a2/a1))
               A(k,j) = a2
            ENDDO
         ENDDO
      ENDDO
C*    Estimate number of digits lost due to subtraction
      Ldig = ( - LOG10(er + 1.E-33)) + 1.
C*    Solve by backward substitution
      DO i = 2, N
         i1 = N - i + 2
         X(i1) = F(i1)/A(i1,i1)
         j1 = N + 1 - i
         DO j = 1, j1
            F(j) = F(j) - X(i1)*A(j,i1)
         ENDDO
      ENDDO
      X(1) = F(1)/A(1,1)
      END


      SUBROUTINE MATIN(As,Bs,N,M,Determ)
      INCLUDE 'dimension.h'
C
C
C Dummy arguments
C
      DOUBLE PRECISION Determ
      INTEGER M, N
      DOUBLE PRECISION As(NDMSCS,NDMSCS), Bs(NDMSCS)
C
C Local variables
C
      DOUBLE PRECISION a(NDMSCS,NDMSCS), amax, b(NDMSCS), pivot(NDMSCS),
     &                 sog, swap, t
      INTEGER i, icolum, indeks(NDMSCS,2), ipivot(NDMSCS), irow, j,
     &        jcolum, jrow, k, l, l1, nnn
C
C     matrix inversion with accompanying solution of liner equations
C
      EQUIVALENCE (irow,jrow)
      EQUIVALENCE (icolum,jcolum)
      EQUIVALENCE (amax,t,swap)
      DO j = 1, N
         b(j) = Bs(j)
         DO i = 1, N
            a(i,j) = As(i,j)
         ENDDO
      ENDDO
      DO j = 1, N
         indeks(j,1) = 0
         indeks(j,2) = 0
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
            IF (ipivot(j).NE.1) THEN
               DO k = 1, N
                  IF (ipivot(k).GT.1) GOTO 100
                  IF (ipivot(k).NE.1) THEN
                     IF (ABS(amax) - ABS(a(j,k)).LT.0.D0) THEN
                        irow = j
                        icolum = k
                        amax = a(j,k)
                        nnn = nnn + 1
                     ENDIF
                  ENDIF
               ENDDO
            ENDIF
         ENDDO
         IF (nnn.LE.0) GOTO 100
         ipivot(icolum) = ipivot(icolum) + 1
C
C        interchange rows to put pivot element on diagonal
C
         IF (irow.NE.icolum) THEN
            Determ = -Determ
            DO l = 1, N
               swap = a(irow,l)
               a(irow,l) = a(icolum,l)
               a(icolum,l) = swap
            ENDDO
            IF (M.GT.0) THEN
               swap = b(irow)
               b(irow) = b(icolum)
               b(icolum) = swap
            ENDIF
         ENDIF
         indeks(i,1) = irow
         indeks(i,2) = icolum
         pivot(i) = a(icolum,icolum)
         a(icolum,icolum) = 1.0
         DO l = 1, N
            a(icolum,l) = a(icolum,l)/pivot(i)
         ENDDO
C
C        reduce non-pivot rows
C
         IF (M.GT.0) b(icolum) = b(icolum)/pivot(i)
         DO l1 = 1, N
            IF (l1.NE.icolum) THEN
               t = a(l1,icolum)
               a(l1,icolum) = 0.0
               DO l = 1, N
                  a(l1,l) = a(l1,l) - a(icolum,l)*t
               ENDDO
               IF (M.GT.0) b(l1) = b(l1) - b(icolum)*t
            ENDIF
         ENDDO
      ENDDO
C
C     interchange columns
C
      DO i = 1, N
         l = N + 1 - i
         IF (indeks(l,1).NE.indeks(l,2)) THEN
            jrow = indeks(l,1)
            jcolum = indeks(l,2)
            DO k = 1, N
               swap = a(k,jrow)
               a(k,jrow) = a(k,jcolum)
               a(k,jcolum) = swap
            ENDDO
         ENDIF
      ENDDO
  100 DO j = 1, N
         b(j) = b(j)*ipivot(j)
         DO k = 1, N
            a(j,k) = a(j,k)*ipivot(j)*ipivot(k)
         ENDDO
      ENDDO
      DO j = 1, N
         DO i = 1, N
            As(i,j) = a(i,j)
         ENDDO
      ENDDO
      END



      SUBROUTINE MATIN1(As,Bs,N,M,Determ)
      INCLUDE 'dimension.h'
C
C
C PARAMETER definitions
C
      INTEGER ISD, NG
      PARAMETER (ISD = 0,NG = 4*NDMSCS - 3*ISD)
C
C Dummy arguments
C
      DOUBLE PRECISION Determ
      INTEGER M, N
      DOUBLE PRECISION As(NG,NG), Bs(NG)
C
C Local variables
C
      DOUBLE PRECISION a(NG,NG), amax, b(NG), pivot(NG), sog, swap, t
      INTEGER i, icolum, indeks(NG,2), ipivot(NG), irow, j, jcolum,
     &        jrow, k, l, l1, nnn
C
C     matrix inversion with accompanying solution of liner equations
C
      EQUIVALENCE (irow,jrow)
      EQUIVALENCE (icolum,jcolum)
      EQUIVALENCE (amax,t,swap)
      DO j = 1, N
         b(j) = Bs(j)
         DO i = 1, N
            a(i,j) = As(i,j)
         ENDDO
      ENDDO
      DO j = 1, N
         indeks(j,1) = 0
         indeks(j,2) = 0
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
            IF (ipivot(j).NE.1) THEN
               DO k = 1, N
                  IF (ipivot(k).GT.1) GOTO 100
                  IF (ipivot(k).NE.1) THEN
                     IF (ABS(amax) - ABS(a(j,k)).LT.0.D0) THEN
                        irow = j
                        icolum = k
                        amax = a(j,k)
                        nnn = nnn + 1
                     ENDIF
                  ENDIF
               ENDDO
            ENDIF
         ENDDO
         IF (nnn.LE.0) GOTO 100
         ipivot(icolum) = ipivot(icolum) + 1
C
C        interchange rows to put pivot element on diagonal
C
         IF (irow.NE.icolum) THEN
            Determ = -Determ
            DO l = 1, N
               swap = a(irow,l)
               a(irow,l) = a(icolum,l)
               a(icolum,l) = swap
            ENDDO
            IF (M.GT.0) THEN
               swap = b(irow)
               b(irow) = b(icolum)
               b(icolum) = swap
            ENDIF
         ENDIF
         indeks(i,1) = irow
         indeks(i,2) = icolum
         pivot(i) = a(icolum,icolum)
         a(icolum,icolum) = 1.0
         DO l = 1, N
            a(icolum,l) = a(icolum,l)/pivot(i)
         ENDDO
C
C        reduce non-pivot rows
C
         IF (M.GT.0) b(icolum) = b(icolum)/pivot(i)
         DO l1 = 1, N
            IF (l1.NE.icolum) THEN
               t = a(l1,icolum)
               a(l1,icolum) = 0.0
               DO l = 1, N
                  a(l1,l) = a(l1,l) - a(icolum,l)*t
               ENDDO
               IF (M.GT.0) b(l1) = b(l1) - b(icolum)*t
            ENDIF
         ENDDO
      ENDDO
C
C     interchange columns
C
      DO i = 1, N
         l = N + 1 - i
         IF (indeks(l,1).NE.indeks(l,2)) THEN
            jrow = indeks(l,1)
            jcolum = indeks(l,2)
            DO k = 1, N
               swap = a(k,jrow)
               a(k,jrow) = a(k,jcolum)
               a(k,jcolum) = swap
            ENDDO
         ENDIF
      ENDDO
  100 DO j = 1, N
         b(j) = b(j)*ipivot(j)
         DO k = 1, N
            a(j,k) = a(j,k)*ipivot(j)*ipivot(k)
         ENDDO
      ENDDO
      DO j = 1, N
         DO i = 1, N
            As(i,j) = a(i,j)
         ENDDO
      ENDDO
      END
C
      FUNCTION SMAT(Iz)
C
C-----RETURNS CHEMICAL SYMBOL OF AN ELEMENT WITH Z=IZ
C
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
C
C
C Dummy arguments
C
C
C Local variables
C
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
     &     'Es', 'Fm', 'Md', 'No', 'Lr', 'Rf', 'Db', 'Sg', 'Ns', 'Hs',
     &     'Mt', '??'/
      IF (Iz.GT.109) THEN
         SMAT = mat(110)
         RETURN
      ENDIF
      SMAT = mat(Iz)
      END


      SUBROUTINE WHERE(Izaf,Nnuc,Iloc)
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
C
      Iloc = 0
      DO Nnuc = 1, NDNUC
         IF (IZA(Nnuc).EQ.Izaf) RETURN
         IF (IZA(Nnuc).EQ.0) THEN
            Iloc = 1
            RETURN
         ENDIF
      ENDDO
      WRITE (6,*) ' Nucleus Izaf ',Izaf,' not found'
      WRITE (6,*)
     &' INSUFFICIENT MEMORY ALLOCATION TO ACOMODATE ALL REQUESTED NUCLEI
     &'
      WRITE (6,*) ' INCREASE NDNUC PARAMETER IN global.h AND RECOMPILE'
      WRITE (6,*) ' EXECUTION STOPPED'
      STOP
      END
C
C
C
      SUBROUTINE WHEREJC(Izaf,Nejc,Iloc)
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
         IF (IZAejc(Nejc).EQ.Izaf) RETURN
      ENDDO
      Iloc = 1
      WRITE (6,*) ' WHEREJC HAS BEEN ASKED FOR UNKNOWN EJECTILE', Izaf
      WRITE (6,*) ' EXECUTION STOPPED'
      STOP
      END


      SUBROUTINE MTXINV(A,X,Y,Mx,My,N,Eps,Irflag)
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
C
C Dummy arguments
C
      DOUBLE PRECISION Eps
      INTEGER Irflag, N
      DOUBLE PRECISION A(N,N), X(N), Y(N)
      INTEGER Mx(N), My(N)
C
C Local variables
C
      DOUBLE PRECISION delta, piv, z
      INTEGER i, j, k, l
C
C
C*    Pivot determination
      delta = 1.0
      DO k = 1, N
         piv = 0.0
         DO i = k, N
            DO j = k, N
               IF (ABS(A(i,j)).GT.ABS(piv)) THEN
                  piv = A(i,j)
                  Mx(k) = i
                  My(k) = j
               ENDIF
            ENDDO
         ENDDO
         delta = delta*piv
C*       Test for matrix singularity
         IF (ABS(delta).LT.Eps) THEN
            Irflag = 1
            RETURN
         ENDIF
C*       Interchange of pivot row with k row
         l = Mx(k)
         IF (l.NE.k) THEN
            DO j = 1, N
               z = A(l,j)
               A(l,j) = A(k,j)
               A(k,j) = z
            ENDDO
         ENDIF
C*       Interchange of pivot column with k column
         l = My(k)
         IF (l.NE.k) THEN
            DO i = 1, N
               z = A(i,l)
               A(i,l) = A(i,k)
               A(i,k) = z
            ENDDO
         ENDIF
C*       Jordan interchange
         DO j = 1, N
            IF (j.EQ.k) THEN
               X(j) = 1.0/piv
               Y(j) = 1.0
            ELSE
               X(j) = -A(k,j)/piv
               Y(j) = A(j,k)
            ENDIF
            A(k,j) = 0.0
            A(j,k) = 0.0
         ENDDO
         DO i = 1, N
            DO j = 1, N
               A(i,j) = A(i,j) + X(j)*Y(i)
            ENDDO
         ENDDO
      ENDDO
C*    Matrix ordering
      DO k = N, 1, -1
         l = Mx(k)
         IF (l.NE.k) THEN
            DO i = 1, N
               z = A(i,l)
               A(i,l) = A(i,k)
               A(i,k) = z
            ENDDO
         ENDIF
         l = My(k)
         IF (l.NE.k) THEN
            DO j = 1, N
               z = A(l,j)
               A(l,j) = A(k,j)
               A(k,j) = z
            ENDDO
         ENDIF
      ENDDO
      Irflag = 0
      END


      SUBROUTINE MTXDG3(A,F,X,N,Im)
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
C
C
C Dummy arguments
C
      INTEGER Im, N
      DOUBLE PRECISION A(3,N), F(N), X(N)
C
C Local variables
C
      INTEGER i, n1, ni
C
C
      n1 = N - 1
      IF (Im.LE.1) THEN
C*       Matrix decomposition - forward sweep  (A = LU)
         IF (N.GE.2) THEN
            A(3,1) = -A(3,1)/A(2,1)
            DO i = 2, n1
               A(2,i) = A(2,i) + A(1,i)*A(3,i - 1)
               A(3,i) = -A(3,i)/A(2,i)
            ENDDO
            A(2,N) = A(2,N) + A(1,N)*A(3,n1)
         ENDIF
         IF (Im.GT.0) RETURN
      ENDIF
C*    Forward sweep (p = L-1 F)
      F(1) = F(1)/A(2,1)
      DO i = 2, N
         F(i) = (F(i) - A(1,i)*F(i - 1))/A(2,i)
      ENDDO
C*    Backward sweep (x = U-1 p)
      X(N) = F(N)
      DO i = 1, n1
         ni = N - i
         X(ni) = F(ni) + A(3,ni)*X(ni + 1)
      ENDDO
      END



      SUBROUTINE INTERMAT(Xi,Si,Yi,N,Xo,So,Yo,M,L,Emin,Emax)
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:apu*
Ccc   *                      I N T E R M A T                             *
Ccc   *                                                                  *
Ccc   * Interpolate along the first dimension of the array Yi assumed    *
Ccc   * to contain histograms on the same equidistant grid Si.           *
Ccc   * The resulting function(!), on the different but uniform grid, is *
Ccc   * added to the values contained in the array Yo in the energy      *
Ccc   * range specified by Emin and Emax. Data outside this range are    *
Ccc   * ignored. Emin and Emax must be within energy range span by Xi.   *
Ccc   * The interpolation is linear. The total area is conserved         *
Ccc   * but NOT for a single histogram bin. Negavtive results            *
Ccc   * are set to 0 (if this happens total area may not be conserved).  *
Ccc   *                                                                  *
Ccc   * This routine is used to summ two distributions stored on a       *
Ccc   * different energy grid. Usually, the first dimension of Y         *
Ccc   * corresponds to energy (interpolated) and the second one          *
Ccc   * to spin.                                                         *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * input:Xi   X value of the centre of the first histogram bin      *
Ccc   *       Si   input argument increment (hisogram bin width)         *
Ccc   *       Yi   histogram values given at the middle of the histogram *
Ccc   *            bin (e.g., in case of spectrum this will be cross     *
Ccc   *            section in mb/MeV)                                    *
Ccc   *        N   number of histogram bins (1-st dimension of Yi)       *
Ccc   *                                                                  *
Ccc   *       Xo   X value of the first element in the Yo array          *
Ccc   *       So   step in the argument of the Yo array                  *
Ccc   *       Yo   values of the function to which interpolated results  *
Ccc   *            will be added (in the above example of spectrum these *
Ccc   *            will be in mb/MeV)                                    *
Ccc   *        M   number of points in the Yo array (1-st dimension)     *
Ccc   *        L   second dimension of Yi and Yo                         *
Ccc   *       Emin lower limit of energy range for interpolation         *
Ccc   *       Emax upper limit of energy range for interpolation         *
Ccc   *                                                                  *
Ccc   * output:                                                          *
Ccc   *       Yo   interpolated function values corresponding to XO(i)   *
Ccc   *                                                                  *
Ccc   * calls:none                                                       *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      IMPLICIT NONE
C
C
C Dummy arguments
C
      DOUBLE PRECISION Emax, Emin, Si, So, Xi, Xo
      INTEGER L, M, N
      DOUBLE PRECISION Yi(N,L), Yo(M,L)
C
C Local variables
C
      REAL FLOAT
      INTEGER ii1, ii2, imax, imin, io1
      INTEGER INT
      DOUBLE PRECISION xint, xis, yiud, yiue
C-----Check ranges and steps
C     IF(Emin.LT.Xo) THEN
      IF (Emin - Xo.LT. - 0.0001) THEN
         WRITE (6,*) ' '
         WRITE (6,*) 'INTERMAT: Inconsistent request               '
         WRITE (6,*) 'INTERMAT: Lower limit point requested: ', Emin
         WRITE (6,*) 'INTERMAT: is below the minimum:        ', Xo
         WRITE (6,*) 'INTERMAT: Execution terminated'
         STOP
      ENDIF
      IF (Emax - (Xo + (M-1)*So).GT.0.0001) THEN
         WRITE (6,*) ' '
         WRITE (6,*) 'INTERMAT: Inconsistent request               '
         WRITE (6,*) 'INTERMAT: Upper limit point requested: ', Emax
         WRITE (6,*) 'INTERMAT: is above the maximum:        ',
     &               Xo + (M - 1)*So
         WRITE (6,*) 'INTERMAT: Execution terminated'
         STOP
      ENDIF
      IF (Xi - Emin.GT.0.5*Si) THEN
         WRITE (6,*) ' '
         WRITE (6,*) 'INTERMAT: Lower limit point provided:  ', Xi
         WRITE (6,*) 'INTERMAT: Lower limit point requested: ', Emin
         WRITE (6,*) 'INTERMAT: I am instructed not to extrapolate '
         WRITE (6,*) 'INTERMAT: Execution terminated'
         STOP
      ENDIF
      IF (Emax - (Xi + (N-1)*Si).GT.0.5*Si) THEN
         WRITE (6,*) ' '
         WRITE (6,*) 'INTERMAT: Upper limit point requested: ', Emax
         WRITE (6,*) 'INTERMAT: Upper limit point  provided: ',
     &               Xi + (N - 1)*Si
         WRITE (6,*) 'INTERMAT: I am instructed not to extrapolate '
         WRITE (6,*) 'INTERMAT: Execution terminated'
         STOP
      ENDIF
      IF (So.LE.0 .OR. Si.LE.0) THEN
         WRITE (6,*) ' '
         WRITE (6,*) 'INTERMAT: Both X increments must be positive '
         WRITE (6,*) 'INTERMAT: Provided input  increment: ', Si
         WRITE (6,*) 'INTERMAT: Provided output increment: ', So
         WRITE (6,*) 'INTERMAT: Execution terminated'
         STOP
      ENDIF
C-----Start with the matrix
      DO ii2 = 1, L   !take one column at a time
C--------extrapolate to get just one point above the last
         yiue = 2*Yi(N,ii2) - Yi(N - 1,ii2)
         yiue = MAX(0.0D0,yiue)
C--------extrapolate to get just one point blow the first
         yiud = 2*Yi(1,ii2) - Yi(2,ii2)
         yiud = MAX(0.0D0,yiud)
C--------define indices corresponding to the requested energy range
         imin = (Emin - Xo)/So + 1.01
         imax = (Emax - Xo)/So + 1.01
C--------start intrpolation
         DO io1 = imin, imax
            xis = ((Xo + (io1-1)*So) - Xi)/Si + 1
            ii1 = INT(xis)
            IF (N.EQ.1) THEN
               xint = Yi(1,ii2)
            ELSEIF (ii1.EQ.0) THEN
               xint = yiud + (Yi(1,ii2) - yiud)*(xis - FLOAT(ii1))
            ELSEIF (ii1.EQ.N) THEN
               xint = Yi(ii1,ii2) + (yiue - Yi(ii1,ii2))
     &                *(xis - FLOAT(ii1))
            ELSE
               xint = Yi(ii1,ii2) + (Yi(ii1 + 1,ii2) - Yi(ii1,ii2))
     &                *(xis - FLOAT(ii1))
            ENDIF
            IF (xint.LT.0) xint = 0
            Yo(io1,ii2) = Yo(io1,ii2) + xint
         ENDDO
      ENDDO
      END



      SUBROUTINE BINTERMAT(Yi,Xi,Sxi,Nxi,Zi,Szi,Nzi,Yo,Xo,Sxo,Nxo,Zo,
     &                     Szo,Nzo,Exmin,Exmax,Ezmin,Ezmax)
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:apu*
Ccc   *                    B I N T E R M A T                             *
Ccc   *                                                                  *
Ccc   * Interpolate along both dimensions of the array Yi(x,z) assumed   *
Ccc   * to contain histograms on the equidistant grid Sxi, Szi.          *
Ccc   * The resulting function(!), on the different but uniform grid, is *
Ccc   * added to the values contained in the array Yo in the energy      *
Ccc   * rectangle specified by Exmin-Exmax and Ezmin-Ezmax.              *
Ccc   * Data outside this range are ignored.                             *
Ccc   * E.min and E.max must be within energy range span by Xi,Zi.       *
Ccc   * The interpolation is bilinear. The total area is conserved       *
Ccc   * but NOT for a one single histogram bin.   Negative results       *
Ccc   * are set to 0 (if this happens total area is not conserved).      *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * input:                                                           *
Ccc   *       Yi   histogram values given at the middle of the histogram *
Ccc   *            bin (e.g., in case of spectrum this will be cross     *
Ccc   *            section in mb/MeV)                                    *
Ccc   *       Xi   X value of the centre of the first histogram bin      *
Ccc   *       Sxi  input argument increment 1-st dim (hisogram bin width)*
Ccc   *       Nxi  first dimension of the Yi array                       *
Ccc   *       Zi   Z value of the centre of the first bin (2nd dim)      *
Ccc   *       Szi  input argument increment 2-nd dim (hisogram bin width)*
Ccc   *       Nzi  second dimension of the Yi array                      *
Ccc   *                                                                  *
Ccc   *       Yo   values of the function to which interpolated results  *
Ccc   *            will be added (in the above example of spectrum these *
Ccc   *            will be in mb/MeV)                                    *
Ccc   *       Xo   X value of the first element in the Yo array (1st dim)*
Ccc   *       Sxo  step in the argument of the Yo array  (1st dim)       *
Ccc   *       Nxo  first dimension of the Yo array                       *
Ccc   *       Zo   X value of the first element in the Yo array (2nd dim)*
Ccc   *       Szo  step in the argument of the Yo array  (2nd dim)       *
Ccc   *       Nzo  second dimension of the Yo array                      *
Ccc   *       Exmin lower limit of energy range for interpolation        *
Ccc   *             (1st dim)                                            *
Ccc   *       Exmax upper limit of energy range for interpolation        *
Ccc   *             (1st dim)                                            *
Ccc   *       Ezmin lower limit of energy range for interpolation        *
Ccc   *             (2nd dim)                                            *
Ccc   *       Ezmax upper limit of energy range for interpolation        *
Ccc   *             (2nd dim)                                            *
Ccc   *                                                                  *
Ccc   * output:                                                          *
Ccc   *       Yo   interpolated function values at  (Xo(i),Zo(i))        *
Ccc   *                                                                  *
Ccc   * calls:none                                                       *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      IMPLICIT NONE
C
C
C Dummy arguments
C
      DOUBLE PRECISION Exmax, Exmin, Ezmax, Ezmin, Sxi, Sxo, Szi, Szo,
     &                 Xi, Xo, Zi, Zo
      INTEGER Nxi, Nxo, Nzi, Nzo
      DOUBLE PRECISION Yi(Nxi,Nzi), Yo(Nxo,Nzo)
C
C Local variables
C
      DOUBLE PRECISION f1, f2, f3, f4, fyi(0:Nxi + 1,0:Nzi + 1),
     &                 summino, t, u, xint, xis, zis
      INTEGER INT
      INTEGER ixi, ixmax, ixmin, ixo, izi, izmax, izmin, izo
C-----Check ranges and steps
      IF (Nxi.EQ.1 .OR. Nzi.EQ.1) THEN
         WRITE (6,*) ' DIMENSION EQUAL TO 1 IN BINTERMAT'
         STOP
      ENDIF
      IF (Exmin - Xo.LT. - 0.0001) THEN
         WRITE (6,*) ' '
         WRITE (6,*) 'BINTERMAT: Inconsistent request (1)           '
         WRITE (6,*) 'BINTERMAT: Lower limit point requested: ', Exmin
         WRITE (6,*) 'BINTERMAT: is below the minimum:        ', Xo
         WRITE (6,*) 'BINTERMAT: Execution terminated'
         STOP
      ENDIF
      IF (Exmax - (Xo + (Nxo-1)*Sxo).GT.0.0001) THEN
         WRITE (6,*) ' '
         WRITE (6,*) 'BINTERMAT: Inconsistent request (2)           '
         WRITE (6,*) 'BINTERMAT: Upper limit point requested: ', Exmax
         WRITE (6,*) 'BINTERMAT: is above the maximum:   ',
     &               Xo + (Nxo - 1)*Sxo
         WRITE (6,*) 'BINTERMAT: Execution terminated'
         STOP
      ENDIF
      IF (Xi - Exmin.GT.0.5*Sxi) THEN
         WRITE (6,*) ' '
         WRITE (6,*) 'BINTERMAT: Lower X limit point provided:  ', Xi
         WRITE (6,*) 'BINTERMAT: Lower X limit point requested: ', Exmin
         WRITE (6,*) 'BINTERMAT: I am instructed not to extrapolate '
         WRITE (6,*) 'BINTERMAT: Execution terminated'
         STOP
      ENDIF
      IF (Exmax - (Xi + (Nxi-1)*Sxi).GT.0.5*Sxi) THEN
         WRITE (6,*) ' '
         WRITE (6,*) 'BINTERMAT: Upper X limit point requested: ', Exmax
         WRITE (6,*) 'BINTERMAT: Upper X limit point provided:',
     &               Xi + (Nxi - 1)*Sxi
         WRITE (6,*) 'BINTERMAT: I am instructed not to extrapolate '
         WRITE (6,*) 'BINTERMAT: Execution terminated'
         STOP
      ENDIF
      IF (Ezmin - Zo.LT. - 0.0001) THEN
         WRITE (6,*) ' '
         WRITE (6,*) 'BINTERMAT: Inconsistent request (3)           '
         WRITE (6,*) 'BINTERMAT: Lower limit point requested: ', Ezmin
         WRITE (6,*) 'BINTERMAT: is below the minimum:        ', Zo
         WRITE (6,*) 'BINTERMAT: Execution terminated'
         STOP
      ENDIF
      IF (Ezmax - (Zo + (Nzo-1)*Szo).GT.0.0001) THEN
         WRITE (6,*) ' '
         WRITE (6,*) 'BINTERMAT: Inconsistent request (4)           '
         WRITE (6,*) 'BINTERMAT: Upper limit point requested: ', Ezmax
         WRITE (6,*) 'BINTERMAT: is above the maximum:    ',
     &               Zo + (Nzo - 1)*Szi
         WRITE (6,*) 'BINTERMAT: Execution terminated'
         STOP
      ENDIF
      IF (Zi - Ezmin.GT.0.5*Szi) THEN
         WRITE (6,*) ' '
         WRITE (6,*) 'BINTERMAT: Lower Z limit point provided:  ', Zi
         WRITE (6,*) 'BINTERMAT: Lower Z limit point requested: ', Ezmin
         WRITE (6,*) 'BINTERMAT: I am instructed not to extrapolate '
         WRITE (6,*) 'BINTERMAT: Execution terminated'
         STOP
      ENDIF
      IF (Ezmax - (Zi + (Nzi-1)*Szi).GT.0.5*Szi) THEN
         WRITE (6,*) ' '
         WRITE (6,*) 'BINTERMAT: Upper Z limit point requested: ', Ezmax
         WRITE (6,*) 'BINTERMAT: Upper Z limit point  provided:',
     &               Zi + (Nzi - 1)*Szi
         WRITE (6,*) 'BINTERMAT: I am instructed not to extrapolate '
         WRITE (6,*) 'BINTERMAT: Execution terminated'
         STOP
      ENDIF
      IF (Sxo.LE.0 .OR. Sxi.LE.0 .OR. Szo.LE.0 .OR. Szi.LE.0) THEN
         WRITE (6,*) ' '
         WRITE (6,*) 'BINTERMAT: All increments must be positive '
         WRITE (6,*) 'BINTERMAT: Provided input  increments: ', Sxi, Szi
         WRITE (6,*) 'BINTERMAT: Provided output increments: ', Sxo, Szo
         WRITE (6,*) 'BINTERMAT: Execution terminated'
         STOP
      ENDIF
C-----transfer input matrix onto  fyi (contains frame)
      DO ixi = 1, Nxi
         DO izi = 1, Nzi
            fyi(ixi,izi) = Yi(ixi,izi)
         ENDDO
      ENDDO
C-----define indices corresponding to the requested x range (1-st dim)
      ixmin = (Exmin - Xo)/Sxo + 1.01
      ixmax = (Exmax - Xo)/Sxo + 1.01
C
C-----define indices corresponding to the requested z range (2-nd dim)
      izmin = (Ezmin - Zo)/Szo + 1.01
      izmax = (Ezmax - Zo)/Szo + 1.01
C-----Fill frame of the fyi matrix
      DO izi = 1, Nzi   !over z (2-nd dimension)
C--------extrapolate to get one column to the left of Yi
         fyi(0,izi) = 2*Yi(1,izi) - Yi(2,izi)
C--------extrapolate to get one column at the end of Yi
         fyi(Nxi + 1,izi) = 2*Yi(Nxi,izi) - Yi(Nxi - 1,izi)
      ENDDO
      DO ixi = 1, Nxi   !over x (1-st dimension)
C--------extrapolate to get one row on the top of Yi
         fyi(ixi,0) = 2*Yi(ixi,1) - Yi(ixi,1)
C--------extrapolate to get one row at the bottom of Yi
         fyi(ixi,Nzi + 1) = 2*Yi(ixi,Nzi) - Yi(ixi,Nzi - 1)
      ENDDO
C-----start intrpolation
      summino = 0
      DO izo = izmin, izmax   !over z (2-nd dimension)
         DO ixo = ixmin, ixmax   !over x (1-st dimension)
C-----------localize four sorounding points
            xis = ((Xo + (ixo-1)*Sxo) - Xi)/Sxi + 1
            ixi = INT(xis)
            t = xis - ixi
            zis = ((Zo + (izo-1)*Szo) - Zi)/Szi + 1
            izi = INT(zis)
            u = zis - izi
            f1 = fyi(ixi,izi)
            f2 = fyi(ixi + 1,izi)
            f3 = fyi(ixi + 1,izi + 1)
            f4 = fyi(ixi,izi + 1)
C-----------interpolate
            xint = (1 - t)*(1 - u)*f1 + t*(1 - u)*f2 + t*u*f3 + (1 - t)
     &             *u*f4
            IF (xint.LT.0) xint = 0
            Yo(ixo,izo) = Yo(ixo,izo) + xint
            IF (izo.EQ.izmin .OR. izo.EQ.izmax) xint = xint/2.
            IF (ixo.EQ.ixmin .OR. ixo.EQ.ixmax) xint = xint/2.
            summino = summino + xint
         ENDDO
      ENDDO
C     WRITE(6,*)'recoil to cont=',summino*Sxo*Szo
      END
