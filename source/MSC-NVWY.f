Ccc   * $Author: herman $
Ccc   * $Date: 2003-09-25 21:16:57 $
Ccc   * $Id: MSC-NVWY.f,v 1.7 2003-09-25 21:16:57 herman Exp $
C
C
      SUBROUTINE DECHMS(Jc, Ipc, Nnur, Nejc)
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:PPu*
Ccc   *                         D E C M S C                              *
Ccc   *                (function to function version)                    *
Ccc   *                                                                  *
Ccc   * Calculates decay of a continuum state in nucleus 1 into          *
Ccc   * continuum  of the residual nucleus NNUR  through the emission of *
Ccc   * the ejectile NEJC in terms of the Heidelberg Multistep Compound  *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * input:JC   - spin index of the decaying state                    *
Ccc   *       IPC  - parity of the decaying state                        *
Ccc   *       NNUR - residual nucleus index                              *
Ccc   *       NEJC - ejectile index                                      *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * output:SUM - SUM of transmission coefficients over all outgoing  *
Ccc   *              channels for the requested decay (partial sums are  *
Ccc   *              stored in SCRM array for continuum)                 *
Ccc   *                                                                  *
Ccc   * calls: none                                                      *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * author: M.Herman                                                 *
Ccc   * date:   25.May.1994                                              *
Ccc   * revision:#    by:name                     on:xx.mon.199x         *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C
C PARAMETER definitions
C
      INTEGER NC2, IV
      PARAMETER(NC2 = NDMSCS + 2, IV = 18)
C
C COMMON variables
C
      INTEGER IE(NC2)
      DOUBLE PRECISION OMJd(0:2*NDMSCS + IV, NDLW), PIM(NDMSCS, NDMSCS), 
     &                 PREp(NC2, 2), SCRm(NDEX, NDLW, 2, 2, NDMSCS), 
     &                 WP(NDEX, NDLW, 2), YP(NDEX, NDMSCS, 3, 2), 
     &                 ZP(NDEX, NC2, 3, 2), ZSUm(NDEX, NC2, 2)
      COMMON /MSC   / SCRm, WP, YP, ZP, ZSUm, OMJd, PIM, PREp, IE
C
C Dummy arguments
C
      INTEGER Ipc, Jc, Nejc, Nnur
C
C Local variables
C
      DOUBLE PRECISION corr, hisr, ps1, ps2, s, smax, smin, tl1, tl2, 
     &                 tl3, tlj, xjc, xjr
      REAL FLOAT
      INTEGER i, ichsp, ier, iermax, ietl, iexc, ip1, ip2, ipar, itlc, 
     &        j, jr, l, lmax, lmaxf, lmin, mul, n
      INTEGER MIN0
C
C
C-----n o t e jtl.lt.5 has a special meaning
C-----n o t e missing transition to region between last level and ecut
C-----        for neutron emission
Cpr   WRITE(6,*) 'check for the parity in ro'
      hisr = HIS(Nnur)
      xjc = FLOAT(Jc) + HIS(1)
C-----clear scratch matrix
      DO n = 1, NDMSCS
         DO j = 1, NLW
            DO i = 1, NEX(Nnur) + 1
               SCRm(i, j, 1, Nejc, n) = 0.0
               SCRm(i, j, 2, Nejc, n) = 0.0
            ENDDO
         ENDDO
      ENDDO
      iexc = NEX(1) - NEXr(Nejc, 1)
      itlc = iexc - 5
      iermax = NEX(1) - iexc
      IF(iermax.GE.1)THEN
C--------
C--------decay to the continuum
C--------
C--------do loop over r.n. spins
         DO jr = 1, NLW, LTUrbo
            xjr = FLOAT(jr) + hisr
            smin = ABS(xjr - SEJc(Nejc))
            smax = xjr + SEJc(Nejc)
            mul = smax - smin + 1.0001
C-----------do loop over channel spin
            DO ichsp = 1, mul
               s = smin + FLOAT(ichsp - 1)
               lmin = ABS(xjc - s) + 1.01
               lmaxf = xjc + s + 1.01
               lmaxf = MIN0(NLW, lmaxf)
               ipar = (1 + Ipc*( - 1)**(lmin - 1))/2
C--------------parity index of r.n. state populated by emission with LMIN
               ip1 = 2 - ipar
C--------------parity index of r.n. state populated by emission with LMIN+1
               ip2 = 1
               IF(ip1.EQ.1)ip2 = 2
C--------------do loop over exciton classes
               DO n = 1, NDMSCS
C-----------------decay to the highest possible bin (non neutron only)
                  IF(ZEJc(Nejc).NE.0.0D0)THEN
                     ietl = 5
                     lmax = lmaxf
                     lmax = MIN0(LMAxtl(ietl + 1, Nejc, Nnur), lmax)
C--------------------do loop over l (odd and even l-values treated separately)
C--------------------IP1 and IP2 decide to which parity each SUMTL  goes
                     ps1 = 0.0
                     DO l = lmin, lmax, 2
C-----------------------Calculation of stage dependent transmission coefficients
C-----------------------using matrix element determined from the incident channel
C-----------------------factor of 4 missing (it will be applied when PS is calculated)
                        tlj = WP(ietl, l, Nejc)
                        tl1 = tlj*ZP(iermax, n, 1, Nejc)
     &                        /(1. + tlj*ZSUm(iermax, n, Nejc))**2
                        tl2 = tlj*ZP(iermax, n, 2, Nejc)
     &                        /(1. + tlj*ZSUm(iermax, n + 1, Nejc))**2
                        tl3 = tlj*ZP(iermax, n, 3, Nejc)
     &                        /(1. + tlj*ZSUm(iermax, n + 2, Nejc))**2
                        ps1 = ps1 + 
     &                        (tl1*YP(iermax, n, 1, Nejc)*OMJd(IE(n)
     &                        - 3, jr) + tl2*YP(iermax, n, 2, Nejc)
     &                        *OMJd(IE(n) - 1, jr)
     &                        + tl3*YP(iermax, n, 3, Nejc)
     &                        *OMJd(IE(n) + 1, jr))*PREp(n, Nejc)*4.
                     ENDDO
                     ps2 = 0.0
                     DO l = lmin + 1, lmax, 2
                        tlj = WP(ietl, l, Nejc)
                        tl1 = tlj*ZP(iermax, n, 1, Nejc)
     &                        /(1. + tlj*ZSUm(iermax, n, Nejc))**2
                        tl2 = tlj*ZP(iermax, n, 2, Nejc)
     &                        /(1. + tlj*ZSUm(iermax, n + 1, Nejc))**2
                        tl3 = tlj*ZP(iermax, n, 3, Nejc)
     &                        /(1. + tlj*ZSUm(iermax, n + 2, Nejc))**2
                        ps2 = ps2 + 
     &                        (tl1*YP(iermax, n, 1, Nejc)*OMJd(IE(n)
     &                        - 3, jr) + tl2*YP(iermax, n, 2, Nejc)
     &                        *OMJd(IE(n) - 1, jr)
     &                        + tl3*YP(iermax, n, 3, Nejc)
     &                        *OMJd(IE(n) + 1, jr))*PREp(n, Nejc)*4.
                     ENDDO
C--------------------do loop over l   ***done***
                     SCRm(iermax, jr, ip1, Nejc, n)
     &                  = SCRm(iermax, jr, ip1, Nejc, n) + ps1*TURbo
                     SCRm(iermax, jr, ip2, Nejc, n)
     &                  = SCRm(iermax, jr, ip2, Nejc, n) + ps2*TURbo
                     PIM(n, n) = PIM(n, n) + (ps1 + ps2)*TURbo
                  ENDIF
C-----------------decay to the highest but one bin (conditional see the next IF)
                  IF(ZEJc(Nejc).EQ.0.0D0 .AND. NEX(1).EQ.NEX(1) - 1)THEN
                     ietl = 6
                     lmax = lmaxf
                     lmax = MIN0(LMAxtl(ietl, Nejc, Nnur), lmax)
C--------------------do loop over l (odd and even l-values treated separately)
C--------------------IP1 and IP2 decide which parity each SUMTL  goes to
                     ps1 = 0.0
                     DO l = lmin, lmax, 2
                        tlj = WP(ietl, l, Nejc)
                        tl1 = tlj*ZP(iermax, n, 1, Nejc)
     &                        /(1. + tlj*ZSUm(iermax, n, Nejc))**2
                        tl2 = tlj*ZP(iermax, n, 2, Nejc)
     &                        /(1. + tlj*ZSUm(iermax, n + 1, Nejc))**2
                        tl3 = tlj*ZP(iermax, n, 3, Nejc)
     &                        /(1. + tlj*ZSUm(iermax, n + 2, Nejc))**2
                        ps1 = ps1 + 
     &                        (tl1*YP(iermax, n, 1, Nejc)*OMJd(IE(n)
     &                        - 3, jr) + tl2*YP(iermax, n, 2, Nejc)
     &                        *OMJd(IE(n) - 1, jr)
     &                        + tl3*YP(iermax, n, 3, Nejc)
     &                        *OMJd(IE(n) + 1, jr))*PREp(n, Nejc)*4.
                     ENDDO
                     ps2 = 0.0
                     DO l = lmin + 1, lmax, 2
                        tlj = WP(ietl, l, Nejc)
                        tl1 = tlj*ZP(iermax, n, 1, Nejc)
     &                        /(1. + tlj*ZSUm(iermax, n, Nejc))**2
                        tl2 = tlj*ZP(iermax, n, 2, Nejc)
     &                        /(1. + tlj*ZSUm(iermax, n + 1, Nejc))**2
                        tl3 = tlj*ZP(iermax, n, 3, Nejc)
     &                        /(1. + tlj*ZSUm(iermax, n + 2, Nejc))**2
                        ps2 = ps2 + 
     &                        (tl1*YP(iermax, n, 1, Nejc)*OMJd(IE(n)
     &                        - 3, jr) + tl2*YP(iermax, n, 2, Nejc)
     &                        *OMJd(IE(n) - 1, jr)
     &                        + tl3*YP(iermax, n, 3, Nejc)
     &                        *OMJd(IE(n) + 1, jr))*PREp(n, Nejc)*4.
                     ENDDO
C--------------------do loop over l   ***done***
C--------------------CORR in the next lines accounts for the Tl interpolation
C--------------------and integration over overlaping bins (2/3), it turned out it must
C--------------------be energy step and also emission step dependent
                     corr = 0.4444/(DE - XN(Nnur) + XN(1))*TURbo
                     SCRm(iermax, jr, ip1, Nejc, n)
     &                  = SCRm(iermax, jr, ip1, Nejc, n) + ps1*corr
                     SCRm(iermax, jr, ip2, Nejc, n)
     &                  = SCRm(iermax, jr, ip2, Nejc, n) + ps2*corr
                     PIM(n, n) = PIM(n, n) + (ps1 + ps2)*corr
                  ENDIF
C-----------------do loop over r.n. energies (highest bin and eventually the second
C-----------------bin from the top excluded as already done)
                  DO ier = iermax - 1, 1, -1
                     ietl = NEX(1) - ier - itlc
                     lmax = lmaxf
                     lmax = MIN0(LMAxtl(ietl, Nejc, Nnur), lmax)
C--------------------do loop over l (odd and even l-values treated separately)
C--------------------IP1 and IP2 decide which parity each SUMTL  goes to
                     ps1 = 0.0
                     DO l = lmin, lmax, 2
                        tlj = WP(ietl, l, Nejc)
                        tl1 = tlj*ZP(ier, n, 1, Nejc)
     &                        /(1. + tlj*ZSUm(ier, n, Nejc))**2
                        tl2 = tlj*ZP(ier, n, 2, Nejc)
     &                        /(1. + tlj*ZSUm(ier, n + 1, Nejc))**2
                        tl3 = tlj*ZP(ier, n, 3, Nejc)
     &                        /(1. + tlj*ZSUm(ier, n + 2, Nejc))**2
                        ps1 = ps1 + 
     &                        (tl1*YP(ier, n, 1, Nejc)*OMJd(IE(n) - 3, 
     &                        jr) + tl2*YP(ier, n, 2, Nejc)
     &                        *OMJd(IE(n) - 1, jr)
     &                        + tl3*YP(ier, n, 3, Nejc)
     &                        *OMJd(IE(n) + 1, jr))*PREp(n, Nejc)*4.
                     ENDDO
                     ps2 = 0.0
                     DO l = lmin + 1, lmax, 2
                        tlj = WP(ietl, l, Nejc)
                        tl1 = tlj*ZP(ier, n, 1, Nejc)
     &                        /(1. + tlj*ZSUm(ier, n, Nejc))**2
                        tl2 = tlj*ZP(ier, n, 2, Nejc)
     &                        /(1. + tlj*ZSUm(ier, n + 1, Nejc))**2
                        tl3 = tlj*ZP(ier, n, 3, Nejc)
     &                        /(1. + tlj*ZSUm(ier, n + 2, Nejc))**2
                        ps2 = ps2 + 
     &                        (tl1*YP(ier, n, 1, Nejc)*OMJd(IE(n) - 3, 
     &                        jr) + tl2*YP(ier, n, 2, Nejc)
     &                        *OMJd(IE(n) - 1, jr)
     &                        + tl3*YP(ier, n, 3, Nejc)
     &                        *OMJd(IE(n) + 1, jr))*PREp(n, Nejc)*4.
                     ENDDO
C--------------------do loop over l   ***done***
                     SCRm(ier, jr, ip1, Nejc, n)
     &                  = SCRm(ier, jr, ip1, Nejc, n) + ps1*TURbo
                     SCRm(ier, jr, ip2, Nejc, n)
     &                  = SCRm(ier, jr, ip2, Nejc, n) + ps2*TURbo
                     PIM(n, n) = PIM(n, n) + (ps1 + ps2)*TURbo
                  ENDDO
C-----------------do loop over r.n. energies ***done***
               ENDDO
            ENDDO
C-----------do loop over channel spins ***done***
         ENDDO
C--------do loop over and r.n. spins ***done***
C--------decay to the continuum ------ done -----------------------------
         DO j = 1, NLW, LTUrbo
            DO n = 1, NDMSCS
               PIM(n, n) = PIM(n, n)
     &                     - 0.5*(SCRm(1, j, 1, Nejc, n) + SCRm(1, j, 2, 
     &                     Nejc, n))
            ENDDO
         ENDDO
99001    FORMAT(1X, F5.2, 12G10.3)
      ENDIF
      END
C
      SUBROUTINE HMSC(Nvwful)
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:PPu*
Ccc   *                         H M S                                    *
Ccc   *                                                                  *
Ccc   * Calculates Multistep Compound decay in terms of the NVWY theory. *
Ccc   * Neutrons protons gammas are considered.                          *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * input:NVWFUL - if .TRUE. entire decay is calculated within MSC   *
Ccc   *                (without H-F following it). It means that enough  *
Ccc   *                MSC classes are taken into account. If .FALSE.    *
Ccc   *                H-F calculation will follow MSC chanin.           *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * output:none                                                      *
Ccc   *                                                                  *
Ccc   * calls:    AUERST                                                 *
Ccc   *           DECHMS                                                 *
Ccc   *           E1                                                     *
Ccc   *           GDOWN                                                  *
Ccc   *               W                                                  *
Ccc   *               W1                                                 *
Ccc   *               W2                                                 *
Ccc   *               W3                                                 *
Ccc   *           MATIN                                                  *
Ccc   *           MATIN1                                                 *
Ccc   *           OMJ                                                    *
Ccc   *           ROPHM                                                  *
Ccc   *           TRATES                                                 *
Ccc   *           VQ                                                     *
Ccc   *              WOBL                                                *
Ccc   *           W (see above)                                          *
Ccc   *           WHERE                                                  *
Ccc   *           WILLI                                                  *
Ccc   *           WOBL                                                   *
Ccc   *           WT                                                     *
Ccc   *               W (see above)                                      *
Ccc   *           ZERO                                                   *
Ccc   *               W (see above)                                      *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * author: M.Herman                                                 *
Ccc   * date:   25.May.1994                                              *
Ccc   * revision:1    by:M. Herman                on:04.Feb.2000         *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C
C PARAMETER definitions
C
      INTEGER NC2, ISD, NG, IV
      PARAMETER(NC2 = NDMSCS + 2, ISD = 0, NG = 4*NDMSCS - 3*ISD, 
     &          IV = 18)
C
C COMMON variables
C
      DOUBLE PRECISION BMSc, EXTr, FACt(100), G, GSP(1), 
     &                 OMJd(0:2*NDMSCS + IV, NDLW), PIM(NDMSCS, NDMSCS), 
     &                 PIM1(NDMSCS), PREp(NC2, 2), 
     &                 SCRm(NDEX, NDLW, 2, 2, NDMSCS), SIGnx, 
     &                 WP(NDEX, NDLW, 2), YP(NDEX, NDMSCS, 3, 2), 
     &                 ZP(NDEX, NC2, 3, 2), ZSUm(NDEX, NC2, 2)
      INTEGER IE(NC2)
      COMMON /DELETE/ GSP
      COMMON /EB    / EXTr, BMSc, FACt, G
      COMMON /MSC   / SCRm, WP, YP, ZP, ZSUm, OMJd, PIM, PREp, IE
      COMMON /PRSI  / SIGnx
C
C Dummy arguments
C
      LOGICAL Nvwful
C
C Local variables
C
      DOUBLE PRECISION accn, apig(NG, NG), apim(NDMSCS, NDMSCS), atil, 
     &                 aw(NDMSCS), awj(NDMSCS, NDLW), awm(NDMSCS), 
     &                 bbb(NDMSCS), ccre, cgd, coulb, det, ecc, ecor, 
     &                 ecor1, ecor2, ecor3, efc, eg, egdr, egdrr, emis, 
     &                 en, entl, eptl, ew, gamma, gdrw, 
     &                 gspmsc(NDMSCS, NDEX), omn, omp, pig(NG, NG), 
     &                 pimem(NDMSCS), pop1, pop2, popars, popart(NDMSCS)
     &                 , prop, propt, rbu, rog, rop, roph(NC2, 4), 
     &                 roph3(NC2), rophc(NC2), ropht(NC2), sdabs, 
     &                 sdsp(NDEX), sp, spg(NDMSCS), spr(NG), spw, 
     &                 sumtlg, t, tg, tg1, tg2, tg3, tgc, tl1, tl2, tl3, 
     &                 tlg, tls, tx, undeg(NG, NG), 
     &                 undex(NDMSCS, NDMSCS), vl, vmn, vu, xep(NC2, 2), 
     &                 xga, xxxx, y, yd, ygdr, yre, ysp, pigx(NG), 
     &                 pigy(NG)
      DOUBLE PRECISION E1, OMJ, ROPHM, VQ, W, WILLI, WOBL, WT
      REAL FLOAT
      INTEGER i, icse, ie1, iec, iex1, iexc, ih(NC2), ikc, iloc, inc, 
     &        ini, ip(NC2), ipii, ispin, ix, ixni, izares, izpcon, j, 
     &        jr, jx, k, k1, k2, k3, kc, kc1, kcn, ke, kg, kgin, ki, ks, 
     &        m, m1, m2, nc1, ncm, ncont, nejc, nnur, npigzero, nresn, 
     &        nresp, mpigx(NG), mpigy(NG)
C
C
      EQUIVALENCE(PIM(1, 1), PIM1)
C
      WRITE(6, '(1X,//,'' Options in Heidelberg M.S.C.'')')
      WRITE(6, '('' ----------------------------'',/)')
C     WRITE(6,'('' Transmission coeff. calculated dividing o.m. transmis
C     xsion ceoff. according to acc. state densities'')')
      WRITE(6, 
     &'('' Transmission coeff. calculated using matrix elements determin
     &ed in the incident channel'')')
C     WRITE(6,'('' gamma down calculated directly from the optical model
C     x imaginary part'')')
      WRITE(6, 
     &'('' Gamma down calculated using matrix element determined from th
     &e optical model imaginary part'')')
      WRITE(6, '('' using full spreading width of the GDR'')')
C     WRITE(6,'('' using spreading width of the GDR split among three cl
C     *asses'')')
C     WRITE(6,'('' using single particl matrix element with analogy to (
C     *1,4)-->(2,4)'')')
C-----input parameters
      SIGnx = 0.26*A(1)**0.66666667
      BMSc = Q(1, 1)
C-----input parameters              ***** done *******
      DO i = 1, NDEX
         sdsp(i) = 0.0
         DO j = 1, NDMSCS
            gspmsc(j, i) = 0.0
         ENDDO
      ENDDO
C-----determination of the initial exciton configuration for H.I.
      IF(AEJc(0).GT.4.0D0 .AND. XNI.EQ.0.D0)THEN
         coulb = 1.44*Z(0)*ZEJc(0)/(A(0)**0.66667*1.22)
         efc = EXCn/(4.6 + 0.54*(EIN - coulb)/AEJc(0))
         ecc = (0.09 + (0.38 - 0.08*(A(0)-AEJc(0))/(A(0)+AEJc(0)))
     &         *SQRT((EIN-coulb)/AEJc(0)))*AEJc(0)
         IF(EIN.LT.coulb + 30.0D0)THEN
            XNI = ecc
         ELSE
            XNI = efc
         ENDIF
         ixni = XNI + 0.5
         XNI = FLOAT(ixni)
         iex1 = XNI/2.0 + 0.5
         EX1 = FLOAT(iex1)/2.0
         EX2 = EX1
      ENDIF
C-----determination of the initial exciton configuration **** done ****
      EXTr = EXCn
      IF(NLW.GT.NDLW - 1)THEN
         WRITE(6, *)'NDLW IS TOO SMALL FOR MSC'
         WRITE(6, '('' INCREASE IT TO '',I3,'' AND RECOMPILE'')')NLW + 1
         WRITE(6, *)'EXECUTION STOPPED'
         STOP
      ENDIF
C-----calculation of factorial
      FACt(1) = 1.
      DO i = 1, 99
         FACt(i + 1) = FACt(i)*FLOAT(i)
      ENDDO
C-----calculation of factorial      ***** done *******
      CSMsc(0) = 0.0
      CSMsc(1) = 0.0
      CSMsc(2) = 0.0
      izares = 1000.0*Z(1) + A(1) - 1.
      CALL WHERE(izares, nresn, iloc)
      IF(iloc.EQ.1)THEN
         WRITE(6, *)
     &'RESIDUAL NUCLEUS AFTER NEUTRON EMISSION FROM CN HAS NOT BEEN DETE
     &RMINED BEFORE CALL OF HMSC'
         WRITE(6, *)'OPEN NEUTRON EMISSION'
         WRITE(6, *)'EXECUTION STOPPED'
         STOP
      ENDIF
      izares = 1000.0*(Z(1) - 1.0) + A(1) - 1.
      CALL WHERE(izares, nresp, iloc)
      IF(iloc.EQ.1)THEN
         WRITE(6, *)
     &'RESIDUAL NUCLEUS AFTER PROTON  EMISSION FROM CN HAS NOT BEEN DETE
     &RMINED BEFORE CALL OF HMSC'
         WRITE(6, *)'OPEN PROTON  EMISSION'
         WRITE(6, *)'EXECUTION STOPPED'
         STOP
      ENDIF
C-----s.p.l.d. for MSC is set to A/GDIV (default g=A/13)
      G = A(1)/GDIv
      IF(STMro.EQ.1.0D0)THEN
         WRITE(6, 
     &'(     '' Microscopic p-h state densities will be calculated with 
     &parameters'')')
         CALL TRATES
         ini = 1000000
         IF(ZEJc(0).EQ.1.0D0)ini = 100
         IF(ZEJc(0).EQ.2.0D0)ini = 1000200
      ENDIF
      Nvwful = .FALSE.
      IF(NOUt.GT.0)WRITE(6, 99001)
99001 FORMAT(1X, //, 1X, 'Heidelberg M.S.C. (ausiliary output)', //)
      IF(SQRT(1.5D0*G*EXCn) + 6D0.LT.2.0D0*NDMSCS + 1.D0)Nvwful = .TRUE.
      IF(Nvwful)WRITE(6, 99002)
99002 FORMAT(1X, 
     &'C.N. decay will be totaly calculated in terms of the Heidelberg M
     &.S.C. theory')
      IF(.NOT.Nvwful)WRITE(6, 99003)
99003 FORMAT(1X, 
     &'M.S.C. calculations of the C.N. decay will be followed by the Hau
     &ser-Feshbach approach')
      gdrw = GDRpar(2, 1)
      egdr = (GDRpar(1, 1)*GDRpar(3, 1) + GDRpar(4, 1)*GDRpar(6, 1))
     &       /(GDRpar(3, 1) + GDRpar(6, 1))
C-----n o t e  an approximation to calculate a single width for the
C-----cammel-like GDR shapes (here 1/2 of the distance between two
C-----peaks is added to the average; this being absolutely arbitrary)
      IF(GDRpar(6, 1).NE.0.0D0)gdrw = (GDRpar(2, 1)*GDRpar(3, 1) + 
     &                                GDRpar(5, 1)*GDRpar(6, 1))
     &                                /(GDRpar(3, 1) + GDRpar(6, 1))
     &                                + 0.5*(GDRpar(4, 1) - GDRpar(1, 1)
     &                                )
      WRITE(6, '('' Spreading/total GDR width ='',G12.5)')D1Fra
      egdrr = EXCn - egdr
C-----
C-----calculation of spin distribution for each exciton number
C-----
      DO iexc = 1, 2*NDMSCS + IV
         DO ispin = 1, NLW + 1
            OMJd(iexc, ispin) = OMJ(iexc, 1, 1, ispin, HIS(1), 1)
         ENDDO
      ENDDO
C-----
C-----calculation of average matrix element w from tlj
C-----
      DO i = 1, NEX(1)
C-----next 4 l. - calculation of accessible state density for absorption
         entl = ETL(i, 1, nresn) + Q(1, 1)
         eptl = ETL(i, 2, nresp) + Q(2, 1)
COBL     OMN=G**3*WOBL(2,1,ENTL,-1)/4.
COBL     OMP=G**3*WOBL(2,1,EPTL,-1)/4.
         omn = WT(3, 2, 1, entl)
         omp = WT(3, 2, 1, eptl)
         DO j = 1, NLW
            tx = TL(i, j, 1, nresn)
            IF(tx.GT.1.0D-7)THEN
               WP(i, j, 1) = (2.0 - tx - 2.0*SQRT(1.0 - tx))/tx/omn
            ELSE
               WP(i, j, 1) = 0.0
            ENDIF
            tx = TL(i, j, 2, nresp)
            IF(tx.GT.1.0D-7)THEN
               WP(i, j, 2) = (2.0 - tx - 2.0*SQRT(1.0 - tx))/tx/omp
            ELSE
               WP(i, j, 2) = 0.0
            ENDIF
         ENDDO
      ENDDO
C-----
C-----determination of the exciton structure of the initial stage
C-----for light ion induced reactions (n, p, d, 3-H, 3-He, and 4-He)
C-----
      vu = TORy/(TORy + 1.)
      vl = 1.0/(TORy + 1.0)
      IF(XNI.NE.0.0D0)THEN
         IE(1) = XNI
      ELSE
         IE(1) = 3
      ENDIF
      xep(1, 1) = EX1
      xep(1, 2) = EX2
      IF(EX1.EQ.0.0D0 .AND. EX2.EQ.0.0D0)THEN
         izpcon = ZEJc(0) + 1.0
         IF(izpcon.EQ.2)THEN
            xep(1, 1) = vu
            IF(AEJc(0).EQ.2.D0)xep(1, 1) = xep(1, 1) + 1.0
            IF(AEJc(0).EQ.3.D0)xep(1, 1) = xep(1, 1) + 2.0
            xep(1, 2) = 1.0 + vl
            IE(1) = (xep(1, 1) + xep(1, 2) + 0.0001) + 1
         ELSEIF(izpcon.EQ.3)THEN
            xep(1, 1) = 2.5
            IF(AEJc(0).EQ.3.D0)xep(1, 1) = xep(1, 1) - 1.0
            xep(1, 2) = 2.5
            IE(1) = (xep(1, 1) + xep(1, 2) + 0.0001) + 1
         ELSE
            xep(1, 1) = 1.0 + vl
            xep(1, 2) = vu
         ENDIF
      ENDIF
      WRITE(6, 99004)xep(1, 1), xep(1, 2), IE(1)
99004 FORMAT(1X, 
     &       'Initial particle type number of excitons for neutrons=', 
     &       F5.2, ' for protons=', F5.2, ' total number of excitons=', 
     &       I2)
      IF(IE(1).GT.IV + 1)THEN
         WRITE(6, 
     &'('' DIMENSIONS INSUFICIENT TO TREAT INITIAL EXCITON'',   '' NUMBE
     &R'',/,'' SET IV IN PARAMETER CARD TO '',I2,'' AND REC''   ,''OMPIL
     &E'')')IE(1) - 1
         STOP
      ENDIF
      ip(1) = xep(1, 1) + xep(1, 2) + 0.00001
      ih(1) = IE(1) - ip(1)
      PREp(1, 1) = xep(1, 1)/ip(1)
      PREp(1, 2) = xep(1, 2)/ip(1)
C-----
C-----determination of exciton structure of the next stages
C-----
      DO i = 2, NC2
         IE(i) = IE(i - 1) + 2.
         ih(i) = ih(i - 1) + 1.
         ip(i) = ip(i - 1) + 1.
         xep(i, 1) = xep(i - 1, 1) + PREp(i - 1, 1)*vl + PREp(i - 1, 2)
     &               *vu
         xep(i, 2) = xep(i - 1, 2) + PREp(i - 1, 1)*vu + PREp(i - 1, 2)
     &               *vl
         PREp(i, 1) = xep(i, 1)/ip(i)
         PREp(i, 2) = xep(i, 2)/ip(i)
      ENDDO
C-----
C-----ereasing ZP and YP matrices
C-----
      nc1 = NDMSCS + 1
      DO i = 1, NDEX
         DO m = 1, 3
            DO k = 1, NDMSCS
               YP(i, k, m, 1) = 0.0
               YP(i, k, m, 2) = 0.0
               ZP(i, k, m, 1) = 0.0
               ZP(i, k, m, 2) = 0.0
            ENDDO
            DO k = nc1, NC2
               ZP(i, k, m, 1) = 0.0
               ZP(i, k, m, 2) = 0.0
            ENDDO
         ENDDO
      ENDDO
C-----
C-----calculation of C.N. state density in each stage
C-----
      popars = 0.0
C-----RBU stands for the average ratio of the matrix elements connecting
C-----initial channel to bound and unbound states
C-----(RBU=m-to-bound/m-to-unbound)
      rbu = 1.
      WRITE(6, 
     &'(     '' Ratio of unbound-bound to unbound-unbound matrix element
     &s'',F6.2)')rbu
      DO k = 1, NDMSCS
C-----Pauli corrected excitation energy
         ecor = EXCn - ((ip(k)**2 + ih(k)**2)/4.0 + (ip(k) - ih(k))
     &          /4.0 - ih(k)/2.0)/G
C--------try to use Oblozinsky expression
COBL     AW(K)=WOBL(IP(K),IH(K),ECOR,-1)
         aw(k) = W(ip(k), ih(k), ecor)
         IF(STMro.EQ.0.D0)THEN
            awm(k) = WT(IE(k), ip(k), ih(k), ecor)
         ELSE
            ncont = 10000 + ip(k)*100 + ih(k)
            awm(k) = ROPHM(ncont, ini, EXCn, 1.D0)
         ENDIF
C--------calculation of the class population from the initial channel
         IF(aw(k).GT.0.0D0)THEN
            popart(k) = rbu - 1 + WILLI(IE(k), ecor)
     &                  /W(ip(k), ih(k), ecor)
            popart(k) = (1.0 - popars)*rbu/popart(k)
         ENDIF
C--------correction to remove gradual absorption
C        IF(K.EQ.1) POPART(K)=1.
C--------------------------------------------------
         popars = popars + popart(k)
         DO j = 1, NLW
            awj(k, j) = awm(k)*OMJd(IE(k), j)
         ENDDO
      ENDDO
C-----state density printout
      WRITE(6, 
     &'('' Relative population of subsequent MSC classes from the open s
     &pace:'',/)')
      WRITE(6, 99021)popart
      IF(NOUt.GT.1)THEN
         WRITE(6, 
     &'('' State density in C.N. as a function of spin and class number'
     &',/)')
         DO k = 1, NDMSCS
            WRITE(6, 99021)(awj(k, j), j = 1, 12)
         ENDDO
      ENDIF
C-----
C-----calculation of average matrix element vmn from o.m.p.
C-----(should be divided by 2*pi*g**3 but omitted as it cancels later)
C-----
      CALL GDOWN(y, 2, 1, EXCn)
      vmn = 2.0*IE(1)*VQ(2, 1, EXCn)*W(2, 1, EXCn)/y
C-----
C-----calculation of yp, zp and zsum
C-----
      DO i = 1, NEX(nresn)
         ew = EX(i, nresn)
         en = EMAx(nresn) - ew
COBL     ZP(I,1,1,1)=WOBL(2,1,EN+Q(1,1),-1)*G**3/4
         ZP(i, 1, 1, 1) = W(2, 1, en + Q(2, 1))*G**3/2
         YP(i, 1, 1, 1) = 0.0
         DO k = 1, NDMSCS
C-----------Pauli corrections
            ecor = ((ip(k)**2 + ih(k)**2)/4.0 + (ip(k) - ih(k))
     &             /4.0 - ih(k)/2.0)/G
            ecor1 = (((ip(k)-2)**2 + (ih(k)-1)**2)
     &              /4.0 + (ip(k) - ih(k) - 1)/4.0 - (ih(k) - 1)/2.0)/G
            ecor2 = (((ip(k)-1)**2 + ih(k)**2)/4.0 + (ip(k) - ih(k) - 1)
     &              /4.0 - ih(k)/2.0)/G
            ecor3 = ((ip(k)**2 + (ih(k)+1)**2)/4.0 + (ip(k) - ih(k) + 1)
     &              /4.0 - (ih(k) + 1)/2.0)/G
C----------------------
            ZP(i, k, 1, 1) = ZP(i, 1, 1, 1)
            IF(ew - ecor2.GT.0.0D0)THEN
               CALL ZERO(yre, ip(k), ih(k), ew - ecor)
               IF(yre.NE.0D0)ZP(i, k, 2, 1)
     &            = yre*G*G/W(ip(k) - 1, ih(k), ew - ecor2)/ip(k)
            ENDIF
            ZP(i, k, 3, 1) = G*(ih(k) + 1)*ih(k)/2.
C-----------
C-----------final state number per bin for neutron emission
C-----------
            IF(STMro.EQ.0.D0)THEN
COBL           YP(I,K,1,1)=DE*WOBL(IP(K)-2,IH(K)-1,EW-ECOR1,-1)/2.
COBL           YP(I,K,2,1)=DE*WOBL(IP(K)-1,IH(K),EW-ECOR2,-1)/2.
COBL           YP(I,K,3,1)=DE*WOBL(IP(K),IH(K)+1,EW-ECOR3,-1)/2.
               YP(i, k, 1, 1) = DE*WT(IE(k) - 3, ip(k) - 2, ih(k) - 1, 
     &                          ew - ecor1)
               YP(i, k, 2, 1) = DE*WT(IE(k) - 1, ip(k) - 1, ih(k), 
     &                          ew - ecor2)
               YP(i, k, 3, 1) = DE*WT(IE(k) + 1, ip(k), ih(k) + 1, 
     &                          ew - ecor3)
            ELSE
               IF(IE(k).GT.3 .AND. ip(k).GE.2 .AND. ih(k).GE.1)THEN
                  ncont = 20000 + (ip(k) - 2)*100 + ih(k) - 1
                  YP(i, k, 1, 1) = ROPHM(ncont, ini, ew, DE)
               ELSE
                  YP(i, k, 1, 1) = 0.0
               ENDIF
               IF(IE(k).GT.1 .AND. ip(k).GE.1)THEN
                  ncont = 20000 + (ip(k) - 1)*100 + ih(k)
                  YP(i, k, 2, 1) = ROPHM(ncont, ini, ew, DE)
               ELSE
                  YP(i, k, 2, 1) = 0.0
               ENDIF
               ncont = 20000 + ip(k)*100 + ih(k) + 1
               YP(i, k, 3, 1) = ROPHM(ncont, ini, ew, DE)
            ENDIF
         ENDDO
         ZP(i, nc1, 1, 1) = ZP(i, 1, 1, 1)
         ZP(i, NC2, 1, 1) = ZP(i, 1, 1, 1)
C--------Pauli corrections
         ecor1 = (((ip(k)+1)**2 + (ih(k)+1)**2)/4.0 + (ip(k) - ih(k))
     &           /4.0 - (ih(k) + 1)/2.0)/G
         ecor2 = ((ip(k)**2 + (ih(k)+1)**2)/4.0 + (ip(k) - ih(k) + 1)
     &           /4.0 - (ih(k) + 1)/2.0)/G
C----------------------
         IF(ew - ecor2.GT.0.0D0)THEN
            CALL ZERO(yre, ip(NDMSCS) + 1, ih(NDMSCS) + 1, ew - ecor1)
            IF(yre.NE.0.D0)ZP(i, nc1, 2, 1)
     &         = yre*G*G/W(ip(NDMSCS), ih(NDMSCS) + 1, ew - ecor2)
     &         /(ip(NDMSCS) + 1)
         ENDIF
      ENDDO
      DO i = 1, NEX(nresn)
         DO k = 3, NC2
            ZSUm(i, k, 1) = ZP(i, k, 1, 1) + ZP(i, k - 1, 2, 1)
     &                      + ZP(i, k - 2, 3, 1)
         ENDDO
         ZSUm(i, 2, 1) = ZP(i, 2, 1, 1) + ZP(i, 1, 2, 1)
         ZSUm(i, 1, 1) = ZP(i, 1, 1, 1)
      ENDDO
C-----
C-----calculation of YP, ZP and ZSUM
C-----
      DO i = 1, NEX(nresp)
         ew = EX(i, nresp)
         en = EMAx(nresp) - ew
COBL     ZP(I,1,1,2)=WOBL(2,1,EN+Q(1,1),-1)*G**3/4.
         ZP(i, 1, 1, 2) = W(2, 1, en + Q(2, 1))*G**3/2.
         YP(i, 1, 1, 2) = 0.0
         DO k = 1, NDMSCS
C-----------Pauli corrections
            ecor = ((ip(k)**2 + ih(k)**2)/4.0 + (ip(k) - ih(k))
     &             /4.0 - ih(k)/2.0)/G
            ecor1 = (((ip(k)-2)**2 + (ih(k)-1)**2)
     &              /4.0 + (ip(k) - ih(k) - 1)/4.0 - (ih(k) - 1)/2.0)/G
            ecor2 = (((ip(k)-1)**2 + ih(k)**2)/4.0 + (ip(k) - ih(k) - 1)
     &              /4.0 - ih(k)/2.0)/G
            ecor3 = ((ip(k)**2 + (ih(k)+1)**2)/4.0 + (ip(k) - ih(k) + 1)
     &              /4.0 - (ih(k) + 1)/2.0)/G
C----------------------
            ZP(i, k, 1, 2) = ZP(i, 1, 1, 2)
            IF(ew - ecor2.GT.0.0D0)THEN
               CALL ZERO(yre, ip(k), ih(k), ew - ecor)
               IF(yre.NE.0.D0)ZP(i, k, 2, 2)
     &            = yre*G*G/W(ip(k) - 1, ih(k), ew - ecor2)/ip(k)
            ELSE
               ZP(i, k, 2, 2) = 0.0
            ENDIF
            ZP(i, k, 3, 2) = G*(ih(k) + 1)*ih(k)/2.
C-----------
C-----------final state number per bin for proton emission
C-----------
            IF(STMro.EQ.0.D0)THEN
COBL           YP(I,K,1,2)=DE*WOBL(IP(K)-2,IH(K)-1,EW-ECOR1,-1)/2.
COBL           YP(I,K,2,2)=DE*WOBL(IP(K)-1,IH(K),EW-ECOR2,-1)/2.
COBL           YP(I,K,3,2)=DE*WOBL(IP(K),IH(K)+1,EW-ECOR3,-1)/2.
               YP(i, k, 1, 2) = DE*WT(IE(k) - 3, ip(k) - 2, ih(k) - 1, 
     &                          ew - ecor1)
               YP(i, k, 2, 2) = DE*WT(IE(k) - 1, ip(k) - 1, ih(k), 
     &                          ew - ecor2)
               YP(i, k, 3, 2) = DE*WT(IE(k) + 1, ip(k), ih(k) + 1, 
     &                          ew - ecor3)
            ELSE
               IF(IE(k).GT.3 .AND. ip(k).GE.2 .AND. ih(k).GE.1)THEN
                  ncont = 30000 + (ip(k) - 2)*100 + ih(k) - 1
                  YP(i, k, 1, 2) = ROPHM(ncont, ini, ew, DE)
               ELSE
                  YP(i, k, 1, 2) = 0.0
               ENDIF
               IF(IE(k).GT.1 .AND. ip(k).GE.1)THEN
                  ncont = 30000 + (ip(k) - 1)*100 + ih(k)
                  YP(i, k, 2, 2) = ROPHM(ncont, ini, ew, DE)
               ELSE
                  YP(i, k, 2, 2) = 0.0
               ENDIF
               ncont = 30000 + ip(k)*100 + ih(k) + 1
               YP(i, k, 3, 2) = ROPHM(ncont, ini, ew, DE)
            ENDIF
         ENDDO
         ZP(i, nc1, 1, 2) = ZP(i, 1, 1, 2)
         ZP(i, NC2, 1, 2) = ZP(i, 1, 1, 2)
C--------Pauli corrections
         ecor1 = (((ip(k)+1)**2 + (ih(k)+1)**2)/4.0 + (ip(k) - ih(k))
     &           /4.0 - (ih(k) + 1)/2.0)/G
         ecor2 = ((ip(k)**2 + (ih(k)+1)**2)/4.0 + (ip(k) - ih(k) + 1)
     &           /4.0 - (ih(k) + 1)/2.0)/G
C----------------------
         IF(ew - ecor2.GT.0.0D0)THEN
            CALL ZERO(yre, ip(NDMSCS) + 1, ih(NDMSCS) + 1, ew - ecor1)
            ZP(i, nc1, 2, 2) = yre*G*G/W(ip(NDMSCS), ih(NDMSCS) + 1, 
     &                         ew - ecor2)/(ip(NDMSCS) + 1)
         ELSE
            ZP(i, nc1, 2, 2) = 0.0
         ENDIF
      ENDDO
      DO i = 1, NEX(nresp)
         DO k = 3, NC2
            ZSUm(i, k, 2) = ZP(i, k, 1, 2) + ZP(i, k - 1, 2, 2)
     &                      + ZP(i, k - 2, 3, 2)
         ENDDO
         ZSUm(i, 2, 2) = ZP(i, 2, 1, 2) + ZP(i, 1, 2, 2)
         ZSUm(i, 1, 2) = ZP(i, 1, 1, 2)
      ENDDO
C-----
C-----printout of final state densities and accessible state densities
C-----for proton absorption (used for the determination of Tl splitting)
C-----
      IF(NOUt.GT.1)THEN
         DO k = 1, NDMSCS
            WRITE(6, 99005)k, ip(k), ih(k), IE(k), EXCn, awm(k)
99005       FORMAT(1X, //, '  STEP K=', I3, ' P=', I2, ' H=', I2, ' N=', 
     &             I2, ' EXCN=', F8.3, ' AWM=', G11.3, //, 4X, 
     &'Y(.,K,1)  Y(.,K,2)   Y(.,K,3)  Z(.,K,1)   Z(.,K,2)   Z(.,K,3)   Z
     &SUM(.,K)', //)
            WRITE(6, 99006)(i, YP(i, k, 1, 1), YP(i, k, 2, 1), 
     &                     YP(i, k, 3, 1), ZP(i, k, 1, 1), 
     &                     ZP(i, k, 2, 1), ZP(i, k, 3, 1), ZSUm(i, k, 1)
     &                     , i = 1, 30)
99006       FORMAT(I3, 7G11.3)
         ENDDO
      ENDIF
      DO i = 1, 2
         ipii = -( - 1.0)**i
         ie1 = 2
         IF(i.EQ.2)ie1 = 1
         DO j = 1, NLW, LTUrbo
            IF(POP(NEX(1), j, i, 1).NE.0.0D0)THEN
               emis = 0.0
               DO ix = 1, NDMSCS
                  DO jx = 1, NDMSCS
                     PIM(ix, jx) = 0.0
                  ENDDO
               ENDDO
C
C********************** neutron emission *******************************
C
               CALL DECHMS(j, ipii, nresn, 1)
C
C***************** proton emission *************************************
C
               CALL DECHMS(j, ipii, nresp, 2)
C--------------memorize emission to be used in gamma part
               DO k1 = 1, NDMSCS
                  pimem(k1) = PIM(k1, k1)
               ENDDO
C--------------
C--------------do loops over stages
C--------------calculates intranuclear transitions (V dependent) incl. nondiagonal
C--------------
               ncm = NDMSCS - 1
               DO k1 = 1, ncm
                  rop = awj(k1, j)
C-----------------12.56636 stands for 2*2*pi (one 2 comes from Gamma=2*W)
C-----------------next line calculates interstage rate directly from imag.
C-----------------o.m. potential
C                 PIM(K1,K1+1)=-12.56636*ROP*VQ(IP(K1),IH(K1),EXCN)*IE(K1)
C-----------------
C-----------------next 4 lines calculate interstage rate VMN calculated from o.m.
C-----------------ang. mom. coeff. being neglected as they nearly cancel out
                  ecor = ((ip(k1)**2 + ih(k1)**2)
     &                   /4.0 + (ip(k1) - ih(k1))/4.0 - ih(k1)/2.0)/G
                  CALL GDOWN(y, ip(k1), ih(k1), EXCn - ecor)
C-----------------YD should be multiplied by G**3 but omitted since omitted in VMN
                  yd = y/aw(k1)
                  PIM(k1, k1 + 1) = -3.14159*2.*rop*vmn*yd
C
C-----------------next 4 lines calculate interstage rate using parameter M**2=0.00005
C ECOR=((IP(K1)**2+IH(K1)**2)/4.0+(IP(K1)-IH(K1))/4.0-IH(K1)/2.0)/G
C                 CALL GDOWN(Y,IP(K1),IH(K1),$)
C                 YD=Y*G**3/AW(K1)/2.
C                 PIM(K1,K1+1)=-39.47835*0.00005*ROP*XD(J,K1)*YD
C
                  PIM(k1 + 1, k1) = PIM(k1, k1 + 1)
                  PIM(k1, k1) = PIM(k1, k1) - PIM(k1, k1 + 1)
                  IF(k1.GT.1)PIM(k1, k1) = PIM(k1, k1) - PIM(k1, k1 - 1)
               ENDDO
               PIM(NDMSCS, NDMSCS) = PIM(NDMSCS, NDMSCS)
     &                               - PIM(NDMSCS, NDMSCS - 1)
C--------------including of the coupling to the next class out of the matrix
C--------------in the last diagonal element of PIM (if .NOT.NVWFUL)
               IF(.NOT.Nvwful)THEN
                  rop = awj(NDMSCS, j)
C-----------------next line calculates interstage rate from imag. o.m. potential
C                 PIM(NDMSCS,NDMSCS)=PIM(NDMSCS,NDMSCS)+ROP*
C                 1
C                 VQ(IP(NDMSCS),IH(NDMSCS),EXCN)*12.56636*IE(NDMSCS)
C-----------------next 4 lines calculate interstage rate VMN calculated from o.m.
C-----------------ang. mom. coeff. being neglected as they nearly cancel out
                  ecor = ((ip(NDMSCS)**2 + ih(NDMSCS)**2)
     &                   /4.0 + (ip(NDMSCS) - ih(NDMSCS))
     &                   /4.0 - ih(NDMSCS)/2.0)/G
                  CALL GDOWN(y, ip(NDMSCS), ih(NDMSCS), EXCn - ecor)
C-----------------YD should be multiplied by G**3 but omitted since omitted in VMN
                  yd = y/aw(NDMSCS)
                  cgd = 3.14159*2.*vmn*yd
                  PIM(NDMSCS, NDMSCS) = PIM(NDMSCS, NDMSCS)
     &                                  + 3.14159*2*rop*vmn*yd
C
C-----------------next 4 lines calculate interstage rate using parameter
C-----------------|M**2|=0.00005
C                 ECOR = ((IP(NDMSCS)**2+IH(NDMSCS)**2)/4.0+(IP(NDMSCS)
C                 1               -IH(NDMSCS))*/4.0-IH(NDMSCS)/2.0)/G
C                 CALL GDOWN(Y,IP(NDMSCS),IH(NDMSCS),EXCN-ECOR)
C                 YD = Y*G**3/AW(NDMSCS)/2.
C                 PIM(NDMSCS,NDMSCS) = PIM(NDMSCS,NDMSCS) + 39.47835
C                 1               *0.00005*ROP*XD(J,NDMSCS)*YD
C
               ENDIF
C--------------printout of the pi matrix
               IF(NOUt.GT.2)THEN
                  WRITE(6, 99007)j, i
99007             FORMAT(1X, 'PIM MATRIX   J=', I2, ' I=', I2)
                  DO m2 = 1, NDMSCS
                     WRITE(6, 99021)(PIM(m1, m2), m1 = 1, NDMSCS)
                  ENDDO
                  WRITE(6, 99022)
               ENDIF
C--------------memorize PI matrix
               DO k1 = 1, NDMSCS
                  DO k2 = 1, NDMSCS
                     apim(k1, k2) = PIM(k1, k2)
                  ENDDO
               ENDDO
C--------------
C--------------invertion of the PIM matrix
C--------------
               DO inc = 1, NDMSCS
                  bbb(inc) = 0.0
               ENDDO
               CALL MATIN(PIM, bbb, NDMSCS, 1, det)
               IF(NOUt.GT.2)THEN
                  WRITE(6, 99008)j, i
99008             FORMAT(1X, 'Inverse PIM matrix   J=', I2, ' I=', I2)
                  DO m2 = 1, NDMSCS
                     WRITE(6, 99021)(PIM(m1, m2), m1 = 1, NDMSCS)
                  ENDDO
                  WRITE(6, 99022)
               ENDIF
C--------------
C--------------check of the invertion and printout
C--------------
               DO k1 = 1, NDMSCS
                  DO k2 = 1, NDMSCS
                     undex(k1, k2) = 0.
                     DO k3 = 1, NDMSCS
                        undex(k1, k2) = undex(k1, k2) + apim(k3, k1)
     &                                  *PIM(k3, k2)
                     ENDDO
                  ENDDO
               ENDDO
               IF(NOUt.GT.2)THEN
                  WRITE(6, 99009)j, i
99009             FORMAT(1X, 'Control matrix   J=', I2, ' I=', I2)
                  DO m2 = 1, NDMSCS
                     WRITE(6, 99021)(undex(m1, m2), m1 = 1, NDMSCS)
                  ENDDO
                  WRITE(6, 99022)
               ENDIF
C--------------
C--------------normalization of the inverted PIM matrix with the ini. population
C--------------
               DO k1 = 1, NDMSCS
                  spg(k1) = 0.0
                  DO k2 = 1, NDMSCS
                     PIM(k1, k2) = PIM(k1, k2)*POP(NEX(1), j, i, 1)
     &                             *popart(k2)
                  ENDDO
               ENDDO
               IF(NOUt.GT.1)WRITE(6, 99010)POP(NEX(1), j, i, 1)
99010          FORMAT(1X, 'Population to be distributed=', E12.5, ' mb', 
     &                /)
               IF(NOUt.GT.3)WRITE(6, 99011)PIM1
99011          FORMAT(1X, 'PIM TO BE USED (NORMALIZED):', /, 1X, 
     &                (12E11.4), /)
C
C************  normalization of emission rates **************************
C
C--------------neutron emission (if accepted, note IF on IDNA)
               IF(IDNa(2, 3).GT.0)THEN
                  nejc = 1
                  nnur = nresn
                  DO ke = 1, NEX(nnur)
                     icse = NEX(nnur) - ke + 1
                     DO kg = 1, NDMSCS
                        DO ki = 1, NDMSCS
                           DO jr = 1, NLW, LTUrbo
                              pop1 = SCRm(ke, jr, 1, nejc, kg)
     &                               *PIM(kg, ki)/DE
                              pop2 = SCRm(ke, jr, 2, nejc, kg)
     &                               *PIM(kg, ki)/DE
                              popt = pop1 + pop2
                              IF(icse.GT.1)THEN
                                 CSE(icse, nejc, 1) = CSE(icse, nejc, 1)
     &                              + popt
                                 AUSpec(icse, nejc) = AUSpec(icse, nejc)
     &                              + popt
                                 POPcse(ke,nejc,icse,nnur) =  
     &                             POPcse(ke,nejc,icse,nnur) 
     &                              + popt

                              ENDIF
                              POP(ke, jr, 1, nnur)
     &                           = POP(ke, jr, 1, nnur) + pop1
                              POP(ke, jr, 2, nnur)
     &                           = POP(ke, jr, 2, nnur) + pop2
                              emis = emis + popt*DE
                              CSMsc(nejc) = CSMsc(nejc) + popt*DE
                              spg(kg) = spg(kg) + popt*DE
                           ENDDO
                        ENDDO
                     ENDDO
                  ENDDO
                  IF(nejc.EQ.1)THEN
                     nnur = nresn
                  ELSE
                     nnur = nresp
                  ENDIF
               ENDIF
C--------------proton emission (if accepted, note IF on IDNA)
               IF(IDNa(4, 3).GT.0)THEN
                  nejc = 2
                  nnur = nresp
                  DO ke = 1, NEX(nnur)
                     icse = NEX(nnur) - ke + 1
                     DO kg = 1, NDMSCS
                        DO ki = 1, NDMSCS
                           DO jr = 1, NLW, LTUrbo
                              pop1 = SCRm(ke, jr, 1, nejc, kg)
     &                               *PIM(kg, ki)/DE
                              pop2 = SCRm(ke, jr, 2, nejc, kg)
     &                               *PIM(kg, ki)/DE
                              popt = pop1 + pop2
                              IF(icse.GT.1)THEN
                                 CSE(icse, nejc, 1) = CSE(icse, nejc, 1)
     &                              + popt
                                 AUSpec(icse, nejc) = AUSpec(icse, nejc)
     &                              + popt
                                 POPcse(ke,nejc,icse,nnur) =  
     &                             POPcse(ke,nejc,icse,nnur) 
     &                              + popt
                              ENDIF
                              POP(ke, jr, 1, nnur)
     &                           = POP(ke, jr, 1, nnur) + pop1
                              POP(ke, jr, 2, nnur)
     &                           = POP(ke, jr, 2, nnur) + pop2
                              emis = emis + popt*DE
                              CSMsc(nejc) = CSMsc(nejc) + popt*DE
                              spg(kg) = spg(kg) + popt*DE
                           ENDDO
                        ENDDO
                     ENDDO
                  ENDDO
               ENDIF
               IF(NEX(nresn).GT.0)POPmax(nresn) = CSMsc(1)/NEX(nresn)
               IF(NEX(nresp).GT.0)POPmax(nresp) = CSMsc(2)/NEX(nresp)
               IF(NOUt.GT.0)WRITE(6, 99012)j, i, spg
99012          FORMAT(1X, 'J=', I2, ' I=', I1, 
     &                '  emission from subsequent stages:', /, 
     &                (1X, 12E11.4), //)
               IF(NOUt.GT.0)WRITE(6, 99022)
C
C************* Gamma emission  ******************************************
C
               IF(GST.NE.0D0 .AND. IDNa(5, 3).GT.0)THEN
                  IF(egdrr.GT.0.0D0)THEN
C-----------------ereasing PIG matrix
                     DO ix = 1, NG
                        DO jx = 1, NG
                           pig(ix, jx) = 0.0
                        ENDDO
                     ENDDO
C--------------------calculate nuclear temperature for the generalized Lorenzian to be
C--------------------used for E1 strength function determination. NOTE: this temperature
C--------------------is not consistent with the actual level densities.
                     t = 0.
                     iec = NEX(1)
                     atil = 0.073*A(1) + 0.115*A(1)**0.666667
                     gamma = 0.40/A(1)**0.33333
                     accn = atil*(1 + SHC(1)
     &                      *(1 - EXP((-gamma*EX(iec,1))))/EX(iec, 1))
                     IF(EX(iec, 1).GE.YRAst(j, 1))
     &                  t = SQRT((EX(iec,1) - YRAst(j,1))/accn)
C
C--------------------calculation of state densities in all substages of c.n.
C
                     DO kg = 1, NDMSCS
C                       DO KG = 1, NC2
                        ropht(kg) = awj(kg, j)
                        IF(STMro.EQ.0.D0)THEN
                           ecor = (((ip(kg)-1)**2 + (ih(kg)-1)**2)
     &                            /4.0 + (ip(kg) - ih(kg))
     &                            /4.0 - (ih(kg) - 1)/2.0)/G
                           rophc(kg) = WT(IE(kg) - 2, ip(kg) - 1, ih(kg)
     &                                 - 1, egdrr - ecor)
                        ELSE
                           ncont = 10000 + (ip(kg) - 1)*100 + ih(kg) - 1
                           rophc(kg) = ROPHM(ncont, ini, egdrr, 1.D0)
                        ENDIF
                        roph(kg, 1) = 0.0
                        IF(j.GT.1)roph(kg, 1) = rophc(kg)
     &                     *OMJd(IE(kg) - 2, j - 1)
                        roph(kg, 2) = rophc(kg)*OMJd(IE(kg) - 2, j)
                        roph(kg, 3) = rophc(kg)*OMJd(IE(kg) - 2, j + 1)
                        roph3(kg) = roph(kg, 1) + roph(kg, 2)
     &                              + roph(kg, 3)
                        roph(kg, 4) = ropht(kg) - roph3(kg)
                     ENDDO
C--------------------
C--------------------construction of the PIG matrix (contains no gamma contribution !)
C--------------------
C--------------------particle emission in diagonal elements
                     npigzero = 0
                     IF(ISD.EQ.1)pig(1, 1) = pimem(1)
                     IF(pig(1, 1).EQ.0.0D0)npigzero = 1
                     kgin = 1
                     IF(ISD.EQ.1)kgin = 2
                     DO kg = kgin, NDMSCS
                        DO kc = 1, 4
                           ikc = (kg - kgin)*4 + kc + ISD
                           pig(ikc, ikc) = pimem(kg)*roph(kg, kc)
     &                        /ropht(kg)
                           IF(pig(ikc, ikc).EQ.0.0D0)
     &                        npigzero = npigzero + 1
                        ENDDO
                     ENDDO
C--------------------skip gamma emission if there are more than one 0's on the PIG diagonal
                     IF(npigzero.LE.1)THEN
C-----------------------
C-----------------------offdiagonal elements
C-----------------------
                        IF(ISD.NE.0)THEN
C-------------------------(1,4)->(2,1-3) creation of GDR
                           ecor = ((ip(1)**2 + ih(1)**2)
     &                            /4.0 + (ip(1) - ih(1))/4.0 - ih(1)
     &                            /2.0)/G
C--------------------------probability for one hole to have enough energy to create GDR
                           xxxx = WOBL(ip(1), ih(1) - 1, EXCn - ecor, 0)
                           ygdr = WOBL(ip(1), ih(1) - 1, egdrr - ecor, 
     &                            0)/xxxx
C--------------------------using full spreading width of the GDR
                           ccre = -6.28319*roph(1, 4)
     &                            *ygdr*gdrw*D1Fra/roph3(2)
C--------------------------using spreading width of the GDR split among three classes
C                          CCRE=-6.28319*ROPH(1,4)*YGDR*GDRW*D1FRA*ROPHT(1)
C                          CCRE=CCRE/(ROPHT(1)+ROPHT(2)+ROPHT(3))/ROPH3(2)
C--------------------------using single particl matrix element with analogy to
C                          (1,4)-->(2,4) CALL GDOWN(Y,IP(1),IH(1),EXCN-ECOR)
C                          YSP=Y/AW(1)
C                          CCRE=-6.28319*ROPH(1,4)*YGDR*VMN*YSP/ROPH3(2)
C--------------------------
                           pig(1, 2) = ccre*roph(2, 1)
                           pig(2, 1) = pig(1, 2)
                           pig(1, 3) = ccre*roph(2, 2)
                           pig(3, 1) = pig(1, 3)
                           pig(1, 4) = ccre*roph(2, 3)
                           pig(4, 1) = pig(1, 4)
C--------------------------(1,4)->(2,4) use s.p. matrix element
                           CALL GDOWN(y, ip(1), ih(1), EXCn - ecor)
                           ysp = y/aw(1)
                           cgd = -6.28319*vmn*ysp
                           pig(1, 5) = cgd*roph(1, 4)*roph(2, 4)
     &                                 /ropht(2)
                           pig(5, 1) = pig(1, 5)
C--------------------------summing the matrix elements on the right
                           pig(1, 1) = pig(1, 1) - pig(1, 2) - pig(1, 3)
     &                                 - pig(1, 4) - pig(1, 5)
                           pig(2, 2) = pig(2, 2) - pig(2, 1)
                           pig(3, 3) = pig(3, 3) - pig(3, 1)
                           pig(4, 4) = pig(4, 4) - pig(4, 1)
                           pig(5, 5) = pig(5, 5) - pig(5, 1)
                        ENDIF
                        DO kg = kgin, ncm
C--------------------------next  4 lines calculate interstage rate VMN calculated
C--------------------------ang.  from o.m. mom. coeff. being neglected as they nearly
C--------------------------(m,4)->(m+1,4) use s.p. matrix element
C--------------------------cancel out
                           ecor = ((ip(kg)**2 + ih(kg)**2)
     &                            /4.0 + (ip(kg) - ih(kg))/4.0 - ih(kg)
     &                            /2.0)/G
                           CALL GDOWN(y, ip(kg), ih(kg), EXCn - ecor)
C--------------------------Y should be multiplied by G**3 but omitted since
C--------------------------omitted in VMN
                           ysp = y/aw(kg)
                           cgd = -3.14159*2.*vmn*ysp
                           ix = (kg - kgin + 1)*4 + ISD
                           jx = (kg - kgin + 2)*4 + ISD
                           pig(ix, jx) = cgd*roph(kg, 4)
                           pig(ix, ix) = pig(ix, ix) - pig(ix, jx)
                           pig(jx, ix) = pig(ix, jx)
                           pig(jx, jx) = pig(jx, jx) - pig(jx, ix)
                           ygdr = WOBL(ip(kg), ih(kg) - 1, egdrr - ecor, 
     &                            0)
     &                            /WOBL(ip(kg), ih(kg) - 1, EXCn - ecor, 
     &                            0)
                           ecor = (((ip(kg)-1)**2 + (ih(kg)-1)**2)
     &                            /4.0 + (ip(kg) - ih(kg))
     &                            /4.0 - (ih(kg) - 1)/2.0)/G
                           CALL GDOWN(y, ip(kg) - 1, ih(kg) - 1, 
     &                                egdrr - ecor)
C--------------------------Y should be multiplied by G**3 but omitted since
C--------------------------omitted in VMN
                           IF(kg.GT.1)THEN
                              ysp = y/aw(kg - 1)
                           ELSE
C                             e = egdrr -
C                             &
C                             (((ip(kg)-1)**2+(ih(kg)-1)**2)/4.0+ &
C                             (ip(kg)-ih(kg)-2)/4.0-(ih(kg)-1)/2.0)/G ysp =
C-----------------------------set 1p-0h level density to G to avoid 0 at energies
C-----------------------------above neutron binding (it would be more physical
C-----------------------------to y/W(ip(kg)-1,ih(kg)-1,e) ignore MSC gamma
C-----------------------------is emission in such cases but it small anyway)
                              ysp = y/G
                           ENDIF
                           cgd = -6.28319*vmn*ysp
                           DO kc1 = 1, 3
C-----------------------------
C-----------------------------(m,4)->(m+1,1-3) creation of GDR
C-----------------------------
C-----------------------------using full spreading width of the GDR
                              ccre = -6.28319*roph(kg, 4)
     &                               *ygdr*gdrw*D1Fra/roph3(kg + 1)
C-----------------------------using spreading width of the GDR split among three
C-----------------------------classes
C                             CCRE=-6.28319*ROPH(KG,4)*YGDR*GDRW*D1FRA*ROPHT 1
C                             (KG)
C                             CCRE=CCRE/(ROPHT(KG)+ROPHT(KG+1)+ROPHT(KG+2))/ 1
C-----------------------------using ROPH3(KG+1) single particl matrix element with
C-----------------------------analogy to (1,4)-->(2,4) CALL
C                             GDOWN(Y,IP(KG),IH(KG),EXCN-ECOR) YSP=Y/AW(KG)
C                             CCRE=-6.28319*ROPH(KG,4)*YGDR*VMN*YSP/ROPH3
C                             1                     (KG+1)
C-----
                              pig(ix, ix + kc1) = ccre*roph(kg + 1, kc1)
                              pig(ix, ix) = pig(ix, ix)
     &                           - pig(ix, ix + kc1)
                              pig(ix + kc1, ix) = pig(ix, ix + kc1)
                              pig(ix + kc1, ix + kc1)
     &                           = pig(ix + kc1, ix + kc1)
     &                           - pig(ix, ix + kc1)
C-----------------------------
C-----------------------------(m,1-3)->(m+1,4) use GDR spreading width, GDR distruction
C-----------------------------
                              pig(ix + kc1 - 4, jx)
     &                           = -6.28319*roph(kg, kc1)*gdrw*D1Fra
                              pig(ix + kc1 - 4, ix + kc1 - 4)
     &                           = pig(ix + kc1 - 4, ix + kc1 - 4)
     &                           - pig(ix + kc1 - 4, jx)
                              pig(jx, ix + kc1 - 4)
     &                           = pig(ix + kc1 - 4, jx)
                              pig(jx, jx) = pig(jx, jx)
     &                           - pig(jx, ix + kc1 - 4)
C----------------------------
C----------------------------(m,1-3)->(m+1,1-3) use s.p. matrix element and accessible
C----------------------------state densities for the m-1 class, diagonal in
C----------------------------off-diagonal block
                              pig(ix + kc1 - 4, jx + kc1 - 4)
     &                           = cgd*roph(kg, kc1)
                              pig(ix + kc1 - 4, ix + kc1 - 4)
     &                           = pig(ix + kc1 - 4, ix + kc1 - 4)
     &                           - pig(ix + kc1 - 4, jx + kc1 - 4)
                              pig(jx + kc1 - 4, ix + kc1 - 4)
     &                           = pig(ix + kc1 - 4, jx + kc1 - 4)
                              pig(jx + kc1 - 4, jx + kc1 - 4)
     &                           = pig(jx + kc1 - 4, jx + kc1 - 4)
     &                           - pig(jx + kc1 - 4, ix + kc1 - 4)
                           ENDDO
                        ENDDO
C-----------------------
C-----------------------include coupling to the nc+1 class in PIG (when .not.nvwful)
C-----------------------
                        IF(.NOT.Nvwful)THEN
                           ecor = ((ip(NDMSCS)**2 + ih(NDMSCS)**2)
     &                            /4.0 + (ip(NDMSCS) - ih(NDMSCS))
     &                            /4.0 - ih(NDMSCS)/2.0)/G
                           CALL GDOWN(y, ip(NDMSCS), ih(NDMSCS), 
     &                                EXCn - ecor)
C--------------------------Yd should be multiplied by G**3 but omitted since
C--------------------------omitted in VMN
                           ysp = y/aw(NDMSCS)
                           cgd = -3.14159*2.*vmn*ysp
                           ix = (NDMSCS - kgin + 1)*4 + ISD
                           jx = (NDMSCS - kgin + 2)*4 + ISD
                           pig(ix, ix) = pig(ix, ix)
     &                        - cgd*roph(NDMSCS, 4)
                           ygdr = WOBL(ip(NDMSCS), ih(NDMSCS) - 1, 
     &                            egdrr - ecor, 0)
     &                            /WOBL(ip(NDMSCS), ih(NDMSCS) - 1, 
     &                            EXCn - ecor, 0)
                           ecor = ((ip(NDMSCS-1)**2 + ih(NDMSCS-1)**2)
     &                            /4.0 + (ip(NDMSCS-1) - ih(NDMSCS-1))
     &                            /4.0 - ih(NDMSCS - 1)/2.0)/G
                           CALL GDOWN(y, ip(NDMSCS) - 1, ih(NDMSCS) - 1, 
     &                                egdrr - ecor)
C--------------------------Yd should be multiplied by G**3 but omitted since
C--------------------------omitted in VMN
                           ysp = y/aw(NDMSCS - 1)
                           cgd = -6.28319*vmn*ysp
                           DO kc1 = 1, 3
                              pig(ix, ix) = pig(ix, ix)
     &                           + 6.28319*roph(NDMSCS, 4)
     &                           *ygdr*gdrw*D1Fra
                              pig(ix + kc1 - 4, ix + kc1 - 4)
     &                           = pig(ix + kc1 - 4, ix + kc1 - 4)
     &                           + 6.28319*roph(NDMSCS, kc1)*gdrw*D1Fra
                              pig(ix + kc1 - 4, ix + kc1 - 4)
     &                           = pig(ix + kc1 - 4, ix + kc1 - 4)
     &                           - cgd*roph(NDMSCS, kc1)
                           ENDDO
                        ENDIF
C-----------------------
C-----------------------inclusion of semi-direct into the PIG matrix
C-----------------------
                        IF(ISD.EQ.1)THEN
                           ecor = 1./G
                           spw = vmn*WT(3, 2, 1, egdrr - ecor)
                           prop = 1./(egdrr**2 + 0.25*(gdrw + spw)**2)
                           propt = prop*G*vmn*gdrw*D1Fra
C                          WRITE(6,'('' 2,.-->2,4, VERT. COL.'',G12.5)')PROPT
                           pig(2, 5) = pig(2, 5) - propt
                           pig(3, 5) = pig(3, 5) - propt
                           pig(4, 5) = pig(4, 5) - propt
                           propt = prop*gdrw*D1Fra*G/WT(5, 3, 2, 
     &                             EXCn - ecor)
C                          WRITE(6,'('' 2,4-->2,., BOTTOM ROW'',G12.5)')
C                          1                  (PROPT*SPW)
                           pig(5, 2) = pig(5, 2) - propt*spw
                           pig(5, 3) = pig(5, 3) - propt*spw
                           pig(5, 4) = pig(5, 4) - propt*spw
                           pig(5, 5) = pig(5, 5)
     &                                 + propt*(3*spw + (1 - D1Fra)
     &                                 *gdrw)
C                          TESTT=PROPT*(3*SPW+(1-D1FRA)*GDRW)
C                          WRITE(6,'('' 2,4-->2,4, LAST    '',G12.5)') TESTT
                           propt = prop*vmn*G*gdrw
C                          WRITE(6,'('' 2,.-->2,., DIAGONAL'',G12.5)') PROPT
                           pig(2, 2) = pig(2, 2) + propt
                           pig(3, 3) = pig(3, 3) + propt
                           pig(4, 4) = pig(4, 4) + propt
                        ENDIF
C-----------------------setting to zero matrix elem. where GDR is built on j=-1 states
                        IF(j.EQ.1)THEN
                           DO k1 = 1, ncm
                              DO k2 = 1, NG
                                 pig(1 + ISD + (k1 - 1)*4, k2) = 0.0
                                 pig(k2, 1 + ISD + (k1 - 1)*4) = 0.0
                              ENDDO
                           ENDDO
                        ENDIF
C-----------------------memorize PIG matrix
                        DO k1 = 1, NG
                           DO k2 = 1, NG
                              apig(k1, k2) = pig(k1, k2)
                           ENDDO
                        ENDDO
C-----------------------printout of the PIG matrix
                        IF(NG.LE.12 .AND. NOUt.GT.2)THEN
                           WRITE(6, 99013)j, i
99013                      FORMAT(1X, 'PIG matrix   J=', I2, ' I=', I2)
                           DO m2 = 1, NG
                              WRITE(6, 99023)(pig(m2, m1), m1 = 1, NG)
                           ENDDO
                           WRITE(6, 99022)
                        ENDIF
C-----------------------
C-----------------------invertion of the PIG matrix
C-----------------------
C                       DO inc = 1, NG
C                       ggg(inc) = 0.0
C                       ENDDO
C                       CALL MATIN1(pig,ggg,NG,1,det)
                        eps = 1.0D-8
                        CALL MTXINV(pig, pigx, pigy, mpigx, mpigy, NG, 
     &                              eps, irflag)
                        IF(irflag.NE.0)THEN
                           WRITE(6, *)'C.N. state J=', j*ipii, 
     &                     ' PIG-matrix in MSC gamma emission singular '
                           GOTO 20
                        ENDIF
C-----------------------printout of the inverted PIG matrix
                        IF(NG.LE.12 .AND. NOUt.GT.2)THEN
                           WRITE(6, 99014)j, i
99014                      FORMAT(1X, 'Inverse PIG matrix   J=', I2, 
     &                            ' I=', I2)
                           DO m2 = 1, NG
                              WRITE(6, 99023)(pig(m2, m1), m1 = 1, NG)
                           ENDDO
                           WRITE(6, 99022)
                        ENDIF
C-----------------------
C-----------------------check of the invertion and printout
C-----------------------
                        IF(NOUt.GT.2)THEN
                           DO k1 = 1, NG
                              DO k2 = 1, NG
                                 undeg(k1, k2) = 0.0
                                 DO k3 = 1, NG
                                    undeg(k1, k2) = undeg(k1, k2)
     &                                 + apig(k3, k1)*pig(k3, k2)
                                 ENDDO
                              ENDDO
                           ENDDO
                           IF(NG.LE.12)THEN
                              WRITE(6, 99015)j, i
99015                         FORMAT(1X, 'Control PIG matrix   J=', I2, 
     &                               ' I=', I2)
                              DO m2 = 1, NG
                                 WRITE(6, 99023)
     &                                 (undeg(m2, m1), m1 = 1, NG)
                              ENDDO
                           ELSE
                              WRITE(6, 99016)j, i
99016                         FORMAT(1X, 
     &                  'Diagonal elements of the control matrix for J='
     &                  , I2, 'D I=', I2, /)
                              WRITE(6, 99021)(undeg(m1, m1), m1 = 1, NG)
                           ENDIF
                           WRITE(6, 99022)
                        ENDIF
C-----------------------
C-----------------------normalization of the inverted PIG matrix with the initial
C-----------------------population for gamma emission
C-----------------------
                        DO k1 = 1, NG
                           spr(k1) = 0.0
                           IF(ISD.EQ.1)pig(1, k1) = pig(1, k1)
     &                        *POP(NEX(1), j, i, 1)*popart(1)*roph(1, 4)
     &                        /ropht(1)
                           DO k3 = kgin, NDMSCS
                              k2 = (k3 - kgin)*4 + ISD
                              pig(k2 + 1, k1) = pig(k2 + 1, k1)
     &                           *POP(NEX(1), j, i, 1)*popart(k3)
     &                           *roph(k3, 1)/ropht(k3)
                              pig(k2 + 2, k1) = pig(k2 + 2, k1)
     &                           *POP(NEX(1), j, i, 1)*popart(k3)
     &                           *roph(k3, 2)/ropht(k3)
                              pig(k2 + 3, k1) = pig(k2 + 3, k1)
     &                           *POP(NEX(1), j, i, 1)*popart(k3)
     &                           *roph(k3, 3)/ropht(k3)
                              pig(k2 + 4, k1) = pig(k2 + 4, k1)
     &                           *POP(NEX(1), j, i, 1)*popart(k3)
     &                           *roph(k3, 4)/ropht(k3)
                           ENDDO
                        ENDDO
                        IF(NOUt.GT.3)THEN
                           WRITE(6, 99017)(pig(1, k1), k1 = 1, NG)
99017                      FORMAT(1X, 'Normalized PIG to be used ', /, 
     &                            (1X, 12E11.4))
                           WRITE(6, 99022)
                        ENDIF
C-----------------------
C-----------------------do loop over c.n. excitation energy (after gamma emission)
C-----------------------
                        sumtlg = 0.0
                        DO kcn = 1, NEX(1) - 1
                           ew = EX(kcn, 1)
                           eg = EXCn - ew
                           ks = NEX(1) - kcn + 1
C--------------------------gamma transmission coefficient
                           tg = E1(eg, TNUc(kcn, 1))
C--------------------------
C--------------------------SD contribution
C--------------------------
                           IF(ISD.EQ.1)THEN
                              IF(kcn.EQ.1)THEN
C--------------------------------propagator
                                 prop = 1./
     &                                  (egdrr**2 + 0.25*(gdrw*D1Fra + 
     &                                  gdrw*(1-D1Fra)*.75)**2)
C--------------------------------absorption cross section times propagator
                                 sdabs = popart(1)
     &                              *prop*POP(NEX(1), j, i, 1)
     &                              *0.25/awj(1, j)
                                 WRITE(6, 
     &                '('' POP, PROPAGATOR, 3-P J DENS, SDABS'',4G12.5)'
     &                )POP(NEX(1), j, i, 1), prop, awj(1, j), sdabs
                              ENDIF
                              IF(STMro.EQ.0.D0)THEN
                                 rog = WT(1, 1, 0, ew)*DE
                              ELSE
                                 rog = ROPHM(10100, ini, ew, DE)
                              ENDIF
C-----------------------------GAMMA GAMMA / ((2 PI G)*(1+X)**2) *G
                              xga = (2.0 - tg - 2.0*SQRT(1.0 - tg))/tg
                              tlg = tg/(1 + xga)**2/(6.28*OMJd(1, j))
C                             TLG = TG/4./ROPH3(1)
                              tl1 = 0.0
                              IF(j.GT.1)tl1 = OMJd(1, j - 1)
                              tl2 = OMJd(1, j)
                              tl3 = OMJd(1, j + 1)
                              tls = rog*(tl1 + tl2 + tl3)*sdabs*tlg
                              sumtlg = sumtlg + tls/sdabs
C-----------------------------n o t e  ! ! ! x-section is not distributed over CN
C-----------------------------bins
                              sdsp(ks) = sdsp(ks) + tls
                              GSP(ks) = GSP(ks) + tls
                           ENDIF
C
C--------------------------SD contribution done
C
C--------------------------do loop over stages and substages
                           DO kg = kgin, NDMSCS
                              IF(rophc(kg).NE.0.0D0)THEN
C--------------------------------state density for gamma emission
                                 IF(STMro.EQ.0.D0)THEN
                                    ecor = 
     &                                 (((ip(kg)-1)**2 + (ih(kg)-1)**2)
     &                                 /4.0 + (ip(kg) - ih(kg))
     &                                 /4.0 - (ih(kg) - 1)/2.0)/G
                                    rog = WT(IE(kg) - 2, ip(kg) - 1, 
     &                                 ih(kg) - 1, ew - ecor)*DE
                                 ELSE
                                    ncont = 10000 + (ip(kg) - 1)
     &                                 *100 + ih(kg) - 1
                                    rog = ROPHM(ncont, ini, ew, DE)
                                 ENDIF
                                 tgc = tg*rog/roph3(kg)
                                 tg1 = 0.0
                                 IF(j.GT.1)tg1 = tgc*roph(kg, 1)
     &                              *OMJd(IE(kg) - 2, j - 1)
                                 tg2 = tgc*roph(kg, 2)
     &                                 *OMJd(IE(kg) - 2, j)
                                 tg3 = tgc*roph(kg, 3)
     &                                 *OMJd(IE(kg) - 2, j + 1)
                                 DO k1 = 1, NG
                                    ix = (kg - kgin)*4 + 1 + ISD
                                    sp = pig(k1, ix)*tg1/DE
                                    spr(ix) = spr(ix) + sp
                                    IF(ks.GT.1)THEN
                                       CSE(ks, 0, 1) = CSE(ks, 0, 1)
     &                                    + sp
                                       AUSpec(ks, 0) = AUSpec(ks, 0)
     &                                    + sp
                                       POPcse(kcn,0,ks,1) =  
     &                                    POPcse(kcn,0,ks,1) 
     &                                    + sp
                                    ENDIF
                                    gspmsc(kg, ks) = gspmsc(kg, ks) + sp
                                    IF(j.GT.1)POP(kcn, j - 1, ie1, 1)
     &                                 = POP(kcn, j - 1, ie1, 1) + sp
                                    emis = emis + sp*DE
                                    ix = ix + 1
                                    sp = pig(k1, ix)*tg2/DE
                                    spr(ix) = spr(ix) + sp
                                    IF(ks.GT.1)THEN
                                       CSE(ks, 0, 1) = CSE(ks, 0, 1)
     &                                    + sp
                                       AUSpec(ks, 0) = AUSpec(ks, 0)
     &                                    + sp
                                       POPcse(kcn,0,ks,1) =  
     &                                    POPcse(kcn,0,ks,1) 
     &                                    + sp
                                    ENDIF
                                    gspmsc(kg, ks) = gspmsc(kg, ks) + sp
                                    POP(kcn, j, ie1, 1)
     &                                 = POP(kcn, j, ie1, 1) + sp
                                    emis = emis + sp*DE
                                    ix = ix + 1
                                    sp = pig(k1, ix)*tg3/DE
                                    spr(ix) = spr(ix) + sp
                                    IF(ks.GT.1)THEN
                                       CSE(ks, 0, 1) = CSE(ks, 0, 1)
     &                                    + sp
                                       AUSpec(ks, 0) = AUSpec(ks, 0)
     &                                    + sp
                                       POPcse(kcn,0,ks,1) =  
     &                                    POPcse(kcn,0,ks,1) 
     &                                    + sp
                                    ENDIF
                                    gspmsc(kg, ks) = gspmsc(kg, ks) + sp
                                    POP(kcn, j + 1, ie1, 1)
     &                                 = POP(kcn, j + 1, ie1, 1) + sp
                                    emis = emis + sp*DE
                                 ENDDO
                              ENDIF
                           ENDDO
                        ENDDO
C                       WRITE(6,'('' SD GAMMA GAMMA ='',G12.5)') SUMTLG
C-----------------------T GAMMA GAMMA ESTIMATE
C                       DO LE = 1, 21, 2
C                       ENG = LE
C                       TG = E1(ENG,T)
C                       XGA = (2.0 - TG - 2.0*SQRT(1.0 - TG))/TG
Ccc                     TLG1 = GNOT*PROP*TG/(6.28*OMJ(1,1,0,J,HIS(1),0))
C                       1                  /(1+XGA)/2.
C                       TLG1 = GNOT*PROP*SUMTLG*(1 + XGA)/2.
Ccc                     TLG2 = PROP*TG/(6.28*OMJ(1,1,0,J,HIS(1),0))
C                       1                  /(1+XGA)**2
C                       TLG2 = PROP*SUMTLG
C-----------------------coupling to 1,2,3 subclasses
C                       TLG3 = TLG2*VMN*G/G**3
C-----------------------coupling to 4-th subclass
C                       END DO
C-----------------------t gamma gamma estimate done
                        IF(NOUt.GT.0)THEN
                           WRITE(6, 99022)
                           IF(ISD.EQ.1)WRITE(6, 99018)j, i, spr
99018                      FORMAT(1X, //, 1X, 'J=', I2, ' I=', I1, 
     &                         ' Gamma emission from subsequent stages:'
     &                         , /, 1X, E11.4, /, (1X, 4E11.4))
                           IF(ISD.EQ.0)WRITE(6, 99019)j, i, spr
99019                      FORMAT(1X, //, 1X, 'J=', I2, ' I=', I1, 
     &                         ' GAMMA EMISSION FROM SUBSEQUENT STAGES:'
     &                         , /, (1X, 4E11.4))
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
 20         IF(POP(NEX(1), j, i, 1).GT.0)REDmsc(j, i)
     &         = (POP(NEX(1), j, i, 1) - emis)/POP(NEX(1), j, i, 1)
            POP(NEX(1), j, i, 1) = POP(NEX(1), j, i, 1) - emis
         ENDDO
      ENDDO
C-----
C-----spectra histograms
C-----
      WRITE(6, 99020)NDMSCS
99020 FORMAT(1X, //, 30X, 'H e i d e l b e r g  M. S. C.  d e c a y  (', 
     &       I2, ' stages)', //)
C     IF(GST.NE.0)CALL AUERST(1, 0)
C     CALL AUERST(1, 1)
      IF(IOUt.GT.0)WRITE(6, 
     &           '(2X,A2,'' MSC emission cross section'',G12.5,'' mb'')'
     &           )SYMbe(1), CSMsc(1)
C     CALL AUERST(1, 2)
      IF(IOUt.GT.0)WRITE(6, 
     &           '(2X,A2,'' MSC emission cross section'',G12.5,'' mb'')'
     &           )SYMbe(2), CSMsc(2)
99021 FORMAT(1X, 12E11.4)
99022 FORMAT(1X, /)
99023 FORMAT(1X, 12E11.4)
      END
