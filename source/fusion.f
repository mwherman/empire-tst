Ccc   * $Author: mike $
Ccc   * $Date: 2001-11-06 08:50:34 $
Ccc   * $Id: fusion.f,v 1.3 2001-11-06 08:50:34 mike Exp $
C
      SUBROUTINE MARENG(Npro, Ntrg)
C
C
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:ppu*
Ccc   *                         M A R E N G                              *
Ccc   *                                                                  *
Ccc   * Calculates initial compound nucleus population after projectile  *
Ccc   * absorption  using transmission coefficients obtained from        *
Ccc   * the optical or the distributed barrier  model.                   *
Ccc   *                                                                  *
Ccc   * input:NPRO - projectile index (normally 0)                       *
Ccc   *       NTRG - target index (normally 0)                           *
Ccc   *                                                                  *
Ccc   * output:none                                                      *
Ccc   *                                                                  *
Ccc   * calls: HITL                                                      *
Ccc   *            BNDG                                                  *
Ccc   *                WHERE                                             *
Ccc   *                BAR                                               *
Ccc   *                    POTENT                                        *
Ccc   *                        POT                                       *
Ccc   *                POT                                               *
Ccc   *            PUSH                                                  *
Ccc   *                F                                                 *
Ccc   *                G                                                 *
Ccc   *                    F                                             *
Ccc   *                INTGRS                                            *
Ccc   *            XFUS                                                  *
Ccc   *        OMTL                                                      *
Ccc   *            FACT                                                  *
Ccc   *            PREANG                                                *
Ccc   *            SCAT                                                  *
Ccc   *                INTEG                                             *
Ccc   *                RCWFN                                             *
Ccc   *            SETPOTS                                               *
Ccc   *                OMPAR                                             *
Ccc   *            SHAPEC                                                *
Ccc   *                CGAMMA                                            *
Ccc   *            SHAPEL                                                *
Ccc   *                CLEB                                              *
Ccc   *                RACAH                                             *
Ccc   *            SPIN0                                                 *
Ccc   *            SPIN05                                                *
Ccc   *            SPIN1                                                 *
Ccc   *                                                                  *
Ccc   * author: M.Herman                                                 *
Ccc   * date:   15.Feb.1993                                              *
Ccc   * revision:1    by:Herman                   on:  .Oct.1994         *
Ccc   * revision:2    by:Capote                   on:  .Feb.2001         *
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C COMMON variables
C
      DOUBLE PRECISION ELTl(NDLW), S1
      COMMON /ELASTIC/ ELTl
      COMMON /WAN   / S1
C
C Dummy arguments
C
      INTEGER Npro, Ntrg
C
C Local variables
C
      DOUBLE PRECISION chsp, coef, csmax, csvalue, dtmp, eee, PI, smax, 
     &                 smin, stl(NDLW), sum, wf
      DOUBLE PRECISION DMAX1
      REAL FLOAT
      INTEGER i, ichsp, ip, ipa, j, k, l, lmax, lmin, maxlw, mul
      INTEGER MIN0
      DOUBLE PRECISION PAR, W2, xmas_npro, xmas_ntrg, RMU
      LOGICAL tlj_calc
      PAR(i, ipa, l) = 0.5*(1.0 - ( - 1.0)**i*ipa*( - 1.0)**l)
      tlj_calc = .FALSE.
C
C     Reduced mass corrected for proper mass values
C
C     xmas_Npro = ((AEJc(Npro)*amumev+XMAss_ej(Npro))/(amumev+xnexc))
C     xmas_Ntrg = ((A(Ntrg)*amumev+XMAss(Ntrg))/(amumev+xnexc))
C     rmu= xmas_Npro*xmas_Ntrg/(xmas_Npro+xmas_Ntrg)
      xmas_npro = (AEJc(Npro)*AMUmev + XMAss_ej(Npro))/AMUmev
      xmas_ntrg = (A(Ntrg)*AMUmev + XMAss(Ntrg))/AMUmev
C     rmu = xmas_npro*xmas_ntrg/(xmas_npro + xmas_ntrg)
      el = EINl
      CALL KINEMA(el, ecms, xmas_npro, xmas_ntrg, RMU, ak2, 1, RELkin)
C
C     wf = W2*EIN*rmu
C     wf = W2*ecms*rmu
      wf = ak2/10.D0
C
      coef = PI/wf/(2*XJLv(1, Ntrg) + 1.0)/(2*SEJc(Npro) + 1.0)
      S1 = 0.5
      maxlw = NDLW
      IF(AINT(XJLv(1,Ntrg) + SEJc(Npro)) - XJLv(1, Ntrg) - SEJc(Npro)
     &   .EQ.0.0D0)S1 = 1.0
      csmax = 0.0
      CSFus = 0.0
      DO i = 1, NDLW
         stl(i) = 0.0
      ENDDO
C-----if FUSREAD true read l distribution of fusion cross section
C-----and calculate transmission coefficients
      IF(FUSread)THEN
         DO j = 1, NDLW
            READ(11, *, END = 50)csvalue
            stl(j) = csvalue*wf/PI/(2*j - 1)
            IF(stl(j).GT.1.0D0)THEN
               WRITE(6, *)' '
               WRITE(6, 
     &'(''TOO LARGE INPUT FUSION CROSS SECTION'',              '' FOR l=
     &'',I3,'' RESULTING Tl>1'')')j - 1
               WRITE(6, *)' EXECUTION STOPPED!!!'
               STOP
            ENDIF
         ENDDO
 50      NLW = j - 1
         WRITE(6, *)
     &  ' Spin distribution of fusion cross section read from the file '
         WRITE(6, *)
     &          ' (all previous instructions concerning fusion ignored)'
         GOTO 100
      ENDIF
C-----calculation of o.m. transmission coefficients for absorption
      IF(KTRlom(Npro, Ntrg).GT.0)THEN
C
         einlab = -EINl
         IWArn = 0
C
         IF(DIRect.EQ.2 .AND. AEJc(Npro).LE.1)THEN
C           Target nucleus (elastic channel), incident neutron or proton
            WRITE(6, *)' CC transmission coefficients used for ', 
     &                 'fusion determination'
C------     Transmission coefficient matrix for incident channel
C           is calculated (DIRECT = 2 (CCM)) using ECIS code.
C           Preparing INPUT and RUNNING ECIS
C           (or reading already calculated file)
            IF(DEFormed)THEN
               CALL ECIS_CCVIBROT(Npro, Ntrg, einlab, .TRUE.)
            ELSE
               CALL ECIS_CCVIB(Npro, Ntrg, einlab, .TRUE.)
            ENDIF
            CALL ECIS2EMPIRE_TL_TRG(Npro, Ntrg, maxlw, stl)
            tlj_calc = .TRUE.
         ENDIF
C
         IF(.NOT.tlj_calc)THEN
            WRITE(6, *)' Spherical OM transmission coefficients', 
     &                 ' used for fusion determination'
            IF(MODelecis.EQ.0 .OR. DIRect.EQ.3)THEN
               CALL OMTL(Npro, Ntrg, einlab, maxlw, stl, 1)
            ELSE
               WRITE(6, *)' Fusion cross section normalized', 
     &                    ' to coupled channel reaction cross section'
               CALL OMTL(Npro, Ntrg, einlab, maxlw, stl, 0)
            ENDIF
         ENDIF
C        IWARN=0 - 'NO Warnings'
C        IWARN=1 - 'A out of the recommended range '
C        IWARN=2 - 'Z out of the recommended range '
C        IWARN=3 - 'Energy requested lower than recommended for this potential'
C        IWARN=4 - 'Energy requested higher than recommended for this potential'
         IF(IWArn.EQ.1)WRITE(6, *)
     &                       ' WARNING: OMP not recommended for A='
     &                       , A(Ntrg)
         IF(IWArn.EQ.2)WRITE(6, *)
     &                       ' WARNING: OMP not recommended for Z='
     &                       , Z(Ntrg)
         IF(IWArn.EQ.3 .OR. IWArn.EQ.4)WRITE(6, *)
     &      ' WARNING: OMP not recommended for E=', EIN
         IWArn = 0
C
         IF(maxlw.GT.NDLW)THEN
            WRITE(6, *)' '
            WRITE(6, *)' INSUFFICIENT NUMBER OF PARTIAL WAVES ALLOWED'
            WRITE(6, *)' INCREASE NDLW IN dimension.h UP TO', maxlw + 1
            WRITE(6, *)' AND RECOMPILE THE CODE'
            STOP
         ENDIF
C
      ENDIF
C-----calculation of h.i. transmission coefficients for fusion
      IF(KTRlom(Npro, Ntrg).EQ.0)CALL HITL(stl)
C-----calculation of transmission coefficients ----done------
      DO i = 1, NDLW
         ELTl(i) = stl(i)
      ENDDO
C-----channel spin min and max
 100  eee = SEJc(Npro) - XJLv(1, Ntrg)
      smin = ABS(eee)
      smax = SEJc(Npro) + XJLv(1, Ntrg)
      mul = smax - smin + 1.0001
      CSFus = 0.0
C-----do loop over parity
      DO ip = 1, 2
C-----do loop over compound nucleus spin
         DO j = 1, NDLW
            sum = 0.0
            DO ichsp = 1, mul
               chsp = smin + FLOAT(ichsp - 1)
               lmin = ABS(j - chsp - S1) + 0.0001
               lmax = j + chsp - S1 + 0.0001
               lmin = lmin + 1
               lmax = lmax + 1
               lmax = MIN0(NDLW, lmax)
               lmax = MIN0(maxlw, lmax)
               DO k = lmin, lmax
                  sum = sum + PAR(ip, LVP(1, 0), k - 1)*stl(k)*DRTl(k)
               ENDDO
            ENDDO
            POP(NEX(1), j, ip, 1) = coef*sum*(FLOAT(2*j + 1) - 2.0*S1)
     &                              *FUSred
            CSFus = CSFus + POP(NEX(1), j, ip, 1)
            csmax = DMAX1(POP(NEX(1), j, ip, 1), csmax)
         ENDDO
      ENDDO
C-----CAPOTE 2001
      IF((DIRect.EQ.1 .OR. DIRect.EQ.3) .AND. AEJc(Npro).LE.1)THEN
         ecis_abs = 0.
C--------read ECIS absorption cross section
         OPEN(45, FILE = 'ecis95.cs', STATUS = 'OLD')
         READ(45, *, END = 150)totcs
         READ(45, *, END = 150)ecis_abs
 150     CLOSE(45)
         SINl = 0
         OPEN(UNIT = 45, FILE = 'ecis95.ics', STATUS = 'old', ERR = 200)
C--------assuming maximum number of collective states is 100
         DO l = 1, 100
            READ(45, *, END = 200)dtmp
            SINl = SINl + dtmp
         ENDDO
 200     CLOSE(45)
C
         IF(SINl.GT.ecis_abs)THEN
            WRITE(6, 
     &'(///                                                       5x,''*
     &*************************************************'')')
            WRITE(6, 
     &     '(5x,'' Direct cross section calculation do not converge '')'
     &     )
            WRITE(6, 
     &'(6x,''Inelastic cross section ='',F8.2,'' mb''/                  
     &  6x,''Reaction  cross section ='',F8.2,'' mb''/)')SINl, ecis_abs
            WRITE(6, 
     &     '(5x,'' Either change OMP or change calculation method   '')'
     &     )
            WRITE(6, 
     &     '(5x,''        (DIRPOT)   or   (DIRECT) parameters       '')'
     &     )
            WRITE(6, 
     &     '(5x,'' This problem usually happens using DWBA method   '')'
     &     )
            WRITE(6, 
     &     '(5x,'' to treat strong coupled nuclei                   '')'
     &     )
            WRITE(6, 
     &     '(5x,''            CALCULATION STOPPED                   '')'
     &     )
            WRITE(6, 
     &     '(5x,''**************************************************'')'
     &     )
            STOP 200
         ENDIF
C--------Renormalizing Tls
C
         IF(MODelecis.GT.0 .AND. DIRect.EQ.1)THEN
C           for CC OMP renormalizing to reaction XS calculated by ECIS
            DO l = 1, maxlw
               stl(l) = stl(l)*(ecis_abs - SINl)/CSFus
               ELTl(l) = stl(l)
            ENDDO
         ELSE
C           for SOMP including inelastic reaction XS calculated by ECIS
C           in the SCAT2 calculated reaction XS
            DO l = 1, maxlw
               stl(l) = stl(l)*(CSFus - SINl)/CSFus
               ELTl(l) = stl(l)
            ENDDO
         ENDIF
C--------channel spin min and max
         eee = SEJc(Npro) - XJLv(1, Ntrg)
         smin = ABS(eee)
         smax = SEJc(Npro) + XJLv(1, Ntrg)
         mul = smax - smin + 1.0001
         CSFus = 0.0
         DO ip = 1, 2 ! over parity
            DO j = 1, NDLW !over compound nucleus spin
               sum = 0.0
               DO ichsp = 1, mul
                  chsp = smin + FLOAT(ichsp - 1)
                  lmin = ABS(j - chsp - S1) + 0.0001
                  lmax = j + chsp - S1 + 0.0001
                  lmin = lmin + 1
                  lmax = lmax + 1
                  lmax = MIN0(NDLW, lmax)
                  lmax = MIN0(maxlw, lmax)
                  DO k = lmin, lmax
                     sum = sum + PAR(ip, LVP(1, 0), k - 1)*stl(k)
     &                     *DRTl(k)
                  ENDDO
               ENDDO
               POP(NEX(1), j, ip, 1) = coef*sum*(FLOAT(2*j + 1) - 2.0*S1
     &                                 )*FUSred
               CSFus = CSFus + POP(NEX(1), j, ip, 1)
               csmax = DMAX1(POP(NEX(1), j, ip, 1), csmax)
            ENDDO
         ENDDO
C--------Renormalization of Tls and fusion cros section done for DIRECT.eq.1
C--------add ECIS inelastic to the fusion cross section
C        Only needed for non CC OMP potentials
         IF(DIRect.NE.2)CSFus = CSFus + SINl
C
      ENDIF
C
      DO j = NDLW, 1, -1
         NLW = j
         IF(POP(NEX(1), j, 1, 1)*10000.D0.GT.csmax)GOTO 300
         IF(POP(NEX(1), j, 2, 1)*10000.D0.GT.csmax)GOTO 300
      ENDDO
C-----the next line can be used to increase the number of partial waves
C-----e.g., to account for a high-spin isomer
C     NLW = NLW + 3
C-----check whether NLW is not larger then max spin at which nucleus
C-----is still stable
 300  IF(NLW.GT.JSTab(1))THEN
         NLW = JSTab(1)
         IF(IOUt.GT.0)THEN
            WRITE(6, '('' Maximum spin to preserve stability is'',I4)')
     &            JSTab(1)
            WRITE(6, 
     &            '('' Calculations will be truncated at this limit'')')
            WRITE(6, 
     &            '('' part of the fusion cross section will be lost'')'
     &            )
         ENDIF
         DO j = NLW + 1, NDLW
            CSFus = CSFus - POP(NEX(1), j, 1, 1) - POP(NEX(1), j, 2, 1)
            POP(NEX(1), j, 1, 1) = 0.0
            POP(NEX(1), j, 2, 1) = 0.0
         ENDDO
         RETURN
      ENDIF
      IF((POP(NEX(1),NLW,1,1)*20.D0.GT.csmax .OR. POP(NEX(1),NLW,2,1)
     &   *20.D0.GT.csmax) .AND. NLW.EQ.NDLW)THEN
         WRITE(6, *)'POP1=', POP(NEX(1), NLW, 1, 1), 'POP2=', 
     &              POP(NEX(1), NLW, 2, 1), 'NLW=', NLW
         WRITE(6, 
     &'('' NUMBER OF PARTIAL WAVES FOR WHICH CODE IS DIMENSIONE'',      
     &''D IS INSUFFICIENT'',/,'' INCREASE NDLW IN THE dimensio'',       
     &''n.h FILE AND RECOMPILE  '',/,'' EXECUTION  S T O P P E '',      
     &''D '')')
         STOP
      ENDIF
      END
C
C
C
      SUBROUTINE BASS(Ein, Zp, Ap, Zt, At, Bfus, E1, Crl, Csfus)
Ccc
Ccc   *********************************************************************
Ccc   *                                                         class:ppu *
Ccc   *                         B A S S
Ccc   * Calculates fusion x-section critical l-value for a heavy-ion
Ccc   * induced reaction according to Bass model. E1 is the energy at
Ccc   * which the linear dependence of l critical begins.
Ccc   * ref: formulae  from Bass, Nucl. Phys. A231(1974)45,
Ccc   * and nuclear potential from Phys. Rev. Lett. 39(1977)265
Ccc   *
Ccc   * input:EIN-incident energy (c.m.)
Ccc   *       ZP -Z of a projectile
Ccc   *       AP -A of a projectile
Ccc   *       ZT -Z of a target
Ccc   *       AT -A of a target
Ccc   *
Ccc   * output:BFUS-fusion barrier
Ccc   *        E1   -see above
Ccc   *        CRL  -critical angular momentum
Ccc   *        CSFUS-fusion x-section
Ccc   *
Ccc   * calls:FINDA
Ccc   *
Ccc   * authors:A.D'Arrigo, M.Herman, A.Taccone                           *
Ccc   * date:     .Jul.1991
Ccc   * addapted      by:M.Herman                 on:18.Feb.1993
Ccc   * revision:#    by:name                     on:xx.mon.199x
Ccc   *                   D I S A B L E D
Ccc   *********************************************************************
Ccc
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C
C COMMON variables
C
      DOUBLE PRECISION MI, R1, R12, R2
      COMMON /FIND  / R1, R2, R12, MI
C
C Dummy arguments
C
      DOUBLE PRECISION Ap, At, Bfus, Crl, Csfus, E1, Ein, Zp, Zt
C
C Local variables
C
      DOUBLE PRECISION arg, d, dfu, e2, ee2, f, ht, le1, le2, m0, m1, 
     &                 m2, p, t, vc, vmax, vmm, vn, x, y
      INTEGER INT
      INTEGER j, jl
C
      DATA e2, m0, ht, d/1.44, 1.044, 6.589, 1.35/
      m1 = Ap*m0
      m2 = At*m0
      MI = Ap*At/(Ap + At)*m0
      p = Ap**(1./3.)
      t = At**(1./3.)
      R1 = 1.16*p - 1.39/p
      R2 = 1.16*t - 1.39/t
      R12 = R1 + R2
      x = Zp*Zt*e2*(R1 + R2)/(R1*R2*R12**2)*0.07
      y = ht**2*(R1 + R2)/(MI*R1*R2*R12**3)*0.07
      f = 1./(1. + 2./5.*((m1*R1**2.+m2*R2**2.)/(MI*R12**2.)))
      le1 = SQRT((1. - x)/y)
      le2 = le1/f
      vc = Zp*Zt*e2/R12
      vn = R1*R2/R12*(1./(0.03 + 0.0061))
      E1 = vc - vn + (ht**2*le1**2)/(2.*MI*R12**2)
      ee2 = vc - vn + (ht**2*le2**2)/(2.*MI*R12**2)
      dfu = -d*LOG(x)/(1. - 2.*d/R12)
      arg = dfu/d
      IF(arg.GT.74.D0)arg = 74.
      Bfus = Zp*Zt*e2/R12*(R12/(R12 + dfu) - d/(x*R12)*EXP((-arg)))
      IF(Ein.GT.Bfus)THEN
         Crl = SQRT((2.*MI*R12**2/ht**2)*(Ein - vc + vn))
         vmm = 0.0
         vmax = 10000.0
         IF(Ein.LT.E1)THEN
            jl = INT(le1)
            DO j = 1, jl
               Crl = jl - j + 1
               vmm = vmax
               CALL FINDA(Zp, Zt, Crl, vmax)
               IF(vmax.LE.Ein)GOTO 50
            ENDDO
         ENDIF
 50      IF(Ein.GT.ee2)Crl = le2
         IF(Ein.LT.E1)Crl = Crl + (Ein - vmax)/(vmm - vmax)
      ELSE
         WRITE(6, '(1X,''Incident energy below fusion barrier'')')
         STOP
      ENDIF
      Csfus = 657.*(Ap + At)*Crl**2/(Ap*At*Ein)
      END
C
      SUBROUTINE FINDA(Zp, Zt, Crl, Vm)
Ccc
Ccc ********************************************************************
Ccc *                                                         class:mpu*
Ccc *                         F I N D A                                *
Ccc *                                                                  *
Ccc * Solves the equation in Bass model; VM is a solution              *
Ccc *                                                                  *
Ccc * input:ZP -Z of a projectile                                      *
Ccc *       AP -A of a projectile                                      *
Ccc *       ZT -Z of a target                                          *
Ccc *       AT -A of a target                                          *
Ccc *       CRL-l critical                                             *
Ccc *                                                                  *
Ccc * output:VM -solution                                              *
Ccc *                                                                  *
Ccc * calls:none                                                       *
Ccc *                                                                  *
Ccc * author: D'Arrigo                                                 *
Ccc * date:     .Jul.1991                                              *
Ccc * addapted      by:M.Herman                 on:18.Feb.1993         *
Ccc * revision:#    by:name                     on:xx.mon.199x         *
Ccc *                                                                  *
Ccc *                                                                  *
Ccc ********************************************************************
Ccc
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C
C COMMON variables
C
      DOUBLE PRECISION MI, R1, R12, R2
      COMMON /FIND  / R1, R2, R12, MI
C
C Dummy arguments
C
      DOUBLE PRECISION Crl, Vm, Zp, Zt
C
C Local variables
C
      DOUBLE PRECISION e2, eps, ht, xm, xn, xp
      INTEGER nr
C
      DATA e2, ht/1.44, 6.589/
      DATA eps/0.001/
      nr = 0
      xn = R12
      xp = 3*R12
 100  xm = (xn + xp)/2
      nr = nr + 1
      IF(nr.LE.50)THEN
         IF(ABS((-Zp*Zt*e2/xm**2) + ((-ht**2*Crl**2/(MI*xm**3)))
     &      + R1*R2/R12*(0.03/3.3*EXP((xm-R12)/3.3)
     &      +0.0061/0.65*EXP((xm-R12)/0.65))
     &      /(0.03*EXP((xm-R12)/3.3)+0.0061*EXP((xm-R12)/0.65))**2)
     &      .GT.eps)THEN
            IF(((-Zp*Zt*e2/xm**2)) + ((-ht**2*Crl**2/(MI*xm**3)))
     &         + R1*R2/R12*(0.03/3.3*EXP((xm-R12)/3.3)
     &         + 0.0061/0.65*EXP((xm-R12)/0.65))
     &         /(0.03*EXP((xm-R12)/3.3) + 0.0061*EXP((xm-R12)/0.65))
     &         **2.LT.0.D0)THEN
               xp = xm
               GOTO 100
            ELSEIF(((-Zp*Zt*e2/xm**2)) + ((-ht**2*Crl**2/(MI*xm**3)))
     &             + R1*R2/R12*(0.03/3.3*EXP((xm-R12)/3.3)
     &             + 0.0061/0.65*EXP((xm-R12)/0.65))
     &             /(0.03*EXP((xm-R12)/3.3) + 0.0061*EXP((xm-R12)/0.65))
     &             **2.NE.0.D0)THEN
               xn = xm
               GOTO 100
            ENDIF
         ENDIF
      ENDIF
      Vm = Zp*Zt*e2/xm + ht**2*Crl**2/(2*MI*xm**2)
     &     - R1*R2/R12/(0.03*EXP((xm-R12)/3.3)
     &     + 0.0061*EXP((xm-R12)/0.65))
      IF(nr.GT.50)WRITE(6, '(10X,''MAX NO. OF ITERATIONS IN FINDA'')')
      END
C
      DOUBLE PRECISION FUNCTION XFUS(Ein, Ap, At, D, Crl)
Ccc
Ccc ********************************************************************
Ccc *                                                         class:ppu*
Ccc *                         X F U S                                  *
Ccc *                                                                  *
Ccc *                                                                  *
Ccc *                                                                  *
Ccc * input:EIN - incident energy (c.m.)                               *
Ccc *       AP  - projectile A                                         *
Ccc *       AT  - target A                                             *
Ccc *       D   - difusness in transmission coefficient formula        *
Ccc *       CRL - l critical for fusion                                *
Ccc *                                                                  *
Ccc * output:XFUS- fusion x-section                                    *
Ccc *                                                                  *
Ccc * calls:none                                                       *
Ccc *                                                                  *
Ccc * author: M.Herman                                                 *
Ccc * date:     .Jul.1991                                              *
Ccc * revision:#    by:name                     on:xx.mon.199x         *
Ccc *                                                                  *
Ccc ********************************************************************
Ccc
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C
C Dummy arguments
C
      DOUBLE PRECISION Ap, At, Crl, D, Ein
C
C Local variables
C
      DOUBLE PRECISION al, args, sum, tl
      REAL FLOAT
      INTEGER i, icrl
      INTEGER INT
C
      sum = 0.0
      icrl = INT(Crl + 5.0*D)
      DO i = 1, icrl
         al = FLOAT(i - 1)
         args = (al - Crl)/D
         IF(args.GT.74.D0)args = 74.
         tl = 1./(1. + EXP(args))
         sum = (2.*al + 1.)*tl + sum
      ENDDO
      XFUS = 657.*(Ap + At)/(Ap*At*Ein)*sum
      END
C
C
      SUBROUTINE PUSH(Ecm, A, Ap, At, Bas, Expush, Sigi, Trunc, Stl, 
     &                Nlw, Ndlw)
Ccc ********************************************************************
Ccc *                                                         class:ppu*
Ccc *                      P U S H                                     *
Ccc *                                                                  *
Ccc *  Calculates fusion transmission coefficients in the distributed  *
Ccc *  fusion barrier model.                                           *
Ccc *                                                                  *
Ccc *                                                                  *
Ccc * output:STL - fusion transmission coefficients                    *
Ccc *                                                                  *
Ccc * calls:INTGRS                                                     *
Ccc *                                                                  *
Ccc * author: M.Herman                                                 *
Ccc * date:   about 1992                                               *
Ccc * revision:#    by:name                     on:xx.mon.199x         *
Ccc *                                                                  *
Ccc *                                                                  *
Ccc ********************************************************************
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C
C COMMON variables
C
      DOUBLE PRECISION BAVe, E, EROt, SIG
      COMMON /EXTRAP/ BAVe, EROt, E, SIG
C
C Dummy arguments
C
      DOUBLE PRECISION A, Ap, At, Bas, Ecm, Expush, Sigi, Trunc
      INTEGER Ndlw, Nlw
      DOUBLE PRECISION Stl(Ndlw)
C
C Local variables
C
      DOUBLE PRECISION amu, dintf, prob, r0, rf, xlow, xmax
      DOUBLE PRECISION F, G
      INTEGER j
      EXTERNAL F, G
C
      DATA r0/1.07/
      E = Ecm
      SIG = Sigi
      BAVe = Bas + Expush
      xlow = MAX(BAVe - Trunc*SIG, 0.D0)
      xmax = BAVe + Trunc*SIG
      CALL INTGRS(xlow, xmax, F, dintf)
      amu = At*Ap/A
      rf = r0*(At**0.3333 + Ap**0.3333)
      DO j = 1, Ndlw
         EROt = (j - 1)*j*20.79259/(amu*rf**2)
         EROt = EROt/2.0
         CALL INTGRS(xlow, xmax, G, prob)
         Stl(j) = prob/dintf
         IF(Stl(j).NE.0.0D0)Nlw = j
      ENDDO
      END
C
      DOUBLE PRECISION FUNCTION F(X)
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C COMMON variables
C
      DOUBLE PRECISION BAVe, E, EROt, SIG
      COMMON /EXTRAP/ BAVe, EROt, E, SIG
C
C Dummy arguments
C
      DOUBLE PRECISION X
C
      F = EXP(( - (BAVe-X)/(2.0*SIG**2))**2)
      END
C
      DOUBLE PRECISION FUNCTION G(X)
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C Capote 2001, added common variables
C
C COMMON variables
C
      DOUBLE PRECISION BAVe, E, EROt, SIG
      COMMON /EXTRAP/ BAVe, EROt, E, SIG
C
C
C Local variables
C
      DOUBLE PRECISION arg, htom, pi
      DOUBLE PRECISION F
      EXTERNAL F
C
      DATA pi, htom/3.14159D0, 4.D0/
      arg = -2.*pi*(E - X - EROt)/htom
      IF(arg.LT.( - 74.D0))G = F(X)
      IF(arg.GT.74.D0)G = 0.
      IF(ABS(arg).LE.74.D0)G = F(X)/(1 + EXP((-2.*pi*(E-X-EROt)/htom)))
      END
