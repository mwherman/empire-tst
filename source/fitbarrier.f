Ccc   * $Author: shoblit $
Ccc   * $Date: 2012-02-09 21:34:11 +0100 (Do, 09 Feb 2012) $
Ccc   * $Id: fitbarrier.f,v 1.7 2009/06/15 21:52:21 Capote Exp $
 
      SUBROUTINE WKBFIS(Ee,Nnuc,Tfdd,Tdirp,Tabsp)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8, DIMENSION(2*NFParab) :: EINters
      REAL*8, DIMENSION(NFIsbarpnt) :: EPS_1d, VDEf_1d
      REAL*8 :: FAZa2
      REAL*8, DIMENSION(NFParab) :: HO, VBArex
      INTEGER, DIMENSION(0:2*NFParab) :: IIExtr
      INTEGER :: K, NEXtr, NPOints
      REAL*8 :: SMIu, UEXc
      REAL*8, DIMENSION(NFParab) :: VHEigth, VWIdth
      REAL*8, DIMENSION(NFParab + 1) :: VPOs
      COMMON /INTER / EINters
      COMMON /NUMBAR/ EPS_1d, VDEf_1d, NPOints, IIExtr, NEXtr
      COMMON /NUMBAR1/ VHEigth, VWIdth, VPOs
      COMMON /VARGS / UEXc, SMIu, K
      COMMON /VBAR  / VBArex, HO, FAZa2
C
C Dummy arguments
C
      REAL*8 :: Ee
      INTEGER :: Nnuc
      REAL*8, DIMENSION(NFParab,NFParab) :: Tabsp, Tdirp
      REAL*8, DIMENSION(NFParab) :: Tfdd
C
C Local variables
C
      REAL*8 :: abserr, epsa, epsb, phase_sub
      REAL*8 :: DCOS, DEXP, DSQRT
      REAL*8, DIMENSION(NFParab) :: delt, deltt, exm2del, exp2del, 
     &                              phasep, phase_h
      LOGICAL :: discrete
      REAL*8 :: dmom, dnum, rmiu, tdr, w
      REAL*8, EXTERNAL :: FMOMENT1, GAUSSLEGENDRE41
      INTEGER :: ih, ih1, iphas_opt, iw, iw1, j, kh, kw
      INTEGER :: INT
      REAL*8, DIMENSION(2*NFParab) :: phase
      REAL :: SNGL
C
C*** End of declarations rewritten by SPAG
C
C
      rmiu = 0.054D0*A(Nnuc)**(5.D0/3.D0)
      SMIu = DSQRT(0.5D0*rmiu)
      UEXc = Ee
      discrete = .TRUE.
 
      DO K = 1, 2*NRBar
        phase(K) = 0.D0
      ENDDO
C-----reasigning humps and wells
      DO kh = 1, NRBar, 2
        VHEigth(kh) = VBArex(INT(kh/2) + 1)
        VWIdth(kh) = H(1,INT(kh/2) + 1)
      ENDDO
 
      DO kw = 2, NRBar, 2
        VHEigth(kw) = VBArex(NRHump + INT(kw/2))
        VWIdth(kw) = H(1,NRHump + INT(kw/2))
      ENDDO
 
      IF(FISbar(Nnuc).EQ.3.)THEN
        DO j = 1, NEXtr
          VPOs(j) = DEFfis(j)
        ENDDO
        VPOs(NEXtr + 1) = 100.D0
      ENDIF
 
C---- Momentum integrals are calculated
      iphas_opt = 0
                   ! phases calculated on decoupled parabolas
 
      IF(FISbar(Nnuc).EQ.3.OR.FISopt(Nnuc).GT.0)iphas_opt = 1
      IF(FISbar(Nnuc).EQ.3)THEN
        CALL PHASES(Ee,phase,phase_h,Nnuc,iphas_opt,discrete)
      ELSE
        CALL PHASES_PARAB(Ee,Nnuc,phase,discrete)
      ENDIF
 
C-----Calculating transmission from phases
      DO ih = 1, NRBar, 2
        Tfdd(ih/2 + 1) = 1.D0/(1.D0 + DEXP(2.D0*phase(ih)))
      ENDDO
 
      IF(FISopt(Nnuc).EQ.0)RETURN
 
C-------Imaginary potential strengths
      w = 0.D0
      DO iw = 2, NRBar, 2
        phasep(iw/2 + 1) = phase(iw)
      ENDDO
 
      DO iw = 2, NRBar, 2
        deltt(iw) = 0.D0
        dmom = MAX(VHEigth(iw - 1),VHEigth(iw + 1))
 
        w = WIMag(iw/2,1) + WIMag(iw/2,2)*(Ee - VHEigth(iw))
     &      + WIMag(iw/2,3)*DEXP(Ee - dmom)
 
        IF(Ee.LE.VHEigth(iw))w = 0.D0
        IF(Ee.GT.dmom.AND.w.LT.1.D0)w = w + (Ee - dmom)*(1.D0 - w)/0.1
        IF(Ee.LT.VHEigth(iw))phase(iw) = 0.D0
        deltt(iw) = w*phase(iw)
      ENDDO
 
      DO iw = 2, NRBar, 2
        delt(iw/2 + 1) = (1.D0)*deltt(iw)
      ENDDO
      DO iw = 2, NRWel + 1
        IF(2.D0*delt(iw).GT.EXPmax)delt(iw) = EXPmax/2.D0
        exp2del(iw) = DEXP(2.D0*delt(iw))
        exm2del(iw) = DEXP( - 2.D0*delt(iw))
        IF(delt(iw).EQ.0.D0)exp2del(iw) = 1.D0
        IF(delt(iw).EQ.0.D0)exm2del(iw) = 1.D0
      ENDDO
C-----Direct transmission coefficients
      DO ih = 1, NRHump
        DO ih1 = 1, NRHump
          Tdirp(ih,ih1) = 0.D0
          Tdirp(ih1,ih) = 0.D0
          IF(ih.EQ.ih1)Tdirp(ih,ih1) = Tfdd(ih)
        ENDDO
      ENDDO
C-----subwell excitation energy
      IF(NRWel.EQ.1.AND.Ee.LT.VHEigth(2))THEN
        Tabsp(1,2) = 0.D0
        IF(FISbar(Nnuc).EQ.3)THEN
          epsa = VPOs(1) - SQRT(VHEigth(1) - Ee)/(SMIu*VWIdth(1))
          epsb = VPOs(3) + SQRT(VHEigth(3) - Ee)/(SMIu*VWIdth(3))
        ELSE
          epsa = EINters(1)
          epsb = EINters(6)
        ENDIF
        dmom = GAUSSLEGENDRE41(FMOMENT1,epsa,epsb,abserr)
        IF(dmom.GT.0.D0.AND.abserr.GT.dmom*0.03)THEN
          WRITE(*,*)' WARNING: For extremum ', K, 
     &              ' phase integral is not accurate (', 
     &              SNGL(abserr/dmom*100.D0), ' %)'
        ENDIF
        phase_sub = MIN(dmom,50.D0)
        Tdirp(1,2) = 1.D0/(1.D0 + DEXP(2.D0*phase_sub))
      ENDIF
      IF(NRWel.EQ.2.AND.Ee.LT.VHEigth(2).AND.Ee.LT.VHEigth(4))THEN
        Tabsp(1,2) = 0.D0
        Tabsp(1,3) = 0.D0
        Tabsp(2,3) = 0.D0
        Tabsp(3,2) = 0.D0
        Tdirp(1,2) = 0.D0
        Tdirp(2,1) = 0.D0
        Tdirp(2,3) = 0.D0
        Tdirp(3,2) = 0.D0
        IF(FISbar(Nnuc).EQ.3)THEN
          epsa = VPOs(1) - SQRT(VHEigth(1) - Ee)/(SMIu*VWIdth(1))
          epsb = VPOs(5) + SQRT(VHEigth(5) - Ee)/(SMIu*VWIdth(5))
        ELSE
          epsa = EINters(1)
          epsb = EINters(10)
        ENDIF
        dmom = GAUSSLEGENDRE41(FMOMENT1,epsa,epsb,abserr)
        IF(dmom.GT.0.D0.AND.abserr.GT.dmom*0.03)THEN
          WRITE(*,*)' WARNING: For extremum ', K, 
     &              ' phase integral is not accurate (', 
     &              SNGL(abserr/dmom*100.D0), ' %)'
        ENDIF
        phase_sub = MIN(dmom,50.D0)
        Tdirp(1,3) = 1.D0/(1.D0 + DEXP(2.D0*phase_sub))
        RETURN
      ENDIF
      IF(NRWel.EQ.2.AND.Ee.GT.VHEigth(2).AND.Ee.LT.VHEigth(4))THEN
        Tabsp(1,3) = 0.D0
        Tabsp(3,2) = 0.D0
        Tabsp(2,3) = 0.D0
        Tdirp(1,2) = 0.D0
        IF(FISbar(Nnuc).EQ.3)THEN
          epsa = VPOs(3) - SQRT(VHEigth(3) - Ee)/(SMIu*VWIdth(3))
          epsb = VPOs(5) + SQRT(VHEigth(5) - Ee)/(SMIu*VWIdth(5))
        ELSE
          epsa = EINters(5)
          epsb = EINters(10)
        ENDIF
        dmom = GAUSSLEGENDRE41(FMOMENT1,epsa,epsb,abserr)
        IF(dmom.GT.0.D0.AND.abserr.GT.dmom*0.03)THEN
          WRITE(*,*)' WARNING: For extremum ', K, 
     &              ' phase integral is not accurate (', 
     &              SNGL(abserr/dmom*100.D0), ' %)'
        ENDIF
        phase_sub = MIN(dmom,50.D0)
        Tdirp(2,3) = 1.D0/(1.D0 + DEXP(2.D0*phase_sub))
        Tdirp(3,2) = Tdirp(2,3)
        dmom = (1.D0 - Tdirp(1,1))*(1.D0 - Tdirp(2,3))
        Tdirp(1,3) = Tdirp(1,1)*Tdirp(2,3)
     &               /(exp2del(2) + 2.D0*DSQRT(dmom)
     &               *DCOS(2.D0*phasep(2)) + dmom*exm2del(2))
        Tabsp(1,2) = Tdirp(1,3)
     &               *(exp2del(2) - (1.D0 - Tdirp(2,3))*exm2del(2)
     &               - Tdirp(2,3))/Tdirp(2,3)
        RETURN
      ENDIF
      IF(NRWel.EQ.2.AND.Ee.LT.VHEigth(2).AND.Ee.GT.VHEigth(4))THEN
        Tabsp(1,2) = 0.D0
        Tabsp(3,2) = 0.D0
        Tabsp(2,3) = 0.D0
        Tdirp(2,3) = 0.D0
        IF(FISbar(Nnuc).EQ.3)THEN
          epsa = VPOs(1) - SQRT(VHEigth(1) - Ee)/(SMIu*VWIdth(1))
          epsb = VPOs(3) + SQRT(VHEigth(3) - Ee)/(SMIu*VWIdth(3))
        ELSE
          epsa = EINters(1)
          epsb = EINters(6)
        ENDIF
        dmom = GAUSSLEGENDRE41(FMOMENT1,epsa,epsb,abserr)
        IF(dmom.GT.0.D0.AND.abserr.GT.dmom*0.03)THEN
          WRITE(*,*)' WARNING: For extremum ', K, 
     &              ' phase integral is not accurate (', 
     &              SNGL(abserr/dmom*100.D0), ' %)'
        ENDIF
        phase_sub = MIN(dmom,50.D0)
        Tdirp(1,2) = 1.D0/(1.D0 + DEXP(2.D0*phase_sub))
        Tdirp(2,1) = Tdirp(1,2)
 
        dmom = (1.D0 - Tdirp(1,2))*(1.D0 - Tdirp(3,3))
        Tdirp(1,3) = Tdirp(1,2)*Tdirp(3,3)
     &               /(exp2del(3) + 2.D0*DSQRT(dmom)
     &               *DCOS(2.D0*phasep(3)) + dmom*exm2del(3))
        Tabsp(1,3) = Tdirp(1,3)
     &               *(exp2del(3) - (1.D0 - Tdirp(3,3))*exm2del(3)
     &               - Tdirp(3,3))/Tdirp(3,3)
        RETURN
      ENDIF
 
C-----direct forward
      DO ih1 = NRHump, 2, -1
        DO ih = ih1 - 1, 1, -1
          dmom = (1.D0 - Tdirp(ih,ih))*(1.D0 - Tdirp(ih + 1,ih1))
          dnum = exp2del(ih + 1) + 2.D0*DSQRT(dmom)
     &           *DCOS(2.D0*phasep(ih + 1)) + dmom*exm2del(ih + 1)
 
          IF(dnum.EQ.0.D0)THEN
            Tdirp(ih,ih1) = 0.D0
          ELSE
            Tdirp(ih,ih1) = Tdirp(ih,ih)*Tdirp(ih + 1,ih1)/dnum
          ENDIF
        ENDDO
      ENDDO
 
C-----direct backward
      DO ih1 = 1, NRHump - 1
        DO ih = ih1 + 1, NRHump - 1
          dmom = (1.D0 - Tdirp(ih,ih))*(1.D0 - Tdirp(ih + 1,ih1))
          Tdirp(ih,ih1) = Tdirp(ih,ih)*Tdirp(ih + 1,ih1)
     &                    /(exp2del(ih + 1) + 2.D0*DSQRT(dmom)
     &                    *DCOS(2.D0*phasep(ih+1))
     &                    + dmom*exm2del(ih + 1))
        ENDDO
      ENDDO
 
C--------Absorption coefficients
C--------forward
      DO iw = 1, NRWel
        DO iw1 = iw + 1, NRWel + 1
          IF(delt(iw1).GT.0.D0)THEN
            dmom = (1.D0 - Tdirp(iw,iw1 - 1))*(1.D0 - Tdirp(iw1,NRHump))
            tdr = Tdirp(iw,iw1 - 1)*Tdirp(iw1,NRHump)
     &            /(exp2del(iw1) + 2.D0*DSQRT(dmom)
     &            *DCOS(2.D0*phasep(iw1)) + dmom*exm2del(iw1))
 
            Tabsp(iw,iw1) = tdr*(exp2del(iw1) - (1.D0 - Tdirp(iw1,NRHump
     &                      ))*exm2del(iw1) - Tdirp(iw1,NRHump))
     &                      /Tdirp(iw1,NRHump)
          ELSE
            Tabsp(iw,iw1) = 0.D0
          ENDIF
        ENDDO
      ENDDO
C------backward
      DO iw = NRWel + 1, 3, -1
        DO iw1 = iw - 1, 2, -1
          IF(delt(iw1).GT.0.D0)THEN
            dmom = (1.D0 - Tdirp(iw - 1,iw1))*(1.D0 - Tdirp(iw1 - 1,1))
            tdr = Tdirp(iw - 1,iw1)*Tdirp(iw1 - 1,1)
     &            /(exp2del(iw1) + 2.D0*DSQRT(dmom)
     &            *DCOS(2.D0*phasep(iw1)) + dmom*exm2del(iw1))
            Tabsp(iw,iw1) = tdr*(exp2del(iw1) - (1.D0 - Tdirp(iw1-1,1))
     &                      *exm2del(iw1) - Tdirp(iw1 - 1,1))
     &                      /Tdirp(iw1 - 1,1)
          ELSE
            Tabsp(iw,iw1) = 0.D0
          ENDIF
        ENDDO
      ENDDO
 
      RETURN
      END SUBROUTINE WKBFIS
 
!---------------------------------------------------------------------------
 
      SUBROUTINE PHASES_PARAB(Ee,Nnuc,Phase,Discrete)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8, DIMENSION(2*NFParab) :: EINters, EJOin
      REAL*8, DIMENSION(NFParab) :: EPSil, HO, HO_p, VBArex, VJJ
      REAL*8 :: FAZa2, SMIu, SMIu_p, UEXc
      INTEGER :: KBArr
      COMMON /INTER / EINters
      COMMON /PARAB / SMIu, EPSil, EJOin, VJJ, HO_p
      COMMON /VARGS / UEXc, SMIu_p, KBArr
      COMMON /VBAR  / VBArex, HO, FAZa2
C
C Dummy arguments
C
      LOGICAL :: Discrete
      REAL*8 :: Ee
      INTEGER :: Nnuc
      REAL*8, DIMENSION(NFParab) :: Phase
C
C Local variables
C
      REAL*8 :: abserr, dmom, es, ftmp, rmiu
      REAL*8 :: DSQRT
      REAL*8, EXTERNAL :: FMOMENTPARAB, GAUSSLEGENDRE41
      INTEGER :: INT
      INTEGER :: j, k
C
C*** End of declarations rewritten by SPAG
C
C     INTEGER iphas_opt
 
      rmiu = 0.054D0*A(Nnuc)**(5.D0/3.D0)        !mu in the formula
      SMIu = DSQRT(0.5D0*rmiu)
      SMIu_p = SMIu
      UEXc = Ee
 
      DO k = 1, NRBar, 2
        VJJ(k) = VBArex(INT(k/2) + 1)
      ENDDO
      DO k = 2, NRBar, 2
        VJJ(k) = VBArex(NRHump + INT(k/2))
      ENDDO
 
      DO k = 1, NRBar
        HO_p(k) = HO(k)
      ENDDO
 
      IF(.NOT.Discrete)THEN
        DO k = 1, NRBar, 2
          VJJ(k) = EFB(INT(k/2) + 1)
          HO(k) = HCOnt(INT(k/2) + 1)
        ENDDO
        DO k = 2, NRBar, 2
          VJJ(k) = EFB(NRHump + INT(k/2))
          HO(k) = HCOnt(NRHump + INT(k/2))
        ENDDO
      ENDIF
 
C-----deformations at saddles and wells and matching points-----------
C     Fission barriers are modelled by NRBar parabolas
C     EPSil(i) are the parabolas vortex
C     EJOin(i) are the corresponding deformation at which parabolas join
      EPSil(1) = SQRT(VJJ(1))/(SMIu*HO(1))
      EJOin(2) = EPSil(1)
     &           + SQRT((VJJ(1) - VJJ(2))/(1.D0 + (HO(1)/HO(2))**2))
     &           /(SMIu*HO(1))
      EJOin(1) = 0.D0
                     !2*EPSil(1) - EJOin(2)
      DO k = 2, NRBar
        EJOin(2*k - 1) = EJOin(2*(k - 1))
        EPSil(k) = EJOin(2*(k - 1)) + (HO(k - 1)/HO(k))
     &             **2*(EJOin(2*(k-1)) - EPSil(k - 1))
        IF(k.LT.NRBar)EJOin(2*k) = EPSil(k)
     &                             + SQRT(( - 1)**k*(VJJ(k+1) - VJJ(k))
     &                             /(1.D0 + (HO(k)/HO(k+1))**2))
     &                             /(SMIu*HO(k))
      ENDDO
      EJOin(2*NRBar) = 2*EPSil(NRBar) - EJOin(2*NRBar - 1)
      EJOin(2*NRBar) = EPSil(NRBar) + SQRT(VJJ(NRBar))/(SMIu*HO(NRBar))
 
      DO j = 1, 2*NRBar
        EINters(j) = -1.
      ENDDO
 
      DO j = 1, NRBar
        ftmp = ( - 1)**j*(Ee - VJJ(j))
        IF(ftmp.GE.0.D0)THEN
          IF(Ee.EQ.VJJ(j))THEN
            EINters(2*j - 1) = EPSil(j)   !Ee
            EINters(2*j) = EPSil(j)   !Ee
            CYCLE
          ENDIF
          es = SQRT(ftmp)/(SMIu*HO(j))
          EINters(2*j - 1) = EPSil(j) - es
          EINters(2*j) = EPSil(j) + es
        ENDIF
      ENDDO
 
C     IF(iphas_opt.NE.0)THEN
      DO j = 2, 2*NRBar - 1, 2
        IF(EINters(j).LT.EJOin(j))THEN
          EINters(j + 1) = EINters(j)
        ELSE
          EINters(j) = EINters(j + 1)
        ENDIF
      ENDDO
C     ENDIF
 
C     Momentum integrals
      UEXc = Ee
      DO k = 1, NRBar
        Phase(k) = 0.D0
        IF(EINters(2*k).GE.0..AND.EINters(2*k - 1).GE.0.)THEN
          dmom = GAUSSLEGENDRE41(FMOMENTPARAB,EINters(2*k - 1),
     &           EINters(2*k),abserr)
        ELSE
          dmom = ( - 1)**(k + 1)*PI*(VJJ(k) - Ee)/HO(k)
        ENDIF
        Phase(k) = MIN(dmom,50.D0)
      ENDDO
 
      RETURN
      END SUBROUTINE PHASES_PARAB
 
!---------------------------------------------------------------------------
C========================================================================
      FUNCTION FMOMENTPARAB(Eps)
C
C     Integrand (To be called from Gauss-Legendre integration routine)
C     To be defined as external function
C
      IMPLICIT NONE
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      INTEGER :: KBArr
      REAL*8 :: SMIu, UEXc
      COMMON /VARGS / UEXc, SMIu, KBArr
C
C Dummy arguments
C
      REAL*8 :: Eps
      REAL*8 :: FMOMENTPARAB
C
C Local variables
C
      REAL*8 :: DABS, DSQRT
      REAL*8 :: VDEF
C
C*** End of declarations rewritten by SPAG
C
 
      FMOMENTPARAB = 2.D0*SMIu*DSQRT(DABS(UEXc - VDEF(Eps)))
      RETURN
      END FUNCTION FMOMENTPARAB
 
!---------------------------------------------------------------------------
 
      FUNCTION VDEF(Eps)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8, DIMENSION(2*NFParab) :: EJOin
      REAL*8, DIMENSION(NFParab) :: EPSil, HO, VJJ
      REAL*8 :: SMIu
      COMMON /PARAB / SMIu, EPSil, EJOin, VJJ, HO
C
C Dummy arguments
C
      REAL*8 :: Eps
      REAL*8 :: VDEF
C
C Local variables
C
      INTEGER :: j
C
C*** End of declarations rewritten by SPAG
C
C COMMON variables
 
 
C
C     , Einters(2*NFPARAB)
C
C     calculation of the deformation potential energy
      VDEF = 0.D0
C
C      Commented by MS, to be adressed in the bright future (post 3.1 Rivoli :-)
C
C      IF (Eps.LE.EJOin(2)) THEN
C         VDEF = VJJ(1) - (SMIu*HO(1)*(Eps - EPSil(1)))**2
C         RETURN
C      ENDIF
C      IF (Eps.GE.EJOin(2*NRBar - 1)) THEN
C         VDEF = VJJ(NRBar) - (SMIu*HO(NRBar)*(Eps - EPSil(NRBar)))**2
C         RETURN
C      ENDIF
C
C      DO j = 2, NRBar - 1
      DO j = 1, NRBar
        IF(Eps.GE.EJOin(2*j - 1).AND.Eps.LE.EJOin(2*j))THEN
          VDEF = VJJ(j) + ( - 1)**j*(SMIu*HO(j)*(Eps - EPSil(j)))**2
          RETURN
        ENDIF
      ENDDO
      RETURN
      END FUNCTION VDEF
 
!---------------------------------------------------------------------------
 
C============NUMERICAL BARRIERS=================================
 
      SUBROUTINE NUMBARR(Nnuc,Vbarex,Ho)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8, DIMENSION(NFIsbarpnt) :: EPS_1d, VDEf_1d
      INTEGER, DIMENSION(0:2*NFParab) :: IIExtr
      INTEGER :: NEXtr, NPOints
      REAL*8, DIMENSION(NFParab) :: VHEigth, VWIdth
      REAL*8, DIMENSION(NFParab + 1) :: VPOs
      COMMON /NUMBAR/ EPS_1d, VDEf_1d, NPOints, IIExtr, NEXtr
      COMMON /NUMBAR1/ VHEigth, VWIdth, VPOs
C
C Dummy arguments
C
      INTEGER :: Nnuc
      REAL*8, DIMENSION(NFParab) :: Ho, Vbarex
C
C Local variables
C
      REAL*8 :: centr, heigth, rmiu, smiu, ucentr, uheigth, uwidth, 
     &          width
      REAL :: defstep
      REAL*8 :: DSQRT
      REAL*8, DIMENSION(2*NFParab) :: ejoin
      REAL*8, DIMENSION(NFParab) :: epsil, vjj
      INTEGER :: FIND_EXTREM
      INTEGER :: id, j, k, nrsm
      INTEGER :: INT
C
C*** End of declarations rewritten by SPAG
C
 
 
 
 
 
C     Functions
 
      rmiu = 0.054D0*A(Nnuc)**(5.D0/3.D0)
      smiu = DSQRT(0.5D0*rmiu)
      nrsm = NRSmooth(Nnuc)
 
C-----Generating numerical barrier starting from parabolas' parameters
      IF(FISbar(Nnuc).LE.2.)THEN
        DO k = 1, NRBar, 2
          vjj(k) = Vbarex(INT(k/2) + 1)
        ENDDO
        DO k = 2, NRBar, 2
          vjj(k) = Vbarex(NRHump + INT(k/2))
        ENDDO
 
C--------deformations at saddles and wells and matching points-----------
C        Fission barriers are modelled by NRBar parabolas
C        EPSil(i) are the parabolas vortex
C        EJOin(i) are the corresponding deformation at which parabolas join
        epsil(1) = SQRT(vjj(1))/(smiu*Ho(1))
        ejoin(2) = epsil(1)
     &             + SQRT((vjj(1) - vjj(2))/(1.D0 + (Ho(1)/Ho(2))**2))
     &             /(smiu*Ho(1))
        ejoin(1) = 2*epsil(1) - ejoin(2)
        DO k = 2, NRBar
          ejoin(2*k - 1) = ejoin(2*(k - 1))
          epsil(k) = ejoin(2*(k - 1)) + (Ho(k - 1)/Ho(k))
     &               **2*(ejoin(2*(k-1)) - epsil(k - 1))
          IF(k.LT.NRBar)ejoin(2*k) = epsil(k)
     &                               + SQRT(( - 1)**k*(vjj(k+1) - vjj(k)
     &                               )/(1.D0 + (Ho(k)/Ho(k+1))**2))
     &                               /(smiu*Ho(k))
        ENDDO
        ejoin(2*NRBar) = 2*epsil(NRBar) - ejoin(2*NRBar - 1)
        defstep = 0.01D0
        NPOints = INT((epsil(NRBar) + SQRT(vjj(NRBar))/(smiu*Ho(NRBar)))
     &            /defstep)
 
        DO id = 1, NPOints
          EPS_1d(id) = defstep*id
          IF(EPS_1d(id).LE.ejoin(2))VDEf_1d(id) = vjj(1)
     &       - (smiu*Ho(1)*(EPS_1d(id) - epsil(1)))**2
          IF(EPS_1d(id).GE.ejoin(2*NRBar - 1))VDEf_1d(id) = vjj(NRBar)
     &       - (smiu*Ho(NRBar)*(EPS_1d(id) - epsil(NRBar)))**2
          DO j = 2, NRBar - 1
            IF(EPS_1d(id).GE.ejoin(2*j - 1).AND.EPS_1d(id).LE.ejoin(2*j)
     &         )VDEf_1d(id) = vjj(j) + ( - 1)
     &                        **j*(smiu*Ho(j)*(EPS_1d(id) - epsil(j)))
     &                        **2
          ENDDO
        ENDDO
      ENDIF
 
C-----Finding maxima and minima of the deformation energy curve
C                   initializes iiextr() and nextr
      NEXtr = FIND_EXTREM(Nnuc)
      DO j = 1, NEXtr
        VPOs(j) = DEFfis(j)
      ENDDO
      VPOs(NEXtr + 1) = 100.D0
C==================================================================
C     Fitting parabola
      DO j = 1, NEXtr
        CALL PARABFIT(IIExtr(j),nrsm,rmiu,EPS_1d,VDEf_1d,centr,heigth,
     &                width,ucentr,uheigth,uwidth)
        IF(width.LT.0.05D0)CYCLE       ! Skipping very narrow peaks
C       write(*,*) ' Def: ',sngl(EPS_1d(iiextr(j))),
C     &         ' (',sngl(centr),' +/- ',sngl(ucentr),')'
C       write(*,*) ' Heigth :',sngl(Vdef_1d(iiextr(j))),
C     &         ' (',sngl(heigth),' +/- ',sngl(uheigth),')'
C       write(*,*) ' Width :', sngl(width), ' +/- ', sngl(uwidth)
C--------------------------------------------------
C       Initializes parabola's parameters
C
C       The real height and position of the barrier are used
C       (not the fitted parabola height or center)
 
        NEXtr = NRBar
        VHEigth(j) = VDEf_1d(IIExtr(j))
        VWIdth(j) = width
        VPOs(j) = EPS_1d(IIExtr(j))
      ENDDO
C     Vpos(0) = 0.d0
      VPOs(NEXtr + 1) = 100.D0
 
      RETURN
      END SUBROUTINE NUMBARR
 
!---------------------------------------------------------------------------
 
C================================================================
      SUBROUTINE PHASES(Uexcit1,Phase,Phase_h,Nnuc,Iphas_opt,Discrete)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8, DIMENSION(NFIsbarpnt) :: EPS_1d, VDEf_1d
      INTEGER, DIMENSION(0:2*NFParab) :: IIExtr
      INTEGER :: K, NEXtr, NPOints
      REAL*8 :: SMIu, UEXc
      REAL*8, DIMENSION(NFParab) :: VHEigth, VWIdth
      REAL*8, DIMENSION(NFParab + 1) :: VPOs
      COMMON /NUMBAR/ EPS_1d, VDEf_1d, NPOints, IIExtr, NEXtr
      COMMON /NUMBAR1/ VHEigth, VWIdth, VPOs
      COMMON /VARGS / UEXc, SMIu, K
C
C Dummy arguments
C
      LOGICAL :: Discrete
      INTEGER :: Iphas_opt, Nnuc
      REAL*8 :: Uexcit1
      REAL*8, DIMENSION(2*NFParab) :: Phase
      REAL*8, DIMENSION(NFParab) :: Phase_h
C
C Local variables
C
      REAL*8 :: abserr, dmom, epsa, epsb, rmiu
      REAL*8 :: DSQRT
      REAL*8 :: FINDINTERSECT, FMOMENT1, GAUSSLEGENDRE41
      INTEGER :: i
      INTEGER :: INT
      ! REAL*8, DIMENSION(NFParab) :: phase_w
      REAL :: SNGL
C
C*** End of declarations rewritten by SPAG
C
C================================================================
 
 
 
C-------------------------------------------------------------------
C     FUNCTIONS
 
      rmiu = 0.054D0*A(Nnuc)**(5.D0/3.D0)
      UEXc = Uexcit1
      SMIu = DSQRT(0.5D0*rmiu)
C
      DO K = 1, 2*NRBar
        Phase(K) = 0.D0
      ENDDO
 
      IF(.NOT.Discrete)THEN
        i = 0
        DO K = 1, NRHump
          VHEigth(K + i) = EFB(K)
          VWIdth(K + i) = HCOnt(K)
          i = i + 1
        ENDDO
        i = 1
        DO K = NRHump + 1, NRBar
          VHEigth(K - NRHump + i) = EFB(K)
          VWIdth(K - NRHump + i) = HCOnt(K)
          i = i + 1
        ENDDO
      ENDIF
 
C---- Momentum integrals are calculated
      IF(Iphas_opt.EQ.1)THEN
        DO K = 1, NEXtr                                   ! humps and wells
          IF(MOD(K,2).EQ.1)THEN                           ! humps
            IF(Uexcit1.GE.VHEigth(K))THEN                 ! Hill-Wheeler formula
              dmom = PI*(VHEigth(K) - Uexcit1)/VWIdth(K)
              Phase(K) = MIN(dmom,50.D0)
            ELSE                                          !WKB
              epsa = FINDINTERSECT(Uexcit1,IIExtr(K - 1),IIExtr(K),
     &               .FALSE.)
              epsb = FINDINTERSECT(Uexcit1,IIExtr(K),IIExtr(K + 1),
     &               .FALSE.)
              dmom = GAUSSLEGENDRE41(FMOMENT1,epsa,epsb,abserr)
              IF(dmom.GT.0.D0.AND.abserr.GT.dmom*0.053)THEN
                WRITE(*,*)' WARNING: For extremum ', K, 
     &                    ' phase integral is not accurate (', 
     &                    SNGL(abserr/dmom*100.D0), ' %)', Uexcit1, 
     &                    VHEigth(K), VWIdth(K), Discrete
              ENDIF
              Phase(K) = MIN(dmom,50.D0)
            ENDIF
          ELSEIF(Uexcit1.LE.VHEigth(K))THEN
            Phase(K) = 0.D0
          ELSE                                            ! WKB
            epsa = FINDINTERSECT(Uexcit1,IIExtr(K - 1),IIExtr(K),.TRUE.)
            epsb = FINDINTERSECT(Uexcit1,IIExtr(K),IIExtr(K + 1),.TRUE.)
            dmom = GAUSSLEGENDRE41(FMOMENT1,epsa,epsb,abserr)
            IF(dmom.GT.0.D0.AND.abserr.GT.dmom*0.053)THEN
              WRITE(*,*)' WARNING: For extremum ', K, 
     &                  ' phase integral is not accurate (', 
     &                  SNGL(abserr/dmom*100.D0), ' %)', Uexcit1, 
     &                  VHEigth(K), VWIdth(K), Discrete
            ENDIF
            Phase(K) = MIN(dmom,50.D0)
          ENDIF
        ENDDO
      ELSE
        DO K = 1, NEXtr                              ! only parabolic barriers
          dmom = PI*(VHEigth(K) - Uexcit1)/VWIdth(K) !Hill-Wheeler
          Phase(K) = MIN(dmom,50.D0)
        ENDDO
      ENDIF
C
      DO K = 1, NEXtr
        IF(MOD(K,2).EQ.1)THEN
          Phase_h(K - INT(K/2)) = Phase(K)
        !ELSE
        !  phase_w(K - INT(K/2)) = Phase(K)
        ENDIF
      ENDDO
 
      RETURN
      END SUBROUTINE PHASES
 
!---------------------------------------------------------------------------
 
      FUNCTION FMOMENT1(Eps)
C
C     Integrand (To be called from Gauss-Legendre integration routine)
C
C     To be defined as external function
C
      IMPLICIT NONE
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      INTEGER :: KBArr
      REAL*8 :: SMIu, UEXc
      COMMON /VARGS / UEXc, SMIu, KBArr
C
C Dummy arguments
C
      REAL*8 :: Eps
      REAL*8 :: FMOMENT1
C
C Local variables
C
      REAL*8 :: DABS, DSQRT
      REAL*8 :: VDEF1
C
C*** End of declarations rewritten by SPAG
C
      FMOMENT1 = 2.D0*SMIu*DSQRT(DABS(UEXc - VDEF1(Eps)))
      RETURN
      END FUNCTION FMOMENT1
 
!---------------------------------------------------------------------------
 
      FUNCTION VDEF1(Eps)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8, DIMENSION(NFIsbarpnt) :: EPS_1d, VDEf_1d
      INTEGER, DIMENSION(0:2*NFParab) :: IIExtr
      INTEGER :: NEXtr, NPOints
      COMMON /NUMBAR/ EPS_1d, VDEf_1d, NPOints, IIExtr, NEXtr
C
C Dummy arguments
C
      REAL*8 :: Eps
      REAL*8 :: VDEF1
C
C Local variables
C
      REAL*8 :: ei, eip, vi, vip
      INTEGER :: idef
C
C*** End of declarations rewritten by SPAG
C
C
C     This function calculates real shape of the deformation energy
C     by linear interpolation to obtain the value of the barrier Vdef
C     at deformation EPS (needed to integrte this function)
C
C     Called by gaussian integration
C
C
C     Local variables
C
C     The four lines below is a very simple (and unefficient) search
C     Should be replaced by efficient search routine to locate the element idef of the array EPS_1D()
      idef = 1
      DO WHILE (Eps.GT.EPS_1d(idef).AND.idef.LE.NPOints)
        idef = idef + 1
      ENDDO
      IF(idef.NE.1)idef = idef - 1
 
      vi = VDEf_1d(idef)
      vip = VDEf_1d(idef + 1)
      ei = EPS_1d(idef)
      eip = EPS_1d(idef + 1)
 
      IF(ei.EQ.eip)THEN
C       Special case treated here to avoid division by zero
C       We assume that in this case vi = vip
        VDEF1 = vi
        RETURN
      ENDIF
 
      VDEF1 = vi + (Eps - ei)/(eip - ei)*(vip - vi)
      RETURN
      END FUNCTION VDEF1
 
!---------------------------------------------------------------------------
 
C===================================================
      SUBROUTINE PARABFIT(Imax,Npfit,Rmiu,Epss,Vdeff,Centr,Heigth,Width,
     &                    Ucentr,Uheigth,Uwidth)
      IMPLICIT NONE
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      REAL*8 :: Centr, Heigth, Rmiu, Ucentr, Uheigth, Uwidth, Width
      INTEGER :: Imax, Npfit
      REAL*8, DIMENSION(*) :: Epss, Vdeff
C
C Local variables
C
      REAL*8, DIMENSION(3) :: aa, siga
      REAL*8 :: chisqr
      INTEGER :: i, ierr, npts
      REAL*8, DIMENSION(1024) :: x, y
C
C*** End of declarations rewritten by SPAG
C
                    ! Position of the maximum
                    ! Number of points to fit
                    ! MIU = 0.054d0*ANUC**(5.d0/3.d0)
                     ! Deformation Array        (X)
                     ! Deformation Energy Array (Y)
                                     ! BARRIER'S PARAMETER
 
C     Local variables
 
      npts = 0                       ! BARRIER'S PARAMETER UNCERTAINTIES
 
      DO i = Imax - Npfit, Imax + Npfit
        npts = npts + 1
        x(npts) = Epss(i) - Epss(Imax)
        y(npts) = Vdeff(i)
      ENDDO
 
 
C
C FITS A POLYNOMIAL OF THE FORM:
C   Y=A(1)+A(2)*X+A(3)*X**2+...+A(NTERMS)*X**(NTERMS-1)
C THOUGH THE NPTS POINTS (X,Y)
C IERR=0 OK
C IERR=2 SINGULAR MATRIX
C IERR=3 REQUIREMENTS FOR USE OF THIS ROUTINE NOT FULLFILLED
C
      CALL WPLFT(npts,3,ierr,1,x,y,aa,siga,chisqr)
C     do i = 1,npts
C       write(*,'(1x,F5.3,2x,d15.8,3x,d15.8)') x(i), y(i),
C    &              a(1)+a(2)*x(i)+a(3)*x(i)**2
C     enddo
C     write(*,*) a(1),a(2),a(3)
C     write(*,*) sigmaa(1),sigmaa(2),sigmaa(3)
C
C      write(*,*) ' PARABOLIC FITTING CHISQR = ',sngl(CHISQR)
 
      Heigth = aa(1)
      Uheigth = siga(1)
      Width = SQRT(2.D0*ABS(aa(3))/Rmiu)
      Uwidth = SQRT(2.D0*ABS(siga(3))/Rmiu)
      Centr = Epss(Imax)
      IF(aa(3).NE.0.D0)Centr = 0.5D0*aa(2)/aa(3)
      Ucentr = ABS(Centr)*SQRT((siga(2)/aa(2))**2 + (siga(3)/aa(3))**2)
      Centr = Epss(Imax) + Centr
      RETURN
      END SUBROUTINE PARABFIT
 
!---------------------------------------------------------------------------
 
      SUBROUTINE WPLFT(Npts,Nterms,Ierr,Mode,X,Y,Aa,Sigmaa,Chisqr)
C
C*** Start of declarations rewritten by SPAG
C
C PARAMETER definitions
C
      INTEGER, PARAMETER :: MAXdeg = 3, WEIght = 0
C
C Dummy arguments
C
      REAL*8 :: Chisqr
      INTEGER :: Ierr, Mode, Npts, Nterms
      REAL*8, DIMENSION(Nterms) :: Aa, Sigmaa
      REAL*8, DIMENSION(Npts) :: X, Y
C
C Local variables
C
      REAL*8, DIMENSION(MAXdeg,MAXdeg) :: array
      REAL*8 :: chisq, det, fnpts, free1, sigma, sumx, varnce, wmean, 
     &          ymean
      REAL*8 :: DBLE, DSQRT
      INTEGER :: deg, i, j, k
      REAL :: freen, yfit
      REAL*8, DIMENSION(MAXdeg) :: r, sigmax, xmean
      REAL*8, DIMENSION(Npts) :: w
C
C*** End of declarations rewritten by SPAG
C
 
C FITS A POLYNOMIAL OF THE FORM:
C   Y=A(1)+A(2)*X+A(3)*X**2+...+A(NTERMS)*X**(NTERMS-1)
C THOUGH THE NTPS POINTS (X,Y)
C MODE 0 = WEIGTH 0 != UNWEIGHTED
C IERR=0 OK
C IERR=2 SINGULAR MATRIX
C IERR=3 REQUIREMENTS FOR USE OF THIS ROUTINE NOT FULLFILLED
C REQUIREMENTS ARE:
C        MAX NUM TERMS = 12
C        MIN NUM TERMS = 2
C        NTPS >= NTERMS
 
 
 
      Ierr = 0
      deg = Nterms - 1
      IF(deg.LT.1.OR.deg.GT.MAXdeg.OR.Npts.LT.Nterms)THEN
        Ierr = 3
        RETURN
      ENDIF
 
C       INITIALIZE SUMS AND ARRAYS

      sumx = 0.D0
      ymean = 0.D0
      sigma = 0.D0
      chisq = 0.D0
      Aa(1) = 0.
      Sigmaa(1) = 0.
      DO j = 1, deg
        xmean(j) = 0.D0
        sigmax(j) = 0.D0
        r(j) = 0.D0
        Aa(1 + j) = 0.
        Sigmaa(1 + j) = 0.
C       DO 28 K=1,DEG
C28      ARRAY(J,K)=0.D0
        DO k = 1, Nterms
          array(j,k) = 0.D0
        ENDDO
      ENDDO
 
 
C       ACCUMULATE WEIGHTED SUMS
 
      DO i = 1, Npts
        w(i) = ABS(Y(i))
        IF(Mode.NE.WEIght)w(i) = 1.
        sumx = sumx + DBLE(w(i))
C       YMEAN = Sy
        ymean = ymean + DBLE(w(i)*Y(i))
C       XMEAN(1)=Sx, XMEAN(2)=Sxx
C       S=NPTS*(NPTS+1)/2
        DO j = 1, deg
          xmean(j) = xmean(j) + DBLE(w(i)*X(i)**j)
        ENDDO
      ENDDO
      ymean = ymean/sumx
      DO j = 1, deg
        xmean(j) = xmean(j)/sumx
      ENDDO
      fnpts = Npts
      wmean = sumx/fnpts
      DO i = 1, Npts
        w(i) = w(i)/wmean
      ENDDO
 
      DO i = 1, Npts
        sigma = sigma + DBLE(w(i)*(Y(i) - ymean)**2)
        DO j = 1, deg
          sigmax(j) = sigmax(j) + DBLE(w(i)*(X(i)**j - xmean(j))**2)
          r(j) = r(j) + DBLE(w(i)*(X(i)**j - xmean(j))*(Y(i) - ymean))
          DO k = 1, j
            array(j,k) = array(j,k)
     &                   + DBLE(w(i)*(X(i)**j - xmean(j))*(X(i)
     &                   **k - xmean(k)))
          ENDDO
        ENDDO
      ENDDO
      free1 = Npts - 1
      sigma = DSQRT(sigma/free1)
      DO j = 1, deg
        sigmax(j) = DSQRT(sigmax(j)/free1)
        r(j) = r(j)/(free1*sigmax(j)*sigma)
        DO k = 1, j
          array(j,k) = array(j,k)/(free1*sigmax(j)*sigmax(k))
          array(k,j) = array(j,k)
        ENDDO
      ENDDO
 
C       INVERT SYMMETRIC MATRIX
 
      CALL MATINVM(deg,det,array)
      IF(det.EQ.0.D0)THEN
        Ierr = 2
        RETURN
      ENDIF
 
C       CALCULATE COEFFICIENTS
 
      Aa(1) = ymean
      DO j = 1, deg
        DO k = 1, deg
          Aa(j + 1) = Aa(j + 1) + r(k)*array(j,k)
        ENDDO
        Aa(j + 1) = Aa(j + 1)*sigma/sigmax(j)
        Aa(1) = Aa(1) - Aa(j + 1)*xmean(j)
      ENDDO
 
      DO i = 1, Npts
        yfit = Aa(1)
        DO j = 1, deg
          yfit = yfit + Aa(j + 1)*X(i)**j
        ENDDO
        chisq = chisq + w(i)*(Y(i) - yfit)**2
      ENDDO
      freen = Npts - Nterms
      IF(freen.EQ.0)freen = 1.
      Chisqr = chisq*wmean/freen
 
C       CALCULATE UNCERTAINTIES
 
      IF(Mode.EQ.WEIght)THEN
        varnce = 1./wmean
      ELSE
        varnce = Chisqr
      ENDIF
      DO j = 1, deg
        Sigmaa(1 + j) = array(j,j)*varnce/(free1*sigmax(j)**2)
        Sigmaa(1 + j) = SQRT(Sigmaa(1 + j))
      ENDDO
      Sigmaa(1) = varnce/fnpts
      DO j = 1, deg
        DO k = 1, deg
          Sigmaa(1) = Sigmaa(1) + varnce*xmean(j)*xmean(k)*array(j,k)
     &                /(free1*sigmax(j)*sigmax(k))
        ENDDO
      ENDDO
 
      Sigmaa(1) = SQRT(Sigmaa(1))
      DO j = 1, deg
        DO k = 1, deg
          array(j,k) = array(j,k)*varnce/(free1*sigmax(j)*sigmax(k))
        ENDDO
      ENDDO
 
      RETURN
      END SUBROUTINE WPLFT
 
!---------------------------------------------------------------------------
C++++++++++++++++++++++++++++
 
      SUBROUTINE MATINVM(Norder,Det,Array)
C
C*** Start of declarations rewritten by SPAG
C
C PARAMETER definitions
C
      INTEGER, PARAMETER :: MAXdeg = 3
C
C Dummy arguments
C
      REAL*8 :: Det
      INTEGER :: Norder
      REAL*8, DIMENSION(MAXdeg,MAXdeg) :: Array
C
C Local variables
C
      REAL*8 :: amax, savx
      REAL*8 :: DABS
      INTEGER :: i, j, k, l
      INTEGER, DIMENSION(MAXdeg) :: ik, jk
C
C*** End of declarations rewritten by SPAG
C
 
      Det = 1.D0
      DO k = 1, Norder
 
C       FIND LARGEST ELEMENT ARRAY(I,J) IN REST OF MATRIX
 
        amax = 0.D0
    5   DO i = k, Norder
          DO j = k, Norder
            IF(DABS(amax).LE.DABS(Array(i,j)))THEN
              amax = Array(i,j)
              ik(k) = i
              jk(k) = j
            ENDIF
          ENDDO
        ENDDO
 
C       INTERCHANGE ROWS AND COLUMNS TO PUT AMAX IN ARRAY(K,K)
 
        IF(amax.NE.0.D0)THEN
          i = ik(k)
          IF(i.LT.k)GOTO 5
          IF(i.NE.k)THEN
            DO j = 1, Norder
              savx = Array(k,j)
              Array(k,j) = Array(i,j)
              Array(i,j) = -savx
            ENDDO
          ENDIF
          j = jk(k)
          IF(j.LT.k)GOTO 5
          IF(j.NE.k)THEN
            DO i = 1, Norder
              savx = Array(i,k)
              Array(i,k) = Array(i,j)
              Array(i,j) = -savx
            ENDDO
          ENDIF
 
C       ACCUMULATE ELEMENTS OF INVERSE MATRIX
 
          DO i = 1, Norder
            IF(i.NE.k)THEN
              Array(i,k) = -Array(i,k)/amax
            ENDIF
          ENDDO
          DO i = 1, Norder
            DO j = 1, Norder
              IF(i.NE.k)THEN
                IF(j.NE.k)THEN
                  Array(i,j) = Array(i,j) + Array(i,k)*Array(k,j)
                ENDIF
              ENDIF
            ENDDO
          ENDDO
          DO j = 1, Norder
            IF(j.NE.k)THEN
              Array(k,j) = Array(k,j)/amax
            ENDIF
          ENDDO
          Array(k,k) = 1./amax
          Det = Det*amax
        ELSE
          Det = 0.D0
          RETURN
        ENDIF
      ENDDO
 
C       RESTORE ORDENING OF MATRIX
 
      DO l = 1, Norder
        k = Norder - l + 1
        j = ik(k)
        IF(j.GT.k)THEN
          DO i = 1, Norder
            savx = Array(i,k)
            Array(i,k) = -Array(i,j)
            Array(i,j) = savx
          ENDDO
        ENDIF
        i = jk(k)
        IF(i.GT.k)THEN
          DO j = 1, Norder
            savx = Array(k,j)
            Array(k,j) = -Array(i,j)
            Array(i,j) = savx
          ENDDO
        ENDIF
      ENDDO

      RETURN
      END SUBROUTINE MATINVM
 
!---------------------------------------------------------------------------
 
      FUNCTION FINDINTERSECT(Uexc,Ja,Jb,Iswell)
      INCLUDE 'dimension.h'
      ! INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8, DIMENSION(NFIsbarpnt) :: EPS_1d, VDEf_1d
      INTEGER, DIMENSION(0:2*NFParab) :: IIExtr
      INTEGER :: NEXtr, NPOints
      COMMON /NUMBAR/ EPS_1d, VDEf_1d, NPOints, IIExtr, NEXtr
C
C Dummy arguments
C
      LOGICAL :: Iswell
      INTEGER :: Ja, Jb
      REAL*8 :: Uexc
      REAL*8 :: FINDINTERSECT
C
C Local variables
C
      INTEGER :: is0, is1, j
      REAL*8 :: slope
C
C*** End of declarations rewritten by SPAG
C
C
C     Calculates solutions (beta2(i)) of the equation
C         V(beta2) = Excitation Energy
C
C     If solution not found, then assign first or last point
C     of the interval depending on whether we are solving the equation
C     in the rigth(left) side of the barrier(well) case
 
      is0 = -1
      IF(ABS(Uexc - VDEf_1d(Ja)).EQ.Uexc - VDEf_1d(Ja)) is0 = 1
      DO j = Ja, Jb
C      checking for a sign change in Uexc-Vdef
        is1 = -1
        IF(ABS(Uexc - VDEf_1d(j)).EQ.Uexc - VDEf_1d(j)) is1 = 1
        IF(is1.EQ.is0)CYCLE
C      Sign of (Uexc-Vdef_1d(j)) changed, calculating the precise value
C      of the deformation EPS at which Uexc = Vdef
        FINDINTERSECT = EPS_1d(j - 1) + (EPS_1d(j) - EPS_1d(j - 1))
     &                  *(Uexc - VDEf_1d(j - 1))
     &                  /(VDEf_1d(j) - VDEf_1d(j - 1))
        RETURN
      ENDDO
C
C     Below is the analysis if intersection not found in [ja,jb]
C
      slope = VDEf_1d(Jb) - VDEf_1d(Ja)
      IF(Iswell)THEN
C       WELLS
        IF(slope.GE.0)THEN
          FINDINTERSECT = EPS_1d(Jb) ! ascending
        ELSE
          FINDINTERSECT = EPS_1d(Ja) ! descending
        ENDIF
C       BARRIERS
      ELSEIF(slope.GE.0)THEN
        FINDINTERSECT = EPS_1d(Ja)   ! ascending
      ELSE
        FINDINTERSECT = EPS_1d(Jb)   ! descending
      ENDIF

      RETURN
      END FUNCTION FINDINTERSECT
 
!---------------------------------------------------------------------------
 
      REAL*8 FUNCTION GAUSSLEGENDRE41(F,Ea,Eb,Abserr)
      IMPLICIT NONE
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      REAL*8 :: Abserr, Ea, Eb
      REAL*8, EXTERNAL :: F
C
C Local variables
C
      REAL*8 :: absc, abscm1, centr1, fsum, hlgth1, resg1, resk1
      INTEGER :: j, jtw, jtwm1
      REAL*8, DIMENSION(10) :: wg
      REAL*8, DIMENSION(21) :: wgk, xgk
C
C*** End of declarations rewritten by SPAG
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
C     Integrating from Ea to Eb, converting to symmetric grid from -1 to +1
      centr1 = 0.5D+00*(Ea + Eb)
      hlgth1 = 0.5D+00*(Eb - Ea)
C
C     COMPUTE THE 41-POINT GAUSS-KRONROD APPROXIMATION TO
C     THE INTEGRAL, AND ESTIMATE THE ABSOLUTE ERROR USING ONLY 21 points
C
      resg1 = 0.0D+00
      resk1 = wgk(21)*F(centr1)
      DO j = 1, 10
        jtw = j*2
        jtwm1 = jtw - 1
        absc = hlgth1*xgk(jtw)
        fsum = F(centr1 - absc) + F(centr1 + absc)
        abscm1 = hlgth1*xgk(jtwm1)
        resg1 = resg1 + wg(j)*fsum
        resk1 = resk1 + wgk(jtw)*fsum + wgk(jtwm1)
     &          *(F(centr1 - abscm1) + F(centr1 + abscm1))
      ENDDO
 
      GAUSSLEGENDRE41 = resk1*hlgth1
      Abserr = ABS((resk1 - resg1)*hlgth1)
 
      RETURN
      END FUNCTION GAUSSLEGENDRE41
