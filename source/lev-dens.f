Ccc   * $Author: shoblit $
Ccc   * $Date: 2012-02-09 21:34:11 +0100 (Do, 09 Feb 2012) $
Ccc   * $Id: lev-dens.f,v 1.77 2009/08/03 00:35:20 Capote Exp $
C
C
C
      SUBROUTINE ROEMP(Nnuc,Cf,Asaf)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: A2, A23, ACR, ACRt, AP1, AP2, ATIl, BF, DEL, DELp, 
     &          DETcrt, ECOnd, GAMma, SCR, TCRt, UCRt
      INTEGER :: NLWst
      COMMON /CRIT  / TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl
      COMMON /PARAM / AP1, AP2, GAMma, DEL, DELp, BF, A23, A2, NLWst
C
C Dummy arguments
C
      REAL*8 :: Asaf, Cf
      INTEGER :: Nnuc
C
C Local variables
C
      REAL*8 :: aj, defit, dshif, dshift, ecrt, ellq, pi2, rotemp
      INTEGER :: ia, iz, kk, nplot
C
C*** End of declarations rewritten by SPAG
C
CCC
CCC   *****************************************************************
CCC   *                                                      CLASS:PPU*
CCC   *                         R O E M P                             *
CCC   *                                                               *
CCC   *                                                               *
CCC   * CALCULATES TABLE OF ENERGY AND SPIN DEPENDENT LEVEL DENSITIES *
CCC   *                                                               *
CCC   * INPUT:                                                        *
CCC   *  NNUC - index of the nucleus                                  *
CCC   *  CF   - 1. for the saddle point, 0. otherwise                 *
CCC   *  ASAF - controls a=parameter at a saddle point                *
CCC   *       - if ASAF.GE.0 it corresponds to the gamma-param.       *
CCC   *         in the Ignatyuk formula (ASAF=0 selects               *
CCC   *         asymptotic value for a)                               *
CCC   *       - if ASAF.lt.0 asymptotic value of a-parameter          *
CCC   *         times ABS(ASAF) is taken for at the saddle point      *
CCC   *  BF controls shape of the nucleus                             *
CCC   *     BF=0. stands for the saddle point                         *
CCC   *     BF=1. stands for the oblate   yrast state                 *
CCC   *     BF=2. stands for the prolate  yrast state                 *
CCC   *     BF=3. stands for the triaxial yrast state                 *
CCC   *       SCUTF - SPIN CUT-OFF FACTOR (0.146 IS RECOMMENDED)      *
CCC   *                                                               *
CCC   * OUTPUT:RO(.,.,NNUC) - LEVEL DENSITIES                         *
CCC   *       DAMIRO                                                  *
CCC   *                                                               *
CCC   *****************************************************************
CCC
                                                                ! CRIT
                                                                ! PARAM
 
 
 
      pi2 = PI*PI                                               ! PARAM
      BF = 1.0
      IF(Cf.NE.0.0D0)BF = 0.0D0
      A23 = A(Nnuc)**0.666667D0
      ia = INT(A(Nnuc))
      iz = INT(Z(Nnuc))
 
      IF(NEX(Nnuc).LE.0.0D0.AND.FITlev.EQ.0)THEN
        WRITE(8,
     &'('' EXCITATION ENERGY TABLE FOR A='',I3,'' Z='',I3,         ''   
     & HAS NOT BEEN DETERMINED BEFORE CALL OF PRERO'',//   ,'' LEVEL DEN
     &SITIES WILL NOT BE CALCULATED'')')ia, iz
        RETURN
      ENDIF
 
      IF(EX(NEX(Nnuc),Nnuc).LE.0.0D0.AND.FITlev.EQ.0)RETURN
      CALL PRERORITO(Nnuc)
      NLWst = NLW
 
      IF(FISshi(Nnuc).EQ.1.D0.OR.AEJc(0).GT.4)CALL PRERO(Nnuc)
 
C-----set level density parameter systematics
C-----EMPIRE-3.0-dependence
      CALL EGSMSYS(AP1,AP2,GAMma,DEL,DELp,Nnuc)
      IF(BF.EQ.0.0D0.AND.Asaf.GE.0.0D0)GAMma = Asaf
      ATIl = AP1*FLOAT(ia) + AP2*A23
      ATIl = ATIl*ATIlnor(Nnuc)
C-----calculate crtical values
      CALL DAMIRO_CRT(ia,iz,SHC(Nnuc),IOUt,0)
      IF(BF.EQ.0.D0.AND.Asaf.LT.0.0D0)ACR = ACRt
C-----fit of cumulative low-lying discrete levels
      IF(BF.NE.0.D0)CALL LEVFIT(Nnuc,nplot,dshif,dshift,defit)
      IF(IOUt.EQ.6.AND.NLV(Nnuc).GT.3)
     &   CALL PLOT_ZVV_NUMCUMUL(Nnuc,defit,nplot,NLWst)
      IF(FITlev.GT.0.0D0.AND.NLV(Nnuc).GT.3)
     &   CALL PLOT_GNU_NUMCUMUL(Nnuc,nplot,defit,dshift,DEL,NLWst)
 
      IF(Q(1,Nnuc).EQ.0.0D0)THEN
        REWIND(25)
        CALL BNDG(1,Nnuc,Q(1,Nnuc))
      ENDIF
 
C     Ecrt = UCRt - DEL - dshift
      ecrt = UCRt - DEL
      IF(ecrt.LT.Q(1,Nnuc))THEN
        ellq = ecrt - (ELV(NLV(Nnuc),Nnuc) + LDShif(Nnuc))
      ELSE
        ellq = Q(1,Nnuc) - (ELV(NLV(Nnuc),Nnuc) + LDShif(Nnuc))
        IF(FIRst_ein.AND.Nnuc.EQ.1)THEN
          WRITE(8,*)
          WRITE(8,*)
     &' WARNING: Ecrt= Ucrt-DEL > Bn, Calculated D0 may be not accurate'
          WRITE(8,*)' WARNING: Tuning of the CN ATILNO may be required'
          WRITE(8,*)
        ENDIF
      ENDIF
 
      DO kk = 1, NEX(Nnuc)
        IF(FITlev.LE.0.0D0.OR.EX(kk,Nnuc).GE.ELV(NLV(Nnuc),Nnuc)
     &     + LDShif(Nnuc))THEN
 
          IF(ecrt.LT.Q(1,Nnuc))THEN
            IF(EX(kk,Nnuc).LT.ecrt.AND.ellq.NE.0.0D0)THEN
              dshif = dshift*(ecrt - EX(kk,Nnuc))/ellq
            ELSE
              dshif = 0.D0
            ENDIF
          ELSEIF(EX(kk,Nnuc).LT.Q(1,Nnuc).AND.ellq.NE.0.0D0)THEN
            dshif = dshift*(Q(1,Nnuc) - EX(kk,Nnuc))/ellq
          ELSE
            dshif = 0.D0
          ENDIF
 
          IF(BF.EQ.0.0D0)THEN
            CALL DAMIRO_FISHI(kk,Nnuc,Asaf,rotemp,aj)
          ELSE
            CALL DAMIRO(kk,Nnuc,dshif,0.0D0,rotemp,aj)
          ENDIF
        ENDIF
      ENDDO
 
      IF(IOUt.EQ.6)CALL PLOT_ZVV_GSLD(Nnuc)
 
C     Added INITIALIZATION for ROPar(1,Nnuc) and ROPar(3,Nnuc)
      ROPar(1,Nnuc) = ACR
      ROPar(3,Nnuc) = DEL
 
 
      RETURN
      END SUBROUTINE ROEMP
 
!---------------------------------------------------------------------------
 
CCC
CCC   *****************************************************************
      SUBROUTINE DAMIRO(Kk,Nnuc,Dshif,Destep,Rotemp,Aj)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: A2, A23, ACR, ACRt, AP1, AP2, ATIl, BF, DEL, DELp, 
     &          DETcrt, ECOnd, GAMma, SCR, TCRt, UCRt
      INTEGER :: NLWst
      COMMON /CRIT  / TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl
      COMMON /PARAM / AP1, AP2, GAMma, DEL, DELp, BF, A23, A2, NLWst
C
C Dummy arguments
C
      REAL*8 :: Aj, Destep, Dshif, Rotemp
      INTEGER :: Kk, Nnuc
C
C Local variables
C
      REAL*8 :: ac, cigor, momort, mompar, stab, t, u
      LOGICAL :: bcs
      REAL*8 :: BSQ, FSHELL, ROBCS, RODEF
      INTEGER :: egsm, i, lazy
C
C*** End of declarations rewritten by SPAG
C
CCC   *****************************************************************
 
                                                                ! CRIT
                                                                ! PARAM
 
 
C      REAL*8 erac,arac,tconst,rofgrac,e0,urac,sigg,u1
 
      bcs = .TRUE.                                              ! PARAM
C-----EGSM - J>>K (egsm=0) and EGSM (egsm=1)
 
      egsm = 0
 
 
      lazy = 0
 
      IF(Destep.NE.0.0D0)THEN
        u = (Kk - 1)*Destep + DEL + Dshif
      ELSE
        u = EX(Kk,Nnuc) + DEL + Dshif
      ENDIF
      IF(u.LE.0.0D0)RETURN
      IF(u.GT.UCRt)THEN
        u = u - ECOnd
        IF(u.LE.0.0D0)RETURN
        bcs = .FALSE.
      ELSE
        bcs = .TRUE.
      ENDIF
      IF(lazy.EQ.1)THEN
        Aj = 0.D0
        u = 0.D0
        CALL SIGMAK(A(Nnuc),Z(Nnuc),DEF(1,Nnuc),1.0D0,u,ac,Aj,mompar,
     &              momort,A2,stab,cigor)
      ENDIF
C-----do loop over angular momentum
C     DO i = 1, 1  ! only the first momentum is used, valid only for even-even nuclei
      DO i = 1, NLWst
        Aj = REAL(i) + HIS(Nnuc)
C--------spin  dependent moments of inertia for yrast states by Karwowski
C--------(spin dependent deformation beta calculated according to B.-Mot.)
C--------temporary value of 'a' parameter needed for ground state deformation
C--------damping (no surface correction)
        IF(lazy.NE.1)THEN
 
          ATIl = AP1*A(Nnuc) + AP2*A23
          ATIl = ATIl*ATIlnor(Nnuc)
          ac = ATIl*FSHELL(u,SHC(Nnuc),GAMma)
C--------here FSHELL can become negative
          IF(ac.LE.0.0D0)RETURN
          CALL SIGMAK(A(Nnuc),Z(Nnuc),DEF(1,Nnuc),1.0D0,u,ac,Aj,mompar,
     &                momort,A2,stab,cigor)
C--------'a' including surface dependent factor
          ATIl = AP1*A(Nnuc) + AP2*A23*BSQ(cigor)
          ATIl = ATIl*ATIlnor(Nnuc)
        ENDIF
        ac = ATIl*FSHELL(u,SHC(Nnuc),GAMma)
        IF(ac.LE.0.0D0)RETURN
 
        IF(A2.LT.0.D0)THEN
          BF = 1
        ELSE
          BF = 2
        ENDIF
C
        IF(bcs)THEN
          Rotemp = ROBCS(A(Nnuc),u,Aj,mompar,momort,A2,t,BF)
        ELSE
          Rotemp = RODEF(A(Nnuc),u,ac,Aj,mompar,momort,t,YRAst(i,Nnuc),
     &             HIS(Nnuc),BF,EXPmax,A2,egsm)
        ENDIF
 
        RO(Kk,i,1,Nnuc) = Rotemp
        RO(Kk,i,2,Nnuc) = Rotemp
 
        IF(i.EQ.1)TNUc(Kk,Nnuc) = t
      ENDDO
      RETURN
      END SUBROUTINE DAMIRO
 
!---------------------------------------------------------------------------
 
CCC
CCC
      FUNCTION ROBCS(A,U,Aj,Mompar,Momort,A2,T,Bf)
      IMPLICIT REAL*8(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: ACR, ACRt, ATIl, DETcrt, ECOnd, MOMo, MOMp, SCR, TCRt, 
     &          UCRt
      COMMON /CRIT  / TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl
      COMMON /MOM   / MOMo, MOMp
C
C Dummy arguments
C
      REAL*8 :: A, A2, Aj, Bf, Momort, Mompar, T, U
      REAL*8 :: ROBCS
C
C Local variables
C
      REAL*8 :: const, det, dphi2, exp1, phi, phi2, rot_k, rot_q, ro_j, 
     &          ro_u, s, seff2, vib_kq
      REAL*8 :: DEXP, DSQRT
      REAL*8 :: ro_pi
C
C*** End of declarations rewritten by SPAG
C
CCC   ********************************************************************
CCC   *                                                         CLASS:APU*
CCC   *                        R O B C S                                 *
CCC   * Calculates level densities in the framework of the BCS model     *
CCC   *                                                                  *
CCC   ********************************************************************
C Dummy arguments
C
                                                                ! CRIT
C
C-----CONST=1/(2*SQRT(2 PI))
      DATA const/0.199471D0/                !,denopt!,bet
 
      ROBCS = 0.D0
      dphi2 = U/UCRt
      phi2 = 1.D0 - dphi2
      phi = DSQRT(phi2)
      T = 2.D0*TCRt*phi/LOG((phi + 1.D0)/(1.D0 - phi))
      s = SCR*TCRt*dphi2/T
      det = DETcrt*dphi2*(1.D0 + phi2)**2
      MOMp = Mompar*TCRt*dphi2/T
      IF(MOMp.LT.0.0D0)MOMp = 2.D0
                                 ! RETURN
      MOMo = Momort*0.3333D0 + 0.6666D0*Momort*TCRt*dphi2/T
      IF(MOMo.LT.0.0D0)RETURN
      seff2 = MOMp*T
      IF(ABS(A2).GT.0.005D0)seff2 = MOMp**0.333D0*MOMo**0.6666D0*T
      IF(seff2.LE.0.0D0)RETURN
      ro_u = DEXP(s)/DSQRT(det)
      exp1 = (Aj + 0.5)**2/(2.D0*seff2)
      IF(exp1.GT.20D0)RETURN
      ro_j = const*(2.D0*Aj + 1.D0)/seff2**1.5*DEXP( - exp1)
      IF(ro_j.LT.1D-15)RETURN
      ro_pi = 0.5D0
      ROBCS = ro_u*ro_j*ro_pi
      IF(Bf.EQ.0.D0)RETURN
      CALL COLL_KQ_EGSM(A,T,MOMo,A2,U,vib_kq,rot_k,rot_q)
      ROBCS = ROBCS*rot_k*rot_q*vib_kq
 
      RETURN
      END FUNCTION ROBCS
 
!---------------------------------------------------------------------------
 
CCC
CCC
      FUNCTION RODEF(A,E,Ac,Aj,Mompar,Momort,T,Yrast,Ss,Bf,Expmax,A2,
     &               Egsm)
 
 
      IMPLICIT REAL*8(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      REAL*8 :: A, A2, Ac, Aj, Bf, E, Expmax, Momort, Mompar, Ss, T, 
     &          Yrast
      INTEGER :: Egsm
      REAL*8 :: RODEF
C
C Local variables
C
      REAL*8 :: ak, arg, con, const, det, e1, pi, ro, rot_k, rot_q, 
     &          ro_j, ro_pi, ro_u, s, seff, seff2, sumx, u, vib_kq
      REAL*8 :: DSQRT
      INTEGER :: i, k, kmin
C
C*** End of declarations rewritten by SPAG
C
Ccc   *********************************************************************
Ccc   *                                                         class:ppu *
Ccc   *                         R O D E F                                 *
Ccc   *                                                                   *
Ccc   *  Calculates spin dependent level densities (for a single parity)  *
Ccc   *  in the dynamical approach.                                       *
Ccc   *  Different deformation at each spin is generally considered.      *
Ccc   *  Collective enhancement effects are taken into account including  *
Ccc   *  their energy fade-out.                                           *
Ccc   *                                                                   *
Ccc   *********************************************************************
Ccc
C Dummy arguments
C
 
      DATA const/0.01473144/, pi/3.14159259D0/
C-----CONST=1.0/(24.0*SQRT(2.0))/2.0
C-----the last 2.0 takes into account parity (half to half)
C-----BF controls shape of the nucleus
C-----BF=0. stands for the saddle point         (rot. perpend. to symm.)
C-----BF=1. stands for the oblate yrast state   (rot. paralel  to symm.)
C-----BF=2. stands for the prolate yrast state  (rot. perpend. to symm.)
C-----BF=3. stands for the triaxial yrast state (rot. perpend. to long )
 
      Expmax = 700.
      IF(Ac.LE.0..OR.E.LE.0.D0)RETURN
C----
      RODEF = 0.D0
      T = DSQRT(E/Ac)
      seff2 = Mompar**0.333D0*Momort**0.6666D0*T
C-----FG
      IF(Egsm.EQ.0)THEN
        s = 2.*Ac*T
        det = 45.84*Ac**3*T**5
        ro_u = EXP(s)/SQRT(det)
        ro_j = (1.D0/(2.D0*SQRT(2.D0*pi)))*(2.D0*Aj + 1.D0)
     &         /seff2**1.5*EXP( - (Aj + 0.5)**2/(2.D0*seff2))
        ro_pi = 0.5
        ro = ro_u*ro_j*ro_pi
        IF(Bf.EQ.0.D0)THEN
          RODEF = ro
          RETURN
        ENDIF
        CALL COLL_KQ_EGSM(A,T,Momort,A2,E,vib_kq,rot_k,rot_q)
        RODEF = ro*rot_k*rot_q*vib_kq
        RETURN
      ENDIF
 
 
C-----EGSM
      sumx = 0.0
      IF(Mompar.LT.0.0D0.OR.Momort.LT.0.0D0)THEN
        WRITE(8,*)' WARNING: Negative moment of inertia for spin ', Aj
        WRITE(8,*)' WARNING: 0 level density returned by rodef'
        RETURN
      ENDIF
 
      IF(Ac.EQ.0.0D0)THEN
        WRITE(8,'(''ERROR: LEVEL DENS. PARAMETER a=0 IN RODEF'')')
        STOP
      ENDIF
      seff = 1.0/Mompar - 1.0/Momort
Cms---yrast recalculated
      Yrast = Aj*(Aj + 1.)/(2.*Momort)
      e1 = E - Yrast
      IF(e1.LE.0.0D0)RETURN
      T = SQRT(e1/Ac)
      const = (16.*SQRT(6.*pi))**( - 1)
      con = const/Ac**0.25/SQRT(Mompar*T)
 
      IF(Ss.EQ.( - 1.0D0))THEN
        arg = 2*SQRT(Ac*e1)
        IF(arg.LE.( - Expmax))THEN
          sumx = 0.0
        ELSEIF(e1.GT.1.0D0)THEN
          sumx = EXP(arg)/e1**1.25
        ELSE
          sumx = EXP(arg)
        ENDIF
 
        IF(Aj.LT.1.0D0)GOTO 10
      ENDIF
      i = Aj + 1.
 
      IF(Ss.EQ.( - 1.0D0))THEN
        kmin = 2
      ELSE
        kmin = 1
      ENDIF
 
      DO k = kmin, i
        ak = k + Ss
        IF(e1.LE.0.0D0)RETURN
        IF(Bf.NE.1.0D0)THEN
C-----------rotation perpendicular to the symmetry axis (prolate nucleus)
          u = e1 - 0.5*ak**2*seff
        ELSE
C-----------rotation parallel to the symmetry axis (oblate nucleus)
          u = e1 - 0.5*(Aj*(Aj + 1.) - ak**2)*ABS(seff)
        ENDIF
        IF(u.LE.0.0D0)EXIT
        arg = 2.0*SQRT(Ac*u)
        IF(arg.GT.( - Expmax))THEN
          IF(u.GT.1.0D0)THEN
            sumx = sumx + 2.0*EXP(arg)/u**1.25
          ELSE
            sumx = sumx + 2.0*EXP(arg)
          ENDIF
        ENDIF
      ENDDO
   10 ro = con*sumx
      IF(Bf.EQ.0.D0)THEN
        RODEF = ro
        RETURN
      ENDIF
      CALL COLL_KQ_EGSM(A,T,Momort,A2,e1,vib_kq,rot_k,rot_q)
C-----rot_K=1
      RODEF = ro*rot_q*vib_kq
 
      RETURN
      END FUNCTION RODEF
 
!---------------------------------------------------------------------------
CCC
CCC
      SUBROUTINE COLL_KQ_EGSM(A,T,Momo,A2,U,Vib_kq,Rot_k,Rot_q)
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      REAL*8 :: A, A2, Momo, Rot_k, Rot_q, T, U, Vib_kq
C
C Local variables
C
      REAL*8 :: ftmp, qr, qv, vibrk
C
C*** End of declarations rewritten by SPAG
C
CCC***************************************************************
CCC   Calculates collective enhancements and damping for EGSM and GSM
CCC***************************************************************
 
C     To avoid Compiler warning
      ftmp = A2
C-----vibrational enhancement factor (EMPIRE-2.19)
      CALL VIB_K_EGSM(A,T,vibrk)
C-----damping of vibrational effects
      CALL VIB_Q_EGSM(T,qv)
      IF(qv.GE.0.999D0)vibrk = 1.0
      Vib_kq = qv - vibrk*(qv - 1.)
C-----rotational enhancement
Cc         IF (ABS(A2).LT.0.05D0)THEN
C            rot_K=1.d0
C            rot_Q=1.d0
C            return
Cc         ENDIF
      Rot_k = Momo*T
C-----damping of rotational effects
      CALL ROT_Q_EGSM(U,qr)
      Rot_q = 1.0 - qr*(1.0 - 1.0/(Momo*T))
      RETURN
      END SUBROUTINE COLL_KQ_EGSM
 
!---------------------------------------------------------------------------
CCC
CCC
      SUBROUTINE VIB_K_EGSM(A,T,Vibrk)
      IMPLICIT REAL*8(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      REAL*8 :: A, T, Vibrk
C
C Local variables
C
      REAL*8 :: cost, ht, m0, pi, r0, sdrop
C
C*** End of declarations rewritten by SPAG
C
CCCC  *****************************************************************
CCCC  *  Liquid drop vibrational enhancement of level densities
CCCC  *****************************************************************
      DATA m0, pi, r0, ht/1.044, 3.14159259, 1.26, 6.589/
      sdrop = 17./(4.*pi*r0**2)
      cost = 3.*m0*A/(4.*pi*ht**2*sdrop)
      Vibrk = EXP(1.7*cost**(2./3.)*T**(4./3.))
      END SUBROUTINE VIB_K_EGSM
 
!---------------------------------------------------------------------------
 
      SUBROUTINE VIB_Q_EGSM(T,Q)
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      REAL*8 :: Q, T
C
C Local variables
C
      REAL*8 :: arg, dt, thalf
C
C*** End of declarations rewritten by SPAG
C
CCC   *****************************************************************
CCC   *         DAMPING FOR VIBRATIONAL EFFECTS                       *
CCC   * Q=0 FOR T=0, Q=1/2 FOR T=THALF    , Q=1 FOR T=INFINITY        *
CCC   *****************************************************************
      thalf = 1.
      dt = 0.1
      arg = (T - thalf)/dt
      Q = 1.0/(EXP((-arg)) + 1.0)
      END SUBROUTINE VIB_Q_EGSM
 
!---------------------------------------------------------------------------
 
      SUBROUTINE ROT_Q_EGSM(E1,Qk)
      IMPLICIT REAL*8(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      REAL*8 :: E1, Qk
C
C Local variables
C
      REAL*8 :: dmpdiff, dmphalf
C
C*** End of declarations rewritten by SPAG
C
CCCC  *****************************************************************
CCCC  * damping of rotational  effects with Fermi function independent
CCCC  * of deformation and mass number (consistent with the builtin systematics)
CCCC  *****************************************************************
      Qk = 0.
      dmphalf = 40.
      dmpdiff = 10.
      Qk = 1./(1. + EXP((-dmphalf/dmpdiff)))
     &     - 1./(1. + EXP((E1-dmphalf)/dmpdiff))
      END SUBROUTINE ROT_Q_EGSM
 
!---------------------------------------------------------------------------
 
C==========================================================
      FUNCTION BSQ(Cigor)
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      REAL*8 :: Cigor
      REAL*8 :: BSQ
C
C Local variables
C
      REAL*8 :: qigor
C
C*** End of declarations rewritten by SPAG
C
 
 
      qigor = ( - 0.00246 + 0.3912961*Cigor - 0.00536399*Cigor**2 - 
     &        0.051313*Cigor**3 + 0.043075445*Cigor**4) - 0.375
      IF(qigor.GT.0.077D0)THEN
        BSQ = 0.983 + 0.439*qigor
      ELSE
        BSQ = 1.0 + 0.4*(Cigor - 1.0)**2
      ENDIF
      RETURN
      END FUNCTION BSQ
 
!---------------------------------------------------------------------------
 
C****************************************************
      SUBROUTINE DAMIRO_CRT(Ia,Iz,Shcn,Iout,Ifis)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: A2, A23, ACR, ACRt, AP1, AP2, ATIl, BF, DEL, DELp, 
     &          DETcrt, ECOnd, GAMma, SCR, TCRt, UCRt
      INTEGER :: NLWst
      COMMON /CRIT  / TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl
      COMMON /PARAM / AP1, AP2, GAMma, DEL, DELp, BF, A23, A2, NLWst
C
C Dummy arguments
C
      INTEGER :: Ia, Ifis, Iout, Iz
      REAL*8 :: Shcn
C
C Local variables
C
      REAL*8 :: ar, pi, pi2, xr
      REAL*8 :: FSHELL
      INTEGER :: ix
      CHARACTER(2) :: SMAT
C
C*** End of declarations rewritten by SPAG
C
C****************************************************
 
                                                                ! CRIT
                                                                ! PARAM
 
 
 
 
      pi = 3.141592654D0                                        ! PARAM
      pi2 = pi*pi
      TCRt = 0.567*DELp
 
      ar = ATIl*(1.0 + Shcn*GAMma)
      DO ix = 1, 20
        xr = ar*TCRt**2
        ACRt = ATIl*FSHELL(xr,Shcn,GAMma)
        IF(ABS(ACRt - ar).LE.0.001D0*ACRt)GOTO 10
        ar = ACRt
      ENDDO
      WRITE(8,*)
     &' WARNING: Search for critical a-parameter has not converged for A
     &=', Ia, ' Z=', Iz
      WRITE(8,*)' WARNING: Last iteration has given acrt=', ACRt
      WRITE(8,*)' WARNING: Setting Acrt to 0.1, execution continues'
      ACRt = MAX(ACRt,0.1D0)
 
   10 IF(ACRt.LT.0.0D0)ACRt = 0.1D0
      ECOnd = 1.5*ACRt*DELp**2/pi2
      UCRt = ACRt*TCRt**2 + ECOnd
C-----45.84 stands for (12/SQRT(pi))**2
      DETcrt = 45.84*ACRt**3*TCRt**5
      ACR = ATIl*FSHELL(UCRt,Shcn,GAMma)
      SCR = 2.*ACRt*TCRt
 
      IF(Iout.EQ.6.AND.Ifis.EQ.0)THEN
        WRITE(8,
     &'(1X,/,''  LEVEL DENSITY FOR A SINGLE PARITY FOR ''       ,I3,''-'
     &',A2)')Ia, SMAT(Iz)
 
        WRITE(8,
     &'(2X,/,''  Atil='', F6.3,      ''  Acrt='',F6.3,''  Ucrt='', F6.3,
     & ''  Ecrt='', F6.3,      ''  Econd='', F5.3,''  Det='', G11.3, '' 
     & Scrt='',F6.3)')ATIl, ACRt, UCRt, UCRt - DEL, ECOnd, DETcrt, SCR
      ENDIF
 
      RETURN
      END SUBROUTINE DAMIRO_CRT
 
!---------------------------------------------------------------------------
 
      SUBROUTINE SIGMAK(A,Z,B,Bf,E,Ac,Aj,Mompar,Momort,A2,Stab,Cigor)
      IMPLICIT REAL*8(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      REAL*8 :: A, A2, Ac, Aj, B, Bf, Cigor, E, Momort, Mompar, Stab, Z
C
C Local variables
C
      REAL*8 :: a4, arg, beta, bt, c1, c2, c3, damp, dt, eta, gamma, pi, 
     &          r1, r2, r3, rbmsph, ry, t, tgscr, x, y, ycrit
C
C*** End of declarations rewritten by SPAG
C
Cccc  ******************************************************************
Ccccc *                                                                *
Ccccc *                    S I G M A K                                 *
Ccccc *                                                                *
Cccc  *  Paralel and orthogonal spin cut-off paprameters calculated    *
Cccc  *  following Vigdor and Karwowski (Phys.Rev.C26(1982)1068)       *
Cccc  *  Calculates also def. parameter alpha2 (leg. pol. expansion)   *
Cccc  *  in function of spin in terms of the ldm + dampped g.s. defor. *
Ccccc *                                                                *
Ccccc *  Input: A - nucleus mass number                                *
Ccccc *         Z - nucleus atomic number                              *
Ccccc *         B - ground state deformation (beta2)                   *
Ccccc *        Bf >  0 yrast states                                    *
Ccccc *           =  0 outer saddle (    AXIAL SYMMETRY,MASS  SYMMETRY *
Ccccc *           = -1 outer saddle (    AXIAL SYMMETRY,MASS ASYMMETRY *
Ccccc *           = -2 inner  point (NON-AXIAL SYMMETRY,MASS  SYMMETRY *
Ccccc *         E - excitation energy                                  *
Ccccc *        Ac - level density parameter                            *
Ccccc *        Aj - spin                                               *
Ccccc *                                                                *
Ccccc * Output: Mompar - parallel moment of inertia                    *
Ccccc *         Momort - orthgonal moment of inertia                   *
Ccccc *             A2 - nuclear deformation EPS including damped      *
Ccccc *                  static and dynamical deformation              *
Ccccc *           Stab - maximum spin ensuring stability against       *
Ccccc *                  fission                                       *
Ccccc *          Cigor - ratio of the longest and shortest axis        *
Ccccc *                  to calculate Igor's factor accounting         *
Ccccc *                  for increase of the lev. den. parameter       *
Ccccc *                  due to increased nuclear surface              *
Ccccc *                                                                *
Ccccc *                                                                *
Ccccc *                                                                *
Ccccc *                                                                *
Cccc  ******************************************************************
 
      pi = 3.14159259D0
      IF(A.LE.0.D0)RETURN
C-----Damping ground state deformation (DT=0.4 Tgscr=1.5 MeV)
      dt = 0.4
      t = SQRT(E/Ac)
      tgscr = 1.5
      damp = 1.0/(1.0 + EXP((t-tgscr)/dt))
      bt = B*damp
 
C---- YBM : Y di Bohr-Mottelson, vol.2, pag.663
C     YBM=2.1*AJ**2/A**2.33333
      eta = 1.0 - 1.7826*(A - 2.0*Z)**2/A**2
      x = 0.01965*Z**2/eta/A
      ycrit = 1.4*(1 - x)**2
      Stab = SQRT(ycrit*eta*A**2.33333/1.9249)
      y = 1.9249*Aj*(Aj + 1.0)/eta/A**2.33333
      IF(y.GT.ycrit)y = ycrit
C-----calculation of dynamic deformation in terms of the ldm
C-----saddle point
      IF(Bf.EQ.0.0D0)THEN
        beta = 7./6.*SQRT(4*pi/5.)*(1.0 - x)
     &         *SQRT(4. - 15.*y/7./(1 - x)**2)
        arg = 1./SQRT(4. - 15.*y/7./(1 - x)**2)
        IF(arg.GT.1.0D0)arg = 1.0
        gamma = pi/3. - ACOS(arg)
      ELSEIF(Bf.LT.0.0D0)THEN
        beta = bt       ! the actual static saddle deformation is used (must be < 1.5 !!!)
C        default: axial symmetry                     (outer saddle)
 
        gamma = pi/3.
 
C        arbitrarily fixed nonaxiality of 10 degrees (inner saddle)
        IF(Bf.LT. - 1.50D0)gamma = pi/18.
C-----yrast states
      ELSEIF(Bf.GT.0.0D0)THEN
        gamma = pi/3.
        beta = 7./6.*SQRT(4*pi/5.)*(1.0 - x)
     &         *( - 1. + SQRT(1. + 15.*y/7./(1-x)**2))
        beta = beta + bt    ! adding damped static deformation 'bt'
      ENDIF
 
      r3 = 1. + SQRT(5./4./pi)*beta*COS(gamma - 2.*pi)
      r2 = 1. + SQRT(5./4./pi)*beta*COS(gamma - 4.*pi/3.)
      r1 = 1. + SQRT(5./4./pi)*beta*COS(gamma - 2.*pi/3.)
 
      Cigor = MAX(r1,r2,r3)
      ry = r2/r1
      A2 = (ry - 1.0)/(1.0 + 0.5*ry)
      IF(A2.GT.0.9D0)A2 = 0.9
      IF(A2.LT.( - 0.9D0))A2 = -0.9
C---- next line (if uncommented) neglects all deformations
C     A2=0
      IF(A2.LT.0.0D0)THEN
        c1 = -0.266
        c2 = -0.896
        c3 = -0.571
      ELSE
        c1 = -0.700
        c2 = 0.663
        c3 = 0.286
      ENDIF
      a4 = A2**2*(0.057 + 0.171*x + c2*y) + c3*A2*y
      a4 = a4/(1.0 - 0.37*x - c1*y)
      rbmsph = 0.01448*A**1.66667
      Mompar = (1.0 - A2 + 0.429*A2**2 + 0.268*A2**3 - 0.212*A2**4 - 
     &         1.143*A2*a4 + 0.494*A2**2*a4 + 0.266*a4**2)*rbmsph
      Momort = (1 + 0.5*A2 + 1.286*A2**2 + 0.581*A2**3 - 0.451*A2**4 + 
     &         0.571*A2*a4 + 1.897*A2**2*a4 + 0.700*a4**2)*rbmsph
 
C     Ignatyuk estimates
C      mompar = rbmsph*(1. - (2./3.)*a2)
C      momort = rbmsph*(1. + (1./3.)*a2)
 
      IF(ABS(A2).LE.0.001D0)Momort = Mompar
      END SUBROUTINE SIGMAK
 
!---------------------------------------------------------------------------
 
      FUNCTION FSHELL(X,Xs,Xg)
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      REAL*8 :: X, Xg, Xs
      REAL*8 :: FSHELL
C
C*** End of declarations rewritten by SPAG
C
      IF(X.GT.0.01D0)THEN
        FSHELL = 1.0 + (1.0 - EXP((-Xg*X)))*Xs/X
      ELSE
        FSHELL = 1 + Xg*Xs
      ENDIF
      RETURN
      END FUNCTION FSHELL
 
!---------------------------------------------------------------------------
 
C*********************************************************
      SUBROUTINE ROGSM(Nnuc)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: A2, A23, ACR, ACRt, AP1, AP2, ATIl, BF, DEL, DELp, 
     &          DETcrt, ECOnd, GAMma, SCR, TCRt, UCRt
      INTEGER :: NLWst
      COMMON /CRIT  / TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl
      COMMON /PARAM / AP1, AP2, GAMma, DEL, DELp, BF, A23, A2, NLWst
C
C Dummy arguments
C
      INTEGER :: Nnuc
C
C Local variables
C
      REAL*8 :: ac, cga, def2, defit, mm2, momort, mompar, om2, om3, 
     &          shcn, u
      REAL*8 :: FSHELL
      INTEGER :: ia, iz, kk, nplot
      INTEGER :: INT
C
C*** End of declarations rewritten by SPAG
C
C*********************************************************
 
 
C
C COMMON variables
 
                                                                ! CRIT
                                                                ! PARAM
 
 
C Local variables
 
      ia = INT(A(Nnuc))
      iz = INT(Z(Nnuc))
 
      CALL ROGSM_SYS(Nnuc,AP1,AP2,GAMma,DEL,DELp,om2,om3,cga)
 
 
 
      shcn = SHC(Nnuc)
 
      ATIl = AP1*A(Nnuc) + AP2*A23
 
      ATIl = ATIl*ATIlnor(Nnuc)
 
      u = Q(1,Nnuc) + DEL
 
      ac = ATIl*FSHELL(u,shcn,GAMma)
 
      IF(ac.LE.0.0D0)RETURN
 
 
 
      CALL PRERORITO(Nnuc)
      NLWst = NLW
      CALL DAMIRO_CRT(ia,iz,shcn,IOUt,0)
 
      def2 = DEF(1,Nnuc)
 
      mm2 = .24*A(Nnuc)**.66666
      mompar = 0.608*ACRt*mm2*(1. - 0.6667*def2)
      momort = 0.608*ACRt*mm2*(1. + 0.3330*def2)
 
      defit = (ELV(NLV(Nnuc),Nnuc) + 2.D0)/(NEXreq - 1)
      nplot = (ELV(NLV(Nnuc),Nnuc) + 2.D0)/defit
 
      IF(IOUt.EQ.6.AND.NLV(Nnuc).GT.3)THEN
        DO kk = 1, NEXreq
          u = (kk - 1)*defit + DEL
          CALL BCS_FG(Nnuc,kk,u,mompar,momort,NLWst,def2,om2,om3,cga,
     &                GAMma,shcn)
        ENDDO
        CALL PLOT_ZVV_NUMCUMUL(Nnuc,defit,nplot,NLWst)
      ENDIF
      DO kk = 1, NEXreq
        u = EX(kk,Nnuc) + DEL
        CALL BCS_FG(Nnuc,kk,u,mompar,momort,NLWst,def2,om2,om3,cga,
     &              GAMma,shcn)
      ENDDO
      IF(IOUt.EQ.6)CALL PLOT_ZVV_GSLD(Nnuc)
 
C      Added INITIALIZATION for ROPar(1,Nnuc) and ROPar(3,Nnuc)
      ROPar(1,Nnuc) = ac
      ROPar(3,Nnuc) = DEL
      RETURN
      END SUBROUTINE ROGSM
 
!---------------------------------------------------------------------------
 
CCC   ********************************************************************
      SUBROUTINE BCS_FG(Nnuc,Kk,U,Mompar,Momort,Nlwst,Def2,Om2,Om3,Cga,
     &                  Gamm,Shcn)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: ACR, ACRt, ATIl, DETcrt, ECOnd, SCR, TCRt, UCRt
      COMMON /CRIT  / TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl
C
C Dummy arguments
C
      REAL*8 :: Cga, Def2, Gamm, Momort, Mompar, Om2, Om3, Shcn, U
      INTEGER :: Kk, Nlwst, Nnuc
C
C Local variables
C
      REAL*8 :: ac, aj, arg, const, det, dphi2, mm2, momo, momp, phi, 
     &          phi2, q2, q3, rho, rotemp, rot_kq, ro_j, ro_pi, ro_u, s, 
     &          seff2, seff2ort, t, vib_kq
      REAL*8 :: DSQRT
      REAL*8 :: FSHELL
      INTEGER :: i
      REAL :: REAL
C
C*** End of declarations rewritten by SPAG
C
CCC   ********************************************************************
 
 
C
 
C Local variables
 
C-----CONST=1/(2*SQRT(2 PI))
      DATA const/0.199471D0/                                 ! CRIT
C
      mm2 = .24*A(Nnuc)**.66666
C
      IF(U.LE.UCRt)THEN
C-----BCS
        dphi2 = U/UCRt
        phi2 = 1.D0 - dphi2
        phi = DSQRT(phi2)
        t = 2.D0*TCRt*phi/LOG((phi + 1.D0)/(1.D0 - phi))
        s = SCR*TCRt*(1.D0 - phi2)/t
        det = DETcrt*(1.D0 - phi2)*(1.D0 + phi2)**2
        momp = Mompar*TCRt*(1 - phi2)/t
        IF(momp.LT.0.0D0)RETURN
        momo = Momort*0.3333D0 + 0.6666D0*Momort*TCRt*(1.D0 - phi2)/t
        IF(momo.LT.0.0D0)RETURN
      ELSE
C-----FG
        U = U - ECOnd
        ac = ATIl*FSHELL(U,Shcn,Gamm)
        IF(ac.LE.0..OR.U.LE.0.D0)RETURN
        t = DSQRT(U/ac)
        s = 2.*ac*t
        det = 45.84*ac**3*t**5
        momp = 0.608*ac*mm2*(1. - 0.6667*Def2)
        momo = 0.608*ac*mm2*(1. + 0.3330*Def2)
      ENDIF
C
      seff2 = momp*t
      IF(ABS(Def2).GT.0.005D0)seff2 = momp**0.333D0*momo**0.6666D0*t
      IF(seff2.LE.0.0D0)RETURN
      seff2ort = momo*t
C-----collective enhancements
      IF(Om2.GT.0.)THEN
        CALL VIB_KQ_GSM(t,Om2,Cga,5,q2)
      ELSE
        q2 = 1.D0
      ENDIF
      CALL VIB_KQ_GSM(t,Om3,Cga,7,q3)
      CALL ROT_KQ_GSM(A(Nnuc),Def2,seff2ort,U,rot_kq)
      vib_kq = q2*q3
      ro_u = EXP(s)/SQRT(det)
      ro_pi = 0.5D0
C
      DO i = 1, Nlwst
        aj = REAL(i) + HIS(Nnuc)
        arg = s - (aj + 0.5D0)**2/(2.D0*seff2)
        IF(arg.LE.0.0D0)CYCLE  !return
        ro_j = const*(2.D0*aj + 1.D0)
     &         /seff2**1.5*EXP( - (aj + 0.5)**2/(2.D0*seff2))
        rho = ro_u*ro_j*ro_pi
        rotemp = rho*rot_kq*vib_kq
        RO(Kk,i,1,Nnuc) = rotemp
        RO(Kk,i,2,Nnuc) = rotemp
        IF(i.EQ.1)TNUc(Kk,Nnuc) = t
      ENDDO
      RETURN
      END SUBROUTINE BCS_FG
 
!---------------------------------------------------------------------------
CCC
CCC
 
CCC
CCC
      SUBROUTINE VIB_KQ_GSM(T,Om,Cga,Lam,Q)
Ccc   ********************************************************************
Ccc   *                                                          class:pu*
Ccc   *                      Q V I B R                                   *
Ccc   *                                                                  *
Ccc   * Calculates level density vibrational enhancement factor  using   *
Ccc   * Ignatyuk's formula including damping (see RIPL's)                *
Ccc   *                                                                  *
Ccc   * input: T - nuclear temperature                                   *
Ccc   *       OM - energy of the vibrational level                       *
Ccc   *      LAM - multipolarity (5 for 2+; 7 for 3- states)             *
Ccc   *                                                                  *
Ccc   * output: Q - vibrational enhancement factor due to the OM state   *
Ccc   *                                                                  *
Ccc   * calls:none                                                       *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      IMPLICIT REAL*8(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      REAL*8 :: Cga, Om, Q, T
      INTEGER :: Lam
C
C Local variables
C
      REAL*8 :: DEXP, DLOG
      REAL*8 :: fn, gam, s, u
C
C*** End of declarations rewritten by SPAG
C
      Q = 1.D0
      IF(T.LT.0.01)RETURN
      gam = Cga*(Om**2 + (2.*3.141593*T)**2)
      fn = DEXP( - gam/Om/2.D0)/(DEXP(Om/T) - 1.D0)
      IF(fn.LT.0.D0)RETURN
      u = Lam*Om*fn
      s = Lam*((1. + fn)*DLOG(1. + fn) - fn*DLOG(fn))
      Q = DEXP(s - u/T)
      IF(Q.LT.1.D0)Q = 1.D0
      RETURN
      END SUBROUTINE VIB_KQ_GSM
 
!---------------------------------------------------------------------------
 
CCC   ********************************************************************
      SUBROUTINE ROT_KQ_GSM(A,Bet,Sig4,U,Qr)
CCC   ********************************************************************
      IMPLICIT REAL*8(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      REAL*8 :: A, Bet, Qr, Sig4, U
C
C Local variables
C
      REAL*8 :: dcr1, ucr1
C
C*** End of declarations rewritten by SPAG
C
C***** QROT INCLUDING DAMPING ***
 
      ucr1 = 120.*Bet*Bet*A**.33333
      dcr1 = 1400.*Bet*Bet/A**.66666
      IF(Bet.GT.0.D0)THEN
        Qr = 1./(1. + EXP((U-ucr1)/dcr1))
        Qr = Qr*(Sig4 - 1.) + 1.
      ELSE
        Qr = 1.D0
      ENDIF
      RETURN
      END SUBROUTINE ROT_KQ_GSM
 
!---------------------------------------------------------------------------
 
CCC   ********************************************************************
      SUBROUTINE PRERORITO(Nnuc)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: A2, A23, AP1, AP2, BF, DEL, DELp, GAMma
      INTEGER :: NLWst
      COMMON /PARAM / AP1, AP2, GAMma, DEL, DELp, BF, A23, A2, NLWst
C
C Dummy arguments
C
      INTEGER :: Nnuc
C
C Local variables
C
      INTEGER :: i, ia, iz, k
      INTEGER :: INT
C
C*** End of declarations rewritten by SPAG
C
CCC   ********************************************************************
 
CCC
                                                                ! PARAM
 
C-----check of the input data ---------------------------------------
      ia = INT(A(Nnuc))                                         ! PARAM
      iz = INT(Z(Nnuc))
      IF(NLW.LE.0)THEN
        WRITE(8,
     &'('' MAXIMUM NUMBER OF PARTIAL WAVES HAS NOT BEEN'',             '
     &' DETRMINED BEFORE CALL OF PRERO'',//,                           '
     &' EXECUTION STOPPED'')')
        STOP
      ENDIF
      IF(ia.LE.0.OR.iz.LE.0)THEN
        WRITE(8,
     &'('' A='',I3,'' AND/OR Z='',I2,                                '' 
     &HAS NOT BEEN DETERMINED BEFORE CALL OF PRERO'',               //,'
     &' EXECUTION STOPPED'')')ia, iz
        STOP
      ENDIF
      IF(Nnuc.GT.NDNuc)THEN
        WRITE(8,
     &'('' PRERO  CALLED FOR A NUCLEUS INDEX NNUC=''                   ,
     &I3,'' WHICH EXCEEDS DIMENSIONS'',/,                              '
     &' CHECK THIS CALL OR INCREASE NDNUC TO'',I4,                     '
     &' IN dimension.h AND RECOMPILE'',//,                             '
     &'EXECUTION STOPPED'')')Nnuc, Nnuc
        STOP
      ENDIF
      IF(EX(NEX(Nnuc),Nnuc).LE.0.0D0.AND.FITlev.EQ.0)RETURN
 
C-----set to 0 level density array
      DO i = 1, NDEx
        DO k = 1, NDLw
          IF(BF.NE.0.0D0)THEN
            RO(i,k,1,Nnuc) = 0.0
            RO(i,k,2,Nnuc) = 0.0
          ELSE
            ROF(i,k,Nnuc) = 0.0
          ENDIF
        ENDDO
      ENDDO
      RETURN
      END SUBROUTINE PRERORITO
 
!---------------------------------------------------------------------------
 
CCC
 
      SUBROUTINE PRERO(Nnuc)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: A2, A23, AP1, AP2, BF, DEL, DELp, GAMma
      INTEGER :: NLWst
      COMMON /PARAM / AP1, AP2, GAMma, DEL, DELp, BF, A23, A2, NLWst
C
C Dummy arguments
C
      INTEGER :: Nnuc
C
C Local variables
C
      REAL*8 :: aj, cigor, ftmp, fx, momort, mompar, s, sb, sb0, sbnor, 
     &          segnor, segs, selmax, stab, x, x0, x1, xi, xk
      REAL :: FLOAT
      INTEGER :: i, ia, iz, j, jstabf, k, kstab, ldstab
      INTEGER :: INT, MIN0
      REAL*8 :: SHCFADE
      REAL :: xfis
C
C*** End of declarations rewritten by SPAG
C
CCC
CCC   ********************************************************************
CCC   *                                                         CLASS:APU*
CCC   *                        P R E R O                                 *
CCC   *                                                                  *
CCC   *                                                                  *
CCC   * PREPARES FOR LEVEL DENSITY CALCULATIONS. CHECKS FOR THE          *
CCC   * ENERGY TABLE DETERMINATION, SETS YRAST ENERGIES, FISSION         *
CCC   * BARRIERS, SCALING FACTOR, AND CLEANS UP LEVEL DENSITY TABLES.    *
CCC   *                                                                  *
CCC   *                                                                  *
CCC   * INPUT:NNUC - index of the nucleus                                *
CCC   *       CF   - 1 for saddle point, 0 otherwise                     *
CCC   *                                                                  *
CCC   * calls: BARFIT                                                    *
CCC   *           LPOLY                                                  *
CCC   *        SHCFADE                                                   *
CCC   *        SIGMAK                                                    *
CCC   *                                                                  *
CCC   * AUTHOR: M.HERMAN                                                 *
CCC   * DATE:   11.NOV.1998                                              *
CCC   * REVISION:1    BY:M Herman                 ON:08.Feb.2000         *
CCC   *   Liquid drop stability limit revised. Myers & Swiatecki fission *
CCC   * barriers for Z>102 introduced.                                   *
CCC   *                                                                  *
CCC   *                                                                  *
CCC   ********************************************************************
CCC
                                                                ! PARAM
C-----check of the input data ---------------------------------------
      ia = INT(A(Nnuc))                                         ! PARAM
      iz = INT(Z(Nnuc))
 
      IF(NLW.LE.0)THEN
        WRITE(8,
     &'('' MAXIMUM NUMBER OF PARTIAL WAVES HAS NOT BEEN'',             '
     &' DETRMINED BEFORE CALL OF PRERO'',//,                           '
     &' EXECUTION STOPPED'')')
        STOP
      ENDIF
      IF(ia.LE.0.OR.iz.LE.0)THEN
        WRITE(8,
     &'('' A='',I3,'' AND/OR Z='',I2,                                '' 
     &HAS NOT BEEN DETERMINED BEFORE CALL OF PRERO'',               //,'
     &' EXECUTION STOPPED'')')ia, iz
        STOP
      ENDIF
      IF(Nnuc.GT.NDNuc)THEN
        WRITE(8,
     &'('' PRERO  CALLED FOR A NUCLEUS INDEX NNUC=''                   ,
     &I3,'' WHICH EXCEEDS DIMENSIONS'',/,                              '
     &' CHECK THIS CALL OR INCREASE NDNUC TO'',I4,                     '
     &' IN dimension.h AND RECOMPILE'',//,                             '
     &'EXECUTION STOPPED'')')Nnuc, Nnuc
        STOP
      ENDIF
 
      IF(EX(NEX(Nnuc),Nnuc).LE.0.0D0.AND.FITlev.EQ.0)RETURN
C-----check of the input data ---- done -----------------------------
      IF(FISshi(Nnuc).EQ.1.D0)THEN
C-----check whether the nucleus is fissile
        FISsil(Nnuc) = .TRUE.
        xfis = 0.0205*Z(Nnuc)**2/A(Nnuc)
        IF(xfis.LT.0.3D0)FISsil(Nnuc) = .FALSE.
      ENDIF
C-----determination of the yrast and saddle point energies
C
C-----determination of the LD rotational stability limit LDSTAB
      CALL SIGMAK(A(Nnuc),Z(Nnuc),0.0D0,1.0D0,0.0D0,15.0D0,0.0D0,mompar,
     &            momort,ftmp,stab,cigor)
      kstab = stab
C-----set fission barrier at sky (just in case it is not calculated)
      sb0 = 1000.
      sb = 1000.
      IF(iz.GT.19.AND.iz.LT.102)THEN
        CALL BARFIT(iz,ia,0,sb0,segs,stab)
        ldstab = stab
      ELSE
        ldstab = kstab
      ENDIF
      NLWst = NLW
      IF(HIS(Nnuc).EQ. - 0.5D0)THEN
        ldstab = ldstab - 1
        kstab = kstab - 1
      ENDIF
 
      IF(FISb(1,Nnuc).EQ.0.0D0)THEN
C-----determination of the fission barrier at J=0 (for Z.GE.102)
C-----according to Myers&Swiatecki, Phys. Rev. C60(1999)014606
        IF(iz.GE.102)THEN
          x0 = 48.5428
          x1 = 34.15
          xi = (A(Nnuc) - 2*Z(Nnuc))/A(Nnuc)
          xk = 1.9 + (Z(Nnuc) - 80.0)/75.0
          s = A(Nnuc)**0.666667D0*(1.0 - xk*xi**2)
          x = Z(Nnuc)**2/A(Nnuc)/(1.0 - xk*xi**2)
          fx = 0.0
          IF(x.LE.x0.AND.x.GE.x1)fx = 0.000199749*(x0 - x)**3
          IF(x.LE.x1.AND.x.GE.30.0D0)fx = 0.595553 - 0.124136*(x - x1)
          sb0 = s*fx
          WRITE(8,
     &'('' Liquid drop fission barrier for '',i3,''-'',A2,         '' se
     &t to '',G10.5)')INT(A(Nnuc)), SYMb(Nnuc), sb0
        ENDIF
C
C--------determination of the yrast, saddle point energies and deformations
C
C--------do loop over angular momentum
        segnor = 1.0
        sbnor = 1.0
        jstabf = 0
 
        DO j = 1, NLW
          aj = FLOAT(j - 1)
          CALL SIGMAK(A(Nnuc),Z(Nnuc),DEF(1,Nnuc),1.0D0,0.0D0,15.0D0,aj,
     &                mompar,momort,ftmp,stab,cigor)
C           CALL SIGMAK(A(Nnuc),Z(Nnuc),DEF(1,Nnuc),1.0D0,0.0D0,15.0D0,
C    &                  aj,mompar,momort,beta,stab,cigor)
C           IF (Cf.EQ.0.0D0) DEF(j,Nnuc) = beta ! Commented to avoid using wrong beta out from SIGMAK
 
          IF(iz.GT.19.AND.iz.LT.102)THEN
            sb = 0.0
            IF(j - 1.LE.ldstab)CALL BARFIT(iz,ia,j - 1,sb,segs,selmax)
            IF(j - 1.EQ.ldstab)segnor = segs/(aj*(aj + 1)/(2.0*momort))
            IF(j - 1.GT.ldstab)segs = aj*(aj + 1)/(2.0*momort)*segnor
C------------- Yrast states redefined for normal states to avoid discontinuities
C              as proposed by MS, except for HI induced reactions (AJEc(0)>4)
            IF(AEJc(0).LE.4.)segs = aj*(aj + 1)/(2.0*momort)       ! Jan 2011
          ELSE
C--------------out of the BARFIT range of applicability;
C--------------fission barrier spin dependence is assumed to be  that of
C--------------A=256 Z=102 and normalized at J=0 to the value of Myers &
C--------------Swiatecki (SB0)
            CALL BARFIT(102,256,j - 1,sb,segs,selmax)
            IF(j.EQ.1)sbnor = sb0/sb
            sb = sb*sbnor
            segs = aj*(aj + 1)/(2.0*momort)
          ENDIF
C            segs = aj*(aj + 1)/(2.0*momort)
          YRAst(j,Nnuc) = segs
          SHCjf(j,Nnuc) = SHCFADE(j - 1,SHRj,SHRd)
          FISb(j,Nnuc) = sb*QFIs + segs
          IF(JSTab(Nnuc).NE.0.AND.j.GE.JSTab(Nnuc))EXIT
C-----------determination of stability limit including shell correction
          IF(sb*QFIs - SHCjf(j,Nnuc)*SHC(Nnuc).LE.0.001D0)EXIT
          jstabf = j
        ENDDO
        IF(JSTab(Nnuc).EQ.0)JSTab(Nnuc) = jstabf
      ENDIF
C      IF (JSTab(Nnuc).EQ.0) NLWst = MIN0(JSTab(Nnuc),NLWst)
      NLWst = MIN0(JSTab(Nnuc),NLWst,NLW)
 
      RETURN
C-----yrast and saddle point energies ----- done ---------------
C-----set to 0 level density array
      DO i = 1, NDEx
        DO k = 1, NDLw
          IF(BF.NE.0.0D0)THEN
            RO(i,k,1,Nnuc) = 0.0
            RO(i,k,2,Nnuc) = 0.0
          ELSE
            ROF(i,k,Nnuc) = 0.0
          ENDIF
        ENDDO
      ENDDO
C-----setting to 0 level density array ------ done ------
      END SUBROUTINE PRERO
 
!---------------------------------------------------------------------------
 
      FUNCTION SHCFADE(J,Shrj,Shrd)
      IMPLICIT REAL*8(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      INTEGER :: J
      REAL*8 :: Shrd, Shrj
      REAL*8 :: SHCFADE
C
C Local variables
C
      REAL :: FLOAT
C
C*** End of declarations rewritten by SPAG
C
C
Ccc   ********************************************************************
Ccc   *                                                         CLASS:PPU*
Ccc   *                      S H C F A D E                               *
Ccc   *                                                                  *
Ccc   * calculates angular momentum (J) fade-out of the shell            *
Ccc   * correction to the fission barrier                                *
Ccc   *                                                                  *
Ccc   ********************************************************************
      SHCFADE = 1.
      IF(Shrd.NE.0.D0)SHCFADE = 1.0/(1.0 + EXP((FLOAT(J)-Shrj)/Shrd))
      RETURN
      END FUNCTION SHCFADE
 
!---------------------------------------------------------------------------
 
      SUBROUTINE ROGC(Nnuc,Scutf)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: A2, A23, AM, AP1, AP2, BF, DEL, DELp, EO, GAMma, T, UX
      INTEGER :: NLWst
      COMMON /CT    / AM, UX, EO, T
      COMMON /PARAM / AP1, AP2, GAMma, DEL, DELp, BF, A23, A2, NLWst
C
C Dummy arguments
C
      INTEGER :: Nnuc
      REAL*8 :: Scutf
C
C Local variables
C
      REAL*8 :: amas, arg, atil, defit, e, efort, enorm, eps, exl, 
     &          rolowint, ro_j, ro_pi, ro_u, sigh, sigl, tm, u, xj
      REAL*8 :: DEXP
      REAL*8 :: FSHELL
      INTEGER :: i, ig, igna, iter, j, nplot
      INTEGER :: INT
C
C*** End of declarations rewritten by SPAG
C
CCC
CCC   ********************************************************************
CCC   *                                                         CLASS:PPU*
CCC   *                         R O G C                                  *
CCC   * CALCULATES TABLE OF ENERGY AND SPIN DEPENDENT LEVEL DENSITIES    *
CCC   * FOR NUCLEUS NNUC ACCORDING TO GILBERT-CAMERON                    *
CCC   *                                                                  *
CCC   * INPUT:NNUC - INDEX OF THE NUCLEUS                                *
CCC   *       SCUTF - SPIN CUT-OFF FACTOR (0.146 IS RECOMMENDED)         *
CCC   *                                                                  *
CCC   * OUTPUT:RO(.,.,NNUC) - LEVEL DENSITIES                            *
CCC   *                                                                  *
CCC   *                                                                  *
CCC   ********************************************************************
CCC
 
C-----next call prepares for lev. dens. calculations
      CALL PRERO(Nnuc)
 
      amas = A(Nnuc)
      A23 = A(Nnuc)**0.666667D0
      igna = 0
      ro_pi = 0.5
C-----zero potentially undefined variables
      GAMma = 0.D0
      exl = 0.D0
      sigh = 0.D0
C-----a-parameter given in input
      IF(ROPaa(Nnuc).GT.0.0D0)ROPar(1,Nnuc) = ROPaa(Nnuc)
C-----Ignatyuk parametrization
      enorm = 5.D0
      IF(ROPaa(Nnuc).EQ.0.0D0)THEN
        atil = 0.154D0*A(Nnuc) + 6.3D-5*A(Nnuc)**2
C--------next line assures normalization to experimental data (on average)
        atil = atil*ATIlnor(Nnuc)
        GAMma = -0.054D0
        ROPar(1,Nnuc) = atil*FSHELL(enorm,SHC(Nnuc), - GAMma)
        igna = 1
      ENDIF
C-----Arthurs' parametrization
      IF(ROPaa(Nnuc).EQ.( - 1.0D0))THEN
        atil = 0.1375*A(Nnuc) - 8.36E-5*A(Nnuc)**2
C--------next line assures normalization to experimental data (on average)
        atil = atil*ATIlnor(Nnuc)
        GAMma = -0.054D0
        ROPar(1,Nnuc) = atil*FSHELL(enorm,SHC(Nnuc), - GAMma)
        igna = 1
      ENDIF
C-----Mebel's  parametrization (taken from the INC code for the case
C-----of no collective enhancements) normalized to existing exp. data
      IF(ROPaa(Nnuc).EQ.( - 2.0D0))THEN
        atil = 0.114*A(Nnuc) + 9.80E-2*A23
C--------next line assures normalization to experimental data (on average)
        atil = atil*ATIlnor(Nnuc)
        GAMma = -0.051D0
        ROPar(1,Nnuc) = atil*FSHELL(enorm,SHC(Nnuc), - GAMma)
        igna = 1
      ENDIF
C
C-----If parameters given in input, they are initialized
C
      AM = ROPar(1,Nnuc)
      UX = ROPar(2,Nnuc)
      DEL = ROPar(3,Nnuc)
      EO = ROPar(4,Nnuc)
      T = ROPar(5,Nnuc)
C
C-----calculation of nuclear temperature if t=0
C
      IF(T.EQ.0.D0)THEN
        IF(UX.EQ.0.0D0)THEN
          T = 0.9 - 0.0024*amas
          IF(amas.LT.100.D0)T = 60./amas + 0.06
        ELSE
          T = SQRT(AM/UX) - 3./2./UX
          T = 1./T
          tm = T
        ENDIF
      ENDIF
C
C-----calculation of spin cut-off parameter from resolved levels
C
      sigl = 0.
      DO i = 2, NLV(Nnuc)
        sigl = sigl + (ABS(XJLv(i,Nnuc)) + 0.5)**2
      ENDDO
      IF(NLV(Nnuc).GT.1)sigl = sigl/(NLV(Nnuc) - 1)
      sigl = sigl/2.
      IF(sigl.LT.0.5D0)sigl = 0.5
C
C-----calculation of matching point /if UX=0.0/
C
      iter = 0
   10 IF(AM*T.LE.6.D0.OR.iter.GT.300)THEN
        WRITE(8,*)'WARNING: '
        IF(iter.LT.101)THEN
          WRITE(8,*)'WARNING: Number of iterations in ROGC ', iter
          WRITE(8,*)'WARNING: Can not calculate Ux'
        ELSE
          WRITE(8,*)'WARNING: Maximum number if iterations in ROGC'
        ENDIF
        WRITE(8,*)'WARNING: Level density parameters inconsistent'
        WRITE(8,*)'WARNING: This may happen if you have used default'
        WRITE(8,*)'WARNING: systematics for too light nucleus or '
        WRITE(8,*)'WARNING: have allowed for too many discrete levels'
        WRITE(8,*)'WARNING: entering the region where these are lost'
        WRITE(8,*)'WARNING: Reanalise GC l.d. parameters for:'
        WRITE(8,*)'WARNING: Z=', INT(Z(Nnuc)), '  A=', INT(A(Nnuc))
        WRITE(8,*)'WARNING: a=', AM, ' T=', T
        WRITE(8,*)'WARNING: I will use the last T=', tm, ' for calc.'
        WRITE(8,*)'WARNING: '
        IF(FITlev.GE.0.0D0)THEN
C-----------set nuclear temperature to the value from the systematics
          T = 0.9 - 0.0024*amas
          IF(amas.LT.100.D0)T = 60./amas + 0.06
          tm = T
C            GOTO 500 !????
        ELSEIF(FITlev.LT.0.0D0)THEN
          WRITE(8,*)' ERROR IN DISCRETE LEVEL FITTING'
          WRITE(8,*)' EXECUTION STOPPED BECAUSE OF FITLEV<0 OPTION '
          STOP 'ERROR IN DISCRETE LEVEL FITTING (GC)'
        ENDIF
      ENDIF
 
      IF(igna.NE.0D0)THEN
        DO i = 1, 10
C           write(*,*) '***',a(nnuc),z(nnuc),ux
C           write(*,*) am, 6/t, atil
          IF(UX.EQ.0.0D0)UX = T*T*(AM - 3/T + SQRT((AM-6/T)*AM))/2.0
          AM = atil*FSHELL(UX,SHC(Nnuc), - GAMma)
        ENDDO
      ELSE
        IF(UX.EQ.0.0D0)UX = T*T*(AM - 3/T + SQRT((AM-6/T)*AM))/2.0
      ENDIF
 
      exl = UX + DEL
C-----RCN 12/2004
C-----IF(Scutf.LT.0.0D0)sigh calculated according to Dilg's recommendations
C-----0.6079 = 6/pi^2   a=6/pi^2*g  sig^2 = <m^2>gt  Scutf = <m^2>
      sigh = Scutf*0.6079*A23*SQRT(UX*AM)
C
C-----determination of the index in EX-array such that EX(IG,.).LT.EXL
C-----(low-energy level density formula is used up to IG)
C
      DO i = 1, NEX(Nnuc)
        IF(EX(i,Nnuc).GT.exl)GOTO 20
      ENDDO
      ig = NEX(Nnuc)
      GOTO 30
   20 ig = i - 1
   30 IF(EO.EQ.0.0D0)THEN
        ro_u = DEXP(2.*SQRT(AM*UX))/(12.*SQRT(2*sigh))/AM**0.25/UX**1.25
        EO = exl - T*LOG(T*ro_u)
      ENDIF
C-----fit nuclear temperature (and Ux) to discrete levels
      IF(NLV(Nnuc).GT.5.AND.ROPar(2,Nnuc).EQ.0.0D0.AND.ROPar(5,Nnuc)
     &   .EQ.0.0D0)THEN
        eps = MIN(NLV(Nnuc)*0.03,0.5)
        rolowint = EXP(( - EO/T))*(EXP(ELV(NLV(Nnuc),Nnuc)/T) - 1.)
        IF(ABS(rolowint + 1.0 - NLV(Nnuc)).GT.eps)THEN
          tm = T
          T = T + 0.01*LOG((NLV(Nnuc) - 1)/EXP((-EO/T))
     &        /(EXP(ELV(NLV(Nnuc),Nnuc)/T) - 1))
          UX = 0.0
          EO = 0.0
          iter = iter + 1
          IF(iter.LE.300)GOTO 10
        ENDIF
      ENDIF
 
C-----plot fit of the levels with the low energy l.d. formula
      defit = (ELV(NLV(Nnuc),Nnuc) + 2.D0)/(NEXreq - 1)
      nplot = (ELV(NLV(Nnuc),Nnuc) + 2.D0)/defit
 
      IF(FITlev.GT.0.0D0.AND.NLV(Nnuc).GT.5)
     &   CALL PLOT_GNU_NUMCUMUL_GC(Nnuc)
 
      ROPar(1,Nnuc) = AM
      ROPar(2,Nnuc) = UX
      ROPar(3,Nnuc) = DEL
      ROPar(4,Nnuc) = EO
      ROPar(5,Nnuc) = T
      IF(ig.NE.0)THEN
C-----calculation of level densities below EXL
C-----(low energy formula)
        DO i = 1, ig
          e = EX(i,Nnuc)
          arg = (e - EO)/T
          IF(arg.LT.EXPmax)THEN
            ro_u = EXP(arg)/T
C--------------Spin-cutoff is interpolated
            SIG = sigl
            IF(e.GT.ECUt(Nnuc))SIG = (sigh - sigl)*(e - ECUt(Nnuc))
     &                               /(exl - ECUt(Nnuc)) + sigl
            DO j = 1, NLW
              xj = j + HIS(Nnuc)
              arg = (xj + 0.5)**2/(2.*SIG)
              IF(arg.LE.EXPmax)THEN
                ro_j = (2*xj + 1.)/(2.*SIG)*EXP( - arg)
                RO(i,j,1,Nnuc) = ro_u*ro_j*ro_pi
                RO(i,j,2,Nnuc) = ro_u*ro_j*ro_pi
              ENDIF
            ENDDO
            efort = e
            UEXcit(i,Nnuc) = efort
            TNUc(i,Nnuc) = SQRT(efort/AM)
          ENDIF
        ENDDO
      ENDIF
      ig = ig + 1
      IF(ig.LE.NEX(Nnuc))THEN
C
C--------calculation of level densities for energies surpassing
C--------EXL /fermi gas formula/
C
        DO i = ig, NEX(Nnuc)
          u = EX(i,Nnuc) - DEL
          IF(igna.EQ.1)AM = atil*FSHELL(u,SHC(Nnuc), - GAMma)
          UEXcit(i,Nnuc) = MAX(u,0.D0)
          TNUc(i,Nnuc) = SQRT(u/AM)
C-----------Dilg's recommendations
          SIG = Scutf*0.6079*A23*SQRT(u*AM)
          arg = 2.*SQRT(AM*u)
          IF(arg.LE.EXPmax)THEN
            ro_u = DEXP(arg)/(12.*SQRT(2*SIG))/AM**0.25/u**1.25
            DO j = 1, NLW
              xj = j + HIS(Nnuc)
              arg = (xj + 0.5)**2/(2.*SIG)
              IF(arg.LT.EXPmax)THEN
                ro_j = (2*xj + 1.)/(2.*SIG)*EXP( - arg)
                RO(i,j,1,Nnuc) = ro_u*ro_j*ro_pi
                RO(i,j,2,Nnuc) = ro_u*ro_j*ro_pi
              ENDIF
            ENDDO
          ENDIF
        ENDDO
      ENDIF
 
      IF(IOUt.GE.6..AND.FITlev.EQ.0.0D0.AND.NEX(Nnuc).GT.1)THEN
        CALL PLOT_ZVV_GSLD(Nnuc)
        CALL PLOT_ZVV_NUMCUMUL(Nnuc,defit,nplot,NLWst)
      ENDIF
 
      ROPar(4,Nnuc) = EO
      ROPar(2,Nnuc) = UX
      RETURN
      END SUBROUTINE ROGC
 
!---------------------------------------------------------------------------
 
C
C
 
      SUBROUTINE READ_SHELL_CORR
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C Local variables
C
      REAL*8 :: defcorr, shelmsr
      CHARACTER(2) :: dum
      INTEGER :: iloc, na, nnuc, nz
C
C*** End of declarations rewritten by SPAG
C
Ccc
Ccc   ********************************************************************
Ccc   *                                                          class:au*
Ccc   *             R E A D _ S H E L L _ C O R R                        *
Ccc   *                                                                  *
Ccc   * Reads MS Shell Corrections from RIPL                           *
Ccc   *                                                                  *
Ccc   * input: none (implicit - all considered nuclei)                   *
Ccc   *                                                                  *
Ccc   * output:none (implicit - shell corrections for considered nuclei) *
Ccc   *                                                                  *
Ccc   * calls:none                                                       *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
 
C-----Reading MS shell corrections and deformation energies
      OPEN(11,FILE = TRIM(EMPiredir)//'/RIPL/densities/shellcor-ms.dat',
     &     STATUS = 'old')
C-----Skipping header lines
      READ(11,*)
      READ(11,*)
      READ(11,*)
      READ(11,*)
   10 READ(11,1010,END = 30,ERR = 20)nz, na, dum, shelmsr, defcorr
 1010 FORMAT(2(i4),1x,a2,2x,f7.3,1x,f8.3)
      CALL WHERE(nz*1000 + na,nnuc,iloc)
      IF(iloc.EQ.0)THEN
        SHC(nnuc) = shelmsr*SHLlnor(nnuc)
        IF(ADIv.EQ.1)SHC(nnuc) = (shelmsr - defcorr)*SHLlnor(nnuc)
      ENDIF
C-----projectile
      IF(nz.EQ.Z(0).AND.na.EQ.A(0))THEN
        SHC(0) = shelmsr*SHLlnor(nnuc)
        IF(ADIv.EQ.1)SHC(0) = (shelmsr - defcorr)*SHLlnor(nnuc)
      ENDIF
      GOTO 10
   20 WRITE(8,*)' ERROR: Error reading shell correction file'
      STOP ' ERROR: Error reading shell correction file'
   30 CLOSE(11)
      RETURN
      END SUBROUTINE READ_SHELL_CORR
 
!---------------------------------------------------------------------------
C
      SUBROUTINE ROHFB(Nnuc)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C PARAMETER definitions
C
      INTEGER, PARAMETER :: NLDgrid = 60, JMAx = 50
C
C COMMON variables
C
      REAL*8, DIMENSION(0:NLDgrid,2) :: CGRid
      INTEGER :: IUGrid
      REAL*8, DIMENSION(0:NLDgrid) :: UUGrid
      COMMON /UCGRID/ UUGrid, CGRid, IUGrid
C
C Dummy arguments
C
      INTEGER :: Nnuc
C
C Local variables
C
      REAL*8 :: acorr, c1, c2, corr1, defit, hhh, pcorr, r1, r2, 
     &          rocumul, u
      CHARACTER(2) :: car2
      REAL*8 :: DEXP, DSQRT
      REAL*8 :: DLOG10
      CHARACTER(50) :: filename
      REAL :: FLOAT
      INTEGER :: i, ia, iar, ij, il, ipp, iwin, iz, izr, j, jmaxl, k, 
     &           khi, kk, klo, nplot
      INTEGER :: INT
      INTEGER*4 :: PIPE
      REAL*8, DIMENSION(0:NLDgrid,JMAx,2) :: rhogrid
      REAL*8, DIMENSION(0:NLDgrid,2) :: rhoogrid, rhotgrid
      REAL*8, DIMENSION(0:NLDgrid) :: tgrid
C
C*** End of declarations rewritten by SPAG
C
CCC
CCC   *********************************************************************
CCC   *                                                         CLASS:PPU *
CCC   *                      R O H F B                                    *
CCC   *                                                                   *
CCC   *  Reads level densities numerically calculated by a combinatorial  *
CCC   *  method using Hartree-Fock-Bogoliubov single particle levels          *
CCC   *            (to be included within RIPL-3)                         *
CCC   *                                                                   *
CCC   *     S.Hilaire and S. Goriely, Nucl.Phys.A 779 (2006) 63-81        *
CCC   *  "Global microscopic nuclear level densities within the HFB plus  *
CCC   *   combinatorial method for practical applications"                    *
CCC   *                                                                   *
CCC   *  Interpolates LDs linearily in log to the EMPIRE energy grid.     *
CCC   *                                                                   *
CCC   *  INPUT:                                                           *
CCC   *  NNUC - INDEX OF THE NUCLEUS (POSITION IN THE TABLES)             *
CCC   *                                                                   *
CCC   *                                                                   *
CCC   * OUTPUT:NONE                                                       *
CCC   *                                                                   *
CCC   *********************************************************************
CCC
C     FOLLOWS RIPL-3 DIMENSIONS
 
      ia = A(Nnuc)
      iz = Z(Nnuc)
C-----next call prepares for lev. dens. calculations
      CALL PRERO(Nnuc)
C
C-----initialization
C
      jmaxl = MIN(NDLw,JMAx)
      DO i = 0, NLDgrid
        UUGrid(i) = 0.
        tgrid(i) = 0.
        DO ipp = 1, 2
          CGRid(i,ipp) = 0.
          rhoogrid(i,ipp) = 1.D-20
          rhotgrid(i,ipp) = 1.D-20
          DO j = 1, jmaxl
            rhogrid(i,j,ipp) = 1.D-20
          ENDDO
        ENDDO
      ENDDO
      WRITE(filename,1010)iz
 1010 FORMAT('/RIPL/densities/total/level-densities-hfb/z',i3.3,'.tab')
      OPEN(UNIT = 34,FILE = TRIM(EMPiredir)//filename,ERR = 40)
   10 READ(34,1020,ERR = 40,END = 40)car2
 1020 FORMAT(23x,a2,i3,3x,i3)   !,2x,a8)
      IF(car2.NE.'Z=')GOTO 10
      BACKSPACE(34)
      READ(34,1020,ERR = 40,END = 40)car2, izr, iar   !, paritate
      IF(iar.NE.ia.OR.izr.NE.iz)GOTO 10
C
C-----reading microscopic lev. dens. from the RIPL-3 file
C
      READ(34,*,END = 40)
      READ(34,*,END = 40)
      i = 1
   20 READ(34,1030,END = 40)UUGrid(i), tgrid(i), CGRid(i,1), 
     &                      rhoogrid(i,1), rhotgrid(i,1), 
     &                      (rhogrid(i,j,1),j = 1,jmaxl)
 1030 FORMAT(1x,f6.2,f7.3,1x,53E9.2)
      IF(UUGrid(i).GT.0.001)THEN
        IF(i.EQ.NLDgrid)THEN
          i = 1
          READ(34,*,END = 40)
        ELSE
          i = i + 1
          GOTO 20
        ENDIF
      ENDIF
C     SKIPPING 4 TITLE LINES
      READ(34,*,END = 40)
      READ(34,*,END = 40)
      READ(34,*,END = 40)
      READ(34,*,END = 40)
   30 READ(34,1030,END = 40)UUGrid(i), tgrid(i), CGRid(i,2), 
     &                      rhoogrid(i,2), rhotgrid(i,2), 
     &                      (rhogrid(i,j,2),j = 1,jmaxl)
      IF(UUGrid(i).LE.0.001)GOTO 50
      IF(i.EQ.NLDgrid)GOTO 50
      i = i + 1
      GOTO 30
   40 WRITE(8,*)' ERROR: NO HFB LEV. DENS. FOR Z=', iz, ' A=', ia
      WRITE(8,*)' ERROR: USE OTHER LEVEL DENSITIES. '
      STOP ' ERROR: RIPL HFB ground state lev density missing'
   50 CLOSE(34)
C
C     Using correction files given by A. Koning on March 2008.
C     Corrections are defined exactly as ROHfba() and ROHfbp() parameters
C     by fitting available discrete levels' and D0s' information
C
      IF(ROHfba(Nnuc).LT. - 10.D0.OR.ROHfbp(Nnuc).LT. - 10.D0)THEN
C       Corrections are read only if they are not given in the input,
C       otherwise input values are taken
C
        WRITE(filename,1040)iz
 1040   FORMAT('/RIPL/densities/total/level-densities-hfb/z',i3.3,
     &         '.cor')
        OPEN(UNIT = 34,FILE = TRIM(EMPiredir)//filename,ERR = 60)
        pcorr = 0.D0
        acorr = 0.D0
   55   READ(34,1050,ERR = 60,END = 60)izr, iar, acorr, pcorr
 1050   FORMAT(1x,i3,1x,i3,10x,f11.5,1x,f11.5)
        IF(iar.NE.ia.OR.izr.NE.iz)GOTO 55
 
        IF(ROHfbp(Nnuc).LT. - 10.D0)ROHfbp(Nnuc) = pcorr
        IF(ROHfba(Nnuc).LT. - 10.D0)ROHfba(Nnuc) = acorr
 
C-------printing microscopic lev. dens. corrections from the RIPL-3 file
 
        IF(ROHfba(Nnuc).NE.0.D0)THEN
          WRITE(8,'('' GS HFB L.D. norm  in '',I3,A2,'' set to '',F8.3)'
     &          )ia, SYMb(Nnuc), ROHfba(Nnuc)
          WRITE(12,
     &          '('' GS HFB L.D. norm  in '',I3,A2,'' set to '',F8.3)')
     &          ia, SYMb(Nnuc), ROHfba(Nnuc)
        ENDIF
        IF(ROHfbp(Nnuc).NE.0.D0)THEN
          WRITE(8,'('' GS HFB L.D. shift in '',I3,A2,'' set to '',F8.3)'
     &          )ia, SYMb(Nnuc), ROHfbp(Nnuc)
          WRITE(12,
     &          '('' GS HFB L.D. shift in '',I3,A2,'' set to '',F8.3)')
     &          ia, SYMb(Nnuc), ROHfbp(Nnuc)
        ENDIF
   60   CLOSE(34)
        IF(ROHfba(Nnuc).LT. - 10.D0)ROHfba(Nnuc) = 0.D0
        IF(ROHfbp(Nnuc).LT. - 10.D0)ROHfbp(Nnuc) = 0.D0
        GOTO 65
        WRITE(8,*)' ERROR: reading microsc. LD corrections FOR Z=', iz, 
     &            ' A=', ia, ' IN HFB'
   65   CLOSE(34)
Cxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
      ENDIF
      IUGrid = i - 1
      defit = 0.D0
      IF(IOUt.EQ.6.AND.NLV(Nnuc).GT.3)
     &   CALL PLOT_ZVV_NUMCUMUL(Nnuc,defit,nplot,jmaxl)
      DO kk = 1, NEX(Nnuc)
        u = EX(kk,Nnuc) - ROHfbp(Nnuc)
        UEXcit(kk,Nnuc) = EX(kk,Nnuc)
        IF(u.LT.0.)CYCLE
        IF(u.GT.200.0D0)THEN
          WRITE(8,*)' '
          WRITE(8,*)' ERROR: HFB LEV. DENS. DEFINED UP TO 200 MeV ONLY'
          WRITE(8,*)' ERROR: REQUESTED ENERGY IS ', u, ' MeV'
          WRITE(8,*)' ERROR: YOU HAVE TO USE OTHER LEVEL DENSITIES'
          WRITE(8,*)' ERROR: EXECUTION STOPPED'
          STOP ' ERROR: TOO HIGH ENERGY FOR HFB LEV. DENS.'
        ENDIF
        corr1 = 1.D0
        IF(ROHfba(Nnuc).NE.0.D0)corr1 = DEXP(ROHfba(Nnuc)*DSQRT(u))
C
C--------interpolation in the level density tables
C
        klo = 1
        khi = IUGrid
        IF(u.LE.UUGrid(klo))THEN
          klo = 0
          khi = 1
          GOTO 75
        ENDIF
        IF(u.GE.UUGrid(khi))THEN
          klo = IUGrid - 1
          GOTO 75
        ENDIF
   70   IF(khi - klo.GT.1)THEN
          k = (khi + klo)/2.
          IF(UUGrid(k).GT.u)THEN
            khi = k
          ELSE
            klo = k
          ENDIF
          GOTO 70
        ENDIF
   75   hhh = UUGrid(khi) - UUGrid(klo)
        c1 = (UUGrid(khi) - u)/hhh
        c2 = (u - UUGrid(klo))/hhh
        DO j = 1, jmaxl
          DO ipp = 1, 2
            r1 = rhogrid(klo,j,ipp)
            r2 = rhogrid(khi,j,ipp)
            IF(r1.GT.0.AND.r2.GT.0)THEN
              RO(kk,j,ipp,Nnuc) = 10.**(c1*DLOG10(r1) + c2*DLOG10(r2))
     &                            *corr1
            ELSE
              RO(kk,j,ipp,Nnuc) = (c1*r1 + c2*r2)*corr1
            ENDIF
            IF(RO(kk,j,ipp,Nnuc).LT.0)RO(kk,j,ipp,Nnuc) = 0.D0
          ENDDO
        ENDDO
        TNUc(kk,Nnuc) = c1*tgrid(klo) + c2*tgrid(khi)
      ENDDO
 
C-----plot of the l.d. formula
      IF(IOUt.EQ.6.AND.NLV(Nnuc).GT.3)CALL PLOT_ZVV_GSLD(Nnuc)
 
C--------cumulative plot of levels along with the l.d. formula
      IF(FITlev.GT.0.0D0.AND.NLV(Nnuc).GT.3)THEN
        WRITE(8,1060)INT(Z(Nnuc)), SYMb(Nnuc), INT(A(Nnuc)), NLV(Nnuc)
 1060   FORMAT('Cumulative plot for ',I3,'-',A2,'-',I3,
     &         '   Microscopic LD,  Ncut=',I3)
        OPEN(35,FILE = 'fort.35')
        WRITE(35,*)
     &'set terminal postscript enhanced color lw 2 solid "Helvetica" 20'
        WRITE(35,*)'set output "|cat >>CUMULPLOT.PS"'
        WRITE(35,1070)INT(Z(Nnuc)), SYMb(Nnuc), INT(A(Nnuc)), NLV(Nnuc)
 1070   FORMAT('set title "',I3,'-',A2,'-',I3,
     &         '   Microscopic LD,  Ncut=',I3,'"')
        WRITE(35,*)'set logscale y'
        WRITE(35,*)'set xlabel "Excitation energy (MeV)" 0,0'
        WRITE(35,*)'set ylabel "Cumulative number of levels" 0,0'
        WRITE(35,*)'set style line 1 lt 1 lw 2'
        WRITE(35,*)'set style line 2 lt 5 lw 2'
        WRITE(35,
     &'(''plot "fort.36" w filledcu y2 ls 2 t "Discrete levels", "fort.3
     &4" w l ls 1 t "Level density" '')')
        CLOSE(35)
        OPEN(34,FILE = 'fort.34')
        OPEN(36,FILE = 'fort.36')
        WRITE(36,*)'0.0 1.0'
        DO il = 2, NLV(Nnuc)
          WRITE(36,*)ELV(il,Nnuc)*1D6, FLOAT(il - 1)
          WRITE(36,*)ELV(il,Nnuc)*1D6, FLOAT(il)
        ENDDO
        rocumul = 1.0
        WRITE(34,*)'0.0  ', rocumul
 
        DO kk = 2, IUGrid !NFIsen1
          IF(UUGrid(kk).GT.ELV(NLV(Nnuc),Nnuc) + 2.D0)EXIT
C-----------integration over energy. Parity dependence explicitly considered.
C-----------There is a factor 1/2 steming from the trapezoid integration
C              rocumul = rocumul + 0.5d0*defit
          DO ij = 1, NLW
            rocumul = rocumul + (RO(kk - 1,ij,1,Nnuc) + RO(kk,ij,1,Nnuc)
     &                + RO(kk - 1,ij,2,Nnuc) + RO(kk,ij,2,Nnuc))
          ENDDO
          WRITE(34,*)UUGrid(kk)*1D6, CGRid(kk,1) + CGRid(kk,2)
        ENDDO
        CLOSE(36)
        CLOSE(34)
        IF(IOPsys.EQ.0)THEN
          iwin = PIPE('gnuplot fort.35')
          CLOSE(35)
        ENDIF
      ENDIF
 
      RETURN
      END SUBROUTINE ROHFB
 
!---------------------------------------------------------------------------
 
      SUBROUTINE LEVFIT(Nnuc,Nplot,Dshif,Dshift,Defit)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: A2, A23, ACR, ACRt, AP1, AP2, ATIl, BF, DEL, DELp, 
     &          DETcrt, ECOnd, GAMma, SCR, TCRt, UCRt
      INTEGER :: NLWst
      COMMON /CRIT  / TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl
      COMMON /PARAM / AP1, AP2, GAMma, DEL, DELp, BF, A23, A2, NLWst
C
C Dummy arguments
C
      REAL*8 :: Defit, Dshif, Dshift
      INTEGER :: Nnuc, Nplot
C
C Local variables
C
      REAL*8 :: aj, dshi, ecrt, ellq, exkk, rocumd, rocumu, rocumul, 
     &          rolev, rotemp
      INTEGER :: i, ij, iter, kk, kkl, kku
      INTEGER :: NINT
      REAL :: REAL, SNGL
C
C*** End of declarations rewritten by SPAG
C
 
 
                                                                ! CRIT
                                                                ! PARAM
 
                                                                ! PARAM
 
 
C-----fit level densities to discrete levels applying energy shift
C-----which will linearly go to 0 at neutron binding energy
C-----
C-----get neutron binding energy  if not yet defined
      IF(Q(1,Nnuc).EQ.0.0D0)THEN    !,ldshif(40)
        REWIND(25)
        CALL BNDG(1,Nnuc,Q(1,Nnuc))
      ENDIF
C-----get distance between Qn and the last level
      ellq = Q(1,Nnuc) - (ELV(NLV(Nnuc),Nnuc) + LDShif(Nnuc))
      Dshift = 0.0
      iter = 0
C-----we are not going to fit discrete levels if there are not more
C-----than three
      IF(NLV(Nnuc).GT.3)THEN
        IF(FITlev.GT.0.0D0)THEN
          WRITE(8,*)' '
          WRITE(8,*)' Fitting l.d. to discrete levels'
          WRITE(8,*)NLV(Nnuc), ' levels at ', ELV(NLV(Nnuc),Nnuc), 
     &              ' MeV'
        ENDIF
        Defit = (ELV(NLV(Nnuc),Nnuc) + LDShif(Nnuc) + 2.D0)/(NEXreq - 1)
        Nplot = (ELV(NLV(Nnuc),Nnuc) + LDShif(Nnuc) + 2.D0)/Defit
 
    5   rocumul = 1.0
        iter = iter + 1
        kkl = 0
        kku = 0
        DO kk = 1, NDEx
C-----------clean RO matrix
          DO i = 1, NDLw
            RO(kk,i,1,Nnuc) = 0.D0
            RO(kk,i,2,Nnuc) = 0.D0
          ENDDO
        ENDDO
        DO kk = 1, NEXreq
C-----------decrease energy shift above the last level to become 0 at Qn
          exkk = (kk - 1)*Defit
          IF(exkk.LE.ELV(NLV(Nnuc),Nnuc) + LDShif(Nnuc))THEN
            Dshif = Dshift
          ELSEIF(exkk.LT.Q(1,Nnuc).AND.ellq.NE.0.0D0)THEN
            Dshif = Dshift*(Q(1,Nnuc) - exkk)/ellq
          ELSE
            Dshif = 0.0
          ENDIF
 
          CALL DAMIRO(kk,Nnuc,Dshif,Defit,rotemp,aj)
 
          DO ij = 1, NLWst
C--------------Integration over energy. Parity dependence explicitly considered.
C--------------There is a factor 1/2 steming from the trapezoid integration
            IF(kk.GT.1)rocumul = rocumul + 
     &                           0.5D0*Defit*(RO(kk - 1,ij,1,Nnuc)
     &                           + RO(kk,ij,1,Nnuc)
     &                           + RO(kk - 1,ij,2,Nnuc)
     &                           + RO(kk,ij,2,Nnuc))
          ENDDO
          IF(rocumul.LE.NLV(Nnuc))THEN
            kkl = kk
            rocumd = rocumul
          ELSEIF(kku.EQ.0)THEN
            kku = kk
            rocumu = rocumul
          ENDIF
        ENDDO
 
        rocumd = LOG(rocumd)
        rocumu = LOG(rocumu)
        rolev = LOG(REAL(NLV(Nnuc)))
        dshi = (rolev - rocumd)/(rocumu - rocumd)
        dshi = (kkl - 1 + dshi)*Defit
        dshi = dshi - (ELV(NLV(Nnuc),Nnuc) + LDShif(Nnuc))
        Dshift = Dshift + dshi
 
        IF(FITlev.GT.0.0D0)THEN
          ecrt = UCRt - DEL - Dshift
          WRITE(8,*)
          WRITE(8,*)'*****   A=', NINT(A(Nnuc)), ' Z=', NINT(Z(Nnuc)), 
     &              ' Bn=', SNGL(Q(1,Nnuc)), ' LDshif=', 
     &              SNGL(LDShif(Nnuc))
          WRITE(8,'(A7,G12.5,A6,G12.5,A9,G12.5,A7,G12.5)')'Ucrt = ', 
     &          UCRt, ' Ecrt=', ecrt, ' Econd = ', ECOnd, ' DEL = ', DEL
          WRITE(8,'(A5,I3,4X,G12.5,A15,2(G12.5,1x))')'It # ', iter, 
     &          dshi, ' Final shift = ', Dshift
          WRITE(8,*)
        ENDIF
        IF(ABS(dshi).GT.0.01D0.AND.iter.LE.20)GOTO 5
      ENDIF
 
      RETURN
      END SUBROUTINE LEVFIT
 
!---------------------------------------------------------------------------
 
CCC
CCC
CCC   *************FISSION**********************************
CCC
CCC
 
CCC   *****************************************************************
      SUBROUTINE DAMIRO_FISHI(Kk,Nnuc,Asaf,Rotemp,Aj)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: A2, A23, ACR, ACRt, AP1, AP2, ATIl, BF, DEL, DELp, 
     &          DETcrt, ECOnd, GAMma, SCR, TCRt, UCRt
      INTEGER :: NLWst
      COMMON /CRIT  / TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl
      COMMON /PARAM / AP1, AP2, GAMma, DEL, DELp, BF, A23, A2, NLWst
C
C Dummy arguments
C
      REAL*8 :: Aj, Asaf, Rotemp
      INTEGER :: Kk, Nnuc
C
C Local variables
C
      REAL*8 :: ac, accn, ampl, cigor, momort, mompar, phi, rbmsph, 
     &          saimid, saimin, saimx, selmax, shredt, stab, t, temp, u
      LOGICAL :: bcs
      REAL*8 :: BSQ, FSHELL, ROBCS, RODEF
      INTEGER :: egsm, i, ia, iz
      INTEGER :: INT
      REAL :: REAL
C
C*** End of declarations rewritten by SPAG
C
CCC   *****************************************************************
 
                                                                ! CRIT
                                                                ! PARAM
 
      bcs = .TRUE.                                              ! PARAM
      rbmsph = 0.01448*A(Nnuc)**1.6667
      ia = INT(A(Nnuc))
      iz = INT(Z(Nnuc))
 
C-----do loop over angular momentum
      DO i = 1, NLWst
        Aj = REAL(i) + HIS(Nnuc)
 
C-----a-parameter and U determination for fission channel
C-----------temperature fade-out of the shell correction
C-----------ACCN  serves only to calculate temperature fade-out
        IF(EX(Kk,Nnuc).GT.UCRt)THEN
          accn = ATIl*(1 + SHC(Nnuc)*(1 - EXP((-GAMma*EX(Kk,Nnuc))))
     &           /EX(Kk,Nnuc))
        ELSE
          accn = ACRt
        ENDIF
        temp = 0.
        IF(EX(Kk,Nnuc).GE.YRAst(i,Nnuc))
     &     temp = SQRT((EX(Kk,Nnuc) - YRAst(i,Nnuc))/accn)
        ampl = EXP(TEMp0*SHRt)
        shredt = 1.
        IF(temp.GE.TEMp0)shredt = ampl*EXP(( - SHRt*temp))
C--------temperature fade-out of the shell correction  --- done ----
        u = EX(Kk,Nnuc) + DEL - FISb(i,Nnuc) + SHC(Nnuc)
     &      *shredt*SHCjf(i,Nnuc)
        IF(u.GT.UCRt)THEN
          u = u - ECOnd
          bcs = .FALSE.
        ELSE
          bcs = .TRUE.
        ENDIF
        UEXcit(Kk,Nnuc) = MAX(u,0.D0)
        IF(u.LE.0.0D0)RETURN
        IF(Z(Nnuc).LT.102.0D0.AND.Z(Nnuc).GE.19.0D0)THEN
C-----------next line is to calculate deformation parameter A2 only
          CALL SIGMAK(A(Nnuc),Z(Nnuc),DEF(1,Nnuc),0.0D0,u,accn,Aj,
     &                mompar,momort,A2,stab,cigor)
          CALL MOMFIT(iz,ia,i - 1,saimin,saimid,saimx,selmax)
          mompar = saimin*rbmsph
          momort = saimx*rbmsph
        ELSE
          CALL SIGMAK(A(Nnuc),Z(Nnuc),DEF(1,Nnuc),0.0D0,u,accn,Aj,
     &                mompar,momort,A2,stab,cigor)
        ENDIF
C--------calculation of level density parameter 'a' including surface
C--------dependent factor
        ATIl = AP1*A(Nnuc) + AP2*A23*BSQ(cigor)
        ATIl = ATIl*ATIlnor(Nnuc)
        IF(Asaf.GE.0.D0)ac = ATIl*FSHELL(u,SHC(Nnuc),Asaf)
        IF(Asaf.LT.0.D0)ac = -ATIl*Asaf
        IF(ac.LE.0.D0)RETURN
 
        IF(bcs)THEN
          Rotemp = ROBCS(A(Nnuc),u,Aj,mompar,momort,A2,t,BF)
          IF(i.EQ.1)THEN
            phi = SQRT(1.D0 - u/UCRt)
            t = 2.0*TCRt*phi/LOG((phi + 1.D0)/(1.D0 - phi))
          ENDIF
        ELSE
 
C-----------EGSM - J>>K (egsm=0) and EGSM (egsm=1)
 
          egsm = 0
          Rotemp = RODEF(A(Nnuc),u,ac,Aj,mompar,momort,t,YRAst(i,Nnuc),
     &             HIS(Nnuc),BF,EXPmax,A2,egsm)
          IF(i.EQ.1)t = SQRT(u/ac)
        ENDIF
 
        ROF(Kk,i,Nnuc) = Rotemp
        IF(i.EQ.1)TNUc(Kk,Nnuc) = t
      ENDDO
      RETURN
      END SUBROUTINE DAMIRO_FISHI
 
!---------------------------------------------------------------------------
 
C=======================================================================
      SUBROUTINE DAMI_RO_HFB_FIS(Nnuc,Ib,Rafis)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      INTEGER :: Ib, Nnuc
      REAL*8 :: Rafis
C
C Local variables
C
      REAL*8 :: aaj, bbb, def2, excn1, ggg, mompar, rotemp, rrr1, rrr2, 
     &          rrry, temp, xmax
      REAL :: FLOAT
      INTEGER :: ia, iff, in, ipp, iz, jj, kk, nr
      INTEGER :: INT
      REAL :: rbmsph
C
C*** End of declarations rewritten by SPAG
C
C
 
                                                                ! CRIT
 
                                                                ! CRITFIS
 
                                                                ! PARAM
 
 
 
 
 
 
      iz = INT(Z(Nnuc))                                         ! PARAM
      ia = INT(A(Nnuc))
      in = ia - iz
C-----continuum, level densities at saddle points
      excn1 = EMAx(Nnuc)
C-----where continuum starts,ends,steps in between
      XMInn(Ib) = 0.0001
      DO nr = 1, NRFdis(Ib)
        IF(EFDis(nr,Ib).GT.XMInn(Ib))XMInn(Ib) = EFDis(nr,Ib)
      ENDDO
C     IF(ECFis(ib).gt.0.) XMInn(Ib) = ECFis(ib)
 
      IF(excn1.LE.(EFB(Ib) + XMInn(Ib)))THEN
        xmax = XMInn(Ib) + 3.5D0
      ELSE
        xmax = excn1 - (EFB(Ib) + XMInn(Ib)) + 3.5D0
      ENDIF
      DEStepp(Ib) = (xmax - XMInn(Ib))/100.
      NRBinfis(Ib) = INT((xmax - XMInn(Ib))/DEStepp(Ib))
 
      IF(NRBinfis(Ib).GT.NFIsenmax)THEN
        WRITE(8,*)' ERROR: Level density at saddle exceeds dimensions', 
     &            ' Increase NFISENMAX in dimension.h'
        STOP 'ERROR: Level density at saddle exceeds NFISENMAX'
      ENDIF
 
      DO kk = 1, NRBinfis(Ib)
        UGRid(kk,Ib) = XMInn(Ib) + (kk - 1)*DEStepp(Ib)
      ENDDO
      CALL HFB_FIS(Ib,Nnuc)
      Rafis = 1.D0
 
      iff = BFF(Ib)
      rbmsph = 0.01448*A(Nnuc)**1.66667
C     See eq.(1.38) of the Ignatyuk Book (Stat.prop....)
 
      bbb = DEFfis(Ib) ! the actual static saddle deformation is used
      IF(bbb.GT.1.5D0)THEN
        WRITE(8,*)
     &      ' WARNING: Deformation reset to 1.5 for HFB fiss.barrier b='
     &      , Ib
        WRITE(8,*)
     &          ' WARNING:  for moment of inertia calculation (SIGMAK) '
        bbb = 1.5D0
      ENDIF
 
      ggg = PI/3.                 ! axial symmetry
      IF(iff.EQ.2)ggg = PI/18.    ! arbitrarily fixed asymmetry to 10 degrees
 
      rrr2 = 1. + SQRT(5./4./PI)*bbb*COS(ggg - 4.*PI/3.)
      rrr1 = 1. + SQRT(5./4./PI)*bbb*COS(ggg - 2.*PI/3.)
 
      rrry = rrr2/rrr1
      def2 = (rrry - 1.0)/(1.0 + 0.5*rrry)
      IF(def2.GT.0.9D0)def2 = 0.9
      IF(def2.LT.( - 0.9D0))def2 = -0.9
 
      mompar = rbmsph*(1. - (2./3.)*def2)
 
      DO jj = 1, NLW
        aaj = FLOAT(jj) + HIS(Nnuc)
        DO kk = 1, NRBinfis(Ib)
          temp = TNUcf(kk,Nnuc)
C
C-----------SYMMETRY ENHANCEMENT
C
C           The normal GS is axial and mass symmetric
C
          DO ipp = 1, 2
            rotemp = ROFisp(kk,jj,ipp,Ib)
C              Nonaxial symmetry with mass symmetry
 
            IF(iff.EQ.2)rotemp = rotemp*SQRT(PI/2.D0)*SQRT(mompar*temp)
C              Mass asymmetry (with axial symmetry) is already considered in HFB calculations
C              IF (Iff.EQ.3) rotemp = rotemp*2.   ! axial, mass asymmetry
            ROFisp(kk,jj,ipp,Ib) = rotemp*ROHfb_norm(Ib)
          ENDDO
        ENDDO
      ENDDO
 
      IF(IOUt.EQ.6)CALL PLOT_ZVV_SADLD(Nnuc,Ib)
 
      END SUBROUTINE DAMI_RO_HFB_FIS
 
!---------------------------------------------------------------------------
C**************************************************************************
      SUBROUTINE HFB_FIS(Ib,Nnuc)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C PARAMETER definitions
C
      INTEGER, PARAMETER :: NLDgrid = 60, JMAx = 50
C
C Dummy arguments
C
      INTEGER :: Ib, Nnuc
C
C Local variables
C
      REAL*8 :: c1, c2, hhh, r1, r2, u
      CHARACTER(2) :: car2
      REAL*8, DIMENSION(0:NLDgrid,2) :: cgrid, rhoogrid, rhotgrid
      REAL*8 :: DEXP, DSQRT
      REAL*8 :: DLOG10
      CHARACTER(38) :: filename
      INTEGER :: i, ia, iar, ipp, iugrid, iz, izr, j, jmaxl, k, khi, kk, 
     &           klo
      REAL*8, DIMENSION(0:NLDgrid,JMAx,2) :: rhogrid
      REAL*8, DIMENSION(0:NLDgrid) :: tgrid, uugrid
C
C*** End of declarations rewritten by SPAG
C
CCC
CCC   *********************************************************************
CCC   *                                                         CLASS:PPU *
CCC   *                      R O H F B_FIS                                    *
CCC   *                                                                   *
CCC   *  Reads level densities numerically calculated by a combinatorial  *
CCC   *  method using Hartree-Fock-Bogoliubov single particle levels      *
CCC   *            (to be included within RIPL-3)                         *
CCC   *                                                                   *
CCC   *     S.Hilaire and S. Goriely, Nucl.Phys.A 779 (2006) 63-81        *
CCC   *  "Global microscopic nuclear level densities within the HFB plus  *
CCC   *   combinatorial method for practical applications"                *
CCC   *                                                                   *
CCC   *  Interpolates LDs linearily in log to the EMPIRE energy grid.     *
CCC   *********************************************************************
 
C     FOLLOWS RIPL-3 DIMENSIONS
 
 
 
      ia = A(Nnuc)
      iz = Z(Nnuc)
C
C-----initialization
C
      jmaxl = MIN(NDLw,JMAx)
      DO i = 0, NLDgrid
        uugrid(i) = 0.
        tgrid(i) = 0.
        DO ipp = 1, 2
          cgrid(i,ipp) = 0.
          rhoogrid(i,ipp) = 1.D-20
          rhotgrid(i,ipp) = 1.D-20
          DO j = 1, jmaxl
            rhogrid(i,j,ipp) = 1.D-20
          ENDDO
        ENDDO
      ENDDO
 
 
C-----reading microscopic lev. dens. from the RIPL-3 file
      WRITE(filename,1010)Ib, iz
 1010 FORMAT('/RIPL/fission/leveldensities/Max',i1,'/z',i3.3)
      OPEN(UNIT = 34,FILE = TRIM(EMPiredir)//filename,ERR = 40)
C     OPEN (UNIT = 34,FILE = trim(empiredir)//
C    &'/RIPL/fission/leveldensities/Max/'//ctmp6, ERR = 300)
   10 READ(34,1020,ERR = 40,END = 40)car2
 1020 FORMAT(23x,a2,i3,3x,i3)
      IF(car2.NE.'Z=')GOTO 10
      BACKSPACE(34)
      READ(34,1020,ERR = 40,END = 40)car2, izr, iar
      IF(iar.NE.ia.OR.izr.NE.iz)GOTO 10
      READ(34,*,END = 40)
      READ(34,*,END = 40)
      i = 1
   20 READ(34,1030,END = 40)uugrid(i), tgrid(i), cgrid(i,1), 
     &                      rhoogrid(i,1), rhotgrid(i,1), 
     &                      (rhogrid(i,j,1),j = 1,jmaxl)
 1030 FORMAT(1x,f6.2,f7.3,1x,53E9.2)
      IF(uugrid(i).GT.0.001)THEN
        IF(i.EQ.NLDgrid)THEN
          i = 1
          READ(34,*,END = 40)
        ELSE
          i = i + 1
          GOTO 20
        ENDIF
      ENDIF
C     SKIPPING 4 TITLE LINES
      READ(34,*,END = 40)
      READ(34,*,END = 40)
      READ(34,*,END = 40)
      READ(34,*,END = 40)
   30 READ(34,1030,END = 40)uugrid(i), tgrid(i), cgrid(i,2), 
     &                      rhoogrid(i,2), rhotgrid(i,2), 
     &                      (rhogrid(i,j,2),j = 1,jmaxl)
      IF(uugrid(i).LE.0.001)GOTO 50
      IF(i.EQ.NLDgrid)GOTO 50
      i = i + 1
      GOTO 30
   40 WRITE(8,*)' NO LEV. DENS. FOR Z=', iz, ' A=', ia, 
     &          ' IN HFB at saddle ', Ib
      WRITE(8,*)
     &' USE OTHER LEVEL DENSITIES. EXECUTION                    TERMINAT
     &ED '
      WRITE(8,*)' ERROR: HFB lev dens. at saddle', Ib, '  missing'
      STOP ' ERROR: HFB lev dens. at saddle missing'
   50 CLOSE(34)
      iugrid = i - 1
      DO kk = 1, NRBinfis(Ib)
        u = XMInn(Ib) + (kk - 1)*DEStepp(Ib) - ROHfbp_sd(Ib)
        IF(u.LT.0.)CYCLE
        IF(u.GT.200.0D0)THEN
          WRITE(8,*)' '
          WRITE(8,*)' HFB LEV. DENS. DEFINED UP TO 200 MeV ONLY'
          WRITE(8,*)' REQUESTED ENERY IS ', u, ' MeV'
          WRITE(8,*)' YOU HAVE TO USE ANOTHER LEVEL DENSITIES'
          WRITE(8,*)' EXECUTION STOPPED'
          STOP 'TOO HIGH ENERGY FOR HFB LEV. DENS.'
        ENDIF
 
C--------interpolation in the level density tables
C
        klo = 1
        khi = iugrid
        IF(u.LE.uugrid(klo))THEN
          klo = 0
          khi = 1
          GOTO 60
        ENDIF
        IF(u.GE.uugrid(khi))THEN
          klo = iugrid - 1
          GOTO 60
        ENDIF
   55   IF(khi - klo.GT.1)THEN
          k = (khi + klo)/2.
          IF(uugrid(k).GT.u)THEN
            khi = k
          ELSE
            klo = k
          ENDIF
          GOTO 55
        ENDIF
   60   hhh = uugrid(khi) - uugrid(klo)
        c1 = (uugrid(khi) - u)/hhh
        c2 = (u - uugrid(klo))/hhh
        DO j = 1, jmaxl
          DO ipp = 1, 2
            r1 = rhogrid(klo,j,ipp)
            r2 = rhogrid(khi,j,ipp)
            IF(r1.GT.1.D-12.AND.r2.GT.1.D-12)THEN
              ROFisp(kk,j,ipp,Ib) = 10.**(c1*DLOG10(r1) + c2*DLOG10(r2))
              IF(ROHfba_sd(Ib).NE.0.D0)ROFisp(kk,j,ipp,Ib)
     &           = ROFisp(kk,j,ipp,Ib)*DEXP(ROHfba_sd(Ib)*DSQRT(u))
            ELSE
              ROFisp(kk,j,ipp,Ib) = (c1*r1 + c2*r2)
            ENDIF
            IF(ROFisp(kk,j,ipp,Ib).LT.0.D0)ROFisp(kk,j,ipp,Ib) = 0.D0
          ENDDO
        ENDDO
        TNUcf(kk,Nnuc) = c1*tgrid(klo) + c2*tgrid(khi)
      ENDDO
      RETURN
      END SUBROUTINE HFB_FIS
 
!---------------------------------------------------------------------------
C==============================================================
      SUBROUTINE DAMI_ROFIS(Nnuc,Ib,Mmod,Rafis)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: A2, A23, ACR, ACRt, AP1, AP2, ATIl, BF, DEL, DELp, 
     &          DETcrt, ECOnd, GAMma, SCR, TCRt, UCRt
      REAL*8, DIMENSION(NFHump) :: ACRtf, DETcrtf, ECOndf, SCRtf, TCRtf, 
     &                             UCRtf
      REAL*8, DIMENSION(NFParab) :: MORtcrt, MPArcrt
      INTEGER :: NLWst
      COMMON /CRIT  / TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl
      COMMON /CRITFIS/ ACRtf, UCRtf, TCRtf, DETcrtf, SCRtf, MORtcrt, 
     &                 MPArcrt, ECOndf
      COMMON /PARAM / AP1, AP2, GAMma, DEL, DELp, BF, A23, A2, NLWst
C
C Dummy arguments
C
      INTEGER :: Ib, Mmod, Nnuc
      REAL*8 :: Rafis
C
C Local variables
C
      REAL*8 :: aaj, accn, aj, ar, cigor, def2, desteppp, excn1, 
     &          momort, mompar, rotemp, shcf, stab, temp, u, 
     &          vibbf12, vibbfdt, vn, xmax
      REAL*8 :: BSQ, FSHELL, ROBCSF, RODEFF
      REAL :: dshiff
      REAL :: FLOAT
      INTEGER :: i, ia, iff, in, iz, jj, kk, nr
      INTEGER :: INT
C
C*** End of declarations rewritten by SPAG
C
C==============================================================
C
 
C-----continuum, level densities at saddle points
      excn1 = EMAx(Nnuc)                                        ! PARAM
C-----where continuum starts,ends,steps in between
      IF(Mmod.EQ.0)THEN
        XMInn(Ib) = 0.01
        DO nr = 1, NRFdis(Ib)
          IF(EFDis(nr,Ib).GT.XMInn(Ib))XMInn(Ib) = EFDis(nr,Ib)
        ENDDO
        IF(ECFis(Ib).GT.0.)XMInn(Ib) = ECFis(Ib)
 
        IF(excn1.LE.(EFB(Ib) + XMInn(Ib)))THEN
          xmax = XMInn(Ib) + 3.5D0
        ELSE
          xmax = excn1 - (EFB(Ib) + XMInn(Ib)) + 3.5D0
        ENDIF
        DEStepp(Ib) = (xmax - XMInn(Ib))/100.
        NRBinfis(Ib) = INT((xmax - XMInn(Ib))/DEStepp(Ib))
 
        IF(NRBinfis(Ib).GT.NFIsenmax)THEN
          WRITE(8,*)' ERROR: Level density at saddle exceeds dimensions'
     &              , ' Increase NFISENMAX in dimension.h'
          STOP 'ERROR: Level density at saddle exceeds NFISENMAX'
        ENDIF
        DO kk = 1, NRBinfis(Ib)
          UGRid(kk,Ib) = XMInn(Ib) + (kk - 1)*DEStepp(Ib)
        ENDDO
      ELSE ! Mmod.GT.0
        XMInnm(Mmod) = 0.0001
        DO nr = 1, NRFdis(Ib)
          IF(EFDism(nr,Mmod).GT.XMInnm(Mmod))XMInnm(Mmod)
     &       = EFDism(nr,Mmod)
        ENDDO
        IF(ECFism(Mmod).GT.0.)XMInnm(Mmod) = ECFism(Mmod)
 
        IF(excn1.LE.(EFBm(Mmod) + XMInnm(Mmod)))THEN
          xmax = XMInn(Mmod) + 3.D0
        ELSE
          xmax = excn1 - (EFBm(Mmod) + XMInnm(Mmod)) + 3.D0
        ENDIF
        DEStepm(Mmod) = (xmax - XMInnm(Mmod))/100.D0
        NRBinfism(Mmod) = INT((xmax - XMInnm(Mmod))/DEStepm(Mmod))
        IF(NRBinfism(Mmod).GT.NFIsenmax)THEN
          WRITE(8,*)' ERROR: Level density at saddle exceeds dimensions'
     &              , ' Increase NFISENMAX in dimension.h'
          STOP 'ERROR: Level density at saddle exceeds NFISENMAX'
        ENDIF
 
        DO kk = 1, NRBinfism(Mmod)
          UGRidf(kk,Mmod) = XMInnm(Mmod) + (kk - 1)*DEStepm(Mmod)
        ENDDO
      ENDIF
 
      iz = INT(Z(Nnuc))
      ia = INT(A(Nnuc))
      in = ia - iz
      A23 = A(Nnuc)**0.666667D0
      iff = 1
 
      temp = 0.D0
C-----EMPIRE-3.0-dependence
      CALL EGSMSYS(AP1,AP2,GAMma,DEL,DELp,Nnuc)
 
      IF(Mmod.EQ.0)THEN
        GAMma = GAMmafis(Ib)
        dshiff = DELtafis(Ib)
        shcf = SHCfis(Ib)
        iff = BFF(Ib)
        desteppp = DEStepp(Ib)
        vibbf12 = VIBf12(Ib)
        vibbfdt = VIBfdt(Ib)
        vn = VIBfnorm(Ib)
      ELSE ! Mmod.GT.0
        NRBinfis(Ib) = NRBinfism(Mmod)
        XMInn(Ib) = XMInnm(Mmod)
        GAMma = GAMmafism(Mmod)
        DELp = DELtafism(Mmod)
        shcf = SHCfism(Mmod)
        iff = BFFm(Mmod)
        desteppp = DEStepm(Mmod)
        vibbf12 = VIBf12m(Mmod)
        vibbfdt = VIBfdtm(Mmod)
        vn = VIBfnormm(Mmod)
      ENDIF
 
      GAMma = GAMma/A(Nnuc)**0.333333
      ATIl = AP1*A(Nnuc) + AP2*A23
      ATIl = ATIl*Rafis
      ar = ATIl*(1.0 + shcf*GAMma)
C
      DELp = 14./SQRT(A(Nnuc))
      DEL = 0.
      IF(FISden(Nnuc).LE.1.)THEN
        IF(MOD(in,2).NE.0)DEL = DELp
        IF(MOD(iz,2).NE.0)DEL = DEL + DELp
      ENDIF
C
      aj = 0.
      u = 0.
C     ROFisp(nfisenmax,ndlw,2,nfhump),
      DO kk = 1, NDEx
        DO i = 1, NDLw
          ROFisp(kk,i,1,Ib) = 0.D0
          ROFisp(kk,i,2,Ib) = 0.D0
        ENDDO
      ENDDO
 
      def2 = DEFfis(Ib) ! the actual static saddle deformation is used
      IF(def2.GT.1.5D0)THEN
        WRITE(8,*)
     &       ' WARNING: Deformation reset to 1.5 for fission barrier b='
     &       , Ib
        WRITE(8,*)
     &       ' WARNING:  for fission LD calculations (SIGMAK,RODEFF)   '
        def2 = 1.5D0
      ENDIF
 
      IF(iff.EQ.2)THEN
C       Non-axial, non-axiality parameter assumed as 10 degrees
        CALL SIGMAK(A(Nnuc),Z(Nnuc),def2, - 2.D0,u,ar,aj,mompar,momort,
     &              A2,stab,cigor)
      ELSE
C       Axial symmetry
        CALL SIGMAK(A(Nnuc),Z(Nnuc),def2, - 1.D0,u,ar,aj,mompar,momort,
     &              A2,stab,cigor)
      ENDIF
C
C-----calculation of level density parameter 'a' including surface
C-----dependent factor
      ATIl = AP1*A(Nnuc) + BSQ(cigor)*AP2*A23
      ATIl = ATIl*Rafis
 
      CALL DAMIRO_CRT(ia,iz,shcf,IOUt,1)
 
      MOMparcrt = mompar
      MOMortcrt = momort
 
      IF(mompar.LT.0.0D0.OR.momort.LT.0.0D0)THEN
        WRITE(8,*)'WARNING: Negative moment of inertia for spin ', aj
        WRITE(8,*)'WARNING: 0 level density returned by rodef'
        RETURN
      ENDIF
 
      DO jj = 1, NLW
        aaj = FLOAT(jj) + HIS(Nnuc)
        DO kk = 1, NRBinfis(Ib)
          rotemp = 0.D0
          u = XMInn(Ib) + (kk - 1)*desteppp + DEL + dshiff
          IF(u.GT.UCRt)THEN
            u = u - ECOnd
            accn = ATIl*FSHELL(u,shcf,GAMma)
            IF(accn.LE.0.0D0)RETURN
 
            rotemp = RODEFF(A(Nnuc),u,accn,aaj,mompar,momort,HIS(Nnuc),
     &               EXPmax,temp,def2,vibbf12,vibbfdt,vn)
          ELSE
            accn = ACRt
            rotemp = ROBCSF(A(Nnuc),u,aaj,MOMparcrt,MOMortcrt,mompar,
     &               temp,def2,vibbf12,vibbfdt,vn)
          ENDIF
C-----------SYMMETRY ENHANCEMENT
C-----------The normal GS is axial and mass symmetric
 
C-----------Nonaxial symmetry with mass symmetry
          IF(iff.EQ.2)rotemp = rotemp*SQRT(PI/2.)*SQRT(mompar*temp)
C---------- Axial symmetry with mass asymmetry
          IF(iff.EQ.3)rotemp = rotemp*2.D0
C
          ROFisp(kk,jj,1,Ib) = rotemp
          ROFisp(kk,jj,2,Ib) = rotemp
 
          IF(Mmod.GT.0)ROFism(kk,jj,Mmod) = rotemp
        ENDDO
      ENDDO
 
      IF(Mmod.EQ.0)THEN
 
        ACRtf(Ib) = ACRt
        UCRtf(Ib) = UCRt
        ECOndf(Ib) = ECOnd
        DETcrtf(Ib) = DETcrt
        TCRtf(Ib) = TCRt
        SCRtf(Ib) = SCR
 
        VIBf12(Ib) = vibbf12
        VIBfdt(Ib) = vibbfdt
        VIBfnorm(Ib) = vn
 
      ELSE ! Mmod.GT.0
 
        ACRtf(Mmod) = ACRt
        UCRtf(Mmod) = UCRt
        ECOndf(Mmod) = ECOnd
        DETcrtf(Mmod) = DETcrt
        TCRtf(Mmod) = TCRt
        SCRtf(Mmod) = SCR
 
        VIBf12(Mmod) = vibbf12
        VIBfdt(Mmod) = vibbfdt
        VIBfnorm(Mmod) = vn
 
      ENDIF
 
      IF(IOUt.EQ.6)CALL PLOT_ZVV_SADLD(Nnuc,Ib)
      RETURN
      END SUBROUTINE DAMI_ROFIS
 
!---------------------------------------------------------------------------
C
C==============================================================
      FUNCTION ROBCSF(A,U,Aj,Mompar,Momort,Mompr,T,Def2,Vibbf12,Vibbfdt,
     &                Vn)
C==============================================================
 
      IMPLICIT REAL*8(A - H), REAL*8(O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: MOMo, MOMp
      COMMON /MOM   / MOMo, MOMp
C
C Dummy arguments
C
      REAL*8 :: A, Aj, Def2, Momort, Mompar, Mompr, T, U, Vibbf12, 
     &          Vibbfdt, Vn
      REAL*8 :: ROBCSF
C
C Local variables
C
      REAL*8 :: bf, ro, rot_k, rot_q, vib_kq
      REAL*8 :: ROBCS
C
C*** End of declarations rewritten by SPAG
C
 
      ROBCSF = 0.D0                                             ! CRIT
      bf = 0.D0
C
      ro = ROBCS(A,U,Aj,Mompar,Momort,Def2,T,bf)
      Mompr = MOMp
      CALL COLL_KQ_FIS(0,A,T,MOMo,Def2,U,vib_kq,rot_k,rot_q,Vibbf12,
     &                 Vibbfdt,Vn)
      ROBCSF = ro*rot_k*rot_q*vib_kq
      RETURN
      END FUNCTION ROBCSF
 
!---------------------------------------------------------------------------
C
C
C==============================================================
      FUNCTION RODEFF(A,E,Ac,Aj,Mompar,Momort,Ss,Expmax,T,Def2,Vibbf12,
     &                Vibbfdt,Vn)
C==============================================================
      IMPLICIT REAL*8(A - H), REAL*8(O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      REAL*8 :: A, Ac, Aj, E, Expmax, Momort, Mompar, Ss, T, Vibbf12, 
     &          Vibbfdt, Vn
      REAL*8 :: Def2
      REAL*8 :: RODEFF
C
C Local variables
C
      REAL*8 :: bf, e1, ro, rot_k, rot_q, vib_kq, yrast
      INTEGER :: egsm
      REAL*8 :: RODEF
C
C*** End of declarations rewritten by SPAG
C
C Dummy arguments
 
      Expmax = 700.
      bf = 0.D0
      IF(Ac.LE.0..OR.E.LE.0.D0)RETURN
C-----Fission LD: EGSM - J>>K (egsm=0) and EGSM (egsm=1)
 
      egsm = 0
      RODEFF = 0.D0
      yrast = 0.D0
      ro = RODEF(A,E,Ac,Aj,Mompar,Momort,T,yrast,Ss,bf,Expmax,Def2,egsm)
      e1 = E - yrast
      CALL COLL_KQ_FIS(egsm,A,T,Momort,Def2,e1,vib_kq,rot_k,rot_q,
     &                 Vibbf12,Vibbfdt,Vn)
      RODEFF = ro*rot_k*rot_q*vib_kq
 
      RETURN
      END FUNCTION RODEFF
 
!---------------------------------------------------------------------------
 
C====================================================================
      SUBROUTINE COLL_KQ_FIS(Egsm,A,T,Momo,Def2,E1,Vib_kq,Rot_k,Rot_q,
     &                       Thalf,Dt,Vn)
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      REAL*8 :: A, Def2, Dt, E1, Momo, Rot_k, Rot_q, T, Thalf, Vib_kq, 
     &          Vn
      INTEGER :: Egsm
C
C Local variables
C
      REAL*8 :: arg, cost, dmpdiff, dmphalf, ht, m0, pi, qr, qv, r0, 
     &          sdrop, vibrk
C
C*** End of declarations rewritten by SPAG
C
C====================================================================
 
C-----vib_K
      DATA m0, pi, r0, ht/1.044, 3.141592, 1.26, 6.589/
      sdrop = 17./(4.*pi*r0**2)
      cost = 3.*m0*A/(4.*pi*ht**2*sdrop)
      vibrk = Vn*4.D0*EXP(1.7*cost**(2./3.)*T**(4./3.))
C-----vib_Q
      arg = (T - Thalf)/Dt
      qv = 1.0/(EXP((-arg)) + 1.0)
      IF(qv.GE.0.999D0)vibrk = 1.0
      Vib_kq = qv - vibrk*(qv - 1.)
C-----rot_K
      Rot_k = 1.0
      IF(Egsm.EQ.0)Rot_k = Momo*T
C-----rot_Q
      dmphalf = 120.D0*A**0.333*Def2**2         !according to RIPL
      dmpdiff = 1400.*A**( - 0.666)*Def2**2
      qr = 1./(1. + EXP((-dmphalf/dmpdiff)))
     &     - 1./(1. + EXP((E1-dmphalf)/dmpdiff))
      Rot_q = 1.0 - qr*(1.0 - 1.0/(Momo*T))
      RETURN
      END SUBROUTINE COLL_KQ_FIS
 
!---------------------------------------------------------------------------
C
 
      SUBROUTINE ROGSM_SYS(Nnuc,Ap1,Ap2,Gamma,Del,Delp,Om2,Om3,Cga)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      REAL*8 :: Ap1, Ap2, Cga, Del, Delp, Gamma, Om2, Om3
      INTEGER :: Nnuc
C
C Local variables
C
      REAL*8 :: a13, a23, arobn, dshif
      REAL :: FLOAT
      REAL :: gamm
      INTEGER :: ia, iz
      INTEGER :: INT
C
C*** End of declarations rewritten by SPAG
C
Cccc
Cccc  ********************************************************************
Cccc  *                    R O G S M_sys                                 *
Cccc  *                                                                  *
Cccc  * GSM level density systematics  from RIPL-2             .         *
Cccc  *                                                                  *
Cccc  ********************************************************************
 
C Local variables
 
      ia = INT(A(Nnuc))
      iz = INT(Z(Nnuc))
      a23 = FLOAT(ia)**0.666667D0
      a13 = FLOAT(ia)**0.333333D0
 
 
      arobn = ATIl_ig(Nnuc)
      Delp = DELp_ig(Nnuc)
      Om2 = OM2_ig(Nnuc)
      IF(Om2.LE.0.D0)Om2 = 30./FLOAT(ia)**.66666
 
      dshif = DSHift_ig(Nnuc)
      IF(arobn.LE.0.D0)dshif = 0.617 - 0.00164*FLOAT(ia)
 
      gamm = 0.375
      Gamma = gamm/a13
 
      Om3 = 50./a23
      Cga = .0075*a13
 
      Ap1 = 0.103
      Ap2 = -0.105
 
      Delp = 12./SQRT(A(Nnuc))
      Del = 0.D0
      IF(MOD((ia-iz),2).NE.0.0D0)Del = Delp
      IF(MOD(iz,2).NE.0.0D0)Del = Del + Delp
 
      Del = Del + dshif
      RETURN
      END SUBROUTINE ROGSM_SYS
 
!---------------------------------------------------------------------------
 
      SUBROUTINE EGSMSYS(Ap1,Ap2,Gamma,Del,Delp,Nnuc)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      REAL*8 :: Ap1, Ap2, Del, Delp, Gamma
      INTEGER :: Nnuc
C
C Local variables
C
      REAL*8 :: gam
      INTEGER :: INT
C
C*** End of declarations rewritten by SPAG
C
Cccc
Cccc  ********************************************************************
Cccc  *                                                          class:au*
Cccc  *                    E G S M s y s                                 *
Cccc  *                                                                  *
Cccc  * EGSM level density systematics fitted to Do from RIPL-3.         *
Cccc  *                                                                  *
Cccc  * Set coefficients in the level-density-parameter formula          *
Cccc  * used in the EMPIRE-specific (EGSM) model:                        *
Cccc  * atil = ap1*A(Nnuc) + ap2*A(nnuc)**2/3                            *
Cccc  * gamma = gam/A(nnuc)**1/3                                         *
Cccc  *                                                                  *
Cccc  * Using liquid drop vibrational enhancement factor (EMPIRE-2.19)   *
Cccc  *                                                                  *
Cccc  * MINUIT fit results:                                              *
Cccc  * EXT PARAMETER                                                    *
Cccc  * NO.   NAME        VALUE                                          *
Cccc  *  1     A1        0.74055E-01                                     *
Cccc  *  2     A2        0.28598E-03                                     *
Cccc  *  3     gam       0.57248                                         *
Cccc  *                                                                  *
Cccc  *  frm=1.70   Chi**2=36 (per degree of freedom)                    *
Cccc  *                                                                  *
Cccc  * author: M.Herman                                                 *
Cccc  * date:   December 2008                                            *
Cccc  ********************************************************************
 
      Del = 0.D0
      Delp = 12./SQRT(A(Nnuc))
      IF(MOD(XN(Nnuc),2.D0).NE.0.0D0)Del = Delp
      IF(MOD(Z(Nnuc),2.D0).NE.0.0D0)Del = Del + Delp
C****************************************************************
Cccc  * MINUIT fit results:
C
C-----parameters of Dec 4, 2008
C     frm=1.70   Chi**2=36 (per degree of freedom)
C     ap1 = 0.74055E-01
C     ap2 = 0.28598E-03
C     gam = 0.57248
C
C****************************************************************
C
C-----parameters of Jan 26, 2011
C  Do-fit using RIPL-3 database, 2.19 vibr enhancement (MINUIT)
C     alpha 0=  .0750000 delta alpha= .500000D-01
C     gam   0=  .5750000 delta gam  = .500000D-02
C ---------------------------------
C alpha=   7.488729E-02 gam=   5.697688E-01
C frm=       1.687021929004768 Chi^2=      27.301609174895010
C     ap1= 7.488729D-02
C     ap2= 0.d0
C     gam= 5.697688D-01
C
C     EGSM model is used
C****************************************************************
C
C-----parameters of Jan 16, 2012 (EMPIRE v.2182 for LD routines)
C  Do-fit using RIPL-3 database, 2.19 vibr enhancement
C     alpha 0=  .0750000 delta alpha= .500000D-01
C     gam   0=  .5750000 delta gam  = .500000D-02
C---------------------------------
C alpha=   7.481815E-02  beta=   0.000000E+00  gam=   5.609533E-01
C frm=       1.672033623535850 Chi^2=      27.242032055693340
C Ncalc=        255 Nres=        300 N(Ucrt>Bn)=         48
C 48 nuclei having Ucrt>Bn skipped in the MINUIT fit
C
C     EGSM (J>>K approx.)
C     Closed formula for spin dependence instead of sum_K
C
      Ap1 = 7.481815D-02
      Ap2 = 0.D0
      gam = 5.609533D-01
      Gamma = gam/A(Nnuc)**(1.D0/3.D0)
      IF(ATIlnoz(INT(Z(Nnuc))).EQ.0.D0)RETURN
      Ap1 = Ap1*ATIlnoz(INT(Z(Nnuc))) !apply elemental normalization factor
      Ap2 = Ap2*ATIlnoz(INT(Z(Nnuc))) !apply elemental normalization factor
      RETURN
      END SUBROUTINE EGSMSYS
