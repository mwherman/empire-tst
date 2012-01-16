Ccc   * $Author: mherman $
Ccc   * $Date: 2012-01-16 02:52:02 +0100 (Mo, 16 Jän 2012) $
Ccc   * $Id: lev-dens.f,v 1.77 2009/08/03 00:35:20 Capote Exp $
C
C
C
      SUBROUTINE ROEMP(Nnuc,Cf,Asaf)
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
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C COMMON variables
C
      REAL*8 TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl    ! CRIT
      REAL*8 AP1, AP2, GAMma, DEL, DELp, BF, A23, A2            ! PARAM
      INTEGER NLWst                                             ! PARAM

      COMMON /CRIT  / TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl
      COMMON /PARAM / AP1, AP2, GAMma, DEL, DELp, BF, A23, A2, NLWst
C
C Dummy arguments
C
      REAL*8 Asaf, Cf
      INTEGER Nnuc
C
C Local variables
C
      REAL*8 aj, defit, dshif, dshift, ellq, pi2, rotemp, Ecrt
 
      REAL FLOAT
      INTEGER ia, in, iz, kk, nplot
      INTEGER INT

      pi2 = PI*PI
      BF = 1.0
      IF (Cf.NE.0.0D0) BF = 0.0D0
      A23 = A(Nnuc)**0.666667
      ia = INT(A(Nnuc))
      iz = INT(Z(Nnuc))
      in = ia - iz

      IF (NEX(Nnuc).LE.0.0D0 .AND. FITlev.EQ.0) THEN
         WRITE (8,
     &   '('' EXCITATION ENERGY TABLE FOR A='',I3,'' Z='',I3,         ''
     &    HAS NOT BEEN DETERMINED BEFORE CALL OF PRERO'',//
     &   ,'' LEVEL DENSITIES WILL NOT BE CALCULATED'')') ia, iz
         RETURN
      ENDIF

      IF (EX(NEX(Nnuc),Nnuc).LE.0.0D0 .AND. FITlev.EQ.0) RETURN
      CALL PRERORITO(Nnuc)
      Nlwst=NLW     

      IF(FISSHI(Nnuc).EQ.1.d0.OR.Aejc(0).GT.4)CALL PRERO(Nnuc)

C-----set level density parameter systematics
C-----EMPIRE-3.0-dependence
      CALL EGSMsys(ap1,ap2,gamma,del,delp,nnuc)
      IF (BF.EQ.0.0D0 .AND. Asaf.GE.0.0D0) GAMma = Asaf
      ATIl = AP1*FLOAT(ia) + AP2*A23
      ATIl = ATIl*ATIlnor(Nnuc)
c-----calculate crtical values
      CALL DAMIRO_CRT(ia,iz,shc(nnuc),IOUt,0)
      IF (BF.EQ.0.D0 .AND. Asaf.LT.0.0D0) ACR = ACRt
C-----fit of cumulative low-lying discrete levels
      IF(BF.NE.0.d0)Call LEVFIT(Nnuc,Nplot,Dshif,Dshift, Defit)
      IF(IOUt.eq.6 .and.NLV(Nnuc).GT.3) 
     &   Call PLOT_ZVV_NumCumul(Nnuc, Defit,nplot, NLwst)
      IF (FITlev.GT.0.0D0 .AND. NLV(Nnuc).GT.3 .AND. RORed.GT.0) 
     &  CALL PLOT_GNU_NumCumul(Nnuc,Nplot,Defit,dshift,DEL,
     &                         NLwst)

      IF (Q(1,Nnuc).EQ.0.0D0) THEN
         REWIND (25)
         CALL BNDG(1,Nnuc,Q(1,Nnuc))
      ENDIF

      ellq = Q(1,Nnuc) - (ELV(NLV(Nnuc),Nnuc) + LDShif(Nnuc))
      Ecrt = UCRt - DEL - dshift
      DO kk = 1, NEX(Nnuc)
         IF (FITlev.LE.0.0D0 .OR. 
     &       EX(kk,Nnuc).GE.ELV(NLV(Nnuc),Nnuc)+ LDShif(Nnuc))
     &      THEN
            IF (EX(kk,Nnuc).LT.Q(1,Nnuc) .AND. ellq.NE.0.0D0) THEN
               dshif = dshift*(Q(1,Nnuc) - EX(kk,Nnuc))/ellq
            ELSE
               dshif = 0.d0
            ENDIF
            IF(BF.EQ.0.0d0)THEN
               CALL DAMIRO_FISHI(kk,Nnuc,Asaf,rotemp,aj)
            ELSE
               CALL DAMIRO(kk,Nnuc,dshif,0.0D0,rotemp,aj)
            ENDIF
         ENDIF
      ENDDO

      IF(IOUt.eq.6) CALL PLOT_ZVV_GSLD(Nnuc)
      
C     Added INITIALIZATION for ROPar(1,Nnuc) and ROPar(3,Nnuc)
      ROPar(1,Nnuc) = ACR
      ROPar(3,Nnuc) = del

      
      RETURN
      END

CCC
CCC   *****************************************************************
      SUBROUTINE DAMIRO(Kk,Nnuc,Dshif,Destep,Rotemp,Aj)
CCC   *****************************************************************

      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C COMMON variables
C
      REAL*8 TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl    ! CRIT
      REAL*8 AP1, AP2, GAMma, DEL, DELp, BF, A23, A2            ! PARAM
      INTEGER NLWst                                             ! PARAM
 
      COMMON /CRIT  / TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl
      COMMON /PARAM / AP1, AP2, GAMma, DEL, DELp, BF, A23, A2, NLWst
C
C Dummy arguments
C
      REAL*8 Aj, Destep, Dshif, Rotemp
      INTEGER Kk, Nnuc
C
C Local variables
C
      REAL*8 ac, bsq, cigor, momort, mompar, stab, t, u

c      REAL*8 erac,arac,tconst,rofgrac,e0,urac,sigg,u1
      LOGICAL bcs
      REAL*8 FSHELL, ROBCS, RODEF
      INTEGER i, ia, iz,egsm,lazy
      INTEGER INT

      bcs = .TRUE.
c-----GSM (egsm=0) and EGSM (egsm=1)
      egsm=1
      lazy=0
      ia = INT(A(Nnuc))
      iz = INT(Z(Nnuc))

      IF (Destep.NE.0.0D0) THEN
          u = (Kk - 1)*Destep+ DEL + Dshif
      ELSE
          u = EX(Kk,Nnuc)+ DEL + Dshif
      ENDIF
      IF (u.LE.0.0D0)RETURN
      IF (u.GT.UCRt) THEN
         u = u - ECOnd
         IF (u.LE.0.0D0) RETURN
         bcs = .FALSE.
      ELSE
         bcs = .TRUE.
      ENDIF

      IF(lazy.EQ.1)THEN
         aj=0.
         CALL SIGMAK(A(Nnuc),Z(Nnuc),DEF(1,Nnuc),1.0D0,u,ac,Aj,
     &               mompar,momort,A2,stab,cigor)
      ENDIF
C-----do loop over angular momentum
      DO i = 1, NLWst
         Aj = REAL(i) + HIS(Nnuc)
C--------spin  dependent moments of inertia for yrast states by Karwowski
C--------(spin dependent deformation beta calculated according to B.-Mot.)
C--------temporary value of 'a' parameter needed for ground state deformation
C--------damping (no surface correction)
         IF(lazy.EQ.1)goto 344
         
         ATIl = AP1*A(Nnuc) + AP2*A23          
         ATIl = ATIl*ATIlnor(Nnuc)  
         ac = ATIl*FSHELL(u,SHC(Nnuc),GAMma)
C--------here FSHELL can become negative
         IF (ac.LE.0.0D0) RETURN      
         CALL SIGMAK(A(Nnuc),Z(Nnuc),DEF(1,Nnuc),1.0D0,u,ac,Aj,
     &               mompar,momort,A2,stab,cigor)
C--------'a' including surface dependent factor
         ATIl = AP1*A(Nnuc) + AP2*A23*BSQ(cigor)
         ATIl = ATIl*ATIlnor(Nnuc)
 344     ac = ATIl*FSHELL(u,SHC(Nnuc),GAMma)
         IF (ac.LE.0.0D0) RETURN
     
         IF (A2.LT.0.D0) THEN
             BF = 1
         ELSE
             BF = 2
         ENDIF
C
        IF (bcs) THEN
            Rotemp = ROBCS(A(Nnuc),u,Aj,mompar,momort,A2,T,BF)* RORed
         ELSE
            Rotemp = RODEF(A(Nnuc),u,ac,Aj,mompar,momort,T,
     &               YRAst(i,Nnuc),HIS(Nnuc),BF,ARGred,EXPmax,a2,egsm)
         ENDIF
        
 346     RO(Kk,i,1,Nnuc) = Rotemp
         RO(Kk,i,2,Nnuc) = Rotemp
 
         IF (i.EQ.1) TNUc(Kk,Nnuc) = t
      ENDDO
      RETURN
      END

CCC
CCC
      REAL*8 FUNCTION ROBCS(A,U,Aj,Mompar,Momort,A2,T,bf)
      IMPLICIT REAL*8 (A-H,O-Z)
CCC   ********************************************************************
CCC   *                                                         CLASS:APU*
CCC   *                        R O B C S                                 *
CCC   * Calculates level densities in the framework of the BCS model     *
CCC   *                                                                  *
CCC   ********************************************************************
C
C COMMON variables
C
      REAL*8 TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl    ! CRIT
      REAL*8 momo,momp
      COMMON /CRIT  / TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl
      COMMON /MOM/momo,momp
C Dummy arguments
C
      REAL*8 A, A2, Aj, U, Momort, Mompar,bf!,denopt!,bet     
C
C Local variables
C
      REAL*8 const, det, dphi2, phi, phi2,
     &       s, seff2, t, ro_u, ro_j
      REAL*8 rot_K,rot_Q,vib_KQ, exp1
C
C-----CONST=1/(2*SQRT(2 PI))
      DATA const/0.199471D0/

      ROBCS = 0.D0
      dphi2 = U/UCRt
      phi2 = 1.D0 - dphi2
      phi = DSQRT(phi2)
      t = 2.D0*TCRt*phi/LOG((phi + 1.D0)/(1.D0 - phi))
      s = SCR*TCRt*dphi2/t
      det = DETcrt*dphi2*(1.D0 + phi2)**2
      momp = Mompar*TCRt*dphi2/t
      IF (momp.LT.0.0D0)momp=2.d0! RETURN
      momo = Momort*0.3333D0 + 0.6666D0*Momort*TCRt*dphi2/t
      IF (momo.LT.0.0D0) RETURN
      seff2 = momp*t
      IF (ABS(A2).GT.0.005D0) seff2 = momp**0.333D0*momo**0.6666D0*t
      IF (seff2.LE.0.0D0) RETURN
      ro_u=dexp(s)/dsqrt(det)
      exp1=(Aj+0.5)**2/(2.d0*seff2)
      IF(exp1.gt.20d0) RETURN
      ro_j=const*(2.d0*Aj + 1.d0)/seff2**1.5*DEXP(-exp1)
      IF(ro_j.LT.1d-15) RETURN
!      ro_pi=0.5D0
!      ROBCS = ro_u * ro_j * ro_pi 
      ROBCS = ro_u * ro_j * 0.5d0
      IF(Bf.EQ.0.d0) RETURN
      CALL COLL_KQ_EGSM(A,T,momo,A2,u,vib_KQ,rot_K,rot_Q)
      ROBCS = ROBCS *  rot_K * rot_Q * vib_KQ
   
      RETURN
      END
 
CCC
CCC
      REAL*8 FUNCTION RODEF(A,E,Ac,Aj,Mompar,Momort,T,
     &                        Yrast,Ss,Bf,Argred,Expmax,a2,egsm)


      IMPLICIT REAL*8 (A-H,O-Z)
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
      REAL*8 A, Ac, Aj, Argred, Bf, E, Expmax, Momort, Mompar,
     &       Ss, Yrast,a2
C
C Local variables
C
      REAL*8 ak, arg, con, const, e1, seff, sort2, sum, t, u
      INTEGER i, k, kmin,egsm
      REAL*8 s,det,seff2, ro,ro_u,ro_j,ro_pi,pi
      REAL*8 rot_K, rot_Q, vib_KQ   

      DATA const/0.01473144/,pi/3.14159259d0/
C-----CONST=1.0/(24.0*SQRT(2.0))/2.0
C-----the last 2.0 takes into account parity (half to half)
C-----BF controls shape of the nucleus
C-----BF=0. stands for the saddle point         (rot. perpend. to symm.)
C-----BF=1. stands for the oblate yrast state   (rot. paralel  to symm.)
C-----BF=2. stands for the prolate yrast state  (rot. perpend. to symm.)
C-----BF=3. stands for the triaxial yrast state (rot. perpend. to long )

      expmax=700.
      IF(ac.LE.0. .or. e.le.0.d0) RETURN
      egsm=0
c----
      RODEF = 0.D0
      T = DSQRT(E/Ac)
      seff2 = mompar**0.333D0*momort**0.6666D0*t
c-----GSM
      IF(egsm.eq.0)THEN
         S = 2.* Ac * T
         DET = 45.84 * Ac**3 * T**5
         ro_u=exp(s)/sqrt(det)
         ro_j=(1.d0/(2.d0*sqrt(2.d0*pi)))*(2.d0*Aj + 1.d0)/seff2 **1.5*
     &         EXP(-(aj+0.5)**2/(2.d0*seff2))
         ro_pi=0.5
         ro = ro_u * ro_j * ro_pi
         IF(BF.EQ.0.d0)THEN
            RODEF = ro
            RETURN
         ENDIF
         CALL COLL_KQ_EGSM(A,T,momort,A2,e,vib_KQ,rot_K,rot_Q)       
         RODEF = ro * rot_K * rot_Q * vib_KQ 
        RETURN
      ENDIF
c-----EGSM
      sum = 0.0
      IF (Mompar.LT.0.0D0 .OR. Momort.LT.0.0D0) THEN
         WRITE (8,*) 'WARNING: Negative moment of inertia for spin ', Aj
         WRITE (8,*) 'WARNING: 0 level density returned by rodef'
         RETURN
      ENDIF

      IF (Ac.EQ.0.0D0) THEN
         WRITE (8,'('' FATAL: LEVEL DENS. PARAMETER a=0 IN RODEF'')')
         STOP
      ENDIF
      seff = 1.0/Mompar - 1.0/Momort
cms---yrast recalculated
      Yrast = Aj*(Aj + 1.)/(2.* Momort)
      e1 = E - Yrast
      IF (e1.LE.0.0D0) RETURN
      t = SQRT(e1/Ac)       
      sort2 = Momort*t
      const=(16.*sqrt(6.*pi))**(-1)
      con = const/Ac**0.25/SQRT(Mompar*t)
   
      IF (Ss.EQ.( - 1.0D0)) THEN
         arg = 2*SQRT(Ac*e1) - Argred
         IF (arg.LE.( - Expmax)) THEN
            sum = 0.0
         ELSEIF (e1.GT.1.0D0) THEN
            sum = EXP(arg)/e1**1.25
         ELSE
            sum = EXP(arg)
         ENDIF
     
         IF (Aj.LT.1.0D0)  GOTO 100
      ENDIF
      i = Aj + 1.

      IF (Ss.EQ.( - 1.0D0)) THEN
         kmin = 2
      ELSE
         kmin = 1
      ENDIF

c      IF(abs(a2).lt.0.05d0) kmin = i
c      kmin = i
      DO k = kmin, i
         ak = k + Ss
         IF (e1.LE.0.0D0) RETURN
         IF (Bf.NE.1.0D0) THEN
C-----------rotation perpendicular to the symmetry axis (prolate nucleus)
            u = e1 - 0.5*ak**2*seff
         ELSE
C-----------rotation parallel to the symmetry axis (oblate nucleus)
            u = e1 - 0.5*(Aj*(Aj + 1.) - ak**2)*ABS(seff)
         ENDIF
         IF (u.LE.0.0D0) GOTO 100
         arg = 2.0*SQRT(Ac*u) - Argred
         IF (arg.GT.( - Expmax)) THEN
            IF (u.GT.1.0D0) THEN
               sum = sum + 2.0*EXP(arg)/u**1.25
            ELSE
               sum = sum + 2.0*EXP(arg)
            ENDIF
         ENDIF
      ENDDO
 100  ro = con * sum 
      IF(BF.EQ.0.d0)THEN
         RODEF = ro
         RETURN
      ENDIF
 101  CALL COLL_KQ_EGSM(A,T,momort,A2,e1,vib_KQ,rot_K,rot_Q)
c-----rot_K=1       
      RODEF = ro * rot_Q * vib_KQ 
  
      RETURN
      END
CCC
CCC
      SUBROUTINE COLL_KQ_EGSM(A,T,momo,A2,u,vib_KQ,rot_K,rot_Q)
CCC***************************************************************
CCC   Calculates collective enhancements and damping for EGSM and GSM
CCC***************************************************************

      REAL*8 A,T,momo,A2,u,vib_KQ,rot_K,rot_Q 
      REAL*8 qv,qr,vibrk
      real*8 ftmp
C     To avoid Compiler warning
      ftmp = A2 
C-----vibrational enhancement factor (EMPIRE-2.19)
      CALL VIB_K_EGSM(A,t,vibrk)
C-----damping of vibrational effects
      CALL VIB_Q_EGSM(t,qv)
      IF (qv.GE.0.999D0) vibrk = 1.0
      vib_KQ = qv - vibrk*(qv - 1.)
C-----rotational enhancement
cc         IF (ABS(A2).LT.0.05D0)THEN
c            rot_K=1.d0
c            rot_Q=1.d0
c            return
cc         ENDIF
      rot_K  = momo*t
C-----damping of rotational effects       
      CALL ROT_Q_EGSM(u,qr)
      rot_Q  = 1.0 - qr * (1.0 - 1.0/(momo*t))
      RETURN
      END
CCC
CCC
      SUBROUTINE VIB_K_EGSM(A,T,Vibrk)
      IMPLICIT REAL*8 (A-H,O-Z)
CCCC  *****************************************************************
CCCC  *  Liquid drop vibrational enhancement of level densities
CCCC  *****************************************************************
C
C Dummy arguments
C
      REAL*8 A, T, Vibrk
C
C Local variables
C
      REAL*8 cost, ht, m0, pi, r0, sdrop
      DATA m0, pi, r0, ht/1.044, 3.14159259, 1.26, 6.589/
      sdrop = 17./(4.*pi*r0**2)
      cost = 3.*m0*A/(4.*pi*ht**2*sdrop)
      Vibrk = EXP(1.7*cost**(2./3.)*T**(4./3.))
      END

      SUBROUTINE VIB_Q_EGSM(T,Q)
CCC   *****************************************************************
CCC   *         DAMPING FOR VIBRATIONAL EFFECTS                       *
CCC   * Q=0 FOR T=0, Q=1/2 FOR T=THALF    , Q=1 FOR T=INFINITY        *
CCC   *****************************************************************
C
C Dummy arguments
C
      REAL*8 Q, T
C
C Local variables
C
      REAL*8 arg, dt, thalf
      thalf = 1.
      dt = 0.1
      arg = (T - thalf)/dt
      Q = 1.0/(EXP((-arg)) + 1.0)
      END


      SUBROUTINE ROT_Q_EGSM(E1,Qk)
      IMPLICIT REAL*8 (A-H,O-Z)
CCCC  *****************************************************************
CCCC  * damping of rotational  effects with Fermi function independent
CCCC  * of deformation and mass number (consistent with the builtin systematics)
CCCC  *****************************************************************
C
C Dummy arguments
C
      REAL*8 E1, Qk
C
C Local variables
C
      REAL*8 dmpdiff, dmphalf
      Qk = 0.
      dmphalf = 40.
      dmpdiff = 10.
      Qk = 1./(1. + EXP((-dmphalf/dmpdiff)))
     &     - 1./(1. + EXP((E1-dmphalf)/dmpdiff))
      END


C==========================================================
      REAL*8 FUNCTION BSQ(cigor)

      REAL*8 cigor,qigor

      qigor = ( - 0.00246 + 0.3912961*cigor -
     &            0.00536399*cigor**2 - 0.051313*cigor**3 +
     &            0.043075445*cigor**4) - 0.375
      IF (qigor.GT.0.077D0) THEN
          BSQ = 0.983 + 0.439*qigor
      ELSE
          BSQ = 1.0 + 0.4*(cigor - 1.0)**2
      ENDIF
      RETURN
      END   


c****************************************************
      SUBROUTINE DAMIRO_CRT(ia,iz,Shcn,iout,ifis)
C****************************************************

      REAL*8 TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl    ! CRIT
      REAL*8 AP1, AP2, GAMma, DEL, DELp, BF, A23, A2            ! PARAM
      INTEGER NLWst                                             ! PARAM

      COMMON /CRIT  / TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl
      COMMON /PARAM / AP1, AP2, GAMma, DEL, DELp, BF, A23, A2, NLWst

      REAL*8 Shcn
      INTEGER ia,iz,iout,ifis
      CHARACTER*2 SMAT
      REAL*8 FSHELL

      real*8 pi, pi2, xr, ar
      INTEGER ix

      pi=3.141592654D0   
      pi2 = PI*PI
      TCRt = 0.567*DELp

      ar = ATIl*(1.0 + shcn*GAMma)
      DO ix = 1, 20
         xr = ar*TCRt**2
         ACRt = ATIl*FSHELL(xr,SHCn,GAMma)
         IF (ABS(ACRt - ar).LE.0.001D0*ACRt) GOTO 70
         ar = ACRt
      ENDDO
      WRITE (8,*)
     &     ' WARNING: Search for critical a-parameter has not converged
     & for A=',ia,' Z=',iz
      WRITE (8,*) ' WARNING: Last iteration has given acrt=', ACRt
      WRITE (8,*)' WARNING: Setting Acrt to 0.1, execution continues'
      ACRt = max(ACRt,0.1d0)

 70   IF (ACRt.LT.0.0D0) ACRt = 0.1d0
      ECOnd = 1.5*ACRt*DELp**2/pi2
      UCRt = ACRt*TCRt**2 + ECOnd
C-----45.84 stands for (12/SQRT(pi))**2
      DETcrt = 45.84*ACRt**3*TCRt**5
      ACR = ATIl*FSHELL(UCRt,SHCn,GAMma)
      SCR = 2.*ACRt*TCRt

      if(iout.EQ.6.AND.ifis.EQ.0)
     & WRITE(8, '(2X,/,2x,i2,1H-,A2,1H-,i3, '': Atil='', F6.3,
     &      ''  Acrt='',F6.3,''  Ucrt='', F6.3, ''  Econd='', F5.3,
     &      ''  Det='', G11.3, ''  Scrt='',F6.3)')
     &      iz,SMAT(iz),ia,atil,acrt,ucrt,econd,detcrt,scr

      
      RETURN
      END



      SUBROUTINE SIGMAK(A,Z,B,Bf,E,Ac,Aj,Mompar,Momort,A2,Stab,Cigor)
      IMPLICIT REAL*8 (A-H,O-Z)
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
Ccccc *           =  0 saddle point (HI FISSION, AXIAL SYMMETRY)       *
Ccccc *           = -1 saddle point (    AXIAL SYMMETRY)               *
Ccccc *           = -2 saddle point (NON-AXIAL SYMMETRY)               *
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
C
C Dummy arguments
C
      REAL*8 A, A2, Ac, Aj, B, Bf, Cigor,
     &       E, Momort, Mompar, Stab, Z
C
C Local variables
C
      REAL*8 a4, arg, beta, bt, c1, c2, c3, damp,
     &       dt, eta, gamma, pi, r1, r2, r3, ry,
     &       rbmsph, t, tgscr, x, y, ycrit

      pi=3.14159259d0
      IF (A.LE.0.D0) RETURN
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
      IF (y.GT.ycrit) y = ycrit
C-----calculation of dynamic deformation in terms of the ldm
C-----saddle point
      IF (Bf.EQ.0.0D0) THEN
         beta = 7./6.*SQRT(4*pi/5.)*(1.0 - x)
     &           *SQRT(4. - 15.*y/7./(1 - x)**2)
         arg  = 1./SQRT(4. - 15.*y/7./(1 - x)**2)
         IF (arg.GT.1.0D0) arg = 1.0
         gamma = pi/3. - ACOS(arg)
      ELSEIF(Bf.LT.0.0D0) THEN
         beta = bt      ! the actual static saddle deformation is used (must be < 1.5 !!!)
         gamma = pi/3.                    ! axial symmetry
         IF (Bf.LT.-1.50D0) gamma = pi/18. ! arbitrarily fixed asymmetry to 10 degrees
C-----yrast states
      ELSEIF (Bf.GT.0.0D0) THEN
         gamma = pi/3.
         beta  = 7./6.*SQRT(4*pi/5.)*(1.0 - x)
     &           *( - 1. + SQRT(1. + 15.*y/7./(1-x)**2))
         beta  = beta + bt  ! adding damped static deformation 'bt'
      ENDIF

      r3 = 1. + SQRT(5./4./pi)*beta*COS(gamma - 2.*pi)
      r2 = 1. + SQRT(5./4./pi)*beta*COS(gamma - 4.*pi/3.)
      r1 = 1. + SQRT(5./4./pi)*beta*COS(gamma - 2.*pi/3.)

      Cigor = MAX(r1,r2,r3)
      ry=r2/r1
      A2=(ry-1.0)/(1.0+0.5*ry)
      IF (A2.GT.0.9D0) A2 = 0.9
      IF (A2.LT.( - 0.9D0)) A2 = -0.9
C---- next line (if uncommented) neglects all deformations
C     A2=0
      IF (A2.LT.0.0D0) THEN
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
c      mompar = rbmsph*(1. - (2./3.)*a2)
c      momort = rbmsph*(1. + (1./3.)*a2)

      IF (ABS(A2).LE.0.001D0) Momort = Mompar
      END

      REAL*8 FUNCTION FSHELL(X,Xs,Xg)
C
C Dummy arguments
C
      REAL*8 X, Xg, Xs
C
C Dummy arguments
C
      IF (X.GT.0.01D0) THEN
         FSHELL = 1.0 + (1.0 - EXP((-Xg*X)))*Xs/X
      ELSE
         FSHELL = 1 + Xg*Xs
      ENDIF
      RETURN
      END



c*********************************************************
      SUBROUTINE ROGSM(nnuc)
c*********************************************************

      INCLUDE 'dimension.h'
      INCLUDE 'global.h'

C
C COMMON variables

      REAL*8 TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl    ! CRIT
      REAL*8 AP1, AP2, GAMma, DEL, DELp, BF, A23, A2            ! PARAM
      INTEGER NLWst                                             ! PARAM

      COMMON /CRIT  / TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl
      COMMON /PARAM / AP1, AP2, GAMma, DEL, DELp, BF, A23, A2, NLWst

      REAL*8  Momort, Mompar, U, rotemp, shcn, def2
      INTEGER ia,iz,kk,nnuc
C Local variables
      REAL*8 mm2,aas,gamm,pi2,dshif,defit
      REAL*8 om2 

      INTEGER nplot
     
      pi2 = PI*PI
      ia = INT(A(Nnuc))
      iz = INT(Z(Nnuc))

      CALL ROGSM_sys(Aas,gamm,del,delp,shcn,om2,dshif,Nnuc)
      dshif=0.d0

      gamma=gamm
      atil=aas
      CALL PRERORITO(Nnuc)
      NLWst = NLW
c     Jstab(Nnuc)=NDLW
      CALL DAMIRO_CRT(ia,iz,shcn,IOUt,0)
      def2=def(1,nnuc)
      rotemp = 0.d0
     
      mm2=.24*A(Nnuc)**.66666
      Mompar = 0.608 * ACRt * mm2 * (1.- 0.6667 * def2)
      Momort = 0.608 * ACRt * mm2 * (1.+ 0.3330 * def2)      

      defit = (ELV(NLV(Nnuc),Nnuc)+dshif +2.d0)
     &           /(NEXreq-1) 
      nplot = (ELV(NLV(Nnuc),Nnuc)+2.d0)/defit

      IF(IOUt.eq.6 .and.NLV(Nnuc).GT.3)THEN    
         DO kk = 1, NEXreq     
            u = (Kk - 1)*Defit+ DEL + Dshif
            CALL BCS_FG(Nnuc,kk,U,Mompar,Momort,Nlwst,def2)
         ENDDO
         Call PLOT_ZVV_NumCumul(Nnuc,Defit,nplot, NLWst)   
      ENDIF
   
      DO kk = 1, NEXreq
          u = EX(Kk,Nnuc)+ DEL + Dshif
          CALL BCS_FG(Nnuc,kk,U,Mompar,Momort,Nlwst,def2)
 110   ENDDO  

       IF(IOUt.eq.6 ) CALL PLOT_ZVV_GSLD(Nnuc)
    
C      Added INITIALIZATION for ROPar(1,Nnuc) and ROPar(3,Nnuc)
       ROPar(1,Nnuc) = ACR
       ROPar(3,Nnuc) = del

      RETURN
      END


      SUBROUTINE BCS_FG(Nnuc,kk,U, Mompar,Momort,Nlwst,def2)

      INCLUDE 'dimension.h'
      INCLUDE 'global.h'

      COMMON /CRIT  / TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl

      REAL*8 TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl ! CRIT


      REAL*8  Aj, Momort, Mompar, U, Momp, T,rotemp,
     &                 shcn,def2
      INTEGER kk,nnuc
C Local variables
      REAL*8 arg, const, det, momo, phi, phi2,dphi2,
     &                 vib_KQ, s, seff2,
     &                 seff2ort,mm2,ac,rot_KQ,
     &                 gamm
      REAL*8 rho, ro_u, ro_j, ro_pi
      REAL*8 om2,om3,cga,q2,q3
      REAL*8 FSHELL
      INTEGER fg 
C-----CONST=1/(2*SQRT(2 PI))
      DATA const/0.199471D0/
     
      mm2=.24*A(Nnuc)**.66666
      IF(U.LE.UCRt) THEN
c-----BCS
         dphi2 = U/UCRt
         phi2 = 1.D0 - dphi2
         phi = DSQRT(phi2)
         t = 2.D0*TCRt*phi/LOG((phi + 1.D0)/(1.D0 - phi))
         s = SCR*TCRt*(1.d0-phi2)/t    
         det = DETcrt*(1.d0-phi2)*(1.D0 + phi2)**2
         momp = Mompar*TCRt*(1-phi2)/t           
         IF (momp.LT.0.0D0)RETURN
         momo = Momort*0.3333D0 + 0.6666D0*Momort*TCRt*(1.d0-phi2)/t
         IF (momo.LT.0.0D0) RETURN  
      ELSE
c------FG
         U = U - ECOnd
         fg = 1
         ac = atil * FSHELL(u, shcn,gamm)
         IF(ac.LE.0. .or. U.le.0.d0) RETURN
         T = DSQRT(U/ac)
         S = 2.* Ac * T
         DET = 45.84 * Ac**3 * T**5
         momp = 0.608 * ac * mm2 * (1.- 0.6667 * def2)
         momo = 0.608 * ac * mm2 * (1.+ 0.3330 * def2)
      ENDIF
      seff2 = momp * t
      IF (ABS(def2).GT.0.005D0) seff2 = 
     &                          momp**0.333D0*momo**0.6666D0*t
      IF (seff2.LE.0.0D0) RETURN
      seff2ort = momo * t
c-----collective enhancements
c     IF(om2.le.0.)
      om2 = 30./A(Nnuc)**.66666
      om3 = 50./A(Nnuc)**.66666
      CGA =.0075*A(Nnuc)**.33333
      CALL VIB_KQ_GSM(T,OM2,CGA,5,Q2)
      CALL VIB_KQ_GSM(T,OM3,CGA,7,Q3)
      CALL ROT_KQ_GSM(A(Nnuc),def2,seff2ort,U,rot_KQ)
c      rot_KQ = Qr
      vib_KQ =Q 2*Q3      
      ro_u = exp(s)/sqrt(det)
      ro_pi = 0.5D0
       
      DO i = 1, NLWst
         Aj = REAL(i) + HIS(Nnuc)
 100     arg = s - (Aj + 0.5D0)**2/(2.D0*seff2)
         If(arg.le.0.0D0) cycle!return
         ro_j=const*(2.d0*Aj + 1.d0)/seff2**1.5*
     &        EXP(-(Aj+0.5)**2/(2.d0*seff2))
         rho = ro_u * ro_j * ro_pi
         rotemp = rho * rot_KQ * vib_KQ              
         RO(Kk,i,1,Nnuc) = Rotemp
         RO(Kk,i,2,Nnuc) = Rotemp            
         IF (i.EQ.1) TNUc(Kk,Nnuc) = t
      ENDDO

      RETURN
      END


CCC
CCC

CCC
CCC
      SUBROUTINE VIB_KQ_GSM(T,OM,CGA,LAM,Q)
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
      IMPLICIT REAL*8 (a-h,o-z)
      REAL*8 T,OM,Q
      INTEGER LAM
      REAL*8 cga, gam, fn, U, S
      Q=1.D0
      IF(T.LT.0.01) RETURN
      GAM=cga*(OM**2+(2.*3.141593*T)**2)
      FN=DEXP(-GAM/OM/2.D0)/(DEXP(OM/T)-1.D0)
      IF(FN.LT.0.d0) RETURN
      U=LAM*OM*FN
      S=LAM*((1.+FN)*DLOG(1.+FN)-FN*DLOG(FN))
      Q=DEXP(S-U/T)
      if (Q.lt.1.D0) Q=1.D0
      RETURN
      END



      SUBROUTINE ROT_KQ_GSM(A,BET,SIG4,U,QR)
      IMPLICIT REAL*8 (A-H,O-Z)
C***** QROT INCLUDING DAMPING ***

      UCR1=120.*BET*BET*A**.33333
      DCR1=1400.*BET*BET/A**.66666
      IF(BET.GT.0.d0)THEN
         Qr=1./(1.+EXP((U-UCR1)/DCR1))
 11      QR=Qr*(SIG4-1.)+1.
      ELSE
         Qr=1.d0
         goto 12
      ENDIF
   12 RETURN
      END




      SUBROUTINE PRERORITO(Nnuc)
CCC   ********************************************************************

CCC
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C COMMON variables
C
      REAL*8 AP1, AP2, GAMma, DEL, DELp, BF, A23, A2            ! PARAM
      INTEGER NLWst                                             ! PARAM
      COMMON /PARAM / AP1, AP2, GAMma, DEL, DELp, BF, A23, A2, NLWst
C
C Dummy arguments
C
      INTEGER Nnuc
C
C Local variables
C
      REAL*8 ac, arg 
      INTEGER i, ia, iz, k
      INTEGER INT

C-----check of the input data ---------------------------------------
      ia = INT(A(Nnuc))
      iz = INT(Z(Nnuc))
      IF (NLW.LE.0) THEN
         WRITE (8,
     &'('' MAXIMUM NUMBER OF PARTIAL WAVES HAS NOT BEEN'',             '
     &' DETRMINED BEFORE CALL OF PRERO'',//,                           '
     &' EXECUTION STOPPED'')')
         STOP
      ENDIF
      IF (ia.LE.0 .OR. iz.LE.0) THEN
         WRITE (8,
     &'('' A='',I3,'' AND/OR Z='',I2,                                ''
     & HAS NOT BEEN DETERMINED BEFORE CALL OF PRERO'',               //,
     &'' EXECUTION STOPPED'')') ia, iz
         STOP
      ENDIF
      IF (Nnuc.GT.NDNUC) THEN
         WRITE (8,
     &'('' PRERO  CALLED FOR A NUCLEUS INDEX NNUC=''                   ,
     &I3,'' WHICH EXCEEDS DIMENSIONS'',/,                              '
     &' CHECK THIS CALL OR INCREASE NDNUC TO'',I4,                     '
     &' IN dimension.h AND RECOMPILE'',//,                             '
     &'EXECUTION STOPPED'')') Nnuc, Nnuc
         STOP
      ENDIF
      IF (EX(NEX(Nnuc),Nnuc).LE.0.0D0 .AND. FITlev.EQ.0) RETURN

C-----setting overall level density scaling factor ------------------
      IF (ARGred.LT.0.0D0) THEN
         i = NEX(Nnuc)
         ac = A(Nnuc)/7.0
         arg = 2*SQRT(EX(i,Nnuc)*ac)
         IF (arg.LT.EXPmax - 1) THEN
            ARGred = 0.
            RORed = 1.
         ELSE
            ARGred = AINT(arg - EXPmax + 1.)
            IF (ARGred.LT.EXPmax) THEN
               RORed = EXP( - ARGred)
            ELSE
               RORed = 0.0
            ENDIF
         ENDIF
      ENDIF

C-----set to 0 level density array
      DO i = 1, NDEX
         DO k = 1, NDLW
            IF (BF.NE.0.0D0) THEN
               RO(i,k,1,Nnuc) = 0.0
               RO(i,k,2,Nnuc) = 0.0
            ELSE
               ROF(i,k,Nnuc) = 0.0
            ENDIF
         ENDDO
      ENDDO
      RETURN
      END

CCC

      SUBROUTINE PRERO(Nnuc)
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
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C COMMON variables
C
      REAL*8 AP1, AP2, GAMma, DEL, DELp, BF, A23, A2            ! PARAM
      INTEGER NLWst                                             ! PARAM
      COMMON /PARAM / AP1, AP2, GAMma, DEL, DELp, BF, A23, A2, NLWst
C
C Dummy arguments
C
      INTEGER Nnuc
C
C Local variables
C
      REAL*8 ac, aj, arg, cigor, fx, momort, mompar, s,
     &       sb, sb0, sbnor, segnor, segs, selmax, stab, x,
     &       x0, x1, xi, xk, ftmp
      REAL FLOAT
      INTEGER i, ia, iz, j, jstabf, k, kstab, ldstab
      INTEGER INT, MIN0
      REAL*8 SHCFADE
C-----check of the input data ---------------------------------------
      ia = INT(A(Nnuc))
      iz = INT(Z(Nnuc))

      IF (NLW.LE.0) THEN
         WRITE (8,
     &'('' MAXIMUM NUMBER OF PARTIAL WAVES HAS NOT BEEN'',             '
     &' DETRMINED BEFORE CALL OF PRERO'',//,                           '
     &' EXECUTION STOPPED'')')
         STOP
      ENDIF
      IF (ia.LE.0 .OR. iz.LE.0) THEN
         WRITE (8,
     &'('' A='',I3,'' AND/OR Z='',I2,                                ''
     & HAS NOT BEEN DETERMINED BEFORE CALL OF PRERO'',               //,
     &'' EXECUTION STOPPED'')') ia, iz
         STOP
      ENDIF
      IF (Nnuc.GT.NDNUC) THEN
         WRITE (8,
     &'('' PRERO  CALLED FOR A NUCLEUS INDEX NNUC=''                   ,
     &I3,'' WHICH EXCEEDS DIMENSIONS'',/,                              '
     &' CHECK THIS CALL OR INCREASE NDNUC TO'',I4,                     '
     &' IN dimension.h AND RECOMPILE'',//,                             '
     &'EXECUTION STOPPED'')') Nnuc, Nnuc
         STOP
      ENDIF

      IF (EX(NEX(Nnuc),Nnuc).LE.0.0D0 .AND. FITlev.EQ.0) RETURN
C-----check of the input data ---- done -----------------------------
      IF(FISSHI(Nnuc).EQ.1.d0)THEN
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
      IF (iz.GT.19 .AND. iz.LT.102) THEN
        CALL BARFIT(iz,ia,0,sb0,segs,stab)
        ldstab = stab
      ELSE
        ldstab = kstab
      ENDIF
      NLWst = NLW   
      IF (HIS(Nnuc).EQ. - 0.5D0) THEN
         ldstab = ldstab - 1
         kstab = kstab - 1
      ENDIF 
    
      IF (FISb(1,Nnuc).EQ.0.0D0) THEN
C-----determination of the fission barrier at J=0 (for Z.GE.102)
C-----according to Myers&Swiatecki, Phys. Rev. C60(1999)014606
         IF (iz.GE.102) THEN
            x0 = 48.5428
            x1 = 34.15
            xi = (A(Nnuc) - 2*Z(Nnuc))/A(Nnuc)
            xk = 1.9 + (Z(Nnuc) - 80.0)/75.0
            s = A(Nnuc)**0.666667*(1.0 - xk*xi**2)
            x = Z(Nnuc)**2/A(Nnuc)/(1.0 - xk*xi**2)
            fx = 0.0
            IF (x.LE.x0 .AND. x.GE.x1) fx = 0.000199749*(x0 - x)**3
            IF (x.LE.x1 .AND. x.GE.30.0D0) fx = 0.595553 -
     &          0.124136*(x - x1)
            sb0 = s*fx
            WRITE (8,
     &'('' Liquid drop fission barrier for '',i3,''-'',A2,         '' se
     &t to '',G10.5)') INT(A(Nnuc)), SYMb(Nnuc), sb0
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
            CALL SIGMAK(A(Nnuc),Z(Nnuc),DEF(1,Nnuc),1.0D0,0.0D0,15.0D0,
     &                  aj,mompar,momort,ftmp,stab,cigor)
C           CALL SIGMAK(A(Nnuc),Z(Nnuc),DEF(1,Nnuc),1.0D0,0.0D0,15.0D0,
C    &                  aj,mompar,momort,beta,stab,cigor)
C           IF (Cf.EQ.0.0D0) DEF(j,Nnuc) = beta ! Commented to avoid using wrong beta out from SIGMAK

            IF (iz.GT.19 .AND. iz.LT.102) THEN
               sb = 0.0
               IF (j - 1.LE.ldstab)
     &             CALL BARFIT(iz,ia,j - 1,sb,segs,selmax)
               IF (j - 1.EQ.ldstab)
     &             segnor = segs/(aj*(aj + 1)/(2.0*momort))
               IF (j - 1.GT.ldstab) segs = aj*(aj + 1)/(2.0*momort)
     &             *segnor
C------------- Yrast states redefined for normal states to avoid discontinuities
C              as proposed by MS, except for HI induced reactions (AJEc(0)>4)
               if(AEJc(0).LE.4.) segs = aj*(aj + 1)/(2.0*momort)   ! Jan 2011
            ELSE
C--------------out of the BARFIT range of applicability;
C--------------fission barrier spin dependence is assumed to be  that of
C--------------A=256 Z=102 and normalized at J=0 to the value of Myers &
C--------------Swiatecki (SB0)
               CALL BARFIT(102,256,j - 1,sb,segs,selmax)
               IF (j.EQ.1) sbnor = sb0/sb
               sb = sb*sbnor
               segs = aj*(aj + 1)/(2.0*momort)
            ENDIF
c            segs = aj*(aj + 1)/(2.0*momort)
            YRAst(j,Nnuc) = segs
            SHCjf(j,Nnuc) = SHCFADE(j - 1,SHRj,SHRd)
            FISb(j,Nnuc) = sb*QFIs + segs
            IF (JSTab(Nnuc).NE.0 .AND. j.GE.JSTab(Nnuc)) GOTO 50
C-----------determination of stability limit including shell correction
            IF (sb*QFIs - SHCjf(j,Nnuc)*SHC(Nnuc).LE.0.001D0) GOTO 50
            jstabf = j
        ENDDO
 50     IF (JSTab(Nnuc).EQ.0) JSTab(Nnuc) = jstabf
      ENDIF
c      IF (JSTab(Nnuc).EQ.0) NLWst = MIN0(JSTab(Nnuc),NLWst)
      NLWst = MIN0(JSTab(Nnuc),NLWst,NLW)

      RETURN
C-----yrast and saddle point energies ----- done ---------------
C-----setting overall level density scaling factor ------------------
      IF (ARGred.LT.0.0D0) THEN
         i = NEX(Nnuc)
         ac = A(Nnuc)/7.0
         arg = 2*SQRT(EX(i,Nnuc)*ac)
         IF (arg.LT.EXPmax - 1) THEN
            ARGred = 0.
            RORed = 1.
         ELSE
            ARGred = AINT(arg - EXPmax + 1.)
            IF (ARGred.LT.EXPmax) THEN
               RORed = EXP( - ARGred)
            ELSE
               RORed = 0.0
            ENDIF
         ENDIF
      ENDIF
C-----setting overall level density scaling factor ----- done -------
C-----set to 0 level density array
      DO i = 1, NDEX
         DO k = 1, NDLW
            IF (BF.NE.0.0D0) THEN
               RO(i,k,1,Nnuc) = 0.0
               RO(i,k,2,Nnuc) = 0.0
            ELSE
               ROF(i,k,Nnuc) = 0.0
            ENDIF
         ENDDO
      ENDDO
C-----setting to 0 level density array ------ done ------
      END




      REAL*8 FUNCTION SHCFADE(J,Shrj,Shrd)
      IMPLICIT REAL*8 (A-H,O-Z)
C
Ccc   ********************************************************************
Ccc   *                                                         CLASS:PPU*
Ccc   *                      S H C F A D E                               *
Ccc   *                                                                  *
Ccc   * calculates angular momentum (J) fade-out of the shell            *
Ccc   * correction to the fission barrier                                *
Ccc   *                                                                  *
Ccc   ********************************************************************
C
C Dummy arguments
C
      INTEGER J
      REAL*8 Shrd, Shrj
C
C Local variables
C
      REAL FLOAT
      SHCFADE = 1.
      IF (Shrd.NE.0.D0) SHCFADE = 1.0/(1.0 + EXP((FLOAT(J)-Shrj)/Shrd))
      RETURN
      END

      SUBROUTINE ROGC(Nnuc,Scutf)
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
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C COMMON variables
C
      REAL*8 A2, A23, AP1, AP2, BF, DEL, DELp, GAMma, eps
      INTEGER NLWst
      COMMON /PARAM / AP1, AP2, GAMma, DEL, DELp, BF, A23, A2, NLWst
      COMMON /CT/ am,ux,eo,T
C
C Dummy arguments
C
      INTEGER Nnuc,nplot
      REAL*8 Scutf,defit
C
C Local variables
C
      REAL*8 am, amas, arg, atil, e, efort, enorm, eo,
     &                 eom, exl, ro_u, ro_j, ro_pi,rolowint, sigh, sigl, 
     &                 t, tm, u, ux, xj
      REAL*8 FSHELL
      INTEGER i, ig, igna, iter, j

      eom = 0.d0
C-----next call prepares for lev. dens. calculations
      CALL PRERO(Nnuc)

      amas = A(Nnuc)
      igna = 0
      ro_pi=0.5
C-----zero potentially undefined variables
      GAMma = 0.d0
      exl = 0.d0
      sigh = 0.d0
C-----a-parameter given in input
      IF (ROPaa(Nnuc).GT.0.0D0) ROPar(1,Nnuc) = ROPaa(Nnuc)
C-----Ignatyuk parametrization
      enorm = 5.d0
      IF (ROPaa(Nnuc).EQ.0.0D0) THEN
         atil = 0.154d0*A(Nnuc) + 6.3d-5*A(Nnuc)**2
C--------next line assures normalization to experimental data (on average)
         atil = atil*ATIlnor(Nnuc)
         GAMma = -0.054d0
         ROPar(1,Nnuc) = atil*FSHELL(enorm,SHC(Nnuc),-GAMma)
c     &     atil*(1.d0 + SHC(Nnuc)*(1.d0 - EXP(GAMma*enorm))/enorm)
         igna = 1
      ENDIF
C-----Arthurs' parametrization
      IF (ROPaa(Nnuc).EQ.( - 1.0D0)) THEN
         atil = 0.1375*A(Nnuc) - 8.36E-5*A(Nnuc)**2
C--------next line assures normalization to experimental data (on average)
         atil = atil*ATIlnor(Nnuc)
         GAMma = -0.054d0
         ROPar(1,Nnuc) = atil*FSHELL(enorm,SHC(Nnuc),-GAMma)
c     &     atil*(1.d0 + SHC(Nnuc)*(1.d0 - EXP(GAMma*enorm))/enorm)
         igna = 1
      ENDIF
C-----Mebel's  parametrization (taken from the INC code for the case
C-----of no collective enhancements) normalized to existing exp. data
      IF (ROPaa(Nnuc).EQ.( - 2.0D0)) THEN
         atil = 0.114*A(Nnuc) + 9.80E-2*A(Nnuc)**0.666667
C--------next line assures normalization to experimental data (on average)
         atil = atil*ATIlnor(Nnuc)
         GAMma = -0.051d0
         ROPar(1,Nnuc) = atil*FSHELL(enorm,SHC(Nnuc),-GAMma)
c     &     atil*(1.d0 + SHC(Nnuc)*(1.d0 - EXP(GAMma*enorm))/enorm)
         igna = 1
      ENDIF
C
C-----If parameters given in input, they are initialized
C
      am = ROPar(1,Nnuc)
      ux = ROPar(2,Nnuc)
      DEL = ROPar(3,Nnuc)
      eo = ROPar(4,Nnuc)
      t = ROPar(5,Nnuc)
C
C-----calculation of nuclear temperature if t=0
C
      IF (t.EQ.0.D0) THEN
         IF (ux.EQ.0.0D0) THEN
            t = 0.9 - 0.0024*amas
            IF (amas.LT.100.D0) t = 60./amas + 0.06
         ELSE
            t = SQRT(am/ux) - 3./2./ux
            t = 1./t
            tm = t  
         ENDIF
      ENDIF
C
C-----calculation of spin cut-off parameter from resolved levels
C
      sigl = 0.
      DO i = 2, NLV(Nnuc)
         sigl = sigl + (ABS(XJLv(i,Nnuc)) + 0.5)**2
      ENDDO
      IF (NLV(Nnuc).GT.1) sigl = sigl/(NLV(Nnuc) - 1)
      sigl = sigl/2.
      IF (sigl.LT.0.5D0) sigl = 0.5
C
C-----calculation of matching point /if UX=0.0/
C
      iter = 0
  100 IF (am*t.LE.6.D0 .OR. iter.GT.300) THEN
         WRITE (8,*) 'WARNING: '
         IF (iter.LT.101) THEN
            WRITE (8,*) 'WARNING: Number of iterations in ROGC ',
     &                  iter
            WRITE (8,*) 'WARNING: Can not calculate Ux'
         ELSE
            WRITE (8,*) 'WARNING: Maximum number if iterations in ROGC'
         ENDIF
         WRITE (8,*) 'WARNING: Level density parameters inconsistent'
         WRITE (8,*) 'WARNING: This may happen if you have used default'
         WRITE (8,*) 'WARNING: systematics for too light nucleus or '
         WRITE (8,*)
     &              'WARNING: have allowed for too many discrete levels'
         WRITE (8,*) 'WARNING: entering the region where these are lost'
         WRITE (8,*) 'WARNING: Reanalise GC l.d. parameters for:'
         WRITE (8,*) 'WARNING: Z=', INT(Z(Nnuc)), '  A=', INT(A(Nnuc))
         WRITE (8,*) 'WARNING: a=', am, ' T=', t
         WRITE (8,*) 'WARNING: I will use the last T=', tm,' for calc.'
         WRITE (8,*) 'WARNING: '
         IF (FITlev.GE.0.0D0) THEN
C-----------set nuclear temperature to the value from the systematics
            t = 0.9 - 0.0024*amas
            IF (amas.LT.100.D0) t = 60./amas + 0.06
            tm = t
c            GOTO 500 !????
         ELSEIF (FITlev.LT.0.0D0) THEN
            WRITE (8,*) ' ERROR IN DISCRETE LEVEL FITTING'
            WRITE (8,*) ' EXECUTION STOPPED BECAUSE OF FITLEV<0 OPTION '
            STOP 'ERROR IN DISCRETE LEVEL FITTING (GC)'
         ENDIF
      ENDIF

      IF (igna.NE.0D0) THEN
         DO i = 1, 10  
C	      write(*,*) '***',a(nnuc),z(nnuc),ux
C	      write(*,*) am, 6/t, atil
            IF (ux.EQ.0.0D0) ux = t*t*(am - 3/t + SQRT((am-6/t)*am))/2.0
            am = atil*FSHELL(ux,SHC(Nnuc),-GAMma)
         ENDDO
      ELSE
         IF (ux.EQ.0.0D0) ux = t*t*(am - 3/t + SQRT((am-6/t)*am))/2.0
      ENDIF

  200 exl = ux + DEL
C-----RCN 12/2004 
C-----IF(Scutf.LT.0.0D0)sigh calculated according to Dilg's recommendations
C-----0.6079 = 6/pi^2   a=6/pi^2*g  sig^2 = <m^2>gt  Scutf = <m^2>
      sigh = Scutf*0.6079*amas**0.6666667*SQRT(ux*am)
C
C-----determination of the index in EX-array such that EX(IG,.).LT.EXL
C-----(low-energy level density formula is used up to IG)
C
      DO i = 1, NEX(Nnuc)
         IF (EX(i,Nnuc).GT.exl) GOTO 300
      ENDDO
      ig = NEX(Nnuc)
      GOTO 400
  300 ig = i - 1
  400 IF (eo.EQ.0.0D0) THEN
         ro_u = DEXP(2.*SQRT(am*ux))/(12.*SQRT(2*sigh))
     &          /am**0.25/ux**1.25
         eo = exl - t*LOG(t*ro_u)
      ENDIF
      eom = eo
C-----fit nuclear temperature (and Ux) to discrete levels
      IF (NLV(Nnuc).GT.5 .AND. ROPar(2,Nnuc).EQ.0.0D0 .AND.
     &    ROPar(5,Nnuc).EQ.0.0D0) THEN
         eps = MIN(NLV(Nnuc)*0.03, 0.5)
         rolowint = EXP(( - eo/t))*(EXP(ELV(NLV(Nnuc),Nnuc)/t) - 1.)
         IF (ABS(rolowint + 1.0 - NLV(Nnuc)).GT.eps) THEN
            tm = t
            t = t + 0.01*LOG((NLV(Nnuc)-1)/EXP((-eo/t))
     &          /(EXP(ELV(NLV(Nnuc),Nnuc)/t) - 1))
            ux = 0.0
            eo = 0.0
            iter = iter + 1
            if (iter.le.300) GOTO 100
         ENDIF
      ENDIF
     
C-----plot fit of the levels with the low energy l.d. formula
      defit = (ELV(NLV(Nnuc),Nnuc)+2.d0)
     &           /(NEXreq-1) 
      nplot = (ELV(NLV(Nnuc),Nnuc)+2.d0)/defit

      IF (FITlev.GT.0.0D0 .AND. NLV(Nnuc).GT.5) 
     &  CALL PLOT_GNU_NumCumul_GC(Nnuc) 
     
 500  ROPar(1,Nnuc) = am
      ROPar(2,Nnuc) = ux
      ROPar(3,Nnuc) = DEL
      ROPar(4,Nnuc) = eo
      ROPar(5,Nnuc) = t
      IF (ig.NE.0) THEN
C-----calculation of level densities below EXL
C-----(low energy formula)
         DO i = 1, ig
            e = EX(i,Nnuc)
            arg = (e - eo)/t - ARGred
            IF (arg.LT.EXPmax) THEN
               ro_u = EXP(arg)/t
C--------------Spin-cutoff is interpolated
               SIG = sigl
               IF (e.GT.ECUt(Nnuc)) SIG = (sigh - sigl)*(e - ECUt(Nnuc))
     &             /(exl - ECUt(Nnuc)) + sigl
               DO j = 1, NLW
                  xj = j + HIS(Nnuc)
                  arg = (xj + 0.5)**2/(2.*SIG)
                  IF (arg.LE.EXPmax) THEN
                     ro_j = (2*xj + 1.)/(2.*SIG)*EXP( - arg)
                     RO(i,j,1,Nnuc) = ro_u*ro_j*ro_pi
                     RO(i,j,2,Nnuc) = ro_u*ro_j*ro_pi
                     IF (RO(i,j,1,Nnuc).LT.RORed) RO(i,j,1,Nnuc) = 0.d0
                     IF (RO(i,j,2,Nnuc).LT.RORed) RO(i,j,2,Nnuc) = 0.d0
                  ENDIF
               ENDDO
               efort = e
               UEXcit(i,Nnuc) = efort
               TNUc(i,Nnuc) = SQRT(efort/am)
            ENDIF
         ENDDO
      ENDIF
      ig = ig + 1
      IF (ig.LE.NEX(Nnuc)) THEN
C
C--------calculation of level densities for energies surpassing
C--------EXL /fermi gas formula/
C
         DO i = ig, NEX(Nnuc)
            u = EX(i,Nnuc) - DEL
            IF (igna.EQ.1) am = atil*FSHELL(u,SHC(Nnuc),-GAMma)
            UEXcit(i,Nnuc) = MAX(u,0.D0)
            TNUc(i,Nnuc) = SQRT(u/am)
C-----------Dilg's recommendations
            SIG = Scutf*0.6079*amas**0.6666667*SQRT(u*am)
            arg = 2.*SQRT(am*u) - ARGred
            IF (arg.LE.EXPmax) THEN
               ro_u = DEXP(arg)/(12.*SQRT(2*SIG))/am**0.25/u**1.25
               DO j = 1, NLW
                  xj = j + HIS(Nnuc)
                  arg = (xj + 0.5)**2/(2.*SIG)
                  IF (arg.LT.EXPmax) THEN
                     ro_j = (2*xj + 1.)/(2.*SIG)*EXP( - arg)
                     RO(i,j,1,Nnuc) = ro_u*ro_j*ro_pi  
                     RO(i,j,2,Nnuc) = ro_u*ro_j*ro_pi  
                     IF (RO(i,j,1,Nnuc).LT.RORed) RO(i,j,1,Nnuc) = 0.d0
                     IF (RO(i,j,2,Nnuc).LT.RORed) RO(i,j,2,Nnuc) = 0.d0         
                  ENDIF
               ENDDO
            ENDIF
         ENDDO
      ENDIF

      IF (IOUt.GE.6. .AND. FITlev.EQ.0.0D0 .AND. NEX(Nnuc).GT.1) THEN
         CALL PLOT_ZVV_GSLD(Nnuc)     
         call PLOT_ZVV_NumCumul(Nnuc, Defit,nplot,NLwst) 
      ENDIF
      
      ROPar(4,Nnuc) = eo
      ROPar(2,Nnuc) = ux
      RETURN
      END

C
C

      SUBROUTINE READ_SHELL_CORR
Ccc
Ccc   ********************************************************************
Ccc   *                                                          class:au*
Ccc   *             R E A D _ S H E L L _ C O R R                        *
Ccc   *                                                                  *
Ccc   * Reads MS Shell Corrections from RIPL-2                           *
Ccc   *                                                                  *
Ccc   * input: none (implicit - all considered nuclei)                   *
Ccc   *                                                                  *
Ccc   * output:none (implicit - shell corrections for considered nuclei) *
Ccc   *                                                                  *
Ccc   * calls:none                                                       *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C Local variables
C
      REAL*8 shelMSr, defcorr
      INTEGER  nz, na, nnuc, iloc
      CHARACTER*2 dum

C-----Reading MS shell corrections and deformation energies
      OPEN(11,FILE=trim(empiredir)//'/RIPL-2/densities/shellcor-ms.dat',
     &    STATUS='old')
C-----Skipping header lines
      READ(11,*)
      READ(11,*)
      READ(11,*)
      READ(11,*)
  40  READ(11,98,END=50,ERR=60)
     &    nz, na, dum, shelMSr, defcorr
  98  FORMAT(2(i4),1x,a2,2x,f7.3,1x,f8.3)
      CALL WHERE(nz*1000+na,nnuc,iloc)
      IF (iloc.EQ.0) THEN
c        SHC(Nnuc) = shelMSr - defcorr
         SHC(Nnuc) = shelMSr
      ENDIF
C-----projectile
      IF (nz.EQ.Z(0) .AND. na.EQ.A(0)) THEN
c        SHC(0) = shelMSr - defcorr
        SHC(0) = shelMSr
      ENDIF
      GO TO 40
  60  STOP 'Error reading shell correction file'
  50  CLOSE(11)
      RETURN
      END
C
      SUBROUTINE ROHFB(Nnuc)
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
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C PARAMETER definitions
C
C     FOLLOWS RIPL-3 DIMENSIONS
      INTEGER NLDGRID, JMAX
      PARAMETER (NLDGRID = 60,JMAX = 50)
      COMMON /UCGRID/ uugrid, cgrid,iugrid 
C
C Dummy arguments
C
      INTEGER Nnuc, nplot
C
C Local variables
C
      REAL*8 c1, c2, hhh, r1, r2, corr1,
     & rhogrid(0:NLDGRID,JMAX,2), rhoogrid(0:NLDGRID,2),
     & rhotgrid(0:NLDGRID,2), cgrid(0:NLDGRID,2),
     & uugrid(0:NLDGRID), tgrid(0:NLDGRID), u, rocumul, pcorr, acorr
      REAL*8 defit
      CHARACTER*2 car2
      REAL*8 DLOG10
      CHARACTER*56 filename
      LOGICAL fexist
      INTEGER i, ipp,ia, iar, iugrid, iz, izr, j, jmaxl, k, khi, kk, klo
      INTEGER*4 PIPE


      ia = A(Nnuc)
      iz = Z(Nnuc)
C-----next call prepares for lev. dens. calculations
      CALL PRERO(Nnuc)
C
C-----initialization
C
      jmaxl = MIN(NDLW,JMAX)
      DO i = 0, NLDGRID
         uugrid(i) = 0.
         tgrid(i) = 0.
         DO ipp = 1, 2
            cgrid(i,ipp) = 0.
            rhoogrid(i,ipp) = 1.d-20
            rhotgrid(i,ipp) = 1.d-20
            DO j = 1, jmaxl
               rhogrid(i,j,ipp) = 1.d-20
            ENDDO
         ENDDO
      ENDDO
      WRITE (filename,99005) iz

99005 FORMAT ('/RIPL-2/densities/total/level-densities-hfb/z',i3.3,
     &'.tab')
      INQUIRE(file = trim(empiredir)//filename, exist = fexist)
      IF(.not.fexist) THEN
       WRITE(8,*) trim(empiredir)//trim(filename), ' does not exist'
       WRITE(*,*) trim(empiredir)//trim(filename), ' does not exist'
       STOP 'ERROR: '
      ENDIF

      OPEN (UNIT = 34,FILE = trim(empiredir)//filename,ERR = 300)
  100 READ (34,99010,ERR=300,END = 300) car2
99010 FORMAT (23x,a2,i3,3x,i3)!,2x,a8)
      IF (car2.NE.'Z=') GOTO 100
      BACKSPACE (34)
      READ (34,99010,ERR=300,END = 300) car2, izr, iar!, paritate
      IF (iar.NE.ia .OR. izr.NE.iz) GOTO 100
C
C-----reading microscopic lev. dens. from the RIPL-3 file
C
      READ (34,*,END = 300)
      READ (34,*,END = 300)
      i = 1
  200 READ (34,99015,END = 300) uugrid(i), tgrid(i), cgrid(i,1),
     &                          rhoogrid(i,1), rhotgrid(i,1),
     &                         (rhogrid(i,j,1),j = 1,jmaxl)
99015 FORMAT (1x,f6.2,f7.3,1x,53E9.2)
      IF (uugrid(i).LE.0.001) GOTO 270
      IF (i.EQ.NLDGRID) GOTO 250
      i = i + 1
      GOTO 200
  250 i = 1
      READ (34,*,END = 300)
C     SKIPPING 4 TITLE LINES
  270 READ(34,*,END = 300)
      READ(34,*,END = 300)
      READ(34,*,END = 300)
      READ(34,*,END = 300)
  280 READ (34,99015,END = 300) uugrid(i), tgrid(i), cgrid(i,2),
     &                          rhoogrid(i,2), rhotgrid(i,2),
     &                         (rhogrid(i,j,2),j = 1,jmaxl)
      IF (uugrid(i).LE.0.001) GOTO 400
      IF (i.EQ.NLDGRID) GOTO 400
      i = i + 1
      GOTO 280
  300 WRITE (8,*) ' NO LEV. DENS. FOR Z=', iz, ' A=', ia, ' IN HFB'
      WRITE (8,*) ' USE OTHER LEVEL DENSITIES. EXECUTION TERMINATED '
      STOP 'RIPL HFB ground state lev dens. missing'
  400 CLOSE (34)
C
C     Using correction files given by A. Koning on March 2008.
C     Corrections are defined exactly as ROHfba() and ROHfbp() parameters
C     by fitting available discrete levels' and D0s' information
C
      IF(ROHfba(Nnuc).lt.-10.d0 .or. ROHfbp(Nnuc).lt.-10.d0) then
C       Corrections are read only if they are not given in the input,
C       otherwise input values are taken
C
        WRITE (filename,99007) iz
99007   FORMAT ('/RIPL-2/densities/total/level-densities-hfb/z',i3.3,
     &'.cor')
        INQUIRE(file = trim(empiredir)//filename, exist = fexist)
        IF(fexist) then
          OPEN (UNIT = 34,FILE = trim(empiredir)//filename,ERR = 440)
          pcorr = 0.d0
          acorr = 0.d0
  110     READ (34,99008,ERR=440,END = 440) izr, iar, acorr, pcorr
99008     FORMAT (1x,i3,1x,i3,10x,f11.5,1x,f11.5)
          IF (iar.NE.ia .OR. izr.NE.iz) GOTO 110

          IF(ROHfbp(Nnuc).lt.-10.d0) ROHfbp(Nnuc) = pcorr
          IF(ROHfba(Nnuc).lt.-10.d0) ROHfba(Nnuc) = acorr

c          ROHfbp(Nnuc) = 0.d0
c          ROHfba(Nnuc) = 0.d0
C---------printing microscopic lev. dens. corrections from the RIPL-3 file
C
          IF(ROHfba(Nnuc).ne.0.d0) then
            WRITE (8,
     &      '('' GS HFB L.D. norm  in '',I3,A2,'' set to '',F8.3)'
     &        ) ia, SYMb(nnuc), ROHfba(Nnuc)
            WRITE (12,
     &      '('' GS HFB L.D. norm  in '',I3,A2,'' set to '',F8.3)'
     &        ) ia, SYMb(nnuc), ROHfba(Nnuc)
          ENDIF
          IF(ROHfbp(Nnuc).ne.0.d0) then
            WRITE (8,
     &      '('' GS HFB L.D. shift in '',I3,A2,'' set to '',F8.3)'
     &        ) ia, SYMb(nnuc), ROHfbp(Nnuc)
            WRITE (12,
     &      '('' GS HFB L.D. shift in '',I3,A2,'' set to '',F8.3)'
     &        ) ia, SYMb(nnuc), ROHfbp(Nnuc)
          ENDIF
  440     CLOSE(34)
          IF(ROHfba(Nnuc).lt.-10.d0) ROHfba(Nnuc)=0.d0
          IF(ROHfbp(Nnuc).lt.-10.d0) ROHfbp(Nnuc)=0.d0
        ELSE ! no correction available'
          IF(ROHfba(Nnuc).lt.-10.d0) ROHfba(Nnuc)=0.d0
          IF(ROHfbp(Nnuc).lt.-10.d0) ROHfbp(Nnuc)=0.d0
        ENDIF
        goto 445
  310   WRITE (8,*) ' Error reading microsc. LD corrections FOR Z=', iz,
     &              ' A=', ia, ' IN HFB'
  445   CLOSE (34)
cxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
      ENDIF
      iugrid = i - 1
      defit=0.d0
      IF(IOUt.eq.6 .and.NLV(Nnuc).GT.3)
     &     call  PLOT_ZVV_NumCumul(Nnuc, defit,nplot,jmaxl)
      DO kk = 1, NEX(Nnuc)
         u = EX(kk,Nnuc) - ROHfbp(nnuc)
         UEXcit(kk,Nnuc) = EX(kk,Nnuc)
         IF (u.LT.0.) CYCLE
         IF (u.GT.200.0D0) THEN
            WRITE (8,*) ' '
            WRITE (8,*) ' HFB LEV. DENS. DEFINED UP TO 200 MeV ONLY'
            WRITE (8,*) ' REQUESTED ENERGY IS ', u, ' MeV'
            WRITE (8,*) ' YOU HAVE TO USE OTHER LEVEL DENSITIES'
            WRITE (8,*) ' EXECUTION STOPPED'
            STOP 'TOO HIGH ENERGY FOR HFB LEV. DENS.'
         ENDIF
         corr1 = 1.d0
         IF(ROHfba(Nnuc).NE.0.d0)
     &       corr1 = dexp(ROHfba(nnuc)*dsqrt(u) )
C
C--------interpolation in the level density tables
C
         klo = 1
         khi = iugrid
         IF (u.LE.uugrid(klo)) THEN
            klo = 0
            khi = 1
            GOTO 500
         ENDIF
         IF (u.GE.uugrid(khi)) THEN
            klo = iugrid - 1
            GOTO 500
         ENDIF
  450    IF (khi - klo.GT.1) THEN
            k = (khi + klo)/2.
            IF (uugrid(k).GT.u) THEN
               khi = k
            ELSE
               klo = k
            ENDIF
            GOTO 450
         ENDIF
  500    hhh = uugrid(khi) - uugrid(klo)
         c1 = (uugrid(khi) - u)/hhh
         c2 = (u - uugrid(klo))/hhh
         DO j = 1, jmaxl
            DO ipp = 1, 2
               r1 = rhogrid(klo,j,ipp)
               r2 = rhogrid(khi,j,ipp)
               IF (r1.GT.0 .AND. r2.GT.0) THEN
                  RO(kk,j,ipp,Nnuc) = 10.**(c1*DLOG10(r1) +
     &                                c2*DLOG10(r2))*corr1
               ELSE
                  RO(kk,j,ipp,Nnuc) = (c1*r1 + c2*r2)*corr1
               ENDIF
               IF (RO(kk,j,ipp,Nnuc).LT.0) RO(kk,j,ipp,Nnuc) = 0.d0
            ENDDO
         ENDDO
         TNUc(kk,Nnuc) = c1*tgrid(klo) + c2*tgrid(khi)
      ENDDO

C-----plot of the l.d. formula
      IF(IOUt.eq.6 .and.NLV(Nnuc).GT.3) CALL PLOT_ZVV_GSLD(Nnuc)

C--------cumulative plot of levels along with the l.d. formula
      IF (FITlev.GT.0.0D0 .AND. NLV(Nnuc).GT.3) THEN
         WRITE (8,99009) INT(Z(Nnuc)), SYMb(Nnuc), INT(A(Nnuc)),
     &                   NLV(Nnuc)
99009    FORMAT ('Cumulative plot for ',I3,'-',A2,'-',I3,
     &           '   Microscopic LD,  Ncut=',I3)
         OPEN (35,FILE = 'fort.35')
         WRITE (35,*) 'set terminal postscript enhanced color lw 2
     & solid "Helvetica" 20'
         WRITE (35,*) 'set output "|cat >>CUMULPLOT.PS"'
         WRITE (35,9909) INT(Z(Nnuc)), SYMb(Nnuc), INT(A(Nnuc)),
     &                    NLV(Nnuc)
 9909    FORMAT ('set title "',I3,'-',A2,'-',I3,
     &           '   Microscopic LD,  Ncut=',I3,'"')
         WRITE (35,*) 'set logscale y'
         WRITE (35,*) 'set xlabel "Excitation energy (MeV)" 0,0'
         WRITE (35,*) 'set ylabel "Cumulative number of levels" 0,0'
         WRITE (35,*) 'set style line 1 lt 1 lw 2'
         WRITE (35,*) 'set style line 2 lt 5 lw 2'
         WRITE (35,'(''plot "fort.36" w filledcu y2 ls 2 t "Discrete lev
     &els", "fort.34" w l ls 1 t "Level density" '')')
         CLOSE (35)
         OPEN (34,FILE = 'fort.34')
         OPEN (36,FILE = 'fort.36')
         WRITE (36,*) '0.0 1.0'
         DO il = 2, NLV(Nnuc)
            WRITE (36,*) ELV(il,Nnuc)*1d6, FLOAT(il - 1)
            WRITE (36,*) ELV(il,Nnuc)*1d6, FLOAT(il)
         ENDDO
         rocumul = 1.0
         WRITE (34,*) '0.0  ', rocumul

         DO kk = 2, iugrid!NFIsen1
            if(uugrid(kk).gt. ELV(NLV(Nnuc),Nnuc)+2.d0)exit
C-----------integration over energy. Parity dependence explicitly considered.
C-----------There is a factor 1/2 steming from the trapezoid integration
C              rocumul = rocumul + 0.5d0*defit/RORed*
C           DO ij = 1, NFISJ1
            DO ij = 1, NLW
c                rocumul = rocumul + 2.d0*RO(i,ij,1,Nnuc)
               rocumul = rocumul +
     &         (RO(kk - 1,ij,1,Nnuc) + RO(kk,ij,1,Nnuc) +
     &          RO(kk - 1,ij,2,Nnuc) + RO(kk,ij,2,Nnuc))      
            ENDDO
         WRITE (34,*)  uugrid(kk)*1d6,cgrid(kk,1)+cgrid(kk,2)
         ENDDO
         CLOSE (36)
         CLOSE (34)
         IF (IOPsys.EQ.0) THEN
            iwin = PIPE('gnuplot fort.35')
            CLOSE (35)
         ENDIF
      ENDIF

      RETURN
      END


      SUBROUTINE LEVFIT(Nnuc,Nplot,Dshif,Dshift,Defit)


      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C COMMON variables
C
      REAL*8 TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl    ! CRIT
      REAL*8 AP1, AP2, GAMma, DEL, DELp, BF, A23, A2            ! PARAM
      INTEGER NLWst                                             ! PARAM

      COMMON /CRIT  / TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl
      COMMON /PARAM / AP1, AP2, GAMma, DEL, DELp, BF, A23, A2, NLWst
C
C Dummy arguments
C
      INTEGER Nnuc
C
C Local variables
C
      REAL*8 aj, defit, dshi, dshif, dshift, ellq, exkk,
     &                 rocumd, rocumu, rocumul, rolev,
     &                 rotemp, ecrt !,ldshif(40)

      INTEGER i, ij, iter, kk, kkl, kku, nplot

C-----fit level densities to discrete levels applying energy shift
C-----which will linearly go to 0 at neutron binding energy
C-----
      IF (FITlev.GT.0 .AND. RORed.EQ.0) THEN
         WRITE (8,*) ' '
         WRITE (8,*) ' CAN NOT FIT DISCRETE LEVELS SINCE RORed IS 0'
         WRITE (8,*) ' CHECK WHETHER YOU CAN INCREASE EXPmax in input.f'
         WRITE (8,*) ' (MAXIMUM ARGUMENT OF THE EXPONENT ALLOWED)'
         WRITE (8,*)
     &             ' IF YOUR SYSTEM ALLOWS FOR THIS DO IT AND RECOMPILE'
         WRITE (8,*) ' OTHERWISE YOU CAN NOT ASK FOR SUCH A HIGH ENERGY'
         WRITE (8,*) ' HAVE NO CLUE WHAT TO DO IN SUCH A CASE'
         WRITE (8,*) ' FOR THE TIME BEING EXECUTION TERMINATED'
         STOP
      ENDIF

C-----get neutron binding energy  if not yet defined
      IF (Q(1,Nnuc).EQ.0.0D0) THEN
         REWIND (25)
         CALL BNDG(1,Nnuc,Q(1,Nnuc))
      ENDIF
C-----get distance between Qn and the last level
      ellq = Q(1,Nnuc) -( ELV(NLV(Nnuc),Nnuc)+ LDShif(Nnuc))
      dshift = 0.0
      iter = 0
C-----we are not going to fit discrete levels if there are not more
C-----than three or if max excitation energy is so high that levels
C-----can not be taken into account (RORed=0)
      IF (NLV(Nnuc).GT.3 .AND. RORed.GT.0) THEN
         IF (FITlev.GT.0.0D0) THEN
            WRITE (8,*) ' '
            WRITE (8,*) ' Fitting l.d. to discrete levels'
            WRITE (8,*) NLV(Nnuc), ' levels at ', ELV(NLV(Nnuc),Nnuc),
     &                  ' MeV'
         ENDIF
         defit = (ELV(NLV(Nnuc),Nnuc) + LDShif(Nnuc)+2.d0)
     &           /(NEXreq-1) 
         nplot = (ELV(NLV(Nnuc),Nnuc)+ LDShif(Nnuc)+2.d0)/defit

  150    rocumul = 1.0
         iter = iter + 1
         kkl = 0
         kku = 0
         DO kk = 1, NDEX
C-----------clean RO matrix
            DO i = 1, NDLW
               RO(kk,i,1,Nnuc) = 0.d0
               RO(kk,i,2,Nnuc) = 0.d0
            ENDDO
         ENDDO
         DO kk = 1, NEXreq
C-----------decrease energy shift above the last level to become 0 at Qn
            exkk = (kk - 1)*defit
            IF (exkk.LE.ELV(NLV(Nnuc),Nnuc)+ LDShif(Nnuc)) THEN
               dshif = dshift
            ELSEIF (exkk.LT.Q(1,Nnuc) .AND. ellq.NE.0.0D0) THEN
               dshif = dshift*(Q(1,Nnuc) - exkk)/ellq
            ELSE
               dshif = 0.0
            ENDIF

            CALL DAMIRO(kk,Nnuc,dshif,defit,rotemp,aj)


            DO ij = 1, NLWst
C-----------Integration over energy. There should be factor 2 because of the
C-----------parity but it cancels with the 1/2 steming from the trapezoid
C-----------integration
C              IF (kk.GT.1) rocumul = rocumul +
C    &                                (RO(kk - 1,ij,Nnuc) + RO(kk,ij,
C    &                                Nnuc))*defit/RORed
C-----------Integration over energy. Parity dependence explicitly considered.
C-----------There is a factor 1/2 steming from the trapezoid integration
               IF (kk.GT.1) rocumul = rocumul + 0.5d0*defit/RORed*
     &         (RO(kk - 1,ij,1,Nnuc) + RO(kk,ij,1,Nnuc) +
     &          RO(kk - 1,ij,2,Nnuc) + RO(kk,ij,2,Nnuc))
            ENDDO
            IF (rocumul.LE.NLV(Nnuc)) THEN
               kkl = kk
               rocumd = rocumul
            ELSEIF (kku.EQ.0) THEN
               kku = kk
               rocumu = rocumul
            ENDIF
         ENDDO

         rocumd = LOG(rocumd)
         rocumu = LOG(rocumu)
         rolev = LOG(REAL(NLV(Nnuc)))
         dshi = (rolev - rocumd)/(rocumu - rocumd)
         dshi = (kkl - 1 + dshi)*defit
         dshi = dshi - (ELV(NLV(Nnuc),Nnuc)+ LDShif(Nnuc))
         dshift = dshift + dshi

         IF (FITlev.GT.0.0D0) then 
          Ecrt = UCRt - DEL - dshift
        write(8,*)
        WRITE (8,*) '*****   A=',nint(A(nnuc)),
     &   ' Z=',nint(Z(nnuc)),' Bn=',sngl(Q(1,nnuc)),
     &     ' LDshif=',LDShif(nnuc)
          WRITE (8,'(A7,G12.5,A6,G12.5,A9,G12.5,A7,G12.5)')
     &    'Ucrt = ',UCRt,' Ecrt=',Ecrt,' Econd = ',Econd,
     &    ' DEL = ',DEL          
          WRITE (8,'(A5,I3,4X,G12.5,A15,2(G12.5,1x))') 
     &    'It # ', iter, dshi, ' Final shift = ',dshift
        write(8,*)
         ENDIF
         IF (ABS(dshi).GT.0.01D0 .and. iter.LE.20) GOTO 150
      ENDIF

      RETURN
      END


CCC
CCC
CCC   *************FISSION**********************************
CCC
CCC

CCC   *****************************************************************
      SUBROUTINE DAMIRO_FISHI(Kk,Nnuc,Asaf,Rotemp,Aj)
CCC   *****************************************************************

      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C COMMON variables
C
      REAL*8 TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl    ! CRIT
      REAL*8 AP1, AP2, GAMma, DEL, DELp, BF, A23, A2            ! PARAM
      INTEGER NLWst                                             ! PARAM
      COMMON /CRIT  / TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl
      COMMON /PARAM / AP1, AP2, GAMma, DEL, DELp, BF, A23, A2, NLWst
C
C Dummy arguments
C
      REAL*8 Aj, Asaf, Rotemp
      INTEGER Kk, Nnuc
C
C Local variables
C
      REAL*8 ac, accn, ampl, bsq, cigor, momort, mompar, phi,
     &       rbmsph, saimid, saimin, saimx, selmax,
     &       shredt, stab, t, temp, u
      LOGICAL bcs
      REAL*8 FSHELL, ROBCS, RODEF
      INTEGER i, ia, iz
      INTEGER INT

      bcs = .TRUE.
      rbmsph = 0.01448*A(Nnuc)**1.6667
      ia = INT(A(Nnuc))
      iz = INT(Z(Nnuc))
    
C-----do loop over angular momentum
      DO i = 1, NLWst
         Aj = REAL(i) + HIS(Nnuc)

C-----a-parameter and U determination for fission channel
C-----------temperature fade-out of the shell correction
C-----------ACCN  serves only to calculate temperature fade-out
         IF (EX(Kk,Nnuc).GT.UCRt) THEN
            accn = ATIl*(1 + SHC(Nnuc)
     &             *(1 - EXP((-GAMma*EX(Kk,Nnuc))))/EX(Kk,Nnuc))
         ELSE
            accn = ACRt
         ENDIF
         temp = 0.
         IF (EX(Kk,Nnuc).GE.YRAst(i,Nnuc))
     &        temp = SQRT((EX(Kk,Nnuc) - YRAst(i,Nnuc))/accn)
         ampl = EXP(TEMp0*SHRt)
         shredt = 1.
         IF (temp.GE.TEMp0) shredt = ampl*EXP(( - SHRt*temp))
C--------temperature fade-out of the shell correction  --- done ----
         u = EX(Kk,Nnuc) + DEL - FISb(i,Nnuc) + SHC(Nnuc)
     &                * shredt*SHCjf(i,Nnuc)
         IF(u.GT.UCRt) THEN
            u = u - ECOnd
            bcs = .FALSE.
         ELSE
            bcs = .TRUE.
         ENDIF
         UEXcit(Kk,Nnuc) = MAX(u,0.D0)
         IF (u.LE.0.0D0) RETURN
         IF (Z(Nnuc).LT.102.0D0 .AND. Z(Nnuc).GE.19.0D0) THEN
C-----------next line is to calculate deformation parameter A2 only
            CALL SIGMAK(A(Nnuc),Z(Nnuc),DEF(1,Nnuc),0.0D0,u,accn,Aj,
     &                     mompar,momort,A2,stab,cigor)
            CALL MOMFIT(iz,ia,i - 1,saimin,saimid,saimx,selmax)
            mompar = saimin*rbmsph
            momort = saimx*rbmsph
         ELSE
            CALL SIGMAK(A(Nnuc),Z(Nnuc),DEF(1,Nnuc),0.0D0,u,accn,Aj,
     &                     mompar,momort,A2,stab,cigor)
         ENDIF
C-----------calculation of level density parameter 'a' including surface
C-----------dependent factor
         ATIl = AP1*A(Nnuc) + AP2*A23*BSQ(cigor)
         ATIl = ATIl*ATIlnor(Nnuc)
         IF (Asaf.GE.0.D0) ac = ATIl*FSHELL(u,SHC(Nnuc),Asaf)
         IF (Asaf.LT.0.D0) ac = -ATIl*Asaf
         IF (ac.LE.0.D0) RETURN

         IF (bcs) THEN
            Rotemp = ROBCS(A(Nnuc),u,Aj,mompar,momort,A2,T,BF)* RORed
! --------The next two lines look like a mistake or some  left over trial
! --------Mihaela please check.     
!            Rotemp = RODEF(A(Nnuc),u,ac,Aj,mompar,momort,T,
!     &               YRAst(i,Nnuc),HIS(Nnuc),BF,ARGred,EXPmax,a2,1)
            IF (i.EQ.1) THEN
               phi = SQRT(1.D0 - u/UCRt)
               t = 2.0*TCRt*phi/LOG((phi + 1.D0)/(1.D0 - phi))
            ENDIF
         ELSE
            Rotemp = RODEF(A(Nnuc),u,ac,Aj,mompar,momort,T,
     &               YRAst(i,Nnuc),HIS(Nnuc),BF,ARGred,EXPmax,a2,1)
            IF (i.EQ.1) t = SQRT(u/ac)
         ENDIF

         ROF(Kk,i,Nnuc) = Rotemp   
         IF (i.EQ.1) TNUc(Kk,Nnuc) = t
      ENDDO
      RETURN
      END


c=======================================================================
      SUBROUTINE DAMI_RO_HFB_FIS(Nnuc,Ib,Rafis)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C
C COMMON variables
C
      REAL*8 TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl    ! CRIT

      REAL*8 ACRtf(NFHUMP), UCRtf(NFHUMP), TCRtf(NFHUMP),       ! CRITFIS
     & DETcrtf(NFHUMP),SCRtf(NFHUMP),MORtcrt(NFPARAB),
     & MPArcrt(NFPARAB), ECOndf(NFHUMP)

      REAL*8 ROFism(0:NFISENMAX,NDLW,NFMOD),HM(NFTRANS,NFMOD),  ! FISSMOD real
     & EFDism(NFTRANS,NFMOD), UGRidf(0:NFISENMAX,NFMOD), EFBm(NFMOD),
     & XMInnm(NFMOD), AFIsm(NFMOD), DEFbm(NFMOD), SHCfism(NFMOD),
     & DELtafism(NFMOD), GAMmafism(NFMOD), WFIsm(NFMOD),
     & DEStepm(NFMOD), TFBm(NFMOD), TDIrm(NFMOD), CSFism(NFMOD),
     & TFB, TDIrect,  ECFism(NFMOD)

      INTEGER BFFm(NFMOD), NRBinfism(NFMOD)                     ! FISSMOD int

      REAL*8 AP1, AP2, GAMma, DEL, DELp, BF, A23, A2            ! PARAM

      INTEGER NLWst                                             ! PARAM

      REAL*8 barnorm(NFHump),hnorm                              ! ROHFBSADD
      REAL*8 rohfbp_sd(NFHump), rohfba_sd(NFHump),              ! ROHFBSADD
     &       rohfb_norm(NFHump)

      COMMON /CRIT  / TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl

      COMMON /CRITFIS/ ACRtf, UCRtf, TCRtf, DETcrtf, SCRtf, MORtcrt,
     &                 MPArcrt, ECOndf

      COMMON /FISSMOD/ ROFism, HM, EFDism, UGRidf, EFBm, XMInnm, AFIsm,
     &                 DEFbm, SHCfism, DELtafism, GAMmafism, WFIsm,
     &                 BFFm, NRBinfism, DEStepm, TFBm, TDIrm, CSFism,
     &                 TFB, TDIrect,ECFism

      COMMON /PARAM / AP1, AP2, GAMma, DEL, DELp, BF, A23, A2, NLWst

      COMMON /ROHFBSADD/rohfbp_sd, rohfba_sd,rohfb_norm,barnorm,hnorm
C
C Dummy arguments
C
      INTEGER Ib, Nnuc
      REAL*8 Rafis
C
C Local variables
C
      REAL*8 aaj, excn1,   rotemp, xmax, mompar, temp
      REAL*8 bbb, ggg, rrry, rrr1, rrr2, def2
      REAL FLOAT
      INTEGER ia, iff, in, iz, jj, kk, nr
      INTEGER INT

     
      iz = INT(Z(Nnuc))
      ia = INT(A(Nnuc))
      in = ia - iz
C-----continuum, level densities at saddle points
      excn1 = EMAx(Nnuc)
C-----where continuum starts,ends,steps in between
      XMInn(Ib) = 0.0001
      DO nr = 1, NRFdis(Ib)
        IF (EFDis(nr,Ib).GT.XMInn(Ib)) XMInn(Ib) = EFDis(nr,Ib)
      ENDDO
C     IF(ECFis(ib).gt.0.) XMInn(Ib) = ECFis(ib)

      IF (excn1.LE.(EFB(Ib) + XMInn(Ib))) THEN
        xmax = XMInn(Ib) + 3.5D0
      ELSE
        xmax = excn1 - (EFB(Ib) + XMInn(Ib)) + 3.5D0
      ENDIF
      DEStepp(Ib) = (xmax - XMInn(Ib))/100.
      NRBinfis(Ib) =INT((xmax - XMInn(Ib))/DEStepp(Ib))

      IF (NRBinfis(Ib).GT.NFISENMAX) THEN
        WRITE (8,*)
     &              ' ERROR: Level density at saddle exceeds dimensions'
     &            , ' Increase NFISENMAX in dimension.h'
        STOP 'ERROR: Level density at saddle exceeds NFISENMAX'
      ENDIF

      DO kk = 1, NRBinfis(Ib)
        UGRid(kk,Ib) = XMInn(Ib) + (kk - 1)*DEStepp(Ib)
      ENDDO     
      CALL HFB_FIS(ib,Nnuc)
      RAFis = 1.d0

      iff = BFF(Ib)
      rbmsph = 0.01448*A(Nnuc)**1.66667
C     See eq.(1.38) of the Ignatyuk Book (Stat.prop....)

      bbb = min(DEFfis(Ib),1.5d0) ! the actual static saddle deformation is used
      ggg = pi/3.                 ! axial symmetry
      IF (iff.eq.2) ggg = pi/18.  ! arbitrarily fixed asymmetry to 10 degrees

      rrr2 = 1. + SQRT(5./4./pi)*bbb*COS(ggg - 4.*pi/3.)
      rrr1 = 1. + SQRT(5./4./pi)*bbb*COS(ggg - 2.*pi/3.)

      rrry=rrr2/rrr1
      def2=(rrry-1.0)/(1.0+0.5*rrry)
      IF (def2.GT.0.9D0) def2 = 0.9
      IF (def2.LT.( - 0.9D0)) def2 = -0.9

      mompar = rbmsph*(1. - (2./3.)*def2)
     
      DO jj = 1,NLW
         aaj = FLOAT(jj) + HIS(Nnuc)
         DO kk = 1,NRBinfis(Ib)
            temp = TNUcf(kk,Nnuc)
c-----------SYMMETRY ENHANCEMENT
            DO ipp = 1, 2
               rotemp = ROFisp(kk,jj,ipp,Ib)
C              Triaxiality with mass symmetry
               IF (Iff.EQ.2) rotemp =
     &                       rotemp*SQRT(pi/2.d0)*SQRT(mompar*temp)
c    &                       rotemp*SQRT(2.d0*pi)*SQRT(mompar*temp)
C              Mass asymmetry is already considered in HFB calculations
C              IF (Iff.EQ.3) rotemp = rotemp*2.
C              No symmetry
               IF (Iff.EQ.4) rotemp =
     &                       rotemp*2.*SQRT(2.*pi)*SQRT(mompar*temp)
               ROFisp(kk,jj,ipp,Ib) = rotemp * rohfb_norm(Ib)
            ENDDO
         ENDDO
      ENDDO
      IF(IOUT.EQ.6)CALL PLOT_ZVV_SadLD(Nnuc,Ib)
      
      END
C**************************************************************************
      SUBROUTINE HFB_FIS(ib,Nnuc)
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

      INCLUDE 'dimension.h'
      INCLUDE 'global.h'

      REAL*8 barnorm(NFHump),hnorm                              ! ROHFBSADD
      REAL*8 rohfbp_sd(NFHump), rohfba_sd(NFHump),              ! ROHFBSADD
     &       rohfb_norm(NFHump)
C
      COMMON /ROHFBSADD/rohfbp_sd, rohfba_sd,rohfb_norm,barnorm,hnorm
C
C PARAMETER definitions
C
      INTEGER NLDGRID, JMAX
C     FOLLOWS RIPL-3 DIMENSIONS
      PARAMETER (NLDGRID = 60,JMAX = 50)
C
C Dummy arguments
C
      INTEGER Nnuc
C
C Local variables
C
      REAL*8 c1, c2, hhh, r1, r2,
     &                 rhogrid(0:NLDGRID,JMAX,2), rhoogrid(0:NLDGRID,2),
     &                 rhotgrid(0:NLDGRID,2), cgrid(0:NLDGRID,2),
     &                 uugrid(0:NLDGRID), tgrid(0:NLDGRID), u


      CHARACTER*2 car2
      REAL*8 DLOG10
      CHARACTER*56 filename

      LOGICAL fexist
      INTEGER i, ia, iar, ipp, iugrid, iz, izr, j, jmaxl, k, khi, kk,
     &        klo

      ia = A(Nnuc)
      iz = Z(Nnuc)

      WRITE (filename,99006) ib, iz
99006 FORMAT
     & ('/RIPL-2/fission/leveldensities/Max',i1,'/z',i3.3)
      INQUIRE(file = trim(empiredir)//filename, exist = fexist)
      IF(.NOT.fexist) THEN
        WRITE (8,*) ' NO LEV. DENS. FOR Z=', iz, ' A=', ia,
     &                  ' IN HFB at saddle ',ib
        WRITE (8,*) ' USE OTHER LEVEL DENSITIES. EXECUTION
     &                    TERMINATED '
        WRITE (8,*)
     &           ' ERROR: HFB lev dens. at saddle ',ib,'  missing'
        STOP ' ERROR: HFB lev dens. at saddle missing'
      ENDIF
C
C-----initialization
C
      jmaxl = MIN(NDLW,JMAX)
      DO i = 0, NLDGRID
         uugrid(i) = 0.
         tgrid(i) = 0.
         DO ipp = 1, 2
            cgrid(i,ipp) = 0.
            rhoogrid(i,ipp) = 1.d-20
            rhotgrid(i,ipp) = 1.d-20
            DO j = 1, jmaxl
               rhogrid(i,j,ipp) = 1.d-20
            ENDDO
         ENDDO
      ENDDO       
C-----reading microscopic lev. dens. from the RIPL-3 file
      OPEN (UNIT = 34,FILE = trim(empiredir)//filename,ERR = 300)
  100 READ (34,99010,ERR=300,END = 300) car2
99010 FORMAT (23x,a2,i3,3x,i3)
      IF (car2.NE.'Z=') GOTO 100
      BACKSPACE (34)
      READ (34,99010,ERR=300,END = 300) car2, izr, iar
      IF (iar.NE.ia .OR. izr.NE.iz) GOTO 100    
      READ (34,*,END = 300)
      READ (34,*,END = 300)
      i = 1
  200 READ (34,99015,END = 300) uugrid(i), tgrid(i), cgrid(i,1),
     &                          rhoogrid(i,1), rhotgrid(i,1),
     &                         (rhogrid(i,j,1),j = 1,jmaxl)
99015 FORMAT (1x,f6.2,f7.3,1x,53E9.2)
      IF (uugrid(i).LE.0.001) GOTO 270
      IF (i.EQ.NLDGRID) GOTO 250
      i = i + 1
      GOTO 200
  250 i = 1
      READ (34,*,END = 300)
C     SKIPPING 4 TITLE LINES
  270 READ(34,*,END = 300)
      READ(34,*,END = 300)
      READ(34,*,END = 300)
      READ(34,*,END = 300)
  280 READ (34,99015,END = 300) uugrid(i), tgrid(i), cgrid(i,2),
     &                          rhoogrid(i,2), rhotgrid(i,2),
     &                         (rhogrid(i,j,2),j = 1,jmaxl)
      IF (uugrid(i).LE.0.001) GOTO 400
      IF (i.EQ.NLDGRID) GOTO 400
      i = i + 1
      GOTO 280
 300  WRITE (8,*) ' NO LEV. DENS. FOR Z=', iz, ' A=', ia,
     &                  ' IN HFB at saddle ',ib
      WRITE (8,*) ' USE OTHER LEVEL DENSITIES. EXECUTION
     &                    TERMINATED '
      WRITE (8,*)
     &           ' ERROR: HFB lev dens. at saddle',ib,'  missing'
            STOP ' ERROR: HFB lev dens. at saddle missing'
  400 CLOSE (34)
      iugrid = i - 1      
      DO kk = 1,NRBinfis(Ib)
         u = XMInn(Ib) + (kk - 1)*DEStepp(Ib) - rohfbp_sd(ib)
         IF (u.LT.0.) cycle
         IF (u.GT.200.0D0) THEN
            WRITE (8,*) ' '
            WRITE (8,*) ' HFB LEV. DENS. DEFINED UP TO 200 MeV ONLY'
            WRITE (8,*) ' REQUESTED ENERY IS ', u, ' MeV'
            WRITE (8,*) ' YOU HAVE TO USE ANOTHER LEVEL DENSITIES'
            WRITE (8,*) ' EXECUTION STOPPED'
            STOP 'TOO HIGH ENERGY FOR HFB LEV. DENS.'
         ENDIF

C--------interpolation in the level density tables
C
         klo = 1
         khi = iugrid
         IF (u.LE.uugrid(klo)) THEN
            klo = 0
            khi = 1
            GOTO 500
         ENDIF
         IF (u.GE.uugrid(khi)) THEN
            klo = iugrid - 1
            GOTO 500
         ENDIF
  450    IF (khi - klo.GT.1) THEN
            k = (khi + klo)/2.
            IF (uugrid(k).GT.u) THEN
               khi = k
            ELSE
               klo = k
            ENDIF
            GOTO 450
         ENDIF
  500    hhh = uugrid(khi) - uugrid(klo)
         c1 = (uugrid(khi) - u)/hhh
         c2 = (u - uugrid(klo))/hhh
         DO j = 1, jmaxl
            DO ipp = 1, 2
               r1 = rhogrid(klo,j,ipp)
               r2 = rhogrid(khi,j,ipp)
               IF (r1.GT.1.d-12 .AND. r2.GT.1.d-12) THEN
                  ROFisp(kk,j,ipp,ib) = 10.**(c1*DLOG10(r1) +
     &                                  c2*DLOG10(r2))
                  IF(rohfba_sd(ib).NE.0.d0) ROFisp(kk,j,ipp,ib) =
     &               ROFisp(kk,j,ipp,ib)*dexp(rohfba_sd(ib)*dsqrt(u))
               ELSE
                  ROFisp(kk,j,ipp,ib) = (c1*r1 + c2*r2)
               ENDIF
               IF (ROFisp(kk,j,ipp,ib).LT.0.d0)
     &             ROFisp(kk,j,ipp,ib) = 0.d0
            ENDDO
         ENDDO
         TNUcf(kk,Nnuc) = c1*tgrid(klo) + c2*tgrid(khi)
      ENDDO
      RETURN
      END

      SUBROUTINE DAMI_ROFIS(Nnuc,Ib,Mmod,Rafis)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C
C COMMON variables
C
      REAL*8 TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl    ! CRIT

      REAL*8 ACRtf(NFHUMP), UCRtf(NFHUMP), TCRtf(NFHUMP),       ! CRITFIS
     & DETcrtf(NFHUMP),SCRtf(NFHUMP),MORtcrt(NFPARAB),
     & MPArcrt(NFPARAB), ECOndf(NFHUMP)

      REAL*8 ROFism(0:NFISENMAX,NDLW,NFMOD),HM(NFTRANS,NFMOD),  ! FISSMOD real
     & EFDism(NFTRANS,NFMOD), UGRidf(0:NFISENMAX,NFMOD), EFBm(NFMOD),
     & XMInnm(NFMOD), AFIsm(NFMOD), DEFbm(NFMOD), SHCfism(NFMOD),
     & DELtafism(NFMOD), GAMmafism(NFMOD), WFIsm(NFMOD),
     & DEStepm(NFMOD), TFBm(NFMOD), TDIrm(NFMOD), CSFism(NFMOD),
     & TFB, TDIrect, ECFism(NFMOD)

      INTEGER BFFm(NFMOD), NRBinfism(NFMOD)                     ! FISSMOD int
      REAL*8 AP1, AP2, GAMma, DEL, DELp, BF, A23, A2            ! PARAM
      INTEGER NLWst                                             ! PARAM

      REAL*8 delf,delfis
      COMMON /CRIT  / TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl
      COMMON /CRITFIS/ ACRtf, UCRtf, TCRtf, DETcrtf, SCRtf, MORtcrt,
     &                 MPArcrt, ECOndf
      COMMON /FISSMOD/ ROFism, HM, EFDism, UGRidf, EFBm, XMInnm, AFIsm,
     &                 DEFbm, SHCfism, DELtafism, GAMmafism, WFIsm,
     &                 BFFm, NRBinfism, DEStepm, TFBm, TDIrm, CSFism,
     &                 TFB, TDIrect,ECFism
      COMMON /PARAM / AP1, AP2, GAMma, DEL, DELp, BF, A23, A2, NLWst
C
C Dummy arguments
C
      INTEGER Ib, Mmod, Nnuc
      REAL*8 Rafis
C
C Local variables
C
      REAL*8 aaj, accn, ar, desteppp, excn1, mm2, r0, cigor,
     &                 rotemp, shcf, u, xmax, mompar, momort, temp,
     &                 vibbf12, vibbfdt, def2, stab, aj, vn
      REAL FLOAT
      INTEGER ia, iff, in, iz, jj, kk, nr
      INTEGER INT
      REAL*8 ROBCSF, RODEFF, FSHELL


       ARGred = 0.
       RORed = 1.
C-----continuum, level densities at saddle points
      excn1 = EMAx(Nnuc)
C-----where continuum starts,ends,steps in between
      IF (Mmod.EQ.0) THEN
         XMInn(Ib) = 0.01
         DO nr = 1, NRFdis(Ib)
            IF (EFDis(nr,Ib).GT.XMInn(Ib)) XMInn(Ib) = EFDis(nr,Ib)
         ENDDO
         IF(ECFis(ib).gt.0.) XMInn(Ib) = ECFis(ib)

         IF (excn1.LE.(EFB(Ib) + XMInn(Ib))) THEN
            xmax = XMInn(Ib) + 3.5D0
         ELSE
            xmax = excn1 - (EFB(Ib) + XMInn(Ib)) + 3.5D0
         ENDIF
         DEStepp(Ib) = (xmax - XMInn(Ib))/100.
         NRBinfis(Ib) =INT((xmax - XMInn(Ib))/DEStepp(Ib))

         IF (NRBinfis(Ib).GT.NFISENMAX) THEN
            WRITE (8,*)
     &              ' ERROR: Level density at saddle exceeds dimensions'
     &              , ' Increase NFISENMAX in dimension.h'
            STOP 'ERROR: Level density at saddle exceeds NFISENMAX'
         ENDIF
         DO kk = 1, NRBinfis(Ib)
            UGRid(kk,Ib) = XMInn(Ib) + (kk - 1)*DEStepp(Ib)
         ENDDO
      ELSE ! Mmod.GT.0
         XMInnm(Mmod) = 0.0001
         DO nr = 1, NRFdis(Ib)
            IF (EFDism(nr,Mmod).GT.XMInnm(Mmod)) XMInnm(Mmod)
     &          = EFDism(nr,Mmod)
         ENDDO
         IF(ECFism(Mmod).gt.0.) XMInnm(Mmod) = ECFism(Mmod)

         IF (excn1.LE.(EFBm(Mmod) + XMInnm(Mmod))) THEN
            xmax = XMInn(Mmod) + 3.d0
         ELSE
            xmax = excn1 - (EFBm(Mmod) + XMInnm(Mmod)) + 3.d0
         ENDIF
         DEStepm(Mmod) = (xmax - XMInnm(Mmod))/100.d0
         NRBinfism(Mmod) = INT((xmax - XMInnm(Mmod))/DEStepm(Mmod))
         IF (NRBinfism(Mmod).GT.NFISENMAX) THEN
            WRITE (8,*)
     &              ' ERROR: Level density at saddle exceeds dimensions'
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

      mm2 = 0.24 * A(Nnuc)**0.666667
      r0=1.24
      iff = 1

      temp=0.d0
C-----EMPIRE-3.0-dependence
      CALL EGSMsys(ap1,ap2,gamma,del,delp,nnuc)

      IF (Mmod.EQ.0) THEN
         GAMma = GAMmafis(Ib)
c         DELf = DELtafis(Ib)
         delp=deltafis(ib)
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
         DELf = DELtafism(Mmod)
                  DELp = DELtafism(Mmod)
         shcf = SHCfism(Mmod)
         iff = BFFm(Mmod)
         desteppp = DEStepm(Mmod)
      ENDIF
      gamma = gamma/A(Nnuc)**0.333333
      ATIl = AP1*A(Nnuc) + AP2*A(Nnuc)**0.666667
      ATIl = ATIl*Rafis
      ar = ATIl*(1.0 + shcf*GAMma)

      delfis = 0.
      IF (FISden(Nnuc).LE.1.) THEN
         IF (MOD(in,2).NE.0) DELfis = DELp
         IF (MOD(iz,2).NE.0) DELfis = DELfis + DELp
      ENDIF
C
     
      aj=0.
      u=0.
      DO kk = 1, NDEX
         DO i = 1, NDLW
c            DO Ib = 1, NRHUMP 
               ROFis(kk,jj,Ib) = 0.d0
               ROFisp(kk,jj,1,Ib) = 0.d0
               ROFisp(kk,jj,2,Ib) = 0.d0
c            ENDDO
         ENDDO
      ENDDO
      if (iff.eq.2) then
C       Axial SYMMETRY
        CALL SIGMAK(A(Nnuc),Z(Nnuc),DEFfis(Ib),-1.0D0,u,ar,
     &                           aj,mompar,momort,A2,stab,cigor)
      else
C       Non-axial SYMMETRY, gamma assumed 10 degrees inside SIGMAK
        CALL SIGMAK(A(Nnuc),Z(Nnuc),DEFfis(Ib),-2.0D0,u,ar,
     &                           aj,mompar,momort,A2,stab,cigor)
      endif
C
C-----calculation of level density parameter 'a' including
C-----surface deformation dependent factor bsq
cc      ATIl = AP1*A(Nnuc) + BSQ(cigor)*AP2*A(Nnuc)**0.666667
cc      ATIl = ATIl*Rafis

      CALL DAMIRO_CRT(ia,iz,shcf,IOUt,1)

      momparcrt=mompar
      momortcrt=momort

      def2 = DEFfis(Ib)

      IF (mompar.LT.0.0D0 .OR. momort.LT.0.0D0) THEN
         WRITE (8,*) 'WARNING: Negative moment of inertia for spin ', Aj
         WRITE (8,*) 'WARNING: 0 level density returned by rodef'
         RETURN
      ENDIF

      DEL = 0.
      IF (FISden(Nnuc).LE.1.) THEN
         IF (MOD(in,2).NE.0) DEL = DELp
         IF (MOD(iz,2).NE.0) DEL = DEL + DELp
      ENDIF
c
 76   DO jj = 1,NLW
         aaj = FLOAT(jj) + HIS(Nnuc)
         DO kk = 1,NRBinfis(Ib)
            rotemp=0.d0
            u = XMInn(Ib) + (kk - 1)*desteppp + DEL!fis
            IF (u.GT.UCRt) THEN
               u = u - ECOnd
               accn = ATIl*FSHELL(u,Shcf,GAMma)
               IF (accn.LE.0.0D0) RETURN
            
               rotemp = RODEFF(A(Nnuc),u,accn,aaj,MOMpar,MOMort,
     &                         HIS(Nnuc),ARGred,
     &                         EXPmax,temp,def2,vibbf12,vibbfdt,vn)
c               write(*,*)'fmg',u,rotemp  
            ELSE
               accn = ACRt
               rotemp = ROBCSF(A(Nnuc),u,aaj,MOMparcrt,MOMortcrt,
     &                  mompar,temp,def2,vibbf12,vibbfdt,vn)*RORed

c            write(*,*)'bcs',u,rotemp  
            ENDIF
c-----------SYMMETRY ENHANCEMENT
 345        IF (Iff.EQ.2) rotemp =
     &             rotemp * SQRT(pi/2.) * SQRT(mompar * temp)
            IF (Iff.EQ.3) rotemp = rotemp * 2.d0
            IF (Iff.EQ.4) rotemp =
     &             rotemp* 2.* SQRT(2.* pi) * SQRT(mompar * temp)

            ROFis(kk,jj,Ib) = rotemp
            ROFisp(kk,jj,1,Ib) = rotemp
            ROFisp(kk,jj,2,Ib) = rotemp

            IF (Mmod.GT.0) ROFism(kk,jj,Mmod) = rotemp ! to be updated
         ENDDO

c        write(*,*)'ro',u, ROFisp(kk,1,1,1)
      ENDDO

 346  ACRtf(Ib) = ACRt
      UCRtf(Ib) = UCRt
      ECOndf(Ib) = ECOnd
      DETcrtf(Ib) = DETcrt
      TCRtf(Ib) = TCRt
      SCRtf(Ib) = SCR
      VIBf12(Ib)= vibbf12
      VIBfdt(Ib)= vibbfdt

      IF(IOUT.EQ.6) CALL PLOT_ZVV_SadLD(Nnuc,Ib)
      RETURN
      END
C

      REAL*8 FUNCTION ROBCSF(A,U,Aj,Mompar,Momort,Mompr,T,
     &                       def2,vibbf12,vibbfdt,vn)


      IMPLICIT REAL*8(A - H), REAL*8(O - Z)
C
C COMMON variables
C
      REAL*8 TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl    ! CRIT
      REAL*8 momo,momp
      COMMON /CRIT  / TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl
      COMMON /MOM/momo,momp
C
C Dummy arguments
C
      REAL*8 A, Aj, Momort, Mompar, Mompr,U, def2, T, bf
      REAL*8 vibbf12,vibbfdt,vn
      REAL*8 ro,rot_K, rot_Q, vib_KQ

     
      ROBCSF = 0.D0
      Bf=0.d0
c      write(*,*)'1',A,U,Aj,Mompar,Momort,def2,T,Bf
      ro = ROBCS(A,U,Aj,Mompar,Momort,def2,T,Bf)
c       write(*,*)'2',A,U,Aj,Mompar,Momort,def2,T,Bf,momp
    
      Mompr=Momp 

      CALL COLL_KQ_FIS(0,A,T,momo,def2,u,vib_KQ,rot_K,rot_Q,
     &                   vibbf12,vibbfdt,vn)
      ROBCSF = ro *  rot_K * rot_Q * vib_KQ

c      write(*,*)'ro', robcsf,ro,  rot_K, rot_Q, vib_KQ
c      pause
      RETURN
      END
C
C
      REAL*8 FUNCTION RODEFF(A,E,ac,Aj,Mompar,Momort,Ss,
     &                     Argred,Expmax,T,def2,vibbf12,vibbfdt,vn)

      IMPLICIT REAL*8(A - H), REAL*8(O - Z)
C Dummy arguments
      REAL*8 A, Ac, Aj, Argred, E, Expmax, Momort, Mompar, Ss,
     &       T,e1,yrast,bf
      REAL*8 vibbf12,vibbfdt,vn
      REAL*8 ro,rot_K, rot_Q, vib_KQ
      INTEGER egsm

      expmax=700.
      Bf=0.d0
      IF(ac.LE.0. .or. e.le.0.d0) RETURN
      egsm=1
      RODEFF = 0.D0
      Yrast=0.d0 
      
      ro =RODEF(A,E,Ac,Aj,Mompar,Momort,T,
     &                        Yrast,Ss,Bf,Argred,Expmax,def2,egsm)
      e1 = E - Yrast
      CALL COLL_KQ_FIS(egsm,A,T,momort,def2,e1,vib_KQ,rot_K,rot_Q,
     &                   vibbf12,vibbfdt,vn)
      RODEFF = ro * rot_K * rot_Q * vib_KQ

      RETURN
      END


      SUBROUTINE COLL_KQ_FIS(egsm,A,T,momo,def2,E1,vib_KQ,rot_K,rot_Q,
     &                   thalf,dt,vn)

      REAL*8 A,T,momo,def2,E1,vib_KQ,rot_K,rot_Q 
      REAL*8 qv,qr,vibrk
      REAL*8 vn


      INTEGER egsm
C
C Local variables
C
      REAL*8 arg, cost, dmpdiff, dmphalf, dt, ht, m0, pi, r0,
     &                 sdrop, thalf

C-----vib_K
      DATA m0, pi, r0, ht/1.044, 3.141592, 1.26, 6.589/
    
      sdrop = 17./(4.*pi*r0**2)
      cost = 3.*m0*A/(4.*pi*ht**2*sdrop)
      Vibrk = vn* 3.d0*EXP(1.7*cost**(2./3.)*T**(4./3.))
C-----vib_Q
      arg = (T - thalf)/dt
      qv = 1.0/(EXP((-arg)) + 1.0)
      IF (qv.GE.0.999D0) vibrk = 1.0
      vib_KQ = qv - vibrk*(qv - 1.)
C-----rot_K
      rot_K  = 1.0
      IF(egsm.eq.0)rot_K  = momo*t
C-----rot_Q
      dmphalf = 120.d0*A**0.333*def2**2         !according to RIPL-2
      dmpdiff =1400.*A**(-0.666)*def2**2
      Qr = 1./(1. + EXP((-dmphalf/dmpdiff)))
     &     - 1./(1. + EXP((e1-dmphalf)/dmpdiff))
      rot_Q  = 1.0 - qr * (1.0 - 1.0/(momo*t))
      RETURN
      END
C

      SUBROUTINE ROGSM_sys(Aas,gamm,del,delp,shcn,om2,dshif,Nnuc)
Cccc
Cccc  ********************************************************************
Cccc  *                    R O G S M_sys                                 *
Cccc  *                                                                  *
Cccc  * GSM level density systematics  from RIPL-2             .         *
Cccc  *                                                                  *
Cccc  ********************************************************************
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'

      REAL*8 aas,shcn,delp,del,gamm,dshif,om2
      INTEGER Nnuc
C Local variables
      REAL*8 shelMSr,shift,del0
      REAL*8 omm2
      INTEGER ia,iz,na,nz
      
      ia = INT(A(Nnuc))
      iz = INT(Z(Nnuc))

      OPEN(23,FILE=trim(empiredir)//'/RIPL-2/densities/total/'//
     &'level-densities-gsm-RIPL2.dat',  STATUS='old')
      READ(23,*)
      READ(23,*)
      READ(23,*)
      READ(23,*)
  40  READ(23,98,END=50,ERR=60)
     &    nz, na, shelMSr,omm2,del0,aas,shift
  98  FORMAT (1x,2(i3,1x),35x,e8.3,1x,e8.3,f7.3,f8.3,7x,f8.3)
         
      IF(nz.eq.iz.and.na.eq.ia)THEN
         shcn = shelMSr
         om2=omm2
         atil=aas
         delp=del0
         dshif=shift
         goto 50
      ELSE
         GOTO 40
      ENDIF
  60  STOP 'Error reading GSM file'
  50  CLOSE(23)
      
      gamm=0.375
      gamm = gamm/A(Nnuc)**0.333333

      IF(delp.EQ.0)  delp = 12./SQRT(A(nnuc))

      del = 0.d0
      IF (MOD((ia-iz),2).NE.0.0D0) del = delp
      IF (MOD(iz,2).NE.0.0D0) del = del + delp
      RETURN
      END

     
      SUBROUTINE EGSMsys(ap1,ap2,gamma,del,delp,nnuc)
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
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
      REAL*8 ap1, ap2, gam, gamma, del, delp

      del = 0.d0
      delp = 12./SQRT(A(nnuc))
      IF (MOD(XN(nnuc),2.D0).NE.0.0D0) del = delp
      IF (MOD(Z(nnuc),2.D0).NE.0.0D0) del = del + delp

Cccc  * MINUIT fit results:                                              
C-----parameters of Dec 4, 2008
C     frm=1.70   Chi**2=36 (per degree of freedom)                    
C     ap1 = 0.74055E-01
C     ap2 = 0.28598E-03
C     gam = 0.57248
C-----parameters of Jan 23, 2011
C     ap1 =  0.76122d-01
C     ap2 = -0.45559d-02
C     gam =  0.58269d0
C   frms  = 1.687
C   Chi-2 = 34.6
C-----parameters of Jan 26, 2011
C  Do-fit using RIPL-3 database, 2.19 vibr enhancement (MINUIT)       
C     alpha 0=  .0750000 delta alpha= .500000D-01
C     gam   0=  .5750000 delta gam  = .500000D-02
C ---------------------------------
C alpha=   7.488729E-02 gam=   5.697688E-01
C frm=       1.687021929004768 Chi^2=      27.301609174895010
C
      ap1 =  7.488729d-02
      ap2 =  0.d0
      gam =  5.697688D-01

c      ap1 = 0.74055E-01
c      ap2 = 0.28598E-03
c      gam = 0.57248
      gamma = gam/A(Nnuc)**0.333333
      IF(ATIlnoz(INT(Z(nnuc))) .eq. 0.d0) return
      ap1 = ap1*ATIlnoz(INT(Z(nnuc))) !apply elemental normalization factor
      ap2 = ap2*ATIlnoz(INT(Z(nnuc))) !apply elemental normalization factor
      RETURN
      END


