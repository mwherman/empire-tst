Ccc   * $Rev: 1961 $
Ccc   * $Author: rcapote $
Ccc   * $Date: 2011-01-27 22:54:59 +0100 (Do, 27 Jän 2011) $

C
C
      SUBROUTINE ROCOL(Nnuc,Cf)
CCC
CCC   *********************************************************************
CCC   *                                                         CLASS:PPU *
CCC   *                         R O C O L                                 *
CCC   *                                                                   *
CCC   *  CALCULATES SPIN DEPENDENT LEVEL DENSITIES INCLUDING              *
CCC   *  VIBRATIONAL AND ROTATIONAL COLLECTIVE EFFECTS AND                *
CCC   *  ACCOUNTING FOR THEIR ENERGY FADE-OUT                             *
CCC   *                             AND                                   *
CCC   *  SETS POTENTIAL SURFACE ENERGY FOR THE SADDLE POINT               *
CCC   *  (INCLUDING SHELL CORRECTION WITH ANGULAR MOMENTUM FADE-OUT)      *
CCC   *                                                                   *
CCC   *  NOTE:                                                            *
CCC   *  LEVEL DENSITY FOR THE SADDLE POINT IS CALCULATED AT THE          *
CCC   *  COMPOUND NUCLEUS ENERGY MINUS SADDLE POINT ENERGY (EX-BFIS)      *
CCC   *  FOR EACH SPIN (BFIS DEPENDS ON SPIN). THUS, ROF(IE,J,NNUC)       *
CCC   *  CORRESPONDS TO THE SADDLE POINT LEVEL DENSITIES CALCULATED       *
CCC   *  AT ENERGY EX(IE,NNUC)-FISB(J,NNUC) AND IS 0 FOR EX(IE,NNUC)      *
CCC   *  LOWER THAN SADDLE POINT ENERGY. IN OTHER WORDS, TAKING ROF(IE,.  *
CCC   *  FOR FISSION OF THE IE CONTINUUM STATE MEANS THAT KINETIC         *
CCC   *  ENERGY OF FRAGMENTS IS 0.                                        *
CCC   *                                                                   *
CCC   *  INPUT:                                                           *
CCC   *  NNUC - INDEX OF THE NUCLEUS (POSITION IN THE TABLES)             *
CCC   *  CF   - 1. FOR THE SADDLE POINT, 0. OTHERWISE                     *
CCC   *                                                                   *
CCC   *  BF CONTROLS SHAPE OF THE NUCLEUS                                 *
CCC   *     BF=0. STANDS FOR THE SADDLE POINT (CF=1)                      *
CCC   *     BF=1. STANDS FOR THE OBLATE   YRAST STATE                     *
CCC   *     BF=2. STANDS FOR THE PROLATE  YRAST STATE                     *
CCC   *     BF=3. STANDS FOR THE TRIAXIAL YRAST STATE                     *
CCC   *                                                                   *
CCC   * OUTPUT:NONE                                                       *
CCC   *                                                                   *
CCC   *                                                                   *
CCC   *********************************************************************
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
      REAL*8 Cf
      INTEGER Nnuc
C
C Local variables
C
      REAL*8 ac, aj, cigor, momort, mompar, rbmsph,
     &                 rotemp, saimid, saimin, saimx, selmax, stab, u
      REAL FLOAT
      INTEGER i, ia, iz, kk
      REAL*8 RODEF
      ia = A(Nnuc)
      iz = Z(Nnuc)
      A23 = A(Nnuc)**0.666667
C-----next call prepares for lev. dens. calculations
      CALL PRERO(Nnuc)
      BF = 1.0
      IF (Cf.NE.0.0D0) BF = 0.0
      DEL = ROPar(3,Nnuc)
      rbmsph = 0.01448*A(Nnuc)**1.66667
      ac = A(Nnuc)/ADIv
      DO kk = 1, NEX(Nnuc)
         u = EX(kk,Nnuc) - DEL
         UEXcit(kk,Nnuc) = MAX(u,0.D0)
         IF (ac.GT.0.D0) THEN
C-----------set nuclear temperature (spin independent taken at J=0 or 1/2)
            IF (BF.EQ.0.0D0) THEN !saddle point
               u = EX(kk,Nnuc) - DEL - FISb(1,Nnuc)
               UEXcit(kk,Nnuc) = MAX(u,0.D0)
               IF (u.GT.0.0D0) TNUcf(kk,Nnuc) = SQRT(u/ac)
            ELSE !normal states
               IF (u.GT.0.0D0) TNUc(kk,Nnuc) = SQRT(u/ac)
            ENDIF
C-----------set nuclear temperature  *** done ***
            DO i = 1, NLWst
               aj = FLOAT(i) + HIS(Nnuc)
C--------------saddle point
               IF (BF.EQ.0.0D0) THEN
                  u = EX(kk,Nnuc) - DEL - FISb(i,Nnuc)
                  UEXcit(kk,Nnuc) = MAX(u,0.D0)
                  IF (u.LE.0.0D0) EXIT
                  IF (Z(Nnuc).LT.102.D0 .AND. Z(Nnuc).GE.19.D0) THEN
C--------------------next call is to calculate deformation parameter A2 only
                     CALL SIGMAK(A(Nnuc),Z(Nnuc),DEF(1,Nnuc),0.0D0,u,ac,
     &                           aj,mompar,momort,A2,stab,cigor)
                     CALL MOMFIT(iz,ia,i - 1,saimin,saimid,saimx,selmax)
                     mompar = saimin*rbmsph
                     momort = saimx*rbmsph
                  ELSE
                     CALL SIGMAK(A(Nnuc),Z(Nnuc),DEF(1,Nnuc),1.0D0,u,ac,
     &                           aj,mompar,momort,A2,stab,cigor)
                  ENDIF
               ENDIF
               IF (u.LE.0.0D0) EXIT
C--------------normal states
               IF (BF.NE.0.0D0) THEN
C-----------------inertia moments by Karwowski with spin dependence
C-----------------(spin dependent deformation beta calculated according to B.-Mot.)
                  CALL SIGMAK(A(Nnuc),Z(Nnuc),DEF(1,Nnuc),1.0D0,u,ac,aj,
     &                        mompar,momort,A2,stab,cigor)
                  IF (A2.LT.0.D0) THEN
                     BF = 1
                  ELSE
                     BF = 2
                  ENDIF
               ENDIF

               rotemp = RODEF(A(Nnuc),u,ac,aj,mompar,momort,
     &                  YRAst(i,Nnuc),HIS(Nnuc),BF,ARGred,EXPmax)
               IF (rotemp.LT.RORed) rotemp = 0.0
               IF (BF.NE.0.0D0) THEN
                  RO(kk,i,1,Nnuc) = rotemp
                  RO(kk,i,2,Nnuc) = rotemp
               ELSE
                  ROF(kk,i,Nnuc) = rotemp
               ENDIF
            ENDDO
         ENDIF
  100 ENDDO
      END


      SUBROUTINE DAMPROT(E1,Qk)
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


      SUBROUTINE VIBR(A,T,Vibrk)
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
      Vibrk = DEXP(1.7*cost**(2./3.)*T**(4./3.))
C-----vibrational enhancement factor (up to EMPIRE-2.19)
C     VIBRK=EXP(4.7957*A**(2./3.)*T**(4./3.)/100.)
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
C     mompar = rbmsph*(1. - (2./3.)*a2)
C     momort = rbmsph*(1. + (1./3.)*a2)

      IF (ABS(A2).LE.0.001D0) Momort = Mompar
      END

      SUBROUTINE SIGMA(A,B,E,Ac,Spar,Sort)
      IMPLICIT REAL*8 (A-H,O-Z)
Cccc  *****************************************************************
Cccc  *  Calculates paralel and orthogonal spin cut-off factors
Cccc  *****************************************************************
C
C Dummy arguments
C
      REAL*8 A, Ac, B, E, Sort, Spar
C
C Local variables
C
      REAL*8 ht, m0, orti, pi, r0, rm2, sphi, t
      DATA m0, r0, ht, pi/1.044, 1.24, 6.589, 3.14159259/
      t = SQRT(E/Ac)
      sphi = 2./5.*m0*r0**2*A**(5./3.)
      orti = sphi*(1. + B/3.)
      rm2 = 0.24*A**(2./3.)
      Spar = SQRT(6./pi**2*rm2*t*Ac*(1. - 2.*B/3.))
      Sort = SQRT(orti*t/ht**2)
      IF (B.LE.0.05D0) Sort = Spar
      END


      SUBROUTINE DAMPKS(A,B,T,Q)
Ccc   *****************************************************************
Ccc   *              damping by Karwowski                             *
Ccc   *        slow for dmpc.gt.0    fast for dmpc.lt.0               *
Ccc   * q=0 for t=0, q=1/2 for t=ecoriolis, q=1 for t=infinity        *
Ccc   *****************************************************************
C
C COMMON variables
C
      REAL*8 DMPc
      COMMON /DAMPAR/ DMPc
C
C Dummy arguments
C
      REAL*8 A, B, Q, T
C
C Local variables
C
      REAL*8 arg, d, delta, r
C-----DMPC=1. selects slow damping
      DMPc = 1.
      IF (ABS(B).LT.0.0001D0 .OR. DMPc.EQ.0.0D0) THEN
         Q = 1.
         RETURN
      ENDIF
      d = ABS(DMPc)
C-----calculation of delta def. param from a2 def. param.
      r = 2.*(B + 1.)/(2. - B)
      r = r**2
      delta = 3.0*(r - 1.0)/2./(2.0 + r)
      delta = ABS(delta)
      IF (DMPc.GT.0.0D0) THEN
C--------slow damping
         arg = 74.0
         IF (T.NE.0.0D0) arg = d*delta*41.0/A**0.3333/T
         IF (arg.GT.74.D0) arg = 74.
         Q = 2.0/(EXP(arg) + 1.0)
      ELSE
C--------fast damping
         arg = 5.0*(1.0 - T*A**0.3333/(d*delta*41.0))
         IF (arg.LT.( - 74.D0)) arg = -74.
         Q = 1/(EXP(arg) + 1)
      ENDIF
      END


      SUBROUTINE DAMPV(T,Q)
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


      SUBROUTINE DAMP(A,B,U,Q)
Ccc   *****************************************************************
Ccc   *                  damping of Rastopchin                        *
Ccc   *****************************************************************
C
C Dummy arguments
C
      REAL*8 A, B, Q, U
C
C Local variables
C
      REAL ALOG
      REAL*8 de, delta, e, q1, q2, r, u1, u2
      Q = 0.0
C-----calculation of delta def. param from a2 def. param.
      r = 2.*(B + 1.)/(2. - B)
      r = r**2
      delta = 3.0*(r - 1.0)/2./(2.0 + r)
      delta = ABS(delta)
      IF (delta.LT.0.01D0 .OR. U.LT.0.0D0) RETURN
      u1 = 170.*A**(1./3.)*delta**2
      u2 = u1/ALOG(2.0)
      de = 1400*A**( - 2./3.)*delta**2
      e = (U - u1)/de
      q1 = 0.
      IF (e.LT.170.D0) q1 = 1./(1. + EXP(e))
      q2 = 0.
      IF (U/u2.LT.170.D0) q2 = EXP(( - U/u2))
      IF (U.LE.u1) THEN
         Q = q1
      ELSE
         Q = q2
      ENDIF
      END


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
      REAL*8 aj, ar, defit, dshi, dshif, dshift, ellq, exkk,
     &                 pi2, rocumd, rocumu, rocumul, rolev,
     &                 rotemp, xr, Ecrt
      CHARACTER*7 caz
      CHARACTER*13 fname
      CHARACTER*20 ctmp1
      CHARACTER*30 title

      REAL FLOAT
      REAL*8 FSHELL
      INTEGER i, ia, ij, in, iter, ix, iz, kk, kkl, kku, nplot
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
     &'('' EXCITATION ENERGY TABLE FOR A='',I3,'' Z='',I3,         ''
     &HAS NOT BEEN DETERMINED BEFORE CALL OF PRERO'',//
     &,'' LEVEL DENSITIES WILL NOT BE CALCULATED'')') ia, iz
         RETURN
      ENDIF
      IF (EX(NEX(Nnuc),Nnuc).LE.0.0D0 .AND. FITlev.EQ.0) RETURN

      CALL PRERO(Nnuc)

C-----determination of the pairing shift DEL according to Moeller-Nix (Nucl. Phys. A536 (1992) 20)
!       DELn = 4.8/FLOAT(in)**0.333333
!       DELp = 4.8/FLOAT(iz)**0.333333
!       DEL = MOD(in,2)*DELn + MOD(iz,2)*DELp
!       DELp = 0.5*(DELn + DELp) !reuse DELp symbol to keep previous notation
! !       DELp = DELn*0.72
! !       DEL = DEL*0.72
C-----determination of the pairing shift --- done -----
C
C-----set level density parameter systematics
C-----EMPIRE-3.0-dependence
      CALL EGSMsys(ap1,ap2,gamma,del,delp,nnuc)
      IF (BF.EQ.0.0D0 .AND. Asaf.GE.0.0D0) GAMma = Asaf
C-----set Ignatyuk type energy dependence for 'a'
      ATIl = AP1*FLOAT(ia) + AP2*A23
      ATIl = ATIl*ATIlnor(Nnuc)
      TCRt = 0.567*DELp
      IF (Asaf.LT.0.D0) THEN
         ACRt = -ATIl*Asaf
      ELSE
         ar = ATIl*(1.0 + SHC(Nnuc)*GAMma)
         DO ix = 1, 20
            xr = ar*TCRt**2
            ACRt = ATIl*FSHELL(xr,SHC(Nnuc),GAMma)
            IF (ABS(ACRt - ar).LE.0.001D0*ACRt) GOTO 100
            ar = ACRt
         ENDDO
         WRITE (8,*)
     &     ' WARNING: Search for critical a-parameter has not converged
     & for A=',A(nnuc),' Z=',Z(nnuc)
         WRITE (8,*)' WARNING: Last iteration has given acrt=', ACRt
         WRITE (8,*)' WARNING: Setting Acrt to 0.1, execution continues'
         ACRt = max(ACRt,0.1d0)
      ENDIF
  100 IF (ACRt.LT.0.1D0) ACRt = 0.1d0
      ECOnd = 1.5*ACRt*DELp**2/pi2
      UCRt = ACRt*TCRt**2 + ECOnd
C-----45.84 stands for (12/SQRT(pi))**2
      DETcrt = 45.84*ACRt**3*TCRt**5
      ACR = ATIl*FSHELL(UCRt,SHC(Nnuc),GAMma)
C
C     Added INITIALIZATION for ROPar(1,Nnuc) and ROPar(3,Nnuc)
C
C     IF(ROPar(1,nnuc).EQ.0. .AND. CF.EQ.0.) ROPar(1,nnuc) = ACR
      IF(CF.EQ.0.) ROPar(1,nnuc) = ACR
      IF(CF.EQ.0.) ROPar(3,nnuc) = del

      IF (BF.EQ.0.D0 .AND. Asaf.LT.0.0D0) ACR = ACRt
      SCR = 2.*ACRt*TCRt
C-----
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
C     ellq = Q(1,Nnuc) - ELV(NLV(Nnuc),Nnuc)
      ellq = Q(1,Nnuc) -( ELV(NLV(Nnuc),Nnuc) + LDShif(Nnuc) )
C-----get distance between Qn and UCRt
C     ellq = Q(1,Nnuc) - UCRt 
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
         defit = (ELV(NLV(Nnuc),Nnuc)+LDShif(Nnuc)+4.d0)/(NEXreq - 1)
C                                             /(NDEX   - 1)
         nplot = (ELV(NLV(Nnuc),Nnuc)+LDShif(Nnuc)+4.d0)/defit
  150    rocumul = 1.0
         iter = iter + 1
         kkl = 0
         kku = 0
         DO kk = 1, NDEX
C-----------clean RO matrix
            IF (BF.NE.0.0D0) THEN
               DO i = 1, NDLW
                  RO(kk,i,1,Nnuc) = 0.d0
                  RO(kk,i,2,Nnuc) = 0.d0
               ENDDO
            ENDIF
C-----------decrease energy shift above the last level to become 0 at Qn
            exkk = (kk - 1)*defit
            IF (exkk.LE.ELV(NLV(Nnuc),Nnuc)+ LDShif(Nnuc)) THEN
C           IF (exkk.LE.UCRt) THEN
               dshif = dshift
            ELSEIF (exkk.LT.Q(1,Nnuc) .AND. ellq.NE.0.0D0) THEN
               dshif = dshift*(Q(1,Nnuc) - exkk)/ellq
C           ELSEIF (exkk.LT.Q(1,Nnuc) .AND. ellq.NE.0.0D0) THEN
C              dshif = dshift*(Q(1,Nnuc) - exkk)/ellq
            ELSE
               dshif = 0.0
            ENDIF
            CALL DAMIRO(kk,Nnuc,dshif,defit,Asaf,rotemp,aj)
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
         dshi = dshi - (ELV(NLV(Nnuc),Nnuc) + LDShif(Nnuc))
         dshift = dshift + dshi
         IF (FITlev.GT.0.0D0) then 
          Ecrt = UCRt - DEL - dshift
	    write(8,*)
	    WRITE (8,*) '*****   A=',nint(A(nnuc)),
     &	 ' Z=',nint(Z(nnuc)),' Bn=',sngl(Q(1,nnuc)),
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

      IF(IOUt.eq.6 .and. NLV(Nnuc).GT.3 .and.
     &  defit*(NEX(Nnuc) - 1) .gt. 0.5*ELV(NLV(Nnuc),Nnuc) ) then

C--------cumulative plot of levels to the zvd
         if(SYMb(Nnuc)(2:2).eq.' ') then
           write(caz,'(I2.2,A1,A1,I3.3)')
     &      int(Z(Nnuc)), SYMb(Nnuc)(1:1),'_',int(A(Nnuc))
         else
           write(caz,'(I2.2,A2,I3.3)')
     &      int(Z(Nnuc)), SYMb(Nnuc), int(A(Nnuc))
         endif
         write(fname,'(A13)') '_GS_EMPNL.zvd'
         write(ctmp1,'(A20)') caz//fname
         write(title,'(a4,1x,i3,''-'',A2,''-'',I3,3H CN)')
     &     'tit:',int(Z(Nnuc)), SYMb(Nnuc), int(A(Nnuc))

         OPEN (36, FILE=ctmp1, STATUS='unknown')

         write(caz,'(A7)') 'Exp_Lev'
         CALL OPEN_ZVV(36,caz,title)
         WRITE (36,*) '0.0 1.0'
         DO kk = 2, NLV(Nnuc)
           WRITE (36,*) ELV(kk,Nnuc)*1d6,FLOAT(kk - 1)
           WRITE (36,*) ELV(kk,Nnuc)*1d6,FLOAT(kk)
         ENDDO
         CALL CLOSE_ZVV(36,' ',' ')

         write(caz,'(A7)') 'Cum_Tot'
         CALL OPEN_ZVV(36,caz,title)
         rocumul = 1.D0
         WRITE (36,*) '0.0 1.0'
         DO kk = 2, NEX(Nnuc)
C           if(defit*(kk - 1) .gt. ELV(NLV(Nnuc),Nnuc)+2.d0) exit
C           if(defit*(kk - 1) .gt. Q(1,Nnuc)+1.d0) exit
C-----------Integration over energy. There should be factor 2 because of the
C-----------parity but it cancels with the 1/2 steming from the trapezoid
C-----------integration
            DO ij = 1, NLWst
C-----------integration over energy. Parity dependence explicitly considered.
C-----------There is a factor 1/2 steming from the trapezoid integration
               rocumul = rocumul + 0.5d0*defit/RORed*
     &         (RO(kk - 1,ij,1,Nnuc) + RO(kk,ij,1,Nnuc) +
     &          RO(kk - 1,ij,2,Nnuc) + RO(kk,ij,2,Nnuc))
            ENDDO
            WRITE (36,*) defit*(kk - 1)*1.d6, rocumul
         ENDDO
         CALL CLOSE_ZVV(36,' ','NUMBER OF LEVELS ')

         close(36)

      ENDIF

C--------plotting fit of the levels with low energy formula  ***done***
C
C--------fitting discrete levels ---- done ------
C--------
C--------do loop over excitation energy
C--------
      IF (Q(1,Nnuc).EQ.0.0D0) THEN
         REWIND (25)
         CALL BNDG(1,Nnuc,Q(1,Nnuc))
      ENDIF
      ellq = Q(1,Nnuc) - (ELV(NLV(Nnuc),Nnuc) + LDShif(Nnuc))
      Ecrt = UCRt - DEL - dshift
C     ellq = Q(1,Nnuc) - Ecrt
      DO kk = 1, NEX(Nnuc)
C-----------clean RO matrix
         IF (BF.NE.0.0D0) THEN
            DO i = 1, NDLW
               RO(kk,i,1,Nnuc) = 0.d0
               RO(kk,i,2,Nnuc) = 0.d0
            ENDDO
         ENDIF
         IF (FITlev.LE.0.0D0 .OR. 
     &       EX(kk,Nnuc).GE.ELV(NLV(Nnuc),Nnuc)+ LDShif(Nnuc))
     &      THEN
C        IF (FITlev.LE.0.0D0 .OR. EX(kk,Nnuc).GE.UCRt) THEN
C           Changed from UCRt  
C           IF (EX(kk,Nnuc).LE.Ecrt) THEN
C              dshif = dshift
C           ELSEIF (EX(kk,Nnuc).LT.Q(1,Nnuc) .AND. ellq.NE.0.0D0) THEN
C              dshif = dshift*(Q(1,Nnuc) - EX(kk,Nnuc))/ellq
C           ELSE
C              dshif = 0.0
C           ENDIF
            IF (EX(kk,Nnuc).LT.Q(1,Nnuc) .AND. ellq.NE.0.0D0) THEN
               dshif = dshift*(Q(1,Nnuc) - EX(kk,Nnuc))/ellq
            ELSE
               dshif = 0.d0
            ENDIF
            CALL DAMIRO(kk,Nnuc,dshif,0.0D0,Asaf,rotemp,aj)
         ENDIF
      ENDDO

C-----plot of the l.d. formula
      IF(IOUt.eq.6 .and. NLV(Nnuc).GT.3) CALL PLOT_ZVV_GSLD(0,Nnuc)

      RETURN
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

      SUBROUTINE DAMIRO(Kk,Nnuc,Dshif,Destep,Asaf,Rotemp,Aj)
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
      REAL*8 Aj, Asaf, Destep, Dshif, Rotemp
      INTEGER Kk, Nnuc
C
C Local variables
C
      REAL*8 ac, accn, ampl, bsq, cigor, momort, mompar, phi,
     &       qigor, rbmsph, saimid, saimin, saimx, selmax,
     &       shredt, stab, t, temp, u
      LOGICAL bcs
      REAL*8 FSHELL, ROBCS, RODEF
      INTEGER i, ia, iz
      INTEGER INT

      bcs = .TRUE.
      rbmsph = 0.01448*A(Nnuc)**1.6667
      ia = INT(A(Nnuc))
      iz = INT(Z(Nnuc))
C-----determination of U for normal states
      IF (BF.NE.0.D0) THEN
         IF (Destep.NE.0.0D0) THEN
            u = (Kk - 1)*Destep + DEL + Dshif
         ELSE
            u = EX(Kk,Nnuc) + DEL + Dshif
         ENDIF
         IF (u.LE.0.0D0) RETURN
         IF (u.GT.UCRt) THEN
            u = u - ECOnd
            IF (u.LE.0.0D0) RETURN
            bcs = .FALSE.
         ELSE
            bcs = .TRUE.
         ENDIF
         IF (Destep.eq.0.0D0 .and. FITLEV.GT.0
     &      .and. EX(kk,Nnuc).LE. Q(1,NNuc)+1.d0) 
     &     write(8,*) '***  E=',sngl(EX(kk,Nnuc)),
     &        ' U= ',sngl(u),' shift(E)=',sngl(dshif) 
      ENDIF
C-----
C-----do loop over angular momentum
C
      DO i = 1, NLWst
         Aj = REAL(i) + HIS(Nnuc)
C
C-----a-parameter and U determination for fission channel
C
         IF (BF.EQ.0.0D0) THEN
C-----------temperature fade-out of the shell correction
C-----------ACCN  serves only to calculate temperature fade-out
            IF (EX(Kk,Nnuc).GT.UCRt) THEN
               accn = ATIl*(1 + SHC(Nnuc)
     &                *(1 - EXP((-GAMma*EX(Kk,Nnuc))))/EX(Kk,Nnuc))
            ELSE
               accn = ACRt
            ENDIF
            temp = 0.
            IF (EX(Kk,Nnuc).GE.YRAst(i,Nnuc))
     &          temp = SQRT((EX(Kk,Nnuc) - YRAst(i,Nnuc))/accn)
            ampl = EXP(TEMp0*SHRt)
            shredt = 1.
            IF (temp.GE.TEMp0) shredt = ampl*EXP(( - SHRt*temp))
C-----------temperature fade-out of the shell correction  --- done ----
            u = EX(Kk,Nnuc) + DEL - FISb(i,Nnuc) + SHC(Nnuc)
     &          *shredt*SHCjf(i,Nnuc)
            IF (u.GT.UCRt) THEN
               u = u - ECOnd
               bcs = .FALSE.
            ELSE
               bcs = .TRUE.
            ENDIF
            UEXcit(Kk,Nnuc) = MAX(u,0.D0)
            IF (u.LE.0.0D0) RETURN
            IF (Z(Nnuc).LT.102.0D0 .AND. Z(Nnuc).GE.19.0D0) THEN
C--------------next line is to calculate deformation parameter A2 only
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
            qigor = ( - 0.00246 + 0.3912961*cigor -
     &              0.00536399*cigor**2 - 0.051313*cigor**3 +
     &              0.043075445*cigor**4) - 0.375
            IF (qigor.GT.0.077D0) THEN
               bsq = 0.983 + 0.439*qigor
            ELSE
               bsq = 1.0 + 0.4*(cigor - 1.0)**2
            ENDIF
            ATIl = AP1*A(Nnuc) + AP2*A23*bsq
            ATIl = ATIl*ATIlnor(Nnuc)
            IF (Asaf.GE.0.D0) ac = ATIl*FSHELL(u,SHC(Nnuc),Asaf)
            IF (Asaf.LT.0.D0) ac = -ATIl*Asaf
            IF (ac.LE.0.D0) RETURN
         ELSE
C
C-----------Yrast states
C
C-----------spin  dependent moments of inertia for yrast states by Karwowski
C-----------(spin dependent deformation beta calculated according to B.-Mot.)
C-----------temporary value of 'a' parameter needed for ground state deformation
C-----------damping (no surface correction)
            ATIl = AP1*A(Nnuc) + AP2*A23
            ATIl = ATIl*ATIlnor(Nnuc)
            ac = ATIl*FSHELL(u,SHC(Nnuc),GAMma)
C-----------HERE here FSHELL can become negative
            IF (ac.LE.0.0D0) RETURN

            CALL SIGMAK(A(Nnuc),Z(Nnuc),DEF(1,Nnuc),1.0D0,u,ac,Aj,
     &                  mompar,momort,A2,stab,cigor)

            IF (A2.LT.0.D0) THEN
               BF = 1
            ELSE
               BF = 2
            ENDIF
C-----------calculation of level density parameter 'a' including surface
C-----------dependent factor
            qigor = ( - 0.00246 + 0.3912961*cigor -
     &              0.00536399*cigor**2 - 0.051313*cigor**3 +
     &              0.043075445*cigor**4) - 0.375
            IF (qigor.GT.0.077D0) THEN
               bsq = 0.983 + 0.439*qigor
            ELSE
               bsq = 1.0 + 0.4*(cigor - 1.0)**2
            ENDIF
            ATIl = AP1*A(Nnuc) + AP2*A23*bsq
            ATIl = ATIl*ATIlnor(Nnuc)
            ac = ATIl*FSHELL(u,SHC(Nnuc),GAMma)
            IF (ac.LE.0.0D0) RETURN
         ENDIF
C        Spin-cut off tuning  (0808 RCN &MS)
C        It destroys the continuity of the LD, we need to implement in a different way.
C        mompar = REDSEF*mompar
C        momort = REDSEF*momort

cc         CALL ROBCS_FG_FIS(A(Nnuc),U,Aj,T,shc(nnuc),gamma,mompar,rotemp)
cc            goto 345

         IF (bcs) THEN
            Rotemp = ROBCS(A(Nnuc),u,Aj,mompar,momort,A2)*RORed
C           IF (Destep.eq.0.D0) Rotemp = 0.d0
            IF (i.EQ.1) THEN
               phi = SQRT(1.D0 - u/UCRt)
               t = 2.0*TCRt*phi/LOG((phi + 1.D0)/(1.D0 - phi))
            ENDIF

         ELSE
            Rotemp = RODEF(A(Nnuc),u,ac,Aj,mompar,momort,YRAst(i,Nnuc),
     &               HIS(Nnuc),BF,ARGred,EXPmax)
            IF (i.EQ.1) t = SQRT(u/ac)
         ENDIF

 345     IF (BF.NE.0.0D0) THEN
            RO(Kk,i,1,Nnuc) = Rotemp
            RO(Kk,i,2,Nnuc) = Rotemp
         ELSE
            ROF(Kk,i,Nnuc) = Rotemp
         ENDIF
         IF (i.EQ.1) then

           TNUc(Kk,Nnuc) = t

         ENDIF
      ENDDO

99999 END

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
C-----check whether the nucleus is fissile
Csin      FISsil(Nnuc) = .TRUE.
Csin      xfis = 0.0205*Z(Nnuc)**2/A(Nnuc)
Csin      IF(xfis.LT.0.3D0)FISsil(Nnuc) = .FALSE.
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
      NLWst = NDLW
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
         DO j = 1, NDLW
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
C
C              Yrast states redefined for normal states to avoid discontinuities
C              as proposed by MS 
C
               segs = aj*(aj + 1)/(2.0*momort)   ! Jan 2011
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

            YRAst(j,Nnuc) = segs
            SHCjf(j,Nnuc) = SHCFADE(j - 1,SHRj,SHRd)
            FISb(j,Nnuc) = sb*QFIs + segs
            IF (JSTab(Nnuc).NE.0 .AND. j.GE.JSTab(Nnuc)) GOTO 50
C-----------determination of stability limit including shell correction
            IF (sb*QFIs - SHCjf(j,Nnuc)*SHC(Nnuc).LE.0.001D0) GOTO 50
            jstabf = j
         ENDDO
   50    IF (JSTab(Nnuc).EQ.0) JSTab(Nnuc) = jstabf
      ENDIF
      IF (JSTab(Nnuc).EQ.0) NLWst = MIN0(JSTab(Nnuc),NLWst)
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


      REAL*8 FUNCTION ROBCS(A,U,Aj,Mompar,Momort,A2)
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

      COMMON /CRIT  / TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl
C
C Dummy arguments
C
      REAL*8 A, A2, Aj, U, Momort, Mompar
C
C Local variables
C
      REAL*8 arg, const, det, dphi2, momo, momp, phi, phi2,
     &                 qdamp, qk, s, seff2, t, vibrk
     &
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
      IF (momp.LT.0.0D0) RETURN
      momo = Momort*0.3333D0 + 0.6666D0*Momort*TCRt*dphi2/t
      IF (momo.LT.0.0D0) RETURN
      seff2 = momp*t
      IF (ABS(A2).GT.0.005D0) seff2 = momp**0.333D0*momo**0.6667D0*t
C     seff2 = momp**0.333D0*momo**0.6667D0*t
      IF (seff2.LE.0.0D0) RETURN
      arg = s - (Aj + 0.5D0)**2/(2.D0*seff2)
      IF (arg.LE.0.0D0) RETURN
C     CALL DAMPKS(A, A2, t, qk)
      CALL DAMPROT(U,qk)
      qdamp = 1.D0 - qk*(1.D0 - 1.D0/(momo*t))
      ROBCS = 0.5D0*const*(2*Aj + 1.D0)*EXP(arg)/SQRT(seff2**3*det)
C-----vibrational enhancement factor (EMPIRE-2.19)
      CALL VIBR(A,t,vibrk)
C-----damping of vibrational effects
      CALL DAMPV(t,qv)
      IF (qv.GE.0.999D0) THEN
         vibrk = 1.0
      ELSE
         vibrk = qv - vibrk*(qv - 1.)
      ENDIF
      ROBCS = ROBCS*vibrk*momo*t*qdamp
      RETURN
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

      REAL*8 FUNCTION EVIBR(Z, A, Shell, Lamb)
Ccc
Ccc   ********************************************************************
Ccc   *                                                          class:pu*
Ccc   *                      E V I B R                                   *
Ccc   *                                                                  *
Ccc   * Estimates energies of the vibrational 2+ and 3- levels  using    *
Ccc   * EMPIRE-3.0  systematics fitted to the levels provided by Hilaire *
Ccc   *                                                                  *
Ccc   * input: Z - atomic mass of the nucleus                            *
Ccc   *        A - mass of the nucleus                                   *
Ccc   *    Shell - shell corrections                                     *
Ccc   *  Defener - deformation energy                                    *
Ccc   *     Lamb - level spin (2 0r 3)                                   *
Ccc   *                                                                  *
Ccc   * output: Q - vibrational enhancement factor due to the OM state   *
Ccc   *                                                                  *
Ccc   * calls:none                                                       *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      IMPLICIT REAL*8 (a-h,o-z)
      REAL*8 Z,A, Shell
      INTEGER LAMB

      Defener = 0.0D0   !just keep it if we decide to go for more tricky 2+
C-----The two Evibr below form a new systematics of the vibrational 2+ energies
C-----They are actually over written since their usage in the Do fit
C-----produces poor results.
      IF(Lamb .EQ. 2 .AND. Defener .LE. 0.01D0) THEN !spherical 2+
         Evibr = (4/(A-Z)**0.333-0.05*Shell)
      ELSEIF(Lamb .EQ. 2 .AND. Defener .GT. 0.01D0) THEN !deformed 2+
         Evibr = (0.30*Defener+(4-0.55*Shell)/(A-Z)**0.333)
      ELSEIF(Lamb .EQ. 3 ) THEN ! any 3-
         fsh = 1./(1.+0.050*Shell)
         Evibr = 110./A**(5./6.)*fsh
      ELSE
         WRITE(8,*)'EVIBR: unsupported spin',Lamb,' (must be 2 or 3)'
         STOP 'EVIBR: unsupported spin (must be 2 or 3)'
      ENDIF
      if(Lamb .EQ. 2) THEN
c        Evibr = 30./A**0.66666   ! Ignatyuk
         Evibr = 27./A**0.666667
      endif
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
C
C Dummy arguments
C
      INTEGER Nnuc
      REAL*8 Scutf
C
C Local variables
C
      REAL*8 am, amas, arg, atil, e, efort, enorm, eo,
     &                 eom, exl, rhou, rjj, rolowint, sigh, sigl, t, tm,
     &                 u, ux, xj
      CHARACTER*6 ctmp
      REAL*8 DEXP
      REAL FLOAT
      INTEGER i, ig, igna, il, iter, j
      INTEGER INT
      INTEGER*4 iwin
      INTEGER*4 PIPE
      eom = 0.0
C-----next call prepares for lev. dens. calculations
      CALL PRERO(Nnuc)
      amas = A(Nnuc)
      igna = 0
C-----zero potentially undefined variables
      GAMma = 0.0
      exl = 0.0
      sigh = 0.0
C-----a-parameter given in input
      IF (ROPaa(Nnuc).GT.0.0D0) ROPar(1,Nnuc) = ROPaa(Nnuc)
C-----Ignatyuk parametrization
      enorm = 5.0
      IF (ROPaa(Nnuc).EQ.0.0D0) THEN
         atil = 0.154*A(Nnuc) + 6.3E-5*A(Nnuc)**2
C--------next line assures normalization to experimental data (on average)
         atil = atil*ATIlnor(Nnuc)
         GAMma = -0.054
         ROPar(1,Nnuc) = atil*(1.0 + SHC(Nnuc)*(1.0 - EXP(GAMma*enorm))
     &                   /enorm)
         igna = 1
      ENDIF
C-----Arthurs' parametrization
      IF (ROPaa(Nnuc).EQ.( - 1.0D0)) THEN
         atil = 0.1375*A(Nnuc) - 8.36E-5*A(Nnuc)**2
C--------next line assures normalization to experimental data (on average)
         atil = atil*ATIlnor(Nnuc)
         GAMma = -0.054
         ROPar(1,Nnuc) = atil*(1.0 + SHC(Nnuc)*(1.0 - EXP(GAMma*enorm))
     &                   /enorm)
         igna = 1
      ENDIF
C-----Mebel's  parametrization (taken from the INC code for the case
C-----of no collective enhancements) normalized to existing exp. data
      IF (ROPaa(Nnuc).EQ.( - 2.0D0)) THEN
         atil = 0.114*A(Nnuc) + 9.80E-2*A(Nnuc)**0.666667
C--------next line assures normalization to experimental data (on average)
         atil = atil*ATIlnor(Nnuc)
         GAMma = -0.051
         ROPar(1,Nnuc) = atil*(1.0 + SHC(Nnuc)*(1.0 - EXP(GAMma*enorm))
     &                   /enorm)
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
         IF (iter.LT.301) THEN
            WRITE (8,*) 'WARNING: Number of iterations in ROGC ',
     &                  iter - 1
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
C--------anyhow, plot fit of the levels with the low energy l.d. formula
         IF (FITlev.GE.0.0D0) THEN
            IF (NLV(Nnuc).GT.3) THEN
               WRITE (8,*) ' a=', A(Nnuc), 'Z=', Z(Nnuc)
               WRITE (8,*) ' A=', am, ' UX=', ux, ' T=', tm, ' EO=', eo
               OPEN (35,FILE = 'fort.35')
               WRITE (35,99005) INT(Z(Nnuc)), SYMb(Nnuc), INT(A(Nnuc)),
     &                          NLV(Nnuc)
99005          FORMAT ('set title "NO SOLUTION FOR ',I3,'-',A2,'-',I3,
     &                 ' Ncut=',I3,'"')
               WRITE (35,*) 'set terminal postscript enhanced color lw 2
     & solid "Helvetica" 20'
               WRITE (35,*) 'set output "|cat >>CUMULPLOT.PS"'
               WRITE (35,*) 'set logscale y'
               WRITE (35,*) 'set xlabel "Excitation energy (MeV)" 0,0'
             WRITE (35,*) 'set ylabel "Cumulative number of levels" 0,0'
               WRITE (35,*) 'set style line 1 lt 5 lw 2'
               WRITE (35,*) 'set style line 2 lt 1 lw 2'
               WRITE (35,'(''plot "fort.36" w filledcu y2 ls 2 t "Discre
     &te levels", "fort.34" w l ls 1 t "Level density" '')')
               CLOSE (35)
               OPEN (34,FILE = 'fort.34')
               OPEN (36,FILE = 'fort.36')
               WRITE (36,*) '0.0 1.0'
               DO il = 1, NLV(Nnuc)
                  WRITE (36,*) ELV(il,Nnuc), FLOAT(il - 1)
                  WRITE (36,*) ELV(il,Nnuc), FLOAT(il)
C-----------------integration over energy. There should be factor
C-----------------2 because of the parity
                  rolowint = EXP(( - eom/tm))
     &                       *(EXP(ELV(il,Nnuc)/tm) - 1.)
                  WRITE (34,*) ELV(il,Nnuc), rolowint + 1.0
               ENDDO
               CLOSE (36)
               CLOSE (34)
               IF (IOPsys.EQ.0) THEN
                  iwin = PIPE('gnuplot fort.35#')
                  CLOSE (35)
               ENDIF
            ENDIF
C-----------set nuclear temperature to the value from the systematics
            t = 0.9 - 0.0024*amas
            IF (amas.LT.100.D0) t = 60./amas + 0.06
            tm = t
            GOTO 500
C-----------plotting fit of the levels with low energy formula  ***done***
         ELSEIF (FITlev.LT.0.0D0) THEN
            WRITE (8,*) ' ERROR IN DISCRETE LEVEL FITTING'
            WRITE (8,*) ' EXECUTION STOPPED BECAUSE OF FITLEV<0 OPTION '
            STOP 'ERROR IN DISCRETE LEVEL FITTING (GC)'
         ENDIF
      ENDIF
      DO i = 1, 10
         IF (ux.EQ.0.0D0) ux = t*t*(am - 3/t + SQRT((am-6/t)*am))/2.0
         IF (igna.EQ.0D0) GOTO 200
         am = atil*(1.0 + SHC(Nnuc)*(1.0 - EXP(GAMma*ux))/ux)
      ENDDO
  200 exl = ux + DEL
C-----IF(Scutf.LT.0.0D0)sigh = could be calculated according to Dilg's recommendations
C-----0.6079 = 6/pi^2          a=6/pi^2*g     sig^2 = <m^2>gt    Scutf = <m^2>
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
         rhou = DEXP(2.*SQRT(am*ux))/(12.*SQRT(2*sigh))
     &          /am**0.25/ux**1.25
         eo = exl - t*LOG(t*rhou)
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
            GOTO 100
         ENDIF
      ENDIF
C-----plot fit of the levels with the low energy l.d. formula
      IF (FITlev.GT.0.0D0 .AND. NLV(Nnuc).GT.5) THEN
         WRITE (8,*) ' A=', A(Nnuc), 'Z=', Z(Nnuc), ' Ncut=', NLV(Nnuc)
         WRITE (8,*) ' a=', am, ' Ux=', ux, ' T=', t, ' EO=', eo
         OPEN (35,FILE = 'fort.35')
         WRITE (35,*) 'set terminal postscript enhanced color lw 2
     & solid "Helvetica" 20'
         WRITE (35,*) 'set output "|cat >>CUMULPLOT.PS"'
         WRITE (35,99010) INT(Z(Nnuc)), SYMb(Nnuc), INT(A(Nnuc)), am, t,
     &                    eo, NLV(Nnuc)
99010    FORMAT ('set title "',I3,'-',A2,'-',I3,': a=',
     &           F4.1,' T=',F4.1,' E0=',F4.1,' Ncut=',I3,'"')
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
         DO il = 1, NLV(Nnuc)
            WRITE (36,*) ELV(il,Nnuc), FLOAT(il - 1)
            WRITE (36,*) ELV(il,Nnuc), FLOAT(il)
C-----------Integration over energy.
            rolowint = EXP(( - eo/t))*(EXP(ELV(il,Nnuc)/t) - 1.)
            WRITE (34,*) ELV(il,Nnuc), rolowint + 1.0
         ENDDO
         CLOSE (36)
         CLOSE (34)
         IF (IOPsys.EQ.0) THEN
            iwin = PIPE('gnuplot fort.35#')
            CLOSE (35)
         ENDIF
      ENDIF
C-----plotting fit of the levels with low energy formula  ***done***
  500 ROPar(1,Nnuc) = am
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
               rhou = EXP(arg)/t
C--------------Spin-cutoff is interpolated
               SIG = sigl
               IF (e.GT.ECUt(Nnuc)) SIG = (sigh - sigl)*(e - ECUt(Nnuc))
     &             /(exl - ECUt(Nnuc)) + sigl
               DO j = 1, NLW
                  xj = j + HIS(Nnuc)
C                 arg = (xj + 1)*xj/(2.*Sig)
                  arg = (xj + 0.5)**2/(2.*SIG)
                  IF (arg.LE.EXPmax) THEN
                     rjj = (2*xj + 1.)/(2.*SIG)*EXP( - arg)
C--------------------0.5 coming from parity
C                    RO(i,j,Nnuc) = 0.5*rhou*rjj
C                    IF (RO(i,j,Nnuc).LT.RORed) RO(i,j,Nnuc) = 0.
                     RO(i,j,1,Nnuc) = 0.5*rhou*rjj
                     RO(i,j,2,Nnuc) = 0.5*rhou*rjj
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
            IF (igna.EQ.1) am = atil*(1.0 + SHC(Nnuc)*(1.0 - EXP(GAMma*u
     &                          ))/u)
            UEXcit(i,Nnuc) = MAX(u,0.D0)
            TNUc(i,Nnuc) = SQRT(u/am)
C-----------RCN 12/2004
C-----------IF(Scutf.LT.0.0D0)sigh = could be calculated according to Dilg's recommendations
C-----------0.6079 = 6/pi^2          a=6/pi^2*g     sig^2 = <m^2>gt    Scutf = <m^2>
            SIG = Scutf*0.6079*amas**0.6666667*SQRT(u*am)
            arg = 2.*SQRT(am*u) - ARGred
            IF (arg.LE.EXPmax) THEN
               rhou = DEXP(arg)/(12.*SQRT(2*SIG))/am**0.25/u**1.25
               DO j = 1, NLW
                  xj = j + HIS(Nnuc)
C                 arg = (xj + 1)*xj/(2.*Sig)
                  arg = (xj + 0.5)**2/(2.*SIG)
                  IF (arg.LT.EXPmax) THEN
                     rjj = (2*xj + 1.)/(2.*SIG)*EXP( - arg)
C--------------------0.5 coming from parity
C                    RO(i,j,Nnuc) = 0.5*rhou*rjj
C                    IF (RO(i,j,Nnuc).LT.RORed) RO(i,j,Nnuc) = 0.d0
                     RO(i,j,1,Nnuc) = 0.5*rhou*rjj
                     RO(i,j,2,Nnuc) = 0.5*rhou*rjj
                     IF (RO(i,j,1,Nnuc).LT.RORed) RO(i,j,1,Nnuc) = 0.d0
                     IF (RO(i,j,2,Nnuc).LT.RORed) RO(i,j,2,Nnuc) = 0.d0
                  ENDIF
               ENDDO
            ENDIF
         ENDDO
      ENDIF
      IF (IOUt.GE.6. .AND. FITlev.GT.0.0D0 .AND. NEX(Nnuc).GT.1) THEN
C--------plot level density
         WRITE (ctmp,'(I3.3,A1,I2.2)') INT(A(Nnuc)), '_', INT(Z(Nnuc))
         OPEN (38,FILE = 'GCLD'//ctmp//'.DAT')
         DO i = 1, NEX(Nnuc)
            u = EX(i,Nnuc)
            rolowint = 0.D0
C           RECTANGULAR INTEGRATION (no 1/2)
            DO j = 1, NLW
               rolowint = rolowint + RO(i,j,1,Nnuc) + RO(i,j,2,Nnuc)
            ENDDO
            WRITE (38,'(1x,8(e10.3,1x))') u, rolowint*EXP(ARGred),
     &             RO(i,1,1,Nnuc)*EXP(ARGred), RO(i,2,1,Nnuc)
     &             *EXP(ARGred), RO(i,3,1,Nnuc)*EXP(ARGred),
     &             RO(i,1,2,Nnuc)*EXP(ARGred), RO(i,2,2,Nnuc)
     &             *EXP(ARGred), RO(i,3,2,Nnuc)*EXP(ARGred)
         ENDDO
         CLOSE (38)
      ENDIF
      ROPar(4,Nnuc) = eo
      ROPar(2,Nnuc) = ux

C-----plot of the l.d. formula
      IF(IOUt.eq.6 .and. NLV(Nnuc).GT.3) CALL PLOT_ZVV_GSLD(2,Nnuc)

	RETURN
      END
C
C
      SUBROUTINE QVIBR(A,T,OM,LAM,Q)
Ccc
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
      REAL*8 A,T,OM,Q
      INTEGER LAM
      REAL*8 cga, gam, fn, U, S
      Q=1.D0
      IF(T.LT.0.01) RETURN
      cga=0.0075D0*A**(1./3.)
      GAM=cga*(OM**2+(2.*3.141593*T)**2)
      FN=DEXP(-GAM/OM/2.D0)/(DEXP(OM/T)-1.D0)
      IF(FN.LT.0.d0) RETURN
      U=LAM*OM*FN
      S=LAM*((1.+FN)*DLOG(1.+FN)-FN*DLOG(FN))
      Q=DEXP(S-U/T)
      if (Q.lt.1.D0) Q=1.D0
      RETURN
      END
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
C        SHC(Nnuc) = shelMSr - defcorr
         SHC(Nnuc) = SHLlnor(nnuc)*shelMSr
      ENDIF
C-----projectile
      IF (nz.EQ.Z(0) .AND. na.EQ.A(0)) THEN
C        SHC(0) = shelMSr - defcorr
         SHC(0) = SHLlnor(nnuc)*shelMSr
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
      REAL*8 c1, c2, hhh, r1, r2, corr1,
     & rhogrid(0:NLDGRID,JMAX,2), rhoogrid(0:NLDGRID,2),
     & rhotgrid(0:NLDGRID,2), cgrid(0:NLDGRID,2),
     & uugrid(0:NLDGRID), tgrid(0:NLDGRID), u, rocumul, pcorr, acorr
      CHARACTER*2 car2
      CHARACTER*8 paritate
      REAL*8 DLOG10
      CHARACTER*30 title
      CHARACTER*13 fname
      CHARACTER*20 ctmp
      CHARACTER*7 caz
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
C99005 FORMAT ('/RIPL-2/densities/Gs/z',i3.3,'.tab')
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
99010 FORMAT (23x,a2,i3,3x,i3,2x,a8)
      IF (car2.NE.'Z=') GOTO 100
      BACKSPACE (34)
      READ (34,99010,ERR=300,END = 300) car2, izr, iar, paritate
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
C99007   FORMAT ('/RIPL-2/densities/Gs/z',i3.3,'.cor')
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
      IF(IOUt.eq.6 .and.NLV(Nnuc).GT.3) then
C--------Cumulative Level Density plot
         if(SYMb(Nnuc)(2:2).eq.' ') then
           write(caz,'(I2.2,A1,A1,I3.3)')
     &      int(Z(Nnuc)), SYMb(Nnuc)(1:1),'_',int(A(Nnuc))
         else
           write(caz,'(I2.2,A2,I3.3)')
     &      int(Z(Nnuc)), SYMb(Nnuc), int(A(Nnuc))
         endif

         write(fname,'(A13)') '_GS_HFBNL.zvd'
         write(ctmp,'(A20)') caz//fname

         write(title,'(a4,1x,i3,''-'',A2,''-'',I3,3H CN)')
     &     'tit:',int(Z(Nnuc)), SYMb(Nnuc), int(A(Nnuc))
         write(caz,'(A7)') 'Exp_Lev'

         OPEN (36, FILE=ctmp, STATUS='unknown')
         CALL OPEN_ZVV(36,caz,title)
         WRITE (36,*) '0.0 1.0'
         DO kk = 2, NLV(Nnuc)
           WRITE (36,*) ELV(kk,Nnuc)*1d6,FLOAT(kk - 1)
           WRITE (36,*) ELV(kk,Nnuc)*1d6,FLOAT(kk)
         ENDDO
         CALL CLOSE_ZVV(36,' ',' ')

         write(caz,'(A7)') 'HFB_total'
         CALL OPEN_ZVV(36,caz,' ')
         WRITE (36,*) '0.0 1.0'
         DO kk = 1, iugrid
           IF(uugrid(kk) .gt. ELV(NLV(Nnuc),Nnuc)+2.d0) exit
           WRITE (36,*) uugrid(kk)*1d6,cgrid(kk,1)+cgrid(kk,2)
         ENDDO
         CALL CLOSE_ZVV(36,'Excitation Energy','NUMBER OF LEVELS')
         close(36)
      ENDIF

      DO kk = 1, NEX(Nnuc)
         u = EX(kk,Nnuc) - ROHfbp(nnuc)
         UEXcit(kk,Nnuc) = EX(kk,Nnuc)
         IF (u.LT.0.) CYCLE
         IF (u.GT.200.0D0) THEN
            WRITE (8,*) ' '
            WRITE (8,*) ' HFB LEV. DENS. DEFINED UP TO 200 MeV ONLY'
            WRITE (8,*) ' REQUESTED ENERY IS ', u, ' MeV'
            WRITE (8,*) ' YOU HAVE TO USE ANOTHER LEVEL DENSITIES'
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
      IF(IOUt.eq.6 .and.NLV(Nnuc).GT.3) CALL PLOT_ZVV_GSLD(3,Nnuc)

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
            WRITE (36,*) ELV(il,Nnuc), FLOAT(il - 1)
            WRITE (36,*) ELV(il,Nnuc), FLOAT(il)
         ENDDO
         rocumul = 1.0
         WRITE (34,*) '0.0  ', rocumul
         DO kk = 2, NFIsen1
C-----------integration over energy. Parity dependence explicitly considered.
C-----------There is a factor 1/2 steming from the trapezoid integration
C              rocumul = rocumul + 0.5d0*defit/RORed*
C           DO ij = 1, NFISJ1
            DO ij = 1, NLW
               rocumul = rocumul +
     &         (RO(kk - 1,ij,1,Nnuc) + RO(kk,ij,1,Nnuc) +
     &          RO(kk - 1,ij,2,Nnuc) + RO(kk,ij,2,Nnuc))
            ENDDO
            WRITE (34,*) ELV(kk,Nnuc), 0.5d0*rocumul
         ENDDO
         CLOSE (36)
         CLOSE (34)
         IF (IOPsys.EQ.0) THEN
            iwin = PIPE('gnuplot fort.35#')
            CLOSE (35)
         ENDIF
      ENDIF
      RETURN
      END

      REAL*8 FUNCTION RODEF(A,E,Ac,Aj,Mompar,Momort,
     &                                Yrast,Ss,Bf,Argred,Expmax)
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
Ccc   *                                                                   *
Ccc   *                                                                   *
Ccc   *********************************************************************
Ccc
C
C
C Dummy arguments
C
      REAL*8 A, Ac, Aj, Argred, Bf, E, Expmax, Momort, Mompar,
     &       Ss, Yrast
C
C Local variables
C
      REAL*8 ak, arg, con, const, e1, qk, qv, seff, sort2,
     &       sum, t, u, vibrk
      INTEGER i, k, kmin

C     REAL*8 q2, om3, q3, om2
C     REAL*8 EVIBR


      DATA const/0.01473144/
C-----CONST=1.0/(24.0*SQRT(2.0))/2.0
C-----the last 2.0 takes into account parity (half to half)
C-----BF controls shape of the nucleus
C-----BF=0. stands for the saddle point         (rot. perpend. to symm.)
C-----BF=1. stands for the oblate yrast state   (rot. paralel  to symm.)
C-----BF=2. stands for the prolate yrast state  (rot. perpend. to symm.)
C-----BF=3. stands for the triaxial yrast state (rot. perpend. to long )
      RODEF = 0.0
      sum = 0.0
      IF (Mompar.LT.0.0D0 .OR. Momort.LT.0.0D0) THEN
         WRITE (8,*) 'WARNING: Negative moment of inertia for spin ', Aj
         WRITE (8,*) 'WARNING: 0 level density returned by rodef'
         RETURN
      ENDIF
      IF (Yrast .LT. 0.0D0) RETURN !Should not happen
      IF (Ac.LE.0.1D0) RETURN 
C     IF (Ac.LE.0.0D0) THEN
C        WRITE (8,'('' FATAL: LEVEL DENS. PARAMETER a<0.05 IN RODEF'')')
C        STOP
C     ENDIF
      seff = 1.0/Mompar - 1.0/Momort
      IF (Bf.EQ.0.0D0) THEN
         e1 = E
      ELSE
         e1 = E - Yrast
      ENDIF
      IF (e1.LE.0.0D0) RETURN
      t = SQRT(e1/Ac)
      con = const/Ac**0.25/SQRT(Mompar*t)
C-----vibrational enhancement factor (EMPIRE-2.19)
      CALL VIBR(A,t,vibrk)
C-----damping of vibrational effects
        CALL DAMPV(t,qv)
        IF (qv.GE.0.999D0) vibrk = 1.0
!       write(6,*)'DEF: A, Aj, vibrk', A, Aj, vibrk, om2, om3, q2, q3
C-----damping of rotational  effects with Fermi function independent
C-----of deformation and mass number (consistent with the builtin systematics)
      CALL DAMPROT(e1,qk)
C-----damping ***** done ********
      sort2 = Momort*t
      IF (Ss.EQ.( - 1.0D0)) THEN
         arg = 2*SQRT(Ac*e1) - Argred
         IF (arg.LE.( - Expmax)) THEN
            sum = 0.0
         ELSEIF (e1.GT.1.0D0) THEN
            sum = EXP(arg)/e1**1.25
         ELSE
            sum = EXP(arg)
         ENDIF
         IF (Aj.LT.1.0D0) GOTO 100
      ENDIF
      i = Aj + 1.
      IF (Ss.EQ.( - 1.0D0)) THEN
         kmin = 2
      ELSE
         kmin = 1
      ENDIF
      DO k = kmin, i
         ak = k + Ss
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
  100 RODEF = con*sum*(1.0 - qk*(1.0 - 1.0/sort2))
     &        *(qv - vibrk*(qv - 1.))
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

      IF(IOUT.EQ.6) CALL PLOT_ZVV_SadLD(Nnuc,Ib)

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
      CHARACTER*8 paritate
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

      OPEN (UNIT = 34,FILE = trim(empiredir)//filename,ERR = 300)
  100 READ (34,99010,ERR = 100,END = 300) car2, izr, iar, paritate
99010 FORMAT (23x,a2,i3,3x,i3,2x,a8)
      IF (car2.NE.'Z=') GOTO 100
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
     &                 rotemp, shcf, u, xmax, xr, mompar, momort, temp,
     &                 vibbf12, vibbfdt, def2, stab, aj, vn
C     REAL*8 qigor
      REAL FLOAT
      INTEGER ia, iff, in, ix, iz, jj, kk, nr
      INTEGER INT
      REAL*8 ROBCSF, RODEFF, FSHELL

C-----continuum, level densities at saddle points
      excn1 = EMAx(Nnuc)
C-----where continuum starts,ends,steps in between
      IF (Mmod.EQ.0) THEN
         XMInn(Ib) = 0.0001
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

c      vibbf12 = vibf12(ib)
c      vibbfdt = vibfdt(ib)

      mm2 = 0.24 * A(Nnuc)**0.666667
      r0=1.24
      iff = 1

C-----EMPIRE-3.0-dependence
      CALL EGSMsys(ap1,ap2,gamma,del,delp,nnuc)

      IF (Mmod.EQ.0) THEN
         GAMma = GAMmafis(Ib)
         DELp = DELtafis(Ib)
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
      ENDIF
      gamma = gamma/A(Nnuc)**0.333333
      ATIl = AP1*A(Nnuc) + AP2*A(Nnuc)**0.666667
      ATIl = ATIl*Rafis
      ar = ATIl*(1.0 + shcf*GAMma)
C
C     ar - approximated value of "a"-parameter, which is only
C     used in SIGMAK to estimate the temperature
C
      aj=0.
      u=0.
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
      bsq = 1.d0
c      qigor = ( - 0.00246 + 0.3912961*cigor -
c     &              0.00536399*cigor**2 - 0.051313*cigor**3 +
c     &              0.043075445*cigor**4) - 0.375
c      IF (qigor.GT.0.077D0) THEN
c         bsq = 0.983 + 0.439*qigor
c      ELSE
c         bsq = 1.0 + 0.4*(cigor - 1.0)**2
c      ENDIF
c      bsq=1.
      ATIl = AP1*A(Nnuc) + bsq*AP2*A(Nnuc)**0.666667
      ATIl = ATIl*Rafis

      TCRt =0.567*DELp
      ar = ATIl*(1.0 + shcf*GAMma)
      DO ix = 1, 20
         xr = ar*TCRt**2
         ACRt = ATIl*FSHELL(xr,shcf,GAMma)
         IF (ABS(ACRt - ar).LE.0.001D0*ACRt) GOTO 300
         ar = ACRt
      ENDDO
      WRITE (8,*) ' WARNING: Last iteration acrt=', ACRt
      WRITE (8,*) ' WARNING: Execution continues'
  300 IF (ACRt.LT.0.0D0) ACRt = 0.0

      ECOnd = 1.5*ACRt*DELp**2/(PI*PI)
      UCRt = ACRt*TCRt**2 + ECOnd
C-----45.84 stands for (12/SQRT(pi))**2
      DETcrt = 45.84*ACRt**3*TCRt**5
      SCR = 2.*ACRt*TCRt

      momparcrt=mompar
      momortcrt=momort

      def2 = DEFfis(Ib)

      IF (mompar.LT.0.0D0 .OR. momort.LT.0.0D0) THEN
         WRITE (8,*) 'WARNING: Negative moment of inertia for spin ', Aj
         WRITE (8,*) 'WARNING: 0 level density returned by rodef'
         RETURN
      ENDIF
c
      DEL = 0.
      IF (FISden(Nnuc).LE.1.) THEN
         IF (MOD(in,2).NE.0) DEL = DELp
         IF (MOD(iz,2).NE.0) DEL = DEL + DELp
      ENDIF

 76   DO jj = 1,NLW
         aaj = FLOAT(jj) + HIS(Nnuc)
         DO kk = 1,NRBinfis(Ib)
            rotemp=0.d0
            u = XMInn(Ib) + (kk - 1)*desteppp + DEL
c-----------Ignatyuk
            IF(FISden(Nnuc).EQ.0)goto 344
            CALL ROBCS_FG_FIS(A(Nnuc),U,Aaj,Temp,shcf,gamma,mompar,
     &                        deffis(ib),rotemp)
           goto 345
 344       IF (u.GT.UCRt) THEN
               u = u - ECOnd
               accn = ATIl*FSHELL(u,Shcf,GAMma)
               IF (accn.LE.0.0D0) RETURN
               rotemp = RODEFF(A(Nnuc),u,accn,aaj,MOMpar,
     &                         MOMortcrt,HIS(Nnuc),ARGred,
     &                         EXPmax,temp,def2,vibbf12,vibbfdt,vn)
            ELSE
               accn = ACRt
               rotemp = ROBCSF(A(Nnuc),u,aaj,MOMparcrt,MOMortcrt,
     &                  mompar,temp,def2,vibbf12,vibbfdt,vn)*RORed

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


      REAL*8 FUNCTION ROBCSF(A,U,Aj,Mompar,Momort,Momp,T,
     &                       def2,vibbf12,vibbfdt,vn)

      IMPLICIT REAL*8(A - H), REAL*8(O - Z)
C
C COMMON variables
C
      REAL*8 TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl    ! CRIT
      COMMON /CRIT  / TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl
C
C Dummy arguments
C
      REAL*8 A, Aj, Momort, Mompar, U, Momp, T
      REAL*8 vibbf12,vibbfdt,vn
      REAL*8 DSQRT
C
C Local variables
C
      REAL*8 arg, const, det, momo, phi, phi2,dphi2,
     &       qk, qv, s, seff2, vibrk, def2,
     &       ro,ro_u,ro_j,ro_pi
C     REAL*8 qdamp
      REAL*8 rot_K, rot_Q, vib_KQ
C-----CONST=1/(2*SQRT(2 PI))
      DATA const/0.199471D0/

      ROBCSF = 0.D0
      dphi2 = U/UCRt
      phi2 = 1.D0 - dphi2
      phi = DSQRT(phi2)

      t = 2.D0*TCRt*phi/LOG((phi + 1.D0)/(1.D0 - phi))
      s = SCR*TCRt*dphi2/t
      det = DETcrt*dphi2*(1.D0 + phi2)**2
      momp = Mompar*TCRt*dphi2/t
      IF (momp.LT.0.0D0) momp=2.d0 ! RETURN
      momo = Momort*0.3333D0 + 0.6666D0*Momort*TCRt*dphi2/t
      IF (momo.LT.0.0D0) RETURN
      seff2 = momp*t
      IF (ABS(def2).GT.0.005D0) seff2 = momp**0.333D0*momo**0.6666D0*t
      IF (seff2.LE.0.0D0) RETURN
      arg = s - (Aj + 0.5D0)**2/(2.D0*seff2)
      IF (arg.LE.0.0D0) RETURN

      ro_u=exp(s)/sqrt(det)
      ro_j=const*(2.d0*Aj + 1.d0)/seff2**1.5*
     &       EXP(-(Aj+0.5)**2/(2.d0*seff2))
      ro_pi=0.5
      ro = ro_u * ro_j * ro_pi
      CALL DAMPROTVIB(U,qk,T,qv,A,vibrk,def2,vibbf12,vibbfdt,vn)
      IF (qv.GE.0.999D0) vibrk = 1.0
      rot_K  = momo*t
      rot_Q  = 1.0 - qk*(1.0 - 1.0/(momo*T))
      vib_KQ = qv - vibrk*(qv - 1.)
      ROBCSF = ro *  rot_K * rot_Q * vib_KQ

      RETURN
      END
C
C
      REAL*8 FUNCTION RODEFF(A,E,ac,Aj,Mompar,Momort,Ss,
     &                       Argred,Expmax,T,def2,vibbf12,vibbfdt,vn)

      IMPLICIT REAL*8(A - H), REAL*8(O - Z)
C Dummy arguments
      REAL*8 A, Ac, Aj, Argred, E, Expmax, Momort, Mompar, Ss,
     &       T
      REAL*8 vibbf12,vibbfdt,vn
C Local variables
      REAL*8 s,det,seff2, ro,ro_u,ro_j,ro_pi,pi
      REAL*8 ak, arg, const, qk, qv, seff, sort2, con,
     &                 sum, u, vibrk
      REAL*8 rot_K, rot_Q, vib_KQ
      INTEGER i, k, kmin

      DATA const/0.01473144/,pi/3.14159259d0/
C-----CONST=1.0/(24.0*SQRT(2.0))/2.0

      expmax=700.
      IF(ac.LE.0. .or. e.le.0.d0) RETURN
      RODEFF = 0.D0

      T = DSQRT(E/Ac)
      sort2 = Momort*t
      seff2 = mompar**0.333D0*momort**0.6666D0*t
c      GOTO 10
c-----GSM
      S = 2.* Ac * T
      DET = 45.84 * Ac**3 * T**5
      ro_u=exp(s)/sqrt(det)
      ro_j=(1.d0/(2.d0*sqrt(2.d0*pi)))*(2.d0*Aj + 1.d0)/seff2 **1.5*
     &       EXP(-(aj+0.5)**2/(2.d0*seff2))
      ro_pi=0.5
      ro = ro_u * ro_j * ro_pi
      GOTO 100
c-----EGSM
 10   sum = 0.D0
      con = const/Ac**0.25/SQRT(Mompar*t)
      seff = 1.0/Mompar - 1.0/Momort

      IF (Ss.EQ.( - 1.0D0)) THEN
         arg = 2*SQRT(Ac*e) - Argred
         IF (arg.LE.( - Expmax)) THEN
            sum = 0.0
         ELSEIF (e.GT.1.0D0) THEN
            sum = EXP(arg)/e**1.25
         ELSE
            sum = EXP(arg)
         ENDIF
         IF (Aj.LT.1.0D0) GOTO 100
      ENDIF
      i = Aj + 1.
      IF (Ss.EQ.( - 1.0D0)) THEN
         kmin = 2
      ELSE
         kmin = 1
      ENDIF

      DO k = kmin, i
         ak = k + Ss
C--------rotation perpendicular to the symmetry axis
         u = e - 0.5*ak**2*seff
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
      ro = con * sum

100   CALL DAMPROTVIB(e,qk,T,qv,A,vibrk,def2,vibbf12,vibbfdt,vn)
      IF (qv.GE.0.999D0) vibrk = 1.0
      rot_K  = momort*t
      rot_Q  = 1.0 - qk*(1.0 - 1.0/sort2)
      vib_KQ = qv - vibrk*(qv - 1.)
      RODEFF = ro * rot_K * rot_Q * vib_KQ

      RETURN
      END

C
      SUBROUTINE DAMPROTVIB(E1,Qk,T,Q,A,Vibrk,def2,thalf,dt,vn)
C
C Dummy arguments
C
      REAL*8 A, E1, Q, Qk, T, Vibrk,def2,vn
C
C Local variables
C
      REAL*8 arg, cost, dmpdiff, dmphalf, dt, ht, m0, pi, r0,
     &                 sdrop, thalf

C-----Qrot
      dmphalf = 120.d0*A**0.333*def2**2         !according to RIPL-2
      dmpdiff =1400.*A**(-0.666)*def2**2
      Qk = 1./(1. + EXP((-dmphalf/dmpdiff)))
     &     - 1./(1. + EXP((E1-dmphalf)/dmpdiff))
C-----Qvib
      arg = (T - thalf)/dt
      Q = 1.0/(EXP((-arg)) + 1.0)
C-----Kvib
      DATA m0, pi, r0, ht/1.044, 3.141592, 1.26, 6.589/
      sdrop = 17./(4.*pi*r0**2)
      cost = 3.*m0*A/(4.*pi*ht**2*sdrop)
      Vibrk = vn* EXP(1.7*cost**(2./3.)*T**(4./3.))
      END



      SUBROUTINE ROBCS_FG_FIS(A,E,Aj,T,shcf,gamma,momp,def2,rotemp)

      IMPLICIT REAL*8(A - H), REAL*8(O - Z)
C
C COMMON variables
      REAL*8 ACR, ACRt, ATIl, DETcrt, ECOnd, SCR, TCRt,
     &                 UCRt
      COMMON /CRIT  / TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl
C Dummy arguments
      REAL*8 A, Aj, Momort, Mompar, U, Momp, T,rotemp,gamma,
     &                 shcf,def2
C Local variables
      REAL*8 arg, const, det, momo, phi, phi2,dphi2,
     &                 vibdamp, robcs, s, seff2,
     &                 seff2ort,mm2,qr,ac,e,rotdamp
C     REAL*8 vibbf12, vibbfdt, vibrk, qv
      REAL*8 om2,om3,cga,q2,q3
      REAL*8 FSHELL
      INTEGER fg
C-----CONST=1/(2*SQRT(2 PI))

      DATA const/0.199471D0/

      rotemp = 0.d0
      u=e
      mm2=.24*A**.66666
      Mompar = 0.608 * ACRt * mm2 * (1.- 0.6667 * def2)
      Momort = 0.608 * ACRt * mm2 * (1.+ 0.3330 * def2)
c
      IF(U.LE.UCRt) then
c-------BCS
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
c-------FG
        U=U-ECOnd
        fg=1
        ac=atil*FSHELL(u, shcf,gamma)
        IF(ac.LE.0. .or. U.le.0.d0) RETURN
        T=DSQRT(U/ac)
        S = 2.* Ac * T
        DET = 45.84 * Ac**3 * T**5
        momp = 0.608 * ac * mm2 * (1.- 0.6667 * def2)
        momo = 0.608 * ac * mm2 * (1.+ 0.3330 * def2)
      ENDIF
      seff2 = momp*t
c
      IF (ABS(def2).GT.0.005D0) seff2 = momp**0.333D0*momo**0.6666D0*t
      IF (seff2.LE.0.0D0) RETURN
      seff2ort = momo*t

C     Rotational enhancement JpT; damping as in EMPIRE
c      CALL DAMPROTVIB(u,Qr,T,Qv,A,Vibrk,def2,vibbf12,vibbfdt)
c      rotdamp = seff2ort*(1.0 - qr*(1.0 - 1.0/seff2ort))
c      vibdamp = qv - vibrk*(qv - 1.)
c      goto 100

C     Rotational enhancement and damping as in RIPL-2 (Ignatyuk prescription)
      om2=30./A**.66666
      om3=50./A**.66666

      CGA=.0075*A**.33333
      CALL DAMPVIB(T,OM2,CGA,5,Q2)
      CALL DAMPVIB(T,OM3,CGA,7,Q3)
      CALL QROT(A,def2,seff2ort,U,QR)
      rotdamp=Qr
      vibdamp=Q2*Q3
 100  arg = s - (Aj + 0.5D0)**2/(2.D0*seff2)
      IF (arg.LE.0.0D0) RETURN
      robcs = 0.5*const*(2.d0*Aj + 1.d0)*EXP(arg)/SQRT(seff2**3*det)
      rotemp=robcs * rotdamp * vibdamp
      RETURN
      END

      SUBROUTINE QROT(A,BET,SIG4,U,QR)
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

      SUBROUTINE DAMPVIB(T,OM,CGA,LAM,Q)
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
Cccc  * atil = ap1*A(Nnuc) + gamma (gam/A(nnuc)**1/3)                    *
Cccc  *                                                                  *
Cccc  * Using liquid drop vibrational enhancement factor (EMPIRE-2.19)   *
Cccc  *                                                                  *
Cccc  * EGSM level density systematics fitted to Do from RIPL-3.         *
Cccc  *                                                                  *
Cccc  * author: M. Herman                                                *
Cccc  * date:   December 2008                                            *
Cccc  * rev 1 : R. Capote                                                *
Cccc  * date:   January 2011                                             *
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
C	frms  = 1.687
C	Chi-2 =	34.6
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
      gamma = gam/A(Nnuc)**0.333333
      IF(ATIlnoz(INT(Z(nnuc))) .eq. 0.d0) return
      ap1 = ap1*ATIlnoz(INT(Z(nnuc))) !apply elemental normalization factor
      ap2 = ap2*ATIlnoz(INT(Z(nnuc))) !apply elemental normalization factor
c      write(*,*)'atilnoz',ATIlnoz(INT(Z(nnuc)))
      RETURN
      END
