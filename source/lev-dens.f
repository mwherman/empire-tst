Ccc   * $Author: Capote $
Ccc   * $Date: 2005-05-18 14:12:16 $
Ccc   * $Id: lev-dens.f,v 1.39 2005-05-18 14:12:16 Capote Exp $
C
C
      SUBROUTINE ROCOL(Nnuc,Cf,Gcc)
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
CCC   *  GCC  - CONTROLS A-PARAMETER DETERMINATION;                       *
CCC   *         GCC=1 A/ADIV TAKEN                ,                       *
CCC   *         GCC=2 FIT                         ,                       *
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
      DOUBLE PRECISION A2, A23, AP1, AP2, BF, DEL, DELp, GAMma
      INTEGER NLWst
      COMMON /PARAM / AP1, AP2, GAMma, DEL, DELp, BF, A23, A2, NLWst
C
C Dummy arguments
C
      DOUBLE PRECISION Cf, Gcc
      INTEGER Nnuc
C
C Local variables
C
      DOUBLE PRECISION ac, aj, cigor, dumm, momort, mompar, rbmsph,
     &                 rotemp, saimid, saimin, saimx, selmax, stab, u,
     &                 x1, x2, x3
      REAL FLOAT
      INTEGER i, ia, iz, kk
      DOUBLE PRECISION RODEF
      ia = A(Nnuc)
      iz = Z(Nnuc)
      A23 = A(Nnuc)**0.666667
C-----next call prepares for lev. dens. calculations
      CALL PRERO(Nnuc,Cf)
      BF = 1.0
      IF (Cf.NE.0.0D0) BF = 0.0
      DEL = ROPar(3,Nnuc)
      rbmsph = 0.01448*A(Nnuc)**1.66667
      ac = A(Nnuc)/ADIv
      IF (Gcc.EQ.2.D0) THEN
         CALL ALIT(iz,ia,x1,x2,x3,dumm,Gcc)
C--------check whether a-parameter determined from the shell-model s.p.s. exists
         IF (x1 + x2 + x3.EQ.0.0D0) THEN
            WRITE (6,*) ' '
            WRITE (6,*) ' !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
            WRITE (6,*) ' NO FIT DATA FOR LEVEL DENSITIES IN NUCLEUS'
            WRITE (6,*) ' A=', A(Nnuc), '  Z=', Z(Nnuc)
            WRITE (6,*) ' EXECUTION TERMINATED !!'
            WRITE (6,*) ' !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
            WRITE (6,*) ' '
            STOP
         ENDIF
      ENDIF
      DO kk = 1, NEX(Nnuc)
         u = EX(kk,Nnuc) - DEL
         UEXcit(kk,Nnuc) = MAX(u,0.D0)
         IF (Gcc.EQ.2.D0) THEN
C-----------a-parameter determined from the fit to the shell-model s.p.s.
            IF (BF.EQ.0.D0) THEN
C--------------saddle point
               ac = x1 + x2*EXP(( - 200.*x3))
            ELSE
C--------------normal states
               ac = x1 + x2*EXP(( - u*x3))
            ENDIF
         ENDIF
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
                  IF (u.LE.0.0D0) GOTO 100
                  IF (Z(Nnuc).LT.102.D0 .AND. Z(Nnuc).GE.19.D0) THEN
C--------------------next call is to calculate deformation parameter A2 only
                     CALL SIGMAK(A(Nnuc),Z(Nnuc),DEF(1,Nnuc),0.0D0,u,ac,
     &                           aj,mompar,momort,A2,stab,cigor,DEFpar,
     &                           DEFga,DEFgw,DEFgp)
                     CALL MOMFIT(iz,ia,i - 1,saimin,saimid,saimx,selmax)
                     mompar = saimin*rbmsph
                     momort = saimx*rbmsph
                  ELSE
                     CALL SIGMAK(A(Nnuc),Z(Nnuc),DEF(1,Nnuc),1.0D0,u,ac,
     &                           aj,mompar,momort,A2,stab,cigor,DEFpar,
     &                           DEFga,DEFgw,DEFgp)
                  ENDIF
               ENDIF
               IF (u.LE.0.0D0) GOTO 100
C--------------normal states
               IF (BF.NE.0.0D0) THEN
C-----------------inertia moments by Karwowski with spin dependence
C-----------------(spin dependent deformation beta calculated according to B.-Mot.)
                  CALL SIGMAK(A(Nnuc),Z(Nnuc),DEF(1,Nnuc),1.0D0,u,ac,aj,
     &                        mompar,momort,A2,stab,cigor,DEFpar,DEFga,
     &                        DEFgw,DEFgp)
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
                  RO(kk,i,Nnuc) = rotemp
               ELSE
                  ROF(kk,i,Nnuc) = rotemp
               ENDIF
            ENDDO
         ENDIF
  100 ENDDO
      END


      DOUBLE PRECISION FUNCTION RODEF(A,E,Ac,Aj,Mompar,Momort,Yrast,Ss,
     &                                Bf,Argred,Expmax)
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
      DOUBLE PRECISION A, Ac, Aj, Argred, Bf, E, Expmax, Momort, Mompar,
     &                 Ss, Yrast
C
C Local variables
C
      DOUBLE PRECISION ak, arg, con, const, e1, qk, qv, seff, sort2,
     &                 sum, t, u, vibrk
      INTEGER i, k, kmin
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
         WRITE (6,*) 'WARNING: Negative moment of inertia for spin ', Aj
         WRITE (6,*) 'WARNING: 0 level density returned by rodef'
         RETURN
      ENDIF
      IF (Ac.EQ.0.0D0) THEN
         WRITE (6,'('' FATAL: LEVEL DENS. PARAMETER a=0 IN RODEF'')')
         STOP
      ENDIF
      seff = 1.0/Mompar - 1.0/Momort
      IF (Bf.EQ.0.0D0) THEN
         e1 = E
      ELSE
         e1 = E - Yrast
         IF (e1.LE.0.0D0) RETURN
      ENDIF
      t = SQRT(e1/Ac)
      con = const/Ac**0.25/SQRT(Mompar*t)
C-----vibrational enhancement factor
C     VIBRK=EXP(4.7957*A**(2./3.)*T**(4./3.)/100.)
      CALL VIBR(A,t,vibrk)
C-----damping of vibrational effects
      CALL DAMPV(t,qv)
      IF (qv.GE.0.999D0) vibrk = 1.0
C-----damping of Rastopchin (NOT consistent with the builtin systematics)
C     call damp(a,a2,u,qr)
C     qk=1.0-qr
C-----damping of rotational  effects by Karwowski (slow and fast)
C-----(NOT consistent with the builtin systematics)
C     CALL DAMPKS(A,A2,T,QK)
C-----damping of rotational  effects with Fermi function independent
C-----of deformation and mass number (consistent with the builtin systematics)
      CALL DAMPROT(e1,qk)
C-----damping ***** done *********
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
C-----------rotation perpendicular to the symmetry axis
            u = e1 - 0.5*ak**2*seff
         ELSE
C-----------rotation parallel to the symmetry axis
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
c      if(a.eq.233)write(6,*)'vibn', vibrk,qv,(qv - vibrk*(qv - 1.))
      END


      SUBROUTINE DAMPROT(E1,Qk)
CCCC  *****************************************************************
CCCC  * damping of rotational  effects with Fermi function independent
CCCC  * of deformation and mass number (consistent with the builtin systematics)
CCCC  *****************************************************************
C
C Dummy arguments
C
      DOUBLE PRECISION E1, Qk
C
C Local variables
C
      DOUBLE PRECISION dmpdiff, dmphalf
      Qk = 0.
      dmphalf = 40.
      dmpdiff = 10.
      Qk = 1./(1. + EXP((-dmphalf/dmpdiff)))
     &     - 1./(1. + EXP((E1-dmphalf)/dmpdiff))
      END


      SUBROUTINE VIBR(A,T,Vibrk)
CCCC  *****************************************************************
CCCC  *  Calculates vibrational enhancement of level densities
CCCC  *****************************************************************
C
C Dummy arguments
C
      DOUBLE PRECISION A, T, Vibrk
C
C Local variables
C
      DOUBLE PRECISION cost, ht, m0, pi, r0, sdrop
      DATA m0, pi, r0, ht/1.044, 3.141592, 1.26, 6.589/
      sdrop = 17./(4.*pi*r0**2)
      cost = 3.*m0*A/(4.*pi*ht**2*sdrop)
      Vibrk = EXP(1.7*cost**(2./3.)*T**(4./3.))
      END


      SUBROUTINE SIGMAK(A,Z,B,Bf,E,Ac,Aj,Mompar,Momort,A2,Stab,Cigor,
     &                  Defpar,Defga,Defgw,Defgp)
Cccc  *****************************************************************
Cccc  *  Paralel and orthogonal spin cut-off paprameters calculated
Cccc  *  following Vigdor and Karwowski (Phys.Rev.C26(1982)1068)
Cccc  *  Calculates also def. parameter alpha2 (leg. pol. expansion)
Cccc  *  in function of spin in terms of the ldm + dampped g.s. defor.
Cccc  *****************************************************************
C
C Dummy arguments
C
      DOUBLE PRECISION A, A2, Ac, Aj, B, Bf, Cigor, Defga, Defgp, Defgw,
     &                 Defpar, E, Momort, Mompar, Stab, Z
C
C Local variables
C
      DOUBLE PRECISION a2y, a4, arg, betaf, betay, bt, c1, c2, c3, damp,
     &                 dt, eta, gammaf, gammay, gauss, pi, r1f, r1y,
     &                 r2f, r2y, r3f, r3y, rbmsph, rf, t, tgscr, x, y,
     &                 ycrit
      pi = 3.14159
      IF (A.LE.0.D0) RETURN
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
      betaf = 7./6.*SQRT(4*pi/5.)*(1.0 - x)
     &        *SQRT(4. - 15.*y/7./(1 - x)**2)
      arg = 1./SQRT(4. - 15.*y/7./(1 - x)**2)
      IF (arg.GT.1.0D0) arg = 1.0
      gammaf = pi/3. - ACOS(arg)
      r3f = 1. + SQRT(5./4./pi)*betaf*COS(gammaf - 2.*pi)
      r2f = 1. + SQRT(5./4./pi)*betaf*COS(gammaf - 4.*pi/3.)
      r1f = 1. + SQRT(5./4./pi)*betaf*COS(gammaf - 2.*pi/3.)
      Cigor = MAX(MAX(r1f,r2f),r3f)
      rf = r3f/r1f
      IF (ABS((r3f-r1f)/r3f).LT.0.2D0) rf = r2f*2./(r1f + r3f)
      A2 = (rf - 1.0)/(1.0 + 0.5*rf)
C-----yrast states
      IF (Bf.NE.0.0D0) THEN
         gammay = pi/3.
         betay = 7./6.*SQRT(4*pi/5.)*(1.0 - x)
     &           *( - 1. + SQRT(1. + 15.*y/7./(1-x)**2))
         r3y = 1. + SQRT(5./4./pi)*betay*COS(gammay - 2.*pi)
         r2y = 1. + SQRT(5./4./pi)*betay*COS(gammay - 4.*pi/3.)
         r1y = 1. + SQRT(5./4./pi)*betay*COS(gammay - 2.*pi/3.)
         Cigor = MAX(r1y,r2y,r3y)
C        RY=R2Y/R1Y
C-----next line neglects dynamic deformation for yrast states
C        RY=1.
C        A2Y=(RY-1.0)/(1.0+0.5*RY)
C-----adding static deformation damped with energy
         dt = 0.1
         t = SQRT(E/Ac)
         tgscr = 1.0
         damp = 1.0/(1.0 + EXP((t-tgscr)/dt))
         bt = B*damp
         a2y = ( - 1.25*Defpar*y/(1. - x)) + bt
         IF (bt.GT.0.0D0) THEN
            gauss = Defga*EXP(( - (Aj-Defgp)**2/Defgw))
            a2y = a2y + gauss*damp
         ENDIF
         A2 = a2y
         IF (A2.GT.0.6D0) A2 = 0.6
         IF (A2.LT.( - 0.6D0)) A2 = -0.6
      ENDIF
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
      a4 = A2**2*(0.057 + 0.17*x + c2*y) + c3*A2*y
      a4 = a4/(1.0 - 0.37*x - c1*y)
      rbmsph = 0.01448*A**1.66667
      Mompar = (1.0 - A2 + 0.429*A2**2 + 0.268*A2**3 - 0.212*A2**4 -
     &         1.143*A2*a4 + 0.494*A2**2*a4 + 0.266*a4**2)*rbmsph
      Momort = (1 + 0.5*A2 + 1.286*A2**2 + 0.581*A2**3 - 0.451*A2**4 +
     &         0.571*A2*a4 + 1.897*A2**2*a4 + 0.700*a4**2)*rbmsph
      IF (ABS(A2).LE.0.001D0) Momort = Mompar
      END


      SUBROUTINE SIGMA(A,B,E,Ac,Spar,Sort)
Cccc  *****************************************************************
Cccc  *  calculates paralel and orthogonal spin cut-off factors
Cccc  *****************************************************************
C
C Dummy arguments
C
      DOUBLE PRECISION A, Ac, B, E, Sort, Spar
C
C Local variables
C
      DOUBLE PRECISION ht, m0, orti, pi, r0, rm2, sphi, t
      DATA m0, r0, ht, pi/1.044, 1.24, 6.589, 3.141592/
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
      DOUBLE PRECISION DMPc
      COMMON /DAMPAR/ DMPc
C
C Dummy arguments
C
      DOUBLE PRECISION A, B, Q, T
C
C Local variables
C
      DOUBLE PRECISION arg, d, delta, r
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
      DOUBLE PRECISION Q, T
C
C Local variables
C
      DOUBLE PRECISION arg, dt, thalf
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
      DOUBLE PRECISION A, B, Q, U
C
C Local variables
C
      REAL ALOG
      DOUBLE PRECISION de, delta, e, q1, q2, r, u1, u2
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
      DOUBLE PRECISION A2, A23, ACR, ACRt, AP1, AP2, ATIl, BET2, BF,
     &                 DEL, DELp, DETcrt, ECOnd, GAMma, SCR, TCRt, UCRt
      INTEGER NLWst
      COMMON /CRIT  / TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl,
     &                BET2
      COMMON /PARAM / AP1, AP2, GAMma, DEL, DELp, BF, A23, A2, NLWst
C
C Dummy arguments
C
      DOUBLE PRECISION Asaf, Cf
      INTEGER Nnuc
C
C Local variables
C
      DOUBLE PRECISION aj, ar, defit, dshi, dshif, dshift, ellq, exkk,
     &                 pi2, rocumd, rocumu, rocumul, rolev, rolowint,
     &                 rotemp, u, xr
      CHARACTER*6 ctmp
      REAL FLOAT
      DOUBLE PRECISION FSHELL
      INTEGER i, ia, ij, il, in, iter, ix, iz, j, kk, kkl, kku, nplot
      INTEGER INT
      INTEGER*4 iwin
      INTEGER*4 PIPE
      pi2 = PI*PI
      BF = 1.0
      IF (Cf.NE.0.0D0) BF = 0.0D0
      A23 = A(Nnuc)**0.666667
      ia = INT(A(Nnuc))
      iz = INT(Z(Nnuc))
      in = ia - iz
      IF (NEX(Nnuc).LE.0.0D0 .AND. FITlev.EQ.0) THEN
         WRITE (6,
     &'('' EXCITATION ENERGY TABLE FOR A='',I3,'' Z='',I3,         ''
     &HAS NOT BEEN DETERMINED BEFORE CALL OF PRERO'',//
     &,'' LEVEL DENSITIES WILL NOT BE CALCULATED'')') ia, iz
         RETURN
      ENDIF
      IF (EX(NEX(Nnuc),Nnuc).LE.0.0D0 .AND. FITlev.EQ.0) RETURN
      CALL PRERO(Nnuc,Cf)
C-----Empire systematics with Nix-Moeller shell corrections
      AP1 = 0.94431E-01
      AP2 = -0.80140E-01
      GAMma = 0.75594E-01
      IF (Z(Nnuc).GE.85.D0) THEN
         AP1 = AP1*1.2402
         AP2 = AP2*1.2402
         GAMma = GAMma*1.2494
      ENDIF
C--------Empire systematics with M-S shell corrections
      IF (SHNix.EQ.0.0D0) THEN
         AP1 = .52268E-01
         AP2 = .13395E+00
         GAMma = .93955E-01
         IF (Z(Nnuc).GE.85.D0) THEN
            AP1 = AP1*1.2942
            AP2 = AP2*1.2942
            GAMma = GAMma*1.2928
         ENDIF
      ENDIF
      IF (BF.EQ.0.0D0 .AND. Asaf.GE.0.0D0) GAMma = Asaf
C-----determination of the pairing shift DEL
      DELp = 12./SQRT(A(Nnuc))
c      IF(A(nnuc).eq.231.)delp=0.8
      DEL = 0.
      IF (MOD(in,2).NE.0) DEL = DELp
      IF (MOD(iz,2).NE.0) DEL = DEL + DELp
C-----determination of the pairing shift --- done -----
C
C-----set Ignatyuk type energy dependence for 'a'
      ATIl = AP1*A(Nnuc) + AP2*A23
      ATIl = ATIl*ATIlnor(Nnuc)
      TCRt = 0.567*DELp
      IF (Asaf.LT.0.D0) THEN
         ACRt = -ATIl*Asaf
      ELSE
         ar = ATIl*(1.0 + SHC(Nnuc)*GAMma)
         DO ix = 1, 20
            xr = ar*TCRt**2
            ACRt = ATIl*FSHELL(xr,SHC(Nnuc),GAMma)
            IF (ABS(ACRt - ar)/ACRt.LE.0.001D0) GOTO 100
            ar = ACRt
         ENDDO
         WRITE (6,*)
     &     ' WARNING: Search for critical a-parameter has not convergeD'
         WRITE (6,*) ' WARNING: Last iteration has given acrt=', ACRt
         WRITE (6,*) ' WARNING: Execution continues'
      ENDIF
  100 IF (ACRt.LT.0.0D0) ACRt = 0.0
      ECOnd = 1.5*ACRt*DELp**2/pi2
      UCRt = ACRt*TCRt**2 + ECOnd
C-----45.84 stands for (12/SQRT(pi))**2
      DETcrt = 45.84*ACRt**3*TCRt**5
      ACR = ATIl*FSHELL(UCRt,SHC(Nnuc),GAMma)
      IF (BF.EQ.0.D0 .AND. Asaf.LT.0.0D0) ACR = ACRt
      SCR = 2.*ACRt*TCRt
C-----
C-----fit level densities to discrete levels applying energy shift
C-----which will linearly go to 0 at neutron binding energy
C-----
      IF (FITlev.GT.0 .AND. RORed.EQ.0) THEN
         WRITE (6,*) ' '
         WRITE (6,*) ' CAN NOT FIT DISCRETE LEVELS SINCE RORed IS 0'
         WRITE (6,*) ' CHECK WHETHER YOU CAN INCREASE EXPmax in input.f'
         WRITE (6,*) ' (MAXIMUM ARGUMENT OF THE EXPONENT ALLOWED)'
         WRITE (6,*)
     &             ' IF YOUR SYSTEM ALLOWS FOR THIS DO IT AND RECOMPILE'
         WRITE (6,*) ' OTHERWISE YOU CAN NOT ASK FOR SUCH A HIGH ENERGY'
         WRITE (6,*) ' HAVE NO CLUE WHAT TO DO IN SUCH A CASE'
         WRITE (6,*) ' FOR THE TIME BEING EXECUTION TERMINATED'
         STOP
      ENDIF
      IF (Q(1,Nnuc).EQ.0.0D0) THEN
         REWIND 25
         CALL BNDG(1,Nnuc,Q(1,Nnuc))
      ENDIF
C--------get distance between Qn and the last level
      ellq = Q(1,Nnuc) - ELV(NLV(Nnuc),Nnuc)
      dshift = 0.0
      iter = 0
C--------we are not going to fit discrete levels if there are not more
C--------than three or if max excitation energy is so high that levels
C--------can not be taken into account (RORed=0)
      IF (NLV(Nnuc).GT.3 .AND. RORed.GT.0) THEN
         IF (FITlev.GT.0.0D0) THEN
            WRITE (6,*) ' '
            WRITE (6,*) ' Fitting l.d. to discrete levels'
            WRITE (6,*) NLV(Nnuc), ' levels at ', ELV(NLV(Nnuc),Nnuc),
     &                  ' MeV'
         ENDIF
         defit = (ELV(NLV(Nnuc),Nnuc) + MAX(FITlev,4.0D0))
     &           /FLOAT(NDEX - 1)
         nplot = (ELV(NLV(Nnuc),Nnuc) + FITlev)/defit
  150    rocumul = 1.0
         iter = iter + 1
         kkl = 0
         kku = 0
         DO kk = 1, NDEX
C--------------clean RO matrix
            IF (BF.NE.0.0D0) THEN
               DO i = 1, NDLW
                  RO(kk,i,Nnuc) = 0.0
               ENDDO
            ENDIF
C--------------decrease energy shift above the last level to become 0 at Qn
            exkk = FLOAT(kk - 1)*defit
            IF (exkk.LE.ELV(NLV(Nnuc),Nnuc)) THEN
               dshif = dshift
            ELSEIF (exkk.LT.Q(1,Nnuc) .AND. ellq.NE.0.0D0) THEN
               dshif = dshift*(Q(1,Nnuc) - exkk)/ellq
            ELSE
               dshif = 0.0
            ENDIF
            CALL DAMIRO(kk,Nnuc,dshif,defit,Asaf,rotemp,aj)
            DO ij = 1, NLWst
               IF (kk.GT.1) rocumul = rocumul +
     &                                (RO(kk - 1,ij,Nnuc) + RO(kk,ij,
     &                                Nnuc))*defit/RORed
            ENDDO
            IF (rocumul.LE.FLOAT(NLV(Nnuc))) THEN
               kkl = kk
               rocumd = rocumul
            ELSEIF (kku.EQ.0) THEN
               kku = kk
               rocumu = rocumul
            ENDIF
         ENDDO
         rocumd = LOG(rocumd)
         rocumu = LOG(rocumu)
         rolev = LOG(FLOAT(NLV(Nnuc)))
         dshi = (rolev - rocumd)/(rocumu - rocumd)
         dshi = (kkl - 1 + dshi)*defit
         dshi = dshi - ELV(NLV(Nnuc),Nnuc)
         dshift = dshift + dshi
         IF (FITlev.GT.0.0D0) WRITE (6,'(I3,4X,3G12.5)') iter, dshi,
     &                               dshift
         IF (ABS(dshi).GT.0.01D0) THEN
            IF (iter.LE.20) GOTO 150
         ENDIF
      ENDIF
C--------cumulative plot of levels along with the l.d. formula
      IF (FITlev.GT.0.0D0 .AND. NLV(Nnuc).GT.3 .AND. RORed.GT.0) THEN
         WRITE (6,99005) INT(Z(Nnuc)), SYMb(Nnuc), INT(A(Nnuc)),
     &                   ATIlnor(Nnuc), ATIl, NLV(Nnuc)
99005    FORMAT ('Cumulative plot for ',I3,'-',A2,'-',I3,' norm=',F6.4,
     &           ' atil=',F4.1,' Ncut=',I3)
         OPEN (35,FILE = 'fort.35')
         WRITE (35,*) 'set terminal postscript enhanced color'
         WRITE (35,*) 'set output "|cat >>CUMULPLOT.PS"'
         WRITE (35,99010) INT(Z(Nnuc)), SYMb(Nnuc), INT(A(Nnuc)),
     &                    dshift, UCRt - DEL - dshift, DEF(1,Nnuc),
     &                    ATIl, NLV(Nnuc)
99010    FORMAT ('set title "Cumul. plot for ',I3,'-',A2,'-',I3,
     &           '   U shift = ',F6.3,' Ucrt = ',F5.2,' Def = ',F6.2,
     &           ' atil=',F4.1,' Ncut=',I3,'"')
         WRITE (35,*) 'set logscale y'
         WRITE (35,*) 'set xlabel "Energy (MeV)" 0,0'
         WRITE (35,*) 'set ylabel "Number of levels" 0,0'
         WRITE (35,*)
     &               'plot "fort.34" t "fit" w l ,"fort.36" t "lev" w l'
C        CLOSE (35)
         OPEN (34,FILE = 'fort.34')
         OPEN (36,FILE = 'fort.36')
         WRITE (36,*) '0.0 1.0'
         DO il = 2, NLV(Nnuc)
            WRITE (36,*) ELV(il,Nnuc), FLOAT(il - 1)
            WRITE (36,*) ELV(il,Nnuc), FLOAT(il)
         ENDDO
         rocumul = 1.0
         WRITE (34,*) '0.0  ', rocumul
         DO kk = 2, nplot
C-----------integration over energy. There should be factor 2 because of the
C-----------parity but it cancels with the 1/2 steming from the trapezoid
C-----------integration
            DO ij = 1, NLWst
               rocumul = rocumul + (RO(kk - 1,ij,Nnuc) + RO(kk,ij,Nnuc))
     &                   *defit/RORed
            ENDDO
            WRITE (34,*) defit*FLOAT(kk - 1), rocumul
         ENDDO
         CLOSE (36)
         CLOSE (34)
         IF (IOPsys.EQ.0) THEN
            iwin = PIPE('gnuplot fort.35#')
            CLOSE (35)
         ENDIF
      ENDIF
C--------plotting fit of the levels with low energy formula  ***done***
C
C--------fitting discrete levels ---- done ------
C--------
C--------do loop over excitation energy
C--------
      IF (Q(1,Nnuc).EQ.0.0D0) THEN
         REWIND 25
         CALL BNDG(1,Nnuc,Q(1,Nnuc))
      ENDIF
      ellq = Q(1,Nnuc) - ELV(NLV(Nnuc),Nnuc)
      DO kk = 1, NEX(Nnuc)
C-----------clean RO matrix
         IF (BF.NE.0.0D0) THEN
            DO i = 1, NDLW
               RO(kk,i,Nnuc) = 0.0
            ENDDO
         ENDIF
         IF (FITlev.LE.0.0D0 .OR. EX(kk,Nnuc).GE.ELV(NLV(Nnuc),Nnuc))
     &       THEN
            IF (EX(kk,Nnuc).LE.Q(1,Nnuc) .AND. ellq.NE.0.0D0) THEN
               dshif = dshift*(Q(1,Nnuc) - EX(kk,Nnuc))/ellq
            ELSE
               dshif = 0.0
            ENDIF
            CALL DAMIRO(kk,Nnuc,dshif,0.0D0,Asaf,rotemp,aj)
         ENDIF
      ENDDO
      IF (IOUt.EQ.6 .AND. FITlev.GT.0.0D0 .AND. NEX(Nnuc).GT.1) THEN
C----------plot level density
         WRITE (ctmp,'(I3.3,A1,I2.2)') INT(A(Nnuc)), '_', INT(Z(Nnuc))
         OPEN (38,FILE = 'EMLD'//ctmp//'.DAT')
         DO kk = 1, NEX(Nnuc)
            u = EX(kk,Nnuc)
            rolowint = 0.D0
            DO j = 1, NDLW
               rolowint = rolowint + 2*RO(kk,j,Nnuc)
            ENDDO
            WRITE (38,'(1x,5(e10.3,1x))') u, rolowint*EXP(ARGred),
     &             2*RO(kk,1,Nnuc)*EXP(ARGred), 2*RO(kk,2,Nnuc)
     &             *EXP(ARGred), 2*RO(kk,3,Nnuc)*EXP(ARGred)
         ENDDO
         CLOSE (38)
      ENDIF
      END


      DOUBLE PRECISION FUNCTION FSHELL(X,Xs,Xg)
C
C Dummy arguments
C
      DOUBLE PRECISION X, Xg, Xs
C
C Dummy arguments
C
      IF (X.GT.0.01D0) THEN
         FSHELL = 1.0 + (1.0 - EXP((-Xg*X)))*Xs/X
      ELSE
         FSHELL = 1 + Xg*Xs
      ENDIF
      END


      SUBROUTINE DAMIRO(Kk,Nnuc,Dshif,Destep,Asaf,Rotemp,Aj)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C COMMON variables
C
      DOUBLE PRECISION A2, A23, ACR, ACRt, AP1, AP2, ATIl, BET2, BF,
     &                 DEL, DELp, DETcrt, ECOnd, GAMma, SCR, TCRt, UCRt
      INTEGER NLWst
      COMMON /CRIT  / TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl,
     &                BET2
      COMMON /PARAM / AP1, AP2, GAMma, DEL, DELp, BF, A23, A2, NLWst
C
C Dummy arguments
C
      DOUBLE PRECISION Aj, Asaf, Destep, Dshif, Rotemp
      INTEGER Kk, Nnuc
C
C Local variables
C
      DOUBLE PRECISION ac, accn, ampl, bsq, cigor, momort, mompar, phi,
     &                 qigor, rbmsph, saimid, saimin, saimx, selmax,
     &                 shredt, stab, t, temp, u
      LOGICAL bcs
      REAL FLOAT
      DOUBLE PRECISION FSHELL, ROBCS, RODEF
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
         IF (u.LE.0.0D0) GOTO 99999
         IF (u.GT.UCRt) THEN
            u = u - ECOnd
            IF (u.LE.0.0D0) GOTO 99999
            bcs = .FALSE.
         ELSE
            bcs = .TRUE.
         ENDIF
      ENDIF

C-----
C-----do loop over angular momentum
C
      DO i = 1, NLWst
         Aj = FLOAT(i) + HIS(Nnuc)
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
            IF (u.LE.0.0D0) GOTO 99999
            IF (Z(Nnuc).LT.102.0D0 .AND. Z(Nnuc).GE.19.0D0) THEN
C--------------next line is to calculate deformation parameter A2 only
               CALL SIGMAK(A(Nnuc),Z(Nnuc),DEF(1,Nnuc),0.0D0,u,accn,Aj,
     &                     mompar,momort,A2,stab,cigor,DEFpar,DEFga,
     &                     DEFgw,DEFgp)
               CALL MOMFIT(iz,ia,i - 1,saimin,saimid,saimx,selmax)
               mompar = saimin*rbmsph
               momort = saimx*rbmsph
            ELSE
               CALL SIGMAK(A(Nnuc),Z(Nnuc),DEF(1,Nnuc),0.0D0,u,accn,Aj,
     &                     mompar,momort,A2,stab,cigor,DEFpar,DEFga,
     &                     DEFgw,DEFgp)
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
            IF (ac.LE.0.D0) GOTO 99999
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
     &                  mompar,momort,A2,stab,cigor,DEFpar,DEFga,DEFgw,
     &                  DEFgp)
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
         IF (bcs) THEN
            Rotemp = ROBCS(A(Nnuc),u,Aj,mompar,momort,A2)*RORed
            IF (i.EQ.1) THEN
               phi = SQRT(1.D0 - u/UCRt)
               t = 2.0*TCRt*phi/LOG((phi + 1.D0)/(1.D0 - phi))
            ENDIF
         ELSE
            Rotemp = RODEF(A(Nnuc),u,ac,Aj,mompar,momort,YRAst(i,Nnuc),
     &               HIS(Nnuc),BF,ARGred,EXPmax)
            IF (i.EQ.1) t = SQRT(u/ac)
         ENDIF
         IF (BF.NE.0.0D0) THEN
            RO(Kk,i,Nnuc) = Rotemp
            IF (i.EQ.1) TNUc(Kk,Nnuc) = t
         ELSE
            ROF(Kk,i,Nnuc) = Rotemp
            IF (i.EQ.1) TNUcf(Kk,Nnuc) = t
         ENDIF
      ENDDO
99999 END


      SUBROUTINE PRERO(Nnuc,Cf)
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
      DOUBLE PRECISION A2, A23, AP1, AP2, BF, DEL, DELp, GAMma
      INTEGER NLWst
      COMMON /PARAM / AP1, AP2, GAMma, DEL, DELp, BF, A23, A2, NLWst
C
C Dummy arguments
C
      DOUBLE PRECISION Cf
      INTEGER Nnuc
C
C Local variables
C
      DOUBLE PRECISION ac, aj, arg, beta, cigor, fx, momort, mompar, s,
     &                 sb, sb0, sbnor, segnor, segs, selmax, stab, x,
     &                 x0, x1, xi, xk
      REAL FLOAT
      INTEGER i, ia, iz, j, jstabf, k, kstab, ldstab
      INTEGER INT, MIN0
      DOUBLE PRECISION SHCFADE
C-----check of the input data ---------------------------------------
      ia = INT(A(Nnuc))
      iz = INT(Z(Nnuc))
      IF (NLW.LE.0) THEN
         WRITE (6,
     &'('' MAXIMUM NUMBER OF PARTIAL WAVES HAS NOT BEEN'',             '
     &' DETRMINED BEFORE CALL OF PRERO'',//,                           '
     &' EXECUTION STOPPED'')')
         STOP
      ENDIF
      IF (ia.LE.0 .OR. iz.LE.0) THEN
         WRITE (6,
     &'('' A='',I3,'' AND/OR Z='',I2,                                ''
     & HAS NOT BEEN DETERMINED BEFORE CALL OF PRERO'',               //,
     &'' EXECUTION STOPPED'')') ia, iz
         STOP
      ENDIF
      IF (Nnuc.GT.NDNUC) THEN
         WRITE (6,
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
     &            momort,beta,stab,cigor,DEFpar,DEFga,DEFgw,DEFgp)
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
            WRITE (6,
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
     &                  aj,mompar,momort,beta,stab,cigor,DEFpar,DEFga,
     &                  DEFgw,DEFgp)
            IF (Cf.EQ.0.0D0) DEF(j,Nnuc) = beta
            IF (iz.GT.19 .AND. iz.LT.102) THEN
               sb = 0.0
               IF (j - 1.LE.ldstab)
     &             CALL BARFIT(iz,ia,j - 1,sb,segs,selmax)
               IF (j - 1.EQ.ldstab)
     &             segnor = segs/(aj*(aj + 1)/(2.0*momort))
               IF (j - 1.GT.ldstab) segs = aj*(aj + 1)/(2.0*momort)
     &             *segnor
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
      NLWst = MIN0(JSTab(Nnuc),NLWst)
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
               RO(i,k,Nnuc) = 0.0
            ELSE
               ROF(i,k,Nnuc) = 0.0
            ENDIF
         ENDDO
      ENDDO
C-----setting to 0 level density array ------ done ------
      END


      DOUBLE PRECISION FUNCTION ROBCS(A,U,Aj,Mompar,Momort,A2)
CCC   ********************************************************************
CCC   *                                                         CLASS:APU*
CCC   *                        R O B C S                                 *
CCC   ********************************************************************
C
C COMMON variables
C
      DOUBLE PRECISION ACR, ACRt, ATIl, BET2, DETcrt, ECOnd, SCR, TCRt,
     &                 UCRt
      COMMON /CRIT  / TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl,
     &                BET2
C
C Dummy arguments
C
      DOUBLE PRECISION A, A2, Aj, Momort, Mompar, U
C
C Local variables
C
      DOUBLE PRECISION arg, const, det, dphi2, momo, momp, phi, phi2,
     &                 qdamp, qk, s, seff2, t, vibrk
      DOUBLE PRECISION DSQRT
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
      IF (ABS(A2).GT.0.005D0) seff2 = momp**0.333D0*momo**0.6666D0*t
      IF (seff2.LE.0.0D0) RETURN
      arg = s - (Aj + 0.5D0)**2/(2.D0*seff2)
      IF (arg.LE.0.0D0) RETURN
C     CALL DAMPKS(A, A2, t, qk)
      CALL DAMPROT(U,qk)
      qdamp = 1.D0 - qk*(1.D0 - 1.D0/(momo*t))
      ROBCS = 0.5D0*const*(2*Aj + 1.D0)*EXP(arg)/SQRT(seff2**3*det)
C-----vibrational enhancement factor
      CALL VIBR(A,t,vibrk)
      ROBCS = ROBCS*vibrk*momo*t*qdamp
      END


      DOUBLE PRECISION FUNCTION SHCFADE(J,Shrj,Shrd)
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
      DOUBLE PRECISION Shrd, Shrj
C
C Local variables
C
      REAL FLOAT
      SHCFADE = 1.
      IF (Shrd.NE.0.D0) SHCFADE = 1.0/(1.0 + EXP((FLOAT(J)-Shrj)/Shrd))
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
      DOUBLE PRECISION A2, A23, AP1, AP2, BF, DEL, DELp, GAMma
      INTEGER NLWst
      COMMON /PARAM / AP1, AP2, GAMma, DEL, DELp, BF, A23, A2, NLWst
C
C Dummy arguments
C
      INTEGER Nnuc
      DOUBLE PRECISION Scutf
C
C Local variables
C
      DOUBLE PRECISION am, amas, arg, atil, cf, e, efort, enorm, eo,
     &                 eom, exl, rhou, rjj, rolowint, sigh, sigl, t, tm,
     &                 u, ux, xj
      CHARACTER*6 ctmp
      DOUBLE PRECISION DEXP
      REAL FLOAT
      INTEGER i, ig, igna, il, iter, j
      INTEGER INT
      INTEGER*4 iwin
      INTEGER*4 PIPE
      eom = 0.0
      cf = 0.0
C-----next call prepares for lev. dens. calculations
      CALL PRERO(Nnuc,cf)
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
         WRITE (6,*) 'WARNING: '
         IF (iter.LT.301) THEN
            WRITE (6,*) 'WARNING: Number of iterations in ROGC ',
     &                  iter - 1
            WRITE (6,*) 'WARNING: Can not calculate Ux'
         ELSE
            WRITE (6,*) 'WARNING: Maximum number if iterations in ROGC'
         ENDIF
         WRITE (6,*) 'WARNING: Level density parameters inconsistent'
         WRITE (6,*) 'WARNING: This may happen if you have used default'
         WRITE (6,*) 'WARNING: systematics for too light nucleus or '
         WRITE (6,*)
     &              'WARNING: have allowed for too many discrete levels'
         WRITE (6,*) 'WARNING: entering the region where these are lost'
         WRITE (6,*) 'WARNING: Reanalise GC l.d. parameters for:'
         WRITE (6,*) 'WARNING: Z=', INT(Z(Nnuc)), '  A=', INT(A(Nnuc))
         WRITE (6,*) 'WARNING: a=', am, ' T=', t
         WRITE (6,*) 'WARNING: '
C--------anyhow, plot fit of the levels with the low energy l.d. formula
         IF (FITlev.GE.0.0D0) THEN
            IF (NLV(Nnuc).GT.3) THEN
               WRITE (6,*) ' a=', A(Nnuc), 'Z=', Z(Nnuc)
               WRITE (6,*) ' A=', am, ' UX=', ux, ' T=', tm, ' EO=', eo
               OPEN (35,FILE = 'fort.35')
               WRITE (35,99005) INT(Z(Nnuc)), SYMb(Nnuc), INT(A(Nnuc)),
     &                          NLV(Nnuc)
99005          FORMAT ('set title "NO SOLUTION FOR ',I3,'-',A2,'-',I3,
     &                 ' Ncut=',I3,'"')
               WRITE (35,*) 'set terminal postscript enhanced color'
               WRITE (35,*) 'set output "|cat >>CUMULPLOT.PS"'
               WRITE (35,*) 'set logscale y'
               WRITE (35,*) 'set xlabel "Energy (MeV)" 0,0'
               WRITE (35,*) 'set ylabel "Number of levels" 0,0'
               WRITE (35,*)
     &               'plot "fort.34" t "fit" w l ,"fort.36" t "lev" w l'
               CLOSE (35)
               OPEN (34,FILE = 'fort.34')
               OPEN (36,FILE = 'fort.36')
               WRITE (36,*) '0.0 1.0'
               DO il = 2, NLV(Nnuc)
                  WRITE (36,*) ELV(il,Nnuc), FLOAT(il - 1)
                  WRITE (36,*) ELV(il,Nnuc), FLOAT(il)
C-----------------integration over energy. There should be factor
C-----------------2 because of the parity
                  rolowint = EXP(( - eom/tm))
     &                       *(EXP(ELV(il,Nnuc)/tm) - 1.)
                  WRITE (34,*) ELV(il,Nnuc), rolowint
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
            WRITE (6,*) ' ERROR IN DISCRETE LEVEL FITTING'
            WRITE (6,*) ' EXECUTION STOPPED BECAUSE OF FITLEV<0 OPTION '
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
         rolowint = EXP(( - eo/t))*(DEXP(ELV(NLV(Nnuc),Nnuc)/t) - 1.)
         IF (ABS(rolowint - NLV(Nnuc)).GT.0.5D0) THEN
            tm = t
            t = t + 0.01*LOG(NLV(Nnuc)/DEXP((-eo/t))
     &          /(DEXP(ELV(NLV(Nnuc),Nnuc)/t) - 1))
            ux = 0.0
            eo = 0.0
            iter = iter + 1
            GOTO 100
         ENDIF
      ENDIF
C-----plot fit of the levels with the low energy l.d. formula
      IF (FITlev.GT.0.0D0 .AND. NLV(Nnuc).GT.5) THEN
         WRITE (6,*) ' A=', A(Nnuc), 'Z=', Z(Nnuc), ' Ncut=', NLV(Nnuc)
         WRITE (6,*) ' a=', am, ' Ux=', ux, ' T=', t, ' EO=', eo
         OPEN (35,FILE = 'fort.35')
         WRITE (35,*) 'set terminal postscript enhanced color'
         WRITE (35,*) 'set output "|cat >>CUMULPLOT.PS"'
         WRITE (35,99010) INT(Z(Nnuc)), SYMb(Nnuc), INT(A(Nnuc)), am, t,
     &                    eo, NLV(Nnuc)
99010    FORMAT ('set title "Cumul.plot for ',I3,'-',A2,'-',I3,': a=',
     &           F4.1,' T=',F4.1,' E0=',F4.1,' Ncut=',I3,'"')
         WRITE (35,*) 'set logscale y'
         WRITE (35,*) 'set xlabel "Energy (MeV)" 0,0'
         WRITE (35,*) 'set ylabel "Number of levels" 0,0'
         WRITE (35,*)
     &               'plot "fort.34" t "fit" w l ,"fort.36" t "lev" w l'
         CLOSE (35)
         OPEN (34,FILE = 'fort.34')
         OPEN (36,FILE = 'fort.36')
         WRITE (36,*) '0.0 1.0'
         DO il = 2, NLV(Nnuc)
            WRITE (36,*) ELV(il,Nnuc), FLOAT(il - 1)
            WRITE (36,*) ELV(il,Nnuc), FLOAT(il)
C-----------Integration over energy.
            rolowint = EXP(( - eo/t))*(EXP(ELV(il,Nnuc)/t) - 1.)
            WRITE (34,*) ELV(il,Nnuc), rolowint
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
                     RO(i,j,Nnuc) = 0.5*rhou*rjj
                     IF (RO(i,j,Nnuc).LT.RORed) RO(i,j,Nnuc) = 0.
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
                     RO(i,j,Nnuc) = 0.5*rhou*rjj
                     IF (RO(i,j,Nnuc).LT.RORed) RO(i,j,Nnuc) = 0.
                  ENDIF
               ENDDO
            ENDIF
         ENDDO
      ENDIF
      IF (IOUt.EQ.6. .AND. FITlev.GT.0.0D0 .AND. NEX(Nnuc).GT.1) THEN
C--------plot level density
         WRITE (ctmp,'(I3.3,A1,I2.2)') INT(A(Nnuc)), '_', INT(Z(Nnuc))
         OPEN (38,FILE = 'GCLD'//ctmp//'.DAT')
         DO i = 1, NEX(Nnuc)
            u = EX(i,Nnuc)
            rolowint = 0.D0
            DO j = 1, NLW
               rolowint = rolowint + 2*RO(i,j,Nnuc)
            ENDDO
            WRITE (38,'(1x,5(e10.3,1x))') u, rolowint*EXP(ARGred),
     &             2*RO(i,1,Nnuc)*EXP(ARGred), 2*RO(i,2,Nnuc)
     &             *EXP(ARGred), 2*RO(i,3,Nnuc)*EXP(ARGred)
         ENDDO
         CLOSE (38)
      ENDIF
      ROPar(4,Nnuc) = eo
      ROPar(2,Nnuc) = ux
      END
C
C
      SUBROUTINE ALIT(Iz,Ia,X1,X2,X3,B,Gcc)
C
C-------------------------------------------------------------class:au
C-----reads fit parameters to calculate a-parameter in level densities
C-----and ground state nuclear deformation 'B'
C-----input IZ and IA of the nucleus and level density control variable
C-----GCC.
C
C
C Dummy arguments
C
      DOUBLE PRECISION B, Gcc, X1, X2, X3
      INTEGER Ia, Iz
C
C Local variables
C
      REAL FLOAT
      INTEGER izia, iziar
      REWIND 23
      izia = Iz*1000 + Ia
  100 READ (23,*,END = 200) iziar, B, X1, X2, X3
      IF (izia.EQ.iziar) RETURN
      GOTO 100
  200 IF (Gcc.EQ.2D0) THEN
         WRITE (6,
     &'('' LEVEL DENSITY FIT FOR Z='',I3,'' A='',I3,'' NOT      FOUND.
     &A/8 USED.'')') Iz, Ia
         B = 0.
         X1 = FLOAT(Ia)/8.
         X2 = 0.
         X3 = 0.
      ELSE
         WRITE (6,
     &'('' DEFORMATION FOR Z='',I3,'' A='',I3,'' NOT FOUND.     ASSUMED
     &SPHERICAL.'')') Iz, Ia
         B = 0.
      ENDIF
      END



      SUBROUTINE ROHFBCS(Nnuc)
CCC
CCC   *********************************************************************
CCC   *                                                         CLASS:PPU *
CCC   *                      R O H F B C S                                *
CCC   *                                                                   *
CCC   *  Reads level densities calculated in the frame of the Hartree-    *
CCC   *  Fock-BCS model and stored in the tables (RIPL-2) and interpolates*
CCC   *  them linearily in log to the EMPIRE energy grid.                 *
CCC   *  level densities were generated and provided to RIPL-2 by         *
CCC   *  S. Goriely.                                                      *
CCC   *                                                                   *
CCC   *                                                                   *
CCC   *                                                                   *
CCC   *  INPUT:                                                           *
CCC   *  NNUC - INDEX OF THE NUCLEUS (POSITION IN THE TABLES)             *
CCC   *                                                                   *
CCC   *                                                                   *
CCC   * OUTPUT:NONE                                                       *
CCC   *                                                                   *
CCC   * CALLS:ALIT                                                        *
CCC   *                                                                   *
CCC   *********************************************************************
CCC
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C PARAMETER definitions
C
      INTEGER NLDGRID, JMAX
      PARAMETER (NLDGRID = 55,JMAX = 30)
C
C Dummy arguments
C
      INTEGER Nnuc
C
C Local variables
C
      DOUBLE PRECISION c1, c2, cf, cgrid(0:NLDGRID), hhh, r1, r2,
     &                 rhogrid(0:NLDGRID,JMAX), rhoogrid(0:NLDGRID),
     &                 rhotgrid(0:NLDGRID), tgrid(0:NLDGRID), u,
     &                 uugrid(0:NLDGRID)
      CHARACTER*2 car2
      DOUBLE PRECISION DLOG10
      CHARACTER*56 filename
      INTEGER i, ia, iar, iugrid, iz, izr, j, jmaxl, k, khi, kk, klo
      cf = 0
      ia = A(Nnuc)
      iz = Z(Nnuc)
C-----next call prepares for lev. dens. calculations
      CALL PRERO(Nnuc,cf)
C
C-----initialization
C
      jmaxl = MIN(NDLW,JMAX)
      DO i = 0, NLDGRID
         uugrid(i) = 0.
         tgrid(i) = 0.
         cgrid(i) = 1.
         rhoogrid(i) = 1.E-20
         rhotgrid(i) = 1.E-20
         DO j = 1, jmaxl
            rhogrid(i,j) = 1.E-20
         ENDDO
      ENDDO
      WRITE (filename,99005) iz
99005 FORMAT ('../RIPL-2/densities/total/level-densities-hfbcs/z',i3.3,
     &        '.dat')
      OPEN (UNIT = 34,FILE = filename,ERR = 300)
  100 READ (34,99010,ERR = 100,END = 300) car2, izr, iar
99010 FORMAT (23x,a2,i3,3x,i3)
      IF (car2.NE.'Z=') GOTO 100
      IF (iar.NE.ia .OR. izr.NE.iz) GOTO 100
C
C-----reading microscopic lev. dens. from the RIPL-2 file
C
      READ (34,*,END = 300)
      READ (34,*,END = 300)
      i = 1
  200 READ (34,99015,END = 400) uugrid(i), tgrid(i), cgrid(i),
     &                          rhoogrid(i), rhotgrid(i),
     &                          (rhogrid(i,j),j = 1,jmaxl)
99015 FORMAT (1x,f6.2,f7.3,1x,1p,33E9.2,0p)
      IF (uugrid(i).LE.0.001) GOTO 400
      IF (i.EQ.NLDGRID) GOTO 400
      i = i + 1
      GOTO 200
  300 WRITE (6,*) ' NO LEV. DENS. FOR Z=', iz, ' A=', ia, ' IN HFBSC'
      WRITE (6,*) ' USE OTHER LEVEL DENSITIES. EXECUTION TERMINATED '
      STOP 'HFBCS lev dens. missing'
  400 CLOSE (34)
      iugrid = i - 1
      DO kk = 1, NEX(Nnuc)
         u = EX(kk,Nnuc)
         UEXcit(kk,Nnuc) = u
         IF (u.LT.0.) RETURN
         IF (u.GT.150.0D0) THEN
            WRITE (6,*) ' '
            WRITE (6,*) ' HFBCS LEV. DENS. DEFINED UP TO 150 MeV ONLY'
            WRITE (6,*) ' REQUESTED ENERY IS ', u, ' MeV'
            WRITE (6,*) ' YOU HAVE TO USE ANOTHER LEVEL DENSITIES'
            WRITE (6,*) ' EXECUTION STOPPED'
            STOP 'TOO HIGH ENERGY FOR HFBCS LEV. DENS.'
         ENDIF
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
            r1 = rhogrid(klo,j)
            r2 = rhogrid(khi,j)
            IF (r1.GT.0 .AND. r2.GT.0) THEN
               RO(kk,j,Nnuc) = 10.**(c1*DLOG10(r1) + c2*DLOG10(r2))
            ELSE
               RO(kk,j,Nnuc) = c1*r1 + c2*r2
            ENDIF
            IF (RO(kk,j,Nnuc).LT.0) RO(kk,j,Nnuc) = 0.
         ENDDO
         TNUc(kk,Nnuc) = c1*tgrid(klo) + c2*tgrid(khi)
      ENDDO
      END
C
C
      SUBROUTINE DAMI_ROFIS(Nnuc,Ib,Mmod,Rafis)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C COMMON variables
C
      DOUBLE PRECISION A2, A23, ACR, ACRt, ACRtf(2), AFIsm(NFMOD), AP1,
     &                 AP2, ATIl, BET2, BF, CSFism(NFMOD), DEFbm(NFMOD),
     &                 DEL, DELp, DELtafism(NFMOD), DEStepm(NFMOD),
     &                 DETcrt, DETcrtf(2), ECOnd, ECOndf(2), EFBm(NFMOD)
     &                 , EFDism(NFTRANS,NFMOD), GAMma, GAMmafism(NFMOD),
     &                 HM(NFTRANS,NFMOD), MORtcrt(NFPARAB),
     &                 MPArcrt(NFPARAB), ROFism(0:NFISENMAX,NDLW,NFMOD),
     &                 SCR, SCRtf(2), SHCfism(NFMOD), TCRt, TCRtf(2),
     &                 TDIrect, TDIrm(NFMOD), TFB, TFBm(NFMOD), UCRt,
     &                 UCRtf(2), UGRidf(0:NFISENMAX,NFMOD), WFIsm(NFMOD)
     &                 , XMInnm(NFMOD)
      INTEGER BFFm(NFMOD), NLWst, NRBinfism(NFMOD)
      COMMON /CRIT  / TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl,
     &                BET2
      COMMON /CRITFIS/ ACRtf, UCRtf, TCRtf, DETcrtf, SCRtf, MORtcrt,
     &                 MPArcrt, ECOndf
      COMMON /FISSMOD/ ROFism, HM, EFDism, UGRidf, EFBm, XMInnm, AFIsm,
     &                 DEFbm, SHCfism, DELtafism, GAMmafism, WFIsm,
     &                 BFFm, NRBinfism, DEStepm, TFBm, TDIrm, CSFism,
     &                 TFB, TDIrect
      COMMON /PARAM / AP1, AP2, GAMma, DEL, DELp, BF, A23, A2, NLWst
C
C Dummy arguments
C
      INTEGER Ib, Mmod, Nnuc
      DOUBLE PRECISION Rafis
C
C Local variables
C
      DOUBLE PRECISION aaj, accn, ar, desteppp, excn1, fshell, mm2, r0,
     &                 rotemp, shcf, t, u, xmax, xr
      CHARACTER*50 filename
      REAL FLOAT
      INTEGER ia, iar, ibb, iff, ii, in, ix, iz, izr, j, jj, kk, nr
      INTEGER INT
      DOUBLE PRECISION ROBCSF, RODEFF
      CHARACTER*2 simb
C-----continuum, level densities at saddle points
      excn1 = EMAx(Nnuc)
      BET2 = DEFfis(Ib)
      IF (NRBarc.EQ.3 .AND. Ib.EQ.2) BET2 = DEFeq
C-----where continuum starts,ends,steps in between
      IF (Mmod.EQ.0) THEN
         XMInn(Ib) = 0.0001
         DO nr = 1, NRFdis(Ib)
            IF (EFDis(nr,Ib).GT.XMInn(Ib)) XMInn(Ib) = EFDis(nr,Ib)
         ENDDO
         IF (NRBarc.EQ.3) XMInn(2) = 0.0001
         IF (excn1.LE.(EFB(Ib) + XMInn(Ib))) THEN
            xmax = XMInn(Ib) + 4.D0
         ELSE
            xmax = excn1 - (EFB(Ib) + XMInn(Ib)) + 4.
         ENDIF
         DEStepp(Ib) = (xmax - XMInn(Ib))/100.
         NRBinfis(Ib) = INT((xmax - XMInn(Ib))/DEStepp(Ib))
         IF (NRBinfis(Ib).GT.NFISENMAX) THEN
            WRITE (6,*)
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
         IF (NRBarc.EQ.3) XMInn(2) = 0.0001
                                           !! Should be checked for multimodal
         IF (excn1.LE.(EFBm(Mmod) + XMInnm(Mmod))) THEN
            xmax = XMInn(Mmod) + 4.
         ELSE
            xmax = excn1 - (EFBm(Mmod) + XMInnm(Mmod)) + 4.
         ENDIF
         DEStepm(Mmod) = (xmax - XMInnm(Mmod))/100.
         NRBinfism(Mmod) = INT((xmax - XMInnm(Mmod))/DEStepm(Mmod))
         IF (NRBinfism(Mmod).GT.NFISENMAX) THEN
            WRITE (6,*)
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
C-----FISDEN(Nnuc)=0 reading microscopic lev. dens. from the RIPL-2 file
      IF (FISden(Nnuc).EQ.0.) THEN
         iz = INT(Z(Nnuc))
         ia = INT(A(Nnuc))
         WRITE (filename,99005) iz
99005    FORMAT ('../RIPL-2/fission/fis-levden-hfbcs-inner/z',i3.3,
     &           '.dat')
         OPEN (UNIT = 81,FILE = filename,ERR = 150)
         READ (81,*,END = 150)
   50    READ (81,99010,ERR = 50,END = 150) simb, izr, iar
99010    FORMAT (23x,a2,i3,3x,i3)
         IF (simb.NE.'Z=') GOTO 50
         IF (iar.NE.ia .OR. izr.NE.iz) GOTO 50
         READ (81,*,END = 150)
         READ (81,*,END = 150)
         ii = 1
  100    READ (81,'(f7.2,f7.3,1x,33e9.2)') UGRid(ii,1), t, t, t, t,
     &         (ROFis(ii,j,1),j = 1,NFISJ)
         IF (UGRid(ii,1).LE.0.001) GOTO 200
         IF (ii.EQ.NFISEN) GOTO 200
         ii = ii + 1
         GOTO 100
  150    WRITE (6,*) ' NO LEV. DENS. FOR Z=', iz, ' A=', ia, ' IN HFBSC'
         WRITE (6,*) ' USE OTHER LEVEL DENSITIES. EXECUTION TERMINATED '
         WRITE (6,*)
     &       ' ERROR: HFBCS lev dens. at the inner saddle point missing'
         STOP 'ERROR: HFBCS lev dens. at the inner saddle point missing'
  200    CLOSE (81)
         IF (NRBar.GT.1) THEN
            WRITE (filename,99015) iz
99015       FORMAT ('../RIPL-2/fission/fis-levden-hfbcs-outer/z',i3.3,
     &              '.dat')
            OPEN (UNIT = 82,FILE = filename,ERR = 260)
            READ (82,*,END = 260)
  220       READ (82,99020,ERR = 220,END = 260) simb, izr, iar
99020       FORMAT (23x,a2,i3,3x,i3)
            IF (simb.NE.'Z=') GOTO 220
            IF (iar.NE.ia .OR. izr.NE.iz) GOTO 220
            READ (82,*,END = 260)
            READ (82,*,END = 260)
            ii = 1
  240       READ (82,'(f7.2,f7.3,1x,33e9.2)') UGRid(ii,2), t, t, t, t,
     &            (ROFis(ii,j,2),j = 1,NFISJ)
            IF (UGRid(ii,2).LE.0.001) GOTO 280
            IF (ii.EQ.NFISEN) GOTO 280
            ii = ii + 1
            GOTO 240
  260       WRITE (6,*) ' NO LEV. DENS. FOR Z=', iz, ' A=', ia,
     &                  ' IN HFBSC'
            WRITE (6,*)
     &                ' USE OTHER LEVEL DENSITIES. EXECUTION TERMINATED'
            WRITE (6,*)
     &        'ERROR: HFBCS lev dens. at the outer saddle-point missing'
            STOP
     &        'ERROR: HFBCS lev dens. at the outer saddle-point missing'
  280       CLOSE (82)
         ENDIF
         IF (NFISJ.LT.NLW) THEN
            DO ibb = 1, NRBarc
               DO j = NFISJ + 1, NLW
                  DO ii = 1, NFISEN
                     ROFis(ii,j,ibb) = 0.
                  ENDDO
               ENDDO
            ENDDO
         ENDIF
      ELSEIF (FISden(Nnuc).EQ.1.) THEN
C
C-----Empire specific
C
         mm2 = 0.24*A(Nnuc)**(2./3.)
         r0 = 1.4
         iff = 1
         AP1 = .52268E-01
         AP2 = .13395E+00
         IF (Z(Nnuc).GE.85.D0) THEN
            AP1 = AP1*1.2942
            AP2 = AP2*1.2942
         ENDIF
         ATIl = AP1*A(Nnuc) + AP2*A23
C        atil = 0.0482 * A(Nnuc) + 0.123 * A(Nnuc)**0.666 !Hilaire
         ATIl = ATIl*Rafis
         IF (Mmod.EQ.0) THEN
            GAMma = GAMmafis(Ib)
            DELp = DELtafis(Ib)
            shcf = SHCfis(Ib)
            iff = BFF(Ib)
            desteppp = DEStepp(Ib)
         ELSE ! Mmod.GT.0
            NRBinfis(Ib) = NRBinfism(Mmod)
            XMInn(Ib) = XMInnm(Mmod)
            GAMma = GAMmafism(Mmod)
            DELp = DELtafism(Mmod)
            shcf = SHCfism(Mmod)
            iff = BFFm(Mmod)
            desteppp = DEStepm(Mmod)
         ENDIF
         TCRt = 0.87*0.567*DELp
         ar = ATIl*(1.0 + shcf*GAMma)
         DO ix = 1, 20
            xr = ar*TCRt**2
            IF (xr.GT.0.01D0) THEN
               fshell = 1.0 + (1.0 - EXP((-GAMma*xr)))*shcf/xr
            ELSE
               fshell = 1 + GAMma*shcf
            ENDIF
            ACRt = ATIl*fshell
            IF (ABS(ACRt - ar).LE.0.001D0*ACRt) GOTO 300
            ar = ACRt
         ENDDO
         WRITE (6,*) ' WARNING: Last iteration acrt=', ACRt
         WRITE (6,*) ' WARNING: Execution continues'
  300    IF (ACRt.LT.0.0D0) ACRt = 0.0
         ECOnd = 1.5*ACRt*DELp**2/(PI*PI)
         UCRt = ACRt*TCRt**2 + ECOnd
C--------45.84 stands for (12/SQRT(pi))**2
         DETcrt = (12./SQRT(PI))**2*ACRt**3*TCRt**5
         SCR = 2.*ACRt*TCRt
         MOMparcrt = 6*ACRt*mm2*(1. - (2./3.)*bet2)/PI**2
         IF (MOMparcrt.LT.2.) MOMparcrt = 2.
         MOMortcrt = 0.0095616*r0**2*A(Nnuc)**(5./3.)
     &               *(1. + (1./3.)*bet2)!DEFfis(Ib))
         DEL = 0.
         IF (MOD(in,2).NE.0) DEL = DELp
         IF (MOD(iz,2).NE.0) DEL = DEL + DELp

         DO jj = 1, NLW
            aaj = FLOAT(jj) + HIS(Nnuc)
            DO kk = 1, NRBinfis(Ib)
               u = XMInn(Ib) + (kk - 1)*desteppp + DEL
               IF (u.GT.UCRt) THEN
                  u = u - ECOnd
                  accn = ATIl*(1 + shcf*(1 - EXP((-GAMma*u)))/u)
                  rotemp = RODEFF(A(Nnuc),u,accn,aaj,MOMparcrt,
     &                     MOMortcrt,HIS(Nnuc),ARGred,EXPmax,iff,bet2)
               ELSE
                  accn = ACRt
                  rotemp = ROBCSF(A(Nnuc),u,aaj,MOMparcrt,MOMortcrt,
     &                     iff)*RORed
               ENDIF
               IF (Mmod.EQ.0) ROFis(kk,jj,Ib) = rotemp
               IF (Mmod.GT.0) ROFism(kk,jj,Mmod) = rotemp
            ENDDO
         ENDDO
         ACRtf(Ib) = ACRt
         UCRtf(Ib) = UCRt
         ECOndf(Ib) = ECOnd
         DETcrtf(Ib) = DETcrt
         TCRtf(Ib) = TCRt
         SCRtf(Ib) = SCR
      ENDIF
      END
C
C
      DOUBLE PRECISION FUNCTION ROBCSF(A,U,Aj,Mompar,Momort,Iff)
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C COMMON variables
C
      DOUBLE PRECISION ACR, ACRt, ATIl, BET2, DETcrt, ECOnd, SCR, TCRt,
     &                 UCRt
      COMMON /CRIT  / TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl,
     &                BET2
C
C Dummy arguments
C
      DOUBLE PRECISION A, Aj, Momort, Mompar, U
      INTEGER Iff
C
C Local variables
C
      DOUBLE PRECISION arg, const, det, momo, momp, phi, phi2, pi,
     &                 qdamp, qk, qv, robcs, s, seff2, t, vibrk
C-----CONST=1/(2*SQRT(2 PI))
      DATA const/0.199471/, pi/3.1415926D0/
      robcs = 0.D0
      ROBCSF = 0.D0
      phi2 = 1.D0 - U/UCRt
      phi = SQRT(phi2)
      t = 2.0*TCRt*phi/LOG((phi + 1.0)/(1.0 - phi))
      s = SCR*TCRt*(1. - phi2)/t
      det = DETcrt*(1. - phi2)*(1. + phi2)**2
      momp = Mompar*TCRt*(1. - phi2)/t
      IF (momp.LT.0.0D0) RETURN
      momo = Momort*0.3333 + 0.6666*Momort*TCRt*(1. - phi2)/t
      IF (momo.LT.0.0D0) RETURN
      seff2 = momp*t
      IF (ABS(BET2).GT.0.005D0) seff2 = momp**0.333*momo**0.6666*t
      arg = s - (Aj + 0.5)**2/(2.0*seff2)
      IF (arg.LE.0.0D0) RETURN
      robcs = 0.5*const*(2*Aj + 1.)*EXP(arg)/SQRT(seff2**3*det)
      CALL DAMPROTVIB(U,qk,t,qv,A,vibrk,BET2)
      qdamp = 1.0 - qk*(1.0 - 1.0/(momo*t))
      ROBCSF = robcs*vibrk*momo*t*qdamp
      IF (Iff.EQ.2) ROBCSF = ROBCSF*2.*SQRT(2.*pi)*SQRT(momp*t)
      IF (Iff.EQ.3) ROBCSF = ROBCSF*2.
      IF (Iff.EQ.4) ROBCSF = ROBCSF*4.*SQRT(2.*pi)*SQRT(momp*t)
      END
C
C
      DOUBLE PRECISION FUNCTION RODEFF(A,E,Ac,Aj,Mompar,Momort,Ss,
     &                                 Argred,Expmax,Iff,bet2)
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C Dummy arguments
C
      DOUBLE PRECISION A, Ac, Aj, Argred, E, Expmax, Momort, Mompar, Ss,
     &                 bet2
      INTEGER Iff
C
C Local variables
C
      DOUBLE PRECISION ak, arg, con, const, pi, qk, qv, seff, sort2,
     &                 sum, t, u, vibrk
      INTEGER i, k, kmin
      DATA const/0.01473144/, pi/3.1515926D0/
C-----CONST=1.0/(24.0*SQRT(2.0))/2.0
      RODEFF = 0.D0
      sum = 0.D0
      IF (Mompar.LT.0.0D0 .OR. Momort.LT.0.0D0) THEN
         WRITE (6,*) 'WARNING: Negative moment of inertia for spin ', Aj
         WRITE (6,*) 'WARNING: 0 level density returned by rodef'
         RETURN
      ENDIF
      IF (Ac.EQ.0.0D0) THEN
         WRITE (6,'('' FATAL: LEVEL DENS. PARAMETER a=0 IN RODEFF'')')
         STOP
      ENDIF
      seff = 1.0/Mompar - 1.0/Momort
      t = SQRT(E/Ac)
      con = const/Ac**0.25/SQRT(Mompar*t)
C-----vibrational ehancement, vib+rot damping
      CALL DAMPROTVIB(E,qk,t,qv,A,vibrk,BET2)
      IF (qv.GE.0.999D0) vibrk = 1.0
      sort2 = Momort*t
      IF (Ss.EQ.( - 1.0D0)) THEN
         arg = 2*SQRT(Ac*E) - Argred
         IF (arg.LE.( - Expmax)) THEN
            sum = 0.0
         ELSEIF (E.GT.1.0D0) THEN
            sum = EXP(arg)/E**1.25
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
C-----------rotation perpendicular to the symmetry axis
         u = E - 0.5*ak**2*seff
C-----------rotation parallel to the symmetry axis
C        u = e - 0.5*(Aj*(Aj + 1.) - ak**2)*ABS(seff)
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
  100 RODEFF = con*sum*(1.0 - qk*(1.0 - 1.0/sort2))
     &         *(qv - vibrk*(qv - 1.))
      IF (Iff.EQ.2) RODEFF = RODEFF*2.*SQRT(2.*pi)*SQRT(Mompar*t)
      IF (Iff.EQ.3) RODEFF = RODEFF*2.
      IF (Iff.EQ.4) RODEFF = RODEFF*4.*SQRT(2.*pi)*SQRT(Mompar*t)
      END
C
C
      SUBROUTINE DAMPROTVIB(E1,Qk,T,Q,A,Vibrk,BET2)
C
C Dummy arguments
C
      DOUBLE PRECISION A, E1, Q, Qk, T, Vibrk,BET2
C
C Local variables
C
      DOUBLE PRECISION arg, cost, dmpdiff, dmphalf, dt, ht, m0, pi, r0,
     &                 sdrop, thalf
      Qk = 0.
c      dmphalf = 20.
c      dmpdiff = 7.
      dmphalf = 120.*A**0.333*bet2**2         !according to RIPL-2
      dmpdiff = 1400.*A**(-0.666)*bet2**2
      Qk = 1./(1. + EXP((-dmphalf/dmpdiff)))
     &     - 1./(1. + EXP((E1-dmphalf)/dmpdiff))
      thalf = 1.
      dt = 0.1
      arg = (T - thalf)/dt
      Q = 1.0/(EXP((-arg)) + 1.0)
      DATA m0, pi, r0, ht/1.044, 3.141592, 1.26, 6.589/
      sdrop = 17./(4.*pi*r0**2)
      cost = 3.*m0*A/(4.*pi*ht**2*sdrop)
c      Vibrk = EXP(1.7*cost**(2./3.)*T**(4./3.))
      Vibrk = EXP(0.06*A**(2./3.)*T**(4./3.))
      END
C
C
