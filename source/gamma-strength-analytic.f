Ccc   * $Author: Capote $
Ccc   * $Date: 2004-09-23 18:05:50 $
Ccc   * $Id: gamma-strength-analytic.f,v 1.9 2004-09-23 18:05:50 Capote Exp $
C
      DOUBLE PRECISION FUNCTION GAMMA_STRENGTH(Znucleus, Anucleus,
     &   Eexcitf, Temperf, Egamma, Keyshape)
C
C     Module "gamma-strength-analytic.f" with
C     main subroutine "GAMMA_STRENGTH.f"
C
C     FORTRAN77 code for  calculation of the dipole radiative
C     strength functions for gamma-decay and photoabsorption
C     at fixed both excitation and gamma-ray energies.
C                (author Vladimir  Plujko)
C             (adapted to UNIX by Mike Herman)
C
C
C
C     ******************************************************
C     *       E1 strength function calculations            *
C     *       ---------------------------------            *
C     *                                                    *
C     *   The input parameters appearing as arguments:     *
C     *   --------------------------------------------     *
C     *                                                    *
C     *   Znucleus = atomic number of a nucleus;           *
C     *                                                    *
C     *   Anucleus = mass number of a nucleus;             *
C     *                                                    *
C     *   Eexcitf  = excitation energy;                    *
C     *                                                    *
C     *   Temperf = Temperf(Eexcitf) ---- temperature  at  *
C     *                            given excitation energy;*
C     *                                                    *
C     *   Egamma   = gamma-ray energy;                     *
C     *                                                    *
C     *   Keyshape = key to specify the E1 strength shape. *
C     *                                                    *
C     *   -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --  *
C     *                                                    *
C     * 'Eexcitf  = Eexcit-Egamma' for  gamma  - decay;    *
C     * 'Eexcitf  = Eexcit'        for photoabsorption;    *
C     *  with 'Eexcit' for initial state excitation energy;*
C     *                                                    *
C     *  Keyshape =1 --> fE1=MLO1                          *
C     *  Keyshape =2 --> fE1=MLO2                          *
C     *  Keyshape =3 --> fE1=MLO3                          *
C     *  Keyshape =4 --> fE1=EGLO                          *
C     *  Keyshape =5 --> fE1=GFL                           *
C     *  Keyshape =6 --> fE1=SLO                           *
C     *                                                    *
C     *  -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --   *
C     *                                                    *
C     *   The input parameters transferring by COMMON's:   *
C     *   ---------------------------------------------    *
C     *                                                    *
C     *  COMMON /PARGDR/  EG1, GW1, CS1, EG2, GW2, CS2, NG *
C     *  COMMON /GFLPARAM/ BETagfl2, S2Plusgfl             *
C     *                                                    *
C     *   -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --  *
C     *                                                    *
C     * 'COMMON/PARGDR/EG1,GW1,CS1,EG2,GW2,CS2,NG'contains *
C     *  input for GDR parameters (all deformed nuclei are *
C     *  considered as  axially symmetric spheroids):      *
C     *                                                    *
C     *  EG1= peak energy of the first peak,               *
C     *  GW1= full width of the first peak at half-maximum,*
C     *  CS1= peak cross section of the first peak,        *
C     *  EG2= peak energy of the second peak,              *
C     *  CS2= peak cross section of the second peak,       *
C     *  GW2= full width of the second peak at half-maximum*
C     *  CS2= peak cross section of the second peak,       *
C     *  NG : for NG=1, single peak(spherical nucleus),    *
C     *       for NG=2, double peaks(deformed nucleus);    *
C     *                                                    *
C     * 'COMMON /GFLPARAM/ BETagfl2, S2Plusgfl'  contains  *
C     *  input parameters of the GFL model:                *
C     *                                                    *
C     *  BETagfl2(=beta**2)= square of "deformation"       *
C     *                     parameter 'beta' associated    *
C     *                     with nuclear quadrupole moment,*
C     *  S2Plus(=(E2+)*beta**2)= product of first-excited  *
C     *                          2+ state energy(in MeV)   *
C     *                          deforamation parameter    *
C     *  [see, S.Raman,C.W.Nestor,Jr, P.Tikkanen,          *
C     *   Atom.Data Nucl.Data Tabl. 78(2001)1,             *
C     *   for beta and E2+ values]                         *
C     *                                                    *
C     *   ----------------------------------------------   *
C     *  All other parameters of the E1 strength           *
C     *  calculations are set by default in this           *
C     *  FUNCTION and are placed in the following          *
C     *  COMMON statements:                                *
C     *                                                    *
C     *    COMMON /INPUTKEY/ KEYset,KEYfbc                 *
C     *    COMMON /INPUTPAR/ FACtor, BC, AKS0, DEG, DMEf   *
C     *    COMMON /INPUTGFL/ LMConst                       *
C     *    COMMON /HEGLO / EEGlo0, AK0, ER0                *
C     *                                                    *
C     *  Setting  the  parameters  of  the MLO  approach   *
C     *  is only described in detail below. In particular, *
C     *  it is governed by the key 'KEYset' which can take *
C     *  the value 1,2,3. The value 'KEYset=1' is assigned *
C     *  by default; so, the following  set of  parameters *
C     *  are adopted: KEYfbc=1=>FACtor=1;KEYdeg=2=>k_{s}=  *
C     *  =k_{s}(Egamma) with AKS0=k_{s}0)=0.3;DEG=n_{s}=1. *
C     *                                                    *
C     ******************************************************
C
      IMPLICIT NONE
      DOUBLE PRECISION E1_gsa
      DOUBLE PRECISION Anucleus, Egamma, Znucleus, f0prime, f1prime,
     &                 LMConst, aa, aa3, hh, Temperf, Eexcitf
      DOUBLE PRECISION AKS0, BC, DEG, DMEf, efermi, FACtor, r0, sigmapn
      DOUBLE PRECISION EG0, GW0, ALPha, GWAll, EEGlo0, AK0, ER0,
     &                 alpfree,  rnucl
C    &                 BETagfl2, S2Plusgfl, EG1, GW1, CS1, EG2, GW2, CS2
      INTEGER KEYfbc, KEYset, Keyshape,
     &        keyinput, kz, ka, kzz1, kaa1, keglo
C
      COMMON /INPUTKEY/ KEYset, KEYfbc
      COMMON /INPUTPAR/ FACtor, BC, AKS0, DEG, DMEf
      COMMON /HEGLO / EEGlo0, AK0, ER0
      COMMON /WHELP / EG0, GW0, ALPha, GWAll
      COMMON /INPUTGFL/ LMConst
      COMMON /MLOCOM1/ keyinput,kzz1,kaa1
      COMMON /LOCALGSA/ r0, efermi
      DATA f0prime/1.49D0/, f1prime/ - 0.04D0/
      kz = Znucleus + 0.001
      ka = Anucleus + 0.001
      IF(keyinput.NE.1)THEN
         kzz1 = kz
         kaa1 = ka
         keyinput = 1
C
C        ****************************************************
C        *   Default assignments  and  brief description of *
C        *   some input parameters are given below.         *
C        *   The  parameters  are fixed at first call,      *
C        *   when  'keyinput=0'; if  parameters dependent on*
C        *   nuclear mass and charge they are recalculated  *
C        *   at next call with others Anucleus and Znucleus *
C        ****************************************************
C
         efermi = 37.
         r0 = 1.27
         sigmapn = 5.
         DMEf = 1.
C
         KEYset = 1
C        KEYset = 2
C        KEYset = 3
C        *************************************************************
C        * 'KEYset' is key for choise of the input parameters        *
C        * to calculate the width of the MLO response function.      *
C        * It specifies the input parameters in  the following way:  *
C        *                                                           *
C        *    KEYset= 1 --> FACtor =1 (KEYfbc=1)                     *
C        *                  k_{s}= k_{s}(Egamma)[power approximation]*
C        *                  with AKS0=k_{s}(0)=0.3;   DEG=n_{s}=1.   *
C        *                                                           *
C        *    KEYset= 2 --> BC = 0.7 (KEYfbc=2)                      *
C        *                  k_{s}= k_{s}(Egamma)[power approximation]*
C        *                  with AKS0=k_{s}(0)=0.7;   DEG=n_{s}=3.   *
C        *                                                           *
C        *    KEYset= 3 --> FACtor =0.5 (KEYfbc=1)                   *
C        *                  k_{s}= k_{s}(Egamma)[power approximation]*
C        *                  with AKS0=k_{s}(0)=0.1;   DEG=n_{s}=3.   *
C        *                                                           *
C        *                  (see below, for comments)                *
C        *************************************************************
C
         IF(KEYset.EQ.1)THEN
            KEYfbc = 1
C           *****************************************************************
C           *  'KEYfbc' is  key  to specify calculation  of the two-body    *
C           *   component of the relaxation time of MLO approach.           *
C           *                                                               *
C           *     KEYfbc=1 -> Calculations at given value of the            *
C           *                 in-medium cross section                       *
C           *                 SIGMA_PN(in-medium)=SIGmapn:                  *
C           *                                                               *
C           *                 SIGMA_PN(in-medium)=SIGMA_PN(free)*FACtor     *
C           *                                                               *
C           *     KEYfbc=2 -> Calculations at fixed relative two-body contr-*
C           *                 ibution,BC,to the GDR width in cold nuclei    *
C           *                 [within extended Steinwedel-Jensen(ESJ) model]*
C           *                                                               *
C           *****************************************************************

            FACtor = 1.
C           ************************************************************
C           * 'FACtor' determines (n,p)in-medium cross-section         *
C           * in comparison with free one                              *
C           * [SIGMA_PN(in-medium)=SIGMA_PN(free)*FACtor]              *
C           ************************************************************
            DEG = 1.
C           ***************************************************************
C           *  'DEG =n_{s}' is exponent of the gamma-ray energy dependence*
C           *  of the  one-body contribution  [k_{s}(Egamma)] to the MLO  *
C           *  response function width.                                   *
C           ***************************************************************
            AKS0 = 0.3
C           ****************************************************************
C           * 'AKS0' specifies one-body contribution to the MLO response   *
C           * fuction width at zero gamma-ray energy 'Egamma'              *
C           ****************************************************************
         ELSE
         IF(KEYset.EQ.2)THEN
               KEYfbc = 2
C              *************************************************************
C              * KEYfbc=2 -> Calculations at fixed relative two-body contr-*
C              *             ibution,BC,to the GDR width in cold nuclei    *
C              *             [within extended Steinwedel-Jensen(ESJ) model]*
C              *************************************************************
               BC = 0.7
               DEG = 3.
               AKS0 = 0.7
            ELSE
               KEYfbc = 1
C               KEYfbc = 2
               FACtor = 0.5
C               BC = 0.7
               DEG = 3.
               AKS0 = 0.1
            ENDIF
         ENDIF
C        *****************************************************
C        * Calculations of the 'ALPha' if 'FACtor' is input  *
C        *****************************************************
         IF(KEYfbc.EQ.1)THEN
            alpfree = 23.514/DMEf/sigmapn
            ALPha = alpfree/FACtor
         ENDIF
C        *****************************************************
C        * Calculation of normalization constant 'LMconst'   *
C        * of the GFL model                                  *
C        *****************************************************
         LMConst = SQRT((1.D0 + f1prime/3.D0)/(1.D0 + f0prime))
C
C        *****************************
C        *   'GWAll' is "wall" width *
C        *****************************
         aa = ka
         aa3 = aa**0.3333333
         rnucl = r0*aa3
         GWAll = 6.857764*SQRT(efermi*DMEf)/rnucl
         ER0 = 41.0/aa3
C        ***************************************************
C        * Systematics of GDR parameters in approximation  *
C        * of spherical nucleus                            *
C        ***************************************************
         EG0 = 31.2/aa3 + 20.6/SQRT(aa3)
         GW0 = 0.026*EG0**1.91
C        *******************************
C        * Parameters of EGLO model    *
C        *******************************
         keglo = 1
C        keglo=2
         IF(keglo.GT.1)THEN
            hh = aa - 145
            AK0 = 1.5
            IF(hh.GT.0.)AK0 = 1.5 + 0.131*hh*hh*EXP( - 0.154*hh)
         ELSE
            hh = aa - 148
            AK0 = 1.
            IF(hh.GT.0.)AK0 = 1.0 + 0.09*hh*hh*EXP( - 0.18*hh)
         ENDIF
         EEGlo0 = 4.5
      ENDIF
      IF(kz.NE.kzz1 .OR. ka.NE.kaa1)THEN
C        ***********************************************
C        * Recalculation of the parameters dependent on*
C        * mass and charge of nucleus                  *
C        ***********************************************
         aa = ka
         aa3 = aa**0.3333333
         EG0 = 31.2/aa3 + 20.6/SQRT(aa3)
         GW0 = 0.026*EG0**1.91
         rnucl = r0*aa3
         GWAll = 6.857764*SQRT(efermi*DMEf)/rnucl
         ER0 = 41.0/aa3
         keglo = 1
C        keglo=2
         IF(keglo.GT.1)THEN
            hh = aa - 145
            AK0 = 1.5
            IF(hh.GT.0.)AK0 = 1.5 + 0.131*hh*hh*EXP( - 0.154*hh)
         ELSE
            hh = aa - 148
            AK0 = 1.
            IF(hh.GT.0.)AK0 = 1.0 + 0.09*hh*hh*EXP( - 0.18*hh)
         ENDIF
         kzz1 = kz
         kaa1 = ka
      ENDIF
      GAMMA_STRENGTH = E1_gsa(Egamma, Eexcitf, Temperf, Keyshape)
      END
C
      DOUBLE PRECISION FUNCTION E1_gsa(Egamma, Eexcitf, Temperf,
     &  Keyshape)
C    **************************************************
C    * Calculation of the E1 strength function shape  *
C    **************************************************
      IMPLICIT NONE
      DOUBLE PRECISION AKS0, ALPha, BC, cross,  CS1, CS2, DEG, DMEf,
     &                 EG0, e10, e11, ee, Eexcitf, EG1, EG2,
     &                 Egamma, EGDr, Temperf, ttf, ef
      DOUBLE PRECISION egiant, EGLO, er2, FACtor, gamwidth, GGDr, GW0,
     &                 GW1, GW2, GWAll, pi24, siggam, sigma0
      DOUBLE PRECISION MLO1, MLO2, MLO3, GFL, SLO
      INTEGER KEYfbc, KEYset, Keyshape, NG, nreson
      COMMON /PARGDR/  EG1, GW1, CS1, EG2, GW2, CS2, NG
      COMMON /INPUTKEY/ KEYset, KEYfbc
      COMMON /INPUTPAR/ FACtor, BC, AKS0, DEG, DMEf
      COMMON /WHELP / EG0, GW0, ALPha, GWAll
      COMMON /HELP  / EGDr, GGDr
      DIMENSION egiant(2), cross(2), gamwidth(2)
      DATA pi24/39.47841761D0/
      egiant(1) = EG1
      cross(1) = CS1
      gamwidth(1) = GW1
      egiant(2) = EG2
      cross(2) = CS2
      gamwidth(2) = GW2
      ee = Egamma
      ef = Eexcitf
      ttf = Temperf
      e10 = 0.
      DO nreson = 1, NG
         EGDr = egiant(nreson)
         GGDr = gamwidth(nreson)
         sigma0 = cross(nreson)
         er2 = EGDr*EGDr
         siggam = GGDr*sigma0
C        *************************************************
C        *  Calculations of the 'ALPha' if 'BC' is input *
C        *************************************************
         IF(KEYfbc.NE.1)THEN
            IF(NG.EQ.1)THEN
               ALPha = (DMEf/pi24)*er2/GGDr/BC
            ELSE
               ALPha = (DMEf/pi24)*EG0**2/GW0/BC
            ENDIF
         ENDIF
         IF(Keyshape.EQ.2)THEN
            e11 = siggam*MLO2(ttf, ef, ee)
         ELSEIF(Keyshape.EQ.3)THEN
            e11 = siggam*MLO3(ttf, ee)
         ELSEIF(Keyshape.EQ.4)THEN
            e11 = siggam*EGLO(ttf, ee)
C-----Plujko_new(for some nuclei (some A with Z=62,64-68) EGLO<0 
C      - problem of this model)
            IF(e11.LT.0)e11=0.0
C-----Plujko_new(END for some nuclei (some A with Z=62,64-68) EGLO<0 
C     - problem of this model)
         ELSEIF(Keyshape.EQ.5)THEN
            e11 = siggam*GFL(ttf, ee)
         ELSEIF(Keyshape.EQ.6)THEN
            e11 = siggam*SLO(ee)
         ELSE
            e11 = siggam*MLO1(ttf, ef, ee)
         ENDIF
         e10 = e10 + e11
      ENDDO
Cb oleg???     E1 = 8.674D-08*e10
      E1_gsa = 8.674D-08*e10
C  oleg
      END
C
      DOUBLE PRECISION FUNCTION SLO(Egamma)
      IMPLICIT NONE
      DOUBLE PRECISION Egamma, EGDr, GGDr
      COMMON /HELP  / EGDr, GGDr
      SLO = Egamma*GGDr/((EGDr*EGDr - Egamma*Egamma)**2 + (Egamma*GGDr)
     &      **2)
      END
C
      DOUBLE PRECISION FUNCTION EGLO(T, Egamma)
      IMPLICIT NONE
      DOUBLE PRECISION AK0, ceglo1, ceglo2, egamma2, EEGlo0, Egamma,
     &                 EGDr, ER0, er2, gel, gel0, GGDr, hh, pi24, T, t2
      COMMON /HEGLO / EEGlo0, AK0, ER0
      COMMON /HELP  / EGDr, GGDr
      DATA pi24/39.47841761D0/
      er2 = EGDr*EGDr
      ceglo1 = GGDr/er2
      ceglo2 = 0.7/er2/EGDr
      egamma2 = Egamma**2
      t2 = T**2
      gel = (AK0 + (1. - AK0)*(Egamma - EEGlo0)/(EGDr - EEGlo0))*ceglo1
      gel = gel*(egamma2 + pi24*t2)
      gel0 = (AK0 - (1. - AK0)*EEGlo0/(EGDr - EEGlo0))*ceglo1
      gel0 = gel0*pi24*t2
      hh = Egamma*gel/((er2 - egamma2)**2 + (Egamma*gel)**2)
      EGLO = hh + ceglo2*gel0
      END
C
      DOUBLE PRECISION FUNCTION GFL(T, Egamma)
      IMPLICIT NONE
      DOUBLE PRECISION Egamma, egamma2, gamma, EGDr, GGDr, T, WIDTHGFL,
     &                 LMConst
      COMMON /INPUTGFL/ LMConst
      COMMON /HELP  / EGDr, GGDr
      egamma2 = Egamma**2
      gamma = WIDTHGFL(T, Egamma)
      GFL = LMConst*EGDr*gamma/((EGDr*EGDr - egamma2)
     &      **2 + (Egamma*gamma)**2)
      END
C
      DOUBLE PRECISION FUNCTION WIDTHGFL(T, Egamma)
      IMPLICIT NONE
      DOUBLE PRECISION T, Egamma, EGDr, GGDr, pi24, BETagfl2, const1,
     &                 const2, gdq0, S2Plusgfl, egamma2, gdq
      COMMON /GFLPARAM/ BETagfl2, S2Plusgfl
      COMMON /HELP  / EGDr, GGDr
      DATA pi24/39.47841761D0/, const1/1.05D0/
      egamma2 = Egamma**2
      gdq0 = const1*SQRT(BETagfl2*EGDr**2 + EGDr*S2Plusgfl)
      gdq = const1*SQRT(BETagfl2*egamma2 + Egamma*S2Plusgfl)
      const2 = (GGDr - gdq0)/(EGDr**2)
      WIDTHGFL = const2*(egamma2 + pi24*T**2) + gdq
      END
C
      DOUBLE PRECISION FUNCTION MLO3(T, Egamma)
      IMPLICIT NONE
      DOUBLE PRECISION amlo3, egamma2, Egamma, EGDr, gamma, GGDr, hh,
     &                 phi, T, WIDTH
C
      COMMON /HELP  / EGDr, GGDr
      egamma2 = Egamma**2
      gamma = WIDTH(T, Egamma)
      amlo3 = gamma/((EGDr*EGDr - egamma2)**2 + (Egamma*gamma)**2)
      phi = Egamma
      IF(Egamma.LT.50.*T)THEN
         IF(Egamma.LE.T/50.)THEN
            phi = T
         ELSE
            hh = Egamma/T
            phi = Egamma/(1. - EXP( - hh))
         ENDIF
      ENDIF
      MLO3 = phi*amlo3
      END
C
      DOUBLE PRECISION FUNCTION WIDTH(T, Egamma)
      IMPLICIT NONE
      DOUBLE PRECISION AKSET, ALPha, CS1, CS2, d, EG0, EG1, EG2,
     &                 Egamma, EGDr, g1body, g2body, GGDr, GW0, GW1,
     &                 GW2, GWAll, pi24, T
      INTEGER NG
      COMMON /PARGDR/  EG1, GW1, CS1, EG2, GW2, CS2, NG
      COMMON /WHELP / EG0, GW0, ALPha, GWAll
      COMMON /HELP  / EGDr, GGDr
      DATA pi24/39.47841761D0/
      d = 1.
      IF(NG.NE.1)d = (GGDr - EGDr**2/ALPha/pi24)/AKSET(EGDr)/GWAll
      g1body = AKSET(Egamma)*GWAll*d
      g2body = (Egamma**2 + pi24*T**2)/ALPha/pi24
      WIDTH = g1body + g2body
      END
C
      DOUBLE PRECISION FUNCTION AKSET(Egamma)
      IMPLICIT NONE
      DOUBLE PRECISION AKS0, akse, aksr, ALPha, BC, CS1, CS2, DEG,
     &                 EG0, EG1, EG2, Egamma, EGDr, FACtor, GGDr, GW0,
     &                 GW1, GW2, GWAll
      DOUBLE PRECISION hhh, pi24, DMEf
C     INTEGER KEYfbc, KEYset, NG
      INTEGER NG
      COMMON /PARGDR/  EG1, GW1, CS1, EG2, GW2, CS2, NG
      COMMON /INPUTPAR/ FACtor, BC, AKS0, DEG, DMEf
      COMMON /WHELP / EG0, GW0, ALPha, GWAll
      COMMON /HELP  / EGDr, GGDr
      DATA pi24/39.47841761D0/
      IF(NG.EQ.1)THEN
         aksr = (GGDr - EGDr**2/ALPha/pi24)/GWAll
      ELSE
         aksr = (GW0 - EG0**2/ALPha/pi24)/GWAll
      ENDIF
      IF(ABS(DEG).GE.0.0001)THEN
         IF(NG.EQ.1)THEN
            hhh = (Egamma - EGDr)/EGDr
         ELSE
            hhh = (Egamma - EG0)/EG0
         ENDIF
         IF(hhh.GE.1)THEN
            akse = AKS0
         ELSE
            akse = aksr + (AKS0 - aksr)*(ABS(hhh))**DEG
         ENDIF
         AKSET = akse
         GOTO 99999
      ENDIF
      AKSET = aksr
99999 END
C
      DOUBLE PRECISION FUNCTION MLO1(T, U, Egamma)
      IMPLICIT NONE
      DOUBLE PRECISION Egamma, hh, phi, SPECRALF, T, U
      MLO1 = 0.d0
      If(Egamma.LE.0.d0) return
      MLO1 = SPECRALF(U, Egamma)
C     Underflow and Egamma = 0 protections introduced, RCN, 2004
      phi=1.d0
      hh = Egamma/T
      IF(hh.LE.15.) phi = 1.d0/(1.d0 - EXP( - hh))
      MLO1 = phi*MLO1
      END
C
      DOUBLE PRECISION FUNCTION SPECRALF(U, Egamma)
      IMPLICIT NONE
      DOUBLE PRECISION AK0, alphaphi, EEGlo0, Egamma, EGDr, egsq, ER0,
     &                 er0sq, er0sq1, ersq, eta, etar0, g, g0, GGDr,
     &                 hhh, hhh1, hhh2, hhh3
      DOUBLE PRECISION hhh4, RATEEXCC, U
      COMMON /HEGLO / EEGlo0, AK0, ER0
      COMMON /HELP  / EGDr, GGDr
      ersq = EGDr**2
      egsq = Egamma**2
      eta = RATEEXCC(U, Egamma)
      etar0 = RATEEXCC(0.D0, EGDr)
      er0sq = ER0**2
      hhh = er0sq + ersq
      er0sq1 = ER0**2
      hhh1 = er0sq1 + ersq
      hhh2 = ersq
      hhh3 = egsq
      hhh4 = eta
      g0 = 2.*etar0*hhh/((ersq - er0sq)**2 + 4.*ersq*etar0**2)
      alphaphi = GGDr/g0
      g = 2.*hhh4*alphaphi*hhh1/((hhh2 - er0sq1)**2 + 4.*hhh3*hhh4**2)
      SPECRALF = Egamma*g/((ersq - egsq)**2 + egsq*g**2)
      END
C
      DOUBLE PRECISION FUNCTION RATEKINC(T, Egamma)
      IMPLICIT NONE
      DOUBLE PRECISION EG0, GW0, ALPha, Egamma, GWAll, pi24, T
      COMMON /WHELP / EG0, GW0, ALPha, GWAll
      DATA pi24/39.47841761D0/
      RATEKINC = (Egamma**2 + pi24*T**2)/ALPha/pi24
      END
C
      DOUBLE PRECISION FUNCTION RATEEXCC(U, Egamma)
      IMPLICIT NONE
      DOUBLE PRECISION ALPha, alphae, Egamma, EGDr, ei, GGDr,
     &                 GWAll, pi24, U, EG0, GW0
      COMMON /WHELP / EG0, GW0, ALPha, GWAll
      COMMON /HELP  / EGDr, GGDr
      DATA pi24/39.47841761D0/
      IF(U.LT.0.)U = 0.
      ei = Egamma + U
      alphae = ALPha/EGDr
      RATEEXCC = ei/alphae/pi24
      END
C
      DOUBLE PRECISION FUNCTION MLO2(T, U, Egamma)
      IMPLICIT NONE
      DOUBLE PRECISION egamma2, Egamma, EGDr, gamma, GGDr, hh, phi, T,
     &                 tpa0, U, WIDTHEXC
      COMMON /HELP  / EGDr, GGDr
      egamma2 = Egamma**2
      gamma = WIDTHEXC(U, Egamma)
      tpa0 = gamma/((EGDr*EGDr - egamma2)**2 + (Egamma*gamma)**2)
      phi = Egamma
      IF(Egamma.LT.50.*T)THEN
         IF(Egamma.LE.T/50.)THEN
            phi = T
         ELSE
            hh = Egamma/T
            phi = Egamma/(1. - EXP( - hh))
         ENDIF
      ENDIF
      MLO2 = phi*tpa0
      END
C
      DOUBLE PRECISION FUNCTION WIDTHEXC(U, Egamma)
      IMPLICIT NONE
      DOUBLE PRECISION AKSET, ALPha, CS1, CS2, d, EG0, EG1, EG2,
     &                 Egamma, EGDr, g1body, g2body, GGDr, GW0, GW1,
     &                 GW2, GWAll, pi24, RATEEXCC
      DOUBLE PRECISION U
      INTEGER NG
      COMMON /PARGDR/  EG1, GW1, CS1, EG2, GW2, CS2, NG
      COMMON /WHELP / EG0, GW0, ALPha, GWAll
      COMMON /HELP  / EGDr, GGDr
      DATA pi24/39.47841761D0/
      d = 1.
      g2body = RATEEXCC(U, Egamma)
      IF(NG.NE.1)d = (GGDr - EGDr**2/ALPha/pi24)/AKSET(EGDr)/GWAll
      g1body = AKSET(Egamma)*GWAll*d
      WIDTHEXC = g1body + g2body
      END

