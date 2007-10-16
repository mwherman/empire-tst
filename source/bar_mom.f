Ccc   * $Author: herman $
Ccc   * $Date: 2007-10-16 14:26:13 $
Ccc   * $Id: bar_mom.f,v 1.10 2007-10-16 14:26:13 herman Exp $
C
      SUBROUTINE BARFIT(Iz,Ia,Il,Bfis,Segs,Selmax)
C
C    THIS SUBROUTINE RETURNS THE BARRIER HEIGHT BFIS, THE GROUND-STATE
C    ENERGY SEGS, IN MEV, AND THE ANGULAR MOMENTUM AT WHICH THE FISSION
C    BARRIER DISAPPEARS,  LMAX,  IN UNITS OF H-BAR,
C    WHEN CALLED WITH INTEGER ARGUMENTS IZ, THE ATOMIC NUMBER,
C    IA, THE ATOMIC MASS NUMBER, AND IL, THE ANGULAR MOMENTUM IN UNITS
C    OF H-BAR, (PLANCK'S CONSTANT DIVIDED BY 2*PI).
C
C         THE FISSION BARRIER FOR  IL = 0  IS CALCULATED FROM A 7TH ORDE
C    FIT IN TWO VARIABLES TO 638 CALCULATED FISSION BARRIERS FOR Z VALUE
C    FROM 20 TO 110.  THESE  638 BARRIERS ARE FIT WITH AN RMS DEVIATION
C    0.10 MEV BY THIS 49-PARAMETER FUNCTION.
C    IF  BARFIT  IS CALLED WITH (IZ,IA) VALUES OUTSIDE THE RANGE OF THE
C    THE BARRIER HEIGHT IS SET TO 0.0, AND A MESSAGE IS
C    PRINTED ON THE DEFAULT OUTPUT FILE.
C
C         FOR IL VALUES NOT EQUAL TO ZERO, THE VALUES OF
C    L AT WHICH THE BARRIER IS  80%  AND  20%  OF THE L=0 VALUE ARE
C    RESPECTIVELY FIT TO 20-PARAMETER FUNCTIONS OF  Z  AND  A, OVER A MO
C    RESTRICTED RANGE OF  A  VALUES, THAN IS THE CASE FOR  L = 0.
C    THE VALUE OF L WHERE THE BARRIER DISAPPEARS, LMAX, FOR 61 NUCLEI,
C    IS FIT TO A 35-PARAMETER FUNCTION OF Z AND A,  WITH THE SAME RANGE
C    Z  AND  A  VALUES AS  L-80  AND  L-20.
C         ONCE AGAIN, IF AN  (IZ,IA) PAIR IS OUTSIDE OF THE RANGE OF
C    VALIDITY OF THE FIT, THE BARRIER VALUE IS SET TO  0.0  AND A MESSAG
C    IS PRINTED.  THESE THREE VALUES  (BFIS(L=0),L-80, AND L-20) AND THE
C    CONSTRAINTS  OF  BFIS = 0 AND  D(BFIS)/DL = 0 AT L = LMAX AND L = 0
C    LEAD TO A FIFTH-ORDER FIT TO BFIS(L) FOR L> L-20.  THE FIRST THREE
C    CONSTRAINTS LEAD TO A THIRD-ORDER FIT FOR THE REGION L < L-20.
C
C         THE GROUND-STATE ENERGIES ARE CALCULATED FROM A 175-PARAMETER
C    FIT IN Z, A, AND L TO 329 GROUND-STATE ENERGIES FOR 36 DIFFERENT
C    Z  AND  A  VALUES.
C    (THE RANGE OF Z AND A IS THE SAME AS FOR L-80, L-20, AND L-MAX)
C
C         THE CALCULATED BARRIERS FROM WHICH THE FITS WERE
C    MADE WERE CALCULATED IN 1983-1985 BY A. J. SIERK OF LOS ALAMOS
C    NATIONAL LABORATORY   GROUP T-9, USING  YUKAWA-PLUS-EXPONENTIAL DOU
C    FOLDED NUCLEAR ENERGY, EXACT COULOUB DIFFUSENESS CORRECTIONS,
C    AND DIFFUSE-MATTER MOMENTS OF INERTIA. THE PARAMETERS OF THE MODEL
C    ARE THOSE DERIVED BY MOLLER AND NIX IN 1979:
C    R-0 = 1.16 FM, AS = 21.13 MEV, KAPPA-S = 2.3  A = 0.68 FM.
C    THE DIFFUSENESS OF THE MATTER AND CHARGE DISTRIBUTIONS USED
C    CORRESPONDS TO A SURFACE DIFFUSENESS PARAMETER (DEFINED BY MYERS)
C    OF 0.99 FM.  THE CALCULATED BARRIERS FOR L = 0 ARE
C    ACCURATE TO A LITTLE LESS THAN 0.1 MEV;  THE OUTPUT FROM THIS
C    SUBROUTINE IS A LITTLE LESS ACCURATE.  WORST ERRORS MAY BE AS LARGE
C    AS 0.5 MEV; CHARACTERISTIC UNCERTAINTY IS IN THE RANGE OF 0.1-0.2
C    MEV.   THE VALUES OF EGS ARE GENERALLY APPROXIMATED TO WITHIN
C    ABOUT 0.1-0.2 MEV;  THE LARGEST DEVIATION IS ABOUT 0.5 MEV,
C    NEAR L-I FOR LIGHT NUCLEI.
C         THE RMS DEVIATION OF LMAX FROM THE 61 INPUT VALUES IS 0.31
C    H-BAR.  THE APPROXIMATE VALUE IS NEARLY ALWAYS WITHIN
C    0.5 H-BAR OF THE CALCULATED ONE.
C
C    BELOW IS A TABLE OF TEST VALUES TO CHECK IMPLEMENTATION
C    OF THE PROGRAM.
C    Z, A,  L    EGND ST  FISS BAR      MOMENTS OF INERTIA     LMAX
C
C   28, 58, 0    0.00     33.14        0.816 3.603 3.608      46.1
C         ,25   21.36     19.50        0.778 3.662 3.662      46.1
C         ,40   49.66      2.97        0.724 3.648 2.650      46.1
C         ,46.1 59.14      0.00        0.746 3.160 3.160      46.1
C   65,153, 0    0.00     28.88        0.621 3.698 3.698      82.3
C         ,50   19.00     16.16        0.615 3.639 3.639      82.3
C         ,80   45.24      0.26        0.616 2.765 2.788      82.3
C         ,82.3 47.04      0.00        0.682 2.231 2.276      82.3
C   93,229, 0    0.00      3.76        0.823 1.747 1.747      68.1
C         ,45    8.21      1.26        0.765 1.578 1.578      68.1
C         ,68.1 17.96      0.00        1.053 1.053 1.236      68.1
C
C    WRITTEN BY A. J. SIERK,  LANL  T-9
C    VERSION 1.0   FEBRUARY, 1984
C    VERSION 1.1   JANUARY, 1985  IMPROVED COEFFICIENTS IN EGS AND LMAX
C    VERSION 1.2   SEPTEMBER, 1985  IMPROVED LMAX, EGS COEFFICIENTS
C
C        COPYRIGHT, 1985,  THE REGENTS OF THE UNIVERSITY OF CALIFORNIA.
C        THIS SOFTWARE WAS PRODUCED UNDER A U. S. GOVERNMENT CONTRACT
C        (W-7405-ENG-36) BY THE LOS ALAMOS NATIONAL LABORATORY, WHICH IS
C        OPERATED BY THE UNIVERSITY OF CALIFORNIA FOR THE U. S. DEPARTME
C        OF ENERGY.  THE U. S. GOVERNMENT IS LICENSED TO USE, REPRODUCE,
C        AND DISTRIBUTE THIS SOFTWARE.  PERMISSION IS GRANTED TO THE PUB
C        TO COPY AND USE THIS SOFTWARE WITHOUT CHARGE, PROVIDED THAT THI
C        NOTICE AND ANY STATEMENT OF AUTHORSHIP ARE REPRODUCED ON ALL
C        COPIES.  NEITHER THE GOVERNMENT NOR THE UNIVERSITY MAKES ANY
C        WARRANTY, EXPRESSED OR IMPLIED, OR ASSUMES ANY LIABILITY
C        OR RESPONSIBILITY FOR THE USE OF THIS SOFTWARE.
C
C    THE FOLLOWING IS NECESSARY FOR 32-BIT MACHINES LIKE DEC VAX, IBM,ET
C
      IMPLICIT DOUBLE PRECISION(a - H), DOUBLE PRECISION(O - z)
C
C
C Dummy arguments
C
      DOUBLE PRECISION Bfis, Segs, Selmax
      INTEGER Ia, Il, Iz
C
C Local variables
C
      DOUBLE PRECISION a, a1, a2, aa, aj, ak, amax, amax2, amin, amin2, 
     &                 bfis0, egs, egs1(5,7), egs2(5,7), egs3(5,7), 
     &                 egs4(5,7), egs5(5,7), egscof(5,7,5), el, el20, 
     &                 el80, ell, elmax, elmcof(5,4), elzcof(7,7), 
     &                 emncof(5,4), emxcof(7,5), pa(7), pl(10), pz(7), 
     &                 q, qa, qb, sel20, sel80, x, y, z, zz
      DOUBLE PRECISION DBLE
      REAL FLOAT
      INTEGER i, j, k, l, m
C
C     DOUBLE PRECISION ELZCOF,PA,PZ,AA,ZZ,EL20,EL80,BFIS0,ELMAX,PL,ELL
C     DOUBLE PRECISION EGS,EGS1,EGS2,EGS3,EGS4,EGS5,EGSCOF
C
C
      EQUIVALENCE (egs1,egscof)
      EQUIVALENCE (egs2,egscof(1,1,2))
      EQUIVALENCE (egs3,egscof(1,1,3))
      EQUIVALENCE (egs4,egscof(1,1,4))
      EQUIVALENCE (egs5,egscof(1,1,5))
      DATA emncof/ - 9.01100E+2, -1.40818E+3, 2.77000E+3, -7.06695E+2, 
     &     8.89867E+2, 1.35355E+4, -2.03847E+4, 1.09384E+4, -4.86297E+3,
     &     -6.18603E+2, -3.26367E+3, 1.62447E+3, 1.36856E+3, 1.31731E+3,
     &     1.53372E+2, 7.48863E+3, -1.21581E+4, 5.50281E+3, -1.33630E+3,
     &     5.05367E-02/
      DATA elmcof/1.84542E+3, -5.64002E+3, 5.66730E+3, -3.15150E+3, 
     &     9.54160E+2, -2.24577E+3, 8.56133E+3, -9.67348E+3, 5.81744E+3,
     &     -1.86997E+3, 2.79772E+3, -8.73073E+3, 9.19706E+3, 
     &     -4.91900E+3, 1.37283E+3, -3.01866E+1, 1.41161E+3, 
     &     -2.85919E+3, 2.13016E+3, -6.49072E+2/
      DATA emxcof/ - 4.10652732E6, 1.00064947E7, -1.09533751E7, 
     &     7.84797252E6, -3.78574926E6, 1.12237945E6, -1.77561170E5, 
     &     1.08763330E7, -2.63758245E7, 2.85472400E7, -2.01107467E7, 
     &     9.48373641E6, -2.73438528E6, 4.13247256E5, -8.76530903E6, 
     &     2.14250513E7, -2.35799595E7, 1.70161347E7, -8.23738190E6, 
     &     2.42447957E6, -3.65427239E5, 6.30258954E6, -1.52999004E7, 
     &     1.65640200E7, -1.16695776E7, 5.47369153E6, -1.54986342E6, 
     &     2.15409246E5, -1.45539891E6, 3.64961835E6, -4.21267423E6, 
     &     3.24312555E6, -1.67927904E6, 5.23795062E5, -7.66576599E4/
      DATA elzcof/5.11819909E+5, -1.30303186E+6, 1.90119870E+6, 
     &     -1.20628242E+6, 5.68208488E+5, 5.48346483E+4, -2.45883052E+4,
     &     -1.13269453E+6, 2.97764590E+6, -4.54326326E+6, 3.00464870E+6,
     &     -1.44989274E+6, -1.02026610E+5, 6.27959815E+4, 1.37543304E+6,
     &     -3.65808988E+6, 5.47798999E+6, -3.78109283E+6, 1.84131765E+6,
     &     1.53669695E+4, -6.96817834E+4, -8.56559835E+5, 2.48872266E+6,
     &     -4.07349128E+6, 3.12835899E+6, -1.62394090E+6, 1.19797378E+5,
     &     4.25737058E+4, 3.28723311E+5, -1.09892175E+6, 2.03997269E+6, 
     &     -1.77185718E+6, 9.96051545E+5, -1.53305699E+5, 
     &     -1.12982954E+4, 4.15850238E+4, 7.29653408E+4, -4.93776346E+5,
     &     6.01254680E+5, -4.01308292E+5, 9.65968391E+4, -3.49596027E+3,
     &     -1.82751044E+5, 3.91386300E+5, -3.03639248E+5, 1.15782417E+5,
     &     -4.24399280E+3, -6.11477247E+3, 3.66982647E+2/
      DATA egs1/ - 1.781665232E6, -2.849020290E6, 9.546305856E5, 
     &     2.453904278E5, 3.656148926E5, 4.358113622E6, 6.960182192E6, 
     &     -2.381941132E6, -6.262569370E5, -9.026606463E5, 
     &     -4.804291019E6, -7.666333374E6, 2.699742775E6, 7.415602390E5,
     &     1.006008724E6, 3.505397297E6, 5.586825123E6, -2.024820713E6, 
     &     -5.818008462E5, -7.353683218E5, -1.740990985E6, 
     &     -2.759325148E6, 1.036253535E6, 3.035749715E5, 3.606919356E5, 
     &     5.492532874E5, 8.598827288E5, -3.399809581E5, -9.852362945E4,
     &     -1.108872347E5, -9.229576432E4, -1.431344258E5, 
     &     5.896521547E4, 1.772385043E4, 1.845424227E4/
      DATA egs2/4.679351387E6, 7.707630513E6, -2.718115276E6, 
     &     -9.845252314E5, -1.107173456E6, -1.137635233E7, 
     &     -1.870617878E7, 6.669154225E6, 2.413451470E6, 2.691480439E6, 
     &     1.237627138E7, 2.030222826E7, -7.334289876E6, -2.656357635E6,
     &     -2.912593917E6, -8.854155353E6, -1.446966194E7, 
     &     5.295832834E6, 1.909275233E6, 2.048899787E6, 4.290642787E6, 
     &     6.951223648E6, -2.601557110E6, -9.129731614E5, 
     &     -9.627344865E5, -1.314924218E6, -2.095971932E6, 
     &     8.193066795E5, 2.716279969E5, 2.823297853E5, 2.131536582E5, 
     &     3.342907992E5, -1.365390745E5, -4.417841315E4, 
     &     -4.427025540E4/
      DATA egs3/ - 3.600471364E6, -5.805932202E6, 1.773029253E6, 
     &     4.064280430E5, 7.419581557E5, 8.829126250E6, 1.422377198E7, 
     &     -4.473342834E6, -1.073350611E6, -1.845960521E6, 
     &     -9.781712604E6, -1.575666314E7, 5.161226883E6, 1.341287330E6,
     &     2.083994843E6, 7.182555931E6, 1.156915972E7, -3.941330542E6, 
     &     -1.108259560E6, -1.543982755E6, -3.579820035E6, 
     &     -5.740079339E6, 2.041827680E6, 5.981648181E5, 7.629263278E5, 
     &     1.122573403E6, 1.777161418E6, -6.714631146E5, -1.952833263E5,
     &     -2.328129775E5, -1.839672155E5, -2.871137706E5, 
     &     1.153532734E5, 3.423868607E4, 3.738902942E4/
      DATA egs4/2.421750735E6, 4.107929841E6, -1.302310290E6, 
     &     -5.267906237E5, -6.197966854E5, -5.883394376E6, 
     &     -9.964568970E6, 3.198405768E6, 1.293156541E6, 1.506909314E6, 
     &     6.387411818E6, 1.079547152E7, -3.517981421E6, -1.424705631E6,
     &     -1.629099740E6, -4.550695232E6, -7.665548805E6, 
     &     2.530844204E6, 1.021187317E6, 1.141553709E6, 2.182540324E6, 
     &     3.646532772E6, -1.228378318E6, -4.813626449E5, 
     &     -5.299974544E5, -6.518758807E5, -1.070414288E6, 
     &     3.772592079E5, 1.372024952E5, 1.505359294E5, 9.952777968E4, 
     &     1.594230613E5, -6.029082719E4, -2.023689807E4, 
     &     -2.176008230E4/
      DATA egs5/ - 4.902668827E5, -8.089034293E5, 1.282510910E5, 
     &     -1.704435174E4, 8.876109934E4, 1.231673941E6, 2.035989814E6, 
     &     -3.727491110E5, 4.071377327E3, -2.375344759E5, 
     &     -1.429330809E6, -2.376692769E6, 5.216954243E5, 7.268703575E4,
     &     3.008350125E5, 1.114306796E6, 1.868800148E6, -4.718718351E5, 
     &     -1.215904582E5, -2.510379590E5, -5.873353309E5, 
     &     -9.903614817E5, 2.742543392E5, 9.055579135E4, 1.364869036E5, 
     &     1.895325584E5, 3.184776808E5, -9.500485442E4, -3.406036086E4,
     &     -4.380685984E4, -2.969272274E4, -4.916872669E4, 
     &     1.596305804E4, 5.741228836E3, 6.669912421E3/
C
C     THE PROGRAM STARTS HERE
C
      IF (Iz.GE.19) THEN
         IF (Iz.GT.111) THEN
            PRINT 99005
C
99005       FORMAT (/10X,
     &             '*  *  *  *  BARFIT CALLED WITH  Z  LESS THAN 19 OR '
     &             ,
     &             ' GREATER THAN 111.  BFIS IS SET TO 0.0.  *  *  *  *'
     &             )
         ELSEIF (Iz.GT.102 .AND. Il.GT.0) THEN
            PRINT 99010
99010       FORMAT (/10X,
     &             '*  *  *  *  BARFIT CALLED WITH  Z  GREATER THAN 102'
     &             ,
     &    ' AND  L  NOT EQUAL TO ZERO.  BFIS IS SET TO 0.0.  *  *  *  *'
     &    )
         ELSE
            z = FLOAT(Iz)
            a = FLOAT(Ia)
            el = FLOAT(Il)
            amin = 1.2*z + 0.01*z*z
            amax = 5.8*z - 0.024*z*z
            IF (a.LT.amin .OR. a.GT.amax) THEN
               PRINT 99015, Ia
99015          FORMAT (/10X,'*  *  *  *  BARFIT CALLED WITH  A =',I3,
     &                 ', OUTSIDE ','THE ALLOWED VALUES FOR Z = ',I3,
     &                 ' *  *  *  *')
            ELSE
               aa = a/400.
               zz = z/100.
               bfis0 = 0.
               CALL LPOLY(zz,7,pz)
               CALL LPOLY(aa,7,pa)
               DO i = 1, 7
                  DO j = 1, 7
                     bfis0 = bfis0 + elzcof(j,i)*pz(j)*pa(i)
                  ENDDO
               ENDDO
               egs = 0.
               Segs = egs
               Bfis = bfis0
               amin2 = 1.4*z + 0.009*z*z
               amax2 = 20. + 3.0*z
               IF ((a.LT.amin2 - 5.D0 .OR. a.GT.amax2 + 10.D0) .AND. 
     &             Il.GT.0) THEN
                  PRINT 99020, Ia, Il
99020             FORMAT (/10X,'*  *  *  *  BARFIT CALLED WITH  A  =',
     &                    I3,', OUTSIDE',' THE ALLOWED VALUES FOR Z = ',
     &                    I3/26X,'FOR NONZERO  L =',I3,'  *  *  *  *')
               ELSE
                  el80 = 0.
                  el20 = 0.
                  elmax = 0.
                  DO i = 1, 4
                     DO j = 1, 5
                        el80 = el80 + DBLE(elmcof(j,i))*pz(j)*pa(i)
                        el20 = el20 + DBLE(emncof(j,i))*pz(j)*pa(i)
                     ENDDO
                  ENDDO
                  sel80 = el80
                  sel20 = el20
                  DO i = 1, 5
                     DO j = 1, 7
                        elmax = elmax + emxcof(j,i)*pz(j)*pa(i)
                     ENDDO
                  ENDDO
                  Selmax = elmax
                  IF (Il.LT.1) RETURN
                  x = sel20/Selmax
                  y = sel80/Selmax
                  IF (el.LE.sel20) THEN
                     q = 0.2/(sel20**2*sel80**2*(sel20 - sel80))
                     qa = q*(4.*sel80**3 - sel20**3)
                     qb = -q*(4.*sel80**2 - sel20**2)
                     Bfis = Bfis*(1. + qa*el**2 + qb*el**3)
                  ELSE
                     aj = (( - 20.*x**5) + 25.*x**4 - 4.)*(y - 1.)
     &                    **2*y*y
                     ak = (( - 20.*y**5) + 25.*y**4 - 1.)*(x - 1.)
     &                    **2*x*x
                     q = 0.2/((y - x)*((1.-x)*(1.-y)*x*y)**2)
                     qa = q*(aj*y - ak*x)
                     qb = -q*(aj*(2.*y + 1.) - ak*(2.*x + 1.))
                     z = el/Selmax
                     a1 = 4.*z**5 - 5.*z**4 + 1.
                     a2 = qa*(2.*z + 1.)
                     Bfis = Bfis*(a1 + (z - 1.)*(a2 + qb*z)*z*z*(z - 1.)
     &                      )
                  ENDIF
                  IF (Bfis.LE.0.0D0 .OR. el.GT.Selmax) Bfis = 0.0
C
C                 NOW CALCULATE ROTATING GROUND-STATE ENERGY
C
                  IF (el.GT.Selmax .AND. Il.NE.1000) RETURN
                  ell = el/elmax
                  IF (Il.EQ.1000) ell = 1.D0
                  CALL LPOLY(ell,9,pl)
                  DO k = 1, 5
                     DO l = 1, 7
                        DO m = 1, 5
                           egs = egs + egscof(m,l,k)*pz(l)*pa(k)
     &                           *pl(2*m - 1)
                        ENDDO
                     ENDDO
                  ENDDO
                  Segs = egs
                  IF (Segs.LT.0.0D0) Segs = 0.0
                  RETURN
               ENDIF
            ENDIF
         ENDIF
         Bfis = 0.0
         Segs = 0.0
         Selmax = 0.0
         RETURN
      ENDIF
      PRINT 99025
99025 FORMAT (/10X,'*  *  *  *  BARFIT CALLED WITH  Z  LESS THAN 19    '
     &        ,' BFIS IS SET TO 100.0.  *  *  *  *')
      Bfis = 100.
      Segs = 0.0
      Selmax = 0.0
      END
C
      SUBROUTINE MOMFIT(Iz,Ia,Il,Saimin,Saimid,Saimx,Selmax)
C
C    THIS SUBROUTINE RETURNS THE THREE PRINCIPAL-AXIS MOMENTS OF INERTIA
C    AND THE ANGULAR MOMENTUM AT WHICH THE FISSION
C    BARRIER DISAPPEARS,  LMAX,  IN UNITS OF H-BAR,
C    WHEN CALLED WITH INTEGER ARGUMENTS IZ, THE ATOMIC NUMBER,
C    IA, THE ATOMIC MASS NUMBER, AND IL, THE ANGULAR MOMENTUM IN UNITS
C    OF H-BAR, (PLANCK'S CONSTANT DIVIDED BY 2*PI).
C
C         THE MOMENTS OF INERTIA AT L = 0, 70% OF LMAX, 95% OF LMAX,
C    AND AT LMAX ARE FIT TO NZ X NA PARAMETER FUNCTIONS, WHERE NZ = 6,
C    NA = 5 FOR MAJOR MOMENTS OF INERTIA, AND NA = 4 FOR MINOR MOMENTS
C    OF INERTIA.  FOR A GIVEN VALUE OF Z AND A, THE MOMENTS AT THESE VAL
C    OF L ARE EVALUATED FROM THE FITTING FUNCTIONS.  THEN A FUNCTION OF
C    L WHICH PASSES THROUGH THESE VALUES IS CALCULATED.  THE VALUE OF TH
C    MAXIMUM MOMENT OF INERTIA IS TYPICALLY APPROXIMATED TO WITHIN ABOUT
C    THE MINOR MOMENT IS USUALLY SOMEWHAT BETTER APPROXIMATED.  A SEPARA
C    4X4 FUNCTION IS USED TO APPROXIMATE THE MAXIMUM MOMENT FOR
C    Z >= 80, SINCE THE SHAPE AT LMAX IS AXIALLY SYMMETRIC AND OBLATE
C    IN THIS REGION.   THE MOMENTS OF INERTIA ARE GIVEN IN
C    UNITS OF 2/5 M-ZERO (R-ZERO)**2, THE MOMENT OF INERTIA OF A RIGIDLY
C    ROTATING, SHARP-SURFACED SPHERE.  R-ZERO = 1.16 A**(1/3) FERMIS,
C    AND M-ZERO *C-SQUARED IS 931.5016*A - .511004*Z  MEV.
C    IF  MOMFIT  IS CALLED WITH (IZ,IA) VALUES OUTSIDE THE RANGE OF THE
C    THE MOMENTS ARE SET TO 0.0, AND A MESSAGE IS
C    PRINTED ON THE DEFAULT OUTPUT FILE.
C
C         THE MOMENTS OF INERTIA FROM WHICH THE FITS WERE
C    MADE WERE CALCULATED IN 1983-1985 BY A. J. SIERK OF LOS ALAMOS
C    NATIONAL LABORATORY,  GROUP T-9, USING  YUKAWA-PLUS-EXPONENTIAL DOU
C    FOLDED NUCLEAR ENERGY, EXACT COULOUB DIFFUSENESS CORRECTIONS,
C    AND DIFFUSE-MATTER MOMENTS OF INERTIA. THE PARAMETERS OF THE MODEL
C    ARE THOSE DERIVED BY MOLLER AND NIX IN 1979:
C    R-0 = 1.16 FM, AS = 21.13 MEV, KAPPA-S = 2.3  A = 0.68 FM.
C    THE DIFFUSENESS OF THE MATTER AND CHARGE DISTRIBUTIONS USED
C    CORRESPONDS TO A SURFACE DIFFUSENESS PARAMETER (DEFINED BY MYERS)
C    OF 0.99 FM.
C
C    BELOW IS A TABLE OF TEST VALUES TO CHECK IMPLEMENTATION
C    OF THE PROGRAM.
C    Z, A,  L    EGND ST  FISS BAR      MOMENTS OF INERTIA     LMAX
C
C   28, 58, 0    0.00     33.14        0.816 3.603 3.608      46.1
C         ,25   21.36     19.50        0.778 3.662 3.662      46.1
C         ,40   49.66      2.97        0.724 3.648 2.650      46.1
C         ,46.1 59.14      0.00        0.746 3.160 3.160      46.1
C   65,153, 0    0.00     28.88        0.621 3.698 3.698      82.3
C         ,50   19.00     16.16        0.615 3.639 3.639      82.3
C         ,80   45.24      0.26        0.616 2.765 2.788      82.3
C         ,82.3 47.04      0.00        0.682 2.231 2.276      82.3
C   93,229, 0    0.00      3.76        0.823 1.747 1.747      68.1
C         ,45    8.21      1.26        0.765 1.578 1.578      68.1
C         ,68.1 17.96      0.00        1.053 1.053 1.236      68.1
C
C    WRITTEN BY A. J. SIERK,  LANL  T-9
C    VERSION 1.0   OCTOBER, 1984
C    VERSION 2.0   OCTOBER, 1985.
C
C        COPYRIGHT, 1985,  THE REGENTS OF THE UNIVERSITY OF CALIFORNIA.
C        THIS SOFTWARE WAS PRODUCED UNDER A U. S. GOVERNMENT CONTRACT
C        (W-7405-ENG-36) BY THE LOS ALAMOS NATIONAL LABORATORY, WHICH IS
C        OPERATED BY THE UNIVERSITY OF CALIFORNIA FOR THE U. S. DEPARTME
C        OF ENERGY.  THE U. S. GOVERNMENT IS LICENSED TO USE, REPRODUCE,
C        AND DISTRIBUTE THIS SOFTWARE.  PERMISSION IS GRANTED TO THE PUB
C        TO COPY AND USE THIS SOFTWARE WITHOUT CHARGE, PROVIDED THAT THI
C        NOTICE AND ANY STATEMENT OF AUTHORSHIP ARE REPRODUCED ON ALL
C        COPIES.  NEITHER THE GOVERNMENT NOR THE UNIVERSITY MAKES ANY
C        WARRANTY, EXPRESSED OR IMPLIED, OR ASSUMES ANY LIABILITY
C        OR RESPONSIBILITY FOR THE USE OF THIS SOFTWARE.
C
      IMPLICIT DOUBLE PRECISION(a - H), DOUBLE PRECISION(O - z)
C
C
C Dummy arguments
C
      INTEGER Ia, Il, Iz
      DOUBLE PRECISION Saimid, Saimin, Saimx, Selmax
C
C Local variables
C
      DOUBLE PRECISION a, aa, aa2, aa3, aa4, ai70, ai70c(6,5), ai95, 
     &                 ai952, ai952c(6,5), ai95c(6,5), aimax, aimax2, 
     &                 aimaxc(6,5), aimaxh, aimidh, aimx2c(6,5), 
     &                 aimx3c(4,4), aimx4c(4,4), aizro, aizroc(6,5), 
     &                 alpha, amax, amax2, amin, amin2, bb, bb2, bb3, 
     &                 bb4, beta, bi70, bi70c(6,4), bi95, bi95c(6,4), 
     &                 bimax, bimaxc(6,4), bizro, bizroc(6,4), el, ell, 
     &                 elmax, emxcof(7,5), f1, f1m, f2, f2m, f3, f4, 
     &                 ff1, ff2, fg1, fg2, gam, gam2, gam3, pa(7), pi, 
     &                 pz(7), q1, q2, q3, q4, q5, q6, sai70, sai95, 
     &                 sai952, saimax, saizro, sbi70, sbi95, sbimax, 
     &                 sbizro, sigt, sigt2, silt, silt2, simax2, sjgt
      REAL FLOAT
      INTEGER i, j, k, l
      DOUBLE PRECISION sjlt, z, zz
      DATA emxcof/ - 4.10652732E6, 1.00064947E7, -1.09533751E7, 
     &     7.84797252E6, -3.78574926E6, 1.12237945E6, -1.77561170E5, 
     &     1.08763330E7, -2.63758245E7, 2.85472400E7, -2.01107467E7, 
     &     9.48373641E6, -2.73438528E6, 4.13247256E5, -8.76530903E6, 
     &     2.14250513E7, -2.35799595E7, 1.70161347E7, -8.23738190E6, 
     &     2.42447957E6, -3.65427239E5, 6.30258954E6, -1.52999004E7, 
     &     1.65640200E7, -1.16695776E7, 5.47369153E6, -1.54986342E6, 
     &     2.15409246E5, -1.45539891E6, 3.64961835E6, -4.21267423E6, 
     &     3.24312555E6, -1.67927904E6, 5.23795062E5, -7.66576599E4/
      DATA aizroc/2.34441624E4, -5.88023986E4, 6.37939552E4, 
     &     -4.79085272E4, 2.27517867E4, -5.35372280E3, -4.19782127E4, 
     &     1.09187735E5, -1.24597673E5, 9.93997182E4, -4.95141312E4, 
     &     1.19847414E4, 4.18237803E4, -1.05557152E5, 1.16142947E5, 
     &     -9.00443421E4, 4.48976290E4, -1.10161792E4, -8.27172333E3, 
     &     2.49194412E4, -3.39090117E4, 3.33727886E4, -1.98040399E4, 
     &     5.37766241E3, 5.79695749E2, -1.61762346E3, 2.14044262E3, 
     &     -3.55379785E3, 3.25502799E3, -1.15583400E3/
      DATA ai70c/3.11420101E4, -7.54335155E4, 7.74456473E4, 
     &     -4.79993065E4, 2.23439118E4, -4.81961155E3, -7.24025043E4, 
     &     1.72276697E5, -1.72027101E5, 1.03891065E5, -4.83180786E4, 
     &     1.08040504E4, 7.14932917E4, -1.72792523E5, 1.75814382E5, 
     &     -1.07245918E5, 4.86163223E4, -1.10623761E4, -2.87206866E4, 
     &     6.76667976E4, -6.50167483E4, 3.67161268E4, -1.74755753E4, 
     &     4.67495427E3, 1.67914908E4, -3.97304542E4, 3.81446552E4, 
     &     -2.04628156E4, 7.20091899E3, -1.49978283E3/
      DATA ai95c/ - 6.17201449E5, 1.45561724E6, -1.47514522E6, 
     &     9.37798508E5, -3.74435017E5, 7.81254880E4, 1.24304280E6, 
     &     -2.94179116E6, 3.00170753E6, -1.92737183E6, 7.79238772E5, 
     &     -1.64803784E5, -1.49648799E6, 3.52658199E6, -3.56784327E6, 
     &     2.26413602E6, -9.02243251E5, 1.88619658E5, 7.27293223E5, 
     &     -1.72140677E6, 1.75634889E6, -1.12885888E6, 4.57150814E5, 
     &     -9.74833991E4, -3.75965723E5, 8.83032946E5, -8.87134867E5, 
     &     5.58350462E5, -2.20433857E5, 4.62178756E4/
      DATA aimaxc/ - 1.07989556E6, 2.54617598E6, -2.56762409E6, 
     &     1.62814115E6, -6.39575059E5, 1.34017942E5, 2.17095357E6, 
     &     -5.13081589E6, 5.19610055E6, -3.31651644E6, 1.31229476E6, 
     &     -2.77511450E5, -2.66020302E6, 6.26593165E6, -6.31060776E6, 
     &     3.99082969E6, -1.56447660E6, 3.25613262E5, 1.29464191E6, 
     &     -3.05746938E6, 3.09487138E6, -1.97160118E6, 7.79696064E5, 
     &     -1.63704652E5, -7.13073644E5, 1.67482279E6, -1.67984330E6, 
     &     1.05446783E6, -4.10928559E5, 8.43774143E4/
      DATA ai952c/ - 7.37473153E5, 1.73682827E6, -1.75850175E6, 
     &     1.11320647E6, -4.41842735E5, 9.02463457E4, 1.49541980E6, 
     &     -3.53222507E6, 3.59762757E6, -2.29652257E6, 9.21077757E5, 
     &     -1.90079527E5, -1.80243593E6, 4.24319661E6, -4.29072662E6, 
     &     2.71416936E6, -1.07624953E6, 2.20863711E5, 8.86920591E5, 
     &     -2.09589683E6, 2.13507675E6, -1.36546686E6, 5.48868536E5, 
     &     -1.14532906E5, -4.62131503E5, 1.08555722E6, -1.09187524E6, 
     &     6.87308217E5, -2.70986162E5, 5.61637883E4/
      DATA aimx2c/ - 1.16343311E6, 2.74470544E6, -2.77664273E6, 
     &     1.76933559E6, -7.02900226E5, 1.49345081E5, 2.36929777E6, 
     &     -5.60655122E6, 5.70413177E6, -3.66528765E6, 1.47006527E6, 
     &     -3.15794626E5, -2.82646077E6, 6.66086824E6, -6.72677653E6, 
     &     4.27484625E6, -1.69427298E6, 3.58429081E5, 1.39112772E6, 
     &     -3.29007553E6, 3.34544584E6, -2.14723142E6, 8.61118401E5, 
     &     -1.84500129E5, -7.21329917E5, 1.69371794E6, -1.69979786E6, 
     &     1.07037781E6, -4.20662028E5, 8.80728361E4/
      DATA aimx3c/ - 2.88270282E3, 5.30111305E3, -3.07626751E3, 
     &     6.56709396E2, 5.84303930E3, -1.07450449E4, 6.24110631E3, 
     &     -1.33480875E3, -4.20629939E3, 7.74058373E3, -4.50256063E3, 
     &     9.65788439E2, 1.23820134E3, -2.28228958E3, 1.33181316E3, 
     &     -2.87363568E2/
      DATA aimx4c/ - 3.34060345E3, 6.26384099E3, -3.77635848E3, 
     &     8.57180868E2, 6.76377873E3, -1.26776571E4, 7.64206952E3, 
     &     -1.73406840E3, -4.74821371E3, 8.89857519E3, -5.36266252E3, 
     &     1.21614216E3, 1.46369384E3, -2.74251101E3, 1.65205435E3, 
     &     -3.74262365E2/
      DATA bizroc/5.88982505E2, -1.35630904E3, 1.32932125E3, 
     &     -7.78518395E2, 2.73122883E2, -3.49600841E1, -9.67701343E2, 
     &     2.24594418E3, -2.24303790E3, 1.35440047E3, -4.96538939E2, 
     &     6.66791793E1, 1.17090267E3, -2.71181535E3, 2.67008958E3, 
     &     -1.58801770E3, 5.66896359E2, -8.21530057E1, -3.83031864E2, 
     &     9.05191483E2, -9.30560410E2, 5.96618532E2, -2.34403480E2, 
     &     3.97909172E1/
      DATA bi70c/2.32414810E3, -5.42381778E3, 5.40202710E3, 
     &     -3.26923144E3, 1.18318943E3, -1.93186467E2, -4.38084778E3, 
     &     1.03523570E4, -1.05573803E4, 6.59901160E3, -2.47601209E3, 
     &     4.19497260E2, 4.35377377E3, -1.01728647E4, 1.01311246E4, 
     &     -6.14038462E3, 2.21957562E3, -3.62854365E2, -1.84533539E3, 
     &     4.41613298E3, -4.59403284E3, 2.95951225E3, -1.14630148E3, 
     &     2.02702459E2/
      DATA bi95c/1.55359266E3, -3.58209715E3, 3.50693744E3, 
     &     -2.03992913E3, 7.05498010E2, -1.49075519E2, -2.86876240E3, 
     &     6.77107086E3, -6.90300614E3, 4.20246063E3, -1.50290693E3, 
     &     3.13662258E2, 2.60138185E3, -5.95414919E3, 5.70261588E3, 
     &     -3.17188958E3, 9.89207911E2, -1.76320647E2, -1.75198402E3, 
     &     4.16635208E3, -4.25212424E3, 2.59953301E3, -9.09813362E2, 
     &     1.51070448E2/
      DATA bimaxc/4.17708254E3, -8.59358778E3, 6.46392215E3, 
     &     -8.84972189E2, -1.59735594E3, 1.39662071E3, -1.56318394E4, 
     &     3.54574417E4, -3.35945173E4, 1.65495998E4, -3.32021998E3, 
     &     -1.46150905E3, 1.41292811E4, -3.11818487E4, 2.77454429E4, 
     &     -1.19628827E4, 1.28008968E3, 1.66111636E3, -1.92878152E4, 
     &     4.56505796E4, -4.66413277E4, 2.89229633E4, -1.07284346E4, 
     &     1.50513815E3/
      DATA pi/3.1415926535898/
C
C     THE PROGRAM STARTS HERE
C
      IF (Iz.GE.19 .AND. Iz.LE.111) THEN
         IF (Iz.GT.102 .AND. Il.GT.0) THEN
            PRINT 99005
99005       FORMAT (/10X,
     &             '*  *  *  *  MOMFIT CALLED WITH  Z  GREATER THAN 102'
     &             ,
     &  ' AND  L  NOT EQUAL TO ZERO.  MOMENTS ARE SET TO 0.0.*  *  *  *'
     &  )
            GOTO 100
         ELSE
            z = FLOAT(Iz)
            a = FLOAT(Ia)
            el = FLOAT(Il)
            amin = 1.2*z + 0.01*z*z
            amax = 5.8*z - 0.024*z*z
            IF (a.LT.amin .OR. a.GT.amax) THEN
               PRINT 99010, Ia
99010          FORMAT (/10X,'*  *  *  *  MOMFIT CALLED WITH  A =',I3,
     &                 ', OUTSIDE ','THE ALLOWED VALUES FOR Z = ',I3,
     &                 ' *  *  *  *')
               GOTO 100
            ELSE
               aa = a/400.
               zz = z/100.
               amin2 = 1.4*z + 0.009*z*z
               amax2 = 20. + 3.0*z
               IF ((a.LT.amin2 - 5.D0 .OR. a.GT.amax2 + 10.D0) .AND. 
     &             Il.GT.0) THEN
                  PRINT 99015, Ia, Il
99015             FORMAT (/10X,'*  *  *  *  MOMFIT CALLED WITH  A  =',
     &                    I3,', OUTSIDE',' THE ALLOWED VALUES FOR Z = ',
     &                    I3/26X,'FOR NONZERO  L =',I3,'  *  *  *  *')
                  GOTO 100
               ELSE
                  CALL LPOLY(aa,6,pa)
                  elmax = 0.
                  CALL LPOLY(zz,7,pz)
                  DO i = 1, 5
                     DO j = 1, 7
                        elmax = elmax + emxcof(j,i)*pz(j)*pa(i)
                     ENDDO
                  ENDDO
                  Selmax = elmax
C--------in case l is larger than the stability limit set it to selmax
                  IF (el.GT.Selmax) el = Selmax
                  ell = el/Selmax
                  IF (Il.EQ.1000) ell = 1.0
                  aizro = 0.
                  ai70 = 0.
                  aimax = 0.
                  ai95 = 0.
                  bizro = 0.
                  bi70 = 0.
                  bimax = 0.
                  bi95 = 0.
                  aimax2 = 0.
                  ai952 = 0.
C
C                 NOW CALCULATE ROTATING MOMENTS OF INERTIA
C
                  IF (el.GT.Selmax .AND. Il.LT.1000) RETURN
                  DO l = 1, 6
                     DO k = 1, 5
                        aizro = aizro + aizroc(l,k)*pz(l)*pa(k)
                        ai70 = ai70 + ai70c(l,k)*pz(l)*pa(k)
                        ai95 = ai95 + ai95c(l,k)*pz(l)*pa(k)
                        aimax = aimax + aimaxc(l,k)*pz(l)*pa(k)
                        ai952 = ai952 + ai952c(l,k)*pz(l)*pa(k)
                        aimax2 = aimax2 + aimx2c(l,k)*pz(l)*pa(k)
                     ENDDO
                     DO k = 1, 4
                        bizro = bizro + bizroc(l,k)*pz(l)*pa(k)
                        bi70 = bi70 + bi70c(l,k)*pz(l)*pa(k)
                        bi95 = bi95 + bi95c(l,k)*pz(l)*pa(k)
                        bimax = bimax + bimaxc(l,k)*pz(l)*pa(k)
                     ENDDO
                  ENDDO
                  ff1 = 1.0
                  ff2 = 0.0
                  fg1 = 1.0
                  fg2 = 0.0
C-----NEXT LINE WAS AFTER AIMAXH=0. (MOVED HERE TO AVOID UNDEFINE)
                  aimidh = 0.
                  IF (Iz.GT.70) THEN
                     aimaxh = 0.
                     DO l = 1, 4
                        DO k = 1, 4
                           aimaxh = aimaxh + aimx3c(l,k)*pz(l)*pa(k)
                           aimidh = aimidh + aimx4c(l,k)*pz(l)*pa(k)
                        ENDDO
                     ENDDO
                     IF (Iz.GT.80) ff1 = 0.0
                     IF (Iz.GE.80 .OR. bimax.GT.0.95D0) fg1 = 0.0
                     IF (aimaxh.GT.aimax) ff1 = 0.0
                     ff2 = 1.0 - ff1
                     fg2 = 1.0 - fg1
                     aimax = aimax*ff1 + ff2*aimaxh
                     aimax2 = aimax2*ff1 + ff2*aimidh
                  ENDIF
                  saizro = aizro
                  bimax = bimax*fg1 + aimidh*fg2
                  IF (saizro.LT.0.0D0) saizro = 0.0
                  sai70 = ai70
                  IF (sai70.LT.0.0D0) sai70 = 0.0
                  sai95 = ai95
                  IF (sai95.LT.0.0D0) sai95 = 0.0
                  saimax = aimax
                  IF (saimax.LT.0.0D0) saimax = 0.0
                  sai952 = ai952
                  IF (sai952.LT.0.0D0) sai952 = 0.0
                  simax2 = aimax2
                  IF (simax2.LT.0.0D0) simax2 = 0.0
                  sbimax = bimax
                  IF (sbimax.LT.0.0D0) sbimax = 0.0
                  sbi70 = bi70
                  IF (sbi70.LT.0.0D0) sbi70 = 0.0
                  sbi95 = bi95
                  IF (sbi95.LT.0.0D0) sbi95 = 0.0
                  sbizro = bizro
                  IF (sbizro.LT.0.0D0) sbizro = 0.0
                  q1 = -3.148849569
                  q2 = 4.465058752
                  q3 = -1.316209183
                  q4 = 2.26129233
                  q5 = -4.94743352
                  q6 = 2.68614119
                  gam = -
     &                  20.*LOG(ABS(saizro - sai95)/ABS(saizro - saimax)
     &                  )
                  aa = q1*saizro + q2*sai70 + q3*sai95
                  bb = q4*saizro + q5*sai70 + q6*sai95
                  gam2 = -20.*LOG(ABS(saizro - sai952)/ABS(saizro - 
     &                   simax2))
                  aa2 = q1*saizro + q2*sai70 + q3*sai952
                  bb2 = q4*saizro + q5*sai70 + q6*sai952
                  aa3 = q1*sbizro + q2*sbi70 + q3*sbi95
                  bb3 = q4*sbizro + q5*sbi70 + q6*sbi95
                  gam3 = 60.
                  alpha = pi*(ell - 0.7)
                  beta = 5.*pi*(ell - 0.9)
                  sigt = saizro + (saimax - saizro)*EXP(gam*(ell - 1.0))
                  silt = saizro + aa*ell**2 + bb*ell**4
                  sjgt = sbi95 + (sbimax - sbi95)*EXP(gam3*(ell - 1.0))
                  sjlt = sbizro + aa3*ell**2 + bb3*ell**4
                  sigt2 = saizro + (simax2 - saizro)
     &                    *EXP(gam2*(ell - 1.0))
                  silt2 = saizro + aa2*ell**2 + bb2*ell**4
                  f1 = silt*COS(alpha)**2 + sigt*SIN(alpha)**2
                  f2 = silt*COS(beta)**2 + sigt*SIN(beta)**2
                  f1m = silt2*COS(alpha)**2 + sigt2*SIN(alpha)**2
                  f2m = silt2*COS(beta)**2 + sigt2*SIN(beta)**2
                  f3 = sjlt*COS(alpha)**2 + sjgt*SIN(alpha)**2
                  f4 = sjlt*COS(beta)**2 + sjgt*SIN(beta)**2
                  IF (ell.LE.0.95D0) THEN
                     IF (ell.LE.0.70D0) THEN
C
C                       ELL IS LESS THAN 0.7,  USE ILT
C
                        Saimin = sjlt
                        Saimx = silt
                        Saimid = silt2
                        IF (ff2.GT.0.01D0 .AND. fg2.GT.0.01D0) GOTO 5
                        GOTO 10
C
C                       ELL IS GREATER THAN 0.7, LESS THAN 0.95  USE FIRST L. C.
C
                     ENDIF
                     Saimx = f1
                     Saimin = f3
                     Saimid = f1m
                     IF (ff2.GT.0.01D0 .AND. fg2.GT.0.01D0) GOTO 5
                     GOTO 10
C
C                    ELL IS GREATER THAN 0.95,  USE 2ND L. C.
C
                  ENDIF
                  Saimx = f2
                  Saimin = f4
                  Saimid = f2m
                  IF (ff2.LE.0.01D0 .OR. fg2.LE.0.01D0) GOTO 10
C
C                 FOR NUCLEI WITH Z GT 80 USE 4TH ORDER FUNCTION WITH SEPARATE
C                 FIT TO IMAX AND IMAX2
C
    5             q1 = 4.001600640
                  q2 = 0.960784314
                  q3 = 2.040816327
                  aa3 = q1*sai70 - q2*saimax - (1. + q3)*saizro
                  bb3 = ( - q1*sai70) + (1. + q2)*saimax + q3*saizro
                  aa4 = q1*sai70 - q2*simax2 - (1. + q3)*saizro
                  bb4 = ( - q1*sai70) + (1. + q2)*simax2 + q3*saizro
                  Saimx = saizro + aa3*ell**2 + bb3*ell**4
                  Saimid = saizro + aa4*ell**2 + bb4*ell**4
               ENDIF
   10          Saimid = MIN(Saimx,Saimid)
               IF (Saimin.LT.0.0D0) Saimin = 0.0
               RETURN
            ENDIF
         ENDIF
      ENDIF
      PRINT 99020
C
99020 FORMAT (/10X,'*  *  *  *  MOMFIT CALLED WITH  Z  LESS THAN 19 OR '
     &        ,' GREATER THAN 111.  MOMENTS ARE SET TO 0.0.  *  *  *  *'
     &        )
  100 aimax = 0.0
      Saimx = 0.0
      Saimin = 0.0
      Saimid = 0.0
      Selmax = 0.0
      END
C
      SUBROUTINE LPOLY(X,N,Pl)
C
C    THIS SUBROUTINE CALCULATES THE ORDINARY LEGENDRE POLYNOMIALS OF
C    ORDER 0 TO N-1 OF ARGUMENT  X  AND STORES THEM IN THE VECTOR
C    PL.  THEY ARE CALCULATED BY RECURSION RELATION FROM THE FIRST TWO
C    POLYNOMIALS.
C
C    WRITTEN BY A. J. SIERK   LANL  T-9  FEBRUARY,1984
C
C    NOTE:  PL AND X MUST BE DOUBLE PRECISION ON 32-BIT COMPUTERS
C
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C
C Dummy arguments
C
      INTEGER N
      DOUBLE PRECISION X
      DOUBLE PRECISION Pl(N)
C
C Local variables
C
      INTEGER i
      Pl(1) = 1.0
      Pl(2) = X
      DO i = 3, N
         Pl(i) = ((2*i - 3)*X*Pl(i - 1) - (i - 2)*Pl(i - 2))/(i - 1)
      ENDDO
      END
