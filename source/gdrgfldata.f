C
      SUBROUTINE GDRGFLDATA(Znucleus, Anucleus)
C
C    ********************************************************************
C    *                                                                  *
C    * Assignment of the GDR parameters and parametrs of the GFL model  *
C    * to nucleus with atomic number 'Znucleus'and mass number'Anucleus'*  .                                                   *
C    *                                                                  *
C    *  The parameters are put in the following COMMON's:               *
C    *   -----------------------------------------------                *
C    *                                                                  *
C    *  COMMON /PARGDR/  EG1, GW1, CS1, EG2, GW2, CS2, NG               *
C    *  COMMON /GFLPARAM/ BETagfl2, S2Plusgfl                           *
C    *                                                                  *
C    *   -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --                *
C    *                                                                  *
C    * 'COMMON/PARGDR/EG1,GW1,CS1,EG2,GW2,CS2,NG'  contains             *
C    *  input for GDR parameters (all deformed nuclei are               *
C    *  considered as  axially symmetric spheroids):                    *
C    *                                                                  *
C    *  EG1= peak energy of the first peak,                             *
C    *  GW1= full width of the first peak at half-maximum,              *
C    *  CS1= peak cross section of the first peak,                      *
C    *  EG2= peak energy of the second peak,                            *
C    *  CS2= peak cross section of the second peak,                     *
C    *  GW2= full width of the second peak at half-maximum              *
C    *  CS2= peak cross section of the second peak,                     *
C    *  NG : for NG=1, single peak(spherical nucleus),                  *
C    *       for NG=2, double peaks(deformed nucleus);                  *
C    *                                                                  *
C    * 'COMMON /GFLPARAM/ BETagfl2, S2Plusgfl'  contains                *
C    *  the parameters of the GFL model:                                *
C    *                                                                  *
C    *  BETagfl2(=beta**2)= square of "deformation"                     *
C    *                     parameter 'beta' associated                  *
C    *                     with nuclear quadrupole moment,              *
C    *  S2Plus(=(E2+)*beta**2)= product of first-excited                *
C    *                          2+ state energy(in MeV)                 *
C    *                          deforamation parameter                  *
C    *                                                                  *
C    *  Setting the GDR parameters                                      *
C    *  --------------------------                                      *
C    *  Attempts are made  initially to set the GDR parameters          *
C    *  from  "beijingn.dat" file (see RIPL).                           *
C    *                                                                  *
C    *  If GDR parameters are not found in "beijing.dat" they           *
C    *  are calculated by global parametrization with effective         *
C    *  quadrupole deformation parameters from "deflib.dat"file.        *
C    *  These effective quadrupole deformation parameters were obtained *
C    *  with  the use of  "Moller.dat" file of RIPL.                    *
C    *                                                                  *
C    *                                                                  *
C    *  Setting parameters of GFL model                                 *
C    *  -------------------------------                                 *
C    *  GFL model -> S.F.Mughabghab,C.L.Dunford,Phys.Lett.B487(2000)155 *
C    *                                                                  *
C    *  Initially attemts are made to set parameter 'beta' and first-   *
C    *  -excited state energy (E2+) from"def_eff.dat" file.This file is *
C    *  prepared from data file "raman_tableI.txt" given by  S.Raman,   *
C    *  C.W.Nestor,Jr, P.Tikkanen [Atom.Data Nucl.Data Tabl. 78(2001)1; *
C    *  Table 1 for even-even nuclei].The value of deformation parameter*
C    *  '|beta2|' from "deflib.dat"file is used for 'beta' if the 'beta'*
C    *  is absent in the "def_eff.dat" file and  global parametrisation *
C    *  for parameter 'S2Plus=(E2+)*beta**2' is used in this case.      *
C    ********************************************************************
C
C-----Plujko_new--replacement of variable z by zz (conflict
C       with Z in global.h)
C-----Plujko_new--replacement of variable csa by csaa (conflict
C       with  in global.h)
      IMPLICIT NONE
      DOUBLE PRECISION a0, a3, aann, alambda, alpha2, b0, CS0, CS1, CS2,
     &                 csaa, csb, EG0, EG1, EG2, ea, eb, GW0, GW1, GW2,
     &                 hbetagfl, henergygfl, BETagfl2, S2Plusgfl
C----Plujko_new
Cb      DOUBLE PRECISION halpha2, hcs1, hcs2, he1, he2, hgw1, hgw2, pi, z,
Cb     &                 betagfl, energygfl, Znucleus, Anucleus
      DOUBLE PRECISION halpha2, hcs1, hcs2, he1, he2, hgw1, hgw2, pi,
     &              zz, betagfl, energygfl, Znucleus, Anucleus
      INTEGER  Key_GDRGFL, Key_shape
      COMMON /GSA/ Key_shape, Key_GDRGFL
C----Plujko_new(End)
      INTEGER i, ka, keyload, kz, n, nana, nanz, NG, nna, nng, nnz,
     &        nzram, naram, numram, keyram, keyalpa, kaa, kzz
C
      COMMON /PARGDR/  EG1, GW1, CS1, EG2, GW2, CS2, NG
      COMMON /GFLPARAM/ BETagfl2, S2Plusgfl
C
C----Plujko_new
Cb      DIMENSION nnz(300), nna(300), nng(300), he1(300), hcs1(300),
Cb     &          hgw1(300), he2(300), hcs2(300), hgw2(300), nanz(9000),
Cb     &          nana(9000), halpha2(9000), hbetagfl(700),
Cb     &          henergygfl(700), nzram(700), naram(700)

      COMMON /MLOCOM2/ keyload, keyalpa, kzz, kaa
      COMMON /MLOCOM3/ nnz(300), nna(300), nng(300),
     &        he1(300), hcs1(300),
     &        hgw1(300), he2(300), hcs2(300), hgw2(300), nanz(9000),
     &        nana(9000), halpha2(9000), hbetagfl(700),
     &        henergygfl(700), nzram(700), naram(700),numram
      DATA pi/3.141592654D0/
Cb    DATA pi/3.141592654D0/, keyload/0/, keyalpa/0/
Cb    DATA kzz/0/, kaa/0/
C----Plujko_new(End)
      kz = Znucleus + 0.001
      ka = Anucleus + 0.001
      IF(kz.EQ.kzz .AND. ka.EQ.kaa) GOTO 99999
      kzz=kz
      kaa=ka
      IF(keyload.NE.1)THEN
         keyload = 1
         OPEN(81, FILE = 'beijingn.dat', STATUS = 'old')
         REWIND 81
         READ(81, 99001)
99001    FORMAT(/)
         DO i = 1, 270
            READ(81, 99002, END = 50, ERR = 50)nnz(i), nna(i), nng(i),
     &           he1(i), hcs1(i), hgw1(i), he2(i), hcs2(i), hgw2(i)
99002       FORMAT(i3, 3x, i4, 3x, i3, 2(2F7.3, f6.3))
         ENDDO
 50      OPEN(82, FILE = 'deflib.dat', STATUS = 'old')
         REWIND 82
         READ(82, 99003)
99003    FORMAT(////)
         DO i = 1, 9000
            READ(82, 99004, END = 100, ERR = 100)nanz(i), nana(i),
     &           halpha2(i)
99004       FORMAT(2I4, f7.3)
         ENDDO
 100     OPEN(84, FILE = 'defeff.dat', STATUS = 'old')
         REWIND 84
         READ(84, '(///////////)')
         numram = 0
         DO i = 1, 700
            READ(84, 99005, END = 200, ERR = 200)nzram(i), naram(i),
     &           henergygfl(i), hbetagfl(i)
99005       FORMAT(I4, I4, f13.5, f10.4)
            numram = i
         ENDDO
      ENDIF
 200  n = ka-kz
      zz = kz
      aann = ka
      a3 = aann**0.3333333
      EG0 = 31.2/a3 + 20.6/SQRT(a3)
      GW0 = 0.026*EG0**1.91
      CS0 = 1.2*120.*n*zz/(aann*pi*GW0)
C----Plujko_new
        IF (Key_GDRGFL.NE.0) THEN
C----Plujko_new(END)
      DO i = 1, 270
         IF(kz.EQ.nnz(i) .AND. ka.EQ.nna(i))THEN
            NG = nng(i)
            EG1 = he1(i)
            CS1 = hcs1(i)
            GW1 = hgw1(i)
            EG2 = he2(i)
            CS2 = hcs2(i)
            GW2 = hgw2(i)
            GOTO 400
         ENDIF
      ENDDO
C----Plujko_new
        ENDIF
C----Plujko_new(END)
C     ***********************************************************
C     *Setting the deformation parameter from "deflib.dat" file *
C     *for calculation of the GDR energies and widths           *
C     ***********************************************************
      keyalpa = 1
      DO i = 1, 9000
         IF(kz.EQ.nanz(i) .AND. ka.EQ.nana(i))THEN
            alpha2 = halpha2(i)
            GOTO 300
         ENDIF
      ENDDO
      alpha2 = 0.
 300  IF(ABS(alpha2).GT.0.001)THEN
C        *************************************************
C        *Global GDR parameterization for deformed nuclei*
C        *( classical sum rule with correction)          *
C        *************************************************
         NG = 2
         alambda = (1. + 0.6*alpha2**2 + 2.*alpha2**3/35.)**0.3333333
         a0 = (1. + alpha2)/alambda
         b0 = (1. - 0.5*alpha2)/alambda
         eb = EG0*(1. - 1.51E-02*(a0 + b0)*(a0 - b0))/b0
         ea = eb/(0.911*a0/b0 + 0.089)
         csaa = CS0/3.
         csb = CS0*2./3.
         EG1 = ea
         EG2 = eb
         CS1 = csaa
         CS2 = csb
         IF(ea.GT.eb)THEN
            EG1 = eb
            EG2 = ea
            CS1 = csb
            CS2 = csaa
         ENDIF
         GW1 = 0.026*EG1**1.91
         GW2 = 0.026*EG2**1.91
      ELSE
C        *******************************************************
C        * Global GDR parameterization for spherical targets   *
C        * ( classical sum rule with correction)               *
C        *******************************************************
         NG = 1
         EG1 = EG0
         GW1 = GW0
         CS1 = CS0
         EG2 = 0.
         GW2 = 0.
         CS2 = 0.
      ENDIF
C     *********************************************************
C     * Setting the GFL parameters '|beta|' from "defeff.dat" *
C     * and 'S2Plus=(E2+)*beta**2'                            *
C     *********************************************************
 400  DO i = 1, numram
         keyram = 0
         IF(kz.EQ.nzram(i) .AND. ka.EQ.naram(i))THEN
            IF(hbetagfl(i).GT.0.)THEN
               keyram = 1
               betagfl = hbetagfl(i)
C              *****************************************************************
C              *'BETagfl2=beta**2' and  'S2Plus=(E2+)*beta**2'  -------        *
C              *parameters of the GFL model[BETagfl2=beta; ENErgygfl=E2+(MeV)] *
C              *****************************************************************
               BETagfl2 = betagfl**2
               energygfl = henergygfl(i)*0.001
               S2Plusgfl = BETagfl2*energygfl
            ENDIF
         ENDIF
         IF(keyram.EQ.1)GOTO 99999
      ENDDO
C     ***********************************************************
C     * Global parametrization for 'S2Plus=(E2+)*beta**2'       *
C     * and setting the '|beta2|' from "deflib.dat" file        *
C     * as 'beta' of the GFL model.                             *
C     *---------------------------------------------------------*
C     *Setting the deformation parameter from "deflib.dat" file *
C     *for  calculation  of  the  GFL model parameter  if it is *
C     *absent ('keyalpa=0')                                    *
C     ***********************************************************
      IF(keyalpa.EQ.0)THEN
         DO i = 1, 9000
            IF(kz.EQ.nanz(i) .AND. ka.EQ.nana(i))THEN
               alpha2 = halpha2(i)
               GOTO 500
            ENDIF
         ENDDO
         alpha2 = 0.
      ENDIF
 500  betagfl = 1.5853*ABS(alpha2)
      BETagfl2 = betagfl**2
      S2Plusgfl = 217.156/aann**2
99999 END
