$DEBUG
Ccc   * $Author: Capote $
Ccc   * $Date: 2004-06-07 10:34:01 $
Ccc   * $Id: gdrgfldata.f,v 1.2 2004-06-07 10:34:01 Capote Exp $
C
      SUBROUTINE GDRGFLDATA(Znucleus, Anucleus)
C
C    ********************************************************************
C    *                                                                  *
C    * Assignment of the GDR and GFL model parameters                   *
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
C    *                          deformation parameter                   *
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
      DOUBLE PRECISION etmp,fjtmp,btmp
	INTEGER nntmp,natmp,MAXgdr
	Parameter (MAXgdr=6000)
C----Plujko_new
Cb      DOUBLE PRECISION halpha2, hcs1, hcs2, he1, he2, hgw1, hgw2, pi, z,
Cb     &                 betagfl, energygfl, Znucleus, Anucleus
      DOUBLE PRECISION halpha2, hcs1, hcs2, he1, he2, hgw1, hgw2, pi,
     &              zz, betagfl, energygfl, Znucleus, Anucleus,
     &              he1t(MAXgdr), hgw1t(MAXgdr), etat(MAXgdr), 
     &              he2t(MAXgdr), hgw2t(MAXgdr), etaeps
      INTEGER  Key_GDRGFL, Key_shape
      COMMON /GSA/ Key_shape, Key_GDRGFL
C----Plujko_new(End)
      INTEGER i, ka, keyload, kz, n, nana, nanz, NG, nna, nng, nnz,
     &        nzram, naram, numram, keyram, keyalpa, kaa, kzz,
	&        nnzt(MAXgdr), nnat(MAXgdr), nngt(MAXgdr)
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
C----Plujko_new(End)
      kz = Znucleus + 0.001
      ka = Anucleus + 0.001
      IF(kz.EQ.kzz .AND. ka.EQ.kaa) return
      kzz=kz
      kaa=ka
      IF(keyload.NE.1)THEN
        keyload = 1
C       README file format (2I4,1X,A2,2I3,6F7.2,2X,A5)
        OPEN(81, FILE = '../RIPL-2/gamma/gdr-parameters-exp.dat', 
     &                                       STATUS = 'old',ERR=3500)
        READ(81,'(///)') ! Skipping first 4 title lines
        DO i = 1, 270
           READ(81,'(2I4, 1x,2x,3x, i3, 6F7.2)', END = 40, 
     & 		  ERR = 40) nnz(i), nna(i), nng(i),
     &          he1(i), hcs1(i), hgw1(i), he2(i), hcs2(i), hgw2(i)
        ENDDO
40	  CLOSE(81)
C       README file format (2i4,1x,a2,f7.3,4f7.2)
41      OPEN(81, FILE = '../RIPL-2/gamma/gdr-parameters-theor.dat', 
     &                                       STATUS = 'old',ERR=3550)
        READ(81,'(///)') ! Skipping first 4 title lines
        DO i = 1, MAXgdr
           READ(81,'(2I4, 1x,2x, f7.3, 4F7.2)', END = 50, 
     & 		  ERR = 50) nnzt(i), nnat(i), etat(i), 
     &          he1t(i), hgw1t(i), he2t(i), hgw2t(i)
	     nngt(i)=2
	     if(he1t(i).eq.he2t(i)) nngt(i)=1
        ENDDO
50      CLOSE(81)
51      OPEN(82, FILE = '../data/deflib.dat', STATUS = 'old',ERR=3600)
        READ(82, '(////)')  ! Skipping first 5 title lines
        DO i = 1, 9000
           READ(82, '((2I4, f7.3))', END = 100, ERR = 100)
     &	            nanz(i), nana(i), halpha2(i)
        ENDDO
100     CLOSE(82)
C       OPEN(84, FILE = 'defeff.dat', STATUS = 'old')
101     OPEN(84, FILE = 
     &       '../RIPL-2/optical/om-data/om-deformations.dat',
     &       STATUS = 'old',ERR=3700)
        READ(84,'(///)') ! Skipping first 4 title lines
        numram = 0
        DO i = 1, 700
C       README file format (2i4,1x,a2,1x,f10.6,1x,f4.1,i3,i2,1x,f10.6,2x,a13)
            READ(84,'(2I4,4x,f10.6,1x,f4.1,6x,f10.6)', END = 200, 
     &      	  ERR = 200) nntmp,natmp,etmp,fjtmp,btmp
C           Selecting only 2+ states
	      if(abs(fjtmp-2.d0).gt.0.0001) cycle 
	      numram=numram+1 
            nzram(numram)=nntmp
		  naram(numram)=natmp
	      henergygfl(numram)=etmp
		  hbetagfl(numram)=btmp
        ENDDO
200     CLOSE(84)
      ENDIF
201   n = ka-kz
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
C       If GDR parameters not found they are going to be retrieved from 
C       the RIPL-2 Goriely theoretical values
        DO i = 1, MAXgdr
          IF(kz.EQ.nnzt(i) .AND. ka.EQ.nnat(i))THEN
            NG = nngt(i)
            EG1 = he1t(i)
            CS1 = CS0
            GW1 = hgw1t(i)
            EG2 = he2t(i)
C           CS2 = hcs2t(i)
            GW2 = hgw2t(i)
	      etaeps = etat(i)
            IF(abs(etaeps-1.d0).GT.0.0001)THEN
C             *************************************************
C             *Global GDR parameterization for deformed nuclei*
C             *(classical sum rule with correction)           *
C             *************************************************
              CS1 = CS0/3.
              CS2 = CS0*2./3.
            ENDIF
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
C              energygfl = henergygfl(i)*0.001
               energygfl = henergygfl(i)
C              RIPL-2 energies in MeV, RCN 06/2004 
               S2Plusgfl = BETagfl2*energygfl
            ENDIF
         ENDIF
         IF(keyram.EQ.1) return
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
 500  BETagfl = 1.5853*ABS(alpha2)
      BETagfl2 = BETagfl**2
      S2Plusgfl = 217.156/aann**2
      RETURN
3500  WRITE(6,'(1x,A14,A39,A43)') 'WARNING: FILE ',
     &           '../RIPL-2/gamma/gdr-parameters-exp.dat',
     &           ' not FOUND, theoretical RIPL-2 will be used'
      GOTO 41
3550  WRITE(6,'(1x,A14,A41,A35)') 'WARNING: FILE ',
     &           '../RIPL-2/gamma/gdr-parameters-theor.dat',
     &           ' not FOUND, DEFAULT GDR VALUES USED'
      GOTO 51
3600  WRITE(6,'(1x,A14,A18,A43)') 'WARNING: FILE ','../data/deflib.dat',	
     &           ' not FOUND, DEFAULT deformation VALUES USED'
      GOTO 101
3700  WRITE(6,'(1x,A14,A45,A54)') 'WARNING: FILE ',	
     &       '../RIPL-2/optical/om-data/om-deformations.dat',
     &           ' not FOUND, DEFAULT dynamical deformation VALUES USED'
	GOTO 201 
      END
