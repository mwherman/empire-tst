Ccc   * $Rev: 2526 $
Ccc   * $Author: shoblit $
Ccc   * $Date: 2012-02-09 21:34:11 +0100 (Do, 09 Feb 2012) $
 
      SUBROUTINE KUMAR_OMP(Amass,Az,Ex,Elab,Rcc,Vlib,Rlib,Alib)
      IMPLICIT REAL*8(A - H,O - Z)
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      REAL*8 :: Amass, Az, Elab, Ex, Rcc
      REAL*8, DIMENSION(6) :: Alib, Rlib, Vlib
C
C Local variables
C
      REAL*8 :: ai, ajie, ajii, ajiref, ajre, ajref, ajrref, an1, an2, 
     &          anr1, anr2, ar, at13, bar, beta, con, const, const1, 
     &          const2, del, diff, dj, dve, dvef, dver, dverf, ebar, ec, 
     &          ecm, ee, eref, erefc, esq, pi, r24i, r24ic, r24r, r24rc, 
     &          ri, roi, ror, rr, v0, voi, vor
      REAL*8, DIMENSION(200) :: aji, e
      INTEGER :: i, j, n, no
C
C*** End of declarations rewritten by SPAG
C
C
C     CALCULATION OF REAL AND IMAGINARY PARTS OF WOODS - SAXON FORM
C     OF POTENTIAL PARAMETERS FOR ALPHA-NUCLEUS  SYSTEMS -
C     Ashok Kumar, S. Kailas, S. Rathi and K. Mahata, Nucl. Phys. A776 (2006) 105.
C
C     PLEASE SEND YOUR COMMENTS TO
C     ashokum@barc.gov.in or kailas@barc.gov.in
C
C     REAL AND IMAGINARY POTENTIALS STARTING FROM SYSTEMATICS OF
C     VOLUME INTEGRALS, RADIUS AND SLOPE OF POTENTIAL AT
C     RADIUS CLOSE TO STRONG ABSORPTION RADIUS (2.4).
C
C     Mahaux, Ngo & Satchler; Nucl.,Phys. A446 (1986) 354
C     FOR DISPERSION CORRECTION.
C
C     INPUT
C      AMASS : MASS OF TARGET NUCLEUS
C      AZ : ATOMIC NO. OF TARGET NUCLEUS
C      EX : 1ST EXCITED STATE ENERGY IN MeV OF TARGET NUCLEUS
C      ELAB : ALPHA ENERGY IN LAB. SYSTEM (MeV) (1 IN EACH CARD)
C
C     Dummy arguments
C
 
 
      pi = 4.*ATAN(1.)
 
      DO i = 1, 6
        Alib(i) = 0.D0
        Rlib(i) = 0.D0
        Vlib(i) = 0.D0
      ENDDO
 
      at13 = Amass**(1./3.)
C       BAR IS THE COULOMB BARRIER ENERGY
      bar = 1.44*2.*Az/1.5/(4.**(1./3.) + at13)
C       R2.4 SYSTEMATICS FOR REAL AND IMAGINARY PARTS OF POTENTIAL,
C       AR AND AI ARE THE DIFFUSENESS PARAMETERS FOR
C       REAL AND IMAGINARY PARTS OF THE POTENTIAL
      r24r = 1.35*at13 + 2.55
      r24i = 1.35*at13 + 2.14
      ar = 0.76
      ai = 0.60
C       EREF IS THE REFERENCE ENERGY FOR NORMALISATION IN MAKING
C       DISPERSION RELATION CALCULATION
      eref = 140.0
      erefc = eref*Amass/(Amass + 4.)
C       AJREF IS THE VOLUME INTEGRAL AT THE REFERENCE ENERGY AND
C       IS CALCULATED USING THE EMPIRICAL RELATION GIVEN  BELOW
      ajref = (224. - 0.98*erefc/Amass**0.184 + 2.57*Az/at13)
     &        *(1. + (2.05/at13))
      ajref = -ajref
C       IMAGINARY PART SYSTEMATICS
C       (REF: A. Shridahar et al.; Phys. Rev. C30 (1984) 1760.)
      ajii = 34.4*(1. + 7.1/at13)
C       CALCULATION OF RADIUS PARAMETERS FOR REAL AND IMAGINARY
C       POTENTIALS BY FITTING R2.4 AND VOLUME INTEGRAL VALUES
C       AT E = 90 MEV
      const = pi/(3.*Amass)
      const1 = pi*ar
      const2 = pi*ai
      ec = 90.*Amass/(Amass + 4.)
      beta = 0.1
      ebar = bar + Ex
      ajrref = (224. - 0.98*ec/Amass**0.184 + 2.57*Az/at13)
     &         *(1. + (2.05/at13))
      ajiref = ajii*(1. - EXP( - (ec-ebar)*beta))
 
      ror = 1.0
      j = 0
   10 rr = ror*at13
      vor = ajrref/const/rr**3./(1 + (const1/rr)**2.)
      r24rc = rr + ar*LOG((vor - 2.4)/2.4)
      diff = r24rc - r24r
      IF(j.LE.200)THEN
        IF(ABS(diff).GT.0.05)THEN
          j = j + 1
          ror = ror + 0.005
          GOTO 10
        ENDIF
      ENDIF
 
      roi = 1.2
      j = 0
   20 ri = roi*at13
      voi = ajiref/const/ri**3./(1. + (const2/ri)**2.)
      r24ic = ri + ai*LOG((voi - 2.4)/2.4)
      diff = r24ic - r24i
      IF(j.LE.200)THEN
        IF(ABS(diff).GT.0.05)THEN
          j = j + 1
          roi = roi + 0.005
          GOTO 20
        ENDIF
      ENDIF
 
      con = 35.5 - 6.*at13
      del = con + bar
      ecm = Elab*Amass/(Amass + 4.)
      IF(ecm.LT.Ex)ecm = Ex
      esq = (ecm - Ex)*(ecm - Ex)
      ajie = ajii*esq/(esq + (del*del))
      IF(Elab.GT.140.0)THEN
        ajre = (224. - 0.98*ecm/Amass**0.184 + 2.57*Az/at13)
     &         *(1. + (2.05/at13))
        GOTO 30
      ENDIF
 
      n = 0
      ee = Ex
      DO i = 1, 50
        n = n + 1
        e(i) = ee
C         ASSUME THE DISPERSION CORRECTION TO VANISH AT EREF.
        IF(e(i).GT.140.0)EXIT
        esq = (e(i) - Ex)*(e(i) - Ex)
        aji(i) = ajii*esq/(esq + (del*del))
        ee = ee + 3.00001
      ENDDO
      no = n
 
      dvef = 0.
      dverf = 0.
      DO j = 1, no - 3
        dj = e(j + 1) - e(j)
        an1 = (Elab - e(j))*LOG(ABS((Elab-e(j))/dj))
        an2 = (Elab - e(j + 1))*LOG(ABS((Elab-e(j+1))/dj))
        dve = ((aji(j+1) - aji(j))/dj)*(an1 - an2)/pi
        anr1 = (eref - e(j))*LOG(ABS((eref-e(j))/dj))
        anr2 = (eref - e(j + 1))*LOG(ABS((eref-e(j+1))/dj))
        dver = ((aji(j+1) - aji(j))/dj)*(anr1 - anr2)/pi
        dvef = dvef + dve
        dverf = dverf + dver
      ENDDO
      v0 = ajref - dverf
      ajre = dvef + v0
      ajre = -ajre
   30 ror = rr/at13
      roi = ri/at13
      vor = ajre/const/rr**3./(1 + (const1/rr)**2.)
      voi = ajie/const/ri**3./(1. + (const2/ri)**2.)
 
      WRITE(102,1010)Elab, vor, ror, ar, voi, roi, ai, ajre, ajie, Az, 
     &               Amass, Ex
C       OUTPUTS: VOR, ROR, AR, VOI, ROI, AI
C
      Vlib(1) = vor
      Alib(1) = ar
      Rlib(1) = ror
      Vlib(2) = voi
      Alib(2) = ai
      Rlib(2) = roi
 
      Rcc = 1.3D0
 
 1010 FORMAT(2x,f9.3,1x,f9.3,1x,f9.3,1x,f9.3,1x,f9.3,1x,f9.3,1x,f9.3,7x,
     &       f9.3,1x,f9.3,7x,f9.3,1x,f9.3,5x,F9.3)
      RETURN
      END SUBROUTINE KUMAR_OMP
 
 
 
