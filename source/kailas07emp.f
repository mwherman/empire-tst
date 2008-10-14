      SUBROUTINE KUMAR_OMP(AMASS,AZ,EX,ELAB,RCC,Vlib,Rlib,Alib)
      IMPLICIT REAL*8 (A-H,O-Z)
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
        REAL*8 EX, ELAB, RCC, AMASS, AZ
        REAL*8 Alib(6), Rlib(6), Vlib(6)

        DIMENSION AJI(200), E(200)

        PI = 4. * ATAN(1.)

	DO i=1,6
          Alib(i)=0.d0
          Rlib(i)=0.d0
          Vlib(i)=0.d0
	ENDDO

        AT13 = AMASS ** (1./3.)
C       BAR IS THE COULOMB BARRIER ENERGY
        BAR = 1.44 * 2. * AZ / 1.5 / (4.**(1./3.) + AT13)
C       R2.4 SYSTEMATICS FOR REAL AND IMAGINARY PARTS OF POTENTIAL,
C       AR AND AI ARE THE DIFFUSENESS PARAMETERS FOR 
C       REAL AND IMAGINARY PARTS OF THE POTENTIAL
        R24R = 1.35 * AT13 + 2.55
        R24I = 1.35 * AT13 + 2.14
        AR = 0.76
        AI = 0.60
C       EREF IS THE REFERENCE ENERGY FOR NORMALISATION IN MAKING
C       DISPERSION RELATION CALCULATION
        EREF  = 140.0
        EREFC = EREF*AMASS / (AMASS + 4.)
C       AJREF IS THE VOLUME INTEGRAL AT THE REFERENCE ENERGY AND
C       IS CALCULATED USING THE EMPIRICAL RELATION GIVEN  BELOW
        AJREF = (224. - 0.98*EREFC/AMASS**0.184 + 2.57 * AZ/AT13)
     1             * (1. + (2.05 / AT13))
        AJREF = - AJREF
C       IMAGINARY PART SYSTEMATICS 
C       (REF: A. Shridahar et al.; Phys. Rev. C30 (1984) 1760.)
        AJII = 34.4 * (1. + 7.1 / AT13)
C       CALCULATION OF RADIUS PARAMETERS FOR REAL AND IMAGINARY
C       POTENTIALS BY FITTING R2.4 AND VOLUME INTEGRAL VALUES
C       AT E = 90 MEV
        CONST = PI / (3. * AMASS)
        CONST1 = PI*AR
        CONST2 = PI*AI
        EC = 90.*AMASS/(AMASS+4.)
        BETA = 0.1
        EBAR = BAR + EX
        AJRREF =  (224. - 0.98*EC / AMASS**0.184 + 2.57 * AZ / AT13)
     1          * (1. + (2.05 / AT13))
        AJIREF = AJII * (1. - EXP( - (EC - EBAR) * BETA))

        ROR = 1.0
        J = 0
 551    RR = ROR * AT13
        VOR = AJRREF / CONST / RR**3. / (1 + (CONST1 / RR)**2.)
        R24RC = RR + AR * LOG ((VOR - 2.4) / 2.4)
        DIFF = R24RC - R24R
        IF (J .GT. 200) GO TO 552
        IF (ABS(DIFF) .LE. 0.05) GO TO 552
        J = J + 1
        ROR = ROR + 0.005
        GO TO 551
 552    CONTINUE

        ROI = 1.2
        J = 0
 557    RI = ROI * AT13
        VOI = AJIREF / CONST/RI**3. / (1. + (CONST2 / RI)**2.)
        R24IC = RI + AI * LOG ((VOI - 2.4)/2.4)
        DIFF = R24IC - R24I
        IF (J .GT. 200) GO TO 558
        IF (ABS(DIFF) .LE. 0.05) GO TO 558
        J = J + 1
        ROI = ROI + 0.005
        GO TO 557
 558    CONTINUE

        CON = 35.5 - 6. *  AT13
        DEL = CON + BAR
        ECM = ELAB * AMASS / (AMASS + 4.)
        IF (ECM .LT. EX) ECM = EX
        ESQ = (ECM - EX) * (ECM - EX)
        AJIE = AJII * ESQ / (ESQ + (DEL * DEL))
        IF (ELAB .GT. 140.0) THEN
         AJRE = (224. - 0.98*ECM / AMASS**0.184 + 2.57 * AZ / AT13)*
     1          (1. + (2.05 / AT13))
         GO TO 820
        ENDIF

        N = 0
        EE = EX
        DO  I = 1 , 50
          N = N + 1
          E(I) = EE
C         ASSUME THE DISPERSION CORRECTION TO VANISH AT EREF.
          IF ( E(I) .GT. 140.0) GO TO 155
          ESQ = (E(I) - EX) * (E(I) - EX)
          AJI(I) = AJII* ESQ / (ESQ + (DEL * DEL))
          EE = EE + 3.00001
        END DO
 155    CONTINUE
        NO= N

        DVEF = 0.
        DVERF = 0.
        DO J = 1 , NO - 3
         DJ = E(J+1) - E(J)
         AN1 = (ELAB - E(J)) * LOG(ABS((ELAB - E(J)) / DJ))
         AN2 = (ELAB - E(J+1)) * LOG(ABS((ELAB - E(J+1)) / DJ))
         DVE = ((AJI(J+1) - AJI(J)) / DJ) * (AN1 - AN2) / PI
         ANR1 = (EREF - E(J)) * LOG(ABS((EREF - E(J)) / DJ))
         ANR2 = (EREF - E(J+1)) * LOG(ABS((EREF - E(J+1)) / DJ))
         DVER = ((AJI(J+1) - AJI(J)) / DJ) * (ANR1 - ANR2) / PI
         DVEF = DVEF + DVE
         DVERF = DVERF + DVER
        END DO
        V0 = AJREF - DVERF
        AJRE = DVEF + V0
        AJRE = -AJRE
 820    CONTINUE
        ROR = RR / AT13
        ROI = RI / AT13
        VOR = AJRE / CONST / RR**3. / (1 + (CONST1 / RR)**2.)
 2543   CONTINUE
        VOI = AJIE / CONST / RI**3. / ( 1. + (CONST2 / RI)**2.)

        WRITE (102,75) ELAB, VOR, ROR, AR, VOI, ROI, AI, AJRE, AJIE,
     1	               AZ, AMASS, EX
C       OUTPUTS: VOR, ROR, AR, VOI, ROI, AI
C       
        vlib(1) = VOR
	alib(1) = AR
	rlib(1) = ROR
        vlib(2) = VOI
	alib(2) = AI
	rlib(2) = ROI

	RCC = 1.3d0

  75    FORMAT (2x,f9.3,x,f9.3,x,f9.3,x,f9.3,x,f9.3,x,f9.3,x,f9.3,7x,
     1          f9.3,x,f9.3,7x,f9.3,x,f9.3,5x,F9.3)
      RETURN
      END
