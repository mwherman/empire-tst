      SUBROUTINE EMPIREDEGAS
C
C     P.Oblozinsky (Brookhaven National Lab), February 15, 2001
C     E. Betak (Inst. Phys. Bratislava), May 2002
C
C     Empiredegas couples the code Empire (M. Herman, Vienna),
C     and the code DEGAS (E. Betak, Bratislava) that is based
C     on the code PEGAS (E. Betak and P. Oblozinsky, Bratislava).
C
C     Empiredegas does the following:
C
C     1. Takes input data, needed by the code Degas, from Empire:
C     See: iz, in etc below.
C     g values: g = a*6/pi**2, if a is taken from Empire
C     g = A/13       if a is not available
C     g = A/val      if val specified in GDIV input,
C     for proton channel GDIVP can be used
C     Pairing energies: delz,deln copied from input.f
C
C     2. Transfers this input via common blocks /degasinput/ and
C     /degasinp/ into subroutine subdegas. Subdegas is the
C     original code Degas converted into subroutine.
C
C     3. Calls subdegas(specdegas,populdegas), where:
C
C     specdegas(particle,energy bin)      = spectra of n,p,g
C     populdegas(nucleus,energy bin,spin) = residual population
C     of CN, CN-n, CN-p
C     Note:
C     CN:   nucleus = 1
C     CN-n: nucleus = 2
C     CN-p: nucleus = 3
C
C     4. Puts results from subdegas into Empire arrays for spectra
C     and population distributions. Empire definitions:
C
C     cse(spectral energy bin,g/n/p,nucleus)  = cse(ie,0/1/2,1)
C     pop(exc energy binn,spin,parity,nucleus) =
C     = pop(ie,j,jpar,1)
C     = pop(ie,j,jpar,nres(1))
C     = pop(ie,j,jpar,nres(2))
C
C     Notes:
C     a) cse and pop are defined in dimension.h and global.h
C     b) in dimension.h one should be careful with ndex and ndlw
C     c) nucleus: 1 = CN, nres(1) = CN-n, nres(2) = CN-p
C     d) units of cse: mb/MeV
C     e) units of pop: mb/MeV
C
C     Relation between Empire and Degas arrays:
C
C     cse    (mb/MeV) = specdegas (mb/MeV)
C     pop    (mb/MeV) = populdegas/estepdegas  ! Continuum
C     popplv (mb)     = populdegas             ! Discrete levels
C
C     Note: populdegas is in units of mb rather than in mb/MeV
C
      INCLUDE 'dimension.h'
      PARAMETER(NDEXD = NDEX + 11)
      INCLUDE 'global.h'
C
C COMMON variables
C
      DOUBLE PRECISION ADEgas3, AKDegas, BDEgas(10, 2), BEProjdegas, 
     &                 BRArdegas(10, 10, 9), CJGsdegas, DDDegas(10), 
     &                 EEXcdegas0, ENDidegas(10, 10), ESTepdegas, 
     &                 GAQdegas, GGDegas(10), SPIdidegas(10, 10), 
     &                 XCDegas(9)
      INTEGER IACdegas, IATdegas, IEDegas, INDegas, IZCdegas, IZDegas, 
     &        KEYdegas0, NBRadegas(10), NEXddegas(10, 10), NOLedegas(9), 
     &        NUDidegas(10)
      REAL*8 JGSdegas
      CHARACTER*79 TITdegas
      COMMON /DEGASINP/ JGSdegas, EEXcdegas0, ESTepdegas, AKDegas, 
     &                  CJGsdegas, BEProjdegas, GAQdegas, ADEgas3, 
     &                  IEDegas, INDegas, IZDegas, IATdegas, IACdegas, 
     &                  IZCdegas, KEYdegas0, TITdegas
      COMMON /DEGASINPUT/ BRArdegas, ENDidegas, SPIdidegas, NEXddegas, 
     &                    BDEgas, GGDegas, DDDegas, XCDegas, NUDidegas, 
     &                    NBRadegas, NOLedegas
      COMMON /DEGASOUT/ SGRdegas
C
C Local variables
C
      DOUBLE PRECISION csemispoplv, deln(150), delz(98), ee0, GDIvp, 
     &                 popdiscrete, popminus, popplus, poptotal, 
     &                 poptotalall, poptotalcont, poptotallv, 
     &                 populdegas(3, NDEXD, 25), renorm, 
     &                 specdegas(3, NDEXD), spectotg, spectotn, 
     &                 spectotp, specundecayed, sumall, sumpopulee0, 
     &                 sumpopultot, tmp, totemis, renpop
      REAL FLOAT
      INTEGER i, ie, ie0, ie1, iemaxdiscrete, ii, iii, il, nnur, j, 
     &        jparity, jspin, ndexmaximum, ndlwmaximum, nexmax, nudim, 
     &        nnuc
      INTEGER INT, nextop
C
      DATA delz/0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 2.46, 0., 
     &     2.09, 0., 1.62, 0., 1.62, 0., 1.83, 0., 1.73, 0., 1.35, 0., 
     &     1.54, 0., 1.20, 0., 1.06, 0., 1.36, 0., 1.43, 0., 1.17, 0., 
     &     1.24, 0., 1.20, 0., 1.28, 0., 1.28, 0., 1.35, 0., 1.36, 0., 
     &     1.19, 0., 1.14, 0., 1.12, 0., 1.58, 0., 1.17, 0., 1.18, 0., 
     &     1.22, 0., 0.97, 0., 0.92, 0., 0.62, 0., 0.68, 0., 0.64, 0., 
     &     0.72, 0., 0.75, 0., 0.71, 0., 0.87, 0., 0.83, 0., 0.89, 0., 
     &     0.79, 0., 0.89, 0., 0.78, 0., 0.69, 0., 0.61, 0., 0.72, 0., 
     &     0.77/
C                                            ! Taken from input.f
      DATA deln/0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 2.67, 0., 
     &     1.80, 0., 1.67, 0., 1.86, 0., 2.04, 0., 1.64, 0., 1.44, 0., 
     &     1.54, 0., 1.30, 0., 1.27, 0., 1.29, 0., 1.41, 0., 1.50, 0., 
     &     1.50, 0., 1.43, 0., 1.88, 0., 1.47, 0., 1.57, 0., 1.46, 0., 
     &     0.93, 0., 0.72, 0., 1.12, 0., 1.29, 0., 0.94, 0., 1.24, 0., 
     &     1.25, 0., 1.14, 0., 1.32, 0., 1.15, 0., 1.24, 0., 1.43, 0., 
     &     1.09, 0., 1.20, 0., 1.04, 0., 0.70, 0., 0.85, 0., 0.76, 0., 
     &     0.92, 0., 0.99, 0., 1.10, 0., 0.92, 0., 0.73, 0., 0.70, 0., 
     &     0.87, 0., 0.61, 0., 0.69, 0., 0.55, 0., 0.40, 0., 0.73, 0., 
     &     0.58, 0., 0.86, 0., 1.13, 0., 0.84, 0., 0.79, 0., 0.82, 0., 
     &     0.71, 0., 0.41, 0., 0.38, 0., 0.67, 0., 0.61, 0., 0.78, 0., 
     &     0.67, 0., 0.67, 0., 0.79, 0., 0.60, 0., 0.57, 0., 0.49, 0., 
     &     0.43, 0., 0.50, 0., 0.39/
C
      ndexmaximum = NDEXD       ! Maximum energy bin allowed by Degas
C
      IF(NDLW.GT.25)THEN
         ndlwmaximum = 25        ! Maximum spin allowed by Degas
      ELSE
         ndlwmaximum = NDLW
      ENDIF
C
      WRITE(42, 99005)
      WRITE(42, *)'Output from Empire-Degas, version February 10, 2001'
      WRITE(42, *)'==================================================='
      WRITE(42, *)'- Printout of input data'
      WRITE(42, *)'- Output from Degas in its original format'
      WRITE(42, *)'- Output from Empire: Results transfered from Degas'
C
C     unit = 41: degasinput.dat, input via subroutine degasread,
C     to be used for test purposes only
C     unit = 42: degasresult.dat, output from Degas
C
C
C
C     Input parameters needed for Degas are taken from Empire:
C
      IZDegas = INT(Z(1))                       ! Z of compound nucleus (CN)
      INDegas = INT(A(1) - Z(1))                ! N of CN
      IF((AEJc(0).EQ.1.) .AND. ZEJc(0).EQ.0.)KEYdegas0 = 1
C     ! Incident neutron
      IF((AEJc(0).EQ.1.) .AND. ZEJc(0).EQ.1.)KEYdegas0 = 2
C     ! Incident proton
C
      EEXcdegas0 = EXCn      ! CN excitation energy (MeV)
      ESTepdegas = DE        ! Energy step (MeV)
      AKDegas = 100.0        ! Kalbach constant for matrix element (MeV**3)
      JGSdegas = XJLv(LEVtarg, 0)  ! Target g.s. spin
      CJGsdegas = XJLv(LEVtarg, 1) ! CN     g.s. spin
      BEProjdegas = 0.0      ! Binding of projectile if not n or p
      GAQdegas = 0.0         ! Quadrupole reaction (default=0.001)
C
      BDEgas(1, 1) = Q(0, 1) ! Neutron binding energy in CN
      BDEgas(1, 2) = Q(2, 1) ! Proton  binding energy in CN
C
      IF(GDIv.GT.1.0)THEN
         GGDegas(1) = A(1)/GDIv
         GGDegas(2) = A(2)/GDIv
         GGDegas(3) = A(3)/GDIv
      ELSE
         GGDegas(1) = A(1)/13.0
         GGDegas(2) = A(2)/13.0
         GGDegas(3) = A(3)/13.0
      ENDIF
      IF(GDIvp.GT.1.0)GGDegas(3) = A(3)/GDIvp
C
C     ggdegas(1) = 10.03     ! g-value for CN, if not specified A/13 used
C     ggdegas(2) =  9.98     ! g-value for CN-n
C     ggdegas(3) =  9.98     ! g-value for CN-p
C
      DDDegas(1) = delz(IZDegas) + deln(INDegas)     ! Pairing for CN
      DDDegas(2) = delz(IZDegas) + deln(INDegas - 1) ! Pairing for CN-n
      DDDegas(3) = delz(IZDegas - 1) + deln(INDegas) ! Pairing for CN-p
C
      IATdegas = INDegas + IZDegas
      IF(IATdegas.LE.0)STOP
      IACdegas = IATdegas
      IZCdegas = IZDegas
      ADEgas3 = FLOAT(IATdegas)**0.3333333
      IF(GAQdegas.EQ.0.)GAQdegas = 0.001
C
C
C     read(ir,20) in,iz,key0
C     N,Z of the compound nucleus, key of the projectile
C     in+iz = 0 ==> stop of the program
C
C     READ(IR,30) EEXC0,ESTEP,AK,JGS,CJGS,BEPROJ,GAQ
C     EXC. ENERGY (MEV), EN. STEP (MEV),
C     MATRIX ELEMENT CONSTANT (MEV**3),
C     G.S. SPIN OF THE TARGET,
C     G.S. SPIN OF THE COMPOSITE SYSTEM,
C     BINDING EN. OF PROJECTILE, IF NOT N OR P (MEV),
C     QUADRUPOLE FRACTION (DEFAULT=0.001)
C
C     READ(IR,45) B
C     BINDING ENERGIES FOR NEUTRONS (NUCLEI ACCORDING
C     THEIR KEYS :  0, 1, ... , 9),
C     LATER FOR PROTONS (0 TO 9)
C
C     READ(IR,45) GG
C     SINGLE-PARTICLE LEVEL DENSITIES (1/MEV)
C     BLANK LINE ON INPUT ==>  ALL G=A/13
C
C     READ(IR,45) DD
C     PAIRING ENERGIES (MEV)
C
C     READ(IR,46) NUDI
C     NUMBER OF DISCRETE LEVELS ENTERED FOR
C     EACH NUCLEUS (PRESENT LIMIT: .LE. 10)
C
C     READ(IR,46) NBRA
C     NUMBER OF DISCRETE LEVELS SUPPLIED FOR
C     EACH NUCLEUS TOGETHER WITH THEIR BRANCHING RATIOS
C     (LIMIT: .LE. NUDI)
C
C
C     READ(IR,47) ENDI(J,I), SPIDI(J,I), NEXD(J,I)
C     ENERGY OF DISCRETE LEVEL,
C     PIN OF DISCRETE LEVEL,
C     EXCITON NUMBER OF DISCRETE LEVEL
C     (INPUT LINES ARE READ IN IN THE ORDER
C     OF INCREASING NUCLEAR KEY NUMBERS (0 TO 9),
C     WITHIN EACH NUCLEUS THEY ARE ORDERED
C     FROM THE GROUND STATE TO THE
C     HIGHEST ENERGY; NEGATIVE EXCITON NUMBER
C     IMPLIES DISTRIBUTION OF EXCITON NUMBERS)
C     NEXD .EQ. -1 ==> SEMI-EQUILIBRIUM DISTRIBUTION
C     OF EXCITON NUMBERS
C     NEXD .EQ. -2 ==> MAXIMAL COLLECTIVITY ASSUMPTION
C
C     IF (I.LE.NBRA(J)) READ(IR,48) (nole(ii), xc(ii), ii=1,9)
C     IF (I.LE.NBRA)
C     NUMBER OF LEVEL, WHERE THE GAMMA GOES
C     (FROM CONSIDERED LEVEL I);
C     BRANCHING RATIO
C     TRANSITIONS TO LEVELS NOT GIVEN IN THIS LINE
C     ARE CONSIDERED WITH ZERO INTENSITY
C     BLANK CARD MEANS NO GAMMA TRANSITIONS ALLOWED,
C     I.E. GROUND STATE OR ISOMER
C
      DO j = 1, 10
         nudim = NUDidegas(j)
         IF(nudim.NE.0)THEN
            DO i = 1, nudim
C
C              READ(IR,47) ENDI(J,I), SPIDI(J,I), NEXD(J,I)
C              IF (I.LE.NBRA(J)) READ(IR,48) (C     IF (I.LE.NBRA)
C
               DO ii = 1, 9
                  IF(NOLedegas(ii).NE.0)THEN
                     ie = NOLedegas(ii)
                     BRArdegas(j, i, ie) = XCDegas(ii)
                  ENDIF
               ENDDO
            ENDDO
         ENDIF
      ENDDO
C
C     call degasread     ! Reads original Degas input, for testing only
C     CALL DEGASWRITE    ! For testing only
C
C     Call subdegas
C
      CALL SUBDEGAS(specdegas, populdegas)
C
C     Residual populations
      sumall = 0.0
      DO nnur = 1, 3  ! 1:CN, 2:CN-n, 3:CN-p
         sumpopultot = 0.0
         DO ie0 = 1, ndexmaximum         ! Energy bin
            ee0 = ESTepdegas*ie0         ! Actual energy
            sumpopulee0 = 0.0
            DO j = 1, ndlwmaximum        ! Spin
               sumpopulee0 = sumpopulee0 + populdegas(nnur, ie0, j)
            ENDDO
            sumpopultot = sumpopultot + sumpopulee0
         ENDDO
         sumall = sumall + sumpopultot
      ENDDO
      WRITE(42, *)'Sum of all populations (mb):', sumall
C
C     Puts results from subdegas into Empire's arrays for spectra and
C     residual population, where
C
C     cse(spectral energy bin,gamma,nucleus) = cse(ie, 0, 1)
C     pop(excitation energy bin,spin,parity,nucleus) =
C     = pop(ie,j,jpar,1)
C     = pop(ie,j,jpar,nres(1))
C     = pop(ie,j,jpar,nres(2))
C
C     Normalization to initial CN population taken from Empire:
C
      poptotal = 0.0
      DO jspin = 1, ndlwmaximum
         DO jparity = 1, 2
            poptotal = poptotal + POP(NEX(1), jspin, jparity, 1)
         ENDDO
      ENDDO
      renorm = poptotal/SGRdegas
      WRITE(42, 99005)
      WRITE(42, *)' Results from Degas transfered to Empire '
      WRITE(42, *)'========================================='
C
C     Spectra from Degas normalized and transferred to Empire:
C
      spectotg = 0.0
      spectotn = 0.0
      spectotp = 0.0
      nexmax = EXCn/DE
      DO ie0 = 1, nexmax      ! Spectral energy bin
C        Gamma spectrum
         IF(IDNa(5, 4).EQ.1)THEN
            CSE(ie0 + 1, 0, 1) = CSE(ie0 + 1, 0, 1) + specdegas(3, ie0)
     &                           *renorm
            AUSpec(ie0 + 1, 0) = AUSpec(ie0 + 1, 0) + specdegas(3, ie0)
     &                           *renorm
            spectotg = spectotg + CSE(ie0, 0, 1)*ESTepdegas
         ENDIF
C        Neutron spectrum
         IF(IDNa(2, 4).EQ.1)THEN
            CSE(ie0 + 1, 1, 1) = CSE(ie0 + 1, 1, 1) + specdegas(1, ie0)
     &                           *renorm
            AUSpec(ie0 + 1, 1) = AUSpec(ie0 + 1, 1) + specdegas(1, ie0)
     &                           *renorm
            spectotn = spectotn + CSE(ie0, 1, 1)*ESTepdegas
         ENDIF
C        Proton spectrum
         IF(IDNa(4, 4).EQ.1)THEN
            CSE(ie0 + 1, 2, 1) = CSE(ie0 + 1, 2, 1) + specdegas(2, ie0)
     &                           *renorm
            AUSpec(ie0 + 1, 2) = AUSpec(ie0 + 1, 2) + specdegas(2, ie0)
     &                           *renorm
            spectotp = spectotp + CSE(ie0, 2, 1)*ESTepdegas
         ENDIF
      ENDDO
C-----total DEGAS emission accepted in calculations (note matrix IDNa)
      totemis = spectotg*IDNa(5, 4) + spectotn*IDNa(2, 4)
     &          + spectotp*IDNa(4, 4)
C-----Renormalize Empire fusion distribution to account for loss due to
C-----accepted DEGAS emission (note that there is no competition with
C-----other reaction mechanisms)
      renpop = (poptotal - totemis)/poptotal
      DO jspin = 1, ndlwmaximum
         DO jparity = 1, 2
            POP(NEX(1), jspin, jparity, 1)
     &         = POP(NEX(1), jspin, jparity, 1)*renpop
         ENDDO
      ENDDO
      WRITE(42, 99005)
      WRITE(42, *)'1) Spectra (mb/MeV)'
      WRITE(42, *)'   Spectral energy, g, n, p'
      DO ie0 = 1, nexmax
         ee0 = ESTepdegas*(ie0 - 1)
         WRITE(42, 99002)ee0, CSE(ie0, 0, 1), CSE(ie0, 1, 1), 
     &                   CSE(ie0, 2, 1)
      ENDDO
      WRITE(42, 99005)
      specundecayed = poptotal - spectotg - spectotn - spectotp
      WRITE(42, *)' Initial CN population Degas  (mb):', sumall
      WRITE(42, 99005)
      WRITE(42, *)' Initial CN population Empire (mb):', poptotal
      WRITE(42, *)' Integral preeq gamma   (mb):', spectotg
      WRITE(42, *)' Integral preeq neutron (mb):', spectotn
      WRITE(42, *)' Integral preeq proton  (mb):', spectotp
      WRITE(42, *)' Integral undecayed     (mb):', specundecayed
C
C     Residual population from Degas normalized and transferred to Empire.
C     This is done for CN, CN-n and CN-p final nuclei, defined by
C     nnur =1, nres(1), nres(2),
C     sunbject to green light from the 'model usage matrix' IDNa.
C     - We first treat residual population of discrete levels.
C       (blocked in the current version)                      
C     - This is followed by residual population for continuum.
C
C     CN, discrete levels
C-----population has been blocked by commenting some lines below
C
      IF(IDNa(5, 4).EQ.1)THEN
         nnuc = 1
C        WRITE(6, *)'nnuc=', nnuc
C        WRITE(6, *)'ELV=', ELV(NLV(nnuc), nnuc)
C        WRITE(6, *)'NEX =', NEX(nnuc)
C        WRITE(6, *)'ndexmaximum=', ndexmaximum
         iemaxdiscrete = ELV(NLV(nnuc), nnuc)/DE + 0.5
C        Number of bins in discrete region
         iemaxdiscrete = iemaxdiscrete - 1
C        WRITE(6, *)'iemaxdiscrete=', iemaxdiscrete
         popdiscrete = 0.0
         DO ie0 = 1, iemaxdiscrete
            DO jspin = 1, ndlwmaximum
               popdiscrete = popdiscrete + populdegas(1, ie0, jspin)
     &                       *renorm
            ENDDO
C           WRITE(6, *)'populdegas to discret=', ie0, popdiscrete
         ENDDO
         popdiscrete = popdiscrete*ESTepdegas
         popdiscrete = popdiscrete/NLV(nnuc)
C        DO il = 1, NLV(nnuc)
C           POPlv(il, nnuc) = POPlv(il, nnuc) + popdiscrete
C-----------store population of discrete levels for recoils' calculations
C           REClev(il, 0) = REClev(il, 0) + popdiscrete
C        ENDDO           ! Even population of discrete levels
C        WRITE(42, 99005)
C        WRITE(42, *)'2) Residual popul for nnuc:', nnuc
C        WRITE(42, *)'   Discrete level, energy, population'
C        DO il = 1, NLV(nnuc)
C           WRITE(42, 99003)il, ELV(il, nnuc), POPlv(il, nnuc)
C        ENDDO
      ENDIF
C
C-----CN, continuum:
C
      IF(IDNa(5, 4).EQ.1)THEN
         nnuc = 1
         ie1 = iemaxdiscrete + 1
         nextop = ie1 + NEX(1) - 2 !highest bin excluding top CN energy
         DO ie0 = ie1, nextop     ! over excitation energy
            DO jspin = 1, ndlwmaximum  ! over spin (parity populated evenly)
               tmp = populdegas(1, ie0, jspin)*0.5*renorm/DE
               POP(ie0 - ie1 + 1, jspin, 1, nnuc) = tmp
               POP(ie0 - ie1 + 1, jspin, 2, nnuc) = tmp
            ENDDO
         ENDDO
         WRITE(42, 99005)
         WRITE(42, *)'Residual population for nnuc:', nnuc
         WRITE(42, *)'Exc energy, 4x spin, +/-, sum'
         DO ie0 = 1, nexmax
            ee0 = ESTepdegas*(ie0 + iemaxdiscrete)
            popplus = 0.0
            popminus = 0.0
            DO jspin = 1, NDLW
               popplus = popplus + POP(ie0, jspin, 1, nnuc)*DE
               popminus = popminus + POP(ie0, jspin, 2, nnuc)*DE
            ENDDO
            WRITE(42, 99002)ee0, POP(ie0, 1, 1, nnuc), 
     &                      POP(ie0, 2, 1, nnuc), POP(ie0, 3, 1, nnuc), 
     &                      POP(ie0, 4, 1, nnuc), popplus
            WRITE(42, 99002)ee0, POP(ie0, 1, 2, nnuc), 
     &                      POP(ie0, 2, 2, nnuc), POP(ie0, 3, 2, nnuc), 
     &                      POP(ie0, 4, 2, nnuc), popminus
         ENDDO
      ENDIF
C
C-----CN-n, discrete levels
C-----population has been blocked by some commenting lines below
C
      IF(IDNa(1, 4).EQ.1)THEN
         nnur = NREs(1)
C--------Number of bins in discrete region
         iemaxdiscrete = ELV(NLV(nnur), nnur)/DE + 0.5
         iemaxdiscrete = iemaxdiscrete - 1
         popdiscrete = 0.0
         DO ie0 = 1, iemaxdiscrete
            DO jspin = 1, ndlwmaximum
               popdiscrete = popdiscrete + populdegas(2, ie0, jspin)
     &                       *renorm
            ENDDO
         ENDDO
C--------Even population of discrete levels
         popdiscrete = popdiscrete/NLV(nnur)
C        DO il = 1, NLV(nnur)
C           POPlv(il, nnur) = POPlv(il, nnur) + popdiscrete
C-----------store population of discrete levels for recoils' calculations
C           REClev(il, 1) = REClev(il, 1) + popdiscrete
C-----------Add isotropic DEGAS contribution to direct ang. distributions
C           popdiscrete = popdiscrete/4.0/PI
C           DO na = 1, NDANG
C              CSAlev(na, il, 1) = CSAlev(na, il, 1) + popdiscrete
C           ENDDO
C        ENDDO
C        WRITE(42, 99005)
C        WRITE(42, *)'2) Residual popul for nnur:', nnur
C        WRITE(42, *)'   Discrete level, energy, population'
C        DO il = 1, NLV(nnur)
C           WRITE(42, 99003)il, ELV(il, nnur), POPlv(il, nnur)
C        ENDDO
      ENDIF
C
C-----CN-n, continuum:
C
      IF(IDNa(2, 4).EQ.1)THEN
         nnur = NREs(1)
         ie1 = iemaxdiscrete + 1
         DO ie0 = ie1, nexmax     ! Excitation energy bin
            DO jspin = 1, ndlwmaximum  ! Spin (parities populated evenly)
               tmp = populdegas(2, ie0, jspin)*0.5*renorm/DE
               POP(ie0 - ie1 + 1, jspin, 1, nnur)
     &            = POP(ie0 - ie1 + 1, jspin, 1, nnur) + tmp
               POP(ie0 - ie1 + 1, jspin, 2, nnur)
     &            = POP(ie0 - ie1 + 1, jspin, 2, nnur) + tmp
            ENDDO
         ENDDO
         WRITE(42, 99005)
         WRITE(42, *)'Residual population for nnur:', nnur
         WRITE(42, *)'Exc energy, 4x spin, +/-, sum'
         DO ie0 = 1, nexmax
            ee0 = ESTepdegas*(ie0 + iemaxdiscrete)
            popplus = 0.0
            popminus = 0.0
            DO jspin = 1, NDLW
               popplus = popplus + POP(ie0, jspin, 1, nnur)*DE
               popminus = popminus + POP(ie0, jspin, 2, nnur)*DE
            ENDDO
            WRITE(42, 99002)ee0, POP(ie0, 1, 1, nnur), 
     &                      POP(ie0, 2, 1, nnur), POP(ie0, 3, 1, nnur), 
     &                      POP(ie0, 4, 1, nnur), popplus
            WRITE(42, 99002)ee0, POP(ie0, 1, 2, nnur), 
     &                      POP(ie0, 2, 2, nnur), POP(ie0, 3, 2, nnur), 
     &                      POP(ie0, 4, 2, nnur), popminus
         ENDDO
      ENDIF
C-----
C-----CN-p, discrete levels
C-----population has been blocked by commenting lines below
C-----
      IF(IDNa(3, 4).EQ.1)THEN
         nnur = NREs(2)
C--------Number of bins in discrete region
         iemaxdiscrete = ELV(NLV(nnur), nnur)/DE + 0.5
         iemaxdiscrete = iemaxdiscrete - 1
         popdiscrete = 0.0
         DO ie0 = 1, iemaxdiscrete
            DO jspin = 1, ndlwmaximum
               popdiscrete = popdiscrete + populdegas(3, ie0, jspin)
     &                       *renorm
            ENDDO
         ENDDO
         popdiscrete = popdiscrete*ESTepdegas
         popdiscrete = popdiscrete/NLV(nnur)
C        DO il = 1, NLV(nnur)
C           POPlv(il, nnur) = POPlv(il, nnur) + popdiscrete
C-----------store population of discrete levels for recoils' calculations
C           REClev(il, 2) = REClev(il, 2) + popdiscrete
C-----------Add isotropic DEGAS contribution to direct ang. distributions
C           popdiscrete = popdiscrete/4.0/PI
C           DO na = 1, NDANG
C              CSAlev(na, il, 2) = CSAlev(na, il, 2) + popdiscrete
C           ENDDO
C        ENDDO        ! Even population of discrete levels
C        WRITE(42, 99005)
C        WRITE(42, *)'2) Residual popul for nnur:', nnur
C        WRITE(42, *)'   Discrete level, energy, population'
C        DO il = 1, NLV(nnur)
C           WRITE(42, 99003)il, ELV(il, nnur), POPlv(il, nnur)
C        ENDDO
      ENDIF
C-----
C-----CN-p, continuum:
C-----
      IF(IDNa(4, 4).EQ.1)THEN
         nnur = NREs(2)
         ie1 = iemaxdiscrete + 1
         DO ie0 = ie1, nexmax     ! Excitation energy bin
            DO jspin = 1, ndlwmaximum  ! Spin (parities populated evenly)
               tmp = populdegas(3, ie0, jspin)*0.5*renorm/DE
               POP(ie0 - ie1 + 1, jspin, 1, nnur)
     &            = POP(ie0 - ie1 + 1, jspin, 1, nnur) + tmp
               POP(ie0 - ie1 + 1, jspin, 2, nnur)
     &            = POP(ie0 - ie1 + 1, jspin, 2, nnur) + tmp
            ENDDO
         ENDDO
         WRITE(42, 99005)
         WRITE(42, *)'Residual population for nnur:', nnur
         WRITE(42, *)'Exc energy, 4x spin, +/-, sum'
         DO ie0 = 1, nexmax
            ee0 = ESTepdegas*(ie0 + iemaxdiscrete)
            popplus = 0.0
            popminus = 0.0
            DO jspin = 1, NDLW
               popplus = popplus + POP(ie0, jspin, 1, nnur)*DE
               popminus = popminus + POP(ie0, jspin, 2, nnur)*DE
            ENDDO
            WRITE(42, 99002)ee0, POP(ie0, 1, 1, nnur), 
     &                      POP(ie0, 2, 1, nnur), POP(ie0, 3, 1, nnur), 
     &                      POP(ie0, 4, 1, nnur), popplus
            WRITE(42, 99002)ee0, POP(ie0, 1, 2, nnur), 
     &                      POP(ie0, 2, 2, nnur), POP(ie0, 3, 2, nnur), 
     &                      POP(ie0, 4, 2, nnur), popminus
         ENDDO
      ENDIF
C-----
C-----As a next step in creating residual population, we check new
C-----CN population integral:
C-----
      poptotallv = 0.0
      DO il = 1, NLV(1)
         poptotallv = poptotallv + POPlv(il, 1)
      ENDDO
      poptotalcont = 0.0
      DO ie0 = 1, nexmax
         DO jspin = 1, ndlwmaximum
            DO jparity = 1, 2
               poptotalcont = poptotalcont + POP(ie0, jspin, jparity, 1)
     &                        *DE
            ENDDO
         ENDDO
      ENDDO
      poptotalall = poptotallv + poptotalcont
      WRITE(42, 99005)
      WRITE(42, *)'  Summary of CN population:'
      WRITE(42, 99005)
      WRITE(42, *)' Excitation energy         (MeV)=', EXCn
      WRITE(42, *)' Initial population Degas  (mb) =', sumall
      WRITE(42, 99005)
      WRITE(42, *)' Initial population Empire (mb) =', poptotal
      WRITE(42, *)' Preeq gamma emission      (mb) =', spectotg
      WRITE(42, *)' Preeq neutron emission    (mb) =', spectotn
      WRITE(42, *)' Preeq proton emission     (mb) =', spectotp
      WRITE(42, *)' New pop Empire discrete   (mb) =', poptotallv
      WRITE(42, *)' New pop Empire continuum  (mb) =', poptotalcont
      WRITE(42, *)' New pop Empire all        (mb) =', poptotalall
C-----
C-----Now we update CSEmis(0,1), where 0 = g, 1 = CN
C-----CSEmis(1,1), where 1 = n, 1 = CN
C-----CSEmis(2,1), where 2 = p, 1 = CN:
C-----
C-----Gamma
      csemispoplv = 0.0
      DO il = 1, NLV(1)
         csemispoplv = csemispoplv + POPlv(il, 1)
      ENDDO
      CSEmis(0, 1) = CSEmis(0, 1) + spectotg*IDNa(5, 4)
      WRITE(42, 99005)
      WRITE(42, *)'CSEmis(0,1), gamma from CN in mb:'
      WRITE(42, *)'Discrete =', csemispoplv
      WRITE(42, *)'All      =', CSEmis(0, 1)
C-----
C-----Neutrons
C-----
      nnur = NREs(1)
      csemispoplv = 0.0
      DO il = 1, NLV(nnur)
         csemispoplv = csemispoplv + POPlv(il, nnur)
      ENDDO
      CSEmis(1, 1) = CSEmis(1, 1) + spectotn*IDNa(2, 4)
      WRITE(42, 99005)
      WRITE(42, *)'CSEmis(1,1), neutrons from CN in mb:'
      WRITE(42, *)'Discrete =', csemispoplv
      WRITE(42, *)'All      =', CSEmis(1, 1)
C-----
C-----Protons
C-----
      nnur = NREs(2)
      csemispoplv = 0.0
      DO il = 1, NLV(nnur)
         csemispoplv = csemispoplv + POPlv(il, nnur)
      ENDDO
      CSEmis(2, 1) = CSEmis(2, 1) + spectotp*IDNa(4, 4)
      WRITE(42, 99005)
      WRITE(42, *)'CSEmis(2,1), protons from CN in mb:'
      WRITE(42, *)'Discrete =', csemispoplv
      WRITE(42, *)'All      =', CSEmis(2, 1)
C-----Summary of CN energy bins
      WRITE(42, 99005)
      WRITE(42, *)'Summary of CN energy bins:'
      WRITE(42, 99005)
C-----Number of bins in CN discrete region
      iemaxdiscrete = ELV(NLV(1), 1)/DE + 0.5
      iemaxdiscrete = iemaxdiscrete - 1
      WRITE(42, *)'Eexc  (MeV) =', EXCn
      WRITE(42, *)'Estep (MeV) =', DE
      WRITE(42, *)'Max CN discrete level =', NLV(1), ELV(NLV(1), 1)
      WRITE(42, *)'Max CN bin in discrete region=', iemaxdiscrete
      WRITE(42, *)'Max CN bin from Empire=', NEX(1)
      DO ie0 = 1, NEX(1)
         WRITE(42, *)'Exc energy from Empire=', ie0, EX(ie0, 1)
      ENDDO
      WRITE(42, *)' '
      ie1 = iemaxdiscrete + 1
      DO ie0 = ie1, nexmax
         iii = ie0 - ie1 + 1
         ee0 = ESTepdegas*(iii + iemaxdiscrete)
         IF(ee0.LT.(EXCn + DE))WRITE(42, *)'Exc energy from Degas = ', 
     &                               iii, ee0
      ENDDO
99001 FORMAT(1x, f5.2, 5E9.2, 2E11.4)
99002 FORMAT(1x, f5.2, 5E10.3, 2E11.4)
99003 FORMAT(1x, i4, f7.3, 5E11.4, 2E11.4)
99004 FORMAT(1x, f5.2, 1x, 4(f9.3, 1x))
99005 FORMAT(/)
      END               ! empiredegas
C
C
      SUBROUTINE DEGASREAD
C-----
C-----Reads input for Degas in its original format.
C-----To be used for testing purposes only. If used, then all input via
C-----Empire will be replaced by data taken from degasinput.dat file.
C-----
      IMPLICIT REAL*8(A - H, O - Z)
C
C COMMON variables
C
      REAL*8 A3, AK, B(10, 2), BEProj, BRAr(10, 10, 9), CJGs, DD(10), 
     &       EEXc0, ENDi(10, 10), ESTep, GAQ, GG(10), SPIdi(10, 10), 
     &       XC(9)
      INTEGER IAC, IAT, IE, IN, IZ, IZC, KEY0, NBRa(10), NEXd(10, 10), 
     &        NOLe(9), NUDi(10)
      REAL*8 JGS
      CHARACTER*79 TIT
      COMMON /DEGASINP/ JGS, EEXc0, ESTep, AK, CJGs, BEProj, GAQ, A3, 
     &                  IE, IN, IZ, IAT, IAC, IZC, KEY0, TIT
      COMMON /DEGASINPUT/ BRAr, ENDi, SPIdi, NEXd, B, GG, DD, XC, NUDi, 
     &                    NBRa, NOLe
C
C Local variables
C
      REAL FLOAT
C     Capote 05/2001 to avoid compiler warnings
      INTEGER i, ii, ir, j, nudim
C     INTEGER i , ii , ir , iw , j , nudim
C     REAL*8 pi
C
C     KEYS:    0-GAMMA,   1-N,   2-P,   3-D,   4-T,   5-HE-3,   6-ALPHA
C
C     THE CHAIN OF THE NUCLEI WITHIN THE REACTION IS DENOTED VIA II0
C
C     0
C     CN(A,Z)
C     /       \
C     1             2
C     (A-1,Z)       (A-1,Z-1)
C     /       \      /      \
C     3            4             5
C     (A-2,Z)      (A-2,Z-1)     (A-2,Z-2)
C     /      \      /      \       /     \
C     6           7             8             9
C     (A-3,Z)     (A-3,Z-1)     (A-3,Z-2)     (A-3,Z-3)
C     /       \     /      \      /       \     /     \
C
C     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C     Capote 05/2001 to avoid compiler warnings
C     DATA BRAr/900*0./ , pi/3.141593/ , ir , iw/41 , 42/
      DATA BRAr/900*0./, ir/41/
C
C     open(5,file='input',status='old')
C     open(6,file='output',status='new')
C
C
C     REACTION SPECIFICATION: Input block from Degas
C
      ir = 41
      READ(ir, 99001)TIT
99001 FORMAT(1X, a79)
C     (FMT 1X,79H)   TITLE CARD OF THE REACTION
      READ(ir, 99002)IN, IZ, KEY0
99002 FORMAT(4I5, f10.3)
C     (FMT 3I4)      N, Z OF THE COMPOSITE NUCLEUS, KEY OF PROJECTILE
C     IN+IZ = 0 ==> STOP OF THE PROGRAM
      IAT = IN + IZ
      IF(IAT.LE.0)STOP
      IAC = IAT
      IZC = IZ
      A3 = FLOAT(IAT)**0.3333333
      READ(ir, 99003)EEXc0, ESTep, AK, JGS, CJGs, BEProj, GAQ
99003 FORMAT(8F8.2)
C-----(FMT 7F8.)     EXC. ENERGY (MEV), EN. STEP (MEV),
C-----MATRIX ELEMENT CONSTANT (MEV**3),
C-----G.S. SPIN OF THE TARGET,
C-----G.S. SPIN OF THE COMPOSITE SYSTEM,
C-----BINDING EN. OF PROJECTILE, IF NOT N OR P (MEV),
C-----QUADRUPOLE FRACTION (DEFAULT=0.001)
      IF(GAQ.EQ.0.)GAQ = 0.001
      READ(ir, 99006)B
C-----(FMT 2*(10F6.))  BINDING ENERGIES FOR NEUTRONS (NUCLEI ACCORDING
C-----THEIR KEYS :  0, 1, ... , 9),
C-----LATER FOR PROTONS (0 TO 9)
      READ(ir, 99006)GG
C-----(FMT 10F6.)    SINGLE-PARTICLE LEVEL DENSITIES (1/MEV)
C-----BLANK LINE ON INPUT ==>  ALL G=A/13
      READ(ir, 99006)DD
C-----(FMT 10F6.)    PAIRING ENERGIES (MEV)
      READ(ir, 99007)NUDi
C-----(FMT 10I6)     NUMBER OF DISCRETE LEVELS ENTERED FOR
C-----EACH NUCLEUS
C-----(PRESENT LIMIT: .LE. 10)
      READ(ir, 99007)NBRa
C-----(FMT 10I6)     NUMBER OF DISCRETE LEVELS SUPPLIED FOR
C-----EACH NUCLEUS TOGETHER WITH THEIR BRANCHING RATIOS
C-----(LIMIT: .LE. NUDI)
      DO j = 1, 10
         nudim = NUDi(j)
         IF(nudim.NE.0)THEN
            DO i = 1, nudim
               READ(ir, 99004)ENDi(j, i), SPIdi(j, i), NEXd(j, i)
99004          FORMAT(2F6.2, i6)
C              (FMT 2F6., I6)  ENERGY OF DISCRETE LEVEL,
C              SPIN OF DISCRETE LEVEL,
C              EXCITON NUMBER OF DISCRETE LEVEL
C              (INPUT LINES ARE READ IN IN THE ORDER
C              OF INCREASING NUCLEAR KEY NUMBERS (0 TO 9),
C              WITHIN EACH NUCLEUS THEY ARE ORDERED
C              FROM THE GROUND STATE TO THE
C              HIGHEST ENERGY; NEGATIVE EXCITON NUMBER
C              IMPLIES DISTRIBUTION OF EXCITON NUMBERS)
C              NEXD .EQ. -1 ==> SEMI-EQUILIBRIUM DISTRIBUTION
C              OF EXCITON NUMBERS
C              NEXD .EQ. -2 ==> MAXIMAL COLLECTIVITY ASSUMPTION
C
               IF(i.LE.NBRa(j))READ(ir, 99005)
     &                              (NOLe(ii), XC(ii), ii = 1, 9)
99005          FORMAT(9(i2, f6.3))
C              IF (I.LE.NBRA)
C              (FMT 9(I2,F6.3))NUMBER OF LEVEL, WHERE THE GAMMA GOES
C              (FROM CONSIDERED LEVEL I);
C              BRANCHING RATIO
C              TRANSITIONS TO LEVELS NOT GIVEN IN THIS LINE
C              ARE CONSIDERED WITH ZERO INTENSITY
C              BLANK CARD MEANS NO GAMMA TRANSITIONS ALLOWED,
C              I.E. GROUND STATE OR ISOMER
               DO ii = 1, 9
                  IF(NOLe(ii).NE.0)THEN
                     IE = NOLe(ii)
                     BRAr(j, i, IE) = XC(ii)
                  ENDIF
               ENDDO
            ENDDO
         ENDIF
      ENDDO
99006 FORMAT(10F6.2)
99007 FORMAT(10I6)
C-----
C-----INPUT PARAMETERS READ IN
C-----
      END
C
C
      SUBROUTINE DEGASWRITE
C-----
C-----Writes all input data needed by Degas.
C-----For testing purposes only.
C-----
      IMPLICIT REAL*8(A - H, O - Z)
C
C COMMON variables
C
      REAL*8 A3, AK, B(10, 2), BEProj, BRAr(10, 10, 9), CJGs, DD(10), 
     &       EEXc0, ENDi(10, 10), ESTep, GAQ, GG(10), SPIdi(10, 10), 
     &       XC(9)
      INTEGER IAC, IAT, IE, IN, IZ, IZC, KEY0, NBRa(10), NEXd(10, 10), 
     &        NOLe(9), NUDi(10)
      REAL*8 JGS
      CHARACTER*79 TIT
      COMMON /DEGASINP/ JGS, EEXc0, ESTep, AK, CJGs, BEProj, GAQ, A3, 
     &                  IE, IN, IZ, IAT, IAC, IZC, KEY0, TIT
      COMMON /DEGASINPUT/ BRAr, ENDi, SPIdi, NEXd, B, GG, DD, XC, NUDi, 
     &                    NBRa, NOLe
C
C Local variables
C
      REAL*8 bbb
      INTEGER i, iw, j, nudim
C
99001 FORMAT(1X, a79)
      iw = 42
      WRITE(iw, 99006)
      WRITE(iw, *)' Printout of input data by degaswrite:'
      WRITE(iw, 99002)IN, IZ, KEY0
99002 FORMAT(4I5, f10.3)
      WRITE(iw, 99003)EEXc0, ESTep, AK, JGS, CJGs, BEProj, GAQ
99003 FORMAT(8F8.2)
      WRITE(iw, 99007)B    ! binding energies
      WRITE(iw, 99007)GG   ! single-particle level density g
      WRITE(iw, 99007)DD   ! pairing energies
      WRITE(iw, 99008)NUDi ! number of discrete levels
      WRITE(iw, 99008)NBRa ! number of branchings
      WRITE(iw, 99006)
      DO j = 1, 10
         nudim = NUDi(j)  ! j=1 means comp nucleus, etc
         IF(nudim.NE.0)THEN
            DO i = 1, nudim ! i=1 means g.s. level, etc
               WRITE(iw, 99004)ENDi(j, i), SPIdi(j, i), NEXd(j, i)
99004          FORMAT(2F6.2, i6)
               IF(i.LE.NBRa(j))THEN
                  DO IE = 1, 9 ! ie=1 means deay goes to g.s., etc
                     bbb = BRAr(j, i, IE)
                     IF(IE.GT.0 .AND. bbb.GT.0.)WRITE(iw, 99005)IE, bbb
99005                FORMAT(9(i2, f6.3))
                  ENDDO
               ENDIF
            ENDDO
            WRITE(iw, 99006)
         ENDIF
      ENDDO
99006 FORMAT(/)
99007 FORMAT(10F6.2)
99008 FORMAT(10I6)
      END
C
C
      SUBROUTINE SUBDEGASTEST
      IMPLICIT REAL*8(A - H, O - Z)
C
C COMMON variables
C
      REAL*8 ADEgas3, AKDegas, BDEgas(10, 2), BEProjdegas, 
     &       BRArdegas(10, 10, 9), CJGsdegas, DDDegas(10), EEXc0degas, 
     &       ENDidegas(10, 10), ESTepdegas, GAQdegas, GGDegas(10), 
     &       SPIdidegas(10, 10), XCDegas(9)
      INTEGER IACdegas, IATdegas, IEDegas, INDegas, IZCdegas, IZDegas, 
     &        KEYdegas0, NBRadegas(10), NEXddegas(10, 10), NOLedegas(9),
     &        NUDidegas(10)
      REAL*8 JGSdegas
      CHARACTER*79 TITdegas
      COMMON /DEGASINP/ JGSdegas, EEXc0degas, ESTepdegas, AKDegas, 
     &                  CJGsdegas, BEProjdegas, GAQdegas, ADEgas3, 
     &                  IEDegas, INDegas, IZDegas, IATdegas, IACdegas, 
     &                  IZCdegas, KEYdegas0, TITdegas
      COMMON /DEGASINPUT/ BRArdegas, ENDidegas, SPIdidegas, NEXddegas, 
     &                    BDEgas, GGDegas, DDDegas, XCDegas, NUDidegas, 
     &                    NBRadegas, NOLedegas
C
C Local variables
C
      REAL*8 a3, ak, b(10, 2), bbb, beproj, brar(10, 10, 9), cjgs, 
     &       dd(10), eexc0, endi(10, 10), estep, gaq, gg(10), 
     &       spidi(10, 10), xc(9)
      INTEGER i, iac, iat, ie, in, iw, iz, izc, j, k, key0, nbra(10), 
     &        nexd(10, 10), nole(9), nudi(10), nudim
      REAL*8 jgs
C
      DO i = 1, 10
         b(i, 1) = BDEgas(i, 1)
         b(i, 2) = BDEgas(i, 2)
         gg(i) = GGDegas(i)
         dd(i) = DDDegas(i)
         nudi(i) = NUDidegas(i)
         nbra(i) = NBRadegas(i)
         DO j = 1, 10
            endi(i, j) = ENDidegas(i, j)
            spidi(i, j) = SPIdidegas(i, j)
            nexd(i, j) = NEXddegas(i, j)
            DO k = 1, 9
               brar(i, j, k) = BRArdegas(i, j, k)
               nole(k) = NOLedegas(k)
               xc(k) = XCDegas(k)
            ENDDO
         ENDDO
      ENDDO
      jgs = JGSdegas
      eexc0 = EEXc0degas
      estep = ESTepdegas
      ak = AKDegas
      cjgs = CJGsdegas
      beproj = BEProjdegas
      gaq = GAQdegas
      a3 = ADEgas3
      ie = IEDegas
      in = INDegas
      iz = IZDegas
      iat = IATdegas
      iac = IACdegas
      izc = IZCdegas
      key0 = KEYdegas0
99001 FORMAT(1X, a79)
      iw = 6
      WRITE(iw, 99006)
C-----write(iw,5)  titdegas
      WRITE(iw, 99002)in, iz, key0
99002 FORMAT(4I5, f10.3)
      WRITE(iw, 99003)eexc0, estep, ak, jgs, cjgs, beproj, gaq
99003 FORMAT(8F8.2)
      WRITE(iw, 99007)b   ! binding energies
      WRITE(iw, 99007)gg  ! single-particle level density g
      WRITE(iw, 99007)dd  ! pairings
      WRITE(iw, 99008)nudi ! number of discrete levels
      WRITE(iw, 99008)nbra ! number of branchings
      WRITE(iw, 99006)
      DO j = 1, 10
         nudim = nudi(j)  ! j=1 means comp nucleus, etc
         IF(nudim.NE.0)THEN
            DO i = 1, nudim
C              ! i=1 means g.s. level, etc
               WRITE(iw, 99004)endi(j, i), spidi(j, i), nexd(j, i)
99004          FORMAT(2F6.2, i6)
               IF(i.LE.nbra(j))THEN
                  DO ie = 1, 9 ! ie=1 means deay goes to g.s., etc
                     bbb = brar(j, i, ie)
                     IF(ie.GT.0 .AND. bbb.GT.0.)WRITE(iw, 99005)ie, bbb
99005                FORMAT(9(i2, f6.3))
                  ENDDO
               ENDIF
            ENDDO
            WRITE(iw, 99006)
         ENDIF
      ENDDO
99006 FORMAT(/)
99007 FORMAT(10F6.2)
99008 FORMAT(10I6)
      END
C
C
C
      SUBROUTINE SUBDEGAS(Se, Popul)
C
Coblo
Coblo
Coblo The original code DEGAS, received from E. Betak (Bratislava,
Coblo  Slovakia, January 2, 2001), was converted into subroutine
Coblo  subdegas by P. Oblozinsky (BNL, USA), January 12, 2001.
Coblo
Coblo  Variables:
Coblo    se(3,NDEXD):       spectra(g/n/p, energy bin)
Coblo    popul(3,NDEXD,25): residual population(g/n/p,energy bin,spin)
Coblo
Coblo Subdegas has the following changes compared to Degas:
Coblo
Coblo  1) The input block from Degas was taken out and extra
Coblo      subroutine degasread was created.
Coblo
Coblo  2) Input data are transfered to subdegas from the main via
Coblo      common blocks degasinput and degasinp for quantities
Coblo      named jcgdegas, instead of original jcg, etc. Then, these
Coblo      new quantities are reassigned back to original quantities
Coblo      via commands jcg = jcgdegas, etc.
Coblo
Coblo  3) Decay is restricted to preequilibrium emission by limiting
Coblo      number of excitons to n = 1,3,5,7. Summation over excitons
Coblo      in Degas is done in do loops over i = 1,nn. Therefore,we
Coblo      allow 4 terms only, see the command:
Ceb    C H A N G E D ! ! !
Coblo
Coblo      - after label 3450: nn = 4 (Emil: this is not fully ok!)
Coblo
Coblo        Note: This is correct for decay of first CN in neutron
Coblo        induced reactions, in other cases relationship between
Coblo        number of excitons n and index i is more complicated.
Coblo
Coblo      Alternative solution:
Coblo
Coblo      - after label 4300: do 4401 i=1,4
Coblo      - after label 4900: do 5800 i=1,4
Coblo      - after label 4778: do 6850 i=1,4
Coblo
Coblo  4) Decay is restricted to the first nucleus by emitting
Coblo       gamma,netron,proton. See condition commented out:
Coblo
Coblo       - after label 4778
Coblo
Coblo  5) Results are primary spectra of gamma, neutrons and protons,
Coblo      and population of respective residual nuclei. These results
Coblo      are in two arrays:
Coblo
Coblo      se(3,NDEXD)      = spectrum(g/n/p,spectral energy bin)
Coblo      popul(3,NDEXD,25)= population(g/n/p,residual energy bin,spin)
Coblo
Coblo     All spectra and residual populations after emission of
Coblo       neutrons and protons are handled by commands:
Coblo
Coblo       - after 4778 continue
Coblo
Coblo     Residual population of CN, popul(1,energy,spin), is handled
Coblo       by the commands related to:
Coblo
Coblo       - 4401 popul(1,ie,il) = popul(1,ie,il) + tfo(i,il)
Coblo
Coblo  6) Gamma cascade is blocked, only primary gamma emission is
Coblo       allowed. this is achieved by 3 commands:
Coblo
Coblo       - after label 3810:  if(ie.ne.nestep)goto 3900
Coblo       - after label 5450:  if(ie.ne.nestep)goto 5650
Coblo       - soubroutine GMMD:  do 20 ieg=nestep,nestep
Cobo
Coblo       Note: The above solution blocks transitions from continuum
Coblo       only.
Coblo
C
C      Program DEGAS
C      STANDARD VERSION, Dec. 1996        including ERRATUM 14 Sept 2000
C                        population explicitely added January 2001
C
C      Includes quadrupole gammas and enables G+C-type level densities
C
C      includes quadrupole gammas  MAXIMAL collectivization enabled
C      GILBERT AND CAMERON version         Blava 7. 5. 1996
C      ** multiplicity of discrete levels added 11. 6. 1996 **
C      *******************************************************
C
C     * * discrete transitions included * *
C
C      ****************************************************************
C      **                                                            **
C      **                       Discrete                             **
C      **      Pre-Equilibrium-Equilibrium Gamma And Spin Code       **
C      **             with multiple particle emission                **
C      **            and full account of gamma cascades              **
C      **     (master equations approach to the exciton model)       **
C      **                                                            **
C      **  E. Betak (Inst. Physics, Bratislava, Slovakia; Dec. 1996) **
C      **                                                            **
C      **                  based on code PEGAS                       **
C      **                      written by                            **
C      **           E. Betak & P. Oblozinsky (Oct. 1992)             **
C      **                                                            **
C      ****************************************************************
C
C
      IMPLICIT REAL*8(A - H, O - Z)
      INCLUDE 'dimension.h'
      PARAMETER(NDEXD = NDEX + 11)
C
C COMMON variables
C
      REAL*8 ADEgas3, AF, AKDegas, BDEgas(10, 2), BEProjdegas, 
     &       BRArdegas(10, 10, 9), C2, CJGs0, CJGsdegas, DDDegas(10), 
     &       EEXc0degas, EF, ENDi(10, 10), ENDidegas(10, 10), ESTep, 
     &       ESTepdegas, EXCef, FAL(140), FFPair(40), FFQ(40), G, GAQ, 
     &       GAQdegas, GGDegas(10), OM(25, NDEXD), OMD(10, 10, 10), 
     &       RFAc(50, 90), SG(NDEXD), SIGma, SPIdi(10, 10), 
     &       SPIdidegas(10, 10), SPP, T1111, TC(25, NDEXD, 25), TL(25), 
     &       TLP(2, NDEXD, 25), XCDegas(9), XX0(25, 3, 25), 
     &       XX02(25, 5, 25), XX2, XX22,  XP(3,25)
      INTEGER I0, IAC, IACdegas, IAO, IAT, IATdegas, IE00, IEDegas, II0,
     &        IIA, IIP, IJ, IL0, INDegas, ITJgs, ITSpp, IZ, IZC, 
     &        IZCdegas, IZDegas, IZO, KEY, KEYdegas0, N, NBR, 
     &        NBRadegas(10), NEStep, NEXddegas(10, 10), NOLedegas(9), 
     &        NUDidegas(10), NUDim
      REAL*8 JGS, JGSdegas
      CHARACTER*79 TITdegas
      COMMON FAL, EF, IAC, IZC
      COMMON /BUF   / IIA, IIP, II0
      COMMON /DAM   / FFQ, FFPair
      COMMON /DEGASINP/ JGSdegas, EEXc0degas, ESTepdegas, AKDegas, 
     &                  CJGsdegas, BEProjdegas, GAQdegas, ADEgas3, 
     &                  IEDegas, INDegas, IZDegas, IATdegas, IACdegas, 
     &                  IZCdegas, KEYdegas0, TITdegas
      COMMON /DEGASINPUT/ BRArdegas, ENDidegas, SPIdidegas, NEXddegas, 
     &                    BDEgas, GGDegas, DDDegas, XCDegas, NUDidegas, 
     &                    NBRadegas, NOLedegas
      COMMON /DEGASOUT/ SGRdegas
      COMMON /DEN   / RFAc, SIGma
      COMMON /GAD   / TC, OMD, ENDi, SPIdi, OM, SG, AF, C2, ESTep, 
     &                EXCef, T1111, GAQ, G, CJGs0, IJ, I0, IAT, NEStep, 
     &                IE00, IZ, N, IL0, NUDim, NBR
      COMMON /GMM   / XX0, XX02, XX2, XX22, XP
      COMMON /MC    / SPP, JGS, IAO, IZO, KEY, ITSpp, ITJgs
      COMMON /TRA   / TL
      COMMON /TRC   / TLP
C
C Dummy arguments
C
      REAL*8 Popul(3, NDEXD, 25), Se(3, NDEXD)
C
C Local variables
C
      REAL*8 a0, a1, a2, a3, aj, ak, akfc, alm(25), alms(25, 25), 
     &       alp(25), alps(25, 25), ared, aver, b(10, 2), b0, b0q, 
     &       beproj, bm, bmq, br, brar(10, 10, 9), c, c0, cj, cjgs, 
     &       cjgs1, cm, cp, dd(10), e, eeo, eer, eexc, eexc0, eg1, egm, 
     &       emg(25, 25), emgg(25, 25, 60), emp(25, 25), emtg, emtp, 
     &       endim, endir, engy, eom, eomm, epmxef(2), estep2, f(25), 
     &       feebr2(10), feebra(10), feed, feepar(10), flip, fsci, ga1, 
     &       gae(2, 90), ge, geg, gf, gg(10), hlp3, omd1(10, 10, 10), 
     &       omd2(10, 10, 10), omr(2, 25, NDEXD), pi, pin0(25), pro, q, 
     &       r(2, 25, 10), rexc, rl, ro1, ro2, s, sci(25), sg1, sgi, 
     &       sgiq, popultot1
      REAL*8 CFA, CHAGUG, FPAIR, FQ, RFA, RO, X0, X02, X2, X22, XDAMP
      DOUBLE PRECISION DABS, DLOG, DMOD
      REAL FLOAT
      INTEGER i, iatx, ie, ie0, ie1, ieo, ieoblo, ieom, ier, igm, ih, 
     &        ii1, iid, iiir, iipnuc, iis, ijc, ik, il, ilis, ilma, 
     &        iloblo, in, indexe, inx, ip, ip1, ir, is, isu, iw, izx, j,
     &        jav, k, key0, l, m, m00, nbr1, nbra(10), ndi, nestdi, 
     &        neste1, nestem, nesteu, nexd(10, 10), nn, nn0, nne, nne1, 
     &        nole(9), nudi(10), nudi1, nudim1
      INTEGER INT, MAX0, MIN0
      REAL*8 jc, jc0
      REAL*8 sgir, sgr, sigf, sigfc, sigmacn(25), sigmacntot, 
     &       st(3, NDEXD), su, sum, sumpro, t(25), t00, t0001, t11, 
     &       t111, tau(25, 25), tf(25), tf0(25, 25), 
     &       tm(9, 25, NDEXD, 25),  
     &       xc(9), xde(25, 25), xdea
C
      DO j = 1, 25            ! CN cross section as function of spin
         sigmacn(j) = 0.0
      ENDDO
      sigmacntot = 0.0
C
Coblo Now reassignement to original quantities:
C
      DO i = 1, 10
         b(i, 1) = BDEgas(i, 1)
         b(i, 2) = BDEgas(i, 2)
         gg(i) = GGDegas(i)
         dd(i) = DDDegas(i)
         nudi(i) = NUDidegas(i)
         nbra(i) = NBRadegas(i)
         DO j = 1, 10
            ENDi(i, j) = ENDidegas(i, j)
            SPIdi(i, j) = SPIdidegas(i, j)
            nexd(i, j) = NEXddegas(i, j)
            DO k = 1, 9
               brar(i, j, k) = BRArdegas(i, j, k)
               nole(k) = NOLedegas(k)
               xc(k) = XCDegas(k)
            ENDDO
         ENDDO
      ENDDO
      JGS = JGSdegas
      eexc0 = EEXc0degas
      ESTep = ESTepdegas
      ak = AKDegas
      cjgs = CJGsdegas
      beproj = BEProjdegas
      GAQ = GAQdegas
      a3 = ADEgas3
      ie = IEDegas
      in = INDegas
      IZ = IZDegas
      IAT = IATdegas
      IAC = IACdegas
      IZC = IZCdegas
      key0 = KEYdegas0
C
C
C     Subscripts TC, TN, TP: (EXCITONS, ENERGY, SPIN)
C     Subscripts OM: (EXCITONS, ENERGY)
C     Subscripts POPUL: (ENERGY, SPIN)
C     Subscripts ALP, ALM, T, etc.: (EXCITONS)
C     Subscripts ALPS, ALMS, EMP, etc.: (EXCITONS, SPIN)
C     Subscripts SE, ST: (PARTICLE, ENERGY)
C     Subscripts TLP: (PARTICLE, ENERGY, SPIN)
C     Subscripts B, XC, R: (..., NUCLEUS, ...)
C     Subscripts OMD etc.: (EXCITONS, SPIN, ENERGY NUMBER)
C     Subscripts NUDI (NUCLUES)
C     Subscripts ENDI, SPIDI, NEXD: (NUCLEUS, ENERGY/SPIN/EXCITONS)
C     Subscripts GAE: GAE(1,...): energy of gamma, GAE(2,...) gamma c.s.
C     Subscripts BRAR: (NUCLEUS, LEVEL FROM, LEVEL TO)
C
C$large
C
C
C     WRITTEN FOR ARBITRARY PROJECTILE, THE INVERSE CROSS SECTIONS
C     APPROXIMATED ACCORDING TO CHATTERJEE AND GUPTA
C
C     THE INITIAL EXCITON CONFIGURATION IS ASSUMED TO BE
C     H0=0, P0=A(PROJ)
C
C     KEYS:    0-GAMMA,   1-N,   2-P,   3-D,   4-T,   5-HE-3,   6-ALPHA
C
C     THE CHAIN OF THE NUCLEI WITHIN THE REACTION IS DENOTED VIA II0
C
C     0
C     CN(A,Z)
C     /       \
C     1             2
C     (A-1,Z)       (A-1,Z-1)
C     /       \      /      \
C     3            4             5
C     (A-2,Z)      (A-2,Z-1)     (A-2,Z-2)
C     /      \      /      \       /     \
C     6           7             8             9
C     (A-3,Z)     (A-3,Z-1)     (A-3,Z-2)     (A-3,Z-3)
C     /       \     /      \      /       \     /     \
C
C     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
      DATA brar/900*0./, pi/3.141593/, ir, iw/41, 42/
      EF = 40.
      FAL(1) = 0.D0
      FAL(2) = 0.D0
      FAL(3) = 0.D0
      DO i = 4, 140
         FAL(i) = DLOG(DFLOAT(i - 3)) + FAL(i - 1)
      ENDDO
      DO j = 1, NDEXD
         DO i = 1, 3
            st(i, j) = 0.
         ENDDO
      ENDDO
C
Coblo Here we cut the original input block
C
      eexc = eexc0
      ITJgs = INT(2.*JGS + 1.)
      CJGs0 = DMOD(cjgs, 1.D0)
C     CJGS0: 0 for integer spin, 0.5 for half-integer spin of comp.syst.
C
C     Arrays dimensioned to NDEXD energy steps only!
C     (10 reserved for discrete states, rest for continuum space)
      endim = eexc
      DO i = 1, 10
         NUDim = nudi(i)
         IF(NUDim.NE.0)endim = MAX(endim, eexc - ENDi(i, NUDim))
      ENDDO
 100  nestdi = INT(endim/ESTep + 0.5)
      IF(nestdi.LT.NDEXD - 10)THEN
         estep2 = ESTep/2.
         NUDim = nudi(1)
         endim = eexc
         IF(NUDim.NE.0)endim = eexc - ENDi(1, NUDim)
         NEStep = INT(endim/ESTep + 0.5) + NUDim
         nesteu = NEStep
         IF(INT(eexc0/ESTep + 0.5).GT.nesteu)
     &      nesteu = INT(eexc0/ESTep + 0.5)
         nestem = nesteu + 1
         IF(gg(1).EQ.0.D0)gg(1) = IAT/13.
         IF(gg(2).EQ.0.D0)gg(2) = (IAT - 1)/13.
         IF(gg(3).EQ.0.D0)gg(3) = (IAT - 1)/13.
         IF(gg(4).EQ.0.D0)gg(4) = (IAT - 2)/13.
         IF(gg(5).EQ.0.D0)gg(5) = (IAT - 2)/13.
         IF(gg(6).EQ.0.D0)gg(6) = (IAT - 2)/13.
         IF(gg(7).EQ.0.D0)gg(7) = (IAT - 3)/13.
         IF(gg(8).EQ.0.D0)gg(8) = (IAT - 3)/13.
         IF(gg(9).EQ.0.D0)gg(9) = (IAT - 3)/13.
         IF(gg(10).EQ.0.D0)gg(10) = (IAT - 3)/13.
         G = gg(1)
         C2 = 1.3178E14*ESTep
         WRITE(iw, 99001)
Coblo    stop                ! Degas stop replaced by return
99001    FORMAT(/4X, 
     &          'DEGAS - Discrete Pre-Equilibrium-Equilibrium Gamma', 
     &          ' And Spin Code'/1X, '(Original PEGAS by', 
     &          ' E. Betak and P. Oblozinsky. Discrete levels by E.', 
     &          ' Betak)'/8X, 
     &          ' == Standard version (E), Sept 2000 / Jan 2001 =='//)
C        WRITE(IW,5)  tit
         KEY = key0
         CALL MASCHA
         m00 = IAO - 1
         iatx = IAT - IAO
         izx = IZ - IZO
         inx = in - IAO + IZO
         AF = 2.*iatx/(iatx + 1.)
         ared = iatx*IAO/FLOAT(iatx + IAO)
         akfc = 2.4179E20*ESTep
         DO j = 1, 25
            r(1, j, 1) = (FLOAT(IAO - IZO) + (j - 1)*inx/FLOAT(iatx))
     &                   /(j + m00)
            r(2, j, 1) = (FLOAT(IZO) + (j - 1)*izx/FLOAT(iatx))
     &                   /(j + m00)
         ENDDO
         jav = MIN0(INT(SQRT(0.299*G*eexc)), 15)
         IF(jav.LT.3)jav = 3
         DO j = jav, 25
            r(1, j, 1) = in/FLOAT(IAT)
            r(2, j, 1) = IZ/FLOAT(IAT)
         ENDDO
C        * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C        R-factor - END
C
C        REACTION C.S. AND FEEDING OF COMPOUND SPIN CALCULATION
         IF(key0.LE.2)engy = eexc0 - b(1, key0)
         IF(key0.GE.3)engy = eexc0 - beproj
         sgr = 0.
         KEY = key0
         IF(key0.LE.2 .OR. key0.EQ.5)CALL TRANS(engy)
         IF(key0.GE.3 .AND. key0.NE.5)READ(ir, 99002)TL
99002    FORMAT(10F6.2)
C        (FMT 106. / 5F6.)  INPUT OF PROJECTILE TRANSMISSION COEFFICIENTS
C        AT INCIDENT ENERGY FOR L=0, 1, ..., 14
         CALL SPPRO
         sigf = 656.623/(engy*ared*ITSpp*ITJgs)
         jc0 = 0.
         IF(MOD(ITJgs + ITSpp, 2).EQ.1)jc0 = 0.5
         emtp = 0.
         DO ijc = 1, 25
            sum = 0.
            jc = jc0 + (ijc - 1)
            sigfc = (2*jc + 1.)*sigf
            s = ABS(JGS - SPP)
 120        rl = ABS(jc - s)
 140        l = INT(rl) + 1
            IF(l.LE.24)THEN
               sum = sum + TL(l)
               rl = rl + 1.
               IF(rl.LE.(jc + s))GOTO 140
            ENDIF
            s = s + 1.
            IF(s.LE.(JGS + SPP))GOTO 120
            sum = sum*sigfc
            IF(sum.GT.emtp)THEN
               ilma = ijc
               emtp = sum
            ENDIF
            sgr = sgr + sum
            pin0(ijc) = sum
         ENDDO
         SGRdegas = sgr
Coblo    IF (SGR.LE.0.d0) STOP       ! Degas stop replaced by return
         IF(sgr.LE.0.D0)RETURN
         eg1 = 29.*SQRT((1. + 2./a3)/a3)
         ga1 = 5.
         sg1 = 10.64*FLOAT(in*IZ)/FLOAT(IAT)
         WRITE(iw, 99003)IAT, eexc0, ESTep, sgr, ak, eg1, ga1, sg1, gg, 
     &                   b, nudi, nbra
99003    FORMAT(/' A = ', I3, '    Eexc = ', F5.2, 
     &          ' MeV      energy step = ', F4.2, 
     &          ' MeV'/' Reaction c.s. = ', F6.1, ' mb      K'' = ', 
     &          F5.0, ' MeV**3'/' GDR param:    E = ', F4.1, 
     &          ' MeV     width = ', F4.2, ' MeV     peak c.s. = ', 
     &          F5.0, 
     &          ' mb'/' Single-particle level dens. (1/MeV) and binding'
     &          , ' energies (MeV) :'/' KEY       0     1     2     3', 
     &          '     4     5     6     7     8     9'/'  g   ', 
     &          10F6.2/' B(N) ', 10F6.2/' B(P) ', 10F6.2/' #d.l.', 
     &          10I6/' #b.r.', 
     &          10I6//' holes  lambda(+)   lambda(-)  particle rate', 
     &          '  gamma rate')
         DO j = 1, 2
            DO ip = 2, 10
               DO i = 1, 25
                  r(j, i, ip) = 0.
               ENDDO
            ENDDO
         ENDDO
C--------THE MAXIMUM EXCITATION ENERGIES FOR A GIVEN CHAIN OF NUCLEI ARE
C--------ESTABLISHED
         xc(1) = eexc0 - b(1, 1)
         xc(2) = eexc0 - b(1, 2)
         xc(3) = xc(1) - b(2, 1)
         xc(4) = xc(1) - b(2, 2)
         xc(5) = xc(2) - b(3, 2)
         xc(6) = xc(3) - b(4, 1)
         xc(7) = xc(3) - b(4, 2)
         xc(8) = xc(5) - b(6, 1)
         xc(9) = xc(5) - b(6, 2)
         II0 = 0
         DO in = 1, 9
            DO i = 1, 25
               DO il = 1, 25
                  DO ie = 1, NDEXD
                     tm(in, i, ie, il) = 0.
                  ENDDO
               ENDDO
            ENDDO
         ENDDO
C        do 950 ie=1,3
C        e=10.*(ie-1) + 5
         SIGma = a3*SQRT(0.56)
         DO i = 1, 45
            DO il = 1, 90
               jc = 0.5*(il - 1)
               RFAc(i, il) = RFA(i, jc)
            ENDDO
         ENDDO
C--------subscripts: EXCITON NUMBER (exact!), (2*J+1)
         DO i = 1, 3
            DO ie = 1, NDEXD
               Se(i, ie) = 0.
            ENDDO
         ENDDO
C        write(iw,83) ii0
C--------NEXT NUCLEUS IN THE CHAIN
         DO i = 1, 25
            DO il = 1, 25
               tf0(i, il) = 0.
               DO ie = 1, NDEXD
                  TC(i, ie, il) = 0.
               ENDDO
            ENDDO
         ENDDO
         DO ie = 1, NDEXD
            DO i = 1, 3
               Se(i, ie) = 0.
            ENDDO
         ENDDO
         IIP = II0 + 1
         IIA = 0
         IF(II0.GE.1)IIA = 1
         IF(II0.GE.3)IIA = 2
         IF(II0.GE.6)IIA = 3
         indexe = IIP + IIA
         NUDim = nudi(IIP)
         NBR = nbra(IIP)
         IF(II0.EQ.0 .OR. II0.EQ.1 .OR. II0.EQ.3 .OR. II0.EQ.6)THEN
            IZC = IZ
            IF(II0.NE.0)CJGs0 = 0.5 - CJGs0
            IF(II0.NE.0)ilma = MAX0(1, ilma - 1)
            N = 1
            XX2 = X2(.5D0, .5D0)
            XX22 = X22(1.D0, 1.D0)
            DO i = 1, 25
               DO is = 1, 5
                  DO IJ = 1, 25
                     IF(is.LE.3)XX0(i, is, IJ) = 0.
                     IF (is.le.3 .and. i.eq.1)  XP(is,IJ)=0.
                     XX02(i, is, IJ) = 0.
                  ENDDO
               ENDDO
            ENDDO
            DO i = 1, 40
               FFPair(i) = 0.0
               FFQ(i) = 0.0
            ENDDO
            DO i = 1, 40
               aj = FLOAT(i - 1)
               FFPair(i) = FPAIR(aj)
            ENDDO
            DO i = 1, 25
               q = i - 0.5
               FFQ(i) = FQ(q)
            ENDDO
            DO i = 1, 25
               N = 2*i - IIA + m00 - 1
               DO IJ = 1, 25
                  cj = CJGs0 + IJ - 1
                  DO is = 1, 5
                     IF(is.EQ.1)s = ABS(cj - 1.)
                     IF(is.EQ.2)s = cj
                     IF(is.EQ.3)s = cj + 1.
                     IF(is.EQ.4)s = ABS(cj - 2.)
                     IF(is.EQ.5)s = cj + 2.
                     IF(is.LE.3)XX0(i, is, IJ) = X0(s, cj)
                     IF(is.le.3 .and. i.eq.1)  XP(IS,IJ)=X2(S,CJ)
                     XX02(i, is, IJ) = X02(s, cj)
                  ENDDO
               ENDDO
            ENDDO
         ENDIF
         IAC = IAT - IIA
C        IZC=IZ-1                 Corrected 14 Sept. 2000
         G = gg(IIP)
         gf = G*EF
         c0 = 6.58195E-22*IAT**3./(2.*pi*G*ak)
         c = c0*eexc
C--------NEUTRON AND PROTON TRANSMISSION COEFFICIENTS
         DO i = 1, 2
            KEY = i
            DO ie = 1, NDEXD
               engy = FLOAT(ie)*ESTep
               CALL TRANS(engy)
               DO il = 1, 25
                  TLP(i, ie, il) = TL(il)
               ENDDO
            ENDDO
         ENDDO
C--------GAMMA INVERSE CROSS SECTIONS (GDR FORM)
         KEY = 0
         DO ie = 1, NDEXD
            engy = FLOAT(ie)*ESTep
            SG(ie) = CHAGUG(engy)
         ENDDO
         IF(II0.NE.0)THEN
            sum = 0.
            DO i = 1, 25
               DO il = 1, 25
                  DO ie = 1, NDEXD
                     TC(i, ie, il) = tm(II0, i, ie, il)
                     sum = sum + TC(i, ie, il)
                  ENDDO
               ENDDO
            ENDDO
            IF(sum.LE.1.E-9)GOTO 200
         ENDIF
C
C--------R-FACTOR CALCULATION FOR SUBSEQUENT NUCLEI
C
         IF(II0.EQ.1 .OR. II0.EQ.3 .OR. II0.EQ.6)THEN
            DO i = 1, 25
               ih = i - 1
               ip = i - IIA + m00
               IF(ip.GT.0)THEN
                  flip = FLOAT(ip)
                  ip1 = ip + 1
                  DO j = 1, IIA
                     IJ = II0 + j
                     ik = IJ - IIA
                     r(1, i, IJ) = MAX((ip1*r(1,i,ik) - 1.D0)/flip, 
     &                             0.D0)
                  ENDDO
                  IJ = IJ + 1
                  r(1, i, IJ) = MIN(ip1*r(1, i, ik)/flip, 1.D0)
               ENDIF
               DO j = 2, 10
                  r(2, i, j) = 1.0D0 - r(1, i, j)
               ENDDO
            ENDDO
         ENDIF
C
C--------END OF R-FACTOR FOR SUBSEQUENT NUCLEI
C
C--------SET ENERGY LIMITS
C
         IF(II0.NE.0)THEN
            eexc = xc(II0)
            endir = 0.
            IF(NUDim.NE.0)endir = ENDi(IIP, NUDim)
            NEStep = INT((eexc - endir)/ESTep + 0.5) + NUDim
         ENDIF
         neste1 = NEStep + 1
         EXCef = MAX(eexc - (NEStep - nudi(IIP))*ESTep, 0.D0)
         DO j = 1, 2
            iiir = indexe + j
            nudim1 = 0
            if (iiir.le.10) nudim1 = nudi(iiir)
            rexc = eexc - b(IIP, j)
            IF(nudim1.NE.0)rexc = rexc - ENDi(iiir, nudim1)
            epmxef(j) = MAX(rexc - INT(rexc/ESTep + 0.5)*ESTep, 0.D0)
         ENDDO
         IF(eexc.GT.0.D0)THEN
            DO ieo = 1, nesteu
               DO i = 1, 25
                  ih = i - 1
                  ip = i - IIA + m00
                  IF(ip.GT.0)THEN
                     ie = neste1 + 1 - ieo
                     IF(ie.GT.NUDim)THEN
                        e = EXCef + (ie - NUDim)*ESTep
                        IF(e.LT.0.)GOTO 150
                        OM(i, ie) = RO(e - dd(IIP), G, ip, ih)
                     ENDIF
                     DO j = 1, 2
                        iiir = indexe + j
                        nudim1 = nudi(iiir)
                        rexc = epmxef(j) + (ie - 1 - nudi(iiir))*ESTep
                        IF(nudim1.NE.0)rexc = rexc + ENDi(iiir, nudim1)
                        IF(iiir.LE.10 .AND. ie.GT.nudi(iiir))
     &                     omr(j, i, ie)
     &                     = RO(rexc - dd(iiir), gg(iiir), ip - 1, ih)
                        IF(iiir.GT.10)omr(j, i, ie)
     &                     = RO(rexc, (IAT - 4.)*G/IAC, ip - 1, ih)
                     ENDDO
                  ENDIF
 150           ENDDO
            ENDDO
            DO i = 1, 10
               DO j = 1, 10
                  DO ie = 1, 10
                     OMD(i, j, ie) = 0.
                     omd1(i, j, ie) = 0.
                     omd2(i, j, ie) = 0.
                  ENDDO
               ENDDO
            ENDDO
            IF(NUDim.NE.0)THEN
               DO ie = 1, NUDim
                  ndi = nexd(IIP, ie)
                  j = SPIdi(IIP, ie) - CJGs0 + 1.
                  IF(j.LE.10)THEN
                     IF(ndi.EQ.( - 1))THEN
                        sum = 0.
                        IF(j.LE.10)THEN
                           DO i = 1, 10
                              ih = i - 1
                              ip = i - IIA + m00
                              OMD(i, j, ie)
     &                           = RO(ENDi(IIP, i), G, ip, ih)
                              sum = sum + OMD(i, j, ie)
                           ENDDO
                           IF(sum.NE.0.)THEN
                              DO i = 1, 10
                                 OMD(i, j, ie) = OMD(i, j, ie)
     &                              /(sum*ESTep)
                              ENDDO
                           ENDIF
                        ELSE
                           WRITE(iw, 99004)II0, IIP, ie, CJGs0, 
     &                           SPIdi(IIP, ie), j
99004                      FORMAT('spiny   ii0,iip', 2I3, '  ie', i3, 
     &                            '  cjgs0,spidi', 2F5.1, '   j=', i3)
                        ENDIF
                     ELSE
                        DO i = 1, 10
                           N = 2*i - IIA + m00 - 1
                           IF(.NOT.((ndi.GE.0) .AND. (N.LT.ndi.OR.N.GT.(
     &                        ndi+1))))OMD(i, j, ie)
     &                        = (2.*SPIdi(IIP, ie) + 1)/ESTep
                        ENDDO
                     ENDIF
                  ENDIF
               ENDDO
            ENDIF
            cjgs1 = CJGs0 - 0.5
            IF(cjgs1.LT.0.)cjgs1 = cjgs1 + 1.
            iiir = indexe + 1
            nudi1 = nudi(iiir)
            IF(nudi1.NE.0)THEN
               DO ie = 1, nudi1
                  ndi = nexd(iiir, ie)
                  IF(ndi.EQ.( - 1))THEN
                     sum = 0.
                     DO i = 1, 10
                        ih = i - 1
                        ip = i - IIA + m00 - 1
                        omd1(i, j, ie) = RO(ENDi(iiir, i), G, ip, ih)
                        sum = sum + omd1(i, j, ie)
                     ENDDO
                     IF(sum.NE.0.)THEN
                        DO i = 1, 10
                           omd1(i, j, ie) = omd1(i, j, ie)/(sum*ESTep)
                        ENDDO
                     ENDIF
                  ELSE
                     j = SPIdi(iiir, ie) - cjgs1 + 1.
                     DO i = 1, 10
                        N = 2*i - IIA + m00 - 2
                        IF(.NOT.(ndi.GE.0 .AND. (N.LT.ndi.OR.N.GT.(ndi+1
     &                     ))))omd1(i, j, ie) = (2.*SPIdi(iiir, ie) + 1)
     &                     /ESTep
                     ENDDO
                  ENDIF
               ENDDO
            ENDIF
            iiir = indexe + 2
            nudi1 = nudi(iiir)
            IF(nudi1.NE.0)THEN
               DO ie = 1, nudi1
                  ndi = nexd(iiir, ie)
                  IF(ndi.EQ.( - 1))THEN
                     sum = 0.
                     DO i = 1, 10
                        ih = i - 1
                        ip = i - IIA + m00 - 1
                        omd2(i, j, ie) = RO(ENDi(iiir, i), G, ip, ih)
                        sum = sum + omd2(i, j, ie)
                     ENDDO
                     IF(sum.NE.0.)THEN
                        DO i = 1, 10
                           omd2(i, j, ie) = omd2(i, j, ie)/(sum*ESTep)
                        ENDDO
                     ENDIF
                  ELSE
                     j = SPIdi(iiir, ie) - cjgs1 + 1.
                     DO i = 1, 10
                        N = 2*i - IIA + m00 - 2
                        IF(.NOT.(ndi.GE.0 .AND. (N.LT.ndi.OR.N.GT.(ndi+1
     &                     ))))omd2(i, j, ie) = (2.*SPIdi(iiir, ie) + 1)
     &                     /ESTep
                     ENDDO
                  ENDIF
               ENDDO
            ENDIF
C
C
Coblo       popul(1,energy,spin) contains residual population of
C           first CN. This population has a peak at the initial
C           excitation energy (reduced by preeq emission of g,n
C           and p), and tail corresponding to spectrum of preeq
C           gamma
            DO iloblo = 1, 25
               DO ieoblo = 1, NDEXD
                  Popul(1, ieoblo, iloblo) = 0.0
               ENDDO
            ENDDO
            popultot1 = 0.0
Ceb         DO ie0 = 1, NEStep
            ie0 = 1
            ie = NEStep + 1 - ie0
            NUDim = nudi(IIP)
            IF(ie.GT.NUDim)e = EXCef + (ie - NUDim)*ESTep
            IF(ie.LE.NUDim)e = ENDi(IIP, ie)
            IF(e.GE.0.)THEN
C
C              If necessary, only selected excitation energies are to be
C              considered, i.e. restriction to IE0=1 in the cycle DO 4500
C              takes only the highest possible excitation energy.
C              Alternatively, the value of the excitation energy E can be
C              used instead of the cycle variable IE0.
C              If you prefer to use selected excitation energies only, skip
C              whole section between labels 4300 and 4500 for energies .LT.
C              the lowest energy considered (see the flag below label 4300)
C
C              Angular part of the density of states
C
               DO i = 1, 25
                  alp(i) = 0.
                  alm(i) = 0.
                  IF(ie.GT.nudi(IIP) .OR. nexd(IIP, ie).LE.0)THEN
                     ih = i - 1
                     ip = i - IIA + m00
                     ip1 = ip + 1
                     N = ip + ih
                     IF(N.LE.0)GOTO 160
                     a0 = 0.25*(ip*ip + ih*ih + ip - 3*ih)
                     a1 = 0.25*((ip - 1)**2 + (ih - 1)**2 + ip - 3*ih + 
     &                    2)
                     a2 = 0.25*((ip + 1)**2 + (ih + 1)**2 + ip - 3*ih - 
     &                    2)
                     a3 = 0.5*((ip + 2)*ip1 + ih*(ih + 1))
                     cp = c0*CFA(e, N)*e/N
                     cm = 1.
                     IF(N.GT.2)cm = c0*CFA(e, N - 2)*e/(N - 2)
C--------------------DENSITY AND TRANSITION RATES CALCULATION
                     ge = G*e
C                    Next line is not recommended by Betak although
c                    it might seem resonable
c                    ge = G*(e-dd(iip)) 
                     hlp3 = 0.
                     IF(ge - a1 - gf.GE..01)hlp3 = (ih - 1)
     &                  *((ge - a1 - gf)/(ge - a1))**(N - 3)
                     IF(cm.NE.0.)alm(i) = 0.5*ip*ih*(N - 2)*(1. - hlp3)
     &                  /cm
                     IF(ge.GE.a3)THEN
                        hlp3 = 0.
                        IF(ge - a2 - gf.GE..01)hlp3 = (ih + 1)
     &                     *EXP((N + 1)*DLOG(ge - a2 - gf) - (N - 1)
     &                     *DLOG(ge - a0))
                        IF(cp.NE.0.)alp(i)
     &                     = 0.5*(EXP((N+1)*DLOG(ge-a2) - (N-1)
     &                     *DLOG(ge-a0)) - hlp3)/(cp*(N + 1))
                        IF(i.eq.25 .AND. om(i+1,ie).eq.0.0D+0)
     &                     alp(i)=0. 
                     ENDIF
                  ENDIF
                  DO IJ = 1, 25
                     cj = CJGs0 + IJ - 1
                     xde(i, IJ) = XDAMP(cj)
                  ENDDO
 160           ENDDO
               IF(II0.EQ.0)THEN
                  N = 3
                  xdea = 0.
                  aver = 0.
                  DO IJ = 1, 25
                     cj = CJGs0 + IJ - 1
                     ijc = 2*cj + 1
                     aver = aver + ijc*RFAc(N, ijc)
                     xdea = xdea + xde(2, IJ)*ijc*RFAc(N, ijc)
                  ENDDO
                  IF(aver.EQ.0.)aver = 1.
                  xdea = xdea/aver
                  IF(xdea.EQ.0.)xdea = 1.
               ENDIF
               DO i = 1, 25
                  DO IJ = 1, 25
                     alps(i, IJ) = alp(i)*xde(i, IJ)/xdea
                  ENDDO
               ENDDO
               DO IJ = 1, 25
                  cj = CJGs0 + IJ - 1
                  ijc = 2*cj + 1
                  DO i = 2, 25
                     N = 2*i - IIA + m00 - 1
                     alms(i, IJ) = 0.
                     IF(.NOT.(ie.LE.NUDim .AND. (il.GT.10.OR.i.GT.10)))
     &                  THEN
                        IF(ie.GT.NUDim .OR. il.GT.10 .OR. i.GT.10 .OR. 
     &                     OMD(i, IJ, ie).NE.0.)THEN
                           IF(ie.LE.NUDim .AND. il.LE.10 .AND. 
     &                        i.LE.10 .AND. N.GT.2)alms(i, IJ)
     &                        = alps(i - 1, IJ)*OMD(i - 1, IJ, ie)
     &                        /OMD(i, IJ, ie)
                           IF(.NOT.(ie.GT.NUDim .AND. (N.LE.2.OR.OM(i,ie
     &                        ).EQ.0.D0)))alms(i, IJ) = alps(i - 1, IJ)
     &                        *OM(i - 1, ie)*RFAc(N - 2, ijc)
     &                        /(OM(i, ie)*RFAc(N, ijc))
                        ENDIF
                     ENDIF
                  ENDDO
               ENDDO
C
C--------------INTRANUCLEAR TRANSITION RATES ESTABLISHED
C
               DO IJ = 1, 25
                  alms(1, IJ) = 0.
                  alps(25, IJ) = 0.
               ENDDO
Coblo          NN = 25       ! Excitons, original value
               nn = 25          ! Excitons restricted to 1,3,5,7
               DO i = 4, 25
                  IF(alp(i).EQ.0.D0)THEN
                     IF(i.LE.nn)nn = i
                  ENDIF
               ENDDO
               IF(ie.EQ.NEStep)nn0 = nn
C--------------DENSITIES AND TRANSITION RATES CALCULATION
C--------------INITIAL EXCITON CONFIGURATION
               DO i = 1, nn
                  DO il = 1, 25
                     emp(i, il) = 0.
                     emg(i, il) = 0.
                     tau(i, il) = 0.
                  ENDDO
               ENDDO
C--------------EMISSION RATES CALCULATIONS FOR GIVEN NUCLEI
               DO i = 1, nn
                  ip = i - IIA + m00
                  ih = i - 1
                  N = ip + ih
                  IF(ie.GT.NUDim)ro1 = OM(i, ie)
                  IF(N.GE.0)THEN
                     DO il = 1, 25
                        cj = CJGs0 + il - 1
                        ijc = 2*cj + 1
                        IF(ie.LE.NUDim .AND. il.LE.10 .AND. i.LE.10)
     &                     ro1 = OMD(i, il, ie)
                        IF(ie.LE.NUDim .AND. (il.GT.10 .OR. i.GT.10))
     &                     ro1 = 0.
                        IF(N.GE.1 .AND. ro1.NE.0.)THEN
                           DO j = 1, 2
                              t11 = 0.
                              iiir = indexe + j
                              nudim1 = nudi(iiir)
                              eom = e - b(IIP, j)
C                             if (eom.lt.estep2 .or. n.eq.1) goto 3799
                              IF(eom.GE.estep2 .AND. N.NE.1)THEN
                                 ieom = INT(eom/ESTep + 0.5)
C                                if (ieom.le.0) goto 3799
                                 IF(ieom.GT.0)THEN
                                    DO ieo = 1, ieom
                                       eeo = ESTep*ieo
                                       eer = eom - eeo
                                       IF(eer.GE.( - estep2))THEN
                                         endir = 0.
                                         IF(nudim1.NE.0)
     &                                      endir = ENDi(iiir, nudim1)
                                         IF(eer.GE.(endir + estep2))THEN
                                         ier = INT((eer - endir)
     &                                      /ESTep + 0.5) + nudim1
                                         ro2 = 0.
                                         IF(ier.GT.nudim1)
     &                                      ro2 = omr(j, i, ier)
C                                            tato podmienka je asi zbytocna,
C                                            ale pre kazdy pripad ...
                                         IF(ro2.NE.0.D0)THEN
                                         DO iis = 1, 25
C                                                  Emission (E, CJ) --->
C                                                  (EOM-EEO, SU)
                                         IF(CJGs0.EQ.0.D0)su = 0.5 + 
     &                                      iis - 1
                                         IF(CJGs0.EQ.0.5D0)su = iis - 1
                                         isu = 2*su + 1
                                         sum = 0.
                                         s = ABS(su - 0.5)
 162                                     rl = ABS(j - s)
 164                                     l = INT(rl) + 1
                                         IF(l.LE.24)THEN
                                         sum = sum + TLP(j, ieo, l)
                                         rl = rl + 1.
                                         IF(rl.LE.(j + s))GOTO 164
                                         ENDIF
                                         s = s + 1.
                                         IF(s.LE.(su + 0.5))GOTO 162
                                         t11 = t11 + 
     &                                      sum*ro2*RFAc(N - 1, isu)
                                         IF(N.EQ.1)WRITE(iw, 99005)j, 
     &                                      iis, ieo, eeo, t11, sum, ro2
99005                                    FORMAT(' part-c  j,iis', i2, 
     &                                      i3, '  ieo,eeo', i2, f4.1, 
     &                                      '  t11', e10.3, '  sum', 
     &                                      e10.3, '  ro2', e10.3)
                                         ENDDO
                                         GOTO 170
                                         ENDIF
                                         ENDIF
                                         IF(i.LE.10 .AND. nudim1.NE.0)
     &                                      THEN
                                         DO iid = 1, nudim1
                                         IF(ABS(ENDi(iiir,iid) - eer)
     &                                      .LE.estep2)THEN
                                         DO iis = 1, 10
                                         ier = INT(ENDi(iiir, iid)
     &                                      /ESTep + 0.5)
C                                                     Emission (E, CJ) --->
C                                                     (EOM-EEO, SU)
                                         IF(CJGs0.EQ.0.D0)su = 0.5 + 
     &                                      iis - 1
                                         IF(CJGs0.EQ.0.5D0)su = iis - 1
                                         isu = 2*su + 1
                                         IF(SPIdi(iiir, iid).NE.su)
     &                                      GOTO 168
                                         IF(.NOT.((j.EQ.1.AND.omd1(i,iis
     &                                      ,iid).NE.0.) .OR. 
     &                                      (j.EQ.2.AND.omd2(i,iis,iid)
     &                                      .NE.0.)))GOTO 168
                                         sum = 0.
                                         s = ABS(su - 0.5)
                                         rl = ABS(j - s)
 166                                     l = INT(rl) + 1
                                         IF(l.LE.24)THEN
                                         sum = sum + TLP(j, ieo, l)
                                         rl = rl + 1.
                                         IF(rl.LE.(j + s))GOTO 166
                                         ENDIF
                                         s = s + 1.
                                         IF(s.LE.(su + 0.5))THEN
                                         rl = ABS(j - s)
                                         GOTO 166
                                         ELSE
                                         IF(j.EQ.1)t11 = t11 + 
     &                                      sum*omd1(i, iis, iid)
                                         IF(j.EQ.2)t11 = t11 + 
     &                                      sum*omd2(i, iis, iid)
                                         ENDIF
 168                                     ENDDO
                                         ENDIF
                                         ENDDO
                                         ENDIF
                                       ENDIF
 170                                ENDDO
                                 ENDIF
                              ENDIF
                              tau(i, il) = tau(i, il)
     &                           + akfc*r(j, i, IIP)*t11
                           ENDDO
                        ENDIF
                        t11 = 0.
                        IF(ie.EQ.NEStep)THEN !Gamma cascade not allowed
                           IF(ie.GT.NBR)THEN
                              IF(ie.NE.1 .AND. ro1.NE.0.)THEN
                                 ie1 = INT(e/ESTep + 0.5)
                                 DO igm = 1, 60
                                    emgg(i, il, igm) = 0.
                                 ENDDO
                                 DO igm = 1, ie1
                                    egm = igm*ESTep
                                    geg = G*egm
C                                   IER = IE-IGM
                                    eer = e - egm
                                    IF(eer.GE.( - estep2) .AND. N.GT.0)
     &                                 THEN
                                       t0001 = 0.
                                       sgi = egm*egm*SG(igm)
                                       sgiq = egm*egm*sgi*GAQ
                                       sgir = 0.
                                       IF((sgi + sgiq).NE.0.)
     &                                    sgir = sgi*sgiq/(sgi + sgiq)
                                       IF((sgi + sgiq).NE.0.)
     &                                    sgi = sgi*sgi/(sgi + sgiq)
                                       sgiq = sgir
                                       DO is = 1, 3
                                         IF(is.EQ.1)su = cj - 1.
                                         IF(is.EQ.2)su = cj
                                         IF(is.EQ.3)su = cj + 1.
                                         IF(is.EQ.4)s = cj - 2.
                                         IF(is.EQ.5)s = cj + 2.
                                         IF(su.GE.0.D0)THEN
                                         isu = 2*su + 1
                                         T1111 = 0.
                                         bm = 0.
                                         b0 = 0.
      if (n.ge.3 .and. i.ne.1 .and. is.le.3
     A    .AND. ((n-2)*xx0(i-1,is,il)+geg*xp(is,il)).ne.0.)
     1            bm=geg*xp(is,il)/((n-2)*xx0(i-1,is,il)
     2                                 +geg*xp(is,il))
      if (is.le.3 .AND. (n*xx0(i,is,il)+geg*xp(is,il)).ne.0.)
     1       b0=n*xx0(i,is,il)/(n*xx0(i,is,il)+geg*xp(is,il))

                                         bmq = 0.
                                         IF(N.GE.1 .AND. i.NE.1)
     &                                      bmq = geg*XX22/((N - 2)
     &                                      *XX02(i - 1, is, il)
     &                                      + geg*XX22)
                                         b0q = N*XX02(i, is, il)
     &                                      /(N*XX02(i, is, il)
     &                                      + geg*XX22)
                                         endir = 0.
                                         IF(NUDim.NE.0)
     &                                      endir = ENDi(IIP, NUDim)
                                         IF(eer.LT.(endir + estep2))THEN
                                         ilis = il + is - 2
                                         IF(is.EQ.4)ilis = il - 2
                                         IF(is.EQ.5)ilis = il + 2
                                         IF(il.LE.10 .AND. 
     &                                      ilis.LE.10 .AND. 
     &                                      ilis.GT.0 .AND. 
     &                                      i.LE.10 .AND. NUDim.NE.0)
     &                                      THEN
                                         DO iid = 1, NUDim
                                         IF(ABS(ENDi(IIP,iid) - eer)
     &                                      .LE.estep2 .AND. 
     &                                      SPIdi(IIP, iid).EQ.su)THEN
                                         T1111 = (b0*sgi + b0q*sgiq)
     &                                      *OMD(i, ilis, iid)
                                 IF(n.GT.2)T1111 = T1111 +
     &                                      (bm*sgi + bmq*sgiq)
     &                                      *OMD(i - 1, ilis, iid)

     
                                         ENDIF
                                         ENDDO
                                         ENDIF
                                         ELSE
                                         iiir = INT((eer - endir)
     &                                      /ESTep + 0.5) + NUDim
                                         IF(iiir.LT.1)GOTO 172
                                         T1111 = b0*sgi*OM(i, iiir)
     &                                      *RFAc(N, isu)
     &                                      + b0q*sgiq*OM(i, iiir)
     &                                      *RFAc(N, isu)
                                         IF(i.NE.1 .AND. N.GT.2)
     &                                      T1111 = T1111 + 
     &                                      sgi*OM(i - 1, iiir)
     &                                      *RFAc(N - 2, isu)
     &                                      *bm + sgiq*OM(i - 1, iiir)
     &                                      *RFAc(N - 2, isu)*bmq
                                         ENDIF
                                         t11 = t11 + T1111
                                         t0001 = t0001 + T1111
                                         ENDIF
 172                                   ENDDO
                                       IF(igm.LE.60)THEN
                                         IF(ie.GT.NUDim .AND. N.GT.0)
     &                                      emgg(i, il, igm)
     &                                      = t0001*C2/(ro1*RFAc(N, ijc)
     &                                      )
                                         IF(ie.LE.NUDim .AND. 
     &                                      il.LE.10 .AND. i.LE.10)
     &                                      emgg(i, il, igm)
     &                                      = t0001*C2/ro1
                                       ENDIF
                                    ENDIF
                                 ENDDO
                              ENDIF
                           ENDIF
                        ENDIF
                        emp(i, il) = 0.
                        emg(i, il) = 0.
                        IF(ro1.NE.0.)THEN
                           IF(ie.GT.NUDim .AND. N.GT.0)emp(i, il)
     &                        = tau(i, il)/(ro1*RFAc(N, ijc))
                           IF(ie.GT.NUDim .AND. N.GT.0)emg(i, il)
     &                        = t11*C2/(ro1*RFAc(N, ijc))
                           IF(ie.LE.NUDim .AND. il.LE.10 .AND. i.LE.10)
     &                        emp(i, il) = tau(i, il)/ro1
                           IF(ie.LE.NUDim .AND. il.LE.10 .AND. i.LE.10)
     &                        emg(i, il) = t11*C2/ro1
                        ENDIF
                        t11 = emp(i, il) + emg(i, il) + alps(i, il)
     &                        + alms(i, il)
                        IF(t11.NE.0.D0)tau(i, il) = 1./t11
                        IJ = i - 1
                        IF(il.EQ.ilma .AND. ie0.EQ.1 .AND. II0.EQ.0)
     &                     WRITE(iw, 99006)ie, il, IJ, alps(i, il), 
     &                           alms(i, il), emp(i, il), emg(i, il)
99006                   FORMAT(1X, 3I4, 2X, 3E12.4, 3X, E12.4)
C-----------------------IF DESIRED, OUTPUT OF INTRANUCLEAR TRANSITION RATES
                     ENDDO
                  ENDIF
               ENDDO
C
C--------------END OF EMISSION RATES CALCULATIONS
C
C--------------MASTER EQ. SOLUTION FOR ONE NUCLEUS WITHIN THE CHAIN
C
               IF(ie0.EQ.1)WRITE(iw, 99007)II0
99007          FORMAT(/' Solutions for nucleus of key ', i2//3x, 'E ', 
     &                4x, 'J', 4x, 'particle', 4x, 'gamma', 4x, 
     &                'population', ' c.s.  t i m e  i n t e g r a l s')
               IF(ie.GT.NBR)THEN
                  DO il = 1, 25
                     IF(ie.GT.NUDim .OR. il.LE.10)THEN
                        nne = 0
                        cj = CJGs0 + il - 1
                        DO i = 1, nn
                           TC(i, ie, il) = 0.
                           IF(ie.GT.NUDim .AND. OM(i, ie).NE.0.)nne = i
                           IF(i.LE.10 .AND. il.LE.10)THEN
                              IF(ie.LE.NUDim .AND. OMD(i, il, ie).NE.0.)
     &                           nne = i
                           ENDIF
                        ENDDO
                        IF(nne.NE.0)THEN
                           nne1 = nne - 1
                           feed = 0.
                           DO i = 1, nn
                              tf(i) = 0.
                              IF(ie.GT.NUDim .AND. ie.NE.NEStep)tf(i)
     &                           = tf0(i, il)
                              IF(ie.GT.NUDim .AND. II0.NE.0)tf(i)
     &                           = tf(i) + tm(II0, i, ie, il)
C                             IF (e.ge.14.1  .and.II0.NE.0)     TF(I) =
C                             TF(I) + TM(II0,I,IE,IL)
                              IF(ie.LE.NUDim .AND. il.LE.10 .AND. 
     &                           i.LE.10 .AND. ie.NE.NEStep)tf(i)
     &                           = tf0(i, il)
                              IF(ie.LE.NUDim .AND. il.LE.10 .AND. 
     &                           i.LE.10 .AND. II0.NE.0)tf(i) = tf(i)
     &                           + tm(II0, i, ie, il)
                              IF(ie.EQ.NEStep .AND. II0.EQ.0 .AND. 
     &                           i.EQ.1)tf(1) = tf(1) + pin0(il)
                              feed = feed + tf(i)
                           ENDDO
                           IF(ie.GT.NUDim .OR. nexd(IIP, ie).LT.0)THEN
                              IF(feed.LE.1.E-9)GOTO 180
C-----------------------------CHATTERJEE AND GUPTA ALGORITHM
                              f(nne) = 0.
                              sci(nne) = 1.
                              IF(nne1.NE.0)THEN
C--------------------------------IF THE COMPUTER SKIPS FOLLOWING 2 DO-LOOPS,
C--------------------------------THIS IF CAN BE DELETED
                                 DO j = 1, nne1
                                    i = nne - j
                                    f(i) = alps(i, il)*tau(i, il)
     &                                 *alms(i + 1, il)*tau(i + 1, il)
                                    IF(f(i).EQ.1.)GOTO 176
C-----------------------------------SOLUTION WITH NO EMISSION, NOT ALLOWED
C-----------------------------------BY CH.+G. ALGORITHM
                                 ENDDO
                                 DO j = 1, nne1
                                    i = nne - j
                                    fsci = f(i)*sci(i + 1)
                                    IF(ABS(fsci - 1.).LT.1.E-9)THEN
                                       IF(DABS(alps(i,il)*tau(i,il) - 1.
     &                                    ).LT.1.E-9)THEN
                                         sci(i) = 1.
                                         GOTO 174
                                       ENDIF
                                    ENDIF
                                    sci(i) = 1./(1. - f(i)*sci(i + 1))
                                 ENDDO
 174                             IF(ie.NE.5 .OR. il.NE.1)THEN
                                 ENDIF
                              ENDIF
                              DO i = 1, nne
                                 br = 0.
                                 sumpro = 0.
                                 IF(i.NE.1)br = br + alps(i - 1, il)
     &                              *t(i - 1)
                                 T(I)=0.
                                 IF(nne1.NE.0)THEN
                                    DO k = i + 1, nne
                                       pro = 1.
                                       DO m = i + 1, k
                                         pro = pro*alms(m, il)
     &                                      *tau(m, il)*sci(m)
                                       ENDDO
                                       sumpro = sumpro + tf(k)*pro
                                    ENDDO
                                 ENDIF
                                 br = br + tf(i) + sumpro
                                 t(i) = sci(i)*tau(i, il)*br
                                 IF(II0.NE.0)tm(II0, i, ie, il) = t(i)
                                 TC(i, ie, il) = t(i)
                              ENDDO
                           ELSEIF(il.LE.10)THEN
                              DO i = 1, 10
                                 IF(emg(i, il).NE.0.)TC(i, ie, il)
     &                              = tf(i)/emg(i, il)
                              ENDDO
                           ENDIF
C--------------------------END OF MAST. EQ. SOLUTION (FOR ONE NUCLEUS AND
C--------------------------ONE EXC. ENERGY)
 176                       emtg = 0.
                           emtp = 0.
                           DO i = 1, nn
                              emtp = emtp + TC(i, ie, il)*emp(i, il)
                              emtg = emtg + TC(i, ie, il)*emg(i, il)
                           ENDDO
                           IF(ie.LE.NUDim)THEN
                              IF(nexd(IIP, ie).LE.0 .AND. 
     &                           emtg.NE.feed .AND. emtg.NE.0.)THEN
                                 DO i = 1, 10
                                    TC(i, ie, il) = TC(i, ie, il)
     &                                 *feed/emtg
                                 ENDDO
                                 GOTO 176
                              ENDIF
                           ENDIF
                           IF(feed.GE.1.E-3 .AND. ie.GE.(NEStep - 1))
     &                        THEN
C-----------------------------CN residual population
                              sigmacn(il) = feed - emtp - emtg
                              sigmacntot = sigmacntot + sigmacn(il)
C                             write(iw,51) e,cj,sigmacn(il),sigmacntot
                           ENDIF
                           IF(feed.GE.1.E-3 .AND. ie.GE.(NEStep - 1))
     &                        WRITE(iw, 99008)e, cj, emtp, emtg, feed, 
     &                              (TC(i, ie, il), i = 1, 3)
99008                      FORMAT(1X, f5.2, 1x, f4.1, 1x, 2G11.4, 2x, 
     &                            g11.4, 2x, 3E9.2)
                        ENDIF
                     ENDIF
C--------------------OUTPUT: EXCITATION ENERGY; SPIN; PARTICLE, GAMMA &
C--------------------TOTAL EMISSION; TIME INTEGRALS OF THE EXCITON STATES
C--------------------(N=1-7*)
 180              ENDDO
C
C-----------------Gamma de-excitation population will be calculated
C
                  IF(ie.NE.1)THEN
C
C                    If you like to consider not all excitation energies,
C                    this condition is to be modified. Currently, the gamma
C                    de-excitation calculated next 13 lines is to all
C                    energies except the lowest one (IE.EQ.1).
C                    Specifically, if you want to consider only the highest
C                    possible excitation energy, you can skip all these
C                    lines (including the line 4400 TF0(I,IL)=T1111).
C
 9876                ie = ie - 1
                     DO il = 1, 25 ! Spins
Ceb                     DO i = 1, nn
                        DO i = 1, 3
Coblo                      do 4401 i=1,4    ! Excitons limited to i=1,4
                           IL0 = il
                           N = 2*i - IIA - 1 + m00
                           T1111 = 0.
                           IF(.NOT.((ie.GT.NUDim) .AND. (N.LE.0.OR.OM(i,
     &                        ie).EQ.0.)))THEN
                              IF(.NOT.((ie.LE.NUDim) .AND. (i.GT.10.OR.
     &                           il.GT.10)))THEN
                                 IF(ie.GT.NUDim .OR. OMD(i, il, ie)
     &                              .NE.0.)THEN
                                    I0 = i
                                    IE00 = ie
                                    CALL GAMMD
                                 ENDIF
                              ENDIF
                           ENDIF
                           tf0(i, il) = T1111
                           Popul(1, ie, il) = Popul(1, ie, il)
     &                        + tf0(i, il)
                        ENDDO
                     ENDDO
                     IF (ie.NE.1) GOTO 9876
                  ENDIF
               ENDIF
            ENDIF
C           4402 write(*,*) ie,il,popul(1,ie,il),popultot1
Ceb         ENDDO
Coblo-------Now residual population of CN at the initial
C-----------excitation energy, ie=nestep.
            DO iloblo = 1, 25
               Popul(1, NEStep, iloblo) = sigmacn(iloblo)
            ENDDO
            IF(NBR.LE.0)THEN
            ENDIF
C
C           Capote 2001
            IF(EEXc0degas.LE.0)THEN        !skip decay of discrete levels
C              IF(EXCn .GT. 0) GOTO 180 !skip decay of discrete levels
C              de-excitation of discrete levels according to branchings
               nbr1 = NBR + 1
               DO ie0 = 1, NBR
                  ie = nbr1 - ie0
                  feebra(ie) = 0.
                  feebr2(ie) = 0.
                  feepar(ie) = 0.
                  DO il = 1, 10
                     DO i = 1, 10
                        IL0 = il
                        N = 2*i - IIA - 1 + m00
                        T1111 = 0.
                        IF(ie.GT.NUDim .OR. OMD(i, il, ie).NE.0.)THEN
                           I0 = i
                           IE00 = ie
                           CALL GAMMD
                           IF(II0.NE.0)feepar(ie) = feepar(ie)
     &                        + tm(II0, i, ie, il)
                        ENDIF
                        feebra(ie) = feebra(ie) + T1111
                     ENDDO
                  ENDDO
               ENDDO
               WRITE(iw, 99009)
99009          FORMAT(//' Decay according to the branching ratios'//
     &          ' Level spin  populated (particle +cont.gam.+disc.gam.)'
     &          )
               DO ie0 = 1, NBR
                  ie = nbr1 - ie0
                  tf(ie) = feebra(ie) + feebr2(ie) + feepar(ie)
                  WRITE(iw, 99010)ENDi(IIP, ie), SPIdi(IIP, ie), tf(ie),
     &                            feepar(ie), feebra(ie), feebr2(ie)
99010             FORMAT(f6.3, f5.1, f10.3, '  (', e9.3, '+', e9.3, '+',
     &                   e9.3, ')')
                  ie1 = ie - 1
                  IF(ie1.NE.0)THEN
                     DO i = 1, ie1
                        IF(brar(IIP, ie, i).NE.0)THEN
                           gae(1, i) = ENDi(IIP, ie) - ENDi(IIP, i)
                           gae(2, i) = brar(IIP, ie, i)*tf(ie)
                           feebr2(i) = feebr2(i) + gae(2, i)
                           WRITE(iw, 99011)gae(1, i), gae(2, i)
99011                      FORMAT(23x, 'Egam =', f6.3, ' MeV,   c.s. =',
     &                            g10.3, ' mb')
                        ENDIF
                     ENDDO
                  ENDIF
               ENDDO
            ENDIF
C
C-----------MASTER EQ. FINISHED
C
C-----------NEUTRON, PROTON AND GAMMA SPECTRA CALCULATION
C
Ceb         DO ie = 1, nesteu
            ie = nesteu              ! Highest energy only!
            DO il = 1, 25
               cj = CJGs0 + il - 1
               ijc = 2*cj + 1
C
Coblo          Do loop over number of excitons:
C              i=1,2,3,4 corresponds to n=1,3,5,7 for the first CN,
C              for other nuclei the relationship is more complicated
C
               DO i = 1, 3         ! 3 corresponds to 5 Excitons
Coblo             do 5800 i=1,4
                  t00 = TC(i, ie, il)
                  N = 2*i - IIA - 1 + m00
                  IF(N.GT.0 .AND. t00.NE.0.D0)THEN
                     IF(ie.GT.NUDim)ro1 = OM(i, ie)*RFAc(N, ijc)
                     IF(ie.LE.NUDim .AND. il.LE.10 .AND. i.LE.10)
     &                  ro1 = OMD(i, il, ie)
                     IF(ie.LE.NUDim .AND. (il.GT.10 .OR. i.GT.10))
     &                  ro1 = 0.
                     IF(ro1.NE.0.D0)THEN
C
C-----------------------LOOP OVER N (J=1), P (J=2) AND GAMMA (J=3)
C
                        DO j = 1, 3
                           iiir = indexe + j
                           nudim1 = NUDim
                           IF(j.LE.2)nudim1 = nudi(iiir)
                           IF(ie.GT.NUDim)eom = EXCef + (ie - NUDim)
     &                        *ESTep
                           IF(ie.LE.NUDim)eom = ENDi(IIP, ie)
                           eomm = eom
                           IF(j.LE.2)eom = eom - b(IIP, j)
                           ieom = INT(eom/ESTep + 0.5)
                           IF(eom.GE.estep2 .AND. ieom.GT.0)THEN
                              DO ieo = 1, ieom
                                 eeo = ESTep*ieo
                                 t11 = 0.
                                 ro2 = 0.
                                 eer = eom - eeo
                                 IF(eer.GE.( - estep2))THEN
                                    IF(j.EQ.3)THEN
                                       geg = G*eeo
                                       IF(ie.NE.NEStep)GOTO 196
C                                      ! Gamma cascade not allowed
                                       IF(ie.LE.NBR)GOTO 196
                                       sgi = eeo*eeo*SG(ieo)*C2
                                       sgiq = eeo*eeo*sgi*GAQ
                                       sgir = 0.
                                       IF((sgi + sgiq).NE.0.)
     &                                    sgir = sgi*sgiq/(sgi + sgiq)
                                       IF((sgi + sgiq).NE.0.)
     &                                    sgi = sgi*sgi/(sgi + sgiq)
                                       sgiq = sgir
                                       IF(eer.LT.( - estep2))GOTO 194
                                       DO is = 1, 3
                                         IF(is.EQ.1)su = cj - 1.
                                         IF(is.EQ.2)su = cj
                                         IF(is.EQ.3)su = cj + 1.
                                         IF(is.EQ.4)s = cj - 2.
                                         IF(is.EQ.5)s = cj + 2.
                                         IF(su.GE.0.D0)THEN
                                         isu = 2*su + 1
                                         T1111 = 0.
                                         bm = 0.
                                         b0 = 0.
      if (n.gt.2 .and. i.ne.1 .and. is.le.3
     A    .AND. ((n-2)*xx0(i-1,is,il) +geg*xp(is,il)).ne.0.) 
     1            bm=geg*xp(is,il)/((n-2)*xx0(i-1,is,il)
     2                                 +geg*xp(is,il))
      if (is.le.3 .AND. (n*xx0(i,is,il)+geg*xp(is,il)).ne.0.) 
     1      b0=n*xx0(i,is,il)/(n*xx0(i,is,il)+geg*xp(is,il))
                                         bmq = 0.
                                         IF(N.GE.3 .AND. i.NE.1)
     &                                      bmq = geg*XX22/((N - 2)
     &                                      *XX02(i - 1, is, il)
     &                                      + geg*XX22)
                                         b0q = N*XX02(i, is, il)
     &                                      /(N*XX02(i, is, il)
     &                                      + geg*XX22)
                                         endir = 0.
                                         IF(NUDim.NE.0)
     &                                      endir = ENDi(IIP, NUDim)
                                         IF(eer.LT.(endir + estep2))THEN
                                         ilis = il + is - 2
                                         IF(is.EQ.4)ilis = il - 2
                                         IF(is.EQ.5)ilis = il + 2
                                         IF(il.LE.10 .AND. 
     &                                      NUDim.NE.0 .AND. 
     &                                      i.LE.10 .AND. 
     &                                      ilis.LE.10 .AND. ilis.GT.0)
     &                                      THEN
                                         DO iid = 1, NUDim
                                         IF(ABS(ENDi(IIP,iid) - eer)
     &                                      .LE.estep2 .AND. 
     &                                      SPIdi(IIP, iid).EQ.su)THEN
                                         T1111 = (b0*sgi + b0q*sgiq)
     &                                      *OMD(i, ilis, iid)
                                         IF(i.NE.1 .AND. n.gt.2)
     &                                      T1111 = T1111 +
     &                                      (bm*sgi + bmq*sgiq)
     &                                      *OMD(i - 1, ilis, iid)

                                         ENDIF
                                         ENDDO
                                         ENDIF
                                         ELSE
                                         iiir = INT((eer - endir)
     &                                      /ESTep + 0.5) + NUDim
                                         IF(iiir.LT.1)GOTO 182
                                         T1111 = b0*sgi*OM(i, iiir)
     &                                      *RFAc(N, isu)
     &                                      + b0q*sgiq*OM(i, iiir)
     &                                      *RFAc(N, isu)
                                         IF(i.NE.1 .AND. N.GT.2)
     &                                      T1111 = T1111 + 
     &                                      sgi*OM(i - 1, iiir)
     &                                      *RFAc(N - 2, isu)
     &                                      *bm + sgiq*OM(i - 1, iiir)
     &                                      *RFAc(N - 2, isu)*bmq
                                         ENDIF
                                         t11 = t11 + T1111
                                         ENDIF
 182                                   ENDDO
C                                      if (ie.gt.nudim) T11 =
C                                      T11*T00/(RO1*RFAC(N,IJC))
                                       t11 = t11*t00/ro1
                                    ELSE
                                       IF(N.EQ.1)GOTO 194
                                       endir = 0.
                                       IF(nudim1.NE.0)
     &                                    endir = ENDi(iiir, nudim1)
                                       IF(eer.GE.(endir + estep2))THEN
                                         ier = INT((eer - endir)
     &                                      /ESTep + 0.5) + nudim1
                                         ro2 = 0.
                                         IF(ier.GT.nudim1)
     &                                      ro2 = omr(j, i, ier)
C                                         tato podmienka je asi zbytocna,
C                                         ale pre kazdy pripad ...
                                         IF(ro2.NE.0.D0)THEN
                                         IF(N.GT.0)THEN
                                         DO iis = 1, 25
                                         t111 = 0.
C----------------------------------------Emission(E, CJ)--->(EOM-EEO, SU)
                                         IF(CJGs0.EQ.0.D0)su = 0.5 + 
     &                                      iis - 1
                                         IF(CJGs0.EQ.0.5D0)su = iis - 1
                                         isu = 2*su + 1
                                         sum = 0.
                                         s = ABS(su - 0.5)
 184                                     rl = ABS(j - s)
 186                                     l = INT(rl) + 1
                                         IF(l.LE.24)THEN
                                         sum = sum + TLP(j, ieo, l)
                                         rl = rl + 1.
                                         IF(rl.LE.(j + s))GOTO 186
                                         ENDIF
                                         s = s + 1.
                                         IF(s.LE.(su + 0.5))GOTO 184
                                         t111 = t00*sum*ro2*RFAc(N - 1, 
     &                                      isu)*r(j, i, IIP)*akfc/ro1
                                         t11 = t11 + t111
                                         ii1 = II0 + IIA + j
                                         IF(II0.LE.5 .AND. j.LE.2 .AND. 
     &                                      ii1.LE.9)
     &                                      tm(ii1, i, ier, iis)
     &                                      = tm(ii1, i, ier, iis)
     &                                      + t111
                                         ENDDO
                                         ENDIF
                                         GOTO 192
                                         ENDIF
                                       ENDIF
                                       IF(i.LE.10)THEN
                                         IF(nudim1.NE.0)THEN
                                         DO iid = 1, nudim1
                                         IF(ABS(ENDi(iiir,iid) - eer)
     &                                      .LE.estep2)THEN
                                         DO iis = 1, 10
                                         t111 = 0.
                                         ier = INT(ENDi(iiir, iid)
     &                                      /ESTep + 0.5)
C----------------------------------------Emission(E, CJ)--->(EOM-EEO, SU)
                                         IF(CJGs0.EQ.0.D0)su = 0.5 + 
     &                                      iis - 1
                                         IF(CJGs0.EQ.0.5D0)su = iis - 1
                                         isu = 2*su + 1
                                         IF(SPIdi(iiir, iid).NE.su)
     &                                      GOTO 190
                                         IF(.NOT.((j.EQ.1.AND.omd1(i,iis
     &                                      ,iid).NE.0.) .OR. 
     &                                      (j.EQ.2.AND.omd2(i,iis,iid)
     &                                      .NE.0.)))GOTO 190
                                         sum = 0.
                                         s = ABS(su - 0.5)
                                         rl = ABS(j - s)
 188                                     l = INT(rl) + 1
                                         IF(l.LE.24)THEN
                                         sum = sum + TLP(j, ieo, l)
                                         rl = rl + 1.
                                         IF(rl.LE.(j + s))GOTO 188
                                         ENDIF
                                         s = s + 1.
                                         IF(s.LE.(su + 0.5))THEN
                                         rl = ABS(j - s)
                                         GOTO 188
                                         ELSE
                                         IF(j.EQ.1)
     &                                      t111 = t00*sum*omd1(i, iis, 
     &                                      iid)*r(j, i, IIP)*akfc/ro1
                                         IF(j.EQ.2)
     &                                      t111 = t00*sum*omd2(i, iis, 
     &                                      iid)*r(j, i, IIP)*akfc/ro1
                                         ii1 = II0 + IIA + j
                                         IF(II0.LE.5 .AND. j.LE.2 .AND. 
     &                                      ii1.LE.9)
     &                                      tm(ii1, i, iid, iis)
     &                                      = tm(ii1, i, iid, iis)
     &                                      + t111
                                         t11 = t11 + t111
                                         ENDIF
 190                                     ENDDO
                                         ENDIF
                                         ENDDO
                                         ENDIF
                                       ENDIF
                                    ENDIF
 192                                Se(j, ieo) = Se(j, ieo) + t11/ESTep
                                 ENDIF
 194                          ENDDO
                           ENDIF
 196                    ENDDO
                     ENDIF
                  ENDIF
               ENDDO
            ENDDO
Ceb         ENDDO
C
C-----------SPECTRA OUTPUT
C
            WRITE(iw, 99012)II0
99012       FORMAT(/' SPECTRA (in mb/MeV) from nucleus of key ', 
     &             i2/'  Energy', 7X, 'neutrons', 8X, 'protons', 9X, 
     &             'gammas')
            DO ieo = 1, nesteu
               eeo = ESTep*ieo
               DO j = 1, 3
                  st(j, ieo) = st(j, ieo) + Se(j, ieo)
                  Se(j, nestem) = Se(j, nestem) + Se(j, ieo)*ESTep
               ENDDO
               sum = Se(1, ieo) + Se(2, ieo) + Se(3, ieo)
               IF(sum.NE.0.)WRITE(iw, 99013)eeo, (Se(j, ieo), j = 1, 3)
99013          FORMAT(1X, F8.3, 3X, 3G15.5)
            ENDDO
            WRITE(iw, 99014)(Se(j, nestem), j = 1, 3)
99014       FORMAT(' Integrals  ', 3G15.5, ' mb'//)
         ENDIF
      ELSE
         ESTep = eexc/NDEXD
         GOTO 100
      ENDIF
C
C-----NEXT NUCLEUS
C
 200  II0 = IIP
C
Coblo IF (II0.LE. 9) GOTO 1000      ! Decay restricted to CN only
C
C
C     Continue with the next nucleus within the reaction chain
C     (the last II0 calculated in the present version is 9).
C     If you need to stop pre-equilibrium calculations earlier
C     (e.g., to continue using different formalism), you have to
C     modify the above condition.
C     Especially, if you want to stop after primary particle emission,
C     i.e. only from the originally created composite system,
C     you can remove (or make it ineffective by marking as a comment)
C     that line completely.
C
      DO iipnuc = 2, 3
         DO i = 1, 25
            DO ie = 1, NDEXD
               Popul(iipnuc, ie, i) = 0.
            ENDDO
         ENDDO
      ENDDO
      DO iipnuc = 2, 3    ! 2: residual after (n,n'), 3: after (n,p)
         DO i = 1, 25     ! Excitons
            DO il = 1, 25 ! Spins
               DO ie = 1, NDEXD ! Energy
                  Popul(iipnuc, ie, il) = Popul(iipnuc, ie, il)
     &               + tm(iipnuc - 1, i, ie, il)
               ENDDO
            ENDDO
         ENDDO
      ENDDO
C
C     Array POPUL(energy,spin) contains population (of daughter
C     nucleus) after the particle emission
C     Array SE(particle,energy) contains emission spectra from
C     the last calculated nucleus
C     Array ST(particle,energy) contains the total emission spectra
C     (summed over all reaction chain)
C     If you need the population prior to particle emission (but
C     after gammas), you have to change the above line
C     to 6850 POPUL(IE,IL)=POPUL(IE,IL)+TC(I,IE,IL)
C
C     WRITE(IW,74)
      DO ieo = 1, nestem
         eeo = ESTep*ieo
         IF(ieo.NE.nestem)THEN
            DO j = 1, 3
               st(j, nestem) = st(j, nestem) + st(j, ieo)
            ENDDO
         ENDIF
C        WRITE (IW,71) EEO,(ST(J,IEO),J=1,3)
         IF(ieo.EQ.nestem)THEN
            DO j = 1, 3
               st(j, ieo) = ESTep*st(j, ieo)
            ENDDO
         ENDIF
C        WRITE (IW,72)     (ST(J,IEO),J=1,3)
      ENDDO
99015 FORMAT(1X, a79)
99016 FORMAT(4I5, F10.3)
99017 FORMAT(8F8.1)
99018 FORMAT(10I6)
99019 FORMAT(2F6.2, i6)
99020 FORMAT(9(I2, F6.3))
99021 FORMAT(1X, f5.1, 1x, f4.1, 1x, 2G11.4, 2x, g11.4, 2x, 
     &       2E10.3//8E10.3)
99022 FORMAT(1X, I2, 2X, 3E12.4, 3X, E12.4)
99023 FORMAT(/' SUMMED SPECTRA (in mb/MeV)'/'  Energy', 7X, 'neutrons', 
     &       8X, 'protons', 9X, 'gammas')
99024 FORMAT(5E22.15)
99025 FORMAT(f5.1, $)
99026 FORMAT(2x)
99027 FORMAT(' PEGAS', 12x, 'cycle = ', i1, '          (maximum = 9)')
      END                  ! subdegas
C
Cmh---have commented the DATABLOCK below as it seems unnecessary and it is
Cmh---in conflict with input.f
C     BLOCKDATA
C     IMPLICIT REAL*8(A - H, O - Z)
C
C COMMON variables
C
C     REAL*8 AF, C2, CJGs0, ENDi(10, 10), ESTep, EXCef, G, GAQ,
C    &       OM(25, NDEXD), OMD(10, 10, 10), SG(NDEXD), SPIdi(10, 10),
C    &       T1111, TC(25, NDEXD, 25)
C     INTEGER I0, IAT, IE0, IJ, IL0, IZ, N, NBR, NEStep, NUDim
C     COMMON /GAD   / TC, OMD, ENDi, SPIdi, OM, SG, AF, C2, ESTep,
C    &                EXCef, T1111, GAQ, G, CJGs0, IJ, I0, IAT, NEStep,
C    &                IE0, IZ, N, IL0, NUDim, NBR
C
C     DATA TC/93750*0.D0/
C     END
C
      DOUBLE PRECISION FUNCTION RO(E, G, P, H)
C     EXCITON LEVEL DENSITY, CORRECTED FOR FINITE DEPTH OF NUCL. POT.
      IMPLICIT REAL*8(a - H, O - Z)
C
C COMMON variables
C
      REAL*8 EF, FAL(140)
      INTEGER IAC, IZC
      COMMON FAL, EF, IAC, IZC
C
C Dummy arguments
C
      REAL*8 E, G
      INTEGER H, P
C
C Local variables
C
      REAL*8 a, al, clfa, ge, r
      INTEGER n
C
      r = 0.
      ge = G*E
      al = 0.5*(P*(P + 1) + H*(H - 1))
      IF(ge.GE.al)THEN
         a = 0.25*(P*P + H*H + P - 3*H)
         ge = ge - a
         n = P + H
         IF(n.GT.0)THEN
            clfa = FAL(P + 3) + FAL(H + 3) + FAL(n + 2)
            r = EXP((n - 1)*LOG(ge) - clfa)
            IF(G*E - al.GT.G*EF)r = r - H*EXP((n - 1)*LOG(ge - G*EF) - 
     &                              clfa)
            r = G*r
         ENDIF
      ENDIF
      RO = r
      END
C
      SUBROUTINE GAMMD
C-----GAMMA EMISSION CALCULATION, DISCRETE LEVELS INCLUDED
C-----(feeding of states with energy IE*ESTEP and spin index IL0
C-----from all higher states by the gamma de-excitation)
      INCLUDE 'dimension.h'
      PARAMETER(NDEXD = NDEX + 11)
      IMPLICIT REAL*8(A - H, O - Z)
C
C COMMON variables
C
      REAL*8 AF, C2, CJGs0, ENDi(10, 10), ESTep, EXCef, G, GAQ, 
     &       OM(25, NDEXD), OMD(10, 10, 10), RFAc(50, 90), SG(NDEXD), 
     &       SIGma, SPIdi(10, 10), T0(25, NDEXD, 25), T1111, 
     &       XX0(25, 3, 25), XX02(25, 5, 25), XX2, XX22,  XP(3,25)
      INTEGER I0, IAT, IE0, II0, IIA, IIP, IJ, IL0, IZ, N, NBR, NEStep, 
     &        NUDim
      COMMON /BUF   / IIA, IIP, II0
      COMMON /DEN   / RFAc, SIGma
      COMMON /GAD   / T0, OMD, ENDi, SPIdi, OM, SG, AF, C2, ESTep, 
     &                EXCef, T1111, GAQ, G, CJGs0, IJ, I0, IAT, NEStep, 
     &                IE0, IZ, N, IL0, NUDim, NBR
      COMMON /GMM   / XX0, XX02, XX2, XX22, XP
C
C Local variables
C
      REAL*8 cj, eeg, eem, geg, gm, s, sgi, sgiq, sgir
      INTEGER i, ie, ie1, ieg, igm, ijc, il, ilp, is, isu
      INTEGER INT
C
C     real*4  om,sg,g,af,estep,rfac,sigma,cjgs0,xx0,xx2,geg,gm,cj,s,c2
C
      IF(N.LE. - 1)RETURN
      ie = IE0
      i = I0
      il = IL0
      cj = CJGs0 + IL0 - 1
      ijc = 2*cj + 1
C
C-----CJ - spin of state which is calculated within this subroutine
C-----The spin, wherefrom the gamma emission proceeds, is given by IS
C-----This spin is SP
C
      IF(ie.GT.NUDim)eem = EXCef + (ie - NUDim)*ESTep
      IF(ie.LE.NUDim)eem = ENDi(IIP, ie)
      ie1 = ie + 1
      IF(NBR.GT.0 .AND. ie.LE.NBR)ie1 = NBR + 1
      IF(ie1.GT.NEStep)RETURN
Coblo DO 20 IEG=IE1,NESTEP
      DO ieg = NEStep, NEStep          ! Gamma cascade not allowed
         IF(ieg.GT.NUDim)eeg = EXCef + (ieg - NUDim)*ESTep
         IF(ieg.LE.NUDim)eeg = ENDi(IIP, ieg)
         gm = eeg - eem
         igm = INT(gm/ESTep + 0.5)
         IF(igm.NE.0)THEN
            gm = igm*ESTep
            geg = G*gm
            DO is = 1, 3
               IF(is.EQ.1)s = cj + 1
               IF(is.EQ.2)s = cj
               IF(is.EQ.3)s = cj - 1
               IF(is.EQ.4)s = cj - 2.
               IF(is.EQ.5)s = cj + 2.
               IF(is.EQ.1)ilp = il + 1
               IF(is.EQ.2)ilp = il
               IF(is.EQ.3)ilp = il - 1
               IF(is.EQ.4)ilp = il - 2
               IF(is.EQ.5)ilp = il + 2
               IF(s.GE.0. .AND. s.LT.24. .AND. ilp.LE.25 .AND. ilp.GT.0)
     &            THEN
                  isu = 2*s + 1
                  IF(.NOT.(ieg.LE.NUDim .AND. (ilp.GT.10.OR.i.GT.10)))
     &               THEN
                     sgi = 0.
                     sgiq = 0.
                     if (XX0(I,IS,ILP).eq.0. .AND. XP(IS,ILP).eq.0.) 
     &                        GOTO 20
                     IF(is.LE.3)sgi = gm*gm*SG(igm)
     &                           /(N*XX0(i, is, ilp) + geg*XP(IS,ILP))
                     if ((N*XX02(I,IS,ILP)+GEG*XX22).ne.0.)
     &               sgiq = GAQ*gm*gm*gm*gm*SG(igm)
     &                      /(N*XX02(i, is, ilp) + geg*XX22)

                     sgir = 0.
                     IF((sgi + sgiq).NE.0.)sgir = sgi*sgiq/(sgi + sgiq)
                     IF((sgi + sgiq).NE.0.)sgi = sgi*sgi/(sgi + sgiq)
                     sgiq = sgir
                     IF(N.GT.0)THEN
                        IF(ieg.GT.NUDim .AND. 
     &                     (OM(i,ieg)*RFAc(N,ijc).NE.0.) .AND. is.LE.3)
     &                     T1111 = T1111 + T0(i, ieg, ilp)
     &                             *sgi*N*XX0(i, is, ilp)
     &                             /(OM(i, ieg)*RFAc(N, isu))
                        IF(ieg.GT.NUDim .AND. 
     &                     (OM(i,ieg)*RFAc(N,ijc).NE.0.))T1111 = T1111 +
     &                     T0(i, ieg, ilp)*sgiq*N*XX02(i, is, ilp)
     &                     /(OM(i, ieg)*RFAc(N, isu))
                        IF(ieg.GT.NUDim .AND. i.EQ.25)GOTO 20
                     ENDIF
                     IF(ieg.GT.NUDim .AND. OM(i + 1, ieg).NE.0.D0 .AND.
     &                  is.LE.3 .AND. (i+1).le.25)
     &                  T1111 = T1111 + T0(i + 1, ieg, ilp)
     &                              *geg*sgi*XP(IS,ILP)/(OM(i + 1, ieg)
     &                              *RFAc(N + 2, isu))

                     IF(ieg.LE.NUDim .AND. OMD(i, ilp, ieg).NE.0. .AND. 
     &                  is.LE.3)T1111 = T1111 + T0(i, ieg, ilp)
     &                                  *sgi*N*XX0(i, is, ilp)
     &                                  /OMD(i, ilp, ieg)
                     IF(ieg.GT.NUDim .AND. OM(i + 1, ieg).NE.0.D0
     &                  .AND. (I+1).le.25)
     &                  T1111 = T1111 + T0(i + 1, ieg, ilp)
     &                          *geg*sgiq*XX22/(OM(i + 1, ieg)
     &                          *RFAc(N + 2, isu))
                     IF(ieg.LE.NUDim .AND. OMD(i, ilp, ieg).NE.0.)
     &                  T1111 = T1111 + T0(i, ieg, ilp)
     &                          *sgiq*N*XX02(i, is, ilp)
     &                          /OMD(i, ilp, ieg)
                     IF(i.NE.10)THEN
                        IF(ieg.LE.NUDim .AND. OMD(i + 1, ilp, ieg)
     &                     .NE.0.D0 .AND. is.LE.3 .AND. (i+1).le.25)
     &                     T1111 = T1111 +
     &                     T0(i + 1, ieg, ilp)
     &                     *geg*sgi*XP(IS,ILP)/OMD(i + 1, ilp, ieg)
                        IF(ieg.LE.NUDim .AND. OMD(i + 1, ilp, ieg)
     &                     .NE.0.D0)T1111 = T1111 + T0(i + 1, ieg, ilp)
     &                     *geg*sgiq*XX22/OMD(i + 1, ilp, ieg)
                     ENDIF
                  ENDIF
               ENDIF
 20         ENDDO
         ENDIF
      ENDDO
      if (ie.gt.nudim .and. n.gt.0 .and.OM(I,IE).eq.0.)
     1                 T1111 = 0.
      if (ie.le.nudim .and. il.le.10 .and. i.le.10
     1                               .and. omd(i,il,ie).eq.0.) 
     2                 T1111 = 0.
      if (ie.gt.nudim .and. n.gt.0 .and.OM(I,IE).ne.0.)
     & T1111 = T1111*OM(i, ie)*C2*RFAc(N, ijc)
      IF(ie.LE.NUDim .AND. il.LE.10 .AND. i.LE.10 
     &   .AND. omd(i,il,ie).ne.0.)
     &   T1111 = T1111*OMD(i, il, ie)*C2
      END
C
      DOUBLE PRECISION FUNCTION CHAGUG(En)
      IMPLICIT REAL*8(a - H, O - z)
C
C COMMON variables
C
      REAL*8 EF, FAL(140), SPP
      INTEGER IAC, IAO, ITJgs, ITSpp, IZC, IZO, KEY
      REAL*8 JGS
      COMMON FAL, EF, IAC, IZC
      COMMON /MC    / SPP, JGS, IAO, IZO, KEY, ITSpp, ITJgs
C
C Dummy arguments
C
      REAL*8 En
C
C Local variables
C
      REAL*8 a, a23, a3, ala, am, amu, anu, d, del, e0, e1, e2, ec, ec2,
     &       egr, em, gam, p, q, r, sgm, sig, xi, z
      REAL FLOAT
C
C     AMAX1(XXX,YYY)=DMAX1(XXX,YYY)
C     AMIN1(XXX,YYY)=DMIN1(XXX,YYY)
C     SQRT(XXX)=DSQRT(XXX)
C     PARTICLE CROSS SECTION APPROXIMATION ACCORDING TO
C     A. CHATTERJEE, K.H.N. MURTHY, S.K. GUPTA
C     PRAMANA 16 (1981), 391
C     NUCL. PHYS. SOL. ST. PHYS.
C     SYMP. DELHI 1980
C     PRIVATE COMM. (FEB. 1981)
C     GAMMA  CROSS SECTION APPROXIMATED BY G.D.R. FORM,
C     S.A. FAYANS (LECTURE, DUBNA, FEB. 1982)
C     KEY NUMBERS
C     0-GAMMA,   1-N,    2-P,    3-D,    4-T,    5-HE-3,    6-ALPHA
C     EN IS THE LABORATORY ENERGY IN MEV
C     THE RESULTING CROSS SECTION IS IN MB.
C     ******************************************************************
      CALL MASCHA
      a = FLOAT(IAC - IAO)
      z = FLOAT(IZC - IZO)
      a3 = a**0.3333333
      IF(IAO.EQ.0)THEN
C--------GAMMAS
         egr = 29.*SQRT((1. + 2./a3)/a3)
         gam = 5.
         sgm = 53.2*FLOAT((IAC - IZC)*IZC)/FLOAT(IAC)
         sig = sgm*gam*En*En/((En*En - egr*egr)**2 + (gam*En)**2)
      ELSE
         a23 = a3*a3
         del = 0.
         IF(KEY.GE.3)del = 1.2
         ec = 1.44*IZO*z/(1.5*a3 + del)
         ec2 = ec*ec
         xi = MAX(En, ec)
         em = MIN(En, 61.D0)
         IF(KEY.EQ.2)THEN
C-----------PROTONS
            p = 82.12/ec + 2.39*a23/ec
            ala = -0.521*a23 - 3.43
            amu = 150.2*a**0.59 + 946.3*(a - 2.*z)/a
            anu = a23*( - 43.9 - 76.25*ec)
         ELSEIF(KEY.EQ.3)THEN
C-----------DEUTERONS
            p = -38.21 + 922.6/ec - 2804./ec2
            ala = -0.0323*a - 5.48
            am = a**0.48
            amu = 336.1*am
            anu = am*(524.3 - 371.8*ec + 5.924*ec2)
         ELSEIF(KEY.EQ.4)THEN
C-----------TRITONS
            p = -11.04 + 619.1/ec - 2147./ec2
            ala = -0.0426*a - 10.33
            am = a**0.37
            amu = 601.9*am
            anu = am*(583.0 - 546.2*ec + 1.718*ec2)
         ELSEIF(KEY.EQ.5)THEN
C-----------HELIUM-3
            p = -3.06 + 278.5/ec - 1389./ec2
            ala = -0.00535*a - 11.16
            am = a**0.40
            amu = 555.5*am
            anu = am*(687.4 - 476.3*ec + 0.509*ec2)
         ELSEIF(KEY.EQ.6)THEN
C-----------ALPHAS
            p = 10.95 - 85.2/ec + 1146./ec2
            ala = 0.0643*a - 13.96
            am = a**0.29
            amu = 781.2*am
            anu = am*( - 304.7 - 470.0*ec - 8.580*ec2)
         ELSE
C-----------NEUTRONS
            p = 0.
            ala = 31.05/a3 - 25.91
            amu = 342.4*a3 + 21.89*a23
            anu = 0.223*a23*a23 + 0.673*a23 + 617.4
            e0 = 0.
            GOTO 50
         ENDIF
         q = ala - anu/ec2 - 2.*p*ec
         r = amu + 2.*anu/ec + p*ec2
         d = q*q - 4.*r*p
         IF(d.LE.0)THEN
            e0 = 0.
         ELSE
            d = SQRT(d)
            e1 = ( - q + d)/(2.*p)
            e2 = ( - q - d)/(2.*p)
            IF(p.LT.0.D0)e0 = MIN(e1, e2)
            IF(p.GE.0.D0)e0 = MAX(e1, e2)
         ENDIF
 50      sig = p*(En - xi)**2 + ala*em + amu + anu*(2. - En/xi)/xi
         IF(En.LT.e0)sig = 0.
      ENDIF
      CHAGUG = sig
      END
C
      SUBROUTINE MASCHA
C-----MASS AND CHARGE SELECTION ACCORDING TO THE KEY
      IMPLICIT REAL*8(A - H, O - Z)
C
C COMMON variables
C
      INTEGER IA, ITJgs, ITSpp, IZ, KEY
      REAL*8 JGS
      REAL*8 SPP
      COMMON /MC    / SPP, JGS, IA, IZ, KEY, ITSpp, ITJgs
C
      IZ = 1
      IF(KEY.GE.5)IZ = 2
      IF(KEY.LE.1)IZ = 0
      IA = KEY - IZ
      END
C
      SUBROUTINE SPPRO
C-----SPIN OF THE PROJECTILE
      IMPLICIT REAL*8(A - H, O - Z)
C
C COMMON variables
C
      INTEGER IA, ISP, ITJgs, IZ, KEY
      REAL*8 JGS
      REAL*8 SP
      COMMON /MC    / SP, JGS, IA, IZ, KEY, ISP, ITJgs
C
      ISP = 2
      IF(KEY.EQ.3)ISP = 3
      IF(KEY.EQ.6)ISP = 1
      SP = (ISP - 1)/2.
      END
C
      DOUBLE PRECISION FUNCTION CFA(E, Nn)
      IMPLICIT REAL*8(A - H, O - Z)
C
C Dummy arguments
C
      REAL*8 E
      INTEGER Nn
C
C Local variables
C
      REAL*8 c, en
      REAL FLOAT
C
C-----ENERGY-DEPENDENT AND EXCITON-NUMBER DEPENDENT M.E. CONSTANT
C
      en = E/FLOAT(Nn)
      c = 0.
      IF(E.GT.0.)THEN
         IF(en.LE.2.)c = SQRT(7./en)*SQRT(2./en)
         IF(en.GE.2. .AND. en.LT.7.)c = SQRT(7./en)
         IF(en.GE.7. .AND. en.LE.15.)c = 1.
         IF(en.GT.15.)c = SQRT(en/15.)
      ENDIF
      CFA = c
      END
C
      DOUBLE PRECISION FUNCTION CLEBSH(A, A1, B, B1, C, C1)
C-----CALCULATES CLEBSH-GORDAN COEFFICIENTS
C-----         (J1 M1 J2 M2!J M)
C
      IMPLICIT REAL*8(A - H, O - Z)
C
C COMMON variables
C
      REAL*8 EF, F(140)
      INTEGER IAC, IZC
      COMMON F, EF, IAC, IZC
C
C Dummy arguments
C
      REAL*8 A, A1, B, B1, C, C1
C
C Local variables
C
      DOUBLE PRECISION DABS
      INTEGER i, iii, j, j1, j10, j2, j3, j4, j5, j6, j7, j8, j9, kh1, 
     &        kh2, kh3, kl1, kl2, kmax, kmin
      INTEGER INT, MAX0, MIN0
      REAL*8 ph, qq, su, sum, x
C
C     real*4 ef,clebsh, a,a1,b,b1,c,c1
C     ifax(xxx)=ifix(sngl(xxx))
      x = A + B + C
      x = ABS(x - INT(x))
      IF(x.LE.0.0001D0)THEN
         IF((A + B - C).GE.0D0)THEN
            IF((A - B + C).GE.0D0)THEN
               IF((B + C - A).GE.0D0)THEN
                  IF(ABS(A1).LE.A)THEN
                     IF(ABS(B1).LE.B)THEN
                        IF(ABS(C1).LE.C)THEN
                           x = A + A1
                           x = ABS(x - INT(x))
                           IF(x.LE.0.001D0)THEN
                              x = B + B1
                              x = ABS(x - INT(x))
                              IF(x.LE.0.001D0)THEN
                                 x = C + C1
                                 x = ABS(x - INT(x))
                                 IF(x.LE.0.001D0)THEN
                                    IF((A1 + B1).EQ.C1)THEN
                                       kh1 = INT(C + B + A1)
                                       kl1 = INT(A1 - A)
                                       kh2 = INT(C - A + B)
                                       kh3 = INT(C + C1)
                                       kl2 = INT(C1 + B - A)
                                       kmin = MAX0(kl1, kl2, 0) + 1
                                       kmax = MIN0(kh1, kh2, kh3) + 1
                                       j1 = INT(C + A - B) + 3
                                       j2 = kh2 + 3
                                       j3 = INT(A + B - C) + 3
                                       j4 = kh3 + 3
                                       j5 = INT(C - C1) + 3
                                       j6 = INT(A + B + C) + 4
                                       j7 = -kl1 + 3
                                       j8 = INT(A + A1) + 3
                                       j9 = INT(B - B1) + 3
                                       j10 = INT(B + B1) + 3
                                       qq = SQRT((2*C + 1.)
     &                                    *EXP(F(j1) + F(j2) + F(j3)
     &                                    + F(j4) + F(j5) - F(j6)
     &                                    - F(j7) - F(j8) - F(j9)
     &                                    - F(j10)))
                                       sum = 0.
                                       DO iii = kmin, kmax
                                         i = iii - 1
                                         j = INT(i + B + B1)
                                         ph = -1.
                                         IF(MOD(j, 2).EQ.0D0)ph = 1.
                                         j1 = kh1 - i + 3
                                         j2 = -kl1 + i + 3
                                         j3 = kh2 - i + 3
                                         j4 = kh3 - i + 3
                                         j5 = i + 3
                                         j6 = i - kl2 + 3
                                         su = ph*EXP(F(j1) + F(j2)
     &                                      - F(j3) - F(j4) - F(j5)
     &                                      - F(j6))
                                         sum = sum + su
                                         IF(DABS(sum/su).LT.1.D-8)
     &                                      GOTO 100
                                       ENDDO
                                       CLEBSH = qq*sum
                                       RETURN
                                    ENDIF
                                 ENDIF
                              ENDIF
                           ENDIF
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDIF
 100  CLEBSH = 0.
      END
C
C
      DOUBLE PRECISION FUNCTION SIXJ(A, B, C, A1, B1, C1)
C
C-----CALCULATES 6-J SYMBOLS
C-----    ( A  B  C)
C-----    ( A1 B1 C1)
C
      IMPLICIT REAL*8(A - H, O - Z)
C
C COMMON variables
C
      REAL*8 EF
      REAL*8 F(140)
      INTEGER IAC, IZC
      COMMON F, EF, IAC, IZC
C
C Dummy arguments
C
      REAL*8 A, A1, B, B1, C, C1
C
C Local variables
C
      DOUBLE PRECISION DABS
      REAL*8 DEL, TRI
      INTEGER INT, MAX0, MIN0
      INTEGER j1, j2, j3, j4, j5, j6, j7, j8, k, kh1, kh2, kh3, kl1, 
     &        kl2, kl3, kl4, kl5, kmax, kmin
      REAL*8 ph, qq, su, sum, x
C
      x = TRI(A, B, C)
      IF(x.NE.0.D0)THEN
         x = TRI(C, A1, B1)
         IF(x.NE.0.D0)THEN
            x = TRI(A1, B, C1)
            IF(x.NE.0.D0)THEN
               x = TRI(A, C1, B1)
               IF(x.NE.0.D0)THEN
                  kl1 = -1
                  kl2 = INT(A + B + C)
                  kl3 = INT(A + B1 + C1)
                  kl4 = INT(A1 + B + C1)
                  kl5 = INT(A1 + B1 + C)
                  kh1 = INT(A + B + A1 + B1)
                  kh2 = INT(B + C + B1 + C1)
                  kh3 = INT(A + C + A1 + C1)
                  kmin = MAX0(kl1, kl2, kl3, kl4, kl5)
                  kmax = MIN0(kh1, kh2, kh3)
                  sum = 0.0
                  DO k = kmin, kmax
                     ph = -1.
                     IF(MOD(k, 2).EQ.0)ph = 1.
                     j1 = k + 4
                     j2 = k - kl2 + 3
                     j3 = k - kl3 + 3
                     j4 = k - kl4 + 3
                     j5 = k - kl5 + 3
                     j6 = kh1 - k + 3
                     j7 = kh2 - k + 3
                     j8 = kh3 - k + 3
                     su = ph*EXP(F(j1) - F(j2) - F(j3) - F(j4) - F(j5)
     &                    - F(j6) - F(j7) - F(j8))
                     sum = sum + su
                     IF(DABS(sum/su).LT.1.D-8)GOTO 100
                  ENDDO
                  qq = DEL(A, B, C)*DEL(A, B1, C1)*DEL(A1, B, C1)
     &                 *DEL(A1, B1, C)
                  SIXJ = sum*qq
                  RETURN
               ENDIF
            ENDIF
         ENDIF
      ENDIF
 100  SIXJ = 0.
      END
C
C
      DOUBLE PRECISION FUNCTION DEL(A, B, C)
      IMPLICIT REAL*8(A - H, O - Z)
C
C COMMON variables
C
      REAL*8 EF, F(140)
      INTEGER IAC, IZC
      COMMON F, EF, IAC, IZC
C
C Dummy arguments
C
      REAL*8 A, B, C
C
C Local variables
C
      INTEGER INT
      INTEGER j1, j2, j3, j4
C
C     REAL*8 F
      j1 = INT(A + B - C + 3.)
      j2 = INT(A - B + C + 3.)
      j3 = INT( - A + B + C + 3.)
      j4 = INT(A + B + C + 4.)
      DEL = SQRT(EXP(F(j1) + F(j2) + F(j3) - F(j4)))
      END
C
C
      DOUBLE PRECISION FUNCTION TRI(A, B, C)
      IMPLICIT REAL*8(A - H, O - Z)
C
C Dummy arguments
C
      REAL*8 A, B, C
C
      TRI = 0.
      IF(ABS(A - B).LE.C .AND. C.LE.(A + B))TRI = 1.
      END
C
C
      DOUBLE PRECISION FUNCTION THREEJ(A, A1, B, B1, C, C1)
C-----CALCULATES 3-J SYMBOLS
C-----     (J1 J2 J
C-----     (M1 M2 M)
C-----M1+M2+M=0
C
      IMPLICIT REAL*8(A - H, O - Z)
C
C Dummy arguments
C
      REAL*8 A, A1, B, B1, C, C1
C
C Local variables
C
      REAL*8 CLEBSH
      REAL*8 d1, ph, tj
      INTEGER INT
      INTEGER k
C
      d1 = -C1
      tj = CLEBSH(A, A1, B, B1, C, d1)/SQRT(2.*C + 1.)
      k = INT(A - B + C1)
      ph = -1.
      IF(MOD(k, 2).EQ.0)ph = 1.
      THREEJ = ph*tj
      END
C
C
      DOUBLE PRECISION FUNCTION RFA(Nn, J)
      IMPLICIT REAL*8(a - H, O - Z)
C
C COMMON variables
C
      REAL*8 RFAc(50, 90), SIGma
      COMMON /DEN   / RFAc, SIGma
C
C Dummy arguments
C
      REAL*8 J
      INTEGER Nn
C
C Local variables
C
      REAL*8 a, sig
C
      RFA = 0.
      IF(Nn.GT.0)THEN
         sig = ((J + .5)**2)/(Nn*SIGma*SIGma)
         a = 1.77245*(Nn**1.5)*(SIGma**3)
         RFA = (2.*J + 1.)*EXP( - sig)/a
      ENDIF
      END
C
C
      DOUBLE PRECISION FUNCTION X0(S, J)
C-----Eq. (16) of the Report
      INCLUDE 'dimension.h'
      PARAMETER(NDEXD = NDEX + 11)
      IMPLICIT REAL*8(a - H, O - Z)
C
C COMMON variables
C
      REAL*8 AF, C2, CJGs0, ENDi(10, 10), ESTep, EXCef, G, GAQ, 
     &       OM(25, NDEXD), OMD(10, 10, 10), RFAc(50, 90), SG(NDEXD), 
     &       SIGma, SPIdi(10, 10), SPP
      INTEGER I0, IAO, IAT, IE00, IJ, IL0, ITJgs, ITSpp, IZ, IZO, KEY, 
     &        NBR, NEStep, NN, NUDim
      REAL*8 JGS, T1111, TC(25, NDEXD, 25)
      COMMON /DEN   / RFAc, SIGma
      COMMON /GAD   / TC, OMD, ENDi, SPIdi, OM, SG, AF, C2, ESTep, 
     &                EXCef, T1111, GAQ, G, CJGs0, IJ, I0, IAT, NEStep, 
     &                IE00, IZ, NN, IL0, NUDim, NBR
      COMMON /MC    / SPP, JGS, IAO, IZO, KEY, ITSpp, ITJgs
C
C Dummy arguments
C
      REAL*8 J
      REAL*8 S
C
C Local variables
C
      REAL*8 a, b, c, d, sum, sumc
      INTEGER ij1, ij2, ij3, ijj, ijs
      REAL*8 j1, j2, j3
      REAL*8 SIXJ, THREEJ, TRI
C
      IF(NN.LE.0)THEN
         X0 = 0.0
         GOTO 99999
      ELSE
         a = TRI(1.D0, S, J)
         IF(a.EQ.0.D0)THEN
            X0 = 0.0
            GOTO 99999
         ELSE
            ijj = 2*J + 1
            ijs = 2*S + 1
            IF(RFAc(NN, ijs).EQ.0.)THEN
               X0 = 0.0
               GOTO 99999
            ELSE
               a = 3*ijj/RFAc(NN, ijs)
               IF(NN.EQ.1)a = 3*ijj/(S + JGS + 1. - ABS(S - JGS))
               j2 = 0.5
               sum = 0.0
            ENDIF
         ENDIF
      ENDIF
 100  ij2 = 2*j2 + 1
      b = ij2*RFAc(1, ij2)
      IF(NN.EQ.1)b = ij2
      sumc = 0.
      j1 = ABS(j2 - 1)
 200  ij1 = 2*j1 + 1
      c = ij1*RFAc(1, ij1)
      c = c*(THREEJ(j2, .5D0, 1.D0, 0.D0, j1, -.5D0)**2)
      d = 0.
      IF(NN.EQ.1)THEN
         j3 = JGS
         d = SIXJ(1.D0, j2, j1, JGS, J, S)**2
      ELSE
         j3 = ABS(j2 - S)
 250     ij3 = 2*j3 + 1
C
         IF(ij3.LE.90)d = RFAc(NN - 1, ij3)
     &                    *(SIXJ(1.D0, j2, j1, j3, J, S)**2) + d
         j3 = j3 + 1.
         IF(j3.LE.(j2 + S))GOTO 250
      ENDIF
      c = c*d
      sumc = sumc + c
      j1 = j1 + 1.
      IF(j1.LE.(j2 + 1))GOTO 200
      b = b*sumc
      sum = sum + b
      j2 = j2 + 1.
      IF(j2.LE.24.5)GOTO 100
      X0 = sum*a
99999 END
C
C
      DOUBLE PRECISION FUNCTION X2(S, J)
C-----Eq. (17) of the Report
      INCLUDE 'dimension.h'
      PARAMETER(NDEXD = NDEX + 11)
      IMPLICIT REAL*8(a - H, O - Z)
C
C COMMON variables
C
      REAL*8 AF, C2, CJGs0, ENDi(10, 10), ESTep, EXCef, G, GAQ, 
     &       OM(25, NDEXD), OMD(10, 10, 10), RFAc(50, 90), SG(NDEXD), 
     &       SIGma, SPIdi(10, 10)
      INTEGER I0, IAT, IE00, IJ, IL0, IZ, N, NBR, NEStep, NUDim
      REAL*8 T1111, TC(25, NDEXD, 25)
      COMMON /DEN   / RFAc, SIGma
      COMMON /GAD   / TC, OMD, ENDi, SPIdi, OM, SG, AF, C2, ESTep, 
     &                EXCef, T1111, GAQ, G, CJGs0, IJ, I0, IAT, NEStep, 
     &                IE00, IZ, N, IL0, NUDim, NBR
C
C Dummy arguments
C
      REAL*8 J
      REAL*8 S
C
C Local variables
C
      REAL*8 a, b, c, sum, sumc
      INTEGER ij1, ij2
      REAL*8 j1, j2, l
      REAL*8 THREEJ, TRI
C
      l = 1.
      IF(N.LE.0)THEN
         X2 = 0.
         GOTO 99999
      ELSE
         a = TRI(l, S, J)
         IF(a.EQ.0.D0)THEN
            X2 = 0.
            GOTO 99999
         ELSE
            j1 = 0.5
            sum = 0.0
         ENDIF
      ENDIF
 100  ij1 = 2*j1 + 1
      b = ij1*RFAc(1, ij1)
      sumc = 0.
      j2 = ABS(j1 - l)
 200  ij2 = 2*j2 + 1
      c = ij2*RFAc(1, ij2)*(THREEJ(j1, .5D0, j2, -.5D0, l, 0.D0)**2)
      sumc = sumc + c
      j2 = j2 + 1.
      IF(j2.LE.(j1 + l))GOTO 200
      b = b*sumc
      sum = sum + b
      j1 = j1 + 1.
      IF(j1.LE.24.5)GOTO 100
      X2 = a*sum*(2*J + 1.)/(2*S + 1.)
99999 END
C
C
      DOUBLE PRECISION FUNCTION XDAMP(J)
C-----Eq. (10) of the Report
C-----New changes 26 Oct., 1994
      INCLUDE 'dimension.h'
      PARAMETER(NDEXD = NDEX + 11)
      IMPLICIT REAL*8(a - H, O - Z)
C
C COMMON variables
C
      REAL*8 AF, C2, CJGs0, ENDi(10, 10), ESTep, EXCef, FFPair(40), 
     &       FFQ(40), G, GAQ, OM(25, NDEXD), OMD(10, 10, 10), 
     &       RFAc(50, 90), SG(NDEXD), SIGma, SPIdi(10, 10)
      INTEGER I0, IAO, IAT, IE00, IJ, IL0, ITJgs, ITSpp, IZ, IZO, KEY, 
     &        N, NBR, NEStep, NUDim
      REAL*8 JGS, SPP, T1111, TC(25, NDEXD, 25)
      COMMON /DAM   / FFQ, FFPair
      COMMON /DEN   / RFAc, SIGma
      COMMON /GAD   / TC, OMD, ENDi, SPIdi, OM, SG, AF, C2, ESTep, 
     &                EXCef, T1111, GAQ, G, CJGs0, IJ, I0, IAT, NEStep, 
     &                IE00, IZ, N, IL0, NUDim, NBR
      COMMON /MC    / SPP, JGS, IAO, IZO, KEY, ITSpp, ITJgs
C
C Dummy arguments
C
      REAL*8 J
C
C Local variables
C
      REAL*8 a, ffi, q, q1, r, r001, r1, rn, rn1, sum, sumb
      INTEGER ij4, ijj, ijjm, ijq, iq, iq1
      INTEGER INT
      REAL*8 j4
C
      sum = 0.
      ijj = 2*J + 1
      IF(N.EQ.1)THEN
         ijjm = MOD(ijj, 2)
C        J integer => ijjm=1;   J half-integer => ijjm=0
         IF(ijjm.EQ.1)THEN
C
C           J integer  =>  j4=1/2,  Q= J-1/2, J+1/2
            q = J - 0.5
            q1 = J + 0.5
            iq = INT(q + 0.5)
            iq1 = INT(q1 + 0.5)
            ijq = 2*q + 1
            ijj = 2*q1 + 1
            r = 0.
            IF(q.GE.0.)r = RFAc(1, ijq)
            r1 = RFAc(1, ijj)
            sum = FFQ(iq1)*r1/(r + r1)
            IF(q.GE.0.)sum = sum + FFQ(iq)*r/(r + r1)
         ELSE
C           J half-integer  =>  j4=0,  Q=J
            q = J
            iq = INT(q + 0.5)
            sum = FFQ(iq)
         ENDIF
         XDAMP = sum
         GOTO 99999
      ELSE
         rn = RFAc(N, ijj)
         IF(rn.EQ.0.)THEN
            XDAMP = sum
            GOTO 99999
         ELSE
            j4 = CJGs0 - 0.5
            IF(j4.LT.0.D0)j4 = j4 + 1.
         ENDIF
      ENDIF
 100  ij4 = 2*j4 + 1
      rn1 = RFAc(N - 1, ij4)
      a = RFAc(N - 1, ij4)/RFAc(N, ijj)
      q = ABS(j4 - J)
      iq = INT(q + 0.5)
      sumb = 0.
 200  ijq = 2*q + 1
      ffi = FFQ(iq)
      r001 = RFAc(1, ijq)
      sumb = sumb + FFQ(iq)*RFAc(1, ijq)
      q = q + 1.
      iq = INT(q + 0.5)
      IF(q.LE.(j4 + J) .AND. q.LE.20.)GOTO 200
      sum = sum + sumb*a
      j4 = j4 + 1.
      IF(j4.LE.24.5)GOTO 100
      XDAMP = sum
99999 END
C
C
      DOUBLE PRECISION FUNCTION FQ(Q)
C-----Eq. (11) of the Report
      INCLUDE 'dimension.h'
      PARAMETER(NDEXD = NDEX + 11)
      IMPLICIT REAL*8(a - H, O - Z)
C
C COMMON variables
C
      REAL*8 AF, C2, CJGs0, ENDi(10, 10), ESTep, EXCef, FFPair(40), 
     &       FFQ(40), G, GAQ, OM(25, NDEXD), OMD(10, 10, 10), 
     &       RFAc(50, 90), SG(NDEXD), SIGma, SPIdi(10, 10)
      INTEGER I0, IAT, IE00, IJ, IL0, IZ, N, NBR, NEStep, NUDim
      REAL*8 T1111, TC(25, NDEXD, 25)
      COMMON /DAM   / FFQ, FFPair
      COMMON /DEN   / RFAc, SIGma
      COMMON /GAD   / TC, OMD, ENDi, SPIdi, OM, SG, AF, C2, ESTep, 
     &                EXCef, T1111, GAQ, G, CJGs0, IJ, I0, IAT, NEStep, 
     &                IE00, IZ, N, IL0, NUDim, NBR
C
C Dummy arguments
C
      REAL*8 Q
C
C Local variables
C
      REAL*8 a, b, sum, sumb
      INTEGER ij3, ij5
      INTEGER INT
      REAL*8 j3, j5
      REAL*8 THREEJ
C
      j3 = 0.
      sum = 0.
 100  ij3 = INT(j3 + 1.)
      a = ij3*FFPair(ij3)
      j5 = ABS(j3 - Q)
      sumb = 0.
 200  b = THREEJ(j5, .5D0, Q, -.5D0, j3, .0D0)**2
      ij5 = 2*j5 + 1
      sumb = sumb + ij5*RFAc(1, ij5)*b
      j5 = j5 + 1.
      IF(j5.LE.(j3 + Q) .AND. j5.LE.24.5)GOTO 200
      sum = sum + sumb*a
      j3 = j3 + 1.
      ij3 = INT(j3 + 1.)
      IF(j3.LE.24.5)GOTO 100
      FQ = sum
      END
C
C
      DOUBLE PRECISION FUNCTION FPAIR(J3)
C-----Eq. (12) of the Report
      INCLUDE 'dimension.h'
      PARAMETER(NDEXD = NDEX + 11)
      IMPLICIT REAL*8(a - H, O - Z)
C
C COMMON variables
C
      REAL*8 AF, C2, CJGs0, ENDi(10, 10), ESTep, EXCef, G, GAQ, 
     &       OM(25, NDEXD), OMD(10, 10, 10), RFAc(50, 90), SG(NDEXD), 
     &       SIGma, SPIdi(10, 10)
      INTEGER I0, IAT, IE00, IJ, IL0, IZ, N, NBR, NEStep, NUDim
      REAL*8 T1111, TC(25, NDEXD, 25)
      COMMON /DEN   / RFAc, SIGma
      COMMON /GAD   / TC, OMD, ENDi, SPIdi, OM, SG, AF, C2, ESTep, 
     &                EXCef, T1111, GAQ, G, CJGs0, IJ, I0, IAT, NEStep, 
     &                IE00, IZ, N, IL0, NUDim, NBR
C
C Dummy arguments
C
      REAL*8 J3
C
C Local variables
C
      REAL*8 a, b, sum, sumb
      INTEGER ij1, ij2
      REAL*8 j1, j2
      REAL*8 THREEJ
C
      j1 = 0.5
      sum = 0.0
 100  ij1 = 2*j1 + 1
      a = ij1*RFAc(1, ij1)
      j2 = ABS(j1 - J3)
      sumb = 0.0
 200  b = THREEJ(j1, .5D0, j2, -.5D0, J3, .0D0)**2
      ij2 = 2*j2 + 1
      sumb = sumb + ij2*RFAc(1, ij2)*b
      j2 = j2 + 1.
      IF(j2.LE.(j1 + J3) .AND. j2.LE.24.5)GOTO 200
      sum = sum + sumb*a
      j1 = j1 + 1.
      IF(j1.LE.24.5)GOTO 100
      FPAIR = sum
      END
C
C
      SUBROUTINE TRANS(E)
C     Transition coefficients for (presently) neutrons and protons
C                                 and 3He
C     according to: Murthy et al., Z. Phys. A305 (1982), 73
C                         includig Erratum, A307 (1982), 374
C
C     Input values:
C     AT, ZT       target mass and charge
C     KEY          key of the particle (1-neutron, 2-proton, 5-He3)
C     E            energy (MeV)
C
C     Output values:
C     TL(i)        transmission coefficients for l=0, ..., 14
C
      IMPLICIT REAL*8(a - H, O - Z)
C
C
C COMMON variables
C
      REAL*8 EF, FAL(140), SPP, TL(25)
      INTEGER IAC, IAO, ITJgs, ITSpp, IZC, IZO, KEY
      REAL*8 JGS
      COMMON FAL, EF, IAC, IZC
      COMMON /MC    / SPP, JGS, IAO, IZO, KEY, ITSpp, ITJgs
      COMMON /TRA   / TL
C
C Dummy arguments
C
      REAL*8 E
C
C Local variables
C
      REAL*8 a, al, al1, an, an2, at, at3, cl, d, dp, elab, fac, r, r0, 
     &       rat, se, v0, vc, vn, zt
      REAL*8 DF, F
      REAL FLOAT
      INTEGER i
      F(al1) = 1./(1. + EXP((al1-cl)/d))
      DF(al1) = (2.*al + 1.)*EXP((al1 - cl)/d)*( - 1./d)*(F(al1))**2
      CALL MASCHA
      at = FLOAT(IAC - IAO)
      zt = FLOAT(IZC - IZO)
      at3 = at**0.3333333
      rat = (at*IAO)/(at + IAO)
C     e=elab*rat
      elab = E/rat
      se = SQRT(E)
      DO i = 1, 25
         TL(i) = 0.
      ENDDO
      IF(KEY.GE.3 .AND. KEY.NE.5)RETURN
      IF(KEY.EQ.1)r = 1.285*at3 - 0.38*se + 1.87
      IF(KEY.EQ.2)r = 1.730*at3 - 0.49*se + 2.27
      IF(KEY.EQ.5)r = 1.638*at3 - 0.38*se + 4.09
      IF(KEY.EQ.1)d = 0.042*at3*E + 0.021*at + 2.80
      IF(KEY.EQ.2)d = 0.041*at + 4.18
      IF(KEY.EQ.5)d = 0.154*at3*E + 0.043*at + 3.07
      IF(KEY.EQ.2)dp = 0.087*at3*E + 0.019*at + 1.47
      IF(KEY.EQ.5)dp = 0.593*at3*E - 0.168*at + 4.44
      IF(KEY.EQ.1)r0 = 1.322 - 7.6E-4*at + 4.E-6*at**2 - 8.E-9*at**3
      IF(KEY.EQ.2)r0 = 1.17
      IF(KEY.EQ.5)r0 = 1.20
      IF(KEY.EQ.1)a = 0.66
      IF(KEY.EQ.2)a = 0.75
      IF(KEY.EQ.5)a = 0.72
C     if (KEY.EQ.1)  VC= 0.
      IF(KEY.EQ.2)vc = 1.44*zt/r
      IF(KEY.EQ.5)vc = 2.88*zt/r
      IF(KEY.EQ.1)v0 = 47.01 - 0.267*E - 0.0018*E**2
      IF(KEY.EQ.2)v0 = 54.0 - 0.32*elab + 24.*(at - 2.*zt)
     &                 /at + 0.4*zt/at3
      IF(KEY.EQ.5)v0 = 151.9 - 0.17*elab + 50.*(at - 2.*zt)/at
      vn = -1.*v0/(1. + EXP((r-r0*at3)/a))
      fac = 0.0482*rat
      IF(KEY.EQ.1)cl = fac*r*r*(E - vn - 4.93)
      IF(KEY.EQ.2)cl = fac*r*r*(E - vn - vc) - 2.00/E
      IF(KEY.EQ.5)cl = fac*r*r*(E - vn - vc)
      IF(KEY.EQ.1)an = 1.09 - 0.085*se
      IF(KEY.EQ.2)an = 1.153 - 1.02/at3
      IF(KEY.EQ.5)an = 1.
      IF(KEY.EQ.1)an2 = MAX(7.94 - 12.98/at3 + 76.38/(at3*se)
     &                  - 37.55/se, 0.D0)
      DO i = 1, 25
         al = FLOAT(i - 1)
         al1 = al*(al + 1.)
         IF(KEY.EQ.1)TL(i) = an*F(al1) - an2*DF(al1)
         IF(KEY.NE.1 .AND. al1.LE.cl)TL(i) = an*F(al1)
         IF(KEY.NE.1 .AND. al1.GT.cl)TL(i) = an*EXP( - (al1 - cl)/dp)/2.
      ENDDO
      END
C
C
      DOUBLE PRECISION FUNCTION X02(S, J)
C-----Analog of Eq. (16) of the Report for E2
      INCLUDE 'dimension.h'
      PARAMETER(NDEXD = NDEX + 11)
      IMPLICIT REAL*8(a - H, O - Z)
C
C COMMON variables
C
      REAL*8 AF, C2, CJGs0, ENDi(10, 10), ESTep, EXCef, G, GAQ, 
     &       OM(25, NDEXD), OMD(10, 10, 10), RFAc(50, 90), SG(NDEXD), 
     &       SIGma, SPIdi(10, 10), SPP
      INTEGER I0, IAO, IAT, IE00, IJ, IL0, ITJgs, ITSpp, IZ, IZO, KEY, 
     &        NBR, NEStep, NN, NUDim
      REAL*8 JGS, T1111, TC(25, NDEXD, 25)
      COMMON /DEN   / RFAc, SIGma
      COMMON /GAD   / TC, OMD, ENDi, SPIdi, OM, SG, AF, C2, ESTep, 
     &                EXCef, T1111, GAQ, G, CJGs0, IJ, I0, IAT, NEStep, 
     &                IE00, IZ, NN, IL0, NUDim, NBR
      COMMON /MC    / SPP, JGS, IAO, IZO, KEY, ITSpp, ITJgs
C
C Dummy arguments
C
      REAL*8 J
      REAL*8 S
C
C Local variables
C
      REAL*8 a, b, c, d, sum, sumc
      INTEGER ij1, ij2, ij3, ijj, ijs
      REAL*8 j1, j2, j3
      REAL*8 SIXJ, THREEJ, TRI
C
      IF(NN.LE.0)THEN
         X02 = 0.0
         GOTO 99999
      ELSE
         a = TRI(1.D0, S, J)
         IF(a.EQ.0.D0)THEN
            X02 = 0.0
            GOTO 99999
         ELSE
            ijj = 2*J + 1
            ijs = 2*S + 1
            IF(RFAc(NN, ijs).EQ.0.)THEN
               X02 = 0.0
               GOTO 99999
            ELSE
               a = 3*ijj/RFAc(NN, ijs)
               IF(NN.EQ.1)a = 5*ijj/(S + JGS + 1. - ABS(S - JGS))
               j2 = 0.5
               sum = 0.0
            ENDIF
         ENDIF
      ENDIF
 100  ij2 = 2*j2 + 1
      b = ij2*RFAc(1, ij2)
      IF(NN.EQ.1)b = ij2
      sumc = 0.
      j1 = ABS(j2 - 1)
 200  ij1 = 2*j1 + 1
      c = ij1*RFAc(1, ij1)
      c = c*(THREEJ(j2, .5D0, 2.D0, 0.D0, j1, -.5D0)**2)
      d = 0.
      IF(NN.EQ.1)THEN
         j3 = JGS
         d = SIXJ(1.D0, j2, j1, JGS, J, S)**2
      ELSE
         j3 = ABS(j2 - S)
 250     ij3 = 2*j3 + 1
         IF(ij3.LE.90)d = RFAc(NN - 1, ij3)
     &                    *(SIXJ(2.D0, j2, j1, j3, J, S)**2) + d
         j3 = j3 + 1.
         IF(j3.LE.(j2 + S))GOTO 250
      ENDIF
      c = c*d
      sumc = sumc + c
      j1 = j1 + 1.
      IF(j1.LE.(j2 + 1))GOTO 200
      b = b*sumc
      sum = sum + b
      j2 = j2 + 1.
      IF(j2.LE.24.5)GOTO 100
      X02 = sum*a
99999 END
C
C
      DOUBLE PRECISION FUNCTION X22(S, J)
C-----Analog of  Eq. (17) of the Report for E2
      INCLUDE 'dimension.h'
      PARAMETER(NDEXD = NDEX + 11)
      IMPLICIT REAL*8(a - H, O - Z)
C
C COMMON variables
C
      REAL*8 AF, C2, CJGs0, ENDi(10, 10), ESTep, EXCef, G, GAQ, 
     &       OM(25, NDEXD), OMD(10, 10, 10), RFAc(50, 90), SG(NDEXD), 
     &       SIGma, SPIdi(10, 10)
      INTEGER I0, IAT, IE00, IJ, IL0, IZ, N, NBR, NEStep, NUDim
      REAL*8 T1111, TC(25, NDEXD, 25)
      COMMON /DEN   / RFAc, SIGma
      COMMON /GAD   / TC, OMD, ENDi, SPIdi, OM, SG, AF, C2, ESTep, 
     &                EXCef, T1111, GAQ, G, CJGs0, IJ, I0, IAT, NEStep, 
     &                IE00, IZ, N, IL0, NUDim, NBR
C
C Dummy arguments
C
      REAL*8 J
      REAL*8 S
C
C Local variables
C
      REAL*8 a, b, c, sum, sumc
      INTEGER ij1, ij2
      REAL*8 j1, j2, l
      REAL*8 THREEJ, TRI
C
      l = 2.
      IF(N.LE.0)THEN
         X22 = 0.
         GOTO 99999
      ELSE
         a = TRI(l, S, J)
         IF(a.EQ.0.D0)THEN
            X22 = 0.
            GOTO 99999
         ELSE
            j1 = 0.5
            sum = 0.0
         ENDIF
      ENDIF
 100  ij1 = 2*j1 + 1
      b = ij1*RFAc(1, ij1)
      sumc = 0.
      j2 = ABS(j1 - l)
 200  ij2 = 2*j2 + 1
      c = ij2*RFAc(1, ij2)*(THREEJ(j1, .5D0, j2, -.5D0, l, 0.D0)**2)
      sumc = sumc + c
      j2 = j2 + 1.
      IF(j2.LE.(j1 + l))GOTO 200
      b = b*sumc
      sum = sum + b
      j1 = j1 + 1.
      IF(j1.LE.24.5)GOTO 100
      X22 = a*sum*(2*J + 1.)/(2*S + 1.)
99999 END
