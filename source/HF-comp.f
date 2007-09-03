Ccc   * $Author: Capote $
Ccc   * $Date: 2007-09-03 14:20:30 $
Ccc   * $Id: HF-comp.f,v 1.92 2007-09-03 14:20:30 Capote Exp $
C
      SUBROUTINE ACCUM(Iec,Nnuc,Nnur,Nejc,Xnor)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:mpu*
Ccc   *                         A C C U M                                *
Ccc   *                                                                  *
Ccc   * Normalizes scratch arrays SCRT and SCRTL with the population     *
Ccc   * divided by the H-F denominator and accumulates the result on the *
Ccc   * population array POP for a given nucleus NNUR                    *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * input:Iec  - energy index of the decaying state                  *
Ccc   *       Nnuc - index of the decaying nucleus                       *
Ccc   *       Nnur - index of the residual nucleus                       *
Ccc   *       Nejc - index of the ejectile (0 for gamma)                 *
Ccc   *       Xnor - normalization factor (POP*STEP/DENHF)               *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * output:none                                                      *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * calls:EXCLUSIVEC                                                 *
Ccc   *       EXCLUSIVEL                                                 *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
C
C
C Dummy arguments
C
      INTEGER Iec, Nejc, Nnuc, Nnur
      DOUBLE PRECISION Xnor
C
C Local variables
C
      DOUBLE PRECISION eemi, excnq, pop1, pop2, poph, popl,
     &                 popll, pops, popt, xcse
      REAL FLOAT
      INTEGER icse, icsh, icsl, ie, il, j, na, nexrt
      INTEGER INT
C-----
C-----Continuum
C-----
      IF (Nnuc.EQ.Nnur) THEN
         excnq = EX(Iec,Nnuc)
      ELSE
         excnq = EX(Iec,Nnuc) - Q(Nejc,Nnuc)
      ENDIF
      nexrt = (excnq - ECUt(Nnur))/DE + 1.0001
      DO ie = 1, nexrt          !loop over residual energies (continuum)
         icse = MIN(INT((excnq - EX(ie,Nnur))/DE + 1.0001),ndecse)
         icse = MAX0(2,icse)
         popt = 0.0
         DO j = 1, NLW, LTUrbo  !loop over residual spins
            pop1 = Xnor*SCRt(ie,j,1,Nejc)
            pop2 = Xnor*SCRt(ie,j,2,Nejc)
            pops = pop1 + pop2
            IF (ie.EQ.1) pops = pops*0.5
            popt = popt + pops !sum over spin/pi at a given energy bin
            POP(ie,j,1,Nnur) = POP(ie,j,1,Nnur) + pop1
            POP(ie,j,2,Nnur) = POP(ie,j,2,Nnur) + pop2
            IF (Nejc.NE.0 .AND. POPmax(Nnur).LT.POP(ie,j,1,Nnur))
     &          POPmax(Nnur) = POP(ie,j,1,Nnur)
         ENDDO !over residual spins
         IF (popt.NE.0.0D+0) THEN
            AUSpec(icse,Nejc) = AUSpec(icse,Nejc) + popt
            CSE(icse,Nejc,Nnuc) = CSE(icse,Nejc,Nnuc) + popt
            IF (ENDf(Nnuc).EQ.1) THEN
               CALL EXCLUSIVEC(Iec,ie,Nejc,Nnuc,Nnur,popt)
            ELSEIF (ENDf(Nnuc).EQ.2) THEN
               CSE(icse,Nejc,0) = CSE(icse,Nejc,0) + popt
            ENDIF
         ENDIF
      ENDDO !over residual energies in continuum
C-----
C-----Discrete levels
C-----
      DO il = 1, NLV(Nnur)
         eemi = excnq - ELV(il,Nnur)
         IF (eemi.LT.0.0D0) RETURN
         pop1 = Xnor*SCRtl(il,Nejc)
C--------Add contribution to discrete level population
         POPlv(il,Nnur) = POPlv(il,Nnur) + pop1
C--------Add contribution to recoils auxiliary matrix for discrete levels
         REClev(il,Nejc) = REClev(il,Nejc) + pop1
C--------Add contribution of discrete levels to emission spectra
C--------Transitions to discrete levels are distributed
C--------between the nearest spectrum bins (inversely proportional to the
C--------distance of the actual energy to the bin energy
C--------Eliminate transitions from the top bin in the 1-st CN (except gammas)

         IF (Nnuc.NE.1 .OR. ENDf(Nnuc).NE.1 .OR. Iec.NE.NEX(1) .OR.
     &       Nejc.EQ.0) THEN
            xcse = eemi/DE + 1.0001
            icsl = min(INT(xcse),NDECSE-1)
            icsh = icsl + 1
            popl = pop1*(FLOAT(icsh) - xcse)/DE
            popll = popl            !we also need popl not multiplied by 2
            IF (icsl.EQ.1) popl = 2.0*popl
            poph = pop1*(xcse - FLOAT(icsl))/DE
            CSE(icsl,Nejc,Nnuc) = CSE(icsl,Nejc,Nnuc) + popl
            CSE(icsh,Nejc,Nnuc) = CSE(icsh,Nejc,Nnuc) + poph
            IF (popll.NE.0.0D+0) THEN
               IF (ENDf(Nnuc).EQ.1) THEN
                  CALL EXCLUSIVEL(Iec,icsl,Nejc,Nnuc,Nnur,popll)
               ELSEIF (ENDf(Nnuc).EQ.2) THEN
                  CSE(icsl,Nejc,0) = CSE(icsl,Nejc,0) + popll
               ENDIF
            ENDIF
            IF (poph.NE.0.0D+0) THEN
               IF (ENDf(Nnuc).EQ.1) THEN
                  CALL EXCLUSIVEL(Iec,icsh,Nejc,Nnuc,Nnur,poph)
               ELSEIF (ENDf(Nnuc).EQ.2) THEN
                  CSE(icsh,Nejc,0) = CSE(icsh,Nejc,0) + poph
               ENDIF
            ENDIF
         ENDIF
C--------Add isotropic CN contribution to direct ang. distributions
         IF (Nnuc.EQ.1 .AND. Iec.EQ.NEX(1) .AND. Nejc.NE.0) THEN
            CSDirlev(il,Nejc) = CSDirlev(il,Nejc) + pop1
            pop1 = pop1/4.0/PI
            DO na = 1, NDANG
               CSAlev(na,il,Nejc) = CSAlev(na,il,Nejc) + pop1
            ENDDO
         ENDIF
      ENDDO
      END


      SUBROUTINE EXCLUSIVEC(Iec,Ief,Nejc,Nnuc,Nnur,Popt)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:mpu*
Ccc   *                   E X C L U S I V E C                            *
Ccc   *                                                                  *
Ccc   * Deconvolutes inclusive spectra calculated by the statistical     *
Ccc   * model into spectra for individual reactions (exclusive) as       *
Ccc   * requested by the ENDF format. EXCLUSIVEC is for transitions      *
Ccc   * to continuum.                                                    *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * input:Iec  - energy index of the decaying state                  *
Ccc   *       Ief  - energy index of the final state (irrel. for fission)*
Ccc   *       Nejc - index of the ejectile (-1 for fission)              *
Ccc   *       Nnuc - index of the decaying nucleus                       *
Ccc   *       Nnur - index of the final nucleus (irrelevant for fission) *
Ccc   *       Popt - x-sec/MeV for the transition from the initial       *
Ccc   *              (Iec,Jcn,Ipar) cell to the final bin at energy Ief  *
Ccc   *              This cross section is directly added to the spectrum*
Ccc   *              and Popt*DE is used to define a portion of the      *
Ccc   *              feeding spectrum that has to be moved.              *
Ccc   *                                                                  *
Ccc   * output:none                                                      *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * calls:none                                                       *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
C
C
C Dummy arguments
C
      INTEGER Iec, Ief, Nejc, Nnuc, Nnur
      DOUBLE PRECISION Popt
C
C Local variables
C
      DOUBLE PRECISION excnq, xnor
      INTEGER icsp, ie, iejc
      INTEGER INT
C     data jsigma/0/,jsigma2/36/
C
C
C
C     POPcse(Ief,Nejc,icsp,INExc(Nnuc))  - spectrum for the population of the
C                                   energy bin with index Ief in Nnuc by
C                                   Nejc particles (cumulative over all
C                                   decays leading to this energy bin)
C
C-----
C-----Fission
C-----
C-----Contribution due to feeding spectra from Nnuc (no decay contribution)
C-----DE spectra (DDX are not done for fission although they could be)
C
      IF (Nejc.EQ. -1) THEN
C        fission of n,nx and npx nuclei considered
         IF (POPbin(Iec,Nnuc).EQ.0) RETURN
         xnor = Popt*DE/POPbin(Iec,Nnuc)
         DO ie = 1, NDECSE
C           DO iejc = 0, NDEJC
            DO iejc = 0, 1  ! only neutrons and photons for the time being
              IF (POPcse(Iec,iejc,ie,INExc(Nnuc)).NE.0)
     &           CSEfis(ie,iejc,Nnuc) = CSEfis(ie,iejc,Nnuc)
     &                + POPcse(Iec,iejc,ie,INExc(Nnuc))*xnor
            ENDDO
         ENDDO
         RETURN   !if fission
      ENDIF
C-----
C-----Particle decay
C-----
      IF (Nnuc.EQ.Nnur) THEN
         excnq = EX(Iec,Nnuc)
      ELSE
         excnq = EX(Iec,Nnuc) - Q(Nejc,Nnuc)
      ENDIF
C-----Contribution coming straight from the current decay
C-----(ignore if residue is inclusive since summation already done in ACCUM)
      icsp = INT((excnq - EX(Ief,Nnur))/DE + 1.0001)
      IF(ENDf(Nnur).EQ.1) THEN
         POPcse(Ief,Nejc,icsp,INExc(Nnur)) =
     &      POPcse(Ief,Nejc,icsp,INExc(Nnur)) + Popt
      ELSE
         CSE(icsp,Nejc,0) = CSE(icsp,Nejc,0) + Popt
      ENDIF
C-----Contribution due to feeding spectra from Nnuc
C-----DE spectra
      IF (Nnuc.NE.1 .OR. Nejc.EQ.0) THEN !skip the first CN except gammas
         IF (POPbin(Iec,Nnuc).EQ.0) RETURN
         xnor = Popt*DE/POPbin(Iec,Nnuc)
         DO ie = 1, NDECSE
            DO iejc = 0, NDEJC
               IF (POPcse(Iec,iejc,ie,INExc(Nnuc)).NE.0) THEN
                   IF(ENDf(Nnur).EQ.2) THEN
                     CSE(ie,iejc,0) = CSE(ie,iejc,0)
     &               + POPcse(Iec,iejc,ie,INExc(Nnuc))*xnor
                   ELSE
                     POPcse(Ief,iejc,ie,INExc(Nnur)) =
     &                 POPcse(Ief,iejc,ie,INExc(Nnur))
     &               + POPcse(Iec,iejc,ie,INExc(Nnuc))*xnor
                   ENDIF
               ENDIF
            ENDDO
C-----------DDX spectra using portions
            DO iejc = 0, NDEJCD
               IF (POPcseaf(Iec,iejc,ie,INExc(Nnuc)).NE.0) THEN
                   IF(ENDf(Nnur).EQ.2) THEN
                      POPcseaf(Ief,iejc,ie,0)
     &                = POPcseaf(Ief,iejc,ie,0)
     &                + POPcseaf(Iec,iejc,ie,INExc(Nnuc))*xnor
                   ELSE
                      POPcseaf(Ief,iejc,ie,INExc(Nnur))
     &                = POPcseaf(Ief,iejc,ie,INExc(Nnur))
     &                + POPcseaf(Iec,iejc,ie,INExc(Nnuc))*xnor
                   ENDIF
               ENDIF
            ENDDO
         ENDDO
      ENDIF
      END


      SUBROUTINE EXCLUSIVEL(Iec,Ie,Nejc,Nnuc,Nnur,Popt)
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:mpu*
Ccc   *                   E X C L U S I V E L                            *
Ccc   *                                                                  *
Ccc   * Deconvolutes inclusive spectra calculated by the statistical     *
Ccc   * model into spectra for individual reactions (exclusive) as       *
Ccc   * requested by the ENDF format. EXCLUSIVEL is for transitions to   *
Ccc   * discrete levels (will store population spectra on POPcse(0,.,.,.)*
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * input:Iec  - energy index of the decaying state                  *
Ccc   *       Ie  -  index of the spectrum bin                           *
Ccc   *       Nejc - index of the ejectile                               *
Ccc   *       Nnuc - index of the decaying nucleus                       *
Ccc   *       Nnur - index of the final nucleus                          *
Ccc   *       Popt - x-sec/MeV for the transition from the initial       *
Ccc   *              (Iec,Jcn,Ipar) cell to the final bin at energy Ief  *
Ccc   *              This cross section is directly added to the spectrum*
Ccc   *              and Popt*DE is used to define a portion of the      *
Ccc   *              feeding spectrum that has to be moved.              *
Ccc   *                                                                  *
Ccc   * output:none                                                      *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * calls:none                                                       *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C
C Dummy arguments
C
      INTEGER Ie, Iec, Nejc, Nnuc, Nnur
      DOUBLE PRECISION Popt
C
C Local variables
C
      INTEGER iejc, iesp
      DOUBLE PRECISION xnor
C
C
C     POPcse(Ief,Nejc,icsp,INExc(Nnuc))  - spectrum for the population of the
C                                   energy bin with index Ief in Nnuc by
C                                   Nejc particles (cumulative over all
C                                   decays leading to this energy bin)
C
C-----Contribution comming straight from the current decay
      IF(ENDf(Nnur).EQ.1) THEN
         POPcse(0,Nejc,Ie,INExc(Nnur)) = POPcse(0,Nejc,Ie,INExc(Nnur))
     &       + Popt
      ELSE
         CSE(ie,Nejc,0) = CSE(ie,Nejc,0) + Popt
      ENDIF
C-----Contribution due to feeding spectra from Nnuc
C-----DE spectra
      IF (Nnur.NE.1 .OR. Nejc.EQ.0) THEN !skip the first CN except gammas
         IF (POPbin(Iec,Nnuc).GT.0) THEN
            xnor = Popt*DE/POPbin(Iec,Nnuc)
            DO iesp = 1, NDECSE
               DO iejc = 0, NDEJC
                  IF (POPcse(Iec,iejc,iesp,INExc(Nnuc)).NE.0) THEN
                    IF(ENDF(Nnur).EQ.2) THEN
                      CSE(iesp,iejc,0) = CSE(iesp,iejc,0)
     &                + POPcse(Iec,iejc,iesp,INExc(Nnuc))*xnor
                    ELSE
                      POPcse(0,iejc,iesp,INExc(Nnur))
     &                = POPcse(0,iejc,iesp,INExc(Nnur))
     &                + POPcse(Iec,iejc,iesp,INExc(Nnuc))*xnor
                    ENDIF
                  ENDIF
               ENDDO
C--------------DDX spectra using portions
               DO iejc = 0, NDEJCD
                  IF(POPcseaf(Iec,iejc,iesp,INExc(Nnuc)).NE.0) THEN
                     IF(ENDF(Nnur).EQ.2) THEN
                        POPcseaf(0,iejc,iesp,0)
     &                  = POPcseaf(0,iejc,iesp,0)
     &                  + POPcseaf(Iec,iejc,iesp,INExc(Nnuc))*xnor
                     ELSE
                        POPcseaf(0,iejc,iesp,INExc(Nnur))
     &                  = POPcseaf(0,iejc,iesp,INExc(Nnur))
     &                  + POPcseaf(Iec,iejc,iesp,INExc(Nnuc))*xnor
                     ENDIF
                  ENDIF
               ENDDO
            ENDDO
         ENDIF
      ENDIF
      END


      SUBROUTINE DECAY(Nnuc,Iec,Jc,Ipc,Nnur,Nejc,Sum)
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:PPu*
Ccc   *                         D E C A Y                                *
Ccc   *                (function to function version)                    *
Ccc   *                                                                  *
Ccc   * Calculates decay of a continuum state in nucleus NNUC into       *
Ccc   * continuum and discrete states of the residual nucleus NNUR       *
Ccc   * through the emission of the ejectile NEJC.                       *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * input:NNUC - decaying nucleus index                              *
Ccc   *       IEC  - energy index of the decaying state                  *
Ccc   *       JC   - spin index of the decaying state                    *
Ccc   *       IPC  - parity of the decaying state                        *
Ccc   *       NNUR - residual nucleus index                              *
Ccc   *       NEJC - ejectile index                                      *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * output:SUM - SUM of transmission coefficients over all outgoing  *
Ccc   *              channels for the requested decay (partial sums are  *
Ccc   *              stored in SCRT and SCRTL arrays for continuum and   *
Ccc   *              discrete levels respectively. SUMs for all ejectiles*
Ccc   *              combine to the total Hauser-Feshbach denominator.   *
Ccc   *              Inverse of the latter multiplied by the population  *
Ccc   *              of the (NNUC,IEC,JC,IPC) state is used to normalize *
Ccc   *              SCRT and SCRTL matrices to give residual nucleus    *
Ccc   *              population.                                         *
Ccc   *                                                                  *
Ccc   * calls:TLLOC                                                      *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C
C Dummy arguments
C
      INTEGER Iec, Ipc, Jc, Nejc, Nnuc, Nnur
      DOUBLE PRECISION Sum
C
C Local variables
C
      DOUBLE PRECISION cor, corr, eout, eoutc, frde, hisr, s, smax,
     &                 smin, sumdl, sumtl1, sumtl2, xjc, xjr
      REAL FLOAT
      INTEGER i, ichsp, ier, iermax, ietl, iexc, il, ip1, ip2, ipar,
     &        itlc, j, jr, l, lmax, lmaxf, lmin, mul
      INTEGER INT, MIN0
C
C
      Sum = 0.0
      hisr = HIS(Nnur)
      xjc = FLOAT(Jc) + HIS(Nnuc)
C-----clear scratch matrices
      SCRtem(Nejc) = 0.d0
      DO j = 1, NLW
         DO i = 1, NEX(Nnur) + 1
            SCRt(i,j,1,Nejc) = 0.d0
            SCRt(i,j,2,Nejc) = 0.d0
         ENDDO
      ENDDO
      iexc = NEX(Nnuc) - NEXr(Nejc,Nnuc)
      itlc = iexc - 5
      iermax = Iec - iexc
      IF (iermax.GE.1) THEN
C-----
C-----decay to the continuum
C-----
C-----do loop over r.n. spins
         DO jr = 1, NLW, LTUrbo
            xjr = FLOAT(jr) + hisr
            smin = ABS(xjr - SEJc(Nejc))
            smax = xjr + SEJc(Nejc)
            mul = INT(smax - smin + 1.0001)
C-----------do loop over channel spin
            DO ichsp = 1, mul
               s = smin + FLOAT(ichsp - 1)
               lmin = INT(ABS(xjc - s) + 1.01)
               lmaxf = INT(xjc + s + 1.01)
               lmaxf = MIN0(NLW,lmaxf)
               ipar = (1 + Ipc*( - 1)**(lmin - 1))/2
C--------------parity index of r.n. state populated by emission with LMIN
               ip1 = 2 - ipar
C--------------parity index of r.n. state populated by emission with LMIN+1
               ip2 = 1
               IF (ip1.EQ.1) ip2 = 2
C--------------decay to the highest possible bin (non neutron only)
               IF (ZEJc(Nejc).NE.0.0D0) THEN
                  lmax = lmaxf
                  lmax = MIN0(LMAxtl(6,Nejc,Nnur),lmax)
C-----------------do loop over l (odd and even l-values treated separately)
C-----------------IP1 and IP2 decide to which parity each SUMTL  goes
                  sumtl1 = 0.0
                  DO l = lmin, lmax, 2
                     sumtl1 = sumtl1 + TL(5,l,Nejc,Nnur)
                  ENDDO
                  sumtl2 = 0.0
                  DO l = lmin + 1, lmax, 2
                     sumtl2 = sumtl2 + TL(5,l,Nejc,Nnur)
                  ENDDO
C-----------------do loop over l   ***done***
                  SCRt(iermax,jr,ip1,Nejc) = SCRt(iermax,jr,ip1,Nejc)
     &             + sumtl1*RO(iermax,jr,ip1,Nnur)*TURbo*TUNe(Nejc,Nnuc)
                  SCRt(iermax,jr,ip2,Nejc) = SCRt(iermax,jr,ip2,Nejc)
     &             + sumtl2*RO(iermax,jr,ip2,Nnur)*TURbo*TUNe(Nejc,Nnuc)
               ENDIF
C--------------decay to the highest but one bin (conditional see the next IF)
               IF (ZEJc(Nejc).EQ.0.0D0 .AND. Iec.EQ.NEX(Nnuc) - 1) THEN
                  lmax = lmaxf
                  lmax = MIN0(LMAxtl(6,Nejc,Nnur),lmax)
C-----------------do loop over l (odd and even l-values treated separately)
C-----------------IP1 and IP2 decide which parity each SUMTL  goes to
                  sumtl1 = 0.0
                  DO l = lmin, lmax, 2
                     sumtl1 = sumtl1 + TL(6,l,Nejc,Nnur)
                  ENDDO
                  sumtl2 = 0.0
                  DO l = lmin + 1, lmax, 2
                     sumtl2 = sumtl2 + TL(6,l,Nejc,Nnur)
                  ENDDO
C-----------------do loop over l   ***done***
C-----------------CORR in the next lines accounts for the Tl interpolation
C-----------------and integration over overlaping bins (2/3), it turned out it must
C-----------------be energy step and also emission step dependent
                  corr = 0.4444/(DE - XN(Nnur) + XN(1))
     &                   *TURbo*TUNe(Nejc,Nnuc)
                  SCRt(iermax,jr,ip1,Nejc) = SCRt(iermax,jr,ip1,Nejc)
     &               + sumtl1*RO(iermax,jr,ip1,Nnur)*corr
                  SCRt(iermax,jr,ip2,Nejc) = SCRt(iermax,jr,ip2,Nejc)
     &               + sumtl2*RO(iermax,jr,ip2,Nnur)*corr
               ENDIF
C--------------do loop over r.n. energies (highest bin and eventually the second
C--------------bin from the top excluded as already done)
               DO ier = iermax - 1, 1, -1
                  ietl = Iec - ier - itlc
                  lmax = lmaxf
                  lmax = MIN0(LMAxtl(ietl,Nejc,Nnur),lmax)
C-----------------do loop over l (odd and even l-values treated separately)
C-----------------IP1 and IP2 decide which parity each SUMTL  goes to
                  sumtl1 = 0.0
                  DO l = lmin, lmax, 2
                     sumtl1 = sumtl1 + TL(ietl,l,Nejc,Nnur)
                  ENDDO
                  sumtl2 = 0.0
                  DO l = lmin + 1, lmax, 2
                     sumtl2 = sumtl2 + TL(ietl,l,Nejc,Nnur)
                  ENDDO
C-----------------do loop over l   ***done***
C
                  SCRt(ier,jr,ip1,Nejc) = SCRt(ier,jr,ip1,Nejc)
     &               + sumtl1*RO(ier,jr,ip1,Nnur)*TURbo*TUNe(Nejc,Nnuc)
                  SCRt(ier,jr,ip2,Nejc) = SCRt(ier,jr,ip2,Nejc)
     &               + sumtl2*RO(ier,jr,ip2,Nnur)*TURbo*TUNe(Nejc,Nnuc)
               ENDDO
C--------------do loop over r.n. energies ***done***
            ENDDO
C-----------do loop over channel spins ***done***
         ENDDO
C--------do loop over and r.n. spins ***done***
C--------decay to the continuum ------ done -----------------------------
C--------trapezoidal integration of ro*tl in continuum for ejectile nejc
         DO j = 1, NLW, LTUrbo
            DO i = 1, iermax
               Sum = Sum + SCRt(i,j,1,Nejc) + SCRt(i,j,2,Nejc)
            ENDDO
            Sum = Sum - 0.5*(SCRt(1,j,1,Nejc) + SCRt(1,j,2,Nejc))
         ENDDO
         Sum = Sum*DE
C--------integration of ro*tl in continuum for ejectile nejc -- done ----
C--------
C--------decay to discrete levels
C-----
      ENDIF
      IF (RORed.NE.0.0D0) THEN
         DO i = 1, NLV(Nejc)
            SCRtl(i,Nejc) = 0.0
         ENDDO
         eoutc = EX(Iec,Nnuc) - Q(Nejc,Nnuc)
C--------do loop over discrete levels -----------------------------------
         DO i = 1, NLV(Nnur)
            eout = eoutc - ELV(i,Nnur)
C-----------level above the bin
C           2.18+
            IF (eout.LT.0.0D0) GOTO 100
            cor = 1.0
            sumdl = 0.0
            CALL TLLOC(Nnur,Nejc,eout,il,frde)
            smin = ABS(XJLv(i,Nnur) - SEJc(Nejc))
            smax = XJLv(i,Nnur) + SEJc(Nejc) + 0.01
            s = smin
C-----------loop over channel spin ----------------------------------------
   20       lmin = INT(ABS(xjc - s) + 1.01)
            lmax = INT(xjc + s + 1.01)
            lmax = MIN0(NLW,lmax)
C-----------do loop over l ------------------------------------------------
            DO l = lmin, lmax
               ipar = 1 + LVP(i,Nnur)*Ipc*( - 1)**(l - 1)
               IF (ipar.NE.0) sumdl = sumdl + TL(il,l,Nejc,Nnur)
     &                                + frde*(TL(il + 1,l,Nejc,Nnur)
     &                                - TL(il,l,Nejc,Nnur))
            ENDDO
C-----------do loop over l --- done ----------------------------------------
            s = s + 1.
            IF (s.LE.smax) GOTO 20
C-----------loop over channel spin ------ done ----------------------------
            sumdl = sumdl*RORed*cor*TUNe(Nejc,Nnuc)
            SCRtl(i,Nejc) = sumdl
            Sum = Sum + sumdl
         ENDDO
C--------do loop over discrete levels --------- done --------------------
      ENDIF
  100 DENhf = DENhf + Sum
      SCRtem(Nejc) = Sum
Cpr   WRITE(6,*) 'TOTAL SUM=',SUM
      END


      SUBROUTINE DECAYD(Nnuc)
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:ppu*
Ccc   *                       D E C A Y D                                *
Ccc   *                                                                  *
Ccc   *  Calculates gamma decay of discrete levels according to          *
Ccc   *  the decay scheme contained in the IBR matrix. Prints out        *
Ccc   *  the results and updates gamma spectrum matrix CSE(.,0,NNUC)     *
Ccc   *  Must be called after all particle emission is done.             *
Ccc   *  NOTE: no particle emission from discrete levels is considered   *
Ccc   *                                                                  *
Ccc   * input:NNUC - nucleus index (position) in the table               *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * output:none                                                      *
Ccc   *                                                                  *
Ccc   * calls:none                                                       *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C
C Dummy arguments
C
      INTEGER Nnuc
C
C Local variables
C
      DOUBLE PRECISION egd, gacs, popl
      INTEGER i, icse, j, j1, l
      INTEGER INT, NINT
C
C
      nejc = -1
      IF (nnuc.EQ.mt91) THEN
        nejc = 1
      ELSEIF (nnuc.EQ.mt649) THEN
         nejc = 2
      ELSEIF (nnuc.EQ.mt849) THEN
         nejc = 3
      ENDIF
      IF (IOUt.GT.2) WRITE (6,99005)
99005 FORMAT (1X,////,1X,27('*'),/,1X,'Discrete gamma transitions ',/,
     &        1X,27('*'),//)
      DO i = 1, NLV(Nnuc) - 1
         l = NLV(Nnuc) - i + 1
         IF (BR(l,1,2,Nnuc).EQ.0. .and. POPlv(l,Nnuc).GT.0. AND.
     &      ISIsom(l,Nnuc).EQ.0) THEN
C-----------Normal level without branching ratios
            IF (IOUt.GT.2) WRITE (6,99010) ELV(l,Nnuc), LVP(l,Nnuc)
     &                            *XJLv(l,Nnuc), POPlv(l,Nnuc)
99010       FORMAT (1X,//,5X,'Level of energy  ',F8.4,' MeV',
     &              ' and spin ',F6.1,' with population ',G13.5,
     &              ' mb is not depopulated (g.s. transition assumed)')
C-----------Well... let it go down to the ground state
            gacs = POPlv(l,Nnuc)
            POPlv(1,Nnuc) = POPlv(1,Nnuc) + gacs
            POPlv(l,Nnuc) = 0.0
            egd = ELV(l,Nnuc)
            icse = min(INT(2.0001 + egd/DE),ndecse)
            CSE(icse,0,Nnuc) = CSE(icse,0,Nnuc) + gacs/DE
            CSEmis(0,Nnuc) = CSEmis(0,Nnuc) + gacs
C-----------Add transition to the exclusive or inclusive gamma spectrum
            IF (ENDf(Nnuc).EQ.1) THEN
               POPcse(0,0,icse,INExc(Nnuc)) = POPcse(0,0,icse
     &          ,INExc(Nnuc)) + gacs/DE
            ELSEIF(ENDf(Nnuc).EQ.2) THEN
               CSE(icse,0,0) = CSE(icse,0,0) + gacs/DE
            ENDIF
         ELSEIF (POPlv(l,Nnuc).GT.0. AND. ISIsom(l,Nnuc).EQ.1 .AND.
     &           nejc.GT.0) THEN
C-----------Isomer state in the residue after n,p, or alpha emission
C-----------No gamma-decay of the isomeric state imposed
C-----------Add gamma cascade population to the direct population
            POPlv(l,Nnuc) = POPlv(l,Nnuc) + CSDirlev(l,nejc)
            IF (IOUt.GT.2) WRITE (6,99012) ELV(l,Nnuc), LVP(l,Nnuc)
     &                            *XJLv(l,Nnuc), POPlv(l,Nnuc)
99012       FORMAT (1X,//,5X,'Level of energy  ',F8.4,' MeV',
     &              ' and spin ',F6.1,' with final population ',G13.5,
     &              ' mb is an isomer')
C-----------We add it to the ground state to have correct total cross section
            POPlv(1,Nnuc) = POPlv(1,Nnuc) + POPlv(l,Nnuc)
         ELSEIF (POPlv(l,Nnuc).GT.0. AND. ISIsom(l,Nnuc).EQ.1) THEN
C-----------Isomer state in any other nucleus
C-----------No gamma-decay of the isomeric state imposed
            IF (IOUt.GT.2) WRITE (6,99012) ELV(l,Nnuc), LVP(l,Nnuc)
     &                            *XJLv(l,Nnuc), POPlv(l,Nnuc)
C-----------We add it to the ground state to have correct total cross section
            POPlv(1,Nnuc) = POPlv(1,Nnuc) + POPlv(l,Nnuc)
         ELSE
C-----------Normal level with branching ratios
            popl = POPlv(l,Nnuc)
            IF (popl.NE.0.0D0) THEN
               IF (IOUt.GT.2) WRITE (6,99015) ELV(l,Nnuc), LVP(l,Nnuc)
     &                               *XJLv(l,Nnuc), popl
99015          FORMAT (1X//,5X,'Decay of  ',F7.4,' MeV  ',F5.1,
     &                 ' level with final population ',G13.5,' mb',/,5X,
     &                 'Level populated ',4X,'E.gamma ',4X,
     &                 'Intensity  ',/)
               DO j = 1, NDBR
                  j1 = NINT(BR(l,j,1,Nnuc))
                  IF (j1.EQ.0) GOTO 100
                  IF (j1.GE.l) THEN
                     WRITE (6,99020)
99020                FORMAT (10X,
     &                       'WARNING: error in discrete level deca',
     &                       'y data',/,10X,
     &                       'Final level above the initial one',/,10X,
     &                       'Further decay not considered ')
                     WRITE (6,
     &'(10X,''WARNING: Nucleus '',I3,''-'',A2,                        ''
     &level '',I3)') INT(A(Nnuc)), SYMb(Nnuc), l
                     GOTO 99999
                  ENDIF
                  gacs = popl*BR(l,j,2,Nnuc)
                  POPlv(j1,Nnuc) = POPlv(j1,Nnuc) + gacs
                  gacs = gacs/(1 + BR(l,j,3,Nnuc))    ! int. conversion
                  egd = ELV(l,Nnuc) - ELV(j1,Nnuc)
                  icse = min(INT(2.0001 + egd/DE),ndecse)
                  CSE(icse,0,Nnuc) = CSE(icse,0,Nnuc) + gacs/DE
                  CSEmis(0,Nnuc) = CSEmis(0,Nnuc) + gacs
C-----------------Add transition to the exclusive gamma spectrum
C-----------------NOTE: internal conversion taken into account
                  IF (ENDf(Nnuc).EQ.1) THEN
                     POPcse(0,0,icse,INExc(Nnuc))
     &                = POPcse(0,0,icse,INExc(Nnuc)) + gacs/DE
                  ELSEIF(ENDf(Nnuc).EQ.2) THEN
                     CSE(icse,0,0) = CSE(icse,0,0) + gacs/DE
                  ENDIF
                  IF (IOUt.GT.2) WRITE (6,99025) ELV(j1,Nnuc),
     &                                  LVP(j1,Nnuc)*XJLv(j1,Nnuc), egd,
     &                                  gacs
99025             FORMAT (5X,F7.4,2X,F5.1,5X,F7.4,5X,G13.5,' mb')
               ENDDO
            ENDIF
         ENDIF
  100 ENDDO
99999 END

      SUBROUTINE DECAYD_DIR(Nnuc, Nejc)
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:ppu*
Ccc   *                       D E C A Y D _ D I R                        *
Ccc   *                                                                  *
Ccc   *  Calculates gamma decay of discrete levels according to          *
Ccc   *  the decay scheme contained in the IBR matrix. Special version   *
Ccc   *  to process direct population of discrete levels by a neutron,   *
Ccc   *  proton or alpha without adding gamma-transitions to spectra.    *
Ccc   *  Must be called after the decay of the first CN bin is done.     *
Ccc   *                                                                  *
Ccc   * input:Nnuc - nucleus  index (position) in the table              *
Ccc   *       Nejc - ejectile index (position) in the table              *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * output:none                                                      *
Ccc   *                                                                  *
Ccc   * calls:none                                                       *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C
C Dummy arguments
C
      INTEGER Nnuc
C
C Local variables
C
      DOUBLE PRECISION gacs, popl
      INTEGER i, j, j1, l
      INTEGER NINT
C
C
      DO i = 1, NLV(Nnuc) - 1
         l = NLV(Nnuc) - i + 1
         IF (BR(l,1,2,Nnuc).EQ.0. .and. CSDirlev(l,nejc).gt.0. ) THEN
C-----------Well... let it go down to the ground state
            gacs = CSDirlev(l,nejc)
            CSDirlev(1,Nejc) = CSDirlev(1,Nejc) + gacs
            CSEmis(0,Nnuc) = CSEmis(0,Nnuc) + gacs
         ELSE
            popl = CSDirlev(l,nejc)
            IF (popl.NE.0.0D0) THEN
               DO j = 1, NDBR
                  j1 = NINT(BR(l,j,1,Nnuc))
                  IF (j1.EQ.0) GOTO 100
                  IF (j1.GE.l) return
                  gacs = popl*BR(l,j,2,Nnuc)
                  CSDirlev(j1,Nejc) = CSDirlev(j1,Nejc) + gacs
                  gacs = gacs/(1 + BR(l,j,3,Nnuc))    ! int. conversion
                  CSEmis(0,Nnuc) = CSEmis(0,Nnuc) + gacs
               ENDDO
            ENDIF
         ENDIF
  100 ENDDO
      END


      SUBROUTINE DECAYG(Nnuc,Iec,Jc,Ipc,Sum)
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:PPu*
Ccc   *                         D E C A Y G                              *
Ccc   *                (function to function version)                    *
Ccc   *                                                                  *
Ccc   * Calculates gamma decay of a continuum state in nucleus NNUC into *
Ccc   * continuum and discrete states in the same nucleus NNUC           *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * input:NNUC - decaying nucleus index                              *
Ccc   *       IEC  - energy index of the decaying state                  *
Ccc   *       JC   - spin index of the decaying state                    *
Ccc   *       IPC  - parity of the decaying state                        *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * output:SUM - SUM of transmission coefficients over all outgoing  *
Ccc   *              channels for the requested decay (partial sums are  *
Ccc   *              stored in SCRT and SCRTL arrays for continuum and   *
Ccc   *              discrete levels respectively. SUMs for all ejectiles*
Ccc   *              combine to the total Huser-Feshbach denominator.    *
Ccc   *              Inverse of the latter multiplied by the population  *
Ccc   *              of the (NNUC,IEC,JC,IPC) state is used to normalize *
Ccc   *              SCRT and SCRTL matrices to give residual nucleus    *
Ccc   *              population.                                         *
Ccc   *                                                                  *
Ccc   * calls:none                                                       *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   ********************************************************************
C *                                                                          *
C * MAXmult - maximal value (=< 10) of gamma-ray multipolarity (L) in        *
C *          calculations of gamma-transitions both between states in        *
C *          continuum and from continuum states to discrete levels;         *
C *          variable 'MAXmult' is transmitted in 'global.h';                *
C *          a value of 'MAXmult' is set in modul 'input.f';                 *
C *          it is equal to 2 by default (SUBROUTINE INPUT) or can be        *
C *          reading from 'input.dat' in other cases (SUBROUTINE READIN).    *
C *                                                                          *
C *                                                                          *
C * E1, M1 and E2 transitions are only taken into account if 'MAXmult =2'.   *
C *                                                                          *
C *                                                                          *
C * Radiative strength functions of higher multipole orders(f_EL, f_ML)      *
C * are calculated with the use of the relationships between                 *
C * single-particle radiative strength functions in the Weisskopf form.      *
C *                                                                          *
C *   Electric transitions:                                                  *
C *   f_E(L+1)/f_EL = eg^2*cee*[(3+L)/(5+L)]^2,                              *
C *   cee=[R/(\hbar*c)]^2, R=r_0*A^(2/3), r_0=1.2 fm => cee=3.7D-5*A^(2/3)   *
C *   xle(i) = f_Ei                                                          *
C *                                                                          *
C *   Magnetic transitions:                                                  *
C *   f_M(L+1)/f_E(L+1) = cme,                                               *
C *   cme= 10[\hbar/(m*c*R]^2 => cme = 0.307/A^(2/3)                         *
C *   xlm(i) = f_Mi                                                          *
C *                                                                          *
C ****************************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C
C Dummy arguments
C
      INTEGER Iec, Ipc, Jc, Nnuc
      DOUBLE PRECISION Sum
C
C Local variables
C
      DOUBLE PRECISION E1, E2, XM1
      DOUBLE PRECISION eg, xjc
      REAL FLOAT
      INTEGER i, ier, ineg, iodd, ipar, ipos, j, jmax, jmin, lmax, lmin
      INTEGER MAX0, MIN0
C
C-----Plujko_new-2005
      INTEGER Jr,lamb, lambmin, lambmax
      DOUBLE PRECISION ha, cee, cme, xle, xlm, xjr,
     &                 scrtpos, scrtneg, hscrtl
      DIMENSION xle(10),xlm(10)
C
C-----MAXmult - maximal gamma-ray multipolarity
      DO i = 1, MAXmult
         xle(i) = 0.0D0
         xlm(i) = 0.0D0
      ENDDO
      IF (MAXmult.GT.2) THEN
         ha = A(Nnuc)**0.666666666666D0
         cee = 3.7D-5*ha
         cme = 0.307D0/ha
      ENDIF
C
Cp    jmin = MAX0(1, Jc - 2)
Cp    jmax = MIN0(NLW, Jc + 2)
      jmin = 1
Cp    jmin = MAX0(1, Jc - MAXmult)
      jmax = MIN0(NLW, Jc + MAXmult)
C
      Sum = 0.d0
      SCRtem(0) = 0.d0
      xjc = FLOAT(Jc) + HIS(Nnuc)
C     WRITE(6,*) ' decaying state spin=',xjc,' and parity=',ipc
C-----clear scratch matrix (continuum)
      DO j = 1, NLW
         DO i = 1, NEX(Nnuc)
            SCRt(i,j,1,0) = 0.0
            SCRt(i,j,2,0) = 0.0
         ENDDO
      ENDDO
C-----clear scratch matrix (discrete levels)
      DO i = 1, NLV(Nnuc)
         SCRtl(i,0) = 0.0
      ENDDO
C-----IPOS is a parity-index of final states reached by gamma
C-----transitions which do not change parity (E2 and M1)
C-----INEG is a parity-index of final states reached by gamma
C-----transitions which do change parity (E1)
      IF (Iec.LT.1) RETURN
      IF (Ipc.GT.0) THEN
         ipos = 1
         ineg = 2
      ELSE
         ipos = 2
         ineg = 1
      ENDIF
C-----
C-----decay to the continuum
C-----
C-----do loop over c.n. energies (loops over spins and parities expanded
      DO ier = Iec - 1, 1, -1
         eg = EX(Iec,Nnuc) - EX(ier,Nnuc)
C--------Plujko_new-2005
         xle(1) = E1(Nnuc,Z,A,eg, TNUc(ier, Nnuc),Uexcit(ier,Nnuc))*
     &            TUNe(0, Nnuc)
         xlm(1) = XM1(eg)*TUNe(0, Nnuc)
         xle(2) = E2(eg)*TUNe(0, Nnuc)
         IF(MAXmult.GT.2) THEN
            xlm(2) = xle(2)*cme
            DO i = 3, MAXmult
             xle(i) = xle(i-1)*eg**2*cee
     &                *((3.0D0 + FLOAT(i))/(5.0D0 + FLOAT(i)))**2
             xlm(i) = xle(i)*cme
            ENDDO
         ENDIF
C
         DO Jr = 1, jmax
            xjr = FLOAT(Jr) + HIS(Nnuc)
            lambmin = MAX0(1,IABS(Jc-Jr))
            lambmax = xjc + xjr + 0.001
            lambmax = MIN0(lambmax,MAXmult)
            IF(lambmin.LE.lambmax)THEN
            scrtpos = 0.0
               scrtneg = 0.0
               DO lamb = lambmin, lambmax
                 IF(lamb/2*2.EQ.lamb)THEN
                   scrtpos = scrtpos + xle(lamb)
                   scrtneg = scrtneg + xlm(lamb)
                 ELSE
                   scrtpos = scrtpos + xlm(lamb)
                   scrtneg = scrtneg + xle(lamb)
                 ENDIF
               ENDDO
               SCRt(ier, Jr, ipos, 0) = scrtpos*RO(ier, Jr, ipos, Nnuc)
               SCRt(ier, Jr, ineg, 0) = scrtneg*RO(ier, Jr, ineg, Nnuc)
            ENDIF
           ENDDO
      ENDDO
C-----do loop over c.n. energies ***done***
C-----decay to the continuum ----** done***-----------------------
C
C-----integration of ro*gtl in continuum for ejectile 0 (TRAPEZOID
      DO j = jmin, jmax
         DO i = 1, Iec - 1
            Sum = Sum + SCRt(i,j,1,0) + SCRt(i,j,2,0)
         ENDDO
         Sum = Sum - 0.5*(SCRt(1,j,1,0) + SCRt(1,j,2,0))
      ENDDO
      Sum = Sum*DE
C-----integration of ro*gtl in continuum for ejectile 0 -- done ----
C-----
C-----DECAY TO DISCRETE LEVELS
C-----
      IF (RORed.NE.0.0D0) THEN
C-----do loop over discrete levels -----------------------------------
         DO i = 1, NLV(Nnuc)
          lmin = ABS(xjc - XJLv(i,Nnuc)) + 0.001
          lmax = xjc + XJLv(i,Nnuc) + 0.001
C---------Plujko_new-2005
          lambmin = MAX0(1,lmin)
          lambmax = MIN0(lmax,MAXmult)
          IF(lambmin.LE.lambmax)THEN
             eg = EX(Iec, Nnuc) - ELV(i, Nnuc)
             ipar = (1 + LVP(i, Nnuc)*Ipc)/2
             iodd = 1 - ipar
             xle(1) = E1(Nnuc,Z,A,eg, TNUc(1, Nnuc),Uexcit(1,Nnuc))
             xlm(1) = XM1(eg)
             IF(lambmax.GE.2) xle(2) = E2(eg)
             IF(lambmax.GT.2) THEN
              xlm(2) = xle(2)*cme
              DO j = 3, lambmax
               xle(j) = xle(j-1)*eg**2*cee
     &                  *((3.0D0 + FLOAT(j))/(5.0D0 + FLOAT(j)))**2
               xlm(j) = xle(j)*cme
              ENDDO
             ENDIF
             hscrtl = 0.0D0
             DO lamb = lambmin, lambmax
              IF(lamb/2*2.EQ.lamb)THEN
               hscrtl = hscrtl +
     &                  xle(lamb)*ipar + xlm(lamb)*iodd
               ELSE
               hscrtl = hscrtl +
     &                  xlm(lamb)*ipar + xle(lamb)*iodd
              ENDIF
             ENDDO
          SCRtl(i, 0) = hscrtl*RORed*TUNe(0, Nnuc)
          Sum = Sum + SCRtl(i, 0)
          ENDIF
         ENDDO
C-----do loop over discrete levels --------- done --------------------
      ENDIF
      SCRtem(0) = Sum
      DENhf = DENhf + Sum
      END


      SUBROUTINE DECAYT(Nnuc,Iec,Jc,Ipc,Sum)
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:PPu*
CCC   *                         D E C A Y T                              *
Ccc   *                (function to function version)                    *
Ccc   *                                                                  *
Ccc   * Calculates gamma decay of a continuum state in nucleus NNUC into *
Ccc   * continuum and discrete states in the same nucleus NNUC           *
Ccc   * (to be used instead of DECAYG if TURBO mode is invoked)          *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * input:NNUC - decaying nucleus index                              *
Ccc   *       IEC  - energy index of the decaying state                  *
Ccc   *       JC   - spin index of the decaying state                    *
Ccc   *       IPC  - parity of the decaying state                        *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * output:SUM - SUM of transmission coefficients over all outgoing  *
Ccc   *              channels for the requested decay (partial sums are  *
Ccc   *              stored in SCRT and SCRTL arrays for continuum and   *
Ccc   *              discrete levels respectively. SUMs for all ejectiles*
Ccc   *              combine to the total Huser-Feshbach denominator.    *
Ccc   *              Inverse of the latter multiplied by the population  *
Ccc   *              of the (NNUC,IEC,JC,IPC) state is used to normalize *
Ccc   *              SCRT and SCRTL matrices to give residual nucleus    *
Ccc   *              population.                                         *
Ccc   *                                                                  *
Ccc   * calls:none                                                       *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   ********************************************************************
C ****************************************************************************
C *                                                                          *
C * MAXmult - maximal value (=< 10) of gamma-ray multipolarity (L) in        *
C *          calculations of gamma-transitions both between states in        *
C *          continuum and from continuum states to discrete levels;         *
C *          variable 'MAXmult' is transmitted in 'global.h';                *
C *          a value of 'MAXmult' is set in modul 'input.f';                 *
C *          it is equal to 2 by default (SUBROUTINE INPUT) or can be        *
C *          reading from 'input.dat' in other cases (SUBROUTINE READIN).    *
C *                                                                          *
C ****************************************************************************
C * E1, M1 and E2 transitions are only taken into account if 'MAXmult =2'.   *
C ****************************************************************************
C *                                                                          *
C * Radiative strength functions of higher multipole orders(f_EL, f_ML)      *
C * are calculated with the use of the relationships between                 *
C * single-particle radiative strength functions in the Weisskopf form.      *
C *                                                                          *
C *   Electric transitions:                                                  *
C *   f_E(L+1)/f_EL = eg^2*cee*[(3+L)/(5+L)]^2,                              *
C *   cee=[R/(\hbar*c)]^2, R=r_0*A^(2/3), r_0=1.2 fm => cee=3.7D-5*A^(2/3)   *
C *   xle(i) = f_Ei                                                          *
C *                                                                          *
C *   Magnetic transitions:                                                  *
C *   f_M(L+1)/f_E(L+1) = cme,                                               *
C *   cme= 10[\hbar/(m*c*R]^2 => cme = 0.307/A^(2/3)                         *
C *   xlm(i) = f_Mi                                                          *
C *                                                                          *
C ****************************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C
C Dummy arguments
C
      INTEGER Iec, Ipc, Jc, Nnuc
      DOUBLE PRECISION Sum
C
C Local variables
C
      DOUBLE PRECISION E1, E2, XM1
      DOUBLE PRECISION eg, sumn, sump, xjc
      REAL FLOAT
      INTEGER i, ier, ineg, iodd, ipar, ipos, j, lmax, lmin
C-----Plujko_new-2005
      INTEGER Jr, lamb, lambmin, lambmax
      DOUBLE PRECISION ha, cee, cme, xle, xlm, xjr, hscrtl
      DIMENSION xle(10),xlm(10)
C-----MAXmult - maximal gamma-ray multipolarity
      DO i = 1, MAXmult
         xle(i) = 0.0D0
         xlm(i) = 0.0D0
      ENDDO
      IF (MAXmult.GT.2) THEN
         ha = A(Nnuc)**0.666666666666D0
         cee = 3.7D-5*ha
         cme = 0.307D0/ha
      ENDIF
C
      Sum = 0.0
      SCRtem(0) = 0.0
      xjc = FLOAT(Jc) + HIS(Nnuc)
C-----clear scratch matrix (continuum)
      DO j = 1, NLW
         DO i = 1, NEX(Nnuc)
            SCRt(i,j,1,0) = 0.d0
            SCRt(i,j,2,0) = 0.d0
         ENDDO
      ENDDO
C-----clear scratch matrix (discrete levels)
      DO i = 1, NLV(Nnuc)
         SCRtl(i,0) = 0.d0
      ENDDO
C-----IPOS is a parity-index of final states reached by gamma
C-----transitions which do not change parity (E2 and M1)
C-----INEG is a parity-index of final states reached by gamma
C-----transitions which do change parity (E1)
      IF (Iec.LT.1) RETURN
      IF (Ipc.GT.0) THEN
         ipos = 1
         ineg = 2
      ELSE
         ipos = 2
         ineg = 1
      ENDIF
C-----
C-----decay to the continuum
C-----
C-----do loop over c.n. energies (loops over spins and parities expanded
      DO ier = Iec - 1, 1, -1
        IF (RO(ier,Jc,1,Nnuc).NE.0.D0 .OR.
     &      RO(ier,Jc,2,Nnuc).NE.0.D0) THEN

           eg = EX(Iec,Nnuc) - EX(ier,Nnuc)
           xle(1) = E1(Nnuc,Z,A,eg, TNUc(ier, Nnuc),Uexcit(ier,Nnuc))*
     &            TUNe(0, Nnuc)
           xlm(1) = XM1(eg)*TUNe(0, Nnuc)
           xle(2) = E2(eg)*TUNe(0, Nnuc)
           IF(MAXmult.GT.2) THEN
             xlm(2) = xle(2)*cme
             DO i = 3, MAXmult
               xle(i) = xle(i-1)*eg**2*cee
     &                *((3.0D0 + FLOAT(i))/(5.0D0 + FLOAT(i)))**2
               xlm(i) = xle(i)*cme
             ENDDO
           ENDIF
           sump = 0.0D0
           sumn = 0.0D0
           DO Jr=1, NLW
            xjr = FLOAT(Jr) + HIS(Nnuc)
            lambmin = MAX0(1,IABS(Jc-Jr))
            lambmax = xjc + xjr + 0.001
            lambmax = MIN0(lambmax,MAXmult)
            IF(lambmin.LE.lambmax)THEN
               DO lamb = lambmin, lambmax
                 IF(lamb/2*2.EQ.lamb)THEN
                   sump = sump + xle(lamb)*RO(ier, Jr, ipos, Nnuc)
                   sumn = sumn + xlm(lamb)*RO(ier, Jr, ineg, Nnuc)
                 ELSE
                   sump = sump + xlm(lamb)*RO(ier, Jr, ipos, Nnuc)
                   sumn = sumn + xle(lamb)*RO(ier, Jr, ineg, Nnuc)
                 ENDIF
               ENDDO
            ENDIF
           ENDDO
           SCRt(ier, Jc, ipos, 0) = sump
           SCRt(ier, Jc, ineg, 0) = sumn
        ENDIF
      ENDDO
C-----do loop over c.n. energies ***done***
C-----decay to the continuum ----** done***---------------------------
C-----integration of ro*gtl in continuum for ejectile 0 (TRAPEZOID
      DO i = 1, Iec - 1
         Sum = Sum + SCRt(i,Jc,1,0) + SCRt(i,Jc,2,0)
      ENDDO
      Sum = Sum - 0.5*(SCRt(1,Jc,1,0) + SCRt(1,Jc,2,0))
      Sum = Sum*DE
C-----integration of ro*gtl in continuum for ejectile 0 -- done ----
C-----
C-----DECAY TO DISCRETE LEVELS
C-----
      IF (RORed.NE.0.0D0) THEN
C--------do loop over discrete levels -----------------------------------
         DO i = 1, NLV(Nnuc)
          lmin = ABS(xjc - XJLv(i,Nnuc)) + 0.001
          lmax = xjc + XJLv(i,Nnuc) + 0.001
C---------Plujko_new-2005
          lambmin = MAX0(1,lmin)
          lambmax = MIN0(lmax,MAXmult)
          IF(lambmin.LE.lambmax)THEN
            eg = EX(Iec, Nnuc) - ELV(i, Nnuc)
            ipar = (1 + LVP(i, Nnuc)*Ipc)/2
            iodd = 1 - ipar
            xle(1) = E1(Nnuc,Z,A,eg, TNUc(1, Nnuc),Uexcit(1,Nnuc))
            xlm(1) = XM1(eg)
            IF(lambmax.GE.2) xle(2) = E2(eg)
            IF(lambmax.GT.2) THEN
              xlm(2) = xle(2)*cme
              DO j = 3, lambmax
               xle(j) = xle(j-1)*eg**2*cee
     &                  *((3.0D0 + FLOAT(j))/(5.0D0 + FLOAT(j)))**2
               xlm(j) = xle(j)*cme
              ENDDO
            ENDIF
            hscrtl = 0.0D0
            DO lamb = lambmin, lambmax
              IF(lamb/2*2.EQ.lamb)THEN
               hscrtl = hscrtl +
     &                  xle(lamb)*ipar + xlm(lamb)*iodd
              ELSE
               hscrtl = hscrtl +
     &                  xlm(lamb)*ipar + xle(lamb)*iodd
              ENDIF
            ENDDO
            SCRtl(i, 0) = hscrtl*RORed*TUNe(0, Nnuc)
            Sum = Sum + SCRtl(i, 0)
          ENDIF
         ENDDO
C-----do loop over discrete levels --------- done --------------------
      ENDIF
      SCRtem(0) = Sum
      DENhf = DENhf + Sum
      END


      SUBROUTINE FISSION(Nnuc,Iec,Jc,Sumfis)
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:ppu*
Ccc   *                         F I S S I O N                            *
Ccc   *                                                                  *
Ccc   *    Calculates fission of the nuclear state defined by NNUC,      *
Ccc   *    IEC, and JC including two viscosity effects.                  *
Ccc   *    Corrected trapezoidal integration rule used.                  *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * input:NNUC-decaying nucleus index                                *
Ccc   *       IEC -decaying state excitation energy index                *
Ccc   *       JC  -decaying state spin index                             *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * output:SUMFIS-integral of Tf*ro for the fission channel over     *
Ccc   *            kinetic energy of fission fragments (to be added      *
Ccc   *            to the Hauser-Feshbach denominator DENHF)             *
Ccc   *                                                                  *
Ccc   * calls:TLF                                                        *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C
C Dummy arguments
C
      INTEGER Iec, Jc, Nnuc
      DOUBLE PRECISION Sumfis
C
C Local variables
C
      DOUBLE PRECISION accn, ampl, atil, ekin, ekinm, erest, fisba,
     &                 fric, gamma, gpart, htom, shredt, sum1, sum2,
     &                 sum3, sumf, sumgs, sumr, tau, temp
      REAL FLOAT
      INTEGER kn, knm
      DOUBLE PRECISION TLF
C
C

      Sumfis = 0.0
      IF (EX(Iec,Nnuc).EQ.0.0D0) RETURN
C-----temperature fade-out of the shell correction
      temp = 0.
      atil = 0.073*A(Nnuc) + 0.115*A(Nnuc)**0.666667
      gamma = 0.40/A(Nnuc)**0.33333
      accn = atil*(1 + SHC(Nnuc)*(1 - EXP((-gamma*EX(Iec,Nnuc))))
     &       /EX(Iec,Nnuc))
      IF (EX(Iec,Nnuc).GE.YRAst(Jc,Nnuc))
     &    temp = SQRT((EX(Iec,Nnuc) - YRAst(Jc,Nnuc))/accn)
      ampl = EXP(TEMp0*SHRt)
      shredt = 1.
      IF (temp.GE.TEMp0) shredt = ampl*EXP(( - SHRt*temp))
C-----temperature fade-out of the shell correction *** done *****
      fisba = FISb(Jc,Nnuc) - SHC(Nnuc)*SHCjf(Jc,Nnuc)*shredt
      ekinm = EX(Iec,Nnuc) - fisba
C
      IF (ekinm.LT.0.0D0) RETURN
      knm = AINT(ekinm/DE + 1.001)
      erest = ekinm - (knm - 1)*DE
C-----IEC to g.s.
      sumgs = TLF(ekinm)
C-----IEC to IEC
      sum1 = TLF(0.0D0)*ROF(Iec,Jc,Nnuc)
      IF (knm.EQ.1) THEN
         Sumfis = 0.5*(sumgs + sum1)*erest
         GOTO 100
      ENDIF
C-----IEC to IEC-1
      sum2 = TLF(DE)*ROF(Iec - 1,Jc,Nnuc)
C-----IEC to g.s.+1
      sum3 = TLF(FLOAT(knm - 1)*DE)*ROF(Iec - knm + 1,Jc,Nnuc)
      Sumfis = 0.5*((sumgs + sum3)*erest + (sum1 + sum2)*DE)
C-----correction to the trapezoidal integration rule
      Sumfis = Sumfis + ((sum3 - sumgs)*erest - (sum1 - sum2)*DE)/12.
      IF (knm.NE.2) THEN
         Sumfis = Sumfis + 0.5*(sum3 + sum2)*DE
         IF (knm.NE.3) THEN
C-----------
C-----------do loop over kinetic energy of fission fragments
C-----------
            sumr = 0.0
            DO kn = Iec - 2, Iec - knm + 2, -1
               ekin = EX(Iec,Nnuc) - EX(kn,Nnuc)
               sumf = TLF(ekin)*ROF(kn,Jc,Nnuc)
               sumr = sumr + sumf
            ENDDO
            Sumfis = Sumfis + sumr*DE
         ENDIF
      ENDIF
  100 IF (Sumfis.EQ.0.0D0) RETURN
      Sumfis = Sumfis*TUNEfi(Nnuc)
      IF (BETav.NE.0.0D0) THEN
C--------reduction of the fission width due to possible return from the
C--------saddle point (nuclear viscosity 1-st effect)
         htom = 1.0
         Sumfis = Sumfis*(SQRT(1.0 + (BETav/2./htom)**2)
     &            - BETav/2./htom)
         IF (fisba - YRAst(Jc,Nnuc).GT.0.0D0) THEN
C-----------reduction of fission width due to the transient time needed
C-----------to form a saddle point (nuclear viscosity 2-nd effect)
C-----------according to Rastopchin et al. Sov. Jour. Nucl. Phys. 53,741(1991)
C-----------omega1=omega0=1.6*10^21 (1/s) hbar*omega=1MeV
C-----------BETAV critical =3.2; 0.19531 stands for 1/(2*omega1**2)
            gpart = DENhf/(RO(Iec,Jc,1,Nnuc)+RO(Iec,Jc,2,Nnuc))/2.d0/PI
C           GFIS = SUMFIS/RO(IEC,JC,NNUC)/2./PI
            tau = LOG(10.0*(fisba - YRAst(Jc,Nnuc))/temp)
            IF (BETav.LT.3.2D0) THEN
               tau = tau/BETav
            ELSE
               tau = tau*BETav*0.19531
            ENDIF
            fric = gpart*tau/0.6589
            fric = MIN(EXPmax,fric)
            IF (fric.GT.0D0) THEN
               fric = EXP(( - fric))
               Sumfis = Sumfis*fric
            ENDIF
         ENDIF
      ENDIF
      DENhf = DENhf + Sumfis
      END


      DOUBLE PRECISION FUNCTION TLF(Ekin)
C-----energy dependent transmission coefficient (note that htom is
C-----fixed below and does not depend on angular momentum as it might)
C
C
C Dummy arguments
C
      DOUBLE PRECISION Ekin
C
C Local variables
C
      DOUBLE PRECISION atlf, htom, pix2
C
C
      DATA pix2/6.28318530717958647692528676655901D0/
      DATA htom/1.D0/
      TLF = 1.D0
      atlf = pix2*Ekin/htom
      IF (atlf.LT.38.D0) TLF = 1./(1. + EXP((-atlf)))
      END


      SUBROUTINE FISFIS(Nnuc,Iec,Ip,Jc,Sumfis,Mmod)
Ccc   ********************************************************************
Ccc   *                                                         class:ppu*
Ccc   *                         F I S F I S                              *
Ccc   *                                                                  *
Ccc   *    Calculates fission of the nuclear state defined by NNUC,      *
Ccc   *    IEC, IP and JC within optical model for fission               *
Ccc   *    using double or triple humped barrier assumptions             *
Ccc   *                                                                  *
Ccc   * input:NNUC-decaying nucleus index                                *
Ccc   *       IEC -decaying state excitation energy index                *
Ccc   *       IP  -decaying state parity                                 *
Ccc   *       JC  -decaying state spin index                             *
Ccc   *       mmod-number of modes (multimodal fission)                  *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * output:SUMFIS - total fission transmission coefficient (to be
Ccc                     added to the Hauser-Feshbach denominator DENHF)  *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * calls:...                                                        *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   ********************************************************************
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C
C COMMON variables
C
      DOUBLE PRECISION HO(NFPARAB), TABs, TDIr,
     &                 TDIr23, TF(NFPARAB), TFCc,
     &                 TFD(NRBar),TG2, VBArex(NFPARAB)

      DOUBLE PRECISION tdirf(NRBar), tdirb(NRBar)
      DOUBLE PRECISION tabsf(NRBar),tabsb(NRBar)
      DOUBLE PRECISION tabs1,tabs2,tabs12,tabs21, tind1, tind2
      DOUBLE PRECISION sumtk, sumtj, rap,barmin

      INTEGER JCC

      COMMON /IMAG  / TF, TDIr, TABs, TDIr23, TG2
      COMMON /NUMBAR/  eps_1d, vdef_1d, npoints, iiextr, nextr

      DOUBLE PRECISION tff(NRBar)
      DOUBLE PRECISION vdef_1d(800), eps_1d(800)
C
C Dummy arguments
C
      INTEGER Iec, Ip, Jc, Mmod, Nnuc,npoints
      DOUBLE PRECISION Sumfis
C
C Local variables
C
      DOUBLE PRECISION aj, ee, tdircont(NRBarc),
     &                 exfis, sfmin, snc,
     &                 tfcon(NRBarc), tfdis(NRBarc),
     &                 tweightw(2), 
     &                 vbarmax(NFPARAB)
      REAL FLOAT
      INTEGER ibar, ist, jnc, nr, ipa, nrhump
C
      ee = EX(Iec,Nnuc)
      Sumfis = 0.D0
      IF (ee.EQ.0.0D0) RETURN

      JCC = Jc

      nrhump = nrbar - nrwel
      DO ibar = 1, nrhump         !NRBarc
         tfdis(ibar) = 0.d0
         tfcon(ibar) = 0.d0
         tdircont(ibar) = 0.d0
         tf(ibar)=0.d0
      ENDDO
      DO ibar = 1, NRBar
         vbarmax(ibar) = 0.d0
      ENDDO

      DO iv = 1, NRWel
         tweightw(iv)=0.d0
      ENDDO

      TABs = 0.D0
      tabss=0.d0
      tabs1 = 0.d0
      tabs2 = 0.d0
      tabs12 = 0.d0
      tabs21 = 0.d0

      tdir = 0.d0
      tdir21 = 0.d0
      tdir23 = 0.d0
      sumtk = 0.d0
      sumtj = 0.d0

      DO k=1, NRBar,2   ! de vazut cu hcont !!!!!!!!!!!!!!!!!!!!!!!!!!
         HO(k) = H(1, int(k/2)+1)
      ENDDO
      DO k=2, NRBar,2
         HO(k) = H(1, NRBarc+int(k/2))
      ENDDO

c-----discrete contribution

c-----numerical barrier from RIPL-3; no discrete transition states, only gs
      IF(FISbar(Nnuc).EQ.3.)THEN
         IF (Jc.EQ.1 .AND. Ip.EQ.1) THEN
             DO ibar = 1, NRBar
                VBArex(ibar) = EFB(ibar)
             ENDDO

             CALL NUMBARR(Nnuc,Vbarex,ho)
             CALL WKBFIS1(Ee, nnuc, tff, tdirf, tdirb, tabsf, tabsb)

             DO k = 1, nrhump !NRBarc
                TFD(k) = Tff(2*(k-1)+1)
             ENDDO
             DO ibar = 1, NRBarc
                tfdis(ibar) = tfdis(ibar) + TFD(ibar)
             ENDDO
          ENDIF
          GOTO 700
       ENDIF

c-----parabolic barriers
      DO nr = 1, NRFdis(1)
         sfmin = SFDis(nr,1)
         ist = 1
         IF (SFDis(nr,1).EQ.0.0 .AND. IPFdis(nr,1).EQ.1) THEN
            sfmin = 0.
            ist = 2
         ENDIF
         IF (SFDis(nr,1).EQ.0.0 .AND. IPFdis(nr,1).EQ. - 1) THEN
            sfmin = 1.
            ist = 2
         ENDIF
         sfmin = sfmin - HIS(Nnuc)

         DO jnc = INT(sfmin), Jc, ist
            IF (jnc.EQ.Jc .AND. IPFdis(nr,1).EQ.Ip) THEN
               DO k=1, NRBar,2
                  HO(k) = H(nr, int(k/2)+1)
               ENDDO
               DO k=2, NRBar,2
                  HO(k) = H(nr, NRBarc+int(k/2))
               ENDDO
               snc = FLOAT(jnc) + HIS(Nnuc)
               DO ibar = 1, NRBar
                  exfis = EFDis(nr,ibar) + HJ(Nnuc,ibar)
     &                    *(snc*(snc + 1) - SFDis(nr,ibar)
     &                    *(SFDis(nr,ibar) + 1))
                  VBArex(ibar) = EFB(ibar) + exfis
                  IF(nr.eq.1) vbarmax(ibar) = exfis
                  IF(exfis.gt.vbarmax(ibar)) vbarmax(ibar) = exfis
               ENDDO

               CALL NUMBARR(Nnuc,Vbarex,HO)
               CALL WKBFIS1(Ee, nnuc, tff, tdirf, tdirb, tabsf, tabsb)

               DO k=1, NRBarc
                  TFD(k) = Tff(2*(k-1)+1)
               ENDDO
               DO ibar = 1, NRBarc
                  tfdis(ibar) = tfdis(ibar) + TFD(ibar)
               ENDDO

               IF(FISopt(Nnuc).GT.0.)THEN
                  IF (tabsb(1) + tdirb(1).GT.1.) tdirb(1) = 0.d0
                  tdir = tdir + tdirb(1)
                  tabs1 = tabs1 + tabsb(1)

                  IF(NRBar.EQ.5)THEN
                     tabs2  = tabs2  + tabsf(5)
                     tabs12 = tabs12 + tabsb(3)
                     tabs21 = tabs21 + tabsf(3)
                     tdir21 = tdir21 + tdirf(3)
                     tdir23 = tdir23 + tdirb(3)
                  ENDIF
               ENDIF
            ENDIF
         ENDDO
      ENDDO
C----continuum contribution
 700  aj = FLOAT(Jc) + HIS(Nnuc)
      IF(Ip.EQ.1)  ipa = 1
      IF(Ip.EQ.-1) ipa = 2

c-----continuum direct and indirect weights
c      barmin =  vbarmax(1) + efb(1)
      barmin =   efb(1)
      DO iw = 1, NRWel
         IF(awf(iw).EQ.0.d0)THEN
            tweightw(iw) = 1.d0
            goto 650!CYCLE
         ENDIF
         barmin =  vbarmax(iw) + efb(iw)
c        IF(barmin.GT.(vbarmax(iw) + efb(iw)))
c     &     barmin =  vbarmax(iw) + efb(iw)
c         IF(barmin.GT.efb(iw))
c     &     barmin =  efb(iw)
         tweightw(iw) = 2.d0 * (Ee - efb(NRBarc + iw)) /
     &                    ((barmin - efb(NRBarc + iw)) *
     &                    (1.d0 + dexp( - (Ee - barmin) / awf(iw))))
         IF(Ee.GT.barmin) tweightw(iw) = 1.d0
 650     IF(Ee.LE.efb(NRBarc + iw)) tweightw(iw) = 0.d0
      ENDDO

c---- Direct transmission of continuum at subbarrier energies
      IF(tweightw(1).EQ.1.d0) GOTO 800
      IF(FISbar(Nnuc).EQ.3.)THEN
         CALL SIMPSNumTDIR(Nnuc,Ee,JCC,Ipa,tdircont,vbarmax)
      ELSE
         CALL SIMPSTDIR(Nnuc,Ee,JCC,Ipa,tdircont,vbarmax)
      ENDIF
      tdircont(1) = tdircont(1) * TUNEfi(Nnuc)

      IF(FISopt(Nnuc).EQ.0)THEN
         tdir = (tdir + tdircont(1))* (1.d0 - tweightw(1))
      ELSE
         tdir = tdir + (1.d0 - tweightw(1)) * tdircont(1)
      ENDIF
      IF(NRBar.EQ.5)THEN
         tdircont(2) = tdircont(2) * TUNEfi(Nnuc)
         tdircont(3) = tdircont(3) * TUNEfi(Nnuc)
         IF(FISopt(Nnuc).EQ.0)THEN
            tdir23 = (tdir23 + tdircont(2)) * (1.d0 - tweightw(2))
            tdir21 = (tdir21 + tdircont(3)) * (1.d0 - tweightw(1)) *
     &                                        (1.d0 - tweightw(2))
         ELSE
            tdir23 = tdir23 + (1.d0 - tweightw(2)) * tdircont(2)
            tdir21 = tdir21 + (1.d0 - tweightw(1)) *
     &                        (1.d0 - tweightw(2)) * tdircont(3) !!!
         ENDIF
      ENDIF

C-----Continuum contribution to each hump transmission coeffiecient

 800  IF(FISbar(Nnuc).EQ.3.)THEN
         CALL SIMPSNumBar(Nnuc,Ee,JCC,Ipa,Nrbarc,tfcon) !numerical barriers
      ELSE
         DO ibar = 1, nrbarc
            CALL SIMPSFIS(Nnuc,Ee,JCC,Ipa,Ibar,TFCc,vbarmax)!parabolic barriers
            tfcon(ibar) = TFCc
         ENDDO
      ENDIF

      DO ibar = 1, nrhump
        TF(ibar) = tfdis(ibar) + tfcon(ibar)* TUNEfi(Nnuc)
      ENDDO
c  
C-----Continuum contribution to absorption
      IF(FISopt(Nnuc).EQ.0)THEN
         Tabs1 = (Tabs1 +tfcon(1)) * tweightw(1)
      ELSE
         Tabs1 = Tabs1 + tweightw(1) * tfcon(1)
      ENDIF
      TABs = Tabs1

      IF(NRHump.EQ.3) THEN
         IF(Ee.GT.efb(5))THEN
            IF(FISopt(Nnuc).GT.0)THEN
               TABs12 = TABs12 +  tfcon(2) * tweightw(2)
               TABs21 = TABs21 +  tfcon(2) * tweightw(1)
               TABs2 =  TABs2 +  tdircont(3) * (1.d0-tweightw(1)) *
     &                  tweightw(2)
            ENDIF
         ELSE
            tabs12 = 0.d0
            tabs21 = 0.d0
            tabs2 = 0.d0
            tdir21 = 0.d0
         ENDIF
         sumtj = tf(1) + tdir23 + tabs12
         sumtk = tf(3) + tdir21 + tabs21
         IF(sumtj.EQ.0..OR.sumtk.EQ.0.)THEN
            Tind1 = 0.d0
            Tind2 = 0.d0
         ELSE
            IF(FISopt(Nnuc).GT.0)THEN
               rap = 1.d0/(1.d0 - tabs12 * tabs21/(sumtj * sumtk))
               Tind1 = tabs1 * (tdir23 + tabs12 * tf(3)/sumtk)/sumtj
               Tind2 = tabs2 * (tf(3) + tabs21 * tdir23/sumtj)/sumtk
               Tind1 = Tind1 * rap
               Tind2 = Tind2 * rap
            ELSE
               Tind1 =  tf(1) * tf(2) * tf(3)/
     &                 (tf(1) * tf(2) + tf(1) * tf(3) + tf(2) * tf(3))
               Tind1 = tweightw(1) * tind1
               Tind2 = 0.d0
            ENDIF
         ENDIF
      ENDIF


      IF (FISopt(Nnuc).EQ.2.) THEN
C        gamma transition in isomeric well, not calculated yet
         TG2 = .00002
      ELSE
         TG2 = 0.d0
      ENDIF
C--------------------------------------------------------------------
C     CALCULATING FISSION CONTRIBUTION TO THE HAUSER-FESHBACH denominator

C-----single-humped
      IF(NRHump.EQ.1) Sumfis = Tf(1)

C-----double-humped
      IF(NRhump.EQ.2)THEN
         IF(TF(1) + TF(2).EQ.0.0) THEN
            Sumfis = 0.d0
         ENDIF
         IF(FISOPT(Nnuc).GT.0)THEN
            Sumfis = Tdir + tabs1 * Tf(2)/(Tf(1) + Tf(2) + Tg2)
         ELSE
            Sumfis = Tdir +  tweightw(1) * Tf(1) * Tf(2)/(Tf(1) + Tf(2))
         ENDIF
         Sumfis =  Tf(1) * Tf(2)/(Tf(1) + Tf(2)) !!!!!!!!!!!!!!!!!!!!!!
      ENDIF

C----triple-humped
      IF(NRHump.EQ.3)THEN
         Sumfis = Tdir + tind1 + tind2
c         sumfis=tf(1)*tf(2)*tf(3)/(tf(1)*tf(2)+tf(1)*tf(3)+tf(2)*tf(3))
c          sumfis=tf(1)*tf(2)/(tf(1)+tf(2))
c     &    *tweightw(1)!+tdir
c         sumfis=tweightw(1)*tind1+tdir
      ENDIF

c---------------------------------------------------------------------
c     WRITING FISSION OUTPUT

      IF (Jc.EQ.1 .AND. Ip.EQ.1 .AND. Mmod.LT.2) THEN
         WRITE (80,*) '  '
         WRITE (80,'(1x,a19,f9.5,a4)') 'Excitation energy =', ee, ' MeV'
         WRITE (80,*) ' '
C---------single-humped
         IF (NRBar.EQ.1) WRITE (80,'(17x,3(a2,9x))') 'Td', 'Tc', 'Tf'
C---------double-humped
         IF (NRBar.EQ.2 .OR.
     &       (NRBar.EQ.3 .AND. NRWel.EQ.1 .AND. FISopt(Nnuc).EQ.0.))
     &       WRITE (80,'(22x,5(a3,9x))') 'TAd', 'TBd', 'TAc', 'TBc',
     &              'Tf'
         IF (NRBar.EQ.3 .AND. NRWel.EQ.1 .AND. FISopt(Nnuc).GE.1.)
     &       WRITE (80,'(22x,7(a4,7x))') 'TAd', 'TBd', 'TAc', 'TBc',
     &              'Tf', 'Tdir', 'Tabs'
C---------triple-humped
         IF (NRBar.EQ.3 .AND. NRWel.EQ.0) WRITE (80,'(22x,6(a4,7x))')
     &        'TAd', 'TBd', 'TCd', 'TAc', 'Teqc', 'Tf'

         IF (NRBar.EQ.5) WRITE (80,'(16x,10(a4,7x),a6)') 'TAd', 'TBd',
     &                  'TCd', 'TAc', 'TBc', 'TCc', 'Tf', 'Tdir',
     &                          'Tabs', 'Tdir23'
      ENDIF


C-----single-humped
      IF (Nrhump.EQ.1)
     &    WRITE (80,'(1x,a2,f4.1,1x,a3,I2,3g11.4)') 'J=', aj, 'Pi=', Ip,
     &          tfdis(1), tfcon(1), Sumfis

C-----double-humped
      IF (Nrhump.EQ.2.) THEN
c         IF (FISopt(Nnuc).EQ.0.)
c     &       WRITE (80,'(1x,a5,i1,1x,a2,f4.1,1x,a3,I2,7g11.4)')
c     &                 'Mode=', Mmod, 'J=', aj, 'Pi=', Ip, tfdis(1),
c     &                tfdis(2), tfcon(1), tfcon(2), Sumfis

c         IF (FISopt(Nnuc).GT.0.)
            WRITE (80,'(1x,a5,i1,1x,a2,f4.1,1x,a3,I2,7g11.4)')
     &                 'Mode=', Mmod, 'J=', aj, 'Pi=', Ip, tfdis(1),
     &                tfdis(2), tfcon(1), tfcon(2), Sumfis, TDIr, TABs
      ENDIF
C-----triple-humped
      IF (Nrhump.EQ.3)
     &  WRITE (80,'(1x,a2,f4.1,1x,a3,I2,10g11.4)') 'J=', aj, 'Pi=', Ip,
     &        tfdis(1), tfdis(2), tfdis(3), tfcon(1), tfcon(2),tfcon(3),
     &        Sumfis, TDIr, TABs, TDIr23

      IF (Sumfis.LT.0.0000000000000001) Sumfis = 0.d0
      DENhf = DENhf + Sumfis
      END
c======================================================================

      SUBROUTINE SIMPSFIS(Nnuc,Ee,JCC,Ipa,Ibar,TFCc,vbarmax)
C-----Simpson integration
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C COMMON variables
C
      INTEGER JCC
      DOUBLE PRECISION  TFCc
C
C Dummy arguments
C
      INTEGER Ibar, Nnuc,ipa
      DOUBLE PRECISION Ee
C
C Local variables
C
      DOUBLE PRECISION arg1, dens, enh1, ux1, vbarmax(NFPARAB)
      DOUBLE PRECISION FISINT
      INTEGER i,ii, nn,INT
C
      TFCc = 0.d0
      ii = int(vbarmax(ibar)/destepp(ibar))
      DO i = 1, NRBinfis(Ibar)
         ux1 = XMInn(Ibar) + (i - 1)*DEStepp(Ibar)
         enh1 = ENH_ld(1,Ibar) + (ENH_ld(2,Ibar) + ENH_ld(3,Ibar)*ux1)
     &          *ux1
         arg1 = 2*PI*(ux1 + EFB(Ibar) - Ee)/Hcont(Ibar)
         IF (arg1.GE.EXPmax) arg1 = EXPmax

         IF (FISden(Nnuc).EQ.1 .AND. enh1.GT.0)
     &       dens = enh1 * ROFisp(i,JCC,ipa,Ibar)/(1.d0 + EXP(arg1))
         IF (FISden(Nnuc).EQ.0 .AND. enh1.GT.0)
     &       dens = enh1 * FISINT(Ibar,ux1,JCC,Nnuc,0)/(1.d0+ EXP(arg1))
         IF (FISden(Nnuc).EQ.2.)
     &       dens = enh1 * FISINT(Ibar,ux1,JCC,Nnuc,ipa)/
     &              (1.d0 + EXP(arg1))
         nn = 2
         IF ((i * 0.5).EQ.INT(i/2)) nn = 4
         IF (i.EQ.1 .OR. i.EQ.(NRBinfis(Ibar))) nn = 1
         dens = nn * dens
         TFCc = TFCc + dens
      ENDDO
      TFCc = TFCc * DEStepp(Ibar)
      RETURN
      END


      SUBROUTINE SIMPSNumBar(Nnuc,Ee,JCC,Ipa,Nrbarc1,tfcon)
C-----Simpson integration
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C COMMON variables
C
      INTEGER JCC
      DOUBLE PRECISION  TFCc
C
C Dummy arguments
C
      DOUBLE PRECISION tfcon(NRBarc),Ee
      INTEGER Nnuc, Nrbarc1, Ipa
C
C Local variables
C
      DOUBLE PRECISION arg1, dens, enh1, ux1, uexcit1
      DOUBLE PRECISION phase(2*NFPARAB)
      DOUBLE PRECISION FISINT
      INTEGER i, nn,INT, ibar
C
      DO Ibar = 1, nrbarc1
         TFCc = 0.d0
         tfcon(Ibar) = 0.d0
         DO i = 1, NRBinfis(ibar)
            ux1 = XMInn(Ibar) + (i - 1)*DEStepp(Ibar)
            enh1 = ENH_ld(1,Ibar) + (ENH_ld(2,Ibar) + ENH_ld(3,Ibar)
     &             * ux1) * ux1
C---------- Momentum integrals are calculated
            uexcit1 = Ee - ux1

            CALL PHASES(uexcit1, phase, nnuc)

            if(Ibar.eq.1) arg1 = 2.d0*phase(1)
            if(Ibar.eq.2) arg1 = 2.d0*phase(3)

            IF (FISden(Nnuc).EQ.1 .AND. enh1.GT.0)
     &           dens = enh1*ROFisp(i,JCC,ipa,Ibar)/(1. + EXP(arg1))

            IF (FISden(Nnuc).EQ.0 .AND. enh1.GT.0)
     &           dens = enh1*FISINT(Ibar,ux1,JCC,Nnuc,0)/(1.+ EXP(arg1))

            IF (FISden(Nnuc).EQ.2.)
     &          dens = enh1 * FISINT(Ibar,ux1,JCC,Nnuc,ipa)/
     &                 (1. + EXP(arg1))
           nn = 2
           IF ((i*0.5).EQ.INT(i/2)) nn = 4
           IF (i.EQ.1 .OR. i.EQ.(NRBinfis(Ibar))) nn = 1

           dens = nn * dens
           TFCc = TFCc + dens
c           if( dens.le.1.d-6*TFCc ) EXIT
        ENDDO

        TFCc = TFCc*DEStepp(Ibar)
        tfcon(ibar) = TFCc * TUNEfi(Nnuc)
      ENDDO
      RETURN
      END
C=============================================================

      SUBROUTINE SIMPSTDIR(Nnuc,Ee,JCC,Ipa,tdircont,vbarmax)
C-----Simpson integration for direct transmission
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C COMMON variables
C
      INTEGER JCC
      DOUBLE PRECISION  vbarmax(nfparab)
C
C Dummy arguments
C
      DOUBLE PRECISION Ee
      INTEGER Ibar, Nnuc,ipa,iw
C
C Local variables
C
      DOUBLE PRECISION FISINT,arg1,ux1
      DOUBLE PRECISION dmom,ttf(nrbarc),ttdir(NRBarc),
     &                 tdircont(nrbarc),romin
      INTEGER i,ii, nn, INT
C

      arg1 = 0.d0
      dmom = 0.d0
      DO ib = 1, NRBarc
         ttdir(ib) = 0.d0
         tdircont(ib) =0.d0
         ttf(ib) = 0.d0
      ENDDO

      ibar = 1
      ii = int(vbarmax(1)/destepp(1))
      romin= ROFisp(ii,JCC,ipa,1)

      DO ib = 2, NRBarc
         IF(romin.gt.ROFisp(ii,JCC,ipa,ib))THEN
            romin = ROFisp(ii,JCC,ipa,ib)
            ibar = ib
            ii = int(vbarmax(ib)/destepp(ib))
         ENDIF
      ENDDO

      DO i = 1, NRBinfis(Ibar)
         ux1 = xminn(ibar) + (i - 1)*DEStepp(Ibar)
         DO ib = 1, NRBarc
            arg1 = 2.d0 * PI * (ux1 + EFB(ib) - Ee)/HCOnt(ib)
            IF (arg1.GE.EXPmax) arg1 = EXPmax
            ttf(ib) = 1.d0/(1.d0 + EXP(arg1))
         ENDDO
         ttdir(nrbarc) = ttf(nrbarc)
         DO iw =  NRWel, 1, -1
            IF(Ee.LT.(EFB(nrbarc + iw) + ux1))THEN
               ttdir(iw) = ttf(iw) *  ttdir(iw + 1)
               GOTO 100
            ENDIF
            arg1 =  - 2.d0 * PI * (ux1 + EFB(NRBarc + iw) - Ee)
     &               /HCOnt(Nrbarc + iw)
            arg1=0.d0
            dmom=(1.d0 - ttf(iw)) * (1.d0 - ttdir(iw + 1))
            ttdir(iw) =  ttf(iw) *  ttdir(iw + 1)/
     &                  (1.d0 + 2.d0 * dSQRT(dmom) *
     &                  COS(arg1) + dmom)
c        ttdir(iw) =  ttdir(iw + 1)*(1.d0/(dSQRT(dmom) *cos(arg1)+dmom))

 100     ENDDO
         IF(NRBar.EQ.5.AND.Ee.GT.EFB(NRBarc + 1))THEN
            dmom=(1.d0 - ttf(1)) * (1.d0 - ttf(2))
            arg1 = - 2.d0 * PI * (ux1 + EFB(NRBarc + 1) - Ee)
     &             /HCOnt(Nrbarc + 1)
            ttdir(3) =  ttf(1) *  ttf(2)/
     &                  (1.d0 + 2.d0 * dSQRT(dmom) *
     &                  dCOS(arg1) + dmom)
         ENDIF

         nn = 2

         IF ((i*0.5).EQ.INT(i/2)) nn = 4
         IF (i.EQ.1 .OR. i.EQ.(NRBinfis(Ibar))) nn = 1

         DO iw = 1, NRBar - 2
            IF (FISden(Nnuc).EQ.1 )
     &           ttdir(iw) = ttdir(iw) *  ROFisp(i,JCC,ipa,ibar)
            IF (FISden(Nnuc).EQ.0 )
     &           ttdir(iw) = ttdir(iw) * FISINT(Ibar,ux1,JCC,Nnuc,0)
            IF (FISden(Nnuc).EQ.2.)
     &           ttdir(iw) = ttdir(iw) * FISINT(Ibar,ux1,JCC,Nnuc,1)
            ttdir(iw) =  ttdir(iw) * nn
            tdircont(iw) = tdircont(iw) + ttdir(iw)
         ENDDO
 300  ENDDO

      DO iw = 1,NRBar -NRWel
         tdircont(iw) = tdircont(iw) * DEStepp(Ibar)
      ENDDO
      RETURN
      END

C=============================================================

      SUBROUTINE SIMPSNumTDIR(Nnuc,Ee,JCC,Ipa,tdircont,vbarmax)
C-----Simpson integration for direct transmission
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C COMMON variables
C
      INTEGER JCC
      DOUBLE PRECISION  vbarmax(nfparab),phase(2 * NFPARAB), uexcit1
C
C Dummy arguments
C
      DOUBLE PRECISION Ee
      INTEGER Ibar, Nnuc,ipa,iw
C
C Local variables
C
      DOUBLE PRECISION FISINT, arg1, ux1
      DOUBLE PRECISION dmom,ttf(nrbarc),ttdir(NRBarc),
     &                 tdircont(nrbarc),romin
      INTEGER i,ii, nn, INT
C
      arg1 = 0.d0
      dmom = 0.d0
      DO ib = 1, NRBarc
         ttdir(ib) = 0.d0
         tdircont(ib) =0.d0
         ttf(ib) = 0.d0
      ENDDO

      ibar = 1
      ii = int(vbarmax(1)/destepp(1))
      romin= ROFisp(ii,JCC,ipa,1)

      DO ib = 2, NRBarc
         IF(romin.gt.ROFisp(ii,JCC,ipa,ib))THEN
            romin = ROFisp(ii,JCC,ipa,ib)
            ibar = ib
            ii = int(vbarmax(ib)/destepp(ib))
         ENDIF
      ENDDO

      DO i = ii, NRBinfis(Ibar)
         ux1 = xminn(ibar) + (i - 1)*DEStepp(Ibar)
         uexcit1 = Ee - ux1

         DO ib = 1, NRBar
            CALL PHASES(uexcit1, phase, nnuc)
         ENDDO

         DO ib = 1, NRBarc
            if(Ib.eq.1) arg1 = 2.d0*phase(1)
            if(Ib.eq.2) arg1 = 2.d0*phase(3)
            if(Ib.eq.3) arg1 = 2.d0*phase(5)

            IF (arg1.GE.EXPmax) arg1 = EXPmax
            ttf(ib) = 1.d0/(1.d0 + EXP(arg1))
         ENDDO

         ttdir(nrbarc) = ttf(nrbarc)

         DO iw =  NRWel, 1, -1
            IF(Ee.LT.(EFB(nrbarc + iw) + ux1))THEN
               ttdir(iw) = ttf(iw) *  ttdir(iw + 1)
               GOTO 100
            ENDIF
            if(Iw.eq.1) arg1 =  2.d0*phase(2)
            if(Iw.eq.2) arg1 =  2.d0*phase(4)

            arg1=0.d0
            dmom=(1.d0 - ttf(iw)) * (1.d0 - ttdir(iw + 1))
            ttdir(iw) =  ttf(iw) *  ttdir(iw + 1)/
     &                  (1.d0 + 2.d0 * dSQRT(dmom) *
     &                  dCOS(arg1) + dmom)
 100     ENDDO
         IF(NRBar.EQ.5.AND.Ee.GT.EFB(NRBarc + 1))THEN
            dmom=(1.d0 - ttf(1)) * (1.d0 - ttf(2))
            arg1 =  2.d0*phase(2)
c            arg1 = - 2.d0 * PI * (ux1 + EFB(NRBarc + 1) - Ee)
c     &             /HCOnt(Nrbarc + 1)
            ttdir(3) =  ttf(1) *  ttf(2)/
     &                  (1.d0 + 2.d0 * dSQRT(dmom) *
     &                  dCOS(arg1) + dmom)
         ENDIF

         nn = 2

         IF ((i*0.5).EQ.INT(i/2)) nn = 4
         IF (i.EQ.1 .OR. i.EQ.(NRBinfis(Ibar))) nn = 1

         DO iw = 1, NRBar - 2
            IF (FISden(Nnuc).EQ.1 )
     &           ttdir(iw) = ttdir(iw) *  ROFisp(i,JCC,ipa,ibar)
            IF (FISden(Nnuc).EQ.0 )
     &           ttdir(iw) = ttdir(iw) * FISINT(Ibar,ux1,JCC,Nnuc,0)
            IF (FISden(Nnuc).EQ.2.)
     &           ttdir(iw) = ttdir(iw) * FISINT(Ibar,ux1,JCC,Nnuc,1)
            ttdir(iw) =  ttdir(iw) * nn
            tdircont(iw) = tdircont(iw) + ttdir(iw)
         ENDDO
 300  ENDDO

      DO iw = 1, NRBar -2 !NRWel
         tdircont(iw) = tdircont(iw) * DEStepp(Ibar)
      ENDDO
      RETURN
      END
c-----------------------------------------------------


      DOUBLE PRECISION FUNCTION FISINT(Ib,Ux,Jcc,Nnuc,ipa)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C
C Dummy arguments
C
      INTEGER Ib, Jcc, Nnuc,ipa
      DOUBLE PRECISION Ux
C
C Local variables
C
      DOUBLE PRECISION c1, c2, hhh, r1, r2
      DOUBLE PRECISION DLOG10
      INTEGER iugrid, k, khi, klo
C
C
      IF (FISden(Nnuc).EQ.1.) iugrid = NRBinfis(Ib)
      IF (FISden(Nnuc).EQ.0.) iugrid = NFISEN !!!!!!!!!!!!!!!!!!!
      IF (FISden(Nnuc).EQ.2.) iugrid = NFISEN1 !!!!!!!!!!!!!!!!!!!!!!

      ROFisp(0,Jcc,ipa,Ib)=0.d0
      klo = 1
      khi = iugrid
      IF (Ux.LE.UGRid(klo,Ib)) THEN
         klo = 0
         khi = 1
         GOTO 200
      ENDIF
      IF (Ux.LE.UGRid(klo,Ib)) THEN
         klo = 0
         khi = 1
         GOTO 200
      ENDIF
      IF (Ux.GE.UGRid(khi,Ib)) THEN
         klo = iugrid - 1
         GOTO 200
      ENDIF
  100 IF (khi - klo.GT.1) THEN
         k = (khi + klo)/2.
         IF (UGRid(k,Ib).GT.Ux) THEN
            khi = k
         ELSE
            klo = k
         ENDIF
         GOTO 100
      ENDIF

C-----LEVEL DENSITY INTERPOLATION
  200 hhh = UGRid(khi,Ib) - UGRid(klo,Ib)
      c1 = (UGRid(khi,Ib) - Ux)/hhh
      c2 = (Ux - UGRid(klo,Ib))/hhh
      IF(ipa.EQ.0)THEN
         r1 = ROFis(klo,Jcc,Ib)
         r2 = ROFis(khi,Jcc,Ib)
      ENDIF
      r1 = ROFisp(klo,Jcc,ipa,Ib)
      r2 = ROFisp(khi,Jcc,ipa,Ib)

      IF (r1.GT.0 .AND. r2.GT.0) THEN
         FISINT = MAX(10.**(c1*DLOG10(r1) + c2*DLOG10(r2)),0.)
      ELSE
         FISINT = MAX(c1*r1 + c2*r2,0.)
      ENDIF
      END

c-----------------------------------------------------------
      SUBROUTINE WRITE_OUTFIS(Nnuc)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C
C COMMON variables
C
      DOUBLE PRECISION  TFIso, TGIso
      DOUBLE PRECISION ACRtf(NFHUMP),  DETcrtf(NFHUMP), ECOndf(NFHUMP),
     &                 SCRtf(NFHUMP),TCRtf(NFHUMP), UCRtf(NFHUMP)

      DOUBLE PRECISION AFIsm(NFMOD), DEFbm(NFMOD), DELtafism(NFMOD),
     &                 DEStepm(NFMOD), EFBm(NFMOD),
     &                 EFDism(NFTRANS,NFMOD), GAMmafism(NFMOD),
     &                 HM(NFTRANS,NFMOD),
     &                 MORtcrt(NFPARAB), MPArcrt(NFPARAB), RFIso,
     &                 ROFism(160,30,NFMOD), SHCfism(NFMOD),
     &                 TDIrect, TDIrm(NFMOD), TFB, TFBm(NFMOD),
     &                 TISo, UGRidf(0:NFISENMAX,3), WFIsm(3),
     &                 XMInnm(NFMOD),ECFis(NFHUMP),ECFism(NFMOD)
      INTEGER BFFm(3), NRBinfism(NFMOD)
      COMMON /CRITFIS/ ACRtf, UCRtf, TCRtf, DETcrtf, SCRtf, MORtcrt,
     &                 MPArcrt, ECOndf
      COMMON /FISSMOD/ ROFism, HM, EFDism, UGRidf, EFBm, XMInnm, AFIsm,
     &                 DEFbm, SHCfism, DELtafism, GAMmafism, WFIsm,
     &                 BFFm, NRBinfism, DEStepm, TFBm, TDIrm, TFB,
     &                 TDIrect,ECFis,ECFism
      COMMON /FIS_ISO/ TFIso, TGIso, TISo, RFIso, PFIso
C
C Dummy arguments
C
      INTEGER Nnuc
C
C Local variables
C
      CHARACTER*36 cara1
      INTEGER i, ib, ibar, m, nr, nrbarc1
      INTEGER INT
C
C
      WRITE (80,'(a40)') '----------------------------------------'
      WRITE (80,'(4x,a2,i3,2x,a2,i3)') 'Z=', INT(Z(Nnuc)), 'A=',
     &                                 INT(A(Nnuc))
      WRITE (80,'(a40)') '----------------------------------------'
      WRITE (80,'(a8,f2.0,a28,a20)') 'FISBAR =', FISbar(Nnuc)
      WRITE (80,*) 'No.of parabolas=', NRBar, '   No.of wells=', NRWel
      WRITE (80,*) '  '
      IF (NRBar.EQ.1) THEN
         WRITE (80,'(a)') '    Va      ha    (in Mev) '
         WRITE (80,'(2f8.3)') EFB(1), H(1,1)
         WRITE (80,*) ' '
         WRITE (80,'(2a10)') 'h2/2J(A)', '(in MeV)'
         WRITE (80,'(f9.4)') HJ(Nnuc,1)
         WRITE (80,*) ' '
         WRITE (80,'(a10)') 'Beta2(A)'
         WRITE (80,'(f9.4)') DEFfis(1)
         WRITE (80,*) ' '
      ENDIF
      IF (NRBar.EQ.2) THEN
         IF (FISmod(Nnuc).EQ.0.) THEN
            WRITE (80,'(a)')
     &                    '    Va      ha      Vb      hb     (in Mev) '
            WRITE (80,'(4f8.3)') (EFB(i),HCOnt(i),i = 1,NRBar)
         ENDIF
         IF (FISmod(Nnuc).EQ.1.) THEN
            WRITE (80,'(a,1x,a)')
     &         '       Va      ha     Vb(SL)   hb(SL)   Vb(ST)   hb(ST)'
     &         , '  (in Mev) '
            WRITE (80,'(6f9.3,15x)') EFB(1), H(1,1), EFBm(1), HM(1,1),
     &                               EFBm(2), HM(1,2)
         ENDIF
         IF (FISmod(Nnuc).EQ.2.) THEN
            WRITE (80,'(a,1x,a)')
     &         '      Va      ha     Vb(SL)    hb(SL)  Vb(ST1)  hb(ST1)'
     &         , '  Vb(ST2)  hb(ST2)  (in Mev) '
            WRITE (80,'(8f9.3,15x)') EFB(1), H(1,1), EFBm(1), HM(1,1),
     &                               EFBm(2), HM(1,2), EFBm(3), HM(1,3)
         ENDIF
         WRITE (80,*) ' '
         WRITE (80,'(3a10)') 'h2/2J(A)', 'h2/2J(B)', '(in MeV)'
         WRITE (80,'(2f9.4)') (HJ(Nnuc,i),i = 1,NRBar)
         WRITE (80,*) ' '
         WRITE (80,'(2a10)') 'Beta2(A)', 'Beta2(B)'
         WRITE (80,'(2f9.4)') (DEFfis(i),i = 1,NRBar)
         WRITE (80,*) ' '
      ENDIF
      IF (NRBar.EQ.3) THEN
         IF (FISmod(Nnuc).EQ.0.) THEN
            WRITE (80,'(a,1x,a)')
     &       '    Va      ha      Vb      hb      Vi      hi  (in Mev) '
            WRITE (80,'(6f8.3,15x)') (EFB(i),H(1,i),i = 1,NRBar)
         ENDIF
         IF (FISmod(Nnuc).EQ.1.) THEN
            WRITE (80,'(a,1x,a)')
     &          '       Va      ha     Vb(SL)   hb(SL)   Vb(ST)  hb(ST)'
     &          , '    Vi      hi  (in Mev) '
            WRITE (80,'(8f9.3,15x)') EFB(1), H(1,1), EFBm(1), HM(1,1),
     &                               EFBm(2), HM(1,2), EFB(3), H(1,3)
         ENDIF
         IF (FISmod(Nnuc).EQ.2.) THEN
            WRITE (80,'(a,1x,a)')
     &         '      Va      ha     Vb(SL)    hb(SL)  Vb(ST1)  hb(ST1)'
     &         , '  Vb(ST2)  hb(ST2)   Vi      hi  (in Mev) '
            WRITE (80,'(10f9.3,15x)') EFB(1), H(1,1), EFBm(1), HM(1,1),
     &                                EFBm(2), HM(1,2), EFBm(3), HM(1,3)
     &                                , EFB(3), H(1,3)
         ENDIF

         WRITE (80,*) ' '
         WRITE (80,'(4a10)') 'h2/2J(A)', 'h2/2J(B)', 'h2/2J(I)',
     &                       '(in MeV)'
         WRITE (80,'(3f9.4)') (HJ(Nnuc,i),i = 1,NRBar)
         WRITE (80,*) ' '
         WRITE (80,'(3a10)') 'Beta2(A)', 'Beta2(B)', 'Beta2(I)'
         WRITE (80,'(3f9.4)') (DEFfis(i),i = 1,NRBar)
         WRITE (80,*) ' '
      ENDIF
      IF (NRBar.EQ.5) THEN
         WRITE (80,'(a,1x,a)')
     &'    Va      ha      Vb      hb      Vc       hc      Vi      hi
     &    Vo      ho  (in Mev) '
         WRITE (80,'(10f8.3,15x)') (EFB(i),H(1,i),i = 1,NRBar)
         WRITE (80,*) ' '
         WRITE (80,'(6a10)') 'h2/2J(A)', 'h2/2J(B)', 'h2/2J(C)',
     &                       'h2/2J(I)', 'h2/2J(O)', '(in MeV)'
         WRITE (80,'(5f9.4)') (HJ(Nnuc,i),i = 1,NRBar)
         WRITE (80,*) ' '
         WRITE (80,'(6a10)') 'Beta2(A)', 'Beta2(B)', 'Beta2(C)',
     &                       'Beta2(I)', 'Beta2(O)', '        '
         WRITE (80,'(5f9.4)') (DEFfis(i),i = 1,NRBar)
         WRITE (80,*) ' '
      ENDIF
      IF (FISmod(Nnuc).EQ.0. .AND. NRBar.GE.3) THEN
         WRITE (80,*) ' '
         WRITE (80,*) '  Tiso1/2 fission = ', TFIso, ' (s)'
         WRITE (80,*) '  Tiso1/2 gamma   = ', TGIso, ' (s)'
         WRITE (80,*) '  Tiso1/2 total   = ', TISo, ' (s)'
         WRITE (80,*) '  Rfiso   = ', RFIso
      ENDIF
      WRITE (80,*) ' '
      IF (FISopt(Nnuc).EQ.0.) cara1 = ' Subbarrier effects neglected '
      IF (FISopt(Nnuc).GT.0.) cara1 = ' Subbarrier effects considered'
      WRITE (80,'(a8,f2.0,a36)') 'FISOPT=', FISopt(Nnuc), cara1
      WRITE (80,'(A8,F2.0)') 'FISMOD =', FISmod(Nnuc)
      WRITE (80,*) ' '
      IF (FISopt(Nnuc).GT.0.) THEN
         WRITE (80,*) '  '
         WRITE (80,*) '      W0         W1         W2'
         WRITE (80,'(3f11.4)') (WIMag(i),i = 1,3)
         WRITE (80,*)
      ENDIF
      DO ibar = 1, NRBar
         IF (ibar.LT.3) WRITE (80,'(a39,I2,a2,I2)')
     &                          'Number of discrete states at barrier',
     &                         ibar, '=', NRFdis(ibar)
         IF (NRBar.EQ.3 .AND. ibar.EQ.3) WRITE (80,'(a46,I2,a2,I2)')
     &        'Number of discrete states in isomeric well', ibar, '=',
     &       NRFdis(ibar)
         IF (NRBar.EQ.5 .AND. (ibar.EQ.3 .OR. ibar.EQ.5))
     &        WRITE (80,'(a46,I2,a2,I2)')
     &        'Number of discrete states in isomeric well', ibar, '=',
     &       NRFdis(ibar)
         WRITE (80,*) 'Jdis Pidis    Edis    homega'

         DO nr = 1, NRFdis(ibar)
            IF (FISmod(Nnuc).EQ.0. .OR.
     &          (FISmod(Nnuc).GT.0. .AND. ibar.NE.2))
     &           WRITE (80,'(1x, 1f3.1, 1x, 1i4, 2x, 1f8.3, 1x, 1f8.3)')
     &          SFDis(nr,ibar), IPFdis(nr,ibar), EFDis(nr,ibar),
     &          H(nr,ibar)
            IF (FISmod(Nnuc).EQ.1. .AND. ibar.EQ.2)
     &           WRITE (80,'(1x, 1f3.1, 1x, 1i4,1x, 4f9.3)')
     &          SFDis(nr,ibar), IPFdis(nr,ibar), EFDism(nr,1), HM(nr,1),
     &          EFDism(nr,2), HM(nr,2)
            IF (FISmod(Nnuc).EQ.2. .AND. ibar.EQ.2)
     &           WRITE (80,'(1x, 1f3.1, 1x, 1i4,1x, 6f9.3)')
     &          SFDis(nr,ibar), IPFdis(nr,ibar), EFDism(nr,1), HM(nr,1),
     &          EFDism(nr,2), HM(nr,2), EFDism(nr,3), HM(nr,3)
         ENDDO
      ENDDO
      WRITE (80,*) '  '
      IF (NRBarc.EQ.3) THEN
         WRITE (80,*) 'Parameters of the outer equivalent barrier'
         WRITE (80,*) '     Veq=', VEQ, ' MeV     heq=', HOEq, ' MeV'
         WRITE (80,*) ' '
      ENDIF
      WRITE (80,'(A8,F2.0)') 'FISDEN =', FISden(Nnuc)
      nrbarc1 = NRBarc
      IF (NRBarc.EQ.3) nrbarc1 = 2
      IF (FISden(Nnuc).EQ.1.) THEN
         DO ib = 1, nrbarc1
            WRITE (80,*) 'Barrier  ', ib
            WRITE (80,'(3(A9,f9.5),a9,f11.5)') 'Acrt=', ACRtf(ib),
     &             'Ucrt=', UCRtf(ib), 'Econd=', ECOndf(ib), 'DETcrt=',
     &             DETcrtf(ib)
            WRITE (80,'(A9,f9.5,A9,f9.5)') 'Tcrt=', TCRtf(ib), 'Scrt=',
     &             SCRtf(ib)
         ENDDO
      ENDIF
      WRITE (80,*) '  '
      IF (FISden(Nnuc).EQ.1.) THEN
         WRITE (80,*)
     &          '  Asymmetry  shell-corr  delta    gamma    atilf/atil '
         DO nr = 1, nrbarc1
            IF (FISmod(Nnuc).EQ.0. .OR.
     &          (FISmod(Nnuc).GT.0. .AND. nr.NE.2))
     &           WRITE (80,'(1x, A8, 1x, I1,4x,I1, 4f9.3)') 'Barrier',
     &          nr, BFF(nr), SHCfis(nr), DELtafis(nr), GAMmafis(nr),
     &          AFIs(nr)
            IF (FISmod(Nnuc).GT.0. .AND. nr.EQ.2) THEN
               DO m = 1, INT(FISmod(Nnuc)) + 1
                  WRITE (80,'(1x, A8, 1x, I1, 2x, I1, 1x, I1, 4f9.3)')
     &                    'Barrier', nr, m, BFFm(m), SHCfism(m),
     &                   DELtafism(m), GAMmafism(m), AFIsm(2)
               ENDDO
            ENDIF
         ENDDO
      ENDIF
      WRITE (80,*) '   '
      WRITE (80,*) '                a0       a1      a2'
      DO nr = 1, nrbarc1
         WRITE (80,'(1x, A8, 1x, I1, 3f9.3)') 'Barrier', nr,
     &          ENH_ld(1,nr), ENH_ld(2,nr), ENH_ld(3,nr)
      ENDDO
      END
