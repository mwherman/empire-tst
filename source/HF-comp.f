Ccc   * $Author: Capote $
Ccc   * $Date: 2008-10-14 21:32:21 $
Ccc   * $Id: HF-comp.f,v 1.98 2008-10-14 21:32:21 Capote Exp $
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
                  CALL EXCLUSIVEL(Iec,icsl,Nejc,Nnuc,Nnur,il,popll)
               ELSEIF (ENDf(Nnuc).EQ.2) THEN
                  CSE(icsl,Nejc,0) = CSE(icsl,Nejc,0) + popll
               ENDIF
            ENDIF
            IF (poph.NE.0.0D+0) THEN
               IF (ENDf(Nnuc).EQ.1) THEN
                  CALL EXCLUSIVEL(Iec,icsh,Nejc,Nnuc,Nnur,il,poph)
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


      SUBROUTINE EXCLUSIVEL(Iec,Ie,Nejc,Nnuc,Nnur,Il,Popt)
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
C---------------------Store also population spectra for discrete levels 
                      POPcselv(Il,iejc,iesp,INExc(Nnur)) 
     &                = POPcselv(Il,iejc,iesp,INExc(Nnur))
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
C
C           TUNe commented as it is dangerous to scale Ts for discrete levels 
C           Dec. 2007, MH, RCN, MS 
C
C           sumdl = sumdl*RORed*cor*TUNe(Nejc,Nnuc)
            sumdl = sumdl*RORed*cor
            SCRtl(i,Nejc) = sumdl
            Sum = Sum + sumdl
         ENDDO
C--------do loop over discrete levels --------- done --------------------
      ENDIF
  100 DENhf = DENhf + Sum
      SCRtem(Nejc) = Sum
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
      IF (IOUt.GT.2) WRITE (8,99005)
99005 FORMAT (1X,////,1X,27('*'),/,1X,'Discrete gamma transitions ',/,
     &        1X,27('*'),//)
      DO i = 1, NLV(Nnuc) - 1
         l = NLV(Nnuc) - i + 1
         IF (BR(l,1,2,Nnuc).EQ.0. .and. POPlv(l,Nnuc).GT.0. AND.
     &      ISIsom(l,Nnuc).EQ.0) THEN
C-----------Normal level without branching ratios
            IF (IOUt.GT.2) WRITE (8,99010) ELV(l,Nnuc), LVP(l,Nnuc)
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
            IF (IOUt.GT.2) WRITE (8,99012) ELV(l,Nnuc), LVP(l,Nnuc)
     &                            *XJLv(l,Nnuc), POPlv(l,Nnuc)
99012       FORMAT (1X,//,5X,'Level of energy  ',F8.4,' MeV',
     &              ' and spin ',F6.1,' with final population ',G13.5,
     &              ' mb is an isomer')
C-----------We add it to the ground state to have correct total cross section
            POPlv(1,Nnuc) = POPlv(1,Nnuc) + POPlv(l,Nnuc)
         ELSEIF (POPlv(l,Nnuc).GT.0. AND. ISIsom(l,Nnuc).EQ.1) THEN
C-----------Isomer state in any other nucleus
C-----------No gamma-decay of the isomeric state imposed
            IF (IOUt.GT.2) WRITE (8,99012) ELV(l,Nnuc), LVP(l,Nnuc)
     &                            *XJLv(l,Nnuc), POPlv(l,Nnuc)
C-----------We add it to the ground state to have correct total cross section
            POPlv(1,Nnuc) = POPlv(1,Nnuc) + POPlv(l,Nnuc)
         ELSE
C-----------Normal level with branching ratios
            popl = POPlv(l,Nnuc)
            IF (popl.NE.0.0D0) THEN
               IF (IOUt.GT.2) WRITE (8,99015) ELV(l,Nnuc), LVP(l,Nnuc)
     &                               *XJLv(l,Nnuc), popl
99015          FORMAT (1X//,5X,'Decay of  ',F7.4,' MeV  ',F5.1,
     &                 ' level with final population ',G13.5,' mb',/,5X,
     &                 'Level populated ',4X,'E.gamma ',4X,
     &                 'Intensity  ',/)
               DO j = 1, NDBR
                  j1 = NINT(BR(l,j,1,Nnuc))
                  IF (j1.EQ.0) GOTO 100
                  IF (j1.GE.l) THEN
                     WRITE (8,99020)
99020                FORMAT (10X,
     &                       'WARNING: error in discrete level deca',
     &                       'y data',/,10X,
     &                       'Final level above the initial one',/,10X,
     &                       'Further decay not considered ')
                     WRITE (8,
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
                  IF (IOUt.GT.2) WRITE (8,99025) ELV(j1,Nnuc),
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
      INTEGER Jr,lamb, lambmin, lambmax
      DOUBLE PRECISION ha, cee, cme, xle, xlm, xjr,
     &                 scrtpos, scrtneg, hscrtl
      DIMENSION xle(10),xlm(10)
C
C----MAXmult - maximal gamma-ray multipolarity
C    maximal value (.LT.10) of gamma-ray multipolarity (L) in        
C    calculations of gamma-transitions both between states in        
C    continuum and from continuum states to discrete levels.         
C    A default value of 'MAXmult' is set to 2 in 'input.f'                  
C    but can be adjusted in the input.         
C
C   The radiative strength functions of higher multipole orders       
C   (f_EL, f_ML) are calculated using the relationships between                 
C   single-particle radiative strength functions in the Weisskopf form.      
C                                                                            
C     Electric transitions:                                                  
C     f_E(L+1)/f_EL = eg^2*cee*[(3+L)/(5+L)]^2,                              
C     cee=[R/(\hbar*c)]^2, R=r_0*A^(2/3), r_0=1.2 fm => cee=3.7D-5*A^(2/3)   
C     xle(i) = f_Ei                                                          
C                                                                            
C     Magnetic transitions:                                                  
C     f_M(L+1)/f_E(L+1) = cme,                                               
C     cme= 10[\hbar/(m*c*R]^2 => cme = 0.307/A^(2/3)                         
C     xlm(i) = f_Mi                                                          
C                                                                            
C 
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
          lambmin = MAX0(1,lmin)
          lambmax = MIN0(lmax,MAXmult)
          IF(lambmin.LE.lambmax)THEN
             eg = EX(Iec, Nnuc) - ELV(i, Nnuc)
             ipar = (1 + LVP(i, Nnuc)*Ipc)/2
             iodd = 1 - ipar
             xle(1) = E1(Nnuc,Z,A,eg, TNUc(1, Nnuc),Uexcit(1,Nnuc))*
     &             TUNe(0, Nnuc)
             xlm(1) = XM1(eg)*TUNe(0, Nnuc)
             IF(lambmax.GE.2) xle(2) = E2(eg)*TUNe(0, Nnuc)
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
          SCRtl(i, 0) = hscrtl*RORed
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
      INTEGER Jr, lamb, lambmin, lambmax
      DOUBLE PRECISION ha, cee, cme, xle, xlm, xjr, hscrtl
      DIMENSION xle(10),xlm(10)
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
          lambmin = MAX0(1,lmin)
          lambmax = MIN0(lmax,MAXmult)
          IF(lambmin.LE.lambmax)THEN
            eg = EX(Iec, Nnuc) - ELV(i, Nnuc)
            ipar = (1 + LVP(i, Nnuc)*Ipc)/2
            iodd = 1 - ipar
            xle(1) = E1(Nnuc,Z,A,eg, TNUc(1, Nnuc),Uexcit(1,Nnuc))*
     &         *TUNe(0, Nnuc)
             IF(lambmax.GE.2) xle(2) = E2(eg)*TUNe(0, Nnuc)
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
          SCRtl(i, 0) = hscrtl*RORed
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
     &                 TF(NFPARAB),
     &                 TFD(NFPARAB),TG2, VBArex(NFPARAB)
      DOUBLE PRECISION tabs1
      DOUBLE PRECISION rap,rap0,barmin

      DOUBLE PRECISION tdirp(NFPARAB,NFPARAB), tabsp(NFPARAB,NFPARAB)
      DOUBLE PRECISION tdirpp(NFPARAB,NFPARAB), tabspp(NFPARAB,NFPARAB)
      DOUBLE PRECISION sumtp(NFPARAB), wdir(NFPARAB), tindp,vsh,vbar

      DOUBLE PRECISION TFIso, TGIso, TISo, RFIso, PFIso
      COMMON /FIS_ISO/ TFIso, TGIso, TISo, RFIso, PFIso

      INTEGER JCC
      COMMON /IMAG  / TF, TDIr, TABs, TG2
C
C Dummy arguments
C
      INTEGER Iec, Ip, Jc, Mmod, Nnuc,ih
      DOUBLE PRECISION Sumfis
C
C Local variables
C
      DOUBLE PRECISION aj, ee, tdircont(NfHump),
     &                 exfis, sfmin, snc,
     &                 tfcon(NfHump), tfdis(NfPARAB),
     &                 vbarmax(NFPARAB)
      REAL FLOAT
      INTEGER ibar, ist, jnc, nr, ipa
C
      ee = EX(Iec,Nnuc)
      IF (ee.EQ.0.0D0) RETURN
      JCC = Jc

c-----initialization
      Sumfis = 0.D0
      DO ih = 1, nrhump
         DO ih1 = 1, nrhump
            tdirp(ih, ih1) = 0.d0
            tdirpp(ih, ih1) = 0.d0
         ENDDO
      ENDDO

      DO iw = 1, nrwel + 1
         sumtp(iw) = 0.d0
         wdir(iw) = 0.d0
         DO iw1 = 2, nrwel + 1
            tabsp(iw, iw1) = 0.d0
            tabspp(iw, iw1) = 0.d0
         ENDDO
      ENDDO

      DO ibar = 1, nrhump
         tfdis(ibar) = 0.d0
         tfcon(ibar) = 0.d0
         tdircont(ibar) = 0.d0
         tf(ibar)=0.d0
      ENDDO

      DO ib = 1, NRBar
         vbarmax(ib) = 0.d0
         vbarex(ib)=efb(ib)
      ENDDO

      TABs = 0.D0
      tabs1 = 0.d0
      tdir = 0.d0


      DO k=1, NRBar,2 
         HO(k) = H(1, int(k/2)+1)
      ENDDO
      DO k=2, NRBar,2
         HO(k) = H(1, NRhump+int(k/2))
      ENDDO


c-----discrete contribution

c-----numerical barrier from RIPL-3; gs is the only discrete transition state
      IF(FISbar(Nnuc).EQ.3.)THEN
         IF (Jc.EQ.1 .AND. Ip.EQ.1) THEN
             DO ibar = 1, NRBar
                VBArex(ibar) = EFB(ibar)
c               ENDDO
c              CALL WKBFIS(Ee, nnuc, tfdis, tdirp,tabsp)
                arg1 = 2*PI*(VBArex(Ibar) - Ee)/H(1,Ibar)
                IF (arg1.GE.EXPmax) arg1 = EXPmax
                TFDis(ibar)= 1.d0/(1.d0 + EXP(arg1))
             ENDDO
          ENDIF
          GOTO 700
      ENDIF

c-----parabolic barriers
      DO nr = 1, NRFdis(1)
c      DO nr = 1,4
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
                  HO(k) = H(nr, NRhump+int(k/2))
               ENDDO
               snc = FLOAT(jnc) + HIS(Nnuc)
               DO ibar = 1, NRBar
                  exfis = EFDis(nr,ibar) + HJ(Nnuc,ibar)
     &                    *(snc*(snc + 1) - SFDis(nr,ibar)
     &                    *(SFDis(nr,ibar) + 1))
                  IF(exfis.lt.0)exfis=0.d0
                  VBArex(ibar) = EFB(ibar) + exfis
                  IF(nr.eq.1) vbarmax(ibar) = exfis
                  IF(exfis.gt.vbarmax(ibar)) vbarmax(ibar) = exfis
               ENDDO

               IF(FISopt(Nnuc).EQ.0.)THEN
                  DO ibar = 1, Nrhump
                     arg1 = 2*PI*(VBArex(Ibar) - Ee)/H(nr,Ibar)
                     IF (arg1.GE.EXPmax) arg1 = EXPmax
                     TFD(ibar)= 1.d0/(1.d0 + EXP(arg1))
                  ENDDO
                  goto 600
               ENDIF

               CALL NUMBARR(Nnuc,Vbarex,HO)
               CALL WKBFIS(Ee, nnuc, tfd, tdirp,tabsp)

               IF(FISopt(Nnuc).GT.0.)THEN
                  DO iw = 1, nrwel
                     DO iw1 = iw + 1, nrwel + 1
                        tabspp(iw, iw1) = tabspp(iw, iw1) +
     &                                    tabsp(iw, iw1)
                     ENDDO
                  ENDDO

                  DO iw = nrwel + 1, 3, -1
                     DO iw1 = iw -1, 2, -1
                        tabspp(iw, iw1) = tabspp(iw, iw1) +
     &                                    tabsp(iw, iw1)
                     ENDDO
                  ENDDO

                  DO ih = 1, nrhump
                     DO ih1 = 1, nrhump
                        IF(tdirp(ih, ih1).LT.1E-10)tdirp(ih, ih1)=0.d0
                        tdirpp(ih, ih1) = tdirpp(ih, ih1) +
     &                                    tdirp(ih, ih1)
                     ENDDO
                  ENDDO
               ENDIF
 600           DO ibar = 1, NRHump
                  tfdis(ibar) = tfdis(ibar) + TFD(ibar)
               ENDDO
            ENDIF
         ENDDO
      ENDDO
C----continuum contribution
 700  aj = FLOAT(Jc) + HIS(Nnuc)
      IF(Ip.EQ.1)  ipa = 1
      IF(Ip.EQ.-1) ipa = 2
c-----continuum direct and indirect weights
      DO iw = 1, NRWel
         IF(awf(iw).EQ.0.d0)THEN
            wdir(iw + 1) = 1.d0
         ELSE
            vsh=-.03
            barmin = min((vbarmax(iw) + efb(iw)-vsh),
     &                   (vbarmax(iw + 1) + efb(iw + 1))-vsh)
            vbar= VBArex(NRhump + iw)+efb(NRhump + iw)

            wdir(iw + 1) = 2.d0 * (Ee - VBArex(NRhump + iw)) /
     &                     ((barmin - VBArex(NRhump + iw)) *
     &                     (1.d0 + dexp( - (Ee - barmin) / awf(iw))))

            wdir(iw + 1) = 2.d0 * (Ee - vbar) /
     &                     ((barmin - vbar) *
     &                     (1.d0 + dexp( - (Ee - barmin) / awf(iw))))
            IF(Ee.GE.barmin) wdir(iw + 1) = 1.d0
            IF(Ee.LE.VBArex(NRhump + iw))wdir(iw + 1) = 0.d0
         endif
      ENDDO

C-----Continuum contribution to each hump transmission coeffiecient
 800  CALL SIMPSFIS(Nnuc,Ee,JCC,Ipa,tfcon)
      DO ih = 1, nrhump
c         tfcon(ih)=0.d0
c         tfdis(ih)=0.d0
        TF(ih) = tfdis(ih) + tfcon(ih) * TUNEfi(Nnuc)
        IF(TF(ih).LE.1.d-19) THEN
          TF(ih) = 0.d0
          tfdis(ih) = 0.d0
          tfcon(ih) = 0.d0
          sumfis=0.d0
          goto 890
        ENDIF
      ENDDO
c
      IF(NRHump.EQ.1) THEN
         SUMfis = TF(1)
         GOTO 890
      ENDIF
c-----continuum direct
      IF(wdir(1).eq.0..and.wdir(2).eq.0.) GOTO 809
      CALL SIMPSTDIR(Nnuc,Ee,JCC,Ipa,tdircont,vbarmax)
      DO ih = 1, nrhump
         tdircont(ih) = tdircont(ih) * TUNEfi(Nnuc)
      ENDDO
c-----adding wheighted continuum direct
 809  DO ih = 1, nrhump
         tdirpp(ih, ih) = tf(ih)
      ENDDO
 810  DO ih = 1, nrhump - 1
         tdirpp(ih, nrhump) = tdirpp(ih, nrhump) + tdircont(ih) 
     &                       * (1.d0 - wdir(ih + 1))
      ENDDO
c      if(nrbar.eq.5) tdirpp(2,3) = tdirpp(2,3) + tdircont(2) 
c 
c-----COMPLETE DAMPING + surrogate OPT.MOD
      IF (FISopt(Nnuc).EQ.0.) THEN
         tfd(nrhump) = tf(nrhump)
         DO ih = Nrhump - 1, 1, -1
            IF(tf(ih) + tfd(ih + 1).gt.0) THEN
               tfd(ih) = tdirpp(ih, nrhump) * (1.d0 - wdir(ih + 1)) + 
     &         wdir(ih+1) * tf(ih) * tfd(ih + 1)/(tf(ih) + tfd(ih + 1))
            ELSE
               tfd(ih) = 0.d0
            ENDIF
         ENDDO
         sumfis=tfd(1)
         GOTO 890
      ENDIF
c-----PARTIAL DAMPING
c-----adding wheighted continuum absorption
      DO iw = 1, nrwel 
         tabspp(iw, iw + 1) = tabspp(iw, iw + 1) + tfcon(iw) *
     &                        wdir(iw + 1)
         tabspp(iw + 1, iw) = tabspp(iw + 1, iw) + tfcon(iw) *
     &                        wdir(iw)
      ENDDO
c-----sum of competting channels in wells
      sumtp(1) = 1.d0
      DO iw = 2, nrwel + 1
         sumtp(iw)= tdirpp(iw-1, 1) + tdirpp(iw, nrhump)
         DO iw1 = 2, nrwel + 1
            IF(iw1.NE.iw) sumtp(iw) = sumtp(iw) + tabspp(iw, iw1)
         ENDDO
      ENDDO

c-----normalization factor for the indirect terms
      rap0=0.d0
      rap = 1.d0
      DO iw = 2, nrwel + 1
         DO iw1 = iw + 1, nrwel + 1
            rap0 = rap0 + tabspp(iw, iw1) * tabspp(iw1, iw)/
     &           (sumtp(iw) * sumtp(iw1))
             rap = 1.d0/(1.d0-rap0)
         ENDDO
      ENDDO
c----sumfis
      DO iw = 2, nrwel + 1
         tindp = 0.d0
         DO iw1 = 2, nrwel + 1
            IF(iw1.NE.iw) tindp = tindp + tabspp(iw, iw1) *
     &                    tdirpp(iw1, nrhump) /
     &                   (sumtp(iw) * sumtp(iw1))
         ENDDO
         tindp = tindp + tdirpp(iw, nrhump)/sumtp(iw)
         sumfis = sumfis + tindp * tabspp(1, iw)  * rap
      ENDDO
      sumfis =  sumfis + tdirpp(1, nrhump)
      tabs = tabspp(1,2)
      tdir = tdirpp(1,nrhump)

      IF (FISopt(Nnuc).EQ.2.) THEN
C--------gamma transition in isomeric well, not calculated yet
         TG2 = .002
         sumfis= tdir+tabs*(tf(2)+rfiso*tg2)/(tf(1)+tf(2)+tg2)
      ELSE
         TG2 = 0.d0
      ENDIF
C-----FISSION CONTRIBUTION TO THE HAUSER-FESHBACH denominator
      IF (Sumfis.LT.1.E-15) Sumfis = 0.d0

 890  DENhf = DENhf + Sumfis


c-----WRITING FISSION OUTPUT

 900  IF (Jc.EQ.1 .AND. Ip.EQ.1 .AND. Mmod.LT.2) THEN
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
         IF (NRBar.EQ.3 .AND. NRWel.EQ.0) WRITE (80,'(22x,7(a4,7x))')
     &        'TAd', 'TBd', 'TCd', 'TAc', 'TBc', 'TCc','Tf'

         IF (NRBar.EQ.5) WRITE (80,'(16x,7(a4,7x),5(a6,5x))') 'TAd',
     &                  'TBd', 'TCd', 'TAc', 'TBc', 'TCc', 'Tf', 
     &                  'Tdir12','Tdir23','Tdir13','Tabs12', 'Tabs13'
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
     &  WRITE (80,'(1x,a2,f4.1,1x,a3,I2,13g11.4)') 'J=', aj, 'Pi=', Ip,
     &        tfdis(1), tfdis(2), tfdis(3), tfcon(1), tfcon(2),tfcon(3),
     &        Sumfis, TDIrpp(1,2),TDIrpp(2,3),TDIrpp(1,3), TABspp(1,2),
     &        TABspp(1,3)

      END
c=======================================================================
      SUBROUTINE SIMPSFIS(Nnuc,Ee,JCC,Ipa,tfcon)
C-----Simpson integration
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C COMMON variables
C
      INTEGER JCC, iphas_opt
      DOUBLE PRECISION  TFCc
C
C Dummy arguments
C
      DOUBLE PRECISION tfcon(Nfhump),Ee
      INTEGER Nnuc, Ipa
C
C Local variables
C
      DOUBLE PRECISION arg1, ux1, uexcit1
      DOUBLE PRECISION phase(2*NFPARAB),phase_h(NFPARAB)
      INTEGER i, nn,INT, ibar
      LOGICAL discrete
C
      discrete = .FALSE.
c-----iphas_opt=0 parabolic shape, iphas_opt=1 non-parabolic numerical shape
      iphas_opt = 1
      DO Ibar = 1, nrhump
         TFCc = 0.d0
         tfcon(Ibar) = 0.d0
         DO i = 1, NRBinfis(ibar)
            ux1 = XMInn(Ibar) + (i - 1)*DEStepp(Ibar)
            IF(FISBAR(Nnuc).EQ.3) THEN
               uexcit1 = Ee - ux1
               CALL PHASES(uexcit1, phase,phase_h, nnuc,iphas_opt,
     &                     discrete)
               arg1 = 2.d0 * phase_h(ibar)    !(2 * ibar -1)
            ELSE
               arg1 = 2*PI*(ux1 + EFB(Ibar) - Ee)/Hcont(Ibar)
            ENDIF
            IF (arg1.GE.EXPmax) arg1 = EXPmax
            nn = 2
            IF ((i*0.5).EQ.INT(i/2)) nn = 4
            IF (i.EQ.1 .OR. i.EQ.(NRBinfis(Ibar))) nn = 1
            TFCc = TFCc + nn *
     &             ROFisp(i, JCC, ipa, Ibar)/(1.d0 + EXP(arg1))
        ENDDO
        TFCc = TFCc * DEStepp(Ibar)
        tfcon(ibar) = TFCc * TUNEfi(Nnuc)
      ENDDO
      RETURN
      END
c=======================================================================
      SUBROUTINE SIMPSTDIR(Nnuc,Ee,JCC,Ipa,tdircont,vbarmax)
C-----Simpson integration for direct transmission
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C COMMON variables
C
      INTEGER JCC
      DOUBLE PRECISION  vbarmax(nfhump)
C
C Dummy arguments
C
      DOUBLE PRECISION Ee
      INTEGER Ibar, Nnuc,ipa
      LOGICAL discrete
C
C Local variables
C
      DOUBLE PRECISION arg1,ux1,uexcit1
      DOUBLE PRECISION dmom,tdirc(NfHump, Nfhump),
     &                 tdircont(NfHump),romin, phase(2*NFPARAB),
     &                 phase_h(NFPARAB)
      INTEGER i,ii, iphas_opt, nn, INT
     
C
      discrete = .FALSE.
      iphas_opt = 1
      arg1 = 0.d0
      dmom = 0.d0
      DO ih = 1, NRHump
         tdirc(ih, nrhump) = 0.d0
         tdircont(ih) =0.d0
      ENDDO
      ibar = 1
      ii = int(vbarmax(1)/destepp(1))
      romin= ROFisp(ii,JCC,ipa,ih)
      DO ib = 1, NRHump
         IF(romin.gt.ROFisp(ii,JCC,ipa,ib))THEN
            romin = ROFisp(ii,JCC,ipa,ib)
            ibar = ib
            ii = int(vbarmax(ib)/destepp(ib))
         ENDIF
      ENDDO
      DO i = 1, NRBinfis(Ibar)                ! ii, NRBinfis(Ibar)
         ux1 = XMInn(ibar) + (i - 1)*DEStepp(Ibar)
         IF(FISBAR(Nnuc).EQ.3)THEN
            uexcit1 = Ee - ux1
            CALL PHASES(uexcit1, phase,phase_h, nnuc,iphas_opt,discrete)
         ENDIF

         DO ib = 1, NRHump
            IF(FISBAR(Nnuc).EQ.3)THEN
               arg1 = 2.d0 * phase_h(ib)
            ELSE
               arg1 = 2.d0 * PI * (ux1 + EFB(ib) - Ee)/HCOnt(ib)
            ENDIF
            IF (arg1.GE.EXPmax) arg1 = EXPmax
            tdirc(ib, ib) = 1.d0/(1.d0 + EXP(arg1))
         ENDDO

         DO ih1 = nrhump - 1, 1, -1
            IF(Ee.LT.(EFB(nrhump + ih1) + ux1))THEN
               tdirc(ih1, nrhump) =  tdirc(ih1, ih1) *
     &                               tdirc(ih1+1, nrhump)
               CYCLE
            ENDIF
c            arg1 =  - 2.d0 * PI * (ux1 + EFB(NRHump + ih1) - Ee)
c     &               /HCOnt(Nrhump + ih1)
            arg1=0.d0
            dmom = (1.d0 - tdirc(ih1, ih1)) *
     &             (1.d0 - tdirc(ih1 + 1, nrhump))
            tdirc(ih1, nrhump) = tdirc(ih1, ih1) *
     &             tdirc(ih1 + 1, nrhump)/
     &             (1.d0 + 2.d0 * dSQRT(dmom) * COS(arg1) + dmom)
         ENDDO
         nn = 2
         IF ((i*0.5).EQ.INT(i/2)) nn = 4
         IF (i.EQ.1 .OR. i.EQ.(NRBinfis(Ibar))) nn = 1

         DO ih = 1, NRHump
            tdirc(ih, nrhump) = nn * tdirc(ih, nrhump) *
     &                          ROFisp(i,JCC,Ipa,Ibar)
            tdircont(ih) = tdircont(ih) + tdirc(ih, nrhump)
         ENDDO
      ENDDO
      DO ih = 1,NRHump
         tdircont(ih) = tdircont(ih) * DEStepp(Ibar)
      ENDDO
      RETURN
      END

