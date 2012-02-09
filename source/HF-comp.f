Ccc   * $Rev: 2526 $
Ccc   * $Author: shoblit $
Ccc   * $Date: 2012-02-09 21:34:11 +0100 (Do, 09 Feb 2012) $
C
      SUBROUTINE ACCUM(Iec,Nnuc,Nnur,Nejc,Xnor)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      INTEGER :: Iec, Nejc, Nnuc, Nnur
      REAL*8 :: Xnor
C
C Local variables
C
      REAL*8 :: eemi, excnq, pop1, pop2, poph, popl, popll, pops, popt, 
     &          xcse
      REAL :: FLOAT
      INTEGER :: icse, icsh, icsl, ie, il, j, na, nexrt
      INTEGER :: INT, MAX0, MIN0
C
C*** End of declarations rewritten by SPAG
C
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
C-----
C-----Continuum
C-----
      IF(Nnuc.EQ.Nnur)THEN
        excnq = EX(Iec,Nnuc)
      ELSE
        excnq = EX(Iec,Nnuc) - Q(Nejc,Nnuc)
      ENDIF
      nexrt = (excnq - ECUt(Nnur))/DE + 1.0001
      DO ie = 1, nexrt          !loop over residual energies (continuum)
        icse = (excnq - EX(ie,Nnur))/DE + 1.0001
        icse = MAX0(2,icse)
        icse = MIN0(NDEcse,icse)
        popt = 0.0
        DO j = 1, NLW   !loop over residual spins
          pop1 = Xnor*SCRt(ie,j,1,Nejc)
          pop2 = Xnor*SCRt(ie,j,2,Nejc)
          pops = pop1 + pop2
          IF(ie.EQ.1)pops = pops*0.5
          popt = popt + pops   !sum over spin/pi at a given energy bin
          POP(ie,j,1,Nnur) = POP(ie,j,1,Nnur) + pop1
          POP(ie,j,2,Nnur) = POP(ie,j,2,Nnur) + pop2
          IF(Nejc.NE.0.AND.POPmax(Nnur).LT.POP(ie,j,1,Nnur))POPmax(Nnur)
     &       = POP(ie,j,1,Nnur)
        ENDDO  !over residual spins
        IF(popt.NE.0.0D+0)THEN
          AUSpec(icse,Nejc) = AUSpec(icse,Nejc) + popt
          CSE(icse,Nejc,Nnuc) = CSE(icse,Nejc,Nnuc) + popt
          CSEt(icse,Nejc) = CSEt(icse,Nejc) + popt
          IF(ENDf(Nnuc).EQ.1)THEN
            CALL EXCLUSIVEC(Iec,ie,Nejc,Nnuc,Nnur,popt)
          ELSE
            CSE(icse,Nejc,0) = CSE(icse,Nejc,0) + popt
          ENDIF
        ENDIF
      ENDDO !over residual energies in continuum
C-----
C-----Discrete levels
C-----
      DO il = 1, NLV(Nnur)
        eemi = excnq - ELV(il,Nnur)
        IF(eemi.LT.0.0D0)RETURN
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
 
        IF(Nnuc.NE.1.OR.ENDf(Nnuc).NE.1.OR.Iec.NE.NEX(1).OR.Nejc.EQ.0)
     &     THEN
 
          IF(NPRim_g.GT.0)THEN     ! Primary gammas stored
            IF(Nnuc.EQ.1.AND.Nejc.EQ.0.AND.Iec.EQ.NEX(1))THEN
C
C               Primary gammas from the CN: Nnuc=1, Nejc=0
C               Originate from the primary excitation energy bin: Iec = NEX(1)
C
              xcse = eemi/DE + 1.0001
              icsl = MIN(INT(xcse),NDEcse - 1)
              ENPg(il) = eemi
              CSEpg(il) = CSEpg(il) + pop1
C
C               CALL EXCLUSIVEL(Iec,icsl,Nejc,Nnuc,Nnur,il,pop1)
C               CALL EXCLUSIVEL(Iec,icsl,Nejc,Nnuc,Nnur,   pop1)
              CYCLE   ! for primary gammas no further processing is needed
 
            ENDIF
          ENDIF
          xcse = eemi/DE + 1.0001
          icsl = MIN(INT(xcse),NDEcse - 1)
          icsh = icsl + 1
          popl = pop1*(FLOAT(icsh) - xcse)/DE
          popll = popl              !we also need popl not multiplied by 2
          IF(icsl.EQ.1)popl = 2.0*popl
C
C           Addition of discrete gamma to spectra
C
          poph = pop1*(xcse - FLOAT(icsl))/DE
          CSE(icsl,Nejc,Nnuc) = CSE(icsl,Nejc,Nnuc) + popl
          CSE(icsh,Nejc,Nnuc) = CSE(icsh,Nejc,Nnuc) + poph
C
          CSEt(icsl,Nejc) = CSEt(icsl,Nejc) + popl
          CSEt(icsh,Nejc) = CSEt(icsh,Nejc) + poph
 
          IF(popll.NE.0.0D+0)THEN
            IF(ENDf(Nnuc).EQ.1)THEN
C                 CALL EXCLUSIVEL(Iec,icsl,Nejc,Nnuc,Nnur,il,popll)
              CALL EXCLUSIVEL(Iec,icsl,Nejc,Nnuc,Nnur,popll)
            ELSE
              CSE(icsl,Nejc,0) = CSE(icsl,Nejc,0) + popll
            ENDIF
          ENDIF
          IF(poph.NE.0.0D+0)THEN
            IF(ENDf(Nnuc).EQ.1)THEN
C                 CALL EXCLUSIVEL(Iec,icsh,Nejc,Nnuc,Nnur,il,poph)
              CALL EXCLUSIVEL(Iec,icsh,Nejc,Nnuc,Nnur,poph)
            ELSE
              CSE(icsh,Nejc,0) = CSE(icsh,Nejc,0) + poph
            ENDIF
          ENDIF
        ENDIF
 
C--------Add isotropic CN contribution to direct ang. distributions
        IF(Nnuc.EQ.1.AND.Iec.EQ.NEX(1).AND.Nejc.NE.0)THEN
          CSDirlev(il,Nejc) = CSDirlev(il,Nejc) + pop1
          pop1 = pop1/4.0/PI
          DO na = 1, NDAng
            CSAlev(na,il,Nejc) = CSAlev(na,il,Nejc) + pop1
          ENDDO
        ENDIF
      ENDDO
      END SUBROUTINE ACCUM
 
!---------------------------------------------------------------------------
 
      SUBROUTINE EXCLUSIVEC(Iec,Ief,Nejc,Nnuc,Nnur,Popt)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      INTEGER :: Iec, Ief, Nejc, Nnuc, Nnur
      REAL*8 :: Popt
C
C Local variables
C
      REAL*8 :: excnq, xnor
      INTEGER :: icsp, ie, iejc, nth
      INTEGER :: INT
C
C*** End of declarations rewritten by SPAG
C
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
C     data jsigma/0/,jsigma2/36/
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
      IF(Nejc.EQ. - 1)THEN
C        fission of n,nx and npx nuclei considered
        IF(POPbin(Iec,Nnuc).EQ.0)RETURN
        xnor = Popt*DE/POPbin(Iec,Nnuc)
        DO ie = 1, NDEcse
C           DO iejc = 0, NDEJC
C           DO iejc = 0, 1  ! only neutrons and photons for the time being
          DO iejc = 1, 1    ! only neutrons for the time being
            IF(POPcse(Iec,iejc,ie,INExc(Nnuc)).NE.0)CSEfis(ie,iejc,Nnuc)
     &         = CSEfis(ie,iejc,Nnuc) + POPcse(Iec,iejc,ie,INExc(Nnuc))
     &           *xnor
          ENDDO
        ENDDO
        RETURN    !if fission
      ENDIF
C-----
C-----Particle decay
C-----
      IF(Nnuc.EQ.Nnur)THEN
        excnq = EX(Iec,Nnuc)
      ELSE
        excnq = EX(Iec,Nnuc) - Q(Nejc,Nnuc)
      ENDIF
C-----Contribution coming straight from the current decay
C-----(ignore if residue is inclusive since summation already done in ACCUM)
      icsp = INT((excnq - EX(Ief,Nnur))/DE + 1.0001)
      IF(ENDf(Nnur).EQ.1)THEN
        POPcse(Ief,Nejc,icsp,INExc(Nnur))
     &    = POPcse(Ief,Nejc,icsp,INExc(Nnur)) + Popt
      ELSE
        CSE(icsp,Nejc,0) = CSE(icsp,Nejc,0) + Popt
      ENDIF
C-----Contribution due to feeding spectra from Nnuc
C-----DE spectra
      IF(Nnuc.NE.1.OR.Nejc.EQ.0)THEN     !skip the first CN except gammas
        IF(POPbin(Iec,Nnuc).EQ.0)RETURN
        xnor = Popt*DE/POPbin(Iec,Nnuc)
        DO ie = 1, NDEcse
          DO iejc = 0, NDEjc
            IF(POPcse(Iec,iejc,ie,INExc(Nnuc)).NE.0)THEN
              IF(ENDf(Nnur).EQ.2)THEN
                CSE(ie,iejc,0) = CSE(ie,iejc,0)
     &                           + POPcse(Iec,iejc,ie,INExc(Nnuc))*xnor
              ELSE
                POPcse(Ief,iejc,ie,INExc(Nnur))
     &            = POPcse(Ief,iejc,ie,INExc(Nnur))
     &            + POPcse(Iec,iejc,ie,INExc(Nnuc))*xnor
              ENDIF
            ENDIF
            IF(ENDfa(Nnur).EQ.1.AND.iejc.GT.0.AND.iejc.LT.3)THEN
              IF(POPcsed(Iec,iejc,ie,INExc(Nnuc)).NE.0)THEN
                POPcsed(Ief,iejc,ie,INExc(Nnur))
     &            = POPcsed(Ief,iejc,ie,INExc(Nnur))
     &            + POPcsed(Iec,iejc,ie,INExc(Nnuc))*xnor
              ENDIF
              IF(POPcsedlab(Iec,iejc,ie,INExc(Nnuc)).NE.0)THEN
                POPcsedlab(Ief,iejc,ie,INExc(Nnur))
     &            = POPcsedlab(Ief,iejc,ie,INExc(Nnur))
     &            + POPcsedlab(Iec,iejc,ie,INExc(Nnuc))*xnor
                DO nth = 1, NDAng
                  POPcsealab(nth,Ief,iejc,ie,INExc(Nnur))
     &              = POPcsealab(nth,Ief,iejc,ie,INExc(Nnur))
     &              + POPcsealab(nth,Iec,iejc,ie,INExc(Nnuc))*xnor
                ENDDO
              ENDIF
C-----------DDX spectra using portions
            ELSEIF(POPcseaf(Iec,iejc,ie,INExc(Nnuc)).NE.0)THEN
              IF(ENDf(Nnur).EQ.2)THEN
                POPcseaf(Ief,iejc,ie,0) = POPcseaf(Ief,iejc,ie,0)
     &            + POPcseaf(Iec,iejc,ie,INExc(Nnuc))*xnor
              ELSE
                POPcseaf(Ief,iejc,ie,INExc(Nnur))
     &            = POPcseaf(Ief,iejc,ie,INExc(Nnur))
     &            + POPcseaf(Iec,iejc,ie,INExc(Nnuc))*xnor
              ENDIF
            ENDIF
          ENDDO
        ENDDO
      ENDIF
      END SUBROUTINE EXCLUSIVEC
 
!---------------------------------------------------------------------------
 
C     SUBROUTINE EXCLUSIVEL(Iec,Ie,Nejc,Nnuc,Nnur,Il,Popt)
C
C     Index Il needed if discrete levels are going to be treated (see ### below)
C
      SUBROUTINE EXCLUSIVEL(Iec,Ie,Nejc,Nnuc,Nnur,Popt)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      INTEGER :: Ie, Iec, Nejc, Nnuc, Nnur
      REAL*8 :: Popt
C
C Local variables
C
      INTEGER :: iejc, iesp, nth
      REAL*8 :: xnor
C
C*** End of declarations rewritten by SPAG
C
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
C
C
C
C     POPcse(Ief,Nejc,icsp,INExc(Nnuc))  - spectrum for the population of the
C                                   energy bin with index Ief in Nnuc by
C                                   Nejc particles (cumulative over all
C                                   decays leading to this energy bin)
C
C-----Contribution comming straight from the current decay
      IF(ENDf(Nnur).EQ.1)THEN
        POPcse(0,Nejc,Ie,INExc(Nnur)) = POPcse(0,Nejc,Ie,INExc(Nnur))
     &                                  + Popt
      ELSE
        CSE(Ie,Nejc,0) = CSE(Ie,Nejc,0) + Popt
      ENDIF
C-----Contribution due to feeding spectra from Nnuc
C-----DE spectra
      IF(Nnur.NE.1.OR.Nejc.EQ.0)THEN     !skip the first CN except gammas
        IF(POPbin(Iec,Nnuc).GT.0)THEN
          xnor = Popt*DE/POPbin(Iec,Nnuc)
          DO iesp = 1, NDEcse
            DO iejc = 0, NDEjc
              IF(POPcse(Iec,iejc,iesp,INExc(Nnuc)).NE.0)THEN
                IF(ENDf(Nnur).EQ.2)THEN
                  CSE(iesp,iejc,0) = CSE(iesp,iejc,0)
     &                               + POPcse(Iec,iejc,iesp,INExc(Nnuc))
     &                               *xnor
                ELSE
                  POPcse(0,iejc,iesp,INExc(Nnur))
     &              = POPcse(0,iejc,iesp,INExc(Nnur))
     &              + POPcse(Iec,iejc,iesp,INExc(Nnuc))*xnor
C---------------------Store also population spectra for discrete levels
C
C     ### Index Il as a dummy parameter of the routine. It is needed to uncomment this part
C         SUBROUTINE EXCLUSIVEL(Iec,Ie,Nejc,Nnuc,Nnur,Il,Popt)
C
C                      POPcselv(Il,iejc,iesp,INExc(Nnur))
C     &                = POPcselv(Il,iejc,iesp,INExc(Nnur))
C     &                + POPcse(Iec,iejc,iesp,INExc(Nnuc))*xnor
                ENDIF
              ENDIF
              IF(ENDfa(Nnur).EQ.1.AND.iejc.GT.0.AND.iejc.LT.3)THEN
                IF(POPcsed(Iec,iejc,iesp,INExc(Nnuc)).NE.0)THEN
                  POPcsed(0,iejc,iesp,INExc(Nnur))
     &              = POPcsed(0,iejc,iesp,INExc(Nnur))
     &              + POPcsed(Iec,iejc,iesp,INExc(Nnuc))*xnor
                ENDIF
                IF(POPcsedlab(Iec,iejc,iesp,INExc(Nnuc)).NE.0)THEN
                  POPcsedlab(0,iejc,iesp,INExc(Nnur))
     &              = POPcsedlab(0,iejc,iesp,INExc(Nnur))
     &              + POPcsedlab(Iec,iejc,iesp,INExc(Nnuc))*xnor
                  DO nth = 1, NDAng
                    POPcsealab(nth,0,iejc,iesp,INExc(Nnur))
     &                = POPcsealab(nth,0,iejc,iesp,INExc(Nnur))
     &                + POPcsealab(nth,Iec,iejc,iesp,INExc(Nnuc))*xnor
                  ENDDO
                ENDIF
C-----------DDX spectra using portions
              ELSEIF(POPcseaf(Iec,iejc,iesp,INExc(Nnuc)).NE.0)THEN
                IF(ENDf(Nnur).EQ.2)THEN
                  POPcseaf(0,iejc,iesp,0) = POPcseaf(0,iejc,iesp,0)
     &              + POPcseaf(Iec,iejc,iesp,INExc(Nnuc))*xnor
                ELSE
                  POPcseaf(0,iejc,iesp,INExc(Nnur))
     &              = POPcseaf(0,iejc,iesp,INExc(Nnur))
     &              + POPcseaf(Iec,iejc,iesp,INExc(Nnuc))*xnor
                ENDIF
              ENDIF
            ENDDO
C--------------DDX spectra using portions
          ENDDO
        ENDIF
      ENDIF
      END SUBROUTINE EXCLUSIVEL
 
!---------------------------------------------------------------------------
 
      SUBROUTINE DECAY(Nnuc,Iec,Jc,Ipc,Nnur,Nejc,Sumx)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      INTEGER :: Iec, Ipc, Jc, Nejc, Nnuc, Nnur
      REAL*8 :: Sumx
C
C Local variables
C
      REAL*8 :: cor, corr, eout, eoutc, frde, hisr, s, smax, smin, 
     &          sumdl, sumtl1, sumtl2, xjc, xjr
      REAL :: FLOAT
      INTEGER :: i, ichsp, ier, iermax, ietl, iexc, il, ip1, ip2, ipar, 
     &           itlc, j, jr, l, lmax, lmaxf, lmin, mul
      INTEGER :: INT, MIN0
C
C*** End of declarations rewritten by SPAG
C
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
Ccc   * output:SUMX - SUM of transmission coefficients over all outgoing *
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
C
C
C
      Sumx = 0.0
      hisr = HIS(Nnur)
      xjc = FLOAT(Jc) + HIS(Nnuc)
C-----clear scratch matrices
      SCRtem(Nejc) = 0.D0
      DO j = 1, NLW
        DO i = 1, NEX(Nnur) + 1
          SCRt(i,j,1,Nejc) = 0.D0
          SCRt(i,j,2,Nejc) = 0.D0
        ENDDO
      ENDDO
      iexc = NEX(Nnuc) - NEXr(Nejc,Nnuc)
      itlc = iexc - 5
      iermax = Iec - iexc
      IF(iermax.GE.1)THEN
C-----
C-----decay to the continuum
C-----
C-----do loop over r.n. spins
        DO jr = 1, NLW
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
            IF(ip1.EQ.1)ip2 = 2
C--------------decay to the highest possible bin (non neutron only)
            IF(ZEJc(Nejc).NE.0.0D0)THEN
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
     &          + sumtl1*RO(iermax,jr,ip1,Nnur)*TUNe(Nejc,Nnuc)
              SCRt(iermax,jr,ip2,Nejc) = SCRt(iermax,jr,ip2,Nejc)
     &          + sumtl2*RO(iermax,jr,ip2,Nnur)*TUNe(Nejc,Nnuc)
            ENDIF
C--------------decay to the highest but one bin (conditional see the next IF)
            IF(ZEJc(Nejc).EQ.0.0D0.AND.Iec.EQ.NEX(Nnuc) - 1)THEN
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
              corr = 0.4444/(DE - XN(Nnur) + XN(1))*TUNe(Nejc,Nnuc)
              SCRt(iermax,jr,ip1,Nejc) = SCRt(iermax,jr,ip1,Nejc)
     &          + sumtl1*RO(iermax,jr,ip1,Nnur)*corr
              SCRt(iermax,jr,ip2,Nejc) = SCRt(iermax,jr,ip2,Nejc)
     &          + sumtl2*RO(iermax,jr,ip2,Nnur)*corr
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
     &                                + sumtl1*RO(ier,jr,ip1,Nnur)
     &                                *TUNe(Nejc,Nnuc)
              SCRt(ier,jr,ip2,Nejc) = SCRt(ier,jr,ip2,Nejc)
     &                                + sumtl2*RO(ier,jr,ip2,Nnur)
     &                                *TUNe(Nejc,Nnuc)
            ENDDO
C--------------do loop over r.n. energies ***done***
          ENDDO
C-----------do loop over channel spins ***done***
        ENDDO
C--------do loop over and r.n. spins ***done***
C--------decay to the continuum ------ done -----------------------------
C--------trapezoidal integration of ro*tl in continuum for ejectile nejc
        DO j = 1, NLW
          DO i = 1, iermax
            Sumx = Sumx + SCRt(i,j,1,Nejc) + SCRt(i,j,2,Nejc)
          ENDDO
          Sumx = Sumx - 0.5*(SCRt(1,j,1,Nejc) + SCRt(1,j,2,Nejc))
        ENDDO
        Sumx = Sumx*DE
C--------integration of ro*tl in continuum for ejectile nejc -- done ----
C--------
C--------decay to discrete levels
C-----
      ENDIF
      DO i = 1, NLV(Nejc)
        SCRtl(i,Nejc) = 0.0
      ENDDO
      eoutc = EX(Iec,Nnuc) - Q(Nejc,Nnuc)
C--------do loop over discrete levels -----------------------------------
      DO i = 1, NLV(Nnur)
        eout = eoutc - ELV(i,Nnur)
C-----------level above the bin
C           2.18+
        IF(eout.LT.0.0D0)EXIT
        cor = 1.0
        sumdl = 0.0
        CALL TLLOC(Nnur,Nejc,eout,il,frde)
        smin = ABS(XJLv(i,Nnur) - SEJc(Nejc))
        smax = XJLv(i,Nnur) + SEJc(Nejc) + 0.01
        s = smin
C-----------loop over channel spin ----------------------------------------
    5   lmin = INT(ABS(xjc - s) + 1.01)
        lmax = INT(xjc + s + 1.01)
        lmax = MIN0(NLW,lmax)
C-----------do loop over l ------------------------------------------------
        DO l = lmin, lmax
          ipar = 1 + LVP(i,Nnur)*Ipc*( - 1)**(l - 1)
          IF(ipar.NE.0)sumdl = sumdl + TL(il,l,Nejc,Nnur)
     &                         + frde*(TL(il + 1,l,Nejc,Nnur)
     &                         - TL(il,l,Nejc,Nnur))
        ENDDO
C-----------do loop over l --- done ----------------------------------------
        s = s + 1.
        IF(s.LE.smax)GOTO 5
C-----------loop over channel spin ------ done ----------------------------
C
C           TUNe commented as it is dangerous to scale Ts for discrete levels
C           Dec. 2007, MH, RCN, MS
C
C           sumdl = sumdl*cor*TUNe(Nejc,Nnuc)
        sumdl = sumdl*cor
        SCRtl(i,Nejc) = sumdl
        Sumx = Sumx + sumdl
      ENDDO
C--------do loop over discrete levels --------- done --------------------
      DENhf = DENhf + Sumx
      SCRtem(Nejc) = Sumx
      END SUBROUTINE DECAY
 
!---------------------------------------------------------------------------
 
      SUBROUTINE DECAYD(Nnuc)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      INTEGER :: Nnuc
C
C Local variables
C
      REAL*8 :: egd, gacs, popl
      INTEGER :: i, icse, j, j1, l, nejc
      INTEGER :: INT, NINT
C
C*** End of declarations rewritten by SPAG
C
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
C
C
C
      nejc = -1
      IF(Nnuc.EQ.MT91)THEN
        nejc = 1
      ELSEIF(Nnuc.EQ.MT649)THEN
        nejc = 2
      ELSEIF(Nnuc.EQ.MT849)THEN
        nejc = 3
      ENDIF
      IF(IOUt.GT.2)WRITE(8,1010)
 1010 FORMAT(1X,////,1X,27('*'),/,1X,'Discrete gamma transitions ',/,1X,
     &       27('*'),//)
      DO i = 1, NLV(Nnuc) - 1
        l = NLV(Nnuc) - i + 1
        IF(BR(l,1,2,Nnuc).EQ.0..AND.POPlv(l,Nnuc).GT.0.AND.
     &     ISIsom(l,Nnuc).EQ.0)THEN
C-----------Normal level without branching ratios
          IF(IOUt.GT.2)WRITE(8,1020)ELV(l,Nnuc), LVP(l,Nnuc)
     &                              *XJLv(l,Nnuc), POPlv(l,Nnuc)
 1020     FORMAT(1X,//,5X,'Level of energy  ',F8.4,' MeV',' and spin ',
     &           F6.1,' with population ',G13.5,
     &           ' mb is not depopulated (g.s. transition assumed)')
C-----------Well... let it go down to the ground state
          gacs = POPlv(l,Nnuc)
          POPlv(1,Nnuc) = POPlv(1,Nnuc) + gacs
          POPlv(l,Nnuc) = 0.0
          egd = ELV(l,Nnuc)
C
C           Xs should be stored in the second bin to avoid losing
C           discrete level XS which should be accounted for entirely
C
          icse = MIN(INT(2.0001 + egd/DE),NDEcse)
          CSE(icse,0,Nnuc) = CSE(icse,0,Nnuc) + gacs/DE
 
          CSEt(icse,0) = CSEt(icse,0) + gacs/DE    ! Jan 2011
 
          CSEmis(0,Nnuc) = CSEmis(0,Nnuc) + gacs
C-----------Add transition to the exclusive or inclusive gamma spectrum
 
          IF(ENDf(Nnuc).EQ.1)THEN
            POPcse(0,0,icse,INExc(Nnuc)) = POPcse(0,0,icse,INExc(Nnuc))
     &        + gacs/DE
          ELSE
            CSE(icse,0,0) = CSE(icse,0,0) + gacs/DE
          ENDIF
        ELSEIF(POPlv(l,Nnuc).GT.0.AND.ISIsom(l,Nnuc).EQ.1.AND.nejc.GT.0)
     &         THEN
C-----------Isomer state in the residue after n,p, or alpha emission
C-----------No gamma-decay of the isomeric state imposed
C-----------Add gamma cascade population to the direct population
          POPlv(l,Nnuc) = POPlv(l,Nnuc) + CSDirlev(l,nejc)
          IF(IOUt.GT.2)WRITE(8,1030)ELV(l,Nnuc), LVP(l,Nnuc)
     &                              *XJLv(l,Nnuc), POPlv(l,Nnuc)
 1030     FORMAT(1X,//,5X,'Level of energy  ',F8.4,' MeV',' and spin ',
     &           F6.1,' with final population ',G13.5,
     &           ' mb is an isomer')
C-----------We add it to the ground state to have correct total cross section
          POPlv(1,Nnuc) = POPlv(1,Nnuc) + POPlv(l,Nnuc)
        ELSEIF(POPlv(l,Nnuc).GT.0.AND.ISIsom(l,Nnuc).EQ.1)THEN
C-----------Isomer state in any other nucleus
C-----------No gamma-decay of the isomeric state imposed
          IF(IOUt.GT.2)WRITE(8,1030)ELV(l,Nnuc), LVP(l,Nnuc)
     &                              *XJLv(l,Nnuc), POPlv(l,Nnuc)
C-----------We add it to the ground state to have correct total cross section
          POPlv(1,Nnuc) = POPlv(1,Nnuc) + POPlv(l,Nnuc)
        ELSE
C-----------Normal level with branching ratios
          popl = POPlv(l,Nnuc)
          IF(popl.NE.0.0D0)THEN
            IF(IOUt.GT.2)WRITE(8,1040)ELV(l,Nnuc), LVP(l,Nnuc)
     &                                *XJLv(l,Nnuc), popl
 1040       FORMAT(1X//,5X,'Decay of  ',F7.4,' MeV  ',F5.1,
     &             ' level with final population ',G13.5,' mb',/,5X,
     &             'Level populated ',4X,'E.gamma ',4X,'Intensity  ',/)
            DO j = 1, NDBr
              j1 = NINT(BR(l,j,1,Nnuc))
              IF(j1.EQ.0)EXIT
              IF(j1.GE.l)THEN
                WRITE(8,1050)
 1050           FORMAT(10X,'WARNING: error in discrete level deca',
     &                 'y data',/,10X,
     &                 'Final level above the initial one',/,10X,
     &                 'Further decay not considered ')
                WRITE(8,
     &'(10X,''WARNING: Nucleus '',I3,''-'',A2,                        ''
     &level '',I3)')INT(A(Nnuc)), SYMb(Nnuc), l
                GOTO 99999
              ENDIF
              gacs = popl*BR(l,j,2,Nnuc)
              POPlv(j1,Nnuc) = POPlv(j1,Nnuc) + gacs
              gacs = gacs/(1 + BR(l,j,3,Nnuc))        ! int. conversion
              egd = ELV(l,Nnuc) - ELV(j1,Nnuc)
C
C                 Xs should be stored in the second bin to avoid losing
C                 discrete level XS which should be accounted for entirely
C
              icse = MIN(INT(2.0001 + egd/DE),NDEcse)
              CSE(icse,0,Nnuc) = CSE(icse,0,Nnuc) + gacs/DE
 
              CSEt(icse,0) = CSEt(icse,0) + gacs/DE      ! Jan 2011
 
              CSEmis(0,Nnuc) = CSEmis(0,Nnuc) + gacs
C-----------------Add transition to the exclusive gamma spectrum
C-----------------NOTE: internal conversion taken into account
              IF(ENDf(Nnuc).EQ.1)THEN
                POPcse(0,0,icse,INExc(Nnuc))
     &            = POPcse(0,0,icse,INExc(Nnuc)) + gacs/DE
              ELSE
                CSE(icse,0,0) = CSE(icse,0,0) + gacs/DE
              ENDIF
              IF(IOUt.GT.2)WRITE(8,1060)ELV(j1,Nnuc), LVP(j1,Nnuc)
     &                                  *XJLv(j1,Nnuc), egd, gacs
 1060         FORMAT(5X,F7.4,2X,F5.1,5X,F7.4,5X,G13.5,' mb')
            ENDDO
          ENDIF
        ENDIF
      ENDDO
99999 END SUBROUTINE DECAYD
 
!---------------------------------------------------------------------------
 
      SUBROUTINE DECAYD_DIR(Nnuc,Nejc)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      INTEGER :: Nejc, Nnuc
C
C Local variables
C
      REAL*8 :: gacs, popl
      INTEGER :: i, j, j1, l
      INTEGER :: NINT
C
C*** End of declarations rewritten by SPAG
C
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
C
C
C
      DO i = 1, NLV(Nnuc) - 1
        l = NLV(Nnuc) - i + 1
        IF(BR(l,1,2,Nnuc).EQ.0..AND.CSDirlev(l,Nejc).GT.0.)THEN
C-----------Well... let it go down to the ground state
          gacs = CSDirlev(l,Nejc)
          CSDirlev(1,Nejc) = CSDirlev(1,Nejc) + gacs
          CSEmis(0,Nnuc) = CSEmis(0,Nnuc) + gacs
        ELSE
          popl = CSDirlev(l,Nejc)
          IF(popl.NE.0.0D0)THEN
            DO j = 1, NDBr
              j1 = NINT(BR(l,j,1,Nnuc))
              IF(j1.EQ.0)EXIT
              IF(j1.GE.l)RETURN
              gacs = popl*BR(l,j,2,Nnuc)
              CSDirlev(j1,Nejc) = CSDirlev(j1,Nejc) + gacs
              gacs = gacs/(1 + BR(l,j,3,Nnuc))        ! int. conversion
              CSEmis(0,Nnuc) = CSEmis(0,Nnuc) + gacs
            ENDDO
          ENDIF
        ENDIF
      ENDDO
      END SUBROUTINE DECAYD_DIR
 
!---------------------------------------------------------------------------
 
      SUBROUTINE DECAYG(Nnuc,Iec,Jc,Ipc,Sumx)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      INTEGER :: Iec, Ipc, Jc, Nnuc
      REAL*8 :: Sumx
C
C Local variables
C
      REAL*8 :: cee, cme, eg, ha, hscrtl, scrtneg, scrtpos, xjc, xjr
      REAL*8 :: E1, E2, XM1
      REAL :: FLOAT
      INTEGER :: i, ier, ineg, iodd, ipar, ipos, j, jmax, jmin, jr, 
     &           lamb, lambmax, lambmin, lmax, lmin
      INTEGER :: IABS, MAX0, MIN0
      REAL*8, DIMENSION(10) :: xle, xlm
C
C*** End of declarations rewritten by SPAG
C
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
Ccc   * output:SUMX - SUM of transmission coefficients over all outgoing *
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
C
C
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
      IF(MAXmult.GT.2)THEN
        ha = A(Nnuc)**0.666666666666D0
        cee = 3.7D-5*ha
        cme = 0.307D0/ha
      ENDIF
C
Cp    jmin = MAX0(1, Jc - 2)
Cp    jmax = MIN0(NLW, Jc + 2)
      jmin = 1
Cp    jmin = MAX0(1, Jc - MAXmult)
      jmax = MIN0(NLW,Jc + MAXmult)
C
      Sumx = 0.D0
      SCRtem(0) = 0.D0
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
      IF(Iec.LT.1)RETURN
      IF(Ipc.GT.0)THEN
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
        xle(1) = E1(Nnuc,Z,A,eg,TNUc(ier,Nnuc),UEXcit(ier,Nnuc))
     &           *TUNe(0,Nnuc)
        xlm(1) = XM1(eg)*TUNe(0,Nnuc)
        xle(2) = E2(eg)*TUNe(0,Nnuc)
        IF(MAXmult.GT.2)THEN
          xlm(2) = xle(2)*cme
          DO i = 3, MAXmult
            xle(i) = xle(i - 1)
     &               *eg**2*cee*((3.0D0 + FLOAT(i))/(5.0D0 + FLOAT(i)))
     &               **2
            xlm(i) = xle(i)*cme
          ENDDO
        ENDIF
C
        DO jr = 1, jmax
          xjr = FLOAT(jr) + HIS(Nnuc)
          lambmin = MAX0(1,IABS(Jc - jr))
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
            SCRt(ier,jr,ipos,0) = scrtpos*RO(ier,jr,ipos,Nnuc)
            SCRt(ier,jr,ineg,0) = scrtneg*RO(ier,jr,ineg,Nnuc)
          ENDIF
        ENDDO
      ENDDO
C-----do loop over c.n. energies ***done***
C-----decay to the continuum ----** done***-----------------------
C
C-----integration of ro*gtl in continuum for ejectile 0 (TRAPEZOID
      DO j = jmin, jmax
        DO i = 1, Iec - 1
          Sumx = Sumx + SCRt(i,j,1,0) + SCRt(i,j,2,0)
        ENDDO
        Sumx = Sumx - 0.5*(SCRt(1,j,1,0) + SCRt(1,j,2,0))
      ENDDO
      Sumx = Sumx*DE
C-----integration of ro*gtl in continuum for ejectile 0 -- done ----
C-----
C-----DECAY TO DISCRETE LEVELS
C-----
C-----do loop over discrete levels -----------------------------------
      DO i = 1, NLV(Nnuc)
        lmin = ABS(xjc - XJLv(i,Nnuc)) + 0.001
        lmax = xjc + XJLv(i,Nnuc) + 0.001
        lambmin = MAX0(1,lmin)
        lambmax = MIN0(lmax,MAXmult)
        IF(lambmin.LE.lambmax)THEN
          eg = EX(Iec,Nnuc) - ELV(i,Nnuc)
          ipar = (1 + LVP(i,Nnuc)*Ipc)/2
          iodd = 1 - ipar
          xle(1) = E1(Nnuc,Z,A,eg,TNUc(1,Nnuc),UEXcit(1,Nnuc))
     &             *TUNe(0,Nnuc)
          xlm(1) = XM1(eg)*TUNe(0,Nnuc)
          IF(lambmax.GE.2)xle(2) = E2(eg)*TUNe(0,Nnuc)
          IF(lambmax.GT.2)THEN
            xlm(2) = xle(2)*cme
            DO j = 3, lambmax
              xle(j) = xle(j - 1)
     &                 *eg**2*cee*((3.0D0 + FLOAT(j))/(5.0D0 + FLOAT(j))
     &                 )**2
              xlm(j) = xle(j)*cme
            ENDDO
          ENDIF
          hscrtl = 0.0D0
          DO lamb = lambmin, lambmax
            IF(lamb/2*2.EQ.lamb)THEN
              hscrtl = hscrtl + xle(lamb)*ipar + xlm(lamb)*iodd
            ELSE
              hscrtl = hscrtl + xlm(lamb)*ipar + xle(lamb)*iodd
            ENDIF
          ENDDO
          SCRtl(i,0) = hscrtl
          Sumx = Sumx + SCRtl(i,0)
        ENDIF
      ENDDO
C-----do loop over discrete levels --------- done --------------------
      SCRtem(0) = Sumx
      DENhf = DENhf + Sumx
      END SUBROUTINE DECAYG
 
!---------------------------------------------------------------------------
 
      SUBROUTINE FISSION(Nnuc,Iec,Jc,Sumfis)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      INTEGER :: Iec, Jc, Nnuc
      REAL*8 :: Sumfis
C
C Local variables
C
      REAL*8 :: accn, ampl, ap1, ap2, atil, del, delp, ekin, ekinm, 
     &          erest, fisba, fric, gamma, gpart, htom, shredt, sum1, 
     &          sum2, sum3, sumf, sumgs, sumr, tau, temp
      REAL :: FLOAT
      INTEGER :: kn, knm
      REAL*8 :: TLF
C
C*** End of declarations rewritten by SPAG
C
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
C
C
      Sumfis = 0.0
      IF(EX(Iec,Nnuc).EQ.0.0D0)RETURN
C-----set level density parameter systematics
C-----EMPIRE-3.0-dependence
      CALL EGSMSYS(ap1,ap2,gamma,del,delp,Nnuc)
C-----set Ignatyuk type energy dependence for 'a'
      atil = ap1*A(Nnuc) + ap2*A(Nnuc)**0.666667
      atil = atil*ATIlnor(Nnuc)
C-----temperature fade-out of the shell correction
      temp = 0.
 
      accn = atil*(1 + SHC(Nnuc)*(1 - EXP((-gamma*EX(Iec,Nnuc))))
     &       /EX(Iec,Nnuc))
      IF(EX(Iec,Nnuc).GE.YRAst(Jc,Nnuc))
     &   temp = SQRT((EX(Iec,Nnuc) - YRAst(Jc,Nnuc))/accn)
      ampl = EXP(TEMp0*SHRt)
      shredt = 1.
      IF(temp.GE.TEMp0)shredt = ampl*EXP(( - SHRt*temp))
C-----temperature fade-out of the shell correction *** done *****
      fisba = FISb(Jc,Nnuc) - SHC(Nnuc)*SHCjf(Jc,Nnuc)*shredt
      ekinm = EX(Iec,Nnuc) - fisba
C
      IF(ekinm.LT.0.0D0)RETURN
      knm = AINT(ekinm/DE + 1.001)
      erest = ekinm - (knm - 1)*DE
C-----IEC to g.s.
      sumgs = TLF(ekinm)
C-----IEC to IEC
      sum1 = TLF(0.0D0)*ROF(Iec,Jc,Nnuc)
      IF(knm.EQ.1)THEN
        Sumfis = 0.5*(sumgs + sum1)*erest
        GOTO 10
      ENDIF
C-----IEC to IEC-1
      sum2 = TLF(DE)*ROF(Iec - 1,Jc,Nnuc)
C-----IEC to g.s.+1
      sum3 = TLF(FLOAT(knm - 1)*DE)*ROF(Iec - knm + 1,Jc,Nnuc)
      Sumfis = 0.5*((sumgs + sum3)*erest + (sum1 + sum2)*DE)
C-----correction to the trapezoidal integration rule
      Sumfis = Sumfis + ((sum3 - sumgs)*erest - (sum1 - sum2)*DE)/12.
      IF(knm.NE.2)THEN
        Sumfis = Sumfis + 0.5*(sum3 + sum2)*DE
        IF(knm.NE.3)THEN
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
   10 IF(Sumfis.LT.1.D-25)RETURN
 
      Sumfis = Sumfis*TUNefi(Nnuc)
      IF(BETav.NE.0.0D0)THEN
C--------reduction of the fission width due to possible return from the
C--------saddle point (nuclear viscosity 1-st effect)
        htom = 1.0
        Sumfis = Sumfis*(SQRT(1.0 + (BETav/2./htom)**2) - BETav/2./htom)
        IF(fisba - YRAst(Jc,Nnuc).GT.0.0D0)THEN
C-----------reduction of fission width due to the transient time needed
C-----------to form a saddle point (nuclear viscosity 2-nd effect)
C-----------according to Rastopchin et al. Sov. Jour. Nucl. Phys. 53,741(1991)
C-----------omega1=omega0=1.6*10^21 (1/s) hbar*omega=1MeV
C-----------BETAV critical =3.2; 0.19531 stands for 1/(2*omega1**2)
          gpart = DENhf/(RO(Iec,Jc,1,Nnuc) + RO(Iec,Jc,2,Nnuc))/2.D0/PI
C           GFIS = SUMFIS/RO(IEC,JC,NNUC)/2./PI
          tau = LOG(10.0*(fisba - YRAst(Jc,Nnuc))/temp)
          IF(BETav.LT.3.2D0)THEN
            tau = tau/BETav
          ELSE
            tau = tau*BETav*0.19531
          ENDIF
          fric = gpart*tau/0.6589
          fric = MIN(EXPmax,fric)
          IF(fric.GT.0D0)THEN
            fric = EXP(( - fric))
            Sumfis = Sumfis*fric
          ENDIF
        ENDIF
      ENDIF
      DENhf = DENhf + Sumfis
      RETURN
      END SUBROUTINE FISSION
 
!---------------------------------------------------------------------------
 
      FUNCTION TLF(Ekin)
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      REAL*8 :: Ekin
      REAL*8 :: TLF
C
C Local variables
C
      REAL*8 :: atlf, htom, pix2
C
C*** End of declarations rewritten by SPAG
C
C-----energy dependent transmission coefficient (note that htom is
C-----fixed below and does not depend on angular momentum as it might)
C
C
C
      DATA pix2/6.28318530717958647692528676655901D0/
      DATA htom/1.D0/
      TLF = 1.D0
      atlf = pix2*Ekin/htom
      IF(atlf.LT.38.D0)TLF = 1./(1. + EXP((-atlf)))
      END FUNCTION TLF
 
!---------------------------------------------------------------------------
 
      SUBROUTINE FISFIS(Nnuc,Iec,Ip,Jc,Sumfis,Mmod)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: FAZa2, PFIso, RFIso, TABs, TDIr, TFIso, TG2, TGIso, TISo
      REAL*8, DIMENSION(NFParab) :: HO, TF, VBArex
      COMMON /FIS_ISO/ TFIso, TGIso, TISo, RFIso, PFIso
      COMMON /IMAG  / TF, TDIr, TABs, TG2
      COMMON /VBAR  / VBArex, HO, FAZa2
C
C Dummy arguments
C
      INTEGER :: Iec, Ip, Jc, Mmod, Nnuc
      REAL*8 :: Sumfis
C
C Local variables
C
      REAL :: arg1
      REAL*8 :: barmin, ee, enh, exfis, rap, rap0, sfmin, snc,
     &          tindp, vbar, vsh
      REAL*8 :: DEXP
      REAL*8, DIMENSION(NFParab) :: enh_asym, sumtp, tfd, vbarmax, wdir
      REAL :: FLOAT
      INTEGER :: ib, ibar, ih, ih1, ipa, ist, iw, iw1, jcc, jnc, k, nr
      INTEGER :: INT
      REAL*8, DIMENSION(NFParab,NFParab) :: tabsp, tabspp, tdirp, tdirpp
      REAL*8, DIMENSION(NFHump) :: tdircont, tfcon
      REAL*8, DIMENSION(NFParab) :: tfdis
C
C*** End of declarations rewritten by SPAG
C
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
C
 
 
 
C
      ee = EX(Iec,Nnuc)
 
C-----initialization
      Sumfis = 0.D0
      jcc = Jc
      tdirp = 0.D0
      tdirpp = 0.D0
      sumtp = 0.D0
      wdir = 0.D0
      tabsp = 0.D0
      tabspp = 0.D0
      tfdis = 0.D0
      tfcon = 0.D0
      tdircont = 0.D0
      TF = 0.D0
 
      TABs = 0.D0
      TDIr = 0.D0
 
      IF(ee.EQ.0.0D0)RETURN
 
      DO ib = 1, NRBar
        vbarmax(ib) = 0.D0
        VBArex(ib) = EFB(ib)
      ENDDO
 
      DO k = 1, NRBar, 2
        HO(k) = H(1,INT(k/2) + 1)
      ENDDO
      DO k = 2, NRBar, 2
        HO(k) = H(1,NRHump + INT(k/2))
      ENDDO
 
      snc = FLOAT(Jc) + HIS(Nnuc)
      DO ibar = 1, NRHump
        enh_asym(ibar) = 1.D0
        IF(BFF(ibar).EQ.2)enh_asym(ibar) = 2.D0*snc + 1.D0
        IF(BFF(ibar).EQ.3)enh_asym(ibar) = 2.D0
      ENDDO
      enh = enh_asym(1)
      DO ih = 2, NRHump
        IF(enh_asym(ih).LT.enh)enh = enh_asym(ih)
      ENDDO
 
CCC Discrete transition states contribution
      DO nr = 1, NRFdis(1)
        sfmin = SFDis(nr,1)
        ist = 1
        IF(SFDis(nr,1).EQ.0.0.AND.IPFdis(nr,1).EQ.1)THEN
          sfmin = 0.
          ist = 2
        ENDIF
        IF(SFDis(nr,1).EQ.0.0.AND.IPFdis(nr,1).EQ. - 1)THEN
          sfmin = 1.
          ist = 2
        ENDIF
        sfmin = sfmin - HIS(Nnuc)
        DO jnc = INT(sfmin), Jc, ist
          IF(jnc.EQ.Jc.AND.IPFdis(nr,1).EQ.Ip)THEN
            DO k = 1, NRBar, 2
              HO(k) = H(nr,INT(k/2) + 1)
            ENDDO
            DO k = 2, NRBar, 2
              HO(k) = H(nr,NRHump + INT(k/2))
            ENDDO
            snc = FLOAT(jnc) + HIS(Nnuc)
            DO ibar = 1, NRBar
              exfis = EFDis(nr,ibar) + HJ(Nnuc,ibar)
     &                *(snc*(snc + 1) - SFDis(nr,ibar)
     &                **(SFDis(nr,ibar) + 1))
C
              VBArex(ibar) = EFB(ibar) + exfis
 
              IF(nr.EQ.1)vbarmax(ibar) = exfis
              IF(exfis.GT.vbarmax(ibar))vbarmax(ibar) = exfis
              IF(VBArex(NRHump + 1).GE.VBArex(1))VBArex(NRHump + 1)
     &           = VBArex(1) - 0.02
              IF(VBArex(NRHump + 1).GE.VBArex(2))VBArex(NRHump + 1)
     &           = VBArex(2) - 0.02
              IF(VBArex(NRHump + 2).GE.VBArex(2))VBArex(NRHump + 2)
     &           = VBArex(2) - 0.02
              IF(VBArex(NRHump + 2).GE.VBArex(3))VBArex(NRHump + 2)
     &           = VBArex(3) - 0.02
            ENDDO
C
            IF(FISopt(Nnuc).EQ.0.)THEN
 
C-----------------complete damping
              DO ibar = 1, NRHump
                arg1 = 2*PI*(VBArex(ibar) - ee)/H(nr,ibar)
                IF(arg1.GE.EXPmax)arg1 = EXPmax
                tfd(ibar) = 1.D0/(1.D0 + EXP(arg1))
              ENDDO
 
            ELSE
 
C-----------------partial damping
C                 IF(FISbar(Nnuc).EQ.3)CALL NUMBARR(Nnuc,Vbarex,HO)
              CALL WKBFIS(ee,Nnuc,tfd,tdirp,tabsp)
C-----------------forward absorption coefficients
              DO iw = 1, NRWel
                DO iw1 = iw + 1, NRWel + 1
                  tabspp(iw,iw1) = tabspp(iw,iw1) + tabsp(iw,iw1)
                ENDDO
              ENDDO
C-----------------backward absorption coefficients
              DO iw = NRWel + 1, 3, -1
                DO iw1 = iw - 1, 2, -1
                  tabspp(iw,iw1) = tabspp(iw,iw1) + tabsp(iw,iw1)
                ENDDO
              ENDDO
C-----------------direct transmission coefficient
              DO ih = 1, NRHump
                DO ih1 = 1, NRHump
                  IF(tdirp(ih,ih1).LT.1D-29)tdirp(ih,ih1) = 0.D0
                  tdirpp(ih,ih1) = tdirpp(ih,ih1) + tdirp(ih,ih1)
                ENDDO
              ENDDO
 
            ENDIF    ! PARTIAL OR FULL DAMPING
 
            DO ibar = 1, NRHump
              tfdis(ibar) = tfdis(ibar) + tfd(ibar)
            ENDDO
          ENDIF
        ENDDO
      ENDDO
 
CCC   Continuum contribution
      IF(Ip.EQ.1)ipa = 1
      IF(Ip.EQ. - 1)ipa = 2
C-----continuum direct and indirect weights for surogate optical model
      DO iw = 1, NRWel
        IF(AWF(iw).EQ.0.D0)THEN
          wdir(iw + 1) = 1.D0
        ELSE
          vsh = 0.D0
          barmin = MIN((vbarmax(iw) + EFB(iw) - vsh),
     &             (vbarmax(iw+1) + EFB(iw+1)) - vsh)
          vbar = VBArex(NRHump + iw) + EFB(NRHump + iw)
 
          wdir(iw + 1) = 2.D0*(ee - VBArex(NRHump + iw))
     &                   /((barmin - VBArex(NRHump+iw))
     &                   *(1.D0 + DEXP(-(ee-barmin)/AWF(iw))))
 
          wdir(iw + 1) = 2.D0*(ee - vbar)
     &                   /((barmin - vbar)*(1.D0 + DEXP(-(ee-barmin)
     &                   /AWF(iw))))
          IF(ee.GE.barmin)wdir(iw + 1) = 1.D0
          IF(ee.LE.VBArex(NRHump + iw))wdir(iw + 1) = 0.D0
        ENDIF
      ENDDO
 
C-----Continuum contribution to each hump transmission coefficient
      CALL SIMPSFIS(Nnuc,ee,jcc,ipa,tfcon)
      DO ih = 1, NRHump
        TF(ih) = tfdis(ih)*enh_asym(ih) + tfcon(ih)
        IF(TF(ih).LE.1.D-29)THEN
          TF(ih) = 0.D0
          tfdis(ih) = 0.D0
          tfcon(ih) = 0.D0
          Sumfis = 0.D0
          GOTO 10
        ENDIF
      ENDDO
C
      IF(NRHump.EQ.1)THEN
        Sumfis = TF(1)
        GOTO 10
      ENDIF
 
C     Continuum direct for surrogate optical model
      IF(AWF(1).NE.0..OR.AWF(2).NE.0.)THEN
        CALL SIMPSTDIR(Nnuc,ee,jcc,ipa,tdircont,vbarmax)
      ENDIF
C-----adding weighted continuum direct
      DO ih = 1, NRHump
        tdirpp(ih,ih) = TF(ih)
      ENDDO
 
      DO ih = 1, NRHump - 1
        tdirpp(ih,NRHump) = tdirpp(ih,NRHump)*enh + tdircont(ih)
     &                      *(1.D0 - wdir(ih + 1))
      ENDDO
C     if(nrbar.eq.5) tdirpp(2,3) = tdirpp(2,3) + tdircont(2)
C    &                       * (1.d0 - wdir(3))
C
      IF(FISopt(Nnuc).EQ.0.)THEN
 
C--------COMPLETE DAMPING + surrogate OPT.MOD
        tfd(NRHump) = TF(NRHump)
        DO ih = NRHump - 1, 1, -1
          IF(TF(ih) + tfd(ih + 1).GT.0)THEN
            tfd(ih) = tdirpp(ih,NRHump)*(1.D0 - wdir(ih + 1))
     &                + wdir(ih + 1)*TF(ih)*tfd(ih + 1)
     &                /(TF(ih) + tfd(ih + 1))
          ELSE
            tfd(ih) = 0.D0
          ENDIF
        ENDDO
        Sumfis = tfd(1)
 
      ELSE
 
C--------PARTIAL DAMPING
C--------adding weighted continuum absorption
        DO iw = 1, NRWel
          tabspp(iw,iw + 1) = tabspp(iw,iw + 1)*enh_asym(iw) + tfcon(iw)
     &                        *wdir(iw + 1)
          tabspp(iw + 1,iw) = tabspp(iw + 1,iw)*enh_asym(iw + 1)
     &                        + tfcon(iw)*wdir(iw)
        ENDDO
C--------sum of competting channels in wells
        sumtp(1) = 1.D0
        DO iw = 2, NRWel + 1
          sumtp(iw) = tdirpp(iw - 1,1) + tdirpp(iw,NRHump)
          DO iw1 = 2, NRWel + 1
            IF(iw1.NE.iw)sumtp(iw) = sumtp(iw) + tabspp(iw,iw1)
          ENDDO
        ENDDO
 
C--------normalization factor for the indirect terms
        rap0 = 0.D0
        rap = 1.D0
        DO iw = 2, NRWel + 1
          DO iw1 = iw + 1, NRWel + 1
            rap0 = rap0 + tabspp(iw,iw1)*tabspp(iw1,iw)
     &             /(sumtp(iw)*sumtp(iw1))
            rap = 1.D0/(1.D0 - rap0)
          ENDDO
        ENDDO
C--------Sumfis
        DO iw = 1, NRWel + 1
          tindp = 0.D0
          DO iw1 = 2, NRWel + 1
            IF(iw1.NE.iw)tindp = tindp + tabspp(iw,iw1)
     &                           *tdirpp(iw1,NRHump)
     &                           /(sumtp(iw)*sumtp(iw1))
          ENDDO
          tindp = tindp + tdirpp(iw,NRHump)/sumtp(iw)
          Sumfis = Sumfis + tindp*tabspp(1,iw)*rap
        ENDDO
        Sumfis = Sumfis + tdirpp(1,NRHump)
        TABs = tabspp(1,2)
        TDIr = tdirpp(1,NRHump)
 
        IF(FISopt(Nnuc).EQ.2.)THEN
C----------gamma transition in isomeric well, as asuggested by MS
          TG2 = .002
          IF(TG2.LT.0)TG2 = 0.D0
          Sumfis = TDIr + TABs*(TF(2) + RFIso*TG2)/(TF(1) + TF(2) + TG2)
        ELSE
          TG2 = 0.D0
        ENDIF
C--------FISSION CONTRIBUTION TO THE HAUSER-FESHBACH denominator
        IF(Sumfis.LT.1.D-25)Sumfis = 0.D0
 
      ENDIF ! END OF COMPLETE OR PARTIAL DAMPING
 
   10 DENhf = DENhf + Sumfis
 
C-----WRITING FISSION OUTPUT
 
      IF(Jc.EQ.1.AND.Ip.EQ.1.AND.Mmod.LT.2)THEN
        WRITE(80,*)'  '
        WRITE(80,'(1x,a19,f9.5,a4)')'Excitation energy =', ee, ' MeV'
        WRITE(80,*)' '
C---------single-humped
        IF(NRBar.EQ.1)WRITE(80,'(17x,3(a2,9x))')'Td', 'Tc', 'Tf'
C---------double-humped
        IF(NRBar.EQ.2.OR.
     &     (NRBar.EQ.3.AND.NRWel.EQ.1.AND.FISopt(Nnuc).EQ.0.))
     &     WRITE(80,'(22x,5(a3,9x))')'TAd', 'TBd', 'TAc', 'TBc', 'Tf'
        IF(NRBar.EQ.3.AND.NRWel.EQ.1.AND.FISopt(Nnuc).GE.1.)
     &     WRITE(80,'(22x,7(a4,7x))')'TAd', 'TBd', 'TAc', 'TBc', 'Tf', 
     &                               'Tdir', 'Tabs'
C---------triple-humped
        IF(NRBar.EQ.3.AND.NRWel.EQ.0)WRITE(80,'(22x,7(a4,7x))')'TAd', 
     &     'TBd', 'TCd', 'TAc', 'TBc', 'TCc', 'Tf'
 
        IF(NRBar.EQ.5)WRITE(80,'(16x,7(a4,7x),5(a6,5x))')'TAd', 'TBd', 
     &                      'TCd', 'TAc', 'TBc', 'TCc', 'Tf', 'Tdir12', 
     &                      'Tdir23', 'Tdir13', 'Tabs12', 'Tabs13'
      ENDIF
C-----single-humped
      IF(NRHump.EQ.1)WRITE(80,'(1x,a2,f4.1,1x,a3,I2,3g11.4)')'J=', snc, 
     &                     'Pi=', Ip, tfdis(1), tfcon(1), Sumfis
 
C-----double-humped
      IF(NRHump.EQ.2.)THEN
        WRITE(80,'(1x,a5,i1,1x,a2,f4.1,1x,a3,I2,7g11.4)')'Mode=', Mmod, 
     &        'J=', snc, 'Pi=', Ip, tfdis(1), tfdis(2), tfcon(1), 
     &        tfcon(2), Sumfis, TDIr, TABs
      ENDIF
C-----triple-humped
      IF(NRHump.EQ.3)WRITE(80,'(1x,a2,f4.1,1x,a3,I2,13g11.4)')'J=', snc, 
     &                     'Pi=', Ip, tfdis(1), tfdis(2), tfdis(3), 
     &                     tfcon(1), tfcon(2), tfcon(3), Sumfis, 
     &                     tdirpp(1,2), tdirpp(2,3), tdirpp(1,3), 
     &                     tabspp(1,2), tabspp(1,3)
 
      RETURN
      END SUBROUTINE FISFIS
 
!---------------------------------------------------------------------------
C=======================================================================
      SUBROUTINE SIMPSFIS(Nnuc,Ee,Jcc,Ipa,Tfcon)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      REAL*8 :: Ee
      INTEGER :: Ipa, Jcc, Nnuc
      REAL*8, DIMENSION(NFHump) :: Tfcon
C
C Local variables
C
      REAL*8 :: arg1, tfcc, uexcit1, ux1
      LOGICAL :: discrete
      INTEGER :: i, ibar, iphas_opt, nn
      INTEGER :: INT
      REAL*8, DIMENSION(2*NFParab) :: phase
      REAL*8, DIMENSION(NFParab) :: phase_h
C
C*** End of declarations rewritten by SPAG
C
C-----Simpson integration
C
      discrete = .FALSE.
C-----iphas_opt=0 parabolic shape, iphas_opt=1 non-parabolic numerical shape
      iphas_opt = 1
      DO ibar = 1, NRHump
        tfcc = 0.D0
        Tfcon(ibar) = 0.D0
        DO i = 1, NRBinfis(ibar)
          ux1 = XMInn(ibar) + (i - 1)*DEStepp(ibar)
          IF(FISbar(Nnuc).EQ.3)THEN
            uexcit1 = Ee - ux1
            CALL PHASES(uexcit1,phase,phase_h,Nnuc,iphas_opt,discrete)
            arg1 = 2.D0*phase_h(ibar)
          ELSE
            arg1 = 2*PI*(ux1 + EFB(ibar) - Ee)/HCOnt(ibar)
          ENDIF
          IF(arg1.GE.EXPmax)arg1 = EXPmax
          nn = 2
          IF((i*0.5).EQ.INT(i/2))nn = 4
          IF(i.EQ.1.OR.i.EQ.(NRBinfis(ibar)))nn = 1
          tfcc = tfcc + nn*ROFisp(i,Jcc,Ipa,ibar)/(1.D0 + EXP(arg1))
        ENDDO
        tfcc = tfcc*DEStepp(ibar)/3.
        Tfcon(ibar) = tfcc*TUNefi(Nnuc)
      ENDDO
      RETURN
      END SUBROUTINE SIMPSFIS
 
!---------------------------------------------------------------------------
C=======================================================================
      SUBROUTINE SIMPSTDIR(Nnuc,Ee,Jcc,Ipa,Tdircont,Vbarmax)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      REAL*8 :: Ee
      INTEGER :: Ipa, Jcc, Nnuc
      REAL*8, DIMENSION(NFHump) :: Tdircont
      REAL*8, DIMENSION(NFHump) :: Vbarmax
C
C Local variables
C
      REAL*8 :: arg1, dmom, romin, uexcit1, ux1
      LOGICAL :: discrete
      INTEGER :: i, ib, ibar, ih, ih1, ii, iphas_opt, nn
      REAL*8, DIMENSION(2*NFParab) :: phase
      REAL*8, DIMENSION(NFParab) :: phase_h
      REAL*8, DIMENSION(NFHump,NFHump) :: tdirc
C
C*** End of declarations rewritten by SPAG
C
C-----Simpson integration for direct transmission
 
C
      discrete = .FALSE.
      iphas_opt = 1
      arg1 = 0.D0
      dmom = 0.D0
      DO ih = 1, NRHump
        tdirc(ih,NRHump) = 0.D0
        Tdircont(ih) = 0.D0
      ENDDO
      ibar = 1
      ii = MAX(INT(Vbarmax(1)/DEStepp(1)),1)
      romin = ROFisp(ii,Jcc,Ipa,ih)
      DO ib = 1, NRHump
        IF(romin.GT.ROFisp(ii,Jcc,Ipa,ib))THEN
          romin = ROFisp(ii,Jcc,Ipa,ib)
          ibar = ib
          ii = MAX(INT(Vbarmax(ib)/DEStepp(ib)),1)
        ENDIF
      ENDDO
      DO i = 1, NRBinfis(ibar)
        ux1 = XMInn(ibar) + (i - 1)*DEStepp(ibar)
        IF(FISbar(Nnuc).EQ.3)THEN
          uexcit1 = Ee - ux1
          CALL PHASES(uexcit1,phase,phase_h,Nnuc,iphas_opt,discrete)
        ENDIF
 
        DO ib = 1, NRHump
          IF(FISbar(Nnuc).EQ.3)THEN
            arg1 = 2.D0*phase_h(ib)
          ELSE
            arg1 = 2.D0*PI*(ux1 + EFB(ib) - Ee)/HCOnt(ib)
          ENDIF
          IF(arg1.GE.EXPmax)arg1 = EXPmax
          tdirc(ib,ib) = 1.D0/(1.D0 + EXP(arg1))
        ENDDO
 
        DO ih1 = NRHump - 1, 1, -1
          IF(Ee.LT.(EFB(NRHump+ih1) + ux1))THEN
            tdirc(ih1,NRHump) = tdirc(ih1,ih1)*tdirc(ih1 + 1,NRHump)
            CYCLE
          ENDIF
C           arg1 =  - 2.d0 * PI * (ux1 + EFB(NRHump + ih1) - Ee)
C     &               /HCOnt(Nrhump + ih1)
C           arg1=0.d0   ! to avoid fluctuations
          dmom = (1.D0 - tdirc(ih1,ih1))*(1.D0 - tdirc(ih1 + 1,NRHump))
          tdirc(ih1,NRHump) = tdirc(ih1,ih1)*tdirc(ih1 + 1,NRHump)
     &                        /(1.D0 + 2.D0*DSQRT(dmom) + dmom)
C    &             (1.d0 + 2.d0 * dSQRT(dmom) * COS(arg1) + dmom)
        ENDDO
        nn = 2
        IF((i*0.5).EQ.INT(i/2))nn = 4
        IF(i.EQ.1.OR.i.EQ.(NRBinfis(ibar)))nn = 1
 
        DO ih = 1, NRHump
          tdirc(ih,NRHump) = nn*tdirc(ih,NRHump)*ROFisp(i,Jcc,Ipa,ibar)
          Tdircont(ih) = Tdircont(ih) + tdirc(ih,NRHump)
        ENDDO
      ENDDO
      DO ih = 1, NRHump
        Tdircont(ih) = Tdircont(ih)*DEStepp(ibar)/3.
      ENDDO
      RETURN
      END SUBROUTINE SIMPSTDIR
