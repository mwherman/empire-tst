C
Ccc   * $Author: herman $
Ccc   * $Date: 2004-06-11 22:13:34 $
Ccc   * $Id: HF-comp.f,v 1.23 2004-06-11 22:13:34 herman Exp $
C
      SUBROUTINE ACCUM(Iec, Nnuc, Nnur, Nejc, Xnor)
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:mpu*
Ccc   *                         A C C U M                                *
Ccc   *                                                                  *
Ccc   * Normalizes scratch arrays SCRT and SCRTL with the population     *
Ccc   * divided by the H-F denominator and accumulates the rsult on the  *
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
Ccc   * author: M.Herman                                                 *
Ccc   * date:   28.Oct.1993                                              *
Ccc   * revision:1    by:M. Herman                on:30.Jul.1997         *
Ccc   * Transitions to discrete levels distributed between adjacent      *
Ccc   * spectrum bins so that after spectrum integration average         *
Ccc   * energy is conserved.                                             *
Ccc   *                                                                  *
Ccc   * revision:2    by:M. Herman                on:31.Jul.2003         *
Ccc   * New algorithm for exclusive spectra using EXCLUSIVEC and         *
Ccc   * EXCLUSIVEL                                                       *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C
C Dummy arguments
C
      INTEGER Iec, Nejc, Nnuc, Nnur
      DOUBLE PRECISION Xnor
C
C Local variables
C
      DOUBLE PRECISION eemi, excnq, pop1, pop2, poph, popl, pops, popt,
     &                 xcse
      REAL FLOAT
      INTEGER icse, icsh, icsl, ie, il, j, nexrt
      INTEGER INT, MAX0 
C
C
C
      IF(Nnuc.EQ.Nnur)THEN
         excnq = EX(Iec, Nnuc)
      ELSE
         excnq = EX(Iec, Nnuc) - Q(Nejc, Nnuc)
      ENDIF
      nexrt = (excnq - ECUt(Nnur))/DE + 2.0001
      DO ie = 1, nexrt                      !loop over residual energies
         icse = (excnq - EX(ie, Nnur))/DE + 1.0001
C
C        IF(Nejc .EQ. 0)
C        &   WRITE(6,*)'continuum bin ',icse ,'EX ',EX(ie,Nnur),' Eemiss',
C        &             excnq-EX(ie,Nnur)
C
         popt =0.0
         DO j = 1, NLW, LTUrbo              !loop over residual spins
            pop1 = Xnor*SCRt(ie, j, 1, Nejc)
            pop2 = Xnor*SCRt(ie, j, 2, Nejc)
            pops = pop1 + pop2
            IF(ie.EQ.1)pops = pops*0.5
            popt = popt + pops !sum over spin/pi at a given energy bin
            icse = MAX0(2, icse)
            AUSpec(icse, Nejc) = AUSpec(icse, Nejc) + pop1 + pop2
            CSE(icse, Nejc, Nnuc) = CSE(icse, Nejc, Nnuc) + pops
            IF(Nnuc.EQ.1 .AND. ENDf.EQ.2) THEN 
               piece = pops/4.0/PI
               DO nang = 1, NDANG
                  CSEa(icse, nang, nejc, 1) = CSEa(icse, nang, nejc, 1)
     &                                        + piece
               ENDDO
            ENDIF 
            POP(ie, j, 1, Nnur) = POP(ie, j, 1, Nnur) + pop1
            POP(ie, j, 2, Nnur) = POP(ie, j, 2, Nnur) + pop2
            IF(Nejc.NE.0 .AND. POPmax(Nnur).LT.POP(ie, j, 1, Nnur))
     &         POPmax(Nnur) = POP(ie, j, 1, Nnur)
         ENDDO
         IF(ENDf.EQ.1 .AND. popt.NE.0.0D+0)  
     &      CALL EXCLUSIVEC(Iec, ie, Nejc, Nnuc, Nnur,popt)
         popt = popt*DE
      ENDDO
      DO il = 1, NLV(Nnur)
         eemi = excnq - ELV(il, Nnur)
         IF(eemi.LT.0.0D0)RETURN
         pop1 = Xnor*SCRtl(il, Nejc)
C--------Transitions to discrete levels are distributed
C--------between the nearest spectrum bins (inversly proportional to the
C--------distance of the actual energy to the bin energy excluding elastic
C--------if ENDf.NE.0
cc       IF((il*Nnuc).NE.1 .OR. IZA(Nnur).NE.IZA(0) .OR. ENDf.EQ.0.0D+0
cc   &      .OR. Iec.NE.NEX(1)) THEN 
         IF(Nnuc.NE.1 .OR. ENDf.NE.1 .OR. Iec.NE.NEX(1) 
     &      .OR. Nejc.EQ.0) THEN 
            xcse = eemi/DE + 1.0001
            icsl = INT(xcse)
            icsh = icsl + 1
            popl = pop1*(FLOAT(icsh) - xcse)/DE
            popll = popl            !we also need popl not multiplied by 2
            IF(icsl.EQ.1)popl = 2.0*popl
            poph = pop1*(xcse - FLOAT(icsl))/DE
            IF(icsl.LE.NDECSE) THEN
               CSE(icsl, Nejc, Nnuc) = CSE(icsl, Nejc, Nnuc) + popl
               IF(ENDf.EQ.1 .AND. popll.NE.0.0D+0) 
     &            CALL EXCLUSIVEL(Iec, icsl, Nejc, Nnuc,Nnur, popll)
            ENDIF
            IF(icsh.LE.NDECSE)THEN
               CSE(icsh, Nejc, Nnuc) = CSE(icsh, Nejc, Nnuc) + poph
               IF(ENDf.EQ.1 .AND. poph.NE.0.0D+0) 
     &            CALL EXCLUSIVEL(Iec, icsh, Nejc, Nnuc,Nnur, poph)
            ELSE
               WRITE(6, *)' '
               WRITE(6, *)' OOPS! I AM OUT OF NDECSE DIMENSION IN ACCUM'
               STOP
            ENDIF
         ELSE
         ENDIF 
         POPlv(il, Nnur) = POPlv(il, Nnur) + pop1
         REClev(il, Nejc) = REClev(il, Nejc) + pop1
C--------Add isotropic CN contribution to direct ang. distributions
         IF(Nnuc.EQ.1 .AND. Iec.EQ.NEX(1) .AND. Nejc.NE.0)THEN
            pop1 = pop1/4.0/PI
            DO na = 1, NDANG
               CSAlev(na, il, Nejc) = CSAlev(na, il, Nejc) + pop1
            ENDDO
         ENDIF
      ENDDO
      END
C
C
      SUBROUTINE EXCLUSIVEC(Iec, Ief, Nejc, Nnuc, Nnur, Popt)
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
Ccc   *       Ief  - energy index of the final state                     *
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
Ccc   * author: M.Herman                                                 *
Ccc   * date:     July 2003                                              *
Ccc   * revision:1    by:                         on:                    *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C
C Dummy arguments
C
      INTEGER Iec, Ief, Nejc, Nnuc, Nnur
      DOUBLE PRECISION Popt
C
C Local variables
C
C     INTEGER iang, icsp, iejc, ie 
      INTEGER icsp, iejc, ie 
C     DOUBLE PRECISION excnq, popa, xnor 
      DOUBLE PRECISION excnq, xnor 
C
C     POPcse(Ief,Nejc,icsp,Nnuc)  - spectrum for the population of the
C                                   energy bin with index Ief in Nnuc by
C                                   Nejc particles (cumulative over all
C                                   decays leading to this energy bin)
C
      IF(Nnuc.EQ.Nnur)THEN
         excnq = EX(Iec, Nnuc)
      ELSE
         excnq = EX(Iec, Nnuc) - Q(Nejc, Nnuc)
      ENDIF
C-----Contribution comming straight from the current decay
      icsp = INT((excnq - EX(Ief,Nnur))/DE + 1.0001)
      POPcse(Ief,Nejc,icsp,Nnur) =  POPcse(Ief,Nejc,icsp,Nnur) + Popt
      
C-----Contribution due to feeding spectra from Nnuc 
C-----DE spectra
      IF(Nnuc.NE.1 .OR. Nejc.EQ.0) THEN !skip the first CN except gammas 
         IF(POPbin(Iec, Nnuc) .EQ. 0) RETURN 
         xnor = Popt*DE/POPbin(Iec, Nnuc)
         DO ie = 1, NDECSE 
            DO iejc = 0, NDEjc 
               IF(POPcse(Iec,iejc,ie,Nnuc).NE.0) 
     &            POPcse(Ief,iejc,ie,Nnur) = POPcse(Ief,iejc,ie,Nnur) +
     &            POPcse(Iec,iejc,ie,Nnuc)*xnor 
            ENDDO 
C--------neutron and proton DDX spectra using portions      
            DO iejc = 1, NDEJCD
               IF(POPcseaf(Iec,iejc,ie,Nnuc).NE.0) THEN 
                  POPcseaf(Ief,iejc,ie,Nnur) = 
     &            POPcseaf(Ief,iejc,ie,Nnur) +
     &            POPcseaf(Iec,iejc,ie,Nnuc)*xnor 
               ENDIF 
            ENDDO 
         ENDDO 
      ENDIF 


      END


      SUBROUTINE EXCLUSIVEL(Iec, Ie, Nejc, Nnuc, Nnur, Popt)
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
Ccc   * author: M.Herman                                                 *
Ccc   * date:     July 2003                                              *
Ccc   * revision:1    by:                         on:                    *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C
C Dummy arguments
C
C     INTEGER Iec, Ief, Nejc, Nnuc, Nnur
      INTEGER Iec, Nejc, Nnuc, Nnur
C     DOUBLE PRECISION Popt, excnq, xnor
      DOUBLE PRECISION Popt, xnor
C
C Local variables
C
C     INTEGER iang, icsp, iejc, ie 
      INTEGER iejc, ie 
C     INTEGER INT
C
C     POPcse(Ief,Nejc,icsp,Nnuc)  - spectrum for the population of the
C                                   energy bin with index Ief in Nnuc by
C                                   Nejc particles (cumulative over all
C                                   decays leading to this energy bin)
C
C-----Contribution comming straight from the current decay
      POPcse(0,Nejc,Ie,Nnur) =  POPcse(0,Nejc,Ie,Nnur) + Popt
C-----Contribution due to feeding spectra from Nnuc 
C-----DE spectra
      IF(Nnuc.NE.1 .OR. Nejc.EQ.0) THEN !skip the first CN except gammas 
C        RCN 01/2004
         IF(POPbin(Iec, Nnuc).GT.0.) THEN    
          xnor = Popt*DE/POPbin(Iec, Nnuc)
          DO iesp = 1, NDECSE 
            DO iejc = 0, NDEjc 
               IF(POPcse(Iec,iejc,iesp,Nnuc).NE.0)
     &            POPcse(0,iejc,iesp,Nnur) = POPcse(0,iejc,iesp,Nnur) +
     &            POPcse(Iec,iejc,iesp,Nnuc)*xnor
            ENDDO 
C--------neutron and proton DDX spectra using portions      
            DO iejc = 1, NDEJCD
               IF(POPcseaf(Iec,iejc,iesp,Nnuc).NE.0) THEN
                  POPcseaf(0,iejc,iesp,Nnur) = 
     &            POPcseaf(0,iejc,iesp,Nnur) +
     &            POPcseaf(Iec,iejc,iesp,Nnuc)*xnor 
                  ief=0
               ENDIF 
            ENDDO 
          ENDDO 
         ENDIF
      ENDIF 


      END


C
      SUBROUTINE DECAY(Nnuc, Iec, Jc, Ipc, Nnur, Nejc, Sum)
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
Ccc   * author: M.Herman                                                 *
Ccc   * date:   28.Oct.1993                                              *
Ccc   * revision:#    by:name                     on:xx.mon.199x         *
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
C
      Sum = 0.0
      hisr = HIS(Nnur)
      xjc = FLOAT(Jc) + HIS(Nnuc)
C-----clear scratch matrices
      SCRtem(Nejc) = 0.0
      DO j = 1, NLW
         DO i = 1, NEX(Nnur) + 1
            SCRt(i, j, 1, Nejc) = 0.0
            SCRt(i, j, 2, Nejc) = 0.0
         ENDDO
      ENDDO
      iexc = NEX(Nnuc) - NEXr(Nejc, Nnuc)
      itlc = iexc - 5
      iermax = Iec - iexc
      IF(iermax.GE.1)THEN
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
               lmaxf = MIN0(NLW, lmaxf)
               ipar = (1 + Ipc*( - 1)**(lmin - 1))/2
C--------------parity index of r.n. state populated by emission with LMIN
               ip1 = 2 - ipar
C--------------parity index of r.n. state populated by emission with LMIN+1
               ip2 = 1
               IF(ip1.EQ.1)ip2 = 2
C--------------decay to the highest possible bin (non neutron only)
               IF(ZEJc(Nejc).NE.0.0D0)THEN
                  lmax = lmaxf
                  lmax = MIN0(LMAxtl(6, Nejc, Nnur), lmax)
C-----------------do loop over l (odd and even l-values treated separately)
C-----------------IP1 and IP2 decide to which parity each SUMTL  goes
                  sumtl1 = 0.0
                  DO l = lmin, lmax, 2
                     sumtl1 = sumtl1 + TL(5, l, Nejc, Nnur)
                  ENDDO
                  sumtl2 = 0.0
                  DO l = lmin + 1, lmax, 2
                     sumtl2 = sumtl2 + TL(5, l, Nejc, Nnur)
                  ENDDO
C-----------------do loop over l   ***done***
                  SCRt(iermax, jr, ip1, Nejc)
     &               = SCRt(iermax, jr, ip1, Nejc)
     &               + sumtl1*RO(iermax, jr, Nnur)
     &               *TURbo*TUNe(Nejc, Nnuc)
                  SCRt(iermax, jr, ip2, Nejc)
     &               = SCRt(iermax, jr, ip2, Nejc)
     &               + sumtl2*RO(iermax, jr, Nnur)
     &               *TURbo*TUNe(Nejc, Nnuc)
               ENDIF
C--------------decay to the highest but one bin (conditional see the next IF)
               IF(ZEJc(Nejc).EQ.0.0D0 .AND. Iec.EQ.NEX(Nnuc) - 1)THEN
                  lmax = lmaxf
                  lmax = MIN0(LMAxtl(6, Nejc, Nnur), lmax)
C-----------------do loop over l (odd and even l-values treated separately)
C-----------------IP1 and IP2 decide which parity each SUMTL  goes to
                  sumtl1 = 0.0
                  DO l = lmin, lmax, 2
                     sumtl1 = sumtl1 + TL(6, l, Nejc, Nnur)
                  ENDDO
                  sumtl2 = 0.0
                  DO l = lmin + 1, lmax, 2
                     sumtl2 = sumtl2 + TL(6, l, Nejc, Nnur)
                  ENDDO
C-----------------do loop over l   ***done***
C-----------------CORR in the next lines accounts for the Tl interpolation
C-----------------and integration over overlaping bins (2/3), it turned out it must
C-----------------be energy step and also emission step dependent
                  corr = 0.4444/(DE - XN(Nnur) + XN(1))
     &                   *TURbo*TUNe(Nejc, Nnuc)
                  SCRt(iermax, jr, ip1, Nejc)
     &               = SCRt(iermax, jr, ip1, Nejc)
     &               + sumtl1*RO(iermax, jr, Nnur)*corr
                  SCRt(iermax, jr, ip2, Nejc)
     &               = SCRt(iermax, jr, ip2, Nejc)
     &               + sumtl2*RO(iermax, jr, Nnur)*corr
               ENDIF
C--------------do loop over r.n. energies (highest bin and eventually the second
C--------------bin from the top excluded as already done)
               DO ier = iermax - 1, 1, -1
                  ietl = Iec - ier - itlc
                  lmax = lmaxf
                  lmax = MIN0(LMAxtl(ietl, Nejc, Nnur), lmax)
C-----------------do loop over l (odd and even l-values treated separately)
C-----------------IP1 and IP2 decide which parity each SUMTL  goes to
                  sumtl1 = 0.0
                  DO l = lmin, lmax, 2
                     sumtl1 = sumtl1 + TL(ietl, l, Nejc, Nnur)
                  ENDDO
                  sumtl2 = 0.0
                  DO l = lmin + 1, lmax, 2
                     sumtl2 = sumtl2 + TL(ietl, l, Nejc, Nnur)
                  ENDDO
C-----------------do loop over l   ***done***
C
                  SCRt(ier, jr, ip1, Nejc) = SCRt(ier, jr, ip1, Nejc)
     &               + sumtl1*RO(ier, jr, Nnur)*TURbo*TUNe(Nejc, Nnuc)
                  SCRt(ier, jr, ip2, Nejc) = SCRt(ier, jr, ip2, Nejc)
     &               + sumtl2*RO(ier, jr, Nnur)*TURbo*TUNe(Nejc, Nnuc)
               ENDDO
C--------------do loop over r.n. energies ***done***
            ENDDO
C-----------do loop over channel spins ***done***
         ENDDO
C--------do loop over and r.n. spins ***done***
C--------decay to the continuum ------ done -----------------------------
99001    FORMAT(1X, F5.2, 12G10.3)
C--------trapezoidal integration of ro*tl in continuum for ejectile nejc
         DO j = 1, NLW, LTUrbo
            DO i = 1, iermax
               Sum = Sum + SCRt(i, j, 1, Nejc) + SCRt(i, j, 2, Nejc)
            ENDDO
            Sum = Sum - 0.5*(SCRt(1, j, 1, Nejc) + SCRt(1, j, 2, Nejc))
         ENDDO
         Sum = Sum*DE
C--------integration of ro*tl in continuum for ejectile nejc -- done ----
C--------
C--------decay to discrete levels
C-----
      ENDIF
      IF(RORed.NE.0.0D0)THEN
         DO i = 1, NLV(Nejc)
            SCRtl(i, Nejc) = 0.0
         ENDDO
         eoutc = EX(Iec, Nnuc) - Q(Nejc, Nnuc)
C--------do loop over discrete levels -----------------------------------
         DO i = 1, NLV(Nnur)
            eout = eoutc - ELV(i, Nnur)
C-----------level above the bin
C           2.18+
            IF(eout.LT.0.0D0)GOTO 100
            cor = 1.0
            sumdl = 0.0
            CALL TLLOC(Nnur, Nejc, eout, il, frde)
            smin = ABS(XJLv(i, Nnur) - SEJc(Nejc))
            smax = XJLv(i, Nnur) + SEJc(Nejc) + 0.01
            s = smin
C-----------loop over channel spin ----------------------------------------
 20         lmin = INT(ABS(xjc - s) + 1.01)
            lmax = INT(xjc + s + 1.01)
            lmax = MIN0(NLW, lmax)
C-----------do loop over l ------------------------------------------------
            DO l = lmin, lmax
               ipar = 1 + LVP(i, Nnur)*Ipc*( - 1)**(l - 1)
               IF(ipar.NE.0)sumdl = sumdl + TL(il, l, Nejc, Nnur)
     &                              + frde*(TL(il + 1, l, Nejc, Nnur)
     &                              - TL(il, l, Nejc, Nnur))
            ENDDO
C-----------do loop over l --- done ----------------------------------------
            s = s + 1.
            IF(s.LE.smax)GOTO 20
C-----------loop over channel spin ------ done ----------------------------
            sumdl = sumdl*RORed*cor*TUNe(Nejc, Nnuc)
            SCRtl(i, Nejc) = sumdl
            Sum = Sum + sumdl
         ENDDO
C--------do loop over discrete levels --------- done --------------------
      ENDIF
 100  DENhf = DENhf + Sum
      SCRtem(Nejc) = Sum
Cpr   WRITE(6,*) 'TOTAL SUM=',SUM
      END
C
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
Ccc   * author: M.Herman                                                 *
Ccc   * date:   11.Jan.1996                                              *
Ccc   * revision:     by:                         on:                    *
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
C
C
C
      IF(IOUt.GT.2)WRITE(6, 99001)
99001 FORMAT(1X, ////, 1X, 27('*'), /, 1X, 
     &       'Discrete gamma transitions ', /, 1X, 27('*'), //)
      DO i = 1, NLV(Nnuc) - 1
         l = NLV(Nnuc) - i + 1
         IF(BR(l, 1, 2, Nnuc).EQ.0.)THEN
            IF(IOUt.GT.2)WRITE(6, 99002)ELV(l, Nnuc), LVP(l, Nnuc)
     &                                  *XJLv(l, Nnuc), POPlv(l, Nnuc)
99002       FORMAT(1X, //, 5X, 'Level of energy  ', F8.4, ' MeV', 
     &             ' and spin ', F6.1, ' with population ', G13.5, 
     &             ' mb is not depopulated ')
         ELSE
            popl = POPlv(l, Nnuc)
            IF(popl.NE.0.0D0)THEN
               IF(IOUt.GT.2)WRITE(6, 99003)ELV(l, Nnuc), LVP(l, Nnuc)
     &                            *XJLv(l, Nnuc), popl
99003          FORMAT(1X//, 5X, 'Decay of  ', F7.4, ' MeV  ', F5.1, 
     &                ' level with final population ', G13.5, ' mb', /, 
     &                5X, 'Level populated ', 4X, 'E.gamma ', 4X, 
     &                'Intensity  ', /)
               DO j = 1, NDBR
                  j1 = NINT(BR(l, j, 1, Nnuc))
                  IF(j1.EQ.0)GOTO 100
                  IF(j1.GE.l)THEN
                     WRITE(6, 99004)
99004                FORMAT(10X, 
     &                      'WARNING: ERROR IN DISCRETE LEVEL DECA', 
     &                      'Y DATA', /, 10X, 
     &                      'FINAL LEVEL ABOVE THE INITIAL ONE', /, 10X, 
     &                      'FURTHER DECAY NOT CONSIDERED ')
                     WRITE(6, 
     &'(10X,''WARNING: NUCLEUS '',I3,''-'',A2,                        ''
     &LEVEL '',I3)')INT(A(Nnuc)), SYMb(Nnuc), l
                     GOTO 99999
                  ENDIF
                  gacs = popl*BR(l, j, 2, Nnuc)
                  POPlv(j1, Nnuc) = POPlv(j1, Nnuc) + gacs
                  gacs = gacs/(1 + BR(l, j, 3, Nnuc)) ! int. conversion
                  egd = ELV(l, Nnuc) - ELV(j1, Nnuc)
                  icse = 2.0001 + egd/DE
                  CSE(icse, 0, Nnuc) = CSE(icse, 0, Nnuc) + gacs/DE
                  CSEmis(0, Nnuc) = CSEmis(0, Nnuc) + gacs
C-----------------Add transition to the exclusive gamma spectrum 
C-----------------NOTE: internal conversion taken into account 
                  IF(ENDf.EQ.1) POPcse(0,0,icse,Nnuc) = POPcse(0,0,icse,
     &               Nnuc)+ gacs/DE
                  IF(IOUt.GT.2)WRITE(6, 99005)ELV(j1, Nnuc), 
     &                               LVP(j1, Nnuc)*XJLv(j1, Nnuc), egd, 
     &                               gacs
99005             FORMAT(5X, F7.4, 2X, F5.1, 5X, F7.4, 5X, G13.5, ' mb')
               ENDDO
            ENDIF
         ENDIF
 100  ENDDO
99999 END
C
      SUBROUTINE DECAYG(Nnuc, Iec, Jc, Ipc, Sum)
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
Ccc   * author: M.Herman                                                 *
Ccc   * date:12 Jan. 1994                                                *
Ccc   * revision:#    by:name                     on:xx.mon.199x         *
Ccc   *                                                                  *
Ccc   ********************************************************************
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
      DOUBLE PRECISION eg, se1, se2, se2m1, sm1, xjc
C     DOUBLE PRECISION gamma, accn, atil, t
      DOUBLE PRECISION E1, E2, XM1
C-----Plujko_new
Cb    REAL FLOAT
      REAL FLOAT,fSpinIc
C-----Plujko_new(End)
      INTEGER i, ier, ineg, iodd, ipar, ipos, j, jmax, jmin, lmax, lmin
      INTEGER MAX0, MIN0
C
C
C
C-----Plujko_new - under construction
      GO TO 250
C
C
C     file  for output of widths
C     OPEN(UNIT=54, FILE='SUMHF.OUT' , STATUS='NEW' )
      IF (F_PRINT.EQ.11) THEN
       OPEN(UNIT=54, FILE='SUMHF.OUT' , STATUS='NEW' )
       WRITE(54, * ) 'Nnuc - decaying nucleus  '
       WRITE(54, * ) 'Iec  - energy index of the decaying state '
       WRITE(54, * ) 'EX(Iec, Nnuc)- energy of the decaying state '
       WRITE(54, * ) 'SpinIc   - spin  of the decaying state  '
       WRITE(54, * ) 'Ipc  - parity of the decaying state    '
       WRITE(54, * )'Sum - sum of transmission coefficients over all '
       WRITE(54, * )'outgoing channels for the requested decay (partial'
       WRITE(54, * )' sums are stored in SCRT and SCRTL arrays for '
       WRITE(54, * )'continuum and discrete levels respectively. SUMs '
       WRITE(54, * )'for all ejectiles combine to the total '
       WRITE(54, * )'Hauser-Feshbach denominator. Inverse of the latter'
       WRITE(54, * )' multiplied by the population of the '
       WRITE(54, * )'(NNUC,IEC,JC,IPC) state is used to normalize  '
       WRITE(54, * )'SCRT and SCRTL matrices to give residual  '
       WRITE(54, * )'nucleuse population. '
      ENDIF
      IF (F_PRINT.EQ.11.OR.F_PRINT.EQ.110) THEN
       WRITE(54, * ) ' =============================================='
       WRITE(54,*)'Projectile energy = ',EINl,' MeV'
       WRITE(54, * )'Gc = Sum / (2 *Pi * RO(Iec, Jc , Nnuc) )'
       WRITE(54, * ) ' ______________________________________________'
       WRITE(54, * ) 'Nnuc   Iec  EX(Iec, Nnuc)  SpinIc  Ipc     Gc'
      ENDIF
      F_PRINT = 100
250   CONTINUE
C-----Plujko_new(End)

      Sum = 0.0
      SCRtem(0) = 0.0
      xjc = FLOAT(Jc) + HIS(Nnuc)
C     WRITE(6,*) ' decaying state spin=',xjc,' and parity=',ipc
      jmin = MAX0(1, Jc - 2)
      jmax = MIN0(NLW, Jc + 2)
C-----clear scratch matrix (continuum)
      DO j = 1, NLW
         DO i = 1, NEX(Nnuc)
            SCRt(i, j, 1, 0) = 0.0
            SCRt(i, j, 2, 0) = 0.0
         ENDDO
      ENDDO
C-----clear scratch matrix (discrete levels)
      DO i = 1, NLV(Nnuc)
         SCRtl(i, 0) = 0.0
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
         eg = EX(Iec, Nnuc) - EX(ier, Nnuc)
C--------next 3 lines could be replaced with the matrix
C--------Plujko_new
         se1 = E1(Nnuc,Z,A,eg, TNUc(ier, Nnuc),Uexcit(ier,Nnuc))*
     &  TUNe(0, Nnuc)
Cb         se1 = E1(eg, TNUc(ier, Nnuc))*TUNe(0, Nnuc)
C--------Plujko_new(End)
         se2 = E2(eg)*TUNe(0, Nnuc)
         sm1 = XM1(eg)*TUNe(0, Nnuc)
         se2m1 = se2 + sm1
         IF(Jc.GT.2)THEN
            IF(Jc.GE.NLW - 1)GOTO 50
            SCRt(ier, Jc - 2, ipos, 0) = se2*RO(ier, Jc - 2, Nnuc)
            SCRt(ier, Jc - 1, ipos, 0) = se2m1*RO(ier, Jc - 1, Nnuc)
            SCRt(ier, Jc, ipos, 0) = se2m1*RO(ier, Jc, Nnuc)
            SCRt(ier, Jc + 1, ipos, 0) = se2m1*RO(ier, Jc + 1, Nnuc)
            SCRt(ier, Jc + 2, ipos, 0) = se2*RO(ier, Jc + 2, Nnuc)
            SCRt(ier, Jc - 1, ineg, 0) = se1*RO(ier, Jc - 1, Nnuc)
            SCRt(ier, Jc, ineg, 0) = se1*RO(ier, Jc, Nnuc)
            SCRt(ier, Jc + 1, ineg, 0) = se1*RO(ier, Jc + 1, Nnuc)
            GOTO 100
         ENDIF
C--------decaying state spin index = 2
         IF(Jc.EQ.2)THEN
            IF(ABS(xjc - 1.5).LT.0.001D0)THEN
               SCRt(ier, Jc - 1, ipos, 0) = se2m1*RO(ier, Jc - 1, Nnuc)
            ELSE
               SCRt(ier, Jc - 1, ipos, 0) = sm1*RO(ier, Jc - 1, Nnuc)
            ENDIF
            SCRt(ier, Jc, ipos, 0) = se2m1*RO(ier, Jc, Nnuc)
            SCRt(ier, Jc + 1, ipos, 0) = se2m1*RO(ier, Jc + 1, Nnuc)
            SCRt(ier, Jc + 2, ipos, 0) = se2*RO(ier, Jc + 2, Nnuc)
            SCRt(ier, Jc - 1, ineg, 0) = se1*RO(ier, Jc - 1, Nnuc)
            SCRt(ier, Jc, ineg, 0) = se1*RO(ier, Jc, Nnuc)
            SCRt(ier, Jc + 1, ineg, 0) = se1*RO(ier, Jc + 1, Nnuc)
            GOTO 100
         ENDIF
C--------decaying state spin = 1/2
         IF(ABS(xjc - 0.5).LT.0.001D0)THEN
            SCRt(ier, Jc, ipos, 0) = sm1*RO(ier, Jc, Nnuc)
            SCRt(ier, Jc + 1, ipos, 0) = se2m1*RO(ier, Jc + 1, Nnuc)
            SCRt(ier, Jc + 2, ipos, 0) = se2*RO(ier, Jc + 2, Nnuc)
            SCRt(ier, Jc, ineg, 0) = se1*RO(ier, Jc, Nnuc)
            SCRt(ier, Jc + 1, ineg, 0) = se1*RO(ier, Jc + 1, Nnuc)
            GOTO 100
         ENDIF
C--------decaying state spin = 0
         IF(ABS(xjc - 0.).LT.0.001D0)THEN
            SCRt(ier, Jc + 1, ipos, 0) = sm1*RO(ier, Jc + 1, Nnuc)
            SCRt(ier, Jc + 2, ipos, 0) = se2*RO(ier, Jc + 2, Nnuc)
            SCRt(ier, Jc + 1, ineg, 0) = se1*RO(ier, Jc + 1, Nnuc)
            GOTO 100
         ENDIF
C--------decaying state spin index = NLW-1
 50      IF(Jc.EQ.NLW - 1)THEN
            SCRt(ier, Jc - 2, ipos, 0) = se2*RO(ier, Jc - 2, Nnuc)
            SCRt(ier, Jc - 1, ipos, 0) = se2m1*RO(ier, Jc - 1, Nnuc)
            SCRt(ier, Jc, ipos, 0) = se2m1*RO(ier, Jc, Nnuc)
            SCRt(ier, Jc + 1, ipos, 0) = se2m1*RO(ier, Jc + 1, Nnuc)
            SCRt(ier, Jc - 1, ineg, 0) = se1*RO(ier, Jc - 1, Nnuc)
            SCRt(ier, Jc, ineg, 0) = se1*RO(ier, Jc, Nnuc)
            SCRt(ier, Jc + 1, ineg, 0) = se1*RO(ier, Jc + 1, Nnuc)
            GOTO 100
         ENDIF
C--------decaying state spin index = NLW
         IF(Jc.EQ.NLW)THEN
            SCRt(ier, Jc - 2, ipos, 0) = se2*RO(ier, Jc - 2, Nnuc)
            SCRt(ier, Jc - 1, ipos, 0) = se2m1*RO(ier, Jc - 1, Nnuc)
            SCRt(ier, Jc, ipos, 0) = se2m1*RO(ier, Jc, Nnuc)
            SCRt(ier, Jc - 1, ineg, 0) = se1*RO(ier, Jc - 1, Nnuc)
            SCRt(ier, Jc, ineg, 0) = se1*RO(ier, Jc, Nnuc)
         ENDIF
 100  ENDDO
C-----do loop over c.n. energies ***done***
C-----decay to the continuum ----** done***---------------------------
99001 FORMAT(1X, F5.2, 12G10.3)
C-----INTEGRATION OF RO*GTL IN CONTINUUM FOR EJECTILE 0 (trapezoid
      DO j = jmin, jmax
         DO i = 1, Iec - 1
            Sum = Sum + SCRt(i, j, 1, 0) + SCRt(i, j, 2, 0)
         ENDDO
         Sum = Sum - 0.5*(SCRt(1, j, 1, 0) + SCRt(1, j, 2, 0))
      ENDDO
      Sum = Sum*DE
C-----INTEGRATION OF RO*GTL IN CONTINUUM FOR EJECTILE 0 -- DONE ----
C-----
C-----DECAY TO DISCRETE LEVELS
C-----
      IF(RORed.NE.0.0D0)THEN
C-----do loop over discrete levels -----------------------------------
         DO i = 1, NLV(Nnuc)
            lmin = ABS(xjc - XJLv(i, Nnuc)) + 0.001
            lmax = xjc + XJLv(i, Nnuc) + 0.001
            IF(lmin.LE.2 .AND. lmax.NE.0)THEN
               eg = EX(Iec, Nnuc) - ELV(i, Nnuc)
               ipar = (1 + LVP(i, Nnuc)*Ipc)/2
               iodd = 1 - ipar
               IF(lmin.EQ.2)THEN
                  SCRtl(i, 0) = SCRtl(i, 0) + E2(eg)*FLOAT(ipar)
               ELSE
C-----------------Plujko_new
                  SCRtl(i, 0) = 
     &  E1(Nnuc,Z,A,eg, TNUc(1,Nnuc),Uexcit(1,Nnuc))*iodd + XM1(eg)*ipar
Cb                SCRtl(i, 0) = E1(eg, TNUc(1,Nnuc))*iodd + XM1(eg)*ipar
C-----------------Plujko_new(End)
                  IF(lmax.NE.1)SCRtl(i, 0) = SCRtl(i, 0) + E2(eg)
     &               *FLOAT(ipar)
               ENDIF
               SCRtl(i, 0) = SCRtl(i, 0)*RORed*TUNe(0, Nnuc)
               Sum = Sum + SCRtl(i, 0)
            ENDIF
         ENDDO
C-----do loop over discrete levels --------- done --------------------
      ENDIF
      SCRtem(0) = Sum
      DENhf = DENhf + Sum
C-----Plujko_new -under construction
      GO TO 255
      Gc = Sum / (2 *Pi * RO(Iec, Jc , Nnuc) )
C     RO(ier, Jc + 1, Nnuc)
      fEX = EX(Iec, Nnuc)
      fSpinIc = FLOAT(Jc) + HIS(Nnuc)
C
C
      iA = INT(A(nnuc))
      WRITE(54, 1112 ) iA, SYMb(Nnuc),Iec, fEX, fSpinIc,  Ipc , Gc
1112  Format(1X, I3, A2, 4X, I2,3X, F9.5 ,6X, F3.1, 4X, I2, 3X, E12.4)
255   CONTINUE
C-----Plujko_new(End)
      END
C
      SUBROUTINE DECAYT(Nnuc, Iec, Jc, Ipc, Sum)
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
Ccc   * author: M.Herman                                                 *
Ccc   * date:20 May. 1994                                                *
Ccc   * revision:#    by:name                     on:xx.mon.199x         *
Ccc   *                                                                  *
Ccc   ********************************************************************
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
      DOUBLE PRECISION eg, se1, se2, se2m1, sm1, sumn, sump, xjc
C     REAL FLOAT
      INTEGER i, ier, ineg, iodd, ipar, ipos, j, lmax, lmin
C
C
C
      Sum = 0.0
      SCRtem(0) = 0.0
      xjc = FLOAT(Jc) + HIS(Nnuc)
C-----clear scratch matrix (continuum)
      DO j = 1, NLW
         DO i = 1, NEX(Nnuc)
            SCRt(i, j, 1, 0) = 0.0
            SCRt(i, j, 2, 0) = 0.0
         ENDDO
      ENDDO
C-----clear scratch matrix (discrete levels)
      DO i = 1, NLV(Nnuc)
         SCRtl(i, 0) = 0.0
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
         IF(RO(ier, Jc, Nnuc).NE.0.D0)THEN
            eg = EX(Iec, Nnuc) - EX(ier, Nnuc)
C-----------next 3 lines should be replaced with the matrix
C-----------Plujko_new
            se1 = 
     &   E1(Nnuc,Z,A,eg, TNUc(ier, Nnuc),Uexcit(ier,Nnuc))*TUNe(0, Nnuc)
Cb          se1 = E1(eg, TNUc(ier, Nnuc))*TUNe(0, Nnuc)
C----Plujko_new(End)
            se2 = E2(eg)*TUNe(0, Nnuc)
            sm1 = XM1(eg)*TUNe(0, Nnuc)
            se2m1 = se2 + sm1
            IF(Jc.GT.2)THEN
               IF(Jc.GE.NLW - 1)GOTO 20
               sump = se2*RO(ier, Jc - 2, Nnuc)
               sump = sump + se2m1*RO(ier, Jc - 1, Nnuc)
               sump = sump + se2m1*RO(ier, Jc, Nnuc)
               sump = sump + se2m1*RO(ier, Jc + 1, Nnuc)
               sump = sump + se2*RO(ier, Jc + 2, Nnuc)
               SCRt(ier, Jc, ipos, 0) = sump
               sumn = se1*RO(ier, Jc - 1, Nnuc)
               sumn = sumn + se1*RO(ier, Jc, Nnuc)
               sumn = sumn + se1*RO(ier, Jc + 1, Nnuc)
               SCRt(ier, Jc, ineg, 0) = sumn
               GOTO 100
            ENDIF
C-----------decaying state spin index = 2
            IF(Jc.EQ.2)THEN
               IF(ABS(xjc - 1.5).LT.0.001D0)THEN
                  sump = se2m1*RO(ier, Jc - 1, Nnuc)
               ELSE
                  sump = sm1*RO(ier, Jc - 1, Nnuc)
               ENDIF
               sump = sump + se2m1*RO(ier, Jc, Nnuc)
               sump = sump + se2m1*RO(ier, Jc + 1, Nnuc)
               sump = sump + se2*RO(ier, Jc + 2, Nnuc)
               SCRt(ier, Jc, ipos, 0) = sump
               sumn = se1*RO(ier, Jc - 1, Nnuc)
               sumn = sumn + se1*RO(ier, Jc, Nnuc)
               sumn = sumn + se1*RO(ier, Jc + 1, Nnuc)
               SCRt(ier, Jc, ineg, 0) = sumn
               GOTO 100
            ENDIF
C-----------decaying state spin = 1/2
            IF(ABS(xjc - 0.5).LT.0.001D0)THEN
               sump = sm1*RO(ier, Jc, Nnuc)
               sump = sump + se2m1*RO(ier, Jc + 1, Nnuc)
               sump = sump + se2*RO(ier, Jc + 2, Nnuc)
               SCRt(ier, Jc, ipos, 0) = sump
               sumn = se1*RO(ier, Jc, Nnuc)
               sumn = sumn + se1*RO(ier, Jc + 1, Nnuc)
               SCRt(ier, Jc, ineg, 0) = sumn
               GOTO 100
            ENDIF
C-----------decaying state spin = 0
            IF(ABS(xjc - 0.).LT.0.001D0)THEN
               SCRt(ier, Jc, ipos, 0) = sm1*RO(ier, Jc + 1, Nnuc)
     &                                  + se2*RO(ier, Jc + 2, Nnuc)
               SCRt(ier, Jc, ineg, 0) = se1*RO(ier, Jc + 1, Nnuc)
               GOTO 100
            ENDIF
C-----------decaying state spin index = NLW-1
 20         IF(Jc.EQ.NLW - 1)THEN
               sump = se2*RO(ier, Jc - 2, Nnuc)
               sump = sump + se2m1*RO(ier, Jc - 1, Nnuc)
               sump = sump + se2m1*RO(ier, Jc, Nnuc)
               sump = sump + se2m1*RO(ier, Jc + 1, Nnuc)
               SCRt(ier, Jc, ipos, 0) = sump
               sumn = se1*RO(ier, Jc - 1, Nnuc)
               sumn = sumn + se1*RO(ier, Jc, Nnuc)
               sumn = sumn + se1*RO(ier, Jc + 1, Nnuc)
               SCRt(ier, Jc, ineg, 0) = sumn
               GOTO 100
            ENDIF
C-----------decaying state spin index = NLW
            IF(Jc.EQ.NLW)THEN
               sump = se2*RO(ier, Jc - 2, Nnuc)
               sump = sump + se2m1*RO(ier, Jc - 1, Nnuc)
               sump = sump + se2m1*RO(ier, Jc, Nnuc)
               SCRt(ier, Jc, ipos, 0) = sump
               SCRt(ier, Jc, ineg, 0) = se1*RO(ier, Jc, Nnuc)
     &                                  + se1*RO(ier, Jc - 1, Nnuc)
            ENDIF
         ENDIF
 100  ENDDO
C-----do loop over c.n. energies ***done***
C-----decay to the continuum ----** done***---------------------------
99001 FORMAT(1X, F5.2, 12G10.3)
C-----INTEGRATION OF RO*GTL IN CONTINUUM FOR EJECTILE 0 (trapezoid
      DO i = 1, Iec - 1
         Sum = Sum + SCRt(i, Jc, 1, 0) + SCRt(i, Jc, 2, 0)
      ENDDO
      Sum = Sum - 0.5*(SCRt(1, Jc, 1, 0) + SCRt(1, Jc, 2, 0))
      Sum = Sum*DE
C-----INTEGRATION OF RO*GTL IN CONTINUUM FOR EJECTILE 0 -- DONE ----
C-----
C-----DECAY TO DISCRETE LEVELS
C-----
      IF(RORed.NE.0.0D0)THEN
C--------do loop over discrete levels -----------------------------------
         DO i = 1, NLV(Nnuc)
            lmin = ABS(xjc - XJLv(i, Nnuc)) + 0.001
            lmax = xjc + XJLv(i, Nnuc) + 0.001
            IF(lmin.LE.2 .AND. lmax.NE.0)THEN
               eg = EX(Iec, Nnuc) - ELV(i, Nnuc)
               ipar = (1 + LVP(i, Nnuc)*Ipc)/2
               iodd = 1 - ipar
               IF(lmin.EQ.2)THEN
                  SCRtl(i, 0) = SCRtl(i, 0) + E2(eg)*FLOAT(ipar)
               ELSE
C-----------------Plujko_new
                  SCRtl(i, 0) = 
     &  E1(Nnuc,A,Z,eg, TNUc(1,Nnuc),Uexcit(1,Nnuc))*iodd + XM1(eg)*ipar
Cb                SCRtl(i, 0) = E1(eg, TNUc(1,Nnuc))*iodd + XM1(eg)*ipar
C-----------------Plujko_new(End)
                  IF(lmax.NE.1)SCRtl(i, 0) = SCRtl(i, 0) + E2(eg)
     &               *FLOAT(ipar)
               ENDIF
               SCRtl(i, 0) = SCRtl(i, 0)*RORed*TUNe(0, Nnuc)
               Sum = Sum + SCRtl(i, 0)
            ENDIF
         ENDDO
C-----do loop over discrete levels --------- done --------------------
      ENDIF
      SCRtem(0) = Sum
      DENhf = DENhf + Sum
      END
C
      SUBROUTINE FISSION(Nnuc, Iec, Jc, Sumfis)
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
Ccc   * author: M.Herman                                                 *
Ccc   * date:   31.Jan.1994                                              *
Ccc   * revision:#    by:name                     on:xx.mon.199x         *
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
     &                 fric, gamma, gpart, htom, PI, shredt, sum1, sum2, 
     &                 sum3, sumf, sumgs, sumr, tau, temp
      REAL FLOAT
      INTEGER kn, knm
      DOUBLE PRECISION TLF
C
C
C
      Sumfis = 0.0
      IF(EX(Iec, Nnuc).EQ.0.0D0)RETURN
C-----temperature fade-out of the shell correction
      temp = 0.
      atil = 0.073*A(Nnuc) + 0.115*A(Nnuc)**0.666667
      gamma = 0.40/A(Nnuc)**0.33333
      accn = atil*(1 + SHC(Nnuc)*(1 - EXP((-gamma*EX(Iec,Nnuc))))
     &       /EX(Iec, Nnuc))
      IF(EX(Iec, Nnuc).GE.YRAst(Jc, Nnuc))
     &   temp = SQRT((EX(Iec,Nnuc) - YRAst(Jc,Nnuc))/accn)
      ampl = EXP(TEMp0*SHRt)
      shredt = 1.
      IF(temp.GE.TEMp0)shredt = ampl*EXP(( - SHRt*temp))
C-----temperature fade-out of the shell correction *** done *****
      fisba = FISb(Jc, Nnuc) - SHC(Nnuc)*SHCjf(Jc, Nnuc)*shredt
      ekinm = EX(Iec, Nnuc) - fisba
C
      IF(ekinm.LT.0.0D0)RETURN
      knm = AINT(ekinm/DE + 1.001)
      erest = ekinm - (knm - 1)*DE
C-----IEC to g.s.
      sumgs = TLF(ekinm)
C-----IEC to IEC
      sum1 = TLF(0.0D0)*ROF(Iec, Jc, Nnuc)
      IF(knm.EQ.1)THEN
         Sumfis = 0.5*(sumgs + sum1)*erest
         GOTO 100
      ENDIF
C-----IEC to IEC-1
      sum2 = TLF(DE)*ROF(Iec - 1, Jc, Nnuc)
C-----IEC to g.s.+1
      sum3 = TLF(FLOAT(knm - 1)*DE)*ROF(Iec - knm + 1, Jc, Nnuc)
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
               ekin = EX(Iec, Nnuc) - EX(kn, Nnuc)
               sumf = TLF(ekin)*ROF(kn, Jc, Nnuc)
               sumr = sumr + sumf
            ENDDO
            Sumfis = Sumfis + sumr*DE
         ENDIF
      ENDIF
 100  IF(Sumfis.EQ.0.0D0)RETURN
      IF(BETav.NE.0.0D0)THEN
C--------reduction of the fission width due to possible return from the
C--------saddle point (nuclear viscosity 1-st effect)
         htom = 1.0
         Sumfis = Sumfis*(SQRT(1.0 + (BETav/2./htom)**2)
     &            - BETav/2./htom)
         IF(fisba - YRAst(Jc, Nnuc).GT.0.0D0)THEN
C-----------reduction of fission width due to the transient time needed
C-----------to form a saddle point (nuclear viscosity 2-nd effect)
C-----------according to Rastopchin et al. Sov. Jour. Nucl. Phys. 53,741(1991)
C-----------omega1=omega0=1.6*10^21 (1/s) hbar*omega=1MeV
C-----------BETAV critical =3.2; 0.19531 stands for 1/(2*omega1**2)
            gpart = DENhf/RO(Iec, Jc, Nnuc)/2./PI
C           GFIS = SUMFIS/RO(IEC,JC,NNUC)/2./PI
            tau = LOG(10.0*(fisba - YRAst(Jc,Nnuc))/temp)
            IF(BETav.LT.3.2D0)THEN
               tau = tau/BETav
            ELSE
               tau = tau*BETav*0.19531
            ENDIF
            fric = gpart*tau/0.6589
            fric = MIN(EXPmax, fric)
            IF(fric.GT.0D0)THEN
               fric = EXP(( - fric))
               Sumfis = Sumfis*fric
            ENDIF
         ENDIF
      ENDIF
      DENhf = DENhf + Sumfis
      END
C
      DOUBLE PRECISION FUNCTION TLF(Ekin)
C-----energy dependent transmission coefficient (note that htom is
C-----fixed below and does not depend on angular momentum as it might)
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C
C     Argument
C
      DOUBLE PRECISION Ekin
C
C     Local variables
C
      DOUBLE PRECISION atlf, htom, pix2
C
      DATA pix2/6.28318530717958647692528676655901D0/
      DATA htom/1.D0/
C
      TLF = 1.D0
      atlf = pix2*Ekin/htom
      IF(atlf.LT.38.D0)TLF = 1./(1. + EXP((-atlf)))
C
      END
C
      SUBROUTINE FISFIS(Nnuc, Iec, Ip, Jc, Sumfis, Cota)
Ccc   ********************************************************************
Ccc   *                                                         class:ppu*
Ccc   *                         F I S F I S                              *
Ccc   *                                                                  *
Ccc   *    Calculates fission of the nuclear state defined by NNUC,      *
Ccc   *    IEC, IP and JC including two viscosity effects.               *
Ccc   *    using double or triple humped barrier assumptions             *
Ccc   *                                                                  *
Ccc   * input:NNUC-decaying nucleus index                                *
Ccc   *       IEC -decaying state excitation energy index                *
Ccc   *       IP  -decaying state parity                                 *
Ccc   *       JC  -decaying state spin index                             *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * output:SUMFIS-integral of the Tf for discrete and continuum over *
Ccc   *            the kinetic energy of fission fragments (to be added  *
Ccc   *            to the Hauser-Feshbach denominator DENHF)             *
Ccc   *                                                                  *
Ccc   * calls:...                                                        *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * authors: M. Sin, R. Capote                                       *
Ccc   * date:   Aug.2002                                                 *
Ccc   * revision:#    by:name                     on:xx.mon.199x         *
Ccc   * 1.1              RCN                            Nov.2002         *
Ccc   * 1.2              MSin                           Nov.2003         *
Ccc   *                                                                  *
Ccc   ********************************************************************
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C     LOCAL VARIABLES
      DOUBLE PRECISION ATIl
      DOUBLE PRECISION ee, Sumfis, Cota
C
      DIMENSION tfdis(NFWELLS), tfcon(NFWELLS)
      COMMON /CRIT  / TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl
      COMMON /PARFIS/   IBAr
      COMMON /COMFIS3/ VBArex(NFWELLS), TFD(NFWELLS)
      COMMON /COMFIS4/ TFC, TFCc, JCC
      COMMON /CMIU  / SMIu, PHAsr(NFWELLS)
      COMMON /IMAG  / TF(NFWELLS), TDIr, TABs, TDIr23

      DOUBLE PRECISION PHAsr, delt, tdir1, tdirr, TDIr, tabss, 
     &                 TABs, TDIr23, tdirr23, tdir231, TF, atal, btal, 
     &                 ctal, dtal, etal, aral, bral, cral, dral, eral,
     &                 wimagg,eeiso,tfcc,tfc,tfcon,tdir23cont

      INTEGER JC,JCC

C
C----------------------------------------
      ee = EX(Iec, Nnuc)
      SUMfis = 0.0
      IF(ee.EQ.0.0D0)RETURN
C
C     Below is an square root of (MIU divided by 2)
      SMIu = 0.1643167*A(Nnuc)**(5./6.)
C
      JCC = Jc
C
      IF(Jc.EQ.1 .AND. Ip.EQ.1)THEN
         WRITE(80, *)'  '
         WRITE(80, '(1x,a19,f9.5,a4)')'Excitation energy =', EE, ' MeV'
         WRITE(80, *)' '
C
         IF(NRBar.EQ.1)WRITE(80, '(17x,3(a2,9x))')'Td', 'Tc', 'Tf'
         IF(NRBar.EQ.2)WRITE(80, '(17x,5(a3,9x))')'TAd', 'TBd', 'TAc', 
     &                       'TBc', 'Tf'
C        Double humped case 
         IF(NRBar.EQ.3.AND.NRWel.EQ.1)WRITE(80, '(17x,7(a4,7x))')'TAd', 
     &                       'TBd', 'TAc', 'TBc', 'Tf', 'Tdir', 'Tabs'
         IF(NRBar.EQ.3.AND.NRWel.EQ.0)WRITE(80, '(17x,7(a4,7x))')'TAd', 
     &                       'TBd', 'TCd', 'TAc', 'TBc', 'TCc', 'Tf'
C        Triple humped case 
         IF(NRBar.EQ.5)WRITE(80, '(16x,9(a4,7x),a6)')'TAd', 'TBd', 
     &                       'TCd', 'TAc', 'TBc', 'TCc', 'Tf', 'Tdir', 
     &                       'Tabs', 'Tdir23'
      ENDIF
C
C     NRBarc = NRBar - NRWel

      DO IBAr = 1, NRBarc
         tfdis(IBAr) = 0.
         tfcon(IBAr) = 0.
      ENDDO
C==============discrete contribution====================
      IF(SUBeff(Nnuc).EQ.0.)THEN
         DO IBAr = 1, NRBarc
            DO nr = 1, NRFdis(IBAr)
               sfmin = SFDis(nr, IBAr)
               ist = 1
               IF(SFDis(nr, IBAr).EQ.0.0 .AND. IPFdis(nr, IBAr).EQ.1)
     &            THEN
                  sfmin = 0.
                  ist = 2
               ENDIF
               IF(SFDis(nr, IBAr).EQ.0.0 .AND. IPFdis(nr, IBAr).EQ. - 1)
     &            THEN
                  sfmin = 1.
                  ist = 2
               ENDIF
               sfmin = sfmin - HIS(Nnuc)
               DO jnc = sfmin, Jc, ist
                  IF(jnc.EQ.Jc .AND. IPFdis(nr, IBAr).EQ.Ip)THEN
                     snc = FLOAT(jnc) + HIS(Nnuc)
                     exfis = EFDis(nr, IBAr) + HJ(nnuc,IBAr)
     &                       *(snc*(snc + 1) - SFDis(nr, IBAr)
     &                       *(SFDis(nr,IBAr) + 1))
                     VBArex(IBAr) = EFB(IBAr) + exfis
                     arg = 2*PI*(VBArex(IBAr) - ee)/H(IBAr)
                     IF(arg.LE.EXPmax)THEN
                        TFD(IBAr) = 1./(1. + EXP(arg))
                     ELSE
                        TFD(IBAr) = 0.0
                     ENDIF
                     tfdis(IBAr) = tfdis(IBAr) + TFD(IBAr)
                  ENDIF
               ENDDO
            ENDDO
         ENDDO
      ENDIF
C
      IF(SUBeff(Nnuc).EQ.1.)THEN
         TDIr = 0.
         TABs = 0.
         TDIr23 = 0.
         TDIr23cont=0.

         IF(NRBar.eq.3.and.nrwel.eq.1)eeiso=ee-efb(3)
         IF(NRBar.eq.5.and.nrwel.eq.2)eeiso=ee-efb(4)
         wimagg=wimag(1)+wimag(2)*eeiso+wimag(3)*eeiso**2
C
         DO nr = 1, NRFdis(1)
            sfmin = SFDis(nr, 1)
            ist = 1
            IF(SFDis(nr, 1).EQ.0.0 .AND. IPFdis(nr, 1).EQ.1)THEN
               sfmin = 0.
               ist = 2
            ENDIF
            IF(SFDis(nr, 1).EQ.0.0 .AND. IPFdis(nr, 1).EQ. - 1)THEN
               sfmin = 1.
               ist = 2
            ENDIF
C
            sfmin = sfmin - HIS(Nnuc)
            DO jnc = sfmin, Jc, ist
C
               IF(jnc.EQ.Jc .AND. IPFdis(nr, 1).EQ.Ip)THEN
                  snc = FLOAT(jnc) + HIS(Nnuc)
                  DO IBAr = 1, NRBar
                     exfis = EFDis(nr, IBAr) + HJ(nnuc,IBAr)
     &                       *(snc*(snc + 1) - SFDis(nr, IBAr)
     &                       *(SFDis(nr,IBAr) + 1))
                     if(exfis.lt.0.)exfis=0.
                     VBArex(IBAr) = EFB(IBAr) + exfis
                  ENDDO
C
                  CALL WKBFISNUM(ee)

                  DO IBAr = 1, NRBarc                    
                     IF(PHAsr(IBAr).LE.0.)THEN  
                        arg = 2*PI*(VBArex(IBAr) - ee)/H(IBAr)
                     ELSE
                        arg = PHAsr(IBAr)
                     ENDIF 
                     IF(arg.GT.EXPmax)arg=expmax
                     TFD(IBAr) = 1./(1. + EXP(arg))
                  ENDDO
C                 
                  IF(NRBar.EQ.3.and.NRWel.eq.1.)THEN
C                    Penetrability calculations for double-humped case
                     delt = WIMagg*PHAsr(3)
                     IF(delt.GT.EXPmax)delt=EXPmax
                     IF(ee.LT.vbarex(1).AND.ee.LT.vbarex(2))THEN
                        tdir1 = (1. - TFD(1))*(1. - TFD(2))
                        tdirr = TFD(1)*TFD(2)
     &                          /(EXP(delt) + 2*SQRT(ABS(tdir1))
     &                          *COS(PHAsr(3)) + tdir1*EXP( - delt))
                        IF(TFD(2).GT.0.)THEN
                           tabss = tdirr*(EXP(delt)/TFD(2)
     &                             - (1 - TFD(2))*EXP( - delt)/TFD(2)
     &                             - 1)
                        ELSE
                           tabss = 0.
                        ENDIF
                     ELSE
                        tdirr = 0.
                        tabss = TFD(1)
                     ENDIF
                  ENDIF   
  
                  IF(NRBar.EQ.5.and.NRWel.eq.2.)THEN
C                    Penetrability calculations for triple-humped case 
c                    (for the particular barrier of Th)
                     IF(ee.le.vbarex(4).and.ee.le.vbarex(5))THEN
                        tfd(1)=0.
                        tfd(2)=0.
                        tfd(3)=0.
                        tdirr=0.
                        tabss=0.
                        tdir23=0.
                        goto 7654 
c--------- to be replaced by numerical calculation
                     ENDIF

                     IF(ee.gt.vbarex(2))then
                        teq=(1.+exp(2*pi*(vbarex(2)-ee)/h(2)))*
     &                      (1.+exp(2*pi*(vbarex(3)-ee)/h(3)))
                        heq= 2*pi*(vbarex(3)-ee)/log(teq-1.) 
                        tdirr23=1./ (1.+exp(2*pi*(vbarex(3)-ee)/heq))
                     ENDIF   
                     IF(ee.lt.vbarex(2).and.ee.lt.vbarex(3))THEN
                        IF(ee.le.vbarex(5))then
                           tdirr23=tfd(2)*tfd(3) !!to be replaced by num.calc.  
                        ELSE
                           tdir231 = (1. - TFD(2))*(1. - TFD(3))
                           tdirr23 = TFD(2)*TFD(3)
     &                    /(1. + 2*SQRT(ABS(tdir231))*COS(PHAsr(5))
     &                         + tdir231)
                        ENDIF
                     ENDIF

                     IF(ee.lt.vbarex(1).and.ee.lt.vbarex(2)
     &                  .AND.ee.GT.vbarex(5))THEN 
                        delt = WIMagg*PHAsr(4)
                        atal = EXP( - delt)*(1. - TFD(1))*(1. - TFD(2))
     &                         + EXP(delt)*(1. - TFD(2))*(1. - TFD(3))
     &                        + EXP( - delt)*(1. - TFD(1))*(1. - TFD(3))
     &                         + EXP(delt)
                        btal = 2*((1. - TFD(1))**0.5)*(1. - TFD(2))
     &                         *(1. - TFD(3))**0.5
                        ctal = 2*((1. - TFD(1))**0.5)*(1. - TFD(3))**0.5
                        dtal = 2*((1. - TFD(1))**0.5)*((1. - TFD(2))
     &                         **0.5)*(2. - TFD(3))
                        etal = 2*((1. - TFD(2))**0.5)*((1. - TFD(3))
     &                         **0.5)*(EXP( - delt)*(1. - TFD(1)) + 
     &                         EXP(delt))

                        tdirr = TFD(1)*TFD(2)*TFD(3)
     &                         /(atal + btal*COS(PHAsr(4) - PHAsr(5))
     &                       + ctal*COS(PHAsr(4) + PHAsr(5))
     &                       + dtal*COS(PHAsr(4)) + etal*COS(PHAsr(5)))


                        aral = EXP( - delt)*(2. - TFD(2) - TFD(3))
     &                       + EXP(delt)*(1. - TFD(1))
     &                       *((1. - TFD(2))*(1. - TFD(3)) + 1.)
                        bral = btal
                        cral = ctal
                        dral = dtal
                        eral = 2*((1. - TFD(2))**0.5)*((1. - TFD(3))
     &                         **0.5) *(EXP(delt)*(1. - TFD(1)) + 
     &                         EXP( - delt))

                        tabss = 1. - (tdirr/(TFD(1)*TFD(2)*TFD(3)))
     &                         *(TFD(1)*TFD(2)*TFD(3)
     &                         + aral + bral*COS((PHAsr(4)-PHAsr(5)))
     &                         + cral*COS((PHAsr(4)+PHAsr(5)))
     &                         + dral*COS(PHAsr(4))
     &                         + eral*COS(PHAsr(5)))  
                     ELSE
                        tdirr=0.
                        tabss=tfd(1)
                     ENDIF
                  ENDIF
 7653            if(tabss+tdirr.GT.1.)tdirr=0.   
                 TDIr = TDIr + tdirr
                 TABs = TABs + tabss
                 TDIr23 = TDIr23 + tdirr23
C
 7654             DO IBAr = 1, NRBarc
                     tfdis(IBAr) = tfdis(IBAr) + TFD(IBAr)
                  ENDDO
              ENDIF
            ENDDO
         ENDDO
      ENDIF
C
C==============continuum contribution====================
      AJ = FLOAT(Jc) + HIS(Nnuc)
      DO IBAr = 1, NRBarc
         arg = 2*PI*(EFB(IBAr) - ee)/H(IBAr)
         IF(arg.LE.EXPmax)THEN
            TFC = EXP(arg)
         ELSE
            TFC = EXP(EXPmax)
         ENDIF

C        SIMPSFIS remains just for testing purposes, is not used anymore
C        GAUSSFIS is more efficient
C        However, as for the moment it does not work, SIMPSFIS is used
         CALL SIMPSFIS(NNUc, IBAr, ee)
C        CALL GAUSSFIS(NNUc, IBAr)
         tfcon(IBAr) = TFCc
         TF(IBAr) = tfdis(IBAr) + tfcon(IBAr) 
      ENDDO
      TABs = TABs + tfcon(1)

      IF(NRBarc.eq.3.)THEN
C        Penetrability calculations for triple-humped case 
         CALL SIMPSFIS(Nnuc,7,ee)
         IBAr = NRBarc
C        CALL GAUSSFIS(NNUc, IBAr)
         Tdir23cont=TFCc
         Tdir23=Tdir23+Tdir23cont   !tfcon(2)*tfcon(3)
      ENDIF   

C
C     CALCULATING FISSION CONTRIBUTION TO THE HAUSER-FESHBACH denominator
      IF(NRBar.EQ.1)THEN
         Sumfis = TF(1)
         WRITE(80, '(1x,a2,f4.1,1x,a3,I2,3g11.4)')'J=', aj, 'Pi=', Ip, 
     &         tfdis(1), tfcon(1), SUMfis
      ENDIF
 
      IF(NRBarc.EQ.2.)THEN
         tnumm = TF(1) + TF(2)
         IF(tnumm.EQ.0.0)THEN
            Sumfis = 0.
         ELSE
            Sumfis = TF(1)*TF(2)/(TF(1) + TF(2))
            IF(SUBeff(Nnuc).EQ.0.)
     &         WRITE(80, '(1x,a2,f4.1,1x,a3,I2,7g11.4)')'J=', 
     &                    aj, 'Pi=', Ip, tfdis(1), tfdis(2), 
     &                    tfcon(1), tfcon(2), SUMfis
            IF(NRWel.eq.1.and.SUBeff(Nnuc).eq.1.)THEN
               cota1 = (TF(1) + TF(2))/2
               IF(cota1.LE.EXPmax)THEN
                 cotaexp = EXP( - cota1)
               ELSE
                 cotaexp = 0.d0
               ENDIF
               Cota = (1 + cotaexp**2)/(1 - cotaexp**2 + 0.00000001)
               WRITE(80, '(1x,a2,f4.1,1x,a3,I2,7g11.4)')'J=', 
     &                    aj, 'Pi=', Ip, tfdis(1), tfdis(2), 
     &                    tfcon(1), tfcon(2), SUMfis, TDIr, TABs
            ENDIF   
         ENDIF

        
      ENDIF
C
      IF(NRBarc.EQ.3)THEN
         tnumm = TF(1) + TF(2) + TF(3)
         IF(tnumm.eq.0.)THEN
            Sumfis=0.
         ELSE
            Sumfis = TF(1)*Tdir23 /(tf(1)+tdir23)
            IF(NRWel.eq.2.and.SUBeff(Nnuc).eq.1.)THEN
               cota1 = (TF(1) + TDIr23)/2
               IF(cota1.LE.EXPmax)THEN
                 cotaexp = EXP( - cota1)
               ELSE
                 cotaexp = 0.d0
               ENDIF
               Cota = (1 + cotaexp**2)/(1 - cotaexp**2 + 0.00000001)
            ENDIF
         ENDIF
         WRITE(80, '(1x,a2,f4.1,1x,a3,I2,10g11.4)')'J=', aj, 'Pi=', Ip, 
     &         tfdis(1), tfdis(2), tfdis(3), tfcon(1), tfcon(2), 
     &         tfcon(3), SUMfis, TDIr, TABs, TDIr23
      ENDIF

      DENhf = DENhf + Sumfis

      END
C
C===================================================================
      SUBROUTINE WKBFISNUM(Ee)
C     Calculates Momentum integrals by Gauss - Legendre method
C-------------------------------------------------------------------
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
      DIMENSION EPSil(NFPARAB), EJOin(2*NFPARAB), VJJ(NFPARAB)
      DIMENSION ho(nfwells)
      DIMENSION einters(2*NFPARAB), dmominteg(NFPARAB)
C
      COMMON /COMFIS3/ VBArex(NFPARAB), TFD(NFPARAB)
      COMMON /COMFIS5/ EPSil, EJOin, VJJ
      COMMON /ExcEnergy/ UEXcitt
      COMMON /CMIU  / SMIu, PHAsr(NFPARAB)
C
      DOUBLE PRECISION Ee, H, TFD, VBArex, SMIu, PHAsr,ho
C
      EXTERNAL FMOMENT
C*******************************************************
      IF(NRBar.EQ.3.and.NRWel.eq.1)THEN
         VJJ(1) = VBArex(1)
         VJJ(2) = VBArex(3)
         VJJ(3) = VBArex(2)
         ho(1)=h(1)
         ho(2)=h(3)
         ho(3)=h(2)
      ENDIF
      IF(NRBar.EQ.5)THEN
         VJJ(1) = VBArex(1)
         VJJ(2) = VBArex(4)
         VJJ(3) = VBArex(2)
         VJJ(4) = VBArex(5)
         VJJ(5) = VBArex(3)
         ho(1)=h(1)
         ho(2)=h(4)
         ho(3)=h(2)
         ho(4)=h(5)
         ho(5)=h(3)
      ENDIF
C-----deformations at saddles and wells and matching points--------------------
C     Fission barriers are modelled by NRBar parabols
C     EPSil(i) are the parabols vortex
C     EJOin(i) are the corresponding deformation at which parabols join
      EPSil(1) = SQRT(VJJ(1))/(SMIu*HO(1))
      EJOin(2) = EPSil(1)
     &           + SQRT((VJJ(1) - VJJ(2))/(1.D0 + (HO(1)/HO(2))**2))
     &           /(SMIu*HO(1))
      EJOin(1) = 2*EPSil(1) - EJOin(2)
C
      DO k = 2, NRBar
         EJOin(2*k - 1) = EJOin(2*(k - 1))
         EPSil(k) = EJOin(2*(k - 1)) + (HO(k - 1)/HO(k))
     &              **2*(EJOin(2*(k-1)) - EPSil(k - 1))
C
         IF(k.LT.NRBar)EJOin(2*k) = EPSil(k)
     &                              + SQRT(( - 1)**k*(VJJ(k+1) - VJJ(k))
     &                              /(1.D0 + (HO(k)/HO(k+1))**2))
     &                              /(SMIu*HO(k))
      ENDDO

      EJOin(2*NRBar) = 2*EPSil(NRBar) - EJOin(2*NRBar - 1)
C-----Einters(i) are the deformations corresponding to the intersection points
C     between parabols and the straigth line parallel to the deformation
C     axis corresponding to the excitation energy
      DO j = 1, 2*NRBar
         einters(j) = -1.
      ENDDO
      DO j = 1, NRBar
         ftmp = ( - 1)**j*(Ee - VJJ(j))
         IF(ftmp.GE.0.D0)THEN
            IF(Ee.EQ.VJJ(j))THEN
               einters(2*j - 1) = Ee
               einters(2*j) = Ee
               GOTO 100
            ENDIF
            es = SQRT(ftmp)/(SMIu*HO(j))
C           LEFT intersect
            einters(2*j - 1) = EPSil(j) - es
C           RIGTH intersect
            einters(2*j) = EPSil(j) + es
         ENDIF
 100  ENDDO
C-------------------------------------------------------------------
C     Difficult logical block below to discriminate between intersection
C     points corresponding to different parabols
      DO j = 1, 2*NRBar, 2
         IF(einters(j).GE.0.)THEN
C           LEFT intersect
            IF(j.GT.1 .AND. einters(j).LT.EJOin(j))einters(j) = -1.
C           RIGTH intersect
            IF(j + 1.LT.2*NRBar)THEN
               IF(einters(j + 1).GT.EJOin(j + 1))einters(j + 1) = -1.
            ENDIF
         ENDIF
      ENDDO
C     Below is tested only for wells
      DO j = 3, 2*NRBar - 2, 4
C        LEFT intersect
         IF(einters(j).LT.0.)einters(j) = einters(j - 1)
C        RIGTH intersect
         IF(einters(j + 1).LT.0.)einters(j + 1) = einters(j + 2)
      ENDDO
C     Below is tested only for hills
      DO j = 1, 2*NRBar, 4
C        LEFT intersect
         IF(j.GT.1 .AND. einters(j).LT.0.)einters(j) = einters(j - 1)
C        RIGTH intersect
         IF(j + 1.LT.2*NRBar .AND. einters(j + 1).LT.0.)einters(j + 1)
     &      = einters(j + 2)
      ENDDO
C---------------------------------
C
C     Momentum integrals calculated by Gauss-Legendre integration
      UEXcitt = Ee
      DO k = 1, NRBar
         dmominteg(k) = 0.D0
         IF(einters(2*k).GE.0. .AND. einters(2*k - 1).GE.0.)dmominteg(k)
     &      = GAUSS_INT(FMOMENT, einters(2*k - 1), einters(2*k), abserr)
      ENDDO
C
C     OPEN(157,FILE='PhaseIntegrals.txt')
C     DO K=1,NRBar
C     if(Einters(2*K).LT.0. .or. Einters(2*K-1).LT.0.) cycle
C        write(157,*) 'From Eps =',sngl(Einters(2*K-1)),
C     &           ' to ',sngl(Einters(2*K))
C     write(157,*) 'Mom.Integral (',K,')=',sngl(dMomInteg(K)),
C     &     ' Err=', sngl(ABSERR)
C     ENDDO
Ci    close(157)
C
C     Graphical test
C
C     IF(Ee.LT.5. .and. Ee.gt.4.6) THEN
C     OPEN(157,FILE='Vdef.dat')
C     ftmp=0.1
C     I=1
C     DO WHILE(ftmp.GT.0.)
C       EPS=I*0.001
C       ftmp=Vdef(EPS)
C       WRITE(157,*) EPS,ftmp
C       I=I+1
C    ENDDO
C    CLOSE(157)
C    OPEN(157,FILE='Vinters.dat')
C    DO  K=1,2*NRBar
C       IF(Einters(K).LT.0) CYCLE
C       WRITE(157,*) Einters(K), Vdef(Einters(K))
C    ENDDO
C    CLOSE(157)
C    OPEN(157,FILE='Vjoin.dat')
C    DO  K=1,NRBar
C       WRITE(157,*) Ejoin(2*K-1), Vdef(Ejoin(2*K-1))
C       WRITE(157,*) Ejoin(2*K), Vdef(Ejoin(2*K))
C    ENDDO
C    CLOSE(157)
C    ENDIF
C    STOP
C
C------------phases
      DO i = 1, NRBar
         phas = 2.*dMomInteg(i)
C-----------super-barrier, Hill-Wheeler
         IF(phas.LE.0.)phas = 2.d0*PI*(VBArex(i) - Ee)/HO(i)
         IF(phas.LE.EXPmax)THEN
            TFD(i) = 1./(1. + EXP(phas))
         ELSE
            TFD(i) = 0.0
         ENDIF
      ENDDO
C
C     fis1
C     Phases output
C     If phases < 0 then we are above the barrier, so we can use
C     Hill-Wheeler formulae for phase calculation :
C     phas = 2.d0*PI*(VBArex(i) - Ee)/HO(i)
      IF(NRBar.EQ.3)THEN
         PHAsr(1) = 2.*dmominteg(1)
         PHAsr(2) = 2.*dmominteg(3)
         PHAsr(3) = 2.*dmominteg(2)
      ENDIF
      IF(NRBar.EQ.5)THEN
         PHAsr(1) = 2.*dmominteg(1)
         PHAsr(2) = 2.*dmominteg(3)
         PHAsr(3) = 2.*dmominteg(5)
         PHAsr(4) = 2.*dmominteg(2)
         PHAsr(5) = 2.*dmominteg(4)
      ENDIF
      END
C
C
C-----------------------------------------------------------
      SUBROUTINE GAUSSFIS(Nnuc, Ibar)
C-----------------------------------------------------------
C     Gauss-Legendre integration of the fission level densities
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
      COMMON /COMFIS4/ TFC, TFCc, jcc
      COMMON /GDENSIT/ NNNuc, IIBar
C
C     INTEGER JCC
      DOUBLE PRECISION xmax, xmin
C     DOUBLE PRECISION AJ, TFC, TFCc, XMInn
      DOUBLE PRECISION TFC, TFCc, XMInn
      DOUBLE PRECISION FDENSITY, abserr
      EXTERNAL FDENSITY
C
C     Passing parameters to the integrand function Fdensity
      IIBar = Ibar
      NNNuc = Nnuc

      TFCc = 0.D0
C
      XMIn = XMInn(Ibar)
      XMAx = XMInn(Ibar) + (nrbinfis(ibar) - 1)*destepp

      TFCc = GAUSS_INT(FDENSITY, xmin, xmax, abserr)
C
C     If Relative Error for Gaussian Integration > 1 %
C
C     Try to find a true upper limit <xabs> which is always below Ee+4
C     The condition used is FDENSITY(xabs)>0.00001
C     We are using 0.5 MeV step because it is enough to get a good accuracy
C
      IF(abserr*100.GT.TFCc)THEN
         xabs = xmax
 50      fff = FDENSITY(xabs)
         IF(fff.LE.0.00001D0)THEN
            xabs = xabs - 0.5
            IF(xabs.LT.(xmin + 0.25))THEN
               xabs = xmin + 0.25
               GOTO 100
            ENDIF
            GOTO 50
         ENDIF
 100     TFCc = 0.D0
         TFCc = GAUSS_INT(FDENSITY, xmin, xabs, abserr)
      ENDIF
C     IF(TFCc.GT.0.d0)
C     > write(*,*) 'Gauss   = ',TFCc,' Err=',ABSERR/TFCc*100
C
      END
C
      REAL*8 FUNCTION FDENSITY(UXX)
C     Penetrability in the continuum.
C     Level densities at the saddle points are used
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C

      COMMON /COMFIS4/  TFC, TFCc,JCC
      COMMON /GDENSIT/ NNUc, IBAr
      COMMON /ROFI1/ enh_ld(3, NFHump)
C
      DATA pix2/6.28318530717958647692528676655901D0/
C-----------------------------------------------
      FDENSITY = 0.D0

      enh1=enh_ld(1,ibar)+(enh_ld(2,ibar)+enh_ld(3,ibar)*UXX)*UXX
      arg = pix2*UXX/H(IBAr)
      IF(arg.GT.EXPmax)arg=expmax

C     FISINT  function takes care of the interpolation from the  LD tables
      FDENSITY =enh1* FISINT(IBAr, Uxx,jcc,nnuc)/(1. + TFC*EXP(arg)) 

      RETURN
      END
C
      REAL*8 FUNCTION FMOMENT(Eps)
      INCLUDE 'dimension.h'
      IMPLICIT REAL*8(A-H,O-Z)
c     INCLUDE 'global.h'
C
      DIMENSION EPSil(NFWELLS), EJOin(2*NFWELLS), VJJ(NFWELLS)
      COMMON /COMFIS5/ EPSil, EJOin, VJJ
      COMMON /CMIU  / SMIu, PHAsr(NFWELLS)
      COMMON /ExcEnergy/ UEXcit
C
      FMOMENT = 2*SMIu*DSQRT(DABS(UEXcit - VDEF(Eps)))
C
      END
C
      REAL*8 FUNCTION VDEF(Eps)
C     calculation of the deformation potential energy
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
      DIMENSION EPSil(NFWELLS), EJOin(2*NFWELLS), VJJ(NFWELLS)
      COMMON /COMFIS5/ EPSil, EJOin, VJJ
      COMMON /CMIU  / SMIu, PHAsr(NFWELLS)
C
C-----------------------------------------------
       
      VDEF = 0.D0
C
      IF(Eps.LE.EJOin(2))THEN
         VDEF = VJJ(1) - (SMIu*H(1)*(Eps - EPSil(1)))**2
         RETURN
      ENDIF
C
      IF(Eps.GE.EJOin(2*NRBar - 1))THEN
         VDEF = VJJ(NRBar) - (SMIu*H(NRBar)*(Eps - EPSil(NRBar)))**2
         RETURN
      ENDIF
C
      DO j = 2, NRBar - 1
         IF(Eps.GE.EJOin(2*j - 1) .AND. Eps.LE.EJOin(2*j))THEN
            VDEF = VJJ(j) + ( - 1)**j*(SMIu*H(j)*(Eps - EPSil(j)))**2
            RETURN
         ENDIF
      ENDDO
C
      END
C
C
      REAL*8 FUNCTION GAUSS_INT(F, Ea, Eb, Abserr)
      IMPLICIT REAL*8(A - H, O - Z)
      REAL*8 Eb, Ea
      REAL*8 F, wg(10), xgk(21), wgk(21)
      REAL*8 centr1, hlgth1, resg1
      INTEGER j
      EXTERNAL F
      SAVE wg, xgk, wgk
C
C     THE ABSCISSAE AND WEIGHTS ARE GIVEN FOR THE INTERVAL (-1,1).
C     BECAUSE OF SYMMETRY ONLY THE POSITIVE ABSCISSAE AND THEIR
C     CORRESPONDING WEIGHTS ARE GIVEN.
C
C     XG - ABSCISSAE OF THE 41-POINT GAUSS-KRONROD RULE
C     WG - WEIGHTS OF THE 20-POINT GAUSS RULE
C
C GAUSS QUADRATURE WEIGHTS AND KRONROD QUADRATURE ABSCISSAE AND WEIGHTS
C AS EVALUATED WITH 80 DECIMAL DIGIT ARITHMETIC BY L. W. FULLERTON,
C BELL LABS, NOV. 1981.
C
      DATA wg(1)/0.017614007139152118311861962351853D0/
      DATA wg(2)/0.040601429800386941331039952274932D0/
      DATA wg(3)/0.062672048334109063569506535187042D0/
      DATA wg(4)/0.083276741576704748724758143222046D0/
      DATA wg(5)/0.101930119817240435036750135480350D0/
      DATA wg(6)/0.118194531961518417312377377711382D0/
      DATA wg(7)/0.131688638449176626898494499748163D0/
      DATA wg(8)/0.142096109318382051329298325067165D0/
      DATA wg(9)/0.149172986472603746787828737001969D0/
      DATA wg(10)/0.152753387130725850698084331955098D0/
C
      DATA xgk(1)/0.998859031588277663838315576545863D0/
      DATA xgk(2)/0.993128599185094924786122388471320D0/
      DATA xgk(3)/0.981507877450250259193342994720217D0/
      DATA xgk(4)/0.963971927277913791267666131197277D0/
      DATA xgk(5)/0.940822633831754753519982722212443D0/
      DATA xgk(6)/0.912234428251325905867752441203298D0/
      DATA xgk(7)/0.878276811252281976077442995113078D0/
      DATA xgk(8)/0.839116971822218823394529061701521D0/
      DATA xgk(9)/0.795041428837551198350638833272788D0/
      DATA xgk(10)/0.746331906460150792614305070355642D0/
      DATA xgk(11)/0.693237656334751384805490711845932D0/
      DATA xgk(12)/0.636053680726515025452836696226286D0/
      DATA xgk(13)/0.575140446819710315342946036586425D0/
      DATA xgk(14)/0.510867001950827098004364050955251D0/
      DATA xgk(15)/0.443593175238725103199992213492640D0/
      DATA xgk(16)/0.373706088715419560672548177024927D0/
      DATA xgk(17)/0.301627868114913004320555356858592D0/
      DATA xgk(18)/0.227785851141645078080496195368575D0/
      DATA xgk(19)/0.152605465240922675505220241022678D0/
      DATA xgk(20)/0.076526521133497333754640409398838D0/
      DATA xgk(21)/0.000000000000000000000000000000000D0/
C
      DATA wgk(1)/0.003073583718520531501218293246031D0/
      DATA wgk(2)/0.008600269855642942198661787950102D0/
      DATA wgk(3)/0.014626169256971252983787960308868D0/
      DATA wgk(4)/0.020388373461266523598010231432755D0/
      DATA wgk(5)/0.025882133604951158834505067096153D0/
      DATA wgk(6)/0.031287306777032798958543119323801D0/
      DATA wgk(7)/0.036600169758200798030557240707211D0/
      DATA wgk(8)/0.041668873327973686263788305936895D0/
      DATA wgk(9)/0.046434821867497674720231880926108D0/
      DATA wgk(10)/0.050944573923728691932707670050345D0/
      DATA wgk(11)/0.055195105348285994744832372419777D0/
      DATA wgk(12)/0.059111400880639572374967220648594D0/
      DATA wgk(13)/0.062653237554781168025870122174255D0/
      DATA wgk(14)/0.065834597133618422111563556969398D0/
      DATA wgk(15)/0.068648672928521619345623411885368D0/
      DATA wgk(16)/0.071054423553444068305790361723210D0/
      DATA wgk(17)/0.073030690332786667495189417658913D0/
      DATA wgk(18)/0.074582875400499188986581418362488D0/
      DATA wgk(19)/0.075704497684556674659542775376617D0/
      DATA wgk(20)/0.076377867672080736705502835038061D0/
      DATA wgk(21)/0.076600711917999656445049901530102D0/
C
C
C     Integrating from Ea to Eint
      centr1 = 0.5D+00*(Ea + Eb)
      hlgth1 = 0.5D+00*(Eb - Ea)
C
C     COMPUTE THE 41-POINT GAUSS-KRONROD APPROXIMATION TO
C     THE INTEGRAL, AND ESTIMATE THE ABSOLUTE ERROR.
C
      resg1 = 0.0D+00
      resk1 = wgk(21)*F(centr1)
      DO j = 1, 10
         jtw = j*2
         jtwm1 = jtw - 1
         absc = hlgth1*xgk(jtw)
         fval1 = F(centr1 - absc)
         fval2 = F(centr1 + absc)
         fsum = fval1 + fval2
         abscm1 = hlgth1*xgk(jtwm1)
         fval1m1 = F(centr1 - abscm1)
         fval2m1 = F(centr1 + abscm1)
         resg1 = resg1 + wg(j)*fsum
         resk1 = resk1 + wgk(jtw)*fsum + wgk(jtwm1)*(fval1m1 + fval2m1)
      ENDDO
C
      GAUSS_INT = resk1*hlgth1
      Abserr = ABS((resk1 - resg1)*hlgth1)
C
      END
C
C---------------------------------------------------
      DOUBLE PRECISION FUNCTION FISINT(Ib, Ux,jcc,nnuc)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'


      DOUBLE PRECISION ugrid,Ux,rofis
C     INTEGER JCC,kk,Ib,iugrid,nrbinfis
      INTEGER JCC,Ib,iugrid,nrbinfis
C
      IF(FISDEN(Nnuc).eq.0.) iugrid = nrbinfis(ib)
      IF(FISDEN(Nnuc).eq.1.) iugrid = NFISEN

      klo = 1
      khi = iugrid
      IF(Ux.LE.UGRid(klo))THEN
         klo = 0
         khi = 1
         GOTO 200
      ENDIF
c 
      IF(Ux.LE.UGRid(klo))THEN
         klo = 0
         khi = 1
         GOTO 200
      ENDIF
C
      IF(Ux.GE.UGRid(khi))THEN
         klo = iugrid - 1
         GOTO 200
      ENDIF
C
100   IF(khi - klo.GT.1)THEN
         k = (khi + klo)/2.
         IF(UGRid(k).GT.Ux)THEN
            khi = k
         ELSE
            klo = k
         ENDIF
         GOTO 100
      ENDIF
C
C     LEVEL DENSITY INTERPOLATION
C
 200  hhh = UGRid(khi) - UGRid(klo)
      c1 = (UGRid(khi) - Ux)/hhh
      c2 = (Ux - UGRid(klo))/hhh
      r1 = ROFis(klo, JCC,Ib)
      r2 = ROFis(khi, JCC,Ib)
      IF(r1.GT.0 .AND. r2.GT.0)THEN
         FISINT = MAX(10.**(c1*DLOG10(r1) + c2*DLOG10(r2)), 0.)
      ELSE
         FISINT = MAX(c1*r1 + c2*r2, 0.)
      ENDIF
C
      END
C

C-----------------------------------------------------------
      SUBROUTINE SIMPSFIS(Nnuc, Ibar, Ee)
C-----------------------------------------------------------
C Simpson integration
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
      COMMON /COMFIS4/ TFC, TFCc,jcc
      COMMON /ROFI1/ enh_ld(3, NFhump)
C
      INTEGER nn, i

c--------------------------
      DOUBLE PRECISION  UGRid 
c-------------------------

      DOUBLE PRECISION arg1,arg2,dens, destepp, UX1,UX2
      DOUBLE PRECISION  rofis, TFC, TFCc, XMInn, H
      DOUBLE PRECISION enh_ld, enh1,enh2,efb


      TFCc = 0.
c----------------------------------------------------------------------
      IF(ibar.eq.7)THEN

         DO i = 1,nrbinfis(3)
C           UX1=efb(3)+xminn(3)-efb(2) + (i - 1)*destepp
            UX2 = xminn(3) + (i - 1)*destepp  

C           arg1 = 2*PI*(efb(2)+UX1-ee)/H(2)
            arg2 = 2*PI*(efb(3)+UX2-ee)/H(3)
C           IF(arg1.GE.EXPmax)arg1=expmax
            IF(arg2.GE.EXPmax)arg2=expmax

C           teq=(1.+exp(arg1))*(1.+exp(arg2))
C           heq= 2*pi*arg2/log(teq-1.) 
            enh2=enh_ld(1,3)+enh_ld(2,3)*ux2+enh_ld(3,3)*ux2**2
            dens =enh2*ROfis(i,jcc,3)/(1. + EXP(arg2))

c            dens =min(FISINT(2,UX1,jcc,nnuc),FISINT(3,UX2,jcc,nnuc
c     &            ))*enh2/((1. + EXP(arg1))*(1. + EXP(arg2)))
            nn = 2
            IF((i*0.5).EQ.INT(i/2))nn = 4
            IF(i.EQ.1 .OR. i.EQ.(nrbinfis(3)))nn = 1
            dens = nn*dens
            TFCc = TFCc + dens
         ENDDO
         TFCc = TFCc*destepp
         RETURN
      ENDIF
c------------------------------------------------------------------------

      DO i = 1, nrbinfis(ibar)
         UX1 = XMInn(Ibar) + (i - 1)*destepp
         if(ux1.lt.0.)ux1=0.001
         enh1=enh_ld(1,ibar)+(enh_ld(2,ibar)+enh_ld(3,ibar)*ux1)*ux1
         arg1 = 2*PI*(UX1+efb(ibar)-ee)/H(Ibar)
         IF(arg1.GE.EXPmax)arg1=expmax

         dens   = enh1* FISINT(Ibar, UX1,jcc,nnuc)/(1. +EXP(arg1))

C        RCN 0/2004 
C        IF(FISden(Nnuc).EQ.0.)THEN                 
C           dens =enh1*ROfis(i,jcc,ibar)/(1. + EXP(arg1))
C        ENDIF
C        IF(FISden(Nnuc).EQ.1.)THEN
C           dens = enh1* FISINT(Ibar, UX1,jcc,nnuc)/(1. +EXP(arg1))
C        ENDIF   

         nn = 2
         IF((i*0.5).EQ.INT(i/2))nn = 4
         IF(i.EQ.1 .OR. i.EQ.(nrbinfis(ibar)))nn = 1
         dens = nn*dens
         TFCc = TFCc + dens
      ENDDO
      TFCc = TFCc*destepp
       
      RETURN 
      END
C
c=============================================================
      SUBROUTINE WRITE_OUTFIS(Nnuc)
      
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'

      COMMON /CRIT  / TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl
      COMMON /ROFI1/ enh_ld(3, NFhump)
      nrbarc = NRBar-NRWel


            WRITE(80, '(a40)')'----------------------------------------'
            WRITE(80, '(4x,a2,i3,2x,a2,i3)')'Z=', INT(Z(nnuc)), 'A=', 
     &            INT(A(nnuc))
            WRITE(80, '(a40)')'----------------------------------------'
            WRITE(80, '(a8,f2.0,a28,a20)')'FISBAR =', FISbar(nnuc)
            WRITE(80,*)'No.of parabolas=', NRBar,
     &            '   No.of wells=', Nrwel
            WRITE(80, *)'  '
C
            IF(NRBar.EQ.1)THEN
               WRITE(80, '(a)')'    Va      ha    (in Mev) '
               WRITE(80, '(2f8.3)')EFB(1), H(1)
               WRITE(80, *)' '
               WRITE(80, '(2a10)')'h2/2J(A)', '(in MeV)'
               WRITE(80, '(f9.4)')HJ(nnuc,1)
               WRITE(80, *)' '
               WRITE(80, '(a10)')'Beta2(A)'
               WRITE(80, '(f9.4)')DEFfis(1)
               WRITE(80, *)' '
            ENDIF
C
            IF(NRBar.EQ.2)THEN
               WRITE(80, '(a)')
     &                    '    Va      ha      Vb      hb     (in Mev) '
               WRITE(80, '(4f8.3)')(EFB(i), H(i), i = 1, NRBar)
               WRITE(80, *)' '
               WRITE(80, '(3a10)')'h2/2J(A)', 'h2/2J(B)', '(in MeV)'
               WRITE(80, '(2f9.4)')(HJ(nnuc,i), i = 1, NRBar)
               WRITE(80, *)' '
               WRITE(80, '(2a10)')'Beta2(A)', 'Beta2(B)'
               WRITE(80, '(2f9.4)')(DEFfis(i), i = 1, NRBar)
               WRITE(80, *)' '
            ENDIF
C
            IF(NRBar.EQ.3)THEN
               WRITE(80, '(a,1x,a)')
     &       '    Va      ha      Vb      hb      Vi      hi  (in Mev) '
               WRITE(80, '(6f8.3,15x)')(EFB(i), H(i), i = 1, NRBar)
               WRITE(80, *)' '
               WRITE(80, '(4a10)')'h2/2J(A)', 'h2/2J(B)', 'h2/2J(I)', 
     &                            '(in MeV)'
               WRITE(80, '(3f9.4)')(HJ(nnuc,i), i = 1, NRBar)
               WRITE(80, *)' '
               WRITE(80, '(3a10)')'Beta2(A)', 'Beta2(B)', 'Beta2(I)'
               WRITE(80, '(3f9.4)')(DEFfis(i), i = 1, NRBar)
               WRITE(80, *)' '
            ENDIF
C
            IF(NRBar.EQ.5)THEN
               WRITE(80, '(a,1x,a)')
     &'    Va      ha      Vb      hb      Vc       hc      Vi
     &hi      Vo      ho  (in Mev) '
               WRITE(80, '(10f8.3,15x)')
     &               (EFB(i), H(i), i = 1, NRBar)
               WRITE(80, *)' '
               WRITE(80, '(6a10)')'h2/2J(A)', 'h2/2J(B)', 'h2/2J(C)', 
     &                            'h2/2J(I)', 'h2/2J(O)', '(in MeV)'
               WRITE(80, '(5f9.4)')(HJ(nnuc,i), i = 1, NRBar)
               WRITE(80, *)' '
               WRITE(80, '(6a10)')'Beta2(A)', 'Beta2(B)', 'Beta2(C)', 
     &                            'Beta2(I)', 'Beta2(O)', '        '
               WRITE(80, '(5f9.4)')(DEFfis(i), i = 1, NRBar)
               WRITE(80, *)' '
            ENDIF
C
            WRITE(80, *)' '
            WRITE(80,*)'SUBEFF=', SUBeff(nnuc)
            WRITE(80, *)' '

            IF(SUBeff(Nnuc).EQ.1.)THEN
               WRITE(80,*) '  '
               WRITE(80,*) '      W0         W1         W2'
               WRITE(80, '(3f11.4)')(wimag(i), i=1,3)
               WRITE(80,*)
            ENDIF   
C
            DO ibar = 1, NRBar
               IF(ibar.LT.3)WRITE(80, '(a39,I2,a2,I2)')
     &                            'Number of discrete states at barrier'
     &                            , ibar, '=', NRFdis(ibar)
               IF(NRBar.EQ.3 .AND. ibar.EQ.3)WRITE(80, '(a48,I2,a2,I2)')
     &            'Number of discrete states at isomeric valley', ibar, 
     &            '=', NRFdis(ibar)
               IF(NRBar.EQ.5 .AND. (ibar.EQ.3 .OR. ibar.EQ.5))
     &            WRITE(80, '(a48,I2,a2,I2)')
     &            'Number of discrete states at isomeric valley', ibar, 
     &            '=', NRFdis(ibar)
               WRITE(80, *)'Edis   Jdis  Pidis'
               DO nr = 1, NRFdis(ibar)
                  WRITE(80, '(1x,1f5.3,1f6.1,1i4)')EFDis(nr, ibar), 
     &                  SFDis(nr, ibar), IPFdis(nr, ibar)
               ENDDO
            ENDDO
            WRITE(80, *)'  '
C
C
            WRITE(80, *)'FISDEN=', FISden(nnuc)
            IF(FISden(nnuc).EQ.0.)THEN
               WRITE(80, '(3(A9,f9.5),a9,f11.5)')'Acrt=', ACRt, 'Ucrt=', 
     &               UCRt, 'Econd=', ECOnd, 'DETcrt=', DETcrt
               WRITE(80, '(A9,f9.5,A9,f9.5)')'Tcrt=', TCRt, 'Scrt=', SCR
            ENDIF

            WRITE(80,*) '   '      
            WRITE(80,*) '                a0       a1  '
            DO nr=1, NRBarc
               WRITE(80,'(1x, A8, 1x, I1, 3f9.3)') 'Barrier', nr,
     &              enh_ld(1,nr),enh_ld(2,nr),enh_ld(3,nr)         
            ENDDO   
      END
