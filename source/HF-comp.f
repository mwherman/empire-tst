Ccc   * $Author: mike $
Ccc   * $Date: 2001-08-21 15:36:17 $
Ccc   * $Id: HF-comp.f,v 1.2 2001-08-21 15:36:17 mike Exp $
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
Ccc   * input:IEC  - energy index of the decaying state                  *
Ccc   *       NNUC - index of the decaying nucleus                       *
Ccc   *       NNUR - index of the residual nucleus                       *
Ccc   *       NEJC - index of the ejectile (0 for gamma)                 *
Ccc   *       XNOR - normalization factor (POP*STEP/DENHF)               *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * output:none                                                      *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * calls:BELLAC                                                     *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * author: M.Herman                                                 *
Ccc   * date:   28.Oct.1993                                              *
Ccc   * revision:1    by:M. Herman                on:30.Jul.1997         *
Ccc   * Transitions to discrete levels distributed between adjacent      *
Ccc   * spectrum bins so that after spectrum integration average         *
Ccc   * energy is conserved.                                             *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
c
c
c Dummy arguments
c
      INTEGER Iec , Nejc , Nnuc , Nnur
      DOUBLE PRECISION Xnor
c
c Local variables
c
      DOUBLE PRECISION eemi , excnq , pop1 , pop2 , poph , popl , pops , 
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
         DO j = 1, NLW, LTUrbo              !loop over residual spins
            pop1 = Xnor*SCRt(ie, j, 1, Nejc)
            pop2 = Xnor*SCRt(ie, j, 2, Nejc)
            pops = pop1 + pop2
            icse = MAX0(2, icse)
            AUSpec(icse, Nejc) = AUSpec(icse, Nejc) + pop1 + pop2
            IF(ie.EQ.1)pops = pops*0.5
            CSE(icse, Nejc, Nnuc) = CSE(icse, Nejc, Nnuc) + pops
            IF(ENDf.EQ.1.D0)CALL BELLAC(Iec, Nejc, Nnuc, pops)
            POP(ie, j, 1, Nnur) = POP(ie, j, 1, Nnur) + pop1
            POP(ie, j, 2, Nnur) = POP(ie, j, 2, Nnur) + pop2
            IF(Nejc.NE.0 .AND. POPmax(Nnur).LT.POP(ie, j, 1, Nnur))
     &         POPmax(Nnur) = POP(ie, j, 1, Nnur)
         ENDDO
      ENDDO
      DO il = 1, NLV(Nnur)
         eemi = excnq - ELV(il, Nnur)
         IF(eemi.LT.0.0D0)RETURN
C--------transitions to discrete levels are distributed
C--------between the nearest spectrum bins (inversly proportional to the
C--------distance of the actual energy to the bin energy
         xcse = eemi/DE + 1.0001
         icsl = INT(xcse)
         icsh = icsl + 1
         pop1 = Xnor*SCRtl(il, Nejc)
         popl = pop1*(FLOAT(icsh) - xcse)/DE
         IF(icsl.EQ.1)popl = 2.0*popl
         poph = pop1*(xcse - FLOAT(icsl))/DE
         IF(icsl.LE.NDECSE)CSE(icsl, Nejc, Nnuc) = CSE(icsl, Nejc, Nnuc)
     &      + popl
         IF(icsh.LE.NDECSE)THEN
            CSE(icsh, Nejc, Nnuc) = CSE(icsh, Nejc, Nnuc) + poph
         ELSE
            WRITE(6, *)' '
            WRITE(6, *)' OOPS! I AM OUT OF NDECSE DIMENSION IN ACCUM'
            STOP
         ENDIF
         IF(ENDf.EQ.1.D0)CALL BELLAC(Iec, Nejc, Nnuc, pop1/DE)
         POPlv(il, Nnur) = POPlv(il, Nnur) + pop1
         REClev(il, Nejc) = REClev(il, Nejc) + pop1
C--------Add isotropic CN contribution to direct ang. distributions 
         IF(Nnuc.EQ.1 .AND. Iec.EQ.NEX(1)) THEN
            pop1 = pop1/4.0/PI
            DO na = 1, NDANG
               CSAlev(na, il, Nejc) = CSAlev(na, il, Nejc) + pop1
            ENDDO
         ENDIF 
      ENDDO
      END
C
C
      SUBROUTINE BELLAC(Iec, Nejc, Nnuc, Popt)
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:mpu*
Ccc   *                      B E L L A C                                 *
Ccc   *                                                                  *
Ccc   * Substracts a piece from the first-emission double-differential   *
Ccc   * spectrum and adds it up to the second emission. Used to          *
Ccc   * produce ddx as requested by the ENDF format.                     *
Ccc   * At present only two emissions are considered, i.e. (n,3n)        *
Ccc   * reaction can't be processed.                                     *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * input:IEC  - energy index of the decaying state                  *
Ccc   *       NNUC - index of the decaying nucleus                       *
Ccc   *       NEJC - index of the ejectile                               *
Ccc   *       POPT - little piece of cross section being   moved from    *
Ccc   *              one spectrum onto another                           *
Ccc   *                                                                  *
Ccc   * output:none                                                      *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * calls:none                                                       *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * author: R. Sturiale & M.Herman                                   *
Ccc   * date:   23.May.1996                                              *
Ccc   * revision:1    by:M.Herman                 on:07.Feb.1997         *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
c
c
c Dummy arguments
c
      INTEGER Iec , Nejc , Nnuc
      DOUBLE PRECISION Popt
c
c Local variables
c
      INTEGER iang , icsp
      INTEGER INT
C
C
C
      IF(Nnuc.EQ.1)THEN
C--------
C--------compound nucleus (correction of the gamma spectrum only)
C--------
         icsp = INT((EX(NEX(1),1) - EX(Iec,1))/DE + 1.0001)
C------- if ICSP=1 ==> Egamma=0 ==> no gamma was emitted ==> return
         IF(icsp.EQ.1)RETURN
         IF(Nejc.EQ.1)THEN
C---------- neutron emission
            CSE(icsp, 0, INRes) = CSE(icsp, 0, INRes) + Popt
            CSE(icsp, 0, 1) = CSE(icsp, 0, 1) - Popt
         ENDIF
         IF(Nejc.EQ.2)THEN
C---------- proton  emission
            CSE(icsp, 0, IPRes) = CSE(icsp, 0, IPRes) + Popt
            CSE(icsp, 0, 1) = CSE(icsp, 0, 1) - Popt
         ENDIF
         IF(Nejc.EQ.3)THEN
C---------- alpha   emission
            CSE(icsp, 0, IARes) = CSE(icsp, 0, IARes) + Popt
            CSE(icsp, 0, 1) = CSE(icsp, 0, 1) - Popt
         ENDIF
      ENDIF
      IF(Nnuc.EQ.INRes)THEN
C-----
C-----first residue after neutron emission
C-----
         icsp = INT((EX(NEX(1),1) - EX(Iec,INRes) - Q(1,1))/DE + 1.0001)
         IF(Nejc.EQ.1)THEN
            DO iang = 1, NDANG
C------------- neutron emission
               CSEa(icsp, iang, 1, INRes) = CSEa(icsp, iang, 1, INRes)
     &            + Popt*CSEan(icsp, iang, 1)
               CSEa(icsp, iang, 1, 1) = CSEa(icsp, iang, 1, 1)
     &                                  - Popt*CSEan(icsp, iang, 1)
            ENDDO
         ENDIF
         IF(Nejc.EQ.2)THEN
C-------    proton  emission
            DO iang = 1, NDANG
               CSEa(icsp, iang, 1, IPRes) = CSEa(icsp, iang, 1, IPRes)
     &            + Popt*CSEan(icsp, iang, 1)
               CSEa(icsp, iang, 1, 1) = CSEa(icsp, iang, 1, 1)
     &                                  - Popt*CSEan(icsp, iang, 1)
            ENDDO
         ENDIF
         IF(Nejc.EQ.3)THEN
C---------- alpha   emission
            DO iang = 1, NDANG
               ANCsea(icsp, iang, 1) = ANCsea(icsp, iang, 1)
     &                                 + Popt*CSEan(icsp, iang, 1)
               CSEa(icsp, iang, 1, 1) = CSEa(icsp, iang, 1, 1)
     &                                  - Popt*CSEan(icsp, iang, 1)
            ENDDO
         ENDIF
      ENDIF
      IF(Nnuc.EQ.IPRes)THEN
C-----
C-----first residue after proton emission
C-----
         icsp = INT((EX(NEX(1),1) - EX(Iec,IPRes) - Q(INRes,1))
     &          /DE + 1.0001)
         IF(Nejc.EQ.1)THEN
            DO iang = 1, NDANG
C------------- neutron emission
               CSEa(icsp, iang, 2, INRes) = CSEa(icsp, iang, 2, INRes)
     &            + Popt*CSEan(icsp, iang, 2)
               CSEa(icsp, iang, 2, 1) = CSEa(icsp, iang, 2, 1)
     &                                  - Popt*CSEan(icsp, iang, 2)
            ENDDO
         ENDIF
         IF(Nejc.EQ.2)THEN
C---------- proton  emission
            DO iang = 1, NDANG
               CSEa(icsp, iang, 2, IPRes) = CSEa(icsp, iang, 2, IPRes)
     &            + Popt*CSEan(icsp, iang, INRes)
               CSEa(icsp, iang, 2, 1) = CSEa(icsp, iang, 2, 1)
     &                                  - Popt*CSEan(icsp, iang, 2)
            ENDDO
         ENDIF
         IF(Nejc.EQ.3)THEN
C---------- alpha   emission
            DO iang = 1, NDANG
               APCsea(icsp, iang, 1) = APCsea(icsp, iang, 1)
     &                                 + Popt*CSEan(icsp, iang, 2)
               CSEa(icsp, iang, 2, 1) = CSEa(icsp, iang, 2, 1)
     &                                  - Popt*CSEan(icsp, iang, 2)
            ENDDO
         ENDIF
      ENDIF
      IF(Nnuc.EQ.IARes)THEN
C-----
C-----first residue after alpha  emission
C-----
         icsp = INT((EX(NEX(1),1) - EX(Iec,IARes) - Q(3,1))/DE + 1.0001)
         IF(Nejc.EQ.1)THEN
            DO iang = 1, NDANG
C------------- neutron emission
               ANCsea(icsp, iang, 2) = ANCsea(icsp, iang, 2)
     &                                 + Popt*CSEan(icsp, iang, 3)
               CSEa(icsp, iang, 3, 1) = CSEa(icsp, iang, 3, 1)
     &                                  - Popt*CSEan(icsp, iang, 3)
            ENDDO
         ENDIF
         IF(Nejc.EQ.2)THEN
C---------- proton  emission
            DO iang = 1, NDANG
               APCsea(icsp, iang, 2) = APCsea(icsp, iang, 2)
     &                                 + Popt*CSEan(icsp, iang, 3)
               CSEa(icsp, iang, 3, 1) = CSEa(icsp, iang, 3, 1)
     &                                  - Popt*CSEan(icsp, iang, 3)
            ENDDO
         ENDIF
         IF(Nejc.EQ.3)THEN
C---------- alpha   emission
            DO iang = 1, NDANG
               CSEa(icsp, iang, 3, 1) = CSEa(icsp, iang, 3, 1)
     &                                  - Popt*CSEan(icsp, iang, 3)
            ENDDO
         ENDIF
      ENDIF
      IF(Nnuc.EQ.ILIres .AND. NDEJC.EQ.4 .AND. NEMc.GT.0)THEN
C-----
C-----first residue after light ion emission
C-----
         icsp = INT((EX(NEX(1),1) - EX(Iec,ILIres) - Q(NDEJC,1))
     &          /DE + 1.0001)
         IF(Nejc.EQ.1)THEN
            DO iang = 1, NDANG
C------------- neutron emission
               CSEa(icsp, iang, NDEJC, 1) = CSEa(icsp, iang, NDEJC, 1)
     &            - Popt*CSEan(icsp, iang, NDEJC)
            ENDDO
         ENDIF
         IF(Nejc.EQ.2)THEN
C---------- proton  emission
            DO iang = 1, NDANG
               CSEa(icsp, iang, NDEJC, 1) = CSEa(icsp, iang, NDEJC, 1)
     &            - Popt*CSEan(icsp, iang, NDEJC)
            ENDDO
         ENDIF
         IF(Nejc.EQ.3)THEN
C---------- alpha   emission
            DO iang = 1, NDANG
               CSEa(icsp, iang, NDEJC, 1) = CSEa(icsp, iang, NDEJC, 1)
     &            - Popt*CSEan(icsp, iang, NDEJC)
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
c
c
c Dummy arguments
c
      INTEGER Iec , Ipc , Jc , Nejc , Nnuc , Nnur
      DOUBLE PRECISION Sum
c
c Local variables
c
      DOUBLE PRECISION cor , corr , eout , eoutc , frde , hisr , s , 
     &                 smax , smin , sumdl , sumtl1 , sumtl2 , xjc , xjr
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
            cor = 1.0
            IF(eout.LT.DE)THEN
C-----------level above the bin
               IF(eout.LT.0.0D0)GOTO 100
               IF(eout.LT.DE)cor = 0.5
            ENDIF
            sumdl = 0.0
            CALL TLLOC(Nnur, Nejc, eout, il, frde)
            smin = ABS(XJLv(i, Nnur) - SEJc(Nejc))
            smax = XJLv(i, Nnur) + SEJc(Nejc) + 0.01
            s = smin
C-----------LOOP OVER CHANNEL SPIN ----------------------------------------
 20         lmin = INT(ABS(xjc - s) + 1.01)
            lmax = INT(xjc + s + 1.01)
            lmax = MIN0(NLW, lmax)
C-----------DO LOOP OVER L ------------------------------------------------
            DO l = lmin, lmax
               ipar = 1 + LVP(i, Nnur)*Ipc*( - 1)**(l - 1)
               IF(ipar.NE.0)sumdl = sumdl + TL(il, l, Nejc, Nnur)
     &                              + frde*(TL(il + 1, l, Nejc, Nnur)
     &                              - TL(il, l, Nejc, Nnur))
            ENDDO
c-----------DO LOOP OVER L --- DONE ----------------------------------------
            s = s + 1.
            IF(s.LE.smax)GOTO 20
C-----------LOOP OVER CHANNEL SPIN ------ DONE ----------------------------
            sumdl = sumdl*RORed*cor*TUNe(Nejc, Nnuc)
            SCRtl(i, Nejc) = sumdl
            Sum = Sum + sumdl
         ENDDO
c--------DO LOOP OVER DISCRETE LEVELS --------- DONE --------------------
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
c
c
c Dummy arguments
c
      INTEGER Nnuc
c
c Local variables
c
      DOUBLE PRECISION egd , gacs , popl
      REAL FLOAT
      INTEGER i, icse, j, j1, l
C
C
C
      IF(IOUt.GT.2)WRITE(6, 99001)
99001 FORMAT(1X, ////, 1X, 27('*'), /, 1X, 
     &       'Discrete gamma transitions ', /, 1X, 27('*'), //)
      DO i = 1, NLV(Nnuc) - 1
         l = NLV(Nnuc) - i + 1
         IF(IBR(l, 1, 2, Nnuc).EQ.0)THEN
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
     &                'Intensity mb ', /)
               DO j = 1, NDBR
                  j1 = IBR(l, j, 1, Nnuc)
                  IF(j1.EQ.0)GOTO 100
                  IF(j1.GE.l)THEN
                     WRITE(6, 99004)
99004                FORMAT(10X, 'ERROR IN DISCRETE LEVEL DECAY DATA', 
     &                      /, 10X, 'FINAL LEVEL ABOVE THE INITIAL ONE', 
     &                      /, 10X, 'FURTHER DECAY NOT CONSIDERED ')
                     GOTO 99999
                  ENDIF
                  gacs = popl*FLOAT(IBR(l, j, 2, Nnuc))/100.
                  POPlv(j1, Nnuc) = POPlv(j1, Nnuc) + gacs
                  egd = ELV(l, Nnuc) - ELV(j1, Nnuc)
                  icse = 2.0001 + egd/DE
                  CSE(icse, 0, Nnuc) = CSE(icse, 0, Nnuc) + gacs/DE
                  CSEmis(0, Nnuc) = CSEmis(0, Nnuc) + gacs
                  IF(IOUt.GT.2)WRITE(6, 99005)ELV(j1, Nnuc), 
     &                               LVP(j1, Nnuc)*XJLv(j1, Nnuc), egd, 
     &                               gacs
99005             FORMAT(5X, F7.4, 2X, F5.1, 5X, F7.4, 5X, G13.5)
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
c
c
c Dummy arguments
c
      INTEGER Iec , Ipc , Jc , Nnuc
      DOUBLE PRECISION Sum
c
c Local variables
c
      DOUBLE PRECISION accn , atil , eg , gamma , se1 , se2 , se2m1 , 
     &                 sm1 , t , xjc
      DOUBLE PRECISION E1 , E2 , XM1
      REAL FLOAT
      INTEGER i, ier, ineg, iodd, ipar, ipos, j, jmax, jmin, lmax, lmin
      INTEGER MAX0, MIN0
C
C
C
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
C-----calculate nuclear temperature for the generalized Lorenzian to be
C-----used for E1 strength function determination. NOTE: this temperature
C-----is not consistent with the actual level densities.
      t = 0.
      IF(EX(Iec, Nnuc).GT.1.0D-5)THEN
         atil = 0.073*A(Nnuc) + 0.115*A(Nnuc)**0.666667
         gamma = 0.40/A(Nnuc)**0.33333
         accn = atil*(1.0 + SHC(Nnuc)*(1.0 - EXP((-gamma*EX(Iec,Nnuc))))
     &          /EX(Iec, Nnuc))
         IF(EX(Iec, Nnuc).GE.YRAst(Jc, Nnuc))
     &      t = SQRT((EX(Iec,Nnuc) - YRAst(Jc,Nnuc))/accn)
      ENDIF
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
         se1 = E1(eg, t)*TUNe(0, Nnuc)
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
                  SCRtl(i, 0) = E1(eg, t)*iodd + XM1(eg)*ipar
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
c
c
c Dummy arguments
c
      INTEGER Iec , Ipc , Jc , Nnuc
      DOUBLE PRECISION Sum
c
c Local variables
c
      DOUBLE PRECISION E1 , E2 , XM1
      DOUBLE PRECISION eg , se1 , se2 , se2m1 , sm1 , sumn , sump , t , 
     &                 xjc
      REAL FLOAT
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
            se1 = E1(eg, t)*TUNe(0, Nnuc)
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
                  SCRtl(i, 0) = E1(eg, t)*iodd + XM1(eg)*ipar
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
c
c
c Dummy arguments
c
      INTEGER Iec , Jc , Nnuc
      DOUBLE PRECISION Sumfis
C
C Local variables
C
      DOUBLE PRECISION accn, ampl, atil, ekin, ekinm, erest, fisbar, 
     &                 fric, gamma, gpart, htom, PI, shredt, sum1, sum2, 
     &                 sum3, sumf, sumgs, sumr, tau, temp
      REAL FLOAT
      INTEGER kn , knm
      DOUBLE PRECISION TLF
C
C
C
      PI = 3.141593
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
      fisbar = FISb(Jc, Nnuc) - SHC(Nnuc)*SHCjf(Jc, Nnuc)*shredt
      ekinm = EX(Iec, Nnuc) - fisbar
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
         IF(fisbar - YRAst(Jc, Nnuc).GT.0.0D0)THEN
C-----------reduction of fission width due to the transient time needed
C-----------to form a saddle point (nuclear viscosity 2-nd effect)
C-----------according to Rastopchin et al. Sov. Jour. Nucl. Phys. 53,741(1991)
C-----------omega1=omega0=1.6*10^21 (1/s) hbar*omega=1MeV
C-----------BETAV critical =3.2; 0.19531 stands for 1/(2*omega1**2)
            gpart = DENhf/RO(Iec, Jc, Nnuc)/2./PI
C           GFIS = SUMFIS/RO(IEC,JC,NNUC)/2./PI
            tau = LOG(10.0*(fisbar - YRAst(Jc,Nnuc))/temp)
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
C Dummy arguments
C
      DOUBLE PRECISION Ekin
c
c Local variables
c
      DOUBLE PRECISION atlf , htom , pi
c
c
      htom = 1.0
      pi = 3.141593
      TLF = 1.
      atlf = 2.*pi*Ekin/htom
      IF(atlf.LT.74.D0)TLF = 1./(1. + EXP((-atlf)))
      END
