Ccc   * $Author: mike $
Ccc   * $Date: 2002-09-20 14:16:53 $
Ccc   * $Id: HRTW-comp.f,v 1.4 2002-09-20 14:16:53 mike Exp $
C
      SUBROUTINE HRTW
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:ppu*
Ccc   *                         H R T W                                  *
Ccc   *                                                                  *
Ccc   *  Calculate decay of the Compound Nucleus capture states in       *
Ccc   *  terms of the HRTW theory (width fluctuation correction).        *
Ccc   *  Uses modified routines of standard Hauser-Feshbach (DECAY,      *
Ccc   *  DECAYG, FISSION) and HRTW_MARENG to decompose capture           *
Ccc   *  cross sections into partial wave components.                    *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   *  CALLS:                                                          *
Ccc   *    HRTW_MARENG                                                   *
Ccc   *             OMTL                                                 *
Ccc   *    ULMDYN                                                        *
Ccc   *    HRTW_DECAY                                                    *
Ccc   *             TLLOC                                                *
Ccc   *             TL2VL                                                *
Ccc   *    HRTW_DECAYG                                                   *
Ccc   *             E1                                                   *
Ccc   *             E2                                                   *
Ccc   *             XM1                                                  *
Ccc   *    HRTW_FISSION                                                  *
Ccc   *             TL2VL                                                *
Ccc   *             TLF                                                  *
Ccc   *    ACCUM                                                         *
Ccc   *        BELLAC                                                    *
Ccc   *                                                                  *
Ccc   * author: M.Herman                                                 *
Ccc   * date:  10 Sept. 2000                                             *
Ccc   * last revision by:                              on:               *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C
C COMMON variables
C
      DOUBLE PRECISION H_Abs(NDHRTW2, 3), H_Sumtl, H_Sumtls, H_Sweak, 
     &                 H_Tav, H_Tl(NDHRTW1, 2), H_Tthr, TFIs
      INTEGER MEMel(NDHRTW2, 3), NDIvf, NH_lch, NSCh
      COMMON /IHRTW / NSCh, NDIvf, MEMel, NH_lch
      COMMON /RHRTW / H_Tl, H_Sumtl, H_Sumtls, H_Sweak, H_Abs, H_Tav, 
     &                H_Tthr, TFIs
C
C Local variables
C
      DOUBLE PRECISION ares, csemist, csfis, sgamc, sum, sumfis, sumg, 
     &                 tlump, xnor, zres
      REAL FLOAT
      INTEGER i, ich, iloc, ip, ipar, izares, jcn, ke, nejc, nhrtw, 
     &        nnuc, nnur
      INTEGER INT
      DOUBLE PRECISION VT1
C
C
C
C-----threshold for considering channel as a 'strong' one
      H_Tthr = 0.0001
C-----set CN nucleus
      nnuc = 1
C-----locate residual nuclei
      DO nejc = 1, NEJcm
         ares = A(nnuc) - AEJc(nejc)
         zres = Z(nnuc) - ZEJc(nejc)
         izares = INT(1000.0*zres + ares)
         CALL WHERE(izares, nnur, iloc)
         IF(iloc.EQ.1)THEN
            WRITE(6, *)' RESIDUAL NUCLEUS WITH A=', ares, ' AND Z=', 
     &                 zres, ' HAS NOT BEEN INITIALIZED'
            WRITE(6, *)' EXECUTION STOPPED'
         ENDIF
         NREs(nejc) = nnur
      ENDDO
C-----reset variables
      sgamc = 0.0
      csemist = 0.0
      csfis = 0.0
      sumfis = 0.0
C-----assure that full gamma cascade in the first CN is
C-----accounted for when width fluctuation (HRTW) is selected
      GCAsc = 1.0
      ke = NEX(nnuc)
C-----
C-----start CN nucleus decay
C-----
C-----do loop over decaying nucleus parity
      DO ipar = 1, 2
         ip = INT(( - 1.0)**(ipar + 1))
C--------do loop over decaying nucleus spin
         DO jcn = 1, NLW
C           WRITE(6,*)'  '
C           WRITE(6,*)'DECAY STATE J=',jcn,' PI=',ipar
C           WRITE(6,*)'  '
            nhrtw = 0
C-----------initialize variables
            DENhf = 0.0
            NSCh = 0
            NH_lch = 0
            H_Sumtls = 0.0
            H_Sumtl = 0.0
            H_Tav = 0.0
            H_Sweak = 0.0
            DO i = 1, NDHRTW1
               H_Tl(i, 1) = 0.0
               H_Tl(i, 2) = 0.0
            ENDDO
C-----------prepare gamma-strength (GMR) parameters (if spin
C-----------dependent GDR selected)
            IF(GDRdyn.EQ.1.0D0)CALL ULMDYN(nnuc, jcn, EX(ke, nnuc))
C-----------
C-----------start the first HRTW run
C-----------
C-----------do loop over ejectiles
            DO nejc = 1, NEJcm
               nnur = NREs(nejc)
C              WRITE(6,*)'emitting ejectile=', nejc
               CALL HRTW_DECAY(nnuc, ke, jcn, ip, nnur, nejc, sum, 
     &                         nhrtw)
C              WRITE(6,*)'sum for ejectile=' , nejc, sum
               H_Sumtl = H_Sumtl + sum
            ENDDO
C-----------do loop over ejectiles       ***done***
C-----------gamma emision
            sumg = 0.0
            CALL HRTW_DECAYG(nnuc, ke, jcn, ip, sumg, nhrtw)
            H_Sumtl = H_Sumtl + sumg
            H_Sweak = H_Sweak + sumg
C-----------fission
            IF(FISsil(nnuc))CALL FISSION(nnuc, ke, jcn, sumfis)
            H_Sumtl = H_Sumtl + sumfis
            H_Sweak = H_Sweak + sumfis
            IF(H_Sumtl.GT.0.0D0 .AND. (H_Sumtl - H_Sweak).GT.0.0D0)THEN
               tlump = (H_Sumtl - H_Sweak)
     &                 /(10.0*(1.0 + (H_Sweak)/(H_Sumtl-H_Sweak)))
C              !define a good Tlump
            ELSE
               tlump = H_Sweak
            ENDIF
            IF(H_Sumtl.GT.0.0D0)THEN
C--------------check whether tfis is not too big compared to a good Tlump
               NDIvf = INT(sumfis/tlump + 1.0)
               TFIs = sumfis/FLOAT(NDIvf)
               H_Sumtls = H_Sumtls + NDIvf*TFIs**2
               H_Tav = H_Sumtls/H_Sumtl
               IF(H_Tav.LT.TFIs)THEN
                  H_Sumtls = H_Sumtls - NDIvf*TFIs**2
                  NDIvf = NDIvf*(TFIs/H_Tav + 1)
                  TFIs = sumfis/FLOAT(NDIvf)
                  H_Sumtls = H_Sumtls + NDIvf*TFIs**2
                  H_Tav = H_Sumtls/H_Sumtl
               ENDIF
C--------------redefine fission transmission coef. using single iteration
               TFIs = VT1(TFIs, H_Tav, H_Sumtl)
               sumfis = FLOAT(NDIvf)*TFIs
            ELSE
               H_Tav = 0.0
            ENDIF
C           WRITE(6,*)'sum gamma ' , sumg
C           WRITE(6,*)'sum fission redifined ' , sumfis
C           WRITE(6,*)'total sum for this state ' , H_Sumtl
C           WRITE(6,*)'sum Tl**2 for this state ' , H_Sumtls
C           WRITE(6,*)'sum of weak for this state ' , H_Sweak
C           WRITE(6,*)'number of strong Tls for this state ' , NH_lch
C           WRITE(6,*)'average Tl for this state ' , H_Tav
C           WRITE(6,*)'strong Tls from this state ' , H_Tl
C           WRITE(6,*)'first entry DENhf=',denhf
C--------------
C--------------the first HRTW run completed
C--------------
C-----------calculate V's for the strong channels (iteration)
C           WRITE(6,*)'  '
            IF(NH_lch.LE.NDHRTW1)
     &         CALL AUSTER(H_Tl, H_Tav, H_Sumtl, H_Sweak, NH_lch, 
     &         NDHRTW1)
C           WRITE(6,*)'strong Vs from this state ' , H_Tl
C           WRITE(6,*)'  '
C           WRITE(6,*)'  '
C           WRITE(6,*)'  '
C           WRITE(6,*)'  '
C-----------
C-----------start the second HRTW run
C-----------
C-----------calculate reaction cross section and its spin distribution
C-----------split into contributions from individual partial waves
            CALL HRTW_MARENG(0, 0, jcn, ipar, ich)
            DO i = 1, ich
C              WRITE(6,*)' '
C              WRITE(6,*)'HRTW entry=',i
C              WRITE(6,*)' '
               NSCh = 0
C-----------do loop over ejectiles (fission is not repeated)
               nhrtw = i
               DENhf = 0.0
               DO nejc = 1, NEJcm
                  nnur = NREs(nejc)
C                 WRITE(6,*)'  '
C                 WRITE(6,*)'second entry with ejec ' , nejc
                  CALL HRTW_DECAY(nnuc, ke, jcn, ip, nnur, nejc, sum, 
     &                            nhrtw)
C                 WRITE(6,*)'sum for ejec=' ,nejc, sum
               ENDDO
C-----------do loop over ejectiles       ***done***
C              CALL HRTW_DECAYG(nnuc,ke,jcn,ip,sumg,nhrtw)
               DENhf = DENhf + sumg + sumfis
C              WRITE(6,*)'second entry DENhf=',denhf
C              WRITE(6,*)'second entry sumg=',sumg
C-----------
C-----------correct scratch matrix for enhancement of the elastic channels
C-----------
               DO nejc = 1, NEJcm
                  nnur = NREs(nejc)
                  IF(IZA(nnur).EQ.IZA(0))
     &               CALL ELCORR(nnuc, ke, jcn, ip, nnur, nejc, nhrtw)
               ENDDO
C-----------
C-----------normalization and accumulation
C-----------
               xnor = H_Abs(i, 1)/DENhf
C              stauc = stauc + RO(ke,jcn,nnuc)*xnor
               IF(RO(ke, jcn, nnuc).NE.0.0D0)sgamc = sgamc + 
     &            DENhf*H_Abs(i, 1)/RO(ke, jcn, nnuc)
C-----------particles
               DO nejc = 1, NEJcm
                  nnur = NREs(nejc)
                  CALL ACCUM(ke, nnuc, nnur, nejc, xnor)
                  CSEmis(nejc, nnuc) = CSEmis(nejc, nnuc)
     &                                 + xnor*SCRtem(nejc)
               ENDDO
C-----------gammas
               CALL ACCUM(ke, nnuc, nnuc, 0, xnor)
               CSEmis(0, nnuc) = CSEmis(0, nnuc) + xnor*SCRtem(0)
               POP(ke, jcn, ipar, nnuc) = 0.0
C-----------fission
               csfis = csfis + sumfis*xnor
C-----------calculate total emission
               DO nejc = 0, NEJcm
                  csemist = csemist + CSEmis(nejc, nnuc)
               ENDDO
               csemist = csemist + csfis
            ENDDO    !loop over partial wave contributions to CN state
         ENDDO       !loop over decaying nucleus spin
      ENDDO          !loop over decaying nucleus parity
      END
C
C
C
      SUBROUTINE HRTW_DECAY(Nnuc, Iec, Jc, Ipc, Nnur, Nejc, Sum, Nhrtw)
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:PPu*
Ccc   *                      H R T W  _  D E C A Y                       *
Ccc   *                (function to function version)                    *
Ccc   *                                                                  *
Ccc   * Calculates decay of a continuum state in nucleus NNUC into       *
Ccc   * continuum anddiscrete states of the residual nucleus NNUR       *
Ccc   * through the emission of the ejectile NEJC including width        *
Ccc   * fluctuation correction according to HRTW theory.                 *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * input:NNUC - decaying nucleus index                              *
Ccc   *       IEC  - energy index of the decaying state                  *
Ccc   *       JC   - spin index of the decaying state                    *
Ccc   *       IPC  - parity of the decaying state                        *
Ccc   *       NNUR - residual nucleus index                              *
Ccc   *       NEJC - ejectile index                                      *
Ccc   *       Nhrtw- 0 first HRTW entry (calculate Tl's summs)           *
Ccc   *             >0 final HRTW entry (calculate emission widths)      *
Ccc   *                                                                  *
Ccc   * output:SUM - SUM of transmission coefficients over all outgoing  *
Ccc   *              channels for the requested decay                    *
Ccc   *              Apart of this standard feature it comunicates       *
Ccc   *              quantities needed for the HRTW theory through       *
Ccc   *              the HRTW common:                                    *
Ccc   *                                                                  *
Ccc   *             H_Tl(.,.) - list of strong Tl's and their number(rho)*
Ccc   *             H_Tloc(.,.) - l and ejectile for each strong Tl      *
Ccc   *             H_sumtls - sum of squared Tl's (all)                 *
Ccc   *             H_sweak - sum of weak Tl's (not squared)             *
Ccc   *             H_lch - number of strong Tl's                        *
Ccc   *             H_abs(.,.) - absorption x-sec decomposed in l's      *
Ccc   *             H_Tav - average transmission coefficient             *
Ccc   *             H_Tthr - threshold to consider Tl as strong          *
Ccc   *             MEMel  - records l and position of elastic channel   *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * calls:TLLOC                                                      *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * author: M.Herman                                                 *
Ccc   * date:    5.Sep.2000                                              *
Ccc   * revision:#    by:name                     on:xx.mon.199x         *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C
C COMMON variables
C
      DOUBLE PRECISION ELTl(NDLW), H_Abs(NDHRTW2, 3), H_Sumtl, H_Sumtls, 
     &                 H_Sweak, H_Tav, H_Tl(NDHRTW1, 2), H_Tthr, TFIs
      INTEGER MEMel(NDHRTW2, 3), NDIvf, NH_lch, NSCh
      COMMON /ELASTIC/ ELTl
      COMMON /IHRTW / NSCh, NDIvf, MEMel, NH_lch
      COMMON /RHRTW / H_Tl, H_Sumtl, H_Sumtls, H_Sweak, H_Abs, H_Tav, 
     &                H_Tthr, TFIs
C
C Dummy arguments
C
      INTEGER Iec, Ipc, Jc, Nejc, Nhrtw, Nnuc, Nnur
      DOUBLE PRECISION Sum
C
C Local variables
C
      DOUBLE PRECISION cor, corr, eout, eoutc, frde, hisr, rho, s, smax, 
     &                 smin, sumdl, sumtl1, sumtl2, tld, xjc, xjr
      REAL FLOAT
      INTEGER i, ichsp, iel, ier, iermax, ietl, iexc, il, ip1, ip2, 
     &        ipar, itlc, j, jr, l, lmax, lmaxf, lmin, mul
      INTEGER INT, MIN0
      DOUBLE PRECISION VT
C
C
C
C     WRITE(6,*)' '
C     WRITE(6,*)'ejectile ,nhrtw ',nejc,nhrtw
C     WRITE(6,*)' '
      hisr = HIS(Nnur)
      xjc = FLOAT(Jc) + HIS(Nnuc)
C-----clear scratch matrices
C     NSCh = 0
      Sum = 0.0
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
         DO jr = 1, NLW, LTUrbo            ! do loop over r.n. spins
            xjr = FLOAT(jr) + hisr
            smin = ABS(xjr - SEJc(Nejc))
            smax = xjr + SEJc(Nejc)
            mul = INT(smax - smin + 1.0001)
            DO ichsp = 1, mul              ! do loop over channel spin
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
                  rho = RO(iermax, jr, Nnur)*DE*TUNe(Nejc, Nnuc)
C-----------------odd and even l-values treated separately
C-----------------IP1 and IP2 decide to which parity each SUMTL  goes
                  sumtl1 = 0.0
                  DO l = lmin, lmax, 2      ! do loop over l
                     IF(Nhrtw.GT.0)THEN
C-----------------------replace Tl with V in the second HRTW entry
                        sumtl1 = sumtl1 + VT(TL(5, l, Nejc, Nnur))
                     ELSE
C-----------------------first entry with HRTW
C                       WRITE(6,*)'A Tl= ' , TL(5,l,Nejc,Nnur)
                        CALL TL2VL(TL(5, l, Nejc, Nnur), rho)
                        sumtl1 = sumtl1 + TL(5, l, Nejc, Nnur)
                     ENDIF
                  ENDDO
                  sumtl2 = 0.0
                  DO l = lmin + 1, lmax, 2
                     IF(Nhrtw.GT.0)THEN
C-----------------------replace Tl with V in the second HRTW entry
                        sumtl2 = sumtl2 + VT(TL(5, l, Nejc, Nnur))
                     ELSE
C-----------------------first entry with HRTW
C                       WRITE(6,*)'B Tl= ' , TL(5,l,Nejc,Nnur)
                        CALL TL2VL(TL(5, l, Nejc, Nnur), rho)
                        sumtl2 = sumtl2 + TL(5, l, Nejc, Nnur)
                     ENDIF
                  ENDDO                     ! over l
                  SCRt(iermax, jr, ip1, Nejc)
     &               = SCRt(iermax, jr, ip1, Nejc)
     &               + sumtl1*RO(iermax, jr, Nnur)
                  SCRt(iermax, jr, ip2, Nejc)
     &               = SCRt(iermax, jr, ip2, Nejc)
     &               + sumtl2*RO(iermax, jr, Nnur)
               ENDIF
C--------------decay to the highest but one bin (conditional see the next IF)
               IF(ZEJc(Nejc).EQ.0.0D0 .AND. Iec.EQ.NEX(Nnuc) - 1)THEN
                  lmax = lmaxf
                  lmax = MIN0(LMAxtl(6, Nejc, Nnur), lmax)
C-----------------CORR in the next lines accounts for the Tl interpolation
C-----------------and integration over overlaping bins (2/3), it turned out it must
C-----------------be energy step and also emission step dependent
                  corr = 0.4444/(DE - XN(Nnur) + XN(1))
                  rho = RO(iermax, jr, Nnur)*DE*corr*TUNe(Nejc, Nnuc)
C-----------------do loop over l (odd and even l-values treated separately)
C-----------------IP1 and IP2 decide which parity each SUMTL  goes to
                  sumtl1 = 0.0
                  DO l = lmin, lmax, 2       ! do loop over l
                     IF(Nhrtw.GT.0)THEN
C-----------------------replace Tl with V in the second HRTW entry
                        sumtl1 = sumtl1 + VT(TL(6, l, Nejc, Nnur))
                     ELSE
C-----------------------first entry with HRTW
C                       WRITE(6,*)'C Tl= ' , TL(6,l,Nejc,Nnur)
                        CALL TL2VL(TL(6, l, Nejc, Nnur), rho)
                        sumtl1 = sumtl1 + TL(6, l, Nejc, Nnur)
                     ENDIF
                  ENDDO
                  sumtl2 = 0.0
                  DO l = lmin + 1, lmax, 2
                     IF(Nhrtw.GT.0)THEN
C-----------------------replace Tl with V in the second HRTW entry
                        sumtl2 = sumtl2 + VT(TL(6, l, Nejc, Nnur))
                     ELSE
C-----------------------first entry with HRTW
C                       WRITE(6,*)'D Tl= ' , TL(6,l,Nejc,Nnur)
                        CALL TL2VL(TL(6, l, Nejc, Nnur), rho)
                        sumtl2 = sumtl2 + TL(6, l, Nejc, Nnur)
                     ENDIF
                  ENDDO                      ! over l
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
                  IF(ier.EQ.1)THEN
                     corr = 0.5
                  ELSE
                     corr = 1.0
                  ENDIF
                  rho = RO(ier, jr, Nnur)*DE*corr*TUNe(Nejc, Nnuc)
C-----------------do loop over l (odd and even l-values treated separately)
C-----------------IP1 and IP2 decide which parity each SUMTL  goes to
                  sumtl1 = 0.0
                  DO l = lmin, lmax, 2
                     IF(Nhrtw.GT.0)THEN
C-----------------------replace Tl with V in the second HRTW entry
                        sumtl1 = sumtl1 + VT(TL(ietl, l, Nejc, Nnur))
                     ELSE
C-----------------------first entry with HRTW
C                       WRITE(6,*)'E Tl= ' , TL(ietl,l,Nejc,Nnur)
                        CALL TL2VL(TL(ietl, l, Nejc, Nnur), rho)
                        sumtl1 = sumtl1 + TL(ietl, l, Nejc, Nnur)
                     ENDIF
                  ENDDO
                  sumtl2 = 0.0
                  DO l = lmin + 1, lmax, 2
                     IF(Nhrtw.GT.0)THEN
C-----------------------replace Tl with V in the second HRTW entry
                        sumtl2 = sumtl2 + VT(TL(ietl, l, Nejc, Nnur))
                     ELSE
C-----------------------first entry with HRTW
C                       WRITE(6,*)'F Tl= ' , TL(ietl,l,Nejc,Nnur)
                        CALL TL2VL(TL(ietl, l, Nejc, Nnur), rho)
                        sumtl2 = sumtl2 + TL(ietl, l, Nejc, Nnur)
                     ENDIF
                  ENDDO
C-----------------do loop over l   ***done***
C
                  SCRt(ier, jr, ip1, Nejc) = SCRt(ier, jr, ip1, Nejc)
     &               + sumtl1*RO(ier, jr, Nnur)
                  SCRt(ier, jr, ip2, Nejc) = SCRt(ier, jr, ip2, Nejc)
     &               + sumtl2*RO(ier, jr, Nnur)
               ENDDO               ! over r.n. energies
            ENDDO           ! over channel spins
         ENDDO        ! over and r.n. spins
C--------trapezoidal integration of ro*tl in continuum for ejectile nejc
         DO j = 1, NLW, LTUrbo
            DO i = 1, iermax
               Sum = Sum + SCRt(i, j, 1, Nejc) + SCRt(i, j, 2, Nejc)
            ENDDO
            Sum = Sum - 0.5*(SCRt(1, j, 1, Nejc) + SCRt(1, j, 2, Nejc))
         ENDDO
         Sum = Sum*DE
         IF(Nhrtw.GT.0)THEN
            DO j = 1, NLW
               DO i = 1, NEX(Nnur) + 1
                  SCRt(i, j, 1, Nejc) = SCRt(i, j, 1, Nejc)
                  SCRt(i, j, 2, Nejc) = SCRt(i, j, 2, Nejc)
               ENDDO
            ENDDO
         ENDIF
C--------integration of ro*tl in continuum for ejectile nejc -- done ----
C--------
C--------decay to discrete levels
C--------
      ENDIF
      IF(RORed.NE.0.0D0)THEN
         DO i = 1, NLV(Nejc)
            SCRtl(i, Nejc) = 0.0
         ENDDO
         IF(Nhrtw.EQ.0 .AND. IZA(Nnur).EQ.IZA(0))THEN
C--------clear memorized elastic channels when entering new J-pi CN state
            iel = 1
            DO i = 1, NDHRTW2
               MEMel(i, 1) = 0
               MEMel(i, 2) = 0
               MEMel(i, 3) = 0
            ENDDO
         ENDIF
         eoutc = EX(Iec, Nnuc) - Q(Nejc, Nnuc)
C--------
C--------do loop over discrete levels -----------------------------------
C--------
         DO i = 1, NLV(Nnur)
            eout = eoutc - ELV(i, Nnur)
            cor = TUNe(Nejc, Nnuc)
            IF(i.EQ.1)cor = 1.0
C-----------level above the bin
            IF(eout.LT.0.0D0)GOTO 100
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
               IF(i.EQ.1 .AND. IZA(Nnur).EQ.IZA(0))THEN
                  tld = ELTl(l)
               ELSE
                  tld = TL(il, l, Nejc, Nnur)
     &                  + frde*(TL(il + 1, l, Nejc, Nnur)
     &                  - TL(il, l, Nejc, Nnur))
               ENDIF
               IF(ipar.NE.0 .AND. tld.GT.0.0D0)THEN
                  IF(Nhrtw.GT.0)THEN
C--------------------entry with nhrtw>0
                     sumdl = sumdl + VT(tld)*cor
                  ELSE
C--------------------entry with nhrtw=0
                     CALL TL2VL(tld, cor)
                     sumdl = sumdl + tld*cor
C                    WRITE(6,*)'sumdl,tld,cor ',sumdl,tld,cor
                     IF(i.EQ.1 .AND. IZA(Nnur).EQ.IZA(0) .AND. 
     &                  tld.GT.H_Tthr)THEN
C-----------------------case of a strong elastic channel
C-----------------------record position of Tl, l and channel spin
                        MEMel(iel, 1) = NH_lch
                        MEMel(iel, 2) = l
                        MEMel(iel, 3) = INT(2.0*s)
C                       WRITE(6,*)'got elastic iel ', iel,
C                       &                     '  MEM# ',MEMel(iel,1),
C                       &                     '  MEMk ',MEMel(iel,2),
C                       &                     '  MEM2s ',MEMel(iel,3)
                        iel = iel + 1
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO
C-----------do loop over l --- done ----------------------------------------
            s = s + 1.
            IF(s.LE.smax)GOTO 20
C-----------loop over channel spin ------ done ----------------------------
            sumdl = sumdl*RORed
            SCRtl(i, Nejc) = sumdl
            Sum = Sum + sumdl
C           WRITE(6,*)'i,sumdl,nejc,nhrtw ', i,sumdl,nejc,nhrtw
         ENDDO
C--------do loop over discrete levels --------- done --------------------
      ENDIF
 100  DENhf = DENhf + Sum
      SCRtem(Nejc) = Sum
Cpr   WRITE(6,*) 'TOTAL SUM=',SUM
C--------decay to the continuum ------ done -----------------------------
      END
C
C
      SUBROUTINE HRTW_DECAYG(Nnuc, Iec, Jc, Ipc, Sum, Nhrtw)
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
Ccc   *       nhrtw- 0 first HRTW entry (calculate Tl's summs)           *
Ccc   *             >0 final HRTW entries (actually not used)            *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * output:SUM - SUM of transmission coefficients over all gamma     *
Ccc   *              channels.                                           *
Ccc   *              Apart of this standard feature it comunicates       *
Ccc   *              quantities needed for the HRTW theory through       *
Ccc   *              the HRTW common.  Note that gamma channels          *
Ccc   *              contribute to the sum of weak channels but their    *
Ccc   *              transmission coefficients are assumed to remain     *
Ccc   *              constant when HRTW is applied.                      *
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
C COMMON variables
C
      DOUBLE PRECISION H_Abs(NDHRTW2, 3), H_Sumtl, H_Sumtls, H_Sweak, 
     &                 H_Tav, H_Tl(NDHRTW1, 2), H_Tthr, TFIs
      COMMON /RHRTW / H_Tl, H_Sumtl, H_Sumtls, H_Sweak, H_Abs, H_Tav, 
     &                H_Tthr, TFIs
C
C Dummy arguments
C
      INTEGER Iec, Ipc, Jc, Nhrtw, Nnuc
      DOUBLE PRECISION Sum
C
C Local variables
C
      DOUBLE PRECISION corr, e1t, e2t, eg, se1, se1s, se2, se2m1, 
     &                 se2m1s, se2s, sm1, sm1s, xjc, xm1t
      DOUBLE PRECISION E1, E2, VT1, XM1
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
C-----clear scratch matrix (dNH_lchrete levels)
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
         IF(ier.EQ.1)THEN
            corr = 0.5
         ELSE
            corr = 1.0
         ENDIF
         eg = EX(Iec, Nnuc) - EX(ier, Nnuc)
         IF(Nhrtw.EQ.0)THEN
            se1 = E1(eg, TNUc(ier, Nnuc))*TUNe(0, Nnuc)
            se2 = E2(eg)*TUNe(0, Nnuc)
            sm1 = XM1(eg)*TUNe(0, Nnuc)
            se2m1 = se2 + sm1
            se1s = se1**2
            se2s = se2**2
            sm1s = sm1**2
            se2m1s = se2**2 + sm1**2
         ELSE
            se1 = VT1(E1(eg, TNUc(ier, Nnuc))*
     &            TUNe(0, Nnuc), H_Tav, H_Sumtl)
            se2 = VT1(E2(eg)*TUNe(0, Nnuc), H_Tav, H_Sumtl)
            sm1 = VT1(XM1(eg)*TUNe(0, Nnuc), H_Tav, H_Sumtl)
            se2m1 = se2 + sm1
         ENDIF
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
C-----------first entry with HRTW
            IF(Nhrtw.EQ.0)THEN
               H_Sumtls = H_Sumtls + se2s*RO(ier, Jc - 2, Nnuc)*corr
               H_Sumtls = H_Sumtls + se2m1s*RO(ier, Jc - 1, Nnuc)*corr
               H_Sumtls = H_Sumtls + se2m1s*RO(ier, Jc, Nnuc)*corr
               H_Sumtls = H_Sumtls + se2m1s*RO(ier, Jc + 1, Nnuc)*corr
               H_Sumtls = H_Sumtls + se2s*RO(ier, Jc + 2, Nnuc)*corr
               H_Sumtls = H_Sumtls + se1s*RO(ier, Jc - 1, Nnuc)*corr
               H_Sumtls = H_Sumtls + se1s*RO(ier, Jc, Nnuc)*corr
               H_Sumtls = H_Sumtls + se1s*RO(ier, Jc + 1, Nnuc)*corr
            ENDIF    !first HRTW entry done
            GOTO 100
         ENDIF
C--------decaying state spin index = 2
         IF(Jc.EQ.2)THEN
            IF(ABS(xjc - 1.5).LT.0.001D0)THEN
               SCRt(ier, Jc - 1, ipos, 0) = se2m1*RO(ier, Jc - 1, Nnuc)
               IF(Nhrtw.EQ.0)H_Sumtls = H_Sumtls + 
     &                                  se2m1s*RO(ier, Jc - 1, Nnuc)
     &                                  *corr
            ELSE
               SCRt(ier, Jc - 1, ipos, 0) = sm1*RO(ier, Jc - 1, Nnuc)
               IF(Nhrtw.EQ.0)H_Sumtls = H_Sumtls + 
     &                                  sm1s*RO(ier, Jc - 1, Nnuc)*corr
            ENDIF
            SCRt(ier, Jc, ipos, 0) = se2m1*RO(ier, Jc, Nnuc)
            SCRt(ier, Jc + 1, ipos, 0) = se2m1*RO(ier, Jc + 1, Nnuc)
            SCRt(ier, Jc + 2, ipos, 0) = se2*RO(ier, Jc + 2, Nnuc)
            SCRt(ier, Jc - 1, ineg, 0) = se1*RO(ier, Jc - 1, Nnuc)
            SCRt(ier, Jc, ineg, 0) = se1*RO(ier, Jc, Nnuc)
            SCRt(ier, Jc + 1, ineg, 0) = se1*RO(ier, Jc + 1, Nnuc)
            IF(Nhrtw.EQ.0)THEN
               H_Sumtls = H_Sumtls + se2m1s*RO(ier, Jc, Nnuc)*corr
               H_Sumtls = H_Sumtls + se2m1s*RO(ier, Jc + 1, Nnuc)*corr
               H_Sumtls = H_Sumtls + se2s*RO(ier, Jc + 2, Nnuc)*corr
               H_Sumtls = H_Sumtls + se1s*RO(ier, Jc - 1, Nnuc)*corr
               H_Sumtls = H_Sumtls + se1s*RO(ier, Jc, Nnuc)*corr
               H_Sumtls = H_Sumtls + se1s*RO(ier, Jc + 1, Nnuc)*corr
            ENDIF
            GOTO 100
         ENDIF
C--------decaying state spin = 1/2
         IF(ABS(xjc - 0.5).LT.0.001D0)THEN
            SCRt(ier, Jc, ipos, 0) = sm1*RO(ier, Jc, Nnuc)
            SCRt(ier, Jc + 1, ipos, 0) = se2m1*RO(ier, Jc + 1, Nnuc)
            SCRt(ier, Jc + 2, ipos, 0) = se2*RO(ier, Jc + 2, Nnuc)
            SCRt(ier, Jc, ineg, 0) = se1*RO(ier, Jc, Nnuc)
            SCRt(ier, Jc + 1, ineg, 0) = se1*RO(ier, Jc + 1, Nnuc)
            IF(Nhrtw.EQ.0)THEN
               H_Sumtls = H_Sumtls + sm1s*RO(ier, Jc, Nnuc)*corr
               H_Sumtls = H_Sumtls + se2m1s*RO(ier, Jc + 1, Nnuc)*corr
               H_Sumtls = H_Sumtls + se2s*RO(ier, Jc + 2, Nnuc)*corr
               H_Sumtls = H_Sumtls + se1s*RO(ier, Jc, Nnuc)*corr
               H_Sumtls = H_Sumtls + se1s*RO(ier, Jc + 1, Nnuc)*corr
            ENDIF
            GOTO 100
         ENDIF
C--------decaying state spin = 0
         IF(ABS(xjc - 0.).LT.0.001D0)THEN
            SCRt(ier, Jc + 1, ipos, 0) = sm1*RO(ier, Jc + 1, Nnuc)
            SCRt(ier, Jc + 2, ipos, 0) = se2*RO(ier, Jc + 2, Nnuc)
            SCRt(ier, Jc + 1, ineg, 0) = se1*RO(ier, Jc + 1, Nnuc)
            IF(Nhrtw.EQ.0)THEN
               H_Sumtls = H_Sumtls + sm1s*RO(ier, Jc + 1, Nnuc)*corr
               H_Sumtls = H_Sumtls + se2s*RO(ier, Jc + 2, Nnuc)*corr
               H_Sumtls = H_Sumtls + se1s*RO(ier, Jc + 1, Nnuc)*corr
            ENDIF
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
            IF(Nhrtw.EQ.0)THEN
               H_Sumtls = H_Sumtls + se2s*RO(ier, Jc - 2, Nnuc)*corr
               H_Sumtls = H_Sumtls + se2m1s*RO(ier, Jc - 1, Nnuc)*corr
               H_Sumtls = H_Sumtls + se2m1s*RO(ier, Jc, Nnuc)*corr
               H_Sumtls = H_Sumtls + se2m1s*RO(ier, Jc + 1, Nnuc)*corr
               H_Sumtls = H_Sumtls + se1s*RO(ier, Jc - 1, Nnuc)*corr
               H_Sumtls = H_Sumtls + se1s*RO(ier, Jc, Nnuc)*corr
               H_Sumtls = H_Sumtls + se1s*RO(ier, Jc + 1, Nnuc)*corr
            ENDIF
            GOTO 100
         ENDIF
C--------decaying state spin index = NLW
         IF(Jc.EQ.NLW)THEN
            SCRt(ier, Jc - 2, ipos, 0) = se2*RO(ier, Jc - 2, Nnuc)
            SCRt(ier, Jc - 1, ipos, 0) = se2m1*RO(ier, Jc - 1, Nnuc)
            SCRt(ier, Jc, ipos, 0) = se2m1*RO(ier, Jc, Nnuc)
            SCRt(ier, Jc - 1, ineg, 0) = se1*RO(ier, Jc - 1, Nnuc)
            SCRt(ier, Jc, ineg, 0) = se1*RO(ier, Jc, Nnuc)
            IF(Nhrtw.EQ.0)THEN
               H_Sumtls = H_Sumtls + se2s*RO(ier, Jc - 2, Nnuc)*corr
               H_Sumtls = H_Sumtls + se2m1s*RO(ier, Jc - 1, Nnuc)*corr
               H_Sumtls = H_Sumtls + se2m1s*RO(ier, Jc, Nnuc)*corr
               H_Sumtls = H_Sumtls + se1s*RO(ier, Jc - 1, Nnuc)*corr
               H_Sumtls = H_Sumtls + se1s*RO(ier, Jc, Nnuc)*corr
            ENDIF
         ENDIF
 100  ENDDO
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
               IF(Nhrtw.EQ.0)THEN
                  e2t = E2(eg)*TUNe(0, Nnuc)
                  e1t = E1(eg, TNUc(1,Nnuc))*TUNe(0, Nnuc)
                  xm1t = XM1(eg)*TUNe(0, Nnuc)
               ELSE
                  e2t = VT1(E2(eg)*TUNe(0, Nnuc), H_Tav, H_Sumtl)
                  e1t = VT1(E1(eg, TNUc(1,Nnuc))*
     &                  TUNe(0, Nnuc), H_Tav, H_Sumtl)
                  xm1t = VT1(XM1(eg)*TUNe(0, Nnuc), H_Tav, H_Sumtl)
               ENDIF
               IF(lmin.EQ.2)THEN
                  SCRtl(i, 0) = SCRtl(i, 0) + e2t*FLOAT(ipar)
                  IF(Nhrtw.EQ.0)H_Sumtls = H_Sumtls + e2t**2*FLOAT(ipar)
               ELSE
                  SCRtl(i, 0) = E1(eg, TNUc(1,Nnuc))*iodd + XM1(eg)*ipar
                  IF(Nhrtw.EQ.0)H_Sumtls = H_Sumtls + e1t**2*iodd + 
     &               xm1t**2*ipar
                  IF(lmax.NE.1)THEN
                     SCRtl(i, 0) = SCRtl(i, 0) + e2t*FLOAT(ipar)
                     IF(Nhrtw.EQ.0)H_Sumtls = H_Sumtls + 
     &                  e2t**2*FLOAT(ipar)
                  ENDIF
               ENDIF
               SCRtl(i, 0) = SCRtl(i, 0)*RORed
               Sum = Sum + SCRtl(i, 0)
            ENDIF
         ENDDO
C-----do loop over discrete levels --------- done --------------------
      ENDIF
      SCRtem(0) = Sum
      DENhf = DENhf + Sum
C-----do loop over c.n. energies ***done***
C-----decay to the continuum ----** done***---------------------------
      END
C
C
      SUBROUTINE TL2VL(T, Rho)
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:PPu*
Ccc   *                      T L 2 V L                                   *
Ccc   *                                                                  *
Ccc   * Service routine for HRTW. It stores strong transmission          *
Ccc   * coefficients, calculates sum of Tl**2, and sum of weak Tl's.     *
Ccc   * On entry with nhrtw>0 returns V that substitute Tl in the HF     *
Ccc   * formula.                                                         *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * input:T    - transmission coefficient                            *
Ccc   *       Rho  - number of channels with this transmission coeff.    *
Ccc   *                                                                  *
Ccc   * output:      quantities needed for the HRTW theory (through      *
Ccc   *              the HRTW common):                                   *
Ccc   *                                                                  *
Ccc   *             H_Tl(.,.) - list of strong Tl's and their num. (rho) *
Ccc   *             H_Tloc(.,.) - l and ejectile for each strong Tl      *
Ccc   *             H_sumtls - sum of squared Tl's (all)                 *
Ccc   *             H_sweak - sum of weak Tl's (not squared)             *
Ccc   *             H_lch - number of strong Tl's                        *
Ccc   *             H_abs(.,.) - absorption x-sec decomposed in l's      *
Ccc   *             H_Tav - average transmission coefficient             *
Ccc   *             H_Tthr - threshold to consider Tl as strong          *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * calls:                                                           *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * author: M.Herman                                                 *
Ccc   * date:   10.Sept.2000                                             *
Ccc   * revision:#    by:name                     on:xx.mon.199x         *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
C
C
C COMMON variables
C
      DOUBLE PRECISION H_Abs(NDHRTW2, 3), H_Sumtl, H_Sumtls, H_Sweak, 
     &                 H_Tav, H_Tl(NDHRTW1, 2), H_Tthr, TFIs
      INTEGER MEMel(NDHRTW2, 3), NDIvf, NH_lch, NSCh
      COMMON /IHRTW / NSCh, NDIvf, MEMel, NH_lch
      COMMON /RHRTW / H_Tl, H_Sumtl, H_Sumtls, H_Sweak, H_Abs, H_Tav, 
     &                H_Tthr, TFIs
C
C Dummy arguments
C
      DOUBLE PRECISION Rho, T
C
C
      H_Sumtls = H_Sumtls + T**2*Rho
      IF(T.GT.H_Tthr)THEN
C        !strong or weak
         NH_lch = NH_lch + 1
         IF(NH_lch.LE.NDHRTW1)THEN
            H_Tl(NH_lch, 1) = T     !record strong Tl's
            H_Tl(NH_lch, 2) = Rho
         ENDIF
      ELSE
         H_Sweak = H_Sweak + T*Rho
C        WRITE(6,*)'weak T,Rho,H_Sweak ',T,Rho,H_Sweak
      ENDIF
      END
C
      DOUBLE PRECISION FUNCTION VT(Tl)
      INCLUDE 'dimension.h'
C
C
C COMMON variables
C
      DOUBLE PRECISION H_Abs(NDHRTW2, 3), H_Sumtl, H_Sumtls, H_Sweak, 
     &                 H_Tav, H_Tl(NDHRTW1, 2), H_Tthr, TFIs
      INTEGER MEMel(NDHRTW2, 3), NDIvf, NH_lch, NSCh
      COMMON /IHRTW / NSCh, NDIvf, MEMel, NH_lch
      COMMON /RHRTW / H_Tl, H_Sumtl, H_Sumtls, H_Sweak, H_Abs, H_Tav, 
     &                H_Tthr, TFIs
C
C Dummy arguments
C
      DOUBLE PRECISION Tl
C
C Local variables
C
      DOUBLE PRECISION VT1
C
C
C
C     Returns value of the HRTW quantity V corresponding to the
C     provided transmission coefficient (if the
C     latter is below the threshold value H_Tthr or total number
C     of strong channels is bigger that NDHRTW1). If number of
C     strong channels is lower than NDHRTW1 and Tl is above the
C     threshold value H_Tthr then precalculated H_Tl(nsch) is returned.
C     Note that VT keeps track of the number of processed strong
C     channels in the variable NSCh.
C
C     input: Tl    - transmission coefficient
C
C     output: Tl
C
C
      IF(NH_lch.GT.NDHRTW1)THEN
         VT = VT1(Tl, H_Tav, H_Sumtl)
      ELSEIF(Tl.LT.H_Tthr)THEN
         VT = VT1(Tl, H_Tav, H_Sumtl)
      ELSE
         NSCh = NSCh + 1
         VT = H_Tl(NSCh, 1)
      ENDIF
      END
C
C
C
      DOUBLE PRECISION FUNCTION VT1(Tl, Tav, Sumtl)
C
C
C Dummy arguments
C
      DOUBLE PRECISION Sumtl, Tav, Tl
C
C Local variables
C
      DOUBLE PRECISION EEF
C
C
C
C     calculates V quantities replacing Tl's in the HRTW theory.
C     Performs just one iteration. To be used when number of
C     strong channels is bigger than NDRTW1.
C
C     input: Tl    - transmission coefficient
C     Tav   - average transmission coefficeint
C     Sumtl - sum of transmission coefficients
C
C     output: VT1
C
C
      VT1 = 0.0
      IF(Sumtl.EQ.0.0D0)RETURN
      VT1 = Tl/Sumtl
      VT1 = 1 + VT1*(EEF(Tl, Tav, Sumtl) - 1.0)
      VT1 = Tl/VT1
      END
C
C
C
      DOUBLE PRECISION FUNCTION EEF(Tl, Tav, Sumtl)
C
C
C Dummy arguments
C
      DOUBLE PRECISION Sumtl, Tav, Tl
C
C Local variables
C
      DOUBLE PRECISION a, al
C
C
C
C     calculates elastic enhancement factor for HRTW theory
C
C     input: Tl     - transmission coefficient
C     Tav    - average of all transmission coefficients
C     Sumtl  - sum of transmission coefficients
C
C     output: Eef - elastic enhancement factor
C
      IF(Tl.LT.1.0D-10)THEN
         EEF = 3.0
         RETURN
      ENDIF
      al = 4.*Tav/Sumtl*(1. + Tl/Sumtl)/(1. + 3.*Tav/Sumtl)
      a = 87.0*(Tl - Tav)**2*Tl**5/Sumtl**7
      EEF = 1.0 + 2.0/(1.0 + Tl**al) + a
      END
C
C
      SUBROUTINE AUSTER(V, Tav, Sumtl, Sweak, Lch, Ndhrtw1)
C
C
C     iterates for V quantities in HRTW theory assuming that weak
C     transmission coefficients remain constant and therefore are
C     not iterated.
C
C     input: Tav   - average transmission coefficient
C     Sumtl - sum of all transmission coefficients
C     Sweak - sum of weak (Tl<H_Tthr) transmission coefficients
C     Lch   - number of strong (Tl>H_Tthr) channels
C
C     input/output: V    - matrix of Tl's (input) or V's (output)
C
C
C
C Dummy arguments
C
      INTEGER Lch, Ndhrtw1
      DOUBLE PRECISION Sumtl, Sweak, Tav
      DOUBLE PRECISION V(Ndhrtw1, 2)
C
C Local variables
C
      DOUBLE PRECISION e(Ndhrtw1), sum, sv, vd(Ndhrtw1), vp(Ndhrtw1)
      DOUBLE PRECISION EEF
      INTEGER i, icount
C
      IF(Lch.GT.Ndhrtw1)THEN
         WRITE(6, *)
     &             'ERROR in AUSTER: Lch bigger than allowed by NDHRTW1'
         WRITE(6, *)'If you see this printed it means a BUG!'
         STOP
      ENDIF
      icount = 0
      sv = Sweak
      sum = Sweak
C     WRITE(6,*)'Sweak ',Sweak
      DO i = 1, Lch
         e(i) = EEF(V(i, 1), Tav, Sumtl) - 1.
         sum = sum + V(i, 1)*V(i, 2)
C        WRITE(6,*)'Tl, rho, sum ',V(i,1),V(i,2),sum
      ENDDO
C     WRITE(6,*)'Sumtl, sum AUSTER ',Sumtl,sum
      DO i = 1, Lch
         vd(i) = V(i, 1)
         vp(i) = V(i, 1)
      ENDDO
 100  DO i = 1, Lch
         vp(i) = V(i, 1)/(1.0 + vp(i)*e(i)/sum)
         sv = sv + vp(i)*V(i, 2)
      ENDDO
      icount = icount + 1
      IF(icount.GT.1000)THEN
         WRITE(6, *)' Maximum iteration number reached in AUSTER'
         RETURN
      ENDIF
      sum = sv
      sv = Sweak
C     WRITE(6,*)'AUSTER iteration ',(vp(i),i=1,Lch)
      DO i = 1, Lch
C
C        relative accuracy of V is set below and may be altered
C        to any resonable value.  1.D-99 avoids division by 0.
C
         IF((ABS(vd(i)-vp(i))/(vp(i)+1.D-99)).GT.1.0D-6)GOTO 200
      ENDDO
      DO i = 1, Lch
         V(i, 1) = vp(i)
      ENDDO
      RETURN
 200  DO i = 1, Lch
         vd(i) = vp(i)
      ENDDO
      GOTO 100
      END
C
C
C
C
      SUBROUTINE ELCORR(Nnuc, Iec, Jc, Ipc, Nnur, Nejc, Nhrtw)
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:PPu*
Ccc   *                      E L C O R R                                 *
Ccc   *                                                                  *
Ccc   * Corrects elastic widths for the elastic enhancement by adding    *
Ccc   * appropriate addditional part to scartch matrices.                *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * input:NNUC - decaying nucleus index                              *
Ccc   *       IEC  - energy index of the decaying state                  *
Ccc   *       JC   - spin index of the decaying state                    *
Ccc   *       IPC  - parity of the decaying state                        *
Ccc   *       NNUR - residual nucleus index                              *
Ccc   *       NEJC - ejectile index                                      *
Ccc   *       nhrtw- 0 MUST NOT be used in this routine                  *
Ccc   *             >0 final HRTW entry (calculate emission widths)      *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * calls:TLLOC                                                      *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * author: M.Herman                                                 *
Ccc   * date:   28.Sep.2000                                              *
Ccc   * revision:#    by:name                     on:xx.mon.199x         *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C
C COMMON variables
C
      DOUBLE PRECISION ELTl(NDLW), H_Abs(NDHRTW2, 3), H_Sumtl, H_Sumtls, 
     &                 H_Sweak, H_Tav, H_Tl(NDHRTW1, 2), H_Tthr, TFIs
      INTEGER MEMel(NDHRTW2, 3), NDIvf, NH_lch, NSCh
      COMMON /ELASTIC/ ELTl
      COMMON /IHRTW / NSCh, NDIvf, MEMel, NH_lch
      COMMON /RHRTW / H_Tl, H_Sumtl, H_Sumtls, H_Sweak, H_Abs, H_Tav, 
     &                H_Tthr, TFIs
C
C Dummy arguments
C
      INTEGER Iec, Ipc, Jc, Nejc, Nhrtw, Nnuc, Nnur
C
C Local variables
C
      DOUBLE PRECISION eout, eoutc, frde, popadd, s, smax, smin, tld, v, 
     &                 xjc
C     DOUBLE PRECISION cor
      DOUBLE PRECISION EEF, VT1
      REAL FLOAT
      INTEGER i, iel, il, ipar, kel, l, lmax, lmin
      INTEGER INT, MIN0
C
C
C
C     WRITE(6,*)'  '
      xjc = FLOAT(Jc) + HIS(Nnuc)
C--------
C--------decay to discrete levels
C--------
      IF(RORed.NE.0.0D0)THEN
         eoutc = EX(Iec, Nnuc) - Q(Nejc, Nnuc)
C--------only ground state considered
         i = 1
         eout = eoutc - ELV(i, Nnur)
         IF(eout.LT.DE)THEN
C--------level above the bin
            IF(eout.LT.0.0D0)GOTO 99999
         ENDIF
         CALL TLLOC(Nnur, Nejc, eout, il, frde)
         smin = ABS(XJLv(i, Nnur) - SEJc(Nejc))
         smax = XJLv(i, Nnur) + SEJc(Nejc) + 0.01
         s = smin
C--------loop over channel spin ----------------------------------------
 50      lmin = INT(ABS(xjc - s) + 1.01)
         lmax = INT(xjc + s + 1.01)
         lmax = MIN0(NLW, lmax)
C--------do loop over l ------------------------------------------------
         DO l = lmin, lmax
            ipar = 1 + LVP(i, Nnur)*Ipc*( - 1)**(l - 1)
            tld = ELTl(l)
C           WRITE(6,*)'l tl, ipar, s ',l,tld,ipar, s
            IF(l.EQ.INT(H_Abs(Nhrtw,2)) .AND. (2.0*s).EQ.H_Abs(Nhrtw, 3)
     &         .AND. ipar.NE.0)THEN
C--------------got a true elastic channel
               IF(tld.GT.H_Tthr .AND. NH_lch.LE.NDHRTW1)THEN
                  DO iel = 1, NDHRTW2
                     IF(MEMel(iel, 3).EQ.INT(2.0*s + 0.001) .AND. 
     &                  MEMel(iel, 2).EQ.l)THEN
                        kel = MEMel(iel, 1)
                        v = H_Tl(kel, 1)
C                       WRITE(6,*)'got strong el. for correction ',v
                     ENDIF
                  ENDDO
               ELSE
                  v = VT1(tld, H_Tav, H_Sumtl)
C                 WRITE(6,*)'got weak elastic for correction',v
               ENDIF
               popadd = v*(EEF(tld, H_Tav, H_Sumtl) - 1.0)
               SCRtl(i, Nejc) = SCRtl(i, Nejc) + popadd
               SCRtem(Nejc) = SCRtem(Nejc) + popadd
C              WRITE(6,*)'TL, VT, EEF, popadd ',Tld, v,
C              &             EEF(Tld,H_Tav,H_Sumtl),popadd
            ENDIF
         ENDDO
C--------do loop over l --- done ----------------------------------------
         s = s + 1.
         IF(s.LE.smax)GOTO 50
C--------loop over channel spin ------ done ----------------------------
      ENDIF
99999 END
C
C
C
      SUBROUTINE HRTW_MARENG(Npro, Ntrg, Jcn, Ip, Ich)
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:ppu*
Ccc   *                H R T W _ M A R E N G                             *
Ccc   *                                                                  *
Ccc   * Calculates initial compound nucleus population after projectile  *
Ccc   * absorption  using transmission coefficients calculated in MARENG *
Ccc   * for elastic channel.                                             *
Ccc   *                                                                  *
Ccc   * input:Npro - projectile index (normally 0)                       *
Ccc   *       Ntrg - target index (normally 0)                           *
Ccc   *       Jcn  - Compound Nucleus spin                               *
Ccc   *       Ip   - Compound Nucleus parity                             *
Ccc   *                                                                  *
Ccc   * output: Ich  - number of physical channels contributing to JCN Ip*
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * author: M.Herman                                                 *
Ccc   * date:   30.Aug.2000                                              *
Ccc   * revision:1    by:xxxxxx                   on:  .xxx.xxxx         *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C
C COMMON variables
C
      DOUBLE PRECISION ELTl(NDLW), H_Abs(NDHRTW2, 3), H_Sumtl, H_Sumtls, 
     &                 H_Sweak, H_Tav, H_Tl(NDHRTW1, 2), H_Tthr, TFIs
      INTEGER MEMel(NDHRTW2, 3), NDIvf, NH_lch, NSCh
      COMMON /ELASTIC/ ELTl
      COMMON /IHRTW / NSCh, NDIvf, MEMel, NH_lch
      COMMON /RHRTW / H_Tl, H_Sumtl, H_Sumtls, H_Sweak, H_Abs, H_Tav, 
     &                H_Tthr, TFIs
C
C Dummy arguments
C
      INTEGER Ich, Ip, Jcn, Npro, Ntrg
C
C Local variables
C
      DOUBLE PRECISION chsp, coef, eee, s1, smax, smin, vl, wf
      REAL FLOAT
      INTEGER i, ichsp, iel, ipa, k, kel, l, lmax, lmin, maxlw, mul
      INTEGER INT, MIN0
      DOUBLE PRECISION PAR, xmas_npro, xmas_ntrg, RMU
      DOUBLE PRECISION VT1
C
C
      PAR(i, ipa, l) = 0.5*(1.0 - ( - 1.0)**i*ipa*( - 1.0)**l)
C
      xmas_npro = (AEJc(Npro)*AMUmev + XMAss_ej(Npro))/AMUmev
      xmas_ntrg = (A(Ntrg)*AMUmev + XMAss(Ntrg))/AMUmev
C     rmu = xmas_npro*xmas_ntrg/(xmas_npro + xmas_ntrg)
C
C     wf = W2*EIN*rmu
C     coef = PI/wf/(2*XJLv(1, Ntrg) + 1.0)/(2*SEJc(Npro) + 1.0)
C
C     wf = .0047837*AEJc(Npro)*A(Ntrg)*EIN/(AEJc(Npro)+A(Ntrg))
C     coef = 3.14159/wf/(2*XJLv(1,Ntrg)+1.0)/(2*SEJc(Npro)+1.0)
C
      el = EINl
      CALL KINEMA(el, ecms, xmas_npro, xmas_ntrg, RMU, ak2, 1, RELkin)
C     wf = W2*ecms*rmu
      wf = ak2/10.D0
      coef = PI/wf/(2*XJLv(1, Ntrg) + 1.0)/(2*SEJc(Npro) + 1.0)
C
      maxlw = NDLW
      s1 = 0.5
      IF(AINT(XJLv(1,Ntrg) + SEJc(Npro)) - XJLv(1, Ntrg) - SEJc(Npro)
     &   .EQ.0.0D0)s1 = 1.0
      CSFus = 0.0
C-----channel spin min and max
      eee = SEJc(Npro) - XJLv(1, Ntrg)
      smin = ABS(eee)
      smax = SEJc(Npro) + XJLv(1, Ntrg)
      mul = smax - smin + 1.0001
      CSFus = 0.0
      Ich = 1
      DO ichsp = 1, mul
         chsp = smin + FLOAT(ichsp - 1)
C        WRITE(6,*)'channel spin ',chsp
         lmin = ABS(Jcn - chsp - s1) + 0.0001
         lmax = Jcn + chsp - s1 + 0.0001
         lmin = lmin + 1
         lmax = lmax + 1
         lmax = MIN0(NDLW, lmax)
         lmax = MIN0(maxlw, lmax)
         DO k = lmin, lmax
            IF(PAR(Ip, LVP(1,0), k - 1).NE.0.0D0)THEN
               IF(Ich.GT.NDHRTW2)THEN
                  WRITE(6, *)' '
                  WRITE(6, *)'E R R O R !'
                  WRITE(6, *)
     &                    'INSUFFICIENT DIMENSION FOR HRTW CALCULATIONS'
                  WRITE(6, *)'INCREASE NDHRTW2 IN THE dimension.h'
                  WRITE(6, *)'AND RECOMPILE THE CODE.'
                  WRITE(6, *)'TRY NDHRTW2=', mul**2 + 1
                  STOP
               ENDIF
               IF(NH_lch.GT.NDHRTW1)THEN
                  vl = VT1(ELTl(k), H_Tav, H_Sumtl)
               ELSEIF(ELTl(k).LT.H_Tthr)THEN
                  vl = VT1(ELTl(k), H_Tav, H_Sumtl)
C                 WRITE(6,*)'weak elastic l=',k-1
               ELSE
                  kel = 0
                  DO iel = 1, NDHRTW2
C                    WRITE(6,*)'iel,#,MEMk,MEM2s,k,spin ',
C                    &            iel,' ',MEMel(iel,1),'
C                    ',MEMel(iel,2),MEMel(iel,3), &            '
C                    ',k,INT(2.0*chsp+0.001)
                     IF(MEMel(iel, 3).EQ.INT(2.0*chsp + 0.001) .AND. 
     &                  MEMel(iel, 2).EQ.k)kel = MEMel(iel, 1)
                  ENDDO
                  IF(kel.EQ.0)THEN
                     WRITE(6, *)' '
                     WRITE(6, *)' MISMATCH OF ELASTIC CHANNEL IN HRTW'
                     WRITE(6, *)' REPORT THIS ERROR ALONG WITH RELATED'
                     WRITE(6, *)
     &                        ' INPUT FILE TO: herman@ndsalpha.iaea.org'
                     STOP
                  ENDIF
                  vl = H_Tl(kel, 1)
C--------------these two if's are to be removed
C                 IF(PAR(ip,LVP(1,0),k-1) .NE.0.0d0 )
C                 &         WRITE(6,*) 'Found strong VL for Mareng'
C                 IF(PAR(ip,LVP(1,0),k-1) .NE.0.0d0 )
C                 &         WRITE(6,*) 'Tl, VL, kel, l, 2*chsp  ',ELTL(k),
C                 &           VL, kel, k, int(2.0*chsp)
               ENDIF
               IF(vl.NE.0.0D0)THEN
                  H_Abs(Ich, 1) = vl*coef*(FLOAT(2*Jcn + 1) - 2.0*s1)
     &                            *FUSred*REDmsc(Jcn, Ip)*DRTl(k)
C                 WRITE(6,*) 'JCN, ip, l, 2*chsp, x-sec ', JCN, ip, k,
C                 &          2.0*chsp,H_Abs(ich,1)
                  H_Abs(Ich, 2) = k
                  H_Abs(Ich, 3) = 2.0*chsp
                  Ich = Ich + 1
               ENDIF
            ENDIF
         ENDDO
      ENDDO
      Ich = Ich - 1
      END
