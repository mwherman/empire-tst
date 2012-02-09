Cc   * $Rev: 2526 $
Ccc   * $Author: shoblit $
Ccc   * $Date: 2012-02-09 21:34:11 +0100 (Do, 09 Feb 2012) $
 
      SUBROUTINE EMPIRE

      USE endf_nubars

      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: ABScs, ELAcs, PFIso, RFIso, SINl, SINlcc, SINlcont, 
     &          TFIso, TGIso, TISo, TOTcs
      INTEGER*4, DIMENSION(250) :: BUFfer
      REAL*8, DIMENSION(500) :: EENc, SIGncf
      REAL*8, DIMENSION(NDLw) :: ELTl
      REAL*8, DIMENSION(NFIsbarpnt) :: EPS_1d, VDEf_1d
      INTEGER, DIMENSION(0:2*NFParab) :: IIExtr
      INTEGER*4 :: INDexb, INDexf
      INTEGER :: NEXtr, NPOints, NRNc
      REAL*8, DIMENSION(NDAngecis) :: XCOs
      COMMON /ECISXS/ ELAcs, TOTcs, ABScs, SINl, SINlcc, SINlcont
      COMMON /ELASTIC/ ELTl
      COMMON /FIS_ISO/ TFIso, TGIso, TISo, RFIso, PFIso
      COMMON /INP_SP5/ EENc, SIGncf, NRNc
      COMMON /KALB  / XCOs
      COMMON /NUMBAR/ EPS_1d, VDEf_1d, NPOints, IIExtr, NEXtr
      COMMON /R250COM/ INDexf, INDexb, BUFfer
C
C Local variables
C
      REAL*8 :: aafis, acs, angstep, ares, atotsp, checkprd, checkxs, 
     &          coef, corrmsd, csemax, csemist, csinel, csmax, csmsdl, 
     &          cspg, csum, dang, dcor, dcs, ded, delang, deltae_pfns, 
     &          dencomp, dtmp0, dtmp1, echannel, ecm, eincid, emeda, 
     &          emedg, emedh, emedn, emedp, eneutr, epre, eps, erecoil, 
     &          espec, fisxse, fmed, fnorm, ftmp, ftmp1, gang, gcs, 
     &          gtotsp, hcs, htotsp, ncs, pcs, piece, pope, poph, popl, 
     &          popleft, poplev, popread, poptot, ptotsp, q2, q3, qmax, 
     &          qstep, recorp, sgamc, spdif, spdiff, stauc, step, sumx, 
     &          sumfis, tcs, tequiv, totemis, totsp, val, 
     &          weight, xcse, xizat
      CHARACTER(9) :: cejectile
      REAL*8, DIMENSION(NDEcse,NDAngecis) :: cseaprnt
      REAL*8, DIMENSION(NDEpfn) :: csepfns, enepfns, post_fisn, 
     &                             ratio2maxw
      REAL*8, DIMENSION(NDEcse) :: csetmp
      REAL*8, DIMENSION(NDNuc) :: csprnt
      CHARACTER(3) :: ctldir
      CHARACTER(23) :: ctmp23
      REAL :: ctotsp, da, dtmp, dtotsp, emedc, emedd, emedt, fniueval, 
     &        fnubar, ftmp_gs, ggmr, ggor, ggqr, theta, totcorr, 
     &        totspec, totsum, ttotsp
      REAL*8 :: DBLE, DEXP, DMAX1
      REAL*8, DIMENSION(NDCollev) :: deform
      REAL*8, DIMENSION(NDAngecis) :: elada, elleg
      LOGICAL :: fexist, nonzero, nvwful, skip_fiss
      REAL :: FLOAT, SNGL
      REAL*8 :: FMAXW, GRAND
      INTEGER :: i, ia, iad, iaf, iam, iang, iang1, ib, icalled, icsh, 
     &           icsl, ie, iizaejc, ikey1, ikey2, ikey3, ikey4, il, 
     &           iloc, ilv, imaxt, imint, ip, ipar, iprinted, irec, 
     &           isigma, isigma0, isigma2, ispec, itimes, itmp, its, iz, 
     &           izares, izf, j, jcn, jfiss, jn, jnmx, jz, jzmx, ke, 
     &           kemax, kemin, l, ltrmax, m, metas, mintsp, na, nang, 
     &           nbr, ncoll, ncon, nejc, nejcec, neles, nepfns, netl, 
     &           nfission, nnuc, nnur, nnurec, npsp, nspec, nspecrec, 
     &           nxsp
      CHARACTER(72) :: inprecord, rtitle
      INTEGER :: INT, MIN0, NINT
      INTEGER, DIMENSION(NDCollev) :: itemp
      CHARACTER(6) :: keyname
      CHARACTER(36) :: nextenergy
      CHARACTER(21), DIMENSION(NDNuc) :: preaction
      CHARACTER(21) :: reactionx
      REAL*8, DIMENSION(NFMod) :: sumfism
      REAL*8, DIMENSION(0:NDEjc + 3,0:15,0:20) :: xcross
      REAL*8 :: xnl, xnor, xsinl, xsinlcont, xtotsp, zres
      REAL*8, DIMENSION(2,NDExclus) :: xnorm
C
C*** End of declarations rewritten by SPAG
C
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:ppu*
Ccc   *                         E M P I R E                              *
Ccc   *                                                                  *
Ccc   *               Used to be main of the EMPIRE code                 *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   ********************************************************************
C
C Local functions
C
      ! controln, controlp,
C     -----------------------------------------------
C     PFNS quantities
C     Total prompt fission spectra only for neutrons
C     and assumed isotropic
C     -----------------------------------------------
C     For lifetime calculation, now commented (RCN/MH Jan 2011)
C     DOUBLE PRECISION taut,tauf,gamt,gamfis
C     -----------------------------------------------
 
      DATA ctldir/'TL/'/                        ! spechk(4),
 
      INCLUDE 'io.h'

      icalled = 0
      CALL THORA(8)
      EIN = 0.0D0
      epre = EIN
      ICAlangs = 0
      xcross = 0.0D0
 
C-----
C-----Skip mandatory part of the standard input
C-----
      REWIND 5
      DO i = 1, 10
        READ(5,*)
      ENDDO
C-----Read line of optional input
  130 READ(5,'(A72)')inprecord
      IF(inprecord(1:1).EQ.'*'.OR.inprecord(1:1).EQ.'#'.OR.
     &   inprecord(1:1).EQ.'!')GOTO 130    ! comments
 
      IF(inprecord(1:1).EQ.'@')THEN  ! title
        DO j = 1, 72
          EMPtitle(j:j) = inprecord(j:j) ! title of the run
        ENDDO
        EMPtitle(1:1) = ' '
      ENDIF
      REWIND 5
C-----
C-----Read and prepare input data
C-----
  140 CALL INPUT
C
      IF(FITomp.LT.0)OPEN(40,FILE = 'OPTFIT.CAL',STATUS = 'UNKNOWN')
C-----
C-----Print input data
C-----
      IF(IOUt.GT.0)CALL PRINPUT
      WRITE(*,'( ''   C.M. incident energy '',G10.5,'' MeV'')')EIN
      WRITE(8,'(/''   C.M. incident energy '',G10.5,'' MeV'')')EIN
C-----
C-----Print results of the systematics
C-----
      IF(FIRst_ein.AND.AEJc(0).EQ.1.AND.ZEJc(0).EQ.0)
     &   CALL SYSTEMATICS(SNGL(A(0)),SNGL(Z(0)),1)
 
C-----Clear CN elastic cross section (1/4*pi)
      ELCncs = 0.0D+0
C-----
C-----Open file 41 with tabulated cross sections
C-----
      IF(FIRst_ein)THEN
        OPEN(53,FILE = 'LOW_ENERGY.OUT',STATUS = 'UNKNOWN')
C       OPEN (UNIT = 68,FILE='ELASTIC.DAT', STATUS = 'UNKNOWN')  ! for Chris
        OPEN(41,FILE = 'XSECTIONS.OUT',STATUS = 'unknown')
 
        i = 0
        DO nnuc = 1, NNUcd
          i = i + 1
          preaction(i) = REAction(nnuc)
        ENDDO
        WRITE(41,'(''#'',I3,10X,i3,''-'',A2,''-'',I3)')i + 4, INT(Z(0)), 
     &        SYMb(0), INT(A(0))
        WRITE(41,'(''#'',A10,1X,(95A12))')'  Einc    ', '  Total     ', 
     &        '  Elastic   ', '  Reaction  ', '  Fission   ', 
     &        (preaction(nnuc),nnuc = 1,MIN(i,NDNuc,87))
        OPEN(98,FILE = 'FISS_XS.OUT',STATUS = 'unknown')
        IF(FISspe.GT.0)THEN
          OPEN(73,FILE = 'PFNS.OUT',STATUS = 'unknown')
          OPEN(74,FILE = 'PFNM.OUT',STATUS = 'unknown')
          IF(IOPsys.EQ.0)THEN   !Linux, Mac
            WRITE(74,
     &           '(''   Elab     <Epfns>  nubar(EVAL)  Tmaxw(equiv) '')'
     &           )
          ELSE                  !Windows
            WRITE(74,
     &           '(''   Elab     <Epfns>  nubar(TEST)  Tmaxw(equiv) '')'
     &           )
          ENDIF
        ENDIF
      ENDIF
C-----
C-----Prepare Giant Resonance parameters - systematics
C-----
      CALL ULM(0)
C-----
C-----Calculate reaction cross section and its spin distribution
C-----
      CALL MARENG(0,0)
C     Total cross section is set to absorption cross section
C     for photon induced reactions (to process them in EMPEND)
C
      IF(INT(AEJc(0)).EQ.0)TOTcs = CSFus
C-----Locate position of the target among residues
      CALL WHERE(IZA(1) - IZAejc(0),nnurec,iloc)
C-----Locate position of the projectile among ejectiles
      CALL WHEREJC(IZAejc(0),nejcec,iloc)
C
C---- Calculate compound nucleus (CN) level density
C
      nnuc = 1
 
C-----check whether NLW is not larger than
C-----max spin at which nucleus is still stable
 
      IF(NLW.GT.JSTab(1).AND.JSTab(1).GT.0)THEN
        WRITE(8,
     &        '('' WARNING: Maximum spin to preserve stability is'',I4)'
     &        )JSTab(1)
        WRITE(8,
     &    '('' WARNING: Calculations will be truncated at this limit'')'
     &    )
        WRITE(8,
     &    '('' WARNING: Maximum stable spin (rot. limit) Jstab < '',I3)'
     &    )JSTab(1) + 1
        IF(JSTab(1).LE.NDLw)THEN
          ftmp1 = 0.D0
          DO j = JSTab(1), MIN(NDLw,NLW)
            ftmp1 = ftmp1 + POP(NEX(1),j,1,1) + POP(NEX(1),j,2,1)
            POP(NEX(1),j,1,1) = 0.0
            POP(NEX(1),j,2,1) = 0.0
          ENDDO
          CSFus = CSFus - ftmp1
          WRITE(8,
     &         '('' WARNING: Some fusion cross section lost : '', F9.3)'
     &         )ftmp1, ' mb, due to the stability limit'
        ELSE
          WRITE(8,
     &'('' WARNING: Increase NDLW in dimension.h and recompile EMPIRE'' 
     &)')
        ENDIF
        NLW = MIN(JSTab(1),NDLw)
      ENDIF
 
      csmax = 0.D0
      DO ip = 1, 2
        DO j = 1, NDLw
          csmax = DMAX1(POP(NEX(1),j,ip,1),csmax)
        ENDDO
      ENDDO
 
      IF((POP(NEX(1),NLW,1,1)*20.D0.GT.csmax.OR.POP(NEX(1),NLW,2,1)
     &   *20.D0.GT.csmax).AND.NLW.EQ.NDLw)THEN
        WRITE(8,*)'POP1=', POP(NEX(1),NLW,1,1), 'POP2=', 
     &            POP(NEX(1),NLW,2,1), 'NLW=', NLW
        WRITE(8,
     &'('' NUMBER OF PARTIAL WAVES FOR WHICH CODE IS DIMENSIONE'',''D IS
     & INSUFFICIENT'',/,'' INCREASE NDLW IN THE dimensio'',''n.h FILE AN
     &D RECOMPILE  '',/,'' EXECUTION  S T O P P E '',''D '')')
        STOP 'Insufficient dimension NDLW for partial waves'
      ENDIF
C
      WRITE(ctmp23,'(i3.3,i3.3,1h_,i3.3,i3.3,1h_,i9.9)')INT(ZEJc(0)), 
     &      INT(AEJc(0)), INT(Z(0)), INT(A(0)), INT(EINl*1000000)
C     TOTcs, ABScs, ELAcs are initialized within MARENG()
      xsinlcont = 0.D0
      xsinl = 0.D0
      checkxs = 0.D0
C     For resolution function (Spreading levels in the continuum)
      isigma0 = 0
      IF(WIDcoll.GT.0.D0)isigma0 = INT((0.02D0 + SQRT(EINl)*WIDcoll)
     &                             /DE + 1.0001)
      ncoll = 0
      ecm = EINl - EIN
      dang = PI/FLOAT(NANgela - 1)
      angstep = 180.D0/(NANgela - 1)
      gang = 180.D0/(NDAng - 1)
C-----
C-----Get ECIS results
C-----
      IF(ICAlangs.GT.0)THEN
        OPEN(45,FILE = (ctldir//ctmp23//'.EXP'),STATUS = 'OLD',
     &       ERR = 150)
        READ(45,*,END = 150)     ! To skip first line <ANG.DIS.> ..
        READ(45,*,END = 150)     ! To skip level identifier line
        DO iang = 1, NANgela
          READ(45,'(24x,D12.5)',END = 150)elada(iang)
        ENDDO
      ELSE
        OPEN(45,FILE = (ctldir//ctmp23//'.LEG'),STATUS = 'OLD',
     &       ERR = 150)
        READ(45,*,END = 150)     ! To skip first line <LEGENDRE> ..
        READ(45,'(5x,i5)',END = 150)neles
        DO iang = 1, MIN(NDAng,neles)
          READ(45,'(10x,D20.10)',END = 150)elleg(iang)
        ENDDO
        CLOSE(45)
        OPEN(45,FILE = (ctldir//ctmp23//'.ANG'),STATUS = 'OLD',
     &       ERR = 150)
        READ(45,*,END = 150)     ! To skip first line <ANG.DIS.> ..
        READ(45,*,END = 150)     ! To skip level identifier line
        iang = 0
        DO iang1 = 1, NANgela
C----------To use only those values corresponding to EMPIRE grid for elastic XS
          READ(45,'(3x,12x,D12.5)',END = 150)ftmp        ! ecis06
          IF(MOD(DBLE(iang1-1)*angstep + gang,gang).NE.0)CYCLE
          iang = iang + 1
          elada(iang) = ftmp
        ENDDO
      ENDIF
      IF(DIRect.NE.0)THEN
        ggmr = 3.
        ggqr = 85.*A(0)**( - 2./3.)
        ggor = 5.
        mintsp = MOD(NINT(2*D_Xjlv(1)),2)
        OPEN(46,FILE = (ctldir//ctmp23//'.ICS'),STATUS = 'OLD',
     &       ERR = 150)
        READ(46,*,END = 150)     ! To skip first line <INE.C.S.> ..
C---------Get and add inelastic cross sections (including double-differential)
        DO i = 2, ND_nlv
          ilv = ICOller(i)
C          RCN 2010
          IF(ICOllev(i).LE.LEVcc.AND.SINlcc.LE.0)EXIT
          IF(ICOllev(i).GT.LEVcc.AND.SINl + SINlcont.LE.0)CYCLE
 
          IF(ilv.LE.NLV(nnurec))THEN
C          For odd nuclides, collective states in continuum have different spin than the ground state
C    &         (mod(NINT(2*D_Xjlv(i)),2).eq.mintsp) )THEN
C------------Adding inelastic to discrete levels
            echannel = EX(NEX(1),1) - Q(nejcec,1) - ELV(ilv,nnurec)
C------------Avoid reading closed channels
            IF(echannel.GE.0.0001)THEN
              xcse = echannel/DE + 1.0001
              icsl = INT(xcse)
              icsh = icsl + 1
              READ(46,*,END = 150)popread
              popread = popread*FCCred
              ncoll = i
              POPlv(ilv,nnurec) = POPlv(ilv,nnurec) + popread
              CSDirlev(ilv,nejcec) = CSDirlev(ilv,nejcec) + popread
              CSEmis(nejcec,1) = CSEmis(nejcec,1) + popread
C--------------Add direct transition to the spectrum
              popl = popread*(FLOAT(icsh) - xcse)/DE
              IF(icsl.EQ.1)popl = 2.0*popl
              poph = popread*(xcse - FLOAT(icsl))/DE
              CSE(icsl,nejcec,1) = CSE(icsl,nejcec,1) + popl
              CSE(icsh,nejcec,1) = CSE(icsh,nejcec,1) + poph
 
              CSEt(icsl,nejcec) = CSEt(icsl,nejcec) + popl
              CSEt(icsh,nejcec) = CSEt(icsh,nejcec) + poph
 
              IF(ICAlangs.LE.0)THEN
                READ(45,*,END = 150)       ! Skipping level identifier line
                iang = 0
                DO iang1 = 1, NANgela
                  READ(45,'(3x,12x,D12.5)',END = 150)ftmp       ! ecis06
C-----------------To use only those values corresponding to EMPIRE grid for inelastic XS
                  IF(MOD(DBLE(iang1-1)*angstep + gang,gang).NE.0)CYCLE
                  iang = iang + 1
                  CSAlev(iang,ilv,nejcec) = CSAlev(iang,ilv,nejcec)
     &              + ftmp
                ENDDO
              ELSEIF(i.LE.ICAlangs)THEN
                READ(45,*,END = 150)         ! Skipping level identifier line
                DO iang = 1, NANgela
                  READ(45,'(24x,D12.5)',END = 150)ftmp
                  CSAlev(iang,ilv,nejcec) = CSAlev(iang,ilv,nejcec)
     &              + ftmp
                ENDDO
              ENDIF
C--------------Construct recoil spectra due to direct transitions
              IF(ENDf(nnurec).GT.0.AND.RECoil.GT.0)THEN
                dang = PI/FLOAT(NDAng - 1)
                coef = 2*PI*dang
C-----------------Check whether integral over angles agrees with x-sec. read from ECIS
                csum = 0.0
                DO iang = 1, NDAng
                  csum = csum + CSAlev(iang,ilv,nejcec)*SANgler(iang)
     &                   *coef
                ENDDO
C-----------------Correct 'coef' for eventual imprecision and include recoil DE
                coef = coef*POPlv(ilv,nnurec)/csum/DERec
                echannel = echannel*EJMass(0)/AMAss(1)
                DO iang = 1, NDAng
                  erecoil = ecm + echannel + 2*SQRT(ecm*echannel)
     &                      *CANgler(iang)
                  irec = erecoil/DERec + 1.001
                  weight = (erecoil - (irec - 1)*DERec)/DERec
C--------------------Escape if we go beyond recoil spectrum dimension
                  IF(irec + 1.GT.NDErec)EXIT
                  csmsdl = CSAlev(iang,ilv,nejcec)*SANgler(iang)*coef
                  RECcse(irec,0,nnurec) = RECcse(irec,0,nnurec)
     &              + csmsdl*(1.0 - weight)
                  RECcse(irec + 1,0,nnurec) = RECcse(irec + 1,0,nnurec)
     &              + csmsdl*weight
                ENDDO
              ENDIF
            ELSE
              READ(46,*,END = 150)popread
              READ(45,*,END = 150)        ! Skipping level identifier line
            ENDIF
          ELSEIF(MSD.EQ.0)THEN
C------------Adding inelastic to continuum  (D_Elv(ND_nlv) = elvr)
            echannel = EX(NEX(1),1) - Q(nejcec,1) - D_Elv(i)
            icsl = INT(echannel/DE + 1.0)
            ncon = MIN(NINT((EXCn-Q(nejcec,1)-ECUt(nnurec))/DE),NDEcse)
C            WRITE(8,*) 'nejcec, nnurec',IZAejc(nejcec), IZA(nnurec)
C            WRITE(8,*) 'Level in continuum',D_Elv(i)
C            WRITE(8,*) 'Its bin number',icsl
C            WRITE(8,*) 'E calc',EX(NEX(1),1)-Q(nejcec,1)-(icsl-1)*DE
C            WRITE(8,*) 'Last discr. level',ELV(NLV(nnurec),nnurec)
C            WRITE(8,*) 'Ecut',ECUt(nnurec)
C            WRITE(8,*) 'Ex',EX(NEX(1),1)-Q(nejcec,1)-(ncon-1)*DE
C            WRITE(8,*) 'Continuum starts at bin number',ncon
C------------Avoid reading closed channels
            IF(echannel.GE.0.0001.AND.icsl.GT.0)THEN
              READ(46,*,END = 150)popread
C
C--------------This level is not counted as a discrete one
C--------------but it is embedded in the continuum
              CSMsd(nejcec) = CSMsd(nejcec) + popread
              xsinlcont = xsinlcont + popread
C--------------Spreading it using resolution function
C
C              Special treatment for Giant Multipole Resonances
C              Any level with D_Def(i,2)<0 treated as GR
C
C              Giant multipole resonances following TALYS
C
C              For each L multipolarity Energy Weighted Sum Rule (EWSR) applies:
C              SUM_i(E_i*beta_i)=57.5*A**(-5/3)*L*(L+1)
C
              isigma = isigma0
              isigma2 = 2*isigma0*isigma0
              IF(D_Def(i,2).LT.0.AND.INT(AEJc(0)).EQ.1.AND.INT(ZEJc(0))
     &           .EQ.0)THEN
                IF(INT(D_Xjlv(i)).EQ.0)isigma = NINT(ggmr/DE + 0.5)
                IF(INT(D_Xjlv(i)).EQ.2)isigma = NINT(ggqr/DE + 0.5)
                IF(INT(D_Xjlv(i)).EQ.3)isigma = NINT(ggor/DE + 0.5)
                isigma2 = 2*isigma*isigma
              ENDIF
              IF(isigma.GT.0)THEN
                dcor = 0.D0
                DO ie = MAX(icsl - 3*isigma,1), 
     &            MIN(icsl + 3*isigma,ncon)
                  dcor = DEXP( - DBLE(ie - icsl)**2/isigma2) + dcor
                ENDDO
                IF(dcor.GT.0.D0)THEN
                  DO ie = MAX(icsl - 3*isigma,1), 
     &              MIN(icsl + 3*isigma,ncon)
                    CSEmsd(ie,nejcec) = CSEmsd(ie,nejcec)
     &                                  + popread/DE*DEXP
     &                                  ( - DBLE(ie - icsl)**2/isigma2)
     &                                  /dcor
                  ENDDO
                ELSE
                  CSEmsd(icsl,nejcec) = CSEmsd(icsl,nejcec) + popread/DE
                ENDIF
              ELSE
                CSEmsd(icsl,nejcec) = CSEmsd(icsl,nejcec) + popread/DE
              ENDIF
              IF(ICAlangs.EQ.0)THEN
                READ(45,*,END = 150)        ! Skipping level identifier line
                iang = 0
                DO iang1 = 1, NANgela
                  READ(45,'(3x,12x,D12.5)',END = 150)ftmp         ! ecis06
C-------------------Use only those values that correspond to EMPIRE grid for inelastic XS
                  IF(MOD(DBLE(iang1-1)*angstep + gang,gang).NE.0)CYCLE
                  iang = iang + 1
                  IF(isigma.GT.0.AND.dtmp.GT.0.D0)THEN
                    DO ie = MAX(icsl - 3*isigma,1), 
     &                MIN(icsl + 3*isigma,ncon)
                      CSEa(ie,iang,nejcec,1) = CSEa(ie,iang,nejcec,1)
     &                  + ftmp/DE*DEXP( - DBLE(ie - icsl)**2/isigma2)
     &                  /dtmp
                    ENDDO
                  ELSE
                    CSEa(icsl,iang,nejcec,1) = CSEa(icsl,iang,nejcec,1)
     &                + ftmp/DE
                  ENDIF
                ENDDO
              ENDIF
            ENDIF
C------------End of adding inelastic to continuum
          ENDIF
        ENDDO
      ENDIF
  150 CLOSE(45)
      IF(DIRect.NE.0)CLOSE(46)
 
      IF(KTRlom(0,0).GT.0.AND.FIRst_ein)THEN
        IF(DIRect.EQ.0)THEN
        ELSEIF(DIRect.EQ.1.OR.DIRect.EQ.2)THEN
          IF((CSMsd(1) + CSMsd(2)).NE.0.)WRITE(8,*)
     &       ' Some discrete levels are embedded into continuum'
        ELSEIF(DIRect.EQ.3)THEN
          IF(xsinlcont.NE.0.)WRITE(8,*)
     &               ' Some discrete levels are embedded into continuum'
        ENDIF
      ENDIF
 
      totcorr = 1.D0
 
      IF(KTRlom(0,0).GT.0)THEN
        IF(ZEJc(0).EQ.0.AND.AEJc(0).GT.0)THEN
 
          IF(INT(ZEJc(0)).EQ.0.AND.TOTcs.GT.0.D0)
     &       totcorr = (ELAcs*ELAred + CSFus + (SINl + SINlcc)
     &       *FCCred + SINlcont)/(TOTcs*TOTred)
C
          WRITE(8,1010)TOTcs, TOTred*totcorr, TOTred*TOTcs*totcorr, 
     &                 CSFus/FUSred, FUSred, CSFus, ELAcs, ELAred, 
     &                 ELAred*ELAcs
 1010     FORMAT(/,2x,'Total cross section         :',e14.7,' mb',
     &           '  ( Scaled by ',f5.2,'  to ',e14.7,' mb )',/,2x,
     &           'Absorption cross section    :',e14.7,' mb',
     &           '  ( Scaled by ',f5.2,'  to ',e14.7,' mb )',/,2x,
     &           'Shape elastic cross section :',e14.7,' mb',
     &           '  ( Scaled by ',f5.2,'  to ',e14.7,' mb )')
          IF((SINlcc + SINl + SINlcont).GT.0)WRITE(8,1020)
     &       SINlcc + SINl + SINlcont, 
     &       ((SINlcc + SINl)*FCCred + SINlcont)
     &       /(SINlcc + SINl + SINlcont), (SINlcc + SINl)
     &       *FCCred + SINlcont
 1020     FORMAT(/,2x,'Direct cross section        :',e14.7,' mb',
     &           '  ( Scaled by ',f5.2,'  to ',e14.7,' mb )')
          WRITE(8,'(/)')
        ENDIF
        IF(ZEJc(0).NE.0.OR.AEJc(0).EQ.0)THEN
C        WRITE (8,99010) CSFus + (SINlcc + SINl)*FCCred + SINlcont
C99010   FORMAT (/,2x,'Absorption cross section    :',e14.7,' mb')
          WRITE(8,1030)CSFus/FUSred, FUSred, CSFus
 1030     FORMAT(/,2x,'Absorption cross section    :',e14.7,' mb',
     &           '  ( Scaled by ',f5.2,'  to ',e14.7,' mb )')
          IF((SINlcc + SINl + SINlcont).GT.0)WRITE(8,1020)
     &       SINlcc + SINl + SINlcont, 
     &       ((SINlcc + SINl)*FCCred + SINlcont)
     &       /(SINlcc + SINl + SINlcont), (SINlcc + SINl)
     &       *FCCred + SINlcont
          WRITE(8,'(/)')
        ENDIF
        WRITE(8,1040)
        WRITE(8,1050)
        gang = 180.0/(NDAng - 1)
        angstep = 180.0/(NANgela - 1)
        DO iang = 1, NANgela/4 + 1
          imint = 4*(iang - 1) + 1
          imaxt = MIN0(4*iang,NANgela)
          WRITE(8,1060)((j - 1)*angstep,ELAred*elada(j),j = imint,imaxt)
        ENDDO
 1040   FORMAT(' ',46x,'SHAPE ELASTIC DIFFERENTIAL CROSS-SECTION',/,' ',
     &         46x,40('*'),/,' ',56x,'CENTER-OF-MASS SYSTEM',///)
 1050   FORMAT(' ',5x,4('    TETA ',2x,'D.SIGMA/D.OMEGA',6x),/)
 1060   FORMAT(' ',5x,4(1p,e12.5,2x,e12.5,6x))
 1070   FORMAT(' ',46x,'INELASTIC DIFFERENTIAL CROSS-SECTION',//,' ',
     &         46x,'  (only discrete levels are listed)',/,' ',46x,
     &         36('*'),/,' ',56x,'CENTER-OF-MASS SYSTEM',///)
 1080   FORMAT('  Angle ',10(6x,i2,'-level'))
 1090   FORMAT('        ',9(5x,'E=',f7.4))
 1100   FORMAT('        ',10(5x,'E=',f7.4))
 1110   FORMAT('        ',9(4x,f4.1,'/',f5.4))
 1120   FORMAT('        ',10(4x,f4.1,'/',f5.4))
 1130   FORMAT(1x,f5.1,3x,11(2x,E12.6))
 1140   FORMAT(6x,3x,11(2x,E12.6))
        WRITE(8,'(//)')
        IF(ncoll.GT.0)THEN
C--------Locate position of the projectile among ejectiles
          CALL WHEREJC(IZAejc(0),nejcec,iloc)
          WRITE(8,*)' '
          gang = 180.D0/(NDAng - 1)
          its = 2
          DO ilv = 2, ncoll
            DO iang = ilv + 1, ncoll
              IF(ICOller(iang).EQ.ICOller(ilv))GOTO 155
            ENDDO
            itemp(its) = ICOller(ilv)
            deform(its) = D_Def(ilv,2)
            its = its + 1
  155     ENDDO
          its = its - 1
          IF(CSAlev(1,ICOller(2),nejcec).GT.0)THEN
            WRITE(8,1070)
            WRITE(8,1080)(itemp(ilv),ilv = 2,MIN(its,10))
            WRITE(8,1090)(ELV(itemp(ilv),nnurec),ilv = 2,MIN(its,10))
            WRITE(8,1110)(XJLv(itemp(ilv),nnurec)*LVP(itemp(ilv),nnurec)
     &                   ,deform(ilv),ilv = 2,MIN(its,10))
            WRITE(8,*)' '
            DO iang = 1, NDAng
              WRITE(8,1130)(iang - 1)*gang, 
     &                     (CSAlev(iang,itemp(ilv),nejcec),ilv = 2,
     &                     MIN(its,10))
            ENDDO
            WRITE(8,*)' '
            WRITE(8,1140)(POPlv(itemp(ilv),nnurec),ilv = 2,MIN(its,10))
            IF(its.GT.10)THEN
              WRITE(8,*)' '
              WRITE(8,*)' '
              WRITE(8,1080)(itemp(ilv),ilv = 11,MIN(its,20))
              WRITE(8,1100)(ELV(itemp(ilv),nnurec),ilv = 11,MIN(its,20))
              WRITE(8,1120)(XJLv(itemp(ilv),nnurec)*LVP(itemp(ilv),
     &                     nnurec),deform(ilv),ilv = 11,MIN(its,20))
              WRITE(8,*)' '
              DO iang = 1, NDAng
                WRITE(8,1130)(iang - 1)*gang, 
     &                       (CSAlev(iang,itemp(ilv),nejcec),ilv = 11,
     &                       MIN(its,20))
              ENDDO
              WRITE(8,*)' '
              WRITE(8,1140)(POPlv(itemp(ilv),nnurec),ilv = 11,MIN(its,20
     &                     ))
            ENDIF
            IF(its.GT.20)THEN
              WRITE(8,*)' '
              WRITE(8,*)' '
              WRITE(8,1080)(itemp(ilv),ilv = 21,MIN(its,30))
              WRITE(8,1100)(ELV(itemp(ilv),nnurec),ilv = 21,MIN(its,30))
              WRITE(8,1120)(XJLv(itemp(ilv),nnurec)*LVP(itemp(ilv),
     &                     nnurec),deform(ilv),ilv = 21,MIN(its,30))
              WRITE(8,*)' '
              DO iang = 1, NDAng
                WRITE(8,1130)(iang - 1)*gang, 
     &                       (CSAlev(iang,itemp(ilv),nejcec),ilv = 21,
     &                       MIN(its,30))
              ENDDO
              WRITE(8,*)' '
              WRITE(8,1140)(POPlv(itemp(ilv),nnurec),ilv = 21,MIN(its,30
     &                     ))
            ENDIF
C
C----------Because of the ENDF format restrictions the maximum
C----------number of discrete levels is limited to 40
C
            IF(its.GT.30)THEN
              WRITE(8,*)' '
              WRITE(8,*)' '
              WRITE(8,1080)(itemp(ilv),ilv = 31,MIN(its,40))
              WRITE(8,1100)(ELV(itemp(ilv),nnurec),ilv = 31,MIN(its,40))
              WRITE(8,1120)(XJLv(itemp(ilv),nnurec)*LVP(itemp(ilv),
     &                     nnurec),deform(ilv),ilv = 31,MIN(its,40))
              WRITE(8,*)' '
              DO iang = 1, NDAng
                WRITE(8,1130)(iang - 1)*gang, 
     &                       (CSAlev(iang,itemp(ilv),nejcec),ilv = 31,
     &                       MIN(its,40))
              ENDDO
              WRITE(8,*)' '
              WRITE(8,1140)(POPlv(itemp(ilv),nnurec),ilv = 31,MIN(its,40
     &                     ))
            ENDIF
            WRITE(8,*)' '
            WRITE(8,*)' '
            WRITE(8,*)' '
          ENDIF
        ENDIF
      ENDIF
C
C-----calculate transmission coefficients in outgoing channels
C
      DO nnuc = 1, NNUcd
        DO nejc = 1, NEJcm
          ares = A(nnuc) - AEJc(nejc)
          zres = Z(nnuc) - ZEJc(nejc)
C           residual nuclei must be heavier than alpha
          IF(ares.LE.4.OR.zres.LE.2)CYCLE
 
          izares = INT(1000*zres + ares)
          CALL WHERE(izares,nnur,iloc)
          IF(iloc.EQ.1)CYCLE
 
          netl = 6
          IF(NEX(nnuc).GT.0)netl = INT((EX(NEX(nnuc),nnuc) - Q(nejc,nnuc
     &                             ))/DE) + 6
 
          IF(netl.GT.NDEtl)CYCLE
 
          ICAlangs = ICAlangs - 10
          itmp = NANgela
          NANgela = 2
          CALL TLEVAL(nejc,nnur,nonzero)
          ICAlangs = ICAlangs + 10
          NANgela = itmp
C-----------print transmission coefficients
          IF(nonzero.AND.IOUt.EQ.5)THEN
            WRITE(8,*)
            WRITE(8,*)' Transmission coefficients for '
            WRITE(8,'(1x,A15,I3,A3,I3,A3,F4.1)')' Projectile: A=', 
     &            INT(AEJc(nejc)), ' Z=', INT(ZEJc(nejc)), ' S=', 
     &            SEJc(nejc)
            WRITE(8,'(1x,A11,I3,A3,I3,A3,F4.1,A3,I2)')' TARGET: A=', 
     &            INT(A(nnur)), ' Z=', INT(Z(nnur)), ' S=', 
     &            SNGL(XJLv(1,nnur)), ' P=', INT(LVP(1,nnur))
            DO i = 1, netl
              IF(TL(i,1,nejc,nnur).GT.0.0)WRITE(8,1030)ETL(i,nejc,nnur), 
     &           (TL(i,j,nejc,nnur),j = 1,12)
            ENDDO
            WRITE(8,'(1X,/)')
          ENDIF
        ENDDO      !over ejectiles (nejc)
      ENDDO     !over nuclei (nnuc)
C
C-----determination of transmission coeff.--done
 1150 FORMAT(1X,14(G10.4,1x))
C
C     Skipping all emission calculations
C     GOTO 99999
C
C-----Locate positions of ENDF MT-numbers 2, 91, 649, and 849
      CALL WHERE(IZA(1) - IZAejc(0),MT2,iloc)
      CALL WHERE(IZA(1) - IZAejc(1),MT91,iloc)
      CALL WHERE(IZA(1) - IZAejc(2),MT649,iloc)
      CALL WHERE(IZA(1) - IZAejc(3),MT849,iloc)
C-----Locate residual nuclei after CN decay
      NREs(0) = 1
      DO nejc = 1, NEJcm
        NREs(nejc) = -1
        ares = A(1) - AEJc(nejc)
        zres = Z(1) - ZEJc(nejc)
C        residual nuclei must be heavier than alpha
        IF(ares.LE.4.AND.zres.LE.2.)CYCLE
        izares = INT(1000.0*zres + ares)
        CALL WHERE(izares,nnur,iloc)
        NREs(nejc) = nnur
      ENDDO
C-----
C-----Calculate MSD contribution
C-----
      corrmsd = 1.0
      xsinl = 0.D0
      IF(MSD.NE.0.AND.EIN.GE.EMInmsd)THEN
C
C--------call ORION
C
C--------The INQUIRE statement determines if stored file exists.
C--------If it does not, the program start new calculations
        WRITE(ctmp23,'(i3.3,i3.3,1h_,i3.3,i3.3,1h_,i9.9)')INT(ZEJc(0)), 
     &        INT(AEJc(0)), INT(Z(0)), INT(A(0)), INT(EINl*1000000)
        INQUIRE(FILE = (ctldir//ctmp23//'.MSD'),EXIST = fexist)
        IF(.NOT.fexist)THEN
          OPEN(15,FILE = (ctldir//ctmp23//'.MSD'),STATUS = 'NEW')
        ELSE
          OPEN(15,FILE = (ctldir//ctmp23//'.MSD'),STATUS = 'OLD')
          WRITE(8,*)' '
          WRITE(8,*)' Using precalculated ORION results for E=', EINl, 
     &              ' MeV'
          WRITE(8,*)' '
          GOTO 165
        ENDIF
        WRITE(8,*)' '
        qmax = 0.99*EIN
        qstep = qmax/3.0
C        Proposed by H. Wienke
        ltrmax = 6
        IF(NLW.LE.15)ltrmax = 5
        IF(NLW.LE.13)ltrmax = 4
        IF(NLW.LE.10)ltrmax = 3
        IF(NLW.LE.8)ltrmax = 2
        IF(NLW.LE.6)ltrmax = 1
        WRITE(15,*)qmax, qstep, ltrmax
        q2 = qmax
        q3 = qmax
  160   CALL ORION(q2,q3,1,EIN,NLW,1,ltrmax,A(0),Z(0),AEJc(0),ZEJc(0),
     &             IOUt,ANGles,NDAng,ICOmpff)
        WRITE(8,
     &        '('' ORION calculated for Q2='', F7.3, '' and Q3='',F7.3)'
     &        )q2, q3
        q2 = q2 - qstep
        IF(q2.LT.( - 0.0001D0))THEN
          q3 = q3 - qstep
          IF(q3.LT.( - 0.0001D0))GOTO 165
          q2 = q3
        ENDIF
C--------Set to Q's to 0 if negative due to rounding error
        IF(q2.LT.0.0D0)q2 = 0.0
        IF(q3.LT.0.0D0)q3 = 0.0
        GOTO 160
  165   REWIND(15)
        READ(15,*)qmax, qstep, ltrmax
        WRITE(8,*)' '
        WRITE(8,*)' '
        CALL ULM(1)
        CALL TRISTAN(0,0,ltrmax,qmax,qstep,xsinl)
        CLOSE(15)
      ENDIF
 
C-----PCROSS exciton model calculations of preequilibrium contribution
C-----including cluster emission by Iwamoto-Harada model and angular
C-----distributions according to Kalbach systematics
C-----
 
      totemis = 0.D0
      IF(EINl.GT.0.1D0.AND.PEQc.GT.0)THEN
C        ftmp = CSFus - xsinl
C        RCN, Jan. 2006, xsinl is replacing PCROSS neutron emission
C        so it should not used for normalization
C        xsinl is calculated by MSD
        ftmp = CSFus
        CALL PCROSS(ftmp,totemis,xsinl)
      ENDIF          ! PCRoss done
 
      corrmsd = 1.D0
      IF((xsinl + totemis + SINl + SINlcc + SINlcont).GT.0..AND.
     &   nejcec.GT.0.AND.NREs(nejcec).GE.0)THEN
C--------Print inelastic PE double differential cross sections
        nejc = nejcec
        nnur = NREs(nejc)
        IF(CSMsd(nejc).GT.0.D0.AND.IOUt.GE.3)THEN
          itimes = FLOAT(NDAng)/11.0 + 0.95
          DO its = 1, itimes
            iad = 1 + (its - 1)*11
            iam = 11 + (its - 1)*11
            iam = MIN0(NDAng,iam)
            IF(nejc.EQ.1)WRITE(8,
     &                       '(//30X,''     N  E  U  T  R  O  N  S ''/)'
     &                       )
            IF(nejc.EQ.2)WRITE(8,
     &                         '(//30X,''     P  R  O  T  O  N  S ''/)')
            IF(nejc.EQ.3)WRITE(8,'(//30X,''     A  L  P  H  A  S ''/)')
            IF(nejc.GT.3)CYCLE
            WRITE(8,'(30X,''A      n      g      l      e      s '')')
            WRITE(8,*)' '
            WRITE(8,'('' Energy  '',11(4X,F5.1,2X))')
     &            (ANGles(ia),ia = iad,iam)
            WRITE(8,*)' '
C-------------Maximum and minimum energy bin
            echannel = EX(NEX(1),1) - Q(nejc,1)
C
C             Following changes in PCROSS to cover discrete levels , Jan 2011
            DO i = 1, MAX(INT(echannel/DE + 1.0001),1)
              WRITE(8,'(1X,F7.3,1X,11E11.4)')FLOAT(i - 1)*DE, 
     &              (MAX(CSEa(i,iang,nejc,1),0.D0),iang = iad,iam)
            ENDDO
            WRITE(8,*)' '
          ENDDO
        ENDIF
        IF(xsinlcont.GT.0)THEN
          WRITE(8,*)' DWBA to continuum XS for inelastic channel ', 
     &              xsinlcont
          SINlcont = xsinlcont
        ELSE
          SINlcont = 0.D0
        ENDIF
        WRITE(8,*)
        IF(CSMsd(0).GT.0.)WRITE(8,*)' g PE emission cross section ', 
     &                              CSMsd(0), ' mb'
        IF(CSMsd(1).GT.0.)WRITE(8,*)' n PE emission cross section ', 
     &                              CSMsd(1), ' mb'
        IF(CSMsd(2).GT.0.)WRITE(8,*)' p PE emission cross section ', 
     &                              CSMsd(2), ' mb'
        IF(CSMsd(3).GT.0.)WRITE(8,*)' a PE emission cross section ', 
     &                              CSMsd(3), ' mb'
        IF(CSMsd(4).GT.0.)WRITE(8,*)' d PE emission cross section ', 
     &                              CSMsd(4), ' mb'
        IF(CSMsd(5).GT.0.)WRITE(8,*)' t PE emission cross section ', 
     &                              CSMsd(5), ' mb'
        IF(CSMsd(6).GT.0.)WRITE(8,*)' h PE emission cross section ', 
     &                              CSMsd(6), ' mb'
        IF(NEMc.GT.0.AND.CSMsd(NDEjc).GT.0.)WRITE(8,*)
     &     ' Cluster PE emission cross section ', CSMsd(NDEjc), ' mb'
        WRITE(8,*)' '
C--------Correct CN population for PE continuum emission in PCROSS and MSD/MSC
        ftmp = 0.D0
        DO i = 1, NDEjc
          ftmp = ftmp + CSMsd(i)
        ENDDO
 
        corrmsd = (CSFus - ftmp)/CSFus
C        corrmsd = (CSFus - (xsinl + totemis))/CSFus
C        write(*,*) ' CSFus=',sngl(CSFus)
C    &       ,' xsinl=',sngl(xsinl),' PCROSS=',sngl(totemis)
 
        IF(corrmsd.LT.0.0D0)THEN
          WRITE(8,*)' CSFus=', SNGL(CSFus), ' xsinl=', SNGL(xsinl), 
     &              ' PCROSS=', SNGL(totemis)
          WRITE(*,*)' CSFus=', SNGL(CSFus), ' xsinl=', SNGL(xsinl), 
     &              ' PCROSS=', SNGL(totemis)
          totemis = CSFus - xsinl
          corrmsd = 0.D0
 
          IF(xsinl.LT.0.0001D0)THEN
            xsinl = 0.D0
            totemis = CSFus
            corrmsd = 0.D0
            WRITE(8,*)' Changed to : xsinl = ', xsinl, ' PCROSS=', 
     &                totemis
          ENDIF
 
          WRITE(8,*)' '
          WRITE(8,*)'PE EMISSION LARGER THEN FUSION CROSS SECTION'
          IF(MSD + MSC.GT.0.AND.ICOmpff.GT.0)THEN
            WRITE(8,*)'TRY TO TURN OFF COMPRESSIONAL FORM FACTOR '
            WRITE(8,*)'SETTING COMPFF TO 0 IN THE OPTIONAL INPUT.'
C              STOP 'PE EMISSION LARGER THEN FUSION CROSS SECTION'
          ENDIF
          IF(MSD + MSC.GT.0.AND.ICOmpff.EQ.0)THEN
            WRITE(8,*)'THIS MAY HAPPEN IF RESPONSE FUNCTIONS ARE '
            WRITE(8,*)'RENORMALIZED IN INPUT OR FITTED TO WRONG '
            WRITE(8,*)'DISCRETE LEVELS. CHECK `EFIT` AND `RESNOR` '
            WRITE(8,*)'IN OPTIONAL INPUT.    '
            WRITE(8,*)'IF THESE ARE FINE TRY ANOTHER OPTICAL MODEL.'
C              STOP 'PE EMISSION LARGER THEN FUSION CROSS SECTION'
          ENDIF
          IF(MSD + MSC.EQ.0)THEN
            WRITE(8,*)'THIS MAY HAPPEN IF TOO MANY DISCRETE LEVELS'
            WRITE(8,*)'ARE EMBEDDED INTO CONTINUUM OR HAVE TOO BIG'
            WRITE(8,*)'DYNAMICAL DEFORMATIONS SPECIFIED IN THE    '
            WRITE(8,*)'COLLECTIVE LEVEL FILE.'
            WRITE(8,*)'TRY TO REDUCE THE NUMBER OF COLLECTIVE LEVELS.'
C              STOP 'PE EMISSION LARGER THEN FUSION CROSS SECTION'
          ENDIF
        ENDIF
        ftmp = 0.D0
        DO i = 1, NLW
          POP(NEX(1),i,1,1) = POP(NEX(1),i,1,1)*corrmsd
          POP(NEX(1),i,2,1) = POP(NEX(1),i,2,1)*corrmsd
          ftmp = ftmp + POP(NEX(1),i,1,1) + POP(NEX(1),i,2,1)
        ENDDO
        WRITE(8,*)' '
        WRITE(8,'(1x,A32,F9.2,A3)')' Absorption cross section       ', 
     &                             SNGL(CSFus), ' mb'
        WRITE(8,'(1x,A32,F9.2,A3,1x,1h(,F6.2,A2,1h))')
     &        ' PE + Direct contribution       ', 
     &        SNGL((1.D0 - corrmsd)*CSFus), ' mb', 
     &        SNGL((1.D0 - corrmsd)*100), ' %'
        WRITE(8,'(1x,A32,F9.2,A3,1x,1h(,F6.2,A2,1h))')
     &        ' MSD contribution               ', SNGL(xsinl), ' mb', 
     &        SNGL(xsinl/CSFus*100), ' %'
        WRITE(8,'(1x,A32,F9.2,A3,1x,1h(,F6.2,A2,1h))')
     &        ' PCROSS contribution            ', SNGL(totemis), ' mb', 
     &        SNGL(totemis/CSFus*100), ' %'
        WRITE(8,'(1x,A32,F9.2,A3)')' Total CN population            ', 
     &                             SNGL(ftmp), ' mb'
        WRITE(8,*)' '
C--------TRISTAN *** done ***
C--------Add MSD contribution to the residual nucleus population
C--------Locate residual nucleus after MSD emission
        DO i = 0, NDEjc
          nnur = NREs(i)
          IF(nnur.LT.0)CYCLE
          IF(CSMsd(i).LE.0.0D0)CYCLE
 
          CALL ACCUMSD(1,nnur,i)
C----------Add PE contribution to energy spectra (angle int.)
          ftmp = 0.D0
          DO ie = 1, NDEcse
            CSE(ie,i,1) = CSE(ie,i,1) + CSEmsd(ie,i)
            CSEt(ie,i) = CSEt(ie,i) + CSEmsd(ie,i)
            ftmp = ftmp + DE*CSEmsd(ie,i)
          ENDDO
C----------Add PE contribution to the total NEJC emission
          CSEmis(i,1) = CSEmis(i,1) + CSMsd(i)
        ENDDO
      ENDIF
 
C-----
C-----HMS Monte Carlo preequilibrium emission
C-----
      IF(EINl.GT.0.1D0.AND.LHMs.NE.0)THEN
        xizat = IZA(0)
        CALL DDHMS(IZAejc(0),xizat,XJLv(LEVtarg,0),EINl,CSFus*corrmsd,
     &             CHMs,DE,DERec,FHMs,NHMs,QDFrac,0,1,0,icalled)
        icalled = 1
C        CSEmis(1,1) = CSEmis(1,1) + CSHms(1,0)
C        CSEmis(2,1) = CSEmis(2,1) + CSHms(2,0)
        WRITE(8,
     &      '('' HMS inclusive neut. emission ='',G12.5,        ''mb'')'
     &      )CSHms(1,0)
        WRITE(8,
     &    '('' HMS inclusive prot. emission ='',G12.5,          ''mb'')'
     &    )CSHms(2,0)
      ENDIF
C-----
C-----PE + DWBA cont. *** done ***
C-----
      ia = INT(A(1))
      IF(IOUt.GT.1)THEN
        WRITE(8,*)' '
        WRITE(8,*)' '
        WRITE(8,
     &  '(''  Compound nucleus '',I3,''-'',A2,  '' spin distribution'')'
     &  )ia, SYMb(1)
        WRITE(8,*)' -----------------------------------------'
        WRITE(8,*)' '
        DO i = 1, NLW
          IF(MOD(ia,2).EQ.0)THEN
            WRITE(8,'(1X,I5,G12.5,5X,I5,G12.5)')i - 1, POP(NEX(1),i,1,1)
     &            , ( - (i - 1)), POP(NEX(1),i,2,1)
          ELSE
            WRITE(8,'(1X,I4,''/2'',G12.5,5X,I4,''/2'',G12.5)')2*i - 1, 
     &            POP(NEX(1),i,1,1), ( - (2*i - 1)), POP(NEX(1),i,2,1)
          ENDIF
        ENDDO
        WRITE(8,*)' '
      ENDIF
      IF(IOUt.GT.0)THEN
        IF(DIRect.EQ.0)THEN
          WRITE(8,
     &  '(''   Absorption cross section = '',G13.6,  '' mb including'')'
     &  )CSFus
          WRITE(8,'(''   PE (not DWBA) = '',  G13.6,'' mb'')')
     &          xsinl + totemis
        ELSEIF(DIRect.EQ.1.OR.DIRect.EQ.2)THEN
          WRITE(8,
     &  '(''   Absorption cross section = '',G13.6,  '' mb including'')'
     &  )CSFus + (SINl + SINlcc)*FCCred + SINlcont
          WRITE(8,
     &'(''   DWBA inelastic to uncoupled discrete levels = '',  G13.6,''
     & mb'')')SINl*FCCred
          WRITE(8,
     &'(''   CC inelastic to coupled discrete levels = '',  G13.6,'' mb'
     &')')SINlcc*FCCred
          WRITE(8,'(''   DWBA to continuum = '',  G13.6,'' mb'')')
     &          xsinlcont
          WRITE(8,'(''   PE (not DWBA) = '',  G13.6,'' mb'')')
     &          xsinl + totemis
          WRITE(8,
     &'(''   Spin distribution calculated using '',  ''CC transmission c
     &oefficients'')')
        ELSEIF(DIRect.EQ.3)THEN
          WRITE(8,
     &  '(''   Absorption cross section = '',G13.6,  '' mb including'')'
     &  )CSFus + (SINl + SINlcc)*FCCred + SINlcont
          WRITE(8,
     &   '(''   DWBA inelastic to discrete levels = '',  G13.6,'' mb'')'
     &   )(SINl + SINlcc)*FCCred
          WRITE(8,'(''   DWBA to continuum = '',  G13.6,'' mb'')')
     &          xsinlcont
          WRITE(8,'(''   PE (not DWBA)  = '',  G13.6,'' mb'')')
     &          xsinl + totemis
          WRITE(8,
     &'(''   Spin distribution does NOT contain'',  '' DWBA inelastic co
     &ntribution '')')
        ENDIF
      ENDIF
C
      WRITE(12,*)' '
      WRITE(12,'('' FUSION CROSS SECTION = '',G12.5,'' mb'')')
     &      CSFus + (SINl + SINlcc)*FCCred + SINlcont
      WRITE(12,'('' TOTAL  CROSS SECTION = '',G13.6, '' mb'')')
     &      TOTcs*TOTred*totcorr
      WRITE(12,*)' '
C
      POPmax(1) = CSFus*1.0E-25
      OPEN(80,FILE = 'FISSION.OUT',STATUS = 'UNKNOWN')
C-----Start DO loop over decaying nuclei
      DO nnuc = 1, NNUcd
 
        IF(QPRod(nnuc).LT. - 999.D0)CYCLE
 
        ROFisp = 0.D0   ! setting saddle point LD to zero (again as protection)
 
C        if(nnuc.le.NNUcd)
        IF(nnuc.LE.NDEjc)WRITE(*,1160)nnuc, NNUcd, INT(Z(nnuc)), 
     &                                SYMb(nnuc), INT(A(nnuc))
                            ! limiting screen printout
 1160   FORMAT(1x,'  Decaying nucleus # ',I3,' of ',I3,' (',I3,'-',A2,
     &         '-',I3,')')
 
        IF(FISsil(nnuc).AND.FISshi(nnuc).NE.1.)THEN
          CALL READ_INPFIS(nnuc)
          IF(FISmod(nnuc).LT.0.1D0)THEN      ! Single mode fission
            DO i = 1, NRHump
              IF(FISden(nnuc).EQ.0)THEN
                CALL DAMI_ROFIS(nnuc,i,0,AFIs(i))
              ELSEIF(FISden(nnuc).EQ.3)THEN
                CALL DAMI_RO_HFB_FIS(nnuc,i,AFIs(i))
              ELSE
                WRITE(8,'('' ERROR: CHECK FISDEN (not 0 or 3)!'')')
                STOP ' FATAL: CHECK FISDEN (not 0 or 3)!'
              ENDIF
            ENDDO
          ELSEIF(FISden(nnuc).EQ.0)THEN
            CALL DAMI_ROFIS(nnuc,1,0,AFIs(1))
            DO m = 1, INT(FISmod(nnuc)) + 1
              CALL DAMI_ROFIS(nnuc,2,m,AFIsm(m))
            ENDDO
          ELSE
            WRITE(8,'('' ERROR: FISmod>0 and FISDEN not 0 ! '')')
            STOP ' FATAL: FISmod>0 and FISDEN not 0 ! '
C                WRITE(8,'('' WARNING: FISmod>0 and FISDEN not 0 !'')')
C                WRITE(8,'('' WARNING: Resetting FISDEN to 0 '')')
C                WRITE(8,
C    &    '('' WARNING: Only EGSM is allowed for multimodal fission'')')
C                 FISden(Nnuc)=0
          ENDIF
          IF(NRBar.EQ.3.AND.NRWel.EQ.1.AND.FISmod(nnuc).EQ.0.)THEN
            TFIso = 2.86896*EXP(2.*PI*(EFB(2) - (EFB(3)+H(1,3)/2.))
     &              /H(1,2))/(H(1,3)*10.**21)
            TGIso = EXP(2.*PI*(EFB(1) - (EFB(3)+H(1,3)/2.))/H(1,1))
     &              /10.**14
            TISo = TFIso*TGIso/(TFIso + TGIso)
            RFIso = TGIso/(TFIso + TGIso)
          ENDIF
          CALL WRITE_OUTFIS(nnuc)
        ENDIF
        ia = INT(A(nnuc))
C--------Reset variables for life-time calculations
        stauc = 0.0
        sgamc = 0.0
        csemist = 0.0
        CSFis = 0.D0
        IF(FISmod(nnuc).GT.0.)THEN
          DO m = 1, INT(FISmod(nnuc)) + 1
            CSFism(m) = 0.D0
          ENDDO
        ENDIF
        sumfis = 0.0
        IF(IOUt.GT.0)THEN
          WRITE(8,*)' '
          WRITE(8,*)' '
          WRITE(8,*)' -------------------------------------'
          WRITE(8,'(I3,2X,''Decaying nucleus '',I3,''-'',A2)')nnuc, ia, 
     &          SYMb(nnuc)
          WRITE(8,*)' -------------------------------------'
          WRITE(8,*)' '
        ENDIF
        WRITE(12,*)' '
        WRITE(12,*)
     &' ---------------------------------------------------------------'
        IF(ABS(QPRod(nnuc) + ELV(LEVtarg,0)).GT.99.99)THEN
          WRITE(12,
     &'(''  Decaying nucleus '',I3,''-'',A2,''-'',I3,     ''  mass='',F1
     &0.6,'' Q-value='',F10.5)')INT(Z(nnuc)), SYMb(nnuc), ia, 
     &                          AMAss(nnuc), QPRod(nnuc)
     &                          + ELV(LEVtarg,0)
        ELSE
          WRITE(12,
     &'(''  Decaying nucleus '',I3,''-'',A2,''-'',I3,     ''  mass='',F1
     &0.6,'' Q-value='',F10.6)')INT(Z(nnuc)), SYMb(nnuc), ia, 
     &                          AMAss(nnuc), QPRod(nnuc)
     &                          + ELV(LEVtarg,0)
        ENDIF
        WRITE(12,*)
     &' ---------------------------------------------------------------'
        IF(FITomp.LE.0)THEN
          IF(nnuc.NE.1)THEN
            IF(nnuc.EQ.MT91)THEN
              nejc = 1
            ELSEIF(nnuc.EQ.MT649)THEN
              nejc = 2
            ELSEIF(nnuc.EQ.MT849)THEN
              nejc = 3
            ELSEIF(nnuc.EQ.1)THEN
              nejc = 0
            ELSE
              GOTO 170
            ENDIF
            dtmp = 0.D0
            DO il = 1, NLV(nnuc)
              dtmp = dtmp + CSDirlev(il,nejc)
            ENDDO
            IF(dtmp.GT.0.0)THEN
              WRITE(12,
     &'(1X,/,10X,''Discrete level population '',      ''before gamma cas
     &cade'')')
              WRITE(12,'(1X,/,10X,40(1H-),/)')
              DO il = 1, NLV(nnuc)
C-----------------Check for the number of branching ratios
                nbr = 0
                DO ib = 1, NDBr
                  IF(BR(il,ib,2,nnuc).EQ.0.)EXIT
                  nbr = ib
                ENDDO
                IF(nbr.EQ.0.AND.il.NE.1.AND.FIRst_ein.AND.
     &             (nnuc.EQ.MT91.OR.nnuc.EQ.MT649.OR.nnuc.EQ.MT849).AND.
     &             ENDf(nnuc).NE.0)WRITE(8,*)
     &             ' WARNING: Branching ratios for level ', il, ' IN ', 
     &             INT(A(nnuc)), '-', SYMb(nnuc), ' are missing'
C    &                            XJLv(il, nnuc), POPlv(il, nnuc), nbr,
                WRITE(12,1240)il, ELV(il,nnuc), LVP(il,nnuc), 
     &                        XJLv(il,nnuc), CSDirlev(il,nejc), nbr, 
     &                        (NINT(BR(il,ib,1,nnuc)),BR(il,ib,2,nnuc),
     &                        ib = 1,nbr)
C-----------------Next IF moves levels population to the ground state
C-----------------to avoid gamma cascade between discrete levels
C-----------------originating from the direct population
C-----------------of discrete levels by a neutron, proton or alpha.
C-----------------These gammas should not go into MT=91, 649, or 849.
                IF((nnuc.EQ.MT91.OR.nnuc.EQ.MT649.OR.nnuc.EQ.MT849).AND.
     &             il.NE.1.AND.ENDf(nnuc).NE.0)THEN
                  POPlv(1,nnuc) = POPlv(1,nnuc) + CSDirlev(il,nejc)
                  POPlv(il,nnuc) = POPlv(il,nnuc) - CSDirlev(il,nejc)
                ENDIF
              ENDDO
C--------------Decay direct population of discrete levels by a neutron,
C--------------proton or alpha without storing emitted gammas in the spectra.
              IF((nnuc.EQ.MT91.OR.nnuc.EQ.MT649.OR.nnuc.EQ.MT849).AND.
     &           ENDf(nnuc).NE.0.AND.il.NE.1)CALL DECAYD_DIR(nnuc,nejc)
            ENDIF
C
C--------------Write elastic to tape 12 and to tape 68
  170       IF(nnuc.EQ.MT2)THEN
              WRITE(12,'(1X,/,10X,40(1H-),/)')
              WRITE(12,*)' '
              WRITE(12,'('' ELASTIC CROSS SECTION ='',G12.5,'' mb'')')
     &              ELAcs*ELAred + 4.*PI*ELCncs
              WRITE(12,*)' '
              WRITE(12,*)' Elastic angular distribution '
              WRITE(12,*)' '
              IF(ICAlangs.GT.0)THEN
                WRITE(12,1170)(ANGles(iang),iang = 1,NANgela)
              ELSE
                delang = 180./FLOAT(NANgela - 1)
                WRITE(12,1170)(FLOAT(iang - 1)*delang,iang = 1,NANgela)
              ENDIF
 1170         FORMAT(10X,8G15.5)
              WRITE(12,1180)((ELAred*elada(iang) + ELCncs),iang = 1,
     &                      NANgela)
 1180         FORMAT(9X,8E15.5)
              WRITE(12,*)' '
              WRITE(12,*)' '
              WRITE(12,*)' Legendre coefficients expansion '
              WRITE(12,*)' '
              WRITE(12,'(1x,A7,I5)')' Lmax =', MIN(NDAng,neles)
              WRITE(12,*)' '
              WRITE(12,'(9X,8D15.8)')(ELAred*elleg(1) + ELCncs), 
     &                               (ELAred*elleg(iang),iang = 2,
     &                               MIN(NDAng,neles))
              WRITE(12,*)' '
              IF(ELCncs.EQ.0.AND.EINl.LT.10.D0)WRITE(8,*)
     &           'WARNING: CN elastic is 0'
C
              IF(FITomp.LT.0)THEN
                WRITE(40,'(F12.4,3D12.5)')EINl, TOTcs, ABScs
                IF(ncoll.GT.0)THEN
C-------------------locate position of the projectile among ejectiles
                  CALL WHEREJC(IZAejc(0),nejcec,iloc)
                  its = 2
                  DO ilv = 2, ncoll
                    DO iang = ilv + 1, ncoll
                      IF(ICOller(iang).EQ.ICOller(ilv))GOTO 175
                    ENDDO
                    itemp(its) = ICOller(ilv)
                    its = its + 1
  175             ENDDO
                  its = its - 1
                  WRITE(40,'(12x,11D12.5)')ELAcs, 
     &                  (CSDirlev(itemp(ilv),nejcec),ilv = 2,MIN(its,10)
     &                  )
                  IF(ICAlangs.GT.0)THEN
                    DO iang = 1, NDAng
                      WRITE(40,'(f12.4,11D12.5)')ANGles(iang), 
     &                      elada(iang) + ELCncs, 
     &                      (CSAlev(iang,itemp(ilv),nejcec),ilv = 2,
     &                      MIN(its,10))
                    ENDDO
                  ENDIF
                ELSE
                  WRITE(40,'(12x,11D12.5)')ELAcs
                  IF(ICAlangs.GT.0)THEN
                    DO iang = 1, NDAng
                      WRITE(40,'(f12.4,11D12.5)')ANGles(iang), 
     &                      elada(iang) + ELCncs
                    ENDDO
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
C--------Jump to end of loop after elastic when fitting
        IF(FITomp.LT.0.AND.nnuc.EQ.MT2)GOTO 190
        POPmax(nnuc) = POPmax(nnuc)*0.0001
        IF(POPmax(nnuc).EQ.0.0D0)THEN
          WRITE(8,*)' '
          WRITE(8,*)'Continuum of this nucleus has not been populated'
          GOTO 180
        ENDIF
C--------Prepare gamma transition parameters
        CALL ULM(nnuc)
C--------Calculate compound nucleus level density at saddle point
        IF(FISshi(nnuc).EQ.1.)THEN
          IF(FISsil(nnuc))THEN
            CALL ROEMP(nnuc,1.D0,0.0D0)
            IF(FIRst_ein)WRITE(8,*)
     &' WARNING: For HI reactions (FISSHI  1), LD model at saddles is EG
     &SM'
            IF(IOUt.EQ.6)THEN
              WRITE(8,'(1X,/,'' Saddle point level density'',/)')
              WRITE(8,1190)(EX(i,nnuc),(ROF(i,j,nnuc),j = 1,12),i = 1,
     &                     NEX(nnuc))
 1190         FORMAT(1X,13G10.4)
            ENDIF
          ENDIF
        ENDIF
C--------
C--------Heidelberg Multistep Compound calculations
C--------
        IF(nnuc.EQ.1.AND.MSC.NE.0)THEN
          CALL HMSC(nvwful)
          CSEmis(0,1) = CSEmis(0,1) + CSMsc(0)
          CSEmis(1,1) = CSEmis(1,1) + CSMsc(1)
          CSEmis(2,1) = CSEmis(2,1) + CSMsc(2)
C           WRITE(8,*) 'MSC: ',CSMsc(0),CSMsc(1),CSMsc(2)
          IF(nvwful)GOTO 180
        ENDIF
 
        IF(nnuc.EQ.1.AND.IOUt.GE.3.AND.
     &     (CSEmis(0,1) + CSEmis(1,1) + CSEmis(2,1) + CSEmis(3,1)
     &     + CSEmis(4,1) + CSEmis(5,1) + CSEmis(6,1)).NE.0)THEN
          WRITE(8,*)' '
          WRITE(8,*)
     &           ' Preequilibrium + Direct spectra (sum of all models):'
          IF(CSEmis(0,1).GT.0)THEN
            CALL AUERST(1,0,0)
            WRITE(8,'(2x,'' g PE emiss cross sect   '',G12.5,'' mb'')')
     &            CSEmis(0,1)
          ENDIF
          IF(CSEmis(1,1).GT.0)THEN
            CALL AUERST(1,1,0)
            WRITE(8,'(2x,'' n PE emiss cross sect   '',G12.5,'' mb'')')
     &            CSEmis(1,1)
          ENDIF
          IF(CSEmis(2,1).GT.0)THEN
            CALL AUERST(1,2,0)
            WRITE(8,'(2x,'' p PE emiss cross sect   '',G12.5,'' mb'')')
     &            CSEmis(2,1)
          ENDIF
          IF(CSEmis(3,1).GT.0)THEN
            CALL AUERST(1,3,0)
            WRITE(8,'(2x,'' a PE emiss cross sect   '',G12.5,'' mb'')')
     &            CSEmis(3,1)
          ENDIF
          IF(CSEmis(4,1).GT.0)THEN
            CALL AUERST(1,4,0)
            WRITE(8,'(2x,'' d PE emiss cross sect   '',G12.5,'' mb'')')
     &            CSEmis(4,1)
          ENDIF
          IF(CSEmis(5,1).GT.0)THEN
            CALL AUERST(1,5,0)
            WRITE(8,'(2x,'' t PE emiss cross sect   '',G12.5,'' mb'')')
     &            CSEmis(5,1)
          ENDIF
          IF(CSEmis(6,1).GT.0)THEN
            CALL AUERST(1,6,0)
            WRITE(8,'(2x,'' h PE emiss cross sect   '',G12.5,'' mb'')')
     &            CSEmis(6,1)
          ENDIF
          WRITE(8,*)
          WRITE(8,*)
        ENDIF
C--------
C--------Start Hauser-Feshbach nnuc nucleus decay
C--------
        popleft = 0.D0
C--------Turn on (KEMIN=1) or off (KEMIN=NEX(NNUC)) gamma cascade
C--------in the first CN, it is preferred to use input parameter GCASC (0=OFF,1=ON)
        kemin = 1
        IF(nnuc.EQ.1.AND.GCAsc.EQ.0.0D0)kemin = NEX(nnuc)
C--------Turn  off (KEMIN=NEX(NNUC)) gamma cascade in the case of OMP fit
        IF(FITomp.NE.0)kemin = NEX(nnuc)
        kemax = NEX(nnuc)
C--------Account for widths fluctuations (HRTW)
        IF(LHRtw.EQ.1.AND.EIN.GT.EHRtw)LHRtw = 0
        IF(nnuc.EQ.1.AND.LHRtw.GT.0)THEN
C
C           Renormalizing transmission coefficients to consider PE emission
C                 before calling HRTW
          DO i = 1, NDLw
            ELTl(i) = ELTl(i)*corrmsd
          ENDDO
          CALL HRTW
 
          IF(ENDf(1).GT.0.AND.RECoil.GT.0)CALL GET_RECOIL(kemax,nnuc)
                                          !recoil spectrum
          kemax = MAX(NEX(nnuc) - 1,1)
          IF(FISsil(nnuc).AND.FISshi(nnuc).NE.1.D0)THEN
            IF(FISmod(nnuc).EQ.0.)WRITE(80,*)'csfis=', CSFis, ' mb'
            IF(FISmod(nnuc).GT.0.)THEN
              WRITE(80,*)'  '
              DO m = 1, INT(FISmod(nnuc)) + 1
                WRITE(80,*)'    Mode=', m, '  csfis=', CSFism(m), ' mb'
              ENDDO
            ENDIF
          ENDIF
        ENDIF
C
        skip_fiss = .FALSE.
        dtmp1 = 0.D0
        dtmp0 = 0.D0
        cspg = 0.D0
 
C--------DO loop over c.n. excitation energy
        DO ke = kemax, kemin, -1
C        DO ke = kemax, kemax
          IF(ke.LE.0)CYCLE
          step = DE
          IF(ke.EQ.NEX(nnuc).OR.ke.EQ.1)step = 0.5*DE
          IF(ke.EQ.NEX(nnuc).AND.nnuc.EQ.1)step = 1.0
          IF(ENDf(1).GT.0)THEN
C--------------Clean auxiliary particle spectra for calculation of recoils
            DO nejc = 0, NEJcm
              DO il = 1, NDLv
                REClev(il,nejc) = 0.D0
              ENDDO
              DO ie = 1, NDEcse
                AUSpec(ie,nejc) = 0.D0
              ENDDO
            ENDDO
C--------------Calculate population in the energy bin ke
            pope = 0.D0
            DO jcn = 1, NLW
              pope = pope + POP(ke,jcn,1,nnuc) + POP(ke,jcn,2,nnuc)
            ENDDO
            POPbin(ke,nnuc) = pope*step
          ENDIF
 
          DO ipar = 1, 2   !over decaying nucleus parity
            ip = INT(( - 1.0)**(ipar + 1))
            DO jcn = 1, NLW    !over decaying nucleus spin
              IF(GDRdyn.EQ.1.0D0)CALL ULMDYN(nnuc,jcn,EX(ke,nnuc))
              DENhf = 0.0
              IF(POP(ke,jcn,ipar,nnuc).LT.POPmax(nnuc))THEN
                popleft = popleft + POP(ke,jcn,ipar,nnuc)*DE
                CYCLE
              ENDIF
              DO nejc = 1, NEJcm     !over ejectiles
                ares = A(nnuc) - AEJc(nejc)
                zres = Z(nnuc) - ZEJc(nejc)
C--------------------Residual nuclei must be heavier than alpha
                IF(ares.LE.4.AND.zres.LE.2.)CYCLE
                izares = INT(1000.0*zres + ares)
                CALL WHERE(izares,nnur,iloc)
                IF(iloc.EQ.1)CYCLE
                CALL DECAY(nnuc,ke,jcn,ip,nnur,nejc,sum)
              ENDDO
C-----------------DO loop over ejectiles       ***done***
C-----------------gamma emision
              CALL DECAYG(nnuc,ke,jcn,ip,sumx)
C-----------------Distribute yrast population over discrete levels
              IF(DENhf.EQ.0.0D0)THEN
                IF(ke.EQ.1)THEN
                  ded = DE*0.5
                ELSE
                  ded = DE
                ENDIF
                IF(IOUt.GT.1)WRITE(8,
     & '('' Yrast state at bin'',I4,'' spin='',F5.1,'' pop='',   G12.5)'
     & )ke, FLOAT(ip)*(FLOAT(jcn) + HIS(nnur)), POP(ke,jcn,ipar,nnuc)
     &  *ded
C
C                    Corrected on Jan 2011, Previously missed gamma XSs
C
                CSEmis(0,nnuc) = CSEmis(0,nnuc) + POP(ke,jcn,ipar,nnuc)
     &                           *ded
 
C--------------------Look for the discrete level with the closest spin
                xnl = 1.0
                spdiff = 100.
                DO il = 1, NLV(nnuc)
                  spdif = ABS(FLOAT(jcn) + HIS(nnur) - XJLv(il,nnuc))
                  IF(spdif.LT.spdiff)THEN
                    spdiff = spdif
                    xnl = 1.
                  ELSE
                    IF(spdif.EQ.spdiff)xnl = xnl + 1.
                  ENDIF
                ENDDO
                DO il = 1, NLV(nnuc)
                  spdif = ABS(FLOAT(jcn) + HIS(nnur) - XJLv(il,nnuc))
                  IF(spdif.EQ.spdiff)THEN
                    SCRtl(il,0) = 1.0D0/xnl
                    DENhf = DENhf + SCRtl(il,0)
                    IF(IOUt.GT.1)WRITE(8,
     &       '(10X,I3,''% of this was assumed to populate level #'',I3)'
     &       )INT(100./xnl), il
                  ENDIF
                ENDDO
              ENDIF
C-----------------
C-----------------Fission ()
              IF(.NOT.skip_fiss)THEN
                IF(FISsil(nnuc).AND.(FISshi(nnuc).EQ.1.))
     &             CALL FISSION(nnuc,ke,jcn,sumfis)
                IF(FISsil(nnuc).AND.(FISshi(nnuc).NE.1.))
     &             CALL FISCROSS(nnuc,ke,ip,jcn,sumfis,sumfism,dencomp,
     &             aafis,0)
              ELSE
                sumfis = 0.D0
                aafis = 0.D0
                DO m = 1, INT(FISmod(nnuc)) + 1
                  sumfism(m) = 0.D0
                ENDDO
              ENDIF
C-----------------Normalization and accumulation
C-----------------
              xnor = POP(ke,jcn,ipar,nnuc)*step/DENhf
              stauc = stauc + RO(ke,jcn,ipar,nnuc)*xnor
              IF(RO(ke,jcn,ipar,nnuc).NE.0.0D0)sgamc = sgamc + 
     &           DENhf*POP(ke,jcn,ipar,nnuc)*step/RO(ke,jcn,ipar,nnuc)
              CALL XSECT(nnuc,m,xnor,sumfis,sumfism,ke,ipar,jcn,dencomp,
     &                   aafis,fisxse)
C
C                 It should be updated for the multimodal fission
C                                          fisxse ->  fisxse(m)
C
              dtmp1 = dtmp1 + fisxse
C
C-----------------Calculate total emission
              DO nejc = 0, NEJcm
                csemist = csemist + CSEmis(nejc,nnuc)
              ENDDO
              csemist = csemist + CSFis
            ENDDO                   !loop over decaying nucleus spin
          ENDDO                     !loop over decaying nucleus parity
C
C           the following if could be commented to calculate fission for
C           all excitation energies
C           if(dtmp1.lt.dabs(dtmp1-dtmp0).lt.1.d-5) skip_fiss = .TRUE.
          dtmp0 = dtmp1
 
          IF(ENDf(nnuc).GT.0.AND.RECoil.GT.0)CALL GET_RECOIL(ke,nnuc)
                                        !recoil spectrum for ke bin
          IF(FISsil(nnuc).AND.FISshi(nnuc).NE.1.D0)THEN
            IF(FISmod(nnuc).EQ.0..AND..NOT.skip_fiss)WRITE(80,*)
     &         'csfis=', CSFis, ' mb', '   fisxse=', dtmp1, ' mb'
            IF(FISmod(nnuc).GT.0..AND.dtmp1.GE.0.D0)THEN
              WRITE(80,*)'  '
              DO m = 1, INT(FISmod(nnuc)) + 1
                WRITE(80,*)'    Mode=', m, '  csfis=', CSFism(m), ' mb'
              ENDDO
            ENDIF
          ENDIF
        ENDDO                   !loop over c.n. excitation energy
C--------
C--------Hauser-Feshbach decay of nnuc  ***done***
C--------
C--------Printout of results for the decay of NNUC nucleus
        IF(IOUt.GT.0)WRITE(8,
     &'(1X,/,'' Population neglected because too'',                     
     &          '' small '',G12.5,/)')popleft*DE
  180   dtmp = 0.D0
        DO il = 1, NLV(nnuc)
          dtmp = dtmp + POPlv(il,nnuc)
        ENDDO
        IF(dtmp.GT.0.D0)THEN
 
          IF(ENDf(nnuc).GT.0)WRITE(8,
     &'(3X,''NOTE: Due to ENDF option discrete levels contribution'',/, 
     &  3X,''NOTE:   was not included in emission spectra'')')
          IF(IOUt.GT.0)WRITE(8,
     &                       '(1X,/,10X,''Discrete level population'')')
          IF(IOUt.GT.0.AND.kemin.EQ.NEX(nnuc).AND.nnuc.EQ.1)WRITE(8,
     &'(10X,''(no gamma cascade in the compound nucleus, primary transit
     &ions only)'',/)')
          IF(IOUt.GT.0.AND.ENDf(nnuc).NE.0.AND.
     &       (nnuc.EQ.MT91.OR.nnuc.EQ.MT649.OR.nnuc.EQ.MT849))WRITE(8,
     &'(3X,''NOTE: due to ENDF option direct particle contribution was s
     &hifted to the g.s.'')')
          IF(IOUt.GT.0)WRITE(8,'(1X,/,10X,40(1H-),/)')
C
C        Primary gamma printout -----------------------
C
          IF(nnuc.EQ.1.AND.NPRim_g.GT.0)THEN
            cspg = 0.D0
            DO il = 1, NLV(nnuc)
              cspg = cspg + CSEpg(il)
            ENDDO
            IF(cspg.GT.0.D0)THEN
              WRITE(12,'(1X,/,10X,40(1H-),/)')
              WRITE(12,
     & '(2x,     '' Primary g  emission cross section'',G12.5,''  mb'')'
     & )cspg
              WRITE(12,'(1X,/,10X,40(1H-),/)')
              WRITE(12,'(11x,A1,A12,A6,A5,4x,A12,A7,1x,A6,1x,A10)')'i', 
     &              '    Elv(i)  ', 'Par  ', ' Spin', ' Prim.g CS   ', 
     &              ' Branch', '  Egamma  '
              WRITE(12,*)' '
              DO il = 1, NLV(nnuc)
                WRITE(12,1200)il, ELV(il,nnuc), LVP(il,nnuc), 
     &                        XJLv(il,nnuc), CSEpg(il), CSEpg(il)
     &                        /cspg*100., ENPg(il)
 1200           FORMAT(I12,F10.5,I5,F8.1,G15.6,1x,F6.2,1x,F10.5)
              ENDDO
              WRITE(12,'(1X,/,10X,40(1H-),/)')
 
              WRITE(8,'(1X,/,10X,40(1H-),/)')
              WRITE(8,
     & '(2x,     '' Primary g  emission cross section'',G12.5,''  mb'')'
     & )cspg
              WRITE(8,'(1X,/,10X,40(1H-),/)')
              WRITE(8,'(11x,A1,A12,A6,A5,4x,A12,A7,1x,A6,1x,A10)')'i', 
     &              '    Elv(i)  ', 'Par  ', ' Spin', ' Prim.g CS   ', 
     &              ' Branch', '  Egamma  '
              WRITE(8,*)' '
              DO il = 1, NLV(nnuc)
                WRITE(8,1200)il, ELV(il,nnuc), LVP(il,nnuc), 
     &                       XJLv(il,nnuc), CSEpg(il), CSEpg(il)
     &                       /cspg*100., ENPg(il)
              ENDDO
              WRITE(8,'(1X,/,10X,40(1H-),/)')
 
            ENDIF
C          Primary gammas -------- done ---------------
            WRITE(12,
     &'(1X,/,10X,''Discrete level population '',              ''before g
     &amma cascade'')')
            WRITE(12,'(1X,/,10X,40(1H-),/)')
          ENDIF
          DO il = 1, NLV(nnuc)
            CSPrd(nnuc) = CSPrd(nnuc) + POPlv(il,nnuc)
            IF(ISIsom(il,nnuc).EQ.0)THEN
              IF(IOUt.GT.0)WRITE(8,1240)il, ELV(il,nnuc), LVP(il,nnuc), 
     &                                  XJLv(il,nnuc), POPlv(il,nnuc)
            ELSE
 1210         FORMAT(I12,F10.5,I5,F8.1,G15.6,A7)
              IF(IOUt.GT.0)WRITE(8,1210)il, ELV(il,nnuc), LVP(il,nnuc), 
     &                                  XJLv(il,nnuc), POPlv(il,nnuc), 
     &                                  ' ISOMER'
            ENDIF
 
            IF(nnuc.EQ.1)THEN
C--------------Check for the number of branching ratios
              nbr = 0
              DO ib = 1, NDBr
                IF(BR(il,ib,2,nnuc).EQ.0.)EXIT
                nbr = ib
              ENDDO
              IF(nbr.EQ.0.AND.il.NE.1.AND.FIRst_ein)WRITE(8,*)
     &           ' WARNING: Branching ratios for level ', il, ' in ', 
     &           INT(A(nnuc)), '-', SYMb(nnuc), ' are missing'
              WRITE(12,1240)il, ELV(il,nnuc), LVP(il,nnuc), 
     &                      XJLv(il,nnuc), POPlv(il,nnuc), nbr, 
     &                      (NINT(BR(il,ib,1,nnuc)),BR(il,ib,2,nnuc),
     &                      ib = 1,nbr)
            ENDIF
          ENDDO
 
          IF(CSPrd(nnuc).GT.0.D0.AND.
     &       (nnuc.EQ.1.OR.nnuc.EQ.MT91.OR.nnuc.EQ.MT649.OR.
     &       nnuc.EQ.MT849))THEN
            WRITE(12,'(1X,/,10X,40(1H-),/)')
            WRITE(12,*)' '
C-----------Write Int. Conv. Coefff. for discrete transitions
            WRITE(12,
     &    '(1X,/,10X,             ''Internal conversion coefficients'')'
     &    )
            WRITE(12,'(1X,/,10X,40(1H-),/)')
            DO il = 1, NLV(nnuc)
C-------------Check for the number of branching ratios
              nbr = 0
              DO ib = 1, NDBr
                IF(BR(il,ib,2,nnuc).EQ.0.)EXIT
                nbr = ib
              ENDDO
              WRITE(12,1220)il, ELV(il,nnuc), LVP(il,nnuc), 
     &                      XJLv(il,nnuc), POPlv(il,nnuc), nbr, 
     &                      (NINT(BR(il,ib,1,nnuc)),BR(il,ib,3,nnuc),
     &                      ib = 1,nbr)
 1220         FORMAT(I12,F10.5,I5,F8.1,G15.6,I3,7(I4,E11.4),:/,
     &               (53X,7(I4,E11.4)))
            ENDDO
            WRITE(12,'(1X,/,10X,40(1H-),/)')
          ENDIF
C--------gamma decay of discrete levels (DECAYD)
          CALL DECAYD(nnuc)
        ENDIF
        ia = INT(A(nnuc))
        iz = INT(Z(nnuc))
        IF(IOUt.GT.0)THEN
          WRITE(8,'(1X,/,10X,40(1H-),/)')
          WRITE(8,
     &'(1X,I3,''-'',A2,''-'',I3,'' production cross section '',G12.5,'' 
     &mb  '',''reaction: '',A21)')iz, SYMb(nnuc), ia, CSPrd(nnuc), 
     &                            REAction(nnuc)
          IF(kemin.EQ.NEX(nnuc).AND.nnuc.EQ.1)WRITE(8,
     &'(1X,''(no gamma cascade in the compound nucleus, primary transiti
     &ons only)'',/)')
        ENDIF
 
        IF(CSPrd(nnuc).GT.0.D0)THEN
C----------Integrating exclusive population spectra (ENDF)
          gtotsp = 0
          xtotsp = 0
          ptotsp = 0
          atotsp = 0
          dtotsp = 0
          ttotsp = 0
          htotsp = 0
          ctotsp = 0
          emedg = 0
          emedn = 0
          emedp = 0
          emeda = 0
          emedd = 0
          emedt = 0
          emedh = 0
          emedc = 0
          gcs = 0
          ncs = 0
          pcs = 0
          acs = 0
          dcs = 0
          tcs = 0
          hcs = 0
C          write(*,'(2x,F3.0,1x,F3.0,2x,A3,2x,F3.1)')
C    &                 A(nnuc),Z(nnuc),' - ',ENDF(nnuc)
          IF(ENDf(nnuc).EQ.1)THEN
            DO ispec = 1, MIN(NEX(1) + 10,NDEcsed)
              gtotsp = gtotsp + POPcse(0,0,ispec,INExc(nnuc))*DE
              gcs = gcs + CSE(ispec,0,nnuc)*DE
C              Write(12,*) nnuc,ispec,'g: ',
C     &           POPcse(0,0,ispec,INExc(nnuc)),CSE(ispec,0,nnuc)
              xtotsp = xtotsp + POPcse(0,1,ispec,INExc(nnuc))*DE
              ncs = ncs + CSE(ispec,1,nnuc)*DE
C              Write(12,*) nnuc,ispec,'n: ',
C     &           POPcse(0,1,ispec,INExc(nnuc)),CSE(ispec,1,nnuc)
              ptotsp = ptotsp + POPcse(0,2,ispec,INExc(nnuc))*DE
              pcs = pcs + CSE(ispec,2,nnuc)*DE
C              Write(12,*) nnuc,ispec,'p: ',
C     &           POPcse(0,2,ispec,INExc(nnuc)),CSE(ispec,2,nnuc)
              atotsp = atotsp + POPcse(0,3,ispec,INExc(nnuc))*DE
              acs = acs + CSE(ispec,3,nnuc)*DE
C              Write(12,*) nnuc,ispec,'a: ',
C     &           POPcse(0,3,ispec,INExc(nnuc)),CSE(ispec,3,nnuc)
              dtotsp = dtotsp + POPcse(0,4,ispec,INExc(nnuc))*DE
              dcs = dcs + CSE(ispec,4,nnuc)*DE
C              Write(12,*) nnuc,ispec,'d: ',
C     &          POPcse(0,4,ispec,INExc(nnuc)),CSE(ispec,4,nnuc)
              ttotsp = ttotsp + POPcse(0,5,ispec,INExc(nnuc))*DE
              tcs = tcs + CSE(ispec,5,nnuc)*DE
C              Write(12,*) nnuc,ispec,'t: ',
C     &          POPcse(0,5,ispec,INExc(nnuc)),CSE(ispec,5,nnuc)
              htotsp = htotsp + POPcse(0,6,ispec,INExc(nnuc))*DE
              hcs = hcs + CSE(ispec,6,nnuc)*DE
C              Write(12,*) nnuc,ispec,'h: ',
C     &          POPcse(0,6,ispec,INExc(nnuc)),CSE(ispec,6,nnuc)
              emedg = emedg + POPcse(0,0,ispec,INExc(nnuc))
     &                *DE*(ispec - 1)*DE
              emedn = emedn + POPcse(0,1,ispec,INExc(nnuc))
     &                *DE*(ispec - 1)*DE
              emedp = emedp + POPcse(0,2,ispec,INExc(nnuc))
     &                *DE*(ispec - 1)*DE
              emeda = emeda + POPcse(0,3,ispec,INExc(nnuc))
     &                *DE*(ispec - 1)*DE
              emedd = emedd + POPcse(0,4,ispec,INExc(nnuc))
     &                *DE*(ispec - 1)*DE
              emedt = emedt + POPcse(0,5,ispec,INExc(nnuc))
     &                *DE*(ispec - 1)*DE
              emedh = emedh + POPcse(0,6,ispec,INExc(nnuc))
     &                *DE*(ispec - 1)*DE
              IF(NDEjc.EQ.7)THEN
                ctotsp = ctotsp + POPcse(0,NDEjc,ispec,INExc(nnuc))*DE
                emedc = emedc + POPcse(0,NDEjc,ispec,INExc(nnuc))
     &                  *DE*(ispec - 1)*DE
              ENDIF
            ENDDO
            POPcs(0,INExc(nnuc)) = gtotsp
            POPcs(1,INExc(nnuc)) = xtotsp
            POPcs(2,INExc(nnuc)) = ptotsp
            POPcs(3,INExc(nnuc)) = atotsp
            POPcs(4,INExc(nnuc)) = dtotsp
            POPcs(5,INExc(nnuc)) = ttotsp
            POPcs(6,INExc(nnuc)) = htotsp
            IF(NDEjc.EQ.7)POPcs(NDEjc,INExc(nnuc)) = ctotsp
 
            WRITE(12,*)
            DO nejc = 0, NDEjc          !loop over ejectiles
              IF(POPcs(nejc,INExc(nnuc)).LE.1.D-6)CYCLE
              IF(nejc.EQ.0)THEN
                cejectile = 'gammas   '
              ELSEIF(nejc.EQ.1)THEN
                cejectile = 'neutrons '
              ELSEIF(nejc.EQ.2)THEN
                cejectile = 'protons  '
              ELSEIF(nejc.EQ.3)THEN
                cejectile = 'alphas   '
              ELSEIF(nejc.EQ.4)THEN
                cejectile = 'deuterons'
              ELSEIF(nejc.EQ.5)THEN
                cejectile = 'tritons  '
              ELSEIF(nejc.EQ.6)THEN
                cejectile = 'helium-3 '
              ELSEIF(nejc.EQ.NDEjc)THEN
                cejectile = 'lt. ions '
              ENDIF
              WRITE(12,1230)iz, SYMb(nnuc), ia, POPcs(nejc,INExc(nnuc)), 
     &                      cejectile
 
 1230         FORMAT(1X,I3,'-',A2,'-',I3,' population cross section ',
     &               G12.6,' mb    : ',A9)
            ENDDO
            WRITE(12,*)
 
            IF(gtotsp.NE.0)emedg = emedg/gtotsp
            IF(xtotsp.NE.0)emedn = emedn/xtotsp
            IF(ptotsp.NE.0)emedp = emedp/ptotsp
            IF(atotsp.NE.0)emeda = emeda/atotsp
            IF(dtotsp.NE.0)emedd = emedd/dtotsp
            IF(ttotsp.NE.0)emedt = emedt/ttotsp
            IF(htotsp.NE.0)emedh = emedh/htotsp
            IF(ctotsp.NE.0)emedc = emedc/ctotsp
C--------------Add contributions to discrete levels for MT=91,649,849
C--------------(merely for checking purpose)
            xnorm(1,INExc(nnuc)) = 1.0D0
            xnorm(2,INExc(nnuc)) = 1.0D0
            IF(nnuc.EQ.MT91)THEN
              nejc = 1
              WRITE(8,
     &'(11X,'' Cont. popul. before g-cascade '',                G12.5,''
     & mb  '')')xtotsp
              WRITE(12,
     &'(11X,'' Cont. popul. before g-cascade '',                G12.5,''
     & mb  '')')xtotsp
              WRITE(8,
     &'(11X,'' Disc. popul. before g-cascade '',                G12.5,''
     & mb  '')')CSDirlev(1,nejc)
              WRITE(12,
     &'(11X,'' Disc. popul. before g-cascade '',                G12.5,''
     & mb  '')')CSDirlev(1,nejc)
              xtotsp = xtotsp + CSDirlev(1,nejc)
C                 DO ilev = 1, NLV(nnuc)
C                    xtotsp = xtotsp + CSDirlev(ilev,nejc)
C                 ENDDO
            ELSEIF(nnuc.EQ.MT649)THEN
              nejc = 2
              WRITE(8,
     &'(11X,'' Cont. popul. before g-cascade '',                G12.5,''
     & mb  '')')ptotsp
              WRITE(12,
     &'(11X,'' Cont. popul. before g-cascade '',                G12.5,''
     & mb  '')')ptotsp
              WRITE(8,
     &'(11X,'' Disc. popul. before g-cascade '',                G12.5,''
     & mb  '')')CSDirlev(1,nejc)
              WRITE(12,
     &'(11X,'' Disc. popul. before g-cascade '',                G12.5,''
     & mb  '')')CSDirlev(1,nejc)
              ptotsp = ptotsp + CSDirlev(1,nejc)
C                 DO ilev = 1, NLV(nnuc)
C                    ptotsp = ptotsp + CSDirlev(ilev,nejc)
C                 ENDDO
            ELSEIF(nnuc.EQ.MT849)THEN
              nejc = 3
              WRITE(8,
     &'(11X,'' Cont. popul. before g-cascade '',                G12.5,''
     & mb  '')')atotsp
              WRITE(8,
     &'(11X,'' Disc. popul. before g-cascade '',                G12.5,''
     & mb  '')')CSDirlev(1,nejc)
              WRITE(12,
     &'(11X,'' Cont. popul. before g-cascade '',                G12.5,''
     & mb  '')')atotsp
              WRITE(12,
     &'(11X,'' Disc. popul. before g-cascade '',                G12.5,''
     & mb  '')')CSDirlev(1,nejc)
              atotsp = atotsp + CSDirlev(1,nejc)
C                 DO ilev = 1, NLV(nnuc)
C                    atotsp = atotsp + CSDirlev(ilev,nejc)
C                 ENDDO
            ELSEIF(atotsp.LT.1.0D-8)THEN
              totsp = CSPrd(nnuc) - dtotsp - htotsp - ttotsp
              IF(NDEjc.EQ.7)totsp = totsp - ctotsp
              nxsp = 0
              IF(xtotsp.GT.0.0D0)THEN
                nxsp = INT(xtotsp/totsp + 0.5D0)
                IF(ttotsp.GT.0.0D0)THEN
                  xnorm(1,INExc(nnuc)) = (nxsp*totsp + dtotsp)/xtotsp
                  xtotsp = nxsp*totsp + dtotsp
                ELSE
                  xnorm(1,INExc(nnuc)) = nxsp*totsp/xtotsp
                  xtotsp = nxsp*totsp
                ENDIF
                POPcs(1,INExc(nnuc)) = xtotsp
                IF(ABS(1.0D0 - xnorm(1,INExc(nnuc))).GT.0.01D0)WRITE(8,
     &'(''WARNING! Exclusive neutron spectrum renormalized by'',        
     &                       f6.3)')xnorm(1,INExc(nnuc))
              ENDIF
              npsp = 0
              IF(ptotsp.GT.0.0D0)THEN
                npsp = INT(ptotsp/totsp + 0.5D0)
                IF(htotsp.GT.0.0D0)THEN
                  xnorm(2,INExc(nnuc)) = (npsp*totsp + dtotsp)/ptotsp
                  ptotsp = npsp*totsp + dtotsp
                ELSE
                  xnorm(2,INExc(nnuc)) = npsp*totsp/ptotsp
                  ptotsp = npsp*totsp
                ENDIF
                POPcs(2,INExc(nnuc)) = ptotsp
                IF(ABS(1.0D0 - xnorm(2,INExc(nnuc))).GT.0.01D0)WRITE(8,
     &'(''WARNING! Exclusive  proton spectrum renormalized by'',        
     &             f6.3)')xnorm(2,INExc(nnuc))
              ENDIF
            ENDIF
 
            WRITE(8,*)' '
            WRITE(8,*)' '
            WRITE(8,*)
     &               '-------------------------------------------------'
            WRITE(8,*)
     &               'Population of residual nuclei (exclusive spectra)'
            WRITE(8,
     &'('' Energy'',14x,''gamma'',9x,''neutron'',8x,             ''proto
     &n'',10x,''alpha'',10x,''deut '',10x,             ''trit '',10x,''H
     &e-3 '')')
            WRITE(8,*)
     &               '-------------------------------------------------'
            DO ispec = 1, MIN(NEX(1) + 10,NDEcsed)
              POPcse(0,1,ispec,INExc(nnuc)) = xnorm(1,INExc(nnuc))
     &          *POPcse(0,1,ispec,INExc(nnuc))
              POPcse(0,2,ispec,INExc(nnuc)) = xnorm(2,INExc(nnuc))
     &          *POPcse(0,2,ispec,INExc(nnuc))
              IF(NDEjc.EQ.7)THEN
                WRITE(8,'(9g15.5)')(ispec - 1)*DE, 
     &                             POPcse(0,0,ispec,INExc(nnuc)), 
     &                             POPcse(0,1,ispec,INExc(nnuc)), 
     &                             POPcse(0,2,ispec,INExc(nnuc)), 
     &                             POPcse(0,3,ispec,INExc(nnuc)), 
     &                             POPcse(0,4,ispec,INExc(nnuc)), 
     &                             POPcse(0,5,ispec,INExc(nnuc)), 
     &                             POPcse(0,6,ispec,INExc(nnuc)), 
     &                             POPcse(0,NDEjc,ispec,INExc(nnuc))
              ELSE
                WRITE(8,'(8g15.5)')(ispec - 1)*DE, 
     &                             POPcse(0,0,ispec,INExc(nnuc)), 
     &                             POPcse(0,1,ispec,INExc(nnuc)), 
     &                             POPcse(0,2,ispec,INExc(nnuc)), 
     &                             POPcse(0,3,ispec,INExc(nnuc)), 
     &                             POPcse(0,4,ispec,INExc(nnuc)), 
     &                             POPcse(0,5,ispec,INExc(nnuc)), 
     &                             POPcse(0,6,ispec,INExc(nnuc))
              ENDIF
            ENDDO
            WRITE(8,*)'-----------------------------------------'
            WRITE(8,'(15X,8g15.6)')gtotsp, xtotsp, ptotsp, atotsp, 
     &                             dtotsp, ttotsp, htotsp, ctotsp
            WRITE(8,'(''E-aver.'',8X,8g15.6)')emedg, emedn, emedp, 
     &            emeda, emedd, emedt, emedh, emedc
            WRITE(8,*)'-----------------------------------------'
            WRITE(8,*)' '
          ENDIF
        ENDIF
        IF(FISmod(nnuc).GT.0)THEN
          CSFis = 0.D0   ! RCN Jan 2006
          DO m = 1, INT(FISmod(nnuc)) + 1
            CSFis = CSFis + CSFism(m)
          ENDDO
        ENDIF
        IF(CSFis.NE.0.0D0)THEN
          WRITE(80,*)
          WRITE(8,*)
          IF(IOUt.GT.0)THEN
 
            DO m = 1, INT(FISmod(nnuc)) + 1
              WFIsm(m) = 0.D0
              IF(CSFis.GT.0.)WFIsm(m) = CSFism(m)/CSFis
              IF(FISmod(nnuc).GT.0.AND.FISshi(nnuc).NE.1)WRITE(80,*)
     &           '    Mode=', m, '   weight=', WFIsm(m)
            ENDDO
            IF(FISshi(nnuc).NE.1.D0)WRITE(80,*)
     &         '   Fission cross section=', CSFis, ' mb'
          ENDIF
          CSPfis(nnuc) = CSFis
          WRITE(8,
     &'(1X,I3,''-'',A2,''-'',I3,'' fission cross  section '',G12.5,''mb 
     & ''/)')iz, SYMb(nnuc), ia, CSFis
C
C          GAMT should be defined if lifetime is going to be calculated
C
C          IF (IOUt.GT.0) THEN
C-------------Calculate average fission life-time and width
C             tauf = stauc*6.589E-22*2.0*PI/CSFis
C             WRITE(8,
C    &         '(''  Average fission life-time'',G12.5,'' s'')') tauf
C              gamfis = gamt*CSFis/csemist
C             WRITE (8,
C    &        '(''  Average fission width    '',G12.5,'' MeV'')') gamfis
C             WRITE (8,*) ' '
C          ENDIF
        ENDIF
        TOTcsfis = TOTcsfis + CSFis
C--------Add compound elastic to shape elastic before everything falls
C--------down on the ground state
        IF(nnuc.EQ.1.AND.INT(AEJc(0)).NE.0.AND.POPlv(LEVtarg,MT2).GT.0.)
     &     THEN
          WRITE(8,*)
          WRITE(8,*)' Incident energy (CMS)      ', EIN, ' MeV'
          WRITE(8,*)' Shape elastic cross section', ELAred*ELAcs, ' mb'
          WRITE(8,*)' CN elastic cross section   ', POPlv(LEVtarg,MT2), 
     &              ' mb'
C----------CN contribution to elastic ddx
          ELCncs = POPlv(LEVtarg,MT2)/4.0/PI
          WRITE(8,*)' CN elastic angular distrib.', ELCncs, ' mb/str'
          WRITE(8,*)
        ENDIF
        checkxs = checkxs + CSPrd(nnuc)
        WRITE(12,*)' '
        WRITE(12,
     &'(1X,I3,''-'',A2,''-'',I3,'' production cross section '',G12.6,'' 
     &mb'')')iz, SYMb(nnuc), ia, CSPrd(nnuc)
        jz = INT(Z(1)) - iz
        jn = INT(A(1)) - ia - jz
        checkprd = CSPrd(nnuc)
        xcross(NDEjc + 2,jz,jn) = CSPrd(nnuc)
        IF(CSPrd(nnuc).GT.0.D0)THEN
          metas = 0
          ftmp_gs = CSPrd(nnuc)
          DO l = NLV(nnuc), 2, -1
            IF(ISIsom(l,nnuc).EQ.1)THEN
              metas = metas + 1
              WRITE(12,
     &'(1X,I3,''-'',A2,''-'',I3,         '' isomer state population  '',
     &G12.6,         '' mb (m'',I1,'' E='',F7.4,''MeV Jp='',F5.1,'')'')'
     &)iz, SYMb(nnuc), ia, POPlv(l,nnuc), metas, ELV(l,nnuc), 
     & LVP(l,nnuc)*XJLv(l,nnuc)
              ftmp_gs = ftmp_gs - POPlv(l,nnuc)
C             CSPrd(nnuc) = CSPrd(nnuc) - POPlv(l,Nnuc)
            ENDIF
          ENDDO
          IF(metas.GT.0)WRITE(12,
     &'(1X,I3,''-'',A2,''-'',I3,           '' ground state population  '
     &',G12.6,'' mb'')')iz, SYMb(nnuc), ia, ftmp_gs
        ENDIF
        IF(CSFis.GT.0.)WRITE(12,
     &                  '(4x,''fission  cross section'',G12.5,''  mb'')'
     &                  )CSFis
        checkprd = checkprd + CSFis
        xcross(NDEjc + 1,jz,jn) = CSFis
        IF(CSEmis(0,nnuc).GT.0.)THEN
          IF(IOUt.GT.2)CALL AUERST(nnuc,0,0)
          WRITE(8,'(''  g  emission cross section'',G12.5,''  mb'')')
     &          CSEmis(0,nnuc)
          WRITE(12,
     &'(10x,                 '' g  emission cross section'',G12.5,''  mb
     &'')')CSEmis(0,nnuc)
 
          IF(nnuc.EQ.1)THEN
            WRITE(8,
     & '(2x,     '' Primary g  emission cross section'',G12.5,''  mb'')'
     & )cspg
            WRITE(12,
     & '(2x,     '' Primary g  emission cross section'',G12.5,''  mb'')'
     & )cspg
          ENDIF
        ENDIF
        xcross(0,jz,jn) = CSEmis(0,nnuc)
C----------------------------------------------------------------------
        IF(CSPrd(nnuc).GT.0.D0)THEN
          DO nejc = 1, NEJcm
            ares = A(nnuc) - AEJc(nejc)
            zres = Z(nnuc) - ZEJc(nejc)
C------------Residual nuclei must be heavier than alpha
            IF(ares.LE.4.AND.zres.LE.2.)CYCLE
            izares = INT(1000.0*zres + ares)
            CALL WHERE(izares,nnur,iloc)
            checkprd = checkprd + CSEmis(nejc,nnuc)
            xcross(nejc,jz,jn) = CSEmis(nejc,nnuc)
            IF(iloc.EQ.1.AND.CSEmis(nejc,nnuc).GT.0.0)WRITE(12,
     &         '('' iloc=1! CSEmis('',i1,'','',i2,'')='',G12.5)')nejc, 
     &         nnuc, CSEmis(nejc,nnuc)
            IF(iloc.EQ.1)CYCLE
            IF(CSEmis(nejc,nnuc).LE.1.D-8)CYCLE
 
            WRITE(12,
     &            '(11X,A2,'' emission cross section'',G12.5,''  mb'')')
     &            SYMbe(nejc), CSEmis(nejc,nnuc)
            IF(ENDf(nnuc).EQ.1.AND.FIRst_ein.AND.IOUt.GT.5.AND.AEJc(0)
     &         .LE.4.)CALL PLOT_EMIS_SPECTRA(nnuc,nejc)
                                 ! excluding HI reactions
            IF(IOUt.GT.0)CALL AUERST(nnuc,nejc,0)
C------------Print residual nucleus population
            poptot = 0.0
            IF(NEX(nnur).GT.0)THEN    !avoid summing non-existent continuum
              DO j = 1, NLW
                DO i = 1, NEX(nnur)
                  poptot = poptot + POP(i,j,1,nnur) + POP(i,j,2,nnur)
                ENDDO
                poptot = poptot - 0.5*(POP(1,j,1,nnur) + POP(1,j,2,nnur)
     &                   )
     &                   - 0.5*(POP(NEX(nnur),j,1,nnur) + POP(NEX(nnur),
     &                   j,2,nnur))
 
              ENDDO
            ENDIF
            poptot = poptot*DE
            poplev = 0.0
            DO i = 1, NLV(nnur)
              poplev = poplev + POPlv(i,nnur)
            ENDDO
            IF(LHMs.NE.0)THEN
              IF(nejc.GT.2)THEN
                poptot = poptot - POPcon(nnur)
                poplev = poplev - POPdis(nnur)
              ELSE
                poptot = poptot + CSHms(1,nnur) + CSHms(2,nnur)
              ENDIF
            ENDIF
 
            IF(A(nnuc).EQ.A(1).AND.Z(nnuc).EQ.Z(1).AND.ENDf(nnuc).GT.0)
     &         THEN
              WRITE(12,
     &             '(13x,   '' total popul.continuum '',G12.5,''  mb'')'
     &             )poptot
              WRITE(12,
     &             '(13x,   '' total popul.disc.lev. '',G12.5,''  mb'')'
     &             )poplev
              WRITE(12,
     &             '(13x,   '' total population      '',G12.5,''  mb'')'
     &             )poplev + poptot
 
              WRITE(8,
     &              '(1x,''    Total popul.continuum '',G12.5,''  mb'')'
     &              )poptot
              WRITE(8,
     &              '(1x,''    Total popul.disc.lev. '',G12.5,''  mb'')'
     &              )poplev
              WRITE(8,
     &              '(1x,''    Total population      '',G12.5,''  mb'')'
     &              )poplev + poptot
            ENDIF
 
            WRITE(8,'(2X,A2,'' emission cross section'',G12.5,''  mb'')'
     &            )SYMbe(nejc), CSEmis(nejc,nnuc)
            WRITE(8,*)' '
 
            IF(IOUt.EQ.4)THEN
              ia = INT(A(nnur))
              WRITE(8,*)' '
              WRITE(8,*)'**************************** '
              WRITE(8,'('' Residual nucleus '',I3,''-'',A2,/)')ia, 
     &              SYMb(nnur)
              WRITE(8,'('' Positive parities population'',/)')
              DO i = NEX(nnur), 1, -1
                ftmp = 0.0
                DO j = 1, 12
                  ftmp = ftmp + POP(i,j,1,nnur)
                ENDDO
                IF(ftmp.GT.0.0)WRITE(8,1250)EX(i,nnur), 
     &                               (POP(i,j,1,nnur),j = 1,12)
              ENDDO
              WRITE(8,*)' '
              WRITE(8,'('' Negative parities population'',/)')
              DO i = NEX(nnur), 1, -1
                ftmp = 0.0
                DO j = 1, 12
                  ftmp = ftmp + POP(i,j,2,nnur)
                ENDDO
                IF(ftmp.GT.0.0)WRITE(8,1250)EX(i,nnur), 
     &                               (POP(i,j,2,nnur),j = 1,12)
              ENDDO
              WRITE(8,'('' '')')
 
            ENDIF
 
          ENDDO    !over ejectiles
C          WRITE (12,
C    &       '(9x,'' Tot prod+emi cross section'',G12.5,''  mb'')')
C    &               checkprd
C          WRITE ( 8,
C    &       '(9x,'' Tot prod+emi cross section'',G12.5,''  mb'')')
C    &               checkprd
          xcross(NDEjc + 3,jz,jn) = checkprd
 
        ENDIF  ! if CSProd > 0
C--------
C--------NNUC nucleus decay    **** done ******
C--------
      ENDDO     !over decaying nuclei
C-----Write a row in the table of cross sections (Note: inelastic has CN elastic subtracted)
Ccccccccccccccccccccccccccccccccccccccccccccccccc
C-----Reaction Cross Sections lower than 1.d-8 are considered zero.
      eps = 1.D-8
      csinel = CSPrd(2) - 4.*PI*ELCncs
      IF(CSPrd(1).LT.eps)CSPrd(1) = 0.D0
      IF(csinel.LT.eps)csinel = 0.D0
      i = 0
      DO nnuc = 3, NNUcd
        IF(CSPrd(nnuc).LT.eps)CSPrd(nnuc) = 0.D0
        i = i + 1
        csprnt(i) = CSPrd(nnuc)
      ENDDO
Cccccccccccccccccccccccccccccccccccccccccccccccc
      WRITE(41,'(G10.5,1P,(95E12.5))')EINl, TOTcs*TOTred*totcorr, 
     &                                ELAcs*ELAred + 4.*PI*ELCncs, 
     &                                CSFus + (SINl + SINlcc)
     &                                *FCCred + SINlcont, TOTcsfis, 
     &                                CSPrd(1), csinel, 
     &                                (csprnt(nnuc),nnuc = 1,
     &                                MIN(i,NDNuc,84))
 
      IF(TOTcsfis.GT.0.D0.AND.FISshi(nnuc).NE.1.D0)
     &   WRITE(98,'(G10.5,2X,1P,(95E12.5))')EINl, TOTcsfis, 
     &   (CSPfis(nnuc),nnuc = 1,NNUcd)
      CLOSE(80)
      CLOSE(79)
      WRITE(12,*)' '
      WRITE(12,*)' '
      WRITE(12,'('' Tot. fission cross section '',G12.4,'' mb'')')
     &      TOTcsfis
      WRITE(8,*)' '
      WRITE(8,*)' '
      WRITE(8,'(''  Tot. fission cross section '',G12.4,'' mb'')')
     &      TOTcsfis
C----
C---- Initialization of PFNS calculations
C----
C fisspec===========
      IF(FISspe.EQ.1)CALL INPUT_SPEC
C     Assumed that a maximum of 2 different isotopes contribute to fission XS
C     (i.e those coming from fissioning nuclei after primary neutron-proton
C     emission and first CN.
      nfission = 0
      nepfns = 0
      enepfns = 0.D0
      csepfns = 0.D0
      fniueval = 1.D0
      fnubar = 1.D0
 
      IF(FISspe.GT.0)THEN
        WRITE(12,*)' '
        WRITE(8,*)' '
        WRITE(12,*)' ****************************************** '
        WRITE(8,*)' *******************************************'
        WRITE(8,*)' PROMPT FISSION NEUTRON SPECTRA calculations'
        WRITE(12,*)' PROMPT FISSION NEUTRON SPECTRA calculations'
      ENDIF
C fisspec===============
C     For Kalbach parameterization
      DO i = 1, NDAng
        theta = DBLE(i - 1)/DBLE(NDAng - 1)*PI
        XCOs(i) = COS(theta)
      ENDDO
C----
C---- ENDF spectra printout (exclusive representation)
C----
      DO nnuc = 1, NNUcd  ! loop over residues (not decaying nuclei)
        IF(ENDf(nnuc).EQ.1)THEN
          IF(CSPrd(nnuc).GT.0.0D0)THEN
            DO nejc = 0, NDEjc           !loop over ejectiles
              IF(POPcs(nejc,INExc(nnuc)).EQ.0.D0)CYCLE
              IF(A(nnuc).LE.4.AND.Z(nnuc).LE.2.)CYCLE
              IF(nejc.GT.0)THEN
                CALL WHERE(IZA(nnuc) + IZAejc(nejc),nnur,iloc)
              ELSE
                nnur = nnuc
                iloc = 0
              ENDIF
              IF(iloc.NE.0)CYCLE
              IF(nejc.EQ.0)THEN
                cejectile = 'gammas   '
                iizaejc = 0
              ELSEIF(nejc.EQ.1)THEN
                cejectile = 'neutrons '
                iizaejc = IZAejc(nejc)
              ELSEIF(nejc.EQ.2)THEN
                cejectile = 'protons  '
                iizaejc = IZAejc(nejc)
              ELSEIF(nejc.EQ.3)THEN
                cejectile = 'alphas   '
                iizaejc = IZAejc(nejc)
              ELSEIF(nejc.EQ.4)THEN
                cejectile = 'deuterons'
                iizaejc = IZAejc(nejc)
              ELSEIF(nejc.EQ.5)THEN
                cejectile = 'tritons  '
                iizaejc = IZAejc(nejc)
              ELSEIF(nejc.EQ.6)THEN
                cejectile = 'helium-3 '
                iizaejc = IZAejc(nejc)
              ELSEIF(nejc.EQ.NDEjc)THEN
                cejectile = 'lt. ions '
                iizaejc = IZAejc(NDEjc)
              ENDIF
              WRITE(12,*)' '
              WRITE(12,*)' Spectrum of ', cejectile, REAction(nnuc), 
     &                   ' ZAP= ', iizaejc
C
C---------------recorp is a recoil correction factor defined 1+Ap/Ar that
C---------------multiplies cross sections and divides outgoing energies
              recorp = 1.0
              IF(nejc.GT.0)recorp = 1. + EJMass(nejc)/AMAss(nnuc)
 
              nspec = MIN(INT(recorp*EMAx(nnuc)/DE) + 2,NDEcse) - 1
C---------------Exclusive DDX spectra (neutrons & protons)
              IF(nejc.GE.1.AND.nejc.LE.2)THEN
                WRITE(12,'(30X,''A     n     g     l     e     s '')')
                WRITE(12,*)' '
                WRITE(12,'('' Energy   '',8G15.5,/,(10X,8G15.5))')
     &                (ANGles(nang),nang = 1,NDAng)
                IF((nnuc.EQ.MT91.AND.nejc.EQ.1).OR.
     &             (nnuc.EQ.MT649.AND.nejc.EQ.2))THEN
                ! first emission reactions
C-----------------------(discrete levels part)
                  DO il = 1, NLV(nnuc)        !(levels)
                    espec = (EMAx(nnuc) - ELV(il,nnuc))/recorp
                    IF(espec.GE.0)WRITE(12,
     &                              '(F10.5,E14.5,7E15.5,/,(9X,8E15.5))'
     &                              ) - espec, 
     &                               (MAX(CSAlev(nang,il,nejc)*recorp/DE
     &                               ,0.D0),nang = 1,NDAng)
                  ENDDO
                ENDIF
C-----------------------(continuum part - same for all n and p)
                DO ie = 1, nspec + 1    ! clean DDX matrix
                  DO nang = 1, NDAng
                    cseaprnt(ie,nang) = 0.0
                  ENDDO
                ENDDO
                IF(LHMs.EQ.0)THEN
                  iprinted = 0
                  DO ie = 1, nspec    ! reconstruct continuum DDX spectrum
                    piece = CSEmsd(ie,nejc)
                    IF(ie.EQ.NEXr(nejc,1))piece = 0.5*piece
                    ftmp = (POPcse(0,nejc,ie,INExc(nnuc))
     &                     - piece*POPcseaf(0,nejc,ie,INExc(nnuc)))
     &                     /4.0/PI
                    IF(ftmp.LT.0.0D0)THEN
                      ftmp = 0.0D0
                      IF(iprinted.EQ.0)WRITE(8,*)
     &                   'WARNING: Corrective action to avoid', 
     &                   ' negative ddx cross sections taken'
                      iprinted = 1
                      POPcseaf(0,nejc,ie,INExc(nnuc))
     &                  = POPcse(0,nejc,ie,INExc(nnuc))/piece
                    ENDIF
                    DO nang = 1, NDAng
                      cseaprnt(ie,nang) = ftmp + CSEa(ie,nang,nejc,1)
     &                  *POPcseaf(0,nejc,ie,INExc(nnuc))
                    ENDDO
                  ENDDO
                  IF((nnuc.EQ.MT91.AND.nejc.EQ.1).OR.
     &               (nnuc.EQ.MT649.AND.nejc.EQ.2))THEN
                    DO nang = 1, NDAng
                              !double the first bin to preserve integral in EMPEND
                      cseaprnt(1,nang) = cseaprnt(1,nang)*2.0
                    ENDDO
                  ENDIF
                  DO ie = 1, nspec - 1
                                       ! print DDX spectrum
                    WRITE(12,'(F10.5,E14.5,7E15.5,/,(9X,8E15.5))')
     &                    FLOAT(ie - 1)*DE/recorp, 
     &                    (cseaprnt(ie,nang)*recorp,nang = 1,NDAng)
                  ENDDO
                  DO ie = nspec, nspec + 1
                                               ! exact DDX spectrum endpoint
                    WRITE(12,'(F10.5,E14.5,7E15.5,/,(9X,8E15.5))')
     &                    EMAx(nnuc)/recorp, 
     &                    (cseaprnt(ie,nang)*recorp,nang = 1,NDAng)
                  ENDDO
                  WRITE(12,*)' '
                ELSE    ! LHMs = 0
C                     iprinted = 0
C                     spechk(1) = 0.0d0
C                     spechk(2) = 0.0d0
C                     spechk(3) = 0.0d0
                  DO ie = 1, nspec    ! reconstruct continuum DDX spectrum
C                       IF (ie.EQ.NEXr(nejc,1)) piece = 0.5*piece
                    csetmp(ie) = (POPcse(0,nejc,ie,INExc(nnuc)) - xnorm(
     &                           nejc,INExc(nnuc))
     &                           *POPcsed(0,nejc,ie,INExc(nnuc)))/4.0/PI
                    IF(csetmp(ie).LT.0.0D0)csetmp(ie) = 0.0D0
C                       spechk(1) = spechk(1) +
C     &                                  POPcse(0,nejc,ie,INExc(nnuc))
C                       spechk(2) = spechk(2) +
C     &                         xnorm(nejc,INExc(nnuc))
C     &                          * POPcsed(0,nejc,ie,INExc(nnuc))
C                       spechk(3) = spechk(3) + csetmp(ie)*4.0*PI
C                       write(8,'(3i5,3e15.5)') nnuc,nejc,ie,
C     &                  POPcse(0,nejc,ie,INExc(nnuc)),
C     &                  xnorm(nejc,INExc(nnuc))
C     &                                  *POPcsed(0,nejc,ie,INExc(nnuc)),
C     &                   csetmp(ie)
                  ENDDO
C                     write(12,'(a5,i5,3f15.4)')'sig0=',nnuc,
C     &                                              (spechk(i)*DE,i=1,3)
                  nspecrec = MIN(INT(recorp*nspec + 1),NDEcse)
                  DO ie = nspec + 1, nspecrec
                    csetmp(ie) = 0.0D0
                  ENDDO
                  CALL HINTERMAT(0.0D0,DE/recorp,csetmp,NDEcse,0.0D0,DE,
     &                           cseaprnt,NDEcse,1,0.0D0,nspec*DE)
C                     spechk(1) = 0.0d0
C                     spechk(2) = 0.0d0
C                     spechk(3) = 0.0d0
C                     spechk(4) = 0.0d0
                  DO ie = 1, nspec
                    ftmp = recorp*cseaprnt(ie,1)
C                       write(12,'(i5,3e15.5)') ie,csetmp(ie),
C     &                cseaprnt(ie,1),POPcsedlab(0,nejc,ie,INExc(nnuc))
                    DO nang = 1, NDAng
                      cseaprnt(ie,nang) = ftmp + xnorm(nejc,INExc(nnuc))
     &                  *POPcsealab(nang,0,nejc,ie,INExc(nnuc))
C                         IF(nang.GT.1) THEN
C                           spechk(1) = spechk(1) +
C     &                      (cseaprnt(ie,nang)+xcse)*
C     &                           (CAngler(nang)-CANgler(nang-1))
C                           spechk(2) = spechk(2) +
C     &                        xnorm(nejc,INExc(nnuc))*
C     &                      (POPcsealab(nang,0,nejc,ie,INExc(nnuc))+
C     &                       POPcsealab(nang-1,0,nejc,ie,INExc(nnuc)))*
C     &                           (CAngler(nang)-CANgler(nang-1))
C                           spechk(3) = spechk(3) +
C     &                      (ftmp + ftmp)*
C     &                           (CAngler(nang)-CANgler(nang-1))
C                          ENDIF
C                         xcse = cseaprnt(ie,nang)
                    ENDDO
C                     spechk(4) = spechk(4) +
C     &                         xnorm(nejc,INExc(nnuc))
C     &                          * POPcsedlab(0,nejc,ie,INExc(nnuc))
                  ENDDO
C                     write(12,'(a5,i5,4f15.4)')'sig1=',nnuc,
C     &                            (PI*spechk(i)*DE,i=1,3),spechk(4)*DE
 
                  IF((nnuc.EQ.MT91.AND.nejc.EQ.1).OR.
     &               (nnuc.EQ.MT649.AND.nejc.EQ.2))THEN
                    DO nang = 1, NDAng
                     !double the first bin to preserve integral in EMPEND
                      cseaprnt(1,nang) = cseaprnt(1,nang)*2.0
                    ENDDO
                  ENDIF
                  DO ie = 1, nspec - 1
                                       ! print DDX spectrum
                    WRITE(12,'(F10.5,E14.5,7E15.5,/,(9X,8E15.5))')
     &                    FLOAT(ie - 1)*DE/recorp, 
     &                    (cseaprnt(ie,nang),nang = 1,NDAng)
                  ENDDO
                  DO ie = nspec, nspec + 1
                                               ! exact DDX spectrum endpoint
                    WRITE(12,'(F10.5,E14.5,7E15.5,/,(9X,8E15.5))')
     &                    EMAx(nnuc)/recorp, 
     &                    (cseaprnt(ie,nang),nang = 1,NDAng)
                  ENDDO
                  WRITE(12,*)' '
                ENDIF
C
              ELSE   !  then (nejc.GE.1 .AND. nejc.LE.2)
C
C-----------------Exclusive DDX spectra (gammas, alphas, light ions (DE))
C-----------------double the first bin x-sec to preserve integral in EMPEND
                POPcse(0,nejc,1,INExc(nnuc))
     &            = POPcse(0,nejc,1,INExc(nnuc))*2
                WRITE(12,*)' '
                WRITE(12,'('' Energy    mb/MeV'')')
                WRITE(12,*)' '
                IF((nnuc.EQ.MT91.AND.nejc.EQ.1).OR.
     &             (nnuc.EQ.MT649.AND.nejc.EQ.2).OR.
     &             (nnuc.EQ.MT849.AND.nejc.EQ.3))THEN
                                        ! first emission
                  DO il = 1, NLV(nnuc)   ! (levels)
C--------------------------Although DDX spectra are available for emission
C--------------------------they are isotropic and only ang. integrated are
C--------------------------printed (4*Pi*CSAlev(1,il,nejc)
                    espec = (EMAx(nnuc) - ELV(il,nnuc))/recorp
                    IF(espec.GE.0)WRITE(12,'(F10.5,E14.5)') - espec, 
     &                                  MAX(CSAlev(1,il,nejc),0.D0)
     &                                  *4.0*PI*recorp/DE
                  ENDDO
                  DO ie = 1, nspec - 1
                                            ! MT=849 (continuum)
                    WRITE(12,'(F10.5,E14.5)')FLOAT(ie - 1)*DE/recorp, 
     &                    MAX(0.D0,POPcse(0,nejc,ie,INExc(nnuc)))*recorp
                  ENDDO
                                          ! MT=849 exact endpoint
                  WRITE(12,'(F10.5,E14.5)')EMAx(nnuc)/recorp, 
     &                  MAX(0.D0,POPcse(0,nejc,nspec,INExc(nnuc)))
     &                  *recorp
                  WRITE(12,'(F10.5,E14.5)')EMAx(nnuc)/recorp, 0.D0
                ELSE    !all other emissions (continuum and levels together)
                  DO ie = 1, nspec - 1
                    WRITE(12,'(F10.5,E14.5)')FLOAT(ie - 1)*DE/recorp, 
     &                    MAX(0.D0,POPcse(0,nejc,ie,INExc(nnuc)))*recorp
                  ENDDO
                                                 ! exact endpoint
                  WRITE(12,'(F10.5,E14.5)')EMAx(nnuc)/recorp, 
     &                  MAX(0.D0,POPcse(0,nejc,nspec,INExc(nnuc)))
     &                  *recorp
                  WRITE(12,'(F10.5,E14.5)')EMAx(nnuc)/recorp, 0.D0
                ENDIF
              ENDIF  !  (nejc.GE.1 .AND. nejc.LE.2)
            ENDDO     ! over ejectiles
            IF(nnuc.NE.1.AND.RECoil.GT.0)
     &         CALL PRINT_RECOIL(nnuc,REAction(nnuc))
          ENDIF  ! IF (CSPrd(nnuc).GT.0.0D0)
        ENDIF  ! IF (ENDf(nnuc).EQ.1)
 
C     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C        PFNS calculations for a given decaying nucleus A(Nnuc),Z(Nnuc)
C
C           Prompt fission spectra are not calculated if:
C           Partial fission cross section is lower than 1.d-7*TOTcsfis
        IF(FISspe.GT.0.AND.TOTcsfis.GT.0.D0.AND.CSPfis(nnuc)
     &     .GT.0.0D0.AND.(Z(0).EQ.Z(nnuc).OR.Z(0) - 1.EQ.Z(nnuc)).AND.
     &     CSPfis(nnuc).GT.1.D-7*TOTcsfis)THEN
C
C           Only PFNS of the neutron chain are considered
C           Checked if Z of the fissioning nucleus
C             is equal to the target, if not PFNS skipped
          IF(NINT(Z(0)).NE.NINT(Z(nnuc)))CYCLE
 
          iaf = A(nnuc)
          izf = Z(nnuc)
C
          nejc = 1   ! neutron emission only
          post_fisn = 0.D0
          ratio2maxw = 0.D0
          cejectile = 'neutrons '
 
          IF(nfission.EQ.0)THEN
C
C           First fissioning nucleus in the isotope chain
C
C             Calculating unique energy grid
C
C             Assumed maximum energy of neutrons from fragments will be 25 MeV
 
            deltae_pfns = 0.1D0
            nepfns = MIN(NINT(25.D0/deltae_pfns) + 1,NDEpfn)
 
            enepfns(1) = 0.00075D0
 
            DO ie = 1, nepfns
              enepfns(ie) = FLOAT(ie - 1)*deltae_pfns
            ENDDO
            enepfns(1) = 0.00075D0   ! Setting the lowest limit to 0.75 keV 0
 
C             Initializing the pseudo incident energy
            eincid = EXCn - Q(1,1)    ! emitting from CN, nnuc = 1
 
C
C             fniu_nubar_eval(eincid) is defined differently in
C             the source file read_nubar_windows.f (only used in Windows)
C
C             Th-232 nubar as a function of E is used, i.e.
C             fniu_nubar_eval(eincid) = fniuTH232(eincid)
C
C             This allows for Windows use and testing avoiding
C             compilation errors of IO package in Windows
C
            IF(NUBarread)fniueval = FNIU_NUBAR_EVAL(eincid)
C             The total nubar is calculated for the first incident (real)
C             energy and used for the normalization of the total PFNS
            IF(fniueval.LE.0.D0)THEN
              fniueval = 1.D0
              WRITE(12,'('' WARNING: Nubar taken as 1'')')
              WRITE(8,'('' WARNING: Nubar taken as 1'')')
              WRITE(8,'('' WARNING: MF=1,MT=456 should be copied'')')
              WRITE(8,'('' WARNING:  from desired evaluation to '')')
              WRITE(8,'('' WARNING:  to file -nubar.endf        '')')
            ENDIF
 
C             Models could be used for NUBAR calculations,
C             however, we prefer to normalize our PFNS to the
C             evaluated NUBAR
C
C             if(fniuEVAL.eq.1) then
C               iafiss = A(1)
C               IF(FISspe.eq.1)
C    &            fnubar = fniuLANL(eincid,NINT(A(1)),NINT(Z(1)))
C               IF(FISspe.eq.2)
C    &            fnubar = fniu    (eincid,NINT(A(1)),NINT(Z(1)))
C             endif
 
            fnubar = fniueval*PFNniu     ! scaling of NUBAR
 
          ELSE
 
C             For higher emission chances, the corresponding
C             neutron binding energy is substracted iteratively
C
            eincid = eincid - Q(1,nnuc)
 
          ENDIF
C
C           If no more excitation energy available, then PFN emission stopped
          IF(eincid.LT.0.D0)CYCLE
 
          WRITE(12,*)
          WRITE(8,*)
 
          WRITE(8,'(1x,a11,i2,a7,f8.3,A18,1x,i3,A1,A2,A1,i3)')
     &          ' Fiss. Nucl ', nfission + 1, ', Uexc=', 
     &          eincid + Q(1,nnuc), ' MeV, for nucleus:', izf, '-', 
     &          SYMb(nnuc), '-', iaf
 
          WRITE(12,*)' '
          WRITE(12,'(''  Fiss. nucleus '',I3,''-'',A2,''-'',I3)')
     &          INT(Z(nnuc)), SYMb(nnuc), INT(A(nnuc))
          WRITE(12,
     &          '(''  Partial fission cross section '',G12.5,'' mb'')')
     &          CSPfis(nnuc)
          WRITE(12,'(''  Ratio of partial to total fission '',G12.5)')
     &          CSPfis(nnuc)/TOTcsfis
          WRITE(12,
     &'(''  Binding energy of fissioning nucleus '',            G12.5,''
     & MeV'')')Q(1,nnuc)
 
          WRITE(8,*)' '
          WRITE(8,'(''  Fiss. nucleus '',I3,''-'',A2,''-'',I3)')
     &          INT(Z(nnuc)), SYMb(nnuc), INT(A(nnuc))
          WRITE(8,'(''  Partial fission cross section '',G12.5,'' mb'')'
     &          )CSPfis(nnuc)
          WRITE(8,'(''  Ratio of partial to total fission '',G12.5)')
     &          CSPfis(nnuc)/TOTcsfis
          WRITE(8,
     &'(''  Binding energy of fissioning nucleus '',            G12.5,''
     & MeV'')')Q(1,nnuc)
 
C-----------Calculating post-fission neutrons in the first chance
          IF(FISspe.EQ.1)CALL GET_FRAGMPFNS_LANL(post_fisn,enepfns,
     &       nepfns,eincid,A(nnuc),Z(nnuc),eneutr,tequiv,Q(1,nnuc),
     &       deltae_pfns,PFNtke,PFNrat,PFNalp)
          IF(FISspe.EQ.2)CALL GET_FRAGMPFNS(post_fisn,enepfns,nepfns,
     &       eincid,A(nnuc),Z(nnuc),eneutr,tequiv,Q(1,nnuc),deltae_pfns,
     &       PFNtke,PFNrat,PFNalp)
 
          fnorm = CSPfis(nnuc)/TOTcsfis
          IF(eneutr.GT.0)THEN
            DO ie = 1, nepfns
              ftmp = FMAXW(enepfns(ie),tequiv)
              IF(ftmp.GT.0)ratio2maxw(ie) = post_fisn(ie)/ftmp
C------------------Accumulating total spectrum for neutrons
              csepfns(ie) = csepfns(ie) + post_fisn(ie)*fnorm
C
C                  CSEfis contains the n,xnf spectra, not being used for now
C                  CSEfis is nitialized inside HF-comp.f (EXCLUSIVE..)
C                  csepfns(ie) = csepfns(ie) + CSEfis(ie,nejc,nnuc)
C
            ENDDO
 
            WRITE(8,'(''  Postfission <En> '', G12.5,'' MeV'')')eneutr
            WRITE(8,'(''  Equivalent Tmaxwell '',G12.5,'' MeV'')')tequiv
            WRITE(8,*)' '
 
            WRITE(12,'(''  Postfission <En> '', G12.5,'' MeV'')')eneutr
            WRITE(12,'(''  Equivalent Tmaxwell '',G12.5,'' MeV'')')
     &            tequiv
            WRITE(12,*)' '
 
            WRITE(12,*)' Spectrum of ', cejectile, 
     &                 '(z,partfis) from CN ', ' ZAP= ', IZA(nnuc)
            WRITE(12,*)' '
            WRITE(12,'(''    Energy    mb/MeV       Ratio to Maxw'')')
            WRITE(12,*)' '
            DO ie = 1, nepfns
              WRITE(12,'(F10.5,E14.5,2x,E14.5)')enepfns(ie), 
     &              post_fisn(ie), ratio2maxw(ie)
C               WRITE (73,'(F10.5,E14.5,4(2x,E14.5))')
C    &               enepfns(ie), post_fisn(ie), ratio2maxw(ie)
            ENDDO
            WRITE(12,'(F10.5,E14.5,2x,E14.5)')enepfns(nepfns), 0.D0, 
     &            0.D0
          ELSE
 
            WRITE(12,'(''  No fission neutrons emitted'')')
            WRITE(8,'(''  No fission neutrons emitted'')')
 
          ENDIF
 
          WRITE(12,*)' '
 
          nfission = nfission + 1
 
        ENDIF   !  IF ( TOTcsfis > 0 & CSPfis(nnuc)> 0 & Z(0)==Z(nnuc) )
 
      ENDDO  ! over decaying nuclei
 
C-----
C-----PRINTING TOTAL PFNS and PFNM quantities
C-----
      IF(FISspe.GT.0.AND.TOTcsfis.GT.0.D0)THEN
 
 
        fnorm = 0.D0
        DO ie = 1, nepfns
          fnorm = fnorm + csepfns(ie)
        ENDDO
 
        IF(fnorm.GT.1.D-7)THEN        ! No PFNS calculated
 
C       Correcting normalization
          ftmp = 0.D0
          DO ie = 1, nepfns
            csepfns(ie) = csepfns(ie)/fnorm
            ftmp = ftmp + csepfns(ie)
          ENDDO
 
          ftmp = 0.D0
          eneutr = 0.D0
          DO ie = 2, nepfns
            fmed = (csepfns(ie) + csepfns(ie - 1))*0.5
            eneutr = eneutr + fmed*(enepfns(ie) + enepfns(ie - 1))*0.5D0
            ftmp = ftmp + fmed
          ENDDO
          IF(ftmp.GT.0)eneutr = eneutr/ftmp
          tequiv = 2.D0/3.D0*eneutr
 
          WRITE(74,'(1X,f8.5,1x,f8.3,2(4x,f7.3))')EINl, eneutr, fnubar, 
     &          tequiv
 
          WRITE(73,
     &'(''  Total PFNS from '',I3,''-'',A2,''-'',I3,'': Elab='',  F8.4,'
     &'MeV, <Epfns>='',f8.4,'' MeV, Tmaxw='',f8.4, '' MeV, Norm='',F10.8
     &)')INT(Z(1)), SYMb(1), INT(A(1)), EINl, eneutr, tequiv, ftmp
 
          WRITE(8,*)
          WRITE(8,*)' ***'
          WRITE(8,*)
          WRITE(8,
     &'(''  Total PFNS from CN '',I3,''-'',A2,''-'',I3,''  Elab='',  F8.
     &4,'' MeV,  Norm='',F10.8)')INT(Z(1)), SYMb(1), INT(A(1)), EINl, 
     &                           ftmp
          WRITE(8,*)
 
          WRITE(8,'(''  Number of fissioning nuclei '',I3)')nfission
          WRITE(8,'(''  Total PFNS average energy  '',G12.5,A5)')eneutr, 
     &          '  MeV'
          WRITE(8,'(''  Equivalent Tmaxwell '',G12.5,A5)')tequiv, 
     &          '  MeV'
          WRITE(8,'(''  Multiplicity (nue) '',F6.3)')fnubar
          IF(fnubar.NE.1)WRITE(8,'(''  Nubar from evaluated library '')'
     &                         )
          IF(fnubar.NE.1.AND.PFNniu.NE.1)
     &       WRITE(8,'(''  Nubar scaled by '',f6.4)')PFNniu
          WRITE(8,*)
 
          WRITE(12,*)
          WRITE(12,*)' ***'
          WRITE(12,*)
          WRITE(12,
     &'(''  Total PFNS from CN '',I3,''-'',A2,''-'',I3,''  Elab='',  F8.
     &4,'' MeV   Norm='',F10.8)')INT(Z(1)), SYMb(1), INT(A(1)), EINl, 
     &                           ftmp
 
          WRITE(12,'(''  Number of fissioning nuclei '',I3)')nfission
          WRITE(12,'(''  Total PFNS average energy  '',G12.5,A5)')
     &          eneutr, '  MeV'
          WRITE(12,'(''  Equivalent Tmaxwell '',G12.5,A5)')tequiv, 
     &          '  MeV'
          WRITE(12,'(''  Normalization '',F12.9)')ftmp
          WRITE(12,'(''  Multiplicity (nue) '',F6.3)')fnubar
          IF(fnubar.NE.1)WRITE(12,
     &                         '(''  Nubar from evaluated library '')')
          IF(fnubar.NE.1.AND.PFNniu.NE.1)
     &       WRITE(12,'(''  Nubar scaled by '',f6.4)')PFNniu
          WRITE(12,*)
 
          WRITE(8,*)' Spectrum of ', cejectile, '(z,fission) from CN ', 
     &              ' ZAP= ', IZA(1)
          WRITE(8,*)' '
          WRITE(8,'(''    Energy    mb/MeV       Ratio to Maxw'')')
          WRITE(8,*)' '
 
          WRITE(12,*)' Spectrum of ', cejectile, '(z,fission) from CN ', 
     &               ' ZAP= ', IZA(1)
          WRITE(12,*)' '
          WRITE(12,'(''    Energy    mb/MeV       Ratio to Maxw'')')
          WRITE(12,*)' '
 
          WRITE(73,'(/,''    Energy    mb/MeV       Ratio to Maxw'')')
          DO ie = 1, nepfns
            ftmp = FMAXW(enepfns(ie),tequiv)
            ftmp1 = 1.D0
            IF(ftmp.GT.0)ftmp1 = csepfns(ie)/ftmp
 
            WRITE(12,'(F10.5,E14.5,2x,E14.5)')enepfns(ie), csepfns(ie)
            WRITE(73,'(F10.5,E14.5,4(2x,E14.5))')enepfns(ie), 
     &            csepfns(ie), ftmp1
            IF(enepfns(ie).GT.7.D0)CYCLE
            WRITE(8,'(F10.5,E14.5,2x,E14.5)')enepfns(ie), csepfns(ie), 
     &            ftmp1
          ENDDO
          WRITE(12,'(F10.5,E14.5,2x,E14.5)')enepfns(nepfns), 0.D0, 0.D0
          WRITE(8,*)'...   PFNS Output supressed for Epfns > 7 MeV '
        ENDIF
      ENDIF
 
      WRITE(12,*)' '
      WRITE(8,*)' '
C
C     end of PFNS calculations
C     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 
C-----
C-----ENDF spectra inclusive representation
C-----
C-----NOTE: transformation into CM
C-----(reduce channel energy to account for recoils)
C-----is not yet done! Should be performed in ACCUM and EXCLUSIVEC/L
C-----NOTE: HMS cumulative spectra (if calculated) are already
C-----stored in CSE(.,x,0) array
C-----
C-----Calculate double-differential spectra
C     DO nejc = 0, NDEJC
C          DO iesp = 1, NDECSE
C            DO nang = 1, NDANG
C               piece = CSEmsd(iesp,nejc)
C               IF (iesp.EQ.NEXr(nejc,1)) piece = 0.5*piece
C               CSEa(iesp,nang,nejc,0)
C    &             = ((CSE(iesp,nejc,0)
C    &             - piece*POPcseaf(0,nejc,iesp,0))
C    &             /4.0/PI      + CSEa(iesp,nang,nejc,1)
C    &             *POPcseaf(0,nejc,iesp,0))
C            ENDDO
C         ENDDO
C     ENDDO
      WRITE(8,*)' '
      WRITE(8,*)
 
      IF(IOUt.GT.5.AND..NOT.EXClusiv)THEN
 
        DO nejc = 0, NEJcm
          csemax = 0.D0
          DO i = 1, NDEx
            csemax = DMAX1(CSE(i,nejc,0),csemax)
          ENDDO
          IF(csemax.LE.0.01D0)CYCLE
 
          CALL AUERST(0,nejc,1)
        ENDDO
 
        IF(FIRst_ein)THEN
 
          WRITE(8,'(11X,''**********************'')')
          WRITE(8,'(11x,'' Total spectra (C.M.)'')')
          WRITE(8,'(11x,''**********************'')')
          DO nejc = 0, NEJcm
            csemax = 0.D0
            DO i = 1, NDEx
              csemax = DMAX1(CSEt(i,nejc),csemax)
            ENDDO
            IF(csemax.LE.0.01D0)CYCLE
 
            CALL PRINT_TOTAL(nejc)
            CALL PLOT_TOTAL_EMIS_SPECTRA(nejc)
          ENDDO
 
        ENDIF
      ENDIF
      WRITE(8,*)
      WRITE(8,*)
      checkxs = checkxs + TOTcsfis
      IF(ABScs.GT.0.)THEN
        WRITE(8,
     &'('' ********************************************'',           23(
     &1H*))')
        WRITE(8,
     & '('' * Incident energy (LAB): '',G12.5,              '' MeV  '')'
     & )EINl
        IF(INT(ZEJc(0)).EQ.0)THEN
          IF(TOTred.NE.1)WRITE(8,
     &                   '('' * Total cross section scaled by '',G13.6)'
     &                   )TOTred
          IF(ELAred.NE.1)WRITE(8,
     &           '('' * Shape Elastic cross section scaled by '',G13.6)'
     &           )ELAred
          WRITE(8,
     &'('' * Calculated total cross section                 '',G13.6,   
     &           '' mb  '')')CSFus + (SINl + SINlcc)*FCCred + SINlcont + 
     &                       ELAred*ELAcs
C         WRITE (8,
C    &  '('' * Scaled total cross section:TOTcs*TOTred*TOTcorr'',G13.6,
C    &              '' mb  '')') TOTcs*TOTred*TOTcorr
C         WRITE (8,
C    &  '('' * OM FUSred*ABScs + ELAred*ELAcs                 '',
C    &         G13.6,'' mb  '')') ELAred*ELAcs + ABScs*FUSred
          WRITE(8,
     &'('' * Calculated shape elastic cross section (ELAcs) '',         
     &G13.6,'' mb  '')')ELAred*ELAcs
        ENDIF
        WRITE(8,
     &'('' * Optical model nonelastic cross section (ABScs) '',G13.6,   
     &           '' mb  '')')(ABScs - (SINl + SINlcc) - SINlcont)
     &                       *FUSred + (SINl + SINlcc)*FCCred + SINlcont
        WRITE(8,
     &'('' * Calculated nonelastic cross section            '',G13.6,   
     &           '' mb  '')')CSFus + (SINl + SINlcc)*FCCred + SINlcont
        IF(FISsil(1))WRITE(8,
     &'('' * Calculated fission cross section               '',G13.6,   
     &           '' mb  '')')TOTcsfis
        WRITE(8,
     &'('' * Production cross section (incl.fission)        '',         
     &  G13.6,'' mb'')')checkxs
        WRITE(8,'('' * Difference: '', F7.2, '' mb ('',F6.2,'') %'')')
     &        CSFus + (SINl + SINlcc)*FCCred + SINlcont - checkxs, 
     &        100.D0*ABS
     &        ((CSFus + (SINl+SINlcc)*FCCred + SINlcont - checkxs))
     &        /(CSFus + (SINl + SINlcc)*FCCred + SINlcont)
        IF(INT(ZEJc(0)).EQ.0.AND.EIN.LE.10.D0)WRITE(8,
     &'('' * Compound elastic cross section (CE) '',G13.6,              
     &'' mb  '')')4.*PI*ELCncs
        IF(FUSred.NE.1)WRITE(8,
     &          '('' * CN formation cross section scaled by '',G13.6  )'
     &          )FUSred
        IF(FCCred.NE.1)WRITE(8,
     &     '('' * Direct collective cross section scaled by '',  G12.5)'
     &     )FCCred
        WRITE(8,
     &'('' ********************************************'',           23(
     &1H*))')
 
        IF(INT(ZEJc(0)).EQ.0)THEN
          WRITE(*,*)
          WRITE(*,
     &'(''   Calculated total cross section                 '',        G
     &13.6, '' mb  '')')CSFus + (SINl + SINlcc)*FCCred + SINlcont + 
     &                  ELAred*ELAcs
          WRITE(*,
     &'(''   Calculated shape elastic cross section         '',         
     &G13.6,'' mb  '')')ELAred*ELAcs
        ENDIF
        WRITE(*,
     &'(''   Calculated nonelastic cross section            '',        G
     &13.6, '' mb  '')')CSFus + (SINl + SINlcc)*FCCred + SINlcont
        IF(FISsil(1))WRITE(*,
     &'(''   Calculated fission cross section               '',        G
     &13.6, '' mb '')')TOTcsfis
        WRITE(*,
     &'(''   Production cross section (incl.fission)        '',         
     &  G13.6,'' mb'')')checkxs
        WRITE(*,'(''   Difference: '', F7.2, '' mb ('',F6.2,'') %'')')
     &        CSFus + (SINl + SINlcc)*FCCred + SINlcont - checkxs, 
     &        100.D0*ABS
     &        ((CSFus + (SINl+SINlcc)*FCCred + SINlcont - checkxs))
     &        /(CSFus + (SINl + SINlcc)*FCCred + SINlcont)
        IF(INT(ZEJc(0)).EQ.0.AND.EIN.LE.10.D0)THEN
          WRITE(*,
     &'(''   Compound elastic cross section (CE) '',     G13.6,'' mb  ''
     &/)')4.*PI*ELCncs
        ELSE
          WRITE(*,*)
        ENDIF
 
      ENDIF
 
      IF(ABS(CSFus + (SINl+SINlcc)*FCCred + SINlcont - checkxs)
     &   .GT.0.01*(CSFus + (SINl+SINlcc)*FCCred + SINlcont))THEN
        WRITE(8,*)
        WRITE(8,'('' WARNING: Sum of production XS(incl.fission)'')')
        WRITE(8,'('' WARNING: is not equal reaction cross section'')')
        IF((CSFus + (SINl+SINlcc)*FCCred + SINlcont).NE.0.D0)
     &     WRITE(8,'('' WARNING:     difference: '', F6.2,'' %'')')
     &     100.D0*ABS(CSFus + (SINl + SINlcc)*FCCred + SINlcont - 
     &     checkxs)/(CSFus + (SINl + SINlcc)*FCCred + SINlcont)
      ENDIF
      IF(TOTred*TOTcs*totcorr.GT.0.D0.AND.
     &   ABS(CSFus + (SINl+SINlcc)*FCCred + SINlcont + ELAred*ELAcs - 
     &   TOTred*TOTcs*totcorr).GT.0.01*TOTred*TOTcs*totcorr)THEN
        WRITE(8,*)
        WRITE(8,'('' WARNING: Total XS is not equal'')')
        WRITE(8,'('' WARNING: Elastic + Absorption cross section'')')
        WRITE(8,'('' WARNING:     difference: '', F6.2,'' %'')')
     &        100.D0*ABS(ABScs + ELAred*ELAcs - TOTred*TOTcs*totcorr)
     &        /(TOTred*TOTcs*totcorr)
      ENDIF
 
      WRITE(8,*)
      WRITE(8,*)'+++++'
      WRITE(8,*)
C-----
C-----ENDF spectra printout (inclusive representation)
C-----
      IF(.NOT.EXClusiv)THEN
C--------Print spectra of residues
        reactionx = '(z,x)  '
        DO nnuc = 1, NNUcd     !loop over decaying nuclei
          IF(ENDf(nnuc).EQ.2.AND.RECoil.GT.0)
     &       CALL PRINT_RECOIL(nnuc,reactionx)
        ENDDO  !over decaying nuclei in ENDF spectra printout
C--------Print inclusive gamma spectrum
        nspec = INT(EMAx(1)/DE) + 2
        IF(nspec.GT.NDEcse - 1)nspec = NDEcse - 1
        WRITE(12,*)' '
        WRITE(12,*)' Spectrum of gammas   (z,x)  ZAP=     0'
        WRITE(12,*)' '
        WRITE(12,'(''    Energy    mb/MeV'')')
        WRITE(12,*)' '
        DO ie = 1, nspec - 1
          WRITE(12,'(F9.4,E15.5)')FLOAT(ie - 1)*DE, 
     &                            MAX(0.D0,CSE(ie,0,0))
        ENDDO
C--------Exact endpoint
        WRITE(12,'(F9.4,E15.5)')EMAx(1), MAX(0.D0,CSE(nspec,0,0))
        WRITE(12,'(F9.4,E15.5)')EMAx(1), 0.D0
 
        totspec = 0.D0
        DO ie = 1, nspec - 1
          totspec = totspec + CSE(ie,0,0)
        ENDDO
        totspec = totspec - 0.5D0*(CSE(1,0,0) + CSE(nspec - 1,0,0))
        totspec = totspec*DE
 
        WRITE(12,*)' '
        WRITE(12,'(1x,'' Integrated spectrum   '',G12.5,'' mb'')')
     &        totspec
        WRITE(12,*)' '
 
C--------Print inclusive spectra of ejectiles
C--------neutrons
        recorp = (1. + EJMass(1)/AMAss(1))
        nspec = MIN0(NDEcse - 1,INT(recorp*(EMAx(1)-Q(1,1))/DE) + 2)
 
        IF(LHMs.GT.0)THEN
          DO ie = 1, nspec + 1
C             Subtract HMS contribution to CM emission spectrum
            CSE(ie,1,0) = CSE(ie,1,0) - CSEhms(ie,1,0)
                    ! clean DDX matrix
            csetmp(ie) = 0.0
            DO nang = 1, NDAng
              cseaprnt(ie,nang) = 0.0
            ENDDO
          ENDDO
          CALL HINTERMAT(0.0D0,DE/recorp,CSE(1,1,0),NDEcse,0.0D0,DE,
     &                   csetmp,NDEcse,1,0.0D0,(nspec + 1)*DE)
          DO ie = 1, nspec  ! reconstruct continuum DDX spectrum
            ftmp = recorp*csetmp(ie)/4.0/PI
            DO nang = 1, NDAng
              cseaprnt(ie,nang) = ftmp + CSEahmslab(ie,nang,1)
              IF(cseaprnt(ie,nang).LT.1.0E-7)cseaprnt(ie,nang) = 0.0D0
            ENDDO
            CSE(ie,1,0) = recorp*csetmp(ie) + CSEhmslab(ie,1,0)
          ENDDO
        ENDIF
 
        IF(nspec.GT.0)THEN
          WRITE(12,*)' '
          WRITE(12,*)' Spectrum of neutrons (z,x)  ZAP=     1'
          WRITE(12,*)' '
          WRITE(12,'(''    Energy    mb/MeV'')')
          WRITE(12,*)' '
          DO ie = 1, nspec - 1
            WRITE(12,'(F9.4,E15.5)')FLOAT(ie - 1)*DE, 
     &                              MAX(0.D0,CSE(ie,1,0))
          ENDDO
C---------Exact endpoint
          WRITE(12,'(F9.4,E15.5)')recorp*(EMAx(1) - Q(1,1)), 
     &                            MAX(0.D0,CSE(nspec,1,0))
          WRITE(12,'(F9.4,E15.5)')recorp*(EMAx(1) - Q(1,1)), 0.D0
 
          totspec = 0.D0
          DO ie = 1, nspec - 1
            totspec = totspec + CSE(ie,1,0)
          ENDDO
          totspec = totspec - 0.5D0*(CSE(1,4,0) + CSE(nspec - 1,1,0))
          totspec = totspec*DE
 
          WRITE(12,*)' '
          WRITE(12,'(1x,'' Integrated spectrum   '',G12.5,'' mb'')')
     &          totspec
          WRITE(12,*)' '
 
C---------------Inclusive DDX spectrum (neutrons)
          IF(LHMs.NE.0)THEN
            WRITE(12,*)' '
            WRITE(12,*)' DDXS of neutrons (z,x)  ZAP=     1'
            WRITE(12,'(30X,''A     n     g     l     e     s '')')
            WRITE(12,*)' '
            WRITE(12,'('' Energy   '',8G15.5,/,(10X,8G15.5))')
     &            (ANGles(nang),nang = 1,NDAng)
            DO ie = 1, nspec - 1
              WRITE(12,'(F10.5,E14.5,7E15.5,/,(9X,8E15.5))')
     &              FLOAT(ie - 1)*DE, (cseaprnt(ie,nang),nang = 1,NDAng)
            ENDDO
            DO ie = nspec, nspec + 1
                                               ! exact DDX spectrum endpoint
              WRITE(12,'(F10.5,E14.5,7E15.5,/,(9X,8E15.5))')
     &              recorp*(EMAx(1) - Q(1,1)), 
     &              (MAX(cseaprnt(ie,nang),0.D0),nang = 1,NDAng)
            ENDDO
          ENDIF
        ENDIF
 
C--------protons
        recorp = (1. + EJMass(2)/AMAss(1))
        nspec = MIN0(NDEcse - 1,INT(recorp*(EMAx(1)-Q(2,1))/DE) + 2)
        IF(LHMs.GT.0)THEN
          DO ie = 1, nspec + 1
C             Subtract HMS contribution to CM emission spectrum
            CSE(ie,2,0) = CSE(ie,2,0) - CSEhms(ie,2,0)
                    ! clean DDX matrix
            csetmp(ie) = 0.0
            DO nang = 1, NDAng
              cseaprnt(ie,nang) = 0.0
            ENDDO
          ENDDO
          CALL HINTERMAT(0.0D0,DE/recorp,CSE(1,2,0),NDEcse,0.0D0,DE,
     &                   csetmp,NDEcse,1,0.0D0,(nspec + 1)*DE)
          DO ie = 1, nspec  ! reconstruct continuum DDX spectrum
            ftmp = recorp*csetmp(ie)/4.0/PI
            DO nang = 1, NDAng
              cseaprnt(ie,nang) = ftmp + CSEahmslab(ie,nang,2)
              IF(cseaprnt(ie,nang).LT.1.0E-7)cseaprnt(ie,nang) = 0.0D0
            ENDDO
            CSE(ie,2,0) = recorp*csetmp(ie) + CSEhmslab(ie,2,0)
          ENDDO
        ENDIF
 
        IF(nspec.GT.0)THEN
          WRITE(12,*)' '
          WRITE(12,*)' Spectrum of protons  (z,x)  ZAP=  1001'
          WRITE(12,*)' '
          WRITE(12,'(''    Energy    mb/MeV'')')
          WRITE(12,*)' '
          DO ie = 1, nspec - 1
            WRITE(12,'(F9.4,E15.5)')FLOAT(ie - 1)*DE, 
     &                              MAX(0.D0,CSE(ie,2,0))
          ENDDO
C---------Exact endpoint
          IF(LHMs.EQ.0)THEN
            WRITE(12,'(F9.4,E15.5)')EMAx(1) - Q(2,1), 
     &                              MAX(0.D0,CSE(nspec,2,0))
            WRITE(12,'(F9.4,E15.5)')EMAx(1) - Q(2,1), 0.D0
          ELSE
            WRITE(12,'(F9.4,E15.5)')recorp*(EMAx(1) - Q(2,1)), 
     &                              MAX(0.D0,CSE(nspec,2,0))
            WRITE(12,'(F9.4,E15.5)')recorp*(EMAx(1) - Q(2,1)), 0.D0
          ENDIF
 
          totspec = 0.D0
          DO ie = 1, nspec - 1
            totspec = totspec + CSE(ie,2,0)
          ENDDO
          totspec = totspec - 0.5D0*(CSE(1,2,0) + CSE(nspec - 1,2,0))
          totspec = totspec*DE
 
          WRITE(12,*)' '
          WRITE(12,'(1x,'' Integrated spectrum   '',G12.5,'' mb'')')
     &          totspec
          WRITE(12,*)' '
 
C---------------Inclusive DDX spectrum (protons)
          IF(LHMs.NE.0)THEN
            WRITE(12,*)' '
            WRITE(12,*)' DDXS of protons (z,x)  ZAP=  1001'
            WRITE(12,'(30X,''A     n     g     l     e     s '')')
            WRITE(12,*)' '
            WRITE(12,'('' Energy   '',8G15.5,/,(10X,8G15.5))')
     &            (ANGles(nang),nang = 1,NDAng)
            DO ie = 1, nspec - 1
              WRITE(12,'(F10.5,E14.5,7E15.5,/,(9X,8E15.5))')
     &              FLOAT(ie - 1)*DE, (cseaprnt(ie,nang),nang = 1,NDAng)
            ENDDO
            DO ie = nspec, nspec + 1
                                               ! exact DDX spectrum endpoint
              WRITE(12,'(F10.5,E14.5,7E15.5,/,(9X,8E15.5))')
     &              recorp*(EMAx(1) - Q(2,1)), 
     &              (MAX(cseaprnt(ie,nang),0.D0),nang = 1,NDAng)
            ENDDO
          ENDIF
        ENDIF
C--------alphas
        recorp = 1. + EJMass(3)/AMAss(1)
        nspec = INT((EMAx(1) - Q(3,1))/DE) + 2
        IF(nspec.GT.0)THEN
          IF(nspec.GT.NDEcse - 1)nspec = NDEcse - 1
          WRITE(12,*)' '
          WRITE(12,*)' Spectrum of alphas   (z,x)  ZAP=  2004'
          WRITE(12,*)' '
          WRITE(12,'(''    Energy    mb/MeV'')')
          WRITE(12,*)' '
 
          DO ie = 1, nspec - 1
            WRITE(12,'(F9.4,E15.5)')FLOAT(ie - 1)*DE, 
     &                              MAX(0.D0,CSE(ie,3,0))
          ENDDO
C---------Exact endpoint
          WRITE(12,'(F9.4,E15.5)')EMAx(1) - Q(3,1), 
     &                            MAX(0.D0,CSE(nspec,3,0))
          WRITE(12,'(F9.4,E15.5)')EMAx(1) - Q(3,1), 0.D0
 
          totspec = 0.D0
          DO ie = 1, nspec - 1
            totspec = totspec + CSE(ie,3,0)
          ENDDO
          totspec = totspec - 0.5D0*(CSE(1,3,0) + CSE(nspec - 1,3,0))
          totspec = totspec*DE
 
          WRITE(12,*)' '
          WRITE(12,'(1x,'' Integrated spectrum   '',G12.5,'' mb'')')
     &          totspec
          WRITE(12,*)' '
 
        ENDIF
 
C--------deuterons
        recorp = (1. + EJMass(4)/AMAss(1))
        nspec = INT((EMAx(1) - Q(4,1))/DE) + 2
        IF(nspec.GT.0)THEN
          IF(nspec.GT.NDEcse - 1)nspec = NDEcse - 1
          WRITE(12,*)' '
          WRITE(12,*)' Spectrum of deuterons(z,x)  ZAP=  1002'
          WRITE(12,*)' '
          WRITE(12,'(''    Energy    mb/MeV'')')
          WRITE(12,*)' '
          DO ie = 1, nspec - 1
            WRITE(12,'(F9.4,E15.5)')FLOAT(ie - 1)*DE, 
     &                              MAX(0.D0,CSE(ie,4,0))
          ENDDO
C---------Exact endpoint
          WRITE(12,'(F9.4,E15.5)')EMAx(1) - Q(4,1), 
     &                            MAX(0.D0,CSE(nspec,4,0))
          WRITE(12,'(F9.4,E15.5)')EMAx(1) - Q(4,1), 0.D0
 
          totspec = 0.D0
          DO ie = 1, nspec - 1
            totspec = totspec + CSE(ie,4,0)
          ENDDO
          totspec = totspec - 0.5D0*(CSE(1,4,0) + CSE(nspec - 1,4,0))
          totspec = totspec*DE
 
          WRITE(12,*)' '
          WRITE(12,'(1x,'' Integrated spectrum   '',G12.5,'' mb'')')
     &          totspec
          WRITE(12,*)' '
 
        ENDIF
 
C--------tritons
        recorp = (1. + EJMass(5)/AMAss(1))
        nspec = INT((EMAx(1) - Q(5,1))/DE) + 2
        IF(nspec.GT.0)THEN
          IF(nspec.GT.NDEcse - 1)nspec = NDEcse - 1
          WRITE(12,*)' '
          WRITE(12,*)' Spectrum of tritons  (z,x)  ZAP=  1003'
          WRITE(12,*)' '
          WRITE(12,'(''    Energy    mb/MeV'')')
          WRITE(12,*)' '
          DO ie = 1, nspec - 1
            WRITE(12,'(F9.4,E15.5)')FLOAT(ie - 1)*DE, 
     &                              MAX(0.D0,CSE(ie,5,0))
          ENDDO
C---------Exact endpoint
          WRITE(12,'(F9.4,E15.5)')EMAx(1) - Q(5,1), 
     &                            MAX(0.D0,CSE(nspec,5,0))
          WRITE(12,'(F9.4,E15.5)')EMAx(1) - Q(5,1), 0.D0
 
          totspec = 0.D0
          DO ie = 1, nspec - 1
            totspec = totspec + CSE(ie,5,0)
          ENDDO
          totspec = totspec - 0.5D0*(CSE(1,5,0) + CSE(nspec - 1,5,0))
          totspec = totspec*DE
 
          WRITE(12,*)' '
          WRITE(12,'(1x,'' Integrated spectrum   '',G12.5,'' mb'')')
     &          totspec
          WRITE(12,*)' '
 
        ENDIF
 
C--------helium-3
        recorp = (1. + EJMass(6)/AMAss(1))
        nspec = INT((EMAx(1) - Q(6,1))/DE) + 2
        IF(nspec.GT.0)THEN
          IF(nspec.GT.NDEcse - 1)nspec = NDEcse - 1
          WRITE(12,*)' '
          WRITE(12,*)' Spectrum of helium-3 (z,x)  ZAP=  2003'
          WRITE(12,*)' '
          WRITE(12,'(''    Energy    mb/MeV'')')
          WRITE(12,*)' '
          DO ie = 1, nspec - 1
            WRITE(12,'(F9.4,E15.5)')FLOAT(ie - 1)*DE, 
     &                              MAX(0.D0,CSE(ie,6,0))
          ENDDO
C---------Exact endpoint
          WRITE(12,'(F9.4,E15.5)')EMAx(1) - Q(6,1), 
     &                            MAX(0.D0,CSE(nspec,6,0))
          WRITE(12,'(F9.4,E15.5)')EMAx(1) - Q(6,1), 0.D0
          totspec = 0.D0
          DO ie = 1, nspec - 1
            totspec = totspec + CSE(ie,6,0)
          ENDDO
          totspec = totspec - 0.5D0*(CSE(1,6,0) + CSE(nspec - 1,6,0))
          totspec = totspec*DE
 
          WRITE(12,*)' '
          WRITE(12,'(1x,'' Integrated spectrum   '',G12.5,'' mb'')')
     &          totspec
          WRITE(12,*)' '
 
        ENDIF
 
C--------light ions
        IF(NDEjc.EQ.7)THEN
          recorp = (1. + EJMass(NDEjc)/AMAss(1))
          nspec = INT((EMAx(1) - Q(NDEjc,1))/DE) + 2
          IF(nspec.GT.0)THEN
            IF(nspec.GT.NDEcse - 1)nspec = NDEcse - 1
            WRITE(12,*)' '
            WRITE(12,
     &           '(''  Spectrum of  '',I1,''-'',A2,4X,A5,I7,''(LI,x)'')'
     &           )INT(AEJc(NDEjc)), SYMbe(NDEjc), ' ZAP=', IZAejc(NDEjc)
            WRITE(12,*)' '
            WRITE(12,'(''    Energy    mb/MeV'')')
            WRITE(12,*)' '
            DO ie = 1, nspec - 1
              WRITE(12,'(F9.4,E15.5)')FLOAT(ie - 1)*DE, 
     &                                MAX(0.D0,CSE(ie,NDEjc,0))
            ENDDO
C------------exact endpoint
            WRITE(12,'(F9.4,E15.5)')EMAx(1) - Q(NDEjc,1), 
     &                              MAX(0.D0,CSE(nspec,NDEjc,0))
            WRITE(12,'(F9.4,E15.5)')EMAx(1) - Q(NDEjc,1), 0.D0
 
            totspec = 0.D0
            DO ie = 1, nspec - 1
              totspec = totspec + CSE(ie,NDEjc,0)
            ENDDO
            totspec = totspec - 
     &                0.5D0*(CSE(1,NDEjc,0) + CSE(nspec - 1,NDEjc,0))
            totspec = totspec*DE
 
            WRITE(12,*)' '
            WRITE(12,'(1x,'' Integrated spectrum   '',G12.5,'' mb'')')
     &            totspec
            WRITE(12,*)' '
 
          ENDIF
        ENDIF
        WRITE(12,*)' '
      ENDIF
 
C  Summary of exclusive emission cross sections
      jnmx = 0
      jzmx = 0
      jfiss = 0
      DO jn = 0, 20
        DO jz = 0, 15
          IF(xcross(NDEjc + 1,jz,jn).GT.0.01D0)jfiss = 1
          IF(xcross(NDEjc + 2,jz,jn).GT.0.01D0)THEN
            jnmx = MAX(jnmx,jn)
            jzmx = MAX(jzmx,jz)
          ENDIF
        ENDDO
      ENDDO
C     IF(jnmx.gt.3 .AND. jzmx.gt.2) THEN
      IF(jnmx.GT.1.AND.jzmx.GT.1)THEN
        iz = INT(Z(1))
        ia = INT(A(1)) - iz
        WRITE(12,*)' '
        WRITE(12,*)' Gamma emission cross sections (mb) ZAP=    0'
        WRITE(12,*)' '
        WRITE(12,'(''  Z / N '',20(i6,2x))')(ia - jn,jn = 0,jnmx)
        DO jz = 0, jzmx
          WRITE(12,'(i5,2x,20f8.2)')iz - jz, 
     &                              (xcross(0,jz,jn),jn = 0,jnmx)
        ENDDO
        WRITE(12,*)' '
        WRITE(12,*)' '
        WRITE(12,*)' Neutron emission cross sections (mb) ZAP=    1'
        WRITE(12,*)' '
        WRITE(12,'(''  Z / N '',20(i6,2x))')(ia - jn,jn = 0,jnmx)
        DO jz = 0, jzmx
          WRITE(12,'(i5,2x,20f8.2)')iz - jz, 
     &                              (xcross(1,jz,jn),jn = 0,jnmx)
        ENDDO
        WRITE(12,*)' '
        WRITE(12,*)' '
        WRITE(12,*)' Proton emission cross sections (mb) ZAP= 1001'
        WRITE(12,*)' '
        WRITE(12,'(''  Z / N '',20(i6,2x))')(ia - jn,jn = 0,jnmx)
        DO jz = 0, jzmx
          WRITE(12,'(i5,2x,20f8.2)')iz - jz, 
     &                              (xcross(2,jz,jn),jn = 0,jnmx)
        ENDDO
        WRITE(12,*)' '
        WRITE(12,*)' '
        WRITE(12,*)' Alpha emission cross sections (mb) ZAP= 2004'
        WRITE(12,*)' '
        WRITE(12,'(''  Z / N '',20(i6,2x))')(ia - jn,jn = 0,jnmx)
        DO jz = 0, jzmx
          WRITE(12,'(i5,2x,20f8.2)')iz - jz, 
     &                              (xcross(3,jz,jn),jn = 0,jnmx)
        ENDDO
        WRITE(12,*)' '
        WRITE(12,*)' '
        WRITE(12,*)' Deuteron emission cross sections (mb) ZAP= 1002'
        WRITE(12,*)' '
        WRITE(12,'(''  Z / N '',20(i6,2x))')(ia - jn,jn = 0,jnmx)
        DO jz = 0, jzmx
          WRITE(12,'(i5,2x,20f8.2)')iz - jz, 
     &                              (xcross(4,jz,jn),jn = 0,jnmx)
        ENDDO
        WRITE(12,*)' '
        WRITE(12,*)' '
        WRITE(12,*)' Triton emission cross sections (mb) ZAP= 1003'
        WRITE(12,*)' '
        WRITE(12,'(''  Z / N '',20(i6,2x))')(ia - jn,jn = 0,jnmx)
        DO jz = 0, jzmx
          WRITE(12,'(i5,2x,20f8.2)')iz - jz, 
     &                              (xcross(5,jz,jn),jn = 0,jnmx)
        ENDDO
        WRITE(12,*)' '
        WRITE(12,*)' '
        WRITE(12,*)' Helium-3 emission cross sections (mb) ZAP= 2003'
        WRITE(12,*)' '
        WRITE(12,'(''  Z / N '',20(i6,2x))')(ia - jn,jn = 0,jnmx)
 
        DO jz = 0, jzmx
          WRITE(12,'(i5,2x,20f8.2)')iz - jz, 
     &                              (xcross(6,jz,jn),jn = 0,jnmx)
        ENDDO
        WRITE(12,*)' '
 
        IF(NDEjc.EQ.7)THEN
          WRITE(12,*)' '
          WRITE(12,
     &         '(I2,''-'',A2,'' emission cross sections (mb) ZAP='',I5)'
     &         )INT(AEJc(NDEjc)), SYMbe(NDEjc), IZAejc(NDEjc)
          WRITE(12,*)' Ion emission cross sections (mb)'
          WRITE(12,*)' '
          WRITE(12,'(''  Z / N '',20(i6,2x))')(ia - jn,jn = 0,jnmx)
          DO jz = 0, jzmx
            WRITE(12,'(i5,2x,20f8.2)')iz - jz, 
     &                                (xcross(NDEjc,jz,jn),jn = 0,jnmx)
          ENDDO
          WRITE(12,*)' '
        ENDIF
        IF(jfiss.GT.0)THEN
          WRITE(12,*)' '
          WRITE(12,*)' Fission cross sections (mb)'
          WRITE(12,*)' '
          WRITE(12,'(''  Z / N '',20(i6,2x))')(ia - jn,jn = 0,jnmx)
          DO jz = 0, jzmx
            WRITE(12,'(i5,2x,20f8.2)')iz - jz, 
     &                                (xcross(NDEjc + 1,jz,jn),jn = 0,
     &                                jnmx)
          ENDDO
          WRITE(12,*)' '
        ENDIF
        WRITE(12,*)' '
        WRITE(12,*)' Initial populations (mb)'
        WRITE(12,*)' '
        WRITE(12,'(''  Z / N '',20(i6,2x))')(ia - jn,jn = 0,jnmx)
        DO jz = 0, jzmx
          WRITE(12,'(i5,2x,20f8.2)')iz - jz, 
     &                              (xcross(NDEjc + 3,jz,jn),jn = 0,
     &                              jnmx)
        ENDDO
        WRITE(12,*)' '
        WRITE(12,*)' '
 
        totsum = 0.D0
        DO jz = 0, jzmx
          DO jn = 0, jnmx
            totsum = totsum + xcross(NDEjc + 2,jz,jn)
          ENDDO
        ENDDO
 
        WRITE(12,*)' Total production cross sections (mb) :', totsum
        WRITE(12,*)' '
        WRITE(12,'(''  Z / N '',20(i6,2x))')(ia - jn,jn = 0,jnmx)
        DO jz = 0, jzmx
          WRITE(12,'(i5,2x,20f8.2)')iz - jz, 
     &                              (xcross(NDEjc + 2,jz,jn),jn = 0,
     &                              jnmx)
        ENDDO
        WRITE(12,*)' '
      ENDIF
C-----End of ENDF spectra (inclusive)
C
  190 IF(FITomp.GE.0)THEN
  195   READ(5,'(A36)',END = 210)nextenergy
        IF(nextenergy(1:1).EQ.'$')THEN
          READ(nextenergy,'(1x,A6,G10.5,4I5)',END = 210)keyname, val, 
     &         ikey1, ikey2, ikey3, ikey4
          CALL OPTIONS(keyname,val,ikey1,ikey2,ikey3,ikey4,1)
          GOTO 195
        ENDIF
        IF(nextenergy(1:1).EQ.'*'.OR.nextenergy(1:1).EQ.'#'.OR.
     &     nextenergy(1:1).EQ.'!')GOTO 195
 
        IF(nextenergy(1:1).EQ.'@')THEN
          BACKSPACE 5
          READ(5,'(A72)')rtitle
 
          rtitle(1:1) = ' '
 
          WRITE(*,*)'***', TRIM(rtitle)
          WRITE(8,*)
     &             '***************************************************'
          WRITE(8,*)'***', TRIM(rtitle)
          WRITE(8,*)
     &             '***************************************************'
          GOTO 195   ! next line
        ENDIF
 
        BACKSPACE 5
C       READ(nextenergy,'(G15.5)',END=1200) EIN
        READ(5,*,END = 210)EIN
 
      ELSE
  200   READ(5,'(A36)',END = 210)nextenergy
        IF(nextenergy(1:1).EQ.'*'.OR.nextenergy(1:1).EQ.'#'.OR.
     &     nextenergy(1:1).EQ.'!'.OR.nextenergy(1:1).EQ.'$')GOTO 200
 
        IF(nextenergy(1:1).EQ.'@')THEN
          BACKSPACE 5
          READ(5,'(A72)')rtitle
 
          rtitle(1:1) = ' '
 
          WRITE(*,*)'***', TRIM(rtitle)
          WRITE(8,*)
     &             '***************************************************'
          WRITE(8,*)'***', TRIM(rtitle)
          WRITE(8,*)
     &             '***************************************************'
          GOTO 200   ! next line
        ENDIF
 
        BACKSPACE 5
 
        READ(5,*,END = 210)EIN, NDAng, ICAlangs
        IF(NDAng.LT.2)THEN
          NDAng = 2
          ANGles(1) = 0.
          ANGles(2) = 180.
        ELSE
          READ(5,*,END = 210)(ANGles(na),na = 1,NDAng)
        ENDIF
        NANgela = NDAng
        IF(NANgela.GT.NDAngecis)THEN
          WRITE(8,*)'ERROR: INCREASE NDANGECIS IN dimension.h UP TO ', 
     &              NANgela
          STOP 'FATAL: INCREASE NDAngecis IN dimension.h'
        ENDIF
      ENDIF
      GOTO 220
  210 EIN = -1
  220 IF(EIN.LT.0.0D0)THEN
C
C        CLOSING FILES
C
        WRITE(12,*)' '
        WRITE(12,*)' CALCULATIONS COMPLETED SUCCESSFULLY'
        CLOSE(5)
        WRITE(*,*)'.'
        WRITE(*,*)' CALCULATIONS COMPLETED SUCCESSFULLY'
        WRITE(8,*)' '
        WRITE(8,*)' CALCULATIONS COMPLETED SUCCESSFULLY'
        CALL THORA(8)
        CLOSE(8)
        CLOSE(11)
        CLOSE(12)
        CLOSE(13)
        IF(FILevel)CLOSE(14)
        CLOSE(15,STATUS = 'delete')
        CLOSE(16,STATUS = 'delete')
        CLOSE(23)
        CLOSE(24)
        CLOSE(29)
        CLOSE(33)
        CLOSE(40)
        CLOSE(41)
        IF(DEGa.GT.0)THEN
          CLOSE(42)
        ELSE
          CLOSE(42,STATUS = 'delete')
        ENDIF
        CLOSE(53)
        CLOSE(58)
        CLOSE(66,STATUS = 'delete')
C        CLOSE (68) ! for Chris
        IF(FISspe.GT.0)THEN
          CLOSE(73)
          CLOSE(74)
        ENDIF
        CLOSE(98)
C--------Saving random seeds
        ftmp = GRAND()
        OPEN(94,FILE = 'R250SEED.DAT',STATUS = 'UNKNOWN')
        WRITE(94,*)INDexf, INDexb
        DO i = 1, 250
          WRITE(94,*)BUFfer(i)
        ENDDO
        CLOSE(94)
        CLOSE(102)
        RETURN
      ENDIF
      IF(EIN.LT.epre.AND..NOT.BENchm)THEN
        WRITE(8,*)EIN, epre
        WRITE(8,*)'ERROR: INPUT ENERGIES OUT OF ORDER !!'
        WRITE(8,*)'ERROR: CHECK YOUR INPUT FILE'
        PAUSE 'FATAL: INPUT ENERGIES OUT OF ORDER !!'
        STOP
      ENDIF
      epre = EIN
C-----
C-----
      IF(FITomp.GE.0)THEN
        IF(LHMs.EQ.0)THEN
          NANgela = 91
          NDAng = 91
        ELSE
          NANgela = NDAnghmx
          NDAng = NDAnghmx
        ENDIF
C-------Set angles for inelastic calculations
        da = 180.0/(NDAng - 1)
        DO na = 1, NDAng
          ANGles(na) = (na - 1)*da
        ENDDO
      ENDIF
      DO na = 1, NDAng
        CANgler(na) = COS(ANGles(NDAng - na + 1)*PI/180.)
        SANgler(na) = SQRT(1.D0 - CANgler(na)**2)
      ENDDO
C     IF(.not.BENchm) FIRst_ein = .FALSE.
      FIRst_ein = .FALSE.
C
      GOTO 140
 1240 FORMAT(I12,F10.5,I5,F8.1,G15.6,I3,7(I4,F7.4),:/,(53X,7(I4,F7.4)))
 1250 FORMAT(1X,F5.2,12G10.3)
      END SUBROUTINE EMPIRE
 
!---------------------------------------------------------------------------
C
C
      SUBROUTINE GET_RECOIL(Ke,Nnuc)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      INTEGER :: Ke, Nnuc
C
C Local variables
C
      REAL*8 :: ares, coef, dang, erecejc, erecod, erecoil, erecpar, 
     &          exqcut, recorr, sumnor, weight, zres
      REAL :: FLOAT
      INTEGER :: icse, ie, il, iloc, ire, irec, izares, na, nejc, nnur
      INTEGER :: INT
C
C*** End of declarations rewritten by SPAG
C
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:ppu*
Ccc   *                         RECOIL                                   *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   *  Constructs recoil spectra:                                      *
Ccc   *  Each excitation bin is given recoil spectrum, when a particle   *
Ccc   *  is emitted its recoil velocity is vector added to the parent    *
Ccc   *  recoil velocity and a resulting spectrum is summed upon daughter*
Ccc   *  recoil spectrum corresponding to the populated energy bin in the*
Ccc   *  daughter (kinematical normalization taken into account).        *
Ccc   *  Daughter recoil spectra will be distributed between             *
Ccc   *  adjacent bins (inversly proportional to the                     *
Ccc   *  distance of the actual energy to the bin energy                 *
Ccc   *  in order to conserve energy).                                   *
Ccc   *  Requires that continuum spectra from each bin are stored on the *
Ccc   *  AUSpec array and those to discrete levels on the REClev for each*
Ccc   *  reaction mechanism considered in the calculations.              *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
C
C
C-----Normalize recoil spectrum of the parent
      sumnor = 0.0
      DO ire = 1, NDErec
        sumnor = sumnor + RECcse(ire,Ke,Nnuc)
      ENDDO
      IF(sumnor.NE.0.0D0)THEN
        DO ire = 1, NDErec
          RECcse(ire,Ke,Nnuc) = RECcse(ire,Ke,Nnuc)/sumnor
        ENDDO
      ENDIF
      dang = PI/FLOAT(NDAng - 1)
      coef = dang/DERec/2.0
      DO nejc = 1, NEJcm   !over ejectiles
        ares = A(Nnuc) - AEJc(nejc)
        zres = Z(Nnuc) - ZEJc(nejc)
C--------Residual nuclei must be heavier than alpha
        IF(ares.LE.4.AND.zres.LE.2.)CYCLE
        izares = INT(1000.0*zres + ares)
        CALL WHERE(izares,nnur,iloc)
        IF(iloc.EQ.1)CYCLE
C--------Decay to continuum
C--------recorr is a recoil correction factor that
C--------divides outgoing energies
        recorr = AMAss(Nnuc)/EJMass(nejc)
        exqcut = EX(Ke,Nnuc) - Q(nejc,Nnuc) - ECUt(nnur)
        DO ie = 1, NDEcse  !over ejec. energy (daughter excitation)
          icse = (exqcut - (ie - 1)*DE)/DE + 1.001
C-----------Daughter bin
          IF(icse.LE.0)EXIT
          erecejc = (ie - 1)*DE/recorr
          DO ire = 1, NDErec            !over recoil spectrum
            erecpar = (ire - 1)*DERec
            DO na = 1, NDAng
              erecoil = erecejc + erecpar
              erecoil = erecoil + 2.0*SQRT(erecejc*erecpar)*CANgler(na)
              irec = erecoil/DERec + 1.001
              weight = (erecoil - (irec - 1)*DERec)/DERec
              IF(irec + 1.GT.NDErec)EXIT
              RECcse(irec,icse,nnur) = RECcse(irec,icse,nnur)
     &                                 + RECcse(ire,Ke,Nnuc)
     &                                 *AUSpec(ie,nejc)*(1.0 - weight)
     &                                 *SANgler(na)*coef
              RECcse(irec + 1,icse,nnur) = RECcse(irec + 1,icse,nnur)
     &          + RECcse(ire,Ke,Nnuc)*AUSpec(ie,nejc)*weight*SANgler(na)
     &          *coef
            ENDDO                     !over angles
          ENDDO                    !over recoil spectrum
        ENDDO                   !over  daugther excitation
C--------Decay to discrete levels (stored with icse=0)
        exqcut = exqcut + ECUt(nnur)
        DO il = 1, NLV(nnur)
          erecod = exqcut - ELV(il,nnur)     !emission energy
          erecod = erecod/recorr
          IF(erecod.LT.0)EXIT
          DO ire = 1, NDErec        !over recoil spectrum
            DO na = 1, NDAng    !over angles
              erecoil = (ire - 1)*DERec + erecod + 
     &                  2.0*SQRT((ire - 1)*DERec*erecod)*CANgler(na)
              irec = erecoil/DERec + 1.001
              weight = (erecoil - (irec - 1)*DERec)/DERec
              IF(irec.GT.NDErec)EXIT
              RECcse(irec,0,nnur) = RECcse(irec,0,nnur)
     &                              + RECcse(ire,Ke,Nnuc)
     &                              *REClev(il,nejc)*(1.0 - weight)
     &                              *SANgler(na)*coef
C------------------------
C                 IF(irec.EQ.5 .AND. RECcse(irec,0,nnur).GT.0
C    &               .AND.na.EQ.10) THEN
C                 WRITE(8,*) '       Parent bin', Ke
C                 WRITE(8,*) 'Recoil bin', ire
C                 WRITE(8,*) 'Erecoil ', erecoil, erecod, nnuc
C                 WRITE(8,*) 'RECcse, RECcse par, REClev',
C    &            RECcse(irec,0,nnur),RECcse(ire,Ke,Nnuc),
C    &            REClev(il,nejc)
C                 ENDIF
C------------------------
              IF(irec + 1.GT.NDErec)EXIT
              RECcse(irec + 1,0,nnur) = RECcse(irec + 1,0,nnur)
     &                                  + RECcse(ire,Ke,Nnuc)
     &                                  *REClev(il,nejc)
     &                                  *weight*SANgler(na)*coef
            ENDDO                     !over angles
          ENDDO                    !over recoil spectrum
        ENDDO                   !over levels
      ENDDO                  !over ejectiles
C-----
C-----Parent recoil spectrum after gamma decay
C-----
      nnur = Nnuc
      nejc = 0
C-----gamma decay to continuum
      DO ie = 1, NDEcse !over ejec. energy (daughter excitation)
        icse = (EX(Ke,Nnuc) - (ie - 1)*DE - ECUt(nnur))/DE + 1.001
C--------!daughter bin
        DO irec = 1, NDErec          !over recoil spectrum
          IF(icse.GT.0)RECcse(irec,icse,nnur) = RECcse(irec,icse,nnur)
     &       + RECcse(irec,Ke,Nnuc)*AUSpec(ie,0)/DERec
        ENDDO                   !over recoil spectrum
      ENDDO                  !over  daugther excitation
C-----gamma decay to discrete levels (stored with icse=0)
      DO il = 1, NLV(nnur)
        DO ire = 1, NDErec              !over recoil spectrum
          RECcse(ire,0,nnur) = RECcse(ire,0,nnur) + RECcse(ire,Ke,Nnuc)
     &                         *REClev(il,nejc)/DERec
        ENDDO                   !over recoil spectrum
      ENDDO                  !over levels
      END SUBROUTINE GET_RECOIL
 
!---------------------------------------------------------------------------
 
      SUBROUTINE PRINT_RECOIL(Nnuc,React)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      INTEGER :: Nnuc
      CHARACTER(21) :: React
C
C Local variables
C
      REAL*8 :: corr
      REAL :: FLOAT
      INTEGER :: ie, ilast
C
C*** End of declarations rewritten by SPAG
C
C-----
C-----Prints recoil spectrum of nnuc residue
C-----
      IF(CSPrd(Nnuc).GT.0.0D0)THEN
C-------Normalize recoil spectra to remove eventual inaccuracy
C-------due to numerical integration of angular distributions
C-------and find last non-zero cross section for printing
        corr = 0.0
        ilast = 0
        DO ie = 1, NDErec
          corr = corr + RECcse(ie,0,Nnuc)
          IF(RECcse(ie,0,Nnuc).NE.0)ilast = ie
        ENDDO
        IF(corr.EQ.0)RETURN
C        WRITE(8,*)'nnuc, rec, cs',nnuc,corr*DERec,CSPrd(nnuc)
        corr = CSPrd(Nnuc)/corr/DERec
        ilast = MIN(ilast + 1,NDErec)
        DO ie = 1, ilast
          RECcse(ie,0,Nnuc) = RECcse(ie,0,Nnuc)*corr
        ENDDO
        WRITE(12,*)' '
        WRITE(12,'(A23,A7,A4,I6,A6,F12.5)')'  Spectrum of recoils  ', 
     &        React, 'ZAP=', IZA(Nnuc), ' mass=', AMAss(Nnuc)
        WRITE(12,*)' '
        WRITE(12,'('' Energy    mb/MeV'')')
        WRITE(12,*)' '
        DO ie = 1, ilast
          WRITE(12,'(F9.4,E15.5)')FLOAT(ie - 1)*DERec, RECcse(ie,0,Nnuc)
        ENDDO
C--------Print end point again with 0 xs for consistency with particle spectra
C        WRITE (12,'(F9.4,E15.5)') FLOAT(ilast - 1)*DERec,
C    &                             RECcse(ilast + 1,0,Nnuc)
        WRITE(12,'(F9.4,E15.5)')FLOAT(ilast - 1)*DERec, 0.D0
        IF(ABS(1.0 - corr).GT.0.01D0.AND.CSPrd(Nnuc).GT.0.001D0)THEN
          WRITE(8,*)' '
          WRITE(8,*)'WARNING:  Ein = ', EIN, ' MeV ZAP = ', IZA(Nnuc), 
     &              ' from ', React
          WRITE(8,*)'WARNING: x-section balance in recoils '
          WRITE(8,*)'WARNING: difference = ', (1.0 - corr)*100.0, '%'
          WRITE(8,*)'WARNING: production cross section = ', CSPrd(Nnuc)
          WRITE(8,*)' '
        ENDIF
      ENDIF
      END SUBROUTINE PRINT_RECOIL
 
!---------------------------------------------------------------------------
 
      SUBROUTINE FISCROSS(Nnuc,Ke,Ip,Jcn,Sumfis,Sumfism,Dencomp,Aafis,
     &                    Ifluct)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: PFIso, RFIso, TABs, TDIr, TFIso, TG2, TGIso, TISo
      REAL*8, DIMENSION(NFParab) :: TF
      COMMON /FIS_ISO/ TFIso, TGIso, TISo, RFIso, PFIso
      COMMON /IMAG  / TF, TDIr, TABs, TG2
C
C Dummy arguments
C
      REAL*8 :: Aafis, Dencomp, Sumfis
      INTEGER :: Ifluct, Ip, Jcn, Ke, Nnuc
      REAL*8, DIMENSION(NFMod) :: Sumfism
C
C Local variables
C
      REAL*8 :: bbfis, cota, cota1, cotaexp, tout
      INTEGER :: INT
      INTEGER :: k, kk, m
C
C*** End of declarations rewritten by SPAG
C
C
 
 
 
 
 
      IF(Ifluct.EQ.0)Dencomp = DENhf
      Aafis = 0.
      cota1 = 0.D0
      IF(FISmod(Nnuc).EQ.0.)THEN
        CALL FISFIS(Nnuc,Ke,Ip,Jcn,Sumfis,0)
        IF(FISopt(Nnuc).GT.0.)THEN
          IF(NRWel.EQ.1)cota1 = (TF(1) + TF(2) + TG2)/2
        ENDIF
      ENDIF
      IF(FISmod(Nnuc).GT.0.)THEN
        TFB = 0.
        TDIrect = 0.
        DO m = 1, INT(FISmod(Nnuc)) + 1
          EFB(2) = EFBm(m)
          DO k = 1, NRFdis(2)
            H(k,2) = HM(k,m)
            EFDis(k,2) = EFDism(k,m)
          ENDDO
          XMInn(2) = XMInnm(m)
          NRBinfis(2) = NRBinfism(m)
          DEStepp(2) = DEStepm(m)
          DO kk = 1, NRBinfis(2)
            UGRid(kk,2) = UGRidf(kk,m)
            ROFisp(kk,Jcn,1,2) = ROFism(kk,Jcn,m)
            ROFisp(kk,Jcn,2,2) = ROFism(kk,Jcn,m)
          ENDDO
          CALL FISFIS(Nnuc,Ke,Ip,Jcn,Sumfis,m)
          TFBm(m) = TF(2)
          TFB = TFB + TFBm(m)
          IF(FISopt(Nnuc).GT.0.)THEN
            TDIrm(m) = TDIr
            TDIrect = TDIrect + TDIr
          ENDIF
          IF(FISopt(Nnuc).GT.0.)cota1 = (TF(1) + TFB + TG2)/2
        ENDDO
      ENDIF
      IF(cota1.LE.EXPmax)THEN
        cotaexp = EXP( - cota1)
      ELSE
        cotaexp = 0.D0
      ENDIF
      cota = (1 + cotaexp**2)/(1 - cotaexp**2 + 0.00000001)
      IF(FISmod(Nnuc).EQ.0..AND.FISopt(Nnuc).GT.0.)THEN
        IF(NRHump.EQ.2.AND.NRWel.EQ.1)tout = TF(2)
        IF(tout*TABs.GT.0.)THEN
          bbfis = (TDIr + Dencomp)*(TF(1) + tout + TG2)
     &            /(TABs*(tout + TG2))
          Aafis = (1. + bbfis**2 + 2*bbfis*cota)**( - 0.5)
        ELSE
          Aafis = 0.
        ENDIF
        PFIso = 0.
        IF(tout.GT.0.AND.TG2.GT.0.)PFIso = (TDIr + Dencomp)*(RFIso - 1.)
     &     *TG2*Aafis/(tout + TG2)
      ENDIF
      IF(FISmod(Nnuc).GT.0.)THEN
        IF(TFB*TABs.GT.0.)THEN
          bbfis = (TDIrect + Dencomp)*(TF(1) + TFB)/(TABs*TFB)
          Aafis = (1. + bbfis**2 + 2*bbfis*cota)**( - 0.5)
        ELSE
          Aafis = 0.
        ENDIF
        DO m = 1, INT(FISmod(Nnuc)) + 1
          IF((TF(1) + TFB).GT.0.)THEN
            Sumfism(m) = TF(1)*TFBm(m)/(TF(1) + TFB)
          ELSE
            Sumfism(m) = 0.
          ENDIF
        ENDDO
      ENDIF
      END SUBROUTINE FISCROSS
 
!---------------------------------------------------------------------------
 
      SUBROUTINE XSECT(Nnuc,M,Xnor,Sumfis,Sumfism,Ke,Ipar,Jcn,Dencomp,
     &                 Aafis,Fisxse)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: PFIso, RFIso, TABs, TDIr, TFIso, TG2, TGIso, TISo
      REAL*8, DIMENSION(NFParab) :: TF
      COMMON /FIS_ISO/ TFIso, TGIso, TISo, RFIso, PFIso
      COMMON /IMAG  / TF, TDIr, TABs, TG2
C
C Dummy arguments
C
      REAL*8 :: Aafis, Dencomp, Fisxse, Sumfis, Xnor
      INTEGER :: Ipar, Jcn, Ke, M, Nnuc
      REAL*8, DIMENSION(NFMod) :: Sumfism
C
C Local variables
C
      REAL*8 :: ares, xnorfis, zres
      INTEGER :: iloc, izares, nejc, nnur
      INTEGER :: INT
C
C*** End of declarations rewritten by SPAG
C
C
 
 
 
 
 
 
      Dencomp = DENhf - Sumfis
      IF(NRBar.GT.3)THEN
      ENDIF
      GOTO 10
      IF(FISsil(Nnuc).AND.FISopt(Nnuc).GT.0..AND.FISshi(Nnuc).NE.1.)THEN
        IF(FISmod(Nnuc).EQ.0.)THEN
          IF((Dencomp + TDIr).GT.0.)THEN
            xnorfis = Xnor*DENhf/(Dencomp + TDIr)
          ELSE
            xnorfis = 0.D0
          ENDIF
C-----------Fission
          Fisxse = xnorfis*(TDIr + Dencomp*Aafis + PFIso)
          CSFis = CSFis + Fisxse
          IF(ENDf(Nnuc).EQ.1.AND.Fisxse.NE.0.0D+0.AND.POPbin(Ke,Nnuc)
     &       .GT.0.D0)CALL EXCLUSIVEC(Ke,0, - 1,Nnuc,0,Fisxse)
        ENDIF
        IF(FISmod(Nnuc).GT.0.)THEN
          IF((Dencomp + TDIrect).GT.0.)THEN
            xnorfis = Xnor*DENhf/(Dencomp + TDIrect)
          ELSE
            xnorfis = 0.
          ENDIF
          Fisxse = 0.D0
          DO M = 1, INT(FISmod(Nnuc)) + 1
            Fisxse = xnorfis*(TDIrm(M)*(1. - Aafis) + TFBm(M)
     &               *Aafis*(Dencomp + TDIrect)/TFB)
            CSFism(M) = CSFism(M) + Fisxse
          ENDDO
C
C-----------Multimodal should be updated to allow for PFNS calculation !!!!
C
        ENDIF
C--------particles
        DO nejc = 1, NEJcm
          ares = A(Nnuc) - AEJc(nejc)
          zres = Z(Nnuc) - ZEJc(nejc)
C           residual nuclei must be heavier than alpha
          IF(ares.LE.4.AND.zres.LE.2.)CYCLE
          izares = INT(1000.0*zres + ares)
          CALL WHERE(izares,nnur,iloc)
          IF(iloc.EQ.1)CYCLE
          CALL ACCUM(Ke,Nnuc,nnur,nejc,Xnor)
          CSEmis(nejc,Nnuc) = CSEmis(nejc,Nnuc) + xnorfis*SCRtem(nejc)
     &                        *(1 - Aafis)
        ENDDO
C--------gammas
        CALL ACCUM(Ke,Nnuc,Nnuc,0,Xnor)
        CSEmis(0,Nnuc) = CSEmis(0,Nnuc) + xnorfis*SCRtem(0)*(1 - Aafis)
        POP(Ke,Jcn,Ipar,Nnuc) = 0.0
        RETURN
      ENDIF
C-----No subbarrier effects
C-----particles
   10 DO nejc = 1, NEJcm
        ares = A(Nnuc) - AEJc(nejc)
        zres = Z(Nnuc) - ZEJc(nejc)
C        residual nuclei must be heavier than alpha
        IF(ares.LE.4.AND.zres.LE.2.)CYCLE
        izares = INT(1000.0*zres + ares)
        CALL WHERE(izares,nnur,iloc)
        IF(iloc.EQ.1)CYCLE
        CALL ACCUM(Ke,Nnuc,nnur,nejc,Xnor)
        CSEmis(nejc,Nnuc) = CSEmis(nejc,Nnuc) + Xnor*SCRtem(nejc)
      ENDDO
C-----gammas
      CALL ACCUM(Ke,Nnuc,Nnuc,0,Xnor)
      CSEmis(0,Nnuc) = CSEmis(0,Nnuc) + Xnor*SCRtem(0)
      POP(Ke,Jcn,Ipar,Nnuc) = 0.0
C-----fission
      IF(FISmod(Nnuc).EQ.0.)THEN
        Fisxse = Sumfis*Xnor
        CSFis = CSFis + Fisxse
        IF(ENDf(Nnuc).EQ.1.AND.Fisxse.NE.0.0D+0.AND.POPbin(Ke,Nnuc)
     &     .GT.0.D0)CALL EXCLUSIVEC(Ke,0, - 1,Nnuc,0,Fisxse)
      ENDIF
C
C-----------Multimodal should be updated to allow for PFNS calculation !!!!
C
      IF(FISmod(Nnuc).GT.0.)THEN
        DO M = 1, INT(FISmod(Nnuc)) + 1
          CSFism(M) = CSFism(M) + Sumfism(M)*Xnor
        ENDDO
      ENDIF
      END SUBROUTINE XSECT
