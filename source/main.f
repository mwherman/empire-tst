Ccc   * $Author: herman $
Ccc   * $Date: 2007-05-22 19:33:44 $
Ccc   * $Id: main.f,v 1.177 2007-05-22 19:33:44 herman Exp $

      SUBROUTINE EMPIRE
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:ppu*
Ccc   *                         E M P I R E                              *
Ccc   *                                                                  *
Ccc   *               Used to be main of the EMPIRE code                 *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   ********************************************************************
      INCLUDE "dimension.h"
      INCLUDE "global.h"
C
C COMMON variables
C
      DOUBLE PRECISION ABScs, AFIsm(NFMOD), CSFism(NFMOD), DEFbm(NFMOD),
     &                 DELtafism(NFMOD), DEStepm(NFMOD), EFBm(NFMOD),
     &                 EFDism(NFTRANS,NFMOD), ELAcs, GAMmafism(NFMOD),
     &                 HM(NFTRANS,NFMOD), PFIso, RFIso,
     &                 ROFism(0:NFISENMAX,NDLW,NFMOD), SHCfism(NFMOD),
     &                 SINl, TDIrect, TDIrm(NFMOD), TFB, TFBm(NFMOD),
     &                 TFIso, TGIso, TISo, TOTcs, SINlcc, SINlcont,
     &                 UGRidf(0:NFISENMAX,NFMOD), WFIsm(NFMOD),
     &                 XMInnm(NFMOD),XCOs(NDAngecis)
      INTEGER BFFm(NFMOD), NRBinfism(NFMOD)
      INTEGER*4 INDexf, INDexb, BUFfer(250)
      COMMON /ECISXS/ ELAcs, TOTcs, ABScs, SINl, SINlcc, SINlcont
      COMMON /FISSMOD/ ROFism, HM, EFDism, UGRidf, EFBm, XMInnm, AFIsm,
     &                 DEFbm, SHCfism, DELtafism, GAMmafism, WFIsm,
     &                 BFFm, NRBinfism, DEStepm, TFBm, TDIrm, CSFism,
     &                 TFB, TDIrect
      COMMON /FIS_ISO/ TFIso, TGIso, TISo, RFIso, PFIso
      COMMON /R250COM/ INDexf,INDexb,BUFfer
      COMMON /KALB/ XCOs

C     DOUBLE PRECISION tres,fkt,prefise(20),nubar(20),nubart
C     DOUBLE PRECISION postfise(20),rfc(20)
C     DOUBLE PRECISION efl(20),efh(20),tm(20)
C     DOUBLE PRECISION er(20),tke(20),amash(20),sn(20),egam(20)
      DOUBLE PRECISION eenc(200),signcf(200)

C     COMMON /nubar1 / tres,fkt,prefise,nubar,nubart
C     COMMON /nubar2 / postfise, rfc
C     COMMON /nubar3 / efl,efh,tm
C     COMMON /inp_sp1/ er,tke,amash,sn,egam
C     COMMON /inp_sp3/ flf,fhf,finput,filsan,fnc
      COMMON /inp_sp5/ eenc,signcf,nrnc
C
C Local functions
C     
      DOUBLE PRECISION fniu,fniuTH232,fniuLANL
C
C Local variables
C
      DOUBLE PRECISION aafis, ares, atotsp, coef, controln, controlp,
     &                 corrmsd, csemax, csemist, csmsdl, csum, cturbo,
     &                 dang, debinhms, ded, delang, dencomp, echannel,
     &                 ecm, elada(NDAngecis), elleg(NDAngecis), emeda,
     &                 emedg, emedh, emedn, emedp, erecoil, espec,
     &                 espmax, epre, ftmp, gamfis, gamt, gang, grand,
     &                 gtotsp, htotsp, piece, pope, poph, popl, popleft,
     &                 poplev, popread, poptot, ptotsp, q2, q3, qmax,
     &                 qstep, recorp, sgamc, spdif, spdiff, stauc,
     &                 step, sum, sumfis, sumfism(NFMOD), tauf, taut,
     &                 totemis, weight, xcse, xizat, xnhms, xnl, xnor,
     &                 xtotsp, xsinlcont, xsinl, zres, angstep, checkXS,
     &                 deform(NDCOLLEV), cseaprnt(ndecse,ndangecis),
     &                 emiss_en(NDEPFN),post_fisn(NDEPFN), tequiv0,
     &                 bindS(0:1), fniuS, tequiv, fmaxw, ftmp1, ftmp2,
C                      Total prompt fission spectra only for two ejectiles (n,g)
C                      Total PF angular distribution defined only for neutrons
     &                 cseapfns(NDEPFN,NDAngecis),enepfns(NDEPFN,0:1),
     &                 csepfns(NDEPFN,0:1),ratio2maxw(NDEPFN),
     &                 tequiv1, tequiv2, ddxs(NDAngecis), ebind, 
     &                 eincid, eee, uuuu, fanisot, eneutr
      CHARACTER*9 cejectile
      CHARACTER*3 ctldir
      CHARACTER*6 keyname
      CHARACTER*23 ctmp23
      CHARACTER*36 nextenergy
      CHARACTER*1 opart(3)
      DOUBLE PRECISION DMAX1, val
      REAL FLOAT
      INTEGER i, ia, iad, iam, iang, iang1, ib, icalled, nfission,
     &        icsh, icsl, ie, iizaejc, il, iloc, ilv, imaxt,
     &        imint, ip, ipar, irec, ispec, itimes, its, iz, izares, j,
     &        jcn, jj, ke, kemax, kemin, kk, ltrmax, m, 
     &        nang, nbr, ncoll, nejc, nejcec, nnuc, mintsp,
     &        nnur, nnurec, nnurn, nnurp, nrbarc1, nspec,   neles,
     &        itemp(NDCOLLEV), ikey1, ikey2, ikey3, ikey4, nepfns(0:1),
     &        isnewchain(0:1), ncon
      INTEGER INT, MIN0, NINT
      LOGICAL nvwful, fexist
      CHARACTER*21 reactionx
      INCLUDE 'io.h'
      DATA ctldir/'TL/'/
      DATA opart/1hn,1hp,1ha/
      icalled = 0
      CALL THORA(6)
      EIN = 0.0d0
      epre=EIN
      ICAlangs = 0
C-----
C-----Read and prepare input data
C-----
 1300 CALL INPUT
C
      IF (FITomp.LT.0) OPEN (40,FILE = 'OPTFIT.CAL',STATUS='UNKNOWN')
C-----
C-----Print input data
C-----
      IF (IOUt.GT.0) CALL PRINPUT
      WRITE (*,'(''  C.M. incident energy '',G10.5,'' MeV'')') EIN
      WRITE (6,'(''  C.M. incident energy '',G10.5,'' MeV'')') EIN
C-----
C-----Print results of the systematics
C-----
      IF (FIRst_ein) CALL SYSTEMATICS(SNGL(A(0)),SNGL(Z(0)),1)
C-----Clear CN elastic cross section (1/4*pi)
      elcncs = 0.0D+0
C-----
C-----Open file 41 with tabulated cross sections
C-----
      IF (FIRst_ein) THEN
        OPEN (41, FILE='XSECTIONS.OUT', STATUS='unknown')
        WRITE(41,'(''#'',I3,10X,i3,''-'',A2,''-'',I3)') NNUcd+3,
     &      int(Z(0)), SYMb(0), int(A(0))   
        WRITE(41,'(''#'',A10,1X,(90A12))') '  Einc    ','  Total     ',
     &       '  Elastic   ','  Reaction  ','  Fission   ',
     &         (REAction(nnuc),nnuc=1,NNUcd)
C       WRITE(41,'(''#'',A10,1X,(4A12\))') '  Einc    ','  Total     ',
C    &       '  Elastic   ','  Reaction  ','  Fission   '
C       DO nnuc=1,NNUcd
C         IF(ENDf(nnuc).GT.0) WRITE(41,'(A12\))') REAction(nnuc)
C       ENDDO 
        OPEN (98, FILE='FISS_XS.OUT', STATUS='unknown')
        WRITE(98,'(''#'',I3,10X,i3,''-'',A2,''-'',I3)') NNUcd+2,
     &      int(Z(0)), SYMb(0), int(A(0))
        WRITE(98,'(''#'',A10,1X,A12,90(1x,I3,''-'',A2,''-'',I3,1x))')
     &      '  Einc    ',' Total_Fiss ',
     &     (int(Z(nnuc)), SYMb(nnuc), int(A(nnuc)),nnuc=1,NNUcd)
        OPEN (73, FILE='PFNS.OUT', STATUS='unknown')
        OPEN (74, FILE='PFNM.OUT', STATUS='unknown')
        WRITE(74,1235)
C1235    FORMAT(' CN(A,Z)    Elab     Q       XSfis(i)  XSfis(i)/XSfis',
C     &            ' <Epfns>   <Epfns>(i)     nue      nue(i) ')
 1235    FORMAT('   Elab    <Epfns>  nue(th) nue(exp)  Tmaxw ')
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
      IF (INT(AEJc(0)).EQ.0) TOTcs = CSFus
C-----Locate position of the target among residues
      CALL WHERE(IZA(1) - IZAejc(0),nnurec,iloc)
C-----Locate position of the projectile among ejectiles
      CALL WHEREJC(IZAejc(0),nejcec,iloc)
C
      WRITE (ctmp23,'(i3.3,i3.3,1h_,i3.3,i3.3,1h_,i9.9)') INT(ZEJc(0)),
     &       INT(AEJc(0)), INT(Z(0)), INT(A(0)), INT(EINl*1000000)
C     TOTcs, ABScs, ELAcs are initialized within MARENG()
      xsinlcont = 0.d0
      xsinl = 0.d0
      checkXS = 0.d0
C     For resolution function (Spreading levels in the continuum)
      isigma0 = 0
C     RCN, To avoid gaussian spreading of the calculated strength
      IF(WIDcoll.GT.0.d0)
     &   isigma0 = INT((0.02d0  + sqrt(EINl)*WIDcoll)/DE + 1.0001)

      ncoll = 0
      ecm = EINl - EIN
      dang = PI/FLOAT(NANgela - 1)
      angstep = 180.d0/(NANgela-1)
      gang = 180.d0/(NDAng-1)
C-----
C-----Get ECIS results
C-----
      IF (ICAlangs.GT.0) THEN
        OPEN (45,FILE = (ctldir//ctmp23//'.EXP'),STATUS = 'OLD',
     &      ERR = 1400)
        READ (45,*,END = 1400)   ! To skip first line <ANG.DIS.> ..
        READ (45,*,END = 1400)   ! To skip level identifier line
        DO iang = 1, NANgela
         READ (45,'(24x,E12.5)',END = 1400) elada(iang)
        ENDDO
      ELSE
        OPEN (45,FILE = (ctldir//ctmp23//'.LEG'),STATUS = 'OLD',
     &      ERR = 1400)
        READ (45,*,END = 1400)   ! To skip first line <LEGENDRE> ..
        READ (45,'(5x,i5)',END = 1400) neles
        DO iang = 1, min(NDAng,neles)
           READ (45,'(10x,d20.10)',END = 1400) elleg(iang)
        ENDDO
        CLOSE(45)
        OPEN (45,FILE = (ctldir//ctmp23//'.ANG'),STATUS = 'OLD',
     &      ERR = 1400)
        READ (45,*,END = 1400)   ! To skip first line <ANG.DIS.> ..
        READ (45,*,END = 1400)   ! To skip level identifier line
        iang = 0
        DO iang1 = 1, NANgela
C----------To use only those values corresponding to EMPIRE grid for elastic XS
           READ (45,'(7x,E12.5)',END = 1400) ftmp
           if(mod(DBLE(iang1-1)*angstep+gang,gang).NE.0) cycle
           iang = iang +1
           elada(iang) = ftmp
        ENDDO
      ENDIF
      IF (DIRect.NE.0) THEN
         ggmr = 3.
         ggqr=85.*A(0)**(-2./3.)
         ggor=5.
         mintsp = mod(NINT(2*D_Xjlv(1)),2)
         OPEN (46,FILE = (ctldir//ctmp23//'.ICS'),STATUS = 'OLD',
     &         ERR = 1400)
         READ (46,*,END = 1400) ! To skip first line <INE.C.S.> ..
C--------Get and add inelastic cross sections (including double-differential)
         DO i = 2, ND_nlv
            ilv = ICOller(i)
            IF ( (ilv.LE.NLV(nnurec)) .and.
C           For odd nuclides, collective states in continuum have different spin than the ground state
     &         (mod(NINT(2*D_Xjlv(i)),2).eq.mintsp) )THEN
C------------Adding inelastic to discrete levels
             echannel = EX(NEX(1),1) - Q(nejcec,1) - ELV(ilv,nnurec)
C------------Avoid reading closed channels
             IF (echannel.GE.0.0001) THEN
               xcse = echannel/DE + 1.0001
               icsl = INT(xcse)
               icsh = icsl + 1
               READ (46,*,END = 1400) popread
               popread = popread*FCCred
               ncoll = i
               POPlv(ilv,nnurec) = POPlv(ilv,nnurec) + popread
               CSDirlev(ilv,nejcec) = CSDirlev(ilv,nejcec) + popread
               CSEmis(nejcec,1) = CSEmis(nejcec,1) + popread
C--------------Add direct transition to the spectrum
               popl = popread*(FLOAT(icsh) - xcse)/DE
               IF (icsl.EQ.1) popl = 2.0*popl
               poph = popread*(xcse - FLOAT(icsl))/DE
               CSE(icsl,nejcec,1) = CSE(icsl,nejcec,1) + popl
               CSE(icsh,nejcec,1) = CSE(icsh,nejcec,1) + poph
               IF (ICAlangs.GT.0) THEN
                IF (i.LE.ICAlangs) THEN
                  READ (45,*,END = 1400)     ! Skipping level identifier line
                  DO iang = 1, NANgela
                    READ (45,'(24x,E12.5)',END = 1400) ftmp
                  CSAlev(iang,ilv,nejcec) = CSAlev(iang,ilv,nejcec)+ftmp
                  ENDDO
                 ENDIF
                ELSE
                 READ (45,*,END = 1400)     ! Skipping level identifier line
                 iang = 0
                 DO iang1 = 1, NANgela
                   READ (45,'(7x,E12.5)',END = 1400) ftmp
C------------------To use only those values corresponding to EMPIRE grid for inelastic XS
                   if(mod(DBLE(iang1-1)*angstep+gang,gang).NE.0) cycle
                   iang = iang +1
                   CSAlev(iang,ilv,nejcec) = CSAlev(iang,ilv,nejcec)
     &                                     + ftmp
                 ENDDO
                ENDIF
C--------------Construct recoil spectra due to direct transitions
               IF (ENDf(nnurec).GT.0 .AND. RECoil.GT.0) THEN
                  dang = PI/FLOAT(NDANG - 1)
                  coef = 2*PI*dang
C-----------------Check whether integral over angles agrees with x-sec. read from ECIS
                  csum = 0.0
                  DO iang = 1, NDANG
                     csum = csum + CSAlev(iang,ilv,nejcec)*SANgler(iang)
     &                      *coef
                  ENDDO
C-----------------Correct 'coef' for eventual imprecision and include recoil DE
                  coef = coef*POPlv(ilv,nnurec)/csum/DERec
                  echannel = echannel*EJMass(0)/AMAss(1)
                  DO iang = 1, NDANG
                     erecoil = ecm + echannel + 2*SQRT(ecm*echannel)
     &                         *CANgler(iang)
                     irec = erecoil/DERec + 1.001
                     weight = (erecoil - (irec - 1)*DERec)/DERec
C--------------------Escape if we go beyond recoil spectrum dimension
                     IF (irec + 1.GT.NDEREC) GOTO 1350
                     csmsdl = CSAlev(iang,ilv,nejcec)*SANgler(iang)*coef
                     RECcse(irec,0,nnurec) = RECcse(irec,0,nnurec)
     &                  + csmsdl*(1.0 - weight)
                     RECcse(irec + 1,0,nnurec)
     &                  = RECcse(irec + 1,0,nnurec) + csmsdl*weight
                  ENDDO
               ENDIF
             ENDIF
           ELSE
C------------Adding inelastic to continuum  (D_Elv(ND_nlv) = elvr)
             echannel = EX(NEX(1),1) - Q(nejcec,1) - D_Elv(i)
             icsl = INT(echannel/DE + 1.0)
             ncon = min(
     &          NINT((EXCn-Q(nejcec,1)-ECUt(nnurec))/DE),NDEcse)
C            WRITE(6,*) 'nejcec, nnurec',IZAejc(nejcec), IZA(nnurec)
C            WRITE(6,*) 'Level in continuum',D_Elv(i)
C            WRITE(6,*) 'Its bin number',icsl
C            WRITE(6,*) 'E calc',EX(NEX(1),1)-Q(nejcec,1)-(icsl-1)*DE
C            WRITE(6,*) 'Last discr. level',ELV(NLV(nnurec),nnurec)
C            WRITE(6,*) 'Ecut',ECUt(nnurec)
C            WRITE(6,*) 'Ex',EX(NEX(1),1)-Q(nejcec,1)-(ncon-1)*DE
C            WRITE(6,*) 'Continuum starts at bin number',ncon
C------------Avoid reading closed channels
             IF (echannel.GE.0.0001 .and. icsl.gt.0 .and. nejcec.le.2)
     &         THEN
               READ (46,*,END = 1400) popread
C
C--------------This level is not counted as a discrete one
C--------------but it is embedded in the continuum
C
               CSMsd(nejcec) = CSMsd(nejcec) + popread
               xsinlcont = xsinlcont + popread
C--------------Spreading it using resolution function
C
C              Special treatment for Giant Multipole Resonances
C              Any level with D_Def(i,2)>0.05 treated as GR
C
C              Giant multipole resonances following TALYS
C
C              For each L multipolarity Energy Weighted Sum Rule (EWSR) applies:
C              SUM_i(E_i*beta_i)=57.5*A**(-5/3)*L*(L+1)
C
               isigma  = isigma0
               isigma2 = 2*isigma0*isigma0
               if(D_Def(i,2).GE.0.05) then
                 if(int(D_Xjlv(i)).eq.0) isigma  = nint(ggmr/DE+0.5)
                 if(int(D_Xjlv(i)).eq.2) isigma  = nint(ggqr/DE+0.5)
                 if(int(D_Xjlv(i)).eq.3) isigma  = nint(ggor/DE+0.5)
                 isigma2 = 2*isigma*isigma
               endif

               if(isigma.gt.0) then
                 dtmp = 0.d0
                 do ie = max(icsl - 3*isigma,1) ,
     &                   min(icsl + 3*isigma,ncon)
                   dtmp = dexp(-dble(ie-icsl)**2/isigma2) + dtmp
                 enddo
                 if(dtmp.gt.0.d0) then
                   do ie = max(icsl - 3*isigma,1) ,
     &                     min(icsl + 3*isigma,ncon)
                     CSEmsd(ie,nejcec) = CSEmsd(ie,nejcec) + 
     &                 popread/DE*dexp(-dble(ie-icsl)**2/isigma2)/dtmp
                   enddo
                 else
                   CSEmsd(icsl  ,nejcec) = CSEmsd(icsl,nejcec) 
     &              + popread/DE
                 endif
               else
                 CSEmsd(icsl  ,nejcec) = CSEmsd(icsl,nejcec) 
     &              + popread/DE
               endif

              IF (ICAlangs.EQ.0) THEN
                 READ (45,*,END = 1400)     ! Skipping level identifier line
                 iang = 0
                 DO iang1 = 1, NANgela
                    READ (45,'(7x,E12.5)',END = 1400) ftmp
C-------------------Use only those values that correspond to EMPIRE grid for inelastic XS
                    if(mod(DBLE(iang1-1)*angstep+gang,gang).NE.0) cycle
                    iang = iang + 1

                    if(isigma.gt.0 .and. dtmp.gt.0.d0) then
                      do ie = max(icsl - 3*isigma,1) ,
     &                        min(icsl + 3*isigma,ncon)
                        CSEa(ie,iang,nejcec,1) =  CSEa(ie,iang,nejcec,1)
     &                  + ftmp/DE * dexp(-dble(ie-icsl)**2/isigma2)/dtmp
                      enddo
                    else
                      CSEa(icsl,iang,nejcec,1) =
     &                    CSEa(icsl,iang,nejcec,1) + ftmp/DE
                    endif
                 ENDDO
               ENDIF
             ENDIF
C------------End of adding inelastic to continuum
           ENDIF
 1350    ENDDO
      ENDIF
 1400 CLOSE (45)
      IF (DIRect.NE.0) CLOSE (46)
C-----Print elastic and direct cross sections from ECIS
      WRITE (6,*) ' '
      WRITE (6,*) ' '
      IF (KTRlom(0,0).GT.0 .AND. FIRst_ein) THEN
        IF (DIRect.EQ.0) THEN
         WRITE (6,*)
     &       ' Results provided by Spherical Optical Model calculations'
         WRITE (6,*) ' '
        ELSEIF (DIRect.EQ.1 .OR. DIRect.EQ.2) THEN
         WRITE (6,*) ' Results provided by Coupled Channel calculations'
         WRITE (6,*) ' Inelastic scattering results provided by'
         WRITE (6,*) ' Coupled Channel + DWBA calculations'
         if(CSMsd(1)+CSMsd(2).NE.0.)
     &   WRITE (6,*) ' Some discrete levels are embedded into continuum'
         WRITE (6,*) ' '
        ELSEIF (DIRect.EQ.3) THEN
         WRITE (6,*)
     &       ' Results provided by Spherical Optical Model calculations'
         WRITE (6,*)
     &     ' Inelastic scattering results provided by DWBA calculations'
         if(xsinlcont.NE.0.)
     &   WRITE (6,*) ' Some discrete levels are embedded into continuum'
         WRITE (6,*) ' '
        ENDIF
      ENDIF

      ElasticCorr = 0.d0
      IF(TOTred.ne.0.d0 .or. FUSred.ne.0.d0)
     &  ElasticCorr = (TOTred - 1.d0)*TOTcs            
     &              + (1.d0 - FUSred)*CSFus/FUSred  
     &              + (1.d0 - FCCred)*(SINl + SINlcc)

      IF (KTRlom(0,0).GT.0) THEN
      IF (ZEJc(0).EQ.0 .AND. AEJc(0).GT.0) THEN
         WRITE (6,99005) TOTcs,TOTred*TOTcs, 
     &                   CSFus/FUSred,CSFus, 
     &                   SINlcc + SINl + SINlcont,
     &                   (SINlcc + SINl)*FCCred + SINlcont,
     &                   ELAcs, ElasticCorr + ELAcs
99005    FORMAT (/,2x,'Total cross section         :',e14.7,' mb',
     &                '  ( Scaled  ',e14.7,' mb )',/,2x,
     &           'Absorption cross section    :',e14.7,' mb',
     &                '  ( Scaled  ',e14.7,' mb )',/,2x,
     &           'Direct cross section        :',e14.7,' mb',
     &                '  ( Scaled  ',e14.7,' mb )',/,2x,
     &           'Shape elastic cross section :',e14.7,' mb',
     &                '  ( Shifted ',e14.7,' mb )')
         IF(ElasticCorr.NE.0.d0) write(6,'(2x, 
     &''** Elastic changed to compensate changes in total/absorption'',
     &//)')
      ENDIF
      IF (ZEJc(0).NE.0 .OR. AEJc(0).EQ.0) THEN
         WRITE (6,99010) CSFus + (SINlcc + SINl)*FCCred + SINlcont
99010    FORMAT (/,2x,'Absorption cross section    :',e14.7,' mb',//)
      ENDIF
      WRITE (6,99015)
      WRITE (6,99020)
      gang = 180.0/(NDAng - 1)
      angstep = 180.0/(NANgela - 1)
      DO iang = 1, NANgela/4 + 1
         imint = 4*(iang - 1) + 1
         imaxt = MIN0(4*iang,NANgela)
         WRITE (6,99025) ((j - 1)*angstep,elada(j),j = imint,imaxt)
      ENDDO
99015 FORMAT (' ',46x,'SHAPE ELASTIC DIFFERENTIAL CROSS-SECTION',/,' ',
     &        46x,40('*'),/,' ',56x,'CENTER-OF-MASS SYSTEM',///)
99020 FORMAT (' ',5x,4('    TETA ',2x,'D.SIGMA/D.OMEGA',6x),/)
99025 FORMAT (' ',5x,4(1p,e12.5,2x,e12.5,6x))
99029 FORMAT (' ',46x,'INELASTIC DIFFERENTIAL CROSS-SECTION',//,
     &              ' ',46x,'  (only discrete levels are listed)',/,' '
     &                 ,46x,36('*'),/,' ',56x,'CENTER-OF-MASS SYSTEM',
     &      ///)
99030 FORMAT ('  Angle ',10(6x,i2,'-level'))
99031 FORMAT ('        ',9(5x,'E=',f7.4))
99032 FORMAT ('        ',10(5x,'E=',f7.4))
99033 FORMAT ('        ',9(4x,f4.1,'/',f5.4))
99034 FORMAT ('        ',10(4x,f4.1,'/',f5.4))
99035 FORMAT (1x,f5.1,3x,11(2x,E12.6))
99040 FORMAT (6x,3x,11(2x,E12.6))
      WRITE (6,'(//)')
      IF (ncoll.GT.0) THEN
C--------Locate position of the projectile among ejectiles
         CALL WHEREJC(IZAejc(0),nejcec,iloc)
         WRITE (6,*) ' '
         gang = 180.d0/(NDANG - 1)
         its = 2
         DO ilv = 2,ncoll
            DO iang= ilv+1,ncoll
              if(ICOller(iang).eq.ICOller(ilv)) goto 700
            ENDDO
            itemp(its) = ICOller(ilv)
            deform(its)   = D_DEF(ilv,2)
            its = its +1
 700     ENDDO
         its = its -1
         IF (CSAlev(1,ICOller(2),nejcec).GT.0) THEN
           WRITE (6,99029)
           WRITE (6,99030) (itemp(ilv),ilv = 2,MIN(its,10))
           WRITE (6,99031) (ELV(itemp(ilv),nnurec),ilv = 2,MIN(its,10))
           WRITE (6,99033) (XJLv(itemp(ilv),nnurec)*
     &           LVP(itemp(ilv),nnurec),deform(ilv),ilv = 2,MIN(its,10))
           WRITE (6,*) ' '
           DO iang = 1, NDANG
             WRITE (6,99035) (iang - 1)*gang,
     &         (CSAlev(iang,itemp(ilv),nejcec),ilv = 2,MIN(its,10))
           ENDDO
           WRITE (6,*) ' '
           WRITE (6,99040)(POPlv(itemp(ilv),nnurec),ilv = 2,MIN(its,10))
           IF(its.gt.10) THEN
              WRITE (6,*) ' '
              WRITE (6,*) ' '
              WRITE (6,99030) (itemp(ilv),ilv = 11,MIN(its,20))
              WRITE (6,99032)(ELV(itemp(ilv),nnurec),ilv=11,MIN(its,20))
              WRITE (6,99034)(XJLv(itemp(ilv),nnurec)*
     &           LVP(itemp(ilv),nnurec),deform(ilv),ilv =11,MIN(its,20))
              WRITE (6,*) ' '
              DO iang = 1, NDANG
                WRITE (6,99035) (iang - 1)*gang,
     &           (CSAlev(iang,itemp(ilv),nejcec),ilv = 11,MIN(its,20))
              ENDDO
              WRITE (6,*) ' '
              WRITE (6,99040) (POPlv(itemp(ilv),nnurec),ilv = 11,
     &                      MIN(its,20))
           ENDIF
           IF(its.gt.20) THEN
              WRITE (6,*) ' '
              WRITE (6,*) ' '
              WRITE (6,99030) (itemp(ilv),ilv = 21,MIN(its,30))
              WRITE (6,99032)(ELV(itemp(ilv),nnurec),ilv=21,MIN(its,30))
              WRITE (6,99034)(XJLv(itemp(ilv),nnurec)*
     &           LVP(itemp(ilv),nnurec),deform(ilv),ilv =21,MIN(its,30))
              WRITE (6,*) ' '
              DO iang = 1, NDANG
                WRITE (6,99035) (iang - 1)*gang,
     &           (CSAlev(iang,itemp(ilv),nejcec),ilv = 21,MIN(its,30))
              ENDDO
              WRITE (6,*) ' '
              WRITE (6,99040) (POPlv(itemp(ilv),nnurec),ilv = 21,
     &                      MIN(its,30))
           ENDIF
C
C----------Because of the ENDF format restrictions the maximum
C----------number of discrete levels is limited to 40
C
           IF(its.gt.30) THEN
              WRITE (6,*) ' '
              WRITE (6,*) ' '
              WRITE (6,99030)(itemp(ilv),ilv = 31,MIN(its,40))
              WRITE (6,99032)(ELV(itemp(ilv),nnurec),ilv=31,MIN(its,40))
              WRITE (6,99034)(XJLv(itemp(ilv),nnurec)*
     &           LVP(itemp(ilv),nnurec),deform(ilv),ilv =31,MIN(its,40))
              WRITE (6,*) ' '
              DO iang = 1, NDANG
                WRITE (6,99035) (iang - 1)*gang,
     &           (CSAlev(iang,itemp(ilv),nejcec),ilv = 31,MIN(its,40))
              ENDDO
              WRITE (6,*) ' '
              WRITE (6,99040) (POPlv(itemp(ilv),nnurec),ilv = 31,
     &                      MIN(its,40))
           ENDIF

           WRITE (6,*) ' '
           WRITE (6,*) ' '
           WRITE (6,*) ' '
         ENDIF
      ENDIF
      ENDIF
C
C     Skipping all emission calculations
C     GOTO 99999
C
C-----Locate postions of ENDF MT-numbers 2, 91, 649, and 849
      CALL WHERE(IZA(1) - IZAejc(0),mt2,iloc)
      CALL WHERE(IZA(1) - IZAejc(1),mt91,iloc)
      CALL WHERE(IZA(1) - IZAejc(2),mt649,iloc)
      CALL WHERE(IZA(1) - IZAejc(3),mt849,iloc)
C-----Locate residual nuclei after CN decay
      NREs(0) = 1
      DO nejc = 1, NEJcm
         NREs(nejc) = -1
         ares = A(1) - AEJc(nejc)
         zres = Z(1) - ZEJc(nejc)
C        residual nuclei must be heavier than alpha
         if(ares.le.4. and. zres.le.2.) cycle
         izares = INT(1000.0*zres + ares)
         CALL WHERE(izares,nnur,iloc)
         NREs(nejc) = nnur
      ENDDO
C-----
C-----Calculate MSD contribution
C-----
      corrmsd = 1.0
      IF (MSD.NE.0 .AND. EIN.GE.EMInmsd) THEN
C
C--------call ORION
C
C--------This part prompts for the name of a data file. The INQUIRE
C--------statement then determines whether or not the file exists.
C--------If it does not, the program start new calculations
         WRITE (ctmp23,'(i3.3,i3.3,1h_,i3.3,i3.3,1h_,i9.9)')
     &       INT(ZEJc(0)), INT(AEJc(0)), INT(Z(0)),
     &       INT(A(0)), INT(EINl*1000000)
         INQUIRE (FILE = (ctldir//ctmp23//'.MSD'),EXIST = fexist)
         IF (.NOT.fexist) THEN
           OPEN (15,FILE = (ctldir//ctmp23//'.MSD'),STATUS='NEW')
         ELSE
           OPEN (15,FILE = (ctldir//ctmp23//'.MSD'),STATUS='OLD')
           WRITE (6,*) ' '
           WRITE (6,*)
     &       ' Using precalculated ORION results for E=',EINl,' MeV'
           WRITE (6,*) ' '
           GOTO 1450
         ENDIF
C        REWIND 15
         WRITE (6,*) ' '
         qmax = 0.99*EIN
         qstep = qmax/3.0
         ltrmax = 4
         IF (NLW.LE.10) ltrmax = 3
         IF (NLW.LE.8) ltrmax = 2
         IF (NLW.LE.6) ltrmax = 1
         WRITE(15,*) qmax,qstep,ltrmax
         q2 = qmax
         q3 = qmax
 1420    CALL ORION(q2,q3,1,EIN,NLW,1,ltrmax,A(0),Z(0),AEJc(0),
     &              ZEJc(0),IOUt,ANGles,NDANG,ICOmpff)
         WRITE (6,
     &'('' ORION calculated for Q2='', F7.3, '' and Q3='',F7.3)') q2, q3
         q2 = q2 - qstep
         IF (q2.LT.( - 0.0001D0)) THEN
            q3 = q3 - qstep
            IF (q3.LT.( - 0.0001D0)) GOTO 1450
            q2 = q3
         ENDIF
C--------Set to Q's to 0 if negative due to rounding error
         IF (q2.LT.0.0D0) q2 = 0.0
         IF (q3.LT.0.0D0) q3 = 0.0
         GOTO 1420
 1450    REWIND 15
         READ(15,*) qmax,qstep,ltrmax
         WRITE (6,*) ' '
         WRITE (6,*) ' '
         CALL ULM(1)
         CALL TRISTAN(0,0,ltrmax,qmax,qstep,xsinl)
         CLOSE(15)
      ENDIF
C-----PCROSS exciton model calculations of preequilibrium contribution
C-----including cluster emission by Iwamoto-Harada model and angular
C-----distributions according to Kalbach systematics
C-----
      totemis = 0.d0
      IF (EINl.GT.0.1D0 .AND. PEQc.GT.0) THEN
C        ftmp = CSFus - xsinl 
C        RCN, Jan. 2006, xsinl is replacing PCROSS neutron emission so it should not used for normalization
C        xsinl is calculated by MSD
         ftmp = CSFus 
         CALL PCROSS(ftmp,totemis)
      ENDIF          ! PCRoss done
      IF ((xsinl+totemis+SINl+SINlcc+SINlcont).gt.0. .AND. nejcec.gt.0
     &    .AND. NREs(nejcec).GE.0 ) THEN
C--------Print inelastic PE double differential cross sections
         nejc = nejcec
         nnur = NREs(nejc)
C        IF (CSMsd(nejc).GT.0.D0 .AND. IOUt.GE.3) THEN
         IF (IOUt.GE.3) THEN
            itimes = FLOAT(NDANG)/11.0 + 0.95
            DO its = 1, itimes
              iad = 1 + (its - 1)*11
              iam = 11 + (its - 1)*11
              iam = MIN0(NDANG,iam)
              IF(nejc.eq.1) WRITE (6,
     &                '(//30X,''     N  E  U  T  R  O  N  S ''/)')
              IF(nejc.eq.2) WRITE (6,
     &                '(//30X,''     P  R  O  T  O  N  S ''/)')
              IF(nejc.eq.3) WRITE (6,
     &                '(//30X,''     A  L  P  H  A  S ''/)')
              WRITE (6,
     &                '(30X,''A      n      g      l      e      s '')')
              WRITE (6,*) ' '
              WRITE (6,'('' Energy  '',11(4X,F5.1,2X))')
     &                (ANGles(ia),ia = iad,iam)
              WRITE (6,*) ' '
C-------------Maximum and minimum energy bin
              echannel = EX(NEX(1),1) - Q(nejc,1)
C-------------Last continuum energy bin is calculated (+ 1 added, 10, 2005)
C             DO i = 1, MAX(INT((echannel-ECUt(nnur))/DE + 1.0001),1)
              DO i = 1, MAX(INT((echannel-ECUt(nnur))/DE + 2.0001),1)
                WRITE (6,'(1X,F7.3,1X,11E11.4)') FLOAT(i - 1)*DE,
     &           (max(CSEa(i,iang,nejc,1),0.d0),iang = iad,iam)
              ENDDO
              WRITE (6,*) ' '
            ENDDO
         ENDIF
         if(xsinlcont.gt.0) then 
            write(6,*)
     &     ' DWBA to continuum XS for inelastic channel ',xsinlcont
            SINlcont =  xsinlcont
         else  
            SINlcont =  0.d0
         endif
C        if(xsinlcont.gt.0) write(6,*)
C    &   ' DWBA to continuum XS for inelastic channel (test) ',SINlcont
               WRITE (6,*)
         if(CSMsd(0).gt.0.) WRITE (6,*)
     &       ' g PE emission cross section ', CSMsd(0), ' mb'
         if(CSMsd(1).gt.0.) WRITE (6,*)
     &       ' n PE emission cross section ', CSMsd(1), ' mb'
         if(CSMsd(2).gt.0.) WRITE (6,*)
     &       ' p PE emission cross section ', CSMsd(2), ' mb'
         if(CSMsd(3).gt.0.) WRITE (6,*)
     &       ' a PE emission cross section ', CSMsd(3), ' mb'
         if(NEMc.GT.0 .AND. CSMsd(NDEjc).gt.0.) WRITE (6,*)
     &   ' Cluster PE emission cross section ', CSMsd(NDEjc), ' mb'
         WRITE (6,*) ' '
C--------Correct CN population for PE continuum emission
         corrmsd = (CSFus - (xsinl + totemis))/CSFus
         IF (corrmsd.LT.0.0D0) THEN
            WRITE (6,*) ' '
            WRITE (6,*) 'PE EMISSION LARGER THEN FUSION CROSS SECTION'
            IF (MSD+MSC.GT.0 .AND. ICOmpff.GT.0) THEN
               WRITE (6,*) 'TRY TO TURN OFF COMPRESSIONAL FORM FACTOR '
               WRITE (6,*) 'SETTING COMPFF TO 0 IN THE OPTIONAL INPUT.'
               STOP 'PE EMISSION LARGER THEN FUSION CROSS SECTION'
            ENDIF
            IF (MSD+MSC.GT.0 .AND. ICOmpff.EQ.0) THEN
               WRITE (6,*) 'THIS MAY HAPPEN IF RESPONSE FUNCTIONS ARE '
               WRITE (6,*) 'RENORMALIZED IN INPUT OR FITTED TO WRONG '
               WRITE (6,*) 'DISCRETE LEVELS. CHECK `EFIT` AND `RESNOR` '
               WRITE (6,*) 'IN OPTIONAL INPUT.    '
               WRITE (6,*)
     &                    'IF THESE ARE FINE TRY ANOTHER OPTICAL MODEL.'
               STOP 'PE EMISSION LARGER THEN FUSION CROSS SECTION'
            ENDIF
            IF (MSD+MSC.EQ.0) THEN
               WRITE (6,*) 'THIS MAY HAPPEN IF TOO MANY DISCRETE LEVELS'
               WRITE (6,*) 'ARE EMBEDDED INTO CONTINUUM OR HAVE TOO BIG'
               WRITE (6,*) 'DYNAMICAL DEFORMATIONS SPECIFIED IN THE    '
               WRITE (6,*) 'COLLECTIVE LEVEL FILE.'
               WRITE (6,*)
     &                  'TRY TO REDUCE THE NUMBER OF COLLECTIVE LEVELS.'
               STOP 'PE EMISSION LARGER THEN FUSION CROSS SECTION'
            ENDIF
         ENDIF
         DO i = 1, NLW
            POP(NEX(1),i,1,1) = POP(NEX(1),i,1,1)*corrmsd
            POP(NEX(1),i,2,1) = POP(NEX(1),i,2,1)*corrmsd
         ENDDO
         WRITE (6,*) ' '
         WRITE (6,*) ' PE + Direct reduction factor   ',1.d0-corrmsd
         WRITE (6,*) ' MSD contribution               ',xsinl/CSFus
         WRITE (6,*) ' PCROSS contribution            ',totemis/CSFus
         WRITE (6,*) ' '
C--------TRISTAN *** done ***
C--------Add MSD contribution to the residual nucleus population
C--------Locate residual nucleus after MSD emission
         DO nejc = 0, NDEjc
           nnur = NREs(nejc)
           IF(nnur.LT.0) CYCLE
           IF (CSMsd(nejc).NE.0.0D0) CALL ACCUMSD(1,nnur,nejc)
C----------Add PE contribution to energy spectra (angle int.)
           DO ie = 1, NDEcse
              CSE(ie,nejc,1) = CSE(ie,nejc,1) + CSEmsd(ie,nejc)
           ENDDO
C----------Add PE contribution to the total NEJC emission
           CSEmis(nejc,1) = CSEmis(nejc,1) + CSMsd(nejc)
         ENDDO
C        Skipping all emitted but neutrons and protons
C        Secondary emission was not tested for proton induced reactions
         nnur = NREs(nejcec)
C        IF(nnur.GE.0) THEN
         IF(nnur.GE.2000) THEN
C----------Second chance preequilibrium emission after MSD emission
C----------Neutron emission
           izares = INT(1000.0*Z(nnur) + A(nnur) - 1)
           CALL WHERE(izares,nnurn,iloc)
           IF (iloc.EQ.0) CALL SCNDPREEQ(nnur,nnurn,1,0)
           IF (iloc.EQ.0 .AND. IOUt.GT.3) CALL AUERST(nnur,1)
C----------Proton emission
           izares = izares - 1000
           CALL WHERE(izares,nnurp,iloc)
           IF (iloc.EQ.0) THEN
             CALL SCNDPREEQ(nnur,nnurp,2,1)
             IF (IOUt.GT.3) CALL AUERST(nnur,2)
           ELSE
             CALL SCNDPREEQ(nnur,nnurp,2,2)
           ENDIF
C----------Second chance preequilibrium *** done ***
         ENDIF
      ENDIF
C-----
C-----PE + DWBA cont. *** done ***
C-----
      ia = INT(A(1))
      IF (IOUt.GT.1) THEN
         WRITE (6,*) ' '
         WRITE (6,*) ' '
         WRITE (6,
     &'(''  Compound nucleus '',I3,''-'',A2,
     &  '' spin distribution'')') ia, SYMb(1)
         WRITE (6,*) ' -----------------------------------------'
         WRITE (6,*) ' '
         DO i = 1, NLW
            IF (MOD(ia,2).EQ.0) THEN
               WRITE (6,'(1X,I5,G12.5,5X,I5,G12.5)') i - 1,
     &                POP(NEX(1),i,1,1), ( - (i - 1)), POP(NEX(1),i,2,1)
            ELSE
               WRITE (6,'(1X,I4,''/2'',G12.5,5X,I4,''/2'',G12.5)')
     &                2*i - 1, POP(NEX(1),i,1,1), ( - (2*i - 1)),
     &                POP(NEX(1),i,2,1)
            ENDIF
         ENDDO
         WRITE (6,*) ' '
      ENDIF
      IF (IOUt.GT.0) THEN
         IF (DIRect.EQ.0) THEN
            WRITE (6,
     &'(''   Fusion cross section = '',G13.6,
     &  '' mb including'')') CSFus
            WRITE (6,'(''   PE (not DWBA) = '',
     &  G13.6,'' mb'')') xsinl + totemis 
         ELSEIF (DIRect.EQ.1 .OR. DIRect.EQ.2) THEN
            WRITE (6,
     &'(''   Fusion cross section = '',G13.6,
     &  '' mb including'')') CSFus + (SINl + SINlcc)*FCCred + SINlcont
            WRITE (6,
     &'(''   DWBA inelastic to uncoupled discrete levels = '',
     &  G13.6,'' mb'')') SINl*FCCred
            WRITE (6,
     &'(''   CC inelastic to coupled discrete levels = '',
     &  G13.6,'' mb'')') SINlcc*FCCred
            WRITE (6,'(''   DWBA to continuum = '',
     &  G13.6,'' mb'')') xsinlcont
            WRITE (6,'(''   PE (not DWBA) = '',
     &  G13.6,'' mb'')') xsinl + totemis
            WRITE (6,
     &'(''   Spin distribution calculated using '',
     &  ''CC transmission coefficients'')')
         ELSEIF (DIRect.EQ.3) THEN
            WRITE (6,
     &'(''   Fusion cross section = '',G13.6,
     &  '' mb including'')') CSFus + (SINl + SINlcc)*FCCred + SINlcont
            WRITE (6,
     &'(''   DWBA inelastic to discrete levels = '',
     &  G13.6,'' mb'')') (SINl  + SINlcc)*FCCred
            WRITE (6,'(''   DWBA to continuum = '',
     &  G13.6,'' mb'')') xsinlcont
            WRITE (6,'(''   PE (not DWBA)  = '',
     &  G13.6,'' mb'')') xsinl + totemis
            WRITE (6,
     &'(''   Spin distribution does NOT contain'',
     &  '' DWBA inelastic contribution '')')
         ENDIF
      ENDIF
      IF (ENDf(1).EQ.0.0D0) THEN
C        WRITE (12,'('' FUSION CROSS SECTION = '',G12.5,'' mb'')') CSFus
         WRITE (12,'('' FUSION CROSS SECTION = '',G13.6, '' mb'')')
     &          CSFus + (SINl + SINlcc)*FCCred + SINlcont
      ELSE
         WRITE (12,*) ' '
         WRITE (12,'('' FUSION CROSS SECTION = '',G12.5,'' mb'')')
     &          CSFus + (SINl + SINlcc)*FCCred + SINlcont
         WRITE (12,'('' TOTAL  CROSS SECTION = '',G13.6,'' mb'')')
     &         TOTcs*TOTred
         WRITE (12,*) ' '
      ENDIF
      POPmax(1) = CSFus*1.0E-25
C-----Renormalization of CN spin distribution if TURBO mode invoked
      IF (LTUrbo.NE.1) THEN
         IF (IOUt.GT.0) WRITE (6,
     &         '('' TURBO mode with LTURBO='',I1,'' has been invoked'')'
     &         ) LTUrbo
         cturbo = 0.0
         DO i = 1, NLW, LTUrbo
            cturbo = cturbo + POP(NEX(1),i,1,1) + POP(NEX(1),i,2,1)
         ENDDO
         cturbo = CSFus/cturbo
         DO i = 1, NLW, LTUrbo
            POP(NEX(1),i,1,1) = POP(NEX(1),i,1,1)*cturbo
            POP(NEX(1),i,2,1) = POP(NEX(1),i,2,1)*cturbo
         ENDDO
      ENDIF
       nubart=0
      OPEN (80,FILE = 'FISSION.OUT',STATUS = 'UNKNOWN')
C-----Start DO loop over decaying nuclei
      DO nnuc = 1, NNUcd
         IF(QPRod(nnuc).LT.-999.d0) CYCLE
         DO kk = 0, NFISENMAX
            DO jj = 1, NDLW
               DO i = 1, NFHUMP
                  ROFis(kk,jj,i) = 0.
               ENDDO
            ENDDO
         ENDDO
         IF (IOUt.GT.0) THEN
           WRITE (*,1234) nnuc,  NNUcd, INT(Z(nnuc)),
     &                  SYMb(nnuc), INT(A(nnuc))
1234       FORMAT(1x, '  Decaying nucleus # ',I3,' of ',I3,
     &   ' (',I3,'-',A2,'-',I3,')' )
         ENDIF
         IF (FISsil(nnuc) .AND. FISshi(nnuc).NE.1.) THEN
            CALL READ_INPFIS(nnuc)
            nrbarc1 = NRBarc
            IF (NRBarc.EQ.3) nrbarc1 = 2
            DO i = 1, nrbarc1
               IF (FISmod(nnuc).EQ.0. .OR.
     &             (FISmod(nnuc).GT.0. .AND. i.NE.2))
     &             CALL DAMI_ROFIS(nnuc,i,0,AFIs(i))
               IF (FISmod(nnuc).GT.0. .AND. i.EQ.2) THEN
                  DO m = 1, INT(FISmod(nnuc)) + 1
                     CALL DAMI_ROFIS(nnuc,i,m,AFIsm(m))
                  ENDDO
               ENDIF
            ENDDO
            IF (NRBar.EQ.3 .AND. NRWel.EQ.1 .AND. FISmod(nnuc).EQ.0.)
     &          THEN
               TFIso = 2.86896*EXP(2.*PI*(EFB(2) - (EFB(3)+H(1,3)/2.))
     &                 /H(1,2))/(H(1,3)*10.**21)
               TGIso = EXP(2.*PI*(EFB(1) - (EFB(3)+H(1,3)/2.))/H(1,1))
     &                 /10.**14
            ENDIF
            IF (NRBar.EQ.5 .AND. NRWel.EQ.2 .AND. FISmod(nnuc).EQ.0.)
     &          THEN
               TFIso = 2.86896*EXP(2.*PI*(VEQ - (EFB(4)+H(1,4)/2.))
     &                 /HOEq)/(H(1,4)*(10.**21))
               TGIso = EXP(2.*PI*(EFB(1) - (EFB(4)+H(1,4)/2.))/H(1,1))
     &                 /10.**14
            ENDIF
            IF (FISmod(nnuc).EQ.0. .AND. NRBar.GE.3) THEN
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
         CSFis = 0.d0
         IF (FISmod(nnuc).GT.0.) THEN
            DO m = 1, INT(FISmod(nnuc)) + 1
               CSFism(m) = 0.d0
            ENDDO
         ENDIF
         sumfis = 0.0
         IF (IOUt.GT.0) THEN
            WRITE (6,*) ' '
            WRITE (6,*) ' '
            WRITE (6,*) ' -------------------------------------'
            WRITE (6,'(I3,2X,''Decaying nucleus '',I3,''-'',A2)') nnuc,
     &             ia, SYMb(nnuc)
            WRITE (6,*) ' -------------------------------------'
            WRITE (6,*) ' '
         ENDIF
         IF (ENDf(nnuc).NE.0.0D0 .OR. FITomp.LT.0) THEN
            WRITE (12,*) ' '
            WRITE (12,*)
     &' ---------------------------------------------------------------'
            IF(abs(QPRod(nnuc) + ELV(LEVtarg,0)).gt.99.99) THEN
               WRITE (12,
     &'(''  Decaying nucleus '',I3,''-'',A2,''-'',I3,     ''  mass='',F1
     &0.6,'' Q-value='',F10.5)') INT(Z(nnuc)), SYMb(nnuc), ia,
     &         AMAss(nnuc), QPRod(nnuc) + ELV(LEVtarg,0)
            ELSE
               WRITE (12,
     &'(''  Decaying nucleus '',I3,''-'',A2,''-'',I3,     ''  mass='',F1
     &0.6,'' Q-value='',F10.6)') INT(Z(nnuc)), SYMb(nnuc), ia,
     &         AMAss(nnuc), QPRod(nnuc) + ELV(LEVtarg,0)
          ENDIF
            WRITE (12,*)
     &' ---------------------------------------------------------------'
            IF (nnuc.NE.1) THEN
               IF (nnuc.EQ.mt91) THEN
                 nejc = 1
               ELSEIF (nnuc.EQ.mt649) THEN
                  nejc = 2
               ELSEIF (nnuc.EQ.mt849) THEN
                  nejc = 3
               ELSEIF (nnuc.EQ.1) THEN
                  nejc = 0
               ELSE
                  GOTO 1460
               ENDIF
               dtmp = 0.d0
               DO il = 1, NLV(nnuc)
                 dtmp = dtmp + CSDirlev(il,nejc)
               ENDDO
               IF(dtmp.LE.0.0) GOTO 1460
               WRITE (12,
     &'(1X,/,10X,''Discrete level population '',      ''before gamma cas
     &cade'')')
               WRITE (12,'(1X,/,10X,40(1H-),/)')
               DO il = 1, NLV(nnuc)
C-----------------Check for the number of branching ratios
                  nbr = 0
                  DO ib = 1, NDBR
                     IF (BR(il,ib,2,nnuc).EQ.0.) GOTO 1455
                     nbr = ib
                  ENDDO
 1455             IF (nbr.EQ.0 .AND. il.NE.1 .AND. FIRst_ein .AND.
     &                (nnuc.EQ.mt91 .OR. nnuc.EQ.mt649 .OR.
     &                nnuc.EQ.mt849)) WRITE (6,*)
     &                 ' WARNING: Branching ratios for level ', il,
     &                ' IN ', INT(A(nnuc)), '-', SYMb(nnuc),
     &                ' are missing'
C    &                            XJLv(il, nnuc), POPlv(il, nnuc), nbr,
                  WRITE (12,99070) il, ELV(il,nnuc), LVP(il,nnuc),
     &                             XJLv(il,nnuc), CSDirlev(il,nejc),
     &                             nbr,
     &                             (NINT(BR(il,ib,1,nnuc)),BR(il,ib,2,
     &                             nnuc),ib = 1,nbr)
C-----------------Next IF moves levels population to the ground state
C-----------------to avoid gamma cascade between discrete levels
C-----------------originating from the direct population
C-----------------of discrete levels by a neutron, proton or alpha.
C-----------------These gammas should not go into MT=91, 649, or 849.
                  IF ((nnuc.EQ.mt91 .OR. nnuc.EQ.mt649 .OR. nnuc.EQ.
     &                mt849) .AND. il.NE.1) THEN
                     POPlv(1,nnuc) = POPlv(1,nnuc) + CSDirlev(il,nejc)
                     POPlv(il,nnuc) = POPlv(il,nnuc) - CSDirlev(il,nejc)
                  ENDIF
               ENDDO
C--------------Decay direct population of discrete levels by a neutron,
C--------------proton or alpha without storing emitted gammas in the spectra.
               IF ((nnuc.EQ.mt91 .OR. nnuc.EQ.mt649 .OR. nnuc.EQ.
     &              mt849) .AND. il.NE.1) THEN
                  CALL DECAYD_DIR(nnuc, nejc)
               ENDIF
C--------------Write elastic to tape 12
 1460          IF (nnuc.EQ.mt2) THEN
                  WRITE (12,'(1X,/,10X,40(1H-),/)')
                  WRITE (12,*) ' '
                  WRITE (12,
     &                   '('' ELASTIC CROSS SECTION ='',G12.5,'' mb'')')
     &                   ELAcs + ElasticCorr + 4.*PI*ELCncs
                  WRITE (12,*) ' '
                  WRITE (12,*) ' Elastic angular distribution '
                  WRITE (12,*) ' '
                  IF (ICAlangs.GT.0) THEN
                    WRITE (12,99045) (ANGles(iang),iang = 1, NANgela)
                  ELSE
                    delang = 180./FLOAT(NANgela - 1)
                    WRITE (12,99045) (FLOAT(iang - 1)*delang,iang = 1,
     &                                                         NANgela)
                  ENDIF
99045             FORMAT (10X,8G15.5)
                  if(ELAcs.GT.0.D0) then
                     WRITE (12,99050) 
     &               ((1.d0 + ElasticCorr/ELAcs)*elada(iang) + ELCncs,
     &               iang = 1,NANgela)
                  else 
                     WRITE (12,99050) 
     &               (elada(iang) + ELCncs,
     &               iang = 1,NANgela)
                  endif
99050             FORMAT (9X,8E15.5)
                  WRITE (12,*) ' '
                  WRITE (12,*) ' '
                  WRITE (12,*) ' Legendre coefficients expansion '
                  WRITE (12,*) ' '
                  WRITE (12,'(1x,A7,I5)') ' Lmax =',min(NDAng,neles)
                  WRITE (12,*) ' '
                  if(ELAcs.GT.0.D0) THEN
                     WRITE (12,'(9X,8D15.8)') 
     &               ((1.d0 + ElasticCorr/ELAcs)*elleg(1) + ELCncs),
     &               ((1.d0 + ElasticCorr/ELAcs)*elleg(iang),
     &               iang = 2,min(NDAng,neles))
                  else
                     WRITE (12,'(9X,8D15.8)') 
     &               (elleg(1) + ELCncs),
     &               (elleg(iang),iang = 2,min(NDAng,neles))
                  endif
                  WRITE (12,*) ' '
                  IF (elcncs.EQ.0) WRITE (6,*)
     &                 'WARNING: CN elastic is 0'
                  IF (FITomp.LT.0) THEN
                   WRITE(40,'(F12.4,3D12.5)') 
     &                    EINl,TOTcs,ABScs
                   IF (ncoll.GT.0) THEN
C-------------------locate position of the projectile among ejectiles
                    CALL WHEREJC(IZAejc(0),nejcec,iloc)
                    its = 2
                    DO ilv = 2,ncoll
                     DO iang= ilv+1,ncoll
                      if(ICOller(iang).eq.ICOller(ilv)) goto 710
                     ENDDO
                     itemp(its) = ICOller(ilv)
                     its = its +1
 710                ENDDO
                    its = its -1
                    WRITE (40,'(12x,11D12.5)') ELAcs,
     &               (CSDirlev(itemp(ilv),nejcec),ilv = 2,MIN(its,10))
                    IF (ICAlangs.gt.0) THEN
                     DO iang = 1, NDANG
                      WRITE (40,'(f12.4,11D12.5)') ANGles(iang),
     &                 elada(iang) + elcncs,
     &              (CSAlev(iang,itemp(ilv),nejcec),ilv = 2,MIN(its,10))
                     ENDDO
                    ENDIF
                   ELSE
                    WRITE (40,'(12x,11D12.5)') ELAcs
                    IF (ICAlangs.gt.0) THEN
                     DO iang = 1, NDANG
                      WRITE (40,'(f12.4,11D12.5)') ANGles(iang),
     &                                             elada(iang) + elcncs
                     ENDDO
                    ENDIF
                   ENDIF
                  ENDIF
              ENDIF
            ENDIF
         ENDIF
C--------Jump to end of loop after elastic when fitting
         If(FITomp.LT.0 .AND. nnuc.EQ.mt2) go to 1155
         POPmax(nnuc) = POPmax(nnuc)*0.0001
         IF (POPmax(nnuc).EQ.0.0D0) THEN
            WRITE (6,*) ' '
            WRITE (6,*)
     &                'Continuum of this nucleus has not been populated'
            GOTO 1500
         ENDIF
C--------Prepare gamma transition parameters
         CALL ULM(nnuc)
C--------Calculate compound nucleus level density at saddle point
         IF (FISshi(nnuc).EQ.1.) THEN
            IF (FISsil(nnuc)) THEN
               IF (ADIv.EQ.0.0D0) CALL ROEMP(nnuc,1.D0,0.0D0)
               IF (ADIv.EQ.1.0D0) CALL ROCOL(nnuc,1.D0,2.D0)
               IF (ADIv.GT.3.0D0) CALL ROCOL(nnuc,1.D0,1.D0)
               IF (ADIv.EQ.2.0D0) WRITE (6,*)
     &  ' MUST NOT USE GILBERT-CAMERON LEVEL DENSITIES FOR SADDLE POINT'
               IF (IOUt.EQ.6) THEN
                  WRITE (6,'(1X,/,'' Saddle point level density'',/)')
                  WRITE (6,99055) (EX(i,nnuc),(ROF(i,j,nnuc),j = 1,12),
     &                            i = 1,NEX(nnuc))
Cpr            WRITE(6,20) (EX(i,nnuc),(ROF(i,j,nnuc),j=13,24),i=1,nex(nnuc))
Cpr            WRITE(6,20) (EX(i,nnuc),(ROF(i,j,nnuc),j=25,36),i=1,nex(nnuc))
Cpr            WRITE(6,20) (EX(i,nnuc),(ROF(i,j,nnuc),j=37,48),i=1,nex(nnuc))
Cpr            WRITE(6,20) (EX(i,nnuc),(ROF(i,j,nnuc),j=49,60),i=1,nex(nnuc))
99055             FORMAT (1X,13G10.4)
               ENDIF
            ENDIF
         ENDIF
C-------
C------- DEGAS exciton model calculations of preequilibrium contribution
C-------
         IF (nnuc.EQ.1 .AND. EIN.GT.5.D0 .AND. DEGa.GT.0) THEN
            CALL EMPIREDEGAS
            WRITE (6,*) ' '
            WRITE (6,*) ' Start of summary from DEGAS'
            WRITE (6,*) ' ---------------------------'
            IF (GDIv.GT.1.0) WRITE (6,*) ' g = A/gdiv, gdiv =', GDIv
            WRITE (6,*) ' '
            WRITE (6,'('' DEGAS gamma emission (CN) ='',G12.5,''mb'')')
     &             CSEmis(0,1)
            WRITE (6,'('' DEGAS neut. emission (CN) ='',G12.5,''mb'')')
     &             CSEmis(1,1)
            WRITE (6,'('' DEGAS prot. emission (CN) ='',G12.5,''mb'')')
     &             CSEmis(2,1)
            WRITE (6,*) ' '
         ENDIF          ! Degas done
C--------
C--------HMS Monte Carlo preequilibrium emission
C--------
         IF (nnuc.EQ.1 .AND. EINl.GT.0.1D0 .AND. LHMs.NE.0) THEN
            CLOSE (8)
            xizat = IZA(0)
            xnhms = NHMs
            debinhms = DE
            IF (debinhms.LT.1.0D0) debinhms = 1.0
            CALL DDHMS(IZAejc(0),xizat,XJLv(LEVtarg,0),EINl,
     &                 CSFus*corrmsd,CHMs,debinhms,xnhms,0,1,0,QDFrac,
     &                 icalled)
            icalled = 1
            CSEmis(1,1) = CSEmis(1,1) + CSHms(1)
            CSEmis(2,1) = CSEmis(2,1) + CSHms(2)
            WRITE (6,
     &        '('' HMS inclusive neut. emission ='',G12.5,
     &          ''mb'')') CSHms(1)
            WRITE (6,
     &        '('' HMS inclusive prot. emission ='',G12.5,
     &          ''mb'')') CSHms(2)
            IF (ENDf(1).EQ.1 .AND. FIRst_ein) THEN
               WRITE (6,*) ' '
               WRITE (6,*)
     &                'WARNING: HMS Inclusive total emissions treated  '
               WRITE (6,*)
     &                'WARNING: as comming from the first CN. Allows   '
               WRITE (6,*)
     &                'WARNING: to check flux balance as long as       '
               WRITE (6,*)
     &                'WARNING: multiple P.E. can be neglected. At     '
               WRITE (6,*)
     &                'WARNING: higher energies this does not hold and '
               WRITE (6,*)
     &                'WARNING: balance will get wrong.  This is OK    '
               WRITE (6,*)
     &                'WARNING: since inclusive spectra are fine and,  '
               WRITE (6,*)
     &                'WARNING: in any case there are no approximations'
               WRITE (6,*)
     &                'WARNING: for production cross sections and      '
               WRITE (6,*)
     &                'WARNING: recoils!                               '
               WRITE (6,*) ' '
               CLOSE (8)
            ENDIF
         ENDIF
C--------
C--------Heidelberg Multistep Compound calculations
C--------
         IF (nnuc.EQ.1 .AND. MSC.NE.0) THEN
            CALL HMSC(nvwful)
            CSEmis(0,1) = CSEmis(0,1) + CSMsc(0)
            CSEmis(1,1) = CSEmis(1,1) + CSMsc(1)
            CSEmis(2,1) = CSEmis(2,1) + CSMsc(2)
            IF (nvwful) GOTO 1500
         ENDIF
         IF (nnuc.EQ.1 .AND. IOUt.GE.3 .AND.
     &     (CSEmis(0,1) + CSEmis(1,1) + CSEmis(2,1) + CSEmis(3,1))
     &       .NE.0) THEN
            WRITE (6,*) ' '
            WRITE (6,*) 
     &        ' Preequilibrium + Direct spectra (sum of all models):'
            IF(CSEmis(0,1).GT.0) CALL AUERST(1,0)
            IF(CSEmis(1,1).GT.0) CALL AUERST(1,1)
            IF(CSEmis(2,1).GT.0) CALL AUERST(1,2)
            IF(CSEmis(3,1).GT.0) CALL AUERST(1,3)
            WRITE (6,*) ' '
            IF (LHMs.NE.0 .AND. ENDf(1).NE.1) THEN
               WRITE (6,*) ' HMS spectra stored as inclusive:'
               CALL AUERST(0,1)
               CALL AUERST(0,2)
               WRITE (6,*) ' '
            ENDIF
         ENDIF
C--------
C--------Start Hauser-Feshbach nnuc nucleus decay
C--------
         popleft = 0.d0
C--------Ensure that full gamma cascade in the first CN is
C--------accounted for in the case of ENDF calculations
         IF (ENDf(1).GT.0.0D0) GCAsc = 1.0
C--------Turn on (KEMIN=1) or off (KEMIN=NEX(NNUC)) gamma cascade
C--------in the first CN
         kemin = 1
         IF (nnuc.EQ.1) THEN
            IF (GCAsc.EQ.0.0D0) kemin = NEX(nnuc)
            IF (GCAsc.EQ. - 1.0D0 .AND. EXCn.GT.20.0D0)
     &          kemin = NEX(nnuc)
         ENDIF
C--------Turn  off (KEMIN=NEX(NNUC)) gamma cascade in the case of OMP fit
C        IF (FITomp.NE.0) kemin = NEX(nnuc)
         kemax = NEX(nnuc)
C--------Account for widths fluctuations (HRTW)
         IF (LHRtw.EQ.1 .AND. EIN.GT.EHRtw) LHRtw = 0
         IF (nnuc.EQ.1 .AND. LHRtw.GT.0) THEN
            CALL HRTW
            IF (ENDf(1).GT.0 .AND. RECoil.GT.0) 
     &        CALL GET_RECOIL(kemax,nnuc) !recoil spectrum
            kemax = max(NEX(nnuc) - 1,1)
            GCAsc = 1.0
         ENDIF
C--------DO loop over c.n. excitation energy
         DO ke = kemax, kemin, -1
            IF(ke.le.0) cycle
            step = DE
            IF (ke.EQ.NEX(nnuc) .OR. ke.EQ.1) step = 0.5*DE
            IF (ke.EQ.NEX(nnuc) .AND. nnuc.EQ.1) step = 1.0
            IF (ENDf(1).GT.0) THEN
C--------------Clean auxiliary particle spectra for calculation of recoils
               DO nejc = 0, NEJcm
                  DO il = 1, NDLV
                     REClev(il,nejc) = 0.d0
                  ENDDO
                  DO ie = 1, NDECSE
                     AUSpec(ie,nejc) = 0.d0
                  ENDDO
               ENDDO
C--------------Calculate population in the energy bin ke
               pope = 0.d0
               DO jcn = 1, NLW, LTUrbo
                  pope = pope + POP(ke,jcn,1,nnuc) + POP(ke,jcn,2,nnuc)
               ENDDO
               POPbin(ke,nnuc) = pope*step
            ENDIF
            DO ipar = 1, 2 !over decaying nucleus parity
               ip = INT(( - 1.0)**(ipar + 1))
               DO jcn = 1, NLW, LTUrbo !over decaying nucleus spin
                  IF (GDRdyn.EQ.1.0D0) CALL ULMDYN(nnuc,jcn,EX(ke,nnuc))
                  DENhf = 0.0
                  IF (POP(ke,jcn,ipar,nnuc).LT.POPmax(nnuc)) THEN
                     popleft = popleft + POP(ke,jcn,ipar,nnuc)*DE
                     GOTO 1470
                  ENDIF
                  DO nejc = 1, NEJcm !over ejectiles
                     ares = A(nnuc) - AEJc(nejc)
                     zres = Z(nnuc) - ZEJc(nejc)
C--------------------Residual nuclei must be heavier than alpha
                     if(ares.le.4. and. zres.le.2.) cycle
                     izares = INT(1000.0*zres + ares)
                     CALL WHERE(izares,nnur,iloc)
                     if(iloc.eq.1) CYCLE
                     CALL DECAY(nnuc,ke,jcn,ip,nnur,nejc,sum)
                  ENDDO
C-----------------DO loop over ejectiles       ***done***
C-----------------gamma emision
                  IF (LTUrbo.EQ.1) THEN
                     CALL DECAYG(nnuc,ke,jcn,ip,sum)
                  ELSE
                     CALL DECAYT(nnuc,ke,jcn,ip,sum)
                  ENDIF
C-----------------Distribute yrast population over discrete levels
                  IF (DENhf.EQ.0.0D0) THEN
                     IF (ke.EQ.1) THEN
                        ded = DE*0.5
                     ELSE
                        ded = DE
                     ENDIF
                     IF (IOUt.GT.1) WRITE (6,
     & '('' Yrast state at bin'',I4,'' spin='',F5.1,'' pop='',   G12.5)'
     & ) ke, FLOAT(ip)*(FLOAT(jcn) + HIS(nnur)), POP(ke,jcn,ipar,nnuc)
     &   *ded
C--------------------Look for the discrete level with the closest spin
                     xnl = 1.0
                     spdiff = 100.
                     DO il = 1, NLV(nnuc)
                        spdif = ABS(FLOAT(jcn) + HIS(nnur)
     &                          - XJLv(il,nnuc))
                        IF (spdif.LT.spdiff) THEN
                           spdiff = spdif
                           xnl = 1.
                        ELSE
                           IF (spdif.EQ.spdiff) xnl = xnl + 1.
                        ENDIF
                     ENDDO
                     DO il = 1, NLV(nnuc)
                        spdif = ABS(FLOAT(jcn) + HIS(nnur)
     &                          - XJLv(il,nnuc))
                        IF (spdif.EQ.spdiff) THEN
                           SCRtl(il,0) = 1.0D0/xnl
                           DENhf = DENhf + SCRtl(il,0)
                           IF (IOUt.GT.1) WRITE (6,
     &'(10X,I3,''% of this was assumed to populate level #'',
     &I3)') INT(100./xnl), il
                        ENDIF
                     ENDDO
                  ENDIF
C-----------------
C-----------------Fission ()
                  IF (FISsil(nnuc) .AND. (FISshi(nnuc).EQ.1.))
     &                CALL FISSION(nnuc,ke,jcn,sumfis)
                  IF (FISsil(nnuc) .AND. (FISshi(nnuc).NE.1.))
     &                CALL FISCROSS(nnuc,ke,ip,jcn,sumfis,sumfism,
     &                              dencomp,aafis,0)
C-----------------Normalization and accumulation
C-----------------
                  xnor = POP(ke,jcn,ipar,nnuc)*step/DENhf
                  stauc = stauc + RO(ke,jcn,nnuc)*xnor
                  IF (RO(ke,jcn,nnuc).NE.0.0D0) sgamc = sgamc +
     &                DENhf*POP(ke,jcn,ipar,nnuc)*step/RO(ke,jcn,nnuc)
                  CALL XSECT(nnuc,m,xnor,sumfis,sumfism,ke,ipar,jcn,
     &                       dencomp,aafis)
C-----------------Calculate total emission
                  DO nejc = 0, NEJcm
                     csemist = csemist + CSEmis(nejc,nnuc)
                  ENDDO
                  csemist = csemist + CSFis
 1470          ENDDO                !loop over decaying nucleus spin
            ENDDO                   !loop over decaying nucleus parity
            IF (ENDf(nnuc).GT.0  .AND. RECoil.GT.0) 
     &         CALL GET_RECOIL(ke,nnuc) !recoil spectrum for ke bin
            IF (FISsil(nnuc)) THEN
               IF (FISmod(nnuc).EQ.0.) WRITE (80,*) 'csfis=', CSFis,
     &             ' mb'
               IF (FISmod(nnuc).GT.0.) THEN
                  WRITE (80,*) '  '
                  DO m = 1, INT(FISmod(nnuc)) + 1
                     WRITE (80,*) '    Mode=', m, '  csfis=', CSFism(m),
     &                            ' mb'
                  ENDDO
               ENDIF
            ENDIF
         ENDDO                  !loop over c.n. excitation energy
C--------
C--------Hauser-Feshbach decay of nnuc  ***done***
C--------
C--------Printout of results for the decay of NNUC nucleus
         IF (IOUt.GT.0) WRITE (6,
     &          '(1X,/,'' Population left because too small '',G12.5,/)'
     &          ) popleft*DE
 1500    dtmp = 0.d0
         DO il = 1, NLV(nnuc)
           dtmp = dtmp + POPlv(il,nnuc)
         ENDDO
         IF(dtmp.LE.0.d0) GOTO 1525
         IF (IOUt.GT.0) WRITE (6,
     &                        '(1X,/,10X,''Discrete level population'')'
     &                        )
         IF (IOUt.GT.0 .AND. kemin.EQ.NEX(nnuc) .AND. nnuc.EQ.1)
     &       WRITE (6,
     &'(10X,''(no gamma cascade in the compound nucleus, primary transit
     &ions only)'',/)')
         IF (IOUt.GT.0 .AND. ENDf(nnuc).NE.0.0D0 .AND.
     &       (nnuc.EQ.mt91 .OR. nnuc.EQ.mt649 .OR. nnuc.EQ.mt849))
     &       WRITE (6,
     &'(10X,''NOTE: due to ENDF option direct particle contribution was
     & shifted to the g.s.'')')
         IF (IOUt.GT.0) WRITE (6,'(1X,/,10X,40(1H-),/)')
         IF (ENDf(nnuc).NE.0 .AND. nnuc.EQ.1) THEN
            WRITE (12,
     &'(1X,/,10X,''Discrete level population '',              ''before g
     &amma cascade'')')
            WRITE (12,'(1X,/,10X,40(1H-),/)')
         ENDIF
         DO il = 1, NLV(nnuc)
            CSPrd(nnuc) = CSPrd(nnuc) + POPlv(il,nnuc)
            IF(ISIsom(il,Nnuc).EQ.0) THEN
              IF (IOUt.GT.0) WRITE (6,99070) il, ELV(il,nnuc),
     &                              LVP(il,nnuc), XJLv(il,nnuc),
     &                              POPlv(il,nnuc)
            ELSE
99071 FORMAT (I12,F10.5,I5,F8.1,G15.6,A7)            
              IF (IOUt.GT.0) WRITE (6,99071) il, ELV(il,nnuc),
     &                              LVP(il,nnuc), XJLv(il,nnuc),
     &                              POPlv(il,nnuc),' ISOMER'
            ENDIF
            IF (ENDf(nnuc).NE.0 .AND. nnuc.EQ.1) THEN
C--------------Check for the number of branching ratios
               nbr = 0
               DO ib = 1, NDBR
                  IF (BR(il,ib,2,nnuc).EQ.0.) GOTO 1510
                  nbr = ib
               ENDDO
 1510          IF (nbr.EQ.0 .AND. il.NE.1 .AND. FIRst_ein) WRITE (6,*)
     &              ' WARNING: Branching ratios for level ', il, ' in ',
     &             INT(A(nnuc)), '-', SYMb(nnuc), ' are missing'
               WRITE (12,99070) il, ELV(il,nnuc), LVP(il,nnuc),
     &                          XJLv(il,nnuc), POPlv(il,nnuc), nbr,
     &                          (NINT(BR(il,ib,1,nnuc)),BR(il,ib,2,nnuc)
     &                          ,ib = 1,nbr)
            ENDIF
         ENDDO
         IF ( (ENDf(nnuc).GT.0 .AND. CSPrd(nnuc).GT.0.d0) .AND.
     &        (nnuc.EQ.1 .OR. nnuc.EQ.mt91 .OR. nnuc.EQ.mt649 .OR.
     &         nnuc.EQ.mt849)) THEN
            WRITE (12,'(1X,/,10X,40(1H-),/)')
            WRITE (12,*) ' '
C-----------Write Int. Conv. Coefff. for discrete transitions
            WRITE (12,'(1X,/,10X,
     &             ''Internal conversion coefficients'')')
            WRITE (12,'(1X,/,10X,40(1H-),/)')
            DO il = 1, NLV(nnuc)
C-------------Check for the number of branching ratios
              nbr = 0
              DO ib = 1, NDBR
                IF (BR(il,ib,2,nnuc).EQ.0.) GOTO 1520
                nbr = ib
              ENDDO
 1520         WRITE (12,99065) il, ELV(il,nnuc), LVP(il,nnuc),
     &                          XJLv(il,nnuc), POPlv(il,nnuc), nbr,
     &                          (NINT(BR(il,ib,1,nnuc)),BR(il,ib,3,nnuc)
     &                          ,ib = 1,nbr)
99065         FORMAT (I12,F10.5,I5,F8.1,G15.6,I3,7(I4,E11.4),:/,
     &                 (53X,7(I4,E11.4)))
            ENDDO
            WRITE (12,'(1X,/,10X,40(1H-),/)')
         ENDIF
C--------gamma decay of discrete levels (DECAYD)
         CALL DECAYD(nnuc)
1525     ia = INT(A(nnuc))
         iz = INT(Z(nnuc))
         IF (IOUt.GT.0) THEN
            WRITE (6,'(1X,/,10X,40(1H-),/)')
            WRITE (6,
     &'(1X,I3,''-'',A2,''-'',I3,'' production cross section '',G12.5,
     &'' mb  '',''reaction: '',A21)') iz, SYMb(nnuc), ia, CSPrd(nnuc),
     &                             REAction(nnuc)
            IF (kemin.EQ.NEX(nnuc) .AND. nnuc.EQ.1) WRITE (6,
     &'(1X,''(no gamma cascade in the compound nucleus, primary transiti
     &ons only)'',/)')
         ENDIF
C--------Integrating exclusive population spectra (ENDF)
         gtotsp = 0
         xtotsp = 0
         ptotsp = 0
         atotsp = 0
         htotsp = 0
         emedg = 0
         emedn = 0
         emedp = 0
         emeda = 0
         emedh = 0
         IF ( ENDf(nnuc).EQ.1 .AND. INExc(nnuc).GE.0)     THEN
           DO ispec = 1, min(NEX(1) + 10,ndecsed)
            gtotsp = gtotsp + POPcse(0,0,ispec,INExc(nnuc))*DE
            xtotsp = xtotsp + POPcse(0,1,ispec,INExc(nnuc))*DE
            ptotsp = ptotsp + POPcse(0,2,ispec,INExc(nnuc))*DE
            atotsp = atotsp + POPcse(0,3,ispec,INExc(nnuc))*DE
            emedg = emedg+POPcse(0,0,ispec,INExc(nnuc))*DE*(ispec-1)*DE
            emedn = emedn+POPcse(0,1,ispec,INExc(nnuc))*DE*(ispec-1)*DE
            emedp = emedp+POPcse(0,2,ispec,INExc(nnuc))*DE*(ispec-1)*DE
            emeda = emeda+POPcse(0,3,ispec,INExc(nnuc))*DE*(ispec-1)*DE
            IF (NDEJC.EQ.4) THEN
               htotsp = htotsp + POPcse(0,NDEJC,ispec,INExc(nnuc))*DE
               emedh = emedh + POPcse(0,NDEJC,ispec,INExc(nnuc))
     &                    *DE*(ispec - 1)*DE
            ENDIF
           ENDDO
           POPcs(0,INExc(nnuc)) = gtotsp
           POPcs(1,INExc(nnuc)) = xtotsp
           POPcs(2,INExc(nnuc)) = ptotsp
           POPcs(3,INExc(nnuc)) = atotsp
           IF (NDEJC.EQ.4) POPcs(NDEJC,INExc(nnuc)) = htotsp
           IF (gtotsp.NE.0) emedg = emedg/gtotsp
           IF (xtotsp.NE.0) emedn = emedn/xtotsp
           IF (ptotsp.NE.0) emedp = emedp/ptotsp
           IF (atotsp.NE.0) emeda = emeda/atotsp
           IF (htotsp.NE.0) emedh = emedh/htotsp
           IF (CSPrd(nnuc).GT.0.d0 .AND. IOUt.GT.3) THEN
C--------------Add contributions to discrete levels for MT=91,649,849
C--------------(merely for checking purpose)
               IF (nnuc.EQ.mt91) THEN
                  nejc = 1
                  WRITE (6,'(11X,'' Sum to continuum         '',G12.5,
     &                '' mb  '')') xtotsp
                  xtotsp = CSDirlev(1,nejc)
c                 DO ilev = 1, NLV(nnuc)
c                    xtotsp = xtotsp + CSDirlev(ilev,nejc)
c                 ENDDO
               ELSEIF (nnuc.EQ.mt649) THEN
                  nejc = 2
                  WRITE (6,'(11X,'' Sum to continuum         '',G12.5,
     &                '' mb  '')') ptotsp
                  ptotsp = CSDirlev(1,nejc)
c                 DO ilev = 1, NLV(nnuc)
c                    ptotsp = ptotsp + CSDirlev(ilev,nejc)
c                 ENDDO
               ELSEIF (nnuc.EQ.mt849) THEN
                  nejc = 3
                  WRITE (6,'(11X,'' Sum to continuum         '',G12.5,
     &                '' mb  '')') atotsp
                  atotsp = CSDirlev(1,nejc)
c                 DO ilev = 1, NLV(nnuc)
c                    atotsp = atotsp + CSDirlev(ilev,nejc)
c                 ENDDO
               ENDIF
               WRITE (6,*) ' '
               WRITE (6,*)
     &                  '----------------------------------------------'
               WRITE (6,*) 'Test printout (exclusive spectra)'
               WRITE (6,
     &'('' Energy'',14x,''gamma'',9x,''neutron'',8x,           ''proton
     &'',10x,''alpha'',9x,''l. ion'')')
               WRITE (6,*)
     &                  '----------------------------------------------'
               DO ispec = 1, min(NEX(1) + 10,ndecsed)
                 IF (NDEJC.EQ.4) THEN
                     WRITE (6,'(6g15.5)') (ispec - 1)*DE,
     &                      POPcse(0,0,ispec,INExc(nnuc)),
     &                      POPcse(0,1,ispec,INExc(nnuc)),
     &                      POPcse(0,2,ispec,INExc(nnuc)),
     &                      POPcse(0,3,ispec,INExc(nnuc)),
     &                      POPcse(0,NDEJC,ispec,INExc(nnuc))
                 ELSE
                     WRITE (6,'(5g15.5)') (ispec - 1)*DE,
     &                      POPcse(0,0,ispec,INExc(nnuc)),
     &                      POPcse(0,1,ispec,INExc(nnuc)),
     &                      POPcse(0,2,ispec,INExc(nnuc)),
     &                      POPcse(0,3,ispec,INExc(nnuc))
                 ENDIF
               ENDDO
               WRITE (6,*) '-----------------------------------------'
               WRITE (6,'(15X,5g15.5)')gtotsp, xtotsp, ptotsp, atotsp,
     &                                  htotsp
               WRITE (6,'(''E-aver.'',8X,6g15.5)')emedg, emedn, emedp,
     &                emeda, emedh, emedg + emedn + emedp + emeda +
     &                emedh
               WRITE (6,*) '-----------------------------------------'
               WRITE (6,*) ' '
               WRITE (6,*)
     &                '----------------------------------------------'
               WRITE (6,*) 'Test printout (portions of DDX spectra)'
               WRITE (6,
     &                '(''     Energy'',8x,'' gamma '',9x,''neutron'',
     &                                9x,''proton '',7x,'' alpha'')')
               DO ispec = 1, NEX(1) + 10
                  WRITE (6,'(5g15.5)') (ispec - 1)*DE,
     &                                 POPcseaf(0,0,ispec,INExc(nnuc)),
     &                                 POPcseaf(0,1,ispec,INExc(nnuc)),
     &                                 POPcseaf(0,2,ispec,INExc(nnuc)),
     &                                 POPcseaf(0,3,ispec,INExc(nnuc))
               ENDDO
               WRITE (6,*) ' '
           ENDIF
         ENDIF
         IF (FISmod(nnuc).GT.0) THEN
           CSFis  = 0.d0 ! RCN Jan 2006
           DO m = 1, INT(FISmod(nnuc)) + 1
              CSFis = CSFis + CSFism(m)
           ENDDO
         ENDIF
         IF (IOUt.GT.0) THEN
           DO m = 1, INT(FISmod(nnuc)) + 1
              IF (CSFis.GT.0.) WFIsm(m) = CSFism(m)/CSFis
              WRITE (80,*) '    Mode=', m, '   weight=', WFIsm(m)
           ENDDO
           WRITE (80,*) '   Fission cross section=', CSFis, ' mb'
         ENDIF
         IF (CSFis.NE.0.0D0) THEN
           CSPfis(nnuc) = CSFis
           WRITE (6,
     &'(1X,I3,''-'',A2,''-'',I3,'' fission cross  section '',G12.5,''
     &mb  ''/)') iz, SYMb(nnuc), ia, CSFis
           IF (IOUt.GT.0) THEN
C-------------Calculate average fission life-time and width
              tauf = stauc*6.589E-22*2.0*PI/CSFis
              WRITE(6,
     &         '(''  Average fission life-time'',G12.5,'' s'')') tauf
               gamfis = gamt*CSFis/csemist
              WRITE (6,
     &        '(''  Average fission width    '',G12.5,'' MeV'')') gamfis
              WRITE (6,*) ' '
           ENDIF
         ENDIF
         IF (csemist.NE.0.0D0 .and. IOUt.GT.0) THEN
C----------Calculate average total life-time and width
           taut = stauc*6.589E-22*2.0*PI/csemist
           WRITE(6,'(/''  Average total   life-time'',G12.5,'' s'')')
     &       taut
           gamt = sgamc/2.0/PI/csemist
           WRITE(6,'(''  Average total   width    '',G12.5,'' MeV'')')
     &                gamt
C          TAUT=6.589E-22/GAMT
C          WRITE(6,'('' Average total life-time 1/width'',g12.5,
C    &               '' s'')') TAUT
         ENDIF
         TOTcsfis = TOTcsfis + CSFis
C--------Add compound elastic to shape elastic before everything falls
C--------down on the ground state
9876     IF (nnuc.EQ.1 .AND. INT(AEJc(0)).NE.0
     &                       .AND. POPlv(LEVtarg,mt2).GT.0.) THEN
           WRITE (6,*)
           WRITE (6,*) ' Incident energy (CMS)      ', EIN, ' MeV'
           WRITE (6,*) ' Shape elastic cross section', 
     &                   ElasticCorr + ELAcs, ' mb'
           WRITE (6,*) ' CN elastic cross section   ',
     &                    POPlv(LEVtarg,mt2),' mb'
C          ELAcs = ELAcs + POPlv(LEVtarg,mt2)   ! commented RCN, 03/07
C----------CN contribution to elastic ddx
           elcncs = POPlv(LEVtarg,mt2)/4.0/PI
           WRITE (6,*)
     &          ' CN elastic angular distrib.', elcncs, ' mb/str'
           WRITE (6,*)
         ENDIF
         checkXS = checkXS + CSPrd(nnuc)
         WRITE (12,*) ' '
         WRITE (12,
     &'(1X,I3,''-'',A2,''-'',I3,'' production cross section '',G12.6,
     &'' mb'')') iz, SYMb(nnuc), ia, CSPrd(nnuc)
         
         IF(CSPrd(nnuc).GT.0.d0) THEN
           metas = 0
           ftmp_gs = CSPrd(nnuc)
           DO l= NLV(Nnuc), 2, -1
            IF(ISIsom(l,Nnuc).EQ.1) THEN 
              metas = metas + 1            
              WRITE(12,'(1X,I3,''-'',A2,''-'',I3,
     &         '' isomer state population  '',G12.6,
     &         ''mb (m'',I1,'' E='',F7.4,''MeV Jp='',F5.1,'')'')')
     &         iz, SYMb(nnuc), ia, POPlv(l,Nnuc),
     &         metas, ELV(l,Nnuc), LVP(l,Nnuc)*XJLv(l,Nnuc) 
              ftmp_gs = ftmp_gs - POPlv(l,Nnuc)
C             CSPrd(nnuc) = CSPrd(nnuc) - POPlv(l,Nnuc)
            ENDIF 
           ENDDO 
           IF(metas.GT.0) WRITE(12,'(1X,I3,''-'',A2,''-'',I3,
     &           '' ground state population  '',G12.6,''mb'')')
     &           iz, SYMb(nnuc), ia, ftmp_gs
         ENDIF
         IF(CSFis.gt.0.)
     &      WRITE (12,'(''    fission  cross section'',G12.5,'' mb'')')
     &          CSFis
         IF(CSEmis(0,nnuc).gt.0.) THEN
           IF(IOUt.GT.2) CALL AUERST(nnuc,0)
           WRITE (6,'(''  g  emission cross section'',G12.5,'' mb'')')
     &          CSEmis(0,nnuc)
           WRITE (12,'('' g  emission cross section'',G12.5,'' mb'')')
     &          CSEmis(0,nnuc)
         ENDIF
C----------------------------------------------------------------------
         IF(CSPrd(nnuc).GT.0.d0) THEN
           DO nejc = 1, NEJcm
             ares = A(nnuc) - AEJc(nejc)
             zres = Z(nnuc) - ZEJc(nejc)
C------------Residual nuclei must be heavier than alpha
             if(ares.le.4. and. zres.le.2.) cycle
             izares = INT(1000.0*zres + ares)
             CALL WHERE(izares,nnur,iloc)
             if(iloc.eq.1) CYCLE
             IF(CSEmis(nejc,nnuc).LE.0.) CYCLE
             WRITE (12,
     &             '(1X,A2,'' emission cross section'',G12.5,'' mb'')')
     &             SYMbe(nejc), CSEmis(nejc,nnuc)
             IF (IOUt.GT.2) CALL AUERST(nnuc,nejc)
             IF (IOUt.GT.0) WRITE (6,
     &             '(2X,A2,'' emission cross section'',G12.5,'' mb'')')
     &             SYMbe(nejc), CSEmis(nejc,nnuc)
C------------Print residual nucleus population
             IF (IOUt.EQ.4) THEN
               ia = INT(A(nnur))
               WRITE (6,*) ' '
               WRITE (6,*) '**************************** '
               WRITE (6,'('' Residual nucleus '',I3,''-'',A2,/)') ia,
     &                SYMb(nnur)
               WRITE (6,'('' Positive parities population'',/)')
               do i = NEX(nnur),1,-1
                 ftmp = 0.0
                 do j = 1,12
                   ftmp = ftmp + POP(i,j,1,nnur)
                 enddo
                 if(ftmp.gt.0.0)
     &             WRITE (6,99075) EX(i,nnur),(POP(i,j,1,nnur),j = 1,12)
               enddo
               WRITE (6,*) ' '
               WRITE (6,'('' Negative parities population'',/)')
               do i = NEX(nnur),1,-1
                 ftmp = 0.0
                 do j = 1,12
                   ftmp = ftmp + POP(i,j,2,nnur)
                 enddo
                 if(ftmp.gt.0.0)
     &             WRITE (6,99075) EX(i,nnur),(POP(i,j,2,nnur),j = 1,12)
               enddo
               WRITE (6,'('' '')')
               poptot = 0.0
               DO j = 1, NLW
                  DO i = 1, NEX(nnur)
                     poptot = poptot + POP(i,j,1,nnur) + POP(i,j,2,nnur)
                  ENDDO
                  poptot = poptot -
     &                     0.5*(POP(1,j,1,nnur) + POP(1,j,2,nnur))
               ENDDO
               poptot = poptot*DE
               WRITE (6,*) ' '
               WRITE (6,
     &         '(1x,''    Total popul.continuum '',G12.5,'' mb'')')
     &          poptot
               poplev = 0.0
               DO i = 1, NLV(nnur)
                  poplev = poplev + POPlv(i,nnur)
               ENDDO
               WRITE (6,
     &         '(1x,''    Total popul.disc.lev. '',G12.5,'' mb'')')
     &          poplev
               WRITE (6,
     &         '(1x,''    Total population      '',G12.5,'' mb'')')
     &          poplev + poptot
               WRITE (6,
     &         '(2X,A2,'' emission cross section'',G12.5,'' mb'')'
     &               ) SYMbe(nejc), CSEmis(nejc,nnuc)
               WRITE (6,*) ' '
             ENDIF
           ENDDO   !over ejectiles
         ENDIF ! if CSProd > 0
C--------
C--------NNUC nucleus decay    **** done ******
C--------
      ENDDO     !over decaying nuclei
C-----Write a row in the table of cross sections (Note: inelastic has CN elastic subtracted)
      WRITE(41,'(G10.5,1P(90E12.5))') EINl, TOTcs*TOTred, 
     &     ELAcs + ElasticCorr + 4.*PI*ELCncs,
     &     CSFus + (SINl+SINlcc)*FCCred + SINlcont,
     &     TOTcsfis, CSPrd(1), CSPrd(2)-4.*PI*ELCncs,
     &     (CSPrd(nnuc),nnuc=3,NNUcd)

C     WRITE(41,'(/G10.5,6E12.5\))') EINl, TOTcs*TOTred, 
C    &     ELAcs + ElasticCorr + 4.*PI*ELCncs,
C    &     CSFus + (SINl+SINlcc)*FCCred + SINlcont,
C    &     TOTcsfis, CSPrd(1), CSPrd(2)-4.*PI*ELCncs
C     DO nnuc=3,NNUcd
C       IF(ENDf(nnuc).GT.0) WRITE(41,'(E12.5\)') CSPrd(nnuc)
C     ENDDO 
C     WRITE (41,'(/)')

      WRITE(98,'(G10.5,2X,1P(90E12.5))') EINl,
     &     TOTcsfis, (CSPfis(nnuc),nnuc=1,NNUcd)
      CLOSE (80)
      CLOSE (79)
      WRITE (12,*) ' '
      WRITE (12,*) ' '
      WRITE (12,'('' Tot. fission cross section '',G12.4,'' mb'')')
     &       TOTcsfis
      WRITE (6,*) ' '
      WRITE (6,*) ' '
      WRITE (6,'(''  Tot. fission cross section '',G12.4,'' mb'')')
     &       TOTcsfis
C----
C---- ENDF spectra printout (exclusive representation)
C----
c fisspec===========
      IF (FISspe.eq.1) CALL INPUT_SPEC
c fisspec===============
C     Assumed that a maximum of 2 different isotopes contribute to fission XS
C     (i.e those coming from fissioning nuclei after primary neutron-proton
C     emission and first CN.
      bindS(0) = 0.d0
      bindS(1) = 0.d0
      isnewchain(0) = 0 
      isnewchain(1) = 0
      fniuS    = 0.d0 ! total multiplicity
      nfission = 0
      nepfns(0)= 0
      nepfns(1)= 0
      tequiv0 = 0.d0
      DO ie = 1, NDEPFN
        ENEpfns(ie,0) = 0.d0
        ENEpfns(ie,1) = 0.d0
        csepfns(ie,0) = 0.d0
        csepfns(ie,1) = 0.d0
        DO nang = 1, NDANGD
          cseapfns(ie,nang) = 0.d0
        ENDDO
      ENDDO
C     For Kalbach parameterization
      do i=1,NDAng
         theta=DBLE(i-1)/DBLE(NDAng-1)*pi
         xcos(i)=cos(theta)
      enddo
      DO nnuc = 1, NNUcd  ! loop over decaying nuclei
         IF (ENDf(nnuc).EQ.1) THEN
           IF (CSPrd(nnuc).GT.0.0D0) THEN
              DO nejc = 0, NDEJC         !loop over ejectiles
                IF (POPcs(nejc,INExc(nnuc)).EQ.0.d0) CYCLE
                ares = A(nnuc) - AEJc(nejc)
                zres = Z(nnuc) - ZEJc(nejc)
C---------------Residual nuclei must be heavier than alpha
                if(ares.le.4. and. zres.le.2.) cycle
                IF(nejc.GE.1 .AND. nejc.LE.3) THEN
                  DO itmp = 3,21
                    IF(REAction(nnuc)(itmp:itmp).eq.opart(nejc))
     &                goto 1529
                  ENDDO
                  CYCLE
                ENDIF
 1529           nspec = min(INT(EMAx(nnuc)/DE) + 2,NDECSE)
                IF (nejc.EQ.0) THEN
                  cejectile = 'gammas   '
                  iizaejc = 0
                ELSEIF (nejc.EQ.1) THEN
                  cejectile = 'neutrons '
                  iizaejc = IZAejc(nejc)
                ELSEIF (nejc.EQ.2) THEN
                  cejectile = 'protons  '
                  iizaejc = IZAejc(nejc)
                ELSEIF (nejc.EQ.3) THEN
                  cejectile = 'alphas   '
                  iizaejc = IZAejc(nejc)
                ELSEIF (nejc.EQ.4) THEN
                  cejectile = 'lt. ions '
                  iizaejc = IZAejc(nejc)
                ENDIF
C---------------Double the first bin x-sec to preserve integral in EMPEND
C               POPcse(0, nejc, 1, INExc(nnuc)) =  POPcse(0, nejc, 1, INExc(nnuc))*2
                WRITE (12,*) ' '
                WRITE (12,*) ' Spectrum of ', cejectile,
     &                         REAction(nnuc), ' ZAP= ', iizaejc
C---------------recorp is a recoil correction factor defined 1+Ap/Ar that
C---------------multiplies cross sections and divides outgoing energies
                recorp = 1.0
                IF (nejc.GT.0) recorp = 1. + EJMass(nejc)/AMAss(nnuc)
C---------------Exclusive DDX spectra (neutrons & protons)
                IF (nejc.GE.1 .AND. nejc.LE.2) THEN
                  WRITE (12,
     &                      '(30X,''A     n     g     l     e     s '')'
     &                      )
                  WRITE (12,*) ' '
                  WRITE (12,'('' Energy   '',8G15.5,/,(10X,8G15.5))')
     &                      (ANGles(nang),nang=1,NDANG)
                  IF ((nnuc.EQ.mt91 .AND. nejc.EQ.1) .OR.
     &                   (nnuc.EQ.mt649 .AND. nejc.EQ.2)) THEN
                                                              ! first emission reactions
C-----------------------(discrete levels part)
                        DO il = 1, NLV(nnuc)  !(levels)
                           espec = (EMAx(nnuc) - ELV(il,nnuc))/recorp
                           IF (espec.GE.0) WRITE (12,
     &'(F10.5,E14.5,7E15.5,/,                                       (9X,
     &8E15.5))') -espec, (max(CSAlev(nang,il,nejc)*recorp/DE,0.d0),
     &nang = 1,NDANG)
                        ENDDO
C-----------------------(continuum part)
                        DO ie = 1, nspec + 1
                                           ! clean DDX matrix
                           DO nang = 1, NDANG
                              cseaprnt(ie,nang) = 0.0
                           ENDDO
                        ENDDO
                        IF (nspec.GT.0) THEN
                           iprinted = 0
                           DO ie = 1, nspec ! reconstruct continuum DDX spectrum
                              piece = CSEmsd(ie,nejc)
                              IF (ie.EQ.NEXr(nejc,1)) piece = 0.5*piece
                              ftmp =(POPcse(0,nejc,ie,INExc(nnuc))-
     &                             piece*POPcseaf(0,nejc,ie,INExc(nnuc))
     &                             )/4.0/PI
                              IF(ftmp.LT.0 .and. iprinted.eq.0) THEN
                                 IF(iprinted.eq.0) WRITE(6,*)
     &                                      'WARNING: Corrective action
     &to avoid negative ddx cross sections taken'
                                 iprinted = 1
                                 ftmp = 0.0
                                 POPcseaf(0,nejc,ie,INExc(nnuc)) =
     &                                  POPcse(0,nejc,ie,INExc(nnuc))/
     &                                  piece
                              ENDIF
                              DO nang = 1, NDANG
                                 cseaprnt(ie,nang) =
     &                          ftmp + CSEa(ie,nang,nejc,INExc(1))*
     &                                POPcseaf(0,nejc,ie,INExc(nnuc))
                              ENDDO
                           ENDDO
                        ENDIF
                        DO nang = 1, NDANG
                                          !double the first bin to preserve integral in EMPEND
                           cseaprnt(1,nang) = cseaprnt(1,nang)*2.0
                        ENDDO
                        DO ie = 1, nspec - 1
                                           ! print DDX spectrum
                           WRITE (12,
     &'(F10.5,E14.5,7E15.5,/,                                 (9X,8E15.5
     &))') FLOAT(ie - 1)*DE/recorp,
     &                  (cseaprnt(ie,nang)*recorp,nang = 1,NDANG)
                        ENDDO
                        DO ie = nspec, nspec + 1
                                               ! exact DDX spectrum endpoint
                           WRITE (12,
     &'(F10.5,E14.5,7E15.5,/,                                 (9X,8E15.5
     &))') EMAx(nnuc)/recorp,
     &     (cseaprnt(ie,nang)*recorp,nang = 1,NDANG)
                        ENDDO
                  ELSE ! than ((nnuc.EQ.mt91 .AND. nejc.EQ.1) .OR. (nnuc.EQ.mt649 .AND. nejc.EQ.2))
C-----------------------Remaining n- or p-emissions (continuum and levels together)
                        DO ie = 1, nspec + 1
                                           ! clean DDX matrix
                           DO nang = 1, NDANG
                              cseaprnt(ie,nang) = 0.0
                           ENDDO
                        ENDDO
                        iprinted = 0
                        DO ie = 1, nspec  ! reconstruct DDX spectrum
                           piece = CSEmsd(ie,nejc)
                           IF (ie.EQ.NEXr(nejc,1)) piece = 0.5*piece
                           ftmp =(POPcse(0,nejc,ie,INExc(nnuc))-
     &                           piece*POPcseaf(0,nejc,ie,INExc(nnuc)
     &                           ))/4.0/PI
                           IF(ftmp.LT.0) THEN
                              ftmp = 0.0
                              POPcseaf(0,nejc,ie,INExc(nnuc)) =
     &                               POPcse(0,nejc,ie,INExc(nnuc))/
     &                               piece
                              IF(iprinted.eq.0) WRITE(6,*)
     &                                   'WARNING: Corrective action to
     &avoid negative ddx cross sections taken'
                              iprinted = 1
                           ENDIF
                           DO nang = 1, NDANG
                              cseaprnt(ie,nang)
     &                           = ftmp + CSEa(ie,nang,nejc,INExc(1))*
     &                          POPcseaf(0,nejc,ie,INExc(nnuc))
                           ENDDO
                        ENDDO
C                       DO ie = 1, nspec  ! reconstruct DDX spectrum
C                          DO nang = 1, NDANG
C                             piece = CSEmsd(ie,nejc)
C                             IF (ie.EQ.NEXr(nejc,1)) piece = 0.5*piece
C                             cseaprnt(ie,nang)
C    &                           = ((POPcse(0,nejc,ie,INExc(nnuc))
C    &                          - piece*POPcseaf(0,nejc,ie,INExc(nnuc)))
C    &                           /4.0/PI + CSEa(ie,nang,nejc,INExc(1))
C    &                           *POPcseaf(0,nejc,ie,INExc(nnuc)))
C                          ENDDO
C                       ENDDO
C-----------------------double the first bin to preserve integral in EMPEND
                        DO nang = 1, NDANG
                           cseaprnt(1,nang) = cseaprnt(1,nang)*2.0
                        ENDDO
                        DO ie = 1, nspec - 1
                           WRITE (12,
     &'(F10.5,E14.5,7E15.5,/,                                 (9X,8E15.5
     &))') FLOAT(ie - 1)*DE/recorp,
     &                  (cseaprnt(ie,nang)*recorp,nang = 1,NDANG)
                        ENDDO
                        DO ie = nspec, nspec + 1
                                               ! exact DDX spectrum endpoint
                           WRITE (12,
     &'(F10.5,E14.5,7E15.5,/,                                 (9X,8E15.5
     &))') EMAx(nnuc)/recorp,
     &     (max(cseaprnt(ie,nang)*recorp,0.d0),nang = 1,NDANG)
                        ENDDO
                  ENDIF ! ((nnuc.EQ.mt91 .AND. nejc.EQ.1) .OR. (nnuc.EQ.mt649 .AND. nejc.EQ.2))
               ELSE !  than (nejc.GE.1 .AND. nejc.LE.2)
C-----------------Exclusive DDX spectra (gammas, alphas, light ions (DE))
C-----------------double the first bin x-sec to preserve integral in EMPEND
                  POPcse(0,nejc,1,INExc(nnuc)) =
     &                  POPcse(0,nejc,1,INExc(nnuc))*2
                  WRITE (12,*) ' '
                  WRITE (12,'('' Energy    mb/MeV'')')
                  WRITE (12,*) ' '
                  IF (nnuc.EQ.mt849 .AND. nejc.EQ.3) THEN
                                        ! first emission (z,a) reaction
                        DO il = 1, NLV(nnuc) ! MT=801,802,... (levels)
C--------------------------Although DDX spectra are available for a-emission
C--------------------------they are isotropic and only ang. integrated are
C--------------------------printed (4*Pi*CSAlev(1,il,3)
                           espec = (EMAx(nnuc) - ELV(il,nnuc))/recorp
                           IF (espec.GE.0) WRITE (12,'(F10.5,E14.5)')
     &                      -espec, max(CSAlev(1,il,3),0.d0)
     &                      *4.0*PI*recorp/DE
                        ENDDO
                        DO ie = 1, nspec - 1
                                            ! MT=849 (continuum)
                           WRITE (12,'(F10.5,E14.5)') FLOAT(ie - 1)
     &                        *DE/recorp,
     &                        max(0.d0,POPcse(0,nejc,ie,INExc(nnuc)))
     &                        *recorp
                        ENDDO
                                          ! MT=849 exact endpoint
                        WRITE (12,'(F10.5,E14.5)') EMAx(nnuc)/recorp,
     &                        max(0.d0,POPcse(0,nejc,nspec,INExc(nnuc)))
     &                        *recorp
                        WRITE (12,'(F10.5,E14.5)') EMAx(nnuc)/recorp,
     &                            0.d0
                  ELSE  !all other emissions (continuum and levels together)
                        DO ie = 1, nspec - 1
                           WRITE (12,'(F10.5,E14.5)') FLOAT(ie - 1)
     &                     *DE/recorp,
     &                     max(0.d0,POPcse(0,nejc,ie,INExc(nnuc)))
     &                     *recorp
                        ENDDO
                                                 ! exact endpoint
                        WRITE (12,'(F10.5,E14.5)') EMAx(nnuc)/recorp,
     &                     max(0.d0,POPcse(0,nejc,nspec,INExc(nnuc)))
     &                     *recorp
                        WRITE (12,'(F10.5,E14.5)') EMAx(nnuc)/recorp,
     &                            0.d0
                  ENDIF
               ENDIF !  (nejc.GE.1 .AND. nejc.LE.2)
 1530         ENDDO   ! over ejectiles
              IF (nnuc.NE.1  .AND. RECoil.GT.0) 
     &          CALL PRINT_RECOIL(nnuc,REAction(nnuc))
           ENDIF ! IF (CSPrd(nnuc).GT.0.0D0)
         ENDIF ! IF (ENDf(nnuc).EQ.1)
C
C--------Prompt fission spectra of neutrons and gammas
C        for a given decaying nucleus
C
         IF (FISSPE.gt.0 .and. 
     &      TOTcsfis.gt.0.d0 .and. CSPfis(nnuc).GT.0.0D0 .and.
     &       ( Z(0).eq.Z(nnuc) .OR.   Z(0)-1.eq.Z(nnuc) ) .AND.
     &      ENDf(nnuc).EQ.1 .and.
C           Prompt fission spectra are not calculated if:
C           Partial fission cross section is lower than 1.d-7*TOTcsfis
     &      CSPfis(Nnuc).GT.1.d-7*TOTcsfis) THEN
C
C           Only neutron and proton isotope chains are considered for PFNS
            izfiss =  NINT(Z(0)) - NINT(Z(nnuc))
            IF (izfiss.gt.1) CYCLE
            iaf = A(nnuc)
            izf = Z(nnuc)

            WRITE
     & (73,'(''*** Fiss. CN nucleus '',I3,''-'',A2,''-'',I3,''  Elab '',
     &  F8.4)') INT(Z(nnuc)), SYMb(nnuc), INT(A(nnuc)), EINl
            DO nejc = 0, 1         !loop over gamma and neutrons only
              DO ie = 1, NDEPFN
                emiss_en(ie) = 0.d0
                post_fisn(ie) = 0.d0
                ratio2maxw(ie) = 0.d0
              ENDDO
              IF (nejc.EQ.0) THEN
                cejectile = 'gammas   '
                iizaejc = 0
                espmax = EMAx(1)
              ELSEIF (nejc.EQ.1) THEN
                  cejectile = 'neutrons '
                  iizaejc = IZAejc(nejc)
                  espmax = EMAx(1) - Q(1,1)
              ENDIF
C
              if(nfission.eq.0) THEN
C               Calculating unique energy grid (only for the first fissioning nucleus)
                nspec = MIN(INT(espmax/DE) + 2,NDECSE - 1)
                if(espmax.LT.25.d0) THEN
C                 Assumed maximum energy of neutrons from fragments will be 25 MeV
                  nspecmax = min( NINT(25.d0/0.1D0) + 2, NDEPFN)
                else
C                 Maximum emission energy from n,xnf
                  nspecmax = min( nspec, NDEPFN)
                endif
                nepfns(nejc) = nspecmax
                DO ie = 1, nspec - 1
                  ENEpfns(ie,nejc) = FLOAT(ie - 1)*DE
                ENDDO
                if(nspec.gt.1) then
                  DO ie = nspec, nspecmax
                    ENEpfns(ie,nejc) = ENEpfns(nspec-1,nejc) +
     &                             FLOAT(ie - nspec + 1)*0.1d0
                  ENDDO
                else
                  DO ie = 1, nspecmax
                    ENEpfns(ie,nejc) = FLOAT(ie - 1)*0.1d0
                  ENDDO
                endif
              ENDIF

              IF(isnewchain(izfiss).eq.0) THEN ! IF(nnuc.eq.1) THEN  ! CN
C           
C             First fissioning nucleus in the isotope chain 
C
C              no gammas from fragments are considered for the time being from the first CN
               IF(nejc.eq.0) CYCLE
C
C              Below first chance, no emissive contributions to fission spectra
C              Only fission neutrons emitted from fully accelerated fragments
C              acc = ROPar(1,nnuc) 
C              bett = DEF(1,Nnuc)
C
               eincid = EXCn - Q(izfiss+1,1)

               IF(FISspe.eq.1)fmultPostfiss=fniuLANL(eincid,iaf,izf)
               IF(FISspe.eq.2)fmultPostfiss=fniu    (eincid,iaf,izf)
 
               write(*,
     &        '(1x,a4,4(f8.3,1x),3x,i3,1x,i3,2x,f10.3,1x,f6.3,1x,f6.3)')
     &            'Ein=',eincid, 0.d0,  0.d0, eincid + Q(1,nnuc),  
     &            iaf, izf, CSPfis(Nnuc)/TOTcsfis, 
     &            CSPfis(Nnuc)/TOTcsfis*fmultPostfiss,0.d0              

C    &             'Ein=', eincid, 11*acc/iaf, 0.d0, EIN, iaf, izf, 
C    &              CSPfis(Nnuc)/TOTcsfis

               WRITE (12,*) ' '
               WRITE
     &        (12,'(''  Fiss. nucleus '',I3,''-'',A2,''-'',I3)')
     &            INT(Z(nnuc)), SYMb(nnuc), INT(A(nnuc))
               WRITE
     &     (12,'(''  Partial fission cross section '',G12.5,'' mb'')')
     &            CSPfis(Nnuc)
               WRITE
     &     (12,'(''  Ratio of partial to total fission '',G12.5)')
     &            CSPfis(Nnuc)/TOTcsfis
               WRITE
     &     (12,'(''  Binding energy of fissioning nucleus '',G12.5)')
     &            Q(1,nnuc)
               WRITE (12,'(''  Postfission multiplicity  '',F6.3)')
     &            fmultPostfiss*CSPfis(Nnuc)/TOTcsfis
               WRITE  (12,'(''  Partial multiplicity      '',F6.3)')
     &            fmultPostfiss*CSPfis(Nnuc)/TOTcsfis
C--------------Summing total multiplicity
               fniuS = fniuS + fmultPostfiss*CSPfis(Nnuc)/TOTcsfis
               nspecmax = nepfns(nejc)
               DO ie = 1, nspecmax
                  emiss_en(ie) = ENEpfns(ie,nejc)
               ENDDO

C--------------Calculating post-fission neutrons in the first chance
               IF (FISspe.eq.1)
     &          CALL get_fragmPFNS_LANL (post_fisn, emiss_en, nspecmax,
     &          eincid, A(Nnuc), Z(Nnuc), eneutr, tequiv)

               IF (FISspe.eq.2)
     &          CALL get_fragmPFNS      (post_fisn, emiss_en, nspecmax,
     &          eincid, A(Nnuc), Z(Nnuc), eneutr, tequiv)

C               subroutine get_fragmPFNS (fragmPFNS, emiss_en, nen_emis,
C               eincid, af, zf, emed, tequiv_maxw)
                if(eneutr.gt.0) then
                 tequiv0 = tequiv 
                 fnorm = CSPfis(nnuc)*fmultPostfiss
                 DO ie = 1, nspecmax
                   ftmp = fmaxw(emiss_en(ie),tequiv)
                   if (ftmp.gt.0) ratio2maxw(ie) = post_fisn(ie)/ftmp
                   post_fisn(ie) = post_fisn(ie) * fnorm
                   CSEfis(ie,nejc,nnuc) = CSEfis(ie,nejc,nnuc) +
     &                                      post_fisn(ie)
C------------------Accumulating total spectrum for neutrons
                   csepfns(ie,nejc) = csepfns(ie,nejc) +
     &                              CSEfis(ie,nejc,nnuc)
C------------------Post-fission neutrons assumed isotropically distributed
                   DO nang = 1, NDAng
                     cseapfns(ie,nang) = cseapfns(ie,nang) +
     &                                CSEfis(ie,nejc,nnuc)/(4.d0*PI)
                   ENDDO
                 ENDDO
                 isnewchain(izfiss) = 1
                 WRITE (12,
     &       '(''  Postfission <En> for fissioning nucleus '',
     &          G12.5,5x,G12.5)') eneutr
                 WRITE (12,
     &       '(''  Equivalent Tmaxwell '',G12.5,5x,G12.5)')
     &           tequiv
                 WRITE (12,*) ' '
C----------------Double the first bin x-sec to preserve integral in EMPEND
C                CSEfis(1,nejc,Nnuc) = CSEfis(1,nejc,Nnuc)*2
                 WRITE (12,*) ' '
                 WRITE (12,*) ' Spectrum of ', cejectile,
     &             '(z,partfis) from CN ', ' ZAP= ', IZA(Nnuc)
                 WRITE (12,*) ' '
                 WRITE (12,'('' Energy    mb/MeV'')')
                 WRITE (12,*) ' '
                 DO ie = 1, nspecmax - 1
                   WRITE (12,'(F10.5,E14.5,2x,E14.5)')
     &               emiss_en(ie), CSEfis(ie,nejc,Nnuc), ratio2maxw(ie)
                   WRITE (73,'(F10.5,E14.5,4(2x,E14.5))')
     &               emiss_en(ie), CSEfis(ie,nejc,Nnuc),
     &               post_fisn(ie), ratio2maxw(ie)
                 ENDDO
                 WRITE (12,'(F10.5,E14.5,2x,E14.5)')
     &             emiss_en(nspecmax), CSEfis(nspecmax,nejc,Nnuc),
     &             ratio2maxw(nspecmax)
                 WRITE (12,'(F10.5,E14.5,2x,E14.5)')
     &             emiss_en(nspecmax), 0.d0, 0.d0
                else
                 WRITE  (12,'(''  No fission neutrons emitted'')')
               endif
               WRITE (12,*) ' '
              ELSE ! (A(nnuc).eq.A(1) .and. Z(nnuc).eq.Z(1))
C
C--------------Fission chances (emissive and post fission contributions to PFNS)
C
C--------------Assumed maximum energy of neutrons from fragments will be 25 MeV
C--------------For high incident energies, nxnf neutrons could eventually
C--------------have higher energies than post-fission neutrons
C
               totspec = 0.d0
               eavespe = 0.d0
               if (nejc.eq.0) egamma =0.d0
               if (nejc.eq.1) eneutr =0.d0
               fmultPrefiss = 0.d0
               nspec = MIN(INT(espmax/DE) + 2,NDECSE - 1)
               IF(nspec.GT.0) THEN
                 DO ie = 1, nspec
                   eee = FLOAT(ie - 1)*DE
C------------------Integral of prefission spectra for normalization
                   totspec  = totspec  + CSEfis(ie,Nejc,Nnuc)
                   eavespe  = eavespe +
     &               FLOAT(ie - 1)*DE*CSEfis(ie,Nejc,Nnuc)
                 ENDDO
                 if(totspec.gt.0) THEN
                   if(nejc.eq.0) then
                     fmultPrefiss = 1.d0  ! gamma multiplicity fixed
                     egamma = eavespe/totspec
                   else
C--------------------Particle multiplicity is obtained from the spectrum
                     fmultPrefiss=NINT(totspec/CSPfis(nnuc))
                     eneutr = eavespe/totspec
                   endif
C------------------Normalizing prefission spectra (converting to mb/MeV)
                   fnorm = CSPfis(nnuc)*fmultPrefiss/totspec/DE
                   DO ie = 1, nspec
                     CSEfis(ie,nejc,nnuc) = CSEfis(ie,nejc,nnuc)*fnorm
                     IF (nejc.eq.0) csepfns(ie,nejc) = csepfns(ie,nejc)
     &                   + CSEfis(ie,nejc,nnuc) ! prompt fission gammas
                   ENDDO
                 endif
               ELSE
                 nspec = 1
               ENDIF
               fmultPostfiss = 0.d0
               nspecmax = nepfns(nejc)
               DO ie = 1, nspecmax
                 emiss_en(ie) = ENEpfns(ie,nejc)
               ENDDO
               IF(nejc.eq.1) THEN
C
C----------------We substract binding energy of the neutrons to the previous
C----------------nuclei in the chain to consider the emitted pre-fission neutron.
C----------------We also substract the corresponding average energy eneutr     
C            
                 bindS(izfiss) = bindS(izfiss) + Q(1,nnuc)

                 eincid = EXCn - Q(izfiss+1,1) - bindS(izfiss) - eneutr 
                 
C                acc = ROPar(1,Nnuc) 
C                bett = DEF(1,Nnuc)
                 IF(FISspe.eq.1)
     &             fmultPostfiss=fniuLANL(eincid,iaf,izf)
                 IF(FISspe.eq.2)
     &             fmultPostfiss=fniu    (eincid,iaf,izf)

                 write(*,
     &        '(1x,a4,4(f8.3,1x),3x,i3,1x,i3,2x,f10.3,1x,f6.3,1x,f6.3)')
     &            'Ein=',eincid , bindS(izfiss),  eneutr, 
     &            eincid + Q(1,nnuc), iaf, izf, CSPfis(Nnuc)/TOTcsfis, 
     &            CSPfis(Nnuc)/TOTcsfis*fmultPostfiss,
     &            CSPfis(Nnuc)/TOTcsfis*fmultPrefiss
                 WRITE (12,*) ' '
                 WRITE
     &        (12,'(''  Fiss. CN nucleus '',I3,''-'',A2,''-'',I3)')
     &            INT(Z(nnuc)), SYMb(nnuc), INT(A(nnuc))
                 WRITE
     &     (12,'(''  Partial fission cross section '',G12.5,'' mb'')')
     &            CSPfis(Nnuc)
                 WRITE
     &     (12,'(''  Ratio of partial to total fission '',G12.5)')
     &            CSPfis(Nnuc)/TOTcsfis
C
C----------------Calculating post-fission neutrons above the neutron emission threshold
                 IF (FISspe.eq.1)
     &           CALL get_fragmPFNS_LANL (post_fisn, emiss_en, nspecmax,
     &           eincid, A(Nnuc), Z(Nnuc), ftmp, tequiv)

                 IF (FISspe.eq.2)
     &           CALL get_fragmPFNS (post_fisn, emiss_en, nspecmax,
     &           eincid, A(Nnuc), Z(Nnuc), ftmp, tequiv)

C                subroutine get_fragmPFNS (fragmPFNS, emiss_en, nen_emis,
C                eincid, af, zf, emed, tequiv_maxw)
                 WRITE
     &     (12,'(''  Binding energy of fiss. nucleus '',G12.5)')
     &            Q(1,nnuc)
                 WRITE
     &     (12,'(''  Sum of Bn (from second nucleus) in '',A2, '' chain
     &'',G12.5)') SYMb(nnuc), bindS(izfiss)
                 WRITE
     &     (12,'(''  Effective incident energy '',G12.5,'' (initial ''
     &   ,G12.5,'')'')')  eincid, EIN

                 totspec = 0.d0
                 eavespe = 0.d0
                 fnorm = CSPfis(nnuc)*fmultPostfiss
                 DO ie = 1, nspecmax
                   post_fisn(ie) = post_fisn(ie) * fnorm
C------------------Accumulating total spectrum for neutrons
                   CSEfis(ie,Nejc,nnuc) = CSEfis(ie,Nejc,nnuc) +
     &                                    post_fisn(ie)
                   csepfns(ie,nejc) = csepfns(ie,nejc) +
     &                                CSEfis(ie,Nejc,nnuc)
                   if(CSEfis(ie,Nejc,nnuc).LE.0) cycle
                   DO iang = 1, NDAng
                     ddxs(iang) = 0.d0
                   ENDDO
C------------------Kalbach systematic for PCROSS DDX calculations
C
                   fanisot =
     &                 1.d0-post_fisn(ie)/CSEfis(ie,Nejc,nnuc)
                   iacc = NINT(A(nnuc))
                   izcc = NINT(Z(nnuc))
                   ebind = Q(1,nnuc)
                   uuuu = eincid + ebind
                   eee = emiss_en(ie)
                   ftmp2 = CSEfis(ie,nejc,nnuc)
                   Call Kalbach( iacc, izcc, 0, 1, 0, 1, eincid,
     &                   uuuu, ebind, eee, ftmp2, fanisot, ddxs, NDAng)
                   DO iang = 1, NDAng
                      cseapfns(ie,iang) = cseapfns(ie,iang) +
     &                                    ddxs(iang)
                   ENDDO
C
C------------------Calculating average energy (integral over non-uniform grid !!)
                   IF(ie.gt.1) then
                     deltae = emiss_en(ie)-emiss_en(ie-1)
                     fmed = ( CSEfis(ie,Nejc,nnuc) +
     &                        CSEfis(ie-1,Nejc,nnuc)) * 0.5d0
                     eavespe = eavespe +
     &                  fmed*deltae*(emiss_en(ie)+emiss_en(ie-1))*0.5d0
                     totspec = totspec + fmed*deltae
                   ENDIF
                 ENDDO
                 ftmp2 = 0.d0
                 if (totspec.gt.0.) ftmp2 = eavespe/totspec
                 WRITE (12,
     &       '(''  Prefission <En> for fissioning nucleus  '',G12.5)')
     &              eneutr
                 tequiv1 = 2.d0/3.d0*eneutr
                 WRITE (12,'(''  Equivalent Tmaxwell '',G12.5)')tequiv1
                 WRITE (12,
     &       '(''  Postfission <En> for fissioning nucleus '',
     &            G12.5)') ftmp
                 WRITE (12,'(''  Equivalent Tmaxwell '',G12.5)')tequiv
                 if(totspec.gt.0.) then
                       WRITE (12,
     &       '(''  Prompt fission neutron average <E>      '',G12.5)')
     &             ftmp2
                   tequiv2 = 2.d0/3.d0*ftmp2
                   WRITE (12,'(''  Equivalent Tmaxwell '',G12.5)')
     &             tequiv2
                   DO ie = 1, nspecmax
C                    ftmp = CSPfis(nnuc)*
C    &                   ( fmaxw(emiss_en(ie),tequiv1)*fmultPrefiss +
C    &                     fmaxw(emiss_en(ie),tequiv )*fmultPostfiss )
C                    ftmp = fmaxw(emiss_en(ie),tequiv2)*
C    &                    (fmultPostfiss+fmultPrefiss)*CSPfis(Nnuc)
                     ftmp = fmaxw(emiss_en(ie),tequiv0)*
     &                    (fmultPostfiss+fmultPrefiss)*CSPfis(Nnuc)
                     ratio2maxw(ie) = 0.d0
                     if (ftmp.gt.0)
     &                 ratio2maxw(ie) = CSEfis(ie,Nejc,nnuc)/ftmp
                   ENDDO
                 endif
                 WRITE
     & (12,'(''  Postfission multiplicity (non-weighted) '',F6.3,2x,
     & F6.3)') fmultPostfiss*CSPfis(Nnuc)/TOTcsfis, fmultPostfiss
                 WRITE
     & (12,'(''  Prefission  multiplicity (non-weighted) '',F6.3,2x,
     & F6.3)') fmultPrefiss*CSPfis(Nnuc)/TOTcsfis, fmultPrefiss
                 WRITE
     & (12,'(''  Partial multiplicity (non-weighted)     '',F6.3,2x,
     & F6.3)')  (fmultPostfiss+fmultPrefiss)*CSPfis(Nnuc)/TOTcsfis,
     &          (fmultPostfiss+fmultPrefiss)
                 WRITE (12,*) ' '
                 fniuS = fniuS +
     &              (fmultPostfiss+fmultPrefiss)*CSPfis(Nnuc)/TOTcsfis

               ENDIF ! IF (Nejc.eq.1)
C--------------double the first bin x-sec to preserve integral in EMPEND
C              CSEfis(1,nejc,Nnuc) = CSEfis(1,nejc,Nnuc)*2
               WRITE (12,*) ' '
               WRITE (12,*) ' Spectrum of ', cejectile,
     &               '(z,partfis) from CN ', ' ZAP= ', IZA(Nnuc)
               WRITE (12,*) ' '
               WRITE (12,'('' Energy    mb/MeV'')')
               WRITE (12,*) ' '
               nspecmax = nepfns(nejc)
               IF(nejc.eq.1) THEN
                 DO ie = 1, nspecmax - 1
                   WRITE (12,'(F10.5,E14.5,2x,E14.5)')
     &              emiss_en(ie), CSEfis(ie,nejc,Nnuc), ratio2maxw(ie)
                   WRITE (73,'(F10.5,E14.5,4(2x,E14.5))')
     &              emiss_en(ie), CSEfis(ie,nejc,Nnuc),
     &              post_fisn(ie), ratio2maxw(ie)
                 ENDDO
                 WRITE (12,'(F10.5,E14.5,2x,E14.5)')
     &            emiss_en(nspecmax), CSEfis(nspecmax,nejc,Nnuc),
     &            ratio2maxw(nspecmax)
                 WRITE (12,'(F10.5,E14.5,2x,E14.5)')
     &            emiss_en(nspecmax), 0.d0, 0.d0
               ELSE
                 DO ie = 1, nspecmax - 1
                   WRITE (12,'(F10.5,E14.5,2x,E14.5)')
     &              emiss_en(ie), CSEfis(ie,nejc,Nnuc)
                 ENDDO
                 WRITE (12,'(F10.5,E14.5,2x,E14.5)')
     &            emiss_en(nspecmax), CSEfis(nspecmax,nejc,Nnuc)
                 WRITE (12,'(F10.5,E14.5,2x,E14.5)')
     &            emiss_en(nspecmax), 0.d0
               ENDIF

              ENDIF  ! (A(nnuc).eq.A(1) .and. Z(nnuc).eq.Z(1))
            ENDDO  ! over ejectiles
            nfission = nfission + 1
         ENDIF  !  IF ( TOTcsfis > 0 & CSPfis(nnuc)> 0 & Z(0)==Z(nnuc) )
      ENDDO  ! over decaying nuclei
C-----
C-----PRINTING TOTAL PFNS and PFNM quantities
C-----
      WRITE (12,*) ' '
      WRITE (12,*) ' '
      WRITE (12,'(''  Sum of fission cross section '',G12.4,'' mb'')')
     &       TOTcsfis
      WRITE (6,*) ' '
      WRITE (6,*) ' '
      WRITE (6,'(''  Sum of fission cross section '',G12.4,'' mb'')')
     &       TOTcsfis

C ????????
      IF (FISspe.gt.0 .and. TOTcsfis.gt.0.d0) THEN
         DO nejc = 0, 1         !loop over gamma and neutrons only
            IF (nejc.EQ.0) THEN
              cejectile = 'gammas   '
            ELSE
              cejectile = 'neutrons '
              WRITE
     & (73,'(''*** PFNS from CN '',I3,''-'',A2,''-'',I3,''  Elab '',
     &  F8.4)') INT(Z(1)), SYMb(1), INT(A(1)), EINl
            ENDIF
            WRITE (12,
     &   '(''  Number of fissioning nuclei '',I3,'' at Elab '',F9.5)')
     &      nfission, EINl
            ftmp1 = 0.d0
            ftmp2 = 0.d0
            DO ie = 2, nepfns(nejc)
              deltae = enepfns(ie,nejc)-enepfns(ie-1,nejc)
              fmed = (csepfns(ie,nejc)+csepfns(ie-1,nejc))*0.5
              ftmp1 = ftmp1 + fmed*deltae
              ftmp2 = ftmp2 + fmed*deltae*
     &                (enepfns(ie,nejc)+enepfns(ie-1,nejc))*0.5d0
            ENDDO
            eaverage = 0.d0
            tequiv = 0.d0
            if(ftmp1.gt.0) then
              eaverage = ftmp2/ftmp1
              tequiv = 2.d0/3.d0*eaverage
            endif
            WRITE (12,'(''  PF particle average energy '',G12.5)')
     &        eaverage
            if(nejc.eq.1) THEN
              WRITE (12,'(''  Equivalent Tmaxwell '',G12.5)') tequiv
              fniuEVAL = fniuTH232(EINl)
cc              WRITE(74,
cc     &        '(1X,f8.5,1x,f8.3,3(1x,f7.3))')
cc     &             EINl, eaverage, fniuS, fniuEVAL, tequiv
C
C-------------PFNS normalized to experimental multiplicity for Thorium
C
             if(Z(0).eq.90) then
                WRITE (12,
     &         '(''  Multiplicity (nue) '',F6.3,5x,''('',f6.3,'')'')')
     &         fniuEVAL, fniuS
                WRITE (*,
     &         '(''  Multiplicity (nue) '',F6.3,5x,''('',f6.3,'')'')')
     &         fniuEVAL, fniuS
                WRITE (*, '(''  PFN average energy '',G12.5)')
     &         eaverage
                WRITE (12,
     &   '(''  PFNS normalized to experimental multiplicity '',F6.3)')
     &         fniuEVAL
              else
                WRITE (12,'(''  Multiplicity (nue) '',F6.3,5x,
     &           ''(Th-232 mult. '',f6.3,'')'')')
     &         fniuS, fniuEVAL
                WRITE (*, '(''  Multiplicity (nue) '',F6.3,5x,
     &           ''(Th-232 mult. '',f6.3,'')'')')
     &         fniuS, fniuEVAL
                WRITE (*, '(''  PFN average energy '',G12.5)')
     &         eaverage
              endif
            endif
            ftmp3 = 0.d0
            fnorm = 1.d0
            if (fniuEVAL.gt.0 .and. fniuS.gt.0) fnorm = fniuEVAL/fniuS
            DO ie = 1, nepfns(nejc)
              ratio2maxw(ie) = 0.d0
              IF(nejc.eq.1) THEN
                ftmp = fmaxw(enepfns(ie,nejc),tequiv)
C               ftmp1 is the integral of csepfns(ie,nejc) over energy (ie index)
                if(ftmp.gt.0) ratio2maxw(ie) =
     &                  csepfns(ie,nejc)/(ftmp*ftmp1)
C
C---------------Integral of the Maxwell distribution must be equal 1
C               if(ie.gt.1) then
C                 deltae = enepfns(ie,nejc)-enepfns(ie-1,nejc)
C                 fmed1 = ( fmaxw(enepfns(ie  ,nejc),tequiv) +
C    &                    fmaxw(enepfns(ie-1,nejc),tequiv) )*0.5d0
C                 ftmp3 = ftmp3 + fmed1*deltae
C                endif
                 DO nang = 1, NDAng
                    cseapfns(ie,nang) = cseapfns(ie,nang)*fnorm
                 ENDDO
              ENDIF
              csepfns(ie,nejc) = csepfns(ie,nejc)*fnorm
            ENDDO
C-----------Double the first bin x-sec to preserve integral in EMPEND
C           CSEpfns(1,nejc) = CSEpfns(1,nejc)*2
            WRITE (12,*) ' '
            WRITE (12,*) ' Spectrum of ', cejectile, '(z,fission) '
            IF(nejc.eq.0) THEN
              WRITE (12,*) ' '
              WRITE (12,'('' Energy    mb/MeV'')')
              WRITE (12,*) ' '
            ELSE
              WRITE (12,'(30X,''A     n     g     l     e     s '')')
              WRITE (12,*) ' '
              WRITE (12,'('' Energy   '',8G15.5,/,(10X,8G15.5))')
     &           (ANGles(nang),nang=1,NDAng)
            ENDIF
            nspecmax = nepfns(nejc)
            IF (nejc.eq.1) THEN
              DO ie = 1, nspecmax - 1   ! print DDX PFNS
C               WRITE (12,'(F10.5,E14.5,2x,E14.5)')
C    &          enepfns(ie,nejc), csepfns(ie,nejc), ratio2maxw(ie)
                WRITE (12,'(F10.5,E14.5,7E15.5,/,(9X,8E15.5))')
     &            enepfns(ie,nejc),(cseapfns(ie,nang),nang = 1,NDANG)
                WRITE (73,'(F10.5,E14.5,4(2x,E14.5))')
     &            enepfns(ie,nejc), csepfns(ie,nejc), ratio2maxw(ie)
              ENDDO
              WRITE (12,'(F10.5,E14.5,7E15.5,/,(9X,8E15.5))')
     &            enepfns(nspecmax,nejc),
     &            (cseapfns(nspecmax,nang),nang = 1,NDANG)
              WRITE (12,'(F10.5,E14.5,7E15.5,/,(9X,8E15.5))')
     &            enepfns(nspecmax,nejc),(0.d0,nang = 1,NDANG)
            ELSE
              DO ie = 1, nspecmax - 1
                WRITE (12,'(F10.5,E14.5,2x,E14.5)')
     &          enepfns(ie,nejc), csepfns(ie,nejc)
              ENDDO
              WRITE (12,'(F10.5,E14.5,2x,E14.5)')
     &        enepfns(nspecmax,nejc), csepfns(nspecmax,nejc)
              WRITE (12,'(F10.5,E14.5,2x,E14.5)')
     &        emiss_en(nspecmax), 0.d0
            endif
            WRITE (12,*) ' '
            WRITE (6,*)
            WRITE (6,*)
            WRITE (6,
     &   '('' Number of fissioning nuclei '',I3,'' at Elab '',F9.5)')
     &      nfission, EINl
            if(nejc.eq.1) then
              fniuEVAL =  fniuTH232(EINl)
              WRITE (6,
     &      '('' Multiplicity (nue) '',F6.3,5x,''('',f6.3,'')'')')
     &        fniuEVAL, fniuS
              WRITE (6,'(''  PF particle average energy '',G12.5)')
     &        eaverage
            endif
         ENDDO ! over ejectiles
      ELSE
         WRITE (12,*) ' '
      ENDIF ! if TOTcsfis > 0
C-----
C-----ENDF spectra inclusive representation
C-----
C-----
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
      WRITE (6,*) ' '
      checkXS = checkXS + TOTcsfis
      IF(ABScs.GT.0.) THEN
        WRITE (6,'('' *******************************************'',
     &           23(1H*))')
        WRITE (6,'('' * Incident energy (LAB): '',G12.5,
     &              '' MeV  '')') EINl
        if(TOTred.ne.1.)
     &  WRITE (6,'('' * Total cross section scaled by '',G12.5)')
     &  TOTred    
        WRITE (6,
     &  '('' * Optical model total cross section              '',G12.5,
     &              '' mb  '')') TOTcs*TOTred
        WRITE (6,
     &  '('' * Calculated total cross section                 '',G12.5,
     &              '' mb  '')') CSFus + (SINl+SINlcc)*FCCred +
     &   SINlcont + ElasticCorr + ELAcs    
        WRITE (6,
     &  '('' * Optical model nonelastic cross section (ABScs) '',G12.5,
     &              '' mb  '')') 
     &   (ABScs - (SINl+SINlcc) - SINlcont)*FUSred
     &   + (SINl+SINlcc)*FCCred + SINlcont
          WRITE (6,
     &  '('' * Calculated nonelastic cross section            '',G12.5,
     &              '' mb  '')') 
     &   CSFus + (SINl+SINlcc)*FCCred + SINlcont
        WRITE (6,
     &  '('' * Production cross section (incl.fission)        '',
     &           G12.5,'' mb'')')  checkXS 
        WRITE (6,'('' * Difference: '', F9.5,'' %'')')
     &    100.d0*abs(
     &    ( CSFus + (SINl+SINlcc)*FCCred + SINlcont - checkXS ) )/
     &    ( CSFus + (SINl+SINlcc)*FCCred + SINlcont)
        WRITE (6,'('' * Compound elastic cross section (CE) '',G12.5,
     &              '' mb  '')') 4.*PI*ELCncs
C       WRITE (6,'('' * Reaction cross section - CE '',G12.5,
C    &              '' mb  '')') CSFus + (SINl+SINlcc)*FCCred 
C    &                           + SINlcont - 4.*PI*ELCncs
        if(FUSred.ne.1.)
     &  WRITE (6,'('' * CN formation cross section scaled by '',G12.5
     &  )') FUSred    
        if(FCCred.ne.1.)
     &  WRITE (6,'('' * Direct collective cross section scaled by '',
     &  G12.5)') FCCred    
         WRITE (6,'('' *******************************************'',
     &           23(1H*))')
      ENDIF
      IF(abs(CSFus + (SINl+SINlcc)*FCCred + SINlcont - checkXS)
     &  .GT.0.01*(CSFus + (SINl+SINlcc)*FCCred + SINlcont)) THEN      
        WRITE (6,*)
        WRITE (6,'('' WARNING: Sum of production XS(incl.fission)'')')
        WRITE (6,'('' WARNING: is not equal reaction cross section'')')
        IF((CSFus + (SINl+SINlcc)*FCCred + SINlcont).NE.0.d0)
     &  WRITE (6,'('' WARNING:     difference: '', F9.5,'' %'')')
     &   100.d0*
     &   abs(CSFus + (SINl+SINlcc)*FCCred + SINlcont - checkXS)/
     &                (CSFus + (SINl+SINlcc)*FCCred + SINlcont)
      ENDIF

      IF(TOTred*TOTcs.gt.0.d0 .and.
     &     abs(CSFus + (SINl+SINlcc)*FCCred + SINlcont + ElasticCorr + 
     &     ELAcs - TOTred*TOTcs) .GT. 0.01*TOTred*TOTcs) THEN      
        WRITE (6,*)
        WRITE (6,'('' WARNING: Total XS is not equal'')')
        WRITE (6,'('' WARNING: Elastic + Absorption cross section'')')
        WRITE (6,'('' WARNING:     difference: '', F9.5,'' %'')')
     & 100.d0*abs(ABScs + ElasticCorr  + ELAcs - TOTred*TOTcs)/
     &                 (TOTred*TOTcs)
      ENDIF

      WRITE (6,*)
      IF (IOUt.GT.1) THEN
         csemax = 0.
         DO nejc = 0, NEJcm
            DO i = 1, NDEX
               csemax = DMAX1(CSE(i,nejc,0),csemax)
            ENDDO
         ENDDO
         IF (csemax.GT.0.D0) THEN
            IF (.NOT.EXClusiv) THEN
              WRITE (6,'(//,11X,''**************************'')')
              WRITE (6,'(   11x,'' Inclusive spectra (C.M.)'')')
              WRITE (6,'(   11x,''**************************''/)')
              DO nejc = 0, NEJcm
                CALL AUERST(0,nejc)
              ENDDO
            ENDIF
         ENDIF
      ENDIF
C-----
C-----ENDF spectra printout (inclusive representation)
C-----
              WRITE(74,
     &        '(1X,f8.5,1x,f8.3,4(1x,f7.3))')
     &             EINl, eaverage, fniuS, fniuEVAL, tequiv
      IF (.NOT.EXClusiv) THEN
C--------Print spectra of residues
         reactionx = '(z,x)  '
         DO nnuc = 1, NNUcd    !loop over decaying nuclei
            IF (ENDf(nnuc).EQ.2 .AND. RECoil.GT.0) 
     &        CALL PRINT_RECOIL(nnuc,reactionx)
         ENDDO !over decaying nuclei in ENDF spectra printout
C--------Print inclusive gamma spectrum
         nspec = INT(EMAx(1)/DE) + 2
         IF (nspec.GT.NDECSE - 1) nspec = NDECSE - 1
         WRITE (12,*) ' '
         WRITE (12,*) ' Spectrum of gammas   (z,x)  ZAP=     0'
         WRITE (12,*) ' '
         WRITE (12,'('' Energy    mb/MeV'')')
         WRITE (12,*) ' '
         DO ie = 1, nspec - 1
            WRITE (12,'(F9.4,E15.5)') FLOAT(ie - 1)*DE,
     &         max(0.d0,CSE(ie,0,0))
         ENDDO
C--------Exact endpoint
         WRITE (12,'(F9.4,E15.5)') EMAx(1), max(0.d0,CSE(nspec,0,0))
         WRITE (12,'(F9.4,E15.5)') EMAx(1), 0.d0
C--------Print inclusive spectra of ejectiles
C--------neutrons
         nspec = INT((EMAx(1) - Q(1,1))/DE) + 2
         IF (nspec.gt.0) THEN
          IF (nspec.GT.NDECSE - 1) nspec = NDECSE - 1
          WRITE (12,*) ' '
          WRITE (12,*) ' Spectrum of neutrons (z,x)  ZAP=     1'
          WRITE (12,*) ' '
          WRITE (12,'('' Energy    mb/MeV'')')
          WRITE (12,*) ' '
          DO ie = 1, nspec - 1
            WRITE (12,'(F9.4,E15.5)') FLOAT(ie - 1)*DE,
     &         max(0.d0,CSE(ie,1,0))
          ENDDO
C---------Exact endpoint
          WRITE (12,'(F9.4,E15.5)') EMAx(1) - Q(1,1),
     &                              max(0.d0,CSE(nspec,1,0))
          WRITE (12,'(F9.4,E15.5)') EMAx(1) - Q(1,1), 0.d0
         ENDIF
C--------protons
         nspec = INT((EMAx(1) - Q(2,1))/DE) + 2
         IF(nspec.gt.0) then
          IF (nspec.GT.NDECSE - 1) nspec = NDECSE - 1
          WRITE (12,*) ' '
          WRITE (12,*) ' Spectrum of protons  (z,x)  ZAP=  1001'
          WRITE (12,*) ' '
          WRITE (12,'('' Energy    mb/MeV'')')
          WRITE (12,*) ' '
          DO ie = 1, nspec - 1
            WRITE (12,'(F9.4,E15.5)') FLOAT(ie - 1)*DE,
     &         max(0.d0,CSE(ie,2,0))
          ENDDO
C---------Exact endpoint
          WRITE (12,'(F9.4,E15.5)') EMAx(1) - Q(2,1),
     &                              max(0.d0,CSE(nspec,2,0))
          WRITE (12,'(F9.4,E15.5)') EMAx(1) - Q(2,1), 0.d0
         ENDIF
C--------alphas
         nspec = INT((EMAx(1) - Q(3,1))/DE) + 2
         IF(nspec.gt.0) then
          IF (nspec.GT.NDECSE - 1) nspec = NDECSE - 1
          WRITE (12,*) ' '
          WRITE (12,*) ' Spectrum of alphas   (z,x)  ZAP=  2004'
          WRITE (12,*) ' '
          WRITE (12,'('' Energy    mb/MeV'')')
          WRITE (12,*) ' '
          DO ie = 1, nspec - 1
            WRITE (12,'(F9.4,E15.5)') FLOAT(ie - 1)*DE,
     &         max(0.d0,CSE(ie,3,0))
          ENDDO
C---------Exact endpoint
          WRITE (12,'(F9.4,E15.5)') EMAx(1) - Q(3,1),
     &                              max(0.d0,CSE(nspec,3,0))
          WRITE (12,'(F9.4,E15.5)') EMAx(1) - Q(3,1), 0.d0
         ENDIF
C--------light ions
         IF (NDEJC.EQ.4) THEN
           nspec = INT((EMAx(1) - Q(NDEJC,1))/DE) + 2
           IF(nspec.gt.0) then
             IF (nspec.GT.NDECSE - 1) nspec = NDECSE - 1
             WRITE (12,*) ' '
             WRITE (12,
     &'(''  Spectrum of  '',I1,''-'',A2,4X,A5,I7,''(LI,x)'')')
     &INT(AEJc(NDEJC)), SYMbe(NDEJC), ' ZAP=', IZAejc(NDEJC)
             WRITE (12,*) ' '
             WRITE (12,'('' Energy    mb/MeV'')')
             WRITE (12,*) ' '
             DO ie = 1, nspec - 1
               WRITE (12,'(F9.4,E15.5)') FLOAT(ie - 1)*DE,
     &                                   max(0.d0,CSE(ie,NDEJC,0))
             ENDDO
C------------exact endpoint
             WRITE (12,'(F9.4,E15.5)') EMAx(1) - Q(NDEJC,1),
     &                                 max(0.d0,CSE(nspec,NDEJC,0))
             WRITE (12,'(F9.4,E15.5)') EMAx(1) - Q(NDEJC,1), 0.d0
           ENDIF
         ENDIF
         WRITE (12,*) ' '
      ENDIF
C-----End of ENDF spectra (inclusive)
      IF (EXClusiv .AND. EINl.GE.1.d0) THEN
         WRITE (6,*) ' '
         WRITE (6,*) '----------------------------------------------'
         WRITE (6,*) 'Test - integrals of portions of DDX spectra'
         WRITE (6,'('' Energy'',12x,'' gamma '',9x,''neutron'',
     &                                10x,''proton '',8x,'' alpha'')')
         WRITE (6,*) '----------------------------------------------'
         DO ispec = 1, NEX(1) + 10
            controlg = 0
            controln = 0
            controlp = 0
            controla = 0
            DO nnuc = 1, NNUcd
               IF(ENDf(nnuc).NE.1) CYCLE
               controlg = controlg + POPcseaf(0,0,ispec,INExc(nnuc))
               controln = controln + POPcseaf(0,1,ispec,INExc(nnuc))
               controlp = controlp + POPcseaf(0,2,ispec,INExc(nnuc))
               controla = controla + POPcseaf(0,3,ispec,INExc(nnuc))
            ENDDO
            WRITE (6,'(5g15.5)') (ispec - 1)*DE, controlg,
     &                       controln, controlp, controla
         ENDDO
      ENDIF
 1155 IF( FITomp.GE.0 ) THEN
        READ (5,'(A36)') nextenergy
        IF(nextenergy(1:1).EQ.'$') THEN
           READ(nextenergy,'(1x,A6,G10.5,4I5)') keyname, val, ikey1,
     &         ikey2, ikey3, ikey4
           CALL OPTIONS(keyname, val, ikey1, ikey2, ikey3, ikey4, 1)
           GO TO 1155
        ELSE
           READ(nextenergy,'(G15.5)') EIN
        ENDIF
       ELSE
        READ (5,*) EIN, NDAng, ICAlangs
          IF(NDAng.lt.2) THEN
            NDAng=2
            ANGles(1) = 0.
            ANGles(2) = 180.
           ELSE
            READ (5,*) (ANGles(na),na=1,NDAng)
           ENDIF
          NANGela=NDAng
          IF(NANgela.GT.NDAngecis) THEN
          WRITE(6,*)
     &         'FATAL: INCREASE NDANGECIS IN dimension.h UP TO ',NANgela
          STOP 'FATAL: INCREASE NDAngecis IN dimension.h'
        ENDIF
       ENDIF
      IF (EIN.LT.0.0D0) THEN
         IF (FILevel) CLOSE (14)
         WRITE (12,*) ' '
         WRITE (6,*) ' '
         WRITE (6,*) ' CALCULATIONS COMPLETED SUCCESSFULLY'
         CLOSE (5)
         CLOSE (11)
         CLOSE (12)
         CLOSE (13)
         CLOSE (14)
         CLOSE (15,STATUS = 'delete')
         CLOSE (16,STATUS = 'delete')
         CLOSE (23)
         CLOSE (24)
         CLOSE (26)
         CLOSE (29)
         CLOSE (33)
         CLOSE (40)
         CLOSE (41)
         CLOSE (66,STATUS = 'delete')
         CLOSE (98)
         CLOSE (73)
         CLOSE (74)
         WRITE (*,*) '.'
         CALL THORA(6)
         CLOSE (6)
C--------Saving random seeds
         ftmp = grand()
         OPEN(94,file='R250SEED.DAT',status='UNKNOWN')
         write(94,*)  indexf, indexb
         Do i = 1, 250
          write(94,*) buffer(i)
         ENDDO
         CLOSE(94)
         RETURN
      ENDIF
      IF(EIN.LT.epre) THEN
         WRITE(6,*) EIN,epre
         WRITE(6,*) 'FATAL: INPUT ENERGIES OUT OF ORDER !!'
         WRITE(6,*) 'FATAL: CHECK YOUR INPUT FILE'
         PAUSE 'FATAL: INPUT ENERGIES OUT OF ORDER !!'
         STOP
      ENDIF
      epre = EIN
C-----
C-----
      IF(FITomp.GE.0) THEN
        NANgela = 91
        NDAng   = 91
C       IF(EIN.GT.20. .AND. EIN.LE.50.) THEN
C         NANgela = 73
C         NDAng   = 73
C       ENDIF
C       IF(EIN.GT.50.) THEN
C         NANgela = 91
C         NDAng   = 91
C       ENDIF
C       IF(NANgela.GT.NDAngecis) THEN
C         WRITE(6,*)
C    &        'FATAL: increase NDAngecis in dimension.h up to ',NANgela
C        STOP 'FATAL: increase NDAngecis in dimension.h'
C       ENDIF
C-------Set angles for inelastic calculations
        da = 180.0/(NDANG - 1)
        DO na = 1, NDANG
          ANGles(na) = (na - 1)*da
        ENDDO
      ENDIF
      DO na = 1, NDANG
        CANgler(na) = COS(ANGles(NDANG - na + 1)*PI/180.)
        SANgler(na) = SQRT(1.D0 - CANgler(na)**2)
      ENDDO
      FIRst_ein = .FALSE.
      GOTO 1300
99070 FORMAT (I12,F10.5,I5,F8.1,G15.6,I3,7(I4,F7.4),:/,(53X,7(I4,F7.4)))
99075 FORMAT (1X,F5.2,12G10.3)
      END
C
C
      SUBROUTINE GET_RECOIL(Ke,Nnuc)
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
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C Dummy arguments
C
      INTEGER Ke, Nnuc
C
C Local variables
C
      DOUBLE PRECISION coef, dang, erecejc, erecod, erecoil, erecpar,
     &                 exqcut, recorr, sumnor, weight, ares, zres
      REAL FLOAT
      INTEGER icse, ie, il, ire, irec, na, nejc, nnur, izares, iloc
C
C
C-----Normalize recoil spectrum of the parent
      sumnor = 0.0
      DO ire = 1, NDEREC
         sumnor = sumnor + RECcse(ire,Ke,Nnuc)
      ENDDO
      IF (sumnor.NE.0.0D0) THEN
         DO ire = 1, NDEREC
            RECcse(ire,Ke,Nnuc) = RECcse(ire,Ke,Nnuc)/sumnor
         ENDDO
      ENDIF
      dang = PI/FLOAT(NDANG - 1)
      coef = dang/DERec/2.0
      DO nejc = 1, NEJcm   !over ejectiles
         ares = A(nnuc) - AEJc(nejc)
         zres = Z(nnuc) - ZEJc(nejc)
C--------Residual nuclei must be heavier than alpha
         if(ares.le.4. and. zres.le.2.) cycle
         izares = INT(1000.0*zres + ares)
         CALL WHERE(izares,nnur,iloc)
         if(iloc.eq.1) CYCLE
C--------Decay to continuum
C--------recorr is a recoil correction factor that
C--------divides outgoing energies
         recorr = AMAss(Nnuc)/EJMass(nejc)
         exqcut = EX(Ke,Nnuc) - Q(nejc,Nnuc) - ECUt(nnur)
         DO ie = 1, NDECSE !over ejec. energy (daughter excitation)
            icse = (exqcut - (ie - 1)*DE)/DE + 1.001
C-----------Daughter bin
            IF (icse.LE.0) GOTO 50
            erecejc = (ie - 1)*DE/recorr
            DO ire = 1, NDEREC          !over recoil spectrum
               erecpar = (ire - 1)*DERec
               DO na = 1, NDANG
                  erecoil = erecejc + erecpar
                  erecoil = erecoil + 2.0*SQRT(erecejc*erecpar)
     &                      *CANgler(na)
                  irec = erecoil/DERec + 1.001
                  weight = (erecoil - (irec - 1)*DERec)/DERec
                  IF (irec + 1.GT.NDEREC) GOTO 20
                  RECcse(irec,icse,nnur) = RECcse(irec,icse,nnur)
     &               + RECcse(ire,Ke,Nnuc)*AUSpec(ie,nejc)
     &               *(1.0 - weight)*SANgler(na)*coef
                  RECcse(irec + 1,icse,nnur)
     &               = RECcse(irec + 1,icse,nnur) + RECcse(ire,Ke,Nnuc)
     &               *AUSpec(ie,nejc)*weight*SANgler(na)*coef
               ENDDO                  !over angles
   20       ENDDO                  !over recoil spectrum
         ENDDO                  !over  daugther excitation
C--------Decay to discrete levels (stored with icse=0)
   50    exqcut = exqcut + ECUt(nnur)
         DO il = 1, NLV(nnur)
            erecod = exqcut - ELV(il,nnur)   !emission energy
            erecod = erecod/recorr
            IF (erecod.LT.0) GOTO 100
            DO ire = 1, NDEREC      !over recoil spectrum
               DO na = 1, NDANG !over angles
                  erecoil = (ire - 1)*DERec + erecod +
     &                       2.0*SQRT((ire - 1)*DERec*erecod)
     &                      *CANgler(na)
                  irec = erecoil/DERec + 1.001
                  weight = (erecoil - (irec - 1)*DERec)/DERec
                  IF (irec.GT.NDEREC) GOTO 60
                  RECcse(irec,0,nnur) = RECcse(irec,0,nnur)
     &                                  + RECcse(ire,Ke,Nnuc)
     &                                  *REClev(il,nejc)*(1.0 - weight)
     &                                  *SANgler(na)*coef
c------------------------
c                 IF(irec.EQ.5 .AND. RECcse(irec,0,nnur).GT.0
c    &               .AND.na.EQ.10) THEN
c                 WRITE(6,*) '       Parent bin', Ke
c                 WRITE(6,*) 'Recoil bin', ire
c                 WRITE(6,*) 'Erecoil ', erecoil, erecod, nnuc
c                 WRITE(6,*) 'RECcse, RECcse par, REClev',
c    &            RECcse(irec,0,nnur),RECcse(ire,Ke,Nnuc),
c    &            REClev(il,nejc)
c                 ENDIF
c------------------------
                  IF (irec + 1.GT.NDEREC) GOTO 60
                  RECcse(irec + 1,0,nnur) = RECcse(irec + 1,0,nnur)
     &               + RECcse(ire,Ke,Nnuc)*REClev(il,nejc)
     &               *weight*SANgler(na)*coef
               ENDDO                  !over angles
   60       ENDDO                  !over recoil spectrum
         ENDDO                  !over levels
  100 ENDDO                  !over ejectiles
C-----
C-----Parent recoil spectrum after gamma decay
C-----
      nnur = Nnuc
      nejc = 0
C-----gamma decay to continuum
      DO ie = 1, NDECSE !over ejec. energy (daughter excitation)
         icse = (EX(Ke,Nnuc) - (ie - 1)*DE - ECUt(nnur))/DE + 1.001
C--------!daughter bin
         DO irec = 1, NDEREC         !over recoil spectrum
            IF (icse.GT.0) RECcse(irec,icse,nnur)
     &          = RECcse(irec,icse,nnur) + RECcse(irec,Ke,Nnuc)
     &          *AUSpec(ie,0)/DERec
         ENDDO                  !over recoil spectrum
      ENDDO                  !over  daugther excitation
C-----gamma decay to discrete levels (stored with icse=0)
      DO il = 1, NLV(nnur)
         DO ire = 1, NDEREC             !over recoil spectrum
            RECcse(ire,0,nnur) = RECcse(ire,0,nnur)
     &                           + RECcse(ire,Ke,Nnuc)*REClev(il,nejc)
     &                           /DERec
         ENDDO                  !over recoil spectrum
      ENDDO                  !over levels
      END

      SUBROUTINE PRINT_RECOIL(Nnuc,React)
C-----
C-----Prints recoil spectrum of nnuc residue
C-----
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C Dummy arguments
C
      INTEGER Nnuc
      CHARACTER*21 React
C
C Local variables
C
      DOUBLE PRECISION corr
      REAL FLOAT
      INTEGER ie, ilast
      IF (CSPrd(Nnuc).GT.0.0D0) THEN
C-------Normalize recoil spectra to remove eventual inaccuracy
C-------due to numerical integration of angular distributions
C-------and find last non-zero cross section for printing
         corr = 0.0
         ilast = 0
         DO ie = 1, NDEREC
            corr = corr + RECcse(ie,0,Nnuc)
            IF (RECcse(ie,0,Nnuc).NE.0) ilast = ie
         ENDDO
         IF (corr.EQ.0) RETURN
C        WRITE(6,*)'nnuc, rec, cs',nnuc,corr*DERec,CSPrd(nnuc)
         corr = CSPrd(Nnuc)/corr/DERec
         ilast = MIN(ilast + 1,NDEREC)
         DO ie = 1, ilast
            RECcse(ie,0,Nnuc) = RECcse(ie,0,Nnuc)*corr
         ENDDO
         WRITE (12,*) ' '
         WRITE (12,'(A23,A7,A4,I6,A6,F12.5)') '  Spectrum of recoils  ',
     &          React, 'ZAP=', IZA(Nnuc), ' mass=', AMAss(Nnuc)
         WRITE (12,*) ' '
         WRITE (12,'('' Energy    mb/MeV'')')
         WRITE (12,*) ' '
         DO ie = 1, ilast
            WRITE (12,'(F9.4,E15.5)') FLOAT(ie - 1)*DERec,
     &                                RECcse(ie,0,Nnuc)
         ENDDO
C--------Print end point again with 0 xs for consistency with particle spectra
C        WRITE (12,'(F9.4,E15.5)') FLOAT(ilast - 1)*DERec,
C    &                             RECcse(ilast + 1,0,Nnuc)
         WRITE (12,'(F9.4,E15.5)') FLOAT(ilast - 1)*DERec,0.d0
         IF (ABS(1.0 - corr).GT.0.01D0 .AND. CSPrd(Nnuc).GT.0.001D0)
     &       THEN
            WRITE (6,*) ' '
            WRITE (6,*) 'WARNING:  Ein = ', EIN, ' MeV ZAP = ',
     &                  IZA(Nnuc), ' from ', React
            WRITE (6,*) 'WARNING: x-section balance in recoils '
            WRITE (6,*) 'WARNING: difference = ', (1.0 - corr)*100.0,
     &                  '%'
            WRITE (6,*) 'WARNING: production cross section = ',
     &                  CSPrd(Nnuc)
            WRITE (6,*) ' '
         ENDIF
      ENDIF
      END

      SUBROUTINE FISCROSS(Nnuc,Ke,Ip,Jcn,Sumfis,Sumfism,Dencomp,Aafis,
     &                    Ifluct)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C COMMON variables
C
      DOUBLE PRECISION AFIsm(NFMOD), CSFism(NFMOD), DEFbm(NFMOD),
     &                 DELtafism(NFMOD), DEStepm(NFMOD), EFBm(NFMOD),
     &                 EFDism(NFTRANS,NFMOD), GAMmafism(NFMOD),
     &                 HM(NFTRANS,NFMOD), PFIso, RFIso,
     &                 ROFism(0:NFISENMAX,NDLW,NFMOD), SHCfism(NFMOD),
     &                 TABs, TDIr, TDIr23, TDIrect, TDIrm(NFMOD),
     &                 TF(NFPARAB), TFB, TFBm(NFMOD), TFIso, TG2, TGIso,
     &                 TISo, UGRidf(0:NFISENMAX,NFMOD), WFIsm(NFMOD),
     &                 XMInnm(NFMOD)
      INTEGER BFFm(NFMOD), NRBinfism(NFMOD)
      COMMON /FISSMOD/ ROFism, HM, EFDism, UGRidf, EFBm, XMInnm, AFIsm,
     &                 DEFbm, SHCfism, DELtafism, GAMmafism, WFIsm,
     &                 BFFm, NRBinfism, DEStepm, TFBm, TDIrm, CSFism,
     &                 TFB, TDIrect
      COMMON /FIS_ISO/ TFIso, TGIso, TISo, RFIso, PFIso
      COMMON /IMAG  / TF, TDIr, TABs, TDIr23, TG2
C
C Dummy arguments
C
      DOUBLE PRECISION Aafis, Dencomp, Sumfis
      INTEGER Ifluct, Ip, Jcn, Ke, Nnuc
      DOUBLE PRECISION Sumfism(NFMOD)
C
C Local variables
C
      DOUBLE PRECISION bbfis, cota, cota1, cotaexp, tout
      INTEGER INT
      INTEGER k, kk, m
      IF (Ifluct.EQ.0) Dencomp = DENhf
      Aafis = 0.
      cota1 = 0.d0
      IF (FISmod(Nnuc).EQ.0.) THEN
         CALL FISFIS(Nnuc,Ke,Ip,Jcn,Sumfis,0)
         IF (FISopt(Nnuc).GT.0.) THEN
            IF (NRWel.EQ.1) cota1 = (TF(1) + TF(2) + TG2)/2
            IF (NRWel.EQ.2) cota1 = (TF(1) + TDIr23 + TG2)/2
         ENDIF
      ENDIF
      IF (FISmod(Nnuc).GT.0.) THEN
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
               ROFis(kk,Jcn,2) = ROFism(kk,Jcn,m)
            ENDDO
            CALL FISFIS(Nnuc,Ke,Ip,Jcn,Sumfis,m)
            TFBm(m) = TF(2)
            TFB = TFB + TFBm(m)
            IF (FISopt(Nnuc).GT.0.) THEN
               TDIrm(m) = TDIr
               TDIrect = TDIrect + TDIr
            ENDIF
            IF (FISopt(Nnuc).GT.0.) cota1 = (TF(1) + TFB + TG2)/2
         ENDDO
      ENDIF
      IF (cota1.LE.EXPmax) THEN
         cotaexp = EXP( - cota1)
      ELSE
         cotaexp = 0.D0
      ENDIF
      cota = (1 + cotaexp**2)/(1 - cotaexp**2 + 0.00000001)
      IF (FISmod(Nnuc).EQ.0. .AND. FISopt(Nnuc).GT.0.) THEN
         IF (NRBarc.EQ.2 .AND. NRWel.EQ.1) tout = TF(2)
         IF (NRBar.EQ.5) tout = TDIr23
         IF (tout*TABs.GT.0.) THEN
            bbfis = (TDIr + Dencomp)*(TF(1) + tout + TG2)
     &              /(TABs*(tout + TG2))
            Aafis = (1. + bbfis**2 + 2*bbfis*cota)**( - 0.5)
         ELSE
            Aafis = 0.
         ENDIF
         PFIso = 0.
         IF (tout.GT.0 .AND. TG2.GT.0.) PFIso = (TDIr + Dencomp)
     &       *(RFIso - 1.)*TG2*Aafis/(tout + TG2)
      ENDIF
      IF (FISmod(Nnuc).GT.0.) THEN
         IF (TFB*TABs.GT.0.) THEN
            bbfis = (TDIrect + Dencomp)*(TF(1) + TFB)/(TABs*TFB)
            Aafis = (1. + bbfis**2 + 2*bbfis*cota)**( - 0.5)
         ELSE
            Aafis = 0.
         ENDIF
         DO m = 1, INT(FISmod(Nnuc)) + 1
            IF ((TF(1) + TFB).GT.0.) THEN
               Sumfism(m) = TF(1)*TFBm(m)/(TF(1) + TFB)
            ELSE
               Sumfism(m) = 0.
            ENDIF
         ENDDO
      ENDIF
      END

      SUBROUTINE XSECT(Nnuc,M,Xnor,Sumfis,Sumfism,Ke,Ipar,Jcn,Dencomp,
     &                 Aafis)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C COMMON variables
C
      DOUBLE PRECISION AFIsm(NFMOD), CSFism(NFMOD), DEFbm(NFMOD),
     &                 DELtafism(NFMOD), DEStepm(NFMOD), EFBm(NFMOD),
     &                 EFDism(NFTRANS,NFMOD), GAMmafism(NFMOD),
     &                 HM(NFTRANS,NFMOD), PFIso, RFIso,
     &                 ROFism(0:NFISENMAX,NDLW,NFMOD), SHCfism(NFMOD),
     &                 TABs, TDIr, TDIr23, TDIrect, TDIrm(NFMOD),
     &                 TF(NFPARAB), TFB, TFBm(NFMOD), TFIso, TG2, TGIso,
     &                 TISo, UGRidf(0:NFISENMAX,NFMOD), WFIsm(NFMOD),
     &                 XMInnm(NFMOD)
      INTEGER BFFm(NFMOD), NRBinfism(NFMOD)
      COMMON /FISSMOD/ ROFism, HM, EFDism, UGRidf, EFBm, XMInnm, AFIsm,
     &                 DEFbm, SHCfism, DELtafism, GAMmafism, WFIsm,
     &                 BFFm, NRBinfism, DEStepm, TFBm, TDIrm, CSFism,
     &                 TFB, TDIrect
      COMMON /FIS_ISO/ TFIso, TGIso, TISo, RFIso, PFIso
      COMMON /IMAG  / TF, TDIr, TABs, TDIr23, TG2
C
C Dummy arguments
C
      DOUBLE PRECISION Aafis, Dencomp, Sumfis, Xnor
      INTEGER Ipar, Jcn, Ke, M, Nnuc
      DOUBLE PRECISION Sumfism(3)
C
C Local variables
C
      INTEGER INT
      INTEGER nejc, nnur, izares, iloc
      DOUBLE PRECISION xnorfis, ares, zres, fisXS
      Dencomp = DENhf - Sumfis
      IF (FISsil(Nnuc) .AND. FISopt(Nnuc).GT.0. .AND. FISshi(Nnuc)
     &    .NE.1.) THEN
         IF (FISmod(Nnuc).EQ.0.) THEN
            IF ((Dencomp + TDIr).GT.0.) THEN
               xnorfis = Xnor*DENhf/(Dencomp + TDIr)
            ELSE
               xnorfis = 0.
            ENDIF
C-----------Fission
            fisXS = xnorfis*(TDIr + Dencomp*Aafis + PFIso)
            CSFis = CSFis + fisXS
            IF (ENDf(Nnuc).EQ.1 .AND. fisXS.NE.0.0D+0)     
     &          CALL EXCLUSIVEC(Ke,0, -1,Nnuc,0,fisXS)
         ENDIF
         IF (FISmod(Nnuc).GT.0.) THEN
            IF ((Dencomp + TDIrect).GT.0.) THEN
               xnorfis = Xnor*DENhf/(Dencomp + TDIrect)
            ELSE
               xnorfis = 0.
            ENDIF
            DO M = 1, INT(FISmod(Nnuc)) + 1
               CSFism(M) = CSFism(M)
     &                     + xnorfis*(TDIrm(M)*(1. - Aafis) + TFBm(M)
     &                     *Aafis*(Dencomp + TDIrect)/TFB)
            ENDDO
C
C-----------Multimodal should be updated to allow for PFNS calculation !!!!
C
         ENDIF
C--------particles
         DO nejc = 1, NEJcm
            ares = A(nnuc) - AEJc(nejc)
            zres = Z(nnuc) - ZEJc(nejc)
C           residual nuclei must be heavier than alpha
            if(ares.le.4. and. zres.le.2.) cycle
            izares = INT(1000.0*zres + ares)
            CALL WHERE(izares,nnur,iloc)
            if(iloc.eq.1) CYCLE
            CALL ACCUM(Ke,Nnuc,nnur,nejc,Xnor)
            CSEmis(nejc,Nnuc) = CSEmis(nejc,Nnuc) + xnorfis*SCRtem(nejc)
     &                          *(1 - Aafis)
         ENDDO
C--------gammas
         CALL ACCUM(Ke,Nnuc,Nnuc,0,Xnor)
         CSEmis(0,Nnuc) = CSEmis(0,Nnuc) + xnorfis*SCRtem(0)*(1 - Aafis)
         POP(Ke,Jcn,Ipar,Nnuc) = 0.0
         RETURN
      ENDIF
C-----No subbarrier effects
C-----particles
      DO nejc = 1, NEJcm
         ares = A(nnuc) - AEJc(nejc)
         zres = Z(nnuc) - ZEJc(nejc)
C        residual nuclei must be heavier than alpha
         if(ares.le.4. and. zres.le.2.) cycle
         izares = INT(1000.0*zres + ares)
         CALL WHERE(izares,nnur,iloc)
         if(iloc.eq.1) CYCLE
         CALL ACCUM(Ke,Nnuc,nnur,nejc,Xnor)
         CSEmis(nejc,Nnuc) = CSEmis(nejc,Nnuc) + Xnor*SCRtem(nejc)
      ENDDO
C-----gammas
      CALL ACCUM(Ke,Nnuc,Nnuc,0,Xnor)
      CSEmis(0,Nnuc) = CSEmis(0,Nnuc) + Xnor*SCRtem(0)
      POP(Ke,Jcn,Ipar,Nnuc) = 0.0
C-----fission
      IF (FISmod(Nnuc).EQ.0.) THEN
           CSFis = CSFis + Sumfis*Xnor
           fisXS = Sumfis*Xnor
           IF (ENDf(Nnuc).EQ.1 .AND. fisXS.NE.0.0D+0)
     &      CALL EXCLUSIVEC(Ke,0, -1,Nnuc,0,fisXS)
        ENDIF
C
C-----------Multimodal should be updated to allow for PFNS calculation !!!!
C
      IF (FISmod(Nnuc).GT.0.) THEN
         DO M = 1, INT(FISmod(Nnuc)) + 1
            CSFism(M) = CSFism(M) + Sumfism(M)*Xnor
         ENDDO
      ENDIF
      END
