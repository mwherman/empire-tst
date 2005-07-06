Ccc   * $Author: Capote $
Ccc   * $Date: 2005-07-06 20:04:56 $
Ccc   * $Id: main.f,v 1.117 2005-07-06 20:04:56 Capote Exp $
C
      PROGRAM EMPIRE
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:ppu*
Ccc   *                         E M P I R E                              *
Ccc   *                                                                  *
Ccc   *     Main of the EMPIRE code.                                     *
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
     &                 TFIso, TGIso, TISo, TOTcs, SINlcc,
     &                 UGRidf(0:NFISENMAX,NFMOD), WFIsm(NFMOD),
     &                 XMInnm(NFMOD)
      INTEGER BFFm(NFMOD), NRBinfism(NFMOD)
      INTEGER*4 INDexf, INDexb, BUFfer(250)
      COMMON /ECISXS/ ELAcs, TOTcs, ABScs, SINl, SINlcc
      COMMON /FISSMOD/ ROFism, HM, EFDism, UGRidf, EFBm, XMInnm, AFIsm,
     &                 DEFbm, SHCfism, DELtafism, GAMmafism, WFIsm,
     &                 BFFm, NRBinfism, DEStepm, TFBm, TDIrm, CSFism,
     &                 TFB, TDIrect
      COMMON /FIS_ISO/ TFIso, TGIso, TISo, RFIso, PFIso
      COMMON /R250COM/ INDexf,INDexb,BUFfer
C
C Local variables
C
      DOUBLE PRECISION aafis, ares, atotsp, coef, controln, controlp,
     &                 corrmsd, csemax, csemist, csmsdl, csum, cturbo,
     &                 dang, debinhms, ded, delang, dencomp, echannel,
     &                 ecm, elada(NDAngecis), emeda, emedg,
     &                 emedh, emedn, emedp, erecoil, espec, espmax,
     &                 epre, ftmp, gamfis, gamt, gang, gtotsp, htotsp,
     &                 piece, pope, poph, popl, popleft, poplev,
     &                 popread, poptot, ptotsp, q2, q3, qmax, qstep,
     &                 recorp, sgamc, spdif, spdiff, stauc,
     &                 step, sum, sumfis, sumfism(NFMOD), tauf, taut,
     &                 totemis, weight, xcse, xizat, xnhms, xnl,
     &                 xnor, xtotsp, xsinlcont, xsinl, zres, angstep,
     &                 deform(NDCOLLEV), cseaprnt(ndecse,ndangecis),
     &                 checkXS
      CHARACTER*9 cejectile
      CHARACTER*6 ctldir, keyname
      CHARACTER*20 ctmp20
      CHARACTER*36 nextenergy
      DOUBLE PRECISION DMAX1, val
      REAL FLOAT
      INTEGER i, ia, iad, iam, iang, iang1, ib, icalled, 
     &        icsh, icsl, ie, iizaejc, il, ilev, iloc, ilv, imaxt,
     &        imint, ip, ipar, irec, ispec, itimes, its, iz, izares, j,
     &        jcn, jj, ke, kemax, kemin, kk, ltrmax, m, mt2, mt649,
     &        mt849, mt91, nang, nbr, ncoll, nejc, nejcec, nelang, nnuc,
     &        nnur, nnurec, nnurn, nnurp, nrbarc1, nspec,
     &        itemp(NDCOLLEV), ikey1, ikey2, ikey3, ikey4
      INTEGER INT, MIN0, NINT
      LOGICAL nvwful, fexist
      CHARACTER*21 reactionx
      INCLUDE 'io.h'
      DATA ctldir/'../TL/'/,epre/0.0/
      icalled = 0
      CALL THORA(6)
C-----
C-----Read and prepare input data
C-----
 1300 CALL INPUT
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
C-----Prepare Giant Resonance parameters - systematics
C-----
      CALL ULM(0)
C-----
C-----Calculate reaction cross section and its spin distribution
C-----
      CALL MARENG(0,0)

C     Total cross section is set to absorption cross section
C        for photon induced reactions (to process them in EMPEND)
C
      IF (INT(AEJc(0)).EQ.0) TOTcs = CSFus

C-----locate position of the target among residues
      CALL WHERE(IZA(1) - IZAejc(0),nnurec,iloc)
C-----locate position of the projectile among ejectiles
      CALL WHEREJC(IZAejc(0),nejcec,iloc)
C
      WRITE (ctmp20,'(i3.3,i3.3,1h_,i3.3,i3.3,1h_,i6.6)') INT(ZEJc(0)),
     &       INT(AEJc(0)), INT(Z(0)), INT(A(0)), INT(EINl*1000)
C     TOTcs, ABScs, ELAcs are initialized within MARENG()
      xsinlcont = 0.d0
      xsinl = 0.d0
      checkXS = 0.d0
C     For resolution function (Spreading levels in the continuum)
      isigma = 0
      IF(WIDcoll.GT.0.d0)
     &   isigma = INT((0.02d0  + sqrt(EINl)*WIDcoll)/DE + 1.0001)
      ncoll = 0
      nelang = NANgela
      ecm = EINl - EIN
      dang = PI/FLOAT(nelang - 1)
      angstep = 180.d0/(NANgela-1)
      gang = 180.d0/(NDAng-1)
C-----
C-----Get ECIS results
C-----
      OPEN (45,FILE = (ctldir//ctmp20//'.ANG'),STATUS = 'OLD',
     &      ERR = 1400)
      READ (45,*,END = 1400)   ! To skip first line <ANG.DIS.> ..
      READ (45,*,END = 1400)   ! To skip level identifier line
      DO iang = 1, nelang
         READ (45,'(7x,E12.5)',END = 1400) elada(iang)
      ENDDO
      IF (DIRect.NE.0) THEN
         OPEN (46,FILE = (ctldir//ctmp20//'.ICS'),STATUS = 'OLD',
     &         ERR = 1400)
         READ (46,*,END = 1400) ! To skip first line <INE.C.S.> ..
C--------get and add inelastic cross sections (including double-differential)
         DO i = 2, ND_nlv
            ilv = ICOller(i)
            IF (ilv.LE.NLV(nnurec)) THEN
C------------ADDING INELASTIC TO DISCRETE LEVELS
             echannel = EX(NEX(1),1) - Q(nejcec,1) - ELV(ilv,nnurec)
C------------avoid reading closed channels
             IF (echannel.GE.0.0001) THEN
               xcse = echannel/DE + 1.0001
               icsl = INT(xcse)
               icsh = icsl + 1
               READ (46,*,END = 1400) popread
               ncoll = i
               POPlv(ilv,nnurec) = POPlv(ilv,nnurec) + popread
               CSDirlev(ilv,nejcec) = CSDirlev(ilv,nejcec) + popread
               CSEmis(nejcec,1) = CSEmis(nejcec,1) + popread
C--------------add direct transition to the spectrum
               popl = popread*(FLOAT(icsh) - xcse)/DE
               IF (icsl.EQ.1) popl = 2.0*popl
               poph = popread*(xcse - FLOAT(icsl))/DE
               CSE(icsl,nejcec,1) = CSE(icsl,nejcec,1) + popl
               CSE(icsh,nejcec,1) = CSE(icsh,nejcec,1) + poph
               READ (45,*,END = 1400)     ! Skipping level identifier line
               iang = 0
               DO iang1 = 1, NANgela
                  READ (45,'(7x,E12.5)',END = 1400) ftmp
C-----------------To use only those values corresponding to EMPIRE grid for inelastic XS
                  if(mod(DBLE(iang1-1)*angstep+gang,gang).NE.0) cycle
                  iang = iang +1
                  CSAlev(iang,ilv,nejcec) = CSAlev(iang,ilv,nejcec)+ftmp
C-----------------add direct transition to the inclusive spectrum CSEa(,,,0)
C                 popl = ftmp*(FLOAT(icsh) - xcse)/DE
C                 IF (icsl.EQ.1) popl = 2.0*popl
C                 CSEa(icsl,iang,nejcec,0) = CSEa(icsl,iang,nejcec,0) +
C    &            popl
C                 poph = ftmp*(xcse - FLOAT(icsl))/DE
C                 CSEa(icsh,iang,nejcec,0) = CSEa(icsh,iang,nejcec,0) +
C    &            poph
               ENDDO
C--------------construct recoil spectra due to direct transitions
               IF (ENDf(nnurec).GT.0) THEN
                  dang = PI/FLOAT(NDANG - 1)
                  coef = 2*PI*dang
C-----------------check whether integral over angles agrees with x-sec. read from ECIS
                  csum = 0.0
                  DO iang = 1, NDANG
                     csum = csum + CSAlev(iang,ilv,nejcec)*SANgler(iang)
     &                      *coef
                  ENDDO
C-----------------correct 'coef' for eventual imprecision and include recoil DE
                  coef = coef*POPlv(ilv,nnurec)/csum/DERec
                  echannel = echannel*EJMass(0)/AMAss(1)
                  DO iang = 1, NDANG
                     erecoil = ecm + echannel + 2*SQRT(ecm*echannel)
     &                         *CANgler(iang)
                     irec = erecoil/DERec + 1.001
                     weight = (erecoil - (irec - 1)*DERec)/DERec
C--------------------escape if we go beyond recoil spectrum dimension
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
C
C------------ADDING INELASTIC TO CONTINUUM  (D_Elv(ND_nlv) = elvr)
             echannel = EX(NEX(1),1) - Q(nejcec,1) - D_Elv(i)
             icsl = INT(echannel/DE + 1.0001)
C------------avoid reading closed channels
             IF (echannel.GE.0.0001 .and. icsl.gt.0 .and. nejcec.le.2)
     &       THEN
               READ (46,*,END = 1400) popread
C
C--------------This level is not counted as a discrete one
C--------------but it is embedded in the continuum
C
               CSMsd(nejcec) = CSMsd(nejcec) + popread
               xsinlcont = xsinlcont + popread
C--------------Spreading it using resolution function (SQRT(2*PI) = 2.5066)
               if(isigma.gt.0) then
                 dtmp = 0.d0
                 do ie = max(icsl - 3*isigma,1) ,
     &                   min(NDEcse,icsl + 3*isigma)
                   dtmp = dexp(-dble(ie-icsl)**2/(2.*isigma*isigma))/
     &                  (2.5066d0*isigma) + dtmp
                 enddo
                 if(dtmp.gt.0.d0) then
                   do ie = max(icsl - 3*isigma,1) ,
     &                     min(NDEcse,icsl + 3*isigma)
                     CSEmsd(ie,nejcec) = CSEmsd(ie,nejcec) + popread/DE
     &                   * dexp(-dble(ie-icsl)**2/(2.*isigma*isigma))
     &                 /(2.5066d0*isigma*dtmp)
                   enddo
                 endif
               else
                 CSEmsd(icsl,nejcec) = CSEmsd(icsl,nejcec) + popread/DE
               endif
               READ (45,*,END = 1400)     ! Skipping level identifier line
               iang = 0
               DO iang1 = 1, NANgela
                  READ (45,'(7x,E12.5)',END = 1400) ftmp
C-----------------To use only those values corresponding to EMPIRE grid for inelastic XS
                  if(mod(DBLE(iang1-1)*angstep+gang,gang).NE.0) cycle
                  iang = iang + 1
                  if(isigma.gt.0 .and. dtmp.gt.0.) then
                    do ie = max(icsl - 3*isigma,1) ,
     &                    min(NDEcse,icsl + 3*isigma)
                      popl = ftmp/DE * dexp(-dble(ie-icsl)**2/
     &                    (2.*isigma*isigma))/(2.5066d0*isigma*dtmp)
                      CSEa(ie,iang,nejcec,1) =  CSEa(ie,iang,nejcec,1) +
     &                popl
C---------------------add direct transition to the inclusive spectrum CSEa(,,,0)
C                     CSEa(ie,iang,nejcec,0) =  CSEa(ie,iang,nejcec,0) +
C    &                popl
                    enddo
                  else
                    CSEa(icsl,iang,nejcec,1) =  CSEa(icsl,iang,nejcec,1)
     &                                     + ftmp/DE
C-------------------add direct transition to the inclusive spectrum CSEa(,,,0)
C                   CSEa(icsl,iang,nejcec,0) =  CSEa(icsl,iang,nejcec,0)
C    &                                     + ftmp/DE
                  endif
              ENDDO
             ENDIF
C------------END OF ADDING INELASTIC TO CONTINUUM
           ENDIF
 1350    ENDDO
      ENDIF
 1400 CLOSE (45)
      IF (DIRect.NE.0) CLOSE (46)
C-----print elastic and direct cross sections from ECIS
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

      IF (KTRlom(0,0).GT.0) THEN

      IF (ZEJc(0).EQ.0 .AND. AEJc(0).GT.0) THEN
C        WRITE (6,99005) TOTcs, CSFus, ELAcs
         WRITE (6,99005) TOTcs, ABScs, ELAcs
99005    FORMAT (/,2x,'Total cross section         :',e14.7,' mb',/,2x,
     &           'Absorption cross section    :',e14.7,' mb',/,2x,
     &           'Shape elastic cross section :',e14.7,' mb',//)
      ENDIF
      IF (ZEJc(0).NE.0 .OR. AEJc(0).EQ.0) THEN
C        WRITE (6,99010) CSFus
         WRITE (6,99010) CSFus + SINlcc + SINl
99010    FORMAT (/,2x,'Absorption cross section    :',e14.7,' mb',//)
      ENDIF

      WRITE (6,99015)
      WRITE (6,99020)
      gang = 180.0/(NDAng - 1)
      angstep = 180.0/(nelang - 1)
      DO iang = 1, nelang/4 + 1
         imint = 4*(iang - 1) + 1
         imaxt = MIN0(4*iang,nelang)
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
99035 FORMAT (1x,f5.0,3x,11(2x,E12.6))
99040 FORMAT (6x,3x,11(2x,E12.6))

      WRITE (6,'(//)')

      IF (ncoll.GT.0) THEN
C--------locate position of the projectile among ejectiles
         CALL WHEREJC(IZAejc(0),nejcec,iloc)
         WRITE (6,*) ' '
         gang = 180.d0/(NDANG - 1)
         its = 2
         DO ilv = 2,ncoll
            DO iang= ilv+1,ncoll
              if(ICOller(iang).eq.ICOller(ilv)) goto 99027
            ENDDO
            itemp(its) = ICOller(ilv)
            deform(its)   = D_DEF(ilv,2)
            its = its +1
99027    ENDDO
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
C-----locate postions of ENDF MT-numbers 2, 91, 649, and 849
      CALL WHERE(IZA(1) - IZAejc(0),mt2,iloc)
      CALL WHERE(IZA(1) - IZAejc(1),mt91,iloc)
      CALL WHERE(IZA(1) - IZAejc(2),mt649,iloc)
      CALL WHERE(IZA(1) - IZAejc(3),mt849,iloc)

C-----locate residual nuclei after CN decay
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
C-----calculate MSD contribution
C-----
      corrmsd = 1.0
      IF (MSD.NE.0 .AND. EIN.GT.3.D0) THEN
C
C--------call ORION
C
C--------This part prompts for the name of a data file. The INQUIRE
C--------statement then determines whether or not the file exists.
C--------If it does not, the program start new calculations
         WRITE (ctmp20,'(i3.3,i3.3,1h_,i3.3,i3.3,1h_,i6.6)')
     &       INT(ZEJc(0)), INT(AEJc(0)), INT(Z(0)),
     &       INT(A(0)), INT(EINl*1000)
         INQUIRE (FILE = (ctldir//ctmp20//'.MSD'),EXIST = fexist)
         IF (.NOT.fexist) THEN
           OPEN (15,FILE = (ctldir//ctmp20//'.MSD'),STATUS='NEW')
         ELSE
           OPEN (15,FILE = (ctldir//ctmp20//'.MSD'),STATUS='OLD')
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
C--------set to Q's to 0 if negative due to rounding error
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
         ftmp = CSFus - xsinl - xsinlcont
         CALL PCROSS(ftmp,totemis)
      ENDIF          ! PCRoss done

      IF ((xsinl+xsinlcont+totemis+SINl+SINlcc).gt.0. .AND. nejcec.gt.0
     &    .AND. NREs(nejcec).GE.0 ) THEN
C--------print inelastic PE double differential cross sections
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
C-------------last continuum energy bin is calculated
              DO i = 1, MAX(INT((echannel-ECUt(nnur))/DE + 1.0001),1)
                WRITE (6,'(1X,F7.3,1X,11E11.4)') FLOAT(i - 1)*DE,
     &           (max(CSEa(i,iang,nejc,1),0.d0),iang = iad,iam)
              ENDDO
              WRITE (6,*) ' '
            ENDDO
         ENDIF

         if(xsinlcont.gt.0) write(6,*)
     &   ' DWBA to continuum XS for inelastic channel ',xsinlcont
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
C--------correct CN population for PE and DWBA into continuum emission
         corrmsd = (CSFus - (xsinl + xsinlcont + totemis))/CSFus
         IF (corrmsd.LT.0.0D0) THEN
            WRITE (6,*) ' '
            WRITE (6,*) 'PE EMISSION LARGER THEN FUSION CROSS SECTION'
            IF (MSD+MSC.GT.0 .AND. ICOmpff.GT.0) THEN
               WRITE (6,*) 'TRY TO TURN OFF COMPRESSIONAL FORM FACTOR '
               WRITE (6,*) 'SETTING COMPFF TO 0 IN THE OPTIONAL INPUT.'
               STOP
            ENDIF
            IF (MSD+MSC.GT.0 .AND. ICOmpff.EQ.0) THEN
               WRITE (6,*) 'THIS MAY HAPPEN IF RESPONSE FUNCTIONS ARE '
               WRITE (6,*) 'RENORMALIZED IN INPUT OR FITTED TO WRONG '
               WRITE (6,*) 'DISCRETE LEVELS. CHECK `EFIT` AND `RESNOR` '
               WRITE (6,*) 'IN OPTIONAL INPUT.    '
               WRITE (6,*)
     &                    'IF THESE ARE FINE TRY ANOTHER OPTICAL MODEL.'
               STOP
            ENDIF
            IF (MSD+MSC.EQ.0) THEN
               WRITE (6,*) 'THIS MAY HAPPEN IF TOO MANY DISCRETE LEVELS'
               WRITE (6,*) 'ARE EMBEDDED INTO CONTINUUM OR HAVE TOO BIG'
               WRITE (6,*) 'DYNAMICAL DEFORMATIONS SPECIFIED IN THE    '
               WRITE (6,*) 'COLLECTIVE LEVEL FILE.'
               WRITE (6,*)
     &                  'TRY TO REDUCE THE NUMBER OF COLLECTIVE LEVELS.'
               STOP
            ENDIF
         ENDIF
         DO i = 1, NLW
            POP(NEX(1),i,1,1) = POP(NEX(1),i,1,1)*corrmsd
            POP(NEX(1),i,2,1) = POP(NEX(1),i,2,1)*corrmsd
         ENDDO
C--------TRISTAN *** done ***
C--------add MSD contribution to the residual nucleus population
C--------locate residual nucleus after MSD emission
         DO nejc = 0, NDEjc
           nnur = NREs(nejc)
           IF(nnur.LT.0) CYCLE
           IF (CSMsd(nejc).NE.0.0D0) CALL ACCUMSD(1,nnur,nejc)
C----------add PE contribution to energy spectra (angle int.)
           DO ie = 1, NDEcse
              CSE(ie,nejc,1) = CSE(ie,nejc,1) + CSEmsd(ie,nejc)
           ENDDO
C----------add PE contribution to the total NEJC emission
           CSEmis(nejc,1) = CSEmis(nejc,1) + CSMsd(nejc)
         ENDDO
C        Skipping all emitted but neutrons and protons
C        Secondary emission was not tested for proton induced reactions
         nnur = NREs(nejcec)
C        IF(nnur.GE.0) THEN
         IF(nnur.GE.2000) THEN
C----------second chance preequilibrium emission after MSD emission
C----------neutron emission
           izares = INT(1000.0*Z(nnur) + A(nnur) - 1)
           CALL WHERE(izares,nnurn,iloc)
           IF (iloc.EQ.0) CALL SCNDPREEQ(nnur,nnurn,1,0)
           IF (iloc.EQ.0 .AND. IOUt.GT.3) CALL AUERST(nnur,1)
C----------proton emission
           izares = izares - 1000
           CALL WHERE(izares,nnurp,iloc)
           IF (iloc.EQ.0) THEN
             CALL SCNDPREEQ(nnur,nnurp,2,1)
             IF (IOUt.GT.3) CALL AUERST(nnur,2)
           ELSE
             CALL SCNDPREEQ(nnur,nnurp,2,2)
           ENDIF
C----------second chance preequilibrium *** done ***
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
            WRITE (6,'(''   DWBA to continuum = '',
     &  G13.6,'' mb'')') xsinlcont
            WRITE (6,'(''   PE + inelastic to continuum = '',
     &  G13.6,'' mb'')') xsinl + totemis
         ELSEIF (DIRect.EQ.1 .OR. DIRect.EQ.2) THEN
            WRITE (6,
     &'(''   Fusion cross section = '',G13.6,
C    &  '' mb including'')') CSFus
     &  '' mb including'')') CSFus + SINl + SINlcc
            WRITE (6,
     &'(''   DWBA inelastic to uncoupled discrete levels = '',
     &  G13.6,'' mb'')') SINl
            WRITE (6,
     &'(''   CC inelastic to coupled discrete levels = '',
     &  G13.6,'' mb'')') SINlcc
            WRITE (6,'(''   DWBA to continuum = '',
     &  G13.6,'' mb'')') xsinlcont
            WRITE (6,'(''   PE + inelastic to continuum = '',
     &  G13.6,'' mb'')') xsinl + totemis
            WRITE (6,
     &'(''   Spin distribution calculated using '',
     &  ''CC transmission coefficients'')')
         ELSEIF (DIRect.EQ.3) THEN
            WRITE (6,
     &'(''   Fusion cross section = '',G13.6,
C    &  '' mb including'')') CSFus
     &  '' mb including'')') CSFus + SINl + SINlcc
            WRITE (6,
     &'(''   DWBA inelastic to discrete levels = '',
     &  G13.6,'' mb'')') SINl
            WRITE (6,'(''   DWBA to continuum = '',
     &  G13.6,'' mb'')') xsinlcont
            WRITE (6,'(''   PE + inelastic to continuum = '',
     &  G13.6,'' mb'')') xsinl + totemis
            WRITE (6,
     &'(''   Spin distribution does NOT contain'',
     &  '' DWBA inelastic contribution '')')
         ENDIF
      ENDIF
      IF (ENDf(1).EQ.0.0D0) THEN
C        WRITE (12,'('' FUSION CROSS SECTION = '',G12.5,'' mb'')') CSFus
         WRITE (12,'('' FUSION CROSS SECTION = '',G13.6, '' mb'')')
     &          CSFus + SINl + SINlcc
      ELSE
         WRITE (12,*) ' '
         WRITE (12,'('' FUSION CROSS SECTION = '',G12.5,'' mb'')')
     &          CSFus + SINl + SINlcc
         WRITE (12,'('' TOTAL  CROSS SECTION = '',G13.6,'' mb'')') TOTcs
         WRITE (12,*) ' '
      ENDIF
      POPmax(1) = CSFus*1.0E-25
C-----renormalization of CN spin distribution if TURBO mode invoked
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
      OPEN (80,FILE = 'FISSION.OUT',STATUS = 'UNKNOWN')
C-----start DO loop over decaying nuclei
      DO nnuc = 1, NNUcd
         IF(QPRod(nnuc).LT.-999.d0) CYCLE
         DO kk = 0, NFISENMAX
            DO jj = 1, NDLW
               DO i = 1, NFHUMP
                  ROFis(kk,jj,i) = 0.
               ENDDO
            ENDDO
         ENDDO
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
C--------reset variables for life-time calculations
         stauc = 0.0
         sgamc = 0.0
         csemist = 0.0
         CSFis = 0.0
         IF (FISmod(nnuc).GT.0.) THEN
            DO m = 1, INT(FISmod(nnuc)) + 1
               CSFism(m) = 0.
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
         IF (ENDf(nnuc).NE.0.0D0) THEN
            WRITE (12,*) ' '
            WRITE (12,*)
     &' ---------------------------------------------------------------'
            WRITE (12,
     &'(''  Decaying nucleus '',I3,''-'',A2,''-'',I3,     ''  mass='',F1
     &0.6,'' Q-value='',F10.6)') INT(Z(nnuc)), SYMb(nnuc), ia,
     &         AMAss(nnuc), QPRod(nnuc) + ELV(LEVtarg,0)
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
C-----------------check for the number of branching ratios
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
C-----------------next IF moves levels population to the ground state
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
C--------------write elastic to tape 12
C1460          WRITE (12,'(1X,/,10X,40(1H-),/)')
 1460          IF (nnuc.EQ.mt2) THEN
                  WRITE (12,'(1X,/,10X,40(1H-),/)')
                  WRITE (12,*) ' '
                  WRITE (12,
     &                   '('' ELASTIC CROSS SECTION ='',G12.5,'' mb'')')
     &                   ELAcs
                  WRITE (12,*) ' '
                  WRITE (12,*) ' Elastic angular distribution '
                  WRITE (12,*) ' '
                  delang = 180./FLOAT(nelang - 1)
                  WRITE (12,99045) (FLOAT(iang - 1)*delang,iang = 1,
     &                             nelang)
99045             FORMAT (10X,8G15.5)
                  WRITE (12,99050) (elada(iang) + elcncs,iang = 1,nelang
     &                             )
99050             FORMAT (9X,8E15.5)
                  WRITE (12,*) ' '
                  IF (elcncs.EQ.0) WRITE (6,*)
     &                 'WARNING: CN elastic is 0'
               ENDIF
            ENDIF
         ENDIF
         POPmax(nnuc) = POPmax(nnuc)*0.0001
         IF (POPmax(nnuc).EQ.0.0D0) THEN
            WRITE (6,*) ' '
            WRITE (6,*)
     &                'Continuum of this nucleus has not been populated'
            GOTO 1500
         ENDIF
C--------prepare gamma transition parameters
         CALL ULM(nnuc)
C--------calculate compound nucleus level density at saddle point
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
     &'('' HMS inclusive neut. emission ='',G12.5,
     &  ''mb'')') CSHms(1)
            WRITE (6,
     &'('' HMS inclusive prot. emission ='',G12.5,
     &  ''mb'')') CSHms(2)
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

         IF (nnuc.EQ.1 .AND. IOUt.GE.3 .AND.
     &     (CSEmis(0,1) + CSEmis(1,1) + CSEmis(2,1) + CSEmis(3,1))
     &       .NE.0) THEN
            WRITE (6,*) ' '
            WRITE (6,*) ' Preequilibrium spectra (sum of all models):'
            CALL AUERST(1,0)
            CALL AUERST(1,1)
            CALL AUERST(1,2)
            CALL AUERST(1,3)
            WRITE (6,*) ' '
            IF (LHMs.NE.0 .AND. ENDf(1).NE.1) THEN
               WRITE (6,*) ' HMS spectra stored as inclusive:'
               CALL AUERST(0,1)
               CALL AUERST(0,2)
               WRITE (6,*) ' '
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
C--------
C--------start Hauser-Feshbach nnuc nucleus decay
C--------
         popleft = 0.0
C--------ensure that full gamma cascade in the first CN is
C--------accounted for in the case of ENDF calculations
         IF (ENDf(1).GT.0.0D0) GCAsc = 1.0
C--------turn on (KEMIN=1) or off (KEMIN=NEX(NNUC)) gamma cascade
C--------in the first CN
         kemin = 1
         IF (nnuc.EQ.1) THEN
            IF (GCAsc.EQ.0.0D0) kemin = NEX(nnuc)
            IF (GCAsc.EQ. - 1.0D0 .AND. EXCn.GT.20.0D0)
     &          kemin = NEX(nnuc)
         ENDIF
C--------turn  off (KEMIN=NEX(NNUC)) gamma cascade in the case of OMP fit
         IF (FITomp.NE.0) kemin = NEX(nnuc)
         kemax = NEX(nnuc)
C--------account for widths fluctuations (HRTW)
         IF (LHRtw.EQ.1 .AND. EIN.GT.EHRtw) LHRtw = 0
         IF (nnuc.EQ.1 .AND. LHRtw.GT.0) THEN
            CALL HRTW
            IF (ENDf(1).GT.0) CALL RECOIL(kemax,nnuc) !recoil spectrum
            kemax = NEX(nnuc) - 1
            GCAsc = 1.0
         ENDIF
C--------do loop over c.n. excitation energy
         DO ke = kemax, kemin, -1
            step = DE
            IF (ke.EQ.NEX(nnuc) .OR. ke.EQ.1) step = 0.5*DE
            IF (ke.EQ.NEX(nnuc) .AND. nnuc.EQ.1) step = 1.0
            IF (ENDf(1).GT.0) THEN
C--------------clean auxiliary particle spectra for calculation of recoils
               DO nejc = 0, NEJcm
                  DO il = 1, NDLV
                     REClev(il,nejc) = 0.0
                  ENDDO
                  DO ie = 1, NDECSE
                     AUSpec(ie,nejc) = 0.0
                  ENDDO
               ENDDO
C--------------calculate population in the energy bin ke
               pope = 0.0
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
C--------------------residual nuclei must be heavier than alpha
                     if(ares.le.4. and. zres.le.2.) cycle
                     izares = INT(1000.0*zres + ares)
                     CALL WHERE(izares,nnur,iloc)
                     if(iloc.eq.1) CYCLE
                     CALL DECAY(nnuc,ke,jcn,ip,nnur,nejc,sum)
                  ENDDO
C-----------------do loop over ejectiles       ***done***
C-----------------gamma emision
                  IF (LTUrbo.EQ.1) THEN
                     CALL DECAYG(nnuc,ke,jcn,ip,sum)
                  ELSE
                     CALL DECAYT(nnuc,ke,jcn,ip,sum)
                  ENDIF
C-----------------distribute yrast population over discrete levels
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
C--------------------look for the discrete level with the closest spin
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
C-----------------fission ()
                  IF (FISsil(nnuc) .AND. (FISshi(nnuc).EQ.1.))
     &                CALL FISSION(nnuc,ke,jcn,sumfis)
                  IF (FISsil(nnuc) .AND. (FISshi(nnuc).NE.1.))
     &                CALL FISCROSS(nnuc,ke,ip,jcn,sumfis,sumfism,
     &                              dencomp,aafis,0)
C-----------------normalization and accumulation
C-----------------
                  xnor = POP(ke,jcn,ipar,nnuc)*step/DENhf
                  stauc = stauc + RO(ke,jcn,nnuc)*xnor
                  IF (RO(ke,jcn,nnuc).NE.0.0D0) sgamc = sgamc +
     &                DENhf*POP(ke,jcn,ipar,nnuc)*step/RO(ke,jcn,nnuc)
                  CALL XSECT(nnuc,m,xnor,sumfis,sumfism,ke,ipar,jcn,
     &                       dencomp,aafis)

C-----------------calculate total emission
                  DO nejc = 0, NEJcm
                     csemist = csemist + CSEmis(nejc,nnuc)
                  ENDDO
                  csemist = csemist + CSFis
 1470          ENDDO                !loop over decaying nucleus spin
            ENDDO                   !loop over decaying nucleus parity
            IF (ENDf(nnuc).GT.0) CALL RECOIL(ke,nnuc) !recoil spectrum for ke bin
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
C--------printout of results for the decay of NNUC nucleus
         IF (IOUt.GT.0) WRITE (6,
     &          '(1X,/,'' Population left because too small '',G12.5,/)'
     &          ) popleft*DE
 1500    dtmp = 0.d0
         DO il = 1, NLV(nnuc)
           dtmp = dtmp + POPlv(il,nnuc)
         ENDDO
         IF(dtmp.LE.0.0) GOTO 1525

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
     &shifted to the g.s.'')')
         IF (IOUt.GT.0) WRITE (6,'(1X,/,10X,40(1H-),/)')
         IF (ENDf(nnuc).NE.0 .AND. nnuc.EQ.1) THEN
            WRITE (12,
     &'(1X,/,10X,''Discrete level population '',              ''before g
     &amma cascade'')')
            WRITE (12,'(1X,/,10X,40(1H-),/)')
         ENDIF
         DO il = 1, NLV(nnuc)
            CSPrd(nnuc) = CSPrd(nnuc) + POPlv(il,nnuc)
            IF (IOUt.GT.0) WRITE (6,99070) il, ELV(il,nnuc),
     &                            LVP(il,nnuc), XJLv(il,nnuc),
     &                            POPlv(il,nnuc)
            IF (ENDf(nnuc).NE.0 .AND. nnuc.EQ.1) THEN
C--------------check for the number of branching ratios
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
         IF (ENDf(nnuc).GT.0 .AND. CSPrd(nnuc).GT.0. .AND.
     &      (nnuc.EQ.1 .OR. nnuc.EQ.mt91 .OR. nnuc.EQ.mt649 .OR.
     &      nnuc.EQ.mt849)) THEN

            WRITE (12,'(1X,/,10X,40(1H-),/)')
            WRITE (12,*) ' '
C-----------write Int. Conv. Coefff. for discrete transitions
            WRITE (12,'(1X,/,10X,
     &             ''Internal conversion coefficients'')')
            WRITE (12,'(1X,/,10X,40(1H-),/)')
            DO il = 1, NLV(nnuc)
C-------------check for the number of branching ratios
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
     &'(1X,I3,''-'',A2,''-'',I3,'' production cross section '',G12.5,''
     &mb  '',''reaction: '',A21)') iz, SYMb(nnuc), ia, CSPrd(nnuc),
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
         DO ispec = 1, NEX(1) + 10
            gtotsp = gtotsp + POPcse(0,0,ispec,nnuc)*DE
            xtotsp = xtotsp + POPcse(0,1,ispec,nnuc)*DE
            ptotsp = ptotsp + POPcse(0,2,ispec,nnuc)*DE
            atotsp = atotsp + POPcse(0,3,ispec,nnuc)*DE
            emedg = emedg + POPcse(0,0,ispec,nnuc)*DE*(ispec - 1)*DE
            emedn = emedn + POPcse(0,1,ispec,nnuc)*DE*(ispec - 1)*DE
            emedp = emedp + POPcse(0,2,ispec,nnuc)*DE*(ispec - 1)*DE
            emeda = emeda + POPcse(0,3,ispec,nnuc)*DE*(ispec - 1)*DE
            IF (NDEJC.EQ.4) THEN
               htotsp = htotsp + POPcse(0,NDEJC,ispec,nnuc)*DE
               emedh = emedh + POPcse(0,NDEJC,ispec,nnuc)
     &                    *DE*(ispec - 1)*DE
            ENDIF
         ENDDO
         POPcs(0,nnuc) = gtotsp
         POPcs(1,nnuc) = xtotsp
         POPcs(2,nnuc) = ptotsp
         POPcs(3,nnuc) = atotsp
         IF (NDEJC.EQ.4) POPcs(NDEJC,nnuc) = htotsp
         IF (gtotsp.NE.0) emedg = emedg/gtotsp
         IF (xtotsp.NE.0) emedn = emedn/xtotsp
         IF (ptotsp.NE.0) emedp = emedp/ptotsp
         IF (atotsp.NE.0) emeda = emeda/atotsp
         IF (htotsp.NE.0) emedh = emedh/htotsp
         IF (CSPrd(nnuc).GT.0. .AND. IOUt.GT.3) THEN
C--------------Add contributions to discrete levels for MT=91,649,849
C--------------(merely for checking purpose)
               IF (nnuc.EQ.mt91) THEN
                  nejc = 1
                  WRITE (6,'(11X,'' Sum to continuum         '',G12.5,
     &                '' mb  '')') xtotsp
                  DO ilev = 1, NLV(nnuc)
                     xtotsp = xtotsp + CSDirlev(ilev,nejc)
                  ENDDO
               ELSEIF (nnuc.EQ.mt649) THEN
                  nejc = 2
                  WRITE (6,'(11X,'' Sum to continuum         '',G12.5,
     &                '' mb  '')') ptotsp
                  DO ilev = 1, NLV(nnuc)
                     ptotsp = ptotsp + CSDirlev(ilev,nejc)
                  ENDDO
               ELSEIF (nnuc.EQ.mt849) THEN
                  nejc = 3
                  WRITE (6,'(11X,'' Sum to continuum         '',G12.5,
     &                '' mb  '')') atotsp
                  DO ilev = 1, NLV(nnuc)
                     atotsp = atotsp + CSDirlev(ilev,nejc)
                  ENDDO
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
               DO ispec = 1, NEX(1) + 10
                 IF (NDEJC.EQ.4) THEN
                     WRITE (6,'(6g15.5)') (ispec - 1)*DE,
     &                      POPcse(0,0,ispec,nnuc),
     &                      POPcse(0,1,ispec,nnuc),
     &                      POPcse(0,2,ispec,nnuc),
     &                      POPcse(0,3,ispec,nnuc),
     &                      POPcse(0,NDEJC,ispec,nnuc)
                 ELSE
                     WRITE (6,'(5g15.5)') (ispec - 1)*DE,
     &                      POPcse(0,0,ispec,nnuc),
     &                      POPcse(0,1,ispec,nnuc),
     &                      POPcse(0,2,ispec,nnuc),
     &                      POPcse(0,3,ispec,nnuc)
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
     &                                 POPcseaf(0,0,ispec,nnuc),
     &                                 POPcseaf(0,1,ispec,nnuc),
     &                                 POPcseaf(0,2,ispec,nnuc),
     &                                 POPcseaf(0,3,ispec,nnuc)
               ENDDO
               WRITE (6,*) ' '
         ENDIF
C--------calculate life-times and widths
         IF (IOUt.GT.0) THEN
            IF (csemist.NE.0.0D0) THEN
               taut = stauc*6.589E-22*2.0*PI/csemist
               WRITE(6,
     &               '(/''  Average total   life-time'',G12.5,'' s'')')
     &                taut
               gamt = sgamc/2.0/PI/csemist
               WRITE(6,
     &               '(''  Average total   width    '',G12.5,'' MeV'')')
     &                gamt
C              TAUT=6.589E-22/GAMT
C              WRITE(6,'('' Average total life-time 1/width'',g12.5,
C              1       '' s'')') TAUT
            ENDIF
            IF (CSFis.NE.0.0D0) THEN
               tauf = stauc*6.589E-22*2.0*PI/CSFis
               WRITE(6,'(''  Average fission life-time'',G12.5,'' s'')')
     &                tauf
               gamfis = gamt*CSFis/csemist
               WRITE (6,
     &        '(''  Average fission width    '',G12.5,'' MeV'')') gamfis
            ENDIF
C-----------life-times and widths  *** done ***
            IF (FISmod(nnuc).GT.0) THEN
               DO m = 1, INT(FISmod(nnuc)) + 1
                  CSFis = CSFis + CSFism(m)
               ENDDO
               DO m = 1, INT(FISmod(nnuc)) + 1
                  IF (CSFis.GT.0.) WFIsm(m) = CSFism(m)/CSFis
                  WRITE (80,*) '    Mode=', m, '   weight=', WFIsm(m)
               ENDDO
               WRITE (80,*) '   Fission cross section=', CSFis, ' mb'
               IF (CSFis.GT.0.) THEN
                 WRITE (6,*) ' '
                 WRITE (6,
     &            '('' Fission    cross section    '',G12.5,'' mb'')'
     &             ) CSFis
               ENDIF
            ENDIF
         ENDIF
         TOTcsfis = TOTcsfis + CSFis
C--------add compound elastic to shape elastic before everything falls
C--------down on the ground state
9876     IF (nnuc.EQ.1 .AND. INT(AEJc(0)).NE.0
     &                       .AND. POPlv(LEVtarg,mt2).GT.0.) THEN
            WRITE (6,*)
            WRITE (6,*) ' Incident energy (CMS)      ', EIN, ' MeV'
            WRITE (6,*) ' Shape elastic cross section', ELAcs, ' mb'
            WRITE (6,*) ' CN elastic cross section   ',
     &                    POPlv(LEVtarg,mt2),' mb'
            ELAcs = ELAcs + POPlv(LEVtarg,mt2)
C-----------CN contribution to elastic ddx
            elcncs = POPlv(LEVtarg,mt2)/4.0/PI
            WRITE (6,*)
     &          ' CN elastic angular distrib.', elcncs, ' mb/str'
            WRITE (6,*)
         ENDIF
         checkXS = checkXS + CSPrd(nnuc)
         WRITE (12,*) ' '
         WRITE (12,
     &'(1X,I3,''-'',A2,''-'',I3,'' production cross section '',G12.6,''
     &mb'')') iz, SYMb(nnuc), ia, CSPrd(nnuc)
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
         IF(CSPrd(nnuc).GT.0.) THEN
            DO nejc = 1, NEJcm
             ares = A(nnuc) - AEJc(nejc)
             zres = Z(nnuc) - ZEJc(nejc)
C------------residual nuclei must be heavier than alpha
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
C------------print residual nucleus population
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
      CLOSE (80)
      CLOSE (79)
C----
C---- ENDF spectra printout (exclusive representation)
C----
      DO nnuc = 1, NNUcd               !loop over decaying nuclei
         IF (ENDf(nnuc).EQ.1) THEN
            IF (CSPrd(nnuc).GT.0.0D0) THEN
               DO nejc = 0, NDEJC         !loop over ejectiles
                  IF (POPcs(nejc,nnuc).EQ.0) CYCLE
                  nspec = INT(EMAx(nnuc)/DE) + 2
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
C-----------------double the first bin x-sec to preserve integral in EMPEND
C                 POPcse(0, nejc, 1, nnuc) =  POPcse(0, nejc, 1, nnuc)*2
                  WRITE (12,*) ' '
                  WRITE (12,*) ' Spectrum of ', cejectile,
     &                         REAction(nnuc), ' ZAP= ', iizaejc
C-----------------recorp is a recoil correction factor defined 1+Ap/Ar that
C-----------------multiplies cross sections and divides outgoing energies
                  recorp = 1.0
                  IF (nejc.GT.0) recorp = 1. + EJMass(nejc)/AMAss(nnuc)
C-----------------Exclusive DDX spectra (neutrons & protons)
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
                           DO ie = 1, nspec ! reconstruct continuum DDX spectrum
                              DO nang = 1, NDANG
                                 piece = CSEmsd(ie,nejc)
                                 IF (ie.EQ.NEXr(nejc,1))
     &                               piece = 0.5*piece
                                 cseaprnt(ie,nang)
     &                              = ((POPcse(0,nejc,ie,nnuc)
     &                              - piece*POPcseaf(0,nejc,ie,nnuc))
     &                              /4.0/PI + CSEa(ie,nang,nejc,1)
     &                              *POPcseaf(0,nejc,ie,nnuc))
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
     &     (max(cseaprnt(ie,nang)*recorp,0.d0),nang = 1,NDANG)
                        ENDDO
                        DO ie = nspec, nspec + 1
                                               ! exact DDX spectrum endpoint
                           WRITE (12,
     &'(F10.5,E14.5,7E15.5,/,                                 (9X,8E15.5
     &))') EMAx(nnuc)/recorp,
     &     (cseaprnt(ie,nang)*recorp,nang = 1,NDANG)
                        ENDDO
                     ELSE
C-----------------------remaining n- or p-emissions (continuum and levels together)
                        DO ie = 1, nspec + 1
                                           ! clean DDX matrix
                           DO nang = 1, NDANG
                              cseaprnt(ie,nang) = 0.0
                           ENDDO
                        ENDDO
                        DO ie = 1, nspec  ! reconstruct DDX spectrum
                           DO nang = 1, NDANG
                              piece = CSEmsd(ie,nejc)
                              IF (ie.EQ.NEXr(nejc,1)) piece = 0.5*piece
                              cseaprnt(ie,nang)
     &                           = ((POPcse(0,nejc,ie,nnuc)
     &                           - piece*POPcseaf(0,nejc,ie,nnuc))
     &                           /4.0/PI + CSEa(ie,nang,nejc,1)
     &                           *POPcseaf(0,nejc,ie,nnuc))
                           ENDDO
                        ENDDO
                        DO nang = 1, NDANG
                                          !double the first bin to preserve integral in EMPEND
                           cseaprnt(1,nang) = cseaprnt(1,nang)*2.0
                        ENDDO
                        DO ie = 1, nspec - 1
                                           ! print DDX spectrum
                           WRITE (12,
     &'(F10.5,E14.5,7E15.5,/,                                 (9X,8E15.5
     &))') FLOAT(ie - 1)*DE/recorp,
     &     (max(cseaprnt(ie,nang)*recorp,0.d0),nang = 1,NDANG)
                        ENDDO
                        DO ie = nspec, nspec + 1
                                               ! exact DDX spectrum endpoint
                           WRITE (12,
     &'(F10.5,E14.5,7E15.5,/,                                 (9X,8E15.5
     &))') EMAx(nnuc)/recorp,
     &     (max(cseaprnt(ie,nang)*recorp,0.d0),nang = 1,NDANG)
                        ENDDO
                     ENDIF

                  ELSE
C--------------------Exclusive DDX spectra (gammas, alphas, light ions (DE))
C
C--------------------double the first bin x-sec to preserve integral in EMPEND
                     POPcse(0,nejc,1,nnuc) = POPcse(0,nejc,1,nnuc)*2
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
     &                         -espec, CSAlev(1,il,3)*4.0*PI*recorp/DE
                        ENDDO
                        DO ie = 1, nspec - 1
                                            ! MT=849 (continuum)
                           WRITE (12,'(F10.5,E14.5)') FLOAT(ie - 1)
     &                            *DE/recorp, POPcse(0,nejc,ie,nnuc)
     &                            *recorp
                        ENDDO
                                          ! MT=849 exact endpoint
                        WRITE (12,'(F10.5,E14.5)') EMAx(nnuc)/recorp,
     &                            POPcse(0,nejc,nspec,nnuc)*recorp
                        WRITE (12,'(F10.5,E14.5)') EMAx(nnuc)/recorp,
     &                            0.d0
                     ELSE  !all other emissions (continuum and levels together)
                        DO ie = 1, nspec - 1
                           WRITE (12,'(F10.5,E14.5)') FLOAT(ie - 1)
     &                            *DE/recorp, POPcse(0,nejc,ie,nnuc)
     &                            *recorp
                        ENDDO
                                                 ! exact endpoint
                        WRITE (12,'(F10.5,E14.5)') EMAx(nnuc)/recorp,
     &                            POPcse(0,nejc,nspec,nnuc)*recorp
                        WRITE (12,'(F10.5,E14.5)') EMAx(nnuc)/recorp,
     &                            0.d0
                     ENDIF
                  ENDIF
 1530          ENDDO  ! over ejectiles
               IF (nnuc.NE.1) CALL PRINT_RECOIL(nnuc,REAction(nnuc))
            ENDIF
         ENDIF

      ENDDO  ! over decaying nuclei
C-----Fission related spectra of particles and gammas
      IF (ENDf(1).GT.0) THEN
         IF (TOTcsfis.GT.0.0D0) THEN
            DO nejc = 0, NDEJC         !loop over ejectiles
C              IF(NEMn.eq.0 .and. nejc.eq.1) cycle
C              IF(NEMp.eq.0 .and. nejc.eq.2) cycle
C              IF(NEMa.eq.0 .and. nejc.eq.3) cycle
C              IF (NDEjc.EQ.4 .AND. NEMc.eq.0 .AND. nejc.eq.4) cycle
               IF (nejc.EQ.0) THEN
                  cejectile = 'gammas   '
                  iizaejc = 0
                  espmax = EMAx(1)
               ELSEIF (nejc.EQ.1) THEN
                  cejectile = 'neutrons '
                  iizaejc = IZAejc(nejc)
                  espmax = EMAx(1) - Q(1,1)
               ELSEIF (nejc.EQ.2) THEN
                  cejectile = 'protons  '
                  iizaejc = IZAejc(nejc)
                  espmax = EMAx(1) - Q(2,1)
               ELSEIF (nejc.EQ.3) THEN
                  cejectile = 'alphas   '
                  iizaejc = IZAejc(nejc)
                  espmax = EMAx(1) - Q(3,1)
               ELSEIF (nejc.EQ.4) THEN
                  cejectile = 'lt. ions '
                  iizaejc = IZAejc(nejc)
                  espmax = EMAx(1) - Q(NDEJC,1)
               ENDIF
               nspec = MIN(INT(espmax/DE) + 2,NDECSE - 1)
               IF(nspec.GT.0) THEN
C----------------double the first bin x-sec to preserve integral in EMPEND
                 CSEfis(1,nejc) = CSEfis(1,nejc)*2
                 WRITE (12,*) ' '
                 WRITE (12,*) ' Spectrum of ', cejectile,
     &                      '(z,fission)          ', ' ZAP= ', iizaejc
                 WRITE (12,*) ' '
                 WRITE (12,'('' Energy    mb/MeV'')')
                 WRITE (12,*) ' '
                 DO ie = 1, nspec - 1
                   WRITE (12,'(F10.5,E14.5)') FLOAT(ie - 1)*DE,
     &                   CSEfis(ie,nejc)
                 ENDDO
                 WRITE (12,'(F10.5,E14.5)') espmax, CSEfis(nspec,nejc)
                 WRITE (12,'(F10.5,E14.5)') espmax, 0.d0
               ENDIF
            ENDDO  ! over ejectiles
         ENDIF
      ENDIF
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
C------double-differential spectra
       DO nejc = 0, NDEJC
c         sumtst=0
          DO iesp = 1, NDECSE
C            WRITE(6,*)'iesp, CSE, nejc',iesp,CSE(iesp,nejc,0), nejc  
c            sumtst=sumtst+CSE(iesp,nejc,0)
             DO nang = 1, NDANG
                piece = CSEmsd(iesp,nejc)
                IF (iesp.EQ.NEXr(nejc,1)) piece = 0.5*piece
                CSEa(iesp,nang,nejc,0)
     &             = ((CSE(iesp,nejc,0)
     &             - piece*POPcseaf(0,nejc,iesp,0))
     &             /4.0/PI + CSEa(iesp,nang,nejc,1)
     &             *POPcseaf(0,nejc,iesp,0))
             ENDDO
          ENDDO
c         WRITE(6,*)'nejc, tot spec',nejc,sumtst*DE 
       ENDDO


c     DO nnuc = 1, NNUcd               !loop over decaying nuclei
c        IF (ENDf(nnuc).EQ.2) THEN
c           DO nejc = 0, NEJcm         !loop over ejectiles
c              IF (nejc.GT.0) THEN
c                 recorr = (AMAss(nnuc) - EJMass(nejc))/AMAss(nnuc)
c              ELSE
c                 recorr = 1.0
c              ENDIF
c              DO icse = 1, NDEX
c                 xccm = (icse - 1)*recorr + 1.0000001
c                 iccml = xccm
c                 iccmh = MIN(NDEX,iccml + 1)
c                 weight = xccm - iccml
c-----------------energy spectra
c                 CSE(iccml,nejc,0) = CSE(iccml,nejc,0)
c    &                                + CSE(icse,nejc,nnuc)
c    &                                *(1.0 - weight)
c                 CSE(iccmh,nejc,0) = CSE(iccmh,nejc,0)
c    &                                + CSE(icse,nejc,nnuc)*weight
c              ENDDO
c           ENDDO
c        ENDIF
c     ENDDO
      WRITE (6,*) ' '
      WRITE (6,'(''  Total fission cross section '',G12.5,'' mb'')')
     &       TOTcsfis
      WRITE (12,*) ' '
      WRITE (12,'('' Tot. fission cross section '',G12.4,'' mb'')')
     &       TOTcsfis

      WRITE (6,*)
      checkXS = checkXS + TOTcsfis
      IF(ABScs.GT.0.) THEN
        WRITE (6,'('' *******************************************'',
     &           23(1H*))')
        WRITE (6,'('' * Incident energy (LAB): '',G12.5,
     &              '' MeV  '')') EINl    
        WRITE (6,'('' * Compound elastic cross section (CE) '',G12.5,
     &              '' mb  '')') 4.*PI*ELCncs
        WRITE (6,'('' * Reaction cross section - CE '',G12.5,
     &              '' mb  '')') ABScs - 4.*PI*ELCncs
        WRITE (6,'('' * Production cross section + fission '',
     &           G12.5,'' mb'')')  checkXS - 4.*PI*ELCncs
        WRITE (6,'('' * Difference: '', F9.5,'' %'')')
     &            100.d0*abs((ABScs - checkXS))/
     &                    (ABScs - 4.*PI*ELCncs)
         WRITE (6,'('' *******************************************'',
     &           23(1H*))')
      ENDIF
      IF(abs(ABScs - checkXS).GT.0.01*ABScs) THEN
         WRITE (6,*)
        WRITE (6,'('' WARNING: Sum of production XS and fission XS'')')
        WRITE (6,'('' WARNING: is not equal reaction cross section'')')
        WRITE (6,'('' WARNING:     difference: '', F9.5,'' %'')')
     &            100.d0*abs((ABScs - 4.*PI*ELCncs - checkXS))/
     &                    (ABScs - 4.*PI*ELCncs)
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
      IF (.NOT.EXClusiv) THEN
C--------print spectra of residues
         reactionx = '(z,x)  '
         DO nnuc = 1, NNUcd    !loop over decaying nuclei
            IF (ENDf(nnuc).EQ.2) CALL PRINT_RECOIL(nnuc,reactionx)
         ENDDO !over decaying nuclei in ENDF spectra printout
C--------print inclusive gamma spectrum
         nspec = INT(EMAx(1)/DE) + 2
         IF (nspec.GT.NDECSE - 1) nspec = NDECSE - 1
         WRITE (12,*) ' '
         WRITE (12,*) ' Spectrum of gammas   (z,x)  ZAP=     0'
         WRITE (12,*) ' '
         WRITE (12,'('' Energy    mb/MeV'')')
         WRITE (12,*) ' '
         DO ie = 1, nspec - 1
            WRITE (12,'(F9.4,E15.5)') FLOAT(ie - 1)*DE, CSE(ie,0,0)
         ENDDO
C--------exact endpoint
         WRITE (12,'(F9.4,E15.5)') EMAx(1), CSE(nspec,0,0)
         WRITE (12,'(F9.4,E15.5)') EMAx(1), 0.d0
C--------print inclusive spectra of ejectiles
C--------neutrons
         nspec = INT((EMAx(1) - Q(1,1))/DE) + 2
         IF (nspec.GT.NDECSE - 1) nspec = NDECSE - 1
         WRITE (12,*) ' '
         WRITE (12,*) ' Spectrum of neutrons (z,x)  ZAP=     1'
         WRITE (12,'(30X,''A      n      g      l      e      s '')')
         WRITE (12,*) ' '
         WRITE (12,'('' Energy   '',8G15.5,/,(10X,8G15.5))') 
     &            (ANGles(nang),nang=1,NDANG)
         DO ie = 1, nspec - 1
           WRITE (12,'(F9.4,8E15.5,/,(9X,8E15.5))') FLOAT(ie - 1)*DE,
     &            (CSEa(ie,nang,1,0),nang = 1,NDANG)
         ENDDO
C--------exact endpoint
         WRITE (12,'(F9.4,8E15.5,/,(9X,8E15.5))') EMAx(1) - Q(1,1),
     &            (CSEa(nspec,nang,1,0),nang = 1,NDANG)
         WRITE (12,'(F9.4,8E15.5,/,(9X,8E15.5))') EMAx(1) - Q(1,1),
     &            (0.d0,nang = 1,NDANG)
C--------protons
         nspec = INT((EMAx(1) - Q(2,1))/DE) + 2
         IF(nspec.gt.0) then
          IF (nspec.GT.NDECSE - 1) nspec = NDECSE - 1
          WRITE (12,*) ' '
          WRITE (12,*) ' Spectrum of protons  (z,x)  ZAP=  1001'
          WRITE (12,'(30X,''A      n      g      l      e      s '')')
          WRITE (12,*) ' '
          WRITE (12,'('' Energy   '',8G15.5,/,(10X,8G15.5))') 
     &            (ANGles(nang),nang=1,NDANG)
          DO ie = 1, nspec - 1
            WRITE (12,'(F9.4,8E15.5,/,(9X,8E15.5))') FLOAT(ie - 1)*DE,
     &            (CSEa(ie,nang,2,0),nang = 1,NDANG)
          ENDDO
C---------exact endpoint
          WRITE (12,'(F9.4,8E15.5,/,(9X,8E15.5))') EMAx(1) - Q(2,1),
     &            (CSEa(nspec,nang,2,0),nang = 1,NDANG)
          WRITE (12,'(F9.4,8E15.5,/,(9X,8E15.5))') EMAx(1) - Q(2,1),
     &            (0.d0,nang = 1,NDANG)
         ENDIF
C--------alphas
         nspec = INT((EMAx(1) - Q(3,1))/DE) + 2
         IF(nspec.gt.0) then
          IF (nspec.GT.NDECSE - 1) nspec = NDECSE - 1
          WRITE (12,*) ' '
          WRITE (12,*) ' Spectrum of alphas   (z,x)  ZAP=  2004'
          WRITE (12,'(30X,''A      n      g      l      e      s '')')
          WRITE (12,*) ' '
          WRITE (12,'('' Energy   '',8G15.5,/,(10X,8G15.5))') 
     &            (ANGles(nang),nang=1,NDANG)
          DO ie = 1, nspec - 1
            WRITE (12,'(F9.4,8E15.5,/,(9X,8E15.5))') FLOAT(ie - 1)*DE,
     &            (CSEa(ie,nang,3,0),nang = 1,NDANG)
          ENDDO
C---------exact endpoint
          WRITE (12,'(F9.4,8E15.5,/,(9X,8E15.5))') EMAx(1) - Q(3,1),
     &            (CSEa(nspec,nang,3,0),nang = 1,NDANG)
          WRITE (12,'(F9.4,8E15.5,/,(9X,8E15.5))') EMAx(1) - Q(3,1),
     &            (0.d0,nang = 1,NDANG)
         ENDIF
C--------light ions
         IF (NDEJC.EQ.4) THEN
           nspec = INT((EMAx(1) - Q(NDEJC,1))/DE) + 2
           IF(nspec.gt.0) then
             IF (nspec.GT.NDECSE - 1) nspec = NDECSE - 1
             WRITE (12,*) ' '
             WRITE (12,
     &'(''  Spectrum of  '',I1,''-'',A2,4X,                          ''(
     &n,x)'')') INT(AEJc(NDEJC)), SYMbe(NDEJC), ' ZAP=', IZAejc(NDEJC)
             WRITE(12,'(30X,''A      n      g      l      e      s '')')
             WRITE (12,*) ' '
             WRITE (12,'('' Energy   '',8G15.5,/,(10X,8G15.5))') 
     &           (ANGles(nang),nang=1,NDANG)
             DO ie = 1, nspec - 1
               WRITE (12,'(F9.4,8E15.5,/,(9X,8E15.5))') FLOAT(ie - 1)
     &                *DE, (CSEa(ie,nang,NDEJC,0),nang = 1,NDANG)
             ENDDO
C------------exact endpoint
             WRITE (12,'(F9.4,8E15.5,/,(9X,8E15.5))') EMAx(1)
     &           - Q(NDEJC,1), (CSEa(nspec,nang,NDEJC,0),nang = 1,NDANG)
             WRITE (12,'(F9.4,8E15.5,/,(9X,8E15.5))') EMAx(1)
     &           - Q(NDEJC,1), (0.d0,nang = 1,NDANG)
           ENDIF
         ENDIF
      ENDIF
C-----end of ENDF spectra (inclusive)

      IF (ENDf(1).GT.0. .AND. EINl.GE.1.d0) THEN
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
               controlg = controlg + POPcseaf(0,0,ispec,nnuc)
               controln = controln + POPcseaf(0,1,ispec,nnuc)
               controlp = controlp + POPcseaf(0,2,ispec,nnuc)
               controla = controla + POPcseaf(0,3,ispec,nnuc)
            ENDDO
            WRITE (6,'(5g15.5)') (ispec - 1)*DE, controlg,
     &                       controln, controlp, controla
         ENDDO
      ENDIF

 1155 READ (5,'(A36)') nextenergy
      IF(nextenergy(1:1).EQ.'$') THEN
         READ(nextenergy,'(1x,A6,G10.5,4I5)') keyname, val, ikey1, 
     &       ikey2, ikey3, ikey4 
         CALL OPTIONS(keyname, val, ikey1, ikey2, ikey3, ikey4, 1)
         GO TO 1155
      ELSE
         READ(nextenergy,'(G15.5)') EIN
      ENDIF
       
      IF (EIN.LT.0.0D0) THEN
         IF (FILevel) CLOSE (14)
         WRITE (12,*) ' '
         WRITE (6,*) ' '
         WRITE (6,*) ' CALCULATIONS COMPLETED SUCCESSFULLY'
         CLOSE (15,STATUS = 'delete')
         CLOSE (16,STATUS = 'delete')
         CLOSE (66,STATUS = 'delete')
         WRITE (*,*) '.'
         CALL THORA(6)
C        SAVING RANDOM SEEDS
         ftmp = grand()
         OPEN(94,file='R250SEED.DAT',status='UNKNOWN')
         write(94,*)  indexf, indexb
         Do i = 1, 250
          write(94,*) buffer(i)
         ENDDO
         CLOSE(94)
         STOP '.REGULAR STOP'
      ENDIF
       IF(EIN.LT.epre) THEN
         WRITE(6,*) 'FATAL: Input energies are not ordered !!'
         WRITE(6,*) 'FATAL: Check your input file'
         PAUSE 'FATAL: Input energies are not ordered !!'
         STOP
       ENDIF
      epre = EIN

C-----
C-----Initialized in input.f
C     NANgela = 19
C     NDAng   = 19
C-----
      IF(EIN.GT.10. .AND. EIN.LE.20.) THEN
        NANgela = 37
        NDAng   = 37
      ENDIF 
      IF(EIN.GT.20. .AND. EIN.LE.50.) THEN
        NANgela = 73
        NDAng   = 73
      ENDIF 
      IF(EIN.GT.50.) THEN
        NANgela = 91
        NDAng   = 91
      ENDIF
      IF(NANgela.GT.NDAngecis) THEN
         WRITE(6,*)
     &        'FATAL: increase NANgecis in dimension.h up to ',NANgela
         STOP 'FATAL: increase NANgecis in dimension.h'
      ENDIF
      FIRst_ein = .FALSE.
      GOTO 1300
C
C     Accuracy of the outgoing energy increased by one digit
C
C99070FORMAT (I12,F10.4,I5,F8.1,G15.6,I3,7(I4,F7.4),:/,(53X,7(I4,F7.4)))
99070 FORMAT (I12,F10.5,I5,F8.1,G15.6,I3,7(I4,F7.4),:/,(53X,7(I4,F7.4)))
99075 FORMAT (1X,F5.2,12G10.3)
      END
C
C
      SUBROUTINE RECOIL(Ke,Nnuc)
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
C-----normalize recoil spectrum of the parent
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
C--------residual nuclei must be heavier than alpha
         if(ares.le.4. and. zres.le.2.) cycle
         izares = INT(1000.0*zres + ares)
         CALL WHERE(izares,nnur,iloc)
         if(iloc.eq.1) CYCLE
C--------decay to continuum
C--------recorr is a recoil correction factor that
C--------divides outgoing energies
         recorr = AMAss(Nnuc)/EJMass(nejc)
         exqcut = EX(Ke,Nnuc) - Q(nejc,Nnuc) - ECUt(nnur)
         DO ie = 1, NDECSE !over ejec. energy (daughter excitation)
            icse = (exqcut - (ie - 1)*DE)/DE + 1.001
C-----------daughter bin
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
C--------decay to discrete levels (stored with icse=0)
   50    exqcut = exqcut + ECUt(nnur)
         DO il = 1, NLV(nnur)
            erecod = exqcut - ELV(il,nnur)   !emission energy
            erecod = erecod/recorr
            IF (erecod.LT.0) GOTO 100
            DO ire = 1, NDEREC      !over recoil spectrum
               DO na = 1, NDANG !over angles
                  erecoil = (ire - 1)*DERec + erecod
                  erecoil = erecoil + 2.0*SQRT((ire - 1)*DERec*erecod)
     &                      *CANgler(na)
                  irec = erecoil/DERec + 1.001
                  weight = (erecoil - (irec - 1)*DERec)/DERec
                  IF (irec.GT.NDEREC) GOTO 60
                  RECcse(irec,0,nnur) = RECcse(irec,0,nnur)
     &                                  + RECcse(ire,Ke,Nnuc)
     &                                  *REClev(il,nejc)*(1.0 - weight)
     &                                  *SANgler(na)*coef
                  IF (irec + 1.GT.NDEREC) GOTO 60
                  RECcse(irec + 1,0,nnur) = RECcse(irec + 1,0,nnur)
     &               + RECcse(ire,Ke,Nnuc)*REClev(il,nejc)
     &               *weight*SANgler(na)*coef
               ENDDO                  !over angles
   60       ENDDO                  !over recoil spectrum
         ENDDO                  !over levels
  100 ENDDO                  !over ejectiles
C-----
C-----parent recoil spectrum after gamma decay
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
C-----prints recoil spectrum of nnuc residue
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
C-------normalize recoil spectra to remove eventual inaccuracy
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
C--------print end point again with 0 xs for consistency with particle spectra
         WRITE (12,'(F9.4,E15.5)') FLOAT(ilast - 1)*DERec,
     &                             RECcse(ilast + 1,0,Nnuc)
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
      DOUBLE PRECISION xnorfis, ares, zres

      Dencomp = DENhf - Sumfis
      IF (FISsil(Nnuc) .AND. FISopt(Nnuc).GT.0. .AND. FISshi(Nnuc)
     &    .NE.1.) THEN
         IF (FISmod(Nnuc).EQ.0.) THEN
            IF ((Dencomp + TDIr).GT.0.) THEN
               xnorfis = Xnor*DENhf/(Dencomp + TDIr)
            ELSE
               xnorfis = 0.
            ENDIF
C-----------fission
            CSFis = CSFis + xnorfis*(TDIr + Dencomp*Aafis)
     &              + xnorfis*PFIso
            IF (ENDf(Nnuc).EQ.1 .AND. CSFis.NE.0.0D+0)
     &          CALL EXCLUSIVEC(Ke,0, - 1,Nnuc,0,CSFis)

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
         ENDIF
C------------particles
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
C------------gammas
         CALL ACCUM(Ke,Nnuc,Nnuc,0,Xnor)
         CSEmis(0,Nnuc) = CSEmis(0,Nnuc) + xnorfis*SCRtem(0)*(1 - Aafis)
         POP(Ke,Jcn,Ipar,Nnuc) = 0.0
         RETURN
      ENDIF
C--------------no subbarrier effects
C--------particles
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
C--------gammas
      CALL ACCUM(Ke,Nnuc,Nnuc,0,Xnor)
      CSEmis(0,Nnuc) = CSEmis(0,Nnuc) + Xnor*SCRtem(0)
      POP(Ke,Jcn,Ipar,Nnuc) = 0.0
C--------fission
      IF (FISmod(Nnuc).EQ.0.) CSFis = CSFis + Sumfis*Xnor
      IF (FISmod(Nnuc).GT.0.) THEN
         DO M = 1, INT(FISmod(Nnuc)) + 1
            CSFism(M) = CSFism(M) + Sumfism(M)*Xnor
         ENDDO
      ENDIF
      END
