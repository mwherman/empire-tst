Ccc   * $Author: herman $
Ccc   * $Date: 2005-05-06 17:39:02 $
Ccc   * $Id: main.f,v 1.82 2005-05-06 17:39:02 herman Exp $
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
     &                 TFIso, TGIso, TISo, TOTcs,
     &                 UGRidf(0:NFISENMAX,NFMOD), WFIsm(NFMOD),
     &                 XMInnm(NFMOD)
      INTEGER BFFm(NFMOD), NRBinfism(NFMOD)
      INTEGER*4 INDexf, INDexb, BUFfer(250)
      COMMON /ECISXS/ ELAcs, TOTcs, ABScs, SINl
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
     &                 ecm, elada(101), elcncs, emeda, emedg, emedh,
     &                 emedn, emedp, erecoil, espec, espmax, ftmp,
     &                 gamfis, gamt, gang, gtotsp, htotsp, piece, pope,
     &                 poph, popl, popleft, poplev, popread, poptot,
     &                 ptotsp, q2, q3, qmax, qstep, recorp, recorr,
     &                 sgamc, spdif, spdiff, stauc, step, sum, sumfis,
     &                 sumfism(NFMOD), tauf, taut, totemis, weight,
     &                 xccm, xcse, xizat, xnhms, xnl, xnor, xtotsp,
     &                 xsinlcont, xsinl, zres
      CHARACTER*9 cejectile
      CHARACTER*6 ctldir
      CHARACTER*20 ctmp20
      DOUBLE PRECISION DMAX1
      REAL FLOAT
      INTEGER i, ia, iad, iam, iang, ib, icalled, iccmh, iccml, icse,
     &        icsh, icsl, ie, iizaejc, il, ilev, iloc, ilv, imaxt,
     &        imint, ip, ipar, irec, ispec, itimes, its, iz, izares, j,
     &        jcn, jj, ke, kemax, kemin, kk, ltrmax, m, mt2, mt649,
     &        mt849, mt91, nang, nbr, ncoll, nejc, nejcec, nelang, nnuc,
     &        nnur, nnurec, nnurn, nnurp, nrbarc1, nspec
      INTEGER INT, MIN0, NINT
      LOGICAL nvwful, fexist
      CHARACTER*21 reactionx
      INCLUDE 'io.h'
      DATA ctldir/'../TL/'/
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
C
C     On request, total cross section is set to absorption cross section
C                      for photon induced reactions
C
      IF (INT(AEJc(0)).EQ.0) TOTcs = CSFus

C-----locate position of the target among residues
      CALL WHERE(IZA(1) - IZAejc(0),nnurec,iloc)
C
      WRITE (ctmp20,'(i3.3,i3.3,1h_,i3.3,i3.3,1h_,i6.6)') INT(ZEJc(0)),
     &       INT(AEJc(0)), INT(Z(0)), INT(A(0)), INT(EINl*1000)
C     TOTcs, ABScs,ELAcs are initialized within MARENG()
      xsinlcont = 0.d0
      xsinl = 0.d0

      ncoll = 0
      nelang = 73
      ecm = EINl - EIN
      dang = 3.14159/FLOAT(nelang - 1)
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
         READ (46,*,END = 1400)
C--------locate position of the projectile among ejectiles
         CALL WHEREJC(IZAejc(0),nejcec,iloc)
                               ! To skip first line <INE.C.S.> ..
C--------get and add inelastic cross sections (including double-differential)
         DO i = 2, ND_nlv
            ilv = ICOller(i)
            IF (ilv.LE.NLV(nnurec)) THEN
C            ADDING INELASTIC TO DISCRETE LEVELS
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
C--------------Empire uses 10 deg grid for inelastic so we have to take
C--------------each 4th result from ECIS (2.5 deg grid)
               DO iang = 1, NDANG - 1
                  READ (45,'(7x,E12.5)',END = 1400)
     &                  CSAlev(iang,ilv,nejcec)
                  READ (45,'(7x,E12.5)',END = 1400)
                  READ (45,'(7x,E12.5)',END = 1400)
                  READ (45,'(7x,E12.5)',END = 1400)
               ENDDO
               READ (45,'(7x,E12.5)',END = 1400)
     &               CSAlev(NDANG,ilv,nejcec)
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
C            ADDING INELASTIC TO CONTINUUM  (D_Elv(ND_nlv) = elvr)
             echannel = EX(NEX(1),1) - Q(nejcec,1) - D_Elv(i)
             icsl = INT(echannel/DE + 1.0001)
C------------avoid reading closed channels
             IF (echannel.GE.0.0001 .and. icsl.gt.0 .and. nejcec.le.2)
     &       THEN
               READ (46,*,END = 1400) popread
C
C              This level is not counted as a discrete one
C              but it is embedded in the continuum
C
C              ncoll = i
               CSMsd(nejcec) = CSMsd(nejcec) + popread
               xsinlcont = xsinlcont + popread
               CSEmsd(icsl,nejcec) = CSEmsd(icsl,nejcec) + popread/DE
               READ (45,*,END = 1400)     ! Skipping level identifier line
C--------------Empire uses 10 deg grid for inelastic so we have to take
C--------------each 4th result from ECIS (2.5 deg grid)
               DO iang = 1, NDANG - 1
                  ftmp = 0.d0
                  READ (45,'(7x,E12.5)',END = 1400) ftmp
                  CSEa(icsl,iang,nejcec,1) =
     &               CSEa(icsl,iang,nejcec,1) + ftmp/DE
                  READ (45,'(7x,E12.5)',END = 1400)
                  READ (45,'(7x,E12.5)',END = 1400)
                  READ (45,'(7x,E12.5)',END = 1400)
               ENDDO
               READ (45,'(7x,E12.5)',END = 1400) ftmp
                 CSEa(icsl,NDAng,nejcec,1) =
     &               CSEa(icsl,NDAng,nejcec,1) + ftmp/DE
             ENDIF
C            END OF ADDING INELASTIC TO CONTINUUM
           ENDIF
 1350    ENDDO
      ENDIF
 1400 CLOSE (45)
      IF (DIRect.NE.0) CLOSE (46)
C-----print elastic and direct cross sections from ECIS
      WRITE (6,*) ' '
      WRITE (6,*) ' '

      IF (KTRlom(0,0).GT.0) THEN
      IF (DIRect.EQ.0) THEN
         WRITE (6,*)
     &       ' Results provided by Spherical Optical Model calculations'
         WRITE (6,*) ' '
      ELSEIF (DIRect.EQ.1 .OR. DIRect.EQ.2) THEN
         WRITE (6,*) ' Results provided by Coupled Channel calculations'
         WRITE (6,*) ' Inelastic scattering results provided by'
         WRITE (6,*) ' Coupled Channel + DWBA calculations'
         if(CSMsd(1)+CSMsd(2).NE.0.)
     >   WRITE (6,*) ' Some discrete levels are embedded into continuum'
         WRITE (6,*) ' '
      ELSEIF (DIRect.EQ.3) THEN
         WRITE (6,*)
     &       ' Results provided by Spherical Optical Model calculations'
         WRITE (6,*)
     &     ' Inelastic scattering results provided by DWBA calculations'
         if(xsinlcont.NE.0.)
     >   WRITE (6,*) ' Some discrete levels are embedded into continuum'
         WRITE (6,*) ' '
      ENDIF
      ENDIF
C
C-----ABScs = CSfus  always  !!!
C
      IF (KTRlom(0,0).GT.0) THEN

      IF (ZEJc(0).EQ.0 .AND. AEJc(0).GT.0) THEN
         WRITE (6,99005) TOTcs, CSFus, ELAcs
99005    FORMAT (/,2x,'Total cross section         :',e14.7,' mb',/,2x,
     &           'Absorption cross section    :',e14.7,' mb',/,2x,
     &           'Shape elastic cross section :',e14.7,' mb',//)
      ENDIF
      IF (ZEJc(0).NE.0 .OR. AEJc(0).EQ.0) THEN
         WRITE (6,99010) CSFus
99010    FORMAT (/,2x,'Absorption cross section    :',e14.7,' mb',//)
      ENDIF

      WRITE (6,99015)
99015 FORMAT (' ',46x,'SHAPE ELASTIC DIFFERENTIAL CROSS-SECTION',/,' ',
     &        46x,40('*'),/,' ',56x,'CENTER-OF-MASS SYSTEM',///)
      WRITE (6,99020)
99020 FORMAT (' ',5x,4('    TETA ',2x,'D.SIGMA/D.OMEGA',6x),/)
      gang = 180.0/(nelang - 1)
      DO iang = 1, nelang/4 + 1
         imint = 4*(iang - 1) + 1
         imaxt = MIN0(4*iang,nelang)
         WRITE (6,99025) ((j - 1)*gang,elada(j),j = imint,imaxt)
99025    FORMAT (' ',5x,4(1p,e12.5,2x,e12.5,6x))
      ENDDO
      WRITE (6,'(//)')

      IF (ncoll.GT.0) THEN
C--------locate position of the projectile among ejectiles
         CALL WHEREJC(IZAejc(0),nejcec,iloc)
         WRITE (6,*) ' '
         gang = 180.0/(NDANG - 1)
         IF (CSAlev(1,ICOller(2),nejcec).GT.0) THEN
            WRITE (6,99030) (ICOller(ilv),ilv = 2,MIN(ncoll,10))
99030       FORMAT ('  Angle ',10(6x,i2,'-level'))
            WRITE (6,*) ' '
            DO iang = 1, NDANG
               IF (CSAlev(1,ICOller(2),nejcec).GT.0) THEN
                  WRITE (6,99035) (iang - 1)*gang,
     &                            (CSAlev(iang,ICOller(ilv),nejcec),
     &                            ilv = 2,MIN(ncoll,10))
99035             FORMAT (1x,f5.0,3x,11(2x,E12.6))
               ENDIF
            ENDDO
            WRITE (6,*) ' '
            WRITE (6,99040) (POPlv(ICOller(ilv),nnurec),ilv = 2,
     &                      MIN(ncoll,10))
99040       FORMAT (6x,3x,11(2x,E12.6))
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
C-----
C-----calculate MSD contribution
C-----
      corrmsd = 1.0
      IF (MSD.NE.0 .AND. EIN.GT.1.D0) THEN
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

         IF (MSD.NE.2) THEN
            q2 = qmax
            q3 = qmax
 1420       CALL ORION(q2,q3,1,EIN,NLW,1,ltrmax,A(0),Z(0),AEJc(0),
     &                 ZEJc(0),IOUt,ANGles,NDANG,ICOmpff)
            WRITE (6,
     &'('' ORION calculated for Q2='', F7.3, '' and Q3='',            F7
     &.3)') q2, q3
            q2 = q2 - qstep
            IF (q2.LT.( - 0.0001D0)) THEN
               q3 = q3 - qstep
               IF (q3.LT.( - 0.0001D0)) GOTO 1450
               q2 = q3
            ENDIF
C-----------set to Q's to 0 if negative due to rounding error
            IF (q2.LT.0.0D0) q2 = 0.0
            IF (q3.LT.0.0D0) q3 = 0.0
            GOTO 1420
         ENDIF
 1450    REWIND 15
         READ(15,*) qmax,qstep,ltrmax
         WRITE (6,*) ' '
         WRITE (6,*) ' '
         CALL ULM(1)
         CALL TRISTAN(0,0,ltrmax,qmax,qstep,xsinl)
        CLOSE(15)
      ENDIF

C-----PCROSS exciton model calculations of preequilibrium contribution
C-----including cluster emission by Iwamoto-Harada model
C-----
      totemis = 0.d0
      IF (EIN.GT.0.1D0 .AND. PEQc.GT.0) THEN
         ftmp = CSFus - xsinl - xsinlcont
         CALL PCROSS(ftmp,totemis)
      ENDIF          ! PCRoss done

      IF ((xsinl + xsinlcont + totemis).gt.0.) THEN
C--------print PE double differential cross sections
C        DO nejc = 0,NDEjc
         nejc = 1
         ares = A(1) - AEJc(nejc)
         zres = Z(1) - ZEJc(nejc)
         izares = INT(1000.0*zres + ares)
         CALL WHERE(izares,nnur,iloc)
         IF (iloc.EQ.1) THEN
           WRITE (6,*) ' RESIDUAL NUCLEUS WITH A=', ares, ' AND Z=',
     &                  zres, ' HAS NOT BEEN INITIALIZED'
           WRITE (6,*) ' EXECUTION STOPPED'
           STOP
         ENDIF

         IF (CSMsd(nejc).GT.0.1D0 .AND. IOUt.GE.3) THEN
            itimes = FLOAT(NDANG)/11.0 + 0.95
            DO its = 1, itimes
              iad = 1 + (its - 1)*11
              iam = 11 + (its - 1)*11
              iam = MIN0(NDANG,iam)
              WRITE (6,
     &                '(//30X,''     N  E  U  T  R  O  N  S ''/)')
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
     &                   (CSEa(i,iang,nejc,1),iang = iad,iam)
              ENDDO
              WRITE (6,*) ' '
            ENDDO
         ENDIF
C        ENDDO
         WRITE (6,*) ' g PE emission cross section ', CSMsd(0), ' mb'
         WRITE (6,*) ' n PE emission cross section ', CSMsd(1), ' mb'
         WRITE (6,*) ' p PE emission cross section ', CSMsd(2), ' mb'
         WRITE (6,*) ' a PE emission cross section ', CSMsd(3), ' mb'
      IF(NEMc.GT.0) WRITE (6,*)
     &     ' Cluster PE emission cross section ', CSMsd(ndejc), ' mb'
         WRITE (6,*) ' '
C--------correct CN population for the PE emission
         corrmsd = (CSFus - (xsinl + xsinlcont + totemis))/CSFus
         IF (corrmsd.LT.0.0D0) THEN
            WRITE (6,*) ' '
            WRITE (6,*) 'MSD EMISSION LARGER THEN FUSION CROSS SECTION'
            IF (ICOmpff.GT.0) THEN
               WRITE (6,*) 'TRY TO TURN OFF COMPRESSIONAL FORM FACTOR '
               WRITE (6,*) 'SETTING COMPFF TO 0 IN THE OPTIONAL INPUT.'
               STOP
            ELSE
               WRITE (6,*) 'THIS MAY HAPPEN IF RESPONSE FUNCTIONS ARE '
               WRITE (6,*) 'RENORMALIZED IN INPUT OR FITTED TO WRONG '
               WRITE (6,*) 'DISCRET LEVELS. CHECK `EFIT` AND `RESNOR` '
               WRITE (6,*) 'IN OPTIONAL INPUT.    '
               WRITE (6,*)
     &                    'IF THESE ARE FINE TRY ANOTHER OPTICAL MODEL.'
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
           ares = A(1) - AEJc(nejc)
           zres = Z(1) - ZEJc(nejc)
           izares = INT(1000.0*zres + ares)
           CALL WHERE(izares,nnur,iloc)
           IF (iloc.EQ.1) THEN
             WRITE (6,*) ' RESIDUAL NUCLEUS WITH A=', ares, ' AND Z=',
     &                  zres, ' HAS NOT BEEN INITIALIZED'
             WRITE (6,*) ' EXECUTION STOPPED'
             STOP
           ENDIF
           IF (CSMsd(nejc).NE.0.0D0) CALL ACCUMSD(1,nnur,nejc)
C----------add PE contribution to energy spectra (angle int.)
           DO ie = 1, NDEcse
              CSE(ie,nejc,1) = CSE(ie,nejc,1) + CSEmsd(ie,nejc)
           ENDDO
C----------add PE contribution to the total NEJC emission
           CSEmis(nejc,1) = CSEmis(nejc,1) + CSMsd(nejc)
         ENDDO
C        Skipping all emitted but neutrons and protons
         ares = A(1) - AEJc(1)
         zres = Z(1) - ZEJc(1)
         izares = INT(1000.0*zres + ares)
         CALL WHERE(izares,nnur,iloc)
C--------second chance preequilibrium emission after MSD emission
C--------neutron emission
         izares = INT(1000.0*Z(nnur) + A(nnur) - 1)
         CALL WHERE(izares,nnurn,iloc)
         IF (iloc.EQ.0) CALL SCNDPREEQ(nnur,nnurn,1,0)
         IF (iloc.EQ.0 .AND. IOUt.GT.3) CALL AUERST(nnur,1)
C--------proton emission
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
C-----
C-----MSD *** done ***
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
            WRITE (6,'(''   PE inelastic to continuum = '',
     &  G13.6,'' mb'')') xsinl + totemis
         ELSEIF (DIRect.EQ.1 .OR. DIRect.EQ.2) THEN
            WRITE (6,
     &'(''   Fusion cross section = '',G13.6,
     &  '' mb including'')') CSFus
            WRITE (6,
     &'(''   CC+DWBA inelastic to discrete levels = '',
     &  G13.6,'' mb'')') SINl
            WRITE (6,'(''   DWBA to continuum = '',
     &  G13.6,'' mb'')') xsinlcont
            WRITE (6,'(''   PE inelastic to continuum = '',
     &  G13.6,'' mb'')') xsinl + totemis
            WRITE (6,
     &'(''   Spin distribution calculated using '',
     &  ''CC transmission coefficients'')')
         ELSEIF (DIRect.EQ.3) THEN
            WRITE (6,
     &'(''   Fusion cross section = '',G13.6,
     &  '' mb including'')') CSFus
            WRITE (6,
     &'(''   DWBA inelastic to discrete levels = '',
     &  G13.6,'' mb'')') SINl
            WRITE (6,'(''   DWBA to continuum = '',
     &  G13.6,'' mb'')') xsinlcont
            WRITE (6,'(''   PE inelastic to continuum = '',
     &  G13.6,'' mb'')') xsinl + totemis
            WRITE (6,
     &'(''   Spin distribution does NOT contain'',
     &  '' DWBA inelastic contribution '')')
         ENDIF
      ENDIF
      IF (ENDf(1).EQ.0.0D0) THEN
         WRITE (12,'('' FUSION CROSS SECTION = '',G13.6, '' mb'')')
     &          CSFus
      ELSE
         WRITE (12,*) ' '
         WRITE (12,'('' FUSION CROSS SECTION = '',G12.5,'' mb'')') CSFus
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
               IF(dtmp.LE.1.e-10) GOTO 1460

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
C--------locate residual nuclei
         DO nejc = 0, NEJcm
            NREs(nejc) = -1
            ares = A(nnuc) - AEJc(nejc)
            zres = Z(nnuc) - ZEJc(nejc)
C           EMITTED NUCLEI MUST BE HEAVIER THAN ALPHA !! (RCN)
            if(ares.le.4. and. zres.le.2.) cycle
            izares = INT(1000.0*zres + ares)
            CALL WHERE(izares,nnur,iloc)
            IF (iloc.EQ.1) THEN
               WRITE (6,*) ' RESIDUAL NUCLEUS WITH A=', ares, ' AND Z=',
     &                     zres, ' HAS NOT BEEN INITIALIZED'
               WRITE (6,*) ' EXECUTION STOPPED'
            ENDIF
            NREs(nejc) = nnur
         ENDDO
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
         IF (nnuc.EQ.1 .AND. EIN.GT.0.1D0 .AND. LHMs.NE.0) THEN
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
         IF (LHRtw.EQ.1 .AND. EIN.GT.5.0D+0) LHRtw = 0
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
C                    EMITTED NUCLEI MUST BE HEAVIER THAN ALPHA !! (RCN)
                     if(NRES(nejc).lt.0) cycle
                     nnur = NREs(nejc)
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
C
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
C-----------------------below is a 2.18 version approach (straight to the level)
C                          POPlv(il, nnuc) = POPlv(il, nnuc)
C    &                        + POP(ke, jcn, ipar, nnuc)*ded/xnl
C                          REClev(il, 0) = REClev(il, 0)
C    &                        + POP(ke, jcn, ipar, nnuc)*ded/xnl
C------------------------2.19 uses standard approach through the SCRtl matrix
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
 1470          ENDDO                   !loop over decaying nucleus spin
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
 1500    IF (IOUt.GT.0) WRITE (6,
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
         IF (ENDf(nnuc).GT.0 .AND.
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
C99065        FORMAT (I12,F10.4,I5,F8.1,G15.6,I3,7(I4,E11.4),:/,
C    &                 (53X,7(I4,E11.4)))
            ENDDO
            WRITE (12,'(1X,/,10X,40(1H-),/)')
         ENDIF
C--------gamma decay of discrete levels (DECAYD)
         CALL DECAYD(nnuc)
         ia = INT(A(nnuc))
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
               WRITE (6,'(15X,5g15.5)') gtotsp, xtotsp, ptotsp, atotsp,
     &                                  htotsp
               WRITE (6,'(''E-aver.'',8X,6g15.5)') emedg, emedn, emedp,
     &                emeda, emedh, emedg + emedn + emedp + emeda +
     &                emedh
               WRITE (6,*) '-----------------------------------------'
               WRITE (6,*) ' '
               WRITE (6,*)
     &                  '----------------------------------------------'
               WRITE (6,*) 'Test printout (portions of DDX spectra)'
               WRITE (6,'(''     Energy'',8x,'' gamma '',9x,''neutron'',
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
               WRITE (6,'('' Average total   life-time'',G12.5,'' s'')')
     &                taut
               gamt = sgamc/2.0/PI/csemist
               WRITE (6,
     &                '('' Average total   width    '',G12.5,'' MeV'')')
     &                gamt
C              TAUT=6.589E-22/GAMT
C              WRITE(6,'('' Average total life-time 1/width'',g12.5,
C              1       '' s'')') TAUT
            ENDIF
            IF (CSFis.NE.0.0D0) THEN
               tauf = stauc*6.589E-22*2.0*PI/CSFis
               WRITE (6,'('' Average fission life-time'',G12.5,'' s'')')
     &                tauf
               gamfis = gamt*CSFis/csemist
               WRITE (6,
     &  '('' Average fission width    '',G12.5,'' MeV'')               '
     &  ) gamfis
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
               WRITE (6,*) ' '
               WRITE (6,
     &            '('' Fission    cross section    '',G12.5,'' mb'')'
     &             ) CSFis
            ENDIF
         ENDIF
         TOTcsfis = TOTcsfis + CSFis
C--------add compound elastic to shape elastic before everything falls
C--------down on the ground state
9876     IF (nnuc.EQ.1 .AND. INT(AEJc(0)).NE.0
     &                       .AND. POPlv(LEVtarg,mt2).GT.0.) THEN
            WRITE (6,*)
            WRITE (6,*) 'Incident energy (CMS)      ', EIN, ' MeV'
            WRITE (6,*) 'Shape elastic cross section', ELAcs, ' mb'
            WRITE (6,*) 'CN elastic cross section   ', POPlv(1,mt2),
     &                  ' mb'
            ELAcs = ELAcs + POPlv(LEVtarg,mt2)
C-----------CN contribution to elastic ddx
            elcncs = POPlv(LEVtarg,mt2)/4.0/PI
            WRITE (6,*) 'CN elastic angular distrib.', elcncs, ' mb/str'
            WRITE (6,*)
         ENDIF
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
C            EMITTED NUCLEI MUST BE HEAVIER THAN ALPHA !! (RCN)
             if(NRES(nejc).lt.0) cycle
             WRITE (12,
     &             '(1X,A2,'' emission cross section'',G12.5,'' mb'')')
     &             SYMbe(nejc), CSEmis(nejc,nnuc)
             nnur = NREs(nejc)
             IF (IOUt.GT.2) CALL AUERST(nnuc,nejc)
             IF (IOUt.GT.0) WRITE (6,
     &               '(2X,A2,'' emission cross section'',G12.5,'' mb'')'
     &               ) SYMbe(nejc), CSEmis(nejc,nnuc)
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
C------------This block should go away with the new way of treating exclusive
C------------spectra. We do not want to print them as they are identical to
C------------those printed already before for pure MSD (no CN is added)
C------------print double differential cross sections
C            IF(CSMsd(nejc).GT.0.D0 .AND. IOUt.GE.3 .AND. nnuc.EQ.1)THEN
C              itimes = FLOAT(NDANG)/11.0 + 0.95
C              DO its = 1, itimes
C                 iad = 1 + (its - 1)*11
C                 iam = 11 + (its - 1)*11
C                 iam = MIN0(NDANG, iam)
C                 WRITE(6, *)' '
C                 WRITE(6,
C    &                 '(30X,''A      n      g      l      e      s '')'
C    &                 )
C                 WRITE(6, *)' '
C                 WRITE(6, '('' Energy  '',11(4X,F5.1,2X))')
C    &                  (ANGles(ia), ia = iad, iam)
C                 WRITE(6, *)' '
C                 DO i = 1, NEX(1)
C                    WRITE(6, '(1X,F7.3,1X,11E11.4)')FLOAT(i - 1)*DE,
C    &                     (CSEa(i, iang, nejc, 1), iang = iad, iam)
C                 ENDDO
C                 WRITE(6, *)' '
C              ENDDO
C            ENDIF
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
                  IF (POPcs(nejc,nnuc).EQ.0) GOTO 1530
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
     &                      ANGles
                     IF ((nnuc.EQ.mt91 .AND. nejc.EQ.1) .OR.
     &                   (nnuc.EQ.mt649 .AND. nejc.EQ.2)) THEN
                                                              ! first emission reactions
C-----------------------(discrete levels part)
                        DO il = 1, NLV(nnuc)  !(levels)
                           espec = (EMAx(nnuc) - ELV(il,nnuc))/recorp
                           IF (espec.GE.0) WRITE (12,
     &'(F10.5,E14.5,7E15.5,/,                                       (9X,
     &8E15.5))') -espec, (CSAlev(nang,il,nejc)*recorp/DE,nang = 1,NDANG)
                        ENDDO
C-----------------------(continuum part)
                        DO ie = 1, nspec + 1
                                           ! clean DDX matrix
                           DO nang = 1, NDANG
                              CSEa(ie,nang,nejc,0) = 0.0
                           ENDDO
                        ENDDO
                        IF (nspec.GE.1) THEN
                           DO ie = 1, nspec ! reconstruct continuum DDX spectrum
                              DO nang = 1, NDANG
                                 piece = CSEmsd(ie,nejc)
                                 IF (ie.EQ.NEXr(nejc,1))
     &                               piece = 0.5*piece
                                 CSEa(ie,nang,nejc,0)
     &                              = ((POPcse(0,nejc,ie,nnuc)
     &                              - piece*POPcseaf(0,nejc,ie,nnuc))
     &                              /4.0/PI + CSEa(ie,nang,nejc,1)
     &                              *POPcseaf(0,nejc,ie,nnuc))
C                                WRITE(6,*)'ie,nang,nnuc,nejc',
C    &                              ie,nang,nnuc,nejc
C                                WRITE(6,*)'POPcse,piece,POPcseaf',
C    &                              POPcse(0,nejc,ie,nnuc),piece,
C    &                              POPcseaf(0,nejc,ie,Nnuc)
                              ENDDO
                           ENDDO
                        ENDIF
                        DO nang = 1, NDANG
                                          !double the first bin to preserve integral in EMPEND
                           CSEa(1,nang,nejc,0) = CSEa(1,nang,nejc,0)*2.0
                        ENDDO
                        DO ie = 1, nspec - 1
                                           ! print DDX spectrum
                           WRITE (12,
     &'(F10.5,E14.5,7E15.5,/,                                 (9X,8E15.5
     &))') FLOAT(ie - 1)*DE/recorp,
     &     (CSEa(ie,nang,nejc,0)*recorp,nang = 1,NDANG)
                        ENDDO
                        DO ie = nspec, nspec + 1
                                               ! exact DDX spectrum endpoint
                           WRITE (12,
     &'(F10.5,E14.5,7E15.5,/,                                 (9X,8E15.5
     &))') EMAx(nnuc)/recorp,
     &     (CSEa(ie,nang,nejc,0)*recorp,nang = 1,NDANG)
                        ENDDO
                     ELSE
C-----------------------remaining n- or p-emissions (continuum and levels together)
                        DO ie = 1, nspec + 1
                                           ! clean DDX matrix
                           DO nang = 1, NDANG
                              CSEa(ie,nang,nejc,0) = 0.0
                           ENDDO
                        ENDDO
                        DO ie = 1, nspec  ! reconstruct DDX spectrum
                           DO nang = 1, NDANG
                              piece = CSEmsd(ie,nejc)
                              IF (ie.EQ.NEXr(nejc,1)) piece = 0.5*piece
                              CSEa(ie,nang,nejc,0)
     &                           = ((POPcse(0,nejc,ie,nnuc)
     &                           - piece*POPcseaf(0,nejc,ie,nnuc))
     &                           /4.0/PI + CSEa(ie,nang,nejc,1)
     &                           *POPcseaf(0,nejc,ie,nnuc))
                           ENDDO
                        ENDDO
                        DO nang = 1, NDANG
                                          !double the first bin to preserve integral in EMPEND
                           CSEa(1,nang,nejc,0) = CSEa(1,nang,nejc,0)*2.0
                        ENDDO
                        DO ie = 1, nspec - 1
                                           ! print DDX spectrum
                           WRITE (12,
     &'(F10.5,E14.5,7E15.5,/,                                 (9X,8E15.5
     &))') FLOAT(ie - 1)*DE/recorp,
     &     (CSEa(ie,nang,nejc,0)*recorp,nang = 1,NDANG)
                        ENDDO
                        DO ie = nspec, nspec + 1
                                               ! exact DDX spectrum endpoint
                           WRITE (12,
     &'(F10.5,E14.5,7E15.5,/,                                 (9X,8E15.5
     &))') EMAx(nnuc)/recorp,
     &     (CSEa(ie,nang,nejc,0)*recorp,nang = 1,NDANG)
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
                        DO ie = nspec, nspec + 1
                                                ! MT=849 exact endpoint
                           WRITE (12,'(F10.5,E14.5)') EMAx(nnuc)/recorp,
     &                            POPcse(0,nejc,ie,nnuc)*recorp
                        ENDDO
                     ELSE  !all other emissions (continnum and levels together)
                        DO ie = 1, nspec - 1
                           WRITE (12,'(F10.5,E14.5)') FLOAT(ie - 1)
     &                            *DE/recorp, POPcse(0,nejc,ie,nnuc)
     &                            *recorp
                        ENDDO
                        DO ie = nspec, nspec + 1
                                                ! exact endpoint
                           WRITE (12,'(F10.5,E14.5)') EMAx(nnuc)/recorp,
     &                            POPcse(0,nejc,ie,nnuc)*recorp
                        ENDDO
                     ENDIF
                  ENDIF
 1530          ENDDO  ! over ejectiles
               IF (nnuc.NE.1) CALL PRINT_RECOIL(nnuc,REAction(nnuc))
            ENDIF
         ENDIF
      ENDDO  ! over decaying nuclei
C-----Fission related spectra of particles and gammas
C     IF (ENDf(nnuc).GT.0) THEN
      IF (ENDf(nnuc).EQ.-1.) THEN
         IF (TOTcsfis.GT.0.0D0) THEN
            DO nejc = 0, NDEJC         !loop over ejectiles
C              IF(POPCS(nejc,nnuc).EQ.0) CYCLE
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
C--------------double the first bin x-sec to preserve integral in EMPEND
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
               DO ie = nspec, nspec + 1
                  WRITE (12,'(F10.5,E14.5)') espmax, CSEfis(ie,nejc)
               ENDDO
            ENDDO  ! over ejectiles
         ENDIF
      ENDIF
C-----
C-----ENDF spectra inclusive representation
C-----
C-----
C-----
C-----sum exclusive energy spectra and double-differential cross
C-----sections into inclusive ones transforming them into CM
C-----(reduce channel energy to account for recoils)
C-----and store them on the 0 nucleus (target)
C-----NOTE: HMS cumulative spectra (if calculated) are already
C-----stored in CSE(.,x,0) array
C-----
      DO nnuc = 1, NNUcd               !loop over decaying nuclei
         IF (ENDf(nnuc).EQ.2) THEN
            DO nejc = 0, NEJcm
               IF (nejc.GT.0) THEN
                  recorr = (AMAss(nnuc) - EJMass(nejc))/AMAss(nnuc)
               ELSE
                  recorr = 1.0
               ENDIF
               DO icse = 1, NDEX
                  xccm = (icse - 1)*recorr + 1.0000001
                  iccml = xccm
                  iccmh = MIN(NDEX,iccml + 1)
                  weight = xccm - iccml
C-----------------energy spectra
                  CSE(iccml,nejc,0) = CSE(iccml,nejc,0)
     &                                + CSE(icse,nejc,nnuc)
     &                                *(1.0 - weight)
C-----------------double contribution to the first energy bin to
C-----------------to conserve the integral
                  IF (iccml.EQ.1 .AND. icse.NE.1) CSE(iccml,nejc,0)
     &                = CSE(iccml,nejc,0) + CSE(icse,nejc,nnuc)
     &                *(1.0 - weight)
                  CSE(iccmh,nejc,0) = CSE(iccmh,nejc,0)
     &                                + CSE(icse,nejc,nnuc)*weight
C-----------------double-differential spectra
                  IF (nnuc.EQ.1) THEN
                                     !CN with possibly anisotropic distr.
                     DO nang = 1, NDANG
                        CSEa(iccml,nang,nejc,0)
     &                     = CSEa(iccml,nang,nejc,0)
     &                     + CSEa(icse,nang,nejc,1)*(1.0 - weight)
C-----------------------double contribution to the first energy bin
C-----------------------to conserve the integral
                        IF (iccml.EQ.1 .AND. icse.NE.1)
     &                      CSEa(iccml,nang,nejc,0)
     &                      = CSEa(iccml,nang,nejc,0)
     &                      + CSEa(icse,nang,nejc,1)*(1.0 - weight)
                        CSEa(iccmh,nang,nejc,0)
     &                     = CSEa(iccmh,nang,nejc,0)
     &                     + CSEa(icse,nang,nejc,1)*weight
                     ENDDO
                  ELSE !other residues with isotropic ang. distributions
                     piece = CSE(icse,nejc,nnuc)/4.0/PI
                     DO nang = 1, NDANG
                        CSEa(iccml,nang,nejc,0)
     &                     = CSEa(iccml,nang,nejc,0)
     &                     + piece*(1.0 - weight)
C-----------------------double contribution to the first energy bin to
C-----------------------to conserve the integral
                        IF (iccml.EQ.1 .AND. icse.NE.1)
     &                      CSEa(iccml,nang,nejc,0)
     &                      = CSEa(iccml,nang,nejc,0)
     &                      + piece*(1.0 - weight)
                        CSEa(iccmh,nang,nejc,0)
     &                     = CSEa(iccmh,nang,nejc,0) + piece*weight
                     ENDDO
                  ENDIF
               ENDDO
            ENDDO
         ENDIF
      ENDDO
      WRITE (6,*) ' '
      WRITE (6,'(''  Total fission cross section '',G12.5,'' mb'')')
     &       TOTcsfis
      WRITE (12,*) ' '
      WRITE (12,'('' Tot. fission cross section '',G12.4,'' mb'')')
     &       TOTcsfis
      IF (IOUt.GT.1) THEN
         csemax = 0.
         DO nejc = 0, NEJcm
            DO i = 1, NDEX
               csemax = DMAX1(CSE(i,nejc,0),csemax)
            ENDDO
         ENDDO
         IF (csemax.GT.0.D0) THEN
            IF(ENDF(1).EQ.2) THEN
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
      IF (ENDf(1).EQ.2) THEN
C--------print spectra of residues
         reactionx = '(z,x)  '
         DO nnuc = 1, NNUcd    !loop over decaying nuclei
            CALL PRINT_RECOIL(nnuc,reactionx)
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
         CSE(nspec + 1,0,0) = 0.0D0
         DO ie = nspec, nspec + 1
                                 ! exact endpoint
            WRITE (12,'(F9.4,E15.5)') EMAx(1), CSE(ie,0,0)
         ENDDO
C--------print inclusive spectra of ejectiles
C--------neutrons
         nspec = INT((EMAx(1) - Q(1,1))/DE) + 2
         IF (nspec.GT.NDECSE - 1) nspec = NDECSE - 1
         WRITE (12,*) ' '
         WRITE (12,*) ' Spectrum of neutrons (z,x)  ZAP=     1'
         WRITE (12,'(30X,''A      n      g      l      e      s '')')
         WRITE (12,*) ' '
         WRITE (12,'('' Energy   '',8G15.5,/,(10X,8G15.5))') ANGles
         DO ie = 1, nspec - 1
            WRITE (12,'(F9.4,8E15.5,/,(9X,8E15.5))') FLOAT(ie - 1)*DE,
     &             (CSEa(ie,nang,1,0),nang = 1,NDANG)
         ENDDO
         DO ie = nspec, nspec + 1
                                 ! exact endpoint
            WRITE (12,'(F9.4,8E15.5,/,(9X,8E15.5))') EMAx(1) - Q(1,1),
     &             (CSEa(ie,nang,1,0),nang = 1,NDANG)
         ENDDO
C--------protons
         nspec = INT((EMAx(1) - Q(2,1))/DE) + 2
          IF(nspec.gt.0) then
           IF (nspec.GT.NDECSE - 1) nspec = NDECSE - 1
           WRITE (12,*) ' '
           WRITE (12,*) ' Spectrum of protons  (z,x)  ZAP=  1001'
           WRITE (12,'(30X,''A      n      g      l      e      s '')')
           WRITE (12,*) ' '
           WRITE (12,'('' Energy   '',8G15.5,/,(10X,8G15.5))') ANGles
           DO ie = 1, nspec - 1
             WRITE (12,'(F9.4,8E15.5,/,(9X,8E15.5))') FLOAT(ie - 1)*DE,
     &             (CSEa(ie,nang,2,0),nang = 1,NDANG)
           ENDDO
           DO ie = nspec, nspec + 1
                                ! exact endpoint
             WRITE (12,'(F9.4,8E15.5,/,(9X,8E15.5))') EMAx(1) - Q(2,1),
     &             (CSEa(ie,nang,2,0),nang = 1,NDANG)
           ENDDO
          ENDIF
C--------alphas
         nspec = INT((EMAx(1) - Q(3,1))/DE) + 2
          IF(nspec.gt.0) then
           IF (nspec.GT.NDECSE - 1) nspec = NDECSE - 1
           WRITE (12,*) ' '
           WRITE (12,*) ' Spectrum of alphas   (z,x)  ZAP=  2004'
           WRITE (12,'(30X,''A      n      g      l      e      s '')')
           WRITE (12,*) ' '
           WRITE (12,'('' Energy   '',8G15.5,/,(10X,8G15.5))') ANGles
           DO ie = 1, nspec - 1
             WRITE (12,'(F9.4,8E15.5,/,(9X,8E15.5))') FLOAT(ie - 1)*DE,
     &             (CSEa(ie,nang,3,0),nang = 1,NDANG)
           ENDDO
           DO ie = nspec, nspec + 1
                                ! exact endpoint
             WRITE (12,'(F9.4,8E15.5,/,(9X,8E15.5))') EMAx(1) - Q(3,1),
     &             (CSEa(ie,nang,3,0),nang = 1,NDANG)
           ENDDO
         ENDIF
C--------light ions
         IF (NDEJC.EQ.4 .AND. NEMc.GT.0) THEN
           nspec = INT((EMAx(1) - Q(4,1))/DE) + 2
            IF(nspec.gt.0) then
             IF (nspec.GT.NDECSE - 1) nspec = NDECSE - 1
             WRITE (12,*) ' '
             WRITE (12,
     &'(''  Spectrum of  '',I1,''-'',A2,4X,                          ''(
     &n,x)'')') INT(AEJc(NDEJC)), SYMbe(NDEJC), ' ZAP=', IZAejc(NDEJC)
             WRITE(12,'(30X,''A      n      g      l      e      s '')')
             WRITE (12,*) ' '
             WRITE (12,'('' Energy   '',8G15.5,/,(10X,8G15.5))') ANGles
             DO ie = 1, nspec - 1
               WRITE (12,'(F9.4,8E15.5,/,(9X,8E15.5))') FLOAT(ie - 1)
     &                *DE, (CSEa(ie,nang,4,0),nang = 1,NDANG)
             ENDDO
             DO ie = nspec, nspec + 1
                                   ! exact endpoint
               WRITE (12,'(F9.4,8E15.5,/,(9X,8E15.5))') EMAx(1)- Q(4,1),
     &                (CSEa(ie,nang,4,0),nang = 1,NDANG)
             ENDDO
            ENDIF
         ENDIF
      ENDIF
C-----end of ENDF spectra (inclusive)

      IF (ENDf(1).GT.0) THEN
         WRITE (6,*)
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

      READ (5,*) EIN
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
     &                 exqcut, recorr, sumnor, weight
      REAL FLOAT
      INTEGER icse, ie, il, ire, irec, na, nejc, nnur
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
      dang = 3.14159/FLOAT(NDANG - 1)
      coef = dang/DERec/2.0
      DO nejc = 1, NEJcm   !over ejectiles
C        EMITTED NUCLEI MUST BE HEAVIER THAN ALPHA !! (RCN)
         if(NRES(nejc).lt.0) cycle
         nnur = NREs(nejc)
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
      INTEGER nejc, nnur
      DOUBLE PRECISION xnorfis

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
C           EMITTED NUCLEI MUST BE HEAVIER THAN ALPHA !! (RCN)
            if(NRES(nejc).lt.0) cycle
            nnur = NREs(nejc)
            CALL ACCUM(Ke,Nnuc,nnur,nejc,Xnor)
            CSEmis(nejc,Nnuc) = CSEmis(nejc,Nnuc) + xnorfis*SCRtem(nejc)
     &                          *(1 - Aafis)
         ENDDO
C------------gammas
         CALL ACCUM(Ke,Nnuc,Nnuc,0,Xnor)
         CSEmis(0,Nnuc) = CSEmis(0,Nnuc) + xnorfis*SCRtem(0)*(1 - Aafis)
         POP(Ke,Jcn,Ipar,Nnuc) = 0.0
         GOTO 99999
      ENDIF
C--------------no subbarrier effects
C--------particles
      DO nejc = 1, NEJcm
C        EMITTED NUCLEI MUST BE HEAVIER THAN ALPHA !! (RCN)
         if(NRES(nejc).lt.0) cycle
         nnur = NREs(nejc)
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
99999 END
