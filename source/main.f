Ccc   * $Author: Sin $
Ccc   * $Date: 2004-10-07 20:40:10 $
Ccc   * $Id: main.f,v 1.40 2004-10-07 20:40:10 Sin Exp $
C
      PROGRAM EMPIRE
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:ppu*
Ccc   *                         E M P I R E                              *
Ccc   *                                                                  *
Ccc   *     Main of the EMPIRE code. A full call structure tree is       *
Ccc   *     given below in alphabetical order:                           *
Ccc   *                                                                  *
Ccc   *    ACCUM                                                         *
Ccc   *        BELLAC                                                    *
Ccc   *    ACCUMSD                                                       *
Ccc   *    AUERST                                                        *
Ccc   *    DECAY                                                         *
Ccc   *        TLLOC                                                     *
Ccc   *    DECAYD                                                        *
Ccc   *    DECAYG                                                        *
Ccc   *        E1                                                        *
Ccc   *        E2                                                        *
Ccc   *        XM1                                                       *
Ccc   *    DECAYT                                                        *
Ccc   *        E1                                                        *
Ccc   *        E2                                                        *
Ccc   *        XM1                                                       *
Ccc   *    FISSION                                                       *
Ccc   *        TLF                                                       *
Ccc   *    HMSC                                                          *
Ccc   *        AUERST                                                    *
Ccc   *        DECHMS                                                    *
Ccc   *        E1                                                        *
Ccc   *        GDOWN                                                     *
Ccc   *            W                                                     *
Ccc   *                W1                                                *
Ccc   *                W2                                                *
Ccc   *                W3                                                *
Ccc   *        MATIN                                                     *
Ccc   *        MATIN1                                                    *
Ccc   *        OMJ                                                       *
Ccc   *        ROPHM                                                     *
Ccc   *        TRATES                                                    *
Ccc   *        VQ                                                        *
Ccc   *            WOBL                                                  *
Ccc   *        W (see above)                                             *
Ccc   *        WHERE                                                     *
Ccc   *        WILLI                                                     *
Ccc   *        WOBL                                                      *
Ccc   *        WT                                                        *
Ccc   *            W (see above)                                         *
Ccc   *        ZERO                                                      *
Ccc   *            W (see above)                                         *
Ccc   *    INPUT                                                         *
Ccc   *        BNDG                                                      *
Ccc   *            WHERE                                                 *
Ccc   *        CLEAR                                                     *
Ccc   *        LEVREAD                                                   *
Ccc   *            BCDNUM                                                *
Ccc   *        PTLEVSET                                                  *
Ccc   *            PTLEVRE                                               *
Ccc   *        READIN                                                    *
Ccc   *            WHERE                                                 *
Ccc   *        READLDP                                                   *
Ccc   *            FSHELL                                                *
Ccc   *            WHERE                                                 *
Ccc   *        READNIX                                                   *
Ccc   *            SHELLC                                                *
Ccc   *                LYMASM                                            *
Ccc   *                    XI                                            *
Ccc   *                    XIMOD                                         *
Ccc   *            WHERE                                                 *
Ccc   *        ROCOL                                                     *
Ccc   *            ALIT                                                  *
Ccc   *            BARFIT                                                *
Ccc   *                LPOLY                                             *
Ccc   *            MOMFIT                                                *
Ccc   *                LPOLY                                             *
Ccc   *            RODEF                                                 *
Ccc   *                DAMPV                                             *
Ccc   *                VIBR                                              *
Ccc   *            SHCFADE                                               *
Ccc   *            SIGMAK                                                *
Ccc   *        ROEMP                                                     *
Ccc   *            BNDG (see above)                                      *
Ccc   *            DAMIRO                                                *
Ccc   *                FSHELL                                            *
Ccc   *                MOMFIT (see above)                                *
Ccc   *                ROBCS                                             *
Ccc   *                    DAMPKS                                        *
Ccc   *                    VIBR                                          *
Ccc   *                RODEF (see above)                                 *
Ccc   *                SIGMAK                                            *
Ccc   *            FSHELL                                                *
Ccc   *            PIPE                                                  *
Ccc   *            PRERO                                                 *
Ccc   *                BARFIT (see above)                                *
Ccc   *                SHCFADE                                           *
Ccc   *                SIGMAK                                            *
Ccc   *        ROGC                                                      *
Ccc   *            BARFIT (see above)                                    *
Ccc   *            PIPE                                                  *
Ccc   *            RIVOLI                                                *
Ccc   *            SHCFADE                                               *
Ccc   *            SIGMAK                                                *
Ccc   *        SMAT                                                      *
Ccc   *        TLEVAL                                                    *
Ccc   *            OMTL                                                  *
Ccc   *                FACT                                              *
Ccc   *                PREANG                                            *
Ccc   *                PRIPOT                                            *
Ccc   *                PRITC                                             *
Ccc   *                PRITD                                             *
Ccc   *                SCAT                                              *
Ccc   *                    INTEG                                         *
Ccc   *                    RCWFN                                         *
Ccc   *                SETPOTS                                           *
Ccc   *                    OMPAR                                         *
Ccc   *                SHAPEC                                            *
Ccc   *                    CGAMMA                                        *
Ccc   *                SHAPEL                                            *
Ccc   *                    CLEB                                          *
Ccc   *                    RACAH                                         *
Ccc   *                SPIN0                                             *
Ccc   *                SPIN05                                            *
Ccc   *                SPIN1                                             *
Ccc   *        WHERE                                                     *
Ccc   *    MARENG                                                        *
Ccc   *        HITL                                                      *
Ccc   *            BNDG (see above)                                      *
Ccc   *            CCFUS                                                 *
Ccc   *                BAR                                               *
Ccc   *                    POTENT                                        *
Ccc   *                        POT                                       *
Ccc   *                POT                                               *
Ccc   *            PUSH                                                  *
Ccc   *                F                                                 *
Ccc   *                G                                                 *
Ccc   *                    F                                             *
Ccc   *                INTGRS                                            *
Ccc   *            XFUS                                                  *
Ccc   *        OMTL (see above)                                          *
Ccc   *    ORION                                                         *
Ccc   *        CCCTRL                                                    *
Ccc   *            FLGLCH                                                *
Ccc   *            OMPOTEN                                               *
Ccc   *        FFCAL                                                     *
Ccc   *        HIBORN                                                    *
Ccc   *            MSTEP                                                 *
Ccc   *        OPMPARN                                                   *
Ccc   *            OMPAR                                                 *
Ccc   *            WHERE                                                 *
Ccc   *        XSEC                                                      *
Ccc   *            LEGNDR                                                *
Ccc   *            XSC12                                                 *
Ccc   *                CLEBSCH                                           *
Ccc   *                XSCABC                                            *
Ccc   *                    CLEBHF                                        *
Ccc   *                    CLEBRD                                        *
Ccc   *                    CLEBZ                                         *
Ccc   *                    RACHLF                                        *
Ccc   *                    RACSIM                                        *
Ccc   *    PRINPUT                                                       *
Ccc   *    ROCOL (see above)                                             *
Ccc   *    ROEMP (see above)                                             *
Ccc   *    TRISTAN                                                       *
Ccc   *        RESPNS                                                    *
Ccc   *            INELAS                                                *
Ccc   *                CLEBTRI                                           *
Ccc   *                DWIDTH                                            *
Ccc   *                RADIAL                                            *
Ccc   *            SPLVL                                                 *
Ccc   *                BCS                                               *
Ccc   *                    ESORT                                         *
Ccc   *                    NUMBER                                        *
Ccc   *                EHO                                               *
Ccc   *                ESORT                                             *
Ccc   *                INDF                                              *
Ccc   *        SPECTR                                                    *
Ccc   *            INVERT                                                *
Ccc   *            LSQLEG                                                *
Ccc   *                MTXGUP                                            *
Ccc   *                PLNLEG                                            *
Ccc   *            PNORM                                                 *
Ccc   *                POLYNM                                            *
Ccc   *            POLYNM                                                *
Ccc   *    ULM                                                           *
Ccc   *    ULMDYN                                                        *
Ccc   *    WHERE                                                         *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * author: M.Herman                                                 *
Ccc   * date:   June 1994                                                *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C     COMMON variables
C
      COMMON /CRIT  / TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl,
     &                bet2
      COMMON /PARAM / AP1, AP2, GAMma, DEL, DELp, BF, A23, A2, NLWst
      COMMON /IMAG  / TF(NFPARAB), TDIr, TABs, TDIr23,TG2
      COMMON /CRITFIS/acrtf(2), ucrtf(2), tcrtf(2), detcrtf(2),
     &                scrtf(2),mortcrt(nfparab),mparcrt(nfparab),
     &                econdf(2)
      COMMON /FISSMOD / rofism(60,30,3), hm(Nftrans,3), 
     &                  efdism(Nftrans,3), UGRidf(0:NFISENMAX, 3),
     &                  efbm(3), xminnm(3),afism(3),
     &                  defbm(3), shcfism(3), deltafism(3),
     &                  gammafism(3),wfism(3),bffm(3), nrbinfism(3)
      COMMON /FIS_ISO / tfiso,tgiso,tiso,rfiso
C
      DOUBLE PRECISION  rofism, hm, efdism, efbm, xminnm, defbm, 
     &                  shcfism, deltafism, gammafism,ugridf,afism,
     &                  tfiso,tgiso,tiso,rfiso,pfiso,tg2,wfism,
     &                  tdirect 
              
      INTEGER NRBar, NRFdis, bffm 
      DOUBLE PRECISION TF, TDIr, TABs, TDIr23,mm2,mortcrt,tout,
     &                 mparcrt, sumfism(3), csfism(3),tfbm(3),tfb,
     &                 tdirm(3),cota,cota1,cotaexp
  
      CHARACTER*9 cejectile
      CHARACTER*21 reactionx
      DOUBLE PRECISION ELAcs, ELAda(101), TOTcs
      DOUBLE PRECISION ftmp, G 
      INTEGER NELang
C     For factorial calculations (SCAT2)
      PARAMETER(LFMAX = 4*NDTL + 2)
      COMMON /FACT  / G(LFMAX)
C-----next COMMON is to transfer elastic ddx from Scat-2
      COMMON /ELASCAT/ ELAda, TOTcs, ELAcs, NELang
C-----Plujko_new
C     INTEGER keyinput, kzz1, kaa1, keyload, keyalpa, kzz, kaa
C     COMMON /MLOCOM1/ keyinput, kzz1, kaa1
C     COMMON /MLOCOM2/ keyload, keyalpa, kzz, kaa
C-----Plujko_new(End)

C
C     Local variables
C
      DOUBLE PRECISION aafis, bbfis, dencomp, xnorfis
C     DOUBLE PRECISION aorg, ares, corr, corrmsd, csemist, csfis, 
      DOUBLE PRECISION       ares,       corrmsd, csemist, csfis, 
     &                 cturbo, ded, delang, elcncs, gamfis, gamt, pope, 
     &                 popleft, poplev, poptot, popread, dang, coef, 
     &                 ecm, echannel, erecoil, csmsdl, xcse, gang, q2, 
     &                 q3, qmax, qstep, recorp, recorr, sgamc, spdif, 
     &                 spdiff, stauc, step, sum, sumfis, tauf, taut, 
C    &                 weight, xccm, xizat, xnhms, xnl, xnor, zorg, zres
     &                 weight, xccm, xizat, xnhms, xnl, xnor, zres
      REAL FLOAT
      INTEGER i, ia, iad, iam, iang, ib, iccmh, iccml, icse, ie, il, 
C    &        ilast, iloc, imt, iorg, ip, ipar, itimes, its, iz, izaorg,
     &               iloc, ip, ipar, itimes, its, iz, 
     &        izares, j, jcn, ke, kemax, kemin, ltrmax, mt649, mt849, 
C    &        mt91, nang, nbr, nejc, ngspec, nnuc, nnur, nnurn, nnurp, 
     &        mt91, nang, nbr, nejc, nnuc, nnur, nnurn, nnurp, 
     &        nspec, irec, icsl, icsh, mt2
      INTEGER INT, MIN0
C     DOUBLE PRECISION csfit(NDANG),  qq(5),  adum(5, 7)
      LOGICAL nvwful
      INCLUDE 'io.h'
C
C-----Plujko_new
C     DATA keyinput/0/, kzz1/0/, kaa1/0/
C     DATA  keyload/0/, keyalpa/0/, kzz/0/, kaa/0/
C     F_PRINT=1
C-----Plujko_new(End)
C    
C     FACTORIAL CALCULATIONS 
C
      CALL FCT()
C
C-----
C-----read and prepare input data
C-----
 1400 CALL INPUT
C-----
C-----print input data
C-----
      IF(IOUt.GT.0)CALL PRINPUT
      WRITE(*, '(''  C.M. incident energy '',G10.5,'' MeV'')')EIN
C-----Clear CN elastic cross section (1/4*pi)
      elcncs = 0.0D+0
C-----
C-----Plujko_new (Prepares Giant Resonance parameters - systematics)
      CALL ULM(0)
C-----Plujko_new (END of Prepares Giant Resonance parameters - systematics)
C-----
C-----calculate reaction cross section and its spin distribution
C-----
      CALL MARENG(0, 0)
C-----
C-----get ECIS results
C-----
      IF(DIRect.GT.0)THEN
C--------locate position of the target among residues
         CALL WHERE(IZA(1) - IZAejc(0), nnurec, iloc)
C--------locate position of the projectile among ejectiles
         CALL WHEREJC(IZAejc(0), nejcec, iloc)
C
         IF((MODelecis.GT.0 .AND. DIRect.NE.3) .OR. DIRect.EQ.2)THEN
C           OPEN(45, FILE = 'ecis95.cs', STATUS = 'OLD')
C           ECIS03, May 2004  
            OPEN(45, FILE = 'ecis03.cs', STATUS = 'OLD')
            READ(45, *, END = 1500)  ! To skip first line <CROSS-S.> ..  
            IF (ZEjc(0).eq.0) READ(45, *, END = 1500) TOTcs
            READ(45, *, END = 1500)ecisabs
            IF (ZEjc(0).eq.0) READ(45, *, END = 1500)ftmp   ! reading this line here in ecis03
C-----------Checking if CC OMP was used
            IF((MODelecis.GT.0 .AND. DIRect.NE.3) .OR. 
     &      DIRect.EQ.2) ELAcs=ftmp
            CLOSE(45)
         ENDIF
         NELang = 73
         ecm = EINl - EIN
         dang = 3.14159/FLOAT(NELang - 1)
C        OPEN(45, FILE = 'ecis95.ang', STATUS = 'OLD')
C        ECIS03, May 2004  
         OPEN(45, FILE = 'ecis03.ang', STATUS = 'OLD')            
         READ(45, *, END = 1500)  ! To skip first line <ANG.DIS.> ..  
         READ(45, *, END = 1500)  ! To skip level identifier line 
         OPEN(46, FILE = 'ecis03.ics', STATUS = 'OLD')
         READ(46, *, END = 1500)  ! To skip first line <INE.C.S.> ..  
C        OPEN(47, FILE = 'ecis03.pol', STATUS = 'OLD')            
C        READ(47, *, END = 1500)  ! To skip first line <ANG.DIS.> ..  
C--------Checking if CC OMP was used
         DO iang = 1, NELang
            IF((MODelecis.GT.0 .AND. DIRect.NE.3) .OR. DIRect.EQ.2)THEN
               READ(45, '(7x,E12.5)', END = 1500)ELAda(iang)
            ELSE
C--------------dummy read
               READ(45, *, END = 1500)
            ENDIF
         ENDDO
C--------get and add inelastic cross sections (including double-differential)
         ncoll = 0
         DO i = 2, ND_nlv
            ilv = ICOller(i)
            IF(ilv.GT.NLV(nnurec))GOTO 1500
            echannel = EX(NEX(1), 1) - Q(nejcec, 1) - ELV(ilv, nnurec)
C-----------avoid reading closed channels
            IF(echannel.GE.0.0001)THEN
               xcse = echannel/DE + 1.0001
               icsl = INT(xcse)
               icsh = icsl + 1
C              ECIS03, May 2004, changed to read from file 46  
               READ(46, *, END = 1500)popread
C              READ(45, *, END = 1500)popread
               ncoll = i
               POPlv(ilv, nnurec) = POPlv(ilv, nnurec) + popread
               CSDirlev(ilv,nejcec) = CSDirlev(ilv,nejcec) + popread
               CSEmis(nejcec, 1) = CSEmis(nejcec, 1) + popread
C--------------add direct transition to the spectrum
               popl = popread*(FLOAT(icsh) - xcse)/DE
               IF(icsl.EQ.1)popl = 2.0*popl
               poph = popread*(xcse - FLOAT(icsl))/DE
               CSE(icsl, nejcec, 1) = CSE(icsl, nejcec, 1) + popl
               CSE(icsh, nejcec, 1) = CSE(icsh, nejcec, 1) + poph
               READ(45, *, END = 1500)    ! Skipping level identifier line
C              READ(47, *, END = 1500)    ! Skipping level identifier line
C              Empire uses 10 deg grid for inelastic so we have to take
C              each 4th result from ECIS (2.5 deg grid)
               DO iang = 1, NDANG - 1
                  READ(45, '(7x,E12.5)', END = 1500)
     &                 CSAlev(iang, ilv, nejcec)
                  READ(45, '(7x,E12.5)', END = 1500)
                  READ(45, '(7x,E12.5)', END = 1500)
                  READ(45, '(7x,E12.5)', END = 1500)
               ENDDO
               READ(45, '(7x,E12.5)', END = 1500)
     &              CSAlev(NDANG, ilv, nejcec)
C--------------construct recoil spectra due to direct transitions
               IF(ENDf.GT.0)THEN
                  dang = PI/FLOAT(NDANG - 1)
                  coef = 2*PI*dang
C                 check whether integral over angles agrees with x-sec. read from ECIS 
                  csum = 0.0
                  DO iang = 1, NDANG 
                     csum = csum + CSAlev(iang, ilv, nejcec)*
     &                      SANgler(iang)*coef
                  ENDDO
C                 correct 'coef' for eventual imprecision and include recoil DE
                  coef = coef*POPlv(ilv, nnurec)/csum/DERec
                  echannel = echannel*EJMass(0)/AMAss(1)
                  DO iang = 1, NDANG
                     erecoil = ecm + echannel + 2*SQRT(ecm*echannel)
     &                         *CANgler(iang)
                     irec = erecoil/DERec + 1.001
                     weight = (erecoil - (irec - 1)*DERec)/DERec
C                    escape if we go beyond recoil spectrum dimension
                     IF(irec + 1.GT.NDEREC)GOTO 1450
                     csmsdl = CSAlev(iang, ilv, nejcec)*SANgler(iang)
     &                        *coef
                     RECcse(irec, 0, nnurec) = RECcse(irec, 0, nnurec)
     &                  + csmsdl*(1.0 - weight)
                     RECcse(irec + 1, 0, nnurec)
     &                  = RECcse(irec + 1, 0, nnurec) + csmsdl*weight
                  ENDDO
               ENDIF
            ENDIF
 1450    ENDDO
 1500    CLOSE(45)
         CLOSE(46)
C	   CLOSE(47)
C--------print elastic and direct cross sections from ECIS
         WRITE(6, *)' '
         WRITE(6, *)' '
         IF((MODelecis.EQ.0 .AND. DIRect.EQ.1) .OR. DIRect.EQ.3)THEN
            WRITE(6, *)' '
            IF(ncoll.GT.0)THEN
               IF(DIRect.EQ.1)THEN
                  WRITE(6, *)' Inelastic scattering results provided by'
                  WRITE(6, *)
     >         ' Coupled Channel + DWBA calculations with ECIS:'
               ENDIF
               IF(DIRect.EQ.3)THEN
                  WRITE(6, *)' Inelastic scattering results provided by'
                  WRITE(6, *)' DWBA calculations with ECIS:'
               ENDIF
               WRITE(6, *)' '
               gang = 180.0/(NDANG - 1)
C
C              RCN, 08/2004
C
                 if(CSAlev(1, ICOller(2), nejcec).gt.0) then
                 WRITE(6, 99001)(ICOller(ilv), ilv = 2, min(ncoll,10))
99001            FORMAT('  Angle ', 10(6x, i2, '-level'))
                 WRITE(6, *)' '
                 DO iang = 1, NDANG
                   if(CSAlev(1, ICOller(2), nejcec).gt.0) then
                    WRITE(6, 99002) (iang - 1)*gang, 
     &               (CSAlev(iang, ICOller(ilv), nejcec),
     &                       ilv = 2, min(ncoll,10))
                     endif  
99002              FORMAT(1x, f5.0, 3x, 11(2x, E12.6))
                 ENDDO
                 WRITE(6, *)' '
                 WRITE(6, 99003)
     &             (POPlv(ICOller(ilv), nnurec),ilv = 2, min(ncoll,10))
99003            FORMAT(6x, 3x, 11(2x, E12.6))
                 WRITE(6, *)' '
                 WRITE(6, *)' '
                 WRITE(6, *)' '
                 endif
            ENDIF
         ENDIF
         IF((MODelecis.GT.0 .AND. DIRect.NE.3) .OR. DIRect.EQ.2)THEN
            WRITE(6, *)' '
            WRITE(6, *)
     &               ' Results provided by Coupled Channel calculations'
     &               , ' with ECIS code:'
              IF(ZEJc(0).EQ.0) THEN
              WRITE(6, 99004)TOTcs, ecisabs, ELAcs
99004       FORMAT(/, 2x, 'Total cross section         :', e14.7, ' mb',
     &             /, 2x, 'Absorption cross section    :', e14.7, ' mb',
     &             /, 2x, 'Shape elastic cross section :', e14.7, ' mb',
     &             //)
              ELSE
              WRITE(6, 99005)ecisabs
99005       FORMAT(/, 2x, 'Absorption cross section    :', e14.7, ' mb',
     &             //)
              ENDIF
C           RCN 05/05
C           The printout below was changed to include the whole angular grid
C           ala SCAT2000
C 
89001       FORMAT(' ', 46x, 'SHAPE ELASTIC DIFFERENTIAL CROSS-SECTION',
     &        /,' ', 46x, 40('*'), /, ' ', 56x, 'CENTER-OF-MASS SYSTEM',
     &      ///)
            WRITE(6, 89001)
            WRITE(6, 89002)
89002       FORMAT(' ', 5x, 4('    TETA ', 2x, 'D.SIGMA/D.OMEGA', 6x), 
     &             /)
            gang = 180.0/(NELang - 1)
            DO iang = 1, NELang/4 + 1
               imint = 4*(iang - 1) + 1
               imaxt = MIN0(4*iang, NELang)
               WRITE(6, 89004)( (j-1)*gang,ELAda(j),j=imint, imaxt)
            ENDDO
89004       FORMAT(' ', 5x, 4(1p, e12.5, 2x, e12.5, 6x))
            WRITE(6, '(//)')
C
C           RCN, 08/2004
C
            if(CSAlev(1, ICOller(2), nejcec).gt.0) then
              gang = 180.0/(NDANG - 1)
              WRITE(6, 99001)(ICOller(ilv), ilv = 2,min(ncoll,10))
              WRITE(6, *)' '
              DO iang = 1, NDANG
               if(CSAlev(1, ICOller(2), nejcec).gt.0) then
                 WRITE(6, 99002)(iang - 1)*gang, 
     &           (CSAlev(iang, ICOller(ilv), nejcec),
     &            ilv = 2, min(ncoll,10))
                 endif
              ENDDO
              WRITE(6, *)' '
              WRITE(6, 99003)(POPlv(ICOller(ilv), nnurec),
     &                 ilv = 2,min(ncoll,10))
              WRITE(6, *)' '
              WRITE(6, *)' '
              WRITE(6, *)' '
              endif
C=========================================================================
C           the following ELSE block is to print ECIS calculated XS
C           (it could be omitted)
         ELSE
C           OPEN(45, FILE = 'ecis95.cs', STATUS = 'OLD')
C           ecis03
            OPEN(45, FILE = 'ecis03.cs', STATUS = 'OLD')
            READ(45, *, END = 1540)  ! To skip first line <CROSS-S.> ..  
            IF(ZEJc(0).EQ.0) READ(45, *, END = 1540)ecistotxs
            READ(45, *, END = 1540)ecisabsxs
            IF(ZEJc(0).EQ.0) READ(45, *, END = 1540)eciselaxs
 1540       CLOSE(45)
            IF(ZEJc(0).EQ.0) THEN 
            WRITE(62, '(1x,f9.3,3x,4F14.3,1x)')EINl, ecistotxs/1000.,
     &            ecisabsxs/1000., eciselaxs/1000., TOTcs/1000.
              ELSE
            WRITE(62, '(1x,f9.3,3x,4F14.3,1x)')EINl, 
     &            ecisabsxs/1000.
              ENDIF
C
C=========================================================================
         ENDIF
      ENDIF
C
C     Skipping all emission calculations
C     GOTO 99999
C
C-----locate postions of ENDF MT-numbers 2, 91, 649, and 849
      CALL WHERE(IZA(1) - IZAejc(0), mt2, iloc)
      CALL WHERE(IZA(1) - IZAejc(1), mt91, iloc)
      CALL WHERE(IZA(1) - IZAejc(2), mt649, iloc)
      CALL WHERE(IZA(1) - IZAejc(3), mt849, iloc)
C-----
C-----calculate MSD contribution
C-----
      corrmsd = 1.0
      IF(MSD.NE.0 .AND. EIN.GT.5.D0)THEN
C--------call ORION
         REWIND 15
         WRITE(6, *)' '
         qmax = 0.99*EIN
         qstep = qmax/3.0
         ltrmax = 4
         IF(NLW.LE.10)ltrmax = 3
         IF(NLW.LE.8)ltrmax = 2
         IF(NLW.LE.6)ltrmax = 1
         IF(MSD.NE.2)THEN
            q2 = qmax
            q3 = qmax
 1520       CALL ORION(q2, q3, 1, EIN, NLW, 1, ltrmax, A(0), Z(0), 
     &                 AEJc(0), ZEJc(0), IOUt, ANGles, NDANG, ICOmpff)
            WRITE(6, 
     &'('' ORION calculated for Q2='', F7.3, '' and Q3='',            F7
     &.3)')q2, q3
            q2 = q2 - qstep
            IF(q2.LT.( - 0.0001D0))THEN
               q3 = q3 - qstep
               IF(q3.LT.( - 0.0001D0))GOTO 1550
               q2 = q3
            ENDIF
C-----------set to Q's to 0 if negative due to rounding error
            IF(q2.LT.0.0D0)q2 = 0.0
            IF(q3.LT.0.0D0)q3 = 0.0
            GOTO 1520
         ENDIF
 1550    REWIND 15
         WRITE(6, *)' '
         WRITE(6, *)' '
         CALL ULM(1)   
         CALL TRISTAN(0, 0, ltrmax, qmax, qstep)
C        CLOSE (15)
C--------print MSD double differential cross sections
         nejc = 1
         IF(ZEJc(0).EQ.1.0D0)nejc = 2
         IF(CSMsd(1).GT.0.D0 .AND. IOUt.GE.3)THEN
            itimes = FLOAT(NDANG)/11.0 + 0.95
            DO its = 1, itimes
               iad = 1 + (its - 1)*11
               iam = 11 + (its - 1)*11
               iam = MIN0(NDANG, iam)
               WRITE(6, *)' '
               WRITE(6, 
     &               '(30X,''A      n      g      l      e      s '')')
               WRITE(6, *)' '
               WRITE(6, '('' Energy  '',11(4X,F5.1,2X))')
     &               (ANGles(ia), ia = iad, iam)
               WRITE(6, *)' '
               DO i = 1, NEX(1)
                  WRITE(6, '(1X,F7.3,1X,11E11.4)')FLOAT(i - 1)*DE, 
     &                  (CSEa(i, iang, nejc, 1), iang = iad, iam)
               ENDDO
               WRITE(6, *)' '
            ENDDO
         ENDIF
         WRITE(6, *)' n MSD emission cross section ', CSMsd(1), ' mb'
         WRITE(6, *)' p MSD emission cross section ', CSMsd(2), ' mb'
         WRITE(6, *)' '
C--------correct CN population for the MSD emission
         corrmsd = (CSFus - CSMsd(1) - CSMsd(2))/CSFus
         IF(corrmsd.LT.0.0D0)THEN
            WRITE(6, *)' '
            WRITE(6, *)'MSD EMISSION LARGER THEN FUSION CROSS SECTION'
            IF(ICOmpff.GT.0)THEN
               WRITE(6, *)'TRY TO TURN OFF COMPRESSIONAL FORM FACTOR '
               WRITE(6, *)'SETTING COMPFF TO 0 IN THE OPTIONAL INPUT.'
               STOP
            ELSE
               WRITE(6, *)'THIS MAY HAPPEN IF RESPONSE FUNCTIONS ARE '
               WRITE(6, *)'RENORMALIZED IN INPUT OR FITTED TO WRONG '
               WRITE(6, *)'DISCRET LEVELS. CHECK `EFIT` AND `RESNOR` '
               WRITE(6, *)'IN OPTIONAL INPUT.    '
               WRITE(6, *)'IF THESE ARE FINE TRY ANOTHER OPTICAL MODEL.'
               STOP
            ENDIF
         ENDIF
         DO i = 1, NLW
            POP(NEX(1), i, 1, 1) = POP(NEX(1), i, 1, 1)*corrmsd
            POP(NEX(1), i, 2, 1) = POP(NEX(1), i, 2, 1)*corrmsd
         ENDDO
C--------TRISTAN *** done ***
C--------add MSD contribution to the residual nucleus population
C--------locate residual nucleus after MSD emission
         ares = A(1) - AEJc(nejc)
         zres = Z(1) - ZEJc(nejc)
         izares = INT(1000.0*zres + ares)
         CALL WHERE(izares, nnur, iloc)
         IF(iloc.EQ.1)THEN
            WRITE(6, *)' RESIDUAL NUCLEUS WITH A=', ares, ' AND Z=',    
     &                 zres, ' HAS NOT BEEN INITIALIZED'
            WRITE(6, *)' EXECUTION STOPPED'
            STOP
         ENDIF
         IF(CSMsd(nejc).NE.0.0D0)CALL ACCUMSD(1, nnur, nejc)
C--------add MSD contribution to energy spectra (angle int.)
         DO nejc = 1, 2
            DO ie = 1, NEX(1)
               CSE(ie, nejc, 1) = CSE(ie, nejc, 1) + CSEmsd(ie, nejc)
            ENDDO
C-----------add MSD contribution to the total NEJC emission
            CSEmis(nejc, 1) = CSEmis(nejc, 1) + CSMsd(nejc)
         ENDDO
C--------second chance preequilibrium emission after MSD emission
C--------neutron emission
         izares = INT(1000.0*Z(nnur) + A(nnur) - 1)
         CALL WHERE(izares, nnurn, iloc)
         IF(iloc.EQ.0)CALL SCNDPREEQ(nnur, nnurn, 1, 0)
         IF(iloc.EQ.0 .AND. IOUt.GT.3)CALL AUERST(nnur, 1)
C--------proton emission
         izares = izares - 1000
         CALL WHERE(izares, nnurp, iloc)
         IF(iloc.EQ.0)THEN
            CALL SCNDPREEQ(nnur, nnurp, 2, 1)
            IF(IOUt.GT.3)CALL AUERST(nnur, 2)
         ELSE
            CALL SCNDPREEQ(nnur, nnurp, 2, 2)
         ENDIF
C--------second chance preequilibrium *** done ***
      ENDIF
C-----
C-----MSD *** done ***
C-----
      ia = INT(A(1))
      IF(IOUt.GT.1)THEN
         WRITE(6, *)' '
         WRITE(6, *)' '
         WRITE(6, 
     &'(''  Compound nucleus '',I3,''-'',A2,                            
     &  '' spin distribution'')')ia, SYMb(1)
         WRITE(6, *)' -----------------------------------------'
         WRITE(6, *)' '
         DO i = 1, NLW
            IF(MOD(ia, 2).EQ.0)THEN
C-------------Plujko_new(Correction of  c.n. spin view J=i-1)
              WRITE(6, '(1X,I5,G12.5,5X,I5,G12.5)')i-1,
     &        POP(NEX(1), i, 1, 1), ( - (i-1)), POP(NEX(1), i, 2, 1)
C-------------Plujko_new(END of correction of c.n. spin view J=i-1 )
C-------------Plujko_new (Cb - blocking comment, correction of c.n. spin view J=i-1)
Cb            WRITE(6, '(1X,I5,G12.5,5X,I5,G12.5)')i,
Cb     &               POP(NEX(1), i, 1, 1), ( - i), POP(NEX(1), i, 2, 1)
            ELSE
               WRITE(6, '(1X,I4,''/2'',G12.5,5X,I4,''/2'',G12.5)')
     &               2*i - 1, POP(NEX(1), i, 1, 1), ( - (2*i - 1)), 
     &               POP(NEX(1), i, 2, 1)
            ENDIF
         ENDDO
         WRITE(6, *)' '
      ENDIF
      IF(IOUt.GT.0)THEN
         IF(DIRect.EQ.0)THEN
            WRITE(6, 
     &'(''   Fusion cross section = '',G13.6,                           
     &  '' mb '')')CSFus
         ELSEIF(DIRect.EQ.1)THEN
            WRITE(6, 
     &'(''   Fusion cross section = '',G13.6,                           
     &  '' mb including'')')CSFus
            WRITE(6, 
     &'(''   CC inelastic to discrete levels = '',                      
     &  G13.6,'' mb'')')SINl
            WRITE(6, 
     &'(''   Spin distribution does NOT contain'',                      
     &  '' CC inelastic contribution '')')
         ELSEIF(DIRect.EQ.2)THEN
            CSFus = CSFus + SINl
            WRITE(6, 
     &'(''   Fusion cross section = '',G13.6,                           
     &  '' mb including'')')CSFus
            WRITE(6, 
     &'(''   CC inelastic to discrete levels = '',                      
     &  G13.6,'' mb'')')SINl
            WRITE(6, 
     &'(''   Spin distribution calculated using '',                     
     &  ''CC transmission coefficients'')')
         ELSEIF(DIRect.EQ.3)THEN
            WRITE(6, 
     &'(''   Fusion cross section = '',G13.6,                           
     &  '' mb including'')')CSFus
            WRITE(6, 
     &'(''   DWBA inelastic to discrete levels = '',                    
     &  G13.6,'' mb'')')SINl
            WRITE(6, 
     &'(''   Spin distribution does NOT contain'',                      
     &  '' DWBA inelastic contribution '')')
         ENDIF
      ENDIF
      IF(ENDf.EQ.0.0D0)THEN
         WRITE(12, '('' FUSION CROSS SECTION = '',G13.6, '' mb'')')CSFus
      ELSE
         WRITE(12, *)' '
         WRITE(12, '('' FUSION CROSS SECTION = '',G12.5,'' mb'')')CSFus
         WRITE(12, '('' TOTAL  CROSS SECTION = '',G13.6,'' mb'')')TOTcs
         WRITE(12, *)' '
      ENDIF
      POPmax(1) = CSFus*1.0E-25
C-----renormalization of CN spin distribution if TURBO mode invoked
      IF(LTUrbo.NE.1)THEN
         IF(IOUt.GT.0)WRITE(6, 
     &         '('' TURBO mode with LTURBO='',I1,'' has been invoked'')'
     &         )LTUrbo
         cturbo = 0.0
         DO i = 1, NLW, LTUrbo
            cturbo = cturbo + POP(NEX(1), i, 1, 1)
     &               + POP(NEX(1), i, 2, 1)
         ENDDO
         cturbo = CSFus/cturbo
         DO i = 1, NLW, LTUrbo
            POP(NEX(1), i, 1, 1) = POP(NEX(1), i, 1, 1)*cturbo
            POP(NEX(1), i, 2, 1) = POP(NEX(1), i, 2, 1)*cturbo
         ENDDO
      ENDIF
C     fisfis d
     
      OPEN(80, FILE = 'FISSION.OUT', STATUS = 'UNKNOWN')
C-----start DO loop over decaying nuclei
      DO nnuc = 1, NNUcd
         DO kk=0,NFISENMAX
            DO jj=1,NDLW
               DO ibars=1,NFHump
                  rofis(kk,jj,ibars)=0.
               ENDDO
            ENDDO
         ENDDO
         IF(FISsil(nnuc).and.FISSHI(Nnuc).NE.1.)THEN
            Call READ_INPFIS(Nnuc)
            FIScon = 2.
            adiv1=adiv
            mm2 = 0.24*A(Nnuc)**(2./3.)
            r0 = 1.4
            nrbarc1=nrbarc
            IF(nrbarc.eq.3)nrbarc1=2
            DO ibars = 1, NRBarc1
               IF(FISmod(nnuc).eq.0..or.
     &              (Fismod(nnuc).gt.0..and.ibars.ne.2))THEN
                  shc(nnuc)=shcfis(ibars)
                  delp=deltafis(ibars)
                  gamma=gammafis(ibars)
                  CALL ROEMP(Nnuc, 1.0D0,afis(ibars))
                  acrtf(ibars)=acrt
                  ucrtf(ibars)=ucrt
                  tcrtf(ibars)=tcrt
                  detcrtf(ibars)=detcrt
                  scrtf(ibars)=scr
                  econdf(ibars)=econd    
                  Mparcrt(ibars)=6*ACRt*mm2*(1. - (2./3.)*
     &                           DEFfis(ibars))/PI**2
                  IF(mparcrt(ibars).lt.2.)mparcrt(ibars)=2. !!!!!!!!!!!
                  Mortcrt(ibars) = 0.0095616*r0**2*A(Nnuc)**(5./3.)
     &                             *(1. + (1./3.)*DEFfis(ibars))  
                  Call DAMI_ROFIS(Nnuc,ibars,0, afis(ibars))
               ENDIF
               IF(FISmod(nnuc).gt.0..and.ibars.eq.2)THEN
                  DO m=1,int(FISmod(nnuc))+1
                     shc(nnuc)=shcfism(m)
                     delp=deltafism(m)
                     gamma=gammafism(m)
                     bff(ibars)=bffm(m)
                     CALL ROEMP(Nnuc, 1.0D0,afism(m))
                     acrtf(ibars)=acrt
                     ucrtf(ibars)=ucrt
                     tcrtf(ibars)=tcrt
                     detcrtf(ibars)=detcrt
                     scrtf(ibars)=scr
                     econdf(ibars)=econd    
                     Mparcrt(ibars)=6*ACRt*mm2*(1. - (2./3.)*
     &                           DEFfis(ibars))/PI**2
                     IF(mparcrt(ibars).lt.2.)mparcrt(ibars)=2. !!!!!!!!!!!
                      Mortcrt(ibars) = 0.0095616*r0**2*A(Nnuc)**
     &                            (5./3.)*(1. + (1./3.)*DEFfis(ibars))
                     Call DAMI_ROFIS(Nnuc,ibars,m, afism(m))
                  ENDDO   
               ENDIF
            ENDDO
            IF(Nrbar.eq.3.and.nrwel.eq.1.and.FISMOD(nnuc).eq.0.)THEN
               tfiso = 2.86896 * exp( 2. * pi *(efb(2)- 
     &            (efb(3)+h(1,3)/2.))/ h(1,2))/
     &            (h(1,3)*10.**21)
               tgiso= exp( 2. * pi * (efb(1)-(efb(3)+h(1,3)/2.))/
     &              h(1,1))/10.**14     
            ENDIF
            IF(Nrbar.eq.5.and.nrwel.eq.2.and.FISMOD(nnuc).eq.0.)THEN
               tfiso = 2.86896 * exp( 2. * pi *(veq- 
     &                 (efb(4)+h(1,4)/2.))/ hoeq)/
     &                  (h(1,4)*(10.**21))
               tgiso= exp( 2. * pi * (efb(1)-(efb(4)+h(1,4)/2.))/
     &              h(1,1))/10.**14
            ENDIF
            IF(fismod(nnuc).eq.0..and.nrbar.ge.3)THEN
               tiso=tfiso*tgiso/(tfiso+tgiso)
               rfiso=tgiso/(tfiso+tgiso)
            ENDIF   
            FIScon=0.
            Call WRITE_OUTFIS(Nnuc)
            adiv=adiv1
         ENDIF
         ia = INT(A(nnuc))
C--------reset variables for life-time calculations
         stauc = 0.0
         sgamc = 0.0
         csemist = 0.0
         csfis = 0.0
         IF(Fismod(nnuc).gt.0.)THEN
            Do m=1,int(fismod(nnuc))+1
               csfism(m)=0.
            ENDDO   
         ENDIF   
         sumfis = 0.0
         IF(IOUt.GT.0)THEN
            WRITE(6, *)' '
            WRITE(6, *)' '
            WRITE(6, *)' -------------------------------------'
            WRITE(6, '(I3,2X,''Decaying nucleus '',I3,''-'',A2)')nnuc, 
     &            ia, SYMb(nnuc)
            WRITE(6, *)' -------------------------------------'
            WRITE(6, *)' '
         ENDIF
         IF(ENDf.NE.0.0D0)THEN
            WRITE(12, *)' '
            WRITE(12, *)' ----------------------------------------------
     &-----------------'
            WRITE(12, 
     &'(''  Decaying nucleus '',I3,''-'',A2,''-'',I3,     ''  mass='',F1
     &0.6,'' Q-value='',F10.6)')INT(Z(nnuc)), SYMb(nnuc), ia, 
     &AMAss(nnuc), QPRod(nnuc)
            WRITE(12, *)' ----------------------------------------------
     &-----------------'
            IF(nnuc.NE.1)THEN
               WRITE(12, 
     &'(1X,/,10X,''Discrete level population '',      ''before gamma cas
     &cade'')')
               WRITE(12, '(1X,/,10X,40(1H-),/)')
               DO il = 1, NLV(nnuc)
C-----------------check for the number of branching ratios
                  nbr = 0
                  DO ib = 1, NDBR
                     IF(BR(il, ib, 2, nnuc).EQ.0.)GOTO 1565
                     nbr = ib
                  ENDDO
 1565             IF(nbr.EQ.0 .AND. il.NE.1 .AND. FIRst_ein .AND. 
     &               (nnuc.EQ.mt91 .OR. nnuc.EQ.mt649 .OR. 
     &               nnuc.EQ.mt849))WRITE(6, *)
     &               ' WARNING: Branching ratios for level ', il, 
     &               ' IN ', INT(A(nnuc)), '-', SYMb(nnuc), 
     &               ' are missing'
                  WRITE(12, 99012)il, ELV(il, nnuc), LVP(il, nnuc), 
     &                            XJLv(il, nnuc), POPlv(il, nnuc), nbr, 
     &                            (NINT(BR(il,ib,1,nnuc)), 
     &                            BR(il, ib, 2, nnuc), ib = 1, nbr)
C-----------------next IF moves levels population to the ground state
C-----------------to avoid gamma cascade between discrete levels
C-----------------originating from the direct population
C-----------------of discrete levels by a neutron, proton or alpha.
C-----------------These gammas should not go into MT=91, 649, or 849.
                  IF((nnuc.EQ.mt91 .OR. nnuc.EQ.mt649 .OR. nnuc.EQ.mt849
     &               ) .AND. il.NE.1)THEN
                     POPlv(1, nnuc) = POPlv(1, nnuc) + POPlv(il, nnuc)
                     POPlv(il, nnuc) = 0.0
                  ENDIF
               ENDDO
C--------------temporary output to check dir contribution to disc levels
C              gang=10.
C              IF(nnuc.EQ.mt91) THEN
C              nejcec=1
C              ELSEIF(nnuc.EQ.mt649) THEN
C              nejcec=2
C              ELSEIF(nnuc.EQ.mt849) THEN
C              nejcec=3
C              ELSE
C              WRITE(6,*)'SALTO Nnuc= ',Nnuc
C              GO TO 3392
C              ENDIF
C              WRITE(6, 9995)(ilv, ilv = 1, 11)
C9995          FORMAT('  Angle  ', 11(6x, i2, '-level'))
C              WRITE(6, *)' '
C              DO iang = 1, NDANG
C              WRITE(6, 99006)(iang - 1)*gang,
C              &               (CSAlev(iang, il, nejcec),il=1,11)
C              ENDDO
C              WRITE(6, *)' '
C              WRITE(6, 9995)(ilv, ilv = 12, 22)
C              WRITE(6, *)' '
C              DO iang = 1, NDANG
C              WRITE(6, 99006)(iang - 1)*gang,
C              &               (CSAlev(iang, il, nejcec),il=12,22)
C              ENDDO
C              WRITE(6, *)' '
C              WRITE(6, 9995)(ilv, ilv = 23,NLV(nnuc) )
C              WRITE(6, *)' '
C              DO iang = 1, NDANG
C              WRITE(6, 99006)(iang - 1)*gang,
C              &               (CSAlev(iang, il, nejcec),il=23, NLV(nnuc))
C              ENDDO
C3392          CONTINUE
C--------------temporary output *** done ***
               WRITE(12, '(1X,/,10X,40(1H-),/)')
C--------------write elastic to tape 12
               IF(nnuc.EQ.mt2)THEN
                 WRITE(12, *)' '
                 WRITE(12, 
     &                  '('' ELASTIC CROSS SECTION ='',G12.5,'' mb'')')
     &                  ELAcs
                 WRITE(12, *)' '
                 WRITE(12, *)' Elastic angular distribution '
                 WRITE(12, *)' '
                 delang = 180./FLOAT(NELang - 1)
                 WRITE(12, 99008)(FLOAT(iang - 1)*delang, iang = 1, 
     &                            NELang)
99008            FORMAT(10X, 8G15.5)
                 WRITE(12, 99009)(ELAda(iang) + elcncs, iang = 1, 
     &                            NELang)
99009            FORMAT(9X, 8E15.5)
                 WRITE(12, *)' '
                  IF(elcncs.EQ.0) WRITE(6,*)'WARNING: CN elastic is 0' 
               ENDIF
            ENDIF
         ENDIF
         POPmax(nnuc) = POPmax(nnuc)*0.0001
         IF(POPmax(nnuc).EQ.0.0D0)THEN
            WRITE(6, *)' '
            WRITE(6, *)
     &                'Continuum of this nucleus has not been populated'
            GOTO 1650
         ENDIF
C--------prepare gamma transition parameters
         CALL ULM(nnuc)
C--------calculate compound nucleus level density at saddle point
C        fisfis
         IF(FISSHI(Nnuc).NE.1.)GOTO 1600
C        fisfis
         IF(FISsil(nnuc))THEN
            IF(ADIv.EQ.0.0D0)CALL ROEMP(nnuc, 1.D0, 0.0D0)
            IF(ADIv.EQ.1.0D0)CALL ROCOL(nnuc, 1.D0, 2.D0)
            IF(ADIv.GT.3.0D0)CALL ROCOL(nnuc, 1.D0, 1.D0)
            IF(ADIv.EQ.2.0D0)WRITE(6, *)
     &  ' MUST NOT USE GILBERT-CAMERON LEVEL DENSITIES FOR SADDLE POINT'
            IF(IOUt.EQ.6)THEN
               WRITE(6, '(1X,/,'' Saddle point level density'',/)')
               WRITE(6, 99010)(EX(i, nnuc), (ROF(i,j,nnuc), j = 1, 12), 
     &                        i = 1, NEX(nnuc))
Cpr            WRITE(6,20) (EX(I,NNUC),(ROF(I,J,NNUC),J=13,24),I=1,NEX(NNUC))
Cpr            WRITE(6,20) (EX(I,NNUC),(ROF(I,J,NNUC),J=25,36),I=1,NEX(NNUC))
Cpr            WRITE(6,20) (EX(I,NNUC),(ROF(I,J,NNUC),J=37,48),I=1,NEX(NNUC))
Cpr            WRITE(6,20) (EX(I,NNUC),(ROF(I,J,NNUC),J=49,60),I=1,NEX(NNUC))
99010          FORMAT(1X, 13G10.4)
            ENDIF
         ENDIF
C--------locate residual nuclei
 1600    DO nejc = 1, NEJcm
            ares = A(nnuc) - AEJc(nejc)
            zres = Z(nnuc) - ZEJc(nejc)
            izares = INT(1000.0*zres + ares)
            CALL WHERE(izares, nnur, iloc)
            IF(iloc.EQ.1)THEN
               WRITE(6, *)' RESIDUAL NUCLEUS WITH A=', ares, ' AND Z=', 
     &                    zres, ' HAS NOT BEEN INITIALIZED'
               WRITE(6, *)' EXECUTION STOPPED'
            ENDIF
            NREs(nejc) = nnur
         ENDDO
C--------
C-------- DEGAS exciton model calculations of preequilibrium contribution
C--------
         IF(nnuc.EQ.1 .AND. EIN.GT.5.D0 .AND. DEGa.GT.0)THEN
            CALL EMPIREDEGAS
            WRITE(6, *)' '
            WRITE(6, *)' Start of summary from DEGAS'
            WRITE(6, *)' ---------------------------'
            IF(GDIv.GT.1.0)WRITE(6, *)' g = A/gdiv, gdiv =', GDIv
            WRITE(6, *)' '
            WRITE(6, '('' DEGAS gamma emission (CN) ='',G12.5,''mb'')')
     &            CSEmis(0, 1)
            WRITE(6, '('' DEGAS neut. emission (CN) ='',G12.5,''mb'')')
     &            CSEmis(1, 1)
            WRITE(6, '('' DEGAS prot. emission (CN) ='',G12.5,''mb'')')
     &            CSEmis(2, 1)
            WRITE(6, *)' '
         ENDIF          ! Degas done
C--------
C-------- PCROSS exciton model calculations of preequilibrium contribution
C-------- including cluster emission by Iwamoto-Harada model 
         IF(nnuc.EQ.1 .AND. EIN.GT.5.D0 .AND. PEQc.GT.0)THEN
            ftmp=CSFus*corrmsd
            CALL PCRoss(ftmp)
         ENDIF          ! PCRoss done
C--------
C--------HMS Monte Carlo preequilibrium emission
C--------
         IF(nnuc.EQ.1 .AND. EIN.GT.5.D0 .AND. LHMs.NE.0)THEN
            CLOSE(8)
            xizat = IZA(0)
            xnhms = NHMs
            debinhms = DE
            IF(debinhms.LT.1.0D0)debinhms = 1.0
            CALL DDHMS(IZAejc(0), xizat, XJLv(LEVtarg, 0), EINl, 
     &                 CSFus*corrmsd, CHMs, debinhms, xnhms, 0, 1, 0)
            CSEmis(1, 1) = CSEmis(1, 1) + CSHms(1)
            CSEmis(2, 1) = CSEmis(2, 1) + CSHms(2)
            WRITE(6, 
     &'('' HMS inclusive neut. emission ='',G12.5,                      
     &  ''mb'')')CSHms(1)
            WRITE(6, 
     &'('' HMS inclusive prot. emission ='',G12.5,                      
     &  ''mb'')')CSHms(2)
            IF(ENDf.EQ.1 .AND. FIRst_ein)THEN
               WRITE(6, *)' '
               WRITE(6, *)
     &                'WARNING: HMS Inclusive total emissions treated  '
               WRITE(6, *)
     &                'WARNING: as comming from the first cn. allows   '
               WRITE(6, *)
     &                'WARNING: to check flux balance as long as       '
               WRITE(6, *)
     &                'WARNING: multiple P.E. can be neglected. At     '
               WRITE(6, *)
     &                'WARNING: higher energies this does not hold and '
               WRITE(6, *)
     &                'WARNING: balance will get wrong.  This is OK    '
               WRITE(6, *)
     &                'WARNING: since inclusive spectra are fine and   '
               WRITE(6, *)
     &                'WARNING: in any case there are no approximations'
               WRITE(6, *)
     &                'WARNING: for production cross sections and      '
               WRITE(6, *)
     &                'WARNING: recoils!                               '
               WRITE(6, *)' '
               CLOSE(8)
            ENDIF
         ENDIF
C--------
C--------Heidelberg Multistep Compound calculations
C--------
         IF(nnuc.EQ.1 .AND. MSC.NE.0)THEN
            CALL HMSC(nvwful)
            CSEmis(0, 1) = CSEmis(0, 1) + CSMsc(0)
            CSEmis(1, 1) = CSEmis(1, 1) + CSMsc(1)
            CSEmis(2, 1) = CSEmis(2, 1) + CSMsc(2)
            IF(nvwful)GOTO 1650
         ENDIF
         IF(nnuc.EQ.1 .AND. IOUt.GE.3)THEN
            WRITE(6, *)' '
            WRITE(6, *)' Preequilibrium spectra (sum of all models):'
            CALL AUERST(1, 0)
            CALL AUERST(1, 1)
            CALL AUERST(1, 2)
            WRITE(6, *)' '
            IF(LHMs.NE.0 .AND. ENDf.NE.1)THEN
               WRITE(6, *)' HMS spectra stored as inclusive:'
               CALL AUERST(0, 1)
               CALL AUERST(0, 2)
               WRITE(6, *)' '
            ENDIF
         ENDIF
C--------
C--------start nnuc nucleus decay
C--------
         popleft = 0.0
C--------ensure that full gamma cascade in the first CN is
C--------accounted for in the case of ENDF calculations
         IF(ENDf.GT.0.0D0)GCAsc = 1.0
C--------turn on (KEMIN=1) or off (KEMIN=NEX(NNUC)) gamma cascade
C--------in the first CN
         kemin = 1
         IF(nnuc.EQ.1)THEN
            IF(GCAsc.EQ.0.0D0)kemin = NEX(nnuc)
            IF(GCAsc.EQ. - 1.0D0 .AND. EXCn.GT.20.0D0)kemin = NEX(nnuc)
         ENDIF
C--------turn  off (KEMIN=NEX(NNUC)) gamma cascade in the case of OMP fit
         IF(FITomp.NE.0) kemin = NEX(nnuc) 
         kemax = NEX(nnuc)
C--------account for widths fluctuations (HRTW)
C
C        Please check the line below for the INCIDENT GAMMAS (02/2004)
C
         IF(LHRtw.EQ.1 .AND. EIN.GT.5.0D+0)LHRtw = 0
         IF(nnuc.EQ.1 .AND. LHRtw.GT.0)THEN
            CALL HRTW
            IF(ENDf.GT.0)CALL RECOIL(kemax, nnuc) !recoil spectrum
            kemax = NEX(nnuc) - 1
            GCAsc = 1.0
         ENDIF
C--------do loop over c.n. excitation energy
         DO ke = kemax, kemin, -1
            step = DE
            IF(ke.EQ.NEX(nnuc) .OR. ke.EQ.1)step = 0.5*DE
            IF(ke.EQ.NEX(nnuc) .AND. nnuc.EQ.1)step = 1.0
            IF(ENDf.GT.0)THEN
C--------------clean auxiliary particle spectra for calculation of recoils
               DO nejc = 0, NEJcm
                  DO il = 1, NDLV
                     REClev(il, nejc) = 0.0
                  ENDDO
                  DO ie = 1, NDECSE
                     AUSpec(ie, nejc) = 0.0
                  ENDDO
               ENDDO
C--------------calculate population in the energy bin ke
               pope = 0.0
               DO jcn = 1, NLW, LTUrbo
                  pope = pope + POP(ke, jcn, 1, nnuc)
     &                   + POP(ke, jcn, 2, nnuc)
               ENDDO
               POPbin(ke,nnuc) = pope*step
            ENDIF
C-----------do loop over decaying nucleus parity
            DO ipar = 1, 2
               ip = INT(( - 1.0)**(ipar + 1))
C--------------do loop over decaying nucleus spin
               DO jcn = 1, NLW, LTUrbo
                  IF(GDRdyn.EQ.1.0D0)
     &               CALL ULMDYN(nnuc, jcn, EX(ke, nnuc))
99011             FORMAT(1X, 'J,DEF ', I3, F9.4)
                  DENhf = 0.0
                  IF(POP(ke, jcn, ipar, nnuc).LT.POPmax(nnuc))THEN
                     popleft = popleft + POP(ke, jcn, ipar, nnuc)*DE
                     GOTO 1610
                  ENDIF
C-----------------do loop over ejectiles
                  DO nejc = 1, NEJcm
                     nnur = NREs(nejc)
                     CALL DECAY(nnuc, ke, jcn, ip, nnur, nejc, sum)
                  ENDDO
C-----------------do loop over ejectiles       ***done***
C-----------------gamma emision
                  IF(LTUrbo.EQ.1)THEN
                     CALL DECAYG(nnuc, ke, jcn, ip, sum)
                  ELSE
                     CALL DECAYT(nnuc, ke, jcn, ip, sum)
                  ENDIF
C-----------------distribute yrast population over discrete levels
C
                  IF(DENhf.EQ.0.0D0)THEN
                     IF(ke.EQ.1)THEN
                        ded = DE*0.5
                     ELSE
                        ded = DE
                     ENDIF
                     IF(IOUt.GT.1)WRITE(6, 
     & '('' Yrast state at bin'',I4,'' spin='',F5.1,'' pop='',   G12.5)'
     & )ke, FLOAT(ip)*(FLOAT(jcn) + HIS(nnur)), POP(ke, jcn, ipar, nnuc)
     &  *ded
C--------------------look for the discrete level with the closest spin
                     xnl = 1.0
                     spdiff = 100.
                     DO il = 1, NLV(nnuc)
                        spdif = ABS(FLOAT(jcn) + HIS(nnur)
     &                          - XJLv(il, nnuc))
                        IF(spdif.LT.spdiff)THEN
                           spdiff = spdif
                           xnl = 1.
                        ELSE
                           IF(spdif.EQ.spdiff)xnl = xnl + 1.
                        ENDIF
                     ENDDO
                     DO il = 1, NLV(nnuc)
                        spdif = ABS(FLOAT(jcn) + HIS(nnur)
     &                          - XJLv(il, nnuc))
                        IF(spdif.EQ.spdiff)THEN
C-----------------------below is a 2.18 version approach (straight to the level)                        
c                          POPlv(il, nnuc) = POPlv(il, nnuc)
c    &                        + POP(ke, jcn, ipar, nnuc)*ded/xnl
c                          REClev(il, 0) = REClev(il, 0)
c    &                        + POP(ke, jcn, ipar, nnuc)*ded/xnl
C------------------------2.19 uses standard approach through the SCRtl matrix
                         SCRTl(il,0) = 1.0D0/xnl
                         DENhf = DENhf + SCRTl(il,0) 
                           IF(IOUt.GT.1)WRITE(6, 
     &'(10X,I3,''% of this was assumed to populate level #'',           
     &I3)')INT(100./xnl), il
                        ENDIF
                     ENDDO
                  ENDIF
C-----------------
C-----------------fission ()
                  IF(FISsil(nnuc).AND.(FISSHI(Nnuc).EQ.1.))
     &            CALL FISSION(nnuc, ke, jcn, sumfis)
                 
                  IF(FISsil(nnuc).AND.(FISSHI(Nnuc).NE.1.))THEN
                     dencomp = DENhf
                     aafis = 0.
                     IF(FISmod(Nnuc).eq.0.)THEN
                        CALL FISFIS(nnuc, ke, ip, jcn, sumfis,0)
                        IF(FISopt(Nnuc).gt.0.)THEN
                           IF(NRWel.eq.1) cota1 = (TF(1) + TF(2)+TG2)/2
                           IF(NRWel.eq.2) cota1 = (TF(1) + TDIr23+TG2)/2
                        ENDIF
                     ENDIF
                     IF(FISmod(Nnuc).gt.0.)THEN                       
                        tfb=0.                      
                        tdirect=0.
                        DO m=1,int(FISmod(Nnuc))+1
                           efb(2)=efbm(m)
                           DO k=1,nrfdis(2)
                              h(k,2)=hm(k,m)
                              efdis(k,2)=efdism(k,m)
                           ENDDO   
                           xminn(2)=xminnm(m)
                           nrbinfis(2)=nrbinfism(m)
                           DO kk=1,nrbinfis(2)
                              UGRid(kk,2)=UGRidf(kk,m)
                           ENDDO
                           DO jj=1,NLW
                              DO kk=1,nrbinfis(2)
                                 ROFis(kk,jj,2)= ROFism(kk,jj,m)
                              ENDDO
                           ENDDO
    
                           CALL FISFIS(nnuc, ke, ip, jcn, sumfis,m)
                           tfbm(m)=tf(2)
                           tfb=tfb+tfbm(m)
                           IF(FISopt(Nnuc).gt.0.)THEN
                              tdirm(m)=tdir
                              tdirect=tdirect+tdir  
                           ENDIF   
                        ENDDO
                        DO m=1,int(FISmod(Nnuc))+1
                           IF((tf(1)+tfb).gt.0.)THEN
                              sumfism(m)=tf(1)*tfbm(m)/(tf(1)+tfb)
                           ELSE
                              sumfism(m)=0.
                           ENDIF 
                           IF(FISopt(Nnuc).gt.0.)
     &                          cota1 = (TF(1) + TFb+TG2)/2
                        ENDDO   
                     ENDIF
   
                     IF(cota1.LE.EXPmax)THEN
                        cotaexp = EXP( - cota1)
                     ELSE
                        cotaexp = 0.d0
                     ENDIF
                     Cota = (1 + cotaexp**2)/(1 - cotaexp**2 +
     &                             0.00000001)
                  ENDIF
 
C-----------------normalization and accumulation
C-----------------
                  xnor = POP(ke, jcn, ipar, nnuc)*step/DENhf
                  stauc = stauc + RO(ke, jcn, nnuc)*xnor
                  IF(RO(ke, jcn, nnuc).NE.0.0D0)sgamc = sgamc +
     &               DENhf*POP(ke, jcn, ipar, nnuc)
     &               *step/RO(ke, jcn, nnuc)
C--------------subbarrier effects
                  IF(FISsil(nnuc) .AND. FISOPT(nnuc).gt.0.
     &               .AND.FISSHI(Nnuc).NE.1.)THEN
                     IF(FISmod(Nnuc).eq.0.)THEN
                        IF((dencomp + TDIr).GT.0.)THEN
                           xnorfis = xnor*DENhf/(dencomp + TDIr)              
                        ELSE
                           xnorfis=0.
                        ENDIF
                        IF(NRBarc.EQ.2.and.NRwel.eq.1)Tout=tf(2)
                        IF(NRBar.EQ.5)Tout=Tdir23
                        IF(Tout*Tabs.GT.0.)THEN
                           bbfis = (TDIr + dencomp)*(TF(1) + Tout + TG2)
     &                             /(TABs*(Tout + TG2))
                           aafis = (1. + bbfis**2 + 2*bbfis*cota)
     &                             **( - 0.5)
                        ELSE
                           aafis = 0.
                        ENDIF
C-----------------------fission
                        pfiso=0.
                        IF(tout.gt.0.and.tg2.gt.0.)
     &                  pfiso=xnorfis*(tdir+ dencomp)*(Rfiso-1.)*
     &                        TG2*aafis/(tout+TG2)
                        csfis = csfis +xnorfis*(tdir+ dencomp*aafis)+
     &                          pfiso
                     ENDIF
   
                     IF(FISmod(Nnuc).gt.0.)THEN
                        IF((dencomp + TDIrect).GT.0.)THEN
                           xnorfis = xnor*DENhf/(dencomp + TDIrect)
                        ELSE
                           xnorfis=0.
                        ENDIF
                        IF(Tfb*Tabs.GT.0.)THEN
                           bbfis = (TDIrect + dencomp)*
     &                             (TF(1) + Tfb)/(TABs*Tfb)
                           aafis = (1. + bbfis**2 + 2*bbfis*cota)
     &                             **( - 0.5)
                        ELSE
                           aafis = 0.
                        ENDIF
C-----------------------fission
                        DO m=1,int(FISmod(Nnuc))+1
                           csfism(m) = csfism(m) + xnorfis*
     &                        (tdirm(m)*(1.-aafis)+
     &                        tfbm(m)*aafis*(dencomp + TDIrect)/tfb)
                        ENDDO  
                     ENDIF     
C-----------------------particles
                     DO nejc = 1, NEJcm
                        nnur = NREs(nejc)
                        CALL ACCUM(ke, nnuc, nnur, nejc, xnor)
                        CSEmis(nejc, nnuc) = CSEmis(nejc, nnuc)
     &                     + xnorfis*SCRtem(nejc)*(1 - aafis)           
                     ENDDO
C-----------------gammas
                     CALL ACCUM(ke, nnuc, nnuc, 0, xnor)
                     CSEmis(0, nnuc) = CSEmis(0, nnuc)
     &                                 + xnorfis*SCRtem(0)*(1 - aafis)
                     POP(ke, jcn, ipar, nnuc) = 0.0
                     GOTO 1605
                  ENDIF
c---------------------------------------------------------------
c--------------no subbarrier effects
C-----------------particles
                  DO nejc = 1, NEJcm
                     nnur = NREs(nejc)
                     CALL ACCUM(ke, nnuc, nnur, nejc, xnor)
                     CSEmis(nejc, nnuc) = CSEmis(nejc, nnuc)
     &                  + xnor*SCRtem(nejc)
                  ENDDO
C-----------------gammas
                  CALL ACCUM(ke, nnuc, nnuc, 0, xnor)
                  CSEmis(0, nnuc) = CSEmis(0, nnuc) + xnor*SCRtem(0)
                  POP(ke, jcn, ipar, nnuc) = 0.0
C-----------------fission                 
                  IF(FISmod(nnuc).eq.0.)csfis = csfis + sumfis * xnor
                  IF(FISmod(nnuc).gt.0.)THEN
                     DO m=1,int(Fismod(nnuc))+1
                        csfism(m)=csfism(m)+sumfism(m)*xnor
                     ENDDO   
                  ENDIF   
C-----------------calculate total emission
 1605             DO nejc = 0, NEJcm
                     csemist = csemist + CSEmis(nejc, nnuc)
                  ENDDO
                  csemist = csemist + csfis
 1610          ENDDO                   !loop over decaying nucleus spin
            ENDDO                   !loop over decaying nucleus parity
            IF(ENDf.GT.0)CALL RECOIL(ke, nnuc)  !recoil spectrum for ke bin
            IF(Fismod(nnuc).eq.0.)WRITE(80,*) 'csfis=', csfis, ' mb'
            IF(Fismod(nnuc).gt.0.)THEN
               write(80,*)'  '
               DO m=1,int(Fismod(nnuc))+1
                  WRITE(80,*)'    Mode=',m,'  csfis=', csfism(m), ' mb'
               ENDDO
            ENDIF
         ENDDO                  !loop over c.n. excitation energy
cccc!!!!!! csemist multimodal
C--------
C--------Hauser-Feshbach decay of nnuc  ***done***
C--------
C--------printout of results for the decay of NNUC nucleus
         IF(IOUt.GT.0)WRITE(6,
     &          '(1X,/,'' Population left because too small '',G12.5,/)'
     &          )popleft*DE
 1650    IF(IOUt.GT.0)WRITE(6,
     &                      '(1X,/,10X,''Discrete level population'')')
         IF(IOUt.GT.0 .AND. kemin.EQ.NEX(nnuc) .AND. nnuc.EQ.1)WRITE(6, 
     &'(10X,''(no gamma cascade in the compound nucleus, primary transit
     &ions only)'',/)')
         IF(IOUt.GT.0 .AND. ENDf.NE.0.0D0 .AND. 
     &      (nnuc.EQ.mt91 .OR. nnuc.EQ.mt649 .OR. nnuc.EQ.mt849))
     &      WRITE(6, 
     &'(10X,''NOTE: due to ENDF option direct particle contribution was 
     &shifted to the g.s.'')')
         IF(IOUt.GT.0)WRITE(6, '(1X,/,10X,40(1H-),/)')
         IF(ENDf.NE.0 .AND. nnuc.EQ.1)THEN
            WRITE(12, '(1X,/,10X,''Discrete level population '',
     &      ''before gamma cascade'')')
            WRITE(12, '(1X,/,10X,40(1H-),/)')
         ENDIF
         DO il = 1, NLV(nnuc)
            CSPrd(nnuc) = CSPrd(nnuc) + POPlv(il, nnuc)
            IF(IOUt.GT.0)WRITE(6, 99012)il, ELV(il, nnuc), LVP(il, nnuc)
     &                                  , XJLv(il, nnuc), 
     &                                  POPlv(il, nnuc)
            IF(ENDf.NE.0 .AND. nnuc.EQ.1) THEN
c--------------check for the number of branching ratios
               nbr = 0
               DO ib = 1, NDBR
                  IF(BR(il, ib, 2, nnuc).EQ.0.) EXIT
                  nbr = ib
               ENDDO
               IF(nbr.EQ.0 .AND. il.NE.1 .AND. FIRst_ein )WRITE(6, *)
     &            ' WARNING: Branching ratios for level ', il, 
     &            ' in ', INT(A(nnuc)), '-', SYMb(nnuc), 
     &            ' are missing'
               WRITE(12,99012)il, ELV(il, nnuc), LVP(il, nnuc),
     &                      XJLv(il, nnuc), POPlv(il, nnuc), nbr, 
     &                      (NINT(BR(il,ib,1,nnuc)), 
     &                      BR(il, ib, 2, nnuc), ib = 1, nbr)
            ENDIF
         ENDDO
         IF(ENDf.GT.0) THEN 
            WRITE(12, '(1X,/,10X,40(1H-),/)')
            WRITE(12,*)' ' 
C-----------write Int. Conv. Coefff. for discrete transitions               
            WRITE(12, 
     &            '(1X,/,10X,''Internal conversion coefficients'')')
            WRITE(12, '(1X,/,10X,40(1H-),/)')
            
            DO il = 1, NLV(nnuc)
C--------------check for the number of branching ratios
               nbr = 0
               DO ib = 1, NDBR
                  IF(BR(il, ib, 2, nnuc).EQ.0.)GOTO 1567
                  nbr = ib
               ENDDO
 1567          CONTINUE
               WRITE(12, 99014)il, ELV(il, nnuc), LVP(il, nnuc), 
     &                         XJLv(il, nnuc), POPlv(il, nnuc), nbr, 
     &                         (NINT(BR(il,ib,1,nnuc)), 
     &                         BR(il, ib, 3, nnuc), ib = 1, nbr)
            ENDDO
            WRITE(12, '(1X,/,10X,40(1H-),/)')
         ENDIF 
C--------gamma decay of discrete levels (DECAYD)
         CALL DECAYD(nnuc)
         ia = INT(A(nnuc))
         iz = INT(Z(nnuc))
         IF(IOUt.GT.0)THEN
            WRITE(6, '(1X,/,10X,40(1H-),/)')
            WRITE(6, 
     &'(1X,I3,''-'',A2,''-'',I3,'' production cross section '',G12.5,'' 
     &mb  '',''reaction: '',A21)')iz, SYMb(nnuc), ia, CSPrd(nnuc), 
     &reaction(nnuc)
            IF(kemin.EQ.NEX(nnuc))WRITE(6, 
     &'(1X,''(no gamma cascade in the compound nucleus, primary transiti
     &ons only)'',/)')
         ENDIF
C--------Integrating exclusive population spectra (ENDF)
         gtotsp=0
         xtotsp=0
         ptotsp=0
         atotsp=0
         htotsp=0
         emedg=0
         emedn=0
         emedp=0
         emeda=0
         emedh=0
         DO ispec=1,NEX(1)+10
            gtotsp=gtotsp+POPcse(0,0,ispec,nnuc)*de
            xtotsp=xtotsp+POPcse(0,1,ispec,nnuc)*de
            ptotsp=ptotsp+POPcse(0,2,ispec,nnuc)*de
            atotsp=atotsp+POPcse(0,3,ispec,nnuc)*de
            emedg=emedg+POPcse(0,0,ispec,nnuc)*de*(ispec-1)*DE
            emedn=emedn+POPcse(0,1,ispec,nnuc)*de*(ispec-1)*DE
            emedp=emedp+POPcse(0,2,ispec,nnuc)*de*(ispec-1)*DE
            emeda=emeda+POPcse(0,3,ispec,nnuc)*de*(ispec-1)*DE
            IF(NDEJC.EQ.4) THEN
               htotsp=htotsp+POPcse(0,NDEJC,ispec,nnuc)*de
               emedh=emedh+POPcse(0,NDEJC,ispec,nnuc)*de*(ispec-1)*DE
            ENDIF
         ENDDO
c        Add contribution from discrete levels for MT=91,649,849
         IF(nnuc.EQ.mt91) THEN
            nejc = 1
            DO ilev = 1, NLV(nnuc)
               xtotsp = xtotsp + CSDirlev(ilev,nejc)
            ENDDO 
         ELSEIF(nnuc.EQ.mt649) THEN
            nejc = 2
            DO ilev = 1, NLV(nnuc)
               ptotsp = ptotsp + CSDirlev(ilev,nejc)
            ENDDO 
         ELSEIF(nnuc.EQ.mt849) THEN
            nejc = 3
            DO ilev = 1, NLV(nnuc)
               atotsp = atotsp + CSDirlev(ilev,nejc)
            ENDDO 
         ENDIF 
         POPCS(0,nnuc) = gtotsp
         POPCS(1,nnuc) = xtotsp
         POPCS(2,nnuc) = ptotsp
         POPCS(3,nnuc) = atotsp
         IF(NDEJC.EQ.4) POPCS(NDEJC,nnuc) = htotsp
         IF(CSPrd(nnuc).NE.0.0D+0) THEN
            emedg=emedg/CSPrd(nnuc)
            emedn=emedn/CSPrd(nnuc)
            emedp=emedp/CSPrd(nnuc)
            emeda=emeda/CSPrd(nnuc)
            emedh=emedh/CSPrd(nnuc)
         ENDIF
         IF(IOUt.GT.3) THEN
            WRITE(6,*) '----------------------------------------------'
            WRITE(6,*) 'Test printout (exclusive spectra)'
            WRITE(6,'('' Energy'',12x,''gamma'',10x,''neutron'',8x,
     &      ''proton'',9x,''alpha'',8x,''l. ion'')')
            WRITE(6,*) '----------------------------------------------'
            DO ispec=1,NEX(1)+10
               IF(NDEJC.EQ.4) THEN
               WRITE(6,'(6g15.5)')(ispec-1)*DE,
     &                 POPcse(0,0,ispec,nnuc),POPcse(0,1,ispec,nnuc),
     &                 POPcse(0,2,ispec,nnuc),POPcse(0,3,ispec,nnuc),
     &                 POPcse(0,NDEJC,ispec,nnuc)
               ELSE
               WRITE(6,'(5g15.5)')(ispec-1)*DE,
     &                 POPcse(0,0,ispec,nnuc),POPcse(0,1,ispec,nnuc),
     &                 POPcse(0,2,ispec,nnuc),POPcse(0,3,ispec,nnuc)
               ENDIF
            ENDDO
            WRITE(6,*) '-----------------------------------------'
            WRITE(6,'(15X,5g15.5)') gtotsp,xtotsp,ptotsp,atotsp,htotsp
            WRITE(6,'(''E-aver.'',8X,6g15.5)') emedg,emedn,emedp,emeda,
     &               emedh,emedg+emedn+emedp+emeda+emedh
            WRITE(6,*) '-----------------------------------------'
            WRITE(6,*) ' '
            WRITE(6,*) '----------------------------------------------'
            WRITE(6,*) 'Test printout (portions of DDX spectra)'
            WRITE(6,'('' Energy'',12x,''neutron'',10x,''proton'')')
            WRITE(6,*) '----------------------------------------------'
            DO ispec=1,NEX(1)+10
               WRITE(6,'(5g15.5)')(ispec-1)*DE,
     &                 POPcseaf(0,1,ispec,nnuc),
     &                 POPcseaf(0,2,ispec,nnuc)
            ENDDO
            WRITE(6, *)' '
         ENDIF
C-----------calculate life-times and widths
         IF(IOUt.GT.0)THEN
            IF(csemist.NE.0.0D0)THEN
               taut = stauc*6.589E-22*2.0*PI/csemist
               WRITE(6, '('' Average total   life-time'',G12.5,'' s'')')
     &               taut
               gamt = sgamc/2.0/PI/csemist
               WRITE(6, 
     &               '('' Average total   width    '',G12.5,'' MeV'')')
     &               gamt
C              TAUT=6.589E-22/GAMT
C              WRITE(6,'('' Average total life-time 1/width'',g12.5,
C              1       '' s'')') TAUT
            ENDIF
            IF(csfis.NE.0.0D0)THEN
               tauf = stauc*6.589E-22*2.0*PI/csfis
               WRITE(6, '('' Average fission life-time'',G12.5,'' s'')')
     &               tauf
               gamfis = gamt*csfis/csemist
               WRITE(6, 
     &  '('' Average fission width    '',G12.5,'' MeV'')               '
     &  )gamfis
            ENDIF
C-----------life-times and widths  *** done ***
            IF(FISmod(Nnuc).gt.0)THEN
               DO m=1,int(FISmod(nnuc))+1
                  csfis=csfis+csfism(m)
               ENDDO  
               DO m=1,int(FISmod(nnuc))+1
                  IF (csfis.gt.0.)wfism(m)=csfism(m)/csfis
                  write(80,*)'    Mode=',m, '   weight=',wfism(m)
               ENDDO  
               write(80,*)'   Fission cross section=',csfis,' mb'
            ENDIF   
            WRITE(6, *)' '
            WRITE(6, '('' Fission    cross section    '',G12.5,'' mb'')'
     &            )csfis
         ENDIF
         TOTcsfis = TOTcsfis + csfis
C--------add compound elastic to shape elastic before everything falls
C--------down on the ground state
         IF(nnuc.EQ.1 .AND. INT(AEJC(0)).NE.0)THEN
            WRITE(6,*) 
            WRITE(6,*) ' SHAPE ELASTIC :', ELAcs,' mb'
            WRITE(6,*) ' CN ELASTIC    :', POPlv(1, mt2),' mb'
            ELAcs = ELAcs + POPlv(1, mt2)
C-----------CN contribution to elastic ddx
            elcncs = POPlv(1, mt2)/4.0/PI
            WRITE(6,*) ' CN ELASTIC ANG.DIST. :', elcncs,' mb/str'
            WRITE(6,*) 
         ENDIF
         WRITE(12, 
     &'(1X,I3,''-'',A2,''-'',I3,'' production cross section '',G12.6,'' 
     &mb'')')iz, SYMb(nnuc), ia, CSPrd(nnuc)
         WRITE(12, '(''    fission  cross section'',G12.5,'' mb'')')
     &         csfis
         IF(IOUt.GT.2)CALL AUERST(nnuc, 0)
         WRITE(6, '(''  g  emission cross section'',G12.5,'' mb'')')
     &         CSEmis(0, nnuc)
         WRITE(12, '('' g  emission cross section'',G12.5,'' mb'')')
     &         CSEmis(0, nnuc)
         DO nejc = 1, NEJcm
            nnur = NREs(nejc)
            IF(IOUt.GT.2)CALL AUERST(nnuc, nejc)
            IF(IOUt.GT.0)WRITE(6, 
     &               '(2X,A2,'' emission cross section'',G12.5,'' mb'')'
     &               )SYMbe(nejc), CSEmis(nejc, nnuc)
            WRITE(12, 
     &            '(1X,A2,'' emission cross section'',G12.5,'' mb'')')
     &            SYMbe(nejc), CSEmis(nejc, nnuc)
C-----------print residual nucleus population
            IF(IOUt.EQ.4)THEN
               nnur = NREs(nejc)
               ia = INT(A(nnur))
               WRITE(6, '('' Residual nucleus '',I3,''-'',A2,/)')ia, 
     &               SYMb(nnur)
               WRITE(6, '('' Positive parities population'',/)')
               WRITE(6, 99013)(EX(i, nnur), (POP(i,j,1,nnur), j = 1, 12)
     &                        , i = NEX(nnur), 1, ( - 1))
               WRITE(6, '('' Negative parities population'',/)')
               WRITE(6, 99013)(EX(i, nnur), (POP(i,j,2,nnur), j = 1, 12)
     &                        , i = NEX(nnur), 1, ( - 1))
               WRITE(6, '('' '')')
               poptot = 0.0
               DO j = 1, NLW
                  DO i = 1, NEX(nnur)
                     poptot = poptot + POP(i, j, 1, nnur)
     &                        + POP(i, j, 2, nnur)
                  ENDDO
                  poptot = poptot - 
     &                     0.5*(POP(1, j, 1, nnur) + POP(1, j, 2, nnur))
               ENDDO
               poptot = poptot*DE
               WRITE(6, *)'Total population of continuum    ', poptot, 
     &                    ' mb'
               poplev = 0.0
               DO i = 1, NLV(nnur)
                  poplev = poplev + POPlv(i, nnur)
               ENDDO
               WRITE(6, *)'Total population of disc. levels ', poplev, 
     &                    ' mb'
               WRITE(6, *)' '
            ENDIF
C-----------This block should go away with the new way of treating exclusive
C-----------spectra. We do not want to rpint them as they are identical to
C-----------those printed already before for pure MSD (no CN is added)
C-----------print double differential cross sections
c           IF(CSMsd(nejc).GT.0.D0 .AND. IOUt.GE.3 .AND. nnuc.EQ.1)THEN
c              itimes = FLOAT(NDANG)/11.0 + 0.95
c              DO its = 1, itimes
c                 iad = 1 + (its - 1)*11
c                 iam = 11 + (its - 1)*11
c                 iam = MIN0(NDANG, iam)
c                 WRITE(6, *)' '
c                 WRITE(6, 
c    &                 '(30X,''A      n      g      l      e      s '')'
c    &                 )
c                 WRITE(6, *)' '
c                 WRITE(6, '('' Energy  '',11(4X,F5.1,2X))')
c    &                  (ANGles(ia), ia = iad, iam)
c                 WRITE(6, *)' '
c                 DO i = 1, NEX(1)
c                    WRITE(6, '(1X,F7.3,1X,11E11.4)')FLOAT(i - 1)*DE, 
c    &                     (CSEa(i, iang, nejc, 1), iang = iad, iam)
c                 ENDDO
c                 WRITE(6, *)' '
c              ENDDO
c           ENDIF
         ENDDO   !over ejectiles
C--------
C--------NNUC nucleus decay    **** done ******
C--------
      ENDDO     !over decaying nuclei
C     fisfis d
      CLOSE(80)
      CLOSE(79)
C     fisfisu
      IF(ENDf.EQ.1)THEN
C--------
C--------ENDF spectra printout (exact exclusive representation)
C-------- 
         DO nnuc = 1, NNUcd               !loop over decaying nuclei
            IF(CSPrd(nnuc).GT.0.0D0)THEN 
               DO nejc = 0, NDEJC         !loop over ejectiles
                  IF(POPCS(nejc,nnuc).EQ.0) CYCLE 
                  nspec = INT(EMAx(nnuc)/DE) + 2
                  nexrt = INT((EMAx(nnuc) - ECUt(Nnuc))/DE + 1.0001)
C
C        Mike, check the lines below
C        It was a bug before. nexrt could be negative
C        I intriduced a jump for this case, but may be this is not the proper thing to do !!! (08/2004) 
C
                    if(nexrt.le.1) cycle 

                  IF(nejc.EQ.0) THEN 
                     cejectile = 'gammas   '
                     iizaejc = 0
                  ELSEIF(nejc.EQ.1) THEN 
                     cejectile = 'neutrons '
                     iizaejc = izaejc(nejc)
                  ELSEIF(nejc.EQ.2) THEN 
                     cejectile = 'protons  '
                     iizaejc = izaejc(nejc)
                  ELSEIF(nejc.EQ.3) THEN 
                     cejectile = 'alphas   '
                     iizaejc = izaejc(nejc)
                  ELSEIF(nejc.EQ.4) THEN 
                     cejectile = 'lt. ions '
                     iizaejc = izaejc(nejc)
                  ENDIF
C-----------------double the first bin x-sec to preserve integral in EMPEND
c                 POPcse(0, nejc, 1, nnuc) =  POPcse(0, nejc, 1, nnuc)*2
                  WRITE(12, *)' '
                  WRITE(12, *)' Spectrum of ', cejectile, 
     &                  reaction(nnuc), ' ZAP= ', iizaejc
C-----------------recorp is a recoil correction factor defined 1+Ap/Ar that
C-----------------multiplies cross sections and divides outgoing energies
                  recorp = 1.0
                  IF(nejc.GT.0)
     &               recorp = 1. + EJMass(nejc)/AMAss(nnuc)
C-----------------Exclusive DDX spectra (neutrons & protons) 
                  IF(nejc.GE.1 .AND. nejc.LE.NDEjcd) THEN  
                     WRITE(12, 
     &                   '(30X,''A     n     g     l     e     s '')')
                     WRITE(12, *)' '
                     WRITE(12, '('' Energy   '',8G15.5,/,(10X,8G15.5))')
     &                           ANGles
                     IF((nnuc.EQ.mt91 .AND. nejc.EQ.1) .OR. 
     &                  (nnuc.EQ.mt649 .AND. nejc.EQ.2)) THEN ! first emission reactions
C-----------------------(discrete levels part)
                        DO il = 1, NLV(nnuc), 1  !(levels)
                           espec = (EMAx(nnuc) - ELV(il, nnuc))/recorp
                           IF(espec.GE.0)
     &                     WRITE(12, '(F10.5,E14.5,7E15.5,/,
     &                           (9X,8E15.5))') -espec, 
     &                           (CSAlev(nang, il, nejc)*recorp/DE, 
     &                            nang = 1, NDANG)
                        ENDDO
C-----------------------(continuum part)
                        DO ie = 1, nspec+1 ! clean DDX matrix
                           DO nang = 1,NDANG 
                              CSEa(ie, nang, nejc, 0) = 0.0
                           ENDDO
                        ENDDO
                        DO ie = 1, nexrt ! reconstruct continuum DDX spectrum
                           DO nang = 1,NDANG 
                              piece = CSEmsd(ie,nejc)
                              IF(ie.EQ.NEXr(nejc, 1)) piece = 0.5*piece
                              CSEa(ie, nang, nejc, 0) = 
     &                        ((POPcse(0,nejc,ie,nnuc) - piece
     &                        *POPcseaf(0,nejc,ie,Nnuc))
     &                        /4.0/PI + CSEa(ie, nang, nejc, 1)*
     &                        POPcseaf(0,nejc,ie,Nnuc))
c                             WRITE(6,*)'ie,nang,nnuc,nejc',
c    &                           ie,nang,nnuc,nejc 
c                             WRITE(6,*)'POPcse,piece,POPcseaf',
c    &                           POPcse(0,nejc,ie,nnuc),piece,
c    &                           POPcseaf(0,nejc,ie,Nnuc)
                           ENDDO
                        ENDDO
                        DO ie = nexrt+1, nspec ! reconstruct discrete DDX spectrum
                           DO nang = 1,NDANG 
c                             piece = CSEmsd(ie,nejc)
c                             IF(ie.EQ.NEXr(nejc, 1)) piece = 0.5*piece
                              CSEa(ie, nang, nejc, 0) = 
     &                           POPcse(0,nejc,ie,nnuc)/4.0/PI 
c                             CSEa(ie, nang, nejc, 0) = 
c    &                        (POPcse(0,nejc,ie,nnuc) - piece
c    &                        *POPcseaf(0,nejc,ie,Nnuc))/4.0/PI
c                             WRITE(6,*)'ie,nang,nnuc,nejc',
c    &                           ie,nang,nnuc,nejc 
c                             WRITE(6,*)'POPcse,piece,POPcseaf',
c    &                           POPcse(0,nejc,ie,nnuc),piece,
c    &                           POPcseaf(0,nejc,ie,Nnuc)
                           ENDDO
                        ENDDO
                        DO nang = 1,NDANG !double the first bin to preserve integral in EMPEND
                           CSEa(1, nang, nejc, 0) = 
     &                     CSEa(1, nang, nejc, 0)*2.0
                        ENDDO 
                        DO ie = 1, nspec-1 ! print DDX spectrum
                            WRITE(12, '(F10.5,E14.5,7E15.5,/,
     &                      (9X,8E15.5))') FLOAT(ie - 1)*DE/recorp, 
     &                      (CSEa(ie, nang, nejc, 0)*recorp,
     &                      nang = 1, NDANG)
                        ENDDO
                        DO ie = nspec, nspec+1 ! exact DDX spectrum endpoint
                            WRITE(12, '(F10.5,E14.5,7E15.5,/,
     &                      (9X,8E15.5))') EMAx(nnuc)/recorp, 
     &                      (CSEa(ie, nang, nejc, 0)*recorp,
     &                      nang = 1, NDANG)
                        ENDDO
                     ELSE 
C-----------------------remaining n- or p-emissions (continnum and levels together)
                        DO ie = 1, nspec+1 ! clean DDX matrix
                           DO nang = 1,NDANG 
                              CSEa(ie, nang, nejc, 0) = 0.0
                           ENDDO
                        ENDDO
                        DO ie = 1, nspec  ! reconstruct DDX spectrum
                           DO nang = 1,NDANG 
                              piece = CSEmsd(ie,nejc)
                              IF(ie.EQ.NEXr(nejc, 1)) piece = 0.5*piece
                              CSEa(ie, nang, nejc, 0) = 
     &                        ((POPcse(0,nejc,ie,nnuc) - piece
     &                        *POPcseaf(0,nejc,ie,Nnuc))
     &                        /4.0/PI + CSEa(ie, nang, nejc, 1)*
     &                        POPcseaf(0,nejc,ie,Nnuc))
                           ENDDO
                        ENDDO
                        DO nang = 1,NDANG !double the first bin to preserve integral in EMPEND
                           CSEa(1, nang, nejc, 0) = 
     &                     CSEa(1, nang, nejc, 0)*2.0
                        ENDDO 
                        DO ie = 1, nspec-1 ! print DDX spectrum
                            WRITE(12, '(F10.5,E14.5,7E15.5,/,
     &                      (9X,8E15.5))') FLOAT(ie - 1)*DE/recorp, 
     &                      (CSEa(ie, nang, nejc, 0)*recorp,
     &                      nang = 1, NDANG)
                        ENDDO
                        DO ie = nspec, nspec+1 ! exact DDX spectrum endpoint
                            WRITE(12, '(F10.5,E14.5,7E15.5,/,
     &                      (9X,8E15.5))') EMAx(nnuc)/recorp, 
     &                      (CSEa(ie, nang, nejc, 0)*recorp,
     &                      nang = 1, NDANG)
                        ENDDO
                     ENDIF 
C-----------------Exclusive DDX spectra (gammas, alphas, light ions (DE)) 
                  ELSE 
C--------------------double the first bin x-sec to preserve integral in EMPEND
                     POPcse(0, nejc, 1, nnuc) =  
     &                  POPcse(0, nejc, 1, nnuc)*2
                     WRITE(12, *)' '
                     WRITE(12, '('' Energy    mb/MeV'')')
                     WRITE(12, *)' '
                     IF(nnuc.EQ.mt849 .AND. nejc.EQ.3) THEN ! first emission (z,a) reaction
                        DO il = 1, NLV(nnuc) ! MT=801,802,... (levels)
C                          Although DDX spectra are available for a-emission
C                          they are isotropic and only ang. integrated are 
C                          printed (4*Pi*CSAlev(1,il,3)
                           espec = (EMAx(nnuc) - ELV(il, nnuc))/recorp
                           IF(espec.GE.0)
     &                     WRITE(12, '(F10.5,E14.5)')
     &                           -espec, CSAlev(1, il, 3)*4.0*PI*
     &                           recorp/DE 
                        ENDDO
                        DO ie = 1,  nspec-1 ! MT=849 (continuum)
                           WRITE(12, '(F10.5,E14.5)')FLOAT(ie - 1)*DE/ 
     &                          recorp, POPcse(0, nejc, ie, nnuc)*recorp
                        ENDDO
                        DO ie = nspec,  nspec+1 ! MT=849 exact endpoint
                           WRITE(12, '(F10.5,E14.5)')EMAx(nnuc)/ 
     &                          recorp, POPcse(0, nejc, ie, nnuc)*recorp
                        ENDDO
                     ELSE  !all other emissions (continnum and levels together)
                        DO ie = 1, nspec-1
                           WRITE(12, '(F10.5,E14.5)')FLOAT(ie - 1)*DE/ 
     &                          recorp, POPcse(0, nejc, ie, nnuc)*recorp
                        ENDDO
                        DO ie = nspec, nspec+1  ! exact endpoint
                           WRITE(12, '(F10.5,E14.5)')EMAx(nnuc)/ 
     &                          recorp, POPcse(0, nejc, ie, nnuc)*recorp
                        ENDDO
                     ENDIF 
                  ENDIF 
               ENDDO  ! over ejectiles
            ENDIF
            IF(nnuc.NE.1) CALL PRINT_RECOIL(nnuc,reaction(nnuc))
         ENDDO  ! over decaying nuclei
C--------Fission related spectra of particles and gammas
         IF(TOTcsfis.GT.0.0D0)THEN 
            DO nejc = 0, NDEJC         !loop over ejectiles
c              IF(POPCS(nejc,nnuc).EQ.0) CYCLE 
               IF(nejc.EQ.0) THEN 
                  cejectile = 'gammas   '
                  iizaejc = 0
                  espmax = EMAx(1) 
               ELSEIF(nejc.EQ.1) THEN 
                  cejectile = 'neutrons '
                  iizaejc = izaejc(nejc)
                  espmax = EMAx(1)-Q(1,1) 
               ELSEIF(nejc.EQ.2) THEN 
                  cejectile = 'protons  '
                  iizaejc = izaejc(nejc)
                  espmax = EMAx(1)-Q(2,1) 
               ELSEIF(nejc.EQ.3) THEN 
                  cejectile = 'alphas   '
                  iizaejc = izaejc(nejc)
                  espmax = EMAx(1)-Q(3,1) 
               ELSEIF(nejc.EQ.4) THEN 
                  cejectile = 'lt. ions '
                  iizaejc = izaejc(nejc)
                  espmax = EMAx(1)-Q(NDEJC,1) 
               ENDIF
               nspec = INT(espmax/DE) + 2
C--------------double the first bin x-sec to preserve integral in EMPEND
               CSEfis(1,nejc) = CSEfis(1,nejc)*2 
               WRITE(12, *)' '
               WRITE(12, *)' Spectrum of ', cejectile, 
     &               '(z,fission)          ', ' ZAP= ', iizaejc
               WRITE(12, *)' '
               WRITE(12, '('' Energy    mb/MeV'')')
               WRITE(12, *)' '
               DO ie = 1, nspec-1
                  WRITE(12, '(F10.5,E14.5)')FLOAT(ie - 1)*DE 
     &                 , CSEfis(ie,nejc)
               ENDDO
               DO ie = nspec, nspec+1
                  WRITE(12, '(F10.5,E14.5)')espmax 
     &                 , CSEfis(ie,nejc)
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
      IF(ENDf.EQ.2.0D0)THEN
         DO nnuc = 1, NNUcd
            DO nejc = 0, NEJcm
               IF(nejc.GT.0)THEN
                  recorr = (AMAss(nnuc) - EJMass(nejc))/AMAss(nnuc)
               ELSE
                  recorr = 1.0
               ENDIF
               DO icse = 1, NDEX
                  xccm = (icse - 1)*recorr + 1.0000001
                  iccml = xccm
                  iccmh = MIN(NDEX, iccml + 1)
                  weight = xccm - iccml
C-----------------energy spectra
                  CSE(iccml, nejc, 0) = CSE(iccml, nejc, 0)
     &                                  + CSE(icse, nejc, nnuc)
     &                                  *(1.0 - weight)
C                 double contribution to the first energy bin to
C                 to conserve the integral
                  IF(iccml.EQ.1 .AND. icse.NE.1)CSE(iccml, nejc, 0)
     &               = CSE(iccml, nejc, 0) + CSE(icse, nejc, nnuc)
     &               *(1.0 - weight)
                  CSE(iccmh, nejc, 0) = CSE(iccmh, nejc, 0)
     &                                  + CSE(icse, nejc, nnuc)*weight
C-----------------double-differential spectra
                  IF(nnuc.EQ.1) THEN !CN with possibly anisotropic distr.
                     DO nang = 1, NDANG
                        CSEa(iccml, nang, nejc, 0)
     &                     = CSEa(iccml, nang, nejc, 0)
     &                     + CSEa(icse, nang, nejc, 1)*(1.0 - weight)
C                       double contribution to the first energy bin to
C                       to conserve the integral
                        IF(iccml.EQ.1 .AND. icse.NE.1)
     &                     CSEa(iccml, nang, nejc, 0)
     &                     = CSEa(iccml, nang, nejc, 0)
     &                     + CSEa(icse, nang, nejc, 1)*(1.0 - weight)
                           CSEa(iccmh, nang, nejc, 0)
     &                     = CSEa(iccmh, nang, nejc, 0)
     &                     + CSEa(icse, nang, nejc, 1)*weight
                     ENDDO
                  ELSE !other residues with isotropic ang. distributions
                     piece = CSE(icse, nejc, nnuc)/4.0/PI
c      WRITE(6,*)'nuc,ejc,piece',nnuc,nejc,piece
                     DO nang = 1, NDANG
                        CSEa(iccml, nang, nejc, 0)
     &                     = CSEa(iccml, nang, nejc, 0)
     &                     + piece*(1.0 - weight)
C                       double contribution to the first energy bin to
C                       to conserve the integral
                        IF(iccml.EQ.1 .AND. icse.NE.1)
     &                     CSEa(iccml, nang, nejc, 0)
     &                     = CSEa(iccml, nang, nejc, 0)
     &                     + piece*(1.0 - weight)
                           CSEa(iccmh, nang, nejc, 0)
     &                     = CSEa(iccmh, nang, nejc, 0)
     &                     + piece*weight
                     ENDDO
                     
                  ENDIF 
               ENDDO
            ENDDO
         ENDDO
      ENDIF
      WRITE(6, *)' '
      WRITE(6, '(''  Total fission cross section '',G12.5,'' mb'')')
     &      TOTcsfis
      WRITE(12, *)' '
      WRITE(12, '('' Tot. fission cross section '',G12.4,'' mb'')')
     &      TOTcsfis
      IF(IOUt.GT.1)THEN
         WRITE(6, '(1X,//,''Inclusive spectra (C.M.)'',//)')
         DO nejc = 0, NEJcm
            CALL AUERST(0, nejc)
         ENDDO
      ENDIF
C-----
C-----ENDF spectra printout (inclusive representation MT = 10 or 5
C-----
      IF(ENDf.EQ.2.0D0)THEN
C--------print spectra of residues
         reactionx = '(z,x)  '
         DO nnuc = 1, NNUcd    !loop over decaying nuclei
            CALL PRINT_RECOIL(nnuc,reactionx)
         ENDDO !over decaying nuclei in ENDF spectra printout
C--------print inclusive gamma spectrum
         nspec = INT(EMAx(1)/DE) + 2
         IF(nspec.GT.NDECSE-1)nspec = NDECSE-1
         WRITE(12, *)' '
         WRITE(12, *)' Spectrum of gammas   (n,x) ZAP=     0'
         WRITE(12, *)' '
         WRITE(12, '('' Energy    mb/MeV'')')
         WRITE(12, *)' '
         DO ie = 1, nspec-1
            WRITE(12, '(F9.4,E15.5)')FLOAT(ie - 1)*DE, CSE(ie, 0, 0)
         ENDDO
         CSE(nspec+1, 0, 0) = 0.0D0
         DO ie = nspec, nspec+1  ! exact endpoint
            WRITE(12, '(F9.4,E15.5)')EMAx(1), CSE(ie, 0, 0)
         ENDDO
C--------print inclusive spectra of ejectiles
C--------neutrons
         nspec = INT((EMAx(1) - Q(1,1))/DE) + 2
         IF(nspec.GT.NDECSE-1)nspec = NDECSE-1
         WRITE(12, *)' '
         WRITE(12, *)' Spectrum of neutrons (n,x) ZAP=     1'
         WRITE(12, '(30X,''A      n      g      l      e      s '')')
         WRITE(12, *)' '
         WRITE(12, '('' Energy   '',8G15.5,/,(10X,8G15.5))')ANGles
         DO ie = 1, nspec-1
            WRITE(12, '(F9.4,8E15.5,/,(9X,8E15.5))')FLOAT(ie - 1)*DE, 
     &            (CSEa(ie, nang, 1, 0), nang = 1, NDANG)
         ENDDO
         DO ie = nspec, nspec+1  ! exact endpoint
            WRITE(12, '(F9.4,8E15.5,/,(9X,8E15.5))') EMAx(1) - Q(1,1), 
     &            (CSEa(ie, nang, 1, 0), nang = 1, NDANG)
         ENDDO
C--------protons
         nspec = INT((EMAx(1) - Q(2,1))/DE) + 2
         IF(nspec.GT.NDECSE-1)nspec = NDECSE-1
         WRITE(12, *)' '
         WRITE(12, *)' Spectrum of protons  (n,x) ZAP=  1001'
         WRITE(12, '(30X,''A      n      g      l      e      s '')')
         WRITE(12, *)' '
         WRITE(12, '('' Energy   '',8G15.5,/,(10X,8G15.5))')ANGles
         DO ie = 1, nspec-1
            WRITE(12, '(F9.4,8E15.5,/,(9X,8E15.5))')FLOAT(ie - 1)*DE, 
     &            (CSEa(ie, nang, 2, 0), nang = 1, NDANG)
         ENDDO
         DO ie = nspec, nspec+1 ! exact endpoint
            WRITE(12, '(F9.4,8E15.5,/,(9X,8E15.5))') EMAx(1) - Q(2,1), 
     &            (CSEa(ie, nang, 2, 0), nang = 1, NDANG)
         ENDDO
C--------alphas
         nspec = INT((EMAx(1) - Q(3,1))/DE) + 2
         IF(nspec.GT.NDECSE-1)nspec = NDECSE-1
         WRITE(12, *)' '
         WRITE(12, *)' Spectrum of alphas   (n,x) ZAP=  2004'
         WRITE(12, '(30X,''A      n      g      l      e      s '')')
         WRITE(12, *)' '
         WRITE(12, '('' Energy   '',8G15.5,/,(10X,8G15.5))')ANGles
         DO ie = 1, nspec-1
            WRITE(12, '(F9.4,8E15.5,/,(9X,8E15.5))')FLOAT(ie - 1)*DE, 
     &            (CSEa(ie, nang, 3, 0), nang = 1, NDANG)
         ENDDO
         DO ie = nspec, nspec+1 ! exact endpoint
            WRITE(12, '(F9.4,8E15.5,/,(9X,8E15.5))') EMAx(1) - Q(3,1), 
     &            (CSEa(ie, nang, 3, 0), nang = 1, NDANG)
         ENDDO
C--------light ions
         IF(NDEJC.EQ.4 .AND. NEMc.GT.0)THEN
            nspec = INT((EMAx(1) - Q(NDEJC,1))/DE) + 2
            IF(nspec.GT.NDECSE)nspec = NDECSE
            WRITE(12, *)' '
            WRITE(12, 
     &'(''  Spectrum of  '',I1,''-'',A2,4X,                          ''(
     &n,x)'')')INT(AEJc(NDEJC)), SYMbe(NDEJC), ' ZAP=', IZAejc(NDEJC)
            WRITE(12, '(30X,''A      n      g      l      e      s '')')
            WRITE(12, *)' '
            WRITE(12, '('' Energy   '',8G15.5,/,(10X,8G15.5))')ANGles
            DO ie = 1, nspec-1
               WRITE(12, '(F9.4,8E15.5,/,(9X,8E15.5))')FLOAT(ie - 1)*DE,
     &               (CSEa(ie, nang, NDEJC, 0), nang = 1, NDANG)
            ENDDO
            DO ie = nspec, nspec+1 ! exact endpoint
               WRITE(12, '(F9.4,8E15.5,/,(9X,8E15.5))') EMAx(1) - 
     &         Q(NDEJC,1), (CSEa(ie, nang, NDEJC, 0), nang = 1, NDANG)
            ENDDO
         ENDIF
      ENDIF
C-----end of ENDF spectra (inclusive)
      IF(ENDf.GT.4) THEN
         WRITE(6,*) '----------------------------------------------'
         WRITE(6,*) 'Test - integrals of portions of DDX spectra'
         WRITE(6,'('' Energy'',12x,''neutron'',10x,''proton'')')
         WRITE(6,*) '----------------------------------------------'
         DO ispec=1,NEX(1)+10
            controln = 0
            controlp = 0
            DO nnuc = 1,NNUCD
               controln = controln + POPcseaf(0,1,ispec,nnuc)
               controlp = controlp + POPcseaf(0,2,ispec,nnuc)
            ENDDO
            WRITE(6,'(3g15.5)')(ispec-1)*DE,
     &              controln, controlp
         ENDDO
      ENDIF 
      READ(5, *)EIN
      IF(EIN.LT.0.0D0)THEN
         IF(FILevel)CLOSE(14)
         WRITE(12, *)' '
         WRITE(6, *)' '
         WRITE(6, *)'CALCULATIONS COMPLETED SUCCESSFULLY'
      CLOSE(15,status='delete')
      CLOSE(16,status='delete')
      CLOSE(66,status='delete')
         STOP 'REGULAR STOP'
      ENDIF
      FIRst_ein = .FALSE.
      GOTO 1400
99012 FORMAT(I12, F10.4, I5, F8.1, G15.6, I3,
     &       7(I4, F7.4), :/, (53X, 7(I4, F7.4)))
99013 FORMAT(1X, F5.2, 12G10.3)
99014 FORMAT(I12, F10.4, I5, F8.1, G15.6, I3, 
     &       7(I4, E11.4), :/, (53X, 7(I4, E11.4)))
      END
C
C
C
      SUBROUTINE RECOIL(Ke, Nnuc)
C
C----
C----Construct recoil spectra:
C----Each excitation bin is given its recoil spectrum, when a particle
C----is emitted its recoil velocity is vector added to the parent recoil
C----velocity and a resulting spectrum is summed upon daughter recoil
C----spectrum corresponding to the populated energy bin in the
C----daughter (kinematical normalization taken into account).
C----Daughter recoil spectra will be distributed between
C----adjacent bins (inversly proportional to the
C----distance of the actual energy to the bin energy
C----in order to conserve energy).
C----Requires that continuum spectra from each bin are stored on the
C----AUSpec array and those to discrete levels on the REClev for each
C----reaction mechanism considered in the calculations.
C----
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
C-----normalize recoil spectrum of the parent
      sumnor = 0.0
      DO ire = 1, NDEREC
         sumnor = sumnor + RECcse(ire, Ke, Nnuc)
      ENDDO
      IF(sumnor.NE.0.0D0)THEN
         DO ire = 1, NDEREC
            RECcse(ire, Ke, Nnuc) = RECcse(ire, Ke, Nnuc)/sumnor
         ENDDO
      ENDIF
      dang = 3.14159/FLOAT(NDANG - 1)
      coef = dang/DERec/2.0
      DO nejc = 1, NEJcm   !over ejectiles
C--------decay to continuum
         nnur = NREs(nejc)
C--------recorr is a recoil correction factor that
C--------divides outgoing energies
         recorr = AMAss(Nnuc)/EJMass(nejc)
         exqcut = EX(Ke, Nnuc) - Q(nejc, Nnuc) - ECUt(nnur)
         DO ie = 1, NDECSE !over ejec. energy (daughter excitation)
            icse = (exqcut - (ie - 1)*DE)/DE + 1.001
C           !daughter bin
            IF(icse.LE.0)GOTO 50
            erecejc = (ie - 1)*DE/recorr
            DO ire = 1, NDEREC          !over recoil spectrum
               erecpar = (ire - 1)*DERec
               DO na = 1, NDANG
                  erecoil = erecejc + erecpar
                  erecoil = erecoil + 2.0*SQRT(erecejc*erecpar)
     &                      *CANgler(na)
                  irec = erecoil/DERec + 1.001
                  weight = (erecoil - (irec - 1)*DERec)/DERec
                  IF(irec + 1.GT.NDEREC)GOTO 20
                  RECcse(irec, icse, nnur) = RECcse(irec, icse, nnur)
     &               + RECcse(ire, Ke, Nnuc)*AUSpec(ie, nejc)
     &               *(1.0 - weight)*SANgler(na)*coef
                  RECcse(irec + 1, icse, nnur)
     &               = RECcse(irec + 1, icse, nnur)
     &               + RECcse(ire, Ke, Nnuc)*AUSpec(ie, nejc)
     &               *weight*SANgler(na)*coef
               ENDDO                  !over angles
 20         ENDDO                  !over recoil spectrum
         ENDDO                  !over  daugther excitation
C--------decay to discrete levels (stored with icse=0)
 50      exqcut = exqcut + ECUt(nnur)
         DO il = 1, NLV(nnur)
            erecod = exqcut - ELV(il, nnur)  !emission energy
            erecod = erecod/recorr
            IF(erecod.LT.0)GOTO 100
            DO ire = 1, NDEREC      !over recoil spectrum
               DO na = 1, NDANG !over angles
                  erecoil = (ire - 1)*DERec + erecod
                  erecoil = erecoil + 2.0*SQRT((ire - 1)*DERec*erecod)
     &                      *CANgler(na)
                  irec = erecoil/DERec + 1.001
                  weight = (erecoil - (irec - 1)*DERec)/DERec
                  IF(irec.GT.NDEREC)GOTO 60
                  RECcse(irec, 0, nnur) = RECcse(irec, 0, nnur)
     &               + RECcse(ire, Ke, Nnuc)*REClev(il, nejc)
     &               *(1.0 - weight)*SANgler(na)*coef
                  IF(irec + 1.GT.NDEREC)GOTO 60
                  RECcse(irec + 1, 0, nnur) = RECcse(irec + 1, 0, nnur)
     &               + RECcse(ire, Ke, Nnuc)*REClev(il, nejc)
     &               *weight*SANgler(na)*coef
               ENDDO                  !over angles
 60         ENDDO                  !over recoil spectrum
         ENDDO                  !over levels
 100  ENDDO                  !over ejectiles
C-----
C-----parent recoil spectrum after gamma decay
C-----
      nnur = Nnuc
      nejc = 0
C-----gamma decay to continuum
      DO ie = 1, NDECSE !over ejec. energy (daughter excitation)
         icse = (EX(Ke, Nnuc) - (ie - 1)*DE - ECUt(nnur))/DE + 1.001
C        !daughter bin
         DO irec = 1, NDEREC         !over recoil spectrum
            IF(icse.GT.0)RECcse(irec, icse, nnur)
     &         = RECcse(irec, icse, nnur) + RECcse(irec, Ke, Nnuc)
     &         *AUSpec(ie, 0)/DERec
         ENDDO                  !over recoil spectrum
      ENDDO                  !over  daugther excitation
C-----gamma decay to discrete levels (stored with icse=0)
      DO il = 1, NLV(nnur)
         DO ire = 1, NDEREC             !over recoil spectrum
            RECcse(ire, 0, nnur) = RECcse(ire, 0, nnur)
     &                             + RECcse(ire, Ke, Nnuc)
     &                             *REClev(il, nejc)/DERec
         ENDDO                  !over recoil spectrum
      ENDDO                  !over levels
      END
      SUBROUTINE PRINT_RECOIL(nnuc,react)
C-----
C-----prints recoil spectrum of nnuc residue
C-----
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
      CHARACTER*21 react
      IF(CSPrd(nnuc).GT.0.0D0)THEN
C-------normalize recoil spectra to remove eventual inaccuracy
C-------due to numerical integration of angular distributions
C-------and find last non-zero cross section for printing
         corr = 0.0
         ilast = 0
         DO ie = 1, NDEREC
            corr = corr + RECcse(ie, 0, nnuc)
            IF(RECcse(ie, 0, nnuc).NE.0)ilast = ie
         ENDDO
         IF(corr.EQ.0) RETURN
c        WRITE(6,*)'nnuc, rec, cs',nnuc,corr*DERec,CSPrd(nnuc)
         corr = CSPrd(nnuc)/corr/DERec
         ilast = MIN(ilast + 1, NDEREC)
         DO ie = 1, ilast
            RECcse(ie, 0, nnuc) = RECcse(ie, 0, nnuc)*corr
         ENDDO
         WRITE(12, *)' '
         WRITE(12, '(A23,A7,A4,I6,A6,F12.5)')
     &         '  Spectrum of recoils  ',react,'ZAP=', IZA(nnuc),
     &         ' mass=', AMAss(nnuc)
         WRITE(12, *)' '
         WRITE(12, '('' Energy    mb/MeV'')')
         WRITE(12, *)' '
         DO ie = 1, ilast
            WRITE(12, '(F9.4,E15.5)')FLOAT(ie - 1)*DERec,
     &            RECcse(ie, 0, nnuc)
         ENDDO
C--------print end point again with 0 xs for consistency with particle spectra
         WRITE(12, '(F9.4,E15.5)')FLOAT(ilast - 1)*DERec,
     &            RECcse(ilast+1, 0, nnuc)
         IF(ABS(1.0 - corr).GT.0.01D0 .AND. CSPrd(nnuc).GT.0.001D0)THEN
            WRITE(6, *)' '
            WRITE(6, *)'WARNING:  Ein = ', EIN, ' MeV ZAP = ', IZA(nnuc)
     &                 ,' from ',react
            WRITE(6, *)'WARNING: x-section balance in recoils '
            WRITE(6, *)'WARNING: difference = ', (1.0 - corr)
     &                 *100.0, '%'
            WRITE(6,*)'WARNING: production cross section = ',CSPrd(nnuc)
            WRITE(6, *)' '
         ENDIF
      ENDIF
      RETURN
      END
