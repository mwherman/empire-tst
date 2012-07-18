Ccc   * $Rev: 2958 $
Ccc   * $Author: rcapote $
Ccc   * $Date: 2012-07-19 00:59:03 +0200 (Do, 19 Jul 2012) $

C
      SUBROUTINE MARENG(Npro,Ntrg)
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:ppu*
Ccc   *                         M A R E N G                              *
Ccc   *                                                                  *
Ccc   * Calculates initial compound nucleus population after projectile  *
Ccc   * absorption  using transmission coefficients obtained from        *
Ccc   * the optical or the distributed barrier  model.                   *
Ccc   *                                                                  *
Ccc   * input:NPRO - projectile index (normally 0)                       *
Ccc   *       NTRG - target index (normally 0)                           *
Ccc   *                                                                  *
Ccc   * output:none                                                      *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C     COMMON variables
C
      DOUBLE PRECISION ABScs, ELAcs, ELTl(NDLW)
      DOUBLE PRECISION S1, SINl, TOTcs, SINlcc, SINlcont
      COMMON /ECISXS/ ELAcs, TOTcs, ABScs, SINl, SINlcc, SINlcont
      COMMON /ELASTIC/ ELTl
      COMMON /WAN   / S1
C
C Dummy arguments
C
      INTEGER Npro, Ntrg
C
C Local variables
C
      DOUBLE PRECISION ak2, chsp, cnj, coef, csmax, csvalue, 
     &                 e1tmp, ecms, einlab, el, ener, p1, parcnj,
     &                 qdtmp, r2, rp, s0, s1a, smax, smin, stl(NDLW),
     &                 sum, wparg, xmas_npro, sel(NDLW), xmas_ntrg
      CHARACTER*3 ctldir
      CHARACTER*132 ctmp
      CHARACTER*23 ctmp23
      DOUBLE PRECISION DMAX1
      LOGICAL dodwba, fexist, ldbwacalc, ltlj, relcal, lodd
      DOUBLE PRECISION E1, E2, SIGQD, XM1
      REAL FLOAT, SNGL
      INTEGER i, ichsp, ip, itmp1, j, k, l, lmax, lmin, maxlw, mul,
     &        nang, itmp2 
      INTEGER IDNINT, INT, MIN0
      INTEGER*4 iwin
      DOUBLE PRECISION PAR
      INTEGER*4 PIPE
      CHARACTER*120 rstring
      DATA ctldir/'TL/'/
      PAR(i,ipa,l) = 0.5*(1.0 - ( - 1.0)**i*ipa*( - 1.0)**l)
C
C-----Zero qd fraction of photabsorption before it can do any damage
C
      QDFrac = 0.0D0
C
C-----No DWBA by default
C
      ldbwacalc = .FALSE.
C
C-----Reduced mass corrected for proper mass values
      xmas_npro = EJMass(Npro) 
	xmas_ntrg = AMAss(Ntrg)

      el = EINl
      ecms = EIN
      S1 = 0.5
      IF (AINT(XJLv(LEVtarg,Ntrg) + SEJc(Npro)) - XJLv(LEVtarg,Ntrg)
     &    - SEJc(Npro).EQ.0.0D0) S1 = 1.0
      ELAcs = 0.D0
      TOTcs = 0.D0
      ABScs = 0.D0
      SINl = 0.D0
      SINlcont = 0.D0
      csmax = 0.0
      CSFus = 0.0
      maxlw = 0
      DO i = 1, NDLW
         stl(i) = 0.d0
         sel(i) = 0.d0
      ENDDO
      WRITE (ctmp23,'(i3.3,i3.3,1h_,i3.3,i3.3,1h_,i9.9)')
     &       INT(ZEJc(Npro)), INT(AEJc(Npro)), INT(Z(Ntrg)),
     &       INT(A(Ntrg)), INT(EINl*1000000)

C-----This part prompts for the name of a data file. The INQUIRE
C-----statement then determines whether or not the file exists.
C-----If it does not, the program calculates new transmission coeff.
      INQUIRE (FILE = (ctldir//ctmp23//'.INC'),EXIST = fexist)
C  
      IF (fexist .and. .not.CALctl) THEN
C--------Here the old calculated files are read
         OPEN (45,FILE = (ctldir//ctmp23//'.INC'),
     &         FORM = 'UNFORMATTED',ERR = 50)
         IF (IOUt.EQ.5) OPEN (46,FILE = ctldir//ctmp23//'_INC.LST')
         READ (45,END = 50) lmax, ener, IRElat(Npro,Ntrg)
         IF (IOUt.EQ.5) WRITE (46,'(A5,I6,E12.6)') 'LMAX:', lmax, ener

         IF (ABS(ener - EINl).LT.1.d-6 .AND. FITomp.EQ.0) THEN
            maxlw = lmax
            DO l = 0, maxlw
               READ (45,END = 50) stl(l + 1)
               IF (IOUt.EQ.5) WRITE (46,*) l, SNGL(stl(l + 1))
            ENDDO
            READ (45,END = 50) ELAcs, TOTcs, ABScs, SINl, SINlcc, CSFus
            SINlcont = max(ABScs - (SINl + SINlcc + CSFus),0.d0)
            IF (IOUt.EQ.5) WRITE (46,'(1x,A21,6(e12.6,1x))')
     &                  'EL,TOT,ABS,INEL,CC;CSFus XSs:', ELAcs, TOTcs,
     &                    ABScs, SINl, SINlcc, CSFus
            READ (45,END = 300) l
            IF(L.EQ.123456) THEN
              IF (IOUt.EQ.5) WRITE (46,*) L
              DO l = 0, maxlw
                READ (45,END = 300) sel(l + 1)
                IF (IOUt.EQ.5) WRITE (46,*) l, SNGL(sel(l + 1))
              ENDDO
            ENDIF
            CLOSE (45)
            IF (IOUt.EQ.5) CLOSE (46)
            IF (IOUt.GT.1) THEN
               WRITE (8,*)
     &' Transmission coefficients for incident channel read from file: '
               WRITE (8,*) ' ', ctldir//ctmp23//'.INC'
               WRITE (8,*)
            ENDIF
            WRITE(8,*) ' Maximum CN spin is ', maxlw
            WRITE(8,*) ' Spin dimension  is ', NDLW
            NLW = NDLW
            WRITE(8,*) 
            GOTO 300
         ENDIF
C
C-------If (energy read from file do not coincide
C-------this nucleus should be recalculated (goto 300)
C
   50    CLOSE (45,STATUS = 'DELETE')
         IF (FITomp.EQ.0) THEN
         WRITE (8,*) 'WARNING: ENERGY MISMATCH:  Elab =', EINl,
     &               ' REQUESTED ENERGY=', SNGL(ener)
         WRITE (8,*) 'WARNING: FILE WITH TRANSM. COEFF.',
     &               ' FOR INC.CHANNEL HAS BEEN DELETED'
          ENDIF
         IF (IOUt.EQ.5) CLOSE (46,STATUS = 'DELETE')
      ENDIF
C-----Calculation of fusion cross section for photon induced reactions
      IF (INT(AEJc(Npro)).EQ.0) THEN
         IF (SDRead) THEN
C-----------Reading of spin distribution from file SDFILE
C           If you have file "SDFILE" -> it is possible to use
C           input spin distribution
C           Format of the file:
C           rows with sequentialy  cnJ, ParcnJ, csvalue
C           where  cnJ     - spin of nucleus,
C                  ParcnJ  - parity
C                  csvalue - cross-section
C
C           limits: cnJ=n*0.5, where n=0,1,2...
C                   A of c.n. /2 = integer --> cnJ=n*1, where n=0,1,2...
C                   A of c.n. /2 = integer+1/2 --> cnJ=n*1/2, where n=0,1,2...
C                   ParcnJ=-1 or 1
C                   csvalue = value in mb
C           Reading no more than 2*NDLW rows
            WRITE (8,*)
     &' Spin distribution of fusion cross section read from SDREAD file'
            WRITE (8,*)
     &          ' (all previous instructions concerning fusion ignored)'
            DO i = 1, 2*NDLW
               READ (43,*,END = 60) cnj, parcnj, csvalue
C--------------Spin of c.n. cnJ=j-S1 => j=cnJ+S1
               IF (2*cnj - DINT(2*cnj).NE.0.00)
     &              STOP 'cnJ!=n*1/2, n=0,+-1...  in SDREAD file'
               j = IDNINT(cnj + S1)
               IF (parcnj.EQ.1.) ip = 1
               IF (parcnj.EQ. - 1.) ip = 2
               IF (parcnj.NE.1 .AND. parcnj.NE. - 1)
     &              STOP 'ParcnJ!=+-1 in SDREAD file'
               POP(NEX(1),j,ip,1) = csvalue
               CSFus = CSFus + POP(NEX(1),j,ip,1)
               csmax = DMAX1(POP(NEX(1),j,ip,1),csmax)
            ENDDO
   60       ABScs=CSFus
            SINlcc=0.d0
            SINl  =0.d0
            SINlcont =0.d0
            NLW = i - 1 ! RCN Aug 2008
C--------END of spin distribution from file SDFILE
         ELSE
            JSTab(1) = NDLW
                          !stability limit not a problem for photoreactions
            IF (EIN.LE.ELV(NLV(Ntrg),Ntrg)) THEN
               WRITE (8,*) 'WARNING: '
               WRITE (8,*) 'WARNING: ECN=', EIN, ' Elev=',
     &                     ELV(NLV(Ntrg),Ntrg)
               WRITE (8,*)
     &                   'WARNING: CN excitation energy below continuum'
               WRITE (8,*)
     &                   'WARNING: cut-off. zero reaction cross section'
               WRITE (8,*) 'WARNING: will result'
               WRITE (8,*) 'WARNING: '
            ENDIF
C-----------E1
            IF (IGE1.NE.0) THEN
C-----------factor 10 near HHBarc from fm**2-->mb
               e1tmp = 10*HHBarc**2*PI*E1(Ntrg,EINl,0.D0,0.D0)
     &                 /(2*EINl**2)
               qdtmp = SIGQD(Z(Ntrg),A(Ntrg),EINl,LQDfac)
               e1tmp = (e1tmp + qdtmp/3.0D0)/(2*XJLv(LEVtarg,Ntrg) + 1)
C--------------do loop over parity
               DO ip = 1, 2
C-----------------Quasideuteron contribution QDTmp by Carlson
                  wparg = PAR(ip,LVP(LEVtarg,Ntrg),1)*e1tmp
C-----------------do loop over compound nucleus spin
                  DO j = 1, NDLW
C--------------------Spin of c.n. J=j-S1
                     IF (ABS(j - S1 - XJLv(LEVtarg,Ntrg)).LE.1.0 .AND.
     &                   (j - S1 + XJLv(LEVtarg,Ntrg)).GE.1.0)
     &                   POP(NEX(1),j,ip,1) = POP(NEX(1),j,ip,1)
     &                   + (FLOAT(2*j + 1) - 2.0*S1)*wparg
                  ENDDO
               ENDDO
            ENDIF
C-----------end of E1
C
C-----------M1
            IF (IGM1.NE.0) THEN
C--------------factor 10 near HHBarc from fm**2-->mb
               e1tmp = 10*HHBarc**2*PI*XM1(EINl)/(2*EINl**2)
     &                 /(2*XJLv(LEVtarg,Ntrg) + 1)
C--------------do loop over parity
               DO ip = 1, 2
                  wparg = PAR(ip,LVP(LEVtarg,Ntrg),2)*e1tmp
C-----------------do loop over compound nucleus spin
                  DO j = 1, NDLW
C--------------------Spin of c.n. J=j-S1
                     IF (ABS(j - S1 - XJLv(LEVtarg,Ntrg)).LE.1.0 .AND.
     &                   (j - S1 + XJLv(LEVtarg,Ntrg)).GE.1.0)
     &                   POP(NEX(1),j,ip,1) = POP(NEX(1),j,ip,1)
     &                   + (FLOAT(2*j + 1) - 2.0*S1)*wparg
                  ENDDO
               ENDDO
            ENDIF
C-----------end of M1
C
C-----------E2
            IF (IGE2.NE.0) THEN
C-----------factor 10 near HHBarc from fm**2-->mb
               e1tmp = 10*HHBarc**2*PI*E2(EINl)/(2*EINl**2)
     &                 /(2*XJLv(LEVtarg,Ntrg) + 1)
C--------------do loop over parity
               DO ip = 1, 2
                  wparg = PAR(ip,LVP(LEVtarg,Ntrg),2)*e1tmp
C-----------------do loop over compound nucleus spin
                  DO j = 1, NDLW
C-----------------Spin of c.n. J=j-S1
C-----------------factor 10 near HHBarc from fm**2-->mb
                     IF (ABS(j - S1 - XJLv(LEVtarg,Ntrg)).LE.2.0 .AND.
     &                   (j - S1 + XJLv(LEVtarg,Ntrg)).GE.2.0)
     &                   POP(NEX(1),j,ip,1) = POP(NEX(1),j,ip,1)
     &                   + (FLOAT(2*j + 1) - 2.0*S1)*wparg
                  ENDDO
               ENDDO
            ENDIF
C-----------end of E2
            DO ip = 1, 2
               DO j = 1, NDLW
                  CSFus = CSFus + POP(NEX(1),j,ip,1)
                  csmax = DMAX1(POP(NEX(1),j,ip,1),csmax)
               ENDDO
            ENDDO
            IF (IGE1.NE.0 .AND. CSFus.GT.0.D0) QDFrac = qdtmp/CSFus
 
            ABScs=CSFus
            SINlcc=0.d0
            SINl  =0.d0
            SINlcont =0.d0
  
         ENDIF
         NLW = NDLW
C--------END of calculation of fusion cross section
C--------for photon induced reactions
  100    RETURN
      ENDIF
      IF (FUSread) THEN
C--------if FUSREAD true read l distribution of fusion cross section
C--------and calculate transmission coefficients
         el = EINl
         CALL KINEMA(el,ecms,xmas_npro,xmas_ntrg,ak2,1,RELkin)
         coef=1.d0
         IF (INT(AEJc(0)).GT.0) coef = 10.*PI/ak2/
     &      (2*XJLv(LEVtarg,Ntrg) + 1.0)/(2*SEJc(Npro) + 1.0)

         CSFus = 0.d0
         DO il = 1, NDLW
            READ (11,*,END = 150) csvalue
            stl(il) = csvalue
            CSFus = CSFus + csvalue*(2*(il-1)+1)*coef
            IF (stl(il).GT.1.0D0) THEN
               WRITE (8,
     &'('' ERROR: INPUT FUSION TRANSMISSION > 1'', '' FOR l='',I3)') 
     & il - 1
               WRITE (8,*) ' FATAL: EXECUTION STOPPED!!!'
               STOP
            ENDIF
         ENDDO

  150    NLW = il - 1
         maxlw = min(NLW,NDLW-1)
         ABScs = CSFus
         SINlcc=0.d0
         SINl=0.d0
         SINlcont =0.d0
         WRITE (8,*)
     &  ' Spin distribution of fusion cross section read from the file '
         WRITE (8,*)
     &          ' (all previous instructions concerning fusion ignored)'
         WRITE (8,*) 'Maximum angular momentum :',maxlw
         WRITE (8,*) 'Fusion cros section      :',CSFus
         WRITE (8,*) 

C--------calculation of o.m. transmission coefficients for absorption
      ELSEIF (KTRlom(Npro,Ntrg).GT.0) THEN
         einlab = -EINl
         IWArn = 0
         ldbwacalc = .FALSE.

         CCCalc = .FALSE.
         ltlj = .FALSE.
         lodd = .false.
         IF( (mod(nint(Z(Ntrg)),2).ne.0 .or. 
     >        mod(nint(A(Ntrg)-Z(Ntrg)),2).ne.0) .and.
     >        mod(nint(A(Ntrg)),2).ne.0 ) lodd = .true.  
         dodwba = .FALSE.
         DO l = 1, NDCOLLEV
            IF (ICOllev(l).LT.LEVcc) CYCLE ! Skipping coupled levels
            dodwba = .TRUE.
         ENDDO
         IF (DIRect.EQ.3) dodwba = .TRUE.
         IF (dodwba .AND. DIRect.GT.0 ) THEN
            IF (DIRect.EQ.1 .OR. DIRect.EQ.3) THEN
C--------------Saving KTRlom(0,0)
               itmp1 = KTRlom(0,0)
               KTRlom(0,0) = KTRompcc
               CCCalc = .TRUE.
            ENDIF
C-----------DWBA calculation. All collective levels considered
            IF (DIRect.EQ.3) THEN
               WRITE (8,*) ' DWBA calculations for inelastic scattering'
            ELSE
               WRITE (8,*) ' DWBA calculations for inelastic scattering'
               WRITE (8,*) '    to uncoupled coll. levels and continuum'
            ENDIF

            CALL ECIS_CCVIB(Npro,Ntrg,einlab,.TRUE.,1)

            IF (DIRect.NE.3) THEN
               CALL PROCESS_ECIS(IOPsys,'dwba',4,4,ICAlangs)
            ELSE
               CALL PROCESS_ECIS(IOPsys,'INCIDENT',8,4,ICAlangs)
               CALL ECIS2EMPIRE_TL_TRG(Npro,Ntrg,maxlw,stl,sel,.TRUE.)
               ltlj = .TRUE.
                            ! TLs are obtained here for DIRECT=3
               WRITE (8,*) ' SOMP transmission coefficients used for ',
     &                     'fusion determination'
            ENDIF
            IF (DIRect.EQ.1 .OR. DIRect.EQ.3) THEN
C--------------Restoring KTRlom(0,0)
               KTRlom(0,0) = itmp1
               CCCalc = .FALSE.
            ENDIF
            ldbwacalc = .TRUE.
         ENDIF
C
C---------In EMPIRE code the options DIRECT=1 and DIRECT=2 produces exactly
C---------the same array of transmission coefficients calculated
C---------by CC OMP. The differences between DIRECT 1/2 options are in the
C---------calculations of the outgoing channel (TRANSINP() routine)
C---------DIRECT 2 option uses CC method to produce outgoing TLs
C---------for the inelastic channel. DIRECT 1 option assumes SOMP
C---------with only one level (the GS) to calculate the inelastic TLs.
C
         IF ((DIRect.EQ.1 .OR. DIRect.EQ.2) .AND. AEJc(Npro).LE.1) THEN
            WRITE (8,*) ' CC transmission coefficients used for ',
     &                  'fusion determination'
            einlab = -EINl
            IF (DIRect.EQ.1) THEN
C--------------Saving KTRlom(0,0)
               itmp1 = KTRlom(0,0)
               KTRlom(0,0) = KTRompcc
               CCCalc = .TRUE.
            ENDIF
C-----------Transmission coefficient matrix for incident channel
C-----------is calculated by CC method.

            IF (SOFt) THEN
C-------------EXACT SOFT ROTOR MODEL CC calc. by OPTMAN (only coupled levels)
              CALL OPTMAN_CCSOFTROT(Npro,Ntrg,einlab) 
C             CALL ECIS_CCVIB(Npro,Ntrg,einlab,.FALSE., - 1)
              IF (ldbwacalc) THEN
                CALL PROCESS_ECIS(IOPsys,'ccm',3,4,ICAlangs)
              ELSE
                CALL PROCESS_ECIS(IOPsys,'INCIDENT',8,4,ICAlangs)
                CALL ECIS2EMPIRE_TL_TRG(Npro,Ntrg,maxlw,stl,sel,.TRUE.)
              ENDIF
            ELSE

              IF (DEFormed) THEN
C---------------EXACT ROTATIONAL MODEL CC calc. (only coupled levels)
                CALL ECIS_CCVIBROT(Npro,Ntrg,einlab,0)
                IF (ldbwacalc) THEN
                  CALL PROCESS_ECIS(IOPsys,'ccm',3,4,ICAlangs)
                ELSE
                  CALL PROCESS_ECIS(IOPsys,'INCIDENT',8,4,ICAlangs)
                  CALL ECIS2EMPIRE_TL_TRG(
     >                                 Npro,Ntrg,maxlw,stl,sel,.FALSE.)
                ENDIF
              ELSE
C---------------EXACT VIBRATIONAL MODEL CC calc. (only coupled levels)
                CALL ECIS_CCVIB(Npro,Ntrg,einlab,.FALSE., - 1)
                IF (ldbwacalc) THEN
                  CALL PROCESS_ECIS(IOPsys,'ccm',3,4,ICAlangs)
                ELSE
                  CALL PROCESS_ECIS(IOPsys,'INCIDENT',8,4,ICAlangs)
                  CALL ECIS2EMPIRE_TL_TRG(
     >                                  Npro,Ntrg,maxlw,stl,sel,.TRUE.)
                ENDIF
              ENDIF
            ENDIF
            IF (DIRect.EQ.1) THEN
C--------------Restoring KTRlom(0,0)
               KTRlom(0,0) = itmp1
               CCCalc = .FALSE.
            ENDIF
            ltlj = .TRUE.
            IF (ldbwacalc) THEN
C
C-------------Joining DWBA and CCM files
C-------------Total, elastic and reaction cross section is from CCM
C
               IF (IOPsys.EQ.0) THEN
C-----------------LINUX
                  ctmp = 'cp ccm.CS INCIDENT.CS'
                  iwin = PIPE(ctmp)
                  ctmp = 'cp ccm.TLJ INCIDENT.TLJ'
                  iwin = PIPE(ctmp)
C                 Only Legendre elastic expansion is needed
                  ctmp = 'cp ccm.LEG INCIDENT.LEG'
                  iwin = PIPE(ctmp)
               ELSE
C-----------------WINDOWS
                  ctmp = 'copy ccm.CS INCIDENT.CS >NUL'
                  iwin = PIPE(ctmp)
                  ctmp = 'copy ccm.TLJ INCIDENT.TLJ >NUL'
                  iwin = PIPE(ctmp)
C                 Only Legendre elastic expansion is needed
                  ctmp = 'copy ccm.LEG INCIDENT.LEG >NUL'
                  iwin = PIPE(ctmp)
               ENDIF
C--------------Inelastic cross section (incident.ics)
               OPEN (45,FILE = 'dwba.ICS',STATUS = 'OLD',ERR = 220)
               OPEN (46,FILE = 'ccm.ICS'   ,STATUS = 'OLD',ERR = 220)
               OPEN (47,FILE = 'INCIDENT.ICS',STATUS = 'UNKNOWN')
               READ (45,'(A80)',END = 220,ERR = 220) rstring
               READ (46,'(A80)',END = 210,ERR = 210) ! first line is taken from dwba
  210          WRITE (47,'(A80)') rstring
               DO i = 2, ND_nlv
                  READ (45,'(A80)',END = 220,ERR = 220) rstring
                  READ (46,'(A80)',END = 215,ERR = 215) rstring
  215             WRITE (47,'(A80)') rstring
               ENDDO
  220          CLOSE (45,STATUS = 'DELETE')
               CLOSE (46,STATUS = 'DELETE')
               CLOSE (47)
C--------------Angular distribution (incident.ang)
               OPEN (45,FILE = 'dwba.ANG',STATUS = 'OLD',ERR = 240)
               READ (45,'(A80)',ERR = 240, END = 240) rstring
               OPEN (46,FILE = 'ccm.ANG' ,STATUS = 'OLD',ERR = 240)
               READ (46,'(A80)',ERR = 230, END = 230) ! first line is taken from dwba
  230          OPEN (47,FILE = 'INCIDENT.ANG',STATUS = 'UNKNOWN')

               WRITE (47,'(A80)') rstring
               DO i = 1, ND_nlv
C-----------------checking the correspondence of the excited states
                  READ (45,'(i5,6x,i4,i5)',END = 240,ERR = 240) 
     &                 istat1, itmp2, nang
                  READ (46,'(i5,6x,i4)',END = 235,ERR = 235) 
     &                 istat2
C-----------------checking the correspondence of the excited states for even-even targets
                  IF ( .not.lodd .AND. istat1.NE.istat2 ) THEN   
                    WRITE (8,*)
     &            ' WARNING: DWBA and CCM state order do not coincide'
                     STOP
     &            ' WARNING: DWBA and CCM state order do not coincide'
                  ENDIF
                  BACKSPACE (46)
                  READ (46,'(A80)',ERR = 235, END = 235) rstring
                  GOTO 2351 
  235             BACKSPACE (45)
                  READ (45,'(A80)',ERR = 240, END = 240) rstring
 2351             WRITE (47,'(A80)') rstring
                  DO j = 1, itmp2*nang ! ecis06
                     READ (45,'(A80)',ERR = 240,END = 240) rstring
                     READ (46,'(A80)',ERR = 236,END = 236) rstring
  236                WRITE (47,'(A80)') rstring
                  ENDDO
               ENDDO
  240          CLOSE (45) ! ,STATUS = 'DELETE')
               CLOSE (46) ! ,STATUS = 'DELETE')
               CLOSE (47)
C--------------Experimental angular distribution (incident.ang)
               IF( ICAlangs.GT.0) THEN   ! To be updated for ecis06 
                 OPEN (45,FILE = 'dwba.EXP',STATUS = 'OLD',ERR = 260)
                 READ (45,'(A80)',END = 260) rstring
                 OPEN (46,FILE = 'ccm.EXP',STATUS = 'OLD',ERR = 260)
                 READ (46,'(A80)',END = 250) ! first line is taken from dwba
  250            OPEN (47,FILE = 'INCIDENT.EXP',STATUS = 'UNKNOWN')
                 WRITE (47,'(A80)') rstring
                 DO i = 1, ICAlangs
                  READ (45,'(i5,5x,i5)',END = 260) istat1, nang
                  READ (46,'(i5)',END = 255) istat2
C-----------------checking the correspondence of the excited states
                  IF (istat1.NE.istat2) THEN
                     WRITE (8,*)
     &        ' WARNING: Exptl DWBA and CCM state order do not coincide'
                     STOP
     &        ' WARNING: Exptl DWBA and CCM state order do not coincide'
                  ENDIF
  255             BACKSPACE (45)
                  READ (45,'(A80)',END = 260) rstring
                  WRITE (47,'(A80)') rstring
                  DO j = 1, nang
                     READ (45,'(A80)',END = 260) rstring
                     READ (46,'(A80)',END = 256) rstring
  256                WRITE (47,'(A80)') rstring
                  ENDDO
                 ENDDO
                ENDIF
  260          CLOSE (45,STATUS = 'DELETE')
               CLOSE (46,STATUS = 'DELETE')
               CLOSE (47)
               IF (DEFormed) THEN
                CALL ECIS2EMPIRE_TL_TRG(Npro,Ntrg,maxlw,stl,sel,.FALSE.)
               ELSE
                CALL ECIS2EMPIRE_TL_TRG(Npro,Ntrg,maxlw,stl,sel,.TRUE.)
               ENDIF
            ENDIF  ! END of LDWBA (DWBA and CCM joining process)
         ENDIF  ! END of DIRECT=1/2 block
         IF (.NOT.ltlj) THEN
C-----------Transmission coefficient matrix for incident channel
C-----------is calculated like in SOMP i.e.
C-----------SCAT2 like calculation (one state, usually gs, alone)
            CALL ECIS_CCVIB(Npro,Ntrg,einlab,.TRUE.,0)
            CALL PROCESS_ECIS(IOPsys,'INCIDENT',8,3,ICAlangs)
            WRITE (8,*) ' SOMP transmission coefficients used for ',
     &                  'fusion determination'
            CALL ECIS2EMPIRE_TL_TRG(Npro,Ntrg,maxlw,stl,sel,.TRUE.)
         ENDIF
         IF (maxlw.GT.NDLW) THEN
            WRITE (8,*)
     &            ' ERROR: INSUFFICIENT NUMBER OF PARTIAL WAVES ALLOWED'
            WRITE (8,*) ' ERROR: INCREASE NDLW IN dimension.h UP TO',
     &                  maxlw + 1
            WRITE (8,*) ' ERROR: AND RECOMPILE THE CODE'
            STOP ' FATAL: INSUFFICIENT NUMBER OF PARTIAL WAVES ALLOWED'
         ENDIF
         WRITE(8,*) ' Maximum CN spin is ', maxlw
         WRITE(8,*) ' Spin dimension  is ', NDLW
         NLW = NDLW
         WRITE(8,*) 
C--------IWARN=0 - 'NO Warnings'
C--------IWARN=1 - 'A out of the recommended range '
C--------IWARN=2 - 'Z out of the recommended range '
C--------IWARN=3 - 'Energy requested lower than recommended for this potential'
C--------IWARN=4 - 'Energy requested higher than recommended for this potential'
         IF ((IWArn.EQ.1 .or. IWArn.EQ.2) .AND. FIRst_ein
     &       .AND. IOUt.GE.5) WRITE (8,*) ' WARNING: ',KTRlom(0,0),
     &   ' OMP not recommended for target Z,A=', Z(Ntrg),'-',(Ntrg)
         IF ((IWArn.EQ.3 .OR. IWArn.EQ.4) .AND. IOUt.GE.5) WRITE (8,*)
     &      ' WARNING: ',KTRlom(0,0),' OMP not recommended for E=', EINl
         IWArn = 0
      ELSEIF (KTRlom(Npro,Ntrg).EQ.0) THEN
C--------calculation of h.i. transmission coefficients for fusion
         maxlw = NDLW-1
         CALL HITL(stl)
         if(NLW.GT.0) maxlw = min(NDLW-1,maxlw)
         WRITE(8,*) 
         WRITE(8,*) ' Maximum CN spin is ', maxlw + 1
         WRITE(8,*) ' Spin dimension  is ', NDLW
         NLW = NDLW
         WRITE(8,*) 
C--------channel spin min and max
         el = EINl
         CALL KINEMA(el,ecms,xmas_npro,xmas_ntrg,ak2,1,RELkin)
         IF (INT(AEJc(0)).GT.0) coef = 10.*PI/ak2/
     &      (2*XJLv(LEVtarg,Ntrg) + 1.0)/(2*SEJc(Npro) + 1.0)
         smin = ABS(SEJc(Npro) - XJLv(LEVtarg,Ntrg))
         smax = SEJc(Npro) + XJLv(LEVtarg,Ntrg)
         mul = smax - smin + 1.0001
         CSFus = 0.0
         DO ip = 1, 2     ! over parity
          DO j = 1, NLW !over compound nucleus spin
            sum = 0.0
            DO ichsp = 1, mul
               chsp = smin + FLOAT(ichsp - 1)
               lmin = ABS(j - chsp - S1) + 0.0001
               lmax = j + chsp - S1 + 0.0001
               lmin = lmin + 1
               lmax = lmax + 1
               lmax = MIN0(NDLW,lmax)
               lmax = MIN0(maxlw,lmax)
               DO k = lmin, lmax
                  sum = sum + PAR(ip,LVP(LEVtarg,Ntrg),k - 1)*stl(k)
     &                  *DRTl(k)
               ENDDO
            ENDDO
            POP(NEX(1),j,ip,1) = coef*sum*(FLOAT(2*j + 1) - 2.0*S1)
     &                           *FUSred
            CSFus = CSFus + POP(NEX(1),j,ip,1)
            csmax = DMAX1(POP(NEX(1),j,ip,1),csmax)
          ENDDO
         ENDDO
         ABScs = CSFus
         SINlcc = 0.d0
         SINl = 0.d0
         SINlcont =0.d0
      ENDIF ! END of FUSREAD block
C
C-----Moving incident channel results to TL/ directory
C
      IF (KTRlom(Npro,Ntrg).GT.0) THEN
        IF (IOPsys.EQ.0) THEN
C--------LINUX
         ctmp = 'mv INCIDENT.CS '//ctldir//ctmp23//'.CS'
         iwin = PIPE(ctmp)
         IF (DIRect.GT.0) THEN
           ctmp = 'mv INCIDENT.ICS '//ctldir//ctmp23//'.ICS'
           iwin = PIPE(ctmp)
         ENDIF
         IF (ICAlangs.GT.0) THEN
           ctmp = 'mv INCIDENT.EXP '//ctldir//ctmp23//'.EXP'
           iwin = PIPE(ctmp)
         ENDIF
         ctmp = 'mv INCIDENT.ANG '//ctldir//ctmp23//'.ANG'
         iwin = PIPE(ctmp)
         ctmp = 'mv INCIDENT.LEG '//ctldir//ctmp23//'.LEG'
         iwin = PIPE(ctmp)
         ctmp = 'mv INCIDENT.TLJ '//ctldir//ctmp23//'.TLJ'
         iwin = PIPE(ctmp)
        ELSE
C--------WINDOWS
         ctmp = 'move INCIDENT.CS '//ctldir//ctmp23//'.CS >NUL'
         iwin = PIPE(ctmp)
         IF (DIRect.GT.0) THEN
           ctmp = 'move INCIDENT.ICS '//ctldir//ctmp23//'.ICS >NUL'
           iwin = PIPE(ctmp)
         ENDIF
         IF (ICAlangs.GT.0) THEN
           ctmp = 'mv INCIDENT.EXP '//ctldir//ctmp23//'.EXP >NUL'
           iwin = PIPE(ctmp)
         ENDIF
         ctmp = 'move INCIDENT.ANG '//ctldir//ctmp23//'.ANG >NUL'
         iwin = PIPE(ctmp)
         ctmp = 'move INCIDENT.LEG '//ctldir//ctmp23//'.LEG >NUL'
         iwin = PIPE(ctmp)
         ctmp = 'move INCIDENT.TLJ '//ctldir//ctmp23//'.TLJ >NUL'
         iwin = PIPE(ctmp)
        ENDIF
      ENDIF
C
C-----Save TLs, SINl
C
C-----Storing transmission coefficients for the incident channel
      IF (IOUt.EQ.5) THEN
         OPEN (46,FILE = ctldir//ctmp23//'_INC.LST')
         WRITE (46,'(A5,I6,E12.6)') 'LMAX:', maxlw, EINl
         DO l = 0, maxlw
            WRITE (46,*) l, SNGL(stl(l + 1))
         ENDDO
         WRITE (46,'(1x,A30,6(e12.6,1x))') 'EL,TOT,REAC,INEL,CC,CSFus:',
     &          ELAcs, TOTcs, ABScs, SINl, SINLcc, CSFus
         WRITE (46,'(1x,I6)') 123456 
         DO l = 0, maxlw
            WRITE (46,*) l, SNGL(sel(l + 1))
         ENDDO
         CLOSE (46)
      ENDIF
      OPEN (45,FILE = (ctldir//ctmp23//'.INC'),FORM = 'UNFORMATTED')
      WRITE (45) maxlw, EINl, IRElat(Npro,Ntrg)
      DO l = 0, maxlw
         WRITE (45) stl(l + 1)
      ENDDO
      WRITE (45) ELAcs, TOTcs, ABScs, SINl, SINlcc, CSFus
C
C     A new flag is introduced to signal storage of the Shape elastic XS (Sel(L))
C
      l = 123456
      WRITE (45) l 
      DO l = 0, maxlw
         WRITE (45) sel(l + 1)
      ENDDO
      CLOSE (45)

  300 CONTINUE


C-----Print elastic and direct cross sections from ECIS
      WRITE (8,*) ' '
      WRITE (8,*) ' '
      IF (KTRlom(0,0).GT.0 .AND. FIRst_ein) THEN
        IF (DIRect.EQ.0) THEN
         WRITE (8,*)
     &       ' Results provided by Spherical Optical Model calculations'
        ELSEIF (DIRect.EQ.1 .OR. DIRect.EQ.2) THEN
         WRITE (8,*) ' Results provided by Coupled Channel calculations'
         WRITE (8,*) ' Inelastic scattering results provided by'
         WRITE (8,*) ' Coupled Channel + DWBA calculations'
        ELSEIF (DIRect.EQ.3) THEN
         WRITE (8,*)
     &       ' Results provided by Spherical Optical Model calculations'
         WRITE (8,*)
     &     ' Inelastic scattering results provided by DWBA calculations'
        ENDIF
      ENDIF

      IF(CSFus.gt.0.d0 .and. TOTred.ne.1.d0) then
        if(FUSred.NE.1) WRITE (8,*) 'WARNING: INPUT FUSred dismissed'
        FUSred = (TOTred*TOTcs - ELAcs*ELAred)/CSFus
        WRITE (8,'(1x,A18,F5.2,A49,F5.2)') 
     >   ' FUSRED scaled by ', sngl(FUSRED),
     >   ' to impose requested scaling of total by TOTRED =',
     >   sngl(TOTred)
         TOTred = 1.D0
      ENDIF

      el = EINl
      relcal = .FALSE.
      IF (IRElat(Npro,Ntrg).GT.0  .or. RELkin) relcal = .TRUE.
      CALL KINEMA(el,ecms,xmas_npro,xmas_ntrg,ak2,1,relcal)

      IF (EINl.LT.0.3D0 .AND. ZEJc(Npro).EQ.0) THEN
         s0 = FUSred * stl(1)/(2.0D+00*PI*SQRT(1.0D+06*EINl))
         rp = 1.35*(A(Ntrg)**0.333333333)
         r2 = rp*rp
         p1 = (ak2*r2)/(1.0D+00 + ak2*r2)
         s1a = FUSred * stl(2)/(2.0D+00*PI*p1*SQRT(1.0D+06*EINl))
         p2 = (ak2*r2)**2/(9.0D+00+3.0D+00*ak2*r2+(ak2*r2)**2)
         s2a = FUSred * stl(3)/(2.0D+00*PI*p2*SQRT(1.0D+06*EINl))
C--------Corrected scattering radius
         rp = SQRT((ELAred*ELAcs)/(4.0D+00*PI*10.D+00))
         WRITE (8,*)
         IF(S0_obs.GT.0.)   THEN
           WRITE (8,99004) S0_obs,S0_unc
           WRITE (12,99004) S0_obs,S0_unc
99004      FORMAT (7x,54(1h*)/
     &           6x,' LOW ENERGY NEUTRON SCATTERING:'/
     &           6x,' Exp.  Strength functions S0 =',f6.3,' (',f6.4,')')
         ELSE
           WRITE (8,'(7x,54(1h*)/
     &         7x,''LOW ENERGY NEUTRON SCATTERING:'')')
           WRITE (12,'(7x,54(1h*)/
     &         7x,''LOW ENERGY NEUTRON SCATTERING:'')')
         ENDIF
         WRITE (8,99005)
     &         s0*1D4, FUSRED*stl(1), s1a*1D4, 
     &                 FUSRED*stl(2), s2a*1D4, 
     &                 FUSRED*stl(3),
     &         EINl*1.D3, TOTcs*TOTred, rp, 1.35*(A(Ntrg)**0.333333333) 
         WRITE (12,99005)
     &         s0*1D4, FUSRED*stl(1), s1a*1D4, 
     &                 FUSRED*stl(2), s2a*1D4, 
     &                 FUSRED*stl(3),
     &         EINl*1.D3, TOTcs*TOTred, rp, 1.35*(A(Ntrg)**0.333333333)
99005    FORMAT (6x,' Calc. Strength functions S0 =',f6.3,' T0=',d12.6/
     &           6x,'                          S1 =',f6.3,' T1=',d12.6/
     &           6x,'                          S2 =',f6.3,' T2=',d12.6/
     &           6x,' Elab = ',F6.1,' keV',
     &              '        Total XS = ',F9.2,' mb'/
     &           6x,' Scattering radius =',f7.3,' fm'/
     &           6x,'   1.35 A**(1/3)   =',f7.3,' fm'//7x,54(1h*))
         WRITE (8,*)
         WRITE (12,*)
         selast = 0.d0
         DO l = 0, maxlw
           IF(sel(l+1).LT.1.d-15) EXIT
           selast = selast + (2*l+1)*sel(l + 1)
         ENDDO

         IF(selast.gt.0.d0) then
           selast = selast *  10.d0*PI/ak2
           WRITE(8,'(7x,28HSHAPE ELASTIC CROSS SECTION=,F10.3,1x,
     &              6H(ECIS:,F10.3,1H),1x,2hmb)') 
     &              selast, ELAcs 
           WRITE(12,'(7x,28HSHAPE ELASTIC CROSS SECTION=,F10.3,1x,
     &              6H(ECIS:,F10.3,1H),1x,2hmb)') 
     &              selast, ELAcs
           WRITE(53,'(7x,5HElab=,F7.2,1x,3HkeV,
     &              6x,17HSHAPE ELASTIC XS=,F10.3,1x,2hmb)') 
     &              EINl*1000, selast
           WRITE (8,99006)
           WRITE (12,99006)
           WRITE (53,99006)
           DO l = 0, maxlw
             IF(STL(l+1).LT.1.d-15) EXIT
             WRITE (8,99007) l, stl(l + 1), 
     &              10.d0*PI/ak2*sel(l + 1)
             WRITE (12,99007) l, stl(l + 1),
     &              10.d0*PI/ak2*sel(l + 1)
             WRITE (53,99007) l, stl(l + 1),
     &              10.d0*PI/ak2*sel(l + 1)
           ENDDO
           WRITE (8,99008)
           WRITE (12,99008)
           WRITE (53,99008)
99006      FORMAT (6x,' ****************************************'/
     &             6x,' *  L         Tl(L)    Shape Elastic(L) *')
99007      FORMAT (6x,' *',I3,2(1x,D15.7),'   *')
99008      FORMAT (6x,' ****************************************')
           WRITE (8,*)
           WRITE (8,*)
     &     '      SElast = SUM_over_L {(2*L+1)*Shape Elastic(L)}'
           WRITE (8,*)'      Sfus   = SUM_over_L {(2*L+1)*Tl(L)}'
           WRITE (8,*)
     &     '      Sreact = Sfusion + SUM_over_exc.lev.j {Sinl(j)}'
           WRITE (8,*)
           WRITE (12,*)
          ELSE
           WRITE (8,89006)
           WRITE (12,89006)
           WRITE (53,89006)
           DO l = 0, maxlw
             IF(STL(l+1).LT.1.d-15) EXIT
             WRITE (8 ,89007) l, stl(l + 1) 
             WRITE (12,89007) l, stl(l + 1)
             WRITE (53,89007) l, stl(l + 1)
           ENDDO
           WRITE (8,89008)
           WRITE (12,89008)
           WRITE (53,89008)
89006      FORMAT (/6x,' ************************'/
     &             6x,' *  L         Tl(L)     *')
89007      FORMAT (6x,' *',I3,1x,D15.7,'   *')
89008      FORMAT (6x,' ************************')
           WRITE (8,*)
           WRITE (8,*)'      Sfus   = SUM_over_L {(2*L+1)*Tl(L)}'
           WRITE (8,*)
     &     '      Sreact = Sfusion + SUM_over_exc.lev.j {Sinl(j)}'
           WRITE (8,*)
           WRITE (12,*)
          ENDIF
      ENDIF

      IF (INT(AEJc(0)).GT.0)
     &        coef = 10.d0*PI/ak2/
     &           (2*XJLv(LEVtarg,Ntrg) + 1.0)/(2*SEJc(Npro) + 1.0)
C-----channel spin min and max
      smin = ABS(SEJc(Npro) - XJLv(LEVtarg,Ntrg))
      smax = SEJc(Npro) + XJLv(LEVtarg,Ntrg)
      mul = smax - smin + 1.0001
      CSFus = 0.0
      DO ip = 1, 2      ! over parity
         DO j = 1, NLW  !over compound nucleus spin
C        DO j = 1, NDLW !over compound nucleus spin
            sum = 0.0
            DO ichsp = 1, mul
               chsp = smin + FLOAT(ichsp - 1)
               lmin = ABS(j - chsp - S1) + 0.0001
               lmax = j + chsp - S1 + 0.0001
               lmin = lmin + 1
               lmax = lmax + 1
               lmax = MIN0(NDLW,lmax)
               lmax = MIN0(maxlw,lmax)
               DO k = lmin, lmax
                  sum = sum + PAR(ip,LVP(LEVtarg,Ntrg),k - 1)*stl(k)
     &                  *DRTl(k)
               ENDDO
            ENDDO
            POP(NEX(1),j,ip,1) = coef*sum*(FLOAT(2*j + 1) - 2.0*S1)
     &                           *FUSred
            CSFus = CSFus + POP(NEX(1),j,ip,1)
            csmax = DMAX1(POP(NEX(1),j,ip,1),csmax)
         ENDDO
      ENDDO

      IF (ldbwacalc .AND. CSFus.GT.0.D0 .AND. SINl.GT.0.D0) THEN
         IF (DIRect.EQ.3) THEN
            WRITE (8,*)
     &         ' SOMP TLs normalized to substract DWBA contribution'
            WRITE (8,*)
     &         '                               to collective levels'
         ELSE
C-----------DIRECT=1 or DIRECT=2
            WRITE (8,*)
     &         ' CC OMP TLs normalized to substract DWBA contribution'
            WRITE (8,*)
     &         '                               to collective levels'
C           WRITE (8,*)
C    &         '             to discrete un-coupled collective levels'
         ENDIF
      ENDIF
      IF (IOUt.EQ.5) THEN
         WRITE (8,*)
         WRITE (8,*) 
     &   '      CSFus(SUM_Tl)    CSFus+SINl+CC+SINlcont    ABScs(OMP) '
         WRITE (8,'(4x,3(4x,D12.6,4x))')
     &   CSFus, CSFus + SINl + SINlcc + SINlcont, ABScs
         WRITE (8,*)
      ENDIF

C
C     CSFus contains only the reaction cross section to be distributed
C
C-----calculation/reading of transmission coefficients for input channel done ------
C
C-----Passing stl() to HRTW routine, please note that stl() never contain
C-----direct contribution !!!
C
      DO i = 1, NDLW
         ELTl(i) = stl(i)
      ENDDO
      DO j = NDLW, 1, -1
         NLW = j 
         IF (POP(NEX(1),j,1,1)*10000.D0.GT.csmax) exit
         IF (POP(NEX(1),j,2,1)*10000.D0.GT.csmax) exit
      ENDDO

C-----the next line can be used to increase the number of partial waves
C-----e.g., to account for a high-spin isomer
C-----Plujko_new-2005
  400 NLW = min(NLW + 1 + MAXmult,NDLW)
      RETURN
      END

      SUBROUTINE BASS(Ein,Zp,Ap,Zt,At,Bfus,E1,Crl,Csfus)
Ccc
Ccc   *********************************************************************
Ccc   *                                                         class:ppu *
Ccc   *                         B A S S
Ccc   * Calculates fusion x-section critical l-value for a heavy-ion
Ccc   * induced reaction according to Bass model. E1 is the energy at
Ccc   * which the linear dependence of l critical begins.
Ccc   * ref: formulae  from Bass, Nucl. Phys. A231(1974)45,
Ccc   * and nuclear potential from Phys. Rev. Lett. 39(1977)265
Ccc   *
Ccc   * input:EIN-incident energy (c.m.)
Ccc   *       ZP -Z of a projectile
Ccc   *       AP -A of a projectile
Ccc   *       ZT -Z of a target
Ccc   *       AT -A of a target
Ccc   *
Ccc   * output:BFUS-fusion barrier
Ccc   *        E1   -see above
Ccc   *        CRL  -critical angular momentum
Ccc   *        CSFUS-fusion x-section
Ccc   *
Ccc   * calls:FINDA
Ccc   *
Ccc   *********************************************************************
Ccc
C
C COMMON variables
C
      DOUBLE PRECISION MI, R1, R12, R2
      COMMON /FIND  / R1, R2, R12, MI
C
C Dummy arguments
C
      DOUBLE PRECISION Ap, At, Bfus, Crl, Csfus, E1, Ein, Zp, Zt
C
C Local variables
C
      DOUBLE PRECISION arg, d, dfu, e2, ee2, f, ht, le1, le2, m0, m1,
     &                 m2, p, t, vc, vmax, vmm, vn, x, y
      INTEGER INT
      INTEGER j, jl
      DATA e2, m0, ht, d/1.44, 1.044, 6.589, 1.35/
      m1 = Ap*m0
      m2 = At*m0
      MI = Ap*At/(Ap + At)*m0
      p = Ap**(1./3.)
      t = At**(1./3.)
      R1 = 1.16*p - 1.39/p
      R2 = 1.16*t - 1.39/t
      R12 = R1 + R2
      x = Zp*Zt*e2*(R1 + R2)/(R1*R2*R12**2)*0.07
      y = ht**2*(R1 + R2)/(MI*R1*R2*R12**3)*0.07
      f = 1./(1. + 2./5.*((m1*R1**2.+m2*R2**2.)/(MI*R12**2.)))
      le1 = SQRT((1. - x)/y)
      le2 = le1/f
      vc = Zp*Zt*e2/R12
      vn = R1*R2/R12*(1./(0.03 + 0.0061))
      E1 = vc - vn + (ht**2*le1**2)/(2.*MI*R12**2)
      ee2 = vc - vn + (ht**2*le2**2)/(2.*MI*R12**2)
      dfu = -d*LOG(x)/(1. - 2.*d/R12)
      arg = dfu/d
      IF (arg.GT.74.D0) arg = 74.
      Bfus = Zp*Zt*e2/R12*(R12/(R12 + dfu) - d/(x*R12)*EXP((-arg)))
      IF (Ein.GT.Bfus) THEN
         Crl = SQRT((2.*MI*R12**2/ht**2)*(Ein - vc + vn))
         vmm = 0.0
         vmax = 10000.0
         IF (Ein.LT.E1) THEN
            jl = INT(le1)
            DO j = 1, jl
               Crl = jl - j + 1
               vmm = vmax
               CALL FINDA(Zp,Zt,Crl,vmax)
               IF (vmax.LE.Ein) GOTO 50
            ENDDO
         ENDIF
   50    IF (Ein.GT.ee2) Crl = le2
         IF (Ein.LT.E1) Crl = Crl + (Ein - vmax)/(vmm - vmax)
         Csfus = 657.*(Ap + At)*Crl**2/(Ap*At*Ein)

         WRITE (8,*) ' '
         WRITE (8,*)
     &' Fusion cross section calculated using Bass model as a reference'
         WRITE (8,*) ' see Nucl. Phys. A231(1974)45'
         WRITE (8,*) 
     &     ' Nuclear potential from Phys. Rev. Lett. 39(1977)265'
         WRITE (8,*) ' '
         WRITE (8,*) ' Bass Barr =',sngl(Bfus),  ' Crit.L =',sngl(Crl)
         WRITE (8,*) ' '
         WRITE (8,*) ' Bass XS   =',sngl(Csfus),' mb'
         WRITE (8,*) 

      ELSE

         WRITE (8,'(1X,
     >  '' WARNING: Incident energy below the Bass fusion barrier'')')
         WRITE (8,'(1X,
     >  '' WARNING: For the Bass model, fusion cross section = 0'')')
         WRITE (8,*)
         Csfus = 0.d0
      ENDIF
      RETURN
      END

      SUBROUTINE FINDA(Zp,Zt,Crl,Vm)
Ccc
Ccc ********************************************************************
Ccc *                                                         class:mpu*
Ccc *                         F I N D A                                *
Ccc *                                                                  *
Ccc * Solves the equation in Bass model; VM is a solution              *
Ccc *                                                                  *
Ccc * input:ZP -Z of a projectile                                      *
Ccc *       AP -A of a projectile                                      *
Ccc *       ZT -Z of a target                                          *
Ccc *       AT -A of a target                                          *
Ccc *       CRL-l critical                                             *
Ccc *                                                                  *
Ccc * output:VM -solution                                              *
Ccc *                                                                  *
Ccc * calls:none                                                       *
Ccc *                                                                  *
Ccc *                                                                  *
Ccc ********************************************************************
Ccc
C
C COMMON variables
C
      DOUBLE PRECISION MI, R1, R12, R2
      COMMON /FIND  / R1, R2, R12, MI
C
C Dummy arguments
C
      DOUBLE PRECISION Crl, Vm, Zp, Zt
C
C Local variables
C
      DOUBLE PRECISION e2, eps, ht, xm, xn, xp
      INTEGER nr
      DATA e2, ht/1.44, 6.589/
      DATA eps/0.001/
      nr = 0
      xn = R12
      xp = 3*R12
  100 xm = (xn + xp)/2
      nr = nr + 1
      IF (nr.LE.50) THEN
         IF (ABS((-Zp*Zt*e2/xm**2) + ((-ht**2*Crl**2/(MI*xm**3)))
     &       + R1*R2/R12*(0.03/3.3*EXP((xm-R12)/3.3)
     &       +0.0061/0.65*EXP((xm-R12)/0.65))
     &       /(0.03*EXP((xm-R12)/3.3)+0.0061*EXP((xm-R12)/0.65))**2)
     &       .GT.eps) THEN
            IF (((-Zp*Zt*e2/xm**2)) + ((-ht**2*Crl**2/(MI*xm**3)))
     &          + R1*R2/R12*(0.03/3.3*EXP((xm-R12)/3.3)
     &          + 0.0061/0.65*EXP((xm-R12)/0.65))
     &          /(0.03*EXP((xm-R12)/3.3) + 0.0061*EXP((xm-R12)/0.65))
     &          **2.LT.0.D0) THEN
               xp = xm
               GOTO 100
            ELSEIF (((-Zp*Zt*e2/xm**2)) + ((-ht**2*Crl**2/(MI*xm**3)))
     &              + R1*R2/R12*(0.03/3.3*EXP((xm-R12)/3.3)
     &              + 0.0061/0.65*EXP((xm-R12)/0.65))
     &              /(0.03*EXP((xm-R12)/3.3) + 0.0061*EXP((xm-R12)/0.65)
     &              )**2.NE.0.D0) THEN
               xn = xm
               GOTO 100
            ENDIF
         ENDIF
      ENDIF
      Vm = Zp*Zt*e2/xm + ht**2*Crl**2/(2*MI*xm**2)
     &     - R1*R2/R12/(0.03*EXP((xm-R12)/3.3)
     &     + 0.0061*EXP((xm-R12)/0.65))
      IF (nr.GT.50) WRITE (8,'(10X,''MAX NO. OF ITERATIONS IN FINDA'')')
      END


      DOUBLE PRECISION FUNCTION XFUS(Ein,Ap,At,D,Crl)
Ccc
Ccc ********************************************************************
Ccc *                                                         class:ppu*
Ccc *                         X F U S                                  *
Ccc *                                                                  *
Ccc *                                                                  *
Ccc *                                                                  *
Ccc * input:EIN - incident energy (c.m.)                               *
Ccc *       AP  - projectile A                                         *
Ccc *       AT  - target A                                             *
Ccc *       D   - difusness in transmission coefficient formula        *
Ccc *       CRL - l critical for fusion                                *
Ccc *                                                                  *
Ccc * output:XFUS- fusion x-section                                    *
Ccc *                                                                  *
Ccc * calls:none                                                       *
Ccc *                                                                  *
Ccc *                                                                  *
Ccc ********************************************************************
Ccc
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C
C Dummy arguments
C
      DOUBLE PRECISION Ap, At, Crl, D, Ein
C
C Local variables
C
      DOUBLE PRECISION al, args, sum, tl
      REAL FLOAT
      INTEGER i, icrl
      INTEGER INT
      sum = 0.0
      icrl = INT(Crl + 5.0*D)
      DO i = 1, icrl
         al = FLOAT(i - 1)
         args = (al - Crl)/D
         IF (args.GT.74.D0) args = 74.
         tl = 1./(1. + EXP(args))
         sum = (2.*al + 1.)*tl + sum
      ENDDO
      XFUS = 657.*(Ap + At)/(Ap*At*Ein)*sum
      END


      SUBROUTINE PUSH(Ecm,A,Ap,At,Bas,Expush,Sigi,Trunc,Stl,Nlw,Ndlw)
Ccc ********************************************************************
Ccc *                                                         class:ppu*
Ccc *                      P U S H                                     *
Ccc *                                                                  *
Ccc *  Calculates fusion transmission coefficients in the distributed  *
Ccc *  fusion barrier model.                                           *
Ccc *                                                                  *
Ccc *                                                                  *
Ccc * output:STL - fusion transmission coefficients                    *
Ccc *                                                                  *
Ccc * calls:INTGRS                                                     *
Ccc *                                                                  *
Ccc *                                                                  *
Ccc ********************************************************************
C
C COMMON variables
C
      DOUBLE PRECISION BAVe, E, EROt, SIG
      COMMON /EXTRAP/ BAVe, EROt, E, SIG
C
C Dummy arguments
C
      DOUBLE PRECISION A, Ap, At, Bas, Ecm, Expush, Sigi, Trunc
      INTEGER Ndlw, Nlw
      DOUBLE PRECISION Stl(Ndlw)
C
C Local variables
C
      DOUBLE PRECISION amu, dintf, prob, r0, rf, xlow, xmax
      DOUBLE PRECISION F, G
      INTEGER j
      EXTERNAL F, G
      DATA r0/1.07/
      E = Ecm
      SIG = Sigi
      BAVe = Bas + Expush
      xlow = MAX(BAVe - Trunc*SIG,0.D0)
      xmax = BAVe + Trunc*SIG
      CALL INTGRS(xlow,xmax,F,dintf)
      amu = At*Ap/A
      rf = r0*(At**0.3333 + Ap**0.3333)
      Nlw=0
      DO j = 1, Ndlw
         EROt = (j - 1)*j*20.79259/(amu*rf**2)
         EROt = EROt/2.0
         CALL INTGRS(xlow,xmax,G,prob)
         Stl(j) = prob/dintf
         IF (Stl(j).gt.1.d-10) Nlw = j
      ENDDO
      END

      DOUBLE PRECISION FUNCTION F(X)
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C COMMON variables
C
      DOUBLE PRECISION BAVe, E, EROt, SIG
      COMMON /EXTRAP/ BAVe, EROt, E, SIG
C
C Dummy arguments
C
      DOUBLE PRECISION X
      F = EXP(( - (BAVe-X)/(2.0*SIG**2))**2)
      END
C
      DOUBLE PRECISION FUNCTION G(X)
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C COMMON variables
C
      DOUBLE PRECISION BAVe, E, EROt, SIG
      COMMON /EXTRAP/ BAVe, EROt, E, SIG
C
C Dummy arguments
C
      DOUBLE PRECISION X
C
C Local variables
C
      DOUBLE PRECISION arg, htom, pi
      DOUBLE PRECISION F
      EXTERNAL F
      DATA pi, htom/3.14159D0, 4.D0/
      arg = -2.*pi*(E - X - EROt)/htom
      IF (arg.LT.( - 74.D0)) G = F(X)
      IF (arg.GT.74.D0) G = 0.
      IF (ABS(arg).LE.74.D0) G = F(X)
     &                           /(1 + EXP((-2.*pi*(E-X-EROt)/htom)))
      END

      SUBROUTINE PROCESS_ECIS(Iopsys,Outname,Length,Iret,ICAlangs)
C
C Dummy arguments
C
      INTEGER Iopsys, Iret, Length
      CHARACTER*(*) Outname
C
C Local variables
C
      CHARACTER*132 ctmp
      INTEGER*4 iwin
      INTEGER*4 PIPE
      IF (Iopsys.EQ.0) THEN
C--------LINUX
         ctmp = 'cp ecis06.cs  '//Outname(1:Length)//'.CS '
         iwin = PIPE(ctmp)
         IF(ICAlangs.GT.0) THEN
           ctmp = 'cp ecis06.exp  '//Outname(1:Length)//'.EXP '
           iwin = PIPE(ctmp)
          ENDIF
         IF (Iret.EQ.1) RETURN
         ctmp = 'cp ecis06.tlj '//Outname(1:Length)//'.TLJ'
         iwin = PIPE(ctmp)
         IF (Iret.EQ.2) RETURN
         ctmp = 'cp ecis06.ang '//Outname(1:Length)//'.ANG'
         iwin = PIPE(ctmp)
         ctmp = 'cp ecis06.leg '//Outname(1:Length)//'.LEG'
         iwin = PIPE(ctmp)
         IF (Iret.EQ.3) RETURN
         ctmp = 'cp ecis06.ics '//Outname(1:Length)//'.ICS'
         iwin = PIPE(ctmp)
      ELSE
C--------WINDOWS
         ctmp = 'copy ecis06.cs  '//Outname(1:Length)//'.CS  >NUL'
         iwin = PIPE(ctmp)
         IF(ICAlangs.GT.0) THEN
           ctmp = 'cp ecis06.exp  '//Outname(1:Length)//'.EXP  >NUL'
           iwin = PIPE(ctmp)
          ENDIF
         IF (Iret.EQ.1) RETURN
         ctmp = 'copy ecis06.tlj '//Outname(1:Length)//'.TLJ >NUL'
         iwin = PIPE(ctmp)
         IF (Iret.EQ.2) RETURN
         ctmp = 'copy ecis06.ang '//Outname(1:Length)//'.ANG >NUL'
         iwin = PIPE(ctmp)
         ctmp = 'copy ecis06.leg '//Outname(1:Length)//'.LEG >NUL'
         iwin = PIPE(ctmp)
         IF (Iret.EQ.3) RETURN
         ctmp = 'copy ecis06.ics '//Outname(1:Length)//'.ICS >NUL'
         iwin = PIPE(ctmp)
      ENDIF
      END


