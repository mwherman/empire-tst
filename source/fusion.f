Ccc   * $Author: Capote $
Ccc   * $Date: 2005-02-14 17:12:24 $
Ccc   * $Id: fusion.f,v 1.33 2005-02-14 17:12:24 Capote Exp $
C
      SUBROUTINE MARENG(Npro, Ntrg)
C
C
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
Ccc   * author: M.Herman                                                 *
Ccc   * date:   15.Feb.1993                                              *
Ccc   * revision:1    by:Herman                   on:  .Oct.1994         *
Ccc   * revision:2    by:Capote                   on:  .Feb.2001         *
Ccc   * revision:3    by:Capote                   on:  .Jan.2005         *
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C COMMON variables
C
      DOUBLE PRECISION ELTl(NDLW), S1
      COMMON /ELASTIC/ ELTl
      COMMON /WAN   / S1
      COMMON /ECISXS/ ELAcs, TOTcs, ABScs, SINl
C
C Dummy arguments
C
      INTEGER Npro, Ntrg
C
C Local variables
C
      DOUBLE PRECISION chsp, coef, csmax, csvalue, PI, smax,
     &                 smin, stl(NDLW), sum, wf, ParcnJ, cnJ
      DOUBLE PRECISION DMAX1
      REAL FLOAT
      INTEGER i, ichsp, ip, ipa, j, k, l, lmax, lmin, maxlw, mul
      INTEGER MIN0
      DOUBLE PRECISION PAR, W2, xmas_npro, xmas_ntrg, RMU
      INTEGER itmp1
      LOGICAL LDBWacalc, fexist, LTLj, DOdwba
      CHARACTER*132 ctmp
      CHARACTER*120 rstring
      CHARACTER*18 ctmp18
      INTEGER*4 IWIN,PIPE
      CHARACTER*6 cTLdir
       DATA cTLdir/6H../TL//
      PAR(i, ipa, l) = 0.5*(1.0 - ( - 1.0)**i*ipa*( - 1.0)**l)

C
C  Zero qd fraction of photabsorption before it can do any damage
C
      QDfrac=0.0d0
C
C-----Reduced mass corrected for proper mass values
C
C     xmas_npro = (AEJc(Npro)*AMUmev + XMAss_ej(Npro))/AMUmev
      xmas_ntrg = (A(Ntrg)*AMUmev + XMAss(Ntrg))/AMUmev
      xmas_npro = EJMass(Npro)
      el = EINl

      CALL KINEMA(el, ecms, xmas_npro, xmas_ntrg, RMU, ak2, 1, RELkin)
C
C     wf = 10*W2*RMU*ecms
C
      wf = ak2/10.D0

      IF(INT(AEJC(0)).GT.0)
     & coef = PI/wf/(2*XJLv(LEVtarg, Ntrg) + 1.0)/(2*SEJc(Npro) + 1.0)

      S1 = 0.5
      IF(AINT(XJLv(LEVtarg,Ntrg) + SEJc(Npro)) - XJLv(LEVtarg, Ntrg) -
     & SEJc(Npro).EQ.0.0D0) S1 = 1.0

      ELAcs=0.d0
      TOTcs=0.d0
      ABScs=0.d0
      SINl=0.d0
      csmax = 0.0
      CSFus = 0.0
      maxlw = 0
      DO i = 1, NDLW
         stl(i) = 0.0
      ENDDO

      WRITE(ctmp18, '(i2.2,i3.3,1h_,i2.2,i3.3,1h_,i6.6)')
     &         INT(ZEJc(NPRo)),INT(AEJc(NPRo)),
     &         INT(Z(Ntrg)),   INT(A(NTRg)),   INT(EINl*1000)

C-----This part prompts for the name of a data file. The INQUIRE
C-----statement then determines whether or not the file exists.
C-----If it does not, the program calculates new transmission coeff.
      INQUIRE(FILE = (cTLdir//ctmp18//'_INC.BIN'), EXIST = fexist)

      IF(fexist) then
C-------Here the old calculated files should be readed
        OPEN(45, FILE = (cTLdir//ctmp18//'_INC.BIN'),
     &                                     FORM = 'UNFORMATTED',ERR=25)
        IF(IOUt.EQ.5) OPEN(46, FILE = cTLdir//ctmp18//'_INC.LST')
        READ(45, END = 25)lmax, ener
        IF(IOUt.EQ.5) WRITE(46, '(A5,I6,E12.6)')'LMAX:', lmax, ener

        IF(ABS(ener - EINl).LT.0.0001d0) THEN
          maxlw = lmax
          DO l = 0, maxlw
            READ(45,END=25) stl(l+1)
            IF(IOUt.EQ.5) WRITE(46, *) l, SNGL(stl(l+1))
          ENDDO
          READ(45,END=25) ELAcs,TOTcs,ABScs,SINl
  20      CLOSE(45)
          IF(IOUt.EQ.5) WRITE (46,'(1x,A21,4(e12.6,1x))')
     &        'EL,TOT,ABS,INEL XSs:',ELAcs,TOTcs,ABScs,SINl
          IF(IOUt.EQ.5) CLOSE(46)
          IF(IOUt.EQ.5) THEN
            WRITE(6, *)
     &' Transmission coefficients for incident channel read from file: '
            WRITE(6, *) ' ',cTLdir//ctmp18//'_INC.BIN'
          ENDIF

          GOTO 4000
        ENDIF
C
C-------If (energy read from file do not coincide
C-------this nucleus should be recalculated (goto 300)
C
  23    WRITE(6, *)'WARNING: ENERGY MISMATCH:  Elab =', EINl,
     &              ' REQUESTED ENERGY=', SNGL(ener)
  25    CLOSE(45, STATUS = 'DELETE')
        IF(IOUt.EQ.5)CLOSE(46, STATUS = 'DELETE')
        WRITE(6, *)
     &              'WARNING: FILE WITH TRANSM. COEFF.',
     &              ' FOR INC.CHANNEL HAS BEEN DELETED'
        IF(IOUt.EQ.5)CLOSE(46, STATUS = 'DELETE')

      endif
C-----Calculation of fusion cross section for photon induced reactions
3000  IF(INT(AEJC(NPRo)).EQ.0)THEN

        IF(SDREAD) THEN

C---------Reading of spin distribution from file SDFILE
C
C         If you have file "SDFILE" -> it is possible to use
C         input spin distribution
C
C          Format of file:
C          rows with sequentialy  cnJ, ParcnJ, csvalue
C          where  cnJ     - spin of nucleus,
C                 ParcnJ  - parity
C                 csvalue - cross-section
C
C         limits: cnJ=n*0.5, where n=0,1,2...
C                 A of c.n. /2 = integer --> cnJ=n*1, where n=0,1,2...
C                 A of c.n. /2 = integer+1/2 --> cnJ=n*1/2, where n=0,1,2...
C                 ParcnJ=-1 or 1
C                 csvalue = value in mb
C
C         Reading no more than 2*NDLW rows
          WRITE(6, *)
     &' Spin distribution of fusion cross section read from SDREAD file'
          WRITE(6, *)
     &' (all previous instructions concerning fusion ignored)'
           DO i = 1, 2*NDLW
            READ(43, *, END = 101) cnJ,ParcnJ,csvalue
C-----------Spin of c.n. cnJ=j-S1 => j=cnJ+S1
            IF(2*cnJ-DINT(2*cnJ).NE.0.00)
     >               STOP 'cnJ!=n*1/2, n=0,+-1...  in SDREAD file'
            j=IDNINT(cnJ+S1)
            IF(ParcnJ.EQ.1.) ip = 1
            IF(ParcnJ.EQ.-1.)ip = 2
            IF(ParcnJ.NE.1.AND.ParcnJ.NE.-1)
     >               STOP 'ParcnJ!=+-1 in SDREAD file'
            POP(NEX(1), j, ip, 1) = csvalue
            CSFus = CSFus + POP(NEX(1), j, ip, 1)
            csmax = DMAX1(POP(NEX(1), j, ip, 1), csmax)
          ENDDO

101       CONTINUE

C---------END of spin distribution from file SDFILE

        ELSE

          JSTab(1) = NDLW !stability limit not a problem for photoreactions
          IF(EIN.LE.ELV(NLV(Ntrg),Ntrg)) THEN
            WRITE(6,*)'WARNING: '
            WRITE(6,*)'WARNING: ECN=',EIN,' Elev=',ELV(NLV(Ntrg),Ntrg)
            WRITE(6,*)'WARNING: CN excitation energy below continuum'
            WRITE(6,*)'WARNING: cut-off. zero reaction cross section'
            WRITE(6,*)'WARNING: will result'
            WRITE(6,*)'WARNING: '
          ENDIF
C---------E1
          IF(IGE1.NE.0)THEN
C----------factor 10 near HHBarc from fm**2-->mb
           E1Tmp=10*HHBarc**2*PI*E1(Ntrg,Z,A,EINl,0.d0,0.d0)/(2*EINl**2)
           QDTmp=SIGQD(Z(Ntrg),A(Ntrg),EINl,Lqdfac)
           E1Tmp=(E1Tmp+QDTmp/3.0d0)/(2*XJLv(LEVtarg, Ntrg)+1)
C----------do loop over parity
           DO ip = 1, 2
C           Quasideuteron contribution QDTmp by Carlson
            WPARG=PAR(ip, LVP(LEVtarg, 0), 1)*E1Tmp
C-----------do loop over compound nucleus spin
            DO j = 1, NDLW
C-------------Spin of c.n. J=j-S1
              IF(ABS(j-S1-XJLv(LEVtarg, Ntrg)).LE.1.0.AND.
     &              (j-S1+XJLv(LEVtarg, Ntrg)).GE.1.0) THEN
                POP(NEX(1), j, ip, 1) = POP(NEX(1), j, ip, 1) +
     &          (FLOAT(2*j + 1) - 2.0*S1)*WPARG
              ENDIF
            ENDDO
           ENDDO
          ENDIF
C---------end of E1

C---------M1
          IF(IGM1.NE.0)THEN
C----------factor 10 near HHBarc from fm**2-->mb
           E1Tmp=10*HHBarc**2*PI*XM1(EINl)/(2*EINl**2)
     &          /(2*XJLv(LEVtarg, Ntrg)+1)
C----------do loop over parity
           DO ip = 1, 2
            WPARG=PAR(ip, LVP(LEVtarg, 0), 2)*E1Tmp
C-----------do loop over compound nucleus spin
            DO j = 1, NDLW
C-------------Spin of c.n. J=j-S1
              IF(ABS(j-S1-XJLv(LEVtarg, Ntrg)).LE.1.0.AND.
     &              (j-S1+XJLv(LEVtarg, Ntrg)).GE.1.0) THEN
                POP(NEX(1), j, ip, 1)=POP(NEX(1), j, ip, 1) +
     &            (FLOAT(2*j + 1) - 2.0*S1)*WPARG
              ENDIF
            ENDDO
           ENDDO
          ENDIF
C---------end of M1

C---------E2
          IF(IGE2.NE.0)THEN
C----------factor 10 near HHBarc from fm**2-->mb
           E1Tmp=10*HHBarc**2*PI*E2(EINl)/(2*EINl**2)
     &          /(2*XJLv(LEVtarg, Ntrg)+1)
C----------do loop over parity
           DO ip = 1, 2
            WPARG=PAR(ip, LVP(LEVtarg, 0), 2)*E1Tmp
C-----------do loop over compound nucleus spin
            DO j = 1, NDLW
C-------------Spin of c.n. J=j-S1
              IF(ABS(j-S1-XJLv(LEVtarg, Ntrg)).LE.2.0.AND.
     &              (j-S1+XJLv(LEVtarg, Ntrg)).GE.2.0) THEN
C---------------factor 10 near HHBarc from fm**2-->mb
                POP(NEX(1), j, ip, 1)=POP(NEX(1), j, ip, 1) +
     &            (FLOAT(2*j + 1) - 2.0*S1)*WPARG
              ENDIF
            ENDDO
           ENDDO
          ENDIF
C-------end of E2
         DO ip = 1, 2
          DO j = 1, NDLW
              CSFus = CSFus + POP(NEX(1), j, ip, 1)
              csmax = DMAX1(POP(NEX(1), j, ip, 1), csmax)
          ENDDO
        ENDDO
C                     QDTmp=SIGQD(Z(Ntrg),A(Ntrg),EINl,Lqdfac)
        IF(IGE1.NE.0 .and. CSFus.GT.0.d0) QDfrac= QDTmp/CSFus

        ENDIF
C-------END of calculation of fusion cross section
C                for photon induced reactions
        RETURN

      ENDIF

      IF(FUSread) THEN
C
C-------if FUSREAD true read l distribution of fusion cross section
C-------and calculate transmission coefficients
        DO j = 1, NDLW
            READ(11, *, END = 50)csvalue
            stl(j) = csvalue*wf/PI/(2*j - 1)
            IF(stl(j).GT.1.0D0)THEN
               WRITE(6, *)' '
               WRITE(6,
     &'(''TOO LARGE INPUT FUSION CROSS SECTION'',              '' FOR l=
     &'',I3,'' RESULTING Tl>1'')')j - 1
               WRITE(6, *)' EXECUTION STOPPED!!!'
               STOP
            ENDIF
        ENDDO
 50     NLW = j - 1
        maxlw = NLW
        WRITE(6, *)
     &  ' Spin distribution of fusion cross section read from the file '
        WRITE(6, *)
     &          ' (all previous instructions concerning fusion ignored)'

      ELSE ! FUSION XS ARE CALCULATED (.NOT.FUSREAD)

C-------calculation of o.m. transmission coefficients for absorption
        IF(KTRlom(Npro, Ntrg).GT.0)THEN
C
          einlab = -EINl
          IWArn = 0
          LDBWacalc = .FALSE.
          LTLj = .FALSE.

          DOdwba = .FALSE.
          DO l = 1, NDCollev
            if(icollev(l).LT.50) cycle   ! Skipping coupled levels
            DOdwba = .TRUE.
          ENDDO
          IF(DIRECT.EQ.3) DOdwba = .TRUE.

          IF(DOdwba .AND. DIRect.GT.0 .and.  ( .NOT.DEFORMED .OR.
     &          (MOD(NINT(A(Ntrg)),2).eq.0.and.
     &         MOD(NINT(Z(Ntrg)),2).eq.0) ) ) THEN

            IF(DIRECT.EQ.1 .OR. DIRECT.EQ.3) THEN
C-------------Saving KTRlom(0,0)
              itmp1 = KTRlom(0, 0)
              KTRlom(0, 0) = KTRompcc
              CCCalc = .TRUE.
            ENDIF

C           DWBA calculation. All collective levels considered
            IF(DIRECT.EQ.3) THEN
              WRITE(6, *)' DWBA calculations for inelastic scattering'
            ELSE
              WRITE(6, *)' DWBA calculations for inelastic scattering',
     &                   ' to uncoupled collective levels'
            ENDIF

            CALL ECIS_CCVIB(Npro,Ntrg,einlab,.TRUE.,1)

            IF(DIRECT.NE.3) THEN

              CALL PROCESS_ECIS(IOPsys,'dwba',4,4)

            ELSE

              CALL PROCESS_ECIS(IOPsys,'INCIDENT',8,4)
              CALL ECIS2EMPIRE_TL_TRG
     &            (Npro, Ntrg, maxlw, stl, .TRUE. )

              LTLj = .TRUE. ! TLs are obtained here for DIRECT=3

              WRITE(6, *)' SOMP transmission coefficients used for ',
     &                   'fusion determination'

            ENDIF

            IF(DIRECT.EQ.1 .OR. DIRECT.EQ.3) THEN
C-------------Restoring KTRlom(0,0)
              KTRlom(0, 0) = itmp1
              CCCalc = .FALSE.
            ENDIF

            LDBWacalc = .TRUE.
          ENDIF

C
C         In EMPIRE 2.19 DIRECT=1 and DIRECT=2 produces exactly
C         the same array of transmission coefficients calculated
C         by CC OMP. The differences between DIRECT 1/2 options are in the
C         calculations of the outgoing channel (TRANSINP() routine)
C         DIRECT 2 option uses CC method to produce outgoing TLs
C         for the inelastic channel. DIRECT 1 option assumes SOMP
C         with only one level to calculate the inelastic TLs.
C
C                RCN, 01/2005
C
          IF((DIRect.EQ.1 .OR. DIRect.EQ.2) .AND. AEJc(Npro).LE.1) THEN
            WRITE(6, *)' CC transmission coefficients used for ',
     &                 'fusion determination'

            IF(DIRECT.EQ.1) THEN
C-------------Saving KTRlom(0,0)
              itmp1 = KTRlom(0, 0)
              KTRlom(0, 0) = KTRompcc
              CCCalc = .TRUE.
            ENDIF

C-----------Transmission coefficient matrix for incident channel
C-----------is calculated by CC method.
            IF(DEFormed)THEN
C             EXACT ROTATIONAL MODEL CC calc. (only coupled levels)
              CALL ECIS_CCVIBROT(Npro, Ntrg, einlab, 0)
              IF(LDBWacalc) THEN
                CALL PROCESS_ECIS(IOPsys,'ccm',3,4)
              ELSE
                CALL PROCESS_ECIS(IOPsys,'INCIDENT',8,4)
                CALL ECIS2EMPIRE_TL_TRG
     &            (Npro, Ntrg, maxlw, stl, .FALSE.)
              ENDIF
            ELSE
C             EXACT VIBRATIONAL MODEL CC calc. (only coupled levels)
              CALL ECIS_CCVIB(Npro, Ntrg, einlab, .FALSE., -1)
              IF(LDBWacalc) THEN
                CALL PROCESS_ECIS(IOPsys,'ccm',3,4)
              ELSE
                CALL PROCESS_ECIS(IOPsys,'INCIDENT',8,4)
                CALL ECIS2EMPIRE_TL_TRG
     &            (Npro, Ntrg, maxlw, stl, .TRUE. )
              ENDIF
            ENDIF

            IF(DIRECT.EQ.1) THEN
C-------------Restoring KTRlom(0,0)
              KTRlom(0, 0) = itmp1
              CCCalc = .FALSE.
            ENDIF

            LTLj = .TRUE.

            IF(LDBWacalc) THEN
C
C             Joining DWBA and CCM files
C             Total, elastic and reaction cross section is from CCM
C
              IF(IOPsys.EQ.0)THEN
C               LINUX
                ctmp = 'cp ccm.CS INCIDENT.CS'
                iwin = PIPE(ctmp)
                ctmp = 'cp ccm.TLJ INCIDENT.TLJ'
                iwin = PIPE(ctmp)
              ELSE
C               WINDOWS
                ctmp = 'copy ccm.CS INCIDENT.CS >NUL'
                iwin = PIPE(ctmp)
                ctmp = 'copy ccm.TLJ INCIDENT.TLJ'
                iwin = PIPE(ctmp)
              ENDIF

C             Inelastic cross section (incident.ics)
              OPEN(45, FILE = 'dwba.ICS', STATUS = 'OLD', ERR=1000)
              OPEN(46, FILE = 'ccm.ICS' , STATUS = 'OLD')
              OPEN(47, FILE = 'INCIDENT.ICS' , STATUS = 'UNKNOWN')
              READ(45, '(A80)', END = 1000) rstring
              READ(46, '(A80)', END=990) ! first line is taken from dwba
  990         write(47,'(A80)') rstring
              DO i = 2, ND_nlv
                READ(45, '(A80)', END = 1000) rstring
                READ(46,'(A80)',END=995) rstring
  995           write(47,'(A80)') rstring
              ENDDO
 1000         CLOSE(45, STATUS='DELETE')
              CLOSE(46, STATUS='DELETE')
              CLOSE(47)

C             Angular distribution (incident.ang)
              OPEN(45, FILE = 'dwba.ANG', STATUS = 'OLD', ERR=2000)
              READ(45, '(A80)', END = 2000) rstring
              OPEN(46, FILE = 'ccm.ANG' , STATUS = 'OLD', ERR=2000)
              READ(46, '(A80)', END=1005) ! first line is taken from dwba
 1005         OPEN(47, FILE = 'INCIDENT.ANG' , STATUS = 'UNKNOWN')
              write(47,'(A80)') rstring
              DO i = 1, ND_nlv
                READ(45, '(5x,F5.1,A1,4x,i5)', END = 2000)
     >                                           stmp1,ctmp1,nang
                READ(46, '(5x,F5.1,A1)', END = 1010) stmp2,ctmp2
C               checking the correspondence of the excited states
                IF(stmp1.ne.stmp2 .OR. ctmp1.ne.ctmp2) THEN
                  write(6,*)
     >            ' WARNING: DWBA and CCM state order does not coincide'
                  STOP
     >            ' WARNING: DWBA and CCM state order does not coincide'
                ENDIF
 1010           BACKSPACE 45
                READ(45, '(A80)', END = 2000) rstring
                write(47,'(A80)') rstring
                DO j = 1, nang
                  READ(45, '(A80)', END = 2000) rstring
                  READ(46,'(A80)' , END = 1015) rstring
 1015             write(47,'(A80)') rstring
                ENDDO
              ENDDO
2000          CLOSE(45, STATUS='DELETE')
              CLOSE(46, STATUS='DELETE')
              CLOSE(47)

              IF(DEFormed)THEN
                CALL ECIS2EMPIRE_TL_TRG
     &            (Npro, Ntrg, maxlw, stl, .FALSE.)
              ELSE
                CALL ECIS2EMPIRE_TL_TRG
     &            (Npro, Ntrg, maxlw, stl, .TRUE.)
              ENDIF

            ENDIF  ! END of LDWBA (DWBA and CCM joining process)

          ENDIF ! END of DIRECT=1/2 block

          IF( .NOT.LTLj ) THEN
C-----------Transmission coefficient matrix for incident channel
C-----------is calculated like in SOMP i.e.
C           SCAT2 like calculation (one state, usually gs, alone)
            CALL ECIS_CCVIB(Npro,Ntrg,einlab,.TRUE.,0)

            CALL PROCESS_ECIS(IOPsys,'INCIDENT',8,3)

            WRITE(6, *)' SOMP transmission coefficients used for ',
     &                 'fusion determination'
            CALL ECIS2EMPIRE_TL_TRG
     &        (Npro, Ntrg, maxlw, stl, .TRUE.)

          ENDIF

          IF(maxlw.GT.NDLW)THEN
            WRITE(6, *)
     &      ' FATAL: INSUFFICIENT NUMBER OF PARTIAL WAVES ALLOWED'
            WRITE(6, *)
     &      ' FATAL: INCREASE NDLW IN dimension.h UP TO', maxlw + 1
            WRITE(6, *)
     &      ' FATAL: AND RECOMPILE THE CODE'
            STOP ' FATAL: INSUFFICIENT NUMBER OF PARTIAL WAVES ALLOWED'
          ENDIF

C---------IWARN=0 - 'NO Warnings'
C---------IWARN=1 - 'A out of the recommended range '
C---------IWARN=2 - 'Z out of the recommended range '
C---------IWARN=3 - 'Energy requested lower than recommended for this potential'
C---------IWARN=4 - 'Energy requested higher than recommended for this potential'
          IF(IWArn.EQ.1 .AND. FIRst_ein)WRITE(6, *)
     &      ' WARNING: OMP not recommended for A=', A(Ntrg)
          IF(IWArn.EQ.2 .AND. FIRst_ein)WRITE(6, *)
     &      ' WARNING: OMP not recommended for Z=', Z(Ntrg)
          IF(IWArn.EQ.3 .OR. IWArn.EQ.4)WRITE(6, *)
     &      ' WARNING: OMP not recommended for E=', EINl
          IWArn = 0
C
        ELSEIF(KTRlom(Npro, Ntrg).EQ.0) THEN
C----------calculation of h.i. transmission coefficients for fusion
          CALL HITL(stl)
        ENDIF

      ENDIF ! END of FUSREAD block

C
C     Moving incident channels results to TLs directory
C
      IF(IOPsys.EQ.0)THEN
C       LINUX
        ctmp = 'mv INCIDENT.CS '//cTLdir//ctmp18//'.CS'
        iwin = PIPE(ctmp)
        IF(DIRECT.GT.0) THEN
          ctmp = 'mv INCIDENT.ICS '//cTLdir//ctmp18//'.ICS'
          iwin = PIPE(ctmp)
        ENDIF
        ctmp = 'mv INCIDENT.ANG '//cTLdir//ctmp18//'.ANG'
        iwin = PIPE(ctmp)
        ctmp = 'mv INCIDENT.TLJ '//cTLdir//ctmp18//'.TLJ'
        iwin = PIPE(ctmp)
      ELSE
C       WINDOWS
        ctmp = 'move INCIDENT.CS ..\\TL\\'//ctmp18//'.CS >NUL'
        iwin = PIPE(ctmp)
        IF(DIRECT.GT.0) THEN
          ctmp = 'move INCIDENT.ICS ..\\TL\\'//ctmp18//'.ICS >NUL'
          iwin = PIPE(ctmp)
        ENDIF
        ctmp = 'move INCIDENT.ANG ..\\TL\\'//ctmp18//'.ANG >NUL'
        iwin = PIPE(ctmp)
        ctmp = 'move INCIDENT.TLJ ..\\TL\\'//ctmp18//'.TLJ >NUL'
        iwin = PIPE(ctmp)
      ENDIF
C
C     Save TLs, SINl
C
C-----Storing transmission coefficients for the incident channel
      if(IOUT.EQ.5) then
        OPEN(46,file = cTLdir//ctmp18//'_INC.LST')
        WRITE (46,'(A5,I6,E12.6)') 'LMAX:' , maxlw , EINl
        DO l = 0, maxlw
          WRITE (46,*) l,SNGL(stl(l+1))
        ENDDO
        WRITE (46,'(1x,A30,4(e12.6,1x))')
     &        'EL,TOT,REAC,INEL XS:',ELAcs,TOTcs,ABScs,SINl
        CLOSE(46)
      endif

      OPEN(45, FILE = (cTLdir//ctmp18//'_INC.BIN'),
     &                                     FORM = 'UNFORMATTED')
      WRITE(45) maxlw, EINl
      DO l = 0, maxlw
         WRITE(45) stl(l+1)
      ENDDO
      WRITE (45)  ELAcs,TOTcs,ABScs,SINl
      CLOSE(45)

 4000 IF(EINl.LT.0.1D0 .and. ZEjc(NPRo).eq.0) THEN
C
C     Written by V.Avrigeanu, checked by RCN 01/2004
C
         s0 = Stl(1)/(2.0D+00*PI*SQRT(1.0D+06*EINl))
         rp = 1.35*(A(NTRg)**0.333333333)
         r2 = rp*rp
         p1 = (ak2*r2)/(1.0D+00 + ak2*r2)
         s1a = Stl(2)/(2.0D+00*PI*p1*SQRT(1.0D+06*EINl))
C        Corrected scattering radius
         rp = SQRT(ELAcs/(4.0D+00*PI*10.D+00))
         WRITE(6, *)
         WRITE(6, 99005) s0*1D4, s1a*1D4, rp
         WRITE(6, *)
      ENDIF

C--------channel spin min and max
      smin = ABS(SEJc(Npro) - XJLv(LEVtarg, Ntrg))
      smax = SEJc(Npro) + XJLv(LEVtarg, Ntrg)
      mul = smax - smin + 1.0001
      CSFus = 0.0
      DO ip = 1, 2     ! over parity
        DO j = 1, NDLW !over compound nucleus spin
          sum = 0.0
          DO ichsp = 1, mul
            chsp = smin + FLOAT(ichsp - 1)
            lmin = ABS(j - chsp - S1) + 0.0001
            lmax = j + chsp - S1 + 0.0001
            lmin = lmin + 1
            lmax = lmax + 1
            lmax = MIN0(NDLW, lmax)
            lmax = MIN0(maxlw, lmax)
            DO k = lmin, lmax
              sum = sum + PAR(ip, LVP(LEVtarg, 0), k - 1)*stl(k)*DRTl(k)
            ENDDO
          ENDDO
          POP(NEX(1), j, ip, 1) = coef*sum*(FLOAT(2*j+1)-2.0*S1)*FUSred
          CSFus = CSFus + POP(NEX(1), j, ip, 1)
          csmax = DMAX1(POP(NEX(1), j, ip, 1), csmax)
        ENDDO
      ENDDO

      IF(LDBWacalc .AND. CSFus.gt.0.d0 .AND. SINl.gt.0.d0) THEN
        IF (DIRECT.EQ.3) then
          WRITE(6, *)' SOMP TLs normalized to substract',
     &               ' DWBA contribution to collective levels'
        ELSE ! DIRECT=1 or DIRECT=2
          WRITE(6, *)' CC OMP TLs normalized to substract DWBA',
     &               ' contribution to un-coupled levels'
        ENDIF
      ENDIF
      IF(IOUT.EQ.5) THEN
        WRITE(6,*)
        WRITE(6,*) ' CSFus(SUM_Tl)   CSFus + SINl   ABScs(ECIS)'
        WRITE(6,'(1x,3(D12.6,2x))')
     &      SNGL(CSFus),SNGL(CSFus + SINl),SNGL(ABScs)
        WRITE(6,*)
      ENDIF
C
C     Attention MIKE !!!!
C     Needed for all potentials ???????? (check, RCN 01/2005)
C
C     WHENEVER SINl > 0 , CSFus must be increased by SINl amount to
C     obtain fusion cross section, including direct and compound contributions
C     It is not clear to me if SINl is included later in neutron emission
C     In some cases seems to me that it was not included. Please check it !!!

      CSFus = CSFus + SINl

C-----calculation/reading of transmission coefficients for input channel done ------
C
C     Passing stl() to HRTW routine, please note that stl() never contain
C     direct contribution !!!
C
      DO i = 1, NDLW
        ELTl(i) = stl(i)
      ENDDO
C
      DO j = NDLW, 1, -1
         NLW = j
         IF(POP(NEX(1), j, 1, 1)*10000.D0.GT.csmax)GOTO 350
         IF(POP(NEX(1), j, 2, 1)*10000.D0.GT.csmax)GOTO 350
      ENDDO
  350 CONTINUE
C-----the next line can be used to increase the number of partial waves
C-----e.g., to account for a high-spin isomer
      NLW = NLW + 3
C-----check whether NLW is not larger then max spin at which nucleus
C-----is still stable
      IF(NLW.GT.JSTab(1))THEN
         NLW = JSTab(1)
         IF(IOUt.GT.0)THEN
            WRITE(6, '('' Maximum spin to preserve stability is'',I4)')
     &            JSTab(1)
            WRITE(6,
     &            '('' Calculations will be truncated at this limit'')')
            WRITE(6,
     &            '('' part of the fusion cross section will be lost'')'
     &            )
         ENDIF
         DO j = NLW + 1, NDLW
            CSFus = CSFus - POP(NEX(1), j, 1, 1) - POP(NEX(1), j, 2, 1)
            POP(NEX(1), j, 1, 1) = 0.0
            POP(NEX(1), j, 2, 1) = 0.0
         ENDDO
         RETURN
      ENDIF

      IF((POP(NEX(1),NLW,1,1)*20.D0.GT.csmax .OR. POP(NEX(1),NLW,2,1)
     &   *20.D0.GT.csmax) .AND. NLW.EQ.NDLW)THEN
         WRITE(6, *)'POP1=', POP(NEX(1), NLW, 1, 1), 'POP2=',
     &              POP(NEX(1), NLW, 2, 1), 'NLW=', NLW
         WRITE(6,
     &'('' NUMBER OF PARTIAL WAVES FOR WHICH CODE IS DIMENSIONE'',
     &''D IS INSUFFICIENT'',/,'' INCREASE NDLW IN THE dimensio'',
     &''n.h FILE AND RECOMPILE  '',/,'' EXECUTION  S T O P P E '',
     &''D '')')
         STOP
      ENDIF

      RETURN
99005    FORMAT(/
     &   '  Strength functions S0 =',f6.3,' eV**(-1/2)*10**(-4)' /,
     &   '                     S1 =',f6.3,' eV**(-1/2)*10**(-4)'  /,
     &           '  Scattering radius =', f7.3, ' fm')
      END
C
C
C
      SUBROUTINE BASS(Ein, Zp, Ap, Zt, At, Bfus, E1, Crl, Csfus)
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
Ccc   * authors:A.D'Arrigo, M.Herman, A.Taccone                           *
Ccc   * date:     .Jul.1991
Ccc   * addapted      by:M.Herman                 on:18.Feb.1993
Ccc   * revision:#    by:name                     on:xx.mon.199x
Ccc   *                   D I S A B L E D
Ccc   *********************************************************************
Ccc
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
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
C
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
      IF(arg.GT.74.D0)arg = 74.
      Bfus = Zp*Zt*e2/R12*(R12/(R12 + dfu) - d/(x*R12)*EXP((-arg)))
      IF(Ein.GT.Bfus)THEN
         Crl = SQRT((2.*MI*R12**2/ht**2)*(Ein - vc + vn))
         vmm = 0.0
         vmax = 10000.0
         IF(Ein.LT.E1)THEN
            jl = INT(le1)
            DO j = 1, jl
               Crl = jl - j + 1
               vmm = vmax
               CALL FINDA(Zp, Zt, Crl, vmax)
               IF(vmax.LE.Ein)GOTO 50
            ENDDO
         ENDIF
 50      IF(Ein.GT.ee2)Crl = le2
         IF(Ein.LT.E1)Crl = Crl + (Ein - vmax)/(vmm - vmax)
      ELSE
         WRITE(6, '(1X,''Incident energy below fusion barrier'')')
         STOP
      ENDIF
      Csfus = 657.*(Ap + At)*Crl**2/(Ap*At*Ein)
      END
C
      SUBROUTINE FINDA(Zp, Zt, Crl, Vm)
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
Ccc * author: D'Arrigo                                                 *
Ccc * date:     .Jul.1991                                              *
Ccc * addapted      by:M.Herman                 on:18.Feb.1993         *
Ccc * revision:#    by:name                     on:xx.mon.199x         *
Ccc *                                                                  *
Ccc *                                                                  *
Ccc ********************************************************************
Ccc
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
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
C
      DATA e2, ht/1.44, 6.589/
      DATA eps/0.001/
      nr = 0
      xn = R12
      xp = 3*R12
 100  xm = (xn + xp)/2
      nr = nr + 1
      IF(nr.LE.50)THEN
         IF(ABS((-Zp*Zt*e2/xm**2) + ((-ht**2*Crl**2/(MI*xm**3)))
     &      + R1*R2/R12*(0.03/3.3*EXP((xm-R12)/3.3)
     &      +0.0061/0.65*EXP((xm-R12)/0.65))
     &      /(0.03*EXP((xm-R12)/3.3)+0.0061*EXP((xm-R12)/0.65))**2)
     &      .GT.eps)THEN
            IF(((-Zp*Zt*e2/xm**2)) + ((-ht**2*Crl**2/(MI*xm**3)))
     &         + R1*R2/R12*(0.03/3.3*EXP((xm-R12)/3.3)
     &         + 0.0061/0.65*EXP((xm-R12)/0.65))
     &         /(0.03*EXP((xm-R12)/3.3) + 0.0061*EXP((xm-R12)/0.65))
     &         **2.LT.0.D0)THEN
               xp = xm
               GOTO 100
            ELSEIF(((-Zp*Zt*e2/xm**2)) + ((-ht**2*Crl**2/(MI*xm**3)))
     &             + R1*R2/R12*(0.03/3.3*EXP((xm-R12)/3.3)
     &             + 0.0061/0.65*EXP((xm-R12)/0.65))
     &             /(0.03*EXP((xm-R12)/3.3) + 0.0061*EXP((xm-R12)/0.65))
     &             **2.NE.0.D0)THEN
               xn = xm
               GOTO 100
            ENDIF
         ENDIF
      ENDIF
      Vm = Zp*Zt*e2/xm + ht**2*Crl**2/(2*MI*xm**2)
     &     - R1*R2/R12/(0.03*EXP((xm-R12)/3.3)
     &     + 0.0061*EXP((xm-R12)/0.65))
      IF(nr.GT.50)WRITE(6, '(10X,''MAX NO. OF ITERATIONS IN FINDA'')')
      END
C
      DOUBLE PRECISION FUNCTION XFUS(Ein, Ap, At, D, Crl)
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
Ccc * author: M.Herman                                                 *
Ccc * date:     .Jul.1991                                              *
Ccc * revision:#    by:name                     on:xx.mon.199x         *
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
C
      sum = 0.0
      icrl = INT(Crl + 5.0*D)
      DO i = 1, icrl
         al = FLOAT(i - 1)
         args = (al - Crl)/D
         IF(args.GT.74.D0)args = 74.
         tl = 1./(1. + EXP(args))
         sum = (2.*al + 1.)*tl + sum
      ENDDO
      XFUS = 657.*(Ap + At)/(Ap*At*Ein)*sum
      END
C
C
      SUBROUTINE PUSH(Ecm, A, Ap, At, Bas, Expush, Sigi, Trunc, Stl,
     &                Nlw, Ndlw)
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
Ccc * author: M.Herman                                                 *
Ccc * date:   about 1992                                               *
Ccc * revision:#    by:name                     on:xx.mon.199x         *
Ccc *                                                                  *
Ccc *                                                                  *
Ccc ********************************************************************
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
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
C
      DATA r0/1.07/
      E = Ecm
      SIG = Sigi
      BAVe = Bas + Expush
      xlow = MAX(BAVe - Trunc*SIG, 0.D0)
      xmax = BAVe + Trunc*SIG
      CALL INTGRS(xlow, xmax, F, dintf)
      amu = At*Ap/A
      rf = r0*(At**0.3333 + Ap**0.3333)
      DO j = 1, Ndlw
         EROt = (j - 1)*j*20.79259/(amu*rf**2)
         EROt = EROt/2.0
         CALL INTGRS(xlow, xmax, G, prob)
         Stl(j) = prob/dintf
         IF(Stl(j).NE.0.0D0)Nlw = j
      ENDDO
      END
C
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
C
      F = EXP(( - (BAVe-X)/(2.0*SIG**2))**2)
      END
C
      DOUBLE PRECISION FUNCTION G(X)
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C Capote 2001, added common variables
C
C COMMON variables
C
      DOUBLE PRECISION BAVe, E, EROt, SIG
      COMMON /EXTRAP/ BAVe, EROt, E, SIG
C
C
C Local variables
C
      DOUBLE PRECISION arg, htom, pi
      DOUBLE PRECISION F
      EXTERNAL F
C
      DATA pi, htom/3.14159D0, 4.D0/
      arg = -2.*pi*(E - X - EROt)/htom
      IF(arg.LT.( - 74.D0))G = F(X)
      IF(arg.GT.74.D0)G = 0.
      IF(ABS(arg).LE.74.D0)G = F(X)/(1 + EXP((-2.*pi*(E-X-EROt)/htom)))
      END

      SUBROUTINE PROCESS_ECIS(IOPsys,outname,length,iret)
      character*(*) outname
      INTEGER length,iret,IOPsys
C     Locals
      INTEGER*4 IWIN, PIPE
      character*132 ctmp

      IF(IOPsys.EQ.0)THEN
C       LINUX
        ctmp = 'cp ecis03.cs  '//outname(1:length)//'.CS '
        iwin = PIPE(ctmp)
        if(iret.eq.1) return
        ctmp = 'cp ecis03.tlj '//outname(1:length)//'.TLJ'
        iwin = PIPE(ctmp)
        if(iret.eq.2) return
        ctmp = 'cp ecis03.ang '//outname(1:length)//'.ANG'
        iwin = PIPE(ctmp)
        if(iret.eq.3) return
        ctmp = 'cp ecis03.ics '//outname(1:length)//'.ICS'
        iwin = PIPE(ctmp)
      ELSE
C       WINDOWS
        ctmp = 'copy ecis03.cs  '//outname(1:length)//'.CS  >NUL'
        iwin = PIPE(ctmp)
        if(iret.eq.1) return
        ctmp = 'copy ecis03.tlj '//outname(1:length)//'.TLJ >NUL'
        iwin = PIPE(ctmp)
        if(iret.eq.2) return
        ctmp = 'copy ecis03.ang '//outname(1:length)//'.ANG >NUL'
        iwin = PIPE(ctmp)
        if(iret.eq.3) return
        ctmp = 'copy ecis03.ics '//outname(1:length)//'.ICS >NUL'
        iwin = PIPE(ctmp)
      ENDIF
      RETURN
      END

