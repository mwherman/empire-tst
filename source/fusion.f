Ccc   * $Author: Capote $
Ccc   * $Date: 2005-01-25 12:59:27 $
Ccc   * $Id: fusion.f,v 1.24 2005-01-25 12:59:27 Capote Exp $
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
C
C Dummy arguments
C
      INTEGER Npro, Ntrg
C
C Local variables
C
      DOUBLE PRECISION chsp, coef, csmax, csvalue, dtmp, PI, smax,
     &                 smin, stl(NDLW), sum, wf, ParcnJ, cnJ
      DOUBLE PRECISION DMAX1
      REAL FLOAT
      INTEGER i, ichsp, ip, ipa, j, k, l, lmax, lmin, maxlw, mul
      INTEGER MIN0
      DOUBLE PRECISION PAR, W2, xmas_npro, xmas_ntrg, RMU
      INTEGER itmp1
      CHARACTER*1 ctmp1, ctmp2
      DOUBLE PRECISION stmp1, stmp2
      CHARACTER*132 ctmp
      CHARACTER*80 rstring
      INTEGER*4 iwin
      INTEGER*4 PIPE

      PAR(i, ipa, l) = 0.5*(1.0 - ( - 1.0)**i*ipa*( - 1.0)**l)
C
C-----Reduced mass corrected for proper mass values
C
      xmas_npro = (AEJc(Npro)*AMUmev + XMAss_ej(Npro))/AMUmev
      xmas_ntrg = (A(Ntrg)*AMUmev + XMAss(Ntrg))/AMUmev
      el = EINl
      CALL KINEMA(el, ecms, xmas_npro, xmas_ntrg, RMU, ak2, 1, RELkin)
C
C     wf = W2*EIN*rmu  being EIN the CM energy
C
      wf = ak2/10.D0
C
C  Zero qd fraction of photabsorption before it can do any damage
C
      QDfrac=0.0d0

      IF(INT(AEJC(0)).GT.0)
     & coef = PI/wf/(2*XJLv(LEVtarg, Ntrg) + 1.0)/(2*SEJc(Npro) + 1.0)
      S1 = 0.5
      maxlw = NDLW
      IF(AINT(XJLv(LEVtarg,Ntrg) + SEJc(Npro)) - XJLv(LEVtarg, Ntrg) -
     & SEJc(Npro).EQ.0.0D0) S1 = 1.0

      csmax = 0.0
      CSFus = 0.0
      DO i = 1, NDLW
         stl(i) = 0.0
      ENDDO

C-----Calculation of fusion cross section for photon induced reactions
      IF(INT(AEJC(0)).EQ.0)THEN

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

          CSFus = 0.0
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
     &          /(2*XJLv(LEVtarg, Ntrg)+1)
           QDTmp=SIGQD(Z(Ntrg),A(Ntrg),EINl,Lqdfac)/3.0d0
C----------do loop over parity
           DO ip = 1, 2
C           Quasideuteron contribution QDTmp by Carlson
            WPARG=PAR(ip, LVP(LEVtarg, 0), 1)*(E1Tmp + QDTmp)
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
C                     QDTmp=SIGQD(Z(Ntrg),A(Ntrg),EINl,Lqdfac)/3.0d0
        IF(IGE1.NE.0) QDfrac= 3.d0*QDTmp/CSFus

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
        WRITE(6, *)
     &  ' Spin distribution of fusion cross section read from the file '
        WRITE(6, *)
     &          ' (all previous instructions concerning fusion ignored)'

      ELSE

C-------calculation of o.m. transmission coefficients for absorption
        IF(KTRlom(Npro, Ntrg).GT.0)THEN
C
           einlab = -EINl
           IWArn = 0

           IF(DIRect.GT.0) THEN

              IF(KTRompcc.GT.0)THEN
C---------------Saving KTRlom(0,0)
                itmp1 = KTRlom(0, 0)
                KTRlom(0, 0) = KTRompcc
              ENDIF
              CCCalc = .FALSE.
              IF(DIRect.NE.2) CCCalc = .TRUE.

              IF(.NOT.DEFORMED) THEN

                CALL ECIS_CCVIB(Npro,Ntrg,einlab,.TRUE.,1)

                IF(DIRECT.NE.3) THEN

                  IF(IOPsys.EQ.0)THEN
C                   LINUX
                    ctmp = 'cp ecis03.cs dwba.cs'
                    iwin = PIPE(ctmp)
                    ctmp = 'mv ecis03.ang dwba.ang'
                    iwin = PIPE(ctmp)
                    ctmp = 'mv ecis03.ics dwba.ics'
                    iwin = PIPE(ctmp)
C                   ctmp = 'mv ecis03.pol dwba.pol'
C                   iwin = PIPE(ctmp)
                  ELSE
C                   WINDOWS
                    ctmp = 'copy ecis03.cs dwba.cs>NUL'
                    iwin = PIPE(ctmp)
                    ctmp = 'move ecis03.ang dwba.ang>NUL'
                    iwin = PIPE(ctmp)
                    ctmp = 'move ecis03.ics dwba.ics>NUL'
                    iwin = PIPE(ctmp)
C                   ctmp = 'moveecis03.pol dwba.pol>NUL'
C                   iwin = PIPE(ctmp)
                  ENDIF

                ENDIF

              ENDIF

              IF(DIRect.LE.2 .AND. AEJc(Npro).LE.1) THEN
C--------------Target nucleus (elastic channel), incident neutron or proton

               WRITE(6, *)' CC transmission coefficients used for ',
     &                 'fusion determination'
C--------------Transmission coefficient matrix for incident channel
C--------------is calculated (DIRECT = 2 (CCM)) using ECIS code.
C--------------Preparing INPUT and RUNNING ECIS
C--------------(or reading already calculated file)
               IF(DEFormed)THEN

                 CALL ECIS_CCVIBROT(Npro, Ntrg, einlab,.TRUE., 0)
                 CALL ECIS2EMPIRE_TL_TRG(Npro, Ntrg, maxlw, stl,.FALSE.)

               ELSE

                 CALL ECIS_CCVIB(Npro, Ntrg, einlab, .FALSE., -1)

                 IF(IOPsys.EQ.0)THEN
C                  LINUX
                   ctmp = 'cp ecis03.cs ccm.cs'
                   iwin = PIPE(ctmp)
                   ctmp = 'mv ecis03.ang ccm.ang'
                   iwin = PIPE(ctmp)
                   ctmp = 'mv ecis03.ics ccm.ics'
                   iwin = PIPE(ctmp)
C                  ctmp = 'mv ecis03.pol ccm.pol'
C                  iwin = PIPE(ctmp)
                 ELSE
C                  WINDOWS
                   iwin = PIPE('copy ecis03.cs ccm.cs>NUL')
                   iwin = PIPE('move ecis03.ics ccm.ics>NUL')
                   iwin = PIPE('move ecis03.ang ccm.ang>NUL')
C                  iwin = PIPE('move ecis03.pol ccm.pol>NUL')
                 ENDIF
C
C                Joining both DWBA and CCM files
C                total, elastic and reaction cross section is from CCM
C
C                inelastic cross section
                 OPEN(45, FILE = 'dwba.ics', STATUS = 'OLD', ERR=1000)
                 OPEN(46, FILE = 'ccm.ics' , STATUS = 'OLD')
                 OPEN(47, FILE = 'ecis03.ics' , STATUS = 'UNKNOWN')
                 READ(45, '(A80)', END = 1000) rstring
                 READ(46, '(A80)', END=990) ! first line is taken from dwba
  990            write(47,'(A80)') rstring
                 DO i = 2, ND_nlv
                   READ(45, '(A80)', END = 1000) rstring
                   READ(46,'(A80)',END=995) rstring
  995              write(47,'(A80)') rstring
                 ENDDO
 1000            CLOSE(45, STATUS='DELETE')
                 CLOSE(46, STATUS='DELETE')
                 CLOSE(47)
C                angular distribution
                 OPEN(45, FILE = 'dwba.ang', STATUS = 'OLD', ERR=2000)
                 READ(45, '(A80)', END = 2000) rstring
                 OPEN(46, FILE = 'ccm.ang' , STATUS = 'OLD')
                 READ(46, '(A80)', END=1005) ! first line is taken from dwba
 1005            OPEN(47, FILE = 'ecis03.ang' , STATUS = 'UNKNOWN')
                 write(47,'(A80)') rstring
                 DO i = 1, ND_nlv
                   READ(45, '(5x,F5.1,A1,4x,i5)', END = 2000) stmp1,
     &                                                ctmp1,nang
                   READ(46, '(5x,F5.1,A1)', END = 1010) stmp2,ctmp2
C                  checking the correspondence of the excited states
                   IF(stmp1.ne.stmp2 .OR. ctmp1.ne.ctmp2) THEN
                     write(6,*)
     >            ' WARNING: DWBA and CCM state order does not coincide'
                   ENDIF
 1010              BACKSPACE 45
                   READ(45, '(A80)', END = 2000) rstring
                   write(47,'(A80)') rstring
                   DO j = 1, nang
                     READ(45, '(A80)', END = 2000) rstring
                     READ(46,'(A80)' , END = 1015) rstring
 1015                write(47,'(A80)') rstring
                   ENDDO
                 ENDDO
 2000            CLOSE(45, STATUS='DELETE')
                 CLOSE(46, STATUS='DELETE')
                 CLOSE(47)

                 CALL ECIS2EMPIRE_TL_TRG(Npro, Ntrg, maxlw, stl,.TRUE.)

               ENDIF

              ELSE ! DIRECT.GE.2 OR CLUSTER

               WRITE(6, *)' Spherical OM transmission coefficients',
     &                   ' used for fusion determination'
               CALL ECIS_CCVIB(Npro,Ntrg,einlab,.TRUE.,0)
               CALL ECIS2EMPIRE_TL_TRG(Npro, Ntrg, maxlw, stl, .TRUE.)
               IF(.NOT. (MODelecis.EQ.0 .OR. DIRect.EQ.3))
     &           WRITE(6, *)' Fusion cross section normalized',
     &                      ' to coupled channel reaction cross section'
              ENDIF

           ELSE ! DIRECT = 0

             WRITE(6, *)' Spherical OM transmission coefficients',
     &                   ' used for fusion determination'
              CALL ECIS_CCVIB(Npro,Ntrg,einlab,.TRUE.,0)
              CALL ECIS2EMPIRE_TL_TRG(Npro, Ntrg, maxlw, stl, .TRUE.)

           ENDIF
C----------IWARN=0 - 'NO Warnings'
C----------IWARN=1 - 'A out of the recommended range '
C----------IWARN=2 - 'Z out of the recommended range '
C----------IWARN=3 - 'Energy requested lower than recommended for this potential'
C----------IWARN=4 - 'Energy requested higher than recommended for this potential'
           IF(IWArn.EQ.1 .AND. FIRst_ein)WRITE(6, *)
     &      ' WARNING: OMP not recommended for A=', A(Ntrg)
           IF(IWArn.EQ.2 .AND. FIRst_ein)WRITE(6, *)
     &      ' WARNING: OMP not recommended for Z=', Z(Ntrg)
           IF(IWArn.EQ.3 .OR. IWArn.EQ.4)WRITE(6, *)
     &      ' WARNING: OMP not recommended for E=', EINl
           IWArn = 0
C
           IF(maxlw.GT.NDLW)THEN
              WRITE(6, *)
     &       ' WARNING: INSUFFICIENT NUMBER OF PARTIAL WAVES ALLOWED'
              WRITE(6, *)
     &       ' WARNING: INCREASE NDLW IN dimension.h UP TO', maxlw + 1
              WRITE(6, *)
     &       ' WARNING: AND RECOMPILE THE CODE'
              STOP
           ENDIF

        ELSEIF(KTRlom(Npro, Ntrg).EQ.0) THEN
C----------calculation of h.i. transmission coefficients for fusion
           CALL HITL(stl)
        ENDIF

      ENDIF
C-----calculation of transmission coefficients ----done------
      DO i = 1, NDLW
        ELTl(i) = stl(i)
      ENDDO

      smin = ABS(SEJc(Npro) - XJLv(LEVtarg, Ntrg))
      smax = SEJc(Npro) + XJLv(LEVtarg, Ntrg)
      mul = smax - smin + 1.0001
      CSFus = 0.0
C-----do loop over parity
      DO ip = 1, 2
C-------do loop over compound nucleus spin
        DO j = 1, NDLW
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
                  sum = sum + PAR(ip, LVP(LEVtarg, 0), k - 1)*stl(k)*
     &                  DRTl(k)
               ENDDO
          ENDDO
          POP(NEX(1), j, ip, 1) = coef*sum*(FLOAT(2*j + 1) - 2.0*S1)
     &                              *FUSred
          CSFus = CSFus + POP(NEX(1), j, ip, 1)
          csmax = DMAX1(POP(NEX(1), j, ip, 1), csmax)
        ENDDO
      ENDDO

      IF(DIRect.GT.0 .AND. AEJc(Npro).LE.1) THEN

         ecis_abs = 0.
C--------read ECIS03 absorption cross section
         OPEN(45, FILE = 'ecis03.cs', STATUS = 'OLD')
         READ(45, *, END = 150)  ! Skipping first line <CROSS.S>
         IF(ZEJc(0).eq.0) READ(45, *, END = 150)totcs
         READ(45, *, END = 150)ecis_abs
 150     CLOSE(45)
         SINl = 0.d0
         OPEN(UNIT = 45, FILE = 'ecis03.ics', STATUS = 'old', ERR = 200)
         READ(45, *, END = 200)
         DO l = 1, NDCollev
            READ(45, *, END = 200)dtmp
            SINl = SINl + dtmp
         ENDDO
 200     CLOSE(45)
C
         IF(SINl.GT.ecis_abs)THEN
            WRITE(6, *)
     &       ' WARNING: LOOK LONG OUTPUT NON-CONVERGENCE !!'
            WRITE(6,
     &'(///                                                       5x,''*
     &*************************************************'')')
            WRITE(6,
     &     '(5x,'' Direct cross section calculation do not converge '')'
     &     )
            WRITE(6,
     &'(6x,''Inelastic cross section ='',F8.2,'' mb''/
     &  6x,''Reaction  cross section ='',F8.2,'' mb''/)')SINl, ecis_abs
            WRITE(6,
     &     '(5x,'' Either change OMP or change calculation method   '')'
     &     )
            WRITE(6,
     &     '(5x,''        (DIRPOT)   or   (DIRECT) parameters       '')'
     &     )
            WRITE(6,
     &     '(5x,'' This problem usually happens using DWBA method   '')'
     &     )
            WRITE(6,
     &     '(5x,'' to treat strong coupled nuclei                   '')'
     &     )
            WRITE(6,
     &     '(5x,''            CALCULATION STOPPED                   '')'
     &     )
            WRITE(6,
     &     '(5x,''**************************************************'')'
     &     )
            STOP 200
         ENDIF

C--------Renormalizing Tls
         IF(MODelecis.GT.0 .AND. DIRect.EQ.1)THEN
C-----------for CC OMP renormalizing to reaction XS calculated by ECIS
            DO l = 1, maxlw
               stl(l) = stl(l)*(ecis_abs - SINl)/CSFus
               ELTl(l) = stl(l)
            ENDDO
         ELSE
C-----------for SOMP including inelastic reaction XS (from ECIS)
C-----------in the calculated reaction XS
            DO l = 1, maxlw
               stl(l) = stl(l)*(CSFus - SINl)/CSFus
               ELTl(l) = stl(l)
            ENDDO
         ENDIF

C--------channel spin min and max
         smin = ABS(SEJc(Npro) - XJLv(LEVtarg, Ntrg))
         smax = SEJc(Npro) + XJLv(LEVtarg, Ntrg)
         mul = smax - smin + 1.0001
         CSFus = 0.0
         DO ip = 1, 2 ! over parity
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
                     sum = sum + PAR(ip, LVP(LEVtarg, 0), k - 1)*stl(k)
     &                     *DRTl(k)
                  ENDDO
               ENDDO
               POP(NEX(1), j, ip, 1) = coef*sum*(FLOAT(2*j + 1) - 2.0*S1
     &                                 )*FUSred
               CSFus = CSFus + POP(NEX(1), j, ip, 1)
               csmax = DMAX1(POP(NEX(1), j, ip, 1), csmax)
            ENDDO
         ENDDO

C--------Renormalization of Tls and fusion cros section done for DIRECT.eq.1
C--------add ECIS inelastic to the fusion cross section
C        Only needed for non CC OMP potentials
         IF(DIRect.NE.2) CSFus = CSFus + SINl
C
      ENDIF
C
      DO j = NDLW, 1, -1
         NLW = j
         IF(POP(NEX(1), j, 1, 1)*10000.D0.GT.csmax)GOTO 300
         IF(POP(NEX(1), j, 2, 1)*10000.D0.GT.csmax)GOTO 300
      ENDDO
  300 CONTINUE
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