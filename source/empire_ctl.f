Ccc   * $Rev: 2526 $
Ccc   * $Author: shoblit $
Ccc   * $Date: 2012-02-09 21:34:11 +0100 (Do, 09 Feb 2012) $
 
      PROGRAM EMPIRE_CTL
C
C*** Start of declarations rewritten by SPAG
C
C PARAMETER definitions
C
      INTEGER, PARAMETER :: MXFit = 20
C
C COMMON variables
C
      CHARACTER(64) :: EMPiredir
      CHARACTER(72) :: EMPtitle
      COMMON /GLOBAL_E/ EMPiredir, EMPtitle
C
C Local variables
C
      LOGICAL :: autofit, sensit
      REAL, DIMENSION(MXFit) :: dparmx, pars
      INTEGER :: nnft
      REAL :: xitr
C
C*** End of declarations rewritten by SPAG
C
C
C--- Controls execution of EMPIRE: (i) normal calculations, (ii) omp fitting,
C--- (iii) sensitivity matrix.
C---
C--- The program has been designed to make the fitting routine, LOCALFIT,
C--- as independent as possible of the rest of the code, so that it may
C--- be easily exchanged for another routine. It depends on four input
C--- parameters:
C---   pars    - an array of parameters to be varied in fit
C---   dparmx  - array of max variation allowed in each parameter in vars
C---   nnft    - number of parameters in vars
C---   xitr    - a real constructed of the inner and outer loop iteration
C--- It also uses a function CHISQRD(pars), provided below.
C---
C--- The logical variable autofit is true when fitting, false for a normal run.
C--- The logical variable sensit is true for sensitivity calc., false for a normal run.
C
 
      CALL GETENV('EMPIREDIR',EMPiredir)
 
      EMPtitle = '   '
C
C     The following line defines the proper default for WINDOWS work
C     even if EMPIREDIR is not defined
C
      IF(EMPiredir(1:1).EQ.' ')EMPiredir(1:3) = '../'
      OPEN(UNIT = 8,FILE = 'LIST.DAT',STATUS = 'UNKNOWN')
 
      CALL SCAN4FIT(autofit,pars,dparmx,nnft,xitr,sensit)
      IF(autofit.AND.sensit)THEN
        WRITE(8,*)
     &'ERROR: OMP FIT AND SENSITIVITY CALCULATIONS CAN NOT BE RUN TOGETH
     &ER'
        STOP 'NO OMP FIT TOGETHER WITH KALMAN CALCULATIONS'
      ENDIF
 
      IF(autofit)THEN
        CALL LOCALFIT(pars,dparmx,nnft,xitr)
        CALL CLEANUP(nnft)
        CALL EMPIRE
      ELSEIF(sensit)THEN
        CALL SENSITIVITY
      ELSE
        CALL EMPIRE
      ENDIF
 
      CLOSE(8)
 
      STOP
      END PROGRAM EMPIRE_CTL
 
!---------------------------------------------------------------------------
C
C-------------------------------------------------------------------
C
      SUBROUTINE SCAN4FIT(Autofit,Pars,Dparmx,Nnft,Xitr,Sensit)
C
C*** Start of declarations rewritten by SPAG
C
C PARAMETER definitions
C
      INTEGER, PARAMETER :: MXFit = 20, MXInd = 5000, MXInda = 5000
      REAL, PARAMETER :: DISc = 1.0E4
C
C COMMON variables
C
      REAL, DIMENSION(MXInda) :: ANGs, DSIga, SIGa
      REAL, DIMENSION(MXInd) :: DSIg, SIG
      REAL, DIMENSION(0:MXInd) :: EGRid, EN
      INTEGER, DIMENSION(MXInd) :: ICAla, IDAng, IDNt, NANgd, NANgs, 
     &                             NINts
      INTEGER, DIMENSION(3,MXFit) :: IDV
      INTEGER :: NFIt, NNDe
      REAL :: THS0, WT0
      REAL, DIMENSION(MXFit) :: VALs, XVAls
      REAL, DIMENSION(15,2) :: WT
      COMMON /EXPTLDAT/ EN, SIG, DSIg, ANGs, SIGa, DSIga, EGRid, WT0, 
     &                  THS0, NINts, NANgd, NANgs, ICAla, IDNt, IDAng, 
     &                  NNDe
      COMMON /FITPARS/ VALs, XVAls, IDV, NFIt
      COMMON /FITWTS/ WT
C
C Dummy arguments
C
      LOGICAL :: Autofit, Sensit
      INTEGER :: Nnft
      REAL :: Xitr
      REAL, DIMENSION(MXFit) :: Dparmx, Pars
C
C Local variables
C
      REAL*8 :: aap, aat, culbar, eclmn, emax, emin, zzp, zzt
      REAL :: axt, val, xvalmx
      REAL, DIMENSION(MXFit) :: axx, valx, xvalx
      CHARACTER(35) :: cmnd, cmndp, cmndt
      CHARACTER(132) :: ctmp
      LOGICAL :: fexist, linux
      INTEGER :: i, i1, i2, i3, iaa, ichng0, idef, imx, izz, j, j1, k, 
     &           n, ngrid, nwt
      INTEGER, DIMENSION(2,30) :: idw
      INTEGER :: INT
      INTEGER, DIMENSION(6) :: ipotrng
      INTEGER, DIMENSION(MXFit) :: ipt
      INTEGER*4 :: itmp
      INTEGER*4 :: PIPE
      CHARACTER(2), DIMENSION(6) :: pot1
      CHARACTER(1), DIMENSION(3) :: pot2
      REAL, DIMENSION(30) :: wtx
C
C*** End of declarations rewritten by SPAG
C
C
C--- Scans INPUT.DAT for automatic omp fit request (FITOMP=2) and the corresponding
C--- parameters. If a request is found, the fit parameters are analyzed, the
C--- C4 data file is read to prepare experimental data and an appropriate
C--- input file (FITIN.DAT) is prepared.
C--- Scans also INPUT.DAT for the request for sensitivity matrix calculations.
C--- If none of the requests is found, control is returned to EMPIRE, which then
C--- runs normally.
C--- Values returned by scan4fit:
C---   autofit - logical variable, true for fit, false for normal run
C---   sensit - logical variable, true for sensitivity calculations, false for normal run
C---   pars    - an array of parameters to be varied in fit
C---   dparmx  - array of max variation allowed in each parameter in vars
C---   nnft    - number of parameters in vars
C
 
 
 
      DATA emax/30.0/
      DATA pot1/'RV', 'IV', 'RS', 'IS', 'RO', 'IO'/
      DATA pot2/'R', 'D', 'V'/
 
      Autofit = .FALSE.
      Sensit = .FALSE.
      linux = .TRUE.
      WT0 = 1.0
      Xitr = 3.05
 
      EGRid(0) = -1.1
      NFIt = 0
      nwt = 0
 
      OPEN(UNIT = 5,FILE = 'INPUT.DAT',STATUS = 'OLD')
      OPEN(UNIT = 18,FILE = 'FITIN.DAT',STATUS = 'UNKNOWN')
      OPEN(UNIT = 72,FILE = 'INPUT1.DAT',STATUS = 'UNKNOWN')
 
C--- The energy on the first line of input is taken to be the minimum
C--- incident energy of data included in the fit.
      READ(5,'(a35)')cmnd
      READ(cmnd,*)emin
      WRITE(72,'(a35)')cmnd
 
C--- The projectile and target mass and charges are needed to obtain the
C--- neutron s-wave scattering length, in the case of a neutron projectile,
C--- and to estimate the minium incident energy, in the case of a proton
C--- projectile.
      READ(5,'(a35)')cmndt
      READ(cmndt,*)aat, zzt
      WRITE(72,'(a35)')cmndt
 
      READ(5,'(a35)')cmndp
      READ(cmndp,*)aap, zzp
      WRITE(72,'(a35)')cmndp
 
C--- With Zp, Zt and At, the Coulomb barrier and the lowest energy used in
C--- the fit energy mesh can be estimated. For protons, the lowest energy is
C--- taken to about 0.6 to 0.8 of the Coulomb barrier, for neutrons 1 keV.
      culbar = 1.44*zzp*zzt/(3.75 + 1.25*aat**(1./3.))
      eclmn = MAX(culbar - 0.75 - 0.5*(2*zzp - aap) - 0.01*zzt,
     &        0.6*culbar)
      eclmn = INT(DISc*eclmn + 0.5)/DISc
      emin = INT(DISc*emin + 0.5)/DISc
      emin = MAX(0.001D0,emin,eclmn)
      EGRid(1) = emin
      ngrid = 1
 
      WRITE(18,'(f7.3)')EGRid(1)
      WRITE(18,'(a35)')cmndt
      WRITE(18,'(a35)')cmndp
 
      DO i = 1, 7
        READ(5,'(a35)')cmnd
        WRITE(72,'(a35)')cmnd
      ENDDO
 
C--- Prepare (or not) for search for neutron s-wave scattering length
C--- as well as prepare neutron or proton fit input or jump out.
      IF(INT(aap + 0.1).EQ.1.AND.INT(zzp + 0.1).EQ.0)THEN
        WRITE(18,'(i3)')1
        WRITE(18,'(i3)')0
        WRITE(18,'(i3)')0
        WRITE(18,'(i3)')0
        WRITE(18,'(i3)')0
        WRITE(18,'(i3)')0
      ELSEIF(INT(aap + 0.1).EQ.1.AND.INT(zzp + 0.1).EQ.1)THEN
        WRITE(18,'(i3)')0
        WRITE(18,'(i3)')1
        WRITE(18,'(i3)')0
        WRITE(18,'(i3)')0
        WRITE(18,'(i3)')0
        WRITE(18,'(i3)')0
        aat = 0.
C       else if(int(aap+0.1).eq.4 .and. int(zzp+0.1).eq.2) then
C        close(5)
C        close(18,status='delete')
C        close(72,status='delete')
C        return
C        write(18,'(i3)') 0
C        write(18,'(i3)') 0
C        write(18,'(i3)') 1
C        write(18,'(i3)') 0
C        write(18,'(i3)') 0
C        write(18,'(i3)') 0
C        aat=0.
C       else if(int(aap+0.1).eq.2 .and. int(zzp+0.1).eq.1) then
C        close(5)
C        close(18,status='delete')
C        close(72,status='delete')
C        return
C        write(18,'(i3)') 0
C        write(18,'(i3)') 0
C        write(18,'(i3)') 0
C        write(18,'(i3)') 1
C        write(18,'(i3)') 0
C        write(18,'(i3)') 0
C        aat=0.
C       else if(int(aap+0.1).eq.3 .and. int(zzp+0.1).eq.1) then
C        close(5)
C        close(18,status='delete')
C        close(72,status='delete')
C        return
C        write(18,'(i3)') 0
C        write(18,'(i3)') 0
C        write(18,'(i3)') 0
C        write(18,'(i3)') 0
C        write(18,'(i3)') 1
C        write(18,'(i3)') 0
C        aat=0.
C       else if(int(aap+0.1).eq.3 .and. int(zzp+0.1).eq.2) then
C        close(5)
C        close(18,status='delete')
C        close(72,status='delete')
C        return
C        write(18,'(i3)') 0
C        write(18,'(i3)') 0
C        write(18,'(i3)') 0
C        write(18,'(i3)') 0
C        write(18,'(i3)') 0
C        write(18,'(i3)') 1
C        aat=0.
      ELSE
        CLOSE(5)
        CLOSE(18,STATUS = 'delete')
        CLOSE(72,STATUS = 'delete')
        RETURN
      ENDIF
      WRITE(18,'(i3,2f3.0)')0, 0., 0.
 
      izz = zzt + 0.1
      iaa = aat + 0.1
 
   10 READ(5,'(a35)',END = 30)cmnd
 
C--- Check whether sensitivity matrix calculations are requested
      IF(cmnd(1:6).EQ.'KALMAN')THEN
        READ(cmnd,'(6x,g10.5,4i5)')val
        IF(val.GT.0)THEN
          Sensit = .TRUE.
          RETURN
        ENDIF
      ENDIF
 
C--- The fundamental fit command - automatic fitting will be attempted
C--- if its value is 2 or larger.
      IF(cmnd(1:6).EQ.'FITOMP')THEN
        READ(cmnd,'(6x,g10.5,4i5)')val
        IF(val.GT.1.)THEN
          Autofit = .TRUE.
          WRITE(18,'(a6,f10.5)')cmnd(1:6), -1.0
          WRITE(72,'(a6,f10.5)')cmnd(1:6), 1.0
          GOTO 10
        ENDIF
      ENDIF
 
C--- Generic command for varying parameters. Parameters must
C--- be specifies by i1 - RIPL potential number (1-6), i2 - potential
C--- parameter number (r=1,d=2,v=3), and i3 - postion in RIPL file (1-13 for
C--- r and d, 1-24 for v) or by i1=7 (deformation) and i2 - multipolarity.
      IF(cmnd(1:6).EQ.'FITPAR')THEN
        NFIt = MIN(NFIt + 1,MXFit - 1)
        READ(cmnd,'(6x,g10.5,4i5)')valx(NFIt), imx, i1, i2, i3
        xvalx(NFIt) = 0.01*imx
        axx(NFIt) = 1000*i1 + 100*i2 + 13
        GOTO 10
      ENDIF
 
C--- Keyword for varying deformations. At the moment l=2 and 4 are allowed
C--- for rotational excitations, l=2 and 3 for vibrational ones.
      IF(cmnd(1:6).EQ.'FITDEF')THEN
        NFIt = MIN(NFIt + 1,MXFit - 1)
        READ(cmnd,'(6x,g10.5,4i5)')valx(NFIt), imx, i1
        xvalx(NFIt) = 0.01*imx
        axx(NFIt) = 7000 + 100*i1
        GOTO 10
      ENDIF
 
C--- Command for varying weights of specific types of data. idw(1,.) and
C--- idw(2,.) are specified by the corresponding MF and MT numbers.
      IF(cmnd(1:6).EQ.'FITWT ')THEN
        nwt = nwt + 1
        READ(cmnd,'(6x,g10.5,4i5)')wtx(nwt), (idw(j,nwt),j = 1,2)
        IF(wtx(nwt).LT.0.0)wtx(nwt) = 0.0
        GOTO 10
      ENDIF
 
C--- Command for varying the weight of natural element data contained in
C--- the C4 file.
      IF(cmnd(1:6).EQ.'FITWT0')THEN
        nwt = nwt + 1
        READ(cmnd,'(6x,g10.5,4i5)')WT0
        IF(WT0.LT.1.0E-12)THEN
          WT0 = 1.0E6
        ELSE
          WT0 = 1.0/SQRT(WT0)
        ENDIF
        GOTO 10
      ENDIF
 
C--- Command for varying the number of iterations in localfit.
C--- Xitr consists of mxitr+itmax/100
      IF(cmnd(1:6).EQ.'FITITR')THEN
        READ(cmnd,'(6x,g10.5,4i5)')Xitr
        GOTO 10
      ENDIF
 
C--- Command for varying the maximum incident energy of experimental data
C--- used in the fit. The default value is 30 MeV.
      IF(cmnd(1:6).EQ.'FITEMX')THEN
        nwt = nwt + 1
        READ(cmnd,'(6x,g10.5,4i5)')emax
        IF(emax.LT.emin)emax = 30.
        GOTO 10
      ENDIF
 
C--- Command for specifying mesh of incident energies at which EMPIRE
C--- will calculate cross sections for subsequent interpolation of
C--- integrated cross sections in CHISQRD. If specified, the INTERVALS between
C--- energies, from emin, are egrid(2), egrid(2)+0.001*i1,egrid(2)+0.002*i1,...
C--- If not specified, the energies after the GO line are used.
      IF(cmnd(1:6).EQ.'FITGRD')THEN
        READ(cmnd,'(6x,g10.5,4i5)')EGRid(2), i1
        EGRid(0) = 1.1
        IF(EGRid(2).LT.1.0/DISc)THEN
          EGRid(2) = 0.1
          EGRid(3) = 0.02
        ELSE
          EGRid(2) = INT(DISc*EGRid(2) + 0.5)/DISc
          EGRid(3) = 0.001*i1
        ENDIF
        GOTO 10
      ENDIF
 
C--- Last look for FIT keywords -- the code can't look for more after this
C--- Looks for the various potential parameter options, FITRVR, FITRVD, etc.
C--- The parameter i3 corresponds to the RIPL position of the parameter
C--- (1-13 for r and d, 1-24 for v).
      IF(cmnd(1:3).EQ.'FIT')THEN
        DO i1 = 1, 6
          IF(cmnd(4:5).EQ.pot1(i1))GOTO 15
        ENDDO
        WRITE(18,'(a35)')cmnd
        GOTO 10
   15   DO i2 = 1, 3
          IF(cmnd(6:6).EQ.pot2(i2))GOTO 20
        ENDDO
        WRITE(18,'(a35)')cmnd
        GOTO 10
   20   NFIt = MIN(NFIt + 1,MXFit - 1)
        READ(cmnd,'(6x,g10.5,4i5)')valx(NFIt), imx, i3
        xvalx(NFIt) = 0.01*imx
        axx(NFIt) = 1000*i1 + 100*i2 + i3
        GOTO 10
      ENDIF
 
C--- When GO is found, the program either exits or goes on to prepare
C--- parameters and data for the fit.
      IF(cmnd(1:6).EQ.'GO    ')THEN
        IF(Autofit)THEN
          WRITE(18,'(a35)')cmnd
          WRITE(72,'(a35)')cmnd
          DO i = 1, 1000
            READ(5,'(a35)',END = 25)cmnd
            WRITE(72,'(a35)')cmnd
C--- Read the energy mesh from the lines after GO, if it is not specified
C--- by FITGRD.
            IF(EGRid(0).LT.0.AND.cmnd(1:1).NE.'$')THEN
              ngrid = ngrid + 1
              READ(cmnd,*)EGRid(ngrid)
              IF(EGRid(ngrid).LT.0.)EGRid(0) = ngrid - 0.9
              EGRid(ngrid) = INT(DISc*EGRid(ngrid) + 0.5)/DISc
            ENDIF
          ENDDO
          IF(EGRid(0).LT.1.5)THEN
            WRITE(2,*)' WARNING! Only one point in energy grid for fit.'
            WRITE(2,*)'         Default grid will be used.'
            EGRid(0) = 1.1
            EGRid(2) = 0.1
            EGRid(3) = 0.02
          ENDIF
   25     CLOSE(5)
          CLOSE(72)
 
C--- Open the principal file for printing of fit output
          OPEN(UNIT = 2,FILE = 'FIT.OUT',STATUS = 'UNKNOWN')
 
          CALL THORA(2)
 
          WRITE(2,*)
          WRITE(2,*)' Setup of automatic fit procedure initiated.'
          WRITE(2,*)
 
C-- Test for the existence of the C4 data file
          INQUIRE(FILE = ('C4.dat'),EXIST = fexist)
          IF(.NOT.fexist)THEN
            WRITE(2,*)' A C4 data file, *.c4, is necessary for fitting.'
     &                , ' STOP.'
            STOP 'A C4 data file is necessary for fitting'
          ENDIF
 
C--- Tests for the existence of the direct optical potential file
C--- and determines the potentials it contains for posterior consistency
C--- check of the parameters to be varied
          INQUIRE(FILE = ('OMPAR.DIR'),EXIST = fexist)
          IF(.NOT.fexist)THEN
            WRITE(2,*)' A direct optical data file, *-omp.dir, is ', 
     &                'necessary for fitting. STOP.'
            STOP 'A direct optical data file is necessary for fitting'
          ELSE
            OPEN(UNIT = 70,FILE = 'OMPAR.DIR',STATUS = 'old')
            DO i = 1, 11
              READ(70,'(a1)')cmnd(1:1)
            ENDDO
            DO j1 = 1, 6
              READ(70,'(i5)')ipotrng(j1)
              IF(ipotrng(j1).GT.0)THEN
                DO k = 1, 9*ipotrng(j1)
                  READ(70,'(a1)')cmnd(1:1)
                ENDDO
              ENDIF
            ENDDO
            CLOSE(70)
          ENDIF
 
C--- Tests for the existence of the collective level file and
C--- determines whether the nucleus is teated as deformed or not
C--- for posterior consistency of parameters to be varied
          INQUIRE(FILE = ('TARGET_COLL.DAT'),EXIST = fexist)
          IF(.NOT.fexist)THEN
            WRITE(2,*)' A collective level file, *-lev.col, is ', 
     &                'necessary for fitting. STOP.'
            STOP
          ELSE
            OPEN(UNIT = 70,FILE = 'TARGET_COLL.DAT',STATUS = 'old')
            DO i = 1, 3
              READ(70,'(8x,a35)')cmnd
            ENDDO
            idef = 0
            IF(cmnd(28:35).EQ.'deformed')idef = 1
            IF(cmnd(28:35).EQ.'dynamica')THEN
              WRITE(8,*)'ERROR: OPTMAN OMP fit is not implemented'
              STOP 'ERROR: OPTMAN OMP fit is not implemented'
            ENDIF
            CLOSE(70)
          ENDIF
C--- Fit weights for types of cross sections (MF, MT) are initialized to 1.
          DO n = 1, 15
            WT(n,1) = 1.0
            WT(n,2) = 1.0
          ENDDO
 
C--- Weights read as FITWT are analyzed and stored
C--- Weights corresponding to MF=3 as wt(.,1), to MF=4 as wt(.,2)
C--- The strength function weight (MF=3, MT=0) is stored in wt(15,2)
          IF(nwt.GT.0)THEN
            DO n = 1, nwt
              IF(idw(1,n).EQ.3)THEN
                IF(idw(2,n).EQ.0)WT(15,2) = wtx(n)
                IF(idw(2,n).EQ.1)WT(1,1) = wtx(n)
                IF(idw(2,n).EQ.2)WT(3,1) = wtx(n)
                IF(idw(2,n).EQ.3)WT(2,1) = wtx(n)
                IF(idw(2,n).GT.50.AND.idw(2,n).LT.60)WT(idw(2,n) - 47,1)
     &             = wtx(n)
              ELSEIF(idw(1,n).EQ.4)THEN
                IF(idw(2,n).EQ.2)WT(1,2) = wtx(n)
                IF(idw(2,n).GT.50.AND.idw(2,n).LT.60)WT(idw(2,n) - 49,2)
     &             = wtx(n)
              ENDIF
            ENDDO
          ENDIF
 
          IF(NFIt.GT.MXFit - 1)THEN
            WRITE(2,
     &'('' A maximum of'',i3,'' parameters may be fit '',   ''at once.''
     &)')MXFit - 1
            WRITE(2,*)' Please adjust your input accordingly. STOP.'
            STOP
          ENDIF
 
C--- The parameters to be fit are first ordered according to the
C--- parameter type, stored in axx.
          IF(NFIt.GT.1)THEN
            CALL BUBBLE(NFIt,axx,ipt)
          ELSE
            ipt(1) = 1
          ENDIF
 
C--- The parameters to be fit are then analyzed for consistency with
C--- the direct optical potential and level files and stored according
C--- to the internal order used in the CHISQRD routine. The real parameter
C--- axx(n) determining the data type is stored as three integers in idv(.,n)
          IF(NFIt.GT.0)THEN
 
            DO n = 1, NFIt
              VALs(n) = valx(ipt(n))
              Pars(n) = VALs(n)
              XVAls(n) = xvalx(ipt(n))
              Dparmx(n) = XVAls(n)
              axt = axx(ipt(n))
              IDV(1,n) = axt/1000. + 0.1
              axt = axt - 1000*IDV(1,n)
              IDV(2,n) = axt/100. + 0.1
              IDV(3,n) = axt - 100.*IDV(2,n) + 0.1
              IF(IDV(1,n).LT.0.OR.IDV(1,n).GT.7.OR.IDV(2,n).LT.1.OR.
     &           (IDV(2,n).GT.3.AND.IDV(1,n).LT.7))THEN
                WRITE(2,*)'The set of parameters ', (IDV(j,n),j = 1,3), 
     &                    ' , used with FITPAR, is invalid. STOP.'
                STOP
              ENDIF
              IF(IDV(1,n).LT.7)THEN
                IF(ipotrng(IDV(1,n)).EQ.0)THEN
                  WRITE(2,*)'The potential does not contain a term', 
     &                      'of the type FIT'//pot1(IDV(1,n)), 
     &                      ' or FITPAR ', IDV(1,n), '. STOP.'
                  STOP
                ENDIF
                IF(IDV(2,n).EQ.1.OR.IDV(2,n).EQ.2)THEN
                  IF(IDV(3,n).LT.1.OR.IDV(3,n).GT.13)THEN
                    WRITE(2,*)'The value ', IDV(3,n), ' used with FIT', 
     &                        pot1(IDV(1,n))//pot2(IDV(2,n)), 
     &                        'or FITPAR ', IDV(1,n), IDV(2,n), 
     &                        ' is out of range. STOP.'
                    STOP
                  ENDIF
                ELSEIF(IDV(3,n).LT.1.OR.IDV(3,n).GT.25)THEN
                  WRITE(2,*)'The value ', IDV(3,n), ' used with ', 
     &                      'FIT'//pot1(IDV(1,n))//pot2(IDV(2,n)), 
     &                      'or FITPAR ', IDV(1,n), IDV(2,n), 
     &                      ' is out of range. STOP.'
                  STOP
                ENDIF
              ELSE
                IF(IDV(2,n).LT.2.OR.IDV(2,n).GT.4)THEN
                  WRITE(2,*)'The multipolarity ', IDV(2,n), 
     &                      ' used with ', 
     &      'FITBET  is out of range. It must be between 2 and 4. STOP.'
                  STOP
                ENDIF
                IF(idef.EQ.0.AND.IDV(2,n).EQ.4)THEN
                  WRITE(2,*)
     &                     'The multipolarity 4 cannot be varied in the'
     &                     , ' vibrational model. STOP.'
                  STOP
                ENDIF
                IF(idef.EQ.1.AND.IDV(2,n).EQ.3)THEN
                  WRITE(2,*)
     &                     'The multipolarity 3 cannot be varied in the'
     &                     , ' rotational model. STOP.'
                  STOP
                ENDIF
              ENDIF
            ENDDO
            WRITE(2,*)' Initial values of parameters to be adjusted:'
            CALL WRITEPARS
          ELSE
            WRITE(2,*)' No parameters to be varied. Chi-squared will', 
     &                ' be calculated.'
          ENDIF
 
          Nnft = NFIt
          IDV(1,NFIt + 1) = 99
          IDV(2,NFIt + 1) = 9
 
          WRITE(2,*)
 
C--- The experimental data is prepared and the energy mesh for fitting
C--- is pepared and written to FITIN.DAT.
          CALL READC4(emin,emax,izz,iaa)
 
          IF(NFIt.LE.0)RETURN
 
C--- A check for initial displacements of fit parameters is made.
          ichng0 = 0
          DO n = 1, NFIt
            IF(ABS(VALs(n)).GT.1.0E-5)ichng0 = 1
          ENDDO
 
C--- If initial displacements are found, the OMPAR.DIR and TARGET_COLL.DAT
C--- files are modified accordingly. The original files are first moved.
          IF(ichng0.EQ.1)THEN
            IF(IDV(1,1).LT.7)THEN
              IF(linux)THEN
                ctmp = 'mv OMPAR.DIR OMPAR0.DIR'
                itmp = PIPE(ctmp)
              ELSE
                ctmp = 'ren OMPAR.DIR OMPAR0.DIR'
                itmp = PIPE(ctmp)
              ENDIF
            ENDIF
            IF(IDV(1,NFIt).EQ.7)THEN
              IF(linux)THEN
                ctmp = 'mv TARGET_COLL.DAT TARGET_COLL0.DAT'
                itmp = PIPE(ctmp)
              ELSE
                ctmp = 'ren TARGET_COLL.DAT TARGET_COLL0.DAT'
                itmp = PIPE(ctmp)
              ENDIF
            ENDIF
 
C--- The maximum fit displacements are modified so as to not constrain the
C--- initial displacements.
            DO n = 1, NFIt
              xvalx(n) = XVAls(n)
              XVAls(n) = ABS(VALs(n))
            ENDDO
C--- The files are modified according to the initial displacements.
            CALL FILEPREP
 
C--- The displacements are set to 0 in the parameter file and the maximum
C--- fit displacements restored.
            DO n = 1, NFIt
              VALs(n) = 0.0
              XVAls(n) = xvalx(n)
            ENDDO
          ENDIF
 
C--- The maximum fit displacements are now checked to see that at least
C--- one is nonzero.
          xvalmx = 0.0
          DO n = 1, NFIt
            xvalmx = xvalmx + XVAls(n)
          ENDDO
 
C--- If all are zero, nfit is set to zero.
          IF(xvalmx.LT.0.01)THEN
            NFIt = 0
            Nnft = NFIt
            RETURN
          ENDIF
 
C--- The potential and deformation parameter files are now moved to
C--- the appropriate location to serve as a basis for variations during
C--- the fitting.
          IF(IDV(1,1).LT.7)THEN
            IF(linux)THEN
              ctmp = 'mv OMPAR.DIR OMPAR0.DIR'
              itmp = PIPE(ctmp)
            ELSE
              ctmp = 'ren OMPAR.DIR OMPAR0.DIR'
              itmp = PIPE(ctmp)
            ENDIF
          ENDIF
          IF(IDV(1,NFIt).EQ.7)THEN
            IF(linux)THEN
              ctmp = 'mv TARGET_COLL.DAT TARGET_COLL0.DAT'
              itmp = PIPE(ctmp)
            ELSE
              ctmp = 'ren TARGET_COLL.DAT TARGET_COLL0.DAT'
              itmp = PIPE(ctmp)
            ENDIF
          ENDIF
 
          RETURN
        ELSE
          CLOSE(5)
          CLOSE(18,STATUS = 'delete')
          CLOSE(72,STATUS = 'delete')
          RETURN
        ENDIF
      ENDIF
 
C--- Write whatever falls through to FITIN.DAT (18) and INPUT1.DAT (72)
C--- (before GO is found).
      WRITE(18,'(a35)')cmnd
      WRITE(72,'(a35)')cmnd
      GOTO 10
   30 WRITE(8,*)'ERROR: EOF in the MANDATORY SECTION OF THE INPUT FILE.'
      STOP 'EOF in the MANDATORY SECTION OF THE INPUT FILE.'
      END SUBROUTINE SCAN4FIT
 
!---------------------------------------------------------------------------
C
C-------------------------------------------------------------------
C
      SUBROUTINE WRITEPARS
C
C*** Start of declarations rewritten by SPAG
C
C PARAMETER definitions
C
      INTEGER, PARAMETER :: MXFit = 20
C
C COMMON variables
C
      INTEGER, DIMENSION(3,MXFit) :: IDV
      INTEGER :: NFIt
      REAL, DIMENSION(MXFit) :: VALs, XVAls
      COMMON /FITPARS/ VALs, XVAls, IDV, NFIt
C
C Local variables
C
      INTEGER :: n
      CHARACTER(13), DIMENSION(6) :: pot1
      CHARACTER(9), DIMENSION(3) :: pot2
C
C*** End of declarations rewritten by SPAG
C
C
C--- Writes the parameters to be adjusted in a reasonably readable manner
C
 
 
 
      DATA pot1/'Real volume  ', 'Imag volume  ', 'Real surface ', 
     &     'Imag surface ', 'Real spn-orb ', 'Imag spn-orb '/
      DATA pot2/'radius   ', 'diffuse  ', 'strength '/
 
C--- First the optical potential parameters and their fit limits
      DO n = 1, NFIt
        IF(IDV(1,n).LT.7)THEN
          WRITE(2,'(a28,i2,a9,f7.4,a16,f7.4)')pot1(IDV(1,n))
     &          //pot2(IDV(2,n))//'param ', IDV(3,n), ' value = ', 
     &          VALs(n), '   max val = +/-', XVAls(n)
        ELSE
C--- then the deformations and their fit limits
          WRITE(2,'(a28,i2,a9,f7.4,a16,f7.4)')
     &          'Deformation  beta  --   multipolarity ', IDV(2,n), 
     &          ' value = ', VALs(n), '   max val = +/-', XVAls(n)
        ENDIF
      ENDDO
 
      RETURN
      END SUBROUTINE WRITEPARS
 
!---------------------------------------------------------------------------
C
C-------------------------------------------------------------------
C
      SUBROUTINE READC4(Emin,Emax,Izz,Iaa)
C
C*** Start of declarations rewritten by SPAG
C
C PARAMETER definitions
C
      INTEGER, PARAMETER :: MXInd = 5000, MXInda = 5000, MXElvls = 9, 
     &                      NEXlvl = 2
      REAL, PARAMETER :: DISc = 1.0E4
C
C COMMON variables
C
      REAL, DIMENSION(MXInda) :: ANGs, DSIga, SIGa
      REAL, DIMENSION(MXInd) :: DSIg, SIG
      REAL, DIMENSION(0:MXInd) :: EGRid, EN
      CHARACTER(64) :: EMPiredir
      CHARACTER(72) :: EMPtitle
      INTEGER, DIMENSION(MXInd) :: ICAla, IDAng, IDNt, NANgd, NANgs, 
     &                             NINts
      INTEGER :: NNDe
      REAL :: THS0, WT0
      COMMON /EXPTLDAT/ EN, SIG, DSIg, ANGs, SIGa, DSIga, EGRid, WT0, 
     &                  THS0, NINts, NANgd, NANgs, ICAla, IDNt, IDAng, 
     &                  NNDe
      COMMON /GLOBAL_E/ EMPiredir, EMPtitle
C
C Dummy arguments
C
      REAL*8 :: Emax, Emin
      INTEGER :: Iaa, Izz
C
C Local variables
C
      REAL :: aequiv, angfac, cth, dcth, elequiv, elvl, emaxm, eminm, 
     &        ss0tmp, ss0_unc, xxx
      REAL, DIMENSION(MXInd) :: angtmp, dex, dsix, ex, six
      CHARACTER(1) :: cm, ex4st, metap, metat
      REAL, DIMENSION(MXInda) :: dsixa, dthex, sixa, thex
      REAL, DIMENSION(MXElvls) :: elvls
      LOGICAL :: fexist
      INTEGER :: i, ind, inda, inde, indmx, istat0, izap, izat, j, mlvl, 
     &           mtpe, mxlvl, nangdtot, nangtmp, nangtot, nanxtot, 
     &           natmp, nintot, nuind, nztmp
      INTEGER, DIMENSION(MXInd) :: idtmp, ipe, loca, mf, mt
      INTEGER :: INT
      INTEGER, DIMENSION(MXInda) :: ipt
      INTEGER, DIMENSION(15,3) :: istat
      CHARACTER(3) :: lvl
C
C*** End of declarations rewritten by SPAG
C
C
C--- READC4 prepares the experimental data, obtained from the
C--- appropriate C4 file, for fitting. The s-wave neutron strength
C    function is also used, when it exists and is relevant.
C--- The input parameters are
C---      emin - minimum incident energy of data to be considered
C---      emax - maximum incident energy of data to be considered
C---      izz  - charge of the target
C---      iaa  - mass number of the target (for neutron projectile)
C--- iaa is set to zero when the projectile is not a neutron.
C--- izz and iaa are used to obtain the s-wave neutron scattering length.
C---
C--- The routine also accumulates and prints statistics on the number and
C--- type of data and calls the routine WRITENPUT which determines and writes
C--- the energy grid for EMPIRE calculations used in CHISQRD.
C---
C--- To facilitate comparison, in particular, to be able to determine
C--- equality of incident energies, these are discretized on a scale of
C--- 1/disc (MeV), where disc is a compilation parameter.
C
 
 
 
      DATA elequiv/1.5E3/, aequiv/1.0E0/
 
C--- The C4 file expresses energies in eV, so the min and max energy must
C--- be converted accordingly.
      angfac = 45./ATAN(1.)
      eminm = 1.0E6*Emin
      emaxm = 1.0E6*Emax
      EN(0) = 0.0
 
C--- Istat will be used later to accumulate statistics of data types.
      istat0 = 0
      DO i = 1, 15
        DO j = 1, 3
          istat(i,1) = 0
        ENDDO
      ENDDO
 
      WRITE(2,*)
      WRITE(2,*)'Preparing experimental data and input files for fits.'
      WRITE(2,*)
 
C--- If emin=1 keV, the first datum included is assumed to be the strength
C--- function. MF=3, MT=0 is used to specify it internally.
      ind = 0
      IF(INT(eminm + 0.5).EQ.1000)THEN
        ex(1) = EGRid(1)
        mf(1) = 3
        mt(1) = 0
        six(1) = 0.0
        dsix(1) = 0.0
 
C--- It is obtained from the following file.
        INQUIRE(FILE = TRIM(EMPiredir)
     &          //'/RIPL/resonances/resonances0.dat',EXIST = fexist)
        IF(fexist.AND.Iaa.NE.0)THEN
          OPEN(47,FILE = TRIM(EMPiredir)//'/RIPL/resonances'//
     &         '/resonances0.dat',STATUS = 'old')
          READ(47,'(///)')  ! Skipping first 4 title lines
          DO i = 1, 296
C           READ (47,'(2i4,37x,2f6.2)', END = 60, ERR = 60)
C           Changed to RIPL-3 file
C      (i3,1x,a2,1x,i3,2x,f3.1,2x,f6.3,2x,2(e8.2,2x),1x,2(f4.2,2x),2(f4.1,1x),2x,a4).
            READ(47,'(i3,4x,i3,36x,2(f4.2,2x))',END = 5,ERR = 5)nztmp, 
     &           natmp, ss0tmp, ss0_unc
            IF(nztmp.NE.Izz.OR.natmp.NE.Iaa)CYCLE
            six(1) = ss0tmp
            dsix(1) = ss0_unc
          ENDDO
    5     CLOSE(47)
        ENDIF
        IF(six(1).GT.0.0)ind = 1
      ENDIF
 
C--- The C4 data file is used to obtain the rest of the experimental data.
C--- Z and A are not checked so that natural data can be included with the
C--- isotopic data.
      OPEN(UNIT = 26,FILE = 'C4.dat',STATUS = 'old')
 
      ind = ind + 1
      inda = 1
      mxlvl = 0
 
C--- Reads a line from the C4 file.
   10 READ(26,'(i5,i6,a1,i3,i4,3a1,8e9.3,a3)',END = 20)izap, izat, 
     &     metat, mf(ind), mt(ind), metap, ex4st, cm, ex(ind), dex(ind), 
     &     six(ind), dsix(ind), cth, dcth, elvl, xxx, lvl
C--- First check to see that the datum is in the desired energy range.
      IF(ex(ind).GE.eminm.AND.ex(ind).LE.emaxm)THEN
C--- The incident energy is discretized and converted to MeV.
        ex(ind) = INT(1.0E-6*DISc*ex(ind) + 0.5)/DISc
C--- An experimental uncertainty of 10% is assumed when none is found.
        IF(dsix(ind).LT.1.0E-6)dsix(ind) = 0.1*six(ind)
C--- Natural data may be weighted differently by modifiying their
C--- uncertainties.
        IF(MOD(izat,1000).EQ.0)dsix(ind) = WT0*dsix(ind)
C--- Sorting out the data to excited levels. Although data for up to
C--- 9 (mxelvls) excited levels can be stored, only 2 (nexlvl) are
C--- actually used at the moment.
        IF((mt(ind).GT.0.AND.mt(ind).LT.4).OR.
     &     (mt(ind).EQ.51.AND.lvl.EQ.'LVL'))THEN
          IF(elvl.NE.0.0)THEN
            mlvl = 0
            IF(mxlvl.GT.0)THEN
              DO i = 1, mxlvl
                IF(ABS(elvls(i) - elvl).LT.elequiv)mlvl = i
              ENDDO
            ENDIF
            IF(mlvl.EQ.0)THEN
              mxlvl = MIN(mxlvl + 1,MXElvls)
              elvls(mxlvl) = elvl
              mlvl = mxlvl
            ENDIF
            mt(ind) = 50 + mlvl
          ENDIF
          IF(mt(ind).GT.50 + NEXlvl)GOTO 10
C--- Converting and storing differential angular MF=4 data. (Integrated data
C--- was stored when read.)
          IF(mf(ind).EQ.3)THEN
            ind = ind + 1
          ELSEIF(mf(ind).EQ.4)THEN
            sixa(inda) = six(ind)
            dsixa(inda) = dsix(ind)
            dthex(inda) = angfac*(ACOS(cth + dcth) - ACOS(MAX(cth-dcth,
     &                    0.0)))
            thex(inda) = angfac*ACOS(cth)
            IF(ex(ind).NE.ex(ind - 1).OR.mt(ind).NE.mt(ind - 1))THEN
              dsix(ind - 1) = inda - 0.9
              six(ind) = inda + 0.1
              ind = ind + 1
            ENDIF
            inda = inda + 1
          ENDIF
        ENDIF
        IF(ind.GT.MXInd.OR.inda.GT.MXInda)THEN
          WRITE(2,*)' Data arrays too small in READC4.'
          WRITE(2,*)' Increase dimension of MXIND or MXINDA. STOP.'
          STOP ' Increase dimension of MXIND or MXINDA in READC4. STOP.'
        ENDIF
      ENDIF
      GOTO 10
 
C--- indmx is the total number of data points.
   20 CLOSE(26)
      indmx = ind - 1
      IF(mf(indmx).EQ.4)dsix(indmx) = inda - 0.9
 
C--- The experimental data are ordered in incident energy. The ordering is
C--- contained in the array ipe.
      CALL BUBBLE(indmx,ex,ipe)
 
C--- Several counters important for storage are initialized.
C---   inde - number of distinct incident energies
C---   nintot - number of integrated cross sections
C---   nanxtot - number of angular data (angles times ang. dists.)
C---   nangtot - number of angles
C---   nangdtot - number of angular distributions
      inde = 0
      nintot = 0
      nanxtot = 0
      nangtot = 0
      nangdtot = 0
 
C--- a fake point above the real data is added
C--- This fake point defines the energy limit above which the program
C--- will not function as written due to integer*4 overflow in the
C--- discretization.
      ipe(indmx + 1) = indmx + 1
      ex(indmx + 1) = (2**30 - 1)/DISc
      mf(indmx + 1) = 3
      mt(indmx + 1) = 1
 
C--- The data are scanned and combined according to the discretized
C--- incident energy.
      DO i = 1, indmx + 1
        IF(INT(DISc*ex(ipe(i)) + 0.5).GT.INT(DISc*EN(inde) + 0.5))THEN
C--- When a different energy is found, angular distributions of the preceding
C--- energy are stored.
          IF(inde.GT.0)THEN
            IF(NANgd(inde).GT.0)THEN
C--- The angles are first ordered and merged so that only one set of angles
C--- is stored for each incident energy.
              CALL BUBBLE(nangtmp,angtmp,ipt)
              inda = nangtot + 1
              ANGs(inda) = angtmp(ipt(1))
              DO j = 2, nangtmp
                IF(ABS(angtmp(ipt(j)) - ANGs(inda)).GT.aequiv)THEN
                  inda = inda + 1
                  ANGs(inda) = angtmp(ipt(j))
                ENDIF
              ENDDO
C--- The angular data is then stored. The data value is 0 when there is
C--- no data for that angular distribution at the given angle.
              NANgs(inde) = inda - nangtot
              DO j = 1, NANgs(inde)*NANgd(inde)
                SIGa(nanxtot + j) = 0.
                DSIga(nanxtot + j) = 0.
              ENDDO
              inda = nangtot + 1
              nuind = nanxtot + NANgs(inde)*idtmp(ipt(1))
     &                + inda - nangtot
              SIGa(nuind) = 1.0E3*sixa(loca(ipt(1)))
              DSIga(nuind) = 1.0E3*dsixa(loca(ipt(1)))
              DO j = 2, nangtmp
                IF(ABS(angtmp(ipt(j)) - ANGs(inda)).GT.aequiv)
     &             inda = inda + 1
                nuind = nanxtot + NANgs(inde)*idtmp(ipt(j))
     &                  + inda - nangtot
C--- Data are converted to millibarns.
                SIGa(nuind) = 1.0E3*sixa(loca(ipt(j)))
                DSIga(nuind) = 1.0E3*dsixa(loca(ipt(j)))
              ENDDO
              nangtot = inda
              nanxtot = nanxtot + NANgs(inde)*NANgd(inde)
              ICAla(inde) = 1
C--- The maximum number of angular distributions that must be calculated
C--- at a given incident energy is determined.
              DO j = nangdtot - NANgd(inde) + 1, nangdtot
                ICAla(inde) = MAX(ICAla(inde),IDAng(j))
              ENDDO
            ENDIF
          ENDIF
C--- Once the angular data of the preceding energy has been treated,
C--- relevant indices for the new energy are initialized and the energy is
C--- stored.
          inde = inde + 1
          EN(inde) = ex(ipe(i))
          NINts(inde) = 0
          NANgd(inde) = 0
          NANgs(inde) = 0
          nangtmp = 0
        ENDIF
C--- Integrated data are stored. Data are converted to millibarns.
C--- Statistics on data type are accumulated.
        IF(mf(ipe(i)).EQ.3)THEN
          NINts(inde) = NINts(inde) + 1
          nintot = nintot + 1
          IF(mt(ipe(i)).EQ.0)THEN
            SIG(nintot) = six(ipe(i))
            DSIg(nintot) = dsix(ipe(i))
            IDNt(nintot) = 0
            istat0 = 1
          ELSE
            SIG(nintot) = 1.0E3*six(ipe(i))
            DSIg(nintot) = 1.0E3*dsix(ipe(i))
            IF(mt(ipe(i)).EQ.1)THEN
              IDNt(nintot) = 1
              istat(1,1) = istat(1,1) + 1
            ELSEIF(mt(ipe(i)).EQ.2)THEN
              IDNt(nintot) = 3
              istat(3,1) = istat(3,1) + 1
            ELSEIF(mt(ipe(i)).EQ.3)THEN
              IDNt(nintot) = 2
              istat(2,1) = istat(2,1) + 1
            ELSE
              mtpe = mt(ipe(i)) - 47
              IDNt(nintot) = mtpe
              istat(mtpe,1) = istat(mtpe,1) + 1
            ENDIF
          ENDIF
C--- Angular data are stored in a preliminary manner. They can be
C--- processed completely (above) only after a new energy is found
C--- and one is sure that no more distributions follow.
        ELSE
          NANgd(inde) = NANgd(inde) + 1
          nangdtot = nangdtot + 1
          IF(mt(ipe(i)).EQ.2)THEN
            IDAng(nangdtot) = 1
            istat(1,2) = istat(1,2) + 1
            mtpe = 1
          ELSE
            mtpe = mt(ipe(i)) - 49
            IDAng(nangdtot) = mtpe
            istat(mtpe,2) = istat(mtpe,2) + 1
          ENDIF
          DO j = INT(six(ipe(i))), INT(dsix(ipe(i)))
            nangtmp = nangtmp + 1
            angtmp(nangtmp) = thex(j)
            idtmp(nangtmp) = NANgd(inde) - 1
            loca(nangtmp) = j
            istat(mtpe,3) = istat(mtpe,3) + 1
          ENDDO
        ENDIF
      ENDDO
C--- Nnde is the total number of incident energies at which data exist.
      NNDe = inde - 1
      istat(1,1) = istat(1,1) - 1
 
C--- Statistics on experimental data are printed.
      WRITE(2,
     &'('' The experimental data set in the energy range from'',   f6.3,
     &'' MeV to '', f6.2,'' MeV'',/,''  contains values at'',i5,   '' en
     &ergies:'')')Emin, Emax, NNDe
      IF(istat0.GT.0.)WRITE(2,'(7x,''1 s-wave strength function'')')
      IF(istat(1,1).GT.0)WRITE(2,'(i8,'' total cross sections'')')
     &                         istat(1,1)
      IF(istat(2,1).GT.0)WRITE(2,'(i8,'' absorption cross sections'')')
     &                         istat(2,1)
      IF(istat(3,1).GT.0)WRITE(2,'(i8,'' elastic cross sections'')')
     &                         istat(3,1)
      IF(istat(4,1).GT.0)WRITE(2,
     &'(i8,'' inelastic cross sections to'',   '' the 1st excited state'
     &')')istat(4,1)
      IF(istat(5,1).GT.0)WRITE(2,
     &'(i8,'' inelastic cross sections to'',   '' the 2nd excited state'
     &')')istat(5,1)
      IF(istat(1,2).GT.0)WRITE(2,
     &'(i8, '' elastic angular distributi'', ''ons with a total of '',i3
     &,'' points'')')istat(1,2), istat(1,3)
      IF(istat(2,2).GT.0)WRITE(2,
     &'(i8, '' inelastic angular distribu'', ''tions (1st state) with a 
     &total of '',i3,'' points'')')istat(2,2), istat(2,3)
      IF(istat(3,2).GT.0)WRITE(2,
     &'(i8, '' inelastic angular distribu'', ''tions (2nd state) with a 
     &total of '',i3,'' points'')')istat(3,2), istat(3,3)
 
C--- Based on the existent experimental data, the energy mesh for fitting
C--- is now prepared and written to the input file, FITIN.DAT.
      CALL WRITENPUT(Emin)
 
      RETURN
      END SUBROUTINE READC4
 
!---------------------------------------------------------------------------
C
C-------------------------------------------------------------------
C
      SUBROUTINE WRITENPUT(Emin)
C
C*** Start of declarations rewritten by SPAG
C
C PARAMETER definitions
C
      INTEGER, PARAMETER :: MXInd = 5000, MXInda = 5000
      REAL, PARAMETER :: DISc = 1.0E4
C
C COMMON variables
C
      REAL, DIMENSION(MXInda) :: ANGs, DSIga, SIGa
      REAL, DIMENSION(MXInd) :: DSIg, SIG
      REAL, DIMENSION(0:MXInd) :: EGRid, EN
      INTEGER, DIMENSION(MXInd) :: ICAla, IDAng, IDNt, NANgd, NANgs, 
     &                             NINts
      INTEGER :: NNDe
      REAL :: THS0, WT0
      COMMON /EXPTLDAT/ EN, SIG, DSIg, ANGs, SIGa, DSIga, EGRid, WT0, 
     &                  THS0, NINts, NANgd, NANgs, ICAla, IDNt, IDAng, 
     &                  NNDe
C
C Dummy arguments
C
      REAL*8 :: Emin
C
C Local variables
C
      CHARACTER(132) :: ctmp
      REAL :: egr, egrold, tmp
      INTEGER :: ie, ieg, ien, igr, j, ngr, nodata, ntangs
      INTEGER :: INT
      INTEGER*4 :: itmp
      LOGICAL :: linux
      INTEGER*4 :: PIPE
C
C*** End of declarations rewritten by SPAG
C
C
C--- Defines the discretized mesh of incident energies for EMPIRE
C--- calculations and prints it in FITIN.DAT.
C
 
 
      linux = .TRUE.
 
      tmp = Emin
                ! dummy statement
 
      IF(EGRid(0).LT.1.5)THEN
C--- The case in which the energy mesh is defined by the FITGRD keyword.
        ngr = 2*MXInd
      ELSE
C--- The case in which the energy mesh is defined by the input grid.
        ngr = EGRid(0)
      ENDIF
 
C--- Initialize parameters used in writing the energy grid.
      ntangs = 0
      ie = 1
      igr = 1
      nodata = 0
 
C--- The mesh of energies is now written to the FITIN.DAT file.
C--- Energies for which angular data exist are added to the mesh. Each
C--- energy is written with the number of angles and of elastic/inelastic
C--- distributions to be calculated. When the number of angles is greater
C--- than 0, the angles follow the energy on as many lines as are neeeded.
C---
C--- The flag nodata avoids writing energies of the grid which will not
C--- be needed later for interpolation of integrated cross sections nor
C--- for angular distributions.
 
      egr = EGRid(1)
      ieg = INT(DISc*egr + 0.5)
      ien = INT(DISc*EN(1) + 0.5)
 
   10 IF(ien.LT.ieg)THEN
        IF(nodata.GT.1)WRITE(18,'(f8.4,3i8)')egrold, 0, 0
        IF(NANgs(ie).GT.0)THEN
          WRITE(18,'(f8.4,3i8)')EN(ie), NANgs(ie), ICAla(ie)
          WRITE(18,'(10f8.2)')(ANGs(j),j = ntangs + 1,ntangs + NANgs(ie)
     &                        )
          ntangs = ntangs + NANgs(ie)
        ENDIF
C--- Increment ie and define new ien
        ie = ie + 1
        ien = INT(DISc*EN(ie) + 0.5)
        nodata = 0
      ELSEIF(ien.GT.ieg)THEN
        IF(igr.GT.1.AND.nodata.EQ.0)WRITE(18,'(f8.4,3i8)')egr, 0, 0
C--- Increment igr and define new ieg
        IF(ie.GT.NNDe)GOTO 20
        igr = igr + 1
        IF(igr.GT.ngr)GOTO 20
        egrold = egr
        IF(EGRid(0).LT.1.5)THEN
          egr = EGRid(1) + (igr - 1)*(EGRid(2) + 0.5*(igr - 2)*EGRid(3))
        ELSE
          egr = EGRid(igr)
        ENDIF
        ieg = INT(DISc*egr + 0.5)
        nodata = nodata + 1
      ELSE      ! ien.EQ.ieg
        IF(NANgs(ie).GT.0)THEN
          IF(nodata.GT.1)WRITE(18,'(f8.4,2i8)')egrold, 0, 0
          WRITE(18,'(f8.4,2i8)')EN(ie), NANgs(ie), ICAla(ie)
          WRITE(18,'(10f8.2)')(ANGs(j),j = ntangs + 1,ntangs + NANgs(ie)
     &                        )
          ntangs = ntangs + NANgs(ie)
        ELSE
          IF(nodata.GT.1)WRITE(18,'(f8.4,3i8)')egrold, 0, 0
          IF(igr.GT.1)WRITE(18,'(f8.4,3i8)')egr, 0, 0
        ENDIF
C--- Increment igr and define new ieg
        igr = igr + 1
        IF(igr.GT.ngr)GOTO 20
        egrold = egr
        IF(EGRid(0).LT.1.5)THEN
          egr = EGRid(1) + (igr - 1)*(EGRid(2) + 0.5*(igr - 2)*EGRid(3))
        ELSE
          egr = EGRid(igr)
        ENDIF
        ieg = INT(DISc*egr + 0.5)
C--- Increment ie and define new ien
        ie = ie + 1
        IF(ie.GT.NNDe)GOTO 20
        ien = INT(DISc*EN(ie) + 0.5)
        nodata = 0
      ENDIF
      GOTO 10
 
C--- Finalizes the energy grid, as required by EMPIRE
   20 WRITE(18,'(f8.4,2i8)') - 1., 0, 0
      CLOSE(18)
 
      IF(ie.LT.NNDe)THEN
        WRITE(2,*)' Data points exist above the maximum energy, ', 
     &            egrold, ' MeV, of the energy grid used in fitting.'
        WRITE(2,*)' They will be disregarded in the fit.'
      ENDIF
 
C--- The file FITIN.DAT is now moved to INPUT.DAT to perform the
C--- the EMPIRE calculations used in CHISQRD. The original input file
C--- is first moved to INPUT0.DAT
      IF(linux)THEN
        ctmp = 'mv INPUT.DAT INPUT0.DAT'
        itmp = PIPE(ctmp)
        ctmp = 'mv FITIN.DAT INPUT.DAT'
        itmp = PIPE(ctmp)
      ELSE
        ctmp = 'ren INPUT.DAT INPUT0.DAT'
        itmp = PIPE(ctmp)
        ctmp = 'ren FITIN.DAT INPUT.DAT'
        itmp = PIPE(ctmp)
      ENDIF
 
      RETURN
      END SUBROUTINE WRITENPUT
 
!---------------------------------------------------------------------------
C
C-------------------------------------------------------------------
C
      SUBROUTINE BUBBLE(Ind,A,Ipt)
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      INTEGER :: Ind
      REAL, DIMENSION(*) :: A
      INTEGER, DIMENSION(*) :: Ipt
C
C Local variables
C
      INTEGER :: i, imdone, iptr
C
C*** End of declarations rewritten by SPAG
C
 
C--- Performs a simple bubble sort - stored in pointer ipt
 
 
C--- Initialize the pointer array
      DO i = 1, Ind
        Ipt(i) = i
      ENDDO
 
C-- First go up the array
   10 imdone = 1
      DO i = 1, Ind - 1
        IF(A(Ipt(i)).GT.A(Ipt(i+1)))THEN
          imdone = 0
          iptr = Ipt(i)
          Ipt(i) = Ipt(i + 1)
          Ipt(i + 1) = iptr
        ENDIF
      ENDDO
C--- then back down
      DO i = Ind - 1, 1, -1
        IF(A(Ipt(i)).GT.A(Ipt(i+1)))THEN
          imdone = 0
          iptr = Ipt(i)
          Ipt(i) = Ipt(i + 1)
          Ipt(i + 1) = iptr
        ENDIF
      ENDDO
C--- until nothing changes.
      IF(imdone.EQ.0)GOTO 10
 
      RETURN
      END SUBROUTINE BUBBLE
 
!---------------------------------------------------------------------------
C
C-------------------------------------------------------------------
C
      SUBROUTINE LOCALFIT(P0,Dpmx,Nfit,Xitr)
C
C*** Start of declarations rewritten by SPAG
C
C PARAMETER definitions
C
      INTEGER, PARAMETER :: MXFit = 20
C
C Dummy arguments
C
      INTEGER :: Nfit
      REAL :: Xitr
      REAL, DIMENSION(MXFit) :: Dpmx, P0
C
C Local variables
C
      REAL :: chi0, chi1, chi2, chix, dchi2, den, dp1, dp2, dpn, dpt, 
     &        dpx
      REAL :: CHISQRD
      REAL, DIMENSION(MXFit) :: dchi, dp, pp
      INTEGER :: ichng0, id, idsum, it, itfit, itmax, mxitr, n
      INTEGER :: INT
C
C*** End of declarations rewritten by SPAG
C
C
C--- This routine attempts to minimize a function CHISQRD. It performs a
C--- simple gradient search using numerical derivatives.
C--- Its input is given by
C---    p0 - the array of parameters to be varied.
C---    dpmx - an array of which 0.01*dpmx(.) is used to calculate derivatives
C---    nfit - the number of parameters to be varied.
C---    xitr - a real containing the inner and outer loop indices, mxitr and as
C---           itmax, in the form mxitr + itmax/100.
C---           Default values are: mxitr -> min(nfit,3); itmax -> 5.
C--- The output of the routine is given in p0.
C--- It also requires an external function CHISQRD(p0). The call to CHISQRD
C--- should not modify the array p0.
C
 
 
C----------------------------------------------------------------------
 
      IF(Nfit.GT.0)THEN
 
        mxitr = MAX(INT(Xitr + 0.001),MIN(Nfit,3))
        itmax = MAX(MOD(INT(100*Xitr+0.1),100),5)
 
        DO itfit = 1, mxitr
 
          DO n = 1, Nfit
            dp(n) = 0.0D0
            pp(n) = P0(n)
          ENDDO
 
          chi0 = CHISQRD(P0)
 
          WRITE(2,*)'**************************************'
          WRITE(2,*)
          WRITE(2,*)' At start of iteration', itfit, ', p0:'
          WRITE(2,*)(P0(n),n = 1,Nfit)
          WRITE(2,*)
          WRITE(2,*)' chi2=', chi0
          WRITE(2,*)
          WRITE(2,*)'-------------------------------------'
          WRITE(2,*)
          WRITE(2,*)' Initial variations:'
          WRITE(2,*)
 
          dchi2 = 0.0D0
 
          DO n = 1, Nfit
 
            IF(Dpmx(n).GT.1.0D-6)THEN
              dpn = 0.01*Dpmx(n)
              pp(n) = P0(n) + dpn
              chix = CHISQRD(pp)
              pp(n) = P0(n)
              dchi(n) = (chix - chi0)/dpn
            ELSE
              chix = chi0
              dchi(n) = 0.0
            ENDIF
 
            dchi2 = dchi2 + dchi(n)**2
 
            WRITE(2,*)
            WRITE(2,*)' Chi2 from change in ', n, '=   ', chix
            WRITE(2,*)' Derivative(', n, ') of chi2= ', dchi(n)
            WRITE(2,*)
 
          ENDDO
          WRITE(2,*)
 
          IF(dchi2.LT.1.0D-10)THEN
            WRITE(2,*)'Done -- no change in chi2!'
            RETURN
          ENDIF
 
          dchi2 = SQRT(dchi2)
          DO n = 1, Nfit
            dp(n) = -dchi(n)/dchi2
            IF(ABS(dp(n)).LT.1.0D-10)dp(n) = 0.0D0
          ENDDO
 
          WRITE(2,*)'-------------------------------------'
          WRITE(2,*)
          WRITE(2,*)' Normalized gradient'
          WRITE(2,*)( - dp(n),n = 1,Nfit)
 
          WRITE(2,*)
          WRITE(2,*)' Calculations against gradient:'
          WRITE(2,*)
 
          dpx = 0.01D0
          idsum = 0
 
          ichng0 = 0
 
          DO it = 1, itmax
 
            DO n = 1, Nfit
              pp(n) = P0(n) + dpx*dp(n)
            ENDDO
 
            chix = CHISQRD(pp)
 
            WRITE(2,*)
            WRITE(2,*)' At gradient iteration ', it, ', theor. vals:'
            WRITE(2,*)' chi2(', dpx, ')= ', chix
            WRITE(2,*)
 
            IF(it.NE.1)THEN
 
              IF(id.GT.0)THEN
                IF(id.EQ.2.AND..NOT.(dpx.LT.dp2.AND.chix - chi0.GT.chi1)
     &             )THEN
                  WRITE(2,*)
                  WRITE(2,*)'Changing chi0: ', dp1, '-->0'
                  ichng0 = 1
                  chi0 = chi0 + chi1
                  dpt = dp1
                  IF(dpx.GT.dp2)THEN
                    chi1 = chi2 - chi1
                    chi2 = chix - chi0
                    dp1 = dp2 - dpt
                    dp2 = dpx - dpt
                  ELSE
                    chi2 = chi2 - chi1
                    chi1 = chix - chi0
                    dp1 = dpx - dpt
                    dp2 = dp2 - dpt
                  ENDIF
                  DO n = 1, Nfit
                    P0(n) = P0(n) + dpt*dp(n)
                  ENDDO
                ELSE
                  IF(id.EQ.3)THEN
                    dp1 = dp2
                    chi1 = chi2
                  ENDIF
                  chi2 = chix - chi0
                  dp2 = dpx
                ENDIF
              ELSEIF(id.EQ. - 2)THEN
                IF(chix - chi0.GT.chi1)THEN
                  WRITE(2,*)
                  WRITE(2,*)'Changing chi0: ', dpx, '-->0'
                  ichng0 = 1
                  chi1 = chi1 + chi0 - chix
                  chi2 = chi2 + chi0 - chix
                  chi0 = chix
                  dp2 = dp2 - dpx
                  dp1 = dp1 - dpx
                  DO n = 1, Nfit
                    P0(n) = P0(n) + dpx*dp(n)
                  ENDDO
                ELSE
                  dp2 = dp1
                  chi2 = chi1
                  chi1 = chix - chi0
                  dp1 = dpx
                ENDIF
              ELSE
                IF(id.EQ. - 3)THEN
                  dp2 = dp1
                  chi2 = chi1
                ENDIF
                chi1 = chix - chi0
                dp1 = dpx
              ENDIF
 
              IF(chi1.LE.0.0D0)THEN
                den = dp1*chi2 - dp2*chi1
                IF(den.GT.1.0D-6)THEN
                  dpx = 0.5D0*(dp1**2*chi2 - dp2**2*chi1)/den
                  IF(dpx.GT.dp2)THEN
                    dpx = MIN(dpx,10.0*dp2)
                    id = 3
                    IF(chi2.LT.chi1)id = 2
                  ELSEIF(dpx.GT.dp1)THEN
                    id = 2
                  ELSE
                    id = -2
                  ENDIF
                ELSE
                  dpx = 5.0D0*dp2
                  id = 3
                  IF(chi2.LT.chi1)id = 2
                ENDIF
              ELSEIF(chi2.GT.chi1)THEN
                den = dp1*chi2 - dp2*chi1
                IF(den.GT.1.0D-6)THEN
                  dpx = 0.5D0*(dp1**2*chi2 - dp2**2*chi1)/den
                  IF(dpx.LT.0.0D0)dpx = 0.5D0*dp1
                  id = -3
                ELSE
                  dpx = 0.5D0*dp1
                  id = -3
                ENDIF
              ELSEIF(chi2.GT.0.0D0)THEN
                dpx = 0.5D0*dp1
                IF(id.GT.0)dpx = 0.5D0*dpx
                id = -3
              ELSE
                dpx = 2.0D0*dp2
                id = 3
              ENDIF
 
              WRITE(2,*)dpx, dp1, chi1, dp2, chi2
 
            ELSEIF(chix.GT.chi0)THEN
              chi2 = chix - chi0
              dp2 = dpx
              dpx = dpx/2.0D0
              id = -1
            ELSE
              chi1 = chix - chi0
              dp1 = dpx
              dpx = 2.0D0*dpx
              id = 1
 
            ENDIF
 
 
            IF(dpx.LT.1.0D-6)EXIT
 
            idsum = idsum + INT(SIGN(1,id))
 
          ENDDO
 
          IF((dpx.LT.1.0D-6.AND.ichng0.EQ.0).OR.
     &       (idsum + itmax.EQ.0.AND.chi1.GT.0.0D0))THEN
            WRITE(2,*)
            WRITE(2,*)' Search suspended -- no min in last iteration'
            EXIT
          ENDIF
 
          DO n = 1, Nfit
            P0(n) = P0(n) + dpx*dp(n)
          ENDDO
 
        ENDDO
      ENDIF
 
      DO n = 1, Nfit
        dp(n) = 0.0D0
      ENDDO
 
      chix = CHISQRD(P0)
 
      WRITE(2,*)
      WRITE(2,*)'**************************************'
      WRITE(2,*)
      WRITE(2,*)' Final values -- p0:'
      WRITE(2,*)(P0(n),n = 1,Nfit)
      WRITE(2,*)
      WRITE(2,*)' chi2(fin)=', chix
      WRITE(2,*)
      WRITE(2,*)' All done!'
 
      RETURN
      END SUBROUTINE LOCALFIT
 
!---------------------------------------------------------------------------
C
C-------------------------------------------------------------------
C
      FUNCTION CHISQRD(P0)
C
C*** Start of declarations rewritten by SPAG
C
C PARAMETER definitions
C
      INTEGER, PARAMETER :: MXInd = 5000, MXInda = 5000, MXFit = 20
      REAL, PARAMETER :: DISc = 1.0E4
C
C COMMON variables
C
      REAL, DIMENSION(MXInda) :: ANGs, DSIga, SIGa
      REAL, DIMENSION(MXInd) :: DSIg, SIG
      REAL, DIMENSION(0:MXInd) :: EGRid, EN
      INTEGER, DIMENSION(MXInd) :: ICAla, IDAng, IDNt, NANgd, NANgs, 
     &                             NINts
      INTEGER, DIMENSION(3,MXFit) :: IDV
      INTEGER :: NFIt, NNDe
      REAL :: THS0, WT0
      REAL, DIMENSION(MXFit) :: VALs, XVAls
      REAL, DIMENSION(15,2) :: WT
      COMMON /EXPTLDAT/ EN, SIG, DSIg, ANGs, SIGa, DSIga, EGRid, WT0, 
     &                  THS0, NINts, NANgd, NANgs, ICAla, IDNt, IDAng, 
     &                  NNDe
      COMMON /FITPARS/ VALs, XVAls, IDV, NFIt
      COMMON /FITWTS/ WT
C
C Dummy arguments
C
      REAL :: CHISQRD
      REAL, DIMENSION(MXFit) :: P0
C
C Local variables
C
      CHARACTER(35) :: astrngth
      REAL :: chi2, ths1, tsig
      REAL, DIMENSION(2) :: ee
      INTEGER :: i, j, k, n, nh, nl, nt, ntangd, ntint
      INTEGER :: INT
      REAL, DIMENSION(180,10) :: thangd
      REAL, DIMENSION(15,2) :: thsig
C
C*** End of declarations rewritten by SPAG
C
C
C--- Calculates the chi2 using experimental data and EMPIRE calculations
C
 
 
 
C--- If there are parameters being varied, places them in vals, accessible
C--- from FILEPREP, and modifies the OMPAR.DIR and TARGET_COLL.DAT files with
C--- FILEPREP in preparation for the EMPIRE calculations
      IF(NFIt.GT.0)THEN
        DO n = 1, NFIt
          VALs(n) = P0(n)
        ENDDO
        CALL FILEPREP
      ENDIF
 
C--- Now run EMPIRE
      CALL EMPIRE
 
C--- The neutron s-wave strength function is obtained from the EMPIRE
C--- output file and used to initialize chi2, if an experimental value
C--- exists.
      IF(INT(1000.*EN(1) + 0.5).EQ.1.AND.IDNt(1).EQ.0)THEN
        THS0 = 0.0
        ths1 = 0.0
        OPEN(100,FILE = 'OUTPUT.DAT',STATUS = 'old')
        DO i = 1, 200
          READ(100,'(7x,a35)')astrngth
          IF(astrngth(1:14).NE.'Calc. Strength')CYCLE
          READ(astrngth,'(29x,f6.3)')THS0
          READ(100,'(36x,f6.3)')ths1
        ENDDO
        CLOSE(100)
        chi2 = WT(15,2)*((SIG(1) - THS0)/DSIg(1))**2
        ntint = 1
      ELSE
        chi2 = 0.0
        ntint = 0
      ENDIF
 
      ntangd = 0
      nt = 0
 
C--- Zero array elements that are irrelevant but that might be used.
      ee(2) = 0.0
      DO j = 1, 12
        thsig(j,2) = 0.0
      ENDDO
 
C--- The rest of the calculations have been written to OPTFIT.CAL
C--- Data at the first energy are read from the file.
      OPEN(40,FILE = 'OPTFIT.CAL',STATUS = 'OLD')
      READ(40,'(f12.3,2e12.5)')ee(1), (thsig(j,1),j = 1,2)
      READ(40,'(12x,10e12.5)')(thsig(j,1),j = 3,12)
 
C--- The loop runs over the experimental values of the incident energy.
C--- Calculations are read from OPTFIT.CAL according to need for them.
C--- Calculated integrated cross sections are interpolated. Angular
C--- distributions have been calculated at the experimental energy.
      nh = 1
      nl = 2
      DO i = ntint + 1, NNDe
    5   IF(INT(DISc*EN(i) + 0.5).GT.INT(DISc*ee(nh) + 0.5))THEN
          nl = 3 - nl
          nh = 3 - nh
          READ(40,'(f12.4,2e12.5)')ee(nh), (thsig(j,nh),j = 1,2)
          READ(40,'(12x,10e12.5)')(thsig(j,nh),j = 3,12)
          GOTO 5
        ENDIF
        IF(NANgd(i).GT.0)READ(40,'(12x,10e12.5)')
     &                        ((thangd(j,k),k = 1,10),j = 1,NANgs(i))
        IF(NINts(i).GT.0)THEN
          DO j = 1, NINts(i)
            ntint = ntint + 1
C--- Interpolation of integrated cross section
            tsig = ((ee(nh) - EN(i))*thsig(IDNt(ntint),nl)
     &             + (EN(i) - ee(nl))*thsig(IDNt(ntint),nh))
     &             /(ee(nh) - ee(nl))
            chi2 = chi2 + WT(IDNt(ntint),1)
     &             *((tsig - SIG(ntint))/DSIg(ntint))**2
          ENDDO
        ENDIF
C--- Angular distributions
        IF(NANgd(i).GT.0)THEN
          DO k = 1, NANgd(i)
            ntangd = ntangd + 1
            DO j = 1, NANgs(i)
              nt = nt + 1
              IF(SIGa(nt).GT.0.)chi2 = chi2 + WT(IDAng(ntangd),2)
     &                                 + ((thangd(j,IDAng(ntangd))
     &                                 - SIGa(nt))/DSIga(nt))**2
            ENDDO
          ENDDO
        ENDIF
      ENDDO
 
      CHISQRD = chi2
 
      WRITE(*,*)'chi2=', chi2
      WRITE(*,*)
 
      CLOSE(40)
 
      RETURN
      END FUNCTION CHISQRD
 
!---------------------------------------------------------------------------
C
C-------------------------------------------------------------------
C
      SUBROUTINE FILEPREP
C
C*** Start of declarations rewritten by SPAG
C
C PARAMETER definitions
C
      INTEGER, PARAMETER :: MXFit = 20
C
C COMMON variables
C
      INTEGER, DIMENSION(3,MXFit) :: IDV
      INTEGER :: NFIt
      REAL, DIMENSION(MXFit) :: VALs, XVAls
      COMMON /FITPARS/ VALs, XVAls, IDV, NFIt
C
C Local variables
C
      REAL :: beta2, beta3, beta4, betax, betax2, ff, yy
      CHARACTER(13) :: fld
      INTEGER :: i, i1, i2, idef, ir, irng, itmp, j1, jp, js, lind, n, 
     &           nb2, nb3, nlvl, nxtpap, nxtpar
      CHARACTER(100) :: line
      INTEGER, DIMENSION(2,MXFit) :: lpar
      REAL, DIMENSION(6) :: xx
C
C*** End of declarations rewritten by SPAG
C
 
C--- Rewrites the OMPAR.DIR and TARGET_COLL.DAT files.
C--- The values given in vals(.) are added to the appropriate
C--- values in the files, as determined by idv(,.), with changes
C--- limited by the values given in xvals.
 
 
 
 
C--- If no parameters are to be changed, there is nothing to do.
      IF(NFIt.LE.0)RETURN
 
C--- Compares parameter values to limits and changes them if necessary
      DO n = 1, NFIt
        IF(VALs(n).GT.XVAls(n))VALs(n) = XVAls(n)
        IF(VALs(n).LT. - XVAls(n))VALs(n) = -XVAls(n)
      ENDDO
 
 
      nxtpar = 1
C--- The optical model parameters are modified first
      IF(IDV(1,1).LT.7)THEN
C--- Prepares line number and field positions of parameters to be changed
        DO n = 1, NFIt
          lpar(1,n) = 8*IDV(1,n) + 2*IDV(2,n) + (IDV(3,n) - 2)/6 - 9
          lpar(2,n) = MOD(IDV(3,n) - 2,6) + 1
        ENDDO
        lpar(1,NFIt + 1) = 99
        lpar(2,NFIt + 1) = 1
 
        OPEN(UNIT = 70,FILE = 'OMPAR0.DIR',STATUS = 'OLD')
        OPEN(UNIT = 71,FILE = 'OMPAR.DIR',STATUS = 'UNKNOWN')
 
C--- Header of RIPL file read and written
        DO i = 1, 11
          READ(70,'(a100)')line
          WRITE(71,'(a100)')line
        ENDDO
 
C--- Each potential section of the file is read
        DO j1 = 1, 6
          READ(70,'(a100)')line
          WRITE(71,'(a100)')line
          READ(line,'(i5)')irng
C--- When a potential exists
          IF(irng.GT.0)THEN
            nxtpap = nxtpar
C--- If no parameters to be changed here, it is read and written
            DO ir = 1, irng
              READ(70,'(a100)')line
              WRITE(71,'(a100)')line
              IF(lpar(1,nxtpar).GT.8*j1)THEN
                DO i = 1, 8
                  READ(70,'(a100)')line
                  WRITE(71,'(a100)')line
                ENDDO
C--- Otherwise, it is read and the relevant parameters modified before
C--- writing
              ELSE
                lind = 8*j1 - 7
                nxtpap = nxtpar
                DO i = 1, 8
                  READ(70,'(a100)')line
    5             IF(lpar(1,nxtpap).NE.lind)THEN
                    WRITE(71,'(a100)')line
                    lind = lind + 1
                  ELSE
                    READ(line,'(f12.5,6e11.5)')ff, xx
C--- Writing is performed so as to preserve the RIPL format
                    IF(IDV(3,nxtpap).EQ.1)THEN
                      ff = ff + VALs(nxtpap)
                      WRITE(line(1:12),'(f12.5)')ff
                    ELSE
                      itmp = lpar(2,nxtpap)
                      yy = xx(itmp) + VALs(nxtpap)
                      WRITE(fld,'(1pe13.5)')yy
                      fld(10:10) = fld(11:11)
                      fld(11:11) = fld(13:13)
                      itmp = 11*itmp + 2
                      line(itmp:itmp + 10) = fld(1:11)
                    ENDIF
                    nxtpap = nxtpap + 1
                    GOTO 5
                  ENDIF
                ENDDO
              ENDIF
            ENDDO
            nxtpar = nxtpap
          ENDIF
        ENDDO
 
        DO i = 1, 50
          READ(70,'(a100)',END = 10)line
          WRITE(71,'(a100)')line
        ENDDO
   10   CLOSE(70)
        CLOSE(71)
 
      ENDIF
 
C--- Then deformation parameters are modified
      IF(IDV(1,nxtpar).EQ.7)THEN
 
        OPEN(UNIT = 70,FILE = 'TARGET_COLL0.DAT',STATUS = 'OLD')
        OPEN(UNIT = 71,FILE = 'TARGET_COLL.DAT',STATUS = 'UNKNOWN')
 
C--- Reading and writing header
        DO i = 1, 2
          READ(70,'(a100)')line
          WRITE(71,'(a100)')line
        ENDDO
 
C--- Check to see if rotational or vibrational or soft
        READ(70,'(a50)')line(1:50)
        idef = 0
        IF(line(36:43).EQ.'deformed')idef = 1
        IF(line(36:43).EQ.'dynamica')THEN
          idef = 0
          WRITE(8,*)'ERROR: OPTMAN OMP fit is not implemented'
          STOP 'ERROR: OPTMAN OMP fit is not implemented'
        ENDIF
        WRITE(71,'(a50)')line(1:50)
        DO i = 1, 2
          READ(70,'(a50)')line(1:50)
          WRITE(71,'(a50)')line(1:50)
        ENDDO
 
C--- Read, modify and write rotational level file
        IF(idef.NE.0)THEN
          READ(70,'(i8,2i5,f6.1,2e11.3)')nlvl, i1, i2, yy, beta2, beta4
          IF(IDV(2,nxtpar).EQ.2)THEN
            beta2 = beta2 + VALs(nxtpar)
            nxtpar = nxtpar + 1
          ENDIF
          IF(IDV(1,nxtpar).EQ.7.AND.IDV(2,nxtpar).EQ.4)THEN
            beta2 = beta2 + VALs(nxtpar)
            nxtpar = nxtpar + 1
          ENDIF
          WRITE(71,'(i8,2i5,f6.1,2e11.3)')nlvl, i1, i2, yy, beta2, beta4
          DO i = 1, nlvl + 2
            READ(70,'(a50)')line(1:50)
            WRITE(71,'(a50)')line(1:50)
          ENDDO
C--- Read, modify and write vibrational level file
        ELSE
          READ(70,'(i8)')nlvl
          WRITE(71,'(i8)')nlvl
          DO i = 1, 3
            READ(70,'(a50)')line(1:50)
            WRITE(71,'(a50)')line(1:50)
          ENDDO
          nb2 = 1
          nb3 = 1
          DO n = 1, nlvl - 1
            READ(70,'(a29,e11.3)')line(1:29), betax
            READ(line,'(12x,i2,3x,i2)')js, jp
            IF(nb2.EQ.1.AND.IDV(2,nxtpar).EQ.2.AND.js.EQ.2.AND.jp.EQ.1)
     &         THEN
              betax2 = betax
              beta2 = betax + VALs(nxtpar)
              nb2 = 0
              nxtpar = nxtpar + 1
            ENDIF
C--- The deformation parameter of all states of a 2+ vibrational band are
C--- modified.
            IF(nb2.EQ.0.AND.MOD(js,2).EQ.0.AND.jp.EQ.1.AND.
     &         betax.EQ.betax2)THEN
              WRITE(71,'(a29,e11.3)')line(1:29), beta2
            ELSEIF(nb3.EQ.1.AND.IDV(1,nxtpar).EQ.7.AND.IDV(2,nxtpar)
     &             .EQ.3.AND.js.EQ.3.AND.jp.EQ. - 1)THEN
              beta3 = betax + VALs(nxtpar)
              WRITE(71,'(a29,e11.3)')line(1:29), beta3
              nxtpar = nxtpar + 1
              nb3 = 0
            ELSE
              WRITE(71,'(a29,e11.3)')line(1:29), betax
            ENDIF
          ENDDO
        ENDIF
        WRITE(71,*)
        CLOSE(70)
        CLOSE(71)
      ENDIF
 
      RETURN
      END SUBROUTINE FILEPREP
 
!---------------------------------------------------------------------------
C
C-------------------------------------------------------------------
C
      SUBROUTINE CLEANUP(Nnft)
C
C*** Start of declarations rewritten by SPAG
C
C PARAMETER definitions
C
      INTEGER, PARAMETER :: MXInd = 5000, MXInda = 5000
C
C COMMON variables
C
      REAL, DIMENSION(MXInda) :: ANGs, DSIga, SIGa
      REAL, DIMENSION(MXInd) :: DSIg, SIG
      REAL, DIMENSION(0:MXInd) :: EGRid, EN
      INTEGER, DIMENSION(MXInd) :: ICAla, IDAng, IDNt, NANgd, NANgs, 
     &                             NINts
      INTEGER :: NNDe
      REAL :: THS0, WT0
      COMMON /EXPTLDAT/ EN, SIG, DSIg, ANGs, SIGa, DSIga, EGRid, WT0, 
     &                  THS0, NINts, NANgd, NANgs, ICAla, IDNt, IDAng, 
     &                  NNDe
C
C Dummy arguments
C
      INTEGER :: Nnft
C
C Local variables
C
      CHARACTER(132) :: ctmp
      INTEGER*4 :: itmp
      LOGICAL :: linux
      INTEGER*4 :: PIPE
C
C*** End of declarations rewritten by SPAG
C
 
C
C--- CLEANUP prints the final values of the adjusted parameters
C--- and copies the input file with FITOMP=1 for the final run
C--- of EMPIRE after the fitting is done.
C
 
 
 
      linux = .TRUE.
      IF(SIG(1).GT.1.0E-3)THEN
        WRITE(2,*)
        WRITE(2,*)' Neutron s-wave strength function:'
        WRITE(2,'(5x,''Expt value = '',f8.3,'' +/- '',f8.3)')SIG(1), 
     &        DSIg(1)
        WRITE(2,'(5x,''Theo value = '',f8.3)')THS0
        WRITE(2,*)
      ENDIF
 
      IF(Nnft.GT.0)THEN
        WRITE(2,*)
        WRITE(2,*)'Final values of parameters:'
        CALL WRITEPARS
        WRITE(2,*)
      ENDIF
 
      WRITE(2,*)
      CALL THORA(2)
      CLOSE(2)
 
      IF(linux)THEN
        ctmp = 'mv INPUT1.DAT INPUT.DAT'
        itmp = PIPE(ctmp)
      ELSE
        ctmp = 'ren INPUT1.DAT INPUT.DAT'
        itmp = PIPE(ctmp)
      ENDIF
 
      RETURN
      END SUBROUTINE CLEANUP
 
!---------------------------------------------------------------------------
 
      SUBROUTINE SENSITIVITY
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:iou*
Ccc   *                    S E N S I T I V I T Y                         *
Ccc   *                                                                  *
Ccc   *     Modifies the optimal input according to instructions in      *
Ccc   *     the SENSITIVITY.INP file, runs EMPIRE,  and                  *
Ccc   *     calculates sensitivity matrix to be used with KALMAN.        *
Ccc   *                                                                  *
Ccc   * input:none                                                       *
Ccc   *                                                                  *
Ccc   * output:none                                                      *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
 
      IMPLICIT NONE
C
C*** Start of declarations rewritten by SPAG
C
C PARAMETER definitions
C
      INTEGER, PARAMETER :: NDReac = 90, NDKeys = 132
C
C COMMON variables
C
      CHARACTER(64) :: EMPiredir
      CHARACTER(72) :: EMPtitle
      COMMON /GLOBAL_E/ EMPiredir, EMPtitle
C
C Local variables
C
      REAL*8 :: aprojec, atarget, zprojec, ztarget
      CHARACTER(1) :: category, dum
      CHARACTER(132) :: ctmp
      REAL*8 :: einl, val, vale, valmem
      LOGICAL :: fexist, linux
      INTEGER :: i, i1, i1e, i2, i2e, i3, i3e, i4, i4e, ifound, ireac, 
     &           j, k
      INTEGER*4 :: i1p, i2p, itmp
      CHARACTER(80) :: inprecord
      INTEGER :: INT
      CHARACTER(6) :: name, namee
      CHARACTER(1), DIMENSION(NDKeys) :: namecat
      CHARACTER(6), DIMENSION(NDKeys) :: namelst
      CHARACTER(238) :: outrecord
      INTEGER*4 :: PIPE
      REAL*8, DIMENSION(NDReac) :: sensmat, xsec, xsecd, xsecu
      CHARACTER(1080) :: title
C
C*** End of declarations rewritten by SPAG
C
 
 
 
C     integer nreac
C
C
      DATA namelst/'ATILNO', 'CHMS  ', 'DXSRED', 'FUSRED', 'ELARED', 
     &     'GDRST1', 'GDRST2', 'GDRWEI', 'GDRWP ', 'GTILNO', 'PCROSS', 
     &     'QFIS  ', 'RESNOR', 'TOTRED', 'TUNEFI', 'TUNEPE', 'TUNE  ', 
     &     'UOMPAS', 'UOMPAV', 'UOMPVV', 'UOMPWS', 'UOMPWV', 'ALS   ', 
     &     'BETAV ', 'BETCC ', 'BFUS  ', 'BNDG  ', 'CRL   ', 'CSGDR1', 
     &     'CSGDR2', 'CSREAD', 'D1FRA ', 'DEFGA ', 'DEFGP ', 'DEFGW ', 
     &     'DFUS  ', 'DV    ', 'EFIT  ', 'EGDR1 ', 'EGDR2 ', 'EX1   ', 
     &     'EX2   ', 'EXPUSH', 'FCC   ', 'FCD   ', 'GAPN  ', 'GAPP  ', 
     &     'GCROA ', 'GCROD ', 'GCROE0', 'GCROT ', 'GCROUX', 'GDIV  ', 
     &     'GDRESH', 'GDRSPL', 'GDRWA1', 'GDRWA2', 'GGDR1 ', 'GGDR2 ', 
     &     'HOMEGA', 'SHRD  ', 'SHRJ  ', 'SHRT  ', 'SIG   ', 'TEMP0 ', 
     &     'TORY  ', 'TRUNC ', 'WIDEX ', 'COMPFF', 'PFNALP', 'DIRECT', 
     &     'DIRPOT', 'E1    ', 'E2    ', 'EcDWBA', 'ENDF  ', 'FISBAR', 
     &     'FISDEN', 'FISDIS', 'FISMOD', 'FISOPT', 'FISSHI', 'FITLEV', 
     &     'FITOMP', 'FLAM  ', 'GCASC ', 'GDRDYN', 'GDRGFL', 'GO    ', 
     &     'GRMULT', 'GSTRFN', 'GST   ', 'HMS   ', 'HRTW  ', 'IOUT  ', 
     &     'JSTAB ', 'KALMAN', 'LEVDEN', 'LTURBO', 'M1    ', 'MAXHOL', 
     &     'MSC   ', 'MSD   ', 'NACC  ', 'NEX   ', 'NHMS  ', 'NIXSH ', 
     &     'NOUT  ', 'NSCC  ', 'OMPOT ', 'QCC   ', 'QD    ', 'RELKIN', 
     &     'RESOLF', 'STMRO ', 'TRGLEV', 'XNI   ', 'UOMPRV', 'UOMPRW', 
     &     'UOMPRS', 'DEFDYN', 'DEFSTA', 'DEFMSD', 'GRANGN', 'GRANGP', 
     &     'PFNNIU', 'PFNTKE', 'UOMPAW', 'SHELNO', 'ROHFBA', 'ROHFBP', 
     &     'PFNRAT'/
C
C     Fission barr and LD keys, to be included
C
      DATA namecat/'A', 'A', 'T', 'T', 'T', 'A', 'A', 'A', 'A', 'A', 
     &     'T', 'A', 'T', 'T', 'A', 'T', 'A', 'A', 'A', 'A', 'A', 'A', 
     &     'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 
     &     'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 
     &     'R', 'R', 'R', 'R', 'R', 'R', 'T', 'R', 'R', 'R', 'R', 'R', 
     &     'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'F', 'A', 
     &     'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F', 
     &     'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F', 
     &     'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F', 
     &     'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F', 'A', 
     &     'A', 'A', 'T', 'T', 'T', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 
     &     'A', 'A'/
C-----meaning of namecat:
C-----A - variation of the parameter Allowed (default value is 1)
C-----R - variation of the parameter allowed with Restriction
C-----    (parameter must be explicitly specified in the optional part
C-----    of the standard input) ATTENTION: At this time it is not implemented
C-----    and is actually equvalent to F
C-----F - variation of the parameter not allowed (discrete value keyword)
Ccc
C-----T - variation of the parameter allowed; the parameters
C-----    that do not need  i1,i2,i3... specification, e.g., TUNEPE,
C-----    TOTRED, FUSRED, ELARED ...
Ccc
      linux = .TRUE.
      INQUIRE(FILE = ('SENSITIVITY.INP'),EXIST = fexist)
      IF(.NOT.fexist)THEN
        WRITE(8,*)
     &'SENSITIVITY CALCULATIONS REQUESTED BUT NO INPUT FILE INSTRUCTING 
     &WHICH PARAMETERS TO VARY HAS BEEN FOUND '
        STOP 'SENSITIVITY INPUT MISSING'
      ENDIF
      INQUIRE(FILE = ('LEVELS'),EXIST = fexist)
      IF(.NOT.fexist)THEN
        STOP 
     &'SENSITIVITY CALCULATIONS REQUESTED BUT LEVELS FILE MISSING. TO SO
     &LVE: 1) TURN OFF KALMAN OPTION.2) RUN EMPIRE FOR A SINGLE ENERGY. 
     &3) CHECK IF .lev FILE IS PRESENT. 4) TURN ON KALMAN OPTION AND STA
     &RT SENSITIVITY CALCULATIONS. THANKS.'
      ENDIF
C-----Move original (reference) input out of the way
      IF(linux)THEN
        ctmp = 'mv INPUT.DAT INPUTREF.DAT'
        itmp = PIPE(ctmp)
      ELSE
        ctmp = 'ren INPUT.DAT INPUTREF.DAT'
        itmp = PIPE(ctmp)
      ENDIF
C-----
C-----Run calculations with original input
C-----
C
C-----Read target and projectile from the input file
C     OPEN (UNIT = 44,FILE='INPUTREF.DAT', STATUS='OLD')
C     READ(44,'(A80)') inprecord
C     read(44,*) atarget,ztarget
C     read(44,*) aprojec,Zprojec
C     close(44)
 
      CLOSE(5) !close standard INPUT.DAT (just to be sure)
      OPEN(UNIT = 44,FILE = 'INPUTREF.DAT',STATUS = 'OLD')
                                                         !standard input moved out of the way
      OPEN(UNIT = 7,FILE = 'INPUT.DAT',STATUS = 'unknown')
                                                         !input to be run (with changed parameters)
C-----
C-----Read and copy mandatory part of the standard input
C-----
      DO i = 1, 10
        READ(44,'(A80)')inprecord
C--------Read target and projectile from the input file
        IF(i.EQ.2)READ(inprecord,*)atarget, ztarget
        IF(i.EQ.3)READ(inprecord,*)aprojec, zprojec
        WRITE(7,'(A80)')inprecord
      ENDDO
      WRITE(8,*)'Atarget, Ztarget, Aproj, Zproj ', atarget, ztarget, 
     &          aprojec, zprojec
C-----Read line of optional input
   10 READ(44,'(A80)',END = 110)inprecord
      IF(inprecord(1:1).NE.'*'.AND.inprecord(1:1).NE.'#'.AND.
     &   inprecord(1:1).NE.'!')THEN
 
        IF(inprecord(1:1).EQ.'@')THEN
          DO j = 1, 72
            EMPtitle(j:j) = inprecord(j:j)
                                         ! title of the run
          ENDDO
          EMPtitle(1:1) = ' '
          GOTO 10
                 ! next line
        ENDIF
 
        READ(inprecord,'(A6,G10.5,4I5)',END = 20,ERR = 20)namee, vale, 
     &       i1e, i2e, i3e, i4e
C   50 READ (44,'(A6,G10.5,4I5)',ERR = 30) namee,vale,i1e, i2e, i3e, i4e
        IF(namee.EQ.'GO    ')THEN
          WRITE(7,'(A6)')namee
          GOTO 20
                 !Jump to $ format for parameters that happens after GO
        ENDIF
C-----Copy input but skip KALMAN
        IF(namee.NE.'KALMAN')WRITE(7,'(A6,F10.3,4I5)')namee, vale, i1e, 
     &                             i2e, i3e, i4e
      ENDIF
      GOTO 10
C-----
C-----Read and copy optional part of the standard input
C-----
   20 READ(44,'(A80)',END = 30)inprecord
      IF(inprecord(1:1).EQ.'$')THEN
        READ(inprecord,'(1X,A6,G10.5,4I5)',END = 30)namee, vale, i1e, 
     &       i2e, i3e, i4e
        WRITE(7,'(''$'',A6,F10.3,4I5)')namee, vale, i1e, i2e, i3e, i4e
      ELSE
        WRITE(7,'(A80)')inprecord
      ENDIF
      GOTO 20
 
   30 CLOSE(7)
      CLOSE(44)
      CALL EMPIRE !calculations with original input
C-----Move original (reference) outputs out of the way
      IF(linux)THEN
        ctmp = 'mv LIST.DAT LISTREF.DAT'
        itmp = PIPE(ctmp)
        ctmp = 'mv OUTPUT.DAT OUTPUTREF.DAT'
        itmp = PIPE(ctmp)
        ctmp = 'mv XSECTIONS.OUT XSECTIONSREF.OUT'
        itmp = PIPE(ctmp)
      ELSE
        ctmp = 'ren LIST.DAT LISTREF.DAT'
        itmp = PIPE(ctmp)
        ctmp = 'ren OUTPUT.DAT OUTPUTREF.DAT'
        itmp = PIPE(ctmp)
        ctmp = 'ren XSECTIONS.OUT XSECTIONSREF.OUT'
        itmp = PIPE(ctmp)
      ENDIF
C-----
C-----Run sensitivity calculations
C-----
      OPEN(UNIT = 92,FILE = 'SENSITIVITY.MATRIX',STATUS = 'UNKNOWN')
                                                                   ! sensitivity matrix
C-----Go to the end of the SENSITIVITY.MATRIX file
   40 READ(92,*,END = 50)dum
      GOTO 40
   50 OPEN(UNIT = 17,FILE = 'SENSITIVITY.INP',STATUS = 'old')
                                                            !list of parameters to vary
C-----Read one line of the sensitivity input
   60 READ(17,'(A80)',END = 110)inprecord
      IF(inprecord(1:1).EQ.'*'.OR.inprecord(1:1).EQ.'#'.OR.
     &   inprecord(1:1).EQ.'!')GOTO 60
C      READ (inprecord,'(A6,G10.5,4I5)',ERR = 200) name,val,i1,i2, i3, i4
      READ(inprecord,'(A6,G10.5,4I5)',ERR = 100)name, val, i1p, i2p, i3, 
     &     i4
      i2 = INT(atarget) - i1p - i2p + INT(aprojec) - INT(zprojec)
      i1 = INT(ztarget) - i1p
C      write(0,*) name,i1,i2
C-----Check category of the parameter to be varied
      category = 'F'
      DO i = 1, NDKeys
        IF(name.EQ.namelst(i))THEN
          category = namecat(i)
        ENDIF
      ENDDO
      IF(category.EQ.'F')GOTO 60
      valmem = val
      IF(val.GE.1)THEN
C       WRITE(8,*) 'PARAMETER ',name,' VARIATION ',val,
C    &             ' IS BIGGER THAN 1'
        STOP 'PARAMETER VARIATION LARGER THAN 100%'
      ENDIF
C-----Check whether omp is being varied - if so then move Tl directory out of the way
      IF(name(1:4).EQ.'UOMP'.OR.name.EQ.'DEFDYN'.OR.name.EQ.'DEFSTA')
     &   THEN
        IF(linux)THEN
          ctmp = 'mv TL TLREF'
          itmp = PIPE(ctmp)
          ctmp = 'mkdir TL'
          itmp = PIPE(ctmp)
        ELSE
          ctmp = 'ren TL TLREF'
          itmp = PIPE(ctmp)
          ctmp = 'mkdir TL'
          itmp = PIPE(ctmp)
        ENDIF
      ENDIF
      ifound = 0
      DO k = 1, 2 ! 1 for parameter+val, 2 for parameter-val
        OPEN(UNIT = 44,FILE = 'INPUTREF.DAT',STATUS = 'OLD')
                                                            !standard input moved out of the way
        IF(k.EQ.2)val = -val   !normally we only invert the sign
        IF(name(1:4).EQ.'UOMP'.OR.name.EQ.'DEFDYN'.OR.
     &     name.EQ.'DEFSTA'.AND.k.EQ.2)THEN
          IF(linux)THEN
            ctmp = 'rm -r TL'
            itmp = PIPE(ctmp)
            ctmp = 'mkdir TL'
            itmp = PIPE(ctmp)
          ELSE
            ctmp = 'del TL'
            itmp = PIPE(ctmp)
            ctmp = 'mkdir TL'
            itmp = PIPE(ctmp)
          ENDIF
        ENDIF
C     WRITE(8,'(''Varying parameter '',A6,''x''F10.3,4I5)')
C    &      name, 1.0+val, i1,i2, i3, i4
        OPEN(UNIT = 7,FILE = 'INPUT.DAT',STATUS = 'unknown')
                                                         !input to be run (with changed parameters)
C-----
C-----Read and copy mandatory part of the standard input
C-----
        DO i = 1, 10
          READ(44,'(A80)')inprecord
          WRITE(7,'(A80)')inprecord
        ENDDO
C-----Read line of optional input
   65   READ(44,'(A80)',END = 110)inprecord
        IF(inprecord(1:1).NE.'*'.AND.inprecord(1:1).NE.'#'.AND.
     &     inprecord(1:1).NE.'!')THEN      ! comments
 
          IF(inprecord(1:1).EQ.'@')THEN
                                     ! title
            DO j = 1, 72
              EMPtitle(j:j) = inprecord(j:j)
                                         ! title of the run
            ENDDO
            EMPtitle(1:1) = ' '
            GOTO 65
                  ! next line
          ENDIF
 
          READ(inprecord,'(A6,G10.5,4I5)',ERR = 100,END = 75)namee, 
     &         vale, i1e, i2e, i3e, i4e
C
          IF(namee.EQ.'GO    ')THEN
            IF(ifound.EQ.0)THEN
C           IF(category.EQ.'A') THEN
              IF(category.EQ.'A'.OR.category.EQ.'T')THEN
                IF(name(1:4).EQ.'UOMP')THEN !special treatment for omp parameters (they must be negative)
                  WRITE(7,'(A6,F10.3,4I5)')name, -(1.0 + val), i1, i2, 
     &                  i3, i4            ! include omp parameter if missing
                ELSE
                  WRITE(7,'(A6,F10.3,4I5)')name, (1.0 + val), i1, i2, 
     &                  i3, i4
                ENDIF
              ELSEIF(category.EQ.'R')THEN
                CLOSE(7)
                CLOSE(44)
                CLOSE(5)
                GOTO 60 !get next parameter to vary
              ENDIF
            ENDIF
            WRITE(7,'(A6)')namee
C-----
C-----Read and copy optional part of the standard input
C-----
   70       READ(44,'(A80)',END = 75)inprecord
            IF(inprecord(1:1).EQ.'$')THEN
              READ(inprecord,'(1X,A6,G10.5,4I5)',END = 75)namee, vale, 
     &             i1e, i2e, i3e, i4e
C--------Write modified input with increased value of the parameter if name matches
              IF(name.EQ.namee.AND.i1.EQ.i1e.AND.i2.EQ.i2e.AND.
     &           i3.EQ.i3e.AND.i4.EQ.i4e.AND.category.EQ.'A')THEN
                WRITE(7,'(''$'',A6,F10.3,4I5)')namee, vale*(1 + val), 
     &                i1e, i2e, i3e, i4e
              ELSEIF(name.EQ.namee.AND.i1e.EQ.i1p.AND.category.EQ.'T')
     &               THEN
                WRITE(7,'(''$'',A6,F10.3,4I5)')namee, vale*(1.0 + val), 
     &                i1e, i2e, i3e, i4e
              ELSE
                WRITE(7,'(A80)')inprecord
              ENDIF
            ELSE
              WRITE(7,'(A80)')inprecord
            ENDIF
            GOTO 70
          ENDIF
C-----Write modified input with increased value of the parameter if name matches
          IF(name.EQ.namee.AND.i1.EQ.i1e.AND.i2.EQ.i2e.AND.
     &       i3.EQ.i3e.AND.i4.EQ.i4e.AND.category.EQ.'A')THEN
            WRITE(7,'(A6,F10.3,4I5)')namee, vale*(1.0 + val), i1e, i2e, 
     &                               i3e, i4e
            ifound = 1
          ELSEIF(name.EQ.namee.AND.i1e.EQ.i1p.AND.category.EQ.'T')THEN
            WRITE(7,'(A6,F10.3,4I5)')namee, vale*(1.0 + val), i1e, i2e, 
     &                               i3e, i4e
            ifound = 1
          ELSEIF(namee.EQ.'ENDF  ')THEN
            WRITE(7,'(A6,F10.3,4I5)')namee, 0.0, i1e, i2e, i3e, i4e
          ELSEIF(namee.NE.'KALMAN')THEN
            WRITE(7,'(A6,F10.3,4I5)')namee, vale, i1e, i2e, i3e, i4e
          ENDIF
        ENDIF
        GOTO 65
 
   75   CLOSE(7)
        CLOSE(44)
        CLOSE(5)
        CALL EMPIRE
 
C-----Delete modified input that has been used and move XSECTIONS.OUT file
        IF(linux)THEN
          ctmp = 'rm INPUT.DAT'
          itmp = PIPE(ctmp)
          IF(k.EQ.1)THEN
            ctmp = 'mv XSECTIONS.OUT XS-UP.DAT'
          ELSE
            ctmp = 'mv XSECTIONS.OUT XS-DOWN.DAT'
          ENDIF
          itmp = PIPE(ctmp)
        ELSE
          ctmp = 'del INPUT.DAT'
          itmp = PIPE(ctmp)
          IF(k.EQ.1)THEN
            ctmp = 'ren XSECTIONS.OUT XS-UP.DAT'
          ELSE
            ctmp = 'ren XSECTIONS.OUT XS-DOWN.DAT'
          ENDIF
          itmp = PIPE(ctmp)
        ENDIF
      ENDDO !loop over parameter+val and parameter-val
C-----Check whether omp has been varied - if so then restore original Tl directory and delete current
      IF(name(1:4).EQ.'UOMP'.OR.name.EQ.'DEFDYN'.OR.name.EQ.'DEFSTA')
     &   THEN
        IF(linux)THEN
          ctmp = 'rm -rf TL'
          itmp = PIPE(ctmp)
          ctmp = 'mv TLREF TL'
          itmp = PIPE(ctmp)
        ELSE
          ctmp = 'del TL'
          itmp = PIPE(ctmp)
          ctmp = 'ren TLREF TL'
          itmp = PIPE(ctmp)
        ENDIF
      ENDIF
C-----
C-----Calculate sensitivity to the parameter
C-----
      OPEN(UNIT = 34,FILE = 'XS-UP.DAT',STATUS = 'OLD')
                                                      ! x-sections with parameter+val
      READ(34,'(A238)')outrecord
      WRITE(92,'(A238)')outrecord
      WRITE(92,
     &'(''# Parameter: '',A6,2x,4I3,''  variation: +-''F5.3,      ''    
     & Sensitivity matrix'')')name, i1p, i2p, i3, i4, valmem
      READ(outrecord,'(1x,I3)')ireac  !read number of reactions
C-----Check whether ireac is within dimensions
      IF(ireac.GT.NDReac)THEN
        STOP 'INSUFFICIENT NDREAC DIMENSION IN empire_ctl.f'
      ENDIF
C     nreac = ireac/19
C     nreac = MAX(1,nreac)
C     IF(ireac.GT.19*nreac) nreac = nreac + 1
C     DO i=1,nreac
      READ(34,'(A1080)')title
      WRITE(92,'(A1080)')title
C     READ(34,'(A238)') outrecord
C     WRITE(92,'(A238)') outrecord
C     ENDDO
      OPEN(UNIT = 35,FILE = 'XS-DOWN.DAT',STATUS = 'OLD')
                                                        ! x-sections with parameter-val
      READ(35,'(A80)')inprecord  !skip lines with heading
C     DO i=1,nreac
      READ(35,'(A80)')inprecord
C     ENDDO
      OPEN(UNIT = 36,FILE = 'XSECTIONSREF.OUT',STATUS = 'OLD')
                                                             ! central x-sections
      READ(36,'(A80)')inprecord  !skip lines with heading
C     DO i=1,nreac
      READ(36,'(A80)')inprecord
C     ENDDO
   80 READ(34,'(G10.5,1P,(90E12.5))',END = 90)einl, 
     &     (xsecu(i),i = 1,ireac)
      READ(35,'(G10.5,1P,(90E12.5))')einl, (xsecd(i),i = 1,ireac)
      READ(36,'(G10.5,1P,(90E12.5))')einl, (xsec(i),i = 1,ireac)
      DO i = 1, ireac
        IF(ABS(xsecu(i) - xsecd(i)).LE.((xsecu(i)+xsecd(i))*1.0D-5))THEN
          sensmat(i) = 0
        ELSEIF(xsec(i).EQ.0)THEN
          sensmat(i) = 0
        ELSE
C           sensmat(i) = (xsecu(i)-xsecd(i))/(2.0*valmem)
C-----------Relative sensitivity (per variation interval)
          sensmat(i) = (xsecu(i) - xsecd(i))/xsec(i)
        ENDIF
      ENDDO
      WRITE(92,'(G10.5,1P,(90E12.4))')einl, (sensmat(i),i = 1,ireac)
      GOTO 80
   90 CLOSE(34)
      CLOSE(35)
      CLOSE(36)
      WRITE(92,'('' '')') ! write a blank line to separte outputs for different parameters
      GOTO 60  !Parameter done, return and get another parameter to vary
  100 WRITE(8,
     &'('' ERROR: INVALID FORMAT in KEY: '',A6,  '', EMPIRE STOPPED, che
     &ck INPUT file'')')name
      STOP ' FATAL: INVALID FORMAT in input KEY '
C-----Restore standard input
  110 IF(linux)THEN
        ctmp = 'mv INPUTREF.DAT INPUT.DAT'
        itmp = PIPE(ctmp)
        ctmp = 'mv LISTREF.DAT LIST.DAT'
        itmp = PIPE(ctmp)
        ctmp = 'mv OUTPUTREF.DAT OUTPUT.DAT'
        itmp = PIPE(ctmp)
        ctmp = 'mv XSECTIONSREF.OUT XSECTIONS.OUT'
        itmp = PIPE(ctmp)
      ELSE
        ctmp = 'ren INPUTREF.DAT INPUT.DAT'
        itmp = PIPE(ctmp)
        ctmp = 'ren LISTREF.DAT LIST.DAT'
        itmp = PIPE(ctmp)
        ctmp = 'ren OUTPUTREF.DAT OUTPUT.DAT'
        itmp = PIPE(ctmp)
        ctmp = 'ren XSECTIONSREF.OUT XSECTIONS.OUT'
        itmp = PIPE(ctmp)
      ENDIF
      RETURN
 
      END SUBROUTINE SENSITIVITY
 
 
